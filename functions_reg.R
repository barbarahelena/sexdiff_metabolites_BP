## Functions regression

compared_to_permuted_reg <- function(path_true_outcome, path_permuted_outcome){
    theme_Publication <- function(base_size=14, base_family="sans") {
        library(grid)
        library(ggthemes)
        library(stringr)
        (theme_foundation(base_size=base_size, base_family=base_family)
            + theme(plot.title = element_text(face = "bold",
                                              size = rel(1.2), hjust = 0.5),
                    text = element_text(),
                    panel.background = element_rect(colour = NA),
                    plot.background = element_rect(colour = NA),
                    panel.border = element_rect(colour = NA),
                    axis.title = element_text(face = "bold",size = rel(1)),
                    axis.title.y = element_text(angle=90,vjust =2),
                    axis.title.x = element_text(vjust = -0.2),
                    axis.text = element_text(), 
                    axis.line = element_line(colour="black"),
                    axis.ticks = element_line(),
                    panel.grid.major = element_line(colour="#f0f0f0"),
                    panel.grid.minor = element_blank(),
                    legend.key = element_rect(colour = NA),
                    legend.position = "bottom",
                    # legend.direction = "horizontal",
                    legend.key.size= unit(0.2, "cm"),
                    legend.spacing  = unit(0, "cm"),
                    # legend.title = element_text(face="italic"),
                    plot.margin=unit(c(10,5,5,5),"mm"),
                    strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
                    strip.text = element_text(face="bold")
            ))
        
    } 
    
    # load permuted results
    path <- file.path(path_permuted_outcome,'model_results_per_iteration.txt')
    df <- rio::import(path)
    df$`Explained Variance` <- df$`Explained Variance` * 100
    n_iter <- nrow(df)
    path_true <- file.path(path_true_outcome,'model_results_per_iteration.txt')
    df_true <- rio::import(path_true)
    df_true$`Explained Variance` <- df_true$`Explained Variance` * 100
    true_result_explained_variance <- median(df_true$`Explained Variance`)
    df$Model <- 'Permuted'
    df_true$Model <- 'True'
    dff <- rbind(df, df_true)
    
    pl <- ggdensity(df$`Explained Variance`, 
                    main = paste("Density plot of", n_iter,'permuted iterations'),
                    xlab = "Explained Variance [%]", fill=pal_lancet()(1), alpha = 0.7)+
        geom_vline(xintercept = 0, linetype=1, size=1, color="black")
    #print(pl)
    ggsave(pl, path = path_true_outcome, filename = 'density_plot_permuted_iterations.pdf', width = 6, height = 4)
    
    # test if the permuted iterations is significantly smaller than the mean of the true model
    t_test <- t.test(df$`Explained Variance`, mu = true_result_explained_variance, alternative = "less")
    p <- format(x=t_test$p.value, scientific=T, digits = 3)
    pl <- ggplot(df, aes(x=`Explained Variance`))+
        geom_density(fill=pal_lancet()(1), color='black', alpha = 0.7)+
        theme_Publication()+
        labs(x='Explained variance (%)', title=paste0("True model compared to permuted outcome\np-value = ",p))+
        geom_vline(xintercept = true_result_explained_variance, linetype=2, size=1, color='black')+
        geom_vline(xintercept = median(df$`Explained Variance`), linetype=2, size=1, color='black')+
        xlim(c(min(df$`Explained Variance`), true_result_explained_variance+1))
    #print(pl)
    ggsave(pl, path = path_true_outcome, filename = 'density_plot_true_versus_permuted_with_t_test.pdf', width = 6, height = 6)
    
    pl <- ggplot(dff, aes(x=`Explained Variance`, fill=Model))+
        scale_fill_manual(values=pal_lancet()(2))+
        geom_density(color='black', alpha = 0.7)+
        theme_Publication()+
        labs(x='Explained variance (%)', title="True model compared to permuted outcome")+
        geom_vline(xintercept = 0, linetype=1, size=1, color='black')+
        geom_vline(xintercept = median(df_true$`Explained Variance`), linetype=2, size=1, color='black')+
        geom_vline(xintercept = median(df$`Explained Variance`), linetype=2, size=1, color='black')
    #print(pl)
    ggsave(pl, path = path_true_outcome, filename = 'density_plot_true_versus_permuted_with_all_true_iterations.pdf', width = 6, height = 6)
    ggsave(pl, path = path_true_outcome, filename = 'density_plot_true_versus_permuted_with_all_true_iterations.svg', width = 6, height = 6)
    
    # test if  true iterations are significantly larger than zero
    t_test <- t.test(df_true$`Explained Variance`, mu = 0, alternative = "greater")
    p <- format(x=t_test$p.value, scientific=T, digits = 3)
    pl <- ggplot(df_true, aes(x=`Explained Variance`))+
        geom_density(fill=pal_lancet()(2)[2], color='black', alpha = 0.7)+
        theme_Publication()+
        xlab('Explained variance [%]')+
        geom_vline(xintercept = true_result_explained_variance, linetype=2, size=1, color='black')+
        geom_vline(xintercept = 0, linetype=1, size=1, color='black')+
        ggtitle(paste0("True model compared to zero\np-value = ",p))
    #print(pl)
    ggsave(pl, path = path_true_outcome, filename = 'density_plot_true_versus_zero_with_t_test.pdf', width = 6, height = 6)
} 

plot_features_tests_reg <- function(input_path, output_path, top_n=10, 
                                    outcome_name='Outcome', x_lab='Levels'){
    theme_Publication <- function(base_size=12, base_family="sans") {
        library(grid)
        library(ggthemes)
        (theme_foundation(base_size=base_size, base_family=base_family)
            + theme(plot.title = element_text(face = "bold",
                                              size = rel(1.0), hjust = 0.5),
                    text = element_text(family = 'Helvetica'),
                    panel.background = element_rect(colour = NA),
                    plot.background = element_rect(colour = NA),
                    panel.border = element_rect(colour = NA),
                    axis.title = element_text(face = "bold",size = rel(1)),
                    axis.title.y = element_text(angle=90,vjust =2),
                    axis.title.x = element_text(vjust = -0.2),
                    axis.text = element_text(), 
                    axis.line = element_line(colour="black"),
                    axis.ticks = element_line(),
                    panel.grid.major = element_line(colour="#f0f0f0"),
                    panel.grid.minor = element_blank(),
                    legend.key = element_rect(colour = NA),
                    legend.position = "bottom",
                    # legend.direction = "horizontal",
                    legend.key.size= unit(0.2, "cm"),
                    legend.spacing  = unit(0, "cm"),
                    # legend.title = element_text(face="italic"),
                    plot.margin=unit(c(10,5,5,5),"mm"),
                    strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
                    strip.text = element_text(face="bold")
            ))
        
    } 
    
    # create dir for plots
    plot_path <- file.path(output_path, 'plots')
    if(!exists(plot_path)){
        dir.create(plot_path)
    }
    
    # open data from input path and feature importance list
    r <- rio::import(file.path(output_path,'feature_importance.txt'))
    r <- r %>% arrange(-RelFeatImp)
    input_data <- rio::import(file.path(input_path, 'X_data.txt'))
    feature_names <- read.csv(file.path(input_path, 'feat_ids.txt'), sep = '\t', header = F)
    y <- rio::import(file.path(input_path, 'y_reg.txt'))
    names(input_data) <- feature_names$V1
    
    # select features from top_n of ranking
    features_tk <- r$FeatName[1:top_n]
    features_tk <- features_tk[! features_tk %in% c('random_variable1', 'random_variable2')]
    dd <- input_data %>% dplyr::select(any_of(features_tk))
    dd$y <- y$V1 # add outcome var to dataframe

    # calculate correlation and plot for each feature
    for(j in 1:(ncol(dd)-1)){ # column y (outcome var) is last column
        df <- dd[, c(j, ncol(dd))] # determinant and outcome var column
        names(df)[1] <- 'Feature' # name determinant var "feature"
        cc <- cor.test(df$Feature, df$y, method = 'spearman')
        pl <- ggplot(df, aes(x=Feature, y=y))+
            geom_point(color='black', fill=pal_lancet()(2)[1], shape=21)+
            geom_smooth(formula = "y ~ x", method='lm', color=pal_lancet()(2)[2])+
            theme_Publication() +
            theme(legend.position = 'right') +
            labs(x=x_lab, 
                 y=outcome_name, 
                 title=paste(paste(strwrap(r$FeatName[j], width = 45), collapse = "\n"), 
                             "\nrho =", round(cc$estimate, digits = 3), 
                             ' p =', format(cc$p.value, digits = 2, scientific = T)))+
            theme(legend.key.height = unit(1, "cm"))+
            labs(fill=outcome_name)
        fname <- r$FeatName[j]
        cat(j, fname, '\n')
        fname <- str_replace_all(fname, "[*\";,:/\\\\ ]","_")
        ggsave(pl, path = plot_path, filename = paste0(j, '_',fname, '.pdf'), device = 'pdf', width = 5, height = 5)
        ggsave(pl, path = plot_path, filename = paste0(j, '_',fname, '.svg'), device = 'svg', width = 5, height = 5)
    }
    
    # again but now x-axis (metabolite) log10-transformed 
    for(j in 1:(ncol(dd)-1)){ # column y (outcome var) is last column
        df <- dd[, c(j, ncol(dd))] # determinant and outcome var column
        names(df)[1] <- 'Feature' # name determinant var "feature"
        cc <- cor.test(df$Feature, df$y, method = 'spearman')
        pl <- ggplot(df, aes(x=Feature, y=y))+
            geom_point(color='black', fill=pal_lancet()(2)[1], shape=21)+
            geom_smooth(formula = "y ~ x", method='lm', color=pal_lancet()(2)[2])+
            scale_x_log10() +
            theme_Publication() +
            theme(legend.position = 'right') +
            labs(x=x_lab, 
                 y=outcome_name, 
                 title=paste(paste(strwrap(r$FeatName[j], width = 45), collapse = "\n"), 
                             "\nrho =", round(cc$estimate, digits = 3), 
                             ' p =', format(cc$p.value, digits = 2, scientific = T)))+
            theme(legend.key.height = unit(1, "cm"))+
            labs(fill=outcome_name)
        fname <- r$FeatName[j]
        cat(j, fname, '\n')
        fname <- str_replace_all(fname, "[*\";,:/\\\\ ]","_")
        ggsave(pl, path = plot_path, filename = paste0(j, '_',fname, '_log10.pdf'), device = 'pdf', width = 5, height = 5)
        ggsave(pl, path = plot_path, filename = paste0(j, '_',fname, '_log10.svg'), device = 'svg', width = 5, height = 5)
    }
}

plot_features_top_n_reg <- function(input_path, output_path, top_n=20, nrow=4, 
                                    outcome_name='Outcome', x_lab='Levels'){
    theme_Publication <- function(base_size=12, base_family="sans") {
        library(grid)
        library(ggthemes)
        (theme_foundation(base_size=base_size, base_family=base_family)
            + theme(plot.title = element_text(face = "bold", family = 'Helvetica',
                                              size = rel(1.0), hjust = 0.5),
                    text = element_text(family = 'Helvetica'),
                    panel.background = element_rect(colour = NA),
                    plot.background = element_rect(colour = NA),
                    panel.border = element_rect(colour = NA),
                    axis.title = element_text(face = "bold",size = rel(1)),
                    axis.title.y = element_text(angle=90,vjust =2),
                    axis.title.x = element_text(vjust = -0.2),
                    axis.text = element_text(), 
                    axis.line = element_line(colour="black"),
                    axis.ticks = element_line(),
                    panel.grid.major = element_line(colour="#f0f0f0"),
                    panel.grid.minor = element_blank(),
                    legend.key = element_rect(colour = NA),
                    legend.position = "bottom",
                    # legend.direction = "horizontal",
                    legend.key.size= unit(0.2, "cm"),
                    legend.spacing  = unit(0, "cm"),
                    # legend.title = element_text(face="italic"),
                    plot.margin=unit(c(10,5,5,5),"mm"),
                    strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
                    strip.text = element_text(face="bold")
            ))
        
    } 
    
    # create dir for plot
    plot_path <- file.path(output_path, 'plots')
    if(!exists(plot_path)) {
        dir.create(plot_path)
    }
    
    # open data in input_path folder and feature importance list
    r <- rio::import(file.path(output_path,'feature_importance.txt'))
    r <- r %>% arrange(-RelFeatImp) # arrange: decreasing feat imp
    input_data <- rio::import(file.path(input_path, 'X_data.txt'))
    feature_names <- read.csv(file.path(input_path, 'feat_ids.txt'), sep = '\t', header = F)
    names(input_data) <- feature_names$V1
    y <- rio::import(file.path(input_path, 'y_reg.txt'))
    
    # select features that are in top_n
    features_tk <- r$FeatName[1:top_n]
    features_tk <- features_tk[! features_tk %in% c('random_variable1', 'random_variable2')]
    dd <- input_data %>% dplyr::select(any_of(features_tk))
    dd$y <- y$V1 # add outcome var to dataframe
    
    # pivot longer to make facet_wrap below possible (all features in one figure)
    df <- dd %>% pivot_longer(-y, names_to = 'features', values_to = 'values')
    df$features <- factor(df$features, r$FeatName)
    
    # not log transformed
    pl <- ggplot(df, aes(x=values, y=y))+
        geom_point(fill=pal_lancet()(1), color='black', shape=21)+
        geom_smooth(formula = "y ~ x", method='lm', color=pal_lancet()(2)[2])+
        theme_minimal()+
        theme_Publication()+
        theme(legend.position = 'right')+
        labs(x=x_lab, y=outcome_name)+
        theme(legend.key.height = unit(1, "cm"))+
        facet_wrap(~ features, nrow=nrow, scales = 'free')
    pl
    ggsave(pl, path = plot_path, filename = paste0('top_',top_n,'_features_scatterplots.pdf'), 
           device = 'pdf', width = 22, height = 16)
    
    # x-axis (metabolite) log transformed
    pl <- ggplot(df, aes(x=values, y=y))+
        geom_point(fill=pal_lancet()(1), color='black', shape=21)+
        geom_smooth(formula = "y ~ x", method='lm', color=pal_lancet()(2)[2])+
        scale_x_log10()+
        theme_minimal()+
        theme_Publication()+
        theme(legend.position = 'right')+
        labs(x=x_lab, y=outcome_name)+
        theme(legend.key.height = unit(1, "cm"))+
        facet_wrap(~ features, nrow=nrow, scales = 'free')
    pl
    ggsave(pl, path = plot_path, filename = paste0('top_',top_n,'_log10_features_scatterplots.pdf'), 
           device = 'pdf', width = 22, height = 16)
}

plot_feature_importance <- function(path_true, top_n){
    theme_Publication <- function(base_size=12, base_family="sans") {
        library(grid)
        library(ggthemes)
        (theme_foundation(base_size=base_size, base_family=base_family)
            + theme(plot.title = element_text(face = "bold",
                                              size = rel(1.0), hjust = 0.5,
                                              family = 'Helvetica'),
                    text = element_text(family = 'Helvetica'),
                    panel.background = element_rect(colour = NA),
                    plot.background = element_rect(colour = NA),
                    panel.border = element_rect(colour = NA),
                    axis.title = element_text(face = "bold",size = rel(1)),
                    axis.title.y = element_text(angle=90,vjust =2),
                    axis.title.x = element_text(vjust = -0.2),
                    axis.text = element_text(), 
                    axis.line = element_line(colour="black"),
                    axis.ticks = element_line(),
                    panel.grid.major = element_line(colour="#f0f0f0"),
                    panel.grid.minor = element_blank(),
                    legend.key = element_rect(colour = NA),
                    legend.position = "bottom",
                    # legend.direction = "horizontal",
                    legend.key.size= unit(0.2, "cm"),
                    legend.spacing  = unit(0, "cm"),
                    # legend.title = element_text(face="italic"),
                    plot.margin=unit(c(10,5,5,5),"mm"),
                    strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
                    strip.text = element_text(face="bold")
            ))
        
    } 
    cols <- list(low = '#ECE7F2',
                 mid = '#0570B0',
                 high = '#034E7B')
    r <- rio::import(file.path(path_true, 'feature_importance.txt'))
    r <- r %>% arrange(-RelFeatImp)
    mp <- mean(r$RelFeatImp)
    r$Tax <- factor(r$FeatName, levels = rev(r$FeatName))
    r <- r[1:top_n, ]
    mp <- mean(r$RelFeatImp)
    pl <- ggplot(data=r, aes(y=RelFeatImp, x=Tax, fill=RelFeatImp)) + 
        theme_Publication()+
        scale_fill_gradient2(low=cols$low, mid = cols$mid, high=cols$high, space='Lab', 
                             name="", midpoint = 65)+
        geom_bar(stat="identity")+
        coord_flip() +
        labs(y="Relative Importance %", x='', title = 'Metabolite relative importance') +
        theme(axis.text.x = element_text(size=12)) + 
        theme(axis.text.y = element_text(size=10))+
        theme(legend.key.size= unit(0.5, "cm"))+
        theme(legend.position = 'none')
    ggsave(pl, path = path_true, filename = str_c('plot_Feature_Importance_Metabolites.pdf'), device = 'pdf', width = 8, height = 7)
    ggsave(pl, path = path_true, filename = str_c('plot_Feature_Importance_Metabolites.svg'), device = 'svg', width = 8, height = 7)
}

plot_feature_importance_colors <- function(path_true, top_n){
    theme_Publication <- function(base_size=12, base_family="sans") {
        library(grid)
        library(ggthemes)
        (theme_foundation(base_size=base_size, base_family=base_family)
            + theme(plot.title = element_text(face = "bold",
                                              size = rel(1.0), hjust = 0.5,
                                              family = 'Helvetica'),
                    text = element_text(family = 'Helvetica'),
                    panel.background = element_rect(colour = NA),
                    plot.background = element_rect(colour = NA),
                    panel.border = element_rect(colour = NA),
                    axis.title = element_text(face = "bold",size = rel(1)),
                    axis.title.y = element_text(angle=90,vjust =2),
                    axis.title.x = element_text(vjust = -0.2),
                    axis.text = element_text(), 
                    axis.line.x = element_line(colour="black"),
                    axis.ticks.x = element_line(),
                    axis.ticks.y = element_blank(),
                    panel.grid.major = element_line(colour="#f0f0f0"),
                    panel.grid.minor = element_blank(),
                    legend.key = element_rect(colour = NA),
                    legend.position = "bottom",
                    # legend.direction = "horizontal",
                    legend.key.size= unit(0.2, "cm"),
                    legend.spacing  = unit(0, "cm"),
                    # legend.title = element_text(face="italic"),
                    plot.margin=unit(c(10,5,5,5),"mm"),
                    strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
                    strip.text = element_text(face="bold")
            ))
    } 
    
    r <- rio::import(file.path(path_true, 'feature_importance.txt'))
    r <- r %>% arrange(-RelFeatImp)
    a <- rio::import('../data/Info_plasma_metabolites_b.xlsx')
    a <- a %>% select(FeatName = BIOCHEMICAL, sup = `SUPER PATHWAY`) %>% 
        mutate(sup = as.factor(sup))
    summ <- a %>% 
        group_by(sup) %>% 
        dplyr::summarise(count=n()) %>% 
        filter(sup!='Xenobiotics') %>%
        arrange(desc(count)) %>% 
        mutate(sup = reorder(sup, count)) %>% 
        droplevels()
    ra <- left_join(r, a, by='FeatName')
    mp <- mean(r$RelFeatImp)
    ra$Tax <- factor(ra$FeatName, levels = rev(ra$FeatName))
    ra$sup <- factor(ra$sup, rev(levels(summ$sup)))
    ra <- ra[1:top_n, ]
    mp <- mean(ra$RelFeatImp)
    colpal <- c("Lipid"="#00468BFF", 
                "Amino Acid"="#ED0000FF", 
                "Nucleotide"="#42B540FF",
                "Cofactors and Vitamins"="#0099B4FF", 
                "Carbohydrate"="#925E9FFF", 
                "Peptide"="#FDAF91FF", 
                "Partially Characterized Molecules"="#AD002AFF", 
                "Energy"="#ADB6B6FF"
                )
    pl <- ggplot(data=ra, aes(y=RelFeatImp, x=Tax, fill=sup)) + 
        theme_Publication() +
        geom_bar(stat="identity", alpha=0.8) +
        scale_fill_manual(values = colpal)+
        coord_flip() +
        labs(y = 'Relative Importance %', x='', 
             title = 'Metabolite relative importance',
             fill = '') +
        theme(axis.text.x = element_text(size=12)) + 
        theme(axis.text.y = element_text(size=10)) +
        theme(legend.key.size= unit(0.5, "cm")) +
        theme(legend.position = 'bottom', legend.justification = 'center')
    pl
    ggsave(pl, path = path_true, filename = 'plot_Feature_Importance_Metabolites_color.pdf', 
            device = 'pdf', width = 10, height = 7)
    ggsave(pl, path = path_true, filename = 'plot_Feature_Importance_Metabolites_color.svg', 
            device = 'svg', width = 10, height = 7)
}