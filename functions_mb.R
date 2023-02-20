## Functions microbiome

plot_feature_importance_asv <- function(path_true, top_n){
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
    cols <- list(low = '#ECE7F2',
                 mid = '#0570B0',
                 high = '#034E7B')
    r <- rio::import(file.path(path_true, 'feature_importance.txt'))
    r <- r %>% arrange(-RelFeatImp)
    tax <- readRDS("../data/Taxonomy_all_ASVs_Helius.RDS")
    r$Tax <- tax$Tax[match(r$FeatName, tax$ASV)]
    r <- r[1:top_n, ]
    r <- r %>% mutate(Tax = factor(make.unique(Tax), levels = rev(make.unique(Tax))))
    mp <- mean(r$RelFeatImp)
    pl <- ggplot(data=r, aes(y=RelFeatImp, x=Tax, fill=RelFeatImp)) + 
        theme_Publication()+
        scale_fill_gradient2(low=cols$low, mid = cols$mid, high=cols$high, space='Lab', name="",
                             midpoint = 50)+
        geom_bar(stat="identity")+
        coord_flip() +
        ylab("Relative Importance (%)")+
        xlab("") +
        theme(axis.text.x = element_text(size=12)) + 
        theme(axis.text.y = element_text(size=10))+
        theme(legend.key.size= unit(0.5, "cm"))+
        theme(legend.position = 'right')
    ggsave(path = path_true, filename = 'plot_Feature_Importance.pdf', device = 'pdf', width = 8, height = 7)
}

plot_features_tests_asv <- function(input_path, output_path, top_n=10, xlab = "metabolite"){
    theme_Publication <- function(base_size=11, base_family="sans") {
        library(grid)
        library(ggthemes)
        (theme_foundation(base_size=base_size, base_family=base_family)
            + theme(plot.title = element_text(face = "bold",
                                              size = rel(1.0), hjust = 0.5),
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
    x_lab <- xlab
    plot_path <- file.path(output_path, 'plots')
    dir.create(plot_path)
    r <- rio::import(file.path(output_path,'feature_importance.txt'))
    r <- r %>% arrange(-RelFeatImp)
    input_data <- rio::import(file.path(input_path, 'X_data.txt'))
    feature_names <- read.csv(file.path(input_path, 'feat_ids.txt'), sep = '\t', header = F)
    tax <- readRDS("../data/Taxonomy_all_ASVs_Helius.RDS")
    names(input_data) <- feature_names$V1
    if(top_n > ncol(input_data)){
        cat('\n\nRequested no. of features is higher than total number of features in model.\n
                 Showing all features in model.\n\n')
        top_n <- ncol(input_data)
    }
    features_tk <- r$FeatName[1:top_n]
    features_tk <- features_tk[! features_tk %in% c('random_variable1', 'random_variable2')]
    ft_tax <- tax$Tax[match(features_tk, tax$ASV)]
    dd <- input_data %>% dplyr::select(any_of(features_tk))
    y <- rio::import(file.path(input_path, 'y_reg.txt'))
    dd$y <- y$V1
    for(j in 1:length(features_tk)){
        asv <- features_tk[j]
        df <- dd %>% dplyr::select(all_of(asv), y)
        names(df)[1] <- 'Feature'
        df <- df %>% mutate(Feature = Feature / 200)
        tax_asv <- tax$Tax[match(asv, tax$ASV)]
        pl <- ggplot(df, aes(x=y, y=Feature, fill=y))+
            geom_point(fill=pal_lancet()(1), color='black', shape=21)+
            geom_smooth(method='lm', color=pal_lancet()(2)[2])+
            scale_y_log10()+
            theme_minimal()+
            theme_Publication()+
            theme(legend.position = 'right')+
            labs(x=x_lab, y = 'Relative abundance (%)')+
            #scale_fill_gradient2_tableau()+
            theme(legend.key.height = unit(1, "cm"))+
            facet_wrap(~ features, nrow=nrow, scales = 'free')
        pl
        fname <- tax_asv
        cat(j, fname, '\n')
        fname <- str_replace_all(fname, "[*\";,:/\\\\ ]","_")
        #print(pl)
        ggsave(pl, path = plot_path, filename = paste0(j, '_',fname, '.pdf'), device = 'pdf', width = 5, height = 5)
    }
}

plot_features_tests_asv_tot <- function(input_path, output_path, top_n=20, nrow=4, xlab = "metabolite"){
    theme_Publication <- function(base_size=11, base_family="sans") {
        library(grid)
        library(ggthemes)
        (theme_foundation(base_size=base_size, base_family=base_family)
            + theme(plot.title = element_text(face = "bold",
                                              size = rel(1.0), hjust = 0.5),
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
                    strip.text = element_text(face="bold", size = rel(0.8))
            ))
        
    } 
    x_lab <- xlab
    plot_path <- file.path(output_path, 'plots')
    dir.create(plot_path)
    r <- rio::import(file.path(output_path,'feature_importance.txt'))
    r <- r %>% arrange(-RelFeatImp)
    input_data <- rio::import(file.path(input_path, 'X_data.txt'))
    feature_names <- read.csv(file.path(input_path, 'feat_ids.txt'), sep = '\t', header = F)
    names(input_data) <- feature_names$V1
    tax <- readRDS("../data/Taxonomy_all_ASVs_Helius.RDS")
    if(top_n > ncol(input_data)){
        cat('\n\nRequested no. of features is higher than total number of features in model.\nShowing all features in model.\n\n')
        top_n <- ncol(input_data)
    }
    features_tk <- r$FeatName[1:top_n]
    features_tk <- features_tk[! features_tk %in% c('random_variable1', 'random_variable2')]
    dd <- input_data %>% dplyr::select(any_of(features_tk))
    colnames(dd) <- make.unique(tax$Tax[match(colnames(dd), tax$ASV)])
    y <- rio::import(file.path(input_path, 'y_reg.txt'))
    dd$y <- as.numeric(y$V1)
    df <- dd %>% pivot_longer(-y, names_to = 'features', values_to = 'values')
    df <- df %>% mutate(y = factor(y),
                        features = as.factor(features),
                        features = fct_inorder(features),
                        values = values / 200
    )
    pl <- ggplot(df, aes(x=values, y=y))+
        geom_point(fill=pal_lancet()(1), color='black', shape=21)+
        geom_smooth(method='lm', color=pal_lancet()(2)[2])+
        theme_minimal()+
        theme_Publication()+
        theme(legend.position = 'right')+
        labs(x=x_lab, y = 'Relative abundance (%)')+
        #scale_fill_gradient2_tableau()+
        theme(legend.key.height = unit(1, "cm"))+
        facet_wrap(~ features, nrow=nrow, scales = 'free')
    pl
    
    ggsave(pl, path = plot_path, filename = paste0('top_',top_n,'_features.pdf'), device = 'pdf', width=15, height = 14)
    ggsave(pl, path = plot_path, filename = paste0('top_',top_n,'_features.svg'), device = 'svg', width=15, height = 14)
}