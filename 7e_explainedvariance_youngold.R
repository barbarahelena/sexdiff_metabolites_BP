## Explained variance
## Barbara Verhaar, b.j.verhaar@amsterdamumc.nl

# Libraries
library(tidyverse)
library(gghalves)
library(ggsci)

theme_Publication <- function(base_size=12, base_family="sans") {
    library(grid)
    library(ggthemes)
    (theme_foundation(base_size=base_size, base_family=base_family)
        + theme(plot.title = element_text(face = "bold",
                                          size = rel(1.0), hjust = 0.5),
                #family = 'Helvetica'
                text = element_text(),
                panel.background = element_rect(colour = NA),
                plot.background = element_rect(colour = NA),
                panel.border = element_rect(colour = NA),
                axis.title = element_text(face = "bold",size = rel(0.8)),
                axis.title.y = element_text(angle=90,vjust =2),
                axis.line.y = element_line(colour="black"),
                axis.title.x = element_text(vjust = -0.2),
                axis.text = element_text(), 
                axis.line.x = element_line(colour="black"),
                axis.ticks.x = element_line(),
                axis.ticks.y = element_line(),
                panel.grid.major = element_line(colour="#f0f0f0"),
                panel.grid.minor = element_blank(),
                legend.key = element_rect(colour = NA),
                legend.position = "right",
                # legend.direction = "horizontal",
                legend.key.size= unit(0.2, "cm"),
                legend.spacing  = unit(0, "cm"),
                # legend.title = element_text(face="italic"),
                plot.margin=unit(c(5,5,5,5),"mm"),
                strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
                strip.text = element_text(face="bold"),
                plot.caption = element_text(face = "italic", size=rel(0.6))
        ))
} 

# Making table
ev_list <- list()
groups <- c("FemY_SBP", "FemY_DBP", "MaleY_SBP", "MaleY_DBP", 
            "FemO_SBP", "FemO_DBP", "MaleO_SBP", "MaleO_DBP")
for(g in groups){
        li <- list.files(path = file.path(g))
        a <- str_detect(li, regex(g, ignore_case = TRUE))
        b <- !(str_detect(li, 'PERMUTED'))
        ev_list[[g]] <- rio::import(file.path(g, li[which(a&b)], 'aggregated_metrics_regression.txt'))
}

df <- data.frame()
for (i in c(1:8)) {
        group <- str_split(names(ev_list[i]), pattern = '_', 2, simplify = T)[,1]
        ev <- ev_list[[i]]$`Median Explained Variance`
        ev <- case_when(ev < 0 ~ 0, ev>0 ~ ev)
        row <- cbind(group, ev)
        df <- rbind(df, row)
}

df<- df %>% 
    mutate(
        ev = as.numeric(ev)*100,
        group = as.factor(group)
    )

head(df)

write.table(df, "../results/220437_expvar_youngold.csv", sep=",")
clipr::write_clip(df)


# Making table with all iterations
ev_list2 <- list()
groups <- c("FemY_SBP", "FemY_DBP", "MaleY_SBP", "MaleY_DBP", 
            "FemO_SBP", "FemO_DBP", "MaleO_SBP", "MaleO_DBP")
for(g in groups){
    li <- list.files(path = file.path(g))
    a <- str_detect(li, regex(g, ignore_case = TRUE))
    b <- !(str_detect(li, 'PERMUTED'))
    ev_list2[[g]] <- rio::import(file.path(g, li[which(a&b)], 'model_results_per_iteration.txt'))
}

df2 <- data.frame()
for (i in c(1:8)) {
        group <- names(ev_list2[i])
        mean_ev <- mean(ev_list2[[i]]$`Explained Variance`)
        sd_ev <- sd(ev_list2[[i]]$`Explained Variance`)
        median_ev <- median(ev_list2[[i]]$`Explained Variance`)
        iqr_ev <- IQR(ev_list2[[i]]$`Explained Variance`)
        row <- cbind(group, mean_ev, sd_ev, median_ev, iqr_ev)
        df2<- rbind(df2, row)
}

df3 <- df2 %>% 
    mutate(
        mean_ev = as.numeric(mean_ev)*100,
        sd_ev = as.numeric(sd_ev)*100,
        median_ev = as.numeric(median_ev)*100,
        iqr_ev = as.numeric(iqr_ev)*100,
        group = as.factor(group),
        sex = case_when(
            str_detect(group, "Fem") ~ paste0("Female"),
            str_detect(group, "Male") ~ paste0("Male")
        ),
        youngold = case_when(
            str_detect(group, "Y") ~ paste0("young"),
            str_detect(group, "O") ~ paste0("old")
        ),
        bp = case_when(
            str_detect(group, "SBP") ~ paste0("SBP"),
            str_detect(group, "DBP") ~ paste0("DBP")
        )
    )

head(df3)
gem <- df3 %>% mutate(
    mean_ev = format(round(mean_ev, 1), nsmall = 1),
    across(c(sex, bp, youngold), as.factor),
    across(c(sex, bp, youngold), fct_rev)
)



for (i in c(1:length(ev_list2))) {
        ev_list2[[i]]$group <- names(ev_list2[i])
        ev_list2[[i]]$sex <- case_when(
            str_detect(ev_list2[[i]]$group, "Fem") ~ paste0("Female"),
            str_detect(ev_list2[[i]]$group, "Male") ~ paste0("Male")
        )
        ev_list2[[i]]$youngold <- case_when(
            str_detect(ev_list2[[i]]$group, "Y") ~ paste0("young"),
            str_detect(ev_list2[[i]]$group, "O") ~ paste0("old")
        )
        ev_list2[[i]]$bp <- case_when(
            str_detect(ev_list2[[i]]$group, "SBP") ~ paste0("SBP"),
            str_detect(ev_list2[[i]]$group, "DBP") ~ paste0("DBP")
        )
}

tot <- list()
for(i in c(1:length(ev_list2))) {
    li <- dplyr::bind_rows(ev_list2[[i]])
    tot[[i]] <- li
}
compl <- bind_rows(tot)

df4 <- compl %>% 
    mutate(
        ev = as.numeric(`Explained Variance`)*100
    ) %>% 
    mutate(across(c(sex, bp, youngold), as.factor),
           across(c(sex, bp, youngold), fct_rev)) 


(pl4 <- ggplot(df4, aes(x = youngold, y = ev))+
        annotate("rect", xmin=-Inf, xmax=Inf, 
                 ymin=0.0, ymax=-30, alpha=0.5, fill="grey")+
        geom_hline(yintercept=0.0, linetype="dashed")+
        geom_violin(aes(fill = sex), scale = "width", alpha = 0.4,
                    position = position_dodge(1.0)) +
        geom_boxplot(aes(group = interaction(sex, youngold)), width = 0.2, 
                     outlier.shape = NA, position = position_dodge(1.0)) +
        labs(title = 'Machine learning models: explained variance',
             x = '', y = 'Explained variance (%)', linetype = '', size = '') +
        scale_y_continuous(limits = c(-30, 50))+
        #scale_x_discrete(position = "top") +
        scale_color_aaas(guide = "none") +
        scale_fill_aaas() +
        theme_Publication() +
        theme(
            legend.position = 'right',
            legend.title = element_blank(),
            legend.key.width = unit(0.5, "cm"),
            plot.margin = unit(c(1,3,1,1), "lines")
        )) +
        facet_wrap(~bp) +
        geom_text(data = gem, aes(label = str_c(mean_ev, "%"), y = 50,
                                  group = interaction(sex, youngold)),
                    position = position_dodge(1.0))
ggsave("results/220430_expvar_youngold.pdf", device = "pdf", width = 7, height = 5)
