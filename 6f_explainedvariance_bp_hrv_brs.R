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
groups <- c("SBP", "DBP", "BRS", "SDNN")
for(g in groups){
    for(d in c("Male", "Female")){
        li <- list.files(path = file.path(g, d))
        a <- str_detect(li, regex(g, ignore_case = T))
        b <- !(str_detect(li, 'PERMUTED'))
        ev_list[[g]][[d]] <- rio::import(file.path(g, d, li[which(a&b)], 'aggregated_metrics_regression.txt'))
    }
}

df <- data.frame()
for (i in c(1:4)) {
    for(d in c("Male", "Female")){
        group <- str_split(names(ev_list[i]), pattern = '_', 2, simplify = T)[,1]
        sex <- d
        ev <- ev_list[[i]][[d]]$`Median Explained Variance`
        row <- cbind(group, ev, sex)
        df <- rbind(df, row)
    }
}

df<- df %>% 
    mutate(
        ev = as.numeric(ev)*100,
        group = as.factor(group)
    )

head(df)

write.table(df, "results/220319_expvar.csv", sep=",")

# Making permuted table
ev_list <- list()
groups <- c("SBP", "DBP", "BRS", "SDNN")
for(g in groups){
    for(d in c("Male", "Female")){
        li <- list.files(path = file.path(g, d))
        a <- str_detect(li, regex(g, ignore_case = T))
        b <- str_detect(li, 'PERMUTED')
        ev_list[[g]][[d]] <- rio::import(file.path(g, d, li[which(a&b)], 'aggregated_metrics_regression.txt'))
    }
}

df <- data.frame()
for (i in c(1:4)) {
    for(d in c("Male", "Female")){
        group <- str_split(names(ev_list[i]), pattern = '_', 2, simplify = T)[,1]
        sex <- d
        ev <- ev_list[[i]][[d]]$`Median Explained Variance`
        row <- cbind(group, ev, sex)
        df <- rbind(df, row)
    }
}

df<- df %>% 
    mutate(
        ev = as.numeric(ev)*100,
        group = as.factor(group)
    )

head(df)

write.table(df, "results/230223_expvar_perm.csv", sep=",")


# Making table with all iterations
ev_list2 <- list()
groups <- c("SBP", "DBP", "BRS", "SDNN")
for(g in groups){
    for(d in c("Male", "Female")){
        li <- list.files(path = file.path(g, d))
        a <- str_detect(li, regex(g, ignore_case = T))
        b <- !(str_detect(li, 'PERMUTED'))
        ev_list2[[g]][[d]] <- rio::import(file.path(g, d, li[which(a&b)], 'model_results_per_iteration.txt'))
    }
}

df2 <- data.frame()
for (i in c(1:4)) {
    for(d in c("Male", "Female")){
        group <- str_split(names(ev_list2[i]), pattern = '_', 2, simplify = T)[,1]
        sex <- d
        mean_ev <- mean(ev_list2[[i]][[d]]$`Explained Variance`)
        sd_ev <- sd(ev_list2[[i]][[d]]$`Explained Variance`)
        median_ev <- median(ev_list2[[i]][[d]]$`Explained Variance`)
        iqr_ev <- IQR(ev_list2[[i]][[d]]$`Explained Variance`)
        row <- cbind(group, sex, mean_ev, sd_ev, median_ev, iqr_ev)
        df2<- rbind(df2, row)
    }
}

df3 <- df2 %>% 
    mutate(
        mean_ev = as.numeric(mean_ev)*100,
        sd_ev = as.numeric(sd_ev)*100,
        median_ev = as.numeric(median_ev)*100,
        iqr_ev = as.numeric(iqr_ev)*100,
        group = as.factor(group),
        sex = as.factor(sex)
    )

head(df3)
gem <- df3 %>% mutate(
    mean_ev = format(round(mean_ev, 2), nsmall = 2)
)

for (i in c(1:length(ev_list2))) {
    for(d in c("Male", "Female")){
        ev_list2[[i]][[d]]$outcome <- str_split(names(ev_list2[i]), pattern = '_', 2, simplify = T)[,1]
        ev_list2[[i]][[d]]$sex <- d
    }
}

tot <- list()
for(i in c(1:length(ev_list2))) {
    li <- dplyr::bind_rows(ev_list2[[i]])
    tot[[i]] <- li
}
compl <- bind_rows(tot)

df4 <- compl %>% 
    mutate(
        ev = as.numeric(`Explained Variance`)*100,
        outcome = as.factor(outcome),
        outcome = fct_rev(outcome),
        outcome = fct_relevel(outcome, "SDNN", after = 3L),
        sex = as.factor(sex),
        sex = fct_rev(sex)
    )



(pl3 <- ggplot(df4, aes(x = sex, fill = sex, y = ev))+
        geom_hline(yintercept=0.0, linetype="dashed")+
        geom_half_violin(side = "r", scale = "width", alpha = 0.5) +
        geom_half_boxplot(side = "r", fill = "white", width = 0.2, outlier.shape = NA) +
        ggbeeswarm2::geom_beeswarm(aes(color = sex), method = "hex", 
                                   spacing = 1.5, side = -1L, alpha = 0.5)+
        labs(title = 'Machine learning models: explained variance',
             x = '', y = 'Explained variance', linetype = '', size = '') +
        scale_y_continuous(limits = c(-30, 30), breaks = seq(-30, 30, by = 5))+
        scale_color_aaas(guide = "none") +
        scale_fill_aaas(guide = "none") +
        facet_wrap(~outcome) +
        theme_Publication())

(pl3 <- df4 %>% 
        ggplot(., aes(x = sex, fill = sex, y = ev))+
        geom_hline(yintercept=0.0, linetype="dashed")+
        annotate("rect", xmin=-Inf, xmax=Inf, 
                 ymin=0.0, ymax=-50, alpha=0.5, fill="grey")+
        geom_half_violin(side = "r", scale = "width", alpha = 0.5) +
        geom_half_boxplot(side = "r", fill = "white", width = 0.2, 
                          outlier.shape = NA, errorbar.draw = FALSE) +
        ggbeeswarm2::geom_beeswarm(aes(color = sex), method = "hex", size = 0.5,
                                   spacing = 0.8, side = -1L, alpha = 0.5)+
        labs(title = 'Machine learning models: explained variance',
             x = '', y = 'Explained variance (%)', linetype = '', size = '') +
        scale_y_continuous(limits = c(-50, 50), breaks = seq(-50, 50, by = 5))+
        scale_color_aaas(guide = "none") +
        scale_fill_aaas(guide = "none") +
        facet_wrap(~outcome) +
        theme_Publication())

ann_text <- data.frame()
for(g in levels(df3$group)){
    for(d in levels(df3$sex)){
        thisev <- df3 %>% filter(sex == d & group == g) %>% 
            select(median_ev)
        b <- data.frame(sex = d, outcome = g,
                        lab = format(round(thisev[[1]], 2), nsmall=2), 
                        ev = 50)
        ann_text <- rbind(ann_text,b)
    }
}
ann_text <- ann_text %>% mutate(outcome = fct_rev(outcome))

pl3 + geom_text(data = ann_text , mapping = aes(label = str_c(lab, "%")))

ggsave("results/220609_expvar_bp_median.pdf", width = 6, height = 7)