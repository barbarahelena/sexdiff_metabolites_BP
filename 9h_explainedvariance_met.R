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
setwd("Metabolites")
ev_list <- list()
groups <- c("SBP_metabolites/Male_1", "SBP_metabolites/Male_2", "SBP_metabolites/Male_3", 
            "SBP_metabolites/Male_4", "SBP_metabolites/Male_5",
            "SBP_metabolites/Male_6", "SBP_metabolites/Male_7", "SBP_metabolites/Male_8", 
            "SBP_metabolites/Male_9", "SBP_metabolites/Male_10",
            "SBP_metabolites/Fem_1", "SBP_metabolites/Fem_2", "SBP_metabolites/Fem_3", 
            "SBP_metabolites/Fem_4", "SBP_metabolites/Fem_5",
            "SBP_metabolites/Fem_6", "SBP_metabolites/Fem_7", "SBP_metabolites/Fem_8", 
            "SBP_metabolites/Fem_9", "SBP_metabolites/Fem_10",
            "DBP_metabolites/Fem_1", "DBP_metabolites/Fem_2", 
            "DBP_metabolites/Fem_4", "DBP_metabolites/Fem_5",
            "DBP_metabolites/Fem_6", "DBP_metabolites/Fem_7", "DBP_metabolites/Fem_8", 
            "DBP_metabolites/Fem_9", "DBP_metabolites/Fem_10",
            "HRV_metabolites/SDNN_1", "HRV_metabolites/SDNN_2", "HRV_metabolites/SDNN_3", 
            "HRV_metabolites/SDNN_4", "HRV_metabolites/SDNN_5",
            "HRV_metabolites/SDNN_6", "HRV_metabolites/SDNN_7", "HRV_metabolites/SDNN_8", 
            "HRV_metabolites/SDNN_9", "HRV_metabolites/SDNN_10")

for(g in groups){
        li <- list.files(path = file.path(g))
        a <- str_detect(li, regex(str_extract(g, "\\w+$"), ignore_case = TRUE))
        b <- !(str_detect(li, 'PERMUTED'))
        ev_list[[g]] <- rio::import(file.path(g, li[which(a&b)], 'aggregated_metrics_regression.txt'))
}

df <- data.frame()
for (i in c(1:length(ev_list))) {
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

write.table(df, "../results/220814_expvar_met.csv", sep=",")

# Making table with all iterations
ev_list2 <- list()
groups <- c("SBP_metabolites/Male_1", "SBP_metabolites/Male_2", "SBP_metabolites/Male_3", 
            "SBP_metabolites/Male_4", "SBP_metabolites/Male_5",
            "SBP_metabolites/Male_6", "SBP_metabolites/Male_7", "SBP_metabolites/Male_8", 
            "SBP_metabolites/Male_9", "SBP_metabolites/Male_10",
            "SBP_metabolites/Fem_1", "SBP_metabolites/Fem_2", "SBP_metabolites/Fem_3", 
            "SBP_metabolites/Fem_4", "SBP_metabolites/Fem_5",
            "SBP_metabolites/Fem_6", "SBP_metabolites/Fem_7", "SBP_metabolites/Fem_8", 
            "SBP_metabolites/Fem_9", "SBP_metabolites/Fem_10",
            "DBP_metabolites/Fem_1", "DBP_metabolites/Fem_2", 
            "DBP_metabolites/Fem_4", "DBP_metabolites/Fem_5",
            "DBP_metabolites/Fem_6", "DBP_metabolites/Fem_7", "DBP_metabolites/Fem_8", 
            "DBP_metabolites/Fem_9", "DBP_metabolites/Fem_10",
            "HRV_metabolites/SDNN_1", "HRV_metabolites/SDNN_2", "HRV_metabolites/SDNN_3", 
            "HRV_metabolites/SDNN_4", "HRV_metabolites/SDNN_5",
            "HRV_metabolites/SDNN_6", "HRV_metabolites/SDNN_7", "HRV_metabolites/SDNN_8", 
            "HRV_metabolites/SDNN_9", "HRV_metabolites/SDNN_10")

for(g in groups){
    li <- list.files(path = file.path(g))
    a <- str_detect(li, regex(str_extract(g, "\\w+$"), ignore_case = TRUE))
    b <- !(str_detect(li, 'PERMUTED'))
    ev_list2[[g]] <- rio::import(file.path(g, li[which(a&b)], 'model_results_per_iteration.txt'))
}

df2 <- data.frame()
for (i in c(1:length(ev_list2))) {
        met <- str_c(str_split(names(ev_list2[i]), pattern = c('_'), 2, simplify = T)[,1], 
                     str_extract(names(ev_list2[i]), "\\w+$"))
        model <- str_split(met, pattern = '_', 2, simplify = T)[,1]
        mean_ev <- mean(ev_list2[[i]]$`Explained Variance`)
        sd_ev <- sd(ev_list2[[i]]$`Explained Variance`)
        median_ev <- median(ev_list2[[i]]$`Explained Variance`)
        iqr_ev <- IQR(ev_list2[[i]]$`Explained Variance`)
        row <- cbind(met, model, mean_ev, sd_ev, median_ev, iqr_ev)
        df2<- rbind(df2, row)
}

df3 <- df2 %>% 
    mutate(
        model = as.factor(model),
        outcome = case_when(
            model == "SBPMale" ~ paste0("SBP"),
            model == "SBPFem" ~ paste0("SBP"),
            model == "DBPFem"~ paste0("DBP"),
            model == "HRVSDNN" ~ paste0("HRV")
        ),
        outcome = as.factor(outcome),
        outcome = fct_relevel(outcome, "SBP", after = 1L),
        mean_ev = as.numeric(mean_ev)*100,
        sd_ev = as.numeric(sd_ev)*100,
        median_ev = as.numeric(median_ev)*100,
        iqr_ev = as.numeric(iqr_ev)*100,
        met = as.factor(met),
        met = fct_recode(met, "sphingomyelin_1" = "SBPMale_1",
                         "formylmethionine" = "SBPMale_2",
                         "glycochenodeoxycholate" = "SBPMale_3",
                         "acetylglutamate" = "SBPMale_4", 
                         "picolinoylglycine" = "SBPMale_5",
                         "hexadecadienoate" = "SBPMale_6",
                         "sphingomyelin_2" = "SBPMale_7",
                         "sphingomyelin_3" = "SBPMale_8",
                         "vanillylmandelate" = "SBPMale_9",
                         "3-hydroxy-3-methylglutarate" = "SBPMale_10",
                         "linolenoylcarnitine" = "SBPFem_1",
                         "hydroxyphenylacetylglutamine" = "SBPFem_2",
                         "vanillactate" = "SBPFem_3",
                         "DMTPA" = "SBPFem_4",
                         "linoleoylcarnitine_2" = "SBPFem_5",
                         "N-acetylneuraminate" = "HRVSDNN_1",
                         "isobutyrylcarnitine" = "HRVSDNN_2",
                         "gentisate" = "HRVSDNN_3",
                         "cholestenoate" = "HRVSDNN_4",
                         "cysteine" = "HRVSDNN_5",
                         "hydroxylaurate" = "HRVSDNN_6",
                         "phenylacetate" = "HRVSDNN_7",
                         "sphingomyelin_4" = "HRVSDNN_8",
                         "1-oleoyl-GPI" = "HRVSDNN_9",
                         "glycerate" = "HRVSDNN_10",
                         "N-acetylcitrulline" = "DBPFem_1",
                         "etiocholanoloneglucuronide" = "DBPFem_2",
                         "androsteronesulfate" = "DBPFem_4",
                         "serotonin" = "DBPFem_5",
                         "cortoloneglucuronide" = "DBPFem_6",
                         "pregnanediol3glucuronide" = "DBPFem_7",
                         "vanillactate" = "DBPFem_8",
                         "5alphaandrostan" = "DBPFem_9",
                         "epiandrosteronesulfate" = "DBPFem_10"),
        model = as.factor(model),
        model = fct_recode(model,
                           "Male SBP" = "SBPMale",
                           "Female SBP" = "SBPFem",
                           "Female DBP" = "DBPFem",
                           "Male HRV" = "HRVSDNN")
    )

head(df3)
gem <- df3 %>% mutate(
    mean_ev = format(round(mean_ev, 2), nsmall = 2)
)



for (i in c(1:length(ev_list2))) {
        ev_list2[[i]]$met = str_c(str_split(names(ev_list2[i]), pattern = c('_'), 2, simplify = T)[,1], 
                                  str_extract(names(ev_list2[i]), "\\w+$"))
        ev_list2[[i]]$met = as.factor(ev_list2[[i]]$met)
        ev_list2[[i]]$met = fct_recode(ev_list2[[i]]$met, 
                                       "sphingomyelin_1" = "SBPMale_1",
                                       "formylmethionine" = "SBPMale_2",
                                       "glycochenodeoxycholate" = "SBPMale_3",
                                       "acetylglutamate" = "SBPMale_4", 
                                       "picolinoylglycine" = "SBPMale_5",
                                       "hexadecadienoate" = "SBPMale_6",
                                       "sphingomyelin_2" = "SBPMale_7",
                                       "sphingomyelin_3" = "SBPMale_8",
                                       "vanillylmandelate" = "SBPMale_9",
                                       "3-hydroxy-3-methylglutarate" = "SBPMale_10",
                                       "linolenoylcarnitine" = "SBPFem_1",
                                       "hydroxyphenylacetylglutamine" = "SBPFem_2",
                                       "vanillactate" = "SBPFem_3",
                                       "DMTPA" = "SBPFem_4",
                                       "linoleoylcarnitine_2" = "SBPFem_5",
                                       "N-acetylneuraminate" = "HRVSDNN_1",
                                       "isobutyrylcarnitine" = "HRVSDNN_2",
                                       "gentisate" = "HRVSDNN_3",
                                       "cholestenoate" = "HRVSDNN_4",
                                       "cysteine" = "HRVSDNN_5",
                                       "hydroxylaurate" = "HRVSDNN_6",
                                       "phenylacetate" = "HRVSDNN_7",
                                       "sphingomyelin_4" = "HRVSDNN_8",
                                       "1-oleoyl-GPI" = "HRVSDNN_9",
                                       "glycerate" = "HRVSDNN_10",
                                       "N-acetylcitrulline" = "DBPFem_1",
                                       "etiocholanoloneglucuronide" = "DBPFem_2",
                                       "androsteronesulfate" = "DBPFem_4",
                                       "serotonin" = "DBPFem_5",
                                       "cortoloneglucuronide" = "DBPFem_6",
                                       "pregnanediol3glucuronide" = "DBPFem_7",
                                       "vanillactate" = "DBPFem_8",
                                       "5alphaandrostan" = "DBPFem_9",
                                       "epiandrosteronesulfate" = "DBPFem_10")
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
        met = as.factor(met),
        outcome = c(rep("SBP", 4000), rep("DBP", 1800), rep("HRV", 2000)),
        outcome = as.factor(outcome),
        outcome = fct_relevel(outcome, "SBP", after = 0L)
    )

expvarfilter <- df3 %>% filter(median_ev > 0) %>% select(met)
df4 <- df4 %>% filter(met %in% expvarfilter$met)

(pl3 <- df4 %>% 
        ggplot(., aes(x = met, fill = met, y = ev))+
        geom_hline(yintercept=0.0, linetype="dashed")+
        annotate("rect", xmin=-Inf, xmax=Inf, 
                 ymin=0.0, ymax=-30, alpha=0.5, fill="grey")+
        geom_half_violin(side = "r", scale = "width", alpha = 0.5) +
        geom_half_boxplot(side = "r", fill = "white", width = 0.2, outlier.shape = NA,
                          errorbar.draw = FALSE) +
        ggbeeswarm2::geom_beeswarm(aes(color = met), method = "hex", size = 0.9,
                                   spacing = 0.6, side = -1L, alpha = 0.5)+
        labs(title = 'Explained variance of metabolites from microbiota composition',
             x = '', y = 'Explained variance (%)', linetype = '', size = '') +
        scale_y_continuous(limits = c(-30, 50), breaks = seq(-30, 50, by = 5))+
        scale_color_aaas(guide = "none") +
        scale_fill_aaas(guide = "none") +
        facet_grid(.~outcome, scales = "free", space = "free") +
        theme_Publication()+
        theme(axis.text.x = element_text(angle = 45, hjust = 1)))

ann_text <- data.frame()
for(g in expvarfilter$met){
        thisev <- df3 %>% 
            filter(met == g) %>% 
            select(median_ev, outcome)
        b <- data.frame(outcome = thisev[[2]], met = g,
                        lab = format(round(thisev[[1]], 1), nsmall=1), 
                        ev = 50)
        ann_text <- rbind(ann_text,b)
}
ann_text <- ann_text %>% mutate(met = fct_rev(met))

pl3 + geom_text(data = ann_text , mapping = aes(label = str_c(lab, "%")))

ggsave("../results/220814_expvar_met_median.pdf", width = 7, height = 5)
ggsave("../results/220814_expvar_met_median.svg", width = 7, height = 5)
