## Descriptives analyses sex differences project
## Barbara Verhaar, b.j.verhaar@amsterdamumc.nl

#### Libraries ####
pacman::p_load(rio, haven, Hmisc, tidyverse, tidyselect, tableone, ggsci, ggpubr,
               kableExtra, knitr, naniar, rms, clipr)

##### Functions ####
theme_Publication <- function(base_size=12, base_family="sans") {
    library(grid)
    library(ggthemes)
    (theme_foundation(base_size=base_size, base_family=base_family)
        + theme(plot.title = element_text(face = "bold",
                                          size = rel(0.8), hjust = 0.5),
                #family = 'Helvetica'
                text = element_text(),
                panel.background = element_rect(colour = NA),
                plot.background = element_rect(colour = NA),
                panel.border = element_rect(colour = NA),
                axis.title = element_text(face = "bold",size = rel(0.8)),
                axis.title.y = element_text(angle=90,vjust =2),
                axis.title.x = element_text(vjust = -0.2),
                axis.text = element_text(), 
                axis.line.x = element_line(colour="black"),
                axis.line.y = element_line(colour="black"),
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

#### Opening data files ####
helius <- readRDS(helius, "../data/helius_malefemale.RDS")

#### Plots SBP - DBP - HRV (SDNN) - BRS ####
col <- pal_lancet()(2)[c(2,1)]
pl_sbp <- ggplot(helius, aes(x = Sex, y = SBP)) +
    geom_violin(aes(fill = Sex), alpha = 0.7) +
    geom_boxplot(fill = "white", 
                 width = 0.1, outlier.shape = NA) +
    scale_x_discrete(labels = c("man" = "male", "vrouw" = "female"))+
    ggtitle("Systolic blood pressure")+
    ylab("SBP (mmHg)") +
    stat_compare_means(method = "t.test") +
    theme_Publication() +
    scale_fill_manual(values = col, guide = "none")

pl_dbp <- ggplot(helius, aes(x = Sex, y = DBP)) +
    geom_violin(aes(fill = Sex), alpha = 0.7) +
    geom_boxplot(fill = "white", width = 0.1, 
                 outlier.shape = NA) +
    scale_x_discrete(labels = c("man" = "male", "vrouw" = "female"))+
    ggtitle("Diastolic blood pressure")+
    ylab("DBP (mmHg)") +
    stat_compare_means(method = "t.test") +
    theme_Publication() +
    scale_fill_manual(values = col, guide = "none")

pl_brs <- ggplot(helius) +
    geom_violin(aes(x = Sex, y = BRS, fill = Sex), alpha = 0.7) +
    geom_boxplot(aes(x = Sex, y = BRS), fill = "white", width = 0.1, 
                 outlier.shape = NA) +
    scale_x_discrete(labels = c("man" = "male", "vrouw" = "female"))+
    ggtitle("Baroreceptor sensitivity")+
    ylab("BRS (ms/mm Hg)") +
    stat_compare_means(aes(x = Sex, y = BRS), method = "wilcox.test") +
    theme_Publication() +
    scale_fill_manual(values = col, guide = "none")

(pl_sdnn <- ggplot(helius) +
    geom_violin(aes(x = Sex, y = SDNN, fill = Sex), alpha = 0.7) +
    geom_boxplot(aes(x = Sex, y = SDNN), fill = "white", width = 0.1, 
                 outlier.shape = NA) +
    scale_x_discrete(labels = c("man" = "male", "vrouw" = "female"))+
    ggtitle("Heart rate variability")+
    ylab("SDNN") +
    stat_compare_means(aes(x = Sex, y = SDNN), method = "wilcox.test") +
    theme_Publication() +
    scale_fill_manual(values = col, guide = "none"))

#### PCA plot metabolites ####
dim(metabolites)
metabolites[1:10, 1]
mat <- metabolites[,-1]
rownames(mat) <- metabolites[,1]
mat <- t(mat)

pca <- prcomp(mat, center = T, scale. = T)
df <- as.data.frame(pca$x[, 1:5])
summary(pca)
pc1_ev <- round(summary(pca)$importance[2,1] * 100, 2)
pc2_ev <- round(summary(pca)$importance[2,2] * 100, 2)
head(df)
df$ID <- rownames(df)
df$Sex <- helius$Sex[match(df$ID, helius$ID)]

col <- pal_lancet()(2)[c(2,1)]
(plC <- ggplot(df, aes(x=PC1, y=PC2, color=Sex)) +
        geom_point() +
        ggtitle('PCA plasma metabolites') +
        scale_color_manual(values = col) +
        theme_minimal() +
        stat_ellipse() +
        labs(x=str_c('PC1 ', pc1_ev, '%'), y=str_c('PC2 ', pc2_ev, '%')) +
        theme_Publication() +
        theme(legend.title = element_blank()))

#### Differences in metabolite levels ####

# first feature imp plot classification model


# differences in three top predictors
met <- metabolites$Metabolite
metabolites$Metabolite <- NULL # remove column with names

met2 <- as.data.frame(t(as.matrix(metabolites))) # rows become columns
colnames(met2) <- met # metabolite names colnames
met2$ID <- as.integer(rownames(met2)) # IDs as rownames

tot <- left_join(met2, helius, by = "ID")

(plmet1 <- ggplot(tot %>% filter(`dihomo-linolenoylcarnitine (C20:3n3 or 6)*` < 100), 
       aes(x = Sex, y = `dihomo-linolenoylcarnitine (C20:3n3 or 6)*`)) +
    geom_violin(aes(fill = interaction(Sex, Age_cat)), position = position_dodge(1.0)) +
    geom_boxplot(aes(group = interaction(Sex, Age_cat)), 
                 fill = "white", width = 0.2, outlier.shape = NA,
                 position = position_dodge(1.0)) +
    scale_fill_manual(values = pal_jco()(4)[c(1,4)]) +
    stat_compare_means(aes(label = paste0("p = ", ..p.format..),group = interaction(Sex, Age_cat))) +
    labs(x = "Sex", y = "dihomo-linolenoylcarnitine levels",
         title = "dihomo-linolenoylcarnitine") +
    theme_Publication() +
    theme(legend.title = element_blank()))

(plmet2 <- ggplot(tot %>% filter(`dihomo-linolenoylcarnitine (C20:3n3 or 6)*` < 100), 
               aes(x = Sex, y = `dihomo-linolenoylcarnitine (C20:3n3 or 6)*`)) +
        geom_violin(aes(fill = interaction(Sex, Age_cat)), position = position_dodge(1.0)) +
        geom_boxplot(aes(group = interaction(Sex, Age_cat)), 
                     fill = "white", width = 0.2, outlier.shape = NA,
                     position = position_dodge(1.0)) +
        scale_fill_manual(values = color_list, labels = four_labels) +
        stat_compare_means(aes(label = paste0("p = ", ..p.format..),group = interaction(Sex, Age_cat))) +
        labs(x = "Sex", y = "dihomo-linolenoylcarnitine levels",
             title = "dihomo-linolenoylcarnitine") +
        theme_Publication() +
        theme(legend.title = element_blank()))

(plmet3 <- ggplot(tot %>% filter(`dihomo-linolenoylcarnitine (C20:3n3 or 6)*` < 100), 
               aes(x = Sex, y = `dihomo-linolenoylcarnitine (C20:3n3 or 6)*`)) +
        geom_violin(aes(fill = Sex), position = position_dodge(1.0)) +
        geom_boxplot(aes(group = interaction(Sex, Age_cat)), 
                     fill = "white", width = 0.2, outlier.shape = NA,
                     position = position_dodge(1.0)) +
        scale_fill_manual(values = color_list, labels = four_labels) +
        stat_compare_means(aes(label = paste0("p = ", ..p.format..))) +
        labs(x = "Sex", y = "dihomo-linolenoylcarnitine levels",
             title = "dihomo-linolenoylcarnitine") +
        theme_Publication() +
        theme(legend.title = element_blank()))

mf_diff <- ggarrange(plmet1, plmet2, plmet3)

#### Descriptive figure ####
ggarrange(ggarrange(pl_sbp, pl_dbp, pl_brs, pl_sdnn, labels = c("A", "B", "C", "D"), nrow = 1), 
          ggarrange(plC, pl_featimp, labels = c("E", "F"), nrow = 1), 
          mf_diff, labels = c("G"), nrow = 3)
ggsave("results/230217_descriptives_malefemale.pdf", width = 12, height = 9)
ggsave("results/230217_descriptives_malefemale.svg", width = 12, height = 9)
