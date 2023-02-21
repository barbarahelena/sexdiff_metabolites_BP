## Linear regression models: metabolites and microbes
## Barbara Verhaar, b.j.verhaar@amsterdamumc.nl

## Libraries
pacman::p_load(rio, haven, Hmisc, tidyverse, tidyselect, tableone, ggsci, ggpubr,
               kableExtra, knitr, naniar, rms, clipr, broom)

## Functions
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

prepare <- function(bestpred, helius, asvs){
    best <- bestpred %>% 
        select(ASV=FeatName, RelFeatImp) %>% 
        arrange(-RelFeatImp) %>% 
        slice(1:10)
    best_asv <- asvs %>% 
        select(all_of(best$ASV))
    best_asv <- best_asv[, best$ASV]
    best_asv$ID <- as.integer(rownames(best_asv))
    helius_best <- left_join(helius, best_asv, by='ID')
    return(helius_best)
}

metab_func <- function(bestpred, asvs){
    best <- bestpred %>% 
        select(ASV=FeatName, RelFeatImp) %>% 
        arrange(-RelFeatImp) %>% 
        slice(1:20)
    best_asv <- asvs %>% 
        select(all_of(best$ASV))
    best_asv$ID <- as.integer(rownames(best_asv))
    return(best_asv)
}

lin_models <- function(met, df, dfname, asv, writetable = FALSE, figure = TRUE){
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
                    axis.title.x = element_text(vjust = -0.2, size = rel(0.8)),
                    axis.text.y = element_text(size=rel(0.7)), 
                    axis.line.x = element_line(colour="black"),
                    axis.ticks.x = element_line(),
                    axis.ticks.y = element_blank(),
                    panel.grid.major = element_line(colour="#f0f0f0"),
                    panel.grid.minor = element_blank(),
                    legend.key = element_rect(colour = NA),
                    legend.position = "right",
                    #legend.direction = "horizontal",
                    legend.text = element_text(size=rel(0.8), hjust = 0),
                    legend.key.size= unit(0.2, "cm"),
                    legend.spacing  = unit(0, "cm"),
                    legend.title = element_blank(),
                    plot.margin=unit(c(5,5,5,5),"mm"),
                    strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
                    strip.text = element_text(face="bold"),
                    plot.caption = element_text(face = "italic", size=rel(0.6))
            ))
    } 
    
    if(writetable == FALSE & figure == FALSE){
        print("No output set!")
    }    else{
        ## Variable selection
        dfsub <- df %>% select(all_of(met), 
                               Age, Sex, CKDEPI, BMI, Microalb, DM, CurrSmoking, tail(names(.), 10))
        
        ## Models
        res_lin <- c()
        dftemp <- dfsub
        dftemp$met <- scale(dftemp[,met])
            for (i in c(9:18)){
                dftemp$asva <- NULL 
                dftemp$asva <- ((dftemp[,i][[1]]+1) / 14932) * 100
                m1 <- lm(met ~ log10(asva) + Age + Sex + BMI + CKDEPI + DM + Microalb + CurrSmoking, data=dftemp)
                m4 <- lm(met ~ log10(asva) + Age + Sex + BMI + CKDEPI + DM + Microalb + CurrSmoking + log(asva)*Sex, data = dftemp)
                
                asv <- colnames(dftemp)[i]
                taxasv <- tax$Tax[which(tax$ASV==asv)]
                
                m1 <- tidy(m1, conf.int=T)[2,]
                m4 <- tidy(m4)[11,]
                
                resRow <- cbind(asv, taxasv, m1$estimate, m1$conf.low, m1$conf.high, m1$p.value,
                                m4$p.value)
                colnames(resRow) <- c("ASV", "tax", "m1-est", "m1-l95", "m1-u95", "m1-p",
                                      "interaction")
                res_lin <- rbind(res_lin, resRow)
            }
        res_lin <- as.data.frame(res_lin)
        
        afronden <- function(x, no) return(as.numeric(format(round(x, no),no)))

        res_lin2 <- res_lin %>% 
            mutate_at(c(3:7), as.character) %>% 
            mutate_at(c(3:7), as.numeric) %>% 
            mutate_at(c(3:5), ~ afronden(., no = 2)) %>% 
            mutate_at(c(6,7), ~ afronden(., no = 5))
        
        ## Output
        if(figure == TRUE){
            res_lin2 <- res_lin2 %>%
                mutate(tax = factor(make.unique(tax), levels = rev(make.unique(tax)))) %>% 
                pivot_longer(c(3:6), names_to=c("model", "cat"), 
                             names_prefix="m", 
                             names_sep='-',
                             values_to="value") %>% 
                pivot_wider(names_from = cat, values_from = value) %>% 
                mutate(model = factor(model, levels = c("1"), 
                                      labels = c("Age, Sex, BMI, eGFR, DM, Alb")),
                        ASV = factor(ASV),
                       ASV = fct_rev(fct_inorder(ASV))
                )
            
            li <- res_lin2 %>% filter(interaction < 0.05) %>% select(ASV)
            print(res_lin2)
            print(li)
            ylab <- "Change in metabolite (SD) per log10 increase in ASV"
            colors <- pal_lancet()(1)
            
            pl1 <- res_lin2 %>%  
                ggplot(., aes(x=tax,y=est, color = model)) +
                geom_hline(yintercept = 0, color = "grey40") +
                geom_point(position=position_dodge(-0.7)) +
                geom_errorbar(aes(ymin=l95,ymax=u95,width=.3), position=position_dodge(-0.7)) +
                coord_flip()+
                theme_Publication()+
                labs(x = "", y = ylab, 
                     title = str_c(dfname)) +
                scale_y_continuous(breaks = c(-0.75, -0.5, -0.25,0,0.25, 0.5, 0.75), limits = c(-0.9, 0.9)) +
                scale_color_manual(values = colors, guide = "none") +
                theme(legend.title = element_blank(),
                      axis.text.y = element_text(face = ifelse(levels(res_lin2$ASV) %in% li$ASV, "bold", "plain"))
                      )
            return(pl1)
        }
        if(writetable == TRUE){
            openxlsx::write.xlsx(res_lin2, file.path("results/", str_c(dfname,"_linreg.xlsx")))
            return(res_lin2)
        }
    }
}

lin_strata <- function(met,df, dfname, asv, writetable = FALSE, figure = TRUE){
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
                    axis.title.x = element_text(vjust = -0.2, size = rel(0.8)),
                    axis.text.y = element_text(size=rel(0.7)),
                    axis.line.x = element_line(colour="black"),
                    axis.ticks.x = element_line(),
                    axis.ticks.y = element_blank(),
                    panel.grid.major = element_line(colour="#f0f0f0"),
                    panel.grid.minor = element_blank(),
                    legend.key = element_rect(colour = NA),
                    legend.position = "right",
                    #legend.direction = "horizontal",
                    legend.text = element_text(size=rel(0.8), hjust = 0),
                    legend.key.size= unit(0.2, "cm"),
                    legend.spacing  = unit(0, "cm"),
                    legend.title = element_blank(),
                    plot.margin=unit(c(5,5,5,5),"mm"),
                    strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
                    strip.text = element_text(face="bold"),
                    plot.caption = element_text(face = "italic", size=rel(0.6))
            ))
    }

    if(writetable == FALSE & figure == FALSE){
        print("No output set!")
    }    else{
        ## Variable selection
        dfsub <- df %>% select(all_of(met),
                               Age, Sex, CKDEPI, BMI, DM, Microalb, CurrSmoking, tail(names(.), 10))

        ## Models
        res_lin <- c()
        for(a in levels(dfsub$Sex)){
        dfsub$met <- scale(dfsub[,met])
        dftemp <- dfsub %>% filter(Sex == a)
        for (i in c(9:18)){
            dftemp$asva <- NULL
            dftemp$asva <- ((dftemp[,i][[1]]+1) / 14932) * 100
            dfsub$asva <- NULL
            dfsub$asva <- ((dfsub[,i][[1]]+1) / 14932) * 100
            m1 <- lm(met ~ log10(asva) + Age + BMI + CKDEPI + DM + Microalb + CurrSmoking, data=dftemp)
            m2 <- lm(met ~ log10(asva) + Age + BMI + CKDEPI + DM + Microalb + CurrSmoking + log(asva)*Sex, 
                     data = dfsub)

            sex <- a
            asv <- colnames(dftemp)[i]
            taxasv <- tax$Tax[which(tax$ASV==asv)]

            m1 <- tidy(m1, conf.int=T)[2,]
            #print(tidy(m2))
            m2 <- tidy(m2)[11,]

            resRow <- cbind(sex, asv, taxasv, m1$estimate, m1$conf.low, m1$conf.high, m1$p.value,
                            m2$p.value)
            colnames(resRow) <- c("Sex", "ASV", "tax", "m1-est", "m1-l95", "m1-u95", "m1-p",
                                  "interaction")
            res_lin <- rbind(res_lin, resRow)
            }
        }
        res_lin <- as.data.frame(res_lin)

        afronden <- function(x, no) return(as.numeric(format(round(x, no),no)))

        res_lin2 <- res_lin %>%
            mutate_at(c(4:8), as.character) %>%
            mutate_at(c(4:8), as.numeric) %>%
            mutate_at(c(4:6), ~ afronden(., no = 2)) %>%
            mutate_at(c(7,8), ~ afronden(., no = 5))

        ## Output
        if(figure == TRUE){
            res_lin2 <- res_lin2 %>%
                mutate(ASV = factor(ASV),
                       ASV = fct_rev(fct_inorder(ASV)),
                       Sex = as.factor(Sex)
                       ) %>%
                pivot_longer(c(4:7), names_to=c("model", "cat"),
                             names_prefix="m",
                             names_sep='-',
                             values_to="value") %>%
                pivot_wider(names_from = c(Sex, cat), values_from = value) %>%
                mutate(tax = factor(make.unique(tax), levels = rev(make.unique(tax)))) %>%
                pivot_longer(c(5:12), names_to=c("Sex", "cat"), names_sep = "_", values_to = "value")%>%
                pivot_wider(names_from = cat, values_from = value)


            li <- res_lin2 %>% filter(interaction < 0.05) %>% select(ASV) %>% filter(duplicated(.))
            print(li)
            ylab <- "Change in metabolite (SD) per log10 increase in ASV"
            colors <- rev(pal_lancet()(2))

            pl1 <- res_lin2 %>%
                ggplot(., aes(x=tax,y=est, color = Sex)) +
                geom_hline(yintercept = 0, color = "grey40") +
                geom_point(position=position_dodge(-0.5)) +
                geom_errorbar(aes(ymin=l95,ymax=u95,width=.3), position=position_dodge(-0.5)) +
                coord_flip()+
                theme_Publication()+
                labs(x = "", y = ylab,
                     title = str_c(dfname)) +
                #scale_y_continuous(breaks = c(-6, -4, -2, 0, 2, 4, 6, 8)) +
                scale_color_manual(values = colors) +
                theme(legend.title = element_blank(),
                      axis.text.y = element_text(face = ifelse(levels(res_lin2$ASV) %in% li$ASV, "bold", "plain"))
                      )
            return(pl1)
        }
        if(writetable == TRUE){
            openxlsx::write.xlsx(res_lin2, file.path("results/", str_c(dfname,"strata_sphingo_linreg.xlsx")))
        }
    }
}

## Opening HELIUS file, metabolite data and best predictor files
heliusData <- readRDS('../data/helius_malefemale.RDS')
metabolites <- readRDS('../data/HELIUS_plasma_metabolites.RDS')
namemet <- c("sphingomyelin (d18:1/20:2, d18:2/20:1, d16:1/22:2)*",
             "sphingomyelin (d18:2/24:2)*",
             "sphingomyelin (d18:1/22:2, d18:2/22:1, d16:1/24:2)*",
             "vanillylmandelate (VMA)",
             "phenylacetate",
             "gentisate",
             "sphingomyelin (d18:1/20:0, d16:1/22:0)*",
             "glycerate",
             "serotonin",
             "epiandrosterone sulfate")
metabolites2 <- metabolites[,namemet]
metabolites2$ID <- as.integer(rownames(metabolites2))
best_sphingo <- rio::import('Metabolites/SBP_metabolites/Male_1/output_XGB_reg_male_1_sphingomyelin_2022_03_30__16-58-21/feature_importance.txt')
best_sphingo2 <- rio::import('Metabolites/SBP_metabolites/Male_7/output_XGB_reg_sphingomyelin2_Male_7_2022_06_09__10-30-52/feature_importance.txt')
best_sphingo3 <- rio::import('Metabolites/SBP_metabolites/Male_8/output_XGB_reg_sphingomyelin3_Male_8_2022_06_09__11-01-30/feature_importance.txt')
best_vma <- rio::import('Metabolites/SBP_metabolites/Male_9/output_XGB_reg_vanillylmandelate_Male_9_2022_06_09__11-35-22/feature_importance.txt')
best_gentisate <- rio::import('Metabolites/HRV_metabolites/SDNN_3/output_XGB_reg_gentisate_SDNN_3_2022_06_08__13-53-41/feature_importance.txt')
best_phenylacetate <- rio::import('Metabolites/HRV_metabolites/SDNN_7/output_XGB_reg_phenylacetate_SDNN_7_2022_06_03__19-44-50/feature_importance.txt')
best_sphingo4 <- rio::import("Metabolites/HRV_metabolites/SDNN_8/output_XGB_reg_sphingomyelin_SDNN_8_2022_06_03__20-11-02/feature_importance.txt")
best_glycerate <- rio::import("Metabolites/HRV_metabolites/SDNN_10/output_XGB_reg_glycerate_SDNN_10_2022_06_03__21-08-21/feature_importance.txt")
best_serotonin <- rio::import("Metabolites/DBP_metabolites/Fem_5/output_XGB_reg_serotonin_fem_5_2022_08_11__14-58-39/feature_importance.txt")
best_epiandosterone <- rio::import("Metabolites/DBP_metabolites/Fem_10/output_XGB_reg_fem_10_epiandrosteronesulfate_2022_08_12__17-58-03/feature_importance.txt")
asvtable <- rio::import("../data/HELIUS_ASV_table_all.RDS")
asvdf <- as.data.frame(asvtable)
asvdf <- asvdf %>% filter(rownames(.) %in% heliusData$ID)
tax <- rio::import("../data/Taxonomy_all_ASVs_Helius.RDS")

helius_sphingo <- left_join(heliusData, metabolites2, by='ID')

## Prepare metabolite files
helius_sphingo_tot <- prepare(best_sphingo, helius_sphingo, asvdf)
helius_sphingo2_tot <- prepare(best_sphingo2, helius_sphingo, asvdf)
helius_sphingo3_tot <- prepare(best_sphingo3, helius_sphingo, asvdf)
helius_vma_tot <- prepare(best_vma, helius_sphingo, asvdf)
helius_phenylacetate_tot <- prepare(best_phenylacetate, helius_sphingo, asvdf)
helius_gentisate_tot <- prepare(best_gentisate, helius_sphingo, asvdf)
helius_sphingo4_tot <- prepare(best_sphingo4, helius_sphingo, asvdf)
helius_glycerate_tot <- prepare(best_glycerate, helius_sphingo, asvdf)
helius_serotonin_tot <- prepare(best_serotonin, helius_sphingo, asvdf)
helius_epiandosterone_tot <- prepare(best_epiandosterone, helius_sphingo, asvdf)

(pl1 <- lin_models(met="sphingomyelin (d18:1/20:2, d18:2/20:1, d16:1/22:2)*",df=helius_sphingo_tot, dfname="SM 38:3", writetable = FALSE))
ggsave("results/lm_SM_38_3_best10_mf.pdf", width = 5, height = 3, device = "pdf")

(pl2 <- lin_models(met="sphingomyelin (d18:2/24:2)*",df=helius_sphingo2_tot, dfname="SM 42:4", writetable = FALSE))
ggsave("results/lm_SM_42_4_best10_mf.pdf", width = 5, height = 3, device = "pdf")

(pl3 <- lin_models(met="sphingomyelin (d18:1/22:2, d18:2/22:1, d16:1/24:2)*",df=helius_sphingo3_tot, dfname="SM 40:3", writetable = FALSE))
ggsave("results/lm_SM_40_3_best10_mf.pdf", width = 5, height = 3, device = "pdf")

(pl4 <- lin_models(met="vanillylmandelate (VMA)",df=helius_vma_tot, dfname="vanillylmandelate (VMA)", writetable = FALSE))
ggsave("results/lm_vma_best10_mf.pdf", width = 5, height = 3, device = "pdf")

(pl5 <- lin_models(met="phenylacetate", df=helius_phenylacetate_tot, dfname="phenylacetate", writetable = FALSE))
ggsave("results/lm_phenylacetate_best10.pdf", width = 5, height = 3, device = "pdf")

(pl6 <- lin_models(met="gentisate",df=helius_gentisate_tot, dfname="gentisate", writetable = FALSE))
ggsave("results/lm_gentisate_best_10.pdf", width = 5, height = 3, device = "pdf")

(pl7 <- lin_models(met="sphingomyelin (d18:1/20:0, d16:1/22:0)*",df=helius_sphingo4_tot, dfname="SM 38:1", writetable = FALSE))
ggsave("results/lm_SM_38_1_best_10.pdf", width = 5, height = 3, device = "pdf")

(pl8 <- lin_models(met="glycerate",df=helius_glycerate_tot, dfname="glycerate", writetable = FALSE))
ggsave("results/lm_glycerate_best_10.pdf", width = 5, height = 3, device = "pdf")

(pl9 <- lin_models(met="serotonin",df=helius_serotonin_tot, dfname="serotonin", writetable = FALSE))
ggsave("results/lm_serotonin_best_10.pdf", width = 5, height = 3, device = "pdf")

(pl10 <- lin_models(met="epiandrosterone sulfate",df=helius_epiandosterone_tot, dfname="epiandosterone", writetable = FALSE))
ggsave("results/lm_epiandosterone_best_10.pdf", width = 5, height = 3, device = "pdf")

pl <- ggarrange(pl1, pl2, pl3, pl4, pl9, pl10, pl5, pl6, pl7, pl8,
                nrow = 5, ncol = 2, labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J"))
ggsave("results/220814_lm_metabolites_top10.pdf", height = 12, width = 10)

(pl11 <- lin_strata(met="sphingomyelin (d18:1/20:2, d18:2/20:1, d16:1/22:2)*",
                    df=helius_sphingo_tot, dfname="SM 38:3", writetable = FALSE))
ggsave("results/lm_SM_38_3_best10_mf.pdf", width = 5, height = 3, device = "pdf")
(pl12 <- lin_strata(met="sphingomyelin (d18:2/24:2)*", df=helius_sphingo2_tot, 
                    dfname="SM 42:4", writetable = FALSE))
ggsave("results/lm_sphingomyelin_42_4_best10_mf.pdf", width = 5, height = 3, device = "pdf")
(pl13 <- lin_strata(met="sphingomyelin (d18:1/22:2, d18:2/22:1, d16:1/24:2)*",df=helius_sphingo3_tot, dfname="SM 40:3", writetable = FALSE))
ggsave("results/lm_SM_40_3_best_10_mf.pdf", width = 5, height = 3, device = "pdf")
(pl14 <- lin_strata(met="vanillylmandelate (VMA)",df=helius_vma_tot, 
                    dfname="vanillylmandelate (VMA)", writetable = FALSE))
ggsave("results/lm_vma_best_10_mf.pdf", width = 5, height = 3, device = "pdf")
(pl15 <- lin_strata(met="phenylacetate",df=helius_phenylacetate_tot, dfname="phenylacetate", 
                    writetable = FALSE))
ggsave("results/lm_phenylacetate_best_10_mf.pdf", width = 5, height = 3, device = "pdf")
(pl16 <- lin_strata(met="gentisate",df=helius_gentisate_tot, dfname="gentisate", 
                    writetable = FALSE))
ggsave("results/lm_gentisate_best_10_mf.pdf", width = 5, height = 3, device = "pdf")
(pl17 <- lin_strata(met="sphingomyelin (d18:1/20:0, d16:1/22:0)*",df=helius_sphingo4_tot, dfname="SM 38:1", writetable = FALSE))
ggsave("results/lm_SM_38_1_best_10_mf.pdf", width = 5, height = 3, device = "pdf")
(pl18 <- lin_strata(met="glycerate",df=helius_glycerate_tot, dfname="glycerate", 
                    writetable = FALSE))
ggsave("results/lm_glycerate_best_10_mf.pdf", width = 5, height = 3, device = "pdf")
(pl19 <- lin_strata(met="serotonin",df=helius_serotonin_tot, dfname="serotonin", 
                    writetable = FALSE))
ggsave("results/lm_serotonin_best_10_mf.pdf", width = 5, height = 3, device = "pdf")
(pl20 <- lin_strata(met="epiandrosterone sulfate",df=helius_epiandosterone_tot, dfname="epiandosterone", 
                    writetable = FALSE))
ggsave("results/lm_epiandosterone_best_10_mf.pdf", width = 5, height = 3, device = "pdf")

pl <- ggarrange(pl11, pl12, pl13, pl14, pl19, pl20, pl15, pl16, pl17, pl18,
                nrow = 5, ncol = 2, labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J"))
ggsave("results/220814_lm_metabolites_top10_mf.pdf", height = 12, width = 10)

## Tables
(pl1 <- lin_models(met="sphingomyelin (d18:1/20:2, d18:2/20:1, d16:1/22:2)*",df=helius_sphingo_tot, dfname="SM 38:3", writetable = TRUE, figure = FALSE))
(pl2 <- lin_models(met="sphingomyelin (d18:2/24:2)*",df=helius_sphingo2_tot, dfname="SM 42:4", writetable = TRUE, figure = FALSE))
(pl3 <- lin_models(met="sphingomyelin (d18:1/22:2, d18:2/22:1, d16:1/24:2)*",df=helius_sphingo3_tot, dfname="SM 40:3", writetable = TRUE, figure = FALSE))
(pl4 <- lin_models(met="vanillylmandelate (VMA)",df=helius_vma_tot, dfname="vanillylmandelate (VMA)", writetable = TRUE, figure = FALSE))
(pl5 <- lin_models(met="phenylacetate", df=helius_phenylacetate_tot, dfname="phenylacetate", writetable = TRUE, figure = FALSE))
(pl6 <- lin_models(met="gentisate",df=helius_gentisate_tot, dfname="gentisate", writetable = TRUE, figure = FALSE))
(pl7 <- lin_models(met="sphingomyelin (d18:1/20:0, d16:1/22:0)*",df=helius_sphingo4_tot, dfname="SM 38:1", writetable = TRUE, figure = FALSE))
(pl8 <- lin_models(met="glycerate",df=helius_sphingo2_tot, dfname="glycerate", writetable = TRUE, figure = FALSE))
