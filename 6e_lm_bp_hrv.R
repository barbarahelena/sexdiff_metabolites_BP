## LM
## b.j.verhaar@amsterdamumc.nl

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

prepare <- function(bestpred, metabolites, helius){
    best <- bestpred %>% 
        select(Metabolite=FeatName, RelFeatImp) %>% 
        arrange(-RelFeatImp) %>% 
        slice(1:10)
    best_met <- metabolites %>% 
        inner_join(., best, by='Metabolite') %>% 
        arrange(-RelFeatImp)
    rownames(best_met) <- best_met$Metabolite
    best_met$Metabolite <- NULL
    best_met$RelFeatImp <- NULL
    best_met <- as.data.frame(t(as.matrix(best_met)))
    best_met$ID <- as.integer(rownames(best_met))
    helius_best <- left_join(helius, best_met, by='ID')
    return(helius_best)
}

prepare2 <- function(bestpred, bestpred2, metabolites, helius){
    best <- bestpred %>% 
        select(Metabolite=FeatName, RelFeatImp) %>% 
        arrange(-RelFeatImp) %>% 
        slice(1:10)
    best2 <- bestpred2 %>% 
        select(Metabolite=FeatName, RelFeatImp) %>% 
        arrange(-RelFeatImp) %>% 
        slice(1:10)
    besttot <- full_join(best, best2, by = "Metabolite")
    best_met <- metabolites %>% 
        inner_join(., besttot, by='Metabolite') %>% 
        arrange(-RelFeatImp.x)
    rownames(best_met) <- best_met$Metabolite
    best_met$Metabolite <- NULL
    best_met$RelFeatImp <- NULL
    best_met <- as.data.frame(t(as.matrix(best_met)))
    best_met$ID <- as.integer(rownames(best_met))
    helius_best <- left_join(helius, best_met, by='ID')
    return(helius_best)
}

metab_func <- function(bestpred, metabolites){
    best <- bestpred %>% 
        select(Metabolite=FeatName, RelFeatImp) %>% 
        arrange(-RelFeatImp) %>% 
        slice(1:10)
    best_met <- metabolites %>% 
        inner_join(., best, by='Metabolite') %>% 
        arrange(-RelFeatImp)
    rownames(best_met) <- best_met$Metabolite
    best_met$Metabolite <- NULL
    best_met$RelFeatImp <- NULL
    best_met <- as.data.frame(t(as.matrix(best_met)))
    best_met$ID <- as.integer(rownames(best_met))
    return(best)
}

metab_func2 <- function(bestpred, bestpred2, metabolites){
    best <- bestpred %>% 
        select(Metabolite=FeatName, RelFeatImp) %>% 
        arrange(-RelFeatImp) %>% 
        slice(1:10)
    best2 <- bestpred2 %>% 
        select(Metabolite=FeatName, RelFeatImp) %>% 
        arrange(-RelFeatImp) %>% 
        slice(1:10)
    best_tot <- full_join(best, best2, by = "Metabolite")
    return(best_tot)
}

lin_strata <- function(df, dfname, metab, outcome, writetable = FALSE, figure = TRUE){
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
                    axis.title.x = element_text(vjust = -0.2),
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
        dfsub <- df %>% select(all_of(outcome), Age, Sex, Ethnicity, CKDEPI, BMI, Microalb, DM, tail(names(.), 10))
        dfsub$out <- dfsub[[outcome]]
        
        ## Models
        res_lin <- c()
        for (a in levels(dfsub$Sex)){
            dftemp <- dfsub %>% filter(Sex==a)
            for (i in c(9:18)){
                dftemp$met <- NULL 
                dftemp$met <- dftemp[[i]]
                dfsub$met <- dfsub[[i]]
                dftemp$metlog <- log(dftemp$met)
                dfsub$metlog <- log(dfsub$met)
                m1 <- lm(log10(out) ~ scale(met) + Age + BMI + CKDEPI + Microalb, data=dftemp)
                m2 <- lm(log10(out) ~ scale(met) + Age + scale(met)*Sex + CKDEPI + Microalb + BMI, data=dfsub)
                metname <- colnames(dftemp)[i]
                m1 <- tidy(m1, conf.int=T)[2,]
                m2 <- tidy(m2, conf.int=T)[8,]
                resRow <- cbind(a, metname, m1$estimate, m1$conf.low, 
                                m1$conf.high, m1$p.value, m2$p.value)
                colnames(resRow) <- c("Sex","Metabolite", "est", 
                                      "l95", "u95", "p", "interaction")
                res_lin <- rbind(res_lin, resRow)
            }
        }
        res_lin <- as.data.frame(res_lin)
        
        afronden <- function(x, no) return(as.numeric(format(round(x, no),no)))

        res_lin2 <- res_lin %>% 
            mutate_at(c(3:7), as.character) %>% 
            mutate_at(c(3:7), as.numeric) %>% 
            mutate_at(c(3:5), ~ afronden(., no = 2)) %>% 
            mutate_at(c(6:7), ~ afronden(., no = 5))
        
        ## Output
        if(figure == TRUE){
            labslist <- levels(dfsub$Ethnicity)
            res_lin2 <- res_lin2 %>%
                mutate(Metabolite = factor(Metabolite, levels = metab),
                       Metabolite = fct_rev(Metabolite))
            
            ylab <- "log10 change per SD increase"
            colors <- pal_jco()(4)[c(4,1)]
            li <- res_lin2 %>% filter(interaction < 0.1) %>% select(Metabolite) %>% filter(duplicated(.))
            
            pl1 <- ggplot(res_lin2, aes(x=Metabolite,y=est, color=Sex)) +
                geom_hline(yintercept = 0, color = "grey40") +
                geom_point(position=position_dodge(-0.5)) +
                geom_errorbar(aes(ymin=l95,ymax=u95,width=.3), position=position_dodge(-0.5)) +
                coord_flip()+
                theme_Publication()+
                labs(x = "", y = ylab, 
                     title = str_c(dfname)) +
                scale_y_continuous(breaks = c(-0.5, -0.4, -0.3, -0.2, -0.1, 0, 
                                              0.1, 0.2, 0.3, 0.4, 0.5)) +
                scale_color_manual(values = colors) +
                theme(
                    axis.text.y = element_text(face = ifelse(levels(res_lin2$Metabolite) %in% li$Metabolite, "bold", "plain")),
                    legend.title = element_blank())
            return(pl1)
        }
        if(writetable == TRUE){
            openxlsx::write.xlsx(res_lin2, 
                                 str_c("results/220603_",dfname,"_linreg.xlsx")
                                 )
        }
    }
}

## Opening HELIUS file, metabolite data and best predictor files
heliusData <- readRDS('../data/helius_malefemale.RDS')
sampleList <- rio::import('../data/EDTA_samples.xlsx')[[3]]
summary_metabolites <- rio::import('../data/Info_plasma_metabolites.xlsx')
metabolites <- rio::import('../data/HELIUS_EDTA_plasma_metabolites.xlsx')
heliusHrv <- heliusData %>% filter(!is.na(RMSDD)) %>% filter(Age < 55)

best_fem_sdnn <- rio::import('SDNN/Female/output_XGB_reg_SDNN_female_2022_06_03__10-09-11/feature_importance.txt')
best_male_sdnn <- rio::import('SDNN/Male/output_XGB_reg_SDNN_male_2022_06_02__18-27-11/feature_importance.txt')
best_fem_brs <- rio::import('BRS/Female/output_XGB_reg_BRSFem_2022_03_17__19-30-01/feature_importance.txt')
best_male_brs <- rio::import('BRS/Male/output_XGB_reg_BRSMale_2022_03_18__11-14-28/feature_importance.txt')

## Prepare metabolite files
# helius_fem_sdnn <- prepare(best_fem_sdnn, metabolites, heliusHrv)
helius_male_sdnn <- prepare(best_male_sdnn, metabolites, heliusData)
# helius_fem_brs <- prepare(best_fem_brs, metabolites, heliusData)
helius_male_brs <- prepare(best_male_brs, metabolites, heliusData)

# bestfemsdnn <- metab_func(best_fem_sdnn, metabolites)
bestmalesdnn <- metab_func(best_male_sdnn, metabolites)
# bestfembrs <- metab_func(best_fem_brs, metabolites)
bestmalebrs <- metab_func(best_male_brs, metabolites)

# helius_male_sdnn <- helius_male_sdnn %>% filter(!is.na(BRS))

pl1 <- lin_strata(df=helius_male_sdnn, dfname="SDNN", 
                  bestmalesdnn$Metabolite, outcome = "SDNN")
pl2 <- lin_strata(df=helius_male_brs, dfname="xBRS", 
                  bestmalebrs$Metabolite, outcome = "BRS",
                  figure = TRUE)

lin_strata(df=helius_male_sdnn, dfname="SDNN_10", 
                  bestmalesdnn$Metabolite, outcome = "SDNN",
                  writetable = TRUE, figure = FALSE)
lin_strata(df=helius_male_brs, dfname="BRS_10", 
                  bestmalebrs$Metabolite, outcome = "BRS",
                  writetable = TRUE, figure = FALSE)

pl <- ggarrange(pl1, pl2, common.legend = TRUE, legend = "right")
annotate_figure(pl, top = text_grob("SDNN and xBRS predictors: sex differences", face = "bold"))
ggsave("results/220621_sdnnbrs_male_sexdiff_log.pdf", device = "pdf", width = 10, height = 4)


# pl3 <- lin_strata(df=helius_fem_brs, dfname="females", 
#                   bestfembrs$Metabolite, outcome = "BRS", writetable = TRUE)
# pl4 <- lin_strata(df=helius_male_brs, dfname="males", 
#                   bestmalebrs$Metabolite, outcome = "BRS", writetable = TRUE)
# 
# # lin_strata(df=helius_fem_sbp, dfname="females", writetable = TRUE, figure = FALSE)
# # lin_strata(df=helius_male_sbp, dfname="males", writetable = TRUE, figure = FALSE)
# 
# pl <- ggarrange(pl3, pl4, common.legend = TRUE, legend = "right")
# annotate_figure(pl, top = text_grob("BRS predictors: sex differences", face = "bold"))
# ggsave("results/220603_brs_sexdiff_log.pdf", device = "pdf", width = 12, height = 6)
