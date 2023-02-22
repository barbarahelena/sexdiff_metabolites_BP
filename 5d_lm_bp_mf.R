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
    best_met <- metabolites[,best$Metabolite]
    best_met$ID <- as.integer(rownames(best_met))
    helius_best <- left_join(helius, best_met, by='ID')
    return(helius_best)
}

metab_func <- function(bestpred, metabolites){
    best <- bestpred %>% 
        select(Metabolite=FeatName, RelFeatImp) %>% 
        arrange(-RelFeatImp) %>% 
        slice(1:10)
    return(best)
}

lin_strata <- function(df, dfname, outcome, metab, writetable = FALSE, figure = TRUE){
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
        dfsub <- df %>% select(contains(outcome), Age, Sex, CKDEPI, Microalb, 
                               BMI, DM, CurrSmoking, AntiHT, tail(names(.), 10))
        dfsub$outcome <- dfsub[[1]]
        
        ## Models
        res_lin <- c()
        for (a in levels(dfsub$Sex)){
            dftemp <- dfsub %>% filter(Sex == a)
            for (i in c(10:19)){
                dftemp$met <- NULL 
                dftemp$met <- dftemp[,i][[1]]
                dfsub$met <- dfsub[,i][[1]]
                m1 <- lm(outcome ~ scale(met) + Age + CKDEPI + BMI + DM + 
                             Microalb, data=dftemp)
                m2 <- lm(outcome ~ scale(met) + Age + CKDEPI + BMI + DM + 
                             Microalb + scale(met)*Sex, data=dfsub)
                #print(summary(m2))
                metname <- colnames(dftemp)[i]
                m1 <- tidy(m1, conf.int=T)[2,]
                m2 <- tidy(m2, conf.int=T)[9,] 
                resRow <- cbind(a, metname, m1$estimate, m1$conf.low, m1$conf.high, m1$p.value, m2$p.value)
                colnames(resRow) <- c("Sex","Metabolite", "est", "l95", "u95", "p", "interaction")
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
            labslist <- levels(dfsub$Sex)
            res_lin2 <- res_lin2 %>%
                mutate(Metabolite = factor(Metabolite, levels = metab),
                       Metabolite = fct_rev(Metabolite))
            
            ylab <- "mmHg per SD increase"
            colors <- pal_jco()(4)[c(4,1)]
            li <- res_lin2 %>% filter(interaction < 0.1) %>% select(Metabolite) %>% filter(duplicated(.))
            
            pl1 <- ggplot(res_lin2, aes(x=Metabolite,y=est, color=Sex)) +
                geom_hline(yintercept = 0, color = "grey40") +
                geom_point(position=position_dodge(-0.5)) +
                geom_errorbar(aes(ymin=l95,ymax=u95,width=.3), position=position_dodge(-0.5)) +
                coord_flip()+
                theme_Publication()+
                labs(x = "", y = ylab, 
                     title = str_c("Metabolites ", dfname)) +
                scale_y_continuous(breaks = c(-6, -4, -2, 0, 2, 4, 6, 8, 10, 12)) +
                scale_color_manual(values = colors) +
                theme(
                    axis.text.y = element_text(face = ifelse(levels(res_lin2$Metabolite) %in% li$Metabolite, "bold", "plain")),
                    legend.title = element_blank())
            return(pl1)
        }
        if(writetable == TRUE){
            openxlsx::write.xlsx(res_lin2, file.path("results", str_c("221021_",dfname,outcome,
                                                                      "_linreg.xlsx")))
        }
    }
}

## Opening HELIUS file, metabolite data and best predictor files
heliusData <- readRDS('../data/helius_malefemale.RDS')
metabolites <- readRDS('../data/HELIUS_plasma_metabolites.RDS')
best_fem_sbp <- rio::import('SBP/Female/output_XGB_reg_Fem_SBP_2020_11_11__21-19-13/feature_importance.txt')
best_male_sbp <- rio::import('SBP/Male/output_XGB_reg_Male_SBP_2020_11_11__22-19-04/feature_importance.txt')
best_fem_dbp <- rio::import('DBP/Female/output_XGB_reg_Fem_DBP_2020_11_12__06-04-17/feature_importance.txt')
best_male_dbp <- rio::import('DBP/Male/output_XGB_reg_Male_DBP_2020_11_12__07-03-15/feature_importance.txt')

## Prepare metabolite files
helius_fem_sbp <- prepare(best_fem_sbp, metabolites, heliusData)
helius_male_sbp <- prepare(best_male_sbp, metabolites, heliusData)
helius_fem_dbp <- prepare(best_fem_dbp, metabolites, heliusData)
helius_male_dbp <- prepare(best_male_dbp, metabolites, heliusData)

bestfemsbp <- metab_func(best_fem_sbp, metabolites)
bestmalesbp <- metab_func(best_male_sbp, metabolites)
bestfemdbp <- metab_func(best_fem_dbp, metabolites)
bestmaledbp <- metab_func(best_male_dbp, metabolites)

pl1 <- lin_strata(df=helius_fem_sbp, dfname="SBP (women)", outcome = "SBP", bestfemsbp$Metabolite, writetable = FALSE)
pl2 <- lin_strata(df=helius_male_sbp, dfname="SBP (men)", outcome = "SBP", bestmalesbp$Metabolite, writetable = FALSE)

lin_strata(df=helius_fem_sbp, dfname="females", outcome = "SBP", bestfemsbp$Metabolite, 
           figure = FALSE, writetable = TRUE)
lin_strata(df=helius_male_sbp, dfname="males", outcome = "SBP", bestmalesbp$Metabolite, 
           figure = FALSE, writetable = TRUE)
lin_strata(df=helius_fem_dbp, dfname="females", outcome = "DBP", bestfemdbp$Metabolite, 
           figure = FALSE, writetable = TRUE)

pl <- ggarrange(pl1, pl2, common.legend = TRUE, legend = "right")
annotate_figure(pl, top = text_grob("SBP predictors: sex differences", face = "bold"))
ggsave("results/221021_sbp_sexdiff.pdf", device = "pdf", width = 11, height = 4)

pl3 <- lin_strata(df=helius_fem_dbp, dfname="DBP (women)", outcome = "DBP", metab = bestfemdbp$Metabolite, writetable = FALSE)
pl4 <- lin_strata(df=helius_male_dbp, dfname="DBP (men)", outcome = "DBP", bestmaledbp$Metabolite, writetable = FALSE)

plb <- ggarrange(pl3, pl4, common.legend = TRUE, legend = "right")
annotate_figure(plb, top = text_grob("DBP predictors: sex differences", face = "bold"))
ggsave("results/221021_dbp_sexdiff.pdf", device = "pdf", width = 11, height = 4)

plc <- ggarrange(pl1, pl2, pl3, nrow = 2, ncol = 2, common.legend = TRUE, legend = "bottom", 
                 labels = c("A", "B", "C"))
plc
ggsave("results/221021_sbpdbp_sexdiff.pdf", device = "pdf", width = 10, height = 7)
