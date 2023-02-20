## Table 1 - 2 - 3 (descriptive characteristics)
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
helius <- readRDS("../data/helius_malefemale.RDS")

#### Table 1 ####
table1 <- helius %>% 
    select(Age, Age_cat, Sex, Ethnicity, EthnAfr, BMI, CurrSmoking, CVD, DM, HT, 
           AntiHT, SBP, DBP, BRS, SDNN, CKDEPI, ACR_KDIGO, chol, HDL, LDL, HDL, Trig) %>% 
    CreateTableOne(data=., strata = 'Sex', test = TRUE, addOverall = TRUE) %>% 
    print(nonnormal=c("FramRisk"))
# write_clip(kableone(table1)) ### paste and and save in excel
write.csv2(table1, "results/table1.csv")

table2 <- helius %>% 
    filter(!is.na(BRS)) %>% 
    select(Age, Age_cat, Sex, Ethnicity, BMI, CurrSmoking, CVD, DM, HT, 
           AntiHT, SBP, DBP, BRS, SDNN, CKDEPI, ACR_KDIGO) %>% 
    CreateTableOne(data=., strata = 'Sex', test = TRUE, addOverall = TRUE) %>% 
    print(nonnormal=c("FramRisk", "Trig"))
# write_clip(kableone(table2)) ### paste and and save in excel
write.csv2(table2, "results/table2.csv")

table3 <- helius %>% 
    filter(!is.na(BRS) & Age < 55) %>% 
    select(Age, Age_cat, Sex, EthnAfr, BMI, CurrSmoking, CVD, DM, HT, 
           AntiHT, SBP, DBP, BRS, MDRD, ACR_KDIGO, HbA1C, LDL, Trig, FramRisk) %>% 
    CreateTableOne(data=., strata = 'Sex', test = TRUE, addOverall = TRUE) %>% 
    print(nonnormal=c("FramRisk", "Trig"))
# write_clip(kableone(table3)) ### paste and and save in excel
write.csv2(table3, "results/table3.csv")
