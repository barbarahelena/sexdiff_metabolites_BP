## Data cleaning
## Barbara Verhaar, b.j.verhaar@amsterdamumc.nl

#### Libraries ####
pacman::p_load(rio, haven, Hmisc, tidyverse, tidyselect, tableone,
               kableExtra, knitr, naniar, rms, clipr)

#### Opening data files ####
dataDir <- '../data' 
helius_data <- read_sav(str_c(dataDir, "/201104_HELIUS data Barbara Verhaar.sav"))
sampleList <- rio::import(str_c(dataDir, '/EDTA_samples.xlsx'))[[3]]
summary_metabolites <- rio::import(str_c(dataDir,'/Info_plasma_metabolites.xlsx'))
metabolites <- rio::import(str_c(dataDir,'/HELIUS_EDTA_plasma_metabolites.xlsx'))
hrv <- rio::import("../data/20190527-resultsHRVBRS.rda")

#### Data cleaning ####
helius <- helius_data %>% 
    select(ID=Heliusnr, Age=H1_lft, Sex=H1_geslacht, Ethnicity=H1_EtnTotaal, 
           Smoking=H1_Roken, BMI=H1_LO_BMI, WHR=H1_LO_WHR, SBP=H1_LO_GemBPSysZit, 
           DBP=H1_LO_GemBPDiaZit, HT=H1_HT_SelfBPMed, DM=H1_Diabetes_SelfGlucMed,
           DMMed=H1_Diabetesmiddelen, Claudicatio=H1_CI_Rose, PossInf=H1_possINF_Rose, 
           AP=H1_AP_Rose, CVD=H1_CVD_Rose, AntiHT=H1_Antihypertensiva, 
           MDRD=H1_MDRD_eGFR, CKDEPI=H1_CKDEPI_eGFR, CKDStage=H1_CKDEPI_stage, 
           MetSyn=H1_MetSyn_MetabolicSyndrome, LDL=H1_Lab_uitslagRLDL, Trig=H1_Lab_UitslagTRIG,
           HDL=H1_Lab_UitslagHDLS,LDL=H1_Lab_uitslagRLDL, chol = H1_Lab_UitslagCHOL,
           FramRisk=H1_Fram_CVD, SCORENL=H1_SCORE_CVDmort_NL,
           ACR_KDIGO=H1_ACR_KDIGO, Microalb=H1_Microalbuminurie, 
           HbA1C=H1_Lab_UitslagIH1C, Kreat=H1_Lab_UitslagKREA_HP) %>% 
    filter(ID %in% sampleList) %>%
    mutate(
        group = case_when(
            ACR_KDIGO==1 ~ 'Controls',
            DM==1 & ACR_KDIGO==2 ~ 'CKD+T2DM',
            DM==0 & ACR_KDIGO==2 ~ 'CKD-T2DM'
        ),
        group = factor(group, levels = c('Controls', 'CKD+T2DM', 'CKD-T2DM')),
        CKD_group = case_when(
            group =='Controls' ~ 'Controls',
            group == 'CKD+T2DM' ~ 'CKD',
            group == 'CKD-T2DM' ~ 'CKD'
        ),
        CKD_group = factor(CKD_group, levels = c('Controls', 'CKD')),
        Age_cat = case_when(
            Age<=50 ~ "Young", 
            Age >50 ~ "Old"
        ),
        Age_cat = as.factor(Age_cat),
        # strange function - is because this was an SPSS file and we want to keep factor info
        Sex = as_factor(Sex, levels = c("labels"), ordered = FALSE), 
        Sex = fct_recode(Sex, "Male" = "man", "Female" = "vrouw"),
        Sex = fct_rev(Sex),
        SexBin = case_when(Sex == "Male" ~ 1,
                           Sex == "Female" ~ 0),
        SexAge = case_when(
            Sex == "Male" ~ paste0("Male"),
            Sex == "Female" & Age_cat == "Young" ~ paste0("Female <50y"),
            Sex == "Female" & Age_cat == "Old" ~ paste0("Female >50y")
        ),
        SexAge = as.factor(SexAge),
        Ethnicity = as_factor(Ethnicity, levels = c("labels"), ordered = FALSE),
        Smoking = as_factor(Smoking, levels = c("labels"), ordered = FALSE),
        CurrSmoking = fct_recode(Smoking, "Yes" = "Ja", 
                                 "No" = "Nee, ik heb nooit gerookt", 
                                 "No" = "Nee, maar vroeger wel"),
        CurrSmoking = fct_infreq(CurrSmoking),
        HT = as_factor(HT, levels = c("labels"), ordered = FALSE),
        DM = as_factor(DM, levels = c("labels"), ordered = FALSE),
        DM = fct_recode(DM, "Diabetes" = "Ja", "No diabetes" = "Nee"),
        DMMed = as_factor(DMMed, levels = c("labels"), ordered = FALSE),
        Claudicatio = as_factor(Claudicatio, levels = c("labels"), ordered = FALSE),
        PossInf = as_factor(PossInf, levels = c("labels"), ordered = FALSE),
        AP = as_factor(AP, levels = c("labels"), ordered = FALSE),
        CVD = as_factor(CVD, levels = c("labels"), ordered = FALSE),
        AntiHT = as_factor(AntiHT, levels = c("labels"), ordered = FALSE),
        CKDStage = as_factor(CKDStage, levels = c("labels"), ordered = FALSE),
        MetSyn=as_factor(MetSyn, levels = c("labels"), ordered = FALSE),
        ACR_KDIGO = as_factor(ACR_KDIGO, levels = c("labels"), ordered = FALSE),
        Microalb = as_factor(Microalb, levels = c("labels"), ordered = FALSE),
        EthnAfr = ifelse(Ethnicity == "Hind" | Ethnicity == "Dutch", "Non-African", "African")
    ) %>% 
    mutate_if(is.numeric, as.numeric) %>% 
    droplevels()

hrv <- hrv %>% filter(Heliusnr %in% sampleList) %>% 
    filter(Quality == 1) %>% 
    select(ID = Heliusnr, SDNN, BRS = meanBRS) %>% 
    mutate(logBRS = log(BRS),
           logSDNN = log(SDNN))
helius <- left_join(helius, hrv, by = "ID")

saveRDS(helius, "../data/helius_malefemale.RDS")

## Select from metabolite file only metabolites that are not xenobiotic
df_met <- readxl::read_xlsx('../data/HELIUS_EDTA_plasma_metabolites.xlsx')
met <- df_met$Metabolite
df_met$Metabolite <- NULL
head(df_met)
df_met2 <- as.data.frame(t(as.matrix(df_met)))
dim(df_met2) # 369 subjects, 890 metabolites
# colnames(df_met2)
colnames(df_met2) <- met # make metabolites column names

# Open metabolon infofile with super and subpathways
infomet <- readxl::read_xlsx('../data/Info_plasma_metabolites_b.xlsx')
colnames(infomet)
infomet <- infomet %>% select(metabolite = `BIOCHEMICAL`, sup = `SUPER PATHWAY`, sub = `SUB PATHWAY`)
infomet$sup <- as.factor(infomet$sup)
summary(infomet$sup)

# Exclude xenobiotic metabolites from dataset
noxeno <- infomet$metabolite[which(infomet$sup!='Xenobiotics')]
noxeno[1:5]
colnames(df_met2)
df_noxeno <- df_met2[,which(colnames(df_met2) %in% noxeno)]
dim(df_noxeno) # 369 rows (subjects) with 722 metabolites (no xenobiotics)
saveRDS(df_noxeno, "../data/HELIUS_plasma_metabolites.RDS")
