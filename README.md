# Sex differences in associations between metabolite profiles, blood pressure and heart rate variability: the HELIUS study

Aim of the project: to investigate sex-specific plasma metabolite profiles that are associated with blood pressure and autonomic cardiovascular control, in order to better understand sex differences in hypertension. We performed machine learning analyses to predict BP, baroreceptor sensitivity (BRS) and heart rate variability (HRV) from plasma metabolite profiles for men and women separately.

Secondary aim: to investigate the associations between gut microbiota composition and the plasma levels of the metabolites predicting BP and HRV, since this could help target future interventions.

## Data
- Clinical data including blood pressure and heart rate variability is unfortunately not publicly available because the informed consent does not allow us to make this data public. Any researcher can request the data by submitting a proposal to the HELIUS Executive Board (heliuscoordinator@amsterdamumc.nl) as outlined at http://www.heliusstudy.nl/en/researchers/collaboration. The HELIUS Executive Board will check proposals for compatibility with the general objectives, ethical approvals and informed consent forms of the HELIUS study. There are no other restrictions to obtaining the data and all data requests will be processed in the same manner. 
- Metabolomics data: 
- 16S rRNA sequencing data (gut microbiota composition): The 16S sequencing data are available in the European Genome-Phenome Archive (EGA), accession number EGAS00001002969 (https://ega-archive.org/studies/EGAS00001002969).

## RStudio and renv


## Machine learning design



## The scripts
The scripts shared in this repository are numbered, and are discussed below in the same order:

1. Data cleaning (1_data_cleaning.R)

2. Tables (2_tables.R): for table 1, and supplementary tables of subgroups

3. Classification models to predict sex from metabolite profiles: which metabolites are most predictive of sex?
- Create input data (x and y) for XGBoost classification models
- Run XGBoost classification model with bash script
- Process output of XGBoost model

4. Descriptive plots of BP and metabolites (including output of classification model above)

5. Associations between metabolite profiles and systolic and diastolic BP:
- Create input data (x and y) for XGBoost models
- Run XGBoost regression model with bash script
- Process output of XGBoost model
- Linear regression models of best predicting metabolites and BP

6. Associations between metabolite profiles and baroreceptor sensitivity (BRS) and heart rate variability (HRV) (also SDNN):
- Create input data (x and y) for XGBoost models
- Run XGBoost regression model with bash script
- Process output of XGBoost model
- Linear regression models of best predicting metabolites and SDNN / BRS
- Draw plot with explained variance of all models (SBP; DBP; HRV; BRS)

7. Age stratification (=< 50 years and > 50 years) for DBP and HRV models with low explained variance:
- Create input data (x and y) for systolic and diastolic BP for XGBoost models
- Create input data (x and y) for SDNN for XGBoost models
- Run XGBoost regression model with bash script
- Process output of XGBoost model
- Draw plot with explained variance of the models young vs old

8. Sensitivity analysis without subjects with diabetes
- Create input data (x and y) for systolic and diastolic BP for XGBoost models
- Run XGBoost regression model with bash script
- Process output of XGBoost model
- Draw plot with explained variance of all models (SBP; DBP; HRV; BRS)

9. Associations between microbiota composition (16S) and metabolite profiles
- Create input data (x and y) for systolic and diastolic BP for XGBoost models
- Run XGBoost regression model with bash script
- Process output of XGBoost model
- Draw plot with explained variance of all metabolite models with exp var > 2%
- Linear regression models of highest ranked ASVs and metabolite levels, including models stratified for sex (supplements)

