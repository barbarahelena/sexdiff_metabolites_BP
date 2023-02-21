# Sex differences in associations between metabolite profiles, blood pressure and heart rate variability: the HELIUS study

# Project aims
Primary aim: to investigate sex-specific plasma metabolite profiles that are associated with blood pressure and autonomic cardiovascular control, in order to better understand sex differences in hypertension. We performed machine learning analyses to predict BP, baroreceptor sensitivity (BRS) and heart rate variability (HRV) from plasma metabolite profiles for men and women separately.

Secondary aim: to investigate the associations between gut microbiota composition and the plasma levels of the metabolites predicting BP and HRV, since this could help target future interventions.

# Paper
A paper with more details on the methodology can be found here: [insert link]

# Data
For these analyses, we use data of the HELIUS cohort. For more information about this cohort, we refer to the HELIUS website (www.heliusstudy.nl) or the cohort profile paper (Snijder et al. BMJ Open 2017, http://dx.doi.org/10.1136/bmjopen-2017-017873). 
- Clinical data including blood pressure and heart rate variability is unfortunately not publicly available because the informed consent does not allow us to make these data public. Any researcher can request the data by submitting a proposal to the HELIUS Executive Board (heliuscoordinator@amsterdamumc.nl) as outlined at http://www.heliusstudy.nl/en/researchers/collaboration. The HELIUS Executive Board will check proposals for compatibility with the general objectives, ethical approvals and informed consent forms of the HELIUS study. There are no other restrictions to obtaining the data and all data requests will be processed in the same manner. 
- Metabolomics data: see paper for metabolomics methods.
- Infofile metabolomics: list with identified metabolites can be found in the data folder.
- 16S rRNA sequencing data (gut microbiota composition): The 16S sequencing data are available in the European Nucleotide Archive (ENA), accession number [PRJEB25863](https://www.ebi.ac.uk/ena/browser/view/PRJEB25863).

# RStudio and renv
Most of the analyses were performed in RStudio (v.2022.7.2.576) using R (v.4.2.1). We used renv and uploaded a lockfile in this repository to reconstruct the renv.

# Machine learning design
### Introduction
We used a machine learning algorithm to assess which plasma metabolites were most predictive for BP, HRV and BRS in men and women. All machine learning models used the XGBoost algorithm in a nested cross-validation design. In each iteration, the dataset was randomly split into a test set containing 20% of the subjects and a training set with the remaining 80%. Within the train set, 5-fold cross-validation was performed in order to optimize the model hyperparameters. Two random variables were added to the determinants in each iteration to serve as a benchmark. The resulting model was evaluated on the test set which yielded an area under the receiver-operator curve (AUC) for classification models, and explained variance (%) for continuous outcomes as main model quality metrics. In addition, each iteration resulted in a ranked list of metabolites with their relative importance to the prediction, with the first ranked metabolite set at 100% and the other metabolites’ importance calculated relative to the first. These were recorded for each iteration and were averaged across 200 iterations. Because of the definition of the explained variance score formula, the explained variance score could also be a negative value, meaning that the prediction is worse than an intercept. To ensure that these models were not overfitted, we ran identical models in which the data was permuted before every iteration.

<img src="https://user-images.githubusercontent.com/34349946/220138280-b58d5408-0fcc-4fbd-812c-df4e402925d3.png" width="500" alt = "flowchart machine learning model">

### Installing conda env
For installation of the conda environment, you will need the yaml file in this folder (xgb_mac_env.yaml). First change the name / path of the environment from the yaml file, if needed. You can install the conda environment using:
`conda env create --file xgb_mac_env.yaml`

Then activate the environment before running a model with:
`conda activate xgb`

### Running the models
As soon as the conda environment is activate, you'll need the the following items for running the model:
- Input data (x and y) in the right formats as created by the create_input_data scripts (see below ToC: scripts)
- XGBoost model python script (XGBoost.py)
- Parameter grid (metabolite_grid.json or microbiota_grid.json for this project)
- Bash script with the commands to run the model

The XGBoost.py has a --help section with the possible arguments for the function.

### Output of the models
An output folder is created in the same folder as the input folder when running the model.The XGBoost python script outputs the following the results in the output folder:
- aggregated_metrics_regression.txt: aggregated main model metrics (AUC, explained variance)
- all_model_parameters.txt: all parameters used, partly defined by the parameter grid
- conda_environment.txt: file that can be used to create conda environment
- feature_importance.txt: file with the ranked metabolites and their relative importance for the model
- model_results_per_iteration.txt: main model metrics per iteration
- system_info.txt

# ToC: scripts
The scripts shared in this repository are numbered, and are discussed below in the same order:

1. Data cleaning (1_data_cleaning.R). The resulting dataframes were saved as RDS in the data folder (clinical data and metabolomics data separately). From the metabolomics data, all metabolites that were labeled as xenobiotic in the infofile were excluded.

2. Tables (2_tables.R): for table 1, and supplementary tables of subgroups. Results (in csv) can be found in results folder. 

3. Classification models to predict sex from metabolite profiles: which metabolites are most predictive of sex?
- Create input data (x and y) for XGBoost classification models (3a_create-input-data-classification-malefemale.R). Results can be found in MaleFemale folder > input_data.
- Run XGBoost classification model with bash script (3b_malefemale_classification.sh).
- Process output of XGBoost model (3c_process_model_results_class_MF.R). Results can be found in MaleFemale folder > output data.

4. Descriptive plots of BP and metabolites (4_descriptiveplots.R). Sex differences in systolic and diastolic BP, HRV and BRS are plotted. A PCA is drawn to assess differences and overlap between men and women. The feature importance plot from 3) is added to the figure. This figure is then saved to the results folder.

5. Associations between metabolite profiles and systolic and diastolic BP:
- Create input data (x and y) for XGBoost regression models (5a_create-input-data-bp.R)
- Run XGBoost regression model with bash script (5b_BP_metabolites.sh)
- Process output of XGBoost model (5c_process_model_results_reg_bp.R)
- Linear regression models of best predicting metabolites and BP (5d_lm_bp_mf.R)

6. Associations between metabolite profiles and baroreceptor sensitivity (BRS) and heart rate variability (HRV) (also SDNN):
- Create input data (x and y) for XGBoost regression models for HRV and BRS (6a_create-input-data-sdnn.R, 6b_create-input-data-brs.R)
- Run XGBoost regression model with bash script (6c_sdnn_xbrs.sh)
- Process output of XGBoost model (6d_process_model_results_reg_hrvbrs.R)
- Linear regression models of best predicting metabolites and SDNN / BRS (6e_lm_bp_hrv.R)
- Draw plot with explained variance of all models (blood pressure, HRV and BRS) (6f_explainedvariance_bp_hrv_brs.R)

7. Age stratification (=< 50 years and > 50 years) for DBP and HRV models with low explained variance:
- Create input data (x and y) for systolic and diastolic BP for XGBoost regression models (7a_create-input-data-bp-youngold.R)
- Create input data (x and y) for SDNN for XGBoost regression models (7b_create-input-data-sdnn-youngold.R)
- Run XGBoost regression model with bash script (7c_bp_sdnn_youngold.sh)
- Process output of XGBoost model (7d_process_model_results_reg_youngold.R)
- Draw plot with explained variance of the models young vs old (7e_explainedvariance_youngold.R)

8. Sensitivity analysis without subjects with diabetes
- Create input data (x and y) for systolic and diastolic BP for XGBoost models (8a_create-input-data-nodm)
- Run XGBoost regression model with bash script (8b_BP_nodiabetes.sh)
- Process output of XGBoost model (8c_process_model_results_reg_nodm.R)
- Draw plot with explained variance of all models (SBP; DBP; HRV; BRS) (8d_explainedvariance_nodm.R)

9. Associations between microbiota composition (16S) and metabolite profiles
- Create input data (x and y) for systolic and diastolic BP and SDNN for XGBoost regression models (9a_create-input-data-metabolites-sbp.R, 9b_create-input-data-metabolites-dbp.R and 9c_create-input-data-metabolites-sdnn.R)
- Run XGBoost regression models with bash scripts (9d_metabolites_SBP.sh, 9e_metabolites_DBP.sh, 9f_metabolites_HRVMale.sh)
- Process output of XGBoost model (9g_process_model_results_reg_metabolites.R)
- Draw plot with explained variance of all metabolite models with exp var > 2% (9h_explainedvariance_met.R)
- Linear regression models of highest ranked ASVs and metabolite levels, including models stratified for sex (supplements) (9i_lm_met_asvs.R)

# ToC: input and output folders XGBoost models
In this repository, the input and output data of the XGBoost models are also shared. Each folder contains an input and output folder. In alphabetical order, this repository includes the following folders:
- AgeStrata: all age-stratified models (including Male/Female systolic and diastolic BP, and SDNN)
- BRS: models (Male / Female) to predict baroreceptor sensitivity from metabolomics.
- DBP: models (Male / Female) to predict diastolic BP from metabolomics.
- MaleFemale: classfication models to predict sex from metabolomics.
- Metabolites: all models to predict metabolite levels from 16S (gut microbiota composition) data.
- NoDM: models for SBP and DBP in subgroup without diabetes.
- SBP: models (Male / Female) to predict systolic BP from metabolomics.
- SDNN: models (Male / Female) to predict SDNN (= heart rate variability) from metabolomics.

# ToC: results
Tables and figures resulting from these scripts that can be found in the results folder include:
- Tables: table 1 (study population), table 2 (subgroup with Nexfin data), table 3 (subgroup without diabetes)
- Figure 1: descriptive characteristics
- Explained variance: 
  - tables with explained variance of all models; 
  - tables with explained variance of permuted models; 
  - plots with explained variances of BP, HRV, BRS and metabolite models.
- Linear regressions models:
  - tables with linear regression results
  - forest plots with estimates of linear regressions

In addition, in each folder with the output of the XGBoost models, the results from the "process_output" scripts can be found. These include the feature importance plots and the scatter plots (regression) / violin plots (classification) for the highest ranked predictors resulting from the machine learning models. 
