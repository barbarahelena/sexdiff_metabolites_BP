## Processing results blood pressure XGBoost models

source("functions.R")
library(dplyr)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(ggsci)

#### Results male subjects ####
path_true <- file.path('Male/SBP/output_XGB_reg_Male_SBP_2020_11_11__22-19-04')
path_permuted <- file.path('Male/SBP/output_XGB_reg_Male_SBP_2020_11_12__21-31-35_PERMUTED')
data_path <- file.path('Male/SBP/input_data')

#compared_to_permuted_reg(path_true_outcome = path_true, path_permuted_outcome = path_permuted)
plot_features_tests_reg(data_path, path_true, top_n = 20, outcome_name = "SBP", 
                        x_lab = 'Concentration', residuals = FALSE)
plot_features_top_n_reg(data_path, path_true, outcome_name='SBP', 
                        x_lab='Concentration', residuals = FALSE)
plot_feature_importance(path_true, top_n = 20)
plot_feature_importance_colors(path_true, top_n = 20)

path_true <- file.path('Male/DBP/output_XGB_reg_Male_DBP_2020_11_12__07-03-15')
path_permuted <- file.path('Male/DBP/output_XGB_reg_Male_DBP_2020_11_13__05-14-03_PERMUTED')
data_path <- file.path('Male/DBP/input_data')

compared_to_permuted_reg(path_true_outcome = path_true, path_permuted_outcome = path_permuted)
plot_features_tests_reg(data_path, path_true, top_n = 20, outcome_name = "DBP", x_lab = 'Concentration', residuals = FALSE)
plot_features_top_n_reg(data_path, path_true, outcome_name='DBP', x_lab='Concentration', residuals = FALSE)
plot_feature_importance(path_true, top_n = 20)
plot_feature_importance_colors(path_true, top_n = 20)


#### Results female subjects ####
path_true <- file.path('Female/SBP/output_XGB_reg_Fem_SBP_2020_11_11__21-19-13')
path_permuted <- file.path('Female/SBP/output_XGB_reg_Fem_SBP_2020_11_12__20-39-22_PERMUTED')
data_path <- file.path('Female/SBP/input_data')

compared_to_permuted_reg(path_true_outcome = path_true, path_permuted_outcome = path_permuted)
plot_features_tests_reg(data_path, path_true, top_n = 20, 
                        outcome_name = "SBP", x_lab = 'Concentration', residuals = FALSE)
plot_features_top_n_reg(data_path, path_true, outcome_name='SBP', 
                        x_lab='Concentration', residuals = FALSE)
plot_feature_importance(path_true, top_n = 20)
plot_feature_importance_colors(path_true, top_n = 20)

path_true <- file.path('Female/DBP/output_XGB_reg_Fem_DBP_2020_11_12__06-04-17')
path_permuted <- file.path('Female/DBP/output_XGB_reg_Fem_DBP_2020_11_13__04-21-44_PERMUTED')
data_path <- file.path('Female/DBP/input_data')

compared_to_permuted_reg(path_true_outcome = path_true, path_permuted_outcome = path_permuted)
plot_features_tests_reg(data_path, path_true, top_n = 20, 
                        outcome_name = "DBP", x_lab = 'Concentration', residuals = FALSE)
plot_features_top_n_reg(data_path, path_true, outcome_name='DBP', 
                        x_lab='Concentration', residuals = FALSE)
plot_feature_importance(path_true, top_n = 20)
plot_feature_importance_colors(path_true, top_n = 20)
