## Processing results blood pressure XGBoost models

source("functions_reg.R")
library(dplyr)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(ggsci)

#### Results male subjects ####
path_true <- file.path('SBP/Male/output_XGB_reg_Male_SBP_2020_11_11__22-19-04')
path_permuted <- file.path('SBP/Male/output_XGB_reg_Male_SBP_2020_11_12__21-31-35_PERMUTED')
data_path <- file.path('SBP/Male/input_data')

compared_to_permuted_reg(path_true_outcome = path_true, path_permuted_outcome = path_permuted)
plot_features_tests_reg(data_path, path_true, top_n = 10, outcome_name = "SBP", x_lab = 'Concentration')
plot_features_top_n_reg(data_path, path_true, top_n = 10, outcome_name='SBP', x_lab='Concentration')
plot_feature_importance(path_true, top_n = 20)
plot_feature_importance_colors(path_true, top_n = 20)

path_true <- file.path('DBP/Male/output_XGB_reg_Male_DBP_2020_11_12__07-03-15')
path_permuted <- file.path('DBP/Male/output_XGB_reg_Male_DBP_2020_11_13__05-14-03_PERMUTED')
data_path <- file.path('DBP/Male/input_data')

compared_to_permuted_reg(path_true_outcome = path_true, path_permuted_outcome = path_permuted)
plot_features_tests_reg(data_path, path_true, top_n = 10, outcome_name = "DBP", x_lab = 'Concentration')
plot_features_top_n_reg(data_path, path_true, top_n = 10, outcome_name='DBP', x_lab='Concentration')
plot_feature_importance(path_true, top_n = 20)
plot_feature_importance_colors(path_true, top_n = 20)


#### Results female subjects ####
path_true <- file.path('SBP/Female/output_XGB_reg_Fem_SBP_2020_11_11__21-19-13')
path_permuted <- file.path('SBP/Female/output_XGB_reg_Fem_SBP_2020_11_12__20-39-22_PERMUTED')
data_path <- file.path('SBP/Female/input_data')

compared_to_permuted_reg(path_true_outcome = path_true, path_permuted_outcome = path_permuted)
plot_features_tests_reg(data_path, path_true, top_n = 10, outcome_name = "SBP", x_lab = 'Concentration')
plot_features_top_n_reg(data_path, path_true, top_n = 10, outcome_name='SBP', x_lab='Concentration')
plot_feature_importance(path_true, top_n = 20)
plot_feature_importance_colors(path_true, top_n = 20)

path_true <- file.path('DBP/Female/output_XGB_reg_Fem_DBP_2020_11_12__06-04-17')
path_permuted <- file.path('DBP/Female/output_XGB_reg_Fem_DBP_2020_11_13__04-21-44_PERMUTED')
data_path <- file.path('DBP/Female/input_data')

compared_to_permuted_reg(path_true_outcome = path_true, path_permuted_outcome = path_permuted)
plot_features_tests_reg(data_path, path_true, top_n = 10, outcome_name = "DBP", x_lab = 'Concentration')
plot_features_top_n_reg(data_path, path_true, top_n = 10, outcome_name='DBP', x_lab='Concentration')
plot_feature_importance(path_true, top_n = 20)
plot_feature_importance_colors(path_true, top_n = 20)
