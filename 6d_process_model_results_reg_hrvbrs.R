## Processing results xBRS / SDNN (HRV)

source("functions_reg.R")
library(dplyr)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(ggsci)

path_true <- file.path('SDNN/Female/output_XGB_reg_SDNN_female_2022_06_03__10-09-11')
path_permuted <- file.path('SDNN/Female/output_XGB_reg_SDNN_female_2023_02_20__13-35-39_PERMUTED')
data_path <- file.path('SDNN/Female/input_data')

compared_to_permuted_reg(path_true_outcome = path_true, path_permuted_outcome = path_permuted)
plot_feature_importance_colors(path_true, top_n = 20)

path_true <- file.path('SDNN/Male/output_XGB_reg_SDNN_male_2022_06_02__18-27-11')
path_permuted <- file.path('SDNN/Male/output_XGB_reg_SDNN_male_2023_02_20__11-44-58_PERMUTED')
data_path <- file.path('SDNN/Male/input_data')

compared_to_permuted_reg(path_true_outcome = path_true, path_permuted_outcome = path_permuted)
plot_feature_importance_colors(path_true, top_n = 20)

path_true <- file.path('BRS/Female/output_XGB_reg_BRSFem_2022_03_17__19-30-01')
path_permuted <- file.path('BRS/Female/output_XGB_reg_BRS_female_2023_02_20__17-04-17_PERMUTED')
data_path <- file.path('BRS/Female/input_data')

compared_to_permuted_reg(path_true_outcome = path_true, path_permuted_outcome = path_permuted)
plot_feature_importance_colors(path_true, top_n = 20)

path_true <- file.path('BRS/Male/output_XGB_reg_BRSMale_2022_03_18__11-14-28')
path_permuted <- file.path('BRS/Male/output_XGB_reg_BRS_male_2023_02_20__15-09-56_PERMUTED')
data_path <- file.path('BRS/Male/input_data')

compared_to_permuted_reg(path_true_outcome = path_true, path_permuted_outcome = path_permuted)
plot_feature_importance_colors(path_true, top_n = 20)
