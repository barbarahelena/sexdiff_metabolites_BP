## Processing results xBRS / SDNN (HRV)

source("functions.R")
library(dplyr)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(ggsci)

path_true <- file.path('SDNN/Female/output_XGB_reg_SDNN_female_2022_06_03__10-09-11')
data_path <- file.path('SDNN/Female/input_data')

plot_features_tests_reg(data_path, path_true)
plot_feature_importance_color(path_true, top_n = 20)

path_true <- file.path('SDNN/Male/output_XGB_reg_SDNN_male_2022_06_02__18-27-11')
data_path <- file.path('SDNN/Male/input_data')

plot_features_tests_reg(data_path, path_true)
plot_feature_importance_color(path_true, top_n = 20)


path_true <- file.path('BRS/Female/output_XGB_reg_BRSFem_2022_03_17__19-30-01')
data_path <- file.path('BRS/Female/input_data')

plot_features_tests_reg(data_path, path_true)
plot_feature_importance_color(path_true, top_n = 20)

path_true <- file.path('BRS/Male/output_XGB_reg_BRSMale_2022_03_18__11-14-28')
data_path <- file.path('BRS/Male/input_data')

plot_features_tests_reg(data_path, path_true)
plot_feature_importance_color(path_true, top_n = 20)

