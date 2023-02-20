## Processing results age stratification models

source("functions.R")
library(dplyr)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(ggsci)

#### SBP and DBP ####
path_true <- file.path('AgeStrata/FemO_DBP/output_XGB_reg_femo_dbp_2022_04_26__23-28-37')
data_path <- file.path('AgeStrata/FemO_DBP/input_data')

plot_features_tests_reg(data_path, path_true)
plot_feature_importance_class_microbiome(path_true, top_n = 20)


path_true <- file.path('AgeStrata/FemO_SBP/output_XGB_reg_femo_sbp_2022_04_27__01-43-33')
data_path <- file.path('AgeStrata/FemO_SBP/input_data')

plot_features_tests_reg(data_path, path_true)
plot_feature_importance_class_microbiome(path_true, top_n = 20)

path_true <- file.path('AgeStrata/FemY_DBP/output_XGB_reg_femy_dbp_2022_04_25__10-22-04')
data_path <- file.path('AgeStrata/FemY_DBP/input_data')

plot_features_tests_reg(data_path, path_true)
plot_feature_importance_class_microbiome(path_true, top_n = 20)

path_true <- file.path('AgeStrata/FemY_SBP/output_XGB_reg_femy_sbp_2022_04_25__12-25-19')
data_path <- file.path('AgeStrata/FemY_SBP/input_data')

plot_features_tests_reg(data_path, path_true)
plot_feature_importance_class_microbiome(path_true, top_n = 20)


path_true <- file.path('AgeStrata/MaleO_DBP/output_XGB_reg_maleo_dbp_2022_04_27__03-48-56')
data_path <- file.path('AgeStrata/MaleO_DBP/input_data')

plot_features_tests_reg(data_path, path_true)
plot_feature_importance_class_microbiome(path_true, top_n = 20)


path_true <- file.path('AgeStrata/MaleO_SBP/output_XGB_reg_maleo_sbp_2022_04_27__06-10-43')
data_path <- file.path('AgeStrata/MaleO_SBP/input_data')

plot_features_tests_reg(data_path, path_true)
plot_feature_importance_class_microbiome(path_true, top_n = 20)

path_true <- file.path('AgeStrata/MaleY_DBP/output_XGB_reg_maley_dbp_2022_04_25__14-24-38')
data_path <- file.path('AgeStrata/MaleY_DBP/input_data')

plot_features_tests_reg(data_path, path_true)
plot_feature_importance_class_microbiome(path_true, top_n = 20)

path_true <- file.path('AgeStrata/MaleY_SBP/output_XGB_reg_maley_sbp_2022_04_25__15-56-31')
data_path <- file.path('AgeStrata/MaleY_SBP/input_data')

plot_features_tests_reg(data_path, path_true)
plot_feature_importance_class_microbiome(path_true, top_n = 20)


#### SDNN ####
path_true <- file.path('AgeStrata/FemaleY_SDNN/output_XGB_reg_SDNN_femY_2022_06_03__12-13-23')
data_path <- file.path('AgeStrata/FemaleY_SDNN/input_data')

plot_features_tests_reg(data_path, path_true)
plot_feature_importance_class_microbiome(path_true, top_n = 20)

path_true <- file.path('AgeStrata/FemaleO_SDNN/output_XGB_reg_SDNN_femO_2022_06_03__13-20-09')
data_path <- file.path('AgeStrata/FemaleO_SDNN/input_data')

plot_features_tests_reg(data_path, path_true)
plot_feature_importance_class_microbiome(path_true, top_n = 20)
