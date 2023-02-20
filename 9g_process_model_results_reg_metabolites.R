## Processing results subgroups

setwd('malefemale')
source("functions.R")
library(dplyr)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(ggsci)

path_true <- file.path('Metabolites/Male_1/output_XGB_reg_sphingomyelin_2022_03_30__16-58-21')
#path_permuted <- file.path(g, i, li2[which(a&!b)])
data_path <- file.path('Metabolites/Male_1/input_data')

plot_features_tests_asv_tot(data_path, path_true)
plot_feature_importance_asv(path_true, top_n = 20)

path_true <- file.path('Metabolites/Male_7/output_XGB_reg_sphingomyelin2_2022_06_09__10-30-52')
#path_permuted <- file.path(g, i, li2[which(a&!b)])
data_path <- file.path('Metabolites/Male_7/input_data')

plot_features_tests_asv_tot(data_path, path_true)
plot_feature_importance_asv(path_true, top_n = 20)

path_true <- file.path('Metabolites/6BT/output_XGB_reg_6BTmicrobiota_2022_04_26__22-06-22')
data_path <- file.path('Metabolites/6BT/input_data')

plot_features_tests_asv_tot(data_path, path_true)
plot_feature_importance_asv(path_true, top_n = 20)

path_true <- file.path('Metabolites/6BT_l/output_XGB_reg_6BTmicrobiota_2022_04_26__22-58-42')
data_path <- file.path('Metabolites/6BT_l/input_data')

plot_features_tests_asv_tot(data_path, path_true)
plot_feature_importance_asv(path_true, top_n = 20)

path_true <- file.path('Metabolites/Tryp/output_XGB_reg_Trypmicrobiota_2022_04_26__22-24-07')
data_path <- file.path('Metabolites/Tryp/input_data')

plot_features_tests_asv_tot(data_path, path_true)
plot_feature_importance_asv(path_true, top_n = 20)

path_true <- file.path('Metabolites/SDNN_1/')
data_path <- file.path('Metabolites/SDNN_1/input_data')

plot_features_tests_asv_tot(data_path, path_true)
plot_feature_importance_asv(path_true, top_n = 20)

path_true <- file.path('Metabolites/SDNN_2/')
data_path <- file.path('Metabolites/SDNN_2/input_data')

plot_features_tests_asv_tot(data_path, path_true)
plot_feature_importance_asv(path_true, top_n = 20)

path_true <- file.path('Metabolites/SDNN_3')
data_path <- file.path('Metabolites/SDNN_3/input_data')

plot_features_tests_asv_tot(data_path, path_true)
plot_feature_importance_asv(path_true, top_n = 20)

path_true <- file.path('Metabolites/SDNN_4')
data_path <- file.path('Metabolites/SDNN_4/input_data')

plot_features_tests_asv_tot(data_path, path_true)
plot_feature_importance_asv(path_true, top_n = 20)

path_true <- file.path('Metabolites/SDNN_5')
data_path <- file.path('Metabolites/SDNN_5/input_data')

plot_features_tests_asv_tot(data_path, path_true)
plot_feature_importance_asv(path_true, top_n = 20)

path_true <- file.path('Metabolites/SDNN_6/output_XGB_reg_hydroxylaurate_SDNN_6_2022_06_03__19-18-57')
data_path <- file.path('Metabolites/SDNN_6/input_data')

plot_features_tests_asv_tot(data_path, path_true)
plot_feature_importance_asv(path_true, top_n = 20)

path_true <- file.path('Metabolites/SDNN_7')
data_path <- file.path('Metabolites/SDNN_7/input_data')

plot_features_tests_asv_tot(data_path, path_true)
plot_feature_importance_asv(path_true, top_n = 20)

path_true <- file.path('Metabolites/SDNN_8')
data_path <- file.path('Metabolites/SDNN_8/input_data')

plot_features_tests_asv_tot(data_path, path_true)
plot_feature_importance_asv(path_true, top_n = 20)

path_true <- file.path('Metabolites/SDNN_9')
data_path <- file.path('Metabolites/SDNN_9/input_data')

plot_features_tests_asv_tot(data_path, path_true)
plot_feature_importance_asv(path_true, top_n = 20)

path_true <- file.path('Metabolites/SDNN_10')
data_path <- file.path('Metabolites/SDNN_10/input_data')

plot_features_tests_asv_tot(data_path, path_true)
plot_feature_importance_asv(path_true, top_n = 20)
