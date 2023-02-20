## Processing results metabolite folders

source("functions_mb.R")
library(dplyr)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(ggsci)

path_true <- file.path('Metabolites/SBP_metabolites/Male_1/output_XGB_reg_sphingomyelin_2022_03_30__16-58-21')
data_path <- file.path('Metabolites/SBP_metabolites/Male_1/input_data')

plot_features_tests_asv_tot(data_path, path_true)
plot_feature_importance_asv(path_true, top_n = 20)

path_true <- file.path('Metabolites/SBP_metabolites/Male_7/output_XGB_reg_sphingomyelin2_2022_06_09__10-30-52')
data_path <- file.path('Metabolites/SBP_metabolites/Male_7/input_data')

plot_features_tests_asv_tot(data_path, path_true)
plot_feature_importance_asv(path_true, top_n = 20)
