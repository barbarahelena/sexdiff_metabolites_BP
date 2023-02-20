## Process results sensitivity analysis without DM subjects

source("functions_reg.R")

#### Male SBP ####
path_true <- 'NoDM/Male/SBP/output_XGB_reg_nodmmalesbp_2022_10_21__22-14-29'
data_path <- 'NoDM/Male/SBP/input_data'

plot_feature_importance_colors(path_true, top_n = 20)

#### Male DBP ####
path_true <- 'NoDM/Male/DBP/output_XGB_reg_nodmmaledbp_2022_10_22__01-24-36'
data_path <- 'NoDM/Male/DBP/input_data'

plot_feature_importance_colors(path_true, top_n = 20)

#### Female SBP ####
path_true <- 'NoDM/Female/SBP/output_XGB_reg_nodmfemalesbp_2022_10_21__13-50-46'
data_path <- 'NoDM/Female/SBP/input_data'

plot_feature_importance_colors(path_true, top_n = 20)

#### Female DBP ####
path_true <- 'NoDM/Female/DBP/output_XGB_reg_nodmfemaledbp_2022_10_21__18-13-08'
data_path <- 'NoDM/Female/DBP/input_data'

plot_feature_importance_colors(path_true, top_n = 20)
