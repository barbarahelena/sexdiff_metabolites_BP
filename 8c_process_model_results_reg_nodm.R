## Process results sensitivity analysis without DM subjects

source("functions.R")

#### Male SBP ####
path_true <- 'NoDM/Male/SBP/output_XGB_reg_nodmmalesbp_2022_10_21__22-14-29'
# path_permuted <- 'NoDM/Male/SBP/output_XGB_class_MaleFemale_2020_11_18__15-36-42_PERMUTED'
data_path <- 'NoDM/Male/SBP/input_data'

# compared_to_permuted_class(path_true, path_permuted)
plot_feature_importance_class(path_true, 20)
(pl_featimp <- plot_feature_importance_class_microbiome(path_true, top_n = 15))
# plot_features_tests_class(data_path, path_true, top_n=30)
# plot_features_tests_top(data_path, path_true, top_n=20, nrow=4)

#### Male DBP ####
path_true <- 'NoDM/Male/DBP/output_XGB_reg_nodmmaledbp_2022_10_22__01-24-36'
# path_permuted <- 'NoDM/Male/SBP/output_XGB_class_MaleFemale_2020_11_18__15-36-42_PERMUTED'
data_path <- 'NoDM/Male/DBP/input_data'

# compared_to_permuted_class(path_true, path_permuted)
plot_feature_importance_class(path_true, 20)
plot_feature_importance_class_microbiome(path_true, top_n = 15)
# plot_features_tests_class(data_path, path_true, top_n=30)
# plot_features_tests_top(data_path, path_true, top_n=20, nrow=4)


#### Female SBP ####
path_true <- 'NoDM/Female/SBP/output_XGB_reg_nodmfemalesbp_2022_10_21__13-50-46'
# path_permuted <- 'NoDM/Male/SBP/output_XGB_class_MaleFemale_2020_11_18__15-36-42_PERMUTED'
data_path <- 'NoDM/Female/SBP/input_data'

# compared_to_permuted_class(path_true, path_permuted)
plot_feature_importance_class(path_true, 20)
(pl_featimp <- plot_feature_importance_class_microbiome(path_true, top_n = 15))
# plot_features_tests_class(data_path, path_true, top_n=30)
# plot_features_tests_top(data_path, path_true, top_n=20, nrow=4)

#### Female DBP ####
path_true <- 'NoDM/Female/DBP/output_XGB_reg_nodmfemaledbp_2022_10_21__18-13-08'
# path_permuted <- 'NoDM/Male/SBP/output_XGB_class_MaleFemale_2020_11_18__15-36-42_PERMUTED'
data_path <- 'NoDM/Female/DBP/input_data'

# compared_to_permuted_class(path_true, path_permuted)
plot_feature_importance_class(path_true, 20)
plot_feature_importance_class_microbiome(path_true, top_n = 15)
# plot_features_tests_class(data_path, path_true, top_n=30)
# plot_features_tests_top(data_path, path_true, top_n=20, nrow=4)