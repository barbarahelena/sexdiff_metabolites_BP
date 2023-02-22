## Processing results subgroups
## Barbara Verhaar, b.j.verhaar@amsterdamumc.nl

source('functions_class.R')

#### Process results ####
path_true <- 'MaleFemale/output_XGB_class_MaleFemale_2020_11_18__14-57-28'
path_permuted <- 'MaleFemale/output_XGB_class_MaleFemale_2020_11_18__15-36-42_PERMUTED'
data_path <- 'MaleFemale/input_data'

compared_to_permuted_class(path_true, path_permuted)
plot_feature_importance_colors(path_true, top_n = 15)
plot_features_tests_top(data_path, path_true, top_n=15, nrow=4)
