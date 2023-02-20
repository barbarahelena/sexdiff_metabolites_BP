# create XGB input files for all datasets
library(dplyr)
rm(list=ls())

# make data for machine learning XGB classification models

# writes input data files for XGB models as tab-delimited 
# subject ids and feature ids are written as separate tab-delimited files
# write X data / predictors
write_data <- function(x, data_path){
  dir.create(data_path)
  x <- as.matrix(x)
  if(any(is.na(x))){
    cat('There are missing values in the input data!\n')
  }
  write.table(x, file.path(data_path, 'X_data.txt'), row.names = F, col.names = F, sep = '\t', quote = F)
  write.table(colnames(x), file.path(data_path,'feat_ids.txt'), row.names = F, col.names = F, sep = '\t', quote = F)
  write.table(rownames(x), file.path(data_path,'subject_ids.txt'), row.names = F, col.names = F, sep = '\t', quote = F)
}

# write y / predicted outcome
write_y <- function(x, name_y, data_path){
  dir.create(data_path)
  if(missing(name_y)){
    cat('\n\nYou need to provide a name for the y data file!\n')
  }
  if(!name_y %in% c('y_binary.txt', 'y_reg.txt')){
    cat('\nThe file name is not compatible with XGBeast!\n' )
  }
  if(any(is.na(x))){
    cat('\nThere are missing values in the outcome data!\n')
  }
  write.table(x, file = file.path(data_path, name_y), row.names = F, col.names = F, sep = '\t', quote = F)
}

# open data
featimp_fem <- rio::import("Fem/DBP/output_XGB_reg_Fem_DBP_2020_11_12__06-04-17/feature_importance.txt")
featimp_male <- rio::import("Male/DBP/output_XGB_reg_Male_DBP_2020_11_12__07-03-15/feature_importance.txt")
dd <- readxl::read_xlsx('../data/HELIUS_EDTA_plasma_metabolites.xlsx')
d <- readRDS('../data/HELIUS_ASV_table_all.RDS')

## Prep outcome data
met <- dd$Metabolite
dd$Metabolite <- NULL
head(dd)
dd2 <- as.data.frame(t(as.matrix(dd)))
dim(dd)
colnames(dd2)
colnames(dd2) <- met
rownames(dd2) <- paste0('S', rownames(dd2))
listmet <- c(featimp_fem[1:10, 1], featimp_male[1:10, 1])
dd2 <- dd2 %>% select(all_of(listmet))
dd2$ID <- rownames(dd2)

## Prep input data
df <- as.data.frame(d)
rownames(df) <- paste0('S', rownames(df))
df <- df %>% filter(rownames(.) %in% dd2$ID)
tkb <- apply(df, 2, function(x) sum(x > 5) > (0.3*length(x)))
df <- df[,tkb]

# Put d and dd in same sequence of IDs
dd2 <- dd2[match(rownames(df), dd2$ID), ]

# check that outcome subject ids match metabolite subjects ids
all(dd2$ID == rownames(df)) # TRUE
dd2$ID
rownames(df)

# make input data for metabolite 1 fem
path <- 'Metabolites'
dir.create(path)
path <- 'Metabolites/DBP'
dir.create(path)
path <- 'Metabolites/DBP/Fem_1'
dir.create(path)
write_data(df, file.path(path, 'input_data'))
y <- as.data.frame(dd2[,1])
write_y(y, name_y = 'y_reg.txt', file.path(path, 'input_data'))

# make input data for metabolite 2 fem
path <- 'Metabolites/DBP/Fem_2'
dir.create(path)
write_data(df, file.path(path, 'input_data'))
y <- as.data.frame(dd2[,2])
write_y(y, name_y = 'y_reg.txt', file.path(path, 'input_data'))

# make input data for metabolite 3 fem
path <- 'Metabolites/DBP/Fem_3'
dir.create(path)
write_data(df, file.path(path, 'input_data'))
y <- as.data.frame(dd2[,3])
write_y(y, name_y = 'y_reg.txt', file.path(path, 'input_data'))

# make input data for metabolite 4 fem
path <- 'Metabolites/DBP/Fem_4'
dir.create(path)
write_data(df, file.path(path, 'input_data'))
y <- as.data.frame(dd2[,4])
write_y(y, name_y = 'y_reg.txt', file.path(path, 'input_data'))

# make input data for metabolite 5 fem
path <- 'Metabolites/DBP/Fem_5'
dir.create(path)
write_data(df, file.path(path, 'input_data'))
y <- as.data.frame(dd2[,5])
write_y(y, name_y = 'y_reg.txt', file.path(path, 'input_data'))


# make input data for metabolite 6 female
path <- 'Metabolites/DBP/Fem_6'
dir.create(path)
write_data(df, file.path(path, 'input_data'))
y <- as.data.frame(dd2[,6])
write_y(y, name_y = 'y_reg.txt', file.path(path, 'input_data'))

# make input data for metabolite 7 female
path <- 'Metabolites/DBP/Fem_7'
dir.create(path)
write_data(df, file.path(path, 'input_data'))
y <- as.data.frame(dd2[,7])
write_y(y, name_y = 'y_reg.txt', file.path(path, 'input_data'))

# make input data for metabolite 8 female
path <- 'Metabolites/DBP/Fem_8'
dir.create(path)
write_data(df, file.path(path, 'input_data'))
y <- as.data.frame(dd2[,8])
write_y(y, name_y = 'y_reg.txt', file.path(path, 'input_data'))

# make input data for metabolite 9 female
path <- 'Metabolites/DBP/Fem_9'
dir.create(path)
write_data(df, file.path(path, 'input_data'))
y <- as.data.frame(dd2[,9])
write_y(y, name_y = 'y_reg.txt', file.path(path, 'input_data'))

# make input data for metabolite 10 female
path <- 'Metabolites/DBP/Fem_10'
dir.create(path)
write_data(df, file.path(path, 'input_data'))
y <- as.data.frame(dd2[,10])
write_y(y, name_y = 'y_reg.txt', file.path(path, 'input_data'))




# make input data for metabolite 1 Male
path <- 'Metabolites'
dir.create(path)
path <- 'Metabolites/DBP/Male_1'
dir.create(path)
write_data(df, file.path(path, 'input_data'))
y <- dd2[,11]
write_y(y, name_y = 'y_reg.txt', file.path(path, 'input_data'))

# make input data for metabolite 2 Male
path <- 'Metabolites/DBP/Male_2'
dir.create(path)
write_data(df, file.path(path, 'input_data'))
y <- dd2[,12]
write_y(y, name_y = 'y_reg.txt', file.path(path, 'input_data'))

# make input data for metabolite 3 Male
path <- 'Metabolites/DBP/Male_3'
dir.create(path)
write_data(df, file.path(path, 'input_data'))
y <- dd2[,13]
write_y(y, name_y = 'y_reg.txt', file.path(path, 'input_data'))

# make input data for metabolite 4 Male
path <- 'Metabolites/DBP/Male_4'
dir.create(path)
write_data(df, file.path(path, 'input_data'))
y <- dd2[,14]
write_y(y, name_y = 'y_reg.txt', file.path(path, 'input_data'))

# make input data for metabolite 5 Male
path <- 'Metabolites/DBP/Male_5'
dir.create(path)
write_data(df, file.path(path, 'input_data'))
y <- dd2[,15]
write_y(y, name_y = 'y_reg.txt', file.path(path, 'input_data'))


# make input data for metabolite 6 Male
path <- 'Metabolites/DBP/Male_6'
dir.create(path)
write_data(df, file.path(path, 'input_data'))
y <- dd2[,16]
write_y(y, name_y = 'y_reg.txt', file.path(path, 'input_data'))

# make input data for metabolite 7 Male
path <- 'Metabolites/DBP/Male_7'
dir.create(path)
write_data(df, file.path(path, 'input_data'))
y <- dd2[,17]
write_y(y, name_y = 'y_reg.txt', file.path(path, 'input_data'))

# make input data for metabolite 8 Male
path <- 'Metabolites/DBP/Male_8'
dir.create(path)
write_data(df, file.path(path, 'input_data'))
y <- dd2[,18]
write_y(y, name_y = 'y_reg.txt', file.path(path, 'input_data'))

# make input data for metabolite 9 Male
path <- 'Metabolites/DBP/Male_9'
dir.create(path)
write_data(df, file.path(path, 'input_data'))
y <- dd2[,19]
write_y(y, name_y = 'y_reg.txt', file.path(path, 'input_data'))

# make input data for metabolite 10 Male
path <- 'Metabolites/DBP/Male_10'
dir.create(path)
write_data(df, file.path(path, 'input_data'))
y <- dd2[,20]
write_y(y, name_y = 'y_reg.txt', file.path(path, 'input_data'))
