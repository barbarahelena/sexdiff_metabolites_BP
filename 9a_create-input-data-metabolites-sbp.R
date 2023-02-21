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
featimp_fem <- rio::import("SBP/Female/output_XGB_reg_Fem_SBP_2020_11_11__21-19-13/feature_importance.txt")
featimp_male <- rio::import("SBP/Male/output_XGB_reg_Male_SBP_2020_11_11__22-19-04/feature_importance.txt")
dd <- readRDS('../data/HELIUS_plasma_metabolites.RDS')
d <- readRDS('../data/HELIUS_ASV_table_all.RDS')

## Prep outcome data
rownames(dd) <- paste0('S', rownames(dd))
listmet <- c(featimp_fem[1:10, 1], featimp_male[1:10, 1])
dd2 <- dd[,listmet]
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
path <- 'Metabolites/Fem_1'
dir.create(path)
write_data(df, file.path(path, 'input_data'))
y <- as.data.frame(dd2[,1])
write_y(y, name_y = 'y_reg.txt', file.path(path, 'input_data'))

# make input data for metabolite 2 fem
path <- 'Metabolites/Fem_2'
dir.create(path)
write_data(df, file.path(path, 'input_data'))
y <- as.data.frame(dd2[,2])
write_y(y, name_y = 'y_reg.txt', file.path(path, 'input_data'))

# make input data for metabolite 3 fem
path <- 'Metabolites/Fem_3'
dir.create(path)
write_data(df, file.path(path, 'input_data'))
y <- as.data.frame(dd2[,3])
write_y(y, name_y = 'y_reg.txt', file.path(path, 'input_data'))

# make input data for metabolite 4 fem
path <- 'Metabolites/Fem_4'
dir.create(path)
write_data(df, file.path(path, 'input_data'))
y <- as.data.frame(dd2[,4])
write_y(y, name_y = 'y_reg.txt', file.path(path, 'input_data'))

# make input data for metabolite 5 fem
path <- 'Metabolites/Fem_5'
dir.create(path)
write_data(df, file.path(path, 'input_data'))
y <- as.data.frame(dd2[,5])
write_y(y, name_y = 'y_reg.txt', file.path(path, 'input_data'))


# make input data for metabolite 6 female
path <- 'Metabolites/Fem_6'
dir.create(path)
write_data(df, file.path(path, 'input_data'))
y <- as.data.frame(dd2[,6])
write_y(y, name_y = 'y_reg.txt', file.path(path, 'input_data'))

# make input data for metabolite 7 female
path <- 'Metabolites/Fem_7'
dir.create(path)
write_data(df, file.path(path, 'input_data'))
y <- as.data.frame(dd2[,7])
write_y(y, name_y = 'y_reg.txt', file.path(path, 'input_data'))

# make input data for metabolite 8 female
path <- 'Metabolites/Fem_8'
dir.create(path)
write_data(df, file.path(path, 'input_data'))
y <- as.data.frame(dd2[,8])
write_y(y, name_y = 'y_reg.txt', file.path(path, 'input_data'))

# make input data for metabolite 9 female
path <- 'Metabolites/Fem_9'
dir.create(path)
write_data(df, file.path(path, 'input_data'))
y <- as.data.frame(dd2[,9])
write_y(y, name_y = 'y_reg.txt', file.path(path, 'input_data'))

# make input data for metabolite 10 female
path <- 'Metabolites/Fem_10'
dir.create(path)
write_data(df, file.path(path, 'input_data'))
y <- as.data.frame(dd2[,10])
write_y(y, name_y = 'y_reg.txt', file.path(path, 'input_data'))




# make input data for metabolite 1 Male
path <- 'Metabolites'
dir.create(path)
path <- 'Metabolites/Male_1'
dir.create(path)
write_data(df, file.path(path, 'input_data'))
y <- dd2$`sphingomyelin (d18:1/20:2, d18:2/20:1, d16:1/22:2)*`
write_y(y, name_y = 'y_reg.txt', file.path(path, 'input_data'))

# make input data for metabolite 2 Male
path <- 'Metabolites/Male_2'
dir.create(path)
write_data(df, file.path(path, 'input_data'))
y <- dd2$`N-formylmethionine`
write_y(y, name_y = 'y_reg.txt', file.path(path, 'input_data'))

# make input data for metabolite 3 Male
path <- 'Metabolites/Male_3'
dir.create(path)
write_data(df, file.path(path, 'input_data'))
y <- dd2$`glycochenodeoxycholate 3-sulfate`
write_y(y, name_y = 'y_reg.txt', file.path(path, 'input_data'))

# make input data for metabolite 4 Male
path <- 'Metabolites/Male_4'
dir.create(path)
write_data(df, file.path(path, 'input_data'))
y <- dd2$`N-acetylglutamate`
write_y(y, name_y = 'y_reg.txt', file.path(path, 'input_data'))

# make input data for metabolite 5 Male
path <- 'Metabolites/Male_5'
dir.create(path)
write_data(df, file.path(path, 'input_data'))
y <- dd2$picolinoylglycine
write_y(y, name_y = 'y_reg.txt', file.path(path, 'input_data'))


# make input data for metabolite 6 Male
path <- 'Metabolites/Male_6'
dir.create(path)
write_data(df, file.path(path, 'input_data'))
y <- dd2$`hexadecadienoate (16:2n6)`
write_y(y, name_y = 'y_reg.txt', file.path(path, 'input_data'))

# make input data for metabolite 7 Male
path <- 'Metabolites/Male_7'
dir.create(path)
write_data(df, file.path(path, 'input_data'))
y <- dd2$`sphingomyelin (d18:2/24:2)*`
write_y(y, name_y = 'y_reg.txt', file.path(path, 'input_data'))

# make input data for metabolite 8 Male
path <- 'Metabolites/Male_8'
dir.create(path)
write_data(df, file.path(path, 'input_data'))
y <- dd2$`sphingomyelin (d18:1/22:2, d18:2/22:1, d16:1/24:2)*`
write_y(y, name_y = 'y_reg.txt', file.path(path, 'input_data'))

# make input data for metabolite 9 Male
path <- 'Metabolites/Male_9'
dir.create(path)
write_data(df, file.path(path, 'input_data'))
y <- dd2$`vanillylmandelate (VMA)`
write_y(y, name_y = 'y_reg.txt', file.path(path, 'input_data'))

# make input data for metabolite 10 Male
path <- 'Metabolites/Male_10'
dir.create(path)
write_data(df, file.path(path, 'input_data'))
y <- dd2$`3-hydroxy-3-methylglutarate`
write_y(y, name_y = 'y_reg.txt', file.path(path, 'input_data'))
