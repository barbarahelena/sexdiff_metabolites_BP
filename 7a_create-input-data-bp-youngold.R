# Create XGB input files for BP data - young and old subjects separately
# Barbara Verhaar, b.j.verhaar@amsterdamumc.nl
library(dplyr)
rm(list=ls())

#### Functions ####
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

#### Open metabolite data ####
df_met <- readRDS("../data/HELIUS_plasma_metabolites.RDS")
rownames(df_met) <- paste0('S', rownames(df_met))

#### Open clinical data ####
df <- readRDS('../data/helius_malefemale.RDS')
head(df)
df$ID<-paste0('S', df$ID)
d_young_fem <- df %>% filter(!is.na(SBP)) %>% filter(Age_cat == "Young" & Sex == "Female")
d_old_fem <- df %>% filter(!is.na(SBP)) %>% filter(Age_cat == "Old" & Sex == "Female")
d_young_male <- df %>% filter(!is.na(SBP)) %>% filter(Age_cat == "Young" & Sex == "Male")
d_old_male <- df %>% filter(!is.na(SBP)) %>% filter(Age_cat == "Old" & Sex == "Male")

#### Make input data for ML model - Young subjects ####
df_met_young_fem <- df_met %>% filter(rownames(.) %in% d_young_fem$ID) # filter metabolite df
d_young_fem <- d_young_fem[match(rownames(df_met_young_fem), d_young_fem$ID), ] # put in same sequence of IDs
all(d_young_fem$ID == rownames(df_met_young_fem)) # check if input and output IDs match => TRUE
d_young_fem$ID
rownames(df_met_young_fem)

path <- 'AgeStrata/FemY_SBP'
dir.create(path)
write_data(ddd, file.path(path, 'input_data'))
y <- as.data.frame(d_young_fem$SBP)
write_y(y, name_y = 'y_reg.txt', file.path(path, 'input_data'))

path <- 'AgeStrata/FemY_DBP'
dir.create(path)
write_data(ddd, file.path(path, 'input_data'))
y <- as.data.frame(d_young_fem$DBP)
write_y(y, name_y = 'y_reg.txt', file.path(path, 'input_data'))

df_met_young_male <- df_met %>% filter(rownames(.) %in% d_young_male$ID) # filter metabolite df for same subjects
d_young_male <- d_young_male[match(rownames(df_met_young_male), d_young_male$ID), ] # Put in same sequence of IDs
all(d_young_male$ID == rownames(df_met_young_male)) # check if input and output IDs match => TRUE
d_young_male$ID
rownames(df_met_young_male)

path <- 'AgeStrata/MaleY_SBP'
dir.create(path)
write_data(ddd, file.path(path, 'input_data'))
y <- as.data.frame(d_young_male$SBP)
write_y(y, name_y = 'y_reg.txt', file.path(path, 'input_data'))

path <- 'AgeStrata/MaleY_DBP'
dir.create(path)
write_data(ddd, file.path(path, 'input_data'))
y <- as.data.frame(d_young_male$DBP)
write_y(y, name_y = 'y_reg.txt', file.path(path, 'input_data'))

#### Make input data for ML model - Old subjects ####
df_met_old_fem <- df_met %>% filter(rownames(.) %in% d_old_fem$ID) # filter metabolite df
d_old_fem <- d_old_fem[match(rownames(df_met_old_fem), d_old_fem$ID), ] # put in same sequence of IDs
all(d_old_fem$ID == rownames(df_met_old_fem)) # check if input and output IDs match => TRUE
d_old_fem$ID
rownames(df_met_old_fem)

path <- 'AgeStrata/FemO_SBP'
dir.create(path)
write_data(ddd, file.path(path, 'input_data'))
y <- as.data.frame(d_old_fem$SBP)
write_y(y, name_y = 'y_reg.txt', file.path(path, 'input_data'))

path <- 'AgeStrata/FemO_DBP'
dir.create(path)
write_data(ddd, file.path(path, 'input_data'))
y <- as.data.frame(d_old_fem$DBP)
write_y(y, name_y = 'y_reg.txt', file.path(path, 'input_data'))

df_met_old_male <- df_met %>% filter(rownames(.) %in% d_old_male$ID) # filter metabolite df for same subjects
d_old_male <- d_old_male[match(rownames(df_met_old_male), d_old_male$ID), ] # Put in same sequence of IDs
all(d_old_male$ID == rownames(df_met_old_male)) # check if input and output IDs match => TRUE
d_old_male$ID
rownames(df_met_old_male)

path <- 'AgeStrata/MaleO_SBP'
dir.create(path)
write_data(ddd, file.path(path, 'input_data'))
y <- as.data.frame(d_old_male$SBP)
write_y(y, name_y = 'y_reg.txt', file.path(path, 'input_data'))

path <- 'AgeStrata/MaleO_DBP'
dir.create(path)
write_data(ddd, file.path(path, 'input_data'))
y <- as.data.frame(d_old_male$DBP)
write_y(y, name_y = 'y_reg.txt', file.path(path, 'input_data'))