# Create XGB input files for SDNN data
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

#### Open SDNN data ####
df <- readRDS('../data/helius_malefemale.RDS')
head(df)
df$ID<-paste0('S', df$ID)
d_young <- df %>% filter(!is.na(SDNN)) %>% filter(Age_cat == "Young")
d_old <- df %>% filter(!is.na(SDNN)) %>% filter(Age_cat == "Old")

#### Make input data for ML model - Females Young ####
df_met_young <- df_met %>% filter(rownames(.) %in% d_young$ID) # filter metabolite df for same subjects
d_young <- d_young[match(rownames(df_met_young), d_young$ID), ] # Put in same sequence of IDs
all(d_young$ID == rownames(df_met_young)) # check if input and output IDs match => TRUE
d_young$ID
rownames(df_met_young)

path <- 'AgeStrata/FemY_SDNN'
dir.create(path)
write_data(ddd, file.path(path, 'input_data'))
y <- as.data.frame(d_young$logSDNN)
write_y(y, name_y = 'y_reg.txt', file.path(path, 'input_data'))

#### Make input data for ML model - Females Old ####
df_met_old <- df_met %>% filter(rownames(.) %in% d_old$ID) # filter metabolite df for same subjects
d_old <- d_old[match(rownames(df_met_old), d_old$ID), ] # Put in same sequence of IDs
all(d_old$ID == rownames(df_met_old)) # check if input and output IDs match => TRUE
d_old$ID
rownames(df_met_old)

path <- 'AgeStrata/FemO_SDNN'
dir.create(path)
write_data(df_met_old, file.path(path, 'input_data'))
y <- as.data.frame(d_old$logSDNN)
write_y(y, name_y = 'y_reg.txt', file.path(path, 'input_data'))
