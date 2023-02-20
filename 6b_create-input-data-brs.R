# Create XGB input files for BRS data
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
d_male <- df %>% filter(!is.na(BRS)) %>% filter(Sex == "Male")
d_female <- df %>% filter(!is.na(BRS)) %>% filter(Sex == "Female")

#### Make input data for ML model - BRS Males ####
df_met_male <- df_met %>% filter(rownames(.) %in% d_male$ID) # filter metabolite df for same subjects
d_male <- d_male[match(rownames(df_met_male), d_male$ID), ] # Put in same sequence of IDs
all(d_male$ID == rownames(df_met_male)) # check if input and output IDs match => TRUE
d_male$ID
rownames(df_met_male)

path <- 'BRS/Male'
dir.create(path)
write_data(df_met_male, file.path(path, 'input_data'))
y <- as.data.frame(d_male$BRS)
write_y(y, name_y = 'y_reg.txt', file.path(path, 'input_data'))

#### Make input data for ML model - BRS Females ####
df_met_female <- df_met %>% filter(rownames(.) %in% d_female$ID) # filter metabolite df for same subjects
d_female <- d_female[match(rownames(df_met_female), d_female$ID), ] # Put in same sequence of IDs
all(d_female$ID == rownames(df_met_female)) # check if input and output IDs match => TRUE
d_female$ID
rownames(df_met_female)

path <- 'BRS/Female'
dir.create(path)
write_data(d_met_female, file.path(path, 'input_data'))
y <- as.data.frame(d_female$BRS)
write_y(y, name_y = 'y_reg.txt', file.path(path, 'input_data'))
