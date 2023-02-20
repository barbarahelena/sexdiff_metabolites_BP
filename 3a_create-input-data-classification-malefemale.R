# Create XGB input files for classification model
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
df <- df %>% mutate(Sex = case_when(Sex == "Male" ~ 1,
                           Sex == "Female" ~ 0)) # ML model can only work with 0-1

#### Make input data for ML model ####
df_met <- df_met %>% filter(rownames(.) %in% df$ID) # filter metabolite df for same subjects
df <- df[match(rownames(df_met), df$ID), ] # Put in same sequence of IDs
all(df$ID == rownames(df_met)) # check if input and output IDs match => TRUE
df$ID
rownames(df_met)

path <- 'MaleFemale'
dir.create(path)
write_data(df_met, file.path(path, 'input_data'))
y <- as.data.frame(df$Sex)
write_y(y, name_y = 'y_binary.txt', file.path(path, 'input_data'))
