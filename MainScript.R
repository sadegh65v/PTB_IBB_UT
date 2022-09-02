#### 1. Prepare Problem ####
# a) Load packages  ####
require(tidyr)
library(mlbench)
library(e1071)
library(caret)
library(caTools)
library(openxlsx)

# source functions
source("Functions/Adaboost.R")
source("Functions/DecisionsTree.R")
source("Functions/RandomForest.R")
source("Functions/Assesment.R")
source("Functions/PCA.R")
source("Functions/Datacleaning.R")
source("Functions/MainFunction.R")



raw_input_folder= "../Data/training_data_2022-07-21/"




if (file.exists(paste0(raw_input_folder,'all_data.RData'))) {
  load(paste0(raw_input_folder,'all_data.RData'))
}else {
  construct_cleaned_data(raw_input_folder)
  load(paste0(raw_input_folder,'all_data.RData'))
}




# Run_ML_models options:
# result_filename="result.xlsx"
# Data
# meta_data
# combinations_list=NULL
# splited_datasets=10
# train_ratio=0.7
# initial_filter=50
# total_data_used=T
# modelVec = c("Adaboost","Decisions_Tree","RandomForest")
# predictVec = c("Ada_Predict","DT_Predict","RF_Predict")
# use_PCA=T

Run_ML_models(result_filename="result123.xlsx",
              Data=all_data,
              meta_data=metadata,
              combinations_list=list(c(1,2),c(1)),
              splited_datasets=4
)




Run_ML_models(result_filename="resulttest2.xlsx",
              Data=all_data,
              meta_data=metadata,
              combinations_list=list(c(1,2),c(1),c(1,3,4,5)),
              splited_datasets=5,
              train_ratio=0.6,
              initial_filter=50,
              total_data_used=F,
              modelVec = c("Decisions_Tree","RandomForest"),
              predictVec = c("DT_Predict","RF_Predict"),
              use_PCA=F)

###############################################################################################
















