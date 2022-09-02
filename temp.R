
# Load data
load('../data/input_dataset.RData')



###################





####################
#dir.create("Datasets_Tr_Ts")

# Splitting the dataset
library(caTools) 
deatsetsnumber=10
for(i in 1:deatsetsnumber){
  split = sample.split(input_dataset$Class, SplitRatio = 0.60) 
  training_set = subset(input_dataset, split == TRUE) 
  test_set = subset(input_dataset, split == FALSE) 
  write.table(training_set,paste0("Datasets_Tr_Ts/training_set",i,".txt"))
  write.table(test_set,paste0("Datasets_Tr_Ts/test_set",i,".txt"))
}
####################





#### Feature engineering ####




############################



# source functions 
source("Functions/Adaboost.R")
source("functions/DecisionsTree.R")
source("functions/RandomForest.R")
source("functions/Assesment.R")

##############################



#### ML models vector ####
modelVec <- c("Adaboost",
              "Decisions_Tree","RandomForest")
predictVec <- c("Ada_Predict",
                "DT_Predict","RF_Predict")


#### create final results list 
finalmodelresult <- vector(mode = "list", length = length(modelVec))
names(finalmodelresult)=modelVec
a=matrix(NA,deatsetsnumber,5+length(levels(input_dataset$Class)))
rownames(a)=paste0("dataset_",1:deatsetsnumber)
colnames(a)=c("accuracy","sensitivity","specificity","Precision","Recall","auc_False","auc_True" )
for (model in 1:length(modelVec)) {
  finalmodelresult[[model]]=a
  
}



#### run models for all splited datsets 
for (datasetTT in 1:deatsetsnumber) {
  
  print(datasetTT)
  #### Feature selection ####
  
  
  training_set=read.table(paste0("Datasets_Tr_Ts/training_set",datasetTT,".txt"),stringsAsFactors = T)
  testing_set=read.table(paste0("Datasets_Tr_Ts/test_set",datasetTT,".txt"),stringsAsFactors = T)
  
  
  important_features=c()
  
  for (i in 6:length(training_set)) {
    important_features=c(important_features,length(which(training_set[,i]>0)))
  }
  
  pos_sp=which(important_features>=40)
  
  training_set=training_set[,c(1:4,pos_sp+5)]
  
  # Class=training_set$Class
  # training_set=training_set[,-c(1,5)]
  # testing_set$Class
  # pred$class_pred
  # pred$probability
  

  for (z in 1:length(modelVec)) {
    modelFun = get(modelVec[z])
    predictFun = get(predictVec[z])
    modelFit = modelFun(training_set$Class,training_set[,-c(1,5)])
    pred = predictFun(modelFit, testing_set)
    finalmodelresult[[z]][datasetTT,]=unlist(assesment(as.factor(testing_set$Class),pred$class_pred,pred$probability))
  }
  
  

  
}












