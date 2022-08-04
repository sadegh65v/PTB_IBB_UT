library(randomForest)
#####################################################
## Run Random Farest model
RandomForest=function(Class,Features){
  mtry <- tuneRF(Features,Class, ntreeTry=500,
                 stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
  best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
  data <- data.frame(Features,Class=as.factor(Class))
  rf <- randomForest(Class ~ . , data = data, mtry=best.m, importance=TRUE,ntree=100)
  return(rf)
}
######################################################  



  
######################################################
## predictions function
RF_Predict=function(modelFit,Test_Features){
  a=predict(modelFit, newdata = Test_Features, probability=TRUE)
  b=predict(modelFit, newdata = Test_Features, type='prob')
  return(list(probability=b,class_pred=a))
  
  }
#######################################################