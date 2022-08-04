library(rpart)
#####################################################
## Run Decisions Tree model
Decisions_Tree=function(Class,Features){
  data <- data.frame(Features,Class=as.factor(Class))
  DT=rpart(Class~., data = data, method = 'class')
  return(DT)
}
######################################################  



  
######################################################
## predictions function
DT_Predict=function(modelFit,Test_Features){
  a=predict(modelFit, newdata = Test_Features, type = 'class')
  b=predict(modelFit, newdata = Test_Features, type='prob')
  return(list(probability=b,class_pred=a))

  }
#######################################################




