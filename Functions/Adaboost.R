library(adabag)

#####################################################
## Run Adaboost model
Adaboost=function(Class,Features){
  data <- data.frame(Features,Class=as.factor(Class))
  Ada=boosting(Class ~ . ,data=data ,boos=TRUE,
           coeflearn = 'Zhu', mfinal = 10, control = rpart.control(minsplit = 0))
  return(Ada)
}
######################################################  



  
######################################################
## predictions function
Ada_Predict=function(modelFit,Test_Features){
  p_boost = predict(modelFit,Test_Features, probability=TRUE)
  a=p_boost$class
  b=p_boost$prob
  return(list(probability=b,class_pred=a))
  
  }
#######################################################
















