assesment=function(Test_Class,pred_class,probabolity){
  
  result=list()
  aa=as.factor(Test_Class)
  mmm=confusionMatrix(aa, as.factor(pred_class))
  result[["accuracy"]]=mmm$overall[1]
  result[["sensitivity"]]=mmm$byClass[1]
  result[["specificity"]]=mmm$byClass[2]
  result[["Precision"]]=mmm$byClass[5]
  result[["Recall"]]=mmm$byClass[6]
  
  
  classes <- levels(as.factor(Test_Class))
  for (o in 1:length(classes)) {
    true_values <- ifelse(Test_Class==classes[o],1,0)
    # Assess the performance of classifier for class[i]
    pred <- prediction(probabolity[,o],true_values)
    perf <- performance(pred, "tpr", "fpr")
    auc.perf <- performance(pred, measure = "auc")
    result[[paste0("auc_",classes[o])]]=auc.perf@y.values[[1]]
    
  }
  result
}




# add PCA features
PC_model=readRDS("PCmodel_1.rds")
inpt=data37[data37$`metadata$specimen`%in% test_set_ID,pos_sp+4]
inpt_pc=predict(PC_model,inpt)
inpt_final=cbind(inpt,inpt_pc)
model = readRDS("model_2.rds")

intr_data_test=(data.frame(inpt_final))

assesment(as.factor(ifelse(metadata[metadata$specimen%in%test_set_ID,12]=="True",1,0)),
            predict(model, newdata = intr_data_test, probability=TRUE),
            predict(model, newdata = intr_data_test, type='prob'))