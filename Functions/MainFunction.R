
Run_ML_models=function(result_filename="result.xlsx",
                       Data,
                       meta_data,
                       combinations_list=NULL,
                       splited_datasets=10,
                       train_ratio=0.7,
                       initial_filter=50,
                       total_data_used=T,
                       modelVec = c("Adaboost","Decisions_Tree","RandomForest"),
                       predictVec = c("Ada_Predict","DT_Predict","RF_Predict"),
                       use_PCA=T
){
  
  if (is.null(combinations_list)) {
    comb_list=list()
    for (l in 1:6) {
      a= combn(1:6, l)
      v=list()
      for (hh in 1:ncol(a)) {
        v[[hh]]=a[,hh]
      }
      comb_list=c(comb_list,v)
    }}else{
      comb_list=combinations_list
    }
  
  
  uniuqe_id = unique(meta_data$participant_id)
  selected_id=meta_data[,c(2,3,6)]
  selected_id$select=F
  for (i in 1:length(uniuqe_id)) {
    jj=selected_id[selected_id$participant_id==uniuqe_id[i],]
    jj=jj[jj$collect_wk==min(jj$collect_wk),]
    selected_id[selected_id$participant_id==uniuqe_id[i] & selected_id$collect_wk==min(jj$collect_wk),4]= c(T,rep(F,length(jj$participant_id)-1))
  }
  
  
  ID=selected_id[selected_id$select & selected_id$collect_wk<=32 & selected_id$collect_wk>=15 ,1]
  part_ID=selected_id[selected_id$select & selected_id$collect_wk<=32 & selected_id$collect_wk>=15 ,2]
  
  
  total_spec_final=list()
  
  for (j in 1:length(comb_list)) {
    print(paste("combination",paste(comb_list[[j]],collapse = "&")))
    for (k_split in 1:splited_datasets) {
      print(paste("Constructing Dataset",k_split))
      split = sample.split(ID, SplitRatio = train_ratio)
      test_set = subset(ID, split == FALSE)
      specimen=test_set
      test_set=as.data.frame(specimen)
      
      if (total_data_used==T) {
        part_id_test=selected_id[selected_id$specimen%in%specimen,2 ]
        training_set=meta_data[!meta_data$specimen %in% specimen &
                                 !meta_data$participant_id%in%part_id_test ,2 ]
      }else{
        training_set = subset(ID, split == TRUE)
      }
      
      specimen=training_set
      training_set=as.data.frame(specimen)
      
      specimen=meta_data$specimen
      dataset=as.data.frame(specimen)
      for (d in comb_list[[j]]) {
        dataset=cbind(dataset,all_data[[d]][,-1])
      }
      local_train=dataset[dataset$specimen %in% as.factor(training_set$specimen),]
      local_test=dataset[dataset$specimen %in% as.factor(test_set$specimen),]
      
      important_features=c()
      for (i in 2:length(local_train)) {
        important_features=c(important_features,length(which(local_train[,i]>0)))
      }
      pos_sp=which(important_features>=initial_filter)
      local_train=local_train[,pos_sp+1]
      local_test=local_test[,pos_sp+1]
      
      if (use_PCA==T) {
        train_pc=PCA(local_train[,-1])
        train_predict=PCA_Predict(train_pc,local_train[,-1])
        test_predict=PCA_Predict(train_pc,local_test[,-1])
        training_set=cbind(training_set,train_predict)
        test_set=cbind(test_set,test_predict)
        
        
      }else{
        training_set=cbind(training_set,local_train)
        test_set=cbind(test_set,local_test)
      }
      
      
      training_set=cbind(Class=meta_data[meta_data$specimen %in% training_set$specimen,4],
                         collect_wk=meta_data[meta_data$specimen %in% training_set$specimen,6],
                         training_set)
      test_set=cbind(Class=meta_data[meta_data$specimen %in% test_set$specimen,4],
                     collect_wk=meta_data[meta_data$specimen %in% test_set$specimen,6],
                     test_set)
      
      
      if (!dir.exists("Datasets_Tr_Ts")) {
        dir.create("Datasets_Tr_Ts")
      }
      
      write.table(training_set,paste0("Datasets_Tr_Ts/training_set",k_split,".txt"))
      write.table(test_set,paste0("Datasets_Tr_Ts/testing_set",k_split,".txt"))
    }
    
    
    finalmodelresult <- vector(mode = "list", length = length(modelVec))
    names(finalmodelresult)=modelVec
    a=matrix(NA,splited_datasets,5+length(levels(as.factor(meta_data$was_term))))
    rownames(a)=paste0("dataset_",1:splited_datasets)
    colnames(a)=c("accuracy","sensitivity","specificity","Precision","Recall","auc_False","auc_True" )
    for (model in 1:length(modelVec)) {
      finalmodelresult[[model]]=a
    }
    
    for (datasetTT in 1:splited_datasets) {
      
      print(paste("Run ML models on dataset",datasetTT))
      
      training_set=read.table(paste0("Datasets_Tr_Ts/training_set",datasetTT,".txt"),stringsAsFactors = T)
      testing_set=read.table(paste0("Datasets_Tr_Ts/testing_set",datasetTT,".txt"),stringsAsFactors = T)
      
      train_y=training_set$Class
      test_y=testing_set$Class
      x=training_set[,-c(1,3)]
      
      
      for (z in 1:length(modelVec)) {
        modelFun = get(modelVec[z])
        predictFun = get(predictVec[z])
        modelFit = modelFun(train_y,x)
        pred = predictFun(modelFit, testing_set)
        finalmodelresult[[z]][datasetTT,]=unlist(assesment(as.factor(test_y),pred$class_pred,pred$probability))
        
      }
    }
    total_spec_final[[paste0("comb",paste0(comb_list[[j]],collapse = ""),"-n_features",length(training_set)-2)]]=finalmodelresult
  }
  
  
  finalmodelresult <- vector(mode = "list", length = length(modelVec))
  names(finalmodelresult)=modelVec
  a=matrix(NA,length(total_spec_final),9)
  rownames(a)=1:length(total_spec_final)
  if (use_PCA==T) {
    colnames(a)=c("Combination","PC_Number","Accuracy","Sensitivity","Specificity","Precision","Recall","auc_False","auc_True" )
  }else{
    colnames(a)=c("Combination","Features","Accuracy","Sensitivity","Specificity","Precision","Recall","auc_False","auc_True" )
  }
  
  for (model in 1:length(modelVec)) {
    finalmodelresult[[model]]=a
  }
  
  
  for (g in 1:length(total_spec_final)) {
    pc=names(total_spec_final[g])
    pc=strsplit(pc,split = "_")[[1]][2]
    pc=as.numeric(paste0(strsplit(pc,split = "")[[1]][-(1:8)],collapse = ""))
    for (ML_name in 1:length(modelVec)) {
      finalmodelresult[[ML_name]][g,1] =paste(comb_list[[g]],collapse = " & ")
      finalmodelresult[[ML_name]][g,2] =pc
      mean=apply(total_spec_final[[g]][[ML_name]], 2, mean)
      SD=apply(total_spec_final[[g]][[ML_name]], 2, sd)
      for (n in 1:7) {
        finalmodelresult[[ML_name]][g,n+2] = paste0("m=",round(mean[n],digits = 4),"  sd=",round(SD[n],digits = 4))
      }
    }}
  
  hs <- createStyle(
    textDecoration = "BOLD", fontColour = "#FFFFFF", fontSize = 12,
    fontName = "Arial Narrow", fgFill = "#4F80BD"
  )
  write.xlsx(finalmodelresult, file = result_filename,
             colNames = TRUE, borders = "rows", headerStyle = hs,colWidths = "auto")
}
#############################################################################################

