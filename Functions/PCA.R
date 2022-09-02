
#####################################################
## Run PCA 
PCA=function(Features,jth_top_pc =ceiling(0.1*length(Features)) ,scale_indicator = TRUE){
  pc <- prcomp(Features,rank. = jth_top_pc,
               center = TRUE,
               scale. = scale_indicator)
  return(pc)
}
######################################################  




######################################################
## PCA predictions function
PCA_Predict=function(PCmodel,Features){
  transforme_features <- predict(PCmodel, Features)
  return(as.data.frame(transforme_features))
}
#######################################################