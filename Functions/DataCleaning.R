

# Load data

# input_folder= "../Data/training_data_2022-07-21/"
  
load_cleaned_data <- function(input_folder) {
  main=getwd()
  setwd(input_folder)
  sv = read.csv('sv_counts/sp_sv_long.csv',stringsAsFactors=T)
  sv = sv[,c(2,3,6)]
  sv = spread(sv, sv, fract)
  specimen = sv[,1]
  temp = as.matrix(sv[,-1])
  temp[is.na(temp)] = 0
  col_sum = apply(temp>0, 2, function(x){sum(x)})
  
  
  
  temp = temp[,col_sum>=5]
  sv = cbind(specimen, as.data.frame(temp))
  rm(temp,col_sum,specimen)
  
  
  alpha = read.csv('alpha_diversity/alpha_diversity.csv',stringsAsFactors=T)
  
  tax_fam = read.csv('taxonomy/taxonomy_relabd.family.csv',stringsAsFactors=T)
  
  tax_gen = read.csv('taxonomy/taxonomy_relabd.genus.csv',stringsAsFactors=T)
  
  tax_sp = read.csv('taxonomy/taxonomy_relabd.species.csv',stringsAsFactors=T)
  
  
  
  cst = read.csv('community_state_types/cst_valencia.csv',stringsAsFactors=T)
  cst=cst[,-c(2,3,4)]
  
  
  
  meta = read.csv('metadata/metadata.csv')
  str(meta)
  # unique(meta$delivery_wk)
  # unique(meta$age)
  # 
  
  all_data = list(alpha=alpha, tax_fam=tax_fam, tax_gen=tax_gen, tax_sp=tax_sp, cst=cst, sv=sv)
  
  for (i in 1:6){
    print(names(all_data)[i])
    print(dim(all_data[[i]]))
    print(sum(meta$specimen == all_data[[i]]$specimen))
    print(colnames(all_data[[i]])[1])
  }
  
  
  was_term = meta$was_term
  metadata=meta
  for (i in 1:nrow(metadata)) {
    if (all(metadata$age[i] != c("18_to_28","29-38","Above_38","Below_18","Unknown"))) {
      metadata$age[i]<- as.numeric(metadata$age[i])
      age=metadata$age[i]
      if (age <= 18) {metadata$age_group [i]="< 18"}
      if (age > 18 & age <= 28) {metadata$age_group[i]="18-28"}
      if (age >= 29 & age <= 38) {metadata$age_group[i]="29-38"}
      if (age > 38) {metadata$age_group[i]="> 38"}
      
    }else{
      age=metadata$age[i]
      if (age =="18_to_28") {metadata$age_group [i]="18-28"}
      if (age =="29-38") {metadata$age_group [i]="29-38"}
      if (age =="Above_38") {metadata$age_group [i]="> 38"}
      if (age =="Below_18") {metadata$age_group [i]="< 18"}
      if (age =="Unknown") {metadata$age_group [i]="Unknown"}
    }
    
  }
  
  
  save(all_data, metadata, was_term, file='all_data.RData')
  
  # rm(list=ls())
  load('all_data.RData')
  setwd(main)
}

