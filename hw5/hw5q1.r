load('hw5ds1.rdata')
testfold = 2
dstrain1 = hw5ds1[-folds[[testfold]],]

filter_chisq = function(dstrain, ypos='pos', min_count=5, chi_threshold=0.00001){
  df_pos = dstrain[dstrain[,1]==ypos, ][, -1]
  df_neg = dstrain[dstrain[,1]!=ypos, ][, -1]
  
  sum_pos = rep(0, ncol(df_pos))
  sum_neg = rep(0, ncol(df_neg))
  if(nrow(df_pos)>0){
    f1 = df_pos>0  
    sum_pos = colSums(f1)
  }
  if(nrow(df_neg)>0){
    f2 = df_neg>0  
    sum_neg = colSums(f2)
  }
  
  all_sum = sum_pos + sum_neg
  
  df_pos = df_pos[,all_sum>min_count]
  df_neg = df_neg[,all_sum>min_count]
  
  sum_pos = sum_pos[all_sum>min_count]
  sum_neg = sum_neg[all_sum>min_count]
  
  total_pos = nrow(df_pos)
  total_neg = nrow(df_neg)
  count_row = total_pos+total_neg
  
  numFeature = ncol(df_pos)
  
  chi_array = c()
  for(i in 1:numFeature){  
    pos_present = sum_pos[i] 
    neg_present = sum_neg[i] 
    
    pos_absent = total_pos - pos_present #141
    neg_absent = total_neg - neg_present #774106
  
    sum_present = pos_present + neg_present # 27701
    sum_absent = pos_absent + neg_absent #774247
    
    E_PP = sum_present * total_pos / count_row
    E_PA = sum_absent * total_pos / count_row
    E_NP = sum_present * total_neg/ count_row
    E_NA = sum_absent * total_neg/ count_row
    
    chi = 0.0
    chi = chi + ((pos_present - E_PP)*(pos_present - E_PP))/E_PP
    chi = chi + ((pos_absent - E_PA)*(pos_absent - E_PA))/E_PA
    chi = chi + ((neg_present - E_NP)*(neg_present - E_NP))/E_NP
    chi = chi + ((neg_absent - E_NA)*(neg_absent - E_NA))/E_NA
    
    if(is.null(chi)){
      chi_array = append(chi_array, 0)
    }
    else{
      chi_array = append(chi_array, chi)
    }
  }
  chi_array = chi_array[chi_array>chi_threshold]
  
  if(length(chi_array)==0 || is.null(chi_array) || is.na(chi_array)){
    return(list(colpos=NULL, colname=NULL, chistat=NULL))
  }
  
  chi_array = sort(chi_array, decreasing = TRUE)
  indexs = match(names(chi_array), colnames(dstrain))
  return_list = list(colpos=indexs,colname=names(chi_array), chistat=as.numeric(chi_array))
  return(return_list)
}

out1 = filter_chisq(dstrain1)
print(head(out1$chistat, n=15))
print(head(out1$colpos, n=15))
print(head(out1$colname, n=15))
