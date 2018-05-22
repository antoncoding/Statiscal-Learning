load('hw5ds1.rdata')
testfold = 1
dstrain1 = hw5ds1[-folds[[testfold]],]

plogp = function(p){
  if(p < 0.000001){
    return(0)
  }
  else{
    return(log2(p)*p)
  }
}

filter_ig = function(dstrain, ypos='pos', min_count=5, ig_threshold=0.00001){
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
  
  ig_array = c()
  for(i in 1:numFeature){  
    pos_present = sum_pos[i] 
    neg_present = sum_neg[i] 
    
    pos_absent = total_pos - pos_present #141
    neg_absent = total_neg - neg_present #774106
  
    sum_present = pos_present + neg_present # 27701
    sum_absent = pos_absent + neg_absent #774247
    
    # Stats
    pos_ = (total_pos)/(total_neg+total_pos)
    neg_ = (total_neg)/(total_neg+total_pos)
    pos_gvn_present_ = pos_present/sum_present
    pos_gvn_absent_ = pos_absent/sum_absent 
    neg_gvn_present_ = neg_present/sum_present
    neg_gvn_absent_ = neg_absent/sum_absent 
    
    present_ratio = sum_present/(sum_present+sum_absent)
    
    ig = 0
    ig = ig - plogp(pos_) -plogp(neg_) 
    ig = ig + (1 - present_ratio)*plogp(pos_gvn_absent_) + present_ratio*plogp(pos_gvn_present_)
    ig = ig + (1- present_ratio)*plogp(neg_gvn_absent_) + (present_ratio)*plogp(neg_gvn_present_)
    
    if(ig==0){
      ig_array = append(ig_array, 0)
    }
    else{
      ig_array = append(ig_array, ig)
    }
    #break
  }
  ig_array = ig_array[ig_array>ig_threshold]
  
  if(length(ig_array)==0 || is.null(ig_array) || is.na(ig_array)){
    return(list(colpos=NULL, colname=NULL, igvalue=NULL))
  }
  
  ig_array = sort(ig_array, decreasing = TRUE)
  indexs = match(names(ig_array), colnames(dstrain))
  return_list = list(colpos=indexs, colname=names(ig_array), igvalue=as.numeric(ig_array))
  return(return_list)
}



out1 = filter_ig(dstrain1)
out1
print(head(out1$igvalue, n=15))

