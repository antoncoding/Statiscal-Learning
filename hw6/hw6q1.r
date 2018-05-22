load('hw6ds1.rdata')

library('randomForest')
filter_chisq = function(dstrain, ypos="pos", min_count=5, chi_threshold = 1e-5) {
  nugrams = ncol(dstrain) #number of unigram+1
  chiall = rep(-1, nugrams) #the first column is always -1, and willnot be selected.
  yvec = as.numeric(dstrain[,1]==ypos)
  options(warn = -1) #silence the warning
  for(ii in 2:nugrams) {
    tmp1=cbind(yvec, as.numeric(dstrain[,ii]>0))
    tmp1a=table(tmp1[,1], tmp1[,2])
    if(nrow(tmp1a)<2 | ncol(tmp1a)<2) {
      #stop("tmp1a table dimension too small!")
      chiall[ii] = 0
    } else if(sum(tmp1[,2])<=min_count) {
      chiall[ii] = 0
      #cat("feature", ii, "count too low, skip\n")
    } else {
      tmp2=chisq.test(tmp1a, correct=FALSE)
      chiall[ii] = tmp2$statistic
    }
  }
  options(warn = 0) #turn the warnings back on
  o1 = order(chiall, decreasing=TRUE)
  tmpind1 = chiall[o1] > chi_threshold
  if(sum(tmpind1) ==0) {
    #cat("We have not features selected. The maximum value of chisq test is ", max(chiall), "\n")
    return(list(colpos = NULL, colname=NULL, chistat=NULL))
  } else {
    o2=o1[tmpind1]
    retname = names(dstrain)[o2]
    return(list(colpos = o2, colname=retname, chistat=chiall[o2]))
  }
}

rf_carton = function(dsall, folds, testfold, ypos='pos', chi_threshold=0.1, grid_length=20, grid_type="loglinear", rfntree=500, debuglevel=0){
  ycol=1
  dstrain_all = dsall[-folds[[testfold]],]
  dstest = dsall[folds[[testfold]],]
  
  if(testfold==1) {
    tunefold=10
  } else {
    tunefold=testfold-1
  }
  dstune = dsall[folds[[tunefold]],]
  dstrain = dsall[-c(folds[[tunefold]], folds[[testfold]]),]

  dstrain_all_y = as.factor(dstrain_all[,ycol]== ypos)
  dstrain_y = as.factor(dstrain[,ycol] == ypos)
  dstune_y = as.factor(dstune[,ycol] == ypos)
  dstest_y = as.factor(dstest[,ycol]==ypos)
  
  filtered = filter_chisq(dstrain, ypos=ypos, chi_threshold=chi_threshold)
  
  dstrain_x = dstrain[,filtered$colpos]
  dstune_x = dstune[,filtered$colpos]
  
  m_min = 2
  m_max = length(filtered$colpos)
  if(grid_type!='loglinear'){
    grids = unique(round(seq(m_min, m_max, length=grid_length)))
  }else{
    grids = unique(round(exp(seq(log(m_min), log(m_max), length=grid_length))))
  }
  
  tune_f1 = c()
  
  for(i in 1:length(grids)){
    grid = grids[i]
    rm = randomForest(x=dstrain_x, y=dstrain_y, xtest=dstune_x, ytest=dstune_y, mtry=grid, ntree=rfntree)
    precision = rm$test$confusion[2,2]/(rm$test$confusion[1,2]+rm$test$confusion[2,2])
    recall = rm$test$confusion[2,2]/(rm$test$confusion[2,1]+rm$test$confusion[2,2])
    f1 = 2*precision*recall/(precision+recall)
    tune_f1 = append(tune_f1, f1)
  }
  bestIndex = which.max(tune_f1)
  best_m = grids[bestIndex]
  
  # Final Training and Prediction
  
  filtered = filter_chisq(dstrain_all, ypos=ypos, chi_threshold=chi_threshold)
  dstest_x = dstest[,filtered$colpos]
  dstrain_all_x = dstrain_all[,filtered$colpos]
  
  rm = randomForest(x=dstrain_all_x, y=dstrain_all_y, xtest = dstest_x, ytest = dstest_y, mtry = best_m, ntree = rfntree)
  precision = rm$test$confusion[2,2]/(rm$test$confusion[1,2]+rm$test$confusion[2,2])
  recall = rm$test$confusion[2,2]/(rm$test$confusion[2,1]+rm$test$confusion[2,2])
  f1 = 2*precision*recall/(precision+recall)
  test=list(precision=precision, recall=recall, f1=f1)
  
  return_list = list(mgrids=grids, f1_all=tune_f1, best_m=best_m, test=test, fselect=filtered)
  return(return_list)
}


#rftest=rf_carton(ds1, cvfold, testfold=1, debuglevel=0)
set.seed(5556)
rftest=rf_carton(ds1, cvfold, testfold=1, debuglevel=0)

print(rftest)

#print(rftest$mgrids)
#print(rftest$f1_all)
#print(rftest$best_m)
#print(rftest$test)
