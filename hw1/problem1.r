options(scipen=10)
df1_train = read.csv('df1_train.csv')
df1_test1 = read.csv('df1_test1.csv')
df1_test1y = read.csv('df1_test1y.csv')

gpredict<-function(dftrain, dftest){
  dftrain_a <- dftrain[,1]
  dftrain_b <- data.matrix(dftrain[,2:ncol(dftrain)])
  mu_a <- mean(dftrain_a)
  mu_b <- colMeans(dftrain_b)
  sigma_ab <- cov(dftrain_a, dftrain_b) #1*43
  sigma_bb <- cov(dftrain_b)    #43*43
  sigma_bb_i <- solve(sigma_bb) #50*43
  if(!missing(dftest)){
    if(ncol(dftest)==ncol(dftrain_b)){
      # Correct
      predictions <- as.vector(sigma_ab %*% (sigma_bb_i %*% t(sweep(dftest,2,mu_b))) + mu_a )
    }
    else{
      # Column Error
      return(NULL)
    }
  }
  else{
    # Missing Testing File
    predictions = NULL
  }
  # Return Result in List
  return_list =list(mua=mu_a, mub=mu_b, s_ab = sigma_ab, s_bb = sigma_bb, predict= predictions)
  return(return_list)
}

out1 = gpredict(df1_train[1:200,],df1_test1)
print(out1$mua)
print(out1$mub[1:5])
print(out1$s_ab[1:5])
print(out1$s_bb[1:5,1:5])

mae1a = mean(abs(df1_test1y[,1] - out1$predict))
cat("MAE1a=", mae1a, "\n")
