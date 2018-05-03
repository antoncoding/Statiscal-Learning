load(file='rtb1_train.rdata')
rtb1_train = rtb1_train[1:300,]

gen_utagmat <- function(utagvec, y){
  
  # Get all user ID
  user_list = c()
  for (row in utagvec) {
    tags = strsplit(row,',')
    for(tag in tags){
      for(i in tag){
        if(! i %in% user_list){
          if(! is.na(i)){
            user_list <- c(user_list, i)  
          }
        }
      }
    }
  }
  # init return dataframe
  cnames = sapply(user_list, function(i){return(paste('user_',i,sep=""))})
  df <- data.frame(matrix(ncol=length(cnames)+1, nrow = 0))
  colnames(df) <- c('constant',cnames)
  
  for (rowcount in 1:length(utagvec)) {
    row = utagvec[rowcount]
    tags = strsplit(row,',')
    
    df[rowcount,] = rep(c(0), times = length(cnames)+1)
    df[rowcount,1] = 1
    
    for(tag in tags){
      for(i in tag){
        if(!is.na(i)){
          df[rowcount, paste('user_',i,sep="")] = 1 
        }
      }
    }
  }
  
  df = df[colSums(df)>=5]
  cnames = names(df)
  
  # Compute t value and sort
  allt = rep(999, length(cnames))
  names(allt)=cnames
  for(uid in cnames) {
    x = df[,uid]
    if(uid!='constant'){
      allt[uid] = reg_tvalue(y,x)  
    }
  }
  
  df = df[abs(allt)>1]
  allt2 = allt[abs(allt)>1]

  #sort by abs t-value decreasing
  o1 = order(abs(allt2), decreasing=TRUE)
  df2 = df[o1]
  
  return(as.matrix(df2))
}


reg_tvalue = function(y, x) {
  if(length(y) != length(x)) {
    stop("Inconsistent length of y and x")
  }
  y=matrix(y, ncol=1)
  xmat=matrix(1, ncol=2, nrow=length(y))
  xmat[,2] = x    
  bhead = solve(t(xmat)%*%xmat, t(xmat)%*%y)
  yhead = xmat %*% bhead
  e1 = y - yhead
  var1 = sum(e1 * e1) / (length(e1)-2)    
  sigma2 = solve(t(xmat)%*%xmat) * var1
  t1=bhead[2]/sqrt(sigma2[2,2])    
  return(t1)
}

umat1 = gen_utagmat(rtb1_train$user_tags, rtb1_train$paying_price)
head(umat1)

y = rtb1_train$paying_price
w = solve(t(umat1) %*% umat1, t(umat1) %*% y)
print(w)
