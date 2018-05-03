load(file='rtb1_train.rdata')
rtb1_train = rtb1_train[1:200,]

gen_uagentmat <- function(utagvec, y){
  agent_list = c()
  pattern <- "([A-Za-z][A-Za-z0-9]{1,})"
  for (utagstr in utagvec) {
    features=regmatches(utagstr, gregexpr(pattern, utagstr))
    features=lapply(features, unique)
    for(feature in features){
      for(i in feature){
      if(! i %in% agent_list){
        if(! is.na(i)){
          agent_list <- c(agent_list, i)  
        }
      }
      }
    }
  }
  
  # #init return dataframe
  cnames = sapply(agent_list, function(i){return(paste('agent_',i,sep=""))})
  df <- data.frame(matrix(ncol=length(cnames)+1, nrow = 0))
  colnames(df) <- c('constant',cnames)
  
  for (rowcount in 1:length(utagvec)) {
    utagstr = utagvec[rowcount]
  
    features=regmatches(utagstr, gregexpr(pattern, utagstr))
    features=lapply(features, unique)
      
    df[rowcount,] = rep(c(0), times = length(cnames)+1)
    df[rowcount,1] = 1
    
    for(feature in features){
      for(i in feature){
        if(!is.na(i)){
          df[rowcount, paste('agent_',i,sep="")] = 1 
        }
      }
    }
  }
  
  df2 = df[colSums(df)>=10]
  df2 = df2[colSums(df2)<(length(utagvec)/2)]
  df2$constant = 1
  cnames = names(df2)
  
  # #Compute t value and sort
  allt = rep(999, length(cnames))
  names(allt)=cnames
  for(uid in cnames) {
    x = df[,uid]
    if(uid!='constant'){
      allt[uid] = reg_tvalue(y,x)
      print(allt[uid])
    }
  }
  
  df2 = df2[abs(allt)>1]
  allt2 = allt[abs(allt)>1]

  o1 = order(abs(allt2),colnames(df2), decreasing=TRUE)
  df2 = df2[o1]
  
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

umat1 = gen_uagentmat(rtb1_train$user_agent, rtb1_train$paying_price)
head(umat1)

print(head(sort(colSums(umat1), decreasing=TRUE), n=10))

qr1 = qr(umat1, tol =1e-7)
ind3 = qr1$pivot[1:qr1$rank]
rank0 = ncol(umat1)
if(qr1$rank < rank0) {
    cat("There are", rank0, "columns, but rank is only",
        qr1$rank, "\n")
    toremove = qr1$pivot[(qr1$rank+1):rank0]
      cat("list of features removed", toremove,"\n")
      tokeep = qr1$pivot[1:qr1$rank]
        umat1 = umat1[,tokeep]
}
