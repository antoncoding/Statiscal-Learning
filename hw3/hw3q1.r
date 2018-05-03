load(file='rtb2_train.rdata')
nfeat = 20
rtb3 = rtb2_train[1:(nfeat+1)]
y=as.matrix(rtb3[,1])
xmat = model.matrix(paying_price~., data=rtb3)

lm_evmax = function(y, xmat){
  
  N = nrow(xmat) # # of Observations
  M = ncol(xmat) # M: Feature dimension

  lambda = 0.001*N
  I = diag(M)
  w = solve(lambda*I + t(xmat)%*%xmat) %*% t(xmat) %*% y
  e = y - xmat%*%w
  
  beta = as.vector(N/(t(e)%*%e))
  alpha = lambda*beta
  
  # Iteration Start
  cont = TRUE
  while(cont){
    A = alpha*I + beta*(t(xmat)%*%xmat)
    mn = beta*solve(A)%*%t(xmat)%*%y
    e = y - xmat%*%mn
    
    eg = eigen(beta*(t(xmat)%*%xmat))
    gamma = 0
    for(i in c(1:M)){
      gamma = gamma + (eg$values[i]/(eg$values[i]+alpha))
    }

    alpha_new = as.vector(gamma/(t(mn)%*%mn))
    beta_new = as.vector((N-gamma)/t(e)%*%(e))
    
    if(abs(alpha_new - alpha) + abs(beta_new - beta) < 0.00001){
      cont = FALSE
    }
    else{
      alpha = alpha_new
      beta = beta_new  
    }
  }
  mnsd = sqrt(diag(solve(A)))
  result = list(mN=mn, mNsd=mnsd, alpha=alpha, beta=beta)
  return(result)
}

lmev1 = lm_evmax(y, xmat)
lmev1

