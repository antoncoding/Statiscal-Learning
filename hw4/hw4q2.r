load(file="o_cost_train.rdata")
dm_train_t = as.numeric(ds4a_train[,1] == "pos")
tall=as.matrix(dm_train_t)
xmat = model.matrix(~f_past+g1+g2+g3+g4+g5+g6+g7+g8+g9+g10, data=ds4a_train[,-1])

sigmoid = function(x) {
  1 / (1 + exp(-x))
}

compute_w_sd = function(xmat, w, N, I, lambda){
  y_pred = sigmoid(xmat%*%w)
  R = build_R(N, y_pred, TRUE)
  SN = solve(lambda*I + t(xmat)%*%R%*%xmat)
  w_sd = sqrt(diag(SN))
  return(w_sd)
}

build_R = function(N, y_pred,p=FALSE){
  R = diag(N)
  for(i in c(1:N)){
    R[i,i]= y_pred[i]*(1-y_pred[i])
    if(i<10 && p){
      print(R[i,i])
    }
  }
  return(R)
}

build_gamma = function(lambda, lambdas, M){
  gamma = 0
  for(i in c(1:M)){
    gamma = gamma + (lambdas[i]/(lambdas[i]+lambda))
  }
  return(gamma)
}

w_convergence = function(w_old, w_new, param_tol){
  if(mean(abs(w_old-w_new))<param_tol){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}

lambda_convergence = function(lambda_old, lambda_new, param_tol){
  if(mean(abs(lambda_old-lambda_new))<param_tol){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}

logicreg_l2_train = function(y, xmat, lambda_rate=0.0005, param_tol=0.00001, granditertol=2, outitermax=50, inneritermax=20, debuglevel=0){
  N = nrow(xmat)
  M = ncol(xmat)
  I = diag(M)
  lambda = N*lambda_rate
  w = solve((t(xmat)%*%xmat)+lambda*I) %*% t(xmat) %*% y
  # Outer Loop: Newton's Law: Update W
  for(out in c(1:outitermax))
  { 
    break_cond_w = 10
    # inner for W
    for(inner in c(1:inneritermax)){
      y_pred = sigmoid(xmat%*%w)
      R = build_R(N, y_pred)
      H = t(xmat)%*%R%*%xmat + lambda*I
      E = lambda*w + t(xmat)%*%(y_pred-y)
      w_old = w
      w = w_old - solve(H)%*%E
      # w update counter
      break_cond_w = inner
      # check if converged
      if(w_convergence(w_old, w, param_tol)){
        #print('w break!')
        break 
      }
    }
    
    #inner for lambda
    for(inner in c(1:inneritermax)){
      y_pred = sigmoid(xmat%*%w)
      R = build_R(N, y_pred)
      lambdas = eigen(t(xmat)%*%R%*%xmat)$values
      gamma = build_gamma(lambda, lambdas, M)
      lambda_old = lambda
      lambda = as.numeric(gamma/(t(w)%*%w))
      if(lambda_convergence(lambda_old, lambda, param_tol)){
        #print('lambda break')
        break
      }
    }
    
    # Break Condition    
    if(break_cond_w<=granditertol){
      break
    }
  }
  lambda = as.numeric(lambda)
  w_sd = compute_w_sd(xmat, w, N, I, lambda)
  result = list(w=w, w_sd=w_sd, lambda=lambda, M=M, N=N)
  return(result)
}

model1 = logicreg_l2_train(tall, xmat, debuglevel=0)
model1$w_sd
xmattest1 = model.matrix(~f_past+g1+g2+g3+g4+g5+g6+g7+g8+g9+g10,data=ds4a_train[,-1])


logicreg_l2_predict = function(model1, xmat){
  y_pred = sigmoid(xmat%*%model1$w)
  y_class = c()
  for(y in y_pred){
    if(y>0.5){
      y_class = append(y_class, 1)
    }
    else{
      y_class = append(y_class, 0)
    }
  }
  return(list(prob=y_pred,class=y_class))
}

dm_train_t = as.numeric(ds4a_train[,1] == "pos")
tall=as.matrix(dm_train_t)
xmat2 = model.matrix(~., data=ds4a_train[,c(2, 103:204)])
model2 = logicreg_l2_train(tall, xmat2, debuglevel=0)
xmattest2 = model.matrix(~., data=ds4a_test[,c(2, 103:204)])
logicpred2 = logicreg_l2_predict(model2, xmattest2)
head(logicpred2$class,n=30)
