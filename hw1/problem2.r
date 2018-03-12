mle_update<-function(mu, s, n, x){
  new_mu <- mu + (x-mu)/(n+1)
  new_s <- n/(n+1)*s + x%*%t(x)/(n+1) + n/(n+1)*(mu %*% t(mu)) - new_mu%*% t(new_mu)
  return_list = list(mu=new_mu, s=new_s, n=n+1)
  return(return_list)
}
set.seed(1223)
nobs = 3
nfeature = 7
rawdata=matrix(runif(nobs*nfeature), nrow=nobs, ncol=nfeature)
data1 = rawdata[1:(nobs-1),]
xn = rawdata[nobs,]
cov1 = cov(data1)*(nrow(data1)-1) / nrow(data1)
mu1 = colMeans(data1)
out1 = mle_update(mu1, cov1, nrow(data1), xn)
print(out1$mu[1:3])
print(out1$s[1:3,1:3])
print(out1$n)
