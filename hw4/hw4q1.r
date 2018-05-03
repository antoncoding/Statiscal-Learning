load('phonetrain.rdata')
load('phonetest1.rdata')
trainall = list()
for(aclass in outclass) {
  trainall[[aclass]] = traindata[[aclass]][1:500,]
}

# Functions
pgm_train = function(outclass, trainall){
  result = list()
  count = 1
  for(classdata in trainall){
    mu1 = colMeans(classdata)
    cov1 = cov(classdata)
    logdet = log(det(cov1))
    N1 = nrow(classdata)
    subresult = list(mu1=mu1, sigma1=cov1, prec1=solve(cov1), detsig_log=logdet, N1=N1)    
    result[[outclass[count]]] = subresult
    count = count + 1
  }
  return(result)
}

pgm_predict = function(amodel, testdata){
  if(ncol(testdata)!=3){
    return(NULL)
  }
  N = nrow(testdata)
  answerList = c()
  for(i in c(1:N)){
    xyz = testdata[i,]
    q = c()
    for(j in c(1:6)){
      data = amodel[[j]]
      d = 0  - (((2*pi)*(data$N1/2)) + det(data$sigma)/2 )
      logL = as.matrix(xyz-data$mu1)%*% as.matrix(data$prec1) %*% t(as.matrix(xyz-data$mu1))
      q_ = as.vector(logL*d)
      q = append(q, q_)
    }
    answer = which.max(q)
    answerList = append(answerList, answer)
  }
  return(answerList)
}

model1=pgm_train(outclass, traindata)
pred1=pgm_predict(model1, testds1_feature)
