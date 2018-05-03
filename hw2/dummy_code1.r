options(scipen=20)
#set to your working directory if needed
#setwd('K:\\Users\\hmlu\\svn_drive3\\teaching\\statistical learning\\2016\\slides\\ipinyou')
load(file='rtb1_train.rdata')

log_count = sort(log(table(rtb1_train$ip)), decreasing=TRUE)
head(exp(log_count), n=10)

log_rank = log(1:length(log_count))
plot(exp(log_rank), exp(as.vector(log_count)), type='l', lwd=2)
plot(log_rank, as.vector(log_count), type='l', lwd=2)


ipc = sort(table(rtb1_train$ip), decreasing=TRUE)
threshold = 5
ind1 = ipc>=threshold
ipc = ipc[ind1]
nfeat = length(ipc)
featname = names(ipc)


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


allt = rep(NA, nfeat)
names(allt)=featname
y = rtb1_train$paying_price
for(aid in 1:nfeat) {
    afeat = featname[aid]
    x = as.numeric(rtb1_train$ip == afeat)
    allt[aid] = reg_tvalue(y,x)
}

#sort by abs t-value decreasing
o1 = order(abs(allt), decreasing=TRUE)
allt2 = allt[o1]

#taking abs(t-value)>0.9
t_thres= 0.9
ind2 = abs(allt2)> t_thres
allt3 = allt2[ind2]
nfeat2 = length(allt3)
cat("Number of features from IP=", nfeat2, "\n")

rtb2 = data.frame(paying_price=rtb1_train$paying_price)
nextcol = ncol(rtb2)+1
allt3names= names(allt3)
origfeature = rtb1_train$ip
newfeatname = allt3names
for(afeature in 1:nfeat2) {
    thisfeat = allt3names[afeature]    
    rtb2[[nextcol]] = as.numeric(origfeature == thisfeat)
    aname = paste("ip", afeature, sep="_")
    names(rtb2)[nextcol]=aname
    newfeatname[afeature] = aname
    nextcol= nextcol+1
}

theseq = seq.int(1, length(newfeatname), 50)
if(theseq[length(theseq)] != length(newfeatname)) {
    theseq=c(theseq, length(newfeatname))
}

adjrmat = matrix(NA, ncol=2, nrow=length(theseq))
for(aid in seq(theseq)) {
    nf=theseq[aid]
    strfeat = paste(newfeatname[1:nf], collapse ="+")
    model1=as.formula(paste("paying_price~",strfeat, sep=""))
    lm2=lm(model1, data=rtb2)
    lm2s = summary(lm2)
    adjrmat[aid,] = c(nf, lm2s$adj.r)
    cat("Now: ", c(nf, lm2s$adj.r), "\n")
    flush.console()
}

plot(adjrmat[,1], adjrmat[,2], type='l', lwd=3)

#our final model
#nf=501
nf=max(theseq)
strfeat = paste(newfeatname[1:nf], collapse ="+")
model1=as.formula(paste("paying_price~",strfeat, sep=""))
lm2=lm(model1, data=rtb2)
lm2s = summary(lm2)
print(lm2s)

#compare allt3 vs regression
#allt3
reg_t=lm2s$coef[,3][-1]
simple_t = allt3

plot(simple_t, reg_t, pch=19)
abline(a=0, b=1, col='blue')
