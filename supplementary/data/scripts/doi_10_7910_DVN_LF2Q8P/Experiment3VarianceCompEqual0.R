

library(lme4)


# design space for this third (where the variance component on the intercept is .2) of study 1
nl1<-c(rep(200,1000),rep(600,1000))
nl1<-rep(nl1,12)
nl2<-c(rep(20,2000),rep(50,2000))
nl2<-rep(nl2,6)
movement<-c(rep(.05,4000),rep(.1,4000),rep(.15,4000),rep(.2,4000),rep(.4,4000),rep(.75,4000))
result<-matrix(0,nr=24000,nc=34)

for(i in 1:nrow(result)){

ss1<-nl1[i]
ss2<-nl2[i]
ss<-ss1*ss2
movers<-movement[i]

# people in primary schools
id1<-rep(1:ss1,each=ss2)
# primary schools in secondary schools
id2<-rep(1:ss2,each=ss1)


simdat <- function(nobs = ss, xv =c(0,rep(.3,6))){
  X <-matrix(rnorm(nobs *(6)), ncol=(6))
  # level 1 dummy variable
  X[,2]<-(X[,2]>0)*1
  # level 2 continuous predictor
  X[,3]<-rep(aggregate(X[,3],by=list(id1),FUN=mean)[,2],each=max(id2))
  # level 3 continuous predictor
  X[,5]<-rep(aggregate(X[,5],by=list(id2),FUN=mean)[,2],each=max(id1))
  # level 2 dummy variable
  X[,4]<-rep((X[seq(from=1,to=nrow(X),by=max(id2)),4]>0)*1,each=max(id2))
  # level 3 dummy variable
  X[,6]<-rep((X[seq(from=1,to=nrow(X),by=max(id1)),6]>0)*1,each=max(id1))
  X<-cbind(1,X)
  xb<-X %*% xv
  y<-rnorm(nobs,xb)
  out<-as.matrix(cbind(y,X))
  return(out)}


coefs<-c(0,rep(.3,6))
X<-simdat(ss,coefs)

# pull out the dv
y<-(X[,1]>0)*1
# pull out the predictors
X<-X[,3:8]

# Select the cases that will move...
movers2<-sample(1:nrow(X),movers*nrow(X))
# list of unique ids at the highest level
uniqid2<-unique(id2)
for(q in 1:length(movers2)){
# id2 for the q^th mover
idmover<-id2[movers2[q]]
# select a new id2  from the list of other values of id2
newid<-sample(uniqid2[-idmover],1)
# replace the old value of id2 with the new value
id2[movers2[q]]<-newid}


m1<-glmer(y ~ scale(X)  +(1|id1) + (1|id2)   ,family=binomial) #cross-classified (correct)
m2<-glmer(y ~ scale(X) +(1|id2/id1), family=binomial) #standard hierarchical (shortcut)


result[i,1:7]<-fixef(m1)
result[i,8:14]<-fixef(m2)
result[i,15:21]<-sqrt(diag(vcov(m1)))
result[i,22:28]<-sqrt(diag(vcov(m2)))
result[i,29:30]<-as.numeric(VarCorr(m1))
result[i,31:32]<-as.numeric(VarCorr(m2))
result[i,33]<-sigma(m1)^2
result[i,34]<-sigma(m2)^2}

results<-cbind(result,movement,nl1,nl2,0)
results3.0<-results




