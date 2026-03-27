

library(lme4)



# design space for this third (where the variance component on the intercept is .2) of study 1
nl1<-c(rep(200,1000),rep(600,1000),rep(2000,1000))
nl1<-rep(nl1,18)
nl2<-c(rep(20,3000),rep(35,3000),rep(50,3000))
nl2<-rep(nl2,6)
movement<-c(rep(.05,9000),rep(.1,9000),rep(.15,9000),rep(.2,9000),rep(.4,9000),rep(.75,9000))
result<-matrix(0,nr=54000,nc=34)

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
y<-X[,1]
# pull out the predictors
X<-X[,3:8]

# "vcs..." is what is systematically added to the outcome in order to introduce variance explained at level 2
# the line using "sample" shuffles this added variance so that it does not overlap with the third level
vcs200<-(1:(ss1))
vcs200<-vcs200-mean(vcs200)
vcs200<-vcs200/200
vcs200<-vcs200*1.548
vcs200<-sample(vcs200,length(vcs200))
var(vcs200)

vcs600<-(1:(ss1))
vcs600<-vcs600-mean(vcs600)
vcs600<-vcs600/600
vcs600<-vcs600*1.548
vcs600<-sample(vcs600,length(vcs600))
 var(vcs600) 

vcs2000<-(1:(ss1))
vcs2000<-vcs2000-mean(vcs2000)
vcs2000<-vcs2000/2000
vcs2000<-vcs2000*1.548
vcs2000<-sample(vcs2000,length(vcs2000))
var(vcs2000) 

# Depending on the sample size, adjust the outcome
if(ss1==200){var.y<-rep(vcs200,each=ss/ss1)
y2<-y+var.y}
if(ss1==600){var.y<-rep(vcs600,each=ss/ss1)
y2<-y+var.y}
if(ss1==2000){var.y<-rep(vcs2000,each=ss/ss1)
y2<-y+var.y}


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

m1<-lmer(y2 ~ X  +(1|id1) + (1|id2)   , REML=TRUE) #cross-classified (correct)
m2<-lmer(y2 ~ X +(1|id2/id1), REML=TRUE) #standard hierarchical (shortcut)


result[i,1:7]<-fixef(m1)
result[i,8:14]<-fixef(m2)
result[i,15:21]<-sqrt(diag(vcov(m1)))
result[i,22:28]<-sqrt(diag(vcov(m2)))
result[i,29:30]<-as.numeric(VarCorr(m1))
result[i,31:32]<-as.numeric(VarCorr(m2))
result[i,33]<-sigma(m1)^2
result[i,34]<-sigma(m2)^2}

results<-cbind(result,movement,nl1,nl2,0.2)
results1.2<-results

