



library(lme4)


# design space for this half (where the variance component on the intercept is 1) of study 2
nl1<-c(rep(2,1000),rep(5,1000),rep(10,1000))
nl1<-rep(nl1,12)
nl2<-c(rep(1000,3000),rep(5000,3000))
nl2<-rep(nl2,6)
movement<-c(rep(.05,6000),rep(.1,6000),rep(.15,6000),rep(.2,6000),rep(.4,6000),rep(.75,6000))
result<-matrix(0,nr=36000,nc=34)
nl3<-50

for(i in 1:nrow(result)){
  

ss1<-nl1[i] 
ss2<-nl2[i] 
ss<-ss1*ss2 
ss3<-50
movers<-movement[i]

# Nominal variables to denote nesting
# time in people
id1<-rep(1:ss2,each=ss1)
# people in cities
id2<-rep(1:ss3,each=ss/ss3)

coefs<-rep(.3,6)

simdat <- function(nobs = ss, xv =c(0,rep(.3,6))){
  X <-matrix(rnorm(nobs *(6)), ncol=(6))
  # level 1 dummy variable
  X[,2]<-(X[,2]>0)*1
  # level 2 continuous predictor
  X[,3]<-rep(X[seq(from=1,to=ss,by=ss1),3],each=ss1)
  # level 2 dummy predictor
  X[,4]<-rep(((X[seq(from=1,to=ss,by=ss1),4])>0)*1,each=ss1)
  # level 3 continuous predictor
  X[,5]<-rep(X[seq(from=1,to=ss,by=ss3),5],each=ss3)
  # level 3 dummy predictor
  X[,6]<-rep(((X[seq(from=1,to=ss,by=ss3),6])>0)*1,each=ss3)
  X<-cbind(1,X)
  xb<-X %*% xv
  y<-rnorm(nobs,xb)
  out<-as.matrix(cbind(y,X))
  return(out)}

X<-simdat(ss,c(0,coefs))

# pull out the dv
y<-X[,1]
# pull out the predictors
X<-X[,3:8]


vcs1000<-(1:(ss2))
vcs1000<-vcs1000-mean(vcs1000)
vcs1000<-vcs1000/1000
vcs1000<-vcs1000*3.464
vcs1000<-sample(vcs1000,length(vcs1000))
var(vcs1000)

vcs5000<-(1:(ss2))
vcs5000<-vcs5000-mean(vcs5000)
vcs5000<-vcs5000/500
vcs5000<-vcs5000*1.732
vcs5000<-sample(vcs5000,length(vcs5000))
var(vcs5000)


if(ss2==5000){var.y<-rep(vcs5000,each=ss/ss2)
y2<-y+var.y}
if(ss2==1000){var.y<-rep(vcs1000,each=ss/ss2)
y2<-y+var.y}

# Select the individuals (at level 2) that will move...
movers2<-sample(1:ss2,movers*ss2)


# select the time point (after #1) that they move
if(ss1>2){when<-sample(2:ss1,length(movers2),replace=TRUE)}
if(ss1==2){when<-rep(2,length(movers2))}

# the rows associated with the first mover
for(u in 1:length(movers2)){
movers3<-which(id1==movers2[u])
# the value of city that needs to change for the mover
cityofmover<-id2[movers3][when[u]:ss1]
# randomly select a new city
newcity<-sample(unique(id2)[-cityofmover],1)
id2[movers3[when[u]:ss1]]<-newcity}

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

results<-cbind(result,movement,nl1,nl2,1)
results2.1<-results

