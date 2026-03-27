library(coda)
library(gridExtra)
library(grid)
library(gtable)
library(ggplot2)
library(Hmisc)
library(mlogit)
library(MASS)
options(warn=-1)


############################### TABLES AND FIGURES IN THE MAIN TEXT ############################################################################

############################################ Table 2 ##########################################################################################
load("dataset_2012.RData")

df<-subset(dataset, select=c("country.name", "ActMeetMun",
"country.name", "ActMeetMun", "ActContMun",  "ActContAut" ,                           
"ActMeetImp" , "ActSolveProb" ,  "ActMeetPty" ,  "ActContGvt",                            
"ActProtest" , "ActBlock" ,  "ActVoted",  "ActSign" ,                              
"ActShare" , "education" ,  "female", "age", "relative.incomeproxy" ,
"victim", "corrupt", "spatial.distance" ,
"compulsory.voting", "gdp.pc", "rule.of.lw", "enp", "social.spending"))                       
df<-na.exclude(df)


Y <- cbind(df$ActMeetMun, df$ActContMun, df$ActContAut,
		 df$ActContGvt, df$ActSolveProb, 
		df$ActMeetImp, df$ActMeetPty, df$ActVoted, 
		df$ActShare, df$ActSign, df$ActProtest, df$ActBlock)

Country<-as.numeric(factor(df$country.name), levels=unique(df$country.name))
		

Matrix.Country<-matrix(0,nrow(Y),length(unique(Country)))           
for (i in 1:length(unique(Country))) Matrix.Country[Country==i,i]<-1


df$age.centered<-(df$age-mean(df$age))/(2*sd(df$age))
df$education.centered<-(df$education-mean(df$education))/(2*sd(df$education))


df$relative.incomeproxy.centered<-(df$relative.incomeproxy-mean(df$relative.incomeproxy))/(2*sd(df$relative.incomeproxy))


df$spatial.distance.centered<-(df$spatial.distance-mean(df$spatial.distance))/(2*sd(df$spatial.distance))

X<-cbind(1, df$age.centered, df$female, df$education.centered, 
	df$relative.incomeproxy.centered, 
	df$victim, df$corrupt, 
	df$spatial.distance.centered,
	(df$rule.of.lw-mean(df$rule.of.lw))/(2*sd(df$rule.of.lw)),
		df$compulsory.voting,
		(df$enp-mean(df$enp))/(2*sd(df$enp)),
		(df$gdp.pc-mean(df$gdp.pc))/(2*sd(df$gdp.pc)),
		(df$social.spending-mean(df$social.spending))/(2*sd(df$social.spending))
)


load("Results_MainModel")

# Convergence checks for Main Model
Alpha<-mcmc.list(as.mcmc(t(Results$alpha[,,1])), 
		as.mcmc(t(Results$alpha[,,2])),
		as.mcmc(t(Results$alpha[,,3])))
print(paste("Convergence Alpha:", gelman.diag(Alpha)[[2]]<1.2, sep=" "))


Alpha.Conv<-mcmc.list(as.mcmc(t(Results$alpha_conv[-seq(12,dim(Results$alpha_conv)[1],by=12),,1])), 
		as.mcmc(t(Results$alpha_conv[-seq(12,dim(Results$alpha_conv)[1],by=12),,2])),
		as.mcmc(t(Results$alpha_conv[-seq(12,dim(Results$alpha_conv)[1],by=12),,3])))
print(paste("Convergence Alpha Conv:", gelman.diag(Alpha.Conv)[[2]]<1.2, sep=" "))


Alpha.Unconv<-mcmc.list(as.mcmc(t(Results$alpha_unconv[-seq(1,dim(Results$alpha_unconv)[1],by=12),,1])), 
		as.mcmc(t(Results$alpha_unconv[-seq(1,dim(Results$alpha_unconv)[1],by=12),,2])),
		as.mcmc(t(Results$alpha_unconv[-seq(1,dim(Results$alpha_unconv)[1],by=12),,3])))
print(paste("Convergence Alpha Unconventional:", gelman.diag(Alpha.Unconv)[[2]]<1.2, sep=" "))



P.Conv<-mcmc.list(as.mcmc((Results$P_conv[2,,1])), 
		as.mcmc((Results$P_conv[2,,2])),
		as.mcmc((Results$P_conv[2,,3])))
print(paste("Convergence Prob. Conventional:", gelman.diag(P.Conv)[[1]][,1]<1.2, sep=" "))


P.Unconv<-mcmc.list(as.mcmc((Results$P_unconv[2,,1])), 
		as.mcmc((Results$P_unconv[2,,2])),
		as.mcmc((Results$P_unconv[2,,3])))
print(paste("Convergence Prob. Unconventional", gelman.diag(P.Unconv)[[1]][,1]<1.2, sep=" "))


Beta2<-mcmc.list(as.mcmc(t(Results$beta2[,,1])),
	as.mcmc(t(Results$beta2[,,2])),
	as.mcmc(t(Results$beta2[,,3])))
Beta3<-mcmc.list(as.mcmc(t(Results$beta3[,,1])),
	as.mcmc(t(Results$beta3[,,2])),
	as.mcmc(t(Results$beta3[,,3])))
Beta4<-mcmc.list(as.mcmc(t(Results$beta4[,,1])),
	as.mcmc(t(Results$beta4[,,2])),
	as.mcmc(t(Results$beta4[,,3])))
print(paste("Convergence Regression coefficients:", gelman.diag(Beta2)[[2]]<1.2 & gelman.diag(Beta3)[[2]]<1.2 & gelman.diag(Beta4)[[2]]<1.2, sep=" "))



# First we process the data needed to produce Table 2 and the other results reported in the research note and Section A.3.1 of the Online Appendix
ALPHA<-rbind(t(Results$alpha[,,1]), 
		t(Results$alpha[,,2]),
		t(Results$alpha[,,3]))


Alpha.means<-matrix(0, 12,4)
rownames(Alpha.means)<-NULL
colnames(Alpha.means)<-c("Activity", "Mean", "95.Low", "95.High")
Alpha.means[,1]<-c("Municipal meeting", "Contacting municipality", "Contacting local authority", "Contacting national authority",
                    		"Solving Problems", "Improvements meeting", "Party meeting", "Voting",
                    		"Sharing online", "Petitioning", "Protesting", "Blocking")


for (j in 1:12) {

Alpha.means[j,2:4]<-c(mean(c(ALPHA[, seq(j, ncol(ALPHA), by=12)])), quantile(c(ALPHA[, seq(j, ncol(ALPHA), by=12)]), c(0.025,0.975)))

}

save(Alpha.means, file="Alpha.means.MainModel")

Alpha.all<-cbind(rep(rownames(table(df$country.name)), each=12),  		
		rep(Alpha.means[,1], length(unique(df$country.name))), 
		colMeans(ALPHA), t(apply(ALPHA, 2, quantile, c(0.025, 0.975))))

colnames(Alpha.all)<-c("Country", "Activity",  "Mean", "95.Low", "95.High")
rownames(Alpha.all)<-NULL
save(Alpha.all, file="Alpha.all.MainModel")


ALPHA.CONV<-rbind(t(Results$alpha_conv[,,1]), 
		t(Results$alpha_conv[,,2]),
		t(Results$alpha_conv[,,3]))



Alpha.conv.means<-matrix(0, 12,4)
rownames(Alpha.conv.means)<-NULL
colnames(Alpha.conv.means)<-NULL

Alpha.conv.means[,1]<-c("Municipal meeting", "Contacting municipality", "Contacting local authority", "Contacting national authority",
                    		"Solving Problems", "Improvements meeting", "Party meeting", "Voting",
                    		"Sharing online", "Petitioning", "Protesting", "Blocking")
Alpha.conv.means[,2]<-"Conventional"

for (j in 1:12) {

m<-round(mean(c(ALPHA.CONV[, seq(j, ncol(ALPHA.CONV), by=12)])), digits=3)
q.l<-round(quantile(c(ALPHA.CONV[, seq(j, ncol(ALPHA.CONV), by=12)]), c(0.025,0.975))[1], digits=3)
q.h<-round(quantile(c(ALPHA.CONV[, seq(j, ncol(ALPHA.CONV), by=12)]), c(0.025,0.975))[2], digits=3)



if (nchar(m)<5 & grepl("\\.", m)==FALSE) {
m<-paste(m,".", sep="")
while (nchar(m)<5) {
m<-paste(m, "0", sep="")
} 
} else if (nchar(m)<5 & grepl("\\.", m)==TRUE) {
while (nchar(m)<5) {
m<-paste(m, "0", sep="")
} 
}


if (nchar(q.l)<5 & grepl("\\.", q.l)==FALSE) {
q.l<-paste(q.l,".", sep="")
while (nchar(q.l)<5) {
q.l<-paste(q.l, "0", sep="")
} 
} else if (nchar(q.l)<5 & grepl("\\.", q.l)==TRUE) {
while (nchar(q.l)<5) {
q.l<-paste(q.l, "0", sep="")
} 
}

if (nchar(q.h)<5 & grepl("\\.", q.h)==FALSE) {
q.h<-paste(q.h,".", sep="")
while (nchar(q.h)<5) {
q.h<-paste(q.h, "0", sep="")
} 
} else if (nchar(q.h)<5 & grepl("\\.", q.h)==TRUE) {
while (nchar(q.h)<5) {
q.h<-paste(q.h, "0", sep="")
} 
}




Alpha.conv.means[j,3:4]<-c(m, paste(paste("(", q.l, ",", sep=""), paste(q.h,")", sep=""),sep=""))


}


	


Alpha.conv.all<-cbind(rep(rownames(table(df$country.name)), each=12),  		
		rep(Alpha.conv.means[,1], length(unique(df$country.name))), "Conventional",
		colMeans(ALPHA.CONV), t(apply(ALPHA.CONV, 2, quantile, c(0.025, 0.975))))

colnames(Alpha.conv.all)<-c("Country", "Activity", "Dimension", "Mean", "95.Low", "95.High")
rownames(Alpha.conv.all)<-NULL


ALPHA.UNCONV<-rbind(t(Results$alpha_unconv[,,1]), 
		t(Results$alpha_unconv[,,2]),
		t(Results$alpha_unconv[,,3]))



Alpha.unconv.means<-matrix(0, 12,4)
Alpha.unconv.means[,1]<-c("Municipal meeting", "Contacting municipality", "Contacting local authority", "Contacting national authority",
                    		"Solving Problems", "Improvements meeting", "Party meeting", "Voting",
                    		"Sharing online", "Petitioning", "Protesting", "Blocking")
Alpha.unconv.means[,2]<-"Unconventional"

for (j in 1:12) {

m<-round(mean(c(ALPHA.UNCONV[, seq(j, ncol(ALPHA.UNCONV), by=12)])), digits=3)
q.l<-round(quantile(c(ALPHA.UNCONV[, seq(j, ncol(ALPHA.UNCONV), by=12)]), c(0.025,0.975))[1], digits=3)
q.h<-round(quantile(c(ALPHA.UNCONV[, seq(j, ncol(ALPHA.UNCONV), by=12)]), c(0.025,0.975))[2], digits=3)


if (nchar(m)<5 & grepl("\\.", m)==FALSE) {
m<-paste(m,".", sep="")
while (nchar(m)<5) {
m<-paste(m, "0", sep="")
} 
} else if (nchar(m)<5 & grepl("\\.", m)==TRUE) {
while (nchar(m)<5) {
m<-paste(m, "0", sep="")
} 
}


if (nchar(q.l)<5 & grepl("\\.", q.l)==FALSE) {
q.l<-paste(q.l,".", sep="")
while (nchar(q.l)<5) {
q.l<-paste(q.l, "0", sep="")
} 
} else if (nchar(q.l)<5 & grepl("\\.", q.l)==TRUE) {
while (nchar(q.l)<5) {
q.l<-paste(q.l, "0", sep="")
} 
}

if (nchar(q.h)<5 & grepl("\\.", q.h)==FALSE) {
q.h<-paste(q.h,".", sep="")
while (nchar(q.h)<5) {
q.h<-paste(q.h, "0", sep="")
} 
} else if (nchar(q.h)<5 & grepl("\\.", q.h)==TRUE) {
while (nchar(q.h)<5) {
q.h<-paste(q.h, "0", sep="")
} 
}

Alpha.unconv.means[j,3:4]<-c(m, paste(paste("(", q.l, ",", sep=""), paste(q.h,")", sep=""),sep=""))


}



Alpha.unconv.all<-cbind(rep(rownames(table(df$country.name)), each=12),  		
		rep(Alpha.unconv.means[,1], length(unique(df$country.name))), "Unconventional",
		colMeans(ALPHA.UNCONV), 
		t(apply(ALPHA.UNCONV, 2, quantile, c(0.025, 0.975))))
colnames(Alpha.unconv.all)<-c("Country", "Activity", "Dimension", "Mean", "95.Low", "95.High")
rownames(Alpha.unconv.all)<-NULL

Country.Loadings<-data.frame(rbind(Alpha.conv.all, Alpha.unconv.all))
save(Country.Loadings, file="Country.Loadings.MainModel")

BETA2<-rbind((t(Results$beta2[,,1])),
	(t(Results$beta2[,,2])),
	(t(Results$beta2[,,3])))


BETA3<-rbind((t(Results$beta3[,,1])),
	(t(Results$beta3[,,2])),
	(t(Results$beta3[,,3])))


BETA4<-rbind((t(Results$beta4[,,1])),
	(t(Results$beta4[,,2])),
	(t(Results$beta4[,,3])))


NU<-rbind((t(Results$nu[,,1])),
	(t(Results$nu[,,2])),
	(t(Results$nu[,,3])))


NU2<-NU[, 1:length(unique(Country))]
NU3<-NU[, (1+length(unique(Country))): (2*length(unique(Country)))]
NU4<-NU[, (1+2*length(unique(Country))): (3*length(unique(Country)))]



Beta.Agitator<-BETA2[seq(1, nrow(BETA2), length.out=1000),]
Beta.Conventional<-BETA3[seq(1, nrow(BETA3), length.out=1000),]
Beta.Activist<-BETA4[seq(1, nrow(BETA4), length.out=1000),]
Nu.Agitator<-NU2[seq(1, nrow(NU2), length.out=1000),]
Nu.Conventional<-NU3[seq(1, nrow(NU3), length.out=1000),]
Nu.Activist<-NU4[seq(1, nrow(NU4), length.out=1000),]


prob.Agitator<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X%*%Beta.Agitator[s,]+Nu.Agitator[s,Country])/(1+exp(X%*%Beta.Agitator[s,]+Nu.Agitator[s,Country])+
			exp(X%*%Beta.Conventional[s,]+Nu.Conventional[s,Country])+exp(X%*%Beta.Activist[s,]+Nu.Activist[s,Country])))

prob.Conventional<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X%*%Beta.Conventional[s,]+Nu.Conventional[s,Country])/(1+exp(X%*%Beta.Agitator[s,]+Nu.Agitator[s,Country])+
			exp(X%*%Beta.Conventional[s,]+Nu.Conventional[s,Country])+exp(X%*%Beta.Activist[s,]+Nu.Activist[s,Country])))

prob.Activist<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X%*%Beta.Activist[s,]+Nu.Activist[s,Country])/(1+exp(X%*%Beta.Agitator[s,]+Nu.Agitator[s,Country])+
			exp(X%*%Beta.Conventional[s,]+Nu.Conventional[s,Country])+exp(X%*%Beta.Activist[s,]+Nu.Activist[s,Country])))
prob.Outsider<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X%*%Beta.Agitator[s,]+Nu.Agitator[s,Country])+
			exp(X%*%Beta.Conventional[s,]+Nu.Conventional[s,Country])+exp(X%*%Beta.Activist[s,]+Nu.Activist[s,Country])))


Posterior.Probs.MainModel<-cbind(colMeans(prob.Outsider), colMeans(prob.Agitator), colMeans(prob.Conventional), colMeans(prob.Activist))
save(Posterior.Probs.MainModel, file="Posterior.Probs.MainModel")


Outsider.by.Country<-c()
Activist.by.Country<-c()
Conventional.by.Country<-c()
Agitator.by.Country<-c()

for (j in 1:length(unique(Country))) {

Outsider.by.Country<-rbind(Outsider.by.Country,
c(unique(Country)[j],mean(colMeans(prob.Outsider[Country==j,])), quantile(colMeans(prob.Outsider[Country==j,]), c(0.025,0.975)))
)


Activist.by.Country<-rbind(Activist.by.Country,
c(unique(Country)[j],mean(colMeans(prob.Activist[Country==j,])), quantile(colMeans(prob.Activist[Country==j,]), c(0.025,0.975)))
)


Conventional.by.Country<-rbind(Conventional.by.Country,
c(unique(Country)[j],mean(colMeans(prob.Conventional[Country==j,])), quantile(colMeans(prob.Conventional[Country==j,]), c(0.025,0.975)))
)


Agitator.by.Country<-rbind(Agitator.by.Country,
c(unique(Country)[j],mean(colMeans(prob.Agitator[Country==j,])), quantile(colMeans(prob.Agitator[Country==j,]), c(0.025,0.975)))
)

}


save(Outsider.by.Country, file="Outsider.by.Country.MainModel")
save(Agitator.by.Country, file="Agitator.by.Country.MainModel")
save(Activist.by.Country, file="Activist.by.Country.MainModel")
save(Conventional.by.Country, file="Conventional.by.Country.MainModel")



ALPHA<-rbind(t(Results$alpha[,,1]), 
		t(Results$alpha[,,2]),
		t(Results$alpha[,,3]))


ALPHA.CONV<-rbind(t(Results$alpha_conv[,,1]), 
		t(Results$alpha_conv[,,2]),
		t(Results$alpha_conv[,,3]))


ALPHA.UNCONV<-rbind(t(Results$alpha_unconv[,,1]), 
		t(Results$alpha_unconv[,,2]),
		t(Results$alpha_unconv[,,3]))

Alpha<-ALPHA[seq(1, nrow(BETA2), length.out=1000),]
Alpha.Conv<-ALPHA.CONV[seq(1, nrow(BETA2), length.out=1000),]
Alpha.Unconv<-ALPHA.UNCONV[seq(1, nrow(BETA2), length.out=1000),]


P.CONV<-rbind(t(Results$P_conv[,,1]), 
		t(Results$P_conv[,,2]),
		t(Results$P_conv[,,3]))
P.Conv<-P.CONV[seq(1, nrow(BETA2), length.out=1000),]


P.UNCONV<-rbind(t(Results$P_unconv[,,1]), 
		t(Results$P_unconv[,,2]),
		t(Results$P_unconv[,,3]))

P.Unconv<-P.UNCONV[seq(1, nrow(BETA2), length.out=1000),]




C_unconv=rbinom(nrow(Y), 1, .5)+1
C.Conv<-matrix(0,nrow(Y), nrow(Alpha))
C.Unconv<-matrix(0,nrow(Y), nrow(Alpha))


for (s in 1:nrow(Alpha)){ 

pnum<-matrix(0,nrow(Y),2)

for (k in 1:2) {
lindividual<-matrix(0,nrow(Y),ncol(Y))
for (l in 1:ncol(Y)) {
mutmp=pnorm(Matrix.Country%*%t(matrix(Alpha[s,],ncol(Y), ncol(Matrix.Country)))[,l]+
(k-1)*((Matrix.Country%*%t(matrix(Alpha.Conv[s,],ncol(Y), ncol(Matrix.Country))))[,l])+
(C_unconv-1)*((Matrix.Country%*%t(matrix(Alpha.Unconv[s,],ncol(Y), ncol(Matrix.Country))))[,l]))
lindividual[,l]<-(mutmp^Y[,l])*((1-mutmp)^(1-Y[,l]))
}

pnum[,k]<-apply(lindividual,1,prod)*P.Conv[s,k]
}

p<-pnum/(apply(pnum,1,sum))
p[is.na(p)==1]<-rep(1/2,2)
C_conv<-rMultinom(p,1)

C.Conv[,s]<-C_conv




for (k in 1:2) {
lindividual<-matrix(0,nrow(Y),ncol(Y))
for (l in 1:ncol(Y)) {
mutmp=pnorm(Matrix.Country%*%t(matrix(Alpha[s,],ncol(Y), ncol(Matrix.Country)))[,l]+
(C_conv-1)*((Matrix.Country%*%t(matrix(Alpha.Conv[s,],ncol(Y), ncol(Matrix.Country))))[,l])+
(k-1)*((Matrix.Country%*%t(matrix(Alpha.Unconv[s,],ncol(Y), ncol(Matrix.Country))))[,l]))
lindividual[,l]<-(mutmp^Y[,l])*((1-mutmp)^(1-Y[,l]))
}

pnum[,k]<-apply(lindividual,1,prod)*P.Unconv[s,k]
}

p<-pnum/(apply(pnum,1,sum))
p[is.na(p)==1]<-rep(1/2,2)
C_unconv<-rMultinom(p,1)

C.Unconv[,s]<-C_unconv


}



CC<-rowMeans(C.Conv-1)
CU<-rowMeans(C.Unconv-1)

Probs.Dimensions.MainModel<-cbind(CC, CU)
save(Probs.Dimensions.MainModel, file="Probs.Dimensions.MainModel")


Posterior.Conv<-c()

for (j in 1:nrow(C.Conv)) {

table<-table(as.matrix(C.Conv[j,]))

if (dim(table)==2) {
Posterior.Conv<-rbind(Posterior.Conv,
c(which(max(table(as.matrix(C.Conv[j,]))/sum(table(as.matrix(C.Conv[j,]))))==table(as.matrix(C.Conv[j,]))/sum(table(as.matrix(C.Conv[j,]))))[1],
table(as.matrix(C.Conv[j,]))/sum(table(as.matrix(C.Conv[j,]))))
)
} else if (dim(table)==1 & names(table)==1) {

Posterior.Conv<-rbind(Posterior.Conv,
			c(1,1,0))
} else if (dim(table)==1 & names(table)==2) {
Posterior.Conv<-rbind(Posterior.Conv,
			c(2,0,1))
}
}



Posterior.Conv.Country<-cbind(Country, Posterior.Conv)
save(Posterior.Conv.Country, file="Posterior.Conv.Country.MainModel")


Posterior.Classification.Conv.Low<-cbind(matrix(colMeans(Posterior.Conv[Posterior.Conv[,1]==1,-1])),
t(apply(Posterior.Conv[Posterior.Conv[,1]==1,-1], 2, quantile, c(0.025, 0.975))))


Posterior.Classification.Conv.High<-cbind(matrix(colMeans(Posterior.Conv[Posterior.Conv[,1]==2,-1])),
t(apply(Posterior.Conv[Posterior.Conv[,1]==2,-1], 2, quantile, c(0.025, 0.975))))

save(Posterior.Classification.Conv.High, file="Posterior.Classification.Conv.High.MainModel")
save(Posterior.Classification.Conv.Low, file="Posterior.Classification.Conv.Low.MainModel")



Posterior.Unconv<-c()

for (j in 1:nrow(C.Unconv)) {

table<-table(as.matrix(C.Unconv[j,]))

if (dim(table)==2) {
Posterior.Unconv<-rbind(Posterior.Unconv,
c(which(max(table(as.matrix(C.Unconv[j,]))/sum(table(as.matrix(C.Unconv[j,]))))==table(as.matrix(C.Unconv[j,]))/sum(table(as.matrix(C.Unconv[j,]))))[1],
table(as.matrix(C.Unconv[j,]))/sum(table(as.matrix(C.Unconv[j,]))))
)
} else if (dim(table)==1 & names(table)==1) {

Posterior.Unconv<-rbind(Posterior.Unconv,
			c(1,1,0))
} else if (dim(table)==1 & names(table)==2) {
Posterior.Unconv<-rbind(Posterior.Unconv,
			c(2,0,1))
}
}


Posterior.Unconv.Country<-cbind(Country, Posterior.Unconv)
save(Posterior.Unconv.Country, file="Posterior.Unconv.Country.MainModel")



Posterior.Classification.Unconv.Low<-cbind(matrix(colMeans(Posterior.Unconv[Posterior.Unconv[,1]==1,-1])),
t(apply(Posterior.Unconv[Posterior.Unconv[,1]==1,-1], 2, quantile, c(0.025, 0.975))))


Posterior.Classification.Unconv.High<-cbind(matrix(colMeans(Posterior.Unconv[Posterior.Unconv[,1]==2,-1])),
t(apply(Posterior.Unconv[Posterior.Unconv[,1]==2,-1], 2, quantile, c(0.025, 0.975))))

save(Posterior.Classification.Unconv.High, file="Posterior.Classification.Unconv.High.MainModel")
save(Posterior.Classification.Unconv.Low, file="Posterior.Classification.Unconv.Low.MainModel")



Assignment.Unconv<-c()
Assignment.Conv<-c()

for (j in 1:nrow(C.Conv)) {

Assignment.Conv<-rbind(Assignment.Conv, c(Country[j], j, which(table(C.Conv[j,])==max(table(C.Conv[j,])))[1]))
Assignment.Unconv<-rbind(Assignment.Unconv, c(Country[j], j, which(table(C.Unconv[j,])==max(table(C.Unconv[j,])))[1]))
}


save(Assignment.Conv, file="Assignment.Conv.MainModel")
save(Assignment.Unconv, file="Assignment.Unconv.MainModel")


# Covariate Marginal Effects
colnames(X)<-c("Intercept", "Age", "Female", "Education", 
		 "Income", "Victimization", "Corruption",
		  "Ideology", "Rule of Law", "Compulsory Voting", "ENPP",
		"GDP", "Social.Spending")

dummies<-c(3, 6)
continuous<-c(2,4,5,8)
ordinal<-7


Marginal.Individual<-matrix(0, nrow=4*(length(dummies)+length(continuous)+length(ordinal)), 3)
colnames(Marginal.Individual)<-c("Mean", "95.Low", "95.High")
rownames(Marginal.Individual)<-paste(rep(colnames(X)[sort(c(dummies, continuous, ordinal))], each=4),
rep(c("Outsider", "Agitator", "Conventional", "Activist"), (length(dummies)+length(continuous)+length(ordinal))), 
sep="-")

for (j in 1:length(dummies)) {
X.0<-X
X.1<-X

X.0[,dummies[j]]<-0
X.1[,dummies[j]]<-1


prob.Agitator.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Outsider.0<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Agitator.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))


prob.Outsider.1<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))


Marginal.Individual[grep(colnames(X)[dummies[j]], rownames(Marginal.Individual))[1],]<-c(mean(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0)),
											quantile(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0), c(0.025,0.975)))

Marginal.Individual[grep(colnames(X)[dummies[j]], rownames(Marginal.Individual))[2],]<-c(mean(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0)), quantile(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0), c(0.025,0.975)))


Marginal.Individual[grep(colnames(X)[dummies[j]], rownames(Marginal.Individual))[3],]<-c(mean(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0)), quantile(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0), c(0.025,0.975)))


Marginal.Individual[grep(colnames(X)[dummies[j]], rownames(Marginal.Individual))[4],]<-c(mean(colMeans(prob.Activist.1)-colMeans(prob.Activist.0)), quantile(colMeans(prob.Activist.1)-colMeans(prob.Activist.0), c(0.025,0.975)))

}


for (j in 1:length(continuous)){
X.0<-X
X.1<-X
X.1[,continuous[j]]<-X.0[,continuous[j]]+sd(X.0[,continuous[j]])


prob.Agitator.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Outsider.0<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Agitator.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))


prob.Outsider.1<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))



Marginal.Individual[grep(colnames(X)[continuous[j]], rownames(Marginal.Individual))[1],]<-c(mean(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0)),
											quantile(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0), c(0.025,0.975)))

Marginal.Individual[grep(colnames(X)[continuous[j]], rownames(Marginal.Individual))[2],]<-c(mean(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0)), quantile(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0), c(0.025,0.975)))


Marginal.Individual[grep(colnames(X)[continuous[j]], rownames(Marginal.Individual))[3],]<-c(mean(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0)), quantile(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0), c(0.025,0.975)))


Marginal.Individual[grep(colnames(X)[continuous[j]], rownames(Marginal.Individual))[4],]<-c(mean(colMeans(prob.Activist.1)-colMeans(prob.Activist.0)), quantile(colMeans(prob.Activist.1)-colMeans(prob.Activist.0), c(0.025,0.975)))

}

for (j in 1:length(ordinal)){
X.0<-X
X.1<-X
X.1[,ordinal[j]]<-X.0[,ordinal[j]]+1


prob.Agitator.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Outsider.0<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Agitator.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))


prob.Outsider.1<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))



Marginal.Individual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Individual))[1],]<-c(mean(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0)),
											quantile(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0), c(0.025,0.975)))

Marginal.Individual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Individual))[2],]<-c(mean(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0)), quantile(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0), c(0.025,0.975)))


Marginal.Individual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Individual))[3],]<-c(mean(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0)), quantile(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0), c(0.025,0.975)))


Marginal.Individual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Individual))[4],]<-c(mean(colMeans(prob.Activist.1)-colMeans(prob.Activist.0)), quantile(colMeans(prob.Activist.1)-colMeans(prob.Activist.0), c(0.025,0.975)))

}


Marginal.Individual[,1:3]<-100*Marginal.Individual[,1:3]
Marginal.Individual[grep("Ideology", rownames(Marginal.Individual)),1:3]<-(Marginal.Individual[grep("Ideology", rownames(Marginal.Individual)),1:3]/sd(X[,grep("Ideology", colnames(X))]))


continuous<-c(9, 11:13)
ordinal<-10


Marginal.Contextual<-matrix(0, nrow=4*(length(continuous)+length(ordinal)), 3)
colnames(Marginal.Contextual)<-c("Mean", "95.Low", "95.High")
rownames(Marginal.Contextual)<-paste(rep(colnames(X)[sort(c(continuous, ordinal))], each=4),
rep(c("Outsider", "Agitator", "Conventional", "Activist"), (length(continuous)+length(ordinal))), 
sep="-")



for (j in 1:length(continuous)){
X.0<-X
X.1<-X
X.1[,continuous[j]]<-X.0[,continuous[j]]+sd(X.0[,continuous[j]])


prob.Agitator.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Outsider.0<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Agitator.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))


prob.Outsider.1<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))



Marginal.Contextual[grep(colnames(X)[continuous[j]], rownames(Marginal.Contextual))[1],]<-c(mean(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0)),
											quantile(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0), c(0.025,0.975)))

Marginal.Contextual[grep(colnames(X)[continuous[j]], rownames(Marginal.Contextual))[2],]<-c(mean(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0)), quantile(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0), c(0.025,0.975)))


Marginal.Contextual[grep(colnames(X)[continuous[j]], rownames(Marginal.Contextual))[3],]<-c(mean(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0)), quantile(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0), c(0.025,0.975)))


Marginal.Contextual[grep(colnames(X)[continuous[j]], rownames(Marginal.Contextual))[4],]<-c(mean(colMeans(prob.Activist.1)-colMeans(prob.Activist.0)), quantile(colMeans(prob.Activist.1)-colMeans(prob.Activist.0), c(0.025,0.975)))

}

for (j in 1:length(ordinal)){
X.0<-X
X.1<-X
X.1[,ordinal[j]]<-X.0[,ordinal[j]]+1


prob.Agitator.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Outsider.0<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Agitator.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))


prob.Outsider.1<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))



Marginal.Contextual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Contextual))[1],]<-c(mean(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0)),
											quantile(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0), c(0.025,0.975)))

Marginal.Contextual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Contextual))[2],]<-c(mean(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0)), quantile(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0), c(0.025,0.975)))


Marginal.Contextual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Contextual))[3],]<-c(mean(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0)), quantile(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0), c(0.025,0.975)))


Marginal.Contextual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Contextual))[4],]<-c(mean(colMeans(prob.Activist.1)-colMeans(prob.Activist.0)), quantile(colMeans(prob.Activist.1)-colMeans(prob.Activist.0), c(0.025,0.975)))

}

Marginal.Contextual[,1:3]<-100*Marginal.Contextual[,1:3]


save(Marginal.Individual, file="Marginal.Individual.MainModel")
save(Marginal.Contextual, file="Marginal.Contextual.MainModel")

Marginal.Country<-matrix(0, nrow=4*length(unique(Country)), ncol=3)
colnames(Marginal.Country)<-c("Mean", "95.Low", "95.High")
rownames(Marginal.Country)<-paste(rep(unique(df$country.name), each=4),
rep(c("Outsider", "Agitator", "Conventional", "Activist"), length(unique(Country))), 
sep="-")



Nu.Agitator.0<-rowMeans(Nu.Agitator)
Nu.Conventional.0<-rowMeans(Nu.Conventional)
Nu.Activist.0<-rowMeans(Nu.Activist)


for (j in 1:length(unique(Country))) {


Nu.Agitator.1<-Nu.Agitator[,j]
Nu.Conventional.1<-Nu.Conventional[,j]
Nu.Activist.1<-Nu.Activist[,j]



prob.Agitator.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X%*%(Beta.Agitator[s,])+matrix(Nu.Agitator.0[s],nrow(X)))/(1+exp(X%*%(Beta.Agitator[s,])+matrix(Nu.Agitator.0[s],nrow(X)))+
			exp(X%*%(Beta.Conventional[s,])+matrix(Nu.Conventional.0[s],nrow(X)))+exp(X%*%(Beta.Activist[s,])+matrix(Nu.Activist.0[s],nrow(X)))))

prob.Conventional.0<-sapply(1:dim(Beta.Agitator)[1],  function(s) exp(X%*%(Beta.Conventional[s,])+matrix(Nu.Conventional.0[s],nrow(X)))/(1+exp(X%*%(Beta.Agitator[s,])+matrix(Nu.Agitator.0[s],nrow(X)))+
			exp(X%*%(Beta.Conventional[s,])+matrix(Nu.Conventional.0[s],nrow(X)))+exp(X%*%(Beta.Activist[s,])+matrix(Nu.Activist.0[s],nrow(X)))))


prob.Activist.0<-sapply(1:dim(Beta.Agitator)[1],  function(s) exp(X%*%(Beta.Activist[s,])+matrix(Nu.Activist.0[s],nrow(X)))/(1+exp(X%*%(Beta.Agitator[s,])+matrix(Nu.Agitator.0[s],nrow(X)))+
			exp(X%*%(Beta.Conventional[s,])+matrix(Nu.Conventional.0[s],nrow(X)))+exp(X%*%(Beta.Activist[s,])+matrix(Nu.Activist.0[s],nrow(X)))))


prob.Outsider.0<-sapply(1:dim(Beta.Agitator)[1],  function(s) 1/(1+exp(X%*%(Beta.Agitator[s,])+matrix(Nu.Agitator.0[s],nrow(X)))+
			exp(X%*%(Beta.Conventional[s,])+matrix(Nu.Conventional.0[s],nrow(X)))+exp(X%*%(Beta.Activist[s,])+matrix(Nu.Activist.0[s],nrow(X)))))


prob.Agitator.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X%*%(Beta.Agitator[s,])+matrix(Nu.Agitator.1[s],nrow(X)))/(1+exp(X%*%(Beta.Agitator[s,])+matrix(Nu.Agitator.1[s],nrow(X)))+
			exp(X%*%(Beta.Conventional[s,])+matrix(Nu.Conventional.1[s],nrow(X)))+exp(X%*%(Beta.Activist[s,])+matrix(Nu.Activist.1[s],nrow(X)))))

prob.Conventional.1<-sapply(1:dim(Beta.Agitator)[1],  function(s) exp(X%*%(Beta.Conventional[s,])+matrix(Nu.Conventional.1[s],nrow(X)))/(1+exp(X%*%(Beta.Agitator[s,])+matrix(Nu.Agitator.1[s],nrow(X)))+
			exp(X%*%(Beta.Conventional[s,])+matrix(Nu.Conventional.1[s],nrow(X)))+exp(X%*%(Beta.Activist[s,])+matrix(Nu.Activist.1[s],nrow(X)))))

prob.Activist.1<-sapply(1:dim(Beta.Agitator)[1],  function(s) exp(X%*%(Beta.Activist[s,])+matrix(Nu.Activist.1[s],nrow(X)))/(1+exp(X%*%(Beta.Agitator[s,])+matrix(Nu.Agitator.1[s],nrow(X)))+
			exp(X%*%(Beta.Conventional[s,])+matrix(Nu.Conventional.1[s],nrow(X)))+exp(X%*%(Beta.Activist[s,])+matrix(Nu.Activist.1[s],nrow(X)))))


prob.Outsider.1<-sapply(1:dim(Beta.Agitator)[1],  function(s) 1/(1+exp(X%*%(Beta.Agitator[s,])+matrix(Nu.Agitator.1[s],nrow(X)))+
			exp(X%*%(Beta.Conventional[s,])+matrix(Nu.Conventional.1[s],nrow(X)))+exp(X%*%(Beta.Activist[s,])+matrix(Nu.Activist.1[s],nrow(X)))))



Marginal.Country[agrep(unique(df$country.name)[j], rownames(Marginal.Country))[1],]<-c(mean(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0)),
											quantile(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0), c(0.025,0.975)))

Marginal.Country[agrep( unique(df$country.name)[j], rownames(Marginal.Country))[2],]<-c(mean(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0)), quantile(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0), c(0.025,0.975)))


Marginal.Country[agrep( unique(df$country.name)[j], rownames(Marginal.Country))[3],]<-c(mean(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0)), quantile(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0), c(0.025,0.975)))


Marginal.Country[agrep( unique(df$country.name)[j], rownames(Marginal.Country))[4],]<-c(mean(colMeans(prob.Activist.1)-colMeans(prob.Activist.0)), quantile(colMeans(prob.Activist.1)-colMeans(prob.Activist.0), c(0.025,0.975)))

}

Marginal.Country[,1:3]<-100*Marginal.Country[,1:3]

save(Marginal.Country, file="Marginal.Country.MainModel")


Alpha.Baseline<-ALPHA
Alpha.Baseline.Means<-matrix(colMeans(Alpha.Baseline), ncol(Y), ncol(Matrix.Country))

Alpha.Conv.Baseline<-ALPHA.CONV
Alpha.Conv.Baseline.Means<-matrix(colMeans(Alpha.Conv.Baseline), ncol(Y), ncol(Matrix.Country))

Alpha.Unconv.Baseline<-ALPHA.UNCONV
Alpha.Unconv.Baseline.Means<-matrix(colMeans(Alpha.Unconv.Baseline), ncol(Y), ncol(Matrix.Country))

P.Conv.Baseline<-P.CONV
P.Unconv.Baseline<-P.UNCONV

Baseline<-sum(log(mean(P.Conv.Baseline[,1])*mean(P.Unconv.Baseline[,1])* apply(sapply(1:ncol(Y),function(l) (pnorm((Matrix.Country%*%t(Alpha.Baseline.Means))[,l])^Y[,l])*((1-pnorm((Matrix.Country%*%t(Alpha.Baseline.Means))[,l]))^(1-Y[,l]))),1,prod)+
mean(P.Conv.Baseline[,2])*mean(P.Unconv.Baseline[,1])* apply(sapply(1:ncol(Y),function(l) (pnorm((Matrix.Country%*%t(Alpha.Baseline.Means))[,l]+(Matrix.Country%*%t(Alpha.Conv.Baseline.Means))[,l])^Y[,l])*((1-pnorm((Matrix.Country%*%t(Alpha.Baseline.Means))[,l]+(Matrix.Country%*%t(Alpha.Conv.Baseline.Means))[,l]))^(1-Y[,l]))),1,prod)+
mean(P.Conv.Baseline[,1])*mean(P.Unconv.Baseline[,2])* apply(sapply(1:ncol(Y),function(l) (pnorm((Matrix.Country%*%t(Alpha.Baseline.Means))[,l]+(Matrix.Country%*%t(Alpha.Unconv.Baseline.Means))[,l])^Y[,l])*((1-pnorm((Matrix.Country%*%t(Alpha.Baseline.Means))[,l]+(Matrix.Country%*%t(Alpha.Unconv.Baseline.Means))[,l]))^(1-Y[,l]))),1,prod)+
mean(P.Conv.Baseline[,2])*mean(P.Unconv.Baseline[,2])* apply(sapply(1:ncol(Y),function(l) (pnorm((Matrix.Country%*%t(Alpha.Baseline.Means))[,l]+(Matrix.Country%*%t(Alpha.Conv.Baseline.Means))[,l]+(Matrix.Country%*%t(Alpha.Unconv.Baseline.Means))[,l])^Y[,l])*((1-pnorm((Matrix.Country%*%t(Alpha.Baseline.Means))[,l]+(Matrix.Country%*%t(Alpha.Conv.Baseline.Means))[,l]+(Matrix.Country%*%t(Alpha.Unconv.Baseline.Means))[,l]))^(1-Y[,l]))),1,prod)
)
)

BIC.Baseline<--2*Baseline+log(nrow(Y))*(ncol(Alpha.Baseline)+
(ncol(Alpha.Conv.Baseline)-1)+
(ncol(Alpha.Unconv.Baseline)-1)+ncol(P.Conv.Baseline))

AIC.Baseline<--2*Baseline+2*(ncol(Alpha.Baseline)+
(ncol(Alpha.Conv.Baseline)-1)+
(ncol(Alpha.Unconv.Baseline)-1)+ncol(P.Conv.Baseline))

CAIC.Baseline<--2*Baseline+(log(nrow(Y))+1)*(ncol(Alpha.Baseline)+
(ncol(Alpha.Conv.Baseline)-1)+
(ncol(Alpha.Unconv.Baseline)-1)+ncol(P.Conv.Baseline))

Expected.Deviance<-mean(sapply((nrow(P.Conv.Baseline)-999):nrow(P.Conv.Baseline), function(s)
-2*sum(log(
P.Conv.Baseline[s,1]*P.Unconv.Baseline[s,1]* apply(sapply(1:ncol(Y), function(l) (pnorm(Matrix.Country%*%t(matrix(Alpha.Baseline[s,],ncol(Y), ncol(Matrix.Country)))[,l])^Y[,l])*((1-pnorm(Matrix.Country%*%t(matrix(Alpha.Baseline[s,],ncol(Y), ncol(Matrix.Country)))[,l]))^(1-Y[,l]))),1,prod)+
P.Conv.Baseline[s,2]*P.Unconv.Baseline[s,1]* apply(sapply(1:ncol(Y), function(l) (pnorm(Matrix.Country%*%t(matrix(Alpha.Conv.Baseline[s,],ncol(Y), ncol(Matrix.Country)))[,l]+Matrix.Country%*%t(matrix(Alpha.Baseline[s,],ncol(Y), ncol(Matrix.Country)))[,l])^Y[,l])*((1-pnorm(Matrix.Country%*%t(matrix(Alpha.Conv.Baseline[s,],ncol(Y), ncol(Matrix.Country)))[,l]+Matrix.Country%*%t(matrix(Alpha.Baseline[s,],ncol(Y), ncol(Matrix.Country)))[,l]))^(1-Y[,l]))),1,prod)+
P.Conv.Baseline[s,1]*P.Unconv.Baseline[s,2]* apply(sapply(1:ncol(Y), function(l) (pnorm(Matrix.Country%*%t(matrix(Alpha.Unconv.Baseline[s,],ncol(Y), ncol(Matrix.Country)))[,l]+Matrix.Country%*%t(matrix(Alpha.Baseline[s,],ncol(Y), ncol(Matrix.Country)))[,l])^Y[,l])*((1-pnorm(Matrix.Country%*%t(matrix(Alpha.Unconv.Baseline[s,],ncol(Y), ncol(Matrix.Country)))[,l]+Matrix.Country%*%t(matrix(Alpha.Baseline[s,],ncol(Y), ncol(Matrix.Country)))[,l]))^(1-Y[,l]))),1,prod)+
P.Conv.Baseline[s,2]*P.Unconv.Baseline[s,2]* apply(sapply(1:ncol(Y), function(l) (pnorm(Matrix.Country%*%t(matrix(Alpha.Conv.Baseline[s,],ncol(Y), ncol(Matrix.Country)))[,l]+Matrix.Country%*%t(matrix(Alpha.Unconv.Baseline[s,],ncol(Y), ncol(Matrix.Country)))[,l]+Matrix.Country%*%t(matrix(Alpha.Baseline[s,],ncol(Y), ncol(Matrix.Country)))[,l])^Y[,l])*((1-pnorm(Matrix.Country%*%t(matrix(Alpha.Conv.Baseline[s,],ncol(Y), ncol(Matrix.Country)))[,l]+Matrix.Country%*%t(matrix(Alpha.Unconv.Baseline[s,],ncol(Y), ncol(Matrix.Country)))[,l]+Matrix.Country%*%t(matrix(Alpha.Baseline[s,],ncol(Y), ncol(Matrix.Country)))[,l]))^(1-Y[,l]))),1,prod)
)
)
)
)

Deviance.At.Posterior.Means<--2*Baseline

DIC.Baseline<-2*Expected.Deviance-Deviance.At.Posterior.Means

a.Baseline<-sapply((nrow(P.Conv.Baseline)-999):nrow(P.Conv.Baseline), function(s) P.Conv.Baseline[s,1]*P.Unconv.Baseline[s,1]* apply(sapply(1:ncol(Y), function(l) (pnorm(Matrix.Country%*%t(matrix(Alpha.Baseline[s,],ncol(Y), ncol(Matrix.Country)))[,l])^Y[,l])*((1-pnorm(Matrix.Country%*%t(matrix(Alpha.Baseline[s,],ncol(Y), ncol(Matrix.Country)))[,l]))^(1-Y[,l]))),1,prod)+
P.Conv.Baseline[s,2]*P.Unconv.Baseline[s,1]* apply(sapply(1:ncol(Y), function(l) (pnorm(Matrix.Country%*%t(matrix(Alpha.Conv.Baseline[s,],ncol(Y), ncol(Matrix.Country)))[,l]+Matrix.Country%*%t(matrix(Alpha.Baseline[s,],ncol(Y), ncol(Matrix.Country)))[,l])^Y[,l])*((1-pnorm(Matrix.Country%*%t(matrix(Alpha.Conv.Baseline[s,],ncol(Y), ncol(Matrix.Country)))[,l]+Matrix.Country%*%t(matrix(Alpha.Baseline[s,],ncol(Y), ncol(Matrix.Country)))[,l]))^(1-Y[,l]))),1,prod)+
P.Conv.Baseline[s,1]*P.Unconv.Baseline[s,2]* apply(sapply(1:ncol(Y), function(l) (pnorm(Matrix.Country%*%t(matrix(Alpha.Unconv.Baseline[s,],ncol(Y), ncol(Matrix.Country)))[,l]+Matrix.Country%*%t(matrix(Alpha.Baseline[s,],ncol(Y), ncol(Matrix.Country)))[,l])^Y[,l])*((1-pnorm(Matrix.Country%*%t(matrix(Alpha.Unconv.Baseline[s,],ncol(Y), ncol(Matrix.Country)))[,l]+Matrix.Country%*%t(matrix(Alpha.Baseline[s,],ncol(Y), ncol(Matrix.Country)))[,l]))^(1-Y[,l]))),1,prod)+
P.Conv.Baseline[s,2]*P.Unconv.Baseline[s,2]* apply(sapply(1:ncol(Y), function(l) (pnorm(Matrix.Country%*%t(matrix(Alpha.Conv.Baseline[s,],ncol(Y), ncol(Matrix.Country)))[,l]+Matrix.Country%*%t(matrix(Alpha.Unconv.Baseline[s,],ncol(Y), ncol(Matrix.Country)))[,l]+Matrix.Country%*%t(matrix(Alpha.Baseline[s,],ncol(Y), ncol(Matrix.Country)))[,l])^Y[,l])*((1-pnorm(Matrix.Country%*%t(matrix(Alpha.Conv.Baseline[s,],ncol(Y), ncol(Matrix.Country)))[,l]+Matrix.Country%*%t(matrix(Alpha.Unconv.Baseline[s,],ncol(Y), ncol(Matrix.Country)))[,l]+Matrix.Country%*%t(matrix(Alpha.Baseline[s,],ncol(Y), ncol(Matrix.Country)))[,l]))^(1-Y[,l]))),1,prod))

WAIC.Baseline<--2*(sum(log(rowMeans(a.Baseline)))-sum(rowSums((log(a.Baseline)-rowMeans(log(a.Baseline)))^2)/999))


save(BIC.Baseline, file="BIC.Baseline")
save(AIC.Baseline, file="AIC.Baseline")
save(CAIC.Baseline, file="CAIC.Baseline")
save(WAIC.Baseline, file="WAIC.Baseline")
save(DIC.Baseline, file="DIC.Baseline")
save(Baseline, file="Baseline")

# Now we produce Table 2
Table.2<-rbind(Alpha.conv.means,Alpha.unconv.means)
Table.2<-Table.2[c(8, 1:4, 6,5, 7,10, 9, 11, 12, 20, 13:16, 18, 17, 19, 22, 21, 23, 24),] 
colnames(Table.2)<-c("Activity", "Dimension", "Mean", "Interval")
print(Table.2)

Table.2<-cbind(Table.2[1:12,-c(2)],Table.2[13:24,3:4])


Titles<-matrix(c("", "Conventional", "Dimension", "Unconventional", "Dimension"),nrow=1)

g1 <- tableGrob(Table.2, rows = NULL)
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g1), l = 1, r = ncol(g1))
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g1), l = 1, r = 1)
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g1), l = 1, r = 3)
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 1, l = 1, r = ncol(g1))


g0 <- tableGrob(Titles, rows = NULL)
g0 <- gtable_add_grob(g0,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g0), l = 1, r = 3)
g0 <- gtable_add_grob(g0,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g0), l = 1, r = 1)
g0 <- gtable_add_grob(g0,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 1, l = 1, r = ncol(g0))

g<-rbind(g0,g1)


pdf(file = "Table.2.pdf", height=10, width=10)
grid.draw(g)
dev.off()


############################################ Table 3 ################################################################################

rm(list=ls())

load("dataset_2012.RData")

df<-subset(dataset, select=c("country.name", "ActMeetMun",
"country.name", "ActMeetMun", "ActContMun",  "ActContAut" ,                           
"ActMeetImp" , "ActSolveProb" ,  "ActMeetPty" ,  "ActContGvt",                            
"ActProtest" , "ActBlock" ,  "ActVoted",  "ActSign" ,                              
"ActShare" , "education" ,  "female", "age", "relative.incomeproxy" ,
"victim", "corrupt", "spatial.distance" ,
"compulsory.voting", "gdp.pc", "rule.of.lw", "enp", "social.spending"))                       
df<-na.exclude(df)



Y <- cbind(df$ActMeetMun, df$ActContMun, df$ActContAut,
		 df$ActContGvt, df$ActSolveProb, 
		df$ActMeetImp, df$ActMeetPty, df$ActVoted, 
		df$ActShare, df$ActSign, df$ActProtest, df$ActBlock)
colnames(Y)<-c("ActMeetMun", "ActContMun", "ActContAut", "ActContGvt",
		"ActSolveProb", "ActMeetImp", "ActMeetPty", "ActVoted",
		"ActShare", "ActSign", "ActProtest", "ActBlock")

Country<-as.numeric(factor(df$country.name), levels=unique(df$country.name))
		

Matrix.Country<-matrix(0,nrow(Y),length(unique(Country)))           
for (i in 1:length(unique(Country))) Matrix.Country[Country==i,i]<-1

df$age.centered<-(df$age-mean(df$age))/(2*sd(df$age))
df$education.centered<-(df$education-mean(df$education))/(2*sd(df$education))


df$relative.incomeproxy.centered<-(df$relative.incomeproxy-mean(df$relative.incomeproxy))/(2*sd(df$relative.incomeproxy))


df$spatial.distance.centered<-(df$spatial.distance-mean(df$spatial.distance))/(2*sd(df$spatial.distance))

X<-cbind(1, df$age.centered, df$female, df$education.centered, 
	df$relative.incomeproxy.centered, 
	df$victim, df$corrupt, 
	df$spatial.distance.centered,
	(df$rule.of.lw-mean(df$rule.of.lw))/(2*sd(df$rule.of.lw)),
		df$compulsory.voting,
		(df$enp-mean(df$enp))/(2*sd(df$enp)),
		(df$gdp.pc-mean(df$gdp.pc))/(2*sd(df$gdp.pc)),
		(df$social.spending-mean(df$social.spending))/(2*sd(df$social.spending))
)



load("Results_MetricinvarianceModel")


# Convergence Checks
Alpha<-mcmc.list(as.mcmc(t(Results$alpha[,,1])), 
		as.mcmc(t(Results$alpha[,,2])),
		as.mcmc(t(Results$alpha[,,3])))
print(paste("Convergence Alpha:", gelman.diag(Alpha)[[2]]<1.2, sep=" "))


Alpha.Conv<-mcmc.list(as.mcmc(t(Results$alpha_conv[-12,,1])), 
		as.mcmc(t(Results$alpha_conv[-12,,2])),
		as.mcmc(t(Results$alpha_conv[-12,,3])))
print(paste("Convergence Alpha Conv:", gelman.diag(Alpha.Conv)[[2]]<1.2, sep=" "))


Alpha.Unconv<-mcmc.list(as.mcmc(t(Results$alpha_unconv[-1,,1])), 
		as.mcmc(t(Results$alpha_unconv[-1,,2])),
		as.mcmc(t(Results$alpha_unconv[-1,,3])))
print(paste("Convergence Alpha Unconventional:", gelman.diag(Alpha.Unconv)[[2]]<1.2, sep=" "))


P.Conv<-mcmc.list(as.mcmc((Results$P_conv[2,,1])), 
		as.mcmc((Results$P_conv[2,,2])),
		as.mcmc((Results$P_conv[2,,3])))
print(paste("Convergence Prob. Conventional:", gelman.diag(P.Conv)[[1]][,1]<1.2, sep=" "))



P.Unconv<-mcmc.list(as.mcmc((Results$P_unconv[2,,1])), 
		as.mcmc((Results$P_unconv[2,,2])),
		as.mcmc((Results$P_unconv[2,,3])))
print(paste("Convergence Prob. Unconventional", gelman.diag(P.Unconv)[[1]][,1]<1.2, sep=" "))




Alpha.MetricInvariance<-rbind(t(Results$alpha[,,1]), t(Results$alpha[,,2]),
		t(Results$alpha[,,3]))
Alpha.MetricInvariance.Means<-matrix(colMeans(Alpha.MetricInvariance), ncol(Y), ncol(Matrix.Country))


Alpha.Conv.MetricInvariance<-rbind(t(Results$alpha_conv[,,1]), t(Results$alpha_conv[,,2]),
		t(Results$alpha_conv[,,3]))

Alpha.Unconv.MetricInvariance<-rbind(t(Results$alpha_unconv[,,1]), t(Results$alpha_unconv[,,2]),
		t(Results$alpha_unconv[,,3]))

P.Conv.MetricInvariance<-rbind(t(Results$P_conv[,,1]), 
		t(Results$P_conv[,,2]),
		t(Results$P_conv[,,3]))

P.Unconv.MetricInvariance<-rbind(t(Results$P_unconv[,,1]),  
		t(Results$P_unconv[,,2]),
		t(Results$P_unconv[,,3]))


MetricInvariance<-sum(log(mean(P.Conv.MetricInvariance[,1])*mean(P.Unconv.MetricInvariance[,1])* apply(sapply(1:ncol(Y),function(l) (pnorm((Matrix.Country%*%t(Alpha.MetricInvariance.Means))[,l])^Y[,l])*((1-pnorm((Matrix.Country%*%t(Alpha.MetricInvariance.Means))[,l]))^(1-Y[,l]))),1,prod)+
mean(P.Conv.MetricInvariance[,2])*mean(P.Unconv.MetricInvariance[,1])* apply(sapply(1:ncol(Y),function(l) (pnorm((Matrix.Country%*%t(Alpha.MetricInvariance.Means))[,l]+colMeans(Alpha.Conv.MetricInvariance)[l])^Y[,l])*((1-pnorm((Matrix.Country%*%t(Alpha.MetricInvariance.Means))[,l]+colMeans(Alpha.Conv.MetricInvariance)[l]))^(1-Y[,l]))),1,prod)+
mean(P.Conv.MetricInvariance[,1])*mean(P.Unconv.MetricInvariance[,2])* apply(sapply(1:ncol(Y),function(l) (pnorm((Matrix.Country%*%t(Alpha.MetricInvariance.Means))[,l]+colMeans(Alpha.Unconv.MetricInvariance)[l])^Y[,l])*((1-pnorm((Matrix.Country%*%t(Alpha.MetricInvariance.Means))[,l]+colMeans(Alpha.Unconv.MetricInvariance)[l]))^(1-Y[,l]))),1,prod)+
mean(P.Conv.MetricInvariance[,2])*mean(P.Unconv.MetricInvariance[,2])* apply(sapply(1:ncol(Y),function(l) (pnorm((Matrix.Country%*%t(Alpha.MetricInvariance.Means))[,l]+colMeans(Alpha.Conv.MetricInvariance)[l]+colMeans(Alpha.Unconv.MetricInvariance)[l])^Y[,l])*((1-pnorm((Matrix.Country%*%t(Alpha.MetricInvariance.Means))[,l]+colMeans(Alpha.Conv.MetricInvariance)[l]+colMeans(Alpha.Unconv.MetricInvariance)[l]))^(1-Y[,l]))),1,prod)
)
)



npars.MetricInvariance<-(ncol(Alpha.MetricInvariance)+
(ncol(Alpha.Conv.MetricInvariance)-1)+
(ncol(Alpha.Unconv.MetricInvariance)-1)+ncol(P.Conv.MetricInvariance))



BIC.MetricInvariance<--2*MetricInvariance+log(nrow(Y))*(ncol(Alpha.MetricInvariance)+
(ncol(Alpha.Conv.MetricInvariance)-1)+
(ncol(Alpha.Unconv.MetricInvariance)-1)+ncol(P.Conv.MetricInvariance))



AIC.MetricInvariance<--2*MetricInvariance+2*(ncol(Alpha.MetricInvariance)+
(ncol(Alpha.Conv.MetricInvariance)-1)+
(ncol(Alpha.Unconv.MetricInvariance)-1)+ncol(P.Conv.MetricInvariance))



CAIC.MetricInvariance<--2*MetricInvariance+(log(nrow(Y))+1)*(ncol(Alpha.MetricInvariance)+
(ncol(Alpha.Conv.MetricInvariance)-1)+
(ncol(Alpha.Unconv.MetricInvariance)-1)+ncol(P.Conv.MetricInvariance))


Expected.Deviance<-mean(sapply((nrow(P.Conv.MetricInvariance)-999):nrow(P.Conv.MetricInvariance), function(s)
-2*sum(log(
P.Conv.MetricInvariance[s,1]*P.Unconv.MetricInvariance[s,1]* apply(sapply(1:ncol(Y), function(l) (pnorm(Matrix.Country%*%t(matrix(Alpha.MetricInvariance[s,],ncol(Y), ncol(Matrix.Country)))[,l])^Y[,l])*((1-pnorm(Matrix.Country%*%t(matrix(Alpha.MetricInvariance[s,],ncol(Y), ncol(Matrix.Country)))[,l]))^(1-Y[,l]))),1,prod)+
P.Conv.MetricInvariance[s,2]*P.Unconv.MetricInvariance[s,1]* apply(sapply(1:ncol(Y), function(l) (pnorm(Alpha.Conv.MetricInvariance[s,l]+Matrix.Country%*%t(matrix(Alpha.MetricInvariance[s,],ncol(Y), ncol(Matrix.Country)))[,l])^Y[,l])*((1-pnorm(Alpha.Conv.MetricInvariance[s,l]+Matrix.Country%*%t(matrix(Alpha.MetricInvariance[s,],ncol(Y), ncol(Matrix.Country)))[,l]))^(1-Y[,l]))),1,prod)+
P.Conv.MetricInvariance[s,1]*P.Unconv.MetricInvariance[s,2]* apply(sapply(1:ncol(Y), function(l) (pnorm(Alpha.Unconv.MetricInvariance[s,l]+Matrix.Country%*%t(matrix(Alpha.MetricInvariance[s,],ncol(Y), ncol(Matrix.Country)))[,l])^Y[,l])*((1-pnorm(Alpha.Unconv.MetricInvariance[s,l]+Matrix.Country%*%t(matrix(Alpha.MetricInvariance[s,],ncol(Y), ncol(Matrix.Country)))[,l]))^(1-Y[,l]))),1,prod)+
P.Conv.MetricInvariance[s,2]*P.Unconv.MetricInvariance[s,2]* apply(sapply(1:ncol(Y), function(l) (pnorm(Alpha.Conv.MetricInvariance[s,l]+Alpha.Unconv.MetricInvariance[s,l]+Matrix.Country%*%t(matrix(Alpha.MetricInvariance[s,],ncol(Y), ncol(Matrix.Country)))[,l])^Y[,l])*((1-pnorm(Alpha.Conv.MetricInvariance[s,l]+Alpha.Unconv.MetricInvariance[s,l]+Matrix.Country%*%t(matrix(Alpha.MetricInvariance[s,],ncol(Y), ncol(Matrix.Country)))[,l]))^(1-Y[,l]))),1,prod)
)
)
)
)

Deviance.At.Posterior.Means<--2*MetricInvariance

DIC.MetricInvariance<-2*Expected.Deviance-Deviance.At.Posterior.Means

a.MetricInvariance<-sapply((nrow(P.Conv.MetricInvariance)-999):nrow(P.Conv.MetricInvariance), function(s) P.Conv.MetricInvariance[s,1]*P.Unconv.MetricInvariance[s,1]* apply(sapply(1:ncol(Y), function(l) (pnorm(Matrix.Country%*%t(matrix(Alpha.MetricInvariance[s,],ncol(Y), ncol(Matrix.Country)))[,l])^Y[,l])*((1-pnorm(Matrix.Country%*%t(matrix(Alpha.MetricInvariance[s,],ncol(Y), ncol(Matrix.Country)))[,l]))^(1-Y[,l]))),1,prod)+
P.Conv.MetricInvariance[s,2]*P.Unconv.MetricInvariance[s,1]* apply(sapply(1:ncol(Y), function(l) (pnorm(Alpha.Conv.MetricInvariance[s,l]+Matrix.Country%*%t(matrix(Alpha.MetricInvariance[s,],ncol(Y), ncol(Matrix.Country)))[,l])^Y[,l])*((1-pnorm(Alpha.Conv.MetricInvariance[s,l]+Matrix.Country%*%t(matrix(Alpha.MetricInvariance[s,],ncol(Y), ncol(Matrix.Country)))[,l]))^(1-Y[,l]))),1,prod)+
P.Conv.MetricInvariance[s,1]*P.Unconv.MetricInvariance[s,2]* apply(sapply(1:ncol(Y), function(l) (pnorm(Alpha.Unconv.MetricInvariance[s,l]+Matrix.Country%*%t(matrix(Alpha.MetricInvariance[s,],ncol(Y), ncol(Matrix.Country)))[,l])^Y[,l])*((1-pnorm(Alpha.Unconv.MetricInvariance[s,l]+Matrix.Country%*%t(matrix(Alpha.MetricInvariance[s,],ncol(Y), ncol(Matrix.Country)))[,l]))^(1-Y[,l]))),1,prod)+
P.Conv.MetricInvariance[s,2]*P.Unconv.MetricInvariance[s,2]* apply(sapply(1:ncol(Y), function(l) (pnorm(Alpha.Conv.MetricInvariance[s,l]+Alpha.Unconv.MetricInvariance[s,l]+Matrix.Country%*%t(matrix(Alpha.MetricInvariance[s,],ncol(Y), ncol(Matrix.Country)))[,l])^Y[,l])*((1-pnorm(Alpha.Conv.MetricInvariance[s,l]+Alpha.Unconv.MetricInvariance[s,l]+Matrix.Country%*%t(matrix(Alpha.MetricInvariance[s,],ncol(Y), ncol(Matrix.Country)))[,l]))^(1-Y[,l]))),1,prod)
)

WAIC.MetricInvariance<--2*(sum(log(rowMeans(a.MetricInvariance)))-sum(rowSums((log(a.MetricInvariance)-rowMeans(log(a.MetricInvariance)))^2)/999))




save(BIC.MetricInvariance, file="BIC.MetricInvariance")
save(AIC.MetricInvariance, file="AIC.MetricInvariance")
save(CAIC.MetricInvariance, file="CAIC.MetricInvariance")
save(WAIC.MetricInvariance, file="WAIC.MetricInvariance")
save(DIC.MetricInvariance, file="DIC.MetricInvariance")
save(MetricInvariance, file="MetricInvariance")

rm(Results)

load("Results_ScalarinvarianceModel")


# Check Convergence
Alpha<-mcmc.list(as.mcmc(t(Results$alpha[,,1])), 
		as.mcmc(t(Results$alpha[,,2])),
		as.mcmc(t(Results$alpha[,,3])))
print(paste("Convergence Alpha:", gelman.diag(Alpha)[[2]]<1.2, sep=" "))


Alpha.Conv<-mcmc.list(as.mcmc(t(Results$alpha_conv[-12,,1])), 
		as.mcmc(t(Results$alpha_conv[-12,,2])),
		as.mcmc(t(Results$alpha_conv[-12,,3])))
print(paste("Convergence Alpha Conv:", gelman.diag(Alpha.Conv)[[2]]<1.2, sep=" "))


Alpha.Unconv<-mcmc.list(as.mcmc(t(Results$alpha_unconv[-1,,1])), 
		as.mcmc(t(Results$alpha_unconv[-1,,2])),
		as.mcmc(t(Results$alpha_unconv[-1,,3])))
print(paste("Convergence Alpha Unconventional:", gelman.diag(Alpha.Unconv)[[2]]<1.2, sep=" "))

P.Conv<-mcmc.list(as.mcmc((Results$P_conv[2,,1])), 
		as.mcmc((Results$P_conv[2,,2])),
		as.mcmc((Results$P_conv[2,,3])))
print(paste("Convergence Prob. Conventional:", gelman.diag(P.Conv)[[1]][,1]<1.2, sep=" "))



P.Unconv<-mcmc.list(as.mcmc((Results$P_unconv[2,,1])), 
		as.mcmc((Results$P_unconv[2,,2])),
		as.mcmc((Results$P_unconv[2,,3])))
print(paste("Convergence Prob. Unconventional", gelman.diag(P.Unconv)[[1]][,1]<1.2, sep=" "))




Alpha.ScalarInvariance<-rbind(t(Results$alpha[,,1]), t(Results$alpha[,,2]),
		t(Results$alpha[,,3]))

Alpha.Conv.ScalarInvariance<-rbind(t(Results$alpha_conv[,,1]), t(Results$alpha_conv[,,2]),
		t(Results$alpha_conv[,,3]))

Alpha.Unconv.ScalarInvariance<-rbind(t(Results$alpha_unconv[,,1]), t(Results$alpha_unconv[,,2]),
		t(Results$alpha_unconv[,,3]))

P.Conv.ScalarInvariance<-rbind(t(Results$P_conv[,,1]), # Class 2 
		t(Results$P_conv[,,2]),
		t(Results$P_conv[,,3]))

P.Unconv.ScalarInvariance<-rbind(t(Results$P_unconv[,,1]), # Class 2 
		t(Results$P_unconv[,,2]),
		t(Results$P_unconv[,,3]))


ScalarInvariance<-sum(log(mean(P.Conv.ScalarInvariance[,1])*mean(P.Unconv.ScalarInvariance[,1])* apply(sapply(1:ncol(Y), function(l) (pnorm(colMeans(Alpha.ScalarInvariance)[l])^Y[,l])* ((1-pnorm(colMeans(Alpha.ScalarInvariance)[l]))^(1-Y[,l]))),1,prod)+
mean(P.Conv.ScalarInvariance[,2])*mean(P.Unconv.ScalarInvariance[,1])* apply(sapply(1:ncol(Y), function(l) (pnorm(colMeans(Alpha.ScalarInvariance)[l]+colMeans(Alpha.Conv.ScalarInvariance)[l])^Y[,l])* ((1-pnorm(colMeans(Alpha.ScalarInvariance)[l]+colMeans(Alpha.Conv.ScalarInvariance)[l]))^(1-Y[,l]))),1,prod)+
mean(P.Conv.ScalarInvariance[,1])*mean(P.Unconv.ScalarInvariance[,2])* apply(sapply(1:ncol(Y), function(l) (pnorm(colMeans(Alpha.ScalarInvariance)[l]+colMeans(Alpha.Unconv.ScalarInvariance)[l])^Y[,l])* ((1-pnorm(colMeans(Alpha.ScalarInvariance)[l]+colMeans(Alpha.Unconv.ScalarInvariance)[l]))^(1-Y[,l]))),1,prod)+
mean(P.Conv.ScalarInvariance[,2])*mean(P.Unconv.ScalarInvariance[,2])* apply(sapply(1:ncol(Y), function(l) (pnorm(colMeans(Alpha.ScalarInvariance)[l]+colMeans(Alpha.Conv.ScalarInvariance)[l]+colMeans(Alpha.Unconv.ScalarInvariance)[l])^Y[,l])* ((1-pnorm(colMeans(Alpha.ScalarInvariance)[l]+colMeans(Alpha.Conv.ScalarInvariance)[l]+colMeans(Alpha.Unconv.ScalarInvariance)[l]))^(1-Y[,l]))),1,prod)
))

npars.ScalarInvariance<-(ncol(Alpha.ScalarInvariance)+
(ncol(Alpha.Conv.ScalarInvariance)-1)+
(ncol(Alpha.Unconv.ScalarInvariance)-1)+ncol(P.Conv.ScalarInvariance))


BIC.ScalarInvariance<--2*ScalarInvariance+log(nrow(Y))*(ncol(Alpha.ScalarInvariance)+
(ncol(Alpha.Conv.ScalarInvariance)-1)+
(ncol(Alpha.Unconv.ScalarInvariance)-1)+ncol(P.Conv.ScalarInvariance))


AIC.ScalarInvariance<--2*ScalarInvariance+2*(ncol(Alpha.ScalarInvariance)+
(ncol(Alpha.Conv.ScalarInvariance)-1)+
(ncol(Alpha.Unconv.ScalarInvariance)-1)+ncol(P.Conv.ScalarInvariance))


CAIC.ScalarInvariance<--2*ScalarInvariance+(log(nrow(Y))+1)*(ncol(Alpha.ScalarInvariance)+
(ncol(Alpha.Conv.ScalarInvariance)-1)+
(ncol(Alpha.Unconv.ScalarInvariance)-1)+ncol(P.Conv.ScalarInvariance))



Expected.Deviance<-mean(sapply((nrow(P.Conv.ScalarInvariance)-999):nrow(P.Conv.ScalarInvariance), function(s)
-2*sum(log(
P.Conv.ScalarInvariance[s,1]*P.Unconv.ScalarInvariance[s,1]* apply(sapply(1:ncol(Y), function(l) (pnorm(Alpha.ScalarInvariance[s,l])^Y[,l])* ((1-pnorm(Alpha.ScalarInvariance[s,l]))^(1-Y[,l]))),1,prod)+
P.Conv.ScalarInvariance[s,2]*P.Unconv.ScalarInvariance[s,1]* apply(sapply(1:ncol(Y), function(l) (pnorm(Alpha.ScalarInvariance[s,l]+Alpha.Conv.ScalarInvariance[s,l])^Y[,l])* ((1-pnorm(Alpha.ScalarInvariance[s,l]+Alpha.Conv.ScalarInvariance[s,l]))^(1-Y[,l]))),1,prod)+
P.Conv.ScalarInvariance[s,1]*P.Unconv.ScalarInvariance[s,2]* apply(sapply(1:ncol(Y), function(l) (pnorm(Alpha.ScalarInvariance[s,l]+Alpha.Unconv.ScalarInvariance[s,l])^Y[,l])* ((1-pnorm(Alpha.ScalarInvariance[s,l]+Alpha.Unconv.ScalarInvariance[s,l]))^(1-Y[,l]))),1,prod)+
P.Conv.ScalarInvariance[s,2]*P.Unconv.ScalarInvariance[s,2]* apply(sapply(1:ncol(Y), function(l) (pnorm(Alpha.ScalarInvariance[s,l]+Alpha.Conv.ScalarInvariance[s,l]+Alpha.Unconv.ScalarInvariance[s,l])^Y[,l])* ((1-pnorm(Alpha.ScalarInvariance[s,l]+Alpha.Conv.ScalarInvariance[s,l]+Alpha.Unconv.ScalarInvariance[s,l]))^(1-Y[,l]))),1,prod)
))
)
)

Deviance.At.Posterior.Means<--2*ScalarInvariance

DIC.ScalarInvariance<-2*Expected.Deviance-Deviance.At.Posterior.Means

a.ScalarInvariance<-sapply((nrow(P.Conv.ScalarInvariance)-999):nrow(P.Conv.ScalarInvariance), function(s) P.Conv.ScalarInvariance[s,1]*P.Unconv.ScalarInvariance[s,1]* apply(sapply(1:ncol(Y), function(l) (pnorm(Alpha.ScalarInvariance[s,l])^Y[,l])* ((1-pnorm(Alpha.ScalarInvariance[s,l]))^(1-Y[,l]))),1,prod)+
P.Conv.ScalarInvariance[s,2]*P.Unconv.ScalarInvariance[s,1]* apply(sapply(1:ncol(Y), function(l) (pnorm(Alpha.ScalarInvariance[s,l]+Alpha.Conv.ScalarInvariance[s,l])^Y[,l])* ((1-pnorm(Alpha.ScalarInvariance[s,l]+Alpha.Conv.ScalarInvariance[s,l]))^(1-Y[,l]))),1,prod)+
P.Conv.ScalarInvariance[s,1]*P.Unconv.ScalarInvariance[s,2]* apply(sapply(1:ncol(Y), function(l) (pnorm(Alpha.ScalarInvariance[s,l]+Alpha.Unconv.ScalarInvariance[s,l])^Y[,l])* ((1-pnorm(Alpha.ScalarInvariance[s,l]+Alpha.Unconv.ScalarInvariance[s,l]))^(1-Y[,l]))),1,prod)+
P.Conv.ScalarInvariance[s,2]*P.Unconv.ScalarInvariance[s,2]* apply(sapply(1:ncol(Y), function(l) (pnorm(Alpha.ScalarInvariance[s,l]+Alpha.Conv.ScalarInvariance[s,l]+Alpha.Unconv.ScalarInvariance[s,l])^Y[,l])* ((1-pnorm(Alpha.ScalarInvariance[s,l]+Alpha.Conv.ScalarInvariance[s,l]+Alpha.Unconv.ScalarInvariance[s,l]))^(1-Y[,l]))),1,prod)
)

WAIC.ScalarInvariance<--2*(sum(log(rowMeans(a.ScalarInvariance)))-sum(rowSums((log(a.ScalarInvariance)-rowMeans(log(a.ScalarInvariance)))^2)/999))


save(BIC.ScalarInvariance, file="BIC.ScalarInvariance")
save(AIC.ScalarInvariance, file="AIC.ScalarInvariance")
save(CAIC.ScalarInvariance, file="CAIC.ScalarInvariance")
save(WAIC.ScalarInvariance, file="WAIC.ScalarInvariance")
save(DIC.ScalarInvariance, file="DIC.ScalarInvariance")
save(ScalarInvariance, file="ScalarInvariance")

rm(Results)

load("Results_SingleclassModel")


# Check Convergence
Alpha<-mcmc.list(as.mcmc(t(Results$alpha[,,1])), 
		as.mcmc(t(Results$alpha[,,2])),
		as.mcmc(t(Results$alpha[,,3])))
print(paste("Convergence Alpha:", gelman.diag(Alpha)[[2]]<1.2, sep=" "))


Alpha.Conv<-mcmc.list(as.mcmc(t(Results$alpha_conv[-seq(12,dim(Results$alpha_conv)[1],by=12),,1])), 
		as.mcmc(t(Results$alpha_conv[-seq(12,dim(Results$alpha_conv)[1],by=12),,2])),
		as.mcmc(t(Results$alpha_conv[-seq(12,dim(Results$alpha_conv)[1],by=12),,3])))
print(paste("Convergence Alpha Conv:", gelman.diag(Alpha.Conv)[[2]]<1.2, sep=" "))


Alpha.Unconv<-mcmc.list(as.mcmc(t(Results$alpha_unconv[-seq(1,dim(Results$alpha_unconv)[1],by=12),,1])), 
		as.mcmc(t(Results$alpha_unconv[-seq(1,dim(Results$alpha_unconv)[1],by=12),,2])),
		as.mcmc(t(Results$alpha_unconv[-seq(1,dim(Results$alpha_unconv)[1],by=12),,3])))
print(paste("Convergence Alpha Unconventional:", gelman.diag(Alpha.Unconv)[[2]]<1.2, sep=" "))


Alpha.SingleClass<-rbind(t(Results$alpha[,,1]), t(Results$alpha[,,2]),
		t(Results$alpha[,,3]))
Alpha.SingleClass.Means<-matrix(colMeans(Alpha.SingleClass), ncol(Y), ncol(Matrix.Country))


Alpha.Conv.SingleClass<-rbind(t(Results$alpha_conv[,,1]), t(Results$alpha_conv[,,2]),
		t(Results$alpha_conv[,,3]))
Alpha.Conv.SingleClass.Means<-matrix(colMeans(Alpha.Conv.SingleClass), ncol(Y), ncol(Matrix.Country))

Alpha.Unconv.SingleClass<-rbind(t(Results$alpha_unconv[,,1]), t(Results$alpha_unconv[,,2]),
		t(Results$alpha_unconv[,,3]))
Alpha.Unconv.SingleClass.Means<-matrix(colMeans(Alpha.Unconv.SingleClass), ncol(Y), ncol(Matrix.Country))


 
SingleClass<-sum(log(apply(sapply(1:ncol(Y),function(l) (pnorm((Matrix.Country%*%t(Alpha.SingleClass.Means))[,l]+colMeans(Alpha.Conv.SingleClass)[l]+colMeans(Alpha.Unconv.SingleClass)[l])^Y[,l])*((1-pnorm((Matrix.Country%*%t(Alpha.SingleClass.Means))[,l]+colMeans(Alpha.Conv.SingleClass)[l]+colMeans(Alpha.Unconv.SingleClass)[l]))^(1-Y[,l]))),1,prod)
)
)



npars.SingleClass<-(ncol(Alpha.SingleClass)+
(ncol(Alpha.Conv.SingleClass)-1)+
(ncol(Alpha.Unconv.SingleClass)-1))



BIC.SingleClass<--2*SingleClass+log(nrow(Y))*(ncol(Alpha.SingleClass)+
(ncol(Alpha.Conv.SingleClass)-1)+
(ncol(Alpha.Unconv.SingleClass)-1))



AIC.SingleClass<--2*SingleClass+2*(ncol(Alpha.SingleClass)+
(ncol(Alpha.Conv.SingleClass)-1)+
(ncol(Alpha.Unconv.SingleClass)-1))




CAIC.SingleClass<--2*SingleClass+(log(nrow(Y))+1)*(ncol(Alpha.SingleClass)+
(ncol(Alpha.Conv.SingleClass)-1)+
(ncol(Alpha.Unconv.SingleClass)-1))



Expected.Deviance<-mean(sapply((nrow(Alpha.SingleClass)-999):nrow(Alpha.SingleClass), function(s)
-2*sum(log(apply(sapply(1:ncol(Y), function(l) (pnorm(Alpha.Conv.SingleClass[s,l]+Alpha.Unconv.SingleClass[s,l]+Matrix.Country%*%t(matrix(Alpha.SingleClass[s,],ncol(Y), ncol(Matrix.Country)))[,l])^Y[,l])*((1-pnorm(Alpha.Conv.SingleClass[s,l]+Alpha.Unconv.SingleClass[s,l]+Matrix.Country%*%t(matrix(Alpha.SingleClass[s,],ncol(Y), ncol(Matrix.Country)))[,l]))^(1-Y[,l]))),1,prod)
)
)
)
)

Deviance.At.Posterior.Means<--2*SingleClass

DIC.SingleClass<-2*Expected.Deviance-Deviance.At.Posterior.Means

a.SingleClass<-sapply((nrow(Alpha.SingleClass)-999):nrow(Alpha.SingleClass), function(s) apply(sapply(1:ncol(Y), function(l) (pnorm(Alpha.Conv.SingleClass[s,l]+Alpha.Unconv.SingleClass[s,l]+Matrix.Country%*%t(matrix(Alpha.SingleClass[s,],ncol(Y), ncol(Matrix.Country)))[,l])^Y[,l])*((1-pnorm(Alpha.Conv.SingleClass[s,l]+Alpha.Unconv.SingleClass[s,l]+Matrix.Country%*%t(matrix(Alpha.SingleClass[s,],ncol(Y), ncol(Matrix.Country)))[,l]))^(1-Y[,l]))),1,prod)
)

WAIC.SingleClass<--2*(sum(log(rowMeans(a.SingleClass)))-sum(rowSums((log(a.SingleClass)-rowMeans(log(a.SingleClass)))^2)/999))




save(BIC.SingleClass, file="BIC.SingleClass")
save(AIC.SingleClass, file="AIC.SingleClass")
save(CAIC.SingleClass, file="CAIC.SingleClass")
save(WAIC.SingleClass, file="WAIC.SingleClass")
save(DIC.SingleClass, file="DIC.SingleClass")
save(SingleClass, file="SingleClass")


rm(list=ls())


load("BIC.Baseline")
load("AIC.Baseline")
load("CAIC.Baseline")
load("WAIC.Baseline")
load("DIC.Baseline")
load("Baseline")


load("BIC.MetricInvariance")
load("AIC.MetricInvariance")
load("CAIC.MetricInvariance")
load("WAIC.MetricInvariance")
load("DIC.MetricInvariance")
load("MetricInvariance")

load("BIC.ScalarInvariance")
load("AIC.ScalarInvariance")
load("CAIC.ScalarInvariance")
load("WAIC.ScalarInvariance")
load("DIC.ScalarInvariance")
load("ScalarInvariance")


load("BIC.SingleClass")
load("AIC.SingleClass")
load("CAIC.SingleClass")
load("WAIC.SingleClass")
load("DIC.SingleClass")
load("SingleClass")





AIC.Baseline<-round(AIC.Baseline, digits=2)
if (nchar(AIC.Baseline)<9) {
while (nchar(AIC.Baseline)<9) {
AIC.Baseline<-paste(AIC.Baseline, "0", sep="")
} 
} 
BIC.Baseline<-round(BIC.Baseline, digits=2)
if (nchar(BIC.Baseline)<9) {
while (nchar(BIC.Baseline)<9) {
BIC.Baseline<-paste(BIC.Baseline, "0", sep="")
} 
} 
CAIC.Baseline<-round(CAIC.Baseline, digits=2)
if (nchar(CAIC.Baseline)<9) {
while (nchar(CAIC.Baseline)<9) {
CAIC.Baseline<-paste(CAIC.Baseline, "0", sep="")
} 
} 
DIC.Baseline<-round(DIC.Baseline, digits=2)
if (nchar(DIC.Baseline)<9) {
while (nchar(DIC.Baseline)<9) {
DIC.Baseline<-paste(DIC.Baseline, "0", sep="")
} 
} 
WAIC.Baseline<-round(WAIC.Baseline, digits=2)
if (nchar(WAIC.Baseline)<9) {
while (nchar(WAIC.Baseline)<9) {
WAIC.Baseline<-paste(WAIC.Baseline, "0", sep="")
} 
} 

AIC.MetricInvariance<-round(AIC.MetricInvariance, digits=2)
if (nchar(AIC.MetricInvariance)<9) {
while (nchar(AIC.MetricInvariance)<9) {
AIC.MetricInvariance<-paste(AIC.MetricInvariance, "0", sep="")
} 
} 

BIC.MetricInvariance<-round(BIC.MetricInvariance, digits=2)
if (nchar(BIC.MetricInvariance)<9) {
while (nchar(BIC.MetricInvariance)<9) {
BIC.MetricInvariance<-paste(BIC.MetricInvariance, "0", sep="")
} 
} 

CAIC.MetricInvariance<-round(CAIC.MetricInvariance, digits=2)
if (nchar(CAIC.MetricInvariance)<9) {
while (nchar(CAIC.MetricInvariance)<9) {
CAIC.MetricInvariance<-paste(CAIC.MetricInvariance, "0", sep="")
} 
} 

DIC.MetricInvariance<-round(DIC.MetricInvariance, digits=2)
if (nchar(DIC.MetricInvariance)<9) {
while (nchar(DIC.MetricInvariance)<9) {
DIC.MetricInvariance<-paste(DIC.MetricInvariance, "0", sep="")
} 
} 

WAIC.MetricInvariance<-round(WAIC.MetricInvariance, digits=2)
if (nchar(WAIC.MetricInvariance)<9) {
while (nchar(WAIC.MetricInvariance)<9) {
WAIC.MetricInvariance<-paste(WAIC.MetricInvariance, "0", sep="")
} 
} 



AIC.ScalarInvariance<-round(AIC.ScalarInvariance, digits=2)
if (nchar(AIC.ScalarInvariance)<9) {
while (nchar(AIC.ScalarInvariance)<9) {
AIC.ScalarInvariance<-paste(AIC.ScalarInvariance, "0", sep="")
} 
} 

BIC.ScalarInvariance<-round(BIC.ScalarInvariance, digits=2)
if (nchar(BIC.ScalarInvariance)<9) {
while (nchar(BIC.ScalarInvariance)<9) {
BIC.ScalarInvariance<-paste(BIC.ScalarInvariance, "0", sep="")
} 
} 

CAIC.ScalarInvariance<-round(CAIC.ScalarInvariance, digits=2)
if (nchar(CAIC.ScalarInvariance)<9) {
while (nchar(CAIC.ScalarInvariance)<9) {
CAIC.ScalarInvariance<-paste(CAIC.ScalarInvariance, "0", sep="")
} 
} 

DIC.ScalarInvariance<-round(DIC.ScalarInvariance, digits=2)
if (nchar(DIC.ScalarInvariance)<9) {
while (nchar(DIC.ScalarInvariance)<9) {
DIC.ScalarInvariance<-paste(DIC.ScalarInvariance, "0", sep="")
} 
} 

WAIC.ScalarInvariance<-round(WAIC.ScalarInvariance, digits=2)
if (nchar(WAIC.ScalarInvariance)<9) {
while (nchar(WAIC.ScalarInvariance)<9) {
WAIC.ScalarInvariance<-paste(WAIC.ScalarInvariance, "0", sep="")
} 
} 


AIC.SingleClass<-round(AIC.SingleClass, digits=2)
if (nchar(AIC.SingleClass)<9) {
while (nchar(AIC.SingleClass)<9) {
AIC.SingleClass<-paste(AIC.SingleClass, "0", sep="")
} 
} 

BIC.SingleClass<-round(BIC.SingleClass, digits=2)
if (nchar(BIC.SingleClass)<9) {
while (nchar(BIC.SingleClass)<9) {
BIC.SingleClass<-paste(BIC.SingleClass, "0", sep="")
} 
} 

CAIC.SingleClass<-round(CAIC.SingleClass, digits=2)
if (nchar(CAIC.SingleClass)<9) {
while (nchar(CAIC.SingleClass)<9) {
CAIC.SingleClass<-paste(CAIC.SingleClass, "0", sep="")
} 
} 

DIC.SingleClass<-round(DIC.SingleClass, digits=2)
if (nchar(DIC.SingleClass)<9) {
while (nchar(DIC.SingleClass)<9) {
DIC.SingleClass<-paste(DIC.SingleClass, "0", sep="")
} 
} 

WAIC.SingleClass<-round(WAIC.SingleClass, digits=2)
if (nchar(WAIC.SingleClass)<9) {
while (nchar(WAIC.SingleClass)<9) {
WAIC.SingleClass<-paste(WAIC.SingleClass, "0", sep="")
} 
} 



AIC.Baseline<-paste(paste(substr(AIC.Baseline,1,3), ",", sep=""), substr(AIC.Baseline, 4,9), sep="")
BIC.Baseline<-paste(paste(substr(BIC.Baseline,1,3), ",", sep=""), substr(BIC.Baseline, 4,9), sep="")
CAIC.Baseline<-paste(paste(substr(CAIC.Baseline,1,3), ",", sep=""), substr(CAIC.Baseline, 4,9), sep="")
DIC.Baseline<-paste(paste(substr(DIC.Baseline,1,3), ",", sep=""), substr(DIC.Baseline, 4,9), sep="")
WAIC.Baseline<-paste(paste(substr(WAIC.Baseline,1,3), ",", sep=""), substr(WAIC.Baseline, 4,9), sep="")
AIC.MetricInvariance<-paste(paste(substr(AIC.MetricInvariance,1,3), ",", sep=""), substr(AIC.MetricInvariance, 4,9), sep="")
BIC.MetricInvariance<-paste(paste(substr(BIC.MetricInvariance,1,3), ",", sep=""), substr(BIC.MetricInvariance, 4,9), sep="")
CAIC.MetricInvariance<-paste(paste(substr(CAIC.MetricInvariance,1,3), ",", sep=""), substr(CAIC.MetricInvariance, 4,9), sep="")
DIC.MetricInvariance<-paste(paste(substr(DIC.MetricInvariance,1,3), ",", sep=""), substr(DIC.MetricInvariance, 4,9), sep="")
WAIC.MetricInvariance<-paste(paste(substr(WAIC.MetricInvariance,1,3), ",", sep=""), substr(WAIC.MetricInvariance, 4,9), sep="")
AIC.ScalarInvariance<-paste(paste(substr(AIC.ScalarInvariance,1,3), ",", sep=""), substr(AIC.ScalarInvariance, 4,9), sep="")
BIC.ScalarInvariance<-paste(paste(substr(BIC.ScalarInvariance,1,3), ",", sep=""), substr(BIC.ScalarInvariance, 4,9), sep="")
CAIC.ScalarInvariance<-paste(paste(substr(CAIC.ScalarInvariance,1,3), ",", sep=""), substr(CAIC.ScalarInvariance, 4,9), sep="")
DIC.ScalarInvariance<-paste(paste(substr(DIC.ScalarInvariance,1,3), ",", sep=""), substr(DIC.ScalarInvariance, 4,9), sep="")
WAIC.ScalarInvariance<-paste(paste(substr(WAIC.ScalarInvariance,1,3), ",", sep=""), substr(WAIC.ScalarInvariance, 4,9), sep="")
AIC.SingleClass<-paste(paste(substr(AIC.SingleClass,1,3), ",", sep=""), substr(AIC.SingleClass, 4,9), sep="")
BIC.SingleClass<-paste(paste(substr(BIC.SingleClass,1,3), ",", sep=""), substr(BIC.SingleClass, 4,9), sep="")
CAIC.SingleClass<-paste(paste(substr(CAIC.SingleClass,1,3), ",", sep=""), substr(CAIC.SingleClass, 4,9), sep="")
DIC.SingleClass<-paste(paste(substr(DIC.SingleClass,1,3), ",", sep=""), substr(DIC.SingleClass, 4,9), sep="")
WAIC.SingleClass<-paste(paste(substr(WAIC.SingleClass,1,3), ",", sep=""), substr(WAIC.SingleClass, 4,9), sep="")




pvalue.12<-round(pchisq(-2*(MetricInvariance-Baseline), df =(204*3+2)-(204*2+12+2), lower.tail = F), digits=3)
if (nchar(pvalue.12)==1) {
pvalue.12<-paste(pvalue.12, ".", sep="")
}
if (nchar(pvalue.12)>1 & nchar(pvalue.12)<5) {
while (nchar(pvalue.12)<5) {
pvalue.12<-paste(pvalue.12, "0", sep="")
} 
} 


pvalue.13<-round(pchisq(-2*(ScalarInvariance-Baseline), df =(204*3+2)-(12*3+2), lower.tail = F) , digits=3)
if (nchar(pvalue.13)==1) {
pvalue.13<-paste(pvalue.13, ".", sep="")
}
if (nchar(pvalue.13)>1 & nchar(pvalue.13)<5) {
while (nchar(pvalue.13)<5) {
pvalue.13<-paste(pvalue.13, "0", sep="")
} 
} 


pvalue.14<-round(pchisq(-2*(SingleClass-Baseline), df =(204*3+2)-(204*3), lower.tail = F), digits=3)
if (nchar(pvalue.14)==1) {
pvalue.14<-paste(pvalue.14, ".", sep="")
}
if (nchar(pvalue.14)>1 & nchar(pvalue.14)<5) {
while (nchar(pvalue.14)<5) {
pvalue.14<-paste(pvalue.14, "0", sep="")
} 
} 



Table.3<-matrix(0,nrow=6, ncol=4)
colnames(Table.3)<-c("Baseline model", "Model assuming metric invariance", "Model assuming scalar invariance", "Model assuming a single type")
rownames(Table.3)<-c("Akaike Information Criterion (AIC)",
		     "Bayesian Information Criterion (BIC)",
		     "Consistent AIC (CAIC)", 
		     "Deviance Information Criterion (DIC)", 
		     "Watanabe-Akaike Ctierion (WAIC)", 
		     "Chi-squared test (p-value)")
Table.3[1,]<-c(AIC.Baseline, AIC.MetricInvariance, AIC.ScalarInvariance, AIC.SingleClass)
Table.3[2,]<-c(BIC.Baseline, BIC.MetricInvariance, BIC.ScalarInvariance, BIC.SingleClass)
Table.3[3,]<-c(CAIC.Baseline, CAIC.MetricInvariance, CAIC.ScalarInvariance, CAIC.SingleClass)
Table.3[4,]<-c(DIC.Baseline, DIC.MetricInvariance, DIC.ScalarInvariance, DIC.SingleClass)
Table.3[5,]<-c(WAIC.Baseline, WAIC.MetricInvariance, WAIC.ScalarInvariance, WAIC.SingleClass)
Table.3[6,1]<-""
Table.3[6,2]<-pvalue.12
Table.3[6,3]<-pvalue.13
Table.3[6,4]<-pvalue.14


print(Table.3)


Table.3<-cbind(rownames(Table.3), Table.3)
rownames(Table.3)<-NULL


g1 <- tableGrob(Table.3, rows = NULL)
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g1), l = 1, r = ncol(g1))
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 1, l = 1, r = ncol(g1))


pdf(file = "Table.3.pdf", height=5, width=15)
grid.draw(g1)
dev.off()




############################################ Figure 1 ################################################################################

rm(list=ls())
load("Probs.Dimensions.MainModel")
prob.conv.type<-Probs.Dimensions.MainModel[,1]
prob.unconv.type<-Probs.Dimensions.MainModel[,2]


load("Posterior.Probs.MainModel")

per.outsiders <- round(colMeans(Posterior.Probs.MainModel)[1], digits=2)
if (nchar(per.outsiders)<4) {
while (nchar(per.outsiders)<4) {
per.outsiders<-paste(per.outsiders, "0", sep="")
} 
}


per.agitators<- round(colMeans(Posterior.Probs.MainModel)[2], digits=2)
if (nchar(per.agitators)<4) {
while (nchar(per.agitators)<4) {
per.agitators<-paste(per.agitators, "0", sep="")
} 
}


per.activists<- round(colMeans(Posterior.Probs.MainModel)[4], digits=2)
if (nchar(per.activists)<4) {
while (nchar(per.activists)<4) {
per.activists<-paste(per.activists, "0", sep="")
} 
}

per.conventionals <-max(round(colMeans(Posterior.Probs.MainModel)[3], digits=2), 1-round(colMeans(Posterior.Probs.MainModel)[1], digits=2)-round(colMeans(Posterior.Probs.MainModel)[2], digits=2)-round(colMeans(Posterior.Probs.MainModel)[4], digits=2))
if (nchar(per.conventionals)<4) {
while (nchar(per.conventionals)<4) {
per.conventionals<-paste(per.conventionals, "0", sep="")
} 
}




Points<-cbind(prob.conv.type, prob.unconv.type)
Points<-data.frame(Points)
names(Points)<-c("x", "y")


p.1<-ggplot(Points, aes(x=x, y=y))+
geom_point(colour = "lightgray", size = 0.5)+theme_bw()+
xlab("Probability of high conventional type")+
ylab("Probability of high 
unconventional type")+
theme(axis.text.x = element_text(color="black", size=11))+
	theme(axis.text.x = element_text(color="black", size=11))+
 theme(axis.title.y = element_text(size=13, face="bold"))+
 theme(axis.title.x = element_text(size=13, face="bold"))+
 theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))+
 theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))+
 geom_vline(xintercept = 0.5, linetype="dashed")+
 geom_hline(yintercept = 0.5, linetype="dashed")+
annotate(geom="text", x=0.25, y=0.25, label=paste("Outsiders",
per.outsiders, sep="\n"), color="black", size=5)+
annotate(geom="text", x=0.75, y=0.25, label=paste("Conventionals",
per.conventionals, sep="\n"), color="black", size=5)+
annotate(geom="text", x=0.25, y=0.75, label=paste("Agitators",
per.agitators, sep="\n"), color="black", size=5)+
annotate(geom="text", x=0.75, y=0.75, label=paste("Activists",
per.activists, sep="\n"), color="black", size=5)+
theme(plot.margin=unit(c(1.5,1,0.5,1), "lines"))


Types<-cbind(c("Outsider", "Agitator", "Conventional", "Activist"), colMeans(Posterior.Probs.MainModel), t(apply(Posterior.Probs.MainModel, 2, quantile, c(0.025,0.975))))
Types<-data.frame(Types)
names(Types)<-c("Type", "Mean", "Low", "High")

for (k in 2:ncol(Types)) {
Types[,k]<- as.numeric(levels(Types[,k]))[Types[,k]] 
}


Types$Type<- factor(Types$Type, 
	levels = c("Outsider", "Agitator", "Conventional",
		"Activist"))

p.2<- ggplot(Types, aes(x=Type, y=Mean)) + 
  geom_point(size=2, fill="black") + ylab("Probability of 
type assignment")+
	xlab("")+
  geom_errorbar(aes(ymin=Low, ymax=High), width=.1,
                 position=position_dodge(0.05))+
	theme_bw()+ ylim(c(0,0.82))+
	theme(axis.text.x = element_text(color="black", size=14))+
	theme(axis.text.x = element_text(color="black", size=11))+
 theme(axis.title.y = element_text(size=13, face="bold"))+
theme(plot.margin=unit(c(1.5,1,0.5,1), "lines"))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))


# Multiple plot function
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



pdf(file = "Figure.1.pdf", height=10)
grid.draw(multiplot(p.1, p.2, cols=1 ))
dev.off()

############################################ Figure 2 ################################################################################

rm(list=ls())

# Multiple plot function
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


load("Outsider.by.Country.MainModel")
load("Agitator.by.Country.MainModel")
load("Conventional.by.Country.MainModel")
load("Activist.by.Country.MainModel")

df.outsiders<-data.frame(cbind("Outsiders", Outsider.by.Country))
names(df.outsiders)<-c("Type", "Country", "Mean", "Low", "High")

df.agitators<-data.frame(cbind("Agitators", Agitator.by.Country))
names(df.agitators)<-c("Type", "Country", "Mean", "Low", "High")
 
 
df.conventionals<-data.frame(cbind("Conventionals", Conventional.by.Country))
names(df.conventionals)<-c("Type", "Country", "Mean", "Low", "High")

df.activists<-data.frame(cbind("Activists", Activist.by.Country))
names(df.activists)<-c("Type", "Country", "Mean", "Low", "High")
 
df.Figure2<-rbind(df.outsiders,	df.agitators,
	 df.conventionals,
	 df.activists)



df.Figure2$abbr<-NA
df.Figure2$abbr[df.Figure2$Country==1]<-"ARG"
df.Figure2$abbr[df.Figure2$Country==2]<-"BRA"
df.Figure2$abbr[df.Figure2$Country==3]<-"CHL"
df.Figure2$abbr[df.Figure2$Country==4]<-"COL"
df.Figure2$abbr[df.Figure2$Country==5]<-"CRI"
df.Figure2$abbr[df.Figure2$Country==6]<-"DOM"
df.Figure2$abbr[df.Figure2$Country==7]<-"ECU"
df.Figure2$abbr[df.Figure2$Country==8]<-"ESV"
df.Figure2$abbr[df.Figure2$Country==9]<-"GTM"
df.Figure2$abbr[df.Figure2$Country==10]<-"HND"
df.Figure2$abbr[df.Figure2$Country==11]<-"MEX"
df.Figure2$abbr[df.Figure2$Country==12]<-"NIC"
df.Figure2$abbr[df.Figure2$Country==13]<-"PAN"
df.Figure2$abbr[df.Figure2$Country==14]<-"PAR"
df.Figure2$abbr[df.Figure2$Country==15]<-"PER"
df.Figure2$abbr[df.Figure2$Country==16]<-"URU"
df.Figure2$abbr[df.Figure2$Country==17]<-"VEN"


for (k in 3:5){

df.Figure2[,k]<-as.numeric(levels(df.Figure2[,k]))[df.Figure2[,k]]
}


Outsiders<-df.Figure2[df.Figure2$Type=="Outsiders",-c(1,2)]

Agitators<-df.Figure2[df.Figure2$Type=="Agitators",-c(1,2)]

Conventionals<-df.Figure2[df.Figure2$Type=="Conventionals",-c(1,2)]

Activists<-df.Figure2[df.Figure2$Type=="Activists",-c(1,2)]


p.1<-ggplot(Outsiders,  aes(as.factor(abbr), Mean)) +
 geom_point(size=1, fill="black") +
  geom_errorbar(aes(ymin=Low, ymax=High), width=.1,
                 position=position_dodge(0.05))+
	ylab("")+xlab("")+ggtitle("Outsiders")+theme_bw()+
	theme(plot.margin=unit(c(0.5,1,0,0), "lines"))+
	#scale_y_continuous(limits=c(0.675,0.805), breaks=c(0.7, 0.75, 0.80), 
	#labels=c("0.70", "0.75", "0.80"))+
	theme(plot.title = element_text(hjust = 0))


p.2<-ggplot(Agitators,  aes(as.factor(abbr), Mean)) +
 geom_point(size=1, fill="black") +
  geom_errorbar(aes(ymin=Low, ymax=High), width=.1,
                 position=position_dodge(0.05))+
	ylab("")+xlab("")+ggtitle("Agitators")+theme_bw()+
	theme(plot.margin=unit(c(0.5,1,0,0), "lines"))+
	#scale_y_continuous(limits=c(0.02,0.08), breaks=c(0.02, 0.05, 0.08), 
	#labels=c("0.02", "0.05", "0.08"))+
	theme(plot.title = element_text(hjust = 0))


p.3<-ggplot(Conventionals,  aes(as.factor(abbr), Mean)) +
 geom_point(size=1, fill="black") +
  geom_errorbar(aes(ymin=Low, ymax=High), width=.1,
                 position=position_dodge(0.05))+
	ylab("")+xlab("")+ggtitle("Conventionals")+theme_bw()+
	theme(plot.margin=unit(c(0.5,1,0,0), "lines"))+
	#scale_y_continuous(limits=c(0.15,0.27), breaks=c(0.15, 0.2, 0.25), 
	#labels=c("0.15", "0.20", "0.25"))+
	theme(plot.title = element_text(hjust = 0))


p.4<-ggplot(Activists,  aes(as.factor(abbr), Mean)) +
 geom_point(size=1, fill="black") +
  geom_errorbar(aes(ymin=Low, ymax=High), width=.1,
                 position=position_dodge(0.05))+
	ylab("")+xlab("")+ggtitle("Activists")+theme_bw()+
	theme(plot.margin=unit(c(0.5,1,0,0), "lines"))+
	#scale_y_continuous(limits=c(0.0,0.04), breaks=c(0.0, 0.02, 0.04), 
	#labels=c("0.00", "0.02",  "0.04"))+
	theme(plot.title = element_text(hjust = 0))




pdf(file = "Figure.2.pdf")
grid.draw(multiplot(p.1, p.2, p.3, p.4, cols=1))
dev.off()


############################################ Figure 3 ################################################################################

rm(list=ls())
load("Marginal.Individual.MainModel")
load("Marginal.Contextual.MainModel")


Figure.Individual<-cbind(unlist(strsplit(rownames(Marginal.Individual), "-"))[seq(2,length(unlist(strsplit(rownames(Marginal.Individual), "-"))), by=2)],
unlist(strsplit(rownames(Marginal.Individual), "-"))[seq(1,length(unlist(strsplit(rownames(Marginal.Individual), "-"))), by=2)],
Marginal.Individual[,1:3])
Figure.Individual<-data.frame(Figure.Individual)
names(Figure.Individual)<-c("Type", "Variable", "Mean", "Low", "High")


Figure.Contextual<-cbind(unlist(strsplit(rownames(Marginal.Contextual), "-"))[seq(2,length(unlist(strsplit(rownames(Marginal.Contextual), "-"))), by=2)],
unlist(strsplit(rownames(Marginal.Contextual), "-"))[seq(1,length(unlist(strsplit(rownames(Marginal.Contextual), "-"))), by=2)],
Marginal.Contextual[,1:3])
Figure.Contextual<-data.frame(Figure.Contextual)
names(Figure.Contextual)<-c("Type", "Variable", "Mean", "Low", "High")


Figure.3<-rbind(Figure.Individual, Figure.Contextual)

Figure.3$Type<- factor(Figure.3$Type, 
	levels = c("Outsider",
		"Agitator",
		"Conventional", "Activist"))



Figure.3$Variable<-factor(Figure.3$Variable, 
	levels=rev(c("Age", "Female", "Education", 
			"Income", "Corruption", 
			"Victimization",
			"Ideology",
		"Rule of Law", "Compulsory Voting", "ENPP", 
			"GDP", "Social.Spending")),
       labels=rev(c("Age",
			  "Female",
			  "Education", 
			  "Relative Income",
			  "Perceived Corruption", 
			   "Crime Victimization",
			 "Ideological Distance to Incumbent",
			"Rule of Law",
			  "Compulsory Voting",
			  "ENPP", 
			  "GDP per capita",
			  "Social Spending")))




for (k in 3:5){
Figure.3[,k]<-as.numeric(levels(Figure.3[,k]))[Figure.3[,k]]
}

p<-ggplot(Figure.3,
 aes(y=Variable, x=Mean)) + 
    geom_errorbarh(aes(xmin=Low, 
	xmax=High, height=.2), colour="black") + facet_wrap(~Type)+
    geom_point(size=1.5)+
theme_bw()+xlab("Marginal effects (in percentage points)")+ylab("")+
geom_hline(yintercept=0, linetype="longdash", colour="gray40")+
theme(strip.text.x = element_text(size = 12, face="bold"))+
geom_vline(xintercept=0, linetype="longdash", colour="gray40")+
theme(strip.background = element_rect(fill = alpha('white'))) +
theme(axis.title.x=element_text(vjust=2, size=11))+
theme(axis.text.y=element_text(size=11))


pdf(file = "Figure.3.pdf")
grid.draw(p)
dev.off()




########################### TABLES AND FIGURES IN THE ONLINE APPENDIX #########################################################

############################################ SECTION A.1 ############################################################################

############################################ Table A.1 ################################################################################

rm(list=ls())
load("dataset_2012.RData")

dataset$abbreviations<-NA
dataset$abbreviations[dataset$country.name=="Argentina"]<-"ARG"
dataset$abbreviations[dataset$country.name=="Belize"]<-"BLZ"
dataset$abbreviations[dataset$country.name=="Brazil"]<-"BRA"
dataset$abbreviations[dataset$country.name=="Chile"]<-"CHL"
dataset$abbreviations[dataset$country.name=="Colombia"]<-"COL"
dataset$abbreviations[dataset$country.name=="Costa Rica"]<-"CRI"
dataset$abbreviations[dataset$country.name=="Dominican Republic"]<-"DOM"
dataset$abbreviations[dataset$country.name=="Ecuador"]<-"ECU"
dataset$abbreviations[dataset$country.name=="El Salvador"]<-"ESV"
dataset$abbreviations[dataset$country.name=="Guatemala"]<-"GTM"
dataset$abbreviations[dataset$country.name=="Guyana"]<-"GUY"
dataset$abbreviations[dataset$country.name=="Haiti"]<-"HTI"
dataset$abbreviations[dataset$country.name=="Honduras"]<-"HND"
dataset$abbreviations[dataset$country.name=="Jamaica"]<-"JAM"
dataset$abbreviations[dataset$country.name=="Mexico"]<-"MEX"
dataset$abbreviations[dataset$country.name=="Nicaragua"]<-"NIC"
dataset$abbreviations[dataset$country.name=="Panama"]<-"PAN"
dataset$abbreviations[dataset$country.name=="Paraguay"]<-"PAR"
dataset$abbreviations[dataset$country.name=="Peru"]<-"PER"
dataset$abbreviations[dataset$country.name=="Suriname"]<-"SUR"
dataset$abbreviations[dataset$country.name=="Trinidad and Tobago"]<-"TTO"
dataset$abbreviations[dataset$country.name=="Uruguay"]<-"URU"
dataset$abbreviations[dataset$country.name=="Venezuela"]<-"VEN"


Table.A.1<-cbind(rownames(table(dataset$country.name)), unique(dataset$abbreviations),table(dataset$country.name))

Table.A.1<-cbind(c(Table.A.1[c(1, 3, 4:10, 13, 15:19, 22:23),1], "Observations (main specification)",
Table.A.1[c(2, 11, 12, 14, 20, 21),1], "Observations (additional specifications)"),
c(Table.A.1[c(1, 3, 4:10, 13, 15:19, 22:23),2], "", Table.A.1[c(2, 11, 12, 14, 20, 21),2], ""),
c(Table.A.1[c(1, 3, 4:10, 13, 15:19, 22:23),3],
sum(as.numeric(Table.A.1[c(1, 3, 4:10, 13, 15:19, 22:23),3])),
Table.A.1[c(2, 11, 12, 14, 20, 21),3],
sum(as.numeric(Table.A.1[,3]))) 
)
colnames(Table.A.1)<-c("Country", "Abbreviation", "Sample Size")
rownames(Table.A.1)<-NULL
print(Table.A.1)


Main.Specification<-Table.A.1[1:18,]


Additional.Specifications<-rbind(c("", "", ""), Table.A.1[19:25,])
colnames(Additional.Specifications)<-NULL


g1 <- tableGrob(Main.Specification, rows = NULL)
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g1), l = 1, r = ncol(g1))
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 1, l = 1, r = ncol(g1))

g2 <- tableGrob(Additional.Specifications, rows = NULL)
g2 <- gtable_add_grob(g2,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g2), l = 1, r = ncol(g2))
g2 <- gtable_add_grob(g2,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 1, l = 1, r = ncol(g2))

g<-rbind(g1, g2)

pdf(file = "Table.A.1.pdf", height=10)
grid.draw(g)
dev.off()

############################################ Table A.2 ################################################################################


Table.A.2<-cbind(c("Voting", "Municipal meeting", "Contacting municipality",
		"Contacting local authority", "Contacting national authority",
 		"Improvements meeting", "Solving Problems",
		"Party meeting", "Sharing online",
		"Petitioning", "Protesting", "Blocking"),
rbind(c(round(mean(dataset$ActVoted), digits=2), round(sd(dataset$ActVoted), digits=2), paste(paste(paste("[", round(range(dataset$ActVoted)[1], digits=2), sep=""), ", ", sep=""),paste(round(range(dataset$ActVoted)[2],digits=2), "]", sep=""))),
c(round(mean(dataset$ActMeetMun), digits=2), round(sd(dataset$ActMeetMun), digits=2), paste(paste(paste("[", round(range(dataset$ActMeetMun)[1], digits=2), sep=""), ", ", sep=""),paste(round(range(dataset$ActMeetMun)[2], digits=2), "]", sep=""))),
c(round(mean(dataset$ActContMun), digits=2), round(sd(dataset$ActContMun), digits=2), paste(paste(paste("[", round(range(dataset$ActContMun)[1], digits=2), sep=""), ", ", sep=""),paste(round(range(dataset$ActContMun)[2], digits=2), "]", sep=""))),
c(round(mean(dataset$ActContAut), digits=2), round(sd(dataset$ActContAut), digits=2), paste(paste(paste("[", round(range(dataset$ActContAut)[1], digits=2), sep=""), ", ", sep=""),paste(round(range(dataset$ActContAut)[2], digits=2), "]", sep=""))),
c(round(mean(dataset$ActContGvt), digits=2), round(sd(dataset$ActContGvt), digits=2), paste(paste(paste("[", round(range(dataset$ActContGvt)[1], digits=2), sep=""), ", ", sep=""),paste(round(range(dataset$ActContGvt)[2], digits=2), "]", sep=""))),
c(round(mean(dataset$ActMeetImp), digits=2), round(sd(dataset$ActMeetImp), digits=2), paste(paste(paste("[", round(range(dataset$ActMeetImp)[1], digits=2), sep=""), ", ", sep=""),paste(round(range(dataset$ActMeetImp)[2], digits=2), "]", sep=""))),
c(round(mean(dataset$ActSolveProb), digits=2), round(sd(dataset$ActSolveProb), digits=2), paste(paste(paste("[", round(range(dataset$ActSolveProb)[1], digits=2), sep=""), ", ", sep=""),paste(round(range(dataset$ActSolveProb)[2], digits=2), "]", sep=""))),
c(round(mean(dataset$ActMeetPty), digits=2), round(sd(dataset$ActMeetPty), digits=2), paste(paste(paste("[", round(range(dataset$ActMeetPty)[1], digits=2), sep=""), ", ", sep=""),paste(round(range(dataset$ActMeetPty)[2], digits=2), "]", sep=""))),
c(round(mean(dataset$ActSign), digits=2), round(sd(dataset$ActSign), digits=2), paste(paste(paste("[", round(range(dataset$ActSign)[1], digits=2), sep=""), ", ", sep=""),paste(round(range(dataset$ActSign)[2], digits=2), "]", sep=""))),
c(round(mean(dataset$ActShare), digits=2), round(sd(dataset$ActShare), digits=2), paste(paste(paste("[", round(range(dataset$ActShare)[1], digits=2), sep=""), ", ", sep=""),paste(round(range(dataset$ActShare)[2], digits=2), "]", sep=""))),
c(round(mean(dataset$ActProtest), digits=2), round(sd(dataset$ActProtest), digits=2), paste(paste(paste("[", round(range(dataset$ActProtest)[1], digits=2), sep=""), ", ", sep=""),paste(round(range(dataset$ActProtest)[2], digits=2), "]", sep=""))),
c(round(mean(dataset$ActBlock), digits=2), round(sd(dataset$ActBlock), digits=2), paste(paste(paste("[", round(range(dataset$ActBlock)[1], digits=2), sep=""), ", ", sep=""),paste(round(range(dataset$ActBlock)[2], digits=2), "]", sep=""))))
)
colnames(Table.A.2)<-c("Dependent Variables", "Mean", "Std. Dev", "Range")

print(Table.A.2)

g <- tableGrob(Table.A.2, rows = NULL)
g <- gtable_add_grob(g,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g), l = 1, r = ncol(g))
g <- gtable_add_grob(g,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 1, l = 1, r = ncol(g))

pdf(file = "Table.A.2.pdf")
grid.draw(g)
dev.off()

############################################ Table A.3 ################################################################################


Main.Model<-cbind(c("Main Model", "Age", "Female",  "Education",  "Relative Income",  "Perceived Corruption", 
  "Crime Victimization", "Ideological Distance to Incumbent", "Rule of Law",
  "Compulsory Voting",  "ENPP",  "GDP per capita",  "Social Spending"),
rbind(c("", "", ""),
c(round(mean(dataset$age, na.rm=T), digits=2), round(sd(dataset$age, na.rm=T), digits=2), paste(paste(paste("[", round(range(dataset$age, na.rm=T)[1], digits=2), sep=""), ", ", sep=""),paste(round(range(dataset$age, na.rm=T)[2], digits=2), "]", sep=""))),
c(round(mean(dataset$female, na.rm=T), digits=2), round(sd(dataset$female, na.rm=T), digits=2), paste(paste(paste("[", round(range(dataset$female, na.rm=T)[1], digits=2), sep=""), ", ", sep=""),paste(round(range(dataset$female, na.rm=T)[2],digits=2), "]", sep=""))),
c(round(mean(dataset$education, na.rm=T), digits=2), round(sd(dataset$education, na.rm=T), digits=2), paste(paste(paste("[", round(range(dataset$education, na.rm=T)[1], digits=2), sep=""), ", ", sep=""),paste(round(range(dataset$education, na.rm=T)[2],digits=2), "]", sep=""))),
c(round(mean(dataset$relative.incomeproxy, na.rm=T), digits=2), round(sd(dataset$relative.incomeproxy, na.rm=T), digits=2), paste(paste(paste("[", round(range(dataset$relative.incomeproxy, na.rm=T)[1], digits=2), sep=""), ", ", sep=""),paste(round(range(dataset$relative.incomeproxy, na.rm=T)[2],digits=2), "]", sep=""))),
c(round(mean(dataset$corrupt, na.rm=T), digits=2), round(sd(dataset$corrupt, na.rm=T), digits=2), paste(paste(paste("[", round(range(dataset$corrupt, na.rm=T)[1], digits=2), sep=""), ", ", sep=""),paste(round(range(dataset$corrupt, na.rm=T)[2],digits=2), "]", sep=""))),
c(round(mean(dataset$victim, na.rm=T), digits=2), round(sd(dataset$victim, na.rm=T), digits=2), paste(paste(paste("[", round(range(dataset$victim, na.rm=T)[1], digits=2), sep=""), ", ", sep=""),paste(round(range(dataset$victim,na.rm=T)[2], digits=2), "]", sep=""))),
c(round(mean(dataset$spatial.distance, na.rm=T), digits=2), round(sd(dataset$spatial.distance, na.rm=T), digits=2), paste(paste(paste("[", round(range(dataset$spatial.distance, na.rm=T)[1], digits=2), sep=""), ", ", sep=""),paste(round(range(dataset$spatial.distance,na.rm=T)[2], digits=2), "]", sep=""))),
c(round(mean(dataset$compulsory.voting, na.rm=T), digits=2), round(sd(dataset$compulsory.voting, na.rm=T), digits=2), paste(paste(paste("[", round(range(dataset$compulsory.voting, na.rm=T)[1], digits=2), sep=""), ", ", sep=""),paste(round(range(dataset$compulsory.voting, na.rm=T)[2],digits=2), "]", sep=""))),
c(round(mean(dataset$enp, na.rm=T), digits=2), round(sd(dataset$enp, na.rm=T), digits=2), paste(paste(paste("[", round(range(dataset$enp, na.rm=T)[1], digits=2), sep=""), ", ", sep=""),paste(round(range(dataset$enp, na.rm=T)[2], digits=2), "]", sep=""))),
c(round(mean(dataset$gdp.pc/1000, na.rm=T), digits=2), round(sd(dataset$gdp.pc/1000, na.rm=T), digits=2), paste(paste(paste("[", round(range(dataset$gdp.pc/1000, na.rm=T)[1], digits=2), sep=""), ", ", sep=""),paste(round(range(dataset$gdp.pc/1000, na.rm=T)[2],digits=2), "]", sep=""))),
c(round(mean(dataset$rule.of.lw, na.rm=T), digits=2), round(sd(dataset$rule.of.lw, na.rm=T), digits=2), paste(paste(paste("[", round(range(dataset$rule.of.lw, na.rm=T)[1], digits=2), sep=""), ", ", sep=""),paste(round(range(dataset$rule.of.lw, na.rm=T)[2],digits=2), "]", sep=""))),
c(round(mean(dataset$social.spending/100, na.rm=T), digits=2), round(sd(dataset$social.spending/100, na.rm=T), digits=2), paste(paste(paste("[", round(range(dataset$social.spending/100, na.rm=T)[1], digits=2), sep=""), ", ", sep=""),paste(round(range(dataset$social.spending/100, na.rm=T)[2],digits=2), "]", sep=""))))
)


dataset$income.abovemean<-ifelse(dataset$income>mean(dataset$income),1,0)
dataset$income.2<-ifelse(dataset$income>=quantile(dataset$income)[2] & dataset$income<quantile(dataset$income)[3],1,0)
dataset$income.3<-ifelse(dataset$income>=quantile(dataset$income)[3] & dataset$income<quantile(dataset$income)[4],1,0)
dataset$income.4<-ifelse(dataset$income>=quantile(dataset$income)[4],1,0)


Alternative.Income.Measures<-cbind(c("Alternative Income Measures", "Income (proxy)", "Above-average (monetary) income",
"Second Income Quartile", "Third Income Quartile", "Fourth Income Quartile", 
"Continuous LAPOP Income Variable", "Perceived Relative Deprivation"),
rbind(c("", "", ""),
c(round(mean(dataset$incomeproxy, na.rm=T), digits=2), round(sd(dataset$incomeproxy, na.rm=T), digits=2), paste(paste(paste("[", round(range(dataset$incomeproxy, na.rm=T)[1], digits=2), sep=""), ", ", sep=""),paste(round(range(dataset$incomeproxy, na.rm=T)[2], digits=2), "]", sep=""))),
c(round(mean(dataset$income.abovemean, na.rm=T), digits=2), round(sd(dataset$income.abovemean, na.rm=T), digits=2), paste(paste(paste("[", round(range(dataset$income.abovemean, na.rm=T)[1], digits=2), sep=""), ", ", sep=""),paste(round(range(dataset$income.abovemean, na.rm=T)[2], digits=2), "]", sep=""))),
c(round(mean(dataset$income.2, na.rm=T), digits=2), round(sd(dataset$income.2, na.rm=T), digits=2), paste(paste(paste("[", round(range(dataset$income.2, na.rm=T)[1], digits=2), sep=""), ", ", sep=""),paste(round(range(dataset$income.2, na.rm=T)[2], digits=2), "]", sep=""))),
c(round(mean(dataset$income.3, na.rm=T), digits=2), round(sd(dataset$income.3, na.rm=T), digits=2), paste(paste(paste("[", round(range(dataset$income.3, na.rm=T)[1], digits=2), sep=""), ", ", sep=""),paste(round(range(dataset$income.3, na.rm=T)[2], digits=2), "]", sep=""))),
c(round(mean(dataset$income.4, na.rm=T), digits=2), round(sd(dataset$income.4, na.rm=T), digits=2), paste(paste(paste("[", round(range(dataset$income.4, na.rm=T)[1], digits=2), sep=""), ", ", sep=""),paste(round(range(dataset$income.4, na.rm=T)[2], digits=2), "]", sep=""))),
c(round(mean(dataset$income, na.rm=T), digits=2), round(sd(dataset$income, na.rm=T), digits=2), paste(paste(paste("[", round(range(dataset$income, na.rm=T)[1], digits=2), sep=""), ", ", sep=""),paste(round(range(dataset$income, na.rm=T)[2], digits=2), "]", sep=""))),
c(round(mean(dataset$intrapersonal.deprivation.longer.worse, na.rm=T), digits=2), round(sd(dataset$intrapersonal.deprivation.longer.worse, na.rm=T), digits=2), paste(paste(paste("[", round(range(dataset$intrapersonal.deprivation.longer.worse, na.rm=T)[1], digits=2), sep=""), ", ", sep=""),paste(round(range(dataset$intrapersonal.deprivation.longer.worse, na.rm=T)[2], digits=2), "]", sep="")))
)
)



Alternative.Ideological.Measures<-c(round(mean(dataset$government.support, na.rm=T), digits=2), round(sd(dataset$government.support, na.rm=T), digits=2), paste(paste(paste("[", round(range(dataset$government.support, na.rm=T)[1], digits=2), sep=""), ", ", sep=""),paste(round(range(dataset$government.support, na.rm=T)[2], digits=2), "]", sep="")))
rm(dataset)

load("dataset_2012_wpartysupport.RData")

Alternative.Ideological.Measures<-rbind(Alternative.Ideological.Measures,
c(round(mean(dataset$voteintention.incumbent, na.rm=T), digits=2), round(sd(dataset$voteintention.incumbent, na.rm=T), digits=2), paste(paste(paste("[", round(range(dataset$voteintention.incumbent, na.rm=T)[1], digits=2), sep=""), ", ", sep=""),paste(round(range(dataset$voteintention.incumbent, na.rm=T)[2], digits=2), "]", sep=""))),
c(round(mean(dataset$partisanship.incumbent, na.rm=T), digits=2), round(sd(dataset$partisanship.incumbent, na.rm=T), digits=2), paste(paste(paste("[", round(range(dataset$partisanship.incumbent, na.rm=T)[1], digits=2), sep=""), ", ", sep=""),paste(round(range(dataset$partisanship.incumbent, na.rm=T)[2], digits=2), "]", sep=""))))

Alternative.Ideological.Measures<-rbind(c("Alternative Measures of Ideological Preferences", "", "", "", ""),
	cbind(c("Government Support", "Prospective Vote for Incumbent", "Close to Incumbent Party"),
						Alternative.Ideological.Measures))
rownames(Alternative.Ideological.Measures)<-NULL
rm(dataset)

Table.A.3<-rbind(Main.Model,Alternative.Income.Measures,Alternative.Ideological.Measures)
colnames(Table.A.3)<-c("Variables", "Mean", "Std. Dev", "Range (in sample)")

print(Table.A.3)

g1 <- tableGrob(Main.Model, rows = NULL)
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g1), l = 1, r = ncol(g1))
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 1, l = 1, r = ncol(g1))

g2 <- tableGrob(Alternative.Income.Measures, rows = NULL)
g2 <- gtable_add_grob(g2,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g2), l = 1, r = ncol(g2))
g2 <- gtable_add_grob(g2,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 1, l = 1, r = ncol(g2))


g3 <- tableGrob(Alternative.Ideological.Measures, rows = NULL)
g3 <- gtable_add_grob(g3,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g3), l = 1, r = ncol(g3))
g3 <- gtable_add_grob(g3,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 1, l = 1, r = ncol(g3))


Titles<-matrix(c("Variables", "Mean", "Std. Dev", "Range"),nrow=1)

g0 <- tableGrob(Titles, rows = NULL)
g0 <- gtable_add_grob(g0,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g0), l = 1, r = ncol(g0))
g0 <- gtable_add_grob(g0,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 1, l = 1, r = ncol(g0))


g<-rbind(g0,g1,g2,g3)

pdf(file = "Table.A.3.pdf", height=10, width=10)
grid.draw(g)
dev.off()




############################################### SECTION A.3.1 ################################################################################
################################################# Figure A.1 ##################################################################################

rm(list=ls())
load("Country.Loadings.MainModel")


Country.Loadings$abbr<-NA
Country.Loadings$abbr[Country.Loadings$Country=="Argentina"]<-"ARG"
Country.Loadings$abbr[Country.Loadings$Country=="Brazil"]<-"BRA"
Country.Loadings$abbr[Country.Loadings$Country=="Chile"]<-"CHL"
Country.Loadings$abbr[Country.Loadings$Country=="Colombia"]<-"COL"
Country.Loadings$abbr[Country.Loadings$Country=="Costa Rica"]<-"CRI"
Country.Loadings$abbr[Country.Loadings$Country=="Dominican Republic"]<-"DOM"
Country.Loadings$abbr[Country.Loadings$Country=="Ecuador"]<-"ECU"
Country.Loadings$abbr[Country.Loadings$Country=="El Salvador"]<-"ESV"
Country.Loadings$abbr[Country.Loadings$Country=="Guatemala"]<-"GTM"
Country.Loadings$abbr[Country.Loadings$Country=="Honduras"]<-"HND"
Country.Loadings$abbr[Country.Loadings$Country=="Mexico"]<-"MEX"
Country.Loadings$abbr[Country.Loadings$Country=="Nicaragua"]<-"NIC"
Country.Loadings$abbr[Country.Loadings$Country=="Panama"]<-"PAN"
Country.Loadings$abbr[Country.Loadings$Country=="Paraguay"]<-"PAR"
Country.Loadings$abbr[Country.Loadings$Country=="Peru"]<-"PER"
Country.Loadings$abbr[Country.Loadings$Country=="Uruguay"]<-"URU"
Country.Loadings$abbr[Country.Loadings$Country=="Venezuela"]<-"VEN"



activities=c("Municipal meeting", "Contacting municipality", "Contacting local authority", 
		"Contacting national authority",
             "Solving Problems", "Improvements meeting", 
		 "Party meeting", "Voting",
             "Sharing online", "Petitioning", "Protesting", "Blocking")




for (k in 4:6) {
Country.Loadings[,k]<-as.numeric(levels(Country.Loadings[,k]))[Country.Loadings[,k]]
}



first<-c(8,1,2,3,4,6)

for (i in 1:length(activities)) {
a<-subset(Country.Loadings[as.character(Country.Loadings$Activity)==activities[i],])

if (i==4) {
title="Contacting nat. authority"
} else if (i==5) {
title="Solving comm. problems" 
} else {
title=activities[i]
}

if (i %in%first) {
m<-c(0.25,0.5,0,0)
} else m<-c(0.25,0.5,0,0)


if (i <12) {
 p=ggplot(a, aes(x=abbr, y=Mean, group=Dimension, color=Dimension))+
geom_point()+
geom_errorbar(aes(ymin=X95.Low,ymax=X95.High),width=0.1,
position=position_dodge(0.05))+
 ggtitle(title)+
theme_bw()+ylab("")+xlab("")+ 
theme(axis.text.x=element_text(angle=90,size=6))+ylim(c(0,4.5))+
theme(plot.margin=unit(m, "lines"))+
theme(axis.text.x = element_text(size=7, vjust=0.2))+
theme(legend.position="none")+theme(plot.title = element_text(size = 11))+
scale_color_manual(values=c("black", "gray"))+theme(plot.title = element_text(hjust = 0))
assign(paste("plot",i,sep=""),p)
} else if (i==12) {
 p=ggplot(a, aes(x=abbr, y=Mean, group=Dimension, color=Dimension))+
geom_point()+
geom_errorbar(aes(ymin=X95.Low,ymax=X95.High),width=0.1,
position=position_dodge(0.05))+
 ggtitle(title)+
theme_bw()+ylab("")+xlab("")+ 
theme(axis.text.x=element_text(angle=90,size=6))+ylim(c(0,4.5))+
theme(plot.margin=unit(m, "lines"))+
theme(axis.text.x = element_text(size=7, vjust=0.2))+
theme(plot.title = element_text(size = 11))+
scale_color_manual(values=c("black", "gray"))+theme(plot.title = element_text(hjust = 0))+
theme(legend.position="none")
assign(paste("plot",i,sep=""),p)
}
}



# Multiple plot function
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



pdf(file = "Figure.A.1.pdf")
grid.draw(multiplot(plot8, plot3, plot5, plot9,
   plot1, plot4, plot7, plot11,
   plot2, plot6, plot10, plot12,
	cols=3))
dev.off()

################################################# Figure A.2 ##################################################################################

rm(list=ls())
load("Alpha.all.MainModel")



df.figureA2<-data.frame(Alpha.all)



df.figureA2$abbr<-NA
df.figureA2$abbr[df.figureA2$Country=="Argentina"]<-"ARG"
df.figureA2$abbr[df.figureA2$Country=="Brazil"]<-"BRA"
df.figureA2$abbr[df.figureA2$Country=="Chile"]<-"CHL"
df.figureA2$abbr[df.figureA2$Country=="Colombia"]<-"COL"
df.figureA2$abbr[df.figureA2$Country=="Costa Rica"]<-"CRI"
df.figureA2$abbr[df.figureA2$Country=="Dominican Republic"]<-"DOM"
df.figureA2$abbr[df.figureA2$Country=="Ecuador"]<-"ECU"
df.figureA2$abbr[df.figureA2$Country=="El Salvador"]<-"ESV"
df.figureA2$abbr[df.figureA2$Country=="Guatemala"]<-"GTM"
df.figureA2$abbr[df.figureA2$Country=="Honduras"]<-"HND"
df.figureA2$abbr[df.figureA2$Country=="Mexico"]<-"MEX"
df.figureA2$abbr[df.figureA2$Country=="Nicaragua"]<-"NIC"
df.figureA2$abbr[df.figureA2$Country=="Panama"]<-"PAN"
df.figureA2$abbr[df.figureA2$Country=="Paraguay"]<-"PAR"
df.figureA2$abbr[df.figureA2$Country=="Peru"]<-"PER"
df.figureA2$abbr[df.figureA2$Country=="Uruguay"]<-"URU"
df.figureA2$abbr[df.figureA2$Country=="Venezuela"]<-"VEN"


load("Alpha.means.MainModel")

df.figureA2.2<-data.frame(Alpha.means)
df.figureA2.2$abbr<-"Average"

df.figureA2.3<-rbind(df.figureA2.2, df.figureA2[,-1])

df.figureA2.3$abbr<-factor(df.figureA2.3$abbr, 
levels=c(unique(df.figureA2$abbr), "Average"))

df.figureA2.3$Activity<-factor(df.figureA2.3$Activity,
levels=c("Voting", "Municipal meeting",
	"Contacting municipality", "Contacting local authority",
	"Contacting national authority",
	"Improvements meeting", "Solving Problems",
	"Party meeting",
	"Petitioning",  "Sharing online", "Protesting", "Blocking"),
labels=c("Voting", "Municipal meeting",
	"Contacting municipality", "Contacting local authority",
	"Contacting nat. authority",
	"Improvements meeting", "Solving comm. problems",
	"Party meeting", 
	"Petitioning", "Sharing online", "Protesting", "Blocking"))


for (k in 2:4){

df.figureA2.3[,k]<-as.numeric(levels(df.figureA2.3[,k]))[df.figureA2.3[,k]]
}

p<-ggplot(df.figureA2.3, aes(x=abbr, y=Mean, colour=abbr, fill==abbr))+
geom_point()+geom_errorbar(aes(ymin=X95.Low,ymax=X95.High),width=0.1,
position=position_dodge(0.05))+
facet_wrap(~Activity)+theme_bw()+
theme(axis.text.x = element_text(size=6, angle=90, vjust=0.2))+
theme(strip.text = element_text(size = 8))+
scale_color_manual(values=c(rep("gray",17),"black"))+
xlab("")+ ylab("")+theme(legend.position = "none") 


pdf(file = "Figure.A.2.pdf")
grid.draw(p)
dev.off()


################################################# Figure A.3 ##################################################################################

rm(list=ls())

load("Posterior.Classification.Conv.High.MainModel")
load("Posterior.Classification.Conv.Low.MainModel")

Conv.High<-cbind(c(1,2), Posterior.Classification.Conv.High, "Conventional Dimension", "Classified as High Type")
colnames(Conv.High)<-c("Score",  "Mean", "Low", "High", "Dimension", "Classification")


Conv.Low<-cbind(c(1,2), Posterior.Classification.Conv.Low, "Conventional Dimension", "Classified as Low Type")
colnames(Conv.Low)<-c("Score",  "Mean", "Low", "High", "Dimension", "Classification")

load("Posterior.Classification.Unconv.High.MainModel")
load("Posterior.Classification.Unconv.Low.MainModel")

Unconv.High<-cbind(c(1,2), Posterior.Classification.Unconv.High, "Unonventional Dimension", "Classified as High Type")
colnames(Unconv.High)<-c("Score",  "Mean", "Low", "High", "Dimension", "Classification")


Unconv.Low<-cbind(c(1,2),Posterior.Classification.Unconv.Low, "Unonventional Dimension", "Classified as Low Type")
colnames(Unconv.Low)<-c("Score",  "Mean", "Low", "High", "Dimension", "Classification")


Figure.A.3<-data.frame(rbind(Conv.High, Conv.Low, Unconv.High, Unconv.Low))

for (k in 1:4) {
Figure.A.3[,k]<-as.numeric(levels(Figure.A.3[,k]))[Figure.A.3[,k]] 
}

Figure.A.3$Classification<-factor(Figure.A.3$Classification,
			levels=c("Classified as High Type", 
				"Classified as Low Type"))


p<-ggplot(Figure.A.3, aes(x=Score, y=Mean)) +geom_point()+
geom_errorbar(aes(ymin=Low, ymax=High), width=.1)+
  facet_grid(Classification~Dimension)+theme_bw()+
scale_x_continuous(breaks=c(1,2))+ylab("")+
theme(strip.text.x=element_text(size=12, face="bold"))+
theme(strip.text.y=element_text(size=12, face="bold"))+
xlab(expression(paste(T[c],"                                                                              ", T[u], sep="     ")))


pdf(file = "Figure.A.3.pdf")
grid.draw(p)
dev.off()


################################################# Figure A.4 ##################################################################################

rm(list=ls())
load("Posterior.Probs.MainModel")

Distributions<-Posterior.Probs.MainModel


Distributions<-rbind(cbind("Outsider", Distributions[,1]), 
cbind("Agitator", Distributions[,2]),
cbind("Conventional", Distributions[,3]),
cbind("Activist", Distributions[,4]))


Distributions<-data.frame(Distributions)
names(Distributions)<-c("Type", "Density")



for (k in 2:ncol(Distributions)) {
Distributions[,k]<- as.numeric(levels(Distributions[,k]))[Distributions[,k]] 
}


p<-ggplot(Distributions, aes(x=Density, color=Type, fill=Type)) +
  geom_density()+ scale_fill_grey()+theme_bw()+
 ylab("")+xlab("Posterior probabilities of type assignment")+ theme(legend.position="bottom")+
 theme(legend.title = element_blank()) +
guides(fill=guide_legend(nrow=2,byrow=TRUE))+
scale_x_continuous(lim=c(0,1))+
theme(axis.text.y = element_blank())+
theme(legend.text = element_text(size=10), legend.title = element_text(size=10, colour="white"), legend.position="bottom", legend.title.align=0.5)+
theme(axis.title.x = element_text(size=13, face="bold"))+
theme(axis.text.x = element_text(size=11))+
theme(axis.ticks.y =element_blank())

pdf(file = "Figure.A.4.pdf")
grid.draw(p)
dev.off()


################################################# Table A.4 ###################################################################################

rm(list=ls())
load("Outsider.by.Country.MainModel")
load("Agitator.by.Country.MainModel")
load("Activist.by.Country.MainModel")
load("Conventional.by.Country.MainModel")


Table.A.4<-matrix(0, nrow=17, ncol=9)
Table.A.4[,1]<-c("ARG", "BRA", "CHL", "COL", "CRI", "DOM",
	"ECU", "ESV", "GTM", "HND", "MEX", "NIC", "PAN", 
	"PAR", "PER", "URU", "VEN")


for (j in 1:17) {

m.o<-round(100*Outsider.by.Country[j,2], digits=2)
ql.o<-round(100*Outsider.by.Country[j,3], digits=2)
qh.o<-round(100*Outsider.by.Country[j,4], digits=2)

m.ag<-round(100*Agitator.by.Country[j,2], digits=2)
ql.ag<-round(100*Agitator.by.Country[j,3], digits=2)
qh.ag<-round(100*Agitator.by.Country[j,4], digits=2)

m.c<-round(100*Conventional.by.Country[j,2], digits=2)
ql.c<-round(100*Conventional.by.Country[j,3], digits=2)
qh.c<-round(100*Conventional.by.Country[j,4], digits=2)


m.ac<-round(100*Activist.by.Country[j,2], digits=2)
ql.ac<-round(100*Activist.by.Country[j,3], digits=2)
qh.ac<-round(100*Activist.by.Country[j,4], digits=2)

if (nchar(m.o)<5) {
while (nchar(m.o)<5) {
m.o<-paste(m.o, "0", sep="")
} 
} 



if (nchar(ql.o)<5) {
while (nchar(ql.o)<5) {
ql.o<-paste(ql.o, "0", sep="")
} 
}

if (nchar(qh.o)<5) {
while (nchar(qh.o)<5) {
qh.o<-paste(qh.o, "0", sep="")
} 
}

 
if (nchar(m.ag)<4) {
while (nchar(m.ag)<4) {
m.ag<-paste(m.ag, "0", sep="")
}
}


if (nchar(ql.ag)<4) {
while (nchar(ql.ag)<4) {
ql.ag<-paste(ql.ag, "0", sep="")
} 
}

if (nchar(qh.ag)<4) {
while (nchar(qh.ag)<4) {
qh.ag<-paste(qh.ag, "0", sep="")
} 
}



if (nchar(m.c)<5) {
while (nchar(m.c)<5) {
m.c<-paste(m.c, "0", sep="")
} 
}

if (nchar(ql.c)<5) {
while (nchar(ql.c)<5) {
ql.c<-paste(ql.c, "0", sep="")
} 
}


if (nchar(qh.c)<5) {
while (nchar(qh.c)<5) {
qh.c<-paste(qh.c, "0", sep="")
} 
}


if (nchar(m.ac)<4) {
while (nchar(m.ac)<4) {
m.ac<-paste(m.ac, "0", sep="")
} 
}

if (nchar(ql.ac)<4) {
while (nchar(ql.ac)<4) {
ql.ac<-paste(ql.ac, "0", sep="")
} 
}

if (nchar(qh.ac)<4) {
while (nchar(qh.ac)<4) {
qh.ac<-paste(qh.ac, "0", sep="")
} 
}

Table.A.4[j,2:3]<-c(m.o, paste(paste("(", ql.o, ",", sep=""), paste(qh.o,")", sep=""),sep=""))
Table.A.4[j,4:5]<-c(m.ag, paste(paste("(", ql.ag, ",", sep=""), paste(qh.ag,")", sep=""),sep=""))
Table.A.4[j,8:9]<-c(m.ac, paste(paste("(", ql.ac, ",", sep=""), paste(qh.ac,")", sep=""),sep=""))
Table.A.4[j,6:7]<-c(m.c, paste(paste("(", ql.c, ",", sep=""), paste(qh.c,")", sep=""),sep=""))

}

colnames(Table.A.4)<-c("Country", "Mean", "Interval", "Mean", "Interval", "Mean", "Interval", "Mean", "Interval")

print(Table.A.4)



Titles<-matrix(c("", "Prob.", "Outsider", "Prob.", "Agitator", "Prob.", "Conventional", "Prob.", "Activist"),nrow=1)




g1 <- tableGrob(Table.A.4, rows = NULL)
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g1), l = 1, r = ncol(g1))
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g1), l = 1, r = 1)
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g1), l = 1, r = 3)
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g1), l = 1, r = 5)
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g1), l = 1, r = 7)
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g1), l = 1, r = 9)
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 1, l = 1, r = ncol(g1))



g0 <- tableGrob(Titles, rows = NULL)
g0 <- gtable_add_grob(g0,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g0), l = 1, r = 3)
g0 <- gtable_add_grob(g0,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g0), l = 1, r = 1)
g0 <- gtable_add_grob(g0,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g0), l = 1, r = 5)
g0 <- gtable_add_grob(g0,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g0), l = 1, r = 7)
g0 <- gtable_add_grob(g0,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g0), l = 1, r = 9)
g0 <- gtable_add_grob(g0,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 1, l = 1, r = ncol(g0))

g<-rbind(g0, g1)

pdf(file = "Table.A.4.pdf", height=10, width=10)
grid.draw(g)
dev.off()


################################################# Table A.5 ###################################################################################

rm(list=ls())
load("Posterior.Conv.Country.MainModel")
load("Posterior.Unconv.Country.MainModel")

AvePP<-c()

for (j in 1:17) {
dd.conv<-Posterior.Conv.Country[Posterior.Conv.Country[,1]==j,]
dd.unconv<-Posterior.Unconv.Country[Posterior.Unconv.Country[,1]==j,]

m.c<-round(mean(c(dd.conv[dd.conv[,2]==1,3], dd.conv[dd.conv[,2]==2,4])), digits=2)
m.u<-round(mean(c(dd.unconv[dd.unconv[,2]==1,3], dd.unconv[dd.unconv[,2]==2,4])), digits=2)
m.b<-round(mean(c(dd.conv[dd.conv[,2]==1,3], dd.conv[dd.conv[,2]==2,4], dd.unconv[dd.unconv[,2]==1,3], dd.unconv[dd.unconv[,2]==2,4])), digits=2)

if (nchar(m.c)<4 & m.c<1) {
while (nchar(m.c)<4) {
m.c<-paste(m.c, "0", sep="")
} 
}

if (m.c==1) {
m.c<-"1.00"
} 

if (nchar(m.u)<4 & m.u<1) {
while (nchar(m.u)<4) {
m.u<-paste(m.u, "0", sep="")
} 
}

if (m.u==1) {
m.u<-"1.00"
} 


if (nchar(m.b)<4 &  m.b<1) {
while (nchar(m.b)<4) {
m.b<-paste(m.b, "0", sep="")
}
} 

if (m.b==1) {
m.b<-"1.00"
} 
AvePP<-rbind(AvePP, c(j, m.c, m.u, m.b))
}

AvePP<-data.frame(AvePP)
names(AvePP)<-c("Country.Name", "Tc", "Tu", "Tc & Tu")

AvePP$abbr<-NA
AvePP$abbr[AvePP$Country==1]<-"ARG"
AvePP$abbr[AvePP$Country==2]<-"BRA"
AvePP$abbr[AvePP$Country==3]<-"CHL"
AvePP$abbr[AvePP$Country==4]<-"COL"
AvePP$abbr[AvePP$Country==5]<-"CRI"
AvePP$abbr[AvePP$Country==6]<-"DOM"
AvePP$abbr[AvePP$Country==7]<-"ECU"
AvePP$abbr[AvePP$Country==8]<-"ESV"
AvePP$abbr[AvePP$Country==9]<-"GTM"
AvePP$abbr[AvePP$Country==10]<-"HND"
AvePP$abbr[AvePP$Country==11]<-"MEX"
AvePP$abbr[AvePP$Country==12]<-"NIC"
AvePP$abbr[AvePP$Country==13]<-"PAN"
AvePP$abbr[AvePP$Country==14]<-"PAR"
AvePP$abbr[AvePP$Country==15]<-"PER"
AvePP$abbr[AvePP$Country==16]<-"URU"
AvePP$abbr[AvePP$Country==17]<-"VEN"


Table.A.5<-AvePP[,c(5,2:4)]
names(Table.A.5)<-c("Country.Name", "Tc", "Tu", "Tc & Tu")

print(Table.A.5)

Titles<-matrix(c("", "", "AvePP", ""),nrow=1)


g1 <- tableGrob(Table.A.5, rows = NULL)
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
       t = 2, b = nrow(g1), l = 1, r = ncol(g1))
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 1, l = 1, r = ncol(g1))



g0 <- tableGrob(Titles, rows = NULL)
g0 <- gtable_add_grob(g0,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 1, l = 1, r = ncol(g0))

g<-rbind(g0,g1)

pdf(file = "Table.A.5.pdf", height=10, width=10)
grid.draw(g)
dev.off()

################################################# Figure A.5 ##################################################################################

rm(list=ls())
load("dataset_2012.RData")


df<-subset(dataset, select=c("country.name", "ActMeetMun",
"country.name", "ActMeetMun", "ActContMun",  "ActContAut" ,                           
"ActMeetImp" , "ActSolveProb" ,  "ActMeetPty" ,  "ActContGvt",                            
"ActProtest" , "ActBlock" ,  "ActVoted",  "ActSign" ,                              
"ActShare" , 
"compulsory.voting", "gdp.pc", "rule.of.lw", "enp", "social.spending"))                       
df<-na.exclude(df)



Y <- cbind(df$ActMeetMun, df$ActContMun, df$ActContAut,
		 df$ActContGvt, df$ActSolveProb, 
		df$ActMeetImp, df$ActMeetPty, df$ActVoted, 
		df$ActShare, df$ActSign, df$ActProtest, df$ActBlock)
colnames(Y)<-c("ActMeetMun", "ActContMun", "ActContAut", "ActContGvt",
		"ActSolveProb", "ActMeetImp", "ActMeetPty", "ActVoted",
		"ActShare", "ActSign", "ActProtest", "ActBlock")

Country<-as.numeric(factor(df$country.name), levels=unique(df$country.name))
		

Matrix.Country<-matrix(0,nrow(Y),length(unique(Country)))           
for (i in 1:length(unique(Country))) Matrix.Country[Country==i,i]<-1



X<-cbind(1, (df$rule.of.lw-mean(df$rule.of.lw))/(2*sd(df$rule.of.lw)),
		df$compulsory.voting,
		(df$enp-mean(df$enp))/(2*sd(df$enp)),
		(df$gdp.pc-mean(df$gdp.pc))/(2*sd(df$gdp.pc)),
		(df$social.spending-mean(df$social.spending))/(2*sd(df$social.spending))
)


load("Results_MacroModel")

# Convergence Checks for Macro Model (only country-level variables)

Alpha<-mcmc.list(as.mcmc(t(Results$alpha[,,1])), 
		as.mcmc(t(Results$alpha[,,2])),
		as.mcmc(t(Results$alpha[,,3])))
print(paste("Convergence Alpha:", gelman.diag(Alpha)[[2]]<1.2, sep=" "))


Alpha.Conv<-mcmc.list(as.mcmc(t(Results$alpha_conv[-seq(12,dim(Results$alpha_conv)[1],by=12),,1])), 
		as.mcmc(t(Results$alpha_conv[-seq(12,dim(Results$alpha_conv)[1],by=12),,2])),
		as.mcmc(t(Results$alpha_conv[-seq(12,dim(Results$alpha_conv)[1],by=12),,3])))
print(paste("Convergence Alpha Conv:", gelman.diag(Alpha.Conv)[[2]]<1.2, sep=" "))


Alpha.Unconv<-mcmc.list(as.mcmc(t(Results$alpha_unconv[-seq(1,dim(Results$alpha_unconv)[1],by=12),,1])), 
		as.mcmc(t(Results$alpha_unconv[-seq(1,dim(Results$alpha_unconv)[1],by=12),,2])),
		as.mcmc(t(Results$alpha_unconv[-seq(1,dim(Results$alpha_unconv)[1],by=12),,3])))
print(paste("Convergence Alpha Unconventional:", gelman.diag(Alpha.Unconv)[[2]]<1.2, sep=" "))



P.Conv<-mcmc.list(as.mcmc((Results$P_conv[2,,1])), 
		as.mcmc((Results$P_conv[2,,2])),
		as.mcmc((Results$P_conv[2,,3])))
print(paste("Convergence Prob. Conventional:", gelman.diag(P.Conv)[[1]][,1]<1.2, sep=" "))


P.Unconv<-mcmc.list(as.mcmc((Results$P_unconv[2,,1])), 
		as.mcmc((Results$P_unconv[2,,2])),
		as.mcmc((Results$P_unconv[2,,3])))
print(paste("Convergence Prob. Unconventional", gelman.diag(P.Unconv)[[1]][,1]<1.2, sep=" "))


Beta2<-mcmc.list(as.mcmc(t(Results$beta2[,,1])),
	as.mcmc(t(Results$beta2[,,2])),
	as.mcmc(t(Results$beta2[,,3])))

Beta3<-mcmc.list(as.mcmc(t(Results$beta3[,,1])),
	as.mcmc(t(Results$beta3[,,2])),
	as.mcmc(t(Results$beta3[,,3])))
Beta4<-mcmc.list(as.mcmc(t(Results$beta4[,,1])),
	as.mcmc(t(Results$beta4[,,2])),
	as.mcmc(t(Results$beta4[,,3])))
print(paste("Convergence Regression coefficients:", gelman.diag(Beta2)[[2]]<1.2 & gelman.diag(Beta3)[[2]]<1.2 & gelman.diag(Beta4)[[2]]<1.2, sep=" "))



BETA2<-rbind((t(Results$beta2[,,1])),
	(t(Results$beta2[,,2])),
	(t(Results$beta2[,,3])))

BETA3<-rbind((t(Results$beta3[,,1])),
	(t(Results$beta3[,,2])),
	(t(Results$beta3[,,3])))

BETA4<-rbind((t(Results$beta4[,,1])),
	(t(Results$beta4[,,2])),
	(t(Results$beta4[,,3])))

NU<-rbind((t(Results$nu[,,1])),
	(t(Results$nu[,,2])),
	(t(Results$nu[,,3])))

NU2<-NU[, 1:length(unique(Country))]
NU3<-NU[, (1+length(unique(Country))): (2*length(unique(Country)))]
NU4<-NU[, (1+2*length(unique(Country))): (3*length(unique(Country)))]

Beta.Agitator<-BETA2[seq(1, nrow(BETA2), length.out=1000),]
Beta.Conventional<-BETA3[seq(1, nrow(BETA3), length.out=1000),]
Beta.Activist<-BETA4[seq(1, nrow(BETA4), length.out=1000),]
Nu.Agitator<-NU2[seq(1, nrow(NU2), length.out=1000),]
Nu.Conventional<-NU3[seq(1, nrow(NU3), length.out=1000),]
Nu.Activist<-NU4[seq(1, nrow(NU4), length.out=1000),]

# Marginal Covariate Effects


colnames(X)<-c("Intercept", 
		 "Rule of Law", "Compulsory Voting", "ENPP",
		"GDP", "Social.Spending")


## Contextual

continuous<-c(2, 4:6)
ordinal<-3


Marginal.Contextual<-matrix(0, nrow=4*(length(continuous)+length(ordinal)), 3)
colnames(Marginal.Contextual)<-c("Mean", "95.Low", "95.High")
rownames(Marginal.Contextual)<-paste(rep(colnames(X)[sort(c(continuous, ordinal))], each=4),
rep(c("Outsider", "Agitator", "Conventional", "Activist"), (length(continuous)+length(ordinal))), 
sep="-")



for (j in 1:length(continuous)){
X.0<-X
X.1<-X
X.1[,continuous[j]]<-X.0[,continuous[j]]+sd(X.0[,continuous[j]])


prob.Agitator.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Outsider.0<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Agitator.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))


prob.Outsider.1<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))



Marginal.Contextual[grep(colnames(X)[continuous[j]], rownames(Marginal.Contextual))[1],]<-c(mean(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0)),
											quantile(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0), c(0.025,0.975)))

Marginal.Contextual[grep(colnames(X)[continuous[j]], rownames(Marginal.Contextual))[2],]<-c(mean(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0)), quantile(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0), c(0.025,0.975)))


Marginal.Contextual[grep(colnames(X)[continuous[j]], rownames(Marginal.Contextual))[3],]<-c(mean(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0)), quantile(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0), c(0.025,0.975)))


Marginal.Contextual[grep(colnames(X)[continuous[j]], rownames(Marginal.Contextual))[4],]<-c(mean(colMeans(prob.Activist.1)-colMeans(prob.Activist.0)), quantile(colMeans(prob.Activist.1)-colMeans(prob.Activist.0), c(0.025,0.975)))

}

for (j in 1:length(ordinal)){
X.0<-X
X.1<-X
X.1[,ordinal[j]]<-X.0[,ordinal[j]]+1


prob.Agitator.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Outsider.0<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Agitator.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))


prob.Outsider.1<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))



Marginal.Contextual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Contextual))[1],]<-c(mean(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0)),
											quantile(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0), c(0.025,0.975)))

Marginal.Contextual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Contextual))[2],]<-c(mean(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0)), quantile(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0), c(0.025,0.975)))


Marginal.Contextual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Contextual))[3],]<-c(mean(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0)), quantile(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0), c(0.025,0.975)))


Marginal.Contextual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Contextual))[4],]<-c(mean(colMeans(prob.Activist.1)-colMeans(prob.Activist.0)), quantile(colMeans(prob.Activist.1)-colMeans(prob.Activist.0), c(0.025,0.975)))

}

Marginal.Contextual[,1:3]<-100*Marginal.Contextual[,1:3]



Figure.Contextual<-cbind(unlist(strsplit(rownames(Marginal.Contextual), "-"))[seq(2,length(unlist(strsplit(rownames(Marginal.Contextual), "-"))), by=2)],
unlist(strsplit(rownames(Marginal.Contextual), "-"))[seq(1,length(unlist(strsplit(rownames(Marginal.Contextual), "-"))), by=2)],
Marginal.Contextual[,1:3])
Figure.Contextual<-data.frame(Figure.Contextual)
names(Figure.Contextual)<-c("Type", "Variable", "Mean", "Low", "High")

Figure.A.5<-Figure.Contextual

Figure.A.5$Type<- factor(Figure.A.5$Type, 
	levels = c("Outsider",
		"Agitator",
		"Conventional", "Activist"))



Figure.A.5$Variable<-factor(Figure.A.5$Variable, 
	levels=rev(c("Rule of Law", "Compulsory Voting", "ENPP", 
			"GDP", "Social.Spending")),
       labels=rev(c("Rule of Law",
			  "Compulsory Voting",
			  "ENPP", 
			  "GDP per capita",
			  "Social Spending")))

for (k in 3:5){
Figure.A.5[,k]<-as.numeric(levels(Figure.A.5[,k]))[Figure.A.5[,k]]
}


p<-ggplot(Figure.A.5,
 aes(y=Variable, x=Mean)) + 
    geom_errorbarh(aes(xmin=Low, 
	xmax=High, height=.2), colour="black") + facet_wrap(~Type)+
    geom_point(size=1.5)+
theme_bw()+xlab("Marginal effects (in percentage points)")+ylab("")+
geom_hline(yintercept=0, linetype="longdash", colour="gray40")+
theme(strip.text.x = element_text(size = 12, face="bold"))+
geom_vline(xintercept=0, linetype="longdash", colour="gray40")+
theme(strip.background = element_rect(fill = alpha('white'))) +
theme(axis.title.x=element_text(vjust=2, size=11))+
theme(axis.text.y=element_text(size=11))

pdf(file = "Figure.A.5.pdf")
grid.draw(p)
dev.off()

################################################# Figure A.6 ##################################################################################

rm(list=ls())
load("Marginal.Country.MainModel")
Figure.A.6<-cbind(unlist(strsplit(rownames(Marginal.Country), "-"))[seq(2,length(unlist(strsplit(rownames(Marginal.Country), "-"))), by=2)],
unlist(strsplit(rownames(Marginal.Country), "-"))[seq(1,length(unlist(strsplit(rownames(Marginal.Country), "-"))), by=2)],
Marginal.Country[,1:3])
Figure.A.6<-data.frame(Figure.A.6)
names(Figure.A.6)<-c("Type", "Country", "Mean", "Low", "High")



Figure.A.6$Type<- factor(Figure.A.6$Type, 
	levels = c("Outsider",
		"Agitator",
		"Conventional", "Activist"))


Figure.A.6$Country<- factor(Figure.A.6$Country, 
	levels=rev(c("Argentina", "Brazil", "Chile", "Colombia", "Costa Rica", "Dominican Republic",
		"Ecuador", "El Salvador", "Guatemala", "Honduras", "Mexico", "Nicaragua", "Panama",
		"Paraguay", "Peru", "Uruguay", "Venezuela")),
        labels=rev(c("ARG", "BRA", "CHL", "COL", "CRI", "DOM", "ECU", "ESV", "GTM", "HND", "MEX", "NIC",
		"PAN", "PAR", "PER", "URU", "VEN")))



for (k in 3:5){

Figure.A.6[,k]<-as.numeric(levels(Figure.A.6[,k]))[Figure.A.6[,k]]
}


p<-ggplot(Figure.A.6,
 aes(y=Country, x=Mean)) + 
    geom_errorbarh(aes(xmin=Low, 
	xmax=High, height=.2), colour="black") + facet_wrap(~Type, scales="free_x")+
    geom_point(size=1.5)+
theme_bw()+xlab("Marginal effects (in percentage points)")+ylab("")+
geom_hline(yintercept=0, linetype="longdash", colour="gray40")+
theme(strip.text.x = element_text(size = 12, face="bold"))+
geom_vline(xintercept=0, linetype="longdash", colour="gray40")+
theme(strip.background = element_rect(fill = alpha('white'))) +
theme(axis.title.x=element_text(vjust=2, size=11))+
theme(axis.text.y=element_text(size=11))




pdf(file = "Figure.A.6.pdf")
grid.draw(p)
dev.off()

################################################# Table A.6 ##################################################################################
rm(list=ls())

load("dataset_2012.RData")

df<-subset(dataset, select=c("country.name", "ActMeetMun",
"country.name", "ActMeetMun", "ActContMun",  "ActContAut" ,                           
"ActMeetImp" , "ActSolveProb" ,  "ActMeetPty" ,  "ActContGvt",                            
"ActProtest" , "ActBlock" ,  "ActVoted",  "ActSign" ,                              
"ActShare" , "education" ,  "female", "age", "relative.incomeproxy" ,
"victim", "corrupt", "spatial.distance" ,
"compulsory.voting", "gdp.pc", "rule.of.lw", "enp", "social.spending"))                       
df<-na.exclude(df)


Y <- cbind(df$ActMeetMun, df$ActContMun, df$ActContAut,
		 df$ActContGvt, df$ActSolveProb, 
		df$ActMeetImp, df$ActMeetPty, df$ActVoted, 
		df$ActShare, df$ActSign, df$ActProtest, df$ActBlock)
colnames(Y)<-c("ActMeetMun", "ActContMun", "ActContAut", "ActContGvt",
		"ActSolveProb", "ActMeetImp", "ActMeetPty", "ActVoted",
		"ActShare", "ActSign", "ActProtest", "ActBlock")

Country<-as.numeric(factor(df$country.name), levels=unique(df$country.name))
		

Matrix.Country<-matrix(0,nrow(Y),length(unique(Country)))           
for (i in 1:length(unique(Country))) Matrix.Country[Country==i,i]<-1

MatriX.Country<-Matrix.Country

df$age.centered<-(df$age-mean(df$age))/(2*sd(df$age))
df$education.centered<-(df$education-mean(df$education))/(2*sd(df$education))


df$relative.incomeproxy.centered<-(df$relative.incomeproxy-mean(df$relative.incomeproxy))/(2*sd(df$relative.incomeproxy))


df$spatial.distance.centered<-(df$spatial.distance-mean(df$spatial.distance))/(2*sd(df$spatial.distance))

X<-cbind(1, df$age.centered, df$female, df$education.centered, 
	df$relative.incomeproxy.centered, 
	df$victim, df$corrupt, 
	df$spatial.distance.centered,
	(df$rule.of.lw-mean(df$rule.of.lw))/(2*sd(df$rule.of.lw)),
		df$compulsory.voting,
		(df$enp-mean(df$enp))/(2*sd(df$enp)),
		(df$gdp.pc-mean(df$gdp.pc))/(2*sd(df$gdp.pc)),
		(df$social.spending-mean(df$social.spending))/(2*sd(df$social.spending))
)


load("Results_MainModel")



Alpha.Baseline<-rbind(t(Results$alpha[,,1]), t(Results$alpha[,,2]),
		t(Results$alpha[,,3]))
Alpha.Baseline.Means<-matrix(colMeans(Alpha.Baseline), ncol(Y), ncol(Matrix.Country))


Alpha.Conv.Baseline<-rbind(t(Results$alpha_conv[,,1]), t(Results$alpha_conv[,,2]),
		t(Results$alpha_conv[,,3]))
Alpha.Conv.Baseline.Means<-matrix(colMeans(Alpha.Conv.Baseline), ncol(Y), ncol(Matrix.Country))

Alpha.Unconv.Baseline<-rbind(t(Results$alpha_unconv[,,1]), t(Results$alpha_unconv[,,2]),
		t(Results$alpha_unconv[,,3]))
Alpha.Unconv.Baseline.Means<-matrix(colMeans(Alpha.Unconv.Baseline), ncol(Y), ncol(Matrix.Country))

P.Conv.Baseline<-rbind(t(Results$P_conv[,,1]), 
		t(Results$P_conv[,,2]),
		t(Results$P_conv[,,3]))

P.Unconv.Baseline<-rbind(t(Results$P_unconv[,,1]),  
		t(Results$P_unconv[,,2]),
		t(Results$P_unconv[,,3]))



Beta2.Baseline<-rbind((t(Results$beta2[,,1])),
	(t(Results$beta2[,,2])),
	(t(Results$beta2[,,3])))

Beta3.Baseline<-rbind((t(Results$beta3[,,1])),
	(t(Results$beta3[,,2])),
	(t(Results$beta3[,,3])))


Beta4.Baseline<-rbind((t(Results$beta4[,,1])),
	(t(Results$beta4[,,2])),
	(t(Results$beta4[,,3])))



NU<-rbind((t(Results$nu[,,1])),
	(t(Results$nu[,,2])),
	(t(Results$nu[,,3])))


Nu2.Baseline<-NU[, 1:length(unique(Country))]
Nu3.Baseline<-NU[, (1+length(unique(Country))): (2*length(unique(Country)))]
Nu4.Baseline<-NU[, (1+2*length(unique(Country))): (3*length(unique(Country)))]



load("Posterior.Probs.MainModel")

set.seed(3001)
y<-rMultinom(Posterior.Probs.MainModel,1)


Baseline<-sum(log(mean(P.Conv.Baseline[,1])*mean(P.Unconv.Baseline[,1])* apply(sapply(1:ncol(Y),function(l) (pnorm((Matrix.Country%*%t(Alpha.Baseline.Means))[,l])^Y[,l])*((1-pnorm((Matrix.Country%*%t(Alpha.Baseline.Means))[,l]))^(1-Y[,l]))),1,prod)+
mean(P.Conv.Baseline[,2])*mean(P.Unconv.Baseline[,1])* apply(sapply(1:ncol(Y),function(l) (pnorm((Matrix.Country%*%t(Alpha.Baseline.Means))[,l]+(Matrix.Country%*%t(Alpha.Conv.Baseline.Means))[,l])^Y[,l])*((1-pnorm((Matrix.Country%*%t(Alpha.Baseline.Means))[,l]+(Matrix.Country%*%t(Alpha.Conv.Baseline.Means))[,l]))^(1-Y[,l]))),1,prod)+
mean(P.Conv.Baseline[,1])*mean(P.Unconv.Baseline[,2])* apply(sapply(1:ncol(Y),function(l) (pnorm((Matrix.Country%*%t(Alpha.Baseline.Means))[,l]+(Matrix.Country%*%t(Alpha.Unconv.Baseline.Means))[,l])^Y[,l])*((1-pnorm((Matrix.Country%*%t(Alpha.Baseline.Means))[,l]+(Matrix.Country%*%t(Alpha.Unconv.Baseline.Means))[,l]))^(1-Y[,l]))),1,prod)+
mean(P.Conv.Baseline[,2])*mean(P.Unconv.Baseline[,2])* apply(sapply(1:ncol(Y),function(l) (pnorm((Matrix.Country%*%t(Alpha.Baseline.Means))[,l]+(Matrix.Country%*%t(Alpha.Conv.Baseline.Means))[,l]+(Matrix.Country%*%t(Alpha.Unconv.Baseline.Means))[,l])^Y[,l])*((1-pnorm((Matrix.Country%*%t(Alpha.Baseline.Means))[,l]+(Matrix.Country%*%t(Alpha.Conv.Baseline.Means))[,l]+(Matrix.Country%*%t(Alpha.Unconv.Baseline.Means))[,l]))^(1-Y[,l]))),1,prod)))+
sum((as.vector(y)==2)*log(exp(X%*%colMeans(Beta2.Baseline)+colMeans(Nu2.Baseline)[Country])/(1+exp(X%*%colMeans(Beta2.Baseline)+colMeans(Nu2.Baseline)[Country])+exp(X%*%colMeans(Beta3.Baseline)+colMeans(Nu3.Baseline)[Country])+exp(X%*%colMeans(Beta4.Baseline)+colMeans(Nu4.Baseline)[Country])))+
(as.vector(y)==3)*log(exp(X%*%colMeans(Beta3.Baseline)+colMeans(Nu3.Baseline)[Country])/(1+exp(X%*%colMeans(Beta2.Baseline)+colMeans(Nu2.Baseline)[Country])+exp(X%*%colMeans(Beta3.Baseline)+colMeans(Nu3.Baseline)[Country])+exp(X%*%colMeans(Beta4.Baseline)+colMeans(Nu4.Baseline)[Country])))+
(as.vector(y)==4)*log(exp(X%*%colMeans(Beta4.Baseline)+colMeans(Nu4.Baseline)[Country])/(1+exp(X%*%colMeans(Beta2.Baseline)+colMeans(Nu2.Baseline)[Country])+exp(X%*%colMeans(Beta3.Baseline)+colMeans(Nu3.Baseline)[Country])+exp(X%*%colMeans(Beta4.Baseline)+colMeans(Nu4.Baseline)[Country])))+
(as.vector(y)==1)*log(1/(1+exp(X%*%colMeans(Beta2.Baseline)+colMeans(Nu2.Baseline)[Country])+exp(X%*%colMeans(Beta3.Baseline)+colMeans(Nu3.Baseline)[Country])+exp(X%*%colMeans(Beta4.Baseline)+colMeans(Nu4.Baseline)[Country]))))

rm(Results)

### Individual vars
X.individualvars<-cbind(1, df$age.centered, df$female, df$education.centered, 
	df$relative.incomeproxy.centered, 
	df$victim, df$corrupt, 
	df$spatial.distance.centered)

X<-X.individualvars

load("Results_MicroModel")


Alpha.Individualvars<-rbind(t(Results$alpha[,,1]), t(Results$alpha[,,2]),
		t(Results$alpha[,,3]))
Alpha.Individualvars.Means<-matrix(colMeans(Alpha.Individualvars), ncol(Y), ncol(Matrix.Country))


Alpha.Conv.Individualvars<-rbind(t(Results$alpha_conv[,,1]), t(Results$alpha_conv[,,2]),
		t(Results$alpha_conv[,,3]))
Alpha.Conv.Individualvars.Means<-matrix(colMeans(Alpha.Conv.Individualvars), ncol(Y), ncol(Matrix.Country))

Alpha.Unconv.Individualvars<-rbind(t(Results$alpha_unconv[,,1]), t(Results$alpha_unconv[,,2]),
		t(Results$alpha_unconv[,,3]))
Alpha.Unconv.Individualvars.Means<-matrix(colMeans(Alpha.Unconv.Individualvars), ncol(Y), ncol(Matrix.Country))

P.Conv.Individualvars<-rbind(t(Results$P_conv[,,1]), 
		t(Results$P_conv[,,2]),
		t(Results$P_conv[,,3]))

P.Unconv.Individualvars<-rbind(t(Results$P_unconv[,,1]),  
		t(Results$P_unconv[,,2]),
		t(Results$P_unconv[,,3]))



Beta2.Individualvars<-rbind((t(Results$beta2[,,1])),
	(t(Results$beta2[,,2])),
	(t(Results$beta2[,,3])))

Beta3.Individualvars<-rbind((t(Results$beta3[,,1])),
	(t(Results$beta3[,,2])),
	(t(Results$beta3[,,3])))


Beta4.Individualvars<-rbind((t(Results$beta4[,,1])),
	(t(Results$beta4[,,2])),
	(t(Results$beta4[,,3])))



BETA2<-rbind((t(Results$beta2[,,1])),
	(t(Results$beta2[,,2])),
	(t(Results$beta2[,,3])))


BETA3<-rbind((t(Results$beta3[,,1])),
	(t(Results$beta3[,,2])),
	(t(Results$beta3[,,3])))


BETA4<-rbind((t(Results$beta4[,,1])),
	(t(Results$beta4[,,2])),
	(t(Results$beta4[,,3])))


Beta.Agitator<-BETA2[seq(1, nrow(BETA2), length.out=1000),]
Beta.Conventional<-BETA3[seq(1, nrow(BETA3), length.out=1000),]
Beta.Activist<-BETA4[seq(1, nrow(BETA4), length.out=1000),]


prob.Agitator<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X%*%Beta.Agitator[s,])/(1+exp(X%*%Beta.Agitator[s,])+
			exp(X%*%Beta.Conventional[s,])+exp(X%*%Beta.Activist[s,])))

prob.Conventional<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X%*%Beta.Conventional[s,])/(1+exp(X%*%Beta.Agitator[s,])+
			exp(X%*%Beta.Conventional[s,])+exp(X%*%Beta.Activist[s,])))

prob.Activist<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X%*%Beta.Activist[s,])/(1+exp(X%*%Beta.Agitator[s,])+
			exp(X%*%Beta.Conventional[s,])+exp(X%*%Beta.Activist[s,])))
prob.Outsider<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X%*%Beta.Agitator[s,])+
			exp(X%*%Beta.Conventional[s,])+exp(X%*%Beta.Activist[s,])))


Posterior.Probs.Individualvars<-cbind(colMeans(prob.Outsider), colMeans(prob.Agitator), colMeans(prob.Conventional), colMeans(prob.Activist))




set.seed(3002)
y<-rMultinom(Posterior.Probs.Individualvars,1)



Individualvars<-sum(log(mean(P.Conv.Individualvars[,1])*mean(P.Unconv.Individualvars[,1])* apply(sapply(1:ncol(Y),function(l) (pnorm((Matrix.Country%*%t(Alpha.Individualvars.Means))[,l])^Y[,l])*((1-pnorm((Matrix.Country%*%t(Alpha.Individualvars.Means))[,l]))^(1-Y[,l]))),1,prod)+
mean(P.Conv.Individualvars[,2])*mean(P.Unconv.Individualvars[,1])* apply(sapply(1:ncol(Y),function(l) (pnorm((Matrix.Country%*%t(Alpha.Individualvars.Means))[,l]+(Matrix.Country%*%t(Alpha.Conv.Individualvars.Means))[,l])^Y[,l])*((1-pnorm((Matrix.Country%*%t(Alpha.Individualvars.Means))[,l]+(Matrix.Country%*%t(Alpha.Conv.Individualvars.Means))[,l]))^(1-Y[,l]))),1,prod)+
mean(P.Conv.Individualvars[,1])*mean(P.Unconv.Individualvars[,2])* apply(sapply(1:ncol(Y),function(l) (pnorm((Matrix.Country%*%t(Alpha.Individualvars.Means))[,l]+(Matrix.Country%*%t(Alpha.Unconv.Individualvars.Means))[,l])^Y[,l])*((1-pnorm((Matrix.Country%*%t(Alpha.Individualvars.Means))[,l]+(Matrix.Country%*%t(Alpha.Unconv.Individualvars.Means))[,l]))^(1-Y[,l]))),1,prod)+
mean(P.Conv.Individualvars[,2])*mean(P.Unconv.Individualvars[,2])* apply(sapply(1:ncol(Y),function(l) (pnorm((Matrix.Country%*%t(Alpha.Individualvars.Means))[,l]+(Matrix.Country%*%t(Alpha.Conv.Individualvars.Means))[,l]+(Matrix.Country%*%t(Alpha.Unconv.Individualvars.Means))[,l])^Y[,l])*((1-pnorm((Matrix.Country%*%t(Alpha.Individualvars.Means))[,l]+(Matrix.Country%*%t(Alpha.Conv.Individualvars.Means))[,l]+(Matrix.Country%*%t(Alpha.Unconv.Individualvars.Means))[,l]))^(1-Y[,l]))),1,prod)))+
sum((as.vector(y)==2)*log(exp(X.individualvars%*%colMeans(Beta2.Individualvars))/(1+exp(X.individualvars%*%colMeans(Beta2.Individualvars))+exp(X.individualvars%*%colMeans(Beta3.Individualvars))+exp(X.individualvars%*%colMeans(Beta4.Individualvars))))+
(as.vector(y)==3)*log(exp(X.individualvars%*%colMeans(Beta3.Individualvars))/(1+exp(X.individualvars%*%colMeans(Beta2.Individualvars))+exp(X.individualvars%*%colMeans(Beta3.Individualvars))+exp(X.individualvars%*%colMeans(Beta4.Individualvars))))+
(as.vector(y)==4)*log(exp(X.individualvars%*%colMeans(Beta4.Individualvars))/(1+exp(X.individualvars%*%colMeans(Beta2.Individualvars))+exp(X.individualvars%*%colMeans(Beta3.Individualvars))+exp(X.individualvars%*%colMeans(Beta4.Individualvars))))+
(as.vector(y)==1)*log(1/(1+exp(X.individualvars%*%colMeans(Beta2.Individualvars))+exp(X.individualvars%*%colMeans(Beta3.Individualvars))+exp(X.individualvars%*%colMeans(Beta4.Individualvars)))))




rm(Results)




load("Results_NocovarsModel")


# Estimates
Alpha.Novars<-rbind(t(Results$alpha[,,1]), t(Results$alpha[,,2]),
		t(Results$alpha[,,3]))
Alpha.Novars.Means<-matrix(colMeans(Alpha.Novars), ncol(Y), ncol(Matrix.Country))


Alpha.Conv.Novars<-rbind(t(Results$alpha_conv[,,1]), t(Results$alpha_conv[,,2]),
		t(Results$alpha_conv[,,3]))
Alpha.Conv.Novars.Means<-matrix(colMeans(Alpha.Conv.Novars), ncol(Y), ncol(Matrix.Country))

Alpha.Unconv.Novars<-rbind(t(Results$alpha_unconv[,,1]), t(Results$alpha_unconv[,,2]),
		t(Results$alpha_unconv[,,3]))
Alpha.Unconv.Novars.Means<-matrix(colMeans(Alpha.Unconv.Novars), ncol(Y), ncol(Matrix.Country))

P.Conv.Novars<-rbind(t(Results$P_conv[,,1]), 
		t(Results$P_conv[,,2]),
		t(Results$P_conv[,,3]))

P.Unconv.Novars<-rbind(t(Results$P_unconv[,,1]),  
		t(Results$P_unconv[,,2]),
		t(Results$P_unconv[,,3]))





Nu2.Novars<-NU[, 1:length(unique(Country))]
Nu3.Novars<-NU[, (1+length(unique(Country))): (2*length(unique(Country)))]
Nu4.Novars<-NU[, (1+2*length(unique(Country))): (3*length(unique(Country)))]



NU2<-NU[, 1:length(unique(Country))]
NU3<-NU[, (1+length(unique(Country))): (2*length(unique(Country)))]
NU4<-NU[, (1+2*length(unique(Country))): (3*length(unique(Country)))]


Nu.Agitator<-NU2[seq(1, nrow(NU2), length.out=1000),]
Nu.Conventional<-NU3[seq(1, nrow(NU3), length.out=1000),]
Nu.Activist<-NU4[seq(1, nrow(NU4), length.out=1000),]




prob.Agitator<-sapply(1:dim(Nu.Agitator)[1], function(s) exp(Nu.Agitator[s,Country])/(1+exp(Nu.Agitator[s,Country])+
			exp(Nu.Conventional[s,Country])+exp(Nu.Activist[s,Country])))

prob.Conventional<-sapply(1:dim(Nu.Agitator)[1], function(s) exp(Nu.Conventional[s,Country])/(1+exp(Nu.Agitator[s,Country])+
			exp(Nu.Conventional[s,Country])+exp(Nu.Activist[s,Country])))

prob.Activist<-sapply(1:dim(Nu.Agitator)[1], function(s) exp(Nu.Activist[s,Country])/(1+exp(Nu.Agitator[s,Country])+
			exp(Nu.Conventional[s,Country])+exp(Nu.Activist[s,Country])))
prob.Outsider<-sapply(1:dim(Nu.Agitator)[1], function(s) 1/(1+exp(Nu.Agitator[s,Country])+
			exp(Nu.Conventional[s,Country])+exp(Nu.Activist[s,Country])))


Posterior.Probs.Novars<-cbind(colMeans(prob.Outsider), colMeans(prob.Agitator), colMeans(prob.Conventional), colMeans(prob.Activist))


set.seed(3003)
y<-rMultinom(Posterior.Probs.Novars,1)



Novars<-sum(log(mean(P.Conv.Novars[,1])*mean(P.Unconv.Novars[,1])* apply(sapply(1:ncol(Y),function(l) (pnorm((Matrix.Country%*%t(Alpha.Novars.Means))[,l])^Y[,l])*((1-pnorm((Matrix.Country%*%t(Alpha.Novars.Means))[,l]))^(1-Y[,l]))),1,prod)+
mean(P.Conv.Novars[,2])*mean(P.Unconv.Novars[,1])* apply(sapply(1:ncol(Y),function(l) (pnorm((Matrix.Country%*%t(Alpha.Novars.Means))[,l]+(Matrix.Country%*%t(Alpha.Conv.Novars.Means))[,l])^Y[,l])*((1-pnorm((Matrix.Country%*%t(Alpha.Novars.Means))[,l]+(Matrix.Country%*%t(Alpha.Conv.Novars.Means))[,l]))^(1-Y[,l]))),1,prod)+
mean(P.Conv.Novars[,1])*mean(P.Unconv.Novars[,2])* apply(sapply(1:ncol(Y),function(l) (pnorm((Matrix.Country%*%t(Alpha.Novars.Means))[,l]+(Matrix.Country%*%t(Alpha.Unconv.Novars.Means))[,l])^Y[,l])*((1-pnorm((Matrix.Country%*%t(Alpha.Novars.Means))[,l]+(Matrix.Country%*%t(Alpha.Unconv.Novars.Means))[,l]))^(1-Y[,l]))),1,prod)+
mean(P.Conv.Novars[,2])*mean(P.Unconv.Novars[,2])* apply(sapply(1:ncol(Y),function(l) (pnorm((Matrix.Country%*%t(Alpha.Novars.Means))[,l]+(Matrix.Country%*%t(Alpha.Conv.Novars.Means))[,l]+(Matrix.Country%*%t(Alpha.Unconv.Novars.Means))[,l])^Y[,l])*((1-pnorm((Matrix.Country%*%t(Alpha.Novars.Means))[,l]+(Matrix.Country%*%t(Alpha.Conv.Novars.Means))[,l]+(Matrix.Country%*%t(Alpha.Unconv.Novars.Means))[,l]))^(1-Y[,l]))),1,prod)))+
sum((as.vector(y)==2)*log(exp(colMeans(Nu2.Novars)[Country])/(1+exp(colMeans(Nu2.Novars)[Country])+exp(colMeans(Nu3.Novars)[Country])+exp(colMeans(Nu4.Novars)[Country])))+
(as.vector(y)==3)*log(exp(colMeans(Nu3.Novars)[Country])/(1+exp(colMeans(Nu2.Novars)[Country])+exp(colMeans(Nu3.Novars)[Country])+exp(colMeans(Nu4.Novars)[Country])))+ 
(as.vector(y)==4)*log(exp(colMeans(Nu4.Novars)[Country])/(1+exp(colMeans(Nu2.Novars)[Country])+exp(colMeans(Nu3.Novars)[Country])+exp(colMeans(Nu4.Novars)[Country])))+ 
(as.vector(y)==1)*log(1/(1+exp(colMeans(Nu2.Novars)[Country])+exp(colMeans(Nu3.Novars)[Country])+exp(colMeans(Nu4.Novars)[Country]))))




rm(Results)




For.BF.Individual.Null<-(2*Individualvars-log(nrow(Y))*(ncol(Alpha.Individualvars)+
(ncol(Alpha.Conv.Individualvars)-1)+
(ncol(Alpha.Unconv.Individualvars)-1)+ncol(P.Conv.Individualvars)+ncol(Beta2.Individualvars)+ncol(Beta3.Individualvars)+ncol(Beta4.Individualvars)))-
(2*Novars-log(nrow(Y))*(ncol(Alpha.Novars)+
(ncol(Alpha.Conv.Novars)-1)+
(ncol(Alpha.Unconv.Novars)-1)+ncol(P.Conv.Novars)+ncol(Nu2.Novars)+ncol(Nu3.Baseline)+ncol(Nu4.Novars)))


For.BF.Baseline.Null<-(2*Baseline-log(nrow(Y))*(ncol(Alpha.Baseline)+
(ncol(Alpha.Conv.Baseline)-1)+
(ncol(Alpha.Unconv.Baseline)-1)+ncol(P.Conv.Baseline)+ncol(Beta2.Baseline)+ncol(Beta3.Baseline)+ncol(Beta4.Baseline)))-
(2*Novars-log(nrow(Y))*(ncol(Alpha.Novars)+
(ncol(Alpha.Conv.Novars)-1)+
(ncol(Alpha.Unconv.Novars)-1)+ncol(P.Conv.Novars)+ncol(Nu2.Novars)+ncol(Nu3.Baseline)+ncol(Nu4.Novars)))



For.BF.Individual.Null<-round(For.BF.Individual.Null, digits=2)
For.BF.Baseline.Null<-round(For.BF.Baseline.Null, digits=2)
For.BF.Baseline.Individual<-round(For.BF.Baseline.Null-For.BF.Individual.Null, digits=2)


if (nchar(For.BF.Individual.Null)<8) {
while (nchar(For.BF.Individual.Null)<8) {
For.BF.Individual.Null<-paste(For.BF.Individual.Null, "0", sep="")
} 
} 


if (nchar(For.BF.Baseline.Null)<8) {
while (nchar(For.BF.Baseline.Null)<8) {
For.BF.Baseline.Null<-paste(For.BF.Baseline.Null, "0", sep="")
} 
} 


if (nchar(For.BF.Baseline.Individual)<7) {
while (nchar(For.BF.Baseline.Individual)<7) {
For.BF.Baseline.Individual<-paste(For.BF.Baseline.Individual, "0", sep="")
} 
} 

For.BF.Individual.Null<-paste(paste(substr(For.BF.Individual.Null,1,2), ",", sep=""), substr(For.BF.Individual.Null, 3,8), sep="")
For.BF.Baseline.Null<-paste(paste(substr(For.BF.Baseline.Null,1,2), ",", sep=""), substr(For.BF.Baseline.Null, 3,8), sep="")
For.BF.Baseline.Individual<-paste(paste(substr(For.BF.Baseline.Individual,1,1), ",", sep=""), substr(For.BF.Baseline.Individual, 2,7), sep="")

Table.A.6<-matrix(NA,2,4)
colnames(Table.A.6)<-c("", "Baseline Model", "Individual-level covariates only", "No covariates")
Table.A.6[1,]<-c("Baseline Model", "", For.BF.Baseline.Individual, For.BF.Baseline.Null)
Table.A.6[2,]<-c("Individual-level covariates only", "", "", For.BF.Individual.Null)
rownames(Table.A.6)<-NULL

print(Table.A.6)

g1 <- tableGrob(Table.A.6, rows = NULL)
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g1), l = 1, r = ncol(g1))
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 1, l = 1, r = ncol(g1))

pdf(file = "Table.A.6.pdf", height=10, width=10)
grid.draw(g1)
dev.off()



################################################# Figure A.7 ##################################################################################

rm(list=ls())



load("dataset_2012.RData")

df<-subset(dataset, select=c("country.name", "ActMeetMun",
"country.name", "ActMeetMun", "ActContMun",  "ActContAut" ,                           
"ActMeetImp" , "ActSolveProb" ,  "ActMeetPty" ,  "ActContGvt",                            
"ActProtest" , "ActBlock" ,  "ActVoted",  "ActSign" ,                              
"ActShare" , "education" ,  "female", "age", "relative.incomeproxy" ,
"victim", "corrupt", "spatial.distance" ,
"compulsory.voting", "gdp.pc", "rule.of.lw", "enp", "social.spending"))                       
df<-na.exclude(df)


Y <- cbind(df$ActMeetMun, df$ActContMun, df$ActContAut,
		 df$ActContGvt, df$ActSolveProb, 
		df$ActMeetImp, df$ActMeetPty, df$ActVoted, 
		df$ActShare, df$ActSign, df$ActProtest, df$ActBlock)
colnames(Y)<-c("ActMeetMun", "ActContMun", "ActContAut", "ActContGvt",
		"ActSolveProb", "ActMeetImp", "ActMeetPty", "ActVoted",
		"ActShare", "ActSign", "ActProtest", "ActBlock")

Country<-as.numeric(factor(df$country.name), levels=unique(df$country.name))
		


Matrix.Country<-matrix(0,nrow(Y),length(unique(Country)))           
for (i in 1:length(unique(Country))) Matrix.Country[Country==i,i]<-1



load("Results_DirichletModel")


Alpha<-mcmc.list(as.mcmc(t(Results$alpha[,,1])), 
		as.mcmc(t(Results$alpha[,,2])),
		as.mcmc(t(Results$alpha[,,3])))
print(paste("Convergence Alpha:", gelman.diag(Alpha)[[2]]<1.2, sep=" "))


Alpha.Conv<-mcmc.list(as.mcmc(t(Results$alpha_conv[-seq(12,dim(Results$alpha_conv)[1],by=12),,1])), 
		as.mcmc(t(Results$alpha_conv[-seq(12,dim(Results$alpha_conv)[1],by=12),,2])),
		as.mcmc(t(Results$alpha_conv[-seq(12,dim(Results$alpha_conv)[1],by=12),,3])))
print(paste("Convergence Alpha Conv:", gelman.diag(Alpha.Conv)[[2]]<1.2, sep=" "))



Alpha.Unconv<-mcmc.list(as.mcmc(t(Results$alpha_unconv[-seq(1,dim(Results$alpha_unconv)[1],by=12),,1])), 
		as.mcmc(t(Results$alpha_unconv[-seq(1,dim(Results$alpha_unconv)[1],by=12),,2])),
		as.mcmc(t(Results$alpha_unconv[-seq(1,dim(Results$alpha_unconv)[1],by=12),,3])))
print(paste("Convergence Alpha Unconventional:", gelman.diag(Alpha.Unconv)[[2]]<1.2, sep=" "))



ALPHA<-rbind(t(Results$alpha[,,1]), 
		t(Results$alpha[,,2]),
		t(Results$alpha[,,3]))


ALPHA.CONV<-rbind(t(Results$alpha_conv[,,1]), 
		t(Results$alpha_conv[,,2]),
		t(Results$alpha_conv[,,3]))


ALPHA.UNCONV<-rbind(t(Results$alpha_unconv[,,1]), 
		t(Results$alpha_unconv[,,2]),
		t(Results$alpha_unconv[,,3]))

Alpha<-ALPHA[seq(1, nrow(ALPHA), length.out=1000),]
Alpha.Conv<-ALPHA.CONV[seq(1, nrow(ALPHA), length.out=1000),]
Alpha.Unconv<-ALPHA.UNCONV[seq(1, nrow(ALPHA), length.out=1000),]


P.CONV<-rbind(t(Results$P_conv[,,1]), 
		t(Results$P_conv[,,2]),
		t(Results$P_conv[,,3]))
P.Conv<-P.CONV[seq(1, nrow(ALPHA), length.out=1000),]


P.UNCONV<-rbind(t(Results$P_unconv[,,1]), 
		t(Results$P_unconv[,,2]),
		t(Results$P_unconv[,,3]))

P.Unconv<-P.UNCONV[seq(1, nrow(ALPHA), length.out=1000),]




C_unconv=rbinom(nrow(Y), 1, .5)+1
C.Conv<-matrix(0,nrow(Y), nrow(Alpha))
C.Unconv<-matrix(0,nrow(Y), nrow(Alpha))



for (s in 1:nrow(Alpha)){ 

pnum<-matrix(0,nrow(Y),2)

for (k in 1:2) {
lindividual<-matrix(0,nrow(Y),ncol(Y))
for (l in 1:ncol(Y)) {
mutmp=pnorm(Matrix.Country%*%t(matrix(Alpha[s,],ncol(Y), ncol(Matrix.Country)))[,l]+
(k-1)*((Matrix.Country%*%t(matrix(Alpha.Conv[s,],ncol(Y), ncol(Matrix.Country))))[,l])+
(C_unconv-1)*((Matrix.Country%*%t(matrix(Alpha.Unconv[s,],ncol(Y), ncol(Matrix.Country))))[,l]))
lindividual[,l]<-(mutmp^Y[,l])*((1-mutmp)^(1-Y[,l]))
}

pnum[,k]<-apply(lindividual,1,prod)*P.Conv[s,k]
}

p<-pnum/(apply(pnum,1,sum))
p[is.na(p)==1]<-rep(1/2,2)
C_conv<-rMultinom(p,1)

C.Conv[,s]<-C_conv




for (k in 1:2) {
lindividual<-matrix(0,nrow(Y),ncol(Y))
for (l in 1:ncol(Y)) {
mutmp=pnorm(Matrix.Country%*%t(matrix(Alpha[s,],ncol(Y), ncol(Matrix.Country)))[,l]+
(C_conv-1)*((Matrix.Country%*%t(matrix(Alpha.Conv[s,],ncol(Y), ncol(Matrix.Country))))[,l])+
(k-1)*((Matrix.Country%*%t(matrix(Alpha.Unconv[s,],ncol(Y), ncol(Matrix.Country))))[,l]))
lindividual[,l]<-(mutmp^Y[,l])*((1-mutmp)^(1-Y[,l]))
}

pnum[,k]<-apply(lindividual,1,prod)*P.Unconv[s,k]
}

p<-pnum/(apply(pnum,1,sum))
p[is.na(p)==1]<-rep(1/2,2)
C_unconv<-rMultinom(p,1)

C.Unconv[,s]<-C_unconv


}




Assignment.Unconv<-c()
Assignment.Conv<-c()

for (j in 1:nrow(C.Conv)) {

Assignment.Conv<-rbind(Assignment.Conv, c(Country[j], j, which(table(C.Conv[j,])==max(table(C.Conv[j,])))))
Assignment.Unconv<-rbind(Assignment.Unconv, c(Country[j], j, which(table(C.Unconv[j,])==max(table(C.Unconv[j,])))))
}



conv<-as.data.frame(Assignment.Conv)
names(conv)<-c("Country", "Subject", "Conv")
unconv<-as.data.frame(Assignment.Unconv[,-1])
names(unconv)<-c("Subject", "Unconv")



dimensions<-merge(conv, unconv, by="Subject")

dimensions$Type<-NA
for (j in 1:nrow(dimensions)) {

if (dimensions$Conv[j]==1 & dimensions$Unconv[j]==1) {
dimensions$Type[j]<-"Outsider"
} else if (dimensions$Conv[j]==1 & dimensions$Unconv[j]==2) {
dimensions$Type[j]<-"Agitator"
} else if (dimensions$Conv[j]==2 & dimensions$Unconv[j]==1) {
dimensions$Type[j]<-"Conventional"
} else if (dimensions$Conv[j]==2 & dimensions$Unconv[j]==2) {
dimensions$Type[j]<-"Activist"
}
}



		
df$age.centered<-(df$age-mean(df$age))/(2*sd(df$age))
df$education.centered<-(df$education-mean(df$education))/(2*sd(df$education))


df$relative.incomeproxy.centered<-(df$relative.incomeproxy-mean(df$relative.incomeproxy))/(2*sd(df$relative.incomeproxy))


df$spatial.distance.centered<-(df$spatial.distance-mean(df$spatial.distance))/(2*sd(df$spatial.distance))

X<-cbind(df$age.centered, df$female, df$education.centered, 
	df$relative.incomeproxy.centered, 
	df$victim, df$corrupt, 
	df$spatial.distance.centered,
	(df$rule.of.lw-mean(df$rule.of.lw))/(2*sd(df$rule.of.lw)),
		df$compulsory.voting,
		(df$enp-mean(df$enp))/(2*sd(df$enp)),
		(df$gdp.pc-mean(df$gdp.pc))/(2*sd(df$gdp.pc)),
		(df$social.spending-mean(df$social.spending))/(2*sd(df$social.spending))
)

X<-data.frame(X)
names(X)<-c("Age", "Female", "Education", 
		 "Income", "Victimization", "Corruption",
		  "Ideology", "Rule.of.Law", "Compulsory.Voting", "ENPP",
		"GDP", "Social.Spending")


# Clustered multinomial logit estimates, no uncertainty
##------------------------------------------------------------#
##---- Function for estimating clustered standard errors ----##
##-----------------------------------------------------------##

cl.mlogit   <- function(fm, cluster){
  
  # fm: a fitted mlogit model
  # cluster: a data vector with the cluster
  #          identity of each observation in fm
  
  require(sandwich, quietly = TRUE)
  require(lmtest, quietly = TRUE)
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- length(coefficients(fm))
  dfc <- (M/(M-1))*((N-1)/(N-K))
  uj  <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
  vcovCL <- dfc*sandwich(fm, meat.=crossprod(uj)/N)
  list(coeftest(fm, vcovCL), vcovCL)}




model.formula <- as.formula(paste("class ~ 0 | ",paste(names(X), collapse = "+")))

mlogit.out <- mlogit(model.formula, data = mlogit.data(data.frame("class" = dimensions$Type, X), shape = "wide", choice = "class"), reflevel  = "Outsider")


cl.mlogit.out <- cl.mlogit(mlogit.out, cluster = Country)
clust.coef <- cl.mlogit.out[[1]][, 1]
clust.vcov <- cl.mlogit.out[[2]]
samples<-mvrnorm(n = 1000, clust.coef, clust.vcov)



##--------------------------##
##---- Marginal effects ----##
##--------------------------##


## Individual Effects
X<-cbind(1, as.matrix(X))
colnames(X)[1]<-"Intercept"

dummies<-c(3, 6)
continuous<-c(2,4,5,8)
ordinal<-7


Marginal.Individual<-matrix(0, nrow=4*(length(dummies)+length(continuous)+length(ordinal)), 3)
colnames(Marginal.Individual)<-c("Mean", "95.Low", "95.High")
rownames(Marginal.Individual)<-paste(rep(colnames(X)[sort(c(dummies, continuous, ordinal))], each=4),
rep(c("Outsider", "Agitator", "Conventional", "Activist"), (length(dummies)+length(continuous)+length(ordinal))), 
sep="-")


Beta.Agitator<-samples[,grep("Agitator", colnames(samples))]
Beta.Activist<-samples[,grep("Activist", colnames(samples))]
Beta.Conventional<-samples[,grep("Conventional", colnames(samples))]


for (j in 1:length(dummies)) {
X.0<-X
X.1<-X

X.0[,dummies[j]]<-0
X.1[,dummies[j]]<-1


prob.Agitator.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%Beta.Agitator[s,])/(1+exp(X.0%*%Beta.Agitator[s,])+
			exp(X.0%*%Beta.Conventional[s,])+exp(X.0%*%Beta.Activist[s,])))

prob.Conventional.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%Beta.Conventional[s,])/(1+exp(X.0%*%Beta.Agitator[s,])+
			exp(X.0%*%Beta.Conventional[s,])+exp(X.0%*%Beta.Activist[s,])))


prob.Activist.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%Beta.Activist[s,])/(1+exp(X.0%*%Beta.Agitator[s,])+
			exp(X.0%*%Beta.Conventional[s,])+exp(X.0%*%Beta.Activist[s,])))


prob.Outsider.0<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.0%*%Beta.Agitator[s,])+
			exp(X.0%*%Beta.Conventional[s,])+exp(X.0%*%Beta.Activist[s,])))



prob.Agitator.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%Beta.Agitator[s,])/(1+exp(X.1%*%Beta.Agitator[s,])+
			exp(X.1%*%Beta.Conventional[s,])+exp(X.1%*%Beta.Activist[s,])))

prob.Conventional.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%Beta.Conventional[s,])/(1+exp(X.1%*%Beta.Agitator[s,])+
			exp(X.1%*%Beta.Conventional[s,])+exp(X.1%*%Beta.Activist[s,])))


prob.Activist.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%Beta.Activist[s,])/(1+exp(X.1%*%Beta.Agitator[s,])+
			exp(X.1%*%Beta.Conventional[s,])+exp(X.1%*%Beta.Activist[s,])))


prob.Outsider.1<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.1%*%Beta.Agitator[s,])+
			exp(X.1%*%Beta.Conventional[s,])+exp(X.1%*%Beta.Activist[s,])))

Marginal.Individual[grep(colnames(X)[dummies[j]], rownames(Marginal.Individual))[1],]<-c(mean(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0)),
											quantile(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0), c(0.025,0.975)))

Marginal.Individual[grep(colnames(X)[dummies[j]], rownames(Marginal.Individual))[2],]<-c(mean(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0)), quantile(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0), c(0.025,0.975)))


Marginal.Individual[grep(colnames(X)[dummies[j]], rownames(Marginal.Individual))[3],]<-c(mean(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0)), quantile(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0), c(0.025,0.975)))


Marginal.Individual[grep(colnames(X)[dummies[j]], rownames(Marginal.Individual))[4],]<-c(mean(colMeans(prob.Activist.1)-colMeans(prob.Activist.0)), quantile(colMeans(prob.Activist.1)-colMeans(prob.Activist.0), c(0.025,0.975)))

}






for (j in 1:length(continuous)){
X.0<-X
X.1<-X
X.1[,continuous[j]]<-X.0[,continuous[j]]+sd(X.0[,continuous[j]])

prob.Agitator.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%Beta.Agitator[s,])/(1+exp(X.0%*%Beta.Agitator[s,])+
			exp(X.0%*%Beta.Conventional[s,])+exp(X.0%*%Beta.Activist[s,])))

prob.Conventional.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%Beta.Conventional[s,])/(1+exp(X.0%*%Beta.Agitator[s,])+
			exp(X.0%*%Beta.Conventional[s,])+exp(X.0%*%Beta.Activist[s,])))


prob.Activist.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%Beta.Activist[s,])/(1+exp(X.0%*%Beta.Agitator[s,])+
			exp(X.0%*%Beta.Conventional[s,])+exp(X.0%*%Beta.Activist[s,])))


prob.Outsider.0<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.0%*%Beta.Agitator[s,])+
			exp(X.0%*%Beta.Conventional[s,])+exp(X.0%*%Beta.Activist[s,])))



prob.Agitator.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%Beta.Agitator[s,])/(1+exp(X.1%*%Beta.Agitator[s,])+
			exp(X.1%*%Beta.Conventional[s,])+exp(X.1%*%Beta.Activist[s,])))

prob.Conventional.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%Beta.Conventional[s,])/(1+exp(X.1%*%Beta.Agitator[s,])+
			exp(X.1%*%Beta.Conventional[s,])+exp(X.1%*%Beta.Activist[s,])))


prob.Activist.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%Beta.Activist[s,])/(1+exp(X.1%*%Beta.Agitator[s,])+
			exp(X.1%*%Beta.Conventional[s,])+exp(X.1%*%Beta.Activist[s,])))


prob.Outsider.1<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.1%*%Beta.Agitator[s,])+
			exp(X.1%*%Beta.Conventional[s,])+exp(X.1%*%Beta.Activist[s,])))


Marginal.Individual[grep(colnames(X)[continuous[j]], rownames(Marginal.Individual))[1],]<-c(mean(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0)),
											quantile(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0), c(0.025,0.975)))

Marginal.Individual[grep(colnames(X)[continuous[j]], rownames(Marginal.Individual))[2],]<-c(mean(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0)), quantile(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0), c(0.025,0.975)))


Marginal.Individual[grep(colnames(X)[continuous[j]], rownames(Marginal.Individual))[3],]<-c(mean(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0)), quantile(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0), c(0.025,0.975)))


Marginal.Individual[grep(colnames(X)[continuous[j]], rownames(Marginal.Individual))[4],]<-c(mean(colMeans(prob.Activist.1)-colMeans(prob.Activist.0)), quantile(colMeans(prob.Activist.1)-colMeans(prob.Activist.0), c(0.025,0.975)))

}


for (j in 1:length(ordinal)){
X.0<-X
X.1<-X
X.1[,ordinal[j]]<-X.0[,ordinal[j]]+1

prob.Agitator.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%Beta.Agitator[s,])/(1+exp(X.0%*%Beta.Agitator[s,])+
			exp(X.0%*%Beta.Conventional[s,])+exp(X.0%*%Beta.Activist[s,])))

prob.Conventional.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%Beta.Conventional[s,])/(1+exp(X.0%*%Beta.Agitator[s,])+
			exp(X.0%*%Beta.Conventional[s,])+exp(X.0%*%Beta.Activist[s,])))


prob.Activist.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%Beta.Activist[s,])/(1+exp(X.0%*%Beta.Agitator[s,])+
			exp(X.0%*%Beta.Conventional[s,])+exp(X.0%*%Beta.Activist[s,])))


prob.Outsider.0<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.0%*%Beta.Agitator[s,])+
			exp(X.0%*%Beta.Conventional[s,])+exp(X.0%*%Beta.Activist[s,])))



prob.Agitator.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%Beta.Agitator[s,])/(1+exp(X.1%*%Beta.Agitator[s,])+
			exp(X.1%*%Beta.Conventional[s,])+exp(X.1%*%Beta.Activist[s,])))

prob.Conventional.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%Beta.Conventional[s,])/(1+exp(X.1%*%Beta.Agitator[s,])+
			exp(X.1%*%Beta.Conventional[s,])+exp(X.1%*%Beta.Activist[s,])))


prob.Activist.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%Beta.Activist[s,])/(1+exp(X.1%*%Beta.Agitator[s,])+
			exp(X.1%*%Beta.Conventional[s,])+exp(X.1%*%Beta.Activist[s,])))


prob.Outsider.1<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.1%*%Beta.Agitator[s,])+
			exp(X.1%*%Beta.Conventional[s,])+exp(X.1%*%Beta.Activist[s,])))


Marginal.Individual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Individual))[1],]<-c(mean(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0)),
											quantile(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0), c(0.025,0.975)))

Marginal.Individual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Individual))[2],]<-c(mean(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0)), quantile(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0), c(0.025,0.975)))


Marginal.Individual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Individual))[3],]<-c(mean(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0)), quantile(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0), c(0.025,0.975)))


Marginal.Individual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Individual))[4],]<-c(mean(colMeans(prob.Activist.1)-colMeans(prob.Activist.0)), quantile(colMeans(prob.Activist.1)-colMeans(prob.Activist.0), c(0.025,0.975)))

}

Marginal.Individual[,1:3]<-100*Marginal.Individual[,1:3]
Marginal.Individual[grep("Ideology", rownames(Marginal.Individual)),1:3]<-(Marginal.Individual[grep("Ideology", rownames(Marginal.Individual)),1:3]/sd(X[,grep("Ideology", colnames(X))]))






## Contextual

continuous<-c(9, 11:13)
ordinal<-10


Marginal.Contextual<-matrix(0, nrow=4*(length(continuous)+length(ordinal)), 3)
colnames(Marginal.Contextual)<-c("Mean", "95.Low", "95.High")
rownames(Marginal.Contextual)<-paste(rep(colnames(X)[sort(c(continuous, ordinal))], each=4),
rep(c("Outsider", "Agitator", "Conventional", "Activist"), (length(continuous)+length(ordinal))), 
sep="-")



for (j in 1:length(continuous)){
X.0<-X
X.1<-X
X.1[,continuous[j]]<-X.0[,continuous[j]]+sd(X.0[,continuous[j]])

prob.Agitator.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%Beta.Agitator[s,])/(1+exp(X.0%*%Beta.Agitator[s,])+
			exp(X.0%*%Beta.Conventional[s,])+exp(X.0%*%Beta.Activist[s,])))

prob.Conventional.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%Beta.Conventional[s,])/(1+exp(X.0%*%Beta.Agitator[s,])+
			exp(X.0%*%Beta.Conventional[s,])+exp(X.0%*%Beta.Activist[s,])))


prob.Activist.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%Beta.Activist[s,])/(1+exp(X.0%*%Beta.Agitator[s,])+
			exp(X.0%*%Beta.Conventional[s,])+exp(X.0%*%Beta.Activist[s,])))


prob.Outsider.0<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.0%*%Beta.Agitator[s,])+
			exp(X.0%*%Beta.Conventional[s,])+exp(X.0%*%Beta.Activist[s,])))



prob.Agitator.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%Beta.Agitator[s,])/(1+exp(X.1%*%Beta.Agitator[s,])+
			exp(X.1%*%Beta.Conventional[s,])+exp(X.1%*%Beta.Activist[s,])))

prob.Conventional.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%Beta.Conventional[s,])/(1+exp(X.1%*%Beta.Agitator[s,])+
			exp(X.1%*%Beta.Conventional[s,])+exp(X.1%*%Beta.Activist[s,])))


prob.Activist.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%Beta.Activist[s,])/(1+exp(X.1%*%Beta.Agitator[s,])+
			exp(X.1%*%Beta.Conventional[s,])+exp(X.1%*%Beta.Activist[s,])))


prob.Outsider.1<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.1%*%Beta.Agitator[s,])+
			exp(X.1%*%Beta.Conventional[s,])+exp(X.1%*%Beta.Activist[s,])))



Marginal.Contextual[grep(colnames(X)[continuous[j]], rownames(Marginal.Contextual))[1],]<-c(mean(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0)),
											quantile(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0), c(0.025,0.975)))

Marginal.Contextual[grep(colnames(X)[continuous[j]], rownames(Marginal.Contextual))[2],]<-c(mean(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0)), quantile(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0), c(0.025,0.975)))


Marginal.Contextual[grep(colnames(X)[continuous[j]], rownames(Marginal.Contextual))[3],]<-c(mean(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0)), quantile(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0), c(0.025,0.975)))


Marginal.Contextual[grep(colnames(X)[continuous[j]], rownames(Marginal.Contextual))[4],]<-c(mean(colMeans(prob.Activist.1)-colMeans(prob.Activist.0)), quantile(colMeans(prob.Activist.1)-colMeans(prob.Activist.0), c(0.025,0.975)))

}

for (j in 1:length(ordinal)){
X.0<-X
X.1<-X
X.1[,ordinal[j]]<-X.0[,ordinal[j]]+1

prob.Agitator.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%Beta.Agitator[s,])/(1+exp(X.0%*%Beta.Agitator[s,])+
			exp(X.0%*%Beta.Conventional[s,])+exp(X.0%*%Beta.Activist[s,])))

prob.Conventional.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%Beta.Conventional[s,])/(1+exp(X.0%*%Beta.Agitator[s,])+
			exp(X.0%*%Beta.Conventional[s,])+exp(X.0%*%Beta.Activist[s,])))


prob.Activist.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%Beta.Activist[s,])/(1+exp(X.0%*%Beta.Agitator[s,])+
			exp(X.0%*%Beta.Conventional[s,])+exp(X.0%*%Beta.Activist[s,])))


prob.Outsider.0<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.0%*%Beta.Agitator[s,])+
			exp(X.0%*%Beta.Conventional[s,])+exp(X.0%*%Beta.Activist[s,])))



prob.Agitator.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%Beta.Agitator[s,])/(1+exp(X.1%*%Beta.Agitator[s,])+
			exp(X.1%*%Beta.Conventional[s,])+exp(X.1%*%Beta.Activist[s,])))

prob.Conventional.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%Beta.Conventional[s,])/(1+exp(X.1%*%Beta.Agitator[s,])+
			exp(X.1%*%Beta.Conventional[s,])+exp(X.1%*%Beta.Activist[s,])))


prob.Activist.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%Beta.Activist[s,])/(1+exp(X.1%*%Beta.Agitator[s,])+
			exp(X.1%*%Beta.Conventional[s,])+exp(X.1%*%Beta.Activist[s,])))


prob.Outsider.1<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.1%*%Beta.Agitator[s,])+
			exp(X.1%*%Beta.Conventional[s,])+exp(X.1%*%Beta.Activist[s,])))


Marginal.Contextual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Contextual))[1],]<-c(mean(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0)),
											quantile(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0), c(0.025,0.975)))

Marginal.Contextual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Contextual))[2],]<-c(mean(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0)), quantile(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0), c(0.025,0.975)))


Marginal.Contextual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Contextual))[3],]<-c(mean(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0)), quantile(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0), c(0.025,0.975)))


Marginal.Contextual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Contextual))[4],]<-c(mean(colMeans(prob.Activist.1)-colMeans(prob.Activist.0)), quantile(colMeans(prob.Activist.1)-colMeans(prob.Activist.0), c(0.025,0.975)))

}

Marginal.Contextual[,1:3]<-100*Marginal.Contextual[,1:3]


Figure.Individual<-cbind(unlist(strsplit(rownames(Marginal.Individual), "-"))[seq(2,length(unlist(strsplit(rownames(Marginal.Individual), "-"))), by=2)],
unlist(strsplit(rownames(Marginal.Individual), "-"))[seq(1,length(unlist(strsplit(rownames(Marginal.Individual), "-"))), by=2)],
Marginal.Individual[,1:3])
Figure.Individual<-data.frame(Figure.Individual)
names(Figure.Individual)<-c("Type", "Variable", "Mean", "Low", "High")


Figure.Contextual<-cbind(unlist(strsplit(rownames(Marginal.Contextual), "-"))[seq(2,length(unlist(strsplit(rownames(Marginal.Contextual), "-"))), by=2)],
unlist(strsplit(rownames(Marginal.Contextual), "-"))[seq(1,length(unlist(strsplit(rownames(Marginal.Contextual), "-"))), by=2)],
Marginal.Contextual[,1:3])
Figure.Contextual<-data.frame(Figure.Contextual)
names(Figure.Contextual)<-c("Type", "Variable", "Mean", "Low", "High")



Figure.A.7<-rbind(Figure.Individual, Figure.Contextual)



Figure.A.7$Type<- factor(Figure.A.7$Type, 
	levels = c("Outsider",
		"Agitator",
		"Conventional", "Activist"))



Figure.A.7$Variable<-factor(Figure.A.7$Variable, 
	levels=rev(c("Age", "Female", "Education", 
			"Income", "Corruption", 
			"Victimization",
			"Ideology",
"Rule.of.Law", "Compulsory.Voting", "ENPP", 
			"GDP", "Social.Spending")),
       labels=rev(c("Age",
			  "Female",
			  "Education", 
			  "Relative Income",
			  "Perceived Corruption", 
			   "Crime Victimization",
			 "Ideological Distance to Incumbent",
"Rule of Law",
			  "Compulsory Voting",
			  "ENPP", 
			  "GDP per capita",
			  "Social Spending")))


for (k in 3:5){
Figure.A.7[,k]<-as.numeric(levels(Figure.A.7[,k]))[Figure.A.7[,k]]
}


p<-ggplot(Figure.A.7,
 aes(y=Variable, x=Mean)) + 
    geom_errorbarh(aes(xmin=Low, 
	xmax=High, height=.2), colour="black") + facet_wrap(~Type)+
    geom_point(size=1.5)+
theme_bw()+xlab("Marginal effects (in percentage points)")+ylab("")+
geom_hline(yintercept=0, linetype="longdash", colour="gray40")+
theme(strip.text.x = element_text(size = 12, face="bold"))+
geom_vline(xintercept=0, linetype="longdash", colour="gray40")+
theme(strip.background = element_rect(fill = alpha('white'))) +
theme(axis.title.x=element_text(vjust=2, size=11))+
theme(axis.text.y=element_text(size=11))

pdf(file = "Figure.A.7.pdf")
grid.draw(p)
dev.off()



################################################# SECTION A.3.2 ##################################################################################
################################################# Table A.7 ###################################################################################

rm(list=ls())
load("dataset_2012.RData")

df<-subset(dataset, select=c("country.name", "ActMeetMun",
"country.name", "ActMeetMun", "ActContMun",  "ActContAut" ,                           
"ActMeetImp" , "ActSolveProb" ,  "ActMeetPty" ,  "ActContGvt",                            
"ActProtest" , "ActBlock" ,  "ActVoted",  "ActSign" ,                              
"ActShare" , "education" ,  "female", "age", "relative.incomeproxy" ,
"victim", "corrupt", "spatial.distance" ,
"compulsory.voting", "gdp.pc", "rule.of.lw", "enp", "social.spending"))                       
df<-na.exclude(df)


Y <- cbind(df$ActMeetMun, df$ActContMun, df$ActContAut,
		 df$ActContGvt, df$ActSolveProb, 
		df$ActMeetImp, df$ActMeetPty, df$ActVoted, 
		df$ActShare, df$ActSign, df$ActProtest, df$ActBlock)

Country<-as.numeric(factor(df$country.name), levels=unique(df$country.name))
		

Matrix.Country<-matrix(0,nrow(Y),length(unique(Country)))           
for (i in 1:length(unique(Country))) Matrix.Country[Country==i,i]<-1


df$age.centered<-(df$age-mean(df$age))/(2*sd(df$age))
df$education.centered<-(df$education-mean(df$education))/(2*sd(df$education))


df$relative.incomeproxy.centered<-(df$relative.incomeproxy-mean(df$relative.incomeproxy))/(2*sd(df$relative.incomeproxy))


df$spatial.distance.centered<-(df$spatial.distance-mean(df$spatial.distance))/(2*sd(df$spatial.distance))

X<-cbind(1, df$age.centered, df$female, df$education.centered, 
	df$relative.incomeproxy.centered, 
	df$victim, df$corrupt, 
	df$spatial.distance.centered,
	(df$rule.of.lw-mean(df$rule.of.lw))/(2*sd(df$rule.of.lw)),
		df$compulsory.voting,
		(df$enp-mean(df$enp))/(2*sd(df$enp)),
		(df$gdp.pc-mean(df$gdp.pc))/(2*sd(df$gdp.pc)),
		(df$social.spending-mean(df$social.spending))/(2*sd(df$social.spending))
)


load("Results_LogitModel")

# Convergence checks for Income proxy Model
Alpha<-mcmc.list(as.mcmc(t(Results$alpha[,,1])), 
		as.mcmc(t(Results$alpha[,,2])),
		as.mcmc(t(Results$alpha[,,3])))
print(paste("Convergence Alpha:", gelman.diag(Alpha)[[2]]<1.2, sep=" "))


Alpha.Conv<-mcmc.list(as.mcmc(t(Results$alpha_conv[-seq(12,dim(Results$alpha_conv)[1],by=12),,1])), 
		as.mcmc(t(Results$alpha_conv[-seq(12,dim(Results$alpha_conv)[1],by=12),,2])),
		as.mcmc(t(Results$alpha_conv[-seq(12,dim(Results$alpha_conv)[1],by=12),,3])))
print(paste("Convergence Alpha Conv:", gelman.diag(Alpha.Conv)[[2]]<1.2, sep=" "))


Alpha.Unconv<-mcmc.list(as.mcmc(t(Results$alpha_unconv[-seq(1,dim(Results$alpha_unconv)[1],by=12),,1])), 
		as.mcmc(t(Results$alpha_unconv[-seq(1,dim(Results$alpha_unconv)[1],by=12),,2])),
		as.mcmc(t(Results$alpha_unconv[-seq(1,dim(Results$alpha_unconv)[1],by=12),,3])))
print(paste("Convergence Alpha Unconventional:", gelman.diag(Alpha.Unconv)[[2]]<1.2, sep=" "))



P.Conv<-mcmc.list(as.mcmc((Results$P_conv[2,,1])), 
		as.mcmc((Results$P_conv[2,,2])),
		as.mcmc((Results$P_conv[2,,3])))
print(paste("Convergence Prob. Conventional:", gelman.diag(P.Conv)[[1]][,1]<1.2, sep=" "))


P.Unconv<-mcmc.list(as.mcmc((Results$P_unconv[2,,1])), 
		as.mcmc((Results$P_unconv[2,,2])),
		as.mcmc((Results$P_unconv[2,,3])))
print(paste("Convergence Prob. Unconventional", gelman.diag(P.Unconv)[[1]][,1]<1.2, sep=" "))


Beta2<-mcmc.list(as.mcmc(t(Results$beta2[,,1])),
	as.mcmc(t(Results$beta2[,,2])),
	as.mcmc(t(Results$beta2[,,3])))
Beta3<-mcmc.list(as.mcmc(t(Results$beta3[,,1])),
	as.mcmc(t(Results$beta3[,,2])),
	as.mcmc(t(Results$beta3[,,3])))
Beta4<-mcmc.list(as.mcmc(t(Results$beta4[,,1])),
	as.mcmc(t(Results$beta4[,,2])),
	as.mcmc(t(Results$beta4[,,3])))
print(paste("Convergence Regression coefficients:", gelman.diag(Beta2)[[2]]<1.2 & gelman.diag(Beta3)[[2]]<1.2 & gelman.diag(Beta4)[[2]]<1.2, sep=" "))




# First we process the data needed to produce Table A.7 and the other results reported in Section A.3.2 of the Online Appendix
ALPHA<-rbind(t(Results$alpha[,,1]), 
		t(Results$alpha[,,2]),
		t(Results$alpha[,,3]))


Alpha.means<-matrix(0, 12,4)
rownames(Alpha.means)<-NULL
colnames(Alpha.means)<-c("Activity", "Mean", "95.Low", "95.High")
Alpha.means[,1]<-c("Municipal meeting", "Contacting municipality", "Contacting local authority", "Contacting national authority",
                    		"Solving Problems", "Improvements meeting", "Party meeting", "Voting",
                    		"Sharing online", "Petitioning", "Protesting", "Blocking")

for (j in 1:12) {

Alpha.means[j,2:4]<-c(mean(c(ALPHA[, seq(j, ncol(ALPHA), by=12)])), quantile(c(ALPHA[, seq(j, ncol(ALPHA), by=12)]), c(0.025,0.975)))

}

save(Alpha.means, file="Alpha.means.LogitModel")

Alpha.all<-cbind(rep(rownames(table(df$country.name)), each=12),  		
		rep(Alpha.means[,1], length(unique(df$country.name))), 
		colMeans(ALPHA), t(apply(ALPHA, 2, quantile, c(0.025, 0.975))))

colnames(Alpha.all)<-c("Country", "Activity",  "Mean", "95.Low", "95.High")
rownames(Alpha.all)<-NULL

save(Alpha.all, file="Alpha.all.LogitModel")

ALPHA.CONV<-rbind(t(Results$alpha_conv[,,1]), 
		t(Results$alpha_conv[,,2]),
		t(Results$alpha_conv[,,3]))



Alpha.conv.means<-matrix(0, 12,4)
rownames(Alpha.conv.means)<-NULL
colnames(Alpha.conv.means)<-NULL

Alpha.conv.means[,1]<-c("Municipal meeting", "Contacting municipality", "Contacting local authority", "Contacting national authority",
                    		"Solving Problems", "Improvements meeting", "Party meeting", "Voting",
                    		"Sharing online", "Petitioning", "Protesting", "Blocking")
Alpha.conv.means[,2]<-"Conventional"

for (j in 1:12) {

m<-round(mean(c(ALPHA.CONV[, seq(j, ncol(ALPHA.CONV), by=12)])), digits=3)
q.l<-round(quantile(c(ALPHA.CONV[, seq(j, ncol(ALPHA.CONV), by=12)]), c(0.025,0.975))[1], digits=3)
q.h<-round(quantile(c(ALPHA.CONV[, seq(j, ncol(ALPHA.CONV), by=12)]), c(0.025,0.975))[2], digits=3)



if (nchar(m)<5 & grepl("\\.", m)==FALSE) {
m<-paste(m,".", sep="")
while (nchar(m)<5) {
m<-paste(m, "0", sep="")
} 
} else if (nchar(m)<5 & grepl("\\.", m)==TRUE) {
while (nchar(m)<5) {
m<-paste(m, "0", sep="")
} 
}


if (nchar(q.l)<5 & grepl("\\.", q.l)==FALSE) {
q.l<-paste(q.l,".", sep="")
while (nchar(q.l)<5) {
q.l<-paste(q.l, "0", sep="")
} 
} else if (nchar(q.l)<5 & grepl("\\.", q.l)==TRUE) {
while (nchar(q.l)<5) {
q.l<-paste(q.l, "0", sep="")
} 
}

if (nchar(q.h)<5 & grepl("\\.", q.h)==FALSE) {
q.h<-paste(q.h,".", sep="")
while (nchar(q.h)<5) {
q.h<-paste(q.h, "0", sep="")
} 
} else if (nchar(q.h)<5 & grepl("\\.", q.h)==TRUE) {
while (nchar(q.h)<5) {
q.h<-paste(q.h, "0", sep="")
} 
}




Alpha.conv.means[j,3:4]<-c(m, paste(paste("(", q.l, ",", sep=""), paste(q.h,")", sep=""),sep=""))


}


	


Alpha.conv.all<-cbind(rep(rownames(table(df$country.name)), each=12),  		
		rep(Alpha.conv.means[,1], length(unique(df$country.name))), "Conventional",
		colMeans(ALPHA.CONV), t(apply(ALPHA.CONV, 2, quantile, c(0.025, 0.975))))

colnames(Alpha.conv.all)<-c("Country", "Activity", "Dimension", "Mean", "95.Low", "95.High")
rownames(Alpha.conv.all)<-NULL


ALPHA.UNCONV<-rbind(t(Results$alpha_unconv[,,1]), 
		t(Results$alpha_unconv[,,2]),
		t(Results$alpha_unconv[,,3]))



Alpha.unconv.means<-matrix(0, 12,4)
Alpha.unconv.means[,1]<-c("Municipal meeting", "Contacting municipality", "Contacting local authority", "Contacting national authority",
                    		"Solving Problems", "Improvements meeting", "Party meeting", "Voting",
                    		"Sharing online", "Petitioning", "Protesting", "Blocking")
Alpha.unconv.means[,2]<-"Unconventional"

for (j in 1:12) {

m<-round(mean(c(ALPHA.UNCONV[, seq(j, ncol(ALPHA.UNCONV), by=12)])), digits=3)
q.l<-round(quantile(c(ALPHA.UNCONV[, seq(j, ncol(ALPHA.UNCONV), by=12)]), c(0.025,0.975))[1], digits=3)
q.h<-round(quantile(c(ALPHA.UNCONV[, seq(j, ncol(ALPHA.UNCONV), by=12)]), c(0.025,0.975))[2], digits=3)


if (nchar(m)<5 & grepl("\\.", m)==FALSE) {
m<-paste(m,".", sep="")
while (nchar(m)<5) {
m<-paste(m, "0", sep="")
} 
} else if (nchar(m)<5 & grepl("\\.", m)==TRUE) {
while (nchar(m)<5) {
m<-paste(m, "0", sep="")
} 
}


if (nchar(q.l)<5 & grepl("\\.", q.l)==FALSE) {
q.l<-paste(q.l,".", sep="")
while (nchar(q.l)<5) {
q.l<-paste(q.l, "0", sep="")
} 
} else if (nchar(q.l)<5 & grepl("\\.", q.l)==TRUE) {
while (nchar(q.l)<5) {
q.l<-paste(q.l, "0", sep="")
} 
}

if (nchar(q.h)<5 & grepl("\\.", q.h)==FALSE) {
q.h<-paste(q.h,".", sep="")
while (nchar(q.h)<5) {
q.h<-paste(q.h, "0", sep="")
} 
} else if (nchar(q.h)<5 & grepl("\\.", q.h)==TRUE) {
while (nchar(q.h)<5) {
q.h<-paste(q.h, "0", sep="")
} 
}

Alpha.unconv.means[j,3:4]<-c(m, paste(paste("(", q.l, ",", sep=""), paste(q.h,")", sep=""),sep=""))


}



Alpha.unconv.all<-cbind(rep(rownames(table(df$country.name)), each=12),  		
		rep(Alpha.unconv.means[,1], length(unique(df$country.name))), "Unconventional",
		colMeans(ALPHA.UNCONV), 
		t(apply(ALPHA.UNCONV, 2, quantile, c(0.025, 0.975))))
colnames(Alpha.unconv.all)<-c("Country", "Activity", "Dimension", "Mean", "95.Low", "95.High")
rownames(Alpha.unconv.all)<-NULL


Country.Loadings<-data.frame(rbind(Alpha.conv.all, Alpha.unconv.all))
save(Country.Loadings, file="Country.Loadings.LogitModel")



BETA2<-rbind((t(Results$beta2[,,1])),
	(t(Results$beta2[,,2])),
	(t(Results$beta2[,,3])))


BETA3<-rbind((t(Results$beta3[,,1])),
	(t(Results$beta3[,,2])),
	(t(Results$beta3[,,3])))


BETA4<-rbind((t(Results$beta4[,,1])),
	(t(Results$beta4[,,2])),
	(t(Results$beta4[,,3])))


NU<-rbind((t(Results$nu[,,1])),
	(t(Results$nu[,,2])),
	(t(Results$nu[,,3])))


NU2<-NU[, 1:length(unique(Country))]
NU3<-NU[, (1+length(unique(Country))): (2*length(unique(Country)))]
NU4<-NU[, (1+2*length(unique(Country))): (3*length(unique(Country)))]



Beta.Agitator<-BETA2[seq(1, nrow(BETA2), length.out=1000),]
Beta.Conventional<-BETA3[seq(1, nrow(BETA3), length.out=1000),]
Beta.Activist<-BETA4[seq(1, nrow(BETA4), length.out=1000),]
Nu.Agitator<-NU2[seq(1, nrow(NU2), length.out=1000),]
Nu.Conventional<-NU3[seq(1, nrow(NU3), length.out=1000),]
Nu.Activist<-NU4[seq(1, nrow(NU4), length.out=1000),]


prob.Agitator<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X%*%Beta.Agitator[s,]+Nu.Agitator[s,Country])/(1+exp(X%*%Beta.Agitator[s,]+Nu.Agitator[s,Country])+
			exp(X%*%Beta.Conventional[s,]+Nu.Conventional[s,Country])+exp(X%*%Beta.Activist[s,]+Nu.Activist[s,Country])))

prob.Conventional<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X%*%Beta.Conventional[s,]+Nu.Conventional[s,Country])/(1+exp(X%*%Beta.Agitator[s,]+Nu.Agitator[s,Country])+
			exp(X%*%Beta.Conventional[s,]+Nu.Conventional[s,Country])+exp(X%*%Beta.Activist[s,]+Nu.Activist[s,Country])))

prob.Activist<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X%*%Beta.Activist[s,]+Nu.Activist[s,Country])/(1+exp(X%*%Beta.Agitator[s,]+Nu.Agitator[s,Country])+
			exp(X%*%Beta.Conventional[s,]+Nu.Conventional[s,Country])+exp(X%*%Beta.Activist[s,]+Nu.Activist[s,Country])))
prob.Outsider<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X%*%Beta.Agitator[s,]+Nu.Agitator[s,Country])+
			exp(X%*%Beta.Conventional[s,]+Nu.Conventional[s,Country])+exp(X%*%Beta.Activist[s,]+Nu.Activist[s,Country])))


Posterior.Probs<-cbind(colMeans(prob.Outsider), colMeans(prob.Agitator), colMeans(prob.Conventional), colMeans(prob.Activist))
save(Posterior.Probs, file="Posterior.Probs.LogitModel")



Outsider.by.Country<-c()
Activist.by.Country<-c()
Conventional.by.Country<-c()
Agitator.by.Country<-c()

for (j in 1:length(unique(Country))) {

Outsider.by.Country<-rbind(Outsider.by.Country,
c(unique(Country)[j],mean(colMeans(prob.Outsider[Country==j,])), quantile(colMeans(prob.Outsider[Country==j,]), c(0.025,0.975)))
)


Activist.by.Country<-rbind(Activist.by.Country,
c(unique(Country)[j],mean(colMeans(prob.Activist[Country==j,])), quantile(colMeans(prob.Activist[Country==j,]), c(0.025,0.975)))
)


Conventional.by.Country<-rbind(Conventional.by.Country,
c(unique(Country)[j],mean(colMeans(prob.Conventional[Country==j,])), quantile(colMeans(prob.Conventional[Country==j,]), c(0.025,0.975)))
)


Agitator.by.Country<-rbind(Agitator.by.Country,
c(unique(Country)[j],mean(colMeans(prob.Agitator[Country==j,])), quantile(colMeans(prob.Agitator[Country==j,]), c(0.025,0.975)))
)

}


save(Outsider.by.Country, file="Outsider.by.Country.LogitModel")
save(Agitator.by.Country, file="Agitator.by.Country.LogitModel")
save(Activist.by.Country, file="Activist.by.Country.LogitModel")
save(Conventional.by.Country, file="Conventional.by.Country.LogitModel")


ALPHA<-rbind(t(Results$alpha[,,1]), 
		t(Results$alpha[,,2]),
		t(Results$alpha[,,3]))


ALPHA.CONV<-rbind(t(Results$alpha_conv[,,1]), 
		t(Results$alpha_conv[,,2]),
		t(Results$alpha_conv[,,3]))


ALPHA.UNCONV<-rbind(t(Results$alpha_unconv[,,1]), 
		t(Results$alpha_unconv[,,2]),
		t(Results$alpha_unconv[,,3]))

Alpha<-ALPHA[seq(1, nrow(BETA2), length.out=1000),]
Alpha.Conv<-ALPHA.CONV[seq(1, nrow(BETA2), length.out=1000),]
Alpha.Unconv<-ALPHA.UNCONV[seq(1, nrow(BETA2), length.out=1000),]


P.CONV<-rbind(t(Results$P_conv[,,1]), 
		t(Results$P_conv[,,2]),
		t(Results$P_conv[,,3]))
P.Conv<-P.CONV[seq(1, nrow(BETA2), length.out=1000),]


P.UNCONV<-rbind(t(Results$P_unconv[,,1]), 
		t(Results$P_unconv[,,2]),
		t(Results$P_unconv[,,3]))

P.Unconv<-P.UNCONV[seq(1, nrow(BETA2), length.out=1000),]




C_unconv=rbinom(nrow(Y), 1, .5)+1
C.Conv<-matrix(0,nrow(Y), nrow(Alpha))
C.Unconv<-matrix(0,nrow(Y), nrow(Alpha))



for (s in 1:nrow(Alpha)){ 

pnum<-matrix(0,nrow(Y),2)

for (k in 1:2) {
lindividual<-matrix(0,nrow(Y),ncol(Y))
for (l in 1:ncol(Y)) {
mutmp=exp(Matrix.Country%*%t(matrix(Alpha[s,],ncol(Y), ncol(Matrix.Country)))[,l]+
(k-1)*((Matrix.Country%*%t(matrix(Alpha.Conv[s,],ncol(Y), ncol(Matrix.Country))))[,l])+
(C_unconv-1)*((Matrix.Country%*%t(matrix(Alpha.Unconv[s,],ncol(Y), ncol(Matrix.Country))))[,l]))/(1+exp(Matrix.Country%*%t(matrix(Alpha[s,],ncol(Y), ncol(Matrix.Country)))[,l]+
(k-1)*((Matrix.Country%*%t(matrix(Alpha.Conv[s,],ncol(Y), ncol(Matrix.Country))))[,l])+
(C_unconv-1)*((Matrix.Country%*%t(matrix(Alpha.Unconv[s,],ncol(Y), ncol(Matrix.Country))))[,l])))
lindividual[,l]<-(mutmp^Y[,l])*((1-mutmp)^(1-Y[,l]))
}

pnum[,k]<-apply(lindividual,1,prod)*P.Conv[s,k]
}

p<-pnum/(apply(pnum,1,sum))
p[is.na(p)==1]<-rep(1/2,2)
C_conv<-rMultinom(p,1)

C.Conv[,s]<-C_conv




for (k in 1:2) {
lindividual<-matrix(0,nrow(Y),ncol(Y))
for (l in 1:ncol(Y)) {
mutmp=exp(Matrix.Country%*%t(matrix(Alpha[s,],ncol(Y), ncol(Matrix.Country)))[,l]+
(C_conv-1)*((Matrix.Country%*%t(matrix(Alpha.Conv[s,],ncol(Y), ncol(Matrix.Country))))[,l])+
(k-1)*((Matrix.Country%*%t(matrix(Alpha.Unconv[s,],ncol(Y), ncol(Matrix.Country))))[,l]))/(1+exp(Matrix.Country%*%t(matrix(Alpha[s,],ncol(Y), ncol(Matrix.Country)))[,l]+
(C_conv-1)*((Matrix.Country%*%t(matrix(Alpha.Conv[s,],ncol(Y), ncol(Matrix.Country))))[,l])+
(k-1)*((Matrix.Country%*%t(matrix(Alpha.Unconv[s,],ncol(Y), ncol(Matrix.Country))))[,l])))
lindividual[,l]<-(mutmp^Y[,l])*((1-mutmp)^(1-Y[,l]))
}

pnum[,k]<-apply(lindividual,1,prod)*P.Unconv[s,k]
}

p<-pnum/(apply(pnum,1,sum))
p[is.na(p)==1]<-rep(1/2,2)
C_unconv<-rMultinom(p,1)

C.Unconv[,s]<-C_unconv


}




CC<-rowMeans(C.Conv-1)
CU<-rowMeans(C.Unconv-1)

Probs.Dimensions<-cbind(CC, CU)
save(Probs.Dimensions, file="Probs.Dimensions.LogitModel")


Posterior.Conv<-c()

for (j in 1:nrow(C.Conv)) {

table<-table(as.matrix(C.Conv[j,]))

if (dim(table)==2) {
Posterior.Conv<-rbind(Posterior.Conv,
c(which(max(table(as.matrix(C.Conv[j,]))/sum(table(as.matrix(C.Conv[j,]))))==table(as.matrix(C.Conv[j,]))/sum(table(as.matrix(C.Conv[j,]))))[1],
table(as.matrix(C.Conv[j,]))/sum(table(as.matrix(C.Conv[j,]))))
)
} else if (dim(table)==1 & names(table)==1) {

Posterior.Conv<-rbind(Posterior.Conv,
			c(1,1,0))
} else if (dim(table)==1 & names(table)==2) {
Posterior.Conv<-rbind(Posterior.Conv,
			c(2,0,1))
}
}






Posterior.Conv.Country<-cbind(Country, Posterior.Conv)
save(Posterior.Conv.Country, file="Posterior.Conv.Country.LogitModel")


Posterior.Unconv<-c()

for (j in 1:nrow(C.Unconv)) {

table<-table(as.matrix(C.Unconv[j,]))

if (dim(table)==2) {
Posterior.Unconv<-rbind(Posterior.Unconv,
c(which(max(table(as.matrix(C.Unconv[j,]))/sum(table(as.matrix(C.Unconv[j,]))))==table(as.matrix(C.Unconv[j,]))/sum(table(as.matrix(C.Unconv[j,]))))[1],
table(as.matrix(C.Unconv[j,]))/sum(table(as.matrix(C.Unconv[j,]))))
)
} else if (dim(table)==1 & names(table)==1) {

Posterior.Unconv<-rbind(Posterior.Unconv,
			c(1,1,0))
} else if (dim(table)==1 & names(table)==2) {
Posterior.Unconv<-rbind(Posterior.Unconv,
			c(2,0,1))
}
}



Posterior.Unconv.Country<-cbind(Country, Posterior.Unconv)
save(Posterior.Unconv.Country, file="Posterior.Unconv.Country.LogitModel")


# Covariate Marginal Effects
colnames(X)<-c("Intercept", "Age", "Female", "Education", 
		 "Income", "Victimization", "Corruption",
		  "Ideology", "Rule of Law", "Compulsory Voting", "ENPP",
		"GDP", "Social.Spending")

dummies<-c(3, 6)
continuous<-c(2,4,5,8)
ordinal<-7


Marginal.Individual<-matrix(0, nrow=4*(length(dummies)+length(continuous)+length(ordinal)), 3)
colnames(Marginal.Individual)<-c("Mean", "95.Low", "95.High")
rownames(Marginal.Individual)<-paste(rep(colnames(X)[sort(c(dummies, continuous, ordinal))], each=4),
rep(c("Outsider", "Agitator", "Conventional", "Activist"), (length(dummies)+length(continuous)+length(ordinal))), 
sep="-")

for (j in 1:length(dummies)) {
X.0<-X
X.1<-X

X.0[,dummies[j]]<-0
X.1[,dummies[j]]<-1


prob.Agitator.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Outsider.0<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Agitator.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))


prob.Outsider.1<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))


Marginal.Individual[grep(colnames(X)[dummies[j]], rownames(Marginal.Individual))[1],]<-c(mean(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0)),
											quantile(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0), c(0.025,0.975)))

Marginal.Individual[grep(colnames(X)[dummies[j]], rownames(Marginal.Individual))[2],]<-c(mean(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0)), quantile(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0), c(0.025,0.975)))


Marginal.Individual[grep(colnames(X)[dummies[j]], rownames(Marginal.Individual))[3],]<-c(mean(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0)), quantile(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0), c(0.025,0.975)))


Marginal.Individual[grep(colnames(X)[dummies[j]], rownames(Marginal.Individual))[4],]<-c(mean(colMeans(prob.Activist.1)-colMeans(prob.Activist.0)), quantile(colMeans(prob.Activist.1)-colMeans(prob.Activist.0), c(0.025,0.975)))

}


for (j in 1:length(continuous)){
X.0<-X
X.1<-X
X.1[,continuous[j]]<-X.0[,continuous[j]]+sd(X.0[,continuous[j]])


prob.Agitator.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Outsider.0<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Agitator.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))


prob.Outsider.1<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))



Marginal.Individual[grep(colnames(X)[continuous[j]], rownames(Marginal.Individual))[1],]<-c(mean(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0)),
											quantile(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0), c(0.025,0.975)))

Marginal.Individual[grep(colnames(X)[continuous[j]], rownames(Marginal.Individual))[2],]<-c(mean(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0)), quantile(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0), c(0.025,0.975)))


Marginal.Individual[grep(colnames(X)[continuous[j]], rownames(Marginal.Individual))[3],]<-c(mean(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0)), quantile(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0), c(0.025,0.975)))


Marginal.Individual[grep(colnames(X)[continuous[j]], rownames(Marginal.Individual))[4],]<-c(mean(colMeans(prob.Activist.1)-colMeans(prob.Activist.0)), quantile(colMeans(prob.Activist.1)-colMeans(prob.Activist.0), c(0.025,0.975)))

}

for (j in 1:length(ordinal)){
X.0<-X
X.1<-X
X.1[,ordinal[j]]<-X.0[,ordinal[j]]+1


prob.Agitator.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Outsider.0<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Agitator.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))


prob.Outsider.1<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))



Marginal.Individual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Individual))[1],]<-c(mean(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0)),
											quantile(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0), c(0.025,0.975)))

Marginal.Individual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Individual))[2],]<-c(mean(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0)), quantile(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0), c(0.025,0.975)))


Marginal.Individual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Individual))[3],]<-c(mean(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0)), quantile(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0), c(0.025,0.975)))


Marginal.Individual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Individual))[4],]<-c(mean(colMeans(prob.Activist.1)-colMeans(prob.Activist.0)), quantile(colMeans(prob.Activist.1)-colMeans(prob.Activist.0), c(0.025,0.975)))

}


Marginal.Individual[,1:3]<-100*Marginal.Individual[,1:3]
Marginal.Individual[grep("Ideology", rownames(Marginal.Individual)),1:3]<-(Marginal.Individual[grep("Ideology", rownames(Marginal.Individual)),1:3]/sd(X[,grep("Ideology", colnames(X))]))


continuous<-c(9, 11:13)
ordinal<-10


Marginal.Contextual<-matrix(0, nrow=4*(length(continuous)+length(ordinal)), 3)
colnames(Marginal.Contextual)<-c("Mean", "95.Low", "95.High")
rownames(Marginal.Contextual)<-paste(rep(colnames(X)[sort(c(continuous, ordinal))], each=4),
rep(c("Outsider", "Agitator", "Conventional", "Activist"), (length(continuous)+length(ordinal))), 
sep="-")



for (j in 1:length(continuous)){
X.0<-X
X.1<-X
X.1[,continuous[j]]<-X.0[,continuous[j]]+sd(X.0[,continuous[j]])


prob.Agitator.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Outsider.0<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Agitator.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))


prob.Outsider.1<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))



Marginal.Contextual[grep(colnames(X)[continuous[j]], rownames(Marginal.Contextual))[1],]<-c(mean(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0)),
											quantile(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0), c(0.025,0.975)))

Marginal.Contextual[grep(colnames(X)[continuous[j]], rownames(Marginal.Contextual))[2],]<-c(mean(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0)), quantile(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0), c(0.025,0.975)))


Marginal.Contextual[grep(colnames(X)[continuous[j]], rownames(Marginal.Contextual))[3],]<-c(mean(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0)), quantile(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0), c(0.025,0.975)))


Marginal.Contextual[grep(colnames(X)[continuous[j]], rownames(Marginal.Contextual))[4],]<-c(mean(colMeans(prob.Activist.1)-colMeans(prob.Activist.0)), quantile(colMeans(prob.Activist.1)-colMeans(prob.Activist.0), c(0.025,0.975)))

}

for (j in 1:length(ordinal)){
X.0<-X
X.1<-X
X.1[,ordinal[j]]<-X.0[,ordinal[j]]+1


prob.Agitator.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Outsider.0<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Agitator.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))


prob.Outsider.1<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))



Marginal.Contextual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Contextual))[1],]<-c(mean(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0)),
											quantile(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0), c(0.025,0.975)))

Marginal.Contextual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Contextual))[2],]<-c(mean(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0)), quantile(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0), c(0.025,0.975)))


Marginal.Contextual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Contextual))[3],]<-c(mean(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0)), quantile(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0), c(0.025,0.975)))


Marginal.Contextual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Contextual))[4],]<-c(mean(colMeans(prob.Activist.1)-colMeans(prob.Activist.0)), quantile(colMeans(prob.Activist.1)-colMeans(prob.Activist.0), c(0.025,0.975)))

}

Marginal.Contextual[,1:3]<-100*Marginal.Contextual[,1:3]


save(Marginal.Individual, file="Marginal.Individual.LogitModel")
save(Marginal.Contextual, file="Marginal.Contextual.LogitModel")

Marginal.Country<-matrix(0, nrow=4*length(unique(Country)), ncol=3)
colnames(Marginal.Country)<-c("Mean", "95.Low", "95.High")
rownames(Marginal.Country)<-paste(rep(unique(df$country.name), each=4),
rep(c("Outsider", "Agitator", "Conventional", "Activist"), length(unique(Country))), 
sep="-")



Nu.Agitator.0<-rowMeans(Nu.Agitator)
Nu.Conventional.0<-rowMeans(Nu.Conventional)
Nu.Activist.0<-rowMeans(Nu.Activist)


for (j in 1:length(unique(Country))) {


Nu.Agitator.1<-Nu.Agitator[,j]
Nu.Conventional.1<-Nu.Conventional[,j]
Nu.Activist.1<-Nu.Activist[,j]



prob.Agitator.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X%*%(Beta.Agitator[s,])+matrix(Nu.Agitator.0[s],nrow(X)))/(1+exp(X%*%(Beta.Agitator[s,])+matrix(Nu.Agitator.0[s],nrow(X)))+
			exp(X%*%(Beta.Conventional[s,])+matrix(Nu.Conventional.0[s],nrow(X)))+exp(X%*%(Beta.Activist[s,])+matrix(Nu.Activist.0[s],nrow(X)))))

prob.Conventional.0<-sapply(1:dim(Beta.Agitator)[1],  function(s) exp(X%*%(Beta.Conventional[s,])+matrix(Nu.Conventional.0[s],nrow(X)))/(1+exp(X%*%(Beta.Agitator[s,])+matrix(Nu.Agitator.0[s],nrow(X)))+
			exp(X%*%(Beta.Conventional[s,])+matrix(Nu.Conventional.0[s],nrow(X)))+exp(X%*%(Beta.Activist[s,])+matrix(Nu.Activist.0[s],nrow(X)))))


prob.Activist.0<-sapply(1:dim(Beta.Agitator)[1],  function(s) exp(X%*%(Beta.Activist[s,])+matrix(Nu.Activist.0[s],nrow(X)))/(1+exp(X%*%(Beta.Agitator[s,])+matrix(Nu.Agitator.0[s],nrow(X)))+
			exp(X%*%(Beta.Conventional[s,])+matrix(Nu.Conventional.0[s],nrow(X)))+exp(X%*%(Beta.Activist[s,])+matrix(Nu.Activist.0[s],nrow(X)))))


prob.Outsider.0<-sapply(1:dim(Beta.Agitator)[1],  function(s) 1/(1+exp(X%*%(Beta.Agitator[s,])+matrix(Nu.Agitator.0[s],nrow(X)))+
			exp(X%*%(Beta.Conventional[s,])+matrix(Nu.Conventional.0[s],nrow(X)))+exp(X%*%(Beta.Activist[s,])+matrix(Nu.Activist.0[s],nrow(X)))))


prob.Agitator.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X%*%(Beta.Agitator[s,])+matrix(Nu.Agitator.1[s],nrow(X)))/(1+exp(X%*%(Beta.Agitator[s,])+matrix(Nu.Agitator.1[s],nrow(X)))+
			exp(X%*%(Beta.Conventional[s,])+matrix(Nu.Conventional.1[s],nrow(X)))+exp(X%*%(Beta.Activist[s,])+matrix(Nu.Activist.1[s],nrow(X)))))

prob.Conventional.1<-sapply(1:dim(Beta.Agitator)[1],  function(s) exp(X%*%(Beta.Conventional[s,])+matrix(Nu.Conventional.1[s],nrow(X)))/(1+exp(X%*%(Beta.Agitator[s,])+matrix(Nu.Agitator.1[s],nrow(X)))+
			exp(X%*%(Beta.Conventional[s,])+matrix(Nu.Conventional.1[s],nrow(X)))+exp(X%*%(Beta.Activist[s,])+matrix(Nu.Activist.1[s],nrow(X)))))

prob.Activist.1<-sapply(1:dim(Beta.Agitator)[1],  function(s) exp(X%*%(Beta.Activist[s,])+matrix(Nu.Activist.1[s],nrow(X)))/(1+exp(X%*%(Beta.Agitator[s,])+matrix(Nu.Agitator.1[s],nrow(X)))+
			exp(X%*%(Beta.Conventional[s,])+matrix(Nu.Conventional.1[s],nrow(X)))+exp(X%*%(Beta.Activist[s,])+matrix(Nu.Activist.1[s],nrow(X)))))


prob.Outsider.1<-sapply(1:dim(Beta.Agitator)[1],  function(s) 1/(1+exp(X%*%(Beta.Agitator[s,])+matrix(Nu.Agitator.1[s],nrow(X)))+
			exp(X%*%(Beta.Conventional[s,])+matrix(Nu.Conventional.1[s],nrow(X)))+exp(X%*%(Beta.Activist[s,])+matrix(Nu.Activist.1[s],nrow(X)))))



Marginal.Country[agrep(unique(df$country.name)[j], rownames(Marginal.Country))[1],]<-c(mean(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0)),
											quantile(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0), c(0.025,0.975)))

Marginal.Country[agrep( unique(df$country.name)[j], rownames(Marginal.Country))[2],]<-c(mean(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0)), quantile(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0), c(0.025,0.975)))


Marginal.Country[agrep( unique(df$country.name)[j], rownames(Marginal.Country))[3],]<-c(mean(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0)), quantile(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0), c(0.025,0.975)))


Marginal.Country[agrep( unique(df$country.name)[j], rownames(Marginal.Country))[4],]<-c(mean(colMeans(prob.Activist.1)-colMeans(prob.Activist.0)), quantile(colMeans(prob.Activist.1)-colMeans(prob.Activist.0), c(0.025,0.975)))

}

Marginal.Country[,1:3]<-100*Marginal.Country[,1:3]

save(Marginal.Country, file="Marginal.Country.LogitModel")



# Now we produce Table A.7

Table.A.7<-rbind(Alpha.conv.means,Alpha.unconv.means)
Table.A.7<-Table.A.7[c(8, 1:4, 6,5, 7,10, 9, 11, 12, 20, 13:16, 18, 17, 19, 22, 21, 23, 24),] 
colnames(Table.A.7)<-c("Activity", "Dimension", "Mean", "Interval")
print(Table.A.7)

Table.A.7<-cbind(Table.A.7[1:12,-c(2)],Table.A.7[13:24,3:4])


Titles<-matrix(c("", "Conventional", "Dimension", "Unconventional", "Dimension"),nrow=1)

g1 <- tableGrob(Table.A.7, rows = NULL)
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g1), l = 1, r = ncol(g1))
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g1), l = 1, r = 1)
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g1), l = 1, r = 3)
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 1, l = 1, r = ncol(g1))


g0 <- tableGrob(Titles, rows = NULL)
g0 <- gtable_add_grob(g0,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g0), l = 1, r = 3)
g0 <- gtable_add_grob(g0,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g0), l = 1, r = 1)
g0 <- gtable_add_grob(g0,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 1, l = 1, r = ncol(g0))

g<-rbind(g0,g1)


pdf(file = "Table.A.7.pdf", height=10, width=10)
grid.draw(g)
dev.off()

################################################# Figure A.8 ##################################################################################

rm(list=ls())
load("Country.Loadings.LogitModel")



Country.Loadings$abbr<-NA
Country.Loadings$abbr[Country.Loadings$Country=="Argentina"]<-"ARG"
Country.Loadings$abbr[Country.Loadings$Country=="Brazil"]<-"BRA"
Country.Loadings$abbr[Country.Loadings$Country=="Chile"]<-"CHL"
Country.Loadings$abbr[Country.Loadings$Country=="Colombia"]<-"COL"
Country.Loadings$abbr[Country.Loadings$Country=="Costa Rica"]<-"CRI"
Country.Loadings$abbr[Country.Loadings$Country=="Dominican Republic"]<-"DOM"
Country.Loadings$abbr[Country.Loadings$Country=="Ecuador"]<-"ECU"
Country.Loadings$abbr[Country.Loadings$Country=="El Salvador"]<-"ESV"
Country.Loadings$abbr[Country.Loadings$Country=="Guatemala"]<-"GTM"
Country.Loadings$abbr[Country.Loadings$Country=="Honduras"]<-"HND"
Country.Loadings$abbr[Country.Loadings$Country=="Mexico"]<-"MEX"
Country.Loadings$abbr[Country.Loadings$Country=="Nicaragua"]<-"NIC"
Country.Loadings$abbr[Country.Loadings$Country=="Panama"]<-"PAN"
Country.Loadings$abbr[Country.Loadings$Country=="Paraguay"]<-"PAR"
Country.Loadings$abbr[Country.Loadings$Country=="Peru"]<-"PER"
Country.Loadings$abbr[Country.Loadings$Country=="Uruguay"]<-"URU"
Country.Loadings$abbr[Country.Loadings$Country=="Venezuela"]<-"VEN"



activities=c("Municipal meeting", "Contacting municipality", "Contacting local authority", 
		"Contacting national authority",
             "Solving Problems", "Improvements meeting", 
		 "Party meeting", "Voting",
             "Sharing online", "Petitioning", "Protesting", "Blocking")




for (k in 4:6) {
Country.Loadings[,k]<-as.numeric(levels(Country.Loadings[,k]))[Country.Loadings[,k]]
}



first<-c(8,1,2,3,4,6)

for (i in 1:length(activities)) {
a<-subset(Country.Loadings[as.character(Country.Loadings$Activity)==activities[i],])

if (i==4) {
title="Contacting nat. authority"
} else if (i==5) {
title="Solving comm. problems" 
} else {
title=activities[i]
}

if (i %in%first) {
m<-c(0.25,0.5,0,0)
} else m<-c(0.25,0.5,0,0)


if (i <12) {
 p=ggplot(a, aes(x=abbr, y=Mean, group=Dimension, color=Dimension))+
geom_point()+
geom_errorbar(aes(ymin=X95.Low,ymax=X95.High),width=0.1,
position=position_dodge(0.05))+
 ggtitle(title)+
theme_bw()+ylab("")+xlab("")+ 
theme(axis.text.x=element_text(angle=90,size=6))+ylim(c(0,10))+
theme(plot.margin=unit(m, "lines"))+
theme(axis.text.x = element_text(size=7, vjust=0.2))+
theme(legend.position="none")+theme(plot.title = element_text(size = 11))+
scale_color_manual(values=c("black", "gray"))+theme(plot.title = element_text(hjust = 0))
assign(paste("plot",i,sep=""),p)
} else if (i==12) {
 p=ggplot(a, aes(x=abbr, y=Mean, group=Dimension, color=Dimension))+
geom_point()+
geom_errorbar(aes(ymin=X95.Low,ymax=X95.High),width=0.1,
position=position_dodge(0.05))+
 ggtitle(title)+
theme_bw()+ylab("")+xlab("")+ 
theme(axis.text.x=element_text(angle=90,size=6))+ylim(c(0,10))+
theme(plot.margin=unit(m, "lines"))+
theme(axis.text.x = element_text(size=7, vjust=0.2))+
theme(plot.title = element_text(size = 11))+
scale_color_manual(values=c("black", "gray"))+theme(plot.title = element_text(hjust = 0))+
theme(legend.position="none")
assign(paste("plot",i,sep=""),p)
}
}




# Multiple plot function
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


pdf(file = "Figure.A8.pdf")
grid.draw(multiplot(plot8, plot3, plot5, plot9,
   plot1, plot4, plot7, plot11,
   plot2, plot6, plot10, plot12,
	cols=3))
dev.off()


################################################# Figure A.9 ##################################################################################

rm(list=ls())
load("Alpha.all.LogitModel")


df.figureA9<-data.frame(Alpha.all)



df.figureA9$abbr<-NA
df.figureA9$abbr[df.figureA9$Country=="Argentina"]<-"ARG"
df.figureA9$abbr[df.figureA9$Country=="Brazil"]<-"BRA"
df.figureA9$abbr[df.figureA9$Country=="Chile"]<-"CHL"
df.figureA9$abbr[df.figureA9$Country=="Colombia"]<-"COL"
df.figureA9$abbr[df.figureA9$Country=="Costa Rica"]<-"CRI"
df.figureA9$abbr[df.figureA9$Country=="Dominican Republic"]<-"DOM"
df.figureA9$abbr[df.figureA9$Country=="Ecuador"]<-"ECU"
df.figureA9$abbr[df.figureA9$Country=="El Salvador"]<-"ESV"
df.figureA9$abbr[df.figureA9$Country=="Guatemala"]<-"GTM"
df.figureA9$abbr[df.figureA9$Country=="Honduras"]<-"HND"
df.figureA9$abbr[df.figureA9$Country=="Mexico"]<-"MEX"
df.figureA9$abbr[df.figureA9$Country=="Nicaragua"]<-"NIC"
df.figureA9$abbr[df.figureA9$Country=="Panama"]<-"PAN"
df.figureA9$abbr[df.figureA9$Country=="Paraguay"]<-"PAR"
df.figureA9$abbr[df.figureA9$Country=="Peru"]<-"PER"
df.figureA9$abbr[df.figureA9$Country=="Uruguay"]<-"URU"
df.figureA9$abbr[df.figureA9$Country=="Venezuela"]<-"VEN"


load("Alpha.means.LogitModel")
df.figureA9.2<-data.frame(Alpha.means)
df.figureA9.2$abbr<-"Average"

df.figureA9.3<-rbind(df.figureA9[,-1], df.figureA9.2)

df.figureA9.3$abbr<-factor(df.figureA9.3$abbr, 
levels=c(unique(df.figureA9$abbr), "Average"))

df.figureA9.3$Activity<-factor(df.figureA9.3$Activity,
levels=c("Voting", "Municipal meeting",
	"Contacting municipality", "Contacting local authority",
	"Contacting national authority",
	"Improvements meeting", "Solving Problems",
	"Party meeting",
	"Petitioning",  "Sharing online", "Protesting", "Blocking"),
labels=c("Voting", "Municipal meeting",
	"Contacting municipality", "Contacting local authority",
	"Contacting nat. authority",
	"Improvements meeting", "Solving comm. problems",
	"Party meeting", 
	"Petitioning", "Sharing online", "Protesting", "Blocking"))


for (k in 2:4){

df.figureA9.3[,k]<-as.numeric(levels(df.figureA9.3[,k]))[df.figureA9.3[,k]]
}

p<-ggplot(df.figureA9.3, aes(x=abbr, y=Mean, colour=abbr, fill==abbr))+
geom_point()+geom_errorbar(aes(ymin=X95.Low,ymax=X95.High),width=0.1,
position=position_dodge(0.05))+
facet_wrap(~Activity)+theme_bw()+
theme(axis.text.x = element_text(size=6, angle=90, vjust=0.2))+
theme(strip.text = element_text(size = 8))+
scale_color_manual(values=c(rep("gray",17),"black"))+
xlab("")+ ylab("")+theme(legend.position = "none") 


pdf(file = "Figure.A.9.pdf")
grid.draw(p)
dev.off()

################################################# Figure A.10 ##################################################################################

rm(list=ls())

load("Posterior.Probs.LogitModel")

load("Probs.Dimensions.LogitModel")

prob.conv.type<-Probs.Dimensions[,1]
prob.unconv.type<-Probs.Dimensions[,2]



per.outsiders <- round(colMeans(Posterior.Probs)[1], digits=2)
if (nchar(per.outsiders)<4) {
while (nchar(per.outsiders)<4) {
per.outsiders<-paste(per.outsiders, "0", sep="")
} 
}


per.agitators<- round(colMeans(Posterior.Probs)[2], digits=2)
if (nchar(per.agitators)<4) {
while (nchar(per.agitators)<4) {
per.agitators<-paste(per.agitators, "0", sep="")
} 
}


per.activists<- round(colMeans(Posterior.Probs)[4], digits=2)
if (nchar(per.activists)<4) {
while (nchar(per.activists)<4) {
per.activists<-paste(per.activists, "0", sep="")
} 
}

per.conventionals <-max(round(colMeans(Posterior.Probs)[3], digits=2), 1-round(colMeans(Posterior.Probs)[1], digits=2)-round(colMeans(Posterior.Probs)[2], digits=2)-round(colMeans(Posterior.Probs)[4], digits=2))
if (nchar(per.conventionals)<4) {
while (nchar(per.conventionals)<4) {
per.conventionals<-paste(per.conventionals, "0", sep="")
} 
}



Points<-cbind(prob.conv.type, prob.unconv.type)
Points<-data.frame(Points)
names(Points)<-c("x", "y")


p.1<-ggplot(Points, aes(x=x, y=y))+
geom_point(colour = "lightgray", size = 0.5)+theme_bw()+
xlab("Probability of high conventional type")+
ylab("Probability of high 
unconventional type")+
theme(axis.text.x = element_text(color="black", size=11))+
	theme(axis.text.x = element_text(color="black", size=11))+
 theme(axis.title.y = element_text(size=13, face="bold"))+
 theme(axis.title.x = element_text(size=13, face="bold"))+
 theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))+
 theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))+
 geom_vline(xintercept = 0.5, linetype="dashed")+
 geom_hline(yintercept = 0.5, linetype="dashed")+
annotate(geom="text", x=0.25, y=0.25, label=paste("Outsiders",
per.outsiders, sep="\n"), color="black", size=5)+
annotate(geom="text", x=0.75, y=0.25, label=paste("Conventionals",
per.conventionals, sep="\n"), color="black", size=5)+
annotate(geom="text", x=0.25, y=0.75, label=paste("Agitators",
per.agitators, sep="\n"), color="black", size=5)+
annotate(geom="text", x=0.75, y=0.75, label=paste("Activists",
per.activists, sep="\n"), color="black", size=5)#+
theme(plot.margin=unit(c(1.5,1,0,1), "lines"))



Types<-cbind(c("Outsider", "Agitator", "Conventional", "Activist"), colMeans(Posterior.Probs), t(apply(Posterior.Probs, 2, quantile, c(0.025,0.975))))
Types<-data.frame(Types)
names(Types)<-c("Type", "Mean", "Low", "High")

for (k in 2:ncol(Types)) {
Types[,k]<- as.numeric(levels(Types[,k]))[Types[,k]] 
}


Types$Type<- factor(Types$Type, 
	levels = c("Outsider", "Agitator", "Conventional",
		"Activist"))

p.2<- ggplot(Types, aes(x=Type, y=Mean)) + 
  geom_point(size=2, fill="black") + ylab("Probability of 
type assignment")+
	xlab("")+
  geom_errorbar(aes(ymin=Low, ymax=High), width=.1,
                 position=position_dodge(0.05))+
	theme_bw()+ ylim(c(0,1))+
	theme(axis.text.x = element_text(color="black", size=14))+
	theme(axis.text.x = element_text(color="black", size=11))+
 theme(axis.title.y = element_text(size=13, face="bold"))+
theme(plot.margin=unit(c(1.5,1,0,1), "lines"))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))


# Multiple plot function
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

pdf(file = "Figure.A.10.pdf", height=10)
grid.draw(multiplot(p.1, p.2, col=1))
dev.off()

################################################# Figure A.11 ##################################################################################

rm(list=ls())
load("Posterior.Probs.LogitModel")

Distributions<-Posterior.Probs


Distributions<-rbind(cbind("Outsider", Distributions[,1]), 
cbind("Agitator", Distributions[,2]),
cbind("Conventional", Distributions[,3]),
cbind("Activist", Distributions[,4]))


Distributions<-data.frame(Distributions)
names(Distributions)<-c("Type", "Density")



for (k in 2:ncol(Distributions)) {
Distributions[,k]<- as.numeric(levels(Distributions[,k]))[Distributions[,k]] 
}


p<-ggplot(Distributions, aes(x=Density, color=Type, fill=Type)) +
  geom_density()+ scale_fill_grey()+theme_bw()+
 ylab("")+xlab("Posterior probabilities of type assignment")+ theme(legend.position="bottom")+
 theme(legend.title = element_blank()) +
guides(fill=guide_legend(nrow=2,byrow=TRUE))+
scale_x_continuous(lim=c(0,1))+
theme(axis.text.y = element_blank())+
theme(legend.text = element_text(size=10), legend.title = element_text(size=10, colour="white"), legend.position="bottom", legend.title.align=0.5)+
theme(axis.title.x = element_text(size=13, face="bold"))+
theme(axis.text.x = element_text(size=11))+
theme(axis.ticks.y =element_blank())

pdf(file = "Figure.A.11.pdf")
grid.draw(p)
dev.off()



################################################# Figure A.12 ##################################################################################

rm(list=ls())

# Multiple plot function
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


load("Outsider.by.Country.LogitModel")
load("Agitator.by.Country.LogitModel")
load("Conventional.by.Country.LogitModel")
load("Activist.by.Country.LogitModel")

df.outsiders<-data.frame(cbind("Outsiders", Outsider.by.Country))
names(df.outsiders)<-c("Type", "Country", "Mean", "Low", "High")

df.agitators<-data.frame(cbind("Agitators", Agitator.by.Country))
names(df.agitators)<-c("Type", "Country", "Mean", "Low", "High")
 
 
df.conventionals<-data.frame(cbind("Conventionals", Conventional.by.Country))
names(df.conventionals)<-c("Type", "Country", "Mean", "Low", "High")

df.activists<-data.frame(cbind("Activists", Activist.by.Country))
names(df.activists)<-c("Type", "Country", "Mean", "Low", "High")
 
df.Figure.A.12<-rbind(df.outsiders,	df.agitators,
	 df.conventionals,
	 df.activists)



df.Figure.A.12$abbr<-NA
df.Figure.A.12$abbr[df.Figure.A.12$Country==1]<-"ARG"
df.Figure.A.12$abbr[df.Figure.A.12$Country==2]<-"BRA"
df.Figure.A.12$abbr[df.Figure.A.12$Country==3]<-"CHL"
df.Figure.A.12$abbr[df.Figure.A.12$Country==4]<-"COL"
df.Figure.A.12$abbr[df.Figure.A.12$Country==5]<-"CRI"
df.Figure.A.12$abbr[df.Figure.A.12$Country==6]<-"DOM"
df.Figure.A.12$abbr[df.Figure.A.12$Country==7]<-"ECU"
df.Figure.A.12$abbr[df.Figure.A.12$Country==8]<-"ESV"
df.Figure.A.12$abbr[df.Figure.A.12$Country==9]<-"GTM"
df.Figure.A.12$abbr[df.Figure.A.12$Country==10]<-"HND"
df.Figure.A.12$abbr[df.Figure.A.12$Country==11]<-"MEX"
df.Figure.A.12$abbr[df.Figure.A.12$Country==12]<-"NIC"
df.Figure.A.12$abbr[df.Figure.A.12$Country==13]<-"PAN"
df.Figure.A.12$abbr[df.Figure.A.12$Country==14]<-"PAR"
df.Figure.A.12$abbr[df.Figure.A.12$Country==15]<-"PER"
df.Figure.A.12$abbr[df.Figure.A.12$Country==16]<-"URU"
df.Figure.A.12$abbr[df.Figure.A.12$Country==17]<-"VEN"


for (k in 3:5){

df.Figure.A.12[,k]<-as.numeric(levels(df.Figure.A.12[,k]))[df.Figure.A.12[,k]]
}


Outsiders<-df.Figure.A.12[df.Figure.A.12$Type=="Outsiders",-c(1,2)]

Agitators<-df.Figure.A.12[df.Figure.A.12$Type=="Agitators",-c(1,2)]

Conventionals<-df.Figure.A.12[df.Figure.A.12$Type=="Conventionals",-c(1,2)]

Activists<-df.Figure.A.12[df.Figure.A.12$Type=="Activists",-c(1,2)]


p.1<-ggplot(Outsiders,  aes(as.factor(abbr), Mean)) +
 geom_point(size=1, fill="black") +
  geom_errorbar(aes(ymin=Low, ymax=High), width=.1,
                 position=position_dodge(0.05))+
	ylab("")+xlab("")+ggtitle("Outsiders")+theme_bw()+
	theme(plot.margin=unit(c(0.5,1,0,0), "lines"))+
	#scale_y_continuous(limits=c(0.7,0.83), breaks=c(0.7, 0.725, 0.75, 0.775, 0.80, 0.825))+
	theme(plot.title = element_text(hjust = 0))


p.2<-ggplot(Agitators,  aes(as.factor(abbr), Mean)) +
 geom_point(size=1, fill="black") +
  geom_errorbar(aes(ymin=Low, ymax=High), width=.1,
                 position=position_dodge(0.05))+
	ylab("")+xlab("")+ggtitle("Agitators")+theme_bw()+
	theme(plot.margin=unit(c(0.5,1,0,0), "lines"))+
	#scale_y_continuous(limits=c(0.015,0.075), breaks=seq(0.02, 0.07, by=0.01))+
	theme(plot.title = element_text(hjust = 0))


p.3<-ggplot(Conventionals,  aes(as.factor(abbr), Mean)) +
 geom_point(size=1, fill="black") +
  geom_errorbar(aes(ymin=Low, ymax=High), width=.1,
                 position=position_dodge(0.05))+
	ylab("")+xlab("")+ggtitle("Conventionals")+theme_bw()+
	theme(plot.margin=unit(c(0.5,1,0,0), "lines"))+
	#scale_y_continuous(limits=c(0.13,0.241), breaks=seq(0.14, 0.24, by=0.02))+
	theme(plot.title = element_text(hjust = 0))


p.4<-ggplot(Activists,  aes(as.factor(abbr), Mean)) +
 geom_point(size=1, fill="black") +
  geom_errorbar(aes(ymin=Low, ymax=High), width=.1,
                 position=position_dodge(0.05))+
	ylab("")+xlab("")+ggtitle("Activists")+theme_bw()+
	theme(plot.margin=unit(c(0.5,1,0,0), "lines"))+
	#scale_y_continuous(limits=c(0.005,0.04), breaks=c(0.01, 0.02, 0.03))+
	theme(plot.title = element_text(hjust = 0))




pdf(file = "Figure.A.12.pdf")
grid.draw(multiplot(p.1, p.2, p.3, p.4, cols=1))
dev.off()

################################################# Table A.8 ##################################################################################

rm(list=ls())
load("Posterior.Conv.Country.LogitModel")
load("Posterior.Unconv.Country.LogitModel")

AvePP<-c()

for (j in 1:17) {
dd.conv<-Posterior.Conv.Country[Posterior.Conv.Country[,1]==j,]
dd.unconv<-Posterior.Unconv.Country[Posterior.Unconv.Country[,1]==j,]

m.c<-round(mean(c(dd.conv[dd.conv[,2]==1,3], dd.conv[dd.conv[,2]==2,4])), digits=2)
m.u<-round(mean(c(dd.unconv[dd.unconv[,2]==1,3], dd.unconv[dd.unconv[,2]==2,4])), digits=2)
m.b<-round(mean(c(dd.conv[dd.conv[,2]==1,3], dd.conv[dd.conv[,2]==2,4], dd.unconv[dd.unconv[,2]==1,3], dd.unconv[dd.unconv[,2]==2,4])), digits=2)


if (nchar(m.c)<4 & m.c<1) {
while (nchar(m.c)<4) {
m.c<-paste(m.c, "0", sep="")
} 
}

if (m.c==1) {
m.c<-"1.00"
} 

if (nchar(m.u)<4 & m.u<1) {
while (nchar(m.u)<4) {
m.u<-paste(m.u, "0", sep="")
} 
}

if (m.u==1) {
m.u<-"1.00"
} 


if (nchar(m.b)<4 &  m.b<1) {
while (nchar(m.b)<4) {
m.b<-paste(m.b, "0", sep="")
}
} 

if (m.b==1) {
m.b<-"1.00"
} 



AvePP<-rbind(AvePP, c(j, m.c, m.u, m.b))
}

AvePP<-data.frame(AvePP)
names(AvePP)<-c("Country.Name", "Tc", "Tu", "Tc & Tu")

AvePP$abbr<-NA
AvePP$abbr[AvePP$Country==1]<-"ARG"
AvePP$abbr[AvePP$Country==2]<-"BRA"
AvePP$abbr[AvePP$Country==3]<-"CHL"
AvePP$abbr[AvePP$Country==4]<-"COL"
AvePP$abbr[AvePP$Country==5]<-"CRI"
AvePP$abbr[AvePP$Country==6]<-"DOM"
AvePP$abbr[AvePP$Country==7]<-"ECU"
AvePP$abbr[AvePP$Country==8]<-"ESV"
AvePP$abbr[AvePP$Country==9]<-"GTM"
AvePP$abbr[AvePP$Country==10]<-"HND"
AvePP$abbr[AvePP$Country==11]<-"MEX"
AvePP$abbr[AvePP$Country==12]<-"NIC"
AvePP$abbr[AvePP$Country==13]<-"PAN"
AvePP$abbr[AvePP$Country==14]<-"PAR"
AvePP$abbr[AvePP$Country==15]<-"PER"
AvePP$abbr[AvePP$Country==16]<-"URU"
AvePP$abbr[AvePP$Country==17]<-"VEN"



Table.A.8<-AvePP[,c(5,2:4)]
names(Table.A.8)<-c("Country.Name", "Tc", "Tu", "Tc & Tu")

print(Table.A.8)

Titles<-matrix(c("", "", "AvePP", ""),nrow=1)


g1 <- tableGrob(Table.A.8, rows = NULL)
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g1), l = 1, r = ncol(g1))
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 1, l = 1, r = ncol(g1))



g0 <- tableGrob(Titles, rows = NULL)
g0 <- gtable_add_grob(g0,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 1, l = 1, r = ncol(g0))

g<-rbind(g0,g1)

pdf(file = "Table.A.8.pdf", height=10, width=10)
grid.draw(g)
dev.off()


################################################# Figure A.13 ##################################################################################


rm(list=ls())
load("Marginal.Individual.LogitModel")
load("Marginal.Contextual.LogitModel")



Figure.Individual<-cbind(unlist(strsplit(rownames(Marginal.Individual), "-"))[seq(2,length(unlist(strsplit(rownames(Marginal.Individual), "-"))), by=2)],
unlist(strsplit(rownames(Marginal.Individual), "-"))[seq(1,length(unlist(strsplit(rownames(Marginal.Individual), "-"))), by=2)],
Marginal.Individual[,1:3])
Figure.Individual<-data.frame(Figure.Individual)
names(Figure.Individual)<-c("Type", "Variable", "Mean", "Low", "High")


Figure.Contextual<-cbind(unlist(strsplit(rownames(Marginal.Contextual), "-"))[seq(2,length(unlist(strsplit(rownames(Marginal.Contextual), "-"))), by=2)],
unlist(strsplit(rownames(Marginal.Contextual), "-"))[seq(1,length(unlist(strsplit(rownames(Marginal.Contextual), "-"))), by=2)],
Marginal.Contextual[,1:3])
Figure.Contextual<-data.frame(Figure.Contextual)
names(Figure.Contextual)<-c("Type", "Variable", "Mean", "Low", "High")



Figure.A.13<-rbind(Figure.Individual, Figure.Contextual)

Figure.A.13$Type<- factor(Figure.A.13$Type, 
	levels = c("Outsider",
		"Agitator",
		"Conventional", "Activist"))



Figure.A.13$Variable<-factor(Figure.A.13$Variable, 
	levels=rev(c("Age", "Female", "Education", 
			"Income", "Corruption", 
			"Victimization",
			"Ideology",
"Rule of Law", "Compulsory Voting", "ENPP", 
			"GDP", "Social.Spending")),
       labels=rev(c("Age",
			  "Female",
			  "Education", 
			  "Relative Income",
			  "Perceived Corruption", 
			   "Crime Victimization",
			 "Ideological Distance to Incumbent",
			"Rule of Law",
			  "Compulsory Voting",
			  "ENPP", 
			  "GDP per capita",
			  "Social Spending")))




for (k in 3:5){
Figure.A.13[,k]<-as.numeric(levels(Figure.A.13[,k]))[Figure.A.13[,k]]
}


p<-ggplot(Figure.A.13,
 aes(y=Variable, x=Mean)) + 
    geom_errorbarh(aes(xmin=Low, 
	xmax=High, height=.2), colour="black") + facet_wrap(~Type)+
    geom_point(size=1.5)+
theme_bw()+xlab("Marginal effects (in percentage points)")+ylab("")+
geom_hline(yintercept=0, linetype="longdash", colour="gray40")+
theme(strip.text.x = element_text(size = 12, face="bold"))+
geom_vline(xintercept=0, linetype="longdash", colour="gray40")+
theme(strip.background = element_rect(fill = alpha('white'))) +
theme(axis.title.x=element_text(vjust=2, size=11))+
theme(axis.text.y=element_text(size=11))


pdf(file = "Figure.A.13.pdf")
grid.draw(p)
dev.off()

################################################# Figure A.14 ##################################################################################

rm(list=ls())
load("Marginal.Country.LogitModel")
Figure.A.14<-cbind(unlist(strsplit(rownames(Marginal.Country), "-"))[seq(2,length(unlist(strsplit(rownames(Marginal.Country), "-"))), by=2)],
unlist(strsplit(rownames(Marginal.Country), "-"))[seq(1,length(unlist(strsplit(rownames(Marginal.Country), "-"))), by=2)],
Marginal.Country[,1:3])
Figure.A.14<-data.frame(Figure.A.14)
names(Figure.A.14)<-c("Type", "Country", "Mean", "Low", "High")



Figure.A.14$Type<- factor(Figure.A.14$Type, 
	levels = c("Outsider",
		"Agitator",
		"Conventional", "Activist"))


Figure.A.14$Country<- factor(Figure.A.14$Country, 
	levels=rev(c("Argentina", "Brazil", "Chile", "Colombia", "Costa Rica", "Dominican Republic",
		"Ecuador", "El Salvador", "Guatemala", "Honduras", "Mexico", "Nicaragua", "Panama",
		"Paraguay", "Peru", "Uruguay", "Venezuela")))



for (k in 3:5){

Figure.A.14[,k]<-as.numeric(levels(Figure.A.14[,k]))[Figure.A.14[,k]]
}


p<-ggplot(Figure.A.14,
 aes(y=Country, x=Mean)) + 
    geom_errorbarh(aes(xmin=Low, 
	xmax=High, height=.2), colour="black") + facet_wrap(~Type, scales="free_x")+
    geom_point(size=1.5)+
theme_bw()+xlab("Marginal effects (in percentage points)")+ylab("")+
geom_hline(yintercept=0, linetype="longdash", colour="gray40")+
theme(strip.text.x = element_text(size = 12, face="bold"))+
geom_vline(xintercept=0, linetype="longdash", colour="gray40")+
theme(strip.background = element_rect(fill = alpha('white'))) +
theme(axis.title.x=element_text(vjust=2, size=11))+
theme(axis.text.y=element_text(size=11))




pdf(file = "Figure.A.14.pdf")
grid.draw(p)
dev.off()

################################################# SECTION A.3.3 ##################################################################################
################################################# Figure A.15 ##################################################################################


rm(list=ls())
load("dataset_2012.RData")


df<-subset(dataset, select=c("country.name", "ActMeetMun",
"country.name", "ActMeetMun", "ActContMun",  "ActContAut" ,                           
"ActMeetImp" , "ActSolveProb" ,  "ActMeetPty" ,  "ActContGvt",                            
"ActProtest" , "ActBlock" ,  "ActVoted",  "ActSign" ,                              
"ActShare" , "education" ,  "female", "age", "incomeproxy" ,
"victim", "corrupt", "spatial.distance" ,
"compulsory.voting", "gdp.pc", "rule.of.lw", "enp", "social.spending"))                       
df<-na.exclude(df)


Y <- cbind(df$ActMeetMun, df$ActContMun, df$ActContAut,
		 df$ActContGvt, df$ActSolveProb, 
		df$ActMeetImp, df$ActMeetPty, df$ActVoted, 
		df$ActShare, df$ActSign, df$ActProtest, df$ActBlock)
colnames(Y)<-c("ActMeetMun", "ActContMun", "ActContAut", "ActContGvt",
		"ActSolveProb", "ActMeetImp", "ActMeetPty", "ActVoted",
		"ActShare", "ActSign", "ActProtest", "ActBlock")

Country<-as.numeric(factor(df$country.name), levels=unique(df$country.name))
		

Matrix.Country<-matrix(0,nrow(Y),length(unique(Country)))           
for (i in 1:length(unique(Country))) Matrix.Country[Country==i,i]<-1

df$age.centered<-(df$age-mean(df$age))/(2*sd(df$age))
df$education.centered<-(df$education-mean(df$education))/(2*sd(df$education))


df$incomeproxy.centered<-(df$incomeproxy-mean(df$incomeproxy))/(2*sd(df$incomeproxy))


df$spatial.distance.centered<-(df$spatial.distance-mean(df$spatial.distance))/(2*sd(df$spatial.distance))

X<-cbind(1, df$age.centered, df$female, df$education.centered, 
	df$incomeproxy.centered, 
	df$victim, df$corrupt, 
	df$spatial.distance.centered,
	(df$rule.of.lw-mean(df$rule.of.lw))/(2*sd(df$rule.of.lw)),
		df$compulsory.voting,
		(df$enp-mean(df$enp))/(2*sd(df$enp)),
		(df$gdp.pc-mean(df$gdp.pc))/(2*sd(df$gdp.pc)),
		(df$social.spending-mean(df$social.spending))/(2*sd(df$social.spending))
)



load("Results_IncomeproxyModel")

Alpha<-mcmc.list(as.mcmc(t(Results$alpha[,,1])), 
		as.mcmc(t(Results$alpha[,,2])),
		as.mcmc(t(Results$alpha[,,3])))
print(paste("Convergence Alpha:", gelman.diag(Alpha)[[2]]<1.2, sep=" "))


Alpha.Conv<-mcmc.list(as.mcmc(t(Results$alpha_conv[-seq(12,dim(Results$alpha_conv)[1],by=12),,1])), 
		as.mcmc(t(Results$alpha_conv[-seq(12,dim(Results$alpha_conv)[1],by=12),,2])),
		as.mcmc(t(Results$alpha_conv[-seq(12,dim(Results$alpha_conv)[1],by=12),,3])))
print(paste("Convergence Alpha Conv:", gelman.diag(Alpha.Conv)[[2]]<1.2, sep=" "))


Alpha.Unconv<-mcmc.list(as.mcmc(t(Results$alpha_unconv[-seq(1,dim(Results$alpha_unconv)[1],by=12),,1])), 
		as.mcmc(t(Results$alpha_unconv[-seq(1,dim(Results$alpha_unconv)[1],by=12),,2])),
		as.mcmc(t(Results$alpha_unconv[-seq(1,dim(Results$alpha_unconv)[1],by=12),,3])))
print(paste("Convergence Alpha Unconventional:", gelman.diag(Alpha.Unconv)[[2]]<1.2, sep=" "))



P.Conv<-mcmc.list(as.mcmc((Results$P_conv[2,,1])), 
		as.mcmc((Results$P_conv[2,,2])),
		as.mcmc((Results$P_conv[2,,3])))
print(paste("Convergence Prob. Conventional:", gelman.diag(P.Conv)[[1]][,1]<1.2, sep=" "))


P.Unconv<-mcmc.list(as.mcmc((Results$P_unconv[2,,1])), 
		as.mcmc((Results$P_unconv[2,,2])),
		as.mcmc((Results$P_unconv[2,,3])))
print(paste("Convergence Prob. Unconventional", gelman.diag(P.Unconv)[[1]][,1]<1.2, sep=" "))



BETA2<-rbind((t(Results$beta2[,,1])),
	(t(Results$beta2[,,2])),
	(t(Results$beta2[,,3])))
BETA3<-rbind((t(Results$beta3[,,1])),
	(t(Results$beta3[,,2])),
	(t(Results$beta3[,,3])))

BETA4<-rbind((t(Results$beta4[,,1])),
	(t(Results$beta4[,,2])),
	(t(Results$beta4[,,3])))

NU<-rbind((t(Results$nu[,,1])),
	(t(Results$nu[,,2])),
	(t(Results$nu[,,3])))
NU2<-NU[, 1:length(unique(Country))]
NU3<-NU[, (1+length(unique(Country))): (2*length(unique(Country)))]
NU4<-NU[, (1+2*length(unique(Country))): (3*length(unique(Country)))]



Beta.Agitator<-BETA2[seq(1, nrow(BETA2), length.out=1000),]
Beta.Conventional<-BETA3[seq(1, nrow(BETA3), length.out=1000),]
Beta.Activist<-BETA4[seq(1, nrow(BETA4), length.out=1000),]
Nu.Agitator<-NU2[seq(1, nrow(NU2), length.out=1000),]
Nu.Conventional<-NU3[seq(1, nrow(NU3), length.out=1000),]
Nu.Activist<-NU4[seq(1, nrow(NU4), length.out=1000),]

# Marginal covariate effects
colnames(X)<-c("Intercept", "Age", "Female", "Education", 
		 "Income", "Victimization", "Corruption",
		  "Ideology", "Rule of Law", "Compulsory Voting", "ENPP",
		"GDP", "Social.Spending")


## Individual Effects
dummies<-c(3, 6)
continuous<-c(2,4,5,8)
ordinal<-7


Marginal.Individual<-matrix(0, nrow=4*(length(dummies)+length(continuous)+length(ordinal)), 3)
colnames(Marginal.Individual)<-c("Mean", "95.Low", "95.High")
rownames(Marginal.Individual)<-paste(rep(colnames(X)[sort(c(dummies, continuous, ordinal))], each=4),
rep(c("Outsider", "Agitator", "Conventional", "Activist"), (length(dummies)+length(continuous)+length(ordinal))), 
sep="-")

for (j in 1:length(dummies)) {
X.0<-X
X.1<-X

X.0[,dummies[j]]<-0
X.1[,dummies[j]]<-1


prob.Agitator.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Outsider.0<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Agitator.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))


prob.Outsider.1<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))


Marginal.Individual[grep(colnames(X)[dummies[j]], rownames(Marginal.Individual))[1],]<-c(mean(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0)),
											quantile(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0), c(0.025,0.975)))

Marginal.Individual[grep(colnames(X)[dummies[j]], rownames(Marginal.Individual))[2],]<-c(mean(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0)), quantile(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0), c(0.025,0.975)))


Marginal.Individual[grep(colnames(X)[dummies[j]], rownames(Marginal.Individual))[3],]<-c(mean(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0)), quantile(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0), c(0.025,0.975)))


Marginal.Individual[grep(colnames(X)[dummies[j]], rownames(Marginal.Individual))[4],]<-c(mean(colMeans(prob.Activist.1)-colMeans(prob.Activist.0)), quantile(colMeans(prob.Activist.1)-colMeans(prob.Activist.0), c(0.025,0.975)))

}


for (j in 1:length(continuous)){
X.0<-X
X.1<-X
X.1[,continuous[j]]<-X.0[,continuous[j]]+sd(X.0[,continuous[j]])


prob.Agitator.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Outsider.0<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Agitator.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))


prob.Outsider.1<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))



Marginal.Individual[grep(colnames(X)[continuous[j]], rownames(Marginal.Individual))[1],]<-c(mean(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0)),
											quantile(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0), c(0.025,0.975)))

Marginal.Individual[grep(colnames(X)[continuous[j]], rownames(Marginal.Individual))[2],]<-c(mean(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0)), quantile(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0), c(0.025,0.975)))


Marginal.Individual[grep(colnames(X)[continuous[j]], rownames(Marginal.Individual))[3],]<-c(mean(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0)), quantile(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0), c(0.025,0.975)))


Marginal.Individual[grep(colnames(X)[continuous[j]], rownames(Marginal.Individual))[4],]<-c(mean(colMeans(prob.Activist.1)-colMeans(prob.Activist.0)), quantile(colMeans(prob.Activist.1)-colMeans(prob.Activist.0), c(0.025,0.975)))

}

for (j in 1:length(ordinal)){
X.0<-X
X.1<-X
X.1[,ordinal[j]]<-X.0[,ordinal[j]]+1


prob.Agitator.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Outsider.0<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Agitator.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))


prob.Outsider.1<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))



Marginal.Individual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Individual))[1],]<-c(mean(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0)),
											quantile(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0), c(0.025,0.975)))

Marginal.Individual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Individual))[2],]<-c(mean(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0)), quantile(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0), c(0.025,0.975)))


Marginal.Individual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Individual))[3],]<-c(mean(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0)), quantile(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0), c(0.025,0.975)))


Marginal.Individual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Individual))[4],]<-c(mean(colMeans(prob.Activist.1)-colMeans(prob.Activist.0)), quantile(colMeans(prob.Activist.1)-colMeans(prob.Activist.0), c(0.025,0.975)))

}


Marginal.Individual[,1:3]<-100*Marginal.Individual[,1:3]
Marginal.Individual[grep("Ideology", rownames(Marginal.Individual)),1:3]<-(Marginal.Individual[grep("Ideology", rownames(Marginal.Individual)),1:3]/sd(X[,grep("Ideology", colnames(X))]))


continuous<-c(9, 11:13)
ordinal<-10


Marginal.Contextual<-matrix(0, nrow=4*(length(continuous)+length(ordinal)), 3)
colnames(Marginal.Contextual)<-c("Mean", "95.Low", "95.High")
rownames(Marginal.Contextual)<-paste(rep(colnames(X)[sort(c(continuous, ordinal))], each=4),
rep(c("Outsider", "Agitator", "Conventional", "Activist"), (length(continuous)+length(ordinal))), 
sep="-")



for (j in 1:length(continuous)){
X.0<-X
X.1<-X
X.1[,continuous[j]]<-X.0[,continuous[j]]+sd(X.0[,continuous[j]])


prob.Agitator.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Outsider.0<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Agitator.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))


prob.Outsider.1<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))



Marginal.Contextual[grep(colnames(X)[continuous[j]], rownames(Marginal.Contextual))[1],]<-c(mean(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0)),
											quantile(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0), c(0.025,0.975)))

Marginal.Contextual[grep(colnames(X)[continuous[j]], rownames(Marginal.Contextual))[2],]<-c(mean(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0)), quantile(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0), c(0.025,0.975)))


Marginal.Contextual[grep(colnames(X)[continuous[j]], rownames(Marginal.Contextual))[3],]<-c(mean(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0)), quantile(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0), c(0.025,0.975)))


Marginal.Contextual[grep(colnames(X)[continuous[j]], rownames(Marginal.Contextual))[4],]<-c(mean(colMeans(prob.Activist.1)-colMeans(prob.Activist.0)), quantile(colMeans(prob.Activist.1)-colMeans(prob.Activist.0), c(0.025,0.975)))

}

for (j in 1:length(ordinal)){
X.0<-X
X.1<-X
X.1[,ordinal[j]]<-X.0[,ordinal[j]]+1


prob.Agitator.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Outsider.0<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Agitator.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))


prob.Outsider.1<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))



Marginal.Contextual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Contextual))[1],]<-c(mean(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0)),
											quantile(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0), c(0.025,0.975)))

Marginal.Contextual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Contextual))[2],]<-c(mean(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0)), quantile(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0), c(0.025,0.975)))


Marginal.Contextual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Contextual))[3],]<-c(mean(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0)), quantile(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0), c(0.025,0.975)))


Marginal.Contextual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Contextual))[4],]<-c(mean(colMeans(prob.Activist.1)-colMeans(prob.Activist.0)), quantile(colMeans(prob.Activist.1)-colMeans(prob.Activist.0), c(0.025,0.975)))

}

Marginal.Contextual[,1:3]<-100*Marginal.Contextual[,1:3]



Figure.Individual<-cbind(unlist(strsplit(rownames(Marginal.Individual), "-"))[seq(2,length(unlist(strsplit(rownames(Marginal.Individual), "-"))), by=2)],
unlist(strsplit(rownames(Marginal.Individual), "-"))[seq(1,length(unlist(strsplit(rownames(Marginal.Individual), "-"))), by=2)],
Marginal.Individual[,1:3])
Figure.Individual<-data.frame(Figure.Individual)
names(Figure.Individual)<-c("Type", "Variable", "Mean", "Low", "High")


Figure.Contextual<-cbind(unlist(strsplit(rownames(Marginal.Contextual), "-"))[seq(2,length(unlist(strsplit(rownames(Marginal.Contextual), "-"))), by=2)],
unlist(strsplit(rownames(Marginal.Contextual), "-"))[seq(1,length(unlist(strsplit(rownames(Marginal.Contextual), "-"))), by=2)],
Marginal.Contextual[,1:3])
Figure.Contextual<-data.frame(Figure.Contextual)
names(Figure.Contextual)<-c("Type", "Variable", "Mean", "Low", "High")


Figure.A.15<-rbind(Figure.Individual, Figure.Contextual)

Figure.A.15$Type<- factor(Figure.A.15$Type, 
	levels = c("Outsider",
		"Agitator",
		"Conventional", "Activist"))



Figure.A.15$Variable<-factor(Figure.A.15$Variable, 
	levels=rev(c("Age", "Female", "Education", 
			"Income", "Corruption", 
			"Victimization",
			"Ideology",
		"Rule of Law", "Compulsory Voting", "ENPP", 
			"GDP", "Social.Spending")),
       labels=rev(c("Age",
			  "Female",
			  "Education", 
			  "Income (proxy)",
			  "Perceived Corruption", 
			   "Crime Victimization",
			 "Ideological Distance to Incumbent",
			"Rule of Law",
			  "Compulsory Voting",
			  "ENPP", 
			  "GDP per capita",
			  "Social Spending")))




for (k in 3:5){
Figure.A.15[,k]<-as.numeric(levels(Figure.A.15[,k]))[Figure.A.15[,k]]
}

p<-ggplot(Figure.A.15,
 aes(y=Variable, x=Mean)) + 
    geom_errorbarh(aes(xmin=Low, 
	xmax=High, height=.2), colour="black") + facet_wrap(~Type)+
    geom_point(size=1.5)+
theme_bw()+xlab("Marginal effects (in percentage points)")+ylab("")+
geom_hline(yintercept=0, linetype="longdash", colour="gray40")+
theme(strip.text.x = element_text(size = 12, face="bold"))+
geom_vline(xintercept=0, linetype="longdash", colour="gray40")+
theme(strip.background = element_rect(fill = alpha('white'))) +
theme(axis.title.x=element_text(vjust=2, size=11))+
theme(axis.text.y=element_text(size=11))


pdf(file = "Figure.A.15.pdf")
grid.draw(p)
dev.off()

################################################# Figure A.16 ##################################################################################


rm(list=ls())
load("dataset_2012.RData")



df<-subset(dataset, select=c("country.name", "ActMeetMun",
"country.name", "ActMeetMun", "ActContMun",  "ActContAut" ,                           
"ActMeetImp" , "ActSolveProb" ,  "ActMeetPty" ,  "ActContGvt",                            
"ActProtest" , "ActBlock" ,  "ActVoted",  "ActSign" ,                              
"ActShare" , "education" ,  "female", "age", "income"  ,
"victim", "corrupt", "spatial.distance" ,
"compulsory.voting", "gdp.pc", "rule.of.lw", "enp", "social.spending"))                       
df<-na.exclude(df)


Y <- cbind(df$ActMeetMun, df$ActContMun, df$ActContAut,
		 df$ActContGvt, df$ActSolveProb, 
		df$ActMeetImp, df$ActMeetPty, df$ActVoted, 
		df$ActShare, df$ActSign, df$ActProtest, df$ActBlock)
colnames(Y)<-c("ActMeetMun", "ActContMun", "ActContAut", "ActContGvt",
		"ActSolveProb", "ActMeetImp", "ActMeetPty", "ActVoted",
		"ActShare", "ActSign", "ActProtest", "ActBlock")

Country<-as.numeric(factor(df$country.name), levels=unique(df$country.name))
		

Matrix.Country<-matrix(0,nrow(Y),length(unique(Country)))           
for (i in 1:length(unique(Country))) Matrix.Country[Country==i,i]<-1

df$age.centered<-(df$age-mean(df$age))/(2*sd(df$age))
df$education.centered<-(df$education-mean(df$education))/(2*sd(df$education))


#df$income.centered<-(df$income-mean(df$income))/(2*sd(df$income))

df$income.abovemean<-ifelse(df$income>mean(df$income),1,0)


df$spatial.distance.centered<-(df$spatial.distance-mean(df$spatial.distance))/(2*sd(df$spatial.distance))

X<-cbind(1, df$age.centered, df$female, df$education.centered, 
	df$income.abovemean, 
	df$victim, df$corrupt, 
	df$spatial.distance.centered,
	(df$rule.of.lw-mean(df$rule.of.lw))/(2*sd(df$rule.of.lw)),
		df$compulsory.voting,
		(df$enp-mean(df$enp))/(2*sd(df$enp)),
		(df$gdp.pc-mean(df$gdp.pc))/(2*sd(df$gdp.pc)),
		(df$social.spending-mean(df$social.spending))/(2*sd(df$social.spending))
)


load("Results_IncomeaboveaverageModel")

Alpha<-mcmc.list(as.mcmc(t(Results$alpha[,,1])), 
		as.mcmc(t(Results$alpha[,,2])),
		as.mcmc(t(Results$alpha[,,3])))
print(paste("Convergence Alpha:", gelman.diag(Alpha)[[2]]<1.2, sep=" "))


Alpha.Conv<-mcmc.list(as.mcmc(t(Results$alpha_conv[-seq(12,dim(Results$alpha_conv)[1],by=12),,1])), 
		as.mcmc(t(Results$alpha_conv[-seq(12,dim(Results$alpha_conv)[1],by=12),,2])),
		as.mcmc(t(Results$alpha_conv[-seq(12,dim(Results$alpha_conv)[1],by=12),,3])))
print(paste("Convergence Alpha Conv:", gelman.diag(Alpha.Conv)[[2]]<1.2, sep=" "))


Alpha.Unconv<-mcmc.list(as.mcmc(t(Results$alpha_unconv[-seq(1,dim(Results$alpha_unconv)[1],by=12),,1])), 
		as.mcmc(t(Results$alpha_unconv[-seq(1,dim(Results$alpha_unconv)[1],by=12),,2])),
		as.mcmc(t(Results$alpha_unconv[-seq(1,dim(Results$alpha_unconv)[1],by=12),,3])))
print(paste("Convergence Alpha Unconventional:", gelman.diag(Alpha.Unconv)[[2]]<1.2, sep=" "))



P.Conv<-mcmc.list(as.mcmc((Results$P_conv[2,,1])), 
		as.mcmc((Results$P_conv[2,,2])),
		as.mcmc((Results$P_conv[2,,3])))
print(paste("Convergence Prob. Conventional:", gelman.diag(P.Conv)[[1]][,1]<1.2, sep=" "))


P.Unconv<-mcmc.list(as.mcmc((Results$P_unconv[2,,1])), 
		as.mcmc((Results$P_unconv[2,,2])),
		as.mcmc((Results$P_unconv[2,,3])))
print(paste("Convergence Prob. Unconventional", gelman.diag(P.Unconv)[[1]][,1]<1.2, sep=" "))



BETA2<-rbind((t(Results$beta2[,,1])),
	(t(Results$beta2[,,2])),
	(t(Results$beta2[,,3])))
BETA3<-rbind((t(Results$beta3[,,1])),
	(t(Results$beta3[,,2])),
	(t(Results$beta3[,,3])))

BETA4<-rbind((t(Results$beta4[,,1])),
	(t(Results$beta4[,,2])),
	(t(Results$beta4[,,3])))

NU<-rbind((t(Results$nu[,,1])),
	(t(Results$nu[,,2])),
	(t(Results$nu[,,3])))
NU2<-NU[, 1:length(unique(Country))]
NU3<-NU[, (1+length(unique(Country))): (2*length(unique(Country)))]
NU4<-NU[, (1+2*length(unique(Country))): (3*length(unique(Country)))]



Beta.Agitator<-BETA2[seq(1, nrow(BETA2), length.out=1000),]
Beta.Conventional<-BETA3[seq(1, nrow(BETA3), length.out=1000),]
Beta.Activist<-BETA4[seq(1, nrow(BETA4), length.out=1000),]
Nu.Agitator<-NU2[seq(1, nrow(NU2), length.out=1000),]
Nu.Conventional<-NU3[seq(1, nrow(NU3), length.out=1000),]
Nu.Activist<-NU4[seq(1, nrow(NU4), length.out=1000),]

# Marginal covariate effects
colnames(X)<-c("Intercept", "Age", "Female", "Education", 
		 "Income", "Victimization", "Corruption",
		  "Ideology", "Rule of Law", "Compulsory Voting", "ENPP",
		"GDP", "Social.Spending")


## Individual Effects
dummies<-c(3, 5, 6)
continuous<-c(2,4,8)
ordinal<-7



Marginal.Individual<-matrix(0, nrow=4*(length(dummies)+length(continuous)+length(ordinal)), 3)
colnames(Marginal.Individual)<-c("Mean", "95.Low", "95.High")
rownames(Marginal.Individual)<-paste(rep(colnames(X)[sort(c(dummies, continuous, ordinal))], each=4),
rep(c("Outsider", "Agitator", "Conventional", "Activist"), (length(dummies)+length(continuous)+length(ordinal))), 
sep="-")

for (j in 1:length(dummies)) {
X.0<-X
X.1<-X

X.0[,dummies[j]]<-0
X.1[,dummies[j]]<-1


prob.Agitator.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Outsider.0<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Agitator.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))


prob.Outsider.1<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))


Marginal.Individual[grep(colnames(X)[dummies[j]], rownames(Marginal.Individual))[1],]<-c(mean(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0)),
											quantile(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0), c(0.025,0.975)))

Marginal.Individual[grep(colnames(X)[dummies[j]], rownames(Marginal.Individual))[2],]<-c(mean(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0)), quantile(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0), c(0.025,0.975)))


Marginal.Individual[grep(colnames(X)[dummies[j]], rownames(Marginal.Individual))[3],]<-c(mean(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0)), quantile(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0), c(0.025,0.975)))


Marginal.Individual[grep(colnames(X)[dummies[j]], rownames(Marginal.Individual))[4],]<-c(mean(colMeans(prob.Activist.1)-colMeans(prob.Activist.0)), quantile(colMeans(prob.Activist.1)-colMeans(prob.Activist.0), c(0.025,0.975)))

}


for (j in 1:length(continuous)){
X.0<-X
X.1<-X
X.1[,continuous[j]]<-X.0[,continuous[j]]+sd(X.0[,continuous[j]])


prob.Agitator.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Outsider.0<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Agitator.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))


prob.Outsider.1<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))



Marginal.Individual[grep(colnames(X)[continuous[j]], rownames(Marginal.Individual))[1],]<-c(mean(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0)),
											quantile(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0), c(0.025,0.975)))

Marginal.Individual[grep(colnames(X)[continuous[j]], rownames(Marginal.Individual))[2],]<-c(mean(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0)), quantile(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0), c(0.025,0.975)))


Marginal.Individual[grep(colnames(X)[continuous[j]], rownames(Marginal.Individual))[3],]<-c(mean(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0)), quantile(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0), c(0.025,0.975)))


Marginal.Individual[grep(colnames(X)[continuous[j]], rownames(Marginal.Individual))[4],]<-c(mean(colMeans(prob.Activist.1)-colMeans(prob.Activist.0)), quantile(colMeans(prob.Activist.1)-colMeans(prob.Activist.0), c(0.025,0.975)))

}

for (j in 1:length(ordinal)){
X.0<-X
X.1<-X
X.1[,ordinal[j]]<-X.0[,ordinal[j]]+1


prob.Agitator.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Outsider.0<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Agitator.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))


prob.Outsider.1<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))



Marginal.Individual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Individual))[1],]<-c(mean(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0)),
											quantile(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0), c(0.025,0.975)))

Marginal.Individual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Individual))[2],]<-c(mean(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0)), quantile(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0), c(0.025,0.975)))


Marginal.Individual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Individual))[3],]<-c(mean(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0)), quantile(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0), c(0.025,0.975)))


Marginal.Individual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Individual))[4],]<-c(mean(colMeans(prob.Activist.1)-colMeans(prob.Activist.0)), quantile(colMeans(prob.Activist.1)-colMeans(prob.Activist.0), c(0.025,0.975)))

}


Marginal.Individual[,1:3]<-100*Marginal.Individual[,1:3]
Marginal.Individual[grep("Ideology", rownames(Marginal.Individual)),1:3]<-(Marginal.Individual[grep("Ideology", rownames(Marginal.Individual)),1:3]/sd(X[,grep("Ideology", colnames(X))]))


continuous<-c(9, 11:13)
ordinal<-10


Marginal.Contextual<-matrix(0, nrow=4*(length(continuous)+length(ordinal)), 3)
colnames(Marginal.Contextual)<-c("Mean", "95.Low", "95.High")
rownames(Marginal.Contextual)<-paste(rep(colnames(X)[sort(c(continuous, ordinal))], each=4),
rep(c("Outsider", "Agitator", "Conventional", "Activist"), (length(continuous)+length(ordinal))), 
sep="-")



for (j in 1:length(continuous)){
X.0<-X
X.1<-X
X.1[,continuous[j]]<-X.0[,continuous[j]]+sd(X.0[,continuous[j]])


prob.Agitator.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Outsider.0<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Agitator.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))


prob.Outsider.1<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))



Marginal.Contextual[grep(colnames(X)[continuous[j]], rownames(Marginal.Contextual))[1],]<-c(mean(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0)),
											quantile(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0), c(0.025,0.975)))

Marginal.Contextual[grep(colnames(X)[continuous[j]], rownames(Marginal.Contextual))[2],]<-c(mean(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0)), quantile(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0), c(0.025,0.975)))


Marginal.Contextual[grep(colnames(X)[continuous[j]], rownames(Marginal.Contextual))[3],]<-c(mean(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0)), quantile(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0), c(0.025,0.975)))


Marginal.Contextual[grep(colnames(X)[continuous[j]], rownames(Marginal.Contextual))[4],]<-c(mean(colMeans(prob.Activist.1)-colMeans(prob.Activist.0)), quantile(colMeans(prob.Activist.1)-colMeans(prob.Activist.0), c(0.025,0.975)))

}

for (j in 1:length(ordinal)){
X.0<-X
X.1<-X
X.1[,ordinal[j]]<-X.0[,ordinal[j]]+1


prob.Agitator.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Outsider.0<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Agitator.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))


prob.Outsider.1<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))



Marginal.Contextual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Contextual))[1],]<-c(mean(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0)),
											quantile(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0), c(0.025,0.975)))

Marginal.Contextual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Contextual))[2],]<-c(mean(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0)), quantile(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0), c(0.025,0.975)))


Marginal.Contextual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Contextual))[3],]<-c(mean(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0)), quantile(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0), c(0.025,0.975)))


Marginal.Contextual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Contextual))[4],]<-c(mean(colMeans(prob.Activist.1)-colMeans(prob.Activist.0)), quantile(colMeans(prob.Activist.1)-colMeans(prob.Activist.0), c(0.025,0.975)))

}

Marginal.Contextual[,1:3]<-100*Marginal.Contextual[,1:3]



Figure.Individual<-cbind(unlist(strsplit(rownames(Marginal.Individual), "-"))[seq(2,length(unlist(strsplit(rownames(Marginal.Individual), "-"))), by=2)],
unlist(strsplit(rownames(Marginal.Individual), "-"))[seq(1,length(unlist(strsplit(rownames(Marginal.Individual), "-"))), by=2)],
Marginal.Individual[,1:3])
Figure.Individual<-data.frame(Figure.Individual)
names(Figure.Individual)<-c("Type", "Variable", "Mean", "Low", "High")


Figure.Contextual<-cbind(unlist(strsplit(rownames(Marginal.Contextual), "-"))[seq(2,length(unlist(strsplit(rownames(Marginal.Contextual), "-"))), by=2)],
unlist(strsplit(rownames(Marginal.Contextual), "-"))[seq(1,length(unlist(strsplit(rownames(Marginal.Contextual), "-"))), by=2)],
Marginal.Contextual[,1:3])
Figure.Contextual<-data.frame(Figure.Contextual)
names(Figure.Contextual)<-c("Type", "Variable", "Mean", "Low", "High")


Figure.A.16<-rbind(Figure.Individual, Figure.Contextual)

Figure.A.16$Type<- factor(Figure.A.16$Type, 
	levels = c("Outsider",
		"Agitator",
		"Conventional", "Activist"))



Figure.A.16$Variable<-factor(Figure.A.16$Variable, 
	levels=rev(c("Age", "Female", "Education", 
			"Income", "Corruption", 
			"Victimization",
			"Ideology",
		"Rule of Law", "Compulsory Voting", "ENPP", 
			"GDP", "Social.Spending")),
       labels=rev(c("Age",
			  "Female",
			  "Education", 
			  "Above-average (monetary) Income",
			  "Perceived Corruption", 
			   "Crime Victimization",
			 "Ideological Distance to Incumbent",
			"Rule of Law",
			  "Compulsory Voting",
			  "ENPP", 
			  "GDP per capita",
			  "Social Spending")))




for (k in 3:5){
Figure.A.16[,k]<-as.numeric(levels(Figure.A.16[,k]))[Figure.A.16[,k]]
}

p<-ggplot(Figure.A.16,
 aes(y=Variable, x=Mean)) + 
    geom_errorbarh(aes(xmin=Low, 
	xmax=High, height=.2), colour="black") + facet_wrap(~Type)+
    geom_point(size=1.5)+
theme_bw()+xlab("Marginal effects (in percentage points)")+ylab("")+
geom_hline(yintercept=0, linetype="longdash", colour="gray40")+
theme(strip.text.x = element_text(size = 12, face="bold"))+
geom_vline(xintercept=0, linetype="longdash", colour="gray40")+
theme(strip.background = element_rect(fill = alpha('white'))) +
theme(axis.title.x=element_text(vjust=2, size=11))+
theme(axis.text.y=element_text(size=11))


pdf(file = "Figure.A.16.pdf")
grid.draw(p)
dev.off()

################################################# Figure A.17 ##################################################################################



rm(list=ls())
load("dataset_2012.RData")

df<-subset(dataset, select=c("country.name", "ActMeetMun",
"country.name", "ActMeetMun", "ActContMun",  "ActContAut" ,                           
"ActMeetImp" , "ActSolveProb" ,  "ActMeetPty" ,  "ActContGvt",                            
"ActProtest" , "ActBlock" ,  "ActVoted",  "ActSign" ,                              
"ActShare" , "education" ,  "female", "age", "income"  ,
"victim", "corrupt", "spatial.distance" ,
"compulsory.voting", "gdp.pc", "rule.of.lw", "enp", "social.spending"))                       
df<-na.exclude(df)


Y <- cbind(df$ActMeetMun, df$ActContMun, df$ActContAut,
		 df$ActContGvt, df$ActSolveProb, 
		df$ActMeetImp, df$ActMeetPty, df$ActVoted, 
		df$ActShare, df$ActSign, df$ActProtest, df$ActBlock)
colnames(Y)<-c("ActMeetMun", "ActContMun", "ActContAut", "ActContGvt",
		"ActSolveProb", "ActMeetImp", "ActMeetPty", "ActVoted",
		"ActShare", "ActSign", "ActProtest", "ActBlock")

Country<-as.numeric(factor(df$country.name), levels=unique(df$country.name))
		

Matrix.Country<-matrix(0,nrow(Y),length(unique(Country)))           
for (i in 1:length(unique(Country))) Matrix.Country[Country==i,i]<-1

df$age.centered<-(df$age-mean(df$age))/(2*sd(df$age))
df$education.centered<-(df$education-mean(df$education))/(2*sd(df$education))


df$income.2<-ifelse(df$income>=quantile(df$income)[2] & df$income<quantile(df$income)[3],1,0)
df$income.3<-ifelse(df$income>=quantile(df$income)[3] & df$income<quantile(df$income)[4],1,0)
df$income.4<-ifelse(df$income>=quantile(df$income)[4],1,0)



df$spatial.distance.centered<-(df$spatial.distance-mean(df$spatial.distance))/(2*sd(df$spatial.distance))

X<-cbind(1, df$age.centered, df$female, df$education.centered, 
	df$income.2, df$income.3, df$income.4, 
	df$victim, df$corrupt, 
	df$spatial.distance.centered,
	(df$rule.of.lw-mean(df$rule.of.lw))/(2*sd(df$rule.of.lw)),
		df$compulsory.voting,
		(df$enp-mean(df$enp))/(2*sd(df$enp)),
		(df$gdp.pc-mean(df$gdp.pc))/(2*sd(df$gdp.pc)),
		(df$social.spending-mean(df$social.spending))/(2*sd(df$social.spending))
)



load("Results_IncomequartilesModel")

Alpha<-mcmc.list(as.mcmc(t(Results$alpha[,,1])), 
		as.mcmc(t(Results$alpha[,,2])),
		as.mcmc(t(Results$alpha[,,3])))
print(paste("Convergence Alpha:", gelman.diag(Alpha)[[2]]<1.2, sep=" "))


Alpha.Conv<-mcmc.list(as.mcmc(t(Results$alpha_conv[-seq(12,dim(Results$alpha_conv)[1],by=12),,1])), 
		as.mcmc(t(Results$alpha_conv[-seq(12,dim(Results$alpha_conv)[1],by=12),,2])),
		as.mcmc(t(Results$alpha_conv[-seq(12,dim(Results$alpha_conv)[1],by=12),,3])))
print(paste("Convergence Alpha Conv:", gelman.diag(Alpha.Conv)[[2]]<1.2, sep=" "))


Alpha.Unconv<-mcmc.list(as.mcmc(t(Results$alpha_unconv[-seq(1,dim(Results$alpha_unconv)[1],by=12),,1])), 
		as.mcmc(t(Results$alpha_unconv[-seq(1,dim(Results$alpha_unconv)[1],by=12),,2])),
		as.mcmc(t(Results$alpha_unconv[-seq(1,dim(Results$alpha_unconv)[1],by=12),,3])))
print(paste("Convergence Alpha Unconventional:", gelman.diag(Alpha.Unconv)[[2]]<1.2, sep=" "))



P.Conv<-mcmc.list(as.mcmc((Results$P_conv[2,,1])), 
		as.mcmc((Results$P_conv[2,,2])),
		as.mcmc((Results$P_conv[2,,3])))
print(paste("Convergence Prob. Conventional:", gelman.diag(P.Conv)[[1]][,1]<1.2, sep=" "))


P.Unconv<-mcmc.list(as.mcmc((Results$P_unconv[2,,1])), 
		as.mcmc((Results$P_unconv[2,,2])),
		as.mcmc((Results$P_unconv[2,,3])))
print(paste("Convergence Prob. Unconventional", gelman.diag(P.Unconv)[[1]][,1]<1.2, sep=" "))



BETA2<-rbind((t(Results$beta2[,,1])),
	(t(Results$beta2[,,2])),
	(t(Results$beta2[,,3])))
BETA3<-rbind((t(Results$beta3[,,1])),
	(t(Results$beta3[,,2])),
	(t(Results$beta3[,,3])))

BETA4<-rbind((t(Results$beta4[,,1])),
	(t(Results$beta4[,,2])),
	(t(Results$beta4[,,3])))

NU<-rbind((t(Results$nu[,,1])),
	(t(Results$nu[,,2])),
	(t(Results$nu[,,3])))
NU2<-NU[, 1:length(unique(Country))]
NU3<-NU[, (1+length(unique(Country))): (2*length(unique(Country)))]
NU4<-NU[, (1+2*length(unique(Country))): (3*length(unique(Country)))]



Beta.Agitator<-BETA2[seq(1, nrow(BETA2), length.out=1000),]
Beta.Conventional<-BETA3[seq(1, nrow(BETA3), length.out=1000),]
Beta.Activist<-BETA4[seq(1, nrow(BETA4), length.out=1000),]
Nu.Agitator<-NU2[seq(1, nrow(NU2), length.out=1000),]
Nu.Conventional<-NU3[seq(1, nrow(NU3), length.out=1000),]
Nu.Activist<-NU4[seq(1, nrow(NU4), length.out=1000),]

# Marginal covariate effects
colnames(X)<-c("Intercept", "Age", "Female", "Education", 
		 "Income.2", "Income.3", "Income.4", "Victimization", "Corruption",
		  "Ideology", "Rule of Law", "Compulsory Voting", "ENPP",
		"GDP", "Social.Spending")


## Individual Effects
dummies<-c(3, 5,6,7,8)
continuous<-c(2,4,10)
ordinal<-9



Marginal.Individual<-matrix(0, nrow=4*(length(dummies)+length(continuous)+length(ordinal)), 3)
colnames(Marginal.Individual)<-c("Mean", "95.Low", "95.High")
rownames(Marginal.Individual)<-paste(rep(colnames(X)[sort(c(dummies, continuous, ordinal))], each=4),
rep(c("Outsider", "Agitator", "Conventional", "Activist"), (length(dummies)+length(continuous)+length(ordinal))), 
sep="-")

for (j in 1:length(dummies)) {
X.0<-X
X.1<-X

X.0[,dummies[j]]<-0
X.1[,dummies[j]]<-1


prob.Agitator.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Outsider.0<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Agitator.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))


prob.Outsider.1<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))


Marginal.Individual[grep(colnames(X)[dummies[j]], rownames(Marginal.Individual))[1],]<-c(mean(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0)),
											quantile(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0), c(0.025,0.975)))

Marginal.Individual[grep(colnames(X)[dummies[j]], rownames(Marginal.Individual))[2],]<-c(mean(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0)), quantile(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0), c(0.025,0.975)))


Marginal.Individual[grep(colnames(X)[dummies[j]], rownames(Marginal.Individual))[3],]<-c(mean(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0)), quantile(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0), c(0.025,0.975)))


Marginal.Individual[grep(colnames(X)[dummies[j]], rownames(Marginal.Individual))[4],]<-c(mean(colMeans(prob.Activist.1)-colMeans(prob.Activist.0)), quantile(colMeans(prob.Activist.1)-colMeans(prob.Activist.0), c(0.025,0.975)))

}


for (j in 1:length(continuous)){
X.0<-X
X.1<-X
X.1[,continuous[j]]<-X.0[,continuous[j]]+sd(X.0[,continuous[j]])


prob.Agitator.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Outsider.0<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Agitator.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))


prob.Outsider.1<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))



Marginal.Individual[grep(colnames(X)[continuous[j]], rownames(Marginal.Individual))[1],]<-c(mean(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0)),
											quantile(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0), c(0.025,0.975)))

Marginal.Individual[grep(colnames(X)[continuous[j]], rownames(Marginal.Individual))[2],]<-c(mean(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0)), quantile(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0), c(0.025,0.975)))


Marginal.Individual[grep(colnames(X)[continuous[j]], rownames(Marginal.Individual))[3],]<-c(mean(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0)), quantile(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0), c(0.025,0.975)))


Marginal.Individual[grep(colnames(X)[continuous[j]], rownames(Marginal.Individual))[4],]<-c(mean(colMeans(prob.Activist.1)-colMeans(prob.Activist.0)), quantile(colMeans(prob.Activist.1)-colMeans(prob.Activist.0), c(0.025,0.975)))

}

for (j in 1:length(ordinal)){
X.0<-X
X.1<-X
X.1[,ordinal[j]]<-X.0[,ordinal[j]]+1


prob.Agitator.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Outsider.0<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Agitator.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))


prob.Outsider.1<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))



Marginal.Individual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Individual))[1],]<-c(mean(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0)),
											quantile(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0), c(0.025,0.975)))

Marginal.Individual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Individual))[2],]<-c(mean(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0)), quantile(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0), c(0.025,0.975)))


Marginal.Individual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Individual))[3],]<-c(mean(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0)), quantile(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0), c(0.025,0.975)))


Marginal.Individual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Individual))[4],]<-c(mean(colMeans(prob.Activist.1)-colMeans(prob.Activist.0)), quantile(colMeans(prob.Activist.1)-colMeans(prob.Activist.0), c(0.025,0.975)))

}


Marginal.Individual[,1:3]<-100*Marginal.Individual[,1:3]
Marginal.Individual[grep("Ideology", rownames(Marginal.Individual)),1:3]<-(Marginal.Individual[grep("Ideology", rownames(Marginal.Individual)),1:3]/sd(X[,grep("Ideology", colnames(X))]))


continuous<-c(11, 13:15)
ordinal<-12


Marginal.Contextual<-matrix(0, nrow=4*(length(continuous)+length(ordinal)), 3)
colnames(Marginal.Contextual)<-c("Mean", "95.Low", "95.High")
rownames(Marginal.Contextual)<-paste(rep(colnames(X)[sort(c(continuous, ordinal))], each=4),
rep(c("Outsider", "Agitator", "Conventional", "Activist"), (length(continuous)+length(ordinal))), 
sep="-")



for (j in 1:length(continuous)){
X.0<-X
X.1<-X
X.1[,continuous[j]]<-X.0[,continuous[j]]+sd(X.0[,continuous[j]])


prob.Agitator.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Outsider.0<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Agitator.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))


prob.Outsider.1<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))



Marginal.Contextual[grep(colnames(X)[continuous[j]], rownames(Marginal.Contextual))[1],]<-c(mean(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0)),
											quantile(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0), c(0.025,0.975)))

Marginal.Contextual[grep(colnames(X)[continuous[j]], rownames(Marginal.Contextual))[2],]<-c(mean(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0)), quantile(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0), c(0.025,0.975)))


Marginal.Contextual[grep(colnames(X)[continuous[j]], rownames(Marginal.Contextual))[3],]<-c(mean(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0)), quantile(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0), c(0.025,0.975)))


Marginal.Contextual[grep(colnames(X)[continuous[j]], rownames(Marginal.Contextual))[4],]<-c(mean(colMeans(prob.Activist.1)-colMeans(prob.Activist.0)), quantile(colMeans(prob.Activist.1)-colMeans(prob.Activist.0), c(0.025,0.975)))

}

for (j in 1:length(ordinal)){
X.0<-X
X.1<-X
X.1[,ordinal[j]]<-X.0[,ordinal[j]]+1


prob.Agitator.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Outsider.0<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Agitator.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))


prob.Outsider.1<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))



Marginal.Contextual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Contextual))[1],]<-c(mean(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0)),
											quantile(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0), c(0.025,0.975)))

Marginal.Contextual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Contextual))[2],]<-c(mean(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0)), quantile(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0), c(0.025,0.975)))


Marginal.Contextual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Contextual))[3],]<-c(mean(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0)), quantile(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0), c(0.025,0.975)))


Marginal.Contextual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Contextual))[4],]<-c(mean(colMeans(prob.Activist.1)-colMeans(prob.Activist.0)), quantile(colMeans(prob.Activist.1)-colMeans(prob.Activist.0), c(0.025,0.975)))

}

Marginal.Contextual[,1:3]<-100*Marginal.Contextual[,1:3]



Figure.Individual<-cbind(unlist(strsplit(rownames(Marginal.Individual), "-"))[seq(2,length(unlist(strsplit(rownames(Marginal.Individual), "-"))), by=2)],
unlist(strsplit(rownames(Marginal.Individual), "-"))[seq(1,length(unlist(strsplit(rownames(Marginal.Individual), "-"))), by=2)],
Marginal.Individual[,1:3])
Figure.Individual<-data.frame(Figure.Individual)
names(Figure.Individual)<-c("Type", "Variable", "Mean", "Low", "High")


Figure.Contextual<-cbind(unlist(strsplit(rownames(Marginal.Contextual), "-"))[seq(2,length(unlist(strsplit(rownames(Marginal.Contextual), "-"))), by=2)],
unlist(strsplit(rownames(Marginal.Contextual), "-"))[seq(1,length(unlist(strsplit(rownames(Marginal.Contextual), "-"))), by=2)],
Marginal.Contextual[,1:3])
Figure.Contextual<-data.frame(Figure.Contextual)
names(Figure.Contextual)<-c("Type", "Variable", "Mean", "Low", "High")


Figure.A.17<-rbind(Figure.Individual, Figure.Contextual)

Figure.A.17$Type<- factor(Figure.A.17$Type, 
	levels = c("Outsider",
		"Agitator",
		"Conventional", "Activist"))



Figure.A.17$Variable<-factor(Figure.A.17$Variable, 
	levels=rev(c("Age", "Female", "Education", 
			"Income.2", "Income.3", "Income.4", "Corruption", 
			"Victimization",
			"Ideology",
			"Rule of Law", "Compulsory Voting", "ENPP", 
			"GDP", "Social.Spending")),
       			labels=rev(c("Age",
			  "Female",
			  "Education", 
			  "Second Income Quartile",			  
			"Third Income Quartile",
			  "Fourth Income Quartile",
			  "Perceived Corruption", 
			   "Crime Victimization",
			 "Ideological Distance to Incumbent",
			"Rule of Law",
			  "Compulsory Voting",
			  "ENPP", 
			  "GDP per capita",
			  "Social Spending")))




for (k in 3:5){
Figure.A.17[,k]<-as.numeric(levels(Figure.A.17[,k]))[Figure.A.17[,k]]
}

p<-ggplot(Figure.A.17,
 aes(y=Variable, x=Mean)) + 
    geom_errorbarh(aes(xmin=Low, 
	xmax=High, height=.2), colour="black") + facet_wrap(~Type)+
    geom_point(size=1.5)+
theme_bw()+xlab("Marginal effects (in percentage points)")+ylab("")+
geom_hline(yintercept=0, linetype="longdash", colour="gray40")+
theme(strip.text.x = element_text(size = 12, face="bold"))+
geom_vline(xintercept=0, linetype="longdash", colour="gray40")+
theme(strip.background = element_rect(fill = alpha('white'))) +
theme(axis.title.x=element_text(vjust=2, size=11))+
theme(axis.text.y=element_text(size=11))


pdf(file = "Figure.A.17.pdf")
grid.draw(p)
dev.off()


################################################# Figure A.18 ##################################################################################


rm(list=ls())
load("dataset_2012.RData")


df<-subset(dataset, select=c("country.name", "ActMeetMun",
"country.name", "ActMeetMun", "ActContMun",  "ActContAut" ,                           
"ActMeetImp" , "ActSolveProb" ,  "ActMeetPty" ,  "ActContGvt",                            
"ActProtest" , "ActBlock" ,  "ActVoted",  "ActSign" ,                              
"ActShare" , "education" ,  "female", "age", "income"  ,
"victim", "corrupt", "spatial.distance" ,
"compulsory.voting", "gdp.pc", "rule.of.lw", "enp", "social.spending"))                       
df<-na.exclude(df)


Y <- cbind(df$ActMeetMun, df$ActContMun, df$ActContAut,
		 df$ActContGvt, df$ActSolveProb, 
		df$ActMeetImp, df$ActMeetPty, df$ActVoted, 
		df$ActShare, df$ActSign, df$ActProtest, df$ActBlock)
colnames(Y)<-c("ActMeetMun", "ActContMun", "ActContAut", "ActContGvt",
		"ActSolveProb", "ActMeetImp", "ActMeetPty", "ActVoted",
		"ActShare", "ActSign", "ActProtest", "ActBlock")

Country<-as.numeric(factor(df$country.name), levels=unique(df$country.name))
		

Matrix.Country<-matrix(0,nrow(Y),length(unique(Country)))           
for (i in 1:length(unique(Country))) Matrix.Country[Country==i,i]<-1

df$age.centered<-(df$age-mean(df$age))/(2*sd(df$age))
df$education.centered<-(df$education-mean(df$education))/(2*sd(df$education))


df$income.centered<-(df$income-mean(df$income))/(2*sd(df$income))


df$spatial.distance.centered<-(df$spatial.distance-mean(df$spatial.distance))/(2*sd(df$spatial.distance))

X<-cbind(1, df$age.centered, df$female, df$education.centered, 
	df$income.centered, 
	df$victim, df$corrupt, 
	df$spatial.distance.centered,
	(df$rule.of.lw-mean(df$rule.of.lw))/(2*sd(df$rule.of.lw)),
		df$compulsory.voting,
		(df$enp-mean(df$enp))/(2*sd(df$enp)),
		(df$gdp.pc-mean(df$gdp.pc))/(2*sd(df$gdp.pc)),
		(df$social.spending-mean(df$social.spending))/(2*sd(df$social.spending))
)



load("Results_LAPOPIncomeModel")

Alpha<-mcmc.list(as.mcmc(t(Results$alpha[,,1])), 
		as.mcmc(t(Results$alpha[,,2])),
		as.mcmc(t(Results$alpha[,,3])))
print(paste("Convergence Alpha:", gelman.diag(Alpha)[[2]]<1.2, sep=" "))


Alpha.Conv<-mcmc.list(as.mcmc(t(Results$alpha_conv[-seq(12,dim(Results$alpha_conv)[1],by=12),,1])), 
		as.mcmc(t(Results$alpha_conv[-seq(12,dim(Results$alpha_conv)[1],by=12),,2])),
		as.mcmc(t(Results$alpha_conv[-seq(12,dim(Results$alpha_conv)[1],by=12),,3])))
print(paste("Convergence Alpha Conv:", gelman.diag(Alpha.Conv)[[2]]<1.2, sep=" "))


Alpha.Unconv<-mcmc.list(as.mcmc(t(Results$alpha_unconv[-seq(1,dim(Results$alpha_unconv)[1],by=12),,1])), 
		as.mcmc(t(Results$alpha_unconv[-seq(1,dim(Results$alpha_unconv)[1],by=12),,2])),
		as.mcmc(t(Results$alpha_unconv[-seq(1,dim(Results$alpha_unconv)[1],by=12),,3])))
print(paste("Convergence Alpha Unconventional:", gelman.diag(Alpha.Unconv)[[2]]<1.2, sep=" "))



P.Conv<-mcmc.list(as.mcmc((Results$P_conv[2,,1])), 
		as.mcmc((Results$P_conv[2,,2])),
		as.mcmc((Results$P_conv[2,,3])))
print(paste("Convergence Prob. Conventional:", gelman.diag(P.Conv)[[1]][,1]<1.2, sep=" "))


P.Unconv<-mcmc.list(as.mcmc((Results$P_unconv[2,,1])), 
		as.mcmc((Results$P_unconv[2,,2])),
		as.mcmc((Results$P_unconv[2,,3])))
print(paste("Convergence Prob. Unconventional", gelman.diag(P.Unconv)[[1]][,1]<1.2, sep=" "))



BETA2<-rbind((t(Results$beta2[,,1])),
	(t(Results$beta2[,,2])),
	(t(Results$beta2[,,3])))
BETA3<-rbind((t(Results$beta3[,,1])),
	(t(Results$beta3[,,2])),
	(t(Results$beta3[,,3])))

BETA4<-rbind((t(Results$beta4[,,1])),
	(t(Results$beta4[,,2])),
	(t(Results$beta4[,,3])))

NU<-rbind((t(Results$nu[,,1])),
	(t(Results$nu[,,2])),
	(t(Results$nu[,,3])))
NU2<-NU[, 1:length(unique(Country))]
NU3<-NU[, (1+length(unique(Country))): (2*length(unique(Country)))]
NU4<-NU[, (1+2*length(unique(Country))): (3*length(unique(Country)))]



Beta.Agitator<-BETA2[seq(1, nrow(BETA2), length.out=1000),]
Beta.Conventional<-BETA3[seq(1, nrow(BETA3), length.out=1000),]
Beta.Activist<-BETA4[seq(1, nrow(BETA4), length.out=1000),]
Nu.Agitator<-NU2[seq(1, nrow(NU2), length.out=1000),]
Nu.Conventional<-NU3[seq(1, nrow(NU3), length.out=1000),]
Nu.Activist<-NU4[seq(1, nrow(NU4), length.out=1000),]

# Marginal covariate effects
colnames(X)<-c("Intercept", "Age", "Female", "Education", 
		 "Income", "Victimization", "Corruption",
		  "Ideology", "Rule of Law", "Compulsory Voting", "ENPP",
		"GDP", "Social.Spending")


## Individual Effects
dummies<-c(3, 6)
continuous<-c(2,4,5,8)
ordinal<-7



Marginal.Individual<-matrix(0, nrow=4*(length(dummies)+length(continuous)+length(ordinal)), 3)
colnames(Marginal.Individual)<-c("Mean", "95.Low", "95.High")
rownames(Marginal.Individual)<-paste(rep(colnames(X)[sort(c(dummies, continuous, ordinal))], each=4),
rep(c("Outsider", "Agitator", "Conventional", "Activist"), (length(dummies)+length(continuous)+length(ordinal))), 
sep="-")

for (j in 1:length(dummies)) {
X.0<-X
X.1<-X

X.0[,dummies[j]]<-0
X.1[,dummies[j]]<-1


prob.Agitator.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Outsider.0<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Agitator.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))


prob.Outsider.1<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))


Marginal.Individual[grep(colnames(X)[dummies[j]], rownames(Marginal.Individual))[1],]<-c(mean(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0)),
											quantile(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0), c(0.025,0.975)))

Marginal.Individual[grep(colnames(X)[dummies[j]], rownames(Marginal.Individual))[2],]<-c(mean(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0)), quantile(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0), c(0.025,0.975)))


Marginal.Individual[grep(colnames(X)[dummies[j]], rownames(Marginal.Individual))[3],]<-c(mean(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0)), quantile(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0), c(0.025,0.975)))


Marginal.Individual[grep(colnames(X)[dummies[j]], rownames(Marginal.Individual))[4],]<-c(mean(colMeans(prob.Activist.1)-colMeans(prob.Activist.0)), quantile(colMeans(prob.Activist.1)-colMeans(prob.Activist.0), c(0.025,0.975)))

}


for (j in 1:length(continuous)){
X.0<-X
X.1<-X
X.1[,continuous[j]]<-X.0[,continuous[j]]+sd(X.0[,continuous[j]])


prob.Agitator.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Outsider.0<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Agitator.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))


prob.Outsider.1<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))



Marginal.Individual[grep(colnames(X)[continuous[j]], rownames(Marginal.Individual))[1],]<-c(mean(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0)),
											quantile(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0), c(0.025,0.975)))

Marginal.Individual[grep(colnames(X)[continuous[j]], rownames(Marginal.Individual))[2],]<-c(mean(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0)), quantile(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0), c(0.025,0.975)))


Marginal.Individual[grep(colnames(X)[continuous[j]], rownames(Marginal.Individual))[3],]<-c(mean(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0)), quantile(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0), c(0.025,0.975)))


Marginal.Individual[grep(colnames(X)[continuous[j]], rownames(Marginal.Individual))[4],]<-c(mean(colMeans(prob.Activist.1)-colMeans(prob.Activist.0)), quantile(colMeans(prob.Activist.1)-colMeans(prob.Activist.0), c(0.025,0.975)))

}

for (j in 1:length(ordinal)){
X.0<-X
X.1<-X
X.1[,ordinal[j]]<-X.0[,ordinal[j]]+1


prob.Agitator.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Outsider.0<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Agitator.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))


prob.Outsider.1<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))



Marginal.Individual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Individual))[1],]<-c(mean(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0)),
											quantile(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0), c(0.025,0.975)))

Marginal.Individual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Individual))[2],]<-c(mean(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0)), quantile(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0), c(0.025,0.975)))


Marginal.Individual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Individual))[3],]<-c(mean(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0)), quantile(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0), c(0.025,0.975)))


Marginal.Individual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Individual))[4],]<-c(mean(colMeans(prob.Activist.1)-colMeans(prob.Activist.0)), quantile(colMeans(prob.Activist.1)-colMeans(prob.Activist.0), c(0.025,0.975)))

}


Marginal.Individual[,1:3]<-100*Marginal.Individual[,1:3]
Marginal.Individual[grep("Ideology", rownames(Marginal.Individual)),1:3]<-(Marginal.Individual[grep("Ideology", rownames(Marginal.Individual)),1:3]/sd(X[,grep("Ideology", colnames(X))]))


continuous<-c(9, 11:13)
ordinal<-10


Marginal.Contextual<-matrix(0, nrow=4*(length(continuous)+length(ordinal)), 3)
colnames(Marginal.Contextual)<-c("Mean", "95.Low", "95.High")
rownames(Marginal.Contextual)<-paste(rep(colnames(X)[sort(c(continuous, ordinal))], each=4),
rep(c("Outsider", "Agitator", "Conventional", "Activist"), (length(continuous)+length(ordinal))), 
sep="-")



for (j in 1:length(continuous)){
X.0<-X
X.1<-X
X.1[,continuous[j]]<-X.0[,continuous[j]]+sd(X.0[,continuous[j]])


prob.Agitator.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Outsider.0<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Agitator.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))


prob.Outsider.1<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))



Marginal.Contextual[grep(colnames(X)[continuous[j]], rownames(Marginal.Contextual))[1],]<-c(mean(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0)),
											quantile(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0), c(0.025,0.975)))

Marginal.Contextual[grep(colnames(X)[continuous[j]], rownames(Marginal.Contextual))[2],]<-c(mean(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0)), quantile(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0), c(0.025,0.975)))


Marginal.Contextual[grep(colnames(X)[continuous[j]], rownames(Marginal.Contextual))[3],]<-c(mean(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0)), quantile(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0), c(0.025,0.975)))


Marginal.Contextual[grep(colnames(X)[continuous[j]], rownames(Marginal.Contextual))[4],]<-c(mean(colMeans(prob.Activist.1)-colMeans(prob.Activist.0)), quantile(colMeans(prob.Activist.1)-colMeans(prob.Activist.0), c(0.025,0.975)))

}

for (j in 1:length(ordinal)){
X.0<-X
X.1<-X
X.1[,ordinal[j]]<-X.0[,ordinal[j]]+1


prob.Agitator.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Outsider.0<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Agitator.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))


prob.Outsider.1<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))



Marginal.Contextual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Contextual))[1],]<-c(mean(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0)),
											quantile(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0), c(0.025,0.975)))

Marginal.Contextual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Contextual))[2],]<-c(mean(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0)), quantile(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0), c(0.025,0.975)))


Marginal.Contextual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Contextual))[3],]<-c(mean(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0)), quantile(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0), c(0.025,0.975)))


Marginal.Contextual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Contextual))[4],]<-c(mean(colMeans(prob.Activist.1)-colMeans(prob.Activist.0)), quantile(colMeans(prob.Activist.1)-colMeans(prob.Activist.0), c(0.025,0.975)))

}

Marginal.Contextual[,1:3]<-100*Marginal.Contextual[,1:3]





Figure.Individual<-cbind(unlist(strsplit(rownames(Marginal.Individual), "-"))[seq(2,length(unlist(strsplit(rownames(Marginal.Individual), "-"))), by=2)],
unlist(strsplit(rownames(Marginal.Individual), "-"))[seq(1,length(unlist(strsplit(rownames(Marginal.Individual), "-"))), by=2)],
Marginal.Individual[,1:3])
Figure.Individual<-data.frame(Figure.Individual)
names(Figure.Individual)<-c("Type", "Variable", "Mean", "Low", "High")


Figure.Contextual<-cbind(unlist(strsplit(rownames(Marginal.Contextual), "-"))[seq(2,length(unlist(strsplit(rownames(Marginal.Contextual), "-"))), by=2)],
unlist(strsplit(rownames(Marginal.Contextual), "-"))[seq(1,length(unlist(strsplit(rownames(Marginal.Contextual), "-"))), by=2)],
Marginal.Contextual[,1:3])
Figure.Contextual<-data.frame(Figure.Contextual)
names(Figure.Contextual)<-c("Type", "Variable", "Mean", "Low", "High")


Figure.A.18<-rbind(Figure.Individual, Figure.Contextual)

Figure.A.18$Type<- factor(Figure.A.18$Type, 
	levels = c("Outsider",
		"Agitator",
		"Conventional", "Activist"))



Figure.A.18$Variable<-factor(Figure.A.18$Variable, 
	levels=rev(c("Age", "Female", "Education", 
			"Income", "Corruption", 
			"Victimization",
			"Ideology",
			"Rule of Law", "Compulsory Voting", "ENPP", 
			"GDP", "Social.Spending")),
      			 labels=rev(c("Age",
			  "Female",
			  "Education", 
			  "Continuous LAPOP Income Variable",
			  "Perceived Corruption", 
			   "Crime Victimization",
			 "Ideological Distance to Incumbent",
			"Rule of Law",
			  "Compulsory Voting",
			  "ENPP", 
			  "GDP per capita",
			  "Social Spending")))






for (k in 3:5){
Figure.A.18[,k]<-as.numeric(levels(Figure.A.18[,k]))[Figure.A.18[,k]]
}

p<-ggplot(Figure.A.18,
 aes(y=Variable, x=Mean)) + 
    geom_errorbarh(aes(xmin=Low, 
	xmax=High, height=.2), colour="black") + facet_wrap(~Type)+
    geom_point(size=1.5)+
theme_bw()+xlab("Marginal effects (in percentage points)")+ylab("")+
geom_hline(yintercept=0, linetype="longdash", colour="gray40")+
theme(strip.text.x = element_text(size = 12, face="bold"))+
geom_vline(xintercept=0, linetype="longdash", colour="gray40")+
theme(strip.background = element_rect(fill = alpha('white'))) +
theme(axis.title.x=element_text(vjust=2, size=11))+
theme(axis.text.y=element_text(size=11))


pdf(file = "Figure.A.18.pdf")
grid.draw(p)
dev.off()

################################################# Figure A.19 ##################################################################################

rm(list=ls())
load("dataset_2012.RData")

df<-subset(dataset, select=c("country.name", "ActMeetMun",
"country.name", "ActMeetMun", "ActContMun",  "ActContAut" ,                           
"ActMeetImp" , "ActSolveProb" ,  "ActMeetPty" ,  "ActContGvt",                            
"ActProtest" , "ActBlock" ,  "ActVoted",  "ActSign" ,                              
"ActShare" , "education" ,  "female", "age", "intrapersonal.deprivation.longer.worse" ,
"victim", "corrupt", "spatial.distance" ,
"compulsory.voting", "gdp.pc", "rule.of.lw", "enp", "social.spending"))                       
df<-na.exclude(df)


Y <- cbind(df$ActMeetMun, df$ActContMun, df$ActContAut,
		 df$ActContGvt, df$ActSolveProb, 
		df$ActMeetImp, df$ActMeetPty, df$ActVoted, 
		df$ActShare, df$ActSign, df$ActProtest, df$ActBlock)
colnames(Y)<-c("ActMeetMun", "ActContMun", "ActContAut", "ActContGvt",
		"ActSolveProb", "ActMeetImp", "ActMeetPty", "ActVoted",
		"ActShare", "ActSign", "ActProtest", "ActBlock")

Country<-as.numeric(factor(df$country.name), levels=unique(df$country.name))
		

Matrix.Country<-matrix(0,nrow(Y),length(unique(Country)))           
for (i in 1:length(unique(Country))) Matrix.Country[Country==i,i]<-1

df$age.centered<-(df$age-mean(df$age))/(2*sd(df$age))
df$education.centered<-(df$education-mean(df$education))/(2*sd(df$education))

df$deprivation.centered<-df$intrapersonal.deprivation.longer.worse

df$spatial.distance.centered<-(df$spatial.distance-mean(df$spatial.distance))/(2*sd(df$spatial.distance))

X<-cbind(1, df$age.centered, df$female, df$education.centered, 
	df$deprivation.centered, 
	df$victim, df$corrupt, 
	df$spatial.distance.centered,
	(df$rule.of.lw-mean(df$rule.of.lw))/(2*sd(df$rule.of.lw)),
		df$compulsory.voting,
		(df$enp-mean(df$enp))/(2*sd(df$enp)),
		(df$gdp.pc-mean(df$gdp.pc))/(2*sd(df$gdp.pc)),
		(df$social.spending-mean(df$social.spending))/(2*sd(df$social.spending))
)




load("Results_DeprivationModel")

Alpha<-mcmc.list(as.mcmc(t(Results$alpha[,,1])), 
		as.mcmc(t(Results$alpha[,,2])),
		as.mcmc(t(Results$alpha[,,3])))
print(paste("Convergence Alpha:", gelman.diag(Alpha)[[2]]<1.2, sep=" "))


Alpha.Conv<-mcmc.list(as.mcmc(t(Results$alpha_conv[-seq(12,dim(Results$alpha_conv)[1],by=12),,1])), 
		as.mcmc(t(Results$alpha_conv[-seq(12,dim(Results$alpha_conv)[1],by=12),,2])),
		as.mcmc(t(Results$alpha_conv[-seq(12,dim(Results$alpha_conv)[1],by=12),,3])))
print(paste("Convergence Alpha Conv:", gelman.diag(Alpha.Conv)[[2]]<1.2, sep=" "))


Alpha.Unconv<-mcmc.list(as.mcmc(t(Results$alpha_unconv[-seq(1,dim(Results$alpha_unconv)[1],by=12),,1])), 
		as.mcmc(t(Results$alpha_unconv[-seq(1,dim(Results$alpha_unconv)[1],by=12),,2])),
		as.mcmc(t(Results$alpha_unconv[-seq(1,dim(Results$alpha_unconv)[1],by=12),,3])))
print(paste("Convergence Alpha Unconventional:", gelman.diag(Alpha.Unconv)[[2]]<1.2, sep=" "))



P.Conv<-mcmc.list(as.mcmc((Results$P_conv[2,,1])), 
		as.mcmc((Results$P_conv[2,,2])),
		as.mcmc((Results$P_conv[2,,3])))
print(paste("Convergence Prob. Conventional:", gelman.diag(P.Conv)[[1]][,1]<1.2, sep=" "))


P.Unconv<-mcmc.list(as.mcmc((Results$P_unconv[2,,1])), 
		as.mcmc((Results$P_unconv[2,,2])),
		as.mcmc((Results$P_unconv[2,,3])))
print(paste("Convergence Prob. Unconventional", gelman.diag(P.Unconv)[[1]][,1]<1.2, sep=" "))



BETA2<-rbind((t(Results$beta2[,,1])),
	(t(Results$beta2[,,2])),
	(t(Results$beta2[,,3])))
BETA3<-rbind((t(Results$beta3[,,1])),
	(t(Results$beta3[,,2])),
	(t(Results$beta3[,,3])))

BETA4<-rbind((t(Results$beta4[,,1])),
	(t(Results$beta4[,,2])),
	(t(Results$beta4[,,3])))

NU<-rbind((t(Results$nu[,,1])),
	(t(Results$nu[,,2])),
	(t(Results$nu[,,3])))
NU2<-NU[, 1:length(unique(Country))]
NU3<-NU[, (1+length(unique(Country))): (2*length(unique(Country)))]
NU4<-NU[, (1+2*length(unique(Country))): (3*length(unique(Country)))]



Beta.Agitator<-BETA2[seq(1, nrow(BETA2), length.out=1000),]
Beta.Conventional<-BETA3[seq(1, nrow(BETA3), length.out=1000),]
Beta.Activist<-BETA4[seq(1, nrow(BETA4), length.out=1000),]
Nu.Agitator<-NU2[seq(1, nrow(NU2), length.out=1000),]
Nu.Conventional<-NU3[seq(1, nrow(NU3), length.out=1000),]
Nu.Activist<-NU4[seq(1, nrow(NU4), length.out=1000),]

# Marginal covariate effects
colnames(X)<-c("Intercept", "Age", "Female", "Education", 
		 "Income", "Victimization", "Corruption",
		  "Ideology", "Rule of Law", "Compulsory Voting", "ENPP",
		"GDP", "Social.Spending")


## Individual Effects
dummies<-c(3, 6)
continuous<-c(2,4,5,8)
ordinal<-7




Marginal.Individual<-matrix(0, nrow=4*(length(dummies)+length(continuous)+length(ordinal)), 3)
colnames(Marginal.Individual)<-c("Mean", "95.Low", "95.High")
rownames(Marginal.Individual)<-paste(rep(colnames(X)[sort(c(dummies, continuous, ordinal))], each=4),
rep(c("Outsider", "Agitator", "Conventional", "Activist"), (length(dummies)+length(continuous)+length(ordinal))), 
sep="-")

for (j in 1:length(dummies)) {
X.0<-X
X.1<-X

X.0[,dummies[j]]<-0
X.1[,dummies[j]]<-1


prob.Agitator.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Outsider.0<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Agitator.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))


prob.Outsider.1<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))


Marginal.Individual[grep(colnames(X)[dummies[j]], rownames(Marginal.Individual))[1],]<-c(mean(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0)),
											quantile(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0), c(0.025,0.975)))

Marginal.Individual[grep(colnames(X)[dummies[j]], rownames(Marginal.Individual))[2],]<-c(mean(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0)), quantile(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0), c(0.025,0.975)))


Marginal.Individual[grep(colnames(X)[dummies[j]], rownames(Marginal.Individual))[3],]<-c(mean(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0)), quantile(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0), c(0.025,0.975)))


Marginal.Individual[grep(colnames(X)[dummies[j]], rownames(Marginal.Individual))[4],]<-c(mean(colMeans(prob.Activist.1)-colMeans(prob.Activist.0)), quantile(colMeans(prob.Activist.1)-colMeans(prob.Activist.0), c(0.025,0.975)))

}


for (j in 1:length(continuous)){
X.0<-X
X.1<-X
X.1[,continuous[j]]<-X.0[,continuous[j]]+sd(X.0[,continuous[j]])


prob.Agitator.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Outsider.0<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Agitator.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))


prob.Outsider.1<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))



Marginal.Individual[grep(colnames(X)[continuous[j]], rownames(Marginal.Individual))[1],]<-c(mean(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0)),
											quantile(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0), c(0.025,0.975)))

Marginal.Individual[grep(colnames(X)[continuous[j]], rownames(Marginal.Individual))[2],]<-c(mean(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0)), quantile(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0), c(0.025,0.975)))


Marginal.Individual[grep(colnames(X)[continuous[j]], rownames(Marginal.Individual))[3],]<-c(mean(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0)), quantile(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0), c(0.025,0.975)))


Marginal.Individual[grep(colnames(X)[continuous[j]], rownames(Marginal.Individual))[4],]<-c(mean(colMeans(prob.Activist.1)-colMeans(prob.Activist.0)), quantile(colMeans(prob.Activist.1)-colMeans(prob.Activist.0), c(0.025,0.975)))

}

for (j in 1:length(ordinal)){
X.0<-X
X.1<-X
X.1[,ordinal[j]]<-X.0[,ordinal[j]]+1


prob.Agitator.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Outsider.0<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Agitator.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))


prob.Outsider.1<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))



Marginal.Individual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Individual))[1],]<-c(mean(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0)),
											quantile(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0), c(0.025,0.975)))

Marginal.Individual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Individual))[2],]<-c(mean(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0)), quantile(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0), c(0.025,0.975)))


Marginal.Individual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Individual))[3],]<-c(mean(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0)), quantile(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0), c(0.025,0.975)))


Marginal.Individual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Individual))[4],]<-c(mean(colMeans(prob.Activist.1)-colMeans(prob.Activist.0)), quantile(colMeans(prob.Activist.1)-colMeans(prob.Activist.0), c(0.025,0.975)))

}


Marginal.Individual[,1:3]<-100*Marginal.Individual[,1:3]
Marginal.Individual[grep("Ideology", rownames(Marginal.Individual)),1:3]<-(Marginal.Individual[grep("Ideology", rownames(Marginal.Individual)),1:3]/sd(X[,grep("Ideology", colnames(X))]))


continuous<-c(9, 11:13)
ordinal<-10


Marginal.Contextual<-matrix(0, nrow=4*(length(continuous)+length(ordinal)), 3)
colnames(Marginal.Contextual)<-c("Mean", "95.Low", "95.High")
rownames(Marginal.Contextual)<-paste(rep(colnames(X)[sort(c(continuous, ordinal))], each=4),
rep(c("Outsider", "Agitator", "Conventional", "Activist"), (length(continuous)+length(ordinal))), 
sep="-")



for (j in 1:length(continuous)){
X.0<-X
X.1<-X
X.1[,continuous[j]]<-X.0[,continuous[j]]+sd(X.0[,continuous[j]])


prob.Agitator.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Outsider.0<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Agitator.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))


prob.Outsider.1<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))



Marginal.Contextual[grep(colnames(X)[continuous[j]], rownames(Marginal.Contextual))[1],]<-c(mean(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0)),
											quantile(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0), c(0.025,0.975)))

Marginal.Contextual[grep(colnames(X)[continuous[j]], rownames(Marginal.Contextual))[2],]<-c(mean(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0)), quantile(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0), c(0.025,0.975)))


Marginal.Contextual[grep(colnames(X)[continuous[j]], rownames(Marginal.Contextual))[3],]<-c(mean(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0)), quantile(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0), c(0.025,0.975)))


Marginal.Contextual[grep(colnames(X)[continuous[j]], rownames(Marginal.Contextual))[4],]<-c(mean(colMeans(prob.Activist.1)-colMeans(prob.Activist.0)), quantile(colMeans(prob.Activist.1)-colMeans(prob.Activist.0), c(0.025,0.975)))

}

for (j in 1:length(ordinal)){
X.0<-X
X.1<-X
X.1[,ordinal[j]]<-X.0[,ordinal[j]]+1


prob.Agitator.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Outsider.0<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Agitator.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))


prob.Outsider.1<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))



Marginal.Contextual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Contextual))[1],]<-c(mean(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0)),
											quantile(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0), c(0.025,0.975)))

Marginal.Contextual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Contextual))[2],]<-c(mean(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0)), quantile(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0), c(0.025,0.975)))


Marginal.Contextual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Contextual))[3],]<-c(mean(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0)), quantile(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0), c(0.025,0.975)))


Marginal.Contextual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Contextual))[4],]<-c(mean(colMeans(prob.Activist.1)-colMeans(prob.Activist.0)), quantile(colMeans(prob.Activist.1)-colMeans(prob.Activist.0), c(0.025,0.975)))

}

Marginal.Contextual[,1:3]<-100*Marginal.Contextual[,1:3]



Figure.Individual<-cbind(unlist(strsplit(rownames(Marginal.Individual), "-"))[seq(2,length(unlist(strsplit(rownames(Marginal.Individual), "-"))), by=2)],
unlist(strsplit(rownames(Marginal.Individual), "-"))[seq(1,length(unlist(strsplit(rownames(Marginal.Individual), "-"))), by=2)],
Marginal.Individual[,1:3])
Figure.Individual<-data.frame(Figure.Individual)
names(Figure.Individual)<-c("Type", "Variable", "Mean", "Low", "High")


Figure.Contextual<-cbind(unlist(strsplit(rownames(Marginal.Contextual), "-"))[seq(2,length(unlist(strsplit(rownames(Marginal.Contextual), "-"))), by=2)],
unlist(strsplit(rownames(Marginal.Contextual), "-"))[seq(1,length(unlist(strsplit(rownames(Marginal.Contextual), "-"))), by=2)],
Marginal.Contextual[,1:3])
Figure.Contextual<-data.frame(Figure.Contextual)
names(Figure.Contextual)<-c("Type", "Variable", "Mean", "Low", "High")


Figure.A.19<-rbind(Figure.Individual, Figure.Contextual)

Figure.A.19$Type<- factor(Figure.A.19$Type, 
	levels = c("Outsider",
		"Agitator",
		"Conventional", "Activist"))



Figure.A.19$Variable<-factor(Figure.A.19$Variable, 
			levels=rev(c("Age", "Female", "Education", 
			"Income", "Corruption", 
			"Victimization",
			"Ideology",
			"Rule of Law", "Compulsory Voting", "ENPP", 
			"GDP", "Social.Spending")),
    			  labels=rev(c("Age",
			  "Female",
			  "Education", 
			  "Perceived Relative Deprivation",
			  "Perceived Corruption", 
			   "Crime Victimization",
			 "Ideological Distance to Incumbent",
			"Rule of Law",
			  "Compulsory Voting",
			  "ENPP", 
			  "GDP per capita",
			  "Social Spending")))





for (k in 3:5){
Figure.A.19[,k]<-as.numeric(levels(Figure.A.19[,k]))[Figure.A.19[,k]]
}

p<-ggplot(Figure.A.19,
 aes(y=Variable, x=Mean)) + 
    geom_errorbarh(aes(xmin=Low, 
	xmax=High, height=.2), colour="black") + facet_wrap(~Type)+
    geom_point(size=1.5)+
theme_bw()+xlab("Marginal effects (in percentage points)")+ylab("")+
geom_hline(yintercept=0, linetype="longdash", colour="gray40")+
theme(strip.text.x = element_text(size = 12, face="bold"))+
geom_vline(xintercept=0, linetype="longdash", colour="gray40")+
theme(strip.background = element_rect(fill = alpha('white'))) +
theme(axis.title.x=element_text(vjust=2, size=11))+
theme(axis.text.y=element_text(size=11))


pdf(file = "Figure.A.19.pdf")
grid.draw(p)
dev.off()


################################################# SECTION A.3.4 ##################################################################################
################################################# Table A.9 ##################################################################################


rm(list=ls())
load("dataset_2012.RData")


df<-subset(dataset, select=c("country.name", "ActMeetMun",
"country.name", "ActMeetMun", "ActContMun",  "ActContAut" ,                           
"ActMeetImp" , "ActSolveProb" ,  "ActMeetPty" ,  "ActContGvt",                            
"ActProtest" , "ActBlock" ,  "ActVoted",  "ActSign" ,                              
"ActShare" , "education" ,  "female", "age", "incomeproxy" ,
"victim", "corrupt", 
"government.support",
"compulsory.voting", "gdp.pc", "rule.of.lw", "enp", 
"education.spending"))                       
df<-na.exclude(df)

Y <- cbind(df$ActMeetMun, df$ActContMun, df$ActContAut,
		 df$ActContGvt, df$ActSolveProb, 
		df$ActMeetImp, df$ActMeetPty, df$ActVoted, 
		df$ActShare, df$ActSign, df$ActProtest, df$ActBlock)
colnames(Y)<-c("ActMeetMun", "ActContMun", "ActContAut", "ActContGvt",
		"ActSolveProb", "ActMeetImp", "ActMeetPty", "ActVoted",
		"ActShare", "ActSign", "ActProtest", "ActBlock")

Country<-as.numeric(factor(df$country.name), levels=unique(df$country.name))
		

Matrix.Country<-matrix(0,nrow(Y),length(unique(Country)))           
for (i in 1:length(unique(Country))) Matrix.Country[Country==i,i]<-1

df$age.centered<-(df$age-mean(df$age))/(2*sd(df$age))
df$education.centered<-(df$education-mean(df$education))/(2*sd(df$education))


df$incomeproxy.centered<-(df$incomeproxy-mean(df$incomeproxy))/(2*sd(df$incomeproxy))



X<-cbind(1, df$age.centered, df$female, df$education.centered, 
	df$incomeproxy.centered, 
	df$victim, df$corrupt, 
	df$government.support,
	(df$rule.of.lw-mean(df$rule.of.lw))/(2*sd(df$rule.of.lw)),
		df$compulsory.voting,
		(df$enp-mean(df$enp))/(2*sd(df$enp)),
		(df$gdp.pc-mean(df$gdp.pc))/(2*sd(df$gdp.pc)),
		(df$education.spending-mean(df$education.spending))/(2*sd(df$education.spending))
)





load("Results_GovsupportModel")

Alpha<-mcmc.list(as.mcmc(t(Results$alpha[,,1])), 
		as.mcmc(t(Results$alpha[,,2])),
		as.mcmc(t(Results$alpha[,,3])))
print(paste("Convergence Alpha:", gelman.diag(Alpha)[[2]]<1.2, sep=" "))


Alpha.Conv<-mcmc.list(as.mcmc(t(Results$alpha_conv[-seq(12,dim(Results$alpha_conv)[1],by=12),,1])), 
		as.mcmc(t(Results$alpha_conv[-seq(12,dim(Results$alpha_conv)[1],by=12),,2])),
		as.mcmc(t(Results$alpha_conv[-seq(12,dim(Results$alpha_conv)[1],by=12),,3])))
print(paste("Convergence Alpha Conv:", gelman.diag(Alpha.Conv)[[2]]<1.2, sep=" "))


Alpha.Unconv<-mcmc.list(as.mcmc(t(Results$alpha_unconv[-seq(1,dim(Results$alpha_unconv)[1],by=12),,1])), 
		as.mcmc(t(Results$alpha_unconv[-seq(1,dim(Results$alpha_unconv)[1],by=12),,2])),
		as.mcmc(t(Results$alpha_unconv[-seq(1,dim(Results$alpha_unconv)[1],by=12),,3])))
print(paste("Convergence Alpha Unconventional:", gelman.diag(Alpha.Unconv)[[2]]<1.2, sep=" "))



P.Conv<-mcmc.list(as.mcmc((Results$P_conv[2,,1])), 
		as.mcmc((Results$P_conv[2,,2])),
		as.mcmc((Results$P_conv[2,,3])))
print(paste("Convergence Prob. Conventional:", gelman.diag(P.Conv)[[1]][,1]<1.2, sep=" "))


P.Unconv<-mcmc.list(as.mcmc((Results$P_unconv[2,,1])), 
		as.mcmc((Results$P_unconv[2,,2])),
		as.mcmc((Results$P_unconv[2,,3])))
print(paste("Convergence Prob. Unconventional", gelman.diag(P.Unconv)[[1]][,1]<1.2, sep=" "))


Alpha<-mcmc.list(as.mcmc(t(Results$alpha[,,1])), 
		as.mcmc(t(Results$alpha[,,2])),
		as.mcmc(t(Results$alpha[,,3])))
print(paste("Convergence Alpha:", gelman.diag(Alpha)[[2]]<1.2, sep=" "))


Alpha.Conv<-mcmc.list(as.mcmc(t(Results$alpha_conv[-seq(12,dim(Results$alpha_conv)[1],by=12),,1])), 
		as.mcmc(t(Results$alpha_conv[-seq(12,dim(Results$alpha_conv)[1],by=12),,2])),
		as.mcmc(t(Results$alpha_conv[-seq(12,dim(Results$alpha_conv)[1],by=12),,3])))
print(paste("Convergence Alpha Conv:", gelman.diag(Alpha.Conv)[[2]]<1.2, sep=" "))


Alpha.Unconv<-mcmc.list(as.mcmc(t(Results$alpha_unconv[-seq(1,dim(Results$alpha_unconv)[1],by=12),,1])), 
		as.mcmc(t(Results$alpha_unconv[-seq(1,dim(Results$alpha_unconv)[1],by=12),,2])),
		as.mcmc(t(Results$alpha_unconv[-seq(1,dim(Results$alpha_unconv)[1],by=12),,3])))
print(paste("Convergence Alpha Unconventional:", gelman.diag(Alpha.Unconv)[[2]]<1.2, sep=" "))



P.Conv<-mcmc.list(as.mcmc((Results$P_conv[2,,1])), 
		as.mcmc((Results$P_conv[2,,2])),
		as.mcmc((Results$P_conv[2,,3])))
print(paste("Convergence Prob. Conventional:", gelman.diag(P.Conv)[[1]][,1]<1.2, sep=" "))


P.Unconv<-mcmc.list(as.mcmc((Results$P_unconv[2,,1])), 
		as.mcmc((Results$P_unconv[2,,2])),
		as.mcmc((Results$P_unconv[2,,3])))
print(paste("Convergence Prob. Unconventional", gelman.diag(P.Unconv)[[1]][,1]<1.2, sep=" "))


Beta2<-mcmc.list(as.mcmc(t(Results$beta2[,,1])),
	as.mcmc(t(Results$beta2[,,2])),
	as.mcmc(t(Results$beta2[,,3])))
Beta3<-mcmc.list(as.mcmc(t(Results$beta3[,,1])),
	as.mcmc(t(Results$beta3[,,2])),
	as.mcmc(t(Results$beta3[,,3])))
Beta4<-mcmc.list(as.mcmc(t(Results$beta4[,,1])),
	as.mcmc(t(Results$beta4[,,2])),
	as.mcmc(t(Results$beta4[,,3])))
print(paste("Convergence Regression coefficients:", gelman.diag(Beta2)[[2]]<1.2 & gelman.diag(Beta3)[[2]]<1.2 & gelman.diag(Beta4)[[2]]<1.2, sep=" "))



# First we process the data needed to produce Table 2 and the other results reported in the research note and Section A.3.1 of the Online Appendix
ALPHA<-rbind(t(Results$alpha[,,1]), 
		t(Results$alpha[,,2]),
		t(Results$alpha[,,3]))


Alpha.means<-matrix(0, 12,4)
rownames(Alpha.means)<-NULL
colnames(Alpha.means)<-c("Activity", "Mean", "95.Low", "95.High")
Alpha.means[,1]<-c("Municipal meeting", "Contacting municipality", "Contacting local authority", "Contacting national authority",
                    		"Solving Problems", "Improvements meeting", "Party meeting", "Voting",
                    		"Sharing online", "Petitioning", "Protesting", "Blocking")

for (j in 1:12) {

Alpha.means[j,2:4]<-c(mean(c(ALPHA[, seq(j, ncol(ALPHA), by=12)])), quantile(c(ALPHA[, seq(j, ncol(ALPHA), by=12)]), c(0.025,0.975)))

}

save(Alpha.means, file="Alpha.means.GovsupportModel")

Alpha.all<-cbind(rep(rownames(table(df$country.name)), each=12),  		
		rep(Alpha.means[,1], length(unique(df$country.name))), 
		colMeans(ALPHA), t(apply(ALPHA, 2, quantile, c(0.025, 0.975))))

colnames(Alpha.all)<-c("Country", "Activity",  "Mean", "95.Low", "95.High")
rownames(Alpha.all)<-NULL
save(Alpha.all, file="Alpha.all.GovsupportModel")


ALPHA.CONV<-rbind(t(Results$alpha_conv[,,1]), 
		t(Results$alpha_conv[,,2]),
		t(Results$alpha_conv[,,3]))



Alpha.conv.means<-matrix(0, 12,4)
rownames(Alpha.conv.means)<-NULL
colnames(Alpha.conv.means)<-NULL

Alpha.conv.means[,1]<-c("Municipal meeting", "Contacting municipality", "Contacting local authority", "Contacting national authority",
                    		"Solving Problems", "Improvements meeting", "Party meeting", "Voting",
                    		"Sharing online", "Petitioning", "Protesting", "Blocking")
Alpha.conv.means[,2]<-"Conventional"

for (j in 1:12) {

m<-round(mean(c(ALPHA.CONV[, seq(j, ncol(ALPHA.CONV), by=12)])), digits=3)
q.l<-round(quantile(c(ALPHA.CONV[, seq(j, ncol(ALPHA.CONV), by=12)]), c(0.025,0.975))[1], digits=3)
q.h<-round(quantile(c(ALPHA.CONV[, seq(j, ncol(ALPHA.CONV), by=12)]), c(0.025,0.975))[2], digits=3)



if (nchar(m)<5 & grepl("\\.", m)==FALSE) {
m<-paste(m,".", sep="")
while (nchar(m)<5) {
m<-paste(m, "0", sep="")
} 
} else if (nchar(m)<5 & grepl("\\.", m)==TRUE) {
while (nchar(m)<5) {
m<-paste(m, "0", sep="")
} 
}


if (nchar(q.l)<5 & grepl("\\.", q.l)==FALSE) {
q.l<-paste(q.l,".", sep="")
while (nchar(q.l)<5) {
q.l<-paste(q.l, "0", sep="")
} 
} else if (nchar(q.l)<5 & grepl("\\.", q.l)==TRUE) {
while (nchar(q.l)<5) {
q.l<-paste(q.l, "0", sep="")
} 
}

if (nchar(q.h)<5 & grepl("\\.", q.h)==FALSE) {
q.h<-paste(q.h,".", sep="")
while (nchar(q.h)<5) {
q.h<-paste(q.h, "0", sep="")
} 
} else if (nchar(q.h)<5 & grepl("\\.", q.h)==TRUE) {
while (nchar(q.h)<5) {
q.h<-paste(q.h, "0", sep="")
} 
}




Alpha.conv.means[j,3:4]<-c(m, paste(paste("(", q.l, ",", sep=""), paste(q.h,")", sep=""),sep=""))


}


	


Alpha.conv.all<-cbind(rep(rownames(table(df$country.name)), each=12),  		
		rep(Alpha.conv.means[,1], length(unique(df$country.name))), "Conventional",
		colMeans(ALPHA.CONV), t(apply(ALPHA.CONV, 2, quantile, c(0.025, 0.975))))

colnames(Alpha.conv.all)<-c("Country", "Activity", "Dimension", "Mean", "95.Low", "95.High")
rownames(Alpha.conv.all)<-NULL


ALPHA.UNCONV<-rbind(t(Results$alpha_unconv[,,1]), 
		t(Results$alpha_unconv[,,2]),
		t(Results$alpha_unconv[,,3]))



Alpha.unconv.means<-matrix(0, 12,4)
Alpha.unconv.means[,1]<-c("Municipal meeting", "Contacting municipality", "Contacting local authority", "Contacting national authority",
                    		"Solving Problems", "Improvements meeting", "Party meeting", "Voting",
                    		"Sharing online", "Petitioning", "Protesting", "Blocking")
Alpha.unconv.means[,2]<-"Unconventional"

for (j in 1:12) {

m<-round(mean(c(ALPHA.UNCONV[, seq(j, ncol(ALPHA.UNCONV), by=12)])), digits=3)
q.l<-round(quantile(c(ALPHA.UNCONV[, seq(j, ncol(ALPHA.UNCONV), by=12)]), c(0.025,0.975))[1], digits=3)
q.h<-round(quantile(c(ALPHA.UNCONV[, seq(j, ncol(ALPHA.UNCONV), by=12)]), c(0.025,0.975))[2], digits=3)


if (nchar(m)<5 & grepl("\\.", m)==FALSE) {
m<-paste(m,".", sep="")
while (nchar(m)<5) {
m<-paste(m, "0", sep="")
} 
} else if (nchar(m)<5 & grepl("\\.", m)==TRUE) {
while (nchar(m)<5) {
m<-paste(m, "0", sep="")
} 
}


if (nchar(q.l)<5 & grepl("\\.", q.l)==FALSE) {
q.l<-paste(q.l,".", sep="")
while (nchar(q.l)<5) {
q.l<-paste(q.l, "0", sep="")
} 
} else if (nchar(q.l)<5 & grepl("\\.", q.l)==TRUE) {
while (nchar(q.l)<5) {
q.l<-paste(q.l, "0", sep="")
} 
}

if (nchar(q.h)<5 & grepl("\\.", q.h)==FALSE) {
q.h<-paste(q.h,".", sep="")
while (nchar(q.h)<5) {
q.h<-paste(q.h, "0", sep="")
} 
} else if (nchar(q.h)<5 & grepl("\\.", q.h)==TRUE) {
while (nchar(q.h)<5) {
q.h<-paste(q.h, "0", sep="")
} 
}

Alpha.unconv.means[j,3:4]<-c(m, paste(paste("(", q.l, ",", sep=""), paste(q.h,")", sep=""),sep=""))


}



Alpha.unconv.all<-cbind(rep(rownames(table(df$country.name)), each=12),  		
		rep(Alpha.unconv.means[,1], length(unique(df$country.name))), "Unconventional",
		colMeans(ALPHA.UNCONV), 
		t(apply(ALPHA.UNCONV, 2, quantile, c(0.025, 0.975))))
colnames(Alpha.unconv.all)<-c("Country", "Activity", "Dimension", "Mean", "95.Low", "95.High")
rownames(Alpha.unconv.all)<-NULL

Country.Loadings<-data.frame(rbind(Alpha.conv.all, Alpha.unconv.all))
save(Country.Loadings, file="Country.Loadings.GovsupportModel")




BETA2<-rbind((t(Results$beta2[,,1])),
	(t(Results$beta2[,,2])),
	(t(Results$beta2[,,3])))


BETA3<-rbind((t(Results$beta3[,,1])),
	(t(Results$beta3[,,2])),
	(t(Results$beta3[,,3])))


BETA4<-rbind((t(Results$beta4[,,1])),
	(t(Results$beta4[,,2])),
	(t(Results$beta4[,,3])))


NU<-rbind((t(Results$nu[,,1])),
	(t(Results$nu[,,2])),
	(t(Results$nu[,,3])))


NU2<-NU[, 1:length(unique(Country))]
NU3<-NU[, (1+length(unique(Country))): (2*length(unique(Country)))]
NU4<-NU[, (1+2*length(unique(Country))): (3*length(unique(Country)))]



Beta.Agitator<-BETA2[seq(1, nrow(BETA2), length.out=1000),]
Beta.Conventional<-BETA3[seq(1, nrow(BETA3), length.out=1000),]
Beta.Activist<-BETA4[seq(1, nrow(BETA4), length.out=1000),]
Nu.Agitator<-NU2[seq(1, nrow(NU2), length.out=1000),]
Nu.Conventional<-NU3[seq(1, nrow(NU3), length.out=1000),]
Nu.Activist<-NU4[seq(1, nrow(NU4), length.out=1000),]


Activist<-cbind(1:23,  colMeans(Nu.Activist), t(apply(Nu.Activist,2,quantile, c(0.025, 0.975))))
Agitator<-cbind(1:23, colMeans(Nu.Agitator),  t(apply(Nu.Agitator,2,quantile, c(0.025, 0.975))))
Conventional<-cbind(1:23, colMeans(Nu.Conventional),t(apply(Nu.Conventional,2,quantile, c(0.025, 0.975))))


save(Activist, file="Activist.GovsupportModel")
save(Agitator, file="Agitator.GovsupportModel")
save(Conventional, file="Conventional.GovsupportModel")





prob.Agitator<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X%*%Beta.Agitator[s,]+Nu.Agitator[s,Country])/(1+exp(X%*%Beta.Agitator[s,]+Nu.Agitator[s,Country])+
			exp(X%*%Beta.Conventional[s,]+Nu.Conventional[s,Country])+exp(X%*%Beta.Activist[s,]+Nu.Activist[s,Country])))

prob.Conventional<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X%*%Beta.Conventional[s,]+Nu.Conventional[s,Country])/(1+exp(X%*%Beta.Agitator[s,]+Nu.Agitator[s,Country])+
			exp(X%*%Beta.Conventional[s,]+Nu.Conventional[s,Country])+exp(X%*%Beta.Activist[s,]+Nu.Activist[s,Country])))

prob.Activist<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X%*%Beta.Activist[s,]+Nu.Activist[s,Country])/(1+exp(X%*%Beta.Agitator[s,]+Nu.Agitator[s,Country])+
			exp(X%*%Beta.Conventional[s,]+Nu.Conventional[s,Country])+exp(X%*%Beta.Activist[s,]+Nu.Activist[s,Country])))
prob.Outsider<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X%*%Beta.Agitator[s,]+Nu.Agitator[s,Country])+
			exp(X%*%Beta.Conventional[s,]+Nu.Conventional[s,Country])+exp(X%*%Beta.Activist[s,]+Nu.Activist[s,Country])))


Posterior.Probs<-cbind(colMeans(prob.Outsider), colMeans(prob.Agitator), colMeans(prob.Conventional), colMeans(prob.Activist))
save(Posterior.Probs, file="Posterior.Probs.GovsupportModel")




Outsider.by.Country<-c()
Activist.by.Country<-c()
Conventional.by.Country<-c()
Agitator.by.Country<-c()

for (j in 1:length(unique(Country))) {

Outsider.by.Country<-rbind(Outsider.by.Country,
c(unique(Country)[j],mean(colMeans(prob.Outsider[Country==j,])), quantile(colMeans(prob.Outsider[Country==j,]), c(0.025,0.975)))
)


Activist.by.Country<-rbind(Activist.by.Country,
c(unique(Country)[j],mean(colMeans(prob.Activist[Country==j,])), quantile(colMeans(prob.Activist[Country==j,]), c(0.025,0.975)))
)


Conventional.by.Country<-rbind(Conventional.by.Country,
c(unique(Country)[j],mean(colMeans(prob.Conventional[Country==j,])), quantile(colMeans(prob.Conventional[Country==j,]), c(0.025,0.975)))
)


Agitator.by.Country<-rbind(Agitator.by.Country,
c(unique(Country)[j],mean(colMeans(prob.Agitator[Country==j,])), quantile(colMeans(prob.Agitator[Country==j,]), c(0.025,0.975)))
)

}


save(Outsider.by.Country, file="Outsider.by.Country.GovsupportModel")
save(Agitator.by.Country, file="Agitator.by.Country.GovsupportModel")
save(Activist.by.Country, file="Activist.by.Country.GovsupportModel")
save(Conventional.by.Country, file="Conventional.by.Country.GovsupportModel")



ALPHA<-rbind(t(Results$alpha[,,1]), 
		t(Results$alpha[,,2]),
		t(Results$alpha[,,3]))


ALPHA.CONV<-rbind(t(Results$alpha_conv[,,1]), 
		t(Results$alpha_conv[,,2]),
		t(Results$alpha_conv[,,3]))


ALPHA.UNCONV<-rbind(t(Results$alpha_unconv[,,1]), 
		t(Results$alpha_unconv[,,2]),
		t(Results$alpha_unconv[,,3]))

Alpha<-ALPHA[seq(1, nrow(BETA2), length.out=1000),]
Alpha.Conv<-ALPHA.CONV[seq(1, nrow(BETA2), length.out=1000),]
Alpha.Unconv<-ALPHA.UNCONV[seq(1, nrow(BETA2), length.out=1000),]


P.CONV<-rbind(t(Results$P_conv[,,1]), 
		t(Results$P_conv[,,2]),
		t(Results$P_conv[,,3]))
P.Conv<-P.CONV[seq(1, nrow(BETA2), length.out=1000),]


P.UNCONV<-rbind(t(Results$P_unconv[,,1]), 
		t(Results$P_unconv[,,2]),
		t(Results$P_unconv[,,3]))

P.Unconv<-P.UNCONV[seq(1, nrow(BETA2), length.out=1000),]



C_unconv=rbinom(nrow(Y), 1, .5)+1
C.Conv<-matrix(0,nrow(Y), nrow(Alpha))
C.Unconv<-matrix(0,nrow(Y), nrow(Alpha))



for (s in 1:nrow(Alpha)){ 

pnum<-matrix(0,nrow(Y),2)

for (k in 1:2) {
lindividual<-matrix(0,nrow(Y),ncol(Y))
for (l in 1:ncol(Y)) {
mutmp=exp(Matrix.Country%*%t(matrix(Alpha[s,],ncol(Y), ncol(Matrix.Country)))[,l]+
(k-1)*((Matrix.Country%*%t(matrix(Alpha.Conv[s,],ncol(Y), ncol(Matrix.Country))))[,l])+
(C_unconv-1)*((Matrix.Country%*%t(matrix(Alpha.Unconv[s,],ncol(Y), ncol(Matrix.Country))))[,l]))/(1+exp(Matrix.Country%*%t(matrix(Alpha[s,],ncol(Y), ncol(Matrix.Country)))[,l]+
(k-1)*((Matrix.Country%*%t(matrix(Alpha.Conv[s,],ncol(Y), ncol(Matrix.Country))))[,l])+
(C_unconv-1)*((Matrix.Country%*%t(matrix(Alpha.Unconv[s,],ncol(Y), ncol(Matrix.Country))))[,l])))
lindividual[,l]<-(mutmp^Y[,l])*((1-mutmp)^(1-Y[,l]))
}

pnum[,k]<-apply(lindividual,1,prod)*P.Conv[s,k]
}

p<-pnum/(apply(pnum,1,sum))
p[is.na(p)==1]<-rep(1/2,2)
C_conv<-rMultinom(p,1)

C.Conv[,s]<-C_conv




for (k in 1:2) {
lindividual<-matrix(0,nrow(Y),ncol(Y))
for (l in 1:ncol(Y)) {
mutmp=exp(Matrix.Country%*%t(matrix(Alpha[s,],ncol(Y), ncol(Matrix.Country)))[,l]+
(C_conv-1)*((Matrix.Country%*%t(matrix(Alpha.Conv[s,],ncol(Y), ncol(Matrix.Country))))[,l])+
(k-1)*((Matrix.Country%*%t(matrix(Alpha.Unconv[s,],ncol(Y), ncol(Matrix.Country))))[,l]))/(1+exp(Matrix.Country%*%t(matrix(Alpha[s,],ncol(Y), ncol(Matrix.Country)))[,l]+
(C_conv-1)*((Matrix.Country%*%t(matrix(Alpha.Conv[s,],ncol(Y), ncol(Matrix.Country))))[,l])+
(k-1)*((Matrix.Country%*%t(matrix(Alpha.Unconv[s,],ncol(Y), ncol(Matrix.Country))))[,l])))
lindividual[,l]<-(mutmp^Y[,l])*((1-mutmp)^(1-Y[,l]))
}

pnum[,k]<-apply(lindividual,1,prod)*P.Unconv[s,k]
}

p<-pnum/(apply(pnum,1,sum))
p[is.na(p)==1]<-rep(1/2,2)
C_unconv<-rMultinom(p,1)

C.Unconv[,s]<-C_unconv


}



CC<-rowMeans(C.Conv-1)
CU<-rowMeans(C.Unconv-1)

Probs.Dimensions<-cbind(CC, CU)
save(Probs.Dimensions, file="Probs.Dimensions.GovsupportModel")

# Covariate Marginal Effects
colnames(X)<-c("Intercept", "Age", "Female", "Education", 
		 "Income", "Victimization", "Corruption",
		  "Gov.Support", "Rule of Law", "Compulsory Voting", "ENPP",
		"GDP", "Social.Spending")


dummies<-c(3, 6)
continuous<-c(2,4,5)
ordinal<-c(7,8)



Marginal.Individual<-matrix(0, nrow=4*(length(dummies)+length(continuous)+length(ordinal)), 3)
colnames(Marginal.Individual)<-c("Mean", "95.Low", "95.High")
rownames(Marginal.Individual)<-paste(rep(colnames(X)[sort(c(dummies, continuous, ordinal))], each=4),
rep(c("Outsider", "Agitator", "Conventional", "Activist"), (length(dummies)+length(continuous)+length(ordinal))), 
sep="-")

for (j in 1:length(dummies)) {
X.0<-X
X.1<-X

X.0[,dummies[j]]<-0
X.1[,dummies[j]]<-1


prob.Agitator.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Outsider.0<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Agitator.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))


prob.Outsider.1<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))


Marginal.Individual[grep(colnames(X)[dummies[j]], rownames(Marginal.Individual))[1],]<-c(mean(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0)),
											quantile(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0), c(0.025,0.975)))

Marginal.Individual[grep(colnames(X)[dummies[j]], rownames(Marginal.Individual))[2],]<-c(mean(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0)), quantile(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0), c(0.025,0.975)))


Marginal.Individual[grep(colnames(X)[dummies[j]], rownames(Marginal.Individual))[3],]<-c(mean(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0)), quantile(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0), c(0.025,0.975)))


Marginal.Individual[grep(colnames(X)[dummies[j]], rownames(Marginal.Individual))[4],]<-c(mean(colMeans(prob.Activist.1)-colMeans(prob.Activist.0)), quantile(colMeans(prob.Activist.1)-colMeans(prob.Activist.0), c(0.025,0.975)))

}


for (j in 1:length(continuous)){
X.0<-X
X.1<-X
X.1[,continuous[j]]<-X.0[,continuous[j]]+sd(X.0[,continuous[j]])


prob.Agitator.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Outsider.0<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Agitator.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))


prob.Outsider.1<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))



Marginal.Individual[grep(colnames(X)[continuous[j]], rownames(Marginal.Individual))[1],]<-c(mean(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0)),
											quantile(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0), c(0.025,0.975)))

Marginal.Individual[grep(colnames(X)[continuous[j]], rownames(Marginal.Individual))[2],]<-c(mean(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0)), quantile(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0), c(0.025,0.975)))


Marginal.Individual[grep(colnames(X)[continuous[j]], rownames(Marginal.Individual))[3],]<-c(mean(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0)), quantile(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0), c(0.025,0.975)))


Marginal.Individual[grep(colnames(X)[continuous[j]], rownames(Marginal.Individual))[4],]<-c(mean(colMeans(prob.Activist.1)-colMeans(prob.Activist.0)), quantile(colMeans(prob.Activist.1)-colMeans(prob.Activist.0), c(0.025,0.975)))

}

for (j in 1:length(ordinal)){
X.0<-X
X.1<-X
X.1[,ordinal[j]]<-X.0[,ordinal[j]]+1


prob.Agitator.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Outsider.0<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Agitator.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))


prob.Outsider.1<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))



Marginal.Individual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Individual))[1],]<-c(mean(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0)),
											quantile(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0), c(0.025,0.975)))

Marginal.Individual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Individual))[2],]<-c(mean(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0)), quantile(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0), c(0.025,0.975)))


Marginal.Individual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Individual))[3],]<-c(mean(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0)), quantile(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0), c(0.025,0.975)))


Marginal.Individual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Individual))[4],]<-c(mean(colMeans(prob.Activist.1)-colMeans(prob.Activist.0)), quantile(colMeans(prob.Activist.1)-colMeans(prob.Activist.0), c(0.025,0.975)))

}


Marginal.Individual[,1:3]<-100*Marginal.Individual[,1:3]

## Contextual

continuous<-c(9, 11:13)
ordinal<-10



Marginal.Contextual<-matrix(0, nrow=4*(length(continuous)+length(ordinal)), 3)
colnames(Marginal.Contextual)<-c("Mean", "95.Low", "95.High")
rownames(Marginal.Contextual)<-paste(rep(colnames(X)[sort(c(continuous, ordinal))], each=4),
rep(c("Outsider", "Agitator", "Conventional", "Activist"), (length(continuous)+length(ordinal))), 
sep="-")



for (j in 1:length(continuous)){
X.0<-X
X.1<-X
X.1[,continuous[j]]<-X.0[,continuous[j]]+sd(X.0[,continuous[j]])


prob.Agitator.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Outsider.0<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Agitator.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))


prob.Outsider.1<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))



Marginal.Contextual[grep(colnames(X)[continuous[j]], rownames(Marginal.Contextual))[1],]<-c(mean(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0)),
											quantile(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0), c(0.025,0.975)))

Marginal.Contextual[grep(colnames(X)[continuous[j]], rownames(Marginal.Contextual))[2],]<-c(mean(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0)), quantile(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0), c(0.025,0.975)))


Marginal.Contextual[grep(colnames(X)[continuous[j]], rownames(Marginal.Contextual))[3],]<-c(mean(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0)), quantile(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0), c(0.025,0.975)))


Marginal.Contextual[grep(colnames(X)[continuous[j]], rownames(Marginal.Contextual))[4],]<-c(mean(colMeans(prob.Activist.1)-colMeans(prob.Activist.0)), quantile(colMeans(prob.Activist.1)-colMeans(prob.Activist.0), c(0.025,0.975)))

}

for (j in 1:length(ordinal)){
X.0<-X
X.1<-X
X.1[,ordinal[j]]<-X.0[,ordinal[j]]+1


prob.Agitator.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Outsider.0<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Agitator.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))


prob.Outsider.1<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))



Marginal.Contextual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Contextual))[1],]<-c(mean(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0)),
											quantile(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0), c(0.025,0.975)))

Marginal.Contextual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Contextual))[2],]<-c(mean(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0)), quantile(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0), c(0.025,0.975)))


Marginal.Contextual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Contextual))[3],]<-c(mean(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0)), quantile(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0), c(0.025,0.975)))


Marginal.Contextual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Contextual))[4],]<-c(mean(colMeans(prob.Activist.1)-colMeans(prob.Activist.0)), quantile(colMeans(prob.Activist.1)-colMeans(prob.Activist.0), c(0.025,0.975)))

}

Marginal.Contextual[,1:3]<-100*Marginal.Contextual[,1:3]

save(Marginal.Individual, file="Marginal.Individual.GovsupportModel")
save(Marginal.Contextual, file="Marginal.Contextual.GovsupportModel")


Table.A.9<-rbind(Alpha.conv.means,Alpha.unconv.means)
Table.A.9<-Table.A.9[c(8, 1:4, 6,5, 7,10, 9, 11, 12, 20, 13:16, 18, 17, 19, 22, 21, 23, 24),] 
colnames(Table.A.9)<-c("Activity", "Dimension", "Mean", "Interval")
print(Table.A.9)

Table.A.9<-cbind(Table.A.9[1:12,-c(2)],Table.A.9[13:24,3:4])


Titles<-matrix(c("", "Conventional", "Dimension", "Unconventional", "Dimension"),nrow=1)

g1 <- tableGrob(Table.A.9, rows = NULL)
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g1), l = 1, r = ncol(g1))
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g1), l = 1, r = 1)
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g1), l = 1, r = 3)
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 1, l = 1, r = ncol(g1))


g0 <- tableGrob(Titles, rows = NULL)
g0 <- gtable_add_grob(g0,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g0), l = 1, r = 3)
g0 <- gtable_add_grob(g0,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g0), l = 1, r = 1)
g0 <- gtable_add_grob(g0,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 1, l = 1, r = ncol(g0))

g<-rbind(g0,g1)


pdf(file = "Table.A.9.pdf", height=10, width=10)
grid.draw(g)
dev.off()


################################################# Figure A.20 ##################################################################################

rm(list=ls())
load("Country.Loadings.GovsupportModel")


Country.Loadings$abbr<-NA
Country.Loadings$abbr[Country.Loadings$Country=="Argentina"]<-"ARG"
Country.Loadings$abbr[Country.Loadings$Country=="Belize"]<-"BLZ"
Country.Loadings$abbr[Country.Loadings$Country=="Brazil"]<-"BRA"
Country.Loadings$abbr[Country.Loadings$Country=="Chile"]<-"CHL"
Country.Loadings$abbr[Country.Loadings$Country=="Colombia"]<-"COL"
Country.Loadings$abbr[Country.Loadings$Country=="Costa Rica"]<-"CRI"
Country.Loadings$abbr[Country.Loadings$Country=="Dominican Republic"]<-"DOM"
Country.Loadings$abbr[Country.Loadings$Country=="Ecuador"]<-"ECU"
Country.Loadings$abbr[Country.Loadings$Country=="El Salvador"]<-"ESV"
Country.Loadings$abbr[Country.Loadings$Country=="Guatemala"]<-"GTM"
Country.Loadings$abbr[Country.Loadings$Country=="Guyana"]<-"GUY"
Country.Loadings$abbr[Country.Loadings$Country=="Haiti"]<-"HTI"
Country.Loadings$abbr[Country.Loadings$Country=="Honduras"]<-"HND"
Country.Loadings$abbr[Country.Loadings$Country=="Jamaica"]<-"JAM"
Country.Loadings$abbr[Country.Loadings$Country=="Mexico"]<-"MEX"
Country.Loadings$abbr[Country.Loadings$Country=="Nicaragua"]<-"NIC"
Country.Loadings$abbr[Country.Loadings$Country=="Panama"]<-"PAN"
Country.Loadings$abbr[Country.Loadings$Country=="Paraguay"]<-"PAR"
Country.Loadings$abbr[Country.Loadings$Country=="Peru"]<-"PER"
Country.Loadings$abbr[Country.Loadings$Country=="Suriname"]<-"SUR"
Country.Loadings$abbr[Country.Loadings$Country=="Trinidad and Tobago"]<-"TTO"
Country.Loadings$abbr[Country.Loadings$Country=="Uruguay"]<-"URU"
Country.Loadings$abbr[Country.Loadings$Country=="Venezuela"]<-"VEN"



activities=c("Municipal meeting", "Contacting municipality", "Contacting local authority", 
		"Contacting national authority",
             "Solving Problems", "Improvements meeting", 
		 "Party meeting", "Voting",
             "Sharing online", "Petitioning", "Protesting", "Blocking")




for (k in 4:6) {
Country.Loadings[,k]<-as.numeric(levels(Country.Loadings[,k]))[Country.Loadings[,k]]
}



first<-c(8,1,2,3,4,6)

for (i in 1:length(activities)) {
a<-subset(Country.Loadings[as.character(Country.Loadings$Activity)==activities[i],])

if (i==4) {
title="Contacting nat. authority"
} else if (i==5) {
title="Solving comm. problems" 
} else {
title=activities[i]
}

if (i %in%first) {
m<-c(0.25,0.5,0,0)
} else m<-c(0.25,0.5,0,0)


if (i <12) {
 p=ggplot(a, aes(x=abbr, y=Mean, group=Dimension, color=Dimension))+
geom_point()+
geom_errorbar(aes(ymin=X95.Low,ymax=X95.High),width=0.1,
position=position_dodge(0.05))+
 ggtitle(title)+
theme_bw()+ylab("")+xlab("")+ 
theme(axis.text.x=element_text(angle=90,size=6))+ylim(c(0,4.5))+
theme(plot.margin=unit(m, "lines"))+
theme(axis.text.x = element_text(size=7, vjust=0.2))+
theme(legend.position="none")+theme(plot.title = element_text(size = 11))+
scale_color_manual(values=c("black", "gray"))+theme(plot.title = element_text(hjust = 0))
assign(paste("plot",i,sep=""),p)
} else if (i==12) {
 p=ggplot(a, aes(x=abbr, y=Mean, group=Dimension, color=Dimension))+
geom_point()+
geom_errorbar(aes(ymin=X95.Low,ymax=X95.High),width=0.1,
position=position_dodge(0.05))+
 ggtitle(title)+
theme_bw()+ylab("")+xlab("")+ 
theme(axis.text.x=element_text(angle=90,size=6))+ylim(c(0,4.5))+
theme(plot.margin=unit(m, "lines"))+
theme(axis.text.x = element_text(size=7, vjust=0.2))+
theme(plot.title = element_text(size = 11))+
scale_color_manual(values=c("black", "gray"))+theme(plot.title = element_text(hjust = 0))+
theme(legend.position="none")
assign(paste("plot",i,sep=""),p)
}
}



# Multiple plot function
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



pdf(file = "Figure.A.20.pdf")
grid.draw(multiplot(plot8, plot3, plot5, plot9,
   plot1, plot4, plot7, plot11,
   plot2, plot6, plot10, plot12,
	cols=3))
dev.off()


################################################# Figure A.21 ##################################################################################

rm(list=ls())
rm(list=ls())
load("Alpha.all.GovsupportModel")



df.figureA21<-data.frame(Alpha.all)



df.figureA21$abbr<-NA
df.figureA21$abbr<-NA
df.figureA21$abbr[df.figureA21$Country=="Argentina"]<-"ARG"
df.figureA21$abbr[df.figureA21$Country=="Belize"]<-"BLZ"
df.figureA21$abbr[df.figureA21$Country=="Brazil"]<-"BRA"
df.figureA21$abbr[df.figureA21$Country=="Chile"]<-"CHL"
df.figureA21$abbr[df.figureA21$Country=="Colombia"]<-"COL"
df.figureA21$abbr[df.figureA21$Country=="Costa Rica"]<-"CRI"
df.figureA21$abbr[df.figureA21$Country=="Dominican Republic"]<-"DOM"
df.figureA21$abbr[df.figureA21$Country=="Ecuador"]<-"ECU"
df.figureA21$abbr[df.figureA21$Country=="El Salvador"]<-"ESV"
df.figureA21$abbr[df.figureA21$Country=="Guatemala"]<-"GTM"
df.figureA21$abbr[df.figureA21$Country=="Guyana"]<-"GUY"
df.figureA21$abbr[df.figureA21$Country=="Haiti"]<-"HTI"
df.figureA21$abbr[df.figureA21$Country=="Honduras"]<-"HND"
df.figureA21$abbr[df.figureA21$Country=="Jamaica"]<-"JAM"
df.figureA21$abbr[df.figureA21$Country=="Mexico"]<-"MEX"
df.figureA21$abbr[df.figureA21$Country=="Nicaragua"]<-"NIC"
df.figureA21$abbr[df.figureA21$Country=="Panama"]<-"PAN"
df.figureA21$abbr[df.figureA21$Country=="Paraguay"]<-"PAR"
df.figureA21$abbr[df.figureA21$Country=="Peru"]<-"PER"
df.figureA21$abbr[df.figureA21$Country=="Suriname"]<-"SUR"
df.figureA21$abbr[df.figureA21$Country=="Trinidad and Tobago"]<-"TTO"
df.figureA21$abbr[df.figureA21$Country=="Uruguay"]<-"URU"
df.figureA21$abbr[df.figureA21$Country=="Venezuela"]<-"VEN"


load("Alpha.means.GovsupportModel")
df.figureA21.2<-data.frame(Alpha.means)
df.figureA21.2$abbr<-"Average"

df.figureA21.3<-rbind(df.figureA21.2, df.figureA21[,-1])

df.figureA21.3$abbr<-factor(df.figureA21.3$abbr, 
levels=c(unique(df.figureA21$abbr), "Average"))

df.figureA21.3$Activity<-factor(df.figureA21.3$Activity,
levels=c("Voting", "Municipal meeting",
	"Contacting municipality", "Contacting local authority",
	"Contacting national authority",
	"Improvements meeting", "Solving Problems",
	"Party meeting",
	"Petitioning",  "Sharing online", "Protesting", "Blocking"),
labels=c("Voting", "Municipal meeting",
	"Contacting municipality", "Contacting local authority",
	"Contacting nat. authority",
	"Improvements meeting", "Solving comm. problems",
	"Party meeting", 
	"Petitioning", "Sharing online", "Protesting", "Blocking"))


for (k in 2:4){

df.figureA21.3[,k]<-as.numeric(levels(df.figureA21.3[,k]))[df.figureA21.3[,k]]
}

p<-ggplot(df.figureA21.3, aes(x=abbr, y=Mean, colour=abbr, fill==abbr))+
geom_point()+geom_errorbar(aes(ymin=X95.Low,ymax=X95.High),width=0.1,
position=position_dodge(0.05))+
facet_wrap(~Activity)+theme_bw()+
theme(axis.text.x = element_text(size=5, angle=90, vjust=0.2))+
theme(strip.text = element_text(size = 8))+
scale_color_manual(values=c(rep("gray",23),"black"))+
xlab("")+ ylab("")+theme(legend.position = "none") 

pdf(file = "Figure.A.21.pdf")
grid.draw(p)
dev.off()


################################################# Figure A.22 ##################################################################################


rm(list=ls())

load("Posterior.Probs.GovsupportModel")

load("Probs.Dimensions.GovsupportModel")

prob.conv.type<-Probs.Dimensions[,1]
prob.unconv.type<-Probs.Dimensions[,2]



per.outsiders <- round(colMeans(Posterior.Probs)[1], digits=2)
if (nchar(per.outsiders)<4) {
while (nchar(per.outsiders)<4) {
per.outsiders<-paste(per.outsiders, "0", sep="")
} 
}


per.agitators<- round(colMeans(Posterior.Probs)[2], digits=2)
if (nchar(per.agitators)<4) {
while (nchar(per.agitators)<4) {
per.agitators<-paste(per.agitators, "0", sep="")
} 
}


per.activists<- round(colMeans(Posterior.Probs)[4], digits=2)
if (nchar(per.activists)<4) {
while (nchar(per.activists)<4) {
per.activists<-paste(per.activists, "0", sep="")
} 
}

per.conventionals <-max(round(colMeans(Posterior.Probs)[3], digits=2), 1-round(colMeans(Posterior.Probs)[1], digits=2)-round(colMeans(Posterior.Probs)[2], digits=2)-round(colMeans(Posterior.Probs)[4], digits=2))
if (nchar(per.conventionals)<4) {
while (nchar(per.conventionals)<4) {
per.conventionals<-paste(per.conventionals, "0", sep="")
} 
}



Points<-cbind(prob.conv.type, prob.unconv.type)
Points<-data.frame(Points)
names(Points)<-c("x", "y")


p.1<-ggplot(Points, aes(x=x, y=y))+
geom_point(colour = "lightgray", size = 0.5)+theme_bw()+
xlab("Probability of high conventional type")+
ylab("Probability of high 
unconventional type")+
theme(axis.text.x = element_text(color="black", size=11))+
	theme(axis.text.x = element_text(color="black", size=11))+
 theme(axis.title.y = element_text(size=13, face="bold"))+
 theme(axis.title.x = element_text(size=13, face="bold"))+
 theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))+
 theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))+
 geom_vline(xintercept = 0.5, linetype="dashed")+
 geom_hline(yintercept = 0.5, linetype="dashed")+
annotate(geom="text", x=0.25, y=0.25, label=paste("Outsiders",
per.outsiders, sep="\n"), color="black", size=5)+
annotate(geom="text", x=0.75, y=0.25, label=paste("Conventionals",
per.conventionals, sep="\n"), color="black", size=5)+
annotate(geom="text", x=0.25, y=0.75, label=paste("Agitators",
per.agitators, sep="\n"), color="black", size=5)+
annotate(geom="text", x=0.75, y=0.75, label=paste("Activists",
per.activists, sep="\n"), color="black", size=5)#+
theme(plot.margin=unit(c(1.5,1,0,1), "lines"))



Types<-cbind(c("Outsider", "Agitator", "Conventional", "Activist"), colMeans(Posterior.Probs), t(apply(Posterior.Probs, 2, quantile, c(0.025,0.975))))
Types<-data.frame(Types)
names(Types)<-c("Type", "Mean", "Low", "High")

for (k in 2:ncol(Types)) {
Types[,k]<- as.numeric(levels(Types[,k]))[Types[,k]] 
}


Types$Type<- factor(Types$Type, 
	levels = c("Outsider", "Agitator", "Conventional",
		"Activist"))

p.2<- ggplot(Types, aes(x=Type, y=Mean)) + 
  geom_point(size=2, fill="black") + ylab("Probability of 
type assignment")+
	xlab("")+
  geom_errorbar(aes(ymin=Low, ymax=High), width=.1,
                 position=position_dodge(0.05))+
	theme_bw()+ ylim(c(0,0.82))+
	theme(axis.text.x = element_text(color="black", size=14))+
	theme(axis.text.x = element_text(color="black", size=11))+
 theme(axis.title.y = element_text(size=13, face="bold"))+
theme(plot.margin=unit(c(1.5,1,0,1), "lines"))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))


# Multiple plot function
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

pdf(file = "Figure.A.22.pdf", height=10)
grid.draw(multiplot(p.1, p.2, col=1))
dev.off()


################################################# Figure A.23 ##################################################################################

rm(list=ls())

# Multiple plot function
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


load("Outsider.by.Country.GovsupportModel")
load("Agitator.by.Country.GovsupportModel")
load("Conventional.by.Country.GovsupportModel")
load("Activist.by.Country.GovsupportModel")

df.outsiders<-data.frame(cbind("Outsiders", Outsider.by.Country))
names(df.outsiders)<-c("Type", "Country", "Mean", "Low", "High")

df.agitators<-data.frame(cbind("Agitators", Agitator.by.Country))
names(df.agitators)<-c("Type", "Country", "Mean", "Low", "High")
 
 
df.conventionals<-data.frame(cbind("Conventionals", Conventional.by.Country))
names(df.conventionals)<-c("Type", "Country", "Mean", "Low", "High")

df.activists<-data.frame(cbind("Activists", Activist.by.Country))
names(df.activists)<-c("Type", "Country", "Mean", "Low", "High")
 
df.Figure.A.23<-rbind(df.outsiders,	df.agitators,
	 df.conventionals,
	 df.activists)




df.Figure.A.23$abbr<-NA
df.Figure.A.23$abbr[df.Figure.A.23$Country==1]<-"ARG"
df.Figure.A.23$abbr[df.Figure.A.23$Country==2]<-"BLZ"
df.Figure.A.23$abbr[df.Figure.A.23$Country==3]<-"BRA"
df.Figure.A.23$abbr[df.Figure.A.23$Country==4]<-"CHL"
df.Figure.A.23$abbr[df.Figure.A.23$Country==5]<-"COL"
df.Figure.A.23$abbr[df.Figure.A.23$Country==6]<-"CRI"
df.Figure.A.23$abbr[df.Figure.A.23$Country==7]<-"DOM"
df.Figure.A.23$abbr[df.Figure.A.23$Country==8]<-"ECU"
df.Figure.A.23$abbr[df.Figure.A.23$Country==9]<-"ESV"
df.Figure.A.23$abbr[df.Figure.A.23$Country==10]<-"GTM"
df.Figure.A.23$abbr[df.Figure.A.23$Country==11]<-"GUY"
df.Figure.A.23$abbr[df.Figure.A.23$Country==12]<-"HTI"
df.Figure.A.23$abbr[df.Figure.A.23$Country==13]<-"HND"
df.Figure.A.23$abbr[df.Figure.A.23$Country==14]<-"JAM"
df.Figure.A.23$abbr[df.Figure.A.23$Country==15]<-"MEX"
df.Figure.A.23$abbr[df.Figure.A.23$Country==16]<-"NIC"
df.Figure.A.23$abbr[df.Figure.A.23$Country==17]<-"PAN"
df.Figure.A.23$abbr[df.Figure.A.23$Country==18]<-"PAR"
df.Figure.A.23$abbr[df.Figure.A.23$Country==19]<-"PER"
df.Figure.A.23$abbr[df.Figure.A.23$Country==20]<-"SUR"
df.Figure.A.23$abbr[df.Figure.A.23$Country==21]<-"TTO"
df.Figure.A.23$abbr[df.Figure.A.23$Country==22]<-"URU"
df.Figure.A.23$abbr[df.Figure.A.23$Country==23]<-"VEN"



for (k in 3:5){

df.Figure.A.23[,k]<-as.numeric(levels(df.Figure.A.23[,k]))[df.Figure.A.23[,k]]
}


Outsiders<-df.Figure.A.23[df.Figure.A.23$Type=="Outsiders",-c(1,2)]

Agitators<-df.Figure.A.23[df.Figure.A.23$Type=="Agitators",-c(1,2)]

Conventionals<-df.Figure.A.23[df.Figure.A.23$Type=="Conventionals",-c(1,2)]

Activists<-df.Figure.A.23[df.Figure.A.23$Type=="Activists",-c(1,2)]


p.1<-ggplot(Outsiders,  aes(as.factor(abbr), Mean)) +
 geom_point(size=1, fill="black") +
  geom_errorbar(aes(ymin=Low, ymax=High), width=.1,
                 position=position_dodge(0.05))+
	ylab("")+xlab("")+ggtitle("Outsiders")+theme_bw()+
	theme(plot.margin=unit(c(0.5,1,0,0), "lines"))+
	#scale_y_continuous(limits=c(0.64,0.775), breaks=c(0.65, 0.675, 0.70, 0.725, 0.75))+
	theme(axis.text.x=element_text(size=8))+theme(plot.title = element_text(hjust = 0))


p.2<-ggplot(Agitators,  aes(as.factor(abbr), Mean)) +
 geom_point(size=1, fill="black") +
  geom_errorbar(aes(ymin=Low, ymax=High), width=.1,
                 position=position_dodge(0.05))+
	ylab("")+xlab("")+ggtitle("Agitators")+theme_bw()+
	theme(plot.margin=unit(c(0.5,1,0,0), "lines"))+
	#scale_y_continuous(limits=c(0.06,0.15), breaks=seq(0.08, 0.14, by=0.02))+
theme(axis.text.x=element_text(size=8))+theme(plot.title = element_text(hjust = 0))


p.3<-ggplot(Conventionals,  aes(as.factor(abbr), Mean)) +
 geom_point(size=1, fill="black") +
  geom_errorbar(aes(ymin=Low, ymax=High), width=.1,
                 position=position_dodge(0.05))+
	ylab("")+xlab("")+ggtitle("Conventionals")+theme_bw()+
	theme(plot.margin=unit(c(0.5,1,0,0), "lines"))+
	#scale_y_continuous(limits=c(0.11,0.2251), breaks=c(0.125, 0.15, 0.175, 0.20, 0.225))+
theme(axis.text.x=element_text(size=8))+theme(plot.title = element_text(hjust = 0))


p.4<-ggplot(Activists,  aes(as.factor(abbr), Mean)) +
 geom_point(size=1, fill="black") +
  geom_errorbar(aes(ymin=Low, ymax=High), width=.1,
                 position=position_dodge(0.05))+
	ylab("")+xlab("")+ggtitle("Activists")+theme_bw()+
	theme(plot.margin=unit(c(0.5,1,0,0), "lines"))+
	#scale_y_continuous(limits=c(0.01,0.07), breaks=seq(0.02, 0.06, by=0.01))+
theme(axis.text.x=element_text(size=8))+theme(plot.title = element_text(hjust = 0))




pdf(file = "Figure.A.23.pdf")
grid.draw(multiplot(p.1, p.2, p.3, p.4, cols=1))
dev.off()


################################################# Figure A.24 ##################################################################################

rm(list=ls())
load("Marginal.Individual.GovsupportModel")
load("Marginal.Contextual.GovsupportModel")



Figure.Individual<-cbind(unlist(strsplit(rownames(Marginal.Individual), "-"))[seq(2,length(unlist(strsplit(rownames(Marginal.Individual), "-"))), by=2)],
unlist(strsplit(rownames(Marginal.Individual), "-"))[seq(1,length(unlist(strsplit(rownames(Marginal.Individual), "-"))), by=2)],
Marginal.Individual[,1:3])
Figure.Individual<-data.frame(Figure.Individual)
names(Figure.Individual)<-c("Type", "Variable", "Mean", "Low", "High")


Figure.Contextual<-cbind(unlist(strsplit(rownames(Marginal.Contextual), "-"))[seq(2,length(unlist(strsplit(rownames(Marginal.Contextual), "-"))), by=2)],
unlist(strsplit(rownames(Marginal.Contextual), "-"))[seq(1,length(unlist(strsplit(rownames(Marginal.Contextual), "-"))), by=2)],
Marginal.Contextual[,1:3])
Figure.Contextual<-data.frame(Figure.Contextual)
names(Figure.Contextual)<-c("Type", "Variable", "Mean", "Low", "High")



Figure.A.24<-rbind(Figure.Individual, Figure.Contextual)

Figure.A.24$Type<- factor(Figure.A.24$Type, 
	levels = c("Outsider",
		"Agitator",
		"Conventional", "Activist"))



Figure.A.24$Variable<-factor(Figure.A.24$Variable, 
	levels=rev(c("Age", "Female", "Education", 
			"Income", "Corruption", 
			"Victimization",
			"Gov.Support",
			"Rule of Law", "Compulsory Voting", "ENPP", 
			"GDP", "Social.Spending")),
       labels=rev(c("Age",
			  "Female",
			  "Education", 
			  "Relative Income (proxy)",
			  "Perceived Corruption", 
			   "Crime Victimization",
			 "Government Support",
			"Rule of Law",
			  "Compulsory Voting",
			  "ENPP", 
			  "GDP per capita",
			  "Social Spending")))




for (k in 3:5){
Figure.A.24[,k]<-as.numeric(levels(Figure.A.24[,k]))[Figure.A.24[,k]]
}


p<-ggplot(Figure.A.24,
 aes(y=Variable, x=Mean)) + 
    geom_errorbarh(aes(xmin=Low, 
	xmax=High, height=.2), colour="black") + facet_wrap(~Type)+
    geom_point(size=1.5)+
theme_bw()+xlab("Marginal effects (in percentage points)")+ylab("")+
geom_hline(yintercept=0, linetype="longdash", colour="gray40")+
theme(strip.text.x = element_text(size = 12, face="bold"))+
geom_vline(xintercept=0, linetype="longdash", colour="gray40")+
theme(strip.background = element_rect(fill = alpha('white'))) +
theme(axis.title.x=element_text(vjust=2, size=11))+
theme(axis.text.y=element_text(size=11))


pdf(file = "Figure.A.24.pdf")
grid.draw(p)
dev.off()


################################################# Figure A.25 ##################################################################################

rm(list=ls())


load("Activist.GovsupportModel")
load("Agitator.GovsupportModel")
load("Conventional.GovsupportModel")


Activist<-data.frame(Activist)
names(Activist)<-c("Country",  "Mean", "Low", "High")
Activist$abbr<-NA
Activist$abbr[Activist$Country==1]<-"ARG"
Activist$abbr[Activist$Country==2]<-"BLZ"
Activist$abbr[Activist$Country==3]<-"BRA"
Activist$abbr[Activist$Country==4]<-"CHL"
Activist$abbr[Activist$Country==5]<-"COL"
Activist$abbr[Activist$Country==6]<-"CRI"
Activist$abbr[Activist$Country==7]<-"DOM"
Activist$abbr[Activist$Country==8]<-"ECU"
Activist$abbr[Activist$Country==9]<-"ESV"
Activist$abbr[Activist$Country==10]<-"GTM"
Activist$abbr[Activist$Country==11]<-"GUY"
Activist$abbr[Activist$Country==12]<-"HTI"
Activist$abbr[Activist$Country==13]<-"HND"
Activist$abbr[Activist$Country==14]<-"JAM"
Activist$abbr[Activist$Country==15]<-"MEX"
Activist$abbr[Activist$Country==16]<-"NIC"
Activist$abbr[Activist$Country==17]<-"PAN"
Activist$abbr[Activist$Country==18]<-"PAR"
Activist$abbr[Activist$Country==19]<-"PER"
Activist$abbr[Activist$Country==20]<-"SUR"
Activist$abbr[Activist$Country==21]<-"TTO"
Activist$abbr[Activist$Country==22]<-"URU"
Activist$abbr[Activist$Country==23]<-"VEN"

Agitator<-data.frame(Agitator)
names(Agitator)<-c("Country",  "Mean", "Low", "High")
Agitator$abbr<-NA
Agitator$abbr[Agitator$Country==1]<-"ARG"
Agitator$abbr[Agitator$Country==2]<-"BLZ"
Agitator$abbr[Agitator$Country==3]<-"BRA"
Agitator$abbr[Agitator$Country==4]<-"CHL"
Agitator$abbr[Agitator$Country==5]<-"COL"
Agitator$abbr[Agitator$Country==6]<-"CRI"
Agitator$abbr[Agitator$Country==7]<-"DOM"
Agitator$abbr[Agitator$Country==8]<-"ECU"
Agitator$abbr[Agitator$Country==9]<-"ESV"
Agitator$abbr[Agitator$Country==10]<-"GTM"
Agitator$abbr[Agitator$Country==11]<-"GUY"
Agitator$abbr[Agitator$Country==12]<-"HTI"
Agitator$abbr[Agitator$Country==13]<-"HND"
Agitator$abbr[Agitator$Country==14]<-"JAM"
Agitator$abbr[Agitator$Country==15]<-"MEX"
Agitator$abbr[Agitator$Country==16]<-"NIC"
Agitator$abbr[Agitator$Country==17]<-"PAN"
Agitator$abbr[Agitator$Country==18]<-"PAR"
Agitator$abbr[Agitator$Country==19]<-"PER"
Agitator$abbr[Agitator$Country==20]<-"SUR"
Agitator$abbr[Agitator$Country==21]<-"TTO"
Agitator$abbr[Agitator$Country==22]<-"URU"
Agitator$abbr[Agitator$Country==23]<-"VEN"


Conventional<-data.frame(Conventional)
names(Conventional)<-c("Country", "Mean", "Low", "High")
Conventional$abbr<-NA
Conventional$abbr[Conventional$Country==1]<-"ARG"
Conventional$abbr[Conventional$Country==2]<-"BLZ"
Conventional$abbr[Conventional$Country==3]<-"BRA"
Conventional$abbr[Conventional$Country==4]<-"CHL"
Conventional$abbr[Conventional$Country==5]<-"COL"
Conventional$abbr[Conventional$Country==6]<-"CRI"
Conventional$abbr[Conventional$Country==7]<-"DOM"
Conventional$abbr[Conventional$Country==8]<-"ECU"
Conventional$abbr[Conventional$Country==9]<-"ESV"
Conventional$abbr[Conventional$Country==10]<-"GTM"
Conventional$abbr[Conventional$Country==11]<-"GUY"
Conventional$abbr[Conventional$Country==12]<-"HTI"
Conventional$abbr[Conventional$Country==13]<-"HND"
Conventional$abbr[Conventional$Country==14]<-"JAM"
Conventional$abbr[Conventional$Country==15]<-"MEX"
Conventional$abbr[Conventional$Country==16]<-"NIC"
Conventional$abbr[Conventional$Country==17]<-"PAN"
Conventional$abbr[Conventional$Country==18]<-"PAR"
Conventional$abbr[Conventional$Country==19]<-"PER"
Conventional$abbr[Conventional$Country==20]<-"SUR"
Conventional$abbr[Conventional$Country==21]<-"TTO"
Conventional$abbr[Conventional$Country==22]<-"URU"
Conventional$abbr[Conventional$Country==23]<-"VEN"




Agitator<-cbind("Agitator vs. Outsider", Agitator)
names(Agitator)[1]<-"Type"

Activist<-cbind("Activist vs. Outsider", Activist)
names(Activist)[1]<-"Type"

Conventional<-cbind("Conventional vs. Outsider", Conventional)
names(Conventional)[1]<-"Type"

Figure.A.25<-rbind(Agitator, Conventional, Activist)


Figure.A.25$abbr<-factor(Figure.A.25$abbr, 
	levels=rev(c("ARG", "BLZ", "BRA", "CHL", "COL",
"CRI", "DOM", "ECU", "ESV", "GTM", "GUY", "HTI",
"HND", "JAM", "MEX", "NIC", "PAN", "PAR", "PER", "SUR", "TTO", "URU", "VEN")))


p<-ggplot(Figure.A.25,
 aes(y=abbr, x=Mean)) + 
    geom_errorbarh(aes(xmin=Low, 
	xmax=High, height=.2), colour="black") + facet_wrap(~Type)+
    geom_point(size=1.5)+
theme_bw()+xlab("")+ylab("")+
geom_hline(yintercept=0, linetype="longdash", colour="gray40")+
theme(strip.text.x = element_text(size = 9, face="bold"))+
geom_vline(xintercept=0, linetype="longdash", colour="gray40")+
theme(strip.background = element_rect(fill = alpha('white'))) +
theme(axis.title.x=element_text(vjust=2, size=10))+
theme(axis.text.y=element_text(size=10))+
scale_x_continuous(breaks=c(-0.6, -0.3, 0, 0.3, 0.6))


pdf(file = "Figure.A.25.pdf")
grid.draw(p)
dev.off()





################################################# Figure A.26 ##################################################################################

rm(list=ls())

# First need to process the data, then produce the figure

load("dataset_2012_wpartysupport.RData")


df<-subset(dataset, select=c("country.name", "ActMeetMun",
"country.name", "ActMeetMun", "ActContMun",  "ActContAut" ,                           
"ActMeetImp" , "ActSolveProb" ,  "ActMeetPty" ,  "ActContGvt",                            
"ActProtest" , "ActBlock" ,  "ActVoted",  "ActSign" ,                              
"ActShare" , "education" ,  "female", "age", "relative.incomeproxy" ,
"victim", "corrupt", #"spatial.distance" ,
"voteintention.incumbent", 
"compulsory.voting", "gdp.pc", "rule.of.lw", "enp", "education.spending"
#"social.spending"
))                       
df<-na.exclude(df)


Y <- cbind(df$ActMeetMun, df$ActContMun, df$ActContAut,
		 df$ActContGvt, df$ActSolveProb, 
		df$ActMeetImp, df$ActMeetPty, df$ActVoted, 
		df$ActShare, df$ActSign, df$ActProtest, df$ActBlock)
colnames(Y)<-c("ActMeetMun", "ActContMun", "ActContAut", "ActContGvt",
		"ActSolveProb", "ActMeetImp", "ActMeetPty", "ActVoted",
		"ActShare", "ActSign", "ActProtest", "ActBlock")

Country<-as.numeric(factor(df$country.name), levels=unique(df$country.name))
		

Matrix.Country<-matrix(0,nrow(Y),length(unique(Country)))           
for (i in 1:length(unique(Country))) Matrix.Country[Country==i,i]<-1

df$age.centered<-(df$age-mean(df$age))/(2*sd(df$age))
df$education.centered<-(df$education-mean(df$education))/(2*sd(df$education))


df$relative.incomeproxy.centered<-(df$relative.incomeproxy-mean(df$relative.incomeproxy))/(2*sd(df$relative.incomeproxy))



X<-cbind(1, df$age.centered, df$female, df$education.centered, 
	df$relative.incomeproxy.centered, 
	df$victim, df$corrupt, 
	df$voteintention.incumbent,
	(df$rule.of.lw-mean(df$rule.of.lw))/(2*sd(df$rule.of.lw)),
		df$compulsory.voting,
		(df$enp-mean(df$enp))/(2*sd(df$enp)),
		(df$gdp.pc-mean(df$gdp.pc))/(2*sd(df$gdp.pc)),
		(df$education.spending-mean(df$education.spending))/(2*sd(df$education.spending))
)



load("Results_VoteintentionModel")

## Convergence Checks for Voteintention Model
Alpha<-mcmc.list(as.mcmc(t(Results$alpha[,,1])), 
		as.mcmc(t(Results$alpha[,,2])))
print(paste("Convergence Alpha:", gelman.diag(Alpha)[[2]]<1.2, sep=" "))


Alpha.Conv<-mcmc.list(as.mcmc(t(Results$alpha_conv[-seq(12,dim(Results$alpha_conv)[1],by=12),,1])), 
		as.mcmc(t(Results$alpha_conv[-seq(12,dim(Results$alpha_conv)[1],by=12),,2])))
print(paste("Convergence Alpha Conv:", gelman.diag(Alpha.Conv)[[2]]<1.2, sep=" "))


Alpha.Unconv<-mcmc.list(as.mcmc(t(Results$alpha_unconv[-seq(1,dim(Results$alpha_conv)[1],by=12),,1])), 
		as.mcmc(t(Results$alpha_unconv[-seq(1,dim(Results$alpha_conv)[1],by=12),,2])))
print(paste("Convergence Alpha Unconventional:", gelman.diag(Alpha.Unconv)[[2]]<1.2, sep=" "))



P.Conv<-mcmc.list(as.mcmc((Results$P_conv[2,,1])), 
		as.mcmc((Results$P_conv[2,,2])))
print(paste("Convergence Prob. Conventional:", gelman.diag(P.Conv)[[1]][,1]<1.2, sep=" "))


P.Unconv<-mcmc.list(as.mcmc((Results$P_unconv[2,,1])), 
		as.mcmc((Results$P_unconv[2,,2])))
print(paste("Convergence Prob. Unconventional", gelman.diag(P.Unconv)[[1]][,1]<1.2, sep=" "))


Beta2<-mcmc.list(as.mcmc(t(Results$beta2[,,1])),
	as.mcmc(t(Results$beta2[,,2])))
Beta3<-mcmc.list(as.mcmc(t(Results$beta3[,,1])),
	as.mcmc(t(Results$beta3[,,2])))
Beta4<-mcmc.list(as.mcmc(t(Results$beta4[,,1])),
	as.mcmc(t(Results$beta4[,,2])))
print(paste("Convergence Regression coefficients:", gelman.diag(Beta2)[[2]]<1.2 & gelman.diag(Beta3)[[2]]<1.2 & gelman.diag(Beta4)[[2]]<1.2, sep=" "))



BETA2<-rbind((t(Results$beta2[,,1])),
	(t(Results$beta2[,,2])))
BETA3<-rbind((t(Results$beta3[,,1])),
	(t(Results$beta3[,,2])))
BETA4<-rbind((t(Results$beta4[,,1])),
	(t(Results$beta4[,,2])))
NU<-rbind((t(Results$nu[,,1])),
	(t(Results$nu[,,2])))
NU2<-NU[, 1:length(unique(Country))]
NU3<-NU[, (1+length(unique(Country))): (2*length(unique(Country)))]
NU4<-NU[, (1+2*length(unique(Country))): (3*length(unique(Country)))]



Beta.Agitator<-BETA2[seq(1, nrow(BETA2), length.out=1000),]
Beta.Conventional<-BETA3[seq(1, nrow(BETA3), length.out=1000),]
Beta.Activist<-BETA4[seq(1, nrow(BETA4), length.out=1000),]
Nu.Agitator<-NU2[seq(1, nrow(NU2), length.out=1000),]
Nu.Conventional<-NU3[seq(1, nrow(NU3), length.out=1000),]
Nu.Activist<-NU4[seq(1, nrow(NU4), length.out=1000),]



prob.Agitator<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X%*%Beta.Agitator[s,]+Nu.Agitator[s,Country])/(1+exp(X%*%Beta.Agitator[s,]+Nu.Agitator[s,Country])+
			exp(X%*%Beta.Conventional[s,]+Nu.Conventional[s,Country])+exp(X%*%Beta.Activist[s,]+Nu.Activist[s,Country])))

prob.Conventional<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X%*%Beta.Conventional[s,]+Nu.Conventional[s,Country])/(1+exp(X%*%Beta.Agitator[s,]+Nu.Agitator[s,Country])+
			exp(X%*%Beta.Conventional[s,]+Nu.Conventional[s,Country])+exp(X%*%Beta.Activist[s,]+Nu.Activist[s,Country])))

prob.Activist<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X%*%Beta.Activist[s,]+Nu.Activist[s,Country])/(1+exp(X%*%Beta.Agitator[s,]+Nu.Agitator[s,Country])+
			exp(X%*%Beta.Conventional[s,]+Nu.Conventional[s,Country])+exp(X%*%Beta.Activist[s,]+Nu.Activist[s,Country])))
prob.Outsider<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X%*%Beta.Agitator[s,]+Nu.Agitator[s,Country])+
			exp(X%*%Beta.Conventional[s,]+Nu.Conventional[s,Country])+exp(X%*%Beta.Activist[s,]+Nu.Activist[s,Country])))




Probs.Voteintention<-cbind(Country, rowMeans(prob.Outsider), rowMeans(prob.Agitator), rowMeans(prob.Conventional), rowMeans(prob.Activist))

Posterior.Probs.Voteintention<-cbind(colMeans(prob.Outsider), colMeans(prob.Agitator), colMeans(prob.Conventional), colMeans(prob.Activist))

save(Posterior.Probs.Voteintention, file="Posterior.Probs.Voteintention")

save(Probs.Voteintention, file="Probs.Voteintention")






Outsider.by.Country.Voteintention<-c()
Activist.by.Country.Voteintention<-c()
Conventional.by.Country.Voteintention<-c()
Agitator.by.Country.Voteintention<-c()

for (j in 1:length(unique(Country))) {

Outsider.by.Country.Voteintention<-rbind(Outsider.by.Country.Voteintention,
c(unique(Country)[j],mean(colMeans(prob.Outsider[Country==j,])), quantile(colMeans(prob.Outsider[Country==j,]), c(0.025,0.975)))
)


Activist.by.Country.Voteintention<-rbind(Activist.by.Country.Voteintention,
c(unique(Country)[j],mean(colMeans(prob.Activist[Country==j,])), quantile(colMeans(prob.Activist[Country==j,]), c(0.025,0.975)))
)


Conventional.by.Country.Voteintention<-rbind(Conventional.by.Country.Voteintention,
c(unique(Country)[j],mean(colMeans(prob.Conventional[Country==j,])), quantile(colMeans(prob.Conventional[Country==j,]), c(0.025,0.975)))
)


Agitator.by.Country.Voteintention<-rbind(Agitator.by.Country.Voteintention,
c(unique(Country)[j],mean(colMeans(prob.Agitator[Country==j,])), quantile(colMeans(prob.Agitator[Country==j,]), c(0.025,0.975)))
)

}


save(Outsider.by.Country.Voteintention, file="Outsider.by.Country.Voteintention")
save(Conventional.by.Country.Voteintention, file="Conventional.by.Country.Voteintention")
save(Activist.by.Country.Voteintention, file="Activist.by.Country.Voteintention")
save(Agitator.by.Country.Voteintention, file="Agitator.by.Country.Voteintention")




ALPHA<-rbind(t(Results$alpha[,,1]), 
		t(Results$alpha[,,2]))


ALPHA.CONV<-rbind(t(Results$alpha_conv[,,1]), 
		t(Results$alpha_conv[,,2]))


ALPHA.UNCONV<-rbind(t(Results$alpha_unconv[,,1]), 
		t(Results$alpha_unconv[,,2]))

Alpha<-ALPHA[seq(1, nrow(BETA2), length.out=1000),]
Alpha.Conv<-ALPHA.CONV[seq(1, nrow(BETA2), length.out=1000),]
Alpha.Unconv<-ALPHA.UNCONV[seq(1, nrow(BETA2), length.out=1000),]


P.CONV<-rbind(t(Results$P_conv[,,1]), 
		t(Results$P_conv[,,2]))
P.Conv<-P.CONV[seq(1, nrow(BETA2), length.out=1000),]


P.UNCONV<-rbind(t(Results$P_unconv[,,1]), 
		t(Results$P_unconv[,,2]))

P.Unconv<-P.UNCONV[seq(1, nrow(BETA2), length.out=1000),]



C_unconv=rbinom(nrow(Y), 1, .5)+1
C.Conv<-matrix(0,nrow(Y), nrow(Alpha))
C.Unconv<-matrix(0,nrow(Y), nrow(Alpha))


for (s in 1:nrow(Alpha)){ 

pnum<-matrix(0,nrow(Y),2)

for (k in 1:2) {
lindividual<-matrix(0,nrow(Y),ncol(Y))
for (l in 1:ncol(Y)) {
mutmp=pnorm(Matrix.Country%*%t(matrix(Alpha[s,],ncol(Y), ncol(Matrix.Country)))[,l]+
(k-1)*((Matrix.Country%*%t(matrix(Alpha.Conv[s,],ncol(Y), ncol(Matrix.Country))))[,l])+
(C_unconv-1)*((Matrix.Country%*%t(matrix(Alpha.Unconv[s,],ncol(Y), ncol(Matrix.Country))))[,l]))
lindividual[,l]<-(mutmp^Y[,l])*((1-mutmp)^(1-Y[,l]))
}

pnum[,k]<-apply(lindividual,1,prod)*P.Conv[s,k]
}

p<-pnum/(apply(pnum,1,sum))
p[is.na(p)==1]<-rep(1/2,2)
C_conv<-rMultinom(p,1)

C.Conv[,s]<-C_conv




for (k in 1:2) {
lindividual<-matrix(0,nrow(Y),ncol(Y))
for (l in 1:ncol(Y)) {
mutmp=pnorm(Matrix.Country%*%t(matrix(Alpha[s,],ncol(Y), ncol(Matrix.Country)))[,l]+
(C_conv-1)*((Matrix.Country%*%t(matrix(Alpha.Conv[s,],ncol(Y), ncol(Matrix.Country))))[,l])+
(k-1)*((Matrix.Country%*%t(matrix(Alpha.Unconv[s,],ncol(Y), ncol(Matrix.Country))))[,l]))
lindividual[,l]<-(mutmp^Y[,l])*((1-mutmp)^(1-Y[,l]))
}

pnum[,k]<-apply(lindividual,1,prod)*P.Unconv[s,k]
}

p<-pnum/(apply(pnum,1,sum))
p[is.na(p)==1]<-rep(1/2,2)
C_unconv<-rMultinom(p,1)

C.Unconv[,s]<-C_unconv


}



CC<-rowMeans(C.Conv-1)
CU<-rowMeans(C.Unconv-1)

Probs.Dimensions.Voteintention<-cbind(CC, CU)
save(Probs.Dimensions.Voteintention, file="Probs.Dimensions.Voteintention")






rm(list=ls())

load("dataset_2012_wpartysupport.RData")

df<-subset(dataset, select=c("country.name", "ActMeetMun",
"country.name", "ActMeetMun", "ActContMun",  "ActContAut" ,                           
"ActMeetImp" , "ActSolveProb" ,  "ActMeetPty" ,  "ActContGvt",                            
"ActProtest" , "ActBlock" ,  "ActVoted",  "ActSign" ,                              
"ActShare" , "education" ,  "female", "age", "relative.incomeproxy" ,
"victim", "corrupt", #"spatial.distance" ,
"partisanship.incumbent", 
"compulsory.voting", "gdp.pc", "rule.of.lw", "enp", "education.spending"
#"social.spending"
))                       
df<-na.exclude(df)


Y <- cbind(df$ActMeetMun, df$ActContMun, df$ActContAut,
		 df$ActContGvt, df$ActSolveProb, 
		df$ActMeetImp, df$ActMeetPty, df$ActVoted, 
		df$ActShare, df$ActSign, df$ActProtest, df$ActBlock)
colnames(Y)<-c("ActMeetMun", "ActContMun", "ActContAut", "ActContGvt",
		"ActSolveProb", "ActMeetImp", "ActMeetPty", "ActVoted",
		"ActShare", "ActSign", "ActProtest", "ActBlock")

Country<-as.numeric(factor(df$country.name), levels=unique(df$country.name))
		

Matrix.Country<-matrix(0,nrow(Y),length(unique(Country)))           
for (i in 1:length(unique(Country))) Matrix.Country[Country==i,i]<-1

df$age.centered<-(df$age-mean(df$age))/(2*sd(df$age))
df$education.centered<-(df$education-mean(df$education))/(2*sd(df$education))


df$relative.incomeproxy.centered<-(df$relative.incomeproxy-mean(df$relative.incomeproxy))/(2*sd(df$relative.incomeproxy))




X<-cbind(1, df$age.centered, df$female, df$education.centered, 
	df$relative.incomeproxy.centered, 
	df$victim, df$corrupt, 
	df$partisanship.incumbent,
	(df$rule.of.lw-mean(df$rule.of.lw))/(2*sd(df$rule.of.lw)),
		df$compulsory.voting,
		(df$enp-mean(df$enp))/(2*sd(df$enp)),
		(df$gdp.pc-mean(df$gdp.pc))/(2*sd(df$gdp.pc)),
		(df$education.spending-mean(df$education.spending))/(2*sd(df$education.spending))
)





load("Results_PartisanshipModel")

## Converence Checks for Partisanship Model
Alpha<-mcmc.list(as.mcmc(t(Results$alpha[,,1])), 
		as.mcmc(t(Results$alpha[,,2])))
print(paste("Convergence Alpha:", gelman.diag(Alpha)[[2]]<1.2, sep=" "))


Alpha.Conv<-mcmc.list(as.mcmc(t(Results$alpha_conv[-seq(12,dim(Results$alpha_conv)[1],by=12),,1])), 
		as.mcmc(t(Results$alpha_conv[-seq(12,dim(Results$alpha_conv)[1],by=12),,2])))
print(paste("Convergence Alpha Conv:", gelman.diag(Alpha.Conv)[[2]]<1.2, sep=" "))


Alpha.Unconv<-mcmc.list(as.mcmc(t(Results$alpha_unconv[-seq(1,dim(Results$alpha_conv)[1],by=12),,1])), 
		as.mcmc(t(Results$alpha_unconv[-seq(1,dim(Results$alpha_conv)[1],by=12),,2])))
print(paste("Convergence Alpha Unconventional:", gelman.diag(Alpha.Unconv)[[2]]<1.2, sep=" "))



P.Conv<-mcmc.list(as.mcmc((Results$P_conv[2,,1])), 
		as.mcmc((Results$P_conv[2,,2])))
print(paste("Convergence Prob. Conventional:", gelman.diag(P.Conv)[[1]][,1]<1.2, sep=" "))


P.Unconv<-mcmc.list(as.mcmc((Results$P_unconv[2,,1])), 
		as.mcmc((Results$P_unconv[2,,2])))
print(paste("Convergence Prob. Unconventional", gelman.diag(P.Unconv)[[1]][,1]<1.2, sep=" "))


Beta2<-mcmc.list(as.mcmc(t(Results$beta2[,,1])),
	as.mcmc(t(Results$beta2[,,2])))
Beta3<-mcmc.list(as.mcmc(t(Results$beta3[,,1])),
	as.mcmc(t(Results$beta3[,,2])))
Beta4<-mcmc.list(as.mcmc(t(Results$beta4[,,1])),
	as.mcmc(t(Results$beta4[,,2])))
print(paste("Convergence Regression coefficients:", gelman.diag(Beta2)[[2]]<1.2 & gelman.diag(Beta3)[[2]]<1.2 & gelman.diag(Beta4)[[2]]<1.2, sep=" "))



BETA2<-rbind((t(Results$beta2[,,1])),
	(t(Results$beta2[,,2])))
BETA3<-rbind((t(Results$beta3[,,1])),
	(t(Results$beta3[,,2])))
BETA4<-rbind((t(Results$beta4[,,1])),
	(t(Results$beta4[,,2])))
NU<-rbind((t(Results$nu[,,1])),
	(t(Results$nu[,,2])))
NU2<-NU[, 1:length(unique(Country))]
NU3<-NU[, (1+length(unique(Country))): (2*length(unique(Country)))]
NU4<-NU[, (1+2*length(unique(Country))): (3*length(unique(Country)))]



Beta.Agitator<-BETA2[seq(1, nrow(BETA2), length.out=1000),]
Beta.Conventional<-BETA3[seq(1, nrow(BETA3), length.out=1000),]
Beta.Activist<-BETA4[seq(1, nrow(BETA4), length.out=1000),]
Nu.Agitator<-NU2[seq(1, nrow(NU2), length.out=1000),]
Nu.Conventional<-NU3[seq(1, nrow(NU3), length.out=1000),]
Nu.Activist<-NU4[seq(1, nrow(NU4), length.out=1000),]



prob.Agitator<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X%*%Beta.Agitator[s,]+Nu.Agitator[s,Country])/(1+exp(X%*%Beta.Agitator[s,]+Nu.Agitator[s,Country])+
			exp(X%*%Beta.Conventional[s,]+Nu.Conventional[s,Country])+exp(X%*%Beta.Activist[s,]+Nu.Activist[s,Country])))

prob.Conventional<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X%*%Beta.Conventional[s,]+Nu.Conventional[s,Country])/(1+exp(X%*%Beta.Agitator[s,]+Nu.Agitator[s,Country])+
			exp(X%*%Beta.Conventional[s,]+Nu.Conventional[s,Country])+exp(X%*%Beta.Activist[s,]+Nu.Activist[s,Country])))

prob.Activist<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X%*%Beta.Activist[s,]+Nu.Activist[s,Country])/(1+exp(X%*%Beta.Agitator[s,]+Nu.Agitator[s,Country])+
			exp(X%*%Beta.Conventional[s,]+Nu.Conventional[s,Country])+exp(X%*%Beta.Activist[s,]+Nu.Activist[s,Country])))
prob.Outsider<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X%*%Beta.Agitator[s,]+Nu.Agitator[s,Country])+
			exp(X%*%Beta.Conventional[s,]+Nu.Conventional[s,Country])+exp(X%*%Beta.Activist[s,]+Nu.Activist[s,Country])))




Probs.Partisanship<-cbind(Country, rowMeans(prob.Outsider), rowMeans(prob.Agitator), rowMeans(prob.Conventional), rowMeans(prob.Activist))



Posterior.Probs.Partisanship<-cbind(colMeans(prob.Outsider), colMeans(prob.Agitator), colMeans(prob.Conventional), colMeans(prob.Activist))

save(Posterior.Probs.Partisanship, file="Posterior.Probs.Partisanship")
save(Probs.Partisanship, file="Probs.Partisanship")


Outsider.by.Country.Partisanship<-c()
Activist.by.Country.Partisanship<-c()
Conventional.by.Country.Partisanship<-c()
Agitator.by.Country.Partisanship<-c()

for (j in 1:length(unique(Country))) {

Outsider.by.Country.Partisanship<-rbind(Outsider.by.Country.Partisanship,
c(unique(Country)[j],mean(colMeans(prob.Outsider[Country==j,])), quantile(colMeans(prob.Outsider[Country==j,]), c(0.025,0.975)))
)


Activist.by.Country.Partisanship<-rbind(Activist.by.Country.Partisanship,
c(unique(Country)[j],mean(colMeans(prob.Activist[Country==j,])), quantile(colMeans(prob.Activist[Country==j,]), c(0.025,0.975)))
)


Conventional.by.Country.Partisanship<-rbind(Conventional.by.Country.Partisanship,
c(unique(Country)[j],mean(colMeans(prob.Conventional[Country==j,])), quantile(colMeans(prob.Conventional[Country==j,]), c(0.025,0.975)))
)


Agitator.by.Country.Partisanship<-rbind(Agitator.by.Country.Partisanship,
c(unique(Country)[j],mean(colMeans(prob.Agitator[Country==j,])), quantile(colMeans(prob.Agitator[Country==j,]), c(0.025,0.975)))
)

}


save(Outsider.by.Country.Partisanship, file="Outsider.by.Country.Partisanship")
save(Conventional.by.Country.Partisanship, file="Conventional.by.Country.Partisanship")
save(Activist.by.Country.Partisanship, file="Activist.by.Country.Partisanship")
save(Agitator.by.Country.Partisanship, file="Agitator.by.Country.Partisanship")





ALPHA<-rbind(t(Results$alpha[,,1]), 
		t(Results$alpha[,,2]))


ALPHA.CONV<-rbind(t(Results$alpha_conv[,,1]), 
		t(Results$alpha_conv[,,2]))


ALPHA.UNCONV<-rbind(t(Results$alpha_unconv[,,1]), 
		t(Results$alpha_unconv[,,2]))

Alpha<-ALPHA[seq(1, nrow(BETA2), length.out=1000),]
Alpha.Conv<-ALPHA.CONV[seq(1, nrow(BETA2), length.out=1000),]
Alpha.Unconv<-ALPHA.UNCONV[seq(1, nrow(BETA2), length.out=1000),]


P.CONV<-rbind(t(Results$P_conv[,,1]), 
		t(Results$P_conv[,,2]))
P.Conv<-P.CONV[seq(1, nrow(BETA2), length.out=1000),]


P.UNCONV<-rbind(t(Results$P_unconv[,,1]), 
		t(Results$P_unconv[,,2]))

P.Unconv<-P.UNCONV[seq(1, nrow(BETA2), length.out=1000),]



C_unconv=rbinom(nrow(Y), 1, .5)+1
C.Conv<-matrix(0,nrow(Y), nrow(Alpha))
C.Unconv<-matrix(0,nrow(Y), nrow(Alpha))


for (s in 1:nrow(Alpha)){ 

pnum<-matrix(0,nrow(Y),2)

for (k in 1:2) {
lindividual<-matrix(0,nrow(Y),ncol(Y))
for (l in 1:ncol(Y)) {
mutmp=pnorm(Matrix.Country%*%t(matrix(Alpha[s,],ncol(Y), ncol(Matrix.Country)))[,l]+
(k-1)*((Matrix.Country%*%t(matrix(Alpha.Conv[s,],ncol(Y), ncol(Matrix.Country))))[,l])+
(C_unconv-1)*((Matrix.Country%*%t(matrix(Alpha.Unconv[s,],ncol(Y), ncol(Matrix.Country))))[,l]))
lindividual[,l]<-(mutmp^Y[,l])*((1-mutmp)^(1-Y[,l]))
}

pnum[,k]<-apply(lindividual,1,prod)*P.Conv[s,k]
}

p<-pnum/(apply(pnum,1,sum))
p[is.na(p)==1]<-rep(1/2,2)


C_conv<-rMultinom(p,1)

C.Conv[,s]<-C_conv




for (k in 1:2) {
lindividual<-matrix(0,nrow(Y),ncol(Y))
for (l in 1:ncol(Y)) {
mutmp=pnorm(Matrix.Country%*%t(matrix(Alpha[s,],ncol(Y), ncol(Matrix.Country)))[,l]+
(C_conv-1)*((Matrix.Country%*%t(matrix(Alpha.Conv[s,],ncol(Y), ncol(Matrix.Country))))[,l])+
(k-1)*((Matrix.Country%*%t(matrix(Alpha.Unconv[s,],ncol(Y), ncol(Matrix.Country))))[,l]))
lindividual[,l]<-(mutmp^Y[,l])*((1-mutmp)^(1-Y[,l]))
}

pnum[,k]<-apply(lindividual,1,prod)*P.Unconv[s,k]
}

p<-pnum/(apply(pnum,1,sum))
p[is.na(p)==1]<-rep(1/2,2)


C_unconv<-rMultinom(p,1)

C.Unconv[,s]<-C_unconv


}


CC<-rowMeans(C.Conv-1)
CU<-rowMeans(C.Unconv-1)

Probs.Dimensions.Partisanship<-cbind(CC, CU)
save(Probs.Dimensions.Partisanship, file="Probs.Dimensions.Partisanship")


rm(list=ls())

load("Probs.Dimensions.Voteintention")

prob.conv.type <-Probs.Dimensions.Voteintention[,1]
prob.unconv.type<-Probs.Dimensions.Voteintention[,2]

load("Posterior.Probs.Voteintention")



per.outsiders <- round(colMeans(Posterior.Probs.Voteintention)[1], digits=2)
if (nchar(per.outsiders)<4) {
while (nchar(per.outsiders)<4) {
per.outsiders<-paste(per.outsiders, "0", sep="")
} 
}


per.agitators<- round(colMeans(Posterior.Probs.Voteintention)[2], digits=2)
if (nchar(per.agitators)<4) {
while (nchar(per.agitators)<4) {
per.agitators<-paste(per.agitators, "0", sep="")
} 
}

per.activists<- round(colMeans(Posterior.Probs.Voteintention)[4], digits=2)
if (nchar(per.activists)<4) {
while (nchar(per.activists)<4) {
per.activists<-paste(per.activists, "0", sep="")
} 
}


per.conventionals <-max(round(colMeans(Posterior.Probs.Voteintention)[3], digits=2), 1-round(colMeans(Posterior.Probs.Voteintention)[1], digits=2)-round(colMeans(Posterior.Probs.Voteintention)[2], digits=2)-round(colMeans(Posterior.Probs.Voteintention)[4], digits=2))
if (nchar(per.conventionals)<4) {
while (nchar(per.conventionals)<4) {
per.conventionals<-paste(per.conventionals, "0", sep="")
} 
}


Points<-cbind(prob.conv.type, prob.unconv.type)
Points<-data.frame(Points)
names(Points)<-c("x", "y")


p.1<-ggplot(Points, aes(x=x, y=y))+
geom_point(colour = rgb(0.5,0.5,0.5,0.10), size = 0.5)+theme_bw()+
xlab("Probability of high conventional type")+
ylab("Probability of high 
unconventional type")+ ggtitle("Prospective Vote for Incument")+
theme(plot.title = element_text(hjust = 0.5, size=12, face="bold"))+
theme(axis.text.x = element_text(color="black", size=11))+
	theme(axis.text.x = element_text(color="black", size=11))+
 theme(axis.title.y = element_text(size=13))+
 theme(axis.title.x = element_text(size=13))+
 theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))+
 theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))+
 geom_vline(xintercept = 0.5, linetype="dashed")+
 geom_hline(yintercept = 0.5, linetype="dashed")+
annotate(geom="text", x=0.25, y=0.25, label=paste("Outsiders",
per.outsiders, sep="\n"), color="black", size=5)+
annotate(geom="text", x=0.75, y=0.25, label=paste("Conventionals",
per.conventionals, sep="\n"), color="black", size=5)+
annotate(geom="text", x=0.25, y=0.75, label=paste("Agitators",
per.agitators, sep="\n"), color="black", size=5)+
annotate(geom="text", x=0.75, y=0.75, label=paste("Activists",
per.activists, sep="\n"), color="black", size=5)+
theme(plot.margin=unit(c(2,1,1,1), "lines"))



load("Probs.Dimensions.Partisanship")

prob.conv.type <-Probs.Dimensions.Partisanship[,1]
prob.unconv.type<-Probs.Dimensions.Partisanship[,2]


load("Posterior.Probs.Partisanship")

per.outsiders <- round(colMeans(Posterior.Probs.Partisanship)[1], digits=2)
if (nchar(per.outsiders)<4) {
while (nchar(per.outsiders)<4) {
per.outsiders<-paste(per.outsiders, "0", sep="")
} 
}


per.agitators<- round(colMeans(Posterior.Probs.Partisanship)[2], digits=2)
if (nchar(per.agitators)<4) {
while (nchar(per.agitators)<4) {
per.agitators<-paste(per.agitators, "0", sep="")
} 
}



per.activists<- round(colMeans(Posterior.Probs.Partisanship)[4], digits=2)
if (nchar(per.activists)<4) {
while (nchar(per.activists)<4) {
per.activists<-paste(per.activists, "0", sep="")
} 
}

per.conventionals <-max(round(colMeans(Posterior.Probs.Partisanship)[3], digits=2), 1-round(colMeans(Posterior.Probs.Partisanship)[1], digits=2)-round(colMeans(Posterior.Probs.Partisanship)[2], digits=2)-round(colMeans(Posterior.Probs.Partisanship)[4], digits=2))
if (nchar(per.conventionals)<4) {
while (nchar(per.conventionals)<4) {
per.conventionals<-paste(per.conventionals, "0", sep="")
} 
}




Points<-cbind(prob.conv.type, prob.unconv.type)
Points<-data.frame(Points)
names(Points)<-c("x", "y")



p.2<-ggplot(Points, aes(x=x, y=y))+
geom_point(colour = rgb(0.5,0.5,0.5,0.10), size = 0.5)+theme_bw()+
xlab("Probability of high conventional type")+
ylab("Probability of high 
unconventional type")+ggtitle("Close to Incumbent Party")+
theme(plot.title = element_text(hjust = 0.5, size=12, face="bold"))+
theme(axis.text.x = element_text(color="black", size=11))+
	theme(axis.text.x = element_text(color="black", size=11))+
 theme(axis.title.y = element_text(size=13))+
 theme(axis.title.x = element_text(size=13))+
 theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))+
 theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))+
 geom_vline(xintercept = 0.5, linetype="dashed")+
 geom_hline(yintercept = 0.5, linetype="dashed")+
annotate(geom="text", x=0.25, y=0.25, label=paste("Outsiders",
per.outsiders, sep="\n"), color="black", size=5)+
annotate(geom="text", x=0.75, y=0.25, label=paste("Conventionals",
per.conventionals, sep="\n"), color="black", size=5)+
annotate(geom="text", x=0.25, y=0.75, label=paste("Agitators",
per.agitators, sep="\n"), color="black", size=5)+
annotate(geom="text", x=0.75, y=0.75, label=paste("Activists",
per.activists, sep="\n"), color="black", size=5)+
theme(plot.margin=unit(c(2,1,1,1), "lines"))





# Multiple plot function
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



pdf(file = "Figure.A.26.pdf")
grid.draw(multiplot(p.1, p.2, cols=1 ))
dev.off()



################################################# Figure A.27 ##################################################################################

rm(list=ls())

load("Posterior.Probs.Voteintention")


Types<-cbind(c("Outsider", "Agitator", "Conventional", "Activist"), colMeans(Posterior.Probs.Voteintention), t(apply(Posterior.Probs.Voteintention, 2, quantile, c(0.025,0.975))))
Types<-data.frame(Types)
names(Types)<-c("Type", "Mean", "Low", "High")

for (k in 2:ncol(Types)) {
Types[,k]<- as.numeric(levels(Types[,k]))[Types[,k]] 
}


Types$Type<- factor(Types$Type, 
	levels = c("Outsider", "Agitator", "Conventional",
		"Activist"))

p.1<- ggplot(Types, aes(x=Type, y=Mean)) + 
  geom_point(size=2, fill="black") + ylab("Probability of 
type assignment")+
	xlab("")+
  geom_errorbar(aes(ymin=Low, ymax=High), width=.1,
                 position=position_dodge(0.05))+
	theme_bw()+ ylim(c(0,0.82))+
	theme(axis.text.x = element_text(color="black", size=14))+
	theme(axis.text.x = element_text(color="black", size=11))+
 theme(axis.title.y = element_text(size=13))+
theme(plot.margin=unit(c(1,1,2,1), "lines"))+
ggtitle("Prospective Vote for Incumbent")+
theme(plot.title = element_text(hjust = 0.5, size=12, face="bold"))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))


load("Posterior.Probs.Partisanship")

Types<-cbind(c("Outsider", "Agitator", "Conventional", "Activist"), colMeans(Posterior.Probs.Partisanship), t(apply(Posterior.Probs.Partisanship, 2, quantile, c(0.025,0.975))))
Types<-data.frame(Types)
names(Types)<-c("Type", "Mean", "Low", "High")

for (k in 2:ncol(Types)) {
Types[,k]<- as.numeric(levels(Types[,k]))[Types[,k]] 
}


Types$Type<- factor(Types$Type, 
	levels = c("Outsider", "Agitator", "Conventional",
		"Activist"))

p.2<- ggplot(Types, aes(x=Type, y=Mean)) + 
  geom_point(size=2, fill="black") + ylab("Probability of 
type assignment")+
	xlab("")+
  geom_errorbar(aes(ymin=Low, ymax=High), width=.1,
                 position=position_dodge(0.05))+
	theme_bw()+ ylim(c(0,0.82))+
	theme(axis.text.x = element_text(color="black", size=14))+
	theme(axis.text.x = element_text(color="black", size=11))+
 theme(axis.title.y = element_text(size=13))+
theme(plot.margin=unit(c(2,1,1,1), "lines"))+
ggtitle("Close to Incumbent Party")+
theme(plot.title = element_text(hjust = 0.5, size=12, face="bold"))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))

# Multiple plot function
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



pdf(file = "Figure.A.27.pdf")
grid.draw(multiplot(p.1, p.2, cols=1 ))
dev.off()



################################################# Figure A.28 ##################################################################################

rm(list=ls())

load("Outsider.by.Country.Voteintention")
load("Conventional.by.Country.Voteintention")
load("Activist.by.Country.Voteintention")
load("Agitator.by.Country.Voteintention")



df.outsiders<-Outsider.by.Country.Voteintention
df.conventionals<-Conventional.by.Country.Voteintention
df.activists<-Activist.by.Country.Voteintention
df.agitators<-Agitator.by.Country.Voteintention


df.outsiders<-data.frame(cbind("Outsiders", df.outsiders))
names(df.outsiders)<-c("Type", "Country", "Mean", "Low", "High")
 
df.agitators<-data.frame(cbind("Agitators", df.agitators))
names(df.agitators)<-c("Type", "Country", "Mean", "Low", "High")
 
df.conventionals<-data.frame(cbind("Conventionals", df.conventionals))
names(df.conventionals)<-c("Type", "Country", "Mean", "Low", "High")

df.activists<-data.frame(cbind("Activists", df.activists))
names(df.activists)<-c("Type", "Country", "Mean", "Low", "High")
 

df<-rbind(df.outsiders,df.agitators,	 df.conventionals, df.activists)

df$abbr<-NA
df$abbr[df$Country==1]<-"ARG"
df$abbr[df$Country==2]<-"BLZ"
df$abbr[df$Country==3]<-"BRA"
df$abbr[df$Country==4]<-"CHL"
df$abbr[df$Country==5]<-"COL"
df$abbr[df$Country==6]<-"CRI"
df$abbr[df$Country==7]<-"DOM"
df$abbr[df$Country==8]<-"ECU"
df$abbr[df$Country==9]<-"ESV"
df$abbr[df$Country==10]<-"GTM"
df$abbr[df$Country==11]<-"GUY"
df$abbr[df$Country==12]<-"HTI"
df$abbr[df$Country==13]<-"HND"
df$abbr[df$Country==14]<-"JAM"
df$abbr[df$Country==15]<-"MEX"
df$abbr[df$Country==16]<-"NIC"
df$abbr[df$Country==17]<-"PAN"
df$abbr[df$Country==18]<-"PAR"
df$abbr[df$Country==19]<-"PER"
df$abbr[df$Country==20]<-"SUR"
df$abbr[df$Country==21]<-"TTO"
df$abbr[df$Country==22]<-"URU"
df$abbr[df$Country==23]<-"VEN"



Outsiders.Vote<-df[df$Type=="Outsiders",-c(1,2)]
Agitators.Vote<-df[df$Type=="Agitators",-c(1,2)]
Conventionals.Vote<-df[df$Type=="Conventionals",-c(1,2)]
Activists.Vote<-df[df$Type=="Activists",-c(1,2)]



load("Outsider.by.Country.Partisanship")
load("Conventional.by.Country.Partisanship")
load("Activist.by.Country.Partisanship")
load("Agitator.by.Country.Partisanship")


df.outsiders<-Outsider.by.Country.Partisanship
df.conventionals<-Conventional.by.Country.Partisanship
df.activists<-Activist.by.Country.Partisanship
df.agitators<-Agitator.by.Country.Partisanship

df.outsiders<-data.frame(cbind("Outsiders", df.outsiders))
names(df.outsiders)<-c("Type", "Country", "Mean", "Low", "High")
 
df.agitators<-data.frame(cbind("Agitators", df.agitators))
names(df.agitators)<-c("Type", "Country", "Mean", "Low", "High")
 
df.conventionals<-data.frame(cbind("Conventionals", df.conventionals))
names(df.conventionals)<-c("Type", "Country", "Mean", "Low", "High")

df.activists<-data.frame(cbind("Activists", df.activists))
names(df.activists)<-c("Type", "Country", "Mean", "Low", "High")
 


df<-rbind(df.outsiders,	df.agitators,
	 df.conventionals,
	 df.activists)

df$abbr<-NA
df$abbr[df$Country==1]<-"ARG"
df$abbr[df$Country==2]<-"BLZ"
df$abbr[df$Country==3]<-"BRA"
df$abbr[df$Country==4]<-"CHL"
df$abbr[df$Country==5]<-"COL"
df$abbr[df$Country==6]<-"CRI"
df$abbr[df$Country==7]<-"DOM"
df$abbr[df$Country==8]<-"ECU"
df$abbr[df$Country==9]<-"ESV"
df$abbr[df$Country==10]<-"GTM"
df$abbr[df$Country==11]<-"GUY"
df$abbr[df$Country==12]<-"HTI"
df$abbr[df$Country==13]<-"HND"
df$abbr[df$Country==14]<-"JAM"
df$abbr[df$Country==15]<-"MEX"
df$abbr[df$Country==16]<-"NIC"
df$abbr[df$Country==17]<-"PAN"
df$abbr[df$Country==18]<-"PAR"
df$abbr[df$Country==19]<-"PER"
df$abbr[df$Country==20]<-"SUR"
df$abbr[df$Country==21]<-"TTO"
df$abbr[df$Country==22]<-"URU"
df$abbr[df$Country==23]<-"VEN"



Outsiders.Partisan<-df[df$Type=="Outsiders",-c(1,2)]

Agitators.Partisan<-df[df$Type=="Agitators",-c(1,2)]

Conventionals.Partisan<-df[df$Type=="Conventionals",-c(1,2)]

Activists.Partisan<-df[df$Type=="Activists",-c(1,2)]



Outsiders.Partisan<-cbind(Outsiders.Partisan, "Close to Incumbent Party")
names(Outsiders.Partisan)[dim(Outsiders.Partisan)[2]]<-"Variable"

Outsiders.Vote<-cbind(Outsiders.Vote, "Prospective Vote for Incument")
names(Outsiders.Vote)[dim(Outsiders.Vote)[2]]<-"Variable"

Outsiders<-rbind(Outsiders.Vote, Outsiders.Partisan)


Activists.Partisan<-cbind(Activists.Partisan, "Close to Incumbent Party")
names(Activists.Partisan)[dim(Activists.Partisan)[2]]<-"Variable"

Activists.Vote<-cbind(Activists.Vote, "Prospective Vote for Incument")
names(Activists.Vote)[dim(Activists.Vote)[2]]<-"Variable"

Activists<-rbind(Activists.Vote, Activists.Partisan)


Agitators.Partisan<-cbind(Agitators.Partisan, "Close to Incumbent Party")
names(Agitators.Partisan)[dim(Agitators.Partisan)[2]]<-"Variable"

Agitators.Vote<-cbind(Agitators.Vote, "Prospective Vote for Incument")
names(Agitators.Vote)[dim(Agitators.Vote)[2]]<-"Variable"

Agitators<-rbind(Agitators.Vote, Agitators.Partisan)

Conventionals.Partisan<-cbind(Conventionals.Partisan, "Close to Incumbent Party")
names(Conventionals.Partisan)[dim(Conventionals.Partisan)[2]]<-"Variable"

Conventionals.Vote<-cbind(Conventionals.Vote, "Prospective Vote for Incument")
names(Conventionals.Vote)[dim(Conventionals.Vote)[2]]<-"Variable"

Conventionals<-rbind(Conventionals.Vote, Conventionals.Partisan)


for (k in 1:3) {

Outsiders[,k]<-as.numeric(levels(Outsiders[,k]))[Outsiders[,k]]
Conventionals[,k]<-as.numeric(levels(Conventionals[,k]))[Conventionals[,k]]
Activists[,k]<-as.numeric(levels(Activists[,k]))[Activists[,k]]
Agitators[,k]<-as.numeric(levels(Agitators[,k]))[Agitators[,k]]
}






p.1<-ggplot(Outsiders,  aes(as.factor(abbr), Mean,group=Variable, color=Variable)) +
 geom_point(position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=Low, ymax=High), width=.1,
                 position=position_dodge(0.5))+
	ylab("")+xlab("")+ggtitle("Outsiders")+theme_bw()+
	theme(plot.margin=unit(c(0.5,1,0,0), "lines"))+
scale_y_continuous(breaks=c(0.68, 0.72, 0.76))+
theme(legend.position="none")+
theme(axis.text.x=element_text(size=6, vjust=0.1))+
scale_color_manual(values=c("black", "gray70"))+theme(plot.title = element_text(hjust = 0))


p.2<-ggplot(Agitators,  aes(as.factor(abbr), Mean,group=Variable, color=Variable)) +
 geom_point(position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=Low, ymax=High), width=.1,
                 position=position_dodge(0.5))+
	ylab("")+xlab("")+ggtitle("Agitators")+theme_bw()+
	theme(plot.margin=unit(c(0.5,1,0,0), "lines"))+
theme(legend.position="none")+
theme(axis.text.x=element_text(size=6, vjust=0.1))+
scale_color_manual(values=c("black", "gray70"))+theme(plot.title = element_text(hjust = 0))

p.3<-ggplot(Conventionals,  aes(as.factor(abbr), Mean,group=Variable, color=Variable)) +
 geom_point(position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=Low, ymax=High), width=.1,
                 position=position_dodge(0.5))+
	ylab("")+xlab("")+ggtitle("Conventionals")+theme_bw()+
	theme(plot.margin=unit(c(0.5,1,0,0), "lines"))+
theme(legend.position="none")+
theme(axis.text.x=element_text(size=6, vjust=0.1))+
scale_color_manual(values=c("black", "gray70"))+theme(plot.title = element_text(hjust = 0))

p.4<-ggplot(Activists,  aes(as.factor(abbr), Mean,group=Variable, color=Variable)) +
 geom_point(position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=Low, ymax=High), width=.1,
                 position=position_dodge(0.5))+
	ylab("")+xlab("")+ggtitle("Activists")+theme_bw()+
	theme(plot.margin=unit(c(0.5,1,0,0), "lines"))+
theme(legend.position="none")+
theme(axis.text.x=element_text(size=6, vjust=0.1))+
scale_color_manual(values=c("black", "gray70"))+
theme(legend.title = element_blank())+theme(plot.title = element_text(hjust = 0))


# Multiple plot function
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



pdf(file = "Figure.A.28.pdf")
grid.draw(multiplot(p.1, p.2, p.3, p.4, cols=1 ))
dev.off()




################################################# SECTION A.3.5 ##################################################################################
################################################# Table A.10 ##################################################################################

rm(list=ls())
load("dataset_2010to2018.RData")

df<-subset(dataset, select=c("country.name", "country.abbreviation", "wave",
"ActMeetMun", "ActMeetImp" , "ActMeetPty" , "ActVoted",  "ActProtest" ,
 "education" ,  "female", "age", "relative.incomeproxy" ,
"victim", "corrupt", "spatial.distance" ,
"compulsory.voting", "gdp.pc", "rule.of.lw", "enp", "social.spending"
))                       
df<-na.exclude(df)


Table.A.10<-table(df$country.name, df$wave)
Table.A.10<-cbind(Table.A.10,rowSums(Table.A.10))
Table.A.10<-rbind(Table.A.10, colSums(Table.A.10))
rownames(Table.A.10)[nrow(Table.A.10)]<-"Observations"
Table.A.10<-cbind(rownames(Table.A.10),c("DOM", "ESV", "HND", "MEX", "NIC", "PAR", "PER", ""),
    Table.A.10)
rownames(Table.A.10)<-c()
colnames(Table.A.10)[1]<-"Country"
colnames(Table.A.10)[2]<-"Abbreviation"
colnames(Table.A.10)[ncol(Table.A.10)]<-"Total"

print(Table.A.10)

g <- tableGrob(Table.A.10, rows = NULL)
g <- gtable_add_grob(g,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g), l = 1, r = ncol(g))
g <- gtable_add_grob(g,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 1, l = 1, r = ncol(g))



pdf(file = "Table.A.10.pdf")
grid.draw(g)
dev.off()
rm(g)



################################################# Table A.11 ##################################################################################

rm(list=ls())
load("Results_Wave2010")
Results.2010<-Results


## Convergence Checks for Wave 2010 Model
Alpha<-mcmc.list(as.mcmc(t(Results.2010$alpha[,,1])), 
		as.mcmc(t(Results.2010$alpha[,,2])),
		as.mcmc(t(Results.2010$alpha[,,3])))
print(paste("Convergence Alpha:", gelman.diag(Alpha)[[2]]<1.2, sep=" "))


Alpha.Conv<-mcmc.list(as.mcmc(t(Results.2010$alpha_conv[-seq(5,dim(Results.2010$alpha_conv)[1],by=5),,1])), 
		as.mcmc(t(Results.2010$alpha_conv[-seq(5,dim(Results.2010$alpha_conv)[1],by=5),,2])),
		as.mcmc(t(Results.2010$alpha_conv[-seq(5,dim(Results.2010$alpha_conv)[1],by=5),,3])))
print(paste("Convergence Alpha Conv:", gelman.diag(Alpha.Conv)[[2]]<1.2, sep=" "))


Alpha.Unconv<-mcmc.list(as.mcmc(t(Results.2010$alpha_unconv[-seq(1,dim(Results.2010$alpha_conv)[1],by=5),,1])), 
		as.mcmc(t(Results.2010$alpha_unconv[-seq(1,dim(Results.2010$alpha_conv)[1],by=5),,2])),
		as.mcmc(t(Results.2010$alpha_unconv[-seq(1,dim(Results.2010$alpha_conv)[1],by=5),,3])))
print(paste("Convergence Alpha Unconventional:", gelman.diag(Alpha.Unconv)[[2]]<1.2, sep=" "))


P.Conv<-mcmc.list(as.mcmc((Results.2010$P_conv[2,,1])), 
		as.mcmc((Results.2010$P_conv[2,,2])),
		as.mcmc((Results.2010$P_conv[2,,3])))
print(paste("Convergence Prob. Conventional:", gelman.diag(P.Conv)[[1]][,1]<1.2, sep=" "))

P.Unconv<-mcmc.list(as.mcmc((Results.2010$P_unconv[2,,1])), 
		as.mcmc((Results.2010$P_unconv[2,,2])),
		as.mcmc((Results.2010$P_unconv[2,,3])))
print(paste("Convergence Prob. Unconventional", gelman.diag(P.Unconv)[[1]][,1]<1.2, sep=" "))


Beta2<-mcmc.list(as.mcmc(t(Results.2010$beta2[,,1])),
	as.mcmc(t(Results.2010$beta2[,,2])),
	as.mcmc(t(Results.2010$beta2[,,3])))

Beta3<-mcmc.list(as.mcmc(t(Results.2010$beta3[,,1])),
	as.mcmc(t(Results.2010$beta3[,,2])),
	as.mcmc(t(Results.2010$beta3[,,3])))

Beta4<-mcmc.list(as.mcmc(t(Results.2010$beta4[,,1])),
	as.mcmc(t(Results.2010$beta4[,,2])),
	as.mcmc(t(Results.2010$beta4[,,3])))

print(paste("Convergence Regression coefficients:", gelman.diag(Beta2)[[2]]<1.2 & gelman.diag(Beta3)[[2]]<1.2 & gelman.diag(Beta4)[[2]]<1.2, sep=" "))



ALPHA.CONV.2010<-rbind(t(Results.2010$alpha_conv[,,1]), 
		t(Results.2010$alpha_conv[,,2]),
		t(Results.2010$alpha_conv[,,3]))


Alpha.conv.averages.2010<-matrix(0, 5,4)
rownames(Alpha.conv.averages.2010)<-NULL
colnames(Alpha.conv.averages.2010)<-NULL

Alpha.conv.averages.2010[,1]<-c("Municipal meeting", "Improvements meeting", "Party meeting", "Voting", "Protesting")
Alpha.conv.averages.2010[,2]<-"Conventional"



for (j in 1:5) {
m<-round(mean(c(ALPHA.CONV.2010[, seq(j, ncol(ALPHA.CONV.2010), by=5)])), digits=3)
q.l<-round(quantile(c(ALPHA.CONV.2010[, seq(j, ncol(ALPHA.CONV.2010), by=5)]), c(0.025,0.975))[1], digits=3)
q.h<-round(quantile(c(ALPHA.CONV.2010[, seq(j, ncol(ALPHA.CONV.2010), by=5)]), c(0.025,0.975))[2], digits=3)


if (nchar(m)<5 & grepl("\\.", m)==FALSE) {
m<-paste(m,".", sep="")
while (nchar(m)<5) {
m<-paste(m, "0", sep="")
} 
} else if (nchar(m)<5 & grepl("\\.", m)==TRUE) {
while (nchar(m)<5) {
m<-paste(m, "0", sep="")
} 
}


if (nchar(q.l)<5 & grepl("\\.", q.l)==FALSE) {
q.l<-paste(q.l,".", sep="")
while (nchar(q.l)<5) {
q.l<-paste(q.l, "0", sep="")
} 
} else if (nchar(q.l)<5 & grepl("\\.", q.l)==TRUE) {
while (nchar(q.l)<5) {
q.l<-paste(q.l, "0", sep="")
} 
}

if (nchar(q.h)<5 & grepl("\\.", q.h)==FALSE) {
q.h<-paste(q.h,".", sep="")
while (nchar(q.h)<5) {
q.h<-paste(q.h, "0", sep="")
} 
} else if (nchar(q.h)<5 & grepl("\\.", q.h)==TRUE) {
while (nchar(q.h)<5) {
q.h<-paste(q.h, "0", sep="")
} 
}



Alpha.conv.averages.2010[j,3:4]<-c(m, paste(paste("(", q.l, ",", sep=""), paste(q.h,")", sep=""),sep=""))
}


ALPHA.UNCONV.2010<-rbind(t(Results.2010$alpha_unconv[,,1]), 
		t(Results.2010$alpha_unconv[,,2]),
		t(Results.2010$alpha_unconv[,,3]))

Alpha.unconv.averages.2010<-matrix(0, 5,4)
Alpha.unconv.averages.2010[,1]<-c("Municipal meeting", "Improvements meeting", "Party meeting", "Voting", "Protesting")
Alpha.unconv.averages.2010[,2]<-"Unconventional"


for (j in 1:5) {
m<-round(mean(c(ALPHA.UNCONV.2010[, seq(j, ncol(ALPHA.UNCONV.2010), by=5)])), digits=3)
q.l<-round(quantile(c(ALPHA.UNCONV.2010[, seq(j, ncol(ALPHA.UNCONV.2010), by=5)]), c(0.025,0.975))[1], digits=3)
q.h<-round(quantile(c(ALPHA.UNCONV.2010[, seq(j, ncol(ALPHA.UNCONV.2010), by=5)]), c(0.025,0.975))[2], digits=3)


if (nchar(m)<5 & grepl("\\.", m)==FALSE) {
m<-paste(m,".", sep="")
while (nchar(m)<5) {
m<-paste(m, "0", sep="")
} 
} else if (nchar(m)<5 & grepl("\\.", m)==TRUE) {
while (nchar(m)<5) {
m<-paste(m, "0", sep="")
} 
}


if (nchar(q.l)<5 & grepl("\\.", q.l)==FALSE) {
q.l<-paste(q.l,".", sep="")
while (nchar(q.l)<5) {
q.l<-paste(q.l, "0", sep="")
} 
} else if (nchar(q.l)<5 & grepl("\\.", q.l)==TRUE) {
while (nchar(q.l)<5) {
q.l<-paste(q.l, "0", sep="")
} 
}

if (nchar(q.h)<5 & grepl("\\.", q.h)==FALSE) {
q.h<-paste(q.h,".", sep="")
while (nchar(q.h)<5) {
q.h<-paste(q.h, "0", sep="")
} 
} else if (nchar(q.h)<5 & grepl("\\.", q.h)==TRUE) {
while (nchar(q.h)<5) {
q.h<-paste(q.h, "0", sep="")
} 
}



Alpha.unconv.averages.2010[j,3:4]<-c(m, paste(paste("(", q.l, ",", sep=""), paste(q.h,")", sep=""),sep=""))


}

Table.A.11<-rbind(Alpha.conv.averages.2010,Alpha.unconv.averages.2010)

Table.A.11<-Table.A.11[c(4,1:3,5, 9, 6:8,10),] 

colnames(Table.A.11)<-c("Activity", "Dimension", "Mean", "Interval")

print(Table.A.11)



Table.A.11<-cbind(Table.A.11[1:5,-c(2)],Table.A.11[6:10,3:4])

Titles<-matrix(c("", "Conventional", "Dimension", "Unconventional", "Dimension"),nrow=1)

g1 <- tableGrob(Table.A.11, rows = NULL)
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g1), l = 1, r = ncol(g1))
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g1), l = 1, r = 1)
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g1), l = 1, r = 3)
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 1, l = 1, r = ncol(g1))


g0 <- tableGrob(Titles, rows = NULL)
g0 <- gtable_add_grob(g0,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g0), l = 1, r = 3)
g0 <- gtable_add_grob(g0,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g0), l = 1, r = 1)
g0 <- gtable_add_grob(g0,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 1, l = 1, r = ncol(g0))

g<-rbind(g0,g1)


pdf(file = "Table.A.11.pdf")
grid.draw(g)
dev.off()


################################################# Table A.12 ##################################################################################

rm(list=ls())
load("Results_Wave2012")
Results.2012<-Results


## Convergence Checks for Wave 2012 Model
Alpha<-mcmc.list(as.mcmc(t(Results.2012$alpha[,,1])), 
		as.mcmc(t(Results.2012$alpha[,,2])),
		as.mcmc(t(Results.2012$alpha[,,3])))
print(paste("Convergence Alpha:", gelman.diag(Alpha)[[2]]<1.2, sep=" "))


Alpha.Conv<-mcmc.list(as.mcmc(t(Results.2012$alpha_conv[-seq(5,dim(Results.2012$alpha_conv)[1],by=5),,1])), 
		as.mcmc(t(Results.2012$alpha_conv[-seq(5,dim(Results.2012$alpha_conv)[1],by=5),,2])),
		as.mcmc(t(Results.2012$alpha_conv[-seq(5,dim(Results.2012$alpha_conv)[1],by=5),,3])))
print(paste("Convergence Alpha Conv:", gelman.diag(Alpha.Conv)[[2]]<1.2, sep=" "))


Alpha.Unconv<-mcmc.list(as.mcmc(t(Results.2012$alpha_unconv[-seq(1,dim(Results.2012$alpha_conv)[1],by=5),,1])), 
		as.mcmc(t(Results.2012$alpha_unconv[-seq(1,dim(Results.2012$alpha_conv)[1],by=5),,2])),
		as.mcmc(t(Results.2012$alpha_unconv[-seq(1,dim(Results.2012$alpha_conv)[1],by=5),,3])))
print(paste("Convergence Alpha Unconventional:", gelman.diag(Alpha.Unconv)[[2]]<1.2, sep=" "))


P.Conv<-mcmc.list(as.mcmc((Results.2012$P_conv[2,,1])), 
		as.mcmc((Results.2012$P_conv[2,,2])),
		as.mcmc((Results.2012$P_conv[2,,3])))
print(paste("Convergence Prob. Conventional:", gelman.diag(P.Conv)[[1]][,1]<1.2, sep=" "))

P.Unconv<-mcmc.list(as.mcmc((Results.2012$P_unconv[2,,1])), 
		as.mcmc((Results.2012$P_unconv[2,,2])),
		as.mcmc((Results.2012$P_unconv[2,,3])))
print(paste("Convergence Prob. Unconventional", gelman.diag(P.Unconv)[[1]][,1]<1.2, sep=" "))


Beta2<-mcmc.list(as.mcmc(t(Results.2012$beta2[,,1])),
	as.mcmc(t(Results.2012$beta2[,,2])),
	as.mcmc(t(Results.2012$beta2[,,3])))

Beta3<-mcmc.list(as.mcmc(t(Results.2012$beta3[,,1])),
	as.mcmc(t(Results.2012$beta3[,,2])),
	as.mcmc(t(Results.2012$beta3[,,3])))

Beta4<-mcmc.list(as.mcmc(t(Results.2012$beta4[,,1])),
	as.mcmc(t(Results.2012$beta4[,,2])),
	as.mcmc(t(Results.2012$beta4[,,3])))

print(paste("Convergence Regression coefficients:", gelman.diag(Beta2)[[2]]<1.2 & gelman.diag(Beta3)[[2]]<1.2 & gelman.diag(Beta4)[[2]]<1.2, sep=" "))



ALPHA.CONV.2012<-rbind(t(Results.2012$alpha_conv[,,1]), 
		t(Results.2012$alpha_conv[,,2]),
		t(Results.2012$alpha_conv[,,3]))


Alpha.conv.averages.2012<-matrix(0, 5,4)
rownames(Alpha.conv.averages.2012)<-NULL
colnames(Alpha.conv.averages.2012)<-NULL

Alpha.conv.averages.2012[,1]<-c("Municipal meeting", "Improvements meeting", "Party meeting", "Voting", "Protesting")
Alpha.conv.averages.2012[,2]<-"Conventional"



for (j in 1:5) {
m<-round(mean(c(ALPHA.CONV.2012[, seq(j, ncol(ALPHA.CONV.2012), by=5)])), digits=3)
q.l<-round(quantile(c(ALPHA.CONV.2012[, seq(j, ncol(ALPHA.CONV.2012), by=5)]), c(0.025,0.975))[1], digits=3)
q.h<-round(quantile(c(ALPHA.CONV.2012[, seq(j, ncol(ALPHA.CONV.2012), by=5)]), c(0.025,0.975))[2], digits=3)


if (nchar(m)<5 & grepl("\\.", m)==FALSE) {
m<-paste(m,".", sep="")
while (nchar(m)<5) {
m<-paste(m, "0", sep="")
} 
} else if (nchar(m)<5 & grepl("\\.", m)==TRUE) {
while (nchar(m)<5) {
m<-paste(m, "0", sep="")
} 
}


if (nchar(q.l)<5 & grepl("\\.", q.l)==FALSE) {
q.l<-paste(q.l,".", sep="")
while (nchar(q.l)<5) {
q.l<-paste(q.l, "0", sep="")
} 
} else if (nchar(q.l)<5 & grepl("\\.", q.l)==TRUE) {
while (nchar(q.l)<5) {
q.l<-paste(q.l, "0", sep="")
} 
}

if (nchar(q.h)<5 & grepl("\\.", q.h)==FALSE) {
q.h<-paste(q.h,".", sep="")
while (nchar(q.h)<5) {
q.h<-paste(q.h, "0", sep="")
} 
} else if (nchar(q.h)<5 & grepl("\\.", q.h)==TRUE) {
while (nchar(q.h)<5) {
q.h<-paste(q.h, "0", sep="")
} 
}



Alpha.conv.averages.2012[j,3:4]<-c(m, paste(paste("(", q.l, ",", sep=""), paste(q.h,")", sep=""),sep=""))
}


ALPHA.UNCONV.2012<-rbind(t(Results.2012$alpha_unconv[,,1]), 
		t(Results.2012$alpha_unconv[,,2]),
		t(Results.2012$alpha_unconv[,,3]))

Alpha.unconv.averages.2012<-matrix(0, 5,4)
Alpha.unconv.averages.2012[,1]<-c("Municipal meeting", "Improvements meeting", "Party meeting", "Voting", "Protesting")
Alpha.unconv.averages.2012[,2]<-"Unconventional"


for (j in 1:5) {
m<-round(mean(c(ALPHA.UNCONV.2012[, seq(j, ncol(ALPHA.UNCONV.2012), by=5)])), digits=3)
q.l<-round(quantile(c(ALPHA.UNCONV.2012[, seq(j, ncol(ALPHA.UNCONV.2012), by=5)]), c(0.025,0.975))[1], digits=3)
q.h<-round(quantile(c(ALPHA.UNCONV.2012[, seq(j, ncol(ALPHA.UNCONV.2012), by=5)]), c(0.025,0.975))[2], digits=3)


if (nchar(m)<5 & grepl("\\.", m)==FALSE) {
m<-paste(m,".", sep="")
while (nchar(m)<5) {
m<-paste(m, "0", sep="")
} 
} else if (nchar(m)<5 & grepl("\\.", m)==TRUE) {
while (nchar(m)<5) {
m<-paste(m, "0", sep="")
} 
}


if (nchar(q.l)<5 & grepl("\\.", q.l)==FALSE) {
q.l<-paste(q.l,".", sep="")
while (nchar(q.l)<5) {
q.l<-paste(q.l, "0", sep="")
} 
} else if (nchar(q.l)<5 & grepl("\\.", q.l)==TRUE) {
while (nchar(q.l)<5) {
q.l<-paste(q.l, "0", sep="")
} 
}

if (nchar(q.h)<5 & grepl("\\.", q.h)==FALSE) {
q.h<-paste(q.h,".", sep="")
while (nchar(q.h)<5) {
q.h<-paste(q.h, "0", sep="")
} 
} else if (nchar(q.h)<5 & grepl("\\.", q.h)==TRUE) {
while (nchar(q.h)<5) {
q.h<-paste(q.h, "0", sep="")
} 
}



Alpha.unconv.averages.2012[j,3:4]<-c(m, paste(paste("(", q.l, ",", sep=""), paste(q.h,")", sep=""),sep=""))


}

Table.A.12<-rbind(Alpha.conv.averages.2012,Alpha.unconv.averages.2012)

Table.A.12<-Table.A.12[c(4,1:3,5, 9, 6:8,10),] 

colnames(Table.A.12)<-c("Activity", "Dimension", "Mean", "Interval")

print(Table.A.12)



Table.A.12<-cbind(Table.A.12[1:5,-c(2)],Table.A.12[6:10,3:4])

Titles<-matrix(c("", "Conventional", "Dimension", "Unconventional", "Dimension"),nrow=1)

g1 <- tableGrob(Table.A.12, rows = NULL)
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g1), l = 1, r = ncol(g1))
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g1), l = 1, r = 1)
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g1), l = 1, r = 3)
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 1, l = 1, r = ncol(g1))


g0 <- tableGrob(Titles, rows = NULL)
g0 <- gtable_add_grob(g0,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g0), l = 1, r = 3)
g0 <- gtable_add_grob(g0,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g0), l = 1, r = 1)
g0 <- gtable_add_grob(g0,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 1, l = 1, r = ncol(g0))

g<-rbind(g0,g1)


pdf(file = "Table.A.12.pdf")
grid.draw(g)
dev.off()


################################################# Table A.13 ##################################################################################

rm(list=ls())

load("Results_Wave2014")
Results.2014<-Results


## Convergence Checks for Wave 2014 Model
Alpha<-mcmc.list(as.mcmc(t(Results.2014$alpha[,,1])), 
		as.mcmc(t(Results.2014$alpha[,,2])),
		as.mcmc(t(Results.2014$alpha[,,3])))
print(paste("Convergence Alpha:", gelman.diag(Alpha)[[2]]<1.2, sep=" "))


Alpha.Conv<-mcmc.list(as.mcmc(t(Results.2014$alpha_conv[-seq(5,dim(Results.2014$alpha_conv)[1],by=5),,1])), 
		as.mcmc(t(Results.2014$alpha_conv[-seq(5,dim(Results.2014$alpha_conv)[1],by=5),,2])),
		as.mcmc(t(Results.2014$alpha_conv[-seq(5,dim(Results.2014$alpha_conv)[1],by=5),,3])))
print(paste("Convergence Alpha Conv:", gelman.diag(Alpha.Conv)[[2]]<1.2, sep=" "))


Alpha.Unconv<-mcmc.list(as.mcmc(t(Results.2014$alpha_unconv[-seq(1,dim(Results.2014$alpha_conv)[1],by=5),,1])), 
		as.mcmc(t(Results.2014$alpha_unconv[-seq(1,dim(Results.2014$alpha_conv)[1],by=5),,2])),
		as.mcmc(t(Results.2014$alpha_unconv[-seq(1,dim(Results.2014$alpha_conv)[1],by=5),,3])))
print(paste("Convergence Alpha Unconventional:", gelman.diag(Alpha.Unconv)[[2]]<1.2, sep=" "))


P.Conv<-mcmc.list(as.mcmc((Results.2014$P_conv[2,,1])), 
		as.mcmc((Results.2014$P_conv[2,,2])),
		as.mcmc((Results.2014$P_conv[2,,3])))
print(paste("Convergence Prob. Conventional:", gelman.diag(P.Conv)[[1]][,1]<1.2, sep=" "))

P.Unconv<-mcmc.list(as.mcmc((Results.2014$P_unconv[2,,1])), 
		as.mcmc((Results.2014$P_unconv[2,,2])),
		as.mcmc((Results.2014$P_unconv[2,,3])))
print(paste("Convergence Prob. Unconventional", gelman.diag(P.Unconv)[[1]][,1]<1.2, sep=" "))


Beta2<-mcmc.list(as.mcmc(t(Results.2014$beta2[,,1])),
	as.mcmc(t(Results.2014$beta2[,,2])),
	as.mcmc(t(Results.2014$beta2[,,3])))

Beta3<-mcmc.list(as.mcmc(t(Results.2014$beta3[,,1])),
	as.mcmc(t(Results.2014$beta3[,,2])),
	as.mcmc(t(Results.2014$beta3[,,3])))

Beta4<-mcmc.list(as.mcmc(t(Results.2014$beta4[,,1])),
	as.mcmc(t(Results.2014$beta4[,,2])),
	as.mcmc(t(Results.2014$beta4[,,3])))

print(paste("Convergence Regression coefficients:", gelman.diag(Beta2)[[2]]<1.2 & gelman.diag(Beta3)[[2]]<1.2 & gelman.diag(Beta4)[[2]]<1.2, sep=" "))



ALPHA.CONV.2014<-rbind(t(Results.2014$alpha_conv[,,1]), 
		t(Results.2014$alpha_conv[,,2]),
		t(Results.2014$alpha_conv[,,3]))


Alpha.conv.averages.2014<-matrix(0, 5,4)
rownames(Alpha.conv.averages.2014)<-NULL
colnames(Alpha.conv.averages.2014)<-NULL

Alpha.conv.averages.2014[,1]<-c("Municipal meeting", "Improvements meeting", "Party meeting", "Voting", "Protesting")
Alpha.conv.averages.2014[,2]<-"Conventional"



for (j in 1:5) {
m<-round(mean(c(ALPHA.CONV.2014[, seq(j, ncol(ALPHA.CONV.2014), by=5)])), digits=3)
q.l<-round(quantile(c(ALPHA.CONV.2014[, seq(j, ncol(ALPHA.CONV.2014), by=5)]), c(0.025,0.975))[1], digits=3)
q.h<-round(quantile(c(ALPHA.CONV.2014[, seq(j, ncol(ALPHA.CONV.2014), by=5)]), c(0.025,0.975))[2], digits=3)


if (nchar(m)<5 & grepl("\\.", m)==FALSE) {
m<-paste(m,".", sep="")
while (nchar(m)<5) {
m<-paste(m, "0", sep="")
} 
} else if (nchar(m)<5 & grepl("\\.", m)==TRUE) {
while (nchar(m)<5) {
m<-paste(m, "0", sep="")
} 
}


if (nchar(q.l)<5 & grepl("\\.", q.l)==FALSE) {
q.l<-paste(q.l,".", sep="")
while (nchar(q.l)<5) {
q.l<-paste(q.l, "0", sep="")
} 
} else if (nchar(q.l)<5 & grepl("\\.", q.l)==TRUE) {
while (nchar(q.l)<5) {
q.l<-paste(q.l, "0", sep="")
} 
}

if (nchar(q.h)<5 & grepl("\\.", q.h)==FALSE) {
q.h<-paste(q.h,".", sep="")
while (nchar(q.h)<5) {
q.h<-paste(q.h, "0", sep="")
} 
} else if (nchar(q.h)<5 & grepl("\\.", q.h)==TRUE) {
while (nchar(q.h)<5) {
q.h<-paste(q.h, "0", sep="")
} 
}



Alpha.conv.averages.2014[j,3:4]<-c(m, paste(paste("(", q.l, ",", sep=""), paste(q.h,")", sep=""),sep=""))
}


ALPHA.UNCONV.2014<-rbind(t(Results.2014$alpha_unconv[,,1]), 
		t(Results.2014$alpha_unconv[,,2]),
		t(Results.2014$alpha_unconv[,,3]))

Alpha.unconv.averages.2014<-matrix(0, 5,4)
Alpha.unconv.averages.2014[,1]<-c("Municipal meeting", "Improvements meeting", "Party meeting", "Voting", "Protesting")
Alpha.unconv.averages.2014[,2]<-"Unconventional"


for (j in 1:5) {
m<-round(mean(c(ALPHA.UNCONV.2014[, seq(j, ncol(ALPHA.UNCONV.2014), by=5)])), digits=3)
q.l<-round(quantile(c(ALPHA.UNCONV.2014[, seq(j, ncol(ALPHA.UNCONV.2014), by=5)]), c(0.025,0.975))[1], digits=3)
q.h<-round(quantile(c(ALPHA.UNCONV.2014[, seq(j, ncol(ALPHA.UNCONV.2014), by=5)]), c(0.025,0.975))[2], digits=3)


if (nchar(m)<5 & grepl("\\.", m)==FALSE) {
m<-paste(m,".", sep="")
while (nchar(m)<5) {
m<-paste(m, "0", sep="")
} 
} else if (nchar(m)<5 & grepl("\\.", m)==TRUE) {
while (nchar(m)<5) {
m<-paste(m, "0", sep="")
} 
}


if (nchar(q.l)<5 & grepl("\\.", q.l)==FALSE) {
q.l<-paste(q.l,".", sep="")
while (nchar(q.l)<5) {
q.l<-paste(q.l, "0", sep="")
} 
} else if (nchar(q.l)<5 & grepl("\\.", q.l)==TRUE) {
while (nchar(q.l)<5) {
q.l<-paste(q.l, "0", sep="")
} 
}

if (nchar(q.h)<5 & grepl("\\.", q.h)==FALSE) {
q.h<-paste(q.h,".", sep="")
while (nchar(q.h)<5) {
q.h<-paste(q.h, "0", sep="")
} 
} else if (nchar(q.h)<5 & grepl("\\.", q.h)==TRUE) {
while (nchar(q.h)<5) {
q.h<-paste(q.h, "0", sep="")
} 
}



Alpha.unconv.averages.2014[j,3:4]<-c(m, paste(paste("(", q.l, ",", sep=""), paste(q.h,")", sep=""),sep=""))


}

Table.A.13<-rbind(Alpha.conv.averages.2014,Alpha.unconv.averages.2014)

Table.A.13<-Table.A.13[c(4,1:3,5, 9, 6:8,10),] 

colnames(Table.A.13)<-c("Activity", "Dimension", "Mean", "Interval")

print(Table.A.13)



Table.A.13<-cbind(Table.A.13[1:5,-c(2)],Table.A.13[6:10,3:4])

Titles<-matrix(c("", "Conventional", "Dimension", "Unconventional", "Dimension"),nrow=1)

g1 <- tableGrob(Table.A.13, rows = NULL)
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g1), l = 1, r = ncol(g1))
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g1), l = 1, r = 1)
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g1), l = 1, r = 3)
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 1, l = 1, r = ncol(g1))


g0 <- tableGrob(Titles, rows = NULL)
g0 <- gtable_add_grob(g0,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g0), l = 1, r = 3)
g0 <- gtable_add_grob(g0,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g0), l = 1, r = 1)
g0 <- gtable_add_grob(g0,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 1, l = 1, r = ncol(g0))

g<-rbind(g0,g1)


pdf(file = "Table.A.13.pdf")
grid.draw(g)
dev.off()


################################################# Table A.14 ##################################################################################

rm(list=ls())
load("Results_Wave2016")
Results.2016<-Results


## Convergence Checks for Wave 2016 Model
Alpha<-mcmc.list(as.mcmc(t(Results.2016$alpha[,,1])), 
		as.mcmc(t(Results.2016$alpha[,,2])),
		as.mcmc(t(Results.2016$alpha[,,3])))
print(paste("Convergence Alpha:", gelman.diag(Alpha)[[2]]<1.2, sep=" "))


Alpha.Conv<-mcmc.list(as.mcmc(t(Results.2016$alpha_conv[-seq(5,dim(Results.2016$alpha_conv)[1],by=5),,1])), 
		as.mcmc(t(Results.2016$alpha_conv[-seq(5,dim(Results.2016$alpha_conv)[1],by=5),,2])),
		as.mcmc(t(Results.2016$alpha_conv[-seq(5,dim(Results.2016$alpha_conv)[1],by=5),,3])))
print(paste("Convergence Alpha Conv:", gelman.diag(Alpha.Conv)[[2]]<1.2, sep=" "))


Alpha.Unconv<-mcmc.list(as.mcmc(t(Results.2016$alpha_unconv[-seq(1,dim(Results.2016$alpha_conv)[1],by=5),,1])), 
		as.mcmc(t(Results.2016$alpha_unconv[-seq(1,dim(Results.2016$alpha_conv)[1],by=5),,2])),
		as.mcmc(t(Results.2016$alpha_unconv[-seq(1,dim(Results.2016$alpha_conv)[1],by=5),,3])))
print(paste("Convergence Alpha Unconventional:", gelman.diag(Alpha.Unconv)[[2]]<1.2, sep=" "))


P.Conv<-mcmc.list(as.mcmc((Results.2016$P_conv[2,,1])), 
		as.mcmc((Results.2016$P_conv[2,,2])),
		as.mcmc((Results.2016$P_conv[2,,3])))
print(paste("Convergence Prob. Conventional:", gelman.diag(P.Conv)[[1]][,1]<1.2, sep=" "))

P.Unconv<-mcmc.list(as.mcmc((Results.2016$P_unconv[2,,1])), 
		as.mcmc((Results.2016$P_unconv[2,,2])),
		as.mcmc((Results.2016$P_unconv[2,,3])))
print(paste("Convergence Prob. Unconventional", gelman.diag(P.Unconv)[[1]][,1]<1.2, sep=" "))


Beta2<-mcmc.list(as.mcmc(t(Results.2016$beta2[,,1])),
	as.mcmc(t(Results.2016$beta2[,,2])),
	as.mcmc(t(Results.2016$beta2[,,3])))

Beta3<-mcmc.list(as.mcmc(t(Results.2016$beta3[,,1])),
	as.mcmc(t(Results.2016$beta3[,,2])),
	as.mcmc(t(Results.2016$beta3[,,3])))

Beta4<-mcmc.list(as.mcmc(t(Results.2016$beta4[,,1])),
	as.mcmc(t(Results.2016$beta4[,,2])),
	as.mcmc(t(Results.2016$beta4[,,3])))

print(paste("Convergence Regression coefficients:", gelman.diag(Beta2)[[2]]<1.2 & gelman.diag(Beta3)[[2]]<1.2 & gelman.diag(Beta4)[[2]]<1.2, sep=" "))



ALPHA.CONV.2016<-rbind(t(Results.2016$alpha_conv[,,1]), 
		t(Results.2016$alpha_conv[,,2]),
		t(Results.2016$alpha_conv[,,3]))


Alpha.conv.averages.2016<-matrix(0, 5,4)
rownames(Alpha.conv.averages.2016)<-NULL
colnames(Alpha.conv.averages.2016)<-NULL

Alpha.conv.averages.2016[,1]<-c("Municipal meeting", "Improvements meeting", "Party meeting", "Voting", "Protesting")
Alpha.conv.averages.2016[,2]<-"Conventional"



for (j in 1:5) {
m<-round(mean(c(ALPHA.CONV.2016[, seq(j, ncol(ALPHA.CONV.2016), by=5)])), digits=3)
q.l<-round(quantile(c(ALPHA.CONV.2016[, seq(j, ncol(ALPHA.CONV.2016), by=5)]), c(0.025,0.975))[1], digits=3)
q.h<-round(quantile(c(ALPHA.CONV.2016[, seq(j, ncol(ALPHA.CONV.2016), by=5)]), c(0.025,0.975))[2], digits=3)


if (nchar(m)<5 & grepl("\\.", m)==FALSE) {
m<-paste(m,".", sep="")
while (nchar(m)<5) {
m<-paste(m, "0", sep="")
} 
} else if (nchar(m)<5 & grepl("\\.", m)==TRUE) {
while (nchar(m)<5) {
m<-paste(m, "0", sep="")
} 
}


if (nchar(q.l)<5 & grepl("\\.", q.l)==FALSE) {
q.l<-paste(q.l,".", sep="")
while (nchar(q.l)<5) {
q.l<-paste(q.l, "0", sep="")
} 
} else if (nchar(q.l)<5 & grepl("\\.", q.l)==TRUE) {
while (nchar(q.l)<5) {
q.l<-paste(q.l, "0", sep="")
} 
}

if (nchar(q.h)<5 & grepl("\\.", q.h)==FALSE) {
q.h<-paste(q.h,".", sep="")
while (nchar(q.h)<5) {
q.h<-paste(q.h, "0", sep="")
} 
} else if (nchar(q.h)<5 & grepl("\\.", q.h)==TRUE) {
while (nchar(q.h)<5) {
q.h<-paste(q.h, "0", sep="")
} 
}



Alpha.conv.averages.2016[j,3:4]<-c(m, paste(paste("(", q.l, ",", sep=""), paste(q.h,")", sep=""),sep=""))
}


ALPHA.UNCONV.2016<-rbind(t(Results.2016$alpha_unconv[,,1]), 
		t(Results.2016$alpha_unconv[,,2]),
		t(Results.2016$alpha_unconv[,,3]))

Alpha.unconv.averages.2016<-matrix(0, 5,4)
Alpha.unconv.averages.2016[,1]<-c("Municipal meeting", "Improvements meeting", "Party meeting", "Voting", "Protesting")
Alpha.unconv.averages.2016[,2]<-"Unconventional"


for (j in 1:5) {
m<-round(mean(c(ALPHA.UNCONV.2016[, seq(j, ncol(ALPHA.UNCONV.2016), by=5)])), digits=3)
q.l<-round(quantile(c(ALPHA.UNCONV.2016[, seq(j, ncol(ALPHA.UNCONV.2016), by=5)]), c(0.025,0.975))[1], digits=3)
q.h<-round(quantile(c(ALPHA.UNCONV.2016[, seq(j, ncol(ALPHA.UNCONV.2016), by=5)]), c(0.025,0.975))[2], digits=3)


if (nchar(m)<5 & grepl("\\.", m)==FALSE) {
m<-paste(m,".", sep="")
while (nchar(m)<5) {
m<-paste(m, "0", sep="")
} 
} else if (nchar(m)<5 & grepl("\\.", m)==TRUE) {
while (nchar(m)<5) {
m<-paste(m, "0", sep="")
} 
}


if (nchar(q.l)<5 & grepl("\\.", q.l)==FALSE) {
q.l<-paste(q.l,".", sep="")
while (nchar(q.l)<5) {
q.l<-paste(q.l, "0", sep="")
} 
} else if (nchar(q.l)<5 & grepl("\\.", q.l)==TRUE) {
while (nchar(q.l)<5) {
q.l<-paste(q.l, "0", sep="")
} 
}

if (nchar(q.h)<5 & grepl("\\.", q.h)==FALSE) {
q.h<-paste(q.h,".", sep="")
while (nchar(q.h)<5) {
q.h<-paste(q.h, "0", sep="")
} 
} else if (nchar(q.h)<5 & grepl("\\.", q.h)==TRUE) {
while (nchar(q.h)<5) {
q.h<-paste(q.h, "0", sep="")
} 
}



Alpha.unconv.averages.2016[j,3:4]<-c(m, paste(paste("(", q.l, ",", sep=""), paste(q.h,")", sep=""),sep=""))


}

Table.A.14<-rbind(Alpha.conv.averages.2016,Alpha.unconv.averages.2016)

Table.A.14<-Table.A.14[c(4,1:3,5, 9, 6:8,10),] 

colnames(Table.A.14)<-c("Activity", "Dimension", "Mean", "Interval")

print(Table.A.14)



Table.A.14<-cbind(Table.A.14[1:5,-c(2)],Table.A.14[6:10,3:4])

Titles<-matrix(c("", "Conventional", "Dimension", "Unconventional", "Dimension"),nrow=1)

g1 <- tableGrob(Table.A.14, rows = NULL)
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g1), l = 1, r = ncol(g1))
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g1), l = 1, r = 1)
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g1), l = 1, r = 3)
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 1, l = 1, r = ncol(g1))


g0 <- tableGrob(Titles, rows = NULL)
g0 <- gtable_add_grob(g0,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g0), l = 1, r = 3)
g0 <- gtable_add_grob(g0,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g0), l = 1, r = 1)
g0 <- gtable_add_grob(g0,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 1, l = 1, r = ncol(g0))

g<-rbind(g0,g1)


pdf(file = "Table.A.14.pdf")
grid.draw(g)
dev.off()


################################################# Table A.15 ##################################################################################

rm(list=ls())
load("Results_Wave2018")
Results.2018<-Results


## Convergence Checks for Wave 2018 Model
Alpha<-mcmc.list(as.mcmc(t(Results.2018$alpha[,,1])), 
		as.mcmc(t(Results.2018$alpha[,,2])),
		as.mcmc(t(Results.2018$alpha[,,3])))
print(paste("Convergence Alpha:", gelman.diag(Alpha)[[2]]<1.2, sep=" "))


Alpha.Conv<-mcmc.list(as.mcmc(t(Results.2018$alpha_conv[-seq(5,dim(Results.2018$alpha_conv)[1],by=5),,1])), 
		as.mcmc(t(Results.2018$alpha_conv[-seq(5,dim(Results.2018$alpha_conv)[1],by=5),,2])),
		as.mcmc(t(Results.2018$alpha_conv[-seq(5,dim(Results.2018$alpha_conv)[1],by=5),,3])))
print(paste("Convergence Alpha Conv:", gelman.diag(Alpha.Conv)[[2]]<1.2, sep=" "))


Alpha.Unconv<-mcmc.list(as.mcmc(t(Results.2018$alpha_unconv[-seq(1,dim(Results.2018$alpha_conv)[1],by=5),,1])), 
		as.mcmc(t(Results.2018$alpha_unconv[-seq(1,dim(Results.2018$alpha_conv)[1],by=5),,2])),
		as.mcmc(t(Results.2018$alpha_unconv[-seq(1,dim(Results.2018$alpha_conv)[1],by=5),,3])))
print(paste("Convergence Alpha Unconventional:", gelman.diag(Alpha.Unconv)[[2]]<1.2, sep=" "))


P.Conv<-mcmc.list(as.mcmc((Results.2018$P_conv[2,,1])), 
		as.mcmc((Results.2018$P_conv[2,,2])),
		as.mcmc((Results.2018$P_conv[2,,3])))
print(paste("Convergence Prob. Conventional:", gelman.diag(P.Conv)[[1]][,1]<1.2, sep=" "))

P.Unconv<-mcmc.list(as.mcmc((Results.2018$P_unconv[2,,1])), 
		as.mcmc((Results.2018$P_unconv[2,,2])),
		as.mcmc((Results.2018$P_unconv[2,,3])))
print(paste("Convergence Prob. Unconventional", gelman.diag(P.Unconv)[[1]][,1]<1.2, sep=" "))


Beta2<-mcmc.list(as.mcmc(t(Results.2018$beta2[,,1])),
	as.mcmc(t(Results.2018$beta2[,,2])),
	as.mcmc(t(Results.2018$beta2[,,3])))

Beta3<-mcmc.list(as.mcmc(t(Results.2018$beta3[,,1])),
	as.mcmc(t(Results.2018$beta3[,,2])),
	as.mcmc(t(Results.2018$beta3[,,3])))

Beta4<-mcmc.list(as.mcmc(t(Results.2018$beta4[,,1])),
	as.mcmc(t(Results.2018$beta4[,,2])),
	as.mcmc(t(Results.2018$beta4[,,3])))

print(paste("Convergence Regression coefficients:", gelman.diag(Beta2)[[2]]<1.2 & gelman.diag(Beta3)[[2]]<1.2 & gelman.diag(Beta4)[[2]]<1.2, sep=" "))



ALPHA.CONV.2018<-rbind(t(Results.2018$alpha_conv[,,1]), 
		t(Results.2018$alpha_conv[,,2]),
		t(Results.2018$alpha_conv[,,3]))


Alpha.conv.averages.2018<-matrix(0, 5,4)
rownames(Alpha.conv.averages.2018)<-NULL
colnames(Alpha.conv.averages.2018)<-NULL

Alpha.conv.averages.2018[,1]<-c("Municipal meeting", "Improvements meeting", "Party meeting", "Voting", "Protesting")
Alpha.conv.averages.2018[,2]<-"Conventional"



for (j in 1:5) {
m<-round(mean(c(ALPHA.CONV.2018[, seq(j, ncol(ALPHA.CONV.2018), by=5)])), digits=3)
q.l<-round(quantile(c(ALPHA.CONV.2018[, seq(j, ncol(ALPHA.CONV.2018), by=5)]), c(0.025,0.975))[1], digits=3)
q.h<-round(quantile(c(ALPHA.CONV.2018[, seq(j, ncol(ALPHA.CONV.2018), by=5)]), c(0.025,0.975))[2], digits=3)


if (nchar(m)<5 & grepl("\\.", m)==FALSE) {
m<-paste(m,".", sep="")
while (nchar(m)<5) {
m<-paste(m, "0", sep="")
} 
} else if (nchar(m)<5 & grepl("\\.", m)==TRUE) {
while (nchar(m)<5) {
m<-paste(m, "0", sep="")
} 
}


if (nchar(q.l)<5 & grepl("\\.", q.l)==FALSE) {
q.l<-paste(q.l,".", sep="")
while (nchar(q.l)<5) {
q.l<-paste(q.l, "0", sep="")
} 
} else if (nchar(q.l)<5 & grepl("\\.", q.l)==TRUE) {
while (nchar(q.l)<5) {
q.l<-paste(q.l, "0", sep="")
} 
}

if (nchar(q.h)<5 & grepl("\\.", q.h)==FALSE) {
q.h<-paste(q.h,".", sep="")
while (nchar(q.h)<5) {
q.h<-paste(q.h, "0", sep="")
} 
} else if (nchar(q.h)<5 & grepl("\\.", q.h)==TRUE) {
while (nchar(q.h)<5) {
q.h<-paste(q.h, "0", sep="")
} 
}



Alpha.conv.averages.2018[j,3:4]<-c(m, paste(paste("(", q.l, ",", sep=""), paste(q.h,")", sep=""),sep=""))
}


ALPHA.UNCONV.2018<-rbind(t(Results.2018$alpha_unconv[,,1]), 
		t(Results.2018$alpha_unconv[,,2]),
		t(Results.2018$alpha_unconv[,,3]))

Alpha.unconv.averages.2018<-matrix(0, 5,4)
Alpha.unconv.averages.2018[,1]<-c("Municipal meeting", "Improvements meeting", "Party meeting", "Voting", "Protesting")
Alpha.unconv.averages.2018[,2]<-"Unconventional"


for (j in 1:5) {
m<-round(mean(c(ALPHA.UNCONV.2018[, seq(j, ncol(ALPHA.UNCONV.2018), by=5)])), digits=3)
q.l<-round(quantile(c(ALPHA.UNCONV.2018[, seq(j, ncol(ALPHA.UNCONV.2018), by=5)]), c(0.025,0.975))[1], digits=3)
q.h<-round(quantile(c(ALPHA.UNCONV.2018[, seq(j, ncol(ALPHA.UNCONV.2018), by=5)]), c(0.025,0.975))[2], digits=3)


if (nchar(m)<5 & grepl("\\.", m)==FALSE) {
m<-paste(m,".", sep="")
while (nchar(m)<5) {
m<-paste(m, "0", sep="")
} 
} else if (nchar(m)<5 & grepl("\\.", m)==TRUE) {
while (nchar(m)<5) {
m<-paste(m, "0", sep="")
} 
}


if (nchar(q.l)<5 & grepl("\\.", q.l)==FALSE) {
q.l<-paste(q.l,".", sep="")
while (nchar(q.l)<5) {
q.l<-paste(q.l, "0", sep="")
} 
} else if (nchar(q.l)<5 & grepl("\\.", q.l)==TRUE) {
while (nchar(q.l)<5) {
q.l<-paste(q.l, "0", sep="")
} 
}

if (nchar(q.h)<5 & grepl("\\.", q.h)==FALSE) {
q.h<-paste(q.h,".", sep="")
while (nchar(q.h)<5) {
q.h<-paste(q.h, "0", sep="")
} 
} else if (nchar(q.h)<5 & grepl("\\.", q.h)==TRUE) {
while (nchar(q.h)<5) {
q.h<-paste(q.h, "0", sep="")
} 
}



Alpha.unconv.averages.2018[j,3:4]<-c(m, paste(paste("(", q.l, ",", sep=""), paste(q.h,")", sep=""),sep=""))


}

Table.A.15<-rbind(Alpha.conv.averages.2018,Alpha.unconv.averages.2018)

Table.A.15<-Table.A.15[c(4,1:3,5, 9, 6:8,10),] 

colnames(Table.A.15)<-c("Activity", "Dimension", "Mean", "Interval")

print(Table.A.15)



Table.A.15<-cbind(Table.A.15[1:5,-c(2)],Table.A.15[6:10,3:4])

Titles<-matrix(c("", "Conventional", "Dimension", "Unconventional", "Dimension"),nrow=1)

g1 <- tableGrob(Table.A.15, rows = NULL)
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g1), l = 1, r = ncol(g1))
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g1), l = 1, r = 1)
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g1), l = 1, r = 3)
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 1, l = 1, r = ncol(g1))


g0 <- tableGrob(Titles, rows = NULL)
g0 <- gtable_add_grob(g0,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g0), l = 1, r = 3)
g0 <- gtable_add_grob(g0,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g0), l = 1, r = 1)
g0 <- gtable_add_grob(g0,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 1, l = 1, r = ncol(g0))

g<-rbind(g0,g1)


pdf(file = "Table.A.15.pdf")
grid.draw(g)
dev.off()


################################################# Figure A.29 ##################################################################################

rm(list=ls())

load("dataset_2010to2018.RData")

WAVE<-c(2010,2012, 2014, 2016,2018)

Types<-c()
Outsider.by.Country.ALL<-c()
Activist.by.Country.ALL<-c()
Agitator.by.Country.ALL<-c()
Conventional.by.Country.ALL<-c()
NU.ACTIVIST.ALL<-c()
NU.AGITATOR.ALL<-c()
NU.CONVENTIONAL.ALL<-c()
Marginal.Individual.ALL<-c()
Marginal.Contextual.ALL<-c()


for (k in 1:length(WAVE)) {

load(paste("Results_Wave", WAVE[k], sep=""))

df<-subset(dataset, wave==WAVE[k], select=c("country.name", "country.abbreviation", "wave",
"ActMeetMun", "ActMeetImp" , "ActMeetPty" , "ActVoted",  "ActProtest" ,
 "education" ,  "female", "age", "relative.incomeproxy" ,
"victim", "corrupt", "spatial.distance" ,
"compulsory.voting", "gdp.pc", "rule.of.lw", "enp", "social.spending"
))                       
df<-na.exclude(df)




Y <- cbind(df$ActMeetMun, df$ActMeetImp, df$ActMeetPty, df$ActVoted, 
		df$ActProtest)
colnames(Y)<-c("ActMeetMun","ActMeetImp", "ActMeetPty", "ActVoted",
		"ActProtest")

Country<-as.numeric(factor(df$country.name), levels=unique(df$country.name))
		

Matrix.Country<-matrix(0,nrow(Y),length(unique(Country)))           
for (i in 1:length(unique(Country))) Matrix.Country[Country==i,i]<-1


df$age.centered<-(df$age-mean(df$age))/(2*sd(df$age))
df$education.centered<-(df$education-mean(df$education))/(2*sd(df$education))


df$relative.incomeproxy.centered<-(df$relative.incomeproxy-mean(df$relative.incomeproxy))/(2*sd(df$relative.incomeproxy))


df$spatial.distance.centered<-(df$spatial.distance-mean(df$spatial.distance))/(2*sd(df$spatial.distance))

X<-cbind(1, df$age.centered, df$female, df$education.centered, 
	df$relative.incomeproxy.centered, 
	df$victim, df$corrupt, 
	df$spatial.distance.centered,
	(df$rule.of.lw-mean(df$rule.of.lw))/(2*sd(df$rule.of.lw)),
		df$compulsory.voting,
		(df$enp-mean(df$enp))/(2*sd(df$enp)),
		(df$gdp.pc-mean(df$gdp.pc))/(2*sd(df$gdp.pc)),
		(df$social.spending-mean(df$social.spending))/(2*sd(df$social.spending))
)




colnames(X)<-c("Intercept", "Age", "Female", "Education", 
		 "Income", "Victimization", "Corruption",
		  "Ideology", "Rule of Law", "Compulsory Voting", "ENPP",
		"GDP", "Social.Spending")




BETA2<-rbind((t(Results$beta2[,,1])),
	(t(Results$beta2[,,2])),
	(t(Results$beta2[,,3])))



BETA3<-rbind((t(Results$beta3[,,1])),
	(t(Results$beta3[,,2])),
	(t(Results$beta3[,,3])))


BETA4<-rbind((t(Results$beta4[,,1])),
	(t(Results$beta4[,,2])),
	(t(Results$beta4[,,3])))


NU<-rbind((t(Results$nu[,,1])),
	(t(Results$nu[,,2])),
	(t(Results$nu[,,3])))


NU2<-NU[, 1:length(unique(Country))]
NU3<-NU[, (1+length(unique(Country))): (2*length(unique(Country)))]
NU4<-NU[, (1+2*length(unique(Country))): (3*length(unique(Country)))]


Beta.Agitator<-BETA2[seq(1, nrow(BETA2), length.out=1000),]
Beta.Conventional<-BETA3[seq(1, nrow(BETA3), length.out=1000),]
Beta.Activist<-BETA4[seq(1, nrow(BETA4), length.out=1000),]
Nu.Agitator<-NU2[seq(1, nrow(NU2), length.out=1000),]
Nu.Conventional<-NU3[seq(1, nrow(NU3), length.out=1000),]
Nu.Activist<-NU4[seq(1, nrow(NU4), length.out=1000),]




prob.Agitator<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X%*%Beta.Agitator[s,]+Nu.Agitator[s,Country])/(1+exp(X%*%Beta.Agitator[s,]+Nu.Agitator[s,Country])+
			exp(X%*%Beta.Conventional[s,]+Nu.Conventional[s,Country])+exp(X%*%Beta.Activist[s,]+Nu.Activist[s,Country])))

prob.Conventional<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X%*%Beta.Conventional[s,]+Nu.Conventional[s,Country])/(1+exp(X%*%Beta.Agitator[s,]+Nu.Agitator[s,Country])+
			exp(X%*%Beta.Conventional[s,]+Nu.Conventional[s,Country])+exp(X%*%Beta.Activist[s,]+Nu.Activist[s,Country])))

prob.Activist<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X%*%Beta.Activist[s,]+Nu.Activist[s,Country])/(1+exp(X%*%Beta.Agitator[s,]+Nu.Agitator[s,Country])+
			exp(X%*%Beta.Conventional[s,]+Nu.Conventional[s,Country])+exp(X%*%Beta.Activist[s,]+Nu.Activist[s,Country])))
prob.Outsider<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X%*%Beta.Agitator[s,]+Nu.Agitator[s,Country])+
			exp(X%*%Beta.Conventional[s,]+Nu.Conventional[s,Country])+exp(X%*%Beta.Activist[s,]+Nu.Activist[s,Country])))



Posterior.Probs<-cbind(colMeans(prob.Outsider), colMeans(prob.Agitator), colMeans(prob.Conventional), colMeans(prob.Activist))
			

Types<-rbind(Types,
cbind(WAVE[k], 
c("Outsider", "Agitator", "Conventional", "Activist"), 
colMeans(Posterior.Probs), 
t(apply(Posterior.Probs, 2, quantile, c(0.025,0.975))))
)


Outsider.by.Country<-c()
Activist.by.Country<-c()
Conventional.by.Country<-c()
Agitator.by.Country<-c()



for (j in 1:length(unique(Country))) {

Outsider.by.Country<-rbind(Outsider.by.Country,
c(unique(Country)[j],mean(colMeans(prob.Outsider[Country==j,])), quantile(colMeans(prob.Outsider[Country==j,]), c(0.025,0.975)))
)


Activist.by.Country<-rbind(Activist.by.Country,
c(unique(Country)[j],mean(colMeans(prob.Activist[Country==j,])), quantile(colMeans(prob.Activist[Country==j,]), c(0.025,0.975)))
)


Conventional.by.Country<-rbind(Conventional.by.Country,
c(unique(Country)[j],mean(colMeans(prob.Conventional[Country==j,])), quantile(colMeans(prob.Conventional[Country==j,]), c(0.025,0.975)))
)


Agitator.by.Country<-rbind(Agitator.by.Country,
c(unique(Country)[j],mean(colMeans(prob.Agitator[Country==j,])), quantile(colMeans(prob.Agitator[Country==j,]), c(0.025,0.975)))
)



}



Outsider.by.Country.ALL<-rbind(Outsider.by.Country.ALL, cbind(WAVE[k], Outsider.by.Country))
Activist.by.Country.ALL<-rbind(Activist.by.Country.ALL, cbind(WAVE[k], Activist.by.Country))
Conventional.by.Country.ALL<-rbind(Conventional.by.Country.ALL, cbind(WAVE[k], Conventional.by.Country))
Agitator.by.Country.ALL<-rbind(Agitator.by.Country.ALL, cbind(WAVE[k], Agitator.by.Country))




# Marginal effects
dummies<-c(3, 6)
continuous<-c(2,4,5,8)
ordinal<-7


Marginal.Individual<-matrix(0, nrow=4*(length(dummies)+length(continuous)+length(ordinal)), 3)
colnames(Marginal.Individual)<-c("Mean", "95.Low", "95.High")
rownames(Marginal.Individual)<-paste(rep(colnames(X)[sort(c(dummies, continuous, ordinal))], each=4),
rep(c("Outsider", "Agitator", "Conventional", "Activist"), (length(dummies)+length(continuous)+length(ordinal))), 
sep="-")

for (j in 1:length(dummies)) {
X.0<-X
X.1<-X

X.0[,dummies[j]]<-0
X.1[,dummies[j]]<-1


prob.Agitator.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Outsider.0<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Agitator.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))


prob.Outsider.1<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))


Marginal.Individual[grep(colnames(X)[dummies[j]], rownames(Marginal.Individual))[1],]<-c(mean(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0)),
											quantile(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0), c(0.025,0.975)))

Marginal.Individual[grep(colnames(X)[dummies[j]], rownames(Marginal.Individual))[2],]<-c(mean(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0)), quantile(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0), c(0.025,0.975)))


Marginal.Individual[grep(colnames(X)[dummies[j]], rownames(Marginal.Individual))[3],]<-c(mean(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0)), quantile(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0), c(0.025,0.975)))


Marginal.Individual[grep(colnames(X)[dummies[j]], rownames(Marginal.Individual))[4],]<-c(mean(colMeans(prob.Activist.1)-colMeans(prob.Activist.0)), quantile(colMeans(prob.Activist.1)-colMeans(prob.Activist.0), c(0.025,0.975)))

}



for (j in 1:length(continuous)){
X.0<-X
X.1<-X
X.1[,continuous[j]]<-X.0[,continuous[j]]+sd(X.0[,continuous[j]])


prob.Agitator.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Outsider.0<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Agitator.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))


prob.Outsider.1<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))



Marginal.Individual[grep(colnames(X)[continuous[j]], rownames(Marginal.Individual))[1],]<-c(mean(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0)),
											quantile(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0), c(0.025,0.975)))

Marginal.Individual[grep(colnames(X)[continuous[j]], rownames(Marginal.Individual))[2],]<-c(mean(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0)), quantile(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0), c(0.025,0.975)))


Marginal.Individual[grep(colnames(X)[continuous[j]], rownames(Marginal.Individual))[3],]<-c(mean(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0)), quantile(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0), c(0.025,0.975)))


Marginal.Individual[grep(colnames(X)[continuous[j]], rownames(Marginal.Individual))[4],]<-c(mean(colMeans(prob.Activist.1)-colMeans(prob.Activist.0)), quantile(colMeans(prob.Activist.1)-colMeans(prob.Activist.0), c(0.025,0.975)))

}

for (j in 1:length(ordinal)){
X.0<-X
X.1<-X
X.1[,ordinal[j]]<-X.0[,ordinal[j]]+1


prob.Agitator.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Outsider.0<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Agitator.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))


prob.Outsider.1<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))



Marginal.Individual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Individual))[1],]<-c(mean(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0)),
											quantile(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0), c(0.025,0.975)))

Marginal.Individual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Individual))[2],]<-c(mean(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0)), quantile(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0), c(0.025,0.975)))


Marginal.Individual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Individual))[3],]<-c(mean(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0)), quantile(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0), c(0.025,0.975)))


Marginal.Individual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Individual))[4],]<-c(mean(colMeans(prob.Activist.1)-colMeans(prob.Activist.0)), quantile(colMeans(prob.Activist.1)-colMeans(prob.Activist.0), c(0.025,0.975)))

}


Marginal.Individual[,1:3]<-100*Marginal.Individual[,1:3]
Marginal.Individual[grep("Ideology", rownames(Marginal.Individual)),1:3]<-(Marginal.Individual[grep("Ideology", rownames(Marginal.Individual)),1:3]/sd(X[,grep("Ideology", colnames(X))]))





continuous<-c(9, 11:13)
ordinal<-10


Marginal.Contextual<-matrix(0, nrow=4*(length(continuous)+length(ordinal)), 3)
colnames(Marginal.Contextual)<-c("Mean", "95.Low", "95.High")
rownames(Marginal.Contextual)<-paste(rep(colnames(X)[sort(c(continuous, ordinal))], each=4),
rep(c("Outsider", "Agitator", "Conventional", "Activist"), (length(continuous)+length(ordinal))), 
sep="-")



for (j in 1:length(continuous)){
X.0<-X
X.1<-X
X.1[,continuous[j]]<-X.0[,continuous[j]]+sd(X.0[,continuous[j]])


prob.Agitator.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Outsider.0<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Agitator.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))


prob.Outsider.1<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))



Marginal.Contextual[grep(colnames(X)[continuous[j]], rownames(Marginal.Contextual))[1],]<-c(mean(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0)),
											quantile(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0), c(0.025,0.975)))

Marginal.Contextual[grep(colnames(X)[continuous[j]], rownames(Marginal.Contextual))[2],]<-c(mean(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0)), quantile(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0), c(0.025,0.975)))


Marginal.Contextual[grep(colnames(X)[continuous[j]], rownames(Marginal.Contextual))[3],]<-c(mean(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0)), quantile(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0), c(0.025,0.975)))


Marginal.Contextual[grep(colnames(X)[continuous[j]], rownames(Marginal.Contextual))[4],]<-c(mean(colMeans(prob.Activist.1)-colMeans(prob.Activist.0)), quantile(colMeans(prob.Activist.1)-colMeans(prob.Activist.0), c(0.025,0.975)))

}

for (j in 1:length(ordinal)){
X.0<-X
X.1<-X
X.1[,ordinal[j]]<-X.0[,ordinal[j]]+1


prob.Agitator.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.0<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Outsider.0<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.0%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.0%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.0%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Agitator.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Conventional.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))

prob.Activist.1<-sapply(1:dim(Beta.Agitator)[1], function(s) exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))


prob.Outsider.1<-sapply(1:dim(Beta.Agitator)[1], function(s) 1/(1+exp(X.1%*%(Beta.Agitator[s,])+(Nu.Agitator[s,])[Country])+
			exp(X.1%*%(Beta.Conventional[s,])+(Nu.Conventional[s,])[Country])+exp(X.1%*%(Beta.Activist[s,])+(Nu.Activist[s,])[Country])))



Marginal.Contextual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Contextual))[1],]<-c(mean(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0)),
											quantile(colMeans(prob.Outsider.1)-colMeans(prob.Outsider.0), c(0.025,0.975)))

Marginal.Contextual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Contextual))[2],]<-c(mean(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0)), quantile(colMeans(prob.Agitator.1)-colMeans(prob.Agitator.0), c(0.025,0.975)))


Marginal.Contextual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Contextual))[3],]<-c(mean(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0)), quantile(colMeans(prob.Conventional.1)-colMeans(prob.Conventional.0), c(0.025,0.975)))


Marginal.Contextual[grep(colnames(X)[ordinal[j]], rownames(Marginal.Contextual))[4],]<-c(mean(colMeans(prob.Activist.1)-colMeans(prob.Activist.0)), quantile(colMeans(prob.Activist.1)-colMeans(prob.Activist.0), c(0.025,0.975)))

}


Marginal.Contextual[,1:3]<-100*Marginal.Contextual[,1:3]




Marginal.Individual.ALL<-rbind(Marginal.Individual.ALL, cbind(WAVE[k], Marginal.Individual))
Marginal.Contextual.ALL<-rbind(Marginal.Contextual.ALL, cbind(WAVE[k], Marginal.Contextual))



NU.ACTIVIST<-cbind(1:7,  colMeans(Nu.Activist), t(apply(Nu.Activist,2,quantile, c(0.025, 0.975))))
NU.AGITATOR<-cbind(1:7, colMeans(Nu.Agitator), t(apply(Nu.Agitator,2,quantile, c(0.025, 0.975))))
NU.CONVENTIONAL<-cbind(1:7, colMeans(Nu.Conventional), t(apply(Nu.Conventional,2,quantile, c(0.025, 0.975))))



NU.ACTIVIST.ALL<-rbind(NU.ACTIVIST.ALL, cbind(WAVE[k], NU.ACTIVIST))
NU.AGITATOR.ALL<-rbind(NU.AGITATOR.ALL, cbind(WAVE[k], NU.AGITATOR))
NU.CONVENTIONAL.ALL<-rbind(NU.CONVENTIONAL.ALL, cbind(WAVE[k], NU.CONVENTIONAL))



}


save(Outsider.by.Country.ALL, file="Outsider.by.Country.ALL")
save(Activist.by.Country.ALL, file="Activist.by.Country.ALL")
save(Agitator.by.Country.ALL, file="Agitator.by.Country.ALL")
save(Conventional.by.Country.ALL, file="Conventional.by.Country.ALL")
save(NU.ACTIVIST.ALL,file="NU.ACTIVIST.ALL")
save(NU.AGITATOR.ALL, file="NU.AGITATOR.ALL")
save(NU.CONVENTIONAL.ALL, file="NU.CONVENTIONAL.ALL")
save(Marginal.Individual.ALL, file="Marginal.Individual.ALL")
save(Marginal.Contextual.ALL, file="Marginal.Contextual.ALL")





Types<-data.frame(Types)
names(Types)<-c("Wave", "Type", "Mean", "Low", "High")

for (k in 3:ncol(Types)) {
Types[,k]<- as.numeric(levels(Types[,k]))[Types[,k]] 
}




Types$Type<- factor(Types$Type, 
	levels = c("Outsider", "Agitator", "Conventional",
		"Activist"))


p<- ggplot(Types, aes(x=Wave, y=Mean)) + 
  geom_point(size=2, fill="black") + ylab("Probability of type assignment")+
	xlab("")+
  geom_errorbar(aes(ymin=Low, ymax=High), width=.1,
                 position=position_dodge(0.05))+
	facet_grid(~Type)+theme_bw()+
theme(axis.text.x = element_text(color="black", size=8))+
theme(axis.title.y = element_text(size=11, face="bold"))+
theme(strip.text.x=element_text(size=12, face="bold"))


pdf(file = "Figure.A.29.pdf")
grid.draw(p)
dev.off()


################################################# Figure A.30 ##################################################################################

rm(list=ls())
load("Outsider.by.Country.ALL")
load("Conventional.by.Country.ALL")
load("Agitator.by.Country.ALL")
load("Activist.by.Country.ALL")

# Multiple plot function
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



df.outsiders<-Outsider.by.Country.ALL[Outsider.by.Country.ALL[,1]==2010,-1]
df.conventionals<-Conventional.by.Country.ALL[Conventional.by.Country.ALL[,1]==2010,-1]
df.agitators<-Agitator.by.Country.ALL[Agitator.by.Country.ALL[,1]==2010,-1]
df.activists<-Activist.by.Country.ALL[Agitator.by.Country.ALL[,1]==2010,-1]



df.outsiders<-data.frame(cbind("Outsiders", df.outsiders))
names(df.outsiders)<-c("Type", "Country", "Mean", "Low", "High")
 
df.agitators<-data.frame(cbind("Agitators", df.agitators))
names(df.agitators)<-c("Type", "Country", "Mean", "Low", "High")
 
df.conventionals<-data.frame(cbind("Conventionals", df.conventionals))
names(df.conventionals)<-c("Type", "Country", "Mean", "Low", "High")

df.activists<-data.frame(cbind("Activists", df.activists))
names(df.activists)<-c("Type", "Country", "Mean", "Low", "High")
 


df<-rbind(df.outsiders,	df.agitators,
	 df.conventionals,
	 df.activists)


df$abbr<-NA
df$abbr[df$Country==1]<-"DOM"
df$abbr[df$Country==2]<-"ESV"
df$abbr[df$Country==3]<-"HND"
df$abbr[df$Country==4]<-"MEX"
df$abbr[df$Country==5]<-"NIC"
df$abbr[df$Country==6]<-"PAR"
df$abbr[df$Country==7]<-"PER"


Outsiders<-df[df$Type=="Outsiders",-c(1,2)]
Agitators<-df[df$Type=="Agitators",-c(1,2)]
Conventionals<-df[df$Type=="Conventionals",-c(1,2)]
Activists<-df[df$Type=="Activists",-c(1,2)]


for (k in 1:3) {
Outsiders[,k]<-as.numeric(levels(Outsiders[,k]))[Outsiders[,k]]
Agitators[,k]<-as.numeric(levels(Agitators[,k]))[Agitators[,k]]
Conventionals[,k]<-as.numeric(levels(Conventionals[,k]))[Conventionals[,k]]
Activists[,k]<-as.numeric(levels(Activists[,k]))[Activists[,k]]
}


p.1<-ggplot(Outsiders,  aes(as.factor(abbr), Mean)) +
 geom_point(size=1, fill="black") +
  geom_errorbar(aes(ymin=Low, ymax=High), width=.1,
                 position=position_dodge(0.05))+
	ylab("")+xlab("")+ggtitle("Outsiders")+theme_bw()+
	theme(plot.margin=unit(c(0.5,1,0,0), "lines"))+
	theme(plot.title = element_text(hjust = 0))


p.2<-ggplot(Agitators,  aes(as.factor(abbr), Mean)) +
 geom_point(size=1, fill="black") +
  geom_errorbar(aes(ymin=Low, ymax=High), width=.1,
                 position=position_dodge(0.05))+
	ylab("")+xlab("")+ggtitle("Agitators")+theme_bw()+
	theme(plot.margin=unit(c(0.5,1,0,0), "lines"))+
	theme(plot.title = element_text(hjust = 0))


p.3<-ggplot(Conventionals,  aes(as.factor(abbr), Mean)) +
 geom_point(size=1, fill="black") +
  geom_errorbar(aes(ymin=Low, ymax=High), width=.1,
                 position=position_dodge(0.05))+
	ylab("")+xlab("")+ggtitle("Conventionals")+theme_bw()+
	scale_y_continuous(breaks=c(0.15, 0.175, 0.20, 0.225))+
	theme(plot.margin=unit(c(0.5,1,0,0), "lines"))+
	theme(plot.title = element_text(hjust = 0))



p.4<-ggplot(Activists,  aes(as.factor(abbr), Mean)) +
 geom_point(size=1, fill="black") +
  geom_errorbar(aes(ymin=Low, ymax=High), width=.1,
                 position=position_dodge(0.05))+
	ylab("")+xlab("")+ggtitle("Activists")+theme_bw()+
	theme(plot.margin=unit(c(0.5,1,0,0), "lines"))+
	theme(plot.title = element_text(hjust = 0))




pdf(file = "Figure.A.30.pdf")
grid.draw(multiplot(p.1, p.2, p.3, p.4, cols=1 ))
dev.off()


################################################# Figure A.31 ##################################################################################

df.outsiders<-Outsider.by.Country.ALL[Outsider.by.Country.ALL[,1]==2012,-1]
df.conventionals<-Conventional.by.Country.ALL[Conventional.by.Country.ALL[,1]==2012,-1]
df.agitators<-Agitator.by.Country.ALL[Agitator.by.Country.ALL[,1]==2012,-1]
df.activists<-Activist.by.Country.ALL[Agitator.by.Country.ALL[,1]==2012,-1]



df.outsiders<-data.frame(cbind("Outsiders", df.outsiders))
names(df.outsiders)<-c("Type", "Country", "Mean", "Low", "High")
 
df.agitators<-data.frame(cbind("Agitators", df.agitators))
names(df.agitators)<-c("Type", "Country", "Mean", "Low", "High")
 
df.conventionals<-data.frame(cbind("Conventionals", df.conventionals))
names(df.conventionals)<-c("Type", "Country", "Mean", "Low", "High")

df.activists<-data.frame(cbind("Activists", df.activists))
names(df.activists)<-c("Type", "Country", "Mean", "Low", "High")
 


df<-rbind(df.outsiders,	df.agitators,
	 df.conventionals,
	 df.activists)


df$abbr<-NA
df$abbr[df$Country==1]<-"DOM"
df$abbr[df$Country==2]<-"ESV"
df$abbr[df$Country==3]<-"HND"
df$abbr[df$Country==4]<-"MEX"
df$abbr[df$Country==5]<-"NIC"
df$abbr[df$Country==6]<-"PAR"
df$abbr[df$Country==7]<-"PER"


Outsiders<-df[df$Type=="Outsiders",-c(1,2)]
Agitators<-df[df$Type=="Agitators",-c(1,2)]
Conventionals<-df[df$Type=="Conventionals",-c(1,2)]
Activists<-df[df$Type=="Activists",-c(1,2)]


for (k in 1:3) {
Outsiders[,k]<-as.numeric(levels(Outsiders[,k]))[Outsiders[,k]]
Agitators[,k]<-as.numeric(levels(Agitators[,k]))[Agitators[,k]]
Conventionals[,k]<-as.numeric(levels(Conventionals[,k]))[Conventionals[,k]]
Activists[,k]<-as.numeric(levels(Activists[,k]))[Activists[,k]]
}


p.1<-ggplot(Outsiders,  aes(as.factor(abbr), Mean)) +
 geom_point(size=1, fill="black") +
  geom_errorbar(aes(ymin=Low, ymax=High), width=.1,
                 position=position_dodge(0.05))+
	ylab("")+xlab("")+ggtitle("Outsiders")+theme_bw()+
	theme(plot.margin=unit(c(0.5,1,0,0), "lines"))+
        scale_y_continuous(breaks=c(0.60, 0.64, 0.68))+
	theme(plot.title = element_text(hjust = 0))


p.2<-ggplot(Agitators,  aes(as.factor(abbr), Mean)) +
 geom_point(size=1, fill="black") +
  geom_errorbar(aes(ymin=Low, ymax=High), width=.1,
                 position=position_dodge(0.05))+
	ylab("")+xlab("")+ggtitle("Agitators")+theme_bw()+
	theme(plot.margin=unit(c(0.5,1,0,0), "lines"))+
	theme(plot.title = element_text(hjust = 0))


p.3<-ggplot(Conventionals,  aes(as.factor(abbr), Mean)) +
 geom_point(size=1, fill="black") +
  geom_errorbar(aes(ymin=Low, ymax=High), width=.1,
                 position=position_dodge(0.05))+
	ylab("")+xlab("")+ggtitle("Conventionals")+theme_bw()+
	#scale_y_continuous(breaks=c(0.15, 0.175, 0.20, 0.225))+
	theme(plot.margin=unit(c(0.5,1,0,0), "lines"))+
        scale_y_continuous(breaks=c(0.10, 0.125, 0.15, 0.175, 0.20))+
	theme(plot.title = element_text(hjust = 0))



p.4<-ggplot(Activists,  aes(as.factor(abbr), Mean)) +
 geom_point(size=1, fill="black") +
  geom_errorbar(aes(ymin=Low, ymax=High), width=.1,
                 position=position_dodge(0.05))+
	ylab("")+xlab("")+ggtitle("Activists")+theme_bw()+
	theme(plot.margin=unit(c(0.5,1,0,0), "lines"))+
	theme(plot.title = element_text(hjust = 0))




pdf(file = "Figure.A.31.pdf")
grid.draw(multiplot(p.1, p.2, p.3, p.4, cols=1 ))
dev.off()

################################################# Figure A.32 ##################################################################################


df.outsiders<-Outsider.by.Country.ALL[Outsider.by.Country.ALL[,1]==2014,-1]
df.conventionals<-Conventional.by.Country.ALL[Conventional.by.Country.ALL[,1]==2014,-1]
df.agitators<-Agitator.by.Country.ALL[Agitator.by.Country.ALL[,1]==2014,-1]
df.activists<-Activist.by.Country.ALL[Agitator.by.Country.ALL[,1]==2014,-1]



df.outsiders<-data.frame(cbind("Outsiders", df.outsiders))
names(df.outsiders)<-c("Type", "Country", "Mean", "Low", "High")
 
df.agitators<-data.frame(cbind("Agitators", df.agitators))
names(df.agitators)<-c("Type", "Country", "Mean", "Low", "High")
 
df.conventionals<-data.frame(cbind("Conventionals", df.conventionals))
names(df.conventionals)<-c("Type", "Country", "Mean", "Low", "High")

df.activists<-data.frame(cbind("Activists", df.activists))
names(df.activists)<-c("Type", "Country", "Mean", "Low", "High")
 


df<-rbind(df.outsiders,	df.agitators,
	 df.conventionals,
	 df.activists)


df$abbr<-NA
df$abbr[df$Country==1]<-"DOM"
df$abbr[df$Country==2]<-"ESV"
df$abbr[df$Country==3]<-"HND"
df$abbr[df$Country==4]<-"MEX"
df$abbr[df$Country==5]<-"NIC"
df$abbr[df$Country==6]<-"PAR"
df$abbr[df$Country==7]<-"PER"


Outsiders<-df[df$Type=="Outsiders",-c(1,2)]
Agitators<-df[df$Type=="Agitators",-c(1,2)]
Conventionals<-df[df$Type=="Conventionals",-c(1,2)]
Activists<-df[df$Type=="Activists",-c(1,2)]


for (k in 1:3) {
Outsiders[,k]<-as.numeric(levels(Outsiders[,k]))[Outsiders[,k]]
Agitators[,k]<-as.numeric(levels(Agitators[,k]))[Agitators[,k]]
Conventionals[,k]<-as.numeric(levels(Conventionals[,k]))[Conventionals[,k]]
Activists[,k]<-as.numeric(levels(Activists[,k]))[Activists[,k]]
}


p.1<-ggplot(Outsiders,  aes(as.factor(abbr), Mean)) +
 geom_point(size=1, fill="black") +
  geom_errorbar(aes(ymin=Low, ymax=High), width=.1,
                 position=position_dodge(0.05))+
	ylab("")+xlab("")+ggtitle("Outsiders")+theme_bw()+
	theme(plot.margin=unit(c(0.5,1,0,0), "lines"))+
        scale_y_continuous(breaks=c(0.60, 0.64, 0.68))+
	theme(plot.title = element_text(hjust = 0))


p.2<-ggplot(Agitators,  aes(as.factor(abbr), Mean)) +
 geom_point(size=1, fill="black") +
  geom_errorbar(aes(ymin=Low, ymax=High), width=.1,
                 position=position_dodge(0.05))+
	ylab("")+xlab("")+ggtitle("Agitators")+theme_bw()+
	theme(plot.margin=unit(c(0.5,1,0,0), "lines"))+
        scale_y_continuous(breaks=c(0.10, 0.15, 0.20))+
	theme(plot.title = element_text(hjust = 0))


p.3<-ggplot(Conventionals,  aes(as.factor(abbr), Mean)) +
 geom_point(size=1, fill="black") +
  geom_errorbar(aes(ymin=Low, ymax=High), width=.1,
                 position=position_dodge(0.05))+
	ylab("")+xlab("")+ggtitle("Conventionals")+theme_bw()+
	#scale_y_continuous(breaks=c(0.15, 0.175, 0.20, 0.225))+
	theme(plot.margin=unit(c(0.5,1,0,0), "lines"))+
        scale_y_continuous(breaks=c(0.10, 0.125, 0.15, 0.175, 0.20, 0.225))+
	theme(plot.title = element_text(hjust = 0))



p.4<-ggplot(Activists,  aes(as.factor(abbr), Mean)) +
 geom_point(size=1, fill="black") +
  geom_errorbar(aes(ymin=Low, ymax=High), width=.1,
                 position=position_dodge(0.05))+
	ylab("")+xlab("")+ggtitle("Activists")+theme_bw()+
	theme(plot.margin=unit(c(0.5,1,0,0), "lines"))+
        scale_y_continuous(breaks=c(0.03, 0.04, 0.05, 0.06))+
	theme(plot.title = element_text(hjust = 0))




pdf(file = "Figure.A.32.pdf")
grid.draw(multiplot(p.1, p.2, p.3, p.4, cols=1 ))
dev.off()

################################################# Figure A.33 ##################################################################################


df.outsiders<-Outsider.by.Country.ALL[Outsider.by.Country.ALL[,1]==2016,-1]
df.conventionals<-Conventional.by.Country.ALL[Conventional.by.Country.ALL[,1]==2016,-1]
df.agitators<-Agitator.by.Country.ALL[Agitator.by.Country.ALL[,1]==2016,-1]
df.activists<-Activist.by.Country.ALL[Agitator.by.Country.ALL[,1]==2016,-1]



df.outsiders<-data.frame(cbind("Outsiders", df.outsiders))
names(df.outsiders)<-c("Type", "Country", "Mean", "Low", "High")
 
df.agitators<-data.frame(cbind("Agitators", df.agitators))
names(df.agitators)<-c("Type", "Country", "Mean", "Low", "High")
 
df.conventionals<-data.frame(cbind("Conventionals", df.conventionals))
names(df.conventionals)<-c("Type", "Country", "Mean", "Low", "High")

df.activists<-data.frame(cbind("Activists", df.activists))
names(df.activists)<-c("Type", "Country", "Mean", "Low", "High")
 


df<-rbind(df.outsiders,	df.agitators,
	 df.conventionals,
	 df.activists)


df$abbr<-NA
df$abbr[df$Country==1]<-"DOM"
df$abbr[df$Country==2]<-"ESV"
df$abbr[df$Country==3]<-"HND"
df$abbr[df$Country==4]<-"MEX"
df$abbr[df$Country==5]<-"NIC"
df$abbr[df$Country==6]<-"PAR"
df$abbr[df$Country==7]<-"PER"


Outsiders<-df[df$Type=="Outsiders",-c(1,2)]
Agitators<-df[df$Type=="Agitators",-c(1,2)]
Conventionals<-df[df$Type=="Conventionals",-c(1,2)]
Activists<-df[df$Type=="Activists",-c(1,2)]


for (k in 1:3) {
Outsiders[,k]<-as.numeric(levels(Outsiders[,k]))[Outsiders[,k]]
Agitators[,k]<-as.numeric(levels(Agitators[,k]))[Agitators[,k]]
Conventionals[,k]<-as.numeric(levels(Conventionals[,k]))[Conventionals[,k]]
Activists[,k]<-as.numeric(levels(Activists[,k]))[Activists[,k]]
}


p.1<-ggplot(Outsiders,  aes(as.factor(abbr), Mean)) +
 geom_point(size=1, fill="black") +
  geom_errorbar(aes(ymin=Low, ymax=High), width=.1,
                 position=position_dodge(0.05))+
	ylab("")+xlab("")+ggtitle("Outsiders")+theme_bw()+
	theme(plot.margin=unit(c(0.5,1,0,0), "lines"))+
        scale_y_continuous(breaks=c(0.52, 0.56, 0.60))+
	theme(plot.title = element_text(hjust = 0))


p.2<-ggplot(Agitators,  aes(as.factor(abbr), Mean)) +
 geom_point(size=1, fill="black") +
  geom_errorbar(aes(ymin=Low, ymax=High), width=.1,
                 position=position_dodge(0.05))+
	ylab("")+xlab("")+ggtitle("Agitators")+theme_bw()+
	theme(plot.margin=unit(c(0.5,1,0,0), "lines"))+
        scale_y_continuous(breaks=c(0.16, 0.20, 0.24))+
	theme(plot.title = element_text(hjust = 0))


p.3<-ggplot(Conventionals,  aes(as.factor(abbr), Mean)) +
 geom_point(size=1, fill="black") +
  geom_errorbar(aes(ymin=Low, ymax=High), width=.1,
                 position=position_dodge(0.05))+
	ylab("")+xlab("")+ggtitle("Conventionals")+theme_bw()+
	#scale_y_continuous(breaks=c(0.15, 0.175, 0.20, 0.225))+
	theme(plot.margin=unit(c(0.5,1,0,0), "lines"))+
        scale_y_continuous(breaks=c(0.12, 0.15, 0.18, 0.21))+
	theme(plot.title = element_text(hjust = 0))



p.4<-ggplot(Activists,  aes(as.factor(abbr), Mean)) +
 geom_point(size=1, fill="black") +
  geom_errorbar(aes(ymin=Low, ymax=High), width=.1,
                 position=position_dodge(0.05))+
	ylab("")+xlab("")+ggtitle("Activists")+theme_bw()+
	theme(plot.margin=unit(c(0.5,1,0,0), "lines"))+
        scale_y_continuous(breaks=c(0.06, 0.08, 0.10))+
	theme(plot.title = element_text(hjust = 0))




pdf(file = "Figure.A.33.pdf")
grid.draw(multiplot(p.1, p.2, p.3, p.4, cols=1 ))
dev.off()


################################################# Figure A.34 ##################################################################################


df.outsiders<-Outsider.by.Country.ALL[Outsider.by.Country.ALL[,1]==2018,-1]
df.conventionals<-Conventional.by.Country.ALL[Conventional.by.Country.ALL[,1]==2018,-1]
df.agitators<-Agitator.by.Country.ALL[Agitator.by.Country.ALL[,1]==2018,-1]
df.activists<-Activist.by.Country.ALL[Agitator.by.Country.ALL[,1]==2018,-1]



df.outsiders<-data.frame(cbind("Outsiders", df.outsiders))
names(df.outsiders)<-c("Type", "Country", "Mean", "Low", "High")
 
df.agitators<-data.frame(cbind("Agitators", df.agitators))
names(df.agitators)<-c("Type", "Country", "Mean", "Low", "High")
 
df.conventionals<-data.frame(cbind("Conventionals", df.conventionals))
names(df.conventionals)<-c("Type", "Country", "Mean", "Low", "High")

df.activists<-data.frame(cbind("Activists", df.activists))
names(df.activists)<-c("Type", "Country", "Mean", "Low", "High")
 


df<-rbind(df.outsiders,	df.agitators,
	 df.conventionals,
	 df.activists)


df$abbr<-NA
df$abbr[df$Country==1]<-"DOM"
df$abbr[df$Country==2]<-"ESV"
df$abbr[df$Country==3]<-"HND"
df$abbr[df$Country==4]<-"MEX"
df$abbr[df$Country==5]<-"NIC"
df$abbr[df$Country==6]<-"PAR"
df$abbr[df$Country==7]<-"PER"


Outsiders<-df[df$Type=="Outsiders",-c(1,2)]
Agitators<-df[df$Type=="Agitators",-c(1,2)]
Conventionals<-df[df$Type=="Conventionals",-c(1,2)]
Activists<-df[df$Type=="Activists",-c(1,2)]


for (k in 1:3) {
Outsiders[,k]<-as.numeric(levels(Outsiders[,k]))[Outsiders[,k]]
Agitators[,k]<-as.numeric(levels(Agitators[,k]))[Agitators[,k]]
Conventionals[,k]<-as.numeric(levels(Conventionals[,k]))[Conventionals[,k]]
Activists[,k]<-as.numeric(levels(Activists[,k]))[Activists[,k]]
}


p.1<-ggplot(Outsiders,  aes(as.factor(abbr), Mean)) +
 geom_point(size=1, fill="black") +
  geom_errorbar(aes(ymin=Low, ymax=High), width=.1,
                 position=position_dodge(0.05))+
	ylab("")+xlab("")+ggtitle("Outsiders")+theme_bw()+
	theme(plot.margin=unit(c(0.5,1,0,0), "lines"))+
        #scale_y_continuous(limits=c(0.55, 0.70), breaks=c(0.60, 0.65, 0.70))+
	theme(plot.title = element_text(hjust = 0))


p.2<-ggplot(Agitators,  aes(as.factor(abbr), Mean)) +
 geom_point(size=1, fill="black") +
  geom_errorbar(aes(ymin=Low, ymax=High), width=.1,
                 position=position_dodge(0.05))+
	ylab("")+xlab("")+ggtitle("Agitators")+theme_bw()+
	theme(plot.margin=unit(c(0.5,1,0,0), "lines"))+
        scale_y_continuous(breaks=c(0.10, 0.125, 0.15, 0.175))+
	theme(plot.title = element_text(hjust = 0))


p.3<-ggplot(Conventionals,  aes(as.factor(abbr), Mean)) +
 geom_point(size=1, fill="black") +
  geom_errorbar(aes(ymin=Low, ymax=High), width=.1,
                 position=position_dodge(0.05))+
	ylab("")+xlab("")+ggtitle("Conventionals")+theme_bw()+
	#scale_y_continuous(breaks=c(0.15, 0.175, 0.20, 0.225))+
	theme(plot.margin=unit(c(0.5,1,0,0), "lines"))+
        scale_y_continuous(breaks=c(0.14, 0.16, 0.18, 0.20, 0.22, 0.24))+
	theme(plot.title = element_text(hjust = 0))



p.4<-ggplot(Activists,  aes(as.factor(abbr), Mean)) +
 geom_point(size=1, fill="black") +
  geom_errorbar(aes(ymin=Low, ymax=High), width=.1,
                 position=position_dodge(0.05))+
	ylab("")+xlab("")+ggtitle("Activists")+theme_bw()+
	theme(plot.margin=unit(c(0.5,1,0,0), "lines"))+
        scale_y_continuous(breaks=c(0.03, 0.04, 0.05, 0.06, 0.07))+
	theme(plot.title = element_text(hjust = 0))




pdf(file = "Figure.A.34.pdf")
grid.draw(multiplot(p.1, p.2, p.3, p.4, cols=1 ))
dev.off()


################################################# Figure A.35 ##################################################################################

rm(list=ls())
load("Marginal.Individual.ALL")
load("Marginal.Contextual.ALL")


individual<-Marginal.Individual.ALL[Marginal.Individual.ALL[,1]==2010,-1]
contextual<-Marginal.Contextual.ALL[Marginal.Contextual.ALL[,1]==2010,-1]


Figure.Individual<-cbind(unlist(strsplit(rownames(individual), "-"))[seq(2,length(unlist(strsplit(rownames(individual), "-"))), by=2)], 
	unlist(strsplit(rownames(individual), "-"))[seq(1,length(unlist(strsplit(rownames(individual), "-"))), by=2)],
	individual[,1:3])
rownames(Figure.Individual)<-c()
Figure.Individual<-data.frame(Figure.Individual)
names(Figure.Individual)<-c("Type", "Variable", "Mean", "Low", "High")

Figure.Contextual<-cbind(unlist(strsplit(rownames(contextual), "-"))[seq(2,length(unlist(strsplit(rownames(contextual), "-"))), by=2)], 
	unlist(strsplit(rownames(contextual), "-"))[seq(1,length(unlist(strsplit(rownames(contextual), "-"))), by=2)],
	contextual[,1:3])
rownames(Figure.Contextual)<-c()
Figure.Contextual<-data.frame(Figure.Contextual)
names(Figure.Contextual)<-c("Type", "Variable", "Mean", "Low", "High")


Figure<-rbind(Figure.Individual, Figure.Contextual)



for (k in 3:5) {
Figure[,k]<-as.numeric(levels(Figure[,k]))[Figure[,k]]
}
 

Figure$Type<- factor(Figure$Type, 
	levels = c("Outsider",
		"Agitator",
		"Conventional", "Activist"))



Figure$Variable<-factor(Figure$Variable, 
	levels=rev(c("Age", "Female", "Education", 
			"Income", "Corruption", 
			"Victimization",
			"Ideology",
"Rule of Law", "Compulsory Voting", "ENPP", 
			"GDP", "Social.Spending")),
       labels=rev(c("Age",
			  "Female",
			  "Education", 
			  "Relative Income",
			  "Perceived Corruption", 
			   "Crime Victimization",
			 "Ideological Distance to Incumbent",
"Rule of Law",
			  "Compulsory Voting",
			  "ENPP", 
			  "GDP per capita",
			  "Social Spending")))



p<-ggplot(Figure,
 aes(y=Variable, x=Mean)) + 
    geom_errorbarh(aes(xmin=Low, 
	xmax=High, height=.2), colour="black") + facet_wrap(~Type)+
    geom_point(size=1.5)+
theme_bw()+xlab("Marginal effects (in percentage points)")+ylab("")+
geom_hline(yintercept=0, linetype="longdash", colour="gray40")+
theme(strip.text.x = element_text(size = 12, face="bold"))+
geom_vline(xintercept=0, linetype="longdash", colour="gray40")+
theme(strip.background = element_rect(fill = alpha('white'))) +
theme(axis.title.x=element_text(vjust=2, size=11))+
theme(axis.text.y=element_text(size=11))+
scale_x_continuous(breaks=c(-10,-5,0,5,10))



pdf(file = "Figure.A.35.pdf")
grid.draw(p)
dev.off()

################################################# Figure A.36 ##################################################################################

individual<-Marginal.Individual.ALL[Marginal.Individual.ALL[,1]==2012,-1]
contextual<-Marginal.Contextual.ALL[Marginal.Contextual.ALL[,1]==2012,-1]


Figure.Individual<-cbind(unlist(strsplit(rownames(individual), "-"))[seq(2,length(unlist(strsplit(rownames(individual), "-"))), by=2)], 
	unlist(strsplit(rownames(individual), "-"))[seq(1,length(unlist(strsplit(rownames(individual), "-"))), by=2)],
	individual[,1:3])
rownames(Figure.Individual)<-c()
Figure.Individual<-data.frame(Figure.Individual)
names(Figure.Individual)<-c("Type", "Variable", "Mean", "Low", "High")

Figure.Contextual<-cbind(unlist(strsplit(rownames(contextual), "-"))[seq(2,length(unlist(strsplit(rownames(contextual), "-"))), by=2)], 
	unlist(strsplit(rownames(contextual), "-"))[seq(1,length(unlist(strsplit(rownames(contextual), "-"))), by=2)],
	contextual[,1:3])
rownames(Figure.Contextual)<-c()
Figure.Contextual<-data.frame(Figure.Contextual)
names(Figure.Contextual)<-c("Type", "Variable", "Mean", "Low", "High")


Figure<-rbind(Figure.Individual, Figure.Contextual)



for (k in 3:5) {
Figure[,k]<-as.numeric(levels(Figure[,k]))[Figure[,k]]
}
 

Figure$Type<- factor(Figure$Type, 
	levels = c("Outsider",
		"Agitator",
		"Conventional", "Activist"))



Figure$Variable<-factor(Figure$Variable, 
	levels=rev(c("Age", "Female", "Education", 
			"Income", "Corruption", 
			"Victimization",
			"Ideology",
"Rule of Law", "Compulsory Voting", "ENPP", 
			"GDP", "Social.Spending")),
       labels=rev(c("Age",
			  "Female",
			  "Education", 
			  "Relative Income",
			  "Perceived Corruption", 
			   "Crime Victimization",
			 "Ideological Distance to Incumbent",
"Rule of Law",
			  "Compulsory Voting",
			  "ENPP", 
			  "GDP per capita",
			  "Social Spending")))



p<-ggplot(Figure,
 aes(y=Variable, x=Mean)) + 
    geom_errorbarh(aes(xmin=Low, 
	xmax=High, height=.2), colour="black") + facet_wrap(~Type)+
    geom_point(size=1.5)+
theme_bw()+xlab("Marginal effects (in percentage points)")+ylab("")+
geom_hline(yintercept=0, linetype="longdash", colour="gray40")+
theme(strip.text.x = element_text(size = 12, face="bold"))+
geom_vline(xintercept=0, linetype="longdash", colour="gray40")+
theme(strip.background = element_rect(fill = alpha('white'))) +
theme(axis.title.x=element_text(vjust=2, size=11))+
theme(axis.text.y=element_text(size=11))+
scale_x_continuous(breaks=c(-10,-5,0,5,10))



pdf(file = "Figure.A.36.pdf")
grid.draw(p)
dev.off()


################################################# Figure A.37 ##################################################################################


individual<-Marginal.Individual.ALL[Marginal.Individual.ALL[,1]==2014,-1]
contextual<-Marginal.Contextual.ALL[Marginal.Contextual.ALL[,1]==2014,-1]


Figure.Individual<-cbind(unlist(strsplit(rownames(individual), "-"))[seq(2,length(unlist(strsplit(rownames(individual), "-"))), by=2)], 
	unlist(strsplit(rownames(individual), "-"))[seq(1,length(unlist(strsplit(rownames(individual), "-"))), by=2)],
	individual[,1:3])
rownames(Figure.Individual)<-c()
Figure.Individual<-data.frame(Figure.Individual)
names(Figure.Individual)<-c("Type", "Variable", "Mean", "Low", "High")

Figure.Contextual<-cbind(unlist(strsplit(rownames(contextual), "-"))[seq(2,length(unlist(strsplit(rownames(contextual), "-"))), by=2)], 
	unlist(strsplit(rownames(contextual), "-"))[seq(1,length(unlist(strsplit(rownames(contextual), "-"))), by=2)],
	contextual[,1:3])
rownames(Figure.Contextual)<-c()
Figure.Contextual<-data.frame(Figure.Contextual)
names(Figure.Contextual)<-c("Type", "Variable", "Mean", "Low", "High")


Figure<-rbind(Figure.Individual, Figure.Contextual)



for (k in 3:5) {
Figure[,k]<-as.numeric(levels(Figure[,k]))[Figure[,k]]
}
 

Figure$Type<- factor(Figure$Type, 
	levels = c("Outsider",
		"Agitator",
		"Conventional", "Activist"))



Figure$Variable<-factor(Figure$Variable, 
	levels=rev(c("Age", "Female", "Education", 
			"Income", "Corruption", 
			"Victimization",
			"Ideology",
"Rule of Law", "Compulsory Voting", "ENPP", 
			"GDP", "Social.Spending")),
       labels=rev(c("Age",
			  "Female",
			  "Education", 
			  "Relative Income",
			  "Perceived Corruption", 
			   "Crime Victimization",
			 "Ideological Distance to Incumbent",
"Rule of Law",
			  "Compulsory Voting",
			  "ENPP", 
			  "GDP per capita",
			  "Social Spending")))



p<-ggplot(Figure,
 aes(y=Variable, x=Mean)) + 
    geom_errorbarh(aes(xmin=Low, 
	xmax=High, height=.2), colour="black") + facet_wrap(~Type)+
    geom_point(size=1.5)+
theme_bw()+xlab("Marginal effects (in percentage points)")+ylab("")+
geom_hline(yintercept=0, linetype="longdash", colour="gray40")+
theme(strip.text.x = element_text(size = 12, face="bold"))+
geom_vline(xintercept=0, linetype="longdash", colour="gray40")+
theme(strip.background = element_rect(fill = alpha('white'))) +
theme(axis.title.x=element_text(vjust=2, size=11))+
theme(axis.text.y=element_text(size=11))+
scale_x_continuous(breaks=c(-20, -10, 0,10,20))



pdf(file = "Figure.A.37.pdf")
grid.draw(p)
dev.off()

################################################# Figure A.38 ##################################################################################

individual<-Marginal.Individual.ALL[Marginal.Individual.ALL[,1]==2016,-1]
contextual<-Marginal.Contextual.ALL[Marginal.Contextual.ALL[,1]==2016,-1]


Figure.Individual<-cbind(unlist(strsplit(rownames(individual), "-"))[seq(2,length(unlist(strsplit(rownames(individual), "-"))), by=2)], 
	unlist(strsplit(rownames(individual), "-"))[seq(1,length(unlist(strsplit(rownames(individual), "-"))), by=2)],
	individual[,1:3])
rownames(Figure.Individual)<-c()
Figure.Individual<-data.frame(Figure.Individual)
names(Figure.Individual)<-c("Type", "Variable", "Mean", "Low", "High")

Figure.Contextual<-cbind(unlist(strsplit(rownames(contextual), "-"))[seq(2,length(unlist(strsplit(rownames(contextual), "-"))), by=2)], 
	unlist(strsplit(rownames(contextual), "-"))[seq(1,length(unlist(strsplit(rownames(contextual), "-"))), by=2)],
	contextual[,1:3])
rownames(Figure.Contextual)<-c()
Figure.Contextual<-data.frame(Figure.Contextual)
names(Figure.Contextual)<-c("Type", "Variable", "Mean", "Low", "High")


Figure<-rbind(Figure.Individual, Figure.Contextual)



for (k in 3:5) {
Figure[,k]<-as.numeric(levels(Figure[,k]))[Figure[,k]]
}
 

Figure$Type<- factor(Figure$Type, 
	levels = c("Outsider",
		"Agitator",
		"Conventional", "Activist"))



Figure$Variable<-factor(Figure$Variable, 
	levels=rev(c("Age", "Female", "Education", 
			"Income", "Corruption", 
			"Victimization",
			"Ideology",
"Rule of Law", "Compulsory Voting", "ENPP", 
			"GDP", "Social.Spending")),
       labels=rev(c("Age",
			  "Female",
			  "Education", 
			  "Relative Income",
			  "Perceived Corruption", 
			   "Crime Victimization",
			 "Ideological Distance to Incumbent",
"Rule of Law",
			  "Compulsory Voting",
			  "ENPP", 
			  "GDP per capita",
			  "Social Spending")))



p<-ggplot(Figure,
 aes(y=Variable, x=Mean)) + 
    geom_errorbarh(aes(xmin=Low, 
	xmax=High, height=.2), colour="black") + facet_wrap(~Type)+
    geom_point(size=1.5)+
theme_bw()+xlab("Marginal effects (in percentage points)")+ylab("")+
geom_hline(yintercept=0, linetype="longdash", colour="gray40")+
theme(strip.text.x = element_text(size = 12, face="bold"))+
geom_vline(xintercept=0, linetype="longdash", colour="gray40")+
theme(strip.background = element_rect(fill = alpha('white'))) +
theme(axis.title.x=element_text(vjust=2, size=11))+
theme(axis.text.y=element_text(size=11))+
scale_x_continuous(breaks=c(-10, -5, 0, 5,10))



pdf(file = "Figure.A.38.pdf")
grid.draw(p)
dev.off()


################################################# Figure A.39 ##################################################################################

individual<-Marginal.Individual.ALL[Marginal.Individual.ALL[,1]==2018,-1]
contextual<-Marginal.Contextual.ALL[Marginal.Contextual.ALL[,1]==2018,-1]


Figure.Individual<-cbind(unlist(strsplit(rownames(individual), "-"))[seq(2,length(unlist(strsplit(rownames(individual), "-"))), by=2)], 
	unlist(strsplit(rownames(individual), "-"))[seq(1,length(unlist(strsplit(rownames(individual), "-"))), by=2)],
	individual[,1:3])
rownames(Figure.Individual)<-c()
Figure.Individual<-data.frame(Figure.Individual)
names(Figure.Individual)<-c("Type", "Variable", "Mean", "Low", "High")

Figure.Contextual<-cbind(unlist(strsplit(rownames(contextual), "-"))[seq(2,length(unlist(strsplit(rownames(contextual), "-"))), by=2)], 
	unlist(strsplit(rownames(contextual), "-"))[seq(1,length(unlist(strsplit(rownames(contextual), "-"))), by=2)],
	contextual[,1:3])
rownames(Figure.Contextual)<-c()
Figure.Contextual<-data.frame(Figure.Contextual)
names(Figure.Contextual)<-c("Type", "Variable", "Mean", "Low", "High")


Figure<-rbind(Figure.Individual, Figure.Contextual)



for (k in 3:5) {
Figure[,k]<-as.numeric(levels(Figure[,k]))[Figure[,k]]
}
 

Figure$Type<- factor(Figure$Type, 
	levels = c("Outsider",
		"Agitator",
		"Conventional", "Activist"))



Figure$Variable<-factor(Figure$Variable, 
	levels=rev(c("Age", "Female", "Education", 
			"Income", "Corruption", 
			"Victimization",
			"Ideology",
"Rule of Law", "Compulsory Voting", "ENPP", 
			"GDP", "Social.Spending")),
       labels=rev(c("Age",
			  "Female",
			  "Education", 
			  "Relative Income",
			  "Perceived Corruption", 
			   "Crime Victimization",
			 "Ideological Distance to Incumbent",
"Rule of Law",
			  "Compulsory Voting",
			  "ENPP", 
			  "GDP per capita",
			  "Social Spending")))



p<-ggplot(Figure,
 aes(y=Variable, x=Mean)) + 
    geom_errorbarh(aes(xmin=Low, 
	xmax=High, height=.2), colour="black") + facet_wrap(~Type)+
    geom_point(size=1.5)+
theme_bw()+xlab("Marginal effects (in percentage points)")+ylab("")+
geom_hline(yintercept=0, linetype="longdash", colour="gray40")+
theme(strip.text.x = element_text(size = 12, face="bold"))+
geom_vline(xintercept=0, linetype="longdash", colour="gray40")+
theme(strip.background = element_rect(fill = alpha('white'))) +
theme(axis.title.x=element_text(vjust=2, size=11))+
theme(axis.text.y=element_text(size=11))+
scale_x_continuous(breaks=c(-10, -5, 0, 5,10))



pdf(file = "Figure.A.39.pdf")
grid.draw(p)
dev.off()


################################################# Figure A.40 ##################################################################################

rm(list=ls())
load("NU.ACTIVIST.ALL")
load("NU.AGITATOR.ALL")
load("NU.CONVENTIONAL.ALL")


Activist<-NU.ACTIVIST.ALL[NU.ACTIVIST.ALL[,1]==2010,-1]
Agitator<-NU.AGITATOR.ALL[NU.AGITATOR.ALL[,1]==2010,-1]
Conventional<-NU.CONVENTIONAL.ALL[NU.CONVENTIONAL.ALL[,1]==2010,-1]


Activist<-data.frame(Activist)
names(Activist)<-c("Country",  "Mean", "Low", "High")
Activist$Country[Activist$Country==1]<-"DOM"
Activist$Country[Activist$Country==2]<-"ESV"
Activist$Country[Activist$Country==3]<-"HND"
Activist$Country[Activist$Country==4]<-"MEX"
Activist$Country[Activist$Country==5]<-"NIC"
Activist$Country[Activist$Country==6]<-"PAR"
Activist$Country[Activist$Country==7]<-"PER"

Agitator<-data.frame(Agitator)
names(Agitator)<-c("Country",  "Mean", "Low", "High")
Agitator$Country[Agitator$Country==1]<-"DOM"
Agitator$Country[Agitator$Country==2]<-"ESV"
Agitator$Country[Agitator$Country==3]<-"HND"
Agitator$Country[Agitator$Country==4]<-"MEX"
Agitator$Country[Agitator$Country==5]<-"NIC"
Agitator$Country[Agitator$Country==6]<-"PAR"
Agitator$Country[Agitator$Country==7]<-"PER"

Conventional<-data.frame(Conventional)
names(Conventional)<-c("Country", "Mean", "Low", "High")
Conventional$Country[Conventional$Country==1]<-"DOM"
Conventional$Country[Conventional$Country==2]<-"ESV"
Conventional$Country[Conventional$Country==3]<-"HND"
Conventional$Country[Conventional$Country==4]<-"MEX"
Conventional$Country[Conventional$Country==5]<-"NIC"
Conventional$Country[Conventional$Country==6]<-"PAR"
Conventional$Country[Conventional$Country==7]<-"PER"



Agitator<-cbind("Agitator vs. Outsider", Agitator)
names(Agitator)[1]<-"Type"

Activist<-cbind("Activist vs. Outsider", Activist)
names(Activist)[1]<-"Type"

Conventional<-cbind("Conventional vs. Outsider", Conventional)
names(Conventional)[1]<-"Type"

df<-rbind(Agitator, Conventional, Activist)



df$Country<-as.factor(df$Country)

df$Country<-factor(df$Country, 
	levels=rev(c("DOM","ESV", "HND", "MEX", "NIC",
			"PAR", "PER")))


p<-ggplot(df,
 aes(y=Country, x=Mean)) + 
    geom_errorbarh(aes(xmin=Low, 
	xmax=High, height=.2), colour="black") + facet_wrap(~Type)+
    geom_point(size=1.5)+
theme_bw()+xlab("")+ylab("")+
geom_hline(yintercept=0, linetype="longdash", colour="gray40")+
theme(strip.text.x = element_text(size = 9, face="bold"))+
geom_vline(xintercept=0, linetype="longdash", colour="gray40")+
theme(strip.background = element_rect(fill = alpha('white'))) +
theme(axis.title.x=element_text(vjust=2, size=10))+
theme(axis.text.y=element_text(size=10))


pdf(file = "Figure.A.40.pdf")
grid.draw(p)
dev.off()

################################################# Figure A.41 ##################################################################################

Activist<-NU.ACTIVIST.ALL[NU.ACTIVIST.ALL[,1]==2012,-1]
Agitator<-NU.AGITATOR.ALL[NU.AGITATOR.ALL[,1]==2012,-1]
Conventional<-NU.CONVENTIONAL.ALL[NU.CONVENTIONAL.ALL[,1]==2012,-1]


Activist<-data.frame(Activist)
names(Activist)<-c("Country",  "Mean", "Low", "High")
Activist$Country[Activist$Country==1]<-"DOM"
Activist$Country[Activist$Country==2]<-"ESV"
Activist$Country[Activist$Country==3]<-"HND"
Activist$Country[Activist$Country==4]<-"MEX"
Activist$Country[Activist$Country==5]<-"NIC"
Activist$Country[Activist$Country==6]<-"PAR"
Activist$Country[Activist$Country==7]<-"PER"

Agitator<-data.frame(Agitator)
names(Agitator)<-c("Country",  "Mean", "Low", "High")
Agitator$Country[Agitator$Country==1]<-"DOM"
Agitator$Country[Agitator$Country==2]<-"ESV"
Agitator$Country[Agitator$Country==3]<-"HND"
Agitator$Country[Agitator$Country==4]<-"MEX"
Agitator$Country[Agitator$Country==5]<-"NIC"
Agitator$Country[Agitator$Country==6]<-"PAR"
Agitator$Country[Agitator$Country==7]<-"PER"

Conventional<-data.frame(Conventional)
names(Conventional)<-c("Country", "Mean", "Low", "High")
Conventional$Country[Conventional$Country==1]<-"DOM"
Conventional$Country[Conventional$Country==2]<-"ESV"
Conventional$Country[Conventional$Country==3]<-"HND"
Conventional$Country[Conventional$Country==4]<-"MEX"
Conventional$Country[Conventional$Country==5]<-"NIC"
Conventional$Country[Conventional$Country==6]<-"PAR"
Conventional$Country[Conventional$Country==7]<-"PER"



Agitator<-cbind("Agitator vs. Outsider", Agitator)
names(Agitator)[1]<-"Type"

Activist<-cbind("Activist vs. Outsider", Activist)
names(Activist)[1]<-"Type"

Conventional<-cbind("Conventional vs. Outsider", Conventional)
names(Conventional)[1]<-"Type"

df<-rbind(Agitator, Conventional, Activist)



df$Country<-as.factor(df$Country)

df$Country<-factor(df$Country, 
	levels=rev(c("DOM","ESV", "HND", "MEX", "NIC",
			"PAR", "PER")))


p<-ggplot(df,
 aes(y=Country, x=Mean)) + 
    geom_errorbarh(aes(xmin=Low, 
	xmax=High, height=.2), colour="black") + facet_wrap(~Type)+
    geom_point(size=1.5)+
theme_bw()+xlab("")+ylab("")+
geom_hline(yintercept=0, linetype="longdash", colour="gray40")+
theme(strip.text.x = element_text(size = 9, face="bold"))+
geom_vline(xintercept=0, linetype="longdash", colour="gray40")+
theme(strip.background = element_rect(fill = alpha('white'))) +
theme(axis.title.x=element_text(vjust=2, size=10))+
theme(axis.text.y=element_text(size=10))


pdf(file = "Figure.A.41.pdf")
grid.draw(p)
dev.off()

################################################# Figure A.42 ##################################################################################

Activist<-NU.ACTIVIST.ALL[NU.ACTIVIST.ALL[,1]==2014,-1]
Agitator<-NU.AGITATOR.ALL[NU.AGITATOR.ALL[,1]==2014,-1]
Conventional<-NU.CONVENTIONAL.ALL[NU.CONVENTIONAL.ALL[,1]==2014,-1]


Activist<-data.frame(Activist)
names(Activist)<-c("Country",  "Mean", "Low", "High")
Activist$Country[Activist$Country==1]<-"DOM"
Activist$Country[Activist$Country==2]<-"ESV"
Activist$Country[Activist$Country==3]<-"HND"
Activist$Country[Activist$Country==4]<-"MEX"
Activist$Country[Activist$Country==5]<-"NIC"
Activist$Country[Activist$Country==6]<-"PAR"
Activist$Country[Activist$Country==7]<-"PER"

Agitator<-data.frame(Agitator)
names(Agitator)<-c("Country",  "Mean", "Low", "High")
Agitator$Country[Agitator$Country==1]<-"DOM"
Agitator$Country[Agitator$Country==2]<-"ESV"
Agitator$Country[Agitator$Country==3]<-"HND"
Agitator$Country[Agitator$Country==4]<-"MEX"
Agitator$Country[Agitator$Country==5]<-"NIC"
Agitator$Country[Agitator$Country==6]<-"PAR"
Agitator$Country[Agitator$Country==7]<-"PER"

Conventional<-data.frame(Conventional)
names(Conventional)<-c("Country", "Mean", "Low", "High")
Conventional$Country[Conventional$Country==1]<-"DOM"
Conventional$Country[Conventional$Country==2]<-"ESV"
Conventional$Country[Conventional$Country==3]<-"HND"
Conventional$Country[Conventional$Country==4]<-"MEX"
Conventional$Country[Conventional$Country==5]<-"NIC"
Conventional$Country[Conventional$Country==6]<-"PAR"
Conventional$Country[Conventional$Country==7]<-"PER"



Agitator<-cbind("Agitator vs. Outsider", Agitator)
names(Agitator)[1]<-"Type"

Activist<-cbind("Activist vs. Outsider", Activist)
names(Activist)[1]<-"Type"

Conventional<-cbind("Conventional vs. Outsider", Conventional)
names(Conventional)[1]<-"Type"

df<-rbind(Agitator, Conventional, Activist)



df$Country<-as.factor(df$Country)

df$Country<-factor(df$Country, 
	levels=rev(c("DOM","ESV", "HND", "MEX", "NIC",
			"PAR", "PER")))


p<-ggplot(df,
 aes(y=Country, x=Mean)) + 
    geom_errorbarh(aes(xmin=Low, 
	xmax=High, height=.2), colour="black") + facet_wrap(~Type)+
    geom_point(size=1.5)+
theme_bw()+xlab("")+ylab("")+
geom_hline(yintercept=0, linetype="longdash", colour="gray40")+
theme(strip.text.x = element_text(size = 9, face="bold"))+
geom_vline(xintercept=0, linetype="longdash", colour="gray40")+
theme(strip.background = element_rect(fill = alpha('white'))) +
theme(axis.title.x=element_text(vjust=2, size=10))+
theme(axis.text.y=element_text(size=10))


pdf(file = "Figure.A.42.pdf")
grid.draw(p)
dev.off()

################################################# Figure A.43 ##################################################################################

Activist<-NU.ACTIVIST.ALL[NU.ACTIVIST.ALL[,1]==2016,-1]
Agitator<-NU.AGITATOR.ALL[NU.AGITATOR.ALL[,1]==2016,-1]
Conventional<-NU.CONVENTIONAL.ALL[NU.CONVENTIONAL.ALL[,1]==2016,-1]


Activist<-data.frame(Activist)
names(Activist)<-c("Country",  "Mean", "Low", "High")
Activist$Country[Activist$Country==1]<-"DOM"
Activist$Country[Activist$Country==2]<-"ESV"
Activist$Country[Activist$Country==3]<-"HND"
Activist$Country[Activist$Country==4]<-"MEX"
Activist$Country[Activist$Country==5]<-"NIC"
Activist$Country[Activist$Country==6]<-"PAR"
Activist$Country[Activist$Country==7]<-"PER"

Agitator<-data.frame(Agitator)
names(Agitator)<-c("Country",  "Mean", "Low", "High")
Agitator$Country[Agitator$Country==1]<-"DOM"
Agitator$Country[Agitator$Country==2]<-"ESV"
Agitator$Country[Agitator$Country==3]<-"HND"
Agitator$Country[Agitator$Country==4]<-"MEX"
Agitator$Country[Agitator$Country==5]<-"NIC"
Agitator$Country[Agitator$Country==6]<-"PAR"
Agitator$Country[Agitator$Country==7]<-"PER"

Conventional<-data.frame(Conventional)
names(Conventional)<-c("Country", "Mean", "Low", "High")
Conventional$Country[Conventional$Country==1]<-"DOM"
Conventional$Country[Conventional$Country==2]<-"ESV"
Conventional$Country[Conventional$Country==3]<-"HND"
Conventional$Country[Conventional$Country==4]<-"MEX"
Conventional$Country[Conventional$Country==5]<-"NIC"
Conventional$Country[Conventional$Country==6]<-"PAR"
Conventional$Country[Conventional$Country==7]<-"PER"



Agitator<-cbind("Agitator vs. Outsider", Agitator)
names(Agitator)[1]<-"Type"

Activist<-cbind("Activist vs. Outsider", Activist)
names(Activist)[1]<-"Type"

Conventional<-cbind("Conventional vs. Outsider", Conventional)
names(Conventional)[1]<-"Type"

df<-rbind(Agitator, Conventional, Activist)



df$Country<-as.factor(df$Country)

df$Country<-factor(df$Country, 
	levels=rev(c("DOM","ESV", "HND", "MEX", "NIC",
			"PAR", "PER")))


p<-ggplot(df,
 aes(y=Country, x=Mean)) + 
    geom_errorbarh(aes(xmin=Low, 
	xmax=High, height=.2), colour="black") + facet_wrap(~Type)+
    geom_point(size=1.5)+
theme_bw()+xlab("")+ylab("")+
geom_hline(yintercept=0, linetype="longdash", colour="gray40")+
theme(strip.text.x = element_text(size = 9, face="bold"))+
geom_vline(xintercept=0, linetype="longdash", colour="gray40")+
theme(strip.background = element_rect(fill = alpha('white'))) +
theme(axis.title.x=element_text(vjust=2, size=10))+
theme(axis.text.y=element_text(size=10))


pdf(file = "Figure.A.43.pdf")
grid.draw(p)
dev.off()

################################################# Figure A.44 ##################################################################################

Activist<-NU.ACTIVIST.ALL[NU.ACTIVIST.ALL[,1]==2018,-1]
Agitator<-NU.AGITATOR.ALL[NU.AGITATOR.ALL[,1]==2018,-1]
Conventional<-NU.CONVENTIONAL.ALL[NU.CONVENTIONAL.ALL[,1]==2018,-1]


Activist<-data.frame(Activist)
names(Activist)<-c("Country",  "Mean", "Low", "High")
Activist$Country[Activist$Country==1]<-"DOM"
Activist$Country[Activist$Country==2]<-"ESV"
Activist$Country[Activist$Country==3]<-"HND"
Activist$Country[Activist$Country==4]<-"MEX"
Activist$Country[Activist$Country==5]<-"NIC"
Activist$Country[Activist$Country==6]<-"PAR"
Activist$Country[Activist$Country==7]<-"PER"

Agitator<-data.frame(Agitator)
names(Agitator)<-c("Country",  "Mean", "Low", "High")
Agitator$Country[Agitator$Country==1]<-"DOM"
Agitator$Country[Agitator$Country==2]<-"ESV"
Agitator$Country[Agitator$Country==3]<-"HND"
Agitator$Country[Agitator$Country==4]<-"MEX"
Agitator$Country[Agitator$Country==5]<-"NIC"
Agitator$Country[Agitator$Country==6]<-"PAR"
Agitator$Country[Agitator$Country==7]<-"PER"

Conventional<-data.frame(Conventional)
names(Conventional)<-c("Country", "Mean", "Low", "High")
Conventional$Country[Conventional$Country==1]<-"DOM"
Conventional$Country[Conventional$Country==2]<-"ESV"
Conventional$Country[Conventional$Country==3]<-"HND"
Conventional$Country[Conventional$Country==4]<-"MEX"
Conventional$Country[Conventional$Country==5]<-"NIC"
Conventional$Country[Conventional$Country==6]<-"PAR"
Conventional$Country[Conventional$Country==7]<-"PER"



Agitator<-cbind("Agitator vs. Outsider", Agitator)
names(Agitator)[1]<-"Type"

Activist<-cbind("Activist vs. Outsider", Activist)
names(Activist)[1]<-"Type"

Conventional<-cbind("Conventional vs. Outsider", Conventional)
names(Conventional)[1]<-"Type"

df<-rbind(Agitator, Conventional, Activist)



df$Country<-as.factor(df$Country)

df$Country<-factor(df$Country, 
	levels=rev(c("DOM","ESV", "HND", "MEX", "NIC",
			"PAR", "PER")))


p<-ggplot(df,
 aes(y=Country, x=Mean)) + 
    geom_errorbarh(aes(xmin=Low, 
	xmax=High, height=.2), colour="black") + facet_wrap(~Type)+
    geom_point(size=1.5)+
theme_bw()+xlab("")+ylab("")+
geom_hline(yintercept=0, linetype="longdash", colour="gray40")+
theme(strip.text.x = element_text(size = 9, face="bold"))+
geom_vline(xintercept=0, linetype="longdash", colour="gray40")+
theme(strip.background = element_rect(fill = alpha('white'))) +
theme(axis.title.x=element_text(vjust=2, size=10))+
theme(axis.text.y=element_text(size=10))


pdf(file = "Figure.A.44.pdf")
grid.draw(p)
dev.off()
rm(list=ls())


################################################# SECTION A.3.6 ##################################################################################
################################################# Figure A.45 ##################################################################################

load("Results_ContinuousModel")



## Convergence Checks for Continuous Model
Alpha<-mcmc.list(as.mcmc(t(Results$alpha[,,1])), 
		as.mcmc(t(Results$alpha[,,2])),
		as.mcmc(t(Results$alpha[,,3])))
print(paste("Convergence Alpha:", gelman.diag(Alpha)[[2]]<1.2, sep=" "))


Alpha.Conv<-mcmc.list(as.mcmc(t(Results$alpha_conv[-seq(12,dim(Results$alpha_conv)[1],by=12),,1])), 
		as.mcmc(t(Results$alpha_conv[-seq(12,dim(Results$alpha_conv)[1],by=12),,2])),
		as.mcmc(t(Results$alpha_conv[-seq(12,dim(Results$alpha_conv)[1],by=12),,3])))
print(paste("Convergence Alpha Conv:", gelman.diag(Alpha.Conv)[[2]]<1.2, sep=" "))


Alpha.Unconv<-mcmc.list(as.mcmc(t(Results$alpha_unconv[-seq(1,dim(Results$alpha_unconv)[1],by=12),,1])), 
		as.mcmc(t(Results$alpha_unconv[-seq(1,dim(Results$alpha_unconv)[1],by=12),,2])),
		as.mcmc(t(Results$alpha_unconv[-seq(1,dim(Results$alpha_unconv)[1],by=12),,3])))
print(paste("Convergence Alpha Unconventional:", gelman.diag(Alpha.Unconv)[[2]]<1.2, sep=" "))


Posterior.Conv<-rbind(t(Results$C_conv[,,1]),
		t(Results$C_conv[,,2]),
		t(Results$C_conv[,,3]))

Posterior.Unconv<-rbind(t(Results$C_unconv[,,1]),
		t(Results$C_unconv[,,2]),
		t(Results$C_unconv[,,3]))


Assignment.Conv<-colMeans(Posterior.Conv)
Assignment.Unconv<-colMeans(Posterior.Unconv)

Assignment.Continuous<-cbind(Assignment.Conv, Assignment.Unconv)



ALPHA.CONV<-rbind(t(Results$alpha_conv[,,1]), 
		t(Results$alpha_conv[,,2]),
		t(Results$alpha_conv[,,3]))



Alpha.conv.averages<-matrix(0, 12,4)
rownames(Alpha.conv.averages)<-NULL
colnames(Alpha.conv.averages)<-NULL

Alpha.conv.averages[,1]<-c("Municipal meeting", "Contacting municipality", "Contacting local authority", "Contacting national authority",
                    		"Solving Problems", "Improvements meeting", "Party meeting", "Voting",
                    		"Sharing online", "Petitioning", "Protesting", "Blocking")
Alpha.conv.averages[,2]<-"Conventional"

for (j in 1:12) {

m<-round(mean(c(ALPHA.CONV[, seq(j, ncol(ALPHA.CONV), by=12)])), digits=3)
q.l<-round(quantile(c(ALPHA.CONV[, seq(j, ncol(ALPHA.CONV), by=12)]), c(0.025,0.975))[1], digits=3)
q.h<-round(quantile(c(ALPHA.CONV[, seq(j, ncol(ALPHA.CONV), by=12)]), c(0.025,0.975))[2], digits=3)


if (nchar(m)<5 & grepl("\\.", m)==FALSE) {
m<-paste(m,".", sep="")
while (nchar(m)<5) {
m<-paste(m, "0", sep="")
} 
} else if (nchar(m)<5 & grepl("\\.", m)==TRUE) {
while (nchar(m)<5) {
m<-paste(m, "0", sep="")
} 
}


if (nchar(q.l)<5 & grepl("\\.", q.l)==FALSE) {
q.l<-paste(q.l,".", sep="")
while (nchar(q.l)<5) {
q.l<-paste(q.l, "0", sep="")
} 
} else if (nchar(q.l)<5 & grepl("\\.", q.l)==TRUE) {
while (nchar(q.l)<5) {
q.l<-paste(q.l, "0", sep="")
} 
}

if (nchar(q.h)<5 & grepl("\\.", q.h)==FALSE) {
q.h<-paste(q.h,".", sep="")
while (nchar(q.h)<5) {
q.h<-paste(q.h, "0", sep="")
} 
} else if (nchar(q.h)<5 & grepl("\\.", q.h)==TRUE) {
while (nchar(q.h)<5) {
q.h<-paste(q.h, "0", sep="")
} 
}


Alpha.conv.averages[j,3:4]<-c(m, paste(paste("(", q.l, ",", sep=""), paste(q.h,")", sep=""),sep=""))



}



ALPHA.UNCONV<-rbind(t(Results$alpha_unconv[,,1]), 
		t(Results$alpha_unconv[,,2]),
		t(Results$alpha_unconv[,,3]))





Alpha.unconv.averages<-matrix(0, 12,4)
rownames(Alpha.unconv.averages)<-NULL
colnames(Alpha.unconv.averages)<-NULL

Alpha.unconv.averages[,1]<-c("Municipal meeting", "Contacting municipality", "Contacting local authority", "Contacting national authority",
                    		"Solving Problems", "Improvements meeting", "Party meeting", "Voting",
                    		"Sharing online", "Petitioning", "Protesting", "Blocking")
Alpha.unconv.averages[,2]<-"Unconventional"



for (j in 1:12) {

m<-round(mean(c(ALPHA.UNCONV[, seq(j, ncol(ALPHA.UNCONV), by=12)])), digits=3)
q.l<-round(quantile(c(ALPHA.UNCONV[, seq(j, ncol(ALPHA.UNCONV), by=12)]), c(0.025,0.975))[1], digits=3)
q.h<-round(quantile(c(ALPHA.UNCONV[, seq(j, ncol(ALPHA.UNCONV), by=12)]), c(0.025,0.975))[2], digits=3)


if (nchar(m)<5 & grepl("\\.", m)==FALSE) {
m<-paste(m,".", sep="")
while (nchar(m)<5) {
m<-paste(m, "0", sep="")
} 
} else if (nchar(m)<5 & grepl("\\.", m)==TRUE) {
while (nchar(m)<5) {
m<-paste(m, "0", sep="")
} 
}


if (nchar(q.l)<5 & grepl("\\.", q.l)==FALSE) {
q.l<-paste(q.l,".", sep="")
while (nchar(q.l)<5) {
q.l<-paste(q.l, "0", sep="")
} 
} else if (nchar(q.l)<5 & grepl("\\.", q.l)==TRUE) {
while (nchar(q.l)<5) {
q.l<-paste(q.l, "0", sep="")
} 
}

if (nchar(q.h)<5 & grepl("\\.", q.h)==FALSE) {
q.h<-paste(q.h,".", sep="")
while (nchar(q.h)<5) {
q.h<-paste(q.h, "0", sep="")
} 
} else if (nchar(q.h)<5 & grepl("\\.", q.h)==TRUE) {
while (nchar(q.h)<5) {
q.h<-paste(q.h, "0", sep="")
} 
}


Alpha.unconv.averages[j,3:4]<-c(m, paste(paste("(", q.l, ",", sep=""), paste(q.h,")", sep=""),sep=""))



}




load("Probs.Dimensions.MainModel")


Upper.panel<-cbind(Assignment.Continuous[,1],Probs.Dimensions.MainModel[,1])
Upper.panel<-data.frame(Upper.panel)
names(Upper.panel)<-c("Continuous", "Discrete")


p.1<-ggplot(Upper.panel, aes(Continuous, Discrete)) +
geom_point(size=3, shape=21)+xlab("Continuous conventional trait")+
ylab("Probability of belonging to
the high conventional tpye")+theme_bw()+ggtitle("Conventional Dimension")+
theme(plot.title = element_text(hjust = 0.5, face="bold", size=14))+
theme(axis.title.x=element_text(size=11))+
theme(axis.title.y=element_text(size=11))+
#scale_x_continuous(limits=c(-1.5, 3), breaks=c(-1,0,1,2))+
theme(plot.margin=unit(c(1,1,1,1), "lines"))+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank())


Lower.panel<-cbind(Assignment.Continuous[,2],Probs.Dimensions.MainModel[,2])
Lower.panel<-data.frame(Lower.panel)
names(Lower.panel)<-c("Continuous", "Discrete")

p.2<-ggplot(Lower.panel, aes(Continuous, Discrete)) +
geom_point(size=3, shape=21)+xlab("Continuous unconventional trait")+
ylab("Probability of belonging to
the high unconventional tpye")+theme_bw()+ggtitle("Unconventional Dimension")+
theme(plot.title = element_text(hjust = 0.5, face="bold", size=14))+
theme(axis.title.x=element_text(size=11))+
theme(axis.title.y=element_text(size=11))+
#scale_x_continuous(limits=c(-1.5, 3), breaks=c(-1,0,1,2))+
theme(plot.margin=unit(c(1,1,1,1), "lines"))+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank())



# Multiple plot function
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


pdf(file = "Figure.A.45.pdf")
grid.draw(multiplot(p.1, p.2, cols=1))
dev.off()


################################################# Figure A.46 ##################################################################################

pred.high.conventional <- ifelse(Probs.Dimensions.MainModel[,1] > 0.5, 1, 0) 


Upper.panel<-Assignment.Continuous[pred.high.conventional == 1,1]
Upper.panel<-data.frame(Upper.panel)
names(Upper.panel)<-c("Density")



p.1<-ggplot(Upper.panel, aes(x=Density))+geom_density(adjust=5)+
	xlab("Continuous conventional trait")+ylab("")+
	theme_bw()+ggtitle("High Conventional Type")+
	theme(plot.title = element_text(hjust = 0.5, face="bold", size=14))+
      scale_x_continuous(limits=c(-0.5,0.5))+
      scale_y_continuous(labels=c("0", "0.2", "0.4", "0.6", "0.8"))+
	theme(plot.margin=unit(c(1,1,1,1), "lines"))+
      geom_vline(xintercept=mean(Assignment.Continuous[pred.high.conventional == 1,1]))+
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank())



Lower.panel<-Assignment.Continuous[pred.high.conventional == 0,1]
Lower.panel<-data.frame(Lower.panel)
names(Lower.panel)<-c("Density")


p.2<-ggplot(Lower.panel, aes(x=Density))+geom_density(adjust=5)+
	xlab("Continuous conventional trait")+ylab("")+
	theme_bw()+ggtitle("Low Conventional Type")+
	theme(plot.title = element_text(hjust = 0.5, face="bold", size=14))+
      scale_x_continuous(limits=c(-0.5,0.5))+
      scale_y_continuous(labels=c("0", "0.2", "0.4", "0.6", "0.8"))+
	theme(plot.margin=unit(c(1,1,1,1), "lines"))+
      geom_vline(xintercept=mean(Assignment.Continuous[pred.high.conventional == 0,1]))+
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank())



pdf(file = "Figure.A.46.pdf")
grid.draw(multiplot(p.1, p.2, cols=1))
dev.off()


################################################# Figure A.47 ##################################################################################

pred.high.unconventional <- ifelse(Probs.Dimensions.MainModel[,2] > 0.5, 1, 0) 


Upper.panel<-Assignment.Continuous[pred.high.unconventional == 1,2]
Upper.panel<-data.frame(Upper.panel)
names(Upper.panel)<-c("Density")



p.1<-ggplot(Upper.panel, aes(x=Density))+geom_density(adjust=2)+
	xlab("Continuous unconventional trait")+ylab("")+
	theme_bw()+ggtitle("High Unconventional Type")+
	theme(plot.title = element_text(hjust = 0.5, face="bold", size=14))+
      scale_x_continuous(limits=c(-0.5,0.5))+
      scale_y_continuous(labels=c("0", "0.2", "0.4", "0.6"))+
	theme(plot.margin=unit(c(1,1,1,1), "lines"))+
      geom_vline(xintercept=mean(Assignment.Continuous[pred.high.unconventional == 1,2]))+
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank())



Lower.panel<-Assignment.Continuous[pred.high.unconventional == 0,2]
Lower.panel<-data.frame(Lower.panel)
names(Lower.panel)<-c("Density")


p.2<-ggplot(Lower.panel, aes(x=Density))+geom_density(adjust=5)+
	xlab("Continuous unconventional trait")+ylab("")+
	theme_bw()+ggtitle("Low Unconventional Type")+
	theme(plot.title = element_text(hjust = 0.5, face="bold", size=14))+
     scale_x_continuous(limits=c(-0.5,0.5))+
         scale_y_continuous(labels=c("0", "0.2", "0.4", "0.6","0.8"))+
	theme(plot.margin=unit(c(1,1,1,1), "lines"))+
      geom_vline(xintercept=mean(Assignment.Continuous[pred.high.unconventional == 0,2]))+
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank())



pdf(file = "Figure.A.47.pdf")
grid.draw(multiplot(p.1, p.2, cols=1))
dev.off()

################################################# Table A.16 ##################################################################################

Table.A.16<-rbind(Alpha.conv.averages,Alpha.unconv.averages)
Table.A.16<-Table.A.16[c(8, 1:4, 6,5, 7,10, 9, 11, 12, 20, 13:16, 18, 17, 19, 22, 21, 23, 24),] 
colnames(Table.A.16)<-c("Activity", "Dimension", "Mean", "Interval")
print(Table.A.16)

Table.A.16<-cbind(Table.A.16[1:12,-c(2)],Table.A.16[13:24,3:4])


Titles<-matrix(c("", "Conventional", "Dimension", "Unconventional", "Dimension"),nrow=1)

g1 <- tableGrob(Table.A.16, rows = NULL)
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g1), l = 1, r = ncol(g1))
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g1), l = 1, r = 1)
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g1), l = 1, r = 3)
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 1, l = 1, r = ncol(g1))


g0 <- tableGrob(Titles, rows = NULL)
g0 <- gtable_add_grob(g0,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g0), l = 1, r = 3)
g0 <- gtable_add_grob(g0,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g0), l = 1, r = 1)
g0 <- gtable_add_grob(g0,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 1, l = 1, r = ncol(g0))

g<-rbind(g0,g1)


pdf(file = "Table.A.16.pdf", height=10, width=10)
grid.draw(g)
dev.off()

