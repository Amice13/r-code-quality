args <- commandArgs(trailingOnly = TRUE)
pool<-as.numeric(args[1])


library(coda)
options(warn=-1)

set.seed(2342)

################################################# TABLES AND FIGURES IN THE MAIN TEXT (MANUSCRIPT) ############################################################################

############################################################# Table 2 ###############################################################################################################################

# In order to produce Table 2, we first need to process the results of the State-space model

load("Estimates_Statespace") 


# Convergence checks - State-space model

ALPHA<-mcmc.list(as.mcmc(t(out$alpha[,,1])), as.mcmc(t(out$alpha[,,2])), as.mcmc(t(out$alpha[,,3])))

BETA2<-mcmc.list(as.mcmc(t(out$beta2[,,1])), as.mcmc(t(out$beta2[,,2])), as.mcmc(t(out$beta2[,,3])))

BETA3<-mcmc.list(as.mcmc(t(out$beta3[,,1])), as.mcmc(t(out$beta3[,,2])), as.mcmc(t(out$beta3[,,3])))

VAR.MATRIX<-mcmc.list(as.mcmc(t(out$Q[c(diag(matrix(1:144, 12,12))),,1])), 
	as.mcmc(t(out$Q[c(diag(matrix(1:144, 12,12))),,2])), as.mcmc(t(out$Q[c(diag(matrix(1:144, 12,12))),,3])))


print(paste("Convergence, Parameter Estimates of the State-space model:", gelman.diag(ALPHA)[[2]]<1.2 & gelman.diag(BETA2)[[2]]<1.2 & 
	gelman.diag(BETA3)[[2]]<1.2 & gelman.diag(VAR.MATRIX)[[2]]<1.2, sep=" "))


# Pooled convergent draws - to be used for inference
Beta2<-rbind(t(out$beta2[,pool:20000, 1]),
		t(out$beta2[,pool:20000, 2]),
		t(out$beta2[,pool:20000, 3]))


Beta3<-rbind(t(out$beta3[, pool:20000, 1]),
		t(out$beta3[,pool:20000 , 2]),
		t(out$beta3[,pool:20000 , 3]))


Q<-rbind((t(out$Q[c(diag(matrix(1:144, 12,12))),pool:20000,1])), 
	(t(out$Q[c(diag(matrix(1:144, 12,12))),pool:20000,2])), (t(out$Q[c(diag(matrix(1:144, 12,12))),pool:20000,3])))


Intercept2<-rbind(t(out$alpha[seq(1, nrow(out$alpha), by=12), pool:20000, 1]),
		t(out$alpha[seq(1, nrow(out$alpha), by=12), pool:20000, 2]),
		t(out$alpha[seq(1, nrow(out$alpha), by=12),pool:20000 , 3]))

Intercept3<-rbind(t(out$alpha[seq(7, nrow(out$alpha), by=12), pool:20000, 1]),
		t(out$alpha[seq(7, nrow(out$alpha), by=12),pool:20000 , 2]),
		t(out$alpha[seq(7, nrow(out$alpha), by=12),pool:20000 , 3]))


Insider2<-rbind(t(out$alpha[seq(2, nrow(out$alpha), by=12),pool:20000 , 1]),
		t(out$alpha[seq(2, nrow(out$alpha), by=12),pool:20000 , 2]),
		t(out$alpha[seq(2, nrow(out$alpha), by=12),pool:20000 , 3]))

Insider2.Dynamics<-cbind(colMeans(Insider2+sqrt(Q[,2])+sqrt(Q[,8])), HPDinterval(as.mcmc(Insider2+sqrt(Q[,2])+sqrt(Q[,8])), p=0.95)) 

Insider3<-rbind(t(out$alpha[seq(8, nrow(out$alpha), by=12),pool:20000, 1]),
		t(out$alpha[seq(8, nrow(out$alpha), by=12),pool:20000, 2]),
		t(out$alpha[seq(8, nrow(out$alpha), by=12),pool:20000 , 3]))


Insider3.Dynamics<-cbind(colMeans(Insider3+sqrt(Q[,2])+sqrt(Q[,8])), HPDinterval(as.mcmc(Insider3+sqrt(Q[,2])+sqrt(Q[,8])), p=0.95)) 


Rooted2<-rbind(t(out$alpha[seq(3, nrow(out$alpha), by=12), pool:20000, 1]),
		t(out$alpha[seq(3, nrow(out$alpha), by=12),pool:20000 ,2]),
		t(out$alpha[seq(3, nrow(out$alpha), by=12),pool:20000 , 3]))

Rooted2.Dynamics<-cbind(colMeans(Rooted2+sqrt(Q[,3])), HPDinterval(as.mcmc(Rooted2+sqrt(Q[,3])), p=0.95)) 

Rooted3<-rbind(t(out$alpha[seq(9, nrow(out$alpha), by=12), pool:20000, 1]),
		t(out$alpha[seq(9, nrow(out$alpha), by=12),pool:20000 , 2]),
		t(out$alpha[seq(9, nrow(out$alpha), by=12), pool:20000, 3]))

Rooted3.Dynamics<-cbind(colMeans(Rooted3-sqrt(Q[,9])),HPDinterval(as.mcmc(Rooted3-sqrt(Q[,9])), p=0.95)) 



Ideologically.Novel2<-rbind(t(out$alpha[seq(4, nrow(out$alpha), by=12),pool:20000 , 1]),
		t(out$alpha[seq(4, nrow(out$alpha), by=12),pool:20000 , 2]),
		t(out$alpha[seq(4, nrow(out$alpha), by=12), pool:20000, 3]))
Ideologically.Novel2.Dynamics<-cbind(colMeans(Ideologically.Novel2+sqrt(rowSums(Q[,1:6]))), HPDinterval(as.mcmc(Ideologically.Novel2+sqrt(rowSums(Q[,1:6]))), p=0.95)) 


Ideologically.Novel3<-rbind(t(out$alpha[seq(10, nrow(out$alpha), by=12),pool:20000 , 1]),
		t(out$alpha[seq(10, nrow(out$alpha), by=12), pool:20000, 2]),
		t(out$alpha[seq(10, nrow(out$alpha), by=12),pool:20000 , 3]))

Ideologically.Novel3.Dynamics<-cbind(colMeans(Ideologically.Novel3+sqrt(rowSums(Q[,1:12]))), HPDinterval(as.mcmc(Ideologically.Novel3+sqrt(rowSums(Q[,1:12]))), p=0.95)) 


OriginNew2<-rbind(t(out$alpha[seq(5, nrow(out$alpha), by=12), pool:20000, 1]),
		t(out$alpha[seq(5, nrow(out$alpha), by=12), pool:20000, 2]),
		t(out$alpha[seq(5, nrow(out$alpha), by=12), pool:20000, 3]))

OriginNew3<-rbind(t(out$alpha[seq(11, nrow(out$alpha), by=12), pool:20000 , 1]),
		t(out$alpha[seq(11, nrow(out$alpha), by=12), pool:20000 , 2]),
		t(out$alpha[seq(11, nrow(out$alpha), by=12), pool:20000 , 3]))


Interaction2<-rbind(t(out$alpha[seq(6, nrow(out$alpha), by=12),pool:20000  , 1]),
		t(out$alpha[seq(6, nrow(out$alpha), by=12), pool:20000 , 2]),
		t(out$alpha[seq(6, nrow(out$alpha), by=12), pool:20000 , 3]))


Interaction.IdeologicallyNovel.StructurallyNotNovel2<-Ideologically.Novel2+Interaction2-OriginNew2-sqrt(rowSums(Q[,1:6]))
Interaction.IdeologicallyNovel.StructurallyNotNovel2.Dynamics<-cbind(colMeans(Interaction.IdeologicallyNovel.StructurallyNotNovel2),
		 HPDinterval(as.mcmc(Interaction.IdeologicallyNovel.StructurallyNotNovel2), p=0.95)) 


Interaction3<-rbind(t(out$alpha[seq(12, nrow(out$alpha), by=12),pool:20000 , 1]),
		t(out$alpha[seq(12, nrow(out$alpha), by=12),pool:20000 , 2]),
		t(out$alpha[seq(12, nrow(out$alpha), by=12),pool:20000 , 3]))

Interaction.IdeologicallyNovel.StructurallyNotNovel3<-Ideologically.Novel3+Interaction3-OriginNew3+sqrt(rowSums(Q[,7:12]))
Interaction.IdeologicallyNovel.StructurallyNotNovel3.Dynamics<-cbind(colMeans(Interaction.IdeologicallyNovel.StructurallyNotNovel3),
		 HPDinterval(as.mcmc(Interaction.IdeologicallyNovel.StructurallyNotNovel3), p=0.95)) 


# Save to be used later to produce in Figure 3 and 4
Dynamics.Statespace<-rbind(cbind("Insider2", Insider2.Dynamics),
cbind("Insider3", Insider3.Dynamics),
cbind("Rooted2", Rooted2.Dynamics),
cbind("Rooted3", Rooted3.Dynamics),
cbind("IdeologicallyNovel.Rooted2",Interaction.IdeologicallyNovel.StructurallyNotNovel2.Dynamics),
cbind("IdeologicallyNovel.Rooted3",Interaction.IdeologicallyNovel.StructurallyNotNovel3.Dynamics),
cbind("Interaction.IdeologicallyNovel.StructurallyNotNovel2", Ideologically.Novel2.Dynamics),
cbind("Interaction.IdeologicallyNovel.StructurallyNotNovel3", Ideologically.Novel3.Dynamics))


save(Dynamics.Statespace, file="Dynamics_Statespace")



Reffect.Country2<-rbind(t(out$reffect_country[1:22, pool:20000, 1]),
		t(out$reffect_country[1:22, pool:20000, 2]),
		t(out$reffect_country[1:22, pool:20000, 3]))
Reffect.Country3<-rbind(t(out$reffect_country[23:44,pool:20000 , 1]),
		t(out$reffect_country[23:44, pool:20000, 2]),
		t(out$reffect_country[23:44,pool:20000 , 3]))

Reffect.Party2<-rbind(t(out$reffect_party[1:204,pool:20000 , 1]),
		t(out$reffect_party[1:204,pool:20000 , 2]),
		t(out$reffect_party[1:204, pool:20000, 3]))
Reffect.Party3<-rbind(t(out$reffect_party[205:408,pool:20000 , 1]),
		t(out$reffect_party[205:408,pool:20000 , 2]),
		t(out$reffect_party[205:408,pool:20000 , 3]))


## Load Data 
df<-read.csv("Dataset.csv")

df<-subset(df, select=c("votes.centered",
			"country",
			"case_id",
			"merged", "dissolved",
			"year_exist",
			"logseats.centered",
		 "government",
			 "EU_parl",
		 "reg_gov",
		 "finance_party_qualify",
		 "efNPP",
	 "elec_coalition", 
	 "indPOL",
	"election_year_dummy",
	"distinctiveness",
	 "insider", 
	 "organisation", 
	 "newfamfirst",
	"OriginNew",
	"post.election.year",	
	"pre.election.year", 
	"seat.product"
	 ))

df<-na.exclude(df)


country<-as.numeric(factor(df$country), levels=unique(df$country))
party<-as.numeric(factor(df$case_id), levels=unique(df$case_id))
num.country<-length(unique(country))
num.party<-length(unique(party))

# 1= survive, 2=merge, 3=dissolve
Y<-cbind(df$merged, df$dissolved)
Y<-cbind(ifelse(rowSums(Y)==0,1,0),Y)
y<-c(sapply(1:nrow(Y), function(i) which(Y[i,]==1)))
n<-length(y)          


Matrix.Country<-matrix(0,n,num.country)
for (i in 1:num.country) Matrix.Country[country==i,i]<-1

Matrix.Party<-matrix(0,n,num.party)
for (i in 1:num.party) Matrix.Party[party==i,i]<-1

duration<-as.numeric(factor(df$year_exist), levels=unique(df$year_exist))
num.duration<-length(unique(duration))

Matrix.Duration<-matrix(0,n,num.duration)
for (i in 1:num.duration) Matrix.Duration[duration==i,i]<-1

Matrix.Outcome<-matrix(0, n, 3)
for (i in 1:3) Matrix.Outcome[y==i,i]<-1



W<-cbind(df$votes.centered, 
	 df$logseats.centered,  
	 df$government,
	 df$EU_parl,
	 df$reg_gov,  
	 df$finance_party_qualify,
	 (df$efNPP-mean(df$efNPP))/(2*sd(df$efNPP)),
	 df$elec_coalition, 
	 (df$indPOL-mean(df$indPOL))/(2*sd(df$indPOL)),
	 (df$seat.product-mean(df$seat.product))/(2*sd(df$seat.product)),
	df$election_year_dummy,
	df$post.election.year,	
	df$pre.election.year, 
	df$distinctiveness)

X<-cbind(1,  
	 df$insider, 
	 df$organisation, 
        df$newfamfirst,
	 df$OriginNew,
	I(df$OriginNew*df$newfamfirst)
	 )



Matrix.Cov.Beta<-solve(diag(ncol(W))+(6/(pi^2))*crossprod(W))

Matrix.Index.Durations<-matrix(0, length(unique(party)), num.duration)
table.durations<-c()

for (i in 1:num.duration) {

Matrix.Index.Durations[,i]<-c(sort(which(duration==i))-1, rep(0,length(unique(party))-length(sort(which(duration==i)))))
table.durations<-c(table.durations, length(sort(which(duration==i)))-1)

}

Matrix.ColIndex.Party<-matrix(0, length(unique(party)), num.duration)
table.durations.colparty<-c()
for (i in 1:num.duration) {

Matrix.ColIndex.Party[,i]<-c(sort(party[which(duration==i)])-1,rep(0,length(unique(party))-length(sort(party[which(duration==i)]))))
table.durations.colparty<-c(table.durations.colparty, length(sort(party[which(duration==i)]))-1)
}


Matrix.ColIndex.Country<-matrix(0, length(unique(country)), num.duration)
table.durations.colcountry<-c()
for (i in 1:num.duration) {

Matrix.ColIndex.Country[,i]<-c(sort(unique(country[which(duration==i)]))-1,rep(0,length(unique(country))-length(sort(unique(country[which(duration==i)])))))
table.durations.colcountry<-c(table.durations.colcountry, length(sort(unique(country[which(duration==i)])))-1)
}




colnames(W)<-c("Electoral.Performance", "Seat.Share", "National.Government", "EP.Access", "Regional.Government",
		"State.Funding", "System.Fragmentation", "Electoral.Coalition", "Anti-establishment.Vote",
		"Seat.Product", "Election.Year", "Post", "Pre",
		"Party.Distinctiveness")

colnames(X)<-c("Intercept", "Insider.Status", "Societal.Rootednes", "Ideological.Novelty",
		"Roots.in.Parties", "Interaction.Novelty.Roots")




## Compute Average Estimates for Table 2

Origin.Averages.2<-cbind(rowMeans(Intercept2), rowMeans(Insider2), rowMeans(Rooted2), rowMeans(Ideologically.Novel2), rowMeans(OriginNew2), rowMeans(Interaction2))
Origin.Averages.3<-cbind(rowMeans(Intercept3), rowMeans(Insider3), rowMeans(Rooted3), rowMeans(Ideologically.Novel3), rowMeans(OriginNew3), rowMeans(Interaction3))


# Marginal Effects Table 2
Marginal.prob.origin<-matrix(NA, nrow=4*2, ncol=5)
colnames(Marginal.prob.origin)<-c("Dep.Variable", "Covariate", "Predictive Comparison",   "95% CI - Lower", "95% CI - Higher")
Marginal.prob.origin[,1]<-c(rep("Dissolution", 4), 
		rep("Merger", 4))
Marginal.prob.origin[,2]<-rep(c(colnames(X)[2:5]),2)


Marginal.prob.controls<-matrix(NA, nrow=(ncol(W)*2), ncol=5)
colnames(Marginal.prob.controls)<-c("Dep.Variable", "Covariate", "Predictive Comparison", "95% CI - Lower", "95% CI - Higher")
Marginal.prob.controls[,1]<-c(rep("Dissolution", ncol(W)), 
		rep("Merger", ncol(W)))
Marginal.prob.controls[,2]<-rep(colnames(W),2)



dummies<-c(2:5)
 
for (j in 1:length(dummies)) {

if (j<=3) {
X.0<-X
X.0[,dummies[j]]<-0
X.1<-X
X.1[,dummies[j]]<-1
} else if (j==4) {
X.0<-X
X.0[,dummies[j]]<-1
X.1<-X
X.1[,dummies[j]]<-0
}


Dissolution<-sapply(1:nrow(Beta2), function(s) mean(
exp(W%*%t(Beta3)[,s]+X.1%*%Origin.Averages.3[s,])/(1+
exp(W%*%t(Beta3)[,s]+X.1%*%Origin.Averages.3[s,])+
exp(W%*%t(Beta2)[,s]+X.1%*%Origin.Averages.2[s,])) -
exp(W%*%t(Beta3)[,s]+X.0%*%Origin.Averages.3[s,])/(1+
exp(W%*%t(Beta3)[,s]+X.0%*%Origin.Averages.3[s,])+
exp(W%*%t(Beta2)[,s]+X.0%*%Origin.Averages.2[s,])))
)

Merger<-sapply(1:nrow(Beta2), function(s) mean(
exp(W%*%t(Beta2)[,s]+X.1%*%Origin.Averages.2[s,])/(1+
exp(W%*%t(Beta3)[,s]+X.1%*%Origin.Averages.3[s,])+
exp(W%*%t(Beta2)[,s]+X.1%*%Origin.Averages.2[s,])) -
exp(W%*%t(Beta2)[,s]+X.0%*%Origin.Averages.2[s,])/(1+
exp(W%*%t(Beta3)[,s]+X.0%*%Origin.Averages.3[s,])+
exp(W%*%t(Beta2)[,s]+X.0%*%Origin.Averages.2[s,])))
)





Marginal.prob.origin[max(grep(colnames(X)[dummies[j]],Marginal.prob.origin[,2])),3:ncol(Marginal.prob.origin)]<-c(mean(Merger),
		HPDinterval(as.mcmc(Merger), p=0.95))


Marginal.prob.origin[min(grep(colnames(X)[dummies[j]],Marginal.prob.origin[,2])),3:ncol(Marginal.prob.origin)]<-c(mean(Dissolution),
		HPDinterval(as.mcmc(Dissolution), p=0.95))



}




Marginal.prob.origin[,3:ncol(Marginal.prob.origin)]<-100*as.numeric(Marginal.prob.origin[,3:ncol(Marginal.prob.origin)])



rm(dummies)
dummies<-c(3:6, 8,11:14)
continuous<-c(1,2,7,9,10)  



for (j in 1:length(dummies)) {

Merger.0<-matrix(NA, nrow=nrow(W), ncol=nrow(Beta2))
Merger.1<-matrix(NA, nrow=nrow(W), ncol=nrow(Beta2))
Dissolution.0<-matrix(NA, nrow=nrow(W), ncol=nrow(Beta2))
Dissolution.1<-matrix(NA, nrow=nrow(W), ncol=nrow(Beta2))


W.0<-W
W.1<-W
W.0[,dummies[j]]<-0
W.1[,dummies[j]]<-1


for (l in 1:num.duration) {

Merger.0[duration==l,]<-sapply(1:nrow(Beta2), function(s) exp(W.0[duration==l,]%*%t(Beta2)[,s]+X[duration==l,]%*%t(cbind(Intercept2[s,l], Insider2[s,l], Rooted2[s,l], Ideologically.Novel2[s,l], OriginNew2[s,l], Interaction2[s,l])))/
(1+exp(W.0[duration==l,]%*%t(Beta2)[,s]+X[duration==l,]%*%t(cbind(Intercept2[s,l], Insider2[s,l], Rooted2[s,l], Ideologically.Novel2[s,l], OriginNew2[s,l], Interaction2[s,l])))+
exp(W.0[duration==l,]%*%t(Beta3)[,s]+X[duration==l,]%*%t(cbind(Intercept3[s,l], Insider3[s,l], Rooted3[s,l], Ideologically.Novel3[s,l], OriginNew3[s,l], Interaction3[s,l]))))
)


Merger.1[duration==l,]<-sapply(1:nrow(Beta2), function(s) exp(W.1[duration==l,]%*%t(Beta2)[,s]+X[duration==l,]%*%t(cbind(Intercept2[s,l], Insider2[s,l], Rooted2[s,l], Ideologically.Novel2[s,l], OriginNew2[s,l], Interaction2[s,l])))/
(1+exp(W.1[duration==l,]%*%t(Beta2)[,s]+X[duration==l,]%*%t(cbind(Intercept2[s,l], Insider2[s,l], Rooted2[s,l], Ideologically.Novel2[s,l], OriginNew2[s,l], Interaction2[s,l])))+
exp(W.1[duration==l,]%*%t(Beta3)[,s]+X[duration==l,]%*%t(cbind(Intercept3[s,l], Insider3[s,l], Rooted3[s,l], Ideologically.Novel3[s,l], OriginNew3[s,l], Interaction3[s,l]))))
)


Dissolution.0[duration==l,]<-sapply(1:nrow(Beta2), function(s) exp(W.0[duration==l,]%*%t(Beta3)[,s]+X[duration==l,]%*%t(cbind(Intercept3[s,l], Insider3[s,l], Rooted3[s,l], Ideologically.Novel3[s,l], OriginNew3[s,l], Interaction3[s,l])))/
(1+exp(W.0[duration==l,]%*%t(Beta2)[,s]+X[duration==l,]%*%t(cbind(Intercept2[s,l], Insider2[s,l], Rooted2[s,l], Ideologically.Novel2[s,l], OriginNew2[s,l], Interaction2[s,l])))+
exp(W.0[duration==l,]%*%t(Beta3)[,s]+X[duration==l,]%*%t(cbind(Intercept3[s,l], Insider3[s,l], Rooted3[s,l], Ideologically.Novel3[s,l], OriginNew3[s,l], Interaction3[s,l]))))
)


Dissolution.1[duration==l,]<-sapply(1:nrow(Beta2), function(s) exp(W.1[duration==l,]%*%t(Beta3)[,s]+X[duration==l,]%*%t(cbind(Intercept3[s,l], Insider3[s,l], Rooted3[s,l], Ideologically.Novel3[s,l], OriginNew3[s,l], Interaction3[s,l])))/
(1+exp(W.1[duration==l,]%*%t(Beta2)[,s]+X[duration==l,]%*%t(cbind(Intercept2[s,l], Insider2[s,l], Rooted2[s,l], Ideologically.Novel2[s,l], OriginNew2[s,l], Interaction2[s,l])))+
exp(W.1[duration==l,]%*%t(Beta3)[,s]+X[duration==l,]%*%t(cbind(Intercept3[s,l], Insider3[s,l], Rooted3[s,l], Ideologically.Novel3[s,l], OriginNew3[s,l], Interaction3[s,l]))))
)



}

Marginal.prob.controls[max(grep(colnames(W)[dummies[j]],Marginal.prob.controls[,2])),3:ncol(Marginal.prob.controls)]<-c(mean(colMeans(Merger.1)-colMeans(Merger.0)),
		HPDinterval(as.mcmc(colMeans(Merger.1)-colMeans(Merger.0)), p=0.95))


Marginal.prob.controls[min(grep(colnames(W)[dummies[j]],Marginal.prob.controls[,2])),3:ncol(Marginal.prob.controls)]<-c(mean(colMeans(Dissolution.1)-colMeans(Dissolution.0)),
		HPDinterval(as.mcmc(colMeans(Dissolution.1)-colMeans(Dissolution.0)), p=0.95))



}





for (j in 1:length(continuous)) {

Merger.0<-matrix(NA, nrow=nrow(W), ncol=nrow(Beta2))
Merger.1<-matrix(NA, nrow=nrow(W), ncol=nrow(Beta2))
Dissolution.0<-matrix(NA, nrow=nrow(W), ncol=nrow(Beta2))
Dissolution.1<-matrix(NA, nrow=nrow(W), ncol=nrow(Beta2))

W.0<-W
W.1<-W
W.1[,continuous[j]]<-W.0[,continuous[j]]+sd(W.0[,continuous[j]])



for (l in 1:num.duration) {

Merger.0[duration==l,]<-sapply(1:nrow(Beta2), function(s) exp(W.0[duration==l,]%*%t(Beta2)[,s]+X[duration==l,]%*%t(cbind(Intercept2[s,l], Insider2[s,l], Rooted2[s,l], Ideologically.Novel2[s,l], OriginNew2[s,l], Interaction2[s,l])))/
(1+exp(W.0[duration==l,]%*%t(Beta2)[,s]+X[duration==l,]%*%t(cbind(Intercept2[s,l], Insider2[s,l], Rooted2[s,l], Ideologically.Novel2[s,l], OriginNew2[s,l], Interaction2[s,l])))+
exp(W.0[duration==l,]%*%t(Beta3)[,s]+X[duration==l,]%*%t(cbind(Intercept3[s,l], Insider3[s,l], Rooted3[s,l], Ideologically.Novel3[s,l], OriginNew3[s,l], Interaction3[s,l]))))
)


Merger.1[duration==l,]<-sapply(1:nrow(Beta2), function(s) exp(W.1[duration==l,]%*%t(Beta2)[,s]+X[duration==l,]%*%t(cbind(Intercept2[s,l], Insider2[s,l], Rooted2[s,l], Ideologically.Novel2[s,l], OriginNew2[s,l], Interaction2[s,l])))/
(1+exp(W.1[duration==l,]%*%t(Beta2)[,s]+X[duration==l,]%*%t(cbind(Intercept2[s,l], Insider2[s,l], Rooted2[s,l], Ideologically.Novel2[s,l], OriginNew2[s,l], Interaction2[s,l])))+
exp(W.1[duration==l,]%*%t(Beta3)[,s]+X[duration==l,]%*%t(cbind(Intercept3[s,l], Insider3[s,l], Rooted3[s,l], Ideologically.Novel3[s,l], OriginNew3[s,l], Interaction3[s,l]))))
)


Dissolution.0[duration==l,]<-sapply(1:nrow(Beta2), function(s) exp(W.0[duration==l,]%*%t(Beta3)[,s]+X[duration==l,]%*%t(cbind(Intercept3[s,l], Insider3[s,l], Rooted3[s,l], Ideologically.Novel3[s,l], OriginNew3[s,l], Interaction3[s,l])))/
(1+exp(W.0[duration==l,]%*%t(Beta2)[,s]+X[duration==l,]%*%t(cbind(Intercept2[s,l], Insider2[s,l], Rooted2[s,l], Ideologically.Novel2[s,l], OriginNew2[s,l], Interaction2[s,l])))+
exp(W.0[duration==l,]%*%t(Beta3)[,s]+X[duration==l,]%*%t(cbind(Intercept3[s,l], Insider3[s,l], Rooted3[s,l], Ideologically.Novel3[s,l], OriginNew3[s,l], Interaction3[s,l]))))
)


Dissolution.1[duration==l,]<-sapply(1:nrow(Beta2), function(s) exp(W.1[duration==l,]%*%t(Beta3)[,s]+X[duration==l,]%*%t(cbind(Intercept3[s,l], Insider3[s,l], Rooted3[s,l], Ideologically.Novel3[s,l], OriginNew3[s,l], Interaction3[s,l])))/
(1+exp(W.1[duration==l,]%*%t(Beta2)[,s]+X[duration==l,]%*%t(cbind(Intercept2[s,l], Insider2[s,l], Rooted2[s,l], Ideologically.Novel2[s,l], OriginNew2[s,l], Interaction2[s,l])))+
exp(W.1[duration==l,]%*%t(Beta3)[,s]+X[duration==l,]%*%t(cbind(Intercept3[s,l], Insider3[s,l], Rooted3[s,l], Ideologically.Novel3[s,l], OriginNew3[s,l], Interaction3[s,l]))))
)


}




Marginal.prob.controls[max(grep(colnames(W)[continuous[j]],Marginal.prob.controls[,2])),3:ncol(Marginal.prob.controls)]<-c(mean(colMeans(Merger.1)-colMeans(Merger.0)),
		HPDinterval(as.mcmc(colMeans(Merger.1)-colMeans(Merger.0)), p=0.95))


Marginal.prob.controls[min(grep(colnames(W)[continuous[j]],Marginal.prob.controls[,2])),3:ncol(Marginal.prob.controls)]<-c(mean(colMeans(Dissolution.1)-colMeans(Dissolution.0)),
		HPDinterval(as.mcmc(colMeans(Dissolution.1)-colMeans(Dissolution.0)), p=0.95))


}




Marginal.prob.controls[,3:ncol(Marginal.prob.controls)]<-100*as.numeric(Marginal.prob.controls[,3:ncol(Marginal.prob.controls)])


X.1<-X
X.1[,5]<-0
X.1[,4]<-0
X.1[,6]<-0



X.0<-X
X.0[,4]<-0
X.0[,5]<-1
X.0[,6]<-1


X.2<-X
X.2[,4]<-0
X.2[,5]<-1
X.2[,6]<-0


Dissolution.1<-sapply(1:nrow(Beta2), function(s) mean(
exp(W%*%t(Beta3)[,s]+X.1%*%Origin.Averages.3[s,])/(1+
exp(W%*%t(Beta3)[,s]+X.1%*%Origin.Averages.3[s,])+
exp(W%*%t(Beta2)[,s]+X.1%*%Origin.Averages.2[s,]))))

Dissolution.0<-sapply(1:nrow(Beta2), function(s) mean(
exp(W%*%t(Beta3)[,s]+X.0%*%Origin.Averages.3[s,])/(1+
exp(W%*%t(Beta3)[,s]+X.0%*%Origin.Averages.3[s,])+
exp(W%*%t(Beta2)[,s]+X.0%*%Origin.Averages.2[s,]))))


Dissolution.2<-sapply(1:nrow(Beta2), function(s) mean(
exp(W%*%t(Beta3)[,s]+X.2%*%Origin.Averages.3[s,])/(1+
exp(W%*%t(Beta3)[,s]+X.2%*%Origin.Averages.3[s,])+
exp(W%*%t(Beta2)[,s]+X.2%*%Origin.Averages.2[s,]))))


Merger.1<-sapply(1:nrow(Beta2), function(s) mean(
exp(W%*%t(Beta2)[,s]+X.1%*%Origin.Averages.2[s,])/(1+
exp(W%*%t(Beta3)[,s]+X.1%*%Origin.Averages.3[s,])+
exp(W%*%t(Beta2)[,s]+X.1%*%Origin.Averages.2[s,]))))


Merger.0<-sapply(1:nrow(Beta2), function(s) mean(
exp(W%*%t(Beta2)[,s]+X.0%*%Origin.Averages.2[s,])/(1+
exp(W%*%t(Beta3)[,s]+X.0%*%Origin.Averages.3[s,])+
exp(W%*%t(Beta2)[,s]+X.0%*%Origin.Averages.2[s,]))))

Merger.2<-sapply(1:nrow(Beta2), function(s) mean(
exp(W%*%t(Beta2)[,s]+X.2%*%Origin.Averages.2[s,])/(1+
exp(W%*%t(Beta3)[,s]+X.2%*%Origin.Averages.3[s,])+
exp(W%*%t(Beta2)[,s]+X.2%*%Origin.Averages.2[s,]))))



# Save to be used in Figure A.7
F.A.7<-cbind(c(rep("Dissolution", 3), rep("Merger", 3)),
rep(c("Organizationally Rooted &
 Not Ideologically Novel", "Built from scratch & 
 Ideologically Novel", "Built from scratch & Not
 Ideologically Novel"),2),
rbind(c(mean(Dissolution.1), HPDinterval(as.mcmc(Dissolution.1), p=0.95)),
c(mean(Dissolution.0), HPDinterval(as.mcmc(Dissolution.0), p=0.95)),
c(mean(Dissolution.2), HPDinterval(as.mcmc(Dissolution.2), p=0.95)),
c(mean(Merger.1), HPDinterval(as.mcmc(Merger.1), p=0.95)),
c(mean(Merger.0), HPDinterval(as.mcmc(Merger.0), p=0.95)),
c(mean(Merger.2), HPDinterval(as.mcmc(Merger.2), p=0.95))
)
)


F.A.7[,3:ncol(F.A.7)]<-100*as.numeric(F.A.7[,3:ncol(F.A.7)])

F.A.7<-as.data.frame(F.A.7)

names(F.A.7)<-c("Dep.Variable", "Covariate", "Mean","Lo.CI", "Hi.CI")

for (k in 3:ncol(F.A.7)) {
F.A.7[,k]<-as.numeric(F.A.7[,k])
}

save(F.A.7, file="FA7")



Marginal.Effects.Table2<-rbind(Marginal.prob.origin, Marginal.prob.controls)
Marginal.Effects.Table2<-data.frame(Marginal.Effects.Table2)
Dissolution<-Marginal.Effects.Table2[Marginal.Effects.Table2$Dep.Variable=="Dissolution",]
Dissolution<-Dissolution[c(1:4, 6,5,7,10,8:9,18,12,13,11,14,17,15,16),] 
Merger<-Marginal.Effects.Table2[Marginal.Effects.Table2$Dep.Variable=="Merger",]
Merger<-Merger[c(1:4, 6,5,7,10,8:9,18,12,13,11,14,17,15,16),] 


Dissolution.Matrix<-matrix(0, 18,4)
Dissolution.Matrix[,1]<-c("Insider Status",
		"Societal Rootedness", "Ideological Novelty",
 		"Roots in Pre-existing Parties", "Seat Share",
		"Electoral Performance", "National Government",
		"State Funding", "EP Access", "Regional Government",
		"Party Distinctiveness", "Party Electoral Coalition",
		"Anti-establishment Vote", "Party System Fragmentation",
		"Seat Product", "Pre-Election Year", "Election Year",
		"Post-Election Year")
Dissolution.Matrix[,2]<-"Dissolution"


for (j in 1:nrow(Dissolution.Matrix)) {

m<-round(as.numeric(Dissolution[j,3]), digits=2)
q.l<-round(as.numeric(Dissolution[j,4]), digits=2)
q.h<-round(as.numeric(Dissolution[j,5]), digits=2)


if (nchar(m)<5 & grepl("\\.", m)==FALSE) {
m<-paste(m,".", sep="")
while (nchar(m)<5) {
m<-paste(m, "0", sep="")
} 
} else if (nchar(m)<4 & grepl("\\.", m)==TRUE) {
while (nchar(m)<4) {
m<-paste(m, "0", sep="")
} 
}


if (nchar(q.l)<5 & grepl("\\.", q.l)==FALSE) {
q.l<-paste(q.l,".", sep="")
while (nchar(q.l)<5) {
q.l<-paste(q.l, "0", sep="")
} 
} else if (nchar(q.l)<4 & grepl("\\.", q.l)==TRUE) {
while (nchar(q.l)<4) {
q.l<-paste(q.l, "0", sep="")
} 
}

if (nchar(q.h)<5 & grepl("\\.", q.h)==FALSE) {
q.h<-paste(q.h,".", sep="")
while (nchar(q.h)<5) {
q.h<-paste(q.h, "0", sep="")
} 
} else if (nchar(q.h)<4 & grepl("\\.", q.h)==TRUE) {
while (nchar(q.h)<4) {
q.h<-paste(q.h, "0", sep="")
} 
}

Dissolution.Matrix[j,3:4]<-c(m, paste(paste("(", q.l, ",", sep=""), paste(q.h,")", sep=""),sep=""))


}



Merger.Matrix<-matrix(0, 18,4)
Merger.Matrix[,1]<-c("Insider Status",
		"Societal Rootedness", "Ideological Novelty",
 		"Roots in Pre-existing Parties", "Seat Share",
		"Electoral Performance", "National Government",
		"State Funding", "EP Access", "Regional Government",
		"Party Distinctiveness", "Party Electoral Coalition",
		"Anti-establishment Vote", "Party System Fragmentation",
		"Seat Product", "Pre-Election Year", "Election Year",
		"Post-Election Year")
Merger.Matrix[,2]<-"Merger"


for (j in 1:nrow(Merger.Matrix)) {

m<-round(as.numeric(Merger[j,3]), digits=2)
q.l<-round(as.numeric(Merger[j,4]), digits=2)
q.h<-round(as.numeric(Merger[j,5]), digits=2)


if (nchar(m)<5 & grepl("\\.", m)==FALSE) {
m<-paste(m,".", sep="")
while (nchar(m)<5) {
m<-paste(m, "0", sep="")
} 
} else if (nchar(m)<4 & grepl("\\.", m)==TRUE) {
while (nchar(m)<4) {
m<-paste(m, "0", sep="")
} 
}


if (nchar(q.l)<5 & grepl("\\.", q.l)==FALSE) {
q.l<-paste(q.l,".", sep="")
while (nchar(q.l)<5) {
q.l<-paste(q.l, "0", sep="")
} 
} else if (nchar(q.l)<4 & grepl("\\.", q.l)==TRUE) {
while (nchar(q.l)<4) {
q.l<-paste(q.l, "0", sep="")
} 
}

if (nchar(q.h)<5 & grepl("\\.", q.h)==FALSE) {
q.h<-paste(q.h,".", sep="")
while (nchar(q.h)<5) {
q.h<-paste(q.h, "0", sep="")
} 
} else if (nchar(q.h)<4 & grepl("\\.", q.h)==TRUE) {
while (nchar(q.h)<4) {
q.h<-paste(q.h, "0", sep="")
} 
}

Merger.Matrix[j,3:4]<-c(m, paste(paste("(", q.l, ",", sep=""), paste(q.h,")", sep=""),sep=""))


}


Table2<-rbind(Dissolution.Matrix,Merger.Matrix)
colnames(Table2)<-c("Variable", "Type of Death", "Mean", "Interval")
print(Table2)


groups<-c(rep("",2), "Formative Features", "", rep("",3), "Party-level Controls", rep("",4), rep("",3), "System-level Controls", rep("",2))

Table.2<-cbind(groups, Dissolution.Matrix[,-c(2)],Merger.Matrix[,3:4])
colnames(Table.2)<-NULL



library(grid)
library(gridExtra)
library(gtable)

Titles<-matrix(c("", "Variable", "Dissolution", "Death", "Merger", "Death"),nrow=1)

g1 <- tableGrob(Table.2, rows = NULL)
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 1, b = nrow(g1), l = 1, r = ncol(g1))
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
       t = 0, b = nrow(g1), l = 1, r = 1)
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 0, b = nrow(g1), l = 2, r = 4)
g1 <- gtable_add_grob(g1,
       grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 0, l = 1, r = ncol(g1))

g1 <- gtable_add_grob(g1,
        grobs = segmentsGrob(
	x0 = unit(0,"npc"),
                       y0 = unit(0,"npc"),
                       x1 = unit(0,"npc"),
                       y1 = unit(4,"npc"),
                       gp = gpar(lwd = 1.0)),
        t = 0, b = nrow(g1), l = 3, r = 3)	


g1 <- gtable_add_grob(g1,
        grobs = segmentsGrob(
	x0 = unit(0,"npc"),
                       y0 = unit(0,"npc"),
                       x1 = unit(4,"npc"),
                       y1 = unit(0,"npc"),
                       gp = gpar(lwd = 1.0)),
        t = 4, b=4, l = 1, r = ncol(g1))	


g1 <- gtable_add_grob(g1,
        grobs = segmentsGrob(
	x0 = unit(0,"npc"),
                       y0 = unit(0,"npc"),
                       x1 = unit(4,"npc"),
                       y1 = unit(0,"npc"),
                       gp = gpar(lwd = 1.0)),
        t = 12, b=12, l = 1, r = ncol(g1))	


g0 <- tableGrob(Titles, rows = NULL)

g<-rbind(g0,g1)


pdf(file = "Table2.pdf", height=10, width=10)
grid.draw(g)
dev.off()


############################################################# Figure 1 ###############################################################################################################################

Figure1<-matrix(NA, nrow=2*2, ncol=5)
colnames(Figure1)<-c("Dep.Variable", "Covariate",  "Mean","Lo.CI", "Hi.CI")
Figure1[,1]<-rep(c("Dissolution", "Merger"),2)
Figure1[,2]<-rep(c("Wo.Roots","W.Roots"),each=2)


X.0<-X
X.1<-X


X.0[,4]<-0
X.0[,5]<-0
X.0[,6]<-0
X.1[,4]<-1
X.1[,5]<-1
X.1[,6]<-0


Dissolution<-sapply(1:nrow(Beta2), function(s) mean(
exp(W%*%t(Beta3)[,s]+X.1%*%Origin.Averages.3[s,])/(1+
exp(W%*%t(Beta3)[,s]+X.1%*%Origin.Averages.3[s,])+
exp(W%*%t(Beta2)[,s]+X.1%*%Origin.Averages.2[s,])) -
exp(W%*%t(Beta3)[,s]+X.0%*%Origin.Averages.3[s,])/(1+
exp(W%*%t(Beta3)[,s]+X.0%*%Origin.Averages.3[s,])+
exp(W%*%t(Beta2)[,s]+X.0%*%Origin.Averages.2[s,])))
)

Merger<-sapply(1:nrow(Beta2), function(s) mean(
exp(W%*%t(Beta2)[,s]+X.1%*%Origin.Averages.2[s,])/(1+
exp(W%*%t(Beta3)[,s]+X.1%*%Origin.Averages.3[s,])+
exp(W%*%t(Beta2)[,s]+X.1%*%Origin.Averages.2[s,])) -
exp(W%*%t(Beta2)[,s]+X.0%*%Origin.Averages.2[s,])/(1+
exp(W%*%t(Beta3)[,s]+X.0%*%Origin.Averages.3[s,])+
exp(W%*%t(Beta2)[,s]+X.0%*%Origin.Averages.2[s,])))
)


Figure1[3,3:ncol(Figure1)]<-c(mean(Dissolution),
		HPDinterval(as.mcmc(Dissolution), p=0.95))
Figure1[2,3:ncol(Figure1)]<-c(mean(Merger),
		HPDinterval(as.mcmc(Merger), p=0.95))



X.0<-X
X.1<-X

X.0[,4]<-0
X.0[,5]<-0
X.0[,6]<-0
X.1[,4]<-1
X.1[,5]<-0
X.1[,6]<-0




Dissolution<-sapply(1:nrow(Beta2), function(s) mean(
exp(W%*%t(Beta3)[,s]+X.1%*%Origin.Averages.3[s,])/(1+
exp(W%*%t(Beta3)[,s]+X.1%*%Origin.Averages.3[s,])+
exp(W%*%t(Beta2)[,s]+X.1%*%Origin.Averages.2[s,])) -
exp(W%*%t(Beta3)[,s]+X.0%*%Origin.Averages.3[s,])/(1+
exp(W%*%t(Beta3)[,s]+X.0%*%Origin.Averages.3[s,])+
exp(W%*%t(Beta2)[,s]+X.0%*%Origin.Averages.2[s,])))
)

Merger<-sapply(1:nrow(Beta2), function(s) mean(
exp(W%*%t(Beta2)[,s]+X.1%*%Origin.Averages.2[s,])/(1+
exp(W%*%t(Beta3)[,s]+X.1%*%Origin.Averages.3[s,])+
exp(W%*%t(Beta2)[,s]+X.1%*%Origin.Averages.2[s,])) -
exp(W%*%t(Beta2)[,s]+X.0%*%Origin.Averages.2[s,])/(1+
exp(W%*%t(Beta3)[,s]+X.0%*%Origin.Averages.3[s,])+
exp(W%*%t(Beta2)[,s]+X.0%*%Origin.Averages.2[s,])))
)


Figure1[1,3:ncol(Figure1)]<-c(mean(Dissolution),
		HPDinterval(as.mcmc(Dissolution), p=0.95))
Figure1[4,3:ncol(Figure1)]<-c(mean(Merger),
		HPDinterval(as.mcmc(Merger), p=0.95))


Figure1<-data.frame(Figure1)

Figure1$Covariate<- factor(Figure1$Covariate, 
		      levels=c("W.Roots",
				 "Wo.Roots"),
			labels = c("with Roots in
Pre-existing 
Parties", "without Roots in
Pre-existing 
Parties"))


Figure1$Dep.Variable<-factor(Figure1$Dep.Variable, 
				levels=c("Dissolution", "Merger"),
				labels=c("Dissolution Death", "Merger Death"))

for (k in 3:ncol(Figure1)) {
Figure1[,k]<-100*as.numeric(Figure1[,k])
}



library(ggplot2)


p<-ggplot(Figure1,
 aes(x=Covariate,y=Mean)) + 
    geom_errorbar(aes(ymin=Lo.CI, 
	ymax=Hi.CI, width=.2), size=1,colour="black") + 
	facet_wrap(~Dep.Variable, scales="free")+
    geom_point(size=3)+
theme_bw()+ylab("")+
xlab("Ideologically Novel Parties")+
theme(axis.title.x = element_text(vjust = -2, size=11, face="bold")) +    
theme(strip.text.x = element_text(size = 12, face="bold"))+
theme(axis.text.y = element_text(size=10)) +
 theme(axis.text.x=element_text(size=10,color="black"))+
theme(plot.margin = margin(0.1, 0.1, .3, -0.2, "cm"))+
geom_hline(yintercept=0, linetype="dashed", 
                color = "gray", size=1)


pdf(file = "Figure1.pdf")
grid.draw(p)
dev.off()



############################################################# Table 3 ###############################################################################################################################

# In order to produce Table 3, we first need to compute the goodness of fit measures of the
# State-space, Cox and Log-normal models (in processing the Cox and Log-normal models, 
# we will also obtain the marginal effects displayed in Figure 4) 


### Goodness of fit measures - State-space model


l.Statespace<-c()
lp.Statespace<-c()
p.Statespace<-c()

for (i in 1:length(unique(df$case_id))) {


X.id<-matrix(X[df$case_id==unique(df$case_id)[i],], ncol=ncol(X))
W.id<-matrix(W[df$case_id==unique(df$case_id)[i],], ncol=ncol(W))
duration.id<-sort(duration[df$case_id==unique(df$case_id)[i]])
duration.id<-1:length(duration.id)
Y.id<-matrix(Y[df$case_id==unique(df$case_id)[i],],ncol=3)
Reffect.Party2.id<-Reffect.Party2[, which(unique(df$case_id)==unique(df$case_id)[i])]
Reffect.Party3.id<-Reffect.Party3[, which(unique(df$case_id)==unique(df$case_id)[i])]
Reffect.Country2.id<-Reffect.Country2[, unique(country[df$case_id==unique(df$case_id)[i]])]
Reffect.Country3.id<-Reffect.Country3[, unique(country[df$case_id==unique(df$case_id)[i]])]

for (l in 1:length(duration.id)){

p.1<-sapply(1:nrow(Beta2), function(s) 1/(1+exp(W.id[duration.id==l,]%*%matrix(Beta2[s,])+
        Reffect.Party2.id[s]+Reffect.Country2.id[s]+
	X.id[duration.id==l,]%*%matrix(cbind(Intercept2[s,l], Insider2[s,l], Rooted2[s,l],
	Ideologically.Novel2[s,l], OriginNew2[s,l],
	Interaction2[s,l])))+exp(W.id[duration.id==l,]%*%matrix(Beta3[s,])+
	Reffect.Party3.id[s]+Reffect.Country3.id[s]+
	X.id[duration.id==l,]%*%matrix(cbind(Intercept3[s,l], Insider3[s,l], Rooted3[s,l],
	Ideologically.Novel3[s,l], OriginNew3[s,l],
	Interaction3[s,l])))))



p.2<-sapply(1:nrow(Beta2), function(s) exp(W.id[duration.id==l,]%*%matrix(Beta2[s,])+
	Reffect.Party2.id[s]+Reffect.Country2.id[s]+
	X.id[duration.id==l,]%*%matrix(cbind(Intercept2[s,l], Insider2[s,l], Rooted2[s,l],
	Ideologically.Novel2[s,l], OriginNew2[s,l],
	Interaction2[s,l])))/(1+exp(W.id[duration.id==l,]%*%matrix(Beta2[s,])+
	Reffect.Party2.id[s]+Reffect.Country2.id[s]+
	X.id[duration.id==l,]%*%matrix(cbind(Intercept2[s,l], Insider2[s,l], Rooted2[s,l],
	Ideologically.Novel2[s,l], OriginNew2[s,l],
	Interaction2[s,l])))+exp(W.id[duration.id==l,]%*%matrix(Beta3[s,])+
	Reffect.Party3.id[s]+Reffect.Country3.id[s]+
	X.id[duration.id==l,]%*%matrix(cbind(Intercept3[s,l], Insider3[s,l], Rooted3[s,l],
	Ideologically.Novel3[s,l], OriginNew3[s,l],
	Interaction3[s,l])))))


p.3<-sapply(1:nrow(Beta2), function(s) exp(W.id[duration.id==l,]%*%matrix(Beta3[s,])+
	Reffect.Party3.id[s]+Reffect.Country3.id[s]+
	X.id[duration.id==l,]%*%matrix(cbind(Intercept3[s,l], Insider3[s,l], Rooted3[s,l],
	Ideologically.Novel3[s,l], OriginNew3[s,l],
	Interaction3[s,l])))/(1+exp(W.id[duration.id==l,]%*%matrix(Beta2[s,])+
	Reffect.Party2.id[s]+Reffect.Country2.id[s]+
	X.id[duration.id==l,]%*%matrix(cbind(Intercept2[s,l], Insider2[s,l], Rooted2[s,l],
	Ideologically.Novel2[s,l], OriginNew2[s,l],
	Interaction2[s,l])))+exp(W.id[duration.id==l,]%*%matrix(Beta3[s,])+
	Reffect.Party3.id[s]+Reffect.Country3.id[s]+
	X.id[duration.id==l,]%*%matrix(cbind(Intercept3[s,l], Insider3[s,l], Rooted3[s,l],
	Ideologically.Novel3[s,l], OriginNew3[s,l],
	Interaction3[s,l])))))


a<-mean(sapply(1:nrow(Beta2), function(s) ((p.1[s]^Y.id[duration.id==l,1])*(p.2[s]^Y.id[duration.id==l,2])*(p.3[s]^Y.id[duration.id==l,3]))))


b<-mean(log(sapply(1:nrow(Beta2), function(s) ((p.1[s]^Y.id[duration.id==l,1])*(p.2[s]^Y.id[duration.id==l,2])*(p.3[s]^Y.id[duration.id==l,3])))))



l.Statespace<-c(l.Statespace, b)


lp.Statespace<-c(lp.Statespace, a)

p.Statespace<-c(p.Statespace, log(a)-b)



}

}


Table.3.Statespace<-c(-2*sum(l.Statespace)+2*(2*ncol(X)), -2*sum(l.Statespace)+log(nrow(Y))*(2*ncol(X)), 
		    -2*sum(l.Statespace)+(log(nrow(Y))+1)*(2*ncol(X)), 
		     -2*(sum(log(lp.Statespace))+2*sum(p.Statespace)))




library(survival)

G.M <- survfit(Surv(year_exist, merged) ~ 1, data=df)
G.D <- survfit(Surv(year_exist, dissolved) ~ 1, data=df)


df$T.D<-NA
df$T.M<-NA
df$Delta.D<-NA
df$Delta.M<-NA


for (i in 1:length(unique(df$case_id))) {

if (any(df$merged[df$case_id==unique(df$case_id)[i]]==1)==FALSE) {
df$T.M[df$case_id==unique(df$case_id)[i]]<-max(df$year_exist[df$case_id==unique(df$case_id)[i]])
df$Delta.M[df$case_id==unique(df$case_id)[i]]<-0
} else if (any(df$merged[df$case_id==unique(df$case_id)[i]]==1)==TRUE) {
df$T.M[df$case_id==unique(df$case_id)[i]]<-df$year_exist[df$case_id==unique(df$case_id)[i] & df$merged==1]
df$Delta.M[df$case_id==unique(df$case_id)[i]]<-1
}

if (any(df$dissolved[df$case_id==unique(df$case_id)[i]]==1)==FALSE) {
df$T.D[df$case_id==unique(df$case_id)[i]]<-max(df$year_exist[df$case_id==unique(df$case_id)[i]])
df$Delta.D[df$case_id==unique(df$case_id)[i]]<-0
} else if (any(df$dissolved[df$case_id==unique(df$case_id)[i]]==1)==TRUE) {
df$T.D[df$case_id==unique(df$case_id)[i]]<-df$year_exist[df$case_id==unique(df$case_id)[i] & df$dissolved==1]
df$Delta.D[df$case_id==unique(df$case_id)[i]]<-1
}

}




AUC.D<-c()
AUC.M<-c()
S<-c()
P.M<-c()
P.D<-c()

for (l in 1:length(unique(duration))) {

T.D.id<-df$T.D[duration==l]
T.M.id<-df$T.M[duration==l]
Delta.D.id<-df$Delta.D[duration==l]
Delta.M.id<-df$Delta.M[duration==l]
X.id<-matrix(X[duration==l,], ncol=ncol(X))
W.id<-matrix(W[duration==l,], ncol=ncol(W))
Y.id<-matrix(Y[duration==l,],ncol=3)
Reffect.Party2.id<-Reffect.Party2[, sort(unique(party[duration==l]))]
Reffect.Party3.id<-Reffect.Party3[, sort(unique(party[duration==l]))]
Reffect.Country2.id<-Reffect.Country2[, sort(unique(country[duration==l]))]
Reffect.Country3.id<-Reffect.Country3[, sort(unique(country[duration==l]))]


p.2<-sapply(1:nrow(Beta2), function(s) exp(W.id%*%matrix(Beta2[s,])+
	Reffect.Party2.id[s]+Reffect.Country2.id[s]+
	X.id%*%matrix(cbind(Intercept2[s,l], Insider2[s,l], Rooted2[s,l],
	Ideologically.Novel2[s,l], OriginNew2[s,l],
	Interaction2[s,l])))/(1+exp(W.id%*%matrix(Beta2[s,])+
	Reffect.Party2.id[s]+Reffect.Country2.id[s]+
	X.id%*%matrix(cbind(Intercept2[s,l], Insider2[s,l], Rooted2[s,l],
	Ideologically.Novel2[s,l], OriginNew2[s,l],
	Interaction2[s,l])))+exp(W.id%*%matrix(Beta3[s,])+
	Reffect.Party3.id[s]+Reffect.Country3.id[s]+
	X.id%*%matrix(cbind(Intercept3[s,l], Insider3[s,l], Rooted3[s,l],
	Ideologically.Novel3[s,l], OriginNew3[s,l],
	Interaction3[s,l])))))



p.3<-sapply(1:nrow(Beta2), function(s) exp(W.id%*%matrix(Beta3[s,])+
	Reffect.Party3.id[s]+Reffect.Country3.id[s]+
	X.id%*%matrix(cbind(Intercept3[s,l], Insider3[s,l], Rooted3[s,l],
	Ideologically.Novel3[s,l], OriginNew3[s,l],
	Interaction3[s,l])))/(1+exp(W.id%*%matrix(Beta2[s,])+
	Reffect.Party2.id[s]+Reffect.Country2.id[s]+
	X.id%*%matrix(cbind(Intercept2[s,l], Insider2[s,l], Rooted2[s,l],
	Ideologically.Novel2[s,l], OriginNew2[s,l],
	Interaction2[s,l])))+exp(W.id%*%matrix(Beta3[s,])+
	Reffect.Party3.id[s]+Reffect.Country3.id[s]+
	X.id%*%matrix(cbind(Intercept3[s,l], Insider3[s,l], Rooted3[s,l],
	Ideologically.Novel3[s,l], OriginNew3[s,l],
	Interaction3[s,l])))))


W.D<-sapply(1:nrow(X.id), function(i) ifelse(Delta.D.id[i]==0, as.numeric(T.D.id[i]>l)/summary(G.D)[[6]][which(summary(G.D)[[2]]==l)],
	as.numeric(T.D.id[i]<=l)/summary(G.D)[[6]][which(summary(G.D)[[2]]==T.D.id[i])]))

W.M<-sapply(1:nrow(X.id), function(i) ifelse(Delta.M.id[i]==0, as.numeric(T.M.id[i]>l)/summary(G.M)[[6]][which(summary(G.M)[[2]]==l)],
	as.numeric(T.M.id[i]<=l)/summary(G.M)[[6]][which(summary(G.M)[[2]]==T.M.id[i])]))


AUC.D.numerator<-sapply(1:nrow(X.id), function(i) sapply((1:nrow(X.id))[-i], function(j) (as.numeric(mean(p.3[i,])>mean(p.3[j,]))+0.5*(as.numeric(mean(p.3[i,])==mean(p.3[j,]))))*
W.D[i]*W.D[j]* Y.id[i,3]*(1-Y.id[j,3])))

AUC.D.denominator<-sapply(1:nrow(X.id), function(i) sapply((1:nrow(X.id))[-i], function(j) W.D[i]*W.D[j]*
Y.id[i,3]*(1-Y.id[j,3])))

AUC.D<-c(AUC.D, sum(rowSums(t(AUC.D.numerator)))/sum(rowSums(t(AUC.D.denominator))))

AUC.M.numerator<-sapply(1:nrow(X.id), function(i) sapply((1:nrow(X.id))[-i], function(j) (as.numeric(mean(p.2[i,])>mean(p.2[j,]))+0.5*(as.numeric(mean(p.2[i,])==mean(p.2[j,]))))*
W.M[i]*W.M[j]*Y.id[i,2]*(1-Y.id[j,2])))


AUC.M.denominator<-sapply(1:nrow(X.id), function(i) sapply((1:nrow(X.id))[-i], function(j) W.M[i]*W.M[j]*
 Y.id[i,2]*(1-Y.id[j,2])))

AUC.M<-c(AUC.M, sum(rowSums(t(AUC.M.numerator)))/sum(rowSums(t(AUC.M.denominator))))


S<-c(S, 1-mean(p.2)-mean(p.3))

P.M<-c(P.M, mean(p.2))
P.D<-c(P.D, mean(p.3))



}


AUC.M<-AUC.M[!is.na(AUC.M)]
P.M<-P.M[!is.na(AUC.M)]
S.M<-S[!is.na(AUC.M)]

AUC.D<-AUC.D[!is.na(AUC.D)]
P.D<-P.D[!is.na(AUC.D)]
S.D<-S[!is.na(AUC.D)]


w.M.numerator<-sapply(1:length(AUC.M), function(t) P.M[t]*prod(S.M[1:(t-1)])*S.M[t] )
w.D.numerator<-sapply(1:length(AUC.D), function(t) P.D[t]*prod(S.D[1:(t-1)])*S.D[t] )


C.M<-sum(sapply(1:length(AUC.M), function(t) AUC.M[t]*w.M.numerator[t]/sum(w.M.numerator)),na.rm=T)
C.D<-sum(sapply(1:length(AUC.D), function(t) AUC.D[t]*w.D.numerator[t]/sum(w.D.numerator)),na.rm=T)



B.D<-c()
B.M<-c()
S<-c()
P.M<-c()
P.D<-c()

for (l in 1:length(unique(duration))) {

T.D.id<-df$T.D[duration==l]
T.M.id<-df$T.M[duration==l]
Delta.D.id<-df$Delta.D[duration==l]
Delta.M.id<-df$Delta.M[duration==l]
X.id<-matrix(X[duration==l,], ncol=ncol(X))
W.id<-matrix(W[duration==l,], ncol=ncol(W))
Y.id<-matrix(Y[duration==l,],ncol=3)
Reffect.Party2.id<-Reffect.Party2[, sort(unique(party[duration==l]))]
Reffect.Party3.id<-Reffect.Party3[, sort(unique(party[duration==l]))]
Reffect.Country2.id<-Reffect.Country2[, sort(unique(country[duration==l]))]
Reffect.Country3.id<-Reffect.Country3[, sort(unique(country[duration==l]))]


p.2<-sapply(1:nrow(Beta2), function(s) exp(W.id%*%matrix(Beta2[s,])+
	Reffect.Party2.id[s]+Reffect.Country2.id[s]+
	X.id%*%matrix(cbind(Intercept2[s,l], Insider2[s,l], Rooted2[s,l],
	Ideologically.Novel2[s,l], OriginNew2[s,l],
	Interaction2[s,l])))/(1+exp(W.id%*%matrix(Beta2[s,])+
	Reffect.Party2.id[s]+Reffect.Country2.id[s]+
	X.id%*%matrix(cbind(Intercept2[s,l], Insider2[s,l], Rooted2[s,l],
	Ideologically.Novel2[s,l], OriginNew2[s,l],
	Interaction2[s,l])))+exp(W.id%*%matrix(Beta3[s,])+
	Reffect.Party3.id[s]+Reffect.Country3.id[s]+
	X.id%*%matrix(cbind(Intercept3[s,l], Insider3[s,l], Rooted3[s,l],
	Ideologically.Novel3[s,l], OriginNew3[s,l],
	Interaction3[s,l])))))



p.3<-sapply(1:nrow(Beta2), function(s) exp(W.id%*%matrix(Beta3[s,])+
	Reffect.Party3.id[s]+Reffect.Country3.id[s]+
	X.id%*%matrix(cbind(Intercept3[s,l], Insider3[s,l], Rooted3[s,l],
	Ideologically.Novel3[s,l], OriginNew3[s,l],
	Interaction3[s,l])))/(1+exp(W.id%*%matrix(Beta2[s,])+
	Reffect.Party2.id[s]+Reffect.Country2.id[s]+
	X.id%*%matrix(cbind(Intercept2[s,l], Insider2[s,l], Rooted2[s,l],
	Ideologically.Novel2[s,l], OriginNew2[s,l],
	Interaction2[s,l])))+exp(W.id%*%matrix(Beta3[s,])+
	Reffect.Party3.id[s]+Reffect.Country3.id[s]+
	X.id%*%matrix(cbind(Intercept3[s,l], Insider3[s,l], Rooted3[s,l],
	Ideologically.Novel3[s,l], OriginNew3[s,l],
	Interaction3[s,l])))))





W.D<-sapply(1:nrow(X.id), function(i) ifelse(Delta.D.id[i]==0, as.numeric(T.D.id[i]>l)/summary(G.D)[[6]][which(summary(G.D)[[2]]==l)],
	as.numeric(T.D.id[i]<=l)/summary(G.D)[[6]][which(summary(G.D)[[2]]==T.D.id[i])]))

W.M<-sapply(1:nrow(X.id), function(i) ifelse(Delta.M.id[i]==0, as.numeric(T.M.id[i]>l)/summary(G.M)[[6]][which(summary(G.M)[[2]]==l)],
	as.numeric(T.M.id[i]<=l)/summary(G.M)[[6]][which(summary(G.M)[[2]]==T.M.id[i])]))



BS.M<-sapply(1:nrow(X.id), function(i) W.M[i]* ((Y.id[i,2]-mean(p.2[i,]))^2))
BS.D<-sapply(1:nrow(X.id), function(i) W.D[i]*((Y.id[i,3]-mean(p.3[i,]))^2))



BS.M<- (1/nrow(X.id)*mean(T.M.id>l))*sum(BS.M)
BS.D<- (1/nrow(X.id)*mean(T.D.id>l))*sum(BS.D)


B.M<-c(B.M, BS.M)
B.D<-c(B.D, BS.D)

S<-c(S, 1-mean(p.2)-mean(p.3))

P.M<-c(P.M, mean(p.2))
P.D<-c(P.D, mean(p.3))



}

B.M<-B.M[!is.na(B.M)]
P.M<-P.M[!is.na(B.M)]
S.M<-S[!is.na(B.M)]

B.D<-B.D[!is.na(B.D)]
P.D<-P.D[!is.na(B.D)]
S.D<-S[!is.na(B.D)]


w.M<-sapply(1:length(B.M), function(t) P.M[t]*prod(S.M[1:(t-1)]))
w.D<-sapply(1:length(B.D), function(t) P.D[t]*prod(S.D[1:(t-1)]))


Table.3.Statespace<-c(Table.3.Statespace, 100*(sum(B.M*w.M)+sum(B.D*w.D)),  100*(C.M*sum(Y[,2])+C.D*sum(Y[,3]))/sum(colSums(Y)[-1]))
save(Table.3.Statespace, file="Table.3.Statespace")


rm(Table.3.Statespace, out, Dynamics.Statespace, F.A.7)



# Now we process the Cox proportional hazards model 

load("Estimates_Cox") 


# Convergence checks - Cox proportional hazards model
BETA2<-mcmc.list(as.mcmc(t(out$beta2[,,1])), as.mcmc(t(out$beta2[,,2])),
	 as.mcmc(t(out$beta2[,,3])))


BETA3<-mcmc.list(as.mcmc(t(out$beta3[,,1])), as.mcmc(t(out$beta3[,,2])),
	 as.mcmc(t(out$beta3[,,3])))


print(paste("Convergence, Parameter Estimates of the Cox model:", gelman.diag(BETA2)[[2]]<1.2 & gelman.diag(BETA3)[[2]]<1.2 , sep=" "))


# Pooled convergent draws - to be used for inference

Beta2<-rbind(t(out$beta2[,pool:20000 , 1]),
		t(out$beta2[,pool:20000, 2]),
		t(out$beta2[,pool:20000, 3]))


Beta3<-rbind(t(out$beta3[,pool:20000 , 1]),
		t(out$beta3[, pool:20000, 2]),
		t(out$beta3[, pool:20000, 3]))


Reffect.Country2<-rbind(t(out$reffect_country[1:22, pool:20000, 1]),
		t(out$reffect_country[1:22,pool:20000, 2]),
		t(out$reffect_country[1:22,pool:20000 , 3]))
Reffect.Country3<-rbind(t(out$reffect_country[23:44,pool:20000 , 1]),
		t(out$reffect_country[23:44,pool:20000 , 2]),
		t(out$reffect_country[23:44,pool:20000 , 3]))

Reffect.Party2<-rbind(t(out$reffect_party[1:204, pool:20000, 1]),
		t(out$reffect_party[1:204, pool:20000, 2]),
		t(out$reffect_party[1:204,pool:20000 , 3]))
Reffect.Party3<-rbind(t(out$reffect_party[205:408,pool:20000 , 1]),
		t(out$reffect_party[205:408, pool:20000 ,2]),
		t(out$reffect_party[205:408,pool:20000, 3]))



## Load data 
df<-read.csv("Dataset.csv")

df<-subset(df, select=c("votes.centered",
			"country",
			"case_id",
			"merged", "dissolved",
			"year_exist",
			"logseats.centered",
		 "government",
			 "EU_parl",
		 "reg_gov",
		 "finance_party_qualify",
		 "efNPP",
	 "elec_coalition", 
	 "indPOL",
	"election_year_dummy",
	"distinctiveness",
	 "insider", 
	 "organisation", 
	 "newfamfirst",
	"OriginNew",
	"post.election.year",	
	"pre.election.year", 
	"seat.product"
	 ))

df<-na.exclude(df)


country<-as.numeric(factor(df$country), levels=unique(df$country))
party<-as.numeric(factor(df$case_id), levels=unique(df$case_id))
num.country<-length(unique(country))
num.party<-length(unique(party))


# 1= survive, 2=merge, 3=dissolve
Y<-cbind(df$merged, df$dissolved)
Y<-cbind(ifelse(rowSums(Y)==0,1,0),Y)
y<-c(sapply(1:nrow(Y), function(i) which(Y[i,]==1)))
n<-length(y)          


Matrix.Country<-matrix(0,n,num.country)
for (i in 1:num.country) Matrix.Country[country==i,i]<-1

Matrix.Party<-matrix(0,n,num.party)
for (i in 1:num.party) Matrix.Party[party==i,i]<-1


Matrix.Outcome<-matrix(0, n, 3)
for (i in 1:3) Matrix.Outcome[y==i,i]<-1


df$OriginNew.newfamfirst<-df$OriginNew*df$newfamfirst

Z<-cbind(1, 
	 df$insider, 
	 df$organisation, 
       df$newfamfirst,
	 df$OriginNew,
	df$OriginNew.newfamfirst, 
	df$votes.centered, 
	 df$logseats.centered,  
	 df$government,
	 df$EU_parl,
	 df$reg_gov,  
	 df$finance_party_qualify,
	 (df$efNPP-mean(df$efNPP))/(2*sd(df$efNPP)),
	 df$elec_coalition, 
	 (df$indPOL-mean(df$indPOL))/(2*sd(df$indPOL)),
	 (df$seat.product-mean(df$seat.product))/(2*sd(df$seat.product)),
	df$election_year_dummy,
	df$post.election.year,	
	df$pre.election.year, 
	df$distinctiveness )



duration<-as.numeric(factor(df$year_exist), levels=unique(df$year_exist))
num.duration<-length(unique(duration))

Matrix.Duration<-matrix(0,n,num.duration)
for (i in 2:num.duration) Matrix.Duration[duration==i,i]<-1


X<-cbind(Matrix.Duration, Z)


colnames(X)<-c(paste("Year", 1:47, sep="-"), "Intercept", "Insider.Status", "Societal.Rootednes", "Ideological.Novelty",
		"Roots.in.Parties", "Interaction.Novelty.Roots", "Electoral.Performance", "Seat.Share", "National.Government", "EP.Access", "Regional.Government",
		"State.Funding", "System.Fragmentation", "Electoral.Coalition", "Anti-establishment.Vote",
		"Seat.Product", "Election.Year", "Post", "Pre",
		"Party.Distinctiveness")



# Computation of Marginal Effects of the Cox Model

Marginal.prob.origin<-matrix(NA, nrow=4*2, ncol=5)
colnames(Marginal.prob.origin)<-c("Dep.Variable", "Covariate", "Predictive Comparison",   "95% CI - Lower", "95% CI - Higher")
Marginal.prob.origin[,1]<-c(rep("Dissolution", 4), 
		rep("Merger", 4))
Marginal.prob.origin[,2]<-rep(c(colnames(X)[49:52]),2)


Marginal.prob.controls<-matrix(NA, nrow=(ncol(X[,-c(1:53)])*2), ncol=5)
colnames(Marginal.prob.controls)<-c("Dep.Variable", "Covariate", "Predictive Comparison",  "95% CI - Lower", "95% CI - Higher")
Marginal.prob.controls[,1]<-c(rep("Dissolution", ncol(X[,-c(1:53)])), 
		rep("Merger", ncol(X[,-c(1:53)])))
Marginal.prob.controls[,2]<-rep(colnames(X)[-c(1:53)],2)





dummies<-c(49:52)
 
for (j in 1:length(dummies)) {

if (j<=3) {
X.0<-X
X.0[,dummies[j]]<-0
X.1<-X
X.1[,dummies[j]]<-1
} else if (j==4) {
X.0<-X
X.0[,dummies[j]]<-1
X.1<-X
X.1[,dummies[j]]<-0
}



Dissolution.0<-sapply(1:nrow(Beta2), function(s) (exp(X.0%*%matrix(Beta3[s,]))/
(exp(X.0%*%matrix(Beta2[s,]))+
exp(X.0%*%matrix(Beta3[s,]))))*(1-
exp(-exp(X.0%*%matrix(Beta2[s,]))-
exp(X.0%*%matrix(Beta3[s,])))))

Dissolution.1<-sapply(1:nrow(Beta2), function(s) (exp(X.1%*%matrix(Beta3[s,]))/
(exp(X.1%*%matrix(Beta2[s,]))+
exp(X.1%*%matrix(Beta3[s,]))))*(1-
exp(-exp(X.1%*%matrix(Beta2[s,]))-
exp(X.1%*%matrix(Beta3[s,])))))


Merger.0<-sapply(1:nrow(Beta2), function(s)(exp(X.0%*%matrix(Beta2[s,]))/
(exp(X.0%*%matrix(Beta2[s,]))+
exp(X.0%*%matrix(Beta3[s,]))))*(1-
exp(-exp(X.0%*%matrix(Beta2[s,]))-
exp(X.0%*%matrix(Beta3[s,])))))


Merger.1<-sapply(1:nrow(Beta2), function(s)(exp(X.1%*%matrix(Beta2[s,]))/
(exp(X.1%*%matrix(Beta2[s,]))+
exp(X.1%*%matrix(Beta3[s,]))))*(1-
exp(-exp(X.1%*%matrix(Beta2[s,]))-
exp(X.1%*%matrix(Beta3[s,])))))



Marginal.prob.origin[max(grep(colnames(X)[dummies[j]],Marginal.prob.origin[,2])),3:ncol(Marginal.prob.origin)]<-c(mean(colMeans(Merger.1-Merger.0)),
		HPDinterval(as.mcmc(colMeans(Merger.1-Merger.0)), p=0.95))


Marginal.prob.origin[min(grep(colnames(X)[dummies[j]],Marginal.prob.origin[,2])),3:ncol(Marginal.prob.origin)]<-c(mean(colMeans(Dissolution.1-Dissolution.0)),
		HPDinterval(as.mcmc(colMeans(Dissolution.1-Dissolution.0)), p=0.95))


}

Marginal.prob.origin[,3:ncol(Marginal.prob.origin)]<-100*as.numeric(Marginal.prob.origin[,3:ncol(Marginal.prob.origin)])


rm(dummies)
dummies<-c(56:59, 61,64:67)
continuous<-c(54,55,60,62,63)  


for (j in 1:length(dummies)) {

X.0<-X
X.0[,dummies[j]]<-0
X.1<-X
X.1[,dummies[j]]<-1


Dissolution.0<-sapply(1:nrow(Beta2), function(s) (exp(X.0%*%matrix(Beta3[s,]))/
(exp(X.0%*%matrix(Beta2[s,]))+
exp(X.0%*%matrix(Beta3[s,]))))*(1-
exp(-exp(X.0%*%matrix(Beta2[s,]))-
exp(X.0%*%matrix(Beta3[s,])))))

Dissolution.1<-sapply(1:nrow(Beta2), function(s) (exp(X.1%*%matrix(Beta3[s,]))/
(exp(X.1%*%matrix(Beta2[s,]))+
exp(X.1%*%matrix(Beta3[s,]))))*(1-
exp(-exp(X.1%*%matrix(Beta2[s,]))-
exp(X.1%*%matrix(Beta3[s,])))))


Merger.0<-sapply(1:nrow(Beta2), function(s)(exp(X.0%*%matrix(Beta2[s,]))/
(exp(X.0%*%matrix(Beta2[s,]))+
exp(X.0%*%matrix(Beta3[s,]))))*(1-
exp(-exp(X.0%*%matrix(Beta2[s,]))-
exp(X.0%*%matrix(Beta3[s,])))))


Merger.1<-sapply(1:nrow(Beta2), function(s)(exp(X.1%*%matrix(Beta2[s,]))/
(exp(X.1%*%matrix(Beta2[s,]))+
exp(X.1%*%matrix(Beta3[s,]))))*(1-
exp(-exp(X.1%*%matrix(Beta2[s,]))-
exp(X.1%*%matrix(Beta3[s,])))))


Marginal.prob.controls[max(grep(colnames(X)[dummies[j]],Marginal.prob.controls[,2])),3:ncol(Marginal.prob.controls)]<-c(mean(colMeans(Merger.1)-colMeans(Merger.0)),
		HPDinterval(as.mcmc(colMeans(Merger.1)-colMeans(Merger.0)), p=0.95))


Marginal.prob.controls[min(grep(colnames(X)[dummies[j]],Marginal.prob.controls[,2])),3:ncol(Marginal.prob.controls)]<-c(mean(colMeans(Dissolution.1)-colMeans(Dissolution.0)),
		HPDinterval(as.mcmc(colMeans(Dissolution.1)-colMeans(Dissolution.0)), p=0.95))



}




for (j in 1:length(continuous)) {

X.0<-X
X.1<-X
X.1[,continuous[j]]<-X.0[,continuous[j]]+sd(X.0[,continuous[j]])



Dissolution.0<-sapply(1:nrow(Beta2), function(s) (exp(X.0%*%matrix(Beta3[s,]))/
(exp(X.0%*%matrix(Beta2[s,]))+
exp(X.0%*%matrix(Beta3[s,]))))*(1-
exp(-exp(X.0%*%matrix(Beta2[s,]))-
exp(X.0%*%matrix(Beta3[s,])))))

Dissolution.1<-sapply(1:nrow(Beta2), function(s) (exp(X.1%*%matrix(Beta3[s,]))/
(exp(X.1%*%matrix(Beta2[s,]))+
exp(X.1%*%matrix(Beta3[s,]))))*(1-
exp(-exp(X.1%*%matrix(Beta2[s,]))-
exp(X.1%*%matrix(Beta3[s,])))))


Merger.0<-sapply(1:nrow(Beta2), function(s)(exp(X.0%*%matrix(Beta2[s,]))/
(exp(X.0%*%matrix(Beta2[s,]))+
exp(X.0%*%matrix(Beta3[s,]))))*(1-
exp(-exp(X.0%*%matrix(Beta2[s,]))-
exp(X.0%*%matrix(Beta3[s,])))))


Merger.1<-sapply(1:nrow(Beta2), function(s)(exp(X.1%*%matrix(Beta2[s,]))/
(exp(X.1%*%matrix(Beta2[s,]))+
exp(X.1%*%matrix(Beta3[s,]))))*(1-
exp(-exp(X.1%*%matrix(Beta2[s,]))-
exp(X.1%*%matrix(Beta3[s,])))))


Marginal.prob.controls[max(grep(colnames(X)[continuous[j]],Marginal.prob.controls[,2])),3:ncol(Marginal.prob.controls)]<-c(mean(colMeans(Merger.1-Merger.0)),
		HPDinterval(as.mcmc(colMeans(Merger.1-Merger.0)), p=0.95))


Marginal.prob.controls[min(grep(colnames(X)[continuous[j]],Marginal.prob.controls[,2])),3:ncol(Marginal.prob.controls)]<-c(mean(colMeans(Dissolution.1-Dissolution.0)),
		HPDinterval(as.mcmc(colMeans(Dissolution.1-Dissolution.0)), p=0.95))



}


Marginal.prob.controls[,3:ncol(Marginal.prob.controls)]<-100*as.numeric(Marginal.prob.controls[,3:ncol(Marginal.prob.controls)])

Marginal.effects<-rbind(Marginal.prob.origin, Marginal.prob.controls)


X.0<-X
X.1<-X

X.0[,51]<-0
X.0[,52]<-0
X.0[,53]<-0
X.1[,51]<-1
X.1[,52]<-1
X.1[,53]<-1




Dissolution.0<-sapply(1:nrow(Beta2), function(s) (exp(X.0%*%matrix(Beta3[s,]))/
(exp(X.0%*%matrix(Beta2[s,]))+
exp(X.0%*%matrix(Beta3[s,]))))*(1-
exp(-exp(X.0%*%matrix(Beta2[s,]))-
exp(X.0%*%matrix(Beta3[s,])))))

Dissolution.1<-sapply(1:nrow(Beta2), function(s) (exp(X.1%*%matrix(Beta3[s,]))/
(exp(X.1%*%matrix(Beta2[s,]))+
exp(X.1%*%matrix(Beta3[s,]))))*(1-
exp(-exp(X.1%*%matrix(Beta2[s,]))-
exp(X.1%*%matrix(Beta3[s,])))))


Merger.0<-sapply(1:nrow(Beta2), function(s)(exp(X.0%*%matrix(Beta2[s,]))/
(exp(X.0%*%matrix(Beta2[s,]))+
exp(X.0%*%matrix(Beta3[s,]))))*(1-
exp(-exp(X.0%*%matrix(Beta2[s,]))-
exp(X.0%*%matrix(Beta3[s,])))))


Merger.1<-sapply(1:nrow(Beta2), function(s)(exp(X.1%*%matrix(Beta2[s,]))/
(exp(X.1%*%matrix(Beta2[s,]))+
exp(X.1%*%matrix(Beta3[s,]))))*(1-
exp(-exp(X.1%*%matrix(Beta2[s,]))-
exp(X.1%*%matrix(Beta3[s,])))))


Marginal.Effects.Cox<-rbind(Marginal.effects, rbind(c("Dissolution", "Interaction.Novelty.Roots", 100*mean(colMeans(Dissolution.1-Dissolution.0)), 100*HPDinterval(as.mcmc(colMeans(Dissolution.1-Dissolution.0)), p=0.95)),
c("Merger","Interaction.Novelty.Roots",100* mean(colMeans(Merger.1-Merger.0)),100*HPDinterval(as.mcmc(colMeans(Merger.1-Merger.0)), p=0.95))))

save(Marginal.Effects.Cox, file="Marginal.Effects.Cox")


lp.Cox<-c()
p.Cox<-c()
l.Cox<-c()

for (i in 1:length(unique(df$case_id))) {


X.id<-matrix(X[df$case_id==unique(df$case_id)[i],], ncol=ncol(X))
duration.id<-sort(duration[df$case_id==unique(df$case_id)[i]])
duration.id<-1:length(duration.id)
Y.id<-matrix(Y[df$case_id==unique(df$case_id)[i],], ncol=3)
Reffect.Party2.id<-Reffect.Party2[, which(unique(df$case_id)==unique(df$case_id)[i])]
Reffect.Party3.id<-Reffect.Party3[, which(unique(df$case_id)==unique(df$case_id)[i])]
Reffect.Country2.id<-Reffect.Country2[, unique(country[df$case_id==unique(df$case_id)[i]])]
Reffect.Country3.id<-Reffect.Country3[, unique(country[df$case_id==unique(df$case_id)[i]])]

for (l in 1:length(duration.id)){

p.1<-sapply(1:nrow(Beta2), function(s) exp(-exp(X.id[duration.id==l,]%*%matrix(Beta2[s,])+Reffect.Party2.id[s]+Reffect.Country2.id[s])-
exp(X.id[duration.id==l,]%*%matrix(Beta3[s,])+Reffect.Party3.id[s]+Reffect.Country3.id[s])))

p.2<-sapply(1:nrow(Beta2), function(s) (exp(X.id[duration.id==l,]%*%matrix(Beta2[s,])+Reffect.Party2.id[s]+Reffect.Country2.id[s])/
(exp(X.id[duration.id==l,]%*%matrix(Beta2[s,])+Reffect.Party2.id[s]+Reffect.Country2.id[s])+
exp(X.id[duration.id==l,]%*%matrix(Beta3[s,])+Reffect.Party3.id[s]+Reffect.Country3.id[s])))*(1-p.1[s]))


p.3<-sapply(1:nrow(Beta2), function(s) (exp(X.id[duration.id==l,]%*%matrix(Beta3[s,])+Reffect.Party3.id[s]+Reffect.Country3.id[s])/
(exp(X.id[duration.id==l,]%*%matrix(Beta2[s,])+Reffect.Party2.id[s]+Reffect.Country2.id[s])+
exp(X.id[duration.id==l,]%*%matrix(Beta3[s,])+Reffect.Party3.id[s]+Reffect.Country3.id[s])))*(1-p.1[s]))


a<-mean(sapply(1:nrow(Beta2), function(s) ((p.1[s]^Y.id[duration.id==l,1])*(p.2[s]^Y.id[duration.id==l,2])*(p.3[s]^Y.id[duration.id==l,3]))))

b<-mean(log(sapply(1:nrow(Beta2), function(s) ((p.1[s]^Y.id[duration.id==l,1])*(p.2[s]^Y.id[duration.id==l,2])*(p.3[s]^Y.id[duration.id==l,3])))))


l.Cox<-c(l.Cox, b)



lp.Cox<-c(lp.Cox, a)
p.Cox<-c(p.Cox, (log(a)-b))



}

}



Table.3.Cox<-c(-2*sum(l.Cox)+2*(2*ncol(X)), -2*sum(l.Cox)+log(nrow(Y))*(2*ncol(X)), 
		    -2*sum(l.Cox)+(log(nrow(Y))+1)*(2*ncol(X)), 
		     -2*(sum(log(lp.Cox))+sum(p.Cox)))




G.M <- survfit(Surv(year_exist, merged) ~ 1, data=df)
G.D <- survfit(Surv(year_exist, dissolved) ~ 1, data=df)


df$T.D<-NA
df$T.M<-NA
df$Delta.D<-NA
df$Delta.M<-NA


for (i in 1:length(unique(df$case_id))) {

if (any(df$merged[df$case_id==unique(df$case_id)[i]]==1)==FALSE) {
df$T.M[df$case_id==unique(df$case_id)[i]]<-max(df$year_exist[df$case_id==unique(df$case_id)[i]])
df$Delta.M[df$case_id==unique(df$case_id)[i]]<-0
} else if (any(df$merged[df$case_id==unique(df$case_id)[i]]==1)==TRUE) {
df$T.M[df$case_id==unique(df$case_id)[i]]<-df$year_exist[df$case_id==unique(df$case_id)[i] & df$merged==1]
df$Delta.M[df$case_id==unique(df$case_id)[i]]<-1
}

if (any(df$dissolved[df$case_id==unique(df$case_id)[i]]==1)==FALSE) {
df$T.D[df$case_id==unique(df$case_id)[i]]<-max(df$year_exist[df$case_id==unique(df$case_id)[i]])
df$Delta.D[df$case_id==unique(df$case_id)[i]]<-0
} else if (any(df$dissolved[df$case_id==unique(df$case_id)[i]]==1)==TRUE) {
df$T.D[df$case_id==unique(df$case_id)[i]]<-df$year_exist[df$case_id==unique(df$case_id)[i] & df$dissolved==1]
df$Delta.D[df$case_id==unique(df$case_id)[i]]<-1
}

}




AUC.D<-c()
AUC.M<-c()
S<-c()
P.M<-c()
P.D<-c()

for (l in 1:length(unique(duration))) {

T.D.id<-df$T.D[duration==l]
T.M.id<-df$T.M[duration==l]
Delta.D.id<-df$Delta.D[duration==l]
Delta.M.id<-df$Delta.M[duration==l]
X.id<-matrix(X[duration==l,], ncol=ncol(X))
Y.id<-matrix(Y[duration==l,],ncol=3)
Reffect.Party2.id<-Reffect.Party2[, sort(unique(party[duration==l]))]
Reffect.Party3.id<-Reffect.Party3[, sort(unique(party[duration==l]))]
Reffect.Country2.id<-Reffect.Country2[, sort(unique(country[duration==l]))]
Reffect.Country3.id<-Reffect.Country3[, sort(unique(country[duration==l]))]


p.1<-sapply(1:nrow(Beta2), function(s) exp(-exp(X.id%*%matrix(Beta2[s,])+Reffect.Party2.id[s]+Reffect.Country2.id[s])-
exp(X.id%*%matrix(Beta3[s,])+Reffect.Party3.id[s]+Reffect.Country3.id[s])))


p.2<-sapply(1:nrow(Beta2), function(s) (exp(X.id%*%matrix(Beta2[s,])+Reffect.Party2.id[s]+Reffect.Country2.id[s])/
(exp(X.id%*%matrix(Beta2[s,])+Reffect.Party2.id[s]+Reffect.Country2.id[s])+
exp(X.id%*%matrix(Beta3[s,])+Reffect.Party3.id[s]+Reffect.Country3.id[s])))*(1-p.1[s]))


p.3<-sapply(1:nrow(Beta2), function(s) (exp(X.id%*%matrix(Beta3[s,])+Reffect.Party3.id[s]+Reffect.Country3.id[s])/
(exp(X.id%*%matrix(Beta2[s,])+Reffect.Party2.id[s]+Reffect.Country2.id[s])+
exp(X.id%*%matrix(Beta3[s,])+Reffect.Party3.id[s]+Reffect.Country3.id[s])))*(1-p.1[s]))



W.D<-sapply(1:nrow(X.id), function(i) ifelse(Delta.D.id[i]==0, as.numeric(T.D.id[i]>l)/summary(G.D)[[6]][which(summary(G.D)[[2]]==l)],
	as.numeric(T.D.id[i]<=l)/summary(G.D)[[6]][which(summary(G.D)[[2]]==T.D.id[i])]))

W.M<-sapply(1:nrow(X.id), function(i) ifelse(Delta.M.id[i]==0, as.numeric(T.M.id[i]>l)/summary(G.M)[[6]][which(summary(G.M)[[2]]==l)],
	as.numeric(T.M.id[i]<=l)/summary(G.M)[[6]][which(summary(G.M)[[2]]==T.M.id[i])]))


AUC.D.numerator<-sapply(1:nrow(X.id), function(i) sapply((1:nrow(X.id))[-i], function(j) (as.numeric(mean(p.3[i,])>mean(p.3[j,]))+0.5*(as.numeric(mean(p.3[i,])==mean(p.3[j,]))))*
W.D[i]*W.D[j]* Y.id[i,3]*(1-Y.id[j,3])))

AUC.D.denominator<-sapply(1:nrow(X.id), function(i) sapply((1:nrow(X.id))[-i], function(j) W.D[i]*W.D[j]*
Y.id[i,3]*(1-Y.id[j,3])))

AUC.D<-c(AUC.D, sum(rowSums(t(AUC.D.numerator)))/sum(rowSums(t(AUC.D.denominator))))

AUC.M.numerator<-sapply(1:nrow(X.id), function(i) sapply((1:nrow(X.id))[-i], function(j) (as.numeric(mean(p.2[i,])>mean(p.2[j,]))+0.5*(as.numeric(mean(p.2[i,])==mean(p.2[j,]))))*
W.M[i]*W.M[j]*Y.id[i,2]*(1-Y.id[j,2])))


AUC.M.denominator<-sapply(1:nrow(X.id), function(i) sapply((1:nrow(X.id))[-i], function(j) W.M[i]*W.M[j]*
 Y.id[i,2]*(1-Y.id[j,2])))

AUC.M<-c(AUC.M, sum(rowSums(t(AUC.M.numerator)))/sum(rowSums(t(AUC.M.denominator))))


S<-c(S, 1-mean(p.2)-mean(p.3))

P.M<-c(P.M, mean(p.2))
P.D<-c(P.D, mean(p.3))



}



AUC.M<-AUC.M[!is.na(AUC.M)]
P.M<-P.M[!is.na(AUC.M)]
S.M<-S[!is.na(AUC.M)]

AUC.D<-AUC.D[!is.na(AUC.D)]
P.D<-P.D[!is.na(AUC.D)]
S.D<-S[!is.na(AUC.D)]


w.M.numerator<-sapply(1:length(AUC.M), function(t) P.M[t]*prod(S.M[1:(t-1)])*S.M[t] )
w.D.numerator<-sapply(1:length(AUC.D), function(t) P.D[t]*prod(S.D[1:(t-1)])*S.D[t] )


C.M<-sum(sapply(1:length(AUC.M), function(t) AUC.M[t]*w.M.numerator[t]/sum(w.M.numerator)),na.rm=T)
C.D<-sum(sapply(1:length(AUC.D), function(t) AUC.D[t]*w.D.numerator[t]/sum(w.D.numerator)),na.rm=T)





B.D<-c()
B.M<-c()
S<-c()
P.M<-c()
P.D<-c()

for (l in 1:length(unique(duration))) {

T.D.id<-df$T.D[duration==l]
T.M.id<-df$T.M[duration==l]
Delta.D.id<-df$Delta.D[duration==l]
Delta.M.id<-df$Delta.M[duration==l]
X.id<-matrix(X[duration==l,], ncol=ncol(X))
Y.id<-matrix(Y[duration==l,],ncol=3)
Reffect.Party2.id<-Reffect.Party2[, sort(unique(party[duration==l]))]
Reffect.Party3.id<-Reffect.Party3[, sort(unique(party[duration==l]))]
Reffect.Country2.id<-Reffect.Country2[, sort(unique(country[duration==l]))]
Reffect.Country3.id<-Reffect.Country3[, sort(unique(country[duration==l]))]


p.1<-sapply(1:nrow(Beta2), function(s) exp(-exp(X.id%*%matrix(Beta2[s,])+Reffect.Party2.id[s]+Reffect.Country2.id[s])-
exp(X.id%*%matrix(Beta3[s,])+Reffect.Party3.id[s]+Reffect.Country3.id[s])))


p.2<-sapply(1:nrow(Beta2), function(s) (exp(X.id%*%matrix(Beta2[s,])+Reffect.Party2.id[s]+Reffect.Country2.id[s])/
(exp(X.id%*%matrix(Beta2[s,])+Reffect.Party2.id[s]+Reffect.Country2.id[s])+
exp(X.id%*%matrix(Beta3[s,])+Reffect.Party3.id[s]+Reffect.Country3.id[s])))*(1-p.1[s]))


p.3<-sapply(1:nrow(Beta2), function(s) (exp(X.id%*%matrix(Beta3[s,])+Reffect.Party3.id[s]+Reffect.Country3.id[s])/
(exp(X.id%*%matrix(Beta2[s,])+Reffect.Party2.id[s]+Reffect.Country2.id[s])+
exp(X.id%*%matrix(Beta3[s,])+Reffect.Party3.id[s]+Reffect.Country3.id[s])))*(1-p.1[s]))


W.D<-sapply(1:nrow(X.id), function(i) ifelse(Delta.D.id[i]==0, as.numeric(T.D.id[i]>l)/summary(G.D)[[6]][which(summary(G.D)[[2]]==l)],
	as.numeric(T.D.id[i]<=l)/summary(G.D)[[6]][which(summary(G.D)[[2]]==T.D.id[i])]))

W.M<-sapply(1:nrow(X.id), function(i) ifelse(Delta.M.id[i]==0, as.numeric(T.M.id[i]>l)/summary(G.M)[[6]][which(summary(G.M)[[2]]==l)],
	as.numeric(T.M.id[i]<=l)/summary(G.M)[[6]][which(summary(G.M)[[2]]==T.M.id[i])]))



BS.M<-sapply(1:nrow(X.id), function(i) W.M[i]* ((Y.id[i,2]-mean(p.2[i,]))^2))
BS.D<-sapply(1:nrow(X.id), function(i) W.D[i]*((Y.id[i,3]-mean(p.3[i,]))^2))



BS.M<- (1/nrow(X.id)*mean(T.M.id>=l))*sum(BS.M)
BS.D<- (1/nrow(X.id)*mean(T.D.id>=l))*sum(BS.D)


B.M<-c(B.M, BS.M)
B.D<-c(B.D, BS.D)

S<-c(S, 1-mean(p.2)-mean(p.3))

P.M<-c(P.M, mean(p.2))
P.D<-c(P.D, mean(p.3))



}

B.M<-B.M[!is.na(B.M)]
P.M<-P.M[!is.na(B.M)]
S.M<-S[!is.na(B.M)]

B.D<-B.D[!is.na(B.D)]
P.D<-P.D[!is.na(B.D)]
S.D<-S[!is.na(B.D)]


w.M<-sapply(1:length(B.M), function(t) P.M[t]*prod(S.M[1:(t-1)]))
w.D<-sapply(1:length(B.D), function(t) P.D[t]*prod(S.D[1:(t-1)]))



Table.3.Cox<-c(Table.3.Cox, 100*(sum(B.M*w.M)+sum(B.D*w.D)),  100*(C.M*sum(Y[,2])+C.D*sum(Y[,3]))/sum(colSums(Y)[-1]))
save(Table.3.Cox, file="Table.3.Cox")


rm(Table.3.Cox, out, Marginal.Effects.Cox)

# # Now we process the Lognormal proportional hazards model 

load("Estimates_Lognormal") 

# Convergence checks - Log-normal model
BETA2<-mcmc.list(as.mcmc(t(out$beta2[,,1])), as.mcmc(t(out$beta2[,,2])),
	 as.mcmc(t(out$beta2[,,3])))


BETA3<-mcmc.list(as.mcmc(t(out$beta3[,,1])), as.mcmc(t(out$beta3[,,2])),
	 as.mcmc(t(out$beta3[,,3])))

SIGMA<-mcmc.list(as.mcmc(t(out$sigma[,,1])), as.mcmc(t(out$sigma[,,2])),
	 as.mcmc(t(out$sigma[,,3])))

print(paste("Convergence, Parameter Estimates of the Log-normal model:", gelman.diag(BETA2)[[2]]<1.2 & gelman.diag(BETA3)[[2]]<1.2 & 
		gelman.diag(SIGMA)[[2]]<1.2, sep=" "))



# Pooled convergent draws - to be used for inference

Beta2<-rbind(t(out$beta2[, pool:20000, 1]),
		t(out$beta2[,pool:20000 , 2]),
		t(out$beta2[,pool:20000 , 3]))


Beta3<-rbind(t(out$beta3[,pool:20000 , 1]),
		t(out$beta3[, pool:20000, 2]),
		t(out$beta3[, pool:20000, 3]))


Reffect.Party2<-rbind(t(out$reffect_party[1:204, pool:20000, 1]),
		t(out$reffect_party[1:204,pool:20000 , 2]),
		t(out$reffect_party[1:204,pool:20000 , 3]))
Reffect.Party3<-rbind(t(out$reffect_party[205:408,pool:20000 , 1]),
		t(out$reffect_party[205:408,pool:20000 , 2]),
		t(out$reffect_party[205:408,pool:20000 , 3]))
Reffect.Country2<-rbind(t(out$reffect_country[1:22,pool:20000 , 1]),
		t(out$reffect_country[1:22,pool:20000 , 2]),
		t(out$reffect_country[1:22,pool:20000 , 3]))
Reffect.Country3<-rbind(t(out$reffect_country[23:44, pool:20000, 1]),
		t(out$reffect_country[23:44,pool:20000 , 2]),
		t(out$reffect_country[23:44,pool:20000 , 3]))


Sigma<-rbind(t(out$sigma[,pool:20000 , 1]),
		t(out$sigma[,pool:20000 , 2]),
		t(out$sigma[,pool:20000 , 3]))

## Load Data 
df<-read.csv("Dataset.csv")
df<-subset(df, select=c("votes.centered",
			"country",
			"case_id",
			"merged", "dissolved",
			"year_exist",
			"logseats.centered",
		 "government",
			 "EU_parl",
		 "reg_gov",
		 "finance_party_qualify",
		 "efNPP",
	 "elec_coalition", 
	 "indPOL",
	"election_year_dummy",
	"distinctiveness",
	 "insider", 
	 "organisation", 
	 "newfamfirst",
	"OriginNew",
	"post.election.year",	
	"pre.election.year", 
	"seat.product"
	 ))


df<-na.exclude(df)

country<-as.numeric(factor(df$country), levels=unique(df$country))
party<-as.numeric(factor(df$case_id), levels=unique(df$case_id))
num.country<-length(unique(country))
num.party<-length(unique(party))
duration<-as.numeric(factor(df$year_exist), levels=unique(df$year_exist))
num.duration<-length(unique(duration))


# 1= survive, 2=merge, 3=dissolve
Y<-cbind(df$merged, df$dissolved)
Y<-cbind(ifelse(rowSums(Y)==0,1,0),Y)
y<-c(sapply(1:nrow(Y), function(i) which(Y[i,]==1)))
n<-length(y)        

Matrix.Country<-matrix(0,n,num.country)
for (i in 1:num.country) Matrix.Country[country==i,i]<-1

Matrix.Party<-matrix(0,n,num.party)
for (i in 1:num.party) Matrix.Party[party==i,i]<-1



Matrix.Outcome<-matrix(0, n, 3)
for (i in 1:3) Matrix.Outcome[y==i,i]<-1


df$OriginNew.newfamfirst<-df$OriginNew*df$newfamfirst

X<-cbind(1, 
	 df$insider, 
	 df$organisation, 
       df$newfamfirst,
	 df$OriginNew,
	df$OriginNew.newfamfirst, 
	df$votes.centered, 
	 df$logseats.centered, 
	 df$government,
	 df$EU_parl,
	 df$reg_gov,  	 df$finance_party_qualify,
	 (df$efNPP-mean(df$efNPP))/(2*sd(df$efNPP)),
	 df$elec_coalition, 
	 (df$indPOL-mean(df$indPOL))/(2*sd(df$indPOL)),
	 (df$seat.product-mean(df$seat.product))/(2*sd(df$seat.product)),
	df$election_year_dummy,
	df$post.election.year,	
	df$pre.election.year, 
	df$distinctiveness )



t<-df$year_exist


colnames(X)<-c("Intercept", "Insider.Status", "Societal.Rootednes", "Ideological.Novelty",
		"Roots.in.Parties", "Interaction.Novelty.Roots", "Electoral.Performance", "Seat.Share", "National.Government", "EP.Access", "Regional.Government",
		"State.Funding", "System.Fragmentation", "Electoral.Coalition", "Anti-establishment.Vote",
		"Seat.Product", "Election.Year", "Post", "Pre",
		"Party.Distinctiveness")




# Computation of Marginal Effects of the Log-normal Model
library(VGAM)


Marginal.prob.origin<-matrix(NA, nrow=4*2, ncol=5)
colnames(Marginal.prob.origin)<-c("Dep.Variable", "Covariate", "Predictive Comparison",  "95% CI - Lower", "95% CI - Higher")
Marginal.prob.origin[,1]<-c(rep("Dissolution", 4), 
		rep("Merger", 4))
Marginal.prob.origin[,2]<-rep(c(colnames(X)[2:5]),2)


Marginal.prob.controls<-matrix(NA, nrow=(ncol(X[,-c(1:6)])*2), ncol=5)
colnames(Marginal.prob.controls)<-c("Dep.Variable", "Covariate", "Predictive Comparison",  "95% CI - Lower", "95% CI - Higher")
Marginal.prob.controls[,1]<-c(rep("Dissolution", ncol(X[,-c(1:6)])), 
		rep("Merger", ncol(X[,-c(1:6)])))
Marginal.prob.controls[,2]<-rep(colnames(X)[-c(1:6)],2)



dummies<-c(2:5)

 
for (j in 1:length(dummies)) {


if (j<=3) {
X.0<-X
X.0[,dummies[j]]<-0
X.1<-X
X.1[,dummies[j]]<-1
} else if (j==4) {
X.0<-X
X.0[,dummies[j]]<-1
X.1<-X
X.1[,dummies[j]]<-0
}



Merger.0<-sapply(1:nrow(Beta2), function(s) mean(pbinorm((log(t)- X.0%*%t(Beta2)[,s])/Sigma[s,1], (X.0%*%t(Beta2)[,s]-X.0%*%t(Beta3)[,s])/sqrt((Sigma[s,1]+Sigma[s,2])^2), mean1 = 0, mean2 = 0, var1 = 1, var2 = 1, cov12 = min(1,1/sqrt((1+(Sigma[s,2]^2))/Sigma[s,1]^2)))))
Merger.1<-sapply(1:nrow(Beta2), function(s) mean(pbinorm((log(t)- X.1%*%t(Beta2)[,s])/Sigma[s,1], (X.1%*%t(Beta2)[,s]-X.1%*%t(Beta3)[,s])/sqrt((Sigma[s,1]+Sigma[s,2])^2), mean1 = 0, mean2 = 0, var1 = 1, var2 = 1, cov12 = min(1,1/sqrt((1+(Sigma[s,2]^2))/Sigma[s,1]^2)))))

Cens.0<-sapply(1:nrow(Beta2), function(s) mean((1-pnorm((log(t)- X.0%*%t(Beta2)[,s])/Sigma[s,1]))*(1-pnorm((log(t)- X.0%*%t(Beta3)[,s])/Sigma[s,2]))))
Cens.1<-sapply(1:nrow(Beta2), function(s) mean((1-pnorm((log(t)- X.1%*%t(Beta2)[,s])/Sigma[s,1]))*(1-pnorm((log(t)- X.1%*%t(Beta3)[,s])/Sigma[s,2]))))


Dissolution.0<-1-Merger.0-Cens.0
Dissolution.1<-1-Merger.1-Cens.1



Marginal.prob.origin[max(grep(colnames(X)[dummies[j]],Marginal.prob.origin[,2])),3:ncol(Marginal.prob.origin)]<-c(mean((Merger.1-Merger.0)),
		HPDinterval(as.mcmc((Merger.1-Merger.0)), p=0.95))


Marginal.prob.origin[min(grep(colnames(X)[dummies[j]],Marginal.prob.origin[,2])),3:ncol(Marginal.prob.origin)]<-c(mean((Dissolution.1-Dissolution.0)),
		HPDinterval(as.mcmc((Dissolution.1-Dissolution.0)), p=0.95))

}



rm(dummies)
dummies<-c(9:12, 14, 17:20)
continuous<-c(7,8,13, 15, 16)  





for (j in 1:length(dummies)) {


X.0<-X
X.0[,dummies[j]]<-0
X.1<-X
X.1[,dummies[j]]<-1


Merger.0<-sapply(1:nrow(Beta2), function(s) mean(pbinorm((log(t)- X.0%*%t(Beta2)[,s])/Sigma[s,1], (X.0%*%t(Beta2)[,s]-X.0%*%t(Beta3)[,s])/sqrt((Sigma[s,1]+Sigma[s,2])^2), mean1 = 0, mean2 = 0, var1 = 1, var2 = 1, cov12 = min(1,1/sqrt((1+(Sigma[s,2]^2))/Sigma[s,1]^2)))))
Merger.1<-sapply(1:nrow(Beta2), function(s) mean(pbinorm((log(t)- X.1%*%t(Beta2)[,s])/Sigma[s,1], (X.1%*%t(Beta2)[,s]-X.1%*%t(Beta3)[,s])/sqrt((Sigma[s,1]+Sigma[s,2])^2), mean1 = 0, mean2 = 0, var1 = 1, var2 = 1, cov12 = min(1,1/sqrt((1+(Sigma[s,2]^2))/Sigma[s,1]^2)))))

Cens.0<-sapply(1:nrow(Beta2), function(s) mean((1-pnorm((log(t)- X.0%*%t(Beta2)[,s])/Sigma[s,1]))*(1-pnorm((log(t)- X.0%*%t(Beta3)[,s])/Sigma[s,2]))))
Cens.1<-sapply(1:nrow(Beta2), function(s) mean((1-pnorm((log(t)- X.1%*%t(Beta2)[,s])/Sigma[s,1]))*(1-pnorm((log(t)- X.1%*%t(Beta3)[,s])/Sigma[s,2]))))


Dissolution.0<-1-Merger.0-Cens.0
Dissolution.1<-1-Merger.1-Cens.1




Marginal.prob.controls[max(grep(colnames(X)[dummies[j]],Marginal.prob.controls[,2])),3:ncol(Marginal.prob.controls)]<-c(mean(Merger.1-Merger.0),
		HPDinterval(as.mcmc(Merger.1-Merger.0), p=0.95))


Marginal.prob.controls[min(grep(colnames(X)[dummies[j]],Marginal.prob.controls[,2])),3:ncol(Marginal.prob.controls)]<-c(mean(Dissolution.1-Dissolution.0),
		HPDinterval(as.mcmc(Dissolution.1-Dissolution.0), p=0.95))



}





for (j in 1:length(continuous)) {

X.0<-X
X.1<-X
X.1[,continuous[j]]<-X.0[,continuous[j]]+sd(X.0[,continuous[j]])




Merger.0<-sapply(1:nrow(Beta2), function(s) mean(pbinorm((log(t)- X.0%*%t(Beta2)[,s])/Sigma[s,1], (X.0%*%t(Beta2)[,s]-X.0%*%t(Beta3)[,s])/sqrt((Sigma[s,1]+Sigma[s,2])^2), mean1 = 0, mean2 = 0, var1 = 1, var2 = 1, cov12 = min(1,1/sqrt((1+(Sigma[s,2]^2))/Sigma[s,1]^2)))))
Merger.1<-sapply(1:nrow(Beta2), function(s) mean(pbinorm((log(t)- X.1%*%t(Beta2)[,s])/Sigma[s,1], (X.1%*%t(Beta2)[,s]-X.1%*%t(Beta3)[,s])/sqrt((Sigma[s,1]+Sigma[s,2])^2), mean1 = 0, mean2 = 0, var1 = 1, var2 = 1, cov12 = min(1,1/sqrt((1+(Sigma[s,2]^2))/Sigma[s,1]^2)))))

Cens.0<-sapply(1:nrow(Beta2), function(s) mean((1-pnorm((log(t)- X.0%*%t(Beta2)[,s])/Sigma[s,1]))*(1-pnorm((log(t)- X.0%*%t(Beta3)[,s])/Sigma[s,2]))))
Cens.1<-sapply(1:nrow(Beta2), function(s) mean((1-pnorm((log(t)- X.1%*%t(Beta2)[,s])/Sigma[s,1]))*(1-pnorm((log(t)- X.1%*%t(Beta3)[,s])/Sigma[s,2]))))


Dissolution.0<-1-Merger.0-Cens.0
Dissolution.1<-1-Merger.1-Cens.1




Marginal.prob.controls[max(grep(colnames(X)[continuous[j]],Marginal.prob.controls[,2])),3:ncol(Marginal.prob.controls)]<-c(mean(Merger.1-Merger.0),
		HPDinterval(as.mcmc(Merger.1-Merger.0), p=0.95))


Marginal.prob.controls[min(grep(colnames(X)[continuous[j]],Marginal.prob.controls[,2])),3:ncol(Marginal.prob.controls)]<-c(mean(Dissolution.1-Dissolution.0),
		HPDinterval(as.mcmc(Dissolution.1-Dissolution.0), p=0.95))



}




Marginal.prob.origin[,3:ncol(Marginal.prob.origin)]<-100*as.numeric(Marginal.prob.origin[,3:ncol(Marginal.prob.origin)])
Marginal.prob.controls[,3:ncol(Marginal.prob.controls)]<-100*as.numeric(Marginal.prob.controls[,3:ncol(Marginal.prob.controls)])
Marginal.effects<-rbind(Marginal.prob.origin, Marginal.prob.controls)


X.0<-X
X.1<-X

X.0[,4]<-0
X.0[,5]<-0
X.0[,6]<-0
X.1[,4]<-1
X.1[,5]<-1
X.1[,6]<-1



Merger.0<-sapply(1:nrow(Beta2), function(s) mean(pbinorm((log(t)- X.0%*%t(Beta2)[,s])/Sigma[s,1], (X.0%*%t(Beta2)[,s]-X.0%*%t(Beta3)[,s])/sqrt((Sigma[s,1]+Sigma[s,2])^2), mean1 = 0, mean2 = 0, var1 = 1, var2 = 1, cov12 = min(1,1/sqrt((1+(Sigma[s,2]^2))/Sigma[s,1]^2)))))
Merger.1<-sapply(1:nrow(Beta2), function(s) mean(pbinorm((log(t)- X.1%*%t(Beta2)[,s])/Sigma[s,1], (X.1%*%t(Beta2)[,s]-X.1%*%t(Beta3)[,s])/sqrt((Sigma[s,1]+Sigma[s,2])^2), mean1 = 0, mean2 = 0, var1 = 1, var2 = 1, cov12 = min(1,1/sqrt((1+(Sigma[s,2]^2))/Sigma[s,1]^2)))))

Cens.0<-sapply(1:nrow(Beta2), function(s) mean((1-pnorm((log(t)- X.0%*%t(Beta2)[,s])/Sigma[s,1]))*(1-pnorm((log(t)- X.0%*%t(Beta3)[,s])/Sigma[s,2]))))
Cens.1<-sapply(1:nrow(Beta2), function(s) mean((1-pnorm((log(t)- X.1%*%t(Beta2)[,s])/Sigma[s,1]))*(1-pnorm((log(t)- X.1%*%t(Beta3)[,s])/Sigma[s,2]))))


Dissolution.0<-1-Merger.0-Cens.0
Dissolution.1<-1-Merger.1-Cens.1


Marginal.Effects.Lognormal<-rbind(Marginal.effects, rbind(c("Dissolution", "Interaction.Novelty.Roots", 100*mean((Dissolution.1-Dissolution.0)), 100*HPDinterval(as.mcmc((Dissolution.1-Dissolution.0)), p=0.95)),
c("Merger","Interaction.Novelty.Roots",100* mean((Merger.1-Merger.0)),100*HPDinterval(as.mcmc((Merger.1-Merger.0)), p=0.95))))

save(Marginal.Effects.Lognormal, file="Marginal.Effects.Lognormal")

# Goodness of fit measures - Log-normal Model

l.Lognormal<-c()
lp.Lognormal<-c()
p.Lognormal<-c()


for (i in 1:length(unique(df$case_id))) {

X.id<-matrix(X[df$case_id==unique(df$case_id)[i],], ncol=ncol(X))
duration.id<-sort(duration[df$case_id==unique(df$case_id)[i]])
duration.id<-1:length(duration.id)
Y.id<-matrix(Y[df$case_id==unique(df$case_id)[i],],ncol=3)
Reffect.Party2.id<-Reffect.Party2[, which(unique(df$case_id)==unique(df$case_id)[i])]
Reffect.Party3.id<-Reffect.Party3[, which(unique(df$case_id)==unique(df$case_id)[i])]
Reffect.Country2.id<-Reffect.Country2[, unique(country[df$case_id==unique(df$case_id)[i]])]
Reffect.Country3.id<-Reffect.Country3[, unique(country[df$case_id==unique(df$case_id)[i]])]
t.id<-df$year_exist[df$case_id==unique(df$case_id)[i]]


for (l in 1:length(duration.id)){

p.1<-sapply(1:nrow(Beta2), function(s) log((1-pnorm((log(t.id[duration.id==l])-X.id[duration.id==l,]%*%matrix(Beta2[s,])-Reffect.Party2.id[s]-Reffect.Country2.id[s])/Sigma[s,1])))+
log((1-pnorm((log(t.id[duration.id==l])-X.id[duration.id==l,]%*%matrix(Beta3[s,])-Reffect.Party3.id[s]-Reffect.Country3.id[s])/Sigma[s,2]))))


p.2<-sapply(1:nrow(Beta2), function(s) -0.5*(((log(t.id[duration.id==l])-X.id[duration.id==l,]%*%matrix(Beta2[s,])-Reffect.Party2.id[s]-Reffect.Country2.id[s])/Sigma[s,1])^2)+
log((1-pnorm((log(t.id[duration.id==l])-X.id[duration.id==l,]%*%matrix(Beta3[s,])-Reffect.Party3.id[s]-Reffect.Country3.id[s])/Sigma[s,2]))))


p.3<-sapply(1:nrow(Beta2), function(s) -0.5*(((log(t.id[duration.id==l])-X.id[duration.id==l,]%*%matrix(Beta3[s,])-Reffect.Party3.id[s]-Reffect.Country3.id[s])/Sigma[s,2])^2)+
log((1-pnorm((log(t.id[duration.id==l])-X.id[duration.id==l,]%*%matrix(Beta2[s,])-Reffect.Party2.id[s]-Reffect.Country2.id[s])/Sigma[s,1]))))




a<-mean(sapply(1:nrow(Beta2), function(s) exp(-(Y.id[duration.id==l,2]*log(sqrt(2*pi)*Sigma[s,1]))-(Y.id[duration.id==l,3]*log(sqrt(2*pi)*Sigma[s,2]))+(p.1[s]*Y.id[duration.id==l,1])+(p.2[s]*Y.id[duration.id==l,2])+(p.3[s]*Y.id[duration.id==l,3]))))

b<-mean(sapply(1:nrow(Beta2), function(s) -(Y.id[duration.id==l,2]*log(sqrt(2*pi)*Sigma[s,1]))-(Y.id[duration.id==l,3]*log(sqrt(2*pi)*Sigma[s,2]))+(p.1[s]*Y.id[duration.id==l,1])+(p.2[s]*Y.id[duration.id==l,2])+(p.3[s]*Y.id[duration.id==l,3])))


l.Lognormal<-c(l.Lognormal, b)

lp.Lognormal<-c(lp.Lognormal, a)
p.Lognormal<-c(p.Lognormal, log(a)-b)




}
}


Table.3.Lognormal<-c(-2*sum(l.Lognormal)+2*(2*ncol(X)), -2*sum(l.Lognormal)+log(nrow(Y))*(2*ncol(X)), 
		    -2*sum(l.Lognormal)+(log(nrow(Y))+1)*(2*ncol(X)), 
		     -2*(sum(log(lp.Lognormal))+2*sum(p.Lognormal)))





G.M <- survfit(Surv(year_exist, merged) ~ 1, data=df)
G.D <- survfit(Surv(year_exist, dissolved) ~ 1, data=df)


df$T.D<-NA
df$T.M<-NA
df$Delta.D<-NA
df$Delta.M<-NA


for (i in 1:length(unique(df$case_id))) {

if (any(df$merged[df$case_id==unique(df$case_id)[i]]==1)==FALSE) {
df$T.M[df$case_id==unique(df$case_id)[i]]<-max(df$year_exist[df$case_id==unique(df$case_id)[i]])
df$Delta.M[df$case_id==unique(df$case_id)[i]]<-0
} else if (any(df$merged[df$case_id==unique(df$case_id)[i]]==1)==TRUE) {
df$T.M[df$case_id==unique(df$case_id)[i]]<-df$year_exist[df$case_id==unique(df$case_id)[i] & df$merged==1]
df$Delta.M[df$case_id==unique(df$case_id)[i]]<-1
}

if (any(df$dissolved[df$case_id==unique(df$case_id)[i]]==1)==FALSE) {
df$T.D[df$case_id==unique(df$case_id)[i]]<-max(df$year_exist[df$case_id==unique(df$case_id)[i]])
df$Delta.D[df$case_id==unique(df$case_id)[i]]<-0
} else if (any(df$dissolved[df$case_id==unique(df$case_id)[i]]==1)==TRUE) {
df$T.D[df$case_id==unique(df$case_id)[i]]<-df$year_exist[df$case_id==unique(df$case_id)[i] & df$dissolved==1]
df$Delta.D[df$case_id==unique(df$case_id)[i]]<-1
}

}



AUC.D<-c()
AUC.M<-c()
S<-c()
P.M<-c()
P.D<-c()

for (l in 1:length(unique(duration))) {

T.D.id<-df$T.D[duration==l]
T.M.id<-df$T.M[duration==l]
Delta.D.id<-df$Delta.D[duration==l]
Delta.M.id<-df$Delta.M[duration==l]
X.id<-matrix(X[duration==l,], ncol=ncol(X))
Y.id<-matrix(Y[duration==l,],ncol=3)
Reffect.Party2.id<-Reffect.Party2[, sort(unique(party[duration==l]))]
Reffect.Party3.id<-Reffect.Party3[, sort(unique(party[duration==l]))]
Reffect.Country2.id<-Reffect.Country2[, sort(unique(country[duration==l]))]
Reffect.Country3.id<-Reffect.Country3[, sort(unique(country[duration==l]))]



p.2<-sapply(1:nrow(Beta2), function(s) -0.5*(((log(l)-X.id%*%matrix(Beta2[s,])-Reffect.Party2.id[s]-Reffect.Country2.id[s])/Sigma[s,1])^2)+
log((1-pnorm((log(l)-X.id%*%matrix(Beta3[s,])-Reffect.Party3.id[s]-Reffect.Country3.id[s])/Sigma[s,2]))))


p.3<-sapply(1:nrow(Beta2), function(s) -0.5*(((log(l)-X.id%*%matrix(Beta3[s,])-Reffect.Party3.id[s]-Reffect.Country3.id[s])/Sigma[s,2])^2)+
log((1-pnorm((log(l)-X.id%*%matrix(Beta2[s,])-Reffect.Party2.id[s]-Reffect.Country2.id[s])/Sigma[s,1]))))



p.2<-exp(p.2)
p.3<-exp(p.3)


W.D<-sapply(1:nrow(X.id), function(i) ifelse(Delta.D.id[i]==0, as.numeric(T.D.id[i]>l)/summary(G.D)[[6]][which(summary(G.D)[[2]]==l)],
	as.numeric(T.D.id[i]<=l)/summary(G.D)[[6]][which(summary(G.D)[[2]]==T.D.id[i])]))

W.M<-sapply(1:nrow(X.id), function(i) ifelse(Delta.M.id[i]==0, as.numeric(T.M.id[i]>l)/summary(G.M)[[6]][which(summary(G.M)[[2]]==l)],
	as.numeric(T.M.id[i]<=l)/summary(G.M)[[6]][which(summary(G.M)[[2]]==T.M.id[i])]))


AUC.D.numerator<-sapply(1:nrow(X.id), function(i) sapply((1:nrow(X.id))[-i], function(j) (as.numeric(mean(p.3[i,])>mean(p.3[j,]))+0.5*(as.numeric(mean(p.3[i,])==mean(p.3[j,]))))*
W.D[i]*W.D[j]* Y.id[i,3]*(1-Y.id[j,3])))

AUC.D.denominator<-sapply(1:nrow(X.id), function(i) sapply((1:nrow(X.id))[-i], function(j) W.D[i]*W.D[j]*
Y.id[i,3]*(1-Y.id[j,3])))

AUC.D<-c(AUC.D, sum(rowSums(t(AUC.D.numerator)))/sum(rowSums(t(AUC.D.denominator))))

AUC.M.numerator<-sapply(1:nrow(X.id), function(i) sapply((1:nrow(X.id))[-i], function(j) (as.numeric(mean(p.2[i,])>mean(p.2[j,]))+0.5*(as.numeric(mean(p.2[i,])==mean(p.2[j,]))))*
W.M[i]*W.M[j]*Y.id[i,2]*(1-Y.id[j,2])))


AUC.M.denominator<-sapply(1:nrow(X.id), function(i) sapply((1:nrow(X.id))[-i], function(j) W.M[i]*W.M[j]*
 Y.id[i,2]*(1-Y.id[j,2])))

AUC.M<-c(AUC.M, sum(rowSums(t(AUC.M.numerator)))/sum(rowSums(t(AUC.M.denominator))))



S<-c(S, 1-mean(p.2)-mean(p.3))

P.M<-c(P.M, mean(p.2))
P.D<-c(P.D, mean(p.3))



}


AUC.M<-AUC.M[!is.na(AUC.M)]
P.M<-P.M[!is.na(AUC.M)]
S.M<-S[!is.na(AUC.M)]

AUC.D<-AUC.D[!is.na(AUC.D)]
P.D<-P.D[!is.na(AUC.D)]
S.D<-S[!is.na(AUC.D)]


w.M.numerator<-sapply(1:length(AUC.M), function(t) P.M[t]*prod(S.M[1:(t-1)])*S.M[t] )
w.D.numerator<-sapply(1:length(AUC.D), function(t) P.D[t]*prod(S.D[1:(t-1)])*S.D[t] )


C.M<-sum(sapply(1:length(AUC.M), function(t) AUC.M[t]*w.M.numerator[t]/sum(w.M.numerator)),na.rm=T)
C.D<-sum(sapply(1:length(AUC.D), function(t) AUC.D[t]*w.D.numerator[t]/sum(w.D.numerator)),na.rm=T)


B.D<-c()
B.M<-c()
S<-c()
P.M<-c()
P.D<-c()

for (l in 1:length(unique(duration))) {

T.D.id<-df$T.D[duration==l]
T.M.id<-df$T.M[duration==l]
Delta.D.id<-df$Delta.D[duration==l]
Delta.M.id<-df$Delta.M[duration==l]
X.id<-matrix(X[duration==l,], ncol=ncol(X))
Y.id<-matrix(Y[duration==l,],ncol=3)
Reffect.Party2.id<-Reffect.Party2[, sort(unique(party[duration==l]))]
Reffect.Party3.id<-Reffect.Party3[, sort(unique(party[duration==l]))]
Reffect.Country2.id<-Reffect.Country2[, sort(unique(country[duration==l]))]
Reffect.Country3.id<-Reffect.Country3[, sort(unique(country[duration==l]))]



p.2<-sapply(1:nrow(Beta2), function(s) -0.5*(((log(l)-X.id%*%matrix(Beta2[s,])-Reffect.Party2.id[s]-Reffect.Country2.id[s])/Sigma[s,1])^2)+
log((1-pnorm((log(l)-X.id%*%matrix(Beta3[s,])-Reffect.Party3.id[s]-Reffect.Country3.id[s])/Sigma[s,2]))))


p.3<-sapply(1:nrow(Beta2), function(s) -0.5*(((log(l)-X.id%*%matrix(Beta3[s,])-Reffect.Party3.id[s]-Reffect.Country3.id[s])/Sigma[s,2])^2)+
log((1-pnorm((log(l)-X.id%*%matrix(Beta2[s,])-Reffect.Party2.id[s]-Reffect.Country2.id[s])/Sigma[s,1]))))



p.2<-exp(p.2)
p.3<-exp(p.3)



W.D<-sapply(1:nrow(X.id), function(i) ifelse(Delta.D.id[i]==0, as.numeric(T.D.id[i]>l)/summary(G.D)[[6]][which(summary(G.D)[[2]]==l)],
	as.numeric(T.D.id[i]<=l)/summary(G.D)[[6]][which(summary(G.D)[[2]]==T.D.id[i])]))

W.M<-sapply(1:nrow(X.id), function(i) ifelse(Delta.M.id[i]==0, as.numeric(T.M.id[i]>l)/summary(G.M)[[6]][which(summary(G.M)[[2]]==l)],
	as.numeric(T.M.id[i]<=l)/summary(G.M)[[6]][which(summary(G.M)[[2]]==T.M.id[i])]))



BS.M<-sapply(1:nrow(X.id), function(i) W.M[i]* ((Y.id[i,2]-mean(p.2[i,]))^2))
BS.D<-sapply(1:nrow(X.id), function(i) W.D[i]*((Y.id[i,3]-mean(p.3[i,]))^2))



BS.M<- (1/nrow(X.id)*mean(T.M.id>l))*sum(BS.M)
BS.D<- (1/nrow(X.id)*mean(T.D.id>l))*sum(BS.D)


B.M<-c(B.M, BS.M)
B.D<-c(B.D, BS.D)

S<-c(S, 1-mean(p.2)-mean(p.3))

P.M<-c(P.M, mean(p.2))
P.D<-c(P.D, mean(p.3))



}

B.M<-B.M[!is.na(B.M)]
P.M<-P.M[!is.na(B.M)]
S.M<-S[!is.na(B.M)]

B.D<-B.D[!is.na(B.D)]
P.D<-P.D[!is.na(B.D)]
S.D<-S[!is.na(B.D)]


w.M<-sapply(1:length(B.M), function(t) P.M[t]*prod(S.M[1:(t-1)]))
w.D<-sapply(1:length(B.D), function(t) P.D[t]*prod(S.D[1:(t-1)]))




Table.3.Lognormal<-c(Table.3.Lognormal, 100*(sum(B.M*w.M)+sum(B.D*w.D)),  100*(C.M*sum(Y[,2])+C.D*sum(Y[,3]))/sum(colSums(Y)[-1]))
save(Table.3.Lognormal, file="Table.3.Lognormal")

rm(Table.3.Lognormal, out, Marginal.Effects.Lognormal)




load("Table.3.Statespace")
load("Table.3.Cox")
load("Table.3.Lognormal")


Table.3<-matrix(0,nrow=6, ncol=3)
colnames(Table.3)<-c("State-space", "Cox", "Log-normal")
rownames(Table.3)<-c("Akaike Information Criterion (AIC)",
		     "Bayesian Information Criterion (BIC)",
		     "Consistent AIC (CAIC)", 
		     "Watanabe Information Criterion (WAIC)", 
		     "Integrated Prediction Error (IPE X 100)", 
		     "C-index (X 100)")
Table.3[,1]<-round(Table.3.Statespace, digits=2)
Table.3[,2]<-round(Table.3.Cox, digits=2)
Table.3[,3]<-round(Table.3.Lognormal,digits=2)

print(Table.3)


Table.3<-cbind(rownames(Table.3), Table.3)
rownames(Table.3)<-NULL


Titles<-matrix(c("", "Models", "", ""),nrow=1)

g1 <- tableGrob(Table.3, rows = NULL)
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g1), l = 1, r = ncol(g1))
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 1, l = 1, r = ncol(g1))

g0 <- tableGrob(Titles, rows = NULL)

g<-rbind(g0,g1)


pdf(file = "Table.3.pdf", height=5, width=15)
grid.draw(g)
dev.off()



################################################# Figure 3 ###############################################################################################################################

load("Marginal.Effects.Cox")
load("Marginal.Effects.Lognormal")
Marginal.Effects.Cox<-Marginal.Effects.Cox[c(1:8,37,38),]
Marginal.Effects.Lognormal<-Marginal.Effects.Lognormal[c(1:8,37,38),]
Marginal.Effects.Cox<-data.frame(Marginal.Effects.Cox)
Marginal.Effects.Lognormal<-data.frame(Marginal.Effects.Lognormal)
Marginal.Effects.Cox$Models<-"Cox"
Marginal.Effects.Lognormal$Models<-"Log-normal"



Models<-rbind(Marginal.Effects.Cox, Marginal.Effects.Lognormal)
names(Models)<-c("Death", "Covariate", "Mean", "Lo.CI", "Hi.CI", "Models" )
for (k in 3:(ncol(Models)-1)) {
Models[,k]<-as.numeric(Models[,k])
}




pd <- position_dodge(-0.5) 

Models$Death<-factor(Models$Death,
		levels=c("Dissolution",
			  "Merger"),
		labels=c("Dissolution Death", "Merger Death"))


Models$Covariate<-factor(Models$Covariate,
		levels=rev(c("Insider.Status", "Societal.Rootednes", "Ideological.Novelty",
		"Roots.in.Parties", "Interaction.Novelty.Roots")),
		labels=rev(c("Insider Status", "Societal Rootedness",
				"Ideological Novelty", 
				"Roots in Pre-existing Parties",
"Ideological Novelty x 
Roots in Pre-existing Parties")))





p<-ggplot(Models,
 aes(x=Covariate,y=Mean, colour=Models)) + 
    geom_errorbar(aes(ymin=Lo.CI, 
	ymax=Hi.CI, width=.2, colour=Models), size=1, position=pd) + 
	facet_wrap(~Death)+
geom_point(aes(shape=Models), size=3, position=pd)+
scale_color_manual(values=c("black", "gray71"))+
scale_shape_manual(values=c(17,19))+
theme_bw()+ylab("")+
xlab("")+
scale_y_continuous(breaks=c(-3, -1.5,0, 1.5), labels=c("-3", 
		"-1.5", "0", "1.5"))+
theme(axis.title.x = element_text(vjust = -2)) +    
theme(strip.text.x = element_text(size = 10, face="bold"))+
theme(strip.text.y = element_text(size = 10, face="bold"))+
theme(axis.text.x = element_text(size=10)) +
 theme(axis.text.y=element_text(size=10, color="black"))+
geom_hline(yintercept=0, linetype="dashed", 
                color = "gray", size=1)+
theme(legend.position="bottom")+
theme( plot.margin = margin(.1, .1, 0, .1, "cm"))+
theme(legend.text=element_text(size=12))+
theme(legend.title=element_text(size=12))+
guides(colour = guide_legend(title.position = "top", 
title.hjust = 0.5, title.vjust = 3.5))


pdf(file = "Figure2.pdf")
grid.draw(p+coord_flip())
dev.off()

rm(Marginal.Effects.Cox, Marginal.Effects.Lognormal)

################################################# Figure 3 ###############################################################################################################################


load("Dynamics_Statespace")

df.dissolution<-Dynamics.Statespace[Dynamics.Statespace[,1]=="Insider3",-1]
df.merger<-Dynamics.Statespace[Dynamics.Statespace[,1]=="Insider2",-1]

df.dissolution<-as.data.frame(cbind("Dissolution Death", 1:47, df.dissolution))
names(df.dissolution)<-c("Death", "x", "y", "lower", "upper")
df.merger<-as.data.frame(cbind("Merger Death", 1:47, df.merger))
names(df.merger)<-c("Death",  "x", "y", "lower", "upper")

data<-rbind(df.dissolution, df.merger)




df.dissolution2<-Dynamics.Statespace[Dynamics.Statespace[,1]=="Rooted3",-1]
df.merger2<-Dynamics.Statespace[Dynamics.Statespace[,1]=="Rooted2",-1]

df.dissolution2<-as.data.frame(cbind("Dissolution Death", 1:47, df.dissolution2))
names(df.dissolution2)<-c("Death", "x", "y", "lower", "upper")
df.merger2<-as.data.frame(cbind("Merger Death", 1:47, df.merger2))
names(df.merger2)<-c("Death",  "x", "y", "lower", "upper")


data2<-rbind(df.dissolution2, df.merger2)


data$Variable<-"Insider Status"
data2$Variable<-"Societally Rooted"

data3<-rbind(data, data2)

for (k in 2:(ncol(data3)-1)) {
data3[,k]<-as.numeric(data3[,k])
}



p<-ggplot(data3) + geom_line(aes(y=y, x=x), size=2)+ 
    geom_ribbon(data=data3,aes(x=x, ymin=lower,ymax=upper),alpha=0.4)+
 facet_grid(Variable ~ Death)+
geom_hline(yintercept=0, linetype="dashed")+
	theme_bw()+
	 theme(legend.position="none")+
	scale_x_continuous(limits = c(1, 47),
	breaks = c(1,5,10,15,20,25,30,35,40,45),
	labels=seq(0,45,by=5))+
xlab("Party Age (in years)")+ylab("Log-hazard of party death")+
	theme(strip.text.x = element_text(size=10, face="bold"),
          strip.text.y = element_text(size=10, face="bold"))

pdf(file = "Figure3.pdf")
grid.draw(p)
dev.off()

################################################# Figure 4 ###############################################################################################################################

load("Dynamics_Statespace")

df.dissolution.1<-Dynamics.Statespace[Dynamics.Statespace[,1]=="IdeologicallyNovel.Rooted3",-1]
df.merger.1<-Dynamics.Statespace[Dynamics.Statespace[,1]=="IdeologicallyNovel.Rooted2",-1]


df.dissolution.2<-Dynamics.Statespace[Dynamics.Statespace[,1]=="Interaction.IdeologicallyNovel.StructurallyNotNovel3",-1]
df.merger.2<-Dynamics.Statespace[Dynamics.Statespace[,1]=="Interaction.IdeologicallyNovel.StructurallyNotNovel2",-1]



df.dissolution.1<-cbind("Dissolution Death", "Ideologically Novel Parties 
with 
			Roots in Pre-existing Parties", 1:47, df.dissolution.1)
df.dissolution.1<-data.frame(df.dissolution.1)
names(df.dissolution.1)<-c("Death","Type", "x", "y", "lower", "upper")
df.merger.1<-cbind("Merger Death", "Ideologically Novel Parties 
with 
			Roots in Pre-existing Parties", 1:47, df.merger.1)
df.merger.1<-data.frame(df.merger.1)
names(df.merger.1)<-c("Death", "Type",  "x", "y", "lower", "upper")


df.dissolution.2<-cbind("Dissolution Death", "Ideologically Novel Parties 
without 
			Roots in Pre-existing Parties", 1:47, df.dissolution.2)
df.dissolution.2<-data.frame(df.dissolution.2)
names(df.dissolution.2)<-c("Death","Type",  "x", "y", "lower", "upper")
df.merger.2<-cbind("Merger Death", "Ideologically Novel Parties 
without 
			Roots in Pre-existing Parties", 1:47, df.merger.2)
df.merger.2<-data.frame(df.merger.2)
names(df.merger.2)<-c("Death","Type",  "x", "y", "lower", "upper")



data<-data.frame(rbind(df.dissolution.1, df.merger.1, df.dissolution.2, df.merger.2))

for (k in 3:ncol(data)) {
data[,k]<-as.numeric(data[,k])
}



p<-ggplot(data) + geom_line(aes(y=y, x=x), size=2)+ 
    geom_ribbon(data=data,aes(x=x, ymin=lower,ymax=upper),alpha=0.4)+
 facet_grid(Type ~ Death)+
geom_hline(yintercept=0, linetype="dashed")+
	theme_bw()+
	 theme(legend.position="none")+
	scale_x_continuous(limits = c(1, 47),
	breaks = c(1,5,10,15,20,25,30,35,40,45),
	labels=seq(0,45,by=5))+
xlab("Party Age (in years)")+ylab("Log-hazard of party death")+
	theme(strip.text.x = element_text(size=10, face="bold"),
          strip.text.y = element_text(size=10, face="bold"))


pdf(file = "Figure4.pdf")
grid.draw(p)
dev.off()

rm(Dynamics.Statespace)

######################################## TABLES AND FIGURES IN THE ONLINE APPENDIX ################################################################

################################################ SECTION A.1 #######################################################################################

################################################# Figure A.1 #######################################################################################


df<-read.csv("Dataset.csv")

df<-subset(df, select=c("votes.centered", "votes",
			"country",
			"case_id",
			"merged", "dissolved",
			"year_exist",
			"logseats.centered",  "log.seats",
		 "government",
			 "EU_parl",
		 "reg_gov",
		 "finance_party_qualify",
		 "efNPP",
	 "elec_coalition", 
	 "indPOL",
	"election_year_dummy",
	"distinctiveness",
	 "insider", 
	 "organisation", 
	 "newfamfirst",
	"OriginNew",
	"post.election.year",	
	"pre.election.year", 
	"seat.product"
	 ))

df<-na.exclude(df)

df$id<-paste(df$country, "-", df$case_id, sep="-")


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



df.Numparties<-c()
for (i in 1:length(unique(df$country))) {
df.Numparties<-rbind(df.Numparties,
c(as.character(unique(df$country)[i]),length(unique(df$id[df$country==unique(df$country)[i]])))
)
}
df.Numparties<-data.frame(df.Numparties)
names(df.Numparties)<-c("Country", "Parties")
df.Numparties[,2]<-as.numeric(df.Numparties[,2])


df.Numparties$Country<-factor(df.Numparties$Country, 
	levels=c("UK","Switzerland",
	"Sweden", "Spain", "Portugal", "Norway", "New Zealand",
	"Netherlands", "Luxembourg", "Japan", "Ireland", "Iceland",
	"Greece", "Germany", "France", "Finland", "Denmark",
	"Cyprus", "Canada", "Belgium", "Austria", "Australia"))

p1<-ggplot(df.Numparties,aes(x=Country,y=as.numeric(Parties)))+
  geom_bar(stat="identity", fill="gray")+theme_bw()+xlab("")+
	ggtitle("Number of parties")+
	coord_flip()+ylab("")+
	theme(plot.title = element_text(hjust = 0.5, size=10))


df.Age<-c()
for (i in 1:length(unique(df$country))) {
dd<-subset(df, country==unique(df$country)[i])
age<-by(dd$year_exist, dd$id,max)
df.Age<-rbind(df.Age,
c(as.character(unique(dd$country)), quantile(age, 0.25, na.rm=TRUE),
quantile(age, 0.75, na.rm=TRUE),
median(age, na.rm=TRUE),
quantile(age, 0.1, na.rm=TRUE),
quantile(age, 0.9, na.rm=TRUE))
)
}

df.Age<-data.frame(df.Age)
names(df.Age)<-c("Country", "lower", "upper", "middle", "ymin", "ymax")
for (k in 2:ncol(df.Age)) {
df.Age[,k]<-as.numeric(df.Age[,k])
}

df.Age$Country<-factor(df.Age$Country, levels=c("UK","Switzerland",
	"Sweden", "Spain", "Portugal", "Norway", "New Zealand",
	"Netherlands", "Luxembourg", "Japan", "Ireland", "Iceland",
	"Greece", "Germany", "France", "Finland", "Denmark",
	"Cyprus", "Canada", "Belgium", "Austria", "Australia"))

p2 <- ggplot(df.Age, aes(x=Country, lower=lower, upper=upper, middle=middle, ymin=ymin, ymax=ymax)) + 
geom_boxplot(stat="identity", outlier.shape=NA, fill="gray80")+theme_bw()+
xlab("")+ggtitle("Parties' Lifespan")+
theme(axis.title.y=element_text(vjust=1.25),
axis.title.x=element_text(vjust=-.35))+
ylab("Years")+	
theme(plot.title = element_text(hjust = 0.5, size=10))+
coord_flip()



pdf(file = "Figure.A.1.pdf", height=10)
grid.draw(multiplot(p1, p2, cols=2 ))
dev.off()


################################################# Figure A.2 ######################################################################################

df.Votes<-c()

for (i in 1:length(unique(df$country))) {
df.Votes<-rbind(df.Votes, c(as.character(unique(df$country)[i]),
quantile(df$votes[df$country==unique(df$country)[i]], 0.25, na.rm=TRUE),
quantile(df$votes[df$country==unique(df$country)[i]], 0.75, na.rm=TRUE),
median(df$votes[df$country==unique(df$country)[i]], na.rm=TRUE),
quantile(df$votes[df$country==unique(df$country)[i]], 0.1, na.rm=TRUE),
quantile(df$votes[df$country==unique(df$country)[i]], 0.9, na.rm=TRUE)
)
)
}


df.Votes<-data.frame(df.Votes)
names(df.Votes)<-c("Country", "lower", "upper", "middle", "ymin", "ymax")
for (k in 2:ncol(df.Votes)) {
df.Votes[,k]<-as.numeric(df.Votes[,k])
}


p<-ggplot(df.Votes, aes(x=Country, lower=lower, upper=upper, middle=middle, ymin=ymin, ymax=ymax)) + 
geom_boxplot(stat="identity", outlier.shape=NA, fill="gray80")+theme_bw()+
xlab("")+
theme(axis.title.y=element_text(vjust=1.25),
axis.title.x=element_text(vjust=-.35))+
ylab("%")+ theme(axis.text.x = element_text(angle = 90, vjust=0.4, hjust=1))



pdf(file = "Figure.A.2.pdf")
grid.draw(p)
dev.off()


################################################## SECTION A.2 #######################################################################################

################################################ Table A.1 ###########################################################################################
library(plyr)



Table.A.1<-cbind(c("Dissolution", "Merger", "Insider Status",
		"Societal Rootedness", "Ideological Novelty",
 		"Roots in Pre-existing Parties", "Seat Share",
		"Electoral Performance", "National Government",
		"State Funding", "EP Access", "Regional Government",
		"Party Distinctiveness", "Party Electoral Coalition",
		"Anti-establishment Vote", "Party System Fragmentation",
		"Seat Product", "Pre-Election Year", "Election Year",
		"Post-Election Year", "Party Age"),
rbind(c(round_any(mean(df$dissolved), 0.01), round_any(sd(df$dissolved), 0.01), paste(paste(paste("[", round_any(range(df$dissolved)[1], 0.01, f=floor), sep=""), ", ", sep=""),paste(round_any(range(df$dissolved)[2],0.01, f=floor), "]", sep=""))),
c(round_any(mean(df$merged), 0.01), round_any(sd(df$merged), 0.01), paste(paste(paste("[", round_any(range(df$merged)[1], 0.01, f=floor), sep=""), ", ", sep=""),paste(round_any(range(df$merged)[2],0.01, f=floor), "]", sep=""))),
c(round_any(mean(df$insider), 0.01, f=floor), round_any(sd(df$insider), 0.01), paste(paste(paste("[", round_any(range(df$insider)[1], 0.01, f=floor), sep=""), ", ", sep=""),paste(round_any(range(df$insider)[2],0.01, f=floor), "]", sep=""))),
c(round_any(mean(df$organisation), 0.01, f=floor), round_any(sd(df$organisation), 0.01), paste(paste(paste("[", round_any(range(df$organisation)[1], 0.01, f=floor), sep=""), ", ", sep=""),paste(round_any(range(df$organisation)[2],0.01, f=floor), "]", sep=""))),
c(round_any(mean(df$newfamfirst), 0.01, f=floor), round_any(sd(df$newfamfirst), 0.01), paste(paste(paste("[", round_any(range(df$newfamfirst)[1], 0.01, f=floor), sep=""), ", ", sep=""),paste(round_any(range(df$newfamfirst)[2],0.01, f=floor), "]", sep=""))),
c(round_any(mean(df$OriginNew), 0.01, f=floor), round_any(sd(df$OriginNew), 0.01), paste(paste(paste("[", round_any(range(df$OriginNew)[1], 0.01, f=floor), sep=""), ", ", sep=""),paste(round_any(range(df$OriginNew)[2],0.01, f=floor), "]", sep=""))),
c(round_any(mean(df$log.seats), 0.01), round_any(sd(df$log.seats), 0.01), paste(paste(paste("[", round_any(range(df$log.seats)[1], 0.01, f=floor), sep=""), ", ", sep=""),paste(round_any(range(df$log.seats)[2],0.01), "]", sep=""))),
c(round_any(mean(df$votes), 0.01), round_any(sd(df$votes), 0.01, f=floor), paste(paste(paste("[", round_any(range(df$votes)[1], 0.01, f=floor), sep=""), ", ", sep=""),paste(round_any(range(df$votes)[2],0.01, f=floor), "]", sep=""))),
c(round_any(mean(df$government), 0.01), round_any(sd(df$government), 0.01, f=floor), paste(paste(paste("[", round_any(range(df$government)[1], 0.01, f=floor), sep=""), ", ", sep=""),paste(round_any(range(df$government)[2],0.01, f=floor), "]", sep=""))),
c(round_any(mean(df$finance_party_qualify), 0.01, f=floor), round_any(sd(df$finance_party_qualify), 0.01, f=floor), paste(paste(paste("[", round_any(range(df$finance_party_qualify)[1], 0.01, f=floor), sep=""), ", ", sep=""),paste(round_any(range(df$finance_party_qualify)[2],0.01, f=floor), "]", sep=""))),
c(round_any(mean(df$EU_parl), 0.01, f=floor), round_any(sd(df$EU_parl), 0.01, f=floor), paste(paste(paste("[", round_any(range(df$EU_parl)[1], 0.01, f=floor), sep=""), ", ", sep=""),paste(round_any(range(df$EU_parl)[2],0.01, f=floor), "]", sep=""))),
c(round_any(mean(df$reg_gov), 0.01, f=floor), round_any(sd(df$reg_gov), 0.01, f=floor), paste(paste(paste("[", round_any(range(df$reg_gov)[1], 0.01, f=floor), sep=""), ", ", sep=""),paste(round_any(range(df$reg_gov)[2],0.01, f=floor), "]", sep=""))),
c(round_any(mean(df$distinctiveness), 0.01), round_any(sd(df$distinctiveness), 0.01), paste(paste(paste("[", round_any(range(df$distinctiveness)[1], 0.01, f=floor), sep=""), ", ", sep=""),paste(round_any(range(df$distinctiveness)[2],0.01, f=floor), "]", sep=""))),
c(round_any(mean(df$elec_coalition), 0.01), round_any(sd(df$elec_coalition), 0.01), paste(paste(paste("[", round_any(range(df$elec_coalition)[1], 0.01, f=floor), sep=""), ", ", sep=""),paste(round_any(range(df$elec_coalition)[2],0.01, f=floor), "]", sep=""))),
c(round_any(mean(df$indPOL), 0.01), round_any(sd(df$indPOL), 0.01), paste(paste(paste("[", round_any(range(df$indPOL)[1], 0.01, f=floor), sep=""), ", ", sep=""),paste(round_any(range(df$indPOL)[2],0.01, f=floor), "]", sep=""))),
c(round_any(mean(df$efNPP), 0.01), round_any(sd(df$efNPP), 0.01), paste(paste(paste("[", round_any(range(df$efNPP)[1], 0.01, f=floor), sep=""), ", ", sep=""),paste(round_any(range(df$efNPP)[2],0.01, f=floor), "]", sep=""))),
c(round_any(mean(df$seat.product), 0.01), round_any(sd(df$seat.product), 0.01), paste(paste(paste("[", round_any(range(df$seat.product)[1], 0.01, f=floor), sep=""), ", ", sep=""),paste(round_any(range(df$seat.product)[2],0.01, f=floor), "]", sep=""))),
c(round_any(mean(df$pre.election.year), 0.01), round_any(sd(df$pre.election.year), 0.01), paste(paste(paste("[", round_any(range(df$pre.election.year)[1], 0.01, f=floor), sep=""), ", ", sep=""),paste(round_any(range(df$pre.election.year)[2],0.01, f=floor), "]", sep=""))),
c(round_any(mean(df$election_year_dummy), 0.01), round_any(sd(df$election_year_dummy), 0.01), paste(paste(paste("[", round_any(range(df$election_year_dummy)[1], 0.01, f=floor), sep=""), ", ", sep=""),paste(round_any(range(df$election_year_dummy)[2],0.01, f=floor), "]", sep=""))),
c(round_any(mean(df$post.election.year), 0.01), round_any(sd(df$post.election.year), 0.01), paste(paste(paste("[", round_any(range(df$post.election.year)[1], 0.01, f=floor), sep=""), ", ", sep=""),paste(round_any(range(df$post.election.year)[2],0.01, f=floor), "]", sep=""))),
c(round_any(mean(df$year_exist), 0.01), round_any(sd(df$year_exist), 0.01, f=floor), paste(paste(paste("[", round_any(range(df$year_exist)[1], 0.01), sep=""), ", ", sep=""),paste(round_any(range(df$year_exist)[2], 0.01), "]", sep=""))))
)
colnames(Table.A.1)<-c("Variable", "Mean", "Std. Dev", "Range")

print(Table.A.1)



groups<-c("Outcomes", "", "", "Formative characteristics", "", "", "", "Party-level controls", rep("",7), "System-level controls", rep("",4), "Duraction Covariate")



Table.A.1<-cbind(groups, Table.A.1)
colnames(Table.A.1)<-NULL



colnames(Table.A.1)<-c("", "Variable", "Mean", "Std. Dev.", "Range")

g <- tableGrob(Table.A.1, rows = NULL)
g <- gtable_add_grob(g,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g), l = 1, r = ncol(g))
g <- gtable_add_grob(g,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 1, l = 1, r = ncol(g))

g<- gtable_add_grob(g,
        grobs = segmentsGrob(
	x0 = unit(0,"npc"),
                       y0 = unit(0,"npc"),
                       x1 = unit(6,"npc"),
                       y1 = unit(0,"npc"),
                       gp = gpar(lwd = 1.0)),
        t = 3, b = 3, l = 1, r = ncol(g))	

g<- gtable_add_grob(g,
        grobs = segmentsGrob(
	x0 = unit(0,"npc"),
                       y0 = unit(0,"npc"),
                       x1 = unit(6,"npc"),
                       y1 = unit(0,"npc"),
                       gp = gpar(lwd = 1.0)),
        t = 7, b = 7, l = 1, r = ncol(g))

g<- gtable_add_grob(g,
        grobs = segmentsGrob(
	x0 = unit(0,"npc"),
                       y0 = unit(0,"npc"),
                       x1 = unit(6,"npc"),
                       y1 = unit(0,"npc"),
                       gp = gpar(lwd = 1.0)),
        t = 15, b = 15, l = 1, r = ncol(g))	

g<- gtable_add_grob(g,
        grobs = segmentsGrob(
	x0 = unit(0,"npc"),
                       y0 = unit(0,"npc"),
                       x1 = unit(6,"npc"),
                       y1 = unit(0,"npc"),
                       gp = gpar(lwd = 1.0)),
        t = 21, b = 21, l = 1, r = ncol(g))	


pdf(file = "Table.A.1.pdf", width=8)
grid.draw(g)
dev.off()

################################################ Table A.2 ###########################################################################################
# Function to compute condition indexes
colldiag <- function(mod,scale=TRUE,center=FALSE,add.intercept=TRUE) {
	result <- NULL
	if (center) add.intercept<-FALSE
	if (is.matrix(mod)||is.data.frame(mod)) {
		X<-as.matrix(mod)
		nms<-colnames(mod)
	}
	else if (!is.null(mod$call$formula)) {
		X<-mod$model[,-1] # delete the dependent variable
	}
	X<-na.omit(X) # delete missing cases
	if (add.intercept) {
		X<-cbind(1,X) # add the intercept
		colnames(X)[1]<-"intercept"
	}
	X<-scale(X,scale=scale,center=center)

	svdX<-svd(X)
	svdX$d
	condindx<-svdX$d[1]/svdX$d

	Phi=svdX$v%*%diag(1/svdX$d)
	Phi<-t(Phi^2)
	pi<-prop.table(Phi,2)

	dim(condindx)<-c(length(condindx),1)
	colnames(condindx)<-"cond.index"
	rownames(condindx)<-1:nrow(condindx)
	colnames(pi)<-colnames(X)
	result$condindx<-condindx
	result$pi<-pi
	class(result)<-"colldiag"
	result
}

print.colldiag <- function(x,dec.places=3,fuzz=NULL,fuzzchar=".",...){
	stopifnot(fuzz>0 & fuzz<1)
	stopifnot(is.character(fuzzchar))
	stopifnot(nchar(fuzzchar)==1)
	fuzzchar<-paste(" ",fuzzchar,sep="")
	width<-dec.places+2
	pi<-formatC(x$pi,format="f",width=width,digits=dec.places)
	if (!is.null(fuzz )) {
		pi[pi < fuzz] <- fuzzchar
	}
	width<-max(nchar(trunc(max(x$condindx))))+dec.places+2
	condindx<-formatC(x$condindx,format="f",width=width,digits=dec.places)
	colnames(condindx)<-NULL
	cat("Condition\nIndex\tVariance Decomposition Proportions\n")
	print(noquote(cbind(condindx,pi)))
}



dd<-cbind(df$insider, df$organisation, df$newfamfirst,
		1-df$OriginNew, df$logseats.centered, df$votes.centered,
		df$government, df$finance_party_qualify, df$EU_parl,
  		df$reg_gov, df$distinctiveness, df$elec_coalition, 
		# centering the continuous predictors that were not already centered
		(df$indPOL-mean(df$indPOL))/(2*sd(df$indPOL)),
		(df$efNPP-mean(df$efNPP))/(2*sd(df$efNPP)),
		(df$seat.product-mean(df$seat.product))/(2*sd(df$seat.product)),
		df$pre.election.year, df$election_year_dummy, df$post.election.year)
dd<-data.frame(dd)

names(dd)<-c("Insider Status", "Societal Rootedness", "Ideological Novelty",
 		"Roots in Pre-existing Parties", "Seat Share",
		"Electoral Performance", "National Government",
		"State Funding", "EP Access", "Regional Government",
		"Party Distinctiveness", "Party Electoral Coalition",
		"Anty-establishment Vote", "Party System Fragmentation",
		"Seat Product", "Pre-Election Year", "Election Year",
		"Post-Election Year")



Table.A.2<-cbind(names(dd),round(matrix(colldiag(dd)$condindx[-1]),digits=2))
colnames(Table.A.2)<-c("Variable", "Condition Indexes")

print(Table.A.2)



groups<-c("Formative characteristics", rep("",4), "Party-level controls", rep("",7), "System-level controls", rep("",4))


Table.A.2<-cbind(groups, Table.A.2)
colnames(Table.A.2)<-NULL

colnames(Table.A.2)<-c("", "Variable", "Condition Indexes")


g <- tableGrob(Table.A.2, rows = NULL)
g <- gtable_add_grob(g,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g), l = 1, r = ncol(g))
g <- gtable_add_grob(g,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 1, l = 1, r = ncol(g))



g<- gtable_add_grob(g,
        grobs = segmentsGrob(
	x0 = unit(0,"npc"),
                       y0 = unit(0,"npc"),
                       x1 = unit(6,"npc"),
                       y1 = unit(0,"npc"),
                       gp = gpar(lwd = 1.0)),
        t = 5, b = 5, l = 1, r = ncol(g))	


g<- gtable_add_grob(g,
        grobs = segmentsGrob(
	x0 = unit(0,"npc"),
                       y0 = unit(0,"npc"),
                       x1 = unit(6,"npc"),
                       y1 = unit(0,"npc"),
                       gp = gpar(lwd = 1.0)),
        t = 13, b = 13, l = 1, r = ncol(g))	



pdf(file = "Table.A.2.pdf")
grid.draw(g)
dev.off()


############################################### Figure A.3 ########################################################################################

Joint<-rbind(cbind(rownames(by(df$merged, df$year_exist,sum)),
by(df$merged, df$year_exist,sum), "Mergers"),
cbind(rownames(by(df$merged, df$year_exist,sum)),
by(df$dissolved, df$year_exist,sum), "Dissolutions"))
Joint<-data.frame(Joint)
names(Joint)<-c("Life", "Death", "Type")
Joint$Life<- as.numeric(Joint$Life)
Joint$Death<- as.numeric(Joint$Death)


p<-ggplot(Joint,aes(x=Life,y=Death))+
  geom_bar(stat="identity", fill="gray")+theme_bw()+xlab("Party Age (in years)")+
	ylab("Number of Parties")+
	scale_x_continuous(limits=c(0,41), breaks = c(seq(0,40,by=10)))+
	facet_wrap(~ Type, scales="free_x")+
     theme(strip.text.x = element_text(size = 12, face="bold"))

pdf(file = "Figure.A.3.pdf")
grid.draw(p)
dev.off()


############################################### Figure A.4 #########################################################################################
Joint<-rbind(cbind(rownames(by(df$merged, df$country,sum)),
by(df$merged, df$country,sum), "Mergers"),
cbind(rownames(by(df$merged, df$country,sum)),
by(df$dissolved, df$country,sum), "Dissolutions"))
Joint<-data.frame(Joint)
names(Joint)<-c("Country", "Death", "Type")
Joint$Death<- as.numeric(Joint$Death)
Joint$Country<-factor(Joint$Country, levels=c("UK","Switzerland",
	"Sweden", "Spain", "Portugal", "Norway", "New Zealand",
	"Netherlands", "Luxembourg", "Japan", "Ireland", "Iceland",
	"Greece", "Germany", "France", "Finland", "Denmark",
	"Cyprus", "Canada", "Belgium", "Austria", "Australia"))


p<-ggplot(Joint,aes(x=Country,y=Death))+
  geom_bar(stat="identity", fill="gray")+theme_bw()+xlab("")+
	scale_y_continuous(limits=c(0,8))+
	facet_wrap(~ Type)+coord_flip()+ylab("Number of Party Deaths")+
theme(strip.text.x = element_text(size = 12, face="bold"))

pdf(file = "Figure.A.4.pdf")
grid.draw(p)
dev.off()


############################################## Figure A.5 #############################################################################################

Active.Parties<-cbind(1:47, table(df$year_exist))

Active.Parties<-data.frame(Active.Parties)
names(Active.Parties)<-c("Age", "Parties")


p <- ggplot(Active.Parties, aes(Age, Parties))+geom_point()+
    ylab("Number of parties")+xlab("Party Age (in years)")+theme_bw()
pdf(file = "Figure.A.5.pdf")
grid.draw(p)
dev.off()


############################################## Figure A.6 #############################################################################################
library(psych)
sessionInfo()
dd<-cbind(df$case_id, df$organisation,  df$newfamfirst,
		df$insider, 1-df$OriginNew)
ddd<-dd[!duplicated(dd),]
ddd<-data.frame(ddd)

Corrs<-c()

for (j in 2:ncol(ddd)) 

for (k in 1:3) {

Corrs<-c(Corrs, abs(phi(table(ddd[,j], ddd[,-c(1,j)][,k]))))

}


print("Range of correlations between formative characteristics")
print(range(Corrs))



names(ddd)[2]<-"Rooted"
names(ddd)[3]<-"Ideologically.Novel"
names(ddd)[4]<-"Insider"
names(ddd)[5]<-"Organizationally.Rooted"


ddd$combination<-paste(ddd$Insider, ddd$Rooted,  ddd$Ideologically.Novel,
			ddd$Organizationally.Rooted, sep="-")

Type<-cbind(rownames(table(ddd$combination)),
			table(ddd$combination))
rownames(Type)<-NULL

Configurations<-matrix(NA, 16, 4)
Configurations[,4]<-Type[,2]

for (j in 1:nrow(Type)) {

a<-unlist(strsplit(Type[j,1], "-"))

if (a[1]=="1") {
Configurations[j,1]<-"Insider Status"
} else if (a[1]=="0") {
Configurations[j,1]<-"No Insider Status"
}

if (a[2]=="1") {
Configurations[j,2]<-"Societally Rooted"
} else if (a[2]=="0") {
Configurations[j,2]<-"Not Societally Rooted"
}

if (a[3]=="1" & a[4]=="1") {
Configurations[j,3]<-"Ideologically Novel with 
Roots in Pre-existing Parties"
} else if (a[3]=="1" & a[4]=="0") {
Configurations[j,3]<-"Ideologically Novel without 
Roots in Pre-existing Parties"
} else if (a[3]=="0" & a[4]=="1") {
Configurations[j,3]<-"Not Ideologically Novel with 
Roots in Pre-existing Parties"
} else if (a[3]=="0" & a[4]=="0") {
Configurations[j,3]<-"Not Ideologically Novel without 
Roots in Pre-existing Parties"
} 


}

Configurations<-data.frame(Configurations)
names(Configurations)<-c("Insider.Status", "Societal.Rootedness", 
			"Variable", "Frequency")

Configurations[,4]<-as.numeric(Configurations[,4])

Configurations[,4]<-100*Configurations[,4]/150


Configurations$Variable<-factor(Configurations$Variable, 
levels=rev(c("Ideologically Novel with \nRoots in Pre-existing Parties",
"Ideologically Novel without \nRoots in Pre-existing Parties",
"Not Ideologically Novel with \nRoots in Pre-existing Parties",
"Not Ideologically Novel without \nRoots in Pre-existing Parties")))

Configurations$Societal.Rootedness<-factor(Configurations$Societal.Rootedness,
levels=c("Societally Rooted", "Not Societally Rooted"))

p<-ggplot(data=Configurations, aes(x=Variable, y=Frequency)) +
  geom_bar(stat="identity", fill="gray", width=0.5)+
  facet_grid(Insider.Status~Societal.Rootedness)+ylab("As % of all sample parties")+xlab("")+
 scale_y_continuous(breaks=c(0, 10, 20, 30, 40))+theme_bw()+
 theme(strip.text=element_text(size=8),
	axis.text.x=element_text(size=8),
	axis.title=element_text(size=9),
	axis.text.y=element_text(size=9))+
theme(plot.margin = margin(0, 0, 0.2, -.15, "cm"))
	

pdf(file = "Figure.A.6.pdf")
grid.draw(p+ coord_flip())
dev.off()



############################################### SECTION A.4 ##################################################################################################


############################################### Figure A.7 ###############################################################################################################################

load("FA7")

F.A.7$Covariate<- factor(F.A.7$Covariate, 
			levels = rev(c("Organizationally Rooted &
 Not Ideologically Novel", "Built from scratch & 
 Ideologically Novel", "Built from scratch & Not
 Ideologically Novel")), 
labels=rev(c("Rooted in Pre-existing Parties
& Not Ideologically Novel", "Built from scratch
& Ideologically Novel", "Built from scratch
& Not Ideologically Novel"))
)


F.A.7$Dep.Variable<-factor(F.A.7$Dep.Variable, 
				levels=c("Dissolution", "Merger"),
				labels=c("Dissolution 
Death", "Merger
 Death"))


p<-ggplot(F.A.7,
 aes(x=Covariate,y=Mean)) + 
    geom_errorbar(aes(ymin=Lo.CI, 
	ymax=Hi.CI, width=.2), size=1,colour="black") + 
	facet_wrap(~Dep.Variable, scales="free")+
    geom_point(size=3)+
theme_bw()+ylab("")+
xlab("")+
theme(strip.text.x = element_text(size = 12, face="bold"))+
theme(axis.text.y = element_text(size=8)) +
 theme(axis.title=element_text(size=7))+
 theme(axis.text.x=element_text(size=9))+
 theme(axis.text.y=element_text(size=12))+
theme(plot.margin = margin(0.2, 0.15, 0, 0, "cm"))


pdf(file = "Figure.A.7.pdf")
grid.draw(p+ coord_flip())
dev.off()


################################################# Table A.3 ###############################################################################################################################


load("Marginal.Effects.Cox")
Marginal.Effects.Cox<-Marginal.Effects.Cox[9:36,]
Marginal.Effects.Cox<-data.frame(Marginal.Effects.Cox)

Marginal.Effects.Cox.Dissolution<-Marginal.Effects.Cox[Marginal.Effects.Cox$Dep.Variable=="Dissolution",]
Marginal.Effects.Cox.Dissolution<-Marginal.Effects.Cox.Dissolution[c(2,1,3,6,4,5,14,8,9,7,10,13,11,12),] 


Marginal.Effects.Cox.Merger<-Marginal.Effects.Cox[Marginal.Effects.Cox$Dep.Variable=="Merger",]
Marginal.Effects.Cox.Merger<-Marginal.Effects.Cox.Merger[c(2,1,3,6,4,5,14,8,9,7,10,13,11,12),] 




load("Marginal.Effects.Lognormal")
Marginal.Effects.Lognormal<-Marginal.Effects.Lognormal[9:36,]
Marginal.Effects.Lognormal<-data.frame(Marginal.Effects.Lognormal)

Marginal.Effects.Lognormal.Dissolution<-Marginal.Effects.Lognormal[Marginal.Effects.Lognormal$Dep.Variable=="Dissolution",]
Marginal.Effects.Lognormal.Dissolution<-Marginal.Effects.Lognormal.Dissolution[c(2,1,3,6,4,5,14,8,9,7,10,13,11,12),] 


Marginal.Effects.Lognormal.Merger<-Marginal.Effects.Lognormal[Marginal.Effects.Lognormal$Dep.Variable=="Merger",]
Marginal.Effects.Lognormal.Merger<-Marginal.Effects.Lognormal.Merger[c(2,1,3,6,4,5,14,8,9,7,10,13,11,12),] 


Cox.Dissolution.Matrix<-matrix(0, 14,4)
Cox.Dissolution.Matrix[,1]<-c("Seat Share",
		"Electoral Performance", "National Government",
		"State Funding", "EP Access", "Regional Government",
		"Party Distinctiveness", "Party Electoral Coalition",
		"Anti-establishment Vote", "Party System Fragmentation",
		"Seat Product", "Pre-Election Year", "Election Year",
		"Post-Election Year")
Cox.Dissolution.Matrix[,2]<-"Dissolution"


for (j in 1:nrow(Cox.Dissolution.Matrix)) {

m<-round(as.numeric(Marginal.Effects.Cox.Dissolution[j,3]), digits=2)
q.l<-round(as.numeric(Marginal.Effects.Cox.Dissolution[j,4]), digits=2)
q.h<-round(as.numeric(Marginal.Effects.Cox.Dissolution[j,5]), digits=2)


if (nchar(m)<5 & grepl("\\.", m)==FALSE) {
m<-paste(m,".", sep="")
while (nchar(m)<5) {
m<-paste(m, "0", sep="")
} 
} else if (nchar(m)<4 & grepl("\\.", m)==TRUE) {
while (nchar(m)<4) {
m<-paste(m, "0", sep="")
} 
}


if (nchar(q.l)<5 & grepl("\\.", q.l)==FALSE) {
q.l<-paste(q.l,".", sep="")
while (nchar(q.l)<5) {
q.l<-paste(q.l, "0", sep="")
} 
} else if (nchar(q.l)<4 & grepl("\\.", q.l)==TRUE) {
while (nchar(q.l)<4) {
q.l<-paste(q.l, "0", sep="")
} 
}

if (nchar(q.h)<5 & grepl("\\.", q.h)==FALSE) {
q.h<-paste(q.h,".", sep="")
while (nchar(q.h)<5) {
q.h<-paste(q.h, "0", sep="")
} 
} else if (nchar(q.h)<4 & grepl("\\.", q.h)==TRUE) {
while (nchar(q.h)<4) {
q.h<-paste(q.h, "0", sep="")
} 
}

Cox.Dissolution.Matrix[j,3:4]<-c(m, paste(paste("(", q.l, ",", sep=""), paste(q.h,")", sep=""),sep=""))


}



Cox.Merger.Matrix<-matrix(0, 14,4)
Cox.Merger.Matrix[,1]<-c("Seat Share",
		"Electoral Performance", "National Government",
		"State Funding", "EP Access", "Regional Government",
		"Party Distinctiveness", "Party Electoral Coalition",
		"Anti-establishment Vote", "Party System Fragmentation",
		"Seat Product", "Pre-Election Year", "Election Year",
		"Post-Election Year")
Cox.Merger.Matrix[,2]<-"Merger"


for (j in 1:nrow(Cox.Merger.Matrix)) {

m<-round(as.numeric(Marginal.Effects.Cox.Merger[j,3]), digits=2)
q.l<-round(as.numeric(Marginal.Effects.Cox.Merger[j,4]), digits=2)
q.h<-round(as.numeric(Marginal.Effects.Cox.Merger[j,5]), digits=2)


if (nchar(m)<5 & grepl("\\.", m)==FALSE) {
m<-paste(m,".", sep="")
while (nchar(m)<5) {
m<-paste(m, "0", sep="")
} 
} else if (nchar(m)<4 & grepl("\\.", m)==TRUE) {
while (nchar(m)<4) {
m<-paste(m, "0", sep="")
} 
}


if (nchar(q.l)<5 & grepl("\\.", q.l)==FALSE) {
q.l<-paste(q.l,".", sep="")
while (nchar(q.l)<5) {
q.l<-paste(q.l, "0", sep="")
} 
} else if (nchar(q.l)<4 & grepl("\\.", q.l)==TRUE) {
while (nchar(q.l)<4) {
q.l<-paste(q.l, "0", sep="")
} 
}

if (nchar(q.h)<5 & grepl("\\.", q.h)==FALSE) {
q.h<-paste(q.h,".", sep="")
while (nchar(q.h)<5) {
q.h<-paste(q.h, "0", sep="")
} 
} else if (nchar(q.h)<4 & grepl("\\.", q.h)==TRUE) {
while (nchar(q.h)<4) {
q.h<-paste(q.h, "0", sep="")
} 
}

Cox.Merger.Matrix[j,3:4]<-c(m, paste(paste("(", q.l, ",", sep=""), paste(q.h,")", sep=""),sep=""))


}




Table.A3.Cox<-rbind(Cox.Dissolution.Matrix,Cox.Merger.Matrix)
colnames(Table.A3.Cox)<-c("Variable", "Type of Death", "Mean", "Interval")

print(Table.A3.Cox)


Lognormal.Dissolution.Matrix<-matrix(0, 14,4)
Lognormal.Dissolution.Matrix[,1]<-c("Seat Share",
		"Electoral Performance", "National Government",
		"State Funding", "EP Access", "Regional Government",
		"Party Distinctiveness", "Party Electoral Coalition",
		"Anti-establishment Vote", "Party System Fragmentation",
		"Seat Product", "Pre-Election Year", "Election Year",
		"Post-Election Year")
Lognormal.Dissolution.Matrix[,2]<-"Dissolution"


for (j in 1:nrow(Lognormal.Dissolution.Matrix)) {

m<-round(as.numeric(Marginal.Effects.Lognormal.Dissolution[j,3]), digits=2)
q.l<-round(as.numeric(Marginal.Effects.Lognormal.Dissolution[j,4]), digits=2)
q.h<-round(as.numeric(Marginal.Effects.Lognormal.Dissolution[j,5]), digits=2)


if (nchar(m)<5 & grepl("\\.", m)==FALSE) {
m<-paste(m,".", sep="")
while (nchar(m)<5) {
m<-paste(m, "0", sep="")
} 
} else if (nchar(m)<4 & grepl("\\.", m)==TRUE) {
while (nchar(m)<4) {
m<-paste(m, "0", sep="")
} 
}


if (nchar(q.l)<5 & grepl("\\.", q.l)==FALSE) {
q.l<-paste(q.l,".", sep="")
while (nchar(q.l)<5) {
q.l<-paste(q.l, "0", sep="")
} 
} else if (nchar(q.l)<4 & grepl("\\.", q.l)==TRUE) {
while (nchar(q.l)<4) {
q.l<-paste(q.l, "0", sep="")
} 
}

if (nchar(q.h)<5 & grepl("\\.", q.h)==FALSE) {
q.h<-paste(q.h,".", sep="")
while (nchar(q.h)<5) {
q.h<-paste(q.h, "0", sep="")
} 
} else if (nchar(q.h)<4 & grepl("\\.", q.h)==TRUE) {
while (nchar(q.h)<4) {
q.h<-paste(q.h, "0", sep="")
} 
}

Lognormal.Dissolution.Matrix[j,3:4]<-c(m, paste(paste("(", q.l, ",", sep=""), paste(q.h,")", sep=""),sep=""))


}




Lognormal.Merger.Matrix<-matrix(0, 14,4)
Lognormal.Merger.Matrix[,1]<-c("Seat Share",
		"Electoral Performance", "National Government",
		"State Funding", "EP Access", "Regional Government",
		"Party Distinctiveness", "Party Electoral Coalition",
		"Anti-establishment Vote", "Party System Fragmentation",
		"Seat Product", "Pre-Election Year", "Election Year",
		"Post-Election Year")
Lognormal.Merger.Matrix[,2]<-"Merger"


for (j in 1:nrow(Lognormal.Merger.Matrix)) {

m<-round(as.numeric(Marginal.Effects.Lognormal.Merger[j,3]), digits=2)
q.l<-round(as.numeric(Marginal.Effects.Lognormal.Merger[j,4]), digits=2)
q.h<-round(as.numeric(Marginal.Effects.Lognormal.Merger[j,5]), digits=2)


if (nchar(m)<5 & grepl("\\.", m)==FALSE) {
m<-paste(m,".", sep="")
while (nchar(m)<5) {
m<-paste(m, "0", sep="")
} 
} else if (nchar(m)<4 & grepl("\\.", m)==TRUE) {
while (nchar(m)<4) {
m<-paste(m, "0", sep="")
} 
}


if (nchar(q.l)<5 & grepl("\\.", q.l)==FALSE) {
q.l<-paste(q.l,".", sep="")
while (nchar(q.l)<5) {
q.l<-paste(q.l, "0", sep="")
} 
} else if (nchar(q.l)<4 & grepl("\\.", q.l)==TRUE) {
while (nchar(q.l)<4) {
q.l<-paste(q.l, "0", sep="")
} 
}

if (nchar(q.h)<5 & grepl("\\.", q.h)==FALSE) {
q.h<-paste(q.h,".", sep="")
while (nchar(q.h)<5) {
q.h<-paste(q.h, "0", sep="")
} 
} else if (nchar(q.h)<4 & grepl("\\.", q.h)==TRUE) {
while (nchar(q.h)<4) {
q.h<-paste(q.h, "0", sep="")
} 
}

Lognormal.Merger.Matrix[j,3:4]<-c(m, paste(paste("(", q.l, ",", sep=""), paste(q.h,")", sep=""),sep=""))


}



Table.A3.Lognormal<-rbind(Lognormal.Dissolution.Matrix,Lognormal.Merger.Matrix)
colnames(Table.A3.Lognormal)<-c("Variable", "Type of Death", "Mean", "Interval")
print(Table.A3.Lognormal)



groups<-c(rep("",3), "Party-level Controls", rep("",4), rep("",3), "System-level Controls", rep("",2))


Table.A3<-cbind(groups, Cox.Dissolution.Matrix[,-c(2)],Cox.Merger.Matrix[,3:4], 
	Lognormal.Dissolution.Matrix[,3:4], Lognormal.Merger.Matrix[,3:4])
Table.A3<-rbind(c("", "", rep(c("Mean", "Interval"),4)), Table.A3)
colnames(Table.A3)<-c("", "Variable", rep(c("Dissolution", "Death", "Merger", "Death"),2))


Titles<-matrix(c("", "", "Cox", "proportional", "hazards", "model", "", "Log-normal", "model", ""),nrow=1)



g1 <- tableGrob(Table.A3, rows = NULL)
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 1, b = nrow(g1), l = 1, r = ncol(g1))
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 1, b = nrow(g1), l = 1, r = 1)
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 1, b = nrow(g1), l = 2, r = 6)


g1 <- gtable_add_grob(g1,
        grobs = segmentsGrob(
	x0 = unit(0,"npc"),
                       y0 = unit(0,"npc"),
                       x1 = unit(0,"npc"),
                       y1 = unit(4,"npc"),
                       gp = gpar(lwd = 1.0)),
        t = 0, b = nrow(g1), l = 3, r = 3)	


g1 <- gtable_add_grob(g1,
        grobs = segmentsGrob(
	x0 = unit(0,"npc"),
                       y0 = unit(0,"npc"),
                       x1 = unit(4,"npc"),
                       y1 = unit(0,"npc"),
                       gp = gpar(lwd = 1.0)),
        t = 10, b=10, l = 1, r = ncol(g1))

g1 <- gtable_add_grob(g1,
        grobs = segmentsGrob(
	x0 = unit(0,"npc"),
                       y0 = unit(0,"npc"),
                       x1 = unit(4,"npc"),
                       y1 = unit(0,"npc"),
                       gp = gpar(lwd = 1.0)),
        t = 2, b=2, l = 1, r = ncol(g1))

g0 <- tableGrob(Titles, rows = NULL)
g0 <- gtable_add_grob(g0,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 1, b = nrow(g0), l = 1, r = ncol(g0))
g0<- gtable_add_grob(g0,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 1, b = nrow(g0), l = 1, r = 1)
g0 <- gtable_add_grob(g0,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 1, b = nrow(g0), l = 2, r = 6)
g<-rbind(g0,g1)


pdf(file = "Table.A.3.pdf", height=10, width=15)
grid.draw(g)
dev.off()

rm(Marginal.Effects.Cox, Marginal.Effects.Lognormal)

################################################# Table A.4 ###############################################################################################################################


# In order to produce Table A.4, we first need to compute the goodness of fit measures of 
# the models modelling the dynmamics as quadratic, logarithmic and linear functions of 
# party age 

# Model specifying the coefficients of the formative features as quadratic functions of parties’ age
load("Estimates_Quadratic") 


# Convergence checks - Quadratic specification
BETA2<-mcmc.list(as.mcmc(t(out$beta2[,,1])), as.mcmc(t(out$beta2[,,2])),
	 as.mcmc(t(out$beta2[,,3])))


BETA3<-mcmc.list(as.mcmc(t(out$beta3[,,1])), as.mcmc(t(out$beta3[,,2])),
	 as.mcmc(t(out$beta3[,,3])))

print(paste("Convergence, Parameter Estimates of the Quadratic Specification:", gelman.diag(BETA2)[[2]]<1.2 & gelman.diag(BETA3)[[2]]<1.2, sep=" "))


# Pooled convergent draws - to be used for inference
Beta2<-rbind(t(out$beta2[, pool:20000, 1]),
		t(out$beta2[,pool:20000 , 2]),
		t(out$beta2[, pool:20000, 3]))


Beta3<-rbind(t(out$beta3[, pool:20000, 1]),
		t(out$beta3[, pool:20000, 2]),
		t(out$beta3[,pool:20000 , 3]))

Reffect.Party2<-rbind(t(out$reffect_party[1:204, pool:20000, 1]),
		t(out$reffect_party[1:204, pool:20000, 2]),
		t(out$reffect_party[1:204,pool:20000 , 3]))
Reffect.Party3<-rbind(t(out$reffect_party[205:408, pool:20000, 1]),
		t(out$reffect_party[205:408,pool:20000 , 2]),
		t(out$reffect_party[205:408,pool:20000 , 3]))
Reffect.Country2<-rbind(t(out$reffect_country[1:22,pool:20000 , 1]),
		t(out$reffect_country[1:22,pool:20000 , 2]),
		t(out$reffect_country[1:22,pool:20000 , 3]))
Reffect.Country3<-rbind(t(out$reffect_country[23:44,pool:20000 , 1]),
		t(out$reffect_country[23:44, pool:20000, 2]),
		t(out$reffect_country[23:44,pool:20000 , 3]))


## LOAD DATA 
df<-read.csv("Dataset.csv")

df<-subset(df, select=c("votes.centered",
			"country", "year_exist",
			"case_id",
			"merged", "dissolved",
			"years.centered", "years.centered.squared",
			"logseats.centered",
		 "government",
			 "EU_parl",
		 "reg_gov",
		 "finance_party_qualify",
		 "efNPP",
	 "elec_coalition", 
	 "indPOL",
	"election_year_dummy",
	"distinctiveness",
	 "insider", 
	 "organisation", 
	 "newfamfirst",
	"OriginNew",
	"post.election.year",	
	"pre.election.year", 
	"seat.product"
	 ))

df<-na.exclude(df)

## DATA PREPARATION
country<-as.numeric(factor(df$country), levels=unique(df$country))
party<-as.numeric(factor(df$case_id), levels=unique(df$case_id))
num.country<-length(unique(country))
num.party<-length(unique(party))
duration<-as.numeric(factor(df$year_exist), levels=unique(df$year_exist))
num.duration<-length(unique(duration))

# 1= survive, 2=merge, 3=dissolve
Y<-cbind(df$merged, df$dissolved)
Y<-cbind(ifelse(rowSums(Y)==0,1,0),Y)
y<-c(sapply(1:nrow(Y), function(i) which(Y[i,]==1)))
n<-length(y)          

Matrix.Country<-matrix(0,n,num.country)
for (i in 1:num.country) Matrix.Country[country==i,i]<-1

Matrix.Party<-matrix(0,n,num.party)
for (i in 1:num.party) Matrix.Party[party==i,i]<-1


Matrix.Outcome<-matrix(0, n, 3)
for (i in 1:3) Matrix.Outcome[y==i,i]<-1


df$OriginNew.newfamfirst<-df$OriginNew*df$newfamfirst

X<-cbind(1,  df$years.centered, df$years.centered.squared,
	 df$insider, I(df$years.centered*df$insider),I(df$years.centered.squared*df$insider),
	 df$organisation, I(df$years.centered*df$organisation), I(df$years.centered.squared*df$organisation),
	 df$newfamfirst, I(df$years.centered*df$newfamfirst),I(df$years.centered.squared*df$newfamfirst),
	 df$OriginNew, I(df$years.centered*df$OriginNew),I(df$years.centered.squared*df$OriginNew),
	 df$OriginNew.newfamfirst, I(df$years.centered*df$OriginNew.newfamfirst),I(df$years.centered.squared*df$OriginNew.newfamfirst),
	 df$votes.centered, 
	 df$logseats.centered,  
	 df$government,
	 df$EU_parl,
	 df$reg_gov,  
	 df$finance_party_qualify,
	 (df$efNPP-mean(df$efNPP))/(2*sd(df$efNPP)),
	 df$elec_coalition, 
	 (df$indPOL-mean(df$indPOL))/(2*sd(df$indPOL)),
	 (df$seat.product-mean(df$seat.product))/(2*sd(df$seat.product)),
	df$election_year_dummy,
	df$post.election.year,	
	df$pre.election.year, 
	df$distinctiveness	 )

	 

# Goodness of fit measures - Quadratic Specification

lp.Quadratic<-c()
p.Quadratic<-c()
l.Quadratic<-c()


for (i in 1:length(unique(df$case_id))) {

X.id<-matrix(X[df$case_id==unique(df$case_id)[i],], ncol=ncol(X))
duration.id<-sort(duration[df$case_id==unique(df$case_id)[i]])
duration.id<-1:length(duration.id)
Y.id<-matrix(Y[df$case_id==unique(df$case_id)[i],],ncol=3)
Reffect.Party2.id<-Reffect.Party2[, which(unique(df$case_id)==unique(df$case_id)[i])]
Reffect.Party3.id<-Reffect.Party3[, which(unique(df$case_id)==unique(df$case_id)[i])]
Reffect.Country2.id<-Reffect.Country2[, unique(country[df$case_id==unique(df$case_id)[i]])]
Reffect.Country3.id<-Reffect.Country3[, unique(country[df$case_id==unique(df$case_id)[i]])]




for (l in 1:length(duration.id)){


p.1<-sapply(1:nrow(Beta2), function(s) 1/(1+exp(X.id[duration.id==l,]%*%matrix(Beta2[s,])+
	Reffect.Party2.id[s]+Reffect.Country2.id[s])+
	exp(X.id[duration.id==l,]%*%matrix(Beta3[s,])+
	Reffect.Party3.id[s]+Reffect.Country3.id[s])))



p.2<-sapply(1:nrow(Beta2), function(s) exp(X.id[duration.id==l,]%*%matrix(Beta2[s,])+
	Reffect.Party2.id[s]+Reffect.Country2.id[s])/(1+exp(X.id[duration.id==l,]%*%matrix(Beta2[s,])+
	Reffect.Party2.id[s]+Reffect.Country2.id[s])+
	exp(X.id[duration.id==l,]%*%matrix(Beta3[s,])+
	Reffect.Party3.id[s]+Reffect.Country3.id[s])))

p.3<-sapply(1:nrow(Beta2), function(s) exp(X.id[duration.id==l,]%*%matrix(Beta3[s,])+
	Reffect.Party3.id[s]+Reffect.Country3.id[s])/(1+exp(X.id[duration.id==l,]%*%matrix(Beta2[s,])+
	Reffect.Party2.id[s]+Reffect.Country2.id[s])+
	exp(X.id[duration.id==l,]%*%matrix(Beta3[s,])+
	Reffect.Party3.id[s]+Reffect.Country3.id[s])))


a<-mean(sapply(1:nrow(Beta2), function(s) ((p.1[s]^Y.id[duration.id==l,1])*(p.2[s]^Y.id[duration.id==l,2])*(p.3[s]^Y.id[duration.id==l,3]))))

b<-mean(log(sapply(1:nrow(Beta2), function(s) ((p.1[s]^Y.id[duration.id==l,1])*(p.2[s]^Y.id[duration.id==l,2])*(p.3[s]^Y.id[duration.id==l,3])))))

l.Quadratic<-c(l.Quadratic, b)

lp.Quadratic<-c(lp.Quadratic, a)
p.Quadratic<-c(p.Quadratic, log(a)-b)





}

}




Table.A.4.Quadratic<-c(-2*sum(l.Quadratic)+2*(2*ncol(X)), -2*sum(l.Quadratic)+log(nrow(Y))*(2*ncol(X)), 
		    -2*sum(l.Quadratic)+(log(nrow(Y))+1)*(2*ncol(X)), 
		     -2*(sum(log(lp.Quadratic))+2*sum(p.Quadratic)))



G.M <- survfit(Surv(year_exist, merged) ~ 1, data=df)
G.D <- survfit(Surv(year_exist, dissolved) ~ 1, data=df)


df$T.D<-NA
df$T.M<-NA
df$Delta.D<-NA
df$Delta.M<-NA


for (i in 1:length(unique(df$case_id))) {

if (any(df$merged[df$case_id==unique(df$case_id)[i]]==1)==FALSE) {
df$T.M[df$case_id==unique(df$case_id)[i]]<-max(df$year_exist[df$case_id==unique(df$case_id)[i]])
df$Delta.M[df$case_id==unique(df$case_id)[i]]<-0
} else if (any(df$merged[df$case_id==unique(df$case_id)[i]]==1)==TRUE) {
df$T.M[df$case_id==unique(df$case_id)[i]]<-df$year_exist[df$case_id==unique(df$case_id)[i] & df$merged==1]
df$Delta.M[df$case_id==unique(df$case_id)[i]]<-1
}

if (any(df$dissolved[df$case_id==unique(df$case_id)[i]]==1)==FALSE) {
df$T.D[df$case_id==unique(df$case_id)[i]]<-max(df$year_exist[df$case_id==unique(df$case_id)[i]])
df$Delta.D[df$case_id==unique(df$case_id)[i]]<-0
} else if (any(df$dissolved[df$case_id==unique(df$case_id)[i]]==1)==TRUE) {
df$T.D[df$case_id==unique(df$case_id)[i]]<-df$year_exist[df$case_id==unique(df$case_id)[i] & df$dissolved==1]
df$Delta.D[df$case_id==unique(df$case_id)[i]]<-1
}

}




AUC.D<-c()
AUC.M<-c()
S<-c()
P.M<-c()
P.D<-c()

for (l in 1:length(unique(duration))) {

T.D.id<-df$T.D[duration==l]
T.M.id<-df$T.M[duration==l]
Delta.D.id<-df$Delta.D[duration==l]
Delta.M.id<-df$Delta.M[duration==l]
X.id<-matrix(X[duration==l,], ncol=ncol(X))
Y.id<-matrix(Y[duration==l,],ncol=3)
Reffect.Party2.id<-Reffect.Party2[, sort(unique(party[duration==l]))]
Reffect.Party3.id<-Reffect.Party3[, sort(unique(party[duration==l]))]
Reffect.Country2.id<-Reffect.Country2[, sort(unique(country[duration==l]))]
Reffect.Country3.id<-Reffect.Country3[, sort(unique(country[duration==l]))]




p.2<-sapply(1:nrow(Beta2), function(s) exp(X.id%*%matrix(Beta2[s,])+
	Reffect.Party2.id[s]+Reffect.Country2.id[s])/(1+exp(X.id%*%matrix(Beta2[s,])+
	Reffect.Party2.id[s]+Reffect.Country2.id[s])+
	exp(X.id%*%matrix(Beta3[s,])+
	Reffect.Party3.id[s]+Reffect.Country3.id[s])))

p.3<-sapply(1:nrow(Beta2), function(s) exp(X.id%*%matrix(Beta3[s,])+
	Reffect.Party3.id[s]+Reffect.Country3.id[s])/(1+exp(X.id%*%matrix(Beta2[s,])+
	Reffect.Party2.id[s]+Reffect.Country2.id[s])+
	exp(X.id%*%matrix(Beta3[s,])+
	Reffect.Party3.id[s]+Reffect.Country3.id[s])))


W.D<-sapply(1:nrow(X.id), function(i) ifelse(Delta.D.id[i]==0, as.numeric(T.D.id[i]>l)/summary(G.D)[[6]][which(summary(G.D)[[2]]==l)],
	as.numeric(T.D.id[i]<=l)/summary(G.D)[[6]][which(summary(G.D)[[2]]==T.D.id[i])]))

W.M<-sapply(1:nrow(X.id), function(i) ifelse(Delta.M.id[i]==0, as.numeric(T.M.id[i]>l)/summary(G.M)[[6]][which(summary(G.M)[[2]]==l)],
	as.numeric(T.M.id[i]<=l)/summary(G.M)[[6]][which(summary(G.M)[[2]]==T.M.id[i])]))


AUC.D.numerator<-sapply(1:nrow(X.id), function(i) sapply((1:nrow(X.id))[-i], function(j) (as.numeric(mean(p.3[i,])>mean(p.3[j,]))+0.5*(as.numeric(mean(p.3[i,])==mean(p.3[j,]))))*
W.D[i]*W.D[j]* Y.id[i,3]*(1-Y.id[j,3])))

AUC.D.denominator<-sapply(1:nrow(X.id), function(i) sapply((1:nrow(X.id))[-i], function(j) W.D[i]*W.D[j]*
Y.id[i,3]*(1-Y.id[j,3])))

AUC.D<-c(AUC.D, sum(rowSums(t(AUC.D.numerator)))/sum(rowSums(t(AUC.D.denominator))))

AUC.M.numerator<-sapply(1:nrow(X.id), function(i) sapply((1:nrow(X.id))[-i], function(j) (as.numeric(mean(p.2[i,])>mean(p.2[j,]))+0.5*(as.numeric(mean(p.2[i,])==mean(p.2[j,]))))*
W.M[i]*W.M[j]*Y.id[i,2]*(1-Y.id[j,2])))


AUC.M.denominator<-sapply(1:nrow(X.id), function(i) sapply((1:nrow(X.id))[-i], function(j) W.M[i]*W.M[j]*
 Y.id[i,2]*(1-Y.id[j,2])))

AUC.M<-c(AUC.M, sum(rowSums(t(AUC.M.numerator)))/sum(rowSums(t(AUC.M.denominator))))


S<-c(S, 1-mean(p.2)-mean(p.3))

P.M<-c(P.M, mean(p.2))
P.D<-c(P.D, mean(p.3))



}


AUC.M<-AUC.M[!is.na(AUC.M)]
P.M<-P.M[!is.na(AUC.M)]
S.M<-S[!is.na(AUC.M)]

AUC.D<-AUC.D[!is.na(AUC.D)]
P.D<-P.D[!is.na(AUC.D)]
S.D<-S[!is.na(AUC.D)]


w.M.numerator<-sapply(1:length(AUC.M), function(t) P.M[t]*prod(S.M[1:(t-1)])*S.M[t] )
w.D.numerator<-sapply(1:length(AUC.D), function(t) P.D[t]*prod(S.D[1:(t-1)])*S.D[t] )


C.M<-sum(sapply(1:length(AUC.M), function(t) AUC.M[t]*w.M.numerator[t]/sum(w.M.numerator)),na.rm=T)
C.D<-sum(sapply(1:length(AUC.D), function(t) AUC.D[t]*w.D.numerator[t]/sum(w.D.numerator)),na.rm=T)




B.D<-c()
B.M<-c()
S<-c()
P.M<-c()
P.D<-c()

for (l in 1:length(unique(duration))) {

T.D.id<-df$T.D[duration==l]
T.M.id<-df$T.M[duration==l]
Delta.D.id<-df$Delta.D[duration==l]
Delta.M.id<-df$Delta.M[duration==l]
X.id<-matrix(X[duration==l,], ncol=ncol(X))
Y.id<-matrix(Y[duration==l,],ncol=3)
Reffect.Party2.id<-Reffect.Party2[, sort(unique(party[duration==l]))]
Reffect.Party3.id<-Reffect.Party3[, sort(unique(party[duration==l]))]
Reffect.Country2.id<-Reffect.Country2[, sort(unique(country[duration==l]))]
Reffect.Country3.id<-Reffect.Country3[, sort(unique(country[duration==l]))]




p.2<-sapply(1:nrow(Beta2), function(s) exp(X.id%*%matrix(Beta2[s,])+
	Reffect.Party2.id[s]+Reffect.Country2.id[s])/(1+exp(X.id%*%matrix(Beta2[s,])+
	Reffect.Party2.id[s]+Reffect.Country2.id[s])+
	exp(X.id%*%matrix(Beta3[s,])+
	Reffect.Party3.id[s]+Reffect.Country3.id[s])))

p.3<-sapply(1:nrow(Beta2), function(s) exp(X.id%*%matrix(Beta3[s,])+
	Reffect.Party3.id[s]+Reffect.Country3.id[s])/(1+exp(X.id%*%matrix(Beta2[s,])+
	Reffect.Party2.id[s]+Reffect.Country2.id[s])+
	exp(X.id%*%matrix(Beta3[s,])+
	Reffect.Party3.id[s]+Reffect.Country3.id[s])))



W.D<-sapply(1:nrow(X.id), function(i) ifelse(Delta.D.id[i]==0, as.numeric(T.D.id[i]>l)/summary(G.D)[[6]][which(summary(G.D)[[2]]==l)],
	as.numeric(T.D.id[i]<=l)/summary(G.D)[[6]][which(summary(G.D)[[2]]==T.D.id[i])]))

W.M<-sapply(1:nrow(X.id), function(i) ifelse(Delta.M.id[i]==0, as.numeric(T.M.id[i]>l)/summary(G.M)[[6]][which(summary(G.M)[[2]]==l)],
	as.numeric(T.M.id[i]<=l)/summary(G.M)[[6]][which(summary(G.M)[[2]]==T.M.id[i])]))



BS.M<-sapply(1:nrow(X.id), function(i) W.M[i]* ((Y.id[i,2]-mean(p.2[i,]))^2))
BS.D<-sapply(1:nrow(X.id), function(i) W.D[i]*((Y.id[i,3]-mean(p.3[i,]))^2))



BS.M<- (1/nrow(X.id)*mean(T.M.id>=l))*sum(BS.M)
BS.D<- (1/nrow(X.id)*mean(T.D.id>=l))*sum(BS.D)


B.M<-c(B.M, BS.M)
B.D<-c(B.D, BS.D)

S<-c(S, 1-mean(p.2)-mean(p.3))

P.M<-c(P.M, mean(p.2))
P.D<-c(P.D, mean(p.3))



}

B.M<-B.M[!is.na(B.M)]
P.M<-P.M[!is.na(B.M)]
S.M<-S[!is.na(B.M)]

B.D<-B.D[!is.na(B.D)]
P.D<-P.D[!is.na(B.D)]
S.D<-S[!is.na(B.D)]


w.M<-sapply(1:length(B.M), function(t) P.M[t]*prod(S.M[1:(t-1)]))
w.D<-sapply(1:length(B.D), function(t) P.D[t]*prod(S.D[1:(t-1)]))


Table.A.4.Quadratic<-c(Table.A.4.Quadratic, 100*(sum(B.M*w.M)+sum(B.D*w.D)),  100*(C.M*sum(Y[,2])+C.D*sum(Y[,3]))/sum(colSums(Y)[-1]))
save(Table.A.4.Quadratic, file="Table.A.4.Quadratic")


# The next bit processes the data that will be used to produce Figures A.8 and A.11
df$years<-(df$year_exist-mean(df$year_exist))/(2*sd(df$year_exist))
df$years.squared<-df$year_exist^2
df$years.squared<-(df$years.squared-mean(df$years.squared))/(2*sd(df$years.squared))

x<-cbind(1,unique(df$years), unique(df$years.squared))


Dynamic.Insider.Merger<-c()
Dynamic.Insider.Dissolution<-c()
Dynamic.Societal.Rootedness.Merger<-c()
Dynamic.Societal.Rootedness.Dissolution<-c()
Dynamic.IdeologicalNovelty.Rooted.Dissolution<-c()
Dynamic.IdeologicalNovelty.Rooted.Merger<-c()
Dynamic.IdeologicalNovelty.NotRooted.Dissolution<-c()
Dynamic.IdeologicalNovelty.NotRooted.Merger<-c()


for (s in 1:nrow(x)){
Dynamic.Insider.Merger<-rbind(Dynamic.Insider.Merger,
c(s,
mean(Beta2[,4:6]%*%matrix(x[s,])),HPDinterval(as.mcmc(Beta2[,4:6]%*%matrix(x[s,])), p=0.95))
)
}

for (s in 1:nrow(x)){
Dynamic.Insider.Dissolution<-rbind(Dynamic.Insider.Dissolution,
c(s,
mean(Beta3[,4:6]%*%matrix(x[s,])),HPDinterval(as.mcmc(Beta3[,4:6]%*%matrix(x[s,])), p=0.95))
)
}


for (s in 1:nrow(x)){
Dynamic.Societal.Rootedness.Merger<-rbind(Dynamic.Societal.Rootedness.Merger,
c(s,
mean(Beta2[,7:9]%*%matrix(x[s,])),HPDinterval(as.mcmc(Beta2[,7:9]%*%matrix(x[s,])), p=0.95))
)
}


for (s in 1:nrow(x)){
Dynamic.Societal.Rootedness.Dissolution<-rbind(Dynamic.Societal.Rootedness.Dissolution,
c(s,
mean(Beta3[,7:9]%*%matrix(x[s,])),HPDinterval(as.mcmc(Beta3[,7:9]%*%matrix(x[s,])), p=0.95))
)
}


for (s in 1:nrow(x)){
Dynamic.IdeologicalNovelty.Rooted.Dissolution<-rbind(Dynamic.IdeologicalNovelty.Rooted.Dissolution,
c(s,
mean(Beta3[,10:12]%*%matrix(x[s,])),HPDinterval(as.mcmc(Beta3[,10:12]%*%matrix(x[s,])), p=0.95))
)
}


for (s in 1:nrow(x)){
Dynamic.IdeologicalNovelty.Rooted.Merger<-rbind(Dynamic.IdeologicalNovelty.Rooted.Merger,
c(s,
mean(Beta2[,10:12]%*%matrix(x[s,])),HPDinterval(as.mcmc(Beta2[,10:12]%*%matrix(x[s,])), p=0.95))
)
}




for (s in 1:nrow(x)){
Dynamic.IdeologicalNovelty.NotRooted.Dissolution<-rbind(Dynamic.IdeologicalNovelty.NotRooted.Dissolution,
c(s,
mean((Beta3[,10:12]+Beta3[,16:18])%*%matrix(x[s,])),HPDinterval(as.mcmc((Beta3[,10:12]+Beta3[,16:18])%*%matrix(x[s,])), p=0.95))
)
}



for (s in 1:nrow(x)){
Dynamic.IdeologicalNovelty.NotRooted.Merger<-rbind(Dynamic.IdeologicalNovelty.NotRooted.Merger,
c(s,
mean((Beta2[,10:12]+Beta2[,16:18])%*%matrix(x[s,])),HPDinterval(as.mcmc((Beta2[,10:12]+Beta2[,16:18])%*%matrix(x[s,])), p=0.95))
)
}




Dynamics.Quadratic<-rbind(
cbind(Dynamic.Insider.Dissolution,"Insider", "Dissolution"),
cbind(Dynamic.Insider.Merger,"Insider", "Merger"),
cbind(Dynamic.Societal.Rootedness.Dissolution,"Societal Rootedness", "Dissolution"),
cbind(Dynamic.Societal.Rootedness.Merger,"Societal Rootedness", "Merger"),
cbind(Dynamic.IdeologicalNovelty.Rooted.Dissolution, "Ideologically Novel & Organizationally Rooted", "Dissolution"),
cbind(Dynamic.IdeologicalNovelty.Rooted.Merger, "Ideologically Novel & Organizationally Rooted", "Merger"),
cbind(Dynamic.IdeologicalNovelty.NotRooted.Dissolution, "Ideologically Novel & NOT Organizationally Rooted", "Dissolution"),
cbind(Dynamic.IdeologicalNovelty.NotRooted.Merger, "Ideologically Novel & NOT Organizationally Rooted", "Merger")
)


Dynamics.Quadratic<-data.frame(Dynamics.Quadratic)
names(Dynamics.Quadratic)<-c("x", "y", "lower", "upper", "Variable", "Death")
save(Dynamics.Quadratic, file="Dynamics_Quadratic")


rm(out, Dynamics.Quadratic, Table.A.4.Quadratic)

# Model specifying the coefficients of the formative features as logarithmic functions of parties’ age
load("Estimates_Weibull") 


# Convergence checks
BETA2<-mcmc.list(as.mcmc(t(out$beta2[,,1])), as.mcmc(t(out$beta2[,,2])),
	 as.mcmc(t(out$beta2[,,3])))


BETA3<-mcmc.list(as.mcmc(t(out$beta3[,,1])), as.mcmc(t(out$beta3[,,2])),
	 as.mcmc(t(out$beta3[,,3])))

print(paste("Convergence, Parameter Estimates of the Logarithmic (Weibull) Specification:", gelman.diag(BETA2)[[2]]<1.2 & gelman.diag(BETA3)[[2]]<1.2, sep=" "))


# Pooled convergent draws - to be used for inference
Beta2<-rbind(t(out$beta2[, pool:20000, 1]),
		t(out$beta2[, pool:20000, 2]),
		t(out$beta2[,pool:20000 , 3]))


Beta3<-rbind(t(out$beta3[,pool:20000 , 1]),
		t(out$beta3[, pool:20000, 2]),
		t(out$beta3[,pool:20000 , 3]))

Reffect.Party2<-rbind(t(out$reffect_party[1:204, pool:20000, 1]),
		t(out$reffect_party[1:204, pool:20000, 2]),
		t(out$reffect_party[1:204,pool:20000 , 3]))
Reffect.Party3<-rbind(t(out$reffect_party[205:408,pool:20000 , 1]),
		t(out$reffect_party[205:408,pool:20000 , 2]),
		t(out$reffect_party[205:408,pool:20000 , 3]))
Reffect.Country2<-rbind(t(out$reffect_country[1:22,pool:20000 , 1]),
		t(out$reffect_country[1:22, pool:20000, 2]),
		t(out$reffect_country[1:22, pool:20000, 3]))
Reffect.Country3<-rbind(t(out$reffect_country[23:44,pool:20000 , 1]),
		t(out$reffect_country[23:44, pool:20000, 2]),
		t(out$reffect_country[23:44, pool:20000, 3]))



## Load data 
df<-read.csv("Dataset.csv")

df<-subset(df, select=c("votes.centered",
			"country", "year_exist",
			"case_id",
			"merged", "dissolved",
			"log.partyyears",
			"logseats.centered",
		 "government",
			 "EU_parl",
		 "reg_gov",
		 "finance_party_qualify",
		 "efNPP",
	 "elec_coalition", 
	 "indPOL",
	"election_year_dummy",
	"distinctiveness",
	 "insider", 
	 "organisation", 
	 "newfamfirst",
	"OriginNew",
	"post.election.year",	
	"pre.election.year", 
	"seat.product"
	 ))

df<-na.exclude(df)



## DATA PREPARATION
country<-as.numeric(factor(df$country), levels=unique(df$country))
party<-as.numeric(factor(df$case_id), levels=unique(df$case_id))
num.country<-length(unique(country))
num.party<-length(unique(party))


# 1= survive, 2=merge, 3=dissolve
Y<-cbind(df$merged, df$dissolved)
Y<-cbind(ifelse(rowSums(Y)==0,1,0),Y)
y<-c(sapply(1:nrow(Y), function(i) which(Y[i,]==1)))
n<-length(y)         

Matrix.Country<-matrix(0,n,num.country)
for (i in 1:num.country) Matrix.Country[country==i,i]<-1

Matrix.Party<-matrix(0,n,num.party)
for (i in 1:num.party) Matrix.Party[party==i,i]<-1


Matrix.Outcome<-matrix(0, n, 3)
for (i in 1:3) Matrix.Outcome[y==i,i]<-1


df$OriginNew.newfamfirst<-df$OriginNew*df$newfamfirst


X<-cbind(1,  df$log.partyyears, 
	 df$insider, I(df$insider*df$logpartyyears), 
	 df$organisation, I(df$organisation*df$logpartyyears),
       df$newfamfirst,I(df$newfamfirst*df$logpartyyears),
	 df$OriginNew,I(df$OriginNew*df$logpartyyears),
	df$OriginNew.newfamfirst, I(df$OriginNew.newfamfirst*df$logpartyyears),
	df$votes.centered,
	 df$logseats.centered,  
	 df$government,
	 df$EU_parl,
	 df$reg_gov,  
	 df$finance_party_qualify,
	 (df$efNPP-mean(df$efNPP))/(2*sd(df$efNPP)),
	 df$elec_coalition, 
	 (df$indPOL-mean(df$indPOL))/(2*sd(df$indPOL)),
	 (df$seat.product-mean(df$seat.product))/(2*sd(df$seat.product)),
	df$election_year_dummy,
	df$post.election.year,	
	df$pre.election.year, 
	df$distinctiveness)


duration<-as.numeric(factor(df$year_exist), levels=unique(df$year_exist))
num.duration<-length(unique(duration))

# Goodness of fit measures - Logarithmic Specification

l.Weibull<-c()
lp.Weibull<-c()
p.Weibull<-c()


for (i in 1:length(unique(df$case_id))) {


X.id<-matrix(X[df$case_id==unique(df$case_id)[i],], ncol=ncol(X))
duration.id<-sort(duration[df$case_id==unique(df$case_id)[i]])
duration.id<-1:length(duration.id)
Y.id<-matrix(Y[df$case_id==unique(df$case_id)[i],],ncol=3)
Reffect.Party2.id<-Reffect.Party2[, which(unique(df$case_id)==unique(df$case_id)[i])]
Reffect.Party3.id<-Reffect.Party3[, which(unique(df$case_id)==unique(df$case_id)[i])]
Reffect.Country2.id<-Reffect.Country2[, unique(country[df$case_id==unique(df$case_id)[i]])]
Reffect.Country3.id<-Reffect.Country3[, unique(country[df$case_id==unique(df$case_id)[i]])]




for (l in 1:length(duration.id)){


p.1<-sapply(1:nrow(Beta2), function(s) 1/(1+exp(X.id[duration.id==l,]%*%matrix(Beta2[s,])+
	Reffect.Party2.id[s]+Reffect.Country2.id[s])+
	exp(X.id[duration.id==l,]%*%matrix(Beta3[s,])+
	Reffect.Party3.id[s]+Reffect.Country3.id[s])))



p.2<-sapply(1:nrow(Beta2), function(s) exp(X.id[duration.id==l,]%*%matrix(Beta2[s,])+
	Reffect.Party2.id[s]+Reffect.Country2.id[s])/(1+exp(X.id[duration.id==l,]%*%matrix(Beta2[s,])+
	Reffect.Party2.id[s]+Reffect.Country2.id[s])+
	exp(X.id[duration.id==l,]%*%matrix(Beta3[s,])+
	Reffect.Party3.id[s]+Reffect.Country3.id[s])))

p.3<-sapply(1:nrow(Beta2), function(s) exp(X.id[duration.id==l,]%*%matrix(Beta3[s,])+
	Reffect.Party3.id[s]+Reffect.Country3.id[s])/(1+exp(X.id[duration.id==l,]%*%matrix(Beta2[s,])+
	Reffect.Party2.id[s]+Reffect.Country2.id[s])+
	exp(X.id[duration.id==l,]%*%matrix(Beta3[s,])+
	Reffect.Party3.id[s]+Reffect.Country3.id[s])))



a<-mean(sapply(1:nrow(Beta2), function(s) ((p.1[s]^Y.id[duration.id==l,1])*(p.2[s]^Y.id[duration.id==l,2])*(p.3[s]^Y.id[duration.id==l,3]))))

b<-mean(log(sapply(1:nrow(Beta2), function(s) ((p.1[s]^Y.id[duration.id==l,1])*(p.2[s]^Y.id[duration.id==l,2])*(p.3[s]^Y.id[duration.id==l,3])))))


l.Weibull<-c(l.Weibull, b)


lp.Weibull<-c(lp.Weibull, a)
p.Weibull<-c(p.Weibull, log(a)-b)






}



}




Table.A.4.Logarithmic<-c(-2*sum(l.Weibull)+2*(2*ncol(X)), -2*sum(l.Weibull)+log(nrow(Y))*(2*ncol(X)), 
		    -2*sum(l.Weibull)+(log(nrow(Y))+1)*(2*ncol(X)), 
		     -2*(sum(log(lp.Weibull))+2*sum(p.Weibull)))




G.M <- survfit(Surv(year_exist, merged) ~ 1, data=df)
G.D <- survfit(Surv(year_exist, dissolved) ~ 1, data=df)


df$T.D<-NA
df$T.M<-NA
df$Delta.D<-NA
df$Delta.M<-NA


for (i in 1:length(unique(df$case_id))) {

if (any(df$merged[df$case_id==unique(df$case_id)[i]]==1)==FALSE) {
df$T.M[df$case_id==unique(df$case_id)[i]]<-max(df$year_exist[df$case_id==unique(df$case_id)[i]])
df$Delta.M[df$case_id==unique(df$case_id)[i]]<-0
} else if (any(df$merged[df$case_id==unique(df$case_id)[i]]==1)==TRUE) {
df$T.M[df$case_id==unique(df$case_id)[i]]<-df$year_exist[df$case_id==unique(df$case_id)[i] & df$merged==1]
df$Delta.M[df$case_id==unique(df$case_id)[i]]<-1
}

if (any(df$dissolved[df$case_id==unique(df$case_id)[i]]==1)==FALSE) {
df$T.D[df$case_id==unique(df$case_id)[i]]<-max(df$year_exist[df$case_id==unique(df$case_id)[i]])
df$Delta.D[df$case_id==unique(df$case_id)[i]]<-0
} else if (any(df$dissolved[df$case_id==unique(df$case_id)[i]]==1)==TRUE) {
df$T.D[df$case_id==unique(df$case_id)[i]]<-df$year_exist[df$case_id==unique(df$case_id)[i] & df$dissolved==1]
df$Delta.D[df$case_id==unique(df$case_id)[i]]<-1
}

}






AUC.D<-c()
AUC.M<-c()
S<-c()
P.M<-c()
P.D<-c()

for (l in 1:length(unique(duration))) {

T.D.id<-df$T.D[duration==l]
T.M.id<-df$T.M[duration==l]
Delta.D.id<-df$Delta.D[duration==l]
Delta.M.id<-df$Delta.M[duration==l]
X.id<-matrix(X[duration==l,], ncol=ncol(X))
Y.id<-matrix(Y[duration==l,],ncol=3)
Reffect.Party2.id<-Reffect.Party2[, sort(unique(party[duration==l]))]
Reffect.Party3.id<-Reffect.Party3[, sort(unique(party[duration==l]))]
Reffect.Country2.id<-Reffect.Country2[, sort(unique(country[duration==l]))]
Reffect.Country3.id<-Reffect.Country3[, sort(unique(country[duration==l]))]




p.2<-sapply(1:nrow(Beta2), function(s) exp(X.id%*%matrix(Beta2[s,])+
	Reffect.Party2.id[s]+Reffect.Country2.id[s])/(1+exp(X.id%*%matrix(Beta2[s,])+
	Reffect.Party2.id[s]+Reffect.Country2.id[s])+
	exp(X.id%*%matrix(Beta3[s,])+
	Reffect.Party3.id[s]+Reffect.Country3.id[s])))

p.3<-sapply(1:nrow(Beta2), function(s) exp(X.id%*%matrix(Beta3[s,])+
	Reffect.Party3.id[s]+Reffect.Country3.id[s])/(1+exp(X.id%*%matrix(Beta2[s,])+
	Reffect.Party2.id[s]+Reffect.Country2.id[s])+
	exp(X.id%*%matrix(Beta3[s,])+
	Reffect.Party3.id[s]+Reffect.Country3.id[s])))


W.D<-sapply(1:nrow(X.id), function(i) ifelse(Delta.D.id[i]==0, as.numeric(T.D.id[i]>l)/summary(G.D)[[6]][which(summary(G.D)[[2]]==l)],
	as.numeric(T.D.id[i]<=l)/summary(G.D)[[6]][which(summary(G.D)[[2]]==T.D.id[i])]))

W.M<-sapply(1:nrow(X.id), function(i) ifelse(Delta.M.id[i]==0, as.numeric(T.M.id[i]>l)/summary(G.M)[[6]][which(summary(G.M)[[2]]==l)],
	as.numeric(T.M.id[i]<=l)/summary(G.M)[[6]][which(summary(G.M)[[2]]==T.M.id[i])]))


AUC.D.numerator<-sapply(1:nrow(X.id), function(i) sapply((1:nrow(X.id))[-i], function(j) (as.numeric(mean(p.3[i,])>mean(p.3[j,]))+0.5*(as.numeric(mean(p.3[i,])==mean(p.3[j,]))))*
W.D[i]*W.D[j]* Y.id[i,3]*(1-Y.id[j,3])))

AUC.D.denominator<-sapply(1:nrow(X.id), function(i) sapply((1:nrow(X.id))[-i], function(j) W.D[i]*W.D[j]*
Y.id[i,3]*(1-Y.id[j,3])))

AUC.D<-c(AUC.D, sum(rowSums(t(AUC.D.numerator)))/sum(rowSums(t(AUC.D.denominator))))

AUC.M.numerator<-sapply(1:nrow(X.id), function(i) sapply((1:nrow(X.id))[-i], function(j) (as.numeric(mean(p.2[i,])>mean(p.2[j,]))+0.5*(as.numeric(mean(p.2[i,])==mean(p.2[j,]))))*
W.M[i]*W.M[j]*Y.id[i,2]*(1-Y.id[j,2])))


AUC.M.denominator<-sapply(1:nrow(X.id), function(i) sapply((1:nrow(X.id))[-i], function(j) W.M[i]*W.M[j]*
 Y.id[i,2]*(1-Y.id[j,2])))

AUC.M<-c(AUC.M, sum(rowSums(t(AUC.M.numerator)))/sum(rowSums(t(AUC.M.denominator))))


S<-c(S, 1-mean(p.2)-mean(p.3))

P.M<-c(P.M, mean(p.2))
P.D<-c(P.D, mean(p.3))



}



AUC.M<-AUC.M[!is.na(AUC.M)]
P.M<-P.M[!is.na(AUC.M)]
S.M<-S[!is.na(AUC.M)]

AUC.D<-AUC.D[!is.na(AUC.D)]
P.D<-P.D[!is.na(AUC.D)]
S.D<-S[!is.na(AUC.D)]


w.M.numerator<-sapply(1:length(AUC.M), function(t) P.M[t]*prod(S.M[1:(t-1)])*S.M[t] )
w.D.numerator<-sapply(1:length(AUC.D), function(t) P.D[t]*prod(S.D[1:(t-1)])*S.D[t] )


C.M<-sum(sapply(1:length(AUC.M), function(t) AUC.M[t]*w.M.numerator[t]/sum(w.M.numerator)),na.rm=T)
C.D<-sum(sapply(1:length(AUC.D), function(t) AUC.D[t]*w.D.numerator[t]/sum(w.D.numerator)),na.rm=T)





B.D<-c()
B.M<-c()
S<-c()
P.M<-c()
P.D<-c()

for (l in 1:length(unique(duration))) {

T.D.id<-df$T.D[duration==l]
T.M.id<-df$T.M[duration==l]
Delta.D.id<-df$Delta.D[duration==l]
Delta.M.id<-df$Delta.M[duration==l]
X.id<-matrix(X[duration==l,], ncol=ncol(X))
Y.id<-matrix(Y[duration==l,],ncol=3)
Reffect.Party2.id<-Reffect.Party2[, sort(unique(party[duration==l]))]
Reffect.Party3.id<-Reffect.Party3[, sort(unique(party[duration==l]))]
Reffect.Country2.id<-Reffect.Country2[, sort(unique(country[duration==l]))]
Reffect.Country3.id<-Reffect.Country3[, sort(unique(country[duration==l]))]




p.2<-sapply(1:nrow(Beta2), function(s) exp(X.id%*%matrix(Beta2[s,])+
	Reffect.Party2.id[s]+Reffect.Country2.id[s])/(1+exp(X.id%*%matrix(Beta2[s,])+
	Reffect.Party2.id[s]+Reffect.Country2.id[s])+
	exp(X.id%*%matrix(Beta3[s,])+
	Reffect.Party3.id[s]+Reffect.Country3.id[s])))

p.3<-sapply(1:nrow(Beta2), function(s) exp(X.id%*%matrix(Beta3[s,])+
	Reffect.Party3.id[s]+Reffect.Country3.id[s])/(1+exp(X.id%*%matrix(Beta2[s,])+
	Reffect.Party2.id[s]+Reffect.Country2.id[s])+
	exp(X.id%*%matrix(Beta3[s,])+
	Reffect.Party3.id[s]+Reffect.Country3.id[s])))

W.D<-sapply(1:nrow(X.id), function(i) ifelse(Delta.D.id[i]==0, as.numeric(T.D.id[i]>l)/summary(G.D)[[6]][which(summary(G.D)[[2]]==l)],
	as.numeric(T.D.id[i]<=l)/summary(G.D)[[6]][which(summary(G.D)[[2]]==T.D.id[i])]))

W.M<-sapply(1:nrow(X.id), function(i) ifelse(Delta.M.id[i]==0, as.numeric(T.M.id[i]>l)/summary(G.M)[[6]][which(summary(G.M)[[2]]==l)],
	as.numeric(T.M.id[i]<=l)/summary(G.M)[[6]][which(summary(G.M)[[2]]==T.M.id[i])]))



BS.M<-sapply(1:nrow(X.id), function(i) W.M[i]* ((Y.id[i,2]-mean(p.2[i,]))^2))
BS.D<-sapply(1:nrow(X.id), function(i) W.D[i]*((Y.id[i,3]-mean(p.3[i,]))^2))



BS.M<- (1/nrow(X.id)*mean(T.M.id>=l))*sum(BS.M)
BS.D<- (1/nrow(X.id)*mean(T.D.id>=l))*sum(BS.D)


B.M<-c(B.M, BS.M)
B.D<-c(B.D, BS.D)

S<-c(S, 1-mean(p.2)-mean(p.3))

P.M<-c(P.M, mean(p.2))
P.D<-c(P.D, mean(p.3))



}

B.M<-B.M[!is.na(B.M)]
P.M<-P.M[!is.na(B.M)]
S.M<-S[!is.na(B.M)]

B.D<-B.D[!is.na(B.D)]
P.D<-P.D[!is.na(B.D)]
S.D<-S[!is.na(B.D)]


w.M<-sapply(1:length(B.M), function(t) P.M[t]*prod(S.M[1:(t-1)]))
w.D<-sapply(1:length(B.D), function(t) P.D[t]*prod(S.D[1:(t-1)]))



Table.A.4.Logarithmic<-c(Table.A.4.Logarithmic, 100*(sum(B.M*w.M)+sum(B.D*w.D)),  100*(C.M*sum(Y[,2])+C.D*sum(Y[,3]))/sum(colSums(Y)[-1]))
save(Table.A.4.Logarithmic, file="Table.A.4.Logarithmic")


# The next bit processes the data that will be used to produce Figures A.9 and A.12

x<-cbind(1,1+unique(df$log.partyyears))


Dynamic.Insider.Merger<-c()
Dynamic.Insider.Dissolution<-c()
Dynamic.Societal.Rootedness.Merger<-c()
Dynamic.Societal.Rootedness.Dissolution<-c()
Dynamic.IdeologicalNovelty.Rooted.Dissolution<-c()
Dynamic.IdeologicalNovelty.Rooted.Merger<-c()
Dynamic.IdeologicalNovelty.NotRooted.Dissolution<-c()
Dynamic.IdeologicalNovelty.NotRooted.Merger<-c()



for (s in 1:nrow(x)){
Dynamic.Insider.Merger<-rbind(Dynamic.Insider.Merger,
c(s,
mean(Beta2[,3:4]%*%matrix(x[s,])),HPDinterval(as.mcmc(Beta2[,3:4]%*%matrix(x[s,])), p=0.95))
)
}

for (s in 1:nrow(x)){
Dynamic.Insider.Dissolution<-rbind(Dynamic.Insider.Dissolution,
c(s,
mean(Beta3[,3:4]%*%matrix(x[s,])),HPDinterval(as.mcmc(Beta3[,3:4]%*%matrix(x[s,])), p=0.95))
)
}


for (s in 1:nrow(x)){
Dynamic.Societal.Rootedness.Merger<-rbind(Dynamic.Societal.Rootedness.Merger,
c(s,
mean(Beta2[,5:6]%*%matrix(x[s,])),HPDinterval(as.mcmc(Beta2[,5:6]%*%matrix(x[s,])), p=0.95))
)
}


for (s in 1:nrow(x)){
Dynamic.Societal.Rootedness.Dissolution<-rbind(Dynamic.Societal.Rootedness.Dissolution,
c(s,
mean(Beta3[,5:6]%*%matrix(x[s,])),HPDinterval(as.mcmc(Beta3[,5:6]%*%matrix(x[s,])), p=0.95))
)
}


for (s in 1:nrow(x)){
Dynamic.IdeologicalNovelty.Rooted.Dissolution<-rbind(Dynamic.IdeologicalNovelty.Rooted.Dissolution,
c(s,
mean(Beta3[,7:8]%*%matrix(x[s,])),HPDinterval(as.mcmc(Beta3[,7:8]%*%matrix(x[s,])), p=0.95))
)
}


for (s in 1:nrow(x)){
Dynamic.IdeologicalNovelty.Rooted.Merger<-rbind(Dynamic.IdeologicalNovelty.Rooted.Merger,
c(s,
mean(Beta2[,7:8]%*%matrix(x[s,])),HPDinterval(as.mcmc(Beta2[,7:8]%*%matrix(x[s,])), p=0.95))
)
}




for (s in 1:nrow(x)){
Dynamic.IdeologicalNovelty.NotRooted.Dissolution<-rbind(Dynamic.IdeologicalNovelty.NotRooted.Dissolution,
c(s,
mean((Beta3[,7:8]+Beta3[,11:12])%*%matrix(x[s,])),HPDinterval(as.mcmc((Beta3[,7:8]+Beta3[,11:12])%*%matrix(x[s,])), p=0.95))
)
}



for (s in 1:nrow(x)){
Dynamic.IdeologicalNovelty.NotRooted.Merger<-rbind(Dynamic.IdeologicalNovelty.NotRooted.Merger,
c(s,
mean((Beta2[,7:8]+Beta2[,11:12])%*%matrix(x[s,])),HPDinterval(as.mcmc((Beta2[,7:8]+Beta2[,11:12])%*%matrix(x[s,])), p=0.95))
)
}



Dynamics.Logarithmic<-rbind(
cbind(Dynamic.Insider.Dissolution,"Insider", "Dissolution"),
cbind(Dynamic.Insider.Merger,"Insider", "Merger"),
cbind(Dynamic.Societal.Rootedness.Dissolution,"Societal Rootedness", "Dissolution"),
cbind(Dynamic.Societal.Rootedness.Merger,"Societal Rootedness", "Merger"),
cbind(Dynamic.IdeologicalNovelty.Rooted.Dissolution, "Ideologically Novel & Organizationally Rooted", "Dissolution"),
cbind(Dynamic.IdeologicalNovelty.Rooted.Merger, "Ideologically Novel & Organizationally Rooted", "Merger"),
cbind(Dynamic.IdeologicalNovelty.NotRooted.Dissolution, "Ideologically Novel & NOT Organizationally Rooted", "Dissolution"),
cbind(Dynamic.IdeologicalNovelty.NotRooted.Merger, "Ideologically Novel & NOT Organizationally Rooted", "Merger")
)

Dynamics.Logarithmic<-data.frame(Dynamics.Logarithmic)
names(Dynamics.Logarithmic)<-c("x", "y", "lower", "upper", "Variable", "Death")
save(Dynamics.Logarithmic, file="Dynamics_Logarithmic")


rm(out, Dynamics.Logarithmic, Table.A.4.Logarithmic)

# Model specifying the coefficients of the formative features as linear functions of parties’ age
load("Estimates_Linear") 


# Convergence checks
BETA2<-mcmc.list(as.mcmc(t(out$beta2[,,1])), as.mcmc(t(out$beta2[,,2])),
	 as.mcmc(t(out$beta2[,,3])))


BETA3<-mcmc.list(as.mcmc(t(out$beta3[,,1])), as.mcmc(t(out$beta3[,,2])),
	 as.mcmc(t(out$beta3[,,3])))

print(paste("Convergence, Parameter Estimates of the Linear Specification:", gelman.diag(BETA2)[[2]]<1.2 & gelman.diag(BETA3)[[2]]<1.2, sep=" "))


# Pooled convergent draws - to be used for inference
Beta2<-rbind(t(out$beta2[, pool:20000, 1]),
		t(out$beta2[, pool:20000, 2]),
		t(out$beta2[,pool:20000 , 3]))


Beta3<-rbind(t(out$beta3[, pool:20000, 1]),
		t(out$beta3[, pool:20000, 2]),
		t(out$beta3[, pool:20000, 3]))

Reffect.Party2<-rbind(t(out$reffect_party[1:204, pool:20000, 1]),
		t(out$reffect_party[1:204, pool:20000, 2]),
		t(out$reffect_party[1:204,pool:20000 , 3]))
Reffect.Party3<-rbind(t(out$reffect_party[205:408,pool:20000 , 1]),
		t(out$reffect_party[205:408,pool:20000 , 2]),
		t(out$reffect_party[205:408,pool:20000 , 3]))
Reffect.Country2<-rbind(t(out$reffect_country[1:22,pool:20000 , 1]),
		t(out$reffect_country[1:22, pool:20000, 2]),
		t(out$reffect_country[1:22, pool:20000, 3]))
Reffect.Country3<-rbind(t(out$reffect_country[23:44, pool:20000, 1]),
		t(out$reffect_country[23:44, pool:20000, 2]),
		t(out$reffect_country[23:44,pool:20000 , 3]))



## Load data 
df<-read.csv("Dataset.csv")

df<-subset(df, select=c("votes.centered",
			"country", "year_exist",
			"case_id",
			"merged", "dissolved",
			"year_exist", "log.partyyears",
			"logseats.centered",
		 "government",
			 "EU_parl",
		 "reg_gov",
		 "finance_party_qualify",
		 "efNPP",
	 "elec_coalition", 
	 "indPOL",
	"election_year_dummy",
	"distinctiveness",
	 "insider", 
	 "organisation", 
	 "newfamfirst",
	"OriginNew",
	"post.election.year",	
	"pre.election.year", 
	"seat.product"
	 ))

df<-na.exclude(df)


## DATA PREPARATION
country<-as.numeric(factor(df$country), levels=unique(df$country))
party<-as.numeric(factor(df$case_id), levels=unique(df$case_id))
num.country<-length(unique(country))
num.party<-length(unique(party))


# 1= survive, 2=merge, 3=dissolve
Y<-cbind(df$merged, df$dissolved)
Y<-cbind(ifelse(rowSums(Y)==0,1,0),Y)
y<-c(sapply(1:nrow(Y), function(i) which(Y[i,]==1)))
n<-length(y)          # Number of obs
K<-3			    # Number of outcomes


Matrix.Country<-matrix(0,n,num.country)
for (i in 1:num.country) Matrix.Country[country==i,i]<-1

Matrix.Party<-matrix(0,n,num.party)
for (i in 1:num.party) Matrix.Party[party==i,i]<-1


Matrix.Outcome<-matrix(0, n, 3)
for (i in 1:3) Matrix.Outcome[y==i,i]<-1

df$OriginNew.newfamfirst<-df$OriginNew*df$newfamfirst

df$trend<-df$year_exist-1

X<-cbind(1,  df$log.partyyears, 
	 df$insider, I(df$insider*df$trend), 
	 df$organisation, I(df$organisation*df$trend),
       df$newfamfirst,I(df$newfamfirst*df$trend),
	 df$OriginNew,I(df$OriginNew*df$trend),
	df$OriginNew.newfamfirst, I(df$OriginNew.newfamfirst*df$trend),
	df$votes.centered, 
	 df$logseats.centered,  
	 df$government,
	 df$EU_parl,
	 df$reg_gov, 
	 df$finance_party_qualify,
	 (df$efNPP-mean(df$efNPP))/(2*sd(df$efNPP)),
	 df$elec_coalition, 
	 (df$indPOL-mean(df$indPOL))/(2*sd(df$indPOL)),
	 (df$seat.product-mean(df$seat.product))/(2*sd(df$seat.product)),
	df$election_year_dummy,
	df$post.election.year,	
	df$pre.election.year, 
	df$distinctiveness
	 )

duration<-as.numeric(factor(df$year_exist), levels=unique(df$year_exist))
num.duration<-length(unique(duration))

# Goodness of fit measures - Linear Specification

l.Linear<-c()
lp.Linear<-c()
p.Linear<-c()


for (i in 1:length(unique(df$case_id))) {

X.id<-matrix(X[df$case_id==unique(df$case_id)[i],], ncol=ncol(X))
duration.id<-sort(duration[df$case_id==unique(df$case_id)[i]])
duration.id<-1:length(duration.id)
Y.id<-matrix(Y[df$case_id==unique(df$case_id)[i],],ncol=3)
Reffect.Party2.id<-Reffect.Party2[, which(unique(df$case_id)==unique(df$case_id)[i])]
Reffect.Party3.id<-Reffect.Party3[, which(unique(df$case_id)==unique(df$case_id)[i])]
Reffect.Country2.id<-Reffect.Country2[, unique(country[df$case_id==unique(df$case_id)[i]])]
Reffect.Country3.id<-Reffect.Country3[, unique(country[df$case_id==unique(df$case_id)[i]])]




for (l in 1:length(duration.id)){


p.1<-sapply(1:nrow(Beta2), function(s) 1/(1+exp(X.id[duration.id==l,]%*%matrix(Beta2[s,])+
	Reffect.Party2.id[s]+Reffect.Country2.id[s])+
	exp(X.id[duration.id==l,]%*%matrix(Beta3[s,])+
	Reffect.Party3.id[s]+Reffect.Country3.id[s])))



p.2<-sapply(1:nrow(Beta2), function(s) exp(X.id[duration.id==l,]%*%matrix(Beta2[s,])+
	Reffect.Party2.id[s]+Reffect.Country2.id[s])/(1+exp(X.id[duration.id==l,]%*%matrix(Beta2[s,])+
	Reffect.Party2.id[s]+Reffect.Country2.id[s])+
	exp(X.id[duration.id==l,]%*%matrix(Beta3[s,])+
	Reffect.Party3.id[s]+Reffect.Country3.id[s])))

p.3<-sapply(1:nrow(Beta2), function(s) exp(X.id[duration.id==l,]%*%matrix(Beta3[s,])+
	Reffect.Party3.id[s]+Reffect.Country3.id[s])/(1+exp(X.id[duration.id==l,]%*%matrix(Beta2[s,])+
	Reffect.Party2.id[s]+Reffect.Country2.id[s])+
	exp(X.id[duration.id==l,]%*%matrix(Beta3[s,])+
	Reffect.Party3.id[s]+Reffect.Country3.id[s])))


a<-mean(sapply(1:nrow(Beta2), function(s) ((p.1[s]^Y.id[duration.id==l,1])*(p.2[s]^Y.id[duration.id==l,2])*(p.3[s]^Y.id[duration.id==l,3]))))

b<-mean(log(sapply(1:nrow(Beta2), function(s) ((p.1[s]^Y.id[duration.id==l,1])*(p.2[s]^Y.id[duration.id==l,2])*(p.3[s]^Y.id[duration.id==l,3])))))



l.Linear<-c(l.Linear, b)




lp.Linear<-c(lp.Linear, a)
p.Linear<-c(p.Linear, log(a)-b)





}

}


Table.A.4.Linear<-c(-2*sum(l.Linear)+2*(2*ncol(X)), -2*sum(l.Linear)+log(nrow(Y))*(2*ncol(X)), 
		    -2*sum(l.Linear)+(log(nrow(Y))+1)*(2*ncol(X)), 
		     -2*(sum(log(lp.Linear))+2*sum(p.Linear)))




G.M <- survfit(Surv(year_exist, merged) ~ 1, data=df)
G.D <- survfit(Surv(year_exist, dissolved) ~ 1, data=df)


df$T.D<-NA
df$T.M<-NA
df$Delta.D<-NA
df$Delta.M<-NA


for (i in 1:length(unique(df$case_id))) {

if (any(df$merged[df$case_id==unique(df$case_id)[i]]==1)==FALSE) {
df$T.M[df$case_id==unique(df$case_id)[i]]<-max(df$year_exist[df$case_id==unique(df$case_id)[i]])
df$Delta.M[df$case_id==unique(df$case_id)[i]]<-0
} else if (any(df$merged[df$case_id==unique(df$case_id)[i]]==1)==TRUE) {
df$T.M[df$case_id==unique(df$case_id)[i]]<-df$year_exist[df$case_id==unique(df$case_id)[i] & df$merged==1]
df$Delta.M[df$case_id==unique(df$case_id)[i]]<-1
}

if (any(df$dissolved[df$case_id==unique(df$case_id)[i]]==1)==FALSE) {
df$T.D[df$case_id==unique(df$case_id)[i]]<-max(df$year_exist[df$case_id==unique(df$case_id)[i]])
df$Delta.D[df$case_id==unique(df$case_id)[i]]<-0
} else if (any(df$dissolved[df$case_id==unique(df$case_id)[i]]==1)==TRUE) {
df$T.D[df$case_id==unique(df$case_id)[i]]<-df$year_exist[df$case_id==unique(df$case_id)[i] & df$dissolved==1]
df$Delta.D[df$case_id==unique(df$case_id)[i]]<-1
}

}




AUC.D<-c()
AUC.M<-c()
S<-c()
P.M<-c()
P.D<-c()

for (l in 1:length(unique(duration))) {

T.D.id<-df$T.D[duration==l]
T.M.id<-df$T.M[duration==l]
Delta.D.id<-df$Delta.D[duration==l]
Delta.M.id<-df$Delta.M[duration==l]
X.id<-matrix(X[duration==l,], ncol=ncol(X))
Y.id<-matrix(Y[duration==l,],ncol=3)
Reffect.Party2.id<-Reffect.Party2[, sort(unique(party[duration==l]))]
Reffect.Party3.id<-Reffect.Party3[, sort(unique(party[duration==l]))]
Reffect.Country2.id<-Reffect.Country2[, sort(unique(country[duration==l]))]
Reffect.Country3.id<-Reffect.Country3[, sort(unique(country[duration==l]))]




p.2<-sapply(1:nrow(Beta2), function(s) exp(X.id%*%matrix(Beta2[s,])+
	Reffect.Party2.id[s]+Reffect.Country2.id[s])/(1+exp(X.id%*%matrix(Beta2[s,])+
	Reffect.Party2.id[s]+Reffect.Country2.id[s])+
	exp(X.id%*%matrix(Beta3[s,])+
	Reffect.Party3.id[s]+Reffect.Country3.id[s])))

p.3<-sapply(1:nrow(Beta2), function(s) exp(X.id%*%matrix(Beta3[s,])+
	Reffect.Party3.id[s]+Reffect.Country3.id[s])/(1+exp(X.id%*%matrix(Beta2[s,])+
	Reffect.Party2.id[s]+Reffect.Country2.id[s])+
	exp(X.id%*%matrix(Beta3[s,])+
	Reffect.Party3.id[s]+Reffect.Country3.id[s])))



W.D<-sapply(1:nrow(X.id), function(i) ifelse(Delta.D.id[i]==0, as.numeric(T.D.id[i]>l)/summary(G.D)[[6]][which(summary(G.D)[[2]]==l)],
	as.numeric(T.D.id[i]<=l)/summary(G.D)[[6]][which(summary(G.D)[[2]]==T.D.id[i])]))

W.M<-sapply(1:nrow(X.id), function(i) ifelse(Delta.M.id[i]==0, as.numeric(T.M.id[i]>l)/summary(G.M)[[6]][which(summary(G.M)[[2]]==l)],
	as.numeric(T.M.id[i]<=l)/summary(G.M)[[6]][which(summary(G.M)[[2]]==T.M.id[i])]))


AUC.D.numerator<-sapply(1:nrow(X.id), function(i) sapply((1:nrow(X.id))[-i], function(j) (as.numeric(mean(p.3[i,])>mean(p.3[j,]))+0.5*(as.numeric(mean(p.3[i,])==mean(p.3[j,]))))*
W.D[i]*W.D[j]* Y.id[i,3]*(1-Y.id[j,3])))

AUC.D.denominator<-sapply(1:nrow(X.id), function(i) sapply((1:nrow(X.id))[-i], function(j) W.D[i]*W.D[j]*
Y.id[i,3]*(1-Y.id[j,3])))

AUC.D<-c(AUC.D, sum(rowSums(t(AUC.D.numerator)))/sum(rowSums(t(AUC.D.denominator))))

AUC.M.numerator<-sapply(1:nrow(X.id), function(i) sapply((1:nrow(X.id))[-i], function(j) (as.numeric(mean(p.2[i,])>mean(p.2[j,]))+0.5*(as.numeric(mean(p.2[i,])==mean(p.2[j,]))))*
W.M[i]*W.M[j]*Y.id[i,2]*(1-Y.id[j,2])))


AUC.M.denominator<-sapply(1:nrow(X.id), function(i) sapply((1:nrow(X.id))[-i], function(j) W.M[i]*W.M[j]*
 Y.id[i,2]*(1-Y.id[j,2])))

AUC.M<-c(AUC.M, sum(rowSums(t(AUC.M.numerator)))/sum(rowSums(t(AUC.M.denominator))))


S<-c(S, 1-mean(p.2)-mean(p.3))

P.M<-c(P.M, mean(p.2))
P.D<-c(P.D, mean(p.3))



}


AUC.M<-AUC.M[!is.na(AUC.M)]
P.M<-P.M[!is.na(AUC.M)]
S.M<-S[!is.na(AUC.M)]

AUC.D<-AUC.D[!is.na(AUC.D)]
P.D<-P.D[!is.na(AUC.D)]
S.D<-S[!is.na(AUC.D)]


w.M.numerator<-sapply(1:length(AUC.M), function(t) P.M[t]*prod(S.M[1:(t-1)])*S.M[t] )
w.D.numerator<-sapply(1:length(AUC.D), function(t) P.D[t]*prod(S.D[1:(t-1)])*S.D[t] )


C.M<-sum(sapply(1:length(AUC.M), function(t) AUC.M[t]*w.M.numerator[t]/sum(w.M.numerator)),na.rm=T)
C.D<-sum(sapply(1:length(AUC.D), function(t) AUC.D[t]*w.D.numerator[t]/sum(w.D.numerator)),na.rm=T)




B.D<-c()
B.M<-c()
S<-c()
P.M<-c()
P.D<-c()

for (l in 1:length(unique(duration))) {

T.D.id<-df$T.D[duration==l]
T.M.id<-df$T.M[duration==l]
Delta.D.id<-df$Delta.D[duration==l]
Delta.M.id<-df$Delta.M[duration==l]
X.id<-matrix(X[duration==l,], ncol=ncol(X))
Y.id<-matrix(Y[duration==l,],ncol=3)
Reffect.Party2.id<-Reffect.Party2[, sort(unique(party[duration==l]))]
Reffect.Party3.id<-Reffect.Party3[, sort(unique(party[duration==l]))]
Reffect.Country2.id<-Reffect.Country2[, sort(unique(country[duration==l]))]
Reffect.Country3.id<-Reffect.Country3[, sort(unique(country[duration==l]))]




p.2<-sapply(1:nrow(Beta2), function(s) exp(X.id%*%matrix(Beta2[s,])+
	Reffect.Party2.id[s]+Reffect.Country2.id[s])/(1+exp(X.id%*%matrix(Beta2[s,])+
	Reffect.Party2.id[s]+Reffect.Country2.id[s])+
	exp(X.id%*%matrix(Beta3[s,])+
	Reffect.Party3.id[s]+Reffect.Country3.id[s])))

p.3<-sapply(1:nrow(Beta2), function(s) exp(X.id%*%matrix(Beta3[s,])+
	Reffect.Party3.id[s]+Reffect.Country3.id[s])/(1+exp(X.id%*%matrix(Beta2[s,])+
	Reffect.Party2.id[s]+Reffect.Country2.id[s])+
	exp(X.id%*%matrix(Beta3[s,])+
	Reffect.Party3.id[s]+Reffect.Country3.id[s])))



W.D<-sapply(1:nrow(X.id), function(i) ifelse(Delta.D.id[i]==0, as.numeric(T.D.id[i]>l)/summary(G.D)[[6]][which(summary(G.D)[[2]]==l)],
	as.numeric(T.D.id[i]<=l)/summary(G.D)[[6]][which(summary(G.D)[[2]]==T.D.id[i])]))

W.M<-sapply(1:nrow(X.id), function(i) ifelse(Delta.M.id[i]==0, as.numeric(T.M.id[i]>l)/summary(G.M)[[6]][which(summary(G.M)[[2]]==l)],
	as.numeric(T.M.id[i]<=l)/summary(G.M)[[6]][which(summary(G.M)[[2]]==T.M.id[i])]))



BS.M<-sapply(1:nrow(X.id), function(i) W.M[i]* ((Y.id[i,2]-mean(p.2[i,]))^2))
BS.D<-sapply(1:nrow(X.id), function(i) W.D[i]*((Y.id[i,3]-mean(p.3[i,]))^2))



BS.M<- (1/nrow(X.id)*mean(T.M.id>=l))*sum(BS.M)
BS.D<- (1/nrow(X.id)*mean(T.D.id>=l))*sum(BS.D)


B.M<-c(B.M, BS.M)
B.D<-c(B.D, BS.D)

S<-c(S, 1-mean(p.2)-mean(p.3))

P.M<-c(P.M, mean(p.2))
P.D<-c(P.D, mean(p.3))



}

B.M<-B.M[!is.na(B.M)]
P.M<-P.M[!is.na(B.M)]
S.M<-S[!is.na(B.M)]

B.D<-B.D[!is.na(B.D)]
P.D<-P.D[!is.na(B.D)]
S.D<-S[!is.na(B.D)]


w.M<-sapply(1:length(B.M), function(t) P.M[t]*prod(S.M[1:(t-1)]))
w.D<-sapply(1:length(B.D), function(t) P.D[t]*prod(S.D[1:(t-1)]))


Table.A.4.Linear<-c(Table.A.4.Linear, 100*(sum(B.M*w.M)+sum(B.D*w.D)),  100*(C.M*sum(Y[,2])+C.D*sum(Y[,3]))/sum(colSums(Y)[-1]))
save(Table.A.4.Linear, file="Table.A.4.Linear")


# The next bit processes the data that will be used to produce Figures A.10 and A.13

x<-cbind(1, unique(df$trend))


Dynamics.Insider.Merger<-c()
Dynamics.Insider.Dissolution<-c()
Dynamics.Societal.Rootedness.Merger<-c()
Dynamics.Societal.Rootedness.Dissolution<-c()
Dynamics.IdeologicalNovelty.Rooted.Dissolution<-c()
Dynamics.IdeologicalNovelty.Rooted.Merger<-c()
Dynamics.IdeologicalNovelty.NotRooted.Dissolution<-c()
Dynamics.IdeologicalNovelty.NotRooted.Merger<-c()


for (s in 1:nrow(x)){
Dynamics.Insider.Merger<-rbind(Dynamics.Insider.Merger,
c(s,
mean(Beta2[,3:4]%*%matrix(x[s,])),HPDinterval(as.mcmc(Beta2[,3:4]%*%matrix(x[s,])), p=0.95))
)
}


for (s in 1:nrow(x)){
Dynamics.Insider.Dissolution<-rbind(Dynamics.Insider.Dissolution,
c(s,
mean(Beta3[,3:4]%*%matrix(x[s,])),HPDinterval(as.mcmc(Beta3[,3:4]%*%matrix(x[s,])), p=0.95))
)
}


for (s in 1:nrow(x)){
Dynamics.Societal.Rootedness.Merger<-rbind(Dynamics.Societal.Rootedness.Merger,
c(s,
mean(Beta2[,5:6]%*%matrix(x[s,])),HPDinterval(as.mcmc(Beta2[,5:6]%*%matrix(x[s,])), p=0.95))
)
}


for (s in 1:nrow(x)){
Dynamics.Societal.Rootedness.Dissolution<-rbind(Dynamics.Societal.Rootedness.Dissolution,
c(s,
mean(Beta3[,5:6]%*%matrix(x[s,])),HPDinterval(as.mcmc(Beta3[,5:6]%*%matrix(x[s,])), p=0.95))
)
}


for (s in 1:nrow(x)){
Dynamics.IdeologicalNovelty.Rooted.Dissolution<-rbind(Dynamics.IdeologicalNovelty.Rooted.Dissolution,
c(s,
mean(Beta3[,7:8]%*%matrix(x[s,])),HPDinterval(as.mcmc(Beta3[,7:8]%*%matrix(x[s,])), p=0.95))
)
}


for (s in 1:nrow(x)){
Dynamics.IdeologicalNovelty.Rooted.Merger<-rbind(Dynamics.IdeologicalNovelty.Rooted.Merger,
c(s,
mean(Beta2[,7:8]%*%matrix(x[s,])),HPDinterval(as.mcmc(Beta2[,7:8]%*%matrix(x[s,])), p=0.95))
)
}




for (s in 1:nrow(x)){
Dynamics.IdeologicalNovelty.NotRooted.Dissolution<-rbind(Dynamics.IdeologicalNovelty.NotRooted.Dissolution,
c(s,
mean((Beta3[,7:8]+Beta3[,11:12])%*%matrix(x[s,])),HPDinterval(as.mcmc((Beta3[,7:8]+Beta3[,11:12])%*%matrix(x[s,])), p=0.95))
)
}



for (s in 1:nrow(x)){
Dynamics.IdeologicalNovelty.NotRooted.Merger<-rbind(Dynamics.IdeologicalNovelty.NotRooted.Merger,
c(s,
mean((Beta2[,7:8]+Beta2[,11:12])%*%matrix(x[s,])),HPDinterval(as.mcmc((Beta2[,7:8]+Beta2[,11:12])%*%matrix(x[s,])), p=0.95))
)
}



Dynamics.Linear<-rbind(
cbind(Dynamics.Insider.Dissolution,"Insider", "Dissolution"),
cbind(Dynamics.Insider.Merger,"Insider", "Merger"),
cbind(Dynamics.Societal.Rootedness.Dissolution,"Societal Rootedness", "Dissolution"),
cbind(Dynamics.Societal.Rootedness.Merger,"Societal Rootedness", "Merger"),
cbind(Dynamics.IdeologicalNovelty.Rooted.Dissolution, "Ideologically Novel & Organizationally Rooted", "Dissolution"),
cbind(Dynamics.IdeologicalNovelty.Rooted.Merger, "Ideologically Novel & Organizationally Rooted", "Merger"),
cbind(Dynamics.IdeologicalNovelty.NotRooted.Dissolution, "Ideologically Novel & NOT Organizationally Rooted", "Dissolution"),
cbind(Dynamics.IdeologicalNovelty.NotRooted.Merger, "Ideologically Novel & NOT Organizationally Rooted", "Merger")
)


Dynamics.Linear<-data.frame(Dynamics.Linear)
names(Dynamics.Linear)<-c("x", "y", "lower", "upper", "Variable", "Death")
save(Dynamics.Linear, file="Dynamics_Linear")

rm(out, Dynamics.Linear, Table.A.4.Linear)

load("Table.3.Statespace")
load("Table.A.4.Quadratic")
load("Table.A.4.Logarithmic")
load("Table.A.4.Linear")


Table.A.4<-matrix(0,nrow=6, ncol=4)
colnames(Table.A.4)<-c("State-space", "Quadratic", "Logarithmic", "Linear")
rownames(Table.A.4)<-c("Akaike Information Criterion (AIC)",
		     "Bayesian Information Criterion (BIC)",
		     "Consistent AIC (CAIC)", 
		     "Watanabe Information Criterion (WAIC)", 
		     "Integrated Prediction Error (IPE X 100)", 
		     "C-index (X 100)")
Table.A.4[,1]<-round(Table.3.Statespace, digits=2)
Table.A.4[,2]<-round(Table.A.4.Quadratic, digits=2)
Table.A.4[,3]<-round(Table.A.4.Logarithmic,digits=2)
Table.A.4[,4]<-round(Table.A.4.Linear,digits=2)

print(Table.A.4)


Table.A.4<-cbind(rownames(Table.A.4), Table.A.4)
rownames(Table.A.4)<-NULL


Titles<-matrix(c("", "", "Models", "", ""),nrow=1)

g1 <- tableGrob(Table.A.4, rows = NULL)
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 2, b = nrow(g1), l = 1, r = ncol(g1))
g1 <- gtable_add_grob(g1,
        grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
        t = 1, l = 1, r = ncol(g1))


g0 <- tableGrob(Titles, rows = NULL)

g<-rbind(g0,g1)


pdf(file = "Table.A.4.pdf", height=5, width=15)
grid.draw(g)
dev.off()

rm(Table.A.4.Quadratic, Table.A.4.Linear, Table.A.4.Logarithmic, Table.3.Statespace)


################################################# Figure A.8 ###########################################################################################


load("Dynamics_Quadratic")

F.A.8<-subset(Dynamics.Quadratic, Variable=="Insider"|Variable=="Societal Rootedness")


F.A.8$Variable<-factor(F.A.8$Variable,
		levels=c("Insider",
			  "Societal Rootedness"),
		labels=c("Insider Status", "Societal Rootedness"))

F.A.8$Death<-factor(F.A.8$Death,
		levels=c("Dissolution",
			  "Merger"),
		labels=c("Dissolution Death", "Merger Death"))


for (k in 1:4) {
F.A.8[,k]<-as.numeric(F.A.8[,k])
}

d1<-subset(F.A.8, Variable=="Insider Status" & 
	Death=="Dissolution Death")

d2<-subset(F.A.8, Variable=="Insider Status" & Death=="Merger Death")

d3<-subset(F.A.8, Variable=="Societal Rootedness" & 
	 Death=="Dissolution Death")

d4<-subset(F.A.8, Variable=="Societal Rootedness" & 
   Death=="Merger Death")




p<-ggplot(F.A.8) + geom_line(aes(y=y, x=x))+ 
  stat_smooth(aes(x = x, y = lower, colour = "grey12"), method = "loess", se = FALSE) +
  stat_smooth(aes(x = x, y = upper, colour = "grey12"), method = "loess", se = FALSE)+
  facet_grid(Variable ~ Death)


g1 <- ggplot(d1) + 
  stat_smooth(aes(x = x, y = lower, colour = "min"), method = "loess", se = FALSE) +
  stat_smooth(aes(x = x, y = upper, colour = "max"), method = "loess", se = FALSE)

gg1 <- ggplot_build(g1)

dd1 <- data.frame(x = gg1$data[[1]]$x,
                  ymin = gg1$data[[1]]$y,
                  ymax = gg1$data[[2]]$y) 



g2 <- ggplot(d2) + 
  stat_smooth(aes(x = x, y = lower, colour = "min"), method = "loess", se = FALSE) +
  stat_smooth(aes(x = x, y = upper, colour = "max"), method = "loess", se = FALSE)

gg2 <- ggplot_build(g2)

dd2 <- data.frame(x = gg2$data[[1]]$x,
                  ymin = gg2$data[[1]]$y,
                  ymax = gg2$data[[2]]$y) 

g3 <- ggplot(d3) + 
  stat_smooth(aes(x = x, y = lower, colour = "min"), method = "loess", se = FALSE) +
  stat_smooth(aes(x = x, y = upper, colour = "max"), method = "loess", se = FALSE)
gg3 <- ggplot_build(g3)

dd3 <- data.frame(x = gg3$data[[1]]$x,
                  ymin = gg3$data[[1]]$y,
                  ymax = gg3$data[[2]]$y) 

g4 <- ggplot(d4) + 
  stat_smooth(aes(x = x, y = lower, colour = "min"), method = "loess", se = FALSE) +
  stat_smooth(aes(x = x, y = upper, colour = "max"), method = "loess", se = FALSE)
gg4 <- ggplot_build(g4)

dd4 <- data.frame(x = gg4$data[[1]]$x,
                  ymin = gg4$data[[1]]$y,
                  ymax = gg4$data[[2]]$y) 




dd1$Variable<-"Insider Status"
dd1$Death<-"Dissolution Death"
dd2$Variable<-"Insider Status"
dd2$Death<-"Merger Death"
dd3$Variable<-"Societal Rootedness"
dd3$Death<-"Dissolution Death"
dd4$Variable<-"Societal Rootedness"
dd4$Death<-"Merger Death"



dd<-rbind(dd1,dd2,dd3,dd4)


p<-p+
  geom_ribbon(data = dd, aes(x = x, ymin = ymin, ymax = ymax),
             alpha = 0.4)+
xlab("Party Age (in years)")+ylab("Log-hazard of party death")+
	facet_grid(Variable ~ Death)+
	geom_hline(yintercept=0, linetype="dashed")+
	theme_bw()+
	 theme(legend.position="none")+
	scale_x_continuous(limits = c(1, 47),
	breaks = c(1,20,40),
	labels=c(0,20,40))+
	theme(strip.text.x = element_text(size=10, face="bold"),
          strip.text.y = element_text(size=10, face="bold"))




pdf(file = "Figure.A.8.pdf")
grid.draw(p)
dev.off()

rm(Dynamics.Quadratic)

################################################# Figure A.9 ###########################################################################################


load("Dynamics_Logarithmic")

F.A.9<-subset(Dynamics.Logarithmic, Variable=="Insider"|Variable=="Societal Rootedness")


F.A.9$Variable<-factor(F.A.9$Variable,
		levels=c("Insider",
			  "Societal Rootedness"),
		labels=c("Insider Status", "Societal Rootedness"))

F.A.9$Death<-factor(F.A.9$Death,
		levels=c("Dissolution",
			  "Merger"),
		labels=c("Dissolution Death", "Merger Death"))


for (k in 1:4) {
F.A.9[,k]<-as.numeric(F.A.9[,k])
}

d1<-subset(F.A.9, Variable=="Insider Status" & 
	Death=="Dissolution Death")

d2<-subset(F.A.9, Variable=="Insider Status" & Death=="Merger Death")

d3<-subset(F.A.9, Variable=="Societal Rootedness" & 
	 Death=="Dissolution Death")

d4<-subset(F.A.9, Variable=="Societal Rootedness" & 
   Death=="Merger Death")




p<-ggplot(F.A.9) + geom_line(aes(y=y, x=x))+ 
  stat_smooth(aes(x = x, y = lower, colour = "grey12"), method = "loess", se = FALSE) +
  stat_smooth(aes(x = x, y = upper, colour = "grey12"), method = "loess", se = FALSE)+
  facet_grid(Variable ~ Death)


g1 <- ggplot(d1) + 
  stat_smooth(aes(x = x, y = lower, colour = "min"), method = "loess", se = FALSE) +
  stat_smooth(aes(x = x, y = upper, colour = "max"), method = "loess", se = FALSE)

gg1 <- ggplot_build(g1)

dd1 <- data.frame(x = gg1$data[[1]]$x,
                  ymin = gg1$data[[1]]$y,
                  ymax = gg1$data[[2]]$y) 



g2 <- ggplot(d2) + 
  stat_smooth(aes(x = x, y = lower, colour = "min"), method = "loess", se = FALSE) +
  stat_smooth(aes(x = x, y = upper, colour = "max"), method = "loess", se = FALSE)

gg2 <- ggplot_build(g2)

dd2 <- data.frame(x = gg2$data[[1]]$x,
                  ymin = gg2$data[[1]]$y,
                  ymax = gg2$data[[2]]$y) 

g3 <- ggplot(d3) + 
  stat_smooth(aes(x = x, y = lower, colour = "min"), method = "loess", se = FALSE) +
  stat_smooth(aes(x = x, y = upper, colour = "max"), method = "loess", se = FALSE)
gg3 <- ggplot_build(g3)

dd3 <- data.frame(x = gg3$data[[1]]$x,
                  ymin = gg3$data[[1]]$y,
                  ymax = gg3$data[[2]]$y) 

g4 <- ggplot(d4) + 
  stat_smooth(aes(x = x, y = lower, colour = "min"), method = "loess", se = FALSE) +
  stat_smooth(aes(x = x, y = upper, colour = "max"), method = "loess", se = FALSE)
gg4 <- ggplot_build(g4)

dd4 <- data.frame(x = gg4$data[[1]]$x,
                  ymin = gg4$data[[1]]$y,
                  ymax = gg4$data[[2]]$y) 




dd1$Variable<-"Insider Status"
dd1$Death<-"Dissolution Death"
dd2$Variable<-"Insider Status"
dd2$Death<-"Merger Death"
dd3$Variable<-"Societal Rootedness"
dd3$Death<-"Dissolution Death"
dd4$Variable<-"Societal Rootedness"
dd4$Death<-"Merger Death"



dd<-rbind(dd1,dd2,dd3,dd4)


p<-p+
  geom_ribbon(data = dd, aes(x = x, ymin = ymin, ymax = ymax),
             alpha = 0.4)+
xlab("Party Age (in years)")+ylab("Log-hazard of party death")+
	facet_grid(Variable ~ Death)+
	geom_hline(yintercept=0, linetype="dashed")+
	theme_bw()+
	 theme(legend.position="none")+
	scale_x_continuous(limits = c(1, 47),
	breaks = c(1,20,40),
	labels=c(0,20,40))+
	theme(strip.text.x = element_text(size=10, face="bold"),
          strip.text.y = element_text(size=10, face="bold"))




pdf(file = "Figure.A.9.pdf")
grid.draw(p)
dev.off()

rm(Dynamics.Logarithmic)

################################################# Figure A.10 ##########################################################################################


load("Dynamics_Linear")


F.A.10<-subset(Dynamics.Linear, Variable=="Insider"|Variable=="Societal Rootedness")


F.A.10$Variable<-factor(F.A.10$Variable,
		levels=c("Insider",
			  "Societal Rootedness"),
		labels=c("Insider Status", "Societal Rootedness"))

F.A.10$Death<-factor(F.A.10$Death,
		levels=c("Dissolution",
			  "Merger"),
		labels=c("Dissolution Death", "Merger Death"))

for (k in 1:4) {
F.A.10[,k]<-as.numeric(F.A.10[,k])
}



d1<-subset(F.A.10, Variable=="Insider Status" & 
	Death=="Dissolution Death")

d2<-subset(F.A.10, Variable=="Insider Status" & Death=="Merger Death")

d3<-subset(F.A.10, Variable=="Societal Rootedness" & 
	 Death=="Dissolution Death")

d4<-subset(F.A.10, Variable=="Societal Rootedness" & 
   Death=="Merger Death")



p<-ggplot(F.A.10) + geom_line(aes(y=y, x=x))+ 
  stat_smooth(aes(x = x, y = lower, colour = "grey12"), method = "loess", se = FALSE) +
  stat_smooth(aes(x = x, y = upper, colour = "grey12"), method = "loess", se = FALSE)+
  facet_grid(Variable ~ Death)



g1 <- ggplot(d1) + 
  stat_smooth(aes(x = x, y = lower, colour = "min"), method = "loess", se = FALSE) +
  stat_smooth(aes(x = x, y = upper, colour = "max"), method = "loess", se = FALSE)

gg1 <- ggplot_build(g1)

dd1 <- data.frame(x = gg1$data[[1]]$x,
                  ymin = gg1$data[[1]]$y,
                  ymax = gg1$data[[2]]$y) 



g2 <- ggplot(d2) + 
  stat_smooth(aes(x = x, y = lower, colour = "min"), method = "loess", se = FALSE) +
  stat_smooth(aes(x = x, y = upper, colour = "max"), method = "loess", se = FALSE)

gg2 <- ggplot_build(g2)

dd2 <- data.frame(x = gg2$data[[1]]$x,
                  ymin = gg2$data[[1]]$y,
                  ymax = gg2$data[[2]]$y) 

g3 <- ggplot(d3) + 
  stat_smooth(aes(x = x, y = lower, colour = "min"), method = "loess", se = FALSE) +
  stat_smooth(aes(x = x, y = upper, colour = "max"), method = "loess", se = FALSE)
gg3 <- ggplot_build(g3)

dd3 <- data.frame(x = gg3$data[[1]]$x,
                  ymin = gg3$data[[1]]$y,
                  ymax = gg3$data[[2]]$y) 

g4 <- ggplot(d4) + 
  stat_smooth(aes(x = x, y = lower, colour = "min"), method = "loess", se = FALSE) +
  stat_smooth(aes(x = x, y = upper, colour = "max"), method = "loess", se = FALSE)
gg4 <- ggplot_build(g4)

dd4 <- data.frame(x = gg4$data[[1]]$x,
                  ymin = gg4$data[[1]]$y,
                  ymax = gg4$data[[2]]$y) 



dd1$Variable<-"Insider Status"
dd1$Death<-"Dissolution Death"
dd2$Variable<-"Insider Status"
dd2$Death<-"Merger Death"
dd3$Variable<-"Societal Rootedness"
dd3$Death<-"Dissolution Death"
dd4$Variable<-"Societal Rootedness"
dd4$Death<-"Merger Death"



dd<-rbind(dd1,dd2,dd3,dd4)


p<-p+
  geom_ribbon(data = dd, aes(x = x, ymin = ymin, ymax = ymax),
              alpha = 0.4)+
xlab("Party Age (in years)")+ylab("Log-hazard of party death")+
	facet_grid(Variable ~ Death)+
	geom_hline(yintercept=0, linetype="dashed")+
	theme_bw()+
	 theme(legend.position="none")+
	scale_x_continuous(limits = c(1, 47),
	breaks = c(1,20,40),
	labels=c(0,20,40))+
	theme(strip.text.x = element_text(size=10, face="bold"),
          strip.text.y = element_text(size=10, face="bold"))


pdf(file = "Figure.A.10.pdf")
grid.draw(p)
dev.off()

rm(Dynamics.Linear)

################################################# Figure A.11 ##########################################################################################


load("Dynamics_Quadratic")

F.A.11<-subset(Dynamics.Quadratic, Variable=="Ideologically Novel & Organizationally Rooted"|
	Variable=="Ideologically Novel & NOT Organizationally Rooted")


F.A.11$Variable<-factor(F.A.11$Variable,
		levels=c("Ideologically Novel & Organizationally Rooted",
			  "Ideologically Novel & NOT Organizationally Rooted"),
		labels=c("Ideologically Novel Parties 
with 
			Roots in Pre-existing Parties", "Ideologically Novel Parties 
without 
			Roots in Pre-existing Parties"))



F.A.11$Death<-factor(F.A.11$Death,
		levels=c("Dissolution",
			  "Merger"),
		labels=c("Dissolution Death", "Merger Death"))


for (k in 1:4) {
F.A.11[,k]<-as.numeric(F.A.11[,k])
}



p<-ggplot(F.A.11) + geom_line(aes(y=y, x=x))+ 
  stat_smooth(aes(x = x, y = lower, colour = "grey12"), method = "loess", se = FALSE) +
  stat_smooth(aes(x = x, y = upper, colour = "grey12"), method = "loess", se = FALSE)+
  facet_grid(Variable ~ Death)


d1<-subset(F.A.11, Variable=="Ideologically Novel Parties 
with 
			Roots in Pre-existing Parties" & Death=="Dissolution Death")

d2<-subset(F.A.11, Variable=="Ideologically Novel Parties 
with 
			Roots in Pre-existing Parties" & Death=="Merger Death")

d3<-subset(F.A.11, Variable=="Ideologically Novel Parties 
without 
			Roots in Pre-existing Parties" &  Death=="Dissolution Death")

d4<-subset(F.A.11, Variable=="Ideologically Novel Parties 
without 
			Roots in Pre-existing Parties" &    Death=="Merger Death")



g1 <- ggplot(d1) + 
  stat_smooth(aes(x = x, y = lower, colour = "min"), method = "loess", se = FALSE) +
  stat_smooth(aes(x = x, y = upper, colour = "max"), method = "loess", se = FALSE)

gg1 <- ggplot_build(g1)

dd1 <- data.frame(x = gg1$data[[1]]$x,
                  ymin = gg1$data[[1]]$y,
                  ymax = gg1$data[[2]]$y) 



g2 <- ggplot(d2) + 
  stat_smooth(aes(x = x, y = lower, colour = "min"), method = "loess", se = FALSE) +
  stat_smooth(aes(x = x, y = upper, colour = "max"), method = "loess", se = FALSE)

gg2 <- ggplot_build(g2)

dd2 <- data.frame(x = gg2$data[[1]]$x,
                  ymin = gg2$data[[1]]$y,
                  ymax = gg2$data[[2]]$y) 

g3 <- ggplot(d3) + 
  stat_smooth(aes(x = x, y = lower, colour = "min"), method = "loess", se = FALSE) +
  stat_smooth(aes(x = x, y = upper, colour = "max"), method = "loess", se = FALSE)
gg3 <- ggplot_build(g3)

dd3 <- data.frame(x = gg3$data[[1]]$x,
                  ymin = gg3$data[[1]]$y,
                  ymax = gg3$data[[2]]$y) 

g4 <- ggplot(d4) + 
  stat_smooth(aes(x = x, y = lower, colour = "min"), method = "loess", se = FALSE) +
  stat_smooth(aes(x = x, y = upper, colour = "max"), method = "loess", se = FALSE)
gg4 <- ggplot_build(g4)

dd4 <- data.frame(x = gg4$data[[1]]$x,
                  ymin = gg4$data[[1]]$y,
                  ymax = gg4$data[[2]]$y) 




dd1$Variable<-"Ideologically Novel Parties 
with 
			Roots in Pre-existing Parties"
dd1$Death<-"Dissolution Death"
dd2$Variable<-"Ideologically Novel Parties 
with 
			Roots in Pre-existing Parties"
dd2$Death<-"Merger Death"
dd3$Variable<-"Ideologically Novel Parties 
without 
			Roots in Pre-existing Parties"
dd3$Death<-"Dissolution Death"
dd4$Variable<-"Ideologically Novel Parties 
without 
			Roots in Pre-existing Parties"
dd4$Death<-"Merger Death"



dd<-rbind(dd1,dd2,dd3,dd4)

dd<-as.data.frame(dd)

p<-p+
  geom_ribbon(data = dd, aes(x = x, ymin = ymin, ymax = ymax),
              alpha = 0.4)+
xlab("Party Age (in years)")+ylab("Log-hazard of party death")+
	facet_grid(Variable ~ Death)+
	geom_hline(yintercept=0, linetype="dashed")+
	theme_bw()+
	 theme(legend.position="none")+
	theme(strip.text.x = element_text(size=10, face="bold"),
          strip.text.y = element_text(size=10, face="bold"))

pdf(file = "Figure.A.11.pdf")
grid.draw(p)
dev.off()

rm(Dynamics.Quadratic)

################################################## Figure A.12 #####################################################################################

load("Dynamics_Logarithmic")

F.A.12<-subset(Dynamics.Logarithmic, Variable=="Ideologically Novel & Organizationally Rooted"|
	Variable=="Ideologically Novel & NOT Organizationally Rooted")


F.A.12$Variable<-factor(F.A.12$Variable,
		levels=c("Ideologically Novel & Organizationally Rooted",
			  "Ideologically Novel & NOT Organizationally Rooted"),
		labels=c("Ideologically Novel Parties 
with 
			Roots in Pre-existing Parties", "Ideologically Novel Parties 
without 
			Roots in Pre-existing Parties"))



F.A.12$Death<-factor(F.A.12$Death,
		levels=c("Dissolution",
			  "Merger"),
		labels=c("Dissolution Death", "Merger Death"))


for (k in 1:4) {
F.A.12[,k]<-as.numeric(F.A.12[,k])
}



p<-ggplot(F.A.12) + geom_line(aes(y=y, x=x))+ 
  stat_smooth(aes(x = x, y = lower, colour = "grey12"), method = "loess", se = FALSE) +
  stat_smooth(aes(x = x, y = upper, colour = "grey12"), method = "loess", se = FALSE)+
  facet_grid(Variable ~ Death)


d1<-subset(F.A.12, Variable=="Ideologically Novel Parties 
with 
			Roots in Pre-existing Parties" & Death=="Dissolution Death")

d2<-subset(F.A.12, Variable=="Ideologically Novel Parties 
with 
			Roots in Pre-existing Parties" & Death=="Merger Death")

d3<-subset(F.A.12, Variable=="Ideologically Novel Parties 
without 
			Roots in Pre-existing Parties" &  Death=="Dissolution Death")

d4<-subset(F.A.12, Variable=="Ideologically Novel Parties 
without 
			Roots in Pre-existing Parties" &    Death=="Merger Death")



g1 <- ggplot(d1) + 
  stat_smooth(aes(x = x, y = lower, colour = "min"), method = "loess", se = FALSE) +
  stat_smooth(aes(x = x, y = upper, colour = "max"), method = "loess", se = FALSE)

gg1 <- ggplot_build(g1)

dd1 <- data.frame(x = gg1$data[[1]]$x,
                  ymin = gg1$data[[1]]$y,
                  ymax = gg1$data[[2]]$y) 



g2 <- ggplot(d2) + 
  stat_smooth(aes(x = x, y = lower, colour = "min"), method = "loess", se = FALSE) +
  stat_smooth(aes(x = x, y = upper, colour = "max"), method = "loess", se = FALSE)

gg2 <- ggplot_build(g2)

dd2 <- data.frame(x = gg2$data[[1]]$x,
                  ymin = gg2$data[[1]]$y,
                  ymax = gg2$data[[2]]$y) 

g3 <- ggplot(d3) + 
  stat_smooth(aes(x = x, y = lower, colour = "min"), method = "loess", se = FALSE) +
  stat_smooth(aes(x = x, y = upper, colour = "max"), method = "loess", se = FALSE)
gg3 <- ggplot_build(g3)

dd3 <- data.frame(x = gg3$data[[1]]$x,
                  ymin = gg3$data[[1]]$y,
                  ymax = gg3$data[[2]]$y) 

g4 <- ggplot(d4) + 
  stat_smooth(aes(x = x, y = lower, colour = "min"), method = "loess", se = FALSE) +
  stat_smooth(aes(x = x, y = upper, colour = "max"), method = "loess", se = FALSE)
gg4 <- ggplot_build(g4)

dd4 <- data.frame(x = gg4$data[[1]]$x,
                  ymin = gg4$data[[1]]$y,
                  ymax = gg4$data[[2]]$y) 




dd1$Variable<-"Ideologically Novel Parties 
with 
			Roots in Pre-existing Parties"
dd1$Death<-"Dissolution Death"
dd2$Variable<-"Ideologically Novel Parties 
with 
			Roots in Pre-existing Parties"
dd2$Death<-"Merger Death"
dd3$Variable<-"Ideologically Novel Parties 
without 
			Roots in Pre-existing Parties"
dd3$Death<-"Dissolution Death"
dd4$Variable<-"Ideologically Novel Parties 
without 
			Roots in Pre-existing Parties"
dd4$Death<-"Merger Death"



dd<-rbind(dd1,dd2,dd3,dd4)

dd<-as.data.frame(dd)

p<-p+
  geom_ribbon(data = dd, aes(x = x, ymin = ymin, ymax = ymax),
              alpha = 0.4)+
xlab("Party Age (in years)")+ylab("Log-hazard of party death")+
	facet_grid(Variable ~ Death)+
	geom_hline(yintercept=0, linetype="dashed")+
	theme_bw()+
	 theme(legend.position="none")+
	theme(strip.text.x = element_text(size=10, face="bold"),
          strip.text.y = element_text(size=10, face="bold"))

pdf(file = "Figure.A.12.pdf")
grid.draw(p)
dev.off()

rm(Dynamics.Logarithmic)

################################################# Figure A.13 #############################################################################################

load("Dynamics_Linear")

F.A.13<-subset(Dynamics.Linear, Variable=="Ideologically Novel & Organizationally Rooted"|
	Variable=="Ideologically Novel & NOT Organizationally Rooted")


F.A.13$Variable<-factor(F.A.13$Variable,
		levels=c("Ideologically Novel & Organizationally Rooted",
			  "Ideologically Novel & NOT Organizationally Rooted"),
		labels=c("Ideologically Novel Parties 
with 
			Roots in Pre-existing Parties", "Ideologically Novel Parties 
without 
			Roots in Pre-existing Parties"))



F.A.13$Death<-factor(F.A.13$Death,
		levels=c("Dissolution",
			  "Merger"),
		labels=c("Dissolution Death", "Merger Death"))




for (k in 1:4) {
F.A.13[,k]<-as.numeric(F.A.13[,k])
}



p<-ggplot(F.A.13) + geom_line(aes(y=y, x=x))+ 
  stat_smooth(aes(x = x, y = lower, colour = "grey12"), method = "loess", se = FALSE) +
  stat_smooth(aes(x = x, y = upper, colour = "grey12"), method = "loess", se = FALSE)+
  facet_grid(Variable ~ Death)


d1<-subset(F.A.13, Variable=="Ideologically Novel Parties 
with 
			Roots in Pre-existing Parties" & Death=="Dissolution Death")

d2<-subset(F.A.13, Variable=="Ideologically Novel Parties 
with 
			Roots in Pre-existing Parties" & Death=="Merger Death")

d3<-subset(F.A.13, Variable=="Ideologically Novel Parties 
without 
			Roots in Pre-existing Parties" &  Death=="Dissolution Death")

d4<-subset(F.A.13, Variable=="Ideologically Novel Parties 
without 
			Roots in Pre-existing Parties" &    Death=="Merger Death")



g1 <- ggplot(d1) + 
  stat_smooth(aes(x = x, y = lower, colour = "min"), method = "loess", se = FALSE) +
  stat_smooth(aes(x = x, y = upper, colour = "max"), method = "loess", se = FALSE)

gg1 <- ggplot_build(g1)

dd1 <- data.frame(x = gg1$data[[1]]$x,
                  ymin = gg1$data[[1]]$y,
                  ymax = gg1$data[[2]]$y) 



g2 <- ggplot(d2) + 
  stat_smooth(aes(x = x, y = lower, colour = "min"), method = "loess", se = FALSE) +
  stat_smooth(aes(x = x, y = upper, colour = "max"), method = "loess", se = FALSE)

gg2 <- ggplot_build(g2)

dd2 <- data.frame(x = gg2$data[[1]]$x,
                  ymin = gg2$data[[1]]$y,
                  ymax = gg2$data[[2]]$y) 

g3 <- ggplot(d3) + 
  stat_smooth(aes(x = x, y = lower, colour = "min"), method = "loess", se = FALSE) +
  stat_smooth(aes(x = x, y = upper, colour = "max"), method = "loess", se = FALSE)
gg3 <- ggplot_build(g3)

dd3 <- data.frame(x = gg3$data[[1]]$x,
                  ymin = gg3$data[[1]]$y,
                  ymax = gg3$data[[2]]$y) 

g4 <- ggplot(d4) + 
  stat_smooth(aes(x = x, y = lower, colour = "min"), method = "loess", se = FALSE) +
  stat_smooth(aes(x = x, y = upper, colour = "max"), method = "loess", se = FALSE)
gg4 <- ggplot_build(g4)

dd4 <- data.frame(x = gg4$data[[1]]$x,
                  ymin = gg4$data[[1]]$y,
                  ymax = gg4$data[[2]]$y) 




dd1$Variable<-"Ideologically Novel Parties 
with 
			Roots in Pre-existing Parties"
dd1$Death<-"Dissolution Death"
dd2$Variable<-"Ideologically Novel Parties 
with 
			Roots in Pre-existing Parties"
dd2$Death<-"Merger Death"
dd3$Variable<-"Ideologically Novel Parties 
without 
			Roots in Pre-existing Parties"
dd3$Death<-"Dissolution Death"
dd4$Variable<-"Ideologically Novel Parties 
without 
			Roots in Pre-existing Parties"
dd4$Death<-"Merger Death"



dd<-rbind(dd1,dd2,dd3,dd4)

dd<-as.data.frame(dd)



p<-p+
  geom_ribbon(data = dd, aes(x = x, ymin = ymin, ymax = ymax),
              alpha = 0.4)+
xlab("Party Age (in years)")+ylab("Log-hazard of party death")+
	facet_grid(Variable ~ Death)+
	geom_hline(yintercept=0, linetype="dashed")+
	theme_bw()+
	 theme(legend.position="none")+
	theme(strip.text.x = element_text(size=10, face="bold"),
          strip.text.y = element_text(size=10, face="bold"))

pdf(file = "Figure.A.13.pdf")
grid.draw(p)
dev.off()

rm(Dynamics.Linear)
