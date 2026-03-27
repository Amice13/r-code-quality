## SETUP FOR ANALYSIS ####

#NOTE: To run, create a "Figures" folder in the same folder as data and script

#install_github("naoki-egami/exr", dependencies = TRUE) # use if necessary
packages <- c("ggplot2","ggstance","gdata","gridExtra","stargazer","haven","foreign",
              "ivreg","lmtest","sandwich","exr","devtools","xtable")

ipak <- function(pkg){new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
if(length(new.pkg)) install.packages(new.pkg, dependencies = TRUE)
sapply(pkg, require, character.only = TRUE)
}
ipak(packages)

rm(list = ls())

setwd(".....")

# Read and Merge datasets

b = read.csv("BaselineMidline_Cleaned.csv",stringsAsFactors = F)
d = data.frame(read.spss("Endline_Cleaned.sav"))
h = data.frame(read.spss("Household_Cleaned.sav"))

d$id <- as.numeric(as.character(d$id))
h$id <- as.numeric(as.character(h$id))

d <- merge(d,b,by="id",all=T)
d <- merge(d,h,by="id",all=T)
d <- d[order(d$id),]
rm(b,h)

colnames(d)


# Setup of main covariates

d$age <- 2018 - d$bQ2
d$male <- ifelse(d$bQ1=="Male",1,0)
d$edu <- (d$bQ13=="Secondary school: 10th Standard completed")+
  (d$bQ13=="Senior secondary school: 12th Standard completed")*2 +
  (d$bQ13=="Graduate: Degree completed")*3 +
  (d$bQ13=="Postgraduate: Degree completed")*3
d$employed <- ifelse(d$bQ14=="Yes",1,0)
d$married <- ifelse(d$bQ10=="Yes",1,0)
d$st <- ifelse(d$bQ4=="ST",1,0)

d$respond_m <- ifelse(!is.na(d$mQ10),1,0)
d$respond_e <- ifelse(!is.na(d$Q1a),1,0)
d$respond_h <- ifelse(!is.na(d$hQ1),1,0)

d$mig <- ifelse(as.numeric(d$Q18a)==3|as.numeric(d$Q18b)==3|
                  as.numeric(d$Q18c)==3|as.numeric(d$Q18d)==3,1,0)


## Functions for Main Analysis ##

#Create hypothetical treatment vectors

set.seed(032719)
reps <- 10000
treat_hyp <- matrix(NA,nrow=nrow(d),ncol=reps)
blocks <- unique(d$block[!is.na(d$block)])
for(i in 1:reps){
  newtreat <- rep(NA,nrow(d))
  for(j in 1:length(blocks)){
    if(sum(!is.na(d$block)&d$block==blocks[j])==1) newtreat[!is.na(d$block)&d$block==blocks[j]] <- rbinom(n = 1,size=1,prob = .5)
    else newtreat[!is.na(d$block)&d$block==blocks[j]] <- sample(d$treatment[!is.na(d$block)&d$block==blocks[j]])
  }
  treat_hyp[,i] <- newtreat
}

#Functions to calculate ATE and P-Value

se <- function(x) sqrt(var(x)/length(x))

standardize <- function(m){
  (m-mean(m[d$treatment==0],na.rm=T))/sd(m[d$treatment==0],na.rm=T)
}

indexer <- function(mat){
  newmat <- matrix(NA,nrow=nrow(mat),ncol=ncol(mat))
  for(j in 1:ncol(mat)){
    m <- mean(mat[d$treatment==0,j],na.rm=T)
    sd <- sd(mat[d$treatment==0,j],na.rm = T)
    if(sd==0){
      sd <- sd(mat[,j],na.rm = T) }
    newmat[,j] <- (mat[,j]-m)/sd
  }
  index_var <- rep(NA,nrow(mat))
  for(j in 1:nrow(mat)){
    index_var[j] <- ifelse(sum(!is.na(newmat[j,]))>0,
                           sum(newmat[j,],na.rm=T),NA)
  }
  sdev <- sd(index_var[d$treatment==0],na.rm=T)
  return(index_var/sdev)
}

regress_iv <- function(var, pre, two.sided){
  
  mean_c <- mean(var[d$treatment==0],na.rm=T)
  mean_t <- mean(var[d$treatment==1],na.rm=T)
  diff_in_means <- mean_t-mean_c
  
  reg_table <- summary(lm(var~d$treatment+pre))$coefficients
  ate <- reg_table[2,1]
  n <- sum(!is.na(var)&!is.na(d$treatment))
  pval_ols <- ifelse(two.sided,reg_table[2,4],
                     ifelse(ate>0,reg_table[2,4]/2,1-reg_table[2,4]/2))
  
  diff_ests <- ate_ests <- rep(NA,length(reps))
  for(i in 1:reps){
    diff_ests[i] <- mean(var[treat_hyp[,i]==1],na.rm=T) - mean(var[treat_hyp[,i]==0],na.rm=T)
    ate_ests[i] <- summary(lm(var~treat_hyp[,i]+pre))$coefficients[2,1]
  }
  
  iv_table <- summary(ivreg(var~ pre | d$mig | d$treatment))$coefficients
  ate_iv <- iv_table[2,1]
  pval_iv <- ifelse(two.sided,iv_table[2,4],
                    ifelse(ate_iv>0,iv_table[2,4]/2,1-iv_table[2,4]/2))
  
  pval_simple <- ifelse(two.sided,mean(abs(diff_ests)>=abs(diff_in_means)),mean(diff_ests>=diff_in_means))
  pval_regress <- ifelse(two.sided,mean(abs(ate_ests)>=abs(ate)),mean(ate_ests>=ate))
  return(c(mean_c,mean_t,pval_simple,ate,pval_regress,pval_ols,ate_iv,pval_iv,n))
}

multiple_results <- function(matr,pr,twosided=F){
  resultsmat <- matrix(nrow=ncol(matr),ncol=9)
  colnames(resultsmat) <- c("C.Mean","T.Mean","P_DIM","Coef","P_OLS_RI","P_OLS","Coef_IV","P_IV","N")
  rownames(resultsmat) <- colnames(matr)
  for(i in 1:ncol(matr)){
    v <- matr[,i]
    p <- pr[,i]
    resultsmat[i,] <- regress_iv(v,p,two.sided=twosided)
    print(paste0(i," of ",ncol(matr)))
  }
  resultsmat <- round(resultsmat,3)
  return(resultsmat)
}


simple_results <- function(va,pre,twosided=F){
  resultsmat <- matrix(nrow=1,ncol=9)
  colnames(resultsmat) <- c("C.Mean","T.Mean","P_DIM","Coef","P_OLS_RI","P_OLS","Coef_IV","P_IV","N")
  rownames(resultsmat) <- c("DV")
  resultsmat[1,] <- regress_iv(va,pre,two.sided = twosided)
  resultsmat <- round(resultsmat,3)
  return(resultsmat)
}


## ANALYSIS ####


## Table 4: Demographics of Respondents ##

covariates <- cbind(d$age,d$male,ifelse(d$edu>=2,1,0),d$employed,d$married,d$st)
cov.table <- matrix(NA,nrow=ncol(covariates)+1,ncol=3)
rownames(cov.table) <- c("N","Mean Age","Pct Male","Pct 12th Grade","Pct Employed","Pct Married","Pct ST")
colnames(cov.table) <- c("Baseline","Midline","Endline")
responses <- list(rep(1,nrow(d)),d$respond_m,d$respond_e)

for(j in 1:3){
  cov_j <- covariates[responses[[j]]==1,]
  cov_j <- cov_j[!is.na(cov_j[,1]),]
  cov.table[1,j] <- nrow(cov_j)
  for(i in 1:ncol(covariates)){
    cov.table[i+1,j] <- mean(cov_j[,i],na.rm=T)
  }
}
print(xtable(data.frame(round(cov.table,2))))


## Figure 3 and Table A8: Migration over time ##

# Figure 3

loc_table <- data.frame(Date = character(length=10), D = character(length=10), Resp = character(length=10), Pct =numeric(length=10))

loc_table$Resp <- rep(c("Treatment","Control"),times=5)
loc_table$D <- rep(1:5,each=2)
loc_table$Date <- rep(c("9/2018","7/2019","11/2019","2/2020","2/2021"),each=2)

for(i in 1:10){
  vars <- d[d$treatment==ifelse(loc_table$Resp[i]=="Treatment",1,0)&d$respond_e==1,
            c("respond_e","Q18d","Q18c","Q18b","Q18a")]  
  loc_table$Pct[i] <- mean(as.numeric(vars[,loc_table$D[i]])==3,na.rm=T)*100
}

loc_table$Date <- factor(loc_table$Date,levels=c("9/2018","7/2019","11/2019","2/2020","2/2021"))
loc_table$Resp <- factor(loc_table$Resp,levels=c("Treatment","Control"))

pdf("Figures/Figure_3.pdf", width = 3, height = 2)
ggplot(data=loc_table, aes(x=Date, y=Pct,group=Resp,color=Resp)) +
  ylab("Percentage overseas") + xlab("")+  labs(color="Group")+
  geom_line(linewidth=1) + scale_x_discrete(labels=c("Sep\n2018","Jul\n2019","Nov\n2019","Feb\n2020","Feb\n2021")) +
  theme(text=element_text(family="serif"),legend.position="none", 
        axis.text.y=element_text(colour="black"),axis.text.x=element_text(colour="black")) + 
  scale_colour_grey(start=0,end=0.6) + 
  scale_y_continuous(limits = c(0, 25)) 
dev.off()

# Table 8A

mat_h1 <- pre_h1 <- matrix(NA,nrow=nrow(d),ncol=4)
colnames(mat_h1) <- c("Moved","Training","Offer","Moved in India")

mat_h1[,1] <- ifelse(as.numeric(d$Q18a)==3|as.numeric(d$Q18b)==3|
                       as.numeric(d$Q18c)==3|as.numeric(d$Q18d)==3,1,0)
mat_h1[,2] <- ifelse(as.numeric(d$Q15)==2,1,0)
mat_h1[,3] <- ifelse(as.numeric(d$Q16)>=3,1,0)
mat_h1[,4] <- ifelse(as.numeric(d$Q18a)==2|as.numeric(d$Q18b)==2|
                       as.numeric(d$Q18c)==2|as.numeric(d$Q18d)==2,1,0)

pre_h1[,1] <- pre_h1[,2] <- pre_h1[,3] <- pre_h1[,4] <- d$english

#Note: P-value on moved in India is reversed because of negative sign - so 1.000 p value is .000
results_moving <- multiple_results(mat_h1,pre_h1)
print(xtable(data.frame(results_moving),digits=3))


## Figure 4 and Table A.10: Democracy Preferences ##

dem_qs <- cbind(as.numeric(d$Q30),as.numeric(d$Q31),as.numeric(d$Q32))
mat_h4 <- cbind(indexer(dem_qs), dem_qs)

pre_dem <- cbind(d$bQ35,d$bQ36,d$bQ37)
pre_dem <- (pre_dem=="Economic development is definitely more important")*1 +
  (pre_dem=="Economic development is somewhat more important")*2 +
  (pre_dem=="Democracy is somewhat more important")*3 + (pre_dem=="Democracy is definitely more important")*4 +
  (pre_dem=="Agree more with Statement 1")*1 + (pre_dem=="Agree more with Statement 2.")*2 +
  (pre_dem==2)*1 +(pre_dem==1)*2
pre_dem[pre_dem==0] <- NA
pre_h4 <- cbind(indexer(pre_dem),pre_dem)

colnames(mat_h4) <- colnames(pre_h4) <- c("Index: Democracy Preference","Democracy vs. Getting Things Done","Democracy vs. Solving Econ. Problems","Democracy vs. Econ. Growth")

#Table A.10
results_dem <- multiple_results(mat_h4,pre_h4,twosided = T)
print(xtable(data.frame(results_dem),digits = 3))

#Figure 4

tol_table <- data.frame(Var = colnames(mat_h4),
                        Coef = numeric(length = ncol(mat_h4)),SE=numeric(length = ncol(mat_h4)))

for(i in 1:ncol(mat_h4)){
  var <- mat_h4[,i]/sd(mat_h4[d$treatment==0,i],na.rm=T)
  tol_table[i,2:3] <- summary(lm(var~d$treatment+pre_h4[,i]))$coefficients[2,1:2]
}

tol_table$Upper <- tol_table$Coef+tol_table$SE*1.65
tol_table$Lower <- tol_table$Coef-tol_table$SE*1.65
tol_table$Var <- factor(tol_table$Var,levels=rev(tol_table$Var[1:ncol(mat_h4)]))
tol_table$Group <- c("Index",rep("Component",times=ncol(mat_h4)-1))
tol_table$Group <- factor(tol_table$Group,levels=c("Index","Component"))

pdf("Figures/Figure_4.pdf", width = 5.5, height = 2)
ggplot(tol_table, aes(x = Coef, y = Var,color=Group)) +
  geom_point(size=2) + 
  scale_x_continuous(limits = c(-.8, .8)) +
  geom_errorbar(aes(xmin = Lower, xmax = Upper),width=0) + 
  geom_vline(xintercept = 0,color="black",linetype="dashed") + scale_colour_grey(start=0,end=.6) +
  theme_bw() + theme(text=element_text(family="serif"),legend.position="none") + 
  ylab("") + xlab("Effect of Program Selection") + ggtitle("")
dev.off()


## Figure 5 and Table A.9: Institutional Trust ##

trust_qs <- cbind(as.numeric(d$Q28a),as.numeric(d$Q28b),as.numeric(d$Q28c),
                  as.numeric(d$Q29a),as.numeric(d$Q29b),as.numeric(d$Q29c))
table(d$Q28)
table(trust_qs)
trust_qs <- matrix(ifelse(as.numeric(trust_qs)==1,NA,trust_qs),ncol=6)
mat_h3 <- cbind(indexer(trust_qs), trust_qs)

pre_trust <- cbind(d$bQ33a,d$bQ33b,d$bQ33c)
pre_trust <- (pre_trust=="A great deal of trust")*4 +
  (pre_trust=="Some trust")*3 + (pre_trust=="No answer")*2 +
  (pre_trust=="A little trust")*2 + (pre_trust=="No trust at all")*1
pre_h3 <- cbind(indexer(pre_trust),pre_trust,pre_trust)

colnames(mat_h3) <- colnames(pre_h3) <- c("Index: Govt Trust","Trust: National","Trust: State","Trust: Local",
                                          "Capable: National","Capable: State","Capable: Local")

#Table A.9
results_trust <- multiple_results(mat_h3,pre_h3,twosided = T)
print(xtable(data.frame(results_trust),digits = 3))

#Robustness Check: Exclude State Level (see Section 5.2)
exclude_state <- indexer(trust_qs[,c(1,3,4,6)])
simple_results(va=exclude_state,pre = pre_h3[,1])

#Figure 5

tol_table <- data.frame(Var = colnames(mat_h3),
                        Coef = numeric(length = ncol(mat_h3)),SE=numeric(length = ncol(mat_h3)))
for(i in 1:ncol(mat_h3)){
    var <- mat_h3[,i]/sd(mat_h3[d$treatment==0,i],na.rm=T)
    tol_table[i,2:3] <- summary(lm(var~d$treatment+pre_h3[,i]))$coefficients[2,1:2]
}

tol_table$Upper <- tol_table$Coef+tol_table$SE*1.65
tol_table$Lower <- tol_table$Coef-tol_table$SE*1.65
tol_table$Var <- factor(tol_table$Var,levels=rev(tol_table$Var[1:ncol(mat_h3)]))
tol_table$Group <- c("Index",rep("Component",times=6))
tol_table$Group <- factor(tol_table$Group,levels=c("Index","Component"))

pdf("Figures/Figure_5.pdf", width = 4.5, height = 2.5)
ggplot(tol_table, aes(x = Coef, y = Var,color=Group)) +
  geom_point(size=2) + 
  scale_x_continuous(limits = c(-.8, .8)) +
  geom_errorbar(aes(xmin = Lower, xmax = Upper),width=0) + 
  geom_vline(xintercept = 0,color="black",linetype="dashed") + scale_colour_grey(start=0,end=.6) +
  theme_bw() + theme(text=element_text(family="serif"),legend.position="none") + 
  ylab("") + xlab("Effect of Program Selection") + ggtitle("")
dev.off()


## Figure 6: Institutional Exposure ##

inst_vars <- cbind(2-as.numeric(d$Q53a),2-as.numeric(d$Q53b),2-as.numeric(d$Q53c),
                   2-as.numeric(d$Q55a),2-as.numeric(d$Q55b),2-as.numeric(d$Q55c))
colnames(inst_vars) <- c("Visa Office (India)","State Hospital (India)","Police Station (India)",
                         "Visa Office (Foreign)","State Hospital (Foreign)","Police Station (Foreign)")

mat_exposure <- cbind(indexer(inst_vars[,1:3]),inst_vars[,1:3],
                      indexer(inst_vars[,4:6]),inst_vars[,4:6])
pre_exposure <- cbind(pre_h3[,1],pre_h3[,1],pre_h3[,1],pre_h3[,1],
                      pre_h3[,1],pre_h3[,1],pre_h3[,1],pre_h3[,1])
colnames(mat_exposure) <- colnames(pre_exposure) <- c("Index: Indian Govt",colnames(inst_vars)[1:3],
                                                      "Index: Foreign Govt",colnames(inst_vars)[4:6])

# Figure 6

mean_table <- data.frame(Var = rep(colnames(inst_vars),times=2),Type = character(length=ncol(inst_vars)*2),Government = character(length=ncol(inst_vars)*2), N =numeric(length=ncol(inst_vars)*2))
mean_table$Group <- rep(c("Treatment","Control"),each=6)
mean_table$Government <- rep(c(rep("India",times=3),rep("Foreign",times=3)),times=2)
mean_table$Type <- rep(c("Visa Office","State Hospital","Police Station"),times=4)

for(i in 1:6){
  mean_table$N[i] <- mean(inst_vars[d$treatment==1,i],na.rm = T)*100
}
for(i in 7:12){
  mean_table$N[i] <- mean(inst_vars[d$treatment==0,i-6],na.rm = T)*100
}
mean_table$Government <- factor(mean_table$Government,levels=c("India","Foreign"))
mean_table$Type <- factor(mean_table$Type, levels=c("Visa Office","State Hospital","Police Station"))

pdf("Figures/Figure_6.pdf", width = 5, height = 2.5)
ggplot(data=mean_table, aes(x=N, y=Type,color=Group)) +
  ylab("") + xlab("Percent Visited") +
  facet_wrap(~Government,ncol=1) +
  theme(text=element_text(family="serif")) +
  scale_fill_grey(start=1,end=0) + scale_colour_grey(start=.6,end=0)  +
  geom_point(stat="identity",size=2) + scale_x_continuous(limits = c(0,50))
dev.off()

# Evaluations of Indian vs. Foreign Governments
table(d$Q54,mat_h1[,1])#Indian government offices
table(d$Q56,mat_h1[,1])# Foreign government offices


## Figure 7 and Table A.11: Political Participation ##

part_qs <- cbind(5-as.numeric(d$Q25),5-as.numeric(d$Q26),5-as.numeric(d$Q27),
                 3-as.numeric(d$Q21),3-as.numeric(d$Q22),3-as.numeric(d$Q23),
                 2-as.numeric(d$Q24a),2-as.numeric(d$Q24b),2-as.numeric(d$Q24c),
                 2-as.numeric(d$Q24d),2-as.numeric(d$Q24e),2-as.numeric(d$Q24f),
                 2-as.numeric(d$Q24g),2-as.numeric(d$Q24i))
mat_h5 <- cbind(indexer(part_qs[,1:3]), part_qs[,1:3],
                indexer(part_qs[,4:6]),part_qs[,4:6],
                indexer(part_qs[,7:14]),part_qs[,7:14])

pre_part <- cbind(d[,c("bQ44a","bQ44b","bQ44c","bQ44d","bQ44e","bQ44f","bQ44g")],d$bQ46)
pre_part <- (pre_part=="No")*1+(pre_part=="Yes")*2+(pre_part=="Very unlikely")*1+
  (pre_part=="Somewhat unlikely")*2 + (pre_part=="Somewhat likely")*3+(pre_part=="Very likely")*4
pre_part[pre_part==0] <- NA
pre_h5 <- cbind(pre_part[,8],pre_part[,8],pre_part[,8],pre_part[,8],
                pre_part[,8],pre_part[,8],pre_part[,8],pre_part[,8],
                indexer(pre_part[,1:7]),pre_part[,1:7],pre_part[,3])

colnames(mat_h5) <- colnames(pre_h5) <- c("Index: Voting Intention","Next National","Next State","Next Local",
                                          "Index: Voting","Voted: 2019 National","Voted: 2018 State","Voted: Local",
                                          "Index: Participation","Attended Rally","Met w/ Campaign",
                                          "Attended Village Council","Attended NGO Meeting","Argued about Politics",
                                          "Worked for Candidate","Donated to Campaign","Spoke at Village Council")

#Table A.11
results_part <- multiple_results(mat_h5,pre_h5,twosided = T)
print(xtable(data.frame(results_part),digits = 3))

#Figure 7

tol_table <- data.frame(Var = colnames(mat_h5),
                        Coef = numeric(length = ncol(mat_h5)),SE=numeric(length = ncol(mat_h5)))
for(i in 1:ncol(mat_h5)){
  var <- mat_h5[,i]/sd(mat_h5[d$treatment==0,i],na.rm=T)
  tol_table[i,2:3] <- summary(lm(var~d$treatment+pre_h5[,i]))$coefficients[2,1:2]
}

tol_table$Upper <- tol_table$Coef+tol_table$SE*1.65
tol_table$Lower <- tol_table$Coef-tol_table$SE*1.65
tol_table$Var <- factor(tol_table$Var,levels=rev(tol_table$Var[1:ncol(mat_h5)]))
tol_table$Group <- c("Index",rep("Component",3),"Index",rep("Component",3),"Index",rep("Component",8))

tol_table$Group <- factor(tol_table$Group,levels=c("Index","Component"))
tol_table$Cluster <- c(rep("Voting Intention",4),rep("Voting",4),rep("Participation",9))
tol_table$Cluster <- factor(tol_table$Cluster,levels=c("Voting Intention","Voting","Participation"))

pdf("Figures/Figure_7.pdf", width = 5.5, height = 5)
ggplot(tol_table, aes(x = Coef, y = Var,color=Group)) +
  geom_point(size=2) + 
  facet_wrap(~Cluster,scales="free_y",ncol = 1) +
  scale_x_continuous(limits = c(-.8, .8)) +
  geom_errorbar(aes(xmin = Lower, xmax = Upper),width=0) + 
  geom_vline(xintercept = 0,color="black",linetype="dashed") + scale_colour_grey(start=0,end=.6) +
  theme_bw() + theme(text=element_text(family="serif"),legend.position="none") + 
  ylab("") + xlab("Effect of Program Selection") + ggtitle("")
dev.off()

# Mean Comparisons
by(mat_h5[,6],INDICES=mat_h1[,1],FUN=mean,na.rm=T) #2019 India election
by(mat_h5[,6],INDICES=d$treatment,FUN=mean,na.rm=T) #2019 India election
by(mat_h5[,7],INDICES=mat_h1[,1],FUN=mean,na.rm=T) #2018 Mizoram election
by(mat_h5[,7],INDICES=d$treatment,FUN=mean,na.rm=T) #2018 Mizoram election


## Figure 8: Household Results ##

hh_mat <- matrix(NA,nrow=nrow(d),ncol=5)

inst_vars <- cbind(as.numeric(d$hQ34A),as.numeric(d$hQ34B),as.numeric(d$hQ34C),
                   as.numeric(d$hQ35A),as.numeric(d$hQ35B),as.numeric(d$hQ35C))
for(i in 1:ncol(inst_vars)){
  inst_vars[,i] <- ifelse(inst_vars[,i]==5,NA,inst_vars[,i])
}
hh_mat[,1] <- indexer(inst_vars)

hh_mat[,2] <- indexer(cbind(as.numeric(d$hQ36),as.numeric(d$hQ37),as.numeric(d$hQ38)))

hh_mat[,3] <- indexer(cbind(5-as.numeric(d$hQ31),5-as.numeric(d$hQ32),5-as.numeric(d$hQ33)))
hh_mat[,4] <- indexer(cbind(5-as.numeric(d$hQ27),5-as.numeric(d$hQ28),5-as.numeric(d$hQ29)))
part_qs <- cbind(d$hQ30A,d$hQ30B,d$hQ30C,d$hQ30D,d$hQ30E,d$hQ30F,d$hQ30G,d$hQ30H,d$hQ30I)
hh_mat[,5] <- indexer(matrix(3-(as.numeric(part_qs)),ncol=9))

colnames(hh_mat) <- c("Institutional Trust","Democracy Preference","Voting Intention","Voting","Participation")

pre_hh <- cbind(pre_h3[,1],pre_h4[,1],pre_h5[,c(1,5,9)])

results_hh <- multiple_results(matr = hh_mat,pr = pre_hh,twosided = T)
print(xtable(data.frame(results_hh),digits=3))

#Figure 8

hh_table <- data.frame(Var = colnames(hh_mat),
                       Coef = numeric(length = ncol(hh_mat)),SE=numeric(length = ncol(hh_mat)))
for(i in 1:ncol(hh_mat)){
  var <- hh_mat[,i]/sd(hh_mat[d$treatment==0,i],na.rm=T)
  hh_table[i,2:3] <- summary(lm(var~d$treatment+pre_hh[,i]))$coefficients[2,1:2]
}

hh_table$Upper <- hh_table$Coef+hh_table$SE*1.65
hh_table$Lower <- hh_table$Coef-hh_table$SE*1.65
hh_table$Var <- factor(hh_table$Var,levels=rev(hh_table$Var[1:ncol(hh_table)]))

pdf("Figures/Figure_8.pdf", width = 4.5, height = 2)
ggplot(hh_table, aes(x = Coef, y = Var)) +
  geom_point(size=2) + 
  scale_x_continuous(limits = c(-.8, .8)) +
  geom_errorbar(aes(xmin = Lower, xmax = Upper),width=0) + 
  geom_vline(xintercept = 0,color="black",linetype="dashed") +
  theme_bw() + theme(text=element_text(family="serif"),legend.position="none") + 
  ylab("") + xlab("Effect of Program Selection") + ggtitle("")
dev.off()


## Appendix-Only Results ####

# Setup

main_outcomes <- cbind(mat_h3[,1],mat_h4[,1],mat_h5[,c(1,5,9)])
main_pre <- cbind(pre_h3[,1],pre_h4[,1],pre_h5[,c(1,5,9)])
main_pre[,3] <- main_pre[,4] <- standardize(main_pre[,3])
colnames(main_outcomes) <- colnames(main_pre) <- c("Institutional Trust","Democracy Preference","Voting Intention",
                                                   "Voting","Participation")

d$pre_income <- (d$bQ21=="Less than Rs. 5,000") + (d$bQ21=="Rs. 5,001 - Rs. 10,000") *2 +
  (d$bQ21=="Rs. 10,001 - Rs. 20,000") *3 + (d$bQ21=="Rs. 20,001 - Rs. 30,000") *4 +
  (d$bQ21=="Rs. 30,001 - Rs. 40,000") *5 + (d$bQ21=="Rs. 40,001 - Rs. 50,000") *6 +
  (d$bQ21=="Rs. 50,000 - Rs. 100,000") *7 + (d$bQ21=="Rs. 100,000 and above") *8

respond_e2 <- c(d$respond_e,0,0,0)
respond_h2 <- c(d$respond_h,0,0,0)
treat2 <- c(d$treatment,1,1,1)


## Table A.2: Balance Table ##

d$pre_trust <- pre_h3[,1]
d$pre_democracy <- pre_h4[,1]
d$pre_participation <- pre_h5[,9]

#F-Test on respondents' baseline covariates among all subjects (nothing is predictive)
#(still nothing is predictive)
balance.model1 <- lm(treatment ~ age+male+edu+employed+st+married+english+
                       pre_income+pre_trust+pre_democracy+pre_participation,data=d)
summary(balance.model1)
f_1 <- summary(balance.model1)$fstatistic[1]
f_reps <- rep(NA,reps)
for(i in 1:reps){
  newmod <- lm(treat_hyp[,i] ~ age+male+edu+employed+st+married+english+
                 pre_income+pre_trust+pre_democracy+pre_participation,data=d)
  f_reps[i]<-summary(newmod)$fstatistic[1]
}
mean(f_reps>=f_1)


#F-Test on respondents' baseline covariates among endline responders (still nothing is predictive)
balance.model2 <- lm(treatment ~ age+male+edu+employed+st+married+english+
                       pre_income+pre_trust+pre_democracy+pre_participation,data=d[d$respond_e==1,])
summary(balance.model2)
f_2 <- summary(balance.model2)$fstatistic[1]
f_reps <- rep(NA,reps)
for(i in 1:reps){
  newmod <- lm(treat_hyp[d$respond_e==1,i] ~ age+male+edu+employed+st+married+english+
                 pre_income++pre_trust+pre_democracy+pre_participation,data=d[d$respond_e==1,])
  f_reps[i]<-summary(newmod)$fstatistic[1]
}
mean(f_reps>=f_2)

#F-Test on respondents' baseline covariates among household responders
balance.model4 <- lm(treatment ~ age+male+edu+employed+st+married+english+
                       pre_income+pre_trust+pre_democracy+pre_participation,data=d[d$respond_h==1,])
summary(balance.model4)
f_4 <- summary(balance.model4)$fstatistic[1]
f_reps <- rep(NA,reps)
for(i in 1:reps){
  newmod <- lm(treat_hyp[d$respond_h==1,i] ~ age+male+edu+employed+st+married+english+
                 pre_income+pre_trust+pre_democracy+pre_participation,data=d[d$respond_h==1,])
  f_reps[i]<-summary(newmod)$fstatistic[1]
}
mean(f_reps>=f_4)

stargazer(balance.model1,balance.model2,balance.model4)


## Table A.3: Response Rates by Treatment Group ##

t.test(respond_e2[treat2==1],respond_e2[treat2==0]) # endline
t.test(respond_h2[treat2==1],respond_h2[treat2==0]) # household

t_hat_e <- mean(respond_e2[treat2==1],na.rm = T)-mean(respond_e2[treat2==0],na.rm = T)
t_hat_h <- mean(respond_h2[treat2==1],na.rm = T)-mean(respond_h2[treat2==0],na.rm = T)
t_ests_e <- t_ests_h <- rep(NA,reps)
for(i in 1:reps){
  t_ests_e[i] <- mean(respond_e2[treat_hyp[,i]==1],na.rm=T) - mean(respond_e2[treat_hyp[,i]==0],na.rm=T) 
  t_ests_h[i] <- mean(respond_h2[treat_hyp[,i]==1],na.rm=T) - mean(respond_h2[treat_hyp[,i]==0],na.rm=T) 
}
t_hat_e # difference in response: endline
t_hat_h # difference in response: household
mean(abs(t_hat_e)<=abs(t_ests_e)) # RI-based p-value: Endline
mean(abs(t_hat_h)<=abs(t_ests_h)) # RI-based p-value: Household


## Table A.4: Predictors of Attrition ##

# endline
attrition_e1 <- lm(d$respond_e ~ d$age+d$edu+d$st+d$employed+d$married+d$male+d$english)
attrition_e2 <- lm(d$respond_e ~ d$age+d$edu+d$st+d$employed+d$married+d$male+d$english+d$pre_income+pre_h3[,1]+pre_h4[,1]+pre_h5[,9])
summary(attrition_e1) # See F-stat p-value
summary(attrition_e2) # See F-stat p-value

# household
attrition_h1 <- lm(d$respond_h ~ d$age+d$edu+d$st+d$employed+d$married+d$male+d$english)
attrition_h2 <- lm(d$respond_h ~ d$age+d$edu+d$st+d$employed+d$married+d$male+d$english+d$pre_income+pre_h3[,1]+pre_h4[,1]+pre_h5[,9])
summary(attrition_h1) # See F-stat p-value
summary(attrition_h2) # See F-stat p-value

stargazer(attrition_e1,attrition_e2,attrition_h1,attrition_h2)#Table A.4


## Figure A.1: Midline vs. Endline Results

part_qs <- cbind(5-as.numeric(d$mQ34),3-as.numeric(d$mQ33h),3-as.numeric(d$mQ33a),
                 3-as.numeric(d$mQ33b),3-as.numeric(d$mQ33c),3-as.numeric(d$mQ33d),
                 3-as.numeric(d$mQ33e),3-as.numeric(d$mQ33f),3-as.numeric(d$mQ33g),3-as.numeric(d$mQ33i))

dem_qs <- cbind(as.numeric(d$mQ36),as.numeric(d$mQ37),as.numeric(d$mQ38))

main_mid <- cbind(standardize(d$mQ35),indexer(dem_qs),
                  standardize(part_qs[,1]),standardize(part_qs[,2]),indexer(part_qs[,3:10]))
colnames(main_mid) <- c("Institutional Trust","Democracy Preference","Voting Intention",
                        "Voting","Participation")

all_vars <- cbind(main_outcomes,main_mid)
all_pre <- cbind(main_pre,main_pre)

tol_table <- data.frame(Var = colnames(all_vars),
                        Coef = numeric(length = ncol(all_vars)),SE=numeric(length = ncol(all_vars)))

for(i in 1:ncol(all_vars)){
  var <- all_vars[,i]
  tol_table[i,2:3] <- summary(lm(var~d$treatment+all_pre[,i]))$coefficients[2,1:2]
}

tol_table$Upper <- tol_table$Coef+tol_table$SE*1.65
tol_table$Lower <- tol_table$Coef-tol_table$SE*1.65
tol_table$Var <- factor(tol_table$Var,levels=rev(tol_table$Var[1:ncol(main_outcomes)]))
tol_table$Survey <- c(rep(c("Endline","Midline"),each=ncol(main_outcomes)))
tol_table$Survey <- factor(tol_table$Survey,levels=c("Midline","Endline"))
tol_table <- tol_table[tol_table$Var%in%c("Voting Intention","Voting","Participation"),]

pdf("Figures/Figure_A1.pdf", width = 5.5, height = 1.5)
ggplot(tol_table, aes(x = Coef, y = Var,color=Survey)) +
  geom_point(size=2,position=position_dodgev(height=-0.5)) + 
  scale_x_continuous(limits = c(-.8, .8)) +
  geom_errorbar(aes(xmin = Lower, xmax = Upper),position=position_dodgev(height=-0.5),,width=0) + 
  geom_vline(xintercept = 0,color="black",linetype="dashed") + scale_colour_grey(start=.6,end=0) +
  theme_bw() + theme(text=element_text(family="serif")) + 
  ylab("") + xlab("Effect of Program Selection") + ggtitle("")
dev.off()


## Table A.12: Interest in Politics ##

interest_mat <- cbind(4-as.numeric(d$Q20c),4-as.numeric(d$Q20b),4-as.numeric(d$Q20a))
interest_mat <- cbind(indexer(interest_mat),interest_mat)

interest_set <- cbind(d$bQ38a,d$bQ38b,d$bQ38a)
interest_set <- (interest_set=="No attention at all")*1 + (interest_set=="A little attention")*2 + 
  (interest_set=="A lot of attention")*3
pre_interest <- cbind(indexer(interest_set),interest_set)

colnames(interest_mat) <- colnames(pre_interest) <- c("Index: Political Interest", "National","State","Local")

results_interest <- multiple_results(interest_mat,pre_interest,twosided = T)
print(xtable(data.frame(results_interest),digits = 3))


## Table A.13: Migrants' Experiences ##

table(d$Q60)
table(d$Q64)
table(d$mQ5e)


## Tables A.14 and A.15: Job Training Effects ##

# Table A.14: Control Group

d$uptake <- mat_h1[,2]
table(d$uptake, d$treatment)

training_1 <- lm(mat_h3[d$treatment==0,1]~uptake+age+male+employed+married+edu+st+pre_h3[d$treatment==0,1],data=d[d$treatment==0,])
training_2 <- lm(mat_h4[d$treatment==0,1]~uptake+age+male+employed+married+edu+st+pre_h4[d$treatment==0,1],data=d[d$treatment==0,])
training_3 <- lm(mat_h5[d$treatment==0,1]~uptake+age+male+employed+married+edu+st+pre_h5[d$treatment==0,1],data=d[d$treatment==0,])
training_4 <- lm(mat_h5[d$treatment==0,5]~uptake+age+male+employed+married+edu+st+pre_h5[d$treatment==0,5],data=d[d$treatment==0,])
training_5 <- lm(mat_h5[d$treatment==0,9]~uptake+age+male+employed+married+edu+st+pre_h5[d$treatment==0,9],data=d[d$treatment==0,])
stargazer(training_1,training_2,training_3,training_4,training_5)
#Note that the pre-treatment outcome variables are listed on separate lines in this table, combined in text

#Table A.15: Treatment Group
training_1 <- lm(mat_h3[d$treatment==1,1]~uptake+mig+age+male+employed+married+edu+st+pre_h3[d$treatment==1,1],data=d[d$treatment==1,])
training_2 <- lm(mat_h4[d$treatment==1,1]~uptake+mig+age+male+employed+married+edu+st+pre_h4[d$treatment==1,1],data=d[d$treatment==1,])
training_3 <- lm(mat_h5[d$treatment==1,1]~uptake+mig+age+male+employed+married+edu+st+pre_h5[d$treatment==1,1],data=d[d$treatment==1,])
training_4 <- lm(mat_h5[d$treatment==1,5]~uptake+mig+age+male+employed+married+edu+st+pre_h5[d$treatment==1,5],data=d[d$treatment==1,])
training_5 <- lm(mat_h5[d$treatment==1,9]~uptake+mig+age+male+employed+married+edu+st+pre_h5[d$treatment==1,9],data=d[d$treatment==1,])
stargazer(training_1,training_2,training_3,training_4,training_5)
#Note that the pre-treatment outcome variables are listed on separate lines in this table, combined in text


## Table A.16 and Figure A.2: Heterogeneous Effects

## Table A.16: With specific covariates

d$trust_govt <- mat_h3[,1]
d$democracy <- mat_h4[,1]
d$voting <- mat_h5[,5]
d$vote_intention <- mat_h5[,1]
d$participation <- mat_h5[,9]

d$christian <- ifelse(d$bQ3=="Christian",1,0)
d$pre_voting <- pre_h5[,5]
d$pre_participation <- pre_h5[,9]
d$pre_trust <- pre_h3[,1]
d$pre_democracy <- pre_h4[,1]


main_vars <- cbind(d$trust_govt,d$democracy,d$voting,d$vote_intention,d$participation)
main_var_names <- c("Trust in Govt","Support for Democracy","Past Voting","Voting Intention","Participation")

cov_vars <- cbind(covariates[,c(1:4,6)],ifelse(d$bQ3=="Christian",1,0),d$pre_voting,d$pre_trust,d$pre_participation,d$pre_democracy)

het_results <- het_cor <- matrix(NA, nrow = ncol(cov_vars),ncol=ncol(main_vars))
colnames(het_results) <- colnames(het_cor) <- main_var_names
rownames(het_results) <- rownames(het_cor) <- c("Age","Gender","Education","Employed","ST","Christian","Voting","Trust in Govt","Participation","Support for Democracy")

for(i in 1:nrow(het_results)){
  for(j in 1:ncol(het_results)){
    covar <- cov_vars[,i]
    outcome <- main_vars[,j]
    het_results[i,j] <- summary(lm(outcome~d$treatment*covar))$coefficients[4,3]
  }
}
print(xtable(het_results,digits=2))

#Figure A.2: Non-Parametric Version (Naoki et al.)

heterogeneous_effects <- data.frame(Trust_Govt=numeric(length=nrow(d)),Democracy=numeric(length=nrow(d)),
                                    Past_Voting=numeric(length=nrow(d)),Vote_Intention=numeric(length=nrow(d)),Participation=numeric(length=nrow(d)))

for(i in 1:ncol(heterogeneous_effects)){
  v <- main_vars[,i]
  sate_est <- summary(lm(main_vars[,i]~treatment,data = d))$coefficients[2,1:2]
  if(i==1) exr_out <- exr(outcome = "trust_govt", treatment = "treatment", covariates = c("age", "male", "edu","employed", "married", "st","christian","pre_voting","pre_trust","pre_participation","pre_democracy"), data = d,sate_estimate = sate_est) 
  if(i==2) exr_out <- exr(outcome = "democracy", treatment = "treatment", covariates = c("age", "male", "edu","employed", "married", "st","christian","pre_voting","pre_trust","pre_participation","pre_democracy"), data = d,sate_estimate = sate_est)
  if(i==3) exr_out <- exr(outcome = "voting", treatment = "treatment", covariates = c("age", "male", "edu","employed", "married", "st","christian","pre_voting","pre_trust","pre_participation","pre_democracy"), data = d,sate_estimate = sate_est)
  if(i==4) exr_out <- exr(outcome = "vote_intention", treatment = "treatment", covariates = c("age", "male", "edu","employed", "married", "st","christian","pre_voting","pre_trust","pre_participation","pre_democracy"), data = d,sate_estimate = sate_est)
  if(i==5) exr_out <- exr(outcome = "participation", treatment = "treatment", covariates = c("age", "male", "edu","employed", "married", "st","christian","pre_voting","pre_trust","pre_participation","pre_democracy"), data = d,sate_estimate = sate_est)
  tau_estimates <- exr_out$tau
  heterogeneous_effects[1:length(tau_estimates),i] <- matrix(tau_estimates,ncol=1)
}
heterogeneous_effects[heterogeneous_effects==0] <- NA

mat_he <- NULL
for(i in 1:ncol(heterogeneous_effects)){
  mat_he <- rbind(mat_he, cbind(main_var_names[i],heterogeneous_effects[!is.na(heterogeneous_effects[,i]),i]))
}
heterogeneous_effects <- data.frame(mat_he)
colnames(heterogeneous_effects) <- c("Variable","Tau")
heterogeneous_effects$Variable <- factor(heterogeneous_effects$Variable,levels=main_var_names)
heterogeneous_effects[,2] <- as.numeric(heterogeneous_effects[,2])
max(heterogeneous_effects[,2])

pdf("Figures/Figure_A2.pdf", width = 4, height = 3)
ggplot(heterogeneous_effects, aes(x=Tau)) + facet_wrap(~Variable,ncol=1) +
  geom_histogram(binwidth = .05) + theme(text=element_text(family="serif")) + geom_vline(xintercept = 0)+
  scale_x_continuous(limits = c(-0.5,1)) + xlab("Estimated Treatment Effects") + ylab("")
dev.off()

