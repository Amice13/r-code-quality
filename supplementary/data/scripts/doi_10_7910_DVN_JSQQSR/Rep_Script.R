#install_github("naoki-egami/exr", dependencies = TRUE) # use if necessary
packages <- c("ggplot2","ggstance","gdata","gridExtra","stargazer","haven","foreign",
              "ivreg","lmtest","sandwich","exr","devtools","xtable","egg")

ipak <- function(pkg){new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
if(length(new.pkg)) install.packages(new.pkg, dependencies = TRUE)
sapply(pkg, require, character.only = TRUE)
}
ipak(packages)


#set working directory to rep file location
#please create a folder labeled "Figures" to capture the figures
#setwd("")

rm(list = ls())
#sink("Quant_LogFile.txt")

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

## Basic Demographics of Respondents

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
print(xtable(data.frame(round(cov.table[,c(1,3)],2))))

## Functions for Main Analysis ####

#Create hypothetical treatment vectors

set.seed(032719) #seed marks date of first analysis of midline data
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
  se_ols <- reg_table[2,2]
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
  return(c(mean_c,mean_t,pval_simple,ate,se_ols,pval_regress,pval_ols,ate_iv,pval_iv,n))
}

multiple_results <- function(matr,pr,twosided=F){
  resultsmat <- matrix(nrow=ncol(matr),ncol=10)
  colnames(resultsmat) <- c("C.Mean","T.Mean","P_DIM","Coef","SE","P_OLS_RI","P_OLS","Coef_IV","P_IV","N")
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
  resultsmat <- matrix(nrow=1,ncol=10)
  colnames(resultsmat) <- c("C.Mean","T.Mean","P_DIM","Coef","SE","P_OLS_RI","P_OLS","Coef_IV","P_IV","N")
  rownames(resultsmat) <- c("DV")
  resultsmat[1,] <- regress_iv(va,pre,two.sided = twosided)
  resultsmat <- round(resultsmat,3)
  return(resultsmat)
}

likely_regress_2 <-  function(var, pre, li, twosided=F){
  
  reg_table <- summary(lm(var~d$treatment*li+pre))$coefficients
  rest <- likely <- diff <- rep(NA,3)
  
  rest <- reg_table[2,c(1,2,4)]
  diff <- reg_table[5,c(1,2,4)]
  
  unli <- (1-li)
  reg_table_2 <- summary(lm(var~d$treatment*unli+pre))$coefficients # same regression, just with likely reversed
  likely <- reg_table_2[2,c(1,2,4)]
  
  return(c(likely, rest, diff))
}

likely_results_2 <- function(matr,pr,lik,two.sided=rep(F,ncol(matr))){
  resultsmat <- matrix(nrow=ncol(matr),ncol=9)
  colnames(resultsmat) <- c("ATE_Likely","SE_Likely","Pval_Likely","ATE_Others","SE_Others","Pval_Others","Difference","SE_Diff","P-Value")
  rownames(resultsmat) <- colnames(matr)
  for(i in 1:ncol(matr)){
    v <- matr[,i]
    p <- pr[,i]
    resultsmat[i,] <- likely_regress_2(v,p,li=lik,twosided = two.sided[i])
    print(paste0(i," of ",ncol(matr)))
  }
  return(resultsmat)
}


## Migration Figure (Figure 1) ####

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

pdf("Figures/Figure_1.pdf", width = 3, height = 2)
ggplot(data=loc_table, aes(x=Date, y=Pct,group=Resp,color=Resp)) +
  ylab("Percentage overseas") + xlab("")+  labs(color="Group")+
  geom_line(linewidth=1) + scale_x_discrete(labels=c("Sep\n2018","Jul\n2019","Nov\n2019","Feb\n2020","Feb\n2021")) +
  theme(text=element_text(family="serif"),legend.position="none", 
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(),axis.line = element_line(colour = "black"),
        axis.text.y=element_text(colour="black"),axis.text.x=element_text(colour="black")) + 
  scale_colour_grey(start=0,end=0.6) + 
  scale_y_continuous(limits = c(0, 25)) 
dev.off()

# Migration Table (Table A.8)

mat_h1 <- pre_h1 <- matrix(NA,nrow=nrow(d),ncol=4)
colnames(mat_h1) <- c("Moved","Training","Offer","Moved in India")

mat_h1[,1] <- ifelse(as.numeric(d$Q18a)==3|as.numeric(d$Q18b)==3|
                       as.numeric(d$Q18c)==3|as.numeric(d$Q18d)==3,1,0)
mat_h1[,2] <- ifelse(as.numeric(d$Q15)==2,1,0)
mat_h1[,3] <- ifelse(as.numeric(d$Q16)>=3,1,0)
mat_h1[,4] <- ifelse(as.numeric(d$Q18a)==2|as.numeric(d$Q18b)==2|
                       as.numeric(d$Q18c)==2|as.numeric(d$Q18d)==2,1,0)

pre_h1[,1] <- pre_h1[,2] <- pre_h1[,3] <- pre_h1[,4] <- d$english
# English ability is the best pre-treatment predictor of migration

results_moving <- multiple_results(mat_h1,pre_h1)
print(xtable(data.frame(results_moving),digits=3))
#Note: Direction on final hypothesis is opposite, so 1.000 p-value translates to .000 p-value.

family_abroad <- ifelse(!(strtrim(d$bq30a_2,4)%in%c("    ","Indi")),1,0)
mean(family_abroad)


## Tolerance ####

vars <- cbind(d$bQ54,d$bQ55b,d$bQ55d,d$bQ55e,d$bQ55f,d$bQ55c,d$Q42,d$Q43b,d$Q43d,d$Q43e,d$Q43f,d$Q43c)
vars[,1] <- (vars[,1]=="Yes")*1+(vars[,1]=="Only in some cases")*.5+(vars[,1]=="No")*0
vars[,7] <- ifelse(as.numeric(vars[,7])==1,1,0)
vars[,8:12] <- 6-as.numeric(vars[,8:12])

vars[,2:6] <- matrix((vars[,2:6]=="Very Positive")*5+ (vars[,2:6]=="Positive")*4 + (vars[,2:6]=="Neutral" | vars[,2:6]=="No answer")*3 +
                       (vars[,2:6]=="Negative")*2 + (vars[,2:6]=="Very Negative")*1,ncol=5)
vars <- matrix(as.numeric(vars),ncol=ncol(vars))
colnames(vars) <- c("Pre_Mizo","Pre_Indian","Pre_Bangladesh","Pre_Pakistan","Pre_MiddleEast","Pre_European",
                    "Post_Mizo","Post_Indian","Post_Bangladesh","Post_Pakistan","Post_MiddleEast","Post_European")

# Descriptive Table (Table A.11)

sum_table <- cbind(colnames(vars),round(colMeans(vars[d$treatment==1,],na.rm=T),2),round(colMeans(vars[d$treatment==0,],na.rm=T),2))
colnames(sum_table) <- c("Variable","T.Mean","C.Mean")
tolerance_means <- cbind(sum_table[c(1,3:6),2:3],sum_table[c(7,9:12),2:3])
colnames(tolerance_means) <- c("Pre_T","Pre_C","Post_T","Post_C")
rownames(tolerance_means) <- c("Non-Mizo","Bangladesh","Pakistan","MiddleEast","European")
xtable(tolerance_means)

# Results Table (Table A.10)

mat_h2 <- cbind(indexer(vars[,c(7,9:12)]),vars[,7]==1,vars[,9:12])
colnames(mat_h2) <- c("Tolerance Index","OK to marry non-Mizo","View of Bangladeshis","View of Pakistanis","View of Middle Easterners","View of Europeans")
pre_h2 <- cbind(indexer(vars[,c(1,3:6)]),vars[,c(1,3:6)])

results_tolerance <- multiple_results(mat_h2,pre_h2)
print(xtable(data.frame(results_tolerance)))

# Raw Means Figure (Figure 2A)

vars <- vars[,c(7,9:12)]

mean_table <- data.frame(Group = character(length=ncol(vars)*2), N =numeric(length=ncol(vars)*2))
mean_table$Group <- rep(c("Treatment","Control"),each=ncol(vars))
mean_table$Var <- rep(rownames(results_tolerance)[2:6],times=2)

for(i in 1:nrow(mean_table)){
  groupresponses <- vars[d$treatment==ifelse(mean_table$Group[i]=="Treatment",1,0),]
  if(i<=5) var <- groupresponses[,i]*100
  if(i>5) var <- groupresponses[,(i-5)]*100
  if(i%in%c(2:5,7:10)) var <- (var-100)/4
  mean_table$N[i] <- mean(as.numeric(var),na.rm = T)
}

mean_table$Var <- factor(mean_table$Var, levels=c("View of Europeans","View of Middle Easterners","View of Pakistanis","View of Bangladeshis","OK to marry non-Mizo"))

pdf("Figures/Figure_2A.pdf", width = 7, height = 1.5)
f2a <- ggplot(data=mean_table, aes(x=N, y=Var, color=Group,group=Group)) +
  ylab("") + xlab("")+  labs(color="",fill="") +
  theme(text=element_text(family="serif"),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(),axis.line = element_line(colour = "black"),
        axis.text.y=element_text(colour="black"),axis.text.x=element_text(colour="black"),legend.position="bottom") +
  scale_fill_grey(start=1,end=0) + scale_colour_grey(start=.6,end=0) +
  geom_point(stat="identity",size=2) + scale_x_continuous(limits = c(40,70))
f2a
dev.off()

# Coefficient Plot (Figure 2B)

tol_table <- data.frame(Var = rownames(results_tolerance),
                        Coef = numeric(length = ncol(mat_h2)),SE=numeric(length = ncol(mat_h2)))

for(i in 1:ncol(mat_h2)){
  std_var <- indexer(cbind(mat_h2[,i]))
  tol_table[i,2:3] <- summary(lm(std_var~d$treatment+pre_h2[,i]))$coefficients[2,1:2]
}
tol_table$Upper <- tol_table$Coef+tol_table$SE*1.65
tol_table$Lower <- tol_table$Coef-tol_table$SE*1.65
tol_table$Var <- factor(tol_table$Var,levels=rev(tol_table$Var[1:ncol(mat_h2)]))
tol_table$Group <- c("Index",rep("Component",times=5))
tol_table$Group <- factor(tol_table$Group,levels=c("Index","Component"))

pdf("Figures/Figure_2B.pdf", width = 3.5, height = 2)
f2b <- ggplot(tol_table[1:ncol(mat_h2),], aes(x = Coef, y = Var,color=Group)) +
  geom_point(size=2) + 
  scale_x_continuous(limits = c(-.2, .7)) +
  geom_errorbar(aes(xmin = Lower, xmax = Upper),width=0) + 
  geom_vline(xintercept = 0,color="black",linetype="dashed") + scale_colour_grey(start=0,end=.6) +
  theme_bw() + theme(text=element_text(family="serif"),legend.position="none",panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(),axis.line = element_line(colour = "black"),
                     axis.text.y=element_text(colour="black"),axis.text.x=element_text(colour="black")) + 
  ylab("") + xlab("") + ggtitle("")
f2b
dev.off()

# CACE Coefficient Plot (Figure 2C)

tol_table <- data.frame(Var = rownames(results_tolerance),
                        Coef = numeric(length = ncol(mat_h2)),SE=numeric(length = ncol(mat_h2)))

for(i in 1:ncol(mat_h2)){
  std_var <- indexer(cbind(mat_h2[,i]))
  tol_table[i,2:3] <- summary(ivreg(std_var~ pre_h2[,i] | d$mig | d$treatment))$coefficients[2,1:2]
}
tol_table$Upper <- tol_table$Coef+tol_table$SE*1.65
tol_table$Lower <- tol_table$Coef-tol_table$SE*1.65
tol_table$Var <- factor(tol_table$Var,levels=rev(tol_table$Var[1:ncol(mat_h2)]))
tol_table$Group <- c("Index",rep("Component",times=5))
tol_table$Group <- factor(tol_table$Group,levels=c("Index","Component"))

pdf("Figures/Figure_2C.pdf", width = 3.5, height = 2)
f2c <- ggplot(tol_table[1:ncol(mat_h2),], aes(x = Coef, y = Var,color=Group)) +
  geom_point(size=2) + 
  scale_x_continuous(limits = c(-.5, 3.5)) +
  geom_errorbar(aes(xmin = Lower, xmax = Upper),width=0) + 
  geom_vline(xintercept = 0,color="black",linetype="dashed") + scale_colour_grey(start=0,end=.6) +
  theme_bw() + theme(text=element_text(family="serif"),legend.position="none", axis.text.y=element_text(colour="black") ,panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(),axis.line = element_line(colour = "black"),
                     axis.text.x=element_text(colour="black")) + 
  ylab("") + xlab("") + ggtitle("")
f2c
dev.off()

pdf("Figures/Figure_2.pdf",width = 4,height=5,onefile = FALSE)
ggarrange(f2a,f2b,f2c,ncol=1)
dev.off()


# Descriptive Results by Migration (Figure A.2)

vars <- cbind(mat_h2[,1],pre_h2[,1])
mig_grp <- ifelse(mat_h1[,1]==1,1,
                  ifelse(mat_h1[,4]==1,2,3))

mean_table <- data.frame(Date = character(length=6), Grp=numeric(length=6), Group = character(length=6), Mean =numeric(length=6),SE =numeric(length=6))
mean_table$Group <- rep(c("Migrated Overseas","Migrated Internally","Stayed in Mizoram"),each=2)
mean_table$Grp <- rep(1:3,each=2)
mean_table$Date <- rep(c("Pre","Post"),times=3)

se <- function(x) sqrt(var(x)/length(x))

for(i in 1:nrow(mean_table)){
  groupresponses <- vars[mig_grp==mean_table$Grp[i],ifelse(mean_table$Date[i]=="Post",1,2)]
  groupresponses <- groupresponses[!is.na(groupresponses)]
  mean_table$Mean[i] <- mean(groupresponses)
  mean_table$SE[i] <- se(groupresponses)
}
mean(groupresponses,na.rm=T)

mean_table$Date <- factor(mean_table$Date,levels=c("Pre","Post"))
mean_table$Lower <- mean_table$Mean-1.96*mean_table$SE
mean_table$Upper <- mean_table$Mean+1.96*mean_table$SE
mean_table$Group <- factor(mean_table$Group,levels=c("Migrated Overseas","Migrated Internally","Stayed in Mizoram"))

pdf("Figures/Figure_A2.pdf", width = 4, height = 3)
ggplot(data=mean_table, aes(x=Date, y=Mean, color=Group,group=Group)) +
  ylab("Tolerance Index") + xlab("")+  labs(color="Location",fill="Location") +
  theme(text=element_text(family="serif")) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width=0,position=position_dodge(width=-0.5)) +
  scale_fill_grey(start=0,end=0.6) + scale_colour_grey(start=0,end=0.6) +
  geom_line(stat="identity",size=1,position=position_dodge(width =-0.5)) + 
  geom_point(stat="identity",size=2,position=position_dodge(width=-0.5)) + scale_y_continuous(limits = c(-.2,1.2))+
  geom_hline(yintercept = 0,linetype="dashed")
dev.off()


## Cosmopolitan Identity  ####

vars <- cbind(as.numeric(as.factor(d$bQ27))==1,as.numeric(d$Q33)==1)
colnames(vars) <- c("Pre_Identity","Post_Identity")

#Graph of all identities (Figure 3A)

id_table <- data.frame(Answer=character(length = 20),Ans=numeric(length=20),
                       Date = character(length=20), Group = character(length=20), N =numeric(length=20))
answer_names <- c("Citizen of the world","Mizo","Individual","Part of locality","Indian")

all_answers <- ceiling(as.numeric(as.factor(c(d$bQ27,as.character(d$Q33))))/2)
all_treat <- c(d$treatment,d$treatment)
all_dates <- rep(c("Pre","Post"),each=nrow(d))

id_table$Ans <- rep(1:5,times=4)
id_table$Answer <- rep(answer_names,times=4)
id_table$Group <- rep(c("Treatment","Control"),each=10)
id_table$Date <- rep(rep(c("Pre","Post"),each=5),times=2)

for(i in 1:20){
  groupresponses <- all_answers[all_treat==ifelse(id_table$Group[i]=="Treatment",1,0)&
                                  all_dates==as.character(id_table$Date[i])]
  id_table$N[i] <- mean(groupresponses==id_table$Ans[i],na.rm = T)*100
}

id_table$Answer <- factor(id_table$Answer, levels=c("Individual","Part of locality","Mizo","Indian","Citizen of the world"))
id_table$Date <- factor(id_table$Date,levels=c("Pre","Post"))

pdf("Figures/Figure_3A.pdf", width = 7, height = 2)
f3a <- ggplot(data=id_table[c(6:10,16:20),], aes(x=Group, y=N,group=Answer, color=Answer,fill=Answer)) +
  ylab("Percent identifying primarily as:") + xlab(" ")+  labs(color=" ",fill=" ")+
  theme(text=element_text(family="serif"),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(),axis.line = element_line(colour = "black"),
        axis.text.y=element_text(colour="black"),axis.text.x=element_text(colour="black"),legend.position="bottom") + 
  scale_fill_grey(start=.9,end=0) + scale_colour_grey(start=.9,end=0) +
  geom_bar(stat="identity") + scale_y_continuous(limits = c(0, 100)) + coord_flip()
f3a
dev.off()

# Coefficient Plot (Figure 3B)

cofw <- vars[,2]
pre_cofw <- vars[,1]

cofw_table <- data.frame(Var = "Citizen of the world",
                        Coef = NA,SE=NA)
cofw_table[,2:3] <- summary(lm(cofw~d$treatment+pre_cofw))$coefficients[2,1:2]*100

cofw_table$Upper <- cofw_table$Coef+cofw_table$SE*1.65
cofw_table$Lower <- cofw_table$Coef-cofw_table$SE*1.65

pdf("Figures/Figure_3B.pdf", width = 3.5, height = 1)
f3b <- ggplot(cofw_table, aes(x = Coef, y = Var)) +
  geom_point(size=2) + 
  scale_x_continuous(limits = c(-5, 20)) +
  geom_errorbar(aes(xmin = Lower, xmax = Upper),width=0) + 
  geom_vline(xintercept = 0,color="black",linetype="dashed") +
  theme_bw() + theme(text=element_text(family="serif"),legend.position="none",panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(),axis.line = element_line(colour = "black"),
                     axis.text.y=element_text(colour="black"),axis.text.x=element_text(colour="black")) + 
  ylab("") + xlab("") + ggtitle("")
f3b
dev.off()

# CACE Plot (Figure 3C)

cofw_table <- data.frame(Var = "Citizen of the world",
                         Coef = NA,SE=NA)
cofw_table[,2:3] <- summary(ivreg(cofw~ pre_cofw | d$mig | d$treatment))$coefficients[2,1:2]*100

cofw_table$Upper <- cofw_table$Coef+cofw_table$SE*1.65
cofw_table$Lower <- cofw_table$Coef-cofw_table$SE*1.65

pdf("Figures/Figure_3C.pdf", width = 3.5, height = 1)
f3c <- ggplot(cofw_table, aes(x = Coef, y = Var)) +
  geom_point(size=2) + 
  scale_x_continuous(limits = c(-20, 100)) +
  geom_errorbar(aes(xmin = Lower, xmax = Upper),width=0) + 
  geom_vline(xintercept = 0,color="black",linetype="dashed") +
  theme_bw() + theme(text=element_text(family="serif"),legend.position="none",axis.text.y=element_text(colour="black"),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(),axis.line = element_line(colour = "black"),
                     axis.text.x=element_text(colour="black")) + 
  ylab("") + xlab("") + ggtitle("")
f3c
dev.off()

pdf("Figures/Figure_3.pdf",width = 6.5,height=4,onefile = FALSE)
ggarrange(f3a,f3b,f3c,ncol=1)
dev.off()



## International Cooperation ####

vars <- cbind(d$bQ50,d$bQ52,d$bQ49,d$bQ48,d$bQ38c,d$bQ38d,ifelse(as.numeric(as.factor(d$bQ27))==1,1,0),d$Q40,d$Q41,d$Q39,d$Q38,as.numeric(d$Q20d),as.numeric(d$Q20e),ifelse(as.numeric(d$Q33)==1,1,0))

colnames(vars) <- c("Pre_TradeGood","Pre_PeaceWPakistan","Pre_MigrationGood","Pre_BangladeshMigration","Pre_InterestOther","Pre_InterestForeign","Pre_WorldCit",
                    "Post_TradeGood","Post_PeaceWPakistan","Post_MigrationGood","Post_BangladeshMigration","Post_InterestOther","Post_InterestForeign","Post_WorldCit")

table(vars[,2])
vars[,c(1,3)] <- matrix((vars[,c(1,3)]=="Hurts them a lot")*5+ (vars[,c(1,3)]=="Hurts them a little")*4 + (vars[,c(1,3)]=="Does not affect them much")*3 +
                          (vars[,c(1,3)]=="Improves them a little")*2 + (vars[,c(1,3)]=="Improves them a lot")*1,ncol=2)
vars[,c(2,4)] <- matrix((vars[,c(2,4)]=="Strongly disagree")*5+ (vars[,c(2,4)]=="Somewhat disagree")*4 + (vars[,c(2,4)]=="Neither agree nor disagree")*3 +
                          (vars[,c(2,4)]=="Somewhat agree")*2 + (vars[,c(2,4)]=="Strongly agree")*1,ncol=2)
vars[,5:6] <- vars[,1:4] <- matrix((vars[,5:6]=="A lot of attention")*1+ (vars[,5:6]=="A little attention")*2 + (vars[,5:6]=="No attention at all")*3,ncol=2)
vars[,1:4] <- matrix(ifelse(vars[,1:4]==0,NA,vars[,1:4]),ncol=4)
vars <- matrix(as.numeric(vars),ncol=ncol(vars))
vars[,c(4,11)] <- matrix(5-vars[,c(4,11)],ncol=2) # reverse direction of Bangladeshi migration question

# Results Table (Table A.12)

mat_h4 <- cbind(-indexer(vars[,8:9]),6-vars[,8:9],-indexer(vars[,10:11]),6-vars[,10:11],-indexer(vars[,12:13]),vars[,14])
colnames(mat_h4) <- c("Cooperation Index","Trade Improves Lives","Peace w/ Pakistan is Important","Migration Index","Migration Improves Lives","Support Migration into India","Interest in International News","World Citizen")
pre_h4 <- cbind(-indexer(vars[,1:2]),6-vars[,1:2],-indexer(vars[,3:4]),6-vars[,3:4],-indexer(vars[,5:6]),vars[,7])

results_cooperation <- multiple_results(mat_h4,pre_h4)
print(xtable(data.frame(results_cooperation),digits=3))


# Coefficient Plot (Figure 4A)

mat_h4_short <- mat_h4[,c(1,4,7)]
pre_h4_short <- pre_h4[,c(1,4,7)]

coop_table <- data.frame(Var = rownames(results_cooperation)[c(1,4,7)], 
                         Coef = numeric(length = ncol(mat_h4_short)),SE=numeric(length = ncol(mat_h4_short)))

for(i in 1:ncol(mat_h4_short)){
    var <- mat_h4_short[,i]/sd(mat_h4_short[d$treatment==0,i],na.rm=T)
    coop_table[i,2:3] <- summary(lm(var~d$treatment+pre_h4_short[,i]))$coefficients[2,1:2]
}

coop_table$Upper <- coop_table$Coef+coop_table$SE*1.65
coop_table$Lower <- coop_table$Coef-coop_table$SE*1.65
coop_table$Var <- c("Support for international cooperation","Support for international migration","Interest in international news")
coop_table$Var <- factor(coop_table$Var,levels=c("Interest in international news","Support for international migration","Support for international cooperation"))

pdf("Figures/Figure_4A.pdf", width = 4, height = 1.5)
f4a <- ggplot(coop_table, aes(x = Coef, y = Var)) +
  geom_point(size=2) + 
  scale_x_continuous(limits = c(-.2, .7)) +
  geom_errorbar(aes(xmin = Lower, xmax = Upper),width=0) + 
  geom_vline(xintercept = 0,color="black",linetype="dashed") +
  theme_bw() + theme(text=element_text(family="serif"),legend.position="none",panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(),axis.line = element_line(colour = "black"),
                     axis.text.y=element_text(colour="black"),axis.text.x=element_text(colour="black")) + 
  ylab("") + xlab("") + ggtitle("")
f4a
dev.off()

# CACE Coefficient Plot (Figure 4B)

coop_table <- data.frame(Var = c("Support for international cooperation","Support for international migration","Interest in international news"),
                         Coef = numeric(length =3),SE=numeric(length = 3))

for(i in 1:3){
  std_var <- indexer(cbind(mat_h4_short[,i]))
  coop_table[i,2:3] <- summary(ivreg(std_var~ pre_h4_short[,i] | d$mig | d$treatment))$coefficients[2,1:2]
}
coop_table$Upper <- coop_table$Coef+coop_table$SE*1.65
coop_table$Lower <- coop_table$Coef-coop_table$SE*1.65
coop_table$Var <- factor(coop_table$Var,levels=rev(coop_table$Var))

pdf("Figures/Figure_4B.pdf", width = 4, height = 1.5)
f4b <- ggplot(coop_table, aes(x = Coef, y = Var)) +
  geom_point(size=2) + 
  scale_x_continuous(limits = c(-.5, 3.5)) +
  geom_errorbar(aes(xmin = Lower, xmax = Upper),width=0) + 
  geom_vline(xintercept = 0,color="black",linetype="dashed") + 
  theme_bw() + theme(text=element_text(family="serif"),legend.position="none",axis.text.y=element_text(colour="black"),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(),axis.line = element_line(colour = "black"),
                     axis.text.x=element_text(colour="black")) + 
  ylab("") + xlab("") + ggtitle("")
f4b
dev.off()

pdf("Figures/Figure_4.pdf",width = 4.5,height=3,onefile = FALSE)
ggarrange(f4a,f4b,ncol=1)
dev.off()


## Intercultural Contact ####

vars <- cbind(d$bQ28a,d$bQ28b,d$bQ28c,d$bQ29a,d$bQ29b,d$bQ29c,d$Q51a,d$Q51b,d$Q51c,d$Q52a,d$Q52b,d$Q52c)

colnames(vars) <- c("Pre_MealReligion","Pre_MealEthnic","Pre_MealForeign","Pre_WorkReligion","Pre_WorkEthnic","Pre_WorkForeign",
                    "Post_MealReligion","Post_MealEthnic","Post_MealForeign","Post_WorkReligion","Post_WorkEthnic","Post_WorkForeign")
vars[,1:6] <- matrix((vars[,1:6]=="Never")*5+ (vars[,1:6]=="A few times a year")*4 + (vars[,1:6]=="A few times a month")*3 +
                       (vars[,1:6]=="A few times a week")*2 + (vars[,1:6]=="Almost daily")*1,ncol=6)
vars[,1:6] <- matrix(ifelse(vars[,1:6]==0,NA,vars[,1:6]),ncol=6)
vars <- matrix(as.numeric(vars),ncol=ncol(vars))

# Results Table (Table A.9)

mat_h3 <- cbind(-indexer(vars[,7:12]),6-vars[,7:12])
colnames(mat_h3) <- c("Contact index","Meal w/ non-Christian","Meal w/ non-Mizo","Meal w/ non-Indian","Work w/ non-Christian","Work w/ non-Mizo", "Work w/ non-Indian")
pre_h3 <- cbind(-indexer(vars[,1:6]),6-vars[,1:6])

results_contact <- multiple_results(mat_h3,pre_h3)
print(xtable(data.frame(results_contact),digits=3))


# Raw Results (Figure 5A)

vars <- cbind(vars,vars)

mean_table <- data.frame(Date = character(length=ncol(vars)), Group = character(length=ncol(vars)), Mean =numeric(length=ncol(vars)),SE=numeric(length=ncol(vars)))
mean_table$Group <- rep(c("Treatment","Control"),each=ncol(vars)/2)
mean_table$Date <- rep(rep(c("Pre","Post"),each=ncol(vars)/4),times=2)
mean_table$Var <- rep(c("Meal w/ non-Christian","Meal w/ non-Mizo","Meal w/ non-Indian","Work w/ non-Christian","Work w/ non-Mizo", "Work w/ Non-Indian"),times=4)
se <- function(x) sqrt(var(x)/length(x))

for(i in 1:nrow(mean_table)){
  groupresponses <- vars[d$treatment==ifelse(mean_table$Group[i]=="Treatment",1,0),]
  var <- groupresponses[,i]
  var <- var[!is.na(var)]
  mean_table$Mean[i] <- mean((as.numeric(var)<3))*100
  mean_table$SE[i] <- se((as.numeric(var)<3))*100
}

mean_table$Date <- factor(mean_table$Date,levels=c("Pre","Post"))
mean_table$Var <- factor(mean_table$Var, levels=c("Meal w/ non-Christian","Meal w/ non-Mizo","Meal w/ non-Indian","Work w/ non-Christian","Work w/ non-Mizo", "Work w/ non-Indian"))

mean_table$Activity <- rep(c(rep("Percent sharing a meal with",3),rep("Percent working with",3)),times=4)
mean_table$Activity <- factor(mean_table$Activity, levels=c("Percent working with","Percent sharing a meal with"))
mean_table$Other <- rep(c("Non-Christian","Non-Mizo","Non-Indian"),times=8)
mean_table$Other <- factor(mean_table$Other,levels = c("Non-Indian","Non-Mizo","Non-Christian"))

pdf("Figures/Figure_5A.pdf", width = 5, height = 2.5)
f5a <- ggplot(data=mean_table[mean_table$Date=="Post",], aes(x=Mean, y=Other, color=Group,group=Group)) +
  ylab("") + xlab("")+  labs(color="",fill="") +
  facet_wrap(~Activity,ncol=1) +
  theme(text=element_text(family="serif"),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(),axis.line = element_line(colour = "black"),
        axis.text.y=element_text(colour="black"),axis.text.x=element_text(colour="black"),legend.position = "bottom") +
  scale_fill_grey(start=1,end=0) + scale_colour_grey(start=.6,end=0)  +
  geom_point(stat="identity",size=2) +
  scale_x_continuous(limits = c(0,50))
f5a
dev.off()

# Coefficient Plot (Figure 5B)

contact_table <- data.frame(Var = rownames(results_contact),
                        Coef = numeric(length = ncol(mat_h3)),SE=numeric(length = ncol(mat_h3)))

for(i in 1:ncol(mat_h3)){
  std_var <- indexer(cbind(mat_h3[,i]))
  contact_table[i,2:3] <- summary(lm(std_var~d$treatment+pre_h3[,i]))$coefficients[2,1:2]
}
contact_table$Upper <- contact_table$Coef+contact_table$SE*1.65
contact_table$Lower <- contact_table$Coef-contact_table$SE*1.65
contact_table$Var <- factor(contact_table$Var,levels=rev(contact_table$Var[1:ncol(mat_h3)]))
contact_table$Group <- c("Index",rep("Component",times=6))
contact_table$Group <- factor(contact_table$Group,levels=c("Index","Component"))

pdf("Figures/Figure_5B.pdf", width = 5, height = 2.5)
f5b <- ggplot(contact_table, aes(x = Coef, y = Var,color=Group)) +
  geom_point(size=2) + 
  scale_x_continuous(limits = c(-.2,1)) +
  geom_errorbar(aes(xmin = Lower, xmax = Upper),width=0) + 
  geom_vline(xintercept = 0,color="black",linetype="dashed") + scale_colour_grey(start=0,end=.6) +
  theme_bw() + theme(text=element_text(family="serif"),legend.position="none",panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(),axis.line = element_line(colour = "black"),
                     axis.text.y=element_text(colour="black"),axis.text.x=element_text(colour="black")) + 
  ylab("") + xlab("") + ggtitle("")
f5b
dev.off()

pdf("Figures/Figure_5.pdf",width = 4,height=5,onefile = FALSE)
ggarrange(f5a,f5b,ncol=1)
dev.off()


## Additional Results and Mechanism Tests


## Nationalism and Regional Identity (Table A.13)

vars <- cbind(d$bQ55b,d$bQ27,d$bQ47,d$bQ38b,6-as.numeric(d$Q43b),d$Q34,d$Q36,d$Q37,4-as.numeric(d$Q20c))

colnames(vars) <- c("Pre_Tolerance","Pre_Identity","Pre_AntiMigration","Pre_IndiaNews",
                    "Post_Tolerance","Post_IndianvMizo","Post_Autonomy","Post_AntiMigration","Post_IndiaNews")

vars[,1] <- (vars[,1]=="Very Positive")*5+ (vars[,1]=="Positive")*4 + (vars[,1]=="Neutral" | vars[,1]=="No answer")*3 +
                       (vars[,1]=="Negative")*2 + (vars[,1]=="Very Negative")*1
vars[,2] <- ifelse(vars[,2]=="I see myself as part of the Indian nation",1,0)
vars[,3] <- (vars[,3]=="Strongly disagree")*5+(vars[,3]=="Somewhat disagree")*4+(vars[,3]=="Neither agree nor disagree")*3+
  (vars[,3]=="Somewhat agree")*2+(vars[,3]=="Strongly agree")*1
vars[,4] <- (vars[,4]=="A lot of attention")*1+ (vars[,4]=="A little attention")*2 + (vars[,4]=="No attention at all")*3
vars[,4] <- ifelse(vars[,4]==0,NA,vars[,4])

vars <- matrix(as.numeric(vars),ncol=ncol(vars))
vars <- matrix(ifelse(vars==0,NA,vars),ncol=ncol(vars))

mat_h6 <- cbind(vars[,5:9])
colnames(mat_h6) <- c("View of Mainland Indians","Identify more as Indian than as Mizo",
                      "Mizoram should not be Autonomous","OK with Indians moving to Mizoram",
                      "Interest in Indian Politics")
pre_h6 <- cbind(vars[,1],vars[,2],vars[,3],vars[,3],vars[,4])

results_nationalism <- multiple_results(mat_h6,pre_h6,twosided = T)
print(xtable(data.frame(results_nationalism),digits=3))

## Job Training Effects (Table A.15)

d$uptake <- mat_h1[,2]
table(d$uptake, d$treatment)

training_1 <- lm(mat_h2[d$treatment==0,1]~uptake+age+male+employed+married+edu+st+pre_h2[d$treatment==0,1],data=d[d$treatment==0,])
training_2 <- lm(mat_h4[d$treatment==0,1]~uptake+age+male+employed+married+edu+st+pre_h4[d$treatment==0,1],data=d[d$treatment==0,])
training_3 <- lm(mat_h4[d$treatment==0,4]~uptake+age+male+employed+married+edu+st+pre_h4[d$treatment==0,4],data=d[d$treatment==0,])
training_4 <- lm(mat_h4[d$treatment==0,7]~uptake+age+male+employed+married+edu+st+pre_h4[d$treatment==0,7],data=d[d$treatment==0,])
training_5 <- lm(mat_h4[d$treatment==0,8]~uptake+age+male+employed+married+edu+st+pre_h4[d$treatment==0,8],data=d[d$treatment==0,])
stargazer(training_1,training_2,training_3,training_4,training_5)
#Note that the pre-treatment outcome variables are listed on separate lines in this table, combined in text


## Midline Analysis (Table A.16)

vars_mid <- cbind(6-as.numeric(d$mQ28),2-as.numeric(d$mQ29), 6-as.numeric(d$mQ27),as.numeric(d$mQ26))
vars_mid[vars_mid[,3]==0,3] <- NA # eliminate non-respondents from previous 
mid_h4 <- cbind(indexer(vars_mid[,1:2]),vars_mid[,1:2],indexer(vars_mid[,3:4]),vars_mid[,3:4])
colnames(mid_h4) <- c("Cooperation Index","Trade Improves Lives","Encourage FDI","Migration Index","Migration Improves Lives","Support Migration into India")

results_cooperation <- multiple_results(mid_h4,pre_h4[,1:6])
print(xtable(data.frame(results_cooperation),digits=3))

## Likely Compliers Analysis (Table 2)

main_vars <- cbind(mat_h1[,1],mat_h3[,1],mat_h2[,1],mat_h4[,1],mat_h4[,4],mat_h4[,7],mat_h4[,8])
main_pre <- cbind(pre_h1[,1],pre_h3[,1],pre_h2[,1],pre_h4[,1],pre_h4[,4],pre_h4[,7],pre_h4[,8])
colnames(main_vars) <- c("Moved","Contact","Toleration","Cooperation","Migration","News Interest","World Citizen")
likely <- likely_results_2(main_vars,main_pre,d$likely)
xtable(likely[c(1:3,7,4:6),])
c(sum(!is.na(d$likely)&d$likely==1&d$respond_e==1),sum(!is.na(d$likely)&d$likely==0&d$respond_e==1))#N row at bottom of table
#Note: This code generates two-sided p-values, but hypotheses are one-sided, so p-values are halved in Table 2

# Direct Effects: Blackwell et al. (Table A.17)

d$pre_employed <- ifelse(d$bQ14=="Yes",1,0)
d$post_employed <- ifelse(as.numeric(d$Q3)==1,1,0)

d$pre_wages <- ifelse(d$pre_employed==0,0,as.numeric(d$bQ17))
d$post_wages <- ifelse(d$post_employed==0,0,as.numeric(d$Q5))

seq.g.var <- function(mod.first, mod.direct, med.vars) {
  mat.x <- model.matrix(mod.direct)
  mat.first <- model.matrix(mod.first)
  n <- nrow(mat.x)
  Fhat <- crossprod(mat.x, mat.first)/n
  Fhat[, !(colnames(mat.first) %in% med.vars)] <- 0
  Mhat.inv <- solve(crossprod(mat.first)/n)
  ghat <- t(estfun(mod.direct)) + Fhat %*% Mhat.inv %*% t(estfun(mod.first))
  meat <- crossprod(t(ghat))/n
  bread <- (t(mat.x)%*%mat.x)/n
  vv <- (n/(n-ncol(mat.x)))*(solve(bread) %*% meat %*% solve(bread))/n
  return(vv)
}

new_d <- data.frame(tolerance=mat_h2[,1],pre_tolerance=pre_h2[,1],
                    identity=mat_h4[,8],pre_identity=pre_h4[,8],
                    cooperation=mat_h4[,1],pre_cooperation=pre_h4[,1],
                    migration=mat_h4[,4],pre_migration=pre_h4[,4],
                    interest=mat_h4[,7],pre_interest=pre_h4[,7],
                    treat=d$treatment,wages = d$post_wages,
                    pre_wages=d$pre_wages)

all_results <- data.frame(ATE=numeric(length=5),OLS_SE=numeric(length=5),ACDE=numeric(length=5),ACDE_SE=numeric(length = 5))
rownames(all_results)=c("tolerance","identity","cooperation","migration","interest")


for(i in 1:5){
  v <- new_d[,i*2-1]
  p <- new_d[,i*2]
  baseline <- lm(v ~  treat+p,data=new_d)
  all_results[i,1:2] <- summary(baseline)$coefficients[2,1:2]
  ptbias <- lm(v ~  treat+p+wages,data=new_d)
  first <- lm(v ~ pre_wages + p+treat+wages,data=new_d)
  
  v <- v[rownames(new_d) %in% rownames(model.matrix(first))]
  p <- p[rownames(new_d) %in% rownames(model.matrix(first))]
  new_d_short <- new_d[rownames(new_d) %in% rownames(model.matrix(first)),]
  direct <- lm(I(v - coef(first)["wages"]*wages) ~ treat+p,data=new_d_short)
  direct.vcov <- seq.g.var(first, direct, "wages")
  all_results[i,3:4] <- coeftest(direct, vcov = direct.vcov)[2,1:2]
}
print(xtable(data.frame(round(all_results,2))))


## Heterogeneous Effects (Table A.19)

cov_vars <- cbind(covariates[,c(1:4,6)],ifelse(d$bQ3=="Christian",1,0),d$pre_wages)

het_results <- matrix(NA, nrow = ncol(cov_vars),ncol=ncol(main_vars)-2)
colnames(het_results) <- colnames(main_vars)[3:7]
rownames(het_results) <- c("Age","Gender","Education","Employed","ST","Christian","Wages")
nrow(het_results)

for(i in 1:nrow(het_results)){
  for(j in 1:ncol(het_results)){
    covar <- cov_vars[,i]
    outcome <- main_vars[,j+2]
    het_results[i,j] <- summary(lm(outcome~d$treatment*covar))$coefficients[4,3]
  }
}
print(xtable(het_results))


## Egami et al Heterogeneous Effects Analaysis (Figure A5)

d$tolerance <- mat_h2[,1]
d$identity <- mat_h4[,8]
d$cooperation <- mat_h4[,1]
d$migration <- mat_h4[,4]
d$interest <- mat_h4[,7]
d$christian <- ifelse(d$bQ3=="Christian",1,0)

main_vars <- cbind(mat_h2[,1],mat_h4[,8],mat_h4[,1],mat_h4[,4],mat_h4[,7])

heterogeneous_effects <- data.frame(Tolerance=numeric(length=nrow(d)),Identity=numeric(length=nrow(d)),
                                    Cooperation=numeric(length=nrow(d)),Migration=numeric(length=nrow(d)),
                                    NewsInterest=numeric(length=nrow(d)))

covariates <- c("age", "male", "edu","employed", "married","st","christian") 

for(i in 1:ncol(heterogeneous_effects)){
  v <- main_vars[,i]
  sate_est <- summary(lm(main_vars[,i]~treatment,data = d))$coefficients[2,1:2]
  if(i==1) exr_out <- exr(outcome = "tolerance", treatment = "treatment", covariates = c("age", "male", "edu","employed", "married", "st","christian"), data = d,sate_estimate = sate_est) 
  if(i==2) exr_out <- exr(outcome = "identity", treatment = "treatment", covariates = c("age", "male", "edu","employed", "married", "st","christian"), data = d,sate_estimate = sate_est) 
  if(i==3) exr_out <- exr(outcome = "cooperation", treatment = "treatment", covariates = c("age", "male", "edu","employed", "married", "st","christian"), data = d,sate_estimate = sate_est) 
  if(i==4) exr_out <- exr(outcome = "migration", treatment = "treatment", covariates = c("age", "male", "edu","employed", "married", "st","christian"), data = d,sate_estimate = sate_est) 
  if(i==5) exr_out <- exr(outcome = "interest", treatment = "treatment", covariates = c("age", "male", "edu","employed", "married", "st","christian"), data = d,sate_estimate = sate_est) 
  tau_estimates <- exr_out$tau
  heterogeneous_effects[1:length(tau_estimates),i] <- matrix(tau_estimates,ncol=1)
}
heterogeneous_effects[heterogeneous_effects==0] <- NA
table(heterogeneous_effects$Tolerance,useNA = "always")

main_var_names <- c("Intercultural Tolerance","Citizen of the World","Support for Intl Cooperation","Support for Intl Migration","Interest in Intl News")
mat_he <- NULL
for(i in 1:ncol(heterogeneous_effects)){
  mat_he <- rbind(mat_he, cbind(main_var_names[i],heterogeneous_effects[!is.na(heterogeneous_effects[,i]),i]))
}
heterogeneous_effects <- data.frame(mat_he)
colnames(heterogeneous_effects) <- c("Variable","Tau")
heterogeneous_effects$Variable <- factor(heterogeneous_effects$Variable,levels=main_var_names)
heterogeneous_effects[,2] <- as.numeric(heterogeneous_effects[,2])

pdf("Figures/Figure_A5.pdf", width = 4, height = 4)
ggplot(heterogeneous_effects, aes(x=Tau)) + facet_wrap(~Variable,ncol=1) +
  geom_histogram(binwidth = .025) + theme(text=element_text(family="serif")) + geom_vline(xintercept = 0)+
  scale_x_continuous(limits = c(-.2,.8)) + xlab("Estimated Treatment Effects") + ylab("")
dev.off()


## Household Analysis (Table 3)

vars <- cbind(d$bQ54,d$bQ55d,d$bQ55e,d$bQ55f,d$bQ55c,d$bQ50,d$bQ48,d$hQ42,d$hQ43D,d$hQ43E,d$hQ43F,d$hQ43C,d$hQ41,d$hQ40)
colnames(vars) <- c("Pre_Mizo","Pre_Bangladesh","Pre_Pakistan","Pre_MiddleEast","Pre_European","Pre_MigrationGood","Pre_BangladeshMigration",
                    "Post_Mizo","Post_Bangladesh","Post_Pakistan","Post_MiddleEast","Post_European","Post_MigrationGood","Post_BangladeshMigration")
vars[,1] <- (vars[,1]=="Yes")*1+(vars[,1]=="Only in some cases")*.5+(vars[,1]=="No")*0
vars[,2:5] <- matrix((vars[,2:5]=="Very Positive")*5+ (vars[,2:5]=="Positive")*4 + (vars[,2:5]=="Neutral" | vars[,2:5]=="No answer")*3 +
                       (vars[,2:5]=="Negative")*2 + (vars[,2:5]=="Very Negative")*1,ncol=4)
vars[,6] <- (vars[,6]=="Hurts them a lot")*1+ (vars[,6]=="Hurts them a little")*2 + (vars[,6]=="Does not affect them much")*3 +
                     (vars[,6]=="Improves them a little")*4 + (vars[,6]=="Improves them a lot")*5
vars[,7] <- matrix((vars[,7]=="Strongly disagree")*5+ (vars[,7]=="Somewhat disagree")*4 + (vars[,7]=="Neither agree nor disagree")*3 +
                     (vars[,7]=="Somewhat agree")*2 + (vars[,7]=="Strongly agree")*1)
vars[,8] <- ifelse(as.numeric(vars[,8])==1,1,0)
vars[,9:12] <- 6-as.numeric(vars[,9:12])
vars[,13] <- 6-as.numeric(vars[,13])

vars <- matrix(as.numeric(vars),ncol=ncol(vars))

mat_hh <- cbind(indexer(vars[,8:12]),vars[,8:12],indexer(vars[,13:14]),vars[,13:14])
colnames(mat_hh) <- c("Tolerance Index","OK to Marry Non-Mizo?","View of Bangladeshis","View of Pakistanis","View of Middle Easterners","View of Europeans",
                      "Migration Index","Migration Improves Lives","Support Migration into India")
pre_hh <- cbind(indexer(vars[,1:5]),vars[,1:5],indexer(vars[,6:7]),vars[,6:7])

results_hh <- multiple_results(mat_hh,pre_hh,twosided = F)
print(xtable(data.frame(results_hh),digits=3))

# compare same generation vs. parents' generation
summary(lm(mat_hh[,1]~treatment*same_gen,data=d))


## Attrition Bias Checks ####

d$pre_income <- (d$bQ21=="Less than Rs. 5,000") + (d$bQ21=="Rs. 5,001 - Rs. 10,000") *2 +
  (d$bQ21=="Rs. 10,001 - Rs. 20,000") *3 + (d$bQ21=="Rs. 20,001 - Rs. 30,000") *4 +
  (d$bQ21=="Rs. 30,001 - Rs. 40,000") *5 + (d$bQ21=="Rs. 40,001 - Rs. 50,000") *6 +
  (d$bQ21=="Rs. 50,000 - Rs. 100,000") *7 + (d$bQ21=="Rs. 100,000 and above") *8

respond_e2 <- c(d$respond_e,0,0,0)
respond_h2 <- c(d$respond_h,0,0,0)
treat2 <- c(d$treatment,1,1,1)

## Response Rates by Treatment Group (Table A.3)

t.test(respond_e2[treat2==1],respond_e2[treat2==0]) # endline
t.test(respond_h2[treat2==1],respond_h2[treat2==0]) # household

t_hat_e <- mean(respond_e2[treat2==1],na.rm = T)-mean(respond_e2[treat2==0],na.rm = T)
t_hat_h <- mean(respond_h2[treat2==1],na.rm = T)-mean(respond_h2[treat2==0],na.rm = T)
t_ests_e <- t_ests_h <- rep(NA,reps)
for(i in 1:reps){
  t_ests_e[i] <- mean(respond_e2[treat_hyp[,i]==1],na.rm=T) - mean(respond_e2[treat_hyp[,i]==0],na.rm=T) 
  t_ests_h[i] <- mean(respond_h2[treat_hyp[,i]==1],na.rm=T) - mean(respond_h2[treat_hyp[,i]==0],na.rm=T) 
}
mean(abs(t_hat_e)<=abs(t_ests_e))
mean(abs(t_hat_h)<=abs(t_ests_h))

#Predictors of attrition (Table A.4)

# endline
attrition_e1 <- lm(d$respond_e ~ d$age+d$edu+d$st+d$employed+d$married+d$male+d$english)
attrition_e2 <- lm(d$respond_e ~ d$age+d$edu+d$st+d$employed+d$married+d$male+d$english+d$pre_income+pre_h2[,1]+pre_h4[,1])
summary(attrition_e1) # See F-stat p-value
summary(attrition_e2) # See F-stat p-value

# household
attrition_h1 <- lm(d$respond_h ~ d$age+d$edu+d$st+d$employed+d$married+d$male+d$english)
attrition_h2 <- lm(d$respond_h ~ d$age+d$edu+d$st+d$employed+d$married+d$male+d$english+d$pre_income+pre_h2[,1]+pre_h4[,1])
summary(attrition_h1) # See F-stat p-value
summary(attrition_h2) # See F-stat p-value

stargazer(attrition_e1,attrition_e2,attrition_h1,attrition_h2)#Table A.4

## Balance (Table A.2) ##

d$pre_tolerance <- pre_h2[,1]
d$pre_migration <- pre_h4[,1]

#F-Test on respondents' baseline covariates among all subjects (nothing is predictive)
#(still nothing is predictive)
balance.model1 <- lm(treatment ~ age+male+edu+employed+st+married+english+
                       pre_income+pre_tolerance+pre_migration,data=d)
summary(balance.model1)
f_1 <- summary(balance.model1)$fstatistic[1]
f_reps <- rep(NA,reps)
for(i in 1:reps){
  newmod <- lm(treat_hyp[,i] ~ age+male+edu+employed+st+married+english+
                 pre_income+pre_tolerance+pre_migration,data=d)
  f_reps[i]<-summary(newmod)$fstatistic[1]
}
mean(f_reps>=f_1)


#F-Test on respondents' baseline covariates among endline responders (still nothing is predictive)
balance.model2 <- lm(treatment ~ age+male+edu+employed+st+married+english+
                       pre_income+pre_tolerance+pre_migration,data=d[d$respond_e==1,])
summary(balance.model2)
f_2 <- summary(balance.model2)$fstatistic[1]
f_reps <- rep(NA,reps)
for(i in 1:reps){
  newmod <- lm(treat_hyp[d$respond_e==1,i] ~ age+male+edu+employed+st+married+english+
                 pre_income+pre_tolerance+pre_migration,data=d[d$respond_e==1,])
  f_reps[i]<-summary(newmod)$fstatistic[1]
}
mean(f_reps>=f_2)

#F-Test on respondents' baseline covariates among household responders
balance.model4 <- lm(treatment ~ age+male+edu+employed+st+married+english+
                       pre_income+pre_tolerance+pre_migration,data=d[d$respond_h==1,])
summary(balance.model4)
f_4 <- summary(balance.model4)$fstatistic[1]
f_reps <- rep(NA,reps)
for(i in 1:reps){
  newmod <- lm(treat_hyp[d$respond_h==1,i] ~ age+male+edu+employed+st+married+english+
                 pre_income+pre_tolerance+pre_migration,data=d[d$respond_h==1,])
  f_reps[i]<-summary(newmod)$fstatistic[1]
}
mean(f_reps>=f_4)

stargazer(balance.model1,balance.model2,balance.model4)

#unlink("Quant_LogFile.txt")

