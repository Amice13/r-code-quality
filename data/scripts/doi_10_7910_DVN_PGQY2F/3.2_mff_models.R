#############################################################################################
# Replication Data for: Proksch, Wratil, Wäckerle. (2018). Testing the Validity of Automatic
# Speech Recognition for Political Text Analysis. Political Analysis, forthcoming.
#############################################################################################

#########################
# This script runs the models on the MFF data
# It produces Figures 5 and 6 and Table 3 in the paper.
# It also produces Figures A12, A13, A14, A15, A16, A17 and A18 in the Appendix

#############################################################
# Intro
library(tidyverse)#This code was built with tidyverse version 1.2.1
library(austin)#This code was built with austin version 0.3.0
library(quanteda)#This code was built with quanteda version 1.3.13
library(MCMCpack)#This code was built with MCMCpack version 1.4-4
library(rjags)#This code was built with rjags version 4-8
library(coda)#This code was built with coda version 0.19-2
library(extrafont)#This code was built with extrafont version 0.17
library(xtable)

# Check if all packages are the ones we used
if(packageVersion("tidyverse")=="1.2.1"&
   packageVersion("austin")=="0.3.0"&
   packageVersion("quanteda")=="1.3.13"&
   packageVersion("MCMCpack")=="1.4-4"&
   packageVersion("rjags")=="4-8"&
   packageVersion("coda")=="0.19-2"&
   packageVersion("extrafont")=="0.17"){
  paste("Your packages seem good to go")
}else{
  paste("Warning: Some or all of the package versions you are using are not exactly the ones we used for this paper. If the code doesn't replicate, this might be the reason.")
}

load(file = "generated_data/mffdata.RData")

speechdata.joined=speechdata
#############################################################
# Wordfish for all debates

DFM.mff<-dfm(mff.pre,remove_punct=T, groups = "actormeeting",remove_numbers=T)
DFM.mff=dfm_trim(DFM.mff,min_docfreq = 10)
wf.mff<-wordfish(as.wfm(DFM.mff),verbose=T)

sum.wf.mff=data.frame(theta=wf.mff$theta,
                      lower=wf.mff$theta-1.96*wf.mff$se.theta,
                      upper=wf.mff$theta+1.96*wf.mff$se.theta,
                      index=wf.mff$docs,stringsAsFactors = F)
#save wordfish estimates in speechdata
speechdata.joined=left_join(speechdata,sum.wf.mff)

#############################################################
# Extract relevant budget information and add to speechdata
names(budget_contr)[grepl("year",names(budget_contr))]="Year"
names(budget_contr)[names(budget_contr)=="government"]="actor"
budget_contr$eu_cont_pc=as.numeric(budget_contr$eu_cont_pc)
budget_contr_sum=data.frame(country=unique(budget_contr$actor),stringsAsFactors = F)

budget_contr_eu_cont_pc=aggregate(budget_contr$eu_cont_pc[budget_contr$Year%in%c(2007,2008,2009,2010,2011,2012,2013)],
                                  by=list(budget_contr$actor[budget_contr$Year%in%c(2007,2008,2009,2010,2011,2012,2013)]),mean,na.rm=T)
names(budget_contr_eu_cont_pc)=c("country","eu_cont_pc")

budget_contr_eu_cont_total=aggregate(budget_contr$eu_cont_total[budget_contr$Year%in%c(2007,2008,2009,2010,2011,2012,2013)],
                                     by=list(budget_contr$actor[budget_contr$Year%in%c(2007,2008,2009,2010,2011,2012,2013)]),mean,na.rm=T)
names(budget_contr_eu_cont_total)=c("country","eu_cont_total")

budget_contr_eu_cont_gdp=aggregate(budget_contr$eu_cont_gdp[budget_contr$Year%in%c(2007,2008,2009,2010,2011,2012,2013)],
                                   by=list(budget_contr$actor[budget_contr$Year%in%c(2007,2008,2009,2010,2011,2012,2013)]),mean,na.rm=T)
names(budget_contr_eu_cont_gdp)=c("country","eu_cont_gdp")

budget_contr_sum=left_join(budget_contr_sum,budget_contr_eu_cont_pc)
budget_contr_sum=left_join(budget_contr_sum,budget_contr_eu_cont_total)
budget_contr_sum=left_join(budget_contr_sum,budget_contr_eu_cont_gdp)
names(budget_contr_sum)[names(budget_contr_sum)=="country"]="actor"
speechdata.joined=left_join(speechdata.joined,budget_contr_sum)

#############################################################
# Build Matrix to use in IRT and Wordshoal, Wordeel models

#Calculate Wordfish seperately for each debate and save in matrix
seperate.wf.matrix.for.irt=data.frame(matrix(nrow=length(unique(speechdata.joined$actor)),ncol=length(unique(speechdata.joined$meeting))))
names(seperate.wf.matrix.for.irt)=unique(speechdata.joined$meeting)
row.names(seperate.wf.matrix.for.irt)=unique(speechdata.joined$actor)
i=3125
for(i in names(seperate.wf.matrix.for.irt)){
  DFM.mff<-dfm(corpus_subset(mff.pre,no_mff_meeting==i),remove_punct=T, groups = "actormeeting",remove_numbers=T)
  wf.mff<-wordfish(as.wfm(DFM.mff),verbose=T)
  wf.mff.dat<-summary(wf.mff)
  sum.wf.mff=data.frame(theta=predict(wf.mff,interval="confidence"))
  names(sum.wf.mff)=c("theta","se","lower","upper")
  info=str_split(row.names(sum.wf.mff)," ")
  info <- do.call(rbind,info)
  sum.wf.mff$actor=info[,2]
  sum.wf.mff$meeting=info[,1]
  for(j in row.names(seperate.wf.matrix.for.irt)){
    if(j%in%sum.wf.mff$actor){
      seperate.wf.matrix.for.irt[row.names(seperate.wf.matrix.for.irt)==j,names(seperate.wf.matrix.for.irt)==i]=sum.wf.mff$theta[sum.wf.mff$actor==j]
    }
  }
}

save(speechdata.joined,mff.pre,budget_contr,
     seperate.wf.matrix.for.irt,
     file="generated_data/datasets_mff_final.RData")

################################################################
# RUN MODELS
################################################################

load(file = "generated_data/datasets_mff_final.RData")

##################################################
#Bayesian dynamic factor model
##################################################

#prepare data (anchor by UK and HU)
data.aqua <- seperate.wf.matrix.for.irt
data.aqua <- rbind(data.aqua[which(row.names(data.aqua)=="UK"),], data.aqua[-which(row.names(data.aqua)=="UK"),])
data.aqua <- rbind(data.aqua[which(row.names(data.aqua)=="HU"),], data.aqua[-which(row.names(data.aqua)=="HU"),])

#JAGS code

dynamic_factor_model <- 'model{
#loop through actors
for(i in 1:nactors){
#loop through debates
for(j in 1:ndebate){
Y[i,j] ~ dnorm(mu[i,j], tau[i])
mu[i,j] <- alpha[j] + beta[j] * theta[actor[i], period[j]]
}
change[i] <- theta[i,2] - theta[i,1]
}
#set normal priors on betas and alphas
for(j in 1:ndebate){
beta[j] ~ dnorm(0, 4)
alpha[j] ~ dnorm(0, 4)
}
#set priors on thetas (fix space with two actors)
theta[1, 1] ~ dnorm(1, 1)
theta[2, 1] ~ dnorm(-1, 1)
for(t in 2:nperiods){
theta[1, t] ~ dnorm(theta[1, t-1], tau.evol)
theta[2, t] ~ dnorm(theta[2, t-1], tau.evol)
}
for(c in 3:nactors){
theta[c, 1] ~ dnorm(0, 1)
for(t in 2:nperiods){
theta[c, t] ~ dnorm(theta[c, t-1], tau.evol)
}
}
#set prior on tau
for(i in 1:nactors){
tau[i] ~ dgamma(1, 1)
}
#set priors on evolution parameters for thetas
tau.evol <- 1
}
'

factor_model <- 'model{
#loop through actors
for(i in 1:nactors){
#loop through debates
for(j in 1:ndebate){
Y[i,j] ~ dnorm(mu[i,j], tau[i])
mu[i,j] <- alpha[j] + beta[j] * theta[actor[i]]
}
}
#set normal priors on betas and alphas
for(j in 1:ndebate){
beta[j] ~ dnorm(0, 4)
alpha[j] ~ dnorm(0, 4)
}
#set priors on thetas (fix space with two actors)
theta[1] ~ dnorm(1, 1)
theta[2] ~ dnorm(-1, 1)
for(c in 3:nactors){
theta[c] ~ dnorm(0, 1)
}
#set prior on tau
for(i in 1:nactors){
tau[i] ~ dgamma(1, 1)
}
}
'

#################################
# Dynamic Factor Model aka Wordeel

#set parameters
jags.data.eel <- list(
  Y = as.matrix(data.aqua),
  nactors = 28,
  ndebate = 14,
  nperiods = 2,
  period = c(1,1,1,1,1,1,1,1,1,2,2,2,2,2),
  actor = as.numeric(c(1:28))
)

jags.data.shoal <- list(
  Y = as.matrix(data.aqua),
  nactors = 28,
  ndebate = 14,
  actor = as.numeric(c(1:28))
)

###########################
#fit in JAGS

model.jags.eel <- jags.model(file=textConnection(dynamic_factor_model), data = jags.data.eel, n.chains = 1, inits=list(.RNG.name="base::Wichmann-Hill",.RNG.seed=1711))
posterior.jags.eel <- jags.samples(model.jags.eel, c("theta","beta","alpha", "tau.evol", "change"), n.iter=1000000, thin = 100, progress.bar="text")
posterior.coda.eel <- coda.samples(model.jags.eel, c("theta","beta","alpha", "tau.evol", "change"), n.iter=1000000, thin = 100, progress.bar="text")

#################################
# Factor Model aka Wordshoal

###########################
#fit in JAGS
model.jags.shoal <- jags.model(file=textConnection(factor_model), data = jags.data.shoal, n.chains = 1, inits=list(.RNG.name="base::Wichmann-Hill",.RNG.seed=1711))
posterior.jags.shoal <- jags.samples(model.jags.shoal, c("theta","beta","alpha"), n.iter=1000000, thin = 100, progress.bar="text")
posterior.coda.shoal <- coda.samples(model.jags.shoal, c("theta","beta","alpha"), n.iter=1000000, thin = 100, progress.bar="text")

###################################
# Results for Eel

sum.jags.eel=summary(posterior.coda.eel)[[1]][,1]
sum.jags.eel.beta=data.frame(means=sum.jags.eel[grepl(names(sum.jags.eel),pattern="beta")])
sum.jags.eel.beta$debate=as.numeric(gsub("\\D","",row.names(sum.jags.eel.beta)))

sum.jags.eel.theta=data.frame(means_period1=sum.jags.eel[grepl(names(sum.jags.eel),pattern="theta")&
                                                           grepl(names(sum.jags.eel),pattern=",1")],
                              means_period2=sum.jags.eel[grepl(names(sum.jags.eel),pattern="theta")&
                                                           grepl(names(sum.jags.eel),pattern=",2")],
                              change=sum.jags.eel[grepl(names(sum.jags.eel),pattern="change")])

mff.eel=HPDinterval(posterior.coda.eel[[1]],prob=.9)

mff.eel.thetas=data.frame(lower_period1=mff.eel[grepl(row.names(mff.eel),pattern = "theta")&grepl(row.names(mff.eel),pattern = ",1"),"lower"],
                          upper_period1=mff.eel[grepl(row.names(mff.eel),pattern = "theta")&grepl(row.names(mff.eel),pattern = ",1"),"upper"],
                          lower_period2=mff.eel[grepl(row.names(mff.eel),pattern = "theta")&grepl(row.names(mff.eel),pattern = ",2"),"lower"],
                          upper_period2=mff.eel[grepl(row.names(mff.eel),pattern = "theta")&grepl(row.names(mff.eel),pattern = ",2"),"upper"],
                          lower_change=mff.eel[grepl(row.names(mff.eel),pattern = "change"),"lower"],
                          upper_change=mff.eel[grepl(row.names(mff.eel),pattern = "change"),"upper"])
mff.eel.thetas$actor=row.names(data.aqua)
mff.eel.thetas$actornumber=1:nrow(data.aqua)

sum.jags.eel.theta$actor=row.names(data.aqua)
sum.jags.eel.theta$actornumber=1:nrow(data.aqua)

mff.eel.thetas=left_join(mff.eel.thetas,sum.jags.eel.theta)

mff.eel.betas=data.frame(mff.eel[grepl(row.names(mff.eel),pattern = "beta"),])
mff.eel.betas$debate=as.numeric(gsub("\\D","",row.names(mff.eel.betas)))
mff.eel.betas$lower.abs=abs(mff.eel.betas$lower)
mff.eel.betas$upper.abs=abs(mff.eel.betas$upper)
mff.eel.betas=left_join(mff.eel.betas,sum.jags.eel.beta)

mff.eel.betas$lower[mff.eel.betas$debate==8]=mff.eel.betas$lower[mff.eel.betas$debate==8]*-1
mff.eel.betas$upper[mff.eel.betas$debate==8]=mff.eel.betas$upper[mff.eel.betas$debate==8]*-1
mff.eel.betas$means[mff.eel.betas$debate==8]=mff.eel.betas$means[mff.eel.betas$debate==8]*-1
mff.eel.betas$lower[mff.eel.betas$debate==7]=mff.eel.betas$lower[mff.eel.betas$debate==7]*-1
mff.eel.betas$upper[mff.eel.betas$debate==7]=mff.eel.betas$upper[mff.eel.betas$debate==7]*-1
mff.eel.betas$means[mff.eel.betas$debate==7]=mff.eel.betas$means[mff.eel.betas$debate==7]*-1
mff.eel.betas$lower[mff.eel.betas$debate==14]=mff.eel.betas$lower[mff.eel.betas$debate==14]*-1
mff.eel.betas$upper[mff.eel.betas$debate==14]=mff.eel.betas$upper[mff.eel.betas$debate==14]*-1
mff.eel.betas$means[mff.eel.betas$debate==14]=mff.eel.betas$means[mff.eel.betas$debate==14]*-1
mff.eel.betas$lower[mff.eel.betas$debate==13]=mff.eel.betas$lower[mff.eel.betas$debate==13]*-1
mff.eel.betas$upper[mff.eel.betas$debate==13]=mff.eel.betas$upper[mff.eel.betas$debate==13]*-1
mff.eel.betas$means[mff.eel.betas$debate==13]=mff.eel.betas$means[mff.eel.betas$debate==13]*-1

debate_data_eel=data.frame(debate=mff.eel.betas$debate,
                           meeting=names(seperate.wf.matrix.for.irt))

debate_data_eel=left_join(debate_data_eel,mff.eel.betas)
debate_data_eel<-debate_data_eel%>%
  mutate(
    Date=case_when(
      meeting==3125 ~ "2011-11-15",
      meeting==3132 ~ "2011-12-05",
      meeting==3143 ~ "2012-01-27",
      meeting==3158 ~ "2012-03-26",
      meeting==3160 ~ "2012-04-24",
      meeting==3168 ~ "2012-05-29",
      meeting==3180 ~ "2012-06-26",
      meeting==3184 ~ "2012-07-24",
      meeting==3187 ~ "2012-09-24",#cutoff
      meeting==3235 ~ "2013-04-22",
      meeting==3240 ~ "2013-05-21",
      meeting==3368 ~ "2015-02-15",
      meeting==3484 ~ "2016-09-20",
      meeting==3494 ~ "2016-10-18",
      TRUE ~ "Other"
    )
  )

country_data_eel=mff.eel.thetas

agg.contr.gdp=aggregate(speechdata.joined$eu_cont_gdp,by=list(speechdata.joined$actor),mean)
names(agg.contr.gdp)=c("actor","eu_cont_gdp")
country_data_eel=left_join(country_data_eel,agg.contr.gdp)

geweke_eel <- unlist(geweke.diag(posterior.coda.eel))
geweke_eel.dat=data.frame(gew=geweke_eel[!grepl("change",names(geweke_eel))&!grepl("frac",names(geweke_eel))],
                          names=names(geweke_eel[!grepl("change",names(geweke_eel))&!grepl("frac",names(geweke_eel))]))
geweke_eel.dat=geweke_eel.dat%>%filter(!is.na(gew))

prop.table(table(geweke_eel.dat$gew>2|geweke_eel.dat$gew< -2))
table(geweke_eel.dat$gew>2|geweke_eel.dat$gew< -2)

geweke_shoal <- unlist(geweke.diag(posterior.coda.shoal))
geweke_shoal.dat=data.frame(gew=geweke_shoal,
                            names=names(geweke_shoal))
geweke_shoal.dat=geweke_shoal.dat%>%filter(!is.na(gew))

# This is Figure A12 in Appendix 13
geweke_shaol_graph=ggplot(geweke_shoal.dat) + 
  geom_histogram(binwidth=0.2,aes(x=gew,y=..density..), position="identity") + 
  geom_density(aes(x=gew,y=..density..))+
  labs(x="Geweke Statistics",y="Density")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text=element_text(size=12,colour="black",family="Verdana"),
        axis.title=element_text(size=12,family="Verdana"),
        legend.position="none")+
  scale_x_continuous(limits = c(-4,4))
pdf("graphs_paper/Figure_A12.pdf", width = 6, height = 4) # Open a new pdf file
geweke_shaol_graph
dev.off()

# This is Figure A13 in Appendix 13
geweke_eel_graph=ggplot(geweke_eel.dat) + 
  geom_histogram(binwidth=0.2,aes(x=gew,y=..density..), position="identity") + 
  geom_density(aes(x=gew,y=..density..))+
  labs(x="Geweke Statistics",y="Density")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text=element_text(size=12,colour="black",family="Verdana"),
        axis.title=element_text(size=12,family="Verdana"),
        legend.position="none")+
  scale_x_continuous(limits = c(-4,4))
pdf("graphs_paper/Figure_A13.pdf", width = 6, height = 4) # Open a new pdf file
geweke_eel_graph
dev.off()

prop.table(table(geweke_shoal.dat$gew>2|geweke_shoal.dat$gew< -2))
table(geweke_shoal.dat$gew>2|geweke_shoal.dat$gew< -2)

###################################
# Results for Shoal

sum.jags.shoal=summary(posterior.coda.shoal)[[1]][,1]
sum.jags.shoal.beta=data.frame(means=sum.jags.shoal[grepl(names(sum.jags.shoal),pattern="beta")])
sum.jags.shoal.beta$debate=as.numeric(gsub("\\D","",row.names(sum.jags.shoal.beta)))
sum.jags.shoal.theta=data.frame(means=sum.jags.shoal[grepl(names(sum.jags.shoal),pattern="theta")])
mff.shoal=HPDinterval(posterior.coda.shoal[[1]],prob=.9)

mff.shoal.thetas=data.frame(mff.shoal[grepl(row.names(mff.shoal),pattern = "theta")&!grepl(row.names(mff.shoal),pattern = "tau"),])
mff.shoal.thetas$actor=row.names(data.aqua) 
sum.jags.shoal.theta$actor=row.names(data.aqua)
mff.shoal.thetas=left_join(mff.shoal.thetas,sum.jags.shoal.theta)

mff.betas.shoal=data.frame(mff.shoal[grepl(row.names(mff.shoal),pattern = "beta"),])
mff.betas.shoal$debate=as.numeric(gsub("\\D","",row.names(mff.betas.shoal)))
mff.betas.shoal$lower.abs=abs(mff.betas.shoal$lower)
mff.betas.shoal$upper.abs=abs(mff.betas.shoal$upper)
mff.betas.shoal=left_join(mff.betas.shoal,sum.jags.shoal.beta)

mff.betas.shoal$lower[mff.betas.shoal$debate==8]=mff.betas.shoal$lower[mff.betas.shoal$debate==8]*-1
mff.betas.shoal$upper[mff.betas.shoal$debate==8]=mff.betas.shoal$upper[mff.betas.shoal$debate==8]*-1
mff.betas.shoal$means[mff.betas.shoal$debate==8]=mff.betas.shoal$means[mff.betas.shoal$debate==8]*-1
mff.betas.shoal$lower[mff.betas.shoal$debate==7]=mff.betas.shoal$lower[mff.betas.shoal$debate==7]*-1
mff.betas.shoal$upper[mff.betas.shoal$debate==7]=mff.betas.shoal$upper[mff.betas.shoal$debate==7]*-1
mff.betas.shoal$means[mff.betas.shoal$debate==7]=mff.betas.shoal$means[mff.betas.shoal$debate==7]*-1
mff.betas.shoal$lower[mff.betas.shoal$debate==14]=mff.betas.shoal$lower[mff.betas.shoal$debate==14]*-1
mff.betas.shoal$upper[mff.betas.shoal$debate==14]=mff.betas.shoal$upper[mff.betas.shoal$debate==14]*-1
mff.betas.shoal$means[mff.betas.shoal$debate==14]=mff.betas.shoal$means[mff.betas.shoal$debate==14]*-1
mff.betas.shoal$lower[mff.betas.shoal$debate==13]=mff.betas.shoal$lower[mff.betas.shoal$debate==13]*-1
mff.betas.shoal$upper[mff.betas.shoal$debate==13]=mff.betas.shoal$upper[mff.betas.shoal$debate==13]*-1
mff.betas.shoal$means[mff.betas.shoal$debate==13]=mff.betas.shoal$means[mff.betas.shoal$debate==13]*-1

debate_data_shoal=data.frame(debate=mff.betas.shoal$debate,
                             meeting=names(seperate.wf.matrix.for.irt))

debate_data_shoal=left_join(debate_data_shoal,mff.betas.shoal)
debate_data_shoal<-debate_data_shoal%>%
  mutate(
    Date=case_when(
      meeting==3107 ~ "2011-07-18",
      meeting==3125 ~ "2011-11-15",
      meeting==3132 ~ "2011-12-05",
      meeting==3143 ~ "2012-01-27",
      meeting==3158 ~ "2012-03-26",
      meeting==3160 ~ "2012-04-24",
      meeting==3168 ~ "2012-05-29",
      meeting==3180 ~ "2012-06-26",
      meeting==3184 ~ "2012-07-24",
      meeting==3187 ~ "2012-09-24",
      meeting==3235 ~ "2013-04-22",
      meeting==3240 ~ "2013-05-21",
      meeting==3368 ~ "2015-02-15",
      meeting==3484 ~ "2016-09-20",
      meeting==3494 ~ "2016-10-18",
      meeting==3499 ~ "2016-11-15",
      meeting==3511 ~ "2016-12-13",
      TRUE ~ "Other"
    )
  )

country_data_shoal=mff.shoal.thetas
country_data_shoal=left_join(country_data_shoal,agg.contr.gdp)

#####################################################
# Betas from Shoal models

#########
# This is Figure A14 in Appendix 14
graph.betas.shoal<-debate_data_shoal%>%
  ggplot(mapping=aes(x=as.Date(Date), y=lower, ymin=lower, ymax=upper))+
  geom_errorbar(width=0.2, size=1)+
  geom_point(aes(y=means))+
  geom_smooth(aes(y=means),se=F)+
  labs(x="",y="Beta")+
  theme_bw()+
  geom_hline(yintercept=0,lty=2)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text=element_text(size=12,colour="black",family="Verdana"),
        axis.title=element_text(size=12,family="Verdana"),
        legend.position="none")
graph.betas.shoal

pdf("graphs_paper/Figure_A14.pdf",width = 6,height = 4) # Open a new pdf file
graph.betas.shoal
dev.off()

#####################################################
# Betas from Eel models

#########
# This is Figure A15 in Appendix 14
graph.betas.eel<-debate_data_eel%>%
  ggplot(mapping=aes(x=as.Date(Date), y=lower, ymin=lower, ymax=upper))+
  geom_errorbar(width=0.2, size=1)+
  geom_point(aes(y=means))+
  geom_smooth(aes(y=means),se=F)+
  labs(x="",y="Beta")+
  theme_bw()+
  geom_hline(yintercept=0,lty=2)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text=element_text(size=12,colour="black",family="Verdana"),
        axis.title=element_text(size=12,family="Verdana"),
        legend.position="none")
graph.betas.eel

pdf("graphs_paper/Figure_A15.pdf",width = 6,height = 4) # Open a new pdf file
graph.betas.eel
dev.off()

# in relation to contribution 
#########
# This is Figure 6 in the paper
graph.actor.eel.contribution<-country_data_eel%>%
  filter(actor!="COM")%>%
  ggplot(mapping=aes(x=eu_cont_gdp, y=change,label = actor))+
  geom_text(size=5)+
  geom_smooth(method='lm',formula=y~x,se=F,color="black")+
  labs(x="Receipts from EU budget (% of GDP)",y="Change in position estimates")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text=element_text(size=18,colour="black",family="Verdana"),
        axis.title=element_text(size=18,family="Verdana"),
        legend.position="none")
graph.actor.eel.contribution

cor(country_data_eel$change[country_data_eel$actor!="COM"],country_data_eel$eu_cont_gdp[country_data_eel$actor!="COM"])

pdf("graphs_paper/Figure_6.pdf", width = 12, height = 8) # Open a new pdf file
graph.actor.eel.contribution
dev.off()

#########
# This is Figure A18 in Appendix 16
graph.actor.shoal.contribution<-country_data_shoal%>%
  filter(actor!="COM")%>%
  ggplot(mapping=aes(x=eu_cont_gdp, y=means,label = actor))+
  geom_text(size=5)+
  geom_smooth(method='lm',formula=y~x,se=F,color="black")+
  labs(x="Receipts from EU budget (% of GDP)",y="Position estimate from Wordshoal model")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text=element_text(size=18,colour="black",family="Verdana"),
        axis.title=element_text(size=18,family="Verdana"),
        legend.position="none")
graph.actor.shoal.contribution

pdf("graphs_paper/Figure_A18.pdf", width = 12, height = 8) # Open a new pdf file
graph.actor.shoal.contribution
dev.off()

country_data_eel_long=data.frame(actor=rep(country_data_eel$actor,2),
                                 period=c(rep(1,28),rep(2,28)),
                                 contr=c(rep(country_data_eel$eu_cont_gdp,2)),
                                 theta=c(country_data_eel$means_period1,country_data_eel$means_period2))
country_data_eel_long$payer=ifelse(country_data_eel_long$contr>0,"No","Yes")
country_data_eel_long=country_data_eel_long%>%filter(!is.na(contr))

country_data_eel$actor[which(country_data_eel$means_period1>0&country_data_eel$eu_cont_gdp<0)]
country_data_eel$actor[which(country_data_eel$means_period1<0&country_data_eel$eu_cont_gdp>0)]

country_data_eel_long$payer=as.factor(country_data_eel_long$payer)

model1=lm(theta~period*payer, data=country_data_eel_long)
country_data_eel_long$predicted=predict(model1)

###############
# This is the information for Table 3 in the paper.
data.tab3=aggregate(country_data_eel_long$theta,by=list(country_data_eel_long$payer,country_data_eel_long$period),mean)
d <- data.frame("Negotiation"=c(round(data.tab3$x[2],2),round(data.tab3$x[1],2)),
                "Implementation"=c(round(data.tab3$x[4],2),round(data.tab3$x[3],2)))
row.names(d)=c("Average Contributor","Average Recipient")
print(xtable(d,
             caption = "Table 3: Increasing Polarization in EU MFF Negotiations 2011-2016"),
      caption.placement = "top",
      type="latex",
      file="tables/Table3.tex")

#####################################################
# Thetas from Shoal models
country_data_shoal=country_data_shoal%>%mutate(Contributor=ifelse(eu_cont_gdp>0,"Recipient","Contributor"))
country_data_shoal$Contributor[is.na(country_data_shoal$Contributor)]="Commission"
country_data_shoal$Contributor=factor(country_data_shoal$Contributor,levels = c("Contributor", "Commission","Recipient"))

country_data_shoal$actor <- factor(country_data_shoal$actor, 
                                   levels = country_data_shoal$actor[order(country_data_shoal$Contributor,country_data_shoal$means)])

mean_contr=mean(country_data_shoal$means[country_data_shoal$Contributor=="Contributor"],na.rm=T)
mean_reci=mean(country_data_shoal$means[country_data_shoal$Contributor=="Recipient"],na.rm=T)

country_data_shoal=country_data_shoal%>%
  mutate(
    actor_full=case_when(
      actor=="HU"~ "Hungary",
      actor=="UK"~ "United Kingdom",
      actor=="COM"~ "Commission",
      actor=="EL"~ "Greece",
      actor=="SE"~ "Sweden",
      actor=="EE"~ "Estonia",
      actor=="BE"~ "Belgium",
      actor=="ES"~ "Spain",
      actor=="CZ"~ "Czech Republic",
      actor=="FR"~ "France",
      actor=="SI"~ "Slovenia",
      actor=="PT"~ "Portugal",
      actor=="DE"~ "Germany",
      actor=="FI"~ "Finland",
      actor=="RO"~ "Romania",
      actor=="AT"~ "Austria",
      actor=="BG"~ "Bulgaria",
      actor=="MT"~ "Malta",
      actor=="SK"~ "Slovakia",
      actor=="NL"~ "Netherlands",
      actor=="LU"~ "Luxembourg",
      actor=="LV"~ "Latvia",
      actor=="IT"~ "Italy",
      actor=="LT"~ "Lithuania",
      actor=="IE"~ "Ireland",
      actor=="CY"~ "Cyprus",
      actor=="DK"~ "Denmark",
      actor=="PL"~ "Poland",
      TRUE ~ "other"
    )
  )

country_data_shoal$actor_full <- factor(country_data_shoal$actor_full, 
                                        levels = country_data_shoal$actor_full[order(country_data_shoal$Contributor,country_data_shoal$means)])

##################
# This is Figure 5 in the paper.
graph.actor.shoal<-country_data_shoal%>%
  ggplot(mapping=aes(y=actor_full, x=lower, xmin=lower, xmax=upper,color=Contributor,shape=Contributor))+
  geom_errorbarh(height=0, size=1.5)+
  geom_point(aes(y=actor_full, x=means),size=4)+
  labs(y = '', x = 'Position estimates')+
  scale_colour_manual(values=c("#bf271c","#696969","#313e81"))+
  geom_segment(aes(x = mean_contr, y = 1, xend = mean_contr, yend = 12),linetype='dashed',color="black")+
  geom_segment(aes(x = mean_reci, y = 14, xend = mean_reci, yend = 28),linetype='dashed',color="black")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text=element_text(size=18,colour="black",family="Verdana"),
        axis.title=element_text(size=18,family="Verdana"),
        legend.position = c(0.25, 0.95),
        legend.background = element_rect(color = "black",
                                         fill = "white", size = 0.5, linetype = "solid"), 
        legend.direction = "horizontal",legend.text=element_text(size=14,family="Verdana"),legend.title = element_blank())
graph.actor.shoal
mean_reci-mean_contr

pdf("graphs_paper/Figure_5.pdf", width = 12, height = 8) # Open a new pdf file
graph.actor.shoal
dev.off()

#############################################################
# At this point we need to run it all again with the API

load(file = "generated_data/mffapidata.RData")

#############################################################
# Wordfish for all debates

DFM.mff.api<-dfm(mff.api.pre,remove_punct=T, groups = "actormeeting",remove_numbers=T)
DFM.mff.api=dfm_trim(DFM.mff.api,min_docfreq = 10)
wf.mff.api<-wordfish(as.wfm(DFM.mff.api),verbose=T)

sum.wf.mff.api=data.frame(theta=wf.mff.api$theta,
                          lower=wf.mff.api$theta-1.96*wf.mff.api$se.theta,
                          upper=wf.mff.api$theta+1.96*wf.mff.api$se.theta,
                          index=wf.mff.api$docs,stringsAsFactors = F)
#save wordfish estimates in speechdata
speechdata.joined.api=left_join(speechdata.api,sum.wf.mff.api)

#############################################################
# Extract relevant budget information and add to speechdata
names(budget_contr)[grepl("year",names(budget_contr))]="Year"
names(budget_contr)[names(budget_contr)=="government"]="actor"
budget_contr$eu_cont_pc=as.numeric(budget_contr$eu_cont_pc)
budget_contr_sum=data.frame(country=unique(budget_contr$actor),stringsAsFactors = F)

budget_contr_eu_cont_pc=aggregate(budget_contr$eu_cont_pc[budget_contr$Year%in%c(2007,2008,2009,2010,2011,2012,2013)],
                                  by=list(budget_contr$actor[budget_contr$Year%in%c(2007,2008,2009,2010,2011,2012,2013)]),mean,na.rm=T)
names(budget_contr_eu_cont_pc)=c("country","eu_cont_pc")

budget_contr_eu_cont_total=aggregate(budget_contr$eu_cont_total[budget_contr$Year%in%c(2007,2008,2009,2010,2011,2012,2013)],
                                     by=list(budget_contr$actor[budget_contr$Year%in%c(2007,2008,2009,2010,2011,2012,2013)]),mean,na.rm=T)
names(budget_contr_eu_cont_total)=c("country","eu_cont_total")

budget_contr_eu_cont_gdp=aggregate(budget_contr$eu_cont_gdp[budget_contr$Year%in%c(2007,2008,2009,2010,2011,2012,2013)],
                                   by=list(budget_contr$actor[budget_contr$Year%in%c(2007,2008,2009,2010,2011,2012,2013)]),mean,na.rm=T)
names(budget_contr_eu_cont_gdp)=c("country","eu_cont_gdp")

budget_contr_sum=left_join(budget_contr_sum,budget_contr_eu_cont_pc)
budget_contr_sum=left_join(budget_contr_sum,budget_contr_eu_cont_total)
budget_contr_sum=left_join(budget_contr_sum,budget_contr_eu_cont_gdp)
names(budget_contr_sum)[names(budget_contr_sum)=="country"]="actor"
speechdata.joined.api=left_join(speechdata.joined.api,budget_contr_sum)

#############################################################
# Build Matrix to use in IRT and Wordshoal, Wordeel models

#Calculate Wordfish seperately for each debate and save in matrix
seperate.wf.matrix.for.irt.api=data.frame(matrix(nrow=length(unique(speechdata.joined.api$actor)),ncol=length(unique(speechdata.joined.api$meeting))))
names(seperate.wf.matrix.for.irt.api)=unique(speechdata.joined.api$meeting)
row.names(seperate.wf.matrix.for.irt.api)=unique(speechdata.joined.api$actor)
i=3107
for(i in names(seperate.wf.matrix.for.irt.api)){
  DFM.mff.api<-dfm(corpus_subset(mff.api.pre,no_mff_meeting==i),remove_punct=T, groups = "actormeeting",remove_numbers=T)
  wf.mff.api<-wordfish(as.wfm(DFM.mff.api),verbose=T)
  wf.mff.api.dat<-summary(wf.mff.api)
  sum.wf.mff.api=data.frame(theta=predict(wf.mff.api,interval="confidence"))
  names(sum.wf.mff.api)=c("theta","se","lower","upper")
  info=str_split(row.names(sum.wf.mff.api)," ")
  info <- do.call(rbind,info)
  sum.wf.mff.api$actor=info[,2]
  sum.wf.mff.api$meeting=info[,1]
  for(j in row.names(seperate.wf.matrix.for.irt.api)){
    if(j%in%sum.wf.mff.api$actor){
      seperate.wf.matrix.for.irt.api[j,i]=sum.wf.mff.api$theta[sum.wf.mff.api$actor==j]
    }
  }
}
docvars(mff.api.pre)

save(speechdata.joined.api,mff.api.pre,budget_contr,
     seperate.wf.matrix.for.irt.api,
     file="generated_data/datasets_mffapi_final.RData")

load(file = "generated_data/datasets_mffapi_final.RData")

##################################################
#Bayesian dynamic factor model
##################################################

#prepare data (anchor by UK and HU)
data.aqua <- seperate.wf.matrix.for.irt.api
data.aqua <- rbind(data.aqua[which(row.names(data.aqua)=="UK"),], data.aqua[-which(row.names(data.aqua)=="UK"),])
data.aqua <- rbind(data.aqua[which(row.names(data.aqua)=="HU"),], data.aqua[-which(row.names(data.aqua)=="HU"),])

#JAGS code

dynamic_factor_model <- 'model{
#loop through actors
for(i in 1:nactors){
#loop through debates
for(j in 1:ndebate){
Y[i,j] ~ dnorm(mu[i,j], tau[i])
mu[i,j] <- alpha[j] + beta[j] * theta[actor[i], period[j]]
}
change[i] <- theta[i,2] - theta[i,1]
}
#set normal priors on betas and alphas
for(j in 1:ndebate){
beta[j] ~ dnorm(0, 4)
alpha[j] ~ dnorm(0, 4)
}
#set priors on thetas (fix space with two actors)
theta[1, 1] ~ dnorm(1, 1)
theta[2, 1] ~ dnorm(-1, 1)
for(t in 2:nperiods){
theta[1, t] ~ dnorm(theta[1, t-1], tau.evol)
theta[2, t] ~ dnorm(theta[2, t-1], tau.evol)
}
for(c in 3:nactors){
theta[c, 1] ~ dnorm(0, 1)
for(t in 2:nperiods){
theta[c, t] ~ dnorm(theta[c, t-1], tau.evol)
}
}
#set prior on tau
for(i in 1:nactors){
tau[i] ~ dgamma(1, 1)
}
#set priors on evolution parameters for thetas
tau.evol <- 1
}
'

factor_model <- 'model{
#loop through actors
for(i in 1:nactors){
#loop through debates
for(j in 1:ndebate){
Y[i,j] ~ dnorm(mu[i,j], tau[i])
mu[i,j] <- alpha[j] + beta[j] * theta[actor[i]]
}
}
#set normal priors on betas and alphas
for(j in 1:ndebate){
beta[j] ~ dnorm(0, 4)
alpha[j] ~ dnorm(0, 4)
}
#set priors on thetas (fix space with two actors)
theta[1] ~ dnorm(1, 1)
theta[2] ~ dnorm(-1, 1)
for(c in 3:nactors){
theta[c] ~ dnorm(0, 1)
}
#set prior on tau
for(i in 1:nactors){
tau[i] ~ dgamma(1, 1)
}
}
'

#################################
# Dynamic Factor Model aka Wordeel

#set parameters
jags.data.eel <- list(
  Y = as.matrix(data.aqua),
  nactors = 28,
  ndebate = 14,
  nperiods = 2,
  period = c(1,1,1,1,1,1,1,1,1,2,2,2,2,2),
  actor = as.numeric(c(1:28))
)

jags.data.shoal <- list(
  Y = as.matrix(data.aqua),
  nactors = 28,
  ndebate = 14,
  actor = as.numeric(c(1:28))
)

###########################
#fit in JAGS

model.jags.eel <- jags.model(file=textConnection(dynamic_factor_model), data = jags.data.eel, n.chains = 1, inits=list(.RNG.name="base::Wichmann-Hill",.RNG.seed=1711))
posterior.jags.eel <- jags.samples(model.jags.eel, c("theta","beta","alpha", "tau.evol", "change"), n.iter=1000000, thin = 100, progress.bar="text")
posterior.coda.eel <- coda.samples(model.jags.eel, c("theta","beta","alpha", "tau.evol", "change"), n.iter=1000000, thin = 100, progress.bar="text")

#################################
# Factor Model aka Wordshoal

###########################
#fit in JAGS
model.jags.shoal <- jags.model(file=textConnection(factor_model), data = jags.data.shoal, n.chains = 1, inits=list(.RNG.name="base::Wichmann-Hill",.RNG.seed=1711))
posterior.jags.shoal <- jags.samples(model.jags.shoal, c("theta","beta","alpha"), n.iter=1000000, thin = 100, progress.bar="text")
posterior.coda.shoal <- coda.samples(model.jags.shoal, c("theta","beta","alpha"), n.iter=1000000, thin = 100, progress.bar="text")

###################################
# Results for Eel

sum.jags.eel=summary(posterior.coda.eel)[[1]][,1]
sum.jags.eel.beta=data.frame(means=sum.jags.eel[grepl(names(sum.jags.eel),pattern="beta")])
sum.jags.eel.beta$debate=as.numeric(gsub("\\D","",row.names(sum.jags.eel.beta)))

sum.jags.eel.theta=data.frame(means_period1=sum.jags.eel[grepl(names(sum.jags.eel),pattern="theta")&
                                                           grepl(names(sum.jags.eel),pattern=",1")],
                              means_period2=sum.jags.eel[grepl(names(sum.jags.eel),pattern="theta")&
                                                           grepl(names(sum.jags.eel),pattern=",2")],
                              change=sum.jags.eel[grepl(names(sum.jags.eel),pattern="change")])

mff.api.eel=HPDinterval(posterior.coda.eel[[1]],prob=.9)

mff.api.eel.thetas=data.frame(lower_period1=mff.api.eel[grepl(row.names(mff.api.eel),pattern = "theta")&grepl(row.names(mff.api.eel),pattern = ",1"),"lower"],
                              upper_period1=mff.api.eel[grepl(row.names(mff.api.eel),pattern = "theta")&grepl(row.names(mff.api.eel),pattern = ",1"),"upper"],
                              lower_period2=mff.api.eel[grepl(row.names(mff.api.eel),pattern = "theta")&grepl(row.names(mff.api.eel),pattern = ",2"),"lower"],
                              upper_period2=mff.api.eel[grepl(row.names(mff.api.eel),pattern = "theta")&grepl(row.names(mff.api.eel),pattern = ",2"),"upper"],
                              lower_change=mff.api.eel[grepl(row.names(mff.api.eel),pattern = "change"),"lower"],
                              upper_change=mff.api.eel[grepl(row.names(mff.api.eel),pattern = "change"),"upper"])
mff.api.eel.thetas$actor=row.names(data.aqua)
mff.api.eel.thetas$actornumber=1:nrow(data.aqua)

sum.jags.eel.theta$actor=row.names(data.aqua)
sum.jags.eel.theta$actornumber=1:nrow(data.aqua)

mff.api.eel.thetas=left_join(mff.api.eel.thetas,sum.jags.eel.theta)
country_data_eel=mff.api.eel.thetas

agg.contr.gdp=aggregate(speechdata.joined.api$eu_cont_gdp,by=list(speechdata.joined.api$actor),mean)
names(agg.contr.gdp)=c("actor","eu_cont_gdp")
country_data_eel=left_join(country_data_eel,agg.contr.gdp)

###################################
# Results for Shoal

sum.jags.shoal=summary(posterior.coda.shoal)[[1]][,1]
sum.jags.shoal.beta=data.frame(means=sum.jags.shoal[grepl(names(sum.jags.shoal),pattern="beta")])
sum.jags.shoal.beta$debate=as.numeric(gsub("\\D","",row.names(sum.jags.shoal.beta)))
sum.jags.shoal.theta=data.frame(means=sum.jags.shoal[grepl(names(sum.jags.shoal),pattern="theta")])
mff.api.shoal=HPDinterval(posterior.coda.shoal[[1]],prob=.9)

mff.api.shoal.thetas=data.frame(mff.api.shoal[grepl(row.names(mff.api.shoal),pattern = "theta")&!grepl(row.names(mff.api.shoal),pattern = "tau"),])
mff.api.shoal.thetas$actor=row.names(data.aqua) 
sum.jags.shoal.theta$actor=row.names(data.aqua)
mff.api.shoal.thetas=left_join(mff.api.shoal.thetas,sum.jags.shoal.theta)

country_data_shoal=mff.api.shoal.thetas
country_data_shoal=left_join(country_data_shoal,agg.contr.gdp)

country_data_eel_api=country_data_eel
country_data_shoal_api=country_data_shoal

#####################
# This is Figure A17 in Appendix 15

# in relation to contribution 
graph.actor.eel.contribution<-country_data_eel_api%>%
  filter(actor!="COM")%>%
  ggplot(mapping=aes(x=eu_cont_gdp, y=change,label = actor))+
  geom_text(size=5)+
  geom_smooth(method='lm',formula=y~x,se=F,color="black")+
  labs(x="Receipts from EU budget (% of GDP)",y="Change in position estimates")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text=element_text(size=18,colour="black",family="Verdana"),
        axis.title=element_text(size=18,family="Verdana"),
        legend.position="none")
graph.actor.eel.contribution

cor(country_data_eel_api$change[country_data_eel_api$actor!="COM"],country_data_eel_api$eu_cont_gdp[country_data_eel_api$actor!="COM"])

pdf("graphs_paper/Figure_A17.pdf", width = 12, height = 8) # Open a new pdf file
graph.actor.eel.contribution
dev.off()

country_data_eel_long_api=data.frame(actor=rep(country_data_eel_api$actor,2),
                                 period=c(rep(1,28),rep(2,28)),
                                 contr=c(rep(country_data_eel_api$eu_cont_gdp,2)),
                                 theta=c(country_data_eel_api$means_period1,country_data_eel_api$means_period2))
country_data_eel_long_api$payer=ifelse(country_data_eel_long_api$contr>0,"No","Yes")
country_data_eel_long_api=country_data_eel_long_api%>%filter(!is.na(contr))


country_data_eel_api$actor[which(country_data_eel_api$means_period1>0&country_data_eel_api$eu_cont_gdp<0)]
country_data_eel_api$actor[which(country_data_eel_api$means_period1<0&country_data_eel_api$eu_cont_gdp>0)]

country_data_eel_long_api$payer=as.factor(country_data_eel_long_api$payer)

model1=lm(theta~period*payer, data=country_data_eel_long_api)
country_data_eel_long_api$predicted=predict(model1)

aggregate(country_data_eel_long_api$theta,by=list(country_data_eel_long_api$payer,country_data_eel_long_api$period),mean)

#####################################################
# Thetas from Shoal models
country_data_shoal_api=country_data_shoal_api%>%mutate(Contributor=ifelse(eu_cont_gdp>0,"Recipient","Contributor"))
country_data_shoal_api$Contributor[is.na(country_data_shoal_api$Contributor)]="Commission"
country_data_shoal_api$Contributor=factor(country_data_shoal_api$Contributor,levels = c("Contributor", "Commission","Recipient"))

country_data_shoal_api$actor <- factor(country_data_shoal_api$actor, 
                                   levels = country_data_shoal_api$actor[order(country_data_shoal_api$Contributor,country_data_shoal_api$means)])

mean_contr=mean(country_data_shoal_api$means[country_data_shoal_api$Contributor=="Contributor"],na.rm=T)
mean_reci=mean(country_data_shoal_api$means[country_data_shoal_api$Contributor=="Recipient"],na.rm=T)

country_data_shoal_api=country_data_shoal_api%>%
  mutate(
    actor_full=case_when(
      actor=="HU"~ "Hungary",
      actor=="UK"~ "United Kingdom",
      actor=="COM"~ "Commission",
      actor=="EL"~ "Greece",
      actor=="SE"~ "Sweden",
      actor=="EE"~ "Estonia",
      actor=="BE"~ "Belgium",
      actor=="ES"~ "Spain",
      actor=="CZ"~ "Czech Republic",
      actor=="FR"~ "France",
      actor=="SI"~ "Slovenia",
      actor=="PT"~ "Portugal",
      actor=="DE"~ "Germany",
      actor=="FI"~ "Finland",
      actor=="RO"~ "Romania",
      actor=="AT"~ "Austria",
      actor=="BG"~ "Bulgaria",
      actor=="MT"~ "Malta",
      actor=="SK"~ "Slovakia",
      actor=="NL"~ "Netherlands",
      actor=="LU"~ "Luxembourg",
      actor=="LV"~ "Latvia",
      actor=="IT"~ "Italy",
      actor=="LT"~ "Lithuania",
      actor=="IE"~ "Ireland",
      actor=="CY"~ "Cyprus",
      actor=="DK"~ "Denmark",
      actor=="PL"~ "Poland",
      TRUE ~ "other"
    )
  )

country_data_shoal_api$actor_full <- factor(country_data_shoal_api$actor_full, 
                                        levels = country_data_shoal_api$actor_full[order(country_data_shoal_api$Contributor,country_data_shoal_api$means)])

########
# This is Figure A16 in Appendix 15

graph.actor.shoal.api<-country_data_shoal_api%>%
  ggplot(mapping=aes(y=actor_full, x=lower, xmin=lower, xmax=upper,color=Contributor))+
  geom_errorbarh(height=0, size=1.5)+
  geom_point(aes(y=actor_full, x=means),size=4)+
  labs(y = '', x = 'Position estimates')+
  scale_colour_manual(values=c("#bf271c","#696969","#313e81"))+
  geom_segment(aes(x = mean_contr, y = 1, xend = mean_contr, yend = 12),linetype='dashed',color="black")+
  geom_segment(aes(x = mean_reci, y = 14, xend = mean_reci, yend = 28),linetype='dashed',color="black")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text=element_text(size=18,colour="black",family="Verdana"),
        axis.title=element_text(size=18,family="Verdana"),
        legend.position = c(0.25, 0.95),
        legend.background = element_rect(color = "black",
                                         fill = "white", size = 0.5, linetype = "solid"), 
        legend.direction = "horizontal",legend.text=element_text(size=14,family="Verdana"),legend.title = element_blank())
graph.actor.shoal.api

#difference in means
mean_reci-mean_contr

pdf("graphs_paper/Figure_A16.pdf", width = 12, height = 8) # Open a new pdf file
graph.actor.shoal.api
dev.off()