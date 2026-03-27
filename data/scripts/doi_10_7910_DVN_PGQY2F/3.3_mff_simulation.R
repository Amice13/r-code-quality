#############################################################################################
# Replication Data for: Proksch, Wratil, Wäckerle. (2018). Testing the Validity of Automatic
# Speech Recognition for Political Text Analysis. Political Analysis, forthcoming.
#############################################################################################

#########################
# This script applies the WERSIM method to the MFF data and produces Figure A19

#############################################################
# Intro
#devtools::install_github("jenswaeckerle/wersim")
library(tidyverse)#This code was built with tidyverse version 1.2.1
library(austin)#This code was built with austin version 0.30
library(quanteda)#This code was built with quanteda version 1.3.13
library(MCMCpack)#This code was built with MCMCpack version 1.4-4
library(rjags)#This code was built with rjags version 4-8
library(coda)#This code was built with coda version 0.19-2
library(readtext)#This code was built with readtext version 0.71
library(RecordLinkage)#This code was built with RecordLinkage version 0.4-10
library(wersim)#This code was built with wersim version 0.1.0
library(extrafont)#This code was built with extrafont version 0.17

# Check if all packages are the ones we used
if(packageVersion("tidyverse")=="1.2.1"&
   packageVersion("austin")=="0.3.0"&
   packageVersion("quanteda")=="1.3.13"&
   packageVersion("MCMCpack")=="1.4-4"&
   packageVersion("rjags")=="4-8"&
   packageVersion("coda")=="0.19-2"&
   packageVersion("readtext")=="0.71"&
   packageVersion("RecordLinkage")=="0.4-10"&
   packageVersion("wersim")=="0.1.0"&
   packageVersion("extrafont")=="0.17"){
  paste("Your packages seem good to go")
}else{
  paste("Warning: Some or all of the package versions you are using are not exactly the ones we used for this paper. If the code doesn't replicate, this might be the reason.")
}

load(file = "generated_data/mffdata.RData")

###########################################################
####
#WER Calculation
####
###########################################################

# Verbatim Transcripts

mff.text.verbatim <- readtext("data/mff/MFF_VerbatimSample.txt")
mff.text.verbatim.corpus <- corpus_segment(corpus(mff.text.verbatim), "##*")
docvars(mff.text.verbatim.corpus,"no_mff_debate")=substr(docvars(mff.text.verbatim.corpus,"pattern"),3,6)
docvars(mff.text.verbatim.corpus,"actor")=do.call(rbind,str_split(substr(docvars(mff.text.verbatim.corpus,"pattern"),7,15),"_"))[,1]
docvars(mff.text.verbatim.corpus,"actor_debate")=paste0(docvars(mff.text.verbatim.corpus,"actor"),"_",docvars(mff.text.verbatim.corpus,"no_mff_debate"))
summary(mff.text.verbatim.corpus)
table(summary(mff.text.verbatim.corpus)$actor)

# Youtube Transcripts

mff.text.youtube <- readtext("data/mff/MFF_YouTubeSample.txt")
mff.text.youtube.corpus <- corpus_segment(corpus(mff.text.youtube), "##*")
docvars(mff.text.youtube.corpus,"no_mff_debate")=substr(docvars(mff.text.youtube.corpus,"pattern"),3,6)
docvars(mff.text.youtube.corpus,"actor")=do.call(rbind,str_split(substr(docvars(mff.text.youtube.corpus,"pattern"),7,15),"_"))[,1]
docvars(mff.text.youtube.corpus,"actor_debate")=paste0(docvars(mff.text.youtube.corpus,"actor"),"_",docvars(mff.text.youtube.corpus,"no_mff_debate"))
summary(mff.text.youtube.corpus)
table(summary(mff.text.youtube.corpus)$actor)

wer.mff=wer(r=mff.text.verbatim.corpus,h=mff.text.youtube.corpus)
round(apply(wer.mff[,2:4],2,sum)/sum(apply(wer.mff[,2:4],2,sum))*100,2)
(sum(wer.mff$sub)+sum(wer.mff$ins)+sum(wer.mff$del))/sum(wer.mff$words.ref)

save(file="generated_data/worderrorrates_mff.RData",
     wer.mff)

# api Transcripts

mff.text.api <- readtext("data/mff/MFF_APISample.txt")
mff.text.api.corpus <- corpus_segment(corpus(mff.text.api), "##*")
docvars(mff.text.api.corpus,"no_mff_debate")=substr(docvars(mff.text.api.corpus,"pattern"),3,6)
docvars(mff.text.api.corpus,"actor")=do.call(rbind,str_split(substr(docvars(mff.text.api.corpus,"pattern"),7,15),"_"))[,1]
docvars(mff.text.api.corpus,"actor_debate")=paste0(docvars(mff.text.api.corpus,"actor"),"_",docvars(mff.text.api.corpus,"no_mff_debate"))
summary(mff.text.api.corpus)
table(summary(mff.text.api.corpus)$actor)

wer.mff.api=wer(r=mff.text.verbatim.corpus,h=mff.text.api.corpus)
(sum(wer.mff.api$sub)+sum(wer.mff.api$ins)+sum(wer.mff.api$del))/sum(wer.mff.api$words.ref)

save(file="generated_data/worderrorrates_mff_api.RData",
     wer.mff.api)

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

load("generated_data/worderrorrates_mff.RData")
round(apply(wer.mff[,2:4],2,sum)/sum(apply(wer.mff[,2:4],2,sum))*100,2)
(sum(wer.mff$sub)+sum(wer.mff$ins)+sum(wer.mff$del))/sum(wer.mff$words.ref)

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
speechdata.joined=left_join(speechdata,budget_contr_sum)

#############################################################
# Build Matrix to use in IRT and Wordshoal, Wordeel models
set.seed(1711)
new_wer_to_est=0.05599426+c(0.05,0.1,0.15,0.2)
num_to_sim=50

data.store_mff=data.frame(wer=c(rep(new_wer_to_est[1],num_to_sim),rep(new_wer_to_est[2],num_to_sim),
                                           rep(new_wer_to_est[3],num_to_sim),rep(new_wer_to_est[4],num_to_sim)),
                                     est=NA,cor_to_youtube=NA,stringsAsFactors = F)

data.store.estimates=list()

for(k in 1:nrow(data.store_mff)){
#Calculate Wordfish seperately for each debate and save in matrix
seperate.wf.matrix.for.irt=data.frame(matrix(nrow=length(unique(speechdata.joined$actor)),ncol=length(unique(speechdata.joined$meeting))))
names(seperate.wf.matrix.for.irt)=unique(speechdata.joined$meeting)
row.names(seperate.wf.matrix.for.irt)=unique(speechdata.joined$actor)

for(i in names(seperate.wf.matrix.for.irt)){
  sum.wf.mff<-wersimtext(corpus_subset(mff.pre,no_mff_meeting==i),measured_wer = 0.05599426,new_wer = data.store_mff$wer[k],
                         deletions_sim = 0.2051,insertions_sim = 0.2258,substitutions_sim = 0.5691,num_sims = 1,
                     groupingvar_sim=docvars(corpus_subset(mff.pre,no_mff_meeting==i),"actormeeting"),method="wordfish")
  names(sum.wf.mff)=c("index","theta")
  info=str_split(sum.wf.mff$index," ")
  info <- do.call(rbind,info)
  sum.wf.mff$actor=info[,2]
  sum.wf.mff$meeting=info[,1]
  for(j in row.names(seperate.wf.matrix.for.irt)){
    if(j%in%sum.wf.mff$actor){
      seperate.wf.matrix.for.irt[row.names(seperate.wf.matrix.for.irt)==j,names(seperate.wf.matrix.for.irt)==i]=sum.wf.mff$theta[sum.wf.mff$actor==j]
    }
  }
}

#prepare data (anchor by UK and HU)
data.aqua <- seperate.wf.matrix.for.irt
data.aqua <- rbind(data.aqua[which(row.names(data.aqua)=="UK"),], data.aqua[-which(row.names(data.aqua)=="UK"),])
data.aqua <- rbind(data.aqua[which(row.names(data.aqua)=="HU"),], data.aqua[-which(row.names(data.aqua)=="HU"),])


jags.data.shoal <- list(
  Y = as.matrix(data.aqua),
  nactors = 28,
  ndebate = 14,
  actor = as.numeric(c(1:28))
)

#################################
# Factor Model aka Wordshoal

###########################
#fit in JAGS
model.jags.shoal <- jags.model(file=textConnection(factor_model), data = jags.data.shoal, n.chains = 1, inits=list(.RNG.name="base::Wichmann-Hill",.RNG.seed=1711))
posterior.jags.shoal <- jags.samples(model.jags.shoal, c("theta","beta","alpha"), n.iter=1000000, thin = 100, progress.bar="text")
posterior.coda.shoal <- coda.samples(model.jags.shoal, c("theta","beta","alpha"), n.iter=1000000, thin = 100, progress.bar="text")

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

country_data_shoal=mff.shoal.thetas
agg.contr.gdp=aggregate(speechdata.joined$eu_cont_gdp,by=list(speechdata.joined$actor),mean)
names(agg.contr.gdp)=c("actor","eu_cont_gdp")
country_data_shoal=left_join(country_data_shoal,agg.contr.gdp)

#####################################################
# Thetas from Shoal models
country_data_shoal=country_data_shoal%>%mutate(Contributor=ifelse(eu_cont_gdp>0,"Recipient","Contributor"))
country_data_shoal$Contributor[is.na(country_data_shoal$Contributor)]="Commission"
country_data_shoal$Contributor=factor(country_data_shoal$Contributor,levels = c("Contributor", "Commission","Recipient"))

country_data_shoal$actor <- factor(country_data_shoal$actor, 
                                   levels = country_data_shoal$actor[order(country_data_shoal$Contributor,country_data_shoal$means)])

mean_contr=mean(country_data_shoal$means[country_data_shoal$Contributor=="Contributor"],na.rm=T)
mean_reci=mean(country_data_shoal$means[country_data_shoal$Contributor=="Recipient"],na.rm=T)
mean_reci-mean_contr


data.store_mff$est[k]=mean_reci-mean_contr
data.store.estimates[[k]]=country_data_shoal
print(k)
}

save(data.store_mff,data.store.estimates,file="generated_data/mff_simulations.RData")
#load("generated_data/mff_simulations.RData")
#load("generated_data/worderrorrates_mff_api.RData")
#load("generated_data/worderrorrates_mff.RData")
(sum(wer.mff$sub)+sum(wer.mff$ins)+sum(wer.mff$del))/sum(wer.mff$words.ref)
(sum(wer.mff.api$sub)+sum(wer.mff.api$ins)+sum(wer.mff.api$del))/sum(wer.mff.api$words.ref)

########
# This is Figure A19 in Appendix 17
graph.mff.sim<-data.store_mff%>%
  ggplot(mapping=aes(y=est, x=wer))+
  geom_boxplot(aes(group = wer))+
  geom_point(aes(y=1.09,x=0.0559), color="red")+ 
  annotate("text", label = "Estimate from \nYouTube corpus", x = 0.0559, y = 1.14, size = 4, colour = "red",family="Verdana")+
  geom_point(aes(y=1.13,x=0.20), color="red")+ 
  annotate("text", label = "Estimate from \nAPI corpus", x = 0.18, y = 1.24, size = 4, colour = "red",family="Verdana")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text=element_text(size=14,colour="black",family="Verdana"),
        axis.title=element_text(size=14,family="Verdana"),
        legend.position = c(0.25, 0.95),
        legend.background = element_rect(color = "black",
                                         fill = "white", size = 0.5, linetype = "solid"), 
        legend.direction = "horizontal",legend.text=element_text(size=14),legend.title = element_blank())+
  labs(y = 'Difference between contributor \nand recipient states', x = 'Word error rate')+
  scale_y_continuous(limits = c(0.85,1.35))+
  scale_x_continuous(limits = c(0,0.28))
graph.mff.sim

pdf("graphs_paper/Figure_A19.pdf", width = 6, height = 4) # Open a new pdf file
graph.mff.sim
dev.off()

aggregate(data.store_mff$est,by=list(data.store_mff$wer),FUN=mean)
aggregate(data.store_mff$est,by=list(data.store_mff$wer),FUN=sd)