#Replication code for: Never Waster a Crisis - How COVID-19 Lockdowns and Message Sources Affect Household Emergency Preparedness#
#Tim Marple, Alison Post, and Karen Frick#
# Basics: Data Reading and Coding -----------------------------------------
rm(list=ls())
# setwd("~/Desktop/")
setwd("./NWAC Replicate/")
library(ggplot2)
library(grid)
library(gridExtra)
library(lmtest)
library(MASS)
library(MatchIt)
library(mice)
library(miceadds)
library(plm)
library(sf)
library(sjPlot)
library(stargazer)
library(tmap)
library(Zelig)
load("nwac_survey_data.RData")
load("nwac_auxiliary_data.RData")
row.names(dat)<-1:nrow(dat)

#Coding covariates of interest for analysis: binary or scale#
#Demographics, preparation for lockdown, days in lockdown#
dat$white<-as.numeric(dat$Race=="White/Caucasian")
dat$male<-as.numeric(dat$Gender=="Male")
dat$children<-as.numeric(dat$Children=="Yes")
dat$coll<-as.numeric(dat$Education %in% c("Bachelor's degree in college (4-year)","Master's or Professional (JD/MD) degree","Doctoral degree"))
incdf<-data.frame(number=c(2,5,4,3,1,6),text=as.character(unique(dat$Income)))
dat$inc<-incdf$number[match(dat$Income,incdf$text)]
dat$age<-2019-as.numeric(as.character(dat$Age))
dat$age2<-dat$age^2 #Age squared
rddf<-data.frame(number=c(6,3,5,1,4,2),text=unique(dat$Residence.duration))
dat$res_dur<-rddf$number[match(dat$Residence.duration,rddf$text)]
dat$employed<-as.numeric(dat$Employment %in% c("Working (self-employed)","Working (paid employee)"))
dat$student<-as.numeric(dat$Employment %in% c("Student (Undergraduate or 2-Year Program)","Student (Graduate or Post-Graduate)"))
dat$own<-as.numeric(dat$Own.or.rent=="Own")
dat$rent<-as.numeric(dat$Own.or.rent=="Rent")
dat$apartment<-as.numeric(dat$Residence.type=="Apartment")
dat$house<-as.numeric(dat$Residence.type=="House")
rsdf<-data.frame(number=c(1,2,3,0),text=unique(dat$Residence.Size))
dat$res_size<-rsdf$number[match(dat$Residence.Size,rsdf$text)]
dat$ext_store<-as.numeric(dat$External.storage=="Yes")
dat$cars<-as.numeric(dat$Cars!="0")
dat$english<-as.numeric(dat$Primary.language!="No")
dat$repub<-as.numeric(dat$Partisan1=="Republican" | dat$IndLean=="Republican")
dat$covid_water<-0 #Whether prepared water for Covid lockdown: 1 if in list of what prepared, 0 otherwise (use 0 as default, fill binary)
for(i in 1:nrow(dat)){dat$covid_water[i]<-as.numeric("Water" %in% as.character(unlist(strsplit(as.character(dat$What.covid.prep[i]),","))))}
dat$prior_lockdown<-as.numeric(dat$Prior.Lockdown=="Yes") #Lockdown before statewide order: 1 if yes, 0 otherwise
dat$days_in_lockdown<-NA #Days in lockdown: start with NA, fill using DAY parameter
for(i in 1:nrow(dat)){
  ldis<-as.character(dat$WhenLockdown[i]) #Day of month when locked down
  dsti<-as.numeric(dat$DAY[i]) #Day of month when took survey
  if(ldis==""){ #If this is a no-previous lockdown respondent
    dat$days_in_lockdown[i]<-dsti-19 #Subtract day taken from 19th when statewide ordered
  } else { #If a prior lockdown respondent
    ldis<-as.numeric(as.character(unlist(strsplit(ldis," ")))[3]) #Identify day of month when locked down
    dat$days_in_lockdown[i]<-dsti-ldis #Subtract from day of month when took survey
  }
}
dat$time<-as.numeric(as.character(dat$Duration..in.seconds.))/60 #Response time in total, in minutes

#Trust - generate key and function for trust, and organization key#
tv<-c(-3,-3,-2,-1,1,2,3)
# tv<-c(1,1,2,3,4,5,6)
trust_key<-data.frame(text=c("Completely Distrust","Complete Distrust","Distrust","Somewhat Distrust","Somewhat Trust","Trust","Completely Trust"),
                      number=tv) #Coding scheme: -3, -2, -1, 1, 2, 3, with 0 benchmark as control
trust_to_num<-function(datvec){return(as.numeric(as.character(trust_key$number[match(datvec,trust_key$text)])))} #Functionalize numeric replacement with key
dat$trust_gen<-trust_to_num(dat$Pre.Trust.0) #Get trust in general population by key

#Assignment by organization#
names(dat)
orgMat<-as.matrix(dat[,c(74:79)]) #Matrix for organization in treatment
orgCount<-data.frame(Count=diag(t(orgMat) %*% orgMat)) #Organization assignment counts
orgCount #Consistent assignment across all organizations

#Get vector of organizations to produce key for referencing with trust score assignment#
orgsAll<-unique(as.character(dat$TrustWaterPrepare_DO)) #All unique organization presentation orders, split for unique names
orgsAll<-as.character(unlist(strsplit(orgsAll,split="\\|")))[grep("image",as.character(unlist(strsplit(orgsAll,split="\\|"))))]
orgsAll<-gsub(" image","",orgsAll) #Remove supplementary text from survey descriptor
orgsName<-gsub(" ","",orgsAll) #Clear out empty spaces in names for exact matching, create key below
orgKey<-data.frame(surv=c("FEMA (Federal Emergency Management Agency)","Red Cross","Local water provider", 
                          "Independent academic experts","Local city government"),short=orgsName[c(1,5,4,6,2)])
orgKey #Check it out for accuracy

#Recode likert, rename both likert and  ranking columns for score by order of presentation#
for(i in 1:5){ #Cycle over first through fifth presented organization for recoding
  dat[,paste0("Pre.Trust.1_",i)]<-trust_to_num(dat[,paste0("Pre.Trust.1_",i)]) #Recode numeric
  names(dat)[which(names(dat)==paste0("Pre.Trust.1_",i))]<-paste0("lik_org_",i) #Name in data by order
  dat[,paste0("Pre.Trust.2_",i)]<-as.numeric(as.character((dat[,paste0("Pre.Trust.2_",i)]))) #Recode rank as number
  names(dat)[which(names(dat)==paste0("Pre.Trust.2_",i))]<-paste0("rank_org_",i) #Rename rank by order
  #Recode display order text for easier access later, both in Likert score (first line below) and ranking (second line below)#
  dat$Pre.Trust.1_DO<-gsub(as.character(orgKey$surv[i]), as.character(orgKey$short[i]),as.character(dat$Pre.Trust.1_DO),fixed=T)
  dat$Pre.Trust.2_DO<-gsub(as.character(orgKey$surv[i]), as.character(orgKey$short[i]),as.character(dat$Pre.Trust.2_DO),fixed=T)
}
names(dat)[which(names(dat)=="Pre.Trust.1_DO")]<-"likert_order" #Rename for clear reference later
names(dat)[which(names(dat)=="Pre.Trust.2_DO")]<-"ranking_order" #Rename for clear reference later
# View(dat) #Check data to ensure all smells right (if you'd like)

#Recode Likert and ranking trust scores for each organization, by name, not by order#
org_lik<-data.frame(matrix(NA,nrow=nrow(dat),ncol=(length(orgsName)-1))) #Hollow with NAs for Likert
org_rank<-org_lik #Replicate for ranking values
names(org_lik)<-paste0(orgsName[-3],"_likert") #Name by organization to populate, omit control
names(org_rank)<-paste0(orgsName[-3],"_rank") #Same for ranking
dat<-cbind(dat,org_lik) #Attach both to main data
dat<-cbind(dat,org_rank)

#Cycle over rows, capture above org-specific scores, and scores for presented organization#
dat$trust_in_source<-NA #Likert for presented organization
dat$rank_source<-NA #Ranking for presented organization
for(i in 1:nrow(dat)){
  #For Likert and ranking: get column order with match against orgNames, plug into main data#
  colOrd1<-match(as.character(unlist(strsplit(dat$likert_order[i],"\\|"))),orgsName[-3])
  dat[i,names(org_lik)[colOrd1]]<-as.numeric(dat[i,c(paste0("lik_org_",1:5))])
  colOrd2<-match(as.character(unlist(strsplit(dat$ranking_order[i],"\\|"))),orgsName[-3])
  dat[i,names(org_rank)[colOrd2]]<-as.numeric(dat[i,c(paste0("rank_org_",1:5))])
  if(dat$Control[i]==1){
    dat$trust_in_source[i]<-0
    dat$rank_source[i]<-0
  } else {
    orgShow<-orgsName[which(dat[i,orgsName]==1)]
    dat$trust_in_source[i]<-as.numeric(dat[i,paste0(orgShow,"_likert")])
    dat$rank_source[i]<-(6-as.numeric(dat[i,paste0(orgShow,"_rank")]))
  }
}
# #Inspect to ensure performed as desired (all checks out)#
# names(dat)
# lik_inspect_cols<-c(paste0("lik_org_",1:5),"likert_order",paste0(orgsName[-3],"_likert"))
# View(dat[,lik_inspect_cols]) #Inspect Likert accuracy
# rank_inspect_cols<-c(paste0("rank_org_",1:5),"ranking_order",paste0(orgsName[-3],"_rank"))
# View(dat[,rank_inspect_cols]) #Inspect ranking accuracy

#Inspect distributions#
hist(dat$trust_in_source) #Check trust in source distribution
hist(dat$rank_source) #Check rank source distribution
summary(dat$trust_in_source) #And breakdown
summary(dat$rank_source) #And breakdown

#Code if organization is most trusted source by Likert or ranking#
dat$most_trusted_L<-0 #Start with not as default
dat$most_trusted_R<-0 #For ranking as well
for(i in 1:nrow(dat)){ #Cycle over, code as 1 for either if top score
  dat$most_trusted_L[i]<-as.numeric(dat$trust_in_source[i] >= max(as.numeric(dat[i,c(paste0("lik_org_",1:5))])))
  dat$most_trusted_R[i]<-as.numeric(dat$rank_source[i] == 5)
}
summary(dat$most_trusted_L)
summary(dat$most_trusted_R)

#Binary code whether respondent trusted source at all (positive score) or mistrusted (negative score)#
dat$trust_source_bin<-as.numeric(dat$trust_in_source>0)
dat$mistrust_source_bin<-as.numeric(dat$trust_in_source<0)
summary(dat$trust_source_bin)
summary(dat$mistrust_source_bin)
sum(dat$trust_source_bin*dat$mistrust_source_bin,na.rm = T) #No intersection - as expected

#Post Water Prep: begin with key for numericizing text responses#
dv_key<-data.frame(text=c("Definitely will not","Not likely at all","Somewhat likely","Likely","Very likely","Extremely likely","Definitely will"),num=1:7)
dat$post_dv<-as.numeric(dv_key$num[match(dat$Trust.Prep.Outcome_1,dv_key$text)]) #Match with key
hist(dat$post_dv) #Inspect distribution

#Identify the organization treatment versus control#
dat$control_group<-0
dat$control_group[dat$Control==1]<-1
dat$treatment_group<-1-dat$control
sum(dat$control_group)
sum(dat$treatment_group)

#Get town, county from ZIP code#
dat$county<-zips$County[match(dat$ZIP,zips$Zip.Code)]
dat$city<-zips$City[match(dat$ZIP,zips$Zip.Code)]

# Summary Tables and Plots ------------------------------------------------

#Output demographics summary for write-up#
demcols<-c(which(names(dat)=="white"):which(names(dat)=="days_in_lockdown"))
demcols<-demcols[-which(demcols==which(names(dat)=="age2"))]
demSum<-dat[,demcols]
names(demSum)<-c("White","Male","Children","College","Income","Age","Residence Duration","Employed",
                 "Student","Own Residence","Rent Residence","Residence: Apartment","Residence: House",
                 "Residence Size","External Storage","Cars","English","Republican","Prepared Water for Covid",
                 "Lockdown Before State Order","Days in Lockdown by Response")
stargazer(demSum,out="table_s14_demographics_summary.html")

#Trust in organizations and general trust, confidence intervals and means#
confidence_interval <- function(vector, interval) {
  error <- qt((interval + 1)/2, df = length(vector)-1) * (sd(vector) / sqrt(length(vector)))
  result <- c("lower" = mean(vector) - error,'mid' = mean(vector) ,"upper" = mean(vector) + error)
  return(result)
}

#Summary table for trust in organizations, general, with 95% CI#
orgSum<-data.frame(Organization=orgsName,First=NA,Mean=NA,Third=NA)
for(o in orgsName){
  cn<-ifelse(o=="Control","trust_gen",paste(o,"likert",sep="_"))
  ci<-as.numeric(round(confidence_interval(dat[,cn],0.95),3))
  orgSum[orgSum$Organization==o,2:ncol(orgSum)]<-as.numeric(ci)
}
orgSum<-orgSum[order(orgSum$Mean,decreasing = F),]
names(orgSum)<-c("Group","2.5%","Mean","97.5%")
orgSum$Group<-orgKey$surv[match(orgSum$Group,orgKey$short)]
orgSum$Group[1]<-"General Public"
orgSum$Group[2]<-"FEMA" #Shorter label
stargazer(orgSum,summary = F,rownames = F,out="table_m3_organization_trust_summary.html")

#Difference in most and least trusted organization by row#
dat$orgTrDiff<-NA
for(i in 1:nrow(dat)){
  trVec<-dat[i,paste0(orgsName[-3],"_likert")]
  dat$orgTrDiff[i]<-max(trVec)-min(trVec)
}
hist(dat$orgTrDiff)
summary(dat$orgTrDiff)

#Plot#
jpeg("figure_s2_trust_differences_byRespondent.jpeg",width = 1000, height = 600)
ggplot(dat,aes(orgTrDiff)) + 
  ggtitle("Difference in Maximum and Minimum Organizational Trust Scores") +
  xlab("Difference in Maximum and Minimum Trust Score") + ylab("Count") +
  geom_bar(width = 0.75) + 
  geom_vline(xintercept = c(mean(dat$orgTrDiff)),col="red",size=1)
dev.off()

#Map for CA lockdowns by date or pre-state order#
names(supp)
cali$Date<-supp$DATE[match(cali$NAME,supp$COUNTY)]
cali$DAYS<-supp$DAYS[match(cali$NAME,supp$COUNTY)]
cali$PRESTATE<-supp$PRESTATE[match(cali$NAME,supp$COUNTY)]
cali$PRESTATE_BIN<-ifelse(cali$Date=="3/19/20","No","Yes")

#Bounding box specifications#
bbox_new <- st_bbox(cali) #Current bounding box
xrange <- bbox_new$xmax - bbox_new$xmin #X range
yrange <- bbox_new$ymax - bbox_new$ymin #Y range
bbox_new[3] <- bbox_new[3] + (0.25 * xrange) #Xmax - right
bbox_new[4] <- bbox_new[4] + (0.2 * yrange) #Ymax - top

#Convert to polygon#
bbox_new <- bbox_new %>% st_as_sfc()

#Output figure#
jpeg("figure_m1_ca_lockdown_map.jpeg",width=800,height = 800)
tm_shape(cali, bbox = bbox_new) + tm_fill("Date") + tm_borders() + 
  tm_layout("",
            legend.title.size = 1.5,
            legend.text.size = 1) + 
  tm_layout(legend.position = c("right", "top"), 
            title= 'Figure 1: COVID-19 Lockdown Orders across California Counties by Date Instituted', 
            title.position = c('left', 'top'))
dev.off()

#Breakdown of ranking for organizations#
rankMat<-data.frame(matrix(NA,ncol=6,nrow=5))
names(rankMat)<-c("Rank",orgsName[-3])
rankMat$Rank<-1:5
for(i in 2:ncol(rankMat)){
  rdfT<-data.frame(table(dat[,paste0(names(rankMat)[i],"_rank")]))
  rankMat[,i]<-round(rdfT$Freq[match(rankMat$Rank,rdfT$Var1)]/1211,3)
}
names(rankMat)[3:6]<-c("City Government","Water Provider","Red Cross","Independent Academic Experts")
rankMat #Check before output
stargazer(rankMat,summary = F,rownames = F,out="table_s10_organization_ranking_summary.html")

#Show demographic splits across prior lockdown condition#
mat1<-as.matrix(dat$prior_lockdown)
mat2<-as.matrix(dat[,c("male","white","age","repub","coll","children","english","employed","inc","res_dur","res_size",paste(orgKey$short,"likert",sep="_"))])

#Determine cross-group averages and differences#
ld<-(t(mat2) %*% mat1)/sum(mat1)
nld<-(t(mat2) %*% abs(1-mat1))/sum(1-mat1)
ldbalm<-cbind(ld,nld)
row.names(ldbalm)<-c("Male","White","Age","Republican","College",
                     "Children","English","Employed","Income (Scale)",
                     "Residence Duration (Scale)","Residence Size (Scale)",paste0("Trust: ",orgKey$surv))
colnames(ldbalm)<-c("Prior Lockdown","No Prior Lockdown")
ldbalm<-round(ldbalm,2)
ldbalm

#Determine significance of differences#
mat2<-as.data.frame(mat2)
ldbalm<-as.data.frame(ldbalm)
ldbalm$Difference<-NA
for(i in 1:ncol(mat2)){
  tt<-t.test(mat2[,i]~dat$prior_lockdown)
  difft<-round(tt$estimate[2]-tt$estimate[1],2)
  pt<-tt$p.value
  if(pt<0.01){
    difft<-paste0(difft,"***")
  } else if(pt<0.05){
    difft<-paste0(difft,"**")
  } else if(pt<0.1){
    difft<-paste0(difft,"*")
  }
  ldbalm$Difference[i]<-difft
}
#Output#
stargazer(ldbalm,out="table_s8a_lockdown_balance_table.html",summary = F)

# Regression: Balance and predicting trust breadth ---------------------------------------
#Function for good covariate labels from code titles#
clDF<-data.frame(code=names(dat)[c(81:123,126)],
                 label=c("White","Male","Children in Residence","College Education","Income (Scale)","Age","Age Squared",
                         "Residence Duration","Employed","Student","Own Residence","Rent Residence","Residence: Apartment",
                         "Residence: House","Residence Size (Scale)","External Storage Available","Cars Available (Scale)",
                         "English at Home","Republican (Binary)","Prepared Water for Lockdown","Prior Lockdown (Binary)",
                         "Days in Lockdown (Count)","Time","Trust: General","Trust: FEMA","Trust: City Government","Trust: Water Provider",
                         "Trust: Red Cross","Trust: Independent Academics","Rank: FEMA","Rank: City Government","Rank: Water Provider",
                         "Rank: Red Cross","Rank: Independent Academics","Trust in Source","Message Source Ranking","Source: Most Trusted",
                         "Source: Top Ranked","Binary: Trust Source","Binary: Mistrust Source","Willingness to Prepare (Scale)",
                         "Control","Treatment","Trust Breadth"))
# View(clDF) #Inspect for accuracy before use in replacing model labels

#Function to replace model labels as needed#
covLab<-function(model){return(as.character(clDF$label[match(names(model$model)[-1],clDF$code)]))}

#Balance models on control assignment: OLS (olsB) and logit (logB) (summary show treatment balance)#
olsB<-lm(control_group~male+white+age+repub+coll+children+english+employed+inc+res_dur+res_size,dat)
summary(olsB)
logB<-glm(control_group~male+white+age+repub+coll+children+english+employed+inc+res_dur+res_size,"binomial",dat)
summary(logB)
stargazer(olsB,logB,dep.var.labels = "",column.labels = c("OLS","Logistic"),model.names = F,
          dep.var.caption = "Dependent variable: Assignment to Control Group",
          out="table_s9_balance_models.html",covariate.labels=covLab(olsB))

#Predicting differences in trust scores: olsTR on scale#
trM1<-lm(orgTrDiff~male+white+age+age2+repub+coll+children+english+employed+inc+res_dur+res_size,dat)
trM1r<-coeftest(trM1,vcov. = vcovHC(trM1,"HC1"))
stargazer(trM1r,add.lines=list(c("Observations",nrow(trM1$model)),
                               c("Adj. R2",round(summary(trM1)$adj.r.squared,3))),
          dep.var.labels = "",column.labels = NULL,model.names = F,
          dep.var.caption = "Dependent variable: Spread in Most and Least Trusted Scores",
          out="table_s7_trustScore_breadth_model.html",covariate.labels = covLab(trM1))

# MAIN OLS Regression -----------------------------------------------------
#Main OLS Model: scale movement along main predictors (ols1)#
#Model A: no controls, only experimental treatment, main and robust SE#
ols1a<-lm(post_dv ~ prior_lockdown + trust_in_source,dat)
ols1ar<-coeftest(ols1a,vcov=vcovHC(ols1a,"HC1")) #Robust SEs

#Model B: added demographic controls with treatments#
#Main models with and without robust SE#
ols1b<-lm(post_dv ~ prior_lockdown + trust_in_source + white + male + repub + age + age2 + 
            coll + inc + children + employed + inc + days_in_lockdown,dat)
ols1br<-coeftest(ols1b,vcov=vcovHC(ols1b,"HC1")) #Robust pooling model

#AME plot output#
jpeg("figure_s1_AMEplot_mainOLS.jpeg",width = 600,height = 600)
plot_model(ols1b) + xlab(NULL) + ggtitle(NULL) + theme(text = element_text(size=16)) + 
  scale_x_discrete(labels=rev(covLab(ols1b)))
dev.off()

#Output both models in table#
stargazer(ols1ar,ols1br,covariate.labels=covLab(ols1b),
          add.lines=list(c("Observations",nrow(ols1a$model),nrow(ols1b$model)),
                         c("Adj. R2",round(summary(ols1a)$adj.r.squared,3),round(summary(ols1b)$adj.r.squared,3))),
          dep.var.labels = rep("",2),column.labels = NULL,model.names = F,
          dep.var.caption = "Dependent variable: Willingness to Prepare (7 point Scale)",
          out="table_s1_mainOLS_models.html")

#Model C: autocorrelation between predictors and trust in source#
ols1c<-lm(trust_in_source~white + male + repub + age + age2 + coll + children + employed + inc + days_in_lockdown,dat)
ols1cr<-coeftest(ols1c,vcov=vcovHC(ols1c,"HC1")) #Robust pooling model
stargazer(ols1cr,add.lines=list(c("Observations",nrow(ols1c$model)),c("Adj. R2",round(summary(ols1c)$adj.r.squared,3))),
          dep.var.labels = "",column.labels = NULL,model.names = F,
          dep.var.caption = "Dependent variable: Trust in Source (-3 to 3)",
          covariate.labels=covLab(ols1c),out="table_s3_autocorrelation_mainOLS_models.html")

# MAIN LOGIT Regression ---------------------------------------------------

#Balance for experiencing pre-statewide lockdown#
ballm<-glm(prior_lockdown ~ male + white + age + repub + coll + children + english + employed + inc + res_dur + res_size,"binomial",data=dat)
stargazer(ballm,dep.var.labels = "",column.labels = NULL,model.names = F,
          dep.var.caption = "Dependent variable: Experience Prior Lockdown",
          covariate.labels=covLab(ballm),
          out="table_s8b_lockdown_balance_glm.html")

#Main logit model: being very, extremely, or definitely willing to prepare (log1)#
dat$willing<-as.numeric(dat$post_dv>4) #Define binary outcome variable

#Model 1 - only lockdown#
log1a<-glm(willing ~ prior_lockdown,"binomial",dat)
log1ar<-coeftest(log1a,vcov=vcovHC(log1a,"HC1")) #Robust pooling model

#Model 2 - only trust in source#
log1b<-glm(willing ~ trust_in_source,"binomial",dat)
log1br<-coeftest(log1b,vcov=vcovHC(log1b,"HC1")) #Robust pooling model

#Model 3 - lockdown and trust together#
log1c<-glm(willing ~ prior_lockdown + trust_in_source,"binomial",dat)
log1cr<-coeftest(log1c,vcov=vcovHC(log1c,"HC1")) #Robust pooling model

#Model 4 - both primary covariates with controls#
log1d<-glm(willing ~ prior_lockdown + trust_in_source + white + male + repub + age + age2 + coll + inc + children + employed + inc + days_in_lockdown,"binomial",dat)
log1dr<-coeftest(log1d,vcov=vcovHC(log1d,"HC1")) #Robust pooling model

#Output table#
stargazer(log1ar,log1br,log1cr,log1dr,add.lines = list(c("Observations",nrow(log1a$model),nrow(log1b$model),nrow(log1c$model),nrow(log1d$model)),
                                                       c("AIC",round(log1a$aic,2),round(log1b$aic,2),round(log1c$aic,2),round(log1d$aic,2))),
          dep.var.labels = rep("",4),column.labels = NULL,model.names = F,
          dep.var.caption = "Dependent variable: Willing to Prepare Water for Earthquake",
          covariate.labels=covLab(log1d),out="table_m1_mainLogit_models.html")

#Output main plot#
jpeg("figure_m3_predPlot_mainLogit.jpeg",width = 1200,height = 600)
plot_model(log1c,type="pred",terms = c("trust_in_source","prior_lockdown")) + 
  xlab("Trust in Source") + 
  ylab("Probability of Reporting Very/Extremely/Definitely Willing") + 
  ggtitle("Figure 3: Predicted Individual Willingness to Store Water by Trust in Message Source") + 
  theme(text = element_text(size=18)) + #,legend.position = "none"
  # labs(colour="Prior Lockdown") +   
  scale_colour_discrete(name="Prior Lockdown",labels=c("No","Yes")) +
  scale_x_continuous(breaks = -3:3,
                     labels = c("Completely Distrust","Distrust","Somewhat Distrust","Control",
                                "Somewhat Trust","Trust","Completely Trust"))
dev.off()

#Add table with average responses by trust#
confidence_interval <- function(vector, interval) {
  error <- qt((interval + 1)/2, df = length(vector)-1) * (sd(vector) / sqrt(length(vector)))
  result <- c("lower" = mean(vector) - error, "upper" = mean(vector) + error)
  return(result)
}

#Key and main table#
tkey2<-trust_key
tkey2$text<-as.character(tkey2$text)
tkey2<-rbind(tkey2,c("Control",0))
store_tr<-data.frame(Trust=c("Completely Distrust","Distrust","Somewhat Distrust","Control",
                             "Somewhat Trust","Trust","Completely Trust"),
                     Willing=NA,CI=NA)
for(i in 1:nrow(store_tr)){
  # i<-1
  store_v<-dat$willing[dat$trust_in_source==as.numeric(tkey2$number[tkey2$text==as.character(store_tr$Trust[i])])]
  store_tr$Willing[i]<-round(mean(store_v),2)
  store_tr$CI[i]<-paste0("[",paste(round(confidence_interval(store_v,0.95),2),collapse=", "),"]")
}
store_tr
names(store_tr)<-c("Trust in Message Source","Proportion Willing to Store Water","95% Confidence Interval")
stargazer(store_tr,rownames = F,summary = F,out="table_m4_store_by_trust_table.html")

# MAIN MODELS Robustness Checks -------------------------------------------

#Main model, only respondents with CA lat/lon#
datCa<-dat[dat$CA_latlon==1,] #Data subset for repeated models from above
#Model 1#
log1a<-glm(willing ~ prior_lockdown,"binomial",datCa)
log1ar<-coeftest(log1a,vcov=vcovHC(log1a,"HC1")) #Robust pooling model
#Model 2#
log1b<-glm(willing ~ trust_in_source,"binomial",datCa)
log1br<-coeftest(log1b,vcov=vcovHC(log1b,"HC1")) #Robust pooling model
#Model 3#
log1c<-glm(willing ~ prior_lockdown + trust_in_source,"binomial",datCa)
log1cr<-coeftest(log1c,vcov=vcovHC(log1c,"HC1")) #Robust pooling model
#Model 4#
log1d<-glm(willing ~ prior_lockdown + trust_in_source + white + male + repub + age + age2 + coll + inc + children + employed + inc + days_in_lockdown,"binomial",datCa)
log1dr<-coeftest(log1d,vcov=vcovHC(log1d,"HC1")) #Robust pooling model

stargazer(log1ar,log1br,log1cr,log1dr,add.lines = list(c("Observations",nrow(log1a$model),nrow(log1b$model),nrow(log1c$model),nrow(log1d$model)),
                                                       c("AIC",round(log1a$aic,2),round(log1b$aic,2),round(log1c$aic,2),round(log1d$aic,2))),
          covariate.labels=covLab(log1d),
          dep.var.labels = rep("",4),column.labels = NULL,model.names = F,
          dep.var.caption = "Dependent variable: Willing to Prepare Water for Earthquake",
          out="table_s17_mainLogit_models_onlyCALatLon.html")

#Main model, only respondents with time above 3 minutes#
datTi<-dat[dat$time>3,] #Data subset#
#Model 1#
log1a<-glm(willing ~ prior_lockdown,"binomial",datTi)
log1ar<-coeftest(log1a,vcov=vcovHC(log1a,"HC1")) #Robust pooling model
#Model 2#
log1b<-glm(willing ~ trust_in_source,"binomial",datTi)
log1br<-coeftest(log1b,vcov=vcovHC(log1b,"HC1")) #Robust pooling model
#Model 3#
log1c<-glm(willing ~ prior_lockdown + trust_in_source,"binomial",datTi)
log1cr<-coeftest(log1c,vcov=vcovHC(log1c,"HC1")) #Robust pooling model
#Model 4#
log1d<-glm(willing ~ prior_lockdown + trust_in_source + white + male + repub + age + age2 + coll + inc + children + employed + inc + days_in_lockdown,"binomial",datTi)
log1dr<-coeftest(log1d,vcov=vcovHC(log1d,"HC1")) #Robust pooling model

stargazer(log1ar,log1br,log1cr,log1dr,add.lines = list(c("Observations",nrow(log1a$model),nrow(log1b$model),nrow(log1c$model),nrow(log1d$model)),
                                                       c("AIC",round(log1a$aic,2),round(log1b$aic,2),round(log1c$aic,2),round(log1d$aic,2))),
          covariate.labels=covLab(log1d),
          dep.var.labels = rep("",4),column.labels = NULL,model.names = F,
          dep.var.caption = "Dependent variable: Willing to Prepare Water for Earthquake",
          out="table_s18_mainLogit_models_only5minPlus.html")

#Main model effects by various day in lockdown cutoffs#
dilCutLD<-data.frame(Day=0:max(dat$days_in_lockdown),Beta=NA,Lower=NA,Upper=NA)
for(d in 0:14){
  # d<-0
  glt0<-glm(willing ~ prior_lockdown,"binomial",dat[dat$days_in_lockdown<=d,])
  glt<-coeftest(glt0,vcov=vcovHC(glt0,"HC1")) #Robust pooling model
  dilCutLD$Beta[dilCutLD$Day==d]<-glt[2,1]
  dilCutLD$Lower[dilCutLD$Day==d]<-confint(glt)[2,1]
  dilCutLD$Upper[dilCutLD$Day==d]<-confint(glt)[2,2]
  if(d %in% c(0:2)){
    assign(paste0("dl",d),glt0)
    assign(paste0("dlr",d),glt)
  }
}

jpeg("figure_m2_lockdownFX_byDayCutoff.jpeg",width = 1200,height = 600)
ggplot(dilCutLD, aes(Day, Beta)) +
  ggtitle("Figure 2: Lockdown Effect and Confidence Interval by Days Since Statewide Lockdown") + 
  geom_line(aes(group = 1)) + theme(text = element_text(size=16)) +
  geom_errorbar( aes(ymin = Lower, ymax = Upper),width = 0.2) +
  geom_point(size = 2)
dev.off()

stargazer(dlr0,dlr1,dlr2,add.lines=list(c("Observations",nrow(dl0$model),nrow(dl1$model),nrow(dl2$model)),
                                        c("AIC",round(dl0$aic,2),round(dl1$aic,2),round(dl2$aic,2))),
          covariate.labels=covLab(dl0),
          dep.var.labels = rep("",3),column.labels = c("Day 1","Days 1 - 2","Days 1 - 3"),model.names = F,
          dep.var.caption = "Dependent variable: Willing to Prepare Water for Earthquake",
          out="table_m2_lockdown_models_daySubset.html")

#Days under lockdown instead of prior lockdown condition, models 1, 3, and 4#
#Model 1#
log1ar<-glm(willing ~ days_in_lockdown,"binomial",dat)
log1arr<-coeftest(log1ar,vcov=vcovHC(log1ar,"HC1")) #Robust pooling model
#Model 2#
log1cr<-glm(willing ~ days_in_lockdown + trust_in_source,"binomial",dat)
log1crr<-coeftest(log1cr,vcov=vcovHC(log1cr,"HC1")) #Robust pooling model
#Model 3#
log1dr<-glm(willing ~ days_in_lockdown + trust_in_source + white + male + repub + age + age2 + coll + inc + children + employed,"binomial",dat)
log1drr<-coeftest(log1dr,vcov=vcovHC(log1dr,"HC1")) #Robust pooling model

stargazer(log1arr,log1crr,log1drr,add.lines = list(c("Observations",nrow(log1ar$model),nrow(log1cr$model),nrow(log1dr$model)),
                                                   c("AIC",round(log1ar$aic,2),round(log1cr$aic,2),round(log1dr$aic,2))),
          dep.var.labels = "",column.labels = NULL,model.names = F,
          dep.var.caption = "Dependent variable: Willing to Prepare Water for Earthquake",
          covariate.labels=covLab(log1dr),out="table_s12_mainLogit_models_daysNotPrior.html")

#Replicate main models omitting those who prepared water for Covid: OLS (olsNCW) and logit (logNCW)#
dat_NCW<-dat[which(dat$covid_water==0),] #Data subset#

#Model 1#
olsNCWa<-lm(post_dv ~ prior_lockdown + trust_in_source,dat_NCW)
olsNCWar<-coeftest(olsNCWa,vcov=vcovHC(olsNCWa,"HC1")) #Robust pooling model
#Model 2#
olsNCWb<-lm(post_dv ~ prior_lockdown + trust_in_source + white + male + repub + age + age2 + coll + inc + children + employed,dat_NCW)
olsNCWbr<-coeftest(olsNCWb,vcov=vcovHC(olsNCWb,"HC1")) #Robust pooling model

#Model 1#
logNCWa<-glm(willing ~ prior_lockdown + trust_in_source,"binomial",dat_NCW)
logNCWar<-coeftest(logNCWa,vcov=vcovHC(logNCWa,"HC1")) #Robust pooling model
#Model 2#
logNCWb<-glm(willing ~ prior_lockdown + trust_in_source + white + male + repub + age + age2 + coll + inc + children + employed,"binomial",dat_NCW)
logNCWbr<-coeftest(logNCWb,vcov=vcovHC(logNCWb,"HC1")) #Robust pooling model

stargazer(olsNCWar,olsNCWbr,logNCWar,logNCWbr,add.lines=(list(c("Observations",nrow(olsNCWa$model),nrow(olsNCWb$model),nrow(logNCWa$model),nrow(logNCWb$model)),
                                                              c("Adj. R2",round(summary(olsNCWa)$adj.r.squared,2),round(summary(olsNCWb)$adj.r.squared,2),"",""),
                                                              c("AIC","","",round(logNCWa$aic,2),round(logNCWb$aic,2)))),
          dep.var.labels = "",column.labels = c(rep("OLS",2),rep("Logistic",2)),model.names = F,
          dep.var.caption = "Dependent variable: Willingness to Prepare Water for Earthquake",
          covariate.labels=covLab(olsNCWb),out="table_s4_main_models_noCovidWater.html")

#Replicate main models with only urban water recipients (main providers selected in utility)#
dat_MWP<-dat[which(!dat$Utility.provider %in% c("","Other")),] #Data subset#

#Model 1#
olsMWPa<-lm(post_dv ~ prior_lockdown + trust_in_source,dat_MWP)
olsMWPar<-coeftest(olsMWPa,vcov=vcovHC(olsMWPa,"HC1")) #Robust pooling model
#Model 2#
olsMWPb<-lm(post_dv ~ prior_lockdown + trust_in_source + white + male + repub + age + age2 + coll + inc + children + employed,dat_MWP)
olsMWPbr<-coeftest(olsMWPb,vcov=vcovHC(olsMWPb,"HC1")) #Robust pooling model

#Model 1#
logMWPa<-glm(willing ~ prior_lockdown + trust_in_source,"binomial",dat_MWP)
logMWPar<-coeftest(logMWPa,vcov=vcovHC(logMWPa,"HC1")) #Robust pooling model
#Model 2#
logMWPb<-glm(willing ~ prior_lockdown + trust_in_source + white + male + repub + age + age2 + coll + inc + children + employed,"binomial",dat_MWP)
logMWPbr<-coeftest(logMWPb,vcov=vcovHC(logMWPb,"HC1")) #Robust pooling model

stargazer(olsMWPar,olsMWPbr,logMWPar,logMWPbr,add.lines=(list(c("Observations",nrow(olsMWPa$model),nrow(olsMWPb$model),nrow(logMWPa$model),nrow(logMWPb$model)),
                                                              c("Adj. R2",round(summary(olsMWPa)$adj.r.squared,2),round(summary(olsMWPb)$adj.r.squared,2),"",""),
                                                              c("AIC","","",round(logMWPa$aic,2),round(logMWPb$aic,2)))),
          dep.var.labels = "",column.labels = c(rep("OLS",2),rep("Logistic",2)),model.names = F,
          dep.var.caption = "Dependent variable: Willingness to Prepare Water for Earthquake",
          covariate.labels=covLab(olsMWPb),out="table_s5_main_models_majorWaterProvider.html")

#Supplementary models predicting outcomes with organizations in OLS (olsTR) and logistic (logTR)#
#Model 1 OLS#
olsTR<-lm(post_dv ~ FEMA + CityGovt + RedCross + Academics + Utility,dat)
olsTRr<-coeftest(olsTR,vcov=vcovHC(olsTR,"HC1")) #Robust pooling model
#Model 2 Logit#
logTR<-glm(willing ~ FEMA + CityGovt + RedCross + Academics + Utility,"binomial",dat)
logTRr<-coeftest(logTR,vcov=vcovHC(logTR,"HC1")) #Robust pooling model

stargazer(olsTRr,logTRr,add.lines=(list(c("Observations",nrow(olsTR$model),nrow(logTR$model)),
                                        c("Adj. R2",round(summary(olsTR)$adj.r.squared,2),""),
                                        c("AIC","",round(logTR$aic,2)))),
          dep.var.labels = "",column.labels = c(rep("OLS",1),rep("Logistic",1)),model.names = F,
          dep.var.caption = "Dependent variable: Willingness to Prepare Water",
          out="table_s6_supplementary_outcomesByOrg_models.html")

#Robustness to ordinal logistic model (olog1)#
#Model 1 - main covariates#
olog1a <- polr(factor(post_dv) ~ prior_lockdown + trust_in_source, data = dat, Hess=TRUE)
#Model 2 - added controls#
olog1b <- polr(factor(post_dv) ~ prior_lockdown + trust_in_source + white + male + repub + age + coll + inc + children + employed, data = dat, Hess=TRUE)

#Output#
stargazer(olog1a,olog1b,covariate.labels=covLab(olog1b),
          dep.var.labels = rep("",2),column.labels = NULL,model.names = F,
          dep.var.caption = "Dependent variable: Willingness to Prepare (Factored 7 point)",
          out="table_s2_mainModels_OLogit.html")

# SIMULATION with main logit models ---------------------------------------

#Main logit model back#
log1d<-glm(willing ~ prior_lockdown + trust_in_source + white + male + repub + age + age2 + coll + inc + children + employed + inc + days_in_lockdown,"binomial",dat)
summary(log1d)

#Get trust values from main data#
trDF<-dat[,names(dat)[grep("_likert",names(dat))]]

#Create row-wise simulated most and lowest trust outcomes, lowest and highest lockdown outcomes#
t0l0<-data.frame(prior_lockdown=0,trust_in_source=apply(trDF,1,"min"))
t0l0<-cbind(t0l0,log1d$model[,-c(1:3)])
t1l0<-data.frame(prior_lockdown=0,trust_in_source=apply(trDF,1,"max"))
t1l0<-cbind(t1l0,log1d$model[,-c(1:3)])
t0l1<-data.frame(prior_lockdown=1,trust_in_source=apply(trDF,1,"min"))
t0l1<-cbind(t0l1,log1d$model[,-c(1:3)])
t1l1<-data.frame(prior_lockdown=1,trust_in_source=apply(trDF,1,"max"))
t1l1<-cbind(t1l1,log1d$model[,-c(1:3)])

#Use model to predict row-level outcome probability in each scenario#
pt0l0<-predict.glm(log1d,t0l0,type="response")
pt1l0<-predict.glm(log1d,t1l0,type="response")
pt0l1<-predict.glm(log1d,t0l1,type="response")
pt1l1<-predict.glm(log1d,t1l1,type="response")

#Simulate 100,000 outcomes with set seed#
set.seed(5165)
bootN<-100000
bootDF<-data.frame(t0l0=rep(NA,bootN),t1l0=rep(NA,bootN),
                   t0l1=rep(NA,bootN),t1l1=rep(NA,bootN))
for(b in 1:bootN){
  bootDF$t0l0[b]<-mean(rbinom(length(pt0l0),1,pt0l0))
  bootDF$t1l0[b]<-mean(rbinom(length(pt1l0),1,pt1l0))
  bootDF$t0l1[b]<-mean(rbinom(length(pt0l1),1,pt0l1))
  bootDF$t1l1[b]<-mean(rbinom(length(pt1l1),1,pt1l1))
}

#Data frame for differences, averages at column level, confidence intervals#
diffDF<-data.frame(t1l0=bootDF$t1l0-bootDF$t0l0,
                   t0l1=bootDF$t0l1-bootDF$t0l0,
                   t1l1=bootDF$t1l1-bootDF$t0l0)
colMeans(diffDF)
ci10<-round(100*confidence_interval(diffDF$t1l0,0.95),2)
ci01<-round(100*confidence_interval(diffDF$t0l1,0.95),2)
ci11<-round(100*confidence_interval(diffDF$t1l1,0.95),2)

#Create and output scenario comparison table#
bootDFS<-data.frame(Condition=c("No Lockdown, Low Trust","No Lockdown, High Trust","All Lockdown, Low Trust","All Lockdown, High Trust"),
                   Proportion=paste0(round(100*colMeans(bootDF),2),"%"),
                   Difference=c("-",paste0(round(100*colMeans(diffDF),2),"%")),
                   Confidence=c("-",paste("(",ci10[1],"%, ",ci10[2],"%)",sep=""),
                                paste("(",ci01[1],"%, ",ci01[2],"%)",sep=""),
                                paste("(",ci11[1],"%, ",ci11[2],"%)",sep="")))
names(bootDFS)<-c("Simulation Condition","Proportion Willing to Store Water",
                  "Difference in Proportion","95% Confidence Interval")
stargazer(bootDFS,summary = F,rownames = F,out="table_m5_simulatedProportions_byCondition.html")

#Data frames for plotting simulated distributions#
b10<-data.frame(Proportion=c(bootDF$t0l0,bootDF$t1l0),Type=c(rep("Low Trust, No Prior Lockdown",nrow(bootDF)),rep("High Trust, No Prior Lockdown",nrow(bootDF))))
b01<-data.frame(Proportion=c(bootDF$t0l0,bootDF$t0l1),Type=c(rep("Low Trust, No Prior Lockdown",nrow(bootDF)),rep("Low Trust, All Prior Lockdown",nrow(bootDF))))
b11<-data.frame(Proportion=c(bootDF$t0l0,bootDF$t1l1),Type=c(rep("Low Trust, No Prior Lockdown",nrow(bootDF)),rep("High Trust, All Prior Lockdown",nrow(bootDF))))

#Plot simulated distributions#
g10<-ggplot(b10,aes(x=Proportion,fill=Type)) + geom_density() + ylab("Density") + theme(legend.title = element_blank(),text = element_text(size=18),legend.position = "bottom") + guides(fill = guide_legend(reverse = TRUE))
g01<-ggplot(b01,aes(x=Proportion,fill=Type)) + geom_density() + ylab("Density") + theme(legend.title = element_blank(),text = element_text(size=18),legend.position = "bottom") + guides(fill = guide_legend(reverse = TRUE))
g11<-ggplot(b11,aes(x=Proportion,fill=Type)) + geom_density() + ylab("Density") + theme(legend.title = element_blank(),text = element_text(size=18),legend.position = "bottom") + guides(fill = guide_legend(reverse = TRUE))

#Output#
jpeg("figure_m4_simulatedProportions_byCondition_plots.jpeg",width=1500,height=425)
grid.arrange(g10,g01,g11,nrow=1,top=textGrob("Figure 4: Comparative Bootstrap Simulated Proportion of Sample Storing Water",gp=gpar(fontsize=20)))
dev.off()

# MATCHING attempt at main logit models ---------------------------------

#Diagnose match imbalance across observed data#
m_ps <- glm(prior_lockdown ~ trust_in_source + white + male + repub + age + age2 + 
              coll + inc + children + employed + inc + days_in_lockdown,
            family = binomial(), data = dat)
prs_df<-data.frame(pr_score = predict(m_ps, type = "response"),
                   pld = m_ps$model$prior_lockdown)

#Build matching data frame for matched models#
cols<-c("willing","prior_lockdown","trust_in_source", "white", "male", "repub", "age", "age2", 
        "coll", "inc", "children", "employed", "days_in_lockdown")
aux_df<-dat[,cols]
aux_df <- aux_df[complete.cases(aux_df), ]

#Match on covariates of interest and retain original outcomes and predictors#
set.seed(5165)
mod_match <- matchit(prior_lockdown ~ white + male + repub + age + age2 + 
                     coll + inc + children + employed,
                     data = aux_df,method = "nearest")
summary(mod_match)

#Get matched data for use#
dta_m <- match.data(mod_match)
dim(dta_m)

#Cycle over covariates to assess balance#
cols2<-cols[-c(1:3,13)]
lapply(cols2, function(v) {
  t.test(dta_m[, v] ~ dta_m$prior_lockdown)
})

#Same approach as above, test with two matrix comparison#
mat1<-as.matrix(dta_m$prior_lockdown)
mat2<-as.matrix(dta_m[,cols2])
ld<-(t(mat2) %*% mat1)/sum(mat1)
nld<-(t(mat2) %*% abs(1-mat1))/sum(1-mat1)
ldbalm<-cbind(ld,nld)
row.names(ldbalm)<-c("White","Male","Republican","Age","Age Squared","College",
                     "Income (Scale)","Children","Employed")
colnames(ldbalm)<-c("Prior Lockdown","No Prior Lockdown")
ldbalm<-round(ldbalm,2)
ldbalm

#Assess significance of differences#
mat2<-as.data.frame(mat2)
ldbalm<-as.data.frame(ldbalm)
ldbalm$Difference<-NA
for(i in 1:ncol(mat2)){
  tt<-t.test(mat2[,i]~dta_m$prior_lockdown)
  difft<-round(tt$estimate[2]-tt$estimate[1],2)
  pt<-tt$p.value
  if(pt<0.01){
    difft<-paste0(difft,"***")
  } else if(pt<0.05){
    difft<-paste0(difft,"**")
  } else if(pt<0.1){
    difft<-paste0(difft,"*")
  }
  ldbalm$Difference[i]<-difft
}

#Output balance after matching#
stargazer(ldbalm,out="table_s15_matching_balance_table.html",summary = F)

#Run main models again with matched data: constrained, imbalanced covariates, full set#
logmatch1<-glm(willing ~ prior_lockdown, "binomial",data = dta_m)
logmatch1r<-coeftest(logmatch1,vcov. = vcovHC(logmatch1,"HC1"))

logmatch2<-glm(willing ~ prior_lockdown + trust_in_source + white + male + repub + age + age2 + 
                   coll + inc + children + employed + days_in_lockdown, "binomial",data = dta_m)
logmatch2r<-coeftest(logmatch2,vcov. = vcovHC(logmatch2,"HC1"))
logmatch2r

#Output models for SI#
stargazer(logmatch1r,logmatch2r,covariate.labels=covLab(logmatch2),
          add.lines=list(c("Observations",nrow(logmatch1$model),nrow(logmatch2$model)),
                         c("AIC",round(logmatch1$aic,2),round(logmatch2$aic,2))),
          dep.var.labels = "",column.labels = NULL,model.names = F,
          dep.var.caption = "Dependent variable: Willing to Prepare Water for Earthquake",
          out="table_s16_main_models_matching.html")

# Regression: Predicting Trust in Organizations and Covid Storage ---------------------------

#Trust in organization#
for(i in 1:length(orgsName)){
  # i<-1
  if(i!=3){
    ft<-as.formula(paste0(paste(orgsName[i],"likert",sep="_"),"~ white + male + repub + age + age2 + coll + inc + children + employed + inc + days_in_lockdown"))
    lmt<-lm(ft,dat)
    lmtr<-coeftest(lmt,vcov. = vcovHC(lmt,"HC1"))
    assign(paste0("otlm_",orgsName[i]),lmt)
    assign(paste0("otlmR_",orgsName[i]),lmtr)
  }
}
stargazer(otlmR_FEMA,otlmR_Academics,otlmR_CityGovt,otlmR_RedCross,otlmR_Utility,
          add.lines=list(c("Observations",nrow(otlm_FEMA$model),nrow(otlm_Academics$model),
                           nrow(otlm_CityGovt$model),nrow(otlm_RedCross$model),nrow(otlm_Utility$model)),
                         c("Adj. R2",round(summary(otlm_FEMA)$adj.r.squared,3),round(summary(otlm_Academics)$adj.r.squared,3),
                           round(summary(otlm_CityGovt)$adj.r.squared,3),round(summary(otlm_RedCross)$adj.r.squared,3),
                           round(summary(otlm_Utility)$adj.r.squared,3))),covariate.labels = covLab(otlm_Academics),
          dep.var.labels = "",column.labels = c("FEMA","Academics","City Gov't","Red Cross","Water Provider"),model.names = F,
          dep.var.caption = "Dependent variable: Trust in Organizations (-3 to 3 scale)",
          out="table_s11_orgTrust_models.html")

#Storing Covid goods#
dat$covid_prep<-as.numeric(dat$Covid.prep.binary=="Yes")
dat$covid_water<-0
dat$covid_food<-0
dat$covid_medicine<-0
dat$covid_medsupplies<-0
dat$covid_toiletpaper<-0
dat$covid_luxury<-0
dat$covid_guns<-0
for(i in 1:nrow(dat)){
  # i<-989
  if(dat$What.covid.prep[i]!=""){
    strt<-as.character(unlist(strsplit(as.character(dat$What.covid.prep[i]),split=",")))
    dat$covid_water[i]<-as.numeric("Water" %in% strt)
    dat$covid_food[i]<-as.numeric("Canned and dry food" %in% strt)
    dat$covid_medicine[i]<-as.numeric("Medicine" %in% strt)
    dat$covid_medsupplies[i]<-as.numeric("Medical supplies" %in% strt)
    dat$covid_toiletpaper[i]<-as.numeric("Toilet paper" %in% strt)
    dat$covid_luxury[i]<-as.numeric("Luxury items (alcohol" %in% strt)
    dat$covid_guns[i]<-as.numeric("Firearms and/or ammunition" %in% strt)
  }
}

#Main outcomes, models for each with main controls#
prepDVs<-c("willing","prep","water","food","medicine","medsupplies","toiletpaper","luxury","guns")
ns<-rep(NA,9)
aics<-rep(NA,9)
for(p in prepDVs){
  # p<-prepDVs[1]
  if(p=="willing"){
    ft<-as.formula(paste0(p,"~ white + male + repub + age + age2 + coll + inc + children + employed + inc + days_in_lockdown"))
  } else {
    ft<-as.formula(paste0(paste0("covid_",p),"~ white + male + repub + age + age2 + coll + inc + children + employed + inc + days_in_lockdown"))
  }
  lmt<-glm(ft,"binomial",dat)
  lmtr<-coeftest(lmt,vcov. = vcovHC(lmt,"HC1"))
  assign(paste0("cplm_",which(prepDVs==p)),lmt)
  assign(paste0("cplmr_",which(prepDVs==p)),lmtr)
  ns[which(prepDVs==p)]<-nrow(lmt$model)
  aics[which(prepDVs==p)]<-round(lmt$aic,2)
}

#Output#
s13names<-c("Willing: Earthquake Water","Binary: Prepared for Covid","Water for Covid","Food for Covid",
            "Medicine for Covid","Medical Supplies for Covid","Toilet Paper for Covid","Luxury Goods for Covid",
            "Guns or Ammo for Covid")
stargazer(cplmr_1,cplmr_2,cplmr_3,cplmr_4,cplmr_5,cplmr_6,cplmr_7,cplmr_8,cplmr_9,
          add.lines=list(c("Observations",ns),c("AIC",aics)),
          dep.var.labels = "",column.labels = s13names,model.names = F,
          dep.var.caption = "Dependent variable: Reported Preparation Willingness (Model 1) or Preparation for Covid (Other Models)",
          out="table_s13_covid_prep_models.html",covariate.labels = covLab(cplm_9))
