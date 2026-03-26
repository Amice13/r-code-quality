################################################################## 
# Replication script for "The Influence...", 
# March 2017
# Frederik Hjorth
# Dept. of Political Science
# University of Copenhagen
################################################################## 

#set working dir (i.e., folder where the dataverse folders are extracted to)
setwd("~/Dropbox/research/crime")

#load required packages
require(stargazer) #table presentation
require(rms) #clustered se's
require(Amelia) #multiple imputation
require(reshape2) #converting between long/wide data
require(effects) #marginal effects
require(ggplot2) #plotting
require(car) #recoding
require(haven) #read stata files
require(magrittr) #piping


################################################################## 
# MANUSCRIPT
################################################################## 

############################
## FIGURE 1
############################

#get municipality-level share of non-western immigrants and descendants by year (tables dl'ed from Statistics Denmark)
dst19802006tab<-read.table("data/dst19802006tab.txt",sep="\t",dec=",",header=T)
dst20072009tab<-read.table("data/dst20072009tab.txt",sep="\t",dec=",",header=T)
dst20102014tab<-read.table("data/dst20102014tab.txt",sep="\t",dec=",",header=T)
dst.allyrs<-rbind(melt(dst19802006tab,id.vars="muni"),melt(dst20072009tab,id.vars="muni"),melt(dst20102014tab,id.vars="muni"))
dst.allyrs$year<-as.numeric(substr(dst.allyrs$variable,2,5))

#get zip code-level data (calculated from registry data)
zipscovs<-readRDS("data/zipscovs.rds")

#gather all muni-years and zip-years in one data set
munizipnwimm<-data.frame(unit=c(dst.allyrs$muni,zipscovs$zip),year=c(dst.allyrs$year,zipscovs$year),nwimm=c(dst.allyrs$value,zipscovs$nwimm),level=c(rep("Municipalities",nrow(dst.allyrs)),rep("Zip codes",nrow(zipscovs))))

ggplot(munizipnwimm,aes(x=factor(year),y=100*nwimm)) + 
  geom_boxplot(outlier.size=1) +
  scale_x_discrete(breaks=c(1980,1985,1990,1995,2000,2005,2010)) +
  theme_bw() +
  xlab("") +
  ylab("Unit-level share of non-western immigrants and descendants") +
  facet_grid(.~level,scales="free_x") +
  ylim(c(0,40))

ggsave(file="figures/crime_figure1.pdf",width=9,height=5)

############################
## FIGURE 2
############################

#load data on sizes of administrative units
upop<-read.csv("data/unitpops_inczips.csv",sep=";")
upop$rank<-3
upop$rank[upop$type=="dkoldmunis"]<-1
upop$rank[upop$type=="dknewmunis"]<-2
upop$rank[upop$type=="dkzips"]<-4
upop$rank[upop$type=="uszips"]<-5

#assign labels
levels(upop$type)<-c("DK muni's ('07-)","DK muni's (-'06)","DK zip codes","US counties","US zip codes")
upop$pop<-as.numeric(as.character(upop$pop))
upop$level<-factor(upop$level1or2)
levels(upop$level)<-c("Lower level","Higher level")

ggplot(upop,aes(reorder(type,rank),pop)) +
  geom_boxplot(outlier.size=1) +
  annotation_logticks(sides="l") +
  scale_y_log10(breaks=c(10,100,1000,10000,100000,1000000),labels=c("10","100","1,000","10,000","100,000","1,000,000")) +
  facet_grid(.~level, scale="free_x", space = "free_x") +
  theme_bw() +
  xlab("") +
  ylab("Population (logged)")
ggsave("figures/crime_figure2.pdf",width=11,height=5)

#housekeeping
rm(list=ls())

############################
## TABLE 1
############################

#load municipality data
md<-readRDS(file="data/munidata.rds")
md_im<-readRDS(file="data/munidata_im.rds")

#specify models - the term of interest here is "exat*immlevel" at the end, i.e. anti-imm. attitudes interacted with ethnic diversity
tab1m1<-as.formula(vcrim~female+age+edu+hinc+emp.student+emp.pension+muniedu+munipop+exat*immlevel+muninum)
tab1m2<-as.formula(vcrim~female+age+edu+hinc+emp.student+emp.pension+muniedu+munipop+leftvot.p+lrscale+exat*immlevel+muninum)
tab1m3<-as.formula(vcrim~female+age+edu+hinc+emp.student+emp.pension+muniedu+munipop+exat*immlevel+muninum+elec)
tab1m4<-as.formula(vcrim~female+age+edu+hinc+emp.student+emp.pension+muniedu+munipop+leftvot.p+lrscale+exat*immlevel+muninum+elec)
munimodels<-list(tab1m1,tab1m2,tab1m3,tab1m4)

#function that takes a model, estimates with clustered se's by muni-year on each imputed data set in the amelia object, 
# returns object (miests) with the resulting combined coefficients and se's as well as a placeholder model for setting up the table in stargazer
# coefficients and se's are combined across imputations using mi.meld() from the Amelia package, cf. Rubin (1987)

getmiests<-function(formula){
  require(rms)
  require(Amelia)
  lrm_amout<-lapply(md_im$imputations, function(i) robcov(lrm(formula,data=i,x=T,y=T),cluster=i$munielec))
  coefs_amout<-do.call(rbind, lapply(lrm_amout, function(i) i$coefficients))
  ses_amout<-do.call(rbind, lapply(lrm_amout, function(i) sqrt(diag(i$var))))
  meldedests<-mi.meld(coefs_amout,ses_amout)
  miests<-list(coef=meldedests[[1]],se=meldedests[[2]],phm=glm(formula,data=md_im$imputations$imp1,family="binomial"))
  phmcoefnames<-attr(miests[[3]]$coefficients,"names")
  attr(miests[[1]],"dimnames")[[2]]<-phmcoefnames
  attr(miests[[2]],"dimnames")[[2]]<-phmcoefnames
  return(miests)
}

#get estimates for each of the four models in Table 1
tab1m1ests<-getmiests(munimodels[[1]])
tab1m2ests<-getmiests(munimodels[[2]])
tab1m3ests<-getmiests(munimodels[[3]])
tab1m4ests<-getmiests(munimodels[[4]])

#print Table 1
table1<-stargazer(tab1m1ests[[3]],tab1m2ests[[3]],tab1m3ests[[3]],tab1m4ests[[3]],style="apsr",title="Models using municipality data",
                  intercept.bottom=T,digits=2,dep.var.caption="Prefer stricter punishments for violent crime",
                  coef=list(tab1m1ests$q.mi[1,],tab1m2ests$q.mi[1,],tab1m3ests$q.mi[1,],tab1m4ests$q.mi[1,]),
                  se=list(tab1m1ests$se.mi[1,],tab1m2ests$se.mi[1,],tab1m3ests$se.mi[1,],tab1m4ests$se.mi[1,]),
                  font.size="footnotesize",label="table1",dep.var.labels="Prefer stricter punishments for violent crime",
                  column.sep.width="0pt",omit=c("elec","muninum"),omit.stat="chi2",df=F,star.cutoffs=c(.05,.01,.001),
                  order=c(11,12,344,7,8),
                  covariate.labels=c("Anti-immigration (Imm)","Ethnic diversity (ED)","Imm*ED","Muni. education level",
                                      "Muni population","Gender (f)","Age","Education level","Household income","Student",
                                      "Pensioner","Left-wing voter","Left/right self-placement","Intercept"))

#inset checkmarks for year and muni FE
checkmarks<-c("Municipality fixed effects & \\checkmark & \\checkmark & \\checkmark & \\checkmark \\\\","Year fixed effects &  &  & \\checkmark & \\checkmark \\\\")
concontrols<-c("\\textit{Context-level controls:} & & & &  \\\\ ")
indcontrols<-c("\\textit{Individual-level controls:} & & & & \\\\ ")  

table1<-c(table1[1:41],checkmarks,table1[42:length(table1)])
table1[17]<-gsub("Imm\\*ED","Imm $\\\\times$ ED",table1[17])
table1<-c(table1[1:18],concontrols,table1[19:22],indcontrols,table1[23:49])

#export a tex table (for inclusion in the manuscript) 
writeLines(table1,"tables/crime_table1.txt")

#for replicators, here's Table 1 formatted for R output
stargazer(tab1m1ests[[3]],tab1m2ests[[3]],tab1m3ests[[3]],tab1m4ests[[3]],
          style="apsr",type="text",omit=c("elec","muninum"),order=c(11,12,344,7,8))

#housekeeping
rm(list=objects(pattern="tab"))

############################
## TABLE 2
############################

#load zip code data
zd<-readRDS(file="data/zipdata.rds")

#specify models
tab2m1<-robcov(ols(crimeconcern~nwimm*immiconcern+avgincome+avgeduyrs+pop+fullzipfac,
                   data=zd,x=T,y=T),cluster=zd$fullzipyear)
tab2m2<-robcov(ols(crimeconcern~nwimm*immiconcern+avgincome+avgeduyrs+pop+female+age+agesq+edu+fullzipfac,
                   data=zd,x=T,y=T),cluster=zd$fullzipyear)
tab2m3<-robcov(ols(crimeconcern~nwimm*immiconcern+avgincome+avgeduyrs+pop+fullzipfac+yearfac,
                   data=zd,x=T,y=T),cluster=zd$fullzipyear)
tab2m4<-robcov(ols(crimeconcern~nwimm*immiconcern+avgincome+avgeduyrs+pop+female+age+agesq+edu+fullzipfac+yearfac,
                   data=zd,x=T,y=T),cluster=zd$fullzipyear)

#stargazer
checkmarks<-c("Zip code fixed effects & \\checkmark & \\checkmark & \\checkmark & \\checkmark \\\\","Year fixed effects &  &  & \\checkmark & \\checkmark \\\\")
concontrols<-c("\\textit{Context-level controls:} & & & &  \\\\ ")
indcontrols<-c("\\textit{Individual-level controls:} & & & & \\\\ ") 

table2<-stargazer(tab2m1,tab2m2,tab2m3,tab2m4,style="apsr",title="Models using zip code data",intercept.bottom=T,
                  digits=2,dep.var.caption="Concerned about crime",font.size="footnotesize",label="table2",
                  dep.var.labels="Concern about crime",column.sep.width="0pt",omit.stat=c("f"),df=F,
                  omit=c("fullzipfac","yearfac"),star.cutoffs=c(.05,.01,.001),order=c(2,1,106),
                  covariate.labels=c("Immigration concern (Imm)","Ethnic diversity (ED)","Imm*ED","Zip avg. income",
                                     "Zip avg. education","Zip population","Gender (f)","Age","Age$^2$","Education"))
table2
table2<-c(table2[1:35],checkmarks,table2[36:length(table2)])
table2[17]<-gsub("Imm\\*ED","Imm $\\\\times$ ED",table2[17])
table2[33]<-gsub("Constant","Intercept",table2[33])
table2<-c(table2[1:18],concontrols,table2[19:length(table2)])
table2<-c(table2[1:25],indcontrols,table2[26:length(table2)])
table2
writeLines(table2,con="tables/crime_table2.txt")

#for replicators, here's Table 1 formatted for R output
stargazer(tab2m1,tab2m2,tab2m3,tab2m4,
          style="apsr",type="text",omit=c("fullzipfac","yearfac"),star.cutoffs=c(.05,.01,.001),order=c(2,1,106))

#housekeeping
#rm(list=setdiff(objects(pattern="tab"),"tab2m3"))

############################
## FIGURE 3
############################

#calculate marginal effects for municipality data
#(in both cases, I use estimates from model 3, my preferred specification)
tab1m3phm<-getmiests(munimodels[[3]])[[3]]
tab1m3.coefs<-coef(tab1m3phm)
tab1m3.cov<-vcov(tab1m3phm)
immlevel.seq<-seq(from=0,to=.44,by=.01)
dy.dx<-tab1m3.coefs["exat"]+tab1m3.coefs["exat:immlevel"]*immlevel.seq
se.dy.dx<-sqrt(tab1m3.cov["exat","exat"]+immlevel.seq^2*tab1m3.cov["exat:immlevel","exat:immlevel"]+immlevel.seq*2*tab1m3.cov["exat","exat:immlevel"])
tab1m3.margins<-data.frame(immlevel=immlevel.seq,effect=dy.dx,se=se.dy.dx)
tab1m3.margins$level<-"Municipality data"
names(tab1m3.margins)[1]<-"nwimm"

#calculate marginal effects for zip code data
tab2m3.coefs<-coef(tab2m3)
tab2m3.cov<-vcov(tab2m3)
nwimm.seq<-seq(from=0,to=.5,by=.01)
dy.dx<-tab2m3.coefs["immiconcern"]+tab2m3.coefs["nwimm * immiconcern"]*nwimm.seq
se.dy.dx<-sqrt(tab2m3.cov["immiconcern","immiconcern"]+nwimm.seq^2*tab2m3.cov["nwimm * immiconcern","nwimm * immiconcern"]+nwimm.seq*2*tab2m3.cov["immiconcern","nwimm * immiconcern"])
tab2m3.margins<-data.frame(nwimm=nwimm.seq,effect=dy.dx,se=se.dy.dx)
tab2m3.margins$level<-"Zip code data"

#gather data
munizipmargins<-rbind(tab1m3.margins,tab2m3.margins)
munizipmargins$level<-as.factor(munizipmargins$level)

#education coefficients: these come from simply regressing crime attitude on education + same controls as above
educoefs<-data.frame(educoef=c(1.266,.214),level=c("Municipality data","Zip code data"))

#plot margins
t1<-1.96
t2<-1.64


ggplot(munizipmargins,aes(x=nwimm,y=effect)) +
  geom_hline(yintercept=0,linetype=2,color="dark gray") +
  geom_smooth(aes(ymin=effect-t2*se,ymax=effect+t2*se),stat="identity",alpha=.3,color="black") +
  geom_smooth(aes(ymin=effect-t1*se,ymax=effect+t1*se),stat="identity",alpha=.2,linetype=0) +
  #  geom_rug(data=alld,aes(x=immlevel,y=1.5),position="jitter",sides="b",alpha=.1) +
  facet_grid(level~.,scale="free_y") +
  geom_hline(data=educoefs,aes(yintercept=educoef),color="#888888",linetype=2) +
  geom_text(data=educoefs,aes(x=.25, y=educoef+educoef/4,label="Coefficient on education",group=NULL),size=4,color="#888888") +
  xlab("Ethnic diversity") +
  ylab("Coefficient on immigration attitude") +
  theme_bw()

ggsave(file="figures/crime_figure3.pdf",height=5,width=9)


############################
## FIGURE 4
############################

#predicted values for muni data
tab1m3int<-as.data.frame(effect("exat:immlevel",tab1m3phm,xlevels=list(female=0,age=46,edu=.56,leftvot.p=1,lrscale=6,exat=seq(0,1,by=.05),immlevel=c(0,.06,.16,.47),muninum=153,elec="FT01"),confidence.level=.95))
tab1m3int90<-as.data.frame(effect("exat:immlevel",tab1m3phm,xlevels=list(female=0,age=46,edu=.56,leftvot.p=1,lrscale=6,exat=seq(0,1,by=.05),immlevel=c(0,.06,.16,.47),muninum=153,elec="FT01"),confidence.level=.90))
tab1m3int$lower90<-tab1m3int90$lower
tab1m3int$upper90<-tab1m3int90$upper
tab1m3int$immlevel<-factor(tab1m3int$immlevel)
levels(tab1m3int$immlevel)<-c("Diversity: Minimum","Mean","Mean+2SD","Maximum")
tab1m3int$level<-"Municipality data"
tab1m3int<-tab1m3int[,c(2,1,3:9)]
names(tab1m3int)[1:2]<-c("nwimm","immiconcern")

#predicted values for zip data - need to refit model with lm() function
tab2m4lm<-lm(crimeconcern~nwimm*immiconcern+avgincome+avgeduyrs+pop+female+age+agesq+edu+fullzipfac+yearfac,data=zd)
tab2m4int<-as.data.frame(effect("nwimm:immiconcern",tab2m4lm,xlevels=list(nwimm=c(0,.04,.16,.6),immiconcern=seq(0,1,by=.05),avgincome=60000,avgeduyrs=12,pop=200,female=1,age=40,agesq=40^2,edu=.42,fullzipfac="5000",yearfac="02")))
tab2m4int<-as.data.frame(effect("nwimm*immiconcern",tab2m4lm,xlevels=list(nwimm=c(0,.04,.16,.6),immiconcern=seq(0,1,by=.05),avgincome=60000,avgeduyrs=12,pop=200,female=1,age=40,agesq=40^2,edu=.42,fullzipfac="5000",yearfac="02"),confidence.level=.90))
tab2m4int$lower90<-tab2m4int$lower
tab2m4int$upper90<-tab2m4int$upper
tab2m4int$nwimm<-factor(tab2m4int$nwimm)
levels(tab2m4int$nwimm)<-c("Diversity: Minimum","Mean","Mean+2SD","Maximum")
tab2m4int$level<-"Zip code data"

lohipreds<-rbind(tab1m3int,tab2m4int)
lohipreds$level<-factor(lohipreds$level)

ggplot(lohipreds,aes(x=immiconcern,y=fit)) +
  geom_smooth(aes(ymin=lower,ymax=upper),color="black",stat="identity",alpha=.2) +
  geom_smooth(aes(ymin=lower90,ymax=upper90),color="black",stat="identity",alpha=.3,linetype=0) +
  facet_grid(level~nwimm,scales = "free_y") +
  theme_bw() +
  xlab("Anti-immigration attitude") +
  ylab("Crime attitude") +
  scale_x_continuous(breaks=c(0,.25,.5,.75,1),labels=c("0",".25",".5",".75","1")) +
  scale_y_continuous(breaks=c(0,.25,.5,.75,1),labels=c("0",".25",".5",".75","1"))
ggsave("figures/crime_figure4.pdf",width=9,height=5)


############################
## FIGURE 5
############################

load("data/issp2009.rdata"); issp<-Data ; rm(Data)

#recoding
issp$localimmshare<-ifelse(issp$V66<101,issp$V66,NA)
issp$nationalimmshare<-ifelse(issp$V67<101,issp$V67,NA)
issp$zipcode<-issp$V150
issp$zipcoder<-issp$zipcode
issp$zipcoder[issp$zipcode<1500]<-1000
issp$zipcoder[issp$zipcode>=1500 & issp$zipcode<1800]<-1500
issp$nightafraid<-recode(issp$V62,"1=1;5=0;8=NA")

#pull in actual immshare data
dst<-read_dta(file="data/zipsnwishare.dta")
dst09<-subset(dst,year==2009)

#merge files
issp<-merge(issp,dst09,by.x="zipcoder",by.y="postnummer")

#check correlations
cor.test(issp$localimmshare,issp$immshare)
cor.test(subset(issp,immshare<.4)$localimmshare,subset(issp,immshare<.4)$immshare)
table(issp$immshare>.4)

#plot
ggplot(issp,aes(x=100*immshare,y=localimmshare)) +
  geom_point(position="jitter",alpha=.5) +
  geom_smooth(method="loess",color="black",linetype="dotted") +
  geom_smooth(data=subset(issp,immshare<.4),aes(x=100*immshare,y=localimmshare),method="loess",color="black",size=1) +
  geom_abline(intercept=0, slope=1,linetype="dashed") +
  xlim(c(0,55)) +
  ylim(c(0,55)) +
  ylab("Estimated share of immigrants in neighborhood") +
  xlab("Zip code share of non-western immigrants and descendants") +
  theme_bw()
ggsave(file="figures/crime_figure5.pdf",height=5,width=9)

############################
## FIGURE 6
############################

#data frame for storing interaction coefficients for main res and placebos
coefdat<-data.frame(intcoef=rep(NA,4*2*2),intse=NA,model=rep(1:4,4),type=rep(c(rep("Crime",4),rep("Environment (placebo)",4)),2),level=c(rep("Municipality data",8),rep("Zip code data",8)))

Sys.time()
# muni data interaction terms - careful, this takes a while, about 25 mins on my laptop
coefdat[1:4,1:2]<-munimodels %>% 
  lapply(.,function(x){
    miests<-getmiests(x)
    intcoef<-miests$coef[1,]["exat:immlevel"] 
    intse<-miests$se[1,]["exat:immlevel"] 
    intmat<-matrix(c(intcoef,intse),ncol=2,byrow=F)
    return(intmat)
  }) %>% 
  unlist() %>% 
  matrix(.,ncol=2,byrow=T)

#specify placebo models
tab1m1plac<-as.formula(envir01~female+age+edu+hinc+emp.student+emp.pension+muniedu+munipop+exat*immlevel+muninum)
tab1m2plac<-as.formula(envir01~female+age+edu+hinc+emp.student+emp.pension+muniedu+munipop+leftvot.p+lrscale+exat*immlevel+muninum)
tab1m3plac<-as.formula(envir01~female+age+edu+hinc+emp.student+emp.pension+muniedu+munipop+exat*immlevel+muninum+elec)
tab1m4plac<-as.formula(envir01~female+age+edu+hinc+emp.student+emp.pension+muniedu+munipop+leftvot.p+lrscale+exat*immlevel+muninum+elec)
muniplacmodels<-list(tab1m1plac,tab1m2plac,tab1m3plac,tab1m4plac)

#muni data placebo interaction terms - careful, this takes a while!
coefdat[5:8,1:2]<-muniplacmodels %>% 
  lapply(.,function(x){
    miests<-getmiests(x)
    intcoef<-miests$coef[1,]["exat:immlevel"] 
    intse<-miests$se[1,]["exat:immlevel"] 
    intmat<-matrix(c(intcoef,intse),ncol=2,byrow=F)
    return(intmat)
  }) %>% 
  unlist() %>% 
  matrix(.,ncol=2,byrow=T)
Sys.time()
coefdat

#zip data interaction terms
zipmodels<-list(tab2m1,tab2m2,tab2m3,tab2m4)
coefdat[9:12,1:2]<-zipmodels %>% 
  lapply(.,function(x){
    intcoef<-x$coefficients["nwimm * immiconcern"]
    intse<-sqrt(diag(vcov(x)))["nwimm * immiconcern"]
    return(c(intcoef,intse))
  }) %>% 
  unlist() %>% 
  matrix(.,ncol=2,byrow=T)

#specify zip code placebo models
tab2m1plac<-robcov(ols(polluconcern~nwimm*immiconcern+avgincome+avgeduyrs+pop+fullzipfac,
                   data=zd,x=T,y=T),cluster=zd$fullzipyear)
tab2m2plac<-robcov(ols(polluconcern~nwimm*immiconcern+avgincome+avgeduyrs+pop+female+age+agesq+edu+fullzipfac,
                   data=zd,x=T,y=T),cluster=zd$fullzipyear)
tab2m3plac<-robcov(ols(polluconcern~nwimm*immiconcern+avgincome+avgeduyrs+pop+fullzipfac+yearfac,
                   data=zd,x=T,y=T),cluster=zd$fullzipyear)
tab2m4plac<-robcov(ols(polluconcern~nwimm*immiconcern+avgincome+avgeduyrs+pop+female+age+agesq+edu+fullzipfac+yearfac,
                   data=zd,x=T,y=T),cluster=zd$fullzipyear)

#paste in placebo model interaction terms
zipplacmodels<-list(tab2m1plac,tab2m2plac,tab2m3plac,tab2m4plac)
coefdat[13:16,1:2]<-zipplacmodels %>% 
  lapply(.,function(x){
    intcoef<-x$coefficients["nwimm * immiconcern"]
    intse<-sqrt(diag(vcov(x)))["nwimm * immiconcern"]
    return(c(intcoef,intse))
  }) %>% 
  unlist() %>% 
  matrix(.,ncol=2,byrow=T)

ggplot(coefdat,aes(x=model,y=intcoef,group=type,color=type)) +
  geom_point(position=position_dodge(.2)) +
  geom_errorbar(aes(ymin=intcoef-t1*intse,ymax=intcoef+t1*intse),width=0,size=.6,position=position_dodge(.2)) +
  geom_errorbar(aes(ymin=intcoef-t2*intse,ymax=intcoef+t2*intse),width=0,size=1.0,position=position_dodge(.2)) +
  facet_grid(level~.,scales="free_y") +
  geom_hline(yintercept=0,linetype="dashed") +
  scale_color_manual(values=c("black","#888888"),guide=guide_legend(title=NULL)) +
  xlab("Model") +
  ylab("Interaction coefficient") +
  theme_bw() +
  theme(legend.key=element_blank(),legend.position="bottom")

ggsave("figures/crime_figure6.pdf",width=9,height=5)



