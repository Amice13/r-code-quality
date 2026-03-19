rm(list=ls())
library(plyr)
library('ggmap')
library("mapproj")
library('coefplot')
library('gmapsdistance')
library("googleway")
library('plotrix')
library('questionr')
library('lattice')
library('stargazer')
library('aods3')
library('lmtest')
library('matrixStats')
library('tidyr')
library('readr')
library("cowplot")

set.seed(8675309)


library(sandwich)
library(Matrix)

setwd("/Users/Tanu/Dropbox/Projects/Current/*/Drafts/JMP/JOP/Final Submission")


# Main document tables and figures ----------------------------------------


#Table 1#

load('ihds-data.rda') #ihds data


ihds=read_csv('ihds_participation.csv') #blank csv providing the question coding
benefit=cbind.data.frame(ihds$Benefit,ihds$`Response type`,ihds$Code,rep(NA,11))
colnames(benefit)[4]='percent'
options(digits=4)

for (i in 1:nrow(benefit)){
if (ihds$`Response type`[i]==1){
  benefit[i,4]= with(da36151.0002 , mean(I(eval(parse(text=ihds$Code[i]))!="(0) No one 0"),na.rm=TRUE)) %>% round(., digits=4)}
if (ihds$`Response type`[i]==2){
  benefit[i,4]=with(da36151.0002 , mean(I(eval(parse(text=ihds$Code[i]))!=0),na.rm=TRUE))%>% round(., digits=4)}

if (ihds$`Response type`[i]==3){
  benefit[i,4]= with(da36151.0002 ,mean(I(eval(parse(text=ihds$Code[i]))=="(1) Yes 1"),na.rm=TRUE))%>% round(., digits=4)
}
}
benefit=benefit[,c(1,4)] 
benefit=benefit[-11,]#drop ration cards
benefit[7,1]='Farmer credit scheme'
benefit[8,1]='Rural housing subsidy'
benefit[10,1]='Rural employment guarantee'
benefit[5,1]='Senior food security scheme'
colnames(benefit)=c('Benefit','Percent')

benefit







##Table 2 No analysis necessary. Data directly from author's own notes.##


##Table 3##

#load in main survey data
data=read_csv('jop_data.csv')
#create centered block dummies to be used in regressions 
center_apply <- function(x) {
  apply(x, 2, function(y) y - mean(y))
}
centeredB=model.matrix( ~ as.factor(data$block) - 1) %>% center_apply(.)

#create functions for balance tables
bal_=function(x){
  
  data$x=with(data,eval(parse(text=x)))
  c= data$x[data$t==0]
  lm=lm(x~t+t*centeredB,data=data)
  vcov=vcov(lm,type='HC2')
  coef=coeftest(lm, vcov)
  bal.=as.vector(c(coef[1,1],coef[2,1],coef[2,2],coef[2,4]))%>% round(.,digits=3)
  return(bal.)
}

mean.data_=function(x){
  covs=as.data.frame(x)
  mc=(apply(covs,1,FUN=bal_))%>%t()%>%as.data.frame()
  mc=cbind.data.frame(covs,mc)
  colnames(mc)=c('Variable','Control ','Treatment ','sd','Pr(>|t|)')
  return(mc)}



##prints table##
with(data,mean.data_(c('OBC','SCST','maratha','Muslim','kutchafloor', 'kutcharoof', 'always'
                      ,'same')))%>%stargazer(.,summary=FALSE,rownames = FALSE,type="text")




##Figure 1##

#The following three functions work together to create plots for each family of outcomes#

TE=function(Y){ #main regession specification
  lm=lm(Y~t+OBC+SCST+maratha+Muslim+kutchafloor+kutcharoof+always+same+t*centeredB, data=data) 
  vcov=vcov(lm, type="HC2")
  return(coeftest(lm, vcov)[2,])
}




mf=function(df,name){ #creates an object from the regression that can be used to make a graph
  data.frame(Variable = name %>% as.character(),
             Coefficient = df[1],
             SE = df[2],
             p=df[4]) %>%return()
}

graph=function(dataframe, xlab, ylab=FALSE, order=TRUE, p.adj=TRUE,py=0.30,ylim=.40 ){ #creates the graphs with multiple hypothesis adjusting
  allModelFrame=as.data.frame((dataframe))
  
  if (order==TRUE){allModelFrame=allModelFrame[order(allModelFrame$Coefficient),] 
  allModelFrame$Variable <- factor(allModelFrame$Variable, levels =allModelFrame$Variable[order(allModelFrame$Coefficient)])}
  if (p.adj==TRUE){allModelFrame$adj.p=p.adjust(allModelFrame$p,method="BH") #benjamini hochberg adjustments
  allModelFrame$adj.p=as.character(round(allModelFrame$adj.p, digits=3))
  allModelFrame$p=as.character(round(allModelFrame$p, digits=3))
  allModelFrame$p[allModelFrame$p=='0']='<.001'
  allModelFrame$adj.p[allModelFrame$adj.p=='0']='<.001'
  
  allModelFrame$ptext=allModelFrame$adj.p}
  
  if (p.adj==FALSE){
    allModelFrame$ptext="" #in case no adjusted p values are descired
  }
  # Specify the width of your confidence intervals
  interval1 <- -qnorm((1-0.9)/2)  # 90% multiplier
  interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier
  
  
  # Plot
  zp1 <- ggplot(allModelFrame)
  zp1 <- zp1 + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
  zp1 <- zp1 + geom_linerange(aes(x = Variable, ymin = Coefficient - SE*interval1,
                                  ymax = Coefficient + SE*interval1),
                              lwd = 1, position = position_dodge(width = 1/2))
  zp1 <- zp1 + geom_pointrange(aes(x = Variable, y = Coefficient, ymin = Coefficient - SE*interval2,
                                   ymax = Coefficient + SE*interval2),
                               lwd = 1/2, position = position_dodge(width = 1/2),
                               shape = 20)
  zp1 <- zp1 + coord_flip() + theme_bw()+geom_text(aes(x=Variable,y=py,label=as.character(ptext)),size=2.2)+
    expand_limits(y=ylim)
  zp1 <- zp1 + labs(y='Coefficient', x=xlab)+ theme(axis.text=element_text(size=8))
    if (ylab==FALSE){
      zp1=zp1+theme(axis.title.x = element_blank())}
  return(zp1)
}




#The following code creates a plot for each of the families of outcomes. 
#It then creates one larger figure in the paper at the end. 

#Claim-making
#collect individual level treatment effects
m1=TE(I(data$comp_ind=='Often'|data$comp_ind=='Sometimes')) %>% mf(.,"Make claims individually")
m2=TE(I(data$comp_group=='Often'|data$comp_group=='Sometimes')) %>% mf(.,"Make claims in a group")
m3=TE(I(data$meeting)) %>% mf(.,"Attending local area meeting")
#combine effects in plot for family 1
one=graph(rbind(m3,m2, m1),py=.6, ylim=c(-.1,.8), xlab='Claims', order=FALSE)


#Knowledge about local government

#collect individual level treatment effects
m1=TE(data$ew.party.correct) %>% mf(.,"Correct party for corporator")
m2=TE(data$ew.rep.correct) %>% mf(.,"Correct name for corporator")
m3=TE(data$ward.rep.match) %>% mf(.,"Correct name for a corporator in admin. ward")
##combine effects in plot for family 2
two=graph(rbind(m3,m2,m1), order=FALSE, xlab='Knowledge',py=.6, ylim=c(-.1,.8))

#Policy preferences
m1=TE(data$sw) %>% mf(.,"Social Welfare")
m2=TE(data$streets) %>% mf(.,"Street maintenence")
m3=TE(data$wes) %>%mf(.,"Water, electricity, sewage")
m4=TE(data$lao) %>% mf(.,"Law and order")
m5=TE(data$rsv) %>%mf(.,"Regulating street vendors")
m6=TE(data$hlu) %>% mf(.,"Housing and land use")
m7=TE(data$edu) %>% mf(.,"Education")
m8=TE(data$tran) %>% mf(.,"Transportation")
#Plot for family 3
three=graph(rbind(m1,m2,m3,m4,m5,m6,m7,m8),xlab="Policy prefs.",ylab=FALSE,py=.6, ylim=c(-.1,.8))



#Attitudes

m1=TE(I(data$finance=='Happy'))%>% mf(.,"Happy w/ financial situation") 

m2=TE(I(data$children=='Yes'))%>% mf(.,'Children will have better lives than them') 

m3=TE(I(data$leave_bom=='Would never leave'))%>% mf(.,'Would never consider leaving Mumbai')

m4=TE(I(data$leaders_status=='No')) %>% mf(.,"Status relative to local leaders")

#Plot for family 4
four=graph(rbind(m4,m3, m2,m1), order=FALSE,  xlab='Attitudes',  py=.6, ylim=c(-.1,.8))


#Voting (reasons)

m1=TE(data$vparty) %>% mf(.,"Party")
m2=TE(data$veth) %>% mf(.,"Ethnicity or religion")
m3=TE(data$vneigh) %>% mf(.,"Solving problems in the neighborhood")
m4=TE(data$vfinan) %>% mf(.,"Financial considerations")
m5=TE(data$vpref) %>% mf(.,"Representing city-wide policy preferences")
m6=TE(data$vbetter) %>% mf(.,"Long-term betterment of the Mumbai")

#Plot for family 5
five=graph(rbind(m6,m5,m4,m3,m2,m1), order=TRUE, xlab='Voting (reasons)',py=.6, ylim=c(-.1,.8))


##satisfaction with services

m1=TE(I(data$elec=='Satisfied')) %>% mf(.,"Electricity")
m2=TE(I(data$garb=='Satisfied')) %>% mf(.,"Garbage")
m3=TE(I(data$san=='Satisfied')) %>%mf(.,"Sanitation")
m4=TE(I(data$water=='Satisfied')) %>% mf(.,"Water")
m5=TE(I(data$law=='Satisfied')) %>%mf(.,"Law and order")
m6=TE(I(data$road=='Satisfied')) %>% mf(.,"Roads")
#Plot for family 6
six=graph(rbind(m1,m2,m3,m4,m5,m6),xlab='Satisfaction',ylab=TRUE,py=.6, ylim=c(-.1,.8))



##prints figure##
plot_grid(one, two,five, three, four, six, ncol=1, align="v")


##Table 4##

#Row 1#
subset(data,t==1 & move==0 ) %>%with(., mean(I(comp_ind=='Sometimes'|comp_ind=='Often'))) %>% round(.,digits=2)
subset(data,t==1 & move==1 ) %>%with(., mean(I(comp_ind=='Sometimes'|comp_ind=='Often')))%>% round(.,digits=2)
subset(data,t==0 ) %>%with(., mean(I(comp_ind=='Sometimes'|comp_ind=='Often')))%>% round(.,digits=2)

#Row 2#
subset(data,t==1 & move==0 ) %>%with(., mean(I(comp_group=='Sometimes'|comp_group=='Often')))%>% round(.,digits=2)
subset(data,t==1 & move==1 ) %>%with(., mean(I(comp_group=='Sometimes'|comp_group=='Often')))%>% round(.,digits=2)
subset(data,t==0 ) %>%with(., mean(I(comp_group=='Sometimes'|comp_group=='Often')))%>% round(.,digits=2)

#Row 3#
subset(data,t==1 & move==0 ) %>%with(., mean(meeting))%>% round(.,digits=2)
subset(data,t==1 & move==1 ) %>%with(.,  mean(meeting))%>% round(.,digits=2)
subset(data,t==0 ) %>%with(.,  mean(meeting))%>% round(.,digits=2)

#Row 4#
subset(data,t==1 & move==0 ) %>%with(., mean(ward.rep.match))%>% round(.,digits=2)
subset(data,t==1 & move==1 ) %>%with(.,  mean(ward.rep.match))%>% round(.,digits=2)
subset(data,t==0 ) %>%with(.,  mean(ward.rep.match))%>% round(.,digits=2)

#Row 5#
subset(data,t==1 & move==0 ) %>%with(., mean(wes))%>% round(.,digits=2)
subset(data,t==1 & move==1 ) %>%with(.,  mean(wes))%>% round(.,digits=2)
subset(data,t==0 ) %>%with(.,  mean(wes))%>% round(.,digits=2)

#Row 6#
subset(data,t==1 & move==0 ) %>%with(., mean(rsv))%>% round(.,digits=2)
subset(data,t==1 & move==1 ) %>%with(.,  mean(rsv))%>% round(.,digits=2)
subset(data,t==0 ) %>%with(.,  mean(rsv))%>% round(.,digits=2)




# Appendices --------------------------------------------------------------


#Read in data required#
contact=read_csv('contact_jop.csv')
location=read_csv('locations.csv')
full=read_csv('mhada_list.csv')


#Figure SI.1#
register_google('SET-API-KEY')
map = get_map(location = 'Mumbai', zoom=11,maptype = "toner-lite", source = "stamen")

ggmap(map) +
geom_point(data = contact, aes(x = Longitude, y = Latitude), color = "lightcoral", size = .5)+
geom_point(data = location, aes(x = Longitude, y = Latitude), color = "dodgerblue4", size = 2)+
theme(axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      plot.margin = unit(c(0, 0, -1, -1), 'lines')) +
 xlab('') +
  ylab('')

#Figure SI.2#

#Collect treatment effects
m1=TE(I(data$votebmc=='Yes')) %>% mf(.,"Voting in BMC elections")
m2= TE(I(data$votemh=='Yes')) %>% mf(.,"Voting in state elections")

#Graph effects

graph(rbind(m2,m1), order=FALSE, ylab=TRUE, xlab='')

#Table SI.1# No analysis required; table of code definitions.

#Table SI.2#
#final table prints out at the end
#category
prop=prop.table(table(contact$Category,contact$T),2)%>%as.data.frame.matrix()

prop$p=rep(NA,length(unique(contact$Category)))
for (i in 1:nrow(prop)){
  prop$p[i]=t.test(I((contact$Category)==rownames(prop)[i])~contact$T)$p.value
}

#Income
prop2=prop.table(table(contact$Income,contact$T),2)%>%as.data.frame.matrix()

prop2$p=rep(NA,length(unique(contact$Income)))
for (i in 1:nrow(prop2)){
  prop2$p[i]=t.test(I((contact$Income)==rownames(prop2)[i])~contact$T)$p.value
}

#Scheme
prop3=prop.table(table(contact$Scheme,contact$T),2)%>%as.data.frame.matrix()

prop3$p=rep(NA,length(unique(contact$Scheme)))
for (i in 1:nrow(prop3)){
  prop3$p[i]=t.test(I((contact$Scheme)==rownames(prop3)[i])~contact$T)$p.value
}





contact.balance=rbind(prop,prop2,prop3)
stargazer(contact.balance,summary=FALSE, type='text')

#Table SI.3#

a=prop.table(table(full$Category))%>%as.data.frame()
b=prop.table(table(contact$Category))%>%as.data.frame()
compare=cbind(a,b[,2])


compare$p=rep(NA)
for (i in 1:nrow(compare)){
  compare$p[i]=t.test(I(contact$Category==compare$Var1[i]),I(full$Category==compare$Var1[i]))$p.value%>%round(.,digits=3)
}
a=prop.table(table(full$Income))%>%as.data.frame()
b=prop.table(table(contact$Income))%>%as.data.frame()
compare2=cbind(a,b[,2])

compare2$p=rep(NA)
for (i in 1:nrow(compare2)){
  compare2$p[i]=t.test(I((contact$Income)==compare2$Var1[i]),I((full$Income)==compare2$Var1[i]))$p.value%>%round(.,digits=3)
}


a=prop.table(table(full$Scheme))%>%as.data.frame()
b=prop.table(table(contact$Scheme))%>%as.data.frame()
compare3=cbind(a,b[,2])

compare3$p=rep(NA)
for (i in 1:nrow(compare3)){
  compare3$p[i]=t.test(I((contact$Scheme)==compare3$Var1[i]),I((full$Scheme)==compare3$Var1[i]))$p.value%>%round(.,digits=3)
}



contact.balance=rbind(compare,compare2,compare3)
stargazer(contact.balance,summary=FALSE,rownames = FALSE, type='text')


#Table SI.4# No analysis required for columns 1 and 2, part of author's notes from survey firm
#p values for differences:

prop.test(x = c(413, 421), n = c(500, 500))$p.value
prop.test(x = c(9, 7), n = c(500, 500))$p.value
prop.test(x = c(1, 0), n = c(500, 500))$p.value
prop.test(x = c(5, 11), n = c(500, 500))$p.value
prop.test(x = c(1, 0), n = c(500, 500))$p.value
prop.test(x = c(14, 20), n = c(500, 500))$p.value
prop.test(x = c(19, 10), n = c(500, 500))$p.value
prop.test(x = c(37, 31), n = c(500, 500))$p.value

#Table SI.5#

fit=lm(t~OBC+SCST+maratha+Muslim+
         kutchafloor+kutcharoof+always+same+block,data=data, singular.ok = FALSE)
vcov=vcov(fit, type="HC2")
stargazer(fit, type='text',coef=list(coeftest(fit,vcov=vcov)[,1]), se=list(coeftest(fit,vcov=vcov)[,2]),
          omit=c("block","Constant"),  style="apsr",
          dep.var.labels = "Winning the housing lottery",
          omit.stat = "f",
          
          add.lines = list(c("Block dummies?", "Yes"), c("F Statistic (df = 91; 742)",'1.2046')))

#Table SI.6#
control=subset(data, t==0)
#Means
with(control, c(mean(tv),mean(computer),  mean(internet),mean(twowheeler),
                mean(car), mean(I(kutchafloor==0)), mean(I(kutcharoof==0)), mean(I(tap=="No")), mean(I(toilet=='No')), mean(employed),
                mean(education), mean(I(always==0)), mean(I(!is.na(rent))),mean(I(joint=='Yes'))))


#SDs
with(control, c(sd(tv),sd(computer),sd(internet),sd(twowheeler),
                sd(car), sd(I(kutchafloor==0)), sd(I(kutcharoof==0)), sd(I(tap=="No")), sd(I(toilet=='No')), sd(employed),
                sd(education), sd(I(always==0)), sd(I(!is.na(rent))),sd(I(joint=='Yes'))))


#Table SI.7. No analysis-- summary of variable definitions#

#Table SI.9#
 
#column 1 
with(data, t.test(I(comp_ind=='Sometimes'|comp_ind=='Often')~t))$estimate[1]
with(data, t.test(I(comp_group=='Sometimes'|comp_group=='Often')~t))$estimate[1]
with(data, t.test(meeting~t))$estimate[1]
with(data, t.test(ew.party.correct~t))$estimate[1]
with(data, t.test(ew.rep.correct~t))$estimate[1]
with(data, t.test(ward.rep.match~t))$estimate[1]

with(data, t.test(sw~t))$estimate[1]
with(data, t.test(streets~t))$estimate[1]
with(data, t.test(wes~t))$estimate[1]
with(data, t.test(lao~t))$estimate[1]
with(data, t.test(rsv~t))$estimate[1]
with(data, t.test(hlu~t))$estimate[1]
with(data, t.test(edu~t))$estimate[1]
with(data, t.test(tran~t))$estimate[1]


with(data, t.test(I(finance=='Happy')~t))$estimate[1]
with(data, t.test(I(children=='Yes')~t))$estimate[1]
with(data, t.test(I(leave_bom=='Would never leave')~t))$estimate[1]
with(data, t.test(I(leaders_status=='No')~t))$estimate[1]


with(data, t.test(vparty~t))$estimate[1]
with(data, t.test(veth~t))$estimate[1]
with(data, t.test(vneigh~t))$estimate[1]
with(data, t.test(vfinan~t))$estimate[1]
with(data, t.test(vpref~t))$estimate[1]
with(data, t.test(vbetter~t))$estimate[1]



with(data, t.test(I(elec=='Satisfied')~t))$estimate[1]
with(data, t.test(I(garb=='Satisfied')~t))$estimate[1]
with(data, t.test(I(san=='Satisfied')~t))$estimate[1]
with(data, t.test(I(water=='Satisfied')~t))$estimate[1]
with(data, t.test(I(law=='Satisfied')~t))$estimate[1]
with(data, t.test(I(road=='Satisfied')~t))$estimate[1]

#column 2
with(data, t.test(I(comp_ind=='Sometimes'|comp_ind=='Often')~t))$estimate[2]
with(data, t.test(I(comp_group=='Sometimes'|comp_group=='Often')~t))$estimate[2]
with(data, t.test(meeting~t))$estimate[2]
with(data, t.test(ew.party.correct~t))$estimate[2]
with(data, t.test(ew.rep.correct~t))$estimate[2]
with(data, t.test(ward.rep.match~t))$estimate[2]
with(data, t.test(sw~t))$estimate[2]
with(data, t.test(streets~t))$estimate[2]
with(data, t.test(wes~t))$estimate[2]
with(data, t.test(lao~t))$estimate[2]
with(data, t.test(rsv~t))$estimate[2]
with(data, t.test(hlu~t))$estimate[2]
with(data, t.test(edu~t))$estimate[2]
with(data, t.test(tran~t))$estimate[2]
with(data, t.test(I(finance=='Happy')~t))$estimate[2]
with(data, t.test(I(children=='Yes')~t))$estimate[2]
with(data, t.test(I(leave_bom=='Would never leave')~t))$estimate[2]
with(data, t.test(I(leaders_status=='No')~t))$estimate[2]
with(data, t.test(vparty~t))$estimate[2]
with(data, t.test(veth~t))$estimate[2]
with(data, t.test(vneigh~t))$estimate[2]
with(data, t.test(vfinan~t))$estimate[2]
with(data, t.test(vpref~t))$estimate[2]
with(data, t.test(vbetter~t))$estimate[2]
with(data, t.test(I(elec=='Satisfied')~t))$estimate[2]
with(data, t.test(I(garb=='Satisfied')~t))$estimate[2]
with(data, t.test(I(san=='Satisfied')~t))$estimate[2]
with(data, t.test(I(water=='Satisfied')~t))$estimate[2]
with(data, t.test(I(law=='Satisfied')~t))$estimate[2]
with(data, t.test(I(road=='Satisfied')~t))$estimate[2]


#column 3
with(data, t.test(I(comp_ind=='Sometimes'|comp_ind=='Often')~t))$p.value
with(data, t.test(I(comp_group=='Sometimes'|comp_group=='Often')~t))$p.value
with(data, t.test(meeting~t))$p.value
with(data, t.test(ew.party.correct~t))$p.value
with(data, t.test(ew.rep.correct~t))$p.value
with(data, t.test(ward.rep.match~t))$p.value
with(data, t.test(sw~t))$p.value
with(data, t.test(streets~t))$p.value
with(data, t.test(wes~t))$p.value
with(data, t.test(lao~t))$p.value
with(data, t.test(rsv~t))$p.value
with(data, t.test(hlu~t))$p.value
with(data, t.test(edu~t))$p.value
with(data, t.test(tran~t))$p.value
with(data, t.test(I(finance=='Happy')~t))$p.value
with(data, t.test(I(children=='Yes')~t))$p.value
with(data, t.test(I(leave_bom=='Would never leave')~t))$p.value
with(data, t.test(I(leaders_status=='No')~t))$p.value
with(data, t.test(vparty~t))$p.value
with(data, t.test(veth~t))$p.value
with(data, t.test(vneigh~t))$p.value
with(data, t.test(vfinan~t))$p.value
with(data, t.test(vpref~t))$p.value
with(data, t.test(vbetter~t))$p.value

with(data, t.test(I(elec=='Satisfied')~t))$p.value
with(data, t.test(I(garb=='Satisfied')~t))$p.value
with(data, t.test(I(san=='Satisfied')~t))$p.value
with(data, t.test(I(water=='Satisfied')~t))$p.value
with(data, t.test(I(law=='Satisfied')~t))$p.value
with(data, t.test(I(road=='Satisfied')~t))$p.value






#Table SI.9# For all of the following tables, output is generated after regressions are run

lm1=lm(I(comp_ind=='Often'|comp_ind=='Sometimes')~t+t*centeredB, data=data) 
vcov1=vcov(lm1, type="HC2")

lm1a=lm(I(comp_ind=='Often'|comp_ind=='Sometimes')~t+OBC+SCST+maratha+Muslim+kutchafloor+kutcharoof+always+same+t*centeredB, data=data) 
vcov1a=vcov(lm1a, type="HC2")


lm2=lm(I(comp_group=='Often'|comp_group=='Sometimes')~t+t*centeredB, data=data) 
vcov2=vcov(lm2, type="HC2")

lm2a=lm(I(comp_group=='Often'|comp_group=='Sometimes')~t+OBC+SCST+maratha+Muslim+kutchafloor+kutcharoof+always+same+t*centeredB, data=data) 
vcov2a=vcov(lm2a, type="HC2")


lm3=lm(meeting~t+t*centeredB, data=data) 
vcov3=vcov(lm3, type="HC2")

lm3a=lm(meeting~t+OBC+SCST+maratha+Muslim+kutchafloor+kutcharoof+always+same+t*centeredB, data=data) 
vcov3a=vcov(lm3a, type="HC2")




stargazer(lm1,lm1a,lm2,lm2a, lm3, lm3a,  se=list(sqrt(diag(vcov1)),sqrt(diag(vcov1a)),sqrt(diag(vcov2)),
                                                 sqrt(diag(vcov2a)),sqrt(diag(vcov3)),
                                                 sqrt(diag(vcov3a))),
          covariate.labels = c("T", "OBC", "SCST", "Maratha", "Muslim", "Kutcha floor", 
                               "Kutcha roof", "From Mumbai","From same ward as apt"),
          dep.var.labels   = c(
            "Individual complaint making", "Group complaint making", "Attending local area meetings") ,
          
          type='text',omit.stat = c("f",'ser'),no.space = TRUE, omit='block')




#Table SI.10#

lm1=lm(ew.party.correct~t+t*centeredB, data=data) 
vcov1=vcov(lm1, type="HC2")

lm1a=lm(ew.party.correct~t+OBC+SCST+maratha+Muslim+kutchafloor+kutcharoof+always+same+t*centeredB, data=data) 
vcov1a=vcov(lm1a, type="HC2")

lm2=lm(ew.rep.correct~t+t*centeredB, data=data) 
vcov2=vcov(lm2, type="HC2")

lm2a=lm(ew.rep.correct~t+OBC+SCST+maratha+Muslim+kutchafloor+kutcharoof+always+same+t*centeredB, data=data) 
vcov2a=vcov(lm2a, type="HC2")

lm3=lm(ward.rep.match~t+t*centeredB, data=data) 
vcov3=vcov(lm3, type="HC2")

lm3a=lm(ward.rep.match~t+OBC+SCST+maratha+Muslim+kutchafloor+kutcharoof+always+same+t*centeredB, data=data) 
vcov3a=vcov(lm3a, type="HC2")




stargazer(lm1,lm1a,lm2,lm2a,lm3, lm3a,  se=list(sqrt(diag(vcov1)),sqrt(diag(vcov1a)),sqrt(diag(vcov2)),
                                                sqrt(diag(vcov2a)),sqrt(diag(vcov3)),  sqrt(diag(vcov3a))),
          covariate.labels = c("T", "OBC", "SCST", "Maratha", "Muslim", "Kutcha floor", 
                               "Kutcha roof", "From Mumbai","From same ward as apt"),
          dep.var.labels   = c(
            "Party for corporator", "Name for corporator", "Name for a corporator in admin. ward") ,
          
          type='text',omit.stat = "f",no.space = TRUE, omit='block')





#Table SI.11 and SI.12#

lm1=lm(sw~t+t*centeredB, data=data) 
vcov1=vcov(lm1, type="HC2")

lm1a=lm(sw~t+OBC+SCST+maratha+Muslim+kutchafloor+kutcharoof+always+same+t*centeredB, data=data) 
vcov1a=vcov(lm1a, type="HC2")

lm2=lm(streets~t+t*centeredB, data=data) 
vcov2=vcov(lm2, type="HC2")

lm2a=lm(streets~t+OBC+SCST+maratha+Muslim+kutchafloor+kutcharoof+always+same+t*centeredB, data=data) 
vcov2a=vcov(lm2a, type="HC2")

lm3=lm(wes~t+t*centeredB, data=data) 
vcov3=vcov(lm3, type="HC2")

lm3a=lm(wes~t+OBC+SCST+maratha+Muslim+kutchafloor+kutcharoof+always+same+t*centeredB, data=data) 
vcov3a=vcov(lm3a, type="HC2")

lm4=lm(lao~t+t*centeredB, data=data) 
vcov4=vcov(lm4, type="HC2")

lm4a=lm(lao~t+OBC+SCST+maratha+Muslim+kutchafloor+kutcharoof+always+same+t*centeredB, data=data) 
vcov4a=vcov(lm4a, type="HC2")

lm5=lm(rsv~t+t*centeredB, data=data) 
vcov5=vcov(lm5, type="HC2")

lm5a=lm(rsv~t+OBC+SCST+maratha+Muslim+kutchafloor+kutcharoof+always+same+t*centeredB, data=data) 
vcov5a=vcov(lm5a, type="HC2")

lm6=lm(hlu~t+t*centeredB, data=data) 
vcov6=vcov(lm6, type="HC2")

lm6a=lm(hlu~t+OBC+SCST+maratha+Muslim+kutchafloor+kutcharoof+always+same+t*centeredB, data=data) 
vcov6a=vcov(lm6a, type="HC2")


lm7=lm(edu~t+t*centeredB, data=data) 
vcov7=vcov(lm7, type="HC2")

lm7a=lm(edu~t+OBC+SCST+maratha+Muslim+kutchafloor+kutcharoof+always+same+t*centeredB, data=data) 
vcov7a=vcov(lm7a, type="HC2")



lm8=lm(tran~t+t*centeredB, data=data) 
vcov8=vcov(lm8, type="HC2")

lm8a=lm(tran~t+OBC+SCST+maratha+Muslim+kutchafloor+kutcharoof+always+same+t*centeredB, data=data) 
vcov8a=vcov(lm8a, type="HC2")


#Table SI.11#

stargazer(lm1,lm2,lm3,  lm4,  lm5,  lm6, lm7, lm8,
          se=list(sqrt(diag(vcov1)),sqrt(diag(vcov2)),
                  sqrt(diag(vcov3)),  
                  sqrt(diag(vcov4)),
                  sqrt(diag(vcov5)),
                  sqrt(diag(vcov6)),sqrt(diag(vcov7)),sqrt(diag(vcov8))
          ),
          covariate.labels = c("T"),
          dep.var.labels   = c(
            "Welfare", 
            "Streets", 
            "Water/Elec/San","Law", "Vendors",
            "Housing", "Education","Transportation") ,
          
          type='text',omit.stat = "f",no.space = TRUE, omit='block')

#Table SI.12#

stargazer(lm1a,lm2a, lm3a,  lm4a,  lm5a,  lm6a,  lm7a, lm8a,
          se=list(sqrt(diag(vcov1a)),
                  sqrt(diag(vcov2a)),  sqrt(diag(vcov3a)),
                  sqrt(diag(vcov4a)),
                  sqrt(diag(vcov5a)),
                  sqrt(diag(vcov6a)),sqrt(diag(vcov7a)),sqrt(diag(vcov8a))),
          covariate.labels = c("T", "OBC", "SCST", "Maratha", "Muslim", "Kutcha floor", 
                               "Kutcha roof", "From Mumbai","From same ward as apt"),
          dep.var.labels   = c(
            "Welfare", 
            "Streets", 
            "Water/Elec/San","Law", "Vendors",
            "Housing", "Education","Transportation"),
          
          type='text',omit.stat = "f",no.space = TRUE, omit='block')







#Table SI.13#

lm1=lm(I(finance=='Happy')~t+t*centeredB, data=data) 
vcov1=vcov(lm1, type="HC2")

lm1a=lm(I(finance=='Happy')~t+OBC+SCST+maratha+Muslim+kutchafloor+kutcharoof+always+same+t*centeredB, data=data) 
vcov1a=vcov(lm1a, type="HC2")

lm2=lm(I(children=='Yes')~t+t*centeredB, data=data) 
vcov2=vcov(lm2, type="HC2")

lm2a=lm(I(children=='Yes')~t+OBC+SCST+maratha+Muslim+kutchafloor+kutcharoof+always+same+t*centeredB, data=data) 
vcov2a=vcov(lm2a, type="HC2")

lm3=lm(I(leave_bom=='Would never leave')~t+t*centeredB, data=data) 
vcov3=vcov(lm3, type="HC2")

lm3a=lm(I(leave_bom=='Would never leave')~t+OBC+SCST+maratha+Muslim+kutchafloor+kutcharoof+always+same+t*centeredB, data=data) 
vcov3a=vcov(lm3a, type="HC2")


lm4=lm(I(leaders_status=='No')~t+t*centeredB, data=data) 
vcov3=vcov(lm3, type="HC2")

lm4a=lm(I(leaders_status=='No')~t+OBC+SCST+maratha+Muslim+kutchafloor+kutcharoof+always+same+t*centeredB, data=data) 
vcov3a=vcov(lm3a, type="HC2")




stargazer(lm1,lm1a,lm2,lm2a,lm3, lm3a, lm4, lm4a,  se=list(sqrt(diag(vcov1)),sqrt(diag(vcov1a)),sqrt(diag(vcov2)),
                                                sqrt(diag(vcov2a)),sqrt(diag(vcov3)),  sqrt(diag(vcov3a)),sqrt(diag(vcov4)), sqrt(diag(vcov4a)) ),
          covariate.labels = c("T", "OBC", "SCST", "Maratha", "Muslim", "Kutcha floor", 
                               "Kutcha roof", "From Mumbai","From same ward as apt"),
          dep.var.labels   = c(
            "Happy w/ finances", "Children have better lives", "Never leave Mumbai", "Dont listen to leaders") ,
          
          type='text',omit.stat = "f",no.space = TRUE, omit='block')


#Table SI.14 and SI.15#

lm1=lm(vparty~t+t*centeredB, data=data) 
vcov1=vcov(lm1, type="HC2")

lm1a=lm(vparty~t+OBC+SCST+maratha+Muslim+kutchafloor+kutcharoof+always+same+t*centeredB, data=data) 
vcov1a=vcov(lm1a, type="HC2")

lm2=lm(veth~t+t*centeredB, data=data) 
vcov2=vcov(lm2, type="HC2")

lm2a=lm(veth~t+OBC+SCST+maratha+Muslim+kutchafloor+kutcharoof+always+same+t*centeredB, data=data) 
vcov2a=vcov(lm2a, type="HC2")

lm3=lm(vneigh~t+t*centeredB, data=data) 
vcov3=vcov(lm3, type="HC2")

lm3a=lm(vneigh~t+OBC+SCST+maratha+Muslim+kutchafloor+kutcharoof+always+same+t*centeredB, data=data) 
vcov3a=vcov(lm3a, type="HC2")

lm4=lm(vfinan~t+t*centeredB, data=data) 
vcov4=vcov(lm4, type="HC2")

lm4a=lm(vfinan~t+OBC+SCST+maratha+Muslim+kutchafloor+kutcharoof+always+same+t*centeredB, data=data) 
vcov4a=vcov(lm4a, type="HC2")

lm5=lm(vpref~t+t*centeredB, data=data) 
vcov5=vcov(lm5, type="HC2")

lm5a=lm(vpref~t+OBC+SCST+maratha+Muslim+kutchafloor+kutcharoof+always+same+t*centeredB, data=data) 
vcov5a=vcov(lm5a, type="HC2")

lm6=lm(vbetter~t+t*centeredB, data=data) 
vcov6=vcov(lm6, type="HC2")

lm6a=lm(vbetter~t+OBC+SCST+maratha+Muslim+kutchafloor+kutcharoof+always+same+t*centeredB, data=data) 
vcov6a=vcov(lm6a, type="HC2")


#Table SI.14#

stargazer(lm1,lm2,lm3,  lm4,  lm5,  lm6,  
          se=list(sqrt(diag(vcov1)),sqrt(diag(vcov2)),
                  sqrt(diag(vcov3)),  
                  sqrt(diag(vcov4)),
                  sqrt(diag(vcov5)),
                  sqrt(diag(vcov6))),
          covariate.labels = c("T"),
          dep.var.labels   = c(
            "Party", 
            "Ethnicity/Religion", 
            "Neighborhood problems","Financial problems", "Policy prefs",
             "Improving Mumbai") ,
          
          type='text',omit.stat = "f",no.space = TRUE, omit='block')

#Table SI.15#
stargazer(lm1a,lm2a, lm3a,  lm4a,  lm5a,  lm6a,  
          se=list(sqrt(diag(vcov1a)),
                  sqrt(diag(vcov2a)),  sqrt(diag(vcov3a)),
                  sqrt(diag(vcov4a)),
                  sqrt(diag(vcov5a)),
                  sqrt(diag(vcov6a))),
          covariate.labels = c("T", "OBC", "SCST", "Maratha", "Muslim", "Kutcha floor", 
                               "Kutcha roof", "From Mumbai","From same ward as apt"),
          dep.var.labels   = c(
            "Party", 
            "Ethnicity/Religion", 
            "Neighborhood problems","Financial problems", "Policy prefs",
            "Improving Mumbai") ,
          
          type='text',omit.stat = "f",no.space = TRUE, omit='block')



#Table SI.16 and Table SI.17#
lm1=lm(I(elec=='Satisfied')~t+t*centeredB, data=data) 
vcov1=vcov(lm1, type="HC2")

lm1a=lm(I(elec=='Satisfied')~t+OBC+SCST+maratha+Muslim+kutchafloor+kutcharoof+always+same+t*centeredB, data=data) 
vcov1a=vcov(lm1a, type="HC2")

lm2=lm(I(garb=='Satisfied')~t+t*centeredB, data=data) 
vcov2=vcov(lm2, type="HC2")

lm2a=lm(I(garb=='Satisfied')~t+OBC+SCST+maratha+Muslim+kutchafloor+kutcharoof+always+same+t*centeredB, data=data) 
vcov2a=vcov(lm2a, type="HC2")

lm3=lm(I(san=='Satisfied')~t+t*centeredB, data=data) 
vcov3=vcov(lm3, type="HC2")

lm3a=lm(I(san=='Satisfied')~t+OBC+SCST+maratha+Muslim+kutchafloor+kutcharoof+always+same+t*centeredB, data=data) 
vcov3a=vcov(lm3a, type="HC2")

lm4=lm(I(water=='Satisfied')~t+t*centeredB, data=data) 
vcov4=vcov(lm4, type="HC2")

lm4a=lm(I(water=='Satisfied')~t+OBC+SCST+maratha+Muslim+kutchafloor+kutcharoof+always+same+t*centeredB, data=data) 
vcov4a=vcov(lm4a, type="HC2")

lm5=lm(I(law=='Satisfied')~t+t*centeredB, data=data) 
vcov5=vcov(lm5, type="HC2")

lm5a=lm(I(law=='Satisfied')~t+OBC+SCST+maratha+Muslim+kutchafloor+kutcharoof+always+same+t*centeredB, data=data) 
vcov5a=vcov(lm5a, type="HC2")

lm6=lm(I(road=='Satisfied')~t+t*centeredB, data=data) 
vcov6=vcov(lm6, type="HC2")

lm6a=lm(I(road=='Satisfied')~t+OBC+SCST+maratha+Muslim+kutchafloor+kutcharoof+always+same+t*centeredB, data=data) 
vcov6a=vcov(lm6a, type="HC2")


#Table SI.16#
stargazer(lm1,lm2,lm3,  lm4,  lm5,  lm6, 
          se=list(sqrt(diag(vcov1)),sqrt(diag(vcov2)),
                  sqrt(diag(vcov3)),  
                  sqrt(diag(vcov4)),
                  sqrt(diag(vcov5)),
                  sqrt(diag(vcov6))
          ),
          covariate.labels = c("T"),
          dep.var.labels   = c(
            "Electricity", 
            "Garbage", 
            "Sanitation","Water", "Law and Order",
            "Roads") ,
          
          type='text',omit.stat = "f",no.space = TRUE, omit='block')

#Table SI.17#

stargazer(lm1a,lm2a, lm3a,  lm4a,  lm5a,  lm6a,  
          se=list(sqrt(diag(vcov1a)),
                  sqrt(diag(vcov2a)),  sqrt(diag(vcov3a)),
                  sqrt(diag(vcov4a)),
                  sqrt(diag(vcov5a)),
                  sqrt(diag(vcov6a))),
          covariate.labels = c("T", "OBC", "SCST", "Maratha", "Muslim", "Kutcha floor", 
                               "Kutcha roof", "From Mumbai","From same ward as apt"),
          dep.var.labels   = c(
            "Electricity", 
            "Garbage", 
            "Sanitation","Water", "Law and Order",
            "Roads"),
          
          type='text',omit.stat = "f",no.space = TRUE, omit='block')


#Table SI.18#

lm1=lm(ew.party.correct~t*female+t*centeredB, data=data) 
vcov1=vcov(lm1, type="HC2")



lm2=lm(ew.rep.correct~t*female+t*centeredB, data=data) 
vcov2=vcov(lm2, type="HC2")



lm3=lm(ward.rep.match~t*female+t*centeredB, data=data) 
vcov3=vcov(lm3, type="HC2")



stargazer(lm1,lm2,lm3,  se=list(sqrt(diag(vcov1)),sqrt(diag(vcov2)),
                                sqrt(diag(vcov3))),
          dep.var.labels   = c(
            "Party for corporator", "Name for corporator", "Name for a corporator in admin. ward") ,
          
          type='text',omit.stat = "f",no.space = TRUE, omit='block')


#Table SI.19#

lm1=lm(sw~t*female+t*centeredB, data=data) 
vcov1=vcov(lm1, type="HC2")


lm2=lm(streets~t*female+t*centeredB, data=data) 
vcov2=vcov(lm2, type="HC2")


lm3=lm(wes~t*female+t*centeredB, data=data) 
vcov3=vcov(lm3, type="HC2")


lm4=lm(lao~t*female+t*centeredB, data=data) 
vcov4=vcov(lm4, type="HC2")


lm5=lm(rsv~t*female+t*centeredB, data=data) 
vcov5=vcov(lm5, type="HC2")


lm6=lm(hlu~t*female+t*centeredB, data=data) 
vcov6=vcov(lm6, type="HC2")


lm7=lm(edu~t*female+t*centeredB, data=data) 
vcov7=vcov(lm7, type="HC2")

lm8=lm(tran~t*female+t*centeredB, data=data) 
vcov8=vcov(lm8, type="HC2")


stargazer(lm1,lm2,lm3,  lm4,  lm5,  lm6, lm7, lm8,
          se=list(sqrt(diag(vcov1)),sqrt(diag(vcov2)),
                  sqrt(diag(vcov3)),  
                  sqrt(diag(vcov4)),
                  sqrt(diag(vcov5)),
                  sqrt(diag(vcov6)),sqrt(diag(vcov7)),sqrt(diag(vcov8))
          ),
          
          dep.var.labels   = c(
            "Welfare", 
            "Streets", 
            "Water/Elec/San","Law", "Vendors",
            "Housing", "Education","Transportation") ,
          
          type='text',omit.stat = "f",no.space = TRUE, omit='block')







