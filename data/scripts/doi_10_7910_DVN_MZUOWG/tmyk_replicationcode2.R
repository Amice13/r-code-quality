library(foreign)
library(reshape2)
library(plyr)
library(ggplot2)
library(plm)
library(MASS)
library(coefplot)
library(car)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(broom)

#### Get data ####
setwd(".../TMYK Replication")
Data <- read.csv("TMYK3_pooled.csv")



################ CLEANING ##################

# Respondent Education
# label variable education "1=less HS 2=HS 3=some coll 4=AA 5=BA 6=MA 7=PhD 8=Prof"
Data$edu_coarse <- ifelse(Data$edu<=2, 1,
                          ifelse((Data$edu==3|Data$edu==4), 2,
                                 ifelse(Data$edu==5, 3,
                                        ifelse(Data$edu>5, 4, NA))))

# Office
Data$office<-ifelse(Data$o_type==1, "Mayor",
                    ifelse(Data$o_type==2, "Governor",
                           ifelse(Data$o_type==3, "President",
                                  ifelse(Data$o_type==4, "Clerk",
                                         ifelse(Data$o_type==5, "Comptroller",
                                                ifelse(Data$o_type==6, "Judge",NA))))))
Data$office<-factor(Data$office)
Data$office<-factor(Data$office, levels(Data$office)[c(2,1,4,5,3,6)])


# Retrospective request
Data$retro <-ifelse(Data$subcat==3, 1, 0)

# General categories - 1 = party, 2 = policy, 3 = personal, left 7 as other
Data$gencats<-Data$simple

# Policy info (non-generic policy requests; ==policy3 in .do file)
Data$polinfo<-ifelse(Data$subcat==2, 1, 0)
Data$polinfo[Data$info==240 | Data$info==340]<-0

# Rename soph to polknowl, standardize variable 
names(Data)[names(Data) == 'soph']<-"polknowl"
Data$polknowl<-(Data$polknowl)/(5)

# Profession and education (==profedu in .do file)
Data$profedu<-ifelse(Data$info==410 | Data$info==430 | Data$info==480 , 1, 0)

# Party and policy together
Data$political<-ifelse(Data$subcat==1 | Data$subcat==2, 1, 0)

# Personal info
Data$personal<-ifelse(Data$subcat==3, 1, 0)

# Mayor conditions
Data$nonames<-ifelse(Data$mayorcondition<4,1,0)
Data$mayorcondition<-as.factor(Data$mayorcondition)
levels(Data$mayorcondition) # 1==open, 2==incumbent, 3==challenger, 4==bf, 5==bm, 6==wf, 7==wm
Data$mayor<-NA
Data$mayor[Data$mayorcondition==1]<- "Open"
Data$mayor[Data$mayorcondition==2]<- "Incumbent"
Data$mayor[Data$mayorcondition==3]<- "Challenger"
Data$mayor[Data$mayorcondition==4]<- "Jada"
Data$mayor[Data$mayorcondition==5]<- "Jamal"
Data$mayor[Data$mayorcondition==6]<- "Jenna"
Data$mayor[Data$mayorcondition==7]<- "Jake"

# Office type conditions
Data$office<-NA
Data$office[Data$o_type==1]<- "Mayor"
Data$office[Data$o_type==2]<- "Governor"
Data$office[Data$o_type==3]<- "President"
Data$office[Data$o_type==4]<- "Clerk"
Data$office[Data$o_type==5]<- "Comptroller"
Data$office[Data$o_type==6]<- "Judge"
Data$office<-as.character(Data$office)
Data$office<-factor(Data$office, levels=c("Clerk", "Comptroller", "Judge", "Mayor", "Governor", "President"))

# Rank (reverse order so that higher number means higher rank)
Data$rank[Data$rank==3]<-0
Data$rank[Data$rank==1]<-3
Data$rank[Data$rank==0]<-1

# First-place rank
Data$firstplace<-ifelse(Data$rank==3,1,0)

# Requested social issue info (i.e., possibly identity-related)
Data$social<-ifelse(substring(Data$info,1,2)==21,1,0)
Data$social[substring(Data$info,1,2)==31]<-1

# Requested economic issue info 
Data$economic<-ifelse(substring(Data$info,1,2)==22,1,0)
Data$economic[substring(Data$info,1,2)==32]<-1

# Requested other issue info
Data$other<-ifelse(substring(Data$info,1,2)==23,1,0)
Data$other[substring(Data$info,1,2)==33]<-1

# Requested all but generic info  
Data$spec<-ifelse(((substring(Data$info,1,2)==21)|(substring(Data$info,1,2)==22)|(substring(Data$info,1,2)==23)),1,0)
Data$spec[substring(Data$info,1,2)==31]<-1
Data$spec[substring(Data$info,1,2)==32]<-1
Data$spec[substring(Data$info,1,2)==33]<-1

# Requested generic info  
Data$gen<-ifelse(substring(Data$info,1,2)==24,1,0)
Data$gen[substring(Data$info,1,2)==34]<-1

# testing  
Data$check<-ifelse(((substring(Data$info,1,1)==1)|(substring(Data$info,1,1)==2)|(substring(Data$info,1,1)==3)|(substring(Data$info,1,1)==4)|(substring(Data$info,1,1)==5)|(substring(Data$info,1,1)==6)|(substring(Data$info,1,1)==7)),1,0)




################ FIGURE 1 ##################

Data$request2<-NA
Data$request2[Data$subcat==1]<- "Political"
Data$request2[(Data$subcat==2|Data$subcat==3)&Data$info!=240&Data$info!=340]<- "Specific\nPolicy"
Data$request2[Data$info==240|Data$info==340]<- "General\nPolicy"
Data$request2[Data$subcat==4&Data$info!=410&Data$info!=430&Data$info!=480]<- "Mutable"
Data$request2[Data$info==410|Data$info==430|Data$info==480]<- "Experience"
Data$request2[Data$subcat==5]<- "Immutable"
Data$request2[Data$subcat==6|Data$subcat==7]<- "Other"
derp<-subset(Data,request2=="Political"|request2=="Specific\nPolicy"|request2=="General\nPolicy"|request2=="Mutable"|request2=="Immutable"|request2=="Other"|request2=="Experience")
derp$request2<-as.character(derp$request2)
derp$request2<-factor(derp$request2, levels=c("Political", "Specific\nPolicy", "General\nPolicy","Experience", "Mutable", "Immutable", "Other"))
derp<-derp[!is.na(derp$o_type),]

offgg1 <-derp[c("office", "request2")] %>%
  group_by(office) %>%
  summarise_all(funs(tot_n=sum(!is.na(.))))

offgg2 <-derp[c("office", "request2", "check")] %>%
  group_by(office, request2) %>%
  summarise_all(funs(n=sum(!is.na(.))))

offg<-merge(offgg1, offgg2)
offg$value<-(offg$n/offg$tot_n)

offgg<-offg[,c(1,3,5)] 

grays <- rep("gray20",6)

oplot<- ggplot(data=offg, aes(request2, value)) +
  geom_bar(aes(fill = office), position = "dodge", stat="identity") +
  xlab("") +
  ylab("Percentage of Requests") +
  facet_grid(office~.) +
  theme_minimal(base_size = 16) +
  theme(legend.position="none", 
        panel.grid.major = element_line(linetype='longdash', size=.4), 
        panel.grid.minor = element_line(linetype='longdash', size=.2)) +
  theme(strip.text.y = element_text(angle=0), 
        plot.title = element_text(hjust=.5),
        axis.title.y=element_text(vjust=3)) +
  ggtitle("Types of Requests by Office") +
  scale_fill_manual(values=grays) +
  scale_y_continuous(breaks=c(.1, .2, .3, .4),
                     labels = scales::percent)

ggsave(file="fig1.jpg", oplot, width=11, height=8)



################ FIGURE 2 ##################

cknowl <- Data[c("polknowl", "request2")] 
cknowl$count <- 1

cknowl$request2 <- factor(cknowl$request2, levels=c("Political", "Specific\nPolicy", "General\nPolicy","Experience", "Mutable", "Immutable", "Other"))

ck <- cknowl %>%
  group_by(request2, polknowl) %>%
  summarise_all(funs(n=sum(!is.na(.))))

ck2 <- cknowl[c("polknowl", "count")] %>%
  group_by(polknowl) %>%
  summarise_all(funs(totn=sum(!is.na(.))))

ck3 <- merge(ck, ck2, all.x=T)
ck3 <- ck3[!is.na(ck3$polknowl),]

ck3$value <- (ck3$n/ck3$totn)

cknowl <- cknowl[!is.na(cknowl$polknowl),]

ckplot <- ggplot(ck3, aes(x=polknowl, y=value)) +
  geom_bar(position = "dodge", stat="identity") +
  # geom_smooth(method="lm", colour="gray70") +
  facet_wrap(~request2, nrow=2, ncol=4, scales="fixed") + 
  ggtitle("Request Content by Respondent Political Knowledge") +
  theme(plot.title= element_text(face="bold")) +
  xlab("Political Knowledge") +
  ylab("Percentage of Requests") +
  theme_minimal(base_size = 16) +
  theme(panel.grid.major = element_line(linetype='longdash', size=.4),
        panel.grid.minor = element_line(linetype='longdash', size=.2)) +
  theme(plot.title = element_text(hjust=.5),
        legend.key.width = unit(1,"cm")) +
  scale_x_continuous(breaks=c(0, .2, .4, .6, .8, 1)) + 
  scale_y_continuous(labels = scales::percent) 
# annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
# annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)

ggsave(file="fig2.jpg", ckplot, width=11, height=7)



################ FIGURE 3 ##################

Data$verifiable <- ifelse(((substring(Data$info,1,2)==12&Data$office=="Governor")|
                             (substring(Data$info,1,2)==13)|
                             (substring(Data$info,1,1)==2&(substring(Data$info,1,3)<240))|
                             (substring(Data$info,1,1)==3&(substring(Data$info,1,3)<340))|
                             (substring(Data$info,1,2)<44&(substring(Data$info,1,2)>40))|
                             (substring(Data$info,1,2)==46)|
                             (substring(Data$info,1,2)==47)|
                             (substring(Data$info,1,2)==48)|
                             (substring(Data$info,1,1)==5)),1,0)

Data$vague <- ifelse((Data$info==240)|(Data$info==340)|(Data$info>599),1,0)

# Create relevance 
Data$relevant<-NA

# Comptroller
Data$relevant <- ifelse(Data$office=="Comptroller" & ((substring(Data$info,1,2)==22) | 
                                                        (substring(Data$info,1,2)==32) | Data$info==110 | Data$info==120 | Data$info==130 | Data$info==237 |  Data$info==238 | Data$info==337 | Data$info==338 | 
                                                        Data$info==410 |  Data$info==430 | Data$info==460 | Data$info==440 | Data$info==450 | 
                                                        Data$info==470 | Data$info==480 | Data$info==491 | Data$info==240 | Data$info==340), 1, 0)
# CLerk
Data$relevant <- ifelse(Data$office=="Clerk" & (Data$info==110 | Data$info==120 | Data$info==130 | Data$info==238 | Data$info==338 | 
                                                  Data$info==410 |  Data$info==430 | Data$info==460 | Data$info==440 | Data$info==450 | 
                                                  Data$info==470 | Data$info==480 | Data$info==491 | Data$info==240 | Data$info==340), 1, Data$relevant)
# Judge
Data$relevant <- ifelse(Data$office=="Judge" & ((substring(Data$info,1,2)==21) | Data$info==130 |
                                                  (substring(Data$info,1,2)==31) | Data$info==237 |  Data$info==238 | Data$info==337 | Data$info==338 | 
                                                  Data$info==110 | Data$info==120 | Data$info==130 | 
                                                  Data$info==410 |  Data$info==430 | Data$info==460 | Data$info==440 | Data$info==450 | 
                                                  Data$info==470 | Data$info==480 | Data$info==491 | Data$info==240 | Data$info==340), 1, Data$relevant)
# Mayor
Data$relevant <- ifelse(Data$office=="Mayor" & ((substring(Data$info,1,1)==2 & (Data$info!=226 & Data$info!=231 & Data$info!=232)) | 
                                                  (substring(Data$info,1,1)==3 & (Data$info!=326 & Data$info!=331 & Data$info!=332)) | 
                                                  Data$info==110 | Data$info==120 | Data$info==130 | 
                                                  Data$info==410 |  Data$info==430 | Data$info==460 | Data$info==440 | Data$info==450 | 
                                                  Data$info==470 | Data$info==480 | Data$info==491 | Data$info==240 | Data$info==340), 1, Data$relevant)
# Governor
Data$relevant <- ifelse(Data$office=="Governor" & ((substring(Data$info,1,1)==2  & (Data$info!=231)) | 
                                                     (substring(Data$info,1,1)==3 & (Data$info!=331)) | 
                                                     Data$info==110 | Data$info==120 | Data$info==130 | 
                                                     Data$info==410 |  Data$info==430 | Data$info==460 | Data$info==440 | Data$info==450 | 
                                                     Data$info==470 | Data$info==480 | Data$info==491 | Data$info==240 | Data$info==340), 1, Data$relevant)
# President
Data$relevant <- ifelse(Data$office=="President" & ((substring(Data$info,1,1)==2) | 
                                                      (substring(Data$info,1,1)==3) | 
                                                      Data$info==110 | Data$info==120 | Data$info==130 | 
                                                      Data$info==410 |  Data$info==430 | Data$info==460 | Data$info==440 | Data$info==450 | 
                                                      Data$info==470 | Data$info==480 | Data$info==491 | Data$info==240 | Data$info==340), 1, Data$relevant)

# Flipping the terms to be negative

Data$irrelevant <- ifelse(Data$relevant==1,0,1)
Data$unverifiable <- ifelse(Data$verifiable==1,0,1)

Data$badindex <- (Data$unverifiable + Data$vague + Data$irrelevant)/3
summary(lm(data=Data, badindex~polknowl))

Data$fakeid <- rep(1:nrow(Data))

dfgoodk <- Data[!is.na(Data$polknowl),c("dataset", "fakeid", "polknowl", "unverifiable", "vague", "irrelevant")] %>%
  gather(key=variable, value=value, unverifiable:irrelevant)

table(Data$dataset,Data$polknowl, useNA="ifany")

dfgoodk$variable <- factor(dfgoodk$variable, levels=c("irrelevant", "unverifiable", "vague"))

kplot<-ggplot(data=dfgoodk, aes(x=polknowl, y=value, linetype=variable)) +
  geom_smooth(method="lm", colour="black") +
  ggtitle("Requests by Respondent Political Knowledge") +
  theme(plot.title= element_text(face="bold")) +
  xlab("Political Knowledge") +
  ylab("Information Requests") +
  theme_minimal(base_size = 16) +
  theme(panel.grid.major = element_line(linetype='longdash', size=.4), 
        panel.grid.minor = element_line(linetype='longdash', size=.2)) +
  theme(plot.title = element_text(hjust=.5),
        legend.key.width = unit(1,"cm")) +
  scale_linetype_manual(name="Type of\nRequest", 
                        breaks=c("irrelevant", "unverifiable", "vague"),
                        values=c("dashed", "dotted", "solid"), 
                        labels=c("Irrelevant", "Unverifiable", "Vague"))

ggsave(file="fig3.pdf", kplot, width=11, height=7)



################ FIGURE 4 ##################

Data <- read.csv("TMYK3_pooled_ind.csv")

#### Cleaning

# Respondent Education
# label variable education "1=less HS 2=HS 3=some coll 4=AA 5=BA 6=MA 7=PhD 8=Prof"
Data$edu_coarse <- ifelse(Data$edu<=2, 1,
                          ifelse((Data$edu==3|Data$edu==4), 2,
                                 ifelse(Data$edu==5, 3,
                                        ifelse(Data$edu>5, 4, NA))))

# Office
Data$office<-ifelse(Data$o_type==1, "Mayor",
                    ifelse(Data$o_type==2, "Governor",
                           ifelse(Data$o_type==3, "President",
                                  ifelse(Data$o_type==4, "Clerk",
                                         ifelse(Data$o_type==5, "Comptroller",
                                                ifelse(Data$o_type==6, "Judge",NA))))))
Data$office<-factor(Data$office)
Data$office<-factor(Data$office, levels(Data$office)[c(2,1,4,5,3,6)])


# Policy info (non-generic policy requests; ==policy3 in .do file)
Data$polinfo1<-ifelse(Data$subcat1==2, 1, 0)
Data$polinfo1[Data$info1==240 | Data$info1==340]<-0
Data$polinfo2<-ifelse(Data$subcat2==2, 1, 0)
Data$polinfo2[Data$info2==240 | Data$info2==340]<-0
Data$polinfo3<-ifelse(Data$subcat3==2, 1, 0)
Data$polinfo3[Data$info3==240 | Data$info3==340]<-0

# Rename soph to polknowl, standardize variable 
names(Data)[names(Data) == 'soph']<-"polknowl"
Data$polknowl<-(Data$polknowl)/(5)

# Profession and education (==profedu in .do file)
Data$profedu1<-ifelse(Data$info1==410 | Data$info1==430 | Data$info1==480 , 1, 0)
Data$profedu2<-ifelse(Data$info2==410 | Data$info2==430 | Data$info2==480 , 1, 0)
Data$profedu3<-ifelse(Data$info3==410 | Data$info3==430 | Data$info3==480 , 1, 0)

# Party and policy together
Data$political1<-ifelse(Data$subcat1==1 | Data$subcat1==2, 1, 0)
Data$political2<-ifelse(Data$subcat2==1 | Data$subcat2==2, 1, 0)
Data$political3<-ifelse(Data$subcat3==1 | Data$subcat3==2, 1, 0)

# Personal info
Data$personal1<-ifelse(Data$subcat1==3, 1, 0)
Data$personal2<-ifelse(Data$subcat2==3, 1, 0)
Data$personal3<-ifelse(Data$subcat3==3, 1, 0)

# Dealbreakers
names(Data)[names(Data) == 'o_atttype']<-"disc_enc"
Data$disc_enc[Data$disc_enc==1]<- "Disappointing Response,\nFirst Request"
Data$disc_enc[Data$disc_enc==2]<- "Disappointing Response,\nThird Request"
Data$disc_enc[Data$disc_enc==3]<- "Encouraging Response,\nFirst Request"
Data$disc_enc[Data$disc_enc==4]<- "Encouraging Response,\nThird Request"

Data$disc_enc2[Data$att_badgood==1]<- "Good"
Data$disc_enc2[Data$att_badgood==0]<- "Bad"
Data$disc_enc3[Data$att_earlylater==1]<- "First-Ranked"
Data$disc_enc3[Data$att_earlylater==0]<- "Third-Ranked"

Data$good<-ifelse(Data$att_badgood==1,1,
                  ifelse(Data$att_badgood==0,0,NA))
Data$first<-ifelse(Data$att_earlylater==1,1,
                   ifelse(Data$att_earlylater==0,0,NA))

# Certainty (1 originally certain, 5 is no change); standardize variable
names(Data)[names(Data) == 'att']<-"certainty"
Data$certainty<-(Data$certainty-5)/(-4)

# Mayor conditions
Data$nonames<-ifelse(Data$mayorcondition<4,1,0)
Data$mayorcondition<-as.factor(Data$mayorcondition)
levels(Data$mayorcondition) # 1==open, 2==incumbent, 3==challenger, 4==bf, 5==bm, 6==wf, 7==wm
Data$mayor<-NA
Data$mayor[Data$mayorcondition==1]<- "Open"
Data$mayor[Data$mayorcondition==2]<- "Incumbent"
Data$mayor[Data$mayorcondition==3]<- "Challenger"
Data$mayor[Data$mayorcondition==4]<- "Jada"
Data$mayor[Data$mayorcondition==5]<- "Jamal"
Data$mayor[Data$mayorcondition==6]<- "Jenna"
Data$mayor[Data$mayorcondition==7]<- "Jake"

# Office type conditions
Data$office<-NA
Data$office[Data$o_type==1]<- "Mayor"
Data$office[Data$o_type==2]<- "Governor"
Data$office[Data$o_type==3]<- "President"
Data$office[Data$o_type==4]<- "Clerk"
Data$office[Data$o_type==5]<- "Comptroller"
Data$office[Data$o_type==6]<- "Judge"
Data$office<-as.character(Data$office)
Data$office<-factor(Data$office, levels=c("Clerk", "Comptroller", "Judge", "Mayor", "Governor", "President"))


# Requested social issue info (i.e., possibly identity-related)
Data$social1<-ifelse(substring(Data$info1,1,2)==21,1,0)
Data$social1[substring(Data$info1,1,2)==31]<-1
Data$social2<-ifelse(substring(Data$info2,1,2)==21,1,0)
Data$social2[substring(Data$info2,1,2)==31]<-1
Data$social3<-ifelse(substring(Data$info3,1,2)==21,1,0)
Data$social3[substring(Data$info3,1,2)==31]<-1

# Requested economic issue info 
Data$economic1<-ifelse(substring(Data$info1,1,2)==22,1,0)
Data$economic1[substring(Data$info1,1,2)==32]<-1
Data$economic2<-ifelse(substring(Data$info2,1,2)==22,1,0)
Data$economic2[substring(Data$info2,1,2)==32]<-1
Data$economic3<-ifelse(substring(Data$info3,1,2)==22,1,0)
Data$economic3[substring(Data$info3,1,2)==32]<-1

# Requested other issue info
Data$other1<-ifelse(substring(Data$info1,1,2)==23,1,0)
Data$other1[substring(Data$info1,1,2)==33]<-1
Data$other2<-ifelse(substring(Data$info2,1,2)==23,1,0)
Data$other2[substring(Data$info2,1,2)==33]<-1
Data$other3<-ifelse(substring(Data$info3,1,2)==23,1,0)
Data$other3[substring(Data$info3,1,2)==33]<-1

# Requested all but generic info  
Data$spec1<-ifelse(((substring(Data$info1,1,2)==21)|(substring(Data$info1,1,2)==22)|(substring(Data$info1,1,2)==23)),1,0)
Data$spec1[substring(Data$info1,1,2)==31]<-1
Data$spec1[substring(Data$info1,1,2)==32]<-1
Data$spec1[substring(Data$info1,1,2)==33]<-1
Data$spec2<-ifelse(((substring(Data$info2,1,2)==21)|(substring(Data$info2,1,2)==22)|(substring(Data$info2,1,2)==23)),1,0)
Data$spec2[substring(Data$info2,1,2)==31]<-1
Data$spec2[substring(Data$info2,1,2)==32]<-1
Data$spec2[substring(Data$info2,1,2)==33]<-1
Data$spec3<-ifelse(((substring(Data$info3,1,2)==21)|(substring(Data$info3,1,2)==22)|(substring(Data$info3,1,2)==23)),1,0)
Data$spec3[substring(Data$info3,1,2)==31]<-1
Data$spec3[substring(Data$info3,1,2)==32]<-1
Data$spec3[substring(Data$info3,1,2)==33]<-1

# Requested generic info  
Data$gen1<-ifelse(substring(Data$info1,1,2)==24,1,0)
Data$gen1[substring(Data$info1,1,2)==34]<-1
Data$gen2<-ifelse(substring(Data$info2,1,2)==24,1,0)
Data$gen2[substring(Data$info2,1,2)==34]<-1
Data$gen3<-ifelse(substring(Data$info3,1,2)==24,1,0)
Data$gen3[substring(Data$info3,1,2)==34]<-1

h4a <- lm(data=Data, certainty~disc_enc)
summary(h4a)

Data$pf<-ifelse(!is.na(Data$disc_enc)&Data$disc_enc=="Encouraging Response,\nFirst Request",1,0)
Data$pt<-ifelse(!is.na(Data$disc_enc)&Data$disc_enc=="Encouraging Response,\nThird Request",1,0)
Data$nf<-ifelse(!is.na(Data$disc_enc)&Data$disc_enc=="Discouraging Response,\nFirst Request",1,0)
Data$nt<-ifelse(!is.na(Data$disc_enc)&Data$disc_enc=="Discouraging Response,\nThird Request",1,0)


dealplot <- ddply(Data[!is.na(Data$disc_enc),], "disc_enc", summarise,
                  N    = length(certainty),
                  mean = mean(certainty),
                  sd   = sd(certainty),
                  se   = sd / sqrt(N)
)


dplot<-ggplot(dealplot, aes(x=disc_enc, y=mean, ymin=(mean-1.96*se), ymax=(mean+1.96*se))) + 
  geom_pointrange() + 
  theme_minimal(base_size = 16)  + 
  theme(panel.grid.major = element_line(linetype='longdash', size=.4), 
        panel.grid.minor = element_line(linetype='longdash', size=.2),
        axis.title.y = element_text(margin=margin(r=40)),
        axis.title.x = element_text(margin=margin(t=10)),
        aspect.ratio = 1,
        plot.margin=margin(t=.5, r=0, b=.5, l=0, unit="cm")) + 
  coord_flip() + 
  ylab("Vote Certainty") +
  xlab("Type of Information Provided") +
  ggtitle("Evidence for a 'Deal-Breaker' Heuristic")

ggsave(file="fig4.pdf", dplot, width=11, height=7)


##################################################
################ APPENDIX PLOTS ##################
##################################################


################ SI PLOT 2.1 ##################

Data <- read.csv("TMYK3_pooled.csv")

# Respondent Education
# label variable education "1=less HS 2=HS 3=some coll 4=AA 5=BA 6=MA 7=PhD 8=Prof"
Data$edu_coarse <- ifelse(Data$edu<=2, 1,
                          ifelse((Data$edu==3|Data$edu==4), 2,
                                 ifelse(Data$edu==5, 3,
                                        ifelse(Data$edu>5, 4, NA))))

# Office
Data$office<-ifelse(Data$o_type==1, "Mayor",
                    ifelse(Data$o_type==2, "Governor",
                           ifelse(Data$o_type==3, "President",
                                  ifelse(Data$o_type==4, "Clerk",
                                         ifelse(Data$o_type==5, "Comptroller",
                                                ifelse(Data$o_type==6, "Judge",NA))))))
Data$office<-factor(Data$office)
Data$office<-factor(Data$office, levels(Data$office)[c(2,1,4,5,3,6)])


# Retrospective request
Data$retro <-ifelse(Data$subcat==3, 1, 0)

# General categories - 1 = party, 2 = policy, 3 = personal, left 7 as other
Data$gencats<-Data$simple

# Policy info (non-generic policy requests; ==policy3 in .do file)
Data$polinfo<-ifelse(Data$subcat==2, 1, 0)
Data$polinfo[Data$info==240 | Data$info==340]<-0

# Rename soph to polknowl, standardize variable 
names(Data)[names(Data) == 'soph']<-"polknowl"
Data$polknowl<-(Data$polknowl)/(5)

# Profession and education (==profedu in .do file)
Data$profedu<-ifelse(Data$info==410 | Data$info==430 | Data$info==480 , 1, 0)

# Party and policy together
Data$political<-ifelse(Data$subcat==1 | Data$subcat==2, 1, 0)

# Personal info
Data$personal<-ifelse(Data$subcat==3, 1, 0)

# Mayor conditions
Data$nonames<-ifelse(Data$mayorcondition<4,1,0)
Data$mayorcondition<-as.factor(Data$mayorcondition)
levels(Data$mayorcondition) # 1==open, 2==incumbent, 3==challenger, 4==bf, 5==bm, 6==wf, 7==wm
Data$mayor<-NA
Data$mayor[Data$mayorcondition==1]<- "Open"
Data$mayor[Data$mayorcondition==2]<- "Incumbent"
Data$mayor[Data$mayorcondition==3]<- "Challenger"
Data$mayor[Data$mayorcondition==4]<- "Jada"
Data$mayor[Data$mayorcondition==5]<- "Jamal"
Data$mayor[Data$mayorcondition==6]<- "Jenna"
Data$mayor[Data$mayorcondition==7]<- "Jake"

# Office type conditions
Data$office<-NA
Data$office[Data$o_type==1]<- "Mayor"
Data$office[Data$o_type==2]<- "Governor"
Data$office[Data$o_type==3]<- "President"
Data$office[Data$o_type==4]<- "Clerk"
Data$office[Data$o_type==5]<- "Comptroller"
Data$office[Data$o_type==6]<- "Judge"
Data$office<-as.character(Data$office)
Data$office<-factor(Data$office, levels=c("Clerk", "Comptroller", "Judge", "Mayor", "Governor", "President"))

# Rank (reverse order so that higher number means higher rank)
Data$rank[Data$rank==3]<-0
Data$rank[Data$rank==1]<-3
Data$rank[Data$rank==0]<-1

# First-place rank
Data$firstplace<-ifelse(Data$rank==3,1,0)

# Requested social issue info (i.e., possibly identity-related)
Data$social<-ifelse(substring(Data$info,1,2)==21,1,0)
Data$social[substring(Data$info,1,2)==31]<-1

# Requested economic issue info 
Data$economic<-ifelse(substring(Data$info,1,2)==22,1,0)
Data$economic[substring(Data$info,1,2)==32]<-1

# Requested other issue info
Data$other<-ifelse(substring(Data$info,1,2)==23,1,0)
Data$other[substring(Data$info,1,2)==33]<-1

# Requested all but generic info  
Data$spec<-ifelse(((substring(Data$info,1,2)==21)|(substring(Data$info,1,2)==22)|(substring(Data$info,1,2)==23)),1,0)
Data$spec[substring(Data$info,1,2)==31]<-1
Data$spec[substring(Data$info,1,2)==32]<-1
Data$spec[substring(Data$info,1,2)==33]<-1

# Requested generic info  
Data$gen<-ifelse(substring(Data$info,1,2)==24,1,0)
Data$gen[substring(Data$info,1,2)==34]<-1

# testing  
Data$check<-ifelse(((substring(Data$info,1,1)==1)|(substring(Data$info,1,1)==2)|(substring(Data$info,1,1)==3)|(substring(Data$info,1,1)==4)|(substring(Data$info,1,1)==5)|(substring(Data$info,1,1)==6)|(substring(Data$info,1,1)==7)),1,0)


Data$request2<-NA
Data$request2[Data$subcat==1]<- "Political"
Data$request2[(Data$subcat==2|Data$subcat==3)&Data$info!=240&Data$info!=340]<- "Specific\nPolicy"
Data$request2[Data$info==240|Data$info==340]<- "General\nPolicy"
Data$request2[Data$subcat==4&Data$info!=410&Data$info!=430&Data$info!=480]<- "Mutable"
Data$request2[Data$info==410|Data$info==430|Data$info==480]<- "Experience"
Data$request2[Data$subcat==5]<- "Immutable"
Data$request2[Data$subcat==6|Data$subcat==7]<- "Other"
derp<-subset(Data,request2=="Political"|request2=="Specific\nPolicy"|request2=="General\nPolicy"|request2=="Mutable"|request2=="Immutable"|request2=="Other"|request2=="Experience")
derp$request2<-as.character(derp$request2)
derp$request2<-factor(derp$request2, levels=c("Political", "Specific\nPolicy", "General\nPolicy","Experience", "Mutable", "Immutable", "Other"))
derp<-derp[!is.na(derp$o_type),]

offgg1 <-derp[c("office", "request2")] %>%
  group_by(office) %>%
  summarise_all(funs(tot_n=sum(!is.na(.))))

offgg2 <-derp[c("office", "request2", "check")] %>%
  group_by(office, request2) %>%
  summarise_all(funs(n=sum(!is.na(.))))

offg<-merge(offgg1, offgg2)
offg$value<-(offg$n/offg$tot_n)

offgg<-offg[,c(1,3,5)] 

cknowl <- Data[c("polknowl", "request2")] 
cknowl$count <- 1

cknowl$request2 <- factor(cknowl$request2, levels=c("Political", "Specific\nPolicy", "General\nPolicy","Experience", "Mutable", "Immutable", "Other"))

ck <- cknowl %>%
  group_by(request2, polknowl) %>%
  summarise_all(funs(n=sum(!is.na(.))))

ck2 <- cknowl[c("polknowl", "count")] %>%
  group_by(polknowl) %>%
  summarise_all(funs(totn=sum(!is.na(.))))

ck3 <- merge(ck, ck2, all.x=T)
ck3 <- ck3[!is.na(ck3$polknowl),]

ck3$value <- (ck3$n/ck3$totn)

cknowl <- cknowl[!is.na(cknowl$polknowl),]



cknowlr <- Data[c("polknowl", "request2", "office")] 
cknowlr$count <- 1

cknowlr$request2 <- factor(cknowlr$request2, levels=c("Political", "Specific\nPolicy", "General\nPolicy","Experience", "Mutable", "Immutable", "Other"))

ckr <- cknowlr %>%
  group_by(request2, polknowl, office) %>%
  summarise_all(funs(n=sum(!is.na(.))))

ck2r <- cknowlr[c("polknowl", "count", "office")] %>%
  group_by(polknowl, office) %>%
  summarise_all(funs(totn=sum(!is.na(.))))

ck3r <- merge(ckr, ck2r, all.x=T)
ck3r <- ck3r[!is.na(ck3r$polknowl),]

ck3r$value <- (ck3r$n/ck3r$totn)

cknowlr <- cknowlr[!is.na(cknowlr$polknowl),]

ckplotrr <- ggplot(ck3r, aes(x=polknowl, y=value)) +
  geom_bar(position = "dodge", stat="identity") +
  facet_grid(request2~office, scales="fixed") + 
  ggtitle("Request Content by Respondent Political Knowledge and Office") +
  theme(plot.title= element_text(face="bold")) +
  xlab("Political Knowledge") +
  ylab("Percentage of Requests") +
  theme_minimal(base_size = 16) +
  theme(panel.grid.major = element_line(linetype='longdash', size=.4),
        panel.grid.minor = element_line(linetype='longdash', size=.2)) +
  theme(plot.title = element_text(hjust=.5),
        legend.key.width = unit(1,"cm")) +
  scale_x_continuous(breaks=c(0, .2, .4, .6, .8, 1)) + 
  scale_y_continuous(labels = scales::percent) 
# annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
# annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)

ggsave(file="si_2_1_1.jpg", ckplotrr, width=11, height=18)


## By just obscure/not:

Data$obscure <- ifelse(Data$office=="Comptroller" | Data$office=="Judge" | Data$office == "Clerk", "More Obscure:\nClerk, Comptroller, Judge", "Less Obscure:\nMayor, Governor, President")
cknowlr2 <- Data[c("polknowl", "request2", "obscure")] 
cknowlr2$count <- 1
cknowlr2$obscure <- as.factor(cknowlr2$obscure)
cknowlr2$request2 <- factor(cknowlr2$request2, levels=c("Political", "Specific\nPolicy", "General\nPolicy","Experience", "Mutable", "Immutable", "Other"))


ckr2 <- cknowlr2 %>%
  group_by(request2, polknowl, obscure) %>%
  summarise_all(funs(n=sum(!is.na(.))))

ck2r2 <- cknowlr2[c("polknowl", "count", "obscure")] %>%
  group_by(polknowl, obscure) %>%
  summarise_all(funs(totn=sum(!is.na(.))))

ck3r2 <- merge(ckr2, ck2r2, all.x=T)
ck3r2 <- ck3r2[!is.na(ck3r2$polknowl),]

ck3r2$value <- (ck3r2$n/ck3r2$totn)

cknowlr2 <- cknowlr2[!is.na(cknowlr2$polknowl),]

ckplotrr2 <- ggplot(ck3r2, aes(x=polknowl, y=value)) +
  geom_bar(position = "dodge", stat="identity") +
  facet_grid(request2~obscure, scales="fixed") + 
  ggtitle("Request Content by Respondent Political Knowledge and Office") +
  theme(plot.title= element_text(face="bold")) +
  xlab("Political Knowledge") +
  ylab("Percentage of Requests") +
  theme_minimal(base_size = 16) +
  theme(panel.grid.major = element_line(linetype='longdash', size=.4),
        panel.grid.minor = element_line(linetype='longdash', size=.2)) +
  theme(plot.title = element_text(hjust=.5),
        legend.key.width = unit(1,"cm")) +
  scale_x_continuous(breaks=c(0, .2, .4, .6, .8, 1)) + 
  scale_y_continuous(labels = scales::percent) 

ggsave(file="si_2_1_2.jpg", ckplotrr2, width=11, height=11)


################ SI PLOT 2.2 ##################

cknowle <- Data[c("edu_coarse", "request2")] 
cknowle$count <- 1

cknowle$request2 <- factor(cknowle$request2, levels=c("Political", "Specific\nPolicy", "General\nPolicy","Experience", "Mutable", "Immutable", "Other"))

cke <- cknowle %>%
  group_by(request2, edu_coarse) %>%
  summarise_all(funs(n=sum(!is.na(.))))

ck2e <- cknowle[c("edu_coarse", "count")] %>%
  group_by(edu_coarse) %>%
  summarise_all(funs(totn=sum(!is.na(.))))

ck3e <- merge(cke, ck2e, all.x=T)
ck3e <- ck3e[!is.na(ck3e$edu_coarse),]

ck3e$value <- (ck3e$n/ck3e$totn)

cknowle <- cknowle[!is.na(cknowle$edu_coarse),]

ckplote <- ggplot(ck3e, aes(x=edu_coarse, y=value)) +
  geom_bar(position = "dodge", stat="identity") +
  facet_wrap(~request2, nrow=2, ncol=4, scales="fixed") + 
  ggtitle("Request Content by Respondent Education") +
  theme(plot.title= element_text(face="bold")) +
  xlab("Respondent Education") +
  ylab("Percentage of Requests") +
  theme_minimal(base_size = 16) +
  theme(panel.grid.major = element_line(linetype='longdash', size=.4),
        panel.grid.minor = element_line(linetype='longdash', size=.2)) +
  theme(plot.title = element_text(hjust=.5),
        legend.key.width = unit(1,"cm")) +
  # scale_x_continuous(breaks=c(0, .2, .4, .6, .8, 1)) + 
  scale_y_continuous(labels = scales::percent) 

ggsave(file="si_2_2.jpg", ckplote, width=11, height=7)


################ SI PLOT 2.3 ##################

cknowlo <- Data[c("officeknowl", "request2")] 
cknowlo$count <- 1

cknowlo$request2 <- factor(cknowlo$request2, levels=c("Political", "Specific\nPolicy", "General\nPolicy","Experience", "Mutable", "Immutable", "Other"))

cko <- cknowlo %>%
  group_by(request2, officeknowl) %>%
  summarise_all(funs(n=sum(!is.na(.))))

ck2o <- cknowlo[c("officeknowl", "count")] %>%
  group_by(officeknowl) %>%
  summarise_all(funs(totn=sum(!is.na(.))))

ck3o <- merge(cko, ck2o, all.x=T)
ck3o <- ck3o[!is.na(ck3o$officeknowl),]

ck3o$value <- (ck3o$n/ck3o$totn)

cknowlo <- cknowlo[!is.na(cknowlo$officeknowl),]

ckploto <- ggplot(ck3o, aes(x=officeknowl, y=value)) +
  geom_bar(position = "dodge", stat="identity") +
  facet_wrap(~request2, nrow=2, ncol=4, scales="fixed") + 
  ggtitle("Request Content by Respondent Office-Specific Knowledge") +
  theme(plot.title= element_text(face="bold")) +
  xlab("Respondent Office-Specific Knowledge") +
  ylab("Percentage of Requests") +
  theme_minimal(base_size = 16) +
  theme(panel.grid.major = element_line(linetype='longdash', size=.4),
        panel.grid.minor = element_line(linetype='longdash', size=.2)) +
  theme(plot.title = element_text(hjust=.5),
        legend.key.width = unit(1,"cm")) +
  # scale_x_continuous(breaks=c(0, .2, .4, .6, .8, 1)) + 
  scale_y_continuous(labels = scales::percent) 

ggsave(file="si_2_3.jpg", ckploto, width=11, height=7)


################ SI PLOT 2.4 ##################
igsdata <- read.dta("tmyk2_full_forsean.dta")

extraplot <- ggplot(data=igsdata, aes(x=officeknowl, y=infoneed)) +
  geom_smooth(method="lm", colour="black") +
  ggtitle("Desire for More Information as a Function of Office-Specific Political Knowledge") +
  theme(plot.title= element_text(face="bold")) +
  xlab("Office-Specific Political Knowledge") +
  ylab("Desire for More Information") +
  theme_minimal(base_size = 16) +
  theme(panel.grid.major = element_line(linetype='longdash', size=.4), 
        panel.grid.minor = element_line(linetype='longdash', size=.2)) +
  theme(plot.title = element_text(hjust=.5),
        legend.key.width = unit(1,"cm"))

ggsave(file="si_2_4.jpg", extraplot, width=11, height=7)


################ SI PLOT 3.1 ##################

Data$verifiable <- ifelse(((substring(Data$info,1,2)==12&Data$office=="Governor")|
                             (substring(Data$info,1,2)==13)|
                             (substring(Data$info,1,1)==2&(substring(Data$info,1,3)<240))|
                             (substring(Data$info,1,1)==3&(substring(Data$info,1,3)<340))|
                             (substring(Data$info,1,2)<44&(substring(Data$info,1,2)>40))|
                             (substring(Data$info,1,2)==46)|
                             (substring(Data$info,1,2)==47)|
                             (substring(Data$info,1,2)==48)|
                             (substring(Data$info,1,1)==5)),1,0)

Data$vague <- ifelse((Data$info==240)|(Data$info==340)|(Data$info>599),1,0)

# Create relevance 
Data$relevant<-NA

# Comptroller
Data$relevant <- ifelse(Data$office=="Comptroller" & ((substring(Data$info,1,2)==22) | 
                                                        (substring(Data$info,1,2)==32) | Data$info==110 | Data$info==120 | Data$info==130 | Data$info==237 |  Data$info==238 | Data$info==337 | Data$info==338 | 
                                                        Data$info==410 |  Data$info==430 | Data$info==460 | Data$info==440 | Data$info==450 | 
                                                        Data$info==470 | Data$info==480 | Data$info==491 | Data$info==240 | Data$info==340), 1, 0)
# CLerk
Data$relevant <- ifelse(Data$office=="Clerk" & (Data$info==110 | Data$info==120 | Data$info==130 | Data$info==238 | Data$info==338 | 
                                                  Data$info==410 |  Data$info==430 | Data$info==460 | Data$info==440 | Data$info==450 | 
                                                  Data$info==470 | Data$info==480 | Data$info==491 | Data$info==240 | Data$info==340), 1, Data$relevant)
# Judge
Data$relevant <- ifelse(Data$office=="Judge" & ((substring(Data$info,1,2)==21) | Data$info==130 |
                                                  (substring(Data$info,1,2)==31) | Data$info==237 |  Data$info==238 | Data$info==337 | Data$info==338 | 
                                                  Data$info==110 | Data$info==120 | Data$info==130 | 
                                                  Data$info==410 |  Data$info==430 | Data$info==460 | Data$info==440 | Data$info==450 | 
                                                  Data$info==470 | Data$info==480 | Data$info==491 | Data$info==240 | Data$info==340), 1, Data$relevant)
# Mayor
Data$relevant <- ifelse(Data$office=="Mayor" & ((substring(Data$info,1,1)==2 & (Data$info!=226 & Data$info!=231 & Data$info!=232)) | 
                                                  (substring(Data$info,1,1)==3 & (Data$info!=326 & Data$info!=331 & Data$info!=332)) | 
                                                  Data$info==110 | Data$info==120 | Data$info==130 | 
                                                  Data$info==410 |  Data$info==430 | Data$info==460 | Data$info==440 | Data$info==450 | 
                                                  Data$info==470 | Data$info==480 | Data$info==491 | Data$info==240 | Data$info==340), 1, Data$relevant)
# Governor
Data$relevant <- ifelse(Data$office=="Governor" & ((substring(Data$info,1,1)==2  & (Data$info!=231)) | 
                                                     (substring(Data$info,1,1)==3 & (Data$info!=331)) | 
                                                     Data$info==110 | Data$info==120 | Data$info==130 | 
                                                     Data$info==410 |  Data$info==430 | Data$info==460 | Data$info==440 | Data$info==450 | 
                                                     Data$info==470 | Data$info==480 | Data$info==491 | Data$info==240 | Data$info==340), 1, Data$relevant)
# President
Data$relevant <- ifelse(Data$office=="President" & ((substring(Data$info,1,1)==2) | 
                                                      (substring(Data$info,1,1)==3) | 
                                                      Data$info==110 | Data$info==120 | Data$info==130 | 
                                                      Data$info==410 |  Data$info==430 | Data$info==460 | Data$info==440 | Data$info==450 | 
                                                      Data$info==470 | Data$info==480 | Data$info==491 | Data$info==240 | Data$info==340), 1, Data$relevant)

# Flipping the terms to be negative

Data$irrelevant <- ifelse(Data$relevant==1,0,1)
Data$unverifiable <- ifelse(Data$verifiable==1,0,1)

Data$badindex <- (Data$unverifiable + Data$vague + Data$irrelevant)/3
summary(lm(data=Data, badindex~polknowl))

Data$fakeid <- rep(1:nrow(Data))

dfgoodk <- Data[!is.na(Data$polknowl),c("dataset", "fakeid", "polknowl", "unverifiable", "vague", "irrelevant")] %>%
  gather(key=variable, value=value, unverifiable:irrelevant)

table(Data$dataset,Data$polknowl, useNA="ifany")

dfgoodk$variable <- factor(dfgoodk$variable, levels=c("irrelevant", "unverifiable", "vague"))


dfgoode <- Data[c("edu_coarse", "unverifiable", "irrelevant", "vague")] %>%
  gather(key=variable, value=value, unverifiable:vague)

dfgoode$variable <- factor(dfgoode$variable, levels=c("irrelevant", "unverifiable", "vague"))

eplot <- ggplot(data=dfgoode, aes(x=edu_coarse, y=value, linetype=variable)) +
  geom_smooth(method="lm", colour="black") +
  ggtitle("Requests by Respondent Education") +
  theme(plot.title= element_text(face="bold")) +
  xlab("Education") +
  ylab("Information Requests") +
  theme_minimal(base_size = 16) +
  theme(panel.grid.major = element_line(linetype='longdash', size=.4), 
        panel.grid.minor = element_line(linetype='longdash', size=.2)) +
  theme(plot.title = element_text(hjust=.5),
        legend.key.width = unit(1,"cm")) +
  scale_linetype_manual(name="Type of\nRequest", 
                        breaks=c("irrelevant", "unverifiable", "vague"),
                        values=c("dashed", "dotted", "solid"), 
                        labels=c("Irrelevant", "Unverifiable", "Vague"))

ggsave(file="si_3_1.jpg", eplot, width=11, height=7)



################ SI PLOT 3.2 ##################

# Knowledge plot 
dfgoodko <- Data[!is.na(Data$officeknowl),c("dataset", "fakeid", "officeknowl", "unverifiable", "vague", "irrelevant")] %>%
  gather(key=variable, value=value, unverifiable:irrelevant)

dfgoodko$variable <- factor(dfgoodko$variable, levels=c("irrelevant", "unverifiable", "vague"))

koplot<-ggplot(data=dfgoodko, aes(x=officeknowl, y=value, linetype=variable)) +
  geom_smooth(method="lm", colour="black") +
  ggtitle("Requests by Respondent Knowledge of\nLocal Office Responsibilities") +
  theme(plot.title= element_text(face="bold")) +
  xlab("Office Knowledge") +
  ylab("Information Requests") +
  theme_minimal(base_size = 16) +
  theme(panel.grid.major = element_line(linetype='longdash', size=.4), 
        panel.grid.minor = element_line(linetype='longdash', size=.2)) +
  theme(plot.title = element_text(hjust=.5),
        legend.key.width = unit(1,"cm")) +
  scale_linetype_manual(name="Type of\nRequest", 
                        breaks=c("irrelevant", "unverifiable", "vague"),
                        values=c("dashed", "dotted", "solid"), 
                        labels=c("Irrelevant", "Unverifiable", "Vague"))

ggsave(file="si_3_2.pdf", koplot, width=11, height=7)


################ SI PLOT 3.3 ##################

dfgoodk1 <- Data[Data$rank==1,c("polknowl", "unverifiable", "vague", "irrelevant")] %>%
  gather(key=variable, value=value, unverifiable:irrelevant)

dfgoodk1$variable <- factor(dfgoodk1$variable, levels=c("irrelevant", "unverifiable", "vague"))


fplot <- ggplot(data=dfgoodk1, aes(x=polknowl, y=value, linetype=variable)) +
  geom_smooth(method="lm", colour="black") +
  ggtitle("First Requests by Respondent Political Knowledge") +
  theme(plot.title= element_text(face="bold")) +
  xlab("Political Knowledge") +
  ylab("First Information Requests") +
  theme_minimal(base_size = 16) +
  theme(panel.grid.major = element_line(linetype='longdash', size=.4), 
        panel.grid.minor = element_line(linetype='longdash', size=.2)) +
  theme(plot.title = element_text(hjust=.5),
        legend.key.width = unit(1,"cm")) +
  scale_linetype_manual(name="Type of\nRequest", 
                        breaks=c("irrelevant", "unverifiable", "vague"),
                        values=c("dashed", "dotted", "solid"), 
                        labels=c("Irrelevant", "Unverifiable", "Vague"))

ggsave(file="si_3_3.jpg", fplot, width=11, height=7)


################ SI PLOT 3.5 ##################

dfgoodk3 <- Data[!is.na(Data$polknowl),c("dataset", "fakeid", "polknowl", "office", "unverifiable", "vague", "irrelevant")] %>%
  gather(key=variable, value=value, unverifiable:irrelevant)

dfgoodk3$variable <- factor(dfgoodk3$variable, levels=c("irrelevant", "unverifiable", "vague"))

kplotsi<-ggplot(data=dfgoodk3, aes(x=polknowl, y=value, linetype=variable)) +
  geom_smooth(method="lm", colour="black") +
  facet_wrap(~office, nrow=3, ncol=2) + 
  ggtitle("Requests by Respondent Political Knowledge and Office") +
  theme(plot.title= element_text(face="bold")) +
  xlab("Political Knowledge") +
  ylab("Information Requests") +
  theme_minimal(base_size = 16) +
  theme(panel.grid.major = element_line(linetype='longdash', size=.4), 
        panel.grid.minor = element_line(linetype='longdash', size=.2)) +
  theme(plot.title = element_text(hjust=.5),
        legend.key.width = unit(1,"cm")) +
  scale_linetype_manual(name="Type of\nRequest", 
                        breaks=c("irrelevant", "unverifiable", "vague"),
                        values=c("dashed", "dotted", "solid"), 
                        labels=c("Irrelevant", "Unverifiable", "Vague"))

ggsave(file="si_3_5.jpg", kplotsi, width=11, height=15)


################ SI PLOTS 4.1 ##################
names(Data)[names(Data) == 'o_atttype']<-"disc_enc"
Data$disc_enc[Data$disc_enc==1]<- "Disappointing Response,\nFirst Request"
Data$disc_enc[Data$disc_enc==2]<- "Disappointing Response,\nThird Request"
Data$disc_enc[Data$disc_enc==3]<- "Encouraging Response,\nFirst Request"
Data$disc_enc[Data$disc_enc==4]<- "Encouraging Response,\nThird Request"

names(Data)[names(Data) == 'att']<-"certainty"
Data$certainty[Data$dataset==1]<-(Data$certainty[Data$dataset==1]-5)/(-4)
Data$certainty[Data$dataset==3]<-(Data$certainty[Data$dataset==3]-4)/(-3) question fucked up!
Data$certainty[Data$dataset==3]<- NA
Data$certainty[Data$dataset==4]<-(Data$certainty[Data$dataset==4]-4)/(-3)

dealb.df <- subset(Data, dataset!=3)
 
dealb.df$pf<-ifelse(!is.na(dealb.df$disc_enc)&dealb.df$disc_enc=="Encouraging Response,\nFirst Request",1,0)
dealb.df$pt<-ifelse(!is.na(dealb.df$disc_enc)&dealb.df$disc_enc=="Encouraging Response,\nThird Request",1,0)
dealb.df$nf<-ifelse(!is.na(dealb.df$disc_enc)&dealb.df$disc_enc=="Discouraging Response,\nFirst Request",1,0)
dealb.df$nt<-ifelse(!is.na(dealb.df$disc_enc)&dealb.df$disc_enc=="Discouraging Response,\nThird Request",1,0)

Dtt1 <- subset(dealb.df, edu<5)
Dtt2 <- subset(dealb.df, edu>4)
Dtt3 <- subset(dealb.df, polknowl<0.5)
Dtt4 <- subset(dealb.df, polknowl>0.5)
 

dealplot1 <- ddply(Dtt1[!is.na(Dtt1$disc_enc)&!is.na(Dtt1$certainty),], "disc_enc", summarise,
                   N    = length(certainty),
                   mean = mean(certainty),
                   sd   = sd(certainty),
                   se   = sd / sqrt(N)
 )

dealplot2 <- ddply(Dtt2[!is.na(Dtt2$disc_enc)&!is.na(Dtt2$certainty),], "disc_enc", summarise,
                   N    = length(certainty),
                   mean = mean(certainty),
                   sd   = sd(certainty),
                   se   = sd / sqrt(N)
 )
 
dealplot3 <- ddply(Dtt3[!is.na(Dtt3$disc_enc)&!is.na(Dtt3$certainty),], "disc_enc", summarise,
                   N    = length(certainty),
                   mean = mean(certainty),
                   sd   = sd(certainty),
                   se   = sd / sqrt(N)
 )
 
dealplot4 <- ddply(Dtt4[!is.na(Dtt4$disc_enc)&!is.na(Dtt4$certainty),], "disc_enc", summarise,
                   N    = length(certainty),
                   mean = mean(certainty),
                   sd   = sd(certainty),
                   se   = sd / sqrt(N)
 )

dplot<-ggplot(dealplot1, aes(x=disc_enc, y=mean, ymin=(mean-1.96*se), ymax=(mean+1.96*se))) + 
   geom_pointrange() + 
   theme_minimal(base_size = 16)  + 
   theme(panel.grid.major = element_line(linetype='longdash', size=.4), 
         panel.grid.minor = element_line(linetype='longdash', size=.2),
         axis.title.y = element_text(margin=margin(r=40)),
         axis.title.x = element_text(margin=margin(t=10)),
         aspect.ratio = 1,
         plot.margin=margin(t=.5, r=0, b=.5, l=0, unit="cm")) + 
   coord_flip() + 
   ylab("Vote Certainty") +
   xlab("Type of Information Provided") +
   ggtitle("'Deal-Breaker' Heuristic, low education")
 
 ggsave(file="si_4_1_1.pdf", dplot, width=11, height=7)
 
 
 dplot<-ggplot(dealplot2, aes(x=disc_enc, y=mean, ymin=(mean-1.96*se), ymax=(mean+1.96*se))) + 
    geom_pointrange() + 
   theme_minimal(base_size = 16)  + 
    theme(panel.grid.major = element_line(linetype='longdash', size=.4), 
          panel.grid.minor = element_line(linetype='longdash', size=.2),
          axis.title.y = element_text(margin=margin(r=40)),
          axis.title.x = element_text(margin=margin(t=10)),
          aspect.ratio = 1,
          plot.margin=margin(t=.5, r=0, b=.5, l=0, unit="cm")) + 
    coord_flip() + 
    ylab("Vote Certainty") +
    xlab("Type of Information Provided") +
    ggtitle("'Deal-Breaker' Heuristic, high education")
  
  ggsave(file="si_4_1_2.pdf", dplot, width=11, height=7)
  

 dplot<-ggplot(dealplot3, aes(x=disc_enc, y=mean, ymin=(mean-1.96*se), ymax=(mean+1.96*se))) + 
     geom_pointrange() + 
     theme_minimal(base_size = 16)  + 
     theme(panel.grid.major = element_line(linetype='longdash', size=.4), 
           panel.grid.minor = element_line(linetype='longdash', size=.2),
           axis.title.y = element_text(margin=margin(r=40)),
           axis.title.x = element_text(margin=margin(t=10)),
           aspect.ratio = 1,
           plot.margin=margin(t=.5, r=0, b=.5, l=0, unit="cm")) + 
     coord_flip() + 
     ylab("Vote Certainty") +
     xlab("Type of Information Provided") +
     ggtitle("'Deal-Breaker' Heuristic, low political knowledge")
   
  ggsave(file="si_4_1_3.pdf", dplot, width=11, height=7)
  
   dplot<-ggplot(dealplot4, aes(x=disc_enc, y=mean, ymin=(mean-1.96*se), ymax=(mean+1.96*se))) + 
     geom_pointrange() + 
     theme_minimal(base_size = 16)  + 
     theme(panel.grid.major = element_line(linetype='longdash', size=.4), 
           panel.grid.minor = element_line(linetype='longdash', size=.2),
           axis.title.y = element_text(margin=margin(r=40)),
           axis.title.x = element_text(margin=margin(t=10)),
           aspect.ratio = 1,
           plot.margin=margin(t=.5, r=0, b=.5, l=0, unit="cm")) + 
     coord_flip() + 
     ylab("Vote Certainty") +
     xlab("Type of Information Provided") +
     ggtitle("'Deal-Breaker' Heuristic, high political knowledge")
   
   ggsave(file="si_4_1_4.pdf", dplot, width=11, height=7)  



################ SI PLOTS 4.2 ##################
dealplot_d1 <- dealb.df[!is.na(dealb.df$disc_enc)&dealb.df$o_type==1&dealb.df$dataset==1&!is.na(dealb.df$certainty),] %>%
      group_by(m_type) %>%
      summarise( N    = length(certainty),
                 mean = mean(certainty),
                 sd   = sd(certainty),
                 se   = sd / sqrt(N))
    
dealplot_d3 <- dealb.df[!is.na(dealb.df$disc_enc)&dealb.df$o_type==1&dealb.df$dataset==3&!is.na(dealb.df$certainty),] %>%
      group_by(m_type) %>%
      summarise( N    = length(certainty),
                 mean = mean(certainty),
                 sd   = sd(certainty),
                 se   = sd / sqrt(N))   
   
dplot_d1<-ggplot(dealplot_d1, aes(x=m_type, y=mean, ymin=(mean-1.96*se), ymax=(mean+1.96*se), colour=as.factor(good))) + 
   geom_pointrange(position=position_dodge(width=.1)) + 
   theme_minimal(base_size = 16)  + 
   theme(panel.grid.major = element_line(linetype='longdash', size=.4), 
         panel.grid.minor = element_line(linetype='longdash', size=.2),
         axis.title.y = element_text(margin=margin(r=40)),
         axis.title.x = element_text(margin=margin(t=10)),
         aspect.ratio = 1,
         plot.margin=margin(t=.5, r=0, b=.5, l=0, unit="cm")) + 
   coord_flip() + 
   ylab("Vote Certainty") +
   xlab("Mayoral Candidate") +
   ggtitle("Deal-Breakers by Candidate Gender") +
   scale_colour_manual(name="Information\nRequest\nResponse", 
                       breaks=c(0,1),
                       values=c("black", "gray60"), 
                       labels=c("Disappointing", "Encouraging"))
 
ggsave(file="dealbreakers_female_d1.pdf", dplot_d1, width=11, height=7)
 
dplot_d3<-ggplot(dealplot_d3, aes(x=m_type, y=mean, ymin=(mean-1.96*se), ymax=(mean+1.96*se), colour=as.factor(good))) + 
   geom_pointrange(position=position_dodge(width=.1)) + 
   theme_minimal(base_size = 16)  + 
   theme(panel.grid.major = element_line(linetype='longdash', size=.4), 
         panel.grid.minor = element_line(linetype='longdash', size=.2),
         axis.title.y = element_text(margin=margin(r=40)),
         axis.title.x = element_text(margin=margin(t=10)),
         aspect.ratio = 1,
         plot.margin=margin(t=.5, r=0, b=.5, l=0, unit="cm")) + 
   coord_flip() + 
   ylab("Vote Certainty") +
   xlab("Mayoral Candidate") +
   ggtitle("Deal-Breakers by Candidate Gender") +
   scale_colour_manual(name="Information\nRequest\nResponse", 
                       breaks=c(0,1),
                       values=c("black", "gray60"), 
                       labels=c("Disappointing", "Encouraging"))
 
ggsave(file="dealbreakers_female_d3.pdf", dplot_d3, width=11, height=7)
   
   
   
   
################ SI PLOT 3.4 ##################

Data <- read.csv("TMYK3_pooled_ind.csv")

#### Cleaning

# Respondent Education
# label variable education "1=less HS 2=HS 3=some coll 4=AA 5=BA 6=MA 7=PhD 8=Prof"
Data$edu_coarse <- ifelse(Data$edu<=2, 1,
                          ifelse((Data$edu==3|Data$edu==4), 2,
                                 ifelse(Data$edu==5, 3,
                                        ifelse(Data$edu>5, 4, NA))))

# Office
Data$office<-ifelse(Data$o_type==1, "Mayor",
                    ifelse(Data$o_type==2, "Governor",
                           ifelse(Data$o_type==3, "President",
                                  ifelse(Data$o_type==4, "Clerk",
                                         ifelse(Data$o_type==5, "Comptroller",
                                                ifelse(Data$o_type==6, "Judge",NA))))))
Data$office<-factor(Data$office)
Data$office<-factor(Data$office, levels(Data$office)[c(2,1,4,5,3,6)])


# Policy info (non-generic policy requests; ==policy3 in .do file)
Data$polinfo1<-ifelse(Data$subcat1==2, 1, 0)
Data$polinfo1[Data$info1==240 | Data$info1==340]<-0
Data$polinfo2<-ifelse(Data$subcat2==2, 1, 0)
Data$polinfo2[Data$info2==240 | Data$info2==340]<-0
Data$polinfo3<-ifelse(Data$subcat3==2, 1, 0)
Data$polinfo3[Data$info3==240 | Data$info3==340]<-0

# Rename soph to polknowl, standardize variable 
names(Data)[names(Data) == 'soph']<-"polknowl"
Data$polknowl<-(Data$polknowl)/(5)

# Profession and education (==profedu in .do file)
Data$profedu1<-ifelse(Data$info1==410 | Data$info1==430 | Data$info1==480 , 1, 0)
Data$profedu2<-ifelse(Data$info2==410 | Data$info2==430 | Data$info2==480 , 1, 0)
Data$profedu3<-ifelse(Data$info3==410 | Data$info3==430 | Data$info3==480 , 1, 0)

# Party and policy together
Data$political1<-ifelse(Data$subcat1==1 | Data$subcat1==2, 1, 0)
Data$political2<-ifelse(Data$subcat2==1 | Data$subcat2==2, 1, 0)
Data$political3<-ifelse(Data$subcat3==1 | Data$subcat3==2, 1, 0)

# Personal info
Data$personal1<-ifelse(Data$subcat1==3, 1, 0)
Data$personal2<-ifelse(Data$subcat2==3, 1, 0)
Data$personal3<-ifelse(Data$subcat3==3, 1, 0)

# Dealbreakers
names(Data)[names(Data) == 'o_atttype']<-"disc_enc"
Data$disc_enc[Data$disc_enc==1]<- "Disappointing Response,\nFirst Request"
Data$disc_enc[Data$disc_enc==2]<- "Disappointing Response,\nThird Request"
Data$disc_enc[Data$disc_enc==3]<- "Encouraging Response,\nFirst Request"
Data$disc_enc[Data$disc_enc==4]<- "Encouraging Response,\nThird Request"

Data$disc_enc2[Data$att_badgood==1]<- "Good"
Data$disc_enc2[Data$att_badgood==0]<- "Bad"
Data$disc_enc3[Data$att_earlylater==1]<- "First-Ranked"
Data$disc_enc3[Data$att_earlylater==0]<- "Third-Ranked"

Data$good<-ifelse(Data$att_badgood==1,1,
                  ifelse(Data$att_badgood==0,0,NA))
Data$first<-ifelse(Data$att_earlylater==1,1,
                   ifelse(Data$att_earlylater==0,0,NA))

# Certainty (1 originally certain, 5 is no change); standardize variable
names(Data)[names(Data) == 'att']<-"certainty"
Data$certainty<-(Data$certainty-5)/(-4)

# Mayor conditions
Data$nonames<-ifelse(Data$mayorcondition<4,1,0)
Data$mayorcondition<-as.factor(Data$mayorcondition)
levels(Data$mayorcondition) # 1==open, 2==incumbent, 3==challenger, 4==bf, 5==bm, 6==wf, 7==wm
Data$mayor<-NA
Data$mayor[Data$mayorcondition==1]<- "Open"
Data$mayor[Data$mayorcondition==2]<- "Incumbent"
Data$mayor[Data$mayorcondition==3]<- "Challenger"
Data$mayor[Data$mayorcondition==4]<- "Jada"
Data$mayor[Data$mayorcondition==5]<- "Jamal"
Data$mayor[Data$mayorcondition==6]<- "Jenna"
Data$mayor[Data$mayorcondition==7]<- "Jake"

# Office type conditions
Data$office<-NA
Data$office[Data$o_type==1]<- "Mayor"
Data$office[Data$o_type==2]<- "Governor"
Data$office[Data$o_type==3]<- "President"
Data$office[Data$o_type==4]<- "Clerk"
Data$office[Data$o_type==5]<- "Comptroller"
Data$office[Data$o_type==6]<- "Judge"
Data$office<-as.character(Data$office)
Data$office<-factor(Data$office, levels=c("Clerk", "Comptroller", "Judge", "Mayor", "Governor", "President"))


# Requested social issue info (i.e., possibly identity-related)
Data$social1<-ifelse(substring(Data$info1,1,2)==21,1,0)
Data$social1[substring(Data$info1,1,2)==31]<-1
Data$social2<-ifelse(substring(Data$info2,1,2)==21,1,0)
Data$social2[substring(Data$info2,1,2)==31]<-1
Data$social3<-ifelse(substring(Data$info3,1,2)==21,1,0)
Data$social3[substring(Data$info3,1,2)==31]<-1

# Requested economic issue info 
Data$economic1<-ifelse(substring(Data$info1,1,2)==22,1,0)
Data$economic1[substring(Data$info1,1,2)==32]<-1
Data$economic2<-ifelse(substring(Data$info2,1,2)==22,1,0)
Data$economic2[substring(Data$info2,1,2)==32]<-1
Data$economic3<-ifelse(substring(Data$info3,1,2)==22,1,0)
Data$economic3[substring(Data$info3,1,2)==32]<-1

# Requested other issue info
Data$other1<-ifelse(substring(Data$info1,1,2)==23,1,0)
Data$other1[substring(Data$info1,1,2)==33]<-1
Data$other2<-ifelse(substring(Data$info2,1,2)==23,1,0)
Data$other2[substring(Data$info2,1,2)==33]<-1
Data$other3<-ifelse(substring(Data$info3,1,2)==23,1,0)
Data$other3[substring(Data$info3,1,2)==33]<-1

# Requested all but generic info  
Data$spec1<-ifelse(((substring(Data$info1,1,2)==21)|(substring(Data$info1,1,2)==22)|(substring(Data$info1,1,2)==23)),1,0)
Data$spec1[substring(Data$info1,1,2)==31]<-1
Data$spec1[substring(Data$info1,1,2)==32]<-1
Data$spec1[substring(Data$info1,1,2)==33]<-1
Data$spec2<-ifelse(((substring(Data$info2,1,2)==21)|(substring(Data$info2,1,2)==22)|(substring(Data$info2,1,2)==23)),1,0)
Data$spec2[substring(Data$info2,1,2)==31]<-1
Data$spec2[substring(Data$info2,1,2)==32]<-1
Data$spec2[substring(Data$info2,1,2)==33]<-1
Data$spec3<-ifelse(((substring(Data$info3,1,2)==21)|(substring(Data$info3,1,2)==22)|(substring(Data$info3,1,2)==23)),1,0)
Data$spec3[substring(Data$info3,1,2)==31]<-1
Data$spec3[substring(Data$info3,1,2)==32]<-1
Data$spec3[substring(Data$info3,1,2)==33]<-1

# Requested generic info  
Data$gen1<-ifelse(substring(Data$info1,1,2)==24,1,0)
Data$gen1[substring(Data$info1,1,2)==34]<-1
Data$gen2<-ifelse(substring(Data$info2,1,2)==24,1,0)
Data$gen2[substring(Data$info2,1,2)==34]<-1
Data$gen3<-ifelse(substring(Data$info3,1,2)==24,1,0)
Data$gen3[substring(Data$info3,1,2)==34]<-1

h4a <- lm(data=Data, certainty~disc_enc)
summary(h4a)

Data$pf<-ifelse(!is.na(Data$disc_enc)&Data$disc_enc=="Encouraging Response,\nFirst Request",1,0)
Data$pt<-ifelse(!is.na(Data$disc_enc)&Data$disc_enc=="Encouraging Response,\nThird Request",1,0)
Data$nf<-ifelse(!is.na(Data$disc_enc)&Data$disc_enc=="Discouraging Response,\nFirst Request",1,0)
Data$nt<-ifelse(!is.na(Data$disc_enc)&Data$disc_enc=="Discouraging Response,\nThird Request",1,0)


Data$verifiable1 <- ifelse(((substring(Data$info1,1,2)==12&Data$office=="Governor")|
                              (substring(Data$info1,1,2)==13)|
                              (substring(Data$info1,1,1)==2&(substring(Data$info1,1,3)<240))|
                              (substring(Data$info1,1,1)==3&(substring(Data$info1,1,3)<340))|
                              (substring(Data$info1,1,2)<44&(substring(Data$info1,1,2)>40))|
                              (substring(Data$info1,1,2)==46)|
                              (substring(Data$info1,1,2)==47)|
                              (substring(Data$info1,1,2)==48)|
                              (substring(Data$info1,1,1)==5)),1,0)
Data$verifiable2 <- ifelse(((substring(Data$info2,1,2)==12&Data$office=="Governor")|
                              (substring(Data$info2,1,2)==13)|
                              (substring(Data$info2,1,1)==2&(substring(Data$info2,1,3)<240))|
                              (substring(Data$info2,1,1)==3&(substring(Data$info2,1,3)<340))|
                              (substring(Data$info2,1,2)<44&(substring(Data$info2,1,2)>40))|
                              (substring(Data$info2,1,2)==46)|
                              (substring(Data$info2,1,2)==47)|
                              (substring(Data$info2,1,2)==48)|
                              (substring(Data$info2,1,1)==5)),1,0)
Data$verifiable3 <- ifelse(((substring(Data$info3,1,2)==12&Data$office=="Governor")|
                              (substring(Data$info3,1,2)==13)|
                              (substring(Data$info3,1,1)==2&(substring(Data$info3,1,3)<240))|
                              (substring(Data$info3,1,1)==3&(substring(Data$info3,1,3)<340))|
                              (substring(Data$info3,1,2)<44&(substring(Data$info3,1,2)>40))|
                              (substring(Data$info3,1,2)==46)|
                              (substring(Data$info3,1,2)==47)|
                              (substring(Data$info3,1,2)==48)|
                              (substring(Data$info3,1,1)==5)),1,0)

# specific 
Data$specific1 <- ifelse(((substring(Data$info1,1,2)==21)|
                            (substring(Data$info1,1,2)==22)|
                            (substring(Data$info1,1,2)==23)|
                            (substring(Data$info1,1,2)==31)|
                            (substring(Data$info1,1,2)==32)|
                            (substring(Data$info1,1,2)==33)),1,0)
Data$specific2 <- ifelse(((substring(Data$info2,1,2)==21)|
                            (substring(Data$info2,1,2)==22)|
                            (substring(Data$info2,1,2)==23)|
                            (substring(Data$info2,1,2)==31)|
                            (substring(Data$info2,1,2)==32)|
                            (substring(Data$info2,1,2)==33)),1,0)
Data$specific3 <- ifelse(((substring(Data$info3,1,2)==21)|
                            (substring(Data$info3,1,2)==22)|
                            (substring(Data$info3,1,2)==23)|
                            (substring(Data$info3,1,2)==31)|
                            (substring(Data$info3,1,2)==32)|
                            (substring(Data$info3,1,2)==33)),1,0)

Data$vague1 <- ifelse((Data$info1==240)|(Data$info1==340)|(Data$info1>599),1,0)
Data$vague2 <- ifelse((Data$info2==240)|(Data$info2==340)|(Data$info2>599),1,0)
Data$vague3 <- ifelse((Data$info3==240)|(Data$info3==340)|(Data$info3>599),1,0)

# discriminatory
Data$discrim1 <- ifelse((substring(Data$info1,1,1)==5),1,0)
Data$discrim2 <- ifelse((substring(Data$info2,1,1)==5),1,0)
Data$discrim3 <- ifelse((substring(Data$info3,1,1)==5),1,0)

# relevance 
Data$relevant1<-NA
Data$relevant2<-NA
Data$relevant3<-NA

# Comptroller
Data$relevant1 <- ifelse(Data$office=="Comptroller" & ((substring(Data$info1,1,2)==22) | 
                                                         (substring(Data$info1,1,2)==32) | Data$info1==237 |  Data$info1==238 | Data$info1==337 | Data$info1==338 | 
                                                         Data$info1==410 |  Data$info1==430 | Data$info1==460 | Data$info1==440 | Data$info1==450 | 
                                                         Data$info1==470 | Data$info1==480 | Data$info1==491 | Data$info1==240 | Data$info1==340), 1, 0)
Data$relevant2 <- ifelse(Data$office=="Comptroller" & ((substring(Data$info2,1,2)==22) | 
                                                         (substring(Data$info2,1,2)==32) | Data$info2==237 |  Data$info2==238 | Data$info2==337 | Data$info2==338 | 
                                                         Data$info2==410 |  Data$info2==430 | Data$info2==460 | Data$info2==440 | Data$info2==450 | 
                                                         Data$info2==470 | Data$info2==480 | Data$info2==491 | Data$info2==240 | Data$info2==340), 1, 0)
Data$relevant3 <- ifelse(Data$office=="Comptroller" & ((substring(Data$info3,1,2)==22) | 
                                                         (substring(Data$info3,1,2)==32) | Data$info3==237 |  Data$info3==238 | Data$info3==337 | Data$info3==338 | 
                                                         Data$info3==410 |  Data$info3==430 | Data$info3==460 | Data$info3==440 | Data$info3==450 | 
                                                         Data$info3==470 | Data$info3==480 | Data$info3==491 | Data$info3==240 | Data$info3==340), 1, 0)
# CLerk
Data$relevant1 <- ifelse(Data$office=="Clerk" & (Data$info1==110 | Data$info1==238 | Data$info1==338 | 
                                                   Data$info1==410 |  Data$info1==430 | Data$info1==460 | Data$info1==440 | Data$info1==450 | 
                                                   Data$info1==470 | Data$info1==480 | Data$info1==491 | Data$info1==240 | Data$info1==340), 1, Data$relevant1)
Data$relevant2 <- ifelse(Data$office=="Clerk" & (Data$info2==110 | Data$info2==238 | Data$info2==338 | 
                                                   Data$info2==410 |  Data$info2==430 | Data$info2==460 | Data$info2==440 | Data$info2==450 | 
                                                   Data$info2==470 | Data$info2==480 | Data$info2==491 | Data$info2==240 | Data$info2==340), 1, Data$relevant2)
Data$relevant3 <- ifelse(Data$office=="Clerk" & (Data$info3==110 | Data$info3==238 | Data$info3==338 | 
                                                   Data$info3==410 |  Data$info3==430 | Data$info3==460 | Data$info3==440 | Data$info3==450 | 
                                                   Data$info3==470 | Data$info3==480 | Data$info3==491 | Data$info3==240 | Data$info3==340), 1, Data$relevant3)

# Judge
Data$relevant1 <- ifelse(Data$office=="Judge" & ((substring(Data$info1,1,2)==21) | 
                                                   (substring(Data$info1,1,2)==31) | Data$info1==237 |  Data$info1==238 | Data$info1==337 | Data$info1==338 | 
                                                   Data$info1==110 | Data$info1==120 | Data$info1==130 | 
                                                   Data$info1==410 |  Data$info1==430 | Data$info1==460 | Data$info1==440 | Data$info1==450 | 
                                                   Data$info1==470 | Data$info1==480 | Data$info1==491 | Data$info1==240 | Data$info1==340), 1, Data$relevant1)
Data$relevant2 <- ifelse(Data$office=="Judge" & ((substring(Data$info2,1,2)==21) | 
                                                   (substring(Data$info2,1,2)==31) | Data$info2==237 |  Data$info2==238 | Data$info2==337 | Data$info2==338 | 
                                                   Data$info2==110 | Data$info2==120 | Data$info2==130 | 
                                                   Data$info2==410 |  Data$info2==430 | Data$info2==460 | Data$info2==440 | Data$info2==450 | 
                                                   Data$info2==470 | Data$info2==480 | Data$info2==491 | Data$info2==240 | Data$info2==340), 1, Data$relevant2)
Data$relevant3 <- ifelse(Data$office=="Judge" & ((substring(Data$info3,1,2)==21) | 
                                                   (substring(Data$info3,1,2)==31) | Data$info3==237 |  Data$info3==238 | Data$info3==337 | Data$info3==338 | 
                                                   Data$info3==110 | Data$info3==120 | Data$info3==130 | 
                                                   Data$info3==410 |  Data$info3==430 | Data$info3==460 | Data$info3==440 | Data$info3==450 | 
                                                   Data$info3==470 | Data$info3==480 | Data$info3==491 | Data$info3==240 | Data$info3==340), 1, Data$relevant3)

# Mayor
Data$relevant1 <- ifelse(Data$office=="Mayor" & ((substring(Data$info1,1,1)==2 & (Data$info1!=226 & Data$info1!=231 & Data$info1!=232)) | 
                                                   (substring(Data$info1,1,1)==3 & (Data$info1!=326 & Data$info1!=331 & Data$info1!=332)) | 
                                                   Data$info1==110 | Data$info1==120 | Data$info1==130 | 
                                                   Data$info1==410 |  Data$info1==430 | Data$info1==460 | Data$info1==440 | Data$info1==450 | 
                                                   Data$info1==470 | Data$info1==480 | Data$info1==491 | Data$info1==240 | Data$info1==340), 1, Data$relevant1)
Data$relevant2 <- ifelse(Data$office=="Mayor" & ((substring(Data$info2,1,1)==2 & (Data$info2!=226 & Data$info2!=231 & Data$info2!=232)) | 
                                                   (substring(Data$info2,1,1)==3 & (Data$info2!=326 & Data$info2!=331 & Data$info2!=332)) | 
                                                   Data$info2==110 | Data$info2==120 | Data$info2==130 | 
                                                   Data$info2==410 |  Data$info2==430 | Data$info2==460 | Data$info2==440 | Data$info2==450 | 
                                                   Data$info2==470 | Data$info2==480 | Data$info2==491 | Data$info2==240 | Data$info2==340), 1, Data$relevant2)
Data$relevant3 <- ifelse(Data$office=="Mayor" & ((substring(Data$info3,1,1)==2 & (Data$info3!=226 & Data$info3!=231 & Data$info3!=232)) | 
                                                   (substring(Data$info3,1,1)==3 & (Data$info3!=326 & Data$info3!=331 & Data$info3!=332)) | 
                                                   Data$info3==110 | Data$info3==120 | Data$info3==130 | 
                                                   Data$info3==410 |  Data$info3==430 | Data$info3==460 | Data$info3==440 | Data$info3==450 | 
                                                   Data$info3==470 | Data$info3==480 | Data$info3==491 | Data$info3==240 | Data$info3==340), 1, Data$relevant3)

# Governor
Data$relevant1 <- ifelse(Data$office=="Governor" & ((substring(Data$info1,1,1)==2  & (Data$info1!=231)) | 
                                                      (substring(Data$info1,1,1)==3 & (Data$info1!=331)) | 
                                                      Data$info1==110 | Data$info1==120 | Data$info1==130 | 
                                                      Data$info1==410 |  Data$info1==430 | Data$info1==460 | Data$info1==440 | Data$info1==450 | 
                                                      Data$info1==470 | Data$info1==480 | Data$info1==491 | Data$info1==240 | Data$info1==340), 1, Data$relevant1)
Data$relevant2 <- ifelse(Data$office=="Governor" & ((substring(Data$info2,1,1)==2  & (Data$info2!=231)) | 
                                                      (substring(Data$info2,1,1)==3 & (Data$info2!=331)) | 
                                                      Data$info2==110 | Data$info2==120 | Data$info2==130 | 
                                                      Data$info2==410 |  Data$info2==430 | Data$info2==460 | Data$info2==440 | Data$info2==450 | 
                                                      Data$info2==470 | Data$info2==480 | Data$info2==491 | Data$info2==240 | Data$info2==340), 1, Data$relevant2)
Data$relevant3 <- ifelse(Data$office=="Governor" & ((substring(Data$info3,1,1)==2  & (Data$info3!=231)) | 
                                                      (substring(Data$info3,1,1)==3 & (Data$info3!=331)) | 
                                                      Data$info3==110 | Data$info3==120 | Data$info3==130 | 
                                                      Data$info3==410 |  Data$info3==430 | Data$info3==460 | Data$info3==440 | Data$info3==450 | 
                                                      Data$info3==470 | Data$info3==480 | Data$info3==491 | Data$info3==240 | Data$info3==340), 1, Data$relevant3)

# President
Data$relevant1 <- ifelse(Data$office=="President" & ((substring(Data$info1,1,1)==2) | 
                                                       (substring(Data$info1,1,1)==3) | 
                                                       Data$info1==110 | Data$info1==120 | Data$info1==130 | 
                                                       Data$info1==410 |  Data$info1==430 | Data$info1==460 | Data$info1==440 | Data$info1==450 | 
                                                       Data$info1==470 | Data$info1==480 | Data$info1==491 | Data$info1==240 | Data$info1==340), 1, Data$relevant1)
Data$relevant2 <- ifelse(Data$office=="President" & ((substring(Data$info2,1,1)==2) | 
                                                       (substring(Data$info2,1,1)==3) | 
                                                       Data$info2==110 | Data$info2==120 | Data$info2==130 | 
                                                       Data$info2==410 |  Data$info2==430 | Data$info2==460 | Data$info2==440 | Data$info2==450 | 
                                                       Data$info2==470 | Data$info2==480 | Data$info2==491 | Data$info2==240 | Data$info2==340), 1, Data$relevant2)
Data$relevant3 <- ifelse(Data$office=="President" & ((substring(Data$info3,1,1)==2) | 
                                                       (substring(Data$info3,1,1)==3) | 
                                                       Data$info3==110 | Data$info3==120 | Data$info3==130 | 
                                                       Data$info3==410 |  Data$info3==430 | Data$info3==460 | Data$info3==440 | Data$info3==450 | 
                                                       Data$info3==470 | Data$info3==480 | Data$info3==491 | Data$info3==240 | Data$info3==340), 1, Data$relevant3)

# Flipping the terms to be negative

Data$irrelevant1 <- ifelse(Data$relevant1==1,0,1)
Data$irrelevant2 <- ifelse(Data$relevant2==1,0,1)
Data$irrelevant3 <- ifelse(Data$relevant3==1,0,1)

Data$unverifiable1 <- ifelse(Data$verifiable1==1,0,1)
Data$unverifiable2 <- ifelse(Data$verifiable2==1,0,1)
Data$unverifiable3 <- ifelse(Data$verifiable3==1,0,1)

Data$personalistic1 <- ifelse(Data$discrim1==1,1,0)
Data$personalistic2 <- ifelse(Data$discrim2==1,1,0)
Data$personalistic3 <- ifelse(Data$discrim3==1,1,0)

Data$unverifiable4 <- ifelse((Data$unverifiable1==1|Data$unverifiable2==1|Data$unverifiable3==1),1,0)

Data$irrelevant4 <- ifelse((Data$irrelevant1==1|Data$irrelevant2==1|Data$irrelevant3==1),1,0)

Data$personalistic4 <- ifelse((Data$personalistic1==1|Data$personalistic2==1|Data$personalistic3==1),1,0)

Data$vague4 <- ifelse((Data$vague1==1|Data$vague2==1|Data$vague3==1),1,0)

Data$badstuff <- ifelse((Data$vague4==1|Data$unverifiable4==1|Data$irrelevant4==1),1,0)

dfgoodk <- Data[c("polknowl", "unverifiable4", "vague4", "irrelevant4", "badstuff")] %>%
  gather(key=variable, value=value, unverifiable4:badstuff)

dfgoodk$variable <- factor(dfgoodk$variable, levels=c("irrelevant4", "unverifiable4", "vague4", "badstuff"))

kplot<-ggplot(data=dfgoodk, aes(x=polknowl, y=value, linetype=variable)) +
  geom_smooth(method="lm", colour="black") +
  ggtitle("Requests by Respondent Political Knowledge") +
  theme(plot.title= element_text(face="bold")) +
  xlab("Political Knowledge") +
  ylab("Information Requests") +
  theme_minimal(base_size = 16) +
  theme(panel.grid.major = element_line(linetype='longdash', size=.4), 
        panel.grid.minor = element_line(linetype='longdash', size=.2)) +
  theme(plot.title = element_text(hjust=.5),
        legend.key.width = unit(1,"cm")) +
  scale_linetype_manual(name="Type of\nRequest", 
                        breaks=c("irrelevant4", "unverifiable4", "vague4", "badstuff"),
                        values=c("dashed", "dotted", "twodash", "solid"), 
                        labels=c("Irrelevant", "Unverifiable", "Vague", "Undesirable"))

ggsave(file="si_3_4.pdf", kplot, width=11, height=7)





################ SI PLOT 4.3 ##################
Data$obscure <- ifelse(Data$office=="Comptroller" | Data$office=="Judge" | Data$office == "Clerk", "More Obscure:\nClerk, Comptroller, Judge", "Less Obscure:\nMayor, Governor, President")

dealploto <- Data[!is.na(Data$disc_enc)&!is.na(Data$certainty),] %>%
  group_by(obscure, disc_enc) %>%
  summarise( N    = length(certainty),
             mean = mean(certainty),
             sd   = sd(certainty),
             se   = sd / sqrt(N))

dploto<-ggplot(dealploto, aes(x=disc_enc, y=mean, ymin=(mean-1.96*se), ymax=(mean+1.96*se))) + 
  geom_pointrange() + 
  facet_grid(.~obscure) + 
  theme_minimal(base_size = 16)  + 
  theme(panel.grid.major = element_line(linetype='longdash', size=.4), 
        panel.grid.minor = element_line(linetype='longdash', size=.2),
        axis.title.y = element_text(margin=margin(r=40)),
        axis.title.x = element_text(margin=margin(t=10)),
        aspect.ratio = 1,
        plot.margin=margin(t=.5, r=0, b=.5, l=0, unit="cm")) + 
  coord_flip() + 
  ylab("Vote Certainty") +
  xlab("Type of Information Provided") +
  ggtitle("Evidence for a 'Deal-Breaker' Heuristic")


ggsave(file="si_4_3.jpg", dploto, width=11, height=7)

















