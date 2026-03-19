######################################################
# Figure 1: Reasons for not vaccination and watcher  #
######################################################

###-----load package
library(cjoint)
library(tidyverse)
library(openxlsx)
library(ggplot2)
library(gridExtra)
library(psych)


#######Yahoo##########

###-------load data


cd<-read.csv("covid19_normal.csv")
attach(cd)


###-----figure: reasons for wait and see

#set variables
Q21.2[Q21.2==6]<-NA
Q21.2[Q21.2==7]<-NA
Q21.2[Q21.2==8]<-NA
ws<-prop.table(table(Q21.2))
ws0<-c("side effect","domestic vaccin","herd immunity","no-infection confidence",
       "others priority","no-deterioration confidence")

#set data frame for table
tb_bar1<-data.frame(cbind(ws0,ws))

#set levels order
tb_bar1$ws0 <- factor(tb_bar1$ws0,
                      levels = c("no-infection confidence",
                                 "no-deterioration confidence","domestic vaccin",
                                 "herd immunity","others priority","side effect"))
tb_bar1$ws<-round(as.numeric(ws*100),1)

#plot
bar1<-ggplot(data=tb_bar1, aes(x=ws0,y=ws)) +
  geom_bar(stat="identity")+
  coord_flip()+
  ylab("Proportion")+
  xlab("Reasons")+
  ggtitle("Reasons for Wait and See (YCS)")+
  geom_text(aes(label=ws), hjust=1.3, color="white", size=4.0)
bar1


####--------------------reasons for not vaccination
#set variables
Q21.3[Q21.3==7]<-NA
Q21.3[Q21.3==8]<-NA
Q21.3[Q21.3==9]<-NA
ws2<-prop.table(table(Q21.3))
ws02<-c("side effect","domestic vaccin","herd immunity","no-infection confidence",
        "no-deterioration confidence","others priority")

#set data frame for table
tb_bar2<-data.frame(cbind(ws02,ws2))

#set levels order
tb_bar2$ws02 <- factor(tb_bar2$ws02,
                       levels = c("no-infection confidence",
                                  "no-deterioration confidence","domestic vaccin",
                                  "herd immunity","others priority","side effect"))
tb_bar2$ws2<-round(as.numeric(ws2*100),1)

#plot
bar2<-ggplot(data=tb_bar2, aes(x=ws02,y=ws2)) +
  geom_bar(stat="identity")+
  coord_flip()+
  ylab("Proportion")+
  xlab("Reasons")+
  ggtitle("Reasons for not vaccination (YCS)")+
  geom_text(aes(label=ws2), hjust=1.3, color="white", size=4.0)
bar2

#############Lucid#################


###-------load data

cdl<-read.csv("covid19_normal_lucid.csv")
attach(cdl)


###-----figure: reasons for wait and see
#set variables
cdl$Q12.2[cdl$Q12.2==6]<-NA
cdl$Q12.2[cdl$Q12.2==7]<-NA
cdl$Q12.2[cdl$Q12.2==8]<-NA
wsl<-prop.table(table(cdl$Q12.2))
ws0l<-c("side effect","domestic vaccin","herd immunity","no-infection confidence",
        "others priority","no-deterioration confidence")

#set data frame for table
tb_bar1_l<-data.frame(cbind(ws0l,wsl))

#set levels order
tb_bar1_l$ws0l<- factor(tb_bar1_l$ws0l,
                        levels = c("no-infection confidence",
                                   "no-deterioration confidence","domestic vaccin",
                                   "herd immunity","others priority","side effect"))
tb_bar1_l$wsl<-round(as.numeric(wsl*100),1)

#plot
bar1_l<-ggplot(data=tb_bar1_l, aes(x=ws0l,y=wsl)) +
  geom_bar(stat="identity")+
  coord_flip()+
  ylab("Proportion")+
  xlab("Reasons")+
  ggtitle("Reasons for Wait and See (Lucid)")+
  geom_text(aes(label=wsl), hjust=1.3, color="white", size=4.0)
bar1_l


###----------------reasons for not vaccination
#set variables
cdl$Q12.3[cdl$Q12.3==7]<-NA
cdl$Q12.3[cdl$Q12.3==8]<-NA
cdl$Q12.3[cdl$Q12.3==9]<-NA
ws2l<-prop.table(table(cdl$Q12.3))
ws02l<-c("side effect","domestic vaccin","herd immunity","no-infection confidence",
         "no-deterioration confidence","others priority","infected")

#set data frame for table
tb_bar2l<-data.frame(cbind(ws02l,ws2l))

#set levels order
tb_bar2l$ws02l <- factor(tb_bar2l$ws02l,
                         levels = c("infected", "no-infection confidence",
                                    "no-deterioration confidence","domestic vaccin",
                                    "herd immunity","others priority","side effect"))
tb_bar2l$ws2l<-round(as.numeric(ws2l*100),1)

#plot
bar2l<-ggplot(data=tb_bar2l, aes(x=ws02l,y=ws2l)) +
  geom_bar(stat="identity")+
  coord_flip()+
  ylab("Proportion")+
  xlab("Reasons")+
  ggtitle("Reasons for no vaccination (Lucid)")+
  geom_text(aes(label=ws2l), hjust=1.3, color="white", size=4.0)
bar2l


###---------arrange above four figures

windows(width = 24, height = 16)
img<-grid.arrange(bar1, bar2, bar1_l,bar2l, ncol = 2, nrow = 2)
print(img)
ggsave(file = "fig1_reasons_novac.pdf", plot = img)



 
####-----------------------------cross-bar---------------------------###


#setting variables

#age
cd$age<-as.numeric(cd$Q2.2)
cd$age[cd$age==444]<-NA
cd$age[cd$age==19750618]<-NA
cd$age[cd$age<=19]<-NA
cd$age[cd$age>80]<-NA

cd$age[cd$age>=20 & cd$age<30]<-"20-29"
cd$age[cd$age>=30 & cd$age<40]<-"30-39"
cd$age[cd$age>=40 & cd$age<50]<-"40-49"
cd$age[cd$age>=50 & cd$age<60]<-"50-59"
cd$age[cd$age>=60 & cd$age<70]<-"60-69"
cd$age[cd$age>=70 & cd$age<=80]<-"70-79"


#income
cd$income<-as.numeric(cd$Q4.1_1)


#gender
cd$genderd<-as.numeric(cd$Q2.1)
cd$genderd[cd$gender==2]<-0
cd$genderd[cd$gender==3]<-NA
cd$genderd[cd$gender==4]<-NA

#education
cd$educationd<-cd$Q2.4
cd$educationd[cd$Q2.4==1]<-0
cd$educationd[cd$Q2.4==2]<-0
cd$educationd[cd$Q2.4==3]<-0
cd$educationd[cd$Q2.4==4]<-1
cd$educationd[cd$Q2.4==5]<-1
cd$educationd[cd$Q2.4==6]<-NA
cd$educationd[cd$Q2.4==7]<-NA



###3yahoo:gender 

ws2<-prop.table(table(Q21.2,cd$genderd))
ws0l<-c("side effect","domestic vaccin","herd immunity","no-infection confidence",
        "others priority","no-deterioration confidence")



#set data frame for table
tb_bar1<-data.frame(cbind(ws0,ws2))
colnames(tb_bar1)<-c("reasons","male","female")
#set levels order
tb_bar1$reasons <- factor(tb_bar1$reasons,
                          levels = c("no-infection confidence",
                                     "no-deterioration confidence","domestic vaccin",
                                     "herd immunity","others priority","side effect"))


tb_bar_gendery<-gather(tb_bar1, key = Sex, value = Proportion, -reasons)


bar_gendery = ggplot(tb_bar_gendery, aes(x = reasons,
                                         y = round(as.numeric(Proportion)*100,1),
                                         fill=Sex))+
  geom_col(position = "dodge") +
  scale_fill_grey()+
  coord_flip()+
  ylab("proportion")+ggtitle("Reasons for wait-and-see by sex: YCS")+
  geom_text(aes(label=round(as.numeric(Proportion)*100,1)), hjust=1.3, color="white", size=4.0)

print(bar_gendery)





###3yahoo:gender no vac 

ws2<-prop.table(table(cd$Q21.3,cd$genderd))
ws0l<-c("side effect","domestic vaccin","herd immunity","no-infection confidence",
        "no-deterioration confidence","others priority","infected")



#set data frame for table
tb_bar1<-data.frame(cbind(ws0,ws2))
colnames(tb_bar1)<-c("reasons","male","female")
#set levels order
tb_bar1$reasons <- factor(tb_bar1$reasons,
                          levels = c("infected", "no-infection confidence",
                                     "no-deterioration confidence","domestic vaccin",
                                     "herd immunity","others priority","side effect"))


tb_bar_gendery<-gather(tb_bar1, key = Sex, value = Proportion, -reasons)


bar_gendery2 = ggplot(tb_bar_gendery, aes(x = reasons,
                                         y = as.numeric(Proportion), fill=Sex))+
  geom_col(position = "dodge") +
  scale_fill_grey()+
  coord_flip()+
  ylab("proportion")+ggtitle("Reasons for non-vaccination by sex: YCS")
print(bar_gendery2)






#############cross-bar: gender Lucid

###------setting variables: lucid


#agel
cdl$agel<-as.numeric(cdl$Q2.2)
cdl$agel[cdl$agel==3690306]<-NA
cdl$agel[cdl$agel==5770818]<-NA
cdl$agel[cdl$agel==97]<-NA
cdl$agel[cdl$agel==100]<-NA
cdl$agel[cdl$agel==2]<-NA
cdl$agel[cdl$agel==3]<-NA
cdl$agel[cdl$agel==5]<-NA
cdl$agel[cdl$agel==11]<-NA
cdl$agel[cdl$agel==12]<-NA
cdl$agel[cdl$agel==13]<-NA
cdl$agel[cdl$agel==14]<-NA
cdl$agel[cdl$agel==15]<-NA
cdl$agel[cdl$agel==16]<-NA
cdl$agel[cdl$agel==17]<-NA
cdl$agel[cdl$agel<=19]<-NA
cdl$agel[cdl$agel>80]<-NA
cdl$agel[cdl$agel>=20 & cdl$agel<30]<-"20-29"
cdl$agel[cdl$agel>=30 & cdl$agel<40]<-"30-39"
cdl$agel[cdl$agel>=40 & cdl$agel<50]<-"40-49"
cdl$agel[cdl$agel>=50 & cdl$agel<60]<-"50-59"
cdl$agel[cdl$agel>=60 & cdl$agel<70]<-"60-69"
cdl$agel[cdl$agel>=70 & cdl$agel<=80]<-"70-79"


#income
cdl$income<-as.numeric(cdl$Q4.1_1)


#genderl
cdl$genderdl<-as.numeric(cdl$Q2.1)
cdl$genderdl[cdl$genderdl==2]<-0
cdl$genderdl[cdl$genderdl==3]<-NA
cdl$genderdl[cdl$genderdl==4]<-NA

#educationl
cdl$educationld<-cdl$Q2.4
cdl$educationld[cdl$Q2.4==1]<-0
cdl$educationld[cdl$Q2.4==2]<-0
cdl$educationld[cdl$Q2.4==3]<-0
cdl$educationld[cdl$Q2.4==4]<-1
cdl$educationld[cdl$Q2.4==5]<-1
cdl$educationld[cdl$Q2.4==6]<-NA
cdl$educationld[cdl$Q2.4==7]<-NA



###lucid:gender 

ws2<-prop.table(table(cdl$Q12.2,cdl$genderdl))
ws0l<-c("side effect","domestic vaccin","herd immunity","no-infection confidence",
        "others priority","no-deterioration confidence")



#set data frame for table
tb_bar1<-data.frame(cbind(ws0l,ws2))
colnames(tb_bar1)<-c("reasons","male","female")
#set levels order
tb_bar1$reasons <- factor(tb_bar1$reasons,
                          levels = c("no-infection confidence",
                                     "no-deterioration confidence","domestic vaccin",
                                     "herd immunity","others priority","side effect"))


tb_bar_genderl<-gather(tb_bar1, key = Sex, value = Proportion, -reasons)


bar_genderl = ggplot(tb_bar_genderl, aes(x = reasons,
                                         y = as.numeric(Proportion), fill=Sex))+
  geom_col(position = "dodge") +
  scale_fill_grey()+
  coord_flip()+
  ylab("proportion")+ggtitle("Reasons for wait-and-see by sex: Lucid")
print(bar_genderl)





###lucid:gender no vac 

ws2<-prop.table(table(cdl$Q12.3,cdl$genderdl))
ws0l<-c("side effect","domestic vaccin","herd immunity","no-infection confidence",
        "no-deterioration confidence","others priority","infected")



#set data frame for table
tb_bar1<-data.frame(cbind(ws0,ws2))
colnames(tb_bar1)<-c("reasons","male","female")
#set levels order
tb_bar1$reasons <- factor(tb_bar1$reasons,
                          levels = c("infected", "no-infection confidence",
                                     "no-deterioration confidence","domestic vaccin",
                                     "herd immunity","others priority","side effect"))


tb_bar_gendery<-gather(tb_bar1, key = Sex, value = Proportion, -reasons)


bar_genderl2 = ggplot(tb_bar_gendery, aes(x = reasons,
                                          y = as.numeric(Proportion), fill=Sex))+
  geom_col(position = "dodge") +
  scale_fill_grey()+
  coord_flip()+
  ylab("proportion")+ggtitle("Reasons for non-vaccination by sex: Lucid")
print(bar_genderl2)



###---------arrange above two figures

windows(width = 24, height = 16)
img<-grid.arrange(bar_gendery,bar_genderl,bar_gendery2,bar_genderl2, ncol = 2, nrow = 2)
print(img)
ggsave(file = "figA1_reasons_novac_gender.pdf", plot = img)



#####cross-bar: education yahoo



###yahoo:education

ws2<-prop.table(table(Q21.2,cd$educationd))
ws0l<-c("side effect","domestic vaccin","herd immunity","no-infection confidence",
        "others priority","no-deterioration confidence")



#set data frame for table
tb_bar1<-data.frame(cbind(ws0,ws2))
colnames(tb_bar1)<-c("reasons","non-college glad.","college grad.")
#set levels order
tb_bar1$reasons <- factor(tb_bar1$reasons,
                          levels = c("no-infection confidence",
                                     "no-deterioration confidence","domestic vaccin",
                                     "herd immunity","others priority","side effect"))


tb_bar_educationy<-gather(tb_bar1, key = Education, value = Proportion, -reasons)


bar_educationy = ggplot(tb_bar_educationy, aes(x = reasons,
                                         y = as.numeric(Proportion), fill=Education))+
  geom_col(position = "dodge") +
  scale_fill_grey()+
  coord_flip()+
  ylab("proportion")+ggtitle("Reasons for wait-and-see by education: YCS")
print(bar_educationy)


###yahoo:education novac

ws2<-prop.table(table(Q21.3,cd$educationd))
ws0l<-c("side effect","domestic vaccin","herd immunity","no-infection confidence",
        "no-deterioration confidence","others priority","infected")



#set data frame for table
tb_bar1<-data.frame(cbind(ws0,ws2))
colnames(tb_bar1)<-c("reasons","non-college glad.","college grad.")
#set levels order
tb_bar1$reasons <- factor(tb_bar1$reasons,
                          levels = c("infected", "no-infection confidence",
                                     "no-deterioration confidence","domestic vaccin",
                                     "herd immunity","others priority","side effect"))


tb_bar_educationy<-gather(tb_bar1, key = Education, value = Proportion, -reasons)


bar_educationy2 = ggplot(tb_bar_educationy, aes(x = reasons,
                                               y = as.numeric(Proportion), fill=Education))+
  geom_col(position = "dodge") +
  scale_fill_grey()+
  coord_flip()+
  ylab("proportion")+ggtitle("Reasons for no vaccination by education: YCS")
print(bar_educationy2)


###lucid:education

ws2<-prop.table(table(cdl$Q12.2,cdl$educationld))
ws0l<-c("side effect","domestic vaccin","herd immunity","no-infection confidence",
        "others priority","no-deterioration confidence")



#set data frame for table
tb_bar1<-data.frame(cbind(ws0,ws2))
colnames(tb_bar1)<-c("reasons","non-college glad.","college grad.")
#set levels order
tb_bar1$reasons <- factor(tb_bar1$reasons,
                          levels = c("no-infection confidence",
                                     "no-deterioration confidence","domestic vaccin",
                                     "herd immunity","others priority","side effect"))


tb_bar_educationl<-gather(tb_bar1, key = Education, value = Proportion, -reasons)


bar_educationl = ggplot(tb_bar_educationl, aes(x = reasons,
                                               y = as.numeric(Proportion), fill=Education))+
  geom_col(position = "dodge") +
  scale_fill_grey()+
  coord_flip()+
  ylab("proportion")+ggtitle("Reasons for wait-and-see by education: Lucid")
print(bar_educationl)




###lucid:education novac

ws2<-prop.table(table(cdl$Q12.3,cdl$educationld))
ws0l<-c("side effect","domestic vaccin","herd immunity","no-infection confidence",
        "no-deterioration confidence","others priority","infected")



#set data frame for table
tb_bar1<-data.frame(cbind(ws0,ws2))
colnames(tb_bar1)<-c("reasons","non-college glad.","college grad.")
#set levels order
tb_bar1$reasons <- factor(tb_bar1$reasons,
                          levels = c("infected", "no-infection confidence",
                                     "no-deterioration confidence","domestic vaccin",
                                     "herd immunity","others priority","side effect"))


tb_bar_educationy<-gather(tb_bar1, key = Education, value = Proportion, -reasons)


bar_educationl2 = ggplot(tb_bar_educationy, aes(x = reasons,
                                                y = as.numeric(Proportion), fill=Education))+
  geom_col(position = "dodge") +
  scale_fill_grey()+
  coord_flip()+
  ylab("proportion")+ggtitle("Reasons for no vaccination by education: YCS")
print(bar_educationl2)




###---------arrange above two figures

windows(width = 24, height = 16)
img<-grid.arrange(bar_educationy,bar_educationl,bar_educationy2,bar_educationl2, ncol = 2, nrow = 2)
print(img)
ggsave(file = "figA2_reasons_novac_education.pdf", plot = img)
