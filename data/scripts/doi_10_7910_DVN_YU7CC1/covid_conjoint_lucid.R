##########################
# conjoint for COVID-19  #
# Lucid                  #
##########################



###-----load package
library(cjoint)
library(tidyverse)
library(openxlsx)
library(ggplot2)


###-------load data

covid<-read.csv("covid19_normal.csv")
attach(covid)

conjoint_data_l <- read.qualtrics("covid19_lucid.csv",
                                responses=c("Q11.1","Q11.2", "Q11.3","Q11.4",
                                            "Q11.5"),
                                covariates = c("Q2.1","Q2.2","Q2.4","Q10"),
                                respondentID="ID",new.format = F)

conjoint_data_l<-na.omit(conjoint_data_l)
write.xlsx(conjoint_data,"covid_cj.csv")

cdl<-read.csv("covid19_normal_lucid.csv")
attach(cdl)


###-----simple descriptive analysis------###

#willingness of vaccination
Q12.1[Q12.1==4]<-NA
Q12.1[Q12.1==5]<-NA
prop.table(table(Q12.1))

##reasons for wait and see
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
tb_bar1_l$wsl<-as.numeric(tb_bar1_l$wsl)

#plot
bar1_l<-ggplot(data=tb_bar1_l, aes(x=ws0l,y=wsl)) +
  geom_bar(stat="identity")+
  coord_flip()+
  ylab("Proportion")+
  xlab("Reasons")+
  ggtitle("Reasons for Wait and See (Lucid)")
bar1_l


##reasons for not vaccination
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
tb_bar2l$ws2l<-as.numeric(tb_bar2l$ws2l)

#plot
bar2l<-ggplot(data=tb_bar2l, aes(x=ws02l,y=ws2l)) +
  geom_bar(stat="identity")+
  coord_flip()+
  ylab("Proportion")+
  xlab("Reasons")+
  ggtitle("Reasons for no vaccination (Lucid)")
bar2l



###--------familiar entity--------###

#set variables

famillu<-prop.table(table(cdl$Q10.1))
famil_lablu<-c("older family","neighbor","lover","spouse",
             "friend","younger family","colleague")

#set data frame for table
tb_famillu<-data.frame(cbind(famil_lablu,famillu))

#set levels order
tb_famillu$famil_lablu <- factor(tb_famillu$famil_lablu,levels = c("neighbor","colleague","lover","friend",
                                                           "younger family","older family","spouse"))
tb_famillu$famillu<-as.numeric(famillu)

#plot
famil_barlu<-ggplot(data=tb_famillu, aes(x=famil_lablu,y=famillu)) +
  geom_bar(stat="identity")+
  coord_flip()+
  ylab("Proportion")+
  xlab("")+
  ggtitle("Familiar Entity (Lucid)")
famil_barlu



##########----------------conjoint-----------------------###################


###-------setting Qualtrics design
## Or you can construct the conjoint design manually in in R
attribute_list <- list()
attribute_list[["myself"]] <- c("vaccinate","not vaccinate",
                                "wait and see")
attribute_list[["familiar"]] <-c("vaccinate","not vaccinate",
                                 "wait_and_see")
attribute_list[["society"]] <-  c("vaccinate","not vaccinate",
                                  "wait and see")

covid_design <- makeDesign(type='constraints', attribute.levels=attribute_list)

###----------estimating ACME

results_l <- amce(selected ~ familiar + myself + society, data=conjoint_data_l,
                cluster=TRUE, respondent.id="respondent",
                design=covid_design)

summary(results_l)


###--------visulizing ACME
windows(10,8)
plot(results_l, xlab="Willingness of Vacctination（Average Marginal Component-specific Effects）: LUCID",
     ylim=c(-.3,.3), text.size=13,group.order=c("myself","familiar","society"))


###-----------subset results------------###


###spouse###
#subsetting data
dfcj_spouse_l<-subset(conjoint_data_l, Q10=="spouse")

#estimating ACME

results_spouse_l <- amce(selected ~ familiar + myself + society, data=dfcj_spouse_l,
                       cluster=TRUE, respondent.id="respondent",
                       design=covid_design)

summary(results_spouse_l)

#plot
windows(10,8)
plot(results_spouse_l, xlab="spouse: Willingness of Vacctination（Average Marginal Component-specific Effects）",
     ylim=c(-.3,.3), text.size=13,group.order=c("myself","familiar","society"))


###older family###

#subsetting data
dfcj_older_familiy_l<-subset(conjoint_data_l, Q10=="older family")

#estimating ACME

results_older_familiy_l <- amce(selected ~ familiar + myself + society, 
                                data=dfcj_older_familiy_l,
                              cluster=TRUE, respondent.id="respondent",
                              design=covid_design)

summary(results_older_familiy_l)

#plot
windows(10,8)
plot(results_older_familiy_l, xlab="older familiy: Willingness of Vacctination（Average Marginal Component-specific Effects）",
     ylim=c(-.3,.3), text.size=13,group.order=c("myself","familiar","society"))




###younger family###

#subsetting data
dfcj_younger_familiy_l<-subset(conjoint_data_l, Q10=="younger family")

#estimating ACME

results_younger_familiy_l <- amce(selected ~ familiar + myself + society, 
                                data=dfcj_younger_familiy_l,
                                cluster=TRUE, respondent.id="respondent",
                                design=covid_design)

summary(results_younger_familiy_l)

#plot
windows(10,8)
plot(results_younger_familiy_l, xlab="younger familiy: Willingness of Vacctination（Average Marginal Component-specific Effects）",
     ylim=c(-.3,.3), text.size=13,group.order=c("myself","familiar","society"))



###friend###

#subsetting data
dfcj_friend_l<-subset(conjoint_data_l, Q10=="friend")

#estimating ACME

results_friend_l <- amce(selected ~ familiar + myself + society, data=dfcj_friend_l,
                       cluster=TRUE, respondent.id="respondent",
                       design=covid_design)

summary(results_friend_l)

#plot
windows(10,8)
plot(results_friend_l, xlab="friend: Willingness of Vacctination（Average Marginal Component-specific Effects）",
     ylim=c(-.3,.3), text.size=13,group.order=c("myself","familiar","society"))


###lover###

#subsetting data
dfcj_lover_l<-subset(conjoint_data_l, Q10=="lover")

#estimating ACME

results_lover_l <- amce(selected ~ familiar + myself + society, data=dfcj_lover_l,
                      cluster=TRUE, respondent.id="respondent",
                      design=covid_design)

summary(results_lover_l)

#plot
windows(10,8)
plot(results_lover_l, xlab="lover: Willingness of Vacctination（Average Marginal Component-specific Effects）",
     ylim=c(-.3,.3), text.size=13,group.order=c("myself","familiar","society"))



###colleague###
#subsetting data
dfcj_colleague_l<-subset(conjoint_data_l, Q10=="colleague")

#estimating ACME

results_colleague_l <- amce(selected ~ familiar + myself + society,
                            data=dfcj_colleague_l,
                          cluster=TRUE, respondent.id="respondent",
                          design=covid_design)

summary(results_colleague_l)

#plot
windows(10,8)
plot(results_colleague_l, xlab="colleague: Willingness of Vacctination（Average Marginal Component-specific Effects）",
     ylim=c(-.3,.3), text.size=13,group.order=c("myself","familiar","society"))



###neighbor###

#subsetting data
dfcj_neighbor_l<-subset(conjoint_data_l, Q10=="neighbor")

#estimating ACME

results_neighbor_l <- amce(selected ~ familiar + myself + society, 
                           data=dfcj_neighbor_l,
                         cluster=TRUE, respondent.id="respondent",
                         design=covid_design)

summary(results_neighbor_l)

#plot
windows(10,8)
plot(results_neighbor_l, xlab="neighbor: Willingness of Vacctination（Average Marginal Component-specific Effects）",
     ylim=c(-.3,.3), text.size=13,group.order=c("myself","familiar","society"))


###-------plot compiling AMCE by group--------###

##making table

#table: myself vaccinate

my_spouse_l<-c(results_spouse_l$estimates$myself[1,1],results_spouse_l$estimates$myself[2,1],
             results_spouse_l$estimates$myself[1,1]+results_spouse_l$estimates$myself[2,1]*2.576,
             results_spouse_l$estimates$myself[1,1]-results_spouse_l$estimates$myself[2,1]*2.576)

my_old_l<-c(results_older_familiy_l$estimates$myself[1,1],results_older_familiy_l$estimates$myself[2,1],
          results_older_familiy_l$estimates$myself[1,1]+results_older_familiy_l$estimates$myself[2,1]*2.576,
          results_older_familiy_l$estimates$myself[1,1]-results_older_familiy_l$estimates$myself[2,1]*2.576)

my_young_l<-c(results_younger_familiy_l$estimates$myself[1,1],results_younger_familiy_l$estimates$myself[2,1],
            results_younger_familiy_l$estimates$myself[1,1]+results_younger_familiy_l$estimates$myself[2,1]*2.576,
            results_younger_familiy_l$estimates$myself[1,1]-results_younger_familiy_l$estimates$myself[2,1]*2.576)


my_frined_l<-c(results_friend_l$estimates$myself[1,1],results_friend_l$estimates$myself[2,1],
             results_friend_l$estimates$myself[1,1]+results_friend_l$estimates$myself[2,1]*2.576,
             results_friend_l$estimates$myself[1,1]-results_friend_l$estimates$myself[2,1]*2.576)

my_lover_l<-c(results_lover_l$estimates$myself[1,1],results_lover_l$estimates$myself[2,1],
            results_lover_l$estimates$myself[1,1]+results_lover_l$estimates$myself[2,1]*2.576,
            results_lover_l$estimates$myself[1,1]-results_lover_l$estimates$myself[2,1]*2.576)


my_col_l<-c(results_colleague_l$estimates$myself[1,1],results_colleague_l$estimates$myself[2,1],
          results_colleague_l$estimates$myself[1,1]+results_colleague_l$estimates$myself[2,1]*2.576,
          results_colleague_l$estimates$myself[1,1]-results_colleague_l$estimates$myself[2,1]*2.576)

my_nei_l<-c(results_neighbor_l$estimates$myself[1,1],results_neighbor$estimates$myself[2,1],
          results_neighbor_l$estimates$myself[1,1]+results_neighbor$estimates$myself[2,1]*2.576,
          results_neighbor_l$estimates$myself[1,1]-results_neighbor$estimates$myself[2,1]*2.576)

#table: myself wait and see

my_spouseW_l<-c(results_spouse_l$estimates$myself[1,2],results_spouse_l$estimates$myself[2,2],
              results_spouse_l$estimates$myself[1,2]+results_spouse_l$estimates$myself[2,2]*2.576,
              results_spouse_l$estimates$myself[1,2]-results_spouse_l$estimates$myself[2,2]*2.576)

my_oldW_l<-c(results_older_familiy_l$estimates$myself[1,2],results_older_familiy_l$estimates$myself[2,2],
           results_older_familiy_l$estimates$myself[1,2]+results_older_familiy_l$estimates$myself[2,2]*2.576,
           results_older_familiy_l$estimates$myself[1,2]-results_older_familiy_l$estimates$myself[2,2]*2.576)

my_youngW_l<-c(results_younger_familiy_l$estimates$myself[1,2],results_younger_familiy_l$estimates$myself[2,2],
             results_younger_familiy_l$estimates$myself[1,2]+results_younger_familiy_l$estimates$myself[2,2]*2.576,
             results_younger_familiy_l$estimates$myself[1,2]-results_younger_familiy_l$estimates$myself[2,2]*2.576)


my_frinedW_l<-c(results_friend_l$estimates$myself[1,2],results_friend_l$estimates$myself[2,2],
              results_friend_l$estimates$myself[1,2]+results_friend_l$estimates$myself[2,2]*2.576,
              results_friend_l$estimates$myself[1,2]-results_friend_l$estimates$myself[2,2]*2.576)

my_loverW_l<-c(results_lover_l$estimates$myself[1,2],results_lover_l$estimates$myself[2,2],
             results_lover_l$estimates$myself[1,2]+results_lover_l$estimates$myself[2,2]*2.576,
             results_lover_l$estimates$myself[1,2]-results_lover_l$estimates$myself[2,2]*2.576)


my_colW_l<-c(results_colleague_l$estimates$myself[1,2],results_colleague_l$estimates$myself[2,2],
           results_colleague_l$estimates$myself[1,2]+results_colleague_l$estimates$myself[2,2]*2.576,
           results_colleague_l$estimates$myself[1,2]-results_colleague_l$estimates$myself[2,2]*2.576)

my_neiW_l<-c(results_neighbor_l$estimates$myself[1,2],results_neighbor_l$estimates$myself[2,2],
           results_neighbor_l$estimates$myself[1,2]+results_neighbor_l$estimates$myself[2,2]*2.576,
           results_neighbor_l$estimates$myself[1,2]-results_neighbor_l$estimates$myself[2,2]*2.576)



tb_myself_l<-rbind(my_spouse_l,my_spouseW_l, my_old_l,my_oldW_l, my_young_l,my_youngW_l,
                 my_frined_l,my_frinedW_l, my_lover_l, my_loverW_l,
                 my_col_l, my_colW_l, my_nei_l, my_neiW_l)



#table: familiar

fami_spouse_l<-c(results_spouse_l$estimates$familiar[1,1],results_spouse_l$estimates$familiar[2,1],
                 results_spouse_l$estimates$familiar[1,1]+results_spouse_l$estimates$familiar[2,1]*2.576,
                 results_spouse_l$estimates$familiar[1,1]-results_spouse_l$estimates$familiar[2,1]*2.576)

fami_old_l<-c(results_older_familiy_l$estimates$familiar[1,1],results_older_familiy_l$estimates$familiar[2,1],
            results_older_familiy_l$estimates$familiar[1,1]+results_older_familiy_l$estimates$familiar[2,1]*2.576,
            results_older_familiy_l$estimates$familiar[1,1]-results_older_familiy_l$estimates$familiar[2,1]*2.576)

fami_young_l<-c(results_younger_familiy_l$estimates$familiar[1,1],results_younger_familiy_l$estimates$familiar[2,1],
                results_younger_familiy_l$estimates$familiar[1,1]+results_younger_familiy_l$estimates$familiar[2,1]*2.576,
                results_younger_familiy_l$estimates$familiar[1,1]-results_younger_familiy_l$estimates$familiar[2,1]*2.576)


fami_frined_l<-c(results_friend_l$estimates$familiar[1,1],results_friend_l$estimates$familiar[2,1],
                 results_friend_l$estimates$familiar[1,1]+results_friend_l$estimates$familiar[2,1]*2.576,
                 results_friend_l$estimates$familiar[1,1]-results_friend_l$estimates$familiar[2,1]*2.576)

fami_lover_l<-c(results_lover_l$estimates$familiar[1,1],results_lover_l$estimates$familiar[2,1],
                results_lover_l$estimates$familiar[1,1]+results_lover_l$estimates$familiar[2,1]*2.576,
                results_lover_l$estimates$familiar[1,1]-results_lover_l$estimates$familiar[2,1]*2.576)


fami_col_l<-c(results_colleague_l$estimates$familiar[1,1],results_colleague_l$estimates$familiar[2,1],
              results_colleague_l$estimates$familiar[1,1]+results_colleague_l$estimates$familiar[2,1]*2.576,
              results_colleague_l$estimates$familiar[1,1]-results_colleague_l$estimates$familiar[2,1]*2.576)

fami_nei_l<-c(results_neighbor_l$estimates$familiar[1,1],results_neighbor_l$estimates$familiar[2,1],
              results_neighbor_l$estimates$familiar[1,1]+results_neighbor_l$estimates$familiar[2,1]*2.576,
              results_neighbor_l$estimates$familiar[1,1]-results_neighbor_l$estimates$familiar[2,1]*2.576)






#table: familiar wait and see


fami_spouseW_l<-c(results_spouse_l$estimates$familiar[1,2],results_spouse_l$estimates$familiar[2,2],
                results_spouse_l$estimates$familiar[1,2]+results_spouse_l$estimates$familiar[2,2]*2.576,
                results_spouse_l$estimates$familiar[1,2]-results_spouse_l$estimates$familiar[2,2]*2.576)

fami_oldW_l<-c(results_older_familiy_l$estimates$familiar[1,2],results_older_familiy_l$estimates$familiar[2,2],
             results_older_familiy_l$estimates$familiar[1,2]+results_older_familiy_l$estimates$familiar[2,2]*2.576,
             results_older_familiy_l$estimates$familiar[1,2]-results_older_familiy_l$estimates$familiar[2,2]*2.576)

fami_youngW_l<-c(results_younger_familiy_l$estimates$familiar[1,2],results_younger_familiy_l$estimates$familiar[2,2],
               results_younger_familiy_l$estimates$familiar[1,2]+results_younger_familiy_l$estimates$familiar[2,2]*2.576,
               results_younger_familiy_l$estimates$familiar[1,2]-results_younger_familiy_l$estimates$familiar[2,2]*2.576)


fami_frinedW_l<-c(results_friend_l$estimates$familiar[1,2],results_friend_l$estimates$familiar[2,2],
                results_friend_l$estimates$familiar[1,2]+results_friend_l$estimates$familiar[2,2]*2.576,
                results_friend_l$estimates$familiar[1,2]-results_friend_l$estimates$familiar[2,2]*2.576)

fami_loverW_l<-c(results_lover_l$estimates$familiar[1,2],results_lover_l$estimates$familiar[2,2],
               results_lover_l$estimates$familiar[1,2]+results_lover_l$estimates$familiar[2,2]*2.576,
               results_lover_l$estimates$familiar[1,2]-results_lover_l$estimates$familiar[2,2]*2.576)


fami_colW_l<-c(results_colleague_l$estimates$familiar[1,2],results_colleague_l$estimates$familiar[2,2],
             results_colleague_l$estimates$familiar[1,2]+results_colleague_l$estimates$familiar[2,2]*2.576,
             results_colleague_l$estimates$familiar[1,2]-results_colleague_l$estimates$familiar[2,2]*2.576)

fami_neiW_l<-c(results_neighbor_l$estimates$familiar[1,2],results_neighbor_l$estimates$familiar[2,2],
             results_neighbor_l$estimates$familiar[1,2]+results_neighbor_l$estimates$familiar[2,2]*2.576,
             results_neighbor_l$estimates$familiar[1,2]-results_neighbor_l$estimates$familiar[2,2]*2.576)

tb_familiar_l<-rbind(fami_spouse_l,fami_spouseW_l,fami_old_l,fami_oldW_l,
                     fami_young_l,fami_youngW_l,
                   fami_frined_l,fami_frinedW_l,fami_lover_l,fami_loverW_l,
                   fami_col_l,fami_colW_l,fami_nei_l,fami_neiW_l)

#table: society
soc_spouse_l<-c(results_spouse_l$estimates$society[1,1],results_spouse_l$estimates$society[2,1],
                results_spouse_l$estimates$society[1,1]+results_spouse_l$estimates$society[2,1]*2.576,
                results_spouse_l$estimates$society[1,1]-results_spouse_l$estimates$society[2,1]*2.576)

soc_old_l<-c(results_older_familiy_l$estimates$society[1,1],results_older_familiy_l$estimates$society[2,1],
             results_older_familiy_l$estimates$society[1,1]+results_older_familiy_l$estimates$society[2,1]*2.576,
             results_older_familiy_l$estimates$society[1,1]-results_older_familiy_l$estimates$society[2,1]*2.576)

soc_young_l<-c(results_younger_familiy_l$estimates$society[1,1],results_younger_familiy_l$estimates$society[2,1],
               results_younger_familiy_l$estimates$society[1,1]+results_younger_familiy_l$estimates$society[2,1]*2.576,
               results_younger_familiy_l$estimates$society[1,1]-results_younger_familiy_l$estimates$society[2,1]*2.576)


soc_frined_l<-c(results_friend_l$estimates$society[1,1],results_friend_l$estimates$society[2,1],
                results_friend_l$estimates$society[1,1]+results_friend_l$estimates$society[2,1]*2.576,
                results_friend_l$estimates$society[1,1]-results_friend_l$estimates$society[2,1]*2.576)

soc_lover_l<-c(results_lover_l$estimates$society[1,1],results_lover_l$estimates$society[2,1],
               results_lover_l$estimates$society[1,1]+results_lover_l$estimates$society[2,1]*2.576,
               results_lover_l$estimates$society[1,1]-results_lover_l$estimates$society[2,1]*2.576)


soc_col_l<-c(results_colleague$estimates$society[1,1],results_colleague_l$estimates$society[2,1],
           results_colleague$estimates$society[1,1]+results_colleague_l$estimates$society[2,1]*2.576,
           results_colleague$estimates$society[1,1]-results_colleague_l$estimates$society[2,1]*2.576)

soc_nei_l<-c(results_neighbor_l$estimates$society[1,1],results_neighbor_l$estimates$society[2,1],
             results_neighbor_l$estimates$society[1,1]+results_neighbor_l$estimates$society[2,1]*2.576,
             results_neighbor_l$estimates$society[1,1]-results_neighbor_l$estimates$society[2,1]*2.576)

#table: society wait and see
soc_spouseW_l<-c(results_spouse_l$estimates$society[1,2],results_spouse_l$estimates$society[2,2],
               results_spouse_l$estimates$society[1,2]+results_spouse_l$estimates$society[2,2]*2.576,
               results_spouse_l$estimates$society[1,2]-results_spouse_l$estimates$society[2,2]*2.576)

soc_oldW_l<-c(results_older_familiy_l$estimates$society[1,2],results_older_familiy_l$estimates$society[2,2],
            results_older_familiy_l$estimates$society[1,2]+results_older_familiy_l$estimates$society[2,2]*2.576,
            results_older_familiy_l$estimates$society[1,2]-results_older_familiy_l$estimates$society[2,2]*2.576)

soc_youngW_l<-c(results_younger_familiy_l$estimates$society[1,2],results_younger_familiy_l$estimates$society[2,2],
              results_younger_familiy_l$estimates$society[1,2]+results_younger_familiy_l$estimates$society[2,2]*2.576,
              results_younger_familiy_l$estimates$society[1,2]-results_younger_familiy_l$estimates$society[2,2]*2.576)


soc_frinedW_l<-c(results_friend_l$estimates$society[1,2],results_friend_l$estimates$society[2,2],
               results_friend_l$estimates$society[1,2]+results_friend_l$estimates$society[2,2]*2.576,
               results_friend_l$estimates$society[1,2]-results_friend_l$estimates$society[2,2]*2.576)

soc_loverW_l<-c(results_lover_l$estimates$society[1,2],results_lover_l$estimates$society[2,2],
              results_lover_l$estimates$society[1,2]+results_lover_l$estimates$society[2,2]*2.576,
              results_lover_l$estimates$society[1,2]-results_lover_l$estimates$society[2,2]*2.576)


soc_colW_l<-c(results_colleague$estimates$society[1,2],results_colleague_l$estimates$society[2,2],
            results_colleague$estimates$society[1,2]+results_colleague_l$estimates$society[2,2]*2.576,
            results_colleague$estimates$society[1,2]-results_colleague_l$estimates$society[2,2]*2.576)

soc_neiW_l<-c(results_neighbor_l$estimates$society[1,2],results_neighbor_l$estimates$society[2,2],
            results_neighbor_l$estimates$society[1,2]+results_neighbor_l$estimates$society[2,2]*2.576,
            results_neighbor_l$estimates$society[1,2]-results_neighbor_l$estimates$society[2,2]*2.576)


tb_society_l<-rbind(soc_spouse_l,soc_spouseW_l,soc_old_l,soc_oldW_l,soc_young_l,soc_youngW_l,
                  soc_frined_l,soc_frinedW_l,soc_lover_l,soc_loverW_l,soc_col_l,soc_colW_l,
                  soc_nei_l,soc_neiW_l)


#unified and output
tb_all_l<-rbind(tb_myself_l,tb_familiar_l,tb_society_l)
write.xlsx(tb_all_l,"results_table_lucid.csv")
#In excel file, "group_fam" variable is added and "var" variable is modified. 
#"results_table_forFigure.csv" is reproduced and reload as the below.


#load csv file operated with Excel
df_figGroup_l<-read.csv("results_table_forFigure_lucid.csv")
attach(df_figGroup_l)


#plot with ggplot2

#set dodge
pd <-position_dodge(0.4)

#set levels order
df_figGroup_l$var <- factor(df_figGroup_l$var,
                          levels = c("Society:w&s", "Society:vaccinate",
                                     "Familiar:w&s","Familiar:vaccinate", 
                                     "Myself:w&s","Myself:vaccinate" ))


#plot
pl = ggplot(df_figGroup_l,aes(y=pe,x=var,colour=group_fam))
pl = pl + coord_flip(ylim = c(-.2, .4))
pl=pl+geom_errorbar(aes(ymin=lower, ymax=upper),
                  position=pd, width=.1,na.rm=T) 
pl=pl+geom_point(position=pd,size=3, shape=21, fill="white")+
  labs(y="Willingness of Vacctination（Average Marginal Component-specific Effects) by group\n
       All Base Lines: Not Vaccinate" )+
  labs(x="")
pl=pl+scale_color_discrete(name = "Familiar entity")
Pl=theme_bw1()
pl=pl+ggtitle("AMCE sorted by the selection of familiar entity (Lucid)")
print(pl)

pl = pl + geom_line(yintercept = 0,size=.5,linetype="dotted",position=pd) 
pl = pl +geom_point(aes(ymin=lower,ymax=upper,width=.4),position=pd,size=.6)
pl = pl + scale_y_continuous(breaks=round(seq(-.4,.4,.2),1),labels=c("-.4","-.2","0",".2",".4"))
pl = pl + scale_x_discrete(name="") + scale_colour_discrete("")


pdf(p,"fig5.pdf",width=10,height=12.5) 
print(p)
dev.off()
