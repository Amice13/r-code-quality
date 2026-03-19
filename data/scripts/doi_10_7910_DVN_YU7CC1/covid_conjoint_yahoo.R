##########################
# conjoint for COVID-19  #
# yahoo crowd            #
##########################



###-----load package
library(cjoint)
library(tidyverse)
library(openxlsx)
library(ggplot2)
library(psych)


###-------load data

covid<-read.csv("covid19_normal.csv")
attach(covid)

conjoint_data <- read.qualtrics("covid19.csv",
                                     responses=c("Q20.1","Q20.2", "Q20.3","Q20.4",
                                                 "Q20.5"),
                                covariates = c("Q19.1","Q2.1","Q2.2","Q2.4"),
                                      respondentID="ID",new.format = F)

conjoint_data<-na.omit(conjoint_data)
write.xlsx(conjoint_data,"covid_cj.csv")

cd<-read.csv("covid_cj.csv")
attach(cd)


###-----simple descriptive analysis------###

#willingness of vaccination
Q21.1[Q21.1==4]<-NA
Q21.1[Q21.1==5]<-NA
prop.table(table(covid$Q21.1))

##reasons for wait and see
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
tb_bar1$ws<-as.numeric(ws)

#plot
bar1<-ggplot(data=tb_bar1, aes(x=ws0,y=ws)) +
  geom_bar(stat="identity")+
  coord_flip()+
  ylab("Proportion")+
  xlab("Reasons")+
  ggtitle("Reasons for Wait and See (YSC)")
bar1


##reasons for not vaccination
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
tb_bar2$ws2<-as.numeric(ws2)

#plot
bar2<-ggplot(data=tb_bar2, aes(x=ws02,y=ws2)) +
  geom_bar(stat="identity")+
  coord_flip()+
  ylab("Proportion")+
  xlab("Reasons")+
  ggtitle("Reasons for no vaccination (YSC)")
bar2



###--------familiar entity--------###

#set variables

famil<-prop.table(table(Q19.1))
famil<-famil[-1]
famil_lab<-c("colleague","friend","lover","neighbor",
        "older family","spouse","younger family")

#set data frame for table
tb_famil<-data.frame(cbind(famil_lab,famil))

#set levels order
tb_famil$famil_lab <- factor(tb_famil$famil_lab,levels = c("neighbor","colleague","lover","friend",
                                                           "younger family","older family","spouse"))
tb_famil$famil<-as.numeric(famil)

#plot
famil_bar<-ggplot(data=tb_famil, aes(x=famil_lab,y=famil)) +
  geom_bar(stat="identity")+
  coord_flip()+
  ylab("Proportion")+
  xlab("")+
  ggtitle("Familiar Entity (YSC)")
famil_bar



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

results <- amce(selected ~ familiar + myself + society, data=conjoint_data,
                cluster=TRUE, respondent.id="respondent",
                  design=covid_design)

summary(results)


###--------visulizing ACME
windows(10,8)
plot(results, xlab="Willingness of Vacctination（Average Marginal Component-specific Effects）",
     ylim=c(-.3,.3), text.size=13,group.order=c("myself","familiar","society"))


###-----------subset results------------###


###spouse###
#subsetting data
dfcj_spouse<-subset(conjoint_data, Q2.2=="spouse")

#estimating ACME

results_spouse <- amce(selected ~ familiar + myself + society, data=dfcj_spouse,
                       cluster=TRUE, respondent.id="respondent",
                       design=covid_design)

summary(results_spouse)

#plot
windows(10,8)
plot(results_spouse, xlab="spouse: Willingness of Vacctination（Average Marginal Component-specific Effects）",
     ylim=c(-.3,.3), text.size=13,group.order=c("myself","familiar","society"))


###older family###

#subsetting data
dfcj_older_familiy<-subset(conjoint_data, Q2.2=="older familiar")

#estimating ACME

results_older_familiy <- amce(selected ~ familiar + myself + society, data=dfcj_older_familiy,
                              cluster=TRUE, respondent.id="respondent",
                              design=covid_design)

summary(results_older_familiy)

#plot
windows(10,8)
plot(results_older_familiy, xlab="older familiy: Willingness of Vacctination（Average Marginal Component-specific Effects）",
     ylim=c(-.3,.3), text.size=13,group.order=c("myself","familiar","society"))




###younger family###

#subsetting data
dfcj_younger_familiy<-subset(conjoint_data, Q2.2=="younger family")

#estimating ACME

results_younger_familiy <- amce(selected ~ familiar + myself + society, data=dfcj_younger_familiy,
                                cluster=TRUE, respondent.id="respondent",
                                design=covid_design)

summary(results_younger_familiy)

#plot
windows(10,8)
plot(results_younger_familiy, xlab="younger familiy: Willingness of Vacctination（Average Marginal Component-specific Effects）",
     ylim=c(-.3,.3), text.size=13,group.order=c("myself","familiar","society"))



###friend###

#subsetting data
dfcj_friend<-subset(conjoint_data, Q2.2=="friend")

#estimating ACME

results_friend <- amce(selected ~ familiar + myself + society, data=dfcj_friend,
                       cluster=TRUE, respondent.id="respondent",
                       design=covid_design)

summary(results_friend)

#plot
windows(10,8)
plot(results_friend, xlab="friend: Willingness of Vacctination（Average Marginal Component-specific Effects）",
     ylim=c(-.3,.3), text.size=13,group.order=c("myself","familiar","society"))


###lover###

#subsetting data
dfcj_lover<-subset(conjoint_data, Q2.2=="lover")

#estimating ACME

results_lover <- amce(selected ~ familiar + myself + society, data=dfcj_lover,
                      cluster=TRUE, respondent.id="respondent",
                      design=covid_design)

summary(results_lover)

#plot
windows(10,8)
plot(results_lover, xlab="lover: Willingness of Vacctination（Average Marginal Component-specific Effects）",
     ylim=c(-.3,.3), text.size=13,group.order=c("myself","familiar","society"))



###colleague###
#subsetting data
dfcj_colleague<-subset(conjoint_data, Q2.2=="colleague")

#estimating ACME

results_colleague <- amce(selected ~ familiar + myself + society, data=dfcj_colleague,
                cluster=TRUE, respondent.id="respondent",
                design=covid_design)

summary(results_colleague)

#plot
windows(10,8)
plot(results_colleague, xlab="colleague: Willingness of Vacctination（Average Marginal Component-specific Effects）",
     ylim=c(-.3,.3), text.size=13,group.order=c("myself","familiar","society"))



###neighbor###

#subsetting data
dfcj_neighbor<-subset(conjoint_data, Q2.2=="neighbor")

#estimating ACME

results_neighbor <- amce(selected ~ familiar + myself + society, data=dfcj_neighbor,
                         cluster=TRUE, respondent.id="respondent",
                         design=covid_design)

summary(results_neighbor)

#plot
windows(10,8)
plot(results_neighbor, xlab="neighbor: Willingness of Vacctination（Average Marginal Component-specific Effects）",
     ylim=c(-.3,.3), text.size=13,group.order=c("myself","familiar","society"))


###-------plot compiling AMCE by group--------###

##making table

#table: myself vaccinate

my_spouse<-c(results_spouse$estimates$myself[1,1],results_spouse$estimates$myself[2,1],
results_spouse$estimates$myself[1,1]+results_spouse$estimates$myself[2,1]*2.576,
results_spouse$estimates$myself[1,1]-results_spouse$estimates$myself[2,1]*2.576)

my_old<-c(results_older_familiy$estimates$myself[1,1],results_older_familiy$estimates$myself[2,1],
  results_older_familiy$estimates$myself[1,1]+results_older_familiy$estimates$myself[2,1]*2.576,
  results_older_familiy$estimates$myself[1,1]-results_older_familiy$estimates$myself[2,1]*2.576)

my_young<-c(results_younger_familiy$estimates$myself[1,1],results_younger_familiy$estimates$myself[2,1],
  results_younger_familiy$estimates$myself[1,1]+results_younger_familiy$estimates$myself[2,1]*2.576,
  results_younger_familiy$estimates$myself[1,1]-results_younger_familiy$estimates$myself[2,1]*2.576)


my_frined<-c(results_friend$estimates$myself[1,1],results_friend$estimates$myself[2,1],
  results_friend$estimates$myself[1,1]+results_friend$estimates$myself[2,1]*2.576,
  results_friend$estimates$myself[1,1]-results_friend$estimates$myself[2,1]*2.576)

my_lover<-c(results_lover$estimates$myself[1,1],results_lover$estimates$myself[2,1],
  results_lover$estimates$myself[1,1]+results_lover$estimates$myself[2,1]*2.576,
  results_lover$estimates$myself[1,1]-results_lover$estimates$myself[2,1]*2.576)


my_col<-c(results_colleague$estimates$myself[1,1],results_colleague$estimates$myself[2,1],
  results_colleague$estimates$myself[1,1]+results_colleague$estimates$myself[2,1]*2.576,
  results_colleague$estimates$myself[1,1]-results_colleague$estimates$myself[2,1]*2.576)

my_nei<-c(results_neighbor$estimates$myself[1,1],results_neighbor$estimates$myself[2,1],
  results_neighbor$estimates$myself[1,1]+results_neighbor$estimates$myself[2,1]*2.576,
  results_neighbor$estimates$myself[1,1]-results_neighbor$estimates$myself[2,1]*2.576)

#table: myself wait and see

my_spouseW<-c(results_spouse$estimates$myself[1,2],results_spouse$estimates$myself[2,2],
              results_spouse$estimates$myself[1,2]+results_spouse$estimates$myself[2,2]*2.576,
              results_spouse$estimates$myself[1,2]-results_spouse$estimates$myself[2,2]*2.576)

my_oldW<-c(results_older_familiy$estimates$myself[1,2],results_older_familiy$estimates$myself[2,2],
           results_older_familiy$estimates$myself[1,2]+results_older_familiy$estimates$myself[2,2]*2.576,
           results_older_familiy$estimates$myself[1,2]-results_older_familiy$estimates$myself[2,2]*2.576)

my_youngW<-c(results_younger_familiy$estimates$myself[1,2],results_younger_familiy$estimates$myself[2,2],
             results_younger_familiy$estimates$myself[1,2]+results_younger_familiy$estimates$myself[2,2]*2.576,
             results_younger_familiy$estimates$myself[1,2]-results_younger_familiy$estimates$myself[2,2]*2.576)


my_frinedW<-c(results_friend$estimates$myself[1,2],results_friend$estimates$myself[2,2],
              results_friend$estimates$myself[1,2]+results_friend$estimates$myself[2,2]*2.576,
              results_friend$estimates$myself[1,2]-results_friend$estimates$myself[2,2]*2.576)

my_loverW<-c(results_lover$estimates$myself[1,2],results_lover$estimates$myself[2,2],
             results_lover$estimates$myself[1,2]+results_lover$estimates$myself[2,2]*2.576,
             results_lover$estimates$myself[1,2]-results_lover$estimates$myself[2,2]*2.576)


my_colW<-c(results_colleague$estimates$myself[1,2],results_colleague$estimates$myself[2,2],
           results_colleague$estimates$myself[1,2]+results_colleague$estimates$myself[2,2]*2.576,
           results_colleague$estimates$myself[1,2]-results_colleague$estimates$myself[2,2]*2.576)

my_neiW<-c(results_neighbor$estimates$myself[1,2],results_neighbor$estimates$myself[2,2],
           results_neighbor$estimates$myself[1,2]+results_neighbor$estimates$myself[2,2]*2.576,
           results_neighbor$estimates$myself[1,2]-results_neighbor$estimates$myself[2,2]*2.576)



tb_myself<-rbind(my_spouse,my_spouseW, my_old,my_oldW, my_young,my_youngW,
                 my_frined,my_frinedW, my_lover, my_loverW,
                 my_col, my_colW, my_nei, my_neiW)


#table: familiar 

fami_spouse<-c(results_spouse$estimates$familiar[1,1],results_spouse$estimates$familiar[2,2],
               results_spouse$estimates$familiar[1,1]+results_spouse$estimates$familiar[2,2]*2.576,
               results_spouse$estimates$familiar[1,1]-results_spouse$estimates$familiar[2,2]*2.576)

fami_old<-c(results_older_familiy$estimates$familiar[1,1],results_older_familiy$estimates$familiar[2,2],
            results_older_familiy$estimates$familiar[1,1]+results_older_familiy$estimates$familiar[2,2]*2.576,
            results_older_familiy$estimates$familiar[1,1]-results_older_familiy$estimates$familiar[2,2]*2.576)

fami_young<-c(results_younger_familiy$estimates$familiar[1,1],results_younger_familiy$estimates$familiar[2,2],
              results_younger_familiy$estimates$familiar[1,1]+results_younger_familiy$estimates$familiar[2,2]*2.576,
              results_younger_familiy$estimates$familiar[1,1]-results_younger_familiy$estimates$familiar[2,2]*2.576)


fami_frined<-c(results_friend$estimates$familiar[1,1],results_friend$estimates$familiar[2,2],
               results_friend$estimates$familiar[1,1]+results_friend$estimates$familiar[2,2]*2.576,
               results_friend$estimates$familiar[1,1]-results_friend$estimates$familiar[2,2]*2.576)

fami_lover<-c(results_lover$estimates$familiar[1,1],results_lover$estimates$familiar[2,2],
              results_lover$estimates$familiar[1,1]+results_lover$estimates$familiar[2,2]*2.576,
              results_lover$estimates$familiar[1,1]-results_lover$estimates$familiar[2,2]*2.576)


fami_col<-c(results_colleague$estimates$familiar[1,1],results_colleague$estimates$familiar[2,2],
            results_colleague$estimates$familiar[1,1]+results_colleague$estimates$familiar[2,2]*2.576,
            results_colleague$estimates$familiar[1,1]-results_colleague$estimates$familiar[2,2]*2.576)

fami_nei<-c(results_neighbor$estimates$familiar[1,1],results_neighbor$estimates$familiar[2,2],
            results_neighbor$estimates$familiar[1,1]+results_neighbor$estimates$familiar[2,2]*2.576,
            results_neighbor$estimates$familiar[1,1]-results_neighbor$estimates$familiar[2,2]*2.576)

#table: familiar wait and see

fami_spouseW<-c(results_spouse$estimates$familiar[1,2],results_spouse$estimates$familiar[2,2],
                results_spouse$estimates$familiar[1,2]+results_spouse$estimates$familiar[2,2]*2.576,
                results_spouse$estimates$familiar[1,2]-results_spouse$estimates$familiar[2,2]*2.576)

fami_oldW<-c(results_older_familiy$estimates$familiar[1,2],results_older_familiy$estimates$familiar[2,2],
             results_older_familiy$estimates$familiar[1,2]+results_older_familiy$estimates$familiar[2,2]*2.576,
             results_older_familiy$estimates$familiar[1,2]-results_older_familiy$estimates$familiar[2,2]*2.576)

fami_youngW<-c(results_younger_familiy$estimates$familiar[1,2],results_younger_familiy$estimates$familiar[2,2],
               results_younger_familiy$estimates$familiar[1,2]+results_younger_familiy$estimates$familiar[2,2]*2.576,
               results_younger_familiy$estimates$familiar[1,2]-results_younger_familiy$estimates$familiar[2,2]*2.576)


fami_frinedW<-c(results_friend$estimates$familiar[1,2],results_friend$estimates$familiar[2,2],
                results_friend$estimates$familiar[1,2]+results_friend$estimates$familiar[2,2]*2.576,
                results_friend$estimates$familiar[1,2]-results_friend$estimates$familiar[2,2]*2.576)

fami_loverW<-c(results_lover$estimates$familiar[1,2],results_lover$estimates$familiar[2,2],
               results_lover$estimates$familiar[1,2]+results_lover$estimates$familiar[2,2]*2.576,
               results_lover$estimates$familiar[1,2]-results_lover$estimates$familiar[2,2]*2.576)


fami_colW<-c(results_colleague$estimates$familiar[1,2],results_colleague$estimates$familiar[2,2],
             results_colleague$estimates$familiar[1,2]+results_colleague$estimates$familiar[2,2]*2.576,
             results_colleague$estimates$familiar[1,2]-results_colleague$estimates$familiar[2,2]*2.576)

fami_neiW<-c(results_neighbor$estimates$familiar[1,2],results_neighbor$estimates$familiar[2,2],
             results_neighbor$estimates$familiar[1,2]+results_neighbor$estimates$familiar[2,2]*2.576,
             results_neighbor$estimates$familiar[1,2]-results_neighbor$estimates$familiar[2,2]*2.576)

tb_familiar<-rbind(fami_spouse,fami_spouseW,fami_old,fami_oldW,fami_young,fami_youngW,
                   fami_frined,fami_frinedW,fami_lover,fami_loverW,
                   fami_col,fami_colW,fami_nei,fami_neiW)




#table: society
soc_spouse<-c(results_spouse$estimates$society[1,1],results_spouse$estimates$society[2,1],
              results_spouse$estimates$society[1,1]+results_spouse$estimates$society[2,1]*2.576,
              results_spouse$estimates$society[1,1]-results_spouse$estimates$society[2,1]*2.576)

soc_old<-c(results_older_familiy$estimates$society[1,1],results_older_familiy$estimates$society[2,1],
           results_older_familiy$estimates$society[1,1]+results_older_familiy$estimates$society[2,1]*2.576,
           results_older_familiy$estimates$society[1,1]-results_older_familiy$estimates$society[2,1]*2.576)

soc_young<-c(results_younger_familiy$estimates$society[1,1],results_younger_familiy$estimates$society[2,1],
             results_younger_familiy$estimates$society[1,1]+results_younger_familiy$estimates$society[2,1]*2.576,
             results_younger_familiy$estimates$society[1,1]-results_younger_familiy$estimates$society[2,1]*2.576)


soc_frined<-c(results_friend$estimates$society[1,1],results_friend$estimates$society[2,1],
              results_friend$estimates$society[1,1]+results_friend$estimates$society[2,1]*2.576,
              results_friend$estimates$society[1,1]-results_friend$estimates$society[2,1]*2.576)

soc_lover<-c(results_lover$estimates$society[1,1],results_lover$estimates$society[2,1],
             results_lover$estimates$society[1,1]+results_lover$estimates$society[2,1]*2.576,
             results_lover$estimates$society[1,1]-results_lover$estimates$society[2,1]*2.576)


soc_col<-c(results_colleague$estimates$society[1,1],results_colleague$estimates$society[2,1],
           results_colleague$estimates$society[1,1]+results_colleague$estimates$society[2,1]*2.576,
           results_colleague$estimates$society[1,1]-results_colleague$estimates$society[2,1]*2.576)

soc_nei<-c(results_neighbor$estimates$society[1,1],results_neighbor$estimates$society[2,1],
           results_neighbor$estimates$society[1,1]+results_neighbor$estimates$society[2,1]*2.576,
           results_neighbor$estimates$society[1,1]-results_neighbor$estimates$society[2,1]*2.576)




#table: society wait and see
soc_spouseW<-c(results_spouse$estimates$society[1,2],results_spouse$estimates$society[2,2],
               results_spouse$estimates$society[1,2]+results_spouse$estimates$society[2,2]*2.576,
               results_spouse$estimates$society[1,2]-results_spouse$estimates$society[2,2]*2.576)

soc_oldW<-c(results_older_familiy$estimates$society[1,2],results_older_familiy$estimates$society[2,2],
            results_older_familiy$estimates$society[1,2]+results_older_familiy$estimates$society[2,2]*2.576,
            results_older_familiy$estimates$society[1,2]-results_older_familiy$estimates$society[2,2]*2.576)

soc_youngW<-c(results_younger_familiy$estimates$society[1,2],results_younger_familiy$estimates$society[2,2],
              results_younger_familiy$estimates$society[1,2]+results_younger_familiy$estimates$society[2,2]*2.576,
              results_younger_familiy$estimates$society[1,2]-results_younger_familiy$estimates$society[2,2]*2.576)


soc_frinedW<-c(results_friend$estimates$society[1,2],results_friend$estimates$society[2,2],
               results_friend$estimates$society[1,2]+results_friend$estimates$society[2,2]*2.576,
               results_friend$estimates$society[1,2]-results_friend$estimates$society[2,2]*2.576)

soc_loverW<-c(results_lover$estimates$society[1,2],results_lover$estimates$society[2,2],
              results_lover$estimates$society[1,2]+results_lover$estimates$society[2,2]*2.576,
              results_lover$estimates$society[1,2]-results_lover$estimates$society[2,2]*2.576)


soc_colW<-c(results_colleague$estimates$society[1,2],results_colleague$estimates$society[2,2],
            results_colleague$estimates$society[1,2]+results_colleague$estimates$society[2,2]*2.576,
            results_colleague$estimates$society[1,2]-results_colleague$estimates$society[2,2]*2.576)

soc_neiW<-c(results_neighbor$estimates$society[1,2],results_neighbor$estimates$society[2,2],
            results_neighbor$estimates$society[1,2]+results_neighbor$estimates$society[2,2]*2.576,
            results_neighbor$estimates$society[1,2]-results_neighbor$estimates$society[2,2]*2.576)


tb_society<-rbind(soc_spouse,soc_spouseW,soc_old,soc_oldW,soc_young,soc_youngW,
                  soc_frined,soc_frinedW,soc_lover,soc_loverW,soc_col,soc_colW,
                  soc_nei,soc_neiW)


#unified and output
tb_all<-rbind(tb_myself,tb_familiar,tb_society)
write.xlsx(tb_all,"results_table.csv")
#In excel file, "group_fam" variable is added and "var" variable is modified. 
#"results_table_forFigure.csv" is reproduced and reload as the below.


#load csv file operated with Excel
df_figGroup<-read.csv("results_table_forFigure.csv")
attach(df_figGroup)


#plot with ggplot2

#set dodge
pd <-position_dodge(0.4)

#set levels order
df_figGroup$var <- factor(df_figGroup$var,
                       levels = c("Society:w&s", "Society:vaccinate",
                                  "Familiar:w&s","Familiar:vaccinate", 
                                  "Myself:w&s","Myself:vaccinate" ))

theme_bw1 <- function(base_size = 13, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.text.x =       element_text(size = base_size, colour = "black",  hjust = .5 , vjust=1),
      axis.text.y =       element_text(size = base_size , colour = "black", hjust = 0 , vjust=.5 ), # changes position of X axis text
      axis.ticks =        element_line(colour = "grey50"),
      axis.title.y =      element_text(size = base_size,angle=90,vjust=.01,hjust=.1),
      legend.position = "none"
    )
}



#plot
p = ggplot(df_figGroup,aes(y=pe,x=var,colour=group_fam))
p = p + coord_flip(ylim = c(-.2, .4))
p=p+geom_errorbar(aes(ymin=lower, ymax=upper),
              position=pd, width=.1,na.rm=T) 
p=p+geom_point(position=pd,size=3, shape=21, fill="white")+
  labs(y="Willingness of Vacctination（Average Marginal Component-specific Effects) by group\n
       All Base Lines: Not Vaccinate" )+
  labs(x="")
p=p+scale_color_discrete(name = "Familiar entity")
P=theme_bw1()
p=p+ggtitle("AMCE sorted by the selection of familiar entity (YCS)")


p = p + geom_line(yintercept = 0,size=.5,linetype="dotted",position=pd) 
p = p +geom_point(aes(ymin=lower,ymax=upper,width=.4),position=pd,size=.6)
p = p + scale_y_continuous(breaks=round(seq(-.4,.4,.2),1),labels=c("-.4","-.2","0",".2",".4"))
p = p + scale_x_discrete(name="") + scale_colour_discrete("")
print(p)

pdf(p,"fig5.pdf",width=10,height=12.5) 
print(p)
dev.off()



###-----------subset results------------###


###gender###
#subsetting data
dfcj_male<-na.omit(subset(conjoint_data, Q19.1==2))

#estimating ACME

results_male <- amce(selected ~ familiar + myself + society, data=dfcj_male,
                       cluster=TRUE, respondent.id="respondent",
                       design=covid_design)

summary(results_male)

#plot
windows(10,8)
plot_male<-plot(results_male, xlab="Male: Willingness of Vacctination（Average Marginal Component-specific Effects）",
     ylim=c(-.3,.3), text.size=13,group.order=c("myself","familiar","society"))

#subsetting data
dfcj_female<-na.omit(subset(conjoint_data, Q19.1==1))

#estimating ACME

results_female <- amce(selected ~ familiar + myself + society, data=dfcj_female,
                     cluster=TRUE, respondent.id="respondent",
                     design=covid_design)

summary(results_female)

#plot
windows(10,8)
plot_female<-plot(results_female, xlab="Female: Willingness of Vacctination（Average Marginal Component-specific Effects）",
     ylim=c(-.3,.3), text.size=13,group.order=c("myself","familiar","society"))


###---------arrange above two figures

windows(width = 24, height = 10)
img<-grid.arrange(plot_female,plot_male, ncol = 2, nrow = 1)
print(img)
ggsave(file = "fig_conjoint_sex.pdf", plot = img)



