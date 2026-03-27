###############################
# COVID-19 conjoint analysis  #
###############################

###-----load package
library(cjoint)
library(tidyverse)
library(openxlsx)
library(ggplot2)
library(gridExtra)



##################################################################
#########################YAHOO####################################
##################################################################



###-------load data


conjoint_data <- read.qualtrics("covid19.csv",
                                responses=c("Q20.1","Q20.2", "Q20.3","Q20.4",
                                            "Q20.5"),
                                covariates = c("Q19.1","Q2.1","Q2.2"),
                                respondentID="ID",new.format = F)

conjoint_data<-na.omit(conjoint_data)



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



###---------plot amce


##making table

#table: myself vaccinate
my<-c(results$estimates$myself[1,1],results$estimates$myself[2,1],
      results$estimates$myself[1,1]+results$estimates$myself[2,1]*2.576,
      results$estimates$myself[1,1]-results$estimates$myself[2,1]*2.576)

#table: myself wait and see
myW<-c(results$estimates$myself[1,2],results$estimates$myself[2,2],
       results$estimates$myself[1,2]+results$estimates$myself[2,2]*2.576,
       results$estimates$myself[1,2]-results$estimates$myself[2,2]*2.576)

#table: familiar

fami<-c(results$estimates$familiar[1,1],results$estimates$familiar[2,1],
        results$estimates$familiar[1,1]+results$estimates$familiar[2,1]*2.576,
        results$estimates$familiar[1,1]-results$estimates$familiar[2,1]*2.576)


#table: familiar wait and see


famiW<-c(results$estimates$familiar[1,2],results$estimates$familiar[2,2],
         results$estimates$familiar[1,2]+results$estimates$familiar[2,2]*2.576,
         results$estimates$familiar[1,2]-results$estimates$familiar[2,2]*2.576)


#table: society
soc<-c(results$estimates$society[1,1],results$estimates$society[2,1],
       results$estimates$society[1,1]+results$estimates$society[2,1]*2.576,
       results$estimates$society[1,1]-results$estimates$society[2,1]*2.576)


#table: society wait and see
socW<-c(results$estimates$society[1,2],results$estimates$society[2,2],
        results$estimates$society[1,2]+results$estimates$society[2,2]*2.576,
        results$estimates$society[1,2]-results$estimates$society[2,2]*2.576)


#integrate above 6 tables
tb_all<-rbind(my,myW,fami,famiW,soc,socW)
write.xlsx(tb_all,"results_yahoo.csv")


#load csv file operated with Excel
df_fig<-read.csv("results_figure_yahoo.csv")
attach(df_fig)


#plot with ggplot2

#set dodge
pd <-position_dodge(0.4)

#set levels order
df_fig$var <- factor(df_fig$var,
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
p = ggplot(df_fig,aes(y=pe,x=var,colour=group))
p = p + coord_flip(ylim = c(0, .3))
p=p+geom_errorbar(aes(ymin=lower, ymax=upper),
                    position=pd, width=.1,na.rm=T) 
p=p+geom_point(position=pd,size=3, shape=21, fill="white")+
  labs(y="Willingness of Vacctination（Average Marginal Component-specific Effects) \n
       All Base Lines: Not Vaccinate" )+
  labs(x="")
p=p+scale_color_discrete(name = "Familiar entity")
P=theme_bw1()
p=p+ggtitle("AMCE of conjoint analysis (YCS)")
p=p+ theme(legend.position = "none")
p


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




#plot
p1 = ggplot(df_figGroup,aes(y=pe,x=var,colour=group_fam))
p1 = p1 + coord_flip(ylim = c(-.3, .4))
p1=p1+geom_errorbar(aes(ymin=lower, ymax=upper),
                  position=pd, width=.1,na.rm=T) 
p1=p1+geom_point(position=pd,size=3, shape=21, fill="white")+
  labs(y="Willingness of Vacctination（Average Marginal Component-specific Effects) by group\n
       All Base Lines: Not Vaccinate" )+
  labs(x="")
p1=p1+scale_color_discrete(name = "Familiar entity")
P1=theme_bw1()
p1=p1+ggtitle("AMCE sorted by the selection of familiar entity (YCS)")
p1











#############################################
######################Lucid##################
#############################################



###-------load data


conjoint_data_l <- read.qualtrics("covid19_lucid.csv",
                                  responses=c("Q11.1","Q11.2", "Q11.3","Q11.4",
                                              "Q11.5"),
                                  covariates = c("Q2.1","Q2.2","Q2.4","Q10"),
                                  respondentID="ID",new.format = F)

conjoint_data_l<-na.omit(conjoint_data_l)

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

###--------------- plot amce

##making table

#table: myself vaccinate
my_l<-c(results_l$estimates$myself[1,1],results_l$estimates$myself[2,1],
        results_l$estimates$myself[1,1]+results_l$estimates$myself[2,1]*2.576,
        results_l$estimates$myself[1,1]-results_l$estimates$myself[2,1]*2.576)

#table: myself wait and see
myW_l<-c(results_l$estimates$myself[1,2],results_l$estimates$myself[2,2],
         results_l$estimates$myself[1,2]+results_l$estimates$myself[2,2]*2.576,
         results_l$estimates$myself[1,2]-results_l$estimates$myself[2,2]*2.576)

#table: familiar

fami_l<-c(results_l$estimates$familiar[1,1],results_l$estimates$familiar[2,1],
          results_l$estimates$familiar[1,1]+results_l$estimates$familiar[2,1]*2.576,
          results_l$estimates$familiar[1,1]-results_l$estimates$familiar[2,1]*2.576)


#table: familiar wait and see


famiW_l<-c(results_l$estimates$familiar[1,2],results_l$estimates$familiar[2,2],
           results_l$estimates$familiar[1,2]+results_l$estimates$familiar[2,2]*2.576,
           results_l$estimates$familiar[1,2]-results_l$estimates$familiar[2,2]*2.576)


#table: society
soc_l<-c(results_l$estimates$society[1,1],results_l$estimates$society[2,1],
         results_l$estimates$society[1,1]+results_l$estimates$society[2,1]*2.576,
         results_l$estimates$society[1,1]-results_l$estimates$society[2,1]*2.576)


#table: society wait and see
socW_l<-c(results_l$estimates$society[1,2],results_l$estimates$society[2,2],
          results_l$estimates$society[1,2]+results_l$estimates$society[2,2]*2.576,
          results_l$estimates$society[1,2]-results_l$estimates$society[2,2]*2.576)


#integrate above 6 tables
tb_all_l<-rbind(my_l,myW_l,fami_l,famiW_l,soc_l,socW_l)
write.xlsx(tb_all_l,"results_lucid.csv")
#In excel file, "group_fam" variable is added and "var" variable is modified. 
#"results_table_forFigure.csv" is reproduced and reload as the below.


#load csv file operated with Excel
df_fig_l<-read.csv("results_figure_lucid.csv")
attach(df_fig_l)


#plot with ggplot2

#set dodge
pd <-position_dodge(0.4)

#set levels order
df_fig_l$var <- factor(df_fig_l$var,
                       levels = c("Society:w&s", "Society:vaccinate",
                                  "Familiar:w&s","Familiar:vaccinate", 
                                  "Myself:w&s","Myself:vaccinate" ))


#plot
pl = ggplot(df_fig_l,aes(y=pe,x=var,colour=group))
pl = pl + coord_flip(ylim = c(0, .3))
pl=pl+geom_errorbar(aes(ymin=lower, ymax=upper),
                    position=pd, width=.1,na.rm=T) 
pl=pl+geom_point(position=pd,size=3, shape=21, fill="white")+
  labs(y="Willingness of Vacctination（Average Marginal Component-specific Effects) \n
       All Base Lines: Not Vaccinate" )+
  labs(x="")
pl=pl+scale_color_discrete(name = "Familiar entity")
Pl=theme_bw1()
pl=pl+ggtitle("AMCE of conjoint analysis (Lucid)")
pl=pl+ theme(legend.position = "none")
pl




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
pl2 = ggplot(df_figGroup_l,aes(y=pe,x=var,colour=group_fam))
pl2 = pl2 + coord_flip(ylim = c(-.3, .4))
pl2=pl2+geom_errorbar(aes(ymin=lower, ymax=upper),
                    position=pd, width=.1,na.rm=T) 
pl2=pl2+geom_point(position=pd,size=3, shape=21, fill="white")+
  labs(y="Willingness of Vacctination（Average Marginal Component-specific Effects) by group\n
       All Base Lines: Not Vaccinate" )+
  labs(x="")
pl2=pl2+scale_color_discrete(name = "Familiar entity")
pl2=pl2+ggtitle("AMCE sorted by the selection of familiar entity (Lucid)")
pl2=pl2
pl2










###---------arrange above four figures

windows(width = 23, height = 10)
img1<-grid.arrange(p,pl, ncol = 2, nrow = 1)

ggsave(file = "fig3_conjoint.pdf", plot = img1)

windows(width = 23, height = 10)
img2<-grid.arrange(p1,pl2, ncol = 2, nrow = 1)
img2
ggsave(file = "fig4_conjoint_group.pdf", plot = img2)
