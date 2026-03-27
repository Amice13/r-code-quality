#################################
# Replication Script for: "Immigrant Detention be Banned?: Constituent, Subconstituent, and 
# Elite Influence over House Democrat's Decision to Cosponsor the Dignity for Detained Act"

#Journal: Legislative Studies Quarterly

# Corresponding Author: Jason L. Morín, California State University Northridge
# Author: Loren Collingwood, University of New Mexico

# Date Created: 11/11/2024
#################################





#########################################
#Packages 
########################################

library(tidyverse)
library(miceadds)
library(ggeffects)
library(ggplot2)
library(scales) 
library(xtable)
library(doBy)



#########################################
#Load Data 
########################################

dignity_replication_dat <- file.choose(" ")
dignity_replication_dat <-readRDS(dignity_replication_dat)


#########################################
#Variable Description
########################################

# bioguide_id = Member Congress Code

# cosponsor_dummy = Cosponsored Dignity for Detained Immigrants Act 

# liberal_immigration.mean = Average Liberal Immigration Opinion

# latino_pop = Percent Latino Population  

# asian_pop = Percent Asian American Population 

# foreign_pop = Percent Foreign-Born Population 

# opinion_ordinal = Liberal Immigration Opinion coded as ordinal variable 

# latino_ordinal =  Percent Latino Pop coded as ordinal variable 

# asian_ordinal = Percent Asian American Pop coded as ordinal variable 

# foreign_ordinal = Percent Foreign Born Pop coded as ordinal variable 

# private_prison_dummy = Private Immigrant Detention Facility in a District (dichotomous)

# private_prison.sum = Total Number of Private Detention Facilities (count)

# pac_money_dummy = Private Prison PAC Contribution (dichotomous) 

# pac_money = Private Prison PAC Contribution (continuous) 

# nominate_dim1 = Liberal Ideology (of MC)

# margin_victory = Margin of Victory 

# terms_served = Seniority (Number of Terms Served by MC) 

# retired =  Retiring 

# committee_leader = Committee Leader

# judiciary = Judiciary Committee 

# homeland_security = Homeland Security Committee 

# congress = Congress 

# congress_117 = 117th Congress  

# congress_116 = 116th Congress 

# no_cuellar_bishop = Identifies Henry Cuellar and Sanford Bishop


#########################################################################################
#Figure 1: Private Prison Distrits and PAC Contributions to House Democrats#
#########################################################################################
money<-filter(dignity_replication_dat, pac_money_dummy==1)
money<-money[c("bioname", "pac_money", "congress")]
money <- money%>%
  group_by(bioname)%>%
  mutate(sum_money= sum(pac_money) )


ggplot(money) +
  geom_bar(aes(x = reorder(bioname, sum_money), y = pac_money, fill = factor(congress)), 
           position="stack", stat="identity") +
  scale_fill_grey() + 
  theme_classic () +
  labs(y="Private Prison PAC Contributions", x="House Democrat", fill="Congress") + 
  coord_flip() + 
  scale_y_continuous(breaks=seq(0,60000, by=5000), labels=dollar) + 
  theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1)) 



prison<-filter(dignity_replication_dat, private_prison_dummy==1)
prison<-prison[c("bioname", "private_prison.sum", "congress")]
prison <- prison%>%
  group_by(bioname)%>%
  mutate(mean_prison= mean(private_prison.sum) )

prison <-summaryBy(private_prison.sum~ bioname, FUN=mean, data=prison)

ggplot(prison) +
  geom_bar(aes(x = reorder(bioname, private_prison.sum.mean), y = private_prison.sum.mean), 
           stat="identity") +
  scale_fill_grey() + 
  theme_classic () +
  labs(y="Average Number of Private Prison Facilities", x="House Democrat") + 
  coord_flip()



#########################################################################################
#Table 1: Logistic Regression Predicting Support for Dignity for Detained Immigrants Act#
#########################################################################################


summary (model_1<- glm.cluster(cosponsor_dummy ~  liberal_immigration.mean + latino_pop + asian_pop + private_prison_dummy  + pac_money_dummy + nominate_dim1 +
                                      margin_victory + terms_served + retired + committee_leader + judiciary + homeland_security + congress_117 + congress_116 
                                    , data=dignity_replication_dat, family = "binomial", cluster="bioguide_id"))


summary (model_2<- glm.cluster(cosponsor_dummy ~ liberal_immigration.mean + foreign_pop + private_prison_dummy + pac_money_dummy + nominate_dim1 + 
                                      margin_victory + terms_served + retired + committee_leader + judiciary + homeland_security +  congress_117 + congress_116 
                                    , data=dignity_replication_dat, family = "binomial", cluster="bioguide_id"))





############################################
#Figure 2: Probability of Co sponsorship by Liberal Immigration Opinion
############################################

opinion_predict <-ggpredict(model_1$glm_res, terms="liberal_immigration.mean[0, .5, 1, 1.5, 2, 2.5, 3, 3.5, 4]")

ggplot(opinion_predict, aes(x, predicted)) +
  geom_line() +
  scale_x_continuous(breaks=c(0, .5, 1, 1.5,2, 2.5, 3, 3.5, 4 ))+ 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha=0.1) + 
  labs(y="Pr(Bill Cosponsor)", x= "Liberal Immigration Opinion")


############################################
#Figure 3: Probability of Co sponsorship by Subconstituency
############################################

predict_latino<- ggpredict(model_1$glm_res, terms=c("latino_pop[0,.1, .2, .3, .4, .5, .6]"))

ggplot(predict_latino, aes(x, predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1) + 
  ylim(.4,.9) +
  labs(y="Pr(Bill Cosponsor)", x= "Percent Latino Pop.")


predict_foriegn<- ggpredict(model_2$glm_res, terms=c("foreign_pop[0, .05, .1, .15 .2, .25,  .3, .35, .4"))

ggplot(predict_foriegn, aes(x, predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1) + 
  labs(y="Pr(Bill Cosponsor)", x= "Percent Foreign Pop.")




############################################
#Figure 4: Probability of Cosponsorship by PAC Recipient
############################################
predict_money <-ggpredict(model_1$glm_res, terms="pac_money_dummy")

predict_money$x <-as.factor(predict_money$x)
levels(predict_money$x) <- list("No Donation" = "0", "PAC Donation" = "1")

ggplot(predict_money, 
       aes(factor(x), predicted)) + 
  geom_point(position = position_dodge(.1)) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(.1)) +
  labs(y="Pr(Bill Cosponsor)", x= "", caption="Note: Predicted Probabilities
      with 95% Confidence Intervals") + 
  theme(plot.caption=element_text(hjust=0.5)) + 
  geom_text(aes(label = round(predicted, digits=2), y = predicted + .001),
            position = position_dodge(.3),
            hjust=-.2)




##############################################
#Appendix B: Table B1: Descriptive Statistics#
##############################################

desc_stats <- dignity_replication_dat %>%
  dplyr::select(liberal_immigration.mean,
                latino_pop, 
                asian_pop, 
                foreign_pop,
                private_prison_dummy,
                pac_money_dummy, 
                nominate_dim1,
                margin_victory, 
                terms_served, 
                retired,
                committee_leader, 
                judiciary, 
                homeland_security, 
                congress_117, 
                congress_116)


desc_out <- rbind(desc_stats %>%
                    summarise_all(mean, na.rm=T),
                  desc_stats %>%
                    summarise_all(min, na.rm=T),
                  desc_stats %>%
                    summarise_all(max, na.rm=T),
                  desc_stats %>%
                    summarise_all(sd, na.rm=T))

desc_out <- t(desc_out) %>% as.data.frame( )

colnames(desc_out) <- c("Mean", "Min", "Max", "SD")      
row.names(desc_out) <- c( "liberal_immigration.mean",
                         "latino_pop", 
                         "asian_pop", 
                         "foreign_pop",
                         "private_prison_dummy",
                         "pac_money_dummy", 
                         "nominate_dim1",
                         "margin_victory", 
                         "terms_served", 
                         "retired",
                         "committee_leader", 
                         "judiciary", 
                         "homeland_security", 
                         "congress_117", 
                         "congress_116")

print(xtable(desc_out, 
             caption = "Descriptive statistics",
             label = "desc_stat"), include.rownames=T)


############################################
#Appendix C: Figure C1 Preliminary Results
############################################
ggplot(dignity_replication_dat, aes(y=cosponsor_dummy, x=factor(opinion_ordinal))) +
  stat_summary(fun=mean, 
               geom="bar",
               color="black",
               fill="grey") +
  stat_summary( fun.data = mean_se, 
                geom="errorbar", 
                width=.2) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0,1)) + 
  theme_classic() +
  labs(x="Liberal Immigration Opinion", y="% Cosponsor") + 
  scale_x_discrete(labels=c("Most Conservative", " ","", "Most Liberal"))

ggplot(dignity_replication_dat, aes(y=cosponsor_dummy, x=factor(latino_ordinal))) +
  stat_summary(fun=mean, 
               geom="bar",
               color="black",
               fill="grey") +
  stat_summary( fun.data = mean_se, 
                geom="errorbar", 
                width=.2) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0,1)) + 
  theme_classic() +
  labs(x="Pecent Latino Pop.", y="% Cosponsor") + 
  scale_x_discrete(labels=c("< 15%", "15-29%","30%-44%", "45% or more" ))


ggplot(dignity_replication_dat, aes(y=cosponsor_dummy, x=factor(asian_ordinal))) +
  stat_summary(fun=mean, 
               geom="bar",
               color="black",
               fill="grey") +
  stat_summary( fun.data = mean_se, 
                geom="errorbar", 
                width=.2) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0,1)) + 
  theme_classic() +
  labs(x="Asian American Pop.", y="% Cosponsor") + 
  scale_x_discrete(labels=c("< 15%", "15-29%","30%-44%", "45% or more" ))


ggplot(dignity_replication_dat, aes(y=cosponsor_dummy, x=factor(foreign_ordinal))) +
  stat_summary(fun=mean, 
               geom="bar",
               color="black",
               fill="grey") +
  stat_summary( fun.data = mean_se, 
                geom="errorbar", 
                width=.2) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0,1)) + 
  theme_classic() +
  labs(x="Foreign Born Pop.", y="% Cosponsor") + 
  scale_x_discrete(labels=c("< 15%", "15-29%","30%-44%", "45% or more"))

############################################
#Appendix C: Figure C2: Preliminary Results
############################################

ggplot(dignity_replication_dat, aes(y=cosponsor_dummy, x=factor(private_prison_dummy))) +
  stat_summary(fun=mean, 
               geom="bar",
               color="black",
               fill="grey") +
  stat_summary( fun.data = mean_se, 
                geom="errorbar", 
                width=.2) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0,1)) + 
  theme_classic() +
  labs(x="Private Prison District", y="% Cosponsor") + 
  scale_x_discrete(labels=c("Other", "Private Dention Facility"))


ggplot(dignity_replication_dat, aes(y=cosponsor_dummy, x=factor(pac_money_dummy)))+
  stat_summary(fun=mean, 
               geom="bar",
               color="black",
               fill="grey") +
  stat_summary( fun.data = mean_se, 
                geom="errorbar", 
                width=.2) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0,1)) + 
  theme_classic() +
  labs(x="Private Prison PAC Contributions", y="% Cosponsor") + 
  scale_x_discrete(labels=c("Did Not Accept Contributions", "Accepted Contributions"))




#########################################################################################
#Appendix D: Table D1: Model Replaces PAC dummy variable with a continuous PAC variable #
#########################################################################################

summary (model_d1a<- glm.cluster(cosponsor_dummy ~  liberal_immigration.mean + latino_pop + asian_pop + private_prison_dummy  +  pac_money + nominate_dim1 +
                              margin_victory + terms_served + retired + committee_leader + judiciary + homeland_security + congress_117 + congress_116  
                            , data=dignity_replication_dat, family = "binomial", cluster="bioguide_id"))


summary (model_d1b <- glm.cluster(cosponsor_dummy ~ liberal_immigration.mean + foreign_pop + private_prison_dummy + pac_money + nominate_dim1 + 
                              margin_victory + terms_served + retired + committee_leader + judiciary + homeland_security +  congress_117 + congress_116 
                            , data=dignity_replication_dat, family = "binomial", cluster="bioguide_id"))


#######################################################################
#Appendix D: Table D2: Model Excludes Henry Cuellar and Sanford Bishop#
#######################################################################


summary (model_d2a <- glm.cluster(cosponsor_dummy ~ liberal_immigration.mean + latino_pop + asian_pop + private_prison_dummy  + pac_money_dummy + nominate_dim1 +
                              margin_victory + terms_served + retired + committee_leader + judiciary + homeland_security + congress_117 + congress_116 
                            , data=dignity_replication_dat[dignity_replication_dat$no_cuellar_bishop==1,], family = "binomial", cluster="bioguide_id"))


summary (model_d2b <- glm.cluster(cosponsor_dummy ~  liberal_immigration.mean + foreign_pop + private_prison_dummy + pac_money_dummy + nominate_dim1 + 
                               margin_victory + terms_served + retired + committee_leader + judiciary + homeland_security +  congress_117 + congress_116 
                             , data=dignity_replication_dat[dignity_replication_dat$no_cuellar_bishop==1,], family = "binomial", cluster="bioguide_id"))







