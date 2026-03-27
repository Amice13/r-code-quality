########################################
############# ANALYSIS #################
########################################

#This code reproduces the tables and figures included in the main text. 

#The dataset is created using the code in the script "01_clean&stack.R"

#INCLUDE YOUR PATH HERE
rm(list=ls())
setwd("")

#warning: in some operating systems, the file needs to be opened in UTF-8


# Figure 2 - Type of action respondents think the U.S. should take --------

#install.packages("rio")
library("rio")

#Load data
conj <- rio::import("conjoint-data-stacked.rds")

#Prepare data
attackpct <- table(conj$attack)
attackpct <- cbind(attackpct,prop.table(attackpct))

sanctionpct <- table(conj$sanction)
sanctionpct <- cbind(sanctionpct,prop.table(sanctionpct))

foreignpct <- table(conj$foreignaid)
foreignpct <- cbind(foreignpct,prop.table(foreignpct))

outcome_plot <- cbind(attackpct,sanctionpct, foreignpct)
outcome_plot <- as.data.frame(outcome_plot)
outcome_plot$response <- c(0,1)

#install.packages("tidyr")
library("tidyr")
keycol <- "outcome"
valuecol <- "percent"
gathercols <- c("V2", "V4", "V6")

dat <- gather_(outcome_plot, keycol, valuecol, gathercols)
dat_plot <- dat[ which(dat$response==1), ]
dat_plot$outcomef <- c("Military intervention", "Economic sanctions", "Democracy Aid")
dat_plot$outcomef <- as.factor(dat_plot$outcomef)

dat_plot$outcomef <- factor(dat_plot$outcomef, 
                            levels = c("Military intervention", "Economic sanctions",
                                       "Democracy Aid"))

#plot
#install.packages("ggplot2")
library("ggplot2")


ggplot(dat_plot) +
  geom_bar(aes(y=(percent*100), x=outcomef), stat="identity") +
  ylab("Percentage") +
  theme(axis.title.x=element_blank(),
        panel.background=element_blank(),
        text = element_text(size=15),
        axis.text = element_text(size=17)) +
  scale_y_continuous(limits=c(0, 60)) 

#ggsave("Figure_2.png", width = 20, height = 20, units = "cm")

# Figure 3 - The effect of authoritarian’s regime attributes on the likelihood of choosing a given foreign policy instrument --------

#install.packages("cjoint")
library("cjoint")

#create a dataset with only the variables we need and exclude missings
myvars <- c("attack", "oil", "ally", "trade", "years", "gov", "person", "elections",
            "religion", "militar", "ID", "sanction", "foreignaid", "ideol")
data_short <- conj[myvars]
data_short <- data_short[complete.cases(data_short),]

#run the analyses separately
res_attack <-  glm(attack ~ oil + ally  + trade + years  + 
                     gov + person + elections + religion + militar, 
                   data = data_short, family = binomial(link = "probit"))
res_sanction <-  glm(sanction ~ oil + ally  + trade + years  + 
                       gov + person + elections + religion + militar, 
                     data = data_short, family = binomial(link = "probit"))
res_foreignaid <- glm(foreignaid ~ oil + ally  + trade + years  + 
                        gov + person + elections + religion + militar, 
                      data = data_short, family = binomial(link = "probit"))


#extract the coefficients of each model
attack_coef <- summary(res_attack)
attack_coef <- attack_coef[["coefficients"]]
attack_coef <- data.frame(attack_coef)
attack_coef <- attack_coef[-1,] #remove intercept
attack_coef$Attribute <- c("oil", "ally", "trade", "years", "years", "gov", 
                           "person", "person", "elections", "elections", "religion",
                           "religion", "militar")
attack_coef$Level <- c("Oil-exporting country", "The country is NOT a US ally",
                       "The country is NOT an important trade partner of the US",
                       "10 years", "25 years", "A single unconstrained individual (the leader)",
                       "A member of the military", "A monarch",
                       "Yes and some opposition parties are allowed to run", "Yes but only regime candidates can run",
                       "Buddhist", "Muslim", "Strong")

attack_coef$lb <- attack_coef$Estimate - (1.96*attack_coef$Std..Error)
attack_coef$ub <- attack_coef$Estimate + (1.96*attack_coef$Std..Error)
attack_coef$Level <- as.factor(attack_coef$Level)

myvars <- c("Attribute", "Level", "Estimate", "lb", "ub")
coefs_graph <- attack_coef[myvars]
coefs_graph$outcome <- as.factor(1) 

sanction_coef <- summary(res_sanction)
sanction_coef <- sanction_coef[["coefficients"]]
sanction_coef <- data.frame(sanction_coef)
sanction_coef <- sanction_coef[-1,] #remove intercept
sanction_coef$Attribute <- c("oil", "ally", "trade", "years", "years", "gov", 
                             "person", "person", "elections", "elections", "religion",
                             "religion", "militar")
sanction_coef$Level <- c("Oil-exporting country", "The country is NOT a US ally",
                         "The country is NOT an important trade partner of the US",
                         "10 years", "25 years", "A single unconstrained individual (the leader)",
                         "A member of the military", "A monarch",
                         "Yes and some opposition parties are allowed to run", "Yes but only regime candidates can run",
                         "Buddhist", "Muslim", "Strong")

sanction_coef$lb <- sanction_coef$Estimate - (1.96*sanction_coef$Std..Error)
sanction_coef$ub <- sanction_coef$Estimate + (1.96*sanction_coef$Std..Error)
sanction_coef$Level <- as.factor(sanction_coef$Level)
myvars <- c("Attribute", "Level", "Estimate", "lb", "ub")
coefs_graph3 <- sanction_coef[myvars]
coefs_graph3$outcome <- as.factor(2) 


foreign_coef <- summary(res_foreignaid)
foreign_coef <- foreign_coef[["coefficients"]]
foreign_coef <- data.frame(foreign_coef)
foreign_coef <- foreign_coef[-1,] #remove intercept
foreign_coef$Attribute <- c("oil", "ally", "trade", "years", "years", "gov", 
                            "person", "person", "elections", "elections", "religion",
                            "religion", "militar")
foreign_coef$Level <- c("Oil-exporting country", "The country is NOT a US ally",
                        "The country is NOT an important trade partner of the US",
                        "10 years", "25 years", "A single unconstrained individual (the leader)",
                        "A member of the military", "A monarch",
                        "Yes and some opposition parties are allowed to run", "Yes but only regime candidates can run",
                        "Buddhist", "Muslim", "Strong")
foreign_coef$lb <- foreign_coef$Estimate - (1.96*foreign_coef$Std..Error)
foreign_coef$ub <- foreign_coef$Estimate + (1.96*foreign_coef$Std..Error)
foreign_coef$Level <- as.factor(foreign_coef$Level)

myvars <- c("Attribute", "Level", "Estimate", "lb", "ub")
coefs_graph2 <- foreign_coef[myvars]
coefs_graph2$outcome <- as.factor(3) 

coefs_final <- rbind(coefs_graph, coefs_graph2, coefs_graph3)


coefs_final$outcome <- factor(coefs_final$outcome,
                              levels = c(1,2,3),
                              labels = c("Military intervention", 
                                         "Economic sanctions", "Democracy Aid")) 


#Now we need to create different rows for the titles and the baselines:
coefs_final$Level <- paste0('   ', coefs_final$Level)

#create new rows with information to add
Level <- c("Trade relationships",
           "   (B=The country is an important trade partner of the US)",
           "International military alliance",
           "   (B=The country is a US ally)",
           "Natural resources",
           "   (B=Non-oil exporting country)",
           "Political power and policy are controlled by...",
           "   (B=A group of top regime officials (a council, party comittee or junta))",
           "The regime's leader is...",
           "   (B=A civilian who heads the regime's official party)",
           "Does the regime hold elections?",
           "   (B=No)",
           "Years the regime has been in power...",
           "   (B=4 years)",
           "The country is predominantly...",
           "   (B=Christian)",
           "The regime is militarily...",
           "   (B=Weak)")

library("dplyr")
coefs_final <- add_row(coefs_final, Level=Level)
coefs_final$outcome <- as.character(coefs_final$outcome)
coefs_final$outcome[is.na(coefs_final$outcome)] <- "Military intervention"

coefs_final <- add_row(coefs_final, Level=Level)
coefs_final$outcome[is.na(coefs_final$outcome)] <- "Economic sanctions"

coefs_final <- add_row(coefs_final, Level=Level)
coefs_final$outcome[is.na(coefs_final$outcome)] <- "Democracy Aid"


#Relevel the Levels
coefs_final$Level <- as.factor(coefs_final$Level)
coefs_final$levels_ordered <- coefs_final$Level

coefs_final$levels_ordered <- factor(coefs_final$levels_ordered,
                                     levels=c("Political power and policy are controlled by...",
                                              "   (B=A group of top regime officials (a council, party comittee or junta))",
                                              "   A single unconstrained individual (the leader)",
                                              "The regime's leader is...",
                                              "   (B=A civilian who heads the regime's official party)",
                                              "   A monarch",
                                              "   A member of the military",
                                              "Does the regime hold elections?",
                                              "   (B=No)",
                                              "   Yes but only regime candidates can run",
                                              "   Yes and some opposition parties are allowed to run",
                                              "Years the regime has been in power...",
                                              "   (B=4 years)",
                                              "   10 years",
                                              "   25 years",
                                              "The country is predominantly...",
                                              "   (B=Christian)",
                                              "   Buddhist",
                                              "   Muslim",
                                              "The regime is militarily...",
                                              "   (B=Weak)",
                                              "   Strong" ,
                                              "Natural resources",
                                              "   (B=Non-oil exporting country)",
                                              "   Oil-exporting country" ,
                                              "International military alliance",
                                              "   (B=The country is a US ally)" ,
                                              "   The country is NOT a US ally",
                                              "Trade relationships",
                                              "   (B=The country is an important trade partner of the US)",
                                              "   The country is NOT an important trade partner of the US"))

coefs_final$levels_ordered <- factor(coefs_final$levels_ordered, 
                                     levels=rev(levels(coefs_final$levels_ordered)))


coefs_final$outcome <- factor(coefs_final$outcome,
                              levels = c("Military intervention", 
                                         "Economic sanctions", "Democracy Aid"))


#the warning message is due to missings in the baselines. no worries.

ggplot(coefs_final) +
  geom_point(aes(y=Estimate, x=levels_ordered, color=outcome, group=outcome), 
             position = position_dodge(width = 0.5)) +
  geom_pointrange(aes(y=Estimate, x=levels_ordered, ymin=lb,ymax=ub, 
                       group=outcome), position = position_dodge(width = 0.5)) +
  coord_flip() +
  theme_bw() +
  geom_hline(yintercept=0,linetype=4, colour="black") +
  facet_grid( ~ outcome, drop=T ) +
  theme(legend.position="none",
        axis.text.y = element_text(hjust = 0),
        axis.ticks.y=element_blank(),
        axis.text.x = element_text(size=7)) +
  xlab("") 


#ggsave("Figure_3.png", width = 30, height = 20, units = "cm")


# Figure 4 - Predicted probabilities for alternative regime types --------


# Saudi Arabia

#simulations
dt_arabia <- data.frame(oil="Oil-exporting country", 
                        ally="The country is a US ally",
                        trade="The country is an important trade partner of the US",
                        years="25 years",
                        gov="A single unconstrained individual (the leader)",
                        person="A monarch",
                        elections="No",
                        religion="Muslim",
                        militar="Strong") 

#military intervention
ar_attack_df <- predict(res_attack, dt_arabia,  type = 'response', se=T)
predf <- ar_attack_df$fit # predicted
lower <- ar_attack_df$fit - (1.96*ar_attack_df$se.fit) # lower bounds
upper <- ar_attack_df$fit + (1.96*ar_attack_df$se.fit) # upper bounds

ar_attack_df <- data.frame(predf, lower, upper)
ar_attack_df$country <- c("Saudi Arabia")
ar_attack_df$instrument <- c("Intervention")

#sanction
ar_sanction_df <- predict(res_sanction, dt_arabia,  type = 'response', se=T)
predf <- ar_sanction_df$fit # predicted
lower <- ar_sanction_df$fit - (1.96*ar_sanction_df$se.fit) # lower bounds
upper <- ar_sanction_df$fit + (1.96*ar_sanction_df$se.fit) # upper bounds

ar_sanction_df <- data.frame(predf, lower, upper)
ar_sanction_df$country <- c("Saudi Arabia")
ar_sanction_df$instrument <- c("Sanction")

#Democracy aid
ar_foreign_df <- predict(res_foreignaid, dt_arabia,  type = 'response', se=T)
predf <- ar_foreign_df$fit # predicted
lower <- ar_foreign_df$fit - (1.96*ar_foreign_df$se.fit) # lower bounds
upper <- ar_foreign_df$fit + (1.96*ar_foreign_df$se.fit) # upper bounds

ar_foreign_df <- data.frame(predf, lower, upper)
ar_foreign_df$country <- c("Saudi Arabia")
ar_foreign_df$instrument <- c("Aid")

# Egypt 

dt_egypt <- data.frame(oil="Oil-exporting country", 
                       ally="The country is a US ally",
                       trade="The country is an important trade partner of the US",
                       years="4 years",
                       gov="A group of top regime officials (a council, party committee or junta)",
                       person="A member of the military",
                       elections="Yes and some opposition parties are allowed to run",
                       religion="Muslim",
                       militar="Strong") 


#military intervention
eg_attack_df <- predict(res_attack, dt_egypt,  type = 'response', se=T)
predf <- eg_attack_df$fit # predicted
lower <- eg_attack_df$fit - (1.96*eg_attack_df$se.fit) # lower bounds
upper <- eg_attack_df$fit + (1.96*eg_attack_df$se.fit) # upper bounds

eg_attack_df <- data.frame(predf, lower, upper)
eg_attack_df$country <- c("Egypt")
eg_attack_df$instrument <- c("Intervention")

#sanction
eg_sanction_df <- predict(res_sanction, dt_egypt,  type = 'response', se=T)
predf <- eg_sanction_df$fit # predicted
lower <- eg_sanction_df$fit - (1.96*eg_sanction_df$se.fit) # lower bounds
upper <- eg_sanction_df$fit + (1.96*eg_sanction_df$se.fit) # upper bounds

eg_sanction_df <- data.frame(predf, lower, upper)
eg_sanction_df$country <- c("Egypt")
eg_sanction_df$instrument <- c("Sanction")

#Democracy aid
eg_foreign_df <- predict(res_foreignaid, dt_egypt,  type = 'response', se=T)
predf <- eg_foreign_df$fit # predicted
lower <- eg_foreign_df$fit - (1.96*eg_foreign_df$se.fit) # lower bounds
upper <- eg_foreign_df$fit + (1.96*eg_foreign_df$se.fit) # upper bounds

eg_foreign_df <- data.frame(predf, lower, upper)
eg_foreign_df$country <- c("Egypt")
eg_foreign_df$instrument <- c("Aid")


# Libya 

dt_libya <- data.frame(oil="Oil-exporting country", 
                       ally="The country is NOT a US ally",
                       trade="The country is NOT an important trade partner of the US",
                       years="25 years",
                       gov="A single unconstrained individual (the leader)",
                       person="A member of the military",
                       elections="No",
                       religion="Muslim",
                       militar="Weak") 

#military intervention
li_attack_df <- predict(res_attack, dt_libya,  type = 'response', se=T)
predf <- li_attack_df$fit # predicted
lower <- li_attack_df$fit - (1.96*li_attack_df$se.fit) # lower bounds
upper <- li_attack_df$fit + (1.96*li_attack_df$se.fit) # upper bounds

li_attack_df <- data.frame(predf, lower, upper)
li_attack_df$country <- c("Libya")
li_attack_df$instrument <- c("Intervention")

#sanction
li_sanction_df <- predict(res_sanction, dt_libya,  type = 'response', se=T)
predf <- li_sanction_df$fit # predicted
lower <- li_sanction_df$fit - (1.96*li_sanction_df$se.fit) # lower bounds
upper <- li_sanction_df$fit + (1.96*li_sanction_df$se.fit) # upper bounds

li_sanction_df <- data.frame(predf, lower, upper)
li_sanction_df$country <- c("Libya")
li_sanction_df$instrument <- c("Sanction")

#Democracy aid
li_foreign_df <- predict(res_foreignaid, dt_libya,  type = 'response', se=T)
predf <- li_foreign_df$fit # predicted
lower <- li_foreign_df$fit - (1.96*li_foreign_df$se.fit) # lower bounds
upper <- li_foreign_df$fit + (1.96*li_foreign_df$se.fit) # upper bounds

li_foreign_df <- data.frame(predf, lower, upper)
li_foreign_df$country <- c("Libya")
li_foreign_df$instrument <- c("Aid")

# Tanzania

dt_cam <- data.frame(oil="Non-oil-exporting country", 
                     ally="The country is NOT a US ally",
                     trade="The country is NOT an important trade partner of the US",
                     years="25 years",
                     gov="A group of top regime officials (a council, party committee or junta)",
                     person="A civilian who heads the regime's official party",
                     elections="Yes and some opposition parties are allowed to run",
                     religion="Christian",
                     militar="Weak") 


#miiltary intervention
cm_attack_df <- predict(res_attack, dt_cam,  type = 'response', se=T)
predf <- cm_attack_df$fit # predicted
lower <- cm_attack_df$fit - (1.96*cm_attack_df$se.fit) # lower bounds
upper <- cm_attack_df$fit + (1.96*cm_attack_df$se.fit) # upper bounds

cm_attack_df <- data.frame(predf, lower, upper)
cm_attack_df$country <- c("Tanzania")
cm_attack_df$instrument <- c("Intervention")

#sanction
cm_sanction_df <- predict(res_sanction, dt_cam,  type = 'response', se=T)
predf <- cm_sanction_df$fit # predicted
lower <- cm_sanction_df$fit - (1.96*cm_sanction_df$se.fit) # lower bounds
upper <- cm_sanction_df$fit + (1.96*cm_sanction_df$se.fit) # upper bounds

cm_sanction_df <- data.frame(predf, lower, upper)
cm_sanction_df$country <- c("Tanzania")
cm_sanction_df$instrument <- c("Sanction")

#Democracy aid
cm_foreign_df <- predict(res_foreignaid, dt_cam,  type = 'response', se=T)
predf <- cm_foreign_df$fit # predicted
lower <- cm_foreign_df$fit - (1.96*cm_foreign_df$se.fit) # lower bounds
upper <- cm_foreign_df$fit + (1.96*cm_foreign_df$se.fit) # upper bounds

cm_foreign_df <- data.frame(predf, lower, upper)
cm_foreign_df$country <- c("Tanzania")
cm_foreign_df$instrument <- c("Aid")


#Append datasets
df  <- rbind(ar_attack_df,ar_foreign_df,ar_sanction_df,
             eg_attack_df,eg_foreign_df,eg_sanction_df,
             li_attack_df,li_foreign_df,li_sanction_df,
             cm_attack_df,cm_foreign_df,cm_sanction_df)

df$instrument[df$instrument=="Aid"] <- "Democracy Aid"
df$instrument[df$instrument=="Sanction"] <- "Economic sanctions"
df$instrument[df$instrument=="Intervention"] <- "Military intervention"

#re-establish order group variable
df$instrument <- as.factor(df$instrument)
df$instrument <- factor(df$instrument ,
                        levels=c("Military intervention", "Economic sanctions", "Democracy Aid"))
df$country <- as.factor(df$country)
df$country <- factor(df$country ,
                     levels=c("Libya", "Saudi Arabia", "Egypt",
                              "Tanzania"))

#graph
ggplot(df) +
  geom_pointrange(aes(x = instrument, y = predf, ymin = lower, 
                      ymax = upper, 
                      group=country,
                      shape=country), 
                  position = position_dodge(.3), lwd=1, stat = "identity") +
  ylab(("Predicted probabilities" )) +
  xlab("") +
  theme_bw() +
  scale_shape_manual(values=c(19,18,15,17)) +
  theme(axis.text=element_text(size=20),
        axis.text.x = element_text(),
        legend.position = "bottom",
        legend.title=element_blank(),
        axis.title=element_text(size=15),
        legend.text=element_text(size=15))

#ggsave("Figure_4.png", width = 30, height = 20, units = "cm")


