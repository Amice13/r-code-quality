################################################################################
# #################### Get the data  ###########################################
################################################################################

packs <- c( 'tidyverse', 'ggeffects', 'stargazer', 'sjPlot', 'patchwork','ggpubr', 'stargazer', 'vtable', 'sjlabelled' )
success <- suppressWarnings(sapply(packs, require, character.only = TRUE))
install.packages(names(success)[!success])
sapply(names(success)[!success], require, character.only = TRUE)


getwd()

source("ReplicationCode_01_Preparation.R")
source("ReplicationCode_02_Functions.R")


getwd()



################################################################################
# #################### POLICY CHOICE (H1)   ####################################
################################################################################

###  FIGURE 8 Start ####
# 1st Figure in the Results Section 
fchigh_logit <- glm(FirstChoiceHighPol ~ goal*treat  , data = data, family = "binomial")
summary(fchigh_logit)



FirsthighPol1 <- plot_model(fchigh_logit, type = "int", 
                            show.legend = TRUE,
                            transform = "plogis", show.values = T, dot.size = 5, line.size = 1.5, value.offset = .3, 
                            colors = c("#b3b3b3",  "black", "#666666"), 
                            dodge = 0.5,
                            legend.title = c("") ) +
  labs(y= "", 
       title = paste0( "Predicted probabilities for initial policy choice = max\n(n = ", nobs(fchigh_logit),")")) +
  policyplustheme_w_legend()


FirsthighPol1
fig2tex(FirsthighPol1, "Out/Fig8_FirsthighPol1.tex")
###  Figure 8 END ####

### Formal tests Start ####
fchigh_logit <- glm(FirstChoiceHighPol ~ treat*goal  , data = data, family = "binomial")
summary(fchigh_logit)


car::matchCoefs(fchigh_logit, ":")

car::linearHypothesis(fchigh_logit, c("goalHigh Goal-goalMedium Goal"), c(0))

car::linearHypothesis(fchigh_logit, c("goalHigh Goal-(Intercept)"), 0)


car::linearHypothesis(fchigh_logit, c("treatPolicy+Goal:goalHigh Goal - treatPolicy+Goal:goalMedium Goal"), 0)

car::linearHypothesis(fchigh_logit, c("treatPolicy+Goal:goalHigh Goal - treatGoal Assigned:goalHigh Goal"), 0)

car::linearHypothesis(fchigh_logit, c("treatPolicy+Goal:goalHigh Goal-(Intercept)"), 0)

### Formal tests END ####


###  Figure 9 Start ####
### create the variable FirstChoiceAnyPol that is the opposite of FirstChoiceNoPol
data <- data |> mutate(FirstChoiceAnyPol = case_when(FirstChoiceNoPol==0 ~1, 
                                                     .default = 0) )


FirstChoiceAnyPol <- glm(FirstChoiceAnyPol ~ goal*treat  , data = data, family = "binomial")



FCAnyPol <- plot_model(FirstChoiceAnyPol, type = "int", 
                       show.legend = TRUE,
                       transform = "plogis", show.values = T, dot.size = 5, line.size = 1.5, value.offset = .3, 
                       colors = c("#b3b3b3",  "black", "#666666"), 
                       dodge = 0.5,
                       legend.title = c("") ) +
  labs(y= "", 
       title = paste0( "Predicted probabilities for initially choosing any a policy\n(n = ", nobs(FirstChoiceAnyPol),")")) +
  policyplustheme_w_legend() 


FCAnyPol
fig2tex(FCAnyPol, "Out/Fig9_FCAnyPol.tex")
###  Figure 9 END ####





################################################################################
# #################### FOR THE FEEDBACK (H2)   #################################
################################################################################

###  Figure 10 Start ####
#### descriptive on feedback, as depicted in second figure in the results section  

# Make a feedback dataset
# use gs_data for the goal selected group
ga_data <-  data %>% filter(treat== 'Goal Assigned')

# combine them 
potentialfeedbackers <-   bind_rows(ga_data, gs_data)
# remove low goal who never can get feedback. 
potentialfeedbackers <-  potentialfeedbackers %>% filter(goal!= 'Low Goal')

table(potentialfeedbackers$treat, potentialfeedbackers$goal)

table(potentialfeedbackers$treat, potentialfeedbackers$Feedback_decison, exclude = NULL)
table(potentialfeedbackers$goal, potentialfeedbackers$Feedback_decison, exclude = NULL)

actualfeedbackers <- potentialfeedbackers %>% filter(  !(is.na(Feedback_decison)))
table(actualfeedbackers$Feedback_decison, exclude = NULL)
table(actualfeedbackers$treat, actualfeedbackers$goal)
table(actualfeedbackers$treat)


votednon <-  potentialfeedbackers[potentialfeedbackers$firstpolchoice %in% c('None'),  ]
table(votednon$treat,votednon$goal)
table(votednon$treat)



# remove empty labels in treatment for the actualfeedbackers. 
actualfeedbackers$treat <- factor(actualfeedbackers$treat)


table(actualfeedbackers$treat,actualfeedbackers$Feedback_decison)
# Distribution of feedback reaction
prop.table(table(actualfeedbackers$Feedback_decison))

# how many people did decide to do what by Treatment
prop.table(table(actualfeedbackers$treat, actualfeedbackers$Feedback_decison), margin = 1)




feedbackreaction_stacked <- plot_xtab(
  x   = actualfeedbackers$treat, 
  grp = actualfeedbackers$Feedback_decison, 
  margin  = "row", 
  bar.pos = "stack",
  show.summary = FALSE,
  coord.flip   = FALSE,     
  legend.title = c("Altered after feedback:"),
  geom.colors = c("#fdb863", "#f7f7f7", "#5e3c99"))+
  labs(x= ""
       , title = paste0("Decisions after Feedback (n = ", actualfeedbackers %>%   summarise(total_non_na = sum(!is.na(Feedback_decison))) , ")")) +
  scale_x_discrete(labels = c(paste0("Goal Assigned\n(n = ", actualfeedbackers %>% filter(treat == "Goal Assigned" )  %>%  summarise(total_non_na = sum(!is.na(Feedback_decison))) ,")"),
                              paste0("Goal Selected\n(n = ", actualfeedbackers %>% filter(treat != "Goal Assigned" )  %>%  summarise(total_non_na = sum(!is.na(Feedback_decison))) , ")")
  )) +
  policyplustheme_w_legend() +
  theme(   plot.title = element_text(     margin = margin(b = 8)   )) # move title a bit up

feedbackreaction_stacked 

fig2tex(feedbackreaction_stacked, "Out/Fig10_feedbackreaction_stacked.tex")
###  Figure 10 END ####


#Different reactions between treatment groups ----

cntgTable <- table(actualfeedbackers$treat, actualfeedbackers$Feedback_decison)

chsqTest <- chisq.test(cntgTable, correct = TRUE)
print(cntgTable)
print(chsqTest)



#### Significant difference between first choice groups among all (both treatmentgroups) =====
group = c ("Goal", "Neither", "Policy")
count = c(220, 188, 260)

df = data.frame(group, count) 

chisq.test(df$count, p = c(1/3,1/3,1/3))

# goal-vs-pol-df
goalpoldf <-  df |> filter(group!="Neither") 
chisq.test(goalpoldf$count, p = c(1/2, 1/2))

# goal-vs-neither-df
goalneitherdf <-  df |> filter(group!="Policy") 
chisq.test(goalneitherdf$count, p = c(1/2, 1/2))

# goal-vs-neither-df
polneitherdf <-  df |> filter(group!="Goal") 
chisq.test(polneitherdf$count, p = c(1/2, 1/2))



### FIGURE 11 Start ####

#  After Feedback, Decrease Goal Ambition =====

WannaAlterGoal_all <-  actualfeedbackers %>% filter(Feedback_decison == "Goal")

table(WannaAlterGoal_all$GoalDecreasedAfterFeedback, exclude = NULL)


# 1 obs just dropped out at this point. (see variable Zielanpassung)

WannaAlterGoal <-  WannaAlterGoal_all %>%  filter(! is.na(GoalDecreasedAfterFeedback))

table(WannaAlterGoal$lgoal, exclude = NULL)
table(WannaAlterGoal$GoalDecreasedAfterFeedback, exclude = NULL)
prop.table(table(WannaAlterGoal$GoalDecreasedAfterFeedback))

WannaAlterGoal <- WannaAlterGoal %>% mutate(igoal =  case_when(goal == "Medium Goal"~"Medium", 
                                                               goal == "High Goal" ~"High") )



GoalDecreasedAfterFeedback_logit <- glm(GoalDecreasedAfterFeedback ~ igoal*treat  , data = WannaAlterGoal, family = "binomial")
summary(GoalDecreasedAfterFeedback_logit )

nobs(GoalDecreasedAfterFeedback_logit)

goaldecreasedafterfeedback <-        plot_model(GoalDecreasedAfterFeedback_logit, #  title = "After Feedback: Goal decreased, n = 219",
                                                show.legend = TRUE, dot.size = 5, line.size = 1.5, 
                                                type = "int", show.values = T, value.offset = .3,     
                                                colors = c( "#666666", "black"))  +
  aes(linetype=group, color=group) + #  use different line-types
  scale_linetype_manual(values = c("solid", "longdash")) +  #dotted line definition
  labs(y = "",  color = "", linetype  = "", x="Initial Goal",
       title = paste0("After Feedback: Goal decreased (n = " , nobs(GoalDecreasedAfterFeedback_logit) , ")") )  +
  policyplustheme_w_legend()+
  theme(axis.title.x = element_text(  size=14,  colour = "black"))


goaldecreasedafterfeedback
fig2tex(goaldecreasedafterfeedback, "Out/Fig11_goaldecreasedafterfeedback.tex")
### FIGURE 11 END ####



