############################################################
# This is the code and data to replicate the results from  #
# "Revolving Door Lobbyists and the Value of Congressional #
#  Staff Connections" by Joshua McCrain                    #
# Journal of Politics                                      #
# www.joshuamccrain.com | josh.mccrain@gmail.com           #
############################################################

#install these packages first if necessary:
library(dplyr)
library(stringr)
library(ggplot2)
library(scales) #for plotting purposes
library(lfe)
library(stargazer)

#Set working directory to the replication file location
setwd("C:/Desktop/submission_results")
#Load replication data
load("lobbying_data.RData")

### Main results
## Table 1
m1 <- felm(log(infl_rev) ~ log(num_conn) | first_lobby_yr , data=lobbyists_whole)

m2 <- felm(log(infl_rev) ~ log(num_conn)  + is_cmte + ever_rep + ever_senate  | first_lobby_yr, data=lobbyists_whole)

m3<- felm(log(infl_rev) ~ log(num_conn) + is_cmte + ever_rep + ever_senate + ever_pos_legis + ever_pos_senior + ever_pos_press | first_lobby_yr, data=lobbyists_whole)

m4 <- felm(log(infl_rev) ~ log(num_conn) + is_cmte + ever_rep + ever_senate + ever_pos_legis + ever_pos_senior + ever_pos_press + years_exp + years_exp2 | first_lobby_yr, data=lobbyists_whole)

m5 <- felm(log(infl_rev) ~ log(cmte_conn) + is_cmte + ever_rep + ever_senate + ever_pos_legis + ever_pos_senior + ever_pos_press + years_exp + years_exp2 | first_lobby_yr, data=lobbyists_whole)

#Report highest_rank fixed-effects as coefficients
m6 <- felm(log(infl_rev) ~ log(num_conn) + is_cmte + ever_rep + ever_senate + ever_pos_legis + ever_pos_senior + ever_pos_press + years_exp + years_exp2 + as.factor(highest_rank) | first_lobby_yr, data=lobbyists_whole)

models_2 <- list(m1, m2, m3, m4, m5, m6)

stargazer(models_2, 
          se = list(m1$rse, m2$rse, m3$rse, m4$rse, m5$rse, m6$rse), 
          title = "Total Connections and Lobbying Revenue",
          dep.var.labels = "(log) Highest First Year Lobbying Revenue",
          covariate.labels = c("Number of Connections", "Num. Cmte. Connections", "Ever Committee Staff", "Republican", "Ever Senate Staff", "Legislative Staff","Senior Staff", "Press Staff", "Years of Hill Experience", "Years of Hill Exp. (squared)", "Cmte. Chair", "Committee Staff", "Power Cmte. Chair", "Power Cmte. Staff", "Majority Power Cmte.", "Minority Power Cmte.", "Majority Rank & File"),
          omit.stat = c("f", "ser"),
          style="apsr"
)



### Legislator connections and staff-office connections
## Table 2

m1 <- felm(log(infl_rev) ~ house_conn + senate_conn | first_lobby_yr, data=lobbyists_whole)

m2 <- felm(log(infl_rev) ~ log(num_conn) + house_conn +  senate_conn | first_lobby_yr, data=lobbyists_whole)

m3 <- felm(log(infl_rev) ~ log(num_conn) + house_conn  + senate_conn + is_cmte + ever_rep + ever_senate | first_lobby_yr, data=lobbyists_whole)

m4 <- felm(log(infl_rev) ~ log(num_conn) + house_conn  + senate_conn + is_cmte + ever_rep + ever_senate + ever_pos_legis + ever_pos_senior + ever_pos_press | first_lobby_yr, data=lobbyists_whole)

m5 <- felm(log(infl_rev) ~ staff_offices + house_conn  + senate_conn + is_cmte + ever_rep + ever_senate + ever_pos_legis + ever_pos_senior + ever_pos_press | first_lobby_yr, data=lobbyists_whole)

models_3 <- list(m1, m2, m3, m4, m5)

stargazer(models_3, 
          se = list(m1$rse, m2$rse, m3$rse, m4$rse, m5$rse), 
          title = "Staff Connections, Legislator Connections, and Lobbying Revenue",
          dep.var.labels = "(log) Highest First Year Lobbying Revenue",
          covariate.labels = c("Number of Connections", "Staff-Office Connections", "House Connection", "Senate Connection", "Ever Committee Staff", "Republican", "Ever Senate Staff", "Legislative Staff", "Senior Staff", "Press Staff"),
          omit.stat = c("f", "ser"),
          style="apsr"
)



### Alternative Explanations
## Table 3

ma1 <- felm(log(infl_rev) ~ log(num_conn) + is_cmte + ever_rep + ever_senate + ever_pos_legis + ever_pos_senior + ever_pos_press + specialist + slope | first_lobby_yr, data=lobbyists_whole)
ma2 <- felm(log(infl_rev) ~ log(num_conn) + house_conn + senate_conn + is_cmte + ever_rep + ever_senate + ever_pos_legis + ever_pos_senior + ever_pos_press + specialist + slope | first_lobby_yr, data=lobbyists_whole)

ma3 <- felm(log(infl_rev) ~ log(num_conn) + is_cmte + ever_rep + ever_senate + ever_pos_legis + ever_pos_senior + ever_pos_press + higher_ed + prevexp + specialist + slope| first_lobby_yr, data=lobbyists_whole)
ma4 <- felm(log(infl_rev) ~ log(num_conn) + house_conn + senate_conn + is_cmte + ever_rep + ever_senate + ever_pos_legis + ever_pos_senior + ever_pos_press + higher_ed + prevexp + specialist + slope | first_lobby_yr, data=lobbyists_whole)

ma7 <- felm(log(infl_rev) ~ log(num_conn) + is_cmte + ever_rep + ever_senate + ever_pos_legis + ever_pos_senior + ever_pos_press| first_lobby_yr + total_offices, data=lobbyists_whole)
ma8 <- felm(log(infl_rev) ~ log(num_conn) + house_conn + senate_conn + is_cmte + ever_rep + ever_senate + ever_pos_legis + ever_pos_senior + ever_pos_press | first_lobby_yr + total_offices, data=lobbyists_whole)

modelsa <- list(ma1, ma2, ma3, ma4, ma7, ma8)
stargazer(modelsa, 
          se = list(ma1$rse, ma2$rse, ma3$rse, ma4$rse, ma7$rse, ma8$rse), 
          title = "Alternative Explanations for Predicting Lobbying Revenue",
          dep.var.labels = "(log) Highest First Year Lobbying Revenue",
          covariate.labels = c("Number of Connections","House Connection", "Senate Connection", "Ever Committee Staff", "Republican", "Ever Senate Staff", "Legislative Staff", "Senior Staff", "Press Staff", "Graduate Degree", "Previous Govt. Exper.", "Specialist", "Hill Salary Slope"),         
          omit.stat = c("f", "ser"),
          style="apsr",
          add.lines = list(c("Fixed Effects?", "Year", "Year", "Year", "Year", "Offices + Year", "Offices + Year"))
)




#### Robustness Checks in the Appendix ####

## Table 1C
ma1 <- felm(log(infl_rev) ~ log(num_conn) + is_cmte + ever_rep + ever_senate + ever_pos_legis + ever_pos_senior + ever_pos_press | first_lobby_yr, data=filter(lobbyists_whole, num_conn< 245))
ma2 <- felm(log(infl_rev) ~ log(num_conn) + house_conn + senate_conn + is_cmte + ever_rep + ever_senate + ever_pos_legis + ever_pos_senior + ever_pos_press | first_lobby_yr, data=filter(lobbyists_whole, num_conn< 245))

ma5 <- felm(log(infl_rev) ~ log(num_conn) + is_cmte + ever_rep + ever_senate + ever_pos_legis + ever_pos_senior + ever_pos_press | first_lobby_yr, data=filter(lobbyists_whole, num_conn>1))
ma6 <- felm(log(infl_rev) ~ log(num_conn) + house_conn + senate_conn + is_cmte + ever_rep + ever_senate + ever_pos_legis + ever_pos_senior + ever_pos_press | first_lobby_yr, data=filter(lobbyists_whole, num_conn>1))

ma3 <- felm(log(first_infl_rev) ~ log(num_conn) + is_cmte + ever_rep + ever_senate + ever_pos_legis + ever_pos_senior + ever_pos_press | first_lobby_yr, data=lobbyists_whole)
ma4 <- felm(log(first_infl_rev) ~ log(num_conn) + house_conn + senate_conn + is_cmte + ever_rep + ever_senate + ever_pos_legis + ever_pos_senior + ever_pos_press | first_lobby_yr, data=lobbyists_whole)

ma7 <- felm(log(w_infl_rev) ~ log(num_conn) + is_cmte + ever_rep + ever_senate + ever_pos_legis + ever_pos_senior + ever_pos_press| first_lobby_yr, data=lobbyists_whole)
ma8 <- felm(log(w_infl_rev) ~ log(num_conn) + house_conn + senate_conn + is_cmte + ever_rep + ever_senate + ever_pos_legis + ever_pos_senior + ever_pos_press | first_lobby_yr, data=lobbyists_whole)

modelsa <- list(ma1, ma2, ma5, ma6, ma3, ma4, ma7, ma8)
stargazer(modelsa, 
          se = list(ma1$rse, ma2$rse, ma5$rse, ma6$rse, ma3$rse, ma4$rse, ma7$rse, ma8$rse), 
          title = "Removing Outliers and Alternative Dependent Variable",
          dep.var.labels = c("(log) Highest First Year Lobbying Rev.", "(log) Total First Year Lobbying Rev.", "(log) Highest First Year Lobbying Rev. Alternate"),
          #keep = c("num_conn", "is_connected", "interact", "is_cmte", "ever_rep", "year_salary", "ever_senate", "senior_staff", "higher_ed"),
          covariate.labels = c("Number of Connections","House Connection", "Senate Connection","Staff-Office Connections", "Ever Committee Staff", "Republican", "Ever Senate Staff", "Legislative Staff", "Senior Staff", "Press Staff"),         
          omit.stat = c("f", "ser"),
          style="apsr"
          #add.lines = list(c("Firm Fixed Effects?", "No", "No", "No", "No", "Yes", "Yes"))
)



## Table 2C

ma1 <- felm(log(infl_rev) ~ log(num_conn) + is_cmte + ever_rep + ever_senate + ever_pos_legis + ever_pos_senior + ever_pos_press | first_lobby_yr + office_id, data=lobbyists_whole)
ma2 <- felm(log(infl_rev) ~ log(num_conn) + house_conn + senate_conn + is_cmte + ever_rep + ever_senate + ever_pos_legis + ever_pos_senior + ever_pos_press | first_lobby_yr + office_id, data=lobbyists_whole)

ma3 <- felm(log(infl_rev) ~ staff_offices + is_cmte + ever_rep + ever_senate + ever_pos_legis + ever_pos_senior + ever_pos_press | first_lobby_yr + office_id, data=lobbyists_whole)
ma4 <- felm(log(infl_rev) ~ staff_offices + house_conn + senate_conn + is_cmte + ever_rep + ever_senate + ever_pos_legis + ever_pos_senior + ever_pos_press | first_lobby_yr + office_id, data=lobbyists_whole)

ma1.1 <- felm(log(infl_rev) ~ log(num_conn) + is_cmte + ever_rep + ever_senate + ever_pos_legis + ever_pos_senior + ever_pos_press + as.factor(high_rank_maj) | first_lobby_yr, data=lobbyists_whole)
ma2.1 <- felm(log(infl_rev) ~ log(num_conn) + house_conn + senate_conn + is_cmte + ever_rep + ever_senate + ever_pos_legis + ever_pos_senior + ever_pos_press + as.factor(high_rank_maj) | first_lobby_yr, data=lobbyists_whole)

ma3.1 <- felm(log(infl_rev) ~ staff_offices + is_cmte + ever_rep + ever_senate + ever_pos_legis + ever_pos_senior + ever_pos_press + as.factor(high_rank_maj)| first_lobby_yr, data=lobbyists_whole)
ma4.1 <- felm(log(infl_rev) ~ staff_offices + house_conn + senate_conn + is_cmte + ever_rep + ever_senate + ever_pos_legis + ever_pos_senior + ever_pos_press + as.factor(high_rank_maj)| first_lobby_yr, data=lobbyists_whole)

ma5.1 <- felm(log(infl_rev) ~ log(num_conn) + is_cmte + ever_rep + ever_senate + ever_pos_legis + ever_pos_senior + ever_pos_press + as.factor(highest_rank)| first_lobby_yr, data=lobbyists_whole)
ma6.1 <- felm(log(infl_rev) ~ log(num_conn) + house_conn + senate_conn + is_cmte + ever_rep + ever_senate + ever_pos_legis + ever_pos_senior + ever_pos_press + as.factor(highest_rank)| first_lobby_yr, data=lobbyists_whole)


modelsa <- list(ma1, ma2, ma3, ma4, ma1.1, ma2.1, ma3.1, ma4.1, ma5.1, ma6.1)
stargazer(modelsa, 
          se = list(ma1$rse, ma2$rse, ma3$rse, ma4$rse, ma1.1$rse, ma2.1$rse, ma3.1$rse, ma4.1$rse, ma5.1$rse, ma6.1$rse), 
          title = "Robustness Check - Last Office Fixed-Effects",
          dep.var.labels = "(log) Highest First Year Lobbying Revenue",
          covariate.labels = c("Number of Connections","House Connection", "Senate Connection","Staff-Office Connections", "Ever Committee Staff", "Republican", "Ever Senate Staff", "Legislative Staff", "Senior Staff", "Press Staff", "Cmte. Chair", "Committee", "Power Cmte. Chair", "Power Cmte.", "Majority Power Cmte.", "Minority Power Cmte.", "Majority Rank & File"),         
          omit.stat = c("f", "ser"),
          style="apsr"
)


## Table 3C

ma <- felm(log(infl_rev) ~ log(num_conn) + is_cmte + ever_rep + ever_senate + ever_pos_legis + ever_pos_senior + ever_pos_press | first_lobby_yr, data=filter(lobbyists_whole, gender=="M"))

ma2 <- felm(log(infl_rev) ~ log(num_conn) + house_conn  + senate_conn + is_cmte + ever_rep + ever_senate + ever_pos_legis + ever_pos_senior + ever_pos_press | first_lobby_yr, data=filter(lobbyists_whole, gender=="M"))

ma3 <- felm(log(infl_rev) ~ log(num_conn) + is_cmte + ever_rep + ever_senate + ever_pos_legis + ever_pos_senior + ever_pos_press | first_lobby_yr, data=filter(lobbyists_whole, gender=="F"))

ma4 <- felm(log(infl_rev) ~ log(num_conn) + house_conn  + senate_conn + is_cmte + ever_rep + ever_senate + ever_pos_legis + ever_pos_senior + ever_pos_press | first_lobby_yr, data=filter(lobbyists_whole, gender=="F"))

stargazer(list(ma, ma2, ma3, ma4),
          se = list(ma$rse, ma2$rse, ma3$rse, ma4$rse),
          title = "Robustness Check -- Gender of Lobbyist",
          dep.var.labels = "(log) Highest First Year Lobbying Rev.",
          covariate.labels = c("Number of Connections","House Connection", "Senate Connection","Staff-Office Connections", "Ever Committee Staff", "Republican", "Ever Senate Staff", "Legislative Staff", "Senior Staff", "Press Staff"),
          style="apsr")



## Table 4C

ma5 <- felm(log(infl_rev) ~ log(num_conn) + is_cmte + ever_rep + ever_senate + ever_pos_legis + ever_pos_senior + ever_pos_press| first_lobby_yr + first_firm, data=lobbyists_whole)
ma6 <- felm(log(infl_rev) ~ log(num_conn) + house_conn + senate_conn + is_cmte + ever_rep + ever_senate + ever_pos_legis + ever_pos_senior + ever_pos_press | first_lobby_yr + first_firm, data=lobbyists_whole)

ma7 <- felm(log(infl_rev) ~ log(num_conn) + is_cmte + ever_rep + ever_senate + ever_pos_legis + ever_pos_senior + ever_pos_press| first_lobby_yr + total_offices, data=lobbyists_whole)
ma8 <- felm(log(infl_rev) ~ log(num_conn) + house_conn + senate_conn + is_cmte + ever_rep + ever_senate + ever_pos_legis + ever_pos_senior + ever_pos_press | first_lobby_yr + total_offices, data=lobbyists_whole)


modelsa <- list(ma5, ma6, ma7, ma8)
stargazer(modelsa, 
          se = list(ma1$rse, ma2$rse, ma3$rse, ma4$rse, ma5$rse, ma6$rse, ma7$rse, ma8$rse), 
          title = "Alternative Explanations for Predicting Lobbying Revenue",
          dep.var.labels = "(log) Highest First Year Lobbying Revenue",
          covariate.labels = c("Number of Connections","House Connection", "Senate Connection", "Ever Committee Staff", "Republican", "Ever Senate Staff", "Legislative Staff", "Senior Staff", "Press Staff"),         
          omit.stat = c("f", "ser"),
          style="apsr",
          add.lines = list(c("Fixed Effects?", "Firm + Year", "Firm + Year", "Offices + Year", "Offices + Year"))
)



## Eigenvector Centrality models

m2a <- felm(log(infl_rev) ~ ec + is_cmte + ever_rep + ever_senate + ever_pos_legis + ever_pos_senior + ever_pos_press | first_lobby_yr, data=lobbyists_whole)
m3a <- felm(log(infl_rev) ~ ec + log(num_conn) + is_cmte + ever_rep + ever_senate + ever_pos_legis + ever_pos_senior + ever_pos_press | first_lobby_yr, data=lobbyists_whole)
#removing outliers:
m2 <- felm(log(infl_rev) ~ ec + is_cmte + ever_rep + ever_senate + ever_pos_legis + ever_pos_senior + ever_pos_press | first_lobby_yr, data=filter(lobbyists_whole, ec<= .0101796))
m3 <- felm(log(infl_rev) ~ ec + log(num_conn) + is_cmte + ever_rep + ever_senate + ever_pos_legis + ever_pos_senior + ever_pos_press | first_lobby_yr, data=filter(lobbyists_whole, ec<= .0101796))
m4 <- felm(log(infl_rev) ~ ec + log(num_conn) + ec:log(num_conn) + is_cmte + ever_rep + ever_senate + ever_pos_legis + ever_pos_senior + ever_pos_press | first_lobby_yr, data=filter(lobbyists_whole, ec<= .0101796))

stargazer(list(m2a, m3a, m2, m3),
          se = list(m2a$rse, m3a$rse, m2$rse, m3$rse),
          title = "Eigenvector Centrality",
          dep.var.labels = "(log) Highest First Year Lobbying Rev.",
          covariate.labels = c("Centrality Score", "NUmber of Staff Connections", "Ever Committee Staff", "Republican", "Ever Senate Staff", "Legislative Staff", "Senior Staff", "Press Staff", "Centrality x Connections"),
          add.lines = list(c("Outliers Included?", "Yes", "Yes", "No", "No")),
          style="apsr")




########################
#### Visualizations ####
########################

## To generate predicted results I use Stata's `margins` command
## At this point, go to Stata and run the .do file to generate these .csvs of results
## The .do file is included on Dataverse. 
## To import the .dta file in Stata (lobbying_data_stata.dta), first change the directory to this file in the .do file.
## The CSVs must be saved manually in Stata (the output of the margins command)
## Instructions for this task are included in the .do file as comments.
## I have included pre-generated versions of these CSVs to facilitate replication.


## Figure 1 -- Revenue predicted by connections
no_conns_stata <- read.csv("stata_results_conns.csv")
no_conns_stata$X<-NULL
no_conns_stata$x <- seq(from = 0, to =6.34, by = .05)
no_conns_stata$x <- as.numeric(no_conns_stata$x)
no_conns_stata$num_conn <- exp(no_conns_stata$x)
no_conns_stata$pred_rev <- exp(no_conns_stata$Margin)
no_conns_stata$pred_ub <- exp(no_conns_stata$ub)
no_conns_stata$pred_lb <- exp(no_conns_stata$lb)

#pdf("staff_conns_plot2.pdf", width=6, height=4)
ggplot(filter(no_conns_stata, num_conn<404), aes(x=num_conn, y=pred_rev)) + geom_line(color="dodgerblue2") + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_ribbon(aes(ymin = pred_lb, ymax = pred_ub), alpha=0.2) +
  geom_rug(data=filter(lobbyists_whole, num_conn<404), aes(x = num_conn), inherit.aes=FALSE,col="darkred",alpha=.2) +
  geom_vline(aes(xintercept=70), linetype="dashed", size=.8, col="gray", alpha=.8) +
  scale_y_continuous(labels=dollar)+ 
  ylab("Lobbying Revenue") + xlab("Number of Connections") 

#dev.off()



## Figure 2: Staff Connections and Legislator Connections
staff_offices_stata <- read.csv("stata_staff_offices.csv")
staff_offices_stata$x <- staff_offices_stata$x -1
staff_offices_stata$pred_rev <- exp(staff_offices_stata$Margin)
staff_offices_stata$pred_lb <- exp(staff_offices_stata$lb)
staff_offices_stata$pred_ub <- exp(staff_offices_stata$ub)

#pdf("staff_office_conns.pdf", width=6, height=4)
ggplot(staff_offices_stata, aes(x=x, y=pred_rev)) + geom_line(color="dodgerblue2") + theme_bw() +
  geom_ribbon(aes(ymin = pred_lb, ymax = pred_ub), alpha=0.2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_y_continuous(labels=dollar)+ ylab("Lobbying Revenue") + xlab("Number of Staff-Office Connections") + theme(legend.position = "none") +
  geom_segment(aes(x = 0, xend = 30, y = 284452, yend = 284452) ,linetype="dotted", size=.8, col="black", alpha=.8)+
  geom_ribbon(aes(ymin = 271272.7, ymax = 298271.7), alpha=0.2) + xlim(0,30)
#dev.off()


### Bivariate correlation of connections and revenue:
## Figure 3b in online appendix
#re-organize factor levels for display purposes
lobbyists_whole$quartile <- factor(lobbyists_whole$quartile, levels = c("First", "Second", "Third", "Fourth"))
#pdf("revenue_box.pdf", width=6, height=4)
ggplot(lobbyists_whole, aes(x = quartile, y = log(infl_rev))) + 
  geom_boxplot() + theme_bw() + 
  scale_y_continuous(name="log(Lobbying Revenue)") + 
  scale_x_discrete(name="Staff Connections Quartile")
#dev.off()

