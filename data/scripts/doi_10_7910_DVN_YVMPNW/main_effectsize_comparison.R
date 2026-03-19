################################################################################
#Replication Materials for
#Explanation Giving Promotes Democratic Satisfaction Regardless of Respondent Partisanship
#
#Syntax produces:
#Figure 4 (Effect Size Comparisons)
#The Stata syntax file 'main_table1_table2.do' needs to be run first to produce
#the estimates that are plotted below
################################################################################

library(rio)
library(tidyverse)

#########################################################
#Democratic Satisfaction
#########################################################

################
#Variables
################

#ESS Expl. 
d_explain <- import("lincom_dem_std_explains.dta") %>%
  mutate(
    iv = "Explanations (ESS)", 
    outcome = "Democratic Satisfaction" )

#V-Dem Expl. 
d_reason <- import("lincom_dem_reason_std.dta") %>%
  mutate(
    iv = "Explanations (V-Dem)", 
    outcome = "Democratic Satisfaction" )

#Courts
d_courts <- import("lincom_dem_std_courts.dta") %>%
  mutate(
    iv = "Courts Treat All Same", 
    outcome = "Democratic Satisfaction" )

#Elections
d_elections <- import("lincom_dem_std_elections.dta") %>%
  mutate(
    iv = "Free Fair Elections", 
    outcome = "Democratic Satisfaction" )

#Econ Satisfaction
d_econ <- import("lincom_dem_std_econ.dta") %>%
  mutate(
    iv = "Econ Satisfaction", 
    outcome = "Democratic Satisfaction" )

#Social Trust
d_strust <- import("lincom_dem_std_socialtrust.dta") %>%
  mutate(
    iv = "Social Trust", 
    outcome = "Democratic Satisfaction" )

#Social Ladder
d_status <- import("lincom_dem_std_ladder.dta") %>%
  mutate(
    iv = "Social Status", 
    outcome = "Democratic Satisfaction" )

#Unemployment Ch. 
d_unemp <- import("lincom_dem_unemp_ch.dta") %>%
  mutate(
    iv = "Unemployment Change", 
    outcome = "Democratic Satisfaction" )

################
#Partisanship
################
d_opposing <- import("lincom_dem_opposing.dta") %>%
  rename(
    estimate = `_margin`, 
    stderr = `_se`, 
    z = `_statistic`, 
    p = `_pvalue`, 
    min95 = `_ci_lb`, 
    max95 = `_ci_ub`) %>%
  select(estimate, stderr, z, p, min95, max95) %>%
  mutate(
    iv = "Opposing - Co-Partisan", 
    outcome = "Democratic Satisfaction", 
    parm = NA, 
    label = NA)

################
#Combine
################
demsatis <- bind_rows(d_courts, d_econ, d_elections, d_explain, d_reason, d_status, d_strust, 
                      d_unemp, d_opposing) %>%
  mutate(
    explains = ifelse(iv %in% c("Explanations (V-Dem)", "Explanations (ESS)"), "Explanation Variables", "Controls"), 
    explains = factor(explains, levels=c("Explanation Variables", "Controls")))

#########################################################
#Government Satisfaction
#########################################################

################
#Variables
################

#ESS Expl. 
g_explain <- import("lincom_gov_std_explains.dta") %>%
  mutate(
    iv = "Explanations (ESS)", 
    outcome = "Gov Satisfaction" )

#V-Dem Expl. 
g_reason <- import("lincom_gov_reason_std.dta") %>%
  mutate(
    iv = "Explanations (V-Dem)", 
    outcome = "Gov Satisfaction" )

#Courts
g_courts <- import("lincom_gov_std_courts.dta") %>%
  mutate(
    iv = "Courts Treat All Same", 
    outcome = "Gov Satisfaction" )

#Elections
g_elections <- import("lincom_gov_std_elections.dta") %>%
  mutate(
    iv = "Free Fair Elections", 
    outcome = "Gov Satisfaction" )

#Econ Satisfaction
g_econ <- import("lincom_gov_std_econ.dta") %>%
  mutate(
    iv = "Econ Satisfaction", 
    outcome = "Gov Satisfaction" )

#Social Trust
g_strust <- import("lincom_gov_std_socialtrust.dta") %>%
  mutate(
    iv = "Social Trust", 
    outcome = "Gov Satisfaction" )

#Social Ladder
g_status <- import("lincom_gov_std_ladder.dta") %>%
  mutate(
    iv = "Social Status", 
    outcome = "Gov Satisfaction" )

#Unemployment Ch. 
g_unemp <- import("lincom_gov_unemp_ch.dta") %>%
  mutate(
    iv = "Unemployment Change", 
    outcome = "Gov Satisfaction" )

################
#Partisanship
################

g_opposing <- import("lincom_gov_opposing.dta") %>%
  rename(
    estimate = `_margin`, 
    stderr = `_se`, 
    z = `_statistic`, 
    p = `_pvalue`, 
    min95 = `_ci_lb`, 
    max95 = `_ci_ub`) %>%
  select(estimate, stderr, z, p, min95, max95) %>%
  mutate(
    iv = "Opposing - Co-Partisan", 
    outcome = "Gov Satisfaction", 
    parm = NA, 
    label = NA)

################
#Combine
################
govsatis <- bind_rows(g_courts, g_econ, g_elections, g_explain, g_reason, g_status, g_strust, 
                      g_unemp, g_opposing) %>%
  mutate(
    explains = ifelse(iv %in% c("Explanations (V-Dem)", "Explanations (ESS)"), "Explanation Variables", "Controls"), 
    explains = factor(explains, levels=c("Explanation Variables", "Controls")))

#########################################################
#Figures
#########################################################

comb <- bind_rows(demsatis, govsatis)

ggplot(comb, aes(x=estimate, y = iv)) + 
  geom_pointrange(aes(xmin=min95, xmax=max95)) + 
  facet_grid(explains ~ outcome, space="free_y", scales="free_y") + 
  geom_text(aes(label = round(estimate, 2)), vjust=-0.6) + 
  geom_vline(xintercept=0, linetype="dashed", color="red") + 
  labs(y = NULL, 
       x = "Difference in Satisfaction: 90pct on variable - 10pct") + 
  theme_bw(14) 


ggsave("figure4.png", 
       height=10, width=12)

ggsave("figure4.pdf", 
       height=10, width=12)

