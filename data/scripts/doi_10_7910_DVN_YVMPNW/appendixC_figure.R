################################################################################
#Replication Materials for
#Explanation Giving Promotes Democratic Satisfaction Regardless of Respondent Partisanship
#
#Syntax produces:
#Figure OC1
#The Stata syntax file 'appendixC.do' needs to be run first to produce
#the estimates that are plotted below
################################################################################

rm(list = ls())

library(ggpubr)
library(cowplot)
library(rio)
library(tidyverse)
library(ggstance)

###############################################
#Data (from Stata output)
###############################################


###############################
#Top: Marginal effect of explanation giving by partisanship
###############################


dfunc1 <- function(x) {
  x %>%
    dplyr::rename(
      ame = `_margin`, 
      lower = `_ci_lb`, 
      upper = `_ci_ub`, 
      pid = `_by1`) %>%
    mutate(pid = factor(pid, 
                        levels=c(1,2,3), 
                        labels=c("Co-Partisan", 
                                 "Opposing Partisan", 
                                 "Non-Partisan"))) %>%
    select(ame, lower, upper, pid)
  
}


m3_dydx_expl <- import("tpol_explain_dydx_m3.dta") %>%
  dfunc1() %>%
  mutate(outcome = "Trust in Politicians", 
         expl = "Explanations (ESS)")

m3_dydx_reason <- import("tpol_reason_dydx_m3.dta") %>%
  dfunc1() %>%
  mutate(outcome = "Trust in Politicians", 
         expl = "Explanations (V-DEM")


###############################
#Middle: Predicted values (explanations on x-axis)
###############################


pfunc1 <- function(x) {
  x %>%
    dplyr::rename(
      predval = `_margin`, 
      lower = `_ci_lb`, 
      upper = `_ci_ub`, 
      expl = `_at1`, 
      pid = `_m1`)  %>%
    mutate(pid = factor(pid, 
                        levels=c(1,2,3), 
                        labels=c("Co-Partisan", 
                                 "Opposing Partisan", 
                                 "Non-Partisan"))) %>%
    select(predval, lower, upper, expl, pid)
}

pfunc2 <- function(x) {
  x %>%
    dplyr::rename(
      predval = `_margin`, 
      lower = `_ci_lb`, 
      upper = `_ci_ub`, 
      expl = `_at3`, 
      pid = `_m1`)  %>%
    mutate(pid = factor(pid, 
                        levels=c(1,2,3), 
                        labels=c("Co-Partisan", 
                                 "Opposing Partisan", 
                                 "Non-Partisan"))) %>%
    select(predval, lower, upper, expl, pid)
}

m3_pred_pid_ess <- import("tpol_explain_pred_m3.dta") %>%
  pfunc1() %>%
  mutate(outcome = "Trust in Politicians", 
         moderator = "Explanations (ESS)")


m3_pred_pid_vdem <- import("tpol_reason_pred_m3.dta") %>%
  pfunc2() %>%
  mutate(outcome = "Trust in Politicians", 
         moderator = "Explanations (V-DEM)")


###############################
#Bottom: Predicted values (explanations on x-axis)
###############################


dy_func1 <- function(x) {
  x %>%
    dplyr::rename(
      ame = `_margin`, 
      lower = `_ci_lb`, 
      upper = `_ci_ub`, 
      expl = `_at1`, 
      pid = `_deriv`) %>%
    mutate(pid = factor(pid, 
                        levels=c(2,3), 
                        labels=c("Opposing Partisan", 
                                 "Non-Partisan"))) %>%
    select(ame, lower, upper, expl, pid)
}

dy_func2 <- function(x) {
  x %>%
    dplyr::rename(
      ame = `_margin`, 
      lower = `_ci_lb`, 
      upper = `_ci_ub`, 
      expl = `_at3`, 
      pid = `_deriv`) %>%
    mutate(pid = factor(pid, 
                        levels=c(2,3), 
                        labels=c("Opposing Partisan", 
                                 "Non-Partisan"))) %>%
    select(ame, lower, upper, expl, pid)
}


##PID by ESS Explain
m3_dydx_pid_ess <- import("tpol_cop_explain_dydx_m3.dta") %>%
  dy_func1() %>%
  mutate(outcome = "Trust in Politicians", 
         moderator = "Explanations (ESS)")

##PID by V-DEM Explain
m3_dydx_pid_vdem <- import("tpol_cop_reason_dydx_m3.dta") %>%
  dy_func2() %>%
  mutate(outcome = "Trust in Politicians", 
         moderator = "Explanations (V-DEM)")

###############################################
#Figure
###############################################

#####G1: DYDX Expl. By PID 

g1 <- ggplot(m3_dydx_expl, aes(x=ame, y=pid)) + 
  geom_pointrangeh(aes(xmin=lower, xmax=upper)) + 
  theme_bw() + 
  labs(title = "AME for Explanations (ESS) by PID", 
       y = NULL, x = "AME") + 
  geom_text(aes(label=round(ame, 2)), 
            nudge_y=0.1)

#####G2: DYDX Reason. By PID 

g2 <- ggplot(m3_dydx_reason, aes(x=ame, y=pid)) + 
  geom_pointrangeh(aes(xmin=lower, xmax=upper)) + 
  theme_bw() + 
  labs(title = "AME for Explanations (V-DEM) by PID", 
       y = NULL, x = "AME") + 
  geom_text(aes(label=round(ame, 2)), 
            nudge_y=0.1) + 
  geom_vline(xintercept=0, linetype="dashed", color="red")


#####P3: PID Pred by Explanation 

g3 <- ggplot(m3_pred_pid_ess, 
             aes(x=expl, y=predval, shape=pid, linetype=pid)) + 
  geom_pointrange(aes(ymin=lower, ymax=upper), 
                  position = position_dodge(width=0.1)) + 
  geom_line(position = position_dodge(width=0.1)) + 
  theme_bw() + 
  labs(title = "Predicted Values by ESS Explanations", 
       x = "Explanations (ESS)", y="Predicted Value") + 
  theme(legend.position=c(0.05,0.8), 
        legend.title = element_blank()) +
  guides(shape = guide_legend(nrow=1))

#####P4: PID Pred by Reason 

g4 <- ggplot(m3_pred_pid_vdem, 
             aes(x=expl, y=predval, shape=pid, linetype=pid)) + 
  geom_pointrange(aes(ymin=lower, ymax=upper), 
                  position = position_dodge(width=0.1)) + 
  geom_line(position = position_dodge(width=0.1)) + 
  theme_bw() + 
  labs(title = "Predicted Values by V-DEM Explanations", 
       x = "Explanations (ESS)", y="Predicted Value") + 
  theme(legend.position=c(0.05,0.92), 
        legend.title = element_blank()) +
  guides(shape = guide_legend(nrow=1))


#####P5: DYDX PID. By Explanation 

g5 <- ggplot(m3_dydx_pid_ess, 
             aes(x=expl, y=ame, shape = pid, linetype = pid)) + 
  geom_pointrange(aes(ymin=lower, ymax=upper), 
                  position = position_dodge(width=0.1)) + 
  geom_line(position = position_dodge(width=0.1)) + 
  theme_bw() + 
  labs(title = "Diff from Co-Partisans by ESS Explanations", 
       x = "Explanations (ESS)", y="Diff from Co-Partisans") + 
  theme(legend.position=c(0.15,0.92), 
        legend.title = element_blank()) + 
  geom_hline(yintercept=0, linetype="dashed", color="red") + 
  guides(shape = guide_legend(nrow=1))



#####P6: DYDX PID. By Reason 
g6 <- ggplot(m3_dydx_pid_vdem, 
             aes(x=expl, y=ame, shape = pid, linetype = pid)) + 
  geom_pointrange(aes(ymin=lower, ymax=upper), 
                  position = position_dodge(width=0.1)) + 
  geom_line(position = position_dodge(width=0.1)) + 
  theme_bw() + 
  labs(title = "Diff from Co-Partisans by V-DEM Explanations", 
       x = "Explanations (V-DEM)", y="Diff from Co-Partisans") + 
  theme(legend.position=c(0.15,0.92), 
        legend.title = element_blank()) + 
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  guides(shape = guide_legend(nrow=1))


#######Combined Figure

g3a <- lemon::reposition_legend(g3, 'top left', x=0.002, y=1-0.002)
g4a <- lemon::reposition_legend(g4, 'bottom right', y=0.002)
g5a <- lemon::reposition_legend(g5, 'top right',y=1-.002)
g6a <- lemon::reposition_legend(g6, 'top left', x=0.002, y=1-0.002)

ggarrange(g1,g2,g3a,g4a,g5a,g6a, 
          nrow=3, ncol=2, 
          labels=c("A", "B", "C", "D", "E", "F"))


ggsave("figure_oc1.png", height=14, width=12, dpi=600)

#######################
#Remove data objects
#######################
rm(list = ls())
