############################################################################################################################
########The wrong place at the wrong time? Territorial autonomy and conflict during regime transitions########
#####CEE-FSU regional analyses (marginal effects plots)#####
#####Andreas Juon & Daniel Bochsler 2023 (Comparative Political Studies)#####
############################################################################################################################

###Note: run file "juon_bochsler_cps_autonomy_ceefsu_analyses.R" before running this file
###Note: run file "juon_bochsler_cps_autonomy_ceefsu_iv.do" to obtain marginal effects for instrumental variables analyses

####################################################################################
#########1. Libraries#########
####################################################################################

library(ggplot2)
library(margins)
library(dotwhisker)

####################################################################################
#########2. Main models#########
####################################################################################

####a) margins
sd_onset_1989_main_me <- data.frame(margins_summary(sd_onset_1989_main, variables = c("terraut_1989"), vcov = cluster.vcov(sd_onset_1989_main, as.integer(subset(df_ceefsu, cee_fsu_transition == 1 & sd_l1 == 0)$cowcode))))
sd_onset_1989_main_me$term <- "SDM onset"
terraut_up_1989_main_me <- data.frame(margins_summary(terraut_up_1989_main, variables = c("terraut_1989"), vcov = cluster.vcov(terraut_up_1989_main, as.integer(subset(df_ceefsu, cee_fsu_transition == 1 & sd_l1 == 1 & sd_period_terraut_up_l1==0)$cowcode))))
terraut_up_1989_main_me$term <- "Concession"
sd_escalation_1989_main_me <- data.frame(margins_summary(sd_escalation_1989_main, variables = c("terraut_1989"), vcov = cluster.vcov(sd_escalation_1989_main, as.integer(subset(df_ceefsu, cee_fsu_transition == 1 & violsd_l1 == 0 & sd_l1 == 1)$cowcode))))
sd_escalation_1989_main_me$term <- "Escalation during SDM"
####b) combine
plot_data <- rbind(sd_onset_1989_main_me, terraut_up_1989_main_me, sd_escalation_1989_main_me)
plot_data$estimate <- plot_data$AME
plot_data$conf.low <- plot_data$AME - 1.645 * plot_data$SE
plot_data$conf.high <- plot_data$AME + 1.645 * plot_data$SE
plot_data$model <- 1
####c) construct plot
figure4 <- dwplot(plot_data,
                      vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2),
                      dot_args = list(shape = 19, colour = "black"), 
                      whisker_args = list(colour = "black", linetype = 1)) +
  theme_bw() + theme(plot.title = element_text(size=12)) + xlab("Coefficient estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  theme(plot.title.position = "plot",legend.position = "right", text=element_text(family="Times"), axis.title=element_text(size=9), legend.text=element_text(size=9), plot.title =element_text(size=11)) + 
  xlab("effect estimate") + ylab("outcome") +
  scale_x_continuous(labels=scales::percent_format(accuracy = 1))


####################################################################################
#########3. Robustness checks (appendix E)#########
####################################################################################

##########3.1 Different time windows I - all years between 1990 and 2017##########
####a) margins
r1a_sd_onset_1989_main_me <- data.frame(margins_summary(r1a_sd_onset_1989_main, variables = c("terraut_1989"), vcov = cluster.vcov(r1a_sd_onset_1989_main, as.integer(subset(df_ceefsu, sd_l1 == 0)$cowcode))))
r1a_sd_onset_1989_main_me$term <- "SDM onset"
r1a_terraut_up_1989_main_me <- data.frame(margins_summary(r1a_terraut_up_1989_main, variables = c("terraut_1989"), vcov = cluster.vcov(r1a_terraut_up_1989_main, as.integer(subset(df_ceefsu, sd_l1 == 1 & sd_period_terraut_up_l1==0)$cowcode))))
r1a_terraut_up_1989_main_me$term <- "Concession"
r1a_sd_escalation_1989_main_me <- data.frame(margins_summary(r1a_sd_escalation_1989_main, variables = c("terraut_1989"), vcov = cluster.vcov(r1a_sd_escalation_1989_main, as.integer(subset(df_ceefsu, violsd_l1 == 0 & sd_l1 == 1)$cowcode))))
r1a_sd_escalation_1989_main_me$term <- "Escalation during SDM"
####b) combine
plot_data <- rbind(r1a_sd_onset_1989_main_me, r1a_terraut_up_1989_main_me, r1a_sd_escalation_1989_main_me)
plot_data$estimate <- plot_data$AME
plot_data$conf.low <- plot_data$AME - 1.645 * plot_data$SE
plot_data$conf.high <- plot_data$AME + 1.645 * plot_data$SE
plot_data$model <- 1
####c) construct plot
figurea20 <- dwplot(plot_data,
               vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2),
               dot_args = list(shape = 19, colour = "black"), 
               whisker_args = list(colour = "black", linetype = 1)) +
  theme_bw() + theme(plot.title = element_text(size=12)) + xlab("Coefficient estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  theme(plot.title.position = "plot",legend.position = "right", text=element_text(family="Times"), axis.title=element_text(size=9), legend.text=element_text(size=9), plot.title =element_text(size=11)) + 
  xlab("effect estimate") + ylab("outcome") +
  scale_x_continuous(labels=scales::percent_format(accuracy = 1))

##########3.2 Different time windows II - immediate post-transition phase##########
####a) margins
r1b_sd_onset_1989_main_me <- data.frame(margins_summary(r1b_sd_onset_1989_main, variables = c("terraut_1989"), vcov = cluster.vcov(r1b_sd_onset_1989_main, as.integer(subset(df_ceefsu, year <= 1994 & sd_l1 == 0)$cowcode))))
r1b_sd_onset_1989_main_me$term <- "SDM onset"
r1b_terraut_up_1989_main_me <- data.frame(margins_summary(r1b_terraut_up_1989_main, variables = c("terraut_1989"), vcov = cluster.vcov(r1b_terraut_up_1989_main, as.integer(subset(df_ceefsu, year <= 1994 & sd_l1 == 1 & sd_period_terraut_up_l1==0)$cowcode))))
r1b_terraut_up_1989_main_me$term <- "Concession"
r1b_sd_escalation_1989_main_me <- data.frame(margins_summary(r1b_sd_escalation_1989_main, variables = c("terraut_1989"), vcov = cluster.vcov(r1b_sd_escalation_1989_main, as.integer(subset(df_ceefsu, year <= 1994 & violsd_l1 == 0 & sd_l1 == 1)$cowcode))))
r1b_sd_escalation_1989_main_me$term <- "Escalation during SDM"
####b) combine
plot_data <- rbind(r1b_sd_onset_1989_main_me, r1b_terraut_up_1989_main_me, r1b_sd_escalation_1989_main_me)
plot_data$estimate <- plot_data$AME
plot_data$conf.low <- plot_data$AME - 1.645 * plot_data$SE
plot_data$conf.high <- plot_data$AME + 1.645 * plot_data$SE
plot_data$model <- 1
####c) construct plot
figurea21 <- dwplot(plot_data,
               vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2),
               dot_args = list(shape = 19, colour = "black"), 
               whisker_args = list(colour = "black", linetype = 1)) +
  theme_bw() + theme(plot.title = element_text(size=12)) + xlab("Coefficient estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  theme(plot.title.position = "plot",legend.position = "right", text=element_text(family="Times"), axis.title=element_text(size=9), legend.text=element_text(size=9), plot.title =element_text(size=11)) + 
  xlab("effect estimate") + ylab("outcome") +
  scale_x_continuous(labels=scales::percent_format(accuracy = 1))

##########3.3 Only multi-ethnic states##########
####a) margins
r3a_sd_onset_1989_main_me <- data.frame(margins_summary(r3a_sd_onset_1989_main, variables = c("terraut_1989"), vcov = cluster.vcov(r3a_sd_onset_1989_main, as.integer(subset(df_ceefsu, nopluralitysum_l1 >= 0.1 & cee_fsu_transition == 1 & sd_l1 == 0)$cowcode))))
r3a_sd_onset_1989_main_me$term <- "SDM onset"
r3a_terraut_up_1989_main_me <- data.frame(margins_summary(r3a_terraut_up_1989_main, variables = c("terraut_1989"), vcov = cluster.vcov(r3a_terraut_up_1989_main, as.integer(subset(df_ceefsu, nopluralitysum_l1 >= 0.1 & cee_fsu_transition == 1 & sd_l1 == 1 & sd_period_terraut_up_l1==0)$cowcode))))
r3a_terraut_up_1989_main_me$term <- "Concession"
r3a_sd_escalation_1989_main_me <- data.frame(margins_summary(r3a_sd_escalation_1989_main, variables = c("terraut_1989"), vcov = cluster.vcov(r3a_sd_escalation_1989_main, as.integer(subset(df_ceefsu, nopluralitysum_l1 >= 0.1 & cee_fsu_transition == 1 & violsd_l1 == 0 & sd_l1 == 1)$cowcode))))
r3a_sd_escalation_1989_main_me$term <- "Escalation during SDM"
####b) combine
plot_data <- rbind(r3a_sd_onset_1989_main_me, r3a_terraut_up_1989_main_me, r3a_sd_escalation_1989_main_me)
plot_data$estimate <- plot_data$AME
plot_data$conf.low <- plot_data$AME - 1.645 * plot_data$SE
plot_data$conf.high <- plot_data$AME + 1.645 * plot_data$SE
plot_data$model <- 1
####c) construct plot
figurea22 <- dwplot(plot_data,
               vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2),
               dot_args = list(shape = 19, colour = "black"), 
               whisker_args = list(colour = "black", linetype = 1)) +
  theme_bw() + theme(plot.title = element_text(size=12)) + xlab("Coefficient estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  theme(plot.title.position = "plot",legend.position = "right", text=element_text(family="Times"), axis.title=element_text(size=9), legend.text=element_text(size=9), plot.title =element_text(size=11)) + 
  xlab("effect estimate") + ylab("outcome") +
  scale_x_continuous(labels=scales::percent_format(accuracy = 1))

##########3.4 Only FSU##########
####a) margins
r3b_sd_onset_1989_main_me <- data.frame(margins_summary(r3b_sd_onset_1989_main, variables = c("terraut_1989"), vcov = cluster.vcov(r3b_sd_onset_1989_main, as.integer(subset(df_ceefsu, FSU == 1 & cee_fsu_transition == 1 & sd_l1 == 0)$cowcode))))
r3b_sd_onset_1989_main_me$term <- "SDM onset"
r3b_terraut_up_1989_main_me <- data.frame(margins_summary(r3b_terraut_up_1989_main, variables = c("terraut_1989"), vcov = cluster.vcov(r3b_terraut_up_1989_main, as.integer(subset(df_ceefsu, FSU == 1 & cee_fsu_transition == 1 & sd_l1 == 1 & sd_period_terraut_up_l1==0)$cowcode))))
r3b_terraut_up_1989_main_me$term <- "Concession"
r3b_sd_escalation_1989_main_me <- data.frame(margins_summary(r3b_sd_escalation_1989_main, variables = c("terraut_1989"), vcov = cluster.vcov(r3b_sd_escalation_1989_main, as.integer(subset(df_ceefsu, FSU == 1 & cee_fsu_transition == 1 & violsd_l1 == 0 & sd_l1 == 1)$cowcode))))
r3b_sd_escalation_1989_main_me$term <- "Escalation during SDM"
####b) combine
plot_data <- rbind(r3b_sd_onset_1989_main_me, r3b_terraut_up_1989_main_me, r3b_sd_escalation_1989_main_me)
plot_data$estimate <- plot_data$AME
plot_data$conf.low <- plot_data$AME - 1.645 * plot_data$SE
plot_data$conf.high <- plot_data$AME + 1.645 * plot_data$SE
plot_data$model <- 1
####c) construct plot
figurea23 <- dwplot(plot_data,
               vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2),
               dot_args = list(shape = 19, colour = "black"), 
               whisker_args = list(colour = "black", linetype = 1)) +
  theme_bw() + theme(plot.title = element_text(size=12)) + xlab("Coefficient estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  theme(plot.title.position = "plot",legend.position = "right", text=element_text(family="Times"), axis.title=element_text(size=9), legend.text=element_text(size=9), plot.title =element_text(size=11)) + 
  xlab("effect estimate") + ylab("outcome") +
  scale_x_continuous(labels=scales::percent_format(accuracy = 1))

##########3.5 Only FSU + autonomy measured in 1936##########
####a) margins
r3b_sd_onset_1936_main_me <- data.frame(margins_summary(r3b_sd_onset_1936_main, variables = c("terraut_1936"), vcov = cluster.vcov(r3b_sd_onset_1936_main, as.integer(subset(df_ceefsu, FSU == 1 & cee_fsu_transition == 1 & sd_l1 == 0)$cowcode))))
r3b_sd_onset_1936_main_me$term <- "SDM onset"
r3b_terraut_up_1936_main_me <- data.frame(margins_summary(r3b_terraut_up_1936_main, variables = c("terraut_1936"), vcov = cluster.vcov(r3b_terraut_up_1936_main, as.integer(subset(df_ceefsu, FSU == 1 & cee_fsu_transition == 1 & sd_l1 == 1 & sd_period_terraut_up_l1==0)$cowcode))))
r3b_terraut_up_1936_main_me$term <- "Concession"
r3b_sd_escalation_1936_main_me <- data.frame(margins_summary(r3b_sd_escalation_1936_main, variables = c("terraut_1936"), vcov = cluster.vcov(r3b_sd_escalation_1936_main, as.integer(subset(df_ceefsu, FSU == 1 & cee_fsu_transition == 1 & violsd_l1 == 0 & sd_l1 == 1)$cowcode))))
r3b_sd_escalation_1936_main_me$term <- "Escalation during SDM"
####b) combine
plot_data <- rbind(r3b_sd_onset_1936_main_me, r3b_terraut_up_1936_main_me, r3b_sd_escalation_1936_main_me)
plot_data$estimate <- plot_data$AME
plot_data$conf.low <- plot_data$AME - 1.645 * plot_data$SE
plot_data$conf.high <- plot_data$AME + 1.645 * plot_data$SE
plot_data$model <- 1
####c) construct plot
figurea24 <- dwplot(plot_data,
               vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2),
               dot_args = list(shape = 19, colour = "black"), 
               whisker_args = list(colour = "black", linetype = 1)) +
  theme_bw() + theme(plot.title = element_text(size=12)) + xlab("Coefficient estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  theme(plot.title.position = "plot",legend.position = "right", text=element_text(family="Times"), axis.title=element_text(size=9), legend.text=element_text(size=9), plot.title =element_text(size=11)) + 
  xlab("effect estimate") + ylab("outcome") +
  scale_x_continuous(labels=scales::percent_format(accuracy = 1))

##########3.6 Controlling for autonomy loss##########
####a) margins
r2a_sd_onset_1989_main_me <- data.frame(margins_summary(r2a_sd_onset_1989_main, variables = c("terraut_1989"), vcov = cluster.vcov(r2a_sd_onset_1989_main, as.integer(subset(df_ceefsu, cee_fsu_transition == 1 & sd_l1 == 0)$cowcode))))
r2a_sd_onset_1989_main_me$term <- "SDM onset"
r2a_terraut_up_1989_main_me <- data.frame(margins_summary(r2a_terraut_up_1989_main, variables = c("terraut_1989"), vcov = cluster.vcov(r2a_terraut_up_1989_main, as.integer(subset(df_ceefsu, cee_fsu_transition == 1 & sd_l1 == 1 & sd_period_terraut_up_l1==0)$cowcode))))
r2a_terraut_up_1989_main_me$term <- "Concession"
r2a_sd_escalation_1989_main_me <- data.frame(margins_summary(r2a_sd_escalation_1989_main, variables = c("terraut_1989"), vcov = cluster.vcov(r2a_sd_escalation_1989_main, as.integer(subset(df_ceefsu, cee_fsu_transition == 1 & violsd_l1 == 0 & sd_l1 == 1)$cowcode))))
r2a_sd_escalation_1989_main_me$term <- "Escalation during SDM"
####b) combine
plot_data <- rbind(r2a_sd_onset_1989_main_me, r2a_terraut_up_1989_main_me, r2a_sd_escalation_1989_main_me)
plot_data$estimate <- plot_data$AME
plot_data$conf.low <- plot_data$AME - 1.645 * plot_data$SE
plot_data$conf.high <- plot_data$AME + 1.645 * plot_data$SE
plot_data$model <- 1
####c) construct plot
figurea25 <- dwplot(plot_data,
               vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2),
               dot_args = list(shape = 19, colour = "black"), 
               whisker_args = list(colour = "black", linetype = 1)) +
  theme_bw() + theme(plot.title = element_text(size=12)) + xlab("Coefficient estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  theme(plot.title.position = "plot",legend.position = "right", text=element_text(family="Times"), axis.title=element_text(size=9), legend.text=element_text(size=9), plot.title =element_text(size=11)) + 
  xlab("effect estimate") + ylab("outcome") +
  scale_x_continuous(labels=scales::percent_format(accuracy = 1))

##########3.6 Controlling for bargaining environment##########
####a) margins
r2b_sd_onset_1989_main_me <- data.frame(margins_summary(r2b_sd_onset_1989_main, variables = c("terraut_1989"), vcov = cluster.vcov(r2b_sd_onset_1989_main, as.integer(subset(df_ceefsu, cee_fsu_transition == 1 & sd_l1 == 0)$cowcode))))
r2b_sd_onset_1989_main_me$term <- "SDM onset"
r2b_terraut_up_1989_main_me <- data.frame(margins_summary(r2b_terraut_up_1989_main, variables = c("terraut_1989"), vcov = cluster.vcov(r2b_terraut_up_1989_main, as.integer(subset(df_ceefsu, cee_fsu_transition == 1 & sd_l1 == 1 & sd_period_terraut_up_l1==0)$cowcode))))
r2b_terraut_up_1989_main_me$term <- "Concession"
r2b_sd_escalation_1989_main_me <- data.frame(margins_summary(r2b_sd_escalation_1989_main, variables = c("terraut_1989"), vcov = cluster.vcov(r2b_sd_escalation_1989_main, as.integer(subset(df_ceefsu, cee_fsu_transition == 1 & violsd_l1 == 0 & sd_l1 == 1)$cowcode))))
r2b_sd_escalation_1989_main_me$term <- "Escalation during SDM"
####b) combine
plot_data <- rbind(r2b_sd_onset_1989_main_me, r2b_terraut_up_1989_main_me, r2b_sd_escalation_1989_main_me)
plot_data$estimate <- plot_data$AME
plot_data$conf.low <- plot_data$AME - 1.645 * plot_data$SE
plot_data$conf.high <- plot_data$AME + 1.645 * plot_data$SE
plot_data$model <- 1
####c) construct plot
figurea26 <- dwplot(plot_data,
               vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2),
               dot_args = list(shape = 19, colour = "black"), 
               whisker_args = list(colour = "black", linetype = 1)) +
  theme_bw() + theme(plot.title = element_text(size=12)) + xlab("Coefficient estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  theme(plot.title.position = "plot",legend.position = "right", text=element_text(family="Times"), axis.title=element_text(size=9), legend.text=element_text(size=9), plot.title =element_text(size=11)) + 
  xlab("effect estimate") + ylab("outcome") +
  scale_x_continuous(labels=scales::percent_format(accuracy = 1))

##########3.7 Controlling for SDM years before 1989##########
####a) margins
r2c_sd_onset_1989_main_me <- data.frame(margins_summary(r2c_sd_onset_1989_main, variables = c("terraut_1989"), vcov = cluster.vcov(r2c_sd_onset_1989_main, as.integer(subset(df_ceefsu, cee_fsu_transition == 1 & sd_l1 == 0)$cowcode))))
r2c_sd_onset_1989_main_me$term <- "SDM onset"
r2c_terraut_up_1989_main_me <- data.frame(margins_summary(r2c_terraut_up_1989_main, variables = c("terraut_1989"), vcov = cluster.vcov(r2c_terraut_up_1989_main, as.integer(subset(df_ceefsu, cee_fsu_transition == 1 & sd_l1 == 1 & sd_period_terraut_up_l1==0)$cowcode))))
r2c_terraut_up_1989_main_me$term <- "Concession"
r2c_sd_escalation_1989_main_me <- data.frame(margins_summary(r2c_sd_escalation_1989_main, variables = c("terraut_1989"), vcov = cluster.vcov(r2c_sd_escalation_1989_main, as.integer(subset(df_ceefsu, cee_fsu_transition == 1 & violsd_l1 == 0 & sd_l1 == 1)$cowcode))))
r2c_sd_escalation_1989_main_me$term <- "Escalation during SDM"
####b) combine
plot_data <- rbind(r2c_sd_onset_1989_main_me, r2c_terraut_up_1989_main_me, r2c_sd_escalation_1989_main_me)
plot_data$estimate <- plot_data$AME
plot_data$conf.low <- plot_data$AME - 1.645 * plot_data$SE
plot_data$conf.high <- plot_data$AME + 1.645 * plot_data$SE
plot_data$model <- 1
####c) construct plot
figurea27 <- dwplot(plot_data,
               vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2),
               dot_args = list(shape = 19, colour = "black"), 
               whisker_args = list(colour = "black", linetype = 1)) +
  theme_bw() + theme(plot.title = element_text(size=12)) + xlab("Coefficient estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  theme(plot.title.position = "plot",legend.position = "right", text=element_text(family="Times"), axis.title=element_text(size=9), legend.text=element_text(size=9), plot.title =element_text(size=11)) + 
  xlab("effect estimate") + ylab("outcome") +
  scale_x_continuous(labels=scales::percent_format(accuracy = 1))

##########3.8 Instrumenting for autonomy with autonomy in 1989##########
#see Stata script for calculation of marginal effects
#########IV approach
####a) margins
iv_sd_onset_1989_main_me <- data.frame(term = "SDM onset", estimate = .3929064, conf.low = .0916371, conf.high = .6941758)
iv_terraut_up_1989_main_me <- data.frame(term = "Concession", estimate = .0486404, conf.low = -.1413928, conf.high = .2386736)
iv_sd_escalation_1989_main_me <- data.frame(term = "Escalation during SDM", estimate = .0650236, conf.low = .0168474, conf.high = .1131998)
####b) combine
plot_data <- rbind(iv_sd_onset_1989_main_me, iv_terraut_up_1989_main_me, iv_sd_escalation_1989_main_me)
plot_data$model <- 1
####c) construct plot
figurea29 <- dwplot(plot_data,
               vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2),
               dot_args = list(shape = 19, colour = "black"), 
               whisker_args = list(colour = "black", linetype = 1)) +
  theme_bw() + theme(plot.title = element_text(size=12)) + xlab("Coefficient estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  theme(plot.title.position = "plot",legend.position = "right", text=element_text(family="Times"), axis.title=element_text(size=9), legend.text=element_text(size=9), plot.title =element_text(size=11)) + 
  xlab("effect estimate") + ylab("outcome") +
  scale_x_continuous(labels=scales::percent_format(accuracy = 1))

##########3.9 Probing the impact of SDMs on changes in autonomy in pre-transition period##########
####a) margins
concession_pre_cont_me <- data.frame(margins_summary(concession_pre_cont, variables = c("sd_l1"), vcov = cluster.vcov(concession_pre_cont, as.integer(df_ceefsu_before$cowcode))))
concession_pre_cont_me$term <- "Δ Autonomy, full sample 1946-1988"
concession_pre_cont_fsu_me <- data.frame(margins_summary(concession_pre_cont_fsu, variables = c("sd_l1")))
concession_pre_cont_fsu_me$term <- "Δ Autonomy, Soviet Union 1946-1988"
####b) combine
plot_data <- rbind(concession_pre_cont_me, concession_pre_cont_fsu_me)
plot_data$estimate <- plot_data$AME
plot_data$conf.low <- plot_data$AME - 1.645 * plot_data$SE
plot_data$conf.high <- plot_data$AME + 1.645 * plot_data$SE
plot_data$model <- 1
####c) construct plot
figurea30 <- dwplot(plot_data,
               vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2),
               dot_args = list(shape = 19, colour = "black"), 
               whisker_args = list(colour = "black", linetype = 1)) +
  theme_bw() + theme(plot.title = element_text(size=12)) + xlab("Coefficient estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  theme(plot.title.position = "plot",legend.position = "right", text=element_text(family="Times"), axis.title=element_text(size=9), legend.text=element_text(size=9), plot.title =element_text(size=11)) + 
  xlab("effect estimate") + ylab("outcome")

##########3.10 Reverse estimation##########
####a) margins
sd_onset_1989_placebo_me <- data.frame(margins_summary(sd_onset_1989_placebo, variables = c("sd_onsets_sum_after1989"), vcov = cluster.vcov(sd_onset_1989_placebo, as.integer(subset(df_ceefsu, year <= 2017 & year == year_group_start)$cowcode))))
sd_onset_1989_placebo_me$term <- "# SDM onsets after 1989"
sd_escalation_1989_placebo_me <- data.frame(margins_summary(sd_escalation_1989_placebo, variables = c("sd_escalation_sum_after1989"), vcov = cluster.vcov(sd_escalation_1989_placebo, as.integer(subset(df_ceefsu, year <= 2017 & year == year_group_start)$cowcode))))
sd_escalation_1989_placebo_me$term <- "# Violent SDM onsets after 1989"
####b) combine
plot_data <- rbind(sd_onset_1989_placebo_me, sd_escalation_1989_placebo_me)
plot_data$estimate <- plot_data$AME
plot_data$conf.low <- plot_data$AME - 1.645 * plot_data$SE
plot_data$conf.high <- plot_data$AME + 1.645 * plot_data$SE
plot_data$model <- 1
####c) construct plot
figurea31 <- dwplot(plot_data,
               vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2),
               dot_args = list(shape = 19, colour = "black"), 
               whisker_args = list(colour = "black", linetype = 1)) +
  theme_bw() + theme(plot.title = element_text(size=12)) + xlab("Coefficient estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  theme(plot.title.position = "plot",legend.position = "right", text=element_text(family="Times"), axis.title=element_text(size=9), legend.text=element_text(size=9), plot.title =element_text(size=11)) + 
  xlab("effect estimate") + ylab("predictor")




