#######
#######
####### Replication Data for: The Differential Impact of the Hong Kong National Security Law 
####### on Political Sensitivity Bias in Opinion Polls.
####### This file produces the tables and figures seen in the paper and appendix.
####### Last Updated: July 2023
#######
#######

#setup
rm(list = ls()) # clear workspace

need <- c("foreign", "readstata13", "tidyverse", "estimatr", "csvy","synthdid", "modelsummary",
          "devtools", "dplyr", "tidyr", "lubridate","fixest", "ggiplot") # list packages needed
have <- need %in% rownames(installed.packages()) # checks packages you have
if(any(!have)) install.packages(need[!have]) # install missing packages
invisible(lapply(need, library, character.only=T)) # load needed packages

#######################Pre-treatment tests for 2008################################
#Taiwan independence
setwd(gsub("/Taiwan_independence","",dirname(rstudioapi::getSourceEditorContext()$path)))
load("Taiwan_independence/merged.Rdata")
merge$date <- merge$Date[merge$period==merge$period & merge$name=="indep_Taiwan"]
merge <- subset(merge, period<=0)
merge$treated[merge$name=="indep_Taiwan" & merge$period>-30] <- 1
merge$probit <- qnorm((merge$proBeijing/200)+0.5)
setup = panel.matrices(merge, unit = "name", time = "date", outcome = "probit", treatment = "treated")
tau.hat.Taiwan = synthdid_estimate(setup$Y, setup$N0, setup$T0)

merge$omega [merge$name == merge$name [merge$treated==1][1]] <- 1 #start data cleaning for regression
T1 <- length(merge$name[merge$treated==1])
merge$lambda [merge$period %in% merge$period[merge$treated==1]] <- 1/T1

omega.df <- data.frame(name = rownames(attributes(tau.hat.Taiwan)$setup$Y)[-nrow(attributes(tau.hat.Taiwan)$setup$Y)], omega = attributes(tau.hat.Taiwan)$weights$omega)
lambda.df <- data.frame(date = as.Date(colnames(attributes(tau.hat.Taiwan)$setup$Y)[1:(length(colnames(attributes(tau.hat.Taiwan)$setup$Y))-T1)]), lambda = attributes(tau.hat.Taiwan)$weights$lambda)

merge <- merge(merge, omega.df, by = "name", all = TRUE, no.dups = TRUE)
merge$omega <- ifelse(is.na(merge$omega.x),merge$omega.y,merge$omega.x)
merge <- subset(merge, select = -c(omega.x,omega.y))

merge <- merge(merge, lambda.df, by = "date", all = TRUE, no.dups = TRUE)
merge$lambda <- ifelse(is.na(merge$lambda.x),merge$lambda.y,merge$lambda.x)
merge <- subset(merge, select = -c(lambda.x, lambda.y))

merge$weights <- merge$lambda * merge$omega
merge <- merge[order(merge$name,merge$period,decreasing=TRUE),]

lm_sdid_Taiwan <- feols(probit ~ treated | name + period, data = merge, weights = merge$weights,
                 panel.id = ~name + period, vcov="DK") #run the corresponding regression

#Tibet independence
setwd(gsub("/Tibet_independence","",dirname(rstudioapi::getSourceEditorContext()$path)))
load("Tibet_independence/merged.Rdata")
merge$date <- merge$Date[merge$period==merge$period & merge$name=="indep_Tibet"]
merge <- subset(merge, period<=0)
merge$treated[merge$name=="indep_Tibet" & merge$period>=-30] <- 1
merge$probit <- qnorm((merge$proBeijing/200)+0.5)
setup = panel.matrices(merge, unit = "name", time = "date", outcome = "probit", treatment = "treated")
tau.hat.Tibet = synthdid_estimate(setup$Y, setup$N0, setup$T0)

merge$omega [merge$name == merge$name [merge$treated==1][1]] <- 1 #start data cleaning for regression
T1 <- length(merge$name[merge$treated==1])
merge$lambda [merge$period %in% merge$period[merge$treated==1]] <- 1/T1

omega.df <- data.frame(name = rownames(attributes(tau.hat.Tibet)$setup$Y)[-nrow(attributes(tau.hat.Tibet)$setup$Y)], omega = attributes(tau.hat.Tibet)$weights$omega)
lambda.df <- data.frame(date = as.Date(colnames(attributes(tau.hat.Tibet)$setup$Y)[1:(length(colnames(attributes(tau.hat.Tibet)$setup$Y))-T1)]), lambda = attributes(tau.hat.Tibet)$weights$lambda)

merge <- merge(merge, omega.df, by = "name", all = TRUE, no.dups = TRUE)
merge$omega <- ifelse(is.na(merge$omega.x),merge$omega.y,merge$omega.x)
merge <- subset(merge, select = -c(omega.x,omega.y))

merge <- merge(merge, lambda.df, by = "date", all = TRUE, no.dups = TRUE)
merge$lambda <- ifelse(is.na(merge$lambda.x),merge$lambda.y,merge$lambda.x)
merge <- subset(merge, select = -c(lambda.x, lambda.y))

merge$weights <- merge$lambda * merge$omega
merge <- merge[order(merge$name,merge$period,decreasing=TRUE),]

lm_sdid_Tibet <- feols(probit ~ treated | name + period, data = merge, weights = merge$weights,
                        panel.id = ~name + period, vcov="DK") #run the corresponding regression

#June Fourth (Tiananmen Incident)
setwd(gsub("/64","",dirname(rstudioapi::getSourceEditorContext()$path)))
load("64/merged.Rdata")
merge$date <- merge$Date[merge$period==merge$period & merge$name=="64_students"]
merge <- subset(merge, period<=0)
merge$treated[merge$name %in% treat_list & merge$period>-12] <- 1
merge$probit <- qnorm((merge$proBeijing/200)+0.5)
setup = panel.matrices(merge, unit = "name", time = "date", outcome = "probit", treatment = "treated")
tau.hat.64 = synthdid_estimate(setup$Y, setup$N0, setup$T0)

merge$omega [merge$name %in% merge$name [merge$treated==1]] <- 1 #start data cleaning for regression
T1 <- length(merge$name[merge$treated==1])/3
merge$lambda [merge$period %in% merge$period[merge$treated==1]] <- 1/T1

omega.df <- data.frame(name = rownames(attributes(tau.hat.64)$setup$Y)[1:(nrow(attributes(tau.hat.64)$setup$Y)-3)], omega = attributes(tau.hat.64)$weights$omega)
lambda.df <- data.frame(date = as.Date(colnames(attributes(tau.hat.64)$setup$Y)[1:(length(colnames(attributes(tau.hat.64)$setup$Y))-T1)]), lambda = attributes(tau.hat.64)$weights$lambda)

merge <- merge(merge, omega.df, by = "name", all = TRUE, no.dups = TRUE)
merge$omega <- ifelse(is.na(merge$omega.x),merge$omega.y,merge$omega.x)
merge <- subset(merge, select = -c(omega.x,omega.y))

merge <- merge(merge, lambda.df, by = "date", all = TRUE, no.dups = TRUE)
merge$lambda <- ifelse(is.na(merge$lambda.x),merge$lambda.y,merge$lambda.x)
merge <- subset(merge, select = -c(lambda.x, lambda.y))

merge$weights <- merge$lambda * merge$omega
merge <- merge[order(merge$name,merge$period,decreasing=TRUE),]

lm_sdid_64 <- feols(probit ~ treated | name + period, data = merge, weights = merge$weights,
                 panel.id = ~name + period, vcov="DK") #run the corresponding regression

#Plot three results
regressions_2008 <- list("Taiwan.indep"= lm_sdid_Taiwan, "Tibet.indep"=lm_sdid_Tibet, "June Fourth Incident"=lm_sdid_64)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
save(regressions_2008, file="pretreatment/2008.Rdata") #save regression data

estimates = list(tau.hat.Taiwan, tau.hat.Tibet, tau.hat.64)
names(estimates) = c('Rejection of Taiwan independence', 'Rejection of Tibet independence', 'Support for Beijing on June Fourth incident')
synthdid_plot(estimates, facet.vertical=FALSE,
              control.name='Synthetic public opinion', treated.name='Actual public opinion',
              lambda.comparable=TRUE, se.method = 'none',
              trajectory.linetype = 1, line.width=.75, effect.curvature=-.4,
              trajectory.alpha=.7, effect.alpha=.7,
              diagram.alpha=1, onset.alpha=.7) +
  theme(legend.position=c(.23,.20), legend.direction='vertical',
        legend.key=element_blank(), legend.background=element_blank(),
  ) + xlab("Date") +ylab("Standardised public opinion") +ggtitle("Trend tests for pre and post-2008") #Figure 3
#######################Pre-treatment tests for 2008 ends################################

#######################Pre-treatment tests for 2015######################################
rm(list = ls()) # clear workspace
#Taiwan independence
setwd(gsub("/Taiwan_independence","",dirname(rstudioapi::getSourceEditorContext()$path)))
load("Taiwan_independence/merged.Rdata")
merge$date <- merge$Date[merge$period==merge$period & merge$name=="indep_Taiwan"]
merge <- subset(merge, period<=0)
merge$treated[merge$name=="indep_Taiwan" & merge$period>-30] <- 1
merge$probit <- qnorm((merge$proBeijing/200)+0.5)
merge <- subset(merge, period>-30)
merge$treated[merge$name=="indep_Taiwan" & merge$period<=-10] <- 0
setup = panel.matrices(merge, unit = "name", time = "date", outcome = "probit", treatment = "treated")
tau.hat.Taiwan = synthdid_estimate(setup$Y, setup$N0, setup$T0)

merge$omega [merge$name == merge$name [merge$treated==1][1]] <- 1 #start data cleaning for regression
T1 <- length(merge$name[merge$treated==1])
merge$lambda [merge$period %in% merge$period[merge$treated==1]] <- 1/T1

omega.df <- data.frame(name = rownames(attributes(tau.hat.Taiwan)$setup$Y)[-nrow(attributes(tau.hat.Taiwan)$setup$Y)], omega = attributes(tau.hat.Taiwan)$weights$omega)
lambda.df <- data.frame(date = as.Date(colnames(attributes(tau.hat.Taiwan)$setup$Y)[1:(length(colnames(attributes(tau.hat.Taiwan)$setup$Y))-T1)]), lambda = attributes(tau.hat.Taiwan)$weights$lambda)

merge <- merge(merge, omega.df, by = "name", all = TRUE, no.dups = TRUE)
merge$omega <- ifelse(is.na(merge$omega.x),merge$omega.y,merge$omega.x)
merge <- subset(merge, select = -c(omega.x,omega.y))

merge <- merge(merge, lambda.df, by = "date", all = TRUE, no.dups = TRUE)
merge$lambda <- ifelse(is.na(merge$lambda.x),merge$lambda.y,merge$lambda.x)
merge <- subset(merge, select = -c(lambda.x, lambda.y))

merge$weights <- merge$lambda * merge$omega
merge <- merge[order(merge$name,merge$period,decreasing=TRUE),]

lm_sdid_Taiwan <- feols(probit ~ treated | name + period, data = merge, weights = merge$weights,
                        panel.id = ~name + period, vcov="DK") #run the corresponding regression

#Tibet independence
setwd(gsub("/Tibet_independence","",dirname(rstudioapi::getSourceEditorContext()$path)))
load("Tibet_independence/merged.Rdata")
merge$date <- merge$Date[merge$period==merge$period & merge$name=="indep_Tibet"]
merge <- subset(merge, period<=0)
merge$treated[merge$name=="indep_Tibet" & merge$period>=-30] <- 1
merge$probit <- qnorm((merge$proBeijing/200)+0.5)
merge <- subset(merge, period>=-23)
merge$treated[merge$name=="indep_Tibet" & merge$period<(-10)] <- 0
setup = panel.matrices(merge, unit = "name", time = "date", outcome = "probit", treatment = "treated")
tau.hat.Tibet = synthdid_estimate(setup$Y, setup$N0, setup$T0)

merge$omega [merge$name == merge$name [merge$treated==1][1]] <- 1 #start data cleaning for regression
T1 <- length(merge$name[merge$treated==1])
merge$lambda [merge$period %in% merge$period[merge$treated==1]] <- 1/T1

omega.df <- data.frame(name = rownames(attributes(tau.hat.Tibet)$setup$Y)[-nrow(attributes(tau.hat.Tibet)$setup$Y)], omega = attributes(tau.hat.Tibet)$weights$omega)
lambda.df <- data.frame(date = as.Date(colnames(attributes(tau.hat.Tibet)$setup$Y)[1:(length(colnames(attributes(tau.hat.Tibet)$setup$Y))-T1)]), lambda = attributes(tau.hat.Tibet)$weights$lambda)

merge <- merge(merge, omega.df, by = "name", all = TRUE, no.dups = TRUE)
merge$omega <- ifelse(is.na(merge$omega.x),merge$omega.y,merge$omega.x)
merge <- subset(merge, select = -c(omega.x,omega.y))

merge <- merge(merge, lambda.df, by = "date", all = TRUE, no.dups = TRUE)
merge$lambda <- ifelse(is.na(merge$lambda.x),merge$lambda.y,merge$lambda.x)
merge <- subset(merge, select = -c(lambda.x, lambda.y))

merge$weights <- merge$lambda * merge$omega
merge <- merge[order(merge$name,merge$period,decreasing=TRUE),]

lm_sdid_Tibet <- feols(probit ~ treated | name + period, data = merge, weights = merge$weights,
                       panel.id = ~name + period, vcov="DK") #run the corresponding regression

#June Fourth (Tiananmen Incident)
setwd(gsub("/64","",dirname(rstudioapi::getSourceEditorContext()$path)))
load("64/merged.Rdata")
merge$date <- merge$Date[merge$period==merge$period & merge$name=="64_students"]
merge <- subset(merge, period<=0)
merge$treated[merge$name %in% treat_list & merge$period>-12] <- 1
merge$probit <- qnorm((merge$proBeijing/200)+0.5)
merge <- subset(merge, period>=-10)
merge$treated[merge$name %in% treat_list & merge$period<=-6] <- 0
setup = panel.matrices(merge, unit = "name", time = "date", outcome = "probit", treatment = "treated")
tau.hat.64 = synthdid_estimate(setup$Y, setup$N0, setup$T0)

merge$omega [merge$name %in% merge$name [merge$treated==1]] <- 1 #start data cleaning for regression
T1 <- length(merge$name[merge$treated==1])/3
merge$lambda [merge$period %in% merge$period[merge$treated==1]] <- 1/T1

omega.df <- data.frame(name = rownames(attributes(tau.hat.64)$setup$Y)[1:(nrow(attributes(tau.hat.64)$setup$Y)-3)], omega = attributes(tau.hat.64)$weights$omega)
lambda.df <- data.frame(date = as.Date(colnames(attributes(tau.hat.64)$setup$Y)[1:(length(colnames(attributes(tau.hat.64)$setup$Y))-T1)]), lambda = attributes(tau.hat.64)$weights$lambda)

merge <- merge(merge, omega.df, by = "name", all = TRUE, no.dups = TRUE)
merge$omega <- ifelse(is.na(merge$omega.x),merge$omega.y,merge$omega.x)
merge <- subset(merge, select = -c(omega.x,omega.y))

merge <- merge(merge, lambda.df, by = "date", all = TRUE, no.dups = TRUE)
merge$lambda <- ifelse(is.na(merge$lambda.x),merge$lambda.y,merge$lambda.x)
merge <- subset(merge, select = -c(lambda.x, lambda.y))

merge$weights <- merge$lambda * merge$omega
merge <- merge[order(merge$name,merge$period,decreasing=TRUE),]

lm_sdid_64 <- feols(probit ~ treated | name + period, data = merge, weights = merge$weights,
                    panel.id = ~name + period, vcov="DK") #run the corresponding regression

#Plot three results
regressions_2015 <- list("Taiwan.indep"= lm_sdid_Taiwan, "Tibet.indep"=lm_sdid_Tibet, "June Fourth Incident"=lm_sdid_64)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
save(regressions_2015, file="pretreatment/2015.Rdata") #save regression data

estimates = list(tau.hat.Taiwan, tau.hat.Tibet, tau.hat.64)
names(estimates) = c('Rejection of Taiwan independence', 'Rejection of Tibet independence', 'Support for Beijing on June Fourth incident')
synthdid_plot(estimates, facet.vertical=FALSE,
              control.name='Synthetic public opinion', treated.name='Actual public opinion',
              lambda.comparable=TRUE, se.method = 'none',
              trajectory.linetype = 1, line.width=.75, effect.curvature=-.4,
              trajectory.alpha=.7, effect.alpha=.7,
              diagram.alpha=1, onset.alpha=.7) +
  theme(legend.position=c(.09,.26), legend.direction='vertical',
        legend.key=element_blank(), legend.background=element_blank(),
  ) + xlab("Date") +ylab("Standardised public opinion") +ggtitle("Trend tests for pre and post-2015") #Figure 4
#######################Pre-treatment tests for 2015 ends######################################

######################Regression Table for pretreatment tests in Appendix E############################
rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
load("pretreatment/2008.Rdata") #read saved regression data
load("pretreatment/2015.Rdata")
pretreatment_reg <- list("2008"=regressions_2008, "2015"=regressions_2015)
glance_custom.fixest <- function(x, ...) {
  data.frame(
    'vcov.type'=ifelse(attributes(x$se)$type=="IID","Clustered by poll",attributes(x$se)),
    'fe.p'=ifelse("name" %in% names(x$fixef_id),'Yes','No'),
    'fe.t'=ifelse("period" %in% names(x$fixef_id),'Yes','No')
  )
}
gofmap <- list(list("raw" = "vcov.type", "clean" = "Std.Errors", "fmt" = 2),
               list("raw" = "fe.p", "clean" = "FE: Poll", "fmt" = NULL),
               list("raw" = "fe.t", "clean" = "FE: Time", "fmt" = NULL),
               list("raw" = "nobs", "clean" = "Effective N", "fmt" = 0),
               list("raw" = "r.squared", "clean" = "R2", "fmt" = 3),
               list("raw" = "r2.within.adjusted", "clean" = "R2 Adj.(within)", "fmt" = 3), 
               list("raw" = "rmse", "clean" = "RMSE", "fmt" = 2)) 
table <- modelsummary(pretreatment_reg, output="latex",
             shape="rbind", notes="Note: Effective observations for SC and SDID are smaller due to zero time and unit weights.",
             stars = c('*' = .1, '**' = .05, '***' = .01),
             title = "Regression result table for pre-treatment trend tests.", 
             coef_rename = c("treated" = "Placebo DTE"),  gof_map = gofmap) #Table in Appendix E