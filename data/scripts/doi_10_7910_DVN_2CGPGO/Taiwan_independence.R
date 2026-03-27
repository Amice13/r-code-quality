#######
#######
####### Replication Data for: The Differential Impact of the Hong Kong National Security Law 
####### on Political Sensitivity Bias in Opinion Polls.
####### This file produces the merged data, tables and figures seen in the paper and appendix.
####### Last Updated: August. 2023
#######
#######

#setup
rm(list = ls()) # clear workspace
setwd(gsub("/Taiwan_independence","",dirname(rstudioapi::getSourceEditorContext()$path)))
list.files()
######## PACKAGES ########

# Check system and installs packages user doesn't have, load needed packages
# install.packages("remotes")
#remotes::install_github("grantmcdermott/ggiplot") #install ggiplot
  #devtools::install_github("synth-inference/synthdid")
need <- c("foreign", "readstata13", "tidyverse", "estimatr", "csvy","synthdid", "modelsummary",
          "devtools", "dplyr", "tidyr", "lubridate","fixest", "ggiplot") # list packages needed
have <- need %in% rownames(installed.packages()) # checks packages you have
if(any(!have)) install.packages(need[!have]) # install missing packages
invisible(lapply(need, library, character.only=T)) # load needed packages
source("custom_fuctions/plot.r")

##########################merge treated data with control##################
#code treated unit period
load('cleaned data/indep_Taiwan.Rdata')
a <- subset(a, select=c(Date, proBeijing, sensitivity))
a$name <- "indep_Taiwan"
indep_Taiwan <- a
rm(a)
  Treatment.date=as.Date("2020-06-30")         #set treatment date
  indep_Taiwan$Date = as.Date(indep_Taiwan$Date)
  indep_Taiwan$treated <- ifelse(indep_Taiwan$Date>Treatment.date,1,0)
  a <- 1
  while (indep_Taiwan$Date[a]>Treatment.date) {
    a=a+1
  }
  indep_Taiwan$period <- a - seq_len(nrow(indep_Taiwan))
cat("Treated unit:","indep_Taiwan","\nTtr=",a-1,"\nTun=",a-nrow(indep_Taiwan)+1)
indep_Taiwan <- indep_Taiwan[1:72,]          #code start date=2000
merge <- indep_Taiwan

#function to code period   NOTE: MANUALLY CHANGE THE TIME PERIODS!
code_period <- function(data) {
  data$period <- NA
  closest_indices <- integer(nrow(indep_Taiwan))
  
  for (i in 1:nrow(indep_Taiwan)) {
    ref_date <- indep_Taiwan$Date[i]
    ref_period <- indep_Taiwan$period[i]
    
    # Calculate the absolute difference between the ref_date and the dates in the data
    date_diffs <- abs(as.numeric(data$Date - ref_date))
    
    # Find the index of the minimum difference
    min_diff_index <- which.min(date_diffs)
    
    # Store the index of the minimum difference
    closest_indices[i] <- min_diff_index
  }
  
  # Create a separate data frame with the closest dates and their corresponding periods
  closest_data <- data[closest_indices,]
  closest_data$period <- indep_Taiwan$period
  
  return(closest_data)
}
#end function

#load and code donor pool: medium and low sensitivity only
data.list <- gsub(".Rdata","",list.files("cleaned data",pattern = "\\.Rdata$"))
for (name in data.list){
  load (paste0("cleaned data/",name,".Rdata"))
  if (name == "indep_Taiwan" | a$sensitivity[1]=="high"| a$sensitivity[1]=="extremely high"| a$Date[nrow(a)]>indep_Taiwan$Date[nrow(indep_Taiwan)]){next}
  a <- subset(a, select=c(Date, proBeijing, sensitivity))
  a$name <- name
  a$treated <- 0
  a <- code_period(a)
  merge <- rbind(merge, a)
}
#save(indep_Taiwan,merge, file = "Taiwan_independence/merged.Rdata")
#########################merge ends#####################################

load("Taiwan_independence/merged.Rdata")
#if you want to shorten the time period, do it here
merge <- subset(merge, period>=-20) #exclude data before 2011

#non-standardized demo
setup = panel.matrices(merge, unit = "name", time = "period", outcome = "proBeijing", treatment = "treated")
tau.hat = synthdid_estimate(setup$Y, setup$N0, setup$T0)
plot <- plot(tau.hat, treated.name = "rejection of Taiwan independence", control.name = "synthetic rejection")
plot + xlab("periods") +ylab("proBeijing")

synthdid_effect_curve(tau.hat)
synthdid_rmse_plot(tau.hat)
synthdid_units_plot(
  tau.hat,
  negligible.threshold = 0.001,
  negligible.alpha = 0.3,
  se.method = "none",
  units = NULL
)


tau.hat1 = sc_estimate(setup$Y, setup$N0, setup$T0)
plot(tau.hat1)
tau.hat2 = did_estimate(setup$Y, setup$N0, setup$T0)
plot(tau.hat2)

top.controls = synthdid_controls(tau.hat)#[1:10, , drop=FALSE]
plot(tau.hat, spaghetti.units=rownames(top.controls))
#demo ends



#Date correction
merge$date <- merge$Date[merge$period==merge$period & merge$name=="indep_Taiwan"]
#use probit
merge$probit <- qnorm((merge$proBeijing/200)+0.5)
setup = panel.matrices(merge, unit = "name", time = "date", outcome = "probit", treatment = "treated")
tau.hat = synthdid_estimate(setup$Y, setup$N0, setup$T0)
plot <- plot(tau.hat, treated.name = "rejection of Taiwan independence", control.name = "synthetic rejection")
plot + xlab("periods") +ylab("standardised public opinion")

synthdid_effect_curve(tau.hat)
synthdid_rmse_plot(tau.hat)
synthdid_units_plot(
  list(SDID=tau.hat),
  negligible.threshold = 0.001,
  negligible.alpha = 0.3,
  se.method = "none",
  units = NULL
)

top.controls = synthdid_controls(tau.hat)#[1:10, , drop=FALSE]
plot(tau.hat, spaghetti.units=rownames(top.controls))

#probit regression
#merge <- subset(merge, select = -c(omega,lambda, weights))
merge$omega [merge$name == merge$name [merge$treated==1][1]] <- 1
T1 <- length(merge$name[merge$treated==1])
merge$lambda [merge$period %in% merge$period[merge$treated==1]] <- 1/T1

omega.df <- data.frame(name = rownames(attributes(tau.hat)$setup$Y)[-nrow(attributes(tau.hat)$setup$Y)], omega = attributes(tau.hat)$weights$omega)
lambda.df <- data.frame(date = as.Date(colnames(attributes(tau.hat)$setup$Y)[1:(length(colnames(attributes(tau.hat)$setup$Y))-T1)]), lambda = attributes(tau.hat)$weights$lambda)

merge <- merge(merge, omega.df, by = "name", all = TRUE, no.dups = TRUE)
merge$omega <- ifelse(is.na(merge$omega.x),merge$omega.y,merge$omega.x)
merge <- subset(merge, select = -c(omega.x,omega.y))

merge <- merge(merge, lambda.df, by = "date", all = TRUE, no.dups = TRUE)
merge$lambda <- ifelse(is.na(merge$lambda.x),merge$lambda.y,merge$lambda.x)
merge <- subset(merge, select = -c(lambda.x, lambda.y))

merge$weights <- merge$lambda * merge$omega
merge <- merge[order(merge$name,merge$period,decreasing=TRUE),]

lm_sdid <- feols(probit ~ treated | name + period, data = merge, weights = merge$weights,
               panel.id = ~name + period, vcov="DK")
lm_sdid_new <- feols(probit ~ treated | name + period, data = merge, weights = merge$weights,
                 panel.id = ~name + period, vcov="newey_west")
lm_sdid_clustered <- feols(probit ~ treated | name + period, data = merge, weights = merge$weights,
                 panel.id = ~name + period)
summary(lm_sdid_clustered)

#event study style plot #make sure the last weights calculation was for sdid
merge$time_to_treat = ifelse(merge$name=="indep_Taiwan" & merge$period>0, as.character(merge$date), '2020-01-03')
merge$treatment = ifelse(merge$name=="indep_Taiwan", 1, 0)
lm_ev <- feols(probit ~ i(time_to_treat, treated, ref='2020-01-03')| name + period,data = merge, weights = merge$weights,
               panel.id = ~name + period)
iplot(lm_ev, ci_level = 0.95,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)') #an old-styled plot
ggiplot(lm_ev, ci_level = c(0.95, 0.90),geom_style = 'ribbon',
        xlab = 'Time to treatment',
        main = 'Event study style plot: Differential treatment effect over time \nPoll: Rejection of Taiwan independence',
        theme = theme_minimal() +
          theme(
            plot.title = element_text(hjust = 0.5),
            legend.position = 'none'
          )) #Figure 6

#probit did
tau.hat.did <- did_estimate(setup$Y, setup$N0, setup$T0)
lm_did <- feols(probit ~ treated | name + period, data = merge,
                 panel.id = ~name + period, vcov="DK")
lm_did_new <- feols(probit ~ treated | name + period, data = merge,
                panel.id = ~name + period, vcov="newey_west")
lm_did_clustered <- feols(probit ~ treated | name + period, data = merge,
                    panel.id = ~name + period)
summary(lm_did_clustered)
#probit sc
tau.hat.sc <- sc_estimate(setup$Y, setup$N0, setup$T0)
merge <- subset(merge, select = -c(omega,lambda, weights))
merge$omega [merge$name == merge$name [merge$treated==1][1]] <- 1
merge$lambda  <- 1

omega.df <- data.frame(name = rownames(attributes(tau.hat.sc)$setup$Y)[-nrow(attributes(tau.hat.sc)$setup$Y)], omega = attributes(tau.hat.sc)$weights$omega)

merge <- merge(merge, omega.df, by = "name", all = TRUE, no.dups = TRUE)
merge$omega <- ifelse(is.na(merge$omega.x),merge$omega.y,merge$omega.x)
merge <- subset(merge, select = -c(omega.x,omega.y))

merge$weights <- merge$lambda * merge$omega

lm_sc <- feols(probit ~ treated | period, data = merge, weights = merge$weights,
               panel.id = ~name+period,vcov = "DK")
lm_sc_new <- feols(probit ~ treated | period, data = merge, weights = merge$weights,
               panel.id = ~name+period,vcov = "newey_west")
lm_sc_clustered <- feols(probit ~ treated | period, data = merge, weights = merge$weights,
                   panel.id = ~name+period)
summary(lm_sc_clustered)

#print results tables
models <- list()
models[['SDID']] <- lm_sdid
models[['DID']] <- lm_did
models[['SC']] <- lm_sc

models_nw <- list()
models_nw[['SDID']] <- lm_sdid_new
models_nw[['DID']] <- lm_did_new
models_nw[['SC']] <- lm_sc_new

models_new <- list(lm_sdid_new,lm_sdid_clustered,lm_did_new,lm_did_clustered,lm_sc_new,lm_sc_clustered)
names(models_new) <- c('SDID','SDID','DID','DID','SC','SC')

glance_custom.fixest <- function(x, ...) {
  data.frame(
    'vcov.type'=ifelse(attributes(x$se)$type=="IID","Clustered by poll",attributes(x$se)),
    'fe.p'=ifelse("name" %in% names(x$fixef_id),'√','×'),
    'fe.t'=ifelse("period" %in% names(x$fixef_id),'√','×')
  )
}
gofmap <- list(list("raw" = "vcov.type", "clean" = "Std.Errors", "fmt" = 2),
              list("raw" = "fe.p", "clean" = "FE: Poll", "fmt" = NULL),
               list("raw" = "fe.t", "clean" = "FE: Time", "fmt" = NULL),
               list("raw" = "nobs", "clean" = "Effective N", "fmt" = 0),
               list("raw" = "r.squared", "clean" = "R2", "fmt" = 3),
               list("raw" = "r2.within.adjusted", "clean" = "R2 Adj.(within)", "fmt" = 3), 
               list("raw" = "rmse", "clean" = "RMSE", "fmt" = 2)) 
results_table <- modelsummary(models, output = "latex", notes="Note: Effective observations for SC and SDID are smaller due to zero time and unit weights.",
                              stars = c('*' = .1, '**' = .05, '***' = .01), 
                              title = "Poll: Rejection of Taiwan independence.", 
                              coef_rename = c("treated" = "Differential Treatment Effect"),  gof_map = gofmap) #Table 2
results_table_robust <- modelsummary(models_new, output = "latex", notes="Note: Effective observations for SC and SDID are smaller due to zero time and unit weights.",
                              stars = c('*' = .1, '**' = .05, '***' = .01), 
                              title = "Poll: Rejection of Taiwan independence.", 
                              coef_rename = c("treated" = "Differential Treatment Effect"),  gof_map = gofmap) #Table 6 or E1
results_table_robust <- modelsummary(models_nw, output = "latex", notes="Note: Effective observations for SC and SDID are smaller due to zero time and unit weights.",
                                     stars = c('*' = .1, '**' = .05, '***' = .01), 
                                     title = "Poll: Rejection of Taiwan independence.", 
                                     coef_rename = c("treated" = "Differential Treatment Effect"),  gof_map = gofmap) #Table 4

#three diagrams in 1 go
ses <- c(lm_did[["se"]][["treated"]],lm_sc[["se"]][["treated"]],lm_sdid[["se"]][["treated"]])

estimates = list(tau.hat.did, tau.hat.sc, tau.hat)
names(estimates) = c('Diff-in-Diff', 'Synthetic Control', 'Synthetic Diff-in-Diff')
synthdid_plot(estimates, facet.vertical=FALSE,
              control.name='Synthetic rejection of\nTaiwan independence', treated.name='Actual rejection of\nTaiwan independence',
              lambda.comparable=TRUE, se.method = 'none',
              trajectory.linetype = 1, line.width=.75, effect.curvature=-.4,
              trajectory.alpha=.7, effect.alpha=.7,
              diagram.alpha=1, onset.alpha=.7) +
  theme(legend.position=c(.10,.17), legend.direction='vertical',
        legend.key=element_blank(), legend.background=element_blank(),
        ) + xlab("Date") +ylab("Standardised public opinion") #Figure 2 (upper) and Figure 5 (upper)
#ggsave('taiwantrend.pdf',units='in',width=12.91, height=4.68, limitsize = FALSE)

synthdid_units_plot(estimates, se.manual = ses) + labs(size="Weight:") +
  theme(legend.background=element_blank(), legend.title = element_text(),
        legend.direction='horizontal', legend.position=c(.17,.80),
        strip.background=element_blank(), strip.text.x = element_blank()) #Figure 2 (lower) and Figure 5 (lower)
#ggsave('taiwanunit.pdf',units='in',width=12.91, height=4.68, limitsize = FALSE)

synthdid_plot(tau.hat, overlay=1,
              control.name='Synthetic public opinion', treated.name='Actual public opnion',
              se.method = 'none',
              trajectory.linetype = 1, line.width=.75, effect.curvature=-.4,
              trajectory.alpha=.7, effect.alpha=.7,
              diagram.alpha=1, onset.alpha=.7)

