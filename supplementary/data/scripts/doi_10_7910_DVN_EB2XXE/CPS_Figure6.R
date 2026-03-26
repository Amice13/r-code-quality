# Code for CPS Figure 6

# List of required packages
required_packages <- c("tidyr", "dplyr", "data.table", "srvyr", "survey", "ggpubr", "ggplot2", "broom", "broom.mixed", "jtools", "huxtable")

# Install missing packages
missing_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(missing_packages)) {
  install.packages(missing_packages)
}

# Load all required libraries
lapply(required_packages, library, character.only = TRUE)

setwd('~/Dropbox/CPS/CPS_Materials') # set working directory for the project

# Note, we have no right to this ESS file so anyone looking to replicate should download ESS rounds 1-5 directly from the ESS site by registering their email.
# https://ess.sikt.no/en/data-builder/
# ESS_unweighted<- read.csv("ESS1-ESS5.csv", stringsAsFactors=FALSE, fileEncoding="latin1")

##convert ESSround to factor variable
ESS_unweighted$essround <-as.factor(ESS_unweighted$essround)

ESS_unweighted$ESS_year[ESS_unweighted$essround==1] <- 2002
ESS_unweighted$ESS_year[ESS_unweighted$essround==2] <- 2004
ESS_unweighted$ESS_year[ESS_unweighted$essround==3] <- 2006
ESS_unweighted$ESS_year[ESS_unweighted$essround==4] <- 2008
ESS_unweighted$ESS_year[ESS_unweighted$essround==5] <- 2010

ESS_hu <- ESS_unweighted %>% 
  filter(cntry=="HU") 

# Some data cleaning + recoding for our control variables, IV + DV.
ESS_hu$votebinary <- NA
ESS_hu$votebinary[ESS_hu$vote ==1] <- 1
ESS_hu$votebinary[ESS_hu$vote >1] <- 0

ESS_hu$Closeparty <- NA
ESS_hu$Closeparty[ESS_hu$clsprty ==1] <- 1
ESS_hu$Closeparty[ESS_hu$clsprty >1] <- 0

ESS_hu$"Political interest" <- NA
ESS_hu$"Political interest" [ESS_hu$polintr ==1] <- 4
ESS_hu$"Political interest" [ESS_hu$polintr ==2] <- 3
ESS_hu$"Political interest" [ESS_hu$polintr ==3] <- 2
ESS_hu$"Political interest" [ESS_hu$polintr ==4] <- 1

ESS_hu$Protest <- NA
ESS_hu$Protest[ESS_hu$pbldmn ==1] <- 1
ESS_hu$Protest[ESS_hu$pbldmn >1] <- 0

ESS_hu$Petition <- NA
ESS_hu$Petition[ESS_hu$sgnptit ==1] <- 1
ESS_hu$Petition[ESS_hu$sgnptit >1] <- 0

ESS_hu$Male <- NA
ESS_hu$Male[ESS_hu$gndr ==1] <- 1
ESS_hu$Male[ESS_hu$gndr ==2] <- 0

ESS_hu$SatisfactionHHincome <- NA
ESS_hu$SatisfactionHHincome[ESS_hu$hincfel ==1] <- 4
ESS_hu$SatisfactionHHincome[ESS_hu$hincfel ==2] <- 3
ESS_hu$SatisfactionHHincome[ESS_hu$hincfel ==3] <- 2
ESS_hu$SatisfactionHHincome[ESS_hu$hincfel ==4] <- 1

ESS_hu$lrscale[ESS_hu$lrscale >10] <- NA
ESS_hu$Ideology <- ESS_hu$lrscale 

ESS_hu$Age <- ESS_hu$agea
ESS_hu$Age[ESS_hu$agea==999] <- NA

ESS_hu$Education <- ESS_hu$eisced
ESS_hu$Education[ESS_hu$Education>7]<-NA

election_2002<- subset(ESS_hu, ESS_year==2002)
election_2002$votedfidesz <- NA
election_2002$votedfidesz[election_2002$prtvthu >0   ] <- 0
election_2002$votedfidesz[election_2002$prtvthu==2 ] <- 1

xtabs(~votedfidesz + Protest, data = election_2002)

##weight survey responses using pspwght (post-stratification weight including design weight, provided by ESS)
post_design_2002 <- svydesign(ids = ~idno, 
                         weights = ~ pspwght,
                         data = election_2002)

logit1 <- svyglm(votedfidesz ~ Protest +Closeparty+ Male + Age+ Education + SatisfactionHHincome,  family="binomial", design=post_design_2002)
summary(logit1)
summ(logit1)
plot_summs(logit1, robust = TRUE, inner_ci_level = .9)

logit2 <- svyglm(votedfidesz ~  Protest + Male + Age+ Education + SatisfactionHHincome,  family="binomial", design=post_design_2002)
summary(logit2)
summ(logit2)
plot_summs(logit2, robust = TRUE, inner_ci_level = .9)

logit3 <- svyglm(votedfidesz ~ Protest,  family="binomial", design=post_design_2002)
summary(logit3)
summ(logit3)
plot_summs(logit3, robust = TRUE, inner_ci_level = .9)

logit4 <- svyglm(votedfidesz ~ Petition +Closeparty+ Male + Age+ Education + SatisfactionHHincome,  family="binomial",  design=post_design_2002)
summary(logit4)
summ(logit4)
plot_summs(logit4, robust = TRUE, inner_ci_level = .9)

logit5 <- svyglm(votedfidesz ~ Petition + Male + Age+ Education + SatisfactionHHincome,  family="binomial",  design=post_design_2002)
summary(logit5)
summ(logit5)
plot_summs(logit5, robust = TRUE, inner_ci_level = .9)

logit6 <- svyglm(votedfidesz ~ Petition,  family="binomial",  design=post_design_2002)
summary(logit6)
summ(logit6)
plot_summs(logit6, robust = TRUE,inner_ci_level = .9)


votefidesz_protestpetition02 <- plot_summs(logit1, logit2, logit3, logit4, logit5, logit6, 
                                           coefs = c("Protest" = "Protest", "Petition"="Petition", "Close party" = "Closeparty",
                                                     "Gender" = "Male", "Age"="Age", "Education"="Education", "Satisfaction HH income"="SatisfactionHHincome"),
                                           model.names = c("M1: Protest + all covariates", "M2: Protest + demographics", "M3: Protest only",
                                                           "M4: Petition + all covariates", "M5: Petition + demographics", "M6: Petition only"), scale=T,
                                           colors = "Paired")+ scale_x_continuous(limits = c(-1, 3.5),
                                                                                  breaks = seq(-1,3,1), name = "Log-Odds (Fidesz)")
votefidesz_protestpetition02


if (requireNamespace("huxtable")) {
  # Export all regressions with "Model #" labels,
  # standardized coefficients, and robust standard errors
  export_summs(logit1, logit2, logit3, logit4, logit5, logit6,
               model.names = c("Model 1","Model 2","Model 3", "Model 4","Model 5","Model 6"),
               coefs = c("Protest" = "Protest", "Petition"="Petition", "Close party" = "Closeparty",
                         "Gender" = "Male", "Age"="Age", "Education"="Education", "Satisfaction HH income"="SatisfactionHHincome"),
               scale = TRUE, robust = TRUE)}


election_2006<- subset(ESS_hu, ESS_year==2006)
election_2006$votedfidesz <- NA
election_2006$votedfidesz[election_2006$prtvtahu >0] <- 0
election_2006$votedfidesz[election_2006$prtvtahu==1] <- 1

xtabs(~votedfidesz + Protest, data = election_2006)

##weight survey responses using pspwght (post-stratification weight including design weight, provided by ESS)
post_design_2006 <- svydesign(ids = ~idno, 
                         weights = ~ pspwght,
                         data = election_2006)

logit1 <-svyglm(votedfidesz ~  Protest +Closeparty+ Male + Age+ Education + SatisfactionHHincome,  family="binomial", design=post_design_2006)
summary(logit1)
summ(logit1)
plot_summs(logit1, robust = TRUE, inner_ci_level = .9)

logit2 <-svyglm(votedfidesz ~  Protest + Male + Age+ Education + SatisfactionHHincome,   family="binomial", design=post_design_2006)
summary(logit2)
summ(logit2)
plot_summs(logit2, robust = TRUE, inner_ci_level = .9)

logit3 <-svyglm(votedfidesz ~  Protest,   family="binomial", design=post_design_2006)
summary(logit3)
summ(logit3)
plot_summs(logit3, robust = TRUE, inner_ci_level = .9)

logit4 <-svyglm(votedfidesz ~ Petition +Closeparty+ Male + Age+ Education + SatisfactionHHincome,   family="binomial", design=post_design_2006)
summary(logit4)
summ(logit4)
plot_summs(logit4, robust = TRUE, inner_ci_level = .9)

logit5 <-svyglm(votedfidesz ~ Petition + Male + Age+ Education + SatisfactionHHincome,   family="binomial", design=post_design_2006)
summary(logit5)
summ(logit5)
plot_summs(logit5, robust = TRUE, inner_ci_level = .9)

logit6 <-svyglm(votedfidesz ~ Petition, family="binomial", design=post_design_2006)
summary(logit6)
summ(logit6)
plot_summs(logit6, robust = TRUE, inner_ci_level = .9)


votefidesz_protestpetition06 <- plot_summs(logit1, logit2, logit3, logit4, logit5, logit6, 
                                           coefs = c("Protest" = "Protest", "Petition"="Petition", "Close party" = "Closeparty",
                                                     "Gender" = "Male", "Age"="Age", "Education"="Education", "Satisfaction HH income"="SatisfactionHHincome"),
                                           model.names = c("M1: Protest + all covariates", "M2: Protest + demographics", "M3: Protest only",
                                                           "M4: Petition + all covariates", "M5: Petition + demographics", "M6: Petition only"), scale=T,
                                           colors = "Paired")+ scale_x_continuous(limits = c(-1, 3.5),
                                                                                  breaks = seq(-1,3,1), name = "Log-Odds (Fidesz)")
votefidesz_protestpetition06

if (requireNamespace("huxtable")) {
  # Export all regressions with "Model #" labels,
  # standardized coefficients, and robust standard errors
  export_summs(logit1, logit2, logit3, logit4, logit5, logit6,
               model.names = c("Model 1","Model 2","Model 3", "Model 4","Model 5","Model 6"),
               coefs = c("Protest" = "Protest", "Petition"="Petition", "Close party" = "Closeparty",
                         "Gender" = "Male", "Age"="Age", "Education"="Education", "Satisfaction HH income"="SatisfactionHHincome"),
               scale = TRUE, robust = TRUE)}

election_2010<- subset(ESS_hu, ESS_year==2010)
election_2010$votedfidesz <- NA
election_2010$votedfidesz[election_2010$prtvtchu >0 ] <- 0
election_2010$votedfidesz[election_2010$prtvtchu==1 ] <- 1

xtabs(~votedfidesz + Protest, data = election_2010)

##weight survey responses using pspwght (post-stratification weight including design weight, provided by ESS)
post_design_2010 <- svydesign(ids = ~idno, 
                         weights = ~ pspwght,
                         data = election_2010)

logit1 <-svyglm(votedfidesz ~  Protest +Closeparty+ Male + Age+ Education + SatisfactionHHincome,  family="binomial", design=post_design_2010)
summary(logit1)
summ(logit1)
plot_summs(logit1, robust = TRUE, inner_ci_level = .9)

logit2 <-svyglm(votedfidesz ~  Protest + Male + Age+ Education + SatisfactionHHincome, family="binomial", design=post_design_2010)
summary(logit2)
summ(logit2)
plot_summs(logit2, robust = TRUE, inner_ci_level = .9)

logit3 <-svyglm(votedfidesz ~ Protest,  family="binomial", design=post_design_2010)
summary(logit3)
summ(logit3)
plot_summs(logit3, robust = TRUE, inner_ci_level = .9)

logit4 <-svyglm(votedfidesz ~  Petition +Closeparty+ Male + Age+ Education + SatisfactionHHincome,  family="binomial", design=post_design_2010)
summary(logit4)
summ(logit4)
plot_summs(logit4, robust = TRUE, inner_ci_level = .9)

logit5 <-svyglm(votedfidesz ~ Petition + Male + Age+ Education + SatisfactionHHincome,  family="binomial", design=post_design_2010)
summary(logit5)
summ(logit5)
plot_summs(logit5, robust = TRUE, inner_ci_level = .9)

logit6 <-svyglm(votedfidesz ~ Petition,  family="binomial", design=post_design_2010)
summary(logit6)
summ(logit6)
plot_summs(logit6, robust = TRUE, inner_ci_level = .9)


votefidesz_protestpetition10 <- plot_summs(logit1, logit2, logit3, logit4, logit5, logit6, 
                                           coefs = c("Protest" = "Protest", "Petition"="Petition", "Close party" = "Closeparty",
                                                     "Gender" = "Male", "Age"="Age", "Education"="Education", "Satisfaction HH income"="SatisfactionHHincome"),
                                           model.names = c("M1: Protest + all covariates", "M2: Protest + demographics", "M3: Protest only",
                                                           "M4: Petition + all covariates", "M5: Petition + demographics", "M6: Petition only"), scale=T,
                                           colors = "Paired")+ scale_x_continuous(limits = c(-1, 3.5),
                                                                                  breaks = seq(-1,3,1), name = "Log-Odds (Fidesz)")
votefidesz_protestpetition10

if (requireNamespace("huxtable")) {
  # Export all regressions with "Model #" labels,
  # standardized coefficients, and robust standard errors
  export_summs(logit1, logit2, logit3, logit4, logit5, logit6,
               model.names = c("Model 1","Model 2","Model 3", "Model 4","Model 5","Model 6"),
               coefs = c("Protest" = "Protest", "Petition"="Petition", "Close party" = "Closeparty",
                         "Gender" = "Male", "Age"="Age", "Education"="Education", "Satisfaction HH income"="SatisfactionHHincome"),
               scale = TRUE, robust = TRUE)}


election_2002$votedmszp<- NA
election_2002$votedmszp[election_2002$prtvthu >0 ] <- 0
election_2002$votedmszp[election_2002$prtvthu==5 ] <- 1

xtabs(~votedmszp + Protest, data = election_2002)

##weight survey responses using pspwght (post-stratification weight including design weight, provided by ESS)
post_design_2002 <- svydesign(ids = ~idno, 
                         weights = ~ pspwght,
                         data = election_2002)

logit1 <-svyglm(votedmszp ~  Protest +Closeparty+ Male + Age+ Education + SatisfactionHHincome,  family="binomial", design=post_design_2002)
summary(logit1)
summ(logit1)
plot_summs(logit1, robust = TRUE, inner_ci_level = .9)

logit2 <-svyglm(votedmszp ~  Protest + Male + Age+ Education + SatisfactionHHincome,   family="binomial", design=post_design_2002)
summary(logit2)
summ(logit2)
plot_summs(logit2, robust = TRUE, inner_ci_level = .9)

logit3 <-svyglm(votedmszp ~ Protest,  family="binomial", design=post_design_2002)
summary(logit3)
summ(logit3)
plot_summs(logit3, robust = TRUE, inner_ci_level = .9)

logit4 <-svyglm(votedmszp ~  Petition +Closeparty+ Male + Age+ Education + SatisfactionHHincome,   family="binomial", design=post_design_2002)
summary(logit4)
summ(logit4)
plot_summs(logit4, robust = TRUE, inner_ci_level = .9)

logit5 <-svyglm(votedmszp ~ Petition + Male + Age+ Education + SatisfactionHHincome,  family="binomial", design=post_design_2002)
summary(logit5)
summ(logit5)
plot_summs(logit5, robust = TRUE, inner_ci_level = .9)

logit6 <-svyglm(votedmszp ~ Petition,  family="binomial", design=post_design_2002)
summary(logit6)
summ(logit6)
plot_summs(logit6, robust = TRUE, inner_ci_level = .9)

votedmszp_protestpetition02 <- plot_summs(logit1, logit2, logit3, logit4, logit5, logit6, 
                                          coefs = c("Protest" = "Protest", "Petition"="Petition", "Close party" = "Closeparty",
                                                    "Gender" = "Male", "Age"="Age", "Education"="Education", "Satisfaction HH income"="SatisfactionHHincome"),
                                          model.names = c("M1: Protest + all covariates", "M2: Protest + demographics", "M3: Protest only",
                                                          "M4: Petition + all covariates", "M5: Petition + demographics", "M6: Petition only"), scale=T,
                                          colors = "Paired") + scale_x_continuous(limits = c(-3.5, 1.5),
                                                                                  breaks = seq(-3,2,1),
                                                                                  name = "Log-Odds (MSZP)")
votedmszp_protestpetition02


if (requireNamespace("huxtable")) {
  # Export all regressions with "Model #" labels,
  # standardized coefficients, and robust standard errors
  export_summs(logit1, logit2, logit3, logit4, logit5, logit6,
               model.names = c("Model 1","Model 2","Model 3", "Model 4","Model 5","Model 6"),
               coefs = c("Protest" = "Protest", "Petition"="Petition", "Close party" = "Closeparty",
                         "Gender" = "Male", "Age"="Age", "Education"="Education", "Satisfaction HH income"="SatisfactionHHincome"),
               scale = TRUE, robust = TRUE)}

election_2006$votedmszp<- NA
election_2006$votedmszp[election_2006$prtvtahu >0] <- 0
election_2006$votedmszp[election_2006$prtvtahu==4] <- 1

xtabs(~votedmszp + Protest, data = election_2006)

##weight survey responses using pspwght (post-stratification weight including design weight, provided by ESS)
post_design_2006 <- svydesign(ids = ~idno, 
                         weights = ~ pspwght,
                         data = election_2006)

logit1 <-svyglm(votedmszp ~ Protest +Closeparty+ Male + Age+ Education + SatisfactionHHincome,  family="binomial", design=post_design_2006)
summary(logit1)
summ(logit1)
plot_summs(logit1, robust = TRUE, inner_ci_level = .9)

logit2 <-svyglm(votedmszp ~  Protest + Male + Age+ Education + SatisfactionHHincome,  family="binomial", design=post_design_2006)
summary(logit2)
summ(logit2)
plot_summs(logit2, robust = TRUE, inner_ci_level = .9)

logit3 <-svyglm(votedmszp ~ Protest,  family="quasibinomial", design=post_design_2006)
summary(logit3)
summ(logit3)
plot_summs(logit3, robust = TRUE, inner_ci_level = .9)

logit4 <-svyglm(votedmszp ~  Petition +Closeparty+ Male + Age+ Education + SatisfactionHHincome,  family="binomial", design=post_design_2006)
summary(logit4)
summ(logit4)
plot_summs(logit4, robust = TRUE, inner_ci_level = .9)

logit5 <-svyglm(votedmszp ~  Petition + Male + Age+ Education + SatisfactionHHincome,   family="binomial", design=post_design_2006)
summary(logit5)
summ(logit5)
plot_summs(logit5, robust = TRUE, inner_ci_level = .9)

logit6 <-svyglm(votedmszp ~ Petition,  family="binomial", design=post_design_2006)
summary(logit6)
summ(logit6)
plot_summs(logit6, robust = TRUE, inner_ci_level = .9)

votedmszp_protestpetition06 <- plot_summs(logit1, logit2, logit3, logit4, logit5, logit6, 
                                          coefs = c("Protest" = "Protest", "Petition"="Petition", "Close party" = "Closeparty",
                                                    "Gender" = "Male", "Age"="Age", "Education"="Education", "Satisfaction HH income"="SatisfactionHHincome"),
                                          model.names = c("M1: Protest + all covariates", "M2: Protest + demographics", "M3: Protest only",
                                                          "M4: Petition + all covariates", "M5: Petition + demographics", "M6: Petition only"), scale=T,
                                          colors = "Paired") + scale_x_continuous(limits = c(-4.5, 1.5),
                                                                                  breaks = seq(-3,2,1),
                                                                                  name = "Log-Odds (MSZP)")
votedmszp_protestpetition06

if (requireNamespace("huxtable")) {
  # Export all regressions with "Model #" labels,
  # standardized coefficients, and robust standard errors
  export_summs(logit1, logit2, logit3, logit4, logit5, logit6,
               model.names = c("Model 1","Model 2","Model 3", "Model 4","Model 5","Model 6"),
               coefs = c("Protest" = "Protest", "Petition"="Petition", "Close party" = "Closeparty",
                         "Gender" = "Male", "Age"="Age", "Education"="Education", "Satisfaction HH income"="SatisfactionHHincome"),
               scale = TRUE, robust = TRUE)}


election_2010$votedmszp<- NA
election_2010$votedmszp[election_2010$prtvtchu >0 ] <- 0
election_2010$votedmszp[election_2010$prtvtchu==4 ] <- 1

xtabs(~votedmszp + Protest, data =  election_2010)

##weight survey responses using pspwght (post-stratification weight including design weight, provided by ESS)
post_design_2010 <- svydesign(ids = ~idno, 
                         weights = ~ pspwght,
                         data = election_2010)

logit1 <-svyglm(votedmszp ~ Protest +Closeparty+ Male + Age+ Education + SatisfactionHHincome,   family="binomial", design=post_design_2010)
summary(logit1)
summ(logit1)
plot_summs(logit1, robust = TRUE, inner_ci_level = .9)

logit2 <-svyglm(votedmszp ~ Protest + Male + Age+ Education + SatisfactionHHincome,   family="binomial", design=post_design_2010)
summary(logit2)
summ(logit2)
plot_summs(logit2, robust = TRUE, inner_ci_level = .9)

logit3 <-svyglm(votedmszp ~ Protest,  family="binomial", design=post_design_2010)
summary(logit3)
summ(logit3)
plot_summs(logit3, robust = TRUE, inner_ci_level = .9)

logit4 <-svyglm(votedmszp ~ Petition +Closeparty+ Male + Age+ Education + SatisfactionHHincome,   family="binomial", design=post_design_2010)
summary(logit4)
summ(logit4)
plot_summs(logit4, robust = TRUE, inner_ci_level = .9)

logit5 <-svyglm(votedmszp ~ Petition + Male + Age+ Education + SatisfactionHHincome,   family="binomial", design=post_design_2010)
summary(logit5)
summ(logit5)
plot_summs(logit5, robust = TRUE, inner_ci_level = .9)

logit6 <-svyglm(votedmszp ~ Petition,  family="binomial", design=post_design_2010)
summary(logit6)
summ(logit6)
plot_summs(logit6, robust = TRUE, inner_ci_level = .9)

votedmszp_protestpetition10 <- plot_summs(logit1, logit2, logit3, logit4, logit5, logit6, 
                                          coefs = c("Protest" = "Protest", "Petition"="Petition", "Close party" = "Closeparty",
                                                    "Gender" = "Male", "Age"="Age", "Education"="Education", "Satisfaction HH income"="SatisfactionHHincome"),
                                          model.names = c("M1: Protest + all covariates", "M2: Protest + demographics", "M3: Protest only",
                                                          "M4: Petition + all covariates", "M5: Petition + demographics", "M6: Petition only"), scale=T,
                                          colors = "Paired") + scale_x_continuous(limits = c(-3.5, 1.5),
                                                                                  breaks = seq(-3,2,1),
                                                                                  name = "Log-Odds (MSZP)")

votedmszp_protestpetition10 

if (requireNamespace("huxtable")) {
  # Export all regressions with "Model #" labels,
  # standardized coefficients, and robust standard errors
  export_summs(logit1, logit2, logit3, logit4, logit5, logit6,
               model.names = c("Model 1","Model 2","Model 3", "Model 4","Model 5","Model 6"),
               coefs = c("Protest" = "Protest", "Petition"="Petition", "Close party" = "Closeparty",
                         "Gender" = "Male", "Age"="Age", "Education"="Education", "Satisfaction HH income"="SatisfactionHHincome"),
               scale = TRUE, robust = TRUE)}


Figure6 <- ggarrange(votefidesz_protestpetition02,  votefidesz_protestpetition06 + 
                                theme(axis.text.y = element_blank(),
                                      axis.ticks.y = element_blank(),
                                      axis.title.y = element_blank() ),
                              votefidesz_protestpetition10+ 
                                theme(axis.text.y = element_blank(),
                                      axis.ticks.y = element_blank(),
                                      axis.title.y = element_blank() ),
                              votedmszp_protestpetition02,  votedmszp_protestpetition06 + 
                                theme(axis.text.y = element_blank(),
                                      axis.ticks.y = element_blank(),
                                      axis.title.y = element_blank() ),
                              votedmszp_protestpetition10+ 
                                theme(axis.text.y = element_blank(),
                                      axis.ticks.y = element_blank(),
                                      axis.title.y = element_blank() ),
                              labels = c("2002", "2006", "2010"),
                              hjust=-5.1,
                              vjust= 1.1,
                              ncol = 3, nrow = 2,
                              widths = c(-9, -9, -9),
                              align = "hv", 
                              font.label = list(size = 14, color = "black", face = "bold"),
                              common.legend = TRUE,
                              legend="bottom")
Figure6
ggsave(Figure6, file="./Figure6.pdf", height = 10, width = 10, dpi = 250,  bg="white")




