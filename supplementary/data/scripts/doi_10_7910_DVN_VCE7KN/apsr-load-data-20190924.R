
remotes::install_github("zeehio/facetscales")
remotes::install_github("vincentarelbundock/gt")
remove.packages("rstan")
if (file.exists(".RData")) file.remove(".RData")
remotes::install_github("stan-dev/rstan", ref = "develop", subdir = "rstan/rstan", build_opts = "")
devtools::install_github("jayrobwilliams/RWmisc")

devtools::install_github("rstudio/gt", ref="eff3be7384365a44459691e49b9b740420cd0851")

afurl <- "https://raw.githubusercontent.com/lme4/lme4/master/misc/issues/allFit.R"
eval(parse(text=getURL(afurl)))

install.packages("tidyverse", "tidyr", "gt", "dfoptim", "arm", "brms", "tidybayes", "Rcurl", "merTools", "reshape2", 
                 "modelr", "xtable", "margins", "purrr", "dotwhisker", "ggcorrplot", "GGally", "optimx", "ggridges", 
                 "stargazer", "modelsummary", "lme4", "sf")

library(tidyverse)
library(data.table)
library(broom)
library(tidyr)
library(here)
library(gt)
library(Rtools)
library(psych)
library(readtext)
library(lme4)
library(nnet)
library(mlogit)
library(dfoptim)
library(arm)
library(brms)
library(ordinal)
library(rstan)
library(rstantools)
library(rstanarm)
library(parallel)
library(tidybayes)
library(optimx)
library(RCurl)
library(modelr)
library(merTools)
library(reshape)
library(reshape2)
library(xtable)
library(brant)
library(purrr)
library(margins)
library(reporttools)
library(dotwhisker)
library(ggcorrplot)
library(texreg)
library(corrplot)
library(GGally)
library(maps)
library(ggmap)
library(scales)
library(RColorBrewer)
library(ggstance)
library(ggpubr)
library(ggmcmc)
library(BayesPostEst)
library(bayesplot)
library(coda)
library(RWmisc)
library(facetscales)
library(optimx)
library(shinystan)
library(ggridges)
library(stargazer)
library(raster)
library(modelsummary)
library(countrycode)
library(sessioninfo)


here()


# Data analysis for APSR R&R
# Load data and code factor variable levels as needed.
apsr.data <- read.csv(here("Data", "apsr-data-20190905.csv"), na.strings=c("","NA")) 

apsr.data <- apsr.data %>% 
  mutate(self_sufficient = factor(self_sufficient, ordered = FALSE, levels = c("Don't know/decline to answer", "Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree")),
         party_dems = factor(party_dems, ordered = FALSE, levels = c("Don't know/decline to answer", "Very negative", "Negative", "Neither Positive nor negative", "Positive", "Very positive")),
         party_reps = factor(party_reps, ordered = FALSE, levels = c("Don't know/decline to answer", "Very negative", "Negative", "Neither Positive nor negative", "Positive", "Very positive")),
         troops_econ_nat = factor(troops_econ_nat, ordered = FALSE, levels = c("Don't know/decline to answer", "Very negative", "Negative", "Neither Positive nor negative", "Positive", "Very positive")),
         troops_econ_local = factor(troops_econ_local, ordered = FALSE, levels = c("Don't know/decline to answer", "Very negative", "Negative", "Neither Positive nor negative", "Positive", "Very positive")),
         troops_crime_pers = factor(troops_crime_pers, ordered = FALSE, levels = c("Don't know/Decline to answer", "No", "Yes")),
         troops_1 = factor(troops_1, ordered = FALSE, levels = c("Don't know/decline to answer", "Very unfavorable", "Somewhat unfavorable", "Neutral", "Somewhat favorable", "Very favorable")),
         troops_1_ord = droplevels(troops_1, exclude = c("Don't know/decline to answer")),
         troops_1_ord = factor(troops_1, ordered = TRUE, levels = c("Very unfavorable", "Somewhat unfavorable", "Neutral", "Somewhat favorable", "Very favorable")),
         cooperate = factor(cooperate, levels = c("Don't know/Decline to answer", "Not important", "Neutral", "Somewhat Important", "Very Important")),
         american_people = factor(american_people, ordered = FALSE, levels = c("Don't know/decline to answer", "Very unfavorable", "Somewhat unfavorable", "Neutral", "Somewhat favorable", "Very favorable")),
         american_people_ord = droplevels(american_people, exclude = c("Don't know/decline to answer")),
         american_people_ord = factor(american_people, ordered = TRUE, levels = c("Very unfavorable", "Somewhat unfavorable", "Neutral", "Somewhat favorable", "Very favorable")),
         american_gov = factor(american_gov, ordered = FALSE, levels = c("Don't know/decline to answer", "Very unfavorable", "Somewhat unfavorable", "Neutral", "Somewhat favorable", "Very favorable")),
         american_gov_ord = droplevels(american_gov, exclude = c("Don't know/decline to answer")),
         american_gov_ord = factor(american_gov, ordered = TRUE, levels = c("Very unfavorable", "Somewhat unfavorable", "Neutral", "Somewhat favorable", "Very favorable")),
         freedom_rating = factor(freedom_rating, ordered = FALSE, levels = c("Free", "Partly Free", "Not Free")),
         income = factor(income, ordered = FALSE, levels = c("0%-16%", "17%-34%", "35%-50%", "51%-67%", "65%-83%", "84%-100%"), exclude = NA),
         gender = factor(gender, ordered = FALSE, levels = c("Female", "Male", "Non-binary", "None of the above")),
         gender = relevel(gender, ref = "Male"),
         ideology_cent = ideology - mean(ideology, na.rm = TRUE),
         protest = factor(protest, ordered = FALSE, levels = c("Don't know/Decline to answer", "Never", "One time", "Two times", "Three times", "More than three times")),
         contact_pers = factor(contact_pers, ordered = FALSE, levels = c("Don't know/Decline to answer", "No", "Yes")),
         contact_pers = relevel(contact_pers, ref = "No"),
         contact_nonpers = factor(contact_nonpers, ordered = FALSE, levels = c("Don't know/Decline to answer", "No", "Yes")),
         contact_nonpers = relevel(contact_nonpers, ref = "No"),
         benefit_pers = factor(benefit_pers, ordered = FALSE, levels = c("Don't know/Decline to answer", "No", "Yes")),
         benefit_pers = relevel(benefit_pers, ref = "No"),
         benefit_nonpers = factor(benefit_nonpers, ordered = FALSE, levels = c("Don't know/Decline to answer", "No", "Yes")),
         benefit_nonpers = relevel(benefit_nonpers, ref = "No"),
         american_inf_1 = factor(american_inf_1, ordered = FALSE, levels = c("Don't know/decline to answer", "None", "A little", "Some", "A lot")),
         american_inf_1 = relevel(american_inf_1, ref = "None"),
         american_inf_2 = factor(american_inf_2, ordered = FALSE, levels = c("Don't know/decline to answer", "Very negative", "Negative", "Neither Positive nor negative", "Positive", "Very positive")),
         american_inf_2 = relevel(american_inf_2, ref = "Neither Positive nor negative"),
         troops_security = factor(troops_security, levels = c("Don't know/decline to answer", "Very unhelpful", "Somewhat unhelpful", "Neutral", "Somewhat helpful", "Very helpful")),
         econnat_dummy = ifelse(troops_econ_nat == "Positive" | troops_econ_nat == "Very positive", 1, 0),
         econloc_dummy = ifelse(troops_econ_local == "Positive" | troops_econ_local == "Very positive", 1, 0),
         party_reps = factor(party_reps, levels = c("Don't know/decline to answer", "Very negative", "Negative", "Neither Positive nor negative", "Positive", "Very positive")),
         american_p_dummy = ifelse(american_people == "Somewhat favorable" | american_people == "Very favorable", 1, 0),
         american_g_dummy = ifelse(american_gov == "Somewhat favorable" | american_gov == "Very favorable", 1, 0),
         american_t_dummy = ifelse(troops_1 == "Somewhat favorable" | troops_1 == "Very favorable", 1, 0),
         troops_1_cat = fct_collapse(troops_1,
                                     dk = "Don't know/decline to answer",
                                     neutral = "Neutral",
                                     neg = c("Very unfavorable", "Somewhat unfavorable"),
                                     pos = c("Somewhat favorable", "Very favorable")),
         american_p_cat = fct_collapse(american_people,
                                       dk = "Don't know/decline to answer",
                                       neutral = "Neutral",
                                       neg = c("Very unfavorable", "Somewhat unfavorable"),
                                       pos = c("Somewhat favorable", "Very favorable")),
         american_g_cat = fct_collapse(american_gov,
                                       dk = "Don't know/decline to answer",
                                       neutral = "Neutral",
                                       neg = c("Very unfavorable", "Somewhat unfavorable"),
                                       pos = c("Somewhat favorable", "Very favorable")),
         country = factor(country, exclude = NA)) 


# Session information for appendix
sessioninfo <- session_info()
sessioninfo <- as.data.frame(sessioninfo[[2]]) %>%
  dplyr::select(package, loadedversion, date, source)

rownames(sessioninfo) <- c()

# Full table for appendix

session.table <- xtable(sessioninfo,
                        caption = "Session Information",
                        label = "tab:rsession")

print(session.table,
      tabular.environment = "longtable",
      floating = FALSE,
      size = "footnotesize",
      booktabs = TRUE,
      caption.placement = "top",
      file = here("Tables", "apsr-r-session.tex"))
      

       
