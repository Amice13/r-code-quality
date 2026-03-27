


# WARNING: THESE MODELS WILL TAKE ~36 HOURS TO RUN. PLAN ACCORDINGLY.


# Parallelize machine
options(mc.cores = parallel::detectCores())
ncores = detectCores()
rstan_options(auto_write = TRUE)

# Set vague priors
vagueprior <- set_prior("normal(0, 100)", class = "b")



time.1 <- Sys.time()
# Models with country-level variables
# Troops Question
m1.cat.bayes.ideology <- brm(troops_1_cat ~ contact_pers + contact_nonpers + benefit_pers + benefit_nonpers + gender + ed + age + income + ideology + spend_toa_combined_w_log + baseinprovince + defense + log_threat_environment + log_troops_2017 + polity2 + gdp_constant_log + log_trade_total_2017 + log_students + (1 | country),
                            data = apsr.data,
                            prior = vagueprior,
                            iter = 10000,
                            warmup = 3000,
                            cores = ncores,
                            chains = 4,
                            control = list(adapt_delta = 0.95),
                            seed = 66502,
                            file = here("Bayes Diagnostics", "m1.cat.bayes.ideology"),
                            family = categorical(link = "logit", refcat = "neutral"))
time.2 <- Sys.time()

print(time.2-time.1)


# US Government Question
m2.cat.bayes.ideology <- brm(american_g_cat ~ contact_pers + contact_nonpers + benefit_pers + benefit_nonpers + gender + ed + age + income + ideology + spend_toa_combined_w_log + baseinprovince + defense + log_threat_environment + log_troops_2017 + polity2 + gdp_constant_log + log_trade_total_2017 + log_students + (1 | country),
                            data = apsr.data,
                            prior = vagueprior,
                            iter = 10000,
                            warmup = 3000,
                            cores = ncores,
                            chains = 4,
                            control = list(adapt_delta = 0.95),
                            seed = 66502,
                            file = here("Bayes Diagnostics", "m2.cat.bayes.ideology"),
                            family = categorical(link = "logit", refcat = "neutral"))


# US People Question
m3.cat.bayes.ideology <- brm(american_p_cat ~ contact_pers + contact_nonpers + benefit_pers + benefit_nonpers + gender + ed + age + income + ideology + spend_toa_combined_w_log + baseinprovince + defense + log_threat_environment + log_troops_2017 + polity2 + gdp_constant_log + log_trade_total_2017 + log_students + (1 | country),
                            data = apsr.data,
                            prior = vagueprior,
                            iter = 10000,
                            warmup = 3000,
                            cores = ncores,
                            chains = 4,
                            control = list(adapt_delta = 0.95),
                            seed = 66502,
                            file = here("Bayes Diagnostics", "m3.cat.bayes.ideology"),
                            family = categorical(link = "logit", refcat = "neutral"))


m1.cat.ideology.diag <- ggs(m1.cat.bayes.ideology)
ggmcmc(m1.cat.ideology.diag, file="m1-cat-bayes-ideology-diag.pdf")

m2.cat.ideology.diag <- ggs(m2.cat.bayes.ideology)
ggmcmc(m2.cat.ideology.diag, file="m2-cat-bayes-ideology-diag.pdf")

m3.cat.ideology.diag <- ggs(m3.cat.bayes.ideology)
ggmcmc(m3.cat.ideology.diag, file="m3-cat-bayes-ideology-diag.pdf")

ess.1 <- data.frame(effectiveSize(m1.cat.bayes.ideology)) 
ess.2 <- data.frame(effectiveSize(m2.cat.bayes.ideology)) 
ess.3 <- data.frame(effectiveSize(m3.cat.bayes.ideology)) 

ess.com <- cbind(ess.1, ess.2, ess.3) %>% 
  mutate(Variable = row.names(.)) %>% 
  dplyr::rename(., "Troops Model" = 1, "Government Model" = 2, "People Model" = 3) %>% 
  pivot_longer(cols = c(1:3), names_to = "Model")


# Plot effective sample size
ggplot(ess.com, aes(x = value, y = reorder(Variable, value))) +
  geom_barh(stat = "identity") +
  facet_wrap(. ~ Model) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 2)) +
  labs(x = "Effective Sample Size",
       y = "Variable") 

ggsave(here("Figures", "apsr-ess-cat-logit-ideology-20191001.pdf"))


# Tables
modellist <- list()
modellist[['US Troops']] <- `m1.cat.bayes.ideology`
modellist[['US Government']] <- `m2.cat.bayes.ideology`
modellist[['US People']] <- `m3.cat.bayes.ideology`


# Texreg tables for categorical models 

comap.long <- c("b_mupos_contact_persDontknowDDeclinetoanswer" = "PC: Don't know/Decline to answer", 
                "b_mupos_contact_persYes" = "PC: Yes",
                "b_mupos_contact_nonpersDontknowDDeclinetoanswer" = "NC: Don't know/Decline to answer",
                "b_mupos_contact_nonpersYes" = "NC: Yes",
                "b_mupos_benefit_persDontknowDDeclinetoanswer" = "PB: Don't know/Decline to answer", 
                "b_mupos_benefit_persYes" = "PB: Yes",
                "b_mupos_benefit_nonpersDontknowDDeclinetoanswer" = "NB: Don't know/Decline to answer", 
                "b_mupos_benefit_nonpersYes" = "NB: Yes",
                "b_mupos_genderFemale" = "Gender: Female",                               
                "b_mupos_genderNonMbinary" = "Gender: Non-Binary",
                "b_mupos_genderNoneoftheabove" = "Gender: None of the above",
                "b_mupos_ed" = "Education",
                "b_mupos_age25to34years" = "Age: 25-34 years",
                "b_mupos_age35to44years" = "Age: 35-44 years",
                "b_mupos_age45to54years" = "Age: 45-54 years",
                "b_mupos_age55to64years" = "Age: 55-64 years",
                "b_mupos_ageAge65orolder" = "Age: 65 or older",                        
                "b_mupos_income17%M34%" = "Income Percentile: 17-34",
                "b_mupos_income35%M50%" = "Income Percentile: 35-50",
                "b_mupos_income51%M67%" = "Income Percentile: 51-67",
                "b_mupos_income65%M83%" = "Income Percentile: 65-83",
                "b_mupos_income84%M100%" = "Income Percentile: 84-100",
                "b_mupos_ideology" = "Ideology",
                "b_mupos_spend_toa_combined_w_log" = "log(US Military Spending)",
                "b_mupos_baseinprovince" = "Base in Respondent's Province",
                "b_mupos_defense" = "US Defense Pact",
                "b_mupos_log_threat_environment" = "Threat Environment",
                "b_mupos_log_troops_2017" = "log(US Troops in Country, 2017)",
                "b_mupos_polity2" = "Polity Score",
                "b_mupos_gdp_constant_log" = "log(GDP)",
                "b_mupos_log_trade_total_2017" = "log(Total Trade with US)",
                "b_mupos_log_students" = "log(US Students in Respondent Country, 2017)",
                "b_muneg_contact_persDontknowDDeclinetoanswer" = "PC: Don't know/Decline to answer", 
                "b_muneg_contact_persYes" = "PC: Yes",
                "b_muneg_contact_nonpersDontknowDDeclinetoanswer" = "NC: Don't know/Decline to answer",
                "b_muneg_contact_nonpersYes" = "NC: Yes",
                "b_muneg_benefit_persDontknowDDeclinetoanswer" = "PB: Don't know/Decline to answer", 
                "b_muneg_benefit_persYes" = "PB: Yes",
                "b_muneg_benefit_nonpersDontknowDDeclinetoanswer" = "NB: Don't know/Decline to answer", 
                "b_muneg_benefit_nonpersYes" = "NB: Yes",
                "b_muneg_genderFemale" = "Gender: Female",                               
                "b_muneg_genderNonMbinary" = "Gender: Non-Binary",
                "b_muneg_genderNoneoftheabove" = "Gender: None of the above",
                "b_muneg_ed" = "Education",
                "b_muneg_age25to34years" = "Age: 25-34 years",
                "b_muneg_age35to44years" = "Age: 35-44 years",
                "b_muneg_age45to54years" = "Age: 45-54 years",
                "b_muneg_age55to64years" = "Age: 55-64 years",
                "b_muneg_ageAge65orolder" = "Age: 65 or older",                        
                "b_muneg_income17%M34%" = "Income Percentile: 17-34",
                "b_muneg_income35%M50%" = "Income Percentile: 35-50",
                "b_muneg_income51%M67%" = "Income Percentile: 51-67",
                "b_muneg_income65%M83%" = "Income Percentile: 65-83",
                "b_muneg_income84%M100%" = "Income Percentile: 84-100",
                "b_muneg_ideology" = "Ideology",
                "b_muneg_spend_toa_combined_w_log" = "log(US Military Spending)",
                "b_muneg_baseinprovince" = "Base in Respondent's Province",
                "b_muneg_defense" = "US Defense Pact",
                "b_muneg_log_threat_environment" = "Threat Environment",
                "b_muneg_log_troops_2017" = "log(US Troops in Country, 2017)",
                "b_muneg_polity2" = "Polity Score",
                "b_muneg_gdp_constant_log" = "log(GDP)",
                "b_muneg_log_trade_total_2017" = "log(Total Trade with US)",
                "b_muneg_log_students" = "log(US Students in Respondent Country, 2017)",
                "b_mudk_contact_persDontknowDDeclinetoanswer" = "PC: Don't know/Decline to answer", 
                "b_mudk_contact_persYes" = "PC: Yes",
                "b_mudk_contact_nonpersDontknowDDeclinetoanswer" = "NC: Don't know/Decline to answer",
                "b_mudk_contact_nonpersYes" = "NC: Yes",
                "b_mudk_benefit_persDontknowDDeclinetoanswer" = "PB: Don't know/Decline to answer", 
                "b_mudk_benefit_persYes" = "PB: Yes",
                "b_mudk_benefit_nonpersDontknowDDeclinetoanswer" = "NB: Don't know/Decline to answer", 
                "b_mudk_benefit_nonpersYes" = "NB: Yes",
                "b_mudk_genderFemale" = "Gender: Female",                               
                "b_mudk_genderNonMbinary" = "Gender: Non-Binary",
                "b_mudk_genderNoneoftheabove" = "Gender: None of the above",
                "b_mudk_ed" = "Education",
                "b_mudk_age25to34years" = "Age: 25-34 years",
                "b_mudk_age35to44years" = "Age: 35-44 years",
                "b_mudk_age45to54years" = "Age: 45-54 years",
                "b_mudk_age55to64years" = "Age: 55-64 years",
                "b_mudk_ageAge65orolder" = "Age: 65 or older",                        
                "b_mudk_income17%M34%" = "Income Percentile: 17-34",
                "b_mudk_income35%M50%" = "Income Percentile: 35-50",
                "b_mudk_income51%M67%" = "Income Percentile: 51-67",
                "b_mudk_income65%M83%" = "Income Percentile: 65-83",
                "b_mudk_income84%M100%" = "Income Percentile: 84-100",
                "b_mudk_ideology" = "Ideology",
                "b_mudk_spend_toa_combined_w_log" = "log(US Military Spending)",
                "b_mudk_baseinprovince" = "Base in Respondent's Province",
                "b_mudk_defense" = "US Defense Pact",
                "b_mudk_log_threat_environment" = "Threat Environment",
                "b_mudk_log_troops_2017" = "log(US Troops in Country, 2017)",
                "b_mudk_polity2" = "Polity Score",
                "b_mudk_gdp_constant_log" = "log(GDP)",
                "b_mudk_log_trade_total_2017" = "log(Total Trade with US)",
                "b_mudk_log_students" = "log(US Students in Respondent Country, 2017)")

group.list = list("\\emph{Response:Positive - Personal Contact}" = 1:2,
                  "\\emph{Response:Positive - Network Contact}"  = 3:4,
                  "\\emph{Response:Positive - Personal Benefit}" = 5:6,
                  "\\emph{Response:Positive - Network Benefit}" = 7:8,
                  "\\emph{Response:Positive - Gender Self-Identification}" = 9:11,
                  "\\emph{Response:Positive - Education}" = 12:12,
                  "\\emph{Response:Positive - Age Bracket}" = 13:17,
                  "\\emph{Response:Positive - Income Percentile}" = 18:22,
                  "\\emph{Response:Positive - Ideology}" = 23:23,
                  "\\emph{Response:Positive - Country-Level Variables}" = 24:32,
                  "\\emph{Response:Negative - Personal Contact}" = 33:34,
                  "\\emph{Response:Negative - Network Contact}"  = 35:36,
                  "\\emph{Response:Negative - Personal Benefit}" = 37:38,
                  "\\emph{Response:Negative - Network Benefit}" = 39:40,
                  "\\emph{Response:Negative - Gender Self-Identification}" = 41:43,
                  "\\emph{Response:Negative - Education}" = 44:44,
                  "\\emph{Response:Negative - Age Bracket}" = 45:49,
                  "\\emph{Response:Negative - Income Percentile}" = 50:54,
                  "\\emph{Response:Negative - Ideology}" = 55:55,
                  "\\emph{Response:Negative - Country-Level Variables}" = 56:64,
                  "\\emph{Response: Don't know/Decline - Personal Contact}" = 65:66,
                  "\\emph{Response: Don't know/Decline - Network Contact}"  = 67:68,
                  "\\emph{Response: Don't know/Decline - Personal Benefit}" = 69:70,
                  "\\emph{Response: Don't know/Decline - Network Benefit}" = 71:72,
                  "\\emph{Response: Don't know/Decline - Gender Self-Identification}" = 73:75,
                  "\\emph{Response: Don't know/Decline - Education}" = 76:76,
                  "\\emph{Response: Don't know/Decline - Age Bracket}" = 77:81,
                  "\\emph{Response: Don't know/Decline - Income Percentile}" = 82:86,
                  "\\emph{Response: Don't know/Decline - Ideology}" = 87:87,
                  "\\emph{Response: Don't know/Decline - Country-Level Variables}" = 88:96)



# US Troops Model
table.cat.bayes.ideology <- mcmcreg(`m1.cat.bayes.ideology`,
                                   pars = c('b_mupos', 'b_muneg', 'b_mudk'),
                                   digits = 3,
                                   single.row = TRUE,
                                   custom.coef.names = comap.long,
                                   groups = group.list,
                                   caption = "Categorical Bayesian logit models predicting attitudes towards various United States actors",
                                   caption.above = TRUE,
                                   font = "normalsize",
                                   label = "tab:catlogitbayestroops1")


table.cat.bayes.ideology <- table.cat.bayes.ideology %>% # Selection 2018 spending values
  read_lines()  %>% 
  grep('\\&', ., value = TRUE) %>% 
  paste(collapse = '\\\\ \n') %>% 
  read_fwf(fwf_empty(., n = 200)) %>% # Have to Increase n here to make sure it doesn't cut off cells
  as.data.frame()

table.cat.bayes.ideology.pos <- table.cat.bayes.ideology[c(2:43), ]
table.cat.bayes.ideology.neg <- table.cat.bayes.ideology[c(44:85), ]
table.cat.bayes.ideology.dk <- table.cat.bayes.ideology[c(86:127), ]

table.cat.bayes.ideology.complete <- cbind(table.cat.bayes.ideology.pos, table.cat.bayes.ideology.neg, table.cat.bayes.ideology.dk)
table.cat.bayes.ideology.complete[, c(4,8)] <- "&"
colnames(table.cat.bayes.ideology.complete)[5] <- "X5"
colnames(table.cat.bayes.ideology.complete)[6] <- "X6"
colnames(table.cat.bayes.ideology.complete)[7] <- "X7"
colnames(table.cat.bayes.ideology.complete)[8] <- "X8"
colnames(table.cat.bayes.ideology.complete)[9] <- "X9"
colnames(table.cat.bayes.ideology.complete)[10] <- "X10"
colnames(table.cat.bayes.ideology.complete)[11] <- "X11"
colnames(table.cat.bayes.ideology.complete)[12] <- "X12"

X1 <- c("\\textbf{Random Effects}", "\\quad N", "\\quad Groups", "\\quad Std. Dev.") 
X3 <- c(" ",
        summary(`m1.cat.bayes.ideology`)[[4]],
        length(unique(`m1.cat.bayes.ideology`$data$country)),
        round(summary(`m1.cat.bayes.ideology`)$random$country[3], digits = 3))
X7 <- c(" ",
        summary(`m1.cat.bayes.ideology`)[[4]],
        length(unique(`m1.cat.bayes.ideology`$data$country)),
        round(summary(`m1.cat.bayes.ideology`)$random$country[2], digits = 3))
X11 <- c(" ",
         summary(`m1.cat.bayes.ideology`)[[4]],
         length(unique(`m1.cat.bayes.ideology`$data$country)),
         round(summary(`m1.cat.bayes.ideology`)$random$country[1], digits = 3))

table.cat.bayes.ideology.ranef <- data.frame(X1, X3, X7, X11)

table.cat.bayes.ideology.complete <- plyr::rbind.fill(table.cat.bayes.ideology.complete, table.cat.bayes.ideology.ranef) %>% 
  fill(c(X2, X4, X6, X8, X10, X12)) %>% 
  mutate(X1 = gsub("\\(\\d\\)", " ", X1)) %>% 
  mutate(X5 = gsub("\\(\\d\\)", " ", X5)) %>% 
  mutate(X9 = gsub("\\(\\d\\)", " ", X9)) %>% 
  mutate(X1 = gsub("Response:Positive - ", "", X1))

table.cat.bayes.ideology.complete <- table.cat.bayes.ideology.complete[, -c(5, 6, 9, 10)]
table.cat.bayes.ideology.complete[42, 8] <- "\\\\ \\hline "
table.cat.bayes.ideology.complete[43, 8] <- "\\\\ \\hline "

# Full table for appendix
write.table(table.cat.bayes.ideology.complete, here("Tables", "models-cat-bayes-m1-ideology-20190924.tex"),
            quote = FALSE,
            row.names = FALSE,
            col.names = FALSE,
            na = " ")



# US Government Model
table.cat.bayes.ideology <- mcmcreg(`m2.cat.bayes.ideology`,
                                   pars = c('b_mupos', 'b_muneg', 'b_mudk'),
                                   digits = 3,
                                   single.row = TRUE,
                                   custom.coef.names = comap.long,
                                   groups = group.list,
                                   caption = "Categorical Bayesian logit models predicting attitudes towards various United States actors",
                                   caption.above = TRUE,
                                   font = "normalsize",
                                   label = "tab:catlogitbayesgovernment")


table.cat.bayes.ideology <- table.cat.bayes.ideology %>% # Selection 2018 spending values
  read_lines()  %>% 
  grep('\\&', ., value = TRUE) %>% 
  paste(collapse = '\\\\ \n') %>% 
  read_fwf(fwf_empty(., n = 200)) %>% # Have to Increase n here to make sure it doesn't cut off cells
  as.data.frame()

table.cat.bayes.ideology.pos <- table.cat.bayes.ideology[c(2:43), ]
table.cat.bayes.ideology.neg <- table.cat.bayes.ideology[c(44:85), ]
table.cat.bayes.ideology.dk <- table.cat.bayes.ideology[c(86:127), ]

table.cat.bayes.ideology.complete <- cbind(table.cat.bayes.ideology.pos, table.cat.bayes.ideology.neg, table.cat.bayes.ideology.dk)
table.cat.bayes.ideology.complete[, c(4,8)] <- "&"
colnames(table.cat.bayes.ideology.complete)[5] <- "X5"
colnames(table.cat.bayes.ideology.complete)[6] <- "X6"
colnames(table.cat.bayes.ideology.complete)[7] <- "X7"
colnames(table.cat.bayes.ideology.complete)[8] <- "X8"
colnames(table.cat.bayes.ideology.complete)[9] <- "X9"
colnames(table.cat.bayes.ideology.complete)[10] <- "X10"
colnames(table.cat.bayes.ideology.complete)[11] <- "X11"
colnames(table.cat.bayes.ideology.complete)[12] <- "X12"

X1 <- c("\\textbf{Random Effects}", "\\quad N", "\\quad Groups", "\\quad Std. Dev.") 
X3 <- c(" ",
        summary(`m2.cat.bayes.ideology`)[[4]],
        length(unique(`m2.cat.bayes.ideology`$data$country)),
        round(summary(`m2.cat.bayes.ideology`)$random$country[3], digits = 3))
X7 <- c(" ",
        summary(`m2.cat.bayes.ideology`)[[4]],
        length(unique(`m2.cat.bayes.ideology`$data$country)),
        round(summary(`m2.cat.bayes.ideology`)$random$country[2], digits = 3))
X11 <- c(" ",
         summary(`m2.cat.bayes.ideology`)[[4]],
         length(unique(`m2.cat.bayes.ideology`$data$country)),
         round(summary(`m2.cat.bayes.ideology`)$random$country[1], digits = 3))

table.cat.bayes.ideology.ranef <- data.frame(X1, X3, X7, X11)

table.cat.bayes.ideology.complete <- plyr::rbind.fill(table.cat.bayes.ideology.complete, table.cat.bayes.ideology.ranef) %>% 
  fill(c(X2, X4, X6, X8, X10, X12)) %>% 
  mutate(X1 = gsub("\\(\\d\\)", " ", X1)) %>% 
  mutate(X5 = gsub("\\(\\d\\)", " ", X5)) %>% 
  mutate(X9 = gsub("\\(\\d\\)", " ", X9)) %>% 
  mutate(X1 = gsub("Response:Positive - ", "", X1))

table.cat.bayes.ideology.complete <- table.cat.bayes.ideology.complete[, -c(5, 6, 9, 10)]
table.cat.bayes.ideology.complete[42, 8] <- "\\\\ \\hline "
table.cat.bayes.ideology.complete[43, 8] <- "\\\\ \\hline "

# Full table for appendix
write.table(table.cat.bayes.ideology.complete, here("Tables", "models-cat-bayes-m2-ideology-20190924.tex"),
            quote = FALSE,
            row.names = FALSE,
            col.names = FALSE,
            na = " ")



# US People Model
table.cat.bayes.ideology <- mcmcreg(`m3.cat.bayes.ideology`,
                                   pars = c('b_mupos', 'b_muneg', 'b_mudk'),
                                   digits = 3,
                                   single.row = TRUE,
                                   custom.coef.names = comap.long,
                                   groups = group.list,
                                   caption = "Categorical Bayesian logit models predicting attitudes towards various United States actors",
                                   caption.above = TRUE,
                                   font = "normalsize",
                                   label = "tab:catlogitbayespeople")

table.cat.bayes.ideology <- table.cat.bayes.ideology %>% # Selection 2018 spending values
  read_lines()  %>% 
  grep('\\&', ., value = TRUE) %>% 
  paste(collapse = '\\\\ \n') %>% 
  read_fwf(fwf_empty(., n = 200)) %>% # Have to Increase n here to make sure it doesn't cut off cells
  as.data.frame()

table.cat.bayes.ideology.pos <- table.cat.bayes.ideology[c(2:43), ]
table.cat.bayes.ideology.neg <- table.cat.bayes.ideology[c(44:85), ]
table.cat.bayes.ideology.dk <- table.cat.bayes.ideology[c(86:127), ]

table.cat.bayes.ideology.complete <- cbind(table.cat.bayes.ideology.pos, table.cat.bayes.ideology.neg, table.cat.bayes.ideology.dk)
table.cat.bayes.ideology.complete[, c(4,8)] <- "&"
colnames(table.cat.bayes.ideology.complete)[5] <- "X5"
colnames(table.cat.bayes.ideology.complete)[6] <- "X6"
colnames(table.cat.bayes.ideology.complete)[7] <- "X7"
colnames(table.cat.bayes.ideology.complete)[8] <- "X8"
colnames(table.cat.bayes.ideology.complete)[9] <- "X9"
colnames(table.cat.bayes.ideology.complete)[10] <- "X10"
colnames(table.cat.bayes.ideology.complete)[11] <- "X11"
colnames(table.cat.bayes.ideology.complete)[12] <- "X12"

X1 <- c("\\textbf{Random Effects}", "\\quad N", "\\quad Groups", "\\quad Std. Dev.") 
X3 <- c(" ",
        summary(`m3.cat.bayes.ideology`)[[4]],
        length(unique(`m3.cat.bayes.ideology`$data$country)),
        round(summary(`m3.cat.bayes.ideology`)$random$country[3], digits = 3))
X7 <- c(" ",
        summary(`m3.cat.bayes.ideology`)[[4]],
        length(unique(`m3.cat.bayes.ideology`$data$country)),
        round(summary(`m3.cat.bayes.ideology`)$random$country[2], digits = 3))
X11 <- c(" ",
         summary(`m3.cat.bayes.ideology`)[[4]],
         length(unique(`m3.cat.bayes.ideology`$data$country)),
         round(summary(`m3.cat.bayes.ideology`)$random$country[1], digits = 3))

table.cat.bayes.ideology.ranef <- data.frame(X1, X3, X7, X11)

table.cat.bayes.ideology.complete <- plyr::rbind.fill(table.cat.bayes.ideology.complete, table.cat.bayes.ideology.ranef) %>% 
  fill(c(X2, X4, X6, X8, X10, X12)) %>% 
  mutate(X1 = gsub("\\(\\d\\)", " ", X1)) %>% 
  mutate(X5 = gsub("\\(\\d\\)", " ", X5)) %>% 
  mutate(X9 = gsub("\\(\\d\\)", " ", X9)) %>% 
  mutate(X1 = gsub("Response:Positive - ", "", X1))

table.cat.bayes.ideology.complete <- table.cat.bayes.ideology.complete[, -c(5, 6, 9, 10)]
table.cat.bayes.ideology.complete[42, 8] <- "\\\\ \\hline "
table.cat.bayes.ideology.complete[43, 8] <- "\\\\ \\hline "

# Full table for appendix
write.table(table.cat.bayes.ideology.complete, here("Tables", "models-cat-bayes-m3-ideology-20190924.tex"),
            quote = FALSE,
            row.names = FALSE,
            col.names = FALSE,
            na = " ")





