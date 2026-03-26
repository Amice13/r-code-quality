


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
m1.cat.bayes.nobase <- brm(troops_1_cat ~ contact_pers + contact_nonpers + benefit_pers + benefit_nonpers + american_inf_1 + american_inf_2 + demgov + gender + ed + age + income + ideology + relig + minority + spend_toa_combined_w_log + defense + log_threat_environment + log_troops_2017 + polity2 + gdp_constant_log + log_trade_total_2017 + log_students + (1 | country),
                    data = subset(apsr.data, baseinprovince == 0),
                    prior = vagueprior,
                    iter = 10000,
                    warmup = 3000,
                    cores = ncores,
                    chains = 4,
                    control = list(adapt_delta = 0.95),
                    seed = 66502,
                    file = here("Bayes Diagnostics", "m1.cat.bayes.nobase"),
                    family = categorical(link = "logit", refcat = "neutral"))
time.2 <- Sys.time()

print(time.2-time.1)


# US Government Question
m2.cat.bayes.nobase <- brm(american_g_cat ~ contact_pers + contact_nonpers + benefit_pers + benefit_nonpers + american_inf_1 + american_inf_2 + demgov + gender + ed + age + income + ideology + relig + minority + spend_toa_combined_w_log + defense + log_threat_environment + log_troops_2017 + polity2 + gdp_constant_log + log_trade_total_2017 + log_students + (1 | country),
                    data = subset(apsr.data, baseinprovince == 0),
                    prior = vagueprior,
                    iter = 10000,
                    warmup = 3000,
                    cores = ncores,
                    chains = 4,
                    control = list(adapt_delta = 0.95),
                    seed = 66502,
                    file = here("Bayes Diagnostics", "m2.cat.bayes.nobase"),
                    family = categorical(link = "logit", refcat = "neutral"))


# US People Question
m3.cat.bayes.nobase <- brm(american_p_cat ~ contact_pers + contact_nonpers + benefit_pers + benefit_nonpers + american_inf_1 + american_inf_2 + demgov + gender + ed + age + income + ideology + relig + minority + spend_toa_combined_w_log + defense + log_threat_environment + log_troops_2017 + polity2 + gdp_constant_log + log_trade_total_2017 + log_students + (1 | country),
                    data = subset(apsr.data, baseinprovince == 0),
                    prior = vagueprior,
                    iter = 10000,
                    warmup = 3000,
                    cores = ncores,
                    chains = 4,
                    control = list(adapt_delta = 0.95),
                    seed = 66502,
                    file = here("Bayes Diagnostics", "m3.cat.bayes.nobase"),
                    family = categorical(link = "logit", refcat = "neutral"))


# Convert files to ggmcmc format
m1.cat.nobase.diag <- ggs(m1.cat.bayes.nobase)
ggmcmc(m1.cat.nobase.diag, file="m1-cat-bayes-nobase-diag.pdf")

m2.cat.nobase.diag <- ggs(m2.cat.bayes.nobase)
ggmcmc(m2.cat.nobase.diag, file="m2-cat-bayes-nobase-diag.pdf")

m3.cat.nobase.diag <- ggs(m3.cat.bayes.nobase)
ggmcmc(m3.cat.nobase.diag, file="m3-cat-bayes-nobase-diag.pdf")


# Effective Sample Size
ess.1 <- data.frame(effectiveSize(m1.cat.bayes.nobase)) 
ess.2 <- data.frame(effectiveSize(m2.cat.bayes.nobase)) 
ess.3 <- data.frame(effectiveSize(m3.cat.bayes.nobase)) 

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

ggsave(here("Figures", "apsr-ess-cat-logit-nobase-20191001.pdf"))



# Tables
modellist <- list()
modellist[['US Troops']] <- `m1.cat.bayes.nobase`
modellist[['US Government']] <- `m2.cat.bayes.nobase`
modellist[['US People']] <- `m3.cat.bayes.nobase`


# Texreg tables for categorical models 

comap.long <- c("b_mupos_contact_persDontknowDDeclinetoanswer" = "PC: Don't know/Decline to answer", 
                "b_mupos_contact_persYes" = "PC: Yes",
                "b_mupos_contact_nonpersDontknowDDeclinetoanswer" = "NC: Don't know/Decline to answer",
                "b_mupos_contact_nonpersYes" = "NC: Yes",
                "b_mupos_benefit_persDontknowDDeclinetoanswer" = "PB: Don't know/Decline to answer", 
                "b_mupos_benefit_persYes" = "PB: Yes",
                "b_mupos_benefit_nonpersDontknowDDeclinetoanswer" = "NB: Don't know/Decline to answer", 
                "b_mupos_benefit_nonpersYes" = "NB: Yes",
                "b_mupos_american_inf_1DontknowDdeclinetoanswer" = "American influence (Degree): Don't know/Decline to answer", 
                "b_mupos_american_inf_1Alittle" = "American influence (Degree): A little",
                "b_mupos_american_inf_1Some" = "American influence (Degree): Some",
                "b_mupos_american_inf_1Alot" = "American influence (Degree): A lot",
                "b_mupos_american_inf_2DontknowDdeclinetoanswer" = "American influence (Quality): Don't know/Decline to answer",
                "b_mupos_american_inf_2Verynegative" = "American influence (Quality): Very Negative",
                "b_mupos_american_inf_2Negative" = "American influence (Quality): Negative",
                "b_mupos_american_inf_2Positive" = "American influence (Quality): Positive",                    
                "b_mupos_american_inf_2Verypositive" = "American influence (Quality): Very Positive",
                "b_mupos_demgovNeutral" = "Democratic Government: Neutral",                             
                "b_mupos_demgovNotimportant" = "Democratic Government: Not Important",
                "b_mupos_demgovSomewhatImportant" = "Democratic Government: Somewhat Important",                  
                "b_mupos_demgovVeryImportant" = "Democratic Government: Very Important",
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
                "b_mupos_religProtestant" = "Protestant",
                "b_mupos_religCatholicism" = "Catholicism",
                "b_mupos_religIslam" = "Islam",
                "b_mupos_religJudaism" = "Judaism",
                "b_mupos_religShinto" = "Shinto",
                "b_mupos_religBuddhism" = "Buddhism",
                "b_mupos_religHinduism" = "Hinduism",
                "b_mupos_religLocal" = "Local",
                "b_mupos_religMormonism" = "Mormonism",
                "b_mupos_religDecline to answer" = "Decline to answer",
                "b_mupos_religOther" = "Other",
                "b_mupos_minorityNo" = "Minority: Yes",                                
                "b_mupos_minorityYes" = "Minority: No",
                "b_mupos_spend_toa_combined_w_log" = "log(US Military Spending)",
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
                "b_muneg_american_inf_1DontknowDdeclinetoanswer" = "American influence (Degree): Don't know/Decline to answer", 
                "b_muneg_american_inf_1Alittle" = "American influence (Degree): A little",
                "b_muneg_american_inf_1Some" = "American influence (Degree): Some",
                "b_muneg_american_inf_1Alot" = "American influence (Degree): A lot",
                "b_muneg_american_inf_2DontknowDdeclinetoanswer" = "American influence (Quality): Don't know/Decline to answer",
                "b_muneg_american_inf_2Verynegative" = "American influence (Quality): Very Negative",
                "b_muneg_american_inf_2Negative" = "American influence (Quality): Negative",
                "b_muneg_american_inf_2Positive" = "American influence (Quality): Positive",                    
                "b_muneg_american_inf_2Verypositive" = "American influence (Quality): Very Positive",
                "b_muneg_demgovNeutral" = "Democratic Government: Neutral",                             
                "b_muneg_demgovNotimportant" = "Democratic Government: Not Important",
                "b_muneg_demgovSomewhatImportant" = "Democratic Government: Somewhat Important",                  
                "b_muneg_demgovVeryImportant" = "Democratic Government: Very Important",
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
                "b_muneg_religProtestant" = "Protestant",
                "b_muneg_religCatholicism" = "Catholicism",
                "b_muneg_religIslam" = "Islam",
                "b_muneg_religJudaism" = "Judaism",
                "b_muneg_religShinto" = "Shinto",
                "b_muneg_religBuddhism" = "Buddhism",
                "b_muneg_religHinduism" = "Hinduism",
                "b_muneg_religLocal" = "Local",
                "b_muneg_religMormonism" = "Mormonism",
                "b_muneg_religDecline to answer" = "Decline to answer",
                "b_muneg_religOther" = "Other",
                "b_muneg_minorityNo" = "Minority: Yes",                                
                "b_muneg_minorityYes" = "Minority: No",
                "b_muneg_spend_toa_combined_w_log" = "log(US Military Spending)",
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
                "b_mudk_american_inf_1DontknowDdeclinetoanswer" = "American influence (Degree): Don't know/Decline to answer", 
                "b_mudk_american_inf_1Alittle" = "American influence (Degree): A little",
                "b_mudk_american_inf_1Some" = "American influence (Degree): Some",
                "b_mudk_american_inf_1Alot" = "American influence (Degree): A lot",
                "b_mudk_american_inf_2DontknowDdeclinetoanswer" = "American influence (Quality): Don't know/Decline to answer",
                "b_mudk_american_inf_2Verynegative" = "American influence (Quality): Very Negative",
                "b_mudk_american_inf_2Negative" = "American influence (Quality): Negative",
                "b_mudk_american_inf_2Positive" = "American influence (Quality): Positive",                    
                "b_mudk_american_inf_2Verypositive" = "American influence (Quality): Very Positive",
                "b_mudk_demgovNeutral" = "Democratic Government: Neutral",                             
                "b_mudk_demgovNotimportant" = "Democratic Government: Not Important",
                "b_mudk_demgovSomewhatImportant" = "Democratic Government: Somewhat Important",                  
                "b_mudk_demgovVeryImportant" = "Democratic Government: Very Important",
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
                "b_mudk_religProtestant" = "Protestant",
                "b_mudk_religCatholicism" = "Catholicism",
                "b_mudk_religIslam" = "Islam",
                "b_mudk_religJudaism" = "Judaism",
                "b_mudk_religShinto" = "Shinto",
                "b_mudk_religBuddhism" = "Buddhism",
                "b_mudk_religHinduism" = "Hinduism",
                "b_mudk_religLocal" = "Local",
                "b_mudk_religMormonism" = "Mormonism",
                "b_mudk_religDecline to answer" = "Decline to answer",
                "b_mudk_religOther" = "Other",
                "b_mudk_minorityNo" = "Minority: Yes",                                
                "b_mudk_minorityYes" = "Minority: No",
                "b_mudk_spend_toa_combined_w_log" = "log(US Military Spending)",
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
                  "\\emph{Response:Positive - American Influence (Degree)}" = 9:12,
                  "\\emph{Response:Positive - American Influence (Quality)}" = 13:17,
                  "\\emph{Response:Positive - Democratic Government}" = 18:21,
                  "\\emph{Response:Positive - Gender Self-Identification}" = 22:24,
                  "\\emph{Response:Positive - Education}" = 25:25,
                  "\\emph{Response:Positive - Age Bracket}" = 26:30,
                  "\\emph{Response:Positive - Income Percentile}" = 31:35,
                  "\\emph{Response:Positive - Ideology}" = 36:36,
                  "\\emph{Response:Positive - Religious Self-Identification}" = 37:47,
                  "\\emph{Response:Positive - Minority Self-Identification}" = 48:49,
                  "\\emph{Response:Positive - Country-Level Variables}" = 50:57,
                  "\\emph{Response:Negative - Personal Contact}" = 58:59,
                  "\\emph{Response:Negative - Network Contact}"  = 60:61,
                  "\\emph{Response:Negative - Personal Benefit}" = 62:63,
                  "\\emph{Response:Negative - Network Benefit}" = 64:65,
                  "\\emph{Response:Negative - American Influence (Degree)}" = 66:69,
                  "\\emph{Response:Negative - American Influence (Quality)}" = 70:74,
                  "\\emph{Response:Negative - Democratic Government}" = 75:78,
                  "\\emph{Response:Negative - Gender Self-Identification}" = 79:81,
                  "\\emph{Response:Negative - Education}" = 82:82,
                  "\\emph{Response:Negative - Age Bracket}" = 83:87,
                  "\\emph{Response:Negative - Income Percentile}" = 88:92,
                  "\\emph{Response:Negative - Ideology}" = 93:93,
                  "\\emph{Response:Negative - Religious Self-Identification}" = 94:104,
                  "\\emph{Response:Negative - Minority Self-Identification}" = 105:106,
                  "\\emph{Response:Negative - Country-Level Variables}" = 107:114,
                  "\\emph{Response: Don't know/Decline - Personal Contact}" = 115:116,
                  "\\emph{Response: Don't know/Decline - Network Contact}"  = 117:118,
                  "\\emph{Response: Don't know/Decline - Personal Benefit}" = 119:120,
                  "\\emph{Response: Don't know/Decline - Network Benefit}" = 121:122,
                  "\\emph{Response: Don't know/Decline - American Influence (Degree)}" = 123:126,
                  "\\emph{Response: Don't know/Decline - American Influence (Quality)}" = 127:131,
                  "\\emph{Response: Don't know/Decline - Democratic Government}" = 132:134,
                  "\\emph{Response: Don't know/Decline - Gender Self-Identification}" = 136:138,
                  "\\emph{Response: Don't know/Decline - Education}" = 139:139,
                  "\\emph{Response: Don't know/Decline - Age Bracket}" = 140:144,
                  "\\emph{Response: Don't know/Decline - Income Percentile}" = 145:149,
                  "\\emph{Response: Don't know/Decline - Ideology}" = 150:150,
                  "\\emph{Response: Don't know/Decline - Religious Self-Identification}" = 151:161,
                  "\\emph{Response: Don't know/Decline - Minority Self-Identification}" = 162:163,
                  "\\emph{Response: Don't know/Decline - Country-Level Variables}" = 164:171)


comap.short <- list("contact_persDon't know/Decline to answer" = "PC: Don't know/Decline to answer", 
                    "contact_persYes" = "PC: Yes",
                    "contact_nonpersDon't know/Decline to answer" = "NC: Don't know/Decline to answer",
                    "contact_nonpersYes" = "NC: Yes",
                    "benefit_persDon't know/Decline to answer" = "PB: Don't know/Decline to answer", 
                    "benefit_persYes" = "PB: Yes",
                    "benefit_nonpersDon't know/Decline to answer" = "NB: Don't know/Decline to answer", 
                    "benefit_nonpersYes" = "NB: Yes")

group.list.short = list("\\emph{Personal Contact}" = 1:2,
                        "\\emph{Network Contact}"  = 3:4,
                        "\\emph{Personal Benefit}" = 5:6,
                        "\\emph{Network Benefit}" = 7:8)


# US Troops Model
table.cat.bayes.nobase <- mcmcreg(`m1.cat.bayes.nobase`,
                           pars = c('b_mupos', 'b_muneg', 'b_mudk'),
                           digits = 3,
                           single.row = TRUE,
                           custom.coef.names = comap.long,
                           groups = group.list,
                           caption = "Categorical Bayesian logit models predicting attitudes towards various United States actors",
                           caption.above = TRUE,
                           font = "normalsize",
                           label = "tab:catlogitbayestroops")


table.cat.bayes.nobase <- table.cat.bayes.nobase %>% # Selection 2018 spending values
  read_lines()  %>% 
  grep('\\&', ., value = TRUE) %>% 
  paste(collapse = '\\\\ \n') %>% 
  read_fwf(fwf_empty(., n = 200)) %>% # Have to Increase n here to make sure it doesn't cut off cells
  as.data.frame()

table.cat.bayes.nobase.pos <- table.cat.bayes.nobase[c(2:73), ]
table.cat.bayes.nobase.neg <- table.cat.bayes.nobase[c(74:145), ]
table.cat.bayes.nobase.dk <- table.cat.bayes.nobase[c(146:217), ]

table.cat.bayes.nobase.complete <- cbind(table.cat.bayes.nobase.pos, table.cat.bayes.nobase.neg, table.cat.bayes.nobase.dk)
table.cat.bayes.nobase.complete[, c(4,8)] <- "&"
colnames(table.cat.bayes.nobase.complete)[5] <- "X5"
colnames(table.cat.bayes.nobase.complete)[6] <- "X6"
colnames(table.cat.bayes.nobase.complete)[7] <- "X7"
colnames(table.cat.bayes.nobase.complete)[8] <- "X8"
colnames(table.cat.bayes.nobase.complete)[9] <- "X9"
colnames(table.cat.bayes.nobase.complete)[10] <- "X10"
colnames(table.cat.bayes.nobase.complete)[11] <- "X11"
colnames(table.cat.bayes.nobase.complete)[12] <- "X12"

X1 <- c("\\textbf{Random Effects}", "\\quad N", "\\quad Groups", "\\quad Std. Dev.") 
X3 <- c(" ",
        summary(`m1.cat.bayes.nobase`)[[4]],
        length(unique(`m1.cat.bayes.nobase`$data$country)),
        round(summary(`m1.cat.bayes.nobase`)$random$country[3], digits = 3))
X7 <- c(" ",
        summary(`m1.cat.bayes.nobase`)[[4]],
        length(unique(`m1.cat.bayes.nobase`$data$country)),
        round(summary(`m1.cat.bayes.nobase`)$random$country[2], digits = 3))
X11 <- c(" ",
         summary(`m1.cat.bayes.nobase`)[[4]],
         length(unique(`m1.cat.bayes.nobase`$data$country)),
         round(summary(`m1.cat.bayes.nobase`)$random$country[1], digits = 3))

table.cat.bayes.nobase.ranef <- data.frame(X1, X3, X7, X11)

table.cat.bayes.nobase.complete <- plyr::rbind.fill(table.cat.bayes.nobase.complete, table.cat.bayes.nobase.ranef) %>% 
  fill(c(X2, X4, X6, X8, X10, X12)) %>% 
  mutate(X1 = gsub("\\(\\d\\)", " ", X1)) %>% 
  mutate(X5 = gsub("\\(\\d\\)", " ", X5)) %>% 
  mutate(X9 = gsub("\\(\\d\\)", " ", X9)) %>% 
  mutate(X1 = gsub("Response:Positive - ", "", X1))

table.cat.bayes.nobase.complete <- table.cat.bayes.nobase.complete[, -c(5, 6, 9, 10)]
table.cat.bayes.nobase.complete[72, 8] <- "\\\\ \\hline "
table.cat.bayes.nobase.complete[73, 8] <- "\\\\ \\hline "

# Full table for appendix
write.table(table.cat.bayes.nobase.complete, here("Tables", "models-cat-bayes-nobase-m1-20190924.tex"),
            quote = FALSE,
            row.names = FALSE,
            col.names = FALSE,
            na = " ")




# US Government Model
table.cat.bayes.nobase <- mcmcreg(`m2.cat.bayes.nobase`,
                           pars = c('b_mupos', 'b_muneg', 'b_mudk'),
                           digits = 3,
                           single.row = TRUE,
                           custom.coef.names = comap.long,
                           groups = group.list,
                           caption = "Categorical Bayesian logit models predicting attitudes towards various United States actors",
                           caption.above = TRUE,
                           font = "normalsize",
                           label = "tab:catlogitbayesgovernment")


table.cat.bayes.nobase <- table.cat.bayes.nobase %>% # Selection 2018 spending values
  read_lines()  %>% 
  grep('\\&', ., value = TRUE) %>% 
  paste(collapse = '\\\\ \n') %>% 
  read_fwf(fwf_empty(., n = 200)) %>% # Have to Increase n here to make sure it doesn't cut off cells
  as.data.frame()


table.cat.bayes.nobase.pos <- table.cat.bayes.nobase[c(2:73), ]
table.cat.bayes.nobase.neg <- table.cat.bayes.nobase[c(74:145), ]
table.cat.bayes.nobase.dk <- table.cat.bayes.nobase[c(146:217), ]


table.cat.bayes.nobase.complete <- cbind(table.cat.bayes.nobase.pos, table.cat.bayes.nobase.neg, table.cat.bayes.nobase.dk)
table.cat.bayes.nobase.complete[, c(4,8)] <- "&"
colnames(table.cat.bayes.nobase.complete)[5] <- "X5"
colnames(table.cat.bayes.nobase.complete)[6] <- "X6"
colnames(table.cat.bayes.nobase.complete)[7] <- "X7"
colnames(table.cat.bayes.nobase.complete)[8] <- "X8"
colnames(table.cat.bayes.nobase.complete)[9] <- "X9"
colnames(table.cat.bayes.nobase.complete)[10] <- "X10"
colnames(table.cat.bayes.nobase.complete)[11] <- "X11"
colnames(table.cat.bayes.nobase.complete)[12] <- "X12"

X1 <- c("\\textbf{Random Effects}", "\\quad N", "\\quad Groups", "\\quad Std. Dev.") 
X3 <- c(" ",
        summary(`m2.cat.bayes.nobase`)[[4]],
        length(unique(`m2.cat.bayes.nobase`$data$country)),
        round(summary(`m2.cat.bayes.nobase`)$random$country[3], digits = 3))
X7 <- c(" ",
        summary(`m2.cat.bayes.nobase`)[[4]],
        length(unique(`m2.cat.bayes.nobase`$data$country)),
        round(summary(`m2.cat.bayes.nobase`)$random$country[2], digits = 3))
X11 <- c(" ",
         summary(`m2.cat.bayes.nobase`)[[4]],
         length(unique(`m2.cat.bayes.nobase`$data$country)),
         round(summary(`m2.cat.bayes.nobase`)$random$country[1], digits = 3))

table.cat.bayes.nobase.ranef <- data.frame(X1, X3, X7, X11)

table.cat.bayes.nobase.complete <- plyr::rbind.fill(table.cat.bayes.nobase.complete, table.cat.bayes.nobase.ranef) %>% 
  fill(c(X2, X4, X6, X8, X10, X12)) %>% 
  mutate(X1 = gsub("\\(\\d\\)", " ", X1)) %>% 
  mutate(X5 = gsub("\\(\\d\\)", " ", X5)) %>% 
  mutate(X9 = gsub("\\(\\d\\)", " ", X9)) %>% 
  mutate(X1 = gsub("Response:Positive - ", "", X1))

table.cat.bayes.nobase.complete <- table.cat.bayes.nobase.complete[, -c(5, 6, 9, 10)]
table.cat.bayes.nobase.complete[72, 8] <- "\\\\ \\hline "
table.cat.bayes.nobase.complete[73, 8] <- "\\\\ \\hline "


# Full table for appendix
write.table(table.cat.bayes.nobase.complete, here("Tables", "models-cat-bayes-nobase-m2-20190924.tex"),
            quote = FALSE,
            row.names = FALSE,
            col.names = FALSE,
            na = " ")






# US People Model
table.cat.bayes.nobase <- mcmcreg(`m3.cat.bayes.nobase`,
                           pars = c('b_mupos', 'b_muneg', 'b_mudk'),
                           digits = 3,
                           single.row = TRUE,
                           custom.coef.names = comap.long,
                           groups = group.list,
                           caption = "Categorical Bayesian logit models predicting attitudes towards various United States actors",
                           caption.above = TRUE,
                           font = "normalsize",
                           label = "tab:catlogitbayespeople")

table.cat.bayes.nobase <- table.cat.bayes.nobase %>% # Selection 2018 spending values
  read_lines()  %>% 
  grep('\\&', ., value = TRUE) %>% 
  paste(collapse = '\\\\ \n') %>% 
  read_fwf(fwf_empty(., n = 200)) %>% # Have to Increase n here to make sure it doesn't cut off cells
  as.data.frame()


table.cat.bayes.nobase.pos <- table.cat.bayes.nobase[c(2:73), ]
table.cat.bayes.nobase.neg <- table.cat.bayes.nobase[c(74:145), ]
table.cat.bayes.nobase.dk <- table.cat.bayes.nobase[c(146:217), ]


table.cat.bayes.nobase.complete <- cbind(table.cat.bayes.nobase.pos, table.cat.bayes.nobase.neg, table.cat.bayes.nobase.dk)
table.cat.bayes.nobase.complete[, c(4,8)] <- "&"
colnames(table.cat.bayes.nobase.complete)[5] <- "X5"
colnames(table.cat.bayes.nobase.complete)[6] <- "X6"
colnames(table.cat.bayes.nobase.complete)[7] <- "X7"
colnames(table.cat.bayes.nobase.complete)[8] <- "X8"
colnames(table.cat.bayes.nobase.complete)[9] <- "X9"
colnames(table.cat.bayes.nobase.complete)[10] <- "X10"
colnames(table.cat.bayes.nobase.complete)[11] <- "X11"
colnames(table.cat.bayes.nobase.complete)[12] <- "X12"

X1 <- c("\\textbf{Random Effects}", "\\quad N", "\\quad Groups", "\\quad Std. Dev.") 
X3 <- c(" ",
        summary(`m3.cat.bayes.nobase`)[[4]],
        length(unique(`m3.cat.bayes.nobase`$data$country)),
        round(summary(`m3.cat.bayes.nobase`)$random$country[3], digits = 3))
X7 <- c(" ",
        summary(`m3.cat.bayes.nobase`)[[4]],
        length(unique(`m3.cat.bayes.nobase`$data$country)),
        round(summary(`m3.cat.bayes.nobase`)$random$country[2], digits = 3))
X11 <- c(" ",
         summary(`m3.cat.bayes.nobase`)[[4]],
         length(unique(`m3.cat.bayes.nobase`$data$country)),
         round(summary(`m3.cat.bayes.nobase`)$random$country[1], digits = 3))

table.cat.bayes.nobase.ranef <- data.frame(X1, X3, X7, X11)

table.cat.bayes.nobase.complete <- plyr::rbind.fill(table.cat.bayes.nobase.complete, table.cat.bayes.nobase.ranef) %>% 
  fill(c(X2, X4, X6, X8, X10, X12)) %>% 
  mutate(X1 = gsub("\\(\\d\\)", " ", X1)) %>% 
  mutate(X5 = gsub("\\(\\d\\)", " ", X5)) %>% 
  mutate(X9 = gsub("\\(\\d\\)", " ", X9)) %>% 
  mutate(X1 = gsub("Response:Positive - ", "", X1))

table.cat.bayes.nobase.complete <- table.cat.bayes.nobase.complete[, -c(5, 6, 9, 10)]
table.cat.bayes.nobase.complete[72, 8] <- "\\\\ \\hline "
table.cat.bayes.nobase.complete[73, 8] <- "\\\\ \\hline "
# Full table for appendix
write.table(table.cat.bayes.nobase.complete, here("Tables", "models-cat-bayes-nobase-m3-20190924.tex"),
            quote = FALSE,
            row.names = FALSE,
            col.names = FALSE,
            na = " ")





