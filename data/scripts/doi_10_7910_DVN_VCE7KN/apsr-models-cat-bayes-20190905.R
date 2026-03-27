


# WARNING: THESE MODELS WILL TAKE ~36 HOURS TO RUN. PLAN ACCORDINGLY.

# Read in data
apsr.data <- read.csv(here("Data", "apsr-data-20190905.csv"))


# Parallelize machine
options(mc.cores = parallel::detectCores())
ncores = detectCores()
rstan_options(auto_write = TRUE)

# Set vague priors
vagueprior <- set_prior("normal(0, 100)", class = "b")

time.1 <- Sys.time()
# Models with country-level variables
# Troops Question
m1.cat.bayes <- brm(troops_1_cat ~ contact_pers + contact_nonpers + benefit_pers + benefit_nonpers + american_inf_1 + american_inf_2 + demgov + gender + ed + age + income + ideology + relig + minority + spend_toa_combined_w_log + baseinprovince + defense + log_threat_environment + log_troops_2017 + polity2 + gdp_constant_log + log_trade_total_2017 + log_students + (1 | country),
                    data = apsr.data,
                    prior = vagueprior,
                    iter = 14000,
                    warmup = 4000,
                    cores = ncores,
                    chains = 4,
                    thin = 3,
                    seed = 66502,
                    file = here("Bayes Diagnostics", "m1-cat-bayes-output"),
                    family = categorical(link = "logit", refcat = "neutral"))
time.2 <- Sys.time()

print(time.2-time.1)

m1.cat.diag <- ggs(m1.cat.bayes)
ggmcmc(m1.cat.diag, file="m1-cat-bayes-diag.pdf")


# US Government Question
m2.cat.bayes <- brm(american_g_cat ~ contact_pers + contact_nonpers + benefit_pers + benefit_nonpers + american_inf_1 + american_inf_2 + demgov + gender + ed + age + income + ideology + relig + minority + spend_toa_combined_w_log + baseinprovince + defense + log_threat_environment + log_troops_2017 + polity2 + gdp_constant_log + log_trade_total_2017 + log_students + (1 | country),
                    data = apsr.data,
                    prior = vagueprior,
                    iter = 14000,
                    warmup = 4000,
                    cores = ncores,
                    chains = 4,
                    thin = 3,
                    seed = 66502,
                    file = here("Bayes Diagnostics", "m2-cat-bayes-output"),
                    family = categorical(link = "logit", refcat = "neutral"))

m2.cat.diag <- ggs(m2.cat.bayes)
ggmcmc(m2.cat.diag, file="m2-cat-bayes-diag.pdf")


# US People Question
m3.cat.bayes <- brm(american_p_cat ~ contact_pers + contact_nonpers + benefit_pers + benefit_nonpers + american_inf_1 + american_inf_2 + demgov + gender + ed + age + income + ideology + relig + minority + spend_toa_combined_w_log + baseinprovince + defense + log_threat_environment + log_troops_2017 + polity2 + gdp_constant_log + log_trade_total_2017 + log_students + (1 | country),
                    data = apsr.data,
                    prior = vagueprior,
                    iter = 14000,
                    warmup = 4000,
                    cores = ncores,
                    chains = 4,
                    thin = 3,
                    seed = 66502,
                    file = here("Bayes Diagnostics", "m3-cat-bayes-output"),
                    family = categorical(link = "logit", refcat = "neutral"))


m3.cat.diag <- ggs(m3.cat.bayes)
ggmcmc(m3.cat.diag, file="m3-cat-bayes-diag.pdf")




# Tables
modellist <- list()
modellist[['US Troops']] <- `m1-cat-bayes-output`
modellist[['US Government']] <- `m2-cat-bayes-output`
modellist[['US People']] <- `m3-cat-bayes-output`


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
                "b_mupos_baseinprovince" = "Base in Respondent's Province",
                "b_mupos_defense" = "US Defense Pact",
                "b_mupos_polity2" = "Polity Score",
                "b_mupos_gdp_constant_log" = "log(GDP)",
                "b_mupos_log_trade_total_2017" = "log(Total Trade with US)",
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
                "b_muneg_baseinprovince" = "Base in Respondent's Province",
                "b_muneg_defense" = "US Defense Pact",
                "b_muneg_polity2" = "Polity Score",
                "b_muneg_gdp_constant_log" = "log(GDP)",
                "b_muneg_log_trade_total_2017" = "log(Total Trade with US)",
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
                "b_mudk_baseinprovince" = "Base in Respondent's Province",
                "b_mudk_defense" = "US Defense Pact",
                "b_mudk_polity2" = "Polity Score",
                "b_mudk_gdp_constant_log" = "log(GDP)",
                "b_mudk_log_trade_total_2017" = "log(Total Trade with US)")

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
                  "\\emph{Response:Positive - Country-Level Variables}" = 50:55,
                  "\\emph{Response:Negative - Personal Contact}" = 56:57,
                  "\\emph{Response:Negative - Network Contact}"  = 58:59,
                  "\\emph{Response:Negative - Personal Benefit}" = 60:61,
                  "\\emph{Response:Negative - Network Benefit}" = 62:63,
                  "\\emph{Response:Negative - American Influence (Degree)}" = 64:67,
                  "\\emph{Response:Negative - American Influence (Quality)}" = 68:72,
                  "\\emph{Response:Negative - Democratic Government}" = 73:76,
                  "\\emph{Response:Negative - Gender Self-Identification}" = 77:79,
                  "\\emph{Response:Negative - Education}" = 80:80,
                  "\\emph{Response:Negative - Age Bracket}" = 81:85,
                  "\\emph{Response:Negative - Income Percentile}" = 86:90,
                  "\\emph{Response:Negative - Ideology}" = 91:91,
                  "\\emph{Response:Negative - Religious Self-Identification}" = 92:102,
                  "\\emph{Response:Negative - Minority Self-Identification}" = 103:104,
                  "\\emph{Response:Negative - Country-Level Variables}" = 105:110,
                  "\\emph{Response: Don't know/Decline - Personal Contact}" = 111:112,
                  "\\emph{Response: Don't know/Decline - Network Contact}"  = 113:114,
                  "\\emph{Response: Don't know/Decline - Personal Benefit}" = 115:116,
                  "\\emph{Response: Don't know/Decline - Network Benefit}" = 117:118,
                  "\\emph{Response: Don't know/Decline - American Influence (Degree)}" = 119:122,
                  "\\emph{Response: Don't know/Decline - American Influence (Quality)}" = 123:127,
                  "\\emph{Response: Don't know/Decline - Democratic Government}" = 128:131,
                  "\\emph{Response: Don't know/Decline - Gender Self-Identification}" = 132:134,
                  "\\emph{Response: Don't know/Decline - Education}" = 135:135,
                  "\\emph{Response: Don't know/Decline - Age Bracket}" = 136:140,
                  "\\emph{Response: Don't know/Decline - Income Percentile}" = 141:145,
                  "\\emph{Response: Don't know/Decline - Ideology}" = 146:146,
                  "\\emph{Response: Don't know/Decline - Religious Self-Identification}" = 147:157,
                  "\\emph{Response: Don't know/Decline - Minority Self-Identification}" = 158:159,
                  "\\emph{Response: Don't know/Decline - Country-Level Variables}" = 160:165)


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
table.cat.bayes <- mcmcreg(`m1-cat-bayes-output`,
                           pars = c('b_mupos', 'b_muneg', 'b_mudk'),
                           digits = 3,
                           single.row = TRUE,
                           custom.coef.names = comap.long,
                           groups = group.list,
                           caption = "Categorical Bayesian logit models predicting attitudes towards various United States actors",
                           caption.above = TRUE,
                           font = "normalsize",
                           label = "tab:catlogitbayestroops")

table.cat.bayes <- table.cat.bayes %>% # Selection 2018 spending values
  read_lines()  %>% 
  grep('\\&', ., value = TRUE) %>% 
  paste(collapse = '\\\\ \n') %>% 
  read_fwf(fwf_empty(., n = 200)) %>% # Have to Increase n here to make sure it doesn't cut off cells
  as.data.frame()

table.cat.bayes.pos <- table.cat.bayes[c(2:71), ]
table.cat.bayes.neg <- table.cat.bayes[c(72:141), ]
table.cat.bayes.dk <- table.cat.bayes[c(142:211), ]

table.cat.bayes.complete <- cbind(table.cat.bayes.pos, table.cat.bayes.neg, table.cat.bayes.dk)
table.cat.bayes.complete[, c(4,8)] <- "&"
colnames(table.cat.bayes.complete)[5] <- "X5"
colnames(table.cat.bayes.complete)[6] <- "X6"
colnames(table.cat.bayes.complete)[7] <- "X7"
colnames(table.cat.bayes.complete)[8] <- "X8"
colnames(table.cat.bayes.complete)[9] <- "X9"
colnames(table.cat.bayes.complete)[10] <- "X10"
colnames(table.cat.bayes.complete)[11] <- "X11"
colnames(table.cat.bayes.complete)[12] <- "X12"

X1 <- c("\\textbf{Random Effects}", "\\quad N", "\\quad Groups", "\\quad Std. Dev.") 
X3 <- c(" ",
        summary(`m1-cat-bayes-output`)[[4]],
        length(unique(`m1-cat-bayes-output`$data$country)),
        round(summary(`m1-cat-bayes-output`)$random$country[3], digits = 3))
X7 <- c(" ",
        summary(`m2-cat-bayes-output`)[[4]],
        length(unique(`m2-cat-bayes-output`$data$country)),
        round(summary(`m1-cat-bayes-output`)$random$country[2], digits = 3))
X11 <- c(" ",
         summary(`m2-cat-bayes-output`)[[4]],
         length(unique(`m2-cat-bayes-output`$data$country)),
         round(summary(`m1-cat-bayes-output`)$random$country[1], digits = 3))

table.cat.bayes.ranef <- data.frame(X1, X3, X7, X11)

table.cat.bayes.complete <- plyr::rbind.fill(table.cat.bayes.complete, table.cat.bayes.ranef)%>% 
  fill(c(X2, X4, X6, X8, X10, X12)) %>% 
  mutate(X1 = gsub("\\(\\d\\)", " ", X1)) %>% 
  mutate(X5 = gsub("\\(\\d\\)", " ", X5)) %>% 
  mutate(X9 = gsub("\\(\\d\\)", " ", X9)) %>% 
  mutate(X1 = gsub("Response:Positive - ", "", X1))

table.cat.bayes.complete <- table.cat.bayes.complete[, -c(5, 6, 9, 10)]
table.cat.bayes.complete[70, 8] <- "\\\\ \\hline "
table.cat.bayes.complete[71, 8] <- "\\\\ \\hline "

# Full table for appendix
write.table(table.cat.bayes.complete, here("Tables", "models-cat-bayes-m1-20190818.tex"),
            quote = FALSE,
            row.names = FALSE,
            col.names = FALSE,
            na = " ")

# Short table for in-text reference
table.cat.bayes.complete.short <- table.cat.bayes.complete[-c(13:70),]
table.cat.bayes.complete.short[12, 8] <- "\\\\ \\hline "

write.table(table.cat.bayes.complete.short, here("Tables", "models-cat-bayes-m1-short-20190818.tex"),
            quote = FALSE,
            row.names = FALSE,
            col.names = FALSE,
            na = " ")



# US Government Model
table.cat.bayes <- mcmcreg(`m2-cat-bayes-output`,
                           pars = c('b_mupos', 'b_muneg', 'b_mudk'),
                           digits = 3,
                           single.row = TRUE,
                           custom.coef.names = comap.long,
                           groups = group.list,
                           caption = "Categorical Bayesian logit models predicting attitudes towards various United States actors",
                           caption.above = TRUE,
                           font = "normalsize",
                           label = "tab:catlogitbayesgovernment")
#file = here("Tables", "models-cat-bayes-20180818"))

table.cat.bayes <- table.cat.bayes %>% # Selection 2018 spending values
  read_lines()  %>% 
  grep('\\&', ., value = TRUE) %>% 
  paste(collapse = '\\\\ \n') %>% 
  read_fwf(fwf_empty(., n = 200)) %>% # Have to Increase n here to make sure it doesn't cut off cells
  as.data.frame()

table.cat.bayes.pos <- table.cat.bayes[c(2:71), ]
table.cat.bayes.neg <- table.cat.bayes[c(72:141), ]
table.cat.bayes.dk <- table.cat.bayes[c(142:211), ]

table.cat.bayes.complete <- cbind(table.cat.bayes.pos, table.cat.bayes.neg, table.cat.bayes.dk)
table.cat.bayes.complete[, c(4,8)] <- "&"
colnames(table.cat.bayes.complete)[5] <- "X5"
colnames(table.cat.bayes.complete)[6] <- "X6"
colnames(table.cat.bayes.complete)[7] <- "X7"
colnames(table.cat.bayes.complete)[8] <- "X8"
colnames(table.cat.bayes.complete)[9] <- "X9"
colnames(table.cat.bayes.complete)[10] <- "X10"
colnames(table.cat.bayes.complete)[11] <- "X11"
colnames(table.cat.bayes.complete)[12] <- "X12"

X1 <- c("\\textbf{Random Effects}", "\\quad N", "\\quad Groups", "\\quad Std. Dev.") 
X3 <- c(" ",
        summary(`m2-cat-bayes-output`)[[4]],
        length(unique(`m2-cat-bayes-output`$data$country)),
        round(summary(`m2-cat-bayes-output`)$random$country[3], digits = 3))
X7 <- c(" ",
        summary(`m2-cat-bayes-output`)[[4]],
        length(unique(`m2-cat-bayes-output`$data$country)),
        round(summary(`m2-cat-bayes-output`)$random$country[2], digits = 3))
X11 <- c(" ",
         summary(`m2-cat-bayes-output`)[[4]],
         length(unique(`m2-cat-bayes-output`$data$country)),
         round(summary(`m2-cat-bayes-output`)$random$country[1], digits = 3))

table.cat.bayes.ranef <- data.frame(X1, X3, X7, X11)

table.cat.bayes.complete <- plyr::rbind.fill(table.cat.bayes.complete, table.cat.bayes.ranef)%>% 
  fill(c(X2, X4, X6, X8, X10, X12)) %>% 
  mutate(X1 = gsub("\\(\\d\\)", " ", X1)) %>% 
  mutate(X5 = gsub("\\(\\d\\)", " ", X5)) %>% 
  mutate(X9 = gsub("\\(\\d\\)", " ", X9)) %>% 
  mutate(X1 = gsub("Response:Positive - ", "", X1))

table.cat.bayes.complete <- table.cat.bayes.complete[, -c(5, 6, 9, 10)]
table.cat.bayes.complete[70, 8] <- "\\\\ \\hline "
table.cat.bayes.complete[71, 8] <- "\\\\ \\hline "

# Full table for appendix
write.table(table.cat.bayes.complete, here("Tables", "models-cat-bayes-m2-20190818.tex"),
            quote = FALSE,
            row.names = FALSE,
            col.names = FALSE,
            na = " ")

# Short table for in-text reference
table.cat.bayes.complete.short <- table.cat.bayes.complete[-c(13:70),]
table.cat.bayes.complete.short[12, 8] <- "\\\\ \\hline "

write.table(table.cat.bayes.complete.short, here("Tables", "models-cat-bayes-m2-short-20190818.tex"),
            quote = FALSE,
            row.names = FALSE,
            col.names = FALSE,
            na = " ")


# US People Model
table.cat.bayes <- mcmcreg(`m3-cat-bayes-output`,
                           pars = c('b_mupos', 'b_muneg', 'b_mudk'),
                           digits = 3,
                           single.row = TRUE,
                           custom.coef.names = comap.long,
                           groups = group.list,
                           caption = "Categorical Bayesian logit models predicting attitudes towards various United States actors",
                           caption.above = TRUE,
                           font = "normalsize",
                           label = "tab:catlogitbayespeople")
#file = here("Tables", "models-cat-bayes-20180818"))

table.cat.bayes <- table.cat.bayes %>% # Selection 2018 spending values
  read_lines()  %>% 
  grep('\\&', ., value = TRUE) %>% 
  paste(collapse = '\\\\ \n') %>% 
  read_fwf(fwf_empty(., n = 200)) %>% # Have to Increase n here to make sure it doesn't cut off cells
  as.data.frame()

table.cat.bayes.pos <- table.cat.bayes[c(2:71), ]
table.cat.bayes.neg <- table.cat.bayes[c(72:141), ]
table.cat.bayes.dk <- table.cat.bayes[c(142:211), ]

table.cat.bayes.complete <- cbind(table.cat.bayes.pos, table.cat.bayes.neg, table.cat.bayes.dk)
table.cat.bayes.complete[, c(4,8)] <- "&"
colnames(table.cat.bayes.complete)[5] <- "X5"
colnames(table.cat.bayes.complete)[6] <- "X6"
colnames(table.cat.bayes.complete)[7] <- "X7"
colnames(table.cat.bayes.complete)[8] <- "X8"
colnames(table.cat.bayes.complete)[9] <- "X9"
colnames(table.cat.bayes.complete)[10] <- "X10"
colnames(table.cat.bayes.complete)[11] <- "X11"
colnames(table.cat.bayes.complete)[12] <- "X12"

X1 <- c("\\textbf{Random Effects}", "\\quad N", "\\quad Groups", "\\quad Std. Dev.") 
X3 <- c(" ",
        summary(`m3-cat-bayes-output`)[[4]],
        length(unique(`m3-cat-bayes-output`$data$country)),
        round(summary(`m3-cat-bayes-output`)$random$country[3], digits = 3))
X7 <- c(" ",
        summary(`m3-cat-bayes-output`)[[4]],
        length(unique(`m3-cat-bayes-output`$data$country)),
        round(summary(`m3-cat-bayes-output`)$random$country[2], digits = 3))
X11 <- c(" ",
         summary(`m3-cat-bayes-output`)[[4]],
         length(unique(`m3-cat-bayes-output`$data$country)),
         round(summary(`m3-cat-bayes-output`)$random$country[1], digits = 3))

table.cat.bayes.ranef <- data.frame(X1, X3, X7, X11)

table.cat.bayes.complete <- plyr::rbind.fill(table.cat.bayes.complete, table.cat.bayes.ranef)%>% 
  fill(c(X2, X4, X6, X8, X10, X12)) %>% 
  mutate(X1 = gsub("\\(\\d\\)", " ", X1)) %>% 
  mutate(X5 = gsub("\\(\\d\\)", " ", X5)) %>% 
  mutate(X9 = gsub("\\(\\d\\)", " ", X9)) %>% 
  mutate(X1 = gsub("Response:Positive - ", "", X1))

table.cat.bayes.complete <- table.cat.bayes.complete[, -c(5, 6, 9, 10)]
table.cat.bayes.complete[70, 8] <- "\\\\ \\hline "
table.cat.bayes.complete[71, 8] <- "\\\\ \\hline "

# Full table for appendix
write.table(table.cat.bayes.complete, here("Tables", "models-cat-bayes-m3-20190818.tex"),
            quote = FALSE,
            row.names = FALSE,
            col.names = FALSE,
            na = " ")

# Short table for in-text reference
table.cat.bayes.complete.short <- table.cat.bayes.complete[-c(13:70),]
table.cat.bayes.complete.short[12, 8] <- "\\\\ \\hline "

write.table(table.cat.bayes.complete.short, here("Tables", "models-cat-bayes-m3-short-20190818.tex"),
            quote = FALSE,
            row.names = FALSE,
            col.names = FALSE,
            na = " ")


# Descriptive figures for primary covariates
table.sumstats.cat.bayes <- `m1-cat-bayes-output`$data %>% 
  mutate(gdp_constant_log = as.numeric(round(gdp_constant_log, digits = 2)),
         log_trade_total_2017 = as.numeric(round(log_trade_total_2017, digits = 2)),
         spend_toa_combined_w_log = as.numeric(round(spend_toa_combined_w_log, digits = 2)),
         baseinprovince = factor(baseinprovince, levels = c(0, 1), labels = c("No", "Yes")),
         defense = factor(defense, levels = c(0, 1), labels = c("No", "Yes"))) %>% 
  group_by(country) %>% 
  gather() %>% 
  arrange(key, value) %>% 
  mutate(type = ifelse(key == "gdp_constant_log" | 
                         key == "log_trade_total_2017" | 
                         key == "spend_toa_combined_w_log" |
                         key == "ed" |
                         key == "ideology" |
                         key == "polity2", "Numeric", "Factor"))

# Factor variables
ggplot(data = table.sumstats.cat.bayes %>% filter(type == "Factor"), aes(x = reorder(value, as.numeric(value)))) +
  geom_bar() +
  facet_wrap("key", scales = "free_x") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 30, size = 5)) +
  labs(x = "",
       y = "Count")

ggsave(here("Figures", "apsr-figure-descriptive-1.pdf"))

# Numeric variables
table.sumstats.cat.bayes.numeric <- table.sumstats.cat.bayes %>% 
  ungroup() %>% 
  filter(type == "Numeric") %>% 
  mutate(value = as.numeric(value))

ggplot(data = table.sumstats.cat.bayes.numeric, aes(x = value)) +
  geom_density(bw = .4 , fill = "gray60", alpha = .6) +
  facet_wrap("key", scales = "free_x") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 30, size = 5)) +
  labs(x = "",
       y = "Density")

ggsave(here("Figures", "apsr-figure-descriptive-2.pdf"))



