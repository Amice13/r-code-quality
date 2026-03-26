


# Categorical logit regressions with country fixed effects 

apsr.data$troops_1_cat <- relevel(apsr.data$troops_1_cat, ref = "neutral")
apsr.data$american_g_cat <- relevel(apsr.data$american_g_cat, ref = "neutral")
apsr.data$american_p_cat <- relevel(apsr.data$american_p_cat, ref = "neutral")

m1.mlogit <- multinom(troops_1_cat ~ contact_pers + contact_nonpers + benefit_pers + benefit_nonpers + american_inf_1 + american_inf_2 + demgov + gender + ed + age + income + ideology + relig + minority + spend_toa_combined_w_log + baseinprovince + defense + polity2 + gdp_constant_log + log_trade_total_2017 + log_students + log_threat_environment + log_troops_2017 + factor(country),
                    data = apsr.data,
                    maxit = 1000)

m2.mlogit <- multinom(american_g_cat ~ contact_pers + contact_nonpers + benefit_pers + benefit_nonpers + american_inf_1 + american_inf_2 + demgov + gender + ed + age + income + ideology + relig + minority + spend_toa_combined_w_log + baseinprovince + defense + polity2 + gdp_constant_log + log_trade_total_2017 + log_students + log_threat_environment + log_troops_2017 + factor(country),
                      data = apsr.data,
                      maxit = 1000)

m3.mlogit <- multinom(american_p_cat ~ contact_pers + contact_nonpers + benefit_pers + benefit_nonpers + american_inf_1 + american_inf_2 + demgov + gender + ed + age + income + ideology + relig + minority + spend_toa_combined_w_log + baseinprovince + defense + polity2 + gdp_constant_log + log_trade_total_2017 + log_students + log_threat_environment + log_troops_2017 + factor(country),
                      data = apsr.data,
                      maxit = 1000)


# Multinomial tables

gof.list <- list()
gof.list[[1]] <- extract.multinom(m1.mlogit, include.bic = TRUE, include.loglik = TRUE,
                                  include.nobs = TRUE, 
                                  levels = c("Don't know/Decline", "Negative", "Positive"), beside = TRUE)
gof.list[[2]] <- extract.multinom(m2.mlogit, include.bic = TRUE, include.loglik = TRUE,
                                  include.nobs = TRUE,
                                  levels = c("Don't know/Decline", "Negative", "Positive"), beside = TRUE)
gof.list[[3]] <- extract.multinom(m3.mlogit, include.bic = TRUE, include.loglik = TRUE,
                                  include.nobs = TRUE,
                                  levels = c("Don't know/Decline", "Negative", "Positive"), beside = TRUE)

comap.long <- list("contact_persDon't know/Decline to answer" = "PC: Don't know/Decline to answer", 
                "contact_persYes" = "PC: Yes",
                "contact_nonpersDon't know/Decline to answer" = "NC: Don't know/Decline to answer",
                "contact_nonpersYes" = "NC: Yes",
                "benefit_persDon't know/Decline to answer" = "PB: Don't know/Decline to answer", 
                "benefit_persYes" = "PB: Yes",
                "benefit_nonpersDon't know/Decline to answer" = "NB: Don't know/Decline to answer", 
                "benefit_nonpersYes" = "NB: Yes",
                "american_inf_1Don't know/decline to answer" = "American influence (Degree): Don't know/Decline to answer", 
                "american_inf_1A little" = "American influence (Degree): A little",
                "american_inf_1Some" = "American influence (Degree): Some",
                "american_inf_1A lot" = "American influence (Degree): A lot",
                "american_inf_2Don't know/decline to answer" = "American influence (Quality): Don't know/Decline to answer",
                "american_inf_2Very negative" = "American influence (Quality): Very Negative",
                "american_inf_2Negative" = "American influence (Quality): Negative",
                "american_inf_2Positive" = "American influence (Quality): Positive",                    
                "american_inf_2Very positive" = "American influence (Quality): Very Positive",
                "demgovNeutral" = "Democratic Government: Neutral",                             
                "demgovNot important" = "Democratic Government: Not Important",
                "demgovSomewhat Important" = "Democratic Government: Somewhat Important",                  
                "demgovVery Important" = "Democratic Government: Very Important",
                "genderFemale" = "Gender: Female",                               
                "genderNon-binary" = "Gender: Non-Binary",
                "genderNone of the above" = "Gender: None of the above",
                "ed" = "Education",
                "age25 to 34 years" = "Age: 25-34 years",
                "age35 to 44 years" = "Age: 35-44 years",
                "age45 to 54 years" = "Age: 45-54 years",
                "age55 to 64 years" = "Age: 55-64 years",
                "ageAge 65 or older" = "Age: 65 or older",                        
                "income17%-34%" = "Income Percentile: 17-34",
                "income35%-50%" = "Income Percentile: 35-50",
                "income51%-67%" = "Income Percentile: 51-67",
                "income65%-83%" = "Income Percentile: 65-83",
                "income84%-100%" = "Income Percentile: 84-100",
                "ideology" = "Ideology",
                "religProtestant" = "Protestant",
                "religCatholicism" = "Catholicism",
                "religIslam" = "Islam",
                "religJudaism" = "Judaism",
                "religShinto" = "Shinto",
                "religBuddhism" = "Buddhism",
                "religHinduism" = "Hinduism",
                "religLocal" = "Local",
                "religMormonism" = "Mormonism",
                "religDecline to answer" = "Decline to answer",
                "religOther" = "Other",
                "minorityNo" = "Minority: Yes",                                
                "minorityYes" = "Minority: No",
                "spend_toa_combined_w_log" = "log(US Military Spending)",
                "baseinprovince" = "Base in Respondent's Province",
                "defense" = "US Defense Pact",
                "log_threat_environment" = "log(Threat Environment)",
                "log_troops_2017" = "log(US Troops in Country, 2017)",
                "polity2" = "Polity Score",
                "gdp_constant_log" = "log(GDP)",
                "log_trade_total_2017" = "log(Total Trade with US)",
                "log_students" = "log(US Students in Respondent Country, 2017)",
                "factor(country)Belgium" = "Belgium",
                "factor(country)Germany" = "Germany",
                "factor(country)Italy" = "Italy", 
                "factor(country)Japan" = "Japan",
                "factor(country)Kuwait" = "Kuwait",
                "factor(country)Netherlands" = "Netherlands",
                "factor(country)Philippines" = "Philippines",
                "factor(country)Poland" = "Poland",
                "factor(country)Portugal" = "Portugal",
                "factor(country)South Korea" = "South Korea",
                "factor(country)Spain" = "Spain",
                "factor(country)Turkey" = "Turkey",
                "factor(country)United Kingdom" = "United Kingdom",
                "(Intercept)" = "Intercept")

group.list = list("\\emph{Personal Contact}" = 1:2,
                  "\\emph{Network Contact}"  = 3:4,
                  "\\emph{Personal Benefit}" = 5:6,
                  "\\emph{Network Benefit}" = 7:8,
                  "\\emph{American Influence (Degree)}" = 9:12,
                  "\\emph{American Influence (Quality)}" = 13:17,
                  "\\emph{Democratic Government}" = 18:21,
                  "\\emph{Gender Self-Identification}" = 22:24,
                  "\\emph{Education}" = 25:25,
                  "\\emph{Age Bracket}" = 26:30,
                  "\\emph{Income Percentile}" = 31:35,
                  "\\emph{Ideology}" = 36:36,
                  "\\emph{Religious Self-Identification}" = 37:47,
                  "\\emph{Minority Self-Identification}" = 48:49,
                  "\\emph{Country-Level Variables}" = 50:58,
                  "\\emph{Country Fixed Effects}" = 59:72)

multinomtab <- texreg(m1.mlogit,
                      single.row = TRUE,
                      stars = c(0.01, 0.05),
                      custom.coef.map = comap.long,
                      groups = group.list,
                      gof_map = gof.list[[1]],
                      caption = "Categorical logistic regressions predicting attitudes towards US troop presence.",
                      caption.above = TRUE,
                      label = "tab:catlogitregular1",
                      scalebox = .5,
                      file = here("Tables", "apsr-models-cat-regular-m1-20190930.tex"))

multinomtab <- texreg(m2.mlogit,
                      single.row = TRUE,
                      stars = c(0.01, 0.05),
                      custom.coef.map = comap.long,
                      groups = group.list,
                      gof_map = gof.list[[2]],
                      caption = "Categorical logistic regressions predicting attitudes towards the US government.",
                      caption.above = TRUE,
                      label = "tab:catlogitregular2",
                      scalebox = .5,
                      file = here("Tables", "apsr-models-cat-regular-m2-20190930.tex"))

multinomtab <- texreg(m3.mlogit,
                      single.row = TRUE,
                      stars = c(0.01, 0.05),
                      custom.coef.map = comap.long,
                      groups = group.list,
                      gof_map = gof.list[[3]],
                      caption = "Categorical logistic regressions predicting attitudes towards the US people.",
                      caption.above = TRUE,
                      label = "tab:catlogitregular3",
                      scalebox = .5,
                      file = here("Tables", "apsr-models-cat-regular-m3-20190930.tex"))











# Models containing security benefits question

apsr.data$troops_1_cat <- relevel(apsr.data$troops_1_cat, ref = "neutral")
apsr.data$american_g_cat <- relevel(apsr.data$american_g_cat, ref = "neutral")
apsr.data$american_p_cat <- relevel(apsr.data$american_p_cat, ref = "neutral")

m1.mlogit.sec <- multinom(troops_1_cat ~ contact_pers + contact_nonpers + benefit_pers + benefit_nonpers + american_inf_1 + american_inf_2 + demgov + troops_security + gender + ed + age + income + ideology + relig + minority + spend_toa_combined_w_log + baseinprovince + defense + polity2 + gdp_constant_log + log_trade_total_2017 + log_students + log_threat_environment + log_troops_2017 + factor(country),
                      data = apsr.data,
                      maxit = 1000)

m2.mlogit.sec <- multinom(american_g_cat ~ contact_pers + contact_nonpers + benefit_pers + benefit_nonpers + american_inf_1 + american_inf_2 + demgov + troops_security + gender + ed + age + income + ideology + relig + minority + spend_toa_combined_w_log + baseinprovince + defense + polity2 + gdp_constant_log + log_trade_total_2017 + log_students + log_threat_environment + log_troops_2017 + factor(country),
                      data = apsr.data,
                      maxit = 1000)

m3.mlogit.sec <- multinom(american_p_cat ~ contact_pers + contact_nonpers + benefit_pers + benefit_nonpers + american_inf_1 + american_inf_2 + demgov + troops_security + gender + ed + age + income + ideology + relig + minority + spend_toa_combined_w_log + baseinprovince + defense + polity2 + gdp_constant_log + log_trade_total_2017 + log_students + log_threat_environment + log_troops_2017 + factor(country),
                      data = apsr.data,
                      maxit = 1000)


# Multinomial tables

gof.list <- list()
gof.list[[1]] <- extract.multinom(m1.mlogit.sec, include.bic = TRUE, include.loglik = TRUE,
                                  include.nobs = TRUE, 
                                  levels = c("Don't know/Decline", "Negative", "Positive"), beside = TRUE)
gof.list[[2]] <- extract.multinom(m2.mlogit.sec, include.bic = TRUE, include.loglik = TRUE,
                                  include.nobs = TRUE,
                                  levels = c("Don't know/Decline", "Negative", "Positive"), beside = TRUE)
gof.list[[3]] <- extract.multinom(m3.mlogit.sec, include.bic = TRUE, include.loglik = TRUE,
                                  include.nobs = TRUE,
                                  levels = c("Don't know/Decline", "Negative", "Positive"), beside = TRUE)

comap.long <- list("contact_persDon't know/Decline to answer" = "PC: Don't know/Decline to answer", 
                   "contact_persYes" = "PC: Yes",
                   "contact_nonpersDon't know/Decline to answer" = "NC: Don't know/Decline to answer",
                   "contact_nonpersYes" = "NC: Yes",
                   "benefit_persDon't know/Decline to answer" = "PB: Don't know/Decline to answer", 
                   "benefit_persYes" = "PB: Yes",
                   "benefit_nonpersDon't know/Decline to answer" = "NB: Don't know/Decline to answer", 
                   "benefit_nonpersYes" = "NB: Yes",
                   "american_inf_1Don't know/decline to answer" = "American influence (Degree): Don't know/Decline to answer", 
                   "american_inf_1A little" = "American influence (Degree): A little",
                   "american_inf_1Some" = "American influence (Degree): Some",
                   "american_inf_1A lot" = "American influence (Degree): A lot",
                   "american_inf_2Don't know/decline to answer" = "American influence (Quality): Don't know/Decline to answer",
                   "american_inf_2Very negative" = "American influence (Quality): Very Negative",
                   "american_inf_2Negative" = "American influence (Quality): Negative",
                   "american_inf_2Positive" = "American influence (Quality): Positive",                    
                   "american_inf_2Very positive" = "American influence (Quality): Very Positive",
                   "demgovNeutral" = "Democratic Government: Neutral",                             
                   "demgovNot important" = "Democratic Government: Not Important",
                   "demgovSomewhat Important" = "Democratic Government: Somewhat Important",                  
                   "demgovVery Important" = "Democratic Government: Very Important",
                   "troops_securityDon't know/decline to answer" = "Security: Don't know/Decline to answer",
                   "troops_securityVery unhelpful" = "Security: Very Unhelpful",
                   "troops_securitySomewhat unhelpful" = "Security: Somewhat Unhelpful",
                   "troops_securitySomewhat helpful" = "Security: Somewhat Helpful",
                   "troops_securityVery helpful" = "Security: Very Helpful",
                   "genderFemale" = "Gender: Female",                               
                   "genderNon-binary" = "Gender: Non-Binary",
                   "genderNone of the above" = "Gender: None of the above",
                   "ed" = "Education",
                   "age25 to 34 years" = "Age: 25-34 years",
                   "age35 to 44 years" = "Age: 35-44 years",
                   "age45 to 54 years" = "Age: 45-54 years",
                   "age55 to 64 years" = "Age: 55-64 years",
                   "ageAge 65 or older" = "Age: 65 or older",                        
                   "income17%-34%" = "Income Percentile: 17-34",
                   "income35%-50%" = "Income Percentile: 35-50",
                   "income51%-67%" = "Income Percentile: 51-67",
                   "income65%-83%" = "Income Percentile: 65-83",
                   "income84%-100%" = "Income Percentile: 84-100",
                   "ideology" = "Ideology",
                   "religProtestant" = "Protestant",
                   "religCatholicism" = "Catholicism",
                   "religIslam" = "Islam",
                   "religJudaism" = "Judaism",
                   "religShinto" = "Shinto",
                   "religBuddhism" = "Buddhism",
                   "religHinduism" = "Hinduism",
                   "religLocal" = "Local",
                   "religMormonism" = "Mormonism",
                   "religDecline to answer" = "Decline to answer",
                   "religOther" = "Other",
                   "minorityNo" = "Minority: Yes",                                
                   "minorityYes" = "Minority: No",
                   "spend_toa_combined_w_log" = "log(US Military Spending)",
                   "baseinprovince" = "Base in Respondent's Province",
                   "defense" = "US Defense Pact",
                   "log_threat_environment" = "log(Threat Environment)",
                   "log_troops_2017" = "log(US Troops in Country, 2017)",
                   "polity2" = "Polity Score",
                   "gdp_constant_log" = "log(GDP)",
                   "log_trade_total_2017" = "log(Total Trade with US)",
                   "log_students" = "log(US Students in Respondent Country, 2017)",
                   "factor(country)Belgium" = "Belgium",
                   "factor(country)Germany" = "Germany",
                   "factor(country)Italy" = "Italy", 
                   "factor(country)Japan" = "Japan",
                   "factor(country)Kuwait" = "Kuwait",
                   "factor(country)Netherlands" = "Netherlands",
                   "factor(country)Philippines" = "Philippines",
                   "factor(country)Poland" = "Poland",
                   "factor(country)Portugal" = "Portugal",
                   "factor(country)South Korea" = "South Korea",
                   "factor(country)Spain" = "Spain",
                   "factor(country)Turkey" = "Turkey",
                   "factor(country)United Kingdom" = "United Kingdom",
                   "(Intercept)" = "Intercept")

group.list = list("\\emph{Personal Contact}" = 1:2,
                  "\\emph{Network Contact}"  = 3:4,
                  "\\emph{Personal Benefit}" = 5:6,
                  "\\emph{Network Benefit}" = 7:8,
                  "\\emph{American Influence (Degree)}" = 9:12,
                  "\\emph{American Influence (Quality)}" = 13:17,
                  "\\emph{Democratic Government}" = 18:21,
                  "\\emph{Security Benefits}" = 22:26,
                  "\\emph{Gender Self-Identification}" = 27:29,
                  "\\emph{Education}" = 30:30,
                  "\\emph{Age Bracket}" = 31:35,
                  "\\emph{Income Percentile}" = 36:40,
                  "\\emph{Ideology}" = 41:41,
                  "\\emph{Religious Self-Identification}" = 42:52,
                  "\\emph{Minority Self-Identification}" = 53:54,
                  "\\emph{Country-Level Variables}" = 55:63,
                  "\\emph{Country Fixed Effects}" = 64:77)

multinomtab <- texreg(m1.mlogit.sec,
                      single.row = TRUE,
                      stars = c(0.01, 0.05),
                      custom.coef.map = comap.long,
                      groups = group.list,
                      gof_map = gof.list[[1]],
                      caption = "Categorical logistic regressions predicting attitudes towards US troop presence. Models contain question about security benefits.",
                      caption.above = TRUE,
                      label = "tab:catlogitregular1",
                      scalebox = .45,
                      file = here("Tables", "apsr-models-cat-regular-m1-sec-20190930.tex"))

multinomtab <- texreg(m2.mlogit.sec,
                      single.row = TRUE,
                      stars = c(0.01, 0.05),
                      custom.coef.map = comap.long,
                      groups = group.list,
                      gof_map = gof.list[[2]],
                      caption = "Categorical logistic regressions predicting attitudes towards the US government. Models contain question about security benefits.",
                      caption.above = TRUE,
                      label = "tab:catlogitregular2",
                      scalebox = .45,
                      file = here("Tables", "apsr-models-cat-regular-m2-sec-20190930.tex"))

multinomtab <- texreg(m3.mlogit.sec,
                      single.row = TRUE,
                      stars = c(0.01, 0.05),
                      custom.coef.map = comap.long,
                      groups = group.list,
                      gof_map = gof.list[[3]],
                      caption = "Categorical logistic regressions predicting attitudes towards the US people. Models contain question about security benefits.",
                      caption.above = TRUE,
                      label = "tab:catlogitregular3",
                      scalebox = .45,
                      file = here("Tables", "apsr-models-cat-regular-m3-sec-20190930.tex"))



