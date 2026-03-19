
# Note: Make sure ordinal and nlme packages are detached when using lme4. 
# They conflict with ranef command.

  # Troops Question
  m1 <- clmm(troops_1_ord ~ contact_pers + contact_nonpers + benefit_pers + benefit_nonpers + american_inf_1 + american_inf_2 + demgov + gender + ed + age + income + ideology + relig + minority + (1 | country),
              data = apsr.data,
              nAGQ = 1,
              link = "logit")
  
  # Correlation matrix
  mat.1 <- as.matrix(cov2cor(vcov(m1)))
 
  ggcorrplot(mat.1, lab = TRUE, lab_size = 1.5, type = "full",) +
    theme(axis.text.y = element_text(size = 5),
          axis.text.x = element_text(size = 5)) +
    labs(title = "Correlation Matrix for Multilevel Ordered Logit Model 1 - Troop Presence")
  ggsave("apsr-figure-corrmatrix-ordered-1.pdf", width = 8, height = 10, units = "in", path = "./Figures/")

  
# US Government Question
  m2 <- clmm(american_gov_ord ~ contact_pers + contact_nonpers + benefit_pers + benefit_nonpers + american_inf_1 + american_inf_2 + demgov + gender + ed + age + income + ideology + relig + minority + (1 | country),
             data = apsr.data,
             nAGQ = 1,
             link = "logit")
  
  # Correlation matrix
  mat.2 <- as.matrix(cov2cor(vcov(m2)))
  
  ggcorrplot(mat.2, lab = TRUE, lab_size = 1.5, type = "full") +
    theme(axis.text.y = element_text(size = 5),
          axis.text.x = element_text(size = 5)) +
    labs(title = "Correlation Matrix for Multilevel Ordered Logit Model 2 - US Government")
  
  ggsave("apsrfigure-corrmatrix-ordered-2.pdf", width = 8, height = 10, units = "in", path = "./Figures/")
  
  
# US People Question
  m3 <- clmm(american_people_ord ~ contact_pers + contact_nonpers + benefit_pers + benefit_nonpers + american_inf_1 + american_inf_2 + demgov + gender + ed + age + income + ideology + relig + minority + (1 | country),
             data = apsr.data,
             nAGQ = 1,
             link = "logit")  

  # Correlation matrix
  mat.3 <- as.matrix(cov2cor(vcov(m3)))
  
  ggcorrplot(mat.3, lab = TRUE, lab_size = 1.5, type = "full") +
    theme(axis.text.y = element_text(size = 5),
          axis.text.x = element_text(size = 5)) +
    labs(title = "Correlation Matrix for Multilevel Ordered Logit Model 3 - US People")
  
  ggsave("apsrfigure-corrmatrix-ordered-3.pdf", width = 8, height = 10, units = "in", path = "./Figures/")
  
  
  
# Tables
modellist <- list()
modellist[['US Troops']] <- m1
modellist[['US Government']] <- m2
modellist[['US People']] <- m3

comap.long <- c("contact_persDontknowDDeclinetoanswer" = "PC: Don't know/Decline to answer", 
                "contact_persYes" = "PC: Yes",
                "contact_nonpersDontknowDDeclinetoanswer" = "NC: Don't know/Decline to answer",
                "contact_nonpersYes" = "NC: Yes",
                "benefit_persDontknowDDeclinetoanswer" = "PB: Don't know/Decline to answer", 
                "benefit_persYes" = "PB: Yes",
                "benefit_nonpersDontknowDDeclinetoanswer" = "NB: Don't know/Decline to answer", 
                "benefit_nonpersYes" = "NB: Yes",
                "american_inf_1DontknowDdeclinetoanswer" = "American influence (Degree): Don't know/Decline to answer", 
                "american_inf_1Alittle" = "American influence (Degree): A little",
                "american_inf_1Some" = "American influence (Degree): Some",
                "american_inf_1Alot" = "American influence (Degree): A lot",
                "american_inf_2DontknowDdeclinetoanswer" = "American influence (Quality): Don't know/Decline to answer",
                "american_inf_2Verynegative" = "American influence (Quality): Very Negative",
                "american_inf_2Negative" = "American influence (Quality): Negative",
                "american_inf_2Positive" = "American influence (Quality): Positive",                    
                "american_inf_2Verypositive" = "American influence (Quality): Very Positive",
                "demgovNeutral" = "Democratic Government: Neutral",                             
                "demgovNotimportant" = "Democratic Government: Not Important",
                "demgovSomewhatImportant" = "Democratic Government: Somewhat Important",                  
                "demgovVeryImportant" = "Democratic Government: Very Important",
                "genderFemale" = "Gender: Female",                               
                "genderNonMbinary" = "Gender: Non-Binary",
                "genderNoneoftheabove" = "Gender: None of the above",
                "ed" = "Education",
                "age25to34years" = "Age: 25-34 years",
                "age35to44years" = "Age: 35-44 years",
                "age45to54years" = "Age: 45-54 years",
                "age55to64years" = "Age: 55-64 years",
                "ageAge65orolder" = "Age: 65 or older",                        
                "income17%M34%" = "Income Percentile: 17-34",
                "income35%M50%" = "Income Percentile: 35-50",
                "income51%M67%" = "Income Percentile: 51-67",
                "income65%M83%" = "Income Percentile: 65-83",
                "income84%M100%" = "Income Percentile: 84-100",
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
                "Very unfavorable|Somewhat unfavorable" = "Very unfavorable|Somewhat unfavorable",
                "Somewhat unfavorable|Neutral" = "Somewhat unfavorable|Neutral",
                "Neutral|Somewhat favorable" = "Neutral|Somewhat favorable",
                "Somewhat favorable|Very favorable" = "Somewhat favorable|Very favorable")

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
                  "\\emph{Minority Self-Identification}" = 48:49)


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

gof.1 <- extract.clmm(m1, 
                      include.thresholds = TRUE,
                      include.loglik = TRUE,
                      include.bic = TRUE,
                      include.nobs = TRUE,
                      include.variance = TRUE,
                      include.groups = TRUE)

gof.2 <- extract.clmm(m2, 
                      include.thresholds = TRUE,
                      include.loglik = TRUE,
                      include.bic = TRUE,
                      include.nobs = TRUE,
                      include.variance = TRUE,
                      include.groups = TRUE)

gof.3 <- extract.clmm(m3, 
                      include.thresholds = TRUE,
                      include.loglik = TRUE,
                      include.bic = TRUE,
                      include.nobs = TRUE,
                      include.variance = TRUE,
                      include.groups = TRUE)


table.ordered <- texreg(list(m1, m2, m3),
                              stars = c(0.01, 0.05),
                              custom.coef.names = comap.long,
                              digits = 3,
                              single.row = TRUE,
                              groups = group.list,
                              caption = "Ordered multilevel logit models predicting attitudes towards various United States actors",
                              caption.above = TRUE,
                              label = "tab:orderedlogitbase",
                              scalebox = .5,
                              file = here("Tables", "models-ordered-20190925.tex"))

table.ordered.short <- texreg(list(m1, m2, m3),
                                    stars = c(0.01, 0.05),
                                    custom.coef.map = comap.short,
                                    digits = 3,
                                    single.row = TRUE,
                                    groups = group.list.short,
                                    caption = "Ordered multilevel logit models predicting attitudes towards various United States actors",
                                    caption.above = TRUE,
                                    label = "tab:orderedlogitshort",
                                    scalebox = .7,
                                    file = here("Tables", "models-ordered-short-20180925.tex"))









# Models with country-level variables

# Troops Question
m1.cvars <- clmm(troops_1_ord ~ contact_pers + contact_nonpers + benefit_pers + benefit_nonpers + american_inf_1 + american_inf_2 + demgov + gender + ed + age + income + ideology + relig + minority + spend_toa_combined_w_log + baseinprovince + defense + log_threat_environment + log_troops_2017 + polity2 + gdp_constant_log + log_trade_total_2017 + log_students + (1 | country),
           data = apsr.data,
           nAGQ = 1,
           link = "logit")

# Correlation matrix
mat.1.cvars <- as.matrix(cov2cor(vcov(m1.cvars)))

ggcorrplot(mat.1.cvars, lab = TRUE, lab_size = 1.5, type = "full",) +
  theme(axis.text.y = element_text(size = 5),
        axis.text.x = element_text(size = 5)) +
  labs(title = "Correlation Matrix for Multilevel Ordered Logit Model 1 - Troop Presence")
ggsave("apsr-figure-corrmatrix-ordered-1-cvars.pdf", width = 8, height = 10, units = "in", path = "./Figures/")


# US Government Question
m2.cvars <- clmm(american_gov_ord ~ contact_pers + contact_nonpers + benefit_pers + benefit_nonpers + american_inf_1 + american_inf_2 + demgov + gender + ed + age + income + ideology + relig + minority + spend_toa_combined_w_log + baseinprovince + defense + log_threat_environment + log_troops_2017 + polity2 + gdp_constant_log + log_trade_total_2017 + log_students + (1 | country),
           data = apsr.data,
           nAGQ = 1,
           link = "logit")

# Correlation matrix
mat.2.cvars <- as.matrix(cov2cor(vcov(m2.cvars)))

ggcorrplot(mat.2.cvars, lab = TRUE, lab_size = 1.5, type = "full") +
  theme(axis.text.y = element_text(size = 5),
        axis.text.x = element_text(size = 5)) +
  labs(title = "Correlation Matrix for Multilevel Ordered Logit Model 2 - US Government")

ggsave("apsr-figure-corrmatrix-ordered-2-cvars.pdf", width = 8, height = 10, units = "in", path = "./Figures/")


# US People Question
m3.cvars <- clmm(american_people_ord ~ contact_pers + contact_nonpers + benefit_pers + benefit_nonpers + american_inf_1 + american_inf_2 + demgov + gender + ed + age + income + ideology + relig + minority + spend_toa_combined_w_log + baseinprovince + defense + log_threat_environment + log_troops_2017 + polity2 + gdp_constant_log + log_trade_total_2017 + log_students + (1 | country),
           data = apsr.data,
           nAGQ = 1,
           link = "logit")  

# Correlation matrix
mat.3.cvars <- as.matrix(cov2cor(vcov(m3.cvars)))

ggcorrplot(mat.3.cvars, lab = TRUE, lab_size = 1.5, type = "full") +
  theme(axis.text.y = element_text(size = 5),
        axis.text.x = element_text(size = 5)) +
  labs(title = "Correlation Matrix for Multilevel Ordered Logit Model 3 - US People")

ggsave("apsr-figure-corrmatrix-ordered-3-cvars.pdf", width = 8, height = 10, units = "in", path = "./Figures/")


# Texreg tables for ordered models

comap.long <- c("contact_persDontknowDDeclinetoanswer" = "PC: Don't know/Decline to answer", 
                   "contact_persYes" = "PC: Yes",
                   "contact_nonpersDontknowDDeclinetoanswer" = "NC: Don't know/Decline to answer",
                   "contact_nonpersYes" = "NC: Yes",
                   "benefit_persDontknowDDeclinetoanswer" = "PB: Don't know/Decline to answer", 
                   "benefit_persYes" = "PB: Yes",
                   "benefit_nonpersDontknowDDeclinetoanswer" = "NB: Don't know/Decline to answer", 
                   "benefit_nonpersYes" = "NB: Yes",
                   "american_inf_1DontknowDdeclinetoanswer" = "American influence (Degree): Don't know/Decline to answer", 
                   "american_inf_1Alittle" = "American influence (Degree): A little",
                   "american_inf_1Some" = "American influence (Degree): Some",
                   "american_inf_1Alot" = "American influence (Degree): A lot",
                   "american_inf_2DontknowDdeclinetoanswer" = "American influence (Quality): Don't know/Decline to answer",
                   "american_inf_2Verynegative" = "American influence (Quality): Very Negative",
                   "american_inf_2Negative" = "American influence (Quality): Negative",
                   "american_inf_2Positive" = "American influence (Quality): Positive",                    
                   "american_inf_2Verypositive" = "American influence (Quality): Very Positive",
                   "demgovNeutral" = "Democratic Government: Neutral",                             
                   "demgovNotimportant" = "Democratic Government: Not Important",
                   "demgovSomewhatImportant" = "Democratic Government: Somewhat Important",                  
                   "demgovVeryImportant" = "Democratic Government: Very Important",
                   "genderFemale" = "Gender: Female",                               
                   "genderNonMbinary" = "Gender: Non-Binary",
                   "genderNoneoftheabove" = "Gender: None of the above",
                   "ed" = "Education",
                   "age25to34years" = "Age: 25-34 years",
                   "age35to44years" = "Age: 35-44 years",
                   "age45to54years" = "Age: 45-54 years",
                   "age55to64years" = "Age: 55-64 years",
                   "ageAge65orolder" = "Age: 65 or older",                        
                   "income17%M34%" = "Income Percentile: 17-34",
                   "income35%M50%" = "Income Percentile: 35-50",
                   "income51%M67%" = "Income Percentile: 51-67",
                   "income65%M83%" = "Income Percentile: 65-83",
                   "income84%M100%" = "Income Percentile: 84-100",
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
                   "log_trade_total_2017" = "log(Total Trade with US",
                   "log_students" = "log(US Students in Respondent Country, 2017)",
                   "Very unfavorable|Somewhat unfavorable" = "Very unfavorable|Somewhat unfavorable",
                   "Somewhat unfavorable|Neutral" = "Somewhat unfavorable|Neutral",
                   "Neutral|Somewhat favorable" = "Neutral|Somewhat favorable",
                   "Somewhat favorable|Very favorable" = "Somewhat favorable|Very favorable")

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
                  "\\emph{Country-Level Variables}" = 50:57)


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

gof.1 <- extract.clmm(m1.cvars, 
                        include.thresholds = TRUE,
                        include.loglik = TRUE,
                        include.bic = TRUE,
                        include.nobs = TRUE,
                        include.variance = TRUE,
                        include.groups = TRUE)

gof.2 <- extract.clmm(m2.cvars, 
                      include.thresholds = TRUE,
                      include.loglik = TRUE,
                      include.bic = TRUE,
                      include.nobs = TRUE,
                      include.variance = TRUE,
                      include.groups = TRUE)

gof.3 <- extract.clmm(m3.cvars, 
                      include.thresholds = TRUE,
                      include.loglik = TRUE,
                      include.bic = TRUE,
                      include.nobs = TRUE,
                      include.variance = TRUE,
                      include.groups = TRUE)


table.ordered.cvars <- texreg(list(m1.cvars, m2.cvars, m3.cvars),
                              stars = c(0.01, 0.05),
                              custom.coef.names = comap.long,
                              digits = 3,
                              single.row = TRUE,
                              groups = group.list,
                              caption = "Ordered multilevel logit models predicting attitudes towards various United States actors",
                              caption.above = TRUE,
                              label = "tab:orderedlogitcvars",
                              scalebox = .5,
                              file = here("Tables", "models-ordered-cvars-20190925.tex"))

table.ordered.cvars.short <- texreg(list(m1.cvars, m2.cvars, m3.cvars),
                              stars = c(0.01, 0.05),
                              custom.coef.map = comap.short,
                              digits = 3,
                              single.row = TRUE,
                              groups = group.list.short,
                              caption = "Ordered multilevel logit models predicting attitudes towards various United States actors",
                              caption.above = TRUE,
                              label = "tab:orderedlogitcvarsshort",
                              scalebox = .7,
                              file = here("Tables", "models-ordered-cvars-short-20180925.tex"))

