

# WARNING: THESE MODELS WILL TAKE ~36 HOURS TO RUN. PLAN ACCORDINGLY.


# Parallelize machine
options(mc.cores = parallel::detectCores())
ncores = detectCores()
rstan_options(auto_write = TRUE)



# Note: Make sure ordinal and nlme packages are detached when using lme4. 
# They conflict with ranef command.
detach("package:ordinal", unload = TRUE)

# Set vague priors
vagueprior <- set_prior("normal(0, 100)", class = "b")

# Models with country-level variables
# Troops Question
m1.bayes <- brm(american_t_dummy ~ contact_pers + contact_nonpers + benefit_pers + benefit_nonpers + american_inf_1 + american_inf_2 + demgov + gender + ed + age + income + ideology + relig + minority + spend_toa_combined_w_log + baseinprovince + defense + log_threat_environment + log_troops_2017 + polity2 + gdp_constant_log + log_trade_total_2017 + log_students + (1 | country),
                data = apsr.data,
                prior = vagueprior,
                cores = ncores,
                iter = 10000,
                warmup = 4000,
                thin = 1,
                chains = 4,
                control = list(adapt_delta = 0.90),
                seed = 66502,
                family = bernoulli(link = "logit"),
                file = here("Bayes Diagnostics", "m1.bayes"))


# US Government Question
m2.bayes <- brm(american_g_dummy ~ contact_pers + contact_nonpers + benefit_pers + benefit_nonpers + american_inf_1 + american_inf_2 + demgov + gender + ed + age + income + ideology + relig + minority + spend_toa_combined_w_log + baseinprovince + defense + log_threat_environment + log_troops_2017 + polity2 + gdp_constant_log + log_trade_total_2017 + log_students + (1 | country),
                data = apsr.data,
                prior = vagueprior,
                cores = ncores,
                iter = 10000,
                warmup = 4000,
                thin = 1,
                chains = 4,
                control = list(adapt_delta = 0.90),
                seed = 66502,
                family = bernoulli(link = "logit"),
                file = here("Bayes Diagnostics", "m2.bayes"))


# US People Question
m3.bayes <- brm(american_p_dummy ~ contact_pers + contact_nonpers + benefit_pers + benefit_nonpers + american_inf_1 + american_inf_2 + demgov + gender + ed + age + income + ideology + relig + minority + spend_toa_combined_w_log + baseinprovince + defense + log_threat_environment + log_troops_2017 + polity2 + gdp_constant_log + log_trade_total_2017 + log_students + (1 | country),
                data = apsr.data,
                prior = vagueprior,
                cores = ncores,
                iter = 10000,
                warmup = 4000,
                thin = 1,
                chains = 4,
                control = list(adapt_delta = 0.90),
                seed = 66502,
                family = bernoulli(link = "logit"),
                file = here("Bayes Diagnostics", "m3.bayes"))



# Diagnostic materials
m1.bayes.ggs <- ggs(m1.bayes)
m2.bayes.ggs <- ggs(m2.bayes)
m3.bayes.ggs <- ggs(m3.bayes)

ggmcmc(m1.bayes.ggs, file = here("Bayes Diagnostics", "m1-bayes-diag.pdf"))
ggmcmc(m2.bayes.ggs, file = here("Bayes Diagnostics", "m2-bayes-diag.pdf"))
ggmcmc(m3.bayes.ggs, file = here("Bayes Diagnostics", "m3-bayes-diag.pdf"))

ess.1 <- data.frame(effectiveSize(m1.bayes)) 
ess.2 <- data.frame(effectiveSize(m2.bayes)) 
ess.3 <- data.frame(effectiveSize(m3.bayes)) 

ess.com <- cbind(ess.1, ess.2, ess.3) %>% 
  mutate(Variable = row.names(.)) %>% 
  dplyr::rename(., "Troops Model" = 1, "Government Model" = 2, "People Model" = 3) %>% 
  pivot_longer(cols = c(1:3), names_to = "Model")


# Plot effective sample size
ggplot(ess.com, aes(x = value, y = reorder(Variable, value))) +
  geom_barh(stat = "identity") +
  facet_wrap(. ~ Model) +
  theme_bw() +
  labs(x = "Effective Sample Size",
       y = "Variable")

ggsave(here("Figures", "apsr-ess-logit-20191001.pdf"))


# Correlation matrix plots
# Troops model
mat.1.bayes.logit <- as.matrix(cov2cor(vcov(m1.bayes)))

ggcorrplot(mat.1.bayes.logit, lab = TRUE, lab_size = 1.5, type = "full",) +
  theme(axis.text.y = element_text(size = 5),
        axis.text.x = element_text(size = 5)) +
  labs(title = "Correlation Matrix for Multilevel Bayesian Logit Model: Troop Presence")

ggsave("figure-corrmatrix-1-binary-bayes.pdf", width = 8, height = 10, units = "in", path = "./Figures/")


# Government model
mat.2.bayes.logit <- as.matrix(cov2cor(vcov(m2.bayes)))

ggcorrplot(mat.2.bayes.logit, lab = TRUE, lab_size = 1.5, type = "full",) +
  theme(axis.text.y = element_text(size = 5),
        axis.text.x = element_text(size = 5)) +
  labs(title = "Correlation Matrix for Multilevel Bayesian Logit Model: US Government")

ggsave("figure-corrmatrix-2-binary-bayes.pdf", width = 8, height = 10, units = "in", path = "./Figures/")


# US People
mat.3.bayes.logit <- as.matrix(cov2cor(vcov(m3.bayes)))

ggcorrplot(mat.3.bayes.logit, lab = TRUE, lab_size = 1.5, type = "full",) +
  theme(axis.text.y = element_text(size = 5),
        axis.text.x = element_text(size = 5)) +
  labs(title = "Correlation Matrix for Multilevel Bayesian Logit Model: US People")

ggsave("figure-corrmatrix-3-binary-bayes.pdf", width = 8, height = 10, units = "in", path = "./Figures/")


# Bayesian logit model tables
# Tables
modellist.binary.bayes <- list()
modellist.binary.bayes[['US Troops']] <- `m1.bayes`
modellist.binary.bayes[['US Government']] <- `m2.bayes`
modellist.binary.bayes[['US People']] <- `m3.bayes`

comap.long <- list("b_contact_persDontknowDDeclinetoanswer" = "PC: Don't know/Decline to answer", 
                   "b_contact_persYes" = "PC: Yes",
                   "b_contact_nonpersDontknowDDeclinetoanswer" = "NC: Don't know/Decline to answer",
                   "b_contact_nonpersYes" = "NC: Yes",
                   "b_benefit_persDontknowDDeclinetoanswer" = "PB: Don't know/Decline to answer", 
                   "b_benefit_persYes" = "PB: Yes",
                   "b_benefit_nonpersDontknowDDeclinetoanswer" = "NB: Don't know/Decline to answer", 
                   "b_benefit_nonpersYes" = "NB: Yes",
                   "b_american_inf_1DontknowDdeclinetoanswer" = "American influence (Degree): Don't know/Decline to answer", 
                   "b_american_inf_1Alittle" = "American influence (Degree): A little",
                   "b_american_inf_1Some" = "American influence (Degree): Some",
                   "b_american_inf_1Alot" = "American influence (Degree): A lot",
                   "b_american_inf_2DontknowDdeclinetoanswer" = "American influence (Quality): Don't know/Decline to answer",
                   "b_american_inf_2Verynegative" = "American influence (Quality): Very Negative",
                   "b_american_inf_2Negative" = "American influence (Quality): Negative",
                   "b_american_inf_2Positive" = "American influence (Quality): Positive",                    
                   "b_american_inf_2Verypositive" = "American influence (Quality): Very Positive",
                   "b_demgovNeutral" = "Democratic Government: Neutral",                             
                   "b_demgovNotimportant" = "Democratic Government: Not Important",
                   "b_demgovSomewhatImportant" = "Democratic Government: Somewhat Important",                  
                   "b_demgovVeryImportant" = "Democratic Government: Very Important",
                   "b_genderFemale" = "Gender: Female",                               
                   "b_genderNonMbinary" = "Gender: Non-Binary",
                   "b_genderNoneoftheabove" = "Gender: None of the above",
                   "b_ed" = "Education",
                   "b_age25to34years" = "Age: 25-34 years",
                   "b_age35to44years" = "Age: 35-44 years",
                   "b_age45to54years" = "Age: 45-54 years",
                   "b_age55to64years" = "Age: 55-64 years",
                   "b_ageAge65orolder" = "Age: 65 or older",                        
                   "b_income17%M34%" = "Income Percentile: 17-34",
                   "b_income35%M50%" = "Income Percentile: 35-50",
                   "b_income51%M67%" = "Income Percentile: 51-67",
                   "b_income65%M83%" = "Income Percentile: 65-83",
                   "b_income84%M100%" = "Income Percentile: 84-100",
                   "b_ideology" = "Ideology",
                   "b_religProtestant" = "Protestant",
                   "b_religCatholicism" = "Catholicism",
                   "b_religIslam" = "Islam",
                   "b_religJudaism" = "Judaism",
                   "b_religShinto" = "Shinto",
                   "b_religBuddhism" = "Buddhism",
                   "b_religHinduism" = "Hinduism",
                   "b_religLocal" = "Local",
                   "b_religMormonism" = "Mormonism",
                   "b_religDecline to answer" = "Decline to answer",
                   "b_religOther" = "Other",
                   "b_minorityNo" = "Minority: Yes",                                
                   "b_minorityYes" = "Minority: No",
                   "b_spend_toa_combined_w_log" = "log(US Military Spending)",
                   "b_baseinprovince" = "Base in Respondent's Province",
                   "b_defense" = "US Defense Pact",
                   "b_log_threat_environment" = "Threat Environment",
                   "b_log_troops_2017" = "log(US Troops in Country, 2017)",
                   "b_polity2" = "Polity Score",
                   "b_gdp_constant_log" = "log(GDP)",
                   "b_log_trade_total_2017" = "log(Total Trade with US)",
                   "b_log_students" = "log(US Students in Respondent Country, 2017)")

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
                  "\\emph{Religious Self-Identification}" = 37:46,
                  "\\emph{Minority Self-Identification}" = 47:48,
                  "\\emph{Country-Level Variables}" = 49:57)

comap.short <- list("b_contact_persDontknowDDeclinetoanswer" = "PC: Don't know/Decline to answer", 
                    "b_contact_persYes" = "PC: Yes",
                    "b_contact_nonpersDontknowDDeclinetoanswer" = "NC: Don't know/Decline to answer",
                    "b_contact_nonpersYes" = "NC: Yes",
                    "b_benefit_persDontknowDDeclinetoanswer" = "PB: Don't know/Decline to answer", 
                    "b_benefit_persYes" = "PB: Yes",
                    "b_benefit_nonpersDontknowDDeclinetoanswer" = "NB: Don't know/Decline to answer", 
                    "b_benefit_nonpersYes" = "NB: Yes")

group.list.short = list("Personal Contact" = 1:2,
                        "Network Contact"  = 3:4,
                        "Personal Benefit" = 5:6,
                        "Network Benefit" = 7:8)


table.bayes.binary <- mcmcreg(modellist.binary.bayes, 
                             pars = c("b"),
                             single.row = TRUE,
                             custom.coef.map = comap.long,
                             point.est = "mean", ci = 0.95,
                             brms.re = FALSE,
                             groups = group.list,
                             center = TRUE,
                             caption = "Multilevel Bayesian logit models predicting positive attitudes",
                             caption.above = TRUE,
                             label = "table:bayeslogit",
                             fontsize = NULL,
                             scalebox = 0.6)


table.bayes.binary <- table.bayes.binary %>% # Selection 2018 spending values
  read_lines()  %>% 
  grep('\\&', ., value = TRUE) %>% 
  paste(collapse = '\n') %>% 
  read_fwf(fwf_empty(.)) %>% 
  as.data.frame()


X1 <- c("\\textbf{Random Effects}", "\\quad N", "\\quad Groups", "\\quad Std. Dev.") 
X3 <- c(" ",
        summary(`m1.bayes`)[[4]],
        length(unique(`m1.bayes`$data$country)),
        round(VarCorr(`m1.bayes`)$country$sd[[1]], digits = 3))
X5 <- c(" ",
        summary(`m2.bayes`)[[4]],
        length(unique(`m2.bayes`$data$country)),
        round(VarCorr(`m2.bayes`)$country$sd[[1]], digits = 3))
X7 <- c(" ",
        summary(`m3.bayes`)[[4]],
        length(unique(`m3.bayes`$data$country)),
        round(VarCorr(`m3.bayes`)$country$sd[[1]], digits = 3))

table.bayes.binary.2 <- data.frame(X1, X3, X5, X7)

table.bayes.binary.full <- plyr::rbind.fill(table.bayes.binary, table.bayes.binary.2) %>% 
  fill(c(X2, X4, X6, X8))
table.bayes.binary.full[1,8] <- "\\hline"
table.bayes.binary.full[73, 8] <- "\\\\ \\hline "
table.bayes.binary.full[74, 8] <- "\\\\ \\hline "

write.table(table.bayes.binary.full, here("Tables", "models-binary-bayes-20190930.tex"),
            quote = FALSE,
            row.names = FALSE,
            col.names = FALSE,
            na = " ")


