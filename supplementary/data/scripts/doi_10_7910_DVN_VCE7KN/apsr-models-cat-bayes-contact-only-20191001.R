


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
m1.cat.bayes.contact <- brm(troops_1_cat ~ contact_pers + contact_nonpers + benefit_pers + benefit_nonpers + (1 | country),
                    data = apsr.data,
                    prior = vagueprior,
                    iter = 10000,
                    warmup = 3000,
                    cores = ncores,
                    chains = 4,
                    control = list(adapt_delta = 0.95),
                    seed = 66502,
                    file = here("Bayes Diagnostics", "m1.cat.bayes.contact"),
                    family = categorical(link = "logit", refcat = "neutral"))
time.2 <- Sys.time()

print(time.2-time.1)


# US Government Question
m2.cat.bayes.contact <- brm(american_g_cat ~ contact_pers + contact_nonpers + benefit_pers + benefit_nonpers + (1 | country),
                    data = apsr.data,
                    prior = vagueprior,
                    iter = 10000,
                    warmup = 3000,
                    cores = ncores,
                    chains = 4,
                    control = list(adapt_delta = 0.95),
                    seed = 66502,
                    file = here("Bayes Diagnostics", "m2.cat.bayes.contact"),
                    family = categorical(link = "logit", refcat = "neutral"))


# US People Question
m3.cat.bayes.contact <- brm(american_p_cat ~ contact_pers + contact_nonpers + benefit_pers + benefit_nonpers + (1 | country),
                    data = apsr.data,
                    prior = vagueprior,
                    iter = 10000,
                    warmup = 3000,
                    cores = ncores,
                    chains = 4,
                    control = list(adapt_delta = 0.95),
                    seed = 66502,
                    file = here("Bayes Diagnostics", "m3.cat.bayes.contact"),
                    family = categorical(link = "logit", refcat = "neutral"))


m1.cat.contact.diag <- ggs(m1.cat.bayes.contact)
ggmcmc(m1.cat.diag, file="m1-cat-bayes-diag.pdf")

m2.cat.contact.diag <- ggs(m2.cat.bayes.contact)
ggmcmc(m2.cat.diag, file="m2-cat-bayes-diag.pdf")

m3.cat.contact.diag <- ggs(m3.cat.bayes.contact)
ggmcmc(m3.cat.diag, file="m3-cat-bayes-diag.pdf")

ess.1 <- data.frame(effectiveSize(m1.cat.bayes.contact)) 
ess.2 <- data.frame(effectiveSize(m2.cat.bayes.contact)) 
ess.3 <- data.frame(effectiveSize(m3.cat.bayes.contact)) 

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

ggsave(here("Figures", "apsr-ess-cat-logit-contact-20191001.pdf"))


# Tables
modellist <- list()
modellist[['US Troops']] <- `m1.cat.bayes.contact`
modellist[['US Government']] <- `m2.cat.bayes.contact`
modellist[['US People']] <- `m3.cat.bayes.contact`


# Texreg tables for categorical models 


comap.long <- c("b_mupos_contact_persDontknowDDeclinetoanswer" = "PC: Don't know/Decline to answer", 
                "b_mupos_contact_persYes" = "PC: Yes",
                "b_mupos_contact_nonpersDontknowDDeclinetoanswer" = "NC: Don't know/Decline to answer",
                "b_mupos_contact_nonpersYes" = "NC: Yes",
                "b_mupos_benefit_persDontknowDDeclinetoanswer" = "PB: Don't know/Decline to answer", 
                "b_mupos_benefit_persYes" = "PB: Yes",
                "b_mupos_benefit_nonpersDontknowDDeclinetoanswer" = "NB: Don't know/Decline to answer", 
                "b_mupos_benefit_nonpersYes" = "NB: Yes",
                "b_muneg_contact_persDontknowDDeclinetoanswer" = "PC: Don't know/Decline to answer", 
                "b_muneg_contact_persYes" = "PC: Yes",
                "b_muneg_contact_nonpersDontknowDDeclinetoanswer" = "NC: Don't know/Decline to answer",
                "b_muneg_contact_nonpersYes" = "NC: Yes",
                "b_muneg_benefit_persDontknowDDeclinetoanswer" = "PB: Don't know/Decline to answer", 
                "b_muneg_benefit_persYes" = "PB: Yes",
                "b_muneg_benefit_nonpersDontknowDDeclinetoanswer" = "NB: Don't know/Decline to answer", 
                "b_muneg_benefit_nonpersYes" = "NB: Yes",
                "b_mudk_contact_persDontknowDDeclinetoanswer" = "PC: Don't know/Decline to answer", 
                "b_mudk_contact_persYes" = "PC: Yes",
                "b_mudk_contact_nonpersDontknowDDeclinetoanswer" = "NC: Don't know/Decline to answer",
                "b_mudk_contact_nonpersYes" = "NC: Yes",
                "b_mudk_benefit_persDontknowDDeclinetoanswer" = "PB: Don't know/Decline to answer", 
                "b_mudk_benefit_persYes" = "PB: Yes",
                "b_mudk_benefit_nonpersDontknowDDeclinetoanswer" = "NB: Don't know/Decline to answer", 
                "b_mudk_benefit_nonpersYes" = "NB: Yes")

group.list = list("\\emph{Response:Positive - Personal Contact}" = 1:2,
                  "\\emph{Response:Positive - Network Contact}"  = 3:4,
                  "\\emph{Response:Positive - Personal Benefit}" = 5:6,
                  "\\emph{Response:Positive - Network Benefit}" = 7:8,
                  "\\emph{Response:Negative - Personal Contact}" = 9:10,
                  "\\emph{Response:Negative - Network Contact}"  = 11:12,
                  "\\emph{Response:Negative - Personal Benefit}" = 13:14,
                  "\\emph{Response:Negative - Network Benefit}" = 15:16,
                  "\\emph{Response: Don't know/Decline - Personal Contact}" = 17:18,
                  "\\emph{Response: Don't know/Decline - Network Contact}"  = 19:20,
                  "\\emph{Response: Don't know/Decline - Personal Benefit}" = 21:22,
                  "\\emph{Response: Don't know/Decline - Network Benefit}" = 23:24)


# US Troops Model
table.cat.bayes.contact <- mcmcreg(`m1.cat.bayes.contact`,
                           pars = c('b_mupos', 'b_muneg', 'b_mudk'),
                           digits = 3,
                           single.row = TRUE,
                           custom.coef.names = comap.long,
                           groups = group.list,
                           caption = "Categorical Bayesian logit models predicting attitudes towards various United States actors",
                           caption.above = TRUE,
                           font = "normalsize",
                           label = "tab:catlogitbayestroops1")


table.cat.bayes.contact <- table.cat.bayes.contact %>% # Selection 2018 spending values
  read_lines()  %>% 
  grep('\\&', ., value = TRUE) %>% 
  paste(collapse = '\\\\ \n') %>% 
  read_fwf(fwf_empty(., n = 200)) %>% # Have to Increase n here to make sure it doesn't cut off cells
  as.data.frame()

table.cat.bayes.contact.pos <- table.cat.bayes.contact[c(2:13), ]
table.cat.bayes.contact.neg <- table.cat.bayes.contact[c(14:25), ]
table.cat.bayes.contact.dk <- table.cat.bayes.contact[c(26:37), ]

table.cat.bayes.contact.complete <- cbind(table.cat.bayes.contact.pos, table.cat.bayes.contact.neg, table.cat.bayes.contact.dk)
table.cat.bayes.contact.complete[, c(4,8)] <- "&"
colnames(table.cat.bayes.contact.complete)[5] <- "X5"
colnames(table.cat.bayes.contact.complete)[6] <- "X6"
colnames(table.cat.bayes.contact.complete)[7] <- "X7"
colnames(table.cat.bayes.contact.complete)[8] <- "X8"
colnames(table.cat.bayes.contact.complete)[9] <- "X9"
colnames(table.cat.bayes.contact.complete)[10] <- "X10"
colnames(table.cat.bayes.contact.complete)[11] <- "X11"
colnames(table.cat.bayes.contact.complete)[12] <- "X12"

X1 <- c("\\textbf{Random Effects}", "\\quad N", "\\quad Groups", "\\quad Std. Dev.") 
X3 <- c(" ",
        summary(`m1.cat.bayes.contact`)[[4]],
        length(unique(`m1.cat.bayes.contact`$data$country)),
        round(summary(`m1.cat.bayes.contact`)$random$country[3], digits = 3))
X7 <- c(" ",
        summary(`m1.cat.bayes.contact`)[[4]],
        length(unique(`m1.cat.bayes.contact`$data$country)),
        round(summary(`m1.cat.bayes.contact`)$random$country[2], digits = 3))
X11 <- c(" ",
         summary(`m1.cat.bayes.contact`)[[4]],
         length(unique(`m1.cat.bayes.contact`$data$country)),
         round(summary(`m1.cat.bayes.contact`)$random$country[1], digits = 3))

table.cat.bayes.contact.ranef <- data.frame(X1, X3, X7, X11)

table.cat.bayes.contact.complete <- plyr::rbind.fill(table.cat.bayes.contact.complete, table.cat.bayes.contact.ranef) %>% 
  fill(c(X2, X4, X6, X8, X10, X12)) %>% 
  mutate(X1 = gsub("\\(\\d\\)", " ", X1)) %>% 
  mutate(X5 = gsub("\\(\\d\\)", " ", X5)) %>% 
  mutate(X9 = gsub("\\(\\d\\)", " ", X9)) %>% 
  mutate(X1 = gsub("Response:Positive - ", "", X1))

table.cat.bayes.contact.complete <- table.cat.bayes.contact.complete[, -c(5, 6, 9, 10)]
table.cat.bayes.contact.complete[12, 8] <- "\\\\ \\hline "
table.cat.bayes.contact.complete[13, 8] <- "\\\\ \\hline "

# Full table for appendix
write.table(table.cat.bayes.contact.complete, here("Tables", "models-cat-bayes-m1-contact-20190924.tex"),
            quote = FALSE,
            row.names = FALSE,
            col.names = FALSE,
            na = " ")



# US Government Model
table.cat.bayes.contact <- mcmcreg(`m2.cat.bayes.contact`,
                           pars = c('b_mupos', 'b_muneg', 'b_mudk'),
                           digits = 3,
                           single.row = TRUE,
                           custom.coef.names = comap.long,
                           groups = group.list,
                           caption = "Categorical Bayesian logit models predicting attitudes towards various United States actors",
                           caption.above = TRUE,
                           font = "normalsize",
                           label = "tab:catlogitbayesgovernment")


table.cat.bayes.contact <- table.cat.bayes.contact %>% # Selection 2018 spending values
  read_lines()  %>% 
  grep('\\&', ., value = TRUE) %>% 
  paste(collapse = '\\\\ \n') %>% 
  read_fwf(fwf_empty(., n = 200)) %>% # Have to Increase n here to make sure it doesn't cut off cells
  as.data.frame()

table.cat.bayes.contact.pos <- table.cat.bayes.contact[c(2:13), ]
table.cat.bayes.contact.neg <- table.cat.bayes.contact[c(14:25), ]
table.cat.bayes.contact.dk <- table.cat.bayes.contact[c(26:37), ]

table.cat.bayes.contact.complete <- cbind(table.cat.bayes.contact.pos, table.cat.bayes.contact.neg, table.cat.bayes.contact.dk)
table.cat.bayes.contact.complete[, c(4,8)] <- "&"
colnames(table.cat.bayes.contact.complete)[5] <- "X5"
colnames(table.cat.bayes.contact.complete)[6] <- "X6"
colnames(table.cat.bayes.contact.complete)[7] <- "X7"
colnames(table.cat.bayes.contact.complete)[8] <- "X8"
colnames(table.cat.bayes.contact.complete)[9] <- "X9"
colnames(table.cat.bayes.contact.complete)[10] <- "X10"
colnames(table.cat.bayes.contact.complete)[11] <- "X11"
colnames(table.cat.bayes.contact.complete)[12] <- "X12"

X1 <- c("\\textbf{Random Effects}", "\\quad N", "\\quad Groups", "\\quad Std. Dev.") 
X3 <- c(" ",
        summary(`m2.cat.bayes.contact`)[[4]],
        length(unique(`m2.cat.bayes.contact`$data$country)),
        round(summary(`m2.cat.bayes.contact`)$random$country[3], digits = 3))
X7 <- c(" ",
        summary(`m2.cat.bayes.contact`)[[4]],
        length(unique(`m2.cat.bayes.contact`$data$country)),
        round(summary(`m2.cat.bayes.contact`)$random$country[2], digits = 3))
X11 <- c(" ",
         summary(`m2.cat.bayes.contact`)[[4]],
         length(unique(`m2.cat.bayes.contact`$data$country)),
         round(summary(`m2.cat.bayes.contact`)$random$country[1], digits = 3))

table.cat.bayes.contact.ranef <- data.frame(X1, X3, X7, X11)

table.cat.bayes.contact.complete <- plyr::rbind.fill(table.cat.bayes.contact.complete, table.cat.bayes.contact.ranef) %>% 
  fill(c(X2, X4, X6, X8, X10, X12)) %>% 
  mutate(X1 = gsub("\\(\\d\\)", " ", X1)) %>% 
  mutate(X5 = gsub("\\(\\d\\)", " ", X5)) %>% 
  mutate(X9 = gsub("\\(\\d\\)", " ", X9)) %>% 
  mutate(X1 = gsub("Response:Positive - ", "", X1))

table.cat.bayes.contact.complete <- table.cat.bayes.contact.complete[, -c(5, 6, 9, 10)]
table.cat.bayes.contact.complete[12, 8] <- "\\\\ \\hline "
table.cat.bayes.contact.complete[13, 8] <- "\\\\ \\hline "

# Full table for appendix
write.table(table.cat.bayes.contact.complete, here("Tables", "models-cat-bayes-m2-contact-20190924.tex"),
            quote = FALSE,
            row.names = FALSE,
            col.names = FALSE,
            na = " ")



# US People Model
table.cat.bayes.contact <- mcmcreg(`m3.cat.bayes.contact`,
                           pars = c('b_mupos', 'b_muneg', 'b_mudk'),
                           digits = 3,
                           single.row = TRUE,
                           custom.coef.names = comap.long,
                           groups = group.list,
                           caption = "Categorical Bayesian logit models predicting attitudes towards various United States actors",
                           caption.above = TRUE,
                           font = "normalsize",
                           label = "tab:catlogitbayespeople")

table.cat.bayes.contact <- table.cat.bayes.contact %>% # Selection 2018 spending values
  read_lines()  %>% 
  grep('\\&', ., value = TRUE) %>% 
  paste(collapse = '\\\\ \n') %>% 
  read_fwf(fwf_empty(., n = 200)) %>% # Have to Increase n here to make sure it doesn't cut off cells
  as.data.frame()

table.cat.bayes.contact.pos <- table.cat.bayes.contact[c(2:13), ]
table.cat.bayes.contact.neg <- table.cat.bayes.contact[c(14:25), ]
table.cat.bayes.contact.dk <- table.cat.bayes.contact[c(26:37), ]

table.cat.bayes.contact.complete <- cbind(table.cat.bayes.contact.pos, table.cat.bayes.contact.neg, table.cat.bayes.contact.dk)
table.cat.bayes.contact.complete[, c(4,8)] <- "&"
colnames(table.cat.bayes.contact.complete)[5] <- "X5"
colnames(table.cat.bayes.contact.complete)[6] <- "X6"
colnames(table.cat.bayes.contact.complete)[7] <- "X7"
colnames(table.cat.bayes.contact.complete)[8] <- "X8"
colnames(table.cat.bayes.contact.complete)[9] <- "X9"
colnames(table.cat.bayes.contact.complete)[10] <- "X10"
colnames(table.cat.bayes.contact.complete)[11] <- "X11"
colnames(table.cat.bayes.contact.complete)[12] <- "X12"

X1 <- c("\\textbf{Random Effects}", "\\quad N", "\\quad Groups", "\\quad Std. Dev.") 
X3 <- c(" ",
        summary(`m3.cat.bayes.contact`)[[4]],
        length(unique(`m3.cat.bayes.contact`$data$country)),
        round(summary(`m3.cat.bayes.contact`)$random$country[3], digits = 3))
X7 <- c(" ",
        summary(`m3.cat.bayes.contact`)[[4]],
        length(unique(`m3.cat.bayes.contact`$data$country)),
        round(summary(`m3.cat.bayes.contact`)$random$country[2], digits = 3))
X11 <- c(" ",
         summary(`m3.cat.bayes.contact`)[[4]],
         length(unique(`m3.cat.bayes.contact`$data$country)),
         round(summary(`m3.cat.bayes.contact`)$random$country[1], digits = 3))

table.cat.bayes.contact.ranef <- data.frame(X1, X3, X7, X11)

table.cat.bayes.contact.complete <- plyr::rbind.fill(table.cat.bayes.contact.complete, table.cat.bayes.contact.ranef) %>% 
  fill(c(X2, X4, X6, X8, X10, X12)) %>% 
  mutate(X1 = gsub("\\(\\d\\)", " ", X1)) %>% 
  mutate(X5 = gsub("\\(\\d\\)", " ", X5)) %>% 
  mutate(X9 = gsub("\\(\\d\\)", " ", X9)) %>% 
  mutate(X1 = gsub("Response:Positive - ", "", X1))

table.cat.bayes.contact.complete <- table.cat.bayes.contact.complete[, -c(5, 6, 9, 10)]
table.cat.bayes.contact.complete[12, 8] <- "\\\\ \\hline "
table.cat.bayes.contact.complete[13, 8] <- "\\\\ \\hline "

# Full table for appendix
write.table(table.cat.bayes.contact.complete, here("Tables", "models-cat-bayes-m3-contact-20190924.tex"),
            quote = FALSE,
            row.names = FALSE,
            col.names = FALSE,
            na = " ")







# Descriptive figures for primary covariates
table.sumstats.cat.bayes <- `m1.cat.bayes`$data %>% 
  mutate(gdp_constant_log = as.numeric(round(gdp_constant_log, digits = 2)),
         log_trade_total_2017 = as.numeric(round(log_trade_total_2017, digits = 2)),
         spend_toa_combined_w_log = as.numeric(round(spend_toa_combined_w_log, digits = 2)),
         baseinprovince = factor(baseinprovince, levels = c(0, 1), labels = c("No", "Yes")),
         defense = factor(defense, levels = c(0, 1), labels = c("No", "Yes")),
         log_threat_environment = round(as.numeric(log_threat_environment), digits = 3),
         log_troops_2017 = round(as.numeric(log_troops_2017), digits = 3),
         log_students = round(as.numeric(log_students), digits = 3)) %>% 
  group_by(country) %>% 
  gather() %>% 
  arrange(key, value) %>% 
  mutate(type = ifelse(key == "gdp_constant_log" | 
                         key == "log_trade_total_2017" | 
                         key == "spend_toa_combined_w_log" |
                         key == "ed" |
                         key == "ideology" |
                         key == "polity2" |
                         key == "log_threat_environment" |
                         key == "log_troops_2017" | 
                         key == "log_students", "Numeric", "Factor"))

# Factor variables
ggplot(data = table.sumstats.cat.bayes %>% filter(type == "Factor"), aes(x = reorder(value, as.numeric(value)))) +
  geom_bar() +
  facet_wrap("key", scales = "free_x") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 30, size = 5, hjust = 1)) +
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
  labs(x = "",
       y = "Density")

ggsave(here("Figures", "apsr-figure-descriptive-2.pdf"))



