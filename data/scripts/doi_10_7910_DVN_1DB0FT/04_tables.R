##############
### TABLES ###
##############

# Table 1
summary_cl(qbin.sh.1.cntry, qbin.sh.2.cntry, qbin.sh.3.cntry,
           cluster = ~country,
           order = c(1, 2, 7, 3, 4, 8, 5, 6, 9),
           dep.var.labels = c("Sharedness"),
           covariate.labels = c("Ministries Dyad",
                                "Ministries Difference",
                                "Ministries Difference * Ministries Dyad",
                                "Issue Salience",
                                "Policy Conflict",
                                "Policy Conflict * Issue Salience",
                                "Denmark", "Netherlands"),
           type = "text",
           out = "table_1.txt")


summary_cl(qbin.sh.1.cntry, qbin.sh.2.cntry, qbin.sh.3.cntry,
           cluster = ~country,
           order = c(1, 2, 7, 3, 4, 8, 5, 6, 9),
           dep.var.labels = c("Sharedness"),
           covariate.labels = c("Ministries Dyad",
                                "Ministries Difference",
                                "Ministries Difference * Ministries Dyad",
                                "Issue Salience",
                                "Policy Conflict",
                                "Policy Conflict * Issue Salience",
                                "Denmark", "Netherlands"),
           type = "latex",
           out = "table_1.tex")


# Table A3
summary_cl(qbin.sh.1.cntry_mjr, qbin.sh.2.cntry_mjr, qbin.sh.3.cntry_mjr,
           cluster = ~country,
           order = c(1, 2, 29, 3, 4, 30),
           dep.var.labels = c("Sharedness"),
           covariate.labels = c("Ministries Dyad",
                                "Ministries Difference",
                                "Ministries Difference * Ministries Dyad",
                                "Issue Salience",
                                "Policy Conflict",
                                "Policy Conflict * Issue Salience"),
           type = "text",
           out = "table_a3.txt")

summary_cl(qbin.sh.1.cntry_mjr, qbin.sh.2.cntry_mjr, qbin.sh.3.cntry_mjr,
           cluster = ~country,
           order = c(1, 2, 29, 3, 4, 30),
           dep.var.labels = c("Sharedness"),
           covariate.labels = c("Ministries Dyad",
                                "Ministries Difference",
                                "Ministries Difference * Ministries Dyad",
                                "Issue Salience",
                                "Policy Conflict",
                                "Policy Conflict * Issue Salience"),
           type = "latex",
           out = "table_a3.tex")

# Table A4
summary_cl(qbin.sh.1.cab, qbin.sh.2.cab, qbin.sh.3.cab,
           cluster = ~cabinet,
           order = c(1, 2, 20, 3, 4, 21),
           dep.var.labels = c("Sharedness"),
           covariate.labels = c("Ministries Dyad",
                                "Ministries Difference",
                                "Ministries Difference * Ministries Dyad",
                                "Issue Salience",
                                "Policy Conflict",
                                "Policy Conflict * Issue Salience"),
           type = "text",
           out = "table_a4.txt")

summary_cl(qbin.sh.1.cab, qbin.sh.2.cab, qbin.sh.3.cab,
           cluster = ~cabinet,
           order = c(1, 2, 20, 3, 4, 21),
           dep.var.labels = c("Sharedness"),
           covariate.labels = c("Ministries Dyad",
                                "Ministries Difference",
                                "Ministries Difference * Ministries Dyad",
                                "Issue Salience",
                                "Policy Conflict",
                                "Policy Conflict * Issue Salience"),
           type = "latex",
           out = "table_a4.tex")

# Table A5
summary_cl(beta.sh.1.cntry, beta.sh.2.cntry, beta.sh.3.cntry,
           beta.sh.1.cntry_mjr, beta.sh.2.cntry_mjr, beta.sh.3.cntry_mjr,
           cluster = NULL,
           order = c(1, 2, 29, 3, 4, 30),
           dep.var.labels = c("Sharedness"),
           covariate.labels = c("Ministries Dyad",
                                "Ministries Difference",
                                "Ministries Difference * Ministries Dyad",
                                "Issue Salience",
                                "Policy Conflict",
                                "Policy Conflict * Issue Salience",
                                "Denmark", "Netherlands"),
           type = "text",
           out = "table_a5.txt")

summary_cl(beta.sh.1.cntry, beta.sh.2.cntry, beta.sh.3.cntry,
           beta.sh.1.cntry_mjr, beta.sh.2.cntry_mjr, beta.sh.3.cntry_mjr,
           cluster = NULL,
           order = c(1, 2, 29, 3, 4, 30),
           dep.var.labels = c("Sharedness"),
           covariate.labels = c("Ministries Dyad",
                                "Ministries Difference",
                                "Ministries Difference * Ministries Dyad",
                                "Issue Salience",
                                "Policy Conflict",
                                "Policy Conflict * Issue Salience",
                                "Denmark", "Netherlands"),
           type = "latex",
           out = "table_a5.tex")


# Table A1
Data_Party_Pos %>%
  select(sharedness, ministries_total, ministries_diff, attention_total, position_diff) %>%
  rename("Sharedness" = sharedness,
         "Ministries Dyad" = ministries_total,
         "Ministries Difference" = ministries_diff,
         "Issue Salience" = attention_total,
         "Policy Conflict" = position_diff,
         "Country" = country) %>%
  sumtable(group = "Country", group.long = TRUE,
           factor.numeric = TRUE,
           summ = c('round(mean(x),0)','sd(x)','min(x)', 'pctile(x)[50]', "max(x)"),
           summ.names = c("mean", "sd","min", "med", "max"),
           out = "latex",
           file = "table_a1")


# Table A2
Data_Party %>%
  group_by(country, national_minor) %>%
  summarise(mean_attention = mean(attention_total, na.rm = TRUE)) %>%
  filter(mean_attention >= quantile(mean_attention, 0.95)) %>%
  arrange(country, desc(mean_attention)) %>%
  ungroup() %>%
  mutate(mean_attention = round(mean_attention*100, 2),
         minor = factor(national_minor,
                        levels = sort(c(100, 107, 1910, 230, 1308,
                                        202 , 609 , 500, 1303 , 105,
                                        600, 1305 , 900, 1300 , 322,
                                        602 , 700, 2097, 2000, 1200, 
                                        2100 , 300)),
                        labels = c("Macroeconomics - General",
                                   "National Budget",
                                   "Tax Code",
                                   "Gender Discrimination",
                                   "Immigration",
                                   "Health Care - General",
                                   "Medical Facilities",
                                   "Labour - General",
                                   "Education - General",
                                   "Elementary & Secondary",
                                   "Education - R&D",
                                   "Environment - General",
                                   "Immigration",
                                   "Law & Crime - General",
                                   "Social Welfare - General",
                                   "Elderly Assistance",
                                   "Volunteer Associations",
                                   "Child Care",
                                   "Western Europe",
                                   "Government Operations - General",
                                   "Specific developments within political parties",
                                   "Public Lands")),
         national_minor = as.integer(national_minor)) %>%
  select(country, national_minor, minor, mean_attention) %>%
  xtable(., type = "latex") %>%
  print(file = "table_a2.tex")

# Table A6
# Table A6 was created manually.

# Table A7
# Table A7 was created manually.
