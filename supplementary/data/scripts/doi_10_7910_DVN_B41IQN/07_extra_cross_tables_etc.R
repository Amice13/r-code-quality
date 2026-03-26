# Link between vote and lifestyle

twoway_abs <- table(data_exp$lifestyle, data_exp$vote_today_group)
twoway_abs

twoway_prop <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_prop

res_chisqt <- chisq.test(twoway_abs)
print(res_chisqt)


# Link between income and lifestyle

twoway_abs <- table(data_exp$lifestyle, data_exp$income1)
twoway_abs

twoway_prop <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_prop

res_chisqt <- chisq.test(twoway_abs)
print(res_chisqt)

# Link between income and talking to Syrians

twoway_abs <- table(data_exp$income2, data_exp$syr_talking_ever)
twoway_abs

twoway_prop <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_prop

res_chisqt <- chisq.test(twoway_abs)
print(res_chisqt)

    # regression
res_logit_bas <- glm(syr_talking_ever ~ lifestyle + gender + age + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + Soru1308, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)


# Link between earthquake loss and Syrians' longterm stay / return 

twoway_abs <- table(data_exp$Soru805, data_exp$longterm_stay)
twoway_abs

twoway_prop <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_prop

res_chisqt <- chisq.test(twoway_abs)
print(res_chisqt)

    # regression
res_logit_bas <- data_exp %>%
  glm(longterm_stay ~ Soru805, family = binomial(link = "logit"), data = .)
summary(res_logit_bas)


##

twoway_abs <- table(data_exp$Soru805, data_exp$return_ethical)
twoway_abs

twoway_prop <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_prop

res_chisqt <- chisq.test(twoway_abs)
print(res_chisqt)

    # regression
res_logit_bas <- data_exp %>%
  glm(return_ethical ~ Soru805, family = binomial(link = "logit"), data = .)
summary(res_logit_bas)


# Link between lifestyle and Syrians' longterm stay / return 

twoway_abs <- table(data_exp$lifestyle, data_exp$longterm_stay)
twoway_abs

twoway_prop <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_prop

res_chisqt <- chisq.test(twoway_abs)
print(res_chisqt)

##

twoway_abs <- table(data_exp$lifestyle, data_exp$return_ethical)
twoway_abs

twoway_prop <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_prop

res_chisqt <- chisq.test(twoway_abs)
print(res_chisqt)


# Link between ethnicity and Syrians' longterm stay / return 

twoway_abs <- table(data_exp$eth_self, data_exp$longterm_stay)
twoway_abs

twoway_prop <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_prop

res_chisqt <- chisq.test(twoway_abs)
print(res_chisqt)

    # regression
res_logit_bas <- data_exp %>%
  glm(longterm_stay ~ eth_self, family = binomial(link = "logit"), data = .)
summary(res_logit_bas)

##

twoway_abs <- table(data_exp$eth_self, data_exp$return_ethical)
twoway_abs

twoway_prop <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_prop

res_chisqt <- chisq.test(twoway_abs)
print(res_chisqt)



# Link between ethnicity and personal relationship to Syrians

# Soru 1308: any personal relationship

table(data_exp$Soru1308)
original_table <- table(data_exp$Soru1308)
total_count <- sum(table(data_exp$Soru1308))
percentages <- (original_table / total_count) * 100
print(percentages)

twoway_abs <- table(data_exp$eth_self, data_exp$Soru1308)
twoway_abs

twoway_prop <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_prop

res_chisqt <- chisq.test(twoway_abs)
print(res_chisqt)

# Soru 1301: Wife or Husband

table(data_exp$Soru1301)
original_table <- table(data_exp$Soru1301)
total_count <- sum(table(data_exp$Soru1301))
percentages <- (original_table / total_count) * 100
print(percentages)

twoway_abs <- table(data_exp$eth_self, data_exp$Soru1301)
twoway_abs

twoway_prop <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_prop

res_chisqt <- chisq.test(twoway_abs)
print(res_chisqt)

# Soru 1302: Relative

table(data_exp$Soru1302)
original_table <- table(data_exp$Soru1302)
total_count <- sum(table(data_exp$Soru1302))
percentages <- (original_table / total_count) * 100
print(percentages)

twoway_abs <- table(data_exp$eth_self, data_exp$Soru1302)
twoway_abs

twoway_prop <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_prop

res_chisqt <- chisq.test(twoway_abs)
print(res_chisqt)

# Soru 1303: Neighbour

table(data_exp$Soru1303)
original_table <- table(data_exp$Soru1303)
total_count <- sum(table(data_exp$Soru1303))
percentages <- (original_table / total_count) * 100
print(percentages)

twoway_abs <- table(data_exp$eth_self, data_exp$Soru1303)
twoway_abs

twoway_prop <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_prop

res_chisqt <- chisq.test(twoway_abs)
print(res_chisqt)

# Soru 1304: Colleague

table(data_exp$Soru1304)
original_table <- table(data_exp$Soru1304)
total_count <- sum(table(data_exp$Soru1304))
percentages <- (original_table / total_count) * 100
print(percentages)

twoway_abs <- table(data_exp$eth_self, data_exp$Soru1304)
twoway_abs

twoway_prop <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_prop

res_chisqt <- chisq.test(twoway_abs)
print(res_chisqt)

# Soru 1305: Acquaintance/Friend

table(data_exp$Soru1305)
original_table <- table(data_exp$Soru1305)
total_count <- sum(table(data_exp$Soru1305))
percentages <- (original_table / total_count) * 100
print(percentages)

twoway_abs <- table(data_exp$eth_self, data_exp$Soru1305)
twoway_abs

twoway_prop <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_prop

res_chisqt <- chisq.test(twoway_abs)
print(res_chisqt)

# Soru 1306: Tenant

table(data_exp$Soru1306)
original_table <- table(data_exp$Soru1306)
total_count <- sum(table(data_exp$Soru1306))
percentages <- (original_table / total_count) * 100
print(percentages)

twoway_abs <- table(data_exp$eth_self, data_exp$Soru1306)
twoway_abs

twoway_prop <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_prop

res_chisqt <- chisq.test(twoway_abs)
print(res_chisqt)


# Soru 1307: Employee

table(data_exp$Soru1307)
original_table <- table(data_exp$Soru1307)
total_count <- sum(table(data_exp$Soru1307))
percentages <- (original_table / total_count) * 100
print(percentages)

twoway_abs <- table(data_exp$eth_self, data_exp$Soru1307)
twoway_abs

twoway_prop <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_prop

res_chisqt <- chisq.test(twoway_abs)
print(res_chisqt)


# Link between refugees vs inflation 

table(data_exp$refpol_moreimp)
original_table <- table(data_exp$refpol_moreimp)
total_count <- sum(table(data_exp$refpol_moreimp))
percentages <- (original_table / total_count) * 100
print(percentages)

twoway_abs <- table(data_exp$lifestyle, data_exp$longterm_stay)
twoway_abs

twoway_prop <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_prop

res_chisqt <- chisq.test(twoway_abs)
print(res_chisqt)

# Link between lifestyle and sect for Arabs

table(data_arabs$lifestyle)
original_table <- table(data_arabs$lifestyle)
total_count <- sum(table(data_arabs$lifestyle))
percentages <- (original_table / total_count) * 100
print(percentages)

twoway_abs <- table(data_arabs$lifestyle, data_arabs$sect)
twoway_abs

twoway_prop <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_prop

res_chisqt <- chisq.test(twoway_abs)
print(res_chisqt)


# Link between lifestyle and self-identified ethnicity
twoway_abs <- table(data_exp$lifestyle, data_exp$eth_self)
twoway_abs

  # Ethnicity per lifestyle
twoway_prop <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_prop

res_chisqt <- chisq.test(twoway_abs)
print(res_chisqt)

  # Lifestyle per ethnicity
twoway_sha_col <- twoway_abs |>
  prop.table(margin = 2) |>
  round(digits = 2)
twoway_sha_col

  # Model for acquaintance
res_logit_bas <- glm(Soru1308 ~ lifestyle + gender + age + eth_self + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)


# Link between religious practice and Syrian friend

table(data_exp$syr_friend)
original_table <- table(data_exp$syr_friend)
total_count <- sum(table(data_exp$syr_friend))
percentages <- (original_table / total_count) * 100
print(percentages)

twoway_abs <- table(data_exp$rel_prac, data_exp$syr_friend)
twoway_abs

twoway_prop <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_prop

res_chisqt <- chisq.test(twoway_abs)
print(res_chisqt)


# Link between refugee preferences (with/without edu/emp)
topline(df = data_exp, variable = prefref_gen, weight = weight_siyasi)
topline(df = data_exp, variable = prefref_eduemp, weight = weight_siyasi)
crosstab(df = data_exp, x = prefref_gen, y = prefref_eduemp, weight = weight_siyasi) 

twoway_abs <- table(data_exp$prefref_gen, data_exp$prefref_eduemp)
twoway_abs

twoway_prop <- twoway_abs |>
  prop.table(margin = 1) |>
  round(digits = 2)
twoway_prop

res_chisqt <- chisq.test(twoway_abs)
print(res_chisqt)


# Link between REL ID, ETH ID, and Party

topline(df = data_relcon, variable = vote_may_group, weight = weight_siyasi)
crosstab(df = data_relcon, x = eth_self, y = vote_may_group, weight = weight_siyasi) 
crosstab(df = data_relcon, x = vote_may_group, y = eth_self, weight = weight_siyasi) 


# Link between Description of Syrian and ethnicity/lifestyle

topline(df = data_control, variable = syr_descrip, weight = weight_siyasi)
topline(df = data_exp, variable = syr_descrip, weight = weight_siyasi)
crosstab(df = data_control, x = eth_self, y = syr_descrip, weight = weight_siyasi) 
crosstab(df = data_treat, x = eth_self, y = syr_descrip, weight = weight_siyasi) 
crosstab(df = data_exp, x = eth_self, y = syr_descrip, weight = weight_siyasi) 
crosstab(df = data_control, x = lifestyle, y = syr_descrip, weight = weight_siyasi) 

