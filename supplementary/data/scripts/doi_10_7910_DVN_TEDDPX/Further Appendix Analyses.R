# load data
load("df.RData")

# create variables for analyses

## Treatment
df$treated <- NA
df$treated[df$vaagroup == 2] <- 1
df$treated[df$vaagroup == 1] <- 0

## Are you interested in learning about party positions ahead of elections?
df$interested <- NA
df$interested[df$v93 == 2] <- 0
df$interested[df$v93 == 1] <- 1


## Which of the following statements best describe you? 1-3 didnt vote, 4 voted, 5 dont know
df$voted <- NA
df$voted[df$v104 == 1 | df$v104 == 2 | df$v104 == 3] <- 0
df$voted[df$v104 == 4] <- 1


## Vote switch
df$v12 <- as.numeric(df$v12) # pre-electoral vote intention
df$v105 <- as.numeric(df$v105) # post-electoral vote choice
df$voteswitch <- NA
df$voteswitch[df$v105 == df$v12] <- 0
df$voteswitch[df$v105 != df$v12] <- 1
df[1:200, c("v105", "v12", "voteswitch")]


## Vote switch 
df$v12 <- as.numeric(df$v12) # pre-electoral vote intention
df$v105 <- as.numeric(df$v105) # post-electoral vote choice
df$voteswitch <- NA
df$voteswitch[df$v105 == df$v12] <- 0
df$voteswitch[df$v105 != df$v12] <- 1
df$voteswitch[df$v12 > 7] <- NA
df$voteswitch[df$v105 > 7] <- NA
df[1:200, c("v105", "v12", "voteswitch")]

## check voteswitch proportions by treatment status
df %>%
  group_by(treated) %>%
  summarise(proportion_voteswitch = mean(voteswitch == 1, na.rm = TRUE)) # ~18% per group




## Knowledge 1: Which of the following are European party groups in the European Parliament? Select all that apply
df$party_know <- (df$v112_1 + df$v112_2 + df$v112_5) - (df$v112_3 + df$v112_4 + df$v112_6)
df[1:200, c("party_know", "v112_1", "v112_2", "v112_3", "v112_4", "v112_5", "v112_6")]


## Knowledge 2: Which of the following issues has the European Parliament voted on in the last 5 years? Select all that apply
df$policy_know <- (df$v113_1 + df$v113_3 + df$v113_5) - (df$v113_2 + df$v113_4 + df$v113_6)
df[1:200, c("policy_know", "v113_1", "v113_2", "v113_3", "v113_4", "v113_5", "v113_6")]


## Did respondents start the VAA
df$take_vaa <- NA
df$take_vaa <- apply(df[, paste0("vaa_v", 1:20)], 1, function(x) any(x > -1, na.rm = TRUE))
df$take_vaa <- ifelse(df$take_vaa, 1, 0)

df[10:200, c("take_vaa", "vaa_v1", "vaa_v10", "vaa_v20")]


## Did respondents finish the VAA
df$complete_vaa <- NA
df$complete_vaa <- ifelse(
  df$vaa_v20 > -1 | 
    (is.na(df$vaa_v20) & apply(df[, paste0("vaa_v", 1:19)], 1, function(x) any(x > -1, na.rm = TRUE))),
  1,
  0
)
df$complete_vaa[apply(df[, paste0("vaa_v", 1:20)], 1, function(x) all(is.na(x)))] <- 0

df[10:200, c("complete_vaa", "vaa_v20", "vaa_v1")]


## Gender
df$v76 <- as.character(df$v76)


## Education
df$v75_2[df$v75_2 > 50 | df$v75_2 < 10] <- NA # excluding outliers and unrealistic values


## Country
df$country <- as.character(df$country)





###
### estimate ITTs for ideological voting
###

df$family <- df$v105

df$family[df$family > 7] <- NA

table(df$family)

df$fam1 <- NA
df$fam2 <- NA
df$fam3 <- NA
df$fam4 <- NA
df$fam5 <- NA
df$fam6 <- NA
df$fam7 <- NA

df$fam1[df$family == 1] <- 1
df$fam1[df$family != 1] <- 0

df$fam2[df$family == 2] <- 1
df$fam2[df$family != 2] <- 0


df$fam3[df$family == 3] <- 1
df$fam3[df$family != 3] <- 0


df$fam4[df$family == 4] <- 1
df$fam4[df$family != 4] <- 0


df$fam5[df$family == 5] <- 1
df$fam5[df$family != 5] <- 0


df$fam6[df$family == 6] <- 1
df$fam6[df$family != 6] <- 0


df$fam7[df$family == 7] <- 1
df$fam7[df$family != 7] <- 0


table(df$fam7)

fr <- df %>% 
  filter(country == "1")

de <- df %>% 
  filter(country == "2")

it <- df %>% 
  filter(country == "3")


fam1fr <- lm(fam1 ~ treated, data = fr)
fam2fr <- lm(fam2 ~ treated, data = fr)
fam3fr <- lm(fam3 ~ treated, data = fr)
fam4fr <- lm(fam4 ~ treated, data = fr)
fam5fr <- lm(fam5 ~ treated, data = fr)
fam6fr <- lm(fam6 ~ treated, data = fr)
fam7fr <- lm(fam7 ~ treated, data = fr)
summary(fam1fr)
summary(fam2fr)
summary(fam3fr)
summary(fam4fr)
summary(fam5fr)
summary(fam6fr)
summary(fam7fr)


fam1de <- lm(fam1 ~ treated, data = de)
fam2de <- lm(fam2 ~ treated, data = de)
fam3de <- lm(fam3 ~ treated, data = de)
fam4de <- lm(fam4 ~ treated, data = de)
fam5de <- lm(fam5 ~ treated, data = de)
fam6de <- lm(fam6 ~ treated, data = de)
fam7de <- lm(fam7 ~ treated, data = de)
summary(fam1de)
summary(fam2de)
summary(fam3de)
summary(fam4de)
summary(fam5de)
summary(fam6de)
summary(fam7de)


fam1it <- lm(fam1 ~ treated, data = it)
fam2it <- lm(fam2 ~ treated, data = it)
fam3it <- lm(fam3 ~ treated, data = it)
fam4it <- lm(fam4 ~ treated, data = it)
fam5it <- lm(fam5 ~ treated, data = it)
fam6it <- lm(fam6 ~ treated, data = it)
fam7it <- lm(fam7 ~ treated, data = it)
summary(fam1it)
summary(fam2it)
summary(fam3it)
summary(fam4it)
summary(fam5it)
summary(fam6it)
summary(fam7it)



# Extract coefficients and manually calculate confidence intervals
extract_coef <- function(model, model_name) {
  tidy(model) %>%
    filter(term == "treated") %>%
    mutate(model = model_name,
           conf.low = estimate - 1.96 * std.error,
           conf.high = estimate + 1.96 * std.error)
}

# Collect data for each country
models_fr <- list(fam1fr, fam2fr, fam3fr, fam4fr, fam5fr, fam6fr, fam7fr)
models_de <- list(fam1de, fam2de, fam3de, fam4de, fam5de, fam6de, fam7de)
models_it <- list(fam1it, fam2it, fam3it, fam4it, fam5it, fam6it, fam7it)

model_names_fr <- paste0("fam", 1:7, "_fr")
model_names_de <- paste0("fam", 1:7, "_de")
model_names_it <- paste0("fam", 1:7, "_it")

# Extract coefficients for all models
coef_fr <- do.call(rbind, lapply(1:7, function(i) extract_coef(models_fr[[i]], model_names_fr[i])))
coef_de <- do.call(rbind, lapply(1:7, function(i) extract_coef(models_de[[i]], model_names_de[i])))
coef_it <- do.call(rbind, lapply(1:7, function(i) extract_coef(models_it[[i]], model_names_it[i])))

# Combine all data into one dataframe
coef_data <- bind_rows(coef_fr, coef_de, coef_it)

# Create the plot
ggplot(coef_data, aes(x = model, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange(size = 2, linewidth = 2) +
  theme_minimal(base_size = 20) +
  geom_hline(yintercept = 0, col = "red", linetype = "dashed", size = 2) +
  labs(title = "ITTs of VAA usage on ideological voting",
       x = "Model",
       y = "Coefficient",
       color = "Model") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


###
### visualize distribution of agreement percentages per country
###


# load merged data
load("rdf.RData")

# turn agreement percentage characters into numeric values for all parties
rdf$leftperc_eu <- as.numeric(gsub("%", "", rdf$Left_EU))
rdf$eppperc_eu <- as.numeric(gsub("%", "", rdf$EPP_EU))
rdf$ecrperc_eu <- as.numeric(gsub("%", "", rdf$ECR_EU))
rdf$renewperc_eu <- as.numeric(gsub("%", "", rdf$Renew_EU))
rdf$sdperc_eu <- as.numeric(gsub("%", "", rdf$SD_EU))
rdf$idperc_eu <- as.numeric(gsub("%", "", rdf$ID_EU))
rdf$greenperc_eu <- as.numeric(gsub("%", "", rdf$Greens_EU))
rdf$leftperc_de <- as.numeric(gsub("%", "", rdf$Left_DE))
rdf$cduperc_de <- as.numeric(gsub("%", "", rdf$CDU_DE))
rdf$spdperc_de <- as.numeric(gsub("%", "", rdf$SPD_DE))
rdf$fdpperc_de <- as.numeric(gsub("%", "", rdf$FDP_DE))
rdf$afdperc_de <- as.numeric(gsub("%", "", rdf$AfD_DE))
rdf$greenperc_de <- as.numeric(gsub("%", "", rdf$Greens_DE))
rdf$lfiperc_fr <- as.numeric(gsub("%", "", rdf$La_France_Insoumise_FR))
rdf$eeperc_fr <- as.numeric(gsub("%", "", rdf$Europe_Ecologie_FR))
rdf$psperc_fr <- as.numeric(gsub("%", "", rdf$Parti_Socialiste_FR))
rdf$renaissanceperc_fr <- as.numeric(gsub("%", "", rdf$Renaissance_FR))
rdf$lrperc_fr <- as.numeric(gsub("%", "", rdf$Les_Republicains_FR))
rdf$rnperc_fr <- as.numeric(gsub("%", "", rdf$Rassamblement_National_FR))
rdf$reconqueteperc_fr <- as.numeric(gsub("%", "", rdf$Reconquete_FR))
rdf$fratelliperc_it <- as.numeric(gsub("%", "", rdf$Fratelli_dItalia_IT))
rdf$pdperc_it <- as.numeric(gsub("%", "", rdf$Partito_Democratico_IT))
rdf$movimentoperc_it <- as.numeric(gsub("%", "", rdf$Movimento_5_Stelle_IT))
rdf$legaperc_it <- as.numeric(gsub("%", "", rdf$Lega_IT))
rdf$forzaperc_it <- as.numeric(gsub("%", "", rdf$Forza_Italia_IT))

# reduce data to respondents who completed the VAA
rdf <- rdf %>% 
  filter(rnperc_fr > -1)


# Reshape the dataset
rdf_nat <- rdf %>%
  pivot_longer(cols = ends_with(c("perc_de", "perc_fr", "perc_it")),
               names_to = "vaa_variable",
               values_to = "agreement_perc") %>%
  mutate(country_var = case_when(
    country == 1 & grepl("perc_fr$", vaa_variable) ~ "fr",
    country == 2 & grepl("perc_de$", vaa_variable) ~ "de",
    country == 3 & grepl("perc_it$", vaa_variable) ~ "it",
    TRUE ~ NA_character_  # Filter out irrelevant rows later
  )) %>%
  filter(!is.na(country_var))


ggplot(rdf_nat, aes(x = country_var, y = agreement_perc, fill=country_var)) +
  geom_boxplot(width = 0.1) +
  xlab('Country') +
  ylab('VAA Agreement Percentage') +
  ggtitle("") +
  theme_classic(base_size=18, base_family="serif")+
  theme(text = element_text(size=18),
        axis.text.x = element_text(angle=0, hjust=.5, vjust = 0.5, color = "black"),
        axis.text.y = element_text(color = "black"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="none")+
  scale_y_continuous(breaks = seq(0, 100, by=10), limits=c(0,100), expand = c(0, 0)) +
  # Line below adds half-violin from {ggdist} package
  stat_halfeye(adjust = .5, width = .6, justification = -.2, .width = 0, point_colour = NA)





###
### check attrition in the field experiment
###

table(df$voted, df$treated, useNA = "always")
table(df$v105, df$treated, useNA = "always")
table(df$party_know, df$treated, useNA = "always")
table(df$policy_know, df$treated, useNA = "always")




###
### check demographic composition of the sample
###


table(df$age)

df$married <- 0
df$married[df$v83 == 1] <- 1

df$male <- NA
df$male[df$v76 == 1] <- 1
df$male[df$v76 == 2] <- 0

df$unemployed <- 0
df$unemployed[df$v84 == 6] <- 1

df$christian <- 0
df$christian[df$v89 == 1 | df$v89 == 2 | df$v89 == 3] <- 1

df$workingclass <- 0
df$workingclass[df$v86 == 1] <- 1

df$educationage <- df$v75_2

table(df$pre_loi)

table(df$post_loi)



france_df <- df %>% 
  filter(country == "1")

germany_df <- df %>% 
  filter(country == "2")

italy_df <- df %>% 
  filter(country == "3")




vars <- c("age", "married", "male", "unemployed", "christian",
          "workingclass", "educationage", "pre_loi", "post_loi")

# helper to compute mean and median
m_mean <- function(x) mean(as.numeric(x), na.rm = TRUE)
m_median <- function(x) median(as.numeric(x), na.rm = TRUE)

# label countries, compute means, reshape, and round
means_by_country <- df %>%
  mutate(country_label = case_when(
    country == "1" ~ "France",
    country == "2" ~ "Germany",
    country == "3" ~ "Italy",
    TRUE ~ NA_character_
  )) %>%
  group_by(country_label) %>%
  summarize(across(all_of(vars),
                   ~ ifelse(cur_column() %in% c("pre_loi", "post_loi"),
                            m_median(.x),  # median for these two
                            m_mean(.x)),   # mean for others
                   .names = "{.col}"),
            .groups = "drop") %>%
  pivot_longer(-country_label, names_to = "Variable", values_to = "Value") %>%
  mutate(Variable = factor(Variable, levels = vars)) %>%
  pivot_wider(names_from = country_label, values_from = Value) %>%
  arrange(Variable) %>%
  mutate(across(where(is.numeric), ~round(., 3)))

row_labels <- c(
  age = "Mean Age in Years",
  married = "Proportion Married",
  male = "Proportion Male",
  unemployed = "Proportion Unemployed",
  christian = "Proportion Christian",
  workingclass = "Proportion Working class",
  educationage = "Mean Age in Years when Finished Education",
  pre_loi = "Median Interview Length in Minutes (Pre-Election)",
  post_loi = "Median Interview Length in Minutes (Post-Election)"
)

means_by_country$Variable <- row_labels[as.character(means_by_country$Variable)]

kable(means_by_country,
      format = "latex",
      booktabs = TRUE,
      caption = "Summary Statistics by Country") %>%
  kable_styling(full_width = FALSE)






