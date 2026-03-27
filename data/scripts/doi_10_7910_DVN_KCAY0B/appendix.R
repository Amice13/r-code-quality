########################################################
# Wolf in Sheep's Clothing - Main Text Replication File
# Chiopris, Nalepa and Vanberg
# Online Appendix
########################################################

# Set up workspace
rm(list=ls())
library(tidyverse)
library(readxl)
library(survey)
library(stargazer)
library(glue)
library(factoextra)
library(kableExtra)
library(broom)
library(knitr)
library(stringr)

# Working directory - change as necessary
setwd("~/Dropbox/Auth Backsliding/REPLICATION MARCH 2024/")

# Load data
data <- read_csv("data.csv")

# Remove unnecessary first column with row numbers
data <- data %>% dplyr::select(-"...1")

# This will properly add weights to each observation
design_cbos <- svydesign(ids=~1, probs=NULL, 
                         strata=NULL, data = data, weights=data$waga)

# Create string with all province names to add dummies in regression formula
p <- data %>% dplyr::select(starts_with("province_")) 
p <- stringr::str_c(colnames(p), collapse = " + ")



#---------------------------------------------------------
# Figure 1
#---------------------------------------------------------

histogram_vignette <- ggplot(data= data) + 
  geom_histogram(aes(v_undem), bins=25,color="black",fill="grey80") +
  ylab("Count") + 
  xlab("Interpretation of the Vignette Reform as Undemocratic") + 
  scale_x_continuous(breaks=c(2,4,6,8,10), labels=c(2,4,6,8,10))+
  theme(panel.grid.major = element_blank())+
  theme_bw()

# Save plot
ggsave(filename="output/appendix/figure1_app.pdf", 
       plot = histogram_vignette,
       width = 5, height = 4, dpi = 300, units = "in", device='pdf')


#---------------------------------------------------------
# Figure 2
#---------------------------------------------------------

m2_conjoint <- svyglm(candidate ~ treatment + log(age) + edu + female + l_r +
                        v_undem_fourgroups + democracy_index + town + unemployed +
                        dissatisfaction + interest_pol, 
                      design = design_cbos, data=data)

data_plot_m2conjoint <- data.frame(
  Estimate = m2_conjoint$coefficients,
  lower = confint(m2_conjoint)[,1],
  upper = confint(m2_conjoint)[,2]
)
data_plot_m2conjoint$Variable <- rownames(data_plot_m2conjoint)
data_plot_m2conjoint <- data_plot_m2conjoint[2:nrow(data_plot_m2conjoint),]

data_plot_m2conjoint$Variable <- factor(data_plot_m2conjoint$Variable, 
                                        levels=c("treatment", "democracy_index", "v_undem_fourgroups",
                                                 "dissatisfaction", "female", "town", "edu",
                                                 "interest_pol", "unemployed",
                                                 "log(age)", "l_r" ))



predictors_conjoint <- ggplot(data = data_plot_m2conjoint) +
  geom_linerange(aes(xmin=lower, xmax=upper, y=Variable))+
  geom_point(aes(x = Estimate,  y = Variable)) +
  theme_bw() +
  geom_vline(aes(xintercept=0), alpha=0.3, color="grey") +
  theme(axis.ticks.x =element_blank(),
        axis.title.y=element_blank(),
        panel.grid= element_blank(),
        panel.grid.major= element_blank(),
        legend.position = "none") +
  scale_y_discrete(labels=c("treatment" = "Treatment" ,
                            "democracy_index" = "Democracy index", 
                            "v_undem_fourgroups" = "Undemocratic interpretation",
                            "dissatisfaction" = "Dissatisfaction", 
                            "female" = "Gender", 
                            "town" = "Town Size",
                            "edu" = "Education",
                            "interest_pol" = "Interest in politics",
                            "unemployed" = "Unemployed",
                            "log(age)" = "Age (logs)", 
                            "l_r"  ="Left-right" ))


ggsave(filename="output/appendix/figure2_app.pdf", 
       plot = predictors_conjoint,
       width = 6, height = 4, dpi = 300, units = "in", device='pdf')


#---------------------------------------------------------
# Figure 3
#---------------------------------------------------------

# Regression for prediction
ff_withoutFE_10 <- as.formula(paste("pis", 
     paste("v_undem*l_r","democracy_index",
     "age", "edu", "town", "unemployed",  "dissatisfaction", "female", 
     "religiosity", "l_r","interest_pol", 
     sep=" + "), sep = " ~ " ))

m2 <- svyglm(ff_withoutFE_10, design = design_cbos, data=data, family=quasibinomial)

# Find left voters
left_voters <- data[data$l_r<=4 & !is.na(data$l_r),]

# Find median values
data_predictions <- data.frame(
  l_r = rep(seq(1, 3, by=1), each=10),
  v_undem = rep(seq(1,10, by=1), 3), 
  v_acc = median(left_voters$v_acc, na.rm = T),     
  age = median(left_voters$age, na.rm = T), 
  edu= median(left_voters$edu, na.rm = T), 
  town = median(left_voters$town, na.rm = T), 
  unemployed=  median(left_voters$unemployed, na.rm = T), 
  dissatisfaction = median(left_voters$dissatisfaction, na.rm = T), 
  female = median(left_voters$female, na.rm = T), 
  religiosity = median(left_voters$religiosity, na.rm = T), 
  interest_pol = median(left_voters$interest_pol, na.rm = T), 
  democracy_index = median(left_voters$democracy_index, na.rm = T))

# Predicted probabilities
predictions <- data.frame(l_r = data_predictions$l_r,
                          v_undem = data_predictions$v_undem, 
                          predict.glm(m2, data_predictions,type="response", se.fit = T))
predictions <- predictions %>% select(-residual.scale)
# Confidence Intervals
predictions$lower <- predictions$fit - 1.96*predictions$se.fit
predictions$upper <- predictions$fit + 1.96*predictions$se.fit
# As factor
predictions$l_r <- as.factor(predictions$l_r)


## Plot

predicted_probs_left <- ggplot(data= predictions, 
                                   aes(x= v_undem, y= fit, group= l_r, color=l_r)) + 
  geom_point() + theme_bw() + geom_linerange(aes(ymin=lower, ymax=upper,  xmin=v_undem, color=l_r)) +
  geom_ribbon(aes(ymin=lower, ymax=upper, 
                  fill=l_r), alpha=0.2, color=NA) +
  scale_color_manual(values = c("grey70", "grey50", "black"), 
                     labels= c("1", "2", "3"))+
  scale_fill_manual(values = c("grey70", "grey50", "black"), 
                    labels= c("1", "2", "3"))+
  labs(x ="Interpretation of vignette reform as undemocratic", 
       y ="Predicted probability of voting for PiS",
       color = "Self-Placement in Left-Right Spectrum") + 
  scale_x_continuous(breaks=seq(2, 10, 2))+
  theme(axis.ticks.x =element_blank(),
        axis.text = element_text(size = 30),
        axis.title=element_text(size=30),
        legend.text = element_text(size = 25),  # Increase the size of legend text
        legend.title = element_text(size = 25), 
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        legend.position = "bottom")+ guides(fill="none") +
  ylim(0,0.5)
predicted_probs_left

ggsave(filename="output/appendix/figure3_app.pdf", 
       plot = predicted_probs_left,
       width = 12, height = 9, dpi = 300, units = "in", device='pdf')


#---------------------------------------------------------
# Figure 4
#---------------------------------------------------------

# Select right voters
right_voters <- data[data$l_r>=5 & !is.na(data$l_r),]

# Model
mod_inter_cj <- lm(data= right_voters, candidate ~ treatment*as.factor(v_undem_fourgroups))
coefeq <- matrix(data=0, nrow=1, ncol=length(mod_inter_cj$coefficients))
colnames(coefeq) <- names(mod_inter_cj$coefficients)
coefeq[1, "treatment"] <- 1

# ACME for group 1
coefeq1 <- coefeq
wht1 <- multcomp::glht(mod_inter_cj, linfct = coefeq1)
g1ci <- confint(wht1) %>% tidy

# ACME for group 2
coefeq2 <- coefeq
coefeq2[1,"treatment:as.factor(v_undem_fourgroups)2"] <- 1
wht2 <- multcomp::glht(mod_inter_cj, linfct = coefeq2)
g2ci <- confint(wht2) %>% tidy

# ACME for group 3
coefeq3 <- coefeq
coefeq3[1,"treatment:as.factor(v_undem_fourgroups)3"] <- 1
wht3 <- multcomp::glht(mod_inter_cj, linfct = coefeq3)
g3ci <- confint(wht3) %>% tidy

# ACME for group 4
coefeq4 <- coefeq
coefeq4[1,"treatment:as.factor(v_undem_fourgroups)4"] <- 1
wht4 <- multcomp::glht(mod_inter_cj, linfct = coefeq4)
g4ci <- confint(wht4) %>% tidy

# Dataframe for plot
data_conditional_interaction <- data.frame(
  group = c("Low", "Mid-Low", "Mid-High", "High"),
  estimate = c(g1ci$estimate, g2ci$estimate, g3ci$estimate, g4ci$estimate),
  lower = c(g1ci$conf.low,g2ci$conf.low,g3ci$conf.low,g4ci$conf.low), 
  upper = c(g1ci$conf.high,g2ci$conf.high,g3ci$conf.high,g4ci$conf.high))

data_conditional_interaction$group <- factor(data_conditional_interaction$group, 
                                             levels =data_conditional_interaction$group)

# Create Plot
plot_conditional_inter <- ggplot(data= data_conditional_interaction, 
                                 aes(x= group, y= estimate)) + 
  geom_point(size=1.7) + theme_bw() + geom_linerange(aes(ymin=lower, ymax=upper, 
                                                         xmin=group), size=0.5) +
  geom_hline(aes(yintercept=0), linetype = "solid", color="brown4", linewidth=0.5) +
  labs(x ="Interpretation of vignette reform as undemocratic", 
       y ="Punishment for undemocratic proposal") + 
  theme(axis.ticks.x =element_blank(),
        axis.title=element_text(size=10),
        panel.grid.minor = element_blank(),
        legend.position = "none") 

# Save plot
ggsave(filename="output/appendix/figure4_app.pdf", 
       plot = plot_conditional_inter,
       width = 5, height = 4, dpi = 300, units = "in", device='pdf')


#---------------------------------------------------------
# Table 1
#---------------------------------------------------------

# Assign names to groups for easier interpretation
data <- data %>% mutate(towngroups = case_when(
  town <= 0 ~ "1. village",
  town == 1 ~"2. town up to 20 000",
  town ==  2 ~"3. city up to 20-50 000",
  town ==  3 ~"4. city up to 50-100 000",
  between(data$town, 4,5) ~"5. city up to 100-500 000",
  town == 6 ~"6. city above 500 000"))

# Raw number
freq_town <- as.data.frame(table(data$towngroups)) 
# Percentage
freq_town$Freq <- freq_town$Freq/nrow(data)*100

# Rename variables for better readability
freq_town <- freq_town %>% 
  rename("locality size" = "Var1", "percentage"= "Freq")

# Write table as txt file and save  
table_output <- capture.output(kable(freq_town, digits=1))
writeLines(table_output, "output/appendix/table1_app.txt")
rm(table_output)


#---------------------------------------------------------
# Table 2
#---------------------------------------------------------

# Create age groups
data <- data %>% mutate(agegroups = case_when(
  age <= 24 ~ "18-24",
  between(data$age, 25,34) ~"25-34",
  between(data$age, 35,44) ~"35-44",
  between(data$age, 45,54) ~"45-54",
  between(data$age, 55,64) ~"55-64",
  between(data$age, 65,85) ~"65-85"))

# Raw number
freq_gender_age <- as.data.frame(table(data$agegroups,data$female)) 
# Percentage
freq_gender_age$Freq <- freq_gender_age$Freq/nrow(data)*100

# Write 0/1 as male/female, and reverse order so female is first
freq_gender_age$Var2 <- ifelse(freq_gender_age$Var2 == 0, "male", "female")
freq_gender_age <- freq_gender_age %>%
  rename("age" = "Var1", "gender" ="Var2", "percentages" = "Freq") %>% 
  dplyr::select(gender,age, percentages) 
freq_gender_age <- freq_gender_age[c(7:12,1:6),]


# Write table as txt file and save  
table_output <- capture.output(kable(freq_gender_age, digits=1)) 
writeLines(table_output, "output/appendix/table2_app.txt")
rm(table_output)


#---------------------------------------------------------
# Table 3
#---------------------------------------------------------

# Create aggregated groups for interpretability
data <- data %>% mutate(edugroups = case_when(
  edu <= 4 ~ "1. some high school or lower",
  between(data$edu, 5,9) ~"2. high school diploma",
  between(data$edu, 10,12) ~"3. college diploma or more"))


# Raw number
freq_edu <- as.data.frame(table(data$edugroups)) 
# Percentage
freq_edu$Freq <- freq_edu$Freq/nrow(data)*100

# Rename for readability
freq_edu <- freq_edu %>%
  rename("education"="Var1", "percentage" = "Freq")


# Write table as txt file and save  
table_output <- capture.output(kable(freq_edu, digits=1)) 
writeLines(table_output, "output/appendix/table3_app.txt")
rm(table_output)

#---------------------------------------------------------
# Table 4
#---------------------------------------------------------

# Raw number
freq_prov <- as.data.frame(table(data$province)) 
# Percentage
freq_prov$Freq <- freq_prov$Freq/nrow(data)*100

# Rename for readability
freq_prov <- freq_prov %>%
  rename("administrative unit"="Var1", "percentage" = "Freq")

# Write table as txt file and save  
table_output <- capture.output(kable(freq_prov, digits=1)) 
writeLines(table_output, "output/appendix/table4_app.txt")
rm(table_output)



#---------------------------------------------------------
# Table 5
#---------------------------------------------------------

## Balance in the following: age, gender, dem, education, town size (?), left-right self placement

data$v_undem_fourgroups <- data$v_undem_fourgroups

# Mean and sd for each variable in each group
table <- data %>% select(c("v_undem_fourgroups", "treatment", "age", "edu", "town", "female",  
                           "democracy_index", "l_r", "religiosity")) %>%
  group_by(treatment, v_undem_fourgroups) %>% 
  filter(complete.cases(across(everything()))) %>%
  summarise(across(everything(), list(mean, sd)))   

# Find number of observations 
n_obs <- data %>% select(c("v_undem_fourgroups", "treatment", "age", "edu", "town", "female",  
                           "democracy_index", "l_r", "religiosity")) %>%
  group_by(treatment, v_undem_fourgroups) %>% 
  filter(complete.cases(across(everything()))) %>%
  summarize(n=n())
table <- merge(table, n_obs, by =c("treatment", "v_undem_fourgroups"))

# Find se
table$sqrt_n <- sqrt(table$n)
table_se <- table %>% select(ends_with("_2"), sqrt_n) %>%
  mutate_all( ~. /sqrt_n)

# Create one single table
colnames(table) <- str_replace(colnames(table), "_1", "_mean")
colnames(table_se) <- str_replace(colnames(table_se), "_2", "_se")
table <- table %>% select("treatment", "v_undem_fourgroups", ends_with("mean"))
table <- cbind(table, table_se) %>% select(-sqrt_n)

# Shape table correctly
table$treatment <- ifelse(table$treatment ==1, "treat", "control")
cols_ms <- paste0(c("mean", "se"), collapse ="|")
table1 <- table %>% pivot_longer(cols = matches(cols_ms),
                                 names_to = "variable", 
                                 values_to = "value") 
table1$type <- str_extract(table1$variable, cols_ms)
table1$variable <- str_remove(table1$variable, paste0(c("_mean", "_se"), collapse ="|"))
table2 <- table1 %>% pivot_wider(
  names_from = c("type", "treatment"),
  values_from = c("value")
)
table2 <- table2 %>% rename(group = v_undem_fourgroups)


# Find t-statistics
tstats_all = data.frame(variable = NA, group=NA, t_stat = NA, p_value =NA)
variables = c("age", "edu", "town", "female",  "democracy_index", "l_r", "religiosity")
for (v in c(1: length(variables))){
  for (i in c(1:4)){
    form = as.formula(glue({variables[v]}, " ~ treatment")) 
    tstats = data.frame(variable = NA, group=NA, t_stat = NA, p_value =NA)
    t = t.test(form, data = data[data$v_undem_fourgroups==i,])
    tstats[i,3] = t$statistic
    tstats[i,4] = t$p.value
    tstats[i,2] = i
    tstats[i,1] = str_extract(t$data.name, "[:graph:]+[:space:]")
    tstats_all <- rbind(tstats_all, tstats)
    rm(tstats)
  }
}
tstats_all <- tstats_all[!is.na(tstats_all$variable),] %>% unique()
tstats_all$variable <- str_trim(tstats_all$variable)

# Merge means, sd with t-stat
balance_table <- merge(table2, tstats_all, by=c("group","variable"))
#rm(t, tstats_all, table, table_se, table1, table2, 
#    cols_ms, form, v,variables, n_obs, i, variables)

balance_table <- balance_table %>% 
  transmute(
    group = group, variable=variable,
    mean_control  = glue("{format(round(mean_control, 3), nsmall = 3)} ({round(se_control, 3)})"),
    mean_treatment  = glue("{format(round(mean_treat, 3), nsmall = 3)} ({round(se_treat, 3)})"),
    t_stat = round(t_stat,3),
    p_value = round(p_value,3)) 


### Adjust the names
balance_table <- balance_table %>% mutate(variable =
                                            recode(variable,  
                                                   democracy_index = "Democracy Index", 
                                                   edu = "Education", 
                                                   female = "Gender", 
                                                   l_r = "Self-Placement in Left-Right Spectrum", 
                                                   age = "Age", 
                                                   religiosity = "Religiosity", 
                                                   town = "Town Size"))

# Create new row for each group
balance_table <- rbind(c("", "Democratic Assessment: Low", "", "", "", ""), 
                       balance_table[balance_table$group==1,], 
                       c("","Democratic Assessment: Mid-Low", "", "", "", ""), 
                       balance_table[balance_table$group==2,], 
                       c("","Democratic Assessment: Mid-High", "", "", "", ""), 
                       balance_table[balance_table$group==3,], 
                       c("","Democratic Assessment: High", "", "", "", ""), 
                       balance_table[balance_table$group==4,])
balance_table <- balance_table %>% select(- group)

# Adjust column names
colnames(balance_table) <- c("Variable", "Mean (SE) - Control", "Mean (SE) - Treatment", 
                             "T-Stat", "P-value")
# Save table as txt
balance_table %>%
  kable(format = "simple", 
        align = "lrrrr",
        row.names = FALSE) %>%
  writeLines("output/appendix/table5_app.txt")



#---------------------------------------------------------
# Table 6
#---------------------------------------------------------


# Column 2-4: use all variables in the regression

# Extract all questions related to democracy
variables_dem <- data %>% dplyr::select(starts_with("d_")) %>% colnames()
# remove extras (indices)
variables_dem <- variables_dem[1:10]
variables_dem <- str_c(variables_dem, collapse = " + ")

# Create formula for regression
formula_dem_questions <- as.formula(glue("pis ~ v_undem*l_r  + age + edu + town + unemployed + 
  dissatisfaction + female + religiosity + province +
  interest_pol  + {variables_dem}"))

# Run reg for Column 2
m_dem_rob1 <- svyglm(formula_dem_questions, 
                     design = design_cbos, data=data, family=quasibinomial)


# Questions about political system
formula_system_questions <- as.formula(glue("pis ~ v_undem*l_r  + age + edu + town + unemployed + 
  dissatisfaction + female + religiosity + province +
  interest_pol+ p_strong_leader + p_experts + p_military + p_democracy"))

# Run reg for Column 3
m_dem_rob2 <- svyglm(formula_system_questions, 
                     design = design_cbos, data=data, family=quasibinomial)


# All question: political system and democracy
formula_all_questions <- as.formula(glue("pis ~ v_undem*l_r  + age + edu + town + unemployed + 
  dissatisfaction + female + religiosity  +
  interest_pol  + p_strong_leader + p_experts + p_military + p_democracy + 
                                         {variables_dem} + province"))

# Run reg for Column 4
m_dem_rob3 <- svyglm(formula_all_questions, 
                     design = design_cbos, data=data, family=quasibinomial)


# Column 1: use first PC
variables_dem_d <- data %>% dplyr::select("SYS_id", starts_with("d_")) 
variables_dem_d <- variables_dem_d[,1:11] %>% drop_na()
pca <- prcomp(variables_dem_d[,2:11], scale = TRUE)

# find coord
coords <- get_pca_ind(pca)
coords <- coords$coord
# merge with all other variables
pca_data <- data.frame(SYS_id = variables_dem_d$SYS_id, 
                       first_pca = coords[,1])
pca_data <- merge(data,pca_data, by="SYS_id")

# Run regression for Column 1
design_cbos2 <- svydesign(ids=~1, probs=NULL, 
                          strata=NULL, data = pca_data, weights=pca_data$waga)

m_dem_rob_pca <- svyglm(pis ~ v_undem*l_r  + age + edu + town + unemployed + 
                          dissatisfaction + female + religiosity  +
                          interest_pol  + first_pca + province, 
                        design = design_cbos2, data=pca_data, family=quasibinomial)



# Turn into tables that are easy to read
tidy_m1 <- tidy(m_dem_rob_pca) %>%
  mutate(column = "(1)")  %>%
  select(term, column, estimate, std.error, p.value) %>%
  mutate(across(c(estimate, std.error, p.value), 
                ~round(., digits = 3)))
tidy_m2 <- tidy(m_dem_rob1) %>%
  mutate(column = "(2)")%>%
  select(term, column,estimate, std.error, p.value) %>%
  mutate(across(c(estimate, std.error, p.value), 
                ~round(., digits = 3)))
tidy_m3 <- tidy(m_dem_rob2) %>%
  mutate(column = "(3)") %>%
  select(term, column,estimate, std.error, p.value) %>%
  mutate(across(c(estimate, std.error, p.value), 
                ~round(., digits = 3)))
tidy_m4 <- tidy(m_dem_rob3) %>%
  mutate(column = "(4)") %>%
  select(term, column,estimate, std.error, p.value) %>%
  mutate(across(c(estimate, std.error, p.value), 
                ~round(., digits = 3)))


# Combine the results into one data frame
combined_results <- rbind(tidy_m1, tidy_m2, tidy_m3, tidy_m4)

# Turn p-values into stars
# Write function
p_value_stars <- function(p) {
  ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.1, "*", "")))
}
# Use function
combined_results <- combined_results %>%
  mutate(significance = sapply(p.value, p_value_stars)) %>%
  select(-p.value)

# Turn into wide format
combined_results_wide <- combined_results %>%
  pivot_wider(names_from = column, 
              values_from = c(estimate, std.error, significance))

# Rename the variables for legibility
combined_results_wide <- combined_results_wide %>%
  mutate(Variable = case_when(
    term == "v_undem" ~ "Undemocratic interpretation",
    term ==  "l_r" ~ "Self-placement in left-right spectrum",
    term ==  "democracy_index" ~ "Expressed Preferences for Democracy",
    term ==  "edu" ~ "Education",
    term == "town" ~ "Town Size",
    term ==  "unemployed" ~ "Unemployed",
    term ==  "dissatisfaction" ~ "Dissatisfaction with one’s condition",
    term ==  "female" ~ "Gender",
    term ==  "religiosity" ~ "Religiosity",
    term ==  "interest_pol" ~ "Interest in politics",
    term ==  "age" ~ "Age",
    term ==  "v_undem:l_r" ~ "Undemocratic interpretation x left-right",
    term == "first_pca"  ~ "First Principal Component"
  ))

# Select only relevant columns and group variables as one row for readability
results <- combined_results_wide %>% 
  dplyr::select(c("Variable", 
                  "estimate_(1)", "std.error_(1)","significance_(1)",
                  "estimate_(2)", "std.error_(2)", "significance_(2)",
                  "estimate_(3)", "std.error_(3)", "significance_(3)",
                  "estimate_(4)", "std.error_(4)", "significance_(4)")) %>%
  filter(!is.na(Variable)) %>% 
  rbind(c("Democracy Variables", 
          "No", "", "", 
          "Yes", "", "", 
          "No", "", "", 
          "Yes", "", "")) %>%
  rbind(c("Political System Variables", 
          "No", "", "", 
          "No", "", "", 
          "Yes", "", "", 
          "Yes", "", "")) %>%
  rbind(c("Province FE", 
          "Yes", "","",
          "Yes", "", "", 
          "Yes", "", "", 
          "Yes", "", "")) %>%
  rbind(c("Number of Observations", # Add number of observations 
          nobs(m_dem_rob_pca), "", "", 
          nobs(m_dem_rob1),"", "", 
          nobs(m_dem_rob2),"", "",
          nobs(m_dem_rob3),"", ""
          )) %>% 
  rename( # rename variables
    est1 = "estimate_(1)", st.err1 = "std.error_(1)", sig1 = "significance_(1)",
    est2 = "estimate_(2)",  st.err2 = "std.error_(2)", sig2 = "significance_(2)",
    est3 = "estimate_(3)", st.err3 = "std.error_(3)", sig3 = "significance_(3)",
    est4 = "estimate_(4)", st.err4 = "std.error_(4)", sig4 = "significance_(4)"
  ) %>% # change "NA" into empty space
  mutate(across(everything(), ~if_else(is.na(.), "", as.character(.))))
# Rearrange rows so that more important are on top
results <- results[c(1,2,12,3:11,13:16),]

# Remove unnecessary dataframes
rm(combined_results, combined_results_wide, tidy_m1, tidy_m2, tidy_m3, tidy_m4)

# Create a table with kable
table_output <- kable(results)

# Capture the table output 
captured_output <- capture.output(table_output)

# Write the output to a text file
writeLines(captured_output, "output/appendix/table6_app.txt")






