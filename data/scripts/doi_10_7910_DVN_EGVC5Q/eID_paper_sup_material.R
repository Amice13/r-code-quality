###########################################
## Replication file: Garbe et al        ### 
## This file reproduces all tables and  ###
## figures in the paper as well as the  ###
## online appendix                      ###
###########################################

# required packages

library(haven)
library(dplyr)
library(tidyverse)
library(stargazer)
library(lmtest)
library(sandwich)
library(multiwayvcov)
library(estimatr)
library(xtable)
library(janitor)
library(vtable)
library(ggpubr)
library(cregg) # for marginal means
library(cjbart) # for heterogeneity analysis
library(ggsci)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### 1. Load and recode data ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Load data
d <- read_csv("./Data/eID_data.csv")
# Drop incomplete interviews
d <- d %>% filter(consent == 1 & Status == "Approved")

# Run conjoint clean code to produce profile-level dataset
source("Code/cjoint_clean_final.R")

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### 2. PAPER ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++

######## FIGURE 2 ########

process_data <- function(data, group_var, factor_var) {
  data %>%
    mutate(digital_id_supportive_factor = as.factor(digital_id_supportive),
           likely_register_digital_id_factor = as.factor(likely_register_digital_id)) %>%
    group_by({{ group_var }}, {{ factor_var }}) %>%
    summarize(n = n()) %>%
    group_by({{ group_var }}) %>%
    mutate(total = sum(n),
           pct = (n/total) * 100) %>%
    filter(!is.na({{ group_var }})) %>% 
    mutate({{ group_var }} := case_when(
      {{ group_var }} == "dominant" ~ "Dominant",
      {{ group_var }} == "opposition" ~ "Opposition",
      {{ group_var }} == "securitized" ~ "Securitized",
      {{ group_var }} == "other" ~ "Other",
      TRUE ~ {{ group_var }})) %>% 
    filter(!is.na({{ factor_var}})) 
}

term_order <- c("Dominant", "Opposition", "Securitized", "Other") # factor levels for ethnic_group variable

## register
groups_register <- process_data(cjoint_clean, ethnic_group, likely_register_digital_id_factor)
groups_register$ethnic_group <- factor(groups_register$ethnic_group, levels = term_order)

# plot by ethnic group
groups_register_plot <- ggplot(groups_register, aes(fill=likely_register_digital_id_factor, y=pct, x=ethnic_group)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(values = c("1" = "gray5", "2" = "gray35", "3" = "gray65", "4" = "gray80"),
                    labels = c("Not at all likely", "Not very likely", "Somewhat likely", "Very likely"),
                    name = "If this policy were enacted, \nhow likely would you be to \nregister for the eID program?") +
  geom_vline(xintercept = 0.25) +
  labs( x="", y="" ) +
  theme_minimal() +
  ggtitle("By Group")+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text( size=12, angle = 0, vjust = 0.5),
        axis.text = element_text( size=12, hjust=0.5),
        legend.text = element_text( size=12),
        legend.title = element_text( size=12, face="bold"),
        axis.text.x = element_text(angle = 30)) 

ggsave(groups_register_plot,
       file = "Writing/Figures/register_groups_plot.pdf",
       height = 4,
       width = 7)

# plot full sample
register_all_plot <- ggplot(groups_register, aes(x=likely_register_digital_id_factor, y=pct,fill=likely_register_digital_id_factor)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(values = c("1" = "gray5", "2" = "gray35", "3" = "gray65", "4" = "gray80"),
                    labels = c("Not at all likely", "Not very likely", "Somewhat likely", "Very likely"),
                    name = "If this policy were enacted, \nhow likely would you be to \nregister for the eID program?") +
  geom_vline(xintercept = 0.25) +
  labs( x="", y="%" ) +
  theme_minimal() +
  ggtitle("Full Sample")+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text( size=12, angle = 0, vjust = 0.5),
        axis.text = element_text( size=12, hjust=0.5),
        legend.text = element_text( size=12),
        legend.title = element_text( size=12, face="bold" ),
        axis.text.x = element_blank())

ggsave(register_all_plot,
       file = "Writing/Figures/register_all_plot.pdf",
       height = 4,
       width = 7)

groups_register_both <- ggarrange(register_all_plot, groups_register_plot, common.legend = TRUE, align = "h", legend = "right")

## support
groups_support <- process_data(cjoint_clean, ethnic_group, digital_id_supportive_factor)
groups_support$ethnic_group <- factor(groups_support$ethnic_group, levels = term_order)

# plot by ethnic group
groups_support_plot <- ggplot(groups_support, aes(fill=digital_id_supportive_factor, y=pct, x=ethnic_group)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(values = c("1" = "gray5", "2" = "gray35", "3" = "gray65", "4" = "gray80"),
                    labels = c("Not at all supportive", "Not very supportive", "Somewhat supportive", "Very supportive"),
                    name = "If this policy were enacted, \nhow supportive would you be \nof the digital ID program?") +
  geom_vline(xintercept = 0.25) +
  labs(x="", y="" ) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text( size=12, angle = 0, vjust = 0.5),
        axis.text = element_text( size=12, hjust=0.5),
        legend.text = element_text( size=12),
        legend.title = element_text( size=12, face="bold" ),
        axis.text.x = element_text(angle = 30)) 

ggsave(groups_support_plot,
       file = "Writing/Figures/support_groups_plot.pdf",
       height = 4,
       width = 7)

# plot support full sample 
support_all_plot <- ggplot(groups_support, aes(x=digital_id_supportive_factor, y=pct,fill=digital_id_supportive_factor)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(values = c("1" = "gray5", "2" = "gray35", "3" = "gray65", "4" = "gray80"),
                    labels = c("Not at all supportive", "Not very supportive", "Somewhat supportive", "Very supportive"),
                    name = "If this policy were enacted, \nhow supportive would you be \nof the digital ID program?") +
  geom_vline(xintercept = 0.25) +
  labs( x="", y="%" ) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text( size=12, angle = 0, vjust = 0.5),
        axis.text = element_text( size=12, hjust=0.5),
        legend.text = element_text( size=12),
        legend.title = element_text( size=12, face="bold" ),
        axis.text.x = element_blank())

ggsave(support_all_plot,
       file = "Writing/Figures/support_all_plot.pdf",
       height = 4,
       width = 7)

groups_support_both <- ggarrange(support_all_plot,groups_support_plot, common.legend = TRUE, align = "h", legend = "right")

register_support_all <- ggarrange(groups_register_both,groups_support_both, align = "h",ncol = 1)

ggsave(register_support_all,
       file = "Writing/Figures/support_register_joint_plot.pdf",
       height = 10,
       width = 12)


######## FIGURE 3 ########

## Primary outcomes
primary_outcomes <- c("choice_final", "digital_id_supportive", "likely_register_digital_id")

### Plot with CATE
### show just main effects for subgroups
x_coef_label_list <- list("Constant" = "(Intercept)",
                          "Public Service" = "public_service_dm",
                          "Social Protection" = "social_prot_dm" ,
                          "Surveillance" = "security_dm",
                          "Voting" = "voting_dm",
                          "Tax Registration" = "tax_reg_dm")

dominant <- cjoint_clean[cjoint_clean$dominant == 1, ]
opposition <- cjoint_clean[cjoint_clean$opposition == 1, ]
securitized <- cjoint_clean[cjoint_clean$securitized == 1, ]
other <- cjoint_clean[cjoint_clean$other == 1, ]

maineffectsFUN_groups_plot <- function(data, vars) {
  lm_robust(
    formula = formula(paste0("cbind(", paste0(vars, collapse = ", "), ") ~  
                                      (public_service_dm + social_prot_dm + security_dm + tax_reg_dm + voting_dm)")),
    data = data,
    clusters = instanceID,
    se_type = "stata"
  ) %>%
    tidy() %>%
    mutate(term = factor(term)) %>%
    filter(term != "(Intercept)")
}

dom_main <- maineffectsFUN_groups_plot(dominant, primary_outcomes)
opp_main  <- maineffectsFUN_groups_plot(opposition, primary_outcomes)
sec_main  <- maineffectsFUN_groups_plot(securitized, primary_outcomes)
other_main  <- maineffectsFUN_groups_plot(other, primary_outcomes)
all_main  <- maineffectsFUN_groups_plot(cjoint_clean, primary_outcomes)

levels(dom_main$term) <- x_coef_label_list
levels(opp_main$term) <- x_coef_label_list
levels(sec_main$term) <- x_coef_label_list
levels(other_main$term) <- x_coef_label_list
levels(all_main$term) <- x_coef_label_list

attribute_names <- c("Public Service", "Social Protection", "Surveillance", "Tax Registration", "Voting")

dom_main$ethnic_group <- "dominant"
opp_main$ethnic_group <- "opposition"
sec_main$ethnic_group <- "securitized"
other_main$ethnic_group <- "other"
all_main$ethnic_group <- "all"

main_coefs_groups <- rbind(dom_main, opp_main, sec_main, other_main, all_main)
main_coefs_groups$outcome_name[main_coefs_groups$outcome == "choice_final"] <- "Choice"
main_coefs_groups$outcome_name[main_coefs_groups$outcome == "digital_id_supportive"] <- "Support"
main_coefs_groups$outcome_name[main_coefs_groups$outcome == "likely_register_digital_id"] <- "Register"

main_coefs_groups$term <- factor(main_coefs_groups$term, levels = c("Public Service",
                                                                    "Social Protection",
                                                                    "Surveillance",
                                                                    "Tax Registration",
                                                                    "Voting"))

term_order <- c("other",  "securitized", "opposition", "dominant","all")
main_coefs_groups$ethnic_group <- factor(main_coefs_groups$ethnic_group, levels = term_order)                                                                   
main_coefs_groups$outcome_name <- factor(main_coefs_groups$outcome_name, levels = c("Choice", "Support", "Register")) 

main_coefs_plot <-  ggplot(main_coefs_groups,aes(x = estimate,
                                                 y = term,
                                                 color = ethnic_group, 
                                                 shape = ethnic_group)) +
  facet_wrap(~outcome_name) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_pointrange(aes(xmin = conf.low, 
                      xmax = conf.high),
                  position = position_dodge(width = 0.6))  + # <-- use scale_x_continuous instead of scale_y_discrete
  scale_y_discrete(limits = rev) +
  scale_color_manual(labels = c("Other", "Securitized","Opposition", "Dominant","All"),
                     values = c("dominant" = "chartreuse4","opposition" = "firebrick","securitized" = "darkblue", "other" = "gray50", "all" = "gray25"),
                     guide = guide_legend(reverse = TRUE)) +
  scale_shape_manual(labels = c("Other", "Securitized", "Opposition", "Dominant","All"),
                     values = c("dominant" =15,"opposition" =19,"securitized" =17, "other" = 8, "all" = 9),
                     guide = guide_legend(reverse = TRUE)) +  #coord_cartesian(xlim = c(-1,1)) +
  theme_bw()+ 
  theme(legend.position="bottom", 
        legend.title = element_text(size = 8), 
        legend.text = element_text(size = 8)) +
  labs(x = "Coefficient", y = "Attribute")+
  theme(legend.title = element_blank())

ggsave(main_coefs_plot, filename = "Writing/Figures/main_coefs.pdf",
       width = 8.5, height = 6.5)


####### FIGURE 4 ########
secondary_outcomes <- c("easy_access_govt_services",
                        "adequate_data_privacy",
                        "worry_punished_view",
                        "worry_vote_counted",
                        "worry_personal_info")

cjoint_clean_r1_dom <- subset(cjoint_clean_r1, dominant == 1)
cjoint_clean_r1_opp <- subset(cjoint_clean_r1, opposition == 1)
cjoint_clean_r1_sec <- subset(cjoint_clean_r1, securitized == 1)
cjoint_clean_r1_other <- subset(cjoint_clean_r1, other == 1)


dom_main <- maineffectsFUN_groups_plot(cjoint_clean_r1_dom, secondary_outcomes)
opp_main <- maineffectsFUN_groups_plot(cjoint_clean_r1_opp, secondary_outcomes)
sec_main <- maineffectsFUN_groups_plot(cjoint_clean_r1_sec, secondary_outcomes)
other_main <- maineffectsFUN_groups_plot(cjoint_clean_r1_other, secondary_outcomes)
all_main <- maineffectsFUN_groups_plot(cjoint_clean_r1, secondary_outcomes)


levels(dom_main$term) <- x_coef_label_list
levels(opp_main$term) <- x_coef_label_list
levels(sec_main$term) <- x_coef_label_list
levels(other_main$term) <- x_coef_label_list
levels(all_main$term) <- x_coef_label_list

## show just main effects for subgroups

dom_main$ethnic_group <- "dominant"
opp_main$ethnic_group <- "opposition"
sec_main$ethnic_group <- "securitized"
other_main$ethnic_group <- "other"
all_main$ethnic_group <- "all"

mechanisms_coefs_groups <- rbind(dom_main, opp_main, sec_main, other_main, all_main)
mechanisms_coefs_groups$outcome_name[mechanisms_coefs_groups$outcome == "easy_access_govt_services"] <- "Access Services"
mechanisms_coefs_groups$outcome_name[mechanisms_coefs_groups$outcome == "adequate_data_privacy"] <- "Data Privacy"
mechanisms_coefs_groups$outcome_name[mechanisms_coefs_groups$outcome == "worry_punished_view"] <- "Worry Punished View"
mechanisms_coefs_groups$outcome_name[mechanisms_coefs_groups$outcome == "worry_vote_counted"] <- "Worry Vote Counted"
mechanisms_coefs_groups$outcome_name[mechanisms_coefs_groups$outcome == "worry_personal_info"] <- "Worry Personal Info"

term_order <- c("other","securitized" ,"opposition","dominant","all")
mechanisms_coefs_groups$ethnic_group <- factor(mechanisms_coefs_groups$ethnic_group, levels = term_order)                          

mechanisms_coefs_plot <-
  ggplot(mechanisms_coefs_groups,aes(x = estimate,
                                     y = term,
                                     color = ethnic_group, 
                                     shape = ethnic_group
  )) +
  facet_wrap(~outcome_name) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_pointrange(aes(xmin = conf.low, 
                      xmax = conf.high),
                  position = position_dodge(width = 0.7))  + # <-- use scale_x_continuous instead of scale_y_discrete
  scale_y_discrete(limits = rev) +
  scale_color_manual(labels = c("Other", "Securitized","Opposition", "Dominant","All"),
                     values = c("dominant" = "chartreuse4","opposition" = "firebrick","securitized" = "darkblue", "other" = "gray50", "all" = "gray25"),
                     guide = guide_legend(reverse = TRUE)) +
  scale_shape_manual(labels = c("Other", "Securitized", "Opposition", "Dominant","All"),
                     values = c("dominant" =15,"opposition" =19,"securitized" =17, "other" = 8, "all" = 9),
                     guide = guide_legend(reverse = TRUE)) +  #coord_cartesian(xlim = c(-1,1)) +
  theme_bw()+ 
  theme(legend.position="bottom", 
        legend.title = element_text(size = 11), 
        legend.text = element_text(size = 16),
        axis.text.y = element_text(size = 15),
        strip.text.x = element_text(size = 16),
        axis.title = element_text(size = 16),
        axis.text.x = element_text(size = 9,angle = 45, vjust = 1, hjust=1)) +
  labs(x = "Coefficient", y = "Attribute")+
  theme(legend.title = element_blank())

ggsave(mechanisms_coefs_plot, filename = "Writing/Figures/mechanisms_coefs.pdf",
       width = 11, height = 13)


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### 3. APPENDIX (PAPER) ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++

######## TABLES: DESCRIPTIVE STATISTICS (TABLE A1) ######## 

vars_list = c("hear_huduma_namba",
              "voted_binary",
              "female",
              "selected_age",
              "garissa", "homabay", "kirinyaga","kisumu","migori","muranga",
              "nairobi", "nyandarua", "nyeri", "siaya",
              "kalenjin", "kikuyu", "luo", "somali", "other_language", "language_refused",
              "no_formal_schooling", "some_primary_school", "primary_school_completed", 
              "secondary_school_completed", "post_secondary_other_than_uni", 
              "at_least_some_university","education_refused",
              "christian", "muslim", "no_religion", "religion_refused")

descriptive_vars <- d %>%
  select(all_of(vars_list))

desc_labs <- c("Heard Huduma Namba",
               "Voted in 2017 Election",
               "Female",
               "Age",
               "Garissa", "Homa Bay", "Kirinyaga", "Kisumu", "Migori", "Murang'a",
               "Nairobi", "Nyandarua", "Nyeri", "Siaya",
               "Kalenjin","Kikuyu","Luo",
               "Somali", "Other Language",
               "Refused to answer",
               "No formal schooling",
               "Some primary school",
               "Primary school completed",
               "Secondary school completed",
               "Post-secondary qualifications other than university",
               "At least some university",
               "Refused to answer",
               "Christian",
               "Muslim",
               "No Religion",
               "Refused to answer")

st(descriptive_vars, summ = c( 'notNA(x)', 'mean(x)','sd(x)','min(x)','max(x)'), labels = desc_labs,
   summ.names = c('N', 'Mean', 'Std. Dev.', 'Min', 'Max'), digits = 3, #fixed.digits = TRUE,
   out = "latex", file = "Writing/Tables/descriptive_stats")


######## TABLES: REGIONS AND ETHNICITY (TABLE A2) ######## 

region_ethnicity <- d %>%
  count(region, language, name = "n") %>%
  pivot_wider(names_from = region, values_from = n) %>%
  adorn_totals(c("row","col"))

re_rws <- c(1,3,5,7,9,11,13,15)
re_col <- rep("\\rowcolor[gray]{0.95}", length(re_rws))

table_output <- print(xtable(region_ethnicity), booktabs = TRUE, include.rownames = FALSE,
                      add.to.row = list(pos = as.list(re_rws), command = re_col))

output_file <- "Writing/Tables/descriptive_region_ethnicity.tex"
cat(table_output, sep = "\n", file = output_file)

######## TABLES: BALANCE (TABLE A3) ######## 

balanceFUN <- function(outcome, data){
  data <- data
  f <- paste0(outcome, "~ hear_huduma_namba + voted_binary + female + selected_age + as.factor(region) + as.factor(tongue) + as.factor(education) + as.factor(religion)")
  mod.main <- lm(as.formula(f), data = data)
  mod.main.c <- coeftest(mod.main, vcov = vcovCL(mod.main, cluster = data$instanceID))
  return(list(mod.main = mod.main,
              se = mod.main.c[, 2]))
}

attributes <- c("public_service",  "social_prot", "security", "tax_reg", "voting")

lm_balance <- lapply(attributes, function(x) balanceFUN(x, cjoint_clean))

coef_labs_balance = c("Heard Huduma Namba", "Voted in 2017 Election","Female","Age",
                      "Region: Homa Bay", "Region: Kirinyaga", "Region: Kisumu",
                      "Region: Migori","Region: Murang'a", "Region: Nairobi",
                      "Region: Nyandarua","Region: Nyeri","Region: Siaya",
                      "Language: Kalenjin","Language: Kikuyu","Language: Luo",
                      "Language: Somali", 
                      "Education: Some primary school",
                      "Education: Primary school completed",
                      "Education: Secondary school completed",
                      "Education: Post-secondary qualifications other than university",
                      "Education: At least some university",
                      "Religion: Christian",
                      "Religion: Muslim",
                      "Religion: No Religion",
                      "Constant")

stargazer(lapply(lm_balance, '[[', 1),
          se = lapply(lm_balance, '[[', 2),
          omit.stat = c("f", "ser"),
          column.labels = c("Public Service", "Social Protection", "Surveillance", "Tax Registration", "Voting"),
          covariate.labels = coef_labs_balance,
          add.lines = list("CRSE at respondent level"),
          type = "latex",
          out = "Writing/Tables/balance_regressions.tex",
          no.space = T,
          font.size = "footnotesize",
          float = F, header=F, 
          omit.table.layout = "nld",
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001))

######## TABLES: MAIN EFFECTS MODEL (TABLE A4) ######## 

dom_maineffectsFun<- function(outcome, data, group){
  data <- data
  if(group == "socioecon_edu_control"){
    f <- paste0(outcome, "~ (public_service_dm + social_prot_dm + security_dm + tax_reg_dm + voting_dm)*(dominant_dm + ", socioeconomic_edu_controls_plus, ")")
  } else if (group == "enumFE"){
    f <- paste0(outcome, "~ (public_service_dm + social_prot_dm + security_dm + tax_reg_dm + voting_dm)*(dominant_dm + ", enum_dm_list_plus, ")")
  } else if (group == "roundFE"){
    f <- paste0(outcome, "~ (public_service_dm + social_prot_dm + security_dm + tax_reg_dm + voting_dm)*(dominant_dm + ", round_dm_list_plus, ")")
  } else if (group == "main"){
    f <- paste0(outcome, "~ (public_service_dm + social_prot_dm + security_dm + tax_reg_dm + voting_dm)*(dominant_dm)")
  } else if (group == "nairobi"){
    f <- paste0(outcome, "~ (public_service_dm + social_prot_dm + security_dm + tax_reg_dm + voting_dm)*(dominant_dm + nairobi_dm)")
  }
  mod.main <- lm(as.formula(f), data = data) 
  mod.main.c <- coeftest(mod.main, vcov = vcovCL(mod.main, cluster = data$instanceID))
  return(list(mod.main = mod.main,
              se = mod.main.c[, 2],
              pval = mod.main.c[, 4]
  ))
}

opp_maineffectsFun<- function(outcome, data, group){
  data <- data
  if(group == "socioecon_edu_control"){
    f <- paste0(outcome, "~ (public_service_dm + social_prot_dm + security_dm + tax_reg_dm + voting_dm)*(opposition_dm + ", socioeconomic_edu_controls_plus, ")")
  } else if (group == "enumFE"){
    f <- paste0(outcome, "~ (public_service_dm + social_prot_dm + security_dm + tax_reg_dm + voting_dm)*(opposition_dm + ", enum_dm_list_plus, ")")
  } else if (group == "roundFE"){
    f <- paste0(outcome, "~ (public_service_dm + social_prot_dm + security_dm + tax_reg_dm + voting_dm)*(opposition_dm + ", round_dm_list_plus, ")")
  } else if (group == "main"){
    f <- paste0(outcome, "~ (public_service_dm + social_prot_dm + security_dm + tax_reg_dm + voting_dm)*(opposition_dm)")
  } else if (group == "nairobi"){
    f <- paste0(outcome, "~ (public_service_dm + social_prot_dm + security_dm + tax_reg_dm + voting_dm)*(opposition_dm + nairobi_dm)")
  }
  mod.main <- lm(as.formula(f), data = data) 
  mod.main.c <- coeftest(mod.main, vcov = vcovCL(mod.main, cluster = data$instanceID))
  return(list(mod.main = mod.main,
              se = mod.main.c[, 2],
              pval = mod.main.c[, 4]
  ))
}

sec_maineffectsFun<- function(outcome, data, group){
  data <- data
  if(group == "socioecon_edu_control"){
    f <- paste0(outcome, "~ (public_service_dm + social_prot_dm + security_dm + tax_reg_dm + voting_dm)*(securitized_dm + ", socioeconomic_edu_controls_plus, ")")
  } else if (group == "enumFE"){
    f <- paste0(outcome, "~ (public_service_dm + social_prot_dm + security_dm + tax_reg_dm + voting_dm)*(securitized_dm + ", enum_dm_list_plus, ")")
  } else if (group == "roundFE"){
    f <- paste0(outcome, "~ (public_service_dm + social_prot_dm + security_dm + tax_reg_dm + voting_dm)*(securitized_dm + ", round_dm_list_plus, ")")
  } else if (group == "main"){
    f <- paste0(outcome, "~ (public_service_dm + social_prot_dm + security_dm + tax_reg_dm + voting_dm)*(securitized_dm )")
  } else if (group == "nairobi"){
    f <- paste0(outcome, "~ (public_service_dm + social_prot_dm + security_dm + tax_reg_dm + voting_dm)*(securitized_dm + nairobi_dm)")
  }
  mod.main <- lm(as.formula(f), data = data) 
  mod.main.c <- coeftest(mod.main, vcov = vcovCL(mod.main, cluster = data$instanceID))
  return(list(mod.main = mod.main,
              se = mod.main.c[, 2],
              pval = mod.main.c[, 4]
  ))
}

main_ethnic_opp <- lapply(primary_outcomes, function(x) opp_maineffectsFun(x, cjoint_clean , "main"))
main_ethnic_sec <- lapply(primary_outcomes, function(x) sec_maineffectsFun(x, cjoint_clean , "main"))
main_ethnic_dom <- lapply(primary_outcomes, function(x) dom_maineffectsFun(x, cjoint_clean , "main"))

main_ethnic_new <- c(main_ethnic_dom, main_ethnic_opp, main_ethnic_sec)

coef_labs_main_new <- c("Public Service",
                        "Social Protection",
                        "Surveillance",
                        "Tax Registration",
                        "Voting",
                        "Dominant",
                        "Pub. Service x Dom.",
                        "Soc. Prot. x Dom.",
                        "Surveillance x Dom.",
                        "Tax Reg. x Dom.",
                        "Voting x Dom.",
                        "Opposition",
                        "Pub. Service x Opp.",
                        "Soc. Prot. x Opp.",
                        "Surveillance x Opp.",
                        "Tax Reg. x Opp.",
                        "Voting x Opp.",
                        "Securitized",
                        "Pub. Service x Sec.",
                        "Soc. Prot. x Sec.",
                        "Surveillance x Sec.",
                        "Tax Reg. x Sec.",
                        "Voting x Sec.",
                        "Constant")

stargazer(lapply(main_ethnic_new, '[[', 1),
          se = lapply(main_ethnic_new, '[[', 2),
          omit.stat = c("f", "ser"),
          covariate.labels = coef_labs_main_new,
          column.labels = rep(c("Choice", "Support", "Register"), 3),
          add.lines = list("CRSE at respondent level"),
          type = "latex",
          out = "Writing/Tables/main_effects_new.tex",
          no.space = T,
          font.size = "footnotesize",
          float = F, header=F, 
          omit.table.layout = "nld",
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001))

######## TABLES: MAIN EFFECTS MODEL (TABLE A5-A7) ######## 

##### TABLE A5 
main_secondary_dom <- lapply(secondary_outcomes, function(x) dom_maineffectsFun(x, cjoint_clean_r1, "main"))

coef_labs_main_dom <- c("Public Service",
                        "Social Protection",
                        "Surveillance",
                        "Tax Registration",
                        "Voting",
                        "Dominant",
                        "Pub. Service x Dom.",
                        "Soc. Prot. x Dom.",
                        "Surveillance x Dom.",
                        "Tax Reg. x Dom.",
                        "Voting x Dom.",
                        "Constant")

stargazer(lapply(main_secondary_dom, '[[', 1),
          se = lapply(main_secondary_dom, '[[', 2),
          omit.stat = c("f", "ser"),
          column.labels = c("Access Services", "Data Privacy", "Worry", "Worry", "Worry \\\\ & & & Punished View & Vote Counted & Personal Info"),
          covariate.labels = coef_labs_main_dom,
          add.lines = list("CRSE at respondent level"),
          title = "Mechanisms: Dominant",
          label = "tab:mechanisms_dom",
          type = "latex",
          out = "Writing/Tables/mechanisms_dom.tex",
          no.space = T,
          font.size = "footnotesize",
          float = F, header=F, 
          omit.table.layout = "nld",
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001))

##### TABLE A6 
main_secondary_opp <- lapply(secondary_outcomes, function(x) opp_maineffectsFun(x, cjoint_clean_r1 , "main"))
coef_labs_main_opp <- c("Public Service",
                        "Social Protection",
                        "Surveillance",
                        "Tax Registration",
                        "Voting",
                        "Opposition",
                        "Pub. Service x Opp.",
                        "Soc. Prot. x Opp.",
                        "Surveillance x Opp.",
                        "Tax Reg. x Opp.",
                        "Voting x Opp.",
                        "Constant")

stargazer(lapply(main_secondary_opp, '[[', 1),
          se = lapply(main_secondary_opp, '[[', 2),
          omit.stat = c("f", "ser"),
          column.labels = c("Access Services", "Data Privacy", "Worry", "Worry", "Worry \\\\ & & & Punished View & Vote Counted & Personal Info"),
          covariate.labels = coef_labs_main_opp,
          add.lines = list("CRSE at respondent level"),
          title = "Mechanisms: Opposition",
          label = "tab:mechanisms_opp",
          type = "latex",
          out = "Writing/Tables/mechanisms_opp.tex",
          no.space = T,
          font.size = "footnotesize",
          float = F, header=F, 
          omit.table.layout = "nld",
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001))

##### TABLE A7 
main_secondary_sec <- lapply(secondary_outcomes, function(x) sec_maineffectsFun(x, cjoint_clean_r1, "main"))
coef_labs_main_sec <- c("Public Service",
                        "Social Protection",
                        "Surveillance",
                        "Tax Registration",
                        "Voting",
                        "Securitized",
                        "Pub. Service x Sec.",
                        "Soc. Prot. x Sec.",
                        "Surveillance x Sec.",
                        "Tax Reg. x Sec.",
                        "Voting x Sec.",
                        "Constant")

stargazer(lapply(main_secondary_sec, '[[', 1),
          se = lapply(main_secondary_sec, '[[', 2),
          omit.stat = c("f", "ser"),
          column.labels = c("Access Services", "Data Privacy", "Worry", "Worry", "Worry \\\\ & & & Punished View & Vote Counted & Personal Info"),
          covariate.labels = coef_labs_main_sec,
          add.lines = list("CRSE at respondent level"),
          title = "Mechanisms: Securitized",
          label = "tab:mechanisms_sec",
          type = "latex",
          out = "Writing/Tables/mechanisms_sec.tex",
          no.space = T,
          font.size = "footnotesize",
          float = F, header=F, 
          omit.table.layout = "nld",
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001))

#### FIGURES: MARGINAL MEANS (FIGURES A1-A5) ####

# attr(cjoint_clean_r1$public_service_f, "label") <- "Public Services"
# attr(cjoint_clean_r1$social_prot_f, "label") <- "Social Protection"
# attr(cjoint_clean_r1$security_f, "label") <- "Surveillance"
# attr(cjoint_clean_r1$tax_reg_f, "label") <- "Tax Registration"
# attr(cjoint_clean_r1$voting_f, "label") <- "Voting"

outcome_means <- cjoint_clean_r1 %>%
  group_by(group) %>%
  summarize(worry_punished_view = mean(worry_punished_view, na.rm = T),
            easy_access_govt_services = mean(easy_access_govt_services, na.rm = T),
            adequate_data_privacy = mean(adequate_data_privacy, na.rm = T),
            worry_vote_counted = mean(worry_vote_counted, na.rm = T),
            worry_personal_info = mean(worry_personal_info, na.rm = T)) %>%
  ungroup() %>%
  mutate(BY = group)

mm_figure <- function(data, outcome_var, xlim_range, xlab_text, file_name) {
  mms <- cj(data, 
            formula(paste(outcome_var, "~ public_service_f + social_prot_f + security_f + tax_reg_f + voting_f")),
            id = ~instanceID,
            estimate = "mm",
            by = ~group)
  
  plot(mms, xlim = xlim_range, xlab = xlab_text) +
    ggplot2::facet_wrap(~BY, ncol = 2) +
    ggplot2::geom_vline(aes(xintercept = !!sym(outcome_var)), data = outcome_means, linewidth = 0.25) 
  ggsave(paste("Writing/Figures/mm_groups_facet_",file_name,".pdf",sep=""), width = 9, height = 6)
}

##### FIGURE A1 - BETTER/EASIER ACCESS TO GOV SERVICES UNDER eID 

mm_figure(cjoint_clean_r1, "easy_access_govt_services", c(1, 4), "Marginal Mean (Agreement, 4 = Strongly Agree)", "easy_access_govt_services")

##### FIGURE A2 - DATA PRIVACY PROTECTION UNDER eID 

mm_figure(cjoint_clean_r1, "adequate_data_privacy", c(1, 4), "Marginal Mean (Agreement, 4 = Strongly Agree)", "adequate_data_privacy")

##### FIGURE A3 - CONCERN USAGE OF PERSONAL INFO UNDER eID 

mm_figure(cjoint_clean_r1, "worry_personal_info", c(1, 4), "Marginal Mean (Agreement, 4 = Strongly Agree)", "worry_personal_info")

##### FIGURE A4 - CONCERN VOTE COUNTING UNDER eID 

mm_figure(cjoint_clean_r1, "worry_vote_counted", c(1, 4), "Marginal Mean (Agreement, 4 = Strongly Agree)", "worry_vote_counted")

##### FIGURE A5 - CONCERN PUNISHMENT FOR POLVIEWS UNDER eID (FIGURE A5) 

mm_figure(cjoint_clean_r1, "worry_punished_view", c(1, 4),  "Marginal Mean (Agreement, 4 = Strongly Agree)", "worry_punished_view")

######## TABLES: MAIN EFFECTS ANALYSIS: NAIROBI DUMMY (TABLE A8) ########

main_nairobi_dom <- lapply(primary_outcomes, function(x) dom_maineffectsFun(x, cjoint_clean, "nairobi"))
main_nairobi_opp <- lapply(primary_outcomes, function(x) opp_maineffectsFun(x, cjoint_clean, "nairobi"))
main_nairobi_sec <- lapply(primary_outcomes, function(x) sec_maineffectsFun(x, cjoint_clean, "nairobi"))

main_nairobi <- c(main_nairobi_dom, main_nairobi_opp, main_nairobi_sec)

vars_order_nairobi <- c("public_service_dm", "social_prot_dm", "security_dm","tax_reg_dm", "voting_dm",
                        "dominant_dm", "public_service_dm:dominant_dm", "social_prot_dm:dominant_dm", "security_dm:dominant_dm","tax_reg_dm:dominant_dm", "voting_dm:dominant_dm",
                        "opposition_dm", "public_service_dm:opposition_dm", "social_prot_dm:opposition_dm", "security_dm:opposition_dm","tax_reg_dm:opposition_dm", "voting_dm:opposition_dm",
                        "securitized_dm", "public_service_dm:securitized_dm", "social_prot_dm:securitized_dm", "security_dm:securitized_dm","tax_reg_dm:securitized_dm", "voting_dm:securitized_dm",
                        "Constant")

stargazer(lapply(main_nairobi, '[[', 1),
          se = lapply(main_nairobi, '[[', 2),
          omit.stat = c("f", "ser"),
          omit = "nairobi_dm",
          column.labels = rep(c("Choice", "Support", "Register"),3),
          covariate.labels = coef_labs_main_new,
          add.lines = list(c("CRSE at respondent level",rep("",9)), c("Nairobi Dummy", rep("Yes",9))),
          title = "Main effects: Nairobi",
          label = "tab:main_effects_nairobi",
          type = "latex",
          out = "Writing/Tables/main_effects_nairobi.tex",
          no.space = T,
          font.size = "footnotesize",
          float = F, header=F, 
          omit.table.layout = "nld",
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001),
          order=paste0("^", vars_order_nairobi , "$"))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### 4. SUPLEMENTARY MATERIALS (ONLINE APPENDIX) ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++

##### 4.1. ADDITIONAL ROBUSTNESS CHECKS ########

###### 4.1.1 SOCIOECONOMIC CONTROLS (TABLE 01) ########

## Controlling for education, income, and device ownership
vars_order_FE <- c("public_service_dm", "social_prot_dm", "security_dm","tax_reg_dm", "voting_dm",
                   "dominant_dm", "public_service_dm:dominant_dm", "social_prot_dm:dominant_dm", "security_dm:dominant_dm","tax_reg_dm:dominant_dm", "voting_dm:dominant_dm",
                   "opposition_dm", "public_service_dm:opposition_dm", "social_prot_dm:opposition_dm", "security_dm:opposition_dm","tax_reg_dm:opposition_dm", "voting_dm:opposition_dm",
                   "securitized_dm", "public_service_dm:securitized_dm", "social_prot_dm:securitized_dm", "security_dm:securitized_dm","tax_reg_dm:securitized_dm", "voting_dm:securitized_dm",
                   "Constant")

socioeconomic_edu_controls <- c("post_hs_dm","source_income_dm", "dev_ownership_dm")
socioeconomic_edu_controls_plus <- paste0(socioeconomic_edu_controls, collapse = "+")

main_socioeconomic_edu_control_dom <- lapply(primary_outcomes, function(x) dom_maineffectsFun(x, cjoint_clean, "socioecon_edu_control"))
main_socioeconomic_edu_control_opp <- lapply(primary_outcomes, function(x) opp_maineffectsFun(x, cjoint_clean, "socioecon_edu_control"))
main_socioeconomic_edu_control_sec <- lapply(primary_outcomes, function(x) sec_maineffectsFun(x, cjoint_clean, "socioecon_edu_control"))

main_socioeconomic_edu_control<- c(main_socioeconomic_edu_control_dom, main_socioeconomic_edu_control_opp, main_socioeconomic_edu_control_sec)

stargazer(lapply(main_socioeconomic_edu_control, '[[', 1),
          se = lapply(main_socioeconomic_edu_control, '[[', 2),
          omit.stat = c("f", "ser"),
          column.labels = rep(c("Choice", "Support", "Register"),3),
          covariate.labels = coef_labs_main_new,
          add.lines = list(c("CRSE at respondent level",rep("",9)), c("Demographic Controls", rep("Yes",9))),
          type = "latex",
          out = "Writing/Tables/main_effects_socioeconomic_edu_controls.tex",
          no.space = T,
          font.size = "footnotesize",
          label = "tab:mainfe_socioedu",
          float = F, 
          header=F, 
          omit.table.layout = "nld",
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001),
          omit = socioeconomic_edu_controls,
          order=paste0("^", vars_order_FE , "$"),
          notes = c("Socio-economic variables: source of paid income, asset ownership index, post-secondary education"),
          notes.append = FALSE,
          notes.align = "l")

###### 4.1.2 ENUMERATOR FIXED EFFECTS ########
# cjoint_clean <- cjoint_clean %>%
#   mutate(one_column = 1) %>%
#   mutate(row = row_number()) %>%
#   mutate(enum_name = sprintf("%02d", enum_name)) %>%
#   pivot_wider(names_from = enum_name, names_prefix = "enum_",
#               values_from = one_column, values_fill = 0)
# 
# enum_variable_list <- names(cjoint_clean[,grepl("enum_", names(cjoint_clean_enum))])
# 
# # demean enumerator variables
# for(i in 1:length(enum_variable_list)){
#   variable <- enum_variable_list[i]
#   enum_number <- substr(variable, 6, 7)
#   vector <- data.frame(cjoint_clean_enum[,which(grepl(variable, names(cjoint_clean_enum)))])
#   colnames(vector) <- c("enum_var")
#   vector <- vector %>%
#     mutate(enum_var = ifelse(enum_var == "NULL", 0, enum_var)) %>%
#     mutate(enum_var = as.numeric(enum_var)) %>%
#     mutate(dm_var = enum_var - mean(enum_var, na.rm = T)) %>%
#     select(dm_var)
#   
#   cjoint_clean_enum <- cbind(cjoint_clean_enum, vector)
#   
#   names(cjoint_clean_enum)[names(cjoint_clean_enum) == "dm_var"] <- c(paste0("enum_dm_", enum_number))
# }

enum_dm_list <- names(cjoint_clean[,grepl("enum_dm", names(cjoint_clean))])
enum_dm_list_plus <- paste(enum_dm_list, collapse = " + ")

main_enumFE_dom <- lapply(primary_outcomes, function(x) dom_maineffectsFun(x, cjoint_clean, "enumFE"))
main_enumFE_opp <- lapply(primary_outcomes, function(x) opp_maineffectsFun(x, cjoint_clean, "enumFE"))
main_enumFE_sec <- lapply(primary_outcomes, function(x) sec_maineffectsFun(x, cjoint_clean, "enumFE"))

main_enumFE <- c(main_enumFE_dom, main_enumFE_opp, main_enumFE_sec)

stargazer(lapply(main_enumFE, '[[', 1),
          se = lapply(main_enumFE, '[[', 2),
          omit.stat = c("f", "ser"),
          column.labels = rep(c("Choice", "Support", "Register"),3),
          covariate.labels = coef_labs_main_new,
          add.lines = list(c("CRSE at respondent level",rep("",9)), c("Enumerator FE", rep("Yes",9))),
          title = "Main Effects Enumerator FE",
          label = "tab:main_effects_enumFE",
          type = "latex",
          out = "Writing/Tables/main_effects_enumFE.tex",
          no.space = T,
          font.size = "footnotesize",
          float = F, header=F, 
          omit.table.layout = "nld",
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001),
          omit = enum_dm_list,
          order=paste0("^", vars_order_FE , "$"))

###### 4.1.3 ROUND FE #######
# Main Effects

round_dm_list_plus <- "round1_dm + round2_dm + round3_dm"

main_roundFE_dom <- lapply(primary_outcomes, function(x) dom_maineffectsFun(x, cjoint_clean, "roundFE"))
main_roundFE_opp <- lapply(primary_outcomes, function(x) opp_maineffectsFun(x, cjoint_clean, "roundFE"))
main_roundFE_sec <- lapply(primary_outcomes, function(x) sec_maineffectsFun(x, cjoint_clean, "roundFE"))

main_roundFE <- c(main_roundFE_dom, main_roundFE_opp, main_roundFE_sec)

stargazer(lapply(main_roundFE, '[[', 1),
          se = lapply(main_roundFE, '[[', 2),
          omit.stat = c("f", "ser"),
          column.labels = rep(c("Choice", "Support", "Register"),3),
          covariate.labels = coef_labs_main_new,
          add.lines = list(c("CRSE at respondent level",rep("",9)), c("Round FE", rep("Yes",9))),
          label = "tab:main_effects_roundFE",
          type = "latex",
          out = "Writing/Tables/main_effects_roundFE.tex",
          no.space = T,
          font.size = "footnotesize",
          float = F, header=F, 
          omit.table.layout = "nld",
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001),
          omit = c("round1_dm", "round2_dm", "round3_dm"),
          order=paste0("^", vars_order_FE , "$"))


###### 4.1.4  DROPPING UNCLASSIFIED RESPONDENT #######

cjoint_clean_drop_others <- subset(cjoint_clean, !(dominant == 0 & opposition == 0 & securitized == 0))

cjoint_clean_drop_others <- cjoint_clean_drop_others %>%
  mutate(dominant_dm = dominant - mean(dominant, na.rm = T),
         opposition_dm = opposition - mean(opposition, na.rm = T),
         securitized_dm = securitized - mean(securitized, na.rm = T))

####### 4.1.4.1 main effects 

main_ethnic_drop_others_dom <- lapply(primary_outcomes, function(x) dom_maineffectsFun(x, cjoint_clean_drop_others, "main"))
main_ethnic_drop_others_opp <- lapply(primary_outcomes, function(x) opp_maineffectsFun(x, cjoint_clean_drop_others, "main"))
main_ethnic_drop_others_sec <- lapply(primary_outcomes, function(x) sec_maineffectsFun(x, cjoint_clean_drop_others, "main"))

main_ethnic_drop_others <- c(main_ethnic_drop_others_dom, main_ethnic_drop_others_opp, main_ethnic_drop_others_sec)

stargazer(lapply(main_ethnic_drop_others, '[[', 1),
          se = lapply(main_ethnic_drop_others, '[[', 2),
          omit.stat = c("f", "ser"),
          covariate.labels = coef_labs_main_new,
          column.labels = rep(c("Choice", "Support", "Register"), 3),
          add.lines = list("CRSE at respondent level"),
          type = "latex",
          out = "Writing/Tables/main_effects_drop_others.tex",
          no.space = T,
          font.size = "footnotesize",
          float = F, header=F, 
          omit.table.layout = "nld",
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001))


####### 4.1.4.2 mechanisms 

cjoint_clean_drop_others_r1 <- subset(cjoint_clean_drop_others, round == 1)

# drop DK and refuse
cjoint_clean_drop_others_r1$easy_access_govt_services_m <- cjoint_clean_drop_others_r1$easy_access_govt_services %in% c(-999, -998)
cjoint_clean_drop_others_r1$easy_access_govt_services[cjoint_clean_drop_others_r1$easy_access_govt_services_m] <- NA

cjoint_clean_drop_others_r1$adequate_data_privacy_m <- cjoint_clean_drop_others_r1$adequate_data_privacy %in% c(-999, -998)
cjoint_clean_drop_others_r1$adequate_data_privacy[cjoint_clean_drop_others_r1$adequate_data_privacy_m] <- NA

cjoint_clean_drop_others_r1$worry_punished_view_m <- cjoint_clean_drop_others_r1$worry_punished_view %in% c(-999, -998)
cjoint_clean_drop_others_r1$worry_punished_view[cjoint_clean_drop_others_r1$worry_punished_view_m] <- NA

cjoint_clean_drop_others_r1$worry_vote_counted_m <- cjoint_clean_drop_others_r1$worry_vote_counted %in% c(-999, -998)
cjoint_clean_drop_others_r1$worry_vote_counted[cjoint_clean_drop_others_r1$worry_vote_counted_m] <- NA

cjoint_clean_drop_others_r1$worry_personal_info_m <- cjoint_clean_drop_others_r1$worry_personal_info %in% c(-999, -998)
cjoint_clean_drop_others_r1$worry_personal_info[cjoint_clean_drop_others_r1$worry_personal_info_m] <- NA

######## 4.1.4.2a Mechanism: dominant 

secondary_drop_others_dom <- lapply(secondary_outcomes, function(x) dom_maineffectsFun(x, cjoint_clean_drop_others_r1 , "main"))

stargazer(lapply(secondary_drop_others_dom, '[[', 1),
          se = lapply(secondary_drop_others_dom, '[[', 2),
          omit.stat = c("f", "ser"),
          column.labels = c("Access Services", "Data Privacy", "Worry", "Worry", "Worry \\\\ & & & Punished View & Vote Counted & Personal Info"),
          covariate.labels = coef_labs_main_dom,
          add.lines = list("CRSE at respondent level"),
          type = "latex",
          out = "Writing/Tables/secondary_drop_others_dom.tex",
          no.space = T,
          font.size = "footnotesize",
          float = F, header=F, 
          omit.table.layout = "nld",
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001))


######## 4.1.4.2b Mechanisms: opposition 

secondary_drop_others_opp <- lapply(secondary_outcomes, function(x) opp_maineffectsFun(x, cjoint_clean_drop_others_r1, "main"))

stargazer(lapply(secondary_drop_others_opp, '[[', 1),
          se = lapply(secondary_drop_others_opp, '[[', 2),
          omit.stat = c("f", "ser"),
          column.labels = c("Access Services", "Data Privacy", "Worry", "Worry", "Worry \\\\ & & & Punished View & Vote Counted & Personal Info"),
          covariate.labels = coef_labs_main_opp,
          add.lines = list("CRSE at respondent level"),
          type = "latex",
          out = "Writing/Tables/secondary_drop_others_opp.tex",
          no.space = T,
          font.size = "footnotesize",
          float = F, header=F, 
          omit.table.layout = "nld",
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001))

######## 4.1.4.2c Mechanisms securitized 

secondary_drop_others_sec <- lapply(secondary_outcomes, function(x) sec_maineffectsFun(x, cjoint_clean_drop_others_r1 , "main"))

stargazer(lapply(secondary_drop_others_sec, '[[', 1),
          se = lapply(secondary_drop_others_sec, '[[', 2),
          omit.stat = c("f", "ser"),
          column.labels = c("Access Services", "Data Privacy", "Worry", "Worry", "Worry \\\\ & & & Punished View & Vote Counted & Personal Info"),
          covariate.labels = coef_labs_main_sec,
          add.lines = list("CRSE at respondent level"),
          type = "latex",
          out = "Writing/Tables/secondary_drop_others_sec.tex",
          no.space = T,
          font.size = "footnotesize",
          float = F, header=F, 
          omit.table.layout = "nld",
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001))


###### 4.1.5 COMPARISON TO DOMINANT GROUP  ######

#######  4.1.5.1 main effects 

dominant_comparisonFun <- function(outcome, data){
  data <- data
  f <- paste0(outcome, "~ (public_service_dm + social_prot_dm + security_dm + tax_reg_dm + voting_dm)*(opposition_dm + securitized_dm)")
  mod.main <- lm(as.formula(f), data = data)
  mod.main.c <- coeftest(mod.main, vcov = vcovCL(mod.main, cluster = data$instanceID))
  return(list(mod.main = mod.main,
              se = mod.main.c[, 2]))
}

dominant_comparison <- lapply(primary_outcomes, function(x) dominant_comparisonFun(x,subset(cjoint_clean, dominant ==1|opposition==1|securitized==1)))

coef_labs_dom_comparison <- c("Public Service",
                              "Social Protection",
                              "Surveillance",
                              "Tax Registration",
                              "Voting",
                              "Opposition",
                              "Pub. Service x Opp.",
                              "Soc. Prot. x Opp.",
                              "Surveillance x Opp.",
                              "Tax Reg. x Opp.",
                              "Voting x Opp.",
                              "Securitized",
                              "Pub. Service x Sec.",
                              "Soc. Prot. x Sec.",
                              "Surveillance x Sec.",
                              "Tax Reg. x Sec.",
                              "Voting x Sec.",
                              "Constant")

stargazer(lapply(dominant_comparison, '[[', 1),
          se = lapply(dominant_comparison, '[[', 2),
          omit.stat = c("f", "ser"),
          column.labels = c("Choice", "Support", "Register"),
          covariate.labels = coef_labs_dom_comparison,
          add.lines = list("CRSE at respondent level"),
          type = "latex",
          out = "Writing/Tables/comparison_dominant_main.tex",
          no.space = T,
          font.size = "footnotesize",
          float = F, header=F, 
          omit.table.layout = "nld",
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001),
          order= c(1,2,3,4,5,6,8,10,12,14,16,7,9,11,13,15,17,18))


#######  4.1.5.2 mechanisms 

mech_dominant_comparison <- lapply(secondary_outcomes, function(x) dominant_comparisonFun(x,subset(cjoint_clean_r1, dominant ==1|opposition==1|securitized==1)))

stargazer(lapply(mech_dominant_comparison, '[[', 1),
          se = lapply(mech_dominant_comparison, '[[', 2),
          omit.stat = c("f", "ser"),
          column.labels = c("Access Services", "Data Privacy", "Worry", "Worry", "Worry \\\\ & & & Punished View & Vote Counted & Personal Info"),
          covariate.labels = coef_labs_dom_comparison,
          add.lines = list("CRSE at respondent level"),
          type = "latex",
          out = "Writing/Tables/comparison_dominant_mechanisms.tex",
          no.space = T,
          font.size = "footnotesize",
          float = F, header=F, 
          omit.table.layout = "nld",
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001),
          order= c(1,2,3,4,5,6,8,10,12,14,16,7,9,11,13,15,17,18))


###### 4.1.6 RESULTS FIRST ROUND ###### 

main_ethnic_d_opp_r1 <- lapply(primary_outcomes, function(x) opp_maineffectsFun(x, cjoint_clean_r1, "main"))
main_ethnic_d_sec_r1 <- lapply(primary_outcomes, function(x) sec_maineffectsFun(x, cjoint_clean_r1 , "main"))
main_ethnic_d_dom_r1 <- lapply(primary_outcomes, function(x) dom_maineffectsFun(x, cjoint_clean_r1 , "main"))

main_ethnic_new_r1 <- c(main_ethnic_d_dom_r1, main_ethnic_d_opp_r1, main_ethnic_d_sec_r1)

stargazer(lapply(main_ethnic_new_r1, '[[', 1),
          se = lapply(main_ethnic_new_r1, '[[', 2),
          omit.stat = c("f", "ser"),
          covariate.labels = coef_labs_main_new,
          column.labels = rep(c("Choice", "Support", "Register"), 3),
          add.lines = list("CRSE at respondent level"),
          type = "latex",
          out = "Writing/Tables/main_effects_new_r1.tex",
          no.space = T,
          font.size = "footnotesize",
          float = F, header=F, 
          omit.table.layout = "nld",
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001)
)


###### 4.1.7 Multiple hypothesis testing corections ###### 

# Run above

## subsets
### dominant
### opposition
### securitized

## models for main effects analysis
### main_ethnic_dom
### main_ethnic_opp
### main_ethnic_sec

## models for mechanisms analysis
### main_secondary_dom
### main_secondary_opp
### main_secondary_sec

# Run pre-registered tradeoffs analysis
tradeoffsFun <- function(outcome, data){
  data <- data
  f <- paste(outcome, "~ (public_service_dm + social_prot_dm + security_dm + tax_reg_dm + voting_dm)^2") %>%
    as.formula()
  mod.x <- lm(f, data = data)
  mod.x.c <- coeftest(mod.x, vcov = vcovCL(mod.x, cluster = ~instanceID))
  return(list(mod.x = mod.x,
              se = mod.x.c[, 2],
              pval = mod.x.c[, 4]
  ))
}

tradeoffs_dominant <- lapply(primary_outcomes, function(x) tradeoffsFun(x, dominant))
tradeoffs_opposition <- lapply(primary_outcomes, function(x) tradeoffsFun(x, opposition))
tradeoffs_securitized <- lapply(primary_outcomes, function(x) tradeoffsFun(x, securitized))

# Apply FDR correction across all pre-registered hypotheses, by outcome

# 13 hypotheses total (including pre-registered tradeoffs hypotheses)

# choice outcome
preregistered_choice_raw <- c(sapply(main_ethnic_dom,'[[', 3)[c(2:6, 8, 9), 1], #h1-h5, h6a, h6b
                              sapply(main_ethnic_sec, '[[', 3)[10, 1], #h7
                              sapply(main_ethnic_opp, '[[', 3)[12, 1], #h8
                              sapply(tradeoffs_securitized, '[[', 3)[c(8, 11), 1], # tradeoffs 1
                              sapply(tradeoffs_opposition, '[[', 3)[c(10, 13), 1]) # tradeoffs 2

# support outcome
preregistered_support_raw <- c(sapply(main_ethnic_dom,'[[', 3)[c(2:6, 8, 9), 2], #h1-h5, h6a, h6b
                               sapply(main_ethnic_sec, '[[', 3)[10, 2], #h7
                               sapply(main_ethnic_opp, '[[', 3)[12, 2], #h8
                               sapply(tradeoffs_securitized, '[[', 3)[c(8, 11), 2], # tradeoffs 1a, 1b
                               sapply(tradeoffs_opposition, '[[', 3)[c(10, 13), 2]) # tradeoffs 2a, 2b


# register outcome
preregistered_register_raw <- c(sapply(main_ethnic_dom,'[[', 3)[c(2:6, 8, 9), 3], #h1-h5, h6a, h6b
                                sapply(main_ethnic_sec, '[[', 3)[10, 3], #h7
                                sapply(main_ethnic_opp, '[[', 3)[12, 3], #h8
                                sapply(tradeoffs_securitized, '[[', 3)[c(8, 11), 3], # tradeoffs 1a, 1b
                                sapply(tradeoffs_opposition, '[[', 3)[c(10, 13), 3]) # tradeoffs 2a, 2b


# adjusted p-values
all_raw <- c(preregistered_choice_raw,
             preregistered_support_raw,
             preregistered_register_raw)
all_adjusted_bh <- p.adjust(all_raw, method = "BH")

coef_names <- c("Public Service",
                "Social Protection",
                "Security",
                "Tax Registration",
                "Voting",
                "Pub. Service x Dominant",
                "Soc. Prot. x Dominant",
                "Security x Securitized",
                "Voting x Opposition",
                "Pub. Service x Security - Sec",
                "Soc. Prot. x Security - Sec",
                "Pub. Service x Voting - Opp",
                "Soc. Prot. x Security - Opp"
)
h_names <- c("H1", "H2", "H3", "H4", "H5", "H6A", "H6B", "H7", "H8", "TH1A", "TH1B", "TH2A", "TH2B")
outcomes <- c("Choice", "Support", "Register")

mht_adjusted <- data.frame(Hypothesis = rep(h_names, 3),
                           Outcomes = rep(outcomes, each = length(h_names)),
                           Coefficient = rep(coef_names, 3),
                           Raw = all_raw,
                           Adjusted_FDR = all_adjusted_bh) %>%
  mutate(Change_FDR = ifelse(Raw < 0.05 & Adjusted_FDR > 0.05, "Y", "N")) %>%
  filter(!Hypothesis %in% c("TH1A", "TH1B", "TH2A", "TH2B")) # remove tradeoffs hypotheses from table

# Apply FWER correction (Bonferroni) across all three outcomes for each hypothesis

sapply(main_ethnic_dom,'[[', 3)[c(2:6, 8, 9), ]
sapply(main_ethnic_sec, '[[', 3)[10, ]
sapply(main_ethnic_opp, '[[', 3)[12, ]

raw_main <- data.frame(rbind(sapply(main_ethnic_dom,'[[', 3)[c(2:6, 8, 9), ],
                             sapply(main_ethnic_sec, '[[', 3)[10, ],
                             sapply(main_ethnic_opp, '[[', 3)[12, ]
))
colnames(raw_main) <- outcomes
rownames(raw_main) <- h_names[1:9]

bf_main <- apply(raw_main, 1, function(x) p.adjust(x, method = "bonferroni"))
adjusted_fwer <- c(bf_main[1, ],
                   bf_main[2, ],
                   bf_main[3, ]
)

# Add to adjusted dataframe
mht_adjusted$Adjusted_FWER <- adjusted_fwer
mht_adjusted <- mht_adjusted %>%
  mutate(Change_FWER = ifelse(Raw < 0.05 & Adjusted_FWER > 0.05, "Y", "N"))

# Create table
print(xtable(mht_adjusted), include.rownames = F, type = "latex", file = "./Writing/Tables/mht_corrections.tex")

# Apply FDR correction across all secondary outcome coefficients

secondary_pvals_dom <- data.frame(sapply(main_secondary_dom, '[[', 3))
colnames(secondary_pvals_dom) <- secondary_outcomes
secondary_pvals_dom$coef <- row.names(secondary_pvals_dom)
secondary_pvals_dom <- secondary_pvals_dom %>% pivot_longer(cols = secondary_outcomes,
                                                            names_to = 'outcome',
                                                            values_to = 'pval'
) %>%
  arrange(outcome)


secondary_pvals_opp <- data.frame(sapply(main_secondary_opp, '[[', 3))[-c(1:6), ]
colnames(secondary_pvals_opp) <- secondary_outcomes
secondary_pvals_opp$coef <- row.names(secondary_pvals_opp)
secondary_pvals_opp <- secondary_pvals_opp %>% pivot_longer(cols = secondary_outcomes,
                                                            names_to = 'outcome',
                                                            values_to = 'pval') %>%
  arrange(outcome)


secondary_pvals_sec <- data.frame(sapply(main_secondary_sec, '[[', 3))[-c(1:6), ]
colnames(secondary_pvals_sec) <- secondary_outcomes
secondary_pvals_sec$coef <- row.names(secondary_pvals_sec)
secondary_pvals_sec <- secondary_pvals_sec %>% pivot_longer(cols = secondary_outcomes,
                                                            names_to = 'outcome',
                                                            values_to = 'pval') %>%
  arrange(outcome)

secondary_pvals_all <- rbind(secondary_pvals_dom,
                             secondary_pvals_opp,
                             secondary_pvals_sec) %>%
  arrange(outcome)
secondary_pvals_all <- secondary_pvals_all[-which(secondary_pvals_all$coef == "(Intercept)"), ] # remove intercept

pval_fdr <- p.adjust(secondary_pvals_all$pval, method = "BH")

coef_names <- c("Public Service",
                "Social Protection",
                "Security",
                "Tax Registration",
                "Voting",
                "Dominant",
                "Pub. Service x Dominant",
                "Soc. Prot. x Dominant",
                "Security x Dominant",
                "Tax Registration x Dominant",
                "Voting x Dominant",
                "Opposition",
                "Pub. Service x Opposition",
                "Soc. Prot. x Opposition",
                "Security x Opposition",
                "Tax Registration x Opposition",
                "Voting x Opposition",
                "Securitized",
                "Pub. Service x Securitized",
                "Soc. Prot. x Securitized",
                "Security x Securitized",
                "Tax Registration x Securitized",
                "Voting x Securitized")

secondary_outcomes_names <- c(
  "Data Privacy",
  "Access Services",
  "Worry Personal Info",
  "Worry Punished View",
  "Worry Vote Counted"
)    

mht_adjusted_secondary <- data.frame(Outcomes = rep(secondary_outcomes_names, each = length(coef_names)),
                                     Coefficient = rep(coef_names, length(secondary_outcomes)),
                                     Raw = secondary_pvals_all$pval,
                                     Adjusted_FDR = pval_fdr
) %>%
  mutate(Change_FDR = ifelse(Raw < 0.05 & Adjusted_FDR > 0.05, "Y", "N"))

# Split up into separate tables for each outcome

# Data Privacy
mht_adjusted_secondary_data_privacy <- mht_adjusted_secondary %>%
  filter(Outcomes == "Data Privacy") %>%
  select(!Outcomes)
# Create table
print(xtable(mht_adjusted_secondary_data_privacy), include.rownames = F, type = "latex", file = "./Writing/Tables/mht_corrections_secondary_data_privacy.tex")

# Access Services
mht_adjusted_secondary_access_services <- mht_adjusted_secondary %>%
  filter(Outcomes == "Access Services") %>%
  select(!Outcomes)
# Create table
print(xtable(mht_adjusted_secondary_access_services), include.rownames = F, type = "latex", file = "./Writing/Tables/mht_corrections_secondary_access_services.tex")

# Worry Personal Info
mht_adjusted_secondary_worry_info <- mht_adjusted_secondary %>%
  filter(Outcomes == "Worry Personal Info") %>%
  select(!Outcomes)
# Create table
print(xtable(mht_adjusted_secondary_worry_info), include.rownames = F, type = "latex", file = "./Writing/Tables/mht_corrections_secondary_worry_info.tex")

# Worry Punished View
mht_adjusted_secondary_punished_view <- mht_adjusted_secondary %>%
  filter(Outcomes == "Worry Punished View") %>%
  select(!Outcomes)
# Create table
print(xtable(mht_adjusted_secondary_punished_view), include.rownames = F, type = "latex", file = "./Writing/Tables/mht_corrections_secondary_punished_view.tex")

# Worry Vote Counted
mht_adjusted_secondary_vote_counted <- mht_adjusted_secondary %>%
  filter(Outcomes == "Worry Vote Counted") %>%
  select(!Outcomes)
# Create table
print(xtable(mht_adjusted_secondary_vote_counted), include.rownames = F, type = "latex", file = "./Writing/Tables/mht_corrections_secondary_vote_counted.tex")

###### 4.1.8 EPR Classification ######
# (opposition as powerless, senior partners as dominant, junior partners as others)

EPR_classification <- cjoint_clean %>%
  mutate(dominant = ifelse(mother_tongue == "3" | mother_tongue == "4" | mother_tongue == "11" |
                             mother_tongue == "13" | mother_tongue == "15", 1, 0)) %>%
  mutate(opposition = ifelse(mother_tongue == "2" | mother_tongue == "7" | mother_tongue == "12",
                             1, 0)) %>%
  mutate(securitized = ifelse(mother_tongue == "5", 1, 0)) %>%
  mutate(dominant_dm = dominant - mean(dominant, na.rm = T)) %>%
  mutate(opposition_dm = opposition - mean(opposition, na.rm = T)) %>%
  mutate(securitized_dm = securitized - mean(securitized, na.rm = T))

coef_labs_main_EPR <- c("Public Service",
                        "Social Protection",
                        "Surveillance",
                        "Tax Registration",
                        "Voting",
                        "Senior Partner",
                        "Pub. Service x Sen.",
                        "Soc. Prot. x Sen.",
                        "Surveillance x Sen.",
                        "Tax Reg. x Sen.",
                        "Voting x Sen.",
                        "Powerless",
                        "Pub. Service x Pow.",
                        "Soc. Prot. x Pow.",
                        "Surveillance x Pow.",
                        "Tax Reg. x Pow.",
                        "Voting x Pow.",
                        "Discriminated",
                        "Pub. Service x Dis.",
                        "Soc. Prot. x Dis.",
                        "Surveillance x Dis.",
                        "Tax Reg. x Dis.",
                        "Voting x Dis.",
                        "Constant")

coef_labs_main_opp_EPR <- c("Public Service",
                            "Social Protection",
                            "Surveillance",
                            "Tax Registration",
                            "Voting",
                            "Powerless",
                            "Pub. Service x Pow.",
                            "Soc. Prot. x Pow.",
                            "Surveillance x Pow.",
                            "Tax Reg. x Pow.",
                            "Voting x Pow.",
                            "Constant")

coef_labs_main_sec_EPR <- c("Public Service",
                            "Social Protection",
                            "Surveillance",
                            "Tax Registration",
                            "Voting",
                            "Discriminated",
                            "Pub. Service x Dis.",
                            "Soc. Prot. x Dis.",
                            "Surveillance x Dis.",
                            "Tax Reg. x Dis.",
                            "Voting x Dis.",
                            "Constant")

coef_labs_main_dom_EPR <- c("Public Service",
                            "Social Protection",
                            "Surveillance",
                            "Tax Registration",
                            "Voting",
                            "Senior Partner",
                            "Pub. Service x Sen.",
                            "Soc. Prot. x Sen.",
                            "Surveillance x Sen.",
                            "Tax Reg. x Sen.",
                            "Voting x Sen.",
                            "Constant")

####### 4.1.8.1 main effects 

main_ethnic_EPR_dom <- lapply(primary_outcomes, function(x) dom_maineffectsFun(x, EPR_classification , "main"))
main_ethnic_EPR_opp <- lapply(primary_outcomes, function(x) opp_maineffectsFun(x, EPR_classification , "main"))
main_ethnic_EPR_sec <- lapply(primary_outcomes, function(x) sec_maineffectsFun(x, EPR_classification , "main"))


main_ethnic_EPR <- c(main_ethnic_EPR_dom, main_ethnic_EPR_opp, main_ethnic_EPR_sec)

stargazer(lapply(main_ethnic_EPR, '[[', 1),
          se = lapply(main_ethnic_EPR, '[[', 2),
          omit.stat = c("f", "ser"),
          covariate.labels = coef_labs_main_EPR,
          column.labels = rep(c("Choice", "Support", "Register"), 3),
          add.lines = list("CRSE at respondent level"),
          type = "latex",
          out = "Writing/Tables/main_effects_EPR_classification.tex",
          no.space = T,
          font.size = "footnotesize",
          float = F, header=F, 
          omit.table.layout = "nld",
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001))


####### 4.1.8.2 Mechanisms 

EPR_r1 <- EPR_classification[EPR_classification$round == 1, ]

EPR_r1$easy_access_govt_services_m <- EPR_r1$easy_access_govt_services %in% c(-999, -998)
EPR_r1$easy_access_govt_services[EPR_r1$easy_access_govt_services_m] <- NA

EPR_r1$adequate_data_privacy_m <- EPR_r1$adequate_data_privacy %in% c(-999, -998)
EPR_r1$adequate_data_privacy[EPR_r1$adequate_data_privacy_m] <- NA

EPR_r1$worry_punished_view_m <- EPR_r1$worry_punished_view %in% c(-999, -998)
EPR_r1$worry_punished_view[EPR_r1$worry_punished_view_m] <- NA

EPR_r1$worry_vote_counted_m <- EPR_r1$worry_vote_counted %in% c(-999, -998)
EPR_r1$worry_vote_counted[EPR_r1$worry_vote_counted_m] <- NA

EPR_r1$worry_personal_info_m <- EPR_r1$worry_personal_info %in% c(-999, -998)
EPR_r1$worry_personal_info[EPR_r1$worry_personal_info_m] <- NA


####### 4.1.8.2a Mechanisms: Dominant 

secondary_EPR_dom <- lapply(secondary_outcomes, function(x) dom_maineffectsFun(x, EPR_r1 , "main"))

stargazer(lapply(secondary_EPR_dom, '[[', 1),
          se = lapply(secondary_EPR_dom, '[[', 2),
          omit.stat = c("f", "ser"),
          column.labels = c("Access Services", "Data Privacy", "Worry", "Worry", "Worry \\\\ & & & Punished View & Vote Counted & Personal Info"),
          covariate.labels = coef_labs_main_dom_EPR,
          add.lines = list("CRSE at respondent level"),
          type = "latex",
          out = "Writing/Tables/secondary_EPR_classification_dom.tex",
          no.space = T,
          font.size = "footnotesize",
          float = F, header=F, 
          omit.table.layout = "nld",
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001))


####### 4.1.8.3 Mechanisms: Opposition

secondary_EPR_opp <- lapply(secondary_outcomes, function(x) opp_maineffectsFun(x, EPR_r1 , "main"))

stargazer(lapply(secondary_EPR_opp, '[[', 1),
          se = lapply(secondary_EPR_opp, '[[', 2),
          omit.stat = c("f", "ser"),
          column.labels = c("Access Services", "Data Privacy", "Worry", "Worry", "Worry \\\\ & & & Punished View & Vote Counted & Personal Info"),
          covariate.labels = coef_labs_main_opp_EPR,
          add.lines = list("CRSE at respondent level"),
          type = "latex",
          out = "Writing/Tables/secondary_EPR_classification_opp.tex",
          no.space = T,
          font.size = "footnotesize",
          float = F, header=F, 
          omit.table.layout = "nld",
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001))

####### 4.1.8.4 Mechanisms: Securitized 

secondary_EPR_sec <- lapply(secondary_outcomes, function(x) sec_maineffectsFun(x, EPR_r1 , "main"))

stargazer(lapply(secondary_EPR_sec, '[[', 1),
          se = lapply(secondary_EPR_sec, '[[', 2),
          omit.stat = c("f", "ser"),
          column.labels = c("Access Services", "Data Privacy", "Worry", "Worry", "Worry \\\\ & & & Punished View & Vote Counted & Personal Info"),
          covariate.labels = coef_labs_main_sec_EPR,
          add.lines = list("CRSE at respondent level"),
          type = "latex",
          out = "Writing/Tables/secondary_EPR_classification_sec.tex",
          no.space = T,
          font.size = "footnotesize",
          float = F, header=F, 
          omit.table.layout = "nld",
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001))

##### 4.2. THERMOMETER QUESTIONS (Q45 - Q49) - eID ATTRIBUTES #####

therm_questions_plot <- function(q_number, title, variable) {
  
  # Plot for subgroups
  plot_subgroups <- ggplot(subset(d, d[[variable]] >= 0 & !is.na(d$Subgroup)),
                           aes(x = !!sym(variable), color = Subgroup)) +
    geom_freqpoly(aes(x = !!sym(variable), color = Subgroup), binwidth = 1, linetype = 1) +
    scale_color_manual(values = c("darkgreen", "blue", "red")) +
    scale_x_continuous(limits = c(1, 10), breaks = c(1:10)) +
    theme_minimal() +
    labs(title = paste(q_number, ": ", title),
         x = "Feeling toward Digital ID policy \n from 1 (very negative) to 10 (very positive)", y = "Count")
  
  # Plot for pooled histogram
  plot_hist <- ggplot(subset(d, d[[variable]] >= 0), aes(x = !!sym(variable), y = ..count..)) +
    geom_histogram(binwidth = 1, fill = "lightgrey", color = "black", linetype = 1) +
    scale_x_continuous(breaks = c(1:10)) +
    theme_minimal() +
    labs(title = paste(q_number, ": ", title),
         x = "Feeling toward Digital ID policy \n from 1 (very negative) to 10 (very positive)", y = "Count")
  
  # Save plots
  ggsave(plot_subgroups, filename = paste("Writing/Figures/q", q_number, "_subgroups.pdf", sep = ""),
         width = 6, height = 5)
  
  ggsave(plot_hist, filename = paste("Writing/Figures/q", q_number, "_hist.pdf", sep = ""),
         width = 6, height = 5)
}

###### Q45: Government Transfers 

therm_questions_plot("45", "Government Transfers", "q_55")

###### Q46: Public Services

therm_questions_plot("46", "Public Services", "q_56")

###### Q47: Data Sharing across Government Agencies

therm_questions_plot("47", "Data Sharing across Government Agencies", "q_57")

###### Q48: Tax Registration

therm_questions_plot("48", "Tax Registration", "q_58")

###### 4.2.5 Q49: Voter Registration 

therm_questions_plot("49", "Voter Registration", "q_59")

##### 4.3. MANIPULATION CHECKS (MC) #####

mpcheck <- cjoint_clean 
mpcheck$connnected_voter_reg <- as.numeric(mpcheck$connnected_voter_reg)
mpcheck$take_biometric_photo <- as.numeric(mpcheck$take_biometric_photo)
mpcheck$voting <- as.numeric(mpcheck$voting)
mpcheck$security <- as.numeric(mpcheck$security)

# code whether respondent passes manipulation check

mpcheck$voting_correct <- ifelse((mpcheck$voting == 1 & mpcheck$connnected_voter_reg == 1 
                                  & mpcheck$round == "1") |
                                   (mpcheck$voting == 0 & mpcheck$connnected_voter_reg == 2 
                                    & mpcheck$round == "1"),1,
                                 ifelse((mpcheck$voting == 1 & mpcheck$connnected_voter_reg == 2 
                                         & mpcheck$round == "1") |
                                          (mpcheck$voting == 0 & mpcheck$connnected_voter_reg == 1 
                                           & mpcheck$round == "1"),0,NA))

mpcheck$security_correct <- ifelse((mpcheck$security == 1 & mpcheck$round == "2" &
                                      mpcheck$take_biometric_photo == 1) |
                                     (mpcheck$security == 0 &  mpcheck$round == "2" &
                                        mpcheck$take_biometric_photo == 2),1,
                                   ifelse((mpcheck$security == 1 &  mpcheck$round == "2" &
                                             mpcheck$take_biometric_photo == 2) |
                                            (mpcheck$security == 0 &  mpcheck$round == "2" &
                                               mpcheck$take_biometric_photo == 1),0,NA))

# delete all policy profiles for individuals who did not pass the manipulation check in the first round
mpcheck <- mpcheck %>%
  group_by(instanceID, round)%>%
  mutate(voting_correct_all = ifelse(any(voting_correct == 0),0,1),
         security_correct_all = ifelse(any(security_correct == 0),0,1))

mpcheck <- mpcheck %>%
  group_by(instanceID)%>%
  mutate(correct_both = ifelse(any(na.omit(security_correct_all == 0)) | 
                                 any(na.omit(voting_correct_all ==0)),0,1))


# aggregate to respondent level
mpcheck_sub <- mpcheck %>% 
  select(instanceID, securitized,dominant,opposition, correct_both, county)%>%
  na.omit()%>%
  unique()%>%
  mutate(group = ifelse(securitized==1,"securitzed",
                        ifelse(dominant==1,"dominant",
                               ifelse(opposition==1,"opposition","other"))))%>%
  group_by(group)%>%
  mutate(number = length(group))


mpcheck_fail_both <- subset(mpcheck_sub, mpcheck_sub$correct_both == 0)

# plot manipulation check fails across groups
mpcheck_fail_both <- mpcheck_fail_both %>% select(instanceID, group, number)%>%
  unique()%>%
  group_by(group)%>%
  mutate(number_fail = length(group))%>%
  select(group, number, number_fail)%>%
  unique()%>%
  summarise(percent = number_fail/number)

###### 4.3.1 OVERVIEW ######

plot_mp_fail <- ggplot(data=mpcheck_fail_both, aes(x=group, y=percent*100, fill=group)) +
  geom_bar(stat="identity")+
  theme_bw()+
  scale_fill_flatui()+
  ylab("Percent of failed manipulation checks (1st or 2nd round)")
plot_mp_fail

# plot manipulation check fails across counties
# aggregate to respondent level
mpcheck_sub_c <- mpcheck %>% 
  select(instanceID, correct_both, county)%>%
  na.omit()%>%
  unique()%>%
  group_by(county)%>%
  mutate(number = length(county))

mpcheck_fail_both_c <- subset(mpcheck_sub_c, mpcheck_sub_c$correct_both == 0)

# plot manipulation check fails across groups

mpcheck_fail_both_c <- mpcheck_fail_both_c %>% select(instanceID, county, number)%>%
  unique()%>%
  group_by(county)%>%
  mutate(number_fail = length(county))%>%
  select(county, number, number_fail)%>%
  unique()%>%
  summarise(percent = number_fail/number)

# regions and ethnicity
mpcheck_fail_both_c$region[mpcheck_fail_both_c$county == 1] <-	"Nairobi"
mpcheck_fail_both_c$region[mpcheck_fail_both_c$county == 2] <-	"Kirinyaga"
mpcheck_fail_both_c$region[mpcheck_fail_both_c$county == 3] <-	"Murang'a"
mpcheck_fail_both_c$region[mpcheck_fail_both_c$county == 4] <-	"Nyandarua"
mpcheck_fail_both_c$region[mpcheck_fail_both_c$county == 5] <-	"Nyeri"
mpcheck_fail_both_c$region[mpcheck_fail_both_c$county == 6] <-	"Homa Bay"
mpcheck_fail_both_c$region[mpcheck_fail_both_c$county == 7] <-	"Kisumu"
mpcheck_fail_both_c$region[mpcheck_fail_both_c$county == 8] <-	"Migori"
mpcheck_fail_both_c$region[mpcheck_fail_both_c$county == 9] <-	"Siaya"
mpcheck_fail_both_c$region[mpcheck_fail_both_c$county == 10] <-	"Garissa"


plot_mp_fail_region <- ggplot(data=mpcheck_fail_both_c, aes(x=region, y=percent*100, fill=region)) +
  geom_bar(stat="identity")+
  theme_bw()+
  scale_fill_flatui()+
  ylab("Percent of failed manipulation checks (1st or 2nd round)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
plot_mp_fail_region

joint_mp_plot <- ggarrange(plot_mp_fail, plot_mp_fail_region, ncol = 1)

ggsave(joint_mp_plot,filename = "Writing/Figures/failed_mp_joint_plot.pdf",
       width = 6.5, height = 10)


###### 4.3.2 MAIN EFFECTS: excluding Failed MC from Round 1 ######

mpcheck <- mpcheck %>% group_by(instanceID) %>%
  mutate(pass_r1 = ifelse(any(voting_correct_all) == 1,1,0))
mpcheck_pass_r1 <-  mpcheck %>% filter(pass_r1 == 1)
mpcheck_pass_r1 <- subset(mpcheck_pass_r1, mpcheck_pass_r1$round != 1)

main_ethnic_d_dom_mp <- lapply(primary_outcomes, function(x) dom_maineffectsFun(x, mpcheck_pass_r1 , "main"))
main_ethnic_d_opp_mp <- lapply(primary_outcomes, function(x) opp_maineffectsFun(x, mpcheck_pass_r1 , "main"))
main_ethnic_d_sec_mp <- lapply(primary_outcomes, function(x) sec_maineffectsFun(x, mpcheck_pass_r1 , "main"))

main_ethnic_new_mp <- c(main_ethnic_d_dom_mp, main_ethnic_d_opp_mp, main_ethnic_d_sec_mp)

stargazer(lapply(main_ethnic_new_mp, '[[', 1),
          se = lapply(main_ethnic_new_mp, '[[', 2),
          omit.stat = c("f", "ser"),
          covariate.labels = coef_labs_main_new,
          column.labels = rep(c("Choice", "Support", "Register"), 3),
          type = "latex",
          out = "Writing/Tables/main_effects_mp_r1.tex",
          add.lines = list("CRSE at respondent level"),
          no.space = T,
          font.size = "footnotesize",
          float = F, header=F, 
          omit.table.layout = "nld",
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001))


###### 4.3.3 MAIN EFFECTS: excluding Failed MC from Round 1 & 2

mpcheck_pass_r2 <- mpcheck %>% filter(correct_both == 1)
mpcheck_pass_r2 <- subset(mpcheck_pass_r2, mpcheck_pass_r2$round == 3)

main_ethnic_d_dom_mp2 <- lapply(primary_outcomes, function(x) dom_maineffectsFun(x, mpcheck_pass_r2 , "main"))
main_ethnic_d_opp_mp2 <- lapply(primary_outcomes, function(x) opp_maineffectsFun(x, mpcheck_pass_r2 , "main"))
main_ethnic_d_sec_mp2 <- lapply(primary_outcomes, function(x) sec_maineffectsFun(x, mpcheck_pass_r2 , "main"))

main_ethnic_new_mp2 <- c(main_ethnic_d_dom_mp2, main_ethnic_d_opp_mp2, main_ethnic_d_sec_mp2)

stargazer(lapply(main_ethnic_new_mp2, '[[', 1),
          se = lapply(main_ethnic_new_mp2, '[[', 2),
          omit.stat = c("f", "ser"),
          covariate.labels = coef_labs_main_new,
          column.labels = rep(c("Choice", "Support", "Register"), 3),
          type = "latex",
          out = "Writing/Tables/main_effects_mp_r2.tex",
          no.space = T,
          font.size = "footnotesize",
          float = F, header=F, 
          omit.table.layout = "nld",
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001))

##### 4.4. MARGINAL MEANS ##### 

outcome_means <- cjoint_clean %>%
  group_by(group) %>%
  summarize(likely_register_digital_id = mean(likely_register_digital_id, na.rm = T),
            digital_id_supportive = mean(digital_id_supportive, na.rm = T)) %>%
  ungroup() %>%
  mutate(BY = group)

# choice probability
mms <- cj(cjoint_clean, 
          choice_final ~ public_service_f + social_prot_f + security_f + tax_reg_f + voting_f,
          id = ~instanceID,
          estimate = "mm",
          by = ~group)

plot(mms, xlim = c(0.3, 0.7), xlab = "Marginal Mean (Choice Probability)") +
  ggplot2::facet_wrap(~BY, ncol = 2) +
  ggplot2::geom_vline(xintercept = 0.5, linewidth = 0.25)
ggsave("Writing/Figures/mm_groups_facet_choice_final.pdf", width = 9, height = 6)


# register
mm_figure(cjoint_clean, "likely_register_digital_id", c(1, 4), "Marginal Mean (Likelihood of Registering, 4 = Very Likely)", "likely_register_digital_id")

# support
mm_figure(cjoint_clean, "digital_id_supportive", c(1, 4), "Marginal Mean (Support for eID Program, 4 = Very Supportive)", "digital_id_supportive")


##### 4.5.HETEROGENEOUS EFFECTS ANALYSES#####

###### 4.5.1 VARIABLE IMPORTANCE METRICS (WHOLE SAMPLE) ###### 

cj_model <- cjbart(cjoint_clean_sub,
                   Y = "choice_final",
                   id = "instanceID",
                   round = "round",
                   use_round = TRUE)

het_effects <- IMCE(data = cjoint_clean_sub,
                    model = cj_model,
                    attribs = c("public_service", "security", "tax_reg", "voting", "social_prot"),
                    ref_levels = c("Do not share to improve PS", "Sec services need consent", "Not linked to tax ID", "eID not required to vote", "Not linked to transfers"),
                    cores = 1)


my_het_vimp <- het_vimp(het_effects)

# Plotting variable importance scores by attribute
plot(my_het_vimp, att_levels = "Share to improve PS")
ggsave("./Writing/Figures/cjbart_vimp_all_public_service.pdf", height = 6, width = 8)

plot(my_het_vimp, att_levels = "Linked to transfers")
ggsave("./Writing/Figures/cjbart_vimp_all_social_prot.pdf", height = 6, width = 8)

plot(my_het_vimp, att_levels = "Linked to tax ID")
ggsave("./Writing/Figures/cjbart_vimp_all_tax.pdf", height = 6, width = 8)

plot(my_het_vimp, att_levels = "Sec services access data")
ggsave("./Writing/Figures/cjbart_vimp_all_security.pdf", height = 6, width = 8)

plot(my_het_vimp, att_levels = "eID required to vote")
ggsave("./Writing/Figures/cjbart_vimp_all_voting.pdf", height = 6, width = 8)

###### 4.5.2 VARIABLE IMPORTANCE METRICS ###### 

####### 4.5.2.1 For Heterogeneity Analysis of Dominant Group #######

cjoint_clean_dom <- cjoint_clean_sub %>%
  filter(dominant == "Yes") %>%
  dplyr::select(!c(dominant, opposition, securitized))

cj_model_dom <- cjbart(cjoint_clean_dom,
                       Y = "choice_final",
                       id = "instanceID",
                       round = "round",
                       use_round = TRUE)

het_effects_dom <- IMCE(data = cjoint_clean_dom,
                        model = cj_model_dom,
                        attribs = c("public_service", "security", "tax_reg", "voting", "social_prot"),
                        ref_levels = c("Do not share to improve PS", "Sec services need consent", "Not linked to tax ID", "eID not required to vote", "Not linked to transfers"),
                        cores = 1)

my_het_vimp_dom <- het_vimp(het_effects_dom)

plot(my_het_vimp_dom, att_levels = "Sec services access data")
ggsave("./Writing/Figures/cjbart_vimp_dominant_security.pdf", height = 6, width = 8)


####### 4.5.2.2 For Heterogeneity Analysis of Opposition Group #######

cjoint_clean_opp <- cjoint_clean_sub %>%
  filter(opposition == "Yes") %>%
  dplyr::select(!c(dominant, opposition, securitized))

cj_model_opp <- cjbart(cjoint_clean_opp,
                       Y = "choice_final",
                       id = "instanceID",
                       round = "round",
                       use_round = TRUE)


het_effects_opp <- IMCE(data = cjoint_clean_opp,
                        model = cj_model_opp,
                        attribs = c("public_service", "security", "tax_reg", "voting", "social_prot"),
                        ref_levels = c("Do not share to improve PS", "Sec services need consent", "Not linked to tax ID", "eID not required to vote", "Not linked to transfers"),
                        cores = 1)

my_het_vimp_opp <- het_vimp(het_effects_opp)

plot(my_het_vimp_opp, att_levels = "Sec services access data")
ggsave("./Writing/Figures/cjbart_vimp_opposition_security.pdf", height = 6, width = 8)

####### 4.5.2.3 For Heterogeneity Analysis of Securitized Group #######

cjoint_clean_sec <- cjoint_clean_sub %>%
  filter(securitized == "Yes") %>%
  dplyr::select(!c(dominant, opposition, securitized))

cj_model_sec <- cjbart(cjoint_clean_sec,
                       Y = "choice_final",
                       id = "instanceID",
                       round = "round",
                       use_round = TRUE)

het_effects_sec <- IMCE(data = cjoint_clean_sec,
                        model = cj_model_sec,
                        attribs = c("public_service", "security", "tax_reg", "voting", "social_prot"),
                        ref_levels = c("Do not share to improve PS", "Sec services need consent", "Not linked to tax ID", "eID not required to vote", "Not linked to transfers"),
                        cores = 1)

my_het_vimp_sec <- het_vimp(het_effects_sec)

plot(my_het_vimp_sec, att_levels = "Sec services access data")
ggsave("./Writing/Figures/cjbart_vimp_securitized_security.pdf", height = 6, width = 8)


###### 4.5.3 HETEROGENEITY IN IMCEs ######

# Plotting heterogeneity in IMCEs by covariate (Narirobi all is there)

plothetFun <- function(model, covariate, subset_name){
  plot(model, covar = covariate)
  ggsave(paste("./Writing/Figures/cjbart_het_", subset_name, "_", covariate, ".pdf", sep = ""), height = 6, width = 6)
}

####### FIGURES 014-016

plothetFun(het_effects_dom, "nairobi", "dominant")
plothetFun(het_effects_opp, "nairobi", "opposition")
plothetFun(het_effects_sec, "nairobi", "securitized")


##### 4.6 PAP RESULTS #####

# Function - takes outcome, data subset, subgroups (either "ethnic" or "political")
maineffectsFun <- function(outcome, data, subgroups){
  data <- data
  if(subgroups == "ethnic"){
    f <- paste0(outcome, "~ (public_service_dm + social_prot_dm + security_dm + tax_reg_dm + voting_dm)*(dominant_dm + opposition_dm + securitized_dm)")
  } else if(subgroups == "political"){
    f <- paste0(outcome, "~ (public_service_dm + social_prot_dm + security_dm + tax_reg_dm + voting_dm)*political_opposition_dm")
  }
  mod.main <- lm(as.formula(f), data = data)
  #lm(choice_final ~ (public_service_dm + social_prot_dm + security_dm + tax_reg_dm + voting_dm)*(dominant_dm + opposition_dm + securitized_dm),
  #   data = cjoint_clean)
  mod.main.c <- coeftest(mod.main, vcov = vcovCL(mod.main, cluster = data$instanceID))
  return(list(mod.main = mod.main,
              se = mod.main.c[, 2]))
}

###### 4.6.1 MAIN EFFECTS ######

coef_labs_main <- c("Public Service",
                    "Social Protection",
                    "Surveillance",
                    "Tax Registration",
                    "Voting",
                    "Dominant",
                    "Opposition",
                    "Securitized",
                    "Pub. Service x Dom.",
                    "Pub. Service x Opp.",
                    "Pub. Service x Sec.",
                    "Soc. Prot. x Dom.",
                    "Soc. Prot. x Opp.",
                    "Soc. Prot. x Sec.",
                    "Surveillance x Dom.",
                    "Surveillance x Opp.",
                    "Surveillance x Sec.",
                    "Tax Reg. x Dom.",
                    "Tax Reg. x Opp.",
                    "Tax Reg. x Sec.",
                    "Voting x Dom.",
                    "Voting x Opp.",
                    "Voting x Sec.",
                    "Constant")

main_ethnic <- lapply(primary_outcomes, function(x) maineffectsFun(x, cjoint_clean, "ethnic"))

stargazer(lapply(main_ethnic, '[[', 1),
          se = lapply(main_ethnic, '[[', 2),
          omit.stat = c("f", "ser"),
          column.labels = c("Choice", "Support", "Register"),
          covariate.labels = coef_labs_main,
          add.lines = list("CRSE at respondent level"),
          title = "Effects of Hypothetical Policy Dimensions on Support for eID Program (Main Effects Analysis)",
          label = "tab:maineffects",
          type = "latex",
          out = "Writing/Tables/maineffects.tex",
          no.space = T,
          font.size = "footnotesize",
          float = F, header=F, 
          omit.table.layout = "nld",
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001))

###### 4.6.2 TRADEOFFS (PAP) ######

tradeoffsFun <- function(outcome, data){
  data <- data
  f <- paste(outcome, "~ (public_service_dm + social_prot_dm + security_dm + tax_reg_dm + voting_dm)^2") %>%
    as.formula()
  mod.x <- lm(f, data = data)
  mod.x.c <- coeftest(mod.x, vcov = vcovCL(mod.x, cluster = ~instanceID))
  return(list(mod.x = mod.x,
              se = mod.x.c[, 2],
              pval = mod.x.c[, 4]
  ))
}

tradeoffs_dominant <- lapply(primary_outcomes, function(x) tradeoffsFun(x, dominant))
tradeoffs_opposition <- lapply(primary_outcomes, function(x) tradeoffsFun(x, opposition))
tradeoffs_securitized <- lapply(primary_outcomes, function(x) tradeoffsFun(x, securitized))

# ethnic subgroups pooled analysis
coef_labs_x <- c("Public Service",
                 "Social Protection",
                 "Surveillance",
                 "Tax Registration",
                 "Voting",
                 "Pub. Service x Soc. Prot.",
                 "Pub. Service x Surv.",
                 "Pub. Service x Tax Reg.",
                 "Pub. Service x Voting",
                 "Soc. Prot. x Surv.",
                 "Soc. Prot. x Tax Reg.",
                 "Soc. Prot. x Voting",
                 "Surv. x Tax Reg.",
                 "Surv. x Voting",
                 "Tax Reg. x Voting",
                 "Constant")

tradeoffs_ethnic <- c(tradeoffs_dominant, tradeoffs_opposition, tradeoffs_securitized)

stargazer(lapply(tradeoffs_ethnic, '[[', 1),
          se = lapply(tradeoffs_ethnic, '[[', 2),
          omit.stat = c("f", "ser"),
          covariate.labels = coef_labs_x,
          column.labels = rep(c("Choice", "Support", "Register"), 3),
          add.lines = list("CRSE at respondent level"),
          type = "latex",
          out = "Writing/Tables/tradeoffs.tex",
          no.space = T,
          font.size = "footnotesize",
          float = F, header=F, 
          omit.table.layout = "nld",
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001))


###### 4.6.3 EXPLORATORY ANALYSIS ######

####### 4.6.3.1 Secondary Outcomes #######

######## 4.6.3.1.a  mechanisms (PAP) ######## 

main_secondary_ethnic <- lapply(secondary_outcomes, function(x) maineffectsFun(x, cjoint_clean_r1, "ethnic"))

stargazer(lapply(main_secondary_ethnic, '[[', 1),
          se = lapply(main_secondary_ethnic, '[[', 2),
          omit.stat = c("f", "ser"),
          column.labels = c("Access Services", "Data Privacy", "Worry", "Worry", "Worry \\\\ & & & Punished View & Vote Counted & Personal Info"),
          covariate.labels = coef_labs_main,
          add.lines = list("CRSE at respondent level"),
          title = "Effects of Hypothetical Policy Dimensions on Mechanisms Outcomes (Main Effects Analysis, Ethnic Groups)",
          label = "tab:maineffectsmech",
          type = "latex",
          out = "Writing/Tables/maineffectsmech.tex",
          no.space = T,
          font.size = "footnotesize",
          float = F, header=F, 
          omit.table.layout = "nld",
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001))

######## 4.6.3.1.b tradeoffs mechanisms (PAP): dominant 

dominant_r1 <- cjoint_clean_r1[cjoint_clean_r1$dominant == 1, ]
opposition_r1 <- cjoint_clean_r1[cjoint_clean_r1$opposition == 1, ]
securitized_r1 <- cjoint_clean_r1[cjoint_clean_r1$securitized == 1, ]

tradeoffs_secondary_dominant <- lapply(secondary_outcomes, function(x) tradeoffsFun(x, dominant_r1))

stargazer(lapply(tradeoffs_secondary_dominant, '[[', 1),
          se = lapply(tradeoffs_secondary_dominant, '[[', 2),
          omit.stat = c("f", "ser"),
          column.labels = c("Access Services", "Data Privacy", "Worry", "Worry", "Worry \\\\ & & & Punished View & Vote Counted & Personal Info"),
          covariate.labels = coef_labs_x,
          add.lines = list("CRSE at respondent level"),
          type = "latex",
          out = "Writing/Tables/tradeoffmech_dom.tex",
          no.space = T,
          font.size = "footnotesize",
          float = F, header=F, 
          omit.table.layout = "nld",
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001))


######## 4.6.3.1.c tradeoffs mechanisms (PAP): opposition 

tradeoffs_secondary_opposition <- lapply(secondary_outcomes, function(x) tradeoffsFun(x, opposition_r1))

stargazer(lapply(tradeoffs_secondary_opposition, '[[', 1),
          se = lapply(tradeoffs_secondary_opposition, '[[', 2),
          omit.stat = c("f", "ser"),
          column.labels = c("Access Services", "Data Privacy", "Worry", "Worry", "Worry \\\\ & & & Punished View & Vote Counted & Personal Info"),
          covariate.labels = coef_labs_x,
          add.lines = list("CRSE at respondent level"),
          type = "latex",
          out = "Writing/Tables/tradeoffmech_opp.tex",
          no.space = T,
          font.size = "footnotesize",
          float = F, header=F, 
          omit.table.layout = "nld",
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001))


######## 4.6.3.1.c tradeoffs mechanisms (PAP): securitized 

tradeoffs_secondary_securitized <- lapply(secondary_outcomes, function(x) tradeoffsFun(x, securitized_r1))

stargazer(lapply(tradeoffs_secondary_securitized, '[[', 1),
          se = lapply(tradeoffs_secondary_securitized, '[[', 2),
          omit.stat = c("f", "ser"),
          column.labels = c("Access Services", "Data Privacy", "Worry", "Worry", "Worry \\\\ & & & Punished View & Vote Counted & Personal Info"),
          covariate.labels = coef_labs_x,
          add.lines = list("CRSE at respondent level"),
          type = "latex",
          out = "Writing/Tables/tradeoffmech_sec.tex",
          no.space = T,
          font.size = "footnotesize",
          float = F, header=F, 
          omit.table.layout = "nld",
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001))

###### 4.6.3.2 Political Coalitions ######

######## Main effects

main_pol <- lapply(primary_outcomes, function(x) maineffectsFun(x, cjoint_clean, "political"))

coef_labs_main_po <- c("Public Service",
                       "Social Protection",
                       "Surveillance",
                       "Tax Registration",
                       "Voting",
                       "Political Opposition",
                       "Pub. Service x Pol. Opp.",
                       "Soc. Prot. x Pol. Opp.",
                       "Surveillance x Pol. Opp.",
                       "Tax Reg. x Pol. Opp.",
                       "Voting x Pol. Opp.",
                       "Constant"
)

stargazer(lapply(main_pol, '[[', 1),
          se = lapply(main_pol, '[[', 2),
          omit.stat = c("f", "ser"),
          column.labels = c("Choice", "Support", "Register"),
          covariate.labels = coef_labs_main_po,
          add.lines = list("CRSE at respondent level"),
          title = "Effects of Hypothetical Policy Dimensions on Support for eID Program (Main Effects Analysis, Political Coalitions)",
          label = "tab:maineffectspol",
          type = "latex",
          out = "Writing/Tables/maineffectspol.tex",
          no.space = T,
          font.size = "footnotesize",
          float = F, header=F,
          omit.table.layout = "nld",
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001))

######## 4.6.3.2.b tradeoffs

pol_opposition <- cjoint_clean[cjoint_clean$political_opposition == 1, ]
pol_non_opposition <- cjoint_clean[cjoint_clean$political_opposition == 0, ]

tradeoffs_pol_opposition <- lapply(primary_outcomes, function(x) tradeoffsFun(x, pol_opposition))
tradeoffs_pol_non_opposition <- lapply(primary_outcomes, function(x) tradeoffsFun(x, pol_non_opposition))

tradeoffs_political <- c(tradeoffs_pol_opposition, tradeoffs_pol_non_opposition)

stargazer(lapply(tradeoffs_political, '[[', 1),
          se = lapply(tradeoffs_political, '[[', 2),
          omit.stat = c("f", "ser"),
          column.labels = rep(c("Choice", "Support", "Register"),2),
          covariate.labels = coef_labs_x,
          add.lines = list("CRSE at respondent level"),
          type = "latex",
          out = "Writing/Tables/tradeoffpol.tex",
          no.space = T,
          font.size = "footnotesize",
          float = F, header=F,
          omit.table.layout = "nld",
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001))

######## 4.6.3.2.c mechanisms

main_secondary_pol <- lapply(secondary_outcomes, function(x) maineffectsFun(x, cjoint_clean_r1, "political"))

stargazer(lapply(main_secondary_pol, '[[', 1),
          se = lapply(main_secondary_pol, '[[', 2),
          omit.stat = c("f", "ser"),
          column.labels = c("Access Services", "Data Privacy", "Worry", "Worry", "Worry \\\\ & & & Punished View & Vote Counted & Personal Info"),
          covariate.labels = coef_labs_main_po,
          add.lines = list("CRSE at respondent level"),
          title = "Effects of Hypothetical Policy Dimensions on Mechanisms Outcomes (Main Effects Analysis, Political Coalitions)",
          label = "tab:maineffectsmechpol",
          type = "latex",
          out = "Writing/Tables/maineffectsmechpol.tex",
          no.space = T,
          font.size = "footnotesize",
          float = F, header=F,
          omit.table.layout = "nld",
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001))

######## 4.6.3.2.d tradeoffs mechanisms

pol_opposition_r1 <- cjoint_clean_r1[cjoint_clean_r1$political_opposition == 1, ]
pol_non_opposition_r1 <- cjoint_clean_r1[cjoint_clean_r1$political_opposition == 0, ]

tradeoffs_secondary_pol_opposition <- lapply(secondary_outcomes, function(x) tradeoffsFun(x, pol_opposition_r1))
tradeoffs_secondary_pol_non_opposition <- lapply(secondary_outcomes, function(x) tradeoffsFun(x, pol_non_opposition_r1))


tradeoffs_secondary_political <- c(tradeoffs_secondary_pol_opposition, tradeoffs_secondary_pol_non_opposition)


stargazer(lapply(tradeoffs_secondary_political, '[[', 1),
          se = lapply(tradeoffs_secondary_political, '[[', 2),
          omit.stat = c("f", "ser"),
          column.labels = c("Access Services", "Data Privacy", "Worry", "Worry", "Worry", "Access Services", "Data Privacy", "Worry", "Worry", "Worry \\\\ & & & Punished View & Vote Counted & Personal Info & & & Punished View & Vote Counted & Personal Info "),
          covariate.labels = coef_labs_x,
          add.lines = list("CRSE at respondent level"),
          type = "latex",
          out = "Writing/Tables/tradeoffmechpol.tex",
          no.space = T,
          font.size = "footnotesize",
          float = F, header=F,
          omit.table.layout = "nld",
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001))



###### 4.6.4 ROBUSTNESS CHECK (PAP) ######

maineffects_robustnessFun <- function(outcome, data, subgroups, robustness_type){
  data <- data
  if(subgroups == "ethnic"){
    if(robustness_type == "enumFE"){
      f <- paste0(outcome, "~ (public_service_dm + social_prot_dm + security_dm + tax_reg_dm + voting_dm)*(dominant_dm + opposition_dm + securitized_dm) + factor(enum_name)")
    } else if(robustness_type == "roundFE"){
      f <- paste0(outcome, "~ (public_service_dm + social_prot_dm + security_dm + tax_reg_dm + voting_dm)*(dominant_dm + opposition_dm + securitized_dm) + factor(round)")
    } else if(robustness_type == "nairobi"){
      f <- paste0(outcome, "~ (public_service_dm + social_prot_dm + security_dm + tax_reg_dm + voting_dm)*(dominant_dm + opposition_dm + securitized_dm) + nairobi_dm")
    }
  } else if(subgroups == "political"){
    if(robustness_type == "enumFE"){
      f <- paste0(outcome, "~ (public_service_dm + social_prot_dm + security_dm + tax_reg_dm + voting_dm)*(dominant_dm + opposition_dm + securitized_dm) + factor(enum_name)")
    } else if(robustness_type == "roundFE"){
      f <- paste0(outcome, "~ (public_service_dm + social_prot_dm + security_dm + tax_reg_dm + voting_dm)*(dominant_dm + opposition_dm + securitized_dm) + factor(round)")
    } else if(robustness_type == "nairobi"){
      f <- paste0(outcome, "~ (public_service_dm + social_prot_dm + security_dm + tax_reg_dm + voting_dm)*(dominant_dm + opposition_dm + securitized_dm) + nairobi_dm")
    }
  }
  mod.main <- lm(as.formula(f), data = data)
  #lm(choice_final ~ (public_service_dm + social_prot_dm + security_dm + tax_reg_dm + voting_dm)*(dominant_dm + opposition_dm + securitized_dm),
  #   data = cjoint_clean)
  mod.main.c <- coeftest(mod.main, vcov = vcovCL(mod.main, cluster = data$instanceID))
  return(list(mod.main = mod.main,
              se = mod.main.c[, 2]))
}


tradeoffs_robustnessFun <- function(outcome, data, robustness_type){
  data <- data
  if(robustness_type == "enumFE"){
    f <- paste(outcome, "~ (public_service_dm + social_prot_dm + security_dm + tax_reg_dm + voting_dm)^2 + factor(enum_name)") %>%
      as.formula()
  } else if(robustness_type == "roundFE"){
    f <- paste(outcome, "~ (public_service_dm + social_prot_dm + security_dm + tax_reg_dm + voting_dm)^2 + factor(round)") %>%
      as.formula()
  } else if(robustness_type == "nairobi"){
    f <- paste(outcome, "~ (public_service_dm + social_prot_dm + security_dm + tax_reg_dm + voting_dm)^2 + nairobi_dm") %>%
      as.formula()
  }
  mod.x <- lm(f, data = data)
  mod.x.c <- coeftest(mod.x, vcov = vcovCL(mod.x, cluster = ~instanceID))
  return(list(mod.x = mod.x,
              se = mod.x.c[, 2]))
}

####### 4.6.4.1 Main Effects: Enumerator FE #######

main_ethnic_enumFE <- lapply(primary_outcomes, function(x) maineffects_robustnessFun(x, cjoint_clean, "ethnic", "enumFE"))

stargazer(lapply(main_ethnic_enumFE, '[[', 1),
          se = lapply(main_ethnic_enumFE, '[[', 2),
          omit.stat = c("f", "ser"),
          column.labels = c("Choice", "Support", "Register"),
          covariate.labels = coef_labs_main,
          add.lines = list(c("CRSE at respondent level",rep("",3)), c("Enumerator FE", rep("Yes",3))),
          type = "latex",
          out = "Writing/Tables/maineffects_PAP_enumFE.tex",
          no.space = T,
          font.size = "footnotesize",
          float = F, header=F, 
          omit.table.layout = "nld",
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001),
          omit = "enum_name")

####### 4.6.4.2 Tradeoffs (PAP): Enumerator FE ######

tradeoff_dom_enumFE <- lapply(primary_outcomes, function(x) tradeoffs_robustnessFun(x, dominant, "enumFE"))
tradeoff_opp_enumFE <- lapply(primary_outcomes, function(x) tradeoffs_robustnessFun(x, opposition, "enumFE"))
tradeoff_sec_enumFE <- lapply(primary_outcomes, function(x) tradeoffs_robustnessFun(x, securitized, "enumFE"))

tradeoff_enumFE <- c(tradeoff_dom_enumFE, tradeoff_opp_enumFE, tradeoff_sec_enumFE)

stargazer(lapply(tradeoff_enumFE, '[[', 1),
          se = lapply(tradeoff_enumFE, '[[', 2),
          omit.stat = c("f", "ser"),
          column.labels = rep(c("Choice", "Support", "Register"),3),
          covariate.labels = coef_labs_x,
          add.lines = list(c("CRSE at respondent level",rep("",9)), c("Enumerator FE", rep("Yes",9))),
          type = "text",
          out = "Writing/Tables/tradeoffs_PAP_enumFE.tex",
          no.space = T,
          font.size = "footnotesize",
          float = F, header=F, 
          omit.table.layout = "nld",
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001),
          omit = "enum_name")

####### 4.6.4.3 Mechanisms (PAP): Enumerator FE #######

secondary_ethnic_enumFE <- lapply(secondary_outcomes, function(x) maineffects_robustnessFun(x, cjoint_clean_r1, "ethnic", "enumFE"))

stargazer(lapply(secondary_ethnic_enumFE, '[[', 1),
          se = lapply(secondary_ethnic_enumFE, '[[', 2),
          omit.stat = c("f", "ser"),
          column.labels = c("Access Services", "Data Privacy", "Worry", "Worry", "Worry \\\\ & & & Punished View & Vote Counted & Personal Info"),
          covariate.labels = coef_labs_main,
          add.lines = list(c("CRSE at respondent level",rep("",5)), c("Enumerator FE", rep("Yes",5))),
          type = "latex",
          out = "Writing/Tables/mechanisms_PAP_enumFE.tex",
          no.space = T,
          font.size = "footnotesize",
          float = F, header=F, 
          omit.table.layout = "nld",
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001),
          omit = "enum_name")

####### 4.6.4.4 Tradeoffs Mechanisms (PAP): Enumerator FE #######
######## Tradeoffs Mechanisms: dominant 

tradeoffmech_dom_enumFE <- lapply(secondary_outcomes, function(x) tradeoffs_robustnessFun(x, dominant_r1, "enumFE"))

stargazer(lapply(tradeoffmech_dom_enumFE, '[[', 1),
          se = lapply(tradeoffmech_dom_enumFE, '[[', 2),
          omit.stat = c("f", "ser"),
          column.labels = c("Access Services", "Data Privacy", "Worry", "Worry", "Worry \\\\ & & & Punished View & Vote Counted & Personal Info"),
          covariate.labels = coef_labs_x,
          add.lines = list(c("CRSE at respondent level",rep("",5)), c("Enumerator FE", rep("Yes",5))),
          type = "latex",
          out = "Writing/Tables/tradeoffmech_dom_PAP_enumFE.tex",
          no.space = T,
          font.size = "footnotesize",
          float = F, header=F, 
          omit.table.layout = "nld",
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001),
          omit = "enum_name")


######## Tradeoffs Mechanisms: opposition 

tradeoffmech_opp_enumFE <- lapply(secondary_outcomes, function(x) tradeoffs_robustnessFun(x, opposition_r1, "enumFE"))

stargazer(lapply(tradeoffmech_opp_enumFE, '[[', 1),
          se = lapply(tradeoffmech_opp_enumFE, '[[', 2),
          omit.stat = c("f", "ser"),
          column.labels = c("Access Services", "Data Privacy", "Worry", "Worry", "Worry \\\\ & & & Punished View & Vote Counted & Personal Info"),
          covariate.labels = coef_labs_x,
          add.lines = list(c("CRSE at respondent level",rep("",5)), c("Enumerator FE", rep("Yes",5))),
          type = "latex",
          out = "Writing/Tables/tradeoffmech_opp_PAP_enumFE.tex",
          no.space = T,
          font.size = "footnotesize",
          float = F, header=F, 
          omit.table.layout = "nld",
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001),
          omit = "enum_name")

######## Tradeoffs Mechanisms:securitized 

tradeoffmech_sec_enumFE <- lapply(secondary_outcomes, function(x) tradeoffs_robustnessFun(x, securitized_r1, "enumFE"))

stargazer(lapply(tradeoffmech_sec_enumFE, '[[', 1),
          se = lapply(tradeoffmech_sec_enumFE, '[[', 2),
          omit.stat = c("f", "ser"),
          column.labels = c("Access Services", "Data Privacy", "Worry", "Worry", "Worry \\\\ & & & Punished View & Vote Counted & Personal Info"),
          covariate.labels = coef_labs_x,
          add.lines = list(c("CRSE at respondent level",rep("",5)), c("Enumerator FE", rep("Yes",5))),
          type = "latex",
          out = "Writing/Tables/tradeoffmech_sec_PAP_enumFE.tex",
          no.space = T,
          font.size = "footnotesize",
          float = F, header=F, 
          omit.table.layout = "nld",
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001),
          omit = "enum_name")

####### 4.6.4.2 Round Fixed Effects #######

########4.6.4.2.a main effects 

main_ethnic_roundFE <- lapply(primary_outcomes, function(x) maineffects_robustnessFun(x, cjoint_clean, "ethnic", "roundFE"))

stargazer(lapply(main_ethnic_roundFE, '[[', 1),
          se = lapply(main_ethnic_roundFE, '[[', 2),
          omit.stat = c("f", "ser"),
          column.labels = c("Choice", "Support", "Register"),
          covariate.labels = coef_labs_main,
          add.lines = list(c("CRSE at respondent level",rep("",3)), c("Round FE", rep("Yes",3))),
          type = "latex",
          out = "Writing/Tables/maineffects_PAP_roundFE.tex",
          no.space = T,
          font.size = "footnotesize",
          float = F, header=F, 
          omit.table.layout = "nld",
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001),
          omit = "round")


######## 4.6.4.2.b tradeoffs (PAP)

tradeoff_dom_roundFE <- lapply(primary_outcomes, function(x) tradeoffs_robustnessFun(x, dominant, "roundFE"))
tradeoff_opp_roundFE <- lapply(primary_outcomes, function(x) tradeoffs_robustnessFun(x, opposition, "roundFE"))
tradeoff_sec_roundFE <- lapply(primary_outcomes, function(x) tradeoffs_robustnessFun(x, securitized, "roundFE"))

tradeoff_roundFE <- c(tradeoff_dom_roundFE, tradeoff_opp_roundFE, tradeoff_sec_roundFE)

stargazer(lapply(tradeoff_roundFE, '[[', 1),
          se = lapply(tradeoff_roundFE, '[[', 2),
          omit.stat = c("f", "ser"),
          column.labels = rep(c("Choice", "Support", "Register"),3),
          covariate.labels = coef_labs_x,
          add.lines = list(c("CRSE at respondent level",rep("",9)), c("Round FE", rep("Yes",9))),
          type = "latex",
          out = "Writing/Tables/tradeoffs_PAP_roundFE.tex",
          no.space = T,
          font.size = "footnotesize",
          float = F, header=F, 
          omit.table.layout = "nld",
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001),
          omit = "round")


####### 4.6.4.3 Nairobi Dummy #######


######## 4.6.4.3.a main effects 

# Main Effects
main_ethnic_nairobi <- lapply(primary_outcomes, function(x) maineffects_robustnessFun(x, cjoint_clean, "ethnic", "nairobi"))

coef_labs_main_PAP_nairobi <- c("Public Service",
                                "Social Protection",
                                "Surveillance",
                                "Tax Registration",
                                "Voting",
                                "Dominant",
                                "Opposition",
                                "Securitized",
                                "Nairobi",
                                "Pub. Service x Dom.",
                                "Pub. Service x Opp.",
                                "Pub. Service x Sec.",
                                "Soc. Prot. x Dom.",
                                "Soc. Prot. x Opp.",
                                "Soc. Prot. x Sec.",
                                "Surveillance x Dom.",
                                "Surveillance x Opp.",
                                "Surveillance x Sec.",
                                "Tax Reg. x Dom.",
                                "Tax Reg. x Opp.",
                                "Tax Reg. x Sec.",
                                "Voting x Dom.",
                                "Voting x Opp.",
                                "Voting x Sec.",
                                "Constant")

stargazer(lapply(main_ethnic_nairobi, '[[', 1),
          se = lapply(main_ethnic_nairobi, '[[', 2),
          omit.stat = c("f", "ser"),
          column.labels = c("Choice", "Support", "Register"),
          covariate.labels = coef_labs_main_PAP_nairobi,
          add.lines = list("CRSE at respondent level"),
          type = "latex",
          out = "Writing/Tables/maineffects_PAP_nairobi.tex",
          no.space = T,
          font.size = "footnotesize",
          float = F, header=F, 
          omit.table.layout = "nld",
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001))

######## 4.6.4.3.b tradeoffs 

tradeoff_dom_nairobi <- lapply(primary_outcomes, function(x) tradeoffs_robustnessFun(x, dominant, "nairobi"))
tradeoff_opp_nairobi <- lapply(primary_outcomes, function(x) tradeoffs_robustnessFun(x, opposition, "nairobi"))
tradeoff_sec_nairobi <- lapply(primary_outcomes, function(x) tradeoffs_robustnessFun(x, securitized, "nairobi"))

tradeoff_nairobi <- c(tradeoff_dom_nairobi, tradeoff_opp_nairobi, tradeoff_sec_nairobi)

coef_labs_x_nairobi <- c("Public Service",
                         "Social Protection",
                         "Surveillance",
                         "Tax Registration",
                         "Voting",
                         "Pub. Service x Soc. Prot.",
                         "Pub. Service x Surv.",
                         "Pub. Service x Tax Reg.",
                         "Pub. Service x Voting",
                         "Soc. Prot. x Surv.",
                         "Soc. Prot. x Tax Reg.",
                         "Soc. Prot. x Voting",
                         "Surv. x Tax Reg.",
                         "Surv. x Voting",
                         "Tax Reg. x Voting",
                         "Nairobi",
                         "Constant")

stargazer(lapply(tradeoff_nairobi, '[[', 1),
          se = lapply(tradeoff_nairobi, '[[', 2),
          omit.stat = c("f", "ser"),
          column.labels = rep(c("Choice", "Support", "Register"),3),
          covariate.labels = coef_labs_x_nairobi,
          add.lines = list("CRSE at respondent level"),
          type = "latex",
          out = "Writing/Tables/tradeoffs_PAP_nairobi.tex",
          no.space = T,
          font.size = "footnotesize",
          float = F, header=F, 
          omit.table.layout = "nld",
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001),
          order = c(1,2,3,4,5,7,8,9,10,11,12,13,14,15,16,6,17))

######## 4.6.4.3.c mechanisms 

secondary_ethnic_nairobi <- lapply(secondary_outcomes, function(x) maineffects_robustnessFun(x, cjoint_clean_r1, "ethnic", "nairobi"))

stargazer(lapply(secondary_ethnic_nairobi, '[[', 1),
          se = lapply(secondary_ethnic_nairobi, '[[', 2),
          omit.stat = c("f", "ser"),
          column.labels = c("Access Services", "Data Privacy", "Worry", "Worry", "Worry \\\\ & & & Punished View & Vote Counted & Personal Info"),
          covariate.labels = coef_labs_main_PAP_nairobi,
          add.lines = list("CRSE at respondent level"),
          type = "latex",
          out = "Writing/Tables/mechanisms_PAP_nairobi.tex",
          no.space = T,
          font.size = "footnotesize",
          float = F, header=F, 
          omit.table.layout = "nld",
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001))

######## tradeoffs mechanisms: dominant 

tradeoffmech_dom_nairobi <- lapply(secondary_outcomes, function(x) tradeoffs_robustnessFun(x, dominant_r1, "nairobi"))

stargazer(lapply(tradeoffmech_dom_nairobi, '[[', 1),
          se = lapply(tradeoffmech_dom_nairobi, '[[', 2),
          omit.stat = c("f", "ser"),
          column.labels = c("Access Services", "Data Privacy", "Worry", "Worry", "Worry \\\\ & & & Punished View & Vote Counted & Personal Info"),
          covariate.labels = coef_labs_x_nairobi,
          add.lines = list("CRSE at respondent level"),
          type = "latex",
          out = "Writing/Tables/tradeoffmech_dom_PAP_nairobi.tex",
          no.space = T,
          font.size = "footnotesize",
          float = F, header=F, 
          omit.table.layout = "nld",
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001),
          order = c(1,2,3,4,5,7,8,9,10,11,12,13,14,15,16,6,17))

######## tradeoffs mechanisms: opposition 

tradeoffmech_opp_nairobi <- lapply(secondary_outcomes, function(x) tradeoffs_robustnessFun(x, opposition_r1, "nairobi"))

stargazer(lapply(tradeoffmech_opp_nairobi, '[[', 1),
          se = lapply(tradeoffmech_opp_nairobi, '[[', 2),
          omit.stat = c("f", "ser"),
          column.labels = c("Access Services", "Data Privacy", "Worry", "Worry", "Worry \\\\ & & & Punished View & Vote Counted & Personal Info"),
          covariate.labels = coef_labs_x_nairobi,
          add.lines = list("CRSE at respondent level"),
          type = "latex",
          out = "Writing/Tables/tradeoffmech_opp_PAP_nairobi.tex",
          no.space = T,
          font.size = "footnotesize",
          float = F, header=F, 
          omit.table.layout = "nld",
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001),
          order = c(1,2,3,4,5,7,8,9,10,11,12,13,14,15,16,6,17))

######## tradeoffs mechanisms: securitized 

tradeoffmech_sec_nairobi <- lapply(secondary_outcomes, function(x) tradeoffs_robustnessFun(x, securitized_r1, "nairobi"))

stargazer(lapply(tradeoffmech_sec_nairobi, '[[', 1),
          se = lapply(tradeoffmech_sec_nairobi, '[[', 2),
          omit.stat = c("f", "ser"),
          column.labels = c("Access Services", "Data Privacy", "Worry", "Worry", "Worry \\\\ & & & Punished View & Vote Counted & Personal Info"),
          covariate.labels = coef_labs_x_nairobi,
          add.lines = list("CRSE at respondent level"),
          type = "latex",
          out = "Writing/Tables/tradeoffmech_sec_PAP_nairobi.tex",
          no.space = T,
          font.size = "footnotesize",
          float = F, header=F, 
          omit.table.layout = "nld",
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001),
          order = c(1,2,3,4,5,7,8,9,10,11,12,13,14,15,16,6,17))


