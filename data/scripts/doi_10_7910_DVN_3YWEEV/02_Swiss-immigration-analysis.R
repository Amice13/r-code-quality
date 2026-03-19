######################################################
#### Overseas Climate Adaptation Assistance:
#### Assessing Public Support in Switzerland
### Andonova, Zucha, Montfort, Dolsak, Prakash
## Script compiled by Simon Montfort
# 16.12.2024
######################################################
R.version
# platform       aarch64-apple-darwin20      
# arch           aarch64                     
# os             darwin20                    
# system         aarch64, darwin20           
# status                                     
# major          4                           
# minor          3.2                         
# year           2023                        
# month          10                          
# day            31                          
# svn rev        85441                       
# language       R                           
# version.string R version 4.3.2 (2023-10-31)
# nickname       Eye Holes          

rm(list = ls())

# packages
library(cjoint)
library(dplyr)
library(ggthemes)
library(ggplot2)
library(gridExtra)
library(lubridate)
library(readxl)
library(tibble)
library(ggpubr)
library(emmeans)
library(sjPlot)
library(cregg)
library(xtable)
library(knitr)
library(kableExtra)
library(magrittr)
library(jtools)
library(texreg)
library(stringi)

setwd("/Users/simon/Documents/repo/swiss-immigration")

##########################################################
# descriptive statistics
##########################################################

dat <- readRDS("data-survey/swiss_immigration_survey_data.rds")

dat_desc <- dat %>% 
  dplyr::select(choice, age, gender, educ, income, employment, UserLanguage,
                immi_pc_quart, attrib1_lab, attrib2_lab, attrib3_lab, attrib4_lab, attrib5_lab, attrib6_lab,
                mech_lang_bin, mech_cult_bin, mech_proud_bin, mech_devel_bin,
                left_right_bin) %>% 
  mutate_all(as.character) 


p_desc <- dat_desc %>% 
  pivot_longer(., everything(), names_to = "Question", values_to = "Response") %>% 
  mutate(Question = factor(Question, levels = c("choice", "attrib1_lab", "attrib2_lab", "attrib3_lab", "attrib4_lab", "attrib5_lab", "attrib6_lab",
                                                "age", "gender", "educ", "income", "employment", "UserLanguage",
                                                "immi_pc_quart",
                                                "mech_lang_bin", "mech_cult_bin", "mech_proud_bin", "mech_devel_bin",
                                                "left_right_bin"
                                                # "urban_rural"
  ))) %>% 
  group_by(Question, Response) %>% 
  count() %>% 
  # mutate(Response = factor(Response, levels = as.character(c(0:14, NA)))) %>% 
  ggplot(aes(x = Response, y = n)) +
  geom_col() + labs(x = "", y = "") +
  coord_flip() +
  facet_wrap(~Question, scales = "free_y", ncol = 3,
             labeller = labeller(Question = c("choice" = "Choice",
                                              "attrib1_lab" = "Recipient developing country", 
                                              "attrib2_lab" = "Number of climate\nmigrants to accept\nfrom this country per year", 
                                              "attrib3_lab" = "Climate aid to give\nto this country\n(CHF) per year", 
                                              "attrib4_lab" = "Value of Swiss\ntrade with this\ncountry", 
                                              "attrib5_lab" = "Extreme weather event", 
                                              "attrib6_lab" = "Percentage of this\ncountry's votes\nin line with Switzerland's\nposition at the UN\nSecurity Council",
                                              "age" = "Age", 
                                              "gender" = "Gender",
                                              "educ" = "Education", 
                                              "income" = "Income", 
                                              "employment" = "Employment",
                                              "UserLanguage" = "Language",
                                              "immi_pc_quart" = "Immigration pc quartiles",
                                              "mech_lang_bin" = "Language Mechanism", 
                                              "mech_cult_bin" = "Culture Mechanism", 
                                              "mech_proud_bin" = "Proudness Mechanism", 
                                              "mech_devel_bin" = "Development Mechanism",
                                              "left_right_bin" = "Left-right" 
             ))
  ) +
  theme_light()
p_desc
ggsave(p_desc, filename = "plots/p_desc.pdf", height = 14, width = 10)

##########################################################
# analyse results
##########################################################

rename_vars <- function(res){
  res %>% 
    mutate(feature_lab = "",
           feature_lab = ifelse(feature == "attrib1_lab", "Recipient developing country", feature_lab),
           feature_lab = ifelse(feature == "attrib2_lab", "Number of climate\nmigrants to accept\nfrom this country per year", feature_lab),
           feature_lab = ifelse(feature == "attrib3_lab", "Climate aid to give\nto this country\n(CHF) per year", feature_lab),
           feature_lab = ifelse(feature == "attrib4_lab", "Value of Swiss\ntrade with this\ncountry", feature_lab),
           feature_lab = ifelse(feature == "attrib5_lab", "Extreme weather event", feature_lab),
           feature_lab = ifelse(feature == "attrib6_lab", "Percentage of this\ncountry's votes\nin line with Switzerland's\nposition at the UN\nSecurity Council", feature_lab),
           feature_lab = factor(feature_lab, levels = c("Recipient developing country", "Number of climate\nmigrants to accept\nfrom this country per year",
                                                        "Climate aid to give\nto this country\n(CHF) per year", "Value of Swiss\ntrade with this\ncountry",
                                                        "Extreme weather event", "Percentage of this\ncountry's votes\nin line with Switzerland's\nposition at the UN\nSecurity Council"))
    ) %>% 
    mutate(star = "",
           star = ifelse(p < 0.05, "*", star),
           star = ifelse(p < 0.01, "**", star),
           star = ifelse(p < 0.001, "***", star),
           p = round(as.numeric(p), digits = 3),
           p = paste0(p, star)
    ) %>% 
    dplyr::select(feature_lab, level, estimate, p, lower, upper)
}

#### I.
res <- mm(dat, choice ~ attrib1_lab + attrib2_lab + attrib3_lab + attrib4_lab + attrib5_lab + attrib6_lab, id = ~id, h0 = 0.5)

export_table <- function(name, col_label, formula, by){
  if (!is.null(by)) {
    by_cregg <- as.formula(by)
  } else {
    by_cregg <- by
  }
  formula_cregg <- as.formula(formula)
  amce_by <- cj(dat, formula_cregg, id = ~id, by = by_cregg, estimate = "amce")
  mm_by <- cj(dat, formula_cregg, id = ~id, by = by_cregg, estimate = "mm", h0 = 0.5)
  amce_by <- amce_by %>% dplyr::select(any_of(c("BY", "feature", "level", "estimate", "p"))) %>% rename("estimate AMCE" = "estimate", "p-value AMCE" = "p")
  mm_by <- mm_by %>% dplyr::select(any_of(c("BY", "feature", "level", "estimate", "p"))) %>% rename("estimate MM" = "estimate", "p-value MM" = "p")
  if (!is.null(by)) {
    formula_lm <- paste0(unlist(strsplit(as.character(formula), "+",  fixed = T)), collapse = paste0("*", gsub("~", "", by), " +"))
  } else {
    formula_lm <- formula_cregg
  }
  res <- lm(formula_lm, dat)
  x <- summary(res)
  x <- coeftest(res, vcov = vcovCL(res, cluster = ~id, type = "HC"))
  
  if (!is.null(by)) {
    out <- mm_by %>% dplyr::select(level, feature, BY, `estimate MM`, `p-value MM`) %>% left_join(amce_by, by = c("feature", "level", "BY")) 
  } else {
    out <- mm_by %>% dplyr::select(level, feature, `estimate MM`, `p-value MM`) %>% left_join(amce_by, by = c("feature", "level"))
  }
  gof <- as.data.frame(rbind(c("Number of observations", "", "",  "",nobs(res), ""),
                             c("R2", "", "",  "",sapply(lapply(list(res), summ), attr, "rsq"), ""),
                             c("Adj.R2", "", "",  "",sapply(lapply(list(res), summ), attr, "arsq"), ""))) %>% 
    mutate(V2 = factor(V2),
           V5 = round(as.numeric(V5), 4)) %>% 
    mutate_if(is.double, as.character)
  if (!is.null(by)) {gof <- cbind(gof[,1:2], rep("", nrow(gof)), gof[,3:6])}
  colnames(gof) <- colnames(out)
  
  options(scipen = 999)
  c_names <- if(!is.null(by)) {
    c("Attribute levels", col_label, "Estimate", "p-value", "Estimate", "p-value")
  } else {
    c("Attribute levels", "Estimate", "p-value", "Estimate", "p-value")
  }
  tab <- out %>%
    dplyr::select(-feature) %>% 
    mutate_at(vars("estimate MM", "p-value MM", "estimate AMCE", "p-value AMCE"), round, 4) %>%
    mutate_at(vars("estimate MM", "p-value MM", "estimate AMCE", "p-value AMCE"), as.character) %>% 
    bind_rows(gof %>% dplyr::select(-feature)) %>%
    # dplyr::select(all_of("Attribute levels", col_label, c("Estimate", "p-value", "Estimate", "p-value")))
    mutate(`estimate AMCE` = ifelse(is.na(`p-value AMCE`), "baseline", `estimate AMCE`)) %>%
    mutate(`p-value AMCE` = ifelse(is.na(`p-value AMCE`), "", `p-value AMCE`)) %>%
    mutate(`estimate MM` = ifelse(is.na(`estimate MM`), "", `estimate MM`)) %>%
    kable(format = 'latex', booktabs = TRUE, col.names = c_names, caption = paste(name, "estimates"), longtable = T) %>% 
    kable_styling(font_size = 9) %>% 
    footnote("Standard errors for the computation of p-values were clustered by respondent id. For MM estimates that are derived from AMCEs (for details, see Leeper et al., 2018), p-values are computed under null hypothesis that the estimate is equal to 0.5 for the binary choice outcome where respondents choose the preferred policy package when presented with two policy pairs with randomized attribute levels. The label for the attribute level ’Percentage of this country's votes in line with Switzerland's position at the UN Security Council’ was replaced with to ’UN Security Council votes in line with Switzerland’ for better readability.",
             footnote_as_chunk = T,
             threeparttable = TRUE,
    )
  
  if (!is.null(by)){
    tab <- tab %>% add_header_above(c(" " = 2, "MM" = 2, "AMCE" = 2))
  } else {
    tab <- tab %>% add_header_above(c(" " = 1, "MM" = 2, "AMCE" = 2))
  }
  
  attr <- data.frame(attribute_levels = c("Recipient developing country", "Number of climate migrants to accept from this country per year",
                                          "Climate aid to give to this country (CHF) per year", "Value of Swiss trade with this country",
                                          "Extreme weather event", "UN Security Council votes in line with Switzerland"),
                     len = c(4,6,5,3,4,3))
  
  
  
  attributes_to_keep <- as.numeric(unlist(stri_extract_all_regex(unique(out$feature), "[0-9]")))
  attr <- attr[attributes_to_keep,]
  
  attr$end <- cumsum(attr$len)
  attr$start <- attr$end - attr$len + 1
  
  
  len <- ifelse(!is.null(by), length(unique(out$BY)), 1)
  for (i in 1:len){
    add <- (i-1)*length(levels(out$level))
    for (j in 1:length(attr$attribute_levels)){
      tab <- tab %>% pack_rows(attr$attribute_levels[j], attr$start[j] + add, attr$end[j] + add)
    }
  }
  tab <- tab %>% kableExtra::row_spec(length(levels(out$level))*len, extra_latex_after = "\\midrule")
  writeLines(tab, paste0("tables/", name, ".tex"))
}
export_table("Baseline", "", "choice ~ attrib1_lab + attrib2_lab + attrib3_lab + attrib4_lab + attrib5_lab + attrib6_lab", NULL)

res <- rename_vars(res) %>% 
  mutate(feature_lab)

p_baseline <- res %>% 
  ggplot(aes(level, estimate)) + 
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  scale_x_discrete(limits=rev) +
  facet_grid(feature_lab ~ ., scales = "free", space = "free_y") +
  theme_light() + 
  coord_flip() +
  theme(panel.grid = element_blank(),
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = "black", size =9)) +
  geom_hline(yintercept = 0.5, lty = 2, alpha = 0.7, color = "gray50") 

ggsave(p_baseline, file = "plots/p_baseline.pdf", width = 10, height = 10)

## II. 
mm_by <- cj(dat, choice ~ attrib1_lab + attrib2_lab + attrib3_lab + attrib4_lab + attrib5_lab + attrib6_lab, id = ~id, estimate = "mm", by = ~experimental_setup, h0 = 0.5)
export_table("Interaction with experimental setup", "Experimental setup value", "choice ~ attrib1_lab + attrib2_lab + attrib3_lab + attrib4_lab + attrib5_lab + attrib6_lab", "~experimental_setup")

p_exp <- mm_by %>% 
  mutate(
    feature_lab = "",
    feature_lab = ifelse(feature == "attrib1_lab", "Recipient developing country", feature_lab),
    feature_lab = ifelse(feature == "attrib2_lab", "Number of climate\nmigrants to accept\nfrom this country per year", feature_lab),
    feature_lab = ifelse(feature == "attrib3_lab", "Climate aid to give\nto this country\n(CHF) per year", feature_lab),
    feature_lab = ifelse(feature == "attrib4_lab", "Value of Swiss\ntrade with this\ncountry", feature_lab),
    feature_lab = ifelse(feature == "attrib5_lab", "Extreme weather event", feature_lab),
    feature_lab = ifelse(feature == "attrib6_lab", "Percentage of this\ncountry's votes\nin line with Switzerland's\nposition at the UN\nSecurity Council", feature_lab),
    feature_lab = factor(feature_lab, levels = c("Recipient developing country", "Number of climate\nmigrants to accept\nfrom this country per year",
                                                 "Climate aid to give\nto this country\n(CHF) per year", "Value of Swiss\ntrade with this\ncountry",
                                                 "Extreme weather event", "Percentage of this\ncountry's votes\nin line with Switzerland's\nposition at the UN\nSecurity Council"))
  ) %>% 
  mutate(by = "",
         by = ifelse(BY == "ch", "Switzerland", by),
         by = ifelse(BY == "control", "Control", by),
         by = ifelse(BY == "oecd", "OECD", by),
         by = factor(by, levels = c("Control", "Switzerland", "OECD")),
  ) %>% 
  ggplot(aes(level, estimate)) + 
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  scale_x_discrete(limits=rev) +
  facet_grid(feature_lab ~ by, scales = "free_y", space = "free_y") +
  theme_light() + 
  coord_flip() +
  labs(y ="Marginal Means", x = "") +
  theme(panel.grid = element_blank(),
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = "black", size =9)) +
  geom_hline(yintercept = 0.5, lty = 2, alpha = 0.7, color = "gray50") 

ggsave(p_exp, file = "plots/p_exp.pdf", width = 10, height = 10)


## III. 
mm_by <- cj(dat, choice ~ attrib1_lab + attrib2_lab + attrib3_lab + attrib4_lab + attrib5_lab + attrib6_lab, id = ~id, estimate = "mm", h0 = 0.5, by = ~mech_lang_bin)
export_table("Interaction with immigrant language", "Language", "choice ~ attrib1_lab + attrib2_lab + attrib3_lab + attrib4_lab + attrib5_lab + attrib6_lab", "~mech_lang_bin")

p_mech_lang <- mm_by %>% 
  mutate(by = factor(BY, levels = c("No agreement", "Agreement")),
         feature_lab = "",
         feature_lab = ifelse(feature == "attrib1_lab", "Recipient developing country", feature_lab),
         feature_lab = ifelse(feature == "attrib2_lab", "Number of climate\nmigrants to accept\nfrom this country per year", feature_lab),
         feature_lab = ifelse(feature == "attrib3_lab", "Climate aid to give\nto this country\n(CHF) per year", feature_lab),
         feature_lab = ifelse(feature == "attrib4_lab", "Value of Swiss\ntrade with this\ncountry", feature_lab),
         feature_lab = ifelse(feature == "attrib5_lab", "Extreme weather event", feature_lab),
         feature_lab = ifelse(feature == "attrib6_lab", "Percentage of this\ncountry's votes\nin line with Switzerland's\nposition at the UN\nSecurity Council", feature_lab),
         feature_lab = factor(feature_lab, levels = c("Recipient developing country", "Number of climate\nmigrants to accept\nfrom this country per year",
                                                      "Climate aid to give\nto this country\n(CHF) per year", "Value of Swiss\ntrade with this\ncountry",
                                                      "Extreme weather event", "Percentage of this\ncountry's votes\nin line with Switzerland's\nposition at the UN\nSecurity Council"))
  ) %>% 
  ggplot(aes(level, estimate)) + 
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  scale_x_discrete(limits=rev) +
  facet_grid(feature_lab ~ by, scales = "free_y", 
             space = "free_y"
  ) +
  theme_light() + 
  coord_flip() +
  labs(y ="Marginal Means", x = "", subtitle = "Immigrants from countries whose languages are similar to those spoken in Switzerland will integrate more easily in Switzerland\n 
       No agreement ('Disagree strongly', 'Disagree', 'Neither nor'), Agreement ('Agree', 'Agree strongly')") +
  theme(panel.grid = element_blank(),
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = "black", size =9)) +
  geom_hline(yintercept = 0.5, lty = 2, alpha = 0.7, color = "gray50") 

ggsave(p_mech_lang, file = "plots/p_mech_lang.pdf", width = 10, height = 10)

## IV. 
mm_by <- cj(dat, choice ~ attrib1_lab + attrib2_lab + attrib3_lab + attrib4_lab + attrib5_lab + attrib6_lab, id = ~id, estimate = "mm", h0 = 0.5, by = ~mech_cult_bin)
export_table("Interaction with culture", "Perceived similarity of immigrants", "choice ~ attrib1_lab + attrib2_lab + attrib3_lab + attrib4_lab + attrib5_lab + attrib6_lab", "~mech_cult_bin")

p_mech_cult <- mm_by %>% 
  mutate(by = factor(BY, levels = c("No agreement", "Agreement")),
         feature_lab = "",
         feature_lab = ifelse(feature == "attrib1_lab", "Recipient developing country", feature_lab),
         feature_lab = ifelse(feature == "attrib2_lab", "Number of climate\nmigrants to accept\nfrom this country per year", feature_lab),
         feature_lab = ifelse(feature == "attrib3_lab", "Climate aid to give\nto this country\n(CHF) per year", feature_lab),
         feature_lab = ifelse(feature == "attrib4_lab", "Value of Swiss\ntrade with this\ncountry", feature_lab),
         feature_lab = ifelse(feature == "attrib5_lab", "Extreme weather event", feature_lab),
         feature_lab = ifelse(feature == "attrib6_lab", "Percentage of this\ncountry's votes\nin line with Switzerland's\nposition at the UN\nSecurity Council", feature_lab),
         feature_lab = factor(feature_lab, levels = c("Recipient developing country", "Number of climate\nmigrants to accept\nfrom this country per year",
                                                      "Climate aid to give\nto this country\n(CHF) per year", "Value of Swiss\ntrade with this\ncountry",
                                                      "Extreme weather event", "Percentage of this\ncountry's votes\nin line with Switzerland's\nposition at the UN\nSecurity Council"))
  ) %>% 
  ggplot(aes(level, estimate)) + 
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  scale_x_discrete(limits=rev) +
  facet_grid(feature_lab ~ by, scales = "free_y", 
             space = "free_y"
  ) +
  theme_light() + 
  coord_flip() +
  labs(y ="Marginal Means", x = "", subtitle = "Immigrants from countries that are culturally similar to Switzerland will integrate more easily in Switzerland\n 
       No agreement ('Disagree strongly', 'Disagree', 'Neither nor'), Agreement ('Agree', 'Agree strongly')") +
  theme(panel.grid = element_blank(),
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = "black", size =9)) +
  geom_hline(yintercept = 0.5, lty = 2, alpha = 0.7, color = "gray50") 

ggsave(p_mech_cult, file = "plots/p_mech_cult.pdf", width = 10, height = 10)

## IV. 
mm_by <- cj(dat, choice ~ attrib1_lab + attrib2_lab + attrib3_lab + attrib4_lab + attrib5_lab + attrib6_lab, id = ~id, estimate = "mm", h0 = 0.5, by = ~mech_proud_bin)
export_table("Interaction with pride",  "Pride of being Swiss", "choice ~ attrib1_lab + attrib2_lab + attrib3_lab + attrib4_lab + attrib5_lab + attrib6_lab", "~mech_cult_bin")

p_mech_proud <- mm_by %>% 
  mutate(by = factor(BY, levels = c("No agreement", "Agreement")),
         feature_lab = "",
         feature_lab = ifelse(feature == "attrib1_lab", "Recipient developing country", feature_lab),
         feature_lab = ifelse(feature == "attrib2_lab", "Number of climate\nmigrants to accept\nfrom this country per year", feature_lab),
         feature_lab = ifelse(feature == "attrib3_lab", "Climate aid to give\nto this country\n(CHF) per year", feature_lab),
         feature_lab = ifelse(feature == "attrib4_lab", "Value of Swiss\ntrade with this\ncountry", feature_lab),
         feature_lab = ifelse(feature == "attrib5_lab", "Extreme weather event", feature_lab),
         feature_lab = ifelse(feature == "attrib6_lab", "Percentage of this\ncountry's votes\nin line with Switzerland's\nposition at the UN\nSecurity Council", feature_lab),
         feature_lab = factor(feature_lab, levels = c("Recipient developing country", "Number of climate\nmigrants to accept\nfrom this country per year",
                                                      "Climate aid to give\nto this country\n(CHF) per year", "Value of Swiss\ntrade with this\ncountry",
                                                      "Extreme weather event", "Percentage of this\ncountry's votes\nin line with Switzerland's\nposition at the UN\nSecurity Council"))
  ) %>% 
  ggplot(aes(level, estimate)) + 
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  scale_x_discrete(limits=rev) +
  facet_grid(feature_lab ~ by, scales = "free_y", 
             space = "free_y"
  ) +
  theme_light() + 
  coord_flip() +
  labs(y ="Marginal Means", x = "", subtitle = "I am proud of Swiss culture and history\n 
       No agreement ('Disagree strongly', 'Disagree', 'Neither nor'), Agreement ('Agree', 'Agree strongly')") +
  theme(panel.grid = element_blank(),
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = "black", size =9)) +
  geom_hline(yintercept = 0.5, lty = 2, alpha = 0.7, color = "gray50") 

ggsave(p_mech_proud, file = "plots/p_mech_proud.pdf", width = 10, height = 10)


## IV. 
mm_by <- cj(dat, choice ~ attrib1_lab + attrib2_lab + attrib3_lab + attrib4_lab + attrib5_lab + attrib6_lab, id = ~id, estimate = "mm", h0 = 0.5, by = ~mech_devel_bin)
export_table("Interaction with belief about development", "", "choice ~ attrib1_lab + attrib2_lab + attrib3_lab + attrib4_lab + attrib5_lab + attrib6_lab", "~mech_devel_bin")

p_mech_develop <- mm_by %>% 
  mutate(by = factor(BY, levels = c("No agreement", "Agreement")),
         feature_lab = "",
         feature_lab = ifelse(feature == "attrib1_lab", "Recipient developing country", feature_lab),
         feature_lab = ifelse(feature == "attrib2_lab", "Number of climate\nmigrants to accept\nfrom this country per year", feature_lab),
         feature_lab = ifelse(feature == "attrib3_lab", "Climate aid to give\nto this country\n(CHF) per year", feature_lab),
         feature_lab = ifelse(feature == "attrib4_lab", "Value of Swiss\ntrade with this\ncountry", feature_lab),
         feature_lab = ifelse(feature == "attrib5_lab", "Extreme weather event", feature_lab),
         feature_lab = ifelse(feature == "attrib6_lab", "Percentage of this\ncountry's votes\nin line with Switzerland's\nposition at the UN\nSecurity Council", feature_lab),
         feature_lab = factor(feature_lab, levels = c("Recipient developing country", "Number of climate\nmigrants to accept\nfrom this country per year",
                                                      "Climate aid to give\nto this country\n(CHF) per year", "Value of Swiss\ntrade with this\ncountry",
                                                      "Extreme weather event", "Percentage of this\ncountry's votes\nin line with Switzerland's\nposition at the UN\nSecurity Council"))
  ) %>% 
  ggplot(aes(level, estimate)) + 
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  scale_x_discrete(limits=rev) +
  facet_grid(feature_lab ~ by, scales = "free_y", 
             space = "free_y"
  ) +
  theme_light() + 
  coord_flip() +
  labs(y ="Marginal Means", x = "", subtitle = "I believe that development supports development\n 
       No agreement ('Disagree strongly', 'Disagree', 'Neither nor'), Agreement ('Agree', 'Agree strongly')") +
  theme(panel.grid = element_blank(),
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = "black", size =9)) +
  geom_hline(yintercept = 0.5, lty = 2, alpha = 0.7, color = "gray50") 

ggsave(p_mech_develop, file = "plots/p_mech_develop.pdf", width = 10, height = 10)


## IV. 
mm_by <- cj(dat, choice ~ attrib1_lab + attrib2_lab + attrib3_lab + attrib4_lab + attrib5_lab + attrib6_lab, id = ~id, estimate = "mm", h0 = 0.5, by = ~ immi_pc_quart)
export_table("Interaction with immigration p.c.", "Immigration p.c.", "choice ~ attrib1_lab + attrib2_lab + attrib3_lab + attrib4_lab + attrib5_lab + attrib6_lab", "~immi_pc_quart")

p_immig_pc <- mm_by %>% 
  mutate(by = factor(BY, levels = c("1st quartile", "1st-2nd quartile",  "2nd-3rd quartile", "above 3rd quartile")),
         feature_lab = "",
         feature_lab = ifelse(feature == "attrib1_lab", "Recipient developing country", feature_lab),
         feature_lab = ifelse(feature == "attrib2_lab", "Number of climate\nmigrants to accept\nfrom this country per year", feature_lab),
         feature_lab = ifelse(feature == "attrib3_lab", "Climate aid to give\nto this country\n(CHF) per year", feature_lab),
         feature_lab = ifelse(feature == "attrib4_lab", "Value of Swiss\ntrade with this\ncountry", feature_lab),
         feature_lab = ifelse(feature == "attrib5_lab", "Extreme weather event", feature_lab),
         feature_lab = ifelse(feature == "attrib6_lab", "Percentage of this\ncountry's votes\nin line with Switzerland's\nposition at the UN\nSecurity Council", feature_lab),
         feature_lab = factor(feature_lab, levels = c("Recipient developing country", "Number of climate\nmigrants to accept\nfrom this country per year",
                                                      "Climate aid to give\nto this country\n(CHF) per year", "Value of Swiss\ntrade with this\ncountry",
                                                      "Extreme weather event", "Percentage of this\ncountry's votes\nin line with Switzerland's\nposition at the UN\nSecurity Council"))
  ) %>% 
  ggplot(aes(level, estimate)) + 
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  scale_x_discrete(limits=rev) +
  facet_grid(feature_lab ~ by, scales = "free_y", 
             space = "free_y"
  ) +
  theme_light() + 
  coord_flip() +
  labs(y ="Marginal Means", x = "") +
  theme(panel.grid = element_blank(),
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = "black", size =9)) +
  geom_hline(yintercept = 0.5, lty = 2, alpha = 0.7, color = "gray50") 

ggsave(p_immig_pc, file = "plots/p_immig_pc.pdf", width = 10, height = 10)

## IV. 
mm_by <- cj(dat, choice ~ attrib1_lab + attrib2_lab + attrib3_lab + attrib4_lab + attrib5_lab + attrib6_lab, id = ~id, estimate = "mm", h0 = 0.5, by = ~ immi_quart)
export_table("Interaction with immigration", "Immigration (municipality)", "choice ~ attrib1_lab + attrib2_lab + attrib3_lab + attrib4_lab + attrib5_lab + attrib6_lab", "~immi_quart")

p_immig <- mm_by %>% 
  mutate(by = factor(BY, levels = c("1st quartile", "1st-2nd quartile",  "2nd-3rd quartile", "above 3rd quartile")),
         feature_lab = "",
         feature_lab = ifelse(feature == "attrib1_lab", "Recipient developing country", feature_lab),
         feature_lab = ifelse(feature == "attrib2_lab", "Number of climate\nmigrants to accept\nfrom this country per year", feature_lab),
         feature_lab = ifelse(feature == "attrib3_lab", "Climate aid to give\nto this country\n(CHF) per year", feature_lab),
         feature_lab = ifelse(feature == "attrib4_lab", "Value of Swiss\ntrade with this\ncountry", feature_lab),
         feature_lab = ifelse(feature == "attrib5_lab", "Extreme weather event", feature_lab),
         feature_lab = ifelse(feature == "attrib6_lab", "Percentage of this\ncountry's votes\nin line with Switzerland's\nposition at the UN\nSecurity Council", feature_lab),
         feature_lab = factor(feature_lab, levels = c("Recipient developing country", "Number of climate\nmigrants to accept\nfrom this country per year",
                                                      "Climate aid to give\nto this country\n(CHF) per year", "Value of Swiss\ntrade with this\ncountry",
                                                      "Extreme weather event", "Percentage of this\ncountry's votes\nin line with Switzerland's\nposition at the UN\nSecurity Council"))
  ) %>% 
  ggplot(aes(level, estimate)) + 
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  scale_x_discrete(limits=rev) +
  facet_grid(feature_lab ~ by, scales = "free_y", 
             space = "free_y"
  ) +
  theme_light() + 
  coord_flip() +
  labs(y ="Marginal Means", x = "") +
  theme(panel.grid = element_blank(),
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = "black", size =9)) +
  geom_hline(yintercept = 0.5, lty = 2, alpha = 0.7, color = "gray50") 

ggsave(p_immig, file = "plots/p_immig.pdf", width = 10, height = 10)

## IV. 
mm_by <- cj(dat, choice ~ attrib1_lab + attrib2_lab + attrib3_lab + attrib4_lab + attrib5_lab + attrib6_lab, id = ~id, estimate = "mm", h0 = 0.5, by = ~ left_right_bin)
export_table("Interaction with left-right", "Left-right", "choice ~ attrib1_lab + attrib2_lab + attrib3_lab + attrib4_lab + attrib5_lab + attrib6_lab", "~left_right_bin")

p_left_right <- mm_by %>% 
  mutate(by = factor(BY, levels = c("Left", "Centrist", "Right")),
         feature_lab = "",
         feature_lab = ifelse(feature == "attrib1_lab", "Recipient developing country", feature_lab),
         feature_lab = ifelse(feature == "attrib2_lab", "Number of climate\nmigrants to accept\nfrom this country per year", feature_lab),
         feature_lab = ifelse(feature == "attrib3_lab", "Climate aid to give\nto this country\n(CHF) per year", feature_lab),
         feature_lab = ifelse(feature == "attrib4_lab", "Value of Swiss\ntrade with this\ncountry", feature_lab),
         feature_lab = ifelse(feature == "attrib5_lab", "Extreme weather event", feature_lab),
         feature_lab = ifelse(feature == "attrib6_lab", "Percentage of this\ncountry's votes\nin line with Switzerland's\nposition at the UN\nSecurity Council", feature_lab),
         feature_lab = factor(feature_lab, levels = c("Recipient developing country", "Number of climate\nmigrants to accept\nfrom this country per year",
                                                      "Climate aid to give\nto this country\n(CHF) per year", "Value of Swiss\ntrade with this\ncountry",
                                                      "Extreme weather event", "Percentage of this\ncountry's votes\nin line with Switzerland's\nposition at the UN\nSecurity Council"))
  ) %>% 
  ggplot(aes(level, estimate)) + 
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  scale_x_discrete(limits=rev) +
  facet_grid(feature_lab ~ by, scales = "free_y", 
             space = "free_y"
  ) +
  theme_light() + 
  coord_flip() +
  labs(y ="Marginal Means", x = "", subtitle = "Lef-right scale: Left (1-4), Centrist (5-6), Right (7-11)") +
  theme(panel.grid = element_blank(),
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = "black", size =9)) +
  geom_hline(yintercept = 0.5, lty = 2, alpha = 0.7, color = "gray50") 

ggsave(p_left_right, file = "plots/p_left_right.pdf", width = 10, height = 10)

plot_left_right <- dat %>% 
  count(left_right_bin) %>% 
  ggplot(aes(left_right_bin, n)) +
  geom_col() + 
  labs(y = "", x = "Number of Observations", subtitle = "Lef-right scale: Left (1-4), Centrist (5-6), Right (7-11)") +
  theme_light()
ggsave(plot_left_right, file = "plots/left_right.pdf")

dat <- dat %>% mutate(attrib2_lab = as.factor(attrib2_lab))
mm_by <- cj(dat, choice ~ attrib1_lab + attrib3_lab + attrib4_lab + attrib5_lab + attrib6_lab, id = ~id, estimate = "mm", h0 = 0.5, by = ~attrib2_lab)
export_table("Interaction with number of climate migrants", "Number of climate migrants", "choice ~ attrib1_lab + attrib3_lab + attrib4_lab + attrib5_lab + attrib6_lab", "~attrib2_lab")

p_int_n_migr <- mm_by %>% 
  mutate(
    # by = BY,
    by = factor(BY, levels = c("0", "250", "500", "750", "1,000", "1,250")),
    feature_lab = "",
    feature_lab = ifelse(feature == "attrib1_lab", "Recipient developing country", feature_lab),
    feature_lab = ifelse(feature == "attrib2_lab", "Number of climate\nmigrants to accept\nfrom this country per year", feature_lab),
    feature_lab = ifelse(feature == "attrib3_lab", "Climate aid to give\nto this country\n(CHF) per year", feature_lab),
    feature_lab = ifelse(feature == "attrib4_lab", "Value of Swiss\ntrade with this\ncountry", feature_lab),
    feature_lab = ifelse(feature == "attrib5_lab", "Extreme weather event", feature_lab),
    feature_lab = ifelse(feature == "attrib6_lab", "Percentage of this\ncountry's votes\nin line with Switzerland's\nposition at the UN\nSecurity Council", feature_lab),
    feature_lab = factor(feature_lab, levels = c("Recipient developing country", "Number of climate\nmigrants to accept\nfrom this country per year",
                                                 "Climate aid to give\nto this country\n(CHF) per year", "Value of Swiss\ntrade with this\ncountry",
                                                 "Extreme weather event", "Percentage of this\ncountry's votes\nin line with Switzerland's\nposition at the UN\nSecurity Council"))
  ) %>% 
  ggplot(aes(level, estimate)) + 
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  scale_x_discrete(limits=rev) +
  facet_grid(feature_lab ~ by, scales = "free_y", 
             space = "free_y"
  ) +
  theme_light() + 
  coord_flip() +
  labs(y ="Marginal Means", x = "") +
  theme(panel.grid = element_blank(),
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = "black", size =9)) +
  geom_hline(yintercept = 0.5, lty = 2, alpha = 0.7, color = "gray50") 
ggsave(p_int_n_migr, file = "plots/p_int_n_migr.pdf", width = 10, height = 10)

dat <- dat %>% mutate(attrib6_lab = as.factor(attrib6_lab))
mm_by <- cj(dat, choice ~ attrib1_lab + attrib2_lab + attrib3_lab + attrib4_lab + attrib5_lab , id = ~id, estimate = "mm", h0 = 0.5, by = ~ attrib6_lab)
export_table("Interaction with UN Security Council votes in line with Switzerland", "Votes in line with Switzerland", "choice ~ attrib1_lab + attrib2_lab + attrib3_lab + attrib4_lab + attrib5_lab ", "~attrib6_lab")

p_int_UN <- mm_by %>% 
  mutate(
    by = BY,
    # by = factor(BY, levels = c("0", "250", "500", "750", "1,000", "1,250")),
    feature_lab = "",
    feature_lab = ifelse(feature == "attrib1_lab", "Recipient developing country", feature_lab),
    feature_lab = ifelse(feature == "attrib2_lab", "Number of climate\nmigrants to accept\nfrom this country per year", feature_lab),
    feature_lab = ifelse(feature == "attrib3_lab", "Climate aid to give\nto this country\n(CHF) per year", feature_lab),
    feature_lab = ifelse(feature == "attrib4_lab", "Value of Swiss\ntrade with this\ncountry", feature_lab),
    feature_lab = ifelse(feature == "attrib5_lab", "Extreme weather event", feature_lab),
    feature_lab = ifelse(feature == "attrib6_lab", "Percentage of this\ncountry's votes\nin line with Switzerland's\nposition at the UN\nSecurity Council", feature_lab),
    feature_lab = factor(feature_lab, levels = c("Recipient developing country", "Number of climate\nmigrants to accept\nfrom this country per year",
                                                 "Climate aid to give\nto this country\n(CHF) per year", "Value of Swiss\ntrade with this\ncountry",
                                                 "Extreme weather event", "Percentage of this\ncountry's votes\nin line with Switzerland's\nposition at the UN\nSecurity Council"))
  ) %>% 
  ggplot(aes(level, estimate)) + 
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  scale_x_discrete(limits=rev) +
  facet_grid(feature_lab ~ by, scales = "free_y", 
             space = "free_y"
  ) +
  theme_light() + 
  coord_flip() +
  labs(y ="Marginal Means", x = "") +
  theme(panel.grid = element_blank(),
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = "black", size =9)) +
  geom_hline(yintercept = 0.5, lty = 2, alpha = 0.7, color = "gray50") 
ggsave(p_int_UN, file = "plots/p_int_UN.pdf", width = 10, height = 10)


dat <- dat %>% mutate(attrib4_lab = as.factor(attrib4_lab))
mm_by <- cj(dat, choice ~ attrib1_lab + attrib2_lab + attrib3_lab + attrib5_lab + attrib6_lab, id = ~id, estimate = "mm", h0 = 0.5, by = ~ attrib4_lab)
export_table("Interaction with value of Swiss trade with this country", "Value of Swiss trade", "choice ~ attrib1_lab + attrib2_lab + attrib3_lab + attrib5_lab + attrib6_lab", "~attrib4_lab")


p_int_trade <- mm_by %>% 
  mutate(
    by = factor(BY, levels = c("0 million ", "500 million ", "1,000 million ")),
    feature_lab = "",
    feature_lab = ifelse(feature == "attrib1_lab", "Recipient developing country", feature_lab),
    feature_lab = ifelse(feature == "attrib2_lab", "Number of climate\nmigrants to accept\nfrom this country per year", feature_lab),
    feature_lab = ifelse(feature == "attrib3_lab", "Climate aid to give\nto this country\n(CHF) per year", feature_lab),
    feature_lab = ifelse(feature == "attrib4_lab", "Value of Swiss\ntrade with this\ncountry", feature_lab),
    feature_lab = ifelse(feature == "attrib5_lab", "Extreme weather event", feature_lab),
    feature_lab = ifelse(feature == "attrib6_lab", "Percentage of this\ncountry's votes\nin line with Switzerland's\nposition at the UN\nSecurity Council", feature_lab),
    feature_lab = factor(feature_lab, levels = c("Recipient developing country", "Number of climate\nmigrants to accept\nfrom this country per year",
                                                 "Climate aid to give\nto this country\n(CHF) per year", "Value of Swiss\ntrade with this\ncountry",
                                                 "Extreme weather event", "Percentage of this\ncountry's votes\nin line with Switzerland's\nposition at the UN\nSecurity Council"))
  ) %>% 
  ggplot(aes(level, estimate)) + 
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  scale_x_discrete(limits=rev) +
  facet_grid(feature_lab ~ by, scales = "free_y", 
             space = "free_y"
  ) +
  theme_light() + 
  coord_flip() +
  labs(y ="Marginal Means", x = "") +
  theme(panel.grid = element_blank(),
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = "black", size =9)) +
  geom_hline(yintercept = 0.5, lty = 2, alpha = 0.7, color = "gray50") 
ggsave(p_int_trade, file = "plots/p_int_trade.pdf", width = 10, height = 10)


dat <- dat %>% 
  mutate(
    # urban_rural = ifelse(urban_rural %in% c("intermediary", "rural"), "rural", "urban"),
    # urban_rural = factor(urban_rural, levels = c("urban", "rural"),
    urban_rural = factor(urban_rural, levels = c("urban", "intermediary", "rural"))
  )
mm_by <- cj(dat, choice ~ attrib1_lab + attrib2_lab + attrib3_lab + attrib4_lab + attrib5_lab + attrib6_lab , id = ~id, estimate = "mm", h0 = 0.5, by = ~ urban_rural)
export_table("Interaction with urban rural", "Urban rural", "choice ~ attrib1_lab + attrib3_lab + attrib4_lab + attrib5_lab + attrib6_lab", "~urban_rural")


p_uban_rural <- mm_by %>% 
  mutate(
    by = factor(BY, levels = c("urban", "intermediary", "rural")),
    feature_lab = "",
    feature_lab = ifelse(feature == "attrib1_lab", "Recipient developing country", feature_lab),
    feature_lab = ifelse(feature == "attrib2_lab", "Number of climate\nmigrants to accept\nfrom this country per year", feature_lab),
    feature_lab = ifelse(feature == "attrib3_lab", "Climate aid to give\nto this country\n(CHF) per year", feature_lab),
    feature_lab = ifelse(feature == "attrib4_lab", "Value of Swiss\ntrade with this\ncountry", feature_lab),
    feature_lab = ifelse(feature == "attrib5_lab", "Extreme weather event", feature_lab),
    feature_lab = ifelse(feature == "attrib6_lab", "Percentage of this\ncountry's votes\nin line with Switzerland's\nposition at the UN\nSecurity Council", feature_lab),
    feature_lab = factor(feature_lab, levels = c("Recipient developing country", "Number of climate\nmigrants to accept\nfrom this country per year",
                                                 "Climate aid to give\nto this country\n(CHF) per year", "Value of Swiss\ntrade with this\ncountry",
                                                 "Extreme weather event", "Percentage of this\ncountry's votes\nin line with Switzerland's\nposition at the UN\nSecurity Council"))
  ) %>% 
  ggplot(aes(level, estimate)) + 
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  scale_x_discrete(limits=rev) +
  facet_grid(feature_lab ~ by, scales = "free_y", 
             space = "free_y"
  ) +
  theme_light() + 
  coord_flip() +
  labs(y ="Marginal Means", x = "", subtitle = "Interaction with the percentage of this country's votes in line with Switzerland's position at the UN Security Council") +
  theme(panel.grid = element_blank(),
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = "black", size =9)) +
  geom_hline(yintercept = 0.5, lty = 2, alpha = 0.7, color = "gray50") 
ggsave(p_uban_rural, file = "plots/p_uban_rural.pdf", width = 10, height = 10)

dat <- dat %>% 
  mutate(
    urban_rural = ifelse(urban_rural %in% c("intermediary", "rural"), "rural", "urban"),
    urban_rural = factor(urban_rural, levels = c("urban", "rural")),
  )

dat <- dat %>% 
  mutate(age = ifelse(age %in% c("18-24", "25-34"), "18-34", age),
         age = ifelse(age %in% c("35-44", "45-54", "55-64"), "35-64", age),
         age = ifelse(age %in% c("65-74", "75-84", ">84"), ">65", age),
         age = factor(age, levels = c("18-34", "35-64", ">65"))
  )
mm_by <- cj(dat, choice ~ attrib1_lab + attrib2_lab + attrib3_lab + attrib4_lab + attrib5_lab + attrib6_lab , id = ~id, estimate = "mm", h0 = 0.5, by = ~age)
export_table("Interaction with age","Age", "choice ~ attrib1_lab + attrib2_lab + attrib3_lab + attrib4_lab + attrib5_lab + attrib6_lab", "~age")


p_age <- mm_by %>% 
  mutate(
    # by = BY,
    by = factor(BY, levels =  c("18-34", "35-64", ">65")),
    feature_lab = "",
    feature_lab = ifelse(feature == "attrib1_lab", "Recipient developing country", feature_lab),
    feature_lab = ifelse(feature == "attrib2_lab", "Number of climate\nmigrants to accept\nfrom this country per year", feature_lab),
    feature_lab = ifelse(feature == "attrib3_lab", "Climate aid to give\nto this country\n(CHF) per year", feature_lab),
    feature_lab = ifelse(feature == "attrib4_lab", "Value of Swiss\ntrade with this\ncountry", feature_lab),
    feature_lab = ifelse(feature == "attrib5_lab", "Extreme weather event", feature_lab),
    feature_lab = ifelse(feature == "attrib6_lab", "Percentage of this\ncountry's votes\nin line with Switzerland's\nposition at the UN\nSecurity Council", feature_lab),
    feature_lab = factor(feature_lab, levels = c("Recipient developing country", "Number of climate\nmigrants to accept\nfrom this country per year",
                                                 "Climate aid to give\nto this country\n(CHF) per year", "Value of Swiss\ntrade with this\ncountry",
                                                 "Extreme weather event", "Percentage of this\ncountry's votes\nin line with Switzerland's\nposition at the UN\nSecurity Council"))
  ) %>% 
  ggplot(aes(level, estimate)) + 
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  scale_x_discrete(limits=rev) +
  facet_grid(feature_lab ~ by, scales = "free_y", 
             space = "free_y"
  ) +
  theme_light() + 
  coord_flip() +
  labs(y ="Marginal Means", x = "", subtitle = "Interaction with age") +
  theme(panel.grid = element_blank(),
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = "black", size =9)) +
  geom_hline(yintercept = 0.5, lty = 2, alpha = 0.7, color = "gray50") 
ggsave(p_age, file = "plots/p_age.pdf", width = 10, height = 10)

dat <- dat %>% 
  mutate(
    income = ifelse(income %in% c("<23,000 CHF", "23,000-62,000 CHF"), "<62,000 CHF", income),
    income = ifelse(income %in% c("62,000-92,000 CHF", "92,000-123,000 CHF"), "62,000-123,000 CHF", income),
    income = ifelse(income %in% c("123,000-154,000 CHF", "154'000-231,000 CHF", ">231,000"), ">123,000 CHF", income),
    income = factor(income, levels = c("<62,000 CHF", "62,000-123,000 CHF", ">123,000 CHF")))
mm_by <- cj(dat, choice ~ attrib1_lab + attrib2_lab + attrib3_lab + attrib4_lab + attrib5_lab + attrib6_lab , id = ~id, estimate = "mm", h0 = 0.5, by = ~income)
export_table("Interaction with income", "Income", "choice ~ attrib1_lab + attrib2_lab + attrib3_lab + attrib4_lab + attrib5_lab + attrib6_lab", "~income")


p_income <- mm_by %>% 
  mutate(
    # by = BY,
    by = factor(BY, levels =  c("<62,000 CHF", "62,000-123,000 CHF", ">123,000 CHF")),
    feature_lab = "",
    feature_lab = ifelse(feature == "attrib1_lab", "Recipient developing country", feature_lab),
    feature_lab = ifelse(feature == "attrib2_lab", "Number of climate\nmigrants to accept\nfrom this country per year", feature_lab),
    feature_lab = ifelse(feature == "attrib3_lab", "Climate aid to give\nto this country\n(CHF) per year", feature_lab),
    feature_lab = ifelse(feature == "attrib4_lab", "Value of Swiss\ntrade with this\ncountry", feature_lab),
    feature_lab = ifelse(feature == "attrib5_lab", "Extreme weather event", feature_lab),
    feature_lab = ifelse(feature == "attrib6_lab", "Percentage of this\ncountry's votes\nin line with Switzerland's\nposition at the UN\nSecurity Council", feature_lab),
    feature_lab = factor(feature_lab, levels = c("Recipient developing country", "Number of climate\nmigrants to accept\nfrom this country per year",
                                                 "Climate aid to give\nto this country\n(CHF) per year", "Value of Swiss\ntrade with this\ncountry",
                                                 "Extreme weather event", "Percentage of this\ncountry's votes\nin line with Switzerland's\nposition at the UN\nSecurity Council"))
  ) %>% 
  ggplot(aes(level, estimate)) + 
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  scale_x_discrete(limits=rev) +
  facet_grid(feature_lab ~ by, scales = "free_y", 
             space = "free_y"
  ) +
  theme_light() + 
  coord_flip() +
  labs(y ="Marginal Means", x = "") +
  theme(panel.grid = element_blank(),
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = "black", size =9)) +
  geom_hline(yintercept = 0.5, lty = 2, alpha = 0.7, color = "gray50") 
ggsave(p_income, file = "plots/p_income.pdf", width = 10, height = 10)

dat <- dat %>% mutate(educ = factor(educ, levels = c("University", "Professional Education", "Vocational", "Other")))
mm_by <- cj(dat, choice ~ attrib1_lab + attrib2_lab + attrib3_lab + attrib4_lab + attrib5_lab + attrib6_lab , id = ~id, estimate = "mm", h0 = 0.5, by = ~educ)
export_table("Interaction with education", "Education", "choice ~ attrib1_lab + attrib2_lab + attrib3_lab + attrib4_lab + attrib5_lab + attrib6_lab", "~educ")

p_educ <- mm_by %>% 
  mutate(
    # by = BY,
    by = factor(BY, levels = c("University", "Professional Education", "Vocational", "Other")),
    feature_lab = "",
    feature_lab = ifelse(feature == "attrib1_lab", "Recipient developing country", feature_lab),
    feature_lab = ifelse(feature == "attrib2_lab", "Number of climate\nmigrants to accept\nfrom this country per year", feature_lab),
    feature_lab = ifelse(feature == "attrib3_lab", "Climate aid to give\nto this country\n(CHF) per year", feature_lab),
    feature_lab = ifelse(feature == "attrib4_lab", "Value of Swiss\ntrade with this\ncountry", feature_lab),
    feature_lab = ifelse(feature == "attrib5_lab", "Extreme weather event", feature_lab),
    feature_lab = ifelse(feature == "attrib6_lab", "Percentage of this\ncountry's votes\nin line with Switzerland's\nposition at the UN\nSecurity Council", feature_lab),
    feature_lab = factor(feature_lab, levels = c("Recipient developing country", "Number of climate\nmigrants to accept\nfrom this country per year",
                                                 "Climate aid to give\nto this country\n(CHF) per year", "Value of Swiss\ntrade with this\ncountry",
                                                 "Extreme weather event", "Percentage of this\ncountry's votes\nin line with Switzerland's\nposition at the UN\nSecurity Council"))
  ) %>% 
  ggplot(aes(level, estimate)) + 
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  scale_x_discrete(limits=rev) +
  facet_grid(feature_lab ~ by, scales = "free_y", 
             space = "free_y"
  ) +
  theme_light() + 
  coord_flip() +
  labs(y ="Marginal Means", x = "") +
  theme(panel.grid = element_blank(),
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = "black", size =9)) +
  geom_hline(yintercept = 0.5, lty = 2, alpha = 0.7, color = "gray50") 
ggsave(p_educ, file = "plots/p_educ.pdf", width = 10, height = 10)

dat <- dat %>% 
  mutate(
    employment = as.factor(employment)
  )
mm_by <- cj(dat, choice ~ attrib1_lab + attrib2_lab + attrib3_lab + attrib4_lab + attrib5_lab + attrib6_lab , id = ~id, estimate = "mm", h0 = 0.5, by = ~employment)
export_table("Interaction with employment", "Employment", "choice ~ attrib1_lab + attrib2_lab + attrib3_lab + attrib4_lab + attrib5_lab + attrib6_lab", "~employment")


p_employment <- mm_by %>% 
  mutate(
    # by = BY,
    by = factor(BY, levels = c("Employed", "Not employed")),
    feature_lab = "",
    feature_lab = ifelse(feature == "attrib1_lab", "Recipient developing country", feature_lab),
    feature_lab = ifelse(feature == "attrib2_lab", "Number of climate\nmigrants to accept\nfrom this country per year", feature_lab),
    feature_lab = ifelse(feature == "attrib3_lab", "Climate aid to give\nto this country\n(CHF) per year", feature_lab),
    feature_lab = ifelse(feature == "attrib4_lab", "Value of Swiss\ntrade with this\ncountry", feature_lab),
    feature_lab = ifelse(feature == "attrib5_lab", "Extreme weather event", feature_lab),
    feature_lab = ifelse(feature == "attrib6_lab", "Percentage of this\ncountry's votes\nin line with Switzerland's\nposition at the UN\nSecurity Council", feature_lab),
    feature_lab = factor(feature_lab, levels = c("Recipient developing country", "Number of climate\nmigrants to accept\nfrom this country per year",
                                                 "Climate aid to give\nto this country\n(CHF) per year", "Value of Swiss\ntrade with this\ncountry",
                                                 "Extreme weather event", "Percentage of this\ncountry's votes\nin line with Switzerland's\nposition at the UN\nSecurity Council"))
  ) %>% 
  ggplot(aes(level, estimate)) + 
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  scale_x_discrete(limits=rev) +
  facet_grid(feature_lab ~ by, scales = "free_y", 
             space = "free_y"
  ) +
  theme_light() + 
  coord_flip() +
  labs(y ="Marginal Means", x = "") +
  theme(panel.grid = element_blank(),
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = "black", size =9)) +
  geom_hline(yintercept = 0.5, lty = 2, alpha = 0.7, color = "gray50") 
ggsave(p_employment, file = "plots/p_employment.pdf", width = 10, height = 10)

dat <- dat %>% 
  mutate(
    UserLanguage = as.factor(UserLanguage)
  )
mm_by <- cj(dat, choice ~ attrib1_lab + attrib2_lab + attrib3_lab + attrib4_lab + attrib5_lab + attrib6_lab , id = ~id, estimate = "mm", h0 = 0.5, by = ~UserLanguage)
export_table("Interaction with language", "Language", "choice ~ attrib1_lab + attrib2_lab + attrib3_lab + attrib4_lab + attrib5_lab + attrib6_lab", "~UserLanguage")

p_language <- mm_by %>% 
  mutate(
    # by = BY,
    by = factor(BY, levels = c("DE", "FR", "IT")),
    feature_lab = "",
    feature_lab = ifelse(feature == "attrib1_lab", "Recipient developing country", feature_lab),
    feature_lab = ifelse(feature == "attrib2_lab", "Number of climate\nmigrants to accept\nfrom this country per year", feature_lab),
    feature_lab = ifelse(feature == "attrib3_lab", "Climate aid to give\nto this country\n(CHF) per year", feature_lab),
    feature_lab = ifelse(feature == "attrib4_lab", "Value of Swiss\ntrade with this\ncountry", feature_lab),
    feature_lab = ifelse(feature == "attrib5_lab", "Extreme weather event", feature_lab),
    feature_lab = ifelse(feature == "attrib6_lab", "Percentage of this\ncountry's votes\nin line with Switzerland's\nposition at the UN\nSecurity Council", feature_lab),
    feature_lab = factor(feature_lab, levels = c("Recipient developing country", "Number of climate\nmigrants to accept\nfrom this country per year",
                                                 "Climate aid to give\nto this country\n(CHF) per year", "Value of Swiss\ntrade with this\ncountry",
                                                 "Extreme weather event", "Percentage of this\ncountry's votes\nin line with Switzerland's\nposition at the UN\nSecurity Council"))
  ) %>% 
  ggplot(aes(level, estimate)) + 
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  scale_x_discrete(limits=rev) +
  facet_grid(feature_lab ~ by, scales = "free_y", 
             space = "free_y"
  ) +
  theme_light() + 
  coord_flip() +
  labs(y ="Marginal Means", x = "") +
  theme(panel.grid = element_blank(),
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = "black", size =9)) +
  geom_hline(yintercept = 0.5, lty = 2, alpha = 0.7, color = "gray50") 
ggsave(p_language, file = "plots/p_language.pdf", width = 10, height = 10)

#####################################################
# additional robustness checks
#####################################################

library(FindIt)
# Find interactions
F1  <- FindIt(model.treat = choice ~ attrib1_lab + attrib2_lab + attrib3_lab + attrib4_lab + attrib5_lab + attrib6_lab,
              data = dat,  
              type = "binary",
              nway = "multiple"
) 
summary(F1) 

## Returns predicted values for unique treatment combinations.
pred2 <- predict(F1, unique=TRUE)
## Top 50
head_df <- head(pred2$data, n=50)
colnames(head_df) <- NA
rownames(head_df) <- NULL

## Bottom 50
tail_df <- tail(pred2$data, n=50)
colnames(tail_df) <- NA
rownames(tail_df) <- NULL

# print table
tab_higest <- kable(head_df, "latex", booktabs = TRUE, caption = "50 treatement combinations with the highest treatement effect", longtable = T, col.names = NULL) %>% 
  add_header_above(c("Treatment effect" = 1, "Recipient developing country" = 1, "Number of climate migrants"= 1, 
                     "Climate aid"= 1, "Value of Swiss trade", "Extreme weather event", "UN Security Council with Switzerland"), angle = "90") 
tab_lowest <- kable(tail_df, "latex", booktabs = TRUE, caption = "50 treatement combinations with the lowest treatement effect", longtable = T, col.names = NULL) %>% 
  add_header_above(c("Treatment effect" = 1, "Recipient developing country" = 1, "Number of climate migrants"= 1, 
                     "Climate aid"= 1, "Value of Swiss trade", "Extreme weather event", "UN Security Council with Switzerland"), angle = "90") 
writeLines(tab_higest, "Tables/tab_higest.tex")
writeLines(tab_lowest, "Tables/tab_lowest.tex")


findit_plot_dat <- pred2$data %>% 
  as.data.frame() %>% 
  mutate(index = row_number())

# indifference about immigrants - treatment effects = 0 
indifference <- findit_plot_dat %>% filter(Treatment.effect == 0)

p_trade <- findit_plot_dat %>% 
  ggplot(aes(x = index, y = Treatment.effect, col = attrib1_lab)) + 
  geom_point(alpha = .5) + 
  geom_vline(xintercept = indifference %>% slice(1) %>% pull(index), lty = 2) + 
  geom_hline(yintercept = 0, lty = 3) + 
  theme_light() + 
  facet_wrap(~attrib4_lab) +
  scale_color_colorblind() +
  ylim(min(findit_plot_dat$Treatment.effect), max(findit_plot_dat$Treatment.effect)) +
  labs(y = "Treatment effect", x = "Combination of treatment (index)", col = "") + 
  theme(panel.grid = element_blank(),
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = "black", size =9),
        axis.ticks.length = unit(.25, "cm"),
        legend.position = "bottom")

p_un_votes <- findit_plot_dat %>% 
  ggplot(aes(x = index, y = Treatment.effect, col = attrib1_lab)) + 
  geom_point(alpha = .5) + 
  geom_vline(xintercept = indifference %>% slice(1) %>% pull(index), lty = 2) + 
  geom_hline(yintercept = 0, lty = 3) + 
  facet_wrap(~attrib6_lab) +
  scale_color_colorblind() +
  theme_light() + 
  ylim(min(findit_plot_dat$Treatment.effect), max(findit_plot_dat$Treatment.effect)) +
  labs(y = "Treatment effect", x = "Combination of treatment (index)", col = "") + 
  theme(panel.grid = element_blank(),
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = "black", size =9),
        axis.ticks.length = unit(.25, "cm"), 
        legend.position = "bottom")

p_robust <- ggarrange(p_trade, p_un_votes, common.legend = T, ncol = 1, labels = c("a", "b"), legend="bottom")
ggsave(p_robust, file = "Plots/p_robust.pdf", width = 10, height = 7)
