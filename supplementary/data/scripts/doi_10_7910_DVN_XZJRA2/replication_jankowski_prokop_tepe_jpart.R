################################################
# Replication Script for
# Michael Jankowski, Christine Prokop, Markus Tepe
# Representative Bureaucracy and Public Hiring Preferences. 
# Evidence from a Conjoint Experiment among German Municipal Civil Servants and Private Sector Employees
# Journal of Public Administration Research and Theory (JPART)
# Contact: michael.jankowski@uol.de
################################################

# Install Packages:
# All packages, except for one, should be available from CRAN
# MatchingFrontier is no longer on CRAN
# We have used MatchingFrontier 1.00 (2.0 is available from github)
# MatchingFrontier 1.00 can be installed via (uncomment):

#packageurl <- "https://cran.r-project.org/src/contrib/Archive/MatchingFrontier/MatchingFrontier_1.0.0.tar.gz"
#install.packages(packageurl, repos=NULL, type="source")

# Load required packages

library(tidyverse)
library(cjoint)
library(cregg)
library(gridExtra)
library(foreign)
library(MatchingFrontier)
library(xtable)
library(rvest)
library(cowplot)

# Sample names
sample_names <- c("Priv. Sec.\nEmployees",
                  "Pub. Adm.\nStudents",
                  "Pub. Adm.\nEmployees")

# Load Data
data <- readRDS("JPART_final.rds")

# Estimate Marginal Means

mm_sample <- cj(data,
                wahl ~ note + schulabschluss + berufserfahrung + englisch +
                  engagement + geschlecht + behinderung + alter +
                  herkunftsland,
                id = ~ respondent,
                estimate = "mm",
                by = ~ Sample,
                feature_labels = list("note" = "Grade job qualification",
                                      "schulabschluss" = "School education",
                                      "berufserfahrung" = "Working experience",
                                      "englisch" = "English skills",
                                      "engagement" = "Social engagement",
                                      "geschlecht" = "Gender",
                                      "behinderung" = "Physical disability",
                                      "alter" = "Age",
                                      "herkunftsland" = "(Family) Origin"))

# Adjust levels

levels(mm_sample$level) <- map_chr(levels(mm_sample$level), function(x){
  
  if(!grepl("None", x)) return(x)
  if(grepl("None", x)){
    
    n_rep <- gsub("None|x| |-", "", x) %>% as.numeric()
    
    return(paste0(paste(rep(" ",n_rep), collapse = ""), "None"))
    
  }
  
})

# Help functions for plotting

is_a_factor <- mm_sample %>%
  filter(BY == "Priv. Sec.\nEmployees") %>%
  group_by(feature) %>%
  summarise(n_levels = n()) %>%
  select(n_levels)

is_a_factor <- unlist(lapply(rev(is_a_factor$n_levels), function(x) c(rep(F,x),T)))

font_face <- ifelse(is_a_factor == T, "bold", "italic")

mm_sample$Sample <- factor(mm_sample$Sample,
                           levels = sample_names)

#################################################
# Marginal Means - Figure 1
#################################################

plot(mm_sample, 
     group = "Sample", 
     vline = 0.5,
     header_fmt = "%s") +
  annotate(geom = "rect", 
           xmin = 0.74, 
           xmax = 1.2, 
           ymin = 0, 
           ymax = 44, 
           fill = "white") +
  geom_vline(aes(xintercept = 0.74)) + 
  geom_hline(aes(yintercept = 23.5)) + 
  annotate(geom = "text", x = 0.76, y = 33.5, label = "Meritocratic\nAttributes",
           color = "black", angle = 90) +
  annotate(geom = "text", x = 0.76, y = 11.75, label = "Non-Meritocratic\nAttributes",
           color = "black", angle = 90) +
  aes(shape = Sample) +
  scale_color_manual("Sample",
                     breaks = sample_names,
                     values = c("black", "black", "black")) +
  scale_shape_manual("Sample",
                     breaks = sample_names,
                     values = c(1,15,17)) +
  theme(axis.text.y = element_text(angle = 0, 
                                   face = font_face,
                                   vjust=0.5, 
                                   size = 14)) + 
  theme(panel.grid.major = element_line(colour = "grey90"))

ggsave(file = "Figure1.eps", width = 10, height = 12, dpi = 666)

# Estimates Table (see Table 1 in Appendix)

esttab <- select(mm_sample,
                 Sample,
                 "Feature" = feature,
                 "Level" = level,
                 "Est." = estimate,
                 "SE" = std.error)


esttabx <- split(esttab, esttab$Sample)

export_mm <- bind_cols(esttabx) %>%
  arrange(as.numeric(Feature), -as.numeric(Level)) %>%
  select(Feature, Level, Est., SE, Est.1, SE1, Est.2, SE2)

xtable::xtable(export_mm, 
               caption = "Marginal Mean Estimates for Different Samples", 
               digits = 3) -> esttab

xtable::print.xtable(esttab, 
                     include.rownames = F,
                     type = "html",
                     file = "estimate_table.html")

##########################################################
# Difference in Marginal Means (Figure 2)
##########################################################

# Change Reference Category

data$Sample <- relevel(data$Sample, 2)

mm_sample_diff <- cj(data,
                     wahl ~ note + schulabschluss + berufserfahrung + englisch +
                       engagement + geschlecht + behinderung + alter +
                       herkunftsland,
                     id = ~ respondent,
                     estimate = "mm_diff",
                     by = ~ Sample,
                     feature_labels = list("note" = "Grade job qualification",
                                           "schulabschluss" = "School education",
                                           "berufserfahrung" = "Working experience",
                                           "englisch" = "English skills",
                                           "engagement" = "Social engagement",
                                           "geschlecht" = "Gender",
                                           "behinderung" = "Physical disability",
                                           "alter" = "Age",
                                           "herkunftsland" = "(Family) Origin"))

# Functions for plotting

levels(mm_sample_diff$level) <- map_chr(levels(mm_sample_diff$level), function(x){
  
  if(!grepl("None", x)) return(x)
  if(grepl("None", x)){
    
    n_rep <- gsub("None|x| |-", "", x) %>% as.numeric()
    
    return(paste0(paste(rep(" ",n_rep), collapse = ""), "None"))
    
  }
  
})

is_a_factor <- mm_sample_diff %>%
  filter(BY == "Pub. Adm.\nEmployees - Pub. Adm.\nStudents") %>%
  group_by(feature) %>%
  summarise(n_levels = n()) %>%
  select(n_levels)

is_a_factor <- unlist(lapply(rev(is_a_factor$n_levels), function(x) c(rep(F,x),T)))

font_face <- ifelse(is_a_factor == T, "bold", "italic")

mm_sample_diff$Sample <- factor(mm_sample_diff$Sample,
                                levels = sample_names)

plot(mm_sample_diff, 
     group = "Sample", 
     vline = 0,
     header_fmt = "%s",
     xlab = "Difference in Marginal Means\n(Ref. = Pub. Adm. Students)") +
  xlim(-.15,.15) +
  theme(panel.grid.major = element_line(colour = "grey90")) +
  annotate(geom = "rect", 
           xmin = .13, 
           xmax = Inf, 
           ymin = -Inf, 
           ymax = Inf,
           fill = "white") +
  geom_vline(aes(xintercept = 0.13)) +  
  geom_hline(aes(yintercept = 23.5)) + 
  annotate(geom = "text", x = 0.145, y = 33.5, label = "Meritocratic\nAttributes",
           color = "black", angle = 90) +
  annotate(geom = "text", x = 0.145, y = 11.75, label = "Non-Meritocratic\nAttributes",
           color = "black", angle = 90) +
  aes(shape = Sample) +
  scale_color_manual("Sample",
                     values = rep("black", 3),
                     breaks = sample_names[c(1,3)],
                     limits = sample_names) +
  scale_shape_manual("Sample",
                     breaks = sample_names[c(1,3)],
                     limits = sample_names,
                     values = c(1, 15,17)) +
  theme(axis.text.y = element_text(angle = 0, 
                                   face = font_face,
                                   vjust=0.5, 
                                   size = 14))

ggsave(file = "Figure2.eps", width = 10, height = 12, dpi = 666)

#################################################
# Mechanisms (Figure 4)
#################################################

## Left-Right

data$all_leftright_1[data$all_leftright_1 == ""] <- NA
data$int_lr <- as.numeric(paste(data$all_leftright_1))
data$int_lr[as.numeric(paste(data$all_leftright_1)) < 4] <- "Left"
data$int_lr[as.numeric(paste(data$all_leftright_1)) >= 4 & as.numeric(paste(data$all_leftright_1)) <= 6] <- "Center"
data$int_lr[as.numeric(paste(data$all_leftright_1)) > 6] <- "Right"
data$int_lr <- paste(data$Sample,"-",data$int_lr)
data_lr <- filter(data, !is.na(all_leftright_1))

mm_int_lr <- cj(data_lr,
                wahl ~ note + schulabschluss + berufserfahrung + englisch +
                  engagement + geschlecht + behinderung + alter +
                  herkunftsland,
                id = ~ respondent,
                estimate = "mm",
                by = ~ int_lr,
                feature_labels = list("note" = "Grade job qualification",
                                      "schulabschluss" = "School education",
                                      "berufserfahrung" = "Working experience",
                                      "englisch" = "English skills",
                                      "engagement" = "Social engagement",
                                      "geschlecht" = "Gender",
                                      "behinderung" = "Physical disability",
                                      "alter" = "Age",
                                      "herkunftsland" = "(Family) Origin"))

mm_int_lr$Sample <- gsub(" -.*", "", mm_int_lr$int_lr)
mm_int_lr$left_right <- gsub(".*?- ", "", mm_int_lr$int_lr)
mm_int_lr$left_right <- factor(mm_int_lr$left_right, 
                               c("Left", "Center", "Right"))  
mm_int_lr$Sample <- factor(mm_int_lr$Sample, 
                           sample_names) 

only_origin <- filter(mm_int_lr,
                      feature == "(Family) Origin")

ggplot(only_origin, aes(x = level, y = estimate)) + 
  geom_pointrange(aes(ymin = lower, ymax = upper, 
                      color = left_right,
                      shape = left_right), 
                  position = position_dodge(width = 0.3)) +
  geom_hline(aes(yintercept = 0.5), lty = 2) +
  coord_flip() +
  facet_wrap(~ Sample) +
  xlab("") +
  ylab("Marginal Means") +
  theme_bw(base_size = 14) + 
  theme(legend.position = "top") +
  scale_color_brewer("Ideology",
                     type="qual", 
                     palette=2) +
  scale_shape_manual("Ideology",
                     values = c(1,15,17)) +
  ylim(0.2, 0.8) -> lr_plot

## Perceived Level of Discrimination

data_perc <- filter(data, !is.na(perc))
data_perc$int_perc <- ""
data_perc$int_perc[grepl("1|2|Stimme eher nicht zu|Stimme gar nicht zu", data_perc$perc)] <- "Low"
data_perc$int_perc[grepl("3|teils teils", data_perc$perc)] <- "Some"
data_perc$int_perc[grepl("4|5|Stimme eher zu|Stimme voll zu", data_perc$perc)] <- "High"
data_perc$int_perc <- paste(data_perc$Sample,"-",data_perc$int_perc)

mm_int_perc <- cj(data_perc,
                  wahl ~ note + schulabschluss + berufserfahrung + englisch +
                    engagement + geschlecht + behinderung + alter +
                    herkunftsland,
                  id = ~ respondent,
                  estimate = "mm",
                  by = ~ int_perc,
                  feature_labels = list("note" = "Grade job qualification",
                                        "schulabschluss" = "School education",
                                        "berufserfahrung" = "Working experience",
                                        "englisch" = "English skills",
                                        "engagement" = "Social engagement",
                                        "geschlecht" = "Gender",
                                        "behinderung" = "Physical disability",
                                        "alter" = "Age",
                                        "herkunftsland" = "(Family) Origin"))

mm_int_perc$Sample <- gsub(" -.*", "", mm_int_perc$int_perc)
mm_int_perc$perc <- gsub(".*?- ", "", mm_int_perc$int_perc)
mm_int_perc$perc <- factor(mm_int_perc$perc, 
                           c("Low", "Some", "High"))  
mm_int_perc$Sample <- factor(mm_int_perc$Sample, 
                             sample_names) 

only_origin_perc <- filter(mm_int_perc, feature == "(Family) Origin")

ggplot(only_origin_perc, aes(x = level, y = estimate)) + 
  geom_pointrange(aes(ymin = lower, ymax = upper, 
                      color = perc,
                      shape = perc), 
                  position = position_dodge(width = 0.3)) +
  geom_hline(aes(yintercept = 0.5), lty = 2) +
  coord_flip() +
  facet_wrap(~ Sample) +
  xlab("") +
  ylab("Marginal Means") +
  theme_bw(base_size = 14) + 
  theme(legend.position = "top") +
  scale_color_brewer("Perceived Level of Discrimination",
                     type="qual", 
                     palette=2) +
  scale_shape_manual("Perceived Level of Discrimination",
                     values = c(1,15,17)) +
  ylim(0.2, 0.8) -> perc_plot

## Need for Descriptive Representation

data_drep <- filter(data, !is.na(all_repst_1))
make_num <- function(x){
  
  if(grepl("[0-9]", x)) return(as.integer(x))
  if(!grepl("[0-9]", x)){
    x[x == "Stimme gar nicht zu"] <- "1"
    x[x == "Stimme eher nicht zu"] <- "2"
    x[x == "teils teils"] <- "3"
    x[x == "Stimme eher zu"] <- "4"
    x[x == "Stimme voll zu"] <- "5"
    return(as.integer(x))
  }
  
}

data_drep$all_repst_1 <- map_int(data_drep$all_repst_1, make_num)
data_drep$int_drep[data_drep$all_repst_1 <= 2] <- "No"
data_drep$int_drep[data_drep$all_repst_1 >= 4 ] <- "Yes"
data_drep$int_drep[data_drep$all_repst_1 == 3] <- "Some"
data_drep$int_drep <- paste(data_drep$Sample,"-",data_drep$int_drep)

mm_int_drep <- cj(data_drep,
                  wahl ~ note + schulabschluss + berufserfahrung + englisch +
                    engagement + geschlecht + behinderung + alter +
                    herkunftsland,
                  id = ~ respondent,
                  estimate = "mm",
                  by = ~ int_drep,
                  feature_labels = list("note" = "Grade job qualification",
                                        "schulabschluss" = "School education",
                                        "berufserfahrung" = "Working experience",
                                        "englisch" = "English skills",
                                        "engagement" = "Social engagement",
                                        "geschlecht" = "Gender",
                                        "behinderung" = "Physical disability",
                                        "alter" = "Age",
                                        "herkunftsland" = "(Family) Origin"))

mm_int_drep$Sample <- gsub(" -.*", "", mm_int_drep$int_drep)
mm_int_drep$drep <- gsub(".*?- ", "", mm_int_drep$int_drep)
mm_int_drep$drep <- factor(mm_int_drep$drep, 
                           c("No", "Some", "Yes"))  
mm_int_drep$Sample <- factor(mm_int_drep$Sample, 
                             sample_names) 

only_origin_drep <- filter(mm_int_drep, feature == "(Family) Origin")

ggplot(only_origin_drep, aes(x = level, y = estimate)) + 
  geom_pointrange(aes(ymin = lower, ymax = upper, 
                      color = drep,
                      shape = drep), 
                  position = position_dodge(width = 0.3)) +
  geom_hline(aes(yintercept = 0.5), lty = 2) +
  coord_flip() +
  facet_wrap(~ Sample) +
  ylab("Marginal Means") +
  xlab("") +
  theme_bw(base_size = 14) + 
  theme(legend.position = "top") +
  scale_color_brewer("Need for Descriptive Representation",
                     type="qual", 
                     palette=2) +
  scale_shape_manual("Need for Descriptive Representation",
                     values = c(1,15,17)) +
  ylim(0.2, 0.8) -> drep_plot

# Combine three plots

combo <- cowplot::plot_grid(perc_plot, drep_plot, lr_plot, 
                            ncol = 1,
                            align = "v",
                            labels = paste0(LETTERS[1:3],")"),
                            axis = "l")

combo

ggsave(combo, file = "Figure4.eps", width = 10, height = 12, dpi = 666)

#############################################################
# Descriptive Statistics (Table 2, Main Paper)
#############################################################

survey_data <- data[data$task == 1 & data$profile == 1,]

survey_data <- filter(survey_data, sample != "Pub Adm")

df_pubadm <- readRDS("qualtrics_raw_data_text.rds")

df_pubadm <- df_pubadm[-c(1:2),]
df_pubadm <- df_pubadm[,!grepl("^F-", colnames(df_pubadm))]
df_pubadm <- modify_if(df_pubadm, is.character, function(x) rvest::repair_encoding(x, from = "utf8"))
df_pubadm <- modify_if(df_pubadm, is.character, function(x){
  
  x[x == ""] <- NA
  x
  
})

df_pubadm$sample <- "Pub Adm"

survey_data <- bind_rows(survey_data, df_pubadm)

survey_data$all_age[grepl("bis 35 Jahre", survey_data$all_age)] <- "35"
survey_data$all_age[grepl("36 bis 45", survey_data$all_age)] <- "36"
survey_data$all_age[grepl("46 bis 55", survey_data$all_age)] <- "46"
survey_data$all_age[grepl("?ber 55 Jahre", survey_data$all_age)] <- "56"

survey_data$young <- as.numeric(as.numeric(survey_data$all_age) <= 35)
survey_data$mid_young <- as.numeric(as.numeric(survey_data$all_age) >= 36 & as.numeric(survey_data$all_age) <= 45)
survey_data$mid_old <- as.numeric(as.numeric(survey_data$all_age) >= 46 & as.numeric(survey_data$all_age) <= 55)
survey_data$old <- as.numeric(as.numeric(survey_data$all_age) > 55)

survey_data$need <- as.numeric(survey_data$all_repst_1 == "Stimme voll zu" | survey_data$all_repst_1 == "Stimme eher zu")

survey_data$perc[is.na(survey_data$perc)] <- survey_data$pub_perc[is.na(survey_data$perc)]

survey_data$gender <- as.numeric(survey_data$all_sex == "Weiblich")
survey_data$abitur <- as.numeric(grepl("^Abitur", survey_data$all_edu))
survey_data$migrant <- as.numeric(survey_data$all_migr == "Ja")
survey_data$left_right <- as.numeric(survey_data$all_leftright_1)
survey_data$perc_disc <- as.numeric(survey_data$perc == "Stimme voll zu" | survey_data$perc == "Stimme eher zu")

survey_data$psm1 <- as.numeric(survey_data$all_psm_1 == "Stimme voll zu" | survey_data$all_psm_1 == "Stimme eher zu")
survey_data$psm2 <- as.numeric(survey_data$all_psm_2 == "Stimme voll zu" | survey_data$all_psm_2 == "Stimme eher zu")
survey_data$psm3 <- as.numeric(survey_data$all_psm_3 == "Stimme voll zu" | survey_data$all_psm_3 == "Stimme eher zu")
survey_data$psm4 <- as.numeric(survey_data$all_psm_4 == "Stimme voll zu" | survey_data$all_psm_4 == "Stimme eher zu")
survey_data$psm5 <- as.numeric(survey_data$all_psm_5 == "Stimme voll zu" | survey_data$all_psm_5 == "Stimme eher zu")

summary_vars <- c("young", "mid_young", "mid_old", "old", "gender", "abitur", "migrant", "left_right", "perc_disc", paste0("psm",1:5), "need")

descr_row <- function(x){
  
  mean2 <- function(x) mean(x, na.rm = T)
  se <- function(x, na.rm=T) {
    if (na.rm) x <- na.omit(x)
    sqrt(var(x)/length(x))
  }
  
  data <- survey_data %>%
    select(sample, x) %>%
    group_by(sample) %>%
    summarise_all(funs(mean2, se)) %>%
    select(mean2, se)
  
  data.frame(var = x,
             m1 = data$mean2[1],
             se1 = data$se[1],
             m2 = data$mean2[2],
             se2 = data$se[2],
             m3 = data$mean2[3],
             se3 = data$se[3])
  
}

desc <- map_df(summary_vars, descr_row)

xtable::xtable(desc, 
               caption = "Descriptive Statistic", 
               digits = 3) -> esttab

xtable::print.xtable(esttab, 
                     include.rownames = F,
                     type = "html",
                     file = "descriptive_stats.html")

######################################
## Matching (Figure 3)
######################################

# Match: Private with Students

# Subset data to one observation for each respondent for matching

survey_data <- data[data$task == 1 & data$profile == 1,]

# Keep only Priv. Sec. Employees and Pub Adm Students

match_data <- subset(survey_data, 
                     Sample == "Priv. Sec.\nEmployees" | Sample == "Pub. Adm.\nStudents")

match_data$treatment <- as.numeric(match_data$Sample == "Priv. Sec.\nEmployees")
match_data$age <- as.numeric(match_data$all_age)
match_data$gender <- as.numeric(match_data$all_sex == "Weiblich")
match_data$village <- as.numeric(match_data$all_area == "L?ndliches Dorf")
match_data$city <- as.numeric(match_data$all_area == "Gro?stadt")
match_data$abitur <- as.numeric(grepl("^Abitur", match_data$all_edu))
match_data$migrant <- as.numeric(match_data$all_migr == "Ja")
match_data$left_right <- as.numeric(match_data$all_leftright_1)
match_data$perc_disc <- as.numeric(match_data$perc == "Stimme voll zu" | match_data$perc == "Stimme eher zu")

match_data$psm1 <- as.numeric(match_data$all_psm_1 == "Stimme voll zu" | match_data$all_psm_1 == "Stimme eher zu")
match_data$psm2 <- as.numeric(match_data$all_psm_2 == "Stimme voll zu" | match_data$all_psm_2 == "Stimme eher zu")
match_data$psm3 <- as.numeric(match_data$all_psm_3 == "Stimme voll zu" | match_data$all_psm_3 == "Stimme eher zu")
match_data$psm4 <- as.numeric(match_data$all_psm_4 == "Stimme voll zu" | match_data$all_psm_4 == "Stimme eher zu")
match_data$psm5 <- as.numeric(match_data$all_psm_5 == "Stimme voll zu" | match_data$all_psm_5 == "Stimme eher zu")

match.on <- c("age", "gender", "abitur", "migrant", "left_right", "perc_disc", paste0("psm",1:5))

match_data$progress <- 100

mahal.frontier <- makeFrontier(dataset = match_data[,c("progress", match.on, "treatment", "respondent")],
                               treatment = 'treatment',
                               match.on = match.on,
                               outcome = "progress",
                               keep.vars = "respondent")

# Function for Estimating Effects along the Frontier

run_frontier <- function(n){
  
  matched_data <- generateDataset(mahal.frontier, N = n)
  
  kept_obs <- matched_data[,c("treatment", "respondent")]
  
  data_new <- merge(data, kept_obs, 
                    by.x = "respondent", 
                    by.y = "respondent")
  
  data_new$Sample <- droplevels(data_new$Sample)
  
  mm_sample_diff_matched <- cj(data_new,
                               wahl ~ herkunftsland + behinderung + alter + schulabschluss + note + engagement + englisch + geschlecht + berufserfahrung,
                               id = ~ respondent,
                               by = ~ Sample,
                               estimate = "mm")
  
  mm_sample_diff_matched$n <- sum(matched_data$treatment)  
  
  mm_sample_diff_matched
}

matched_dfs <- plyr::ldply(seq(250,1200,25), 
                           function(x) run_frontier(x), 
                           .progress = "text")

matched_dfs <- matched_dfs[!duplicated(matched_dfs[,c("n", "level", "BY")]),]

plot_data <- matched_dfs %>% filter(feature == "herkunftsland" | feature == "alter")

plot_data$Attribute <- ifelse(grepl("[0-9]", plot_data$level), "Age", "(Family) Background")

# Plot for Family Background

ggplot(filter(plot_data, Attribute == "(Family) Background"), 
       aes(x = n, y = estimate, ymin = lower, ymax = upper,
           fill = BY, lty = BY)) +
  geom_hline(aes(yintercept = 0.5), lty = 2) +
  geom_ribbon(alpha =.5, color = NA) +
  geom_line(color = "black") +
  ylab("Marginal Means") +
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom") + 
  scale_x_reverse() +
  facet_wrap(~ level, ncol = 5) +
  scale_fill_manual("Sample", values = c("orange", "darkblue"))  +
  scale_linetype_manual("Sample", values = c(1,2)) +
  guides(fill = F, lty = F) -> A

A

# Plot for Age 

ggplot(filter(plot_data, Attribute == "Age"), 
       aes(x = n, y = estimate, ymin = lower, ymax = upper,
           fill = BY, lty = BY)) +
  geom_hline(aes(yintercept = 0.5), lty = 2) +
  geom_ribbon(alpha =.5, color = NA) +
  geom_line(color = "white") +
  ylab("Marginal Means") +
  xlab("Size of Pub. Adm. Sample") +
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom") + 
  scale_x_reverse() +
  facet_wrap(~ level, ncol = 5) +
  scale_fill_manual("Sample", values = c("orange", "darkblue")) +
  scale_linetype_manual("Sample", values = c(1,2)) -> B

B

plot_data_student <- plot_data

## Pub. Adm. Employees vs. Private Sector

survey_data <- data[data$task == 1 & data$profile == 1,]

match_data <- subset(survey_data, Sample == "Priv. Sec.\nEmployees")

df_pubadm <- readRDS("qualtrics_raw_data_text.rds")
df_pubadm <- df_pubadm[-c(1:2),]
df_pubadm <- df_pubadm[,!grepl("^F-", colnames(df_pubadm))]
df_pubadm <- modify_if(df_pubadm, is.character, function(x) rvest::repair_encoding(x, from = "utf8"))
df_pubadm <- modify_if(df_pubadm, is.character, function(x){
  
  x[x == ""] <- NA
  x
  
})
df_pubadm$Sample <- "Pub. Adm.\nEmployees"

match_data <- bind_rows(match_data, df_pubadm)

match_data$Sample <- as.factor(match_data$Sample)

match_data$treatment <- as.numeric(match_data$Sample == "Priv. Sec.\nEmployees")

match_data$all_age[grepl("bis 35 Jahre", match_data$all_age)] <- "35"
match_data$all_age[grepl("36 bis 45", match_data$all_age)] <- "36"
match_data$all_age[grepl("46 bis 55", match_data$all_age)] <- "46"
match_data$all_age[grepl("?ber 55 Jahre", match_data$all_age)] <- "56"

match_data$young <- as.numeric(as.numeric(match_data$all_age) <= 35)
match_data$mid_young <- as.numeric(as.numeric(match_data$all_age) >= 36 & as.numeric(match_data$all_age) <= 45)
match_data$mid_old <- as.numeric(as.numeric(match_data$all_age) >= 46 & as.numeric(match_data$all_age) <= 55)

match_data$perc[is.na(match_data$perc)] <- match_data$pub_perc[is.na(match_data$perc)]

match_data$gender <- as.numeric(match_data$all_sex == "Weiblich")
match_data$abitur <- as.numeric(grepl("^Abitur", match_data$all_edu))
match_data$migrant <- as.numeric(match_data$all_migr == "Ja")
match_data$left_right <- as.numeric(match_data$all_leftright_1)
match_data$perc_disc <- as.numeric(match_data$perc == "Stimme voll zu" | match_data$perc == "Stimme eher zu")

match_data$psm1 <- as.numeric(match_data$all_psm_1 == "Stimme voll zu" | match_data$all_psm_1 == "Stimme eher zu")
match_data$psm2 <- as.numeric(match_data$all_psm_2 == "Stimme voll zu" | match_data$all_psm_2 == "Stimme eher zu")
match_data$psm3 <- as.numeric(match_data$all_psm_3 == "Stimme voll zu" | match_data$all_psm_3 == "Stimme eher zu")
match_data$psm4 <- as.numeric(match_data$all_psm_4 == "Stimme voll zu" | match_data$all_psm_4 == "Stimme eher zu")
match_data$psm5 <- as.numeric(match_data$all_psm_5 == "Stimme voll zu" | match_data$all_psm_5 == "Stimme eher zu")

match.on <- c("young", "mid_young", "mid_old", "gender", "abitur", 
              "migrant", "left_right", "perc_disc", 
              paste0("psm",1:5))

match_data$progress <- 100

match_data$respondent[is.na(match_data$respondent)] <- match_data$ResponseId[is.na(match_data$respondent)]

match_data <- match_data[complete.cases(match_data[,c("progress", match.on, "treatment", "respondent")]),]

mahal.frontier <- makeFrontier(dataset = match_data[,c("progress", match.on, "treatment", "respondent")],
                               treatment = 'treatment',
                               match.on = match.on,
                               outcome = "progress",
                               keep.vars = "respondent")

# Function for Estimating Effects along the Frontier

run_frontier <- function(n){
  
  matched_data <- generateDataset(mahal.frontier, N = n)
  
  kept_obs <- matched_data[,c("treatment", "respondent")]
  
  data_new <- merge(data, kept_obs, 
                    by.x = "respondent", 
                    by.y = "respondent")
  
  data_new$Sample <- droplevels(data_new$Sample)
  
  mm_sample_diff_matched <- cj(data_new,
                               wahl ~ herkunftsland + behinderung + alter + schulabschluss + note + engagement + englisch + geschlecht + berufserfahrung,
                               id = ~ respondent,
                               by = ~ Sample,
                               estimate = "mm",
                               alpha = 0.1)
  
  mm_sample_diff_matched$n <- sum(matched_data$treatment)  
  
  mm_sample_diff_matched
}

matched_dfs <- plyr::ldply(seq(250,1200,50), 
                           function(x) run_frontier(x), 
                           .progress = "text")

plot_data <- matched_dfs %>% filter(feature == "herkunftsland" | feature == "alter")

plot_data$Attribute <- ifelse(grepl("[0-9]", plot_data$level), "Age", "(Family) Background")

ggplot(filter(plot_data, Attribute == "(Family) Background"), 
       aes(x = n, y = estimate, ymin = lower, ymax = upper,
           fill = BY, lty = BY)) +
  geom_hline(aes(yintercept = 0.5), lty = 2) +
  geom_ribbon(alpha =.5, color = NA) +
  geom_line(color = "white") +
  ylab("Marginal Means") +
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom") + 
  scale_x_reverse() +
  facet_wrap(~ level, ncol = 5) +
  scale_fill_manual("Sample", values = c("orange", "darkblue"))  +
  scale_linetype_manual("Sample", values = c(1,2)) +
  guides(fill = F, lty = F) -> A2

ggplot(filter(plot_data, Attribute == "Age"), 
       aes(x = n, y = estimate, ymin = lower, ymax = upper,
           fill = BY, lty = BY)) +
  geom_hline(aes(yintercept = 0.5), lty = 2) +
  geom_ribbon(alpha =.5, color = NA) +
  geom_line(color = "white") +
  ylab("Marginal Means") +
  xlab("Size of Pub. Adm. Sample") +
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom") + 
  scale_x_reverse() +
  facet_wrap(~ level, ncol = 5) +
  scale_fill_manual("Sample", values = c("orange", "darkblue")) +
  scale_linetype_manual("Sample", values = c(1,2)) -> B2

plot_data_eyployees <- plot_data

plot_data_eyployees$match_by <- "Pub. Adm. Employees"
plot_data_student$match_by <- "Pub. Adm. Students"

plot_data <- rbind(plot_data_eyployees,
                   plot_data_student)

plot_data <- plot_data %>%
  group_by(match_by, BY, level) %>%
  mutate(n_prop = n/max(n))

ggplot(filter(plot_data_eyployees, Attribute == "(Family) Background"), 
       aes(x = n, 
           y = estimate, 
           ymin = lower, 
           ymax = upper,
           fill = BY, lty = BY)) +
  geom_hline(aes(yintercept = 0.5), lty = 2) +
  geom_ribbon(alpha =.5, color = NA) +
  geom_line(color = "white") +
  ylab("Marginal Means") +
  xlab("Size of Pub. Adm. Employees Sample") +
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom") + 
  scale_x_reverse() +
  facet_grid(~ level) +
  scale_fill_manual("Sample", values = c("darkblue", "darkred", "darkred")) +
  scale_linetype_manual("Sample", values = c(1,2,2)) -> A

ggplot(filter(plot_data_student, Attribute == "(Family) Background"), 
       aes(x = n, 
           y = estimate, 
           ymin = lower, 
           ymax = upper,
           fill = BY, lty = BY)) +
  geom_hline(aes(yintercept = 0.5), lty = 2) +
  geom_ribbon(alpha =.5, color = NA) +
  geom_line(color = "white") +
  ylab("Marginal Means") +
  xlab("Size of Pub. Adm. Student Sample") +
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom") + 
  scale_x_reverse() +
  facet_grid(~ level) +
  scale_fill_manual("Sample", values = c("darkblue", "darkred", "darkred")) +
  scale_linetype_manual("Sample", values = c(1,2,2)) -> B

cowplot::plot_grid(B, A, 
                   labels = c("A)", "B)"), 
                   ncol = 1, align = "v", axis = "l",
                   rel_heights = c(1, 1)) -> C

C

ggsave(C, 
       file = "Figure3.eps",
       width = 10,
       height = 8,
       dpi = 666, device=cairo_ps, fallback_resolution = 666)

ggplot(filter(plot_data_eyployees, Attribute == "Age"), 
       aes(x = n, 
           y = estimate, 
           ymin = lower, 
           ymax = upper,
           fill = BY, lty = BY)) +
  geom_hline(aes(yintercept = 0.5), lty = 2) +
  geom_ribbon(alpha =.5, color = NA) +
  geom_line(color = "white") +
  ylab("Marginal Means") +
  xlab("Size of Pub. Adm. Empl. Sample") +
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom") + 
  scale_x_reverse() +
  facet_grid(~ level) +
  scale_fill_manual("Sample", values = c("darkblue", "darkred", "darkred")) +
  scale_linetype_manual("Sample", values = c(1,2,2)) -> Aage

ggplot(filter(plot_data_student, Attribute == "Age"), 
       aes(x = n, 
           y = estimate, 
           ymin = lower, 
           ymax = upper,
           fill = BY, lty = BY)) +
  geom_hline(aes(yintercept = 0.5), lty = 2) +
  geom_ribbon(alpha =.5, color = NA) +
  geom_line(color = "white") +
  ylab("Marginal Means") +
  xlab("Size of Pub. Adm. Stud. Sample") +
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom") + 
  scale_x_reverse() +
  facet_grid(~ level) +
  scale_fill_manual("Sample", values = c("darkblue", "darkred", "darkred")) +
  scale_linetype_manual("Sample", values = c(1,2,2)) -> Bage

cowplot::plot_grid(Bage, Aage, 
                   labels = c("A)", "B)"), 
                   ncol = 1, align = "v", axis = "l",
                   rel_heights = c(1, 1)) -> Cage

Cage

ggsave(Cage, 
       file = "match_age_appendix.eps",
       width = 10,
       height = 8,
       dpi = 666,
       device=cairo_ps, fallback_resolution = 666)

#########################################
# Interaktion: Qualification & Migration
# Figure 5
#########################################

data$int_edu_mig <- interaction(data$note, data$herkunftsland, sep = "XXX")

mm_int <- cj(data,
             wahl ~ schulabschluss + berufserfahrung + englisch +
               engagement + geschlecht + behinderung + alter +
               int_edu_mig,
             id = ~ respondent,
             estimate = "mm",
             by = ~ Sample,
             feature_labels = list("schulabschluss" = "School education",
                                   "berufserfahrung" = "Working experience",
                                   "englisch" = "English skills",
                                   "engagement" = "Social engagement",
                                   "geschlecht" = "Gender",
                                   "behinderung" = "Physical disability",
                                   "alter" = "Age",
                                   "herkunftsland" = "(Family) Origin",
                                   "int_edu_mig" = "Interaction"))

mm_int <- filter(mm_int, feature == "Interaction")

mm_int$Grade <- gsub("XXX.*", "", mm_int$level)
mm_int$Country <- gsub(".*XXX", "", mm_int$level)

mm_int$Country <- factor(mm_int$Country, 
                         levels = rev(c("Germany", "France", "Poland", "Turkey")))

mm_int$Grade <- factor(mm_int$Grade, 
                       levels = rev(c("Excellent (1.3)", 
                                      "Very good (1.9)", 
                                      "Good (2.2)", 
                                      "Satisfactory (2.6)",
                                      "Sufficient (3.5)")))

mm_int$Sample <- factor(mm_int$Sample,
                        levels = sample_names)

ggplot(mm_int, aes(y = estimate, 
                   x = Grade, 
                   ymin = lower, 
                   ymax = upper,
                   shape = Country)) +
  geom_hline(aes(yintercept = 0.5), lty = 2) +
  geom_pointrange(position = position_dodge(width = 0.3)) +
  coord_flip() +
  facet_wrap(~ Sample) +
  scale_shape_manual(values = c(0,1,15,16)) +
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom") +
  xlab("Grade in Job Qualification") +
  ylab("Marginal Mean")

ggsave(file = "Figure5.eps", width = 12, height = 6, dpi = 666)

#######################################################################
# Assumptions of Conjoint Experiment
#######################################################################

## Carryover Effects

data$task <- as.factor(data$task)

ftest <- cj_anova(data,
                  wahl ~ note + schulabschluss + berufserfahrung + englisch +
                    engagement + geschlecht + behinderung + alter + herkunftsland,
                  by = ~ task)

mm_carryover <- cj(data,
                   wahl ~ note + schulabschluss + berufserfahrung + englisch +
                     engagement + geschlecht + behinderung + alter + herkunftsland,
                   id = ~ respondent,
                   estimate = "mm",
                   by = ~ task,
                   feature_labels = list("note" = "Grade in job qualification",
                                         "schulabschluss" = "School education",
                                         "berufserfahrung" = "Working experience",
                                         "englisch" = "English skills",
                                         "engagement" = "Social engagement",
                                         "geschlecht" = "Gender",
                                         "behinderung" = "Physical disability",
                                         "alter" = "Age",
                                         "herkunftsland" = "(Family) Origin"))

levels(mm_carryover$level) <- map_chr(levels(mm_carryover$level), function(x){
  
  if(!grepl("None", x)) return(x)
  if(grepl("None", x)){
    
    n_rep <- gsub("None|x| |-", "", x) %>% as.numeric()
    
    return(paste0(paste(rep(" ",n_rep), collapse = ""), "None"))
    
  }
  
})

is_a_factor <- mm_carryover %>%
  filter(BY == "1") %>%
  group_by(feature) %>%
  summarise(n_levels = n()) %>%
  select(n_levels)

is_a_factor <- unlist(lapply(rev(is_a_factor$n_levels), function(x) c(rep(F,x),T)))

font_face <- ifelse(is_a_factor == T, "bold", "italic")

plot(mm_carryover, 
     group = "task", 
     vline = 0,
     header_fmt = "%s",
     xlab = "MMs") +
  theme(panel.grid.major = element_line(colour = "grey90")) +
  annotate(geom = "rect", 
           xmin = .67, 
           xmax = Inf, 
           ymin = -Inf, 
           ymax = Inf,
           fill = "white") +
  geom_vline(aes(xintercept = 0.5)) +  
  geom_hline(aes(yintercept = 23.5)) + 
  annotate(geom = "text", x = 0.7, y = 33.5, label = "Meritocratic\nAttributes",
           color = "black", angle = 90) +
  annotate(geom = "text", x = 0.7, y = 11.75, label = "Non-Meritocratic\nAttributes",
           color = "black", angle = 90) +
  aes(shape = task) +
  scale_color_manual("task",
                     breaks = 1:6,
                     values = rep("black", 6)) +
  scale_shape_manual("task",
                     breaks = 1:6,
                     values = 1:6) +
  theme(axis.text.y = element_text(angle = 0, 
                                   face = font_face,
                                   vjust=0.5, 
                                   size = 14)) + 
  theme(panel.grid.major = element_line(colour = "grey90"))

ggsave(file = "carryover_effects.eps", width = 10, height = 13, dpi = 666)

### Profile Order Effects

data$profile <- as.factor(data$profile)

ftest <- cj_anova(data,
                  wahl ~ note + schulabschluss + berufserfahrung + englisch +
                    engagement + geschlecht + behinderung + alter + herkunftsland,
                  by = ~ profile)

ftest

mm_profile <- cj(data,
                 wahl ~ note  + schulabschluss + berufserfahrung + englisch +
                   engagement + geschlecht + behinderung + alter + herkunftsland,
                 id = ~ respondent,
                 estimate = "mm",
                 by = ~ profile,
                 feature_labels = list("note" = "Grade in job qualification",
                                       "schulabschluss" = "School education",
                                       "berufserfahrung" = "Working experience",
                                       "englisch" = "English skills",
                                       "engagement" = "Social engagement",
                                       "geschlecht" = "Gender",
                                       "behinderung" = "Physical disability",
                                       "alter" = "Age",
                                       "herkunftsland" = "(Family) Origin"))

levels(mm_profile$level) <- map_chr(levels(mm_profile$level), function(x){
  
  if(!grepl("None", x)) return(x)
  if(grepl("None", x)){
    
    n_rep <- gsub("None|x| |-", "", x) %>% as.numeric()
    
    return(paste0(paste(rep(" ",n_rep), collapse = ""), "None"))
    
  }
  
})

is_a_factor <- mm_profile %>%
  filter(BY == "1") %>%
  group_by(feature) %>%
  summarise(n_levels = n()) %>%
  select(n_levels)

is_a_factor <- unlist(lapply(rev(is_a_factor$n_levels), function(x) c(rep(F,x),T)))

font_face <- ifelse(is_a_factor == T, "bold", "italic")

plot(mm_profile, 
     group = "profile", 
     vline = 0,
     header_fmt = "%s",
     xlab = "MMs") +
  theme(panel.grid.major = element_line(colour = "grey90")) +
  annotate(geom = "rect", 
           xmin = .67, 
           xmax = Inf, 
           ymin = -Inf, 
           ymax = Inf,
           fill = "white") +
  geom_vline(aes(xintercept = 0.5)) +  
  geom_hline(aes(yintercept = 23.5)) + 
  annotate(geom = "text", x = 0.68, y = 33.5, label = "Meritocratic\nAttributes",
           color = "black", angle = 90) +
  annotate(geom = "text", x = 0.68, y = 11.75, label = "Non-Meritocratic\nAttributes",
           color = "black", angle = 90) +
  aes(shape = profile) +
  scale_color_manual("profile",
                     breaks = 1:2,
                     values = rep("black", 6)) +
  scale_shape_manual("profile",
                     breaks = 1:2,
                     values = 1:2) +
  theme(axis.text.y = element_text(angle = 0, 
                                   face = font_face,
                                   vjust=0.5, 
                                   size = 14)) + 
  theme(panel.grid.major = element_line(colour = "grey90"))

ggsave(file = "profile_order_effects.eps", width = 11, height = 13, dpi = 666)

### Randomization

select(data, 
       `Grade in job qualification` = note,
       `School education` = schulabschluss,
       `Working experience` = berufserfahrung,
       `English skills` = englisch,
       `Social engagement` = engagement,
       Gender = geschlecht,
       `Physical disability` = behinderung,
       Age = alter,
       `(Family) Origin` = herkunftsland) %>%
  gather("Feature", "Level") %>%
  group_by(Feature) %>%
  count(Level) %>%
  mutate(percent = 100*n/sum(n)) %>%
  ungroup() %>%
  mutate(Level = gsub("x[0-9] - ", "", .$Level)) %>%
  mutate(Level = gsub('(.{1,20})(\\s|$)', '\\1\n', .$Level)) -> df

ggplot(df, aes(x = Level, y = percent)) +
  geom_col() +
  facet_wrap(~ Feature, scales = "free") +
  ylab("%") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("randomization_test.eps", height = 8, width = 12, dpi = 666)

############################################################
# Selected Profiles (First vs Second)
# Potential Satisfycing by always selecting 1st profile...?
############################################################

n_tasks <- nrow(data)/2
n1pro <- sum(data$profile[data$wahl == 1] == 1)
probs <- pbinom(3700:4000, size = n_tasks, prob = 0.5)

ggplot(data = NULL, aes(x = 3700:4000,
                        y = probs)) +
  geom_line() +
  ylab("Cumulated Probability") +
  xlab("Number of times profile 1 was selected") +
  geom_vline(aes(xintercept = n1pro), lty = 2) +
  geom_hline(aes(yintercept = pbinom(n1pro, size = n_tasks, prob = 0.5)), lty = 2) +
  theme_bw(base_size = 16)

ggsave("satisfy.eps", width = 10, height = 6, dpi = 666)
