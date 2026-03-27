##----------------------------------------------------------------------------##
##                   Fighting For a Better Life:
##           Protests and Public Opinion in South Africa                   
##
##              REPLICATION FILE 3: CONJOINT ANALYSIS
##----------------------------------------------------------------------------##


##----------------------------------------------------------------
## 1. Set up                                                     
##----------------------------------------------------------------

# A function to install the required packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE,repos = c(
      CRAN = 'https://cran.rstudio.com',
      CRANextra = 'https://macos.rbind.io'
    )
    
    
    )
  sapply(pkg, require, character.only = TRUE)
}


packages_needed <- c("foreign", 
                     "readr",
                     "plyr",
                     "dplyr",
                     "tidyverse",
                     "tidyr",
                     "tibble", 
                     "knitr",
                     "printr",
                     "stargazer",
                     "ggplot2",
                     "rmarkdown",
                     "repmis",
                     "multiwayvcov",
                     "lmtest",
                     "stringr",
                     "kableExtra",
                     "MASS",
                     "fastDummies",
                     "cjoint",
                     "cregg",
                     "haven",
                     "readr",
                     "qualtRics",
                     "report",
                     "htmlTable",
                     "arm",
                     "Hmisc",
                     "varhandle",
                     "ggmap",
                     "readxl",
                     "lessR",
                     "writexl",
                     "texreg",
                     "kableExtra"
                     
)

ipak(packages_needed)

### Set WD --------

#path = " "
#setwd(path)
#getwd()  


##----------------------------------------------------------------
## 2. Data Read In                                               
##----------------------------------------------------------------

# Outcome "Protest sympathy"
load("Rdataframe_sa_protest_conjoint_outcome_protest_sympathy.RData")

# Outcome "Policy support"
load("Rdataframe_sa_protest_conjoint_outcome_policy_support.RData")

# Outcome "Public disorder"
load("Rdataframe_sa_protest_conjoint_outcome_protest_public_disorder.RData")

# Outcome "Deservingness"
load("Rdataframe_sa_protest_conjoint_outcome_policy_deservingness.RData")

# Diagnostics
load("Rdataframe_sa_protest_conjoint_diagnotstics.RData")

##------------------------------------------------------------
## 3. Conjoint Analysis                                                
##----------------------------------------------------------------

formula <- selected ~ Participants + Grievance + Blame + Tactics + Duration + Police 

# Figure 4 (a) - Outcome "Protest Sympathy"
# ----------------------------------------------------------------
df_conjoint <- sa_protest_conjoint_out1 #Setting outcome data

model_amce <- cj(df_conjoint, formula, id = ~Response.ID)

p <- ggplot(model_amce, aes(x = level, y = estimate)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") + 
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) + 
  coord_flip() + 
  labs(x = "", y = "AMCEs") + 
  facet_grid(feature ~ ., scales = "free", space = "free") + 
  scale_y_continuous(limits = c(-0.35, 0.2), breaks = seq(-0.35, 0.2, by = 0.25)) +
  theme_bw() + 
  theme(
    legend.position = "none",
    strip.text.y = element_text(size = 8, face = "bold"),
    panel.spacing = unit(1, "lines"),
    axis.text.y = element_text(size = 11),
    axis.title.x = element_text(size = 9.5)
  )
print(p)

# Appendix Table D2
# ---------------------------------------------------------------------------
table1 <- head(model_amce[c("feature", "level", "estimate", "std.error", "lower", "upper")], 20L)
kbl(table1, booktabs = T, format = 'latex') %>%
  add_header_above(c("Conjoint Features" = 2, "AMCEs" = 4)) 

# Figure 4 (b) - Outcome "Policy Support"
# ----------------------------------------------------------------
df_conjoint <- sa_protest_conjoint_out2 #Setting outcome data

model_amce <- cj(df_conjoint, formula, id = ~Response.ID)

p <- ggplot(model_amce, aes(x = level, y = estimate)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") + 
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) + 
  coord_flip() + 
  labs(x = "", y = "AMCEs") + 
  facet_grid(feature ~ ., scales = "free", space = "free") + 
  scale_y_continuous(limits = c(-0.35, 0.2), breaks = seq(-0.35, 0.2, by = 0.25)) +
  theme_bw() + 
  theme(
    legend.position = "none",
    strip.text.y = element_text(size = 8, face = "bold"),
    panel.spacing = unit(1, "lines"),
    axis.text.y = element_text(size = 11),
    axis.title.x = element_text(size = 9.5)
  )
print(p)

# Appendix Table D3
# ---------------------------------------------------------------------------
table1 <- head(model_amce[c("feature", "level", "estimate", "std.error", "lower", "upper")], 20L)
kbl(table1, booktabs = T, format = 'latex') %>%
  add_header_above(c("Conjoint Features" = 2, "AMCEs" = 4)) 

# Figure 5 - Racial Group Identification - Outcome "Policy Support"                                               -
#----------------------------------------------------------------------
df_conjoint <- sa_protest_conjoint_out2 #Setting outcome data
df_conjoint$subgroup <- df_conjoint$black
table(df_conjoint$subgroup)

mms <- cj(df_conjoint, formula, id = ~Response.ID, estimate = "mm", by = ~subgroup)
diff_mms <- cj(df_conjoint, formula, id = ~Response.ID, estimate = "mm_diff", by = ~subgroup)
combined_mms <- rbind(mms, diff_mms)

p <- ggplot(combined_mms, aes(x = reorder(level, estimate), y = estimate)) + 
  geom_hline(aes(yintercept = ifelse(BY == "Other - Black", NA, 0.5)), 
             linetype = "dashed", color = "gray") + 
  geom_hline(aes(yintercept = ifelse(BY == "Other - Black", 0, NA)), 
             linetype = "dashed", color = "gray") + 
  geom_point(size = 0.5) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  coord_flip() + 
  labs(x = "", y = "MMs") + 
  facet_grid(feature ~ BY, scales = "free", space = "free") +  # Add `by` as a column facet
  theme_bw() + 
  theme(
    legend.position = "none",
    strip.text.y = element_text(size = 8, face = "bold"),  # Reduced font size
    panel.spacing = unit(1, "lines"),
    axis.text.y = element_text(size = 9.5),
    axis.title.x = element_text(size = 9.5)
  ) +
  scale_y_continuous(limits = c(-0.07, 0.7), breaks = seq(-0.07, 0.7, by = 0.25))
print(p)

# Zoom on tactics
# ----------------------------------------------------------------------
mms1 <- mms %>% filter(feature=="Tactics")
diff_mms1 <- diff_mms %>% filter(feature=="Tactics")
combined_mms1 <- rbind(mms1, diff_mms1)

p <- ggplot(combined_mms1, aes(x = reorder(level, estimate), y = estimate)) + 
  geom_hline(aes(yintercept = ifelse(BY == "Other - Black", NA, 0.5)), 
             linetype = "dashed", color = "gray") + 
  geom_hline(aes(yintercept = ifelse(BY == "Other - Black", 0, NA)), 
             linetype = "dashed", color = "gray") + 
  geom_point(size = 1.5) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, size = 0.5) +
  coord_flip() + 
  labs(x = "", y = "MMs") + 
  facet_grid(feature ~ BY, scales = "free", space = "free") +  # Add `by` as a column facet
  theme_bw() + 
  theme(
    legend.position = "none",
    strip.text.y = element_text(size = 8, face = "bold"),  # Reduced font size
    panel.spacing = unit(1, "lines"),
    axis.text.y = element_text(size = 9.5),
    axis.title.x = element_text(size = 9.5)
  ) +
  scale_y_continuous(limits = c(-0.1, 0.7), breaks = seq(-0.1, 0.7, by = 0.25))
print(p)

# Appendix Table D7
# ---------------------------------------------------------------------------
table1 <- head(combined_mms1[c("BY", "feature", "level", "estimate", "std.error", "lower", "upper")], 20L)
kbl(table1, booktabs = T, format = 'latex') %>%
  add_header_above(c("Conjoint Features" = 2, "MMs" = 4)) 

# Appendix Figure D3 (a) - Outcome "Public disorder"
# ----------------------------------------------------------------
df_conjoint <- sa_protest_conjoint_out4 #Setting outcome data

model_amce <- cj(df_conjoint, formula, id = ~Response.ID)

p <- ggplot(model_amce, aes(x = level, y = estimate)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") + 
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) + 
  coord_flip() + 
  labs(x = "", y = "AMCEs") + 
  facet_grid(feature ~ ., scales = "free", space = "free") + 
  scale_y_continuous(limits = c(-0.2, 0.4), breaks = seq(-0.2, 0.4, by = 0.25)) +
  theme_bw() + 
  theme(
    legend.position = "none",
    strip.text.y = element_text(size = 8, face = "bold"),
    panel.spacing = unit(1, "lines"),
    axis.text.y = element_text(size = 11),
    axis.title.x = element_text(size = 9.5)
  )
print(p)

# Appendix Figure D3 (b) - Outcome "Deservingness"
# ----------------------------------------------------------------
df_conjoint <- sa_protest_conjoint_out5 #Setting outcome data

model_amce <- cj(df_conjoint, formula, id = ~Response.ID)

p <- ggplot(model_amce, aes(x = level, y = estimate)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") + 
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) + 
  coord_flip() + 
  labs(x = "", y = "AMCEs") + 
  facet_grid(feature ~ ., scales = "free", space = "free") + 
  scale_y_continuous(limits = c(-0.35, 0.2), breaks = seq(-0.35, 0.2, by = 0.25)) +
  theme_bw() + 
  theme(
    legend.position = "none",
    strip.text.y = element_text(size = 8, face = "bold"),
    panel.spacing = unit(1, "lines"),
    axis.text.y = element_text(size = 11),
    axis.title.x = element_text(size = 9.5)
  )
print(p)

# Appendeix Figure D4 (a) Interactions "Tactics x Blame"                                            -
#----------------------------------------------------------------
df_conjoint <- sa_protest_conjoint_out2 #Setting outcome data
df_conjoint$tactics_blame <- interaction(df_conjoint$Blame,
                                         df_conjoint$Tactics, sep=" and ")
attr(df_conjoint$tactics_blame, "label") <- "Blame x Tactics"

f4 <- selected ~ tactics_blame 

mms <- mm(df_conjoint, f4, id = ~Response.ID)
mms <- mms %>% arrange(estimate) # Order results by coefficient sizes
levels(mms$level) <- c("Peaceful March x Nobody",
                       "Peaceful March x Municipal",
                       "Peaceful March x Nat. Govt.",
                       "Peaceful March x President",
                       "Peaceful March x Ward Councl.",
                       "Riots x Nobody",
                       "Riots x Municipal",
                       "Riots x Nat. Govt.",
                       "Riots x President",
                       "Riots x Ward Councl.",
                       "Road Blocks x Nobody",
                       "Road Blocks x Municipal",
                       "Road Blocks x Nat. Govt.",
                       "Road Blocks x President",
                       "Road Blocks x Ward Councl.") # Renaming levels for the plot
mms2 <- mms %>% mutate(
  tactics = case_when(
    level %in% c("Riots x Nobody",
                 "Riots x Municipal",
                 "Riots x Nat. Govt.",
                 "Riots x President",
                 "Riots x Ward Councl.") ~ "Riots",
    level %in% c("Peaceful March x Nobody",
                 "Peaceful March x Municipal",
                 "Peaceful March x Nat. Govt.",
                 "Peaceful March x President",
                 "Peaceful March x Ward Councl.") ~ "Peaceful March",
    level %in% c("Riots x Ward Councl.",
                 "Road Blocks x Nobody",
                 "Road Blocks x Municipal",
                 "Road Blocks x Nat. Govt.",
                 "Road Blocks x President",
                 "Road Blocks x Ward Councl.") ~ "Road Blocks")) # Adding grouping variable according to tactics

plot(mms2,
     group = "tactics",
     feature_headers = TRUE,
     header_fmt = "(%s)",
     size = 1,
     xlab = "Probability of Outcome (MMs)",
     cex.lab = 0.5,
     ylab = "",
     #  legend_title = if (is.null(group)) "Feature" else group,
     legend_pos = "none",
     xlim = NULL,
     vline = 0.5,
     vline_color = "gray",
     theme = ggplot2::theme_bw()) +
  coord_flip() +
  ggplot2::theme(axis.text.x = element_text(angle = 90, hjust=0.95,vjust=0.2, size=10))

# Appendix Table D4
# ---------------------------------------------------------------------------
table1 <- head(mms2[c("feature", "level", "estimate", "std.error", "lower", "upper")], 20L)
kbl(table1, booktabs = T, format = 'latex') %>%
  add_header_above(c("Conjoint Features" = 2, "MMs" = 4)) 

# Appendix Figure D4 (b) Interactions "Tactics x Grievance"                                            -
#----------------------------------------------------------------
df_conjoint <- sa_protest_conjoint_out2 #Setting outcome data
df_conjoint$tactics_grievance <- interaction(df_conjoint$Grievance,
                                         df_conjoint$Tactics, 
                                         sep=" and ")
attr(df_conjoint$tactics_grievance, "label") <- "Grievance x Tactics"

f4 <- selected ~ tactics_grievance 

mms <- mm(df_conjoint, f4, id = ~Response.ID)

mms <- mms %>% arrange(estimate) # Order results by coefficient sizes
levels(mms$level) <- c("Peaceful March x Sanitation",
                       "Peaceful March x Clean Water",
                       "Peaceful March x Electricity",
                       "Riots x Sanitation",
                       "Riots x Clean Water",
                       "Riots x Electricity",
                       "Road Blocks x Sanitation",
                       "Road Blocks x Clean Water",
                       "Road Blocks x Electricity") # Renaming levels for the plot
mms2 <- mms %>% mutate(
  tactics = case_when(
    level %in% c("Riots x Sanitation",
                 "Riots x Clean Water",
                 "Riots x Electricity") ~ "Riots",
    level %in% c("Peaceful March x Sanitation",
                 "Peaceful March x Clean Water",
                 "Peaceful March x Electricity") ~ "Peaceful March",
    level %in% c("Road Blocks x Sanitation",
                 "Road Blocks x Clean Water",
                 "Road Blocks x Electricity") ~ "Road Blocks")) # Adding grouping variable according to tactics

plot(mms2,
     group = "tactics",
     feature_headers = TRUE,
     header_fmt = "(%s)",
     size = 1,
     xlab = "Probability of Outcome (MMs)",
     cex.lab = 0.5,
     ylab = "",
     #  legend_title = if (is.null(group)) "Feature" else group,
     legend_pos = "none",
     xlim = NULL,
     vline = 0.5,
     vline_color = "gray",
     theme = ggplot2::theme_bw()) +
  coord_flip() +
  ggplot2::theme(axis.text.x = element_text(angle = 90, hjust=0.95,vjust=0.2, size=10))

# Appendix Table D5
# ---------------------------------------------------------------------------
table1 <- head(mms2[c("feature", "level", "estimate", "std.error", "lower", "upper")], 20L)
kbl(table1, booktabs = T, format = 'latex') %>%
  add_header_above(c("Conjoint Features" = 2, "MMs" = 4)) 

# Appendix Figure D4 (c) Interactions "Tactics x Size"                                            -
#----------------------------------------------------------------
df_conjoint <- sa_protest_conjoint_out2 #Setting outcome data
df_conjoint$tactics_size <- interaction(df_conjoint$Participants,
                                        df_conjoint$Tactics, 
                                        sep=" and ")
attr(df_conjoint$tactics_size, "label") <- "Participants x Tactics"

f4 <- selected ~ tactics_size 

mms <- mm(df_conjoint, f4, id = ~Response.ID)

mms <- mms %>% arrange(estimate) # Order results by coefficient sizes
levels(mms$level)
levels(mms$level) <- c("Peaceful March x 10 protesters",
                       "Peaceful March x 100 protesters",
                       "Peaceful March x 500 protesters",
                       "Riots x 10 protesters",
                       "Riots x 100 protesters",
                       "Riots x 500 protesters",
                       "Road Blocks x 10 protesters",
                       "Road Blocks x 100 protesters",
                       "Road Blocks x 500 protesters") # Renaming levels for the plot
levels(mms$level)

mms2 <- mms %>% mutate(
  tactics = case_when(
    level %in% c("Riots x 10 protesters",
                 "Riots x 100 protesters",
                 "Riots x 500 protesters") ~ "Riots",
    level %in% c("Peaceful March x 10 protesters",
                 "Peaceful March x 100 protesters",
                 "Peaceful March x 500 protesters") ~ "Peaceful March",
    level %in% c("Road Blocks x 10 protesters",
                 "Road Blocks x 100 protesters",
                 "Road Blocks x 500 protesters") ~ "Road Blocks")) # Adding grouping variable according to tactics

plot(mms2,
     group = "tactics",
     feature_headers = TRUE,
     header_fmt = "(%s)",
     size = 1,
     xlab = "Probability of Outcome (MMs)",
     cex.lab = 0.5,
     ylab = "",
     #  legend_title = if (is.null(group)) "Feature" else group,
     legend_pos = "none",
     xlim = NULL,
     vline = 0.5,
     vline_color = "gray",
     theme = ggplot2::theme_bw()) +
  coord_flip() +
  ggplot2::theme(axis.text.x = element_text(angle = 90, hjust=0.95,vjust=0.2, size=10))

# Appendix Table D6
# ---------------------------------------------------------------------------
table1 <- head(mms2[c("feature", "level", "estimate", "std.error", "lower", "upper")], 20L)
kbl(table1, booktabs = T, format = 'latex') %>%
  add_header_above(c("Conjoint Features" = 2, "MMs" = 4)) 

# Appendeix Figure D5 (c) Interactions "Grievance x Blame x Size" fo Riots                                            -
#----------------------------------------------------------------
df_conjoint <- sa_protest_conjoint_out2 #Setting outcome data
df_conjoint$tactics_grievance_blame_size <- interaction(df_conjoint$Participants, 
                                                        df_conjoint$Grievance, 
                                                        df_conjoint$Blame,
                                                        sep=" and ")
attr(df_conjoint$tactics_grievance_blame_size, "label") <- "Participants x Grievance x Blame"

f4 <- selected ~ tactics_grievance_blame_size 
mms <- cj(df_conjoint, f4, id = ~Response.ID, estimate = "mm", by = ~Tactics)

mms1 <- mms %>% 
  filter(BY=="riots", grepl("ward|president|national", level)==FALSE) %>%
  arrange(estimate)
plot(mms1) +
  geom_vline(xintercept=0.5, colour="grey", linetype="dashed") +
  theme(legend.position = "none") +
  ggplot2::theme(axis.text.x = element_text(angle = 90, hjust=0.95,vjust=0.2, size=10))

##----------------------------------------------------------------
## Experiment Validation / Diagnostics                        
##---------------------------------------------------------------
df_conjoint <- sa_protest_conjoint_diagnotstics #Setting outcome data
formula <- selected ~ Participants + Grievance + Blame + Tactics + Duration + Police 

# Appendix Figure D1 (a): Display Frequencies and Proportions
#---------------------------------------------------------------
df_conjoint <- sa_protest_conjoint_diagnotstics #Setting outcome data

plot(cregg:: cj_freqs(df_conjoint, formula, id = ~Response.ID)) +
  theme(text = element_text(size=10))

# Appendix Figure D1 (b): RESPONDENT's FATIGUE
#---------------------------------------------------------------
df_conjoint <- sa_protest_conjoint_diagnotstics #Setting outcome data
df_conjoint$task <- as.factor(as.numeric(df_conjoint$task))

plot(cj(df_conjoint, formula,
        id = ~Response.ID,
        by = ~task,
        estimate = "mm"),
     group = "task",
     vline = 0.5)+
  theme(text = element_text(size=10))

# Appendix Figure D1 (c): CARRYOVER
#---------------------------------------------------------------
df_conjoint <- sa_protest_conjoint_diagnotstics #Setting outcome data
df_conjoint$profile <- as.factor(df_conjoint$profile)

plot(cj(df_conjoint,  formula,
        id = ~Response.ID,
        by = ~profile,
        estimate = "mm"),
     group = "profile", vline = 0.5)+
  theme(text = element_text(size=10))

# Appendix Figure D2: vignette Treatment Effects Check
# ---------------------------------------------------------------
df_conjoint <- sa_protest_conjoint_diagnotstics #Setting outcome data
df_conjoint$Vignette <- as.factor(df_conjoint$Vignette)

plot(cj(df_conjoint, formula,
        id = ~Response.ID,
        by = ~Vignette,
        estimate = "mm"),
     group = "Vignette",
     vline = 0.5)+
  theme(text = element_text(size=10))


##----------------------------------------------------------------------------##
##                            END OF SCRIPT 
##----------------------------------------------------------------------------##