##----------------------------------------------------------------------------##
##           PROGRAMMATIC REDISTRIBUTION AND CLIENTELISM:                    
##        EVIDENCE FROM A CONJOINT SURVEY EXPERIMENT IN BRAZIL 
##
##                  REPLICATION FILE 2: APPENDIX
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## 1) Set up                                                     
##----------------------------------------------------------------------------##

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
                     "tidyr",
                     "tibble", 
                     "knitr",
                     "printr",
                     "stargazer",
                     "ggplot2",
                     "MatchIt",
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
                     "report",
                     "htmlTable",
                     "arm",
                     "FindIt",
                     "Hmisc",
                     "ggcorrplot", 
                     "sjPlot",
                     "GGally"
                     
)

ipak(packages_needed)

### Set WD -------

#path = " "
#setwd(path)
#getwd() 

##----------------------------------------------------------------------------##
## 2. Data Read In                                               
##----------------------------------------------------------------------------##
#Data read in====

# loading datasets
load("conjoint_redistribution_voting_na.Rdata")
load("conjoint_redistribution_voting_V2_na.Rdata")
load("conjoint_fiscal_na.Rdata")
load("conjoint_corruption_na.Rdata")
load("conjoint_help_na.Rdata")
load("conjoint_poverty_na.Rdata")
load("conjoint_inequality_na.Rdata")
load("conjoint_services_na.Rdata")

##----------------------------------------------------------------------------##
## 3. NEW VARIABLES                                                                -
##----------------------------------------------------------------------------##

# Interaction effects
conjoint_redistribution_voting_na$client_locpol <- interaction(conjoint_redistribution_voting_na$Local.politics,conjoint_redistribution_voting_na$Clientelism,sep=" and ")
conjoint_redistribution_voting_na$client_tax <- interaction(conjoint_redistribution_voting_na$Tax.preferences,conjoint_redistribution_voting_na$Clientelism,sep=" and ")
conjoint_redistribution_voting_na$locpol_tax <- interaction(conjoint_redistribution_voting_na$Local.politics,conjoint_redistribution_voting_na$Tax.preferences,sep=" and ")

conjoint_redistribution_voting_v2_na$client_locpol <- interaction(conjoint_redistribution_voting_v2_na$Local.politics,conjoint_redistribution_voting_v2_na$Clientelism,sep=" and ")
conjoint_redistribution_voting_v2_na$client_tax <- interaction(conjoint_redistribution_voting_v2_na$Tax.preferences,conjoint_redistribution_voting_v2_na$Clientelism,sep=" and ")
conjoint_redistribution_voting_v2_na$locpol_tax <- interaction(conjoint_redistribution_voting_v2_na$Local.politics,conjoint_redistribution_voting_v2_na$Tax.preferences,sep=" and ")

conjoint_fiscal_na$client_locpol <- interaction(conjoint_fiscal_na$Local.politics,conjoint_fiscal_na$Clientelism,sep=" and ")
conjoint_fiscal_na$client_tax <- interaction(conjoint_fiscal_na$Tax.preferences,conjoint_fiscal_na$Clientelism,Sep=" and ")

conjoint_corruption_na$client_locpol <- interaction(conjoint_corruption_na$Local.politics,conjoint_corruption_na$Clientelism, sep=" and ")
conjoint_corruption_na$client_tax <- interaction(conjoint_corruption_na$Tax.preferences,conjoint_corruption_na$Clientelism,sep=" and ")

conjoint_help_na$client_locpol <- interaction(conjoint_help_na$Local.politics,conjoint_help_na$Clientelism,sep=" and ")
conjoint_help_na$client_tax <- interaction(conjoint_help_na$Tax.preferences,conjoint_help_na$Clientelism,sep=" and ")

conjoint_poverty_na$client_locpol <- interaction(conjoint_poverty_na$Local.politics,conjoint_poverty_na$Clientelism,sep=" and ")
conjoint_poverty_na$client_tax <- interaction(conjoint_poverty_na$Tax.preferences,conjoint_poverty_na$Clientelism,sep=" and ")

conjoint_inequality_na$client_locpol <- interaction(conjoint_inequality_na$Local.politics,conjoint_inequality_na$Clientelism, sep=" and ")
conjoint_inequality_na$client_tax <- interaction(conjoint_inequality_na$Tax.preferences,conjoint_inequality_na$Clientelism,sep=" and ")

conjoint_services_na$client_locpol <- interaction(conjoint_services_na$Local.politics,conjoint_services_na$Clientelism,sep=" and ")
conjoint_services_na$client_tax <- interaction(conjoint_services_na$Tax.preferences,conjoint_services_na$Clientelism,sep=" and ")

# Labels
attr(conjoint_redistribution_voting_na$client_locpol, "label") <- "Programmatic Distribution x Clientelism"
attr(conjoint_redistribution_voting_na$client_tax, "label") <- "Income Taxes x Clientelism"
attr(conjoint_redistribution_voting_na$locpol_tax, "label") <- "Programmatic Distribution x Income Taxes"

attr(conjoint_redistribution_voting_v2_na$client_locpol, "label") <- "Programmatic Distribution x Clientelism"
attr(conjoint_redistribution_voting_v2_na$client_tax, "label") <- "Income Taxes x Clientelism"
attr(conjoint_redistribution_voting_v2_na$locpol_tax, "label") <- "Programmatic Distribution x Income Taxes"

attr(conjoint_fiscal_na$client_locpol, "label") <- "Programmatic Distribution x Clientelism"
attr(conjoint_fiscal_na$client_tax, "label") <- "Income Taxes x Clientelism"

attr(conjoint_corruption_na$client_locpol, "label") <- "Programmatic Distribution x Clientelism"
attr(conjoint_corruption_na$client_tax, "label") <- "Income Taxes x Clientelism"

attr(conjoint_help_na$client_locpol, "label") <- "Programmatic Distribution x Clientelism"
attr(conjoint_help_na$client_tax, "label") <- "Income Taxes x Clientelism"

attr(conjoint_poverty_na$client_locpol, "label") <- "Programmatic Distribution x Clientelism"
attr(conjoint_poverty_na$client_tax, "label") <- "Income Taxes x Clientelism"

attr(conjoint_inequality_na$client_locpol, "label") <- "Programmatic Distribution x Clientelism"
attr(conjoint_inequality_na$client_tax, "label") <- "Income Taxes x Clientelism"

attr(conjoint_services_na$client_locpol, "label") <- "Programmatic Distribution x Clientelism"
attr(conjoint_services_na$client_tax, "label") <- "Income Taxes x Clientelism"

# DEFINE FORMULAS                                      
# ----------------------------------------------------------------------------##

f1 <- selected ~ Age + Gender + Occupation + Clientelism + Local.politics + Tax.preferences 
f2 <- selected ~ client_locpol
f3 <- selected ~ client_tax 
f4 <- selected ~ locpol_tax 

##----------------------------------------------------------------------------##
## B.1 DIAGNOSTICS                                      -
##----------------------------------------------------------------------------##

# (a) Display Frequencies and Proportions
# ---------------------------------------------------------------

plot(cregg::cj_freqs(conjoint_redistribution_voting_na, f1,
                     id = ~Response.ID)) +
  theme(text = element_text(size=10))

# (b) Carryover
# ---------------------------------------------------------------

conjoint_redistribution_voting_na$profile <- as.factor(conjoint_redistribution_voting_na$profile)

plot(cj(conjoint_redistribution_voting_na,  f1,
        id = ~Response.ID,
        by = ~profile,
        estimate = "mm"), 
     group = "profile", vline = 0.5)+ 
  theme(text = element_text(size=10))


# (c) Profile-order effects
# ---------------------------------------------------------------

conjoint_redistribution_voting_na$task <- as.factor(as.numeric(conjoint_redistribution_voting_na$task))

plot(cj(conjoint_redistribution_voting_na, f1,
        id = ~Response.ID,
        by = ~task,
        estimate = "mm"), 
     group = "task", 
     vline = 0.5)+ 
  theme(text = element_text(size=10))

# (d) Attribute-level effects
# ---------------------------------------------------------------

conjoint_redistribution_voting_na$Idade.rowpos <- as.factor(as.numeric(conjoint_redistribution_voting_na$Idade.rowpos))

plot(cj(conjoint_redistribution_voting_na, f1,
        id = ~Response.ID,
        by = ~Idade.rowpos,
        estimate = "mm"), 
     group = "Idade.rowpos", 
     vline = 0.5) +
  theme(text = element_text(size=10))

##-----------------------------------------------------------------------------##
## A.5 Attention Checks
##-----------------------------------------------------------------------------##

# FIGURE B.3: Main effects on Candidate Support, by Attention Check 
# --------------------------------------------------------------------
mms <- cj(conjoint_redistribution_voting_na, f1, id = ~Response.ID, estimate = "mm", by = ~attention_check_1)
diff_mms <- cj(conjoint_redistribution_voting_na, f1, id = ~Response.ID, estimate = "mm_diff", 
               by = ~attention_check_1)
plot(rbind(mms, diff_mms)) +
  ggplot2::facet_wrap(~BY, ncol = 3L) +
  geom_vline(xintercept=0.5, colour="grey", linetype="dashed") +
  theme(legend.position = "none")

# FIGURE B.4: Interaction effects on Candidate Support, by Attention Check
# -------------------------------------------------------------------------

# (a) Clientelism x Programmatic distribution
mms <- cj(conjoint_redistribution_voting_na, f2, id = ~Response.ID, estimate = "mm", by = ~attention_check_1)
diff_mms <- cj(conjoint_redistribution_voting_na, f2, id = ~Response.ID, estimate = "mm_diff", by = ~attention_check_1)

plot(rbind(mms, diff_mms)) +
  ggplot2::facet_wrap(~BY, ncol = 3L) +
  geom_vline(xintercept=0.5, colour="grey", linetype="dashed") +
  theme(legend.position = "none")

# (b) Clientelism x Taxation
mms <- cj(conjoint_redistribution_voting_na, f3, id = ~Response.ID, estimate = "mm", by = ~attention_check_1)
diff_mms <- cj(conjoint_redistribution_voting_na, f3, id = ~Response.ID, estimate = "mm_diff", by = ~attention_check_1)

plot(rbind(mms, diff_mms)) +
  ggplot2::facet_wrap(~BY, ncol = 3L) +
  geom_vline(xintercept=0.5, colour="grey", linetype="dashed") +
  theme(legend.position = "none")


##-----------------------------------------------------------------------------##
## C. MAIN RESULTS
##-----------------------------------------------------------------------------##

# TABLE C.1: Main effects 
# ----------------------------------------------------------------------------

# (a) Candidate support, AMCEs
model_amce <- cj(conjoint_redistribution_voting_na, f1, id = ~Response.ID,
                 feature_labels = list(
                   Age = "Age",
                   Gender = "Gender",
                   Occupation = "Occupation",
                   Clientelism = "Clientelist\ndistribution",  # Line break added
                   Local.politics = "Programmatic\ndistribution",  # Line break added
                   Tax.preferences = "Tax\npreferences"  # Line break added
                 ))
table1 <- head(model_amce[c("feature", "level", "estimate", "std.error")], 20L)

# (b) Likelihood of winning, AMCEs
model_amce <- cj(conjoint_redistribution_voting_v2_na, f1, id = ~Response.ID,
                 feature_labels = list(
                   Age = "Age",
                   Gender = "Gender",
                   Occupation = "Occupation",
                   Clientelism = "Clientelist\ndistribution",  # Line break added
                   Local.politics = "Programmatic\ndistribution",  # Line break added
                   Tax.preferences = "Tax\npreferences"  # Line break added
                 ))
table2 <- head(model_amce[c("feature", "level", "estimate", "std.error")], 20L)


# combined table for export
table <- merge(x = table1, y = table2, by = c("feature", "level")) 

kbl(table, longtable = T, booktabs = T, format = 'latex') %>%
  add_header_above(c("Conjoint Features" = 2, "Candidate Support, AMCE" = 2, "Likelihood of Winning, AMCEs" = 2)) %>%
  kable_styling(latex_options = c("repeat_header")) 

# TABLE C.2: Interaction effects 
# ----------------------------------------------------------------------------

# Programmatic Distribution x Clientelism
mms <- mm(conjoint_redistribution_voting_na, f2, id = ~Response.ID)
table1 <- head(mms[c("feature", "level", "estimate", "std.error")], 20L)

# Income Taxes x Clientelism
mms <- mm(conjoint_redistribution_voting_na, f3, id = ~Response.ID)
table2 <- head(mms[c("feature", "level", "estimate", "std.error")], 20L)

# combined table for export
table <- rbind(table1, table2) 

kbl(table, longtable = T, booktabs = T, format = 'latex') %>%
  add_header_above(c("Conjoint Features" = 2, "Interaction Effects, MM" = 2)) %>%
  kable_styling(latex_options = c("repeat_header")) 

# TABLE C.3-C.8: Beliefs -
# ----------------------------------------------------------------------------

# a) Fiscal capacity
mms <- mm(conjoint_fiscal_na, f2, id = ~Response.ID)
table1 <- head(mms[c("feature", "level", "estimate", "std.error")], 20L)

# b) Corruption
mms <- mm(conjoint_corruption_na, f2, id = ~Response.ID)
table2 <- head(mms[c("feature", "level", "estimate", "std.error")], 20L)

# c) Help people like you
mms <- mm(conjoint_help_na, f2, id = ~Response.ID)
table3 <- head(mms[c("feature", "level", "estimate", "std.error")], 20L)

# d) Help poor people
mms <- mm(conjoint_poverty_na, f2, id = ~Response.ID)
table4 <- head(mms[c("feature", "level", "estimate", "std.error")], 20L)

# e) Inequality
mms <- mm(conjoint_inequality_na, f2, id = ~Response.ID)
table5 <- head(mms[c("feature", "level", "estimate", "std.error")], 20L)

# f) Public services
mms <- mm(conjoint_services_na, f2, id = ~Response.ID)
table6 <- head(mms[c("feature", "level", "estimate", "std.error")], 20L)

# 3C: table for export - 1 mechanism at a time
kbl(table1, longtable = T, booktabs = T, format = 'latex') %>%
  add_header_above(c("Conjoint Features" = 2, "Improve fiscal capacity" = 2)) %>%
  kable_styling(latex_options = c("repeat_header")) 

kbl(table2, longtable = T, booktabs = T, format = 'latex') %>%
  add_header_above(c("Conjoint Features" = 2, "Engage in corruption" = 2)) %>%
  kable_styling(latex_options = c("repeat_header")) 

kbl(table3, longtable = T, booktabs = T, format = 'latex') %>%
  add_header_above(c("Conjoint Features" = 2, "Help in economic distress" = 2)) %>%
  kable_styling(latex_options = c("repeat_header")) 

kbl(table4, longtable = T, booktabs = T, format = 'latex') %>%
  add_header_above(c("Conjoint Features" = 2, "Help poor people" = 2)) %>%
  kable_styling(latex_options = c("repeat_header")) 

kbl(table5, longtable = T, booktabs = T, format = 'latex') %>%
  add_header_above(c("Conjoint Features" = 2, "Reduce inequality" = 2)) %>%
  kable_styling(latex_options = c("repeat_header")) 

kbl(table6, longtable = T, booktabs = T, format = 'latex') %>%
  add_header_above(c("Conjoint Features" = 2, "Deliver public services" = 2)) %>%
  kable_styling(latex_options = c("repeat_header")) 


# TABLE C.9: Subgroup analysis, non-poor vs. poor 
# ----------------------------------------------------------------------------
mms <- cj(conjoint_redistribution_voting_na, f2, id = ~Response.ID, estimate = "mm", by = ~ind_poverty)
table1 <- head(mms[c("feature", "level", "estimate", "std.error")], 20L)

mms <- mms %>% arrange(estimate) # Order results by coefficient sizes
levels(mms$level)

diff_mms <- cj(conjoint_redistribution_voting_na, f2, id = ~Response.ID, estimate = "mm_diff", by = ~ind_poverty)
table2 <- head(diff_mms[c("feature", "level", "estimate", "std.error")], 20L)

diff_mms <- diff_mms %>% arrange(estimate) # Order results by coefficient sizes
levels(diff_mms$level)

kbl(table1, longtable = T, booktabs = T, format = 'latex') %>%
  add_header_above(c("Conjoint Features" = 2, "MM Effects" = 2)) %>%
  kable_styling(latex_options = c("repeat_header")) 

kbl(table2, longtable = T, booktabs = T, format = 'latex') %>%
  add_header_above(c("Conjoint Features" = 2, "MM Effects Difference Poor-Non-Poor" = 2)) %>%
  kable_styling(latex_options = c("repeat_header")) 

# TABLE C.10: Subgroup analysis, main effects, non-poor vs. poor 
# ----------------------------------------------------------------------------
mms <- cj(conjoint_redistribution_voting_na, f1, id = ~Response.ID, estimate = "mm", by = ~ind_poverty)
table1 <- head(mms[c("feature", "level", "estimate", "std.error")], 40L)

mms <- mms %>% arrange(estimate) # Order results by coefficient sizes
levels(mms$level)

diff_mms <- cj(conjoint_redistribution_voting_na, f1, id = ~Response.ID, estimate = "mm_diff", by = ~ind_poverty)
table2 <- head(diff_mms[c("feature", "level", "estimate", "std.error")], 40L)

diff_mms <- diff_mms %>% arrange(estimate) # Order results by coefficient sizes
levels(diff_mms$level)

kbl(table1, longtable = T, booktabs = T, format = 'latex') %>%
  add_header_above(c("Conjoint Features" = 2, "MM Effects" = 2)) %>%
  kable_styling(latex_options = c("repeat_header")) 

kbl(table2, longtable = T, booktabs = T, format = 'latex') %>%
  add_header_above(c("Conjoint Features" = 2, "MM Effects Difference Poor-Non-Poor" = 2)) %>%
  kable_styling(latex_options = c("repeat_header")) 

##-----------------------------------------------------------------------------##
## D. ADDITIONAL RESULTS
##-----------------------------------------------------------------------------##


# FIGURE D.1: Effects on Candidate Support and the Likelihood of Winning, MMs 
# ----------------------------------------------------------------------------

# a) Candidate support, MMs
model_mm <- cj(conjoint_redistribution_voting_na, f1, id = ~Response.ID, estimate = "mm",
               feature_labels = list(Age = "Age",
                                     Gender = "Gender",
                                     Occupation = "Occupation",
                                     Clientelism = "Clientelist distribution",
                                     Local.politics = "Programmatic distribution", 
                                     Tax.preferences = "Tax preferences"))

plot_obj <- ggplot(model_mm, aes(x = reorder(level, estimate), y = estimate)) + 
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") + 
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) + 
  coord_flip() + 
  labs(x = "", y = "AMCEs") + 
  facet_grid(feature ~ ., scales = "free", space = "free") + 
  scale_y_continuous(limits = c(0.4, 0.8), breaks = seq(0.4, 0.8, by = 0.1)) +
  theme_bw() + 
  theme(
    legend.position = "none",
    strip.text.y = element_text(size = 8, face = "bold"),  # Reduced font size to 8
    panel.spacing = unit(1, "lines"),
    axis.text.y = element_text(size = 9.5),
    axis.title.x = element_text(size = 9.5)
  )
print(plot_obj)

# b) Likelihood of winning, MMs
model_mm <- cj(conjoint_redistribution_voting_v2_na, f1, id = ~Response.ID, estimate = "mm",
               feature_labels = list(Age = "Age",
                                     Gender = "Gender",
                                     Occupation = "Occupation",
                                     Clientelism = "Clientelist distribution",
                                     Local.politics = "Programmatic distribution", 
                                     Tax.preferences = "Tax preferences"))

plot_obj <- ggplot(model_mm, aes(x = reorder(level, estimate), y = estimate)) + 
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") + 
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) + 
  coord_flip() + 
  labs(x = "", y = "AMCEs") + 
  facet_grid(feature ~ ., scales = "free", space = "free") + 
  scale_y_continuous(limits = c(0.4, 0.8), breaks = seq(0.4, 0.8, by = 0.1)) +
  theme_bw() + 
  theme(
    legend.position = "none",
    strip.text.y = element_text(size = 8, face = "bold"),  # Reduced font size to 8
    panel.spacing = unit(1, "lines"),
    axis.text.y = element_text(size = 9.5),
    axis.title.x = element_text(size = 9.5)
  )
print(plot_obj)


# FIGURE D.2: Interaction Effects of Programatic Distribution x Income Taxes on Candidate Support, Ms 
# ----------------------------------------------------------------------------

# a) Candidate Support
mms <- mm(conjoint_redistribution_voting_na, f4, id = ~Response.ID)

# Plot
plot_obj <- ggplot(mms, aes(x = reorder(level, estimate), y = estimate)) + 
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") + 
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) + 
  coord_flip() + 
  labs(x = "", y = "MMs") + 
  facet_grid(feature ~ ., scales = "free", space = "free") + 
  theme_bw() + 
  theme(
    legend.position = "none",
    strip.text.y = element_text(size = 8, face = "bold"),  # Reduced font size to 8
    panel.spacing = unit(1, "lines"),
    axis.text.y = element_text(size = 9.5),
    axis.title.x = element_text(size = 9.5)
  ) 
print(plot_obj)


# b) Likelihood of Winning
mms <- mm(conjoint_redistribution_voting_v2_na, f4, id = ~Response.ID)

# Plot
plot_obj <- ggplot(mms, aes(x = reorder(level, estimate), y = estimate)) + 
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") + 
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) + 
  coord_flip() + 
  labs(x = "", y = "MMs") + 
  facet_grid(feature ~ ., scales = "free", space = "free") + 
  theme_bw() + 
  theme(
    legend.position = "none",
    strip.text.y = element_text(size = 8, face = "bold"),  # Reduced font size to 8
    panel.spacing = unit(1, "lines"),
    axis.text.y = element_text(size = 9.5),
    axis.title.x = element_text(size = 9.5)
  ) 
print(plot_obj)


# FIGURE D.3: Effects on Voter Beliefs, AMCEs 
# ----------------------------------------------------------------------------

# a) Fiscal capacity
model_amce <- cj(conjoint_fiscal_na, f1, id = ~Response.ID,
                 feature_labels = list(Age = "Age",
                                       Gender = "Gender",
                                       Occupation = "Occupation",
                                       Clientelism = "Clientelist\n distribution",
                                       Local.politics = "Programmatic\n distribution", 
                                       Tax.preferences = "Tax\n preferences"))

plot_obj <- ggplot(model_amce, aes(x = reorder(level, estimate), y = estimate)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") + 
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) + 
  coord_flip() + 
  labs(x = "", y = "AMCEs") + 
  facet_grid(feature ~ ., scales = "free", space = "free") + 
  scale_y_continuous(limits = c(-0.4, 0.4), breaks = seq(-0.4, 0.4, by = 0.1)) +
  theme_bw() + 
  theme(
    legend.position = "none",
    strip.text.y = element_text(size = 9, face = "bold"),  
    panel.spacing = unit(1, "lines"),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 12)
  )
print(plot_obj)


# b) Corruption
model_amce <- cj(conjoint_corruption_na, f1, id = ~Response.ID,
                 feature_labels = list(Age = "Age",
                                       Gender = "Gender",
                                       Occupation = "Occupation",
                                       Clientelism = "Clientelist\n distribution",
                                       Local.politics = "Programmatic\n distribution", 
                                       Tax.preferences = "Tax\n preferences"))

plot_obj <- ggplot(model_amce, aes(x = reorder(level, estimate), y = estimate)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") + 
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) + 
  coord_flip() + 
  labs(x = "", y = "AMCEs") + 
  facet_grid(feature ~ ., scales = "free", space = "free") + 
  scale_y_continuous(limits = c(-0.4, 0.4), breaks = seq(-0.4, 0.4, by = 0.1)) +
  theme_bw() + 
  theme(
    legend.position = "none",
    strip.text.y = element_text(size = 9, face = "bold"),  # Reduced font size to 8
    panel.spacing = unit(1, "lines"),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 12)
  )
print(plot_obj)


# c) Help people like you
model_amce <- cj(conjoint_help_na, f1, id = ~Response.ID,
                 feature_labels = list(Age = "Age",
                                       Gender = "Gender",
                                       Occupation = "Occupation",
                                       Clientelism = "Clientelist\n distribution",
                                       Local.politics = "Programmatic\n distribution", 
                                       Tax.preferences = "Tax\n preferences"))

plot_obj <- ggplot(model_amce, aes(x = reorder(level, estimate), y = estimate)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") + 
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) + 
  coord_flip() + 
  labs(x = "", y = "AMCEs") + 
  facet_grid(feature ~ ., scales = "free", space = "free") + 
  scale_y_continuous(limits = c(-0.4, 0.4), breaks = seq(-0.4, 0.4, by = 0.1)) +
  theme_bw() + 
  theme(
    legend.position = "none",
    strip.text.y = element_text(size = 9, face = "bold"),  # Reduced font size to 8
    panel.spacing = unit(1, "lines"),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 12)
  )
print(plot_obj)


# Help the poor
model_amce <- cj(conjoint_poverty_na, f1, id = ~Response.ID,
                 feature_labels = list(Age = "Age",
                                       Gender = "Gender",
                                       Occupation = "Occupation",
                                       Clientelism = "Clientelist\n distribution",
                                       Local.politics = "Programmatic\n distribution", 
                                       Tax.preferences = "Tax\n preferences"))

plot_obj <- ggplot(model_amce, aes(x = reorder(level, estimate), y = estimate)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") + 
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) + 
  coord_flip() + 
  labs(x = "", y = "AMCEs") + 
  facet_grid(feature ~ ., scales = "free", space = "free") + 
  scale_y_continuous(limits = c(-0.4, 0.4), breaks = seq(-0.4, 0.4, by = 0.1)) +
  theme_bw() + 
  theme(
    legend.position = "none",
    strip.text.y = element_text(size = 9, face = "bold"),  # Reduced font size to 8
    panel.spacing = unit(1, "lines"),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 12)
  )
print(plot_obj)

# e) Reduce inequality
model_amce <- cj(conjoint_inequality_na, f1, id = ~Response.ID,
                 feature_labels = list(Age = "Age",
                                       Gender = "Gender",
                                       Occupation = "Occupation",
                                       Clientelism = "Clientelist\n distribution",
                                       Local.politics = "Programmatic\n distribution", 
                                       Tax.preferences = "Tax\n preferences"))

plot_obj <- ggplot(model_amce, aes(x = reorder(level, estimate), y = estimate)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") + 
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) + 
  coord_flip() + 
  labs(x = "", y = "AMCEs") + 
  facet_grid(feature ~ ., scales = "free", space = "free") + 
  scale_y_continuous(limits = c(-0.4, 0.4), breaks = seq(-0.4, 0.4, by = 0.1)) +
  theme_bw() + 
  theme(
    legend.position = "none",
    strip.text.y = element_text(size = 9, face = "bold"),  # Reduced font size to 8
    panel.spacing = unit(1, "lines"),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 12)
  )
print(plot_obj)

# f) Public services
model_amce <- cj(conjoint_services_na, f1, id = ~Response.ID,
                 feature_labels = list(Age = "Age",
                                       Gender = "Gender",
                                       Occupation = "Occupation",
                                       Clientelism = "Clientelist\n distribution",
                                       Local.politics = "Programmatic\n distribution", 
                                       Tax.preferences = "Tax\n preferences"))

plot_obj <- ggplot(model_amce, aes(x = reorder(level, estimate), y = estimate)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") + 
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) + 
  coord_flip() + 
  labs(x = "", y = "AMCEs") + 
  facet_grid(feature ~ ., scales = "free", space = "free") + 
  scale_y_continuous(limits = c(-0.4, 0.4), breaks = seq(-0.4, 0.4, by = 0.1)) +
  theme_bw() + 
  theme(
    legend.position = "none",
    strip.text.y = element_text(size = 9, face = "bold"),  # Reduced font size to 8
    panel.spacing = unit(1, "lines"),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 12)
  )
print(plot_obj)


# FIGURE D.4: Effects on Voter Beliefs, MMs 
# ----------------------------------------------------------------------------

# a) Fiscal capacity
model_mm <- cj(conjoint_fiscal_na, f1, id = ~Response.ID, estimate = "mm",
               feature_labels = list(Age = "Age",
                                     Gender = "Gender",
                                     Occupation = "Occupation",
                                     Clientelism = "Clientelist\n distribution",
                                     Local.politics = "Programmatic\n distribution", 
                                     Tax.preferences = "Tax\n preferences"))

plot_obj <- ggplot(model_mm, aes(x = reorder(level, estimate), y = estimate)) + 
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") + 
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) + 
  coord_flip() + 
  labs(x = "", y = "MMs") + 
  facet_grid(feature ~ ., scales = "free", space = "free") + 
  scale_y_continuous(limits = c(0.2, 0.8), breaks = seq(0.2, 0.8, by = 0.1)) +
  theme_bw() + 
  theme(
    legend.position = "none",
    strip.text.y = element_text(size = 9, face = "bold"),  # Reduced font size to 8
    panel.spacing = unit(1, "lines"),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 12)
  )
print(plot_obj)

# b) Corruption
model_mm <- cj(conjoint_corruption_na, f1, id = ~Response.ID, estimate = "mm",
               feature_labels = list(Age = "Age",
                                     Gender = "Gender",
                                     Occupation = "Occupation",
                                     Clientelism = "Clientelist\n distribution",
                                     Local.politics = "Programmatic\n distribution", 
                                     Tax.preferences = "Tax\n preferences"))

plot_obj <- ggplot(model_mm, aes(x = reorder(level, estimate), y = estimate)) + 
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") + 
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) + 
  coord_flip() + 
  labs(x = "", y = "MMs") + 
  facet_grid(feature ~ ., scales = "free", space = "free") + 
  scale_y_continuous(limits = c(0.2, 0.8), breaks = seq(0.2, 0.8, by = 0.1)) +
  theme_bw() + 
  theme(
    legend.position = "none",
    strip.text.y = element_text(size = 9, face = "bold"),  # Reduced font size to 8
    panel.spacing = unit(1, "lines"),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 12)
  )
print(plot_obj)

# c) Help people like you
model_mm <- cj(conjoint_help_na, f1, id = ~Response.ID, estimate = "mm",
               feature_labels = list(Age = "Age",
                                     Gender = "Gender",
                                     Occupation = "Occupation",
                                     Clientelism = "Clientelist\n distribution",
                                     Local.politics = "Programmatic\n distribution", 
                                     Tax.preferences = "Tax\n preferences"))

plot_obj <- ggplot(model_mm, aes(x = reorder(level, estimate), y = estimate)) + 
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") + 
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) + 
  coord_flip() + 
  labs(x = "", y = "MMs") + 
  facet_grid(feature ~ ., scales = "free", space = "free") + 
  scale_y_continuous(limits = c(0.2, 0.8), breaks = seq(0.2, 0.8, by = 0.1)) +
  theme_bw() + 
  theme(
    legend.position = "none",
    strip.text.y = element_text(size = 9, face = "bold"),  # Reduced font size to 8
    panel.spacing = unit(1, "lines"),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 12)
  )
print(plot_obj)

# Help the poor
model_mm <- cj(conjoint_poverty_na, f1, id = ~Response.ID, estimate = "mm",
               feature_labels = list(Age = "Age",
                                     Gender = "Gender",
                                     Occupation = "Occupation",
                                     Clientelism = "Clientelist\n distribution",
                                     Local.politics = "Programmatic\n distribution", 
                                     Tax.preferences = "Tax\n preferences"))

plot_obj <- ggplot(model_mm, aes(x = reorder(level, estimate), y = estimate)) + 
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") + 
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) + 
  coord_flip() + 
  labs(x = "", y = "MMs") + 
  facet_grid(feature ~ ., scales = "free", space = "free") + 
  scale_y_continuous(limits = c(0.2, 0.8), breaks = seq(0.2, 0.8, by = 0.1)) +
  theme_bw() + 
  theme(
    legend.position = "none",
    strip.text.y = element_text(size = 9, face = "bold"),  # Reduced font size to 8
    panel.spacing = unit(1, "lines"),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 12)
  )
print(plot_obj)

# e) Reduce inequality
model_mm <- cj(conjoint_inequality_na, f1, id = ~Response.ID, estimate = "mm",
               feature_labels = list(Age = "Age",
                                     Gender = "Gender",
                                     Occupation = "Occupation",
                                     Clientelism = "Clientelist\n distribution",
                                     Local.politics = "Programmatic\n distribution", 
                                     Tax.preferences = "Tax\n preferences"))

plot_obj <- ggplot(model_mm, aes(x = reorder(level, estimate), y = estimate)) + 
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") + 
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) + 
  coord_flip() + 
  labs(x = "", y = "MMs") + 
  facet_grid(feature ~ ., scales = "free", space = "free") + 
  scale_y_continuous(limits = c(0.2, 0.8), breaks = seq(0.2, 0.8, by = 0.1)) +
  theme_bw() + 
  theme(
    legend.position = "none",
    strip.text.y = element_text(size = 8, face = "bold"),  # Reduced font size to 8
    panel.spacing = unit(1, "lines"),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 12)
  )
print(plot_obj)


# f) Public services
model_mm <- cj(conjoint_services_na, f1, id = ~Response.ID, estimate = "mm",
               feature_labels = list(Age = "Age",
                                     Gender = "Gender",
                                     Occupation = "Occupation",
                                     Clientelism = "Clientelist\n distribution",
                                     Local.politics = "Programmatic\n distribution", 
                                     Tax.preferences = "Tax\n preferences"))

plot_obj <- ggplot(model_mm, aes(x = reorder(level, estimate), y = estimate)) + 
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") + 
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) + 
  coord_flip() + 
  labs(x = "", y = "MMs") + 
  facet_grid(feature ~ ., scales = "free", space = "free") + 
  scale_y_continuous(limits = c(0.2, 0.8), breaks = seq(0.2, 0.8, by = 0.1)) +
  theme_bw() + 
  theme(
    legend.position = "none",
    strip.text.y = element_text(size = 9, face = "bold"),  # Reduced font size to 8
    panel.spacing = unit(1, "lines"),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 12)
  )
print(plot_obj)


# FIGURE D.5: Interaction Effects of Clientelism x Programmatic Distribution on Voter Beliefs, MMs 
# ---------------------------------------------------------------------------------------------------

# a) Fiscal capacity
mms <- mm(conjoint_fiscal_na, f2, id = ~Response.ID)

# Plot
plot_obj <- ggplot(mms, aes(x = reorder(level, estimate), y = estimate)) + 
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") + 
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) + 
  coord_flip() + 
  labs(x = "", y = "MMs") + 
  facet_grid(feature ~ ., scales = "free", space = "free") + 
  theme_bw() + 
  theme(
    legend.position = "none",
    strip.text.y = element_text(size = 8, face = "bold"),  # Reduced font size to 8
    panel.spacing = unit(1, "lines"),
    axis.text.y = element_text(size = 9.5),
    axis.title.x = element_text(size = 9.5)
  ) 
print(plot_obj)


# b) Engage in corruption
mms <- mm(conjoint_corruption_na, f2, id = ~Response.ID)

# Plot
plot_obj <- ggplot(mms, aes(x = reorder(level, estimate), y = estimate)) + 
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") + 
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) + 
  coord_flip() + 
  labs(x = "", y = "MMs") + 
  facet_grid(feature ~ ., scales = "free", space = "free") + 
  theme_bw() + 
  theme(
    legend.position = "none",
    strip.text.y = element_text(size = 8, face = "bold"),  # Reduced font size to 8
    panel.spacing = unit(1, "lines"),
    axis.text.y = element_text(size = 9.5),
    axis.title.x = element_text(size = 9.5)
  ) 
print(plot_obj)


# c) Help in economic distress
mms <- mm(conjoint_help_na, f2, id = ~Response.ID)

# Plot
plot_obj <- ggplot(mms, aes(x = reorder(level, estimate), y = estimate)) + 
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") + 
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) + 
  coord_flip() + 
  labs(x = "", y = "MMs") + 
  facet_grid(feature ~ ., scales = "free", space = "free") + 
  theme_bw() + 
  theme(
    legend.position = "none",
    strip.text.y = element_text(size = 8, face = "bold"),  # Reduced font size to 8
    panel.spacing = unit(1, "lines"),
    axis.text.y = element_text(size = 9.5),
    axis.title.x = element_text(size = 9.5)
  ) 
print(plot_obj)

# d) Help poor people
mms <- mm(conjoint_poverty_na, f2, id = ~Response.ID)

# Plot
plot_obj <- ggplot(mms, aes(x = reorder(level, estimate), y = estimate)) + 
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") + 
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) + 
  coord_flip() + 
  labs(x = "", y = "MMs") + 
  facet_grid(feature ~ ., scales = "free", space = "free") + 
  theme_bw() + 
  theme(
    legend.position = "none",
    strip.text.y = element_text(size = 8, face = "bold"),  # Reduced font size to 8
    panel.spacing = unit(1, "lines"),
    axis.text.y = element_text(size = 9.5),
    axis.title.x = element_text(size = 9.5)
  ) 
print(plot_obj)


# e) Reduce inequality
mms <- mm(conjoint_inequality_na, f2, id = ~Response.ID)

# Plot
plot_obj <- ggplot(mms, aes(x = reorder(level, estimate), y = estimate)) + 
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") + 
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) + 
  coord_flip() + 
  labs(x = "", y = "MMs") + 
  facet_grid(feature ~ ., scales = "free", space = "free") + 
  theme_bw() + 
  theme(
    legend.position = "none",
    strip.text.y = element_text(size = 8, face = "bold"),  # Reduced font size to 8
    panel.spacing = unit(1, "lines"),
    axis.text.y = element_text(size = 9.5),
    axis.title.x = element_text(size = 9.5)
  ) 
print(plot_obj)


# f) Deliver public services
mms <- mm(conjoint_services_na, f2, id = ~Response.ID)

# Plot
plot_obj <- ggplot(mms, aes(x = reorder(level, estimate), y = estimate)) + 
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") + 
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) + 
  coord_flip() + 
  labs(x = "", y = "MMs") + 
  facet_grid(feature ~ ., scales = "free", space = "free") + 
  theme_bw() + 
  theme(
    legend.position = "none",
    strip.text.y = element_text(size = 8, face = "bold"),  # Reduced font size to 8
    panel.spacing = unit(1, "lines"),
    axis.text.y = element_text(size = 9.5),
    axis.title.x = element_text(size = 9.5)
  ) 
print(plot_obj)


# FIGURE D.6: Interaction Effects on likelihood of winning, MMs 
# ---------------------------------------------------------------------------------------------------

# a) Interaction: Clientelism x Programmatic Distribution
mms <- mm(conjoint_redistribution_voting_v2_na, f2, id = ~Response.ID)

# Plot
plot_obj <- ggplot(mms, aes(x = reorder(level, estimate), y = estimate)) + 
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") + 
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) + 
  coord_flip() + 
  labs(x = "", y = "MMs") + 
  facet_grid(feature ~ ., scales = "free", space = "free") + 
  theme_bw() + 
  theme(
    legend.position = "none",
    strip.text.y = element_text(size = 8, face = "bold"),  # Reduced font size to 8
    panel.spacing = unit(1, "lines"),
    axis.text.y = element_text(size = 9.5),
    axis.title.x = element_text(size = 9.5)
  ) 
print(plot_obj)

# b) Interaction: Clientelism x Income taxes
mms <- mm(conjoint_redistribution_voting_v2_na, f3, id = ~Response.ID)

plot_obj <- ggplot(mms, aes(x = reorder(level, estimate), y = estimate)) + 
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") + 
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) + 
  coord_flip() + 
  labs(x = "", y = "MMs") + 
  facet_grid(feature ~ ., scales = "free", space = "free") + 
  theme_bw() + 
  theme(
    legend.position = "none",
    strip.text.y = element_text(size = 8, face = "bold"),  # Reduced font size to 8
    panel.spacing = unit(1, "lines"),
    axis.text.y = element_text(size = 9.5),
    axis.title.x = element_text(size = 9.5)
  ) 
print(plot_obj)


# FIGURE D.7: Interaction Effects of Clientelism x Taxation on Voter Beliefs, MMs 
# ---------------------------------------------------------------------------------------------------

# a) Fiscal capacity
mms <- mm(conjoint_fiscal_na, f3, id = ~Response.ID)

plot_obj <- ggplot(mms, aes(x = reorder(level, estimate), y = estimate)) + 
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") + 
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) + 
  coord_flip() + 
  labs(x = "", y = "MMs") + 
  facet_grid(feature ~ ., scales = "free", space = "free") + 
  theme_bw() + 
  theme(
    legend.position = "none",
    strip.text.y = element_text(size = 8, face = "bold"),  # Reduced font size to 8
    panel.spacing = unit(1, "lines"),
    axis.text.y = element_text(size = 9.5),
    axis.title.x = element_text(size = 9.5)
  ) 
print(plot_obj)

# b) Engage in corruption
mms <- mm(conjoint_corruption_na, f3, id = ~Response.ID)

plot_obj <- ggplot(mms, aes(x = reorder(level, estimate), y = estimate)) + 
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") + 
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) + 
  coord_flip() + 
  labs(x = "", y = "MMs") + 
  facet_grid(feature ~ ., scales = "free", space = "free") + 
  theme_bw() + 
  theme(
    legend.position = "none",
    strip.text.y = element_text(size = 8, face = "bold"),  # Reduced font size to 8
    panel.spacing = unit(1, "lines"),
    axis.text.y = element_text(size = 9.5),
    axis.title.x = element_text(size = 9.5)
  ) 
print(plot_obj)

# c) Help in economic distress
mms <- mm(conjoint_help_na, f3, id = ~Response.ID)

plot_obj <- ggplot(mms, aes(x = reorder(level, estimate), y = estimate)) + 
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") + 
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) + 
  coord_flip() + 
  labs(x = "", y = "MMs") + 
  facet_grid(feature ~ ., scales = "free", space = "free") + 
  theme_bw() + 
  theme(
    legend.position = "none",
    strip.text.y = element_text(size = 8, face = "bold"),  # Reduced font size to 8
    panel.spacing = unit(1, "lines"),
    axis.text.y = element_text(size = 9.5),
    axis.title.x = element_text(size = 9.5)
  ) 
print(plot_obj)

# d) Help poor people
mms <- mm(conjoint_poverty_na, f3, id = ~Response.ID)

plot_obj <- ggplot(mms, aes(x = reorder(level, estimate), y = estimate)) + 
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") + 
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) + 
  coord_flip() + 
  labs(x = "", y = "MMs") + 
  facet_grid(feature ~ ., scales = "free", space = "free") + 
  theme_bw() + 
  theme(
    legend.position = "none",
    strip.text.y = element_text(size = 8, face = "bold"),  # Reduced font size to 8
    panel.spacing = unit(1, "lines"),
    axis.text.y = element_text(size = 9.5),
    axis.title.x = element_text(size = 9.5)
  ) 
print(plot_obj)

# e) Reduce inequality
mms <- mm(conjoint_inequality_na, f3, id = ~Response.ID)

plot_obj <- ggplot(mms, aes(x = reorder(level, estimate), y = estimate)) + 
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") + 
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) + 
  coord_flip() + 
  labs(x = "", y = "MMs") + 
  facet_grid(feature ~ ., scales = "free", space = "free") + 
  theme_bw() + 
  theme(
    legend.position = "none",
    strip.text.y = element_text(size = 8, face = "bold"),  # Reduced font size to 8
    panel.spacing = unit(1, "lines"),
    axis.text.y = element_text(size = 9.5),
    axis.title.x = element_text(size = 9.5)
  ) 
print(plot_obj)


# f) Deliver public services
mms <- mm(conjoint_services_na, f3, id = ~Response.ID)

plot_obj <- ggplot(mms, aes(x = reorder(level, estimate), y = estimate)) + 
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") + 
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) + 
  coord_flip() + 
  labs(x = "", y = "MMs") + 
  facet_grid(feature ~ ., scales = "free", space = "free") + 
  theme_bw() + 
  theme(
    legend.position = "none",
    strip.text.y = element_text(size = 8, face = "bold"),  # Reduced font size to 8
    panel.spacing = unit(1, "lines"),
    axis.text.y = element_text(size = 9.5),
    axis.title.x = element_text(size = 9.5)
  ) 
print(plot_obj)

# FIGURE D.8: Interaction Effects on Candidate Support, Clientelism x Taxation, Poor vs. Non-poor, MMs 
# -----------------------------------------------------------------------------------------------------
mms <- cj(conjoint_redistribution_voting_na, f3, id = ~Response.ID, estimate = "mm", by = ~ind_poverty)
mms <- mms %>% arrange(estimate) # Order results by coefficient sizes

diff_mms <- cj(conjoint_redistribution_voting_na, f3, id = ~Response.ID, estimate = "mm_diff", by = ~ind_poverty)
diff_mms <- diff_mms %>% arrange(estimate) # Order results by coefficient sizes

combined_mms <- rbind(mms, diff_mms)

# Plot
plot_obj <- ggplot(combined_mms, aes(x = reorder(level, estimate), y = estimate)) +
  geom_hline(aes(yintercept = ifelse(BY == "Poor - Non-poor", NA, 0.5)), 
             linetype = "dashed", color = "gray") + 
  geom_hline(aes(yintercept = ifelse(BY == "Poor - Non-poor", 0, NA)), 
             linetype = "dashed", color = "gray") + 
  geom_point(size = 1) + 
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
  scale_y_continuous(limits = c(-0.3, 0.8), breaks = seq(-0.3, 0.8, by = 0.5))
print(plot_obj)


# FIGURE D.9: Interaction Effects on Candidate Support, Clientelism x Programmatic Distribution, Poor vs. Non-poor, MMs 
# -----------------------------------------------------------------------------------------------------
mms <- cj(conjoint_redistribution_voting_na, f2, id = ~Response.ID, estimate = "mm", by = ~ind_poverty)
mms <- mms %>% arrange(estimate) # Order results by coefficient sizes

diff_mms <- cj(conjoint_redistribution_voting_na, f2, id = ~Response.ID, estimate = "mm_diff", by = ~ind_poverty)
diff_mms <- diff_mms %>% arrange(estimate) # Order results by coefficient sizes

combined_mms <- rbind(mms, diff_mms)

# Plot
plot_obj <- ggplot(combined_mms, aes(x = reorder(level, estimate), y = estimate)) +
  geom_hline(aes(yintercept = ifelse(BY == "Poor - Non-poor", NA, 0.5)), 
            linetype = "dashed", color = "gray") + 
  geom_hline(aes(yintercept = ifelse(BY == "Poor - Non-poor", 0, NA)), 
             linetype = "dashed", color = "gray") + 
  geom_point(size = 1) + 
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
  scale_y_continuous(limits = c(-0.3, 0.8), breaks = seq(-0.3, 0.8, by = 0.5))
print(plot_obj)


# FIGURE D.10: Interaction Effects on Likelihood of Winning, Poor vs. Non-poor, MMs 
# -----------------------------------------------------------------------------------------------------

# a) Clientelism x Programatic distribution
mms <- cj(conjoint_redistribution_voting_v2_na, f2, id = ~Response.ID, estimate = "mm", by = ~ind_poverty)
mms <- mms %>% arrange(estimate) # Order results by coefficient sizes

diff_mms <- cj(conjoint_redistribution_voting_v2_na, f2, id = ~Response.ID, estimate = "mm_diff", by = ~ind_poverty)
diff_mms <- diff_mms %>% arrange(estimate) # Order results by coefficient sizes

combined_mms <- rbind(mms, diff_mms)

# Plot
plot_obj <- ggplot(combined_mms, aes(x = reorder(level, estimate), y = estimate)) +
  geom_hline(aes(yintercept = ifelse(BY == "Poor - Non-poor", NA, 0.5)), 
             linetype = "dashed", color = "gray") + 
  geom_hline(aes(yintercept = ifelse(BY == "Poor - Non-poor", 0, NA)), 
             linetype = "dashed", color = "gray") + 
  geom_point(size = 1) + 
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
  scale_y_continuous(limits = c(-0.3, 0.8), breaks = seq(-0.3, 0.8, by = 0.5))
print(plot_obj)

# b) Clientelism x Taxation
mms <- cj(conjoint_redistribution_voting_v2_na, f3, id = ~Response.ID, estimate = "mm", by = ~ind_poverty)
mms <- mms %>% arrange(estimate) # Order results by coefficient sizes

diff_mms <- cj(conjoint_redistribution_voting_v2_na, f3, id = ~Response.ID, estimate = "mm_diff", by = ~ind_poverty)
diff_mms <- diff_mms %>% arrange(estimate) # Order results by coefficient sizes

combined_mms <- rbind(mms, diff_mms)

# Plot
plot_obj <- ggplot(combined_mms, aes(x = reorder(level, estimate), y = estimate)) +
  geom_hline(aes(yintercept = ifelse(BY == "Poor - Non-poor", NA, 0.5)), 
             linetype = "dashed", color = "gray") + 
  geom_hline(aes(yintercept = ifelse(BY == "Poor - Non-poor", 0, NA)), 
             linetype = "dashed", color = "gray") + 
  geom_point(size = 1) + 
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
  scale_y_continuous(limits = c(-0.3, 0.8), breaks = seq(-0.3, 0.8, by = 0.5))
print(plot_obj)



# FIGURE D.11-D.13: Interaction Effects on Voter beliefs, Clientelism x Programatic distribution, Poor vs. Non-poor, MMs 
# -----------------------------------------------------------------------------------------------------

# a) Fiscal
mms <- cj(conjoint_fiscal_na, f2, id = ~Response.ID, estimate = "mm", by = ~ind_poverty)
mms <- mms %>% arrange(estimate) # Order results by coefficient sizes

diff_mms <- cj(conjoint_fiscal_na, f2, id = ~Response.ID, estimate = "mm_diff", by = ~ind_poverty)
diff_mms <- diff_mms %>% arrange(estimate) # Order results by coefficient sizes

combined_mms <- rbind(mms, diff_mms)

# Plot
plot_obj <- ggplot(combined_mms, aes(x = reorder(level, estimate), y = estimate)) +
  geom_hline(aes(yintercept = ifelse(BY == "Poor - Non-poor", NA, 0.5)), 
             linetype = "dashed", color = "gray") + 
  geom_hline(aes(yintercept = ifelse(BY == "Poor - Non-poor", 0, NA)), 
             linetype = "dashed", color = "gray") + 
  geom_point(size = 1) + 
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
  scale_y_continuous(limits = c(-0.3, 0.8), breaks = seq(-0.3, 0.8, by = 0.5))
print(plot_obj)

# b) Corruption
mms <- cj(conjoint_corruption_na, f2, id = ~Response.ID, estimate = "mm", by = ~ind_poverty)
mms <- mms %>% arrange(estimate) # Order results by coefficient sizes

diff_mms <- cj(conjoint_corruption_na, f2, id = ~Response.ID, estimate = "mm_diff", by = ~ind_poverty)
diff_mms <- diff_mms %>% arrange(estimate) # Order results by coefficient sizes

combined_mms <- rbind(mms, diff_mms)

# Plot
plot_obj <- ggplot(combined_mms, aes(x = reorder(level, estimate), y = estimate)) +
  geom_hline(aes(yintercept = ifelse(BY == "Poor - Non-poor", NA, 0.5)), 
             linetype = "dashed", color = "gray") + 
  geom_hline(aes(yintercept = ifelse(BY == "Poor - Non-poor", 0, NA)), 
             linetype = "dashed", color = "gray") + 
  geom_point(size = 1) + 
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
  scale_y_continuous(limits = c(-0.3, 0.8), breaks = seq(-0.3, 0.8, by = 0.5))
print(plot_obj)


# c) Help
mms <- cj(conjoint_help_na, f2, id = ~Response.ID, estimate = "mm", by = ~ind_poverty)
mms <- mms %>% arrange(estimate) # Order results by coefficient sizes

diff_mms <- cj(conjoint_help_na, f2, id = ~Response.ID, estimate = "mm_diff", by = ~ind_poverty)
diff_mms <- diff_mms %>% arrange(estimate) # Order results by coefficient sizes

combined_mms <- rbind(mms, diff_mms)

# Plot
plot_obj <- ggplot(combined_mms, aes(x = reorder(level, estimate), y = estimate)) +
  geom_hline(aes(yintercept = ifelse(BY == "Poor - Non-poor", NA, 0.5)), 
             linetype = "dashed", color = "gray") + 
  geom_hline(aes(yintercept = ifelse(BY == "Poor - Non-poor", 0, NA)), 
             linetype = "dashed", color = "gray") + 
  geom_point(size = 1) + 
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
  scale_y_continuous(limits = c(-0.3, 0.8), breaks = seq(-0.3, 0.8, by = 0.5))
print(plot_obj)

# d) Poor
mms <- cj(conjoint_poverty_na, f2, id = ~Response.ID, estimate = "mm", by = ~ind_poverty)
mms <- mms %>% arrange(estimate) # Order results by coefficient sizes

diff_mms <- cj(conjoint_poverty_na, f2, id = ~Response.ID, estimate = "mm_diff", by = ~ind_poverty)
diff_mms <- diff_mms %>% arrange(estimate) # Order results by coefficient sizes

combined_mms <- rbind(mms, diff_mms)

# Plot
plot_obj <- ggplot(combined_mms, aes(x = reorder(level, estimate), y = estimate)) +
  geom_hline(aes(yintercept = ifelse(BY == "Poor - Non-poor", NA, 0.5)), 
             linetype = "dashed", color = "gray") + 
  geom_hline(aes(yintercept = ifelse(BY == "Poor - Non-poor", 0, NA)), 
             linetype = "dashed", color = "gray") + 
  geom_point(size = 1) + 
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
  scale_y_continuous(limits = c(-0.3, 0.8), breaks = seq(-0.3, 0.8, by = 0.5))
print(plot_obj)


# e) Inequality
mms <- cj(conjoint_inequality_na, f2, id = ~Response.ID, estimate = "mm", by = ~ind_poverty)
mms <- mms %>% arrange(estimate) # Order results by coefficient sizes

diff_mms <- cj(conjoint_inequality_na, f2, id = ~Response.ID, estimate = "mm_diff", by = ~ind_poverty)
diff_mms <- diff_mms %>% arrange(estimate) # Order results by coefficient sizes

combined_mms <- rbind(mms, diff_mms)

# Plot
plot_obj <- ggplot(combined_mms, aes(x = reorder(level, estimate), y = estimate)) +
  geom_hline(aes(yintercept = ifelse(BY == "Poor - Non-poor", NA, 0.5)), 
             linetype = "dashed", color = "gray") + 
  geom_hline(aes(yintercept = ifelse(BY == "Poor - Non-poor", 0, NA)), 
             linetype = "dashed", color = "gray") + 
  geom_point(size = 1) + 
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
  scale_y_continuous(limits = c(-0.3, 0.8), breaks = seq(-0.3, 0.8, by = 0.5))
print(plot_obj)


# e) Service delivery
mms <- cj(conjoint_services_na, f2, id = ~Response.ID, estimate = "mm", by = ~ind_poverty)
mms <- mms %>% arrange(estimate) # Order results by coefficient sizes

diff_mms <- cj(conjoint_services_na, f2, id = ~Response.ID, estimate = "mm_diff", by = ~ind_poverty)
diff_mms <- diff_mms %>% arrange(estimate) # Order results by coefficient sizes

combined_mms <- rbind(mms, diff_mms)

# Plot
plot_obj <- ggplot(combined_mms, aes(x = reorder(level, estimate), y = estimate)) +
  geom_hline(aes(yintercept = ifelse(BY == "Poor - Non-poor", NA, 0.5)), 
             linetype = "dashed", color = "gray") + 
  geom_hline(aes(yintercept = ifelse(BY == "Poor - Non-poor", 0, NA)), 
             linetype = "dashed", color = "gray") + 
  geom_point(size = 1) + 
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
  scale_y_continuous(limits = c(-0.3, 0.8), breaks = seq(-0.3, 0.8, by = 0.5))
print(plot_obj)


# FIGURE D.14 Correlations for Poverty Indicator
#----------------------------------------------------------------
corr <- data.frame(Income=conjoint_redistribution_voting_na$income_st, Meal=conjoint_redistribution_voting_na$meal_st, Medicine=conjoint_redistribution_voting_na$med_st, Bolsa=conjoint_redistribution_voting_na$bolsa_st, Poverty=conjoint_redistribution_voting_na$ind_poverty) 

corr[] <- lapply(corr,as.integer)
ggcorrplot(corr, lab=TRUE)
sjp.corr(corr, corr.method="spearman") # Spearman correlations


# FIGURE D.16: Interaction Effects on Candidate Support, By Gender, MMs 
# -----------------------------------------------------------------------------------------------------

# a) Clientelism x Programatic distribution
mms <- cj(conjoint_redistribution_voting_na, f2, id = ~Response.ID, estimate = "mm", by = ~gender_st)
mms <- mms %>% arrange(estimate) # Order results by coefficient sizes

diff_mms <- cj(conjoint_redistribution_voting_na, f2, id = ~Response.ID, estimate = "mm_diff", by = ~gender_st)
diff_mms <- diff_mms %>% arrange(estimate) # Order results by coefficient sizes

combined_mms <- rbind(mms, diff_mms)

# Plot
plot_obj <- ggplot(combined_mms, aes(x = reorder(level, estimate), y = estimate)) +
  geom_hline(aes(yintercept = ifelse(BY == "Male - Female", NA, 0.5)), 
             linetype = "dashed", color = "gray") + 
  geom_hline(aes(yintercept = ifelse(BY == "Male - Female", 0, NA)), 
             linetype = "dashed", color = "gray") + 
  geom_point(size = 1) + 
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
  scale_y_continuous(limits = c(-0.3, 0.8), breaks = seq(-0.3, 0.8, by = 0.5))
print(plot_obj)

# b) Clientelism x Taxation
mms <- cj(conjoint_redistribution_voting_na, f3, id = ~Response.ID, estimate = "mm", by = ~gender_st)
mms <- mms %>% arrange(estimate) # Order results by coefficient sizes

diff_mms <- cj(conjoint_redistribution_voting_na, f3, id = ~Response.ID, estimate = "mm_diff", by = ~gender_st)
diff_mms <- diff_mms %>% arrange(estimate) # Order results by coefficient sizes

combined_mms <- rbind(mms, diff_mms)

# Plot
plot_obj <- ggplot(combined_mms, aes(x = reorder(level, estimate), y = estimate)) +
  geom_hline(aes(yintercept = ifelse(BY == "Male - Female", NA, 0.5)), 
             linetype = "dashed", color = "gray") + 
  geom_hline(aes(yintercept = ifelse(BY == "Male - Female", 0, NA)), 
             linetype = "dashed", color = "gray") + 
  geom_point(size = 1) + 
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
  scale_y_continuous(limits = c(-0.3, 0.8), breaks = seq(-0.3, 0.8, by = 0.5))
print(plot_obj)


# FIGURE D.17: Interaction Effects on Candidate Support, By Education, MMs 
# -----------------------------------------------------------------------------------------------------

# a) Clientelism x Programatic distribution
mms <- cj(conjoint_redistribution_voting_na, f2, id = ~Response.ID, estimate = "mm", by = ~education_st_v2)
mms <- mms %>% arrange(estimate) # Order results by coefficient sizes

diff_mms <- cj(conjoint_redistribution_voting_na, f2, id = ~Response.ID, estimate = "mm_diff", by = ~education_st_v2)
diff_mms <- diff_mms %>% arrange(estimate) # Order results by coefficient sizes

combined_mms <- rbind(mms, diff_mms)

# Plot
plot_obj <- ggplot(combined_mms, aes(x = reorder(level, estimate), y = estimate)) +
  geom_hline(aes(yintercept = ifelse(BY %in% c("Low - High", "Med - High"), NA, 0.5)), 
             linetype = "dashed", color = "gray") + 
  geom_hline(aes(yintercept = ifelse(BY %in% c("Low - High", "Med - High"), 0, NA)), 
             linetype = "dashed", color = "gray") + 
  geom_point(size = 1) + 
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
  scale_y_continuous(limits = c(-0.5, 1), breaks = seq(-0.5, 1, by = 0.5))
print(plot_obj)

# b) Clientelism x Taxation
mms <- cj(conjoint_redistribution_voting_na, f3, id = ~Response.ID, estimate = "mm", by = ~education_st_v2)
mms <- mms %>% arrange(estimate) # Order results by coefficient sizes

diff_mms <- cj(conjoint_redistribution_voting_na, f3, id = ~Response.ID, estimate = "mm_diff", by = ~education_st_v2)
diff_mms <- diff_mms %>% arrange(estimate) # Order results by coefficient sizes

combined_mms <- rbind(mms, diff_mms)

# Plot
plot_obj <- ggplot(combined_mms, aes(x = reorder(level, estimate), y = estimate)) +
  geom_hline(aes(yintercept = ifelse(BY %in% c("Low - High", "Med - High"), NA, 0.5)), 
             linetype = "dashed", color = "gray") + 
  geom_hline(aes(yintercept = ifelse(BY %in% c("Low - High", "Med - High"), 0, NA)), 
             linetype = "dashed", color = "gray") + 
  geom_point(size = 1) + 
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
  scale_y_continuous(limits = c(-0.5, 1), breaks = seq(-0.5, 1, by = 0.5))
print(plot_obj)


# FIGURE C.18: Interaction Effects on Candidate Support, By Partisanship, MMs 
# -----------------------------------------------------------------------------------------------------

# a) Clientelism x Programatic distribution
mms <- cj(conjoint_redistribution_voting_na, f2, id = ~Response.ID, estimate = "mm", by = ~PT_support_st)
mms <- mms %>% arrange(estimate) # Order results by coefficient sizes

diff_mms <- cj(conjoint_redistribution_voting_na, f2, id = ~Response.ID, estimate = "mm_diff", by = ~PT_support_st)
diff_mms <- diff_mms %>% arrange(estimate) # Order results by coefficient sizes

combined_mms <- rbind(mms, diff_mms)

# Plot
plot_obj <- ggplot(combined_mms, aes(x = reorder(level, estimate), y = estimate)) +
  geom_hline(aes(yintercept = ifelse(BY %in% c("other party - no party affiliation", "PT - no party affiliation"), NA, 0.5)), 
             linetype = "dashed", color = "gray") + 
  geom_hline(aes(yintercept = ifelse(BY %in% c("other party - no party affiliation", "PT - no party affiliation"), 0, NA)), 
             linetype = "dashed", color = "gray") + 
  geom_point(size = 1) + 
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
  scale_y_continuous(limits = c(-0.5, 1), breaks = seq(-0.5, 1, by = 0.5))
print(plot_obj)

# b) Clientelism x Taxation
mms <- cj(conjoint_redistribution_voting_na, f3, id = ~Response.ID, estimate = "mm", by = ~PT_support_st)
mms <- mms %>% arrange(estimate) # Order results by coefficient sizes

diff_mms <- cj(conjoint_redistribution_voting_na, f3, id = ~Response.ID, estimate = "mm_diff", by = ~PT_support_st)
diff_mms <- diff_mms %>% arrange(estimate) # Order results by coefficient sizes

combined_mms <- rbind(mms, diff_mms)

# Plot
plot_obj <- ggplot(combined_mms, aes(x = reorder(level, estimate), y = estimate)) +
  geom_hline(aes(yintercept = ifelse(BY %in% c("other party - no party affiliation", "PT - no party affiliation"), NA, 0.5)), 
             linetype = "dashed", color = "gray") + 
  geom_hline(aes(yintercept = ifelse(BY %in% c("other party - no party affiliation", "PT - no party affiliation"), 0, NA)), 
             linetype = "dashed", color = "gray") + 
  geom_point(size = 1) + 
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
  scale_y_continuous(limits = c(-0.5, 1), breaks = seq(-0.5, 1, by = 0.5))
print(plot_obj)

##----------------------------------------------------------------------------##
##                            END OF SCRIPT 
##----------------------------------------------------------------------------##

