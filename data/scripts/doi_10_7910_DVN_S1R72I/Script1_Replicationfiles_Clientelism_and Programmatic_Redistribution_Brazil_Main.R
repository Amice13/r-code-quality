##----------------------------------------------------------------------------##
##           PROGRAMMATIC REDISTRIBUTION AND CLIENTELISM:                    
##        EVIDENCE FROM A CONJOINT SURVEY EXPERIMENT IN BRAZIL 
##
##                  REPLICATION FILE 1: MAIN PAPER
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
                     "Hmisc"
)

ipak(packages_needed)


### Set WD --------

#path = " "
#setwd(path)
#getwd()  

##----------------------------------------------------------------------------##
## 2) Data Read In                                               
##----------------------------------------------------------------------------##

# loading datasets
load("conjoint_redistribution_voting_na.Rdata")
load("conjoint_redistribution_voting_v2_na.Rdata")

##----------------------------------------------------------------------------##
## 3) FIGURES FOR THE PAPER                                      -
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
# ---- Figure 1: Main Effects ----
##----------------------------------------------------------------------------##

# Define the formula
f1 <- selected ~ Age + Gender + Occupation + Clientelism + Local.politics + Tax.preferences 

## --- (a) Candidate support model ---
model_amce_support <- cj(conjoint_redistribution_voting_na, f1, id = ~Response.ID,
                         feature_labels = list(
                           Age = "Age",
                           Gender = "Gender",
                           Occupation = "Occupation",
                           Clientelism = "Clientelist\ndistribution",
                           Local.politics = "Programmatic\ndistribution",
                           Tax.preferences = "Tax\npreferences"
                         ))

# Save ordered levels by feature and estimate
levels_ordered <- model_amce_support$level[order(model_amce_support$feature, model_amce_support$estimate)]

# Re-factor the 'level' variable
model_amce_support$level <- factor(model_amce_support$level, levels = levels_ordered)

plot_obj <- ggplot(model_amce_support, aes(x = level, y = estimate)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") + 
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) + 
  coord_flip() + 
  labs(x = "", y = "AMCEs") + 
  facet_grid(feature ~ ., scales = "free", space = "free") + 
  scale_y_continuous(limits = c(-0.45, 0.2), breaks = seq(-0.45, 0.2, by = 0.1)) +
  theme_bw() + 
  theme(
    legend.position = "none",
    strip.text.y = element_text(size = 8, face = "bold"),
    panel.spacing = unit(1, "lines"),
    axis.text.y = element_text(size = 9.5),
    axis.title.x = element_text(size = 9.5)
  )
print(plot_obj)


# --- (b) Likelihood of winning model ---
model_amce_win <- cj(conjoint_redistribution_voting_v2_na, f1, id = ~Response.ID,
                     feature_labels = list(
                       Age = "Age",
                       Gender = "Gender",
                       Occupation = "Occupation",
                       Clientelism = "Clientelist\ndistribution",
                       Local.politics = "Programmatic\ndistribution",
                       Tax.preferences = "Tax\npreferences"
                     ))

# Apply same level ordering
model_amce_win$level <- factor(model_amce_win$level, levels = levels_ordered)

plot_obj <- ggplot(model_amce_win, aes(x = level, y = estimate)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") + 
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) + 
  coord_flip() + 
  labs(x = "", y = "AMCEs") + 
  facet_grid(feature ~ ., scales = "free", space = "free") + 
  scale_y_continuous(limits = c(-0.45, 0.2), breaks = seq(-0.45, 0.2, by = 0.1)) +
  theme_bw() + 
  theme(
    legend.position = "none",
    strip.text.y = element_text(size = 8, face = "bold"),
    panel.spacing = unit(1, "lines"),
    axis.text.y = element_text(size = 9.5),
    axis.title.x = element_text(size = 9.5)
  )
print(plot_obj)

##----------------------------------------------------------------------------##
# ---- Figure 2: Interaction Effects ----
##----------------------------------------------------------------------------##

# a) Interaction: Clientelism x Programmatic Distribution
conjoint_redistribution_voting_na$client_locpol <- interaction(conjoint_redistribution_voting_na$Local.politics,
                                                               conjoint_redistribution_voting_na$Clientelism, 
                                                               sep=" and ")
attr(conjoint_redistribution_voting_na$client_locpol, "label") <- "Programmatic Distribution x Clientelism"

# Estimation (MMs)
f2 <- selected ~ client_locpol 
mms <- mm(conjoint_redistribution_voting_na, f2, id = ~Response.ID)

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
conjoint_redistribution_voting_na$client_tax <- interaction(conjoint_redistribution_voting_na$Tax.preferences,
                                                            conjoint_redistribution_voting_na$Clientelism, 
                                                            sep=" and ")
attr(conjoint_redistribution_voting_na$client_tax, "label") <- "Income Taxes x Clientelism"

# Estimation (MMs)
f3 <- selected ~ client_tax 
mms <- mm(conjoint_redistribution_voting_na, f3, id = ~Response.ID)

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


##----------------------------------------------------------------------------##
# ---- Figure 3: Main effect, poor vs. non-poor (candidate support) ----
##----------------------------------------------------------------------------##

# Estimation (MMs)
mms <- cj(conjoint_redistribution_voting_na, f1, id = ~Response.ID, estimate = "mm", by = ~ind_poverty, 
          feature_labels = list(
            Age = "Age",
            Gender = "Gender",
            Occupation = "Occupation",
            Clientelism = "Clientelist\ndistribution",  # Line break added
            Local.politics = "Programmatic\ndistribution",  # Line break added
            Tax.preferences = "Tax\npreferences"  # Line break added 
            ))
diff_mms <- cj(conjoint_redistribution_voting_na, f1, id = ~Response.ID, estimate = "mm_diff", by = ~ind_poverty,
               feature_labels = list(
                 Age = "Age",
                 Gender = "Gender",
                 Occupation = "Occupation",
                 Clientelism = "Clientelist\ndistribution",  # Line break added
                 Local.politics = "Programmatic\ndistribution",  # Line break added
                 Tax.preferences = "Tax\npreferences"  # Line break added 
                 ))
combined_mms <- rbind(mms, diff_mms)

# Apply consistent factor level ordering
combined_mms$level <- factor(combined_mms$level, levels = levels_ordered)

# Plot
plot_obj <- ggplot(combined_mms, aes(x = level, y = estimate)) + 
  geom_hline(aes(yintercept = ifelse(BY == "Poor - Non-poor", NA, 0.5)), 
             linetype = "dashed", color = "gray") + 
  geom_hline(aes(yintercept = ifelse(BY == "Poor - Non-poor", 0, NA)), 
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
  scale_y_continuous(limits = c(-0.15, 0.8), breaks = seq(-0.15, 0.8, by = 0.25))

print(plot_obj)

##----------------------------------------------------------------------------##
# ---- Figure 4: Interaction effects, poor vs. non-poor (candidate support) ----
##----------------------------------------------------------------------------##

# New interaction variable: Clientelism x Programmatic distribution
conjoint_redistribution_voting_na$client_locpol <- interaction(conjoint_redistribution_voting_na$Local.politics,
                                                               conjoint_redistribution_voting_na$Clientelism,
                                                               sep=" and ")
attr(conjoint_redistribution_voting_na$client_locpol, "label") <- "Programmatic Distribution x Clientelism" 

# Estimation (MMs)
f4 <- selected ~ client_locpol # formula for estimation

mms <- cj(conjoint_redistribution_voting_na, f4, id = ~Response.ID, estimate = "mm", by = ~ind_poverty)
mms <- mms %>% arrange(estimate) # Order results by coefficient sizes

diff_mms <- cj(conjoint_redistribution_voting_na, f4, id = ~Response.ID, estimate = "mm_diff", by = ~ind_poverty)
diff_mms <- diff_mms %>% arrange(estimate) # Order results by coefficient sizes

combined_mms <- rbind(mms, diff_mms)
combined_mms2 <- combined_mms %>% 
  filter(BY != "Non-poor") %>%
  mutate(
    clientelism = case_when(
      level %in% c("No change and Patronage",
                   "Local CCT and Patronage",
                   "Health clinics and Patronage") ~ "Patronage",
      level %in% c("No change and Vote buying",
                   "Health clinics and Vote buying",
                   "Local CCT and Vote buying") ~ "Vote buying",
      level %in% c("No change and No clientelism",
                   "Health clinics and No clientelism",
                   "Local CCT and No clientelism") ~ "No clientelism")) %>%
  arrange(BY, clientelism)

# Define custom level order
levels_ordered <- c(
  "No change and No clientelism",
  "Local CCT and No clientelism",
  "Health clinics and No clientelism",
  "No change and Patronage",
  "No change and Vote buying",
  "Health clinics and Patronage",
  "Health clinics and Vote buying",
  "Local CCT and Vote buying",
  "Local CCT and Patronage"
)

# Apply manual factor ordering
combined_mms2$level <- factor(combined_mms2$level, levels = levels_ordered)

# Plot
plot_obj <- ggplot(combined_mms2, aes(x = level, y = estimate)) + 
  geom_hline(aes(yintercept = ifelse(BY == "Poor - Non-poor", NA, 0.5)), 
             linetype = "dashed", color = "gray") + 
  geom_hline(aes(yintercept = ifelse(BY == "Poor - Non-poor", 0, NA)), 
             linetype = "dashed", color = "gray") + 
  geom_point(size = 2) + 
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

##----------------------------------------------------------------------------##
##                            END OF SCRIPT 
##----------------------------------------------------------------------------##
