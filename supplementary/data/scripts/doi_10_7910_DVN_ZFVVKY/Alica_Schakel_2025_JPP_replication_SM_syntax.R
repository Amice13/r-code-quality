
# This is the replication R script for the Supplementary Materials included
# in Alica, Berkay and Schakel, Arjan H. (2025) 'Does multilevel government
# increase legitimacy? Citizens' preferences for subnational authority and
# acceptance of governmental decisions,' Journal of Public Policy, forthcoming.

### Install and open all the necessary packages for the analysis and data visualisation:
my_packages <- c("foreign", "car", "MASS", "stargazer", "ggplot2", "ggeffects",
                 "glm.predict", "haven", "dplyr", "viridis", "brant", "fixest",
                 "DescTools", "devtools", "coreSim", "separationplot",
                 "reshape2", "modmarg", "ggthemes", "gridExtra", "scales",
                 "repmis", "FactoMineR", "corrplot", "psych", "rms", "ordinal",
                 "effects", "erer", "arm", "lme4", "sjPlot", "jtools", "expss",
                 "Hmisc", "gmodels", "pollster", "installr", "tidyr",
                 "marginaleffects", "ggpubr", "patchwork", "modelsummary",
                 "gtsummary")

my_installed_packages <- my_packages %in% rownames(installed.packages())
if (any(my_installed_packages == FALSE)) {
  install.packages(my_packages[!my_installed_packages])
}

invisible(lapply(my_packages, library, character.only = TRUE))

dataset <- readRDS("Alica_Schakel_2025_JPP_replication.rds") # Open dataset.

View(dataset) # View dataset.

## APPENDIX B. Measuring preferences for subnational authority. ####

# IMPORTANT NOTE: Appendix B has its own replication dataset; OPEN Alica_Schakel_2025_JPP_replication_ICVS_dataset.dta
pca_dataset <- readRDS("Alica_Schakel_2025_JPP_replication_ICVS_dataset.rds")
#The sources of the International Constitutional Value Survey (ICVS) are: 
  
#Brown, Alexander J., John Kincaid, Jacob Deem, and Richard Cole. 2016.
#Measuring citizen attachment to federal principles: Results from Australia,
#Canada, the United States, Germany and Great Britain. Paper presented at the
#24th World Congress of Politics Science of the International Political Science
#Association, Poznan, Poland, July 2016. 

#Brown, Alexander J., Jacob Deem, and John Kincaid. 2018. Federal attachment and
#popular trust & confidence: Lessons from the International Constitutional
#Values Survey (Mark 2). Paper presented at the 25th World Congress of Politics
#Science of the International Political Science Association, Brisbane, Australia, July 2018.

### Table B1: Descriptive statistics of survey items to tap preferences for subnational authority. ####

tbl_summary(dataset[c("psa", "sf_intvl", "sh_intvl", "sf1", "sf2", "sf3", "sh1", "sh2", "sh3")],
            type = all_continuous() ~ "continuous2",
            statistic = all_continuous() ~ c("{median} ({p25}, {p75})", "{min}, {max}"))

### Table B3: Principal component analysis ####


#### Table B3a. Principal component analysis: Preference for subnational authority. ####

##### Norway (NOR) ####
data_NOR <- pca_dataset[ which(pca_dataset$country=="NOR"), ]
psa_pca_NOR <- PCA(data_NOR[, c("sf_1",
                                "sf_2",
                                "sf_3",
                                "sh_1",
                                "sh_2",
                                "sh_3")], scale. = TRUE)
summary(psa_pca_NOR)
psa_pca_NOR$eig

psa_items1_6_NOR <- data_NOR[, c("sf_1",
                                 "sf_2",
                                 "sf_3",
                                 "sh_1",
                                 "sh_2",
                                 "sh_3")]

psa_cor_NOR <- cor(psa_items1_6_NOR)

alpha(psa_items1_6_NOR)

dimdesc(psa_pca_NOR)


##### Australia (AUS) ####
data_AUS <- pca_dataset[ which(pca_dataset$country=="AUS"), ]
psa_pca_AUS <- PCA(data_AUS[, c("sf_1",
                                "sf_2",
                                "sf_3",
                                "sh_1",
                                "sh_2",
                                "sh_3")], scale. = TRUE)

psa_pca_AUS$eig
summary(psa_pca_AUS)

psa_items1_6_AUS <- data_AUS[, c("sf_1",
                                 "sf_2",
                                 "sf_3",
                                 "sh_1",
                                 "sh_2",
                                 "sh_3")]

psa_cor_AUS <- cor(psa_items1_6_AUS)


alpha(psa_items1_6_AUS)

dimdesc(psa_pca_AUS)


##### Belgium (BEL) ####
data_BEL <- pca_dataset[ which(pca_dataset$country=="BEL"), ]
psa_pca_BEL <- PCA(data_BEL[, c("sf_1",
                                "sf_2",
                                "sf_3",
                                "sh_1",
                                "sh_2",
                                "sh_3")], scale. = TRUE)

psa_pca_BEL$eig
summary(psa_pca_BEL)

psa_items1_6_BEL <- data_BEL[, c("sf_1",
                                 "sf_2",
                                 "sf_3",
                                 "sh_1",
                                 "sh_2",
                                 "sh_3")]

psa_cor_BEL <- cor(psa_items1_6_BEL)

alpha(psa_items1_6_BEL)

dimdesc(psa_pca_BEL)

##### Canada (CAN) ####
data_CAN <- pca_dataset[ which(pca_dataset$country=="CAN"), ]
psa_pca_CAN <- PCA(data_CAN[, c("sf_1",
                                "sf_2",
                                "sf_3",
                                "sh_1",
                                "sh_2",
                                "sh_3")], scale. = TRUE)

psa_pca_CAN$eig
summary(psa_pca_CAN)

psa_items1_6_CAN <- data_CAN[, c("sf_1",
                                 "sf_2",
                                 "sf_3",
                                 "sh_1",
                                 "sh_2",
                                 "sh_3")]

psa_cor_CAN <- cor(psa_items1_6_CAN)


alpha(psa_items1_6_CAN)

dimdesc(psa_pca_CAN)

##### France (FRA) ####
data_FRA <- pca_dataset[ which(pca_dataset$country=="FRA"), ]
psa_pca_FRA <- PCA(data_FRA[, c("sf_1",
                                "sf_2",
                                "sf_3",
                                "sh_1",
                                "sh_2",
                                "sh_3")], scale. = TRUE)

psa_pca_FRA$eig
summary(psa_pca_FRA)

psa_items1_6_FRA <- data_FRA[, c("sf_1",
                                 "sf_2",
                                 "sf_3",
                                 "sh_1",
                                 "sh_2",
                                 "sh_3")]

psa_cor_FRA <- cor(psa_items1_6_FRA)


alpha(psa_items1_6_FRA)

dimdesc(psa_pca_FRA)


##### Germany (GER) ####
data_GER <- pca_dataset[ which(pca_dataset$country=="GER"), ]
psa_pca_GER <- PCA(data_GER[, c("sf_1",
                                "sf_2",
                                "sf_3",
                                "sh_1",
                                "sh_2",
                                "sh_3")], scale. = TRUE)

psa_pca_GER$eig
summary(psa_pca_GER)

psa_items1_6_GER <- data_GER[, c("sf_1",
                                 "sf_2",
                                 "sf_3",
                                 "sh_1",
                                 "sh_2",
                                 "sh_3")]

psa_cor_GER <- cor(psa_items1_6_GER)


alpha(psa_items1_6_GER)

dimdesc(psa_pca_GER)


##### Switzerland (SWI) ####
data_SWI <- pca_dataset[ which(pca_dataset$country=="SWI"), ]
psa_pca_SWI <- PCA(data_SWI[, c("sf_1",
                                "sf_2",
                                "sf_3",
                                "sh_1",
                                "sh_2",
                                "sh_3")], scale. = TRUE)

summary(psa_pca_SWI)

psa_items1_6_SWI <- data_SWI[, c("sf_1",
                                 "sf_2",
                                 "sf_3",
                                 "sh_1",
                                 "sh_2",
                                 "sh_3")]


alpha(psa_items1_6_SWI)

dimdesc(psa_pca_SWI)

##### United Kingdom (UK) ####
data_UK <- pca_dataset[ which(pca_dataset$country=="UK"), ]
psa_pca_UK <- PCA(data_UK[, c("sf_1",
                              "sf_2",
                              "sf_3",
                              "sh_1",
                              "sh_2",
                              "sh_3")], scale. = TRUE)

summary(psa_pca_UK)

psa_items1_6_UK <- data_UK[, c("sf_1",
                               "sf_2",
                               "sf_3",
                               "sh_1",
                               "sh_2",
                               "sh_3")]

alpha(psa_items1_6_UK)

##### United States (USA) ####

data_USA <- pca_dataset[ which(pca_dataset$country=="USA"), ]
psa_pca_USA <- PCA(data_USA[, c("sf_1",
                                "sf_2",
                                "sf_3",
                                "sh_1",
                                "sh_2",
                                "sh_3")], scale. = TRUE)

summary(psa_pca_USA)

psa_items1_6_USA <- data_USA[, c("sf_1",
                                 "sf_2",
                                 "sf_3",
                                 "sh_1",
                                 "sh_2",
                                 "sh_3")]


alpha(psa_items1_6_USA)

##### All counties (ALL) ####
all.countries.all.items.pca <- PCA(pca_dataset[, c("sf_1",
                                                   "sf_2",
                                                   "sf_3",
                                                   "sh_1",
                                                   "sh_2",
                                                   "sh_3")], scale. = TRUE)

all.countries.all.items.pca$eig
summary(all.countries.all.items.pca)

all.items <- pca_dataset[, c("sf_1",
                             "sf_2",
                             "sf_3",
                             "sh_1",
                             "sh_2",
                             "sh_3")]

psa_cor <- cor(all.items)

psych::alpha(x = all.items)

dimdesc(all.countries.all.items.pca)

CronbachAlpha(all.items)

#### Table B3b. Principal component analysis: Preference for self-rule. ####

##### Norway (NOR) ####
items1_3_NOR <- data_NOR[, c("sf_1",
                             "sf_2",
                             "sf_3")]

self_rule_cor_NOR <- cor(items1_3_NOR)

CronbachAlpha(items1_3_NOR)

shared_rule_pca_NOR <- PCA(data_NOR[, c("sf_1",
                                        "sf_2",
                                        "sf_3")], scale. = TRUE)

##### Australia (AUS) ####

sf_pca_AUS <- PCA(data_AUS[, c("sf_1",
                               "sf_2",
                               "sf_3")], scale. = TRUE)
summary(sf_pca_AUS)

##### Belgium (BEL) ####
sf_pca_BEL <- PCA(data_BEL[, c("sf_1",
                               "sf_2",
                               "sf_3")], scale. = TRUE)
summary(sf_pca_BEL)

##### Canada (CAN) ####
sf_pca_CAN <- PCA(data_CAN[, c("sf_1",
                               "sf_2",
                               "sf_3")], scale. = TRUE)
summary(sf_pca_CAN)

##### France (FRA) ####
sf_pca_FRA <- PCA(data_FRA[, c("sf_1",
                               "sf_2",
                               "sf_3")], scale. = TRUE)
summary(sf_pca_FRA)

##### Germany (GER) ####
sf_pca_GER <- PCA(data_GER[, c("sf_1",
                               "sf_2",
                               "sf_3")], scale. = TRUE)
summary(sf_pca_GER)

##### Switzerland (SWI) ####
sf_pca_SWI <- PCA(data_SWI[, c("sf_1",
                               "sf_2",
                               "sf_3")], scale. = TRUE)
summary(sf_pca_SWI)

##### United Kingdom (UK) ####
sf_pca_UK <- PCA(data_UK[, c("sf_1",
                             "sf_2",
                             "sf_3")], scale. = TRUE)
summary(sf_pca_UK)

##### United States (USA) ####
sf_pca_USA <- PCA(data_USA[, c("sf_1",
                               "sf_2",
                               "sf_3")], scale. = TRUE)
summary(sf_pca_USA)

##### All Countries (ALL) ####
all_countires_self_rule_pca <- PCA(pca_dataset[, c("sf_1",
                                                   "sf_2",
                                                   "sf_3")], scale. = TRUE)

summary(all_countires_self_rule_pca)

all_countires_self_rule_pca$eig

#Then correlation matrix for self-rule items
items1_3 <- pca_dataset[, c("sf_1", "sf_2", "sf_3")]
self_rule_cor <- cor(items1_3)

#Cronbach's Alpha
psych::alpha(x = items1_3)


#### Table B3c. Principal component analysis: Preference for shared rule.####

##### Norway (NOR) ####
items4_6_NOR <- data_NOR[, c("sh_1",
                             "sh_2",
                             "sh_3")]

shared_rule_cor_NOR <- cor(items4_6_NOR)

CronbachAlpha(items4_6_NOR)

shared_rule_pca_NOR <- PCA(data_NOR[, c("sh_1",
                                        "sh_2",
                                        "sh_3")], scale. = TRUE)

##### Australia (AUS) ####
sh_pca_AUS <- PCA(data_AUS[, c("sh_1",
                               "sh_2",
                               "sh_3")], scale. = TRUE)
summary(sh_pca_AUS)

##### Belgium (BEL) ####
sh_pca_BEL <- PCA(data_BEL[, c("sh_1",
                               "sh_2",
                               "sh_3")], scale. = TRUE)
summary(sh_pca_BEL)

##### Canada (CAN) ####
sh_pca_CAN <- PCA(data_CAN[, c("sh_1",
                               "sh_2",
                               "sh_3")], scale. = TRUE)
summary(sh_pca_CAN)

##### France (FRA) ####
sh_pca_FRA <- PCA(data_FRA[, c("sh_1",
                               "sh_2",
                               "sh_3")], scale. = TRUE)
summary(sh_pca_FRA)

##### Germany (GER) ####
sh_pca_GER <- PCA(data_GER[, c("sh_1",
                               "sh_2",
                               "sh_3")], scale. = TRUE)
summary(sh_pca_GER)

##### Switzerland (SWI) ####
sh_pca_SWI <- PCA(data_SWI[, c("sh_1",
                               "sh_2",
                               "sh_3")], scale. = TRUE)
summary(sh_pca_SWI)

##### United Kingdom (UK) ####
sh_pca_UK <- PCA(data_UK[, c("sh_1",
                             "sh_2",
                             "sh_3")], scale. = TRUE)
summary(sh_pca_UK)

##### United States (USA) ####
sh_pca_USA <- PCA(data_USA[, c("sh_1",
                               "sh_2",
                               "sh_3")], scale. = TRUE)
summary(sh_pca_USA)

##### All countries (ALL) ####

all_countires_shared_rule_pca <- PCA(pca_dataset[, c("sh_1",
                                                     "sh_2",
                                                     "sh_3")], scale. = TRUE)
summary(all_countires_shared_rule_pca)

all_countires_shared_rule_pca$eig

#Then correlation matrix for self-rule items
items4_6 <- pca_dataset[, c("sh_1", "sh_2", "sh_3")]
shared_rule_cor <- cor(items4_6)

#Cronbach's Alpha Shared Rule:
psych::alpha(x = items4_6)


## APPENDIX C: Full model results. ####

# IMPORTANT NOTE: The point estimates are statistically significantly different
# from each other when their 84% CIs do not overlap and the 95% CI of the
# DIFFERENCE between two point estimates does not include zero; see data and methods

### Table C1. Model 1-Q1-Q3-Q5: Regression table. ####

m1.psa.munbase <- feols(first_answer ~ psa * tier_m | responseid,
                        data = dataset)

summary(m1.psa.munbase) # See coefficients of the model

m1.psa.coubase <- feols(first_answer ~ psa * tier_c | responseid,
                        data = dataset)

summary(m1.psa.coubase) # See coefficients of the model

m1.psa.natbase <- feols(first_answer ~ psa * tier_n | responseid,
                        data = dataset)

summary(m1.psa.natbase) # See coefficients of the model

### Figure C1. Model 1-Q1-Q3-Q5: Marginal effects. ####

##### Figure C1A-C1B. Decision taken by the municipality. ####

#Model:
m1.psa.munbase <- feols(first_answer ~ psa * tier_m | responseid,
                        data = dataset)

summary(m1.psa.munbase) # See coefficients of the model

preds.psalow.munbase <- plot_predictions(m1.psa.munbase,
                                         condition = list(tier_m = list("C", "N"),
                                                          psa = 0),
                                         conf_level = 0.84,
                                         gray = T,
                                         draw = F) # Get the estimates & CIs when psa low

preds.high.munbase <- plot_predictions(m1.psa.munbase,
                                       condition = list(tier_m = list("C", "N"),
                                                        psa = 1),
                                       conf_level = 0.84,
                                       gray = T,
                                       draw = F) # Get the estimates & CIs when psa high


plot1.m1.munbase <- plot_predictions(m1.psa.munbase,
                                     condition = list(tier_m = list("C", "N"),
                                                      psa = "minmax"),
                                     conf_level = 0.95,
                                     gray = T)

plot1.m1.munbase <-  plot1.m1.munbase + geom_hline(yintercept = 2.073779 , #m.bas
                                                   linetype = 1) +
  labs(x="Decision taken by",
       y="Willingness to accept decision (4-point merged scale)",
       shape="Preference for\nSubnational Authority",
       title="A: Base category (line) is the decision taken\nby respondent's municipality") +
  scale_x_discrete(labels = c("County",
                              "National government"))  +
  scale_shape_discrete(labels = c("Min (= 0)", "Max (= 1)"))

plot1.m1.munbase <- plot1.m1.munbase + geom_linerange(data=preds.psalow.munbase,
                                                      aes(y = estimate,
                                                          x = tier_m,
                                                          ymin = conf.low,
                                                          ymax = conf.high),
                                                      position = position_nudge(x = -0.037),
                                                      linewidth = 1.2) +
  geom_linerange(data = preds.high.munbase,
                 aes(y = estimate,
                     x = tier_m,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.037),
                 linewidth = 1.2) + ylim(1.25, 2.50)

plot1.m1.munbase <- plot1.m1.munbase + theme_classic() +
  theme(legend.position = c(0.35, 0.9),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18, hjust = 0.5, vjust = 1.5, face = "bold"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 13))

plot1.m1.munbase$layers[[1]]$geom_params$fatten = 8

ggsave("plot1_left_munbase.jpeg", plot1.m1.munbase, width = 8, height = 6)

##### Line graph with M as baseline #

# 84% CIs

plot2.m1.munbase.84ci <- plot_predictions(m1.psa.munbase,
                                          condition = list("psa", "tier_m"),
                                          conf_level = 0.84,
                                          gray = T)

plot2.m1.munbase.84ci <- plot2.m1.munbase.84ci +
  labs(x = "Preference for subnational authority",
       y = "",
       linetype = "Decision taken by",
       title="B: Base category (line) is the decision taken\nby respondent's municipality")

plot2.m1.munbase.84ci <- plot2.m1.munbase.84ci +
  scale_linetype_manual(labels = c("Municipality",
                                   "County",
                                   "National government"),
                        values = c("solid", "dotted", "dashed")) +
  theme_classic() + theme(legend.position = c(0.8, 0.2),
                          axis.title = element_text(size = 16),
                          axis.text = element_text(size = 14),
                          plot.title = element_text(size = 18,
                                                    hjust = .5,
                                                    vjust = 1.5,
                                                    face = "bold"),
                          legend.title = element_text(size = 14),
                          legend.text = element_text(size = 13)
  ) + ylim(1.25, 2.50)


ggsave("plot1_right_munbase_84CI.jpeg", plot2.m1.munbase.84ci, width = 11,
       height = 7)

#### Put Figure C1A and C1B in one combined plot:

combined.plots.1 <- ggarrange(plot1.m1.munbase, plot2.m1.munbase.84ci,
                              nrow = 1, ncol = 2)

ggsave(combined.plots.1, filename = 'Appendix_Figure_C1A_C1B.jpeg',
         width = 13, height = 6)


##### Figure C1C-C1D. Decision taken by the county. ####

#Model:
m1.psa.coubase <- feols(first_answer ~ psa * tier_c | responseid,
                        data = dataset)

summary(m1.psa.coubase) # See coefficients of the model

preds.psalow.coubase <- plot_predictions(m1.psa.coubase,
                                         condition = list(tier_c = list("M", "N"),
                                                          psa = 0),
                                         conf_level = 0.84,
                                         gray = T,
                                         draw = F) # Get the estimates & CIs when psa low

preds.high.coubase <- plot_predictions(m1.psa.coubase,
                                       condition = list(tier_c = list("M", "N"),
                                                        psa = 1),
                                       conf_level = 0.84,
                                       gray = T,
                                       draw = F) # Get the estimates & CIs when psa high


plot1.m1.coubase <- plot_predictions(m1.psa.coubase,
                                     condition = list(tier_c = list("M", "N"),
                                                      psa = "minmax"),
                                     conf_level = 0.95,
                                     gray = T)

plot1.m1.coubase <-  plot1.m1.coubase + geom_hline(yintercept = 1.829764 ,#c.base
                                                   linetype = 1) +
  labs(x="Decision taken by",
       y="Willingness to accept decision (4-point merged scale)",
       shape="Preference for Subnational Authority",
       title="C: Base category (line) is the decision made\nby respondent's county") +
  scale_x_discrete(labels = c("Municipality",
                              "National government"))  +
  scale_shape_discrete(labels = c("Min (= 0)", "Max (= 1)"))

plot1.m1.coubase <- plot1.m1.coubase + geom_linerange(data=preds.psalow.coubase,
                                                      aes(y = estimate,
                                                          x = tier_c,
                                                          ymin = conf.low,
                                                          ymax = conf.high),
                                                      position = position_nudge(x = -0.037),
                                                      linewidth = 1.2) +
  geom_linerange(data = preds.high.coubase,
                 aes(y = estimate,
                     x = tier_c,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.037),
                 linewidth = 1.2) + ylim(1.25, 2.50)

plot1.m1.coubase <- plot1.m1.coubase + theme_classic() +
  theme(legend.position = c(0.35, 0.9),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18, hjust = 0.5, vjust = 1.5, face = "bold"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 13))

plot1.m1.coubase$layers[[1]]$geom_params$fatten = 8

ggsave("plot1_left_coubase.jpeg", plot1.m1.coubase, width = 8, height = 6)

#Right-hand plot (84% CIs):
plot2.m1.coubase.84ci <- plot_predictions(m1.psa.coubase,
                                          condition = list("psa", "tier_c"),
                                          conf_level = 0.84,
                                          gray = T)

plot2.m1.coubase.84ci <- plot2.m1.coubase.84ci +
  labs(x = "Preference for subnational authority",
       y = "",
       linetype = "Decision taken by",
       title="D: Base category (line) is the decision made\nby respondent's county")

plot2.m1.coubase.84ci <- plot2.m1.coubase.84ci +
  scale_linetype_manual(labels = c("County",
                                   "Municipality",
                                   "National government"),
                        values = c("solid", "dotted", "dashed")) +
  theme_classic() + theme(legend.position = c(0.8, 0.2),
                          axis.title = element_text(size = 16),
                          axis.text = element_text(size = 14),
                          plot.title = element_text(size = 18,
                                                    hjust = .5,
                                                    vjust = 1.5,
                                                    face = "bold"),
                          legend.title = element_text(size = 14),
                          legend.text = element_text(size = 13)
  ) + ylim(1.25, 2.50)


ggsave("plot1_right_coubase_84CI.jpeg", plot2.m1.coubase.84ci, width = 11,
       height = 7)

#### Put Figure C1C and Figure C1D in one combined plot:

combined.plots.2 <- ggarrange(plot1.m1.coubase, plot2.m1.coubase.84ci,
                              nrow = 1, ncol = 2)

ggsave(combined.plots.2, filename = 'Appendix_Figure_C1C_C1D.jpeg',
         width = 13, height = 6)


##### Figure C1E-C1F. Decision taken by the national government. ####

#Model:
m1.psa.natbase <- feols(first_answer ~ psa * tier_n | responseid,
                        data = dataset)

summary(m1.psa.natbase) # See coefficients of the model

preds.psalow.natbase <- plot_predictions(m1.psa.natbase,
                                         condition = list(tier_n = list("M", "C"),
                                                          psa = 0),
                                         conf_level = 0.84,
                                         gray = T,
                                         draw = F) # Get the estimates & CIs when psa low

preds.high.natbase <- plot_predictions(m1.psa.natbase,
                                       condition = list(tier_n = list("M", "C"),
                                                        psa = 1),
                                       conf_level = 0.84,
                                       gray = T,
                                       draw = F) # Get the estimates & CIs when psa high


plot1.m1.natbase <- plot_predictions(m1.psa.natbase,
                                     condition = list(tier_n = list("M", "C"),
                                                      psa = "minmax"),
                                     conf_level = 0.95,
                                     gray = T)

plot1.m1.natbase <-  plot1.m1.natbase + geom_hline(yintercept = 2.096457 , #n.bas
                                                   linetype = 1) +
  labs(x="Decision taken by",
       y="Willingness to accept decision (4-point merged scale)",
       shape="Preference for\nSubnational Authority",
       title="E: Base category (line) is the decision made\nby the national government") +
  scale_x_discrete(labels = c("Municipality",
                              "County"))  +
  scale_shape_discrete(labels = c("Min (= 0)", "Max (= 1)"))

plot1.m1.natbase <- plot1.m1.natbase + geom_linerange(data=preds.psalow.natbase,
                                                      aes(y = estimate,
                                                          x = tier_n,
                                                          ymin = conf.low,
                                                          ymax = conf.high),
                                                      position = position_nudge(x = -0.037),
                                                      linewidth = 1.2) +
  geom_linerange(data = preds.high.natbase,
                 aes(y = estimate,
                     x = tier_n,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.037),
                 linewidth = 1.2) + ylim(1.25, 2.50)

plot1.m1.natbase <- plot1.m1.natbase + theme_classic() +
  theme(legend.position = c(0.35, 0.9),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18, hjust = 0.5, vjust = 1.5, face = "bold"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 13))

plot1.m1.natbase$layers[[1]]$geom_params$fatten = 8

ggsave("plot1_left_natbase.jpeg", plot1.m1.natbase, width = 8, height = 6)


##### Line graph with N as baseline #

# 84% CIs

plot2.m1.natbase.84ci <- plot_predictions(m1.psa.natbase,
                                          condition = list("psa", "tier_n"),
                                          conf_level = 0.84,
                                          gray = T)

plot2.m1.natbase.84ci <- plot2.m1.natbase.84ci +
  labs(x = "Preference for subnational authority",
       y = "",
       linetype = "Decision taken by",
       title="F: Base category (line) is the decision made\nby the national government")

plot2.m1.natbase.84ci <- plot2.m1.natbase.84ci +
  scale_linetype_manual(labels = c("National government",
                                   "Municipality",
                                   "County"),
                        values = c("solid", "dashed", "dotted")) +
  theme_classic() + theme(legend.position = c(0.8, 0.2),
                          axis.title = element_text(size = 16),
                          axis.text = element_text(size = 14),
                          plot.title = element_text(size = 18,
                                                    hjust = .5,
                                                    vjust = 1.5,
                                                    face = "bold"),
                          legend.title = element_text(size = 14),
                          legend.text = element_text(size = 13)
  ) + ylim(1.25, 2.50)


ggsave("plot1_right_natbase_84CI.jpeg", plot2.m1.natbase.84ci, width = 11,
       height = 7)

#### Put Figures C1E and C1F in one combined plot:

combined.plots.3 <- ggarrange(plot1.m1.natbase, plot2.m1.natbase.84ci,
                              nrow = 1, ncol = 2)

ggsave(combined.plots.3, filename = 'Appendix_Figure_C1E_C1F.jpeg',
         width = 13, height = 6)


### Table C2a. Model 2-Q2-Q4-Q6, base-category = municipality: Regression table. ####
m2.supportM_merged <- feols(second_answer ~ first_answer_factor * psa * support_m + tier_m | responseid,
                            data = dataset)
summary(m2.supportM_merged)
modelsummary(m2.supportM_merged, stars=T, output = "Q2_mun_merged.docx")


### Table C2b. Model 2-Q2-Q4-Q6, base-category = county: Regression table. ####

m2.supportC <- feols(second_answer~ first_answer_factor * psa * Support_c + tier_c | responseid,
                     data = dataset)
summary(m2.supportC)

modelsummary::modelsummary(m2.supportC, stars = T,
                           output = "county_merged.docx", shape = term ~ model + statistic)


### Table C2c. Model 2-Q2-Q4-Q6, base-category = national government: Regression table. ####

m2.supportN <- feols(second_answer ~ first_answer_factor * psa * Support + tier_n | responseid,
                     data = dataset)
summary(m2.supportN)

modelsummary::modelsummary(m2.supportN, stars = T,
                           output = "nat_merged.docx", shape = term ~ model + statistic)

## APPENDIX D: Robustness test I: DV with five answer categories (non-merged) ####

### Table D1: Model 1-Q1-Q3-Q5: DV 5 answer categories. ####

model1.unmerged.psa.munbase <- feols(first_answer_original ~ psa * tier_m | responseid,
                        data = dataset)

summary(model1.unmerged.psa.munbase) # See coefficients of the model

model1.unmerged.psa.coubase <- feols(first_answer_original ~ psa * tier_c | responseid,
                        data = dataset)

summary(model1.unmerged.psa.coubase) # See coefficients of the model

model1.unmerged.psa.natbase <- feols(first_answer_original ~ psa * tier_n | responseid,
                        data = dataset)

summary(model1.unmerged.psa.natbase) # See coefficients of the model

### Figure D1. Model 1-Q1-Q3-Q5: DV 5 answer categories. ####

#### Figure D1A-D1B. Base category: municipality. ####

#Model:
model1.unmerged.psa.munbase <- feols(first_answer_original ~ psa * tier_m | responseid,
                        data = dataset)

summary(model1.unmerged.psa.munbase) # See coefficients of the model

preds.psalow.munbase.unmerged <- plot_predictions(model1.unmerged.psa.munbase,
                                         condition = list(tier_m = list("C", "N"),
                                                          psa = 0),
                                         conf_level = 0.84,
                                         gray = T,
                                         draw = F) # Get the estimates & CIs when psa low

preds.high.munbase.unmerged <- plot_predictions(model1.unmerged.psa.munbase,
                                       condition = list(tier_m = list("C", "N"),
                                                        psa = 1),
                                       conf_level = 0.84,
                                       gray = T,
                                       draw = F) # Get the estimates & CIs when psa high


plot1.m1.munbase.unmerged <- plot_predictions(model1.unmerged.psa.munbase,
                                     condition = list(tier_m = list("C", "N"),
                                                      psa = "minmax"),
                                     conf_level = 0.95,
                                     gray = T)

plot1.m1.munbase.unmerged <-  plot1.m1.munbase.unmerged + geom_hline(yintercept = 2.080595 , #m.bas
                                                   linetype = 1) +
  labs(x="Decision taken by",
       y="Willingness to accept decision (5-point unmerged scale)",
       shape="Preference for\nSubnational Authority",
       title="A: Base category (line) is the decision taken\nby respondent's municipality") +
  scale_x_discrete(labels = c("County",
                              "National government"))  +
  scale_shape_discrete(labels = c("Min (= 0)", "Max (= 1)"))

plot1.m1.munbase.unmerged <- plot1.m1.munbase.unmerged + geom_linerange(data=preds.psalow.munbase.unmerged,
                                                      aes(y = estimate,
                                                          x = tier_m,
                                                          ymin = conf.low,
                                                          ymax = conf.high),
                                                      position = position_nudge(x = -0.037),
                                                      linewidth = 1.2) +
  geom_linerange(data = preds.high.munbase.unmerged,
                 aes(y = estimate,
                     x = tier_m,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.037),
                 linewidth = 1.2) + ylim(1.25, 2.50)

plot1.m1.munbase.unmerged <- plot1.m1.munbase.unmerged + theme_classic() +
  theme(legend.position = c(0.35, 0.9),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18, hjust = 0.5, vjust = 1.5, face = "bold"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 13)) + annotate("text", x=2,
                                                          y=2.5,
                                                          label="*",
                                                          size=8)

plot1.m1.munbase.unmerged$layers[[1]]$geom_params$fatten = 8

ggsave("plot1_left_munbase_unmerged.jpeg", plot1.m1.munbase.unmerged, width = 8, height = 6)

##### Line graph with M as baseline #

# 84% CIs

plot2.m1.munbase.84ci.unmerged <- plot_predictions(model1.unmerged.psa.munbase,
                                          condition = list("psa", "tier_m"),
                                          conf_level = 0.84,
                                          gray = T)

plot2.m1.munbase.84ci.unmerged <- plot2.m1.munbase.84ci.unmerged +
  labs(x = "Preference for subnational authority",
       y = "",
       linetype = "Decision taken by",
       title="B: Base category (line) is the decision taken\nby respondent's municipality")

plot2.m1.munbase.84ci.unmerged <- plot2.m1.munbase.84ci.unmerged +
  scale_linetype_manual(labels = c("Municipality",
                                   "County",
                                   "National government"),
                        values = c("solid", "dotted", "dashed")) +
  theme_classic() + theme(legend.position = c(0.8, 0.2),
                          axis.title = element_text(size = 16),
                          axis.text = element_text(size = 14),
                          plot.title = element_text(size = 18,
                                                    hjust = .5,
                                                    vjust = 1.5,
                                                    face = "bold"),
                          legend.title = element_text(size = 14),
                          legend.text = element_text(size = 13)
  ) + ylim(1.25, 2.50)


ggsave("plot1_right_munbase_84CI_unmerged.jpeg", plot2.m1.munbase.84ci.unmerged, width = 11,
       height = 7)

#### Put Figure D1A and D1B in one combined plot:

combined.plots.1.unmerged <- ggarrange(plot1.m1.munbase.unmerged, plot2.m1.munbase.84ci.unmerged,
                              nrow = 1, ncol = 2)

ggsave(combined.plots.1.unmerged, filename = 'Figure_D1A_D1B.jpeg',
       width = 13, height = 6)


#### Figure D1C-D1D. Base category: county. ####

#Model:
model1.unmerged.psa.coubase <- feols(first_answer_original ~ psa * tier_c | responseid,
                                     data = dataset)

summary(model1.unmerged.psa.coubase) # See coefficients of the model


preds.psalow.coubase.unmerged <- plot_predictions(model1.unmerged.psa.coubase,
                                         condition = list(tier_c = list("M", "N"),
                                                          psa = 0),
                                         conf_level = 0.84,
                                         gray = T,
                                         draw = F) # Get the estimates & CIs when psa low

preds.high.coubase.unmerged <- plot_predictions(model1.unmerged.psa.coubase,
                                       condition = list(tier_c = list("M", "N"),
                                                        psa = 1),
                                       conf_level = 0.84,
                                       gray = T,
                                       draw = F) # Get the estimates & CIs when psa high


plot1.m1.coubase.unmerged <- plot_predictions(model1.unmerged.psa.coubase,
                                     condition = list(tier_c = list("M", "N"),
                                                      psa = "minmax"),
                                     conf_level = 0.95,
                                     gray = T)

plot1.m1.coubase.unmerged <-  plot1.m1.coubase.unmerged + geom_hline(yintercept = 1.823922 ,#c.base
                                                   linetype = 1) +
  labs(x="Decision taken by",
       y="Willingness to accept decision (5-point unmerged scale)",
       shape="Preference for Subnational Authority",
       title="C: Base category (line) is the decision made\nby respondent's county") +
  scale_x_discrete(labels = c("Municipality",
                              "National government"))  +
  scale_shape_discrete(labels = c("Min (= 0)", "Max (= 1)"))

plot1.m1.coubase.unmerged <- plot1.m1.coubase.unmerged + geom_linerange(data=preds.psalow.coubase.unmerged,
                                                      aes(y = estimate,
                                                          x = tier_c,
                                                          ymin = conf.low,
                                                          ymax = conf.high),
                                                      position = position_nudge(x = -0.037),
                                                      linewidth = 1.2) +
  geom_linerange(data = preds.high.coubase.unmerged,
                 aes(y = estimate,
                     x = tier_c,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.037),
                 linewidth = 1.2) + ylim(1.25, 2.50)

plot1.m1.coubase.unmerged <- plot1.m1.coubase.unmerged + theme_classic() +
  theme(legend.position = c(0.35, 0.9),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18, hjust = 0.5, vjust = 1.5, face = "bold"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 13)) + annotate("text", x=2,
                                                        y=2.5,
                                                        label="*",
                                                        size=8)

plot1.m1.coubase.unmerged$layers[[1]]$geom_params$fatten = 8

ggsave("plot1_left_coubase.unmerged.jpeg", plot1.m1.coubase.unmerged, width = 8, height = 6)

#Right-hand plot (84% CIs):
plot2.m1.coubase.84ci.unmerged <- plot_predictions(model1.unmerged.psa.coubase,
                                          condition = list("psa", "tier_c"),
                                          conf_level = 0.84,
                                          gray = T)

plot2.m1.coubase.84ci.unmerged <- plot2.m1.coubase.84ci.unmerged +
  labs(x = "Preference for subnational authority",
       y = "",
       linetype = "Decision taken by",
       title="D: Base category (line) is the decision made\nby respondent's county")

plot2.m1.coubase.84ci.unmerged <- plot2.m1.coubase.84ci.unmerged +
  scale_linetype_manual(labels = c("County",
                                   "Municipality",
                                   "National government"),
                        values = c("solid", "dotted", "dashed")) +
  theme_classic() + theme(legend.position = c(0.8, 0.2),
                          axis.title = element_text(size = 16),
                          axis.text = element_text(size = 14),
                          plot.title = element_text(size = 18,
                                                    hjust = .5,
                                                    vjust = 1.5,
                                                    face = "bold"),
                          legend.title = element_text(size = 14),
                          legend.text = element_text(size = 13)
  ) + ylim(1.25, 2.50)


ggsave("plot1_right_coubase_84CI.jpeg", plot2.m1.coubase.84ci.unmerged, width = 11,
       height = 7)

#### Put Figure C1C and Figure C1D in one combined plot:

combined.plots.2.unmerged <- ggarrange(plot1.m1.coubase.unmerged, plot2.m1.coubase.84ci.unmerged,
                              nrow = 1, ncol = 2)

ggsave(combined.plots.2.unmerged, filename = 'Figure_D1C_D1D.jpeg',
       width = 13, height = 6)



#### Figure D1E-D1F. Base category = national government. ####

#Model:
model1.unmerged.psa.natbase <- feols(first_answer_original ~ psa * tier_n | responseid,
                        data = dataset)

summary(model1.unmerged.psa.natbase) # See coefficients of the model

preds.psalow.natbase.unmerged <- plot_predictions(model1.unmerged.psa.natbase,
                                         condition = list(tier_n = list("M", "C"),
                                                          psa = 0),
                                         conf_level = 0.84,
                                         gray = T,
                                         draw = F) # Get the estimates & CIs when psa low

preds.high.natbase.unmerged <- plot_predictions(model1.unmerged.psa.natbase,
                                       condition = list(tier_n = list("M", "C"),
                                                        psa = 1),
                                       conf_level = 0.84,
                                       gray = T,
                                       draw = F) # Get the estimates & CIs when psa high


plot1.m1.natbase.unmerged <- plot_predictions(model1.unmerged.psa.natbase,
                                     condition = list(tier_n = list("M", "C"),
                                                      psa = "minmax"),
                                     conf_level = 0.95,
                                     gray = T)

plot1.m1.natbase.unmerged <-  plot1.m1.natbase.unmerged + geom_hline(yintercept = 2.095484 , #n.bas
                                                   linetype = 1) +
  labs(x="Decision taken by",
       y="Willingness to accept decision (5-point unmerged scale)",
       shape="Preference for\nSubnational Authority",
       title="E: Base category (line) is the decision made\nby the national government") +
  scale_x_discrete(labels = c("Municipality",
                              "County"))  +
  scale_shape_discrete(labels = c("Min (= 0)", "Max (= 1)"))

plot1.m1.natbase.unmerged <- plot1.m1.natbase.unmerged + geom_linerange(data=preds.psalow.natbase.unmerged,
                                                      aes(y = estimate,
                                                          x = tier_n,
                                                          ymin = conf.low,
                                                          ymax = conf.high),
                                                      position = position_nudge(x = -0.037),
                                                      linewidth = 1.2) +
  geom_linerange(data = preds.high.natbase.unmerged,
                 aes(y = estimate,
                     x = tier_n,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.037),
                 linewidth = 1.2) + ylim(1.25, 2.50)

plot1.m1.natbase.unmerged <- plot1.m1.natbase.unmerged + theme_classic() +
  theme(legend.position = c(0.8, 0.9),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18, hjust = 0.5, vjust = 1.5, face = "bold"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 13)) + annotate("text", x = c(1,2),
                                                          y = c(2.3, 2.01),
                                                          label = c("*", "*"),
                                                          size = 8)

plot1.m1.natbase.unmerged$layers[[1]]$geom_params$fatten = 8

ggsave("plot1_left_natbase_unmerged.jpeg", plot1.m1.natbase.unmerged, width = 8, height = 6)

##### Line graph with N as baseline #

# 84% CIs

plot2.m1.natbase.84ci.unmerged <- plot_predictions(model1.unmerged.psa.natbase,
                                          condition = list("psa", "tier_n"),
                                          conf_level = 0.84,
                                          gray = T)

plot2.m1.natbase.84ci.unmerged <- plot2.m1.natbase.84ci.unmerged +
  labs(x = "Preference for subnational authority",
       y = "",
       linetype = "Decision taken by",
       title="F: Base category (line) is the decision made\nby the national government")

plot2.m1.natbase.84ci.unmerged <- plot2.m1.natbase.84ci.unmerged +
  scale_linetype_manual(labels = c("National government",
                                   "Municipality",
                                   "County"),
                        values = c("solid", "dashed", "dotted")) +
  theme_classic() + theme(legend.position = c(0.8, 0.2),
                          axis.title = element_text(size = 16),
                          axis.text = element_text(size = 14),
                          plot.title = element_text(size = 18,
                                                    hjust = .5,
                                                    vjust = 1.5,
                                                    face = "bold"),
                          legend.title = element_text(size = 14),
                          legend.text = element_text(size = 13)
  ) + ylim(1.25, 2.50)


ggsave("plot1_right_natbase_84CI_unmerged.jpeg", plot2.m1.natbase.84ci.unmerged, width = 11,
       height = 7)

#### Put Figures C1E and C1 F in one combined plot:

combined.plots.3.unmerged <- ggarrange(plot1.m1.natbase.unmerged, plot2.m1.natbase.84ci.unmerged,
                              nrow = 1, ncol = 2)

ggsave(combined.plots.3.unmerged, filename = 'Figure_D1E_D1F.jpeg',
       width = 13, height = 6)



### Table D2: Model 2-Q2-Q4-Q6: DV 5 answer categories. ####

dataset$first_answer_original_factor <- as.factor(dataset$first_answer_original)

# First, run the models
m2.supportM <- feols(second_answer_original ~ first_answer_original_factor * psa * support_m + tier_m | responseid,
                     data = dataset) # Model 1: municipality as the base category
summary(m2.supportM)

m2.supportC <- feols(second_answer_original ~ first_answer_original_factor * psa * Support_c + tier_c | responseid,
                     data = dataset) # Model 1: county as the base category
summary(m2.supportC)

m2.supportN <- feols(second_answer_original ~ first_answer_original_factor * psa * Support + tier_n | responseid,
                     data = dataset) # Model 1: national government as the base category
summary(m2.supportN)

# Then, get the regression tables

# Table D2a. Model 2-Q2-Q4-Q6-base-category = municipality: DV 5 answer categories.
modelsummary::modelsummary(m2.supportM, stars = T, shape = term ~ model + statistic,
             output = "model_mun_non_merged.docx")

# Table D2b. Model 2-Q2-Q4-Q6-base-category = county: DV 5 answer categories.
modelsummary::modelsummary(m2.supportC, stars = T, shape = term ~ model + statistic,
                           output = "model_cou_non_merged.docx")

# Table D2c. Model 2-Q2-Q4-Q6-base-category = national government: DV 5 answer categories.
modelsummary::modelsummary(m2.supportN, stars = T, shape = term ~ model + statistic,
                           output = "model_nat_non_merged.docx")

### Figure D2A-D2C: Municipality decides to close kindergarten ####
#### Figure D2A: Municipality decides, county supports #####

preds.psalow.mundec <- plot_predictions(m2.supportM,
                                        newdata = datagrid(first_answer_original_factor = 1:5,
                                                           psa = .44,
                                                           support_m = c("Respondent's County Supports"),
                                                           tier_m = c("M")),
                                        by = c("support_m", "psa", "first_answer_original_factor"),
                                        gray = T,
                                        conf_level = 0.84,
                                        draw = F) # Get the estimates & CIs when psa low

preds.high.mundec <- plot_predictions(m2.supportM,
                                      newdata = datagrid(first_answer_original_factor = 1:5,
                                                         psa = .88,
                                                         support_m = c("Respondent's County Supports"),
                                                         tier_m = c("M")),
                                      by = c("support_m", "psa", "first_answer_original_factor"),
                                      gray = T,
                                      conf_level = 0.84,
                                      draw = F) # Get the estimates & CIs when psa high

plot2.supportM_cou <- plot_predictions(m2.supportM,
                                       newdata = datagrid(first_answer_original_factor = 1:5,
                                                          psa = c(.44, .88),
                                                          support_m = c("Respondent's County Supports"),
                                                          tier_m = c("M")),
                                       by = c("support_m", "psa", "first_answer_original_factor"),
                                       gray = T,
                                       conf_level = 0.95,
                                       draw = F)


p2_m_c <- ggplot(plot2.supportM_cou,
                 aes(x = first_answer_original_factor,
                     y = estimate,
                     group = first_answer_original_factor, shape = psa)) +
  geom_hline(yintercept = c(1, 2, 3, 4, 5), color='gray') + 
  facet_grid(.~support_m) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = psa),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_M_c <- p2_m_c +
  labs(x="",
       y="",
       shape="Preference for Subnational Authority") +
  scale_shape_discrete(labels= c("Mean - 1SD (0.44)", "Mean + 1SD (0.88)")) +
  theme_classic()  + theme(legend.position = "bottom",
                           plot.title = element_text(hjust = 0.5, vjust = 1.5))


p2_M_c <- p2_M_c + geom_linerange(data=preds.psalow.mundec,
                                  aes(y = estimate,
                                      x = first_answer_original_factor,
                                      ymin = conf.low,
                                      ymax = conf.high),
                                  position = position_nudge(x = -0.05),
                                  linewidth = 1.2) +
  geom_linerange(data = preds.high.mundec,
                 aes(y = estimate,
                     x = first_answer_original_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)


my.plot.mun.1 <- p2_M_c  + labs(tag = "A:") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        axis.title.y = element_text(size=14),
        strip.text.x = element_text(size = 12),
        plot.tag = element_text(face="bold"),
        plot.tag.position = c(0.16, 0.974)) + 
    annotate("text", x=3, y=3.35, label="*", size=8)


#### Figure D2B: Municipality decides, national government supports ####

preds.low.mundec.2 <- plot_predictions(m2.supportM,
                                       newdata = datagrid(first_answer_original_factor = 1:5,
                                                          psa = .44,
                                                          support_m = c("National Government Supports"),
                                                          tier_m = c("M")),
                                       by = c("support_m", "psa", "first_answer_original_factor"),
                                       gray = T,
                                       conf_level = 0.84,
                                       draw = F) # Get the estimates & CIs when psa low

preds.high.mundec.2 <- plot_predictions(m2.supportM,
                                        newdata = datagrid(first_answer_original_factor = 1:5,
                                                           psa = .88,
                                                           support_m = c("National Government Supports"),
                                                           tier_m = c("M")),
                                        by = c("support_m", "psa", "first_answer_original_factor"),
                                        gray = T,
                                        conf_level = 0.84,
                                        draw = F) # Get the estimates & CIs when psa high

plot2.supportM_nat <- plot_predictions(m2.supportM,
                                       newdata = datagrid(first_answer_original_factor = 1:5,
                                                          psa = c(.44, .88),
                                                          support_m = c("National Government Supports"),
                                                          tier_m = c("M")),
                                       by = c("support_m", "psa", "first_answer_original_factor"),
                                       gray = T,
                                       conf_level = 0.95,
                                       draw = F)


p2_m_n <- ggplot(plot2.supportM_nat,
                 aes(x = first_answer_original_factor,
                     y = estimate,
                     group = first_answer_original_factor, shape = psa)) +
  geom_hline(yintercept = c(1, 2, 3, 4, 5), color='gray') + 
  facet_grid(.~support_m) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = psa),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_M_n <- p2_m_n +
  labs(x="",
       y="",
       shape="Preference for Subnational Authority") +
  scale_shape_discrete(labels= c("Mean - 1SD (0.44)", "Mean + 1SD (0.88)")) +
  theme_classic() + theme(legend.position = "bottom",
                          plot.title = element_text(hjust = 0.5, vjust = 1.5))


p2_M_n <- p2_M_n + geom_linerange(data=preds.low.mundec.2,
                                  aes(y = estimate,
                                      x = first_answer_original_factor,
                                      ymin = conf.low,
                                      ymax = conf.high),
                                  position = position_nudge(x = -0.05),
                                  linewidth = 1.2) +
  geom_linerange(data = preds.high.mundec.2,
                 aes(y = estimate,
                     x = first_answer_original_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)



my.plot.mun.2 <- p2_M_n  + labs(tag = "B:") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        strip.text.x = element_text(size = 12),
        plot.tag = element_text(face="bold"),
        plot.tag.position = c(0.16, 0.974))


#### Figure D2C: Municipality decides, County & National Government Support ####

preds.low.mundec.3 <- plot_predictions(m2.supportM,
                                       newdata = datagrid(first_answer_original_factor = 1:5,
                                                          psa = .44,
                                                          support_m = c("County & National Government Support"),
                                                          tier_m = c("M")),
                                       by = c("support_m", "psa", "first_answer_original_factor"),
                                       gray = T,
                                       conf_level = 0.84,
                                       draw = F) # Get the estimates & CIs when psa low

preds.high.mundec.3 <- plot_predictions(m2.supportM,
                                        newdata = datagrid(first_answer_original_factor = 1:5,
                                                           psa = .88,
                                                           support_m = c("County & National Government Support"),
                                                           tier_m = c("M")),
                                        by = c("support_m", "psa", "first_answer_original_factor"),
                                        gray = T,
                                        conf_level = 0.84,
                                        draw = F) # Get the estimates & CIs when psa high

plot2.supportM_counat <- plot_predictions(m2.supportM,
                                          newdata = datagrid(first_answer_original_factor = 1:5,
                                                             psa = c(.44, .88),
                                                             support_m = c("County & National Government Support"),
                                                             tier_m = c("M")),
                                          by = c("support_m", "psa", "first_answer_original_factor"),
                                          gray = T,
                                          conf_level = 0.95,
                                          draw = F)


p2_m_cn <- ggplot(plot2.supportM_counat,
                  aes(x = first_answer_original_factor,
                      y = estimate,
                      group = first_answer_original_factor, shape = psa)) +
  geom_hline(yintercept = c(1, 2, 3, 4, 5), color='gray') + 
  facet_grid(.~support_m) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = psa),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_M_cn <- p2_m_cn +
  labs(x="", y="",
       shape="Preference for Subnational Authority") +
  scale_shape_discrete(labels= c("Mean - 1SD (0.44)", "Mean + 1SD (0.88)")) +
  theme_classic() + theme(legend.position = "bottom",
                          plot.title = element_text(hjust = 0.5, vjust = 1.5))


p2_M_cn <- p2_M_cn + geom_linerange(data=preds.low.mundec.3,
                                    aes(y = estimate,
                                        x = first_answer_original_factor,
                                        ymin = conf.low,
                                        ymax = conf.high),
                                    position = position_nudge(x = -0.05),
                                    linewidth = 1.2) +
  geom_linerange(data = preds.high.mundec.3,
                 aes(y = estimate,
                     x = first_answer_original_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)



my.plot.mun.3 <- p2_M_cn  + labs(tag = "C:") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        strip.text.x = element_text(size = 12),
        plot.tag = element_text(face="bold"),
        plot.tag.position = c(0.16, 0.974))


# Gather Figure D2A-D2C together horizontally:
p1 <- ggarrange(my.plot.mun.1, my.plot.mun.2, my.plot.mun.3, nrow = 1, ncol = 3,
                common.legend = T, legend = "none") + plot_annotation(title="Municipality decides to close kindergarten") &
  theme(plot.title = element_text(hjust = .5, size = 18), legend.text = element_blank())





### Figure D2D-D2F. County decides to close upper secondary school. #####

#### Figure D2D: County decides to close upper secondary school, and municipality supports ####

preds.psalow.coudec <- plot_predictions(m2.supportC,
                                        newdata = datagrid(first_answer_original_factor = 1:5,
                                                           psa = .44,
                                                           Support_c = c("Respondent's Municipality Supports"),
                                                           tier_c = c("C")),
                                        by = c("Support_c", "psa", "first_answer_original_factor"),
                                        gray = T,
                                        conf_level = 0.84,
                                        draw = F) # Get the estimates & CIs when psa low

preds.high.coudec <- plot_predictions(m2.supportC,
                                      newdata = datagrid(first_answer_original_factor = 1:5,
                                                         psa = .88,
                                                         Support_c = c("Respondent's Municipality Supports"),
                                                         tier_c = c("C")),
                                      by = c("Support_c", "psa", "first_answer_original_factor"),
                                      gray = T,
                                      conf_level = 0.84,
                                      draw = F) # Get the estimates & CIs when psa high

plot2.supportC_mun <- plot_predictions(m2.supportC,
                                       newdata = datagrid(first_answer_original_factor = 1:5,
                                                          psa = c(.44, .88),
                                                          Support_c = c("Respondent's Municipality Supports"),
                                                          tier_c = c("C")),
                                       by = c("Support_c", "psa", "first_answer_original_factor"),
                                       gray = T,
                                       conf_level = 0.95,
                                       draw = F)



p2_c_m <- ggplot(plot2.supportC_mun,
                 aes(x = first_answer_original_factor,
                     y = estimate,
                     group = first_answer_original_factor, shape = psa)) +
  geom_hline(yintercept = c(1, 2, 3, 4, 5), color='gray') + 
  facet_grid(.~Support_c) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = psa),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_C_m <- p2_c_m +
  labs(x="",
       y="Willingness to accept decision\nwhen support is provided by another tier",
       shape="Preference for Subnational Authority") +
  scale_shape_discrete(labels= c("Mean - 1SD (0.44)", "Mean + 1SD (0.88)")) +
  theme_classic()  + theme(legend.position = "bottom",
                           plot.title = element_text(hjust = 0.5, vjust = 1.5),
                           axis.title.y = element_text(size = 18, vjust = 4.5)) 



p2_C_m <- p2_C_m + geom_linerange(data=preds.psalow.coudec,
                                  aes(y = estimate,
                                      x = first_answer_original_factor,
                                      ymin = conf.low,
                                      ymax = conf.high),
                                  position = position_nudge(x = -0.05),
                                  linewidth = 1.2) +
  geom_linerange(data = preds.high.coudec,
                 aes(y = estimate,
                     x = first_answer_original_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)



my.plot.cou.1 <- p2_C_m  + labs(tag = "D:") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        axis.title.y = element_text(size=14),
        strip.text.x = element_text(size = 12),
        plot.tag = element_text(face="bold"),
        plot.tag.position = c(0.16, 0.974)) + annotate("text", x=3, y=3.5, label="**", size=8)


#### Figure D2E: County decides to close upper secondary school and national government supports ####

preds.low.coudec.2 <- plot_predictions(m2.supportC,
                                       newdata = datagrid(first_answer_original_factor = 1:5,
                                                          psa = .44,
                                                          Support_c = c("National Government Supports"),
                                                          tier_c = c("C")),
                                       by = c("Support_c", "psa", "first_answer_original_factor"),
                                       gray = T,
                                       conf_level = 0.84,
                                       draw = F) # Get the estimates & CIs when psa low

preds.high.coudec.2 <- plot_predictions(m2.supportC,
                                        newdata = datagrid(first_answer_original_factor = 1:5,
                                                           psa = .88,
                                                           Support_c = c("National Government Supports"),
                                                           tier_c = c("C")),
                                        by = c("Support_c", "psa", "first_answer_original_factor"),
                                        gray = T,
                                        conf_level = 0.84,
                                        draw = F) # Get the estimates & CIs when psa high

plot2.supportC_nat <- plot_predictions(m2.supportC,
                                       newdata = datagrid(first_answer_original_factor = 1:5,
                                                          psa = c(.44, .88),
                                                          Support_c = c("National Government Supports"),
                                                          tier_c = c("C")),
                                       by = c("Support_c", "psa", "first_answer_original_factor"),
                                       gray = T,
                                       conf_level = 0.95,
                                       draw = F)


p2_c_n <- ggplot(plot2.supportC_nat,
                 aes(x = first_answer_original_factor,
                     y = estimate,
                     group = first_answer_original_factor, shape = psa)) +
  geom_hline(yintercept = c(1, 2, 3, 4, 5), color='gray') + 
  facet_grid(.~Support_c) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = psa),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_C_n <- p2_c_n +
  labs(x="",
       y="",
       shape="Preference for Subnational Authority") +
  scale_shape_discrete(labels= c("Mean - 1SD (0.44)", "Mean + 1SD (0.88)")) +
  theme_classic() + theme(legend.position = "bottom",
                          plot.title = element_text(hjust = 0.5, vjust = 1.5))


p2_C_n <- p2_C_n + geom_linerange(data=preds.low.coudec.2,
                                  aes(y = estimate,
                                      x = first_answer_original_factor,
                                      ymin = conf.low,
                                      ymax = conf.high),
                                  position = position_nudge(x = -0.05),
                                  linewidth = 1.2) +
  geom_linerange(data = preds.high.coudec.2,
                 aes(y = estimate,
                     x = first_answer_original_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)



my.plot.cou.2 <- p2_C_n  + labs(tag = "E:") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        strip.text.x = element_text(size = 12),
        plot.tag = element_text(face="bold"),
        plot.tag.position = c(0.16, 0.974)) + annotate("text", x=1, y=2.1, label="*", size=8)

#### Figure D2F: County decides to close upper secondary school and municipality and national government support ####

preds.low.coudec.3 <- plot_predictions(m2.supportC,
                                       newdata = datagrid(first_answer_original_factor = 1:5,
                                                          psa = .44,
                                                          Support_c = c("Municipality & National Government Support"),
                                                          tier_c = c("C")),
                                       by = c("Support_c", "psa", "first_answer_original_factor"),
                                       gray = T,
                                       conf_level = 0.84,
                                       draw = F) # Get the estimates & CIs when psa low

preds.high.coudec.3 <- plot_predictions(m2.supportC,
                                        newdata = datagrid(first_answer_original_factor = 1:5,
                                                           psa = .88,
                                                           Support_c = c("Municipality & National Government Support"),
                                                           tier_c = c("C")),
                                        by = c("Support_c", "psa", "first_answer_original_factor"),
                                        gray = T,
                                        conf_level = 0.84,
                                        draw = F) # Get the estimates & CIs when psa high

plot2.supportC_munnat <- plot_predictions(m2.supportC,
                                          newdata = datagrid(first_answer_original_factor = 1:5,
                                                             psa = c(.44, .88),
                                                             Support_c = c("Municipality & National Government Support"),
                                                             tier_c = c("C")),
                                          by = c("Support_c", "psa", "first_answer_original_factor"),
                                          gray = T,
                                          conf_level = 0.95,
                                          draw = F)


p2_c_mn <- ggplot(plot2.supportC_munnat,
                  aes(x = first_answer_original_factor,
                      y = estimate,
                      group = first_answer_original_factor, shape = psa)) +
  geom_hline(yintercept = c(1, 2, 3, 4, 5), color='gray') + 
  facet_grid(.~Support_c) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = psa),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_C_mn <- p2_c_mn +
  labs(x="", y="",
       shape="Preference for Subnational Authority") +
  scale_shape_discrete(labels= c("Mean - 1SD (0.44)", "Mean + 1SD (0.88)")) +
  theme_classic() + theme(legend.position = "bottom",
                          plot.title = element_text(hjust = 0.5, vjust = 1.5)) +
  ylim(1,5)


p2_C_mn <- p2_C_mn + geom_linerange(data=preds.low.coudec.3,
                                    aes(y = estimate,
                                        x = first_answer_original_factor,
                                        ymin = conf.low,
                                        ymax = conf.high),
                                    position = position_nudge(x = -0.05),
                                    linewidth = 1.2) +
  geom_linerange(data = preds.high.coudec.3,
                 aes(y = estimate,
                     x = first_answer_original_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)



my.plot.cou.3 <- p2_C_mn  + labs(tag = "F:") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        strip.text.x = element_text(size = 12),
        plot.tag = element_text(face="bold"),
        plot.tag.position = c(0.15, 0.972)) 

# Gather Figures D2D-D2F together horizontally:

p2 <- ggarrange(my.plot.cou.1, my.plot.cou.2, my.plot.cou.3, nrow = 1, ncol = 3,
                common.legend = T, legend = "none") 

# Title for the plot:

p2 <- p2 + plot_annotation(title = "\nCounty decides to close upper secondary school") &
  theme(plot.title = element_text(hjust = .5, size = 18))



### Figure D2G-D2I. National government decides to close a department of a university (college)####

#### Figure D2G: National government decides to close a department of a university (college) and municipality supports ####

preds.psalow.natdec <- plot_predictions(m2.supportN,
                                        newdata = datagrid(first_answer_original_factor = 1:5,
                                                           psa = .44,
                                                           Support = c("Respondent's Municipality Supports"),
                                                           tier_n = c("N")),
                                        by = c("Support", "psa", "first_answer_original_factor"),
                                        gray = T,
                                        conf_level = 0.84,
                                        draw = F) # Get the estimates & CIs when psa low

preds.high.natdec <- plot_predictions(m2.supportN,
                                      newdata = datagrid(first_answer_original_factor = 1:5,
                                                         psa = .88,
                                                         Support = c("Respondent's Municipality Supports"),
                                                         tier_n = c("N")),
                                      by = c("Support", "psa", "first_answer_original_factor"),
                                      gray = T,
                                      conf_level = 0.84,
                                      draw = F) # Get the estimates & CIs when psa high

plot2.supportN_mun <- plot_predictions(m2.supportN,
                                       newdata = datagrid(first_answer_original_factor = 1:5,
                                                          psa = c(.44, .88),
                                                          Support = c("Respondent's Municipality Supports"),
                                                          tier_n = c("N")),
                                       by = c("Support", "psa", "first_answer_original_factor"),
                                       gray = T,
                                       conf_level = 0.95,
                                       draw = F)



p2_n_m <- ggplot(plot2.supportN_mun,
                 aes(x = first_answer_original_factor,
                     y = estimate,
                     group = first_answer_original_factor, shape = psa)) +
  geom_hline(yintercept = c(1, 2, 3, 4, 5), color='gray') + 
  facet_grid(.~Support) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = psa),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_N_m <- p2_n_m +
  labs(x="",
       y="",
       shape="Preference for Subnational Authority") +
  scale_shape_discrete(labels= c("Mean - 1SD (0.44)", "Mean + 1SD (0.88)")) +
  theme_classic()  + theme(legend.position = "bottom",
                           plot.title = element_text(hjust = 0.5, vjust = 1.5))


p2_N_m <- p2_N_m + geom_linerange(data=preds.psalow.natdec,
                                  aes(y = estimate,
                                      x = first_answer_original_factor,
                                      ymin = conf.low,
                                      ymax = conf.high),
                                  position = position_nudge(x = -0.05),
                                  linewidth = 1.2) +
  geom_linerange(data = preds.high.natdec,
                 aes(y = estimate,
                     x = first_answer_original_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)



my.plot.nat.1 <- p2_N_m  + labs(tag = "G:") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        axis.title.y = element_text(size=14),
        strip.text.x = element_text(size = 12),
        plot.tag = element_text(face="bold"),
        plot.tag.position = c(0.16, 0.974)) + annotate("text", x=c(2,3,4),
                                                         y=c(2.9,3.6,4.15),
                                                         label=c("**", "**", "*"), size=8)


#### Figure D2H: National government decides to close a department of a university (college) and county supports ####

preds.low.natdec.2 <- plot_predictions(m2.supportN,
                                       newdata = datagrid(first_answer_original_factor = 1:5,
                                                          psa = .44,
                                                          Support = c("Respondent's County Supports"),
                                                          tier_n = c("N")),
                                       by = c("Support", "psa", "first_answer_original_factor"),
                                       gray = T,
                                       conf_level = 0.84,
                                       draw = F) # Get the estimates & CIs when psa low

preds.high.natdec.2 <- plot_predictions(m2.supportN,
                                        newdata = datagrid(first_answer_original_factor = 1:5,
                                                           psa = .88,
                                                           Support = c("Respondent's County Supports"),
                                                           tier_n = c("N")),
                                        by = c("Support", "psa", "first_answer_original_factor"),
                                        gray = T,
                                        conf_level = 0.84,
                                        draw = F) # Get the estimates & CIs when psa high

plot2.supportN_cou <- plot_predictions(m2.supportN,
                                       newdata = datagrid(first_answer_original_factor = 1:5,
                                                          psa = c(.44, .88),
                                                          Support = c("Respondent's County Supports"),
                                                          tier_n = c("N")),
                                       by = c("Support", "psa", "first_answer_original_factor"),
                                       gray = T,
                                       conf_level = 0.95,
                                       draw = F)


p2_n_c <- ggplot(plot2.supportN_cou,
                 aes(x = first_answer_original_factor,
                     y = estimate,
                     group = first_answer_original_factor, shape = psa)) +
  geom_hline(yintercept = c(1, 2, 3, 4, 5), color='gray') + 
  facet_grid(.~Support) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = psa),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_N_c <- p2_n_c +
  labs(x="Willingness to accept decision",
       y="",
       shape="Preference for Subnational Authority") +
  scale_shape_discrete(labels= c("Mean - 1SD (0.44)", "Mean + 1SD (0.88)")) +
  theme_classic() + theme(legend.position = "bottom",
                          plot.title = element_text(hjust = 0.5, vjust = 1.5),
                          axis.title = element_text(vjust = -0.25))


p2_N_c <- p2_N_c + geom_linerange(data=preds.low.natdec.2,
                                  aes(y = estimate,
                                      x = first_answer_original_factor,
                                      ymin = conf.low,
                                      ymax = conf.high),
                                  position = position_nudge(x = -0.05),
                                  linewidth = 1.2) +
  geom_linerange(data = preds.high.natdec.2,
                 aes(y = estimate,
                     x = first_answer_original_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)



my.plot.nat.2 <- p2_N_c  + labs(tag = "H:") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        strip.text.x = element_text(size = 12),
        plot.tag = element_text(face="bold"),
        plot.tag.position = c(0.16, 0.974)) + annotate("text",
                                                           x=c(1,2,3,4),
                                                           y=c(2.025,2.7,3.45,4.05),
                                                           label=c("*", "**", "**", "*"),
                                                           size=8)


#### Figure D2I: National government decides to close a department of a university (college) and municipality and county support ####

preds.low.natdec.3 <- plot_predictions(m2.supportN,
                                       newdata = datagrid(first_answer_original_factor = 1:5,
                                                          psa = .44,
                                                          Support = c("Municipality & County Support"),
                                                          tier_n = c("N")),
                                       by = c("Support", "psa", "first_answer_original_factor"),
                                       gray = T,
                                       conf_level = 0.84,
                                       draw = F) # Get the estimates & CIs when psa low

preds.high.natdec.3 <- plot_predictions(m2.supportN,
                                        newdata = datagrid(first_answer_original_factor = 1:5,
                                                           psa = .88,
                                                           Support = c("Municipality & County Support"),
                                                           tier_n = c("N")),
                                        by = c("Support", "psa", "first_answer_original_factor"),
                                        gray = T,
                                        conf_level = 0.84,
                                        draw = F) # Get the estimates & CIs when psa high

plot2.supportN_muncou <- plot_predictions(m2.supportN,
                                          newdata = datagrid(first_answer_original_factor = 1:5,
                                                             psa = c(.44, .88),
                                                             Support = c("Municipality & County Support"),
                                                             tier_n = c("N")),
                                          by = c("Support", "psa", "first_answer_original_factor"),
                                          gray = T,
                                          conf_level = 0.95,
                                          draw = F)


p2_n_mc <- ggplot(plot2.supportN_muncou,
                  aes(x = first_answer_original_factor,
                      y = estimate,
                      group = first_answer_original_factor, shape = psa)) +
  geom_hline(yintercept = c(1, 2, 3, 4, 5), color='gray') + 
  facet_grid(.~Support) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = psa),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_N_mc <- p2_n_mc +
  labs(x="", y="",
       shape="Preference for Subnational Authority") +
  scale_shape_discrete(labels= c("Mean - 1SD (0.44)", "Mean + 1SD (0.88)")) +
  theme_classic() + theme(legend.position = "bottom",
                          plot.title = element_text(hjust = 0.5, vjust = 1.5),
                          legend.text = element_text(size = 16)) 


p2_N_mc <- p2_N_mc + geom_linerange(data=preds.low.natdec.3,
                                    aes(y = estimate,
                                        x = first_answer_original_factor,
                                        ymin = conf.low,
                                        ymax = conf.high),
                                    position = position_nudge(x = -0.05),
                                    linewidth = 1.2) +
  geom_linerange(data = preds.high.natdec.3,
                 aes(y = estimate,
                     x = first_answer_original_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)



my.plot.nat.3 <- p2_N_mc  + labs(tag = "I:") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        strip.text.x = element_text(size = 12),
        plot.tag = element_text(face="bold"),
        plot.tag.position = c(0.16, 0.974)) + annotate("text",
                                                           x=c(1,2,3,4),
                                                           y=c(2.2,2.9,3.6,4.45),
                                                           label=c("*", "**", "**", "*"),
                                                           size=8)

p3 <- ggarrange(my.plot.nat.1, my.plot.nat.2, my.plot.nat.3, nrow = 1, ncol = 3,
                common.legend = T, legend = "bottom") + plot_annotation(title = "\nNational government decides to close \na department of a university (college)") &
  theme(plot.title = element_text(hjust = .5, size = 18)) 

my.plot.all <- ggarrange(p1, p2, p3, nrow = 3, ncol = 1)

# Save Figure D2 as a .jpeg file:
ggsave("Figure_D2.jpeg",
       width = 12, height = 17.5)


## APPENDIX E. Robustness test II: Preferences for self-rule and preferences for shared rule. ####

# IMPORTANT NOTE: The point estimates are statistically significantly different
# from each other (p<0.05) when their 84% CIs do not overlap and the 95% CI of
# the DIFFERENCE between two point estimates does not include zero; see data and
# methods.

### Table E1A. Model 1-Q1-Q3-Q5: Preference for self-rule. ####

m1.sf.munbase <- feols(first_answer ~ sf_intvl * tier_m | responseid,
                       data = dataset)

m1.sf_intvl.coubase <- feols(first_answer ~ sf_intvl * tier_c | responseid,
                             data = dataset)

m1.sf_intvl.natbase <- feols(first_answer ~ sf_intvl * tier_n | responseid,
                       data = dataset)

#### Figure E1Aa-E1Ab. Decision taken by the municipality to close a kindergarten.####
m1.sf.munbase <- feols(first_answer ~ sf_intvl * tier_m | responseid,
                       data = dataset)

modelsummary(m1.sf.munbase, stars = T, output = "Q1_mun_sf_int_robust.docx")

#Plot the results:
preds.sflow.munbase <- plot_predictions(m1.sf.munbase,
                                        condition = list(tier_m = list("C", "N"),
                                                         sf_intvl= 0),
                                        conf_level = 0.84,
                                        gray = T,
                                        draw = F) # Get the estimates & CIs when sf_intvllow

preds.sfhigh.munbase <- plot_predictions(m1.sf.munbase,
                                         condition = list(tier_m = list("C", "N"),
                                                          sf_intvl= 1),
                                         conf_level = 0.84,
                                         gray = T,
                                         draw = F) # Get the estimates & CIs when sf_intvlhigh

preds.base.munbase <- plot_predictions(m1.sf.munbase,
                                       condition = list(tier_m = list("M"),
                                                        sf_intvl= 1),
                                       conf_level = 0.84,
                                       gray = T,
                                       draw = F)

plot1.m1.munbase <- plot_predictions(m1.sf.munbase,
                                     condition = list(tier_m = list("C", "N"),
                                                      sf_intvl= "minmax"),
                                     conf_level = 0.95,
                                     gray = T)

plot1.m1.munbase <-  plot1.m1.munbase + geom_hline(yintercept = 2.082658, #m.bas
                                                   linetype = 1) +
  labs(x="Decision taken by",
       y="Willingness to accept decision (4-point merged scale)",
       shape="Preference for Self-Rule",
       title="a: Base category (line) is the decision taken by the municipality.") +
  scale_x_discrete(labels = c("County",
                              "National government")) + ylim(1.25, 2.50) +
  scale_shape_discrete(labels = c("Min (= 0)", "Max (= 1)"))

plot1.m1.munbase <- plot1.m1.munbase + geom_linerange(data=preds.sflow.munbase,
                                                      aes(y = estimate,
                                                          x = tier_m,
                                                          ymin = conf.low,
                                                          ymax = conf.high),
                                                      position = position_nudge(x = -0.0363),
                                                      linewidth = 1.2) +
  geom_linerange(data = preds.sfhigh.munbase,
                 aes(y = estimate,
                     x = tier_m,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.037),
                 linewidth = 1.2)

plot1.m1.munbase <- plot1.m1.munbase + theme_classic() +
  theme(legend.position = c(0.25, 0.85),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(hjust = 0.5, vjust = 1.5, size = 16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14)) + annotate('text', x=2, y=2.5,
                                                          label='**', size=8)

plot1.m1.munbase$layers[[1]]$geom_params$fatten = 8

ggsave("Q1_sf_intvl_left_munbase.jpeg", plot1.m1.munbase, width = 8, height = 6)

# Line graph with M as baseline #

# 95% CIs

plot2.m1.munbase.sf <- plot_predictions(m1.sf.munbase,
                                        condition = list("sf_intvl", "tier_m"),
                                        conf_level = 0.95,
                                        gray = T)

plot2.m1.munbase.sf <- plot2.m1.munbase.sf +
  labs(x = "Preference for self-rule",
       y = "",
       linetype = "Decision taken by",
       title="b: Base category (line) is the decision taken by the municipality.")

plot2.m1.munbase.sf <- plot2.m1.munbase.sf +
  scale_linetype_manual(labels = c("Municipality",
                                   "County",
                                   "National government"),
                        values = c("solid", "dotted", "dashed")) +
  theme_classic() + theme(legend.position = c(0.8, 0.85),
                          legend.title = element_text(size = 14),
                          legend.text = element_text(size = 14),
                          axis.title = element_text(size = 16),
                          axis.text = element_text(size = 14),
                          plot.title = element_text(hjust = .5,
                                                    vjust = 1.5, size = 16)
  ) + ylim(1.15, 2.7)


ggsave("Q1_sf_intvl_right_munbase_95CI.jpeg", plot2.m1.munbase.sf, width = 11, height = 7)

# 84% CIs

plot2.m1.munbase.84ci.sf <- plot_predictions(m1.sf.munbase,
                                             condition = list("sf_intvl", "tier_m"),
                                             conf_level = 0.84,
                                             gray = T)

plot2.m1.munbase.84ci.sf <- plot2.m1.munbase.84ci.sf +
  labs(x = "Preference for self-rule",
       y = "",
       linetype = "Decision taken by",
       title="b: Base category (line) is the decision taken by the municipality.")

plot2.m1.munbase.84ci.sf <- plot2.m1.munbase.84ci.sf +
  scale_linetype_manual(labels = c("Municipality",
                                   "County",
                                   "National government"),
                        values = c("solid", "dotted", "dashed")) +
  theme_classic() + theme(legend.position = c(0.8, 0.85),
                          legend.title = element_text(size = 14),
                          legend.text = element_text(size = 14),
                          axis.title = element_text(size = 16),
                          axis.text = element_text(size = 14),
                          plot.title = element_text(hjust = .5,
                                                    vjust = 1.5, size = 16)
  ) + ylim(1.25, 2.50)

#Save the plot:
ggsave("Q1_sf_intvl_right_munbase_84CI.jpeg", plot2.m1.munbase.84ci.sf, width = 11,
       height = 7)

combined.plots.munbase.sf <- ggarrange(plot1.m1.munbase, plot2.m1.munbase.84ci.sf,
                                 nrow = 1, ncol = 2)

ggsave(combined.plots.munbase.sf, filename = 'Figure_E1Aa_E1Ab.jpeg',
         width = 16, height = 7)

#### Figure E1Ac-E1Ad. Decision taken by the county to close an upper secondary school. ####

m1.sf_intvl.coubase <- feols(first_answer ~ sf_intvl * tier_c | responseid,
                       data = dataset)

modelsummary(m1.sf_intvl.coubase, stars = T, output = "Q1_cou_sf_int_robust.docx")

#Plot the results:

preds.sf_intvllow.coubase <- plot_predictions(m1.sf_intvl.coubase,
                                              condition = list(tier_c = list("M", "N"),
                                                               sf_intvl = 0),
                                              conf_level = 0.84,
                                              gray = T,
                                              draw = F) # Get the estimates & CIs when sf_intvl low

preds.sf_intvlhigh.coubase <- plot_predictions(m1.sf_intvl.coubase,
                                               condition = list(tier_c = list("M", "N"),
                                                                sf_intvl = 1),
                                               conf_level = 0.84,
                                               gray = T,
                                               draw = F) # Get the estimates & CIs when sf_intvl high

preds.sf_intvlhigh.coubase.check <- plot_predictions(m1.sf_intvl.coubase,
                                                     condition = list(tier_c = list("C"),
                                                                      sf_intvl = 1),
                                                     conf_level = 0.84,
                                                     gray = T,
                                                     draw = F)

plot1.m1.coubase.sf <- plot_predictions(m1.sf_intvl.coubase,
                                        condition = list(tier_c = list("M", "N"),
                                                         sf_intvl = "minmax"),
                                        conf_level = 0.95,
                                        gray = T)

plot1.m1.coubase.sf <-  plot1.m1.coubase.sf + geom_hline(yintercept = 1.829628, #c.bas
                                                         linetype = 1) +
  labs(x="Decision taken by",
       y="Willingness to accept decision (4-point merged scale)",
       shape="Preference for self-rule",
       title="c: Base category (line) is the decision taken by the county.") +
  scale_x_discrete(labels = c("Municipality",
                              "National government")) + ylim(1.25, 2.50) +
  scale_shape_discrete(labels = c("Min (= 0)", "Max (= 1)"))

plot1.m1.coubase.sf <- plot1.m1.coubase.sf + geom_linerange(data=preds.sf_intvllow.coubase,
                                                            aes(y = estimate,
                                                                x = tier_c,
                                                                ymin = conf.low,
                                                                ymax = conf.high),
                                                            position = position_nudge(x = -0.037),
                                                            linewidth = 1.2) +
  geom_linerange(data = preds.sf_intvlhigh.coubase,
                 aes(y = estimate,
                     x = tier_c,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.037),
                 linewidth = 1.2)

plot1.m1.coubase.sf <- plot1.m1.coubase.sf + theme_classic() +
  theme(legend.position = c(0.25, 0.85),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(hjust = 0.5, vjust = 1.5, size = 16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14)) + annotate('text', x=2, y=2.4,
                                                          label='*', size=8)

plot1.m1.coubase.sf$layers[[1]]$geom_params$fatten = 8

ggsave("Q1_sf_intvl_left_coubase.jpeg", plot1.m1.coubase.sf, width = 8, height = 6)


# Line graph with C as baseline #

# 95% CIs

plot2.m1.coubase.sf <- plot_predictions(m1.sf_intvl.coubase,
                                        condition = list("sf_intvl", "tier_c"),
                                        conf_level = 0.95,
                                        gray = T)

plot2.m1.coubase.sf <- plot2.m1.coubase.sf +
  labs(x = "Preference for self-rule",
       y = "",
       linetype = "Decision taken by",
       title="d: Base category (line) is the decision taken by the county.")

plot2.m1.coubase.sf <- plot2.m1.coubase.sf +
  scale_linetype_manual(labels = c("County",
                                   "Municipality",
                                   "National government"),
                        values = c("solid", "dotted", "dashed")) +
  theme_classic() + theme(legend.position = c(0.8, 0.3),
                          axis.title = element_text(size = 16),
                          axis.text = element_text(size = 14),
                          plot.title = element_text(hjust = 0.5, vjust = 1.5, size = 16),
                          legend.title = element_text(size = 14),
                          legend.text = element_text(size = 14)
  ) + ylim(1.15, 2.5)


ggsave("Q1_sf_intvl_right_coubase_95CI.jpeg", plot2.m1.coubase.sf, width = 11, height = 7)

# 84% CIs

plot2.m1.coubase.sf.84ci <- plot_predictions(m1.sf_intvl.coubase,
                                             condition = list("sf_intvl", "tier_c"),
                                             conf_level = 0.84,
                                             gray = T)

plot2.m1.coubase.sf.84ci <- plot2.m1.coubase.sf.84ci +
  labs(x = "Preference for self-rule",
       y = "",
       linetype = "Decision taken by",
       title="d: Base category (line) is the decision taken by the county.")

plot2.m1.coubase.sf.84ci <- plot2.m1.coubase.sf.84ci +
  scale_linetype_manual(labels = c("County",
                                   "Municipality",
                                   "National government"),
                        values = c("solid", "dotted", "dashed")) +
  theme_classic() + theme(legend.position = c(0.8, 0.85),
                          legend.title = element_text(size = 14),
                          legend.text = element_text(size = 14),
                          axis.title = element_text(size = 16),
                          axis.text = element_text(size = 14),
                          plot.title = element_text(hjust = .5,
                                                    vjust = 1.5, size = 16)
  ) + ylim(1.25, 2.5)


#Save the combined plot:
combined.plots.coubase.sf <- ggarrange(plot1.m1.coubase.sf, plot2.m1.coubase.sf.84ci,
                                 nrow = 1, ncol = 2)

ggsave(combined.plots.coubase.sf, filename = 'Figure_E1Ac_E1Ad.jpeg',
         width = 16, height = 8)

#### Figure E1Ae-E1Af. Decision taken by the national government to close a department of a university (college).####

preds.sf_intvllow.natbase <- plot_predictions(m1.sf_intvl.natbase,
                                              condition = list(tier_n = list("M", "C"),
                                                               sf_intvl = 0),
                                              conf_level = 0.84,
                                              gray = T,
                                              draw = F) # Get the estimates & CIs when sf_intvl low

preds.sf_intvlhigh.natbase <- plot_predictions(m1.sf_intvl.natbase,
                                               condition = list(tier_n = list("M", "C"),
                                                                sf_intvl = 1),
                                               conf_level = 0.84,
                                               gray = T,
                                               draw = F) # Get the estimates & CIs when sf_intvl high

plot1.m1.natbase.sf <- plot_predictions(m1.sf_intvl.natbase,
                                        condition = list(tier_n = list("M", "C"),
                                                         sf_intvl = "minmax"),
                                        conf_level = 0.95,
                                        gray = T)

plot1.m1.natbase.sf.check <- plot_predictions(m1.sf_intvl.natbase,
                                              condition = list(tier_n = list("N"),
                                                               sf_intvl = 1),
                                              conf_level = 0.84,
                                              gray = T,
                                              draw = F)

plot1.m1.natbase.sf <-  plot1.m1.natbase.sf + geom_hline(yintercept = 2.087714 , #n.bas
                                                         linetype = 1) +
  labs(x="Decision taken by",
       y="Willingness to accept decision (4-point merged scale)",
       shape="Preference for Self-Rule",
       title="e: Base category (line) is the decision taken by the national government.") +
  scale_x_discrete(labels = c("Municipality",
                              "County"))  +
  scale_shape_discrete(labels = c("Min (= 0)", "Max (= 1)"))

plot1.m1.natbase.sf <- plot1.m1.natbase.sf + geom_linerange(data=preds.sf_intvllow.natbase,
                                                            aes(y = estimate,
                                                                x = tier_n,
                                                                ymin = conf.low,
                                                                ymax = conf.high),
                                                            position = position_nudge(x = -0.037),
                                                            linewidth = 1.2) +
  geom_linerange(data = preds.sf_intvlhigh.natbase,
                 aes(y = estimate,
                     x = tier_n,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.037),
                 linewidth = 1.2) + ylim(1.25, 2.50)

plot1.m1.natbase.sf <- plot1.m1.natbase.sf + theme_classic() +
  theme(legend.position = c(0.8, 0.9),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(hjust = 0.5, vjust = 1.5, size = 16)) +
  annotate('text', x=c(1,2), y=c(2.3, 2), label=c("**", "*"), size=8)

plot1.m1.natbase.sf$layers[[1]]$geom_params$fatten = 8

ggsave("Q1_sf_intvl_left_natbase.jpeg", plot1.m1.natbase.sf, width = 8, height = 6)

# Line graph with N as baseline #

summary(m1.sf_intvl.natbase) 

# 95% CIs

plot2.m1.natbase.sf <- plot_predictions(m1.sf_intvl.natbase,
                                        condition = list("sf_intvl", "tier_n"),
                                        conf_level = 0.95,
                                        gray = T)

plot2.m1.natbase.sf <- plot2.m1.natbase.sf +
  labs(x = "Preference for self-rule",
       y = "Willingness to accept decision (4-point merged scale)",
       linetype = "Decision taken by",
       title="f: Base category (line) is the decision taken by the national government.")

plot2.m1.natbase.sf <- plot2.m1.natbase.sf +
  scale_linetype_manual(labels = c("National government",
                                   "Municipality",
                                   "County"),
                        values = c("solid", "dashed", "dotted")) +
  theme_classic() + theme(legend.position = c(0.8, 0.2),
                          legend.text = element_text(size=12),
                          legend.title = element_text(size=12),
                          axis.title = element_text(size = 16),
                          axis.text = element_text(size = 14),
                          plot.title = element_text(hjust = .5,
                                                    vjust = 1.5,
                                                    size = 16)
  ) + ylim(1.25, 2.50)


ggsave("Q1_sf_intvl_right_natbase_95CI.jpeg", plot2.m1.natbase.sf, width = 11, height = 7)

# 84% CIs

plot2.m1.natbase.84ci.sf <- plot_predictions(m1.sf_intvl.natbase,
                                             condition = list("sf_intvl", "tier_n"),
                                             conf_level = 0.84,
                                             gray = T)

plot2.m1.natbase.84ci.sf <- plot2.m1.natbase.84ci.sf +
  labs(x = "Preference for self-rule",
       y = "",
       linetype = "Decision taken by",
       title="f: Base category (line) is the decision taken by the national government.")

plot2.m1.natbase.84ci.sf <- plot2.m1.natbase.84ci.sf +
  scale_linetype_manual(labels = c("National government",
                                   "Municipality",
                                   "County"),
                        values = c("solid", "dashed", "dotted")) +
  theme_classic() + theme(legend.position = c(0.8, 0.2),
                          legend.title = element_text(size = 14),
                          legend.text = element_text(size = 14),
                          axis.title = element_text(size = 16),
                          axis.text = element_text(size = 14),
                          plot.title = element_text(hjust = .5,
                                                    vjust = 1.5,
                                                    size = 16)
  ) + ylim(1.25, 2.50)


ggsave("Q1_sf_intvl_right_natbase_84CI.jpeg", plot2.m1.natbase.84ci.sf, width = 11,
       height = 7)

#Save the combined plot:
combined.plots.natbase.sf <- ggarrange(plot1.m1.natbase.sf, plot2.m1.natbase.84ci.sf,
                                 nrow = 1, ncol = 2)

ggsave(combined.plots.natbase.sf, filename = 'Figure_E1Ae_E1Af.jpg',
         width = 16, height = 8)

### Table E1B. Model 1-Q1-Q3-Q5: Preference for shared rule. ####

m1.sh.munbase <- feols(first_answer ~ sh_intvl * tier_m | responseid,
                       data = dataset) # Base category = municipality

summary(m1.sh.munbase)

m1.sh.coubase <- feols(first_answer ~ sh_intvl * tier_c | responseid,
                       data = dataset) # Base category = county

summary(m1.sh.coubase)

m1.sh.natbase <- feols(first_answer ~ sh_intvl * tier_n | responseid,
                       data = dataset) # Base category = national government

summary(m1.sh.natbase)

#### Figure E1Ba-E1Bb. Decision taken by the municipality. ####

m1.sh.munbase <- feols(first_answer ~ sh_intvl * tier_m | responseid,
                       data = dataset)

preds.shlow.munbase <- plot_predictions(m1.sh.munbase,
                                        condition = list(tier_m = list("C", "N"),
                                                         sh_intvl= 0),
                                        conf_level = 0.84,
                                        gray = T,
                                        draw = F) # Get the estimates & CIs when sh_intvllow

preds.shhigh.munbase <- plot_predictions(m1.sh.munbase,
                                         condition = list(tier_m = list("C", "N"),
                                                          sh_intvl= 1),
                                         conf_level = 0.84,
                                         gray = T,
                                         draw = F) # Get the estimates & CIs when sh_intvlhigh

preds.base.munbase.sh.check <- plot_predictions(m1.sh.munbase,
                                                condition = list(tier_m = list("M"),
                                                                 sh_intvl= 1),
                                                conf_level = 0.84,
                                                gray = T,
                                                draw = F) # check M estimate (baseline)

plot1.m1.munbase.sh <- plot_predictions(m1.sh.munbase,
                                        condition = list(tier_m = list("C", "N"),
                                                         sh_intvl= "minmax"),
                                        conf_level = 0.95,
                                        gray = T)

plot1.m1.munbase.sh <-  plot1.m1.munbase.sh + geom_hline(yintercept = 2.066895, #m.bas
                                                         linetype = 1) +
  labs(x="Decision taken by",
       y="Willingness to accept decision (4-point merged scale)",
       shape="Preference for Shared Rule",
       title="a: Base category (line) is the decision taken by the municipality.") +
  scale_x_discrete(labels = c("County",
                              "National government")) + ylim(1.25, 2.50) +
  scale_shape_discrete(labels = c("Min (= 0)", "Max (= 1)"))

plot1.m1.munbase.sh <- plot1.m1.munbase.sh + geom_linerange(data=preds.shlow.munbase,
                                                            aes(y = estimate,
                                                                x = tier_m,
                                                                ymin = conf.low,
                                                                ymax = conf.high),
                                                            position = position_nudge(x = -0.0363),
                                                            linewidth = 1.2) +
  geom_linerange(data = preds.shhigh.munbase,
                 aes(y = estimate,
                     x = tier_m,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.037),
                 linewidth = 1.2)

plot1.m1.munbase.sh <- plot1.m1.munbase.sh + theme_classic() +
  theme(legend.position = c(0.25, 0.85),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(hjust = 0.5, vjust = 1.5, size = 16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14))

plot1.m1.munbase.sh$layers[[1]]$geom_params$fatten = 8

ggsave("Q1_sh_intvl_left_munbase.jpeg", plot1.m1.munbase.sh, width = 8, height = 6)

# Line graph with M as baseline #

# 95% CIs

plot2.m1.munbase.sh <- plot_predictions(m1.sh.munbase,
                                        condition = list("sh_intvl", "tier_m"),
                                        conf_level = 0.95,
                                        gray = T)

plot2.m1.munbase.sh <- plot2.m1.munbase.sh +
  labs(x = "Preference for Shared Rule",
       y = "",
       linetype = "Decision taken by",
       title="b: Base category (line) is the decision taken by the municipality.")

plot2.m1.munbase.sh <- plot2.m1.munbase.sh +
  scale_linetype_manual(labels = c("Municipality",
                                   "County",
                                   "National government"),
                        values = c("solid", "dotted", "dashed")) +
  theme_classic() + theme(legend.position = c(0.8, 0.85),
                          legend.title = element_text(size = 14),
                          legend.text = element_text(size = 14),
                          axis.title = element_text(size = 16),
                          axis.text = element_text(size = 14),
                          plot.title = element_text(hjust = .5,
                                                    vjust = 1.5, size = 16)
  ) + ylim(1.15, 2.5)


ggsave("Q1_sh_intvl_right_munbase_95CI.jpeg", plot2.m1.munbase.sh, width = 11, height = 7)

# 84% CIs

plot2.m1.munbase.84ci.sh <- plot_predictions(m1.sh.munbase,
                                             condition = list("sh_intvl", "tier_m"),
                                             conf_level = 0.84,
                                             gray = T)

plot2.m1.munbase.84ci.sh <- plot2.m1.munbase.84ci.sh +
  labs(x = "Preference for Shared Rule",
       y = "",
       linetype = "Decision taken by",
       title="b: Base category (line) is the decision taken by the municipality.")

plot2.m1.munbase.84ci.sh <- plot2.m1.munbase.84ci.sh +
  scale_linetype_manual(labels = c("Municipality",
                                   "County",
                                   "National government"),
                        values = c("solid", "dotted", "dashed")) +
  theme_classic() + theme(legend.position = c(0.8, 0.85),
                          legend.title = element_text(size = 14),
                          legend.text = element_text(size = 14),
                          axis.title = element_text(size = 16),
                          axis.text = element_text(size = 14),
                          plot.title = element_text(hjust = .5,
                                                    vjust = 1.5, size = 16)
  ) + ylim(1.25, 2.50)


ggsave("Q1_sh_intvl_right_munbase_84CI.jpeg", plot2.m1.munbase.84ci.sh, width = 11,
       height = 7)

combined.plots.munbase.sh <- ggarrange(plot1.m1.munbase.sh, plot2.m1.munbase.84ci.sh,
                                       nrow = 1, ncol = 2)

ggsave(combined.plots.munbase.sh, filename = 'Figure_E1Ba_E1Bb.jpeg',
         width = 16, height = 8)

#### Figure E1Bc-E1Bd. Decision taken by the county. ####
m1.sh.coubase <- feols(first_answer ~ sh_intvl * tier_c | responseid,
                       data = dataset)

preds.sh_intvllow.coubase <- plot_predictions(m1.sh.coubase,
                                              condition = list(tier_c = list("M", "N"),
                                                               sh_intvl = 0),
                                              conf_level = 0.84,
                                              gray = T,
                                              draw = F) # Get the estimates & CIs when sh_intvl low

preds.sh_intvlhigh.coubase <- plot_predictions(m1.sh.coubase,
                                               condition = list(tier_c = list("M", "N"),
                                                                sh_intvl = 1),
                                               conf_level = 0.84,
                                               gray = T,
                                               draw = F) # Get the estimates & CIs when sh_intvl high

preds.sh_intvlhigh.coubase.check <- plot_predictions(m1.sh.coubase,
                                                     condition = list(tier_c = list("C"),
                                                                      sh_intvl = 1),
                                                     conf_level = 0.84,
                                                     gray = T,
                                                     draw = F)

plot1.m1.coubase.sh <- plot_predictions(m1.sh.coubase,
                                        condition = list(tier_c = list("M", "N"),
                                                         sh_intvl = "minmax"),
                                        conf_level = 0.95,
                                        gray = T)

plot1.m1.coubase.sh <-  plot1.m1.coubase.sh + geom_hline(yintercept = 1.827698, #c.bas
                                                         linetype = 1) +
  labs(x="Decision taken by",
       y="Willingness to accept decision (4-point merged scale)",
       shape="Preference for Shared Rule",
       title="c: Base category (line) is the decision taken by the county.") +
  scale_x_discrete(labels = c("Municipality",
                              "National government")) + ylim(1.25, 2.50) +
  scale_shape_discrete(labels = c("Min (= 0)", "Max (= 1)"))

plot1.m1.coubase.sh <- plot1.m1.coubase.sh + geom_linerange(data=preds.sh_intvllow.coubase,
                                                            aes(y = estimate,
                                                                x = tier_c,
                                                                ymin = conf.low,
                                                                ymax = conf.high),
                                                            position = position_nudge(x = -0.037),
                                                            linewidth = 1.2) +
  geom_linerange(data = preds.sh_intvlhigh.coubase,
                 aes(y = estimate,
                     x = tier_c,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.037),
                 linewidth = 1.2)

plot1.m1.coubase.sh <- plot1.m1.coubase.sh + theme_classic() +
  theme(legend.position = c(0.25, 0.3),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, vjust = 1.5, size = 16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14))

plot1.m1.coubase.sh$layers[[1]]$geom_params$fatten = 8

ggsave("Q1_sh_intvl_left_coubase.jpeg", plot1.m1.coubase.sh, width = 8, height = 6)


# Line graph with C as baseline #

# 95% CIs

plot2.m1.coubase.sh <- plot_predictions(m1.sh.coubase,
                                        condition = list("sh_intvl", "tier_c"),
                                        conf_level = 0.95,
                                        gray = T)

plot2.m1.coubase.sh <- plot2.m1.coubase.sh +
  labs(x = "Preference for Shared Rule",
       y = "",
       linetype = "Decision taken by",
       title="d: Base category (line) is the decision taken by the county.")

plot2.m1.coubase.sh <- plot2.m1.coubase.sh +
  scale_linetype_manual(labels = c("County",
                                   "Municipality",
                                   "National government"),
                        values = c("solid", "dotted", "dashed")) +
  theme_classic() + theme(legend.position = c(0.8, 0.3),
                          axis.title = element_text(size = 16),
                          axis.text = element_text(size = 14),
                          plot.title = element_text(hjust = 0.5, vjust = 1.5, size = 16),
                          legend.title = element_text(size = 14),
                          legend.text = element_text(size = 14)
  ) + ylim(1.15, 2.5)


ggsave("Q1_sh_intvl_right_coubase_95CI.jpeg", plot2.m1.coubase.sh, width = 11, height = 7)

# 84% CIs

plot2.m1.coubase.sh.84ci <- plot_predictions(m1.sh.coubase,
                                             condition = list("sh_intvl", "tier_c"),
                                             conf_level = 0.84,
                                             gray = T)

plot2.m1.coubase.sh.84ci <- plot2.m1.coubase.sh.84ci +
  labs(x = "Preference for Shared Rule",
       y = "",
       linetype = "Decision taken by",
       title="d: Base category (line) is the decision taken by the county.")

plot2.m1.coubase.sh.84ci <- plot2.m1.coubase.sh.84ci +
  scale_linetype_manual(labels = c("County",
                                   "Municipality",
                                   "National government"),
                        values = c("solid", "dotted", "dashed")) +
  theme_classic() + theme(legend.position = c(0.8, 0.3),
                          axis.title = element_text(size = 16),
                          axis.text = element_text(size = 14),
                          plot.title = element_text(hjust = 0.5, vjust = 1.5, size = 16),
                          legend.title = element_text(size = 14),
                          legend.text = element_text(size = 14)
  ) + ylim(1.25, 2.5)


ggsave("Q1_sh_intvl_right_coubase_84CI.jpeg", plot2.m1.coubase.sh.84ci, width = 11,
       height = 7)

combined.plots.coubase.sh <- ggarrange(plot1.m1.coubase.sh, plot2.m1.coubase.sh.84ci,
                                 nrow = 1, ncol = 2)

ggsave(combined.plots.coubase.sh, filename = 'Figure_E1Bc_E1Bd.jpeg',
         width = 16, height = 8)


#### Figure E1Be-E1Bf. Decision taken by the national government. ####

m1.sh.natbase <- feols(first_answer ~ sh_intvl * tier_n | responseid,
                       data = dataset)

preds.sh_intvllow.natbase <- plot_predictions(m1.sh.natbase,
                                              condition = list(tier_n = list("M", "C"),
                                                               sh_intvl = 0),
                                              conf_level = 0.84,
                                              gray = T,
                                              draw = F) # Get the estimates & CIs when sh_intvl low

preds.sh_intvlhigh.natbase <- plot_predictions(m1.sh.natbase,
                                               condition = list(tier_n = list("M", "C"),
                                                                sh_intvl = 1),
                                               conf_level = 0.84,
                                               gray = T,
                                               draw = F) # Get the estimates & CIs when sh_intvl high

plot1.m1.natbase.sh <- plot_predictions(m1.sh.natbase,
                                        condition = list(tier_n = list("M", "C"),
                                                         sh_intvl = "minmax"),
                                        conf_level = 0.95,
                                        gray = T)

plot1.m1.natbase.sh.check <- plot_predictions(m1.sh.natbase,
                                              condition = list(tier_n = list("N"),
                                                               sh_intvl = 1),
                                              conf_level = 0.84,
                                              gray = T,
                                              draw = F)

plot1.m1.natbase.sh <-  plot1.m1.natbase.sh + geom_hline(yintercept = 2.105407 , #n.bas
                                                         linetype = 1) +
  labs(x="Decision taken by",
       y="Willingness to accept decision (4-point merged scale)",
       shape="Preference for Shared Rule",
       title="e: Base category (line) is the decision taken by the national government.") +
  scale_x_discrete(labels = c("Municipality",
                              "County"))  +
  scale_shape_discrete(labels = c("Min (= 0)", "Max (= 1)"))

plot1.m1.natbase.sh <- plot1.m1.natbase.sh + geom_linerange(data=preds.sh_intvllow.natbase,
                                                            aes(y = estimate,
                                                                x = tier_n,
                                                                ymin = conf.low,
                                                                ymax = conf.high),
                                                            position = position_nudge(x = -0.037),
                                                            linewidth = 1.2) +
  geom_linerange(data = preds.sh_intvlhigh.natbase,
                 aes(y = estimate,
                     x = tier_n,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.037),
                 linewidth = 1.2) + ylim(1.25, 2.5)

plot1.m1.natbase.sh <- plot1.m1.natbase.sh + theme_classic() +
  theme(legend.position = c(0.25, 0.9),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(hjust = 0.5, vjust = 1.5, size = 16))

plot1.m1.natbase.sh$layers[[1]]$geom_params$fatten = 8

ggsave("Q1_sh_intvl_left_natbase.jpeg", plot1.m1.natbase.sh, width = 8, height = 6)

# Line graph with N as baseline #


# 95% CIs

plot2.m1.natbase.sh <- plot_predictions(m1.sh.natbase,
                                        condition = list("sh_intvl", "tier_n"),
                                        conf_level = 0.95,
                                        gray = T)

plot2.m1.natbase.sh <- plot2.m1.natbase.sh +
  labs(x = "Preference for Shared Rule",
       y = "Willingness to accept decision (4-point merged scale)",
       linetype = "Decision taken by",
       title="f: Base category (line) is the decision taken by the national government.")

plot2.m1.natbase.sh <- plot2.m1.natbase.sh +
  scale_linetype_manual(labels = c("National government",
                                   "Municipality",
                                   "County"),
                        values = c("solid", "dashed", "dotted")) +
  theme_classic() + theme(legend.position = c(0.8, 0.2),
                          legend.text = element_text(size=14),
                          legend.title = element_text(size=14),
                          axis.title = element_text(size = 16),
                          axis.text = element_text(size = 14),
                          plot.title = element_text(hjust = .5,
                                                    vjust = 1.5,
                                                    size = 16)
  ) + ylim(1.25, 2.5)


ggsave("Q1_sh_intvl_right_natbase_95CI.jpeg", plot2.m1.natbase.sh, width = 11, height = 7)

# 84% CIs

plot2.m1.natbase.84ci.sh <- plot_predictions(m1.sh.natbase,
                                             condition = list("sh_intvl", "tier_n"),
                                             conf_level = 0.84,
                                             gray = T)

plot2.m1.natbase.84ci.sh <- plot2.m1.natbase.84ci.sh +
  labs(x = "Preference for Shared Rule",
       y = "",
       linetype = "Decision taken by",
       title="f: Base category (line) is the decision taken by the national government.")

plot2.m1.natbase.84ci.sh <- plot2.m1.natbase.84ci.sh +
  scale_linetype_manual(labels = c("National government",
                                   "Municipality",
                                   "County"),
                        values = c("solid", "dashed", "dotted")) +
  theme_classic() + theme(legend.position = c(0.8, 0.2),
                          legend.title = element_text(size = 14),
                          legend.text = element_text(size = 14),
                          axis.title = element_text(size = 16),
                          axis.text = element_text(size = 14),
                          plot.title = element_text(hjust = .5,
                                                    vjust = 1.5,
                                                    size = 16)
  ) + ylim(1.25, 2.50)


ggsave("Q1_sh_intvl_right_natbase_84CI.jpeg", plot2.m1.natbase.84ci.sh, width = 11,
       height = 7)

combined.plots.natbase.sh <- ggarrange(plot1.m1.natbase.sh, plot2.m1.natbase.84ci.sh,
                                 nrow = 1, ncol = 2)

ggsave(combined.plots.natbase.sh, filename = 'Figure_E1Be_E1Bf.jpeg',
         width = 16, height = 8)

### Table E1C. Model 1-Q1-Q3-Q5: Preference for each item. ####
# Self-Rule Item 1:
m1.sf1.munbase <- feols(first_answer ~ sf1 * tier_m | responseid,
                        data = dataset)

summary(m1.sf1.munbase)

# Self-Rule Item 2:
m1.sf2.munbase <- feols(first_answer ~ sf2 * tier_m | responseid,
                        data = dataset)
summary(m1.sf2.munbase)

# Self-Rule Item 3:
m1.sf3.munbase <- feols(first_answer ~ sf3 * tier_m | responseid,
                        data = dataset)
summary(m1.sf3.munbase)

# Shared Rule Item 1:
m1.sh1.munbase <- feols(first_answer ~ sh1 * tier_m | responseid,
                        data = dataset)
summary(m1.sh1.munbase)

# Shared Rule Item 2:
m1.sh2.munbase <- feols(first_answer ~ sh2 * tier_m | responseid,
                        data = dataset)
summary(m1.sh2.munbase)

# Shared Rule Item 3:
m1.sh3.munbase <- feols(first_answer ~ sh3 * tier_m | responseid,
                        data = dataset)
summary(m1.sh3.munbase)


### Figure E1C. Model 1-Q1-Q3-Q5-base-cat. = M: Preference for self-rule items 1-3. ####
#### Figure E1CAa-E1CAb. Decision by the municipality to close kindergarten: self-rule item 1.####
preds.sf1low.munbase <- plot_predictions(m1.sf1.munbase,
                                         condition = list(tier_m = list("C", "N"),
                                                          sf1= 1),
                                         conf_level = 0.84,
                                         gray = T,
                                         draw = F) # Get the estimates & CIs when sf1low

preds.sf1high.munbase <- plot_predictions(m1.sf1.munbase,
                                          condition = list(tier_m = list("C", "N"),
                                                           sf1= 4),
                                          conf_level = 0.84,
                                          gray = T,
                                          draw = F) # Get the estimates & CIs when sf1high

preds.base.munbase.sf1.check <- plot_predictions(m1.sf1.munbase,
                                                 condition = list(tier_m = list("M"),
                                                                  sf1= 1),
                                                 conf_level = 0.84,
                                                 gray = T,
                                                 draw = F) # check M estimate (baseline)

plot1.m1.munbase.sf1 <- plot_predictions(m1.sf1.munbase,
                                         condition = list(tier_m = list("C", "N"),
                                                          sf1= "minmax"),
                                         conf_level = 0.95,
                                         gray = T)

plot1.m1.munbase.sf1 <-  plot1.m1.munbase.sf1 + geom_hline(yintercept = 2.104943, #m.bas
                                                           linetype = 1) +
  labs(x="Decision taken by",
       y="Willingness to accept decision (4-point merged scale)",
       shape="Preference for Self-Rule Item 1",
       title="a: Base category (line) is the decision taken by the municipality.") +
  scale_x_discrete(labels = c("County",
                              "National government")) + ylim(1.25, 2.50) +
  scale_shape_discrete(labels = c("Min (= 1)", "Max (= 4)"))

plot1.m1.munbase.sf1 <- plot1.m1.munbase.sf1 + geom_linerange(data=preds.sf1low.munbase,
                                                              aes(y = estimate,
                                                                  x = tier_m,
                                                                  ymin = conf.low,
                                                                  ymax = conf.high),
                                                              position = position_nudge(x = -0.0363),
                                                              linewidth = 1.2) +
  geom_linerange(data = preds.sf1high.munbase,
                 aes(y = estimate,
                     x = tier_m,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.037),
                 linewidth = 1.2)

plot1.m1.munbase.sf1 <- plot1.m1.munbase.sf1 + theme_classic() +
  theme(legend.position = c(0.75, 0.25),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        plot.title = element_text(hjust = 0.5, vjust = 1.5, size = 16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14)) + annotate('text', x=c(1,2),
                                                          y=c(2.2,2.5),
                                                          label=c('**', '**'),
                                                          size=8)

plot1.m1.munbase.sf1$layers[[1]]$geom_params$fatten = 8

ggsave("Q1_sf1_left_munbase.jpeg", plot1.m1.munbase.sf1, width = 8, height = 6)

##### Line graph with M as baseline #####

# 95% CIs

plot2.m1.munbase.sf1 <- plot_predictions(m1.sf1.munbase,
                                         condition = list("sf1", "tier_m"),
                                         conf_level = 0.95,
                                         gray = T)

plot2.m1.munbase.sf1 <- plot2.m1.munbase.sf1 +
  labs(x = "Preference for Self-Rule Item 1",
       y = "",
       linetype = "Decision taken by",
       title="b: Base category (line) is the decision taken by the municipality.")

plot2.m1.munbase.sf1 <- plot2.m1.munbase.sf1 +
  scale_linetype_manual(labels = c("Municipality",
                                   "County",
                                   "National government"),
                        values = c("solid", "dotted", "dashed")) +
  theme_classic() + theme(legend.position = c(0.8, 0.3),
                          legend.title = element_text(size = 14),
                          legend.text = element_text(size = 14),
                          axis.title = element_text(size = 16),
                          axis.text = element_text(size = 12),
                          plot.title = element_text(hjust = .5,
                                                    vjust = 1.5, size = 16)
  ) + ylim(1.15, 2.5)


ggsave("Q1_sf1_right_munbase_95CI.jpeg", plot2.m1.munbase.sf1, width = 11, height = 7)

# 84% CIs

plot2.m1.munbase.84ci.sf1 <- plot_predictions(m1.sf1.munbase,
                                              condition = list("sf1", "tier_m"),
                                              conf_level = 0.84,
                                              gray = T)

plot2.m1.munbase.84ci.sf1 <- plot2.m1.munbase.84ci.sf1 +
  labs(x = "Preference for Self-Rule Item 1",
       y = "",
       linetype = "Decision taken by",
       title="b: Base category (line) is the decision taken by the municipality.")

plot2.m1.munbase.84ci.sf1 <- plot2.m1.munbase.84ci.sf1 +
  scale_linetype_manual(labels = c("Municipality",
                                   "County",
                                   "National government"),
                        values = c("solid", "dotted", "dashed")) +
  theme_classic() + theme(legend.position = c(0.8, 0.3),
                          legend.title = element_text(size = 14),
                          legend.text = element_text(size = 14),
                          axis.title = element_text(size = 16),
                          axis.text = element_text(size = 12),
                          plot.title = element_text(hjust = .5,
                                                    vjust = 1.5, size = 16)
  ) + ylim(1.25, 2.50)


ggsave("Q1_sf1_right_munbase_84CI.jpeg", plot2.m1.munbase.84ci.sf1, width = 11,
       height = 7)

combined.plots.munbase.sf1 <- ggarrange(plot1.m1.munbase.sf1, plot2.m1.munbase.84ci.sf1,
                                  nrow = 1, ncol = 2)

ggsave(combined.plots.munbase.sf1, filename = 'Figure_E1CAa_E1CAb.jpeg',
         width = 16, height = 8)



#### Figure E1CAc-E1CAd. Decision by the municipality to close kindergarten: self-rule item 2.####

preds.sf2low.munbase <- plot_predictions(m1.sf2.munbase,
                                         condition = list(tier_m = list("C", "N"),
                                                          sf2= 1),
                                         conf_level = 0.84,
                                         gray = T,
                                         draw = F) # Get the estimates & CIs when sf2low

preds.sf2high.munbase <- plot_predictions(m1.sf2.munbase,
                                          condition = list(tier_m = list("C", "N"),
                                                           sf2= 4),
                                          conf_level = 0.84,
                                          gray = T,
                                          draw = F) # Get the estimates & CIs when sf2high

preds.base.munbase.sf2.check <- plot_predictions(m1.sf2.munbase,
                                                 condition = list(tier_m = list("M"),
                                                                  sf2= 1),
                                                 conf_level = 0.84,
                                                 gray = T,
                                                 draw = F) # check M estimate (baseline)

plot1.m1.munbase.sf2 <- plot_predictions(m1.sf2.munbase,
                                         condition = list(tier_m = list("C", "N"),
                                                          sf2= "minmax"),
                                         conf_level = 0.95,
                                         gray = T)

plot1.m1.munbase.sf2 <-  plot1.m1.munbase.sf2 + geom_hline(yintercept = 2.070865, #m.bas
                                                           linetype = 1) +
  labs(x="Decision taken by",
       y="Willingness to accept decision (4-point merged scale)",
       shape="Preference for Self-Rule Item 2",
       title="c:Base category (line) is the decision taken by the municipality.") +
  scale_x_discrete(labels = c("County",
                              "National government")) + ylim(1.25, 2.50) +
  scale_shape_discrete(labels = c("Min (= 1)", "Max (= 4)"))

plot1.m1.munbase.sf2 <- plot1.m1.munbase.sf2 + geom_linerange(data=preds.sf2low.munbase,
                                                              aes(y = estimate,
                                                                  x = tier_m,
                                                                  ymin = conf.low,
                                                                  ymax = conf.high),
                                                              position = position_nudge(x = -0.0363),
                                                              linewidth = 1.2) +
  geom_linerange(data = preds.sf2high.munbase,
                 aes(y = estimate,
                     x = tier_m,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.037),
                 linewidth = 1.2)

plot1.m1.munbase.sf2 <- plot1.m1.munbase.sf2 + theme_classic() +
  theme(legend.position = c(0.25, 0.85),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        plot.title = element_text(hjust = 0.5, vjust = 1.5, size = 16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14))

plot1.m1.munbase.sf2$layers[[1]]$geom_params$fatten = 8

ggsave("Q1_sf2_left_munbase.jpeg", plot1.m1.munbase.sf2, width = 8, height = 6)

##### Line graph with M as baseline #####

# 95% CIs

plot2.m1.munbase.sf2 <- plot_predictions(m1.sf2.munbase,
                                         condition = list("sf2", "tier_m"),
                                         conf_level = 0.95,
                                         gray = T)

plot2.m1.munbase.sf2 <- plot2.m1.munbase.sf2 +
  labs(x = "Preference for Self-Rule Item 2",
       y = "",
       linetype = "Decision taken by",
       title="d: Base category (line) is the decision taken by the municipality.")

plot2.m1.munbase.sf2 <- plot2.m1.munbase.sf2 +
  scale_linetype_manual(labels = c("Municipality",
                                   "County",
                                   "National government"),
                        values = c("solid", "dotted", "dashed")) +
  theme_classic() + theme(legend.position = c(0.8, 0.3),
                          legend.title = element_text(size = 14),
                          legend.text = element_text(size = 14),
                          axis.title = element_text(size = 16),
                          axis.text = element_text(size = 12),
                          plot.title = element_text(hjust = .5,
                                                    vjust = 1.5, size = 16)
  ) + ylim(1.15, 2.5)


ggsave("Q1_sf2_right_munbase_95CI.jpeg", plot2.m1.munbase.sf2, width = 11, height = 7)

# 84% CIs

plot2.m1.munbase.84ci.sf2 <- plot_predictions(m1.sf2.munbase,
                                              condition = list("sf2", "tier_m"),
                                              conf_level = 0.84,
                                              gray = T)

plot2.m1.munbase.84ci.sf2 <- plot2.m1.munbase.84ci.sf2 +
  labs(x = "Preference for Self-Rule Item 2",
       y = "",
       linetype = "Decision taken by",
       title="d: Base category (line) is the decision taken by the municipality.")

plot2.m1.munbase.84ci.sf2 <- plot2.m1.munbase.84ci.sf2 +
  scale_linetype_manual(labels = c("Municipality",
                                   "County",
                                   "National government"),
                        values = c("solid", "dotted", "dashed")) +
  theme_classic() + theme(legend.position = c(0.8, 0.3),
                          legend.title = element_text(size = 14),
                          legend.text = element_text(size = 14),
                          axis.title = element_text(size = 16),
                          axis.text = element_text(size = 12),
                          plot.title = element_text(hjust = .5,
                                                    vjust = 1.5, size = 16)
  ) + ylim(1.25, 2.50)


ggsave("Q1_sf2_right_munbase_84CI.jpeg", plot2.m1.munbase.84ci.sf2, width = 11,
       height = 7)

combined.plots.munbase.sf2 <- ggarrange(plot1.m1.munbase.sf2, plot2.m1.munbase.84ci.sf2,
                                  nrow = 1, ncol = 2)

ggsave(combined.plots.munbase.sf2, filename = 'Figure_E1CAc_E1CAd.jpeg',
         width = 16, height = 8)

#### Figure E1CAe-E1CAf. Decision by the municipality to close kindergarten: self-rule item 3.####
m1.sf3.munbase <- feols(first_answer ~ sf3 * tier_m | responseid,
                        data = dataset)
summary(m1.sf3.munbase)

preds.sf3low.munbase <- plot_predictions(m1.sf3.munbase,
                                         condition = list(tier_m = list("C", "N"),
                                                          sf3= 1),
                                         conf_level = 0.84,
                                         gray = T,
                                         draw = F) # Get the estimates & CIs when sf3low

preds.sf3high.munbase <- plot_predictions(m1.sf3.munbase,
                                          condition = list(tier_m = list("C", "N"),
                                                           sf3= 4),
                                          conf_level = 0.84,
                                          gray = T,
                                          draw = F) # Get the estimates & CIs when sf3high

preds.base.munbase.sf3.check <- plot_predictions(m1.sf3.munbase,
                                                 condition = list(tier_m = list("M"),
                                                                  sf3= 1),
                                                 conf_level = 0.84,
                                                 gray = T,
                                                 draw = F) # check M estimate (baseline)

preds.base.munbase.sf3.check

plot1.m1.munbase.sf3 <- plot_predictions(m1.sf3.munbase,
                                         condition = list(tier_m = list("C", "N"),
                                                          sf3= "minmax"),
                                         conf_level = 0.95,
                                         gray = T)

plot1.m1.munbase.sf3 <-  plot1.m1.munbase.sf3 + geom_hline(yintercept = 2.068202, #m.bas
                                                           linetype = 1) +
  labs(x="Decision taken by",
       y="Willingness to accept decision (4-point merged scale)",
       shape="Preference for Self-Rule Item 3",
       title="e: Base category (line) is the decision taken by the municipality.") +
  scale_x_discrete(labels = c("County",
                              "National government")) + ylim(1.25, 2.50) +
  scale_shape_discrete(labels = c("Min (= 1)", "Max (= 4)"))

plot1.m1.munbase.sf3 <- plot1.m1.munbase.sf3 + geom_linerange(data=preds.sf3low.munbase,
                                                              aes(y = estimate,
                                                                  x = tier_m,
                                                                  ymin = conf.low,
                                                                  ymax = conf.high),
                                                              position = position_nudge(x = -0.0363),
                                                              linewidth = 1.2) +
  geom_linerange(data = preds.sf3high.munbase,
                 aes(y = estimate,
                     x = tier_m,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.037),
                 linewidth = 1.2)

plot1.m1.munbase.sf3 <- plot1.m1.munbase.sf3 + theme_classic() +
  theme(legend.position = c(0.25, 0.85),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        plot.title = element_text(hjust = 0.5, vjust = 1.5, size = 16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14))

plot1.m1.munbase.sf3$layers[[1]]$geom_params$fatten = 8

ggsave("Q1_sf3_left_munbase.jpeg", plot1.m1.munbase.sf3, width = 8, height = 6)

##### Line graph with M as baseline #####

# 95% CIs

plot2.m1.munbase.sf3 <- plot_predictions(m1.sf3.munbase,
                                         condition = list("sf3", "tier_m"),
                                         conf_level = 0.95,
                                         gray = T)

plot2.m1.munbase.sf3 <- plot2.m1.munbase.sf3 +
  labs(x = "Preference for Self-Rule Item 3",
       y = "",
       linetype = "Decision taken by",
       title="f: Base category (line) is the decision taken by the municipality.")

plot2.m1.munbase.sf3 <- plot2.m1.munbase.sf3 +
  scale_linetype_manual(labels = c("Municipality",
                                   "County",
                                   "National government"),
                        values = c("solid", "dotted", "dashed")) +
  theme_classic() + theme(legend.position = c(0.8, 0.25),
                          legend.title = element_text(size = 14),
                          legend.text = element_text(size = 14),
                          axis.title = element_text(size = 16),
                          axis.text = element_text(size = 12),
                          plot.title = element_text(hjust = .5,
                                                    vjust = 1.5, size = 16)
  ) + ylim(1.15, 2.5)


ggsave("Q1_sf3_right_munbase_95CI.jpeg", plot2.m1.munbase.sf3, width = 11, height = 7)

# 84% CIs

plot2.m1.munbase.84ci.sf3 <- plot_predictions(m1.sf3.munbase,
                                              condition = list("sf3", "tier_m"),
                                              conf_level = 0.84,
                                              gray = T)

plot2.m1.munbase.84ci.sf3 <- plot2.m1.munbase.84ci.sf3 +
  labs(x = "Preference for Self-Rule Item 3",
       y = "",
       linetype = "Decision taken by",
       title="f: Base category (line) is the decision taken by the municipality.")

plot2.m1.munbase.84ci.sf3 <- plot2.m1.munbase.84ci.sf3 +
  scale_linetype_manual(labels = c("Municipality",
                                   "County",
                                   "National government"),
                        values = c("solid", "dotted", "dashed")) +
  theme_classic() + theme(legend.position = c(0.8, 0.25),
                          legend.title = element_text(size = 14),
                          legend.text = element_text(size = 14),
                          axis.title = element_text(size = 16),
                          axis.text = element_text(size = 12),
                          plot.title = element_text(hjust = .5,
                                                    vjust = 1.5, size = 16)
  ) + ylim(1.25, 2.50)


ggsave("Q1_sf3_right_munbase_84CI.jpeg", plot2.m1.munbase.84ci.sf3, width = 11,
       height = 7)

combined.plots.munbase.sf3 <- ggarrange(plot1.m1.munbase.sf3, plot2.m1.munbase.84ci.sf3,
                                  nrow = 1, ncol = 2)

ggsave(combined.plots.munbase.sf3, filename = 'Figure_E1CAe_E1CAf.jpeg',
         width = 16, height = 8)

#### Figure E1CB. Model 1-Q1-Q3-Q5-base-cat. = C: Preference for self-rule items 1-3. ####
#####Figure E1CBa-E1CBb. Decision taken by the county to close upper secondary school: self-rule item 1.####

m1.sf1.coubase <- feols(first_answer ~ sf1 * tier_c | responseid,
                        data = dataset)

summary(m1.sf1.coubase) # See coefficients of the model

preds.sf1low.coubase <- plot_predictions(m1.sf1.coubase,
                                         condition = list(tier_c = list("M", "N"),
                                                          sf1 = 1),
                                         conf_level = 0.84,
                                         gray = T,
                                         draw = F) # Get the estimates & CIs when sf1 low

preds.sf1high.coubase <- plot_predictions(m1.sf1.coubase,
                                          condition = list(tier_c = list("M", "N"),
                                                           sf1 = 4),
                                          conf_level = 0.84,
                                          gray = T,
                                          draw = F) # Get the estimates & CIs when sf1 high

preds.sf1high.coubase.check <- plot_predictions(m1.sf1.coubase,
                                                condition = list(tier_c = list("C"),
                                                                 sf1 = 1),
                                                conf_level = 0.84,
                                                gray = T,
                                                draw = F)

preds.sf1high.coubase.check

plot1.m1.coubase.sf1 <- plot_predictions(m1.sf1.coubase,
                                         condition = list(tier_c = list("M", "N"),
                                                          sf1 = "minmax"),
                                         conf_level = 0.95,
                                         gray = T)

plot1.m1.coubase.sf1 <-  plot1.m1.coubase.sf1 + geom_hline(yintercept = 1.807535, #c.bas
                                                           linetype = 1) +
  labs(x="Decision taken by",
       y="Willingness to accept decision (4-point merged scale)",
       shape="Preference for Self-Rule Item 1",
       title="a: Base category (line) is the decision taken by the county.") +
  scale_x_discrete(labels = c("Municipality",
                              "National government")) + ylim(1.25, 2.50) +
  scale_shape_discrete(labels = c("Min (= 1)", "Max (= 4)"))

plot1.m1.coubase.sf1 <- plot1.m1.coubase.sf1 + geom_linerange(data=preds.sf1low.coubase,
                                                              aes(y = estimate,
                                                                  x = tier_c,
                                                                  ymin = conf.low,
                                                                  ymax = conf.high),
                                                              position = position_nudge(x = -0.037),
                                                              linewidth = 1.2) +
  geom_linerange(data = preds.sf1high.coubase,
                 aes(y = estimate,
                     x = tier_c,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.037),
                 linewidth = 1.2)

plot1.m1.coubase.sf1 <- plot1.m1.coubase.sf1 + theme_classic() +
  theme(legend.position = c(0.75, 0.3),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, vjust = 1.5, size = 16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14)) + annotate('text', x=1,y=2.2, label='**', size=8)

plot1.m1.coubase.sf1$layers[[1]]$geom_params$fatten = 8

ggsave("Q1_sf1_left_coubase.jpeg", plot1.m1.coubase.sf1, width = 8, height = 6)


###### Line graph with C as baseline #####
# 95% CIs

plot2.m1.coubase.sf1 <- plot_predictions(m1.sf1.coubase,
                                         condition = list("sf1", "tier_c"),
                                         conf_level = 0.95,
                                         gray = T)

plot2.m1.coubase.sf1 <- plot2.m1.coubase.sf1 +
  labs(x = "Preference for Self-Rule Item 1",
       y = "",
       linetype = "Decision taken by",
       title="b: Base category (line) is the decision taken by the county.")

plot2.m1.coubase.sf1 <- plot2.m1.coubase.sf1 +
  scale_linetype_manual(labels = c("County",
                                   "Municipality",
                                   "National government"),
                        values = c("solid", "dotted", "dashed")) +
  theme_classic() + theme(legend.position = c(0.8, 0.3),
                          axis.title = element_text(size = 16),
                          axis.text = element_text(size = 14),
                          plot.title = element_text(hjust = 0.5, vjust = 1.5, size = 16),
                          legend.title = element_text(size = 14),
                          legend.text = element_text(size = 14)
  ) + ylim(1.15, 2.5)


ggsave("Q1_sf1_right_coubase_95CI.jpeg", plot2.m1.coubase.sf1, width = 11, height = 7)

# 84% CIs

plot2.m1.coubase.sf1.84ci <- plot_predictions(m1.sf1.coubase,
                                              condition = list("sf1", "tier_c"),
                                              conf_level = 0.84,
                                              gray = T)

plot2.m1.coubase.sf1.84ci <- plot2.m1.coubase.sf1.84ci +
  labs(x = "Preference for Self-Rule Item 1",
       y = "",
       linetype = "Decision taken by",
       title="b: Base category (line) is the decision taken by the county.")

plot2.m1.coubase.sf1.84ci <- plot2.m1.coubase.sf1.84ci +
  scale_linetype_manual(labels = c("County",
                                   "Municipality",
                                   "National government"),
                        values = c("solid", "dotted", "dashed")) +
  theme_classic() + theme(legend.position = c(0.75, 0.25),
                          axis.title = element_text(size = 16),
                          axis.text = element_text(size = 14),
                          plot.title = element_text(hjust = 0.5, vjust = 1.5, size = 16),
                          legend.title = element_text(size = 14),
                          legend.text = element_text(size = 14)
  ) + ylim(1.25, 2.5) 


ggsave("Q1_sf1_right_coubase_84CI.jpeg", plot2.m1.coubase.sf1.84ci, width = 11,
       height = 7)

combined.plots.coubase.sf1 <- ggarrange(plot1.m1.coubase.sf1, plot2.m1.coubase.sf1.84ci,
                                  nrow = 1, ncol = 2)

ggsave(combined.plots.coubase.sf1, filename = 'Figure_E1CBa_E1CBb.jpeg',
         width = 16, height = 8)


##### Figure E1CBc-E1CBd. Decision taken by the county to close upper secondary school: self-rule item 2. ####

m1.sf2.coubase <- feols(first_answer ~ sf2 * tier_c | responseid,
                        data = dataset)

summary(m1.sf2.coubase) # See coefficients of the model

preds.sf2low.coubase <- plot_predictions(m1.sf2.coubase,
                                         condition = list(tier_c = list("M", "N"),
                                                          sf2 = 1),
                                         conf_level = 0.84,
                                         gray = T,
                                         draw = F) # Get the estimates & CIs when sf2 low

preds.sf2high.coubase <- plot_predictions(m1.sf2.coubase,
                                          condition = list(tier_c = list("M", "N"),
                                                           sf2 = 4),
                                          conf_level = 0.84,
                                          gray = T,
                                          draw = F) # Get the estimates & CIs when sf2 high

preds.sf2high.coubase.check <- plot_predictions(m1.sf2.coubase,
                                                condition = list(tier_c = list("C"),
                                                                 sf2 = 1),
                                                conf_level = 0.84,
                                                gray = T,
                                                draw = F)

preds.sf2high.coubase.check

plot1.m1.coubase.sf2 <- plot_predictions(m1.sf2.coubase,
                                         condition = list(tier_c = list("M", "N"),
                                                          sf2 = "minmax"),
                                         conf_level = 0.95,
                                         gray = T)

plot1.m1.coubase.sf2 <-  plot1.m1.coubase.sf2 + geom_hline(yintercept = 1.831317, #c.bas
                                                           linetype = 1) +
  labs(x="Decision taken by",
       y="Willingness to accept decision (4-point merged scale)",
       shape="Preference for Self-Rule Item 2",
       title="c: Base category (line) is the decision taken by the county.") +
  scale_x_discrete(labels = c("Municipality",
                              "National government")) + ylim(1.25, 2.50) +
  scale_shape_discrete(labels = c("Min (= 1)", "Max (= 4)"))

plot1.m1.coubase.sf2 <- plot1.m1.coubase.sf2 + geom_linerange(data=preds.sf2low.coubase,
                                                              aes(y = estimate,
                                                                  x = tier_c,
                                                                  ymin = conf.low,
                                                                  ymax = conf.high),
                                                              position = position_nudge(x = -0.037),
                                                              linewidth = 1.2) +
  geom_linerange(data = preds.sf2high.coubase,
                 aes(y = estimate,
                     x = tier_c,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.037),
                 linewidth = 1.2)

plot1.m1.coubase.sf2 <- plot1.m1.coubase.sf2 + theme_classic() +
  theme(legend.position = c(0.75, 0.3),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, vjust = 1.5, size = 16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14)) + annotate('text', x=2, y=2.3,
                                                          label='*', size=8)

plot1.m1.coubase.sf2$layers[[1]]$geom_params$fatten = 8

ggsave("Q1_sf2_left_coubase.jpeg", plot1.m1.coubase.sf2, width = 8, height = 6)

###### Line graph with C as baseline ####

# 95% CIs

plot2.m1.coubase.sf2 <- plot_predictions(m1.sf2.coubase,
                                         condition = list("sf2", "tier_c"),
                                         conf_level = 0.95,
                                         gray = T)

plot2.m1.coubase.sf2 <- plot2.m1.coubase.sf2 +
  labs(x = "Preference for Self-Rule Item 2",
       y = "",
       linetype = "Decision taken by",
       title="d: Base category (line) is the decision taken by the county.")

plot2.m1.coubase.sf2 <- plot2.m1.coubase.sf2 +
  scale_linetype_manual(labels = c("County",
                                   "Municipality",
                                   "National government"),
                        values = c("solid", "dotted", "dashed")) +
  theme_classic() + theme(legend.position = c(0.8, 0.3),
                          axis.title = element_text(size = 16),
                          axis.text = element_text(size = 14),
                          plot.title = element_text(hjust = 0.5, vjust = 1.5, size = 16),
                          legend.title = element_text(size = 14),
                          legend.text = element_text(size = 14)
  ) + ylim(1.15, 2.5)


ggsave("Q1_sf2_right_coubase_95CI.jpeg", plot2.m1.coubase.sf2, width = 11, height = 7)

# 84% CIs

plot2.m1.coubase.sf2.84ci <- plot_predictions(m1.sf2.coubase,
                                              condition = list("sf2", "tier_c"),
                                              conf_level = 0.84,
                                              gray = T)

plot2.m1.coubase.sf2.84ci <- plot2.m1.coubase.sf2.84ci +
  labs(x = "Preference for Self-Rule Item 2",
       y = "",
       linetype = "Decision taken by",
       title="d: Base category (line) is the decision taken by the county.")

plot2.m1.coubase.sf2.84ci <- plot2.m1.coubase.sf2.84ci +
  scale_linetype_manual(labels = c("County",
                                   "Municipality",
                                   "National government"),
                        values = c("solid", "dotted", "dashed")) +
  theme_classic() + theme(legend.position = c(0.8, 0.3),
                          axis.title = element_text(size = 16),
                          axis.text = element_text(size = 14),
                          plot.title = element_text(hjust = 0.5, vjust = 1.5, size = 16),
                          legend.title = element_text(size = 14),
                          legend.text = element_text(size = 14)
  ) + ylim(1.25, 2.5)


ggsave("Q1_sf2_right_coubase_84CI.jpeg", plot2.m1.coubase.sf2.84ci, width = 11,
       height = 7)

combined.plots.coubase.sf2 <- ggarrange(plot1.m1.coubase.sf2, plot2.m1.coubase.sf2.84ci,
                                  nrow = 1, ncol = 2)

ggsave(combined.plots.coubase.sf2, filename = 'Figure_E1CBc_E1CBd.jpeg',
         width = 16, height = 8)

##### Figure E1CBe-E1CBf. Decision taken by the county to close upper secondary school: self-rule item 3. ####

m1.sf3.coubase <- feols(first_answer ~ sf3 * tier_c | responseid,
                        data = dataset)

summary(m1.sf3.coubase) # See coefficients of the model

preds.sf3low.coubase <- plot_predictions(m1.sf3.coubase,
                                         condition = list(tier_c = list("M", "N"),
                                                          sf3 = 1),
                                         conf_level = 0.84,
                                         gray = T,
                                         draw = F) # Get the estimates & CIs when sf3 low

preds.sf3high.coubase <- plot_predictions(m1.sf3.coubase,
                                          condition = list(tier_c = list("M", "N"),
                                                           sf3 = 4),
                                          conf_level = 0.84,
                                          gray = T,
                                          draw = F) # Get the estimates & CIs when sf3 high

preds.sf3high.coubase.check <- plot_predictions(m1.sf3.coubase,
                                                condition = list(tier_c = list("C"),
                                                                 sf3 = 1),
                                                conf_level = 0.84,
                                                gray = T,
                                                draw = F)

preds.sf3high.coubase.check

plot1.m1.coubase.sf3 <- plot_predictions(m1.sf3.coubase,
                                         condition = list(tier_c = list("M", "N"),
                                                          sf3 = "minmax"),
                                         conf_level = 0.95,
                                         gray = T)

plot1.m1.coubase.sf3 <-  plot1.m1.coubase.sf3 + geom_hline(yintercept = 1.827489, #c.bas
                                                           linetype = 1) +
  labs(x="Decision taken by",
       y="Willingness to accept decision (4-point merged scale)",
       shape="Preference for Self-Rule Item 3",
       title="e: Base category (line) is the decision taken by the county.") +
  scale_x_discrete(labels = c("Municipality",
                              "National government")) + ylim(1.25, 2.50) +
  scale_shape_discrete(labels = c("Min (= 1)", "Max (= 4)"))

plot1.m1.coubase.sf3 <- plot1.m1.coubase.sf3 + geom_linerange(data=preds.sf3low.coubase,
                                                              aes(y = estimate,
                                                                  x = tier_c,
                                                                  ymin = conf.low,
                                                                  ymax = conf.high),
                                                              position = position_nudge(x = -0.037),
                                                              linewidth = 1.2) +
  geom_linerange(data = preds.sf3high.coubase,
                 aes(y = estimate,
                     x = tier_c,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.037),
                 linewidth = 1.2)

plot1.m1.coubase.sf3 <- plot1.m1.coubase.sf3 + theme_classic() +
  theme(legend.position = c(0.75, 0.25),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, vjust = 1.5, size = 16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14))

plot1.m1.coubase.sf3$layers[[1]]$geom_params$fatten = 8

ggsave("Q1_sf3_left_coubase.jpeg", plot1.m1.coubase.sf3, width = 8, height = 6)


###### Line graph with C as baseline ####

# 95% CIs

plot2.m1.coubase.sf3 <- plot_predictions(m1.sf3.coubase,
                                         condition = list("sf3", "tier_c"),
                                         conf_level = 0.95,
                                         gray = T)

plot2.m1.coubase.sf3 <- plot2.m1.coubase.sf3 +
  labs(x = "Preference for Self-Rule Item 3",
       y = "",
       linetype = "Decision taken by",
       title="f: Base category (line) is the decision taken by the county.")

plot2.m1.coubase.sf3 <- plot2.m1.coubase.sf3 +
  scale_linetype_manual(labels = c("County",
                                   "Municipality",
                                   "National government"),
                        values = c("solid", "dotted", "dashed")) +
  theme_classic() + theme(legend.position = c(0.8, 0.25),
                          axis.title = element_text(size = 16),
                          axis.text = element_text(size = 14),
                          plot.title = element_text(hjust = 0.5, vjust = 1.5, size = 16),
                          legend.title = element_text(size = 14),
                          legend.text = element_text(size = 14)
  ) + ylim(1.15, 2.5)


ggsave("Q1_sf3_right_coubase_95CI.jpeg", plot2.m1.coubase.sf3, width = 11, height = 7)

# 84% CIs

plot2.m1.coubase.sf3.84ci <- plot_predictions(m1.sf3.coubase,
                                              condition = list("sf3", "tier_c"),
                                              conf_level = 0.84,
                                              gray = T)

plot2.m1.coubase.sf3.84ci <- plot2.m1.coubase.sf3.84ci +
  labs(x = "Preference for Self-Rule Item 3",
       y = "",
       linetype = "Decision taken by",
       title="f: Base category (line) is the decision taken by the county.")

plot2.m1.coubase.sf3.84ci <- plot2.m1.coubase.sf3.84ci +
  scale_linetype_manual(labels = c("County",
                                   "Municipality",
                                   "National government"),
                        values = c("solid", "dotted", "dashed")) +
  theme_classic() + theme(legend.position = c(0.8, 0.25),
                          axis.title = element_text(size = 16),
                          axis.text = element_text(size = 14),
                          plot.title = element_text(hjust = 0.5, vjust = 1.5, size = 16),
                          legend.title = element_text(size = 14),
                          legend.text = element_text(size = 14)
  ) + ylim(1.25, 2.5)


ggsave("Q1_sf3_right_coubase_84CI.jpeg", plot2.m1.coubase.sf3.84ci, width = 11,
       height = 7)

combined.plots.coubase.sf3 <- ggarrange(plot1.m1.coubase.sf3, plot2.m1.coubase.sf3.84ci,
                                  nrow = 1, ncol = 2)

ggsave(combined.plots.coubase.sf3, filename = 'Figure_E1CBe_E1CBf.jpeg',
         width = 16, height = 8)


#### Figure E1CC. Model 1-Q1-Q3-Q5-base-cat. = N: Preference for self-rule items 1-3. ####

##### Figure E1CCa-E1CCb. Decision by the national government to close a department of an university (college): self-rule item 1. ####

m1.sf1.natbase <- feols(first_answer ~ sf1 * tier_n | responseid,
                        data = dataset)

summary(m1.sf1.natbase) 

preds.sf1low.natbase <- plot_predictions(m1.sf1.natbase,
                                         condition = list(tier_n = list("M", "C"),
                                                          sf1 = 1),
                                         conf_level = 0.84,
                                         gray = T,
                                         draw = F) # Get the estimates & CIs when sf1 low

preds.sf1high.natbase <- plot_predictions(m1.sf1.natbase,
                                          condition = list(tier_n = list("M", "C"),
                                                           sf1 = 4),
                                          conf_level = 0.84,
                                          gray = T,
                                          draw = F) # Get the estimates & CIs when sf1 high

plot1.m1.natbase.sf1 <- plot_predictions(m1.sf1.natbase,
                                         condition = list(tier_n = list("M", "C"),
                                                          sf1 = "minmax"),
                                         conf_level = 0.95,
                                         gray = T)

plot1.m1.natbase.sf1.check <- plot_predictions(m1.sf1.natbase,
                                               condition = list(tier_n = list("N"),
                                                                sf1 = 1),
                                               conf_level = 0.84,
                                               gray = T,
                                               draw = F)

plot1.m1.natbase.sf1.check

plot1.m1.natbase.sf1 <-  plot1.m1.natbase.sf1 + geom_hline(yintercept = 2.087522 , #n.bas
                                                           linetype = 1) +
  labs(x="Decision taken by",
       y="Willingness to accept decision (4-point merged scale)",
       shape="Preference for Self-Rule Item 1",
       title="a: Base category (line) is the decision taken by the national government.") +
  scale_x_discrete(labels = c("Municipality",
                              "County"))  +
  scale_shape_discrete(labels = c("Min (= 1)", "Max (= 4)"))

plot1.m1.natbase.sf1 <- plot1.m1.natbase.sf1 + geom_linerange(data=preds.sf1low.natbase,
                                                              aes(y = estimate,
                                                                  x = tier_n,
                                                                  ymin = conf.low,
                                                                  ymax = conf.high),
                                                              position = position_nudge(x = -0.037),
                                                              linewidth = 1.2) +
  geom_linerange(data = preds.sf1high.natbase,
                 aes(y = estimate,
                     x = tier_n,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.037),
                 linewidth = 1.2) + ylim(1.25, 2.5)

plot1.m1.natbase.sf1 <- plot1.m1.natbase.sf1 + theme_classic() +
  theme(legend.position = c(0.75, 0.9),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        plot.title = element_text(hjust = 0.5, vjust = 1.5, size = 16)) + annotate('text', x=1, y=2.25,
                                                                                   label='**', size=8)

plot1.m1.natbase.sf1$layers[[1]]$geom_params$fatten = 8

ggsave("Q1_sf1_left_natbase.jpeg", plot1.m1.natbase.sf1, width = 8, height = 6)

###### Line graph with N as baseline #####

# 95% CIs

plot2.m1.natbase.sf1 <- plot_predictions(m1.sf1.natbase,
                                         condition = list("sf1", "tier_n"),
                                         conf_level = 0.95,
                                         gray = T)

plot2.m1.natbase.sf1 <- plot2.m1.natbase.sf1 +
  labs(x = "Preference for Self-Rule Item 1",
       y = "Willingness to accept decision (4-point merged scale)",
       linetype = "Decision taken by",
       title="b: Base category (line) is the decision taken by the national government.")

plot2.m1.natbase.sf1 <- plot2.m1.natbase.sf1 +
  scale_linetype_manual(labels = c("National government",
                                   "Municipality",
                                   "County"),
                        values = c("solid", "dashed", "dotted")) +
  theme_classic() + theme(legend.position = c(0.8, 0.2),
                          legend.text = element_text(size=14),
                          legend.title = element_text(size=14),
                          axis.title = element_text(size = 16),
                          axis.text = element_text(size = 14),
                          plot.title = element_text(hjust = .5,
                                                    vjust = 1.5,
                                                    size = 16)
  ) + ylim(1.25, 2.5)


ggsave("Q1_sf1_right_natbase_95CI.jpeg", plot2.m1.natbase.sf1, width = 11, height = 7)

# 84% CIs

plot2.m1.natbase.84ci.sf1 <- plot_predictions(m1.sf1.natbase,
                                              condition = list("sf1", "tier_n"),
                                              conf_level = 0.84,
                                              gray = T)

plot2.m1.natbase.84ci.sf1 <- plot2.m1.natbase.84ci.sf1 +
  labs(x = "Preference for Self-Rule Item 1",
       y = "",
       linetype = "Decision taken by",
       title="b: Base category (line) is the decision taken by the national government.")

plot2.m1.natbase.84ci.sf1 <- plot2.m1.natbase.84ci.sf1 +
  scale_linetype_manual(labels = c("National government",
                                   "Municipality",
                                   "County"),
                        values = c("solid", "dashed", "dotted")) +
  theme_classic() + theme(legend.position = c(0.8, 0.2),
                          legend.title = element_text(size = 14),
                          legend.text = element_text(size = 14),
                          axis.title = element_text(size = 16),
                          axis.text = element_text(size = 12),
                          plot.title = element_text(hjust = .5,
                                                    vjust = 1.5,
                                                    size = 16)
  ) + ylim(1.25, 2.50)


ggsave("Q1_sf1_right_natbase_84CI.jpeg", plot2.m1.natbase.84ci.sf1, width = 11,
       height = 7)

combined.plots.natbase.sf1 <- ggarrange(plot1.m1.natbase.sf1, plot2.m1.natbase.84ci.sf1,
                                  nrow = 1, ncol = 2)

ggsave(combined.plots.natbase.sf1, filename = 'Figure_E1CCa_E1CCb.jpeg',
         width = 16, height = 8)


##### Figure E1CCc-E1CCd. Decision by the national government to close a department of an university (college): self-rule item 2. ####

m1.sf2.natbase <- feols(first_answer ~ sf2 * tier_n | responseid,
                        data = dataset)

summary(m1.sf2.natbase) 

preds.sf2low.natbase <- plot_predictions(m1.sf2.natbase,
                                         condition = list(tier_n = list("M", "C"),
                                                          sf2 = 1),
                                         conf_level = 0.84,
                                         gray = T,
                                         draw = F) # Get the estimates & CIs when sf2 low

preds.sf2high.natbase <- plot_predictions(m1.sf2.natbase,
                                          condition = list(tier_n = list("M", "C"),
                                                           sf2 = 4),
                                          conf_level = 0.84,
                                          gray = T,
                                          draw = F) # Get the estimates & CIs when sf2 high

plot1.m1.natbase.sf2 <- plot_predictions(m1.sf2.natbase,
                                         condition = list(tier_n = list("M", "C"),
                                                          sf2 = "minmax"),
                                         conf_level = 0.95,
                                         gray = T)

plot1.m1.natbase.sf2.check <- plot_predictions(m1.sf2.natbase,
                                               condition = list(tier_n = list("N"),
                                                                sf2 = 1),
                                               conf_level = 0.84,
                                               gray = T,
                                               draw = F)

plot1.m1.natbase.sf2.check

plot1.m1.natbase.sf2 <-  plot1.m1.natbase.sf2 + geom_hline(yintercept = 2.097818 , #n.bas
                                                           linetype = 1) +
  labs(x="Decision taken by",
       y="Willingness to accept decision (4-point merged scale)",
       shape="Preference for Self-Rule Item 2",
       title="c: Base category (line) is the decision taken by the national government.") +
  scale_x_discrete(labels = c("Municipality",
                              "County"))  +
  scale_shape_discrete(labels = c("Min (= 1)", "Max (= 4)"))

plot1.m1.natbase.sf2 <- plot1.m1.natbase.sf2 + geom_linerange(data=preds.sf2low.natbase,
                                                              aes(y = estimate,
                                                                  x = tier_n,
                                                                  ymin = conf.low,
                                                                  ymax = conf.high),
                                                              position = position_nudge(x = -0.037),
                                                              linewidth = 1.2) +
  geom_linerange(data = preds.sf2high.natbase,
                 aes(y = estimate,
                     x = tier_n,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.037),
                 linewidth = 1.2) + ylim(1.25, 2.5)

plot1.m1.natbase.sf2 <- plot1.m1.natbase.sf2 + theme_classic() +
  theme(legend.position = c(0.25, 0.9),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        plot.title = element_text(hjust = 0.5, vjust = 1.5, size = 16)) + annotate('text', x=2,y=2,
                                                                                   label='*', size=8)

plot1.m1.natbase.sf2$layers[[1]]$geom_params$fatten = 8

ggsave("Q1_sf2_left_natbase.jpeg", plot1.m1.natbase.sf2, width = 8, height = 6)

###### Line graph with N as baseline #####

# 95% CIs

plot2.m1.natbase.sf2 <- plot_predictions(m1.sf2.natbase,
                                         condition = list("sf2", "tier_n"),
                                         conf_level = 0.95,
                                         gray = T)

plot2.m1.natbase.sf2 <- plot2.m1.natbase.sf2 +
  labs(x = "Preference for Self-Rule Item 2",
       y = "Willingness to accept decision (4-point merged scale)",
       linetype = "Decision taken by",
       title="d: Base category (line) is the decision taken by the national government.")

plot2.m1.natbase.sf2 <- plot2.m1.natbase.sf2 +
  scale_linetype_manual(labels = c("National government",
                                   "Municipality",
                                   "County"),
                        values = c("solid", "dashed", "dotted")) +
  theme_classic() + theme(legend.position = c(0.8, 0.2),
                          legend.text = element_text(size=14),
                          legend.title = element_text(size=14),
                          axis.title = element_text(size = 16),
                          axis.text = element_text(size = 14),
                          plot.title = element_text(hjust = .5,
                                                    vjust = 1.5,
                                                    size = 16)
  ) + ylim(1.25, 2.5)


ggsave("Q1_sf2_right_natbase_95CI.jpeg", plot2.m1.natbase.sf2, width = 11, height = 7)

# 84% CIs

plot2.m1.natbase.84ci.sf2 <- plot_predictions(m1.sf2.natbase,
                                              condition = list("sf2", "tier_n"),
                                              conf_level = 0.84,
                                              gray = T)

plot2.m1.natbase.84ci.sf2 <- plot2.m1.natbase.84ci.sf2 +
  labs(x = "Preference for Self-Rule Item 2",
       y = "",
       linetype = "Decision taken by",
       title="d: Base category (line) is the decision taken by the national government.")

plot2.m1.natbase.84ci.sf2 <- plot2.m1.natbase.84ci.sf2 +
  scale_linetype_manual(labels = c("National government",
                                   "Municipality",
                                   "County"),
                        values = c("solid", "dashed", "dotted")) +
  theme_classic() + theme(legend.position = c(0.8, 0.2),
                          legend.title = element_text(size = 14),
                          legend.text = element_text(size = 14),
                          axis.title = element_text(size = 16),
                          axis.text = element_text(size = 12),
                          plot.title = element_text(hjust = .5,
                                                    vjust = 1.5,
                                                    size = 16)
  ) + ylim(1.25, 2.50)


ggsave("Q1_sf2_right_natbase_84CI.jpeg", plot2.m1.natbase.84ci.sf2, width = 11,
       height = 7)

combined.plots.natbase.sf2 <- ggarrange(plot1.m1.natbase.sf2, plot2.m1.natbase.84ci.sf2,
                                  nrow = 1, ncol = 2)

ggsave(combined.plots.natbase.sf2, filename = 'Figure_E1CCc_E1CCd.jpeg',
         width = 16, height = 8)

##### Figure E1CCe-E1CCf. Decision by the national government to close a department of an university (college): self-rule item 3. ####

m1.sf3.natbase <- feols(first_answer ~ sf3 * tier_n | responseid,
                        data = dataset)

summary(m1.sf3.natbase) 

preds.sf3low.natbase <- plot_predictions(m1.sf3.natbase,
                                         condition = list(tier_n = list("M", "C"),
                                                          sf3 = 1),
                                         conf_level = 0.84,
                                         gray = T,
                                         draw = F) # Get the estimates & CIs when sf3 low

preds.sf3high.natbase <- plot_predictions(m1.sf3.natbase,
                                          condition = list(tier_n = list("M", "C"),
                                                           sf3 = 4),
                                          conf_level = 0.84,
                                          gray = T,
                                          draw = F) # Get the estimates & CIs when sf3 high

plot1.m1.natbase.sf3 <- plot_predictions(m1.sf3.natbase,
                                         condition = list(tier_n = list("M", "C"),
                                                          sf3 = "minmax"),
                                         conf_level = 0.95,
                                         gray = T)

plot1.m1.natbase.sf3.check <- plot_predictions(m1.sf3.natbase,
                                               condition = list(tier_n = list("N"),
                                                                sf3 = 1),
                                               conf_level = 0.84,
                                               gray = T,
                                               draw = F)

plot1.m1.natbase.sf3.check

plot1.m1.natbase.sf3 <-  plot1.m1.natbase.sf3 + geom_hline(yintercept = 2.104309 , #n.bas
                                                           linetype = 1) +
  labs(x="Decision taken by",
       y="Willingness to accept decision (4-point merged scale)",
       shape="Preference for Self-Rule Item 3",
       title="e: Base category (line) is the decision taken by the national government.") +
  scale_x_discrete(labels = c("Municipality",
                              "County"))  +
  scale_shape_discrete(labels = c("Min (= 1)", "Max (= 4)"))

plot1.m1.natbase.sf3 <- plot1.m1.natbase.sf3 + geom_linerange(data=preds.sf3low.natbase,
                                                              aes(y = estimate,
                                                                  x = tier_n,
                                                                  ymin = conf.low,
                                                                  ymax = conf.high),
                                                              position = position_nudge(x = -0.037),
                                                              linewidth = 1.2) +
  geom_linerange(data = preds.sf3high.natbase,
                 aes(y = estimate,
                     x = tier_n,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.037),
                 linewidth = 1.2) + ylim(1.25, 2.5)

plot1.m1.natbase.sf3 <- plot1.m1.natbase.sf3 + theme_classic() +
  theme(legend.position = c(0.25, 0.9),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        plot.title = element_text(hjust = 0.5, vjust = 1.5, size = 16))

plot1.m1.natbase.sf3$layers[[1]]$geom_params$fatten = 8

ggsave("Q1_sf3_left_natbase.jpeg", plot1.m1.natbase.sf3, width = 8, height = 6)

###### Line graph with N as baseline #####

# 95% CIs

plot2.m1.natbase.sf3 <- plot_predictions(m1.sf3.natbase,
                                         condition = list("sf3", "tier_n"),
                                         conf_level = 0.95,
                                         gray = T)

plot2.m1.natbase.sf3 <- plot2.m1.natbase.sf3 +
  labs(x = "Preference for Self-Rule Item 3",
       y = "Willingness to accept decision (4-point merged scale)",
       linetype = "Decision taken by",
       title="f: Base category (line) is the decision taken by the national government.")

plot2.m1.natbase.sf3 <- plot2.m1.natbase.sf3 +
  scale_linetype_manual(labels = c("National government",
                                   "Municipality",
                                   "County"),
                        values = c("solid", "dashed", "dotted")) +
  theme_classic() + theme(legend.position = c(0.8, 0.2),
                          legend.text = element_text(size=14),
                          legend.title = element_text(size=14),
                          axis.title = element_text(size = 16),
                          axis.text = element_text(size = 14),
                          plot.title = element_text(hjust = .5,
                                                    vjust = 1.5,
                                                    size = 16)
  ) + ylim(1.25, 2.5)


ggsave("Q1_sf3_right_natbase_95CI.jpeg", plot2.m1.natbase.sf3, width = 11, height = 7)

# 84% CIs

plot2.m1.natbase.84ci.sf3 <- plot_predictions(m1.sf3.natbase,
                                              condition = list("sf3", "tier_n"),
                                              conf_level = 0.84,
                                              gray = T)

plot2.m1.natbase.84ci.sf3 <- plot2.m1.natbase.84ci.sf3 +
  labs(x = "Preference for Self-Rule Item 3",
       y = "",
       linetype = "Decision taken by",
       title="f: Base category (line) is the decision taken by the national government.")

plot2.m1.natbase.84ci.sf3 <- plot2.m1.natbase.84ci.sf3 +
  scale_linetype_manual(labels = c("National government",
                                   "Municipality",
                                   "County"),
                        values = c("solid", "dashed", "dotted")) +
  theme_classic() + theme(legend.position = c(0.8, 0.2),
                          legend.title = element_text(size = 14),
                          legend.text = element_text(size = 14),
                          axis.title = element_text(size = 16),
                          axis.text = element_text(size = 12),
                          plot.title = element_text(hjust = .5,
                                                    vjust = 1.5,
                                                    size = 16)
  ) + ylim(1.25, 2.50)


ggsave("Q1_sf3_right_natbase_84CI.jpeg", plot2.m1.natbase.84ci.sf3, width = 11,
       height = 7)

combined.plots.natbase.sf3 <- ggarrange(plot1.m1.natbase.sf3, plot2.m1.natbase.84ci.sf3,
                                  nrow = 1, ncol = 2)

ggsave(combined.plots.natbase.sf3, filename = 'Figure_E1CCe_E1CCf.jpeg',
         width = 16, height = 8)


#### Figure E1Cd. Model 1-Q1-Q3-Q5-base-cat. = M: Preference for shared rule items 4-6. ####
##### Figure E1CDa-E1CDb. Decision by the municipality to close kindergarten: shared rule item 1.####

m1.sh1.munbase <- feols(first_answer ~ sh1 * tier_m | responseid,
                        data = dataset)
summary(m1.sh1.munbase)

preds.sh1low.munbase <- plot_predictions(m1.sh1.munbase,
                                         condition = list(tier_m = list("C", "N"),
                                                          sh1= 1),
                                         conf_level = 0.84,
                                         gray = T,
                                         draw = F) # Get the estimates & CIs when sh1low

preds.sh1high.munbase <- plot_predictions(m1.sh1.munbase,
                                          condition = list(tier_m = list("C", "N"),
                                                           sh1= 4),
                                          conf_level = 0.84,
                                          gray = T,
                                          draw = F) # Get the estimates & CIs when sh1high

preds.base.munbase.sh1.check <- plot_predictions(m1.sh1.munbase,
                                                 condition = list(tier_m = list("M"),
                                                                  sh1= 1),
                                                 conf_level = 0.84,
                                                 gray = T,
                                                 draw = F) # check M estimate (baseline)

preds.base.munbase.sh1.check

plot1.m1.munbase.sh1 <- plot_predictions(m1.sh1.munbase,
                                         condition = list(tier_m = list("C", "N"),
                                                          sh1= "minmax"),
                                         conf_level = 0.95,
                                         gray = T)

plot1.m1.munbase.sh1 <-  plot1.m1.munbase.sh1 + geom_hline(yintercept = 2.068949, #m.bas
                                                           linetype = 1) +
  labs(x="Decision taken by",
       y="Willingness to accept decision (4-point merged scale)",
       shape="Preference for Shared Rule Item 4",
       title="a: Base category (line) is the decision taken by the municipality.") +
  scale_x_discrete(labels = c("County",
                              "National government")) + ylim(1.25, 2.50) +
  scale_shape_discrete(labels = c("Min (= 1)", "Max (= 4)"))

plot1.m1.munbase.sh1 <- plot1.m1.munbase.sh1 + geom_linerange(data=preds.sh1low.munbase,
                                                              aes(y = estimate,
                                                                  x = tier_m,
                                                                  ymin = conf.low,
                                                                  ymax = conf.high),
                                                              position = position_nudge(x = -0.0363),
                                                              linewidth = 1.2) +
  geom_linerange(data = preds.sh1high.munbase,
                 aes(y = estimate,
                     x = tier_m,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.037),
                 linewidth = 1.2)

plot1.m1.munbase.sh1 <- plot1.m1.munbase.sh1 + theme_classic() +
  theme(legend.position = c(0.25, 0.85),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        plot.title = element_text(hjust = 0.5, vjust = 1.5, size = 16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14)) + annotate('text', x=2,y=2.3,
                                                          label='*', size=8)

plot1.m1.munbase.sh1$layers[[1]]$geom_params$fatten = 8

ggsave("Q1_sh1_left_munbase.jpeg", plot1.m1.munbase.sh1, width = 8, height = 6)

###### Line graph with M as baseline #####

# 95% CIs

plot2.m1.munbase.sh1 <- plot_predictions(m1.sh1.munbase,
                                         condition = list("sh1", "tier_m"),
                                         conf_level = 0.95,
                                         gray = T)

plot2.m1.munbase.sh1 <- plot2.m1.munbase.sh1 +
  labs(x = "Preference for Shared Rule Item 4",
       y = "",
       linetype = "Decision taken by",
       title="b: Base category (line) is the decision taken by the municipality.")

plot2.m1.munbase.sh1 <- plot2.m1.munbase.sh1 +
  scale_linetype_manual(labels = c("Municipality",
                                   "County",
                                   "National government"),
                        values = c("solid", "dotted", "dashed")) +
  theme_classic() + theme(legend.position = c(0.8, 0.25),
                          legend.title = element_text(size = 14),
                          legend.text = element_text(size = 14),
                          axis.title = element_text(size = 16),
                          axis.text = element_text(size = 12),
                          plot.title = element_text(hjust = .5,
                                                    vjust = 1.5, size = 16)
  ) + ylim(1.15, 2.5)


ggsave("Q1_sh1_right_munbase_95CI.jpeg", plot2.m1.munbase.sh1, width = 11, height = 7)

# 84% CIs

plot2.m1.munbase.84ci.sh1 <- plot_predictions(m1.sh1.munbase,
                                              condition = list("sh1", "tier_m"),
                                              conf_level = 0.84,
                                              gray = T)

plot2.m1.munbase.84ci.sh1 <- plot2.m1.munbase.84ci.sh1 +
  labs(x = "Preference for Shared Rule Item 4",
       y = "",
       linetype = "Decision taken by",
       title="b: Base category (line) is the decision taken by the municipality.")

plot2.m1.munbase.84ci.sh1 <- plot2.m1.munbase.84ci.sh1 +
  scale_linetype_manual(labels = c("Municipality",
                                   "County",
                                   "National government"),
                        values = c("solid", "dotted", "dashed")) +
  theme_classic() + theme(legend.position = c(0.8, 0.25),
                          legend.title = element_text(size = 14),
                          legend.text = element_text(size = 14),
                          axis.title = element_text(size = 16),
                          axis.text = element_text(size = 12),
                          plot.title = element_text(hjust = .5,
                                                    vjust = 1.5, size = 16)
  ) + ylim(1.25, 2.50)


ggsave("Q1_sh1_right_munbase_84CI.jpeg", plot2.m1.munbase.84ci.sh1, width = 11,
       height = 7)

combined.plots.munbase.sh1 <- ggarrange(plot1.m1.munbase.sh1, plot2.m1.munbase.84ci.sh1,
                                  nrow = 1, ncol = 2)

ggsave(combined.plots.munbase.sh1, filename = 'Figure_E1CDa_E1CDb.jpeg',
         width = 16, height = 8)

##### Figure E1CDc-E1CDd. Decision by the municipality to close kindergarten: shared rule item 2. ####

m1.sh2.munbase <- feols(first_answer ~ sh2 * tier_m | responseid,
                        data = dataset)
summary(m1.sh2.munbase)

preds.sh2low.munbase <- plot_predictions(m1.sh2.munbase,
                                         condition = list(tier_m = list("C", "N"),
                                                          sh2= 1),
                                         conf_level = 0.84,
                                         gray = T,
                                         draw = F) # Get the estimates & CIs when sh2low

preds.sh2high.munbase <- plot_predictions(m1.sh2.munbase,
                                          condition = list(tier_m = list("C", "N"),
                                                           sh2= 4),
                                          conf_level = 0.84,
                                          gray = T,
                                          draw = F) # Get the estimates & CIs when sh2high

preds.base.munbase.sh2.check <- plot_predictions(m1.sh2.munbase,
                                                 condition = list(tier_m = list("M"),
                                                                  sh2= 1),
                                                 conf_level = 0.84,
                                                 gray = T,
                                                 draw = F) # check M estimate (baseline)

preds.base.munbase.sh2.check

plot1.m1.munbase.sh2 <- plot_predictions(m1.sh2.munbase,
                                         condition = list(tier_m = list("C", "N"),
                                                          sh2= "minmax"),
                                         conf_level = 0.95,
                                         gray = T)

plot1.m1.munbase.sh2 <-  plot1.m1.munbase.sh2 + geom_hline(yintercept = 2.067146, #m.bas
                                                           linetype = 1) +
  labs(x="Decision taken by",
       y="Willingness to accept decision (4-point merged scale)",
       shape="Preference for Shared Rule Item 5",
       title="c: Base category (line) is the decision taken by the municipality.") +
  scale_x_discrete(labels = c("County",
                              "National government")) + ylim(1.25, 2.50) +
  scale_shape_discrete(labels = c("Min (= 1)", "Max (= 4)"))

plot1.m1.munbase.sh2 <- plot1.m1.munbase.sh2 + geom_linerange(data=preds.sh2low.munbase,
                                                              aes(y = estimate,
                                                                  x = tier_m,
                                                                  ymin = conf.low,
                                                                  ymax = conf.high),
                                                              position = position_nudge(x = -0.0363),
                                                              linewidth = 1.2) +
  geom_linerange(data = preds.sh2high.munbase,
                 aes(y = estimate,
                     x = tier_m,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.037),
                 linewidth = 1.2)

plot1.m1.munbase.sh2 <- plot1.m1.munbase.sh2 + theme_classic() +
  theme(legend.position = c(0.25, 0.85),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        plot.title = element_text(hjust = 0.5, vjust = 1.5, size = 16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14))

plot1.m1.munbase.sh2$layers[[1]]$geom_params$fatten = 8

ggsave("Q1_sh2_left_munbase.jpeg", plot1.m1.munbase.sh2, width = 8, height = 6)

###### Line graph with M as baseline #####

# 95% CIs

plot2.m1.munbase.sh2 <- plot_predictions(m1.sh2.munbase,
                                         condition = list("sh2", "tier_m"),
                                         conf_level = 0.95,
                                         gray = T)

plot2.m1.munbase.sh2 <- plot2.m1.munbase.sh2 +
  labs(x = "Preference for Shared Rule Item 5",
       y = "",
       linetype = "Decision taken by",
       title="d: Base category (line) is the decision taken by the municipality.")

plot2.m1.munbase.sh2 <- plot2.m1.munbase.sh2 +
  scale_linetype_manual(labels = c("Municipality",
                                   "County",
                                   "National government"),
                        values = c("solid", "dotted", "dashed")) +
  theme_classic() + theme(legend.position = c(0.8, 0.25),
                          legend.title = element_text(size = 14),
                          legend.text = element_text(size = 14),
                          axis.title = element_text(size = 16),
                          axis.text = element_text(size = 12),
                          plot.title = element_text(hjust = .5,
                                                    vjust = 1.5, size = 16)
  ) + ylim(1.15, 2.5)


ggsave("Q1_sh2_right_munbase_95CI.jpeg", plot2.m1.munbase.sh2, width = 11, height = 7)

# 84% CIs

plot2.m1.munbase.84ci.sh2 <- plot_predictions(m1.sh2.munbase,
                                              condition = list("sh2", "tier_m"),
                                              conf_level = 0.84,
                                              gray = T)

plot2.m1.munbase.84ci.sh2 <- plot2.m1.munbase.84ci.sh2 +
  labs(x = "Preference for Shared Rule Item 5",
       y = "",
       linetype = "Decision taken by",
       title="d: Base category (line) is the decision taken by the municipality.")

plot2.m1.munbase.84ci.sh2 <- plot2.m1.munbase.84ci.sh2 +
  scale_linetype_manual(labels = c("Municipality",
                                   "County",
                                   "National government"),
                        values = c("solid", "dotted", "dashed")) +
  theme_classic() + theme(legend.position = c(0.8, 0.25),
                          legend.title = element_text(size = 14),
                          legend.text = element_text(size = 14),
                          axis.title = element_text(size = 16),
                          axis.text = element_text(size = 12),
                          plot.title = element_text(hjust = .5,
                                                    vjust = 1.5, size = 16)
  ) + ylim(1.25, 2.50)


ggsave("Q1_sh2_right_munbase_84CI.jpeg", plot2.m1.munbase.84ci.sh2, width = 11,
       height = 7)


combined.plots.munbase.sh2 <- ggarrange(plot1.m1.munbase.sh2, plot2.m1.munbase.84ci.sh2,
                                  nrow = 1, ncol = 2)

ggsave(combined.plots.munbase.sh2, filename = 'Figure_E1CDc_E1CDd.jpeg',
         width = 16, height = 8)


##### Figure E1CDe-E1CDf. Decision by the municipality to close kindergarten: shared rule item 3.####

m1.sh3.munbase <- feols(first_answer ~ sh3* tier_m | responseid,
                        data = dataset)
summary(m1.sh3.munbase)

preds.sh3low.munbase <- plot_predictions(m1.sh3.munbase,
                                         condition = list(tier_m = list("C", "N"),
                                                          sh3= 1),
                                         conf_level = 0.84,
                                         gray = T,
                                         draw = F) # Get the estimates & CIs when sh3low

preds.sh3high.munbase <- plot_predictions(m1.sh3.munbase,
                                          condition = list(tier_m = list("C", "N"),
                                                           sh3= 4),
                                          conf_level = 0.84,
                                          gray = T,
                                          draw = F) # Get the estimates & CIs when sh3high

preds.base.munbase.sh3.check <- plot_predictions(m1.sh3.munbase,
                                                 condition = list(tier_m = list("M"),
                                                                  sh3= 1),
                                                 conf_level = 0.84,
                                                 gray = T,
                                                 draw = F) # check M estimate (baseline)

preds.base.munbase.sh3.check

plot1.m1.munbase.sh3 <- plot_predictions(m1.sh3.munbase,
                                         condition = list(tier_m = list("C", "N"),
                                                          sh3= "minmax"),
                                         conf_level = 0.95,
                                         gray = T)

plot1.m1.munbase.sh3 <-  plot1.m1.munbase.sh3 + geom_hline(yintercept = 2.085002, #m.bas
                                                           linetype = 1) +
  labs(x="Decision taken by",
       y="Willingness to accept decision (4-point merged scale)",
       shape="Preference for Shared Rule Item 6",
       title="e: Base category (line) is the decision taken by the municipality.") +
  scale_x_discrete(labels = c("County",
                              "National government")) + ylim(1.25, 2.50) +
  scale_shape_discrete(labels = c("Min (= 1)", "Max (= 4)"))

plot1.m1.munbase.sh3 <- plot1.m1.munbase.sh3 + geom_linerange(data=preds.sh3low.munbase,
                                                              aes(y = estimate,
                                                                  x = tier_m,
                                                                  ymin = conf.low,
                                                                  ymax = conf.high),
                                                              position = position_nudge(x = -0.0363),
                                                              linewidth = 1.2) +
  geom_linerange(data = preds.sh3high.munbase,
                 aes(y = estimate,
                     x = tier_m,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.037),
                 linewidth = 1.2)

plot1.m1.munbase.sh3 <- plot1.m1.munbase.sh3 + theme_classic() +
  theme(legend.position = c(0.25, 0.85),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        plot.title = element_text(hjust = 0.5, vjust = 1.5, size = 16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14)) + annotate('text', x=2,y=2.5,
                                                          label='*', size=8)

plot1.m1.munbase.sh3$layers[[1]]$geom_params$fatten = 8

ggsave("Q1_sh3_left_munbase.jpeg", plot1.m1.munbase.sh3, width = 8, height = 6)

###### Line graph with M as baseline ####

# 95% CIs

plot2.m1.munbase.sh3 <- plot_predictions(m1.sh3.munbase,
                                         condition = list("sh3", "tier_m"),
                                         conf_level = 0.95,
                                         gray = T)

plot2.m1.munbase.sh3 <- plot2.m1.munbase.sh3 +
  labs(x = "Preference for Shared Rule Item 6",
       y = "",
       linetype = "Decision taken by",
       title="f: Base category (line) is the decision taken by the municipality.")

plot2.m1.munbase.sh3 <- plot2.m1.munbase.sh3 +
  scale_linetype_manual(labels = c("Municipality",
                                   "County",
                                   "National government"),
                        values = c("solid", "dotted", "dashed")) +
  theme_classic() + theme(legend.position = c(0.8, 0.25),
                          legend.title = element_text(size = 14),
                          legend.text = element_text(size = 14),
                          axis.title = element_text(size = 16),
                          axis.text = element_text(size = 12),
                          plot.title = element_text(hjust = .5,
                                                    vjust = 1.5, size = 16)
  ) + ylim(1.15, 2.5)


ggsave("Q1_sh3_right_munbase_95CI.jpeg", plot2.m1.munbase.sh3, width = 11, height = 7)

# 84% CIs

plot2.m1.munbase.84ci.sh3 <- plot_predictions(m1.sh3.munbase,
                                              condition = list("sh3", "tier_m"),
                                              conf_level = 0.84,
                                              gray = T)

plot2.m1.munbase.84ci.sh3 <- plot2.m1.munbase.84ci.sh3 +
  labs(x = "Preference for Shared Rule Item 6",
       y = "",
       linetype = "Decision taken by",
       title="f: Base category (line) is the decision taken by the municipality.")

plot2.m1.munbase.84ci.sh3 <- plot2.m1.munbase.84ci.sh3 +
  scale_linetype_manual(labels = c("Municipality",
                                   "County",
                                   "National government"),
                        values = c("solid", "dotted", "dashed")) +
  theme_classic() + theme(legend.position = c(0.8, 0.25),
                          legend.title = element_text(size = 14),
                          legend.text = element_text(size = 14),
                          axis.title = element_text(size = 16),
                          axis.text = element_text(size = 12),
                          plot.title = element_text(hjust = .5,
                                                    vjust = 1.5, size = 16)
  ) + ylim(1.25, 2.50)


ggsave("Q1_sh3_right_munbase_84CI.jpeg", plot2.m1.munbase.84ci.sh3, width = 11,
       height = 7)

combined.plots.munbase.sh3 <- ggarrange(plot1.m1.munbase.sh3, plot2.m1.munbase.84ci.sh3,
                                  nrow = 1, ncol = 2)

ggsave(combined.plots.munbase.sh3, filename = 'Figure_E1CDe_E1CDf.jpeg',
         width = 16, height = 8)


##### Figure E1CEa-E1CEb. Decision taken by the county to close upper secondary school: shared rule item 1.####

m1.sh1.coubase <- feols(first_answer ~ sh1 * tier_c | responseid,
                        data = dataset)

summary(m1.sh1.coubase) # See coefficients of the model

preds.sh1low.coubase <- plot_predictions(m1.sh1.coubase,
                                         condition = list(tier_c = list("M", "N"),
                                                          sh1 = 1),
                                         conf_level = 0.84,
                                         gray = T,
                                         draw = F) # Get the estimates & CIs when sh1 low

preds.sh1high.coubase <- plot_predictions(m1.sh1.coubase,
                                          condition = list(tier_c = list("M", "N"),
                                                           sh1 = 4),
                                          conf_level = 0.84,
                                          gray = T,
                                          draw = F) # Get the estimates & CIs when sh1 high

preds.sh1high.coubase.check <- plot_predictions(m1.sh1.coubase,
                                                condition = list(tier_c = list("C"),
                                                                 sh1 = 1),
                                                conf_level = 0.84,
                                                gray = T,
                                                draw = F)

preds.sh1high.coubase.check

plot1.m1.coubase.sh1 <- plot_predictions(m1.sh1.coubase,
                                         condition = list(tier_c = list("M", "N"),
                                                          sh1 = "minmax"),
                                         conf_level = 0.95,
                                         gray = T)

plot1.m1.coubase.sh1 <-  plot1.m1.coubase.sh1 + geom_hline(yintercept = 1.826181, #c.bas
                                                           linetype = 1) +
  labs(x="Decision taken by",
       y="Willingness to accept decision (4-point merged scale)",
       shape="Preference for Shared Rule Item 4",
       title="a: Base category (line) is the decision taken by the county.") +
  scale_x_discrete(labels = c("Municipality",
                              "National government")) + ylim(1.25, 2.50) +
  scale_shape_discrete(labels = c("Min (= 1)", "Max (= 4)"))

plot1.m1.coubase.sh1 <- plot1.m1.coubase.sh1 + geom_linerange(data=preds.sh1low.coubase,
                                                              aes(y = estimate,
                                                                  x = tier_c,
                                                                  ymin = conf.low,
                                                                  ymax = conf.high),
                                                              position = position_nudge(x = -0.037),
                                                              linewidth = 1.2) +
  geom_linerange(data = preds.sh1high.coubase,
                 aes(y = estimate,
                     x = tier_c,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.037),
                 linewidth = 1.2)

plot1.m1.coubase.sh1 <- plot1.m1.coubase.sh1 + theme_classic() +
  theme(legend.position = c(0.75, 0.25),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, vjust = 1.5, size = 16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14))

plot1.m1.coubase.sh1$layers[[1]]$geom_params$fatten = 8

ggsave("Q1_sh1_left_coubase.jpeg", plot1.m1.coubase.sh1, width = 8, height = 6)


###### Line graph with C as baseline ####

# 95% CIs

plot2.m1.coubase.sh1 <- plot_predictions(m1.sh1.coubase,
                                         condition = list("sh1", "tier_c"),
                                         conf_level = 0.95,
                                         gray = T)

plot2.m1.coubase.sh1 <- plot2.m1.coubase.sh1 +
  labs(x = "Preference for Shared Rule Item 4",
       y = "",
       linetype = "Decision taken by",
       title="b: Base category (line) is the decision taken by the county.")

plot2.m1.coubase.sh1 <- plot2.m1.coubase.sh1 +
  scale_linetype_manual(labels = c("County",
                                   "Municipality",
                                   "National government"),
                        values = c("solid", "dotted", "dashed")) +
  theme_classic() + theme(legend.position = c(0.8, 0.25),
                          axis.title = element_text(size = 16),
                          axis.text = element_text(size = 14),
                          plot.title = element_text(hjust = 0.5, vjust = 1.5, size = 16),
                          legend.title = element_text(size = 14),
                          legend.text = element_text(size = 14)
  ) + ylim(1.15, 2.5)


ggsave("Q1_sh1_right_coubase_95CI.jpeg", plot2.m1.coubase.sh1, width = 11, height = 7)

# 84% CIs

plot2.m1.coubase.sh1.84ci <- plot_predictions(m1.sh1.coubase,
                                              condition = list("sh1", "tier_c"),
                                              conf_level = 0.84,
                                              gray = T)

plot2.m1.coubase.sh1.84ci <- plot2.m1.coubase.sh1.84ci +
  labs(x = "Preference for Shared Rule Item 4",
       y = "",
       linetype = "Decision taken by",
       title="b: Base category (line) is the decision taken by the county.")

plot2.m1.coubase.sh1.84ci <- plot2.m1.coubase.sh1.84ci +
  scale_linetype_manual(labels = c("County",
                                   "Municipality",
                                   "National government"),
                        values = c("solid", "dotted", "dashed")) +
  theme_classic() + theme(legend.position = c(0.8, 0.25),
                          axis.title = element_text(size = 16),
                          axis.text = element_text(size = 14),
                          plot.title = element_text(hjust = 0.5, vjust = 1.5, size = 16),
                          legend.title = element_text(size = 14),
                          legend.text = element_text(size = 14)
  ) + ylim(1.25, 2.5)


ggsave("Q1_sh1_right_coubase_84CI.jpeg", plot2.m1.coubase.sh1.84ci, width = 11,
       height = 7)

combined.plots.coubase.sh1 <- ggarrange(plot1.m1.coubase.sh1, plot2.m1.coubase.sh1.84ci,
                                  nrow = 1, ncol = 2)

ggsave(combined.plots.coubase.sh1, filename = 'Figure_E1CEa_E1CEb.jpeg',
         width = 16, height = 8)


##### Figure E1CEc-E1CEd. Decision taken by the county to close upper secondary school: shared rule item 2.####

m1.sh2.coubase <- feols(first_answer ~ sh2 * tier_c | responseid,
                        data = dataset)

summary(m1.sh2.coubase) # See coefficients of the model

preds.sh2low.coubase <- plot_predictions(m1.sh2.coubase,
                                         condition = list(tier_c = list("M", "N"),
                                                          sh2 = 1),
                                         conf_level = 0.84,
                                         gray = T,
                                         draw = F) # Get the estimates & CIs when sh2 low

preds.sh2high.coubase <- plot_predictions(m1.sh2.coubase,
                                          condition = list(tier_c = list("M", "N"),
                                                           sh2 = 4),
                                          conf_level = 0.84,
                                          gray = T,
                                          draw = F) # Get the estimates & CIs when sh2 high

preds.sh2high.coubase.check <- plot_predictions(m1.sh2.coubase,
                                                condition = list(tier_c = list("C"),
                                                                 sh2 = 1),
                                                conf_level = 0.84,
                                                gray = T,
                                                draw = F)

preds.sh2high.coubase.check

plot1.m1.coubase.sh2 <- plot_predictions(m1.sh2.coubase,
                                         condition = list(tier_c = list("M", "N"),
                                                          sh2 = "minmax"),
                                         conf_level = 0.95,
                                         gray = T)

plot1.m1.coubase.sh2 <-  plot1.m1.coubase.sh2 + geom_hline(yintercept = 1.826905, #c.bas
                                                           linetype = 1) +
  labs(x="Decision taken by",
       y="Willingness to accept decision (4-point merged scale)",
       shape="Preference for Shared Rule Item 5",
       title="c: Base category (line) is the decision taken by the county.") +
  scale_x_discrete(labels = c("Municipality",
                              "National government")) + ylim(1.25, 2.50) +
  scale_shape_discrete(labels = c("Min (= 1)", "Max (= 4)"))

plot1.m1.coubase.sh2 <- plot1.m1.coubase.sh2 + geom_linerange(data=preds.sh2low.coubase,
                                                              aes(y = estimate,
                                                                  x = tier_c,
                                                                  ymin = conf.low,
                                                                  ymax = conf.high),
                                                              position = position_nudge(x = -0.037),
                                                              linewidth = 1.2) +
  geom_linerange(data = preds.sh2high.coubase,
                 aes(y = estimate,
                     x = tier_c,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.037),
                 linewidth = 1.2)

plot1.m1.coubase.sh2 <- plot1.m1.coubase.sh2 + theme_classic() +
  theme(legend.position = c(0.75, 0.25),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, vjust = 1.5, size = 16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14))

plot1.m1.coubase.sh2$layers[[1]]$geom_params$fatten = 8

ggsave("Q1_sh2_left_coubase.jpeg", plot1.m1.coubase.sh2, width = 8, height = 6)


###### Line graph with C as baseline #####

# 95% CIs

plot2.m1.coubase.sh2 <- plot_predictions(m1.sh2.coubase,
                                         condition = list("sh2", "tier_c"),
                                         conf_level = 0.95,
                                         gray = T)

plot2.m1.coubase.sh2 <- plot2.m1.coubase.sh2 +
  labs(x = "Preference for Shared Rule Item 5",
       y = "",
       linetype = "Decision taken by",
       title="d: Base category (line) is the decision taken by the county.")

plot2.m1.coubase.sh2 <- plot2.m1.coubase.sh2 +
  scale_linetype_manual(labels = c("County",
                                   "Municipality",
                                   "National government"),
                        values = c("solid", "dotted", "dashed")) +
  theme_classic() + theme(legend.position = c(0.8, 0.25),
                          axis.title = element_text(size = 16),
                          axis.text = element_text(size = 14),
                          plot.title = element_text(hjust = 0.5, vjust = 1.5, size = 16),
                          legend.title = element_text(size = 14),
                          legend.text = element_text(size = 14)
  ) + ylim(1.15, 2.5)


ggsave("Q1_sh2_right_coubase_95CI.jpeg", plot2.m1.coubase.sh2, width = 11, height = 7)

# 84% CIs

plot2.m1.coubase.sh2.84ci <- plot_predictions(m1.sh2.coubase,
                                              condition = list("sh2", "tier_c"),
                                              conf_level = 0.84,
                                              gray = T)

plot2.m1.coubase.sh2.84ci <- plot2.m1.coubase.sh2.84ci +
  labs(x = "Preference for Shared Rule Item 5",
       y = "",
       linetype = "Decision taken by",
       title="d: Base category (line) is the decision taken by the county.")

plot2.m1.coubase.sh2.84ci <- plot2.m1.coubase.sh2.84ci +
  scale_linetype_manual(labels = c("County",
                                   "Municipality",
                                   "National government"),
                        values = c("solid", "dotted", "dashed")) +
  theme_classic() + theme(legend.position = c(0.8, 0.25),
                          axis.title = element_text(size = 16),
                          axis.text = element_text(size = 14),
                          plot.title = element_text(hjust = 0.5, vjust = 1.5, size = 16),
                          legend.title = element_text(size = 14),
                          legend.text = element_text(size = 14)
  ) + ylim(1.25, 2.5)


ggsave("Q1_sh2_right_coubase_84CI.jpeg", plot2.m1.coubase.sh2.84ci, width = 11,
       height = 7)

combined.plots.coubase.sh2 <- ggarrange(plot1.m1.coubase.sh2, plot2.m1.coubase.sh2.84ci,
                                  nrow = 1, ncol = 2)

ggsave(combined.plots.coubase.sh2, filename = 'Figure_E1CEc_E1CEd.jpeg',
         width = 16, height = 8)


##### Figure E1CEe-E1CEf. Decision taken by the county to close upper secondary school: shared rule item 3.####

m1.sh3.coubase <- feols(first_answer ~ sh3 * tier_c | responseid,
                        data = dataset)

summary(m1.sh3.coubase) # See coefficients of the model

preds.sh3low.coubase <- plot_predictions(m1.sh3.coubase,
                                         condition = list(tier_c = list("M", "N"),
                                                          sh3 = 1),
                                         conf_level = 0.84,
                                         gray = T,
                                         draw = F) # Get the estimates & CIs when sh3 low

preds.sh3high.coubase <- plot_predictions(m1.sh3.coubase,
                                          condition = list(tier_c = list("M", "N"),
                                                           sh3 = 4),
                                          conf_level = 0.84,
                                          gray = T,
                                          draw = F) # Get the estimates & CIs when sh3 high

preds.sh3high.coubase.check <- plot_predictions(m1.sh3.coubase,
                                                condition = list(tier_c = list("C"),
                                                                 sh3 = 1),
                                                conf_level = 0.84,
                                                gray = T,
                                                draw = F)

preds.sh3high.coubase.check

plot1.m1.coubase.sh3 <- plot_predictions(m1.sh3.coubase,
                                         condition = list(tier_c = list("M", "N"),
                                                          sh3 = "minmax"),
                                         conf_level = 0.95,
                                         gray = T)

plot1.m1.coubase.sh3 <-  plot1.m1.coubase.sh3 + geom_hline(yintercept = 1.840776, #c.bas
                                                           linetype = 1) +
  labs(x="Decision taken by",
       y="Willingness to accept decision (4-point merged scale)",
       shape="Preference for Shared Rule Item 6",
       title="e: Base category (line) is the decision taken by the county.") +
  scale_x_discrete(labels = c("Municipality",
                              "National government")) + ylim(1.25, 2.50) +
  scale_shape_discrete(labels = c("Min (= 1)", "Max (= 4)"))

plot1.m1.coubase.sh3 <- plot1.m1.coubase.sh3 + geom_linerange(data=preds.sh3low.coubase,
                                                              aes(y = estimate,
                                                                  x = tier_c,
                                                                  ymin = conf.low,
                                                                  ymax = conf.high),
                                                              position = position_nudge(x = -0.037),
                                                              linewidth = 1.2) +
  geom_linerange(data = preds.sh3high.coubase,
                 aes(y = estimate,
                     x = tier_c,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.037),
                 linewidth = 1.2)

plot1.m1.coubase.sh3 <- plot1.m1.coubase.sh3 + theme_classic() +
  theme(legend.position = c(0.75, 0.25),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, vjust = 1.5, size = 16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14)) + annotate('text', x=2,y=2.45,
                                                          label='**', size=8)

plot1.m1.coubase.sh3$layers[[1]]$geom_params$fatten = 8

ggsave("Q1_sh3_left_coubase.jpeg", plot1.m1.coubase.sh3, width = 8, height = 6)


###### Line graph with C as baseline ####

# 95% CIs

plot2.m1.coubase.sh3 <- plot_predictions(m1.sh3.coubase,
                                         condition = list("sh3", "tier_c"),
                                         conf_level = 0.95,
                                         gray = T)

plot2.m1.coubase.sh3 <- plot2.m1.coubase.sh3 +
  labs(x = "Preference for Shared Rule Item 6",
       y = "",
       linetype = "Decision taken by",
       title="f: Base category (line) is the decision taken by the county.")

plot2.m1.coubase.sh3 <- plot2.m1.coubase.sh3 +
  scale_linetype_manual(labels = c("County",
                                   "Municipality",
                                   "National government"),
                        values = c("solid", "dotted", "dashed")) +
  theme_classic() + theme(legend.position = c(0.8, 0.25),
                          axis.title = element_text(size = 16),
                          axis.text = element_text(size = 14),
                          plot.title = element_text(hjust = 0.5, vjust = 1.5, size = 16),
                          legend.title = element_text(size = 14),
                          legend.text = element_text(size = 14)
  ) + ylim(1.15, 2.5)


ggsave("Q1_sh3_right_coubase_95CI.jpeg", plot2.m1.coubase.sh3, width = 11, height = 7)

# 84% CIs

plot2.m1.coubase.sh3.84ci <- plot_predictions(m1.sh3.coubase,
                                              condition = list("sh3", "tier_c"),
                                              conf_level = 0.84,
                                              gray = T)

plot2.m1.coubase.sh3.84ci <- plot2.m1.coubase.sh3.84ci +
  labs(x = "Preference for Shared Rule Item 6",
       y = "",
       linetype = "Decision taken by",
       title="f: Base category (line) is the decision taken by the county.")

plot2.m1.coubase.sh3.84ci <- plot2.m1.coubase.sh3.84ci +
  scale_linetype_manual(labels = c("County",
                                   "Municipality",
                                   "National government"),
                        values = c("solid", "dotted", "dashed")) +
  theme_classic() + theme(legend.position = c(0.8, 0.25),
                          axis.title = element_text(size = 16),
                          axis.text = element_text(size = 14),
                          plot.title = element_text(hjust = 0.5, vjust = 1.5, size = 16),
                          legend.title = element_text(size = 14),
                          legend.text = element_text(size = 14)
  ) + ylim(1.25, 2.5)


ggsave("Q1_sh3_right_coubase_84CI.jpeg", plot2.m1.coubase.sh3.84ci, width = 11,
       height = 7)

# Put the plots together
combined.plots.coubase.sh3 <- ggarrange(plot1.m1.coubase.sh3, plot2.m1.coubase.sh3.84ci,
                                  nrow = 1, ncol = 2)

ggsave(combined.plots.coubase.sh3, filename = 'Figure_E1CEe_E1CEf.jpeg',
         width = 16, height = 8)

#### Figure E1CF. Model 1-Q1-Q3-Q5-base-cat. = N: Preference for shared rule items 4-6.####
##### Figure E1CFa-E1CFb. Decision by the national government to close a department of an university (college): shared rule item 1.####

m1.sh1.natbase <- feols(first_answer ~ sh1 * tier_n | responseid,
                        data = dataset)

summary(m1.sh1.natbase) 

preds.sh1low.natbase <- plot_predictions(m1.sh1.natbase,
                                         condition = list(tier_n = list("M", "C"),
                                                          sh1 = 1),
                                         conf_level = 0.84,
                                         gray = T,
                                         draw = F) # Get the estimates & CIs when sh1 low

preds.sh1high.natbase <- plot_predictions(m1.sh1.natbase,
                                          condition = list(tier_n = list("M", "C"),
                                                           sh1 = 4),
                                          conf_level = 0.84,
                                          gray = T,
                                          draw = F) # Get the estimates & CIs when sh1 high

plot1.m1.natbase.sh1 <- plot_predictions(m1.sh1.natbase,
                                         condition = list(tier_n = list("M", "C"),
                                                          sh1 = "minmax"),
                                         conf_level = 0.95,
                                         gray = T)

plot1.m1.natbase.sh1.check <- plot_predictions(m1.sh1.natbase,
                                               condition = list(tier_n = list("N"),
                                                                sh1 = 1),
                                               conf_level = 0.84,
                                               gray = T,
                                               draw = F)

plot1.m1.natbase.sh1.check

plot1.m1.natbase.sh1 <-  plot1.m1.natbase.sh1 + geom_hline(yintercept = 2.10487 , #n.bas
                                                           linetype = 1) +
  labs(x="Decision taken by",
       y="Willingness to accept decision (4-point merged scale)",
       shape="Preference for Shared Rule Item 4",
       title="a: Base category (line) is the decision taken by the national government.") +
  scale_x_discrete(labels = c("Municipality",
                              "County"))  +
  scale_shape_discrete(labels = c("Min (= 1)", "Max (= 4)"))

plot1.m1.natbase.sh1 <- plot1.m1.natbase.sh1 + geom_linerange(data=preds.sh1low.natbase,
                                                              aes(y = estimate,
                                                                  x = tier_n,
                                                                  ymin = conf.low,
                                                                  ymax = conf.high),
                                                              position = position_nudge(x = -0.037),
                                                              linewidth = 1.2) +
  geom_linerange(data = preds.sh1high.natbase,
                 aes(y = estimate,
                     x = tier_n,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.037),
                 linewidth = 1.2) + ylim(1.25, 2.5)

plot1.m1.natbase.sh1 <- plot1.m1.natbase.sh1 + theme_classic() +
  theme(legend.position = c(0.3, 0.25),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        plot.title = element_text(hjust = 0.5, vjust = 1.5, size = 16)) + annotate('text',
                                                                                   x=1,y=2.35,
                                                                                   label='*',
                                                                                   size=8)

plot1.m1.natbase.sh1$layers[[1]]$geom_params$fatten = 8

ggsave("Q1_sh1_left_natbase.jpeg", plot1.m1.natbase.sh1, width = 8, height = 6)

###### Line graph with N as baseline ####

# 95% CIs

plot2.m1.natbase.sh1 <- plot_predictions(m1.sh1.natbase,
                                         condition = list("sh1", "tier_n"),
                                         conf_level = 0.95,
                                         gray = T)

plot2.m1.natbase.sh1 <- plot2.m1.natbase.sh1 +
  labs(x = "Preference for Shared Rule Item 4",
       y = "Willingness to accept decision (4-point merged scale)",
       linetype = "Decision taken by",
       title="b: Base category (line) is the decision taken by the national government.")

plot2.m1.natbase.sh1 <- plot2.m1.natbase.sh1 +
  scale_linetype_manual(labels = c("National government",
                                   "Municipality",
                                   "County"),
                        values = c("solid", "dashed", "dotted")) +
  theme_classic() + theme(legend.position = c(0.8, 0.2),
                          legend.text = element_text(size=14),
                          legend.title = element_text(size=14),
                          axis.title = element_text(size = 16),
                          axis.text = element_text(size = 14),
                          plot.title = element_text(hjust = .5,
                                                    vjust = 1.5,
                                                    size = 16)
  ) + ylim(1.25, 2.5)


ggsave("Q1_sh1_right_natbase_95CI.jpeg", plot2.m1.natbase.sh1, width = 11, height = 7)

# 84% CIs

plot2.m1.natbase.84ci.sh1 <- plot_predictions(m1.sh1.natbase,
                                              condition = list("sh1", "tier_n"),
                                              conf_level = 0.84,
                                              gray = T)

plot2.m1.natbase.84ci.sh1 <- plot2.m1.natbase.84ci.sh1 +
  labs(x = "Preference for Shared Rule Item 4",
       y = "",
       linetype = "Decision taken by",
       title="b: Base category (line) is the decision taken by the national government.")

plot2.m1.natbase.84ci.sh1 <- plot2.m1.natbase.84ci.sh1 +
  scale_linetype_manual(labels = c("National government",
                                   "Municipality",
                                   "County"),
                        values = c("solid", "dashed", "dotted")) +
  theme_classic() + theme(legend.position = c(0.8, 0.2),
                          legend.title = element_text(size = 14),
                          legend.text = element_text(size = 14),
                          axis.title = element_text(size = 16),
                          axis.text = element_text(size = 12),
                          plot.title = element_text(hjust = .5,
                                                    vjust = 1.5,
                                                    size = 16)
  ) + ylim(1.25, 2.50)


ggsave("Q1_sh1_right_natbase_84CI.jpeg", plot2.m1.natbase.84ci.sh1, width = 11,
       height = 7)


# Put everything in one plot #

combined.plots.natbase.sh1 <- ggarrange(plot1.m1.natbase.sh1, plot2.m1.natbase.84ci.sh1,
                                  nrow = 1, ncol = 2)

ggsave(combined.plots.natbase.sh1, filename = 'Figure_E1CFa_E1CFb.jpeg',
         width = 16, height = 8)

##### Figure E1CFc-E1CFd. Decision by the national government to close a department of an university (college): shared rule item 2.####

m1.sh2.natbase <- feols(first_answer ~ sh2 * tier_n | responseid,
                        data = dataset)

summary(m1.sh2.natbase) 

preds.sh2low.natbase <- plot_predictions(m1.sh2.natbase,
                                         condition = list(tier_n = list("M", "C"),
                                                          sh2 = 1),
                                         conf_level = 0.84,
                                         gray = T,
                                         draw = F) # Get the estimates & CIs when sh2 low

preds.sh2high.natbase <- plot_predictions(m1.sh2.natbase,
                                          condition = list(tier_n = list("M", "C"),
                                                           sh2 = 4),
                                          conf_level = 0.84,
                                          gray = T,
                                          draw = F) # Get the estimates & CIs when sh2 high

plot1.m1.natbase.sh2 <- plot_predictions(m1.sh2.natbase,
                                         condition = list(tier_n = list("M", "C"),
                                                          sh2 = "minmax"),
                                         conf_level = 0.95,
                                         gray = T)

plot1.m1.natbase.sh2.check <- plot_predictions(m1.sh2.natbase,
                                               condition = list(tier_n = list("N"),
                                                                sh2 = 1),
                                               conf_level = 0.84,
                                               gray = T,
                                               draw = F)

plot1.m1.natbase.sh2.check

plot1.m1.natbase.sh2 <-  plot1.m1.natbase.sh2 + geom_hline(yintercept = 2.105949 , #n.bas
                                                           linetype = 1) +
  labs(x="Decision taken by",
       y="Willingness to accept decision (4-point merged scale)",
       shape="Preference for Shared Rule Item 5",
       title="c: Base category (line) is the decision taken by the national government.") +
  scale_x_discrete(labels = c("Municipality",
                              "County"))  +
  scale_shape_discrete(labels = c("Min (= 1)", "Max (= 4)"))

plot1.m1.natbase.sh2 <- plot1.m1.natbase.sh2 + geom_linerange(data=preds.sh2low.natbase,
                                                              aes(y = estimate,
                                                                  x = tier_n,
                                                                  ymin = conf.low,
                                                                  ymax = conf.high),
                                                              position = position_nudge(x = -0.037),
                                                              linewidth = 1.2) +
  geom_linerange(data = preds.sh2high.natbase,
                 aes(y = estimate,
                     x = tier_n,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.037),
                 linewidth = 1.2) + ylim(1.25, 2.5)

plot1.m1.natbase.sh2 <- plot1.m1.natbase.sh2 + theme_classic() +
  theme(legend.position = c(0.3, 0.25),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        plot.title = element_text(hjust = 0.5, vjust = 1.5, size = 16))

plot1.m1.natbase.sh2$layers[[1]]$geom_params$fatten = 8

ggsave("Q1_sh2_left_natbase.jpeg", plot1.m1.natbase.sh2, width = 8, height = 6)

###### Line graph with N as baseline ####

# 95% CIs

plot2.m1.natbase.sh2 <- plot_predictions(m1.sh2.natbase,
                                         condition = list("sh2", "tier_n"),
                                         conf_level = 0.95,
                                         gray = T)

plot2.m1.natbase.sh2 <- plot2.m1.natbase.sh2 +
  labs(x = "Preference for Shared Rule Item 5",
       y = "Willingness to accept decision (4-point merged scale)",
       linetype = "Decision taken by",
       title="d: Base category (line) is the decision taken by the national government.")

plot2.m1.natbase.sh2 <- plot2.m1.natbase.sh2 +
  scale_linetype_manual(labels = c("National government",
                                   "Municipality",
                                   "County"),
                        values = c("solid", "dashed", "dotted")) +
  theme_classic() + theme(legend.position = c(0.8, 0.2),
                          legend.text = element_text(size=14),
                          legend.title = element_text(size=14),
                          axis.title = element_text(size = 16),
                          axis.text = element_text(size = 14),
                          plot.title = element_text(hjust = .5,
                                                    vjust = 1.5,
                                                    size = 16)
  ) + ylim(1.25, 2.5)


ggsave("Q1_sh2_right_natbase_95CI.jpeg", plot2.m1.natbase.sh2, width = 11, height = 7)

# 84% CIs

plot2.m1.natbase.84ci.sh2 <- plot_predictions(m1.sh2.natbase,
                                              condition = list("sh2", "tier_n"),
                                              conf_level = 0.84,
                                              gray = T)

plot2.m1.natbase.84ci.sh2 <- plot2.m1.natbase.84ci.sh2 +
  labs(x = "Preference for Shared Rule Item 5",
       y = "",
       linetype = "Decision taken by",
       title="d: Base category (line) is the decision taken by the national government.")

plot2.m1.natbase.84ci.sh2 <- plot2.m1.natbase.84ci.sh2 +
  scale_linetype_manual(labels = c("National government",
                                   "Municipality",
                                   "County"),
                        values = c("solid", "dashed", "dotted")) +
  theme_classic() + theme(legend.position = c(0.8, 0.2),
                          legend.title = element_text(size = 14),
                          legend.text = element_text(size = 14),
                          axis.title = element_text(size = 16),
                          axis.text = element_text(size = 12),
                          plot.title = element_text(hjust = .5,
                                                    vjust = 1.5,
                                                    size = 16)
  ) + ylim(1.25, 2.50)


ggsave("Q1_sh2_right_natbase_84CI.jpeg", plot2.m1.natbase.84ci.sh2, width = 11,
       height = 7)

# Put everything in one plot #

combined.plots.natbase.sh2 <- ggarrange(plot1.m1.natbase.sh2, plot2.m1.natbase.84ci.sh2,
                                  nrow = 1, ncol = 2)

ggsave(combined.plots.natbase.sh2, filename = 'Figure_E1CFc_E1CFd.jpeg',
         width = 16, height = 8)


##### Figure E1CFe-E1CFf. Decision by the national government to close a department of an university (college): shared rule item 3.####

m1.sh3.natbase <- feols(first_answer ~ sh3 * tier_n | responseid,
                        data = dataset)

summary(m1.sh3.natbase) 

preds.sh3low.natbase <- plot_predictions(m1.sh3.natbase,
                                         condition = list(tier_n = list("M", "C"),
                                                          sh3 = 1),
                                         conf_level = 0.84,
                                         gray = T,
                                         draw = F) # Get the estimates & CIs when sh3 low

preds.sh3high.natbase <- plot_predictions(m1.sh3.natbase,
                                          condition = list(tier_n = list("M", "C"),
                                                           sh3 = 4),
                                          conf_level = 0.84,
                                          gray = T,
                                          draw = F) # Get the estimates & CIs when sh3 high

plot1.m1.natbase.sh3 <- plot_predictions(m1.sh3.natbase,
                                         condition = list(tier_n = list("M", "C"),
                                                          sh3 = "minmax"),
                                         conf_level = 0.95,
                                         gray = T)

plot1.m1.natbase.sh3.check <- plot_predictions(m1.sh3.natbase,
                                               condition = list(tier_n = list("N"),
                                                                sh3 = 1),
                                               conf_level = 0.84,
                                               gray = T,
                                               draw = F)

plot1.m1.natbase.sh3.check

plot1.m1.natbase.sh3 <-  plot1.m1.natbase.sh3 + geom_hline(yintercept = 2.074223 , #n.bas
                                                           linetype = 1) +
  labs(x="Decision taken by",
       y="Willingness to accept decision (4-point merged scale)",
       shape="Preference for Shared Rule Item 6",
       title="e: Base category (line) is the decision taken by the national government.") +
  scale_x_discrete(labels = c("Municipality",
                              "County"))  +
  scale_shape_discrete(labels = c("Min (= 1)", "Max (= 4)"))

plot1.m1.natbase.sh3 <- plot1.m1.natbase.sh3 + geom_linerange(data=preds.sh3low.natbase,
                                                              aes(y = estimate,
                                                                  x = tier_n,
                                                                  ymin = conf.low,
                                                                  ymax = conf.high),
                                                              position = position_nudge(x = -0.037),
                                                              linewidth = 1.2) +
  geom_linerange(data = preds.sh3high.natbase,
                 aes(y = estimate,
                     x = tier_n,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.037),
                 linewidth = 1.2) + ylim(1.25, 2.5)

plot1.m1.natbase.sh3 <- plot1.m1.natbase.sh3 + theme_classic() +
  theme(legend.position = c(0.3, 0.25),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        plot.title = element_text(hjust = 0.5, vjust = 1.5, size = 16)) + annotate('text', x=c(1,2),
                                                                                   y=c(2.2,1.95),
                                                                                   label=c('*','**'),
                                                                                   size=8)

plot1.m1.natbase.sh3$layers[[1]]$geom_params$fatten = 8

ggsave("Q1_sh3_left_natbase.jpeg", plot1.m1.natbase.sh3, width = 8, height = 6)

###### Line graph with N as baseline ####

# 95% CIs

plot2.m1.natbase.sh3 <- plot_predictions(m1.sh3.natbase,
                                         condition = list("sh3", "tier_n"),
                                         conf_level = 0.95,
                                         gray = T)

plot2.m1.natbase.sh3 <- plot2.m1.natbase.sh3 +
  labs(x = "Preference for Shared Rule Item 6",
       y = "Willingness to accept decision (4-point merged scale)",
       linetype = "Decision taken by",
       title="f: Base category (line) is the decision taken by the national government.")

plot2.m1.natbase.sh3 <- plot2.m1.natbase.sh3 +
  scale_linetype_manual(labels = c("National government",
                                   "Municipality",
                                   "County"),
                        values = c("solid", "dashed", "dotted")) +
  theme_classic() + theme(legend.position = c(0.8, 0.2),
                          legend.text = element_text(size=14),
                          legend.title = element_text(size=14),
                          axis.title = element_text(size = 16),
                          axis.text = element_text(size = 14),
                          plot.title = element_text(hjust = .5,
                                                    vjust = 1.5,
                                                    size = 16)
  ) + ylim(1.25, 2.5)


ggsave("Q1_sh3_right_natbase_95CI.jpeg", plot2.m1.natbase.sh3, width = 11, height = 7)

# 84% CIs

plot2.m1.natbase.84ci.sh3 <- plot_predictions(m1.sh3.natbase,
                                              condition = list("sh3", "tier_n"),
                                              conf_level = 0.84,
                                              gray = T)

plot2.m1.natbase.84ci.sh3 <- plot2.m1.natbase.84ci.sh3 +
  labs(x = "Preference for Shared Rule Item 6",
       y = "",
       linetype = "Decision taken by",
       title="f: Base category (line) is the decision taken by the national government.")

plot2.m1.natbase.84ci.sh3 <- plot2.m1.natbase.84ci.sh3 +
  scale_linetype_manual(labels = c("National government",
                                   "Municipality",
                                   "County"),
                        values = c("solid", "dashed", "dotted")) +
  theme_classic() + theme(legend.position = c(0.8, 0.2),
                          legend.title = element_text(size = 14),
                          legend.text = element_text(size = 14),
                          axis.title = element_text(size = 16),
                          axis.text = element_text(size = 12),
                          plot.title = element_text(hjust = .5,
                                                    vjust = 1.5,
                                                    size = 16)
  ) + ylim(1.25, 2.50)


ggsave("Q1_sh3_right_natbase_84CI.jpeg", plot2.m1.natbase.84ci.sh3, width = 11,
       height = 7)


# Put everything in one plot

combined.plots.natbase.sh3 <- ggarrange(plot1.m1.natbase.sh3, plot2.m1.natbase.84ci.sh3,
                                  nrow = 1, ncol = 2)

ggsave(combined.plots.natbase.sh3, filename = 'Figure_E1CFe_E1CFf.jpeg',
         width = 16, height = 8)


#### Tables E2a-E2c ####
##### Table E2a. Model 2-Q2-Q4-Q6-base category = municipality: Preference for each item.####

# Self-Rule Item 1:
mod.sf1.mun.support <- feols(second_answer ~ first_answer_factor * sf1 * support_m + tier_m | responseid,
                             data = dataset)
summary(mod.sf1.mun.support)

# Self-Rule Item 2:
mod.sf2.mun.support <- feols(second_answer ~ first_answer_factor * sf2 * support_m + tier_m | responseid,
                             data = dataset)
summary(mod.sf2.mun.support)

#Self-rule Item 3:
mod.sf3.mun.support <- feols(second_answer ~ first_answer_factor * sf3 * support_m + tier_m | responseid,
                             data = dataset)
summary(mod.sf3.mun.support)

#Shared Rule Item 4:
mod.sh1.mun.support <- feols(second_answer ~ first_answer_factor * sh1 * support_m + tier_m | responseid,
                             data = dataset)
summary(mod.sh1.mun.support)

#Shared Rule Item 5:
mod.sh2.mun.support <- feols(second_answer ~ first_answer_factor * sh2 * support_m + tier_m | responseid,
                             data = dataset)
summary(mod.sh2.mun.support)

#Shared Rule Item 6:
mod.sh3.mun.support <- feols(second_answer ~ first_answer_factor * sh3 * support_m + tier_m | responseid,
                             data = dataset)
summary(mod.sh3.mun.support)



##### Table E2b. Model 2-Q2-Q4-Q6-base category = county government: Preference for each item.####

# Self-Rule Item 1:
m2.supportC.sf1 <- feols(second_answer ~ first_answer_factor * sf1 * Support_c + tier_c | responseid,
                         data = dataset)
summary(m2.supportC.sf1)

# Self-Rule Item 2:
m2.supportC.sf2 <- feols(second_answer ~ first_answer_factor * sf2 * Support_c + tier_c | responseid,
                         data = dataset)
summary(m2.supportC.sf2)

#Self-rule Item 3:
m2.supportC.sf3 <- feols(second_answer ~ first_answer_factor * sf3 * Support_c + tier_c | responseid,
                         data = dataset)
summary(m2.supportC.sf3)

#Shared Rule Item 4:
m2.supportC.sh1 <- feols(second_answer ~ first_answer_factor * sh1 * Support_c + tier_c | responseid,
                         data = dataset)
summary(m2.supportC.sh1)

#Shared Rule Item 5:
m2.supportC.sh2 <- feols(second_answer ~ first_answer_factor * sh2 * Support_c + tier_c | responseid,
                         data = dataset)
summary(m2.supportC.sh2)

#Shared Rule Item 6:
m2.supportC.sh3 <- feols(second_answer ~ first_answer_factor * sh3 * Support_c + tier_c | responseid,
                         data = dataset)
summary(m2.supportC.sh3)


##### Table E2c. Model 2-Q2-Q4-Q6-base category = national government: Preference for each item.####

# Self-Rule Item 1:
m2.supportN.sf1 <- feols(second_answer ~ first_answer_factor * sf1 * Support + tier_n | responseid,
                         data = dataset)
summary(m2.supportN.sf1)

# Self-Rule Item 2:
m2.supportN.sf2 <- feols(second_answer ~ first_answer_factor * sf2 * Support + tier_n | responseid,
                         data = dataset)
summary(m2.supportN.sf2)

#Self-rule Item 3:
m2.supportN.sf3 <- feols(second_answer ~ first_answer_factor * sf3 * Support + tier_n | responseid,
                         data = dataset)
summary(m2.supportN.sf3)

#Shared Rule Item 4:
m2.supportN.sh1 <- feols(second_answer ~ first_answer_factor * sh1 * Support + tier_n | responseid,
                         data = dataset)
summary(m2.supportN.sh1)

#Shared Rule Item 5:
m2.supportN.sh2 <- feols(second_answer ~ first_answer_factor * sh2 * Support + tier_n | responseid,
                         data = dataset)
summary(m2.supportN.sh2)

#Shared Rule Item 6:
m2.supportN.sh3 <- feols(second_answer ~ first_answer_factor * sh3 * Support + tier_n | responseid,
                         data = dataset)
summary(m2.supportN.sh3)

####Figure E2A. Model 2-Q2-Q4-Q6: Preference for self-rule item 1.####
#####Figures E2Aa-E2Ac: Municipality decides to close kindergarten.####

# IMPORTANT NOTE: The point estimates are statistically significantly different
# from each other (p<0.05) when their 84% CIs do not overlap and the 95% CI of
# the DIFFERENCE between two point estimates does not include zero; see data and methods

mod.sf1.mun.support <- feols(second_answer ~ first_answer_factor * sf1 * support_m + tier_m | responseid,
                             data = dataset)
summary(mod.sf1.mun.support)

###### Figure E2Aa: Municipality decides to close kindergarten and county supports####

preds.railow.mundec.sf1 <- plot_predictions(mod.sf1.mun.support,
                                            newdata = datagrid(first_answer_factor = 1:4,
                                                               sf1 = 2.5,
                                                               support_m = c("Respondent's County Supports"),
                                                               tier_m = c("M")),
                                            by = c("support_m", "sf1", "first_answer_factor"),
                                            gray = T,
                                            conf_level = 0.84,
                                            draw = F) # Get the estimates & CIs when RAI low

preds.high.mundec.sf1 <- plot_predictions(mod.sf1.mun.support,
                                          newdata = datagrid(first_answer_factor = 1:4,
                                                             sf1 = 3.93,
                                                             support_m = c("Respondent's County Supports"),
                                                             tier_m = c("M")),
                                          by = c("support_m", "sf1", "first_answer_factor"),
                                          gray = T,
                                          conf_level = 0.84,
                                          draw = F) # Get the estimates & CIs when RAI high

plot2.supportM_cou.sf1 <- plot_predictions(mod.sf1.mun.support,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sf1 = c(2.5, 3.93),
                                                              support_m = c("Respondent's County Supports"),
                                                              tier_m = c("M")),
                                           by = c("support_m", "sf1", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.95,
                                           draw = F)


p2_m_c_sf1 <- ggplot(plot2.supportM_cou.sf1,
                     aes(x = first_answer_factor,
                         y = estimate,
                         group = first_answer_factor, shape = sf1)) +
  geom_hline(yintercept = c(1, 2, 3, 4), color='gray') + 
  facet_grid(.~support_m) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = sf1),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_M_c_sf1 <- p2_m_c_sf1 +
  labs(x="",
       y="",
       shape="Preference for Self-Rule Item 1") +
  scale_shape_discrete(labels= c("Mean - 1SD (2.5)", "Mean + 1SD (3.93)")) +
  theme_classic()  + theme(legend.position = "bottom",
                           plot.title = element_text(hjust = 0.5, vjust = 1.5))


p2_M_c_sf1 <- p2_M_c_sf1 + geom_linerange(data=preds.railow.mundec.sf1,
                                          aes(y = estimate,
                                              x = first_answer_factor,
                                              ymin = conf.low,
                                              ymax = conf.high),
                                          position = position_nudge(x = -0.05),
                                          linewidth = 1.2) +
  geom_linerange(data = preds.high.mundec.sf1,
                 aes(y = estimate,
                     x = first_answer_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)


my.plot.mun.1.sf1 <- p2_M_c_sf1  + labs(tag = "a:") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        axis.title.y = element_text(size=14),
        strip.text.x = element_text(size = 11),
        plot.tag.position = c(0.16, 0.974)) +
  annotate("text", x=3, y=3.3,
                             label=c("*"),
                                  size=8)
my.plot.mun.1.sf1

###### Figure E2Ab: Municipality decides to close kindergarten and national government supports ####

preds.low.mundec.2.sf1 <- plot_predictions(mod.sf1.mun.support,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sf1 = 2.5,
                                                              support_m = c("National Government Supports"),
                                                              tier_m = c("M")),
                                           by = c("support_m", "sf1", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.84,
                                           draw = F) # Get the estimates & CIs when RAI low

preds.high.mundec.2.sf1 <- plot_predictions(mod.sf1.mun.support,
                                            newdata = datagrid(first_answer_factor = 1:4,
                                                               sf1 = 3.93,
                                                               support_m = c("National Government Supports"),
                                                               tier_m = c("M")),
                                            by = c("support_m", "sf1", "first_answer_factor"),
                                            gray = T,
                                            conf_level = 0.84,
                                            draw = F) # Get the estimates & CIs when RAI high

plot2.supportM_nat.sf1 <- plot_predictions(mod.sf1.mun.support,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sf1 = c(2.5, 3.93),
                                                              support_m = c("National Government Supports"),
                                                              tier_m = c("M")),
                                           by = c("support_m", "sf1", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.95,
                                           draw = F)


p2_m_n_sf1 <- ggplot(plot2.supportM_nat.sf1,
                     aes(x = first_answer_factor,
                         y = estimate,
                         group = first_answer_factor, shape = sf1)) +
  geom_hline(yintercept = c(1, 2, 3, 4), color='gray') + 
  facet_grid(.~support_m) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = sf1),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_M_n_sf1 <- p2_m_n_sf1 +
  labs(x="",
       y="",
       shape="Preference for Self-Rule Item 1") +
  scale_shape_discrete(labels= c("Mean - 1SD (2.5)", "Mean + 1SD (3.93)")) +
  theme_classic() + theme(legend.position = "bottom",
                          plot.title = element_text(hjust = 0.5, vjust = 1.5))


p2_M_n_sf1 <- p2_M_n_sf1 + geom_linerange(data=preds.low.mundec.2.sf1,
                                          aes(y = estimate,
                                              x = first_answer_factor,
                                              ymin = conf.low,
                                              ymax = conf.high),
                                          position = position_nudge(x = -0.05),
                                          linewidth = 1.2) +
  geom_linerange(data = preds.high.mundec.2.sf1,
                 aes(y = estimate,
                     x = first_answer_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)



my.plot.mun.2.sf1 <- p2_M_n_sf1  + labs(tag = "b:") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        strip.text.x = element_text(size = 11),
        plot.tag.position = c(0.16, 0.974)) + annotate("text", x=1, y=2.05,
                                                           label="*", size=8)

my.plot.mun.2.sf1

###### Figure E2Ac: Municipality decides to close kindergarten and county and national government support ####

preds.low.mundec.3.sf1 <- plot_predictions(mod.sf1.mun.support,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sf1 = 2.5,
                                                              support_m = c("County & National Government Support"),
                                                              tier_m = c("M")),
                                           by = c("support_m", "sf1", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.84,
                                           draw = F) # Get the estimates & CIs when sf1 low

preds.high.mundec.3.sf1 <- plot_predictions(mod.sf1.mun.support,
                                            newdata = datagrid(first_answer_factor = 1:4,
                                                               sf1 = 3.93,
                                                               support_m = c("County & National Government Support"),
                                                               tier_m = c("M")),
                                            by = c("support_m", "sf1", "first_answer_factor"),
                                            gray = T,
                                            conf_level = 0.84,
                                            draw = F) # Get the estimates & CIs when RAI high

plot2.supportM_counat.sf1 <- plot_predictions(mod.sf1.mun.support,
                                              newdata = datagrid(first_answer_factor = 1:4,
                                                                 sf1 = c(2.5, 3.93),
                                                                 support_m = c("County & National Government Support"),
                                                                 tier_m = c("M")),
                                              by = c("support_m", "sf1", "first_answer_factor"),
                                              gray = T,
                                              conf_level = 0.95,
                                              draw = F)


p2_m_cn_sf1 <- ggplot(plot2.supportM_counat.sf1,
                      aes(x = first_answer_factor,
                          y = estimate,
                          group = first_answer_factor, shape = sf1)) +
  geom_hline(yintercept = c(1, 2, 3, 4), color='gray') + 
  facet_grid(.~support_m) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = sf1),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_M_cn_sf1 <- p2_m_cn_sf1 +
  labs(x="", y="",
       shape="Preference for Self-Rule Item 1") +
  scale_shape_discrete(labels= c("Mean - 1SD (2.5)", "Mean + 1SD (3.93)")) +
  theme_classic() + theme(legend.position = "bottom",
                          plot.title = element_text(hjust = 0.5, vjust = 1.5))


p2_M_cn_sf1 <- p2_M_cn_sf1 + geom_linerange(data=preds.low.mundec.3.sf1,
                                            aes(y = estimate,
                                                x = first_answer_factor,
                                                ymin = conf.low,
                                                ymax = conf.high),
                                            position = position_nudge(x = -0.05),
                                            linewidth = 1.2) +
  geom_linerange(data = preds.high.mundec.3.sf1,
                 aes(y = estimate,
                     x = first_answer_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)



my.plot.mun.3.sf1 <- p2_M_cn_sf1  + labs(tag = "c:") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        strip.text.x = element_text(size = 11),
        plot.tag.position = c(0.16, 0.974))

my.plot.mun.3.sf1

p1.sf1 <- ggarrange(my.plot.mun.1.sf1, my.plot.mun.2.sf1, my.plot.mun.3.sf1, nrow = 1, ncol = 3,
                    common.legend = T, legend = "none") + plot_annotation(title="Municipality decides to close kindergarten") &
  theme(plot.title = element_text(hjust = .5, size = 20),
        legend.text = element_blank())


##### Figures E2Ad-E2Af: County decides to close upper secondary school. ####
m2.supportC.sf1 <- feols(second_answer ~ first_answer_factor * sf1 * Support_c + tier_c | responseid,
                         data = dataset)
summary(m2.supportC.sf1)

###### Figure E2Ad: County decides to close upper secondary school and municipality supports #####
preds.railow.coudec.sf1 <- plot_predictions(m2.supportC.sf1,
                                            newdata = datagrid(first_answer_factor = 1:4,
                                                               sf1 = 2.5,
                                                               Support_c = c("Respondent's Municipality Supports"),
                                                               tier_c = c("C")),
                                            by = c("Support_c", "sf1", "first_answer_factor"),
                                            gray = T,
                                            conf_level = 0.84,
                                            draw = F) # Get the estimates & CIs when RAI low

preds.high.coudec.sf1 <- plot_predictions(m2.supportC.sf1,
                                          newdata = datagrid(first_answer_factor = 1:4,
                                                             sf1 = 3.93,
                                                             Support_c = c("Respondent's Municipality Supports"),
                                                             tier_c = c("C")),
                                          by = c("Support_c", "sf1", "first_answer_factor"),
                                          gray = T,
                                          conf_level = 0.84,
                                          draw = F) # Get the estimates & CIs when RAI high

plot2.supportC_mun.sf1 <- plot_predictions(m2.supportC.sf1,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sf1 = c(2.5, 3.93),
                                                              Support_c = c("Respondent's Municipality Supports"),
                                                              tier_c = c("C")),
                                           by = c("Support_c", "sf1", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.95,
                                           draw = F)


p2_c_m_sf1 <- ggplot(plot2.supportC_mun.sf1,
                     aes(x = first_answer_factor,
                         y = estimate,
                         group = first_answer_factor, shape = sf1)) +
  geom_hline(yintercept = c(1, 2, 3, 4), color='gray') + 
  facet_grid(.~Support_c) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = sf1),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_C_m_sf1 <- p2_c_m_sf1 +
  labs(x="",
       y="Willingness to accept decision\nwhen support is provided by another tier",
       shape="Preference for Self-Rule Item 1") +
  scale_shape_discrete(labels= c("Mean - 1SD (2.5)", "Mean + 1SD (3.93)")) +
  theme_classic()  + theme(legend.position = "bottom",
                           plot.title = element_text(hjust = 0.5, vjust = 1.5),
                           axis.title.y = element_text(size = 18, vjust = 4.5)) 



p2_C_m_sf1 <- p2_C_m_sf1 + geom_linerange(data=preds.railow.coudec.sf1,
                                          aes(y = estimate,
                                              x = first_answer_factor,
                                              ymin = conf.low,
                                              ymax = conf.high),
                                          position = position_nudge(x = -0.05),
                                          linewidth = 1.2) +
  geom_linerange(data = preds.high.coudec.sf1,
                 aes(y = estimate,
                     x = first_answer_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)


 
my.plot.cou.1.sf1 <- p2_C_m_sf1 + labs(tag = "d:") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        axis.title.y = element_text(size=14),
        strip.text.x = element_text(size = 11),
        plot.tag.position = c(0.19, 0.974)) + annotate("text", x=c(2,3,4),
                                                           y=c(2.8,3.5,3.94),
                                                           label=c("*", "**", "*"),
                                                           size=8)

my.plot.cou.1.sf1

###### Figure E2Ae: County decides to close upper secondary school and national government supports ####

preds.low.coudec.2.sf1 <- plot_predictions(m2.supportC.sf1,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sf1 = 2.5,
                                                              Support_c = c("National Government Supports"),
                                                              tier_c = c("C")),
                                           by = c("Support_c", "sf1", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.84,
                                           draw = F) # Get the estimates & CIs when RAI low

preds.high.coudec.2.sf1 <- plot_predictions(m2.supportC.sf1,
                                            newdata = datagrid(first_answer_factor = 1:4,
                                                               sf1 = 3.93,
                                                               Support_c = c("National Government Supports"),
                                                               tier_c = c("C")),
                                            by = c("Support_c", "sf1", "first_answer_factor"),
                                            gray = T,
                                            conf_level = 0.84,
                                            draw = F) # Get the estimates & CIs when RAI high

plot2.supportC_nat.sf1 <- plot_predictions(m2.supportC.sf1,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sf1 = c(2.5, 3.93),
                                                              Support_c = c("National Government Supports"),
                                                              tier_c = c("C")),
                                           by = c("Support_c", "sf1", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.95,
                                           draw = F)


p2_c_n_sf1 <- ggplot(plot2.supportC_nat.sf1,
                     aes(x = first_answer_factor,
                         y = estimate,
                         group = first_answer_factor, shape = sf1)) +
  geom_hline(yintercept = c(1, 2, 3, 4), color='gray') + 
  facet_grid(.~Support_c) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = sf1),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_C_n_sf1 <- p2_c_n_sf1 +
  labs(x="",
       y="",
       shape="Preference for Self-Rule Item 1") +
  scale_shape_discrete(labels= c("Mean - 1SD (2.5)", "Mean + 1SD (3.93)")) +
  theme_classic() + theme(legend.position = "bottom",
                          plot.title = element_text(hjust = 0.5, vjust = 1.5))


p2_C_n_sf1 <- p2_C_n_sf1 + geom_linerange(data=preds.low.coudec.2.sf1,
                                          aes(y = estimate,
                                              x = first_answer_factor,
                                              ymin = conf.low,
                                              ymax = conf.high),
                                          position = position_nudge(x = -0.05),
                                          linewidth = 1.2) +
  geom_linerange(data = preds.high.coudec.2.sf1,
                 aes(y = estimate,
                     x = first_answer_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)

my.plot.cou.2.sf1 <- p2_C_n_sf1  + labs(tag = "e:") +
  theme(axis.text = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 16),
        strip.text.x = element_text(size = 11),
        plot.tag.position = c(0.16, 0.974))

###### Figure E2Af: County decides to close upper secondary school and municipality and national government support ####

preds.low.coudec.3.sf1 <- plot_predictions(m2.supportC.sf1,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sf1 = 2.5,
                                                              Support_c = c("Municipality & National Government Support"),
                                                              tier_c = c("C")),
                                           by = c("Support_c", "sf1", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.84,
                                           draw = F) # Get the estimates & CIs when RAI low

preds.high.coudec.3.sf1 <- plot_predictions(m2.supportC.sf1,
                                            newdata = datagrid(first_answer_factor = 1:4,
                                                               sf1 = 3.93,
                                                               Support_c = c("Municipality & National Government Support"),
                                                               tier_c = c("C")),
                                            by = c("Support_c", "sf1", "first_answer_factor"),
                                            gray = T,
                                            conf_level = 0.84,
                                            draw = F) # Get the estimates & CIs when RAI high

plot2.supportC_munnat.sf1 <- plot_predictions(m2.supportC.sf1,
                                              newdata = datagrid(first_answer_factor = 1:4,
                                                                 sf1 = c(2.5, 3.93),
                                                                 Support_c = c("Municipality & National Government Support"),
                                                                 tier_c = c("C")),
                                              by = c("Support_c", "sf1", "first_answer_factor"),
                                              gray = T,
                                              conf_level = 0.95,
                                              draw = F)


p2_c_mn_sf1 <- ggplot(plot2.supportC_munnat.sf1,
                      aes(x = first_answer_factor,
                          y = estimate,
                          group = first_answer_factor, shape = sf1)) +
  geom_hline(yintercept = c(1, 2, 3, 4), color='gray') + 
  facet_grid(.~Support_c) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = sf1),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_C_mn_sf1 <- p2_c_mn_sf1 +
  labs(x="", y="",
       shape="Preference for Self-Rule Item 1") +
  scale_shape_discrete(labels= c("Mean - 1SD (2.5)", "Mean + 1SD (3.93)")) +
  theme_classic() + theme(legend.position = "bottom",
                          plot.title = element_text(hjust = 0.5, vjust = 1.5))


p2_C_mn_sf1 <- p2_C_mn_sf1 + geom_linerange(data=preds.low.coudec.3.sf1,
                                            aes(y = estimate,
                                                x = first_answer_factor,
                                                ymin = conf.low,
                                                ymax = conf.high),
                                            position = position_nudge(x = -0.05),
                                            linewidth = 1.2) +
  geom_linerange(data = preds.high.coudec.3.sf1,
                 aes(y = estimate,
                     x = first_answer_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)

my.plot.cou.3.sf1 <- p2_C_mn_sf1  + labs(tag = "f:") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        strip.text.x = element_text(size = 11),
        plot.tag.position = c(0.134, 0.973)) + annotate("text", x=3, y=3.4,
                                                           label=c("*"), size=8)

p2.sf1 <- ggarrange(my.plot.cou.1.sf1, my.plot.cou.2.sf1, my.plot.cou.3.sf1, nrow = 1, ncol = 3,
                    common.legend = T, legend = "none") 

p2.sf1 <- p2.sf1 + plot_annotation(title = "\nCounty decides to close upper secondary school") &
  theme(plot.title = element_text(hjust = .5, size = 18))


##### Figures E2Ag-E2Ai: National government decides to close a department of a university (college) #####
m2.supportN.sf1 <- feols(second_answer ~ first_answer_factor * sf1 * Support + tier_n | responseid,
                         data = dataset)
summary(m2.supportN.sf1)

###### Figure E2Ag: National government decides to close a department of a university (college) and municipality supports #####
preds.railow.natdec.sf1 <- plot_predictions(m2.supportN.sf1,
                                            newdata = datagrid(first_answer_factor = 1:4,
                                                               sf1 = 2.5,
                                                               Support = c("Respondent's Municipality Supports"),
                                                               tier_n = c("N")),
                                            by = c("Support", "sf1", "first_answer_factor"),
                                            gray = T,
                                            conf_level = 0.84,
                                            draw = F) # Get the estimates & CIs when RAI low

preds.high.natdec.sf1 <- plot_predictions(m2.supportN.sf1,
                                          newdata = datagrid(first_answer_factor = 1:4,
                                                             sf1 = 3.93,
                                                             Support = c("Respondent's Municipality Supports"),
                                                             tier_n = c("N")),
                                          by = c("Support", "sf1", "first_answer_factor"),
                                          gray = T,
                                          conf_level = 0.84,
                                          draw = F) # Get the estimates & CIs when RAI high

plot2.supportN_mun.sf1 <- plot_predictions(m2.supportN.sf1,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sf1 = c(2.5, 3.93),
                                                              Support = c("Respondent's Municipality Supports"),
                                                              tier_n = c("N")),
                                           by = c("Support", "sf1", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.95,
                                           draw = F)

p2_n_m_sf1 <- ggplot(plot2.supportN_mun.sf1,
                     aes(x = first_answer_factor,
                         y = estimate,
                         group = first_answer_factor, shape = sf1)) +
  geom_hline(yintercept = c(1, 2, 3, 4), color='gray') + 
  facet_grid(.~Support) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = sf1),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_N_m_sf1 <- p2_n_m_sf1 +
  labs(x="",
       y="",
       shape="Preference for Self-Rule Item 1") +
  scale_shape_discrete(labels= c("Mean - 1SD (2.5)", "Mean + 1SD (3.93)")) +
  theme_classic()  + theme(legend.position = "bottom",
                           plot.title = element_text(hjust = 0.5, vjust = 1.5),
                           axis.title.y = element_text(size = 18, vjust = 4.5)) 


p2_N_m_sf1 <- p2_N_m_sf1 + geom_linerange(data=preds.railow.natdec.sf1,
                                          aes(y = estimate,
                                              x = first_answer_factor,
                                              ymin = conf.low,
                                              ymax = conf.high),
                                          position = position_nudge(x = -0.05),
                                          linewidth = 1.2) +
  geom_linerange(data = preds.high.natdec.sf1,
                 aes(y = estimate,
                     x = first_answer_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)


my.plot.nat.1.sf1 <- p2_N_m_sf1  + labs(tag = "g:") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        axis.title.y = element_text(size=14),
        strip.text.x = element_text(size = 11),
        plot.tag.position = c(0.16, 0.974)) + annotate("text", x=c(1, 2, 3, 4),
                                                           y=c(1.9, 2.85, 3.5, 4.05),
                                                           label=c("*", "**", "**", "**"),
                                                           size=8)

my.plot.nat.1.sf1

###### Figure E2Ah: National government decides to close a department of a university (college) and county supports ####

preds.low.natdec.2.sf1 <- plot_predictions(m2.supportN.sf1,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sf1 = 2.5,
                                                              Support = c("Respondent's County Supports"),
                                                              tier_n = c("N")),
                                           by = c("Support", "sf1", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.84,
                                           draw = F) # Get the estimates & CIs when RAI low

preds.high.natdec.2.sf1 <- plot_predictions(m2.supportN.sf1,
                                            newdata = datagrid(first_answer_factor = 1:4,
                                                               sf1 = 3.93,
                                                               Support = c("Respondent's County Supports"),
                                                               tier_n = c("N")),
                                            by = c("Support", "sf1", "first_answer_factor"),
                                            gray = T,
                                            conf_level = 0.84,
                                            draw = F) # Get the estimates & CIs when RAI high

plot2.supportN_cou.sf1 <- plot_predictions(m2.supportN.sf1,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sf1 = c(2.5, 3.93),
                                                              Support = c("Respondent's County Supports"),
                                                              tier_n = c("N")),
                                           by = c("Support", "sf1", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.95,
                                           draw = F)


p2_n_c_sf1 <- ggplot(plot2.supportN_cou.sf1,
                     aes(x = first_answer_factor,
                         y = estimate,
                         group = first_answer_factor, shape = sf1)) +
  geom_hline(yintercept = c(1, 2, 3, 4), color='gray') + 
  facet_grid(.~Support) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = sf1),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_N_c_sf1 <- p2_n_c_sf1 +
  labs(x="Willingness to accept initial decision",
       y="",
       shape="Preference for Self-Rule Item 1") +
  scale_shape_discrete(labels= c("Mean - 1SD (2.5)", "Mean + 1SD (3.93)")) +
  theme_classic() + theme(legend.position = "bottom",
                          plot.title = element_text(hjust = 0.5, vjust = 1.5),
                          axis.title.x = element_text(vjust = -1))


p2_N_c_sf1 <- p2_N_c_sf1 + geom_linerange(data=preds.low.natdec.2.sf1,
                                          aes(y = estimate,
                                              x = first_answer_factor,
                                              ymin = conf.low,
                                              ymax = conf.high),
                                          position = position_nudge(x = -0.05),
                                          linewidth = 1.2) +
  geom_linerange(data = preds.high.natdec.2.sf1,
                 aes(y = estimate,
                     x = first_answer_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)



my.plot.nat.2.sf1 <- p2_N_c_sf1  + labs(tag = "h:") +
  theme(axis.text = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 16),
        strip.text.x = element_text(size = 11),
        plot.tag.position = c(0.16, 0.972)) + annotate("text", x=c(2, 3, 4),
                                                           y=c(2.65, 3.35, 3.955),
                                                           label=c("**", "**", "**"),
                                                           size = 8)
my.plot.nat.2.sf1

###### Figure E2Ai: National government decides to close a department of a university (college) and municipality and county support ####

preds.low.natdec.3.sf1 <- plot_predictions(m2.supportN.sf1,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sf1 = 2.5,
                                                              Support = c("Municipality & County Support"),
                                                              tier_n = c("N")),
                                           by = c("Support", "sf1", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.84,
                                           draw = F) # Get the estimates & CIs when RAI low

preds.high.natdec.3.sf1 <- plot_predictions(m2.supportN.sf1,
                                            newdata = datagrid(first_answer_factor = 1:4,
                                                               sf1 = 3.93,
                                                               Support = c("Municipality & County Support"),
                                                               tier_n = c("N")),
                                            by = c("Support", "sf1", "first_answer_factor"),
                                            gray = T,
                                            conf_level = 0.84,
                                            draw = F) # Get the estimates & CIs when RAI high

plot2.supportN_muncou.sf1 <- plot_predictions(m2.supportN.sf1,
                                              newdata = datagrid(first_answer_factor = 1:4,
                                                                 sf1 = c(2.5, 3.93),
                                                                 Support = c("Municipality & County Support"),
                                                                 tier_n = c("N")),
                                              by = c("Support", "sf1", "first_answer_factor"),
                                              gray = T,
                                              conf_level = 0.95,
                                              draw = F)


p2_n_mc_sf1 <- ggplot(plot2.supportN_muncou.sf1,
                      aes(x = first_answer_factor,
                          y = estimate,
                          group = first_answer_factor, shape = sf1)) +
  geom_hline(yintercept = c(1, 2, 3, 4), color='gray') + 
  facet_grid(.~Support) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = sf1),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_N_mc_sf1 <- p2_n_mc_sf1 +
  labs(x="", y="",
       shape="Preference for Self-Rule Item 1") +
  scale_shape_discrete(labels= c("Mean - 1SD (2.5)", "Mean + 1SD (3.93)")) +
  theme_classic() + theme(legend.position = "bottom",
                          plot.title = element_text(hjust = 0.5, vjust = 1.5))


p2_N_mc_sf1 <- p2_N_mc_sf1 + geom_linerange(data=preds.low.natdec.3.sf1,
                                            aes(y = estimate,
                                                x = first_answer_factor,
                                                ymin = conf.low,
                                                ymax = conf.high),
                                            position = position_nudge(x = -0.05),
                                            linewidth = 1.2) +
  geom_linerange(data = preds.high.natdec.3.sf1,
                 aes(y = estimate,
                     x = first_answer_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)



my.plot.nat.3.sf1 <- p2_N_mc_sf1  + labs(tag = "i:") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        strip.text.x = element_text(size = 11),
        plot.tag.position = c(0.16, 0.972)) + annotate("text", x=c(2, 3, 4),
                                                           y=c(2.7, 3.5, 4.05),
                                                           label=c("*", "**", "*"),
                                                           size = 8)



p3.sf1 <- ggarrange(my.plot.nat.1.sf1, my.plot.nat.2.sf1, my.plot.nat.3.sf1, nrow = 1, ncol = 3,
                    common.legend = T, legend = "bottom") 

p3.sf1 <- p3.sf1 + plot_annotation(title = "\nNational government decides to close \na department of a university (college)") &
  theme(plot.title = element_text(hjust = .5, size = 18),
        legend.title = element_text(size = 16),
        legend.text = element_text(size=15))

my.plot.all.sf1 <- ggarrange(p1.sf1, p2.sf1, p3.sf1, nrow = 3, ncol = 1)


# Figure E2A as a .jpeg file:
ggsave("Figure E2A.jpeg",
       width = 12, height = 17)


#### Figure E2B. Model 2-Q2-Q4-Q6: Preference for self-rule item 2. ####

##### Figures E2Ba-E2Bc: Municipality decides to close kindergarten. ####
mod.sf2.mun.support <- feols(second_answer ~ first_answer_factor * sf2 * support_m + tier_m | responseid,
                             data = dataset)
summary(mod.sf2.mun.support)

###### Figure E2Ba: Municipality decides to close kindergarten and county supports ####

preds.railow.mundec.sf2 <- plot_predictions(mod.sf2.mun.support,
                                            newdata = datagrid(first_answer_factor = 1:4,
                                                               sf2 = 1.67,
                                                               support_m = c("Respondent's County Supports"),
                                                               tier_m = c("M")),
                                            by = c("support_m", "sf2", "first_answer_factor"),
                                            gray = T,
                                            conf_level = 0.84,
                                            draw = F) # Get the estimates & CIs when RAI low

preds.high.mundec.sf2 <- plot_predictions(mod.sf2.mun.support,
                                          newdata = datagrid(first_answer_factor = 1:4,
                                                             sf2 = 3.6,
                                                             support_m = c("Respondent's County Supports"),
                                                             tier_m = c("M")),
                                          by = c("support_m", "sf2", "first_answer_factor"),
                                          gray = T,
                                          conf_level = 0.84,
                                          draw = F) # Get the estimates & CIs when RAI high

plot2.supportM_cou.sf2 <- plot_predictions(mod.sf2.mun.support,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sf2 = c(1.67, 3.6),
                                                              support_m = c("Respondent's County Supports"),
                                                              tier_m = c("M")),
                                           by = c("support_m", "sf2", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.95,
                                           draw = F)



p2_m_c_sf2 <- ggplot(plot2.supportM_cou.sf2,
                     aes(x = first_answer_factor,
                         y = estimate,
                         group = first_answer_factor, shape = sf2)) +
  geom_hline(yintercept = c(1, 2, 3, 4), color='gray') + 
  facet_grid(.~support_m) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = sf2),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_M_c_sf2 <- p2_m_c_sf2 +
  labs(x="",
       y="",
       shape="Preference for Self-Rule Item 2") +
  scale_shape_discrete(labels= c("Mean - 1SD (1.67)", "Mean + 1SD (3.6)")) +
  theme_classic()  + theme(legend.position = "bottom",
                           plot.title = element_text(hjust = 0.5, vjust = 1.5))


p2_M_c_sf2 <- p2_M_c_sf2 + geom_linerange(data=preds.railow.mundec.sf2,
                                          aes(y = estimate,
                                              x = first_answer_factor,
                                              ymin = conf.low,
                                              ymax = conf.high),
                                          position = position_nudge(x = -0.05),
                                          linewidth = 1.2) +
  geom_linerange(data = preds.high.mundec.sf2,
                 aes(y = estimate,
                     x = first_answer_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)



my.plot.mun.1.sf2 <- p2_M_c_sf2  + labs(tag = "a:") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        axis.title.y = element_text(size=14),
        strip.text.x = element_text(size = 11),
        plot.tag.position = c(0.16, 0.974))

my.plot.mun.1.sf2

###### Figure E2Bb: Municipality decides to close kindergarten and national government supports ####

preds.low.mundec.2.sf2 <- plot_predictions(mod.sf2.mun.support,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sf2 = 1.67,
                                                              support_m = c("National Government Supports"),
                                                              tier_m = c("M")),
                                           by = c("support_m", "sf2", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.84,
                                           draw = F) # Get the estimates & CIs when RAI low

preds.high.mundec.2.sf2 <- plot_predictions(mod.sf2.mun.support,
                                            newdata = datagrid(first_answer_factor = 1:4,
                                                               sf2 = 3.6,
                                                               support_m = c("National Government Supports"),
                                                               tier_m = c("M")),
                                            by = c("support_m", "sf2", "first_answer_factor"),
                                            gray = T,
                                            conf_level = 0.84,
                                            draw = F) # Get the estimates & CIs when RAI high

plot2.supportM_nat.sf2 <- plot_predictions(mod.sf2.mun.support,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sf2 = c(1.67, 3.6),
                                                              support_m = c("National Government Supports"),
                                                              tier_m = c("M")),
                                           by = c("support_m", "sf2", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.95,
                                           draw = F)


p2_m_n_sf2 <- ggplot(plot2.supportM_nat.sf2,
                     aes(x = first_answer_factor,
                         y = estimate,
                         group = first_answer_factor, shape = sf2)) +
  geom_hline(yintercept = c(1, 2, 3, 4), color='gray') + 
  facet_grid(.~support_m) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = sf2),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_M_n_sf2 <- p2_m_n_sf2 +
  labs(x="",
       y="",
       shape="Preference for Self-Rule Item 2") +
  scale_shape_discrete(labels= c("Mean - 1SD (1.67)", "Mean + 1SD (3.6)")) +
  theme_classic() + theme(legend.position = "bottom",
                          plot.title = element_text(hjust = 0.5, vjust = 1.5))


p2_M_n_sf2 <- p2_M_n_sf2 + geom_linerange(data=preds.low.mundec.2.sf2,
                                          aes(y = estimate,
                                              x = first_answer_factor,
                                              ymin = conf.low,
                                              ymax = conf.high),
                                          position = position_nudge(x = -0.05),
                                          linewidth = 1.2) +
  geom_linerange(data = preds.high.mundec.2.sf2,
                 aes(y = estimate,
                     x = first_answer_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)



my.plot.mun.2.sf2 <- p2_M_n_sf2  + labs(tag = "b:") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        strip.text.x = element_text(size = 11),
        plot.tag.position = c(0.16, 0.974))

my.plot.mun.2.sf2

###### Figure E2Bc: Municipality decides to close kindergarten and county and national government support ####

preds.low.mundec.3.sf2 <- plot_predictions(mod.sf2.mun.support,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sf2 = 1.67,
                                                              support_m = c("County & National Government Support"),
                                                              tier_m = c("M")),
                                           by = c("support_m", "sf2", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.84,
                                           draw = F) # Get the estimates & CIs when sf2 low

preds.high.mundec.3.sf2 <- plot_predictions(mod.sf2.mun.support,
                                            newdata = datagrid(first_answer_factor = 1:4,
                                                               sf2 = 3.6,
                                                               support_m = c("County & National Government Support"),
                                                               tier_m = c("M")),
                                            by = c("support_m", "sf2", "first_answer_factor"),
                                            gray = T,
                                            conf_level = 0.84,
                                            draw = F) # Get the estimates & CIs when RAI high

plot2.supportM_counat.sf2 <- plot_predictions(mod.sf2.mun.support,
                                              newdata = datagrid(first_answer_factor = 1:4,
                                                                 sf2 = c(1.67, 3.6),
                                                                 support_m = c("County & National Government Support"),
                                                                 tier_m = c("M")),
                                              by = c("support_m", "sf2", "first_answer_factor"),
                                              gray = T,
                                              conf_level = 0.95,
                                              draw = F)


p2_m_cn_sf2 <- ggplot(plot2.supportM_counat.sf2,
                      aes(x = first_answer_factor,
                          y = estimate,
                          group = first_answer_factor, shape = sf2)) +
  geom_hline(yintercept = c(1, 2, 3, 4), color='gray') + 
  facet_grid(.~support_m) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = sf2),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_M_cn_sf2 <- p2_m_cn_sf2 +
  labs(x="", y="",
       shape="Preference for Self-Rule Item 2") +
  scale_shape_discrete(labels= c("Mean - 1SD (1.67)", "Mean + 1SD (3.6)")) +
  theme_classic() + theme(legend.position = "bottom",
                          plot.title = element_text(hjust = 0.5, vjust = 1.5))


p2_M_cn_sf2 <- p2_M_cn_sf2 + geom_linerange(data=preds.low.mundec.3.sf2,
                                            aes(y = estimate,
                                                x = first_answer_factor,
                                                ymin = conf.low,
                                                ymax = conf.high),
                                            position = position_nudge(x = -0.05),
                                            linewidth = 1.2) +
  geom_linerange(data = preds.high.mundec.3.sf2,
                 aes(y = estimate,
                     x = first_answer_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)

my.plot.mun.3.sf2 <- p2_M_cn_sf2  + labs(tag = "c:") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        strip.text.x = element_text(size = 11),
        plot.tag.position = c(0.16, 0.974))

my.plot.mun.3.sf2

p1.sf2 <- ggarrange(my.plot.mun.1.sf2, my.plot.mun.2.sf2, my.plot.mun.3.sf2, nrow = 1, ncol = 3,
                    common.legend = T, legend = "none") + plot_annotation(title="Municipality decides to close kindergarten") &
  theme(plot.title = element_text(hjust = .5, size = 18), legend.text = element_blank())



##### Figures E2Bd-E2Bf: County decides to close upper secondary school ####
m2.supportC.sf2 <- feols(second_answer ~ first_answer_factor * sf2 * Support_c + tier_c | responseid,
                         data = dataset)
summary(m2.supportC.sf2)

###### Figure E2Bd: County decides to close upper secondary school and municipality supports ####

preds.railow.coudec.sf2 <- plot_predictions(m2.supportC.sf2,
                                            newdata = datagrid(first_answer_factor = 1:4,
                                                               sf2 = 1.67,
                                                               Support_c = c("Respondent's Municipality Supports"),
                                                               tier_c = c("C")),
                                            by = c("Support_c", "sf2", "first_answer_factor"),
                                            gray = T,
                                            conf_level = 0.84,
                                            draw = F) # Get the estimates & CIs when RAI low

preds.high.coudec.sf2 <- plot_predictions(m2.supportC.sf2,
                                          newdata = datagrid(first_answer_factor = 1:4,
                                                             sf2 = 3.6,
                                                             Support_c = c("Respondent's Municipality Supports"),
                                                             tier_c = c("C")),
                                          by = c("Support_c", "sf2", "first_answer_factor"),
                                          gray = T,
                                          conf_level = 0.84,
                                          draw = F) # Get the estimates & CIs when RAI high

plot2.supportC_mun.sf2 <- plot_predictions(m2.supportC.sf2,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sf2 = c(1.67, 3.6),
                                                              Support_c = c("Respondent's Municipality Supports"),
                                                              tier_c = c("C")),
                                           by = c("Support_c", "sf2", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.95,
                                           draw = F)


p2_c_m_sf2 <- ggplot(plot2.supportC_mun.sf2,
                     aes(x = first_answer_factor,
                         y = estimate,
                         group = first_answer_factor, shape = sf2)) +
  geom_hline(yintercept = c(1, 2, 3, 4), color='gray') + 
  facet_grid(.~Support_c) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = sf2),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_C_m_sf2 <- p2_c_m_sf2 +
  labs(x="",
       y="Willingness to accept decision\nwhen support is provided by another tier",
       shape="Preference for Self-Rule Item 2") +
  scale_shape_discrete(labels= c("Mean - 1SD (1.67)", "Mean + 1SD (3.6)")) +
  theme_classic()  + theme(legend.position = "bottom",
                           plot.title = element_text(hjust = 0.5, vjust = 1.5),
                           axis.title.y = element_text(size = 18, vjust = 4.5)) 



p2_C_m_sf2 <- p2_C_m_sf2 + geom_linerange(data=preds.railow.coudec.sf2,
                                          aes(y = estimate,
                                              x = first_answer_factor,
                                              ymin = conf.low,
                                              ymax = conf.high),
                                          position = position_nudge(x = -0.05),
                                          linewidth = 1.2) +
  geom_linerange(data = preds.high.coudec.sf2,
                 aes(y = estimate,
                     x = first_answer_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)



my.plot.cou.1.sf2 <- p2_C_m_sf2  + labs(tag = "d:") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        axis.title.y = element_text(size=14),
        strip.text.x = element_text(size = 11),
        plot.tag.position = c(0.19, 0.974))

my.plot.cou.1.sf2

###### Figure E2Be: County decides to close upper secondary school and national government supports ####

preds.low.coudec.2.sf2 <- plot_predictions(m2.supportC.sf2,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sf2 = 1.67,
                                                              Support_c = c("National Government Supports"),
                                                              tier_c = c("C")),
                                           by = c("Support_c", "sf2", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.84,
                                           draw = F) # Get the estimates & CIs when RAI low

preds.high.coudec.2.sf2 <- plot_predictions(m2.supportC.sf2,
                                            newdata = datagrid(first_answer_factor = 1:4,
                                                               sf2 = 3.6,
                                                               Support_c = c("National Government Supports"),
                                                               tier_c = c("C")),
                                            by = c("Support_c", "sf2", "first_answer_factor"),
                                            gray = T,
                                            conf_level = 0.84,
                                            draw = F) # Get the estimates & CIs when RAI high

plot2.supportC_nat.sf2 <- plot_predictions(m2.supportC.sf2,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sf2 = c(1.67, 3.6),
                                                              Support_c = c("National Government Supports"),
                                                              tier_c = c("C")),
                                           by = c("Support_c", "sf2", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.95,
                                           draw = F)


p2_c_n_sf2 <- ggplot(plot2.supportC_nat.sf2,
                     aes(x = first_answer_factor,
                         y = estimate,
                         group = first_answer_factor, shape = sf2)) +
  geom_hline(yintercept = c(1, 2, 3, 4), color='gray') + 
  facet_grid(.~Support_c) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = sf2),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_C_n_sf2 <- p2_c_n_sf2 +
  labs(x="",
       y="",
       shape="Preference for Self-Rule Item 2") +
  scale_shape_discrete(labels= c("Mean - 1SD (1.67)", "Mean + 1SD (3.6)")) +
  theme_classic() + theme(legend.position = "bottom",
                          plot.title = element_text(hjust = 0.5, vjust = 1.5))


p2_C_n_sf2 <- p2_C_n_sf2 + geom_linerange(data=preds.low.coudec.2.sf2,
                                          aes(y = estimate,
                                              x = first_answer_factor,
                                              ymin = conf.low,
                                              ymax = conf.high),
                                          position = position_nudge(x = -0.05),
                                          linewidth = 1.2) +
  geom_linerange(data = preds.high.coudec.2.sf2,
                 aes(y = estimate,
                     x = first_answer_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)



my.plot.cou.2.sf2 <- p2_C_n_sf2  + labs(tag = "e:") +
  theme(axis.text = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 16),
        strip.text.x = element_text(size = 11),
        plot.tag.position = c(0.16, 0.974))

###### Figure E2Bf: County decides to close upper secondary school and municipality and national government support ####

preds.low.coudec.3.sf2 <- plot_predictions(m2.supportC.sf2,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sf2 = 1.67,
                                                              Support_c = c("Municipality & National Government Support"),
                                                              tier_c = c("C")),
                                           by = c("Support_c", "sf2", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.84,
                                           draw = F) # Get the estimates & CIs when RAI low

preds.high.coudec.3.sf2 <- plot_predictions(m2.supportC.sf2,
                                            newdata = datagrid(first_answer_factor = 1:4,
                                                               sf2 = 3.6,
                                                               Support_c = c("Municipality & National Government Support"),
                                                               tier_c = c("C")),
                                            by = c("Support_c", "sf2", "first_answer_factor"),
                                            gray = T,
                                            conf_level = 0.84,
                                            draw = F) # Get the estimates & CIs when RAI high

plot2.supportC_munnat.sf2 <- plot_predictions(m2.supportC.sf2,
                                              newdata = datagrid(first_answer_factor = 1:4,
                                                                 sf2 = c(1.67, 3.6),
                                                                 Support_c = c("Municipality & National Government Support"),
                                                                 tier_c = c("C")),
                                              by = c("Support_c", "sf2", "first_answer_factor"),
                                              gray = T,
                                              conf_level = 0.95,
                                              draw = F)


p2_c_mn_sf2 <- ggplot(plot2.supportC_munnat.sf2,
                      aes(x = first_answer_factor,
                          y = estimate,
                          group = first_answer_factor, shape = sf2)) +
  geom_hline(yintercept = c(1, 2, 3, 4), color='gray') + 
  facet_grid(.~Support_c) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = sf2),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_C_mn_sf2 <- p2_c_mn_sf2 +
  labs(x="", y="",
       shape="Preference for Self-Rule Item 2") +
  scale_shape_discrete(labels= c("Mean - 1SD (1.67)", "Mean + 1SD (3.6)")) +
  theme_classic() + theme(legend.position = "bottom",
                          plot.title = element_text(hjust = 0.5, vjust = 1.5)) 


p2_C_mn_sf2 <- p2_C_mn_sf2 + geom_linerange(data=preds.low.coudec.3.sf2,
                                            aes(y = estimate,
                                                x = first_answer_factor,
                                                ymin = conf.low,
                                                ymax = conf.high),
                                            position = position_nudge(x = -0.05),
                                            linewidth = 1.2) +
  geom_linerange(data = preds.high.coudec.3.sf2,
                 aes(y = estimate,
                     x = first_answer_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)


my.plot.cou.3.sf2 <- p2_C_mn_sf2  + labs(tag = "f:") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        strip.text.x = element_text(size = 11),
        plot.tag.position = c(0.134, 0.973)) 


p2.sf2 <- ggarrange(my.plot.cou.1.sf2, my.plot.cou.2.sf2, my.plot.cou.3.sf2, nrow = 1, ncol = 3,
                    common.legend = T, legend = "none") 

p2.sf2 <- p2.sf2 + plot_annotation(title = "\nCounty decides to close upper secondary school") &
  theme(plot.title = element_text(hjust = .5, size = 18))


##### Figures E2Bg-E2Bi: National government decides to close a department of a university (college) #####
m2.supportN.sf2 <- feols(second_answer ~ first_answer_factor * sf2 * Support + tier_n | responseid,
                         data = dataset)
summary(m2.supportN.sf2)

###### Figure E2Bg: National government decides to close a department of a university (college) and municipality supports #####
preds.railow.natdec.sf2 <- plot_predictions(m2.supportN.sf2,
                                            newdata = datagrid(first_answer_factor = 1:4,
                                                               sf2 = 1.67,
                                                               Support = c("Respondent's Municipality Supports"),
                                                               tier_n = c("N")),
                                            by = c("Support", "sf2", "first_answer_factor"),
                                            gray = T,
                                            conf_level = 0.84,
                                            draw = F) # Get the estimates & CIs when RAI low

preds.high.natdec.sf2 <- plot_predictions(m2.supportN.sf2,
                                          newdata = datagrid(first_answer_factor = 1:4,
                                                             sf2 = 3.6,
                                                             Support = c("Respondent's Municipality Supports"),
                                                             tier_n = c("N")),
                                          by = c("Support", "sf2", "first_answer_factor"),
                                          gray = T,
                                          conf_level = 0.84,
                                          draw = F) # Get the estimates & CIs when RAI high

plot2.supportN_mun.sf2 <- plot_predictions(m2.supportN.sf2,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sf2 = c(1.67, 3.6),
                                                              Support = c("Respondent's Municipality Supports"),
                                                              tier_n = c("N")),
                                           by = c("Support", "sf2", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.95,
                                           draw = F)


p2_n_m_sf2 <- ggplot(plot2.supportN_mun.sf2,
                     aes(x = first_answer_factor,
                         y = estimate,
                         group = first_answer_factor, shape = sf2)) +
  geom_hline(yintercept = c(1, 2, 3, 4), color='gray') + 
  facet_grid(.~Support) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = sf2),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_N_m_sf2 <- p2_n_m_sf2 +
  labs(x="",
       y="",
       shape="Preference for Self-Rule Item 2") +
  scale_shape_discrete(labels= c("Mean - 1SD (1.67)", "Mean + 1SD (3.6)")) +
  theme_classic()  + theme(legend.position = "bottom",
                           plot.title = element_text(hjust = 0.5, vjust = 1.5),
                           axis.title.y = element_text(size = 18, vjust = 4.5)) 



p2_N_m_sf2 <- p2_N_m_sf2 + geom_linerange(data=preds.railow.natdec.sf2,
                                          aes(y = estimate,
                                              x = first_answer_factor,
                                              ymin = conf.low,
                                              ymax = conf.high),
                                          position = position_nudge(x = -0.05),
                                          linewidth = 1.2) +
  geom_linerange(data = preds.high.natdec.sf2,
                 aes(y = estimate,
                     x = first_answer_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)



my.plot.nat.1.sf2 <- p2_N_m_sf2  + labs(tag = "g:") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        axis.title.y = element_text(size=14),
        strip.text.x = element_text(size = 11),
        plot.tag.position = c(0.16, 0.974)) 

my.plot.nat.1.sf2

###### Figure E2Bh: National government decides to close a department of a university (college) and county supports ####

preds.low.natdec.2.sf2 <- plot_predictions(m2.supportN.sf2,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sf2 = 1.67,
                                                              Support = c("Respondent's County Supports"),
                                                              tier_n = c("N")),
                                           by = c("Support", "sf2", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.84,
                                           draw = F) # Get the estimates & CIs when RAI low

preds.high.natdec.2.sf2 <- plot_predictions(m2.supportN.sf2,
                                            newdata = datagrid(first_answer_factor = 1:4,
                                                               sf2 = 3.6,
                                                               Support = c("Respondent's County Supports"),
                                                               tier_n = c("N")),
                                            by = c("Support", "sf2", "first_answer_factor"),
                                            gray = T,
                                            conf_level = 0.84,
                                            draw = F) # Get the estimates & CIs when RAI high

plot2.supportN_cou.sf2 <- plot_predictions(m2.supportN.sf2,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sf2 = c(1.67, 3.6),
                                                              Support = c("Respondent's County Supports"),
                                                              tier_n = c("N")),
                                           by = c("Support", "sf2", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.95,
                                           draw = F)


p2_n_c_sf2 <- ggplot(plot2.supportN_cou.sf2,
                     aes(x = first_answer_factor,
                         y = estimate,
                         group = first_answer_factor, shape = sf2)) +
  geom_hline(yintercept = c(1, 2, 3, 4), color='gray') + 
  facet_grid(.~Support) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = sf2),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_N_c_sf2 <- p2_n_c_sf2 +
  labs(x="Willingness to accept initial decision",
       y="",
       shape="Preference for Self-Rule Item 2") +
  scale_shape_discrete(labels= c("Mean - 1SD (1.67)", "Mean + 1SD (3.6)")) +
  theme_classic() + theme(legend.position = "bottom",
                          plot.title = element_text(hjust = 0.5, vjust = 1.5),
                          axis.title.x = element_text(vjust = -1))


p2_N_c_sf2 <- p2_N_c_sf2 + geom_linerange(data=preds.low.natdec.2.sf2,
                                          aes(y = estimate,
                                              x = first_answer_factor,
                                              ymin = conf.low,
                                              ymax = conf.high),
                                          position = position_nudge(x = -0.05),
                                          linewidth = 1.2) +
  geom_linerange(data = preds.high.natdec.2.sf2,
                 aes(y = estimate,
                     x = first_answer_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)



my.plot.nat.2.sf2 <- p2_N_c_sf2  + labs(tag = "h:") +
  theme(axis.text = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 16),
        strip.text.x = element_text(size = 11),
        plot.tag.position = c(0.16, 0.972))

###### Figure E2Bi: National government decides to close a department of a university (college) and municipality and county support ####

preds.low.natdec.3.sf2 <- plot_predictions(m2.supportN.sf2,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sf2 = 1.67,
                                                              Support = c("Municipality & County Support"),
                                                              tier_n = c("N")),
                                           by = c("Support", "sf2", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.84,
                                           draw = F) # Get the estimates & CIs when RAI low

preds.high.natdec.3.sf2 <- plot_predictions(m2.supportN.sf2,
                                            newdata = datagrid(first_answer_factor = 1:4,
                                                               sf2 = 3.6,
                                                               Support = c("Municipality & County Support"),
                                                               tier_n = c("N")),
                                            by = c("Support", "sf2", "first_answer_factor"),
                                            gray = T,
                                            conf_level = 0.84,
                                            draw = F) # Get the estimates & CIs when RAI high

plot2.supportN_muncou.sf2 <- plot_predictions(m2.supportN.sf2,
                                              newdata = datagrid(first_answer_factor = 1:4,
                                                                 sf2 = c(1.67, 3.6),
                                                                 Support = c("Municipality & County Support"),
                                                                 tier_n = c("N")),
                                              by = c("Support", "sf2", "first_answer_factor"),
                                              gray = T,
                                              conf_level = 0.95,
                                              draw = F)


p2_n_mc_sf2 <- ggplot(plot2.supportN_muncou.sf2,
                      aes(x = first_answer_factor,
                          y = estimate,
                          group = first_answer_factor, shape = sf2)) +
  geom_hline(yintercept = c(1, 2, 3, 4), color='gray') + 
  facet_grid(.~Support) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = sf2),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_N_mc_sf2 <- p2_n_mc_sf2 +
  labs(x="", y="",
       shape="Preference for Self-Rule Item 2") +
  scale_shape_discrete(labels= c("Mean - 1SD (1.67)", "Mean + 1SD (3.6)")) +
  theme_classic() + theme(legend.position = "bottom",
                          plot.title = element_text(hjust = 0.5, vjust = 1.5)) 


p2_N_mc_sf2 <- p2_N_mc_sf2 + geom_linerange(data=preds.low.natdec.3.sf2,
                                            aes(y = estimate,
                                                x = first_answer_factor,
                                                ymin = conf.low,
                                                ymax = conf.high),
                                            position = position_nudge(x = -0.05),
                                            linewidth = 1.2) +
  geom_linerange(data = preds.high.natdec.3.sf2,
                 aes(y = estimate,
                     x = first_answer_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)



my.plot.nat.3.sf2 <- p2_N_mc_sf2  + labs(tag = "i:") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        strip.text.x = element_text(size = 11),
        plot.tag.position = c(0.16, 0.972))


p3.sf2 <- ggarrange(my.plot.nat.1.sf2, my.plot.nat.2.sf2, my.plot.nat.3.sf2, nrow = 1, ncol = 3,
                    common.legend = T, legend = "bottom") 

p3.sf2 <- p3.sf2 + plot_annotation(title = "\nNational government decides to close \na department of a university (college)") &
  theme(plot.title = element_text(hjust = .5, size = 18),
        legend.title = element_text(size = 16),
        legend.text = element_text(size=15))

my.plot.all.sf2 <- ggarrange(p1.sf2, p2.sf2, p3.sf2, nrow = 3, ncol = 1)

# Save Figure E2B as a .jpeg file:
ggsave("Figure_E2B.jpeg",
       width = 12, height = 17)


#### Figure E2C. Model 2-Q2-Q4-Q6: Preference for self-rule item 3 ####

##### Figures E2Ca-E2Cc: Municipality decides to close kindergarten. ####

mod.sf3.mun.support <- feols(second_answer ~ first_answer_factor * sf3 * support_m + tier_m | responseid,
                             data = dataset)
summary(mod.sf3.mun.support)

###### Figure E2Ca: Municipality decides to close kindergarten and county supports ####

preds.railow.mundec.sf3 <- plot_predictions(mod.sf3.mun.support,
                                            newdata = datagrid(first_answer_factor = 1:4,
                                                               sf3 = 2.07,
                                                               support_m = c("Respondent's County Supports"),
                                                               tier_m = c("M")),
                                            by = c("support_m", "sf3", "first_answer_factor"),
                                            gray = T,
                                            conf_level = 0.84,
                                            draw = F) # Get the estimates & CIs when RAI low

preds.high.mundec.sf3 <- plot_predictions(mod.sf3.mun.support,
                                          newdata = datagrid(first_answer_factor = 1:4,
                                                             sf3 = 3.75,
                                                             support_m = c("Respondent's County Supports"),
                                                             tier_m = c("M")),
                                          by = c("support_m", "sf3", "first_answer_factor"),
                                          gray = T,
                                          conf_level = 0.84,
                                          draw = F) # Get the estimates & CIs when RAI high

plot2.supportM_cou.sf3 <- plot_predictions(mod.sf3.mun.support,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sf3 = c(2.07, 3.75),
                                                              support_m = c("Respondent's County Supports"),
                                                              tier_m = c("M")),
                                           by = c("support_m", "sf3", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.95,
                                           draw = F)


p2_m_c_sf3 <- ggplot(plot2.supportM_cou.sf3,
                     aes(x = first_answer_factor,
                         y = estimate,
                         group = first_answer_factor, shape = sf3)) +
  geom_hline(yintercept = c(1, 2, 3, 4), color='gray') + 
  facet_grid(.~support_m) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = sf3),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_M_c_sf3 <- p2_m_c_sf3 +
  labs(x="",
       y="",
       shape="Preference for Self-Rule Item 3") +
  scale_shape_discrete(labels= c("Mean - 1SD (2.07)", "Mean + 1SD (3.75)")) +
  theme_classic()  + theme(legend.position = "bottom",
                           plot.title = element_text(hjust = 0.5, vjust = 1.5))


p2_M_c_sf3 <- p2_M_c_sf3 + geom_linerange(data=preds.railow.mundec.sf3,
                                          aes(y = estimate,
                                              x = first_answer_factor,
                                              ymin = conf.low,
                                              ymax = conf.high),
                                          position = position_nudge(x = -0.05),
                                          linewidth = 1.2) +
  geom_linerange(data = preds.high.mundec.sf3,
                 aes(y = estimate,
                     x = first_answer_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)

my.plot.mun.1.sf3 <- p2_M_c_sf3  + labs(tag = "a:") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        axis.title.y = element_text(size=14),
        strip.text.x = element_text(size = 11),
        plot.tag.position = c(0.16, 0.974))

my.plot.mun.1.sf3

###### Figure E2Cb: Municipality decides to close kindergarten and national government supports ####

preds.low.mundec.2.sf3 <- plot_predictions(mod.sf3.mun.support,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sf3 = 2.07,
                                                              support_m = c("National Government Supports"),
                                                              tier_m = c("M")),
                                           by = c("support_m", "sf3", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.84,
                                           draw = F) # Get the estimates & CIs when RAI low

preds.high.mundec.2.sf3 <- plot_predictions(mod.sf3.mun.support,
                                            newdata = datagrid(first_answer_factor = 1:4,
                                                               sf3 = 3.75,
                                                               support_m = c("National Government Supports"),
                                                               tier_m = c("M")),
                                            by = c("support_m", "sf3", "first_answer_factor"),
                                            gray = T,
                                            conf_level = 0.84,
                                            draw = F) # Get the estimates & CIs when RAI high

plot2.supportM_nat.sf3 <- plot_predictions(mod.sf3.mun.support,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sf3 = c(2.07, 3.75),
                                                              support_m = c("National Government Supports"),
                                                              tier_m = c("M")),
                                           by = c("support_m", "sf3", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.95,
                                           draw = F)


p2_m_n_sf3 <- ggplot(plot2.supportM_nat.sf3,
                     aes(x = first_answer_factor,
                         y = estimate,
                         group = first_answer_factor, shape = sf3)) +
  geom_hline(yintercept = c(1, 2, 3, 4), color='gray') + 
  facet_grid(.~support_m) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = sf3),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_M_n_sf3 <- p2_m_n_sf3 +
  labs(x="",
       y="",
       shape="Preference for Self-Rule Item 3") +
  scale_shape_discrete(labels= c("Mean - 1SD (2.07)", "Mean + 1SD (3.75)")) +
  theme_classic() + theme(legend.position = "bottom",
                          plot.title = element_text(hjust = 0.5, vjust = 1.5))


p2_M_n_sf3 <- p2_M_n_sf3 + geom_linerange(data=preds.low.mundec.2.sf3,
                                          aes(y = estimate,
                                              x = first_answer_factor,
                                              ymin = conf.low,
                                              ymax = conf.high),
                                          position = position_nudge(x = -0.05),
                                          linewidth = 1.2) +
  geom_linerange(data = preds.high.mundec.2.sf3,
                 aes(y = estimate,
                     x = first_answer_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)



my.plot.mun.2.sf3 <- p2_M_n_sf3  + labs(tag = "b:") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        strip.text.x = element_text(size = 11),
        plot.tag.position = c(0.16, 0.974))

my.plot.mun.2.sf3

###### Figure E2Cc: Municipality decides to close kindergarten and county and national government support ####

preds.low.mundec.3.sf3 <- plot_predictions(mod.sf3.mun.support,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sf3 = 2.07,
                                                              support_m = c("County & National Government Support"),
                                                              tier_m = c("M")),
                                           by = c("support_m", "sf3", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.84,
                                           draw = F) # Get the estimates & CIs when sf3 low

preds.high.mundec.3.sf3 <- plot_predictions(mod.sf3.mun.support,
                                            newdata = datagrid(first_answer_factor = 1:4,
                                                               sf3 = 3.75,
                                                               support_m = c("County & National Government Support"),
                                                               tier_m = c("M")),
                                            by = c("support_m", "sf3", "first_answer_factor"),
                                            gray = T,
                                            conf_level = 0.84,
                                            draw = F) # Get the estimates & CIs when RAI high

plot2.supportM_counat.sf3 <- plot_predictions(mod.sf3.mun.support,
                                              newdata = datagrid(first_answer_factor = 1:4,
                                                                 sf3 = c(2.07, 3.75),
                                                                 support_m = c("County & National Government Support"),
                                                                 tier_m = c("M")),
                                              by = c("support_m", "sf3", "first_answer_factor"),
                                              gray = T,
                                              conf_level = 0.95,
                                              draw = F)


p2_m_cn_sf3 <- ggplot(plot2.supportM_counat.sf3,
                      aes(x = first_answer_factor,
                          y = estimate,
                          group = first_answer_factor, shape = sf3)) +
  geom_hline(yintercept = c(1, 2, 3, 4), color='gray') + 
  facet_grid(.~support_m) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = sf3),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_M_cn_sf3 <- p2_m_cn_sf3 +
  labs(x="", y="",
       shape="Preference for Self-Rule Item 3") +
  scale_shape_discrete(labels= c("Mean - 1SD (2.07)", "Mean + 1SD (3.75)")) +
  theme_classic() + theme(legend.position = "bottom",
                          plot.title = element_text(hjust = 0.5, vjust = 1.5))


p2_M_cn_sf3 <- p2_M_cn_sf3 + geom_linerange(data=preds.low.mundec.3.sf3,
                                            aes(y = estimate,
                                                x = first_answer_factor,
                                                ymin = conf.low,
                                                ymax = conf.high),
                                            position = position_nudge(x = -0.05),
                                            linewidth = 1.2) +
  geom_linerange(data = preds.high.mundec.3.sf3,
                 aes(y = estimate,
                     x = first_answer_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)

my.plot.mun.3.sf3 <- p2_M_cn_sf3  + labs(tag = "c:") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        strip.text.x = element_text(size = 11),
        plot.tag.position = c(0.16, 0.974))

my.plot.mun.3.sf3

p1.sf3 <- ggarrange(my.plot.mun.1.sf3, my.plot.mun.2.sf3, my.plot.mun.3.sf3, nrow = 1, ncol = 3,
                    common.legend = T, legend = "none") + plot_annotation(title="Municipality decides to close kindergarten") &
  theme(plot.title = element_text(hjust = .5, size = 18), legend.text = element_blank())


##### Figures E2Cd-E2Cf: County decides to close upper secondary school ####

m2.supportC.sf3 <- feols(second_answer ~ first_answer_factor * sf3 * Support_c + tier_c | responseid,
                         data = dataset)
summary(m2.supportC.sf3)

###### Figure E2Cd: County decides to close upper secondary school and municipality supports #####

preds.railow.coudec.sf3 <- plot_predictions(m2.supportC.sf3,
                                            newdata = datagrid(first_answer_factor = 1:4,
                                                               sf3 = 2.07,
                                                               Support_c = c("Respondent's Municipality Supports"),
                                                               tier_c = c("C")),
                                            by = c("Support_c", "sf3", "first_answer_factor"),
                                            gray = T,
                                            conf_level = 0.84,
                                            draw = F) # Get the estimates & CIs when RAI low

preds.high.coudec.sf3 <- plot_predictions(m2.supportC.sf3,
                                          newdata = datagrid(first_answer_factor = 1:4,
                                                             sf3 = 3.75,
                                                             Support_c = c("Respondent's Municipality Supports"),
                                                             tier_c = c("C")),
                                          by = c("Support_c", "sf3", "first_answer_factor"),
                                          gray = T,
                                          conf_level = 0.84,
                                          draw = F) # Get the estimates & CIs when RAI high

plot2.supportC_mun.sf3 <- plot_predictions(m2.supportC.sf3,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sf3 = c(2.07, 3.75),
                                                              Support_c = c("Respondent's Municipality Supports"),
                                                              tier_c = c("C")),
                                           by = c("Support_c", "sf3", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.95,
                                           draw = F)


p2_c_m_sf3 <- ggplot(plot2.supportC_mun.sf3,
                     aes(x = first_answer_factor,
                         y = estimate,
                         group = first_answer_factor, shape = sf3)) +
  geom_hline(yintercept = c(1, 2, 3, 4), color='gray') + 
  facet_grid(.~Support_c) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = sf3),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_C_m_sf3 <- p2_c_m_sf3 +
  labs(x="",
       y="Willingness to accept decision\nwhen support is provided by another tier",
       shape="Preference for Self-Rule Item 3") +
  scale_shape_discrete(labels= c("Mean - 1SD (2.07)", "Mean + 1SD (3.75)")) +
  theme_classic()  + theme(legend.position = "bottom",
                           plot.title = element_text(hjust = 0.5, vjust = 1.5),
                           axis.title.y = element_text(size = 18, vjust = 4.5)) 



p2_C_m_sf3 <- p2_C_m_sf3 + geom_linerange(data=preds.railow.coudec.sf3,
                                          aes(y = estimate,
                                              x = first_answer_factor,
                                              ymin = conf.low,
                                              ymax = conf.high),
                                          position = position_nudge(x = -0.05),
                                          linewidth = 1.2) +
  geom_linerange(data = preds.high.coudec.sf3,
                 aes(y = estimate,
                     x = first_answer_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)



my.plot.cou.1.sf3 <- p2_C_m_sf3  + labs(tag = "d:") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        axis.title.y = element_text(size=14),
        strip.text.x = element_text(size = 11),
        plot.tag.position = c(0.18, 0.974))

my.plot.cou.1.sf3

###### Figure E2Ce: County decides to close upper secondary school and national government supports ####

preds.low.coudec.2.sf3 <- plot_predictions(m2.supportC.sf3,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sf3 = 2.07,
                                                              Support_c = c("National Government Supports"),
                                                              tier_c = c("C")),
                                           by = c("Support_c", "sf3", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.84,
                                           draw = F) # Get the estimates & CIs when RAI low

preds.high.coudec.2.sf3 <- plot_predictions(m2.supportC.sf3,
                                            newdata = datagrid(first_answer_factor = 1:4,
                                                               sf3 = 3.75,
                                                               Support_c = c("National Government Supports"),
                                                               tier_c = c("C")),
                                            by = c("Support_c", "sf3", "first_answer_factor"),
                                            gray = T,
                                            conf_level = 0.84,
                                            draw = F) # Get the estimates & CIs when RAI high

plot2.supportC_nat.sf3 <- plot_predictions(m2.supportC.sf3,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sf3 = c(2.07, 3.75),
                                                              Support_c = c("National Government Supports"),
                                                              tier_c = c("C")),
                                           by = c("Support_c", "sf3", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.95,
                                           draw = F)

p2_c_n_sf3 <- ggplot(plot2.supportC_nat.sf3,
                     aes(x = first_answer_factor,
                         y = estimate,
                         group = first_answer_factor, shape = sf3)) +
  geom_hline(yintercept = c(1, 2, 3, 4), color='gray') + 
  facet_grid(.~Support_c) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = sf3),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_C_n_sf3 <- p2_c_n_sf3 +
  labs(x="",
       y="",
       shape="Preference for Self-Rule Item 3") +
  scale_shape_discrete(labels= c("Mean - 1SD (2.07)", "Mean + 1SD (3.75)")) +
  theme_classic() + theme(legend.position = "bottom",
                          plot.title = element_text(hjust = 0.5, vjust = 1.5))


p2_C_n_sf3 <- p2_C_n_sf3 + geom_linerange(data=preds.low.coudec.2.sf3,
                                          aes(y = estimate,
                                              x = first_answer_factor,
                                              ymin = conf.low,
                                              ymax = conf.high),
                                          position = position_nudge(x = -0.05),
                                          linewidth = 1.2) +
  geom_linerange(data = preds.high.coudec.2.sf3,
                 aes(y = estimate,
                     x = first_answer_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)

my.plot.cou.2.sf3 <- p2_C_n_sf3  + labs(tag = "e:") +
  theme(axis.text = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 16),
        strip.text.x = element_text(size = 11),
        plot.tag.position = c(0.16, 0.974))

my.plot.cou.2.sf3

###### Figure E2Cf: County decides to close upper secondary school and municipality and national government support ####

preds.low.coudec.3.sf3 <- plot_predictions(m2.supportC.sf3,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sf3 = 2.07,
                                                              Support_c = c("Municipality & National Government Support"),
                                                              tier_c = c("C")),
                                           by = c("Support_c", "sf3", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.84,
                                           draw = F) # Get the estimates & CIs when RAI low

preds.high.coudec.3.sf3 <- plot_predictions(m2.supportC.sf3,
                                            newdata = datagrid(first_answer_factor = 1:4,
                                                               sf3 = 3.75,
                                                               Support_c = c("Municipality & National Government Support"),
                                                               tier_c = c("C")),
                                            by = c("Support_c", "sf3", "first_answer_factor"),
                                            gray = T,
                                            conf_level = 0.84,
                                            draw = F) # Get the estimates & CIs when RAI high

plot2.supportC_munnat.sf3 <- plot_predictions(m2.supportC.sf3,
                                              newdata = datagrid(first_answer_factor = 1:4,
                                                                 sf3 = c(2.07, 3.75),
                                                                 Support_c = c("Municipality & National Government Support"),
                                                                 tier_c = c("C")),
                                              by = c("Support_c", "sf3", "first_answer_factor"),
                                              gray = T,
                                              conf_level = 0.95,
                                              draw = F)


p2_c_mn_sf3 <- ggplot(plot2.supportC_munnat.sf3,
                      aes(x = first_answer_factor,
                          y = estimate,
                          group = first_answer_factor, shape = sf3)) +
  geom_hline(yintercept = c(1, 2, 3, 4), color='gray') + 
  facet_grid(.~Support_c) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = sf3),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_C_mn_sf3 <- p2_c_mn_sf3 +
  labs(x="", y="",
       shape="Preference for Self-Rule Item 3") +
  scale_shape_discrete(labels= c("Mean - 1SD (2.07)", "Mean + 1SD (3.75)")) +
  theme_classic() + theme(legend.position = "bottom",
                          plot.title = element_text(hjust = 0.5, vjust = 1.5))


p2_C_mn_sf3 <- p2_C_mn_sf3 + geom_linerange(data=preds.low.coudec.3.sf3,
                                            aes(y = estimate,
                                                x = first_answer_factor,
                                                ymin = conf.low,
                                                ymax = conf.high),
                                            position = position_nudge(x = -0.05),
                                            linewidth = 1.2) +
  geom_linerange(data = preds.high.coudec.3.sf3,
                 aes(y = estimate,
                     x = first_answer_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)


my.plot.cou.3.sf3 <- p2_C_mn_sf3  + labs(tag = "f:") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        strip.text.x = element_text(size = 11),
        plot.tag.position = c(0.134, 0.973)) 


p2.sf3 <- ggarrange(my.plot.cou.1.sf3, my.plot.cou.2.sf3, my.plot.cou.3.sf3, nrow = 1, ncol = 3,
                    common.legend = T, legend = "none") 

p2.sf3 <- p2.sf3 + plot_annotation(title = "\nCounty decides to close upper secondary school") &
  theme(plot.title = element_text(hjust = .5, size = 18))


##### Figures E2Cg-E2Ci: National government decides to close a department of a university (college) #####
m2.supportN.sf3 <- feols(second_answer ~ first_answer_factor * sf3 * Support + tier_n | responseid,
                         data = dataset)
summary(m2.supportN.sf3)

###### Figure E2Cg: National government decides to close a department of a university (college) and municipality supports #####
preds.railow.natdec.sf3 <- plot_predictions(m2.supportN.sf3,
                                            newdata = datagrid(first_answer_factor = 1:4,
                                                               sf3 = 2.07,
                                                               Support = c("Respondent's Municipality Supports"),
                                                               tier_n = c("N")),
                                            by = c("Support", "sf3", "first_answer_factor"),
                                            gray = T,
                                            conf_level = 0.84,
                                            draw = F) # Get the estimates & CIs when RAI low

preds.high.natdec.sf3 <- plot_predictions(m2.supportN.sf3,
                                          newdata = datagrid(first_answer_factor = 1:4,
                                                             sf3 = 3.75,
                                                             Support = c("Respondent's Municipality Supports"),
                                                             tier_n = c("N")),
                                          by = c("Support", "sf3", "first_answer_factor"),
                                          gray = T,
                                          conf_level = 0.84,
                                          draw = F) # Get the estimates & CIs when RAI high

plot2.supportN_mun.sf3 <- plot_predictions(m2.supportN.sf3,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sf3 = c(2.07, 3.75),
                                                              Support = c("Respondent's Municipality Supports"),
                                                              tier_n = c("N")),
                                           by = c("Support", "sf3", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.95,
                                           draw = F)


p2_n_m_sf3 <- ggplot(plot2.supportN_mun.sf3,
                     aes(x = first_answer_factor,
                         y = estimate,
                         group = first_answer_factor, shape = sf3)) +
  geom_hline(yintercept = c(1, 2, 3, 4), color='gray') + 
  facet_grid(.~Support) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = sf3),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_N_m_sf3 <- p2_n_m_sf3 +
  labs(x="",
       y="",
       shape="Preference for Self-Rule Item 3") +
  scale_shape_discrete(labels= c("Mean - 1SD (2.07)", "Mean + 1SD (3.75)")) +
  theme_classic()  + theme(legend.position = "bottom",
                           plot.title = element_text(hjust = 0.5, vjust = 1.5),
                           axis.title.y = element_text(size = 18, vjust = 4.5)) 

p2_N_m_sf3 <- p2_N_m_sf3 + geom_linerange(data=preds.railow.natdec.sf3,
                                          aes(y = estimate,
                                              x = first_answer_factor,
                                              ymin = conf.low,
                                              ymax = conf.high),
                                          position = position_nudge(x = -0.05),
                                          linewidth = 1.2) +
  geom_linerange(data = preds.high.natdec.sf3,
                 aes(y = estimate,
                     x = first_answer_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)

my.plot.nat.1.sf3 <- p2_N_m_sf3  + labs(tag = "g:") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        axis.title.y = element_text(size=14),
        strip.text.x = element_text(size = 11),
        plot.tag.position = c(0.16, 0.974)) 

my.plot.nat.1.sf3

###### Figure E2Ch: National government decides to close a department of a university (college) and county supports ####

preds.low.natdec.2.sf3 <- plot_predictions(m2.supportN.sf3,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sf3 = 2.07,
                                                              Support = c("Respondent's County Supports"),
                                                              tier_n = c("N")),
                                           by = c("Support", "sf3", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.84,
                                           draw = F) # Get the estimates & CIs when RAI low

preds.high.natdec.2.sf3 <- plot_predictions(m2.supportN.sf3,
                                            newdata = datagrid(first_answer_factor = 1:4,
                                                               sf3 = 3.75,
                                                               Support = c("Respondent's County Supports"),
                                                               tier_n = c("N")),
                                            by = c("Support", "sf3", "first_answer_factor"),
                                            gray = T,
                                            conf_level = 0.84,
                                            draw = F) # Get the estimates & CIs when RAI high

plot2.supportN_cou.sf3 <- plot_predictions(m2.supportN.sf3,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sf3 = c(2.07, 3.75),
                                                              Support = c("Respondent's County Supports"),
                                                              tier_n = c("N")),
                                           by = c("Support", "sf3", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.95,
                                           draw = F)


p2_n_c_sf3 <- ggplot(plot2.supportN_cou.sf3,
                     aes(x = first_answer_factor,
                         y = estimate,
                         group = first_answer_factor, shape = sf3)) +
  geom_hline(yintercept = c(1, 2, 3, 4), color='gray') + 
  facet_grid(.~Support) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = sf3),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_N_c_sf3 <- p2_n_c_sf3 +
  labs(x="Willingness to accept initial decision",
       y="",
       shape="Preference for Self-Rule Item 3") +
  scale_shape_discrete(labels= c("Mean - 1SD (2.07)", "Mean + 1SD (3.75)")) +
  theme_classic() + theme(legend.position = "bottom",
                          plot.title = element_text(hjust = 0.5, vjust = 1.5),
                          axis.title.x = element_text(vjust = -1))


p2_N_c_sf3 <- p2_N_c_sf3 + geom_linerange(data=preds.low.natdec.2.sf3,
                                          aes(y = estimate,
                                              x = first_answer_factor,
                                              ymin = conf.low,
                                              ymax = conf.high),
                                          position = position_nudge(x = -0.05),
                                          linewidth = 1.2) +
  geom_linerange(data = preds.high.natdec.2.sf3,
                 aes(y = estimate,
                     x = first_answer_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)

my.plot.nat.2.sf3 <- p2_N_c_sf3  + labs(tag = "h:") +
  theme(axis.text = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 16),
        strip.text.x = element_text(size = 11),
        plot.tag.position = c(0.16, 0.972))

my.plot.nat.2.sf3

###### Figure E2Ci: National government decides to close a department of a university (college) and municipality and county support ####

preds.low.natdec.3.sf3 <- plot_predictions(m2.supportN.sf3,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sf3 = 2.07,
                                                              Support = c("Municipality & County Support"),
                                                              tier_n = c("N")),
                                           by = c("Support", "sf3", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.84,
                                           draw = F) # Get the estimates & CIs when RAI low

preds.high.natdec.3.sf3 <- plot_predictions(m2.supportN.sf3,
                                            newdata = datagrid(first_answer_factor = 1:4,
                                                               sf3 = 3.75,
                                                               Support = c("Municipality & County Support"),
                                                               tier_n = c("N")),
                                            by = c("Support", "sf3", "first_answer_factor"),
                                            gray = T,
                                            conf_level = 0.84,
                                            draw = F) # Get the estimates & CIs when RAI high

plot2.supportN_muncou.sf3 <- plot_predictions(m2.supportN.sf3,
                                              newdata = datagrid(first_answer_factor = 1:4,
                                                                 sf3 = c(2.07, 3.75),
                                                                 Support = c("Municipality & County Support"),
                                                                 tier_n = c("N")),
                                              by = c("Support", "sf3", "first_answer_factor"),
                                              gray = T,
                                              conf_level = 0.95,
                                              draw = F)


p2_n_mc_sf3 <- ggplot(plot2.supportN_muncou.sf3,
                      aes(x = first_answer_factor,
                          y = estimate,
                          group = first_answer_factor, shape = sf3)) +
  geom_hline(yintercept = c(1, 2, 3, 4), color='gray') + 
  facet_grid(.~Support) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = sf3),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_N_mc_sf3 <- p2_n_mc_sf3 +
  labs(x="", y="",
       shape="Preference for Self-Rule Item 3") +
  scale_shape_discrete(labels= c("Mean - 1SD (2.07)", "Mean + 1SD (3.75)")) +
  theme_classic() + theme(legend.position = "bottom",
                          plot.title = element_text(hjust = 0.5, vjust = 1.5))


p2_N_mc_sf3 <- p2_N_mc_sf3 + geom_linerange(data=preds.low.natdec.3.sf3,
                                            aes(y = estimate,
                                                x = first_answer_factor,
                                                ymin = conf.low,
                                                ymax = conf.high),
                                            position = position_nudge(x = -0.05),
                                            linewidth = 1.2) +
  geom_linerange(data = preds.high.natdec.3.sf3,
                 aes(y = estimate,
                     x = first_answer_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)

my.plot.nat.3.sf3 <- p2_N_mc_sf3  + labs(tag = "i:") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        strip.text.x = element_text(size = 11),
        plot.tag.position = c(0.16, 0.972))

p3.sf3 <- ggarrange(my.plot.nat.1.sf3, my.plot.nat.2.sf3, my.plot.nat.3.sf3, nrow = 1, ncol = 3,
                    common.legend = T, legend = "bottom")

p3.sf3 <- p3.sf3 + plot_annotation(title = "\nNational government decides to close \na department of a university (college)") &
  theme(plot.title = element_text(hjust = .5, size = 18),
        legend.title = element_text(size = 16),
        legend.text = element_text(size=15))

#Gather all SF3 plots together:
my.plot.all.sf3 <- ggarrange(p1.sf3, p2.sf3, p3.sf3, nrow = 3, ncol = 1)

# Save Figure E2C as a .jpeg file:
ggsave("Figure_E2C.jpeg",
       width = 12, height = 17)


#### Figure E2D. Model 2-Q2-Q4-Q6: Preference for shared rule item 4. ####

##### Figures E2Da-E2Dc: Municipality decides to close kindergarten. ####

mod.sh1.mun.support <- feols(second_answer_original ~ first_answer_factor * sh1 * support_m + tier_m | responseid,
                             data = dataset)
summary(mod.sh1.mun.support)

###### Figure E2Da: Municipality decides to close kindergarten and county supports ####

preds.railow.mundec.sh1 <- plot_predictions(mod.sh1.mun.support,
                                            newdata = datagrid(first_answer_factor = 1:4,
                                                               sh1 = 2.28,
                                                               support_m = c("Respondent's County Supports"),
                                                               tier_m = c("M")),
                                            by = c("support_m", "sh1", "first_answer_factor"),
                                            gray = T,
                                            conf_level = 0.84,
                                            draw = F) # Get the estimates & CIs when RAI low

preds.high.mundec.sh1 <- plot_predictions(mod.sh1.mun.support,
                                          newdata = datagrid(first_answer_factor = 1:4,
                                                             sh1 = 3.85,
                                                             support_m = c("Respondent's County Supports"),
                                                             tier_m = c("M")),
                                          by = c("support_m", "sh1", "first_answer_factor"),
                                          gray = T,
                                          conf_level = 0.84,
                                          draw = F) # Get the estimates & CIs when RAI high

plot2.supportM_cou.sh1 <- plot_predictions(mod.sh1.mun.support,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sh1 = c(2.28, 3.85),
                                                              support_m = c("Respondent's County Supports"),
                                                              tier_m = c("M")),
                                           by = c("support_m", "sh1", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.95,
                                           draw = F)



p2_m_c_sh1 <- ggplot(plot2.supportM_cou.sh1,
                     aes(x = first_answer_factor,
                         y = estimate,
                         group = first_answer_factor, shape = sh1)) +
  geom_hline(yintercept = c(1, 2, 3, 4), color='gray') + 
  facet_grid(.~support_m) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = sh1),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_M_c_sh1 <- p2_m_c_sh1 +
  labs(x="",
       y="",
       shape="Preference for Shared Rule Item 4") +
  scale_shape_discrete(labels= c("Mean - 1SD (2.28)", "Mean + 1SD (3.85)")) +
  theme_classic()  + theme(legend.position = "bottom",
                           plot.title = element_text(hjust = 0.5, vjust = 1.5))


p2_M_c_sh1 <- p2_M_c_sh1 + geom_linerange(data=preds.railow.mundec.sh1,
                                          aes(y = estimate,
                                              x = first_answer_factor,
                                              ymin = conf.low,
                                              ymax = conf.high),
                                          position = position_nudge(x = -0.05),
                                          linewidth = 1.2) +
  geom_linerange(data = preds.high.mundec.sh1,
                 aes(y = estimate,
                     x = first_answer_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)

my.plot.mun.1.sh1 <- p2_M_c_sh1  + labs(tag = "a:") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        axis.title.y = element_text(size=14),
        strip.text.x = element_text(size = 11),
        plot.tag.position = c(0.16, 0.974))

my.plot.mun.1.sh1

###### Figure E2Db: Municipality decides to close kindergarten and national government supports ####

preds.low.mundec.2.sh1 <- plot_predictions(mod.sh1.mun.support,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sh1 = 2.28,
                                                              support_m = c("National Government Supports"),
                                                              tier_m = c("M")),
                                           by = c("support_m", "sh1", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.84,
                                           draw = F) # Get the estimates & CIs when RAI low

preds.high.mundec.2.sh1 <- plot_predictions(mod.sh1.mun.support,
                                            newdata = datagrid(first_answer_factor = 1:4,
                                                               sh1 = 3.85,
                                                               support_m = c("National Government Supports"),
                                                               tier_m = c("M")),
                                            by = c("support_m", "sh1", "first_answer_factor"),
                                            gray = T,
                                            conf_level = 0.84,
                                            draw = F) # Get the estimates & CIs when RAI high

plot2.supportM_nat.sh1 <- plot_predictions(mod.sh1.mun.support,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sh1 = c(2.28, 3.85),
                                                              support_m = c("National Government Supports"),
                                                              tier_m = c("M")),
                                           by = c("support_m", "sh1", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.95,
                                           draw = F)


p2_m_n_sh1 <- ggplot(plot2.supportM_nat.sh1,
                     aes(x = first_answer_factor,
                         y = estimate,
                         group = first_answer_factor, shape = sh1)) +
  geom_hline(yintercept = c(1, 2, 3, 4), color='gray') + 
  facet_grid(.~support_m) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = sh1),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_M_n_sh1 <- p2_m_n_sh1 +
  labs(x="",
       y="",
       shape="Preference for Shared Rule Item 4") +
  scale_shape_discrete(labels= c("Mean - 1SD (2.28)", "Mean + 1SD (3.85)")) +
  theme_classic() + theme(legend.position = "bottom",
                          plot.title = element_text(hjust = 0.5, vjust = 1.5))


p2_M_n_sh1 <- p2_M_n_sh1 + geom_linerange(data=preds.low.mundec.2.sh1,
                                          aes(y = estimate,
                                              x = first_answer_factor,
                                              ymin = conf.low,
                                              ymax = conf.high),
                                          position = position_nudge(x = -0.05),
                                          linewidth = 1.2) +
  geom_linerange(data = preds.high.mundec.2.sh1,
                 aes(y = estimate,
                     x = first_answer_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)



my.plot.mun.2.sh1 <- p2_M_n_sh1  + labs(tag = "b:") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        strip.text.x = element_text(size = 11),
        plot.tag.position = c(0.16, 0.974)) + annotate("text", x=1, y=2.05,
                                                           label="*", size=8)

my.plot.mun.2.sh1

###### Figure E2Dc: Municipality decides to close kindergarten and county and national government support ####

preds.low.mundec.3.sh1 <- plot_predictions(mod.sh1.mun.support,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sh1 = 2.28,
                                                              support_m = c("County & National Government Support"),
                                                              tier_m = c("M")),
                                           by = c("support_m", "sh1", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.84,
                                           draw = F) # Get the estimates & CIs when sh1 low

preds.high.mundec.3.sh1 <- plot_predictions(mod.sh1.mun.support,
                                            newdata = datagrid(first_answer_factor = 1:4,
                                                               sh1 = 3.85,
                                                               support_m = c("County & National Government Support"),
                                                               tier_m = c("M")),
                                            by = c("support_m", "sh1", "first_answer_factor"),
                                            gray = T,
                                            conf_level = 0.84,
                                            draw = F) # Get the estimates & CIs when RAI high

plot2.supportM_counat.sh1 <- plot_predictions(mod.sh1.mun.support,
                                              newdata = datagrid(first_answer_factor = 1:4,
                                                                 sh1 = c(2.28, 3.85),
                                                                 support_m = c("County & National Government Support"),
                                                                 tier_m = c("M")),
                                              by = c("support_m", "sh1", "first_answer_factor"),
                                              gray = T,
                                              conf_level = 0.95,
                                              draw = F)


p2_m_cn_sh1 <- ggplot(plot2.supportM_counat.sh1,
                      aes(x = first_answer_factor,
                          y = estimate,
                          group = first_answer_factor, shape = sh1)) +
  geom_hline(yintercept = c(1, 2, 3, 4), color='gray') + 
  facet_grid(.~support_m) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = sh1),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_M_cn_sh1 <- p2_m_cn_sh1 +
  labs(x="", y="",
       shape="Preference for Shared Rule Item 4") +
  scale_shape_discrete(labels= c("Mean - 1SD (2.28)", "Mean + 1SD (3.85)")) +
  theme_classic() + theme(legend.position = "bottom",
                          plot.title = element_text(hjust = 0.5, vjust = 1.5))


p2_M_cn_sh1 <- p2_M_cn_sh1 + geom_linerange(data=preds.low.mundec.3.sh1,
                                            aes(y = estimate,
                                                x = first_answer_factor,
                                                ymin = conf.low,
                                                ymax = conf.high),
                                            position = position_nudge(x = -0.05),
                                            linewidth = 1.2) +
  geom_linerange(data = preds.high.mundec.3.sh1,
                 aes(y = estimate,
                     x = first_answer_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)


my.plot.mun.3.sh1 <- p2_M_cn_sh1  + labs(tag = "c:") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        strip.text.x = element_text(size = 11),
        plot.tag.position = c(0.16, 0.974))

my.plot.mun.3.sh1

p1.sh1 <- ggarrange(my.plot.mun.1.sh1, my.plot.mun.2.sh1, my.plot.mun.3.sh1, nrow = 1, ncol = 3,
                    common.legend = T, legend = "none") + plot_annotation(title="Municipality decides to close kindergarten") &
  theme(plot.title = element_text(hjust = .5, size = 18), legend.text = element_blank())


##### Figures E2Dd-E2Df: County decides to close upper secondary school #####
m2.supportC.sh1 <- feols(second_answer_original ~ first_answer_factor * sh1 * Support_c + tier_c | responseid,
                         data = dataset)
summary(m2.supportC.sh1)

###### Figure E2Dd: County decides to close upper secondary school and municipality supports ####
preds.railow.coudec.sh1 <- plot_predictions(m2.supportC.sh1,
                                            newdata = datagrid(first_answer_factor = 1:4,
                                                               sh1 = 2.28,
                                                               Support_c = c("Respondent's Municipality Supports"),
                                                               tier_c = c("C")),
                                            by = c("Support_c", "sh1", "first_answer_factor"),
                                            gray = T,
                                            conf_level = 0.84,
                                            draw = F) # Get the estimates & CIs when RAI low

preds.high.coudec.sh1 <- plot_predictions(m2.supportC.sh1,
                                          newdata = datagrid(first_answer_factor = 1:4,
                                                             sh1 = 3.85,
                                                             Support_c = c("Respondent's Municipality Supports"),
                                                             tier_c = c("C")),
                                          by = c("Support_c", "sh1", "first_answer_factor"),
                                          gray = T,
                                          conf_level = 0.84,
                                          draw = F) # Get the estimates & CIs when RAI high

plot2.supportC_mun.sh1 <- plot_predictions(m2.supportC.sh1,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sh1 = c(2.28, 3.85),
                                                              Support_c = c("Respondent's Municipality Supports"),
                                                              tier_c = c("C")),
                                           by = c("Support_c", "sh1", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.95,
                                           draw = F)


p2_c_m_sh1 <- ggplot(plot2.supportC_mun.sh1,
                     aes(x = first_answer_factor,
                         y = estimate,
                         group = first_answer_factor, shape = sh1)) +
  geom_hline(yintercept = c(1, 2, 3, 4), color='gray') + 
  facet_grid(.~Support_c) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = sh1),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_C_m_sh1 <- p2_c_m_sh1 +
  labs(x="",
       y="Willingness to accept decision\nwhen support is provided by another tier",
       shape="Preference for Shared Rule Item 4") +
  scale_shape_discrete(labels= c("Mean - 1SD (2.28)", "Mean + 1SD (3.85)")) +
  theme_classic()  + theme(legend.position = "bottom",
                           plot.title = element_text(hjust = 0.5, vjust = 1.5),
                           axis.title.y = element_text(size = 18, vjust = 4.5)) 


p2_C_m_sh1 <- p2_C_m_sh1 + geom_linerange(data=preds.railow.coudec.sh1,
                                          aes(y = estimate,
                                              x = first_answer_factor,
                                              ymin = conf.low,
                                              ymax = conf.high),
                                          position = position_nudge(x = -0.05),
                                          linewidth = 1.2) +
  geom_linerange(data = preds.high.coudec.sh1,
                 aes(y = estimate,
                     x = first_answer_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)


my.plot.cou.1.sh1 <- p2_C_m_sh1  + labs(tag = "d:") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        axis.title.y = element_text(size=14),
        strip.text.x = element_text(size = 11),
        plot.tag.position = c(0.19, 0.974))

my.plot.cou.1.sh1

###### Figure E2De: County decides to close upper secondary school and national government supports ####

preds.low.coudec.2.sh1 <- plot_predictions(m2.supportC.sh1,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sh1 = 2.28,
                                                              Support_c = c("National Government Supports"),
                                                              tier_c = c("C")),
                                           by = c("Support_c", "sh1", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.84,
                                           draw = F) # Get the estimates & CIs when RAI low

preds.high.coudec.2.sh1 <- plot_predictions(m2.supportC.sh1,
                                            newdata = datagrid(first_answer_factor = 1:4,
                                                               sh1 = 3.85,
                                                               Support_c = c("National Government Supports"),
                                                               tier_c = c("C")),
                                            by = c("Support_c", "sh1", "first_answer_factor"),
                                            gray = T,
                                            conf_level = 0.84,
                                            draw = F) # Get the estimates & CIs when RAI high

plot2.supportC_nat.sh1 <- plot_predictions(m2.supportC.sh1,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sh1 = c(2.28, 3.85),
                                                              Support_c = c("National Government Supports"),
                                                              tier_c = c("C")),
                                           by = c("Support_c", "sh1", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.95,
                                           draw = F)


p2_c_n_sh1 <- ggplot(plot2.supportC_nat.sh1,
                     aes(x = first_answer_factor,
                         y = estimate,
                         group = first_answer_factor, shape = sh1)) +
  geom_hline(yintercept = c(1, 2, 3, 4), color='gray') + 
  facet_grid(.~Support_c) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = sh1),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_C_n_sh1 <- p2_c_n_sh1 +
  labs(x="",
       y="",
       shape="Preference for Shared Rule Item 4") +
  scale_shape_discrete(labels= c("Mean - 1SD (2.28)", "Mean + 1SD (3.85)")) +
  theme_classic() + theme(legend.position = "bottom",
                          plot.title = element_text(hjust = 0.5, vjust = 1.5))


p2_C_n_sh1 <- p2_C_n_sh1 + geom_linerange(data=preds.low.coudec.2.sh1,
                                          aes(y = estimate,
                                              x = first_answer_factor,
                                              ymin = conf.low,
                                              ymax = conf.high),
                                          position = position_nudge(x = -0.05),
                                          linewidth = 1.2) +
  geom_linerange(data = preds.high.coudec.2.sh1,
                 aes(y = estimate,
                     x = first_answer_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)

my.plot.cou.2.sh1 <- p2_C_n_sh1  + labs(tag = "e:") +
  theme(axis.text = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 16),
        strip.text.x = element_text(size = 11),
        plot.tag.position = c(0.16, 0.974)) + annotate("text", x=1, y=2.05,
                                                           label='**', size=8)

my.plot.cou.2.sh1

###### Figure E2Df: County decides to close upper secondary school and municipality and national government support ####

preds.low.coudec.3.sh1 <- plot_predictions(m2.supportC.sh1,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sh1 = 2.28,
                                                              Support_c = c("Municipality & National Government Support"),
                                                              tier_c = c("C")),
                                           by = c("Support_c", "sh1", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.84,
                                           draw = F) # Get the estimates & CIs when RAI low

preds.high.coudec.3.sh1 <- plot_predictions(m2.supportC.sh1,
                                            newdata = datagrid(first_answer_factor = 1:4,
                                                               sh1 = 3.85,
                                                               Support_c = c("Municipality & National Government Support"),
                                                               tier_c = c("C")),
                                            by = c("Support_c", "sh1", "first_answer_factor"),
                                            gray = T,
                                            conf_level = 0.84,
                                            draw = F) # Get the estimates & CIs when RAI high

plot2.supportC_munnat.sh1 <- plot_predictions(m2.supportC.sh1,
                                              newdata = datagrid(first_answer_factor = 1:4,
                                                                 sh1 = c(2.28, 3.85),
                                                                 Support_c = c("Municipality & National Government Support"),
                                                                 tier_c = c("C")),
                                              by = c("Support_c", "sh1", "first_answer_factor"),
                                              gray = T,
                                              conf_level = 0.95,
                                              draw = F)


p2_c_mn_sh1 <- ggplot(plot2.supportC_munnat.sh1,
                      aes(x = first_answer_factor,
                          y = estimate,
                          group = first_answer_factor, shape = sh1)) +
  geom_hline(yintercept = c(1, 2, 3, 4), color='gray') + 
  facet_grid(.~Support_c) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = sh1),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_C_mn_sh1 <- p2_c_mn_sh1 +
  labs(x="", y="",
       shape="Preference for Shared Rule Item 4") +
  scale_shape_discrete(labels= c("Mean - 1SD (2.28)", "Mean + 1SD (3.85)")) +
  theme_classic() + theme(legend.position = "bottom",
                          plot.title = element_text(hjust = 0.5, vjust = 1.5))


p2_C_mn_sh1 <- p2_C_mn_sh1 + geom_linerange(data=preds.low.coudec.3.sh1,
                                            aes(y = estimate,
                                                x = first_answer_factor,
                                                ymin = conf.low,
                                                ymax = conf.high),
                                            position = position_nudge(x = -0.05),
                                            linewidth = 1.2) +
  geom_linerange(data = preds.high.coudec.3.sh1,
                 aes(y = estimate,
                     x = first_answer_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)



my.plot.cou.3.sh1 <- p2_C_mn_sh1  + labs(tag = "f:") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        strip.text.x = element_text(size = 11),
        plot.tag.position = c(0.134, 0.973)) 


p2.sh1 <- ggarrange(my.plot.cou.1.sh1, my.plot.cou.2.sh1, my.plot.cou.3.sh1, nrow = 1, ncol = 3,
                    common.legend = T, legend = "none") 

p2.sh1 <- p2.sh1 + plot_annotation(title = "\nCounty decides to close upper secondary school") &
  theme(plot.title = element_text(hjust = .5, size = 18))


##### Figures E2Dg-E2Di: National government decides to close a department of a university (college) #####
m2.supportN.sh1 <- feols(second_answer_original ~ first_answer_factor * sh1 * Support + tier_n | responseid,
                         data = dataset)
summary(m2.supportN.sh1)

###### Figure E2Dg: National government decides to close a department of a university (college) and municipality supports #####
preds.railow.natdec.sh1 <- plot_predictions(m2.supportN.sh1,
                                            newdata = datagrid(first_answer_factor = 1:4,
                                                               sh1 = 2.28,
                                                               Support = c("Respondent's Municipality Supports"),
                                                               tier_n = c("N")),
                                            by = c("Support", "sh1", "first_answer_factor"),
                                            gray = T,
                                            conf_level = 0.84,
                                            draw = F) # Get the estimates & CIs when RAI low

preds.high.natdec.sh1 <- plot_predictions(m2.supportN.sh1,
                                          newdata = datagrid(first_answer_factor = 1:4,
                                                             sh1 = 3.85,
                                                             Support = c("Respondent's Municipality Supports"),
                                                             tier_n = c("N")),
                                          by = c("Support", "sh1", "first_answer_factor"),
                                          gray = T,
                                          conf_level = 0.84,
                                          draw = F) # Get the estimates & CIs when RAI high

plot2.supportN_mun.sh1 <- plot_predictions(m2.supportN.sh1,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sh1 = c(2.28, 3.85),
                                                              Support = c("Respondent's Municipality Supports"),
                                                              tier_n = c("N")),
                                           by = c("Support", "sh1", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.95,
                                           draw = F)


p2_n_m_sh1 <- ggplot(plot2.supportN_mun.sh1,
                     aes(x = first_answer_factor,
                         y = estimate,
                         group = first_answer_factor, shape = sh1)) +
  geom_hline(yintercept = c(1, 2, 3, 4), color='gray') + 
  facet_grid(.~Support) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = sh1),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_N_m_sh1 <- p2_n_m_sh1 +
  labs(x="",
       y="",
       shape="Preference for Shared Rule Item 4") +
  scale_shape_discrete(labels= c("Mean - 1SD (2.28)", "Mean + 1SD (3.85)")) +
  theme_classic()  + theme(legend.position = "bottom",
                           plot.title = element_text(hjust = 0.5, vjust = 1.5),
                           axis.title.y = element_text(size = 18, vjust = 4.5)) 


p2_N_m_sh1 <- p2_N_m_sh1 + geom_linerange(data=preds.railow.natdec.sh1,
                                          aes(y = estimate,
                                              x = first_answer_factor,
                                              ymin = conf.low,
                                              ymax = conf.high),
                                          position = position_nudge(x = -0.05),
                                          linewidth = 1.2) +
  geom_linerange(data = preds.high.natdec.sh1,
                 aes(y = estimate,
                     x = first_answer_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2) + annotate('text', x=c(1,2,3), y=c(2.2,2.95,3.6),
                                             label=c("*", "**", "**"), size=8)


my.plot.nat.1.sh1 <- p2_N_m_sh1  + labs(tag = "g:") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        axis.title.y = element_text(size=14),
        strip.text.x = element_text(size = 11),
        plot.tag.position = c(0.16, 0.974)) 

my.plot.nat.1.sh1

###### Figure E2Dh: National government decides to close a department of a university (college) and county supports ####

preds.low.natdec.2.sh1 <- plot_predictions(m2.supportN.sh1,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sh1 = 2.28,
                                                              Support = c("Respondent's County Supports"),
                                                              tier_n = c("N")),
                                           by = c("Support", "sh1", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.84,
                                           draw = F) # Get the estimates & CIs when RAI low

preds.high.natdec.2.sh1 <- plot_predictions(m2.supportN.sh1,
                                            newdata = datagrid(first_answer_factor = 1:4,
                                                               sh1 = 3.85,
                                                               Support = c("Respondent's County Supports"),
                                                               tier_n = c("N")),
                                            by = c("Support", "sh1", "first_answer_factor"),
                                            gray = T,
                                            conf_level = 0.84,
                                            draw = F) # Get the estimates & CIs when RAI high

plot2.supportN_cou.sh1 <- plot_predictions(m2.supportN.sh1,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sh1 = c(2.28, 3.85),
                                                              Support = c("Respondent's County Supports"),
                                                              tier_n = c("N")),
                                           by = c("Support", "sh1", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.95,
                                           draw = F)


p2_n_c_sh1 <- ggplot(plot2.supportN_cou.sh1,
                     aes(x = first_answer_factor,
                         y = estimate,
                         group = first_answer_factor, shape = sh1)) +
  geom_hline(yintercept = c(1, 2, 3, 4), color='gray') + 
  facet_grid(.~Support) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = sh1),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_N_c_sh1 <- p2_n_c_sh1 +
  labs(x="Willingness to accept initial decision",
       y="",
       shape="Preference for Shared Rule Item 4") +
  scale_shape_discrete(labels= c("Mean - 1SD (2.28)", "Mean + 1SD (3.85)")) +
  theme_classic() + theme(legend.position = "bottom",
                          plot.title = element_text(hjust = 0.5, vjust = 1.5),
                          axis.title.x = element_text(vjust = -1))


p2_N_c_sh1 <- p2_N_c_sh1 + geom_linerange(data=preds.low.natdec.2.sh1,
                                          aes(y = estimate,
                                              x = first_answer_factor,
                                              ymin = conf.low,
                                              ymax = conf.high),
                                          position = position_nudge(x = -0.05),
                                          linewidth = 1.2) +
  geom_linerange(data = preds.high.natdec.2.sh1,
                 aes(y = estimate,
                     x = first_answer_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2) + annotate('text', x=c(1,2,3), y=c(2.1,2.7,3.5),
                                             label=c("**", "**", "**"), size=8)

my.plot.nat.2.sh1 <- p2_N_c_sh1  + labs(tag = "h:") +
  theme(axis.text = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 16),
        strip.text.x = element_text(size = 11),
        plot.tag.position = c(0.16, 0.972))

my.plot.nat.2.sh1

###### Figure E2Di: National government decides to close a department of a university (college) and municipality and county support ####

preds.low.natdec.3.sh1 <- plot_predictions(m2.supportN.sh1,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sh1 = 2.28,
                                                              Support = c("Municipality & County Support"),
                                                              tier_n = c("N")),
                                           by = c("Support", "sh1", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.84,
                                           draw = F) # Get the estimates & CIs when RAI low

preds.high.natdec.3.sh1 <- plot_predictions(m2.supportN.sh1,
                                            newdata = datagrid(first_answer_factor = 1:4,
                                                               sh1 = 3.85,
                                                               Support = c("Municipality & County Support"),
                                                               tier_n = c("N")),
                                            by = c("Support", "sh1", "first_answer_factor"),
                                            gray = T,
                                            conf_level = 0.84,
                                            draw = F) # Get the estimates & CIs when RAI high

plot2.supportN_muncou.sh1 <- plot_predictions(m2.supportN.sh1,
                                              newdata = datagrid(first_answer_factor = 1:4,
                                                                 sh1 = c(2.28, 3.85),
                                                                 Support = c("Municipality & County Support"),
                                                                 tier_n = c("N")),
                                              by = c("Support", "sh1", "first_answer_factor"),
                                              gray = T,
                                              conf_level = 0.95,
                                              draw = F)


p2_n_mc_sh1 <- ggplot(plot2.supportN_muncou.sh1,
                      aes(x = first_answer_factor,
                          y = estimate,
                          group = first_answer_factor, shape = sh1)) +
  geom_hline(yintercept = c(1, 2, 3, 4), color='gray') + 
  facet_grid(.~Support) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = sh1),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_N_mc_sh1 <- p2_n_mc_sh1 +
  labs(x="", y="",
       shape="Preference for Shared Rule Item 4") +
  scale_shape_discrete(labels= c("Mean - 1SD (2.28)", "Mean + 1SD (3.85)")) +
  theme_classic() + theme(legend.position = "bottom",
                          plot.title = element_text(hjust = 0.5, vjust = 1.5))


p2_N_mc_sh1 <- p2_N_mc_sh1 + geom_linerange(data=preds.low.natdec.3.sh1,
                                            aes(y = estimate,
                                                x = first_answer_factor,
                                                ymin = conf.low,
                                                ymax = conf.high),
                                            position = position_nudge(x = -0.05),
                                            linewidth = 1.2) +
  geom_linerange(data = preds.high.natdec.3.sh1,
                 aes(y = estimate,
                     x = first_answer_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2) + annotate('text', x=c(1,2,3), y=c(2.4,3.05,3.7),
                                             label=c("**", "**", "**"), size=8)

my.plot.nat.3.sh1 <- p2_N_mc_sh1  + labs(tag = "i:") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        strip.text.x = element_text(size = 11),
        plot.tag.position = c(0.16, 0.972))

p3.sh1 <- ggarrange(my.plot.nat.1.sh1, my.plot.nat.2.sh1, my.plot.nat.3.sh1, nrow = 1, ncol = 3,
                    common.legend = T, legend = "bottom")

p3.sh1 <- p3.sh1 + plot_annotation(title = "\nNational government decides to close \na department of a university (college)") &
  theme(plot.title = element_text(hjust = .5, size = 18),
        legend.title = element_text(size = 16),
        legend.text = element_text(size=15))

#Gather the plots together:
my.plot.all.sh1 <- ggarrange(p1.sh1, p2.sh1, p3.sh1, nrow = 3, ncol = 1)

# Save Figure E2D as a .jpeg file:
ggsave("Figure_E2D.jpeg",
       width = 12, height = 17)


#### Figure E2E. Model 2-Q2-Q4-Q6: Preference for shared rule item 5. #####

# IMPORTANT NOTE: The point estimates are statistically significantly different
# from each other (p<0.05) when their 84% CIs do not overlap and the 95% CI of
# the DIFFERENCE between two point estimates does not include zero; see data and methods.

##### Figures E2Ea-E2Ec: Municipality decides to close kindergarten. ####

mod.sh2.mun.support <- feols(second_answer ~ first_answer_factor * sh2 * support_m + tier_m | responseid,
                             data = dataset)
summary(mod.sh2.mun.support)

###### Figure E2Ea: Municipality decides to close kindergarten and county supports ####
preds.railow.mundec.sh2 <- plot_predictions(mod.sh2.mun.support,
                                            newdata = datagrid(first_answer_factor = 1:4,
                                                               sh2 = 2.72,
                                                               support_m = c("Respondent's County Supports"),
                                                               tier_m = c("M")),
                                            by = c("support_m", "sh2", "first_answer_factor"),
                                            gray = T,
                                            conf_level = 0.84,
                                            draw = F) # Get the estimates & CIs when RAI low

preds.high.mundec.sh2 <- plot_predictions(mod.sh2.mun.support,
                                          newdata = datagrid(first_answer_factor = 1:4,
                                                             sh2 = 4,
                                                             support_m = c("Respondent's County Supports"),
                                                             tier_m = c("M")),
                                          by = c("support_m", "sh2", "first_answer_factor"),
                                          gray = T,
                                          conf_level = 0.84,
                                          draw = F) # Get the estimates & CIs when RAI high

plot2.supportM_cou.sh2 <- plot_predictions(mod.sh2.mun.support,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sh2 = c(2.72, 4),
                                                              support_m = c("Respondent's County Supports"),
                                                              tier_m = c("M")),
                                           by = c("support_m", "sh2", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.95,
                                           draw = F)

p2_m_c_sh2 <- ggplot(plot2.supportM_cou.sh2,
                     aes(x = first_answer_factor,
                         y = estimate,
                         group = first_answer_factor, shape = sh2)) +
  geom_hline(yintercept = c(1, 2, 3, 4), color='gray') + 
  facet_grid(.~support_m) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = sh2),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_M_c_sh2 <- p2_m_c_sh2 +
  labs(x="",
       y="",
       shape="Preference for Shared Rule Item 5") +
  scale_shape_discrete(labels= c("Mean - 1SD (2.72)", "Mean + 1SD (4)")) +
  theme_classic()  + theme(legend.position = "bottom",
                           plot.title = element_text(hjust = 0.5, vjust = 1.5))


p2_M_c_sh2 <- p2_M_c_sh2 + geom_linerange(data=preds.railow.mundec.sh2,
                                          aes(y = estimate,
                                              x = first_answer_factor,
                                              ymin = conf.low,
                                              ymax = conf.high),
                                          position = position_nudge(x = -0.05),
                                          linewidth = 1.2) +
  geom_linerange(data = preds.high.mundec.sh2,
                 aes(y = estimate,
                     x = first_answer_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)


my.plot.mun.1.sh2 <- p2_M_c_sh2  + labs(tag = "a:") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        axis.title.y = element_text(size=14),
        strip.text.x = element_text(size = 11),
        plot.tag.position = c(0.16, 0.974)) + annotate("text", x=3, y=3.45,
                                                           label="*", size=8)

my.plot.mun.1.sh2

###### Figure E2Eb: Municipality decides to close kindergarten and national government supports ####

preds.low.mundec.2.sh2 <- plot_predictions(mod.sh2.mun.support,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sh2 = 2.72,
                                                              support_m = c("National Government Supports"),
                                                              tier_m = c("M")),
                                           by = c("support_m", "sh2", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.84,
                                           draw = F) # Get the estimates & CIs when RAI low

preds.high.mundec.2.sh2 <- plot_predictions(mod.sh2.mun.support,
                                            newdata = datagrid(first_answer_factor = 1:4,
                                                               sh2 = 4,
                                                               support_m = c("National Government Supports"),
                                                               tier_m = c("M")),
                                            by = c("support_m", "sh2", "first_answer_factor"),
                                            gray = T,
                                            conf_level = 0.84,
                                            draw = F) # Get the estimates & CIs when RAI high

plot2.supportM_nat.sh2 <- plot_predictions(mod.sh2.mun.support,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sh2 = c(2.72, 4),
                                                              support_m = c("National Government Supports"),
                                                              tier_m = c("M")),
                                           by = c("support_m", "sh2", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.95,
                                           draw = F)


p2_m_n_sh2 <- ggplot(plot2.supportM_nat.sh2,
                     aes(x = first_answer_factor,
                         y = estimate,
                         group = first_answer_factor, shape = sh2)) +
  geom_hline(yintercept = c(1, 2, 3, 4), color='gray') + 
  facet_grid(.~support_m) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = sh2),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_M_n_sh2 <- p2_m_n_sh2 +
  labs(x="",
       y="",
       shape="Preference for Shared Rule Item 5") +
  scale_shape_discrete(labels= c("Mean - 1SD (2.72)", "Mean + 1SD (4)")) +
  theme_classic() + theme(legend.position = "bottom",
                          plot.title = element_text(hjust = 0.5, vjust = 1.5))


p2_M_n_sh2 <- p2_M_n_sh2 + geom_linerange(data=preds.low.mundec.2.sh2,
                                          aes(y = estimate,
                                              x = first_answer_factor,
                                              ymin = conf.low,
                                              ymax = conf.high),
                                          position = position_nudge(x = -0.05),
                                          linewidth = 1.2) +
  geom_linerange(data = preds.high.mundec.2.sh2,
                 aes(y = estimate,
                     x = first_answer_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)

my.plot.mun.2.sh2 <- p2_M_n_sh2  + labs(tag = "b:") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        strip.text.x = element_text(size = 11),
        plot.tag.position = c(0.16, 0.974))

my.plot.mun.2.sh2

###### Figure E2Ec: Municipality decides to close kindergarten and county and national government support ####

preds.low.mundec.3.sh2 <- plot_predictions(mod.sh2.mun.support,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sh2 = 2.72,
                                                              support_m = c("County & National Government Support"),
                                                              tier_m = c("M")),
                                           by = c("support_m", "sh2", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.84,
                                           draw = F) # Get the estimates & CIs when sh2 low

preds.high.mundec.3.sh2 <- plot_predictions(mod.sh2.mun.support,
                                            newdata = datagrid(first_answer_factor = 1:4,
                                                               sh2 = 4,
                                                               support_m = c("County & National Government Support"),
                                                               tier_m = c("M")),
                                            by = c("support_m", "sh2", "first_answer_factor"),
                                            gray = T,
                                            conf_level = 0.84,
                                            draw = F) # Get the estimates & CIs when RAI high

plot2.supportM_counat.sh2 <- plot_predictions(mod.sh2.mun.support,
                                              newdata = datagrid(first_answer_factor = 1:4,
                                                                 sh2 = c(2.72, 4),
                                                                 support_m = c("County & National Government Support"),
                                                                 tier_m = c("M")),
                                              by = c("support_m", "sh2", "first_answer_factor"),
                                              gray = T,
                                              conf_level = 0.95,
                                              draw = F)


p2_m_cn_sh2 <- ggplot(plot2.supportM_counat.sh2,
                      aes(x = first_answer_factor,
                          y = estimate,
                          group = first_answer_factor, shape = sh2)) +
  geom_hline(yintercept = c(1, 2, 3, 4), color='gray') + 
  facet_grid(.~support_m) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = sh2),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_M_cn_sh2 <- p2_m_cn_sh2 +
  labs(x="", y="",
       shape="Preference for Shared Rule Item 5") +
  scale_shape_discrete(labels= c("Mean - 1SD (2.72)", "Mean + 1SD (4)")) +
  theme_classic() + theme(legend.position = "bottom",
                          plot.title = element_text(hjust = 0.5, vjust = 1.5))


p2_M_cn_sh2 <- p2_M_cn_sh2 + geom_linerange(data=preds.low.mundec.3.sh2,
                                            aes(y = estimate,
                                                x = first_answer_factor,
                                                ymin = conf.low,
                                                ymax = conf.high),
                                            position = position_nudge(x = -0.05),
                                            linewidth = 1.2) +
  geom_linerange(data = preds.high.mundec.3.sh2,
                 aes(y = estimate,
                     x = first_answer_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)

my.plot.mun.3.sh2 <- p2_M_cn_sh2  + labs(tag = "c:") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        strip.text.x = element_text(size = 11),
        plot.tag.position = c(0.16, 0.974))

my.plot.mun.3.sh2

p1.sh2 <- ggarrange(my.plot.mun.1.sh2, my.plot.mun.2.sh2, my.plot.mun.3.sh2, nrow = 1, ncol = 3,
                    common.legend = T, legend = "none") + plot_annotation(title="Municipality decides to close kindergarten") &
  theme(plot.title = element_text(hjust = .5, size = 18), legend.text = element_blank())


##### Figures E2Ed-E2Ef: County decides to close upper secondary school. ####
m2.supportC.sh2 <- feols(second_answer ~ first_answer_factor * sh2 * Support_c + tier_c | responseid,
                         data = dataset)
summary(m2.supportC.sh2)

###### Figure E2Ed: County decides to close upper secondary school and municipality supports #####
preds.railow.coudec.sh2 <- plot_predictions(m2.supportC.sh2,
                                            newdata = datagrid(first_answer_factor = 1:4,
                                                               sh2 = 2.72,
                                                               Support_c = c("Respondent's Municipality Supports"),
                                                               tier_c = c("C")),
                                            by = c("Support_c", "sh2", "first_answer_factor"),
                                            gray = T,
                                            conf_level = 0.84,
                                            draw = F) # Get the estimates & CIs when RAI low

preds.high.coudec.sh2 <- plot_predictions(m2.supportC.sh2,
                                          newdata = datagrid(first_answer_factor = 1:4,
                                                             sh2 = 4,
                                                             Support_c = c("Respondent's Municipality Supports"),
                                                             tier_c = c("C")),
                                          by = c("Support_c", "sh2", "first_answer_factor"),
                                          gray = T,
                                          conf_level = 0.84,
                                          draw = F) # Get the estimates & CIs when RAI high

plot2.supportC_mun.sh2 <- plot_predictions(m2.supportC.sh2,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sh2 = c(2.72, 4),
                                                              Support_c = c("Respondent's Municipality Supports"),
                                                              tier_c = c("C")),
                                           by = c("Support_c", "sh2", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.95,
                                           draw = F)


p2_c_m_sh2 <- ggplot(plot2.supportC_mun.sh2,
                     aes(x = first_answer_factor,
                         y = estimate,
                         group = first_answer_factor, shape = sh2)) +
  geom_hline(yintercept = c(1, 2, 3, 4), color='gray') + 
  facet_grid(.~Support_c) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = sh2),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_C_m_sh2 <- p2_c_m_sh2 +
  labs(x="",
       y="Willingness to accept decision\nwhen support is provided by another tier",
       shape="Preference for Shared Rule Item 5") +
  scale_shape_discrete(labels= c("Mean - 1SD (2.72)", "Mean + 1SD (4)")) +
  theme_classic()  + theme(legend.position = "bottom",
                           plot.title = element_text(hjust = 0.5, vjust = 1.5),
                           axis.title.y = element_text(size = 18, vjust = 4.5)) 



p2_C_m_sh2 <- p2_C_m_sh2 + geom_linerange(data=preds.railow.coudec.sh2,
                                          aes(y = estimate,
                                              x = first_answer_factor,
                                              ymin = conf.low,
                                              ymax = conf.high),
                                          position = position_nudge(x = -0.05),
                                          linewidth = 1.2) +
  geom_linerange(data = preds.high.coudec.sh2,
                 aes(y = estimate,
                     x = first_answer_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)



my.plot.cou.1.sh2 <- p2_C_m_sh2  + labs(tag = "d:") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        axis.title.y = element_text(size=14),
        strip.text.x = element_text(size = 11),
        plot.tag.position = c(0.19, 0.974))

my.plot.cou.1.sh2

###### Figure E2Ee: County decides to close upper secondary school and national government supports ####

preds.low.coudec.2.sh2 <- plot_predictions(m2.supportC.sh2,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sh2 = 2.72,
                                                              Support_c = c("National Government Supports"),
                                                              tier_c = c("C")),
                                           by = c("Support_c", "sh2", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.84,
                                           draw = F) # Get the estimates & CIs when RAI low

preds.high.coudec.2.sh2 <- plot_predictions(m2.supportC.sh2,
                                            newdata = datagrid(first_answer_factor = 1:4,
                                                               sh2 = 4,
                                                               Support_c = c("National Government Supports"),
                                                               tier_c = c("C")),
                                            by = c("Support_c", "sh2", "first_answer_factor"),
                                            gray = T,
                                            conf_level = 0.84,
                                            draw = F) # Get the estimates & CIs when RAI high

plot2.supportC_nat.sh2 <- plot_predictions(m2.supportC.sh2,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sh2 = c(2.72, 4),
                                                              Support_c = c("National Government Supports"),
                                                              tier_c = c("C")),
                                           by = c("Support_c", "sh2", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.95,
                                           draw = F)


p2_c_n_sh2 <- ggplot(plot2.supportC_nat.sh2,
                     aes(x = first_answer_factor,
                         y = estimate,
                         group = first_answer_factor, shape = sh2)) +
  geom_hline(yintercept = c(1, 2, 3, 4), color='gray') + 
  facet_grid(.~Support_c) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = sh2),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_C_n_sh2 <- p2_c_n_sh2 +
  labs(x="",
       y="",
       shape="Preference for Shared Rule Item 5") +
  scale_shape_discrete(labels= c("Mean - 1SD (2.72)", "Mean + 1SD (4)")) +
  theme_classic() + theme(legend.position = "bottom",
                          plot.title = element_text(hjust = 0.5, vjust = 1.5))


p2_C_n_sh2 <- p2_C_n_sh2 + geom_linerange(data=preds.low.coudec.2.sh2,
                                          aes(y = estimate,
                                              x = first_answer_factor,
                                              ymin = conf.low,
                                              ymax = conf.high),
                                          position = position_nudge(x = -0.05),
                                          linewidth = 1.2) +
  geom_linerange(data = preds.high.coudec.2.sh2,
                 aes(y = estimate,
                     x = first_answer_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)


my.plot.cou.2.sh2 <- p2_C_n_sh2  + labs(tag = "e:") +
  theme(axis.text = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 16),
        strip.text.x = element_text(size = 11),
        plot.tag.position = c(0.16, 0.974))

my.plot.cou.2.sh2

###### Figure E2Ef: County decides to close upper secondary school and municipality and national government support ####

preds.low.coudec.3.sh2 <- plot_predictions(m2.supportC.sh2,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sh2 = 2.72,
                                                              Support_c = c("Municipality & National Government Support"),
                                                              tier_c = c("C")),
                                           by = c("Support_c", "sh2", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.84,
                                           draw = F) # Get the estimates & CIs when RAI low

preds.high.coudec.3.sh2 <- plot_predictions(m2.supportC.sh2,
                                            newdata = datagrid(first_answer_factor = 1:4,
                                                               sh2 = 4,
                                                               Support_c = c("Municipality & National Government Support"),
                                                               tier_c = c("C")),
                                            by = c("Support_c", "sh2", "first_answer_factor"),
                                            gray = T,
                                            conf_level = 0.84,
                                            draw = F) # Get the estimates & CIs when RAI high

plot2.supportC_munnat.sh2 <- plot_predictions(m2.supportC.sh2,
                                              newdata = datagrid(first_answer_factor = 1:4,
                                                                 sh2 = c(2.72, 4),
                                                                 Support_c = c("Municipality & National Government Support"),
                                                                 tier_c = c("C")),
                                              by = c("Support_c", "sh2", "first_answer_factor"),
                                              gray = T,
                                              conf_level = 0.95,
                                              draw = F)


p2_c_mn_sh2 <- ggplot(plot2.supportC_munnat.sh2,
                      aes(x = first_answer_factor,
                          y = estimate,
                          group = first_answer_factor, shape = sh2)) +
  geom_hline(yintercept = c(1, 2, 3, 4), color='gray') + 
  facet_grid(.~Support_c) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = sh2),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_C_mn_sh2 <- p2_c_mn_sh2 +
  labs(x="", y="",
       shape="Preference for Shared Rule Item 5") +
  scale_shape_discrete(labels= c("Mean - 1SD (2.72)", "Mean + 1SD (4)")) +
  theme_classic() + theme(legend.position = "bottom",
                          plot.title = element_text(hjust = 0.5, vjust = 1.5))


p2_C_mn_sh2 <- p2_C_mn_sh2 + geom_linerange(data=preds.low.coudec.3.sh2,
                                            aes(y = estimate,
                                                x = first_answer_factor,
                                                ymin = conf.low,
                                                ymax = conf.high),
                                            position = position_nudge(x = -0.05),
                                            linewidth = 1.2) +
  geom_linerange(data = preds.high.coudec.3.sh2,
                 aes(y = estimate,
                     x = first_answer_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)



my.plot.cou.3.sh2 <- p2_C_mn_sh2  + labs(tag = "f:") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        axis.title.y = element_text(size = 14),
        strip.text.x = element_text(size = 11),
        plot.tag.position = c(0.13, 0.974)) 


p2.sh2 <- ggarrange(my.plot.cou.1.sh2, my.plot.cou.2.sh2, my.plot.cou.3.sh2, nrow = 1, ncol = 3,
                    common.legend = T, legend = "none") 

p2.sh2 <- p2.sh2 + plot_annotation(title = "\nCounty decides to close upper secondary school") &
  theme(plot.title = element_text(hjust = .5, size = 18))


##### #Figures E2Eg-E2Ei: National government decides to close a department of a university (college) #####
m2.supportN.sh2 <- feols(second_answer ~ first_answer_factor * sh2 * Support + tier_n | responseid,
                         data = dataset)
summary(m2.supportN.sh2)

###### Figure E2Eg: National government decides to close a department of a university (college) and municipality supports #####
preds.railow.natdec.sh2 <- plot_predictions(m2.supportN.sh2,
                                            newdata = datagrid(first_answer_factor = 1:4,
                                                               sh2 = 2.72,
                                                               Support = c("Respondent's Municipality Supports"),
                                                               tier_n = c("N")),
                                            by = c("Support", "sh2", "first_answer_factor"),
                                            gray = T,
                                            conf_level = 0.84,
                                            draw = F) # Get the estimates & CIs when RAI low

preds.high.natdec.sh2 <- plot_predictions(m2.supportN.sh2,
                                          newdata = datagrid(first_answer_factor = 1:4,
                                                             sh2 = 4,
                                                             Support = c("Respondent's Municipality Supports"),
                                                             tier_n = c("N")),
                                          by = c("Support", "sh2", "first_answer_factor"),
                                          gray = T,
                                          conf_level = 0.84,
                                          draw = F) # Get the estimates & CIs when RAI high

plot2.supportN_mun.sh2 <- plot_predictions(m2.supportN.sh2,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sh2 = c(2.72, 4),
                                                              Support = c("Respondent's Municipality Supports"),
                                                              tier_n = c("N")),
                                           by = c("Support", "sh2", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.95,
                                           draw = F)


p2_n_m_sh2 <- ggplot(plot2.supportN_mun.sh2,
                     aes(x = first_answer_factor,
                         y = estimate,
                         group = first_answer_factor, shape = sh2)) +
  geom_hline(yintercept = c(1, 2, 3, 4), color='gray') + 
  facet_grid(.~Support) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = sh2),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_N_m_sh2 <- p2_n_m_sh2 +
  labs(x="",
       y="",
       shape="Preference for Shared Rule Item 5") +
  scale_shape_discrete(labels= c("Mean - 1SD (2.72)", "Mean + 1SD (4)")) +
  theme_classic()  + theme(legend.position = "bottom",
                           plot.title = element_text(hjust = 0.5, vjust = 1.5),
                           axis.title.y = element_text(size = 18, vjust = 4.5)) 


p2_N_m_sh2 <- p2_N_m_sh2 + geom_linerange(data=preds.railow.natdec.sh2,
                                          aes(y = estimate,
                                              x = first_answer_factor,
                                              ymin = conf.low,
                                              ymax = conf.high),
                                          position = position_nudge(x = -0.05),
                                          linewidth = 1.2) +
  geom_linerange(data = preds.high.natdec.sh2,
                 aes(y = estimate,
                     x = first_answer_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)


my.plot.nat.1.sh2 <- p2_N_m_sh2  + labs(tag = "g:") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        axis.title.y = element_text(size=14),
        strip.text.x = element_text(size = 11),
        plot.tag.position = c(0.16, 0.974)) 

my.plot.nat.1.sh2

###### Figure E2Eh: National government decides to close a department of a university (college) and county supports ####

preds.low.natdec.2.sh2 <- plot_predictions(m2.supportN.sh2,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sh2 = 2.72,
                                                              Support = c("Respondent's County Supports"),
                                                              tier_n = c("N")),
                                           by = c("Support", "sh2", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.84,
                                           draw = F) # Get the estimates & CIs when RAI low

preds.high.natdec.2.sh2 <- plot_predictions(m2.supportN.sh2,
                                            newdata = datagrid(first_answer_factor = 1:4,
                                                               sh2 = 4,
                                                               Support = c("Respondent's County Supports"),
                                                               tier_n = c("N")),
                                            by = c("Support", "sh2", "first_answer_factor"),
                                            gray = T,
                                            conf_level = 0.84,
                                            draw = F) # Get the estimates & CIs when RAI high

plot2.supportN_cou.sh2 <- plot_predictions(m2.supportN.sh2,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sh2 = c(2.72, 4),
                                                              Support = c("Respondent's County Supports"),
                                                              tier_n = c("N")),
                                           by = c("Support", "sh2", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.95,
                                           draw = F)


p2_n_c_sh2 <- ggplot(plot2.supportN_cou.sh2,
                     aes(x = first_answer_factor,
                         y = estimate,
                         group = first_answer_factor, shape = sh2)) +
  geom_hline(yintercept = c(1, 2, 3, 4), color='gray') + 
  facet_grid(.~Support) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = sh2),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_N_c_sh2 <- p2_n_c_sh2 +
  labs(x="Willingness to accept initial decision",
       y="",
       shape="Preference for Shared Rule Item 5") +
  scale_shape_discrete(labels= c("Mean - 1SD (2.72)", "Mean + 1SD (4)")) +
  theme_classic() + theme(legend.position = "bottom",
                          plot.title = element_text(hjust = 0.5, vjust = 1.5),
                          axis.title.x = element_text(vjust = -1))

p2_N_c_sh2 <- p2_N_c_sh2 + geom_linerange(data=preds.low.natdec.2.sh2,
                                          aes(y = estimate,
                                              x = first_answer_factor,
                                              ymin = conf.low,
                                              ymax = conf.high),
                                          position = position_nudge(x = -0.05),
                                          linewidth = 1.2) +
  geom_linerange(data = preds.high.natdec.2.sh2,
                 aes(y = estimate,
                     x = first_answer_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)


my.plot.nat.2.sh2 <- p2_N_c_sh2  + labs(tag = "h:") +
  theme(axis.text = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 16),
        strip.text.x = element_text(size = 11),
        plot.tag.position = c(0.16, 0.972)) + annotate("text", x=3, y=3.5,
                                                           label="*", size=8)

###### Figure E2Ei: National government decides to close a department of a university (college) and municipality and county support ####

preds.low.natdec.3.sh2 <- plot_predictions(m2.supportN.sh2,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sh2 = 2.72,
                                                              Support = c("Municipality & County Support"),
                                                              tier_n = c("N")),
                                           by = c("Support", "sh2", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.84,
                                           draw = F) # Get the estimates & CIs when RAI low

preds.high.natdec.3.sh2 <- plot_predictions(m2.supportN.sh2,
                                            newdata = datagrid(first_answer_factor = 1:4,
                                                               sh2 = 4,
                                                               Support = c("Municipality & County Support"),
                                                               tier_n = c("N")),
                                            by = c("Support", "sh2", "first_answer_factor"),
                                            gray = T,
                                            conf_level = 0.84,
                                            draw = F) # Get the estimates & CIs when RAI high

plot2.supportN_muncou.sh2 <- plot_predictions(m2.supportN.sh2,
                                              newdata = datagrid(first_answer_factor = 1:4,
                                                                 sh2 = c(2.72, 4),
                                                                 Support = c("Municipality & County Support"),
                                                                 tier_n = c("N")),
                                              by = c("Support", "sh2", "first_answer_factor"),
                                              gray = T,
                                              conf_level = 0.95,
                                              draw = F)


p2_n_mc_sh2 <- ggplot(plot2.supportN_muncou.sh2,
                      aes(x = first_answer_factor,
                          y = estimate,
                          group = first_answer_factor, shape = sh2)) +
  geom_hline(yintercept = c(1, 2, 3, 4), color='gray') + 
  facet_grid(.~Support) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = sh2),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_N_mc_sh2 <- p2_n_mc_sh2 +
  labs(x="", y="",
       shape="Preference for Shared Rule Item 5") +
  scale_shape_discrete(labels= c("Mean - 1SD (2.72)", "Mean + 1SD (4)")) +
  theme_classic() + theme(legend.position = "bottom",
                          plot.title = element_text(hjust = 0.5, vjust = 1.5))


p2_N_mc_sh2 <- p2_N_mc_sh2 + geom_linerange(data=preds.low.natdec.3.sh2,
                                            aes(y = estimate,
                                                x = first_answer_factor,
                                                ymin = conf.low,
                                                ymax = conf.high),
                                            position = position_nudge(x = -0.05),
                                            linewidth = 1.2) +
  geom_linerange(data = preds.high.natdec.3.sh2,
                 aes(y = estimate,
                     x = first_answer_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)


my.plot.nat.3.sh2 <- p2_N_mc_sh2  + labs(tag = "i:") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        strip.text.x = element_text(size = 11),
        plot.tag.position = c(0.16, 0.972))


p3.sh2 <- ggarrange(my.plot.nat.1.sh2, my.plot.nat.2.sh2, my.plot.nat.3.sh2, nrow = 1, ncol = 3,
                    common.legend = T, legend = "bottom")

p3.sh2 <- p3.sh2 + plot_annotation(title = "\nNational government decides to close \na department in a university (college)") &
  theme(plot.title = element_text(hjust = .5, size = 18),
        legend.title = element_text(size = 16),
        legend.text = element_text(size=15))

#Gather all SH2 plots together:
my.plot.all.sh2 <- ggarrange(p1.sh2, p2.sh2, p3.sh2, nrow = 3, ncol = 1)

# Save Figure E2E as a .jpeg file:
ggsave("Figure_E2E.jpeg",
       width = 12, height = 17)


#### Figure E2F. Model 2-Q2-Q4-Q6: Preference for shared rule item 6. ####

# IMPORTANT NOTE: The point estimates are statistically significantly different
# from each other (p<0.05) when their 84% CIs do not overlap and the 95% CI of
# the DIFFERENCE between two point estimates does not include zero; see data and methods.

##### Figures E2Fa-E2Fc: Municipality decides to close kindergarten. ####

mod.sh3.mun.support <- feols(second_answer ~ first_answer_factor * sh3 * support_m + tier_m | responseid,
                             data = dataset)
summary(mod.sh3.mun.support)

###### Figure E2Fa: Municipality decides to close kindergarten and county supports ####
preds.railow.mundec.sh3 <- plot_predictions(mod.sh3.mun.support,
                                            newdata = datagrid(first_answer_factor = 1:4,
                                                               sh3 = 2.57,
                                                               support_m = c("Respondent's County Supports"),
                                                               tier_m = c("M")),
                                            by = c("support_m", "sh3", "first_answer_factor"),
                                            gray = T,
                                            conf_level = 0.84,
                                            draw = F) # Get the estimates & CIs when RAI low

preds.high.mundec.sh3 <- plot_predictions(mod.sh3.mun.support,
                                          newdata = datagrid(first_answer_factor = 1:4,
                                                             sh3 = 3.92,
                                                             support_m = c("Respondent's County Supports"),
                                                             tier_m = c("M")),
                                          by = c("support_m", "sh3", "first_answer_factor"),
                                          gray = T,
                                          conf_level = 0.84,
                                          draw = F) # Get the estimates & CIs when RAI high

plot2.supportM_cou.sh3 <- plot_predictions(mod.sh3.mun.support,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sh3 = c(2.57, 3.92),
                                                              support_m = c("Respondent's County Supports"),
                                                              tier_m = c("M")),
                                           by = c("support_m", "sh3", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.95,
                                           draw = F)


p2_m_c_sh3 <- ggplot(plot2.supportM_cou.sh3,
                     aes(x = first_answer_factor,
                         y = estimate,
                         group = first_answer_factor, shape = sh3)) +
  geom_hline(yintercept = c(1, 2, 3, 4), color='gray') + 
  facet_grid(.~support_m) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = sh3),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_M_c_sh3 <- p2_m_c_sh3 +
  labs(x="",
       y="",
       shape="Preference for Shared Rule Item 6") +
  scale_shape_discrete(labels= c("Mean - 1SD (2.57)", "Mean + 1SD (3.92)")) +
  theme_classic()  + theme(legend.position = "bottom",
                           plot.title = element_text(hjust = 0.5, vjust = 1.5))

p2_M_c_sh3 <- p2_M_c_sh3 + geom_linerange(data=preds.railow.mundec.sh3,
                                          aes(y = estimate,
                                              x = first_answer_factor,
                                              ymin = conf.low,
                                              ymax = conf.high),
                                          position = position_nudge(x = -0.05),
                                          linewidth = 1.2) +
  geom_linerange(data = preds.high.mundec.sh3,
                 aes(y = estimate,
                     x = first_answer_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)

my.plot.mun.1.sh3 <- p2_M_c_sh3  + labs(tag = "a:") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        axis.title.y = element_text(size=14),
        strip.text.x = element_text(size = 11),
        plot.tag.position = c(0.16, 0.974))

my.plot.mun.1.sh3

###### Figure E2Fb: Municipality decides to close kindergarten and national government supports ####

preds.low.mundec.2.sh3 <- plot_predictions(mod.sh3.mun.support,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sh3 = 2.57,
                                                              support_m = c("National Government Supports"),
                                                              tier_m = c("M")),
                                           by = c("support_m", "sh3", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.84,
                                           draw = F) # Get the estimates & CIs when RAI low

preds.high.mundec.2.sh3 <- plot_predictions(mod.sh3.mun.support,
                                            newdata = datagrid(first_answer_factor = 1:4,
                                                               sh3 = 3.92,
                                                               support_m = c("National Government Supports"),
                                                               tier_m = c("M")),
                                            by = c("support_m", "sh3", "first_answer_factor"),
                                            gray = T,
                                            conf_level = 0.84,
                                            draw = F) # Get the estimates & CIs when RAI high

plot2.supportM_nat.sh3 <- plot_predictions(mod.sh3.mun.support,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sh3 = c(2.57, 3.92),
                                                              support_m = c("National Government Supports"),
                                                              tier_m = c("M")),
                                           by = c("support_m", "sh3", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.95,
                                           draw = F)

p2_m_n_sh3 <- ggplot(plot2.supportM_nat.sh3,
                     aes(x = first_answer_factor,
                         y = estimate,
                         group = first_answer_factor, shape = sh3)) +
  geom_hline(yintercept = c(1, 2, 3, 4), color='gray') + 
  facet_grid(.~support_m) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = sh3),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_M_n_sh3 <- p2_m_n_sh3 +
  labs(x="",
       y="",
       shape="Preference for Shared Rule Item 6") +
  scale_shape_discrete(labels= c("Mean - 1SD (2.57)", "Mean + 1SD (3.92)")) +
  theme_classic() + theme(legend.position = "bottom",
                          plot.title = element_text(hjust = 0.5, vjust = 1.5))


p2_M_n_sh3 <- p2_M_n_sh3 + geom_linerange(data=preds.low.mundec.2.sh3,
                                          aes(y = estimate,
                                              x = first_answer_factor,
                                              ymin = conf.low,
                                              ymax = conf.high),
                                          position = position_nudge(x = -0.05),
                                          linewidth = 1.2) +
  geom_linerange(data = preds.high.mundec.2.sh3,
                 aes(y = estimate,
                     x = first_answer_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)


my.plot.mun.2.sh3 <- p2_M_n_sh3  + labs(tag = "b:") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        strip.text.x = element_text(size = 11),
        plot.tag.position = c(0.16, 0.974)) + annotate('text', x=1, y= 2.1,
                                                           label = "*", size=8)

my.plot.mun.2.sh3


###### Figure E2Fc: Municipality decides to close kindergarten and county and national government support ####

preds.low.mundec.3.sh3 <- plot_predictions(mod.sh3.mun.support,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sh3 = 2.57,
                                                              support_m = c("County & National Government Support"),
                                                              tier_m = c("M")),
                                           by = c("support_m", "sh3", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.84,
                                           draw = F) # Get the estimates & CIs when sh3 low

preds.high.mundec.3.sh3 <- plot_predictions(mod.sh3.mun.support,
                                            newdata = datagrid(first_answer_factor = 1:4,
                                                               sh3 = 3.92,
                                                               support_m = c("County & National Government Support"),
                                                               tier_m = c("M")),
                                            by = c("support_m", "sh3", "first_answer_factor"),
                                            gray = T,
                                            conf_level = 0.84,
                                            draw = F) # Get the estimates & CIs when RAI high

plot2.supportM_counat.sh3 <- plot_predictions(mod.sh3.mun.support,
                                              newdata = datagrid(first_answer_factor = 1:4,
                                                                 sh3 = c(2.57, 3.92),
                                                                 support_m = c("County & National Government Support"),
                                                                 tier_m = c("M")),
                                              by = c("support_m", "sh3", "first_answer_factor"),
                                              gray = T,
                                              conf_level = 0.95,
                                              draw = F)


p2_m_cn_sh3 <- ggplot(plot2.supportM_counat.sh3,
                      aes(x = first_answer_factor,
                          y = estimate,
                          group = first_answer_factor, shape = sh3)) +
  geom_hline(yintercept = c(1, 2, 3, 4), color='gray') + 
  facet_grid(.~support_m) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = sh3),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_M_cn_sh3 <- p2_m_cn_sh3 +
  labs(x="", y="",
       shape="Preference for Shared Rule Item 6") +
  scale_shape_discrete(labels= c("Mean - 1SD (2.57)", "Mean + 1SD (3.92)")) +
  theme_classic() + theme(legend.position = "bottom",
                          plot.title = element_text(hjust = 0.5, vjust = 1.5))


p2_M_cn_sh3 <- p2_M_cn_sh3 + geom_linerange(data=preds.low.mundec.3.sh3,
                                            aes(y = estimate,
                                                x = first_answer_factor,
                                                ymin = conf.low,
                                                ymax = conf.high),
                                            position = position_nudge(x = -0.05),
                                            linewidth = 1.2) +
  geom_linerange(data = preds.high.mundec.3.sh3,
                 aes(y = estimate,
                     x = first_answer_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)

my.plot.mun.3.sh3 <- p2_M_cn_sh3  + labs(tag = "c:") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        strip.text.x = element_text(size = 11),
        plot.tag.position = c(0.16, 0.974))

my.plot.mun.3.sh3

p1.sh3 <- ggarrange(my.plot.mun.1.sh3, my.plot.mun.2.sh3, my.plot.mun.3.sh3, nrow = 1, ncol = 3,
                    common.legend = T, legend = "none") + plot_annotation(title="Municipality decides to close kindergarten") &
  theme(plot.title = element_text(hjust = .5, size = 18), legend.text = element_blank())


##### Figures E2Fd-E2Ff: County decides to close upper secondary school #####
m2.supportC.sh3 <- feols(second_answer ~ first_answer_factor * sh3 * Support_c + tier_c | responseid,
                         data = dataset)
summary(m2.supportC.sh3)

###### Figure E2Fd: County decides to close upper secondary school and municipality supports #####

preds.railow.coudec.sh3 <- plot_predictions(m2.supportC.sh3,
                                            newdata = datagrid(first_answer_factor = 1:4,
                                                               sh3 = 2.57,
                                                               Support_c = c("Respondent's Municipality Supports"),
                                                               tier_c = c("C")),
                                            by = c("Support_c", "sh3", "first_answer_factor"),
                                            gray = T,
                                            conf_level = 0.84,
                                            draw = F) # Get the estimates & CIs when RAI low

preds.high.coudec.sh3 <- plot_predictions(m2.supportC.sh3,
                                          newdata = datagrid(first_answer_factor = 1:4,
                                                             sh3 = 3.92,
                                                             Support_c = c("Respondent's Municipality Supports"),
                                                             tier_c = c("C")),
                                          by = c("Support_c", "sh3", "first_answer_factor"),
                                          gray = T,
                                          conf_level = 0.84,
                                          draw = F) # Get the estimates & CIs when RAI high

plot2.supportC_mun.sh3 <- plot_predictions(m2.supportC.sh3,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sh3 = c(2.57, 3.92),
                                                              Support_c = c("Respondent's Municipality Supports"),
                                                              tier_c = c("C")),
                                           by = c("Support_c", "sh3", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.95,
                                           draw = F)


p2_c_m_sh3 <- ggplot(plot2.supportC_mun.sh3,
                     aes(x = first_answer_factor,
                         y = estimate,
                         group = first_answer_factor, shape = sh3)) +
  geom_hline(yintercept = c(1, 2, 3, 4), color='gray') + 
  facet_grid(.~Support_c) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = sh3),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_C_m_sh3 <- p2_c_m_sh3 +
  labs(x="",
       y="Willingness to accept decision\nwhen support is provided by another tier",
       shape="Preference for Shared Rule Item 6") +
  scale_shape_discrete(labels= c("Mean - 1SD (2.57)", "Mean + 1SD (3.92)")) +
  theme_classic()  + theme(legend.position = "bottom",
                           plot.title = element_text(hjust = 0.5, vjust = 1.5),
                           axis.title.y = element_text(size = 18, vjust = 4.5)) 


p2_C_m_sh3 <- p2_C_m_sh3 + geom_linerange(data=preds.railow.coudec.sh3,
                                          aes(y = estimate,
                                              x = first_answer_factor,
                                              ymin = conf.low,
                                              ymax = conf.high),
                                          position = position_nudge(x = -0.05),
                                          linewidth = 1.2) +
  geom_linerange(data = preds.high.coudec.sh3,
                 aes(y = estimate,
                     x = first_answer_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)


my.plot.cou.1.sh3 <- p2_C_m_sh3  + labs(tag = "d:") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        axis.title.y = element_text(size=14),
        strip.text.x = element_text(size = 11),
        plot.tag.position = c(0.19, 0.974)) + annotate('text', x=3, y=3.45,
                                                           label='*', size = 8)

my.plot.cou.1.sh3

###### Figure E2Fe: County decides to close upper secondary school and national government supports ####

preds.low.coudec.2.sh3 <- plot_predictions(m2.supportC.sh3,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sh3 = 2.57,
                                                              Support_c = c("National Government Supports"),
                                                              tier_c = c("C")),
                                           by = c("Support_c", "sh3", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.84,
                                           draw = F) # Get the estimates & CIs when RAI low

preds.high.coudec.2.sh3 <- plot_predictions(m2.supportC.sh3,
                                            newdata = datagrid(first_answer_factor = 1:4,
                                                               sh3 = 3.92,
                                                               Support_c = c("National Government Supports"),
                                                               tier_c = c("C")),
                                            by = c("Support_c", "sh3", "first_answer_factor"),
                                            gray = T,
                                            conf_level = 0.84,
                                            draw = F) # Get the estimates & CIs when RAI high

plot2.supportC_nat.sh3 <- plot_predictions(m2.supportC.sh3,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sh3 = c(2.57, 3.92),
                                                              Support_c = c("National Government Supports"),
                                                              tier_c = c("C")),
                                           by = c("Support_c", "sh3", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.95,
                                           draw = F)


p2_c_n_sh3 <- ggplot(plot2.supportC_nat.sh3,
                     aes(x = first_answer_factor,
                         y = estimate,
                         group = first_answer_factor, shape = sh3)) +
  geom_hline(yintercept = c(1, 2, 3, 4), color='gray') + 
  facet_grid(.~Support_c) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = sh3),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_C_n_sh3 <- p2_c_n_sh3 +
  labs(x="",
       y="",
       shape="Preference for Shared Rule Item 6") +
  scale_shape_discrete(labels= c("Mean - 1SD (2.57)", "Mean + 1SD (3.92)")) +
  theme_classic() + theme(legend.position = "bottom",
                          plot.title = element_text(hjust = 0.5, vjust = 1.5))


p2_C_n_sh3 <- p2_C_n_sh3 + geom_linerange(data=preds.low.coudec.2.sh3,
                                          aes(y = estimate,
                                              x = first_answer_factor,
                                              ymin = conf.low,
                                              ymax = conf.high),
                                          position = position_nudge(x = -0.05),
                                          linewidth = 1.2) +
  geom_linerange(data = preds.high.coudec.2.sh3,
                 aes(y = estimate,
                     x = first_answer_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)


my.plot.cou.2.sh3 <- p2_C_n_sh3  + labs(tag = "e:") +
  theme(axis.text = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 16),
        strip.text.x = element_text(size = 11),
        plot.tag.position = c(0.16, 0.974))

my.plot.cou.2.sh3

###### Figure E2Ff: County decides to close upper secondary school and municipality and national government support ####

preds.low.coudec.3.sh3 <- plot_predictions(m2.supportC.sh3,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sh3 = 2.57,
                                                              Support_c = c("Municipality & National Government Support"),
                                                              tier_c = c("C")),
                                           by = c("Support_c", "sh3", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.84,
                                           draw = F) # Get the estimates & CIs when RAI low

preds.high.coudec.3.sh3 <- plot_predictions(m2.supportC.sh3,
                                            newdata = datagrid(first_answer_factor = 1:4,
                                                               sh3 = 3.92,
                                                               Support_c = c("Municipality & National Government Support"),
                                                               tier_c = c("C")),
                                            by = c("Support_c", "sh3", "first_answer_factor"),
                                            gray = T,
                                            conf_level = 0.84,
                                            draw = F) # Get the estimates & CIs when RAI high

plot2.supportC_munnat.sh3 <- plot_predictions(m2.supportC.sh3,
                                              newdata = datagrid(first_answer_factor = 1:4,
                                                                 sh3 = c(2.57, 3.92),
                                                                 Support_c = c("Municipality & National Government Support"),
                                                                 tier_c = c("C")),
                                              by = c("Support_c", "sh3", "first_answer_factor"),
                                              gray = T,
                                              conf_level = 0.95,
                                              draw = F)


p2_c_mn_sh3 <- ggplot(plot2.supportC_munnat.sh3,
                      aes(x = first_answer_factor,
                          y = estimate,
                          group = first_answer_factor, shape = sh3)) +
  geom_hline(yintercept = c(1, 2, 3, 4), color='gray') + 
  facet_grid(.~Support_c) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = sh3),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_C_mn_sh3 <- p2_c_mn_sh3 +
  labs(x="", y="",
       shape="Preference for Shared Rule Item 6") +
  scale_shape_discrete(labels= c("Mean - 1SD (2.57)", "Mean + 1SD (3.92)")) +
  theme_classic() + theme(legend.position = "bottom",
                          plot.title = element_text(hjust = 0.5, vjust = 1.5))


p2_C_mn_sh3 <- p2_C_mn_sh3 + geom_linerange(data=preds.low.coudec.3.sh3,
                                            aes(y = estimate,
                                                x = first_answer_factor,
                                                ymin = conf.low,
                                                ymax = conf.high),
                                            position = position_nudge(x = -0.05),
                                            linewidth = 1.2) +
  geom_linerange(data = preds.high.coudec.3.sh3,
                 aes(y = estimate,
                     x = first_answer_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)


my.plot.cou.3.sh3 <- p2_C_mn_sh3  + labs(tag = "f:") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        strip.text.x = element_text(size = 11),
        plot.tag.position = c(0.134, 0.973))


p2.sh3 <- ggarrange(my.plot.cou.1.sh3, my.plot.cou.2.sh3, my.plot.cou.3.sh3, nrow = 1, ncol = 3,
                    common.legend = T, legend = "none") 

p2.sh3 <- p2.sh3 + plot_annotation(title = "\nCounty decides to close upper secondary school") &
  theme(plot.title = element_text(hjust = .5, size = 18))


##### Figures E2Fg-E2Fi: National government decides to close a department of a university (college) #####
m2.supportN.sh3 <- feols(second_answer ~ first_answer_factor * sh3 * Support + tier_n | responseid,
                         data = dataset)
summary(m2.supportN.sh3)

###### Figure E2Fg: National government decides to close a department of a university (college) and municipality supports #####
preds.railow.natdec.sh3 <- plot_predictions(m2.supportN.sh3,
                                            newdata = datagrid(first_answer_factor = 1:4,
                                                               sh3 = 2.57,
                                                               Support = c("Respondent's Municipality Supports"),
                                                               tier_n = c("N")),
                                            by = c("Support", "sh3", "first_answer_factor"),
                                            gray = T,
                                            conf_level = 0.84,
                                            draw = F) # Get the estimates & CIs when RAI low

preds.high.natdec.sh3 <- plot_predictions(m2.supportN.sh3,
                                          newdata = datagrid(first_answer_factor = 1:4,
                                                             sh3 = 3.92,
                                                             Support = c("Respondent's Municipality Supports"),
                                                             tier_n = c("N")),
                                          by = c("Support", "sh3", "first_answer_factor"),
                                          gray = T,
                                          conf_level = 0.84,
                                          draw = F) # Get the estimates & CIs when RAI high

plot2.supportN_mun.sh3 <- plot_predictions(m2.supportN.sh3,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sh3 = c(2.57, 3.92),
                                                              Support = c("Respondent's Municipality Supports"),
                                                              tier_n = c("N")),
                                           by = c("Support", "sh3", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.95,
                                           draw = F)


p2_n_m_sh3 <- ggplot(plot2.supportN_mun.sh3,
                     aes(x = first_answer_factor,
                         y = estimate,
                         group = first_answer_factor, shape = sh3)) +
  geom_hline(yintercept = c(1, 2, 3, 4), color='gray') + 
  facet_grid(.~Support) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = sh3),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_N_m_sh3 <- p2_n_m_sh3 +
  labs(x="",
       y="",
       shape="Preference for Shared Rule Item 6") +
  scale_shape_discrete(labels= c("Mean - 1SD (2.57)", "Mean + 1SD (3.92)")) +
  theme_classic()  + theme(legend.position = "bottom",
                           plot.title = element_text(hjust = 0.5, vjust = 1.5),
                           axis.title.y = element_text(size = 18, vjust = 4.5)) 


p2_N_m_sh3 <- p2_N_m_sh3 + geom_linerange(data=preds.railow.natdec.sh3,
                                          aes(y = estimate,
                                              x = first_answer_factor,
                                              ymin = conf.low,
                                              ymax = conf.high),
                                          position = position_nudge(x = -0.05),
                                          linewidth = 1.2) +
  geom_linerange(data = preds.high.natdec.sh3,
                 aes(y = estimate,
                     x = first_answer_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)


my.plot.nat.1.sh3 <- p2_N_m_sh3  + labs(tag = "g:") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        axis.title.y = element_text(size=14),
        strip.text.x = element_text(size = 11),
        plot.tag.position = c(0.16, 0.974)) + annotate("text", x=c(1,2,3,4),
                                                           y=c(2.05, 2.75, 3.45, 3.9),
                                                           label= c("*", "**", "**", "*"),
                                                           size=8)

my.plot.nat.1.sh3

###### Figure E2Fh: National government decides to close a department of a university (college) and county supports ####

preds.low.natdec.2.sh3 <- plot_predictions(m2.supportN.sh3,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sh3 = 2.57,
                                                              Support = c("Respondent's County Supports"),
                                                              tier_n = c("N")),
                                           by = c("Support", "sh3", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.84,
                                           draw = F) # Get the estimates & CIs when RAI low

preds.high.natdec.2.sh3 <- plot_predictions(m2.supportN.sh3,
                                            newdata = datagrid(first_answer_factor = 1:4,
                                                               sh3 = 3.92,
                                                               Support = c("Respondent's County Supports"),
                                                               tier_n = c("N")),
                                            by = c("Support", "sh3", "first_answer_factor"),
                                            gray = T,
                                            conf_level = 0.84,
                                            draw = F) # Get the estimates & CIs when RAI high

plot2.supportN_cou.sh3 <- plot_predictions(m2.supportN.sh3,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sh3 = c(2.57, 3.92),
                                                              Support = c("Respondent's County Supports"),
                                                              tier_n = c("N")),
                                           by = c("Support", "sh3", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.95,
                                           draw = F)


p2_n_c_sh3 <- ggplot(plot2.supportN_cou.sh3,
                     aes(x = first_answer_factor,
                         y = estimate,
                         group = first_answer_factor, shape = sh3)) +
  geom_hline(yintercept = c(1, 2, 3, 4), color='gray') + 
  facet_grid(.~Support) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = sh3),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_N_c_sh3 <- p2_n_c_sh3 +
  labs(x="Willingness to accept initial decision",
       y="",
       shape="Preference for Shared Rule Item 6") +
  scale_shape_discrete(labels= c("Mean - 1SD (2.57)", "Mean + 1SD (3.92)")) +
  theme_classic() + theme(legend.position = "bottom",
                          plot.title = element_text(hjust = 0.5, vjust = 1.5),
                          axis.title.x = element_text(vjust = -1))


p2_N_c_sh3 <- p2_N_c_sh3 + geom_linerange(data=preds.low.natdec.2.sh3,
                                          aes(y = estimate,
                                              x = first_answer_factor,
                                              ymin = conf.low,
                                              ymax = conf.high),
                                          position = position_nudge(x = -0.05),
                                          linewidth = 1.2) +
  geom_linerange(data = preds.high.natdec.2.sh3,
                 aes(y = estimate,
                     x = first_answer_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)


my.plot.nat.2.sh3 <- p2_N_c_sh3  + labs(tag = "h:") +
  theme(axis.text = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 16),
        strip.text.x = element_text(size = 11),
        plot.tag.position = c(0.16, 0.972)) + annotate("text", x=c(2,3,4),
                                                           y=c(2.6, 3.2, 3.8),
                                                           label=c("**", "**", "*"),
                                                           size=8)

my.plot.nat.2.sh3

###### Figure E2Fi: National government decides to close a department of a university (college) and municipality and county support ####

preds.low.natdec.3.sh3 <- plot_predictions(m2.supportN.sh3,
                                           newdata = datagrid(first_answer_factor = 1:4,
                                                              sh3 = 2.57,
                                                              Support = c("Municipality & County Support"),
                                                              tier_n = c("N")),
                                           by = c("Support", "sh3", "first_answer_factor"),
                                           gray = T,
                                           conf_level = 0.84,
                                           draw = F) # Get the estimates & CIs when RAI low

preds.high.natdec.3.sh3 <- plot_predictions(m2.supportN.sh3,
                                            newdata = datagrid(first_answer_factor = 1:4,
                                                               sh3 = 3.92,
                                                               Support = c("Municipality & County Support"),
                                                               tier_n = c("N")),
                                            by = c("Support", "sh3", "first_answer_factor"),
                                            gray = T,
                                            conf_level = 0.84,
                                            draw = F) # Get the estimates & CIs when RAI high

plot2.supportN_muncou.sh3 <- plot_predictions(m2.supportN.sh3,
                                              newdata = datagrid(first_answer_factor = 1:4,
                                                                 sh3 = c(2.57, 3.92),
                                                                 Support = c("Municipality & County Support"),
                                                                 tier_n = c("N")),
                                              by = c("Support", "sh3", "first_answer_factor"),
                                              gray = T,
                                              conf_level = 0.95,
                                              draw = F)


p2_n_mc_sh3 <- ggplot(plot2.supportN_muncou.sh3,
                      aes(x = first_answer_factor,
                          y = estimate,
                          group = first_answer_factor, shape = sh3)) +
  geom_hline(yintercept = c(1, 2, 3, 4), color='gray') + 
  facet_grid(.~Support) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = sh3),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_N_mc_sh3 <- p2_n_mc_sh3 +
  labs(x="", y="",
       shape="Preference for Shared Rule Item 6") +
  scale_shape_discrete(labels= c("Mean - 1SD (2.57)", "Mean + 1SD (3.92)")) +
  theme_classic() + theme(legend.position = "bottom",
                          plot.title = element_text(hjust = 0.5, vjust = 1.5))


p2_N_mc_sh3 <- p2_N_mc_sh3 + geom_linerange(data=preds.low.natdec.3.sh3,
                                            aes(y = estimate,
                                                x = first_answer_factor,
                                                ymin = conf.low,
                                                ymax = conf.high),
                                            position = position_nudge(x = -0.05),
                                            linewidth = 1.2) +
  geom_linerange(data = preds.high.natdec.3.sh3,
                 aes(y = estimate,
                     x = first_answer_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)


my.plot.nat.3.sh3 <- p2_N_mc_sh3  + labs(tag = "i:") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        axis.title.y = element_text(size = 14),
        strip.text.x = element_text(size = 11),
        plot.tag.position = c(0.16, 0.972)) + annotate('text', x=c(2,3),
                                                           y=c(2.75, 3.5),
                                                           label = c('*', '**'),
                                                           size=8)


p3.sh3 <- ggarrange(my.plot.nat.1.sh3, my.plot.nat.2.sh3, my.plot.nat.3.sh3, nrow = 1, ncol = 3,
                    common.legend = T, legend = "bottom")

p3.sh3 <- p3.sh3 + plot_annotation(title = "\nNational government decides to close \na department in university/college") &
  theme(plot.title = element_text(hjust = .5, size = 18),
        legend.title = element_text(size = 16),
        legend.text = element_text(size=15))

my.plot.all.sh3 <- ggarrange(p1.sh3, p2.sh3, p3.sh3, nrow = 3, ncol = 1)

# Save Figure E2F as a.jpeg file:
ggsave("Figure_E2F.jpeg",
       width = 12, height = 17)

## APPENDIX F: Robustness test III: Random-effects model.####

# IMPORTANT NOTE: Appendix F has its own replication dataset; 
# OPEN: Alica_Schakel_2025_JPP_replication_RE_dataset.rds

appendix_f_dataset <- readRDS("Alica_Schakel_2025_JPP_replication_RE_dataset.rds")

### Table F1. Model 1-Q1-Q3-Q5: Random effects model specification. ####

m1 <- lmer(first_answer ~ psa * tier + gender + age + edu + incm +
             urb_rur + econ_reg + left_right + have_chldrn_dum + (1 | municipality_hash)
           + (1 | responseid_hash), data = appendix_f_dataset)

summary(m1)

modelsummary::modelsummary(m1, stars = T, shape = term ~ model + statistic,
                           output = "m1_re.docx")

### Figure F1. Model 1-Q1-Q3-Q5: Random effects model specification. ####

preds.railow.munbase <- plot_predictions(m1,
                                         condition = list(tier = list("M", "C", "N"),
                                                          psa = 0),
                                         conf_level = 0.84,
                                         gray = T,
                                         draw = F) # Get the estimates & CIs when RAI low

preds.high.munbase <- plot_predictions(m1,
                                       condition = list(tier = list("M", "C", "N"),
                                                        psa = 1),
                                       conf_level = 0.84,
                                       gray = T,
                                       draw = F) # Get the estimates & CIs when RAI high


plot1.m1.munbase <- plot_predictions(m1,
                                     condition = list(tier = list("M", "C", "N"),
                                                      psa = "minmax"),
                                     conf_level = 0.95,
                                     gray = T)

plot1.m1.munbase <-  plot1.m1.munbase +
  labs(x="Decision taken by",
       y="Willingness to accept decision (4-point scale)",
       shape="Preference for\nSubnational Authority") +
  scale_x_discrete(labels = c("National government", "Municipality",
                              "County")) + ylim(2.1, 3.5) +
  scale_shape_discrete(labels = c("Min (= 0)", "Max (= 1)"))

plot1.m1.munbase <- plot1.m1.munbase + geom_linerange(data=preds.railow.munbase,
                                                      aes(y = estimate,
                                                          x = tier,
                                                          ymin = conf.low,
                                                          ymax = conf.high),
                                                      position = position_nudge(x = -0.037),
                                                      linewidth = 1.2) +
  geom_linerange(data = preds.high.munbase,
                 aes(y = estimate,
                     x = tier,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.037),
                 linewidth = 1.2)

plot1.m1.munbase <- plot1.m1.munbase + theme_classic() +
  theme(legend.position = c(0.25, 0.25),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 16, hjust = 0.5, vjust = 2.5),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14)) + annotate('text', x=c(3),
                                                          y=c(3.1),
                                                          label=c('*'),
                                                          size=8)

plot1.m1.munbase$layers[[1]]$geom_params$fatten = 8


# Line graph with 84% CIs

plot2.m1.munbase.84ci <- plot_predictions(m1,
                                          condition = list("psa", "tier"),
                                          conf_level = 0.84,
                                          gray = T)

plot2.m1.munbase.84ci <- plot2.m1.munbase.84ci +
  labs(x = "Preference for subnational authority",
       y = "",
       linetype = "Decision taken by")

plot2.m1.munbase.84ci <- plot2.m1.munbase.84ci +
  scale_linetype_manual(labels = c("National government",
                                   "Municipality",
                                   "County"),
                        values = c("solid", "dotted", "dashed")) +
  theme_classic() + theme(legend.position = c(0.8, 0.9),
                          axis.text = element_text(size = 14),
                          axis.title = element_text(size = 16),
                          plot.title = element_text(size = 16, hjust = 0.5, vjust = 2.5),
                          legend.title = element_text(size = 14),
                          legend.text = element_text(size = 14)) + ylim(2.1, 3.5)


ggsave("plot1_right_munbase_84CI.pdf", plot2.m1.munbase.84ci, width = 11,
       height = 7)

Q1_plot <- ggarrange(plot1.m1.munbase, plot2.m1.munbase.84ci, nrow = 1, ncol=2)

# Save Figure 1 as a .jpeg file
ggsave("Figure_F1.jpeg", width = 15, height = 8)



### Table F2. Model 2-Q2-Q4-Q6: Random effects model specification. ####

m2 <- lmer(second_answer ~ psa * support * first_answer_factor + tier + gender + age + edu + incm +
             urb_rur + econ_reg + left_right + have_chldrn_dum + (1 | municipality_hash)
           + (1 | responseid_hash), data = appendix_f_dataset)

summary(m2)
modelsummary(m2, stars = T, shape = term ~ model + statistic,
             output = "m2_re.docx")


### Figure F2. Model 2-Q2-Q4-Q6: Random effects. ####

#### Figures F2A-C: Municipality decides to close kindergarten. ####
##### Figure F2A: Municipality decides to close kindergarten and county supports. ####


preds.psalow.re2.mun <- plot_predictions(m2,
                                         newdata = datagrid(first_answer_factor = 1:4,
                                                            psa=0.44,
                                                            support = c("Respondent's County Supports"),
                                                            tier = c("M")),
                                         by=c("support", "psa",
                                              "first_answer_factor"),
                                         gray=T,
                                         conf_level = 0.84,
                                         draw = F)

preds.psahigh.re2.mun <- plot_predictions(m2,
                                          newdata = datagrid(first_answer_factor = 1:4,
                                                             psa=0.88,
                                                             support = c("Respondent's County Supports"),
                                                             tier = c("M")),
                                          by=c("support", "psa",
                                               "first_answer_factor"),
                                          gray=T,
                                          conf_level = 0.84,
                                          draw = F)

plot.re2.mun <- plot_predictions(m2,
                                 newdata = datagrid(first_answer_factor = 1:4,
                                                    psa=c(0.44, 0.88),
                                                    support = c("Respondent's County Supports"),
                                                    tier = c("M")),
                                 by=c("support", "psa",
                                      "first_answer_factor"),
                                 gray=T,
                                 conf_level = 0.95,
                                 draw = F)

plot2.raw.mun <- ggplot(plot.re2.mun,
                        aes(x=first_answer_factor,
                            y=estimate,
                            group=first_answer_factor, shape=psa)) +
  geom_hline(yintercept = c(1,2,3,4), color='gray') +
  facet_grid(.~support) + geom_pointrange(aes(ymin=conf.low, ymax=conf.high,
                                              group=psa),
                                          position = position_dodge(width=0.2), fatten=7)

p2.labs <- plot2.raw.mun + labs(x="", y="",
                                shape="Preference for\nSubnational Authority") +
  scale_shape_discrete(labels=c("Mean - 1SD (0.44)", "Mean + 1SD (0.88)")) +
  theme_classic() + theme(legend.position = "bottom",
                          plot.title = element_text(hjust=0.5,
                                                    vjust=1.5),
                          axis.title.y = element_text(size = 18,
                                                      vjust=4.5))

p2.mun.add.conf <- p2.labs + geom_linerange(data = preds.psalow.re2.mun,
                                            aes(y=estimate,
                                                x=first_answer_factor,
                                                ymin=conf.low,
                                                ymax=conf.high),
                                            position=position_nudge(x=-0.05),
                                            linewidth=1.2) +
  geom_linerange(data=preds.psahigh.re2.mun,
                 aes(y=estimate,
                     x=first_answer_factor,
                     ymin=conf.low,
                     ymax=conf.high),
                 position=position_nudge(x= 0.05),
                 linewidth=1.2)

re2.mun.plot <- p2.mun.add.conf + labs(tag = "A:") +
  theme(axis.text = element_text(size=12), 
        axis.title = element_text(size=14),
        strip.text.x = element_text(size=10),
        plot.tag.position = c(0.16, 0.974))

F2A <- re2.mun.plot + annotate('text', x=3, y=3.25, label="**", size=8)


##### Figure F2B: Municipality decides to close kindergarten and national government supports. ####

preds.psalow.re2.mun.natsup <- plot_predictions(m2,
                                                newdata = datagrid(first_answer_factor = 1:4,
                                                                   psa=0.44,
                                                                   support = c("National Government Supports"),
                                                                   tier = c("M")),
                                                by=c("support", "psa",
                                                     "first_answer_factor"),
                                                gray=T,
                                                conf_level = 0.84,
                                                draw = F)

preds.psahigh.re2.mun.natsup <- plot_predictions(m2,
                                                 newdata = datagrid(first_answer_factor = 1:4,
                                                                    psa=0.88,
                                                                    support = c("National Government Supports"),
                                                                    tier = c("M")),
                                                 by=c("support", "psa",
                                                      "first_answer_factor"),
                                                 gray=T,
                                                 conf_level = 0.84,
                                                 draw = F)

plot.re2.mun.natsup <- plot_predictions(m2,
                                        newdata = datagrid(first_answer_factor = 1:4,
                                                           psa=c(0.44, 0.88),
                                                           support = c("National Government Supports"),
                                                           tier = c("M")),
                                        by=c("support", "psa",
                                             "first_answer_factor"),
                                        gray=T,
                                        conf_level = 0.95,
                                        draw = F)

plot2.raw.mun.natsup <- ggplot(plot.re2.mun.natsup,
                               aes(x=first_answer_factor,
                                   y=estimate,
                                   group=first_answer_factor, shape=psa)) +
  geom_hline(yintercept = c(1,2,3,4), color='gray') +
  facet_grid(.~support) + geom_pointrange(aes(ymin=conf.low, ymax=conf.high,
                                              group=psa),
                                          position = position_dodge(width=0.2), fatten=7)

p2.labs.mun.natsup <- plot2.raw.mun.natsup + labs(x="", y="",
                                                  shape="Preference for\nSubnational Authority") +
  scale_shape_discrete(labels=c("Mean - 1SD (0.44)", "Mean + 1SD (0.88)")) +
  theme_classic() + theme(legend.position = "bottom",
                          plot.title = element_text(hjust=0.5,
                                                    vjust=1.5),
                          axis.title.y = element_text(size = 18,
                                                      vjust=4.5))

p2.mun.add.conf.natsup <- p2.labs.mun.natsup + geom_linerange(data = preds.psalow.re2.mun.natsup,
                                                              aes(y=estimate,
                                                                  x=first_answer_factor,
                                                                  ymin=conf.low,
                                                                  ymax=conf.high),
                                                              position=position_nudge(x=-0.05),
                                                              linewidth=1.2) +
  geom_linerange(data=preds.psahigh.re2.mun.natsup,
                 aes(y=estimate,
                     x=first_answer_factor,
                     ymin=conf.low,
                     ymax=conf.high),
                 position=position_nudge(x= 0.05),
                 linewidth=1.2)

re2.mun.plot.natsup <- p2.mun.add.conf.natsup + labs(tag = "B:") +
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=14),
        strip.text.x = element_text(size=10),
        plot.tag.position = c(0.16, 0.974))

F2B <- re2.mun.plot.natsup + annotate('text', x=c(1), y=c(1.75),
                                      label=c("**"), size=8)

##### Figure F2C: Municipality decides to close kindergarten and county and national government support. ####

preds.psalow.re2.mun.both <- plot_predictions(m2,
                                              newdata = datagrid(first_answer_factor = 1:4,
                                                                 psa=0.44,
                                                                 support = c("County & National Government Support"),
                                                                 tier = c("M")),
                                              by=c("support", "psa",
                                                   "first_answer_factor"),
                                              gray=T,
                                              conf_level = 0.84,
                                              draw = F)

preds.psahigh.re2.mun.both <- plot_predictions(m2,
                                               newdata = datagrid(first_answer_factor = 1:4,
                                                                  psa=0.88,
                                                                  support = c("County & National Government Support"),
                                                                  tier = c("M")),
                                               by=c("support", "psa",
                                                    "first_answer_factor"),
                                               gray=T,
                                               conf_level = 0.84,
                                               draw = F)

plot.re2.mun.both <- plot_predictions(m2,
                                      newdata = datagrid(first_answer_factor = 1:4,
                                                         psa=c(0.44, 0.88),
                                                         support = c("County & National Government Support"),
                                                         tier = c("M")),
                                      by=c("support", "psa",
                                           "first_answer_factor"),
                                      gray=T,
                                      conf_level = 0.95,
                                      draw = F)

plot2.raw.mun.both <- ggplot(plot.re2.mun.both,
                             aes(x=first_answer_factor,
                                 y=estimate,
                                 group=first_answer_factor, shape=psa)) +
  geom_hline(yintercept = c(1,2,3,4), color='gray') +
  facet_grid(.~support) + geom_pointrange(aes(ymin=conf.low, ymax=conf.high,
                                              group=psa),
                                          position = position_dodge(width=0.2), fatten=7)

p2.labs.mun.both <- plot2.raw.mun.both + labs(x="", y="",
                                              shape="Preference for\nSubnational Authority") +
  scale_shape_discrete(labels=c("Mean - 1SD (0.44)", "Mean + 1SD (0.88)")) +
  theme_classic() + theme(legend.position = "bottom",
                          plot.title = element_text(hjust=0.5,
                                                    vjust=1.5),
                          axis.title.y = element_text(size = 18,
                                                      vjust=4.5))

p2.mun.add.conf.both <- p2.labs.mun.both + geom_linerange(data = preds.psalow.re2.mun.both,
                                                          aes(y=estimate,
                                                              x=first_answer_factor,
                                                              ymin=conf.low,
                                                              ymax=conf.high),
                                                          position=position_nudge(x=-0.05),
                                                          linewidth=1.2) +
  geom_linerange(data=preds.psahigh.re2.mun.both,
                 aes(y=estimate,
                     x=first_answer_factor,
                     ymin=conf.low,
                     ymax=conf.high),
                 position=position_nudge(x= 0.05),
                 linewidth=1.2)

re2.mun.plot.both <- p2.mun.add.conf.both + labs(tag = "C:") +
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=14),
        strip.text.x = element_text(size=10),
        plot.tag.position = c(0.16, 0.974))

F2C <- re2.mun.plot.both

figures_f2a_c <- ggarrange(F2A, F2B, F2C, nrow=1, ncol=3,
                           common.legend = T, legend = 'none') +
  plot_annotation(title="Municipality decides to close kindergarten") & theme(
    plot.title=element_text(hjust = .5, size=18)
  )

#### Figures F2D-F: County decides to close upper secondary school. ####
##### Figure F2D: County decides to close upper secondary school and municipality supports. ####

preds.psalow.re2.cou <- plot_predictions(m2,
                                         newdata = datagrid(first_answer_factor = 1:4,
                                                            psa=0.44,
                                                            support = c("Respondent's Municipality Supports"),
                                                            tier = c("C")),
                                         by=c("support", "psa",
                                              "first_answer_factor"),
                                         gray=T,
                                         conf_level = 0.84,
                                         draw = F)

preds.psahigh.re2.cou <- plot_predictions(m2,
                                          newdata = datagrid(first_answer_factor = 1:4,
                                                             psa=0.88,
                                                             support = c("Respondent's Municipality Supports"),
                                                             tier = c("C")),
                                          by=c("support", "psa",
                                               "first_answer_factor"),
                                          gray=T,
                                          conf_level = 0.84,
                                          draw = F)

plot.re2.cou <- plot_predictions(m2,
                                 newdata = datagrid(first_answer_factor = 1:4,
                                                    psa=c(0.44, 0.88),
                                                    support = c("Respondent's Municipality Supports"),
                                                    tier = c("C")),
                                 by=c("support", "psa",
                                      "first_answer_factor"),
                                 gray=T,
                                 conf_level = 0.95,
                                 draw = F)

plot2.raw.cou <- ggplot(plot.re2.cou,
                        aes(x=first_answer_factor,
                            y=estimate,
                            group=first_answer_factor, shape=psa)) +
  geom_hline(yintercept = c(1,2,3,4), color='gray') +
  facet_grid(.~support) + geom_pointrange(aes(ymin=conf.low, ymax=conf.high,
                                              group=psa),
                                          position = position_dodge(width=0.2), fatten=7)

p2.labs.cou <- plot2.raw.cou + labs(x="",
                                    y="Willingness to accept decision\nwhen support is provided by another tier",
                                    shape="Preference for\nSubnational Authority") +
  scale_shape_discrete(labels=c("Mean - 1SD (0.44)", "Mean + 1SD (0.88)")) +
  theme_classic() + theme(legend.position = "bottom",
                          plot.title = element_text(hjust=0.5,
                                                    vjust=1.5),
                          axis.title.y = element_text(size = 18,
                                                      vjust = 1.5))

p2.cou.add.conf <- p2.labs.cou + geom_linerange(data = preds.psalow.re2.cou,
                                                aes(y=estimate,
                                                    x=first_answer_factor,
                                                    ymin=conf.low,
                                                    ymax=conf.high),
                                                position=position_nudge(x=-0.05),
                                                linewidth=1.2) +
  geom_linerange(data=preds.psahigh.re2.cou,
                 aes(y=estimate,
                     x=first_answer_factor,
                     ymin=conf.low,
                     ymax=conf.high),
                 position=position_nudge(x= 0.05),
                 linewidth=1.2)

re2.cou.plot <- p2.cou.add.conf + labs(tag = "D:") +
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=14),
        strip.text.x = element_text(size=10),
        plot.tag.position = c(0.235, 0.974))
F2D <- re2.cou.plot + annotate('text', x=3, y=3.5, label="**", size=8)

##### Figure F2E: County decides to close upper secondary school and national government supports. ####

preds.psalow.re2.cou.natsup <- plot_predictions(m2,
                                                newdata = datagrid(first_answer_factor = 1:4,
                                                                   psa=0.44,
                                                                   support = c("National Government Supports"),
                                                                   tier = c("C")),
                                                by=c("support", "psa",
                                                     "first_answer_factor"),
                                                gray=T,
                                                conf_level = 0.84,
                                                draw = F)

preds.psahigh.re2.cou.natsup <- plot_predictions(m2,
                                                 newdata = datagrid(first_answer_factor = 1:4,
                                                                    psa=0.88,
                                                                    support = c("National Government Supports"),
                                                                    tier = c("C")),
                                                 by=c("support", "psa",
                                                      "first_answer_factor"),
                                                 gray=T,
                                                 conf_level = 0.84,
                                                 draw = F)

plot.re2.cou.natsup <- plot_predictions(m2,
                                        newdata = datagrid(first_answer_factor = 1:4,
                                                           psa=c(0.44, 0.88),
                                                           support = c("National Government Supports"),
                                                           tier = c("C")),
                                        by=c("support", "psa",
                                             "first_answer_factor"),
                                        gray=T,
                                        conf_level = 0.95,
                                        draw = F)

plot2.raw.cou.natsup <- ggplot(plot.re2.cou.natsup,
                               aes(x=first_answer_factor,
                                   y=estimate,
                                   group=first_answer_factor, shape=psa)) +
  geom_hline(yintercept = c(1,2,3,4), color='gray') +
  facet_grid(.~support) + geom_pointrange(aes(ymin=conf.low, ymax=conf.high,
                                              group=psa),
                                          position = position_dodge(width=0.2), fatten=7)

p2.labs.cou.natsup <- plot2.raw.cou.natsup + labs(x="", y="",
                                                  shape="Preference for\nSubnational Authority") +
  scale_shape_discrete(labels=c("Mean - 1SD (0.44)", "Mean + 1SD (0.88)")) +
  theme_classic() + theme(legend.position = "bottom",
                          plot.title = element_text(hjust=0.5,
                                                    vjust=1.5),
                          axis.title.y = element_text(size = 18,
                                                      vjust=4.5))

p2.cou.add.conf.natsup <- p2.labs.cou.natsup + geom_linerange(data = preds.psalow.re2.cou.natsup,
                                                              aes(y=estimate,
                                                                  x=first_answer_factor,
                                                                  ymin=conf.low,
                                                                  ymax=conf.high),
                                                              position=position_nudge(x=-0.05),
                                                              linewidth=1.2) +
  geom_linerange(data=preds.psahigh.re2.cou.natsup,
                 aes(y=estimate,
                     x=first_answer_factor,
                     ymin=conf.low,
                     ymax=conf.high),
                 position=position_nudge(x= 0.05),
                 linewidth=1.2)

re2.cou.plot.natsup <- p2.cou.add.conf.natsup + labs(tag = "E:") +
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=14),
        strip.text.x = element_text(size=10),
        plot.tag.position = c(0.16, 0.974))

F2E <- re2.cou.plot.natsup + annotate('text', x=c(1), y=c(1.9),
                                      label=c("**"), size=8)

##### Figure F2F: County decides to close upper secondary school and municipality and national government support. ####

preds.psalow.re2.cou.both <- plot_predictions(m2,
                                              newdata = datagrid(first_answer_factor = 1:4,
                                                                 psa=0.44,
                                                                 support = c("Municipality & National Government Support"),
                                                                 tier = c("C")),
                                              by=c("support", "psa",
                                                   "first_answer_factor"),
                                              gray=T,
                                              conf_level = 0.84,
                                              draw = F)

preds.psahigh.re2.cou.both <- plot_predictions(m2,
                                               newdata = datagrid(first_answer_factor = 1:4,
                                                                  psa=0.88,
                                                                  support = c("Municipality & National Government Support"),
                                                                  tier = c("C")),
                                               by=c("support", "psa",
                                                    "first_answer_factor"),
                                               gray=T,
                                               conf_level = 0.84,
                                               draw = F)

plot.re2.cou.both <- plot_predictions(m2,
                                      newdata = datagrid(first_answer_factor = 1:4,
                                                         psa=c(0.44, 0.88),
                                                         support = c("Municipality & National Government Support"),
                                                         tier = c("C")),
                                      by=c("support", "psa",
                                           "first_answer_factor"),
                                      gray=T,
                                      conf_level = 0.95,
                                      draw = F)

plot2.raw.cou.both <- ggplot(plot.re2.cou.both,
                             aes(x=first_answer_factor,
                                 y=estimate,
                                 group=first_answer_factor, shape=psa)) +
  geom_hline(yintercept = c(1,2,3,4), color='gray') +
  facet_grid(.~support) + geom_pointrange(aes(ymin=conf.low, ymax=conf.high,
                                              group=psa),
                                          position = position_dodge(width=0.2), fatten=7)

p2.labs.cou.both <- plot2.raw.cou.both + labs(x="", y="",
                                              shape="Preference for\nSubnational Authority") +
  scale_shape_discrete(labels=c("Mean - 1SD (0.44)", "Mean + 1SD (0.88)")) +
  theme_classic() + theme(legend.position = "bottom",
                          plot.title = element_text(hjust=0.5,
                                                    vjust=1.5),
                          axis.title.y = element_text(size = 18,
                                                      vjust=4.5))

p2.cou.add.conf.both <- p2.labs.cou.both + geom_linerange(data = preds.psalow.re2.cou.both,
                                                          aes(y=estimate,
                                                              x=first_answer_factor,
                                                              ymin=conf.low,
                                                              ymax=conf.high),
                                                          position=position_nudge(x=-0.05),
                                                          linewidth=1.2) +
  geom_linerange(data=preds.psahigh.re2.cou.both,
                 aes(y=estimate,
                     x=first_answer_factor,
                     ymin=conf.low,
                     ymax=conf.high),
                 position=position_nudge(x= 0.05),
                 linewidth=1.2)

re2.cou.plot.both <- p2.cou.add.conf.both + labs(tag = "F:") +
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=14),
        strip.text.x = element_text(size=10),
        plot.tag.position = c(0.15, 0.974))

F2F <- re2.cou.plot.both

figures_f2d_f <- ggarrange(F2D, F2E, F2F, nrow=1, ncol=3,
                           common.legend = T, legend = 'none') +
  plot_annotation(title="County decides to close upper secondary school") & theme(
    plot.title=element_text(hjust = .5, size=18)
  )

#### Figures F2G-I: National government decides to close a department of a university (college). ####

##### Figure F2G: National government decides to close a department of a university (college) and the municipality supports. ####

preds.psalow.re2.nat <- plot_predictions(m2,
                                         newdata = datagrid(first_answer_factor = 1:4,
                                                            psa=0.44,
                                                            support = c("Respondent's Municipality Supports"),
                                                            tier = c("N")),
                                         by=c("support", "psa",
                                              "first_answer_factor"),
                                         gray=T,
                                         conf_level = 0.84,
                                         draw = F)

preds.psahigh.re2.nat <- plot_predictions(m2,
                                          newdata = datagrid(first_answer_factor = 1:4,
                                                             psa=0.88,
                                                             support = c("Respondent's Municipality Supports"),
                                                             tier = c("N")),
                                          by=c("support", "psa",
                                               "first_answer_factor"),
                                          gray=T,
                                          conf_level = 0.84,
                                          draw = F)

plot.re2.nat <- plot_predictions(m2,
                                 newdata = datagrid(first_answer_factor = 1:4,
                                                    psa=c(0.44, 0.88),
                                                    support = c("Respondent's Municipality Supports"),
                                                    tier = c("N")),
                                 by=c("support", "psa",
                                      "first_answer_factor"),
                                 gray=T,
                                 conf_level = 0.95,
                                 draw = F)

plot2.raw.nat <- ggplot(plot.re2.nat,
                        aes(x=first_answer_factor,
                            y=estimate,
                            group=first_answer_factor, shape=psa)) +
  geom_hline(yintercept = c(1,2,3,4), color='gray') +
  facet_grid(.~support) + geom_pointrange(aes(ymin=conf.low, ymax=conf.high,
                                              group=psa),
                                          position = position_dodge(width=0.2), fatten=7)

p2.labs.nat <- plot2.raw.nat + labs(x="", y="",
                                    shape="Preference for\nSubnational Authority") +
  scale_shape_discrete(labels=c("Mean - 1SD (0.44)", "Mean + 1SD (0.88)")) +
  theme_classic() + theme(legend.position = "bottom",
                          plot.title = element_text(hjust=0.5,
                                                    vjust=1.5),
                          axis.title.y = element_text(size = 18,
                                                      vjust=4.5))

p2.nat.add.conf <- p2.labs.nat + geom_linerange(data = preds.psalow.re2.nat,
                                                aes(y=estimate,
                                                    x=first_answer_factor,
                                                    ymin=conf.low,
                                                    ymax=conf.high),
                                                position=position_nudge(x=-0.05),
                                                linewidth=1.2) +
  geom_linerange(data=preds.psahigh.re2.nat,
                 aes(y=estimate,
                     x=first_answer_factor,
                     ymin=conf.low,
                     ymax=conf.high),
                 position=position_nudge(x= 0.05),
                 linewidth=1.2)

re2.nat.plot <- p2.nat.add.conf + labs(tag = "G:") +
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=14),
        strip.text.x = element_text(size=10),
        plot.tag.position = c(0.16, 0.972))

F2G <- re2.nat.plot + annotate('text', x=3, y=3.6, label="**", size=8)

##### Figure F2H: National government decides to close a department of a university (college) and county supports. ####

preds.psalow.re2.nat.cousup <- plot_predictions(m2,
                                                newdata = datagrid(first_answer_factor = 1:4,
                                                                   psa=0.44,
                                                                   support = c("Respondent's County Supports"),
                                                                   tier = c("N")),
                                                by=c("support", "psa",
                                                     "first_answer_factor"),
                                                gray=T,
                                                conf_level = 0.84,
                                                draw = F)

preds.psahigh.re2.nat.cousup <- plot_predictions(m2,
                                                 newdata = datagrid(first_answer_factor = 1:4,
                                                                    psa=0.88,
                                                                    support = c("Respondent's County Supports"),
                                                                    tier = c("N")),
                                                 by=c("support", "psa",
                                                      "first_answer_factor"),
                                                 gray=T,
                                                 conf_level = 0.84,
                                                 draw = F)

plot.re2.nat.cousup <- plot_predictions(m2,
                                        newdata = datagrid(first_answer_factor = 1:4,
                                                           psa=c(0.44, 0.88),
                                                           support = c("Respondent's County Supports"),
                                                           tier = c("N")),
                                        by=c("support", "psa",
                                             "first_answer_factor"),
                                        gray=T,
                                        conf_level = 0.95,
                                        draw = F)

plot2.raw.nat.cousup <- ggplot(plot.re2.nat.cousup,
                               aes(x=first_answer_factor,
                                   y=estimate,
                                   group=first_answer_factor, shape=psa)) +
  geom_hline(yintercept = c(1,2,3,4), color='gray') +
  facet_grid(.~support) + geom_pointrange(aes(ymin=conf.low, ymax=conf.high,
                                              group=psa),
                                          position = position_dodge(width=0.2), fatten=7)

p2.labs.nat.cousup <- plot2.raw.nat.cousup + labs(x="Willingness to accept initial decision", y="",
                                                  shape="Preference for\nSubnational Authority") +
  scale_shape_discrete(labels=c("Mean - 1SD (0.44)", "Mean + 1SD (0.88)")) +
  theme_classic() + theme(legend.position = "bottom",
                          plot.title = element_text(hjust=0.5,
                                                    vjust=1.5),
                          axis.title.y = element_text(size = 18,
                                                      vjust=4.5))

p2.nat.add.conf.cousup <- p2.labs.nat.cousup + geom_linerange(data = preds.psalow.re2.nat.cousup,
                                                              aes(y=estimate,
                                                                  x=first_answer_factor,
                                                                  ymin=conf.low,
                                                                  ymax=conf.high),
                                                              position=position_nudge(x=-0.05),
                                                              linewidth=1.2) +
  geom_linerange(data=preds.psahigh.re2.nat.cousup,
                 aes(y=estimate,
                     x=first_answer_factor,
                     ymin=conf.low,
                     ymax=conf.high),
                 position=position_nudge(x= 0.05),
                 linewidth=1.2)

re2.nat.plot.cousup <- p2.nat.add.conf.cousup + labs(tag = "H:") +
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=14),
        strip.text.x = element_text(size=10),
        plot.tag.position = c(0.16, 0.972))

F2H <- re2.nat.plot.cousup + annotate('text', x=3, y=3.25,
                                      label="**", size=8)

##### Figure F2I: National government decides to close a department of a university (college) and municipality and county supports. ####

preds.psalow.re2.nat.both <- plot_predictions(m2,
                                              newdata = datagrid(first_answer_factor = 1:4,
                                                                 psa=0.44,
                                                                 support = c("Municipality & County Support"),
                                                                 tier = c("N")),
                                              by=c("support", "psa",
                                                   "first_answer_factor"),
                                              gray=T,
                                              conf_level = 0.84,
                                              draw = F)

preds.psahigh.re2.nat.both <- plot_predictions(m2,
                                               newdata = datagrid(first_answer_factor = 1:4,
                                                                  psa=0.88,
                                                                  support = c("Municipality & County Support"),
                                                                  tier = c("N")),
                                               by=c("support", "psa",
                                                    "first_answer_factor"),
                                               gray=T,
                                               conf_level = 0.84,
                                               draw = F)

plot.re2.nat.both <- plot_predictions(m2,
                                      newdata = datagrid(first_answer_factor = 1:4,
                                                         psa=c(0.44, 0.88),
                                                         support = c("Municipality & County Support"),
                                                         tier = c("N")),
                                      by=c("support", "psa",
                                           "first_answer_factor"),
                                      gray=T,
                                      conf_level = 0.95,
                                      draw = F)

plot2.raw.nat.both <- ggplot(plot.re2.nat.both,
                             aes(x=first_answer_factor,
                                 y=estimate,
                                 group=first_answer_factor, shape=psa)) +
  geom_hline(yintercept = c(1,2,3,4), color='gray') +
  facet_grid(.~support) + geom_pointrange(aes(ymin=conf.low, ymax=conf.high,
                                              group=psa),
                                          position = position_dodge(width=0.2), fatten=7)

p2.labs.nat.both <- plot2.raw.nat.both + labs(x="", y="",
                                              shape="Preference for\nSubnational Authority") +
  scale_shape_discrete(labels=c("Mean - 1SD (0.44)", "Mean + 1SD (0.88)")) +
  theme_classic() + theme(legend.position = "bottom",
                          plot.title = element_text(hjust=0.5,
                                                    vjust=1.5),
                          axis.title.y = element_text(size = 18,
                                                      vjust=4.5))

p2.nat.add.conf.both <- p2.labs.nat.both + geom_linerange(data = preds.psalow.re2.nat.both,
                                                          aes(y=estimate,
                                                              x=first_answer_factor,
                                                              ymin=conf.low,
                                                              ymax=conf.high),
                                                          position=position_nudge(x=-0.05),
                                                          linewidth=1.2) +
  geom_linerange(data=preds.psahigh.re2.nat.both,
                 aes(y=estimate,
                     x=first_answer_factor,
                     ymin=conf.low,
                     ymax=conf.high),
                 position=position_nudge(x= 0.05),
                 linewidth=1.2)

re2.nat.plot.both <- p2.nat.add.conf.both + labs(tag = "I:") +
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=14),
        strip.text.x = element_text(size=10),
        plot.tag.position = c(0.16, 0.972))

F2I <- re2.nat.plot.both

figures_f2g_i <- ggarrange(F2G, F2H, F2I, nrow=1, ncol=3,
                           common.legend = T, legend = 'bottom') +
  plot_annotation(title="National government decides to close a\ndepartment of a university (college)") & theme(
    plot.title=element_text(hjust = .5, size=18))

plots_all_re <- ggarrange(figures_f2a_c, figures_f2d_f, figures_f2g_i, nrow = 3, ncol = 1)

# Save Figure F2 as a .jpeg file:
ggsave("Figure_F2.jpeg",
       width = 11, height = 15, dpi = 300)

## END ####
