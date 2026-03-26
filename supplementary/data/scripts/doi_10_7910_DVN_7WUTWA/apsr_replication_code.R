##############################
#### REPLICATION FILES FOR ###
###    "CROSSING OVER"     ###
### NAPOLIO & GROSE (2021) ###
##############################

# Set working directory
# Please set your working directory to where the data and files are saved.
# The below code to set the working directory is provided as an example,
# but you will need to write it where your data are located on your drive

setwd("C:/Users/Nicho/Dropbox/Napolio/Works in Progress/83rd Senate/Final Data/apsr_replication")

# Load necessary packages (please install packages using `install.packages()` before running)

library(sjPlot)
library(plyr)
library(dplyr)
library(ggplot2)
library(extrafont)
library(sandwich)
library(stargazer)
library(grDevices)

# Load necessary data
load("apsr_legislator_data.Rdata") # Data for ideal points (manuscript)
load("apsr_items_data.Rdata") # Data for cutpoints (manuscript)
load("apsr_overtime_cutpoints.Rdata") # Data for observational analysis (manuscript)
load("apsr_age_data.Rdata") # Data for predicting senator deaths (appendix)
load("apsr_legislator_data_three_regimes.Rdata") # Data for ideal points, three regimes only (appendix)
load("apsr_items_data_house.Rdata") # Data for cutpoints for House (appendix)
load("apsr_items_data_passage.Rdata") # Data for cutpoints, passage only (appendix)
load("apsr_items_data_not_passage.Rdata") # Data for cutpoints, non-passage only (appendix)
load("apsr_legislator_data_passage.Rdata") # Data for ideal points, passage only (appendix)
load("apsr_legislator_data_not_passage.Rdata") # Data for ideal points, non-passage only (appendix)

#### Figure 1 ####
mod <- lm(d1_cutpoint ~ regime, items) # Generate mean cutpoints by regime via OLS
cutpointsd1 <- data.frame(get_model_data(mod, type = "pred")$regime) # Extract means
cutpointsd1$regime <- unique(items$regime) # Label observations
cutpointsd1 <-
  join(cutpointsd1, distinct(items[c("regime", "party_control")]), type =
         "left") # Merge in party control for each regime

size <- data.frame(regime = unique(items$regime), # Count votes per regime to scale points
                   size = as.numeric(table(items$regime)))

cutpointsd1 <- join(cutpointsd1, size, type="left") # Merge in votes per regime

cutpointsd1$party_control <- 
  factor(cutpointsd1$party_control, levels = c("D", "R")) # Factorize party control variable for easy plotting

cairo_pdf(file = "Figures/fig1.pdf",
          width = 10,
          height = 3)
ggplot(cutpointsd1,
       aes(
         x = regime,
         y = predicted,
         color = party_control,
         pch = party_control
       )) + geom_point(aes(size = (size))) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0) +
  scale_color_manual(
    values = c("Blue", "Red"),
    name = "Party Control",
    labels = c("Democratic", "Republican")
  ) + theme_minimal() +
  ylim(c(-2, 2)) + xlab("") +
  theme(text = element_text(family = "CMU Serif")) + ylab("") +
  scale_shape_manual(
    name = "Party Control",
    values = 15:16,
    labels = c("Democratic", "Republican")
  ) +
  theme(text = element_text(size = 16)) +
  scale_size_continuous(name = "Count of Votes", range = c(2.5, 4)) +
  guides(size = FALSE)
dev.off()

#### Table 1 ####
modcutpoint1 <- lm(d1_cutpoint ~ party_control, items) # All regimes
modcutpoint2 <- lm(d1_cutpoint ~ party_control, # Regimes 5-7
                   items[items$regime %in% c("Regime 5",
                                             "Regime 6", 
                                             "Regime 7"), ])
modcutpoint3 <- lm(d1_cutpoint ~ party_control, # Second session regimes
                   items[items$regime %in% c("Regime 6",
                                             "Regime 7", 
                                             "Regime 8",
                                             "Regime 9"), ])

vcovs <- list( # Generate clustered standard errors
  vcovCL(modcutpoint1, items$regime),
  vcovCL(modcutpoint2, items$regime[items$regime %in% c("Regime 5",
                                                        "Regime 6", 
                                                        "Regime 7")]),
  vcovCL(modcutpoint3, items$regime[items$regime %in% c("Regime 6",
                                                        "Regime 7", 
                                                        "Regime 8",
                                                        "Regime 9")])
)

ses <- lapply(vcovs, diag) # Extract clustered standard errors
ses <- lapply(ses, sqrt)

stargazer(modcutpoint1, modcutpoint2, modcutpoint3, 
          keep.stat = "n",
          dep.var.labels = "First Dimension Cutpoint",
          covariate.labels = "Democratic Majority",
          column.labels = c("All Regimes", "Regimes 5-7", "Second Session Regimes"),
          keep="party_control", se=ses, star.cutoffs = .01)

#### Figure 2 ####
mod <- lm(scale(sen_slim$nominate_mid_1) ~ factor(congress),
          sen_slim) # Generate means by Congress

all_cutpoints <- data.frame(get_model_data(mod, type = "pred")) # Extract means
names(all_cutpoints)[1] <- "congress" # Rename first column
all_cutpoints <-
  join(all_cutpoints, distinct(sen_slim[c("congress", "party_control")]), type =
         "left") # Merge cutpoints with party control

cairo_pdf(file = "Figures/fig2.pdf",
          width = 10,
          height = 3)
ggplot(all_cutpoints, aes(x = congress, y = congress.predicted)) +
  geom_point(aes(color = party_control)) +
  geom_errorbar(aes(ymin = congress.conf.low, ymax = congress.conf.high, color =
                      party_control),
                width = 0) +
  theme_minimal() + scale_color_manual(name = "Party Control", values =
                                         c("Blue", "Red")) +
  scale_x_continuous(breaks = seq(80, 116, 5)) + ylab("") +
  xlab("Congress") + theme(text = element_text(family = "CMU Serif"))
dev.off()

#### Figure 3 ####
cairo_pdf(file="Figures/fig3.pdf", width=10, height=5)
ggplot(final[!is.na(final$regime), ], aes(x=dim1)) +
  geom_density(aes(fill=party), alpha=.5) + facet_wrap( ~ party_control2, nrow = 1) + theme_minimal() +
  theme(text = element_text(family = "CMU Serif")) +
  xlab("Standardized First Dimension Ideal Point") +
  ylab("Density") + theme(text = element_text(size=16)) +
  scale_fill_manual(name = "Party", values = c("Blue", "Red"))
dev.off()

#### Table 2 ####
modD1 <- # Democrats all regimes
  lm(dim1 ~ party_control + name, final[final$party == "Democratic", ])
modR1 <- # Republicans all regimes
  lm(dim1 ~ party_control + name, final[final$party == "Republican", ])
modD2 <- # Democrats regimes 5-7
  lm(dim1 ~ party_control + name, final[final$party == "Democratic" &
                                          final$regime %in% c("Regime 5",
                                                              "Regime 6", 
                                                              "Regime 7"), ])
modR2 <- # Republicans regimes 5-7
  lm(dim1 ~ party_control + name, final[final$party == "Republican" &
                                          final$regime %in% c("Regime 5",
                                                              "Regime 6", 
                                                              "Regime 7"), ])
modD3 <- # Democrats second session regimes
  lm(dim1 ~ party_control + name, final[final$party == "Democratic" &
                                          final$regime %in% c("Regime 6",
                                                              "Regime 7",
                                                              "Regime 8",
                                                              "Regime 9"), ])
modR3 <- # Republicans second session regimes
  lm(dim1 ~ party_control + name, final[final$party == "Republican" &
                                          final$regime %in% c("Regime 6",
                                                              "Regime 7",
                                                              "Regime 8",
                                                              "Regime 9"), ])
vcovs <- list( # Generate clustered standard errors
  vcovCL(modD1, final$regime[final$party == "Democratic" &
                               !is.na(final$regime)]),
  vcovCL(modR1, final$regime[final$party=="Republican" &
                               !is.na(final$regime)]),
  vcovCL(modD2, final$regime[final$party=="Democratic" &
                               final$regime %in% c("Regime 5",
                                                   "Regime 6", 
                                                   "Regime 7")&
                               !is.na(final$regime)]),
  vcovCL(modR2, final$regime[final$party=="Republican" &
                               final$regime %in% c("Regime 5",
                                                   "Regime 6", 
                                                   "Regime 7")&
                               !is.na(final$regime)]),
  vcovCL(modD3, final$regime[final$party=="Democratic" &
                               final$regime %in% c("Regime 6",
                                                   "Regime 7",
                                                   "Regime 8",
                                                   "Regime 9")&
                               !is.na(final$regime)]),
  vcovCL(modR3, final$regime[final$party=="Republican" &
                               final$regime %in% c("Regime 6",
                                                   "Regime 7",
                                                   "Regime 8",
                                                   "Regime 9")&
                               !is.na(final$regime)])
)


ses <- lapply(vcovs, diag) # Extract clustered standard errors
ses <- lapply(ses, sqrt)
stargazer(
  modD1,
  modR1,
  modD2,
  modR2,
  modD3,
  modR3,
  keep = c("party_control"),
  covariate.labels = "Democratic Majority",
  keep.stat = "n", se = ses,
  dep.var.labels = "First Dimension Ideal Point",
  column.labels = c("All Regimes", "Regimes 5-7", "Second Session Regimes"),
  column.separate = c(2,2,2), star.cutoffs = .01
)

#### APPENDICES ####

#### Appendix B ####
#### Table B1 ####
deathOLS <- # Predict deaths with OLS
  lm(died ~ age * party + nominate_dim1 + nominate_dim2, age)
deathLogit <- # Predict deaths with logistic regression
  glm(died ~ age * party + nominate_dim1 + nominate_dim2,
      age,
      family = binomial(link = "logit"))

stargazer(deathOLS, deathLogit, 
          keep.stat = c("n", "adj.rsq", "ll", "f"),
          star.cutoffs = c(.05, .01))

#### Appendix C ####
#### Table C1 ####
modcutpoint1 <- lm(d2_cutpoint ~ party_control, items) # All regimes
modcutpoint2 <- lm(d2_cutpoint ~ party_control, # Regimes 5-7
                   items[items$regime %in% c("Regime 5",
                                             "Regime 6", 
                                             "Regime 7"), ])
modcutpoint3 <- lm(d2_cutpoint ~ party_control, # Second session regimes
                   items[items$regime %in% c("Regime 6",
                                             "Regime 7", 
                                             "Regime 8",
                                             "Regime 9"), ])

vcovs <- list( # Generate clustered standard errors
  vcovCL(modcutpoint1, items$regime),
  vcovCL(modcutpoint2, items$regime[items$regime %in% c("Regime 5",
                                                        "Regime 6", 
                                                        "Regime 7")]),
  vcovCL(modcutpoint3, items$regime[items$regime %in% c("Regime 6",
                                                        "Regime 7", 
                                                        "Regime 8",
                                                        "Regime 9")])
)

ses <- lapply(vcovs, diag) # Extract clustered standard errors
ses <- lapply(ses, sqrt)

stargazer(modcutpoint1, modcutpoint2, modcutpoint3, 
          keep.stat = "n",
          dep.var.labels = "Second Dimension Cutpoint",
          covariate.labels = "Democratic Majority",
          column.labels = c("All Regimes", "Regimes 5-7", "Second Session Regimes"),
          keep="party_control", se=ses, star.cutoffs = .05)

#### Table C2 ####
modD1 <- # Democrats all regimes
  lm(dim2 ~ party_control + name, final[final$party == "Democratic", ])
modR1 <- # Republicans all regimes
  lm(dim2 ~ party_control + name, final[final$party == "Republican", ])
modD2 <- # Democrats regimes 5-7
  lm(dim2 ~ party_control + name, final[final$party == "Democratic" &
                                          final$regime %in% c("Regime 5",
                                                              "Regime 6", 
                                                              "Regime 7"), ])
modR2 <- # Republicans regimes 5-7
  lm(dim2 ~ party_control + name, final[final$party == "Republican" &
                                          final$regime %in% c("Regime 5",
                                                              "Regime 6", 
                                                              "Regime 7"), ])
modD3 <- # Democrats second session regimes
  lm(dim2 ~ party_control + name, final[final$party == "Democratic" &
                                          final$regime %in% c("Regime 6",
                                                              "Regime 7",
                                                              "Regime 8",
                                                              "Regime 9"), ])
modR3 <- # Republicans second session regimes
  lm(dim2 ~ party_control + name, final[final$party == "Republican" &
                                          final$regime %in% c("Regime 6",
                                                              "Regime 7",
                                                              "Regime 8",
                                                              "Regime 9"), ])
vcovs <- list( # Generate clustered standard errors
  vcovCL(modD1, final$regime[final$party == "Democratic" &
                               !is.na(final$regime)]),
  vcovCL(modR1, final$regime[final$party=="Republican" &
                               !is.na(final$regime)]),
  vcovCL(modD2, final$regime[final$party=="Democratic" &
                               final$regime %in% c("Regime 5",
                                                   "Regime 6", 
                                                   "Regime 7")&
                               !is.na(final$regime)]),
  vcovCL(modR2, final$regime[final$party=="Republican" &
                               final$regime %in% c("Regime 5",
                                                   "Regime 6", 
                                                   "Regime 7")&
                               !is.na(final$regime)]),
  vcovCL(modD3, final$regime[final$party=="Democratic" &
                               final$regime %in% c("Regime 6",
                                                   "Regime 7",
                                                   "Regime 8",
                                                   "Regime 9")&
                               !is.na(final$regime)]),
  vcovCL(modR3, final$regime[final$party=="Republican" &
                               final$regime %in% c("Regime 6",
                                                   "Regime 7",
                                                   "Regime 8",
                                                   "Regime 9")&
                               !is.na(final$regime)])
)


ses <- lapply(vcovs, diag) # Extract clustered standard errors
ses <- lapply(ses, sqrt)
stargazer(
  modD1,
  modR1,
  modD2,
  modR2,
  modD3,
  modR3,
  keep = c("party_control"),
  covariate.labels = "Democratic Majority",
  keep.stat = "n", se = ses,
  dep.var.labels = "Second Dimension Ideal Point",
  column.labels = c("All Regimes", "Regimes 5-7", "Second Session Regimes"),
  column.separate = c(2,2,2), star.cutoffs = c(.05, .01)
)

#### Table C3 ####
final_dem <- subset(final, party == "Democratic") # Subset to Democrats only
final_dem$Southern <- ifelse( # Generate indicator for Southern Democrats (ex-confederate states)
  final_dem$state_abbrev %in% c("AL", "AR", "FL",
                                "GA", "LA", "MS", "NC",
                                "SC", "TN", "TX", "VA"),
  1,0
)

mod_DiD1 <- lm(dim2 ~ party_control * Southern + name, final_dem)
mod_DiD2 <- lm(dim2 ~ party_control * Southern + name,
               final_dem[final_dem$regime %in% c("Regime 5", "Regime 6", "Regime 7"),])
mod_DiD3 <- lm(dim2 ~ party_control * Southern + name,
               final_dem[final_dem$regime %in% c("Regime 8", "Regime 6", "Regime 7", "Regime 9"),])
vcov_DiD <- list(
  vcovCL(mod_DiD1, final_dem$regime),
  vcovCL(mod_DiD2, final_dem$regime[final_dem$regime %in% c("Regime 5", "Regime 6", "Regime 7")]),
  vcovCL(mod_DiD3, final_dem$regime[final_dem$regime %in% c("Regime 8", "Regime 6", "Regime 7", "Regime 9")])
)

se_DiD <- lapply(vcov_DiD, diag)
se_DiD <- lapply(se_DiD, sqrt)

stargazer(
  mod_DiD1, mod_DiD2, mod_DiD3,
  keep = c("party_control", "Southern"),
  se = se_DiD,
  covariate.labels = c("Democratic Majority", "Southern Democrat",
                       "Democratic Majority * Southern Democrat"),
  dep.var.labels = "First Dimension Ideal Point",
  keep.stat = c("n"), star.cutoffs = .01
)

#### Appendix E ####

# NOTE: Figure E1 is the same as Figure 3 in the main manuscript #

#### Table E1 ####
sen_slim$party_control <- factor(sen_slim$party_control, levels = c("Republican", "Democratic"))
sen_slim$cutpoint <- scale(sen_slim$nominate_mid_1)
mod <- lm(cutpoint ~ party_control, sen_slim)

vcov_cutpoint <- vcovCL(mod, cluster = sen_slim$congress)

stargazer(mod, se = list(sqrt(diag(vcov_cutpoint))), keep = "party_control",
          keep.stat=c("n"), type = "text", star.cutoffs = .1)

#### Appendix F ####

#### Figure F1 ####
cairo_pdf(file = "Figures/fig_f1.pdf",
          width = 8,
          height = 10)
ggplot(final, aes(x = regime_numeric, y = dim1, color = party)) +
  geom_line() + facet_wrap( ~ stringr::str_to_title(name)) + theme_minimal() + geom_point(pch =
                                                                                            1) +
  scale_color_manual(name = "", values = c("Blue", "Red")) +
  xlab("Regime") + ylab("Standardized First Dimension Ideal Point") +
  theme(axis.text = element_blank(), text = element_text(family = "CMU Serif")) +
  theme(legend.position = c(.9, .05))
dev.off()

#### Figure F2 ####
cairo_pdf(file = "Figures/fig_f2.pdf",
          width = 8,
          height = 10)
ggplot(final, aes(x = regime_numeric, y = dim2, color = party)) +
  geom_line() + facet_wrap( ~ stringr::str_to_title(name)) + theme_minimal() + geom_point(pch =
                                                                                            1) +
  scale_color_manual(name = "", values = c("Blue", "Red")) +
  xlab("Regime") + ylab("Standardized Second Dimension Ideal Point") +
  theme(axis.text = element_blank(), text = element_text(family = "CMU Serif")) +
  theme(legend.position = c(.9, .05))
dev.off()

#### Figure F3 ####
correlations <- rep(NA, 9)
for (i in 1:9) {
  correlations[i] <-
    cor(final$nominate_dim1[final$regime == unique(final$regime)[i]],
        final$dim1[final$regime == unique(final$regime)[i]])
}

correlations <- data.frame(
  correlation = correlations,
  regime = unique(final$regime),
  x = -.45,
  y = 2.5
)

cairo_pdf(file = "Figures/fig_f3.pdf",
          width = 8,
          height = 8)
ggplot(final, aes(x = nominate_dim1, y = dim1)) +
  geom_point() + geom_smooth(method = "lm", color = "black") + facet_wrap( ~
                                                                             regime, nrow = 3) +
  theme_minimal() + xlab("First Dimension NOMINATE Ideal Point") +
  ylab("Standardized First Dimension Ideal Point (MCMC)") +
  theme(text = element_text(family = "CMU Serif")) +
  geom_label(
    data = correlations,
    aes(
      x = x,
      y = y,
      label = paste("Cor =", round(correlation, 2))
    ),
    family = "CMU Serif",
    size = 3.5
  )
dev.off()

#### Appendix G ####

#### Table G1 ####
mod_DiD1 <- lm(dim1 ~ party_control * Southern + name, final_dem)
mod_DiD2 <- lm(dim1 ~ party_control * Southern + name,
               final_dem[final_dem$regime %in% c("Regime 5", "Regime 6", "Regime 7"),])
mod_DiD3 <- lm(dim1 ~ party_control * Southern + name,
               final_dem[final_dem$regime %in% c("Regime 8", "Regime 6", "Regime 7", "Regime 9"),])
vcov_DiD <- list(
  vcovCL(mod_DiD1, final_dem$regime),
  vcovCL(mod_DiD2, final_dem$regime[final_dem$regime %in% c("Regime 5", "Regime 6", "Regime 7")]),
  vcovCL(mod_DiD3, final_dem$regime[final_dem$regime %in% c("Regime 8", "Regime 6", "Regime 7", "Regime 9")])
)

se_DiD <- lapply(vcov_DiD, diag)
se_DiD <- lapply(se_DiD, sqrt)

stargazer(
  mod_DiD1, mod_DiD2, mod_DiD3,
  keep = c("party_control", "party_control:Southern"),
  se = se_DiD,
  covariate.labels = c("Democratic Majority",
                       "Democratic Majority * Southern Democrat"),
  dep.var.labels = "Second Dimension Ideal Point",
  keep.stat = c("n"), star.cutoffs = .01
)


#### Appendix H ####

#### Table H1 ####
final2$party_control <- factor(final2$party_control, levels = c("R", "D"))

modD1_2 <-
  lm(dim1 ~ party_control + name, final2[final2$party == "Democratic", ])
modR1_2 <-
  lm(dim1 ~ party_control + name, final2[final2$party == "Republican", ])
vcovD1_2 <- vcovCL(modD1_2, final2$regime[final2$party == "Democratic"])
vcovR1_2 <- vcovCL(modR1_2, final2$regime[final2$party == "Republican"])

vcovs <- list(vcovD1_2, vcovR1_2)
ses <- list(rep(NA, 2))
for (i in 1:2) {
  ses[[i]] <- sqrt(diag(vcovs[[i]]))
}

stargazer(
  modD1_2,
  modR1_2,
  keep = c("party_control"),
  covariate.labels = "Democratic Majority",
  dep.var.labels = c("First Dimension Ideal Point"),
  column.labels = rep(c("Democrats", "Republicans")),
  keep.stat = c("n", "adj.rsq"), se = ses,
  star.cutoffs = c(0.01)
)

#### Appendix I ####

#### Table I1 ####
items$party_control <- factor(items$party_control, level = c("R", "D"))
house_items$party_control <- factor(house_items$party_control, level = c("R", "D"))

mod_senate1 <- lm(d1_cutpoint ~ party_control, items)
mod_house1 <- lm(d1_cutpoint ~ party_control, house_items)
mod_senate2<- lm(d1_cutpoint ~ party_control, 
                 items[items$regime %in% c("Regime 5", "Regime 6", "Regime 7"),])
mod_house2 <- lm(d1_cutpoint ~ party_control, 
                 house_items[house_items$regime %in% c("Regime 5", "Regime 6", "Regime 7"),])
mod_senate3 <- lm(d1_cutpoint ~ party_control, 
                  items[items$regime %in% c("Regime 5", "Regime 6", "Regime 7",
                                            "Regime 8", "Regime 9"),])
mod_house3 <- lm(d1_cutpoint ~ party_control, 
                 house_items[house_items$regime %in% c("Regime 5", "Regime 6", "Regime 7",
                                                       "Regime 8", "Regime 9"),])

vcovs <- list(vcovCL(mod_senate1, cluster = items$regime),
              vcovCL(mod_senate2, cluster = items$regime[items$regime %in% c("Regime 5", "Regime 6", "Regime 7")]),
              vcovCL(mod_senate3, cluster = items$regime[items$regime %in% c("Regime 5", "Regime 6", "Regime 7", "Regime 8", "Regime 9")]),
              vcovCL(mod_house1, cluster = house_items$regime),
              vcovCL(mod_house2, cluster = house_items$regime[house_items$regime %in% c("Regime 5", "Regime 6", "Regime 7")]),
              vcovCL(mod_house3, cluster = house_items$regime[house_items$regime %in% c("Regime 5", "Regime 6", "Regime 7", "Regime 8", "Regime 9")])
)
ses <- lapply(vcovs, diag)
ses <- lapply(ses, sqrt)

stargazer(mod_senate1, mod_senate2, mod_senate3,
          mod_house1, mod_house2, mod_house3, 
          se = ses,
          keep = "party_control",
          keep.stat = "n", type = "text",
          star.cutoffs = 0.01)



#### Figure I1 ####
mod_senate <- lm(d1_cutpoint ~ regime, items)
cutpointsd1 <- data.frame(get_model_data(mod_senate, type = "pred")$regime)
cutpointsd1$regime <- unique(items$regime)
cutpointsd1 <-
  join(cutpointsd1, distinct(items[c("regime", "party_control")]), type =
         "left")

size <- data.frame(regime = unique(items$regime),
                   size = as.numeric(table(items$regime)))
cutpointsd1 <- join(cutpointsd1, size, type="left")

mod_house <- lm(d1_cutpoint ~ regime, house_items)
cutpointsd1_house <- data.frame(get_model_data(mod_house, type = "pred")$regime)
cutpointsd1_house$regime <- unique(house_items$regime)
cutpointsd1_house <-
  join(cutpointsd1_house, distinct(house_items[c("regime", "party_control")]), type =
         "left")

size <- data.frame(regime = unique(house_items$regime),
                   size = as.numeric(table(house_items$regime)))
cutpointsd1_house <- join(cutpointsd1_house, size, type="left")


cutpointsd1$chamber <- "Senate"
cutpointsd1_house$chamber <- "House"

cutpoints_all <- rbind(cutpointsd1, cutpointsd1_house)

cutpoints_all$party_control <- factor(cutpoints_all$party_control, levels = c("R", "D"))

cairo_pdf(file = "Figures/fig_i1.pdf",
          width = 10, height = 6)
ggplot(cutpoints_all,
       aes(
         x = regime,
         y = predicted,
         color = party_control,
         pch = party_control
       )) + geom_point(aes(size = (size))) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0) +
  scale_color_manual(
    values = c("Red", "Blue"),
    name = "Party Control",
    labels = c("Republican", "Democratic")
  ) + theme_minimal() +
  ylim(c(-2, 2)) + xlab("") +
  theme(text = element_text(family = "CMU Serif")) + ylab("") +
  scale_shape_manual(
    name = "Party Control",
    values = 15:16,
    labels = c("Republican", "Democratic")
  ) + facet_wrap(~chamber, ncol = 1) +
  theme(text = element_text(size = 16)) +
  scale_size_continuous(name = "Count of Votes", range = c(2.5, 4)) +
  guides(size = FALSE)
dev.off()

#### Table I2 ####
placebo <- items

placebo$party_control <- ifelse(placebo$regime=="Regime 1", "D", "R")
modcutpoint_placebo1 <- lm(d1_cutpoint ~ party_control, placebo)
placebo$party_control <- ifelse(placebo$regime=="Regime 2", "D", "R")
modcutpoint_placebo2 <- lm(d1_cutpoint ~ party_control, placebo)
placebo$party_control <- ifelse(placebo$regime=="Regime 3", "D", "R")
modcutpoint_placebo3 <- lm(d1_cutpoint ~ party_control, placebo)
placebo$party_control <- ifelse(placebo$regime=="Regime 4", "D", "R")
modcutpoint_placebo4 <- lm(d1_cutpoint ~ party_control, placebo)
placebo$party_control <- ifelse(placebo$regime=="Regime 5", "D", "R")
modcutpoint_placebo5 <- lm(d1_cutpoint ~ party_control, placebo)
placebo$party_control <- ifelse(placebo$regime=="Regime 6", "D", "R")
modcutpoint_placebo6 <- lm(d1_cutpoint ~ party_control, placebo)
placebo$party_control <- ifelse(placebo$regime=="Regime 7", "D", "R")
modcutpoint_placebo7 <- lm(d1_cutpoint ~ party_control, placebo)
placebo$party_control <- ifelse(placebo$regime=="Regime 8", "D", "R")
modcutpoint_placebo8 <- lm(d1_cutpoint ~ party_control, placebo)
placebo$party_control <- ifelse(placebo$regime=="Regime 9", "D", "R")
modcutpoint_placebo9 <- lm(d1_cutpoint ~ party_control, placebo)


mods <- list(modcutpoint_placebo1,
             modcutpoint_placebo2,
             modcutpoint_placebo3,
             modcutpoint_placebo4,
             modcutpoint_placebo5,
             modcutpoint_placebo6,
             modcutpoint_placebo7,
             modcutpoint_placebo8,
             modcutpoint_placebo9)
vcovs <- lapply(mods, vcovCL, placebo$regime)
ses <- lapply(vcovs, diag)
ses <- lapply(ses, sqrt)
ses <- unlist(ses)
ses <- ses[seq(2,18, 2)]

ses2 <- lapply(mods, vcovCL, placebo$regime)
ses2 <- lapply(ses2, diag)
ses2 <- lapply(ses2, sqrt)

stargazer(mods, se = ses2,
          keep.stat = "n",
          column.labels = paste("Regime", 1:9),
          keep = "party_control",
          covariate.labels = "Democratic Control",
          apply.coef = function(x){-x},
          star.cutoffs = 0.01)

#### Table I3 ####
placebo2 <- final

placebo2$party_control <- ifelse(placebo2$regime=="Regime 1", "D", "R")
modidealpoint_placebo1D <- lm(dim1 ~ party_control + name, placebo2[placebo2$party=="Democratic",])
placebo2$party_control <- ifelse(placebo2$regime=="Regime 2", "D", "R")
modidealpoint_placebo2D <- lm(dim1 ~ party_control + name, placebo2[placebo2$party=="Democratic",])
placebo2$party_control <- ifelse(placebo2$regime=="Regime 3", "D", "R")
modidealpoint_placebo3D <- lm(dim1 ~ party_control + name, placebo2[placebo2$party=="Democratic",])
placebo2$party_control <- ifelse(placebo2$regime=="Regime 4", "D", "R")
modidealpoint_placebo4D <- lm(dim1 ~ party_control + name, placebo2[placebo2$party=="Democratic",])
placebo2$party_control <- ifelse(placebo2$regime=="Regime 5", "D", "R")
modidealpoint_placebo5D <- lm(dim1 ~ party_control + name, placebo2[placebo2$party=="Democratic",])
placebo2$party_control <- ifelse(placebo2$regime=="Regime 6", "D", "R")
modidealpoint_placebo6D <- lm(dim1 ~ party_control + name, placebo2[placebo2$party=="Democratic",])
placebo2$party_control <- ifelse(placebo2$regime=="Regime 7", "D", "R")
modidealpoint_placebo7D <- lm(dim1 ~ party_control + name, placebo2[placebo2$party=="Democratic",])
placebo2$party_control <- ifelse(placebo2$regime=="Regime 8", "D", "R")
modidealpoint_placebo8D <- lm(dim1 ~ party_control + name, placebo2[placebo2$party=="Democratic",])
placebo2$party_control <- ifelse(placebo2$regime=="Regime 9", "D", "R")
modidealpoint_placebo9D <- lm(dim1 ~ party_control, placebo2[placebo2$party=="Democratic",])

placebo2$party_control <- ifelse(placebo2$regime=="Regime 1", "D", "R")
modidealpoint_placebo1R <- lm(dim1 ~ party_control + name, placebo2[placebo2$party=="Republican",])
placebo2$party_control <- ifelse(placebo2$regime=="Regime 2", "D", "R")
modidealpoint_placebo2R <- lm(dim1 ~ party_control + name, placebo2[placebo2$party=="Republican",])
placebo2$party_control <- ifelse(placebo2$regime=="Regime 3", "D", "R")
modidealpoint_placebo3R <- lm(dim1 ~ party_control + name, placebo2[placebo2$party=="Republican",])
placebo2$party_control <- ifelse(placebo2$regime=="Regime 4", "D", "R")
modidealpoint_placebo4R <- lm(dim1 ~ party_control + name, placebo2[placebo2$party=="Republican",])
placebo2$party_control <- ifelse(placebo2$regime=="Regime 5", "D", "R")
modidealpoint_placebo5R <- lm(dim1 ~ party_control + name, placebo2[placebo2$party=="Republican",])
placebo2$party_control <- ifelse(placebo2$regime=="Regime 6", "D", "R")
modidealpoint_placebo6R <- lm(dim1 ~ party_control + name, placebo2[placebo2$party=="Republican",])
placebo2$party_control <- ifelse(placebo2$regime=="Regime 7", "D", "R")
modidealpoint_placebo7R <- lm(dim1 ~ party_control, placebo2[placebo2$party=="Republican",])
placebo2$party_control <- ifelse(placebo2$regime=="Regime 8", "D", "R")
modidealpoint_placebo8R <- lm(dim1 ~ party_control + name, placebo2[placebo2$party=="Republican",])
placebo2$party_control <- ifelse(placebo2$regime=="Regime 9", "D", "R")
modidealpoint_placebo9R <- lm(dim1 ~ party_control + name, placebo2[placebo2$party=="Republican",])

mods <- list(modidealpoint_placebo1D, modidealpoint_placebo1R,
             modidealpoint_placebo2D, modidealpoint_placebo2R,
             modidealpoint_placebo3D, modidealpoint_placebo3R,
             modidealpoint_placebo4D, modidealpoint_placebo4R,
             modidealpoint_placebo5D, modidealpoint_placebo5R,
             modidealpoint_placebo6D, modidealpoint_placebo6R,
             modidealpoint_placebo7D, modidealpoint_placebo7R,
             modidealpoint_placebo8D, modidealpoint_placebo8R,
             modidealpoint_placebo9D, modidealpoint_placebo9R)
vcovsD <- lapply(mods[seq(1,17, 2)], vcovCL, placebo2$regime[placebo2$party=="Democratic"])
vcovsR <- lapply(mods[seq(2,18, 2)], vcovCL, placebo2$regime[placebo2$party=="Republican"])

seD <- as.list(rep(NA, 9))
seR <- as.list(rep(NA, 9))
for(i in 1:9){
  seD[[i]] <- sqrt(diag(vcovsD[[i]]))  
  seR[[i]] <- sqrt(diag(vcovsR[[i]]))
}

ses <- c(seD, seR)

#Dems#
stargazer(mods[seq(1,17, 2)], se = ses[1:9],
          keep="party_control",
          apply.coef = function(x){-x},
          keep.stat = "n", star.cutoffs = .01,
          covariate.labels = "Democratic Majority")

#Reps#
stargazer(mods[seq(2,18,2)], se = ses[10:18],
          keep="party_control",
          apply.coef = function(x){-x},
          keep.stat = "n", star.cutoffs = .01,
          covariate.labels = "Democratic Majority")

#### Appendix J ####

#### Table J1 ####
mod_final <- lm(d1_cutpoint ~ party_control, items3)
mod_other <- lm(d1_cutpoint ~ party_control, items4)

vcov_final <- vcovCL(mod_final, items3$regime)
vcov_other <- vcovCL(mod_other, items4$regime)

ses <- lapply(list(vcov_final, vcov_other), diag)
ses <- lapply(ses, sqrt)

stargazer(mod_final, mod_other,
          se = ses, covariate.labels = "Democratic Control",
          apply.coef = function(x){-x},
          keep = "party_control", star.cutoffs = 0.01)

#### Table J2 ####
modD_passage <- lm(dim1 ~ party_control + name, final3[final3$party=="Democratic",])
modR_passage <- lm(dim1 ~ party_control + name, final3[final3$party=="Republican",])
modD_other <- lm(dim1 ~ party_control + name, final4[final4$party=="Democratic",])
modR_other <- lm(dim1 ~ party_control + name, final4[final4$party=="Republican",])

vcovs <- list(vcovCL(modD_passage, final3$regime[final3$party=="Democratic"]),
              vcovCL(modR_passage, final3$regime[final3$party=="Republican"]),
              vcovCL(modD_other, final4$regime[final4$party=="Democratic"]),
              vcovCL(modR_other, final4$regime[final4$party=="Republican"]))

ses <- lapply(vcovs, diag)
ses <- lapply(ses, sqrt)

stargazer(modD_passage, modR_passage,
          modD_other, modR_other,
          se = ses, keep = "party_control",
          covariate.labels = "Democratic Majority",
          apply.coef = function(x){-x},
          star.cutoffs = c(0.05, 0.01))


