###
# File description: Conflict on the Campaign Trail - Figures and Model Script
# Replication files 
###

#### 1. Loading data and packages ------

rm( list = ls() ) # clearing our environment 

# Loading the necessary packages:
library(rstudioapi)   # for automatic setwd()
library(stargazer)    # regression table
library(effects)      # for effect plots
library(readstata13)  # for reading in the BESP data
library(lme4)
library(lmerTest)
library(sjPlot)
library(coefplot)
library(ggeffects)
library(ggplot2)
library(ggpubr)
library(patchwork)
library(interactions)
library(bruceR)
library(modelsummary)
library(extrafont)
library(gridExtra)
library(dplyr)
library(performance)
library(car)
library(marginaleffects)


# Load in data 

setwd(dirname(getActiveDocumentContext()$path))

getwd()

model_data <- read.dta13("bes_2015_2019_long_v1.dta")
CI_data <- read.dta13("CI_plot.dta")
spend_data <- read.dta13("spending_plot.dta")


#### 2. Prepping the data ------

# The election indicator needs to be converted to a factor 

class(model_data$electionW)

model_data$electionW <- factor(model_data$electionW, levels = c(1, 2, 3), 
                               labels = c(2015, 2017, 2019))

# PID strength variable - is it a factor? 

class(model_data$pid_strength_W) # Yes

# standardizing the spending variables 

summary(model_data$IP_spend_W)

model_data$std_IP_spend_W <-scale(model_data$IP_spend_W)

summary(model_data$OP_spend_W)

model_data$std_OP_spend_W <-scale(model_data$OP_spend_W)

# standardizing the competitiveness index 

summary(model_data$CI_W)

model_data$std_CI_W <-scale(model_data$CI_W)

# convert the spending and CI vars to numeric

class(model_data$std_IP_spend_W)
model_data$std_IP_spend_W <- as.numeric(model_data$std_IP_spend_W)

class(model_data$std_OP_spend_W)
model_data$std_OP_spend_W <- as.numeric(model_data$std_OP_spend_W)

class(model_data$std_CI_W)
model_data$std_CI_W <- as.numeric(model_data$std_CI_W)

#### 3. Descriptive graphs -----

#### Competitiveness plots 

## Competitiveness strip plot

# Convert the year variable into a factor 

year.factor <- factor(CI_data$year)

# Plotting competitiveness values

CI_plot <- ggplot(data=CI_data, aes(x=year.factor, y=CI_)) + 
  geom_jitter(width = 0.25, colour='gray14') 
CI_plot

# Adding mean and titles/labels 

CI_plot2 <- CI_plot + theme_light()+
  stat_summary(fun.y=mean, geom = "crossbar", colour='red', 
               width = 0.7)+
  labs(x="Election", y = "Competitiveness Index")+
  theme(text=element_text(size=16, family="serif"))
CI_plot2

## Competitiveness line-plot


CI_data <- CI_data %>% 
  group_by(year) %>% 
  mutate(mean_CI = mean(CI_, na.rm = TRUE)) %>% 
  ungroup()

CI_change <- ggplot(data=CI_data,                
                    aes(x = year, y = CI_, group = ConstituencyName)) + #setting variables
  geom_point(size=.5) + #adding points to plot
  geom_line()+
  geom_line(aes(x=year, y = mean_CI), color="red",size=1)+
  theme_light()+
  scale_x_continuous(limits=c(2015,2019),
                     breaks = c(2015,2017,2019), 
                     name = "Election year") +    
  #setting the y-axis with limits breaks and labels
  scale_y_continuous(limits=c(0,1), 
                     breaks = c(0,0.25,0.5,0.75,1), 
                     name = "Competitiveness index")+
  theme(text=element_text(size=16, family="serif"))
CI_change

#### b. --- Campaign spending plots ####

## Campaign spend strip plot

# Convert the year variable into a factor 

year.factor <- factor(spend_dat$year)

# PLotting campaign spend values 

spend_plot <- ggplot(data=spend_data, aes(x=year.factor, y=absolute_spend_)) + 
  geom_jitter(width = 0.25, colour='gray14')
spend_plot

# Adding labels and mean for campaign spending 

spend_plot2 <- spend_plot + theme_light()+
  stat_summary(fun.y=mean, geom = "crossbar", colour='red',
               width = 0.7)+
  labs(x="Election", y = "Absolute candidate spend (£)")+
  theme(text=element_text(size=16, family="serif"))
spend_plot2


#### 4. Fitting the affective polarization model ------

L_IPOP_nlw <- lmer(affpol_nlw_ch_W ~ std_IP_spend_W + std_OP_spend_W + std_CI_W + genderW + higheducW + ageW + ethnicW + countryW + electionW + Constituency + affpol_nlw_LAG_W + (1|id), 
                   REML=T,
                   data = model_data)

class(L_IPOP_nlw) <- "lmerMod"
summary(L_IPOP_nlw)

tab_model(L_IPOP_nlw, digits = 3, show.se = TRUE)

# Standardized model 

model_data$std_affpol_nlw_ch_W <-scale(model_data$affpol_nlw_ch_W)

L_IPOP_std <- lmer(std_affpol_nlw_ch_W ~ std_IP_spend_W + std_OP_spend_W + std_CI_W + genderW + higheducW + ageW + ethnicW + countryW + electionW + Constituency + affpol_nlw_LAG_W + (1|id), 
                   REML=T,
                   data = model_data)

class(L_IPOP_std) <- "lmerMod"
summary(L_IPOP_std)

tab_model(L_IPOP_std, digits = 3, show.se = TRUE)

#### 5. In-party like models ####

# Fitting the longitudinal model  

L_IPL_IPOP <- lmer(IP_like_ch_W ~ std_IP_spend_W + std_OP_spend_W + std_CI_W + genderW + higheducW + ageW + ethnicW + countryW + electionW + IP_like_LAG_W + Constituency + (1|id), 
                   REML=T,
                   data = model_data) 
summary(L_IPL_IPOP)
class(L_IPL_IPOP) <- "lmerMod"

# Longitudinal model results 

tab_model(L_IPL_IPOP, digits = 3, show.se = TRUE)

# Standardized model 

model_data$std_IP_like_ch_W <-scale(model_data$IP_like_ch_W)

L_IPL_IPOP_std <- lmer(std_IP_like_ch_W ~ std_IP_spend_W + std_OP_spend_W + std_CI_W + genderW + higheducW + ageW + ethnicW + countryW + electionW + IP_like_LAG_W + Constituency + (1|id), 
                       REML=T,
                       data = model_data) 
summary(L_IPL_IPOP_std)
class(L_IPL_IPOP_std) <- "lmerMod"

tab_model(L_IPL_IPOP_std, digits = 3, show.se = TRUE)

#### 6. Out-party dislike - single-party ----

# Fitting the longitudinal model 

L_OPD_single_IPOP <- lmer(OP_dislike_S_ch_W ~ std_IP_spend_W + std_OP_spend_W + std_CI_W + genderW + higheducW + ageW + ethnicW + countryW + electionW + OP_dislike_S_LAG_W + Constituency + (1|id), 
                          REML=T,
                          data = model_data) 
summary(L_OPD_single_IPOP)
class(L_OPD_single_IPOP) <- "lmerMod"

# Longitudinal model results 

tab_model(L_OPD_single_IPOP, digits = 3, show.se = TRUE)

# standardized model 

model_data$std_OPD_single <-scale(model_data$OP_dislike_S_ch_W)

L_OPD_single_IPOP_std <- lmer(std_OPD_single ~ std_IP_spend_W + std_OP_spend_W + std_CI_W + genderW + higheducW + ageW + ethnicW + countryW + electionW + OP_dislike_S_LAG_W + Constituency + (1|id), 
                              REML=T,
                              data = model_data) 
summary(L_OPD_single_IPOP_std)
class(L_OPD_single_IPOP_std) <- "lmerMod"

tab_model(L_OPD_single_IPOP_std, digits = 3, show.se = TRUE)

#### 7. Out-party dislike - multi-party ----

# Fitting the longitudinal model

L_OPD_multi_IPOP <- lmer(OP_dislike_M_ch_W ~ std_IP_spend_W + std_OP_spend_W + std_CI_W + genderW + higheducW + ageW + ethnicW + countryW + electionW + OP_dislike_M_LAG_W + Constituency + (1|id), 
                         REML=T,
                         data = model_data) 
summary(L_OPD_multi_IPOP)
class(L_OPD_multi_IPOP) <- "lmerMod"

# Longitudinal model results

tab_model(L_OPD_multi_IPOP, digits = 3, show.se = TRUE)

# standardized model 

model_data$std_OPD_multi <- scale(model_data$OP_dislike_M_ch_W)


L_OPD_multi_IPOP_std <- lmer(std_OPD_multi ~ std_IP_spend_W + std_OP_spend_W + std_CI_W + genderW + higheducW + ageW + ethnicW + countryW + electionW + OP_dislike_M_LAG_W + Constituency + (1|id), 
                             REML=T,
                             data = model_data) 
summary(L_OPD_multi_IPOP_std)
class(L_OPD_multi_IPOP_std) <- "lmerMod"

# Longitudinal model results

tab_model(L_OPD_multi_IPOP_std, digits = 3, show.se = TRUE)

#### 8. Plots ####

# AP


AP_plot <- coefplot(L_IPOP_nlw,
                    coefficients = c("std_IP_spend_W", "std_OP_spend_W", "std_CI_W"),
                    newNames = c(std_IP_spend_W = "IP spend", std_OP_spend_W = "OP spend", std_CI_W = "Competitiveness"),
                    innerCI = 1.96,
                    outerCI = 0,
                    zeroType = 1, 
                    zeroColor = 'black', 
                    title = "",
                    xlim = -0.05,0.075,
                    ylab = 'Affective polarization',
                    xlab = "Coefficient estimate",
                    dodgeHeight = 0.3)+
  theme_light()+
  scale_color_manual(values = c('black'))+
  scale_shape_manual(values = c(15))+
  theme(text = element_text(family = "serif"))+
  xlim(-0.05, 0.08)

AP_plot <- AP_plot+theme(legend.position = "none")

AP_plot 

# IP like 

IPL_plot <- coefplot(L_IPL_IPOP,
                     coefficients = c("std_IP_spend_W", "std_OP_spend_W", "std_CI_W"),
                     newNames = c(std_IP_spend_W = "IP spend", std_OP_spend_W = "OP spend", std_CI_W = "Competitiveness"),
                     innerCI = 1.96,
                     outerCI = 0,
                     zeroType = 1, 
                     zeroColor = 'black', 
                     title = "",
                     xlim = -0.05,0.075,
                     ylab = 'In-party like',
                     xlab = "Coefficient estimate",
                     dodgeHeight = 0.3)+
  theme_light()+
  scale_color_manual(values = c('black'))+
  scale_shape_manual(values = c(15, 17, 18))+
  theme(text = element_text(family = "serif"))+
  xlim(-0.05, 0.08)

IPL_plot <- IPL_plot+theme(legend.position = "none")

IPL_plot 

# OP dislike (single)

OPDS_plot <- coefplot(L_OPD_single_IPOP,
                      coefficients = c("std_IP_spend_W", "std_OP_spend_W", "std_CI_W"),
                      newNames = c(std_IP_spend_W = "IP spend", std_OP_spend_W = "OP spend", std_CI_W = "Competitiveness"),
                      innerCI = 1.96,
                      outerCI = 0,
                      zeroType = 1, 
                      zeroColor = 'black', 
                      title = "",
                      xlim = -0.05,0.075,
                      ylab = 'Out-party dislike (S)',
                      xlab = "Coefficient estimate",
                      dodgeHeight = 0.3)+
  theme_light()+
  scale_color_manual(values = c('black'))+
  scale_shape_manual(values = c(15))+
  theme(text = element_text(family = "serif"))+
  xlim(-0.05, 0.08)

OPDS_plot <- OPDS_plot+theme(legend.position = "none")

OPDS_plot

# OP dislike (multi)

OPDM_plot <- coefplot(L_OPD_multi_IPOP,
                      coefficients = c("std_IP_spend_W", "std_OP_spend_W", "std_CI_W"),
                      newNames = c(std_IP_spend_W = "IP spend", std_OP_spend_W = "OP spend", std_CI_W = "Competitiveness"),
                      innerCI = 1.96,
                      outerCI = 0,
                      zeroType = 1, 
                      zeroColor = 'black', 
                      title = "",
                      xlim = -0.05,0.075,
                      ylab = 'Out-party dislike (M)',
                      xlab = "Coefficient estimate",
                      dodgeHeight = 0.3)+
  theme_light()+
  scale_color_manual(values = c('black'))+
  scale_shape_manual(values = c(15))+
  theme(text = element_text(family = "serif"))+
  xlim(-0.05, 0.08)

OPDM_plot <- OPDM_plot+theme(legend.position = "none")

OPDM_plot

## Combining the plots 


combined_plot <- ggarrange(AP_plot,
                           IPL_plot,
                           OPDS_plot,
                           OPDM_plot,
                           nrow = 4,
                           ncol = 1) 

combined_plot

## using ggplot 

## Affective polarization plot 

AP_data <- coefplot(L_IPOP_nlw,
                    coefficients = c("std_IP_spend_W", "std_OP_spend_W", "std_CI_W"),
                    newNames = c(std_IP_spend_W = "IP spend", 
                                 std_OP_spend_W = "OP spend", 
                                 std_CI_W = "Competitiveness"),
                    plot = FALSE)

AP_plot <- ggplot(AP_data, aes(x = Value, y = Coefficient, shape = Coefficient)) +
  geom_point(size = 3, color = "black") +
  geom_errorbarh(aes(xmin = LowOuter, xmax = HighOuter), height = 0.2, alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  scale_shape_manual(values = c(15, 17, 18)) +
  scale_x_continuous(breaks = seq(-0.05, 0.08, by = 0.025)) + 
  coord_cartesian(xlim = c(-0.05, 0.08)) +
  theme_light() +
  labs(x = "Coefficient estimate",
       y = "Affective polarization",
       title = "") +
  theme(text = element_text(family = "serif"),
        legend.position = "none",
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.text.y = element_text(size = 11))


print(AP_plot)

## In-party like plot 

IPL_data <- coefplot(L_IPL_IPOP,
                     coefficients = c("std_IP_spend_W", "std_OP_spend_W", "std_CI_W"),
                     newNames = c(std_IP_spend_W = "IP spend", 
                                  std_OP_spend_W = "OP spend", 
                                  std_CI_W = "Competitiveness"),
                     plot = FALSE)

IPL_plot <- ggplot(IPL_data, aes(x = Value, y = Coefficient, shape = Coefficient)) +
  geom_point(size = 3, color = "black") +
  geom_errorbarh(aes(xmin = LowOuter, xmax = HighOuter), height = 0.2, alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  scale_shape_manual(values = c(15, 17, 18)) +  
  scale_x_continuous(breaks = seq(-0.05, 0.08, by = 0.025)) + 
  coord_cartesian(xlim = c(-0.05, 0.08)) +
  theme_light() +
  labs(x = "Coefficient estimate",
       y = "In-party like",
       title = "") +
  theme(text = element_text(family = "serif"),
        legend.position = "none",
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.text.y = element_text(size = 11)
  )

print(IPL_plot)

## Out-party dislike single plot 

OPDS_data <- coefplot(L_OPD_single_IPOP,
                      coefficients = c("std_IP_spend_W", "std_OP_spend_W", "std_CI_W"),
                      newNames = c(std_IP_spend_W = "IP spend", 
                                   std_OP_spend_W = "OP spend", 
                                   std_CI_W = "Competitiveness"),
                      plot = FALSE)

OPDS_plot <- ggplot(OPDS_data, aes(x = Value, y = Coefficient, shape = Coefficient)) +
  geom_point(size = 3, color = "black") +
  geom_errorbarh(aes(xmin = LowOuter, xmax = HighOuter), height = 0.2, alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  scale_shape_manual(values = c(15, 17, 18)) +  
  scale_x_continuous(breaks = seq(-0.05, 0.08, by = 0.025)) + 
  coord_cartesian(xlim = c(-0.05, 0.08)) +
  theme_light() +
  labs(x = "Coefficient estimate",
       y = "Out-party dislike (S)",
       title = "") +
  theme(text = element_text(family = "serif"),
        legend.position = "none",
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.text.y = element_text(size = 11)
  )

print(OPDS_plot)

## out-party dislike multi plot

OPDM_data <- coefplot(L_OPD_multi_IPOP,
                      coefficients = c("std_IP_spend_W", "std_OP_spend_W", "std_CI_W"),
                      newNames = c(std_IP_spend_W = "IP spend", 
                                   std_OP_spend_W = "OP spend", 
                                   std_CI_W = "Competitiveness"),
                      plot = FALSE)

OPDM_plot <- ggplot(OPDM_data, aes(x = Value, y = Coefficient, shape = Coefficient)) +
  geom_point(size = 3, color = "black") +
  geom_errorbarh(aes(xmin = LowOuter, xmax = HighOuter), height = 0.2, alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  scale_shape_manual(values = c(15, 17, 18)) +  
  scale_x_continuous(breaks = seq(-0.05, 0.08, by = 0.025)) + 
  coord_cartesian(xlim = c(-0.05, 0.08)) +
  theme_light() +
  labs(x = "Coefficient estimate",
       y = "Out-party dislike (M)",
       title = "") +
  theme(text = element_text(family = "serif"),
        legend.position = "none",
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.text.y = element_text(size = 11)
  )

print(OPDM_plot)


combined_plot <- AP_plot / IPL_plot / OPDS_plot / OPDM_plot 
print(combined_plot)


#### 9. Interaction of spending and competitiveness ####

## Affective polarization

L_IPOP_int <- lmer(affpol_nlw_ch_W ~ std_IP_spend_W + std_OP_spend_W + std_CI_W + std_IP_spend_W:std_CI_W + std_OP_spend_W:std_CI_W + genderW + higheducW + ageW + ethnicW + countryW + electionW + Constituency + affpol_nlw_LAG_W + (1|id), 
                   REML=T,
                   data = model_data)

class(L_IPOP_int) <- "lmerMod"
summary(L_IPOP_int)

# Results 

tab_model(L_IPOP_int, digits = 3, show.se = TRUE)

# marginal effects 

s = slopes(L_IPOP_int, variables = "std_IP_spend_W", 
           newdata = datagrid(std_CI_W = c(-1, 0, 1)))
s

## Table of model results 

stargazer(L_IPOP_int, 
          title = "The regression of affective polarization on campaign effort, moderated by
          competitiveness",
          keep = c("std_IP_spend_W", "std_OP_spend_W", "std_CI_W"," std_IP_spend_W:std_CI_W", " std_OP_spend_W:std_CI_W",  "genderW", "higheducW", "ageW", "ethnicW", "countryW", "affpol_LAG_W", "electionW"),
          covariate.labels=c("In-party spend", "Out-party spend", "Competitiveness","In-party spend x Competitiveness", "Out-party spend x Competitiveness",  "Gender", "Degree-edcuated", "Age", "Ethnicity (White)", "Scotland", "Wales", "Lagged DV", "2017", "2019"),
          keep.stat="n",
          star.cutoffs = c(0.05, 0.01, 0.001))

#### 10. PID interactions ####

# standardizing the PID spending variables 

summary(model_data$IP_spend_PID_W)

model_data$std_IP_spend_PID_W <-scale(model_data$IP_spend_PID_W)

summary(model_data$OP_spend_PID_W)

model_data$std_OP_spend_PID_W <-scale(model_data$OP_spend_PID_W)


#### a. --- Affective polarization ####

table(model_data$pid_pre_wsqueeze_W)

9789/(9789+55344)

# Fitting the model 

L_IPOP_pidint <- lmer(affpol_nlw_ch_W ~ std_IP_spend_PID_W + std_OP_spend_PID_W + std_CI_W + pid_pre_wsqueeze_W + std_IP_spend_PID_W:pid_pre_wsqueeze_W + std_OP_spend_PID_W:pid_pre_wsqueeze_W + std_CI_W:pid_pre_wsqueeze_W + genderW + higheducW + ageW + ethnicW + countryW + electionW + Constituency + affpol_nlw_LAG_W + (1|id), 
                      REML=T,
                      data = model_data)

class(L_IPOP_pidint) <- "lmerMod"
summary(L_IPOP_pidint)

tab_model(L_IPOP_pidint, digits = 3, show.se = TRUE)

## Calculating AMEs

# IP Spend x PID interaction term = 0.046, SE = 0.016, p < 0.01
# Non-partisan AME = -0.02, SE = 0.016, p = 0.203
# Partisan AME = 0.026, SE = 0.006, p < 0.001

ip_spend = slopes(L_IPOP_pidint, variables = "std_IP_spend_PID_W", 
                  newdata = datagrid(pid_pre_wsqueeze_W = c(0, 1)))
ip_spend

# OP Spend x PID interaction term = -0.006, SE = 0.018, p = 0.741
# Non-partisan AME = -0.011, SE = 0.016, p = 0.495
# Partisan AME = -0.016, SE = 0.008, p < 0.1

op_spend = slopes(L_IPOP_pidint, variables = "std_OP_spend_PID_W", 
                  newdata = datagrid(pid_pre_wsqueeze_W = c(0, 1)))
op_spend


# CI x PID interaction term = 0.009, SE = 0.015, p = 0.542
# Non-partisan AME = 0.025, SE = 0.015, p = 0.11
# Partisan AME = 0.016, SE = 0.008, p < 0.05

ci = slopes(L_IPOP_pidint, variables = "std_CI_W", 
            newdata = datagrid(pid_pre_wsqueeze_W = c(0, 1)))

ci

# Retain only the necessary variables from each data frame: term, estimate, 
# conf.low, conf.high, pid_pre_wsqueeze_W

ip_spend_affpol <- ip_spend %>% 
  select(term, estimate, conf.low, conf.high, pid_pre_wsqueeze_W)

op_spend_affpol <- op_spend %>% 
  select(term, estimate, conf.low, conf.high, pid_pre_wsqueeze_W)

ci_affpol <- ci %>% 
  select(term, estimate, conf.low, conf.high, pid_pre_wsqueeze_W)

affpol_AMEs <- bind_rows(ip_spend_affpol,
                             op_spend_affpol,
                             ci_affpol)


# Create new data frame for plotting 

affpol_plot_df <- affpol_AMEs %>%
  filter(pid_pre_wsqueeze_W %in% c(0, 1)) %>%
  mutate(
    pid = factor(pid_pre_wsqueeze_W, levels = c(0,1), labels = c("0","1")))

## Plot code 

posd <- position_dodge(width = 0.6)

ggplot(affpol_plot_df, aes(x = term, y = estimate, color = pid)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  
  # error bars with caps
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.1,
                position = posd,
                size = 0.6) +
  
  # points with custom shapes (shapes determined by term)
  geom_point(aes(shape = term),
             size = 3,
             position = posd) +
  
  # shapes (std_CI_W set to diamond = 23; change to 18 if you prefer)
  scale_shape_manual(
    name = "",
    values = c(
      "std_CI_W" = 18,        # diamond (23 = filled diamond; 5 = hollow diamond; 18 also a diamond-like)
      "std_IP_spend_PID_W" = 15,  # square
      "std_OP_spend_PID_W" = 17   # triangle
    )
  ) +
  guides(shape = "none") + 
  
  
  # custom colour scheme: 0 = light grey, 1 = black
  scale_color_manual(
    name = "PID",
    values = c("0" = "grey65", "1" = "black"),
    labels = c("0" = "Non-partisans", "1" = "Partisans")
  ) +
  
  labs(
    x = "",
    y = "Estimate (AME)",
    title = " "
  ) +
  
  # theme: bigger fonts everywhere + bold axis titles
  theme_bw() +
  theme(
    text = element_text(family = "serif", size = 14),      # base font size
    axis.title = element_text(face = "bold", size = 14),   # bold axis labels
    axis.text = element_text(size = 13),                  # tick labels
    panel.grid.minor = element_blank(),
    legend.title = element_text(hjust = 0.5, face = "bold", size = 14),
    legend.text = element_text(size = 13)
  ) +
  
  scale_x_discrete(labels = c(
    "std_IP_spend_PID_W" = "In-party Spend",
    "std_OP_spend_PID_W" = "Out-party Spend",
    "std_CI_W" = "Competitiveness"
  ))

#### b. --- In-party like ####

L_IPL_pidint <- lmer(IP_like_PID_ch_W ~ std_IP_spend_PID_W + std_OP_spend_PID_W + std_CI_W + pid_pre_wsqueeze_W + std_IP_spend_PID_W:pid_pre_wsqueeze_W + std_OP_spend_PID_W:pid_pre_wsqueeze_W + std_CI_W:pid_pre_wsqueeze_W + genderW + higheducW + ageW + ethnicW + countryW + electionW + IP_like_PID_LAG_W + Constituency + (1|id), 
                     REML=T,
                     data = model_data) 
summary(L_IPL_pidint)
class(L_IPL_pidint) <- "lmerMod"

tab_model(L_IPL_pidint, digits = 3, show.se = TRUE)

## Calculating AMEs

# IP Spend x PID interaction term = 0.162, SE = 0.032, p < 0.001
# Non-partisan AME = -0.073, SE = 0.031, p < 0.05
# Partisan AME = 0.088, SE = 0.013, p < 0.001

ip_spend = slopes(L_IPL_pidint, variables = "std_IP_spend_PID_W", 
                  newdata = datagrid(pid_pre_wsqueeze_W = c(0, 1)))
ip_spend 

# OP Spend x PID interaction term = -0.021, SE = 0.029, p = 0.473
# Non-partisan AME = -0.027, SE = 0.03, p = 0.381
# Partisan AME = -0.047, SE = 0.016, p < 0.01

op_spend = slopes(L_IPL_pidint, variables = "std_OP_spend_PID_W", 
                  newdata = datagrid(pid_pre_wsqueeze_W = c(0, 1)))
op_spend

# CI x PID interaction term = -0.019, SE = 0.029, p = 0.503
# Non-partisan AME = 0.02, SE = 0.029, p = 0.488
# Partisan AME = 0.001, SE = 0.015, p = 0.938

ci = slopes(L_IPL_pidint, variables = "std_CI_W", 
            newdata = datagrid(pid_pre_wsqueeze_W = c(0, 1)))
ci

#### c. --- Out-party dislike - single ####

L_OPD_single_pidint <- lmer(OP_dislike_S_ch_W ~ std_IP_spend_PID_W + std_OP_spend_PID_W + std_CI_W + pid_pre_wsqueeze_W + std_IP_spend_PID_W:pid_pre_wsqueeze_W + std_OP_spend_PID_W:pid_pre_wsqueeze_W + std_CI_W:pid_pre_wsqueeze_W + genderW + higheducW + ageW + ethnicW + countryW + electionW + OP_dislike_S_LAG_W + Constituency + (1|id), 
                            REML=T,
                            data = model_data) 
summary(L_OPD_single_pidint)
class(L_OPD_single_pidint) <- "lmerMod"

# Model results 

tab_model(L_OPD_single_pidint, digits = 3, show.se = TRUE)

## Calculating AMEs

# IP Spend x PID interaction term = -0.039, SE = 0.033, p = 0.235
# Non-partisan AME = 0.003, SE = 0.031, p = 0.913
# Partisan AME = -0.035, SE = 0.012, p < 0.01

ip_spend = slopes(L_OPD_single_pidint, variables = "std_IP_spend_PID_W", 
                  newdata = datagrid(pid_pre_wsqueeze_W = c(0, 1)))
ip_spend 

# OP Spend x PID interaction term = 0.014, SE = 0.03, p = 0.644
# Non-partisan AME = -0.03, SE = 0.031, p = 0.342
# Partisan AME = -0.016, SE = 0.016, p = 0.32

op_spend = slopes(L_OPD_single_pidint, variables = "std_OP_spend_PID_W", 
                  newdata = datagrid(pid_pre_wsqueeze_W = c(0, 1)))
op_spend

# CI x PID interaction term = -0.021, SE = 0.03, p = 0.487
# Non-partisan AME = 0.065, SE = 0.031, p < 0.05
# Partisan AME = 0.044, SE = 0.015, p < 0.01

ci = slopes(L_OPD_single_pidint, variables = "std_CI_W", 
            newdata = datagrid(pid_pre_wsqueeze_W = c(0, 1)))
ci

#### d. --- Out-party dislike - multi ####

L_OPD_multi_pidint <- lmer(OP_disike_M_PID_ch_W ~ std_IP_spend_PID_W + std_OP_spend_PID_W + std_CI_W + pid_pre_wsqueeze_W + std_IP_spend_PID_W:pid_pre_wsqueeze_W + std_OP_spend_PID_W:pid_pre_wsqueeze_W + std_CI_W:pid_pre_wsqueeze_W + genderW + higheducW + ageW + ethnicW + countryW + electionW + OP_dislike_M_PID_LAG_W + Constituency + (1|id), 
                           REML=T,
                           data = model_data) 
summary(L_OPD_multi_pidint)
class(L_OPD_multi_pidint) <- "lmerMod"

tab_model(L_OPD_multi_pidint, digits = 3, show.se = TRUE)

## Calculating AMEs

# IP Spend x PID interaction term = 0.044, SE = 0.019, p < 0.05
# Non-partisan AME = 0.009, SE = 0.018, p = 0.615
# Partisan AME = 0.053, SE = 0.008, p < 0.001

ip_spend = slopes(L_OPD_multi_pidint, variables = "std_IP_spend_PID_W", 
                  newdata = datagrid(pid_pre_wsqueeze_W = c(0, 1)))
ip_spend 

# OP Spend x PID interaction term = -0.036, SE = 0.018, p < 0.05
# Non-partisan AME = 0.017, SE = 0.018, p = 0.359
# Partisan AME = -0.02, SE = 0.01, p < 0.05

op_spend = slopes(L_OPD_multi_pidint, variables = "std_OP_spend_PID_W", 
                  newdata = datagrid(pid_pre_wsqueeze_W = c(0, 1)))
op_spend

# CI x PID interaction term = -0.002, SE = 0.017, p = 0.914
# Non-partisan AME = -0.002, SE = 0.018, p = 0.924
# Partisan AME = -0.004, SE = 0.009, p = 0.694

ci = slopes(L_OPD_multi_pidint, variables = "std_CI_W", 
            newdata = datagrid(pid_pre_wsqueeze_W = c(0, 1)))
ci

# OP dislike model without tactical voters 

nontv_data <- subset(model_data, tactical_voteW == 0)

table(nontv_data$electionW)

OPD_nontv_pidint <- lmer(OP_disike_M_PID_ch_W ~ std_IP_spend_PID_W + std_OP_spend_PID_W + std_CI_W + pid_pre_wsqueeze_W + std_IP_spend_PID_W:pid_pre_wsqueeze_W + std_OP_spend_PID_W:pid_pre_wsqueeze_W + std_CI_W:pid_pre_wsqueeze_W + genderW + higheducW + ageW + ethnicW + countryW + electionW + OP_dislike_M_PID_LAG_W + Constituency + (1|id), 
                           REML=T,
                           data = nontv_data) 
summary(OPD_nontv_pidint)
class(OPD_nontv_pidint) <- "lmerMod"

tab_model(OPD_nontv_pidint, digits = 3, show.se = TRUE)

#### e. --- Stargazer table  ####

stargazer(L_IPOP_pidint, L_IPL_pidint, L_OPD_single_pidint, L_OPD_multi_pidint,
          title = "The regression of affective polarization on campaign effort and
          electoral competitiveness, moderated by partisanship",
          keep = c("std_IP_spend_PID_W", "std_OP_spend_PID_W", "std_CI_W","pid_pre_wsqueeze_W", 
                   "genderW", "higheducW", "ageW", "ethnicW", "countryW", "electionW", "affpol_nlw_LAG_W", 
                   "std_IP_spend_PID_W:pid_pre_wsqueeze_W", "std_OP_spend_PID_W:pid_pre_wsqueeze_W", 
                   "std_CI_W:pid_pre_wsqueeze_W"),
          covariate.labels=c("In-party spend - Non-partisans", "Out-party spend - Non-partisans", 
                             "Competitiveness - Non-partisans", "Partisanship",
                             "Gender", "Degree-edcuated", "Age", "Ethnicity (White)", 
                             "Scotland", "Wales", "2017", "2019", "Lagged DV",
                             "In-party spend - Partisans", 
                             "Out-party spend - Partisans", "Competitiveness - Partisans"),
          keep.stat="n",
          single.row = TRUE,
          star.cutoffs = c(0.05, 0.01, 0.001))

# IP spend non-partisan coefficient in IP like model
# CI non-partisan coefficient and education cofficeint in OP single mode 
# PID coefficient in OP multi model. IP spend interaction term. 

# OP single lagged dv - b = -0.279 SE = 0.004 p < 0.001
# OP Multi Lagged dv - b = -0.415, SE = 0.004, p < 0.001


#### 11. Robustness checks ####

#### a. --- Separate models for CI and spending ----

#### Spending only 

## Affective polarization

Spend_Only_AP <- lmer(affpol_nlw_ch_W ~ std_IP_spend_W + std_OP_spend_W + genderW + higheducW + ageW + ethnicW + countryW + electionW + Constituency + affpol_nlw_LAG_W + (1|id), 
                      REML=T,
                      data = model_data)

class(Spend_Only_AP) <- "lmerMod"
summary(Spend_Only_AP)

tab_model(Spend_Only_AP, digits = 3, show.se = TRUE)

## IP like 

Spend_Only_IPL <- lmer(IP_like_ch_W ~ std_IP_spend_W + std_OP_spend_W + genderW + higheducW + ageW + ethnicW + countryW + electionW + IP_like_LAG_W + Constituency + (1|id), 
                       REML=T,
                       data = model_data) 
summary(Spend_Only_IPL)
class(Spend_Only_IPL) <- "lmerMod"

tab_model(Spend_Only_IPL, digits = 3, show.se = TRUE)



## OP dislike single 

Spend_Only_OPS <- lmer(OP_dislike_S_ch_W ~ std_IP_spend_W + std_OP_spend_W + genderW + higheducW + ageW + ethnicW + countryW + electionW + OP_dislike_S_LAG_W + Constituency + (1|id), 
                       REML=T,
                       data = model_data) 
class(Spend_Only_OPS) <- "lmerMod"

tab_model(Spend_Only_OPS, digits = 3, show.se = TRUE)



## OP dislike multi

Spend_Only_OPM <- lmer(OP_dislike_M_ch_W ~ std_IP_spend_W + std_OP_spend_W + genderW + higheducW + ageW + ethnicW + countryW + electionW + OP_dislike_M_LAG_W + Constituency + (1|id), 
                       REML=T,
                       data = model_data) 
class(Spend_Only_OPM) <- "lmerMod"

tab_model(Spend_Only_OPM, digits = 3, show.se = TRUE)



#### Competitiveness only 

## Affective polarization

CI_Only_AP <- lmer(affpol_nlw_ch_W ~ std_CI_W + genderW + higheducW + ageW + ethnicW + countryW + electionW + Constituency + affpol_nlw_LAG_W + (1|id), 
                   REML=T,
                   data = model_data)

class(CI_Only_AP) <- "lmerMod"
summary(CI_Only_AP)

tab_model(CI_Only_AP, digits = 3, show.se = TRUE)



## IP like 

CI_Only_IPL <- lmer(IP_like_ch_W ~ std_CI_W + genderW + higheducW + ageW + ethnicW + countryW + electionW + IP_like_LAG_W + Constituency + (1|id), 
                    REML=T,
                    data = model_data) 
summary(CI_Only_IPL)
class(CI_Only_IPL) <- "lmerMod"

tab_model(CI_Only_IPL, digits = 3, show.se = TRUE)



## OP dislike single 

CI_Only_OPS <- lmer(OP_dislike_S_ch_W ~ std_CI_W + genderW + higheducW + ageW + ethnicW + countryW + electionW + OP_dislike_S_LAG_W + Constituency + (1|id), 
                    REML=T,
                    data = model_data) 
class(CI_Only_OPS) <- "lmerMod"

tab_model(CI_Only_OPS, digits = 3, show.se = TRUE)



## OP dislike multi

CI_Only_OPM <- lmer(OP_dislike_M_ch_W ~ std_CI_W + genderW + higheducW + ageW + ethnicW + countryW + electionW + OP_dislike_M_LAG_W + Constituency + (1|id), 
                    REML=T,
                    data = model_data) 
class(CI_Only_OPM) <- "lmerMod"

tab_model(CI_Only_OPM, digits = 3, show.se = TRUE)


#### b. ---- Non-extreme party subsample ----

# create a dataset of person-observations in which no far-right party was an 
# effective party in their constituency

nonfarright_data <- subset(model_data, farright_ep_W == 0)

table(nonfarright_data$electionW)

# Fitting the non far-right AP model 

L_IPOP_NFR <- lmer(affpol_nlw_ch_W ~ std_IP_spend_W + std_OP_spend_W + std_CI_W + genderW + higheducW + ageW + ethnicW + countryW + electionW + Constituency + affpol_nlw_LAG_W + (1|id), 
                   REML=T,
                   data = nonfarright_data)

class(L_IPOP_NFR) <- "lmerMod"
summary(L_IPOP_NFR)

# Results 

tab_model(L_IPOP_NFR, digits = 3, show.se = TRUE)

# Fitting the non far-right IP like model 

L_IPL_IPOP_NFR <- lmer(IP_like_ch_W ~ std_IP_spend_W + std_OP_spend_W + std_CI_W + genderW + higheducW + ageW + ethnicW + countryW + electionW + IP_like_LAG_W + Constituency + (1|id), 
                       REML=T,
                       data = nonfarright_data) 
summary(L_IPL_IPOP_NFR)
class(L_IPL_IPOP_NFR) <- "lmerMod"

# Results 

tab_model(L_IPL_IPOP_NFR, digits = 3, show.se = TRUE)

# Fitting the non far-right OP dislike single model 

L_OPD_single_IPOP_NFR <- lmer(OP_dislike_S_ch_W ~ std_IP_spend_W + std_OP_spend_W + std_CI_W + genderW + higheducW + ageW + ethnicW + countryW + electionW + OP_dislike_S_LAG_W + Constituency + (1|id), 
                              REML=T,
                              data = nonfarright_data) 
summary(L_OPD_single_IPOP_NFR)
class(L_OPD_single_IPOP_NFR) <- "lmerMod"

# Results 

tab_model(L_OPD_single_IPOP_NFR, digits = 3, show.se = TRUE)

# Fitting the non far-right OP dislike multi model 

L_OPD_multi_IPOP_NFR <- lmer(OP_dislike_M_ch_W ~ std_IP_spend_W + std_OP_spend_W + std_CI_W + genderW + higheducW + ageW + ethnicW + countryW + electionW + OP_dislike_M_LAG_W + Constituency + (1|id), 
                             REML=T,
                             data = nonfarright_data) 
summary(L_OPD_multi_IPOP_NFR)
class(L_OPD_multi_IPOP_NFR) <- "lmerMod"

# Results

tab_model(L_OPD_multi_IPOP_NFR, digits = 3, show.se = TRUE)

#### c. --- Two-level error terms ----

## Affective polarization

L_IPOP_twolvl <- lmer(affpol_nlw_ch_W ~ std_IP_spend_W + std_OP_spend_W + std_CI_W + genderW + higheducW + ageW + ethnicW + countryW + electionW + Constituency + affpol_nlw_LAG_W + (1|Constituency/id), 
                      REML=T,
                      data = model_data)

class(L_IPOP_twolvl) <- "lmerMod"
summary(L_IPOP_twolvl)

tab_model(L_IPOP_twolvl, digits = 3, show.se = TRUE)

## IP like

L_IPL_IPOP_twolvl <- lmer(IP_like_ch_W ~ std_IP_spend_W + std_OP_spend_W + std_CI_W + genderW + higheducW + ageW + ethnicW + countryW + electionW + IP_like_LAG_W + Constituency + (1|Constituency/id), 
                          REML=T,
                          data = model_data) 
summary(L_IPL_IPOP_twolvl)
class(L_IPL_IPOP_twolvl) <- "lmerMod"

tab_model(L_IPL_IPOP_twolvl, digits = 3, show.se = TRUE)

## OP dislike single 

L_OPD_single_IPOP_twolvl <- lmer(OP_dislike_S_ch_W ~ std_IP_spend_W + std_OP_spend_W + std_CI_W + genderW + higheducW + ageW + ethnicW + countryW + electionW + OP_dislike_S_LAG_W + Constituency + (1|Constituency/id), 
                                 REML=T,
                                 data = model_data) 
summary(L_OPD_single_IPOP_twolvl)
class(L_OPD_single_IPOP_twolvl) <- "lmerMod"

tab_model(L_OPD_single_IPOP_twolvl, digits = 3, show.se = TRUE)

## OP dislike multi 

L_OPD_multi_IPOP_twolvl <- lmer(OP_dislike_M_ch_W ~ std_IP_spend_W + std_OP_spend_W + std_CI_W + genderW + higheducW + ageW + ethnicW + countryW + electionW + OP_dislike_M_LAG_W + Constituency + (1|id), 
                                REML=T,
                                data = model_data) 
summary(L_OPD_multi_IPOP_twolvl)
class(L_OPD_multi_IPOP_twolvl) <- "lmerMod"

tab_model(L_OPD_multi_IPOP_twolvl, digits = 3, show.se = TRUE)

#### d. --- More proximate pre-election wave ----

## Affective polarization

L_IPOP_MP <- lmer(affpol_nlw_ich_W ~ std_IP_spend_W + std_OP_spend_W + std_CI_W + genderW + higheducW + ageW + ethnicW + countryW + electionW + Constituency + affpol_nlw_MPLAG_W + (1|id), 
                  REML=T,
                  data = model_data)

class(L_IPOP_MP) <- "lmerMod"
summary(L_IPOP_MP)

tab_model(L_IPOP_MP, digits = 3, show.se = TRUE)

## In-party like 

L_IPL_IPOP_MP <- lmer(IP_like_ich_W ~ std_IP_spend_W + std_OP_spend_W + std_CI_W + genderW + higheducW + ageW + ethnicW + countryW + electionW + IP_like_MPLAG_W + Constituency + (1|id), 
                      REML=T,
                      data = model_data) 
summary(L_IPL_IPOP_MP)
class(L_IPL_IPOP_MP) <- "lmerMod"

tab_model(L_IPL_IPOP_MP, digits = 3, show.se = TRUE)

## Out-party dislike (single)

L_OPD_single_IPOP_MP <- lmer(OP_dislike_S_ich_W ~ std_IP_spend_W + std_OP_spend_W + std_CI_W + genderW + higheducW + ageW + ethnicW + countryW + electionW + OP_dislike_S_MPLAG_W + Constituency + (1|id), 
                             REML=T,
                             data = model_data) 
summary(L_OPD_single_IPOP_MP)
class(L_OPD_single_IPOP_MP) <- "lmerMod"

tab_model(L_OPD_single_IPOP_MP, digits = 3, show.se = TRUE)

## Out-party dislike (multi)

L_OPD_multi_IPOP_MP <- lmer(OP_dislike_M_ich_W ~ std_IP_spend_W + std_OP_spend_W + std_CI_W + genderW + higheducW + ageW + ethnicW + countryW + electionW + OP_dislike_M_LAG_W + Constituency + (1|id), 
                            REML=T,
                            data = model_data) 
summary(L_OPD_multi_IPOP_MP)
class(L_OPD_multi_IPOP_MP) <- "lmerMod"

tab_model(L_OPD_multi_IPOP_MP, digits = 3, show.se = TRUE)


#### e. --- raw margin models -----

# standardize the raw margin measure

model_data$std_raw_margin <-scale(model_data$raw_margin_W)

## Affective Polarization

L_IPOP_rawv <- lmer(affpol_nlw_ch_W ~ std_IP_spend_W + std_OP_spend_W + std_raw_margin + genderW + higheducW + ageW + ethnicW + countryW + electionW + Constituency + affpol_nlw_LAG_W + (1|id), 
                    REML=T,
                    data = model_data)

class(L_IPOP_rawv) <- "lmerMod"
summary(L_IPOP_rawv)

tab_model(L_IPOP_rawv, digits = 3, show.se = TRUE)

## In-party like 

L_IPL_IPOP_rawv <- lmer(IP_like_ch_W ~ std_IP_spend_W + std_OP_spend_W + std_raw_margin + genderW + higheducW + ageW + ethnicW + countryW + electionW + IP_like_LAG_W + Constituency + (1|id), 
                        REML=T,
                        data = model_data) 
summary(L_IPL_IPOP_rawv)
class(L_IPL_IPOP_rawv) <- "lmerMod"

tab_model(L_IPL_IPOP_rawv, digits = 3, show.se = TRUE)

## Out-party dislike - single

L_OPD_single_IPOP_rawv <- lmer(OP_dislike_S_ch_W ~ std_IP_spend_W + std_OP_spend_W + std_raw_margin + genderW + higheducW + ageW + ethnicW + countryW + electionW + OP_dislike_S_LAG_W + Constituency + (1|id), 
                               REML=T,
                               data = model_data) 
summary(L_OPD_single_IPOP_rawv)
class(L_OPD_single_IPOP_rawv) <- "lmerMod"

tab_model(L_OPD_single_IPOP_rawv, digits = 3, show.se = TRUE)

## Out-party dislike - multi

L_OPD_multi_IPOP_rawv <- lmer(OP_dislike_M_ch_W ~ std_IP_spend_W + std_OP_spend_W + std_raw_margin + genderW + higheducW + ageW + ethnicW + countryW + electionW + OP_dislike_M_LAG_W + Constituency + (1|id), 
                              REML=T,
                              data = model_data) 
summary(L_OPD_multi_IPOP_rawv)
class(L_OPD_multi_IPOP_rawv) <- "lmerMod"

tab_model(L_OPD_multi_IPOP_rawv, digits = 3, show.se = TRUE)

## Effective party interaction

L_IPOP_efp <- lmer(affpol_nlw_ch_W ~ std_IP_spend_W + std_OP_spend_W + std_CI_W 
                   + std_CI_W:eff_parties_W + eff_parties_W + genderW + higheducW 
                   + ageW + ethnicW + countryW + electionW + Constituency + affpol_nlw_LAG_W + (1|id), 
                   REML=T,
                   data = model_data)

class(L_IPOP_efp) <- "lmerMod"
summary(L_IPOP_efp)

tab_model(L_IPOP_efp, digits = 3, show.se = TRUE)

#### f. --- Alternative to spending - contact measures -----

## Affective Polarization

L_IPOP_contact <- lmer(affpol_nlw_ch_W ~ IP_contact_W + OP_contact_W + std_CI_W + genderW + higheducW + ageW + ethnicW + countryW + electionW + Constituency + affpol_nlw_LAG_W + (1|id), 
                       REML=T,
                       data = model_data)

class(L_IPOP_contact) <- "lmerMod"
summary(L_IPOP_contact)

## Table of model results 

stargazer(L_IPOP_contact, 
          title = "The regression of affective polarization on party contact and 
          electoral competitiveness",
          keep = c("IP_contact_W", "OP_contact_W", "std_CI_W", "genderW", "higheducW", "ageW", "ethnicW", "countryW", "affpol_nlw_LAG_W", "electionW"),
          covariate.labels=c("In-party contact", "Out-party contact", "Competitiveness",  "Gender", "Degree-edcuated", "Age", "Ethnicity (White)", "Scotland", "Wales", "Lagged DV", "2017", "2019"),
          keep.stat="n",
          star.cutoffs = c(0.05, 0.01, 0.001))

#### g. --- Alternative AP models -----

## Reiljan API index  

L_Reiljan <- lmer(AP_R_ch_W ~ std_IP_spend_W + std_OP_spend_W + std_CI_W + genderW + higheducW + ageW + ethnicW + countryW + AP_R_LAG_W + electionW + Constituency + (1|id), 
                  REML=T,
                  data = model_data)
summary(L_Reiljan)
class(L_Reiljan) <- "lmerMod"

tab_model(L_Reiljan, digits = 3, show.se = TRUE)



