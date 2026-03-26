##### INFORMATION PROVISION EXPERIMENT
rm(list = ls())

library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(openxlsx)
library(reshape2)
library(grid)
library(gridExtra)
library(mFilter)
library(lubridate)
library(tidyverse)
library(corrplot)
library(plm)
library(fixest)
library(lmtest)
library(sandwich)
library(aTSA)
library(haven)
library(paletteer)
library(labelled)
library(kableExtra)
library(expss)
library(sjlabelled)
library(sjPlot)
library(RColorBrewer)
library(stargazer)

'%ni%' <- Negate("%in%")
################################################################################
# set path to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
################################################################################
df <- read_csv("replication_data.csv") %>% select(-1)
#########################################################

# Some preliminary data cleaning and variable creation
# Factors from character strings
df <- df %>% mutate(c.concern = as.factor(c.concern),
                    age = as.factor(age),
                    econ.hh = as.factor(econ.hh), 
                    education = as.factor(education),
                    pol.lean.soc = as.numeric(pol.lean.soc), 
                    pol.lean.econ = as.numeric(pol.lean.econ))

#Education level factor
df$education <- factor(df$education, levels = c("High school or less", 
                                                "Some college or other non-university certificate or diploma",
                                                "Bachelor's degree",
                                                "Master's degree",
                                                "Doctorate or PhD degree" ))

# Climate change concern factor
df$c.concern <- factor(df$c.concern, levels = c("Not at all worried", "Not very worried", 
                                                "Somewhat worried", "Very worried", "Extremely worried"))

# Economic struggle factor variable
df$econ.hh <- factor(df$econ.hh, levels = c("Living comfortably on present income", "Coping on present income", 
                                            "Finding it difficult on present income", "Finding it very difficult on present income"))
df$econ.hh <- as.numeric(df$econ.hh)
#--------------------------------------------------------------------------------------------------------------------
# Binary belief in anthropogenic climate change
df$anthro_cc <- ifelse(df$c.aware == "Climate change is happening now, caused mainly by human activities.", 1, 0)

# High climate change concern
df$high.concern <- ifelse(df$c.concern %in% c("Very worried", "Extremely worried"), 1, 0)

# University degree
df$uni <- NA
df$uni[df$education %in% c("Bachelor's degree", "Master's degree", "Doctorate or PhD degree")] <- 1
df$uni[df$education %in% c("High school or less","Some college or other non-university certificate or diploma")] <- 0

# Binary variables (high job risk and high job risk beliefs)
df$high.risk <- ifelse(df$index.score >= 10, 1,0)
df$high.prior <- ifelse(df$prior.belief >= 10, 1,0)

# Binary variables: Overestimators (strong and general)
df$lover <- ifelse(df$gap.cat == "A_strong over", 1, 0)
df$over <- ifelse(df$gap > 1, 1, 0)

#---------------------------------------------------------------------------------------------------------------
# Job risk perception gap categories check dataframe
df_gapcheck <- df %>% filter(Screener !="No") %>% dplyr::select(gap.cat, gap, prior.belief, index.score)

# WORKERS ONLY DATA FRAME
df_emp <- filter(df, flow!="Unemployed")

##################################################################################################################
# RESULT SECTION 4.1
#-----------------------------------------------------------------------------------------------------------------
##### RISK BELIEF PRIOR REGRESSIONS (TABLE C2 APPENDIX, Basis for FIGURE 3B)
risk0 <- lm(prior.belief ~ sector.score + occupation.score + unemp.score + resident +  pol.lean.three + high.concern  + female + age + uni + econ.hh, data=df)
risk00 <- lm(prior.belief ~ high.risk + resident + pol.lean.three + high.concern  + female + age + uni + econ.hh, data=df)
risk000 <- lm(prior.belief ~ index.score + resident + pol.lean.three + high.concern  + female + age + uni + econ.hh, data=df)

risk1 <- lm(over ~ resident +  high.concern  + pol.lean.three + high.concern  + female + age + uni + econ.hh, data=df)

risk2 <- lm(lover ~ resident +  high.concern + pol.lean.three + high.concern  + female + age + uni + econ.hh, data=df)

stargazer::stargazer(risk000, risk0, risk00, risk1,risk2, type = "text", title = "Predictors of prior policy-induced job risk beliefs",
                     omit.stat=c("ser","f"), dep.var.labels = c("Prior job risk belief", "Over-estimator", "Strong over (>5)"),
                     covariate.labels = c("Risk index score", 
                                          
                                          "Sectoral risk exposure (Carbon intensity of GVA)",
                                          "Occupational transition adaptive capacity (Green job potential score)", 
                                          "Regional sensitivity (Unemployment rate)",
                                          "High risk (Risk > 10)",
                                          "Sweden",
                                          "Switzerland",
                                          "United States",
                                          "Political leaning CENTER",
                                          "Political leaning RIGHT",
                                          "High climate change concern",
                                          "Female",
                                          "25 to 34 years",
                                          "35 to 44 years",
                                          "45 to 54 years",
                                          "55+ years",
                                          "University degree",
                                          "Economic struggle"))

#########################################################
# FOREST PLOT DRIVERS JOB RISK BELIEFS (FIGURE 3B)
coefs = as.data.frame(summary(risk0)$coefficients[,1:2])
names(coefs)[2] = "se" 
coefs$sort <- c(1,2,3,4,5,6,7,8,9,10,11,12, 13, 14, 15, 16, 17)
coefs <- arrange(coefs, sort)
coefs$vars = c("Constant", "Sector score", "Occupation score","Region score", 
               "Sweden", "Switzerland", "United States",  
               "Center", "Right", "Concern", "Female", 
               "Age 25-34", "Age 35-44", "Age 45-54", "Age 55+",
               "University degree", "Economic struggle")

coefs <- slice(coefs, -c(10:17))
#---------------------------------------------------------------------------------------
# PLOT
beliefpred <- ggplot(coefs, aes(reorder(vars, sort), Estimate, label=vars)) + 
  geom_hline(yintercept=0, lty=3, lwd=0.5, colour="black") +
  geom_errorbar(aes(ymin=Estimate - 1.96*se, ymax=Estimate + 1.96*se), colour="black", 
                lwd=0.3,  width=0.25) +
  geom_point(size=1.5, pch=21, fill="grey", color="black") +
  theme_classic(base_size = 12) +  geom_text(aes(y=Estimate + 1.96*se + 0.2),size=3.2,color="black") +
  labs(
    title = "(b) Predictors of policy-induced job risk beliefs",
    x = "",
    y = "Coefficients and 95%-confidence bands") +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(), axis.ticks.x = element_blank()) +
  annotate("text", x = 3, y = -3.5, label = "Risk sub-components", 
           fontface =2, colour = "black", size=3.2, hjust = 0.5) +
  annotate("text", x = 6, y = -3.5, label = "Country", 
           fontface =2, colour = "black", size=3.2, hjust = 0.5) +
  annotate("text", x = 8.5, y = -3.5, label = "Political leaning", 
           fontface =2, colour = "black", size=3.2, hjust = 0.5) 
#################################################################################################
#################################################################################################
### BAR CHART WITH MEAN JOB RISK BELIEFS vs MEAN INDEX SCORES, overall and by country (FIGURE 3A)
# Function to compute mean and 95% CI
summary_stats <- function(x) {
  m <- mean(x, na.rm = TRUE)
  se <- sd(x, na.rm = TRUE) / sqrt(length(na.omit(x)))
  ci <- 1.96 * se
  return(c(mean = m, lower = m - ci, upper = m + ci))
}

# List all measures of interest
measures <- c("prior.belief", "index.score", "sector.score", "occupation.score", "unemp.score")

# Country-level summary (mean + 95% CI)
df_summary <- df %>%
  dplyr::select(resident, all_of(measures)) %>%
  pivot_longer(cols = all_of(measures), names_to = "measure", values_to = "value") %>%
  dplyr::group_by(resident, measure) %>%
  dplyr::summarise(
    mean = mean(value, na.rm = TRUE),
    lower = mean(value, na.rm = TRUE) - 1.96 * sd(value, na.rm = TRUE) / sqrt(n()),
    upper = mean(value, na.rm = TRUE) + 1.96 * sd(value, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# Overall summary (no resident breakdown)
df_overall <- df %>%
  pivot_longer(cols = all_of(measures), names_to = "measure", values_to = "value") %>%
  dplyr::group_by(measure) %>%
  dplyr::summarise(
    mean = mean(value, na.rm = TRUE),
    lower = mean(value, na.rm = TRUE) - 1.96 * sd(value, na.rm = TRUE) / sqrt(n()),
    upper = mean(value, na.rm = TRUE) + 1.96 * sd(value, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  mutate(resident = "Overall")

# Combine into one plotting dataset
df_plot <- bind_rows(df_summary, df_overall)

df_plot$measure <- factor(
  df_plot$measure,
  levels = c("prior.belief", "index.score", "sector.score", "occupation.score", "unemp.score"),
  labels = c("Job risk belief", "Job risk score", "Sector score", "Occupation score", "Unemployment score")
)


df_plot$resident <- factor(
  df_plot$resident,
  levels = c("Overall", "Germany", "Sweden", "Switzerland", "United States of America"),  
  labels = c("Overall", "Germany", "Sweden", "Switzerland", "United States")
  
)

p1 <- ggplot(df_plot %>% filter(measure %in% c("Job risk belief", "Job risk score")), aes(x = resident, y = mean, fill = measure)) +
  geom_col(position = position_dodge(0.6), width = 0.5, color="black") +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    position = position_dodge(0.6),
    width = 0.2
  ) +
  labs(
    title = "(a) Job risk beliefs and risk scores by country",
    x = "",
    y = "Mean values and 95%-confidence bands",
    fill = "Measure"
  ) +
  scale_fill_manual(
    values = c("Job risk belief" = "lightgrey", "Job risk score" = "white")  ) +
  theme_classic(base_size = 12) +
 
  theme(
    legend.position = c(0.3, 0.95),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8, face = "bold"),
    legend.background = element_rect(fill = "white", linewidth = 0.5, linetype = "solid", colour = "black") ,legend.direction = "horizontal") +
  theme(axis.title.x=element_blank())


overall_diff <- df %>%
  summarise(
    mean_prior = mean(prior.belief, na.rm = TRUE),
    mean_index = mean(index.score, na.rm = TRUE),
    diff = mean_prior - mean_index
  )
#################################################################################################
# save canvas
comb1 <- grid.arrange(p1, beliefpred,  ncol = 2, widths=c(0.45,0.55))

ggsave(plot = comb1,
       "beliefpred.pdf",
       width = 12,
       height = 5,
       dpi = 800
)
#################################################################################################
# T-test mean differences job risk beliefs vs. job-profile based risk scores
t_result <- t.test(df$prior.belief, df$index.score, paired = TRUE)

# Print results
print(t_result)

#################################################################################################
#################################################################################################
# RESULT SECTION 4.2
#----------------------------------------------------------------------------------
# Information provision experiment: OVERALL (TABLE 2)
r00 <- lm(p.support.tax ~ flow , data = df_emp)
r01 <- lm(p.support.tax ~ flow , data = df_emp[df_emp$gap.cat=="A_strong over",])
r02 <- lm(p.support.tax ~ flow, data = df_emp[df_emp$gap.cat=="B_weak over",])
r03 <- lm(p.support.tax ~ flow, data = df_emp[df_emp$gap.cat=="C_accurate",])
r04 <- lm(p.support.tax ~ flow, data = df_emp[df_emp$gap.cat=="D_weak under",])
r05 <- lm(p.support.tax ~ flow, data = df_emp[df_emp$gap.cat=="E_strong under",])
#################################################################################################
# Print table 2
stargazer::stargazer(r00, r01, r02, r03, r04, r05, type = "text", title = "Information provision experiment",
                     dep.var.labels = c("Fossil fuel tax support"), covariate.labels = "Job risk information",
                     
                     column.labels = c("Overall", "Very good news", "Somewhat good news",
                                       "Neutral news", "Somewhat bad news",
                                       "Very bad news"),
                     
                     omit.stat=c("ser","f"))
#################################################################################################
# Information provision experiment by COUNTRY (TABLE D2 APPENDIX)
df_emp$gap.cat <- as.factor(df_emp$gap.cat)
df_emp$gap.cat <- factor(df_emp$gap.cat, levels = c("A_strong over",
                                                    "B_weak over",
                                                    "C_accurate",
                                                    "D_weak under",
                                                    "E_strong under"))

df_emp$flow2 <- df_emp$flow # For table design with interaction to differentiate reference from overall effect.
#-------------------------------------------------------------------------------------------
# Information provision experiment: GERMANY
r0g <- lm(p.support.tax ~ flow , data = df_emp[df_emp$resident=="Germany",])
r1g <- lm(p.support.tax ~ flow2*gap.cat , data = df_emp[df_emp$resident=="Germany",])
#-------------------------------------------------------------------------------------------
# Information provision experiment: Sweden
r0s <- lm(p.support.tax ~ flow , data = df_emp[df_emp$resident=="Sweden",])
r1s <- lm(p.support.tax ~ flow2*gap.cat, data = df_emp[df_emp$resident=="Sweden",])
#-------------------------------------------------------------------------------------------
# Information provision experiment: Switzerland
r0c <- lm(p.support.tax ~ flow , data = df_emp[df_emp$resident=="Switzerland",])
r1c <- lm(p.support.tax ~ flow2*gap.cat , data = df_emp[df_emp$resident=="Switzerland",])
#-------------------------------------------------------------------------------------------
# Information provision experiment: United States
r0u <- lm(p.support.tax ~ flow , data = df_emp[df_emp$resident=="United States of America",])
r1u <- lm(p.support.tax ~ flow2*gap.cat, data = df_emp[df_emp$resident=="United States of America",])
#-------------------------------------------------------------------------------------------
# PRINT TABLE D2 APPENDIX TREATMENT EFFECTS BY COUNTRY
stargazer::stargazer(r0g, r1g, r0s, r1s, r0c, r1c, r0u, r1u, type = "text",
                     title = "Information provision experiment",
                     dep.var.labels = c("Fossil fuel tax support"), 
                     covariate.labels = c("Treated", "Treated (Very good news)", "Somewhat good news", 
                                         "Neutral news","Somewhat bad news", 
                                         "Very bad news", "Treated X Somewhat good news",
                                         "Treated X Neutral news", "Treated X Somewhat bad news",
                                         "Treated X Very bad news"),
                     column.labels = c("Overall", "By information type",
                     "Overall", "By information type", 
                     "Overall", "By information type", 
                     "Overall", "By information type"
                                      ),
                     omit.stat=c("ser","f"))
#################################################################################################
# Information provision experiment by POLITICAL LEANING (FIGURE 4 and TABLE D3 APPENDIX)
library(sjPlot)
library(sjmisc)

# make categorical for PLOT_MODEL FUNCTION
df_emp$flow <- to_factor(df_emp$flow)
#-------------------------------------------------------------------------------------------
# Calculate treatment effect interactions
r00p <- lm(p.support.tax ~ flow*pol.lean.econ, data = df_emp)
r01p <- lm(p.support.tax ~ flow*pol.lean.econ, data = df_emp[df_emp$gap.cat=="A_strong over",]) 
r02p <- lm(p.support.tax ~ flow*pol.lean.econ, data = df_emp[df_emp$gap.cat=="B_weak over",]) 
r03p <- lm(p.support.tax ~ flow*pol.lean.econ, data = df_emp[df_emp$gap.cat=="D_weak under",]) 
r04p <- lm(p.support.tax ~ flow*pol.lean.econ, data = df_emp[df_emp$gap.cat=="E_strong under",]) 
r05p <- lm(p.support.tax ~ flow*pol.lean.econ, data = df_emp[df_emp$gap.cat=="C_accurate",]) 
#-------------------------------------------------------------------------------------------
# Panels for Figure 4
p3 <- plot_model(r00p, type = "pred", terms = c("pol.lean.econ", "flow"), colors = c("red","black")) + 
  ggtitle("(a) Information treatment effects by political leaning") + theme_classic(base_size = 12) +
  labs(y="Fossil fuel tax support") + theme(axis.title.x=element_blank()) +
  theme(legend.position = c(0.85,0.9), legend.text = element_text(size=6), legend.title = element_blank(),
        legend.background = element_rect(fill="white",
                                         linewidth =0.5, linetype="solid", 
                                         colour ="black")) 

p4 <- plot_model(r01p, type = "pred", terms = c("pol.lean.econ", "flow"), colors = c("red","black")) + ggtitle("(b) Very good news") + theme_classic(base_size = 12) + 
  theme(legend.position = "none")  + theme(axis.title.x=element_blank(),
                                                       axis.title.y = element_blank()) + ylim(1.5,7.5)

p5 <- plot_model(r02p, type = "pred", terms = c("pol.lean.econ", "flow"), colors = c("red","black")) + ggtitle("(c) Somewhat good news") + theme_classic(base_size = 12) + 
  theme(legend.position = "none") + theme(axis.title.x=element_blank(),
                                                       axis.title.y = element_blank()) + ylim(1.5,7.5)

p6 <- plot_model(r03p, type = "pred", terms = c("pol.lean.econ", "flow"), colors = c("red","black")) + ggtitle("(d) Somewhat bad news") + theme_classic(base_size = 12) + 
  theme(legend.position = "none")  + theme(axis.title.x=element_blank(),
                                                        axis.title.y = element_blank()) + ylim(1.5,7.5)

p7 <- plot_model(r04p, type = "pred", terms = c("pol.lean.econ", "flow"), colors = c("red","black")) + ggtitle("(e) Very bad news") + theme_classic(base_size = 12) + 
  theme(legend.position = "none") + theme(axis.title.x=element_blank(),
                                                                     axis.title.y = element_blank()) + ylim(1.5,7.5)
#-------------------------------------------------------------------------------------------
# Combine canvas
comb <- grid.arrange(p4,p5,p6,p7) 

p8 <- grid.arrange(p3, comb, ncol=2, widths=c(5,7))

t1 <- ggplot() + 
  annotate("text", x =6, y = 0.5, size=4, 
           label = "Political leaning", color = "black") + 
  theme_void() 

p9 <- grid.arrange(p8,t1, ncol=1, heights=c(11.25,0.75))

ggsave(plot = p9,
       "info_pol.pdf",
       width = 12,
       height = 5,
       dpi = 800
)
#-------------------------------------------------------------------------------------------
# Table D3
stargazer::stargazer(r00p, r01p, r02p, r05p, r03p, r04p,  type = "text", title = "Information provision experiment",
                     dep.var.labels = c("Fossil fuel tax support"), covariate.labels = c("Job risk information", "Political leaning (0[left]-10[right])",
                                                                                         "Information X Political leaning"),
                     
                     column.labels = c("Overall", "Very good news", "Somewhat good news",
                                       "Neutral news", "Somewhat bad news",
                                       "Very bad news"),
                     
                     omit.stat=c("ser","f"))

################################################################################################################################
# RESULT SECTION 4.2.1
################################################################################################################################
# Updating and learning rate heterogeneity (FIGURE 5 and TABLE D4 APPENDIX)
#---------------------------------------------------------
# UPDATING VARIABLE (Difference between posterior and prior job risk belief)
df_emp$update <- df_emp$post.belief - df_emp$prior.belief

# Binary variables for interactions (prior confidence and trust in science)
df_emp$trust.sci.bin <- ifelse(df_emp$trust.sci > 7, 1, 0)
df_emp$conf.prior <- ifelse(df_emp$conf.belief.prior %in% c("Completely confident", "Confident"), 1, 0)
#---------------------------------------------------------
# Calculate learning rates (Dependence of belief updating on perception gap)
up1 <- lm(update ~ gap, data=df_emp)
up2 <- lm(update ~ gap + pol.lean.three + trust.sci.bin + conf.prior + resident + female + age + uni + econ.hh, data=df_emp)
up3 <- lm(update ~ gap * resident + pol.lean.three + trust.sci.bin + conf.prior + female + age + uni + econ.hh, data=df_emp)
up4 <- lm(update ~ gap * pol.lean.three + trust.sci.bin + conf.prior + resident + female + age + uni + econ.hh, data=df_emp)
up5 <- lm(update ~ gap * trust.sci.bin + pol.lean.three + conf.prior + resident + female + age + uni + econ.hh, data=df_emp)
up6 <- lm(update ~ gap * conf.prior + pol.lean.three + trust.sci.bin + resident + female + age + uni + econ.hh, data=df_emp)
#---------------------------------------------------------
# Print table D4 APPENDIX
stargazer::stargazer(up1, up2, up3, up4, up5, up6, type = "text",
                     title = "Heterogenous learning rates",
                     dep.var.labels = "Belief updating [Posterior - Prior job risk belief]",
                     column.labels = c("Overall", "Overall", "By country", "Leaning", "Trust science", "Prior conf"),
                     omit.stat = c("f", "ser"),
                     covariate.labels = c("Perception gap [TRI score - Prior]",
                                          "Political leaning CENTER",
                                          "Political leaning RIGHT",
                                          "Trust in science (binary)",
                                          "Prior confidence (binary)",
                                          "Sweden", "Switzerland", "United States",
                                          "Female", "25 to 34 years", "35 to 44 years", "45 to 54 years", "55+ years",
                                          "University degree", 
                                          "Economic struggle",
                                          "Perception gap X Sweden",
                                          "Perception gap X Switzerland",
                                          "Perception gap X United States",
                                          "Perception gap X CENTER", "Perception gap X RIGHT", 
                                          "Perception gap X Trust in science", "Perception gap X Prior confidence"), digits = 3)

#---------------------------------------------------------
# CREATE FIGURE 5
colors3 <- c("white", "#CCCCCC", "#969696" ,"#636363", "#252525")
#---------------------------------------------------------
# FIGURE 5A
# mean belief prior posterior by group.
chartd <- df %>% filter(flow=="Treated") %>% dplyr::select(gap.cat, prior.belief, post.belief, index.score)

chartd <- melt(chartd, measure.vars = c("prior.belief", "post.belief"), variable.name = "prior_post", value.name = "belief")

chartd2 <- df %>%  filter(flow=="Treated") %>% dplyr::select(prior.belief, post.belief, index.score)
chartd2$gap.cat <- "Total"

chartd2 <- chartd2 %>% melt(measure.vars = c("prior.belief", "post.belief"), variable.name = "prior_post", value.name = "belief")


width <- .5
p_update2 <- ggplot(chartd, aes(x = gap.cat, 
                                y = belief, fill =prior_post)) +
  geom_bar(stat = "summary", fun = "mean", width = 0.5, color = "black", 
           position = position_dodge(width = 0.55), alpha=0.8) + 
  geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.1, linewidth = 0.2, 
                position = position_dodge(width = 0.55)) +
  theme_classic(base_size = 12) +  
  theme(
    legend.position = c(0.85,0.9),legend.direction = "vertical", legend.text = element_text(size=6), 
    legend.title = element_text(size=6, face = "bold"),
    legend.background = element_rect(fill="white",
                                     linewidth =0.5, linetype="solid", 
                                     colour ="black")) +
  scale_fill_manual(values = colors3[c(1,3)], name = "Risk belief",
                    labels=c("Pre-treatment", "Post-treatment")) + 
  xlab("Perception gap category") + 
  ylab("Mean job risk belief") +
  labs(title = "(a) Job risk belief updating") + 
  scale_x_discrete(labels=c("Very good news", "Somewhat good news", "Neutral news",
                            "Somewhat bad news", "Very bad news"))
#--------------------------------------------------------------------------------------------------------------------
#### LEARNING RATE HETEROGENEITY (FIGURE 5B)
df_learn <- as.data.frame(matrix(nrow=12, ncol=5))
colnames(df_learn) <- c( "outcome", "model", "coefficient.name", "coefficient.value", "pval")
df_learn[,2] <- c("overall", "country", "country", "country", "country",
                  "political leaning", "political leaning", "political leaning",
                  "trust in science", "trust in science", "prior confidence", "prior confidence")
df_learn[,1] <- c("")
df_learn[,3] <- as.factor(c("overall", "germany", "sweden",  "switzerland", "united states", 
                            "left", "center", "right", "trust low", 
                            "trust high", "confidence low", "confidence high"))
df_learn[,4] <- c(up2$coefficients[2],
                  up3$coefficients[2],
                    up3$coefficients[2] + up3$coefficients[length(up3$coefficients)-2],
                  up3$coefficients[2] + up3$coefficients[length(up3$coefficients)-1],
                  up3$coefficients[2] + up3$coefficients[length(up3$coefficients)],
                  up4$coefficients[2], up4$coefficients[2] + up4$coefficients[length(up4$coefficients)-1],
                  up4$coefficients[2] + up4$coefficients[length(up4$coefficients)],
                  up5$coefficients[2],  up5$coefficients[2] + up5$coefficients[length(up5$coefficients)],
                  up6$coefficients[2],  up6$coefficients[2] + up6$coefficients[length(up6$coefficients)])
df_learn[,5] <- c("***")
level_order2 <- c("overall",  "germany", "sweden",  "switzerland", "united states",  "left", "center", "right", 
                  "trust low", "trust high", "confidence low", "confidence high")

learning3 <- ggplot(df_learn, aes(x = factor(coefficient.name, level = level_order2), y = outcome, fill = coefficient.value)) +
  geom_tile(color = "black" , alpha=0.6) +
  coord_fixed(ylim= c(1,2.5)) +
  guides(fill = guide_colourbar(barwidth = 6,
                                barheight = 1)) + theme_classic(base_size = 12) +
  scale_fill_distiller(palette ="Reds") +
  xlab("The effect of the perception gap [Prior belief - job risk score] on belief update [Posterior – Prior belief]")+ 
  ylab("") + #theme(legend.position = c(0.77,1)) +
  theme(legend.direction = "horizontal", legend.text = element_text(size=7), legend.title = element_text(size=7, face = "bold"),
        legend.background = element_rect(fill="white",
                                         linewidth =0.5, linetype="solid", 
                                         colour ="black"))+
  geom_text(aes(label = paste0(round(coefficient.value, 3),pval)), color = "black", size = 4) +
  annotate("rect", xmin = 0.5, xmax = 1.5, ymin = 1.55, ymax = 2.5, alpha = .2, color="black") +
  annotate("rect", xmin = 1.5, xmax = 5.5, ymin = 2, ymax = 2.5, alpha = .2, color="black") +
  annotate("rect", xmin = 5.5, xmax = 8.5, ymin = 2, ymax = 2.5, alpha = .2, color="black") +
  annotate("rect", xmin = 8.5, xmax = 10.5, ymin = 2, ymax = 2.5, alpha = .2, color="black") +
  annotate("rect", xmin = 10.5, xmax = 12.5, ymin = 2, ymax = 2.5, alpha = .2, color="black") +
  
  annotate("text", x = 1, y = 2.25, label = "Overall") +
  annotate("text", x = 3.5, y = 2.25, label = "Country") +
  annotate("text", x = 7, y = 2.25, label = "Political leaning") +
  annotate("text", x = 9.5, y = 2.25, label = "Trust in science") +
  annotate("text", x = 11.5, y = 2.25, label = "Prior confidence") +
  
  annotate("rect", xmin = 1.5, xmax = 2.5, ymin = 1.55, ymax = 2,  alpha = .01, color="black") +
  annotate("rect", xmin = 2.5, xmax = 3.5, ymin = 1.55, ymax = 2,  alpha = .01, color="black") +
  annotate("rect", xmin = 3.5, xmax = 4.5, ymin = 1.55, ymax = 2,  alpha = .01, color="black") +
  annotate("rect", xmin = 4.5, xmax = 5.5, ymin = 1.55, ymax = 2, alpha = .01, color="black") +
  annotate("rect", xmin = 5.5, xmax = 6.5, ymin = 1.55, ymax = 2, alpha = .01, color="black") +
  annotate("rect", xmin = 6.5, xmax = 7.5, ymin = 1.55, ymax = 2, alpha = .01, color="black") +
  annotate("rect", xmin = 7.5, xmax = 8.5, ymin = 1.55, ymax = 2, alpha = .01, color="black") +
  annotate("rect", xmin = 8.5, xmax = 9.5, ymin = 1.55, ymax = 2, alpha = .01, color="black") +
  annotate("rect", xmin = 9.5, xmax = 10.5, ymin = 1.55, ymax = 2, alpha = .01, color="black") +
  annotate("rect", xmin = 10.5, xmax = 11.5, ymin = 1.55, ymax = 2, alpha = .01, color="black") +
  annotate("rect", xmin = 11.5, xmax = 12.5, ymin = 1.55, ymax = 2, alpha = .01, color="black") +
  
  annotate("text", x = 2, y = 1.8, label = "Germany") +
  annotate("text", x = 3, y = 1.8, label = "Sweden") +
  annotate("text", x = 4, y = 1.8, label = "Switzerland") +
  annotate("text", x = 5, y = 1.8, label = "USA") +
  annotate("text", x = 6, y = 1.8, label = "Left") +
  annotate("text", x = 7, y = 1.8, label = "Center") +
  annotate("text", x = 8, y = 1.8, label = "Right") +
  annotate("text", x = 9, y = 1.8, label = "Low") +
  annotate("text", x = 10, y = 1.8, label = "High") +
  annotate("text", x = 11, y = 1.8, label = "Low") +
  annotate("text", x = 12, y = 1.8, label = "High")+
  
  theme(axis.ticks.x=element_blank(),
  axis.text.x=element_blank(),
  axis.ticks.y = element_blank()) + 
  theme(legend.position = "none") +
  labs(title = "(b) Learning rate heterogeneity", fill="Coefficients for perception gap") 

#-------------------------------------------------------------------
# COMBINE FIGURE 5A and 5B in one canvas
comb5 <- grid.arrange(p_update2, learning3, ncol=1, heights = c(3,4))

# save canvas
ggsave(plot = comb5,
       "update.pdf",
       width = 12,
       height = 7,
       dpi = 800
)

######################################################################################
######################################################################################

### MORE APPENDICES RESULTS

##########################################################################################
# SECTOR distribution / Risk score subcomponent means by country (APPENDIX Section C)
#########################################################################################
# Ensure labels are clean and consistent
df_wide <- df_plot %>%
  pivot_wider(
    id_cols = resident,
    names_from = measure,
    values_from = c(mean, lower, upper),
    names_glue = "{.value}_{measure}"
  )

df_wide2 <- df_wide[,1] %>% cbind(round(df_wide[,c(4,9,14,2,7,12,5,3,6)], digits = 3))
#---------------------------------------------------------------------------------------
# Print TABLE C1 Appendix
stargazer::stargazer(
  df_wide2,
  summary = FALSE,
  rownames = FALSE,
  title = "Country-Level Summary of Job Risk Beliefs and Index Components",
  label = "tab:job_risk_summary",
  type = "text",
  column.labels = c("Country", "Job risk score", "")
)
################################################################
# Sample representation of sectors by country
df_sample_emp <- df %>% dplyr::filter(Screener !="No") %>% dplyr::group_by(resident) %>% dplyr::summarise(sample = n())
df_sector_emp <- df %>% dplyr::filter(Screener !="No") %>% dplyr::group_by(resident, sector_combined) %>% dplyr::summarise(sector_obs = n())
df_sector_emp <- merge(df_sector_emp, df_sample_emp , by = "resident")
df_sector_emp$obs_share <- df_sector_emp$sector_obs / df_sector_emp$sample

df_sector_emp2 <- df_sector_emp %>%
  reshape2::dcast(sector_combined ~ resident, value.var = c("sector_obs"), fill = 0)
colnames(df_sector_emp2)[c(2:5)] <- c("obs_DE", "obs_SE", "obs_CH", "obs_US")

df_sector_emp3 <- df_sector_emp %>%
  reshape2::dcast(sector_combined ~ resident, value.var = c("obs_share"), fill = 0)
colnames(df_sector_emp3)[c(2:5)] <- c("share_DE", "share_SE", "share_CH", "share_US")

# Import emission intensity info of top and bottom 10 sectors (based on CH)
emintens <- read.xlsx("exposure_by_country.xlsx")

df_sectortable <- merge(emintens, df_sector_emp2, by="sector_combined", all.x = T)
df_sectortable <- merge(df_sectortable, df_sector_emp3, by="sector_combined", all.x = T)

df_sectortable_new <- df_sectortable %>% arrange(Rank) %>% dplyr::select(-c(11:14))

df_sectortable_new[,c(3:14)] <- round(df_sectortable_new[,c(3:14)], digits = 3)

df_sectortable_new <- df_sectortable_new %>% relocate(share_DE, .after = share_US) %>%
  relocate(share_CH, .before = share_SE)
#-----------------------------------------------------------------------------------
# Print TABLE C2 Appendix
stargazer::stargazer(
  df_sectortable_new,
  summary = FALSE,
  rownames = FALSE,
  title = "Country-Level Summary of sector exposure, including sample representations",
  label = "tab:job_risk_summary",
  type = "text", digits = 3
)
################################################################
# Histograms of sector scores (FIGURE C1)
p2 <- ggplot(data = df) + geom_histogram(aes(x=sector.score), fill="lightgrey", color="black", bins = 21, position = "dodge") + 
  facet_wrap(~resident, ncol=4)  +
  labs(y="Number of observations", x="Sector score") + theme_classic(base_size = 12) + theme(legend.position = "none")
#-----------------------------------------------------------------------------------
ggsave(plot = p2,
       "sector_hist.pdf",
       width = 10,
       height = 3,
       dpi = 800
)

######################################################################################
######################################################################################
### Sample balancedness (TABLE D1 APPENDIX)
df_check <- df %>% filter(flow!="Unemployed") %>% dplyr::select(flow, gap, prior.belief, pol.lean.econ, pol.lean.soc,
                                                                c.concern, age, education, econ.hh, female, anthro_cc) %>% mutate_if(is.factor,as.numeric)

balance_check.model <- function(x){ # x is the input
  
  # Conditional means
  mean_d1 <- mean(x[df_check$flow=="Treated"], na.rm=T) # treatment group mean
  mean_d0 <- mean(x[df_check$flow=="Control"], na.rm=T) # control group mean
  
  # Difference in means
  diff_d <- lm(x ~ df_check$flow)
  cov <- vcovHC(diff_d, type = "HC")
  robust.se <- sqrt(diag(cov))
  
  # Store output as a list 
  list(mean_d1 = mean_d1,
       mean_d0 = mean_d0,
       coeff = diff_d$coefficients[2], 
       robust.se = robust.se[2], 
       pval = 2*pnorm(-abs(diff_d$coefficients[2]/robust.se[2])) )             
}

y_desc <- df_check %>% dplyr::select(-1)
y_desc_names <- colnames(df_check)[2:ncol(df_check)]

diff_output <- apply(X = y_desc, MARGIN = 2, FUN = balance_check.model)
diff_output <- round(as.data.frame(rbindlist(diff_output)),2)

obs <- c(as.integer(nrow(df_check[df_check$flow=="Treated",])), 
         as.integer(nrow(df_check[df_check$flow=="Control",])), NA, NA, NA)

diff_output <- rbind(diff_output, obs)

rownames(diff_output)<- c(y_desc_names, "Observations")
colnames(diff_output)<- c("Treatment", "Control", "Difference", "s.e.", "p-value") 

diff_output_df <- as.data.frame(diff_output)
diff_output_df$Variable <- rownames(diff_output_df)
rownames(diff_output_df) <- NULL

diff_output_df <- diff_output_df[, c("Variable", "Treatment", "Control", "Difference", "s.e.", "p-value")]
#--------------------------------------------------------------------------------------------------------
stargazer(
  diff_output_df,
  summary = FALSE,
  rownames = FALSE,
  digits = 2,
  type = "text",
  title = "Covariate Balance Between Treatment and Control Groups",
  label = "tab:balance_table")

#####################################################################################################
#####################################################################################################

