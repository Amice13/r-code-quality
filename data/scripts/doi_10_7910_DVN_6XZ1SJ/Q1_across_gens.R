
# Required packages:
library(tidyverse)
library(reshape2)
library(survival)
library(survminer)
library(car)
library(grid)
library(ggpubr)
library(emmeans)
library(MASS)
library(lme4)
library(lmerTest)
library(performance)

#--------------------------------------------------------------------
# Loading and reformatting survivorship data--------------------------------------------------------------------

surv_data_L5 <- read_csv("survival_L5.csv")
surv_data_RUS <- read_csv("survival_RUS.csv")

# Loading data on maternal origin

origins_L5 <- read_csv("neonate_origins_L5_matriline.csv")

origins_RUS <- read_csv("neonate_origins_RUS_matriline.csv")

# Creating a matriline ID column

origins_L5$matriline_ID <- paste(origins_L5$strain,
                                 origins_L5$matriline)

origins_RUS$matriline_ID <- paste(origins_RUS$strain,
                                  origins_RUS$matriline)

# Back to working with survivorship data:
# Changing the order of the factors
surv_data_L5 <- surv_data_L5[with(surv_data_L5, order(generation, line, switched_age)),]
surv_data_RUS <- surv_data_RUS[with(surv_data_RUS, order(generation, line, switched_age)),]

# Pulling out the censor column, which we will need later
censor_L5 <- surv_data_L5$censor
surv_data_L5 <- surv_data_L5[,-37]

censor_RUS <- surv_data_RUS$censor
surv_data_RUS <- surv_data_RUS[,-34]

# Modifying the dataset from wide to long format (needed for plotting and analysis):
surv_long_L5 <- melt(data = surv_data_L5, 
                     id.vars = c("generation", "line", "switched_age", 
                                 "plate", "well"),
                     variable.name = "day",
                     value.name = "dead_alive")

surv_long_RUS <- melt(data = surv_data_RUS, 
                      id.vars = c("generation", "line", "switched_age", 
                                  "plate", "well"),
                      variable.name = "day",
                      value.name = "dead_alive")

# Lifespan analysis: formatting data--------------------------------------

#L5
# Calculate survival times for each individual
survival_data_per_ind_L5 <- surv_long_L5 %>%
  group_by(generation, line, switched_age, plate, well) %>%
  summarize(surv_time = sum(dead_alive, na.rm = TRUE))

# Adding the censor column back and renaming it
survival_data_per_ind_L5 <- cbind(survival_data_per_ind_L5, censor_L5)

survival_data_per_ind_L5 <- rename(survival_data_per_ind_L5, "censor" = "...7")

# Making a column with combined full sample information: generation, line, switched age (useful for analysis and plotting later)
survival_data_per_ind_L5$final_line <- paste(survival_data_per_ind_L5$generation,
                                             survival_data_per_ind_L5$line,
                                             survival_data_per_ind_L5$switched_age)

# Creating a dataset that does not include censored individuals
surv_data_no_censor_L5 <-  filter(survival_data_per_ind_L5, censor != 0)

# RUS
# Calculate survival times for each individual
survival_data_per_ind_RUS <- surv_long_RUS %>%
  group_by(generation, line, switched_age, plate, well) %>%
  summarize(surv_time = sum(dead_alive, na.rm = TRUE))

# Adding the censor column back and renaming it
survival_data_per_ind_RUS <- cbind(survival_data_per_ind_RUS, censor_RUS)

survival_data_per_ind_RUS <- rename(survival_data_per_ind_RUS, "censor" = "...7")

# Making a column with combined full sample information: generation, line, switched age (useful for analysis and plotting later)
survival_data_per_ind_RUS$final_line <- paste(survival_data_per_ind_RUS$generation,
                                              survival_data_per_ind_RUS$line,
                                              survival_data_per_ind_RUS$switched_age)

# Creating a dataset that does not include censored individuals
surv_data_no_censor_RUS <-  filter(survival_data_per_ind_RUS, censor != 0)

# Need to merge survivorship data and neonate origin data (so that we can include mom identity in the models below)

surv_origin_L5 <- merge(surv_data_no_censor_L5, origins_L5, 
                         by = c("generation", "line", "switched_age", "plate", "well"))

surv_origin_RUS <- merge(surv_data_no_censor_RUS, origins_RUS, 
                        by = c("generation", "line", "switched_age", "plate", "well"))

# Combining the strain datasets

surv_data_no_censor_combo <- rbind(surv_origin_L5, surv_origin_RUS)

# Checking the number of matrilines

summary_n <- surv_data_no_censor_combo %>%
  group_by(strain, generation, line, switched_age) %>%
  summarise(n = length(unique(matriline_ID)))
summary_n

# GLMM on lifespan (count data)---------------------------

# Excluding switched age cohorts in G3 and the G0 generation (not relevant to Question 1)
surv_data_no_censor_G1_3 <- surv_data_no_censor_combo %>%
  filter(switched_age == "N")

# generalized mixed model
# Using Poisson for count data

surv_glmer <- glmer(surv_time ~ strain * line * generation + (1|matriline_ID), family=poisson, data=surv_data_no_censor_G1_3)

# in 'performance' package
check_overdispersion(surv_glmer)
# overdispersed

# Negative binomial GLMM

surv_glmer_nb <- glmer.nb(surv_time ~ strain * line * generation + (1|matriline_ID), data=surv_data_no_censor_G1_3)
surv_summary_nb <- summary(surv_glmer_nb)
surv_summary_nb

surv_table <- surv_summary_nb[[10]]
surv_table <- as.data.frame(surv_table)

# to export model summary for supplement

write.csv(surv_table, "Q1_surv.csv")

# likelihood ratio test

drop1(surv_glmer_nb, test = "Chisq") # tests for 3 way interaction

# model with all two-way interactions included
surv_glmer_twoway <- glmer.nb(surv_time ~ strain + line + generation
                         + strain*generation + strain*line + line*generation
                        + (1|matriline_ID), data=surv_data_no_censor_G1_3)

surv_glmer_SG <- glmer.nb(surv_time ~ strain + line + generation
                       + strain*line + line*generation
                       + (1|matriline_ID), data=surv_data_no_censor_G1_3)

surv_glmer_SL <- glmer.nb(surv_time ~ strain + line + generation
                           + strain*generation + line*generation
                           + (1|matriline_ID), data=surv_data_no_censor_G1_3)

surv_glmer_LG <- glmer.nb(surv_time ~ strain + line + generation
                            + strain*line + strain*generation
                           + (1|matriline_ID), data=surv_data_no_censor_G1_3)

# just main effects
surv_glmer_main <- glmer.nb(surv_time ~ strain + line + generation
                     + (1|matriline_ID), data=surv_data_no_censor_G1_3)

# testing for SG interaction
anova(surv_glmer_twoway, surv_glmer_SG) # significant

# testing for SL interaction
anova(surv_glmer_twoway, surv_glmer_SL) # significant

# testing for LG interaction
anova(surv_glmer_twoway, surv_glmer_LG)

# Model validation (plotting residuals)
plot(surv_glmer_nb)

# Post hoc comparisons: which maternal age groups differ within generations for each strain?

pairwise_survival <- emmeans(surv_glmer_nb, pairwise ~ line | generation | strain)
pairwise_survival

# Lifespan L5 summary statistics---------------------------------------------------------------------------------

# Calculating summary statistics
surv_summary_L5 <- surv_data_no_censor_L5 %>%
  group_by(generation, line, switched_age) %>%
  summarize(
    mean=mean(surv_time, na.rm = TRUE), 
    SD=sd(surv_time, na.rm =TRUE),
    median=median(surv_time, na.rm = TRUE),
    SE=sd(surv_time, na.rm =TRUE)/sqrt(length(!is.na(surv_time))),
    variance=var(surv_time, na.rm = TRUE),
    n=length(surv_time))
surv_summary_L5

surv_summary_L5$final_line <- paste(surv_summary_L5$generation,
                                    surv_summary_L5$line,
                                    surv_summary_L5$switched_age)

# Plotting survivorship curves - Generation 1 - L5:-----------------------------------------------------

surv_L5_gen1 <- filter(surv_data_no_censor_L5, generation == "F1")

fit_L5_gen1 <- survfit(Surv(surv_time, censor) ~ line, data = surv_L5_gen1)

print(fit_L5_gen1)
summary(fit_L5_gen1)

surv_L5_gen1_plot <- ggsurvplot(
  fit_L5_gen1,
  data = surv_L5_gen1,
  linetype = c("solid","solid"),
  palette = c("#0072B2","black"),
  size = 0.75,
  conf.int = FALSE,        
  pval = FALSE, 
  censor = FALSE,
  xlab = element_blank(),
  xlim = c(0,30),
  break.x.by = 5,
  ylab = element_blank(),
  font.tickslab = c(14),
  legend = "none")

surv_L5_gen1_plot$plot <- surv_L5_gen1_plot$plot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                         panel.background = element_blank(), axis.line = element_line(colour = "black"))

surv_L5_gen1_plot$plot <- surv_L5_gen1_plot$plot +  geom_label(
  label="G1", 
  size = 7,
  x=1.2,
  y=0.05,
  label.size = 0,
  color = "black")

surv_L5_gen1_plot

# Generation 2 - L5:-----------------------------------------------------

surv_L5_gen2 <- filter(surv_data_no_censor_L5, generation == "F2")

fit_L5_gen2 <- survfit(Surv(surv_time, censor) ~ line, data = surv_L5_gen2)

print(fit_L5_gen2)
summary(fit_L5_gen2)

surv_L5_gen2_plot <- ggsurvplot(
  fit_L5_gen2,
  data = surv_L5_gen2,
  linetype = c("solid","solid"),
  palette = c("#0072B2","black"),
  size = 0.75,
  conf.int = FALSE,        
  pval = FALSE,   
  censor = FALSE,
  xlab = element_blank(),
  xlim = c(0,30),
  break.x.by = 5,
  ylab = element_blank(),
  font.tickslab = c(14),
  legend = "none")

surv_L5_gen2_plot$plot <- surv_L5_gen2_plot$plot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                         panel.background = element_blank(), axis.line = element_line(colour = "black"))

surv_L5_gen2_plot$plot <- surv_L5_gen2_plot$plot +  geom_label(
  label="G2", 
  size = 7,
  x=1.2,
  y=0.05,
  label.size = 0,
  color = "black")

surv_L5_gen2_plot

# Generation 3 - L5:-----------------------------------------------------

surv_L5_gen3 <- surv_data_no_censor_L5 %>%
  filter(generation == "F3") %>%
  filter(switched_age != "Y")

fit_L5_gen3 <- survfit(Surv(surv_time, censor) ~ final_line, data = surv_L5_gen3)

print(fit_L5_gen3)
summary(fit_L5_gen3)

surv_L5_gen3_plot <- ggsurvplot(
  fit_L5_gen3,
  data = surv_L5_gen3,
  linetype = c("solid","solid"),
  palette = c("#0072B2","black"),
  size = 0.75,
  conf.int = FALSE,        
  pval = FALSE, 
  censor = FALSE,
  xlab = element_blank(),
  xlim = c(0,30),
  break.x.by = 5,
  ylab = element_blank(),
  font.tickslab = c(14),
  legend = "none")

surv_L5_gen3_plot$plot <- surv_L5_gen3_plot$plot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                         panel.background = element_blank(), axis.line = element_line(colour = "black"))

surv_L5_gen3_plot$plot <- surv_L5_gen3_plot$plot +  geom_label(
  label="G3", 
  size = 7,
  x=0.9,
  y=0.05,
  label.size = 0,
  color = "black")

surv_L5_gen3_plot

# Creating the multipanel plot - L5:------------------------------

surv_curves_L5 <- ggarrange(surv_L5_gen1_plot$plot, surv_L5_gen2_plot$plot,
                            surv_L5_gen3_plot$plot,
                            ncol = 1, nrow = 3,
                            heights = c(1, 1, 1))

surv_curves_L5 <- annotate_figure(surv_curves_L5, left = textGrob("Survival probability", 
                                                            rot = 90, gp = gpar(fontsize = 20)))
surv_curves_L5

# Lifespan summary statistics - RUS---------------------------------------------------------------------------------

# Calculating summary statistics
surv_summary_RUS <- surv_data_no_censor_RUS %>%
  group_by(generation, line, switched_age) %>%
  summarize(
    mean=mean(surv_time, na.rm = TRUE), 
    SD=sd(surv_time, na.rm =TRUE),
    median=median(surv_time, na.rm = TRUE),
    SE=sd(surv_time, na.rm =TRUE)/sqrt(length(!is.na(surv_time))),
    variance=var(surv_time, na.rm = TRUE),
    n=length(surv_time))
surv_summary_RUS

surv_summary_RUS$final_line <- paste(surv_summary_RUS$generation,
                                     surv_summary_RUS$line,
                                     surv_summary_RUS$switched_age)

# Plotting survivorship curves - Generation 1 - RUS:-----------------------------------------------------

surv_RUS_gen1 <- filter(surv_data_no_censor_RUS, generation == "F1")

fit_RUS_gen1 <- survfit(Surv(surv_time, censor) ~ line, data = surv_RUS_gen1)

print(fit_RUS_gen1)
summary(fit_RUS_gen1)

surv_RUS_gen1_plot <- ggsurvplot(
  fit_RUS_gen1,
  data = surv_RUS_gen1,
  linetype = c("solid","solid"),
  palette = c("#0072B2","black"),
  size = 0.75,
  conf.int = FALSE,        
  pval = FALSE,  
  censor = FALSE,
  xlab = element_blank(),
  xlim = c(0,30),
  break.x.by = 5,
  ylab = element_blank(),
  font.tickslab = c(14),
  legend = "none")

surv_RUS_gen1_plot$plot <- surv_RUS_gen1_plot$plot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                           panel.background = element_blank(), axis.line = element_line(colour = "black"))

surv_RUS_gen1_plot$plot <- surv_RUS_gen1_plot$plot +  geom_label(
  label="G1", 
  size = 7,
  x=0.9,
  y=0.05,
  label.size = 0,
  color = "black")

surv_RUS_gen1_plot

# Generation 2 - RUS:-----------------------------------------------------

surv_RUS_gen2 <- filter(surv_data_no_censor_RUS, generation == "F2")

fit_RUS_gen2 <- survfit(Surv(surv_time, censor) ~ line, data = surv_RUS_gen2)

print(fit_RUS_gen2)
summary(fit_RUS_gen2)

surv_RUS_gen2_plot <- ggsurvplot(
  fit_RUS_gen2,
  data = surv_RUS_gen2,
  linetype = c("solid","solid"),
  palette = c("#0072B2","black"),
  size = 0.75,
  conf.int = FALSE,        
  pval = FALSE,    
  censor = FALSE,
  xlab = element_blank(),
  xlim = c(0,30),
  break.x.by = 5,
  ylab = element_blank(),
  font.tickslab = c(14),
  legend = "none")

surv_RUS_gen2_plot$plot <- surv_RUS_gen2_plot$plot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                           panel.background = element_blank(), axis.line = element_line(colour = "black"))

surv_RUS_gen2_plot$plot <- surv_RUS_gen2_plot$plot +  geom_label(
  label="G2", 
  size = 7,
  x=0.9,
  y=0.05,
  label.size = 0,
  color = "black")

surv_RUS_gen2_plot

# Generation 3 - RUS:-----------------------------------------------------

surv_RUS_gen3 <- surv_data_no_censor_RUS %>%
  filter(generation == "F3") %>%
  filter(switched_age != "Y")

fit_RUS_gen3 <- survfit(Surv(surv_time, censor) ~ final_line, data = surv_RUS_gen3)

print(fit_RUS_gen3)
summary(fit_RUS_gen3)

surv_RUS_gen3_plot <- ggsurvplot(
  fit_RUS_gen3,
  data = surv_RUS_gen3,
  linetype = c("solid","solid"),
  palette = c("#0072B2","black"),
  size = 0.75,
  conf.int = FALSE,        
  pval = FALSE,   
  censor = FALSE,
  xlab = element_blank(),
  xlim = c(0,30),
  break.x.by = 5,
  ylab = element_blank(),
  font.tickslab = c(14),
  legend = "none")

surv_RUS_gen3_plot$plot <- surv_RUS_gen3_plot$plot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                           panel.background = element_blank(), axis.line = element_line(colour = "black"))

surv_RUS_gen3_plot$plot <- surv_RUS_gen3_plot$plot +  geom_label(
  label="G3", 
  size = 7,
  x=0.9,
  y=0.05,
  label.size = 0,
  color = "black")

surv_RUS_gen3_plot

# Creating the multipanel plot - RUS:------------------------------

surv_curves_RUS <- ggarrange(surv_RUS_gen1_plot$plot, 
                             surv_RUS_gen2_plot$plot, surv_RUS_gen3_plot$plot, 
                             ncol = 1, nrow = 3,
                             heights = c(1, 1, 1))

surv_curves_RUS <- annotate_figure(surv_curves_RUS, left = textGrob("Survival probability", 
                                                                  rot = 90, gp = gpar(fontsize = 20)))

surv_curves_RUS

#--------------------------------------------------------------------
# Loading and reformatting neonate data--------------------------------------------------------------------

neonate_data_L5 <- read_csv("neonates_L5.csv")
neonate_data_RUS <- read_csv("neonates_RUS.csv")

# Changing the order of the factors
neonate_data_L5 <- neonate_data_L5[with(neonate_data_L5, order(generation, line, switched_age)),]
neonate_data_RUS <- neonate_data_RUS[with(neonate_data_RUS, order(generation, line, switched_age)),]

# Removing censored individuals (we were not able to quantify their LRO, lost before natural death)
neonate_data_no_censor_L5 <- filter(neonate_data_L5, censor != 0)
neonate_data_no_censor_RUS <- filter(neonate_data_RUS, censor != 0)

# Modifying the dataset from wide to long format (needed for plotting and analysis):
neo_long_nc_L5 <- melt(data = neonate_data_no_censor_L5,
                       id.vars = c("generation", "line", "switched_age", "plate", "well"),
                       variable.name = "day",
                       value.name = "num_neonates")

neo_long_nc_RUS <- melt(data = neonate_data_no_censor_RUS,
                        id.vars = c("generation", "line", "switched_age", "plate", "well"),
                        variable.name = "day",
                        value.name = "num_neonates")

# Removing 'censor' altogether, not needed after censored individuals are removed
neo_long_nc_L5 <- filter(neo_long_nc_L5, day != "censor")
neo_long_nc_RUS <- filter(neo_long_nc_RUS, day != "censor")

# L5: Calculating LRO, MDR, and 25% LRO per individual-----------------------

# Calculating LRO, maximum LRO, and 25% LRO (needed for downstream analyses)
LRO_per_ind_L5 <- neo_long_nc_L5 %>%
  group_by(generation, line, switched_age, plate, well) %>%
  summarize(LRO = sum(num_neonates, na.rm = TRUE),
            max_RO = max(num_neonates, na.rm = TRUE),
            percent_LRO = (sum(num_neonates, na.rm = TRUE))*0.25)

LRO_per_ind_L5$final_line <- paste(LRO_per_ind_L5$generation,
                                   LRO_per_ind_L5$line,
                                   LRO_per_ind_L5$switched_age)

# Summary statistics for LRO and maximum LRO
LRO_summary_L5 <- LRO_per_ind_L5 %>%
  group_by(generation, line, switched_age) %>%
  summarize(
    mean=mean(LRO, na.rm = TRUE), 
    SD=sd(LRO, na.rm =TRUE),
    med=median(LRO, na.rm = TRUE),
    SE=sd(LRO, na.rm =TRUE)/sqrt(length(!is.na(LRO))),
    n=length(LRO),
    mean_max=mean(max_RO, na.rm=TRUE),
    SD_max=sd(max_RO, na.rm =TRUE),
    SE_max=sd(max_RO, na.rm =TRUE)/sqrt(length(!is.na(max_RO))),
    med_max=median(max_RO, na.rm = TRUE),
    min=min(LRO, na.rm=TRUE),
    max=max(LRO, na.rm=TRUE),
    var=var(LRO, na.rm = TRUE))
LRO_summary_L5

LRO_summary_L5$final_line <- paste(LRO_summary_L5$generation, # needed for plotting later
                                   LRO_summary_L5$line,
                                   LRO_summary_L5$switched_age)

# RUS: Calculating LRO, MDR, and 25% LRO per individual-----------------------

# Calculating LRO, maximum LRO, and 25% LRO (needed for downstream analyses)
LRO_per_ind_RUS <- neo_long_nc_RUS %>%
  group_by(generation, line, switched_age, plate, well) %>%
  summarize(LRO = sum(num_neonates, na.rm = TRUE),
            max_RO = max(num_neonates, na.rm = TRUE),
            percent_LRO = (sum(num_neonates, na.rm = TRUE))*0.25)

LRO_per_ind_RUS$final_line <- paste(LRO_per_ind_RUS$generation,
                                    LRO_per_ind_RUS$line,
                                    LRO_per_ind_RUS$switched_age)

# Summary statistics for LRO and maximum LRO
LRO_summary_RUS <- LRO_per_ind_RUS %>%
  group_by(generation, line, switched_age) %>%
  summarize(
    mean=mean(LRO, na.rm = TRUE), 
    SD=sd(LRO, na.rm =TRUE),
    med=median(LRO, na.rm = TRUE),
    SE=sd(LRO, na.rm =TRUE)/sqrt(length(!is.na(LRO))),
    n=length(LRO),
    mean_max=mean(max_RO, na.rm=TRUE),
    SD_max=sd(max_RO, na.rm =TRUE),
    SE_max=sd(max_RO, na.rm =TRUE)/sqrt(length(!is.na(max_RO))),
    med_max=median(max_RO, na.rm = TRUE),
    min=min(LRO, na.rm=TRUE),
    max=max(LRO, na.rm=TRUE),
    var=var(LRO, na.rm = TRUE))
LRO_summary_RUS

LRO_summary_RUS$final_line <- paste(LRO_summary_RUS$generation, # needed for plotting later
                                    LRO_summary_RUS$line,
                                    LRO_summary_RUS$switched_age)

# Need to merge neonate data and neonate origin data (so that we can include mom identity in the models below)

neo_origin_L5 <- merge(LRO_per_ind_L5, origins_L5, 
                        by = c("generation", "line", "switched_age", "plate", "well"))

neo_origin_RUS <- merge(LRO_per_ind_RUS, origins_RUS, 
                         by = c("generation", "line", "switched_age", "plate", "well"))

# Combining the strain datasets

LRO_combo <- rbind(neo_origin_L5, neo_origin_RUS)

#--------------------------------------------------------------------
# Plotting daily reproduction over time - L5 strain---------------------------------------------

# Calculating the mean number of neonates produced across all individuals, per day
RO_per_day_L5 <- neo_long_nc_L5 %>%
  group_by(generation, line, switched_age, day) %>%
  summarize(total_offspring_per_day = sum(num_neonates, na.rm = TRUE),
            mean = mean(num_neonates, na.rm = TRUE),
            SE = sd(num_neonates, na.rm =   
                      TRUE)/sqrt(length(!is.na(num_neonates))))

RO_per_day_L5$final_line <- paste(RO_per_day_L5$generation,
                                  RO_per_day_L5$line,
                                  RO_per_day_L5$switched_age)

# Plotting for each generation: Generation 1 - L5------------------

RO_L5_G1 <- filter(RO_per_day_L5, generation == "F1")

RO_L5_G1$final_line <- factor(RO_L5_G1$final_line, levels = c("F1 old_mother N", "F1 young_mother N"))

RO_L5_G1$day <- as.numeric(RO_L5_G1$day)

RO_raw <- ggplot(data=RO_L5_G1, aes(x=day, y=mean)) + 
  geom_point(aes(group = final_line), size = 1.5) +
  geom_errorbar(aes(x=day, ymin=mean-SE, ymax=mean+SE), width=0.1)

RO_combo <- RO_raw+
  geom_line(data=RO_L5_G1, aes(x=day, y=mean, color = final_line, linetype = final_line, group=final_line), size = 1) +
  scale_color_manual(values = c("#0072B2", "black"), labels = c("O","Y")) +
  scale_linetype_manual(values = c("solid","solid"),labels = c("O","Y"))

RO_L5_plot_time_G1 <- RO_combo + 
  ylab(element_blank()) +
  ylim(c(0,5)) +
  xlab(element_blank()) +
  scale_x_continuous(breaks=seq(0,30,5)) +
  theme(panel.grid.major = element_blank(), 
        axis.text=element_text(size=14),
        legend.position = "none")

RO_L5_plot_time_G1 <- RO_L5_plot_time_G1 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                           panel.background = element_blank(), axis.line = element_line(colour = "black"))

RO_L5_plot_time_G1

# Generation 2 - L5:------------------------------

RO_L5_G2 <- filter(RO_per_day_L5, generation == "F2")

RO_L5_G2$final_line <- factor(RO_L5_G2$final_line, levels = c("F2 old_mother N", "F2 young_mother N"))

RO_L5_G2$day <- as.numeric(RO_L5_G2$day)

RO_raw <- ggplot(data=RO_L5_G2, aes(x=day, y=mean)) + 
  geom_point(aes(group = final_line), size = 1.5) +
  geom_errorbar(aes(x=day, ymin=mean-SE, ymax=mean+SE), width=0.1)

RO_combo <- RO_raw+
  geom_line(data=RO_L5_G2, aes(x=day, y=mean, color = final_line, linetype = final_line, group=final_line), size = 1) +
  scale_color_manual(values = c("#0072B2", "black"), labels = c("O","Y")) +
  scale_linetype_manual(values = c("solid","solid"),labels = c("O","Y"))

RO_L5_plot_time_G2 <- RO_combo + 
  ylab(element_blank()) +
  ylim(c(0,5)) +
  xlab(element_blank()) +
  scale_x_continuous(breaks=seq(0,30,5)) +
  theme(panel.grid.major = element_blank(), 
        axis.text=element_text(size=14),
        legend.position = "none")

RO_L5_plot_time_G2 <- RO_L5_plot_time_G2 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                 panel.background = element_blank(), axis.line = element_line(colour = "black"))

RO_L5_plot_time_G2

# Generation 3 - L5: -----------------------------

RO_L5_G3 <- RO_per_day_L5 %>%
  filter(generation == "F3") %>%
  filter(switched_age != "Y")

RO_L5_G3$final_line <- factor(RO_L5_G3$final_line, 
                              levels = c("F3 old_mother N", "F3 young_mother N"))

RO_L5_G3$day <- as.numeric(RO_L5_G3$day)

RO_raw <- ggplot(data=RO_L5_G3, aes(x=day, y=mean)) + 
  geom_point(aes(group = final_line, color = final_line), size = 1.5) +
  geom_errorbar(aes(x=day, ymin=mean-SE, ymax=mean+SE), width=0.1)

RO_combo <- RO_raw+
  geom_line(data=RO_L5_G3, aes(x=day, y=mean, color = final_line, linetype = final_line, group=final_line), size = 1) +
  scale_color_manual(values = c("#0072B2", "black"), labels = c("O", "Y")) +
  scale_linetype_manual(values = c("solid","solid"),labels = c("O", "Y"))

RO_L5_plot_time_G3 <- RO_combo + 
  ylab(element_blank()) +
  ylim(c(0,5)) +
  xlab(element_blank()) +
  scale_x_continuous(breaks=seq(0,30,5)) +
  theme(panel.grid.major = element_blank(), 
        axis.text=element_text(size=14),
        legend.position = "none")

RO_L5_plot_time_G3 <- RO_L5_plot_time_G3 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                 panel.background = element_blank(), axis.line = element_line(colour = "black"))

RO_L5_plot_time_G3

# Daily reproduction multipanel figure - L5 -----------

RO_L5_time <- ggarrange(RO_L5_plot_time_G1, RO_L5_plot_time_G2,
                        RO_L5_plot_time_G3, 
                        ncol = 1, nrow = 3)

RO_L5_time <- annotate_figure(RO_L5_time,
                             left = textGrob(expression(paste("Offspring ind"^"-1"*"day"^"-1")), 
                                             rot = 90, gp = gpar(fontsize = 20)))

RO_L5_time

# Plotting daily reproduction over time - RUS strain---------------------------------------------

# Calculating the mean number of neonates produced across all individuals, per day
RO_per_day_RUS <- neo_long_nc_RUS %>%
  group_by(generation, line, switched_age, day) %>%
  summarize(total_offspring_per_day = sum(num_neonates, na.rm = TRUE),
            mean = mean(num_neonates, na.rm = TRUE),
            SE = sd(num_neonates, na.rm =   
                      TRUE)/sqrt(length(!is.na(num_neonates))))

RO_per_day_RUS$final_line <- paste(RO_per_day_RUS$generation,
                                   RO_per_day_RUS$line,
                                   RO_per_day_RUS$switched_age)

# Plotting for each generation: Generation 1 - RUS----------------

RO_RUS_G1 <- filter(RO_per_day_RUS, generation == "F1")

RO_RUS_G1$final_line <- factor(RO_RUS_G1$final_line, levels = c("F1 old_mother N", "F1 young_mother N"))

RO_RUS_G1$day <- as.numeric(RO_RUS_G1$day)

RO_raw <- ggplot(data=RO_RUS_G1, aes(x=day, y=mean)) + 
  geom_point(aes(group = final_line), size = 1.5) +
  geom_errorbar(aes(x=day, ymin=mean-SE, ymax=mean+SE), width=0.1)

RO_combo <- RO_raw+
  geom_line(data=RO_RUS_G1, aes(x=day, y=mean, color = final_line, linetype = final_line, group=final_line), size = 1) +
  scale_color_manual(values = c("#0072B2", "black"), labels = c("O","Y")) +
  scale_linetype_manual(values = c("solid","solid"),labels = c("O","Y"))

RO_RUS_plot_time_G1 <- RO_combo + 
  ylab(element_blank()) +
  ylim(c(0,5)) +
  xlab(element_blank()) +
  scale_x_continuous(breaks=seq(0,30,5)) +
  theme(panel.grid.major = element_blank(), 
        axis.text=element_text(size=14),
        legend.position = "none")

RO_RUS_plot_time_G1 <- RO_RUS_plot_time_G1 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                 panel.background = element_blank(), axis.line = element_line(colour = "black"))

RO_RUS_plot_time_G1

# Generation 2 - RUS: ---------------------------

RO_RUS_G2 <- filter(RO_per_day_RUS, generation == "F2")

RO_RUS_G2$final_line <- factor(RO_RUS_G2$final_line, levels = c("F2 old_mother N", "F2 young_mother N"))

RO_RUS_G2$day <- as.numeric(RO_RUS_G2$day)

RO_raw <- ggplot(data=RO_RUS_G2, aes(x=day, y=mean)) + 
  geom_point(aes(group = final_line), size = 1.5) +
  geom_errorbar(aes(x=day, ymin=mean-SE, ymax=mean+SE), width=0.1)

RO_combo <- RO_raw+
  geom_line(data=RO_RUS_G2, aes(x=day, y=mean, color = final_line, linetype = final_line, group=final_line), size = 1) +
  scale_color_manual(values = c("#0072B2", "black"), labels = c("O","Y")) +
  scale_linetype_manual(values = c("solid","solid"),labels = c("O","Y"))

RO_RUS_plot_time_G2 <- RO_combo + 
  ylab(element_blank()) +
  ylim(c(0,5)) +
  xlab(element_blank()) +
  scale_x_continuous(breaks=seq(0,30,5)) +
  theme(panel.grid.major = element_blank(), 
        axis.text=element_text(size=14),
        legend.position = "none")

RO_RUS_plot_time_G2 <- RO_RUS_plot_time_G2 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                 panel.background = element_blank(), axis.line = element_line(colour = "black"))

RO_RUS_plot_time_G2

# Generation 3 - RUS:----------------------------

RO_RUS_G3 <- RO_per_day_RUS %>%
  filter(generation == "F3") %>%
  filter(switched_age != "Y")

RO_RUS_G3$final_line <- factor(RO_RUS_G3$final_line, 
                               levels = c("F3 old_mother N", "F3 young_mother N"))

RO_RUS_G3$day <- as.numeric(RO_RUS_G3$day)

RO_raw <- ggplot(data=RO_RUS_G3, aes(x=day, y=mean)) + 
  geom_point(aes(group = final_line, color = final_line), size = 1.5) +
  geom_errorbar(aes(x=day, ymin=mean-SE, ymax=mean+SE), width=0.1)

RO_combo <- RO_raw +
  geom_line(data=RO_RUS_G3, aes(x=day, y=mean, color = final_line, linetype = final_line, group=final_line), size = 1) +
  scale_color_manual(values = c("#0072B2", "black"), labels = c("O", "Y")) +
  scale_linetype_manual(values = c("solid","solid"),labels = c("O", "Y"))

RO_RUS_plot_time_G3 <- RO_combo + 
  ylab(element_blank()) +
  ylim(c(0,5)) +
  xlab(element_blank()) +
  scale_x_continuous(breaks=seq(0,30,5)) +
  theme(panel.grid.major = element_blank(), 
        axis.text=element_text(size=14),
        legend.position = "none")

RO_RUS_plot_time_G3 <- RO_RUS_plot_time_G3 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                 panel.background = element_blank(), axis.line = element_line(colour = "black"))

RO_RUS_plot_time_G3

# Daily reproduction multipanel figure - RUS--------------

RO_RUS_time <- ggarrange(RO_RUS_plot_time_G1, RO_RUS_plot_time_G2,
                         RO_RUS_plot_time_G3, 
                         ncol = 1, nrow = 3)

RO_RUS_time <- annotate_figure(RO_RUS_time,
                              left = textGrob(expression(paste("Offspring ind"^"-1"*"day"^"-1")), 
                                              rot = 90, gp = gpar(fontsize = 20)))
RO_RUS_time

#--------------------------------------------------------------------
# L5: Rate of reproductive senescence calculations:---------------------

# For each individual:
# Removing data BEFORE MDR, so that we are just capturing the decline,
# Then removing NA and trailing zeros after the last day of neonate production

# Adding MDR to the neo_long_nc dataframe

neo_long_MDR_L5 <- merge(neo_long_nc_L5, LRO_per_ind_L5, 
                         by = c("generation", "line", "switched_age", "plate", "well"))

neo_long_MDR_L5$MDR_match <- neo_long_MDR_L5$num_neonates == neo_long_MDR_L5$max_RO

neo_long_MDR_L5_2 <- filter(neo_long_MDR_L5, MDR_match == "TRUE")

neo_long_MDR_L5_2$day <- as.numeric(neo_long_MDR_L5_2$day)

neo_long_MDR_L5_3 <- neo_long_MDR_L5_2 %>%
  group_by(generation, line, switched_age, plate, well) %>%
  summarize(max_day = max(day))

neo_long_MDR_L5_final <- merge(neo_long_MDR_L5, neo_long_MDR_L5_3,
                               by = c("generation", "line", "switched_age", "plate", "well"))

neo_long_MDR_L5_final$day <- as.numeric(neo_long_MDR_L5_final$day)

neo_long_MDR_L5_final_trim <- filter(neo_long_MDR_L5_final, day >= max_day)

neo_long_MDR_L5_final_trim <- na.omit(neo_long_MDR_L5_final_trim)

neo_long_MDR_L5_final_trim <- filter(neo_long_MDR_L5_final_trim, num_neonates != 0)


# Need to merge neonate data and neonate origin data (so that we can include mom identity in the models below)

neo_long_MDR_L5_final_trim_merged <- merge(neo_long_MDR_L5_final_trim, origins_L5, 
                       by = c("generation", "line", "switched_age", "plate", "well"))


# Grouping data by cohort and generation:

dec_L5_G1_Y <- neo_long_MDR_L5_final_trim_merged %>%
  filter(final_line == "F1 young_mother N") %>%
  filter(day < 15) # day 15 and up had less than 3 reproductive individuals left
dec_L5_G1_O <- neo_long_MDR_L5_final_trim_merged %>%
  filter(final_line == "F1 old_mother N") %>%
  filter(day < 12)

dec_L5_G2_Y <- neo_long_MDR_L5_final_trim_merged %>%
  filter(final_line == "F2 young_mother N") %>%
  filter(day < 15)
dec_L5_G2_O <- neo_long_MDR_L5_final_trim_merged %>%
  filter(final_line == "F2 old_mother N") %>%
  filter(day < 13)

dec_L5_G3_Y <- filter(neo_long_MDR_L5_final_trim_merged, final_line == "F3 young_mother N")
dec_L5_G3_O <- filter(neo_long_MDR_L5_final_trim_merged, final_line == "F3 old_mother N")

dec_L5_G3_YO <- neo_long_MDR_L5_final_trim_merged %>%
  filter(final_line == "F3 young_mother Y") %>%
  filter(day < 12)
dec_L5_G3_OY <- neo_long_MDR_L5_final_trim_merged %>%
  filter(final_line == "F3 old_mother Y") %>%
  filter(day < 13)

# L5: Rate of reproductive senescence plots - Generation 1:---------------------

decline_L5_G1 <- rbind(dec_L5_G1_O, dec_L5_G1_Y)

summ_decline_L5_G1 <- decline_L5_G1 %>%
  group_by(generation, line, switched_age, day) %>%
  summarize(mean = mean(num_neonates, na.rm=TRUE))

plot1 <- ggplot(decline_L5_G1, aes(x=day, y=num_neonates, color=line)) +
  geom_jitter(alpha = 0.2, size = 0.5) +
  scale_color_manual(values = c("#0072B2", "black"), labels = c("O","Y")) +
  ylim(0,6) +
  xlim(0,16) +
  xlab(element_blank()) +
  ylab(element_blank()) +
  theme(panel.grid.major = element_blank(), 
        axis.text=element_text(size=14),
        legend.text=element_text(size=18),
        legend.title = element_blank(),
        legend.position = c(.85,.8),
        legend.key.width = unit(2.5, "line"),
        legend.key = element_rect(fill = NA))

G1_decline <- plot1 +
  geom_smooth(data = decline_L5_G1, aes(x=day, y=num_neonates, color = line), method="lm", se= FALSE)


G1_decline <- G1_decline + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                   panel.background = element_blank(), axis.line = element_line(colour = "black"))

G1_decline

# Generation 2 - L5------------------------

decline_L5_G2 <- rbind(dec_L5_G2_O, dec_L5_G2_Y)

summ_decline_L5_G2 <- decline_L5_G2 %>%
  group_by(generation, line, switched_age, day) %>%
  summarize(mean = mean(num_neonates, na.rm=TRUE))

plot1 <- ggplot(decline_L5_G2, aes(x=day, y=num_neonates, color = line)) +
  geom_jitter(alpha = 0.2, size = 0.5) +
  scale_color_manual(values = c("#0072B2", "black"), labels = c("OO","YY")) +
  xlab(element_blank()) +
  ylab(element_blank()) +
  ylim(0,6) +
  xlim(0,16) +
  theme(panel.grid.major = element_blank(), 
        axis.text=element_text(size=14),
        legend.text=element_text(size=18),
        legend.title = element_blank(),
        legend.position = c(.82,.8),
        legend.key.width = unit(2.5, "line"),
        legend.key = element_rect(fill = NA))

G2_decline <- plot1 +
  geom_smooth(data = decline_L5_G2, aes(x=day, y=num_neonates, color = line), method="lm", se= FALSE)


G2_decline <- G2_decline + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                 panel.background = element_blank(), axis.line = element_line(colour = "black"))

G2_decline

# Generation 3 - L5-----------------------------

decline_L5_G3_Y_O <- rbind(dec_L5_G3_Y, dec_L5_G3_O)

summ_decline_L5_G3_Y_O <- decline_L5_G3_Y_O %>%
  group_by(generation, line, switched_age, day) %>%
  summarize(mean = mean(num_neonates, na.rm=TRUE))

plot1 <- ggplot(decline_L5_G3_Y_O, aes(x=day, y=num_neonates, color = line)) +
  geom_jitter(alpha = 0.2, size = 0.5) +
  scale_color_manual(values = c("#0072B2", "black"), labels = c("OOO","YYY")) +
  xlab(element_blank()) +
  ylab(element_blank()) +
  ylim(0,6) +
  xlim(0,16) +
  theme(panel.grid.major = element_blank(), 
        axis.text=element_text(size=14),
        legend.text=element_text(size=18),
        legend.title = element_blank(),
        legend.position = c(.8,.8),
        legend.key.width = unit(2.5, "line"),
        legend.key = element_rect(fill = NA))

G3_decline <- plot1 +
  geom_smooth(data = decline_L5_G3_Y_O, aes(x=day, y=num_neonates, color = line), method="lm", se= FALSE)


G3_decline <- G3_decline + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                 panel.background = element_blank(), axis.line = element_line(colour = "black"))

G3_decline

# L5 Rate of reproductive senescence multipanel---------------

decline_curves_L5 <- ggarrange(G1_decline, G2_decline, G3_decline, 
                               ncol = 1, nrow = 3)

decline_curves_L5 <- annotate_figure(decline_curves_L5,
                              left = textGrob(expression(paste("Offspring ind"^"-1"*"day"^"-1")), 
                                              rot = 90, gp = gpar(fontsize = 20)))

decline_curves_L5

# L5: Rate of reproductive senescence analysis: Generation 1 - L5------------------

# Models for each maternal age:

lm_G1_L5_Y <- lmer(num_neonates ~ day + (1|matriline_ID), data = dec_L5_G1_Y)
summary(lm_G1_L5_Y)
lm_G1_L5_O <- lmer(num_neonates ~ day + (1|matriline_ID), data = dec_L5_G1_O)
summary(lm_G1_L5_O)

# Statistics (ANCOVA):

m.interaction <- lmer(num_neonates ~ day * line + (1|matriline_ID), data = decline_L5_G1)
anova(m.interaction)
summary(m.interaction)

# Generation 2 - L5---------------------

# Models for each maternal age:

lm_G2_L5_Y <- lmer(num_neonates ~ day + (1|matriline_ID), data = dec_L5_G2_Y)
summary(lm_G2_L5_Y)
lm_G2_L5_O <- lmer(num_neonates ~ day + (1|matriline_ID), data = dec_L5_G2_O)
summary(lm_G2_L5_O)

# Statistics (ANCOVA):

m.interaction <- lmer(num_neonates ~ day * line + (1|matriline_ID), data = decline_L5_G2)
anova(m.interaction)
summary(m.interaction)

# Generation 3 - L5---------------------------

lm_G3_L5_Y <- lmer(num_neonates ~ day + (1|matriline_ID), data = dec_L5_G3_Y)
summary(lm_G3_L5_Y)
lm_G3_L5_O <- lmer(num_neonates ~ day + (1|matriline_ID), data = dec_L5_G3_O)
summary(lm_G3_L5_O)

# Statistics (ANCOVA):

m.interaction <- lmer(num_neonates ~ day * line + (1|matriline_ID), data = decline_L5_G3_Y_O)
anova(m.interaction) #no
summary(m.interaction)

# RUS: Rate of reproductive senescence calculations:---------------------

# Removing data BEFORE MDR, so that we are just capturing the decline:

# Adding MDR to the neo_long_nc dataframe

neo_long_MDR_RUS <- merge(neo_long_nc_RUS, LRO_per_ind_RUS, 
                          by = c("generation", "line", "switched_age", "plate", "well"))

neo_long_MDR_RUS$MDR_match <- neo_long_MDR_RUS$num_neonates == neo_long_MDR_RUS$max_RO

neo_long_MDR_RUS_2 <- filter(neo_long_MDR_RUS, MDR_match == "TRUE")

neo_long_MDR_RUS_2$day <- as.numeric(neo_long_MDR_RUS_2$day)

neo_long_MDR_RUS_3 <- neo_long_MDR_RUS_2 %>%
  group_by(generation, line, switched_age, plate, well) %>%
  summarize(max_day = max(day))

neo_long_MDR_RUS_final <- merge(neo_long_MDR_RUS, neo_long_MDR_RUS_3,
                                by = c("generation", "line", "switched_age", "plate", "well"))

neo_long_MDR_RUS_final$day <- as.numeric(neo_long_MDR_RUS_final$day)

neo_long_MDR_RUS_final_trim <- filter(neo_long_MDR_RUS_final, day >= max_day)

neo_long_MDR_RUS_final_trim <- na.omit(neo_long_MDR_RUS_final_trim)

neo_long_MDR_RUS_final_trim <- filter(neo_long_MDR_RUS_final_trim, num_neonates != 0)

# Need to merge neonate data and neonate origin data (so that we can include mom identity in the models below)

neo_long_MDR_RUS_final_trim_merged <- merge(neo_long_MDR_RUS_final_trim, origins_RUS, 
                                           by = c("generation", "line", "switched_age", "plate", "well"))

# Grouping data by cohort and generation:

dec_RUS_G1_Y <- neo_long_MDR_RUS_final_trim_merged %>%
  filter(final_line == "F1 young_mother N") %>%
  filter(day < 21) # day 21 and up had less than 3 reproductive individuals left
dec_RUS_G1_O <- filter(neo_long_MDR_RUS_final_trim_merged, final_line == "F1 old_mother N")

dec_RUS_G2_Y <- neo_long_MDR_RUS_final_trim_merged %>%
  filter(final_line == "F2 young_mother N") %>%
  filter(day < 18)
dec_RUS_G2_Y <- dec_RUS_G2_Y[-426,] # this individual has extremely low reproduction, no decline to track
dec_RUS_G2_O <- neo_long_MDR_RUS_final_trim_merged %>%
  filter(final_line == "F2 old_mother N") %>%
  filter(day < 16)
dec_RUS_G2_O <- dec_RUS_G2_O[-310:-311,] # this individual has extremely low reproduction, no decline to track

dec_RUS_G3_Y <- filter(neo_long_MDR_RUS_final_trim_merged, final_line == "F3 young_mother N")
dec_RUS_G3_O <- filter(neo_long_MDR_RUS_final_trim_merged, final_line == "F3 old_mother N")

dec_RUS_G3_YO <- neo_long_MDR_RUS_final_trim_merged %>%
  filter(final_line == "F3 young_mother Y") %>%
  filter(day < 16)
dec_RUS_G3_OY <- filter(neo_long_MDR_RUS_final_trim_merged, final_line == "F3 old_mother Y")

# RUS: Rate of reproductive senescence plots - Generation 1:---------------------

decline_RUS_G1 <- rbind(dec_RUS_G1_O, dec_RUS_G1_Y)

summ_decline_RUS_G1 <- decline_RUS_G1 %>%
  group_by(generation, line, switched_age, day) %>%
  summarize(mean = mean(num_neonates, na.rm=TRUE))

plot1 <- ggplot(decline_RUS_G1, aes(x=day, y=num_neonates, color =line)) +
  geom_jitter(alpha = 0.2, size = 0.5) +
  scale_color_manual(values = c("#0072B2", "black"), labels = c("O","Y")) +
  ylim(0,6) +
  xlim(0,18) +
  xlab(element_blank()) +
  ylab(element_blank()) +
  theme(panel.grid.major = element_blank(), 
        axis.text=element_text(size=14),
        legend.text=element_text(size=18),
        legend.title = element_blank(),
        legend.position = c(.85,.8),
        legend.key.width = unit(2.5, "line"),
        legend.key = element_rect(fill = NA))

G1_decline_RUS <- plot1 +
  geom_smooth(data = decline_RUS_G1, aes(x=day, y=num_neonates, color = line), method="lm", se= FALSE)

G1_decline_RUS <- G1_decline_RUS + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                 panel.background = element_blank(), axis.line = element_line(colour = "black"))

G1_decline_RUS

# Generation 2 - RUS-------------------------

decline_RUS_G2 <- rbind(dec_RUS_G2_O, dec_RUS_G2_Y)

summ_decline_RUS_G2 <- decline_RUS_G2 %>%
  group_by(generation, line, switched_age, day) %>%
  summarize(mean = mean(num_neonates, na.rm=TRUE))

plot1 <- ggplot(decline_RUS_G2, aes(x=day, y=num_neonates, color =line)) +
  geom_jitter(alpha = 0.2, size = 0.5) +
  scale_color_manual(values = c("#0072B2", "black"), labels = c("OO","YY")) +
  xlab(element_blank()) +
  ylab(element_blank()) +
  ylim(0,6) +
  xlim(0,18) +
  theme(panel.grid.major = element_blank(), 
        axis.text=element_text(size=14),
        legend.text=element_text(size=18),
        legend.title = element_blank(),
        legend.position = c(.82,.8),
        legend.key.width = unit(2.5, "line"),
        legend.key = element_rect(fill = NA))

G2_decline_RUS <- plot1 +
  geom_smooth(data = decline_RUS_G2, aes(x=day, y=num_neonates, color = line), method="lm", se= FALSE)

G2_decline_RUS <- G2_decline_RUS + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                 panel.background = element_blank(), axis.line = element_line(colour = "black"))

G2_decline_RUS

# Generation 3 - RUS---------------------

decline_RUS_G3_Y_O <- rbind(dec_RUS_G3_Y, dec_RUS_G3_O)

summ_decline_RUS_G3_Y_O <- decline_RUS_G3_Y_O %>%
  group_by(generation, line, switched_age, day) %>%
  summarize(mean = mean(num_neonates, na.rm=TRUE))

plot1 <- ggplot(decline_RUS_G3_Y_O, aes(x=day, y=num_neonates, color = line)) +
  geom_jitter(alpha = 0.2, size = 0.5) +
  scale_color_manual(values = c("#0072B2", "black"), labels = c("OOO","YYY")) +
  xlab(element_blank()) +
  ylab(element_blank()) +
  ylim(0,6) +
  xlim(0,18) +
  theme(panel.grid.major = element_blank(), 
        axis.text=element_text(size=14),
        legend.text=element_text(size=18),
        legend.title = element_blank(),
        legend.position = c(.8,.8),
        legend.key.width = unit(2.5, "line"),
        legend.key = element_rect(fill = NA))

G3_decline_RUS <- plot1 +
  geom_smooth(data = decline_RUS_G3_Y_O, aes(x=day, y=num_neonates, color = line), method="lm", se= FALSE)

G3_decline_RUS <- G3_decline_RUS + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                 panel.background = element_blank(), axis.line = element_line(colour = "black"))

G3_decline_RUS

# RUS Rate of reproductive senescence multipanel -----------------

decline_curves_RUS <- ggarrange(G1_decline_RUS, G2_decline_RUS, G3_decline_RUS, 
                                ncol = 1, nrow = 3)

decline_curves_RUS <- annotate_figure(decline_curves_RUS,
                                     left = textGrob(expression(paste("Offspring ind"^"-1"*"day"^"-1")), 
                                                     rot = 90, gp = gpar(fontsize = 20)))

decline_curves_RUS

# RUS: Rate of reproductive senescence analysis: Generation 1 - RUS---------------------

# Models for each maternal age:

lm_G1_RUS_Y <- lmer(num_neonates ~ day + (1|matriline_ID), data = dec_RUS_G1_Y)
summary(lm_G1_RUS_Y)
lm_G1_RUS_O <- lmer(num_neonates ~ day + (1|matriline_ID), data = dec_RUS_G1_O)
summary(lm_G1_RUS_O)

# Statistics (ANCOVA):

m.interaction <- lmer(num_neonates ~ day * line + (1|matriline_ID), data = decline_RUS_G1)
anova(m.interaction)
summary(m.interaction)

# Generation 2 - RUS--------------------

# Models for each maternal age:

lm_G2_RUS_Y <- lmer(num_neonates ~ day + (1|matriline_ID), data = dec_RUS_G2_Y)
summary(lm_G2_RUS_Y)
lm_G2_RUS_O <- lmer(num_neonates ~ day + (1|matriline_ID), data = dec_RUS_G2_O)
summary(lm_G2_RUS_O)

# Statistics (ANCOVA):

m.interaction <- lmer(num_neonates ~ day * line + (1|matriline_ID), data = decline_RUS_G2)
anova(m.interaction)
summary(m.interaction)

# Generation 3 - RUS---------------------

lm_G3_RUS_Y <- lmer(num_neonates ~ day + (1|matriline_ID), data = dec_RUS_G3_Y)
summary(lm_G3_RUS_Y)
lm_G3_RUS_O <- lmer(num_neonates ~ day + (1|matriline_ID), data = dec_RUS_G3_O)
summary(lm_G3_RUS_O)

# Statistics (ANCOVA):

m.interaction <- lmer(num_neonates ~ day * line + (1|matriline_ID), data = decline_RUS_G3_Y_O)
anova(m.interaction)
summary(m.interaction)

#------------------------------------------------------------------

# Big multipanel plot - all response metrics that are plotted over time----
# Figures 2 and 3

pdf("Q1_overtime_L5.pdf", width=12, height=8)

Q1_overtime_L5 <- ggarrange(surv_curves_L5, RO_L5_time, decline_curves_L5, 
                                ncol = 3, nrow = 1,
                            labels = c("A","B","C"),
                            font.label = list(size = 20))

Q1_overtime_L5 <- annotate_figure(Q1_overtime_L5,
                                     bottom = textGrob("Age (d)", 
                                    gp = gpar(fontsize = 22)))
Q1_overtime_L5

dev.off()

pdf("Q1_overtime_RUS.pdf", width=12, height=8)

Q1_overtime_RUS <- ggarrange(surv_curves_RUS, RO_RUS_time, decline_curves_RUS, 
                            ncol = 3, nrow = 1,
                            labels = c("A","B","C"),
                            font.label = list(size = 20))

Q1_overtime_RUS <- annotate_figure(Q1_overtime_RUS,
                                  bottom = textGrob("Age (d)", 
                                                    gp = gpar(fontsize = 22)))
Q1_overtime_RUS

dev.off()

#------------------------------------------------------------------
# Statistical analyses for LRO - GLM -------------------------

# Removing F0 generation and switched age cohorts (not relevant to Question 1):
# For separate strain, per individual datasets (needed for plotting later)

LRO_per_ind_RUS_G1_3 <- LRO_per_ind_RUS %>%
  filter(generation != "F0") %>%
  filter(switched_age != "Y")

LRO_per_ind_L5_G1_3 <- LRO_per_ind_L5 %>%
  filter(generation != "F0") %>%
  filter(switched_age != "Y")

# Removing F0 generation and switched age cohorts (combined strain dataset):

LRO_combo_G1_3 <- LRO_combo %>%
  filter(generation != "F0") %>%
  filter(switched_age != "Y")

# generalized mixed model
# Using Poisson for count data

LRO_glmer <- glmer(LRO ~ strain * line * generation + (1|matriline_ID), family=poisson, data=LRO_combo_G1_3)

check_overdispersion(LRO_glmer)
# overdispersed

# Negative binomial glmm

LRO_glmer_nb<- glmer.nb(LRO ~ strain * line * generation + (1|matriline_ID), data=LRO_combo_G1_3)

LRO_summary <- summary(LRO_glmer_nb)
LRO_summary

LRO_table <- LRO_summary[[10]]
LRO_table <- as.data.frame(LRO_table)

# exporting model summary for supplement
write.csv(LRO_table, "Q1_LRO.csv")

# likelihood ratio test

drop1(LRO_glmer_nb, test = "Chisq") # tests for 3 way interaction

# model with all two-way interactions included
LRO_glmer_twoway <- glmer.nb(LRO ~ strain + line + generation
                           + strain*generation + strain*line + line*generation
                           + (1|matriline_ID), data=LRO_combo_G1_3)

LRO_glmer_SG <- glmer.nb(LRO ~ strain + line + generation
                       + strain*line + line*generation
                       + (1|matriline_ID), data=LRO_combo_G1_3)

LRO_glmer_SL <- glmer.nb(LRO ~ strain + line + generation
                       + strain*generation + line*generation
                       + (1|matriline_ID), data=LRO_combo_G1_3)

LRO_glmer_LG <- glmer.nb(LRO ~ strain + line + generation
                       + strain*line + strain*generation
                       + (1|matriline_ID), data=LRO_combo_G1_3)

# just main effects
LRO_glmer_main <- glmer.nb(LRO ~ strain + line + generation
                         + (1|matriline_ID), data=LRO_combo_G1_3)

# testing for SG interaction
anova(LRO_glmer_twoway, LRO_glmer_SG)

# testing for SL interaction
anova(LRO_glmer_twoway, LRO_glmer_SL) # significant

# testing for LG interaction
anova(LRO_glmer_twoway, LRO_glmer_LG)

plot(LRO_glmer_nb)

# Post hoc comparisons

EMM_LRO <- emmeans(LRO_glmer_nb, pairwise ~ line | strain)
EMM_LRO

# L5: LRO Figures----------

LRO_L5_G1_3 <- LRO_summary_L5 %>%
  filter(generation != "F0") %>%
  filter(switched_age != "Y")

LRO_L5_G1_3$final_line <- factor(LRO_L5_G1_3$final_line, 
                                 levels = c("F1 young_mother N", "F1 old_mother N",
                                            "F2 young_mother N", "F2 old_mother N",
                                            "F3 young_mother N", "F3 old_mother N"), 
                                 labels = c("Y","O","YY","OO","YYY","OOO"))

LRO_L5_G1_3$generation <- factor(LRO_L5_G1_3$generation,
                                 levels = c("F1","F2","F3"),
                                 labels = c("G1","G2","G3"))

LRO_per_ind_L5_G1_3$final_line <- factor(LRO_per_ind_L5_G1_3$final_line, 
                                         levels = c("F1 young_mother N", "F1 old_mother N",
                                                    "F2 young_mother N", "F2 old_mother N",
                                                    "F3 young_mother N", "F3 old_mother N"), 
                                         labels = c("Y","O","YY","OO","YYY","OOO"))

LRO_per_ind_L5_G1_3$generation <- factor(LRO_per_ind_L5_G1_3$generation,
                                         levels = c("F1","F2","F3"),
                                         labels = c("G1","G2","G3"))

ann_text <- data.frame(final_line = "Y", mean = 0,lab = "BmanL5",
                       generation = factor("G1",levels = c("G1","G2","G3")))
pairwise_L5 <- c("d","c","d","c","d","c") # from pairwise analyses
# Plotting means and SE
LRO_raw <- ggplot(data = LRO_L5_G1_3, mapping = aes(x = final_line, y = mean))+
  geom_point(size = 3) +
  geom_errorbar(data = LRO_L5_G1_3, 
                aes(x = final_line, ymin=mean-SE, ymax=mean+SE), width=0.1) +
  #facet_grid(~generation) +
  geom_text(label=pairwise_L5, 
            nudge_x = 0.3,
            size = 8) +
  geom_text(data = ann_text,label = "BmanL5",
            size = 8)

# Plotting the raw data points
LRO_combo <- LRO_raw +
  geom_jitter(data = LRO_per_ind_L5_G1_3, aes(x = final_line, y = LRO),
              width=.2, alpha=0.2, size = 2)

LRO_L5_plot <- LRO_combo + 
  ylab("") +
  xlab(element_blank()) +
  ylim(c(-0.5,37)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        strip.text=element_text(size=20),
        axis.text=element_text(size=20))
LRO_L5_plot

# RUS: LRO Figures-------------------

LRO_RUS_G1_3 <- LRO_summary_RUS %>%
  filter(generation != "F0") %>%
  filter(switched_age != "Y")

LRO_RUS_G1_3$final_line <- factor(LRO_RUS_G1_3$final_line, 
                                  levels = c("F1 young_mother N", "F1 old_mother N",
                                             "F2 young_mother N", "F2 old_mother N",
                                             "F3 young_mother N", "F3 old_mother N"), 
                                  labels = c("Y","O","YY","OO","YYY","OOO"))

LRO_RUS_G1_3$generation <- factor(LRO_RUS_G1_3$generation,
                                  levels = c("F1","F2","F3"),
                                  labels = c("G1","G2","G3"))

LRO_per_ind_RUS_G1_3$final_line <- factor(LRO_per_ind_RUS_G1_3$final_line, 
                                          levels = c("F1 young_mother N", "F1 old_mother N",
                                                     "F2 young_mother N", "F2 old_mother N",
                                                     "F3 young_mother N", "F3 old_mother N"), 
                                          labels = c("Y","O","YY","OO","YYY","OOO"))

LRO_per_ind_RUS_G1_3$generation <- factor(LRO_per_ind_RUS_G1_3$generation,
                                          levels = c("F1","F2","F3"),
                                          labels = c("G1","G2","G3"))

pairwise_RUS <- c("b","a","b","a","b","a") # from pairwise analyses

ann_text <- data.frame(final_line = "Y", mean = 0,lab = "BmanRUS",
                       generation = factor("G1",levels = c("G1","G2","G3")))
# Plotting means and SE
LRO_raw <- ggplot(data = LRO_RUS_G1_3, mapping = aes(x = final_line, y = mean))+
  geom_point(size = 3) +
  geom_errorbar(data = LRO_RUS_G1_3, 
                aes(x = final_line, ymin=mean-SE, ymax=mean+SE), width=0.1) +
  #facet_grid(~generation) +
  geom_text(label=pairwise_RUS, 
            nudge_x = 0.3,
            size = 8) +
  geom_text(data = ann_text,label = "BmanRUS",
            size = 8,
            nudge_x = 0.1)

# Plotting the raw data points
LRO_combo <- LRO_raw +
  geom_jitter(data = LRO_per_ind_RUS_G1_3, aes(x = final_line, y = LRO),
              width=.2, alpha=0.2, size = 2)

LRO_RUS_plot <- LRO_combo + 
  ylab("") +
  xlab(element_blank()) +
  ylim(c(-0.5,37)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        strip.text=element_text(size=20),
        axis.text=element_text(size=20))
LRO_RUS_plot

# LRO G1 - G3 Multipanel plot:------------------
# Figure 4

pdf("LRO.pdf", width=10, height=11)

LRO <- ggarrange(LRO_RUS_plot, LRO_L5_plot,
                 ncol = 1, nrow = 2,
                 labels = c("A","B"),
                 font.label = list(size = 24))

LRO <- annotate_figure(LRO, left = textGrob(expression(paste("Lifetime reproductive output (offspring ind"^"-1"*")")), 
                                            rot = 90, gp = gpar(fontsize = 24)), 
                       bottom = textGrob("Maternal age cohort", gp = gpar(fontsize = 24)))

LRO
dev.off()

#------------------------------------------------------------------
# Maximum daily reproduction (MDR) analyses--------------------------------------------------

# generalized mixed model
# Using Poisson for count data

MDR_glmer <- glmer(max_RO ~ strain * line * generation + (1|matriline_ID), family=poisson, data=LRO_combo_G1_3)

check_overdispersion(MDR_glmer)

MDR_summary <- summary(MDR_glmer)
MDR_summary

MDR_table <- MDR_summary[[10]]
MDR_table <- as.data.frame(MDR_table)

# exporting model summary for supplement
write.csv(MDR_table, "Q1_MDR.csv")

# likelihood ratio test

drop1(MDR_glmer, test = "Chisq") # tests for 3 way interaction

# model with all two-way interactions included
MDR_glmer_twoway <- glmer(max_RO ~ strain + line + generation
                          + strain*generation + strain*line + line*generation
                          + (1|matriline_ID), family=poisson, data=LRO_combo_G1_3)

MDR_glmer_SG <- glmer(max_RO ~ strain + line + generation
                      + strain*line + line*generation
                      + (1|matriline_ID), family=poisson, data=LRO_combo_G1_3)

MDR_glmer_SL <- glmer(max_RO ~ strain + line + generation
                      + strain*generation + line*generation
                      + (1|matriline_ID), family=poisson, data=LRO_combo_G1_3)

MDR_glmer_LG <- glmer(max_RO ~ strain + line + generation
                      + strain*line + strain*generation
                      + (1|matriline_ID), family=poisson, data=LRO_combo_G1_3)

# just main effects
MDR_glmer_main <- glmer(max_RO ~ strain + line + generation
                        + (1|matriline_ID), family=poisson, data=LRO_combo_G1_3)

MDR_glmer_strain <- glmer(max_RO ~ line + generation
                          + (1|matriline_ID), family=poisson, data=LRO_combo_G1_3)
MDR_glmer_line <- glmer(max_RO ~ strain + generation
                        + (1|matriline_ID), family=poisson, data=LRO_combo_G1_3)
MDR_glmer_generation <- glmer(max_RO ~ strain + line
                              + (1|matriline_ID), family=poisson, data=LRO_combo_G1_3)

# testing for SG interaction
anova(MDR_glmer_twoway, MDR_glmer_SG)

# testing for SL interaction
anova(MDR_glmer_twoway, MDR_glmer_SL)

# testing for LG interaction
anova(MDR_glmer_twoway, MDR_glmer_LG)

# testing for main effect of strain
anova(MDR_glmer_main, MDR_glmer_strain) # significant

# testing for main effect of line
anova(MDR_glmer_main, MDR_glmer_line) # significant

# testing for main effect of generation
anova(MDR_glmer_main, MDR_glmer_generation)

# significant main effects of strain and line
# no need for post-hoc tests - these factors have 2 levels

#------------------------------------------------------------------
# Calculating and analyzing timing of the production of 25% of LRO-----------------------------

# Calculating cumulative reproduction PER INDIVIDUAL:
# Then getting the day when 25% of lifetime fecundity was reached:

neo_long_nc_L5 <- neo_long_nc_L5[with(neo_long_nc_L5, order(generation, line, switched_age, plate, well)),]

neo_long_nc_L5 <- neo_long_nc_L5 %>%
  group_by(generation, line, switched_age, plate, well) %>%
  mutate(cum_RO = cumsum(num_neonates))

# Getting a per day average...

cumulative_average_L5 <- neo_long_nc_L5 %>%
  group_by(generation, line, switched_age, day) %>%
  summarize(mean_cum = mean(cum_RO, na.rm=TRUE))

# Removing individuals with a LRO of zero (not relevant here)
LRO_per_ind_L5 <- filter(LRO_per_ind_L5, LRO != 0)

neo_merge_L5 <- merge(LRO_per_ind_L5, neo_long_nc_L5, by = c("generation","line","switched_age","plate","well"))

sub_L5 <- subset(neo_merge_L5, cum_RO >= percent_LRO , select = c(generation,line,switched_age,plate,well,LRO,percent_LRO,day,cum_RO))

sub_L5$day <- as.numeric(sub_L5$day)

# Contains the time of 25% LRO production (d) for each individual
sub_summ_L5 <- sub_L5 %>%
  group_by(generation, line, switched_age, plate, well) %>%
  summarize(time_25 = min(day))

# Summarizing time of 25% LRO:
LRO_25_summary_L5 <- sub_summ_L5 %>%
  group_by(generation, line, switched_age) %>%
  summarize(
    mean=mean(time_25, na.rm = TRUE),
    SD=sd(time_25, na.rm =TRUE),
    med=median(time_25, na.rm = TRUE),
    SE=sd(time_25, na.rm =TRUE)/sqrt(length(!is.na(time_25))))
LRO_25_summary_L5

# RUS
neo_long_nc_RUS <- neo_long_nc_RUS[with(neo_long_nc_RUS, order(generation, line, switched_age, plate, well)),]

neo_long_nc_RUS <- neo_long_nc_RUS %>%
  group_by(generation, line, switched_age, plate, well) %>%
  mutate(cum_RO = cumsum(num_neonates))

# Getting a per day average...

cumulative_average_RUS <- neo_long_nc_RUS %>%
  group_by(generation, line, switched_age, day) %>%
  summarize(mean_cum = mean(cum_RO, na.rm=TRUE))

# Removing individuals with a LRO of zero (not relevant here)
LRO_per_ind_RUS <- filter(LRO_per_ind_RUS, LRO != 0)

neo_merge_RUS <- merge(LRO_per_ind_RUS, neo_long_nc_RUS, by = c("generation","line","switched_age","plate","well"))

sub_RUS <- subset(neo_merge_RUS, cum_RO >= percent_LRO , select = c(generation,line,switched_age,plate,well,LRO,percent_LRO,day,cum_RO))

sub_RUS$day <- as.numeric(sub_RUS$day)

# Contains the time of 25% LRO production (d) for each individual
sub_summ_RUS <- sub_RUS %>%
  group_by(generation, line, switched_age, plate, well) %>%
  summarize(time_25 = min(day))

# Summarizing time of 25% LRO:
LRO_25_summary_RUS <- sub_summ_RUS %>%
  group_by(generation, line, switched_age) %>%
  summarize(
    mean=mean(time_25, na.rm = TRUE), 
    SD=sd(time_25, na.rm =TRUE),
    med=median(time_25, na.rm = TRUE),
    SE=sd(time_25, na.rm =TRUE)/sqrt(length(!is.na(time_25))))
LRO_25_summary_RUS

# Need to merge time of 25% LRO data and neonate origin data (so that we can include mom identity in the models below)

LRO_25_origin_L5 <- merge(sub_summ_L5, origins_L5, 
                        by = c("generation", "line", "switched_age", "plate", "well"))

LRO_25_origin_RUS <- merge(sub_summ_RUS, origins_RUS, 
                         by = c("generation", "line", "switched_age", "plate", "well"))

# Combining strains:

sub_summ_combo <- rbind(LRO_25_origin_L5, LRO_25_origin_RUS)

# LRO_25 Statistics:------------------------------------

# Removing F0 generation and switched age cohorts:

LRO25_G1_3 <- sub_summ_combo %>%
  filter(generation != "F0") %>%
  filter(switched_age != "Y")

# generalized mixed model
# Using Poisson for count data

LRO25_glmer <- glmer(time_25 ~ strain * line * generation + (1|matriline_ID), family=poisson, data=LRO25_G1_3)

check_overdispersion(LRO25_glmer)

LRO25_summary <- summary(LRO25_glmer)
LRO25_summary

LRO25_table <- LRO25_summary[[10]]
LRO25_table <- as.data.frame(LRO25_table)

# exporting model summary for supplement
write.csv(LRO25_table, "Q1_25LRO.csv")

# likelihood ratio test
drop1(LRO25_glmer, test = "Chisq") # tests for 3 way interaction

# model with all two-way interactions included
LRO25_glmer_twoway <- glmer(time_25 ~ strain + line + generation
                          + strain*generation + strain*line + line*generation
                          + (1|matriline_ID), family=poisson, data=LRO25_G1_3)

LRO25_glmer_SG <- glmer(time_25 ~ strain + line + generation
                      + strain*line + line*generation
                      + (1|matriline_ID), family=poisson, data=LRO25_G1_3)

LRO25_glmer_SL <- glmer(time_25 ~ strain + line + generation
                      + strain*generation + line*generation
                      + (1|matriline_ID), family=poisson, data=LRO25_G1_3)

LRO25_glmer_LG <- glmer(time_25 ~ strain + line + generation
                      + strain*line + strain*generation
                      + (1|matriline_ID), family=poisson, data=LRO25_G1_3)

# just main effects
LRO25_glmer_main <- glmer(time_25 ~ strain + line + generation
                        + (1|matriline_ID), family=poisson, data=LRO25_G1_3)

# testing for SG interaction
anova(LRO25_glmer_twoway, LRO25_glmer_SG)

# testing for SL interaction
anova(LRO25_glmer_twoway, LRO25_glmer_SL) # significant

# testing for LG interaction
anova(LRO25_glmer_twoway, LRO25_glmer_LG)

plot(LRO25_glmer)

# Post hoc comparisons

EMM_25 <- emmeans(LRO25_glmer, pairwise ~ line | strain)
EMM_25

#------------------------------------------------------------------
# Loading and reformatting reproductive period data--------------------------------------------------------------------

rep_data_L5 <- read_csv("rep_period_L5.csv")
rep_data_RUS <- read_csv("rep_period_RUS.csv")

# Changing the order of the factors
rep_data_L5 <- rep_data_L5[with(rep_data_L5, order(generation, line, switched_age)),]
rep_data_RUS <- rep_data_RUS[with(rep_data_RUS, order(generation, line, switched_age)),]

# Removing censored individuals (we did not measure their full reproductive period)
rep_data_no_censor_L5 <- filter(rep_data_L5, censor != 0)
rep_data_no_censor_RUS <- filter(rep_data_RUS, censor != 0)

# Modifying the dataset from wide to long format (needed for plotting and analysis):
rep_long_nc_L5 <- melt(data = rep_data_no_censor_L5,
                       id.vars = c("generation", "line", "switched_age", "plate", "well"),
                       variable.name = "day",
                       value.name = "num_rep_days")
rep_long_nc_L5 <- filter(rep_long_nc_L5, day != "censor")

rep_long_nc_RUS <- melt(data = rep_data_no_censor_RUS,
                        id.vars = c("generation", "line", "switched_age", "plate", "well"),
                        variable.name = "day",
                        value.name = "num_rep_days")
rep_long_nc_RUS <- filter(rep_long_nc_RUS, day != "censor")

# Calculating the reproductive period in days and as a percent of the lifespan, per individual

rep_per_ind_L5 <- rep_long_nc_L5 %>%
  group_by(generation, line, switched_age, plate, well) %>%
  summarize(rep_time = sum(num_rep_days, na.rm = TRUE),
            length = length(num_rep_days[!is.na(num_rep_days)]),
            percent_rep = (rep_time/length)*100,
            prop_rep = (rep_time/length))

rep_per_ind_RUS <- rep_long_nc_RUS %>%
  group_by(generation, line, switched_age, plate, well) %>%
  summarize(rep_time = sum(num_rep_days, na.rm = TRUE),
            length = length(num_rep_days[!is.na(num_rep_days)]),
            percent_rep = (rep_time/length)*100,
            prop_rep = (rep_time/length))

# Need to merge reproductive period data and neonate origin data (so that we can include mom identity in the models below)

rep_origin_L5 <- merge(rep_per_ind_L5, origins_L5, 
                        by = c("generation", "line", "switched_age", "plate", "well"))

rep_origin_RUS <- merge(rep_per_ind_RUS, origins_RUS, 
                         by = c("generation", "line", "switched_age", "plate", "well"))

# Combining strains:

rep_per_ind_combo <- rbind(rep_origin_L5, rep_origin_RUS)

# Calculating summary statistics across individuals- L5
rep_summary_L5 <- rep_per_ind_L5 %>%
  group_by(generation, line, switched_age) %>%
  summarize(
    mean_perc=mean(percent_rep, na.rm = TRUE), 
    SD_perc=sd(percent_rep, na.rm =TRUE),
    med_perc=median(percent_rep, na.rm = TRUE),
    SE_perc=sd(percent_rep, na.rm =TRUE)/sqrt(length(!is.na(percent_rep))),
    mean_days=mean(rep_time, na.rm=TRUE),
    SE_days=sd(rep_time, na.rm =TRUE)/sqrt(length(!is.na(rep_time))),
    med_days=median(rep_time, na.rm=TRUE),
    n = length(percent_rep))
rep_summary_L5

rep_per_ind_L5$final_line <- paste(rep_per_ind_L5$generation,
                                   rep_per_ind_L5$line,
                                   rep_per_ind_L5$switched_age)

rep_summary_L5$final_line <- paste(rep_summary_L5$generation,
                                   rep_summary_L5$line,
                                   rep_summary_L5$switched_age)

# Calculating summary statistics across individuals- RUS

rep_summary_RUS <- rep_per_ind_RUS %>%
  group_by(generation, line, switched_age) %>%
  summarize(
    mean_perc=mean(percent_rep, na.rm = TRUE),
    SD_perc=sd(percent_rep, na.rm =TRUE),
    med_perc=median(percent_rep, na.rm = TRUE),
    SE_perc=sd(percent_rep, na.rm =TRUE)/sqrt(length(!is.na(percent_rep))),
    mean_days=mean(rep_time, na.rm=TRUE),
    SE_days=sd(rep_time, na.rm =TRUE)/sqrt(length(!is.na(rep_time))),
    med_days=median(rep_time, na.rm=TRUE),
    n = length(percent_rep))
rep_summary_RUS

rep_per_ind_RUS$final_line <- paste(rep_per_ind_RUS$generation,
                                    rep_per_ind_RUS$line,
                                    rep_per_ind_RUS$switched_age)

rep_summary_RUS$final_line <- paste(rep_summary_RUS$generation,
                                    rep_summary_RUS$line,
                                    rep_summary_RUS$switched_age)

# Reproductive period analyses ------------------------
# Removing F0 generation and switched age cohorts:

rep_G1_3 <- rep_per_ind_combo %>%
  filter(generation != "F0") %>%
  filter(switched_age != "Y")

# generalized mixed model
# Using binomial for proportion data

bin_glmer <- glmer(prop_rep ~ strain * line * generation + (1|matriline_ID), family=binomial, data=rep_G1_3, weights = length)

check_overdispersion(bin_glmer)

bin_summary <- summary(bin_glmer)
bin_summary

bin_table <- bin_summary[[10]]
bin_table <- as.data.frame(bin_table)

# exporting model summary for supplement
write.csv(bin_table, "Q1_rep.csv")

# Model validation (plotting residuals)
plot(bin_glmer)

# likelihood ratio test

drop1(bin_glmer, test = "Chisq") # tests for 3 way interaction

# post hoc comparisons

pairwise_rep <- emmeans(bin_glmer, pairwise ~ line | generation | strain)
pairwise_rep

#------------------------------------------------------------------