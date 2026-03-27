
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

# Lifespan analysis - formatting data---------------------------------------------------------------------------------

# L5
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

# Calculating summary statistics
surv_summary_L5 <- surv_data_no_censor_L5 %>%
  group_by(generation, line, switched_age) %>%
  summarize(
    mean=mean(surv_time, na.rm = TRUE), 
    median=median(surv_time, na.rm = TRUE),
    SD=sd(surv_time, na.rm =TRUE),
    SE=sd(surv_time, na.rm =TRUE)/sqrt(length(!is.na(surv_time))),
    variance=var(surv_time, na.rm = TRUE),
    n=length(surv_time))
surv_summary_L5

surv_summary_L5$final_line <- paste(surv_summary_L5$generation,
                                    surv_summary_L5$line,
                                    surv_summary_L5$switched_age)

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

# Calculating summary statistics
surv_summary_RUS <- surv_data_no_censor_RUS %>%
  group_by(generation, line, switched_age) %>%
  summarize(
    mean=mean(surv_time, na.rm = TRUE), 
    median=median(surv_time, na.rm = TRUE),
    SD=sd(surv_time, na.rm =TRUE),
    SE=sd(surv_time, na.rm =TRUE)/sqrt(length(!is.na(surv_time))),
    variance=var(surv_time, na.rm = TRUE),
    n=length(surv_time))
surv_summary_RUS

surv_summary_RUS$final_line <- paste(surv_summary_RUS$generation,
                                     surv_summary_RUS$line,
                                     surv_summary_RUS$switched_age)

# Need to merge survivorship data and neonate origin data (so that we can include mom identity in the models below)

surv_origin_L5 <- merge(surv_data_no_censor_L5, origins_L5, 
                        by = c("generation", "line", "switched_age", "plate", "well"))

surv_origin_RUS <- merge(surv_data_no_censor_RUS, origins_RUS, 
                         by = c("generation", "line", "switched_age", "plate", "well"))

# Combining the strain datasets

surv_data_no_censor_combo <- rbind(surv_origin_L5, surv_origin_RUS)


# GLMM on lifespan (count data) ------------------------------

# Need strain data separated for plots, combined for analysis
surv_data_no_censor_L5_G3 <- filter(surv_data_no_censor_L5, generation == "F3")
surv_data_no_censor_RUS_G3 <- filter(surv_data_no_censor_RUS, generation == "F3")
surv_data_no_censor_G3 <- filter(surv_data_no_censor_combo, generation == "F3")

# Adding grandmaternal age and maternal age columns:

surv_data_no_censor_G3 <- surv_data_no_censor_G3 %>%
  mutate(grandmaternal_age = case_when(
    endsWith(line, "old_mother") ~ "O",
    endsWith(line, "young_mother") ~ "Y"
  ))

surv_data_no_censor_G3 <- surv_data_no_censor_G3 %>%
  mutate(maternal_age = case_when(
    endsWith(final_line, "F3 old_mother N") ~ "O",
    endsWith(final_line, "F3 old_mother Y") ~ "Y",
    endsWith(final_line, "F3 young_mother N") ~ "Y",
    endsWith(final_line, "F3 young_mother Y") ~ "O",
  ))

surv_data_no_censor_L5_G3 <- surv_data_no_censor_L5_G3 %>%
  mutate(grandmaternal_age = case_when(
    endsWith(line, "old_mother") ~ "O",
    endsWith(line, "young_mother") ~ "Y"
  ))

surv_data_no_censor_L5_G3 <- surv_data_no_censor_L5_G3 %>%
  mutate(maternal_age = case_when(
    endsWith(final_line, "F3 old_mother N") ~ "O",
    endsWith(final_line, "F3 old_mother Y") ~ "Y",
    endsWith(final_line, "F3 young_mother N") ~ "Y",
    endsWith(final_line, "F3 young_mother Y") ~ "O",
  ))

surv_data_no_censor_RUS_G3 <- surv_data_no_censor_RUS_G3 %>%
  mutate(grandmaternal_age = case_when(
    endsWith(line, "old_mother") ~ "O",
    endsWith(line, "young_mother") ~ "Y"
  ))

surv_data_no_censor_RUS_G3 <- surv_data_no_censor_RUS_G3 %>%
  mutate(maternal_age = case_when(
    endsWith(final_line, "F3 old_mother N") ~ "O",
    endsWith(final_line, "F3 old_mother Y") ~ "Y",
    endsWith(final_line, "F3 young_mother N") ~ "Y",
    endsWith(final_line, "F3 young_mother Y") ~ "O",
  ))

# generalized mixed model
# Using Poisson for count data

surv_glmer <- glmer(surv_time ~ strain * maternal_age * grandmaternal_age + (1|matriline_ID), family=poisson, data=surv_data_no_censor_G3)

# in 'performance' package
check_overdispersion(surv_glmer)

surv_summary <- summary(surv_glmer)
surv_summary

surv_table <- surv_summary[[10]]
surv_table <- as.data.frame(surv_table)

# exporting model summary for supplement
write.csv(surv_table, "Q2_surv.csv")

# Model validation (plotting residuals)
plot(surv_glmer)

# likelihood ratio test

drop1(surv_glmer, test = "Chisq") # tests for 3 way interaction

# post hoc comparisons

pairwise_survival <- emmeans(surv_glmer, pairwise ~ grandmaternal_age * maternal_age | strain)
pairwise_survival

# Plotting survivorship curves - L5-----------------------

# OOO vs. YYO

surv_L5_OOO_YYO <- filter(surv_data_no_censor_L5_G3, maternal_age == "O")

fit_L5_gen3_OOO_YYO <- survfit(Surv(surv_time, censor) ~ final_line, data = surv_L5_OOO_YYO)

print(fit_L5_gen3_OOO_YYO)
summary(fit_L5_gen3_OOO_YYO)

surv_L5_gen3_OOO_YYO_plot <- ggsurvplot(
  fit_L5_gen3_OOO_YYO,
  data = surv_L5_OOO_YYO,
  linetype = c("solid","solid"),
  palette = c("#0072B2","#D55E00"),
  size = 0.75,
  conf.int = FALSE,        
  pval = FALSE, 
  censor = FALSE,
  xlab = element_blank(),
  xlim = c(0,30),
  break.x.by = 5,
  ylab = element_blank(),
  font.y = c(20),
  font.tickslab = c(14),
  legend = "none")

surv_L5_gen3_OOO_YYO_plot$plot <- surv_L5_gen3_OOO_YYO_plot$plot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                         panel.background = element_blank(), axis.line = element_line(colour = "black"))

surv_L5_gen3_OOO_YYO_plot

# OOY vs. YYY

surv_L5_OOY_YYY <- filter(surv_data_no_censor_L5_G3, maternal_age == "Y")

fit_L5_gen3_OOY_YYY <- survfit(Surv(surv_time, censor) ~ final_line, data = surv_L5_OOY_YYY)

print(fit_L5_gen3_OOY_YYY)
summary(fit_L5_gen3_OOY_YYY)

surv_L5_gen3_OOY_YYY_plot <- ggsurvplot(
  fit_L5_gen3_OOY_YYY,
  data = surv_L5_OOY_YYY,
  linetype = c("solid","solid"),
  palette = c("#009E73","black"),
  size = 0.75,
  conf.int = FALSE,        
  pval = FALSE, 
  censor = FALSE,
  xlab = element_blank(),
  xlim = c(0,30),
  break.x.by = 5,
  ylab = element_blank(),
  font.y = c(20),
  font.tickslab = c(14),
  legend = "none")

surv_L5_gen3_OOY_YYY_plot$plot <- surv_L5_gen3_OOY_YYY_plot$plot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                         panel.background = element_blank(), axis.line = element_line(colour = "black"))
surv_L5_gen3_OOY_YYY_plot

# Multipanel plot

surv_curves_L5 <- ggarrange(surv_L5_gen3_OOO_YYO_plot$plot, surv_L5_gen3_OOY_YYY_plot$plot,
                            ncol = 1, nrow = 2,
                            heights = c(1, 1))

surv_curves_L5 <- annotate_figure(surv_curves_L5, left = textGrob("Survival probability", 
                                                                  rot = 90, gp = gpar(fontsize = 20)))
surv_curves_L5

# Plotting survivorship curves - RUS-----------------------

# OOO vs. YYO

surv_RUS_OOO_YYO <- filter(surv_data_no_censor_RUS_G3, maternal_age == "O")

fit_RUS_gen3_OOO_YYO <- survfit(Surv(surv_time, censor) ~ final_line, data = surv_RUS_OOO_YYO)

print(fit_RUS_gen3_OOO_YYO)
summary(fit_RUS_gen3_OOO_YYO)

surv_RUS_gen3_OOO_YYO_plot <- ggsurvplot(
  fit_RUS_gen3_OOO_YYO,
  data = surv_RUS_OOO_YYO,
  linetype = c("solid","solid"),
  palette = c("#0072B2","#D55E00"),
  size = 0.75,
  conf.int = FALSE,        
  pval = FALSE, 
  censor = FALSE,
  xlab = element_blank(),
  xlim = c(0,30),
  break.x.by = 5,
  ylab = element_blank(),
  font.y = c(20),
  font.tickslab = c(14),
  legend = "none")

surv_RUS_gen3_OOO_YYO_plot$plot <- surv_RUS_gen3_OOO_YYO_plot$plot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                         panel.background = element_blank(), axis.line = element_line(colour = "black"))

surv_RUS_gen3_OOO_YYO_plot

# OOY vs. YYY

surv_RUS_OOY_YYY <- filter(surv_data_no_censor_RUS_G3, maternal_age == "Y")

fit_RUS_gen3_OOY_YYY <- survfit(Surv(surv_time, censor) ~ final_line, data = surv_RUS_OOY_YYY)

print(fit_RUS_gen3_OOY_YYY)
summary(fit_RUS_gen3_OOY_YYY)

surv_RUS_gen3_OOY_YYY_plot <- ggsurvplot(
  fit_RUS_gen3_OOY_YYY,
  data = surv_RUS_OOY_YYY,
  linetype = c("solid","solid"),
  palette = c("#009E73","black"),
  size = 0.75,
  conf.int = FALSE,        
  pval = FALSE, 
  censor = FALSE,
  xlab = element_blank(),
  xlim = c(0,30),
  break.x.by = 5,
  ylab = element_blank(),
  font.y = c(20),
  font.tickslab = c(14),
  legend = "none")

surv_RUS_gen3_OOY_YYY_plot$plot <- surv_RUS_gen3_OOY_YYY_plot$plot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                           panel.background = element_blank(), axis.line = element_line(colour = "black"))

surv_RUS_gen3_OOY_YYY_plot

# Multipanel plot

surv_curves_RUS <- ggarrange(surv_RUS_gen3_OOO_YYO_plot$plot, 
                             surv_RUS_gen3_OOY_YYY_plot$plot, 
                             ncol = 1, nrow = 2,
                             heights = c(1, 1))

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
    med=median(LRO, na.rm = TRUE),
    SD=sd(LRO, na.rm =TRUE),
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
    med=median(LRO, na.rm = TRUE),
    SD=sd(LRO, na.rm =TRUE),
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
# Daily reproduction over time - L5 strain---------------------------------------------

# Calculating the mean number of neonates produced across all individuals, per day
RO_per_day_L5 <- neo_long_nc_L5 %>%
  group_by(generation, line, switched_age, day) %>%
  summarize(total_offspring_per_day = sum(num_neonates, na.rm = TRUE),
            mean = mean(num_neonates, na.rm = TRUE),
            n = length(!is.na(num_neonates)),
            SE = sd(num_neonates, na.rm =   
                      TRUE)/sqrt(length(!is.na(num_neonates))))

RO_per_day_L5$final_line <- paste(RO_per_day_L5$generation,
                                  RO_per_day_L5$line,
                                  RO_per_day_L5$switched_age)
# Generation 3 plot - L5: -----------------------------

# OOO vs. YYO

RO_L5_G3_OOO_YYO <- RO_per_day_L5 %>%
  filter(generation == "F3") %>%
  filter(final_line != "F3 old_mother Y") %>%
  filter(final_line != "F3 young_mother N")
  
RO_L5_G3_OOO_YYO$final_line <- factor(RO_L5_G3_OOO_YYO$final_line, 
                              levels = c("F3 old_mother N", "F3 young_mother Y"))

RO_L5_G3_OOO_YYO$day <- as.numeric(RO_L5_G3_OOO_YYO$day)

RO_raw <- ggplot(data=RO_L5_G3_OOO_YYO, aes(x=day, y=mean)) + 
  geom_point(aes(group = final_line, color = final_line), size = 1.5) +
  geom_errorbar(aes(x=day, ymin=mean-SE, ymax=mean+SE), width=0.1)

RO_combo <- RO_raw+
  geom_line(data=RO_L5_G3_OOO_YYO, aes(x=day, y=mean, color = final_line, linetype = final_line, group=final_line), size = 1) +
  scale_color_manual(values = c("#0072B2","#D55E00"), labels = c("OOO","YYO")) +
  scale_linetype_manual(values = c("solid","solid"),labels = c("OOO","YYO"))

RO_L5_plot_time_G3_OOO_YYO <- RO_combo + 
  ylab(element_blank()) +
  ylim(c(0,5)) +
  xlab(element_blank()) +
  scale_x_continuous(breaks=seq(0,30,5)) +
  theme(panel.grid.major = element_blank(), 
        axis.text=element_text(size=14),
        axis.title=element_text(size=20),
        legend.position = "none")

RO_L5_plot_time_G3_OOO_YYO <- RO_L5_plot_time_G3_OOO_YYO + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                 panel.background = element_blank(), axis.line = element_line(colour = "black"))

RO_L5_plot_time_G3_OOO_YYO

# OOY vs. YYY

RO_L5_G3_OOY_YYY <- RO_per_day_L5 %>%
  filter(generation == "F3") %>%
  filter(final_line != "F3 old_mother N") %>%
  filter(final_line != "F3 young_mother Y")

RO_L5_G3_OOY_YYY$final_line <- factor(RO_L5_G3_OOY_YYY$final_line, 
                                      levels = c("F3 old_mother Y", "F3 young_mother N"))

RO_L5_G3_OOY_YYY$day <- as.numeric(RO_L5_G3_OOY_YYY$day)

RO_raw <- ggplot(data=RO_L5_G3_OOY_YYY, aes(x=day, y=mean)) + 
  geom_point(aes(group = final_line, color = final_line), size = 1.5) +
  geom_errorbar(aes(x=day, ymin=mean-SE, ymax=mean+SE), width=0.1)

RO_combo <- RO_raw+
  geom_line(data=RO_L5_G3_OOY_YYY, aes(x=day, y=mean, color = final_line, linetype = final_line, group=final_line), size = 1) +
  scale_color_manual(values = c("#009E73","black"), labels = c("OOY","YYY")) +
  scale_linetype_manual(values = c("solid","solid"),labels = c("OOY","YYY"))

RO_L5_plot_time_G3_OOY_YYY <- RO_combo + 
  ylab(element_blank()) +
  ylim(c(0,5)) +
  xlab(element_blank()) +
  scale_x_continuous(breaks=seq(0,30,5)) +
  theme(panel.grid.major = element_blank(), 
        axis.text=element_text(size=14),
        axis.title=element_text(size=20),
        legend.position = "none")

RO_L5_plot_time_G3_OOY_YYY <- RO_L5_plot_time_G3_OOY_YYY + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                 panel.background = element_blank(), axis.line = element_line(colour = "black"))

RO_L5_plot_time_G3_OOY_YYY

# Multipanel plot

RO_L5_time <- ggarrange(RO_L5_plot_time_G3_OOO_YYO,
                        RO_L5_plot_time_G3_OOY_YYY, 
                        ncol = 1, nrow = 2)

RO_L5_time <- annotate_figure(RO_L5_time,
                              left = textGrob(expression(paste("Offspring ind"^"-1"*"day"^"-1")), 
                                              rot = 90, gp = gpar(fontsize = 20)))

RO_L5_time

# Daily reproduction over time - RUS strain---------------------------------------------

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
# Generation 3 plot - RUS: -----------------------------

# OOO vs. YYO

RO_RUS_G3_OOO_YYO <- RO_per_day_RUS %>%
  filter(generation == "F3") %>%
  filter(final_line != "F3 old_mother Y") %>%
  filter(final_line != "F3 young_mother N")

RO_RUS_G3_OOO_YYO$final_line <- factor(RO_RUS_G3_OOO_YYO$final_line, 
                              levels = c("F3 old_mother N", "F3 young_mother Y"))

RO_RUS_G3_OOO_YYO$day <- as.numeric(RO_RUS_G3_OOO_YYO$day)

RO_raw <- ggplot(data=RO_RUS_G3_OOO_YYO, aes(x=day, y=mean)) + 
  geom_point(aes(group = final_line, color = final_line), size = 1.5) +
  geom_errorbar(aes(x=day, ymin=mean-SE, ymax=mean+SE), width=0.1)

RO_combo <- RO_raw+
  geom_line(data=RO_RUS_G3_OOO_YYO, aes(x=day, y=mean, color = final_line, linetype = final_line, group=final_line), size = 1) +
  scale_color_manual(values = c("#0072B2","#D55E00"), labels = c("OOO","YYO")) +
  scale_linetype_manual(values = c("solid","solid"),labels = c("OOO","YYO"))

RO_RUS_plot_time_G3_OOO_YYO <- RO_combo + 
  ylab(element_blank()) +
  ylim(c(0,5)) +
  xlab(element_blank()) +
  scale_x_continuous(breaks=seq(0,30,5)) +
  theme(panel.grid.major = element_blank(), 
        axis.text=element_text(size=14),
        axis.title=element_text(size=20),
        legend.position = "none")

RO_RUS_plot_time_G3_OOO_YYO <- RO_RUS_plot_time_G3_OOO_YYO + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                 panel.background = element_blank(), axis.line = element_line(colour = "black"))

RO_RUS_plot_time_G3_OOO_YYO

# OOY vs. YYY

RO_RUS_G3_OOY_YYY <- RO_per_day_RUS %>%
  filter(generation == "F3") %>%
  filter(final_line != "F3 old_mother N") %>%
  filter(final_line != "F3 young_mother Y")

RO_RUS_G3_OOY_YYY$final_line <- factor(RO_RUS_G3_OOY_YYY$final_line, 
                                       levels = c("F3 old_mother Y", "F3 young_mother N"))

RO_RUS_G3_OOY_YYY$day <- as.numeric(RO_RUS_G3_OOY_YYY$day)

RO_raw <- ggplot(data=RO_RUS_G3_OOY_YYY, aes(x=day, y=mean)) + 
  geom_point(aes(group = final_line, color = final_line), size = 1.5) +
  geom_errorbar(aes(x=day, ymin=mean-SE, ymax=mean+SE), width=0.1)

RO_combo <- RO_raw+
  geom_line(data=RO_RUS_G3_OOY_YYY, aes(x=day, y=mean, color = final_line, linetype = final_line, group=final_line), size = 1) +
  scale_color_manual(values = c("#009E73","black"), labels = c("OOY","YYY")) +
  scale_linetype_manual(values = c("solid","solid"),labels = c("OOY","YYY"))

RO_RUS_plot_time_G3_OOY_YYY <- RO_combo + 
  ylab(element_blank()) +
  ylim(c(0,5)) +
  xlab(element_blank()) +
  scale_x_continuous(breaks=seq(0,30,5)) +
  theme(panel.grid.major = element_blank(), 
        axis.text=element_text(size=14),
        axis.title=element_text(size=20),
        legend.position = "none")

RO_RUS_plot_time_G3_OOY_YYY <- RO_RUS_plot_time_G3_OOY_YYY + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                   panel.background = element_blank(), axis.line = element_line(colour = "black"))

RO_RUS_plot_time_G3_OOY_YYY

# Multipanel plot

RO_RUS_time <- ggarrange(RO_RUS_plot_time_G3_OOO_YYO,
                         RO_RUS_plot_time_G3_OOY_YYY, 
                         ncol = 1, nrow = 2)

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

dec_L5_G3_Y <- filter(neo_long_MDR_L5_final_trim_merged, final_line == "F3 young_mother N")
dec_L5_G3_O <- filter(neo_long_MDR_L5_final_trim_merged, final_line == "F3 old_mother N")

dec_L5_G3_YO <- neo_long_MDR_L5_final_trim_merged %>%
  filter(final_line == "F3 young_mother Y") %>%
  filter(day < 12)
dec_L5_G3_OY <- neo_long_MDR_L5_final_trim_merged %>%
  filter(final_line == "F3 old_mother Y") %>%
  filter(day < 13)

# L5: Rate of reproductive senescence plots:-------
# OOO vs. YYO

decline_L5_G3_YYO_OOO <- rbind(dec_L5_G3_O, dec_L5_G3_YO)

summ_decline_L5_G3_YYO_OOO <- decline_L5_G3_YYO_OOO %>%
  group_by(generation, line, switched_age, day) %>%
  summarize(mean = mean(num_neonates, na.rm=TRUE),
            n = length(!is.na(num_neonates)))

plot1 <- ggplot(decline_L5_G3_YYO_OOO, aes(x=day, y=num_neonates, color = line)) +
  geom_jitter(alpha = 0.2, size = 0.5) +
  scale_color_manual(values = c("#0072B2", "#D55E00"), labels = c("OOO","YYO")) +
  xlab(element_blank()) +
  ylab(element_blank()) +
  ylim(0,6) +
  xlim(0,16) +
  theme(panel.grid.major = element_blank(), 
        axis.text=element_text(size=14),
        legend.text=element_text(size=18),
        legend.title = element_blank(),
        legend.position = c(.8,.8),
        legend.key.width = unit(3, "line"),
        legend.key = element_rect(fill = NA))

G3_decline_L5_YYO_OOO <- plot1 +
  geom_smooth(data = decline_L5_G3_YYO_OOO, aes(x=day, y=num_neonates, color = line), method="lm", se= FALSE)


G3_decline_L5_YYO_OOO <- G3_decline_L5_YYO_OOO + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                 panel.background = element_blank(), axis.line = element_line(colour = "black"))

G3_decline_L5_YYO_OOO

# YYY vs. OOY

decline_L5_G3_YYY_OOY <- rbind(dec_L5_G3_OY, dec_L5_G3_Y)

summ_decline_L5_G3_YYY_OOY <- decline_L5_G3_YYY_OOY %>%
  group_by(generation, line, switched_age, day) %>%
  summarize(mean = mean(num_neonates, na.rm=TRUE))

plot1 <- ggplot(decline_L5_G3_YYY_OOY, aes(x=day, y=num_neonates, color = line)) +
  geom_jitter(alpha = 0.2, size = 0.5) +
  scale_color_manual(values = c("#009E73", "black"), labels = c("OOY","YYY")) +
  xlab(element_blank()) +
  ylab(element_blank()) +
  ylim(0,6) +
  xlim(0,16) +
  theme(panel.grid.major = element_blank(), 
        axis.text=element_text(size=14),
        legend.text=element_text(size=18),
        legend.title = element_blank(),
        legend.position = c(.8,.8),
        legend.key.width = unit(3, "line"),
        legend.key = element_rect(fill = NA))

G3_decline_L5_YYY_OOY <- plot1 +
  geom_smooth(data = decline_L5_G3_YYY_OOY, aes(x=day, y=num_neonates, color = line), method="lm", se= FALSE)


G3_decline_L5_YYY_OOY <- G3_decline_L5_YYY_OOY + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                       panel.background = element_blank(), axis.line = element_line(colour = "black"))

G3_decline_L5_YYY_OOY

# Multipanel plot

decline_curves_L5 <- ggarrange(G3_decline_L5_YYO_OOO, G3_decline_L5_YYY_OOY, 
                               ncol = 1, nrow = 2)

decline_curves_L5 <- annotate_figure(decline_curves_L5,
                                     left = textGrob(expression(paste("Offspring ind"^"-1"*"day"^"-1")), 
                                                     rot = 90, gp = gpar(fontsize = 20)))
decline_curves_L5

# L5: Rate of reproductive senescence analysis:------------

# To test for reversibility, compare OOO vs YYO and YYY vs OOY
# if reversible, they should be equal

lm_G3_L5_Y <- lmer(num_neonates ~ day + (1|matriline_ID), data = dec_L5_G3_Y)
summary(lm_G3_L5_Y)
lm_G3_L5_O <- lmer(num_neonates ~ day + (1|matriline_ID), data = dec_L5_G3_O)
summary(lm_G3_L5_O)
lm_G3_L5_YO <- lmer(num_neonates ~ day + (1|matriline_ID), data = dec_L5_G3_YO)
summary(lm_G3_L5_YO)
lm_G3_L5_OY <- lmer(num_neonates ~ day + (1|matriline_ID), data = dec_L5_G3_OY)
summary(lm_G3_L5_OY)

# Statistics (ANCOVA):

# Difference in slopes between YYY and OOY?
G3_L5_Y_OY <- rbind(dec_L5_G3_Y, dec_L5_G3_OY)

m.interaction <- lmer(num_neonates ~ day * final_line + (1|matriline_ID), data = G3_L5_Y_OY)
anova(m.interaction)
summary(m.interaction)

# Difference in slopes between OOO and YYO?
G3_L5_O_YO <- rbind(dec_L5_G3_O, dec_L5_G3_YO)

m.interaction <- lmer(num_neonates ~ day * final_line + (1|matriline_ID), data = G3_L5_O_YO)
anova(m.interaction)
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

dec_RUS_G3_Y <- filter(neo_long_MDR_RUS_final_trim_merged, final_line == "F3 young_mother N")
dec_RUS_G3_O <- filter(neo_long_MDR_RUS_final_trim_merged, final_line == "F3 old_mother N")

dec_RUS_G3_YO <- neo_long_MDR_RUS_final_trim_merged %>%
  filter(final_line == "F3 young_mother Y") %>%
  filter(day < 16)
dec_RUS_G3_OY <- filter(neo_long_MDR_RUS_final_trim_merged, final_line == "F3 old_mother Y")


# RUS: Rate of reproductive senescence plots:-------------
# OOO vs. YYO

decline_RUS_G3_YYO_OOO <- rbind(dec_RUS_G3_O, dec_RUS_G3_YO)

summ_decline_RUS_G3_YYO_OOO <- decline_RUS_G3_YYO_OOO %>%
  group_by(generation, line, switched_age, day) %>%
  summarize(mean = mean(num_neonates, na.rm=TRUE))

plot1 <- ggplot(decline_RUS_G3_YYO_OOO, aes(x=day, y=num_neonates, color = line)) +
  geom_jitter(alpha = 0.2, size = 0.5) +
  scale_color_manual(values = c("#0072B2", "#D55E00"), labels = c("OOO","YYO")) +
  xlab(element_blank()) +
  ylab(element_blank()) +
  ylim(0,6) +
  xlim(0,18) +
  theme(panel.grid.major = element_blank(), 
        axis.text=element_text(size=14),
        legend.text=element_text(size=18),
        legend.title = element_blank(),
        legend.position = c(.8,.8),
        legend.key.width = unit(3, "line"),
        legend.key = element_rect(fill = NA))

G3_decline_RUS_YYO_OOO <- plot1 +
  geom_smooth(data = decline_RUS_G3_YYO_OOO, aes(x=day, y=num_neonates, color = line), method="lm", se= FALSE)

G3_decline_RUS_YYO_OOO <- G3_decline_RUS_YYO_OOO + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                         panel.background = element_blank(), axis.line = element_line(colour = "black"))

G3_decline_RUS_YYO_OOO

# OOY vs. YYY

decline_RUS_G3_YYY_OOY <- rbind(dec_RUS_G3_OY, dec_RUS_G3_Y)

summ_decline_RUS_G3_YYY_OOY <- decline_RUS_G3_YYY_OOY %>%
  group_by(generation, line, switched_age, day) %>%
  summarize(mean = mean(num_neonates, na.rm=TRUE))

plot1 <- ggplot(decline_RUS_G3_YYY_OOY, aes(x=day, y=num_neonates, color = line)) +
  geom_jitter(alpha = 0.2, size = 0.5) +
  scale_color_manual(values = c("#009E73", "black"), labels = c("OOY","YYY")) +
  xlab(element_blank()) +
  ylab(element_blank()) +
  ylim(0,6) +
  xlim(0,18) +
  theme(panel.grid.major = element_blank(), 
        axis.text=element_text(size=14),
        legend.text=element_text(size=18),
        legend.title = element_blank(),
        legend.position = c(.8,.8),
        legend.key.width = unit(3, "line"),
        legend.key = element_rect(fill = NA))

G3_decline_RUS_YYY_OOY <- plot1 +
  geom_smooth(data = decline_RUS_G3_YYY_OOY, aes(x=day, y=num_neonates, color = line), method="lm", se= FALSE)

G3_decline_RUS_YYY_OOY <- G3_decline_RUS_YYY_OOY + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                         panel.background = element_blank(), axis.line = element_line(colour = "black"))

G3_decline_RUS_YYY_OOY

# Multipanel plot

decline_curves_RUS <- ggarrange(G3_decline_RUS_YYO_OOO, G3_decline_RUS_YYY_OOY, 
                                ncol = 1, nrow = 2)

decline_curves_RUS <- annotate_figure(decline_curves_RUS,
                                      left = textGrob(expression(paste("Offspring ind"^"-1"*"day"^"-1")), 
                                                      rot = 90, gp = gpar(fontsize = 20)))
decline_curves_RUS

# RUS: Rate of reproductive senescence analysis:------------

# To test for reversibility, compare OOO vs YYO and YYY vs OOY
# if reversible, they should be equal

lm_G3_RUS_Y <- lmer(num_neonates ~ day + (1|matriline_ID), data = dec_RUS_G3_Y)
summary(lm_G3_RUS_Y)
lm_G3_RUS_O <- lmer(num_neonates ~ day + (1|matriline_ID), data = dec_RUS_G3_O)
summary(lm_G3_RUS_O)
lm_G3_RUS_YO <- lmer(num_neonates ~ day + (1|matriline_ID), data = dec_RUS_G3_YO)
summary(lm_G3_RUS_YO)
lm_G3_RUS_OY <- lmer(num_neonates ~ day + (1|matriline_ID), data = dec_RUS_G3_OY)
summary(lm_G3_RUS_OY)

# Statistics (ANCOVA):

# Difference in slopes between YYY and OOY?
G3_RUS_Y_OY <- rbind(dec_RUS_G3_Y, dec_RUS_G3_OY)

m.interaction <- lmer(num_neonates ~ day * final_line + (1|matriline_ID), data = G3_RUS_Y_OY)
anova(m.interaction)
summary(m.interaction)

# Difference in slopes between OOO and YYO?
G3_RUS_O_YO <- rbind(dec_RUS_G3_O, dec_RUS_G3_YO)

m.interaction <- lmer(num_neonates ~ day * final_line + (1|matriline_ID), data = G3_RUS_O_YO)
anova(m.interaction)
summary(m.interaction)

#--------------------------------------------------------------------
# Big multipanel plot - all response metrics that are plotted over time----
# Figures 5 and 6

pdf("Q2_Gen3_L5.pdf", width=14, height=7)

Q2_Gen3_L5 <- ggarrange(surv_curves_L5, RO_L5_time, decline_curves_L5, 
                            ncol = 3, nrow = 1,
                            labels = c("A","B","C"),
                            font.label = list(size = 20))

Q2_Gen3_L5 <- annotate_figure(Q2_Gen3_L5,
                                  bottom = textGrob("Age (d)", 
                                                    gp = gpar(fontsize = 22)))
Q2_Gen3_L5

dev.off()

pdf("Q2_Gen3_RUS.pdf", width=14, height=7)

Q2_Gen3_RUS <- ggarrange(surv_curves_RUS, RO_RUS_time, decline_curves_RUS, 
                        ncol = 3, nrow = 1,
                        labels = c("A","B","C"),
                        font.label = list(size = 20))

Q2_Gen3_RUS <- annotate_figure(Q2_Gen3_RUS,
                              bottom = textGrob("Age (d)", 
                                                gp = gpar(fontsize = 22)))
Q2_Gen3_RUS

dev.off()

#--------------------------------------------------------------------
# Statistical analyses for LRO - GLM----------------
# Isolating the F3 generation:

LRO_per_ind_F3 <- filter(LRO_combo, generation == "F3")
LRO_per_ind_L5_F3 <- filter(LRO_per_ind_L5, generation == "F3")
LRO_per_ind_RUS_F3 <- filter(LRO_per_ind_RUS, generation == "F3")

# Adding grandmaternal age and maternal age columns:

LRO_per_ind_F3 <- LRO_per_ind_F3 %>%
  mutate(grandmaternal_age = case_when(
    endsWith(line, "old_mother") ~ "O",
    endsWith(line, "young_mother") ~ "Y"
  ))

LRO_per_ind_F3 <- LRO_per_ind_F3 %>%
  mutate(maternal_age = case_when(
    endsWith(final_line, "F3 old_mother N") ~ "O",
    endsWith(final_line, "F3 old_mother Y") ~ "Y",
    endsWith(final_line, "F3 young_mother N") ~ "Y",
    endsWith(final_line, "F3 young_mother Y") ~ "O",
  ))

LRO_per_ind_L5_F3 <- LRO_per_ind_L5_F3 %>%
  mutate(grandmaternal_age = case_when(
    endsWith(line, "old_mother") ~ "O",
    endsWith(line, "young_mother") ~ "Y"
  ))

LRO_per_ind_L5_F3 <- LRO_per_ind_L5_F3 %>%
  mutate(maternal_age = case_when(
    endsWith(final_line, "F3 old_mother N") ~ "O",
    endsWith(final_line, "F3 old_mother Y") ~ "Y",
    endsWith(final_line, "F3 young_mother N") ~ "Y",
    endsWith(final_line, "F3 young_mother Y") ~ "O",
  ))

LRO_per_ind_RUS_F3 <- LRO_per_ind_RUS_F3 %>%
  mutate(grandmaternal_age = case_when(
    endsWith(line, "old_mother") ~ "O",
    endsWith(line, "young_mother") ~ "Y"
  ))

LRO_per_ind_RUS_F3 <- LRO_per_ind_RUS_F3 %>%
  mutate(maternal_age = case_when(
    endsWith(final_line, "F3 old_mother N") ~ "O",
    endsWith(final_line, "F3 old_mother Y") ~ "Y",
    endsWith(final_line, "F3 young_mother N") ~ "Y",
    endsWith(final_line, "F3 young_mother Y") ~ "O",
  ))

# generalized mixed model
# Using Poisson for count data

LRO_glmer <- glmer(LRO ~ strain * maternal_age * grandmaternal_age + (1|matriline_ID), family=poisson, data=LRO_per_ind_F3)

check_overdispersion(LRO_glmer)
# Overdispersed

# Negative binomial GLMM

LRO_glmer_nb <- glmer.nb(LRO ~ strain * maternal_age * grandmaternal_age + (1|matriline_ID), data=LRO_per_ind_F3)

LRO_summary <- summary(LRO_glmer_nb)
LRO_summary

LRO_table <- LRO_summary[[10]]
LRO_table <- as.data.frame(LRO_table)

# exporting model summary for supplement
write.csv(LRO_table, "Q2_LRO.csv")

# likelihood ratio test

drop1(LRO_glmer_nb, test = "Chisq") # tests for 3 way interaction

# model with all two-way interactions included
LRO_glmer_twoway <- glmer.nb(LRO ~ strain + grandmaternal_age + maternal_age
                          + strain*grandmaternal_age + strain*maternal_age + grandmaternal_age*maternal_age
                          + (1|matriline_ID), data=LRO_per_ind_F3)

LRO_glmer_SG <- glmer.nb(LRO ~ strain + grandmaternal_age + maternal_age
                      + strain*maternal_age + grandmaternal_age*maternal_age
                      + (1|matriline_ID), data=LRO_per_ind_F3)

LRO_glmer_SM <- glmer.nb(LRO ~ strain + grandmaternal_age + maternal_age
                      + strain*grandmaternal_age + grandmaternal_age*maternal_age
                      + (1|matriline_ID), data=LRO_per_ind_F3)

LRO_glmer_GM <- glmer.nb(LRO ~ strain + grandmaternal_age + maternal_age
                      + strain*grandmaternal_age + strain*maternal_age
                      + (1|matriline_ID), data=LRO_per_ind_F3)

# just main effects
LRO_glmer_main <- glmer.nb(LRO ~ strain + grandmaternal_age + maternal_age
                        + (1|matriline_ID), data=LRO_per_ind_F3)

# testing for SG interaction
anova(LRO_glmer_twoway, LRO_glmer_SG) # significant

# testing for SM interaction
anova(LRO_glmer_twoway, LRO_glmer_SM) # significant

# testing for GM interaction
anova(LRO_glmer_twoway, LRO_glmer_GM) # significant

# plotting residuals
plot(LRO_glmer_nb)

# post hoc comparisons
pairwise_LRO <- emmeans(LRO_glmer_nb, pairwise ~ grandmaternal_age * maternal_age | strain)
pairwise_LRO

# L5: LRO Figures --------------------

LRO_summary_L5_G3 <- filter(LRO_summary_L5, generation == "F3")

LRO_summary_L5_G3$final_line <- factor(LRO_summary_L5_G3$final_line, 
                                       levels = c("F3 young_mother N", "F3 young_mother Y",
                                                  "F3 old_mother N", "F3 old_mother Y"))

LRO_per_ind_L5_G3 <-  filter(LRO_per_ind_L5, generation == "F3")

LRO_per_ind_L5_G3$final_line <- factor(LRO_per_ind_L5_G3$final_line, 
                                       levels = c("F3 young_mother N", "F3 young_mother Y",
                                                  "F3 old_mother N", "F3 old_mother Y"))

pairwise_L5_G3 <- c("a","b","bc","ac") # from pairwise analyses
# Plotting means and SE
LRO_raw <- ggplot(data = LRO_summary_L5_G3, mapping = aes(x = final_line, y = mean)) +
  geom_point(size = 3) +
  geom_line(data = LRO_summary_L5_G3, aes(x = final_line, y =mean, group = line)) +
  geom_errorbar(data = LRO_summary_L5_G3, 
                aes(x = final_line, ymin=mean-SE, ymax=mean+SE), width=0.1) +
  geom_text(label=pairwise_L5_G3, 
            nudge_x = 0.3,
            size = 8) +
  geom_label(label = "BmanL5",
             x = 1,
             y = 35,
             size = 8,
             label.size = 0)

# Plotting the raw data points
LRO_combo <- LRO_raw +
  geom_jitter(data = LRO_per_ind_L5_G3, aes(x = final_line, y = LRO),
              width=.2, alpha=0.2, size = 2)

LRO_L5_G3_plot <- LRO_combo + 
  ylab("") +
  xlab(element_blank()) +
  ylim(c(-0.5,37)) +
  scale_x_discrete(labels=c("YYY", "YYO", "OOO", "OOY")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        strip.text=element_text(size=20),
        axis.text=element_text(size=20))
LRO_L5_G3_plot

# RUS: LRO Figures-------------------------
LRO_summary_RUS_G3 <- filter(LRO_summary_RUS, generation == "F3")

LRO_summary_RUS_G3$final_line <- factor(LRO_summary_RUS_G3$final_line, 
                                        levels = c("F3 young_mother N", "F3 young_mother Y",
                                                   "F3 old_mother N", "F3 old_mother Y"))

LRO_per_ind_RUS_G3 <-  filter(LRO_per_ind_RUS, generation == "F3")

LRO_per_ind_RUS_G3$final_line <- factor(LRO_per_ind_RUS_G3$final_line, 
                                        levels = c("F3 young_mother N", "F3 young_mother Y",
                                                   "F3 old_mother N", "F3 old_mother Y"))

pairwise_RUS_G3 <- c("b","c","a","ab") # from pairwise analyses
# Plotting means and SE
LRO_raw <- ggplot(data = LRO_summary_RUS_G3, mapping = aes(x = final_line, y = mean))+
  geom_point(size = 3) +
  geom_line(data = LRO_summary_RUS_G3, aes(x = final_line, y =mean, group = line)) +
  geom_errorbar(data = LRO_summary_RUS_G3, 
                aes(x = final_line, ymin=mean-SE, ymax=mean+SE), width=0.1) +
  geom_text(label=pairwise_RUS_G3, 
            nudge_x = 0.3,
            size = 8) +
  geom_label(label = "BmanRUS",
             x = 1,
             y = 35,
             size = 8,
             label.size = 0)

# Plotting the raw data points
LRO_combo <- LRO_raw +
  geom_jitter(data = LRO_per_ind_RUS_G3, aes(x = final_line, y = LRO),
              width=.2, alpha=0.2, size = 2)

LRO_RUS_G3_plot <- LRO_combo + 
  ylab("") +
  xlab(element_blank()) +
  ylim(c(-0.5,37)) +
  scale_x_discrete(labels=c("YYY", "YYO", "OOO", "OOY")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        strip.text=element_text(size=20),
        axis.text=element_text(size=20))
LRO_RUS_G3_plot

# LRO G3 Multipanel plot: -------------------------
# Figure 7

pdf("LRO_G3.pdf", width=8, height=11)

LRO <- ggarrange(LRO_RUS_G3_plot, LRO_L5_G3_plot,
                 ncol = 1, nrow = 2,
                 labels = c("A","B"),
                 font.label = list(size = 24))

LRO <- annotate_figure(LRO, left = textGrob(expression(paste("Lifetime reproductive output (offspring ind"^"-1"*")")), 
                                            rot = 90, gp = gpar(fontsize = 24)),
                       bottom = textGrob("Cohort", gp = gpar(fontsize = 24)))

LRO
dev.off()

#--------------------------------------------------------------------
# Maximum daily reproduction (MDR) analyses--------------

# generalized mixed model
# Using Poisson for count data

MDR_glmer <- glmer(max_RO ~ strain * maternal_age * grandmaternal_age + (1|matriline_ID), family=poisson, data=LRO_per_ind_F3)

check_overdispersion(MDR_glmer)

MDR_summary <- summary(MDR_glmer)
MDR_summary

MDR_table <- MDR_summary[[10]]
MDR_table <- as.data.frame(MDR_table)

# exporting model summary for supplement
write.csv(MDR_table, "Q2_MDR.csv")

# likelihood ratio test

drop1(MDR_glmer, test = "Chisq") # tests for 3 way interaction

# model with all two-way interactions included
MDR_glmer_twoway <- glmer(max_RO ~ strain + grandmaternal_age + maternal_age
                          + strain*grandmaternal_age + strain*maternal_age + grandmaternal_age*maternal_age
                          + (1|matriline_ID), family=poisson, data=LRO_per_ind_F3)

MDR_glmer_SG <- glmer(max_RO ~ strain + grandmaternal_age + maternal_age
                      + strain*maternal_age + grandmaternal_age*maternal_age
                      + (1|matriline_ID), family=poisson, data=LRO_per_ind_F3)

MDR_glmer_SM <- glmer(max_RO ~ strain + grandmaternal_age + maternal_age
                      + strain*grandmaternal_age + grandmaternal_age*maternal_age
                      + (1|matriline_ID), family=poisson, data=LRO_per_ind_F3)

MDR_glmer_GM <- glmer(max_RO ~ strain + grandmaternal_age + maternal_age
                      + strain*grandmaternal_age + strain*maternal_age
                      + (1|matriline_ID), family=poisson, data=LRO_per_ind_F3)

# just main effects
MDR_glmer_main <- glmer(max_RO ~ strain + grandmaternal_age + maternal_age
                        + (1|matriline_ID), family=poisson, data=LRO_per_ind_F3)

MDR_glmer_strain <- glmer(max_RO ~ grandmaternal_age + maternal_age
                          + (1|matriline_ID), family=poisson, data=LRO_per_ind_F3)
MDR_glmer_grandmat <- glmer(max_RO ~ strain + maternal_age
                               + (1|matriline_ID), family=poisson, data=LRO_per_ind_F3)
MDR_glmer_mat <- glmer(max_RO ~ strain + grandmaternal_age
                       + (1|matriline_ID), family=poisson, data=LRO_per_ind_F3)

# testing for SG interaction
anova(MDR_glmer_twoway, MDR_glmer_SG)

# testing for SM interaction
anova(MDR_glmer_twoway, MDR_glmer_SM)

# testing for GM interaction
anova(MDR_glmer_twoway, MDR_glmer_GM)

# testing for main effect of strain
anova(MDR_glmer_main, MDR_glmer_strain) # significant

# testing for main effect of grandmaternal age
anova(MDR_glmer_main, MDR_glmer_grandmat) # significant

# testing for main effect of maternal age
anova(MDR_glmer_main, MDR_glmer_mat)

# plotting residuals
plot(MDR_glmer)

#--------------------------------------------------------------------
# L5: Calculating and analyzing timing of the production of 25% of LRO-----

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
    med=median(time_25, na.rm = TRUE),
    SD=sd(time_25, na.rm =TRUE),
    SE=sd(time_25, na.rm =TRUE)/sqrt(length(!is.na(time_25))))
LRO_25_summary_L5

# RUS: Calculating and analyzing timing of the production of 25% of LRO-----

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
    med=median(time_25, na.rm = TRUE),
    SD=sd(time_25, na.rm =TRUE),
    SE=sd(time_25, na.rm =TRUE)/sqrt(length(!is.na(time_25))))
LRO_25_summary_RUS

# Need to merge time of 25% LRO data and neonate origin data (so that we can include mom identity in the models below)

LRO_25_origin_L5 <- merge(sub_summ_L5, origins_L5, 
                          by = c("generation", "line", "switched_age", "plate", "well"))

LRO_25_origin_RUS <- merge(sub_summ_RUS, origins_RUS, 
                           by = c("generation", "line", "switched_age", "plate", "well"))

# Combining strains:

sub_summ_combo <- rbind(LRO_25_origin_L5, LRO_25_origin_RUS)

# Statistics - 25% LRO---------------------------

# Switched age cohorts: are maternal age effects reversible?

sub_summ_combo$final_line <- paste(sub_summ_combo$generation,
                                sub_summ_combo$line,
                                sub_summ_combo$switched_age)

LRO25_F3 <- sub_summ_combo %>%
  filter(generation == "F3")

# Adding grandmaternal age and maternal age columns:

LRO25_F3 <- LRO25_F3 %>%
  mutate(grandmaternal_age = case_when(
    endsWith(line, "old_mother") ~ "O",
    endsWith(line, "young_mother") ~ "Y"
  ))

LRO25_F3 <- LRO25_F3 %>%
  mutate(maternal_age = case_when(
    endsWith(final_line, "F3 old_mother N") ~ "O",
    endsWith(final_line, "F3 old_mother Y") ~ "Y",
    endsWith(final_line, "F3 young_mother N") ~ "Y",
    endsWith(final_line, "F3 young_mother Y") ~ "O",
  ))

sub_summ_L5$final_line <- paste(sub_summ_L5$generation,
                                sub_summ_L5$line,
                                sub_summ_L5$switched_age)

LRO25_L5_F3 <- sub_summ_L5 %>%
  filter(generation == "F3")

# Adding grandmaternal age and maternal age columns:

LRO25_L5_F3 <- LRO25_L5_F3 %>%
  mutate(grandmaternal_age = case_when(
    endsWith(line, "old_mother") ~ "O",
    endsWith(line, "young_mother") ~ "Y"
  ))

LRO25_L5_F3 <- LRO25_L5_F3 %>%
  mutate(maternal_age = case_when(
    endsWith(final_line, "F3 old_mother N") ~ "O",
    endsWith(final_line, "F3 old_mother Y") ~ "Y",
    endsWith(final_line, "F3 young_mother N") ~ "Y",
    endsWith(final_line, "F3 young_mother Y") ~ "O",
  ))

sub_summ_RUS$final_line <- paste(sub_summ_RUS$generation,
                                 sub_summ_RUS$line,
                                 sub_summ_RUS$switched_age)

LRO25_RUS_F3 <- sub_summ_RUS %>%
  filter(generation == "F3")

# Adding grandmaternal age and maternal age columns:

LRO25_RUS_F3 <- LRO25_RUS_F3 %>%
  mutate(grandmaternal_age = case_when(
    endsWith(line, "old_mother") ~ "O",
    endsWith(line, "young_mother") ~ "Y"
  ))

LRO25_RUS_F3 <- LRO25_RUS_F3 %>%
  mutate(maternal_age = case_when(
    endsWith(final_line, "F3 old_mother N") ~ "O",
    endsWith(final_line, "F3 old_mother Y") ~ "Y",
    endsWith(final_line, "F3 young_mother N") ~ "Y",
    endsWith(final_line, "F3 young_mother Y") ~ "O",
  ))

# generalized mixed model
# Using Poisson for count data

LRO25_glmer <- glmer(time_25 ~ strain * maternal_age * grandmaternal_age + (1|matriline_ID), family=poisson, data=LRO25_F3)

check_overdispersion(LRO25_glmer)

LRO25_summary <- summary(LRO25_glmer)
LRO25_summary

LRO25_table <- LRO25_summary[[10]]
LRO25_table <- as.data.frame(LRO25_table)

# exporting model summary for supplement
write.csv(LRO25_table, "Q2_25LRO.csv")

# likelihood ratio test

drop1(LRO25_glmer, test = "Chisq") # tests for 3 way interaction

# model with all two-way interactions included
LRO25_glmer_twoway <- glmer(time_25 ~ strain + grandmaternal_age + maternal_age
                          + strain*grandmaternal_age + strain*maternal_age + grandmaternal_age*maternal_age
                          + (1|matriline_ID), family=poisson, data=LRO25_F3)

LRO25_glmer_SG <- glmer(time_25 ~ strain + grandmaternal_age + maternal_age
                      + strain*maternal_age + grandmaternal_age*maternal_age
                      + (1|matriline_ID), family=poisson, data=LRO25_F3)

LRO25_glmer_SM <- glmer(time_25 ~ strain + grandmaternal_age + maternal_age
                      + strain*grandmaternal_age + grandmaternal_age*maternal_age
                      + (1|matriline_ID), family=poisson, data=LRO25_F3)

LRO25_glmer_GM <- glmer(time_25 ~ strain + grandmaternal_age + maternal_age
                      + strain*grandmaternal_age + strain*maternal_age
                      + (1|matriline_ID), family=poisson, data=LRO25_F3)

# just main effects
LRO25_glmer_main <- glmer(time_25 ~ strain + grandmaternal_age + maternal_age
                        + (1|matriline_ID), family=poisson, data=LRO25_F3)

LRO25_glmer_strain <- glmer(time_25 ~ grandmaternal_age + maternal_age
                          + (1|matriline_ID), family=poisson, data=LRO25_F3)
LRO25_glmer_grandmat <- glmer(time_25 ~ strain + maternal_age
                            + (1|matriline_ID), family=poisson, data=LRO25_F3)
LRO25_glmer_mat <- glmer(time_25 ~ strain + grandmaternal_age
                       + (1|matriline_ID), family=poisson, data=LRO25_F3)

# testing for SG interaction
anova(LRO25_glmer_twoway, LRO25_glmer_SG)

# testing for SM interaction
anova(LRO25_glmer_twoway, LRO25_glmer_SM)

# testing for GM interaction
anova(LRO25_glmer_twoway, LRO25_glmer_GM)

# testing for main effect of strain
anova(LRO25_glmer_main, LRO25_glmer_strain) # significant

# testing for main effect of grandmaternal age
anova(LRO25_glmer_main, LRO25_glmer_grandmat)

# testing for main effect of maternal age
anova(LRO25_glmer_main, LRO25_glmer_mat)

# plotting residuals
plot(LRO25_glmer)

#--------------------------------------------------------------------
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

# Calculating summary statistics across individuals - L5
rep_summary_L5 <- rep_per_ind_L5 %>%
  group_by(generation, line, switched_age) %>%
  summarize(
    mean_perc=mean(percent_rep, na.rm = TRUE), 
    med_perc=median(percent_rep, na.rm = TRUE),
    SD_perc=sd(percent_rep, na.rm =TRUE),
    SE_perc=sd(percent_rep, na.rm =TRUE)/sqrt(length(!is.na(percent_rep))),
    mean_days=mean(rep_time, na.rm=TRUE),
    SD_days=sd(rep_time, na.rm =TRUE),
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
    med_perc=median(percent_rep, na.rm = TRUE),
    SD_perc=sd(percent_rep, na.rm =TRUE),
    SE_perc=sd(percent_rep, na.rm =TRUE)/sqrt(length(!is.na(percent_rep))),
    mean_days=mean(rep_time, na.rm=TRUE),
    SD_days=sd(rep_time, na.rm =TRUE),
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

# Reproductive period analyses:---------
# Switched age cohorts: are maternal age effects reversible?

rep_per_ind_combo$final_line <- paste(rep_per_ind_combo$generation,
                                    rep_per_ind_combo$line,
                                    rep_per_ind_combo$switched_age)

rep_G3 <- filter(rep_per_ind_combo, generation == "F3")

rep_G3 <- rep_G3 %>%
  mutate(grandmaternal_age = case_when(
    endsWith(line, "old_mother") ~ "O",
    endsWith(line, "young_mother") ~ "Y"
  ))

rep_G3 <- rep_G3 %>%
  mutate(maternal_age = case_when(
    endsWith(final_line, "F3 old_mother N") ~ "O",
    endsWith(final_line, "F3 old_mother Y") ~ "Y",
    endsWith(final_line, "F3 young_mother N") ~ "Y",
    endsWith(final_line, "F3 young_mother Y") ~ "O",
  ))

rep_L5_G3 <- filter(rep_per_ind_L5, generation == "F3")

# adding grandmaternal age and maternal age columns:

rep_L5_G3 <- rep_L5_G3 %>%
  mutate(grandmaternal_age = case_when(
    endsWith(line, "old_mother") ~ "O",
    endsWith(line, "young_mother") ~ "Y"
  ))

rep_L5_G3 <- rep_L5_G3 %>%
  mutate(maternal_age = case_when(
    endsWith(final_line, "F3 old_mother N") ~ "O",
    endsWith(final_line, "F3 old_mother Y") ~ "Y",
    endsWith(final_line, "F3 young_mother N") ~ "Y",
    endsWith(final_line, "F3 young_mother Y") ~ "O",
  ))

# Switched age cohorts: are maternal age effects reversible?

rep_RUS_G3 <- filter(rep_per_ind_RUS, generation == "F3")

# adding grandmaternal age and maternal age columns:

rep_RUS_G3 <- rep_RUS_G3 %>%
  mutate(grandmaternal_age = case_when(
    endsWith(line, "old_mother") ~ "O",
    endsWith(line, "young_mother") ~ "Y"
  ))

rep_RUS_G3 <- rep_RUS_G3 %>%
  mutate(maternal_age = case_when(
    endsWith(final_line, "F3 old_mother N") ~ "O",
    endsWith(final_line, "F3 old_mother Y") ~ "Y",
    endsWith(final_line, "F3 young_mother N") ~ "Y",
    endsWith(final_line, "F3 young_mother Y") ~ "O",
  ))

# generalized mixed model
# Using binomial for proportion data

bin_glmer <- glmer(prop_rep ~ strain * maternal_age * grandmaternal_age + (1|matriline_ID), family=binomial, data=rep_G3, weights = length)

check_overdispersion(bin_glmer)

bin_summary <- summary(bin_glmer)
bin_summary

bin_table <- bin_summary[[10]]
bin_table <- as.data.frame(bin_table)

# exporting model summary for supplement
write.csv(bin_table, "Q2_rep.csv")

# Model validation (plotting residuals)
plot(bin_glmer)

# likelihood ratio test

drop1(bin_glmer, test = "Chisq") # tests for 3 way interaction

# model with all two-way interactions included
bin_glmer_twoway <- glmer(prop_rep ~ strain + grandmaternal_age + maternal_age
                            + strain*grandmaternal_age + strain*maternal_age + grandmaternal_age*maternal_age
                            + (1|matriline_ID), family=binomial, data=rep_G3, weights = length)

bin_glmer_SG <- glmer(prop_rep ~ strain + grandmaternal_age + maternal_age
                        + strain*maternal_age + grandmaternal_age*maternal_age
                        + (1|matriline_ID), family=binomial, data=rep_G3, weights = length)

bin_glmer_SM <- glmer(prop_rep ~ strain + grandmaternal_age + maternal_age
                        + strain*grandmaternal_age + grandmaternal_age*maternal_age
                        + (1|matriline_ID), family=binomial, data=rep_G3, weights = length)

bin_glmer_GM <- glmer(prop_rep ~ strain + grandmaternal_age + maternal_age
                        + strain*grandmaternal_age + strain*maternal_age
                        + (1|matriline_ID), family=binomial, data=rep_G3, weights = length)

# just main effects
bin_glmer_main <- glmer(prop_rep ~ strain + grandmaternal_age + maternal_age
                          + (1|matriline_ID), family=binomial, data=rep_G3, weights = length)

# testing for SG interaction
anova(bin_glmer_twoway, bin_glmer_SG)

# testing for SM interaction
anova(bin_glmer_twoway, bin_glmer_SM)

# testing for GM interaction
anova(bin_glmer_twoway, bin_glmer_GM) # significant

# post hoc comparisons

pairwise_rep <- emmeans(bin_glmer, pairwise ~ grandmaternal_age * maternal_age | strain)
pairwise_rep

