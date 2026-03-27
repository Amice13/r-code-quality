#Load data
kmlc_data <- read.csv("kmlc_data.csv")


#Load package
library(plyr)
library(dplyr)

#Demographic characteristics
count(kmlc_data, aspara, wt_var=cohort2)
count(kmlc_data, aspara, wt_var=age_groups)
count(kmlc_data, aspara, wt_var=sex)
count(kmlc_data, gampos, wt_var=cohort2)
count(kmlc_data, gampos, wt_var=age_groups)
count(kmlc_data, gampos, wt_var=sex)
count(kmlc_data, age_groups, wt_var = cohort2)
count(kmlc_data, cohort2, wt_var = sex)


#change to log
kmlc_data$pf_mcllog <- log10(kmlc_data$pf_mcl+1)# +1 records transforms values above 1
kmlc_data$gam_mcllog <- log10(kmlc_data$gam_mcl+1)
kmlc_data$ge.conclog <- log10(kmlc_data$ge.conc)
kmlc_data$ama1.conclog<- log10(kmlc_data$ama1.conc)

#Load package for ggplots
library(ggpubr)
library(ggplot2)

library(EnvStats) #package for adding median and iqr in plots


#comparing means of transmission setting vs ama1 responses
ama1rescohort_kmlc <- ggplot(kmlc_data, aes(x = cohort2, y = ama1.conclog)) + 
  geom_boxplot(position = position_dodge(1)) +
  geom_jitter(shape = 16, position = position_jitter(0.2)) +
  stat_compare_means(label="p.signif",comparisons = list(c("Junju", "Ngerenya Early"), 
                                                         c("Junju", "Ngerenya Late"),
                                                         c("Ngerenya Early", "Ngerenya Late")),
                     step_increase = .1) + #add p value to plots
  labs(x = "Transmission setting",y = "Antibody units (AMA1)") +
  theme(
    panel.background = element_rect(fill = "white"),
    legend.key  = element_rect(fill = "white"),
    axis.line.x = element_line(colour = "black", size = 1),
    axis.line.y = element_line(colour = "black", size = 1)
  )

ama1rescohort + stat_median_iqr_text(y.pos=8)

#comparing means of transmission setting vs gametocyte responses
gamrescohort_kmlc <- ggplot(kmlc_data, aes(x = cohort2, y = ge.conclog)) + 
  geom_boxplot(position = position_dodge(1)) +
  geom_jitter(shape = 16, position = position_jitter(0.2)) +
  stat_compare_means(label="p.signif",comparisons = list(c("Junju", "Ngerenya Early"), 
                                                         c("Junju", "Ngerenya Late"),
                                                         c("Ngerenya Early", "Ngerenya Late")),
                     step_increase = .1) + #add p value to plots
  
  labs(x = "Transmission setting",y = "Antibody units (Gametocyte extract)") +
  theme(
    panel.background = element_rect(fill = "white"),
    legend.key  = element_rect(fill = "white"),
    axis.line.x = element_line(colour = "black", size = 1),
    axis.line.y = element_line(colour = "black", size = 1)
  )

gamrescohort_kmlc + stat_median_iqr_text(y.pos = 8)


#Correlation analysis

#Load colour package for scatter plots
library(viridis)

#Disable exponential scale........# to print p<0.0001 on correlation plots
options(scipen = 999)

#Correlation asexual parasiemia and gametocytaemia
asexgametocytaemia_kmlc <- ggscatter(
  kmlc_data,
  x = "gam_mcllog",
  y = "pf_mcllog",
  color = "age_groups",
  add = "reg.line",
  # Add regressin line
  add.params = list(color = "blue", fill = "lightgray"),
  # Customize reg. line
  conf.int = TRUE
) + scale_color_viridis(alpha=1, begin = 0, end = 0.7, discrete = TRUE, option = "D") + 
  stat_cor(aes(),
             method = "spearman", p.accuracy = 0.0001,
             label.x = 0.3,
             label.y = 5.8) +
  labs(x = "Gametocytaemia (Microscopy)",
       y = "Asexual parasitaemia (Microscopy)") +
  theme(
    panel.background = element_rect(fill = "white"),
    legend.key  = element_rect(fill = "white"),
    axis.line.x = element_line(colour = "black", size = 1),
    axis.line.y = element_line(colour = "black", size = 1)
  )
asexgametocytaemia_kmlc1 <- ggpar(asexgametocytaemia_kmlc, legend.title = "")
asexgametocytaemia_kmlc1


# Correlation asexual parasiemia and gam response
asexgamrescor_kmlc <- ggscatter(
  kmlc_data,
  x = "pf_mcllog",
  y = "ge.conclog",
  color = "age_groups",
  add = "reg.line",
  # Add regressin line
  add.params = list(color = "blue", fill = "lightgray"),
  # Customize reg. line
  conf.int = TRUE
) + scale_color_viridis(alpha=1, begin = 0, end = 0.7, discrete = TRUE, option = "D") + #specifying the colour range 
  stat_cor(aes(),
             method = "spearman", p.accuracy = 0.0001, #add p value accuracy at 0.0001 
             label.x = 0.3,
             label.y = 0.5) +
  labs(x = "Asexual parasitaemia (Microscopy)",
       y = "Antibody units (Gametocyte extract)") +
  theme(
    panel.background = element_rect(fill = "white"),
    legend.key  = element_rect(fill = "white"),
    axis.line.x = element_line(colour = "black", size = 1),
    axis.line.y = element_line(colour = "black", size = 1)
  )

asexgamrescor_kmlc1 <- ggpar(asexgamrescor_kmlc,legend.title = "")
asexgamrescor_kmlc1

# Correlation gam carriage and gam response
gametocytemiagamrescor_kmlc <- ggscatter(
  kmlc_data,
  x = "gam_mcllog" ,
  y = "ge.conclog",
  color = "age_groups",
  add = "reg.line",
  # Add regressin line
  add.params = list(color = "blue", fill = "lightgray"),
  # Customize reg. line
  conf.int = TRUE
) + scale_color_viridis(alpha=1, begin = 0, end = 0.7, discrete = TRUE, option = "D") + 
  stat_cor(aes(),
             method = "spearman", p.accuracy = 0.0001,
             label.x = 0.3,
             label.y = 0.5) + 
  labs(x = "Gametocytaemia (Microscopy)",
       y = "Antibody units (Gametocyte extract)") +
  theme(
    panel.background = element_rect(fill = "white"),
    legend.key  = element_rect(fill = "white"),
    axis.line.x = element_line(colour = "black", size = 1),
    axis.line.y = element_line(colour = "black", size = 1)
  )

gametocytemiagamrescor_kmlc1 <- ggpar(gametocytemiagamrescor_kmlc, legend.title = "")
gametocytemiagamrescor_kmlc1

#Enable exponential scale after finishing scatter plots
options(scipen = 0)

#Combine plots
ggarrange(asexgamrescor_afirm1, asexgamrescor_kmlc1, gametocytemiagamrescor_afirm1
          ,gametocytemiagamrescor_kmlc1,  
          labels = c("A", "B", "C", "D"), nrow=2, ncol=2, common.legend = TRUE, legend = "bottom",
          legend.grob = get_legend(ama1gamres_combined1) ) # save as pdf 9x9 inches.


#Combine the plots
ggarrange(ama1responseseason_afirm, gamresponseseason_afirm, ama1rescohort_kmlc
          ,gamrescohort_kmlc, 
          labels = c("A", "B", "C", "D"), nrow=2, ncol=2) # save as pdf 9x9 inches.

#Plots in the supplementary material

#Parasitaemia by age and transmission setting in KMLC

#To generate agegroups
kmlc_data$age_groups <- kmlc_data$age2
kmlc_data$age_groups <- ifelse((kmlc_data$age2 >=0 &kmlc_data$age2<5),"<5 Yrs", kmlc_data$age_groups)
kmlc_data$age_groups <- ifelse((kmlc_data$age2 >=5 &kmlc_data$age2<10),"5-9 Yrs", kmlc_data$age_groups)
kmlc_data$age_groups <- ifelse((kmlc_data$age2 >=10 &kmlc_data$age2<=15),"10-15 Yrs", kmlc_data$age_groups)

#To reorganise plots in ggplot according to factors
kmlc_data$age_groups <- 
  factor(kmlc_data$age_groups,
         levels = c("<5 Yrs", "5-9 Yrs","10-15 Yrs"))

#Compare means with p-value asexual parasitaemia vs age
asexparasitaemiaage_kmlc <- ggplot(aes(x=age_groups, y= log10(pf_mcl)), data = kmlc_data) +
  geom_boxplot(position = position_dodge(1)) + 
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  stat_compare_means(label="p.signif",comparisons = list(c("<5 Yrs", "5-9 Yrs"), c("<5 Yrs","10-15 Yrs"),
                                                         c("5-9 Yrs", "10-15 Yrs"))) + #add p value to plots
  xlab("Age group") + ylab("Asexual parasitaemia (Microscopy)") + # Set axis labels
  theme(
    panel.background = element_rect(fill = "white"),
    legend.key  = element_rect(fill = "white"),
    axis.line.x = element_line(colour = "black", size = 1),
    axis.line.y = element_line(colour = "black", size = 1))

#add median and iqr text
asexparasitaemiaage_kmlc +  stat_median_iqr_text(y.pos = 9)


#Compare means with p-value gametocytaemia vs age
gametocytaemiaage_kmlc <- ggplot(aes(x=age_groups, y= log10(gam_mcl)), data = kmlc_data) +
  geom_boxplot(position = position_dodge(1)) + 
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  stat_compare_means(label="p.signif",comparisons = list(c("<5 Yrs", "5-9 Yrs"), c("<5 Yrs","10-15 Yrs"),
                                                         c("5-9 Yrs", "10-15 Yrs"))) + #add p value to plots
  xlab("Age group") + ylab("Gametocytaemia (Microscopy)") + # Set axis labels
  theme(
    panel.background = element_rect(fill = "white"),
    legend.key  = element_rect(fill = "white"),
    axis.line.x = element_line(colour = "black", size = 1),
    axis.line.y = element_line(colour = "black", size = 1))

gametocytaemiaage_kmlc + stat_median_iqr_text(y.pos = 4.5)

#comparing means of transmission setting vs asexual parasitaemia
asexparasitaemiacohort_kmlc <- ggplot(kmlc_data, aes(x = cohort2, y = log10(pf_mcl+1))) + 
  geom_boxplot(position = position_dodge(1)) +
  geom_jitter(shape = 16, position = position_jitter(0.2)) +
  stat_compare_means(label="p.signif",comparisons = list(c("Junju", "Ngerenya Early"), 
                                                         c("Junju", "Ngerenya Late"),
                                                         c("Ngerenya Early", "Ngerenya Late")),
                     step_increase = .1) + #add p value to plots
  labs(x = "Transmission setting",y = "Asexual parasitaemia (Microscopy)") +
  theme(
    panel.background = element_rect(fill = "white"),
    legend.key  = element_rect(fill = "white"),
    axis.line.x = element_line(colour = "black", size = 1),
    axis.line.y = element_line(colour = "black", size = 1)
  )

asexparasitaemiacohort_kmlc + stat_mean_sd_text(y.pos = 8)

#comparing means of transmission setting vs gametocytaemia
gametocytaemiacohort_kmlc <- ggplot(kmlc_data, aes(x = cohort2, y = log10(gam_mcl+1)
                                      )) + 
  geom_boxplot(position = position_dodge(1)) +
  geom_jitter(shape = 16, position = position_jitter(0.2)) +
  stat_compare_means(label="p.signif",comparisons = list(c("Junju", "Ngerenya Early"), 
                                                         c("Junju", "Ngerenya Late"),
                                                         c("Ngerenya Early", "Ngerenya Late")),
                     step_increase = .1) + #add p value to plots
  
  labs(x = "Transmission setting",y = "Gametocytaemia (Microscopy)") +
  theme(
    panel.background = element_rect(fill = "white"),
    legend.key  = element_rect(fill = "white"),
    axis.line.x = element_line(colour = "black", size = 1),
    axis.line.y = element_line(colour = "black", size = 1)
  )

gametocytaemiacohort_kmlc + stat_mean_sd_text(y.pos = 4.8)


#combine plots
ggarrange(asexparasitaemiaage_kmlc, gametocytaemiaage_kmlc, asexparasitaemiacohort_kmlc
          ,gametocytaemiacohort_kmlc, 
          labels = c("A", "B", "C", "D"), nrow=2, ncol=2) # save as pdf 9x9 inches


#Save the analysed data
write.csv(kmlc_data,
          file = "kmlc_data2.csv",
          row.names = FALSE)

