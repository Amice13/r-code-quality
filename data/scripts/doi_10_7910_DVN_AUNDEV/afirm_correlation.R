#Load dataset
afirm_data <-read.csv("afirm_data.csv")

#Load counting packages 
library(plyr)
library(dplyr)

#Demographic characteristics
count(afirm_data, season)
count(afirm_data, age_groups2, wt_var=season) 
count(afirm_data, age_groups) 
count(afirm_datagroups, sex)
count(afirm_data, season)
count(afirm_data,seropositive)
count(afirm_data, aspos, wt_var=age_groups)
count(afirm_data, gampos, wt_var=age_groups)
count(afirm_data, gampos, wt_var=sex)
count(afirm_data, aspos, wt_var=sex)
count(afirm_data, gampos, wt_var=season)
count(afirm_data, aspos, wt_var=season)
count(afirm_data, asexualmcl_pos, wt_var=season)
count(afirm_data, asexualmcl_pos, wt_var=age_groups)
count(afirm_data, asexualmcl_pos, wt_var=sex)
count(afirm_data, gammcl_pos, wt_var=season)
count(afirm_data, gammcl_pos, wt_var=age_groups)
count(afirm_data, gammcl_pos, wt_var=sex)


#change to log scale
afirm_data$gam_responselog <- log10(afirm_data$gam_response)
afirm_data$nasba_18slog <- log10(afirm_data$nasba_18s + 1)
afirm_data$nasba_pfs25log <- log10(afirm_data$nasba_pfs25 + 1)
afirm_data$ama1_responselog <- log10(afirm_data$ama1_response)

#Generating age groups
afirm_data$age_groups2 <- afirm_data$age
afirm_data$age_groups2 <- ifelse((afirm_data$age >=0 &afirm_data$age<=4),"<5 Yrs", afirm_data$age_groups2)
afirm_data$age_groups2 <- ifelse((afirm_data$age >=5 &afirm_data$age<10),"5-9 Yrs", afirm_data$age_groups2)
afirm_data$age_groups2 <- ifelse((afirm_data$age >=10 &afirm_data$age<=15),"10-15 Yrs", afirm_data$age_groups2)
afirm_data$age_groups2 <- ifelse((afirm_data$age >=15 &afirm_data$age<=70),">15 Yrs", afirm_data$age_groups2)


#Load package for ggplots
library(ggpubr)
library(ggplot2)


#comparing means of season vs ama1 responses
ama1responseseason_afirm <- ggplot(afirm_data, aes(x = season, y = ama1_responselog)) + 
  geom_boxplot(position = position_dodge(1)) +
  geom_jitter(shape = 16, position = position_jitter(0.2)) +
  geom_signif(comparisons = list(c("Dry", "Wet")),
              step_increase = .1) +
  labs(x = "Season",y = "Antibody units (AMA1)") +
  theme(
    panel.background = element_rect(fill = "white"),
    legend.key  = element_rect(fill = "white"),
    axis.line.x = element_line(colour = "black", size = 1),
    axis.line.y = element_line(colour = "black", size = 1)
  )

ama1responseseason_afirm + stat_median_iqr_text()

#comparing means of season vs gametocyte responses
gamresponseseason_afirm <- ggplot(afirm_data, aes(x = season, y = gam_responselog)) + 
  geom_boxplot(position = position_dodge(1)) +
  geom_jitter(shape = 16, position = position_jitter(0.2)) +
  geom_signif(comparisons = list(c("Dry", "Wet")),
              step_increase = .1) +
  labs(x = "Season",y = "Antibody units (Gametocyte extract)") +
  theme(
    panel.background = element_rect(fill = "white"),
    legend.key  = element_rect(fill = "white"),
    axis.line.x = element_line(colour = "black", size = 1),
    axis.line.y = element_line(colour = "black", size = 1)
  )
gamresponseseason_afirm + stat_median_iqr_text()


# Correlation analysis

#Load colour package
library(viridis)

#Enable exponential scale to print <0.0001 in plot
options(scipen = 0)

# Correlation asexual parasiemia and gametocytemia
gametocyteasexcor_afirm <- ggscatter(
  afirm_data,
  x = "nasba_pfs25log" ,
  y = "nasba_18slog",
  color = "age_groups2",
  add = "reg.line",
  # Add regressin line
  add.params = list(color = "blue", fill = "lightgray"),
  # Customize reg. line
  conf.int = TRUE
) + scale_color_viridis(discrete = TRUE, option = "D") + stat_cor(aes(),
                                                                  method = "spearman", 
                                                                  p.accuracy = 0.0001,
                                                                  label.x = 0,
                                                                  label.y = 3.65) +
  labs(x = "Gametocytaemia (Pfs25 QT NASBA)",
       y = "Asexual parasitaemia (18s QT NASBA)") +
  theme(
    panel.background = element_rect(fill = "white"),
    legend.key  = element_rect(fill = "white"),
    axis.line.x = element_line(colour = "black", size = 1),
    axis.line.y = element_line(colour = "black", size = 1)
  )

gametocyteasexcor_afirm1 <- ggpar(gametocyteasexcor_afirm, legend.title = "")
gametocyteasexcor_afirm1

# Correlation asexual parasiemia (18s) and gam response
asexgamrescor_afirm <- ggscatter(
  afirm_data,
  x = "nasba_18slog" ,
  y = "gam_responselog",
  color = "age_groups2",
  add = "reg.line",
  # Add regressin line
  add.params = list(color = "blue", fill = "lightgray"),
  # Customize reg. line
  conf.int = TRUE
)  + scale_color_viridis(discrete = TRUE, option = "D") + stat_cor(aes(),
             method = "spearman", p.accuracy = 0.0001,
             label.x = 0.3,
             label.y = 0.5) +
  labs(x = "Asexual parasitaemia (18s QT NASBA)",
       y = "Antibody units (Gametocyte extract)") +
  theme(
    panel.background = element_rect(fill = "white"),
    legend.key  = element_rect(fill = "white"),
    axis.line.x = element_line(colour = "black", size = 1),
    axis.line.y = element_line(colour = "black", size = 1)
  )

asexgamrescor_afirm1 <- ggpar(asexgamrescor_afirm, legend.title = "")
asexgamrescor_afirm1

# Correlation gametocytaemia and gam response
gametocytemiagamrescor_afirm <- ggscatter(
  afirm_data,
  x = "nasba_pfs25log" ,
  y = "gam_responselog",
  color = "age_groups2",
  add = "reg.line",
  # Add regressin line
  add.params = list(color = "blue", fill = "lightgray"),
  # Customize reg. line
  conf.int = TRUE
) + scale_color_viridis(discrete = TRUE, option = "D") + stat_cor(aes(),
             method = "spearman", p.accuracy = 0.0001,
             label.x = 0.3,
             label.y = 0.5) +
  labs(x = "Gametocytaemia (Pfs25 QT NASBA)",
       y = "Antibody units (Gametocyte extract)") +
  theme(
    panel.background = element_rect(fill = "white"),
    legend.key  = element_rect(fill = "white"),
    axis.line.x = element_line(colour = "black", size = 1),
    axis.line.y = element_line(colour = "black", size = 1)
  )

gametocytemiagamrescor_afirm1 <- ggpar(gametocytemiagamrescor_afirm, legend.title = "")
gametocytemiagamrescor_afirm1

# Correlation ama1 response and gam response
ama1gamresponsecor_afirm <- ggscatter(
  afirm_data,
  x = "ama1_responselog" ,
  y = "gam_responselog",
  color = "age_groups",
  add = "reg.line",
  # Add regressin line
  add.params = list(color = "blue", fill = "lightgray"),
  # Customize reg. line
  conf.int = TRUE
) + scale_color_viridis(discrete = TRUE, option = "D") + stat_cor(aes(),
             method = "spearman", p.accuracy = 0.0001,
             label.x = 0,
             label.y = 4.5) +
  labs(x = "Antibody units (AMA1)",
       y = "Antibody units (Gametocyte extract)") +
  theme(
    panel.background = element_rect(fill = "white"),
    legend.key  = element_rect(fill = "white"),
    axis.line.x = element_line(colour = "black", size = 1),
    axis.line.y = element_line(colour = "black", size = 1)
  )

  
#Disable exponential scale
options(scipen = 999)

#to reset graphics in r due to inefficient plot displays
dev.off()


#Plots in the supplementary material

#Parasitaemia by age in AFIRM

#Compare means with asexual parasitemia vs age (microscopy)
microscopy_asexualparasiteage_afirm <- ggplot(aes(x=age_groups, y= log10(asexual_mcl)), data = afirm_data) +
  geom_boxplot(position = position_dodge(1)) +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  stat_compare_means(label="p.signif", comparisons = list(c("<5 Yrs", "5-15 Yrs"), c("5-15 Yrs",">15 Yrs"),
                                                          c("<5 Yrs", ">15 Yrs"))) +#add p value to plots
  xlab("Age group") + ylab("Asexual parasitaemia (Microscopy)") + # Set axis labels
  theme(
    panel.background = element_rect(fill = "white"),
    legend.key  = element_rect(fill = "white"),
    axis.line.x = element_line(colour = "black", size = 1),
    axis.line.y = element_line(colour = "black", size = 1))

microscopy_asexualparasiteage_afirm + stat_median_iqr_text(y.pos = 6)

#Compare means gametocytemia vs age (microscopy)
microscopy_gametocytemiaage_afirm <- ggplot(aes(x=age_groups, y= log10(gam_mcl)), data = afirm_data) +
  geom_boxplot(position = position_dodge(1)) + 
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  stat_compare_means(label="p.signif", comparisons = list(c("<5 Yrs", "5-15 Yrs"), c("5-15 Yrs",">15 Yrs"),
                                                          c("<5 Yrs", ">15 Yrs"))) + #add p value to plots
  xlab("Age group") + ylab("Gametocytemia (Microscopy)") + # Set axis labels
  theme(
    panel.background = element_rect(fill = "white"),
    legend.key  = element_rect(fill = "white"),
    axis.line.x = element_line(colour = "black", size = 1),
    axis.line.y = element_line(colour = "black", size = 1))

#Compare means with asexual parasitemia vs age (18s QT NASBA)
asexualparasiteage_afirm <- ggplot(aes(x=age_groups, y= log10(nasba_18s)), data = afirm_data) +
  geom_boxplot(position = position_dodge(1)) +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  stat_compare_means(label="p.signif", comparisons = list(c("<5 Yrs", "5-15 Yrs"), c("5-15 Yrs",">15 Yrs"),
                                                          c("<5 Yrs", ">15 Yrs"))) +#add p value to plots
  xlab("Age group") + ylab("Asexual parasitaemia (18s QT NASBA)") + # Set axis labels
  theme(
    panel.background = element_rect(fill = "white"),
    legend.key  = element_rect(fill = "white"),
    axis.line.x = element_line(colour = "black", size = 1),
    axis.line.y = element_line(colour = "black", size = 1))

asexualparasiteage_afirm + stat_median_iqr_text(y.pos = 6)

#Compare means gametocytemia vs age (Pfs25 QT NASBA)
gametocytemiaage_afirm <- ggplot(aes(x=age_groups, y= log10(nasba_pfs25)), data = afirm_data) +
  geom_boxplot(position = position_dodge(1)) + 
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  stat_compare_means(label="p.signif", comparisons = list(c("<5 Yrs", "5-15 Yrs"), c("5-15 Yrs",">15 Yrs"),
                                                          c("<5 Yrs", ">15 Yrs"))) + #add p value to plots
  xlab("Age group") + ylab("Gametocytaemia (Pfs25 QT NASBA)") + # Set axis labels
  theme(
    panel.background = element_rect(fill = "white"),
    legend.key  = element_rect(fill = "white"),
    axis.line.x = element_line(colour = "black", size = 1),
    axis.line.y = element_line(colour = "black", size = 1))

#Combine plots
ggarrange(microscopy_asexualparasiteage_afirm, microscopy_gametocytemiaage_afirm, 
          asexualparasiteage_afirm,gametocytemiaage_afirm, 
          labels = c("A", "B", "C", "D"), nrow = 2, ncol = 2)


#Parasitaemia by season in AFIRM

#comparing means of season vs asexual parasitemia
microscopy_asexparasitemiaseason_afirm <- ggplot(afirm_data, aes(x = season, y = log10(asexual_mcl))) + 
  geom_boxplot(position = position_dodge(1)) +
  geom_jitter(shape = 16, position = position_jitter(0.2)) +
  geom_signif(comparisons = list(c("Dry", "Wet")),
              step_increase = .1) +
  labs(x = "Season",y = "Asexual parasitaemia (Microscopy)") +
  theme(
    panel.background = element_rect(fill = "white"),
    legend.key  = element_rect(fill = "white"),
    axis.line.x = element_line(colour = "black", size = 1),
    axis.line.y = element_line(colour = "black", size = 1)
  )

#comparing means of season vs asexual parasitemia (18s)
asexparasitemiaseason_afirm <- ggplot(afirm_data, aes(x = season, y = log10(nasba_18s))) + 
  geom_boxplot(position = position_dodge(1)) +
  geom_jitter(shape = 16, position = position_jitter(0.2)) +
  geom_signif(comparisons = list(c("Dry", "Wet")),
              step_increase = .1) +
  labs(x = "Season",y = "Asexual parasitaemia (18s QT NASBA)") +
  theme(
    panel.background = element_rect(fill = "white"),
    legend.key  = element_rect(fill = "white"),
    axis.line.x = element_line(colour = "black", size = 1),
    axis.line.y = element_line(colour = "black", size = 1)
  )

asexparasitemiaseason_afirm + stat_median_iqr_text(y.pos =4.5)

#comparing means of season vs gametocytemia (Microscopy)
microscopy_gametocytemiaseason_afirm <- ggplot(afirm_data, aes(x = season, y = log10(gam_mcl))) + 
  geom_boxplot(position = position_dodge(1)) +
  geom_jitter(shape = 16, position = position_jitter(0.2)) +
  geom_signif(comparisons = list(c("Dry", "Wet")),
              step_increase = .1) +
  labs(x = "Season",y = "Gametocytaemia (Microscopy)") +
  theme(
    panel.background = element_rect(fill = "white"),
    legend.key  = element_rect(fill = "white"),
    axis.line.x = element_line(colour = "black", size = 1),
    axis.line.y = element_line(colour = "black", size = 1)
  )

#comparing means of season vs gametocytemia (Pfs25)
gametocytemiaseason_afirm <- ggplot(afirm_data, aes(x = season, y = log10(nasba_pfs25))) + 
  geom_boxplot(position = position_dodge(1)) +
  geom_jitter(shape = 16, position = position_jitter(0.2)) +
  geom_signif(comparisons = list(c("Dry", "Wet")),
              step_increase = .1) +
  labs(x = "Season",y = "Gametocytaemia (Pfs25 QT NASBA)") +
  theme(
    panel.background = element_rect(fill = "white"),
    legend.key  = element_rect(fill = "white"),
    axis.line.x = element_line(colour = "black", size = 1),
    axis.line.y = element_line(colour = "black", size = 1)
  )

gametocytemiaseason_afirm + stat_median_iqr_text(y.pos = 4)

         
#Save the analysed data
write.csv(afirm_data2,
          file = "afirm_data2.csv",
          row.names = FALSE)
