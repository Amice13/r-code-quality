#Load data
afirm_kmlc <- read.csv("afirm_kmlc.csv")

#Viewing dataset
View(afirm_kmlc)

#Convert to log scale 
afirm_kmlc$gam_responselog <- log10(afirm_kmlc$gam_response)
afirm_kmlc$ama1_responselog <- log10(afirm_kmlc$ama1_response)
afirm_kmlc$gam_mcllog <- log10(afirm_kmlc$gam_mcl+1)
afirm_kmlc$asexual_mcllog <- log10(afirm_kmlc$asexual_mcl+1)
afirm_kmlc$asexualmcl_pos<-ifelse(afirm_kmlc$asexual_mcl > 0, 1, 0) # asexualmcl is asexual parasite density by microscopy
afirm_kmlc$gammcl_pos<-ifelse(afirm_kmlc$gam_mcl > 0, 1, 0)# gammcl is gametocyte density by microscopy

#save data
.csv(afirm_kmlc,
          file = "afirm_kmlc2.csv",
          row.names = FALSE)


#Load packages for ggplots
library(ggpubr)

#Generating age groups
afirm_kmlc$age_groups <- afirm_kmlc$age
afirm_kmlc$age_groups <- ifelse((afirm_kmlc$age >=0 &afirm_kmlc$age<=4),"<5 Yrs", afirm_kmlc$age_groups)
afirm_kmlc$age_groups <- ifelse((afirm_kmlc$age >=5 &afirm_kmlc$age<10),"5-9 Yrs", afirm_kmlc$age_groups)
afirm_kmlc$age_groups <- ifelse((afirm_kmlc$age >=10 &afirm_kmlc$age<=15),"10-15 Yrs", afirm_kmlc$age_groups)
afirm_kmlc$age_groups <- ifelse((afirm_kmlc$age >=15 &afirm_kmlc$age<=70),">15 Yrs", afirm_kmlc$age_groups)


# To rearrage the age groups in the plot
afirm_kmlc$age_groups <-
  factor(afirm_kmlc$age_groups,
         levels = c("<5 Yrs", "5-9 Yrs","10-15 Yrs", ">15 Yrs"))

#gam response vs age groups
gamresponseage_combined <- ggplot(afirm_kmlc, aes(x=age_groups, y= log10(gam_response))) +
  geom_boxplot(position = position_dodge(1)) + 
  geom_jitter(shape=16, position=position_jitter(0.2)) + 
  scale_fill_viridis(discrete = TRUE, option = "D") + #specifying the colour range 
  stat_compare_means(label="p.signif",comparisons = list(c("<5 Yrs", "5-9 Yrs"), c("<5 Yrs","10-15 Yrs"),
                                                         c("<5 Yrs", ">15 Yrs"),c("5-9 Yrs", "10-15 Yrs"),
                                                         c("5-9 Yrs",">15 Yrs"),
                                                         c("10-15 Yrs", ">15 Yrs") )) + #add p value to plots
  xlab("Age group") + ylab("Antibody units (Gametocyte extract)") + # Set axis labels
  theme(
    panel.background = element_rect(fill = "white"),
    legend.key  = element_rect(fill = "white"),
    axis.line.x = element_line(colour = "black", size = 1),
    axis.line.y = element_line(colour = "black", size = 1))

# to get median and iqr
library(EnvStats)

gamresponseage_combined + stat_median_iqr_text(y.pos = 11)


#ama1 vs age
ama1responseage_combined <- ggplot(aes(x=age_groups, y= log10(ama1_response)), data = afirm_kmlc) +
  geom_boxplot(position = position_dodge(1)) +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  stat_compare_means(label="p.signif",comparisons = list(c("<5 Yrs", "5-9 Yrs"), c("<5 Yrs","10-15 Yrs"),
                                                         c("<5 Yrs", ">15 Yrs"),c("5-9 Yrs", "10-15 Yrs"),
                                                         c("5-9 Yrs",">15 Yrs"),
                                                         c("10-15 Yrs", ">15 Yrs") )) + #add p value to plots
  xlab("Age group") + ylab("Antibody units (AMA1)") + # Set axis labels
  theme(
    panel.background = element_rect(fill = "white"),
    legend.key  = element_rect(fill = "white"),
    axis.line.x = element_line(colour = "black", size = 1),
    axis.line.y = element_line(colour = "black", size = 1))

ama1responseage_combined + stat_median_iqr_text(y.pos = 11)

# microscopic gametocytaemia vs age
gametocytaemiaage_combined <- ggplot(aes(x=age_groups, y= log10(gam_mcl)), data = afirm_kmlc) +
  geom_boxplot(position = position_dodge(1)) + 
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  stat_compare_means(label="p.label",hide.NS = T, comparisons = list(c("<5 Yrs", "5-9 Yrs"), c("<5 Yrs","10-15 Yrs"),
                                                                     c("<5 Yrs", ">15 Yrs"),c("5-9 Yrs", "10-15 Yrs"),
                                                                     c("5-9 Yrs",">15 Yrs"),
                                                                     c("10-15 Yrs", ">15 Yrs") )) + #add p value to plots
  xlab("Age group") + ylab("Gametocytaemia (Microscopy)") + # Set axis labels
  theme(
    panel.background = element_rect(fill = "white"),
    legend.key  = element_rect(fill = "white"),
    axis.line.x = element_line(colour = "black", size = 1),
    axis.line.y = element_line(colour = "black", size = 1))

gametocytaemiaage_combined + stat_median_iqr_text()


#microscopic asex parasitaemia vs age
asexparasitaemia_combined <- ggplot(aes(x=age_groups, y= log10(asexual_mcl)), data = afirm_kmlc) +
  geom_boxplot(position = position_dodge(1)) +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  stat_compare_means(label="p.signif",comparisons = list(c("<5 Yrs", "5-9 Yrs"), c("<5 Yrs","10-15 Yrs"),
                                                         c("<5 Yrs", ">15 Yrs"),c("5-9 Yrs", "10-15 Yrs"),
                                                         c("5-9 Yrs",">15 Yrs"),
                                                         c("10-15 Yrs", ">15 Yrs") )) + #add p value to plots
  xlab("Age group") + ylab("Asexual parasitaemia (Microscopy)") + # Set axis labels
  theme(
    panel.background = element_rect(fill = "white"),
    legend.key  = element_rect(fill = "white"),
    axis.line.x = element_line(colour = "black", size = 1),
    axis.line.y = element_line(colour = "black", size = 1))

asexparasitaemia_combined + stat_mean_sd_text(y.pos = 12)

#Combine the plots
ggarrange(asexparasitaemia_combined, gametocytaemiaage_combined, ama1responseage_combined
          ,gamresponseage_combined, 
          labels = c("A", "B", "C", "D"), nrow=2, ncol=2) # save as pdf 9x9 inches.


#Correlation analysis
#Disable exponential scale........# to print p<0.0001 on the correlation plot
options(scipen = 999)

#Load colour package
library(viridis)

#correlate gam response vs ama1
ama1gamres_combined <- ggscatter(
  afirm_kmlc,
  x = "ama1_responselog" ,
  y = "gam_responselog",
  color = "age_groups", 
  add = "reg.line",
  # Add regressin line
  add.params = list(color = "blue", fill = "lightgray"),
  # Customize reg. line
  conf.int = TRUE
) + scale_color_viridis(discrete = TRUE, option = "D") +
  stat_cor(aes(),
             method = "spearman", p.accuracy = 0.0001,
             label.x = 0,
             label.y = 0.5) + 
  labs(x = "Antibody units (AMA1)",
       y = "Antibody units (Gametocyte extract)"
       ) + 
  theme(
    panel.background = element_rect(fill = "white"),
    legend.key  = element_rect(fill = "white"),
    axis.line.x = element_line(colour = "black", size = 1),
    axis.line.y = element_line(colour = "black", size = 1)
  )

ama1gamres_combined1 <- ggpar(ama1gamres_combined, legend.title = "Age")
ama1gamres_combined1
ggarrange(ama1gamres_combined1, legend = "bottom") #to print legend at the bottom

#correlate asexual parasitaemia and gametocytaemia
asexgametocytaemiacor_combined <- ggscatter(
  afirm_kmlc,
  x = "asexual_mcllog" ,
  y = "gam_mcllog",
  color = "age_groups",
  add = "reg.line",
  # Add regressin line
  add.params = list(color = "blue", fill = "lightgray"),
  # Customize reg. line
  conf.int = TRUE
) + scale_color_viridis(discrete = TRUE, option = "D") + stat_cor(aes(),
             method = "spearman", p.accuracy = 0.0001,
             label.x = 0.3,
             label.y = 3) +
  labs(x = "Asexual parasitaemia (Microscopy)",
       y = "Gametocytaemia (Microscopy)") +
  theme(
    panel.background = element_rect(fill = "white"),
    legend.key  = element_rect(fill = "white"),
    axis.line.x = element_line(colour = "black", size = 1),
    axis.line.y = element_line(colour = "black", size = 1)
  )

asexgametocytaemiacor_combined1 <- ggpar(asexgametocytaemiacor_combined, legend.title = "")
asexgametocytaemiacor_combined1

#correlate asexual parasitaemia and gam response
asexgamresponsecor_combined <- ggscatter(
  afirm_kmlc,
  x = "asexual_mcllog" ,
  y = "gam_responselog",
  color = "age_groups",
  add = "reg.line",
  # Add regressin line
  add.params = list(color = "blue", fill = "lightgray"),
  # Customize reg. line
  conf.int = TRUE
) + scale_color_viridis(discrete = TRUE, option = "D") + stat_cor(aes(),
             method = "spearman", p.accuracy = 0.01,
             label.x = 0.3,
             label.y = 5) +
  labs(x = "Asexual parasitaemia (Microscopy)",
       y = "Antibody units (Gametocyte extract)") +
  theme(
    panel.background = element_rect(fill = "white"),
    legend.key  = element_rect(fill = "white"),
    axis.line.x = element_line(colour = "black", size = 1),
    axis.line.y = element_line(colour = "black", size = 1)
  )

asexgamresponsecor_combined1 <- ggpar(asexgamresponsecor_combined,legend.title = "")
asexgamresponsecor_combined1

#correlate asexual parasitaemia and gam response
gametocytaemiagamresponsecor_combined <- ggscatter(
  afirm_kmlc,
  x = "gam_mcllog" ,
  y = "gam_responselog",
  color = "age_groups",
  add = "reg.line",
  # Add regressin line
  add.params = list(color = "blue", fill = "lightgray"),
  # Customize reg. line
  conf.int = TRUE
) + scale_color_viridis(discrete = TRUE, option = "D")+ stat_cor(aes(),
             method = "spearman", p.accuracy = 0.01,
             label.x = 0.3,
             label.y = 5) +
  labs(x = "Gametocytaemia (Microscopy)",
       y = "Antibody units (Gametocyte extract)") +
  theme(
    panel.background = element_rect(fill = "white"),
    legend.key  = element_rect(fill = "white"),
    axis.line.x = element_line(colour = "black", size = 1),
    axis.line.y = element_line(colour = "black", size = 1)
  )

gametocytaemiagamresponsecor_combined1 <- ggpar(gametocytaemiagamresponsecor_combined, legend.title = "")
gametocytaemiagamresponsecor_combined1


#Combine plots
ggarrange(asexgametocytaemiacor_combined1,asexgamresponsecor_combined1,
          gametocytaemiagamresponsecor_combined1, 
          labels = c("A", "B", "C"), nrow=1, ncol=3, common.legend = TRUE, legend = "bottom",
          legend.grob = get_legend(ama1gamres_combined1)) # save as pdf 9x9 inches.


#Enable exponential scale after finishing correlation plots
options(scipen = 0)
