# linear mixed model statistics for RM correlations

# load data
RM_correlation <- read_excel("D:/Meta_AnaOutput_Graphs/Zhu 2022 GAD paper (combined p-WTs)/RM_correlation.xlsx")

# filter out data based on mobility of the animal
RM_values <- RM_correlation %>%
  filter(session1_mobility_pass == 1 & session2_mobility_pass == 1) %>%
  filter(drug == 'saline' | drug == 'altered contexts')

# compare with p-WT (baseline)
RM_values$genotype <- factor(RM_values$genotype, levels = c("p-WT", "a5-i-KO", 'a5-pyr-KO'))

# make sure you compare with saline baseline  
RM_values$drug <- factor(RM_values$drug, levels = c("saline", "altered contexts"))

# test using LMEM & ANOVA
RM_values.model= lmer (RM_values ~ drug * genotype + (1|animal_ID) + (1|animal_ID:expt_date) + (1|cell_ID:animal_ID), data=RM_values, REML=FALSE)
anova(
  RM_values.model,
  type = c("III", "II", "I", "3", "2", "1"),
  ddf = c("Satterthwaite", "Kenward-Roger", "lme4"))

# perform some plotting
par(mfrow=c(1,3))
RM_values.model= lmer (RM_values ~ drug * genotype + (1|animal_ID) + (1|animal_ID:expt_date) + (1|cell_ID:animal_ID), data=RM_values, REML=TRUE)
plot(residuals(RM_values.model),las=1, ylim=c(-2,2))
hist(residuals(RM_values.model),las=1)
qqnorm(residuals(RM_values.model),las=1)
qqline(residuals(RM_values.model), col = "red", lwd=2)
summary(RM_values.model)
sjPlot::plot_model(RM_values.model, title = "RM_values ~ drug * genotype + (1|animal_ID) + (1|animal_ID:expt_date) + (1|cell_ID:animal_ID)") + ylim(-0.3,0.3)
sjPlot:: tab_model(RM_values.model, title = "RM_values ~ drug * genotype + (1|animal_ID) + (1|animal_ID:expt_date) + (1|cell_ID:animal_ID)",
                   auto.label = TRUE, show.stat = TRUE, show.df = TRUE, show.se = TRUE)
