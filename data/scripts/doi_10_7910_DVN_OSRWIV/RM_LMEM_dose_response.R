# linear mixed model statistics for RM correlations

# load data
RM_correlation <- read_excel("D:/Meta_AnaOutput_Graphs/Zhu 2022 GAD paper (combined p-WTs)/RM_correlation.xlsx")

# filter out data based on animal's mobility
RMvalues <- RM_correlation %>%
  filter(session1_mobility_pass == 1 & session2_mobility_pass == 1) %>%
  filter(drug != 'altered contexts')

# compare with p-WT (baseline)
RMvalues$genotype <- factor(RMvalues$genotype, levels = c("p-WT", "a5-i-KO", 'a5-pyr-KO'))

# make sure you compare with saline baseline  
RMvalues$drug <- factor(RMvalues$drug, levels = c("saline", "etomidate 2mg/kg", "etomidate 4mg/kg", "etomidate 6mg/kg", "etomidate 7mg/kg", "etomidate 8mg/kg"))

# test using LMEM & ANOVA
RM_values.model= lmer (RM_values ~ drug * genotype + (1|animal_ID) + (1|animal_ID:expt_date) + (1|cell_ID:animal_ID), data=RMvalues, REML=FALSE)
anova(
  RM_values.model,
  type = c("III", "II", "I", "3", "2", "1"),
  ddf = c("Satterthwaite", "Kenward-Roger", "lme4"))

# perform some plotting
par(mfrow=c(1,3))
RM_values.model= lmer (RM_values ~ drug * genotype + (1|animal_ID) + (1|animal_ID:expt_date) + (1|cell_ID:animal_ID), data=RMvalues, REML=TRUE)
plot(residuals(RM_values.model),las=1, ylim=c(-2,2))
hist(residuals(RM_values.model),las=1)
qqnorm(residuals(RM_values.model),las=1)
qqline(residuals(RM_values.model), col = "red", lwd=2)
summary(RM_values.model)
sjPlot::plot_model(RM_values.model, title = "RM_values ~ drug * genotype + (1|animal_ID) + (1|animal_ID:expt_date) + (1|cell:animal_ID)") + ylim(-0.5, 0.5)
sjPlot:: tab_model(RM_values.model, title = "RM_values ~ drug * genotype + (1|animal_ID) + (1|animal_ID:expt_date) + (1|cell:animal_ID)",
                   auto.label = TRUE, show.stat = TRUE, show.df = TRUE, show.se = TRUE)
