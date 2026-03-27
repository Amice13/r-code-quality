# linear mixed model statistics for PV correlations

# load data
PV_correlation <- read_excel("D:/Meta_AnaOutput_Graphs/Zhu 2022 GAD paper (combined p-WTs)/PV_correlation.xlsx")

# filter out data based on mobility of the animal
PV_values <- PV_correlation %>%
  filter(session1_mobility_pass == 1 & session2_mobility_pass == 1) %>%
  filter(drug == 'saline' | drug == 'altered contexts')

# compare with p-WT (baseline)
PV_values$genotype <- factor(PV_values$genotype, levels = c("p-WT", "a5-i-KO", 'a5-pyr-KO'))

# make sure you compare with saline baseline  
PV_values$drug <- factor(PV_values$drug, levels = c("saline", "altered contexts"))

# test using LMEM & ANOVA
PV_values.model= lmer (PV_values ~ drug * genotype + (1|animal_ID) + (1|animal_ID:expt_date), data=PV_values, REML=FALSE)
anova(
  PV_values.model,
  type = c("III", "II", "I", "3", "2", "1"),
  ddf = c("Satterthwaite", "Kenward-Roger", "lme4"))

# perform some plotting
par(mfrow=c(1,3))
PV_values.model= lmer (PV_values ~ drug * genotype + (1|animal_ID) + (1|animal_ID:expt_date), data=PV_values, REML=TRUE)
plot(residuals(PV_values.model),las=1, ylim=c(-2,2))
hist(residuals(PV_values.model),las=1)
qqnorm(residuals(PV_values.model),las=1)
qqline(residuals(PV_values.model), col = "red", lwd=2)
summary(PV_values.model)
sjPlot::plot_model(PV_values.model, title = "PV_values ~ drug * genotype + (1|animal_ID) + (1|animal_ID:expt_date)") + ylim (-0.3, 0.3)
sjPlot:: tab_model(PV_values.model, title = "PV_values ~ drug * genotype + (1|animal_ID) + (1|animal_ID:expt_date)",
                   auto.label = TRUE, show.stat = TRUE, show.df = TRUE, show.se = TRUE)
