# linear mixed model statistics for RM correlations

# load data
RM_correlation <- read_excel("D:/Meta_AnaOutput_Graphs/Zhu 2022 GAD paper (combined p-WTs)/RM_correlation.xlsx")

# filter out data based on mobility of the animal
RM_values <- RM_correlation %>%
  filter(session1_mobility_pass == 1 & session2_mobility_pass == 1) %>%
  filter(drug != 'altered contexts') %>%
  filter(genotype == 'a5-i-KO')

# make sure you compare with saline baseline  
RM_values$drug <- factor(RM_values$drug, levels = c("saline", "etomidate 2mg/kg", "etomidate 4mg/kg", "etomidate 6mg/kg", "etomidate 7mg/kg", "etomidate 8mg/kg"))

# test using LMEM & ANOVA
RM_values.model= lmer (RM_values ~ drug + (1|animal_ID) + (1|animal_ID:expt_date), data=RM_values, REML=FALSE)
anova(
  RM_values.model,
  type = c("III", "II", "I", "3", "2", "1"),
  ddf = c("Satterthwaite", "Kenward-Roger", "lme4"))

# perform some plotting
par(mfrow=c(1,3))
RM_values.model= lmer (RM_values ~ drug + (1|animal_ID) + (1|animal_ID:expt_date), data=RM_values, REML=TRUE)
plot(residuals(RM_values.model),las=1, ylim=c(-2,2))
hist(residuals(RM_values.model),las=1)
qqnorm(residuals(RM_values.model),las=1)
qqline(residuals(RM_values.model), col = "red", lwd=2)
summary(RM_values.model)
sjPlot::plot_model(RM_values.model, title = "RM_values ~ drug + (1|animal_ID) + (1|animal_ID:expt_date)")
sjPlot:: tab_model(RM_values.model, title = "RM_values ~ drug + (1|animal_ID) + (1|animal_ID:expt_date)",
                   auto.label = TRUE, show.stat = TRUE, show.df = TRUE, show.se = TRUE)
