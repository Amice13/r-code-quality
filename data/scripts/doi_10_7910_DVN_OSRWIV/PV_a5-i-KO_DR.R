# linear mixed model statistics for PV correlations

# load data
PV_correlation <- read_excel("D:/Meta_AnaOutput_Graphs/Zhu 2022 GAD paper (combined p-WTs)/PV_correlation.xlsx")

# filter out data based on mobility of the animal
PV_values <- PV_correlation %>%
  filter(session1_mobility_pass == 1 & session2_mobility_pass == 1) %>%
  filter(drug != 'altered contexts') %>%
  filter(genotype == 'a5-i-KO')

# make sure you compare with saline baseline  
PV_values$drug <- factor(PV_values$drug, levels = c("saline", "etomidate 2mg/kg", "etomidate 4mg/kg", "etomidate 6mg/kg", "etomidate 7mg/kg", "etomidate 8mg/kg"))

# test using LMEM & ANOVA
PV_values.model= lmer (PV_values ~ drug + (1|animal_ID) + (1|animal_ID:expt_date), data=PV_values, REML=FALSE)
anova(
  PV_values.model,
  type = c("III", "II", "I", "3", "2", "1"),
  ddf = c("Satterthwaite", "Kenward-Roger", "lme4"))

# perform some plotting
par(mfrow=c(1,3))
PV_values.model= lmer (PV_values ~ drug + (1|animal_ID) + (1|animal_ID:expt_date), data=PV_values, REML=TRUE)
plot(residuals(PV_values.model),las=1, ylim=c(-2,2))
hist(residuals(PV_values.model),las=1)
qqnorm(residuals(PV_values.model),las=1)
qqline(residuals(PV_values.model), col = "red", lwd=2)
summary(PV_values.model)
sjPlot::plot_model(PV_values.model, title = "PV_values ~ drug + (1|animal_ID) + (1|animal_ID:expt_date)")
sjPlot:: tab_model(PV_values.model, title = "PV_values ~ drug + (1|animal_ID) + (1|animal_ID:expt_date)",
                   auto.label = TRUE, show.stat = TRUE, show.df = TRUE, show.se = TRUE)
