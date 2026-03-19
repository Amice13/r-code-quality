# load data
Mobility <- read_excel("D:/Meta_AnaOutput_Graphs/Zhu 2022 GAD paper (combined p-WTs)/Mobility.xlsx")

# filter out some data that shouldn't be included
Mobility_values <- Mobility %>%
  filter(Session1_Mobility >= 0.15 & Session2_Mobility >= 0.15) %>%
  filter(drug != 'altered contexts') %>%
  filter(genotype == 'p-WT')

# set saline as baseline  
Mobility_values$drug <- factor(Mobility_values$drug, levels = c("saline", "etomidate 2mg/kg", "etomidate 4mg/kg", "etomidate 6mg/kg", "etomidate 7mg/kg", "etomidate 8mg/kg"))

# test using LMEM & ANOVA
Mobility_values.model= lmer (Session1_Mobility ~ drug + (1|animal_ID) + (1|expt_date), data=Mobility_values, REML=TRUE)
anova(
  Mobility_values.model,
  type = c("III", "II", "I", "3", "2", "1"),
  ddf = "Kenward-Roger")

# perform some plotting
par(mfrow=c(1,3))
Mobility_values.model= lmer (Session1_Mobility ~ drug + (1|animal_ID) + (1|expt_date), data=Mobility_values, REML=TRUE)
plot(residuals(Mobility_values.model),las=1, ylim=c(-1,1))
hist(residuals(Mobility_values.model),las=1)
qqnorm(residuals(Mobility_values.model),las=1)
qqline(residuals(Mobility_values.model), col = "red", lwd=2)
summary(Mobility_values.model)
sjPlot::plot_model(Mobility_values.model, title = "Mobility_values ~ drug + (1|animal_ID) + (1|expt_date)")
sjPlot:: tab_model(Mobility_values.model, title = "Mobility_values ~ drug + (1|animal_ID) + (1|expt_date)",
                   auto.label = TRUE, show.stat = TRUE, show.df = TRUE, show.se = TRUE)
