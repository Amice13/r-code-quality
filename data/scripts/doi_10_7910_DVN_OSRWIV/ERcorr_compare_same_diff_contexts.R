# linear mixed model statistics ERcorr

# load data
ERcorr <- read_excel("D:/Meta_AnaOutput_Graphs/Zhu 2022 GAD paper (combined p-WTs)/ER_correlation.xlsx")

# filter out some data that shouldn't be included
ERcorr_values <- ERcorr %>%
  filter(session1_mobility_pass == 1) %>%
  filter(drug == 'saline' | drug == 'altered contexts')

# compare with p-WT (baseline)
ERcorr_values$genotype <- factor(ERcorr_values$genotype, levels = c("p-WT", "a5-i-KO", 'a5-pyr-KO'))

# make sure you compare with saline baseline  
ERcorr_values$drug <- factor(ERcorr_values$drug, levels = c("saline", "altered contexts"))

# test using LMEM & ANOVA
ERcorr_values.model= lmer (Event_Rate_Correlation ~ drug * genotype + (1|animal_ID) + (1|expt_date), data=ERcorr_values, REML=TRUE)
anova(
  ERcorr_values.model,
  type = c("III", "II", "I", "3", "2", "1"),
  ddf = "Kenward-Roger")

# perform some plotting
par(mfrow=c(1,3))
ERcorr_values.model= lmer (Event_Rate_Correlation ~ drug * genotype + (1|animal_ID) + (1|expt_date), data=ERcorr_values, REML=TRUE)
plot(residuals(ERcorr_values.model),las=1, ylim=c(-1,1))
hist(residuals(ERcorr_values.model),las=1)
qqnorm(residuals(ERcorr_values.model),las=1)
qqline(residuals(ERcorr_values.model), col = "red", lwd=2)
summary(ERcorr_values.model)
sjPlot::plot_model(ERcorr_values.model, title = "ERcorr_values ~ drug * genotype + (1|animal_ID) + (1|expt_date)")
sjPlot:: tab_model(ERcorr_values.model, title = "ERcorr_values ~ drug * genotype + (1|animal_ID) + (1|expt_date)",
                   auto.label = TRUE, show.stat = TRUE, show.df = TRUE, show.se = TRUE)
