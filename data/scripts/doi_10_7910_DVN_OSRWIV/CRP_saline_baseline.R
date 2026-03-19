# linear mixed model statistics CRP

# load data
CRP <- read_excel("D:/Meta_AnaOutput_Graphs/Zhu 2022 GAD paper (combined p-WTs)/cell_recurring_probability.xlsx")

# filter out some data that shouldn't be included
CRP_values <- CRP %>%
  filter(session1_mobility_pass == 1 | session2_mobility_pass == 1 ) %>%
  filter(drug == 'saline')

# compare with p-WT (baseline)
CRP_values$genotype <- factor(CRP_values$genotype, levels = c("p-WT", "a5-i-KO", 'a5-pyr-KO'))

# test using LMEM & ANOVA
CRP_values.model= lmer (Cell_Recurring_Probability ~ genotype + (1|animal_ID) + (1|expt_date), data=CRP_values, REML=TRUE)
anova(
  CRP_values.model,
  type = c("III", "II", "I", "3", "2", "1"),
  ddf = "Kenward-Roger")

# perform some plotting
par(mfrow=c(1,3))
CRP_values.model= lmer (Cell_Recurring_Probability ~ genotype + (1|animal_ID) + (1|expt_date), data=CRP_values, REML=TRUE)
plot(residuals(CRP_values.model),las=1, ylim=c(-1,1))
hist(residuals(CRP_values.model),las=1)
qqnorm(residuals(CRP_values.model),las=1)
qqline(residuals(CRP_values.model), col = "red", lwd=2)
summary(CRP_values.model)
sjPlot::plot_model(CRP_values.model, title = "CRP_values ~ genotype + (1|animal_ID) + (1|expt_date)")
sjPlot:: tab_model(CRP_values.model, title = "CRP_values ~ genotype + (1|animal_ID) + (1|expt_date)",
                   auto.label = TRUE, show.stat = TRUE, show.df = TRUE, show.se = TRUE)
