# linear mixed model statistics mean_ER

# load data
mean_ER <- read_excel("D:/Meta_AnaOutput_Graphs/Zhu 2022 GAD paper (combined p-WTs)/mean_ER.xlsx")

# filter out some data that shouldn't be included
mean_ER_values <- mean_ER %>%
  filter(session1_mobility_pass == 1 & session2_mobility_pass == 1) %>%
  filter(drug == 'saline')

# compare with p-WT (baseline)
mean_ER_values$genotype <- factor(mean_ER_values$genotype, levels = c("p-WT", "a5-i-KO", 'a5-pyr-KO'))

# test using LMEM & ANOVA
mean_ER_values.model= lmer (Session1_Mean_Event_Rate ~ genotype + (1|animal_ID) + (1|expt_date), data=mean_ER_values, REML=TRUE)
anova(
  mean_ER_values.model,
  type = c("III", "II", "I", "3", "2", "1"),
  ddf = "Kenward-Roger")

# perform some plotting
par(mfrow=c(1,3))
mean_ER_values.model= lmer (Session1_Mean_Event_Rate ~ genotype + (1|animal_ID) + (1|expt_date), data=mean_ER_values, REML=TRUE)
plot(residuals(mean_ER_values.model),las=1, ylim=c(-1,1))
hist(residuals(mean_ER_values.model),las=1)
qqnorm(residuals(mean_ER_values.model),las=1)
qqline(residuals(mean_ER_values.model), col = "red", lwd=2)
summary(mean_ER_values.model)
sjPlot::plot_model(mean_ER_values.model, title = "mean_ER_values ~ genotype + (1|animal_ID) + (1|expt_date)")
sjPlot:: tab_model(mean_ER_values.model, title = "mean_ER_values ~ genotype + (1|animal_ID) + (1|expt_date)",
                   auto.label = TRUE, show.stat = TRUE, show.df = TRUE, show.se = TRUE)