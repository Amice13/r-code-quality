# Perform LMEM statistics on pMI and MI data (place cell proportions)

# specify directory to the data file (excel/csv)
# change to match your local machine
MI_and_pMI <- read_excel("D:/Meta_AnaOutput_Graphs/Zhu 2022 GAD paper (combined p-WTs)/MI_and_pMI.xlsx")

# get data and filter for only first session (day1/AM)
# filter out data where mobility is below a certain threshold
MIfreq <- MI_and_pMI
MI_data_AM_day1_only <- MIfreq %>%
  filter(expt_time == "day1" | expt_time == "AM") %>%
  filter(Mobility_Pass == 1) %>%
  filter(genotype != 'p-WT')
  filter(drug == 'saline')

# filter out data that you don't need for place cell statistics
MIfreq <- MI_data_AM_day1_only %>%
  nest_by(session,genotype,animal_ID,drug,expt_date,.keep=TRUE) %>%
  dplyr::select(session,genotype,animal_ID,drug,expt_date)

# compare with p-WT (baseline)
MIfreq$genotype <- factor(MIfreq$genotype, levels = c("a5-i-KO", 'a5-pyr-KO'))

# avoid bugs
MIfreq <- MIfreq %>%
  group_by(genotype) %>%
  add_column(lowMIfreq = 0) # add empty column to fill in later with frequencies

# p-value is set as variable lowMI
lowMI = 0.05

numRowsTable = NROW(MIfreq)
for (i in 1:numRowsTable){
  df <- MI_data_AM_day1_only %>%
    filter(session == as.character(MIfreq[i,1]))
  freq = NROW(filter(df, MI_p_values<lowMI)) / NROW(df)
  MIfreq[i,6]=freq
}

# make table of average values of pMI by animal_ID under each condition
pMItable <- aggregate(lowMIfreq ~ genotype:animal_ID:drug, data = MIfreq, mean, 
                      simplify = TRUE)

#################################################################

# test for interaction in mixed effects model
MI.model= lmer (lowMIfreq ~ genotype + (1|animal_ID) + (1|expt_date), data=MIfreq, REML=FALSE)
anova(
  MI.model,
  type = c("III", "II", "I", "3", "2", "1"),
  ddf = c("Satterthwaite", "Kenward-Roger", "lme4"))

# generate figs
MI.model= lmer(lowMIfreq ~ genotype + (1|animal_ID) + (1|expt_date), data=MIfreq, REML=TRUE)
par(mfrow=c(1,3))
plot(residuals(MI.model),las=1, ylim=c(-2,2))
hist(residuals(MI.model),las=1)
qqnorm(residuals(MI.model),las=1)
qqline(residuals(MI.model), col = "red", lwd=2)
coef(MI.model)
summary(MI.model)
sjPlot::plot_model(MI.model, title = "MI ~ genotype  + (1|animal_ID) + (1|expt_date)")
sjPlot:: tab_model(MI.model, title = "MI ~  (1|animal_ID) + (1|expt_date)")