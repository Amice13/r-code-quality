#================================
# Recommendations for Additional Imaging on Head and Neck Imaging Examinations: 
#   Interradiologist Variation and Associated Factors
# PI: Jeffrey P Guenette MD MPH
# Funding: Association of University Radiologists, AHRQ R18 HS029348
# Publication: https://ajronline.org/doi/10.2214/AJR.23.30511
# Associated data files in Harvard Dataverse: https://doi.org/10.7910/DVN/O6JJT6 
#================================


### CLEAR ENVIRONMENT
rm(list = ls())
gc()


### LOAD LIBRARIES
library(tidyverse)  # data organization and wrangling
library(janitor)    # tabyls
library(lme4)       # mixed effects models
library(DHARMa)     # likelihood ratios for glmer
library(MuMIn)      # r-squared for glmer
library(DiagrammeR) # flow chart for Figure 1
library(ggpubr)     # groups plots for Figure 2


### IMPORT DATA
reportdata <- read_csv("Guenette_HNReporting_MasterFullCleaned_Final_Anonymized.csv")
radiologistdata <- read_csv("Guenette_HNReporting_FacultyData_Final_Anonymized.csv")
taxonomydata <- read_csv("Guenette_HNReporting_RecClassificationTaxonomy_Final_Anonymized.csv")
complexitydata <- read_csv("Guenette_HNReporting_ComplexityScore_Anonymized.csv")


### DATA WRANGLING

## Add binary SubspecialtyTraining category for Neuro-HN vs all others
radiologistdata <- radiologistdata %>% 
  mutate(fellowshipvs = fct_collapse(SubspecialtyTraining,
                                     "Neuro-HN" = c("HeadNeck","Neuroradiology"),
                                     "Other" = c("Abdominal-Body", "Breast", "Emergency",
                                                 "Interventional", "MRI", "Multiple", 
                                                 "Musculoskeletal", "Nuclear Medicine", 
                                                 "Pediatric", "Thoracic-Cardiac-Cardiovascular",
                                                 "Unclear", "Womens")))

## Merge diagnostic CT from PET exam with CT
reportdata <- reportdata %>% 
  mutate(procedure = fct_collapse(ProcedureModality, 
                                 "CT" = c("CT","PET")))

## Merge data into a single tibble
mergeddata <- reportdata %>%
  left_join(radiologistdata, by="FinalSignerID") %>%
  left_join(taxonomydata, by="StudyID") %>%
  left_join(complexitydata, by="StudyID") 

## Remove rows without radiologists or signing day (no reports issued)
mergeddata <-
  filter(mergeddata, FinalSignerID != 104) %>%
  filter(!is.na(FinalSignerID)) %>%
  filter(!is.na(FinalReportDay)) %>%
  filter(!is.na(Environment))

## Reduce to single report per patient
# Add column of random numbers
set.seed(7) #Random favorite number
mergeddata <- mergeddata %>%
  add_column(randomnumber = runif(nrow(.)))
# Order rows by PatientID then randomnumber
mergeddata <- 
  arrange(mergeddata,PatientIDAnon,randomnumber)
# Keep only one report per patient, keeping first listed by random number
mergeddata <-
  distinct(mergeddata,PatientIDAnon,.keep_all=TRUE)

## Set new variable to keep nlp false positives in general pool
mergeddata <-
  mutate(mergeddata, TrueRAI=ifelse(FalsePos==9,1,0)) %>%
  mutate(mergeddata, TrueRAI=replace_na(TrueRAI,0))

## Limit cohort to reports by radiologists with >=10 RAI for model stability
# Create new tibble of calculated RAI and RAI rates grouped by radiologist
RAIperrad <- mergeddata %>%
  group_by(FinalSignerID) %>%
  summarise(reports=n(),RAI=sum(TrueRAI),RAIrate=RAI/reports)
# Create subset with rads reporting >=10 RAI
RAI10 <- RAIperrad %>%
  filter(RAI >= 10)
# Pull list of FinalSignerID for the rads reporting >=10 RAI
RAI10list <- pull(RAI10,FinalSignerID)
# Create mergeddata subset of reports from rads reporting >=10 RAI
merged10 <- mergeddata %>%
  filter(FinalSignerID %in% RAI10list)
# Create radiologistdata subset of rads reporting >=10 RAI
radiologistsinmerged10 <- radiologistdata %>%
  filter(FinalSignerID %in% RAI10list)


### FUNCTIONS

# Build tables for categorical data with counts and percentages
make_categorical_table <- function(dat, yaxisname, xaxisname) {
  dat %>%
    tabyl({{yaxisname}}, {{xaxisname}}) %>%
    adorn_totals(c("row","col")) %>%
    adorn_percentages("row") %>%
    adorn_pct_formatting(digits = 1) %>%
    adorn_ns()
}


### TABLE 1: Summary of examination, patient, and radiologist factors of study sample

## Examination Factors

# Modality
make_categorical_table(merged10, TrueRAI, procedure)

# Care Setting
make_categorical_table(merged10, TrueRAI, Environment)

# Reported with trainee
make_categorical_table(merged10, TrueRAI, TraineeYN)

## Patient Factors

# Patient Sex
make_categorical_table(merged10, TrueRAI, Sex)

# Patient Age 
merged10 %>%
  summarise(n(), median=median(Age, na.rm=TRUE), iqrlow=quantile(Age,probs=0.25, na.rm=TRUE), iqrhigh=quantile(Age,probs=0.75, na.rm=TRUE), sd=sd(Age,na.rm=TRUE))

# Patient complexity score 
merged10 %>%
  summarise(n(), median=median(ComplexityScore, na.rm=TRUE), iqrlow=quantile(ComplexityScore,probs=0.25, na.rm=TRUE), iqrhigh=quantile(ComplexityScore,probs=0.75, na.rm=TRUE))

# Patient Self-Reported Race
make_categorical_table(merged10, TrueRAI, Race)

# Patient Self-Reported Ethnicity
make_categorical_table(merged10, TrueRAI, EthnicGroup)

# Patient ADI
# Overall
merged10 %>%
  summarise(n(), median=median(ADI_NATRANK, na.rm=TRUE), iqrlow=quantile(ADI_NATRANK,probs=0.25, na.rm=TRUE), iqrhigh=quantile(ADI_NATRANK,probs=0.75, na.rm=TRUE))

## Radiologist Factors

# Radiologist Years Since Training
radiologistsinmerged10 %>%
  summarise(n(), median=median(YIP, na.rm=TRUE), iqrlow=quantile(YIP,probs=0.25, na.rm=TRUE), iqrhigh=quantile(YIP,probs=0.75, na.rm=TRUE))

# Radiologist fellowship
tabyl(radiologistsinmerged10, fellowshipvs)

# Radiologist practice type
tabyl(radiologistsinmerged10, PracticeType)


### TABLE 2: Interradiologist variation in RAI rates, overall and stratified by examination and radiologist factors 

# Modality
recratesperradCT <- merged10 %>%
  filter(procedure=="CT") %>%
  group_by(FinalSignerID) %>%
  summarise(reports=n(),RAI=sum(TrueRAI),RAIrate=RAI/reports)
recratesperradCT %>%
  summarise(iqr=quantile(RAIrate),totalreports=sum(reports),totalrads=n())

recratesperradCTA <- merged10 %>%
  filter(procedure=="CTA") %>%
  group_by(FinalSignerID) %>%
  summarise(reports=n(),RAI=sum(TrueRAI),RAIrate=RAI/reports)
recratesperradCTA %>%
  summarise(iqr=quantile(RAIrate),totalreports=sum(reports),totalrads=n())

recratesperradMRI <- merged10 %>%
  filter(procedure=="MRI") %>%
  group_by(FinalSignerID) %>%
  summarise(reports=n(),RAI=sum(TrueRAI),RAIrate=RAI/reports)
recratesperradMRI %>%
  summarise(iqr=quantile(RAIrate),totalreports=sum(reports),totalrads=n())

recratesperradMRA <- merged10 %>%
  filter(procedure=="MRA") %>%
  group_by(FinalSignerID) %>%
  summarise(reports=n(),RAI=sum(TrueRAI),RAIrate=RAI/reports)
recratesperradMRA %>%
  summarise(iqr=quantile(RAIrate),totalreports=sum(reports),totalrads=n())

# Care Setting
recratesperradO <- merged10 %>%
  filter(Environment=="O") %>%
  group_by(FinalSignerID) %>%
  summarise(reports=n(),RAI=sum(TrueRAI),RAIrate=RAI/reports)
recratesperradO %>%
  summarise(iqr=quantile(RAIrate),totalreports=sum(reports),totalrads=n())

recratesperradE <- merged10 %>%
  filter(Environment=="E") %>%
  group_by(FinalSignerID) %>%
  summarise(reports=n(),RAI=sum(TrueRAI),RAIrate=RAI/reports)
recratesperradE %>%
  summarise(iqr=quantile(RAIrate),totalreports=sum(reports),totalrads=n())

recratesperradI <- merged10 %>%
  filter(Environment=="I") %>%
  group_by(FinalSignerID) %>%
  summarise(reports=n(),RAI=sum(TrueRAI),RAIrate=RAI/reports)
recratesperradI %>%
  summarise(iqr=quantile(RAIrate),totalreports=sum(reports),totalrads=n())

# Trainee
recratesperradTY <- merged10 %>%
  filter(TraineeYN=="1") %>%
  group_by(FinalSignerID) %>%
  summarise(reports=n(),RAI=sum(TrueRAI),RAIrate=RAI/reports)
recratesperradTY %>%
  summarise(iqr=quantile(RAIrate),totalreports=sum(reports),totalrads=n())

recratesperradTN <- merged10 %>%
  filter(TraineeYN=="0") %>%
  group_by(FinalSignerID) %>%
  summarise(reports=n(),RAI=sum(TrueRAI),RAIrate=RAI/reports)
recratesperradTN %>%
  summarise(iqr=quantile(RAIrate),totalreports=sum(reports),totalrads=n())

# Fellowship
recratesperradHNY <- merged10 %>%
  filter(fellowshipvs=="Neuro-HN") %>%
  group_by(FinalSignerID) %>%
  summarise(reports=n(),RAI=sum(TrueRAI),RAIrate=RAI/reports)
recratesperradHNY %>%
  summarise(iqr=quantile(RAIrate),totalreports=sum(reports),totalrads=n())

recratesperradHNN <- merged10 %>%
  filter(fellowshipvs=="Other") %>%
  group_by(FinalSignerID) %>%
  summarise(reports=n(),RAI=sum(TrueRAI),RAIrate=RAI/reports,outptrt=(sum(Environment=="O")/reports))
recratesperradHNN %>%
  summarise(iqr=quantile(RAIrate),totalreports=sum(reports),totalrads=n())

# Practice Type
recratesperradAc <- merged10 %>%
  filter(PracticeType=="Academic") %>%
  group_by(FinalSignerID) %>%
  summarise(reports=n(),RAI=sum(TrueRAI),RAIrate=RAI/reports)
recratesperradAc %>%
  summarise(iqr=quantile(RAIrate),totalreports=sum(reports),totalrads=n())

recratesperradCom <- merged10 %>%
  filter(PracticeType=="Community") %>%
  group_by(FinalSignerID) %>%
  summarise(reports=n(),RAI=sum(TrueRAI),RAIrate=RAI/reports)
recratesperradCom %>%
  summarise(iqr=quantile(RAIrate),totalreports=sum(reports),totalrads=n())

# Practice Group
recratesperradS1 <- merged10 %>%
  filter(Site=="1") %>%
  group_by(FinalSignerID) %>%
  summarise(reports=n(),RAI=sum(TrueRAI),RAIrate=RAI/reports)
recratesperradS1 %>%
  summarise(iqr=quantile(RAIrate),totalreports=sum(reports),totalrads=n())

recratesperradS2 <- merged10 %>%
  filter(Site=="2") %>%
  group_by(FinalSignerID) %>%
  summarise(reports=n(),RAI=sum(TrueRAI),RAIrate=RAI/reports)
recratesperradS2 %>%
  summarise(iqr=quantile(RAIrate),totalreports=sum(reports),totalrads=n())

recratesperradS3 <- merged10 %>%
  filter(Site=="3") %>%
  group_by(FinalSignerID) %>%
  summarise(reports=n(),RAI=sum(TrueRAI),RAIrate=RAI/reports)
recratesperradS3 %>%
  summarise(iqr=quantile(RAIrate),totalreports=sum(reports),totalrads=n())

recratesperradS4 <- merged10 %>%
  filter(Site=="4") %>%
  group_by(FinalSignerID) %>%
  summarise(reports=n(),RAI=sum(TrueRAI),RAIrate=RAI/reports)
recratesperradS4 %>%
  summarise(iqr=quantile(RAIrate),totalreports=sum(reports),totalrads=n())


### TABLE 3: Frequency of RAI, stratified by examination, patient, and radiologist factors

## Examination Factors

# Modality
make_categorical_table(merged10, procedure, TrueRAI)

# Care Setting
make_categorical_table(merged10, Environment, TrueRAI)

# Reported with trainee
make_categorical_table(merged10, TraineeYN, TrueRAI)

## Patient Factors

# Patient Sex
make_categorical_table(merged10, Sex, TrueRAI)

# Patient Self-Reported Race
make_categorical_table(merged10, Race, TrueRAI)

# Patient Self-Reported Ethnicity
make_categorical_table(merged10, EthnicGroup, TrueRAI)

## Radiologist Factors

# Radiologist fellowship
make_categorical_table(merged10, fellowshipvs, TrueRAI)

# Radiologist practice type
make_categorical_table(merged10, PracticeType, TrueRAI)


### IDENTIFICATION OF RAI

## Count false positives by type
## 1 = NLP error, 2 = rec for other body part in multi-accession report
## 3 = "follow-up per clinical protocol" or similar, 9 = true positive
merged10 %>% count(FalsePos)

## Create spreadsheet of examinations flagged by NLP
flaggedreports <- merged10 %>%
  filter(!is.na(FalsePos))
# Send to CSV file
write_csv(flaggedreports,"NLPFlagged.csv")


### FIGURE 1: SUBJECT COHORT FLOW CHART

grViz(
  "digraph my_flowchart { 
      graph[splines = ortho]
      node [fontname = Helvetica, shape = box, width = 4, height = 1]
      
        node1[label = <Search of data warehouse for<br/>head and neck CT and MRI examinations<br/>at a single multisite health system<br/>June 2021 - May 2022<br/>62,250 reports>]
        
        node2[label = <60,543 reports by<br/>209 radiologists>]
        
        blank1[label = '', width = 0.01, height = 0.01]
        excluded1[label = <Exclusion of examinations without available reports<br/>(1,707 reports)>]
        
        node1 -> blank1[dir = none];
        blank1 -> excluded1[minlen = 2];
        blank1 -> node2;
        { rank = same; blank1 excluded1 }
        
        node3[label = <48,143 reports in 48,143 patients<br/>by 208 radiologists>]
        
        nodex[label = <Single report randomly selected per patient>]
        
        node2 -> nodex;
        nodex -> node3;
        
        node4[label = <Final Sample:<br/>39,200 reports in 39,200 patients<br/>by 61 radiologists with ≥ 10 RAI>]
        
        blank3[label = '', width = 0.01, height = 0.01]
        excluded3[label = <Exclusion of reports by radiologists with fewer than 10 RAI<br/>(8,943 reports by 147 radiologists)>]
        
        node3 -> blank3[dir = none];
        blank3 -> excluded3[minlen = 2];
        blank3 -> node4;
        { rank = same; blank3 excluded3 }
        
     }"
)


### FIGURE 2: BAR CHART - RADIOLOGIST REPORT COUNT AND RAI RATE

# Figure 2A
merged10 %>%
  group_by(FinalSignerID) %>%
  summarise(count=n(),RAI=sum(TrueRAI),RAIrate=RAI/count) %>%
  ggplot(aes(x=reorder(FinalSignerID,count),y=count)) +
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle=90,vjust=0.05)) +
  xlab(" ") +
  ylab("Number of Reports")
ggsave("Fig2A.tiff", device="tiff", dpi="print")

# Figure 2B
merged10 %>%
  group_by(FinalSignerID) %>%
  summarise(count=n(),RAI=sum(TrueRAI),RAIrate=RAI/count) %>%
  ggplot(aes(x=reorder(FinalSignerID,count),y=RAIrate)) +
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle=90,vjust=0.05)) +
  xlab("Radiologist") +
  ylab("RAI Rate") 
ggsave("Fig2B.tiff", device="tiff", dpi="print")


### FIGURE 3: SCATTER PLOT - RADIOLOGIST REPORT COUNT AND RAI RATE WITH REGRESSION LINE

# Plot
merged10 %>%
  group_by(FinalSignerID) %>%
  summarise(count=n(),RAI=sum(TrueRAI),RAIrate=RAI/count) %>%
  ggplot(aes(x=count,y=RAIrate)) +
  geom_point() + 
  geom_smooth(method=lm) +
  xlab("Number of Reports") +
  ylab("RAI Rate") 
ggsave("Fig3.tiff", device="tiff", dpi="print")

## Test: LM assessing relationship of RAI rate with head/neck case volume
# count case volumes and calculate RAI rate by radiologist
RAIcountsbyrad <- merged10 %>%
  group_by(FinalSignerID) %>%
  summarise(count=n(),countper1000=count/1000,RAI=sum(TrueRAI),RAIrate=RAI/count)
# regression
lm_RAIcountsbyrad <- lm(RAIrate ~ countper1000, data=RAIcountsbyrad)
summary(lm_RAIcountsbyrad)


### FIGURE 4: BOX PLOT - RAI RATE VARIATION BY RADIOLOGY GROUP

merged10 %>%
  group_by(FinalSignerID) %>%
  summarise(reports = n(), RAI = sum(TrueRAI), RAIrate = RAI/reports, Site=Site) %>%
  group_by(Site) %>%
  ggplot(aes(x=as.factor(Site),y=RAIrate)) +
  geom_boxplot() +
  xlab("Radiology Group") +
  ylab("RAI Rate")
ggsave("Fig4.tiff", device="tiff", dpi="print")


### DATA WRANGLING FOR STATISTICAL MODELS

## Scale and center linear covariates to allow model convergence
merged10 <- merged10 %>%
  mutate(complexityscaled = ComplexityScore/5) %>%
  mutate(complexitycentered = scale(complexityscaled, center=TRUE, scale=FALSE)) %>%
  mutate(agescaled = Age/10) %>%
  mutate(agecentered = scale(agescaled, center=TRUE, scale=FALSE)) %>%
  mutate(adiscaled = ADI_NATRANK/10) %>%
  mutate(adicentered = scale(adiscaled, center=TRUE, scale=FALSE)) %>%
  mutate(yipscaled = YIP/5) %>%
  mutate(yipcentered = scale(yipscaled, center=TRUE, scale=FALSE))

## Set Neuro-HN as fellowship reference level: baseline group for comparison
merged10$fellowshipvs <- relevel(factor(merged10$fellowshipvs), ref="Neuro-HN")

## Set Community as practice type reference level: baseline group for comparison
merged10$PracticeType <- relevel(factor(merged10$PracticeType), ref="Academic")

## Set Outpatient as care setting reference level: largest number for baseline comparison
merged10$Environment <- relevel(factor(merged10$Environment), ref="O")

## Set White as race reference level: largest number for baseline comparison
merged10$Race <- relevel(factor(merged10$Race), ref="White")

## Order by Patient ID for model
merged10 <- 
  arrange(merged10,FinalSignerID)


### PRIMARY OUTCOME: RAI RATES ARE VARIABLE ACROSS RADIOLOGISTS

# Remove NA values
merged10rmna <- merged10 %>%
  filter(!is.na(Environment)) %>%
  filter(!is.na(Sex)) %>%
  filter(!is.na(agecentered)) %>%
  filter(!is.na(complexitycentered)) 

# H0: no variability in RAI likelihood attributable to radiologists
# Model with radiologist as random variable and covariates to account for differences in case distributions
glmer_simple <- glmer(TrueRAI ~ Environment + Sex + agecentered + complexitycentered + (1|FinalSignerID), data=merged10rmna, family=binomial, glmerControl(optimizer="bobyqa"))
# Base model with the same covariates and no random effect for radiologist clustering
glm_simple <- glm(TrueRAI ~ Environment + Sex + agecentered + complexitycentered, data=merged10rmna, family=binomial)
# Simulated likelihood ratio test using DHARMa package to determine if random effect (radiologist) variance is significant
LRT <- simulateLRT(glm_simple, glmer_simple, n=10, seed=7)
LRT


### SECONDARY OUTCOME: FACTORS RELATED TO RAI LIKELIHOOD/VARIABILITY

## Exploratory Test: GLM including possible Patient and Radiologist factors
glm_merged10 <- glmer(TrueRAI ~ Environment + Sex + agecentered + complexitycentered + EthnicGroup + Race + adicentered + yipcentered + fellowshipvs + PracticeType + TraineeYN + procedure + (1|FinalSignerID), data=merged10, family=binomial, glmerControl(optimizer="bobyqa"))
# estimates, standard errors, p values
summary(glm_merged10)
# odds ratios and confidence intervals (scaled)
cc <- confint(glm_merged10,parm="beta_",method="Wald")
ctab <- cbind(est=fixef(glm_merged10),cc)
rtab <- exp(ctab)
print(rtab,digits=2)
# R-squared
r.squaredGLMM(glm_merged10)


### END ANALYSIS