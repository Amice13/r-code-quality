#================================
# Actionability of Recommendations for Additional Imaging in Head and Neck Radiology
# PI: Jeffrey P Guenette MD MPH
# Funding: Association of University Radiologists, AHRQ R18 HS029348
# Publication: https://doi.org/10.1016/j.jacr.2024.01.005
# Associated data files in Harvard Dataverse: https://doi.org/10.7910/DVN/O6JJT6 
#================================


### CLEAR ENVIRONMENT
rm(list = ls())
gc()


### LOAD LIBRARIES
library(tidyverse)     # data organization and wrangling
library(janitor)       # tabyls
library(lme4)          # mixed effects models
library(DiagrammeR)    # flow chart for Figure 1
library(DiagrammeRsvg) # export tool for Figure 1
library(rsvg)          # export tool for Figure 1


### IMPORT DATA
reportdata <- read_csv("Guenette_HNReporting_MasterFullCleaned_Final_Anonymized.csv")
radiologistdata <- read_csv("Guenette_HNReporting_FacultyData_Final_Anonymized.csv")
taxonomydata <- read_csv("Guenette_HNReporting_RecClassificationTaxonomy_Final_Anonymized.csv")
complexitydata <- read_csv("Guenette_HNReporting_ComplexityScore_Anonymized.csv")


### DATA WRANGLING

## Create categories for patient age
reportdata <- reportdata %>%
  mutate(agecat = cut(Age, breaks=c(-Inf,20,40,60,80,Inf), labels=c("0-20","21-40","41-60","61-80","Over 80")))

## Add binary SubspecialtyTraining category for Neuro-HN vs all others
radiologistdata <- radiologistdata %>% 
  mutate(fellowshipvs = fct_collapse(SubspecialtyTraining,
                                     "Neuro-HN" = c("HeadNeck","Neuroradiology"),
                                     "Other" = c("Abdominal-Body", "Breast", "Emergency",
                                                 "Interventional", "MRI", "Multiple", 
                                                 "Musculoskeletal", "Nuclear Medicine", 
                                                 "Pediatric", "Thoracic-Cardiac-Cardiovascular",
                                                 "Unclear", "Womens")))

## Create Actionable category from taxonomy data
taxonomydata <- taxonomydata %>%
  mutate(Actionable = ifelse(Complete==1&Ambiguous==0&Multiplicity==0&Conditional==0&Alternate==0,1,0))

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

## Set new variable to keep nlp false positives in general pool
mergeddata <-
  mutate(mergeddata, TrueRAI=ifelse(FalsePos==9,1,0)) %>%
  mutate(mergeddata, TrueRAI=replace_na(TrueRAI,0))

## Identify number of unique radiologists
# Collapse to radiologist ID
recratesperrad <- mergeddata %>%
  group_by(FinalSignerID) 
# Pull list of FinalSignerID for the rads in merged
radlist <- pull(recratesperrad,FinalSignerID)
# Create radiologistdata subset of rads in final merged list
radiologistsinmerged <- radiologistdata %>%
  filter(FinalSignerID %in% radlist)

## Isolate data set of reports with unique RAI
onlyRAI <- mergeddata 
onlyRAI <- 
  filter(onlyRAI, TrueRAI == 1) # keep those with RAI
onlyRAI <- 
  filter(onlyRAI, Duplicate != 1) # eliminate those duplicated across accessions

## Create tibble with radiologist data and actionability data for summary statistics
# Calculate actionability statistics
actionabilityperradRAI <- onlyRAI %>%
  group_by(FinalSignerID) %>%
  summarise(rai=n(),actionable=sum(Actionable),actrate=actionable/rai,complete=sum(Complete),comprate=complete/rai,ambiguous=sum(Ambiguous),ambrate=ambiguous/rai,conditional=sum(Conditional),condrate=conditional/rai,multiplicity=sum(Multiplicity),multrate=multiplicity/rai,alternate=sum(Alternate),altrate=alternate/rai)
# Pull list of FinalSignerID for the rads in onlyRAI
radlistRAI <- pull(actionabilityperradRAI,FinalSignerID)
# Create radiologistdata subset of rads in onlyRAI
radiologistsinonlyRAI <- radiologistdata %>%
  filter(FinalSignerID %in% radlistRAI)
# Merge radiologistdata with actionability statistics
radiologistdatawithactionability <- radiologistsinonlyRAI %>%
  left_join(actionabilityperradRAI, by="FinalSignerID")

## Collapse YIP into categories otherwise data too sparse for modeling
# Identify quartiles
radiologistdatawithactionability %>%
  summarise(quantile=quantile(YIP, na.rm=TRUE))
# Group by quartiles
onlyRAI <- onlyRAI %>%
  mutate(yipcat = cut(YIP, c(-Inf,3,10,21,Inf), paste0('careerquartile',1:4)))
radiologistdatawithactionability <- radiologistdatawithactionability %>%
  mutate(yipcat = cut(YIP, c(-Inf,3,10,21,Inf), paste0('careerquartile',1:4)))

## Set Female as radiologist sex reference level
radiologistdatawithactionability$RSex <- relevel(factor(radiologistdatawithactionability$RSex), ref="F")
onlyRAI$RSex <- relevel(factor(onlyRAI$RSex), ref="F")

## Set Neuro-HN as fellowship reference level: baseline group for comparison
onlyRAI$fellowshipvs <- relevel(factor(onlyRAI$fellowshipvs), ref="Neuro-HN")

## Order by Radiologist ID for model
onlyRAI <- 
  arrange(onlyRAI,FinalSignerID)


### PATIENT DEMOGRAPHICS FOR RESULTS SECTION TEXT

## Patient Sex
tabyl(onlyRAI, Sex)

## Patient Age 
# Overall
onlyRAI %>%
  summarise(n(), median=median(Age, na.rm=TRUE), iqrlow=quantile(Age,probs=0.25, na.rm=TRUE), iqrhigh=quantile(Age,probs=0.75, na.rm=TRUE))


### ACTIONABILITY AND LANGUAGE ATTRIBUTES FOR RESULTS SECTION TEXT

## Actionable Rate
tabyl(onlyRAI, Actionable)
tabyl(onlyRAI, Complete)
tabyl(onlyRAI, Ambiguous)
tabyl(onlyRAI, Conditional)
tabyl(onlyRAI, Multiplicity)
tabyl(onlyRAI, Alternate)


### TABLE 2: RADIOLOGIST DEMOGRAPHICS WITH ACTIONABILITY RATES BY DEMOGRAPHIC

## Radiologist Sex
radiologistdatawithactionability %>%
  group_by(RSex) %>%
  summarise(n(),pct=n()/length(radlistRAI),avgact=mean(actrate),sdact=sd(actrate),avgcomp=mean(comprate),sdcomp=sd(comprate),avgamb=mean(ambrate),sdamb=sd(ambrate),avgcond=mean(condrate),sdcond=sd(condrate),avgmult=mean(multrate),sdmult=sd(multrate),avgalt=mean(altrate),sdalt=sd(altrate))

## Radiologist YIP
radiologistdatawithactionability %>%
  group_by(yipcat) %>%
  summarise(n(),pct=n()/length(radlistRAI),avgact=mean(actrate),sdact=sd(actrate),avgcomp=mean(comprate),sdcomp=sd(comprate),avgamb=mean(ambrate),sdamb=sd(ambrate),avgcond=mean(condrate),sdcond=sd(condrate),avgmult=mean(multrate),sdmult=sd(multrate),avgalt=mean(altrate),sdalt=sd(altrate))

## Radiologist fellowship
radiologistdatawithactionability %>%
  group_by(fellowshipvs) %>%
  summarise(n(),pct=n()/length(radlistRAI),avgact=mean(actrate),sdact=sd(actrate),avgcomp=mean(comprate),sdcomp=sd(comprate),avgamb=mean(ambrate),sdamb=sd(ambrate),avgcond=mean(condrate),sdcond=sd(condrate),avgmult=mean(multrate),sdmult=sd(multrate),avgalt=mean(altrate),sdalt=sd(altrate))

# Radiologist practice type
radiologistdatawithactionability %>%
  group_by(PracticeType) %>%
  summarise(n(),pct=n()/length(radlistRAI),avgact=mean(actrate),sdact=sd(actrate),avgcomp=mean(comprate),sdcomp=sd(comprate),avgamb=mean(ambrate),sdamb=sd(ambrate),avgcond=mean(condrate),sdcond=sd(condrate),avgmult=mean(multrate),sdmult=sd(multrate),avgalt=mean(altrate),sdalt=sd(altrate))

# Reported with trainee
onlyRAI %>%
  group_by(TraineeYN) %>%
  summarise(n(),pct=n()/count(onlyRAI),avgact=mean(Actionable),sdact=sd(Actionable),avgcomp=mean(Complete),sdcomp=sd(Complete),avgamb=mean(Ambiguous),sdamb=sd(Ambiguous),avgcond=mean(Conditional),sdcond=sd(Conditional),avgmult=mean(Multiplicity),sdmult=sd(Multiplicity),avgalt=mean(Alternate),sdalt=sd(Alternate))


### TABLE 3 and SUPPLEMENTAL TABLE 1: FREQUENCY OF RAI TYPES

## Count of RAI by modality and exam/body part
raicountsmodalityexam <- onlyRAI %>% 
  group_by(Modality, Exam) %>% 
  summarise(count=n(), pct=(count/nrow(onlyRAI))) %>%
  arrange(desc(count)) %>%
  print(n=100)
# Export for use in Excel/Word
write.csv(raicountsmodalityexam, file = "raicountsmodalityexam.csv")


### TABLE 4: RAI RATES BY INTERPRETED EXAMINATION TYPE

## Count of RAI by exam region and modality
# Count of total reports by exam region and modality
countsexammodality <- mergeddata %>% 
  group_by(ProcedureRegion, ProcedureModality) %>% 
  summarise(count=n()) %>%
  arrange(desc(count))
# Count of total RAI by exam region with relative rate
countsexammodality2 <- onlyRAI %>% 
  group_by(ProcedureRegion, ProcedureModality) %>% 
  summarise(count=n(), pct=(count/nrow(onlyRAI))) %>%
  arrange(desc(count))
# Join data 
countsexammodality <- countsexammodality %>%
  left_join(countsexammodality2, by=c("ProcedureModality"="ProcedureModality","ProcedureRegion"="ProcedureRegion"))
# Summarize with both relative rates (pct) and rate of RAI per exam region
countsexammodality <- countsexammodality %>%
  summarise(ProcedureRegion,ProcedureModality,count.x,count.y,pct,rate=count.y/count.x) %>%
  arrange(desc(pct)) %>%
  print(n=100)
# Export for use in Excel/Word
write.csv(countsexammodality, file = "countsexammodality.csv")


### TABLE 5: RAI ACTIONABILITY STATISTICS

## Test: Actionable - GLM clustering by radiologist assessing radiologist factors
glm_actionable <- glmer(Actionable ~ RSex + yipcat + fellowshipvs + PracticeType + TraineeYN + (1|FinalSignerID), data=onlyRAI, family=binomial, glmerControl(optimizer="bobyqa"))
# estimates, standard errors, p values
summary(glm_actionable)
# odds ratios and confidence intervals
cc <- confint(glm_actionable,parm="beta_",method="Wald")
ctab <- cbind(est=fixef(glm_actionable),cc)
rtab <- exp(ctab)
print(rtab,digits=2)

## Test: Complete - GLM clustering by radiologist assessing radiologist factors
glm_complete <- glmer(Complete ~ RSex + yipcat + fellowshipvs + PracticeType + TraineeYN + (1|FinalSignerID), data=onlyRAI, family=binomial, glmerControl(optimizer="bobyqa"))
# estimates, standard errors, p values
summary(glm_complete)
# odds ratios and confidence intervals
cc <- confint(glm_complete,parm="beta_",method="Wald")
ctab <- cbind(est=fixef(glm_complete),cc)
rtab <- exp(ctab)
print(rtab,digits=2)

## Test: Ambiguous - GLM clustering by radiologist assessing radiologist factors
glm_ambiguous <- glmer(Ambiguous ~ RSex + yipcat + fellowshipvs + PracticeType + TraineeYN + (1|FinalSignerID), data=onlyRAI, family=binomial, glmerControl(optimizer="bobyqa"))
# estimates, standard errors, p values
summary(glm_ambiguous)
# odds ratios and confidence intervals
cc <- confint(glm_ambiguous,parm="beta_",method="Wald")
ctab <- cbind(est=fixef(glm_ambiguous),cc)
rtab <- exp(ctab)
print(rtab,digits=2)

## Test: Conditional - GLM clustering by radiologist assessing radiologist factors
glm_conditional <- glmer(Conditional ~ RSex + yipcat + fellowshipvs + PracticeType + TraineeYN + (1|FinalSignerID), data=onlyRAI, family=binomial, glmerControl(optimizer="bobyqa"))
# estimates, standard errors, p values
summary(glm_conditional)
# odds ratios and confidence intervals
cc <- confint(glm_conditional,parm="beta_",method="Wald")
ctab <- cbind(est=fixef(glm_conditional),cc)
rtab <- exp(ctab)
print(rtab,digits=2)

## Test: Multiplicity - GLM clustering by radiologist assessing radiologist factors
glm_multiplicity <- glmer(Multiplicity ~ RSex + yipcat + fellowshipvs + PracticeType + TraineeYN + (1|FinalSignerID), data=onlyRAI, family=binomial, glmerControl(optimizer="bobyqa"))
# estimates, standard errors, p values
summary(glm_multiplicity)
# odds ratios and confidence intervals
cc <- confint(glm_multiplicity,parm="beta_",method="Wald")
ctab <- cbind(est=fixef(glm_multiplicity),cc)
rtab <- exp(ctab)
print(rtab,digits=2)

## Test: Alternate - GLM clustering by radiologist assessing radiologist factors
glm_alternate <- glmer(Alternate ~ RSex + yipcat + fellowshipvs + PracticeType + TraineeYN + (1|FinalSignerID), data=onlyRAI, family=binomial, glmerControl(optimizer="bobyqa"))
# estimates, standard errors, p values
summary(glm_alternate)
# odds ratios and confidence intervals
cc <- confint(glm_alternate,parm="beta_",method="Wald")
ctab <- cbind(est=fixef(glm_alternate),cc)
rtab <- exp(ctab)
print(rtab,digits=2)


### FIGURE 1: SUBJECT COHORT FLOW CHART

fig1 <- grViz(
  "digraph my_flowchart { 
      graph[splines = ortho]
      node [fontname = Helvetica, shape = box, width = 4, height = 1]
      
        node1[label = <Total Head and Neck Examinations<br/>6/1/2021 to 5/30/2022<br/>(report n = 62,250)>]
        node2[label = <Total Issued Reports<br/>(report n = 60,543)<br/>(radiologist n = 209)>]
        
        blank1[label = '', width = 0.01, height = 0.01]
        excluded1[label = <Exclusion of examinations without reports<br/>(report n = 1,707)>]
        
        node1 -> blank1[dir = none];
        blank1 -> excluded1[minlen = 2];
        blank1 -> node2;
        { rank = same; blank1 excluded1 }
        
        node3[label = <Total Reports with RAI<br/>(report n = 4,382)<br/>(radiologist n = 166)>]
        
        blank2[label = '', width = 0.01, height = 0.01]
        excluded2[label = <Exclusion of reports without RAI<br/>(report n = 56,161)<br/>(radiologist n = 43)>]
        
        node2 -> blank2[dir = none];
        blank2 -> excluded2[minlen = 2];
        blank2 -> node3;
        { rank = same; blank2 excluded2 }
        
     }"
)

fig1 %>% export_svg %>% charToRaw %>% rsvg_png("fig1.png", width=1200)


### END ANALYSIS
