# Replication code for 'Inside the Secret Garden: 
# candidate selection at the 2019 UK general election'

# Chris Butler, Marta Miori & Rob Ford

# https://doi.org/10.1177/13691481241270519
#------------------------------------------------------------------------------

# STAGE 1: LOAD AND CLEAN DATA

candidates <- read.csv("ShortlistedCands2019Anonymous.csv", ,sep=";")

str(candidates)

# Create an ordinal age variable
candidates$yob <- as.numeric(candidates$yob)
candidates$age <- cut(candidates$yob, c(0, 1959, 1974, 1989, 1997), labels = c("61+", "45-60", "31-45", "18-30"))
table(candidates$age, useNA = "ifany")
table(candidates$yob, useNA = "ifany")

# Turn BAME and LGBT into numeric variables
candidates$LGBT <- as.numeric(candidates$LGBT)
candidates$BAME <- as.numeric(candidates$BAME)

# Create an ordinal variable for period when selection occurred
library(forcats)
candidates$selection_period <- ifelse(candidates$selection_period %in% c("a", "b", "c"), 
                                      candidates$selection_period, "na")
candidates$selection_period = factor(candidates$selection_period)
candidates$selection_period <- fct_collapse(candidates$selection_period,
                                            Selected_pre_February_2019 = c("a"),
                                            Selected_February_to_October_2019 = c("b"),
                                            Selected_after_October29th_2019 = c("c"))
table(candidates$selection_period, useNA = "ifany")

# Create a factor variable for sub occupation category
candidates$occupation_sub = factor(candidates$occupation_sub)
candidates$occupation_sub <- fct_collapse(candidates$occupation_sub,
                                          Legal_profession = c("1"),
                                          Education = c("2"),
                                          Physician = c("3"),
                                          Engineers = c("4"),
                                          Councillor_or_Elected_Office = c("5"),
                                          Policy_Research = c("6"),
                                          Party_worker = c("7"),
                                          Journalist = c("8"),
                                          Trade_union_Official = c("9"),
                                          Lobbyist = c("10"),
                                          Business = c("11"),
                                          Armed_forces = c("12"),
                                          Civil_Servant_or_local_Govt = c("13"),
                                          NHS_practitioner = c("14"),
                                          Other = c("17", "18", "19", "21", "23", "24", "99"),
                                          Other_White_Collar = c("20"),
                                          NFP_Sector = c("22"),
                                          Comms = c("25"),
                                          Finance = c("26"),
                                          BlueCollar = c("27"),)

#remove data where shortlist unknown
candidates <- subset(candidates, shortlist_known == 1)

# -----------------------------------------------------------------------------

# STAGE 2: DESCRIPTIVE STATISTICS

# Firstly we want to show the characteristics of shortlisted candidates by party

# Tidy up party names
library(dplyr)
candidates <- candidates %>% mutate(party=recode(party, 'Con' = 'Conservative', 
                                                 'Labour' = 'Labour', 
                                                 'Labour ' = 'Labour', 
                                                 'Liberal Democrat' = 'Lib Dem', 
                                                 'SNP' = 'SNP'))

# Remove duplicate candidates for this exercise only
individuals <- candidates %>% distinct(candidateID, .keep_all = TRUE)

#remove extraneous variables
select <- dplyr::select

individuals <- individuals |> 
  select(party, age, gender, BAME, disability, LGBT, university, oxbridge, occupation_meta,
         occupation_sub, occ_party, occ_party_nat, past_selection, elected_cllr, endo_mp)

table_labels <- list(
  age ~ "Age",
  gender ~ "Female",
  BAME ~ "Ethnic Minority",
  disability ~ "Disabled",
  LGBT ~ "LGBT",
  university ~ "University educated",
  oxbridge ~ "Oxbridge educated",
  occupation_meta ~ "Occupation category",
  occupation_sub ~ "Occupation",
  occ_party ~ "Worked for party",
  occ_party_nat ~ "Worked for national party",
  past_selection ~ "Stood previously",
  elected_cllr ~ "Served as Councillor",
  endo_mp ~ "Endorsed by MP")

library(gtsummary)
library(flextable)
library(ftExtra)

cands_by_party <- tbl_summary(individuals, by = party, missing = "no", label = table_labels) |> 
  add_overall(last = TRUE) |> as_flex_table()

cands_by_party

# save file as image or web page (Table 3)

rm(individuals, table_labels)

# Predict whether time period predicts likelihood of candidates with particular 
# characteristics being shortlisted

# Create binary variable for whether candidate worked in brokerage or instrumental profession

candidates$traditional <- ifelse(candidates$occupation_meta == 1, 1,
                                 ifelse(candidates$occupation_meta == 2, 1, 0))
table(candidates$party, candidates$traditional, useNA = "ifany")

# Remove unknown selection date
candidateswdate <- candidates[candidates$selection_period != "na", ]
table(candidateswdate$party, candidateswdate$selection_period)

# Remove SNP due to lack of variance in selection date
candidateswdate <- candidateswdate[candidateswdate$party != "SNP",]

# Run logistics models with selection period as predictor
snapparty <- glm(occ_party ~ selection_period + party, data = candidateswdate)
snapprofession <- glm(traditional ~ selection_period + party, data = candidateswdate)
snaplocal <- glm(local ~ selection_period + party, data = candidateswdate)
snapsex <- glm(gender ~ selection_period + party, data = candidateswdate)
snapbame <- glm(BAME ~ selection_period + party, data = candidateswdate)
library(texreg)
screenreg(list(snapparty, snapprofession, snaplocal,snapsex,snapbame))

# Create dotwhiskerplot
new.list <- list(snapparty, snapprofession, snaplocal, snapsex, snapbame) 
new.names <- c("Ethnic minority", "Female", "Resident locally", 
               "Traditional Profession ", "Worked for party")
library(dotwhisker)
dwplot(new.list, vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) +
  scale_color_hue("Data", labels = new.names) +
  scale_y_discrete(labels = c("Lib Dem", "Labour", "Selected after 29th October 2019", 
                              "Selected February to October 2019")) +
  xlab("Coefficient Estimate") +
  ggtitle("Conservative, Labour & Lib Dem candidates")

# Rerun models for parties separately
concands <- subset(candidateswdate, party =="Conservative")
labcands <- subset(candidateswdate, party =="Labour"|party=="Labour ")

snappartycon <- glm(occ_party ~ selection_period, data = concands)
snapprofessioncon <- glm(traditional ~ selection_period, data = concands)
snaplocalcon <- glm(local ~ selection_period, data = concands)
snapsexcon <- glm(gender ~ selection_period, data = concands)
snapbamecon <- glm(BAME ~ selection_period, data = concands)

con.list <- list(snappartycon, snapprofessioncon, snaplocalcon, snapsexcon, snapbamecon) 

dwplot(con.list, vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) +
  scale_color_hue("Data", labels = new.names) +
  scale_y_discrete(labels = c("Selected after 29th October 2019", 
                              "Selected February to October 2019")) +
  xlab("Coefficient Estimate") +
  ggtitle("Conservative Party candidates")

snappartylab <- glm(occ_party ~ selection_period, data = labcands)
snapprofessionlab <- glm(traditional ~ selection_period, data = labcands)
snaplocallab <- glm(local ~ selection_period, data = labcands)
snapsexlab <- glm(gender ~ selection_period, data = labcands)
snapbamelab <- glm(BAME ~ selection_period, data = labcands)

lab.list <- list(snappartylab, snapprofessionlab, snaplocallab, snapsexlab, snapbamelab) 

dwplot(lab.list, vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) +
  scale_color_hue("Data", labels = new.names) +
  scale_y_discrete(labels = c("Selected after 29th October 2019", 
                              "Selected February to October 2019")) +
  xlab("Coefficient Estimate") +
  ggtitle("Labour Party candidates")

concands$robust_selection_period <- fct_collapse(concands$selection_period,
                                                 Selected_early = c("Selected_pre_February_2019",
                                                                    "Selected_February_to_October_2019"),
                                                 Selected_late = c("Selected_after_October29th_2019"))

snappartyconrobust <- glm(occ_party ~ robust_selection_period, data = concands)
snapprofessionconrobust <- glm(traditional ~ robust_selection_period, data = concands)
snaplocalconrobust <- glm(local ~ robust_selection_period, data = concands)
snapsexconrobust <- glm(gender ~ robust_selection_period, data = concands)
snapbameconrobust <- glm(BAME ~ robust_selection_period, data = concands)

con.robust.list <- list(snappartyconrobust, snapprofessionconrobust, snaplocalconrobust, 
                        snapsexconrobust, snapbameconrobust) 

dwplot(con.robust.list, vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) +
  scale_color_hue("Data", labels = new.names) +
  scale_y_discrete(labels = c("Selected after 29th October 2019")) +
  xlab("Coefficient Estimate") +
  ggtitle("Conservative Party candidates")

# Save plots as Figures 1a, 1b, 1c & Appendix 3

rm(new.list, snapparty, snappartylab, snapprofession, snapprofessionlab, 
   snaplocal, snaplocallab, snapsex, snapsexlab, snapbame, snapbamelab, 
   new.names, candidateswdate, snappartyconrobust, snapprofessionconrobust, 
   snaplocalconrobust, snapsexconrobust ,snapbameconrobust, con.list, lab.list,
   con.robust.list, snapbamecon, snaplocalcon, snappartycon, snapprofessioncon,
   snapsexcon)

# ------------------------------------------------------

# STAGE 3: ANALYSIS

# We need to create a variable for each separate contest, 
# based on unique combinations of 'party' and 'constituency'
candidates$contest <- as.integer(factor(paste(candidates$party, candidates$constituency), 
                                        levels = unique(paste(candidates$party, candidates$constituency))))

# How many contests were uncontested?
table(candidates$contest)

# Now we run a multi-level logistic model
library(lme4)
selectionmodel <- glmer(success ~ gender + BAME + local + past_selection + elected_cllr + 
                          occ_party + ( 1 | contest), data=candidates, family = binomial(link = "logit"))
screenreg(selectionmodel)

# Next we run the same model but by party
# First, we create subsets for Conservative & Labour candidates

concands <- subset(candidates, party =="Conservative")
labcands <- subset(candidates, party =="Labour"|party=="Labour ")

# Then we subset Labour selections by whether they use all-women shortlists (AWS)
labaws <- subset(labcands, aws == 1)
labmixed <- subset(labcands, aws == 0)

labmodel <- glmer(success ~ gender + BAME + local + past_selection + elected_cllr + occ_party + ( 1 | contest), 
                  data = labmixed, family = binomial(link = "logit"))
awsmodel <- glmer(success ~ BAME + local + past_selection + elected_cllr + occ_party + ( 1 | contest), data = labaws, 
                  family = binomial(link = "logit"))

# Run the model for Conservative selection contests

conmodel <- glmer(success ~ gender + BAME + local + past_selection + elected_cllr + occ_party + (1 | contest), 
                  data = concands, family = binomial(link = "logit"))

screenreg(list(labmodel, awsmodel, selectionmodel, conmodel))

# Visualise results of our four models in a dotwhisker plot
new.list <- list(labmodel, awsmodel, selectionmodel, conmodel) 
new.names <- c("Conservatives", "All Candidates", "Labour AWS", "Labour Non-AWS")
library(ggplot2)

dwplot(new.list, vline = geom_vline(xintercept = 0, "SD (Intercept)" = 0, colour = "grey60", linetype = 2)) + 
  scale_color_manual("Data",labels = new.names, values=c("blue4", "chartreuse4", "deeppink2", "darkred")) + 
  scale_y_discrete(labels = c("Contest level", "Worked for party", "Elected Councillor", "Stood previously", "Live locally", 
                              "Ethnic Minority", "Female")) + xlab("Coefficient Estimate")

# Save as Figure 2
rm(new.list, new.names)

# -----------------------------------------------------------------------------

# STAGE 4: ROBUSTNESS CHECKS

# Restrict party workers to national party workers

SpAdmodel <- glmer(success ~ gender + BAME + local + past_selection + elected_cllr + 
                     occ_party_nat + ( 1 | contest), data=candidates, family = binomial(link = "logit"))
screenreg(list(selectionmodel, SpAdmodel))
htmlreg(list(selectionmodel, SpAdmodel), file = "Output/AppendixIV.doc")

# Calculate pseudo R squared for models
nullmod <- glm(candidates$success~1, family="binomial")
1-logLik(selectionmodel)/logLik(nullmod)
1-logLik(SpAdmodel)/logLik(nullmod)

# Robustness checks with with age
yobmodel <- glmer(success ~ gender + BAME + local + past_selection + elected_cllr + 
                    occ_party + yob + ( 1 | contest), data=candidates, family = binomial(link = "logit"))
agemodel <- glmer(success ~ gender + BAME + local + past_selection + elected_cllr + 
                    occ_party + age + ( 1 | contest), data=candidates, family = binomial(link = "logit"))
screenreg(list(selectionmodel, agemodel, yobmodel))
htmlreg(list(selectionmodel, agemodel, yobmodel), file = "Appendix V.doc")

1-logLik(agemodel)/logLik(nullmod)
1-logLik(yobmodel)/logLik(nullmod)

# Include LGBT & disability in models
appendixVimodel <- glmer(success ~ gender + BAME + local + past_selection + elected_cllr + 
                           occ_party + LGBT + disability + ( 1 | contest), data=candidates, family = binomial(link = "logit"))
screenreg(list(selectionmodel, appendixVimodel))
htmlreg(list(selectionmodel, appendixVimodel), file = "Output/Appendix VI.doc")
1-logLik(appendixVimodel)/logLik(nullmod)

# Interaction models
candidates$female <- candidates$gender
imodel1 <- glmer(success ~ female*local + BAME + past_selection + elected_cllr + occ_party +
                   ( 1 | contest), data=candidates, family = binomial(link = "logit"))
imodel2 <- glmer(success ~ female + BAME*local + past_selection + elected_cllr + occ_party +
                   ( 1 | contest), data=candidates, family = binomial(link = "logit"))
imodel3 <- glmer(success ~ female*BAME + local + past_selection + elected_cllr + occ_party +
                   ( 1 | contest), data=candidates, family = binomial(link = "logit"))
imodel4 <- glmer(success ~ female*yob + BAME + local + past_selection + elected_cllr + occ_party +
                   ( 1 | contest), data=candidates, family = binomial(link = "logit"))

screenreg(list(selectionmodel, imodel1, imodel2, imodel3, imodel4))
htmlreg(list(selectionmodel, imodel1, imodel2, imodel3, imodel4), file = "Output/Interaction checks.doc")
1-logLik(imodel1)/logLik(nullmod)
1-logLik(imodel2)/logLik(nullmod)
1-logLik(imodel3)/logLik(nullmod)
1-logLik(imodel4)/logLik(nullmod)

# Plot interaction effects of age and gender
library(sjPlot)
library(sjmisc)

plot_model(imodel4, type = "pred", terms = c("yob", "female"), axis.title = c("Year of birth", "Probability of selection"),
           title = "Effects of age on selection by gender")
# Save as Figure 3

#-------------------------------------------------------------------------------

# STAGE 5: SUCCESS BY PARTY FACTION

# Create factor variables for party faction
labcands$labfact = factor(labcands$labfact)
labcands$labfact <- fct_collapse(labcands$labfact,
                                 Unknown = c("0"),
                                 Corbynsceptic = c("1"),
                                 Corbynite = c("2"))

# Descriptive statistics of success by Labour faction
table(labcands$success, labcands$labfact)
prop.table(table(labcands$success, labcands$labfact),1)
labfaction <- prop.table(table(labcands$success, labcands$labfact),1)
labfaction[2,]
barplot(labfaction[2,], col = "black", main="Labour selection by faction", ylim = c(0.0, 0.5),
        xlab="Faction", ylab="Proportion of selected candidates", beside=TRUE)
abline(h=0, col = "black")

# Save image as Figure 4

# Now for Conservatives
table(concands$confact, useNA = "ifany")
concands$confact <- as.numeric(concands$confact)
concands$confact <- as.character(concands$confact)
concands$confact <- ifelse(is.na(concands$confact), "NA", concands$confact)
concands$confact <- recode_factor(concands$confact,
                                  "NA" = "Unknown",
                                  "0" = "Remainer",
                                  "1" = "Leaver")
table(concands$confact, useNA = "ifany")
table(concands$success, concands$confact)
prop.table(table(concands$success, concands$confact),1)
confaction <- prop.table(table(concands$success, concands$confact),1)
barplot(confaction[2,], col = "black", main="Conservative selection by EU vote",
        xlab="Faction", ylab="Proportion of selected candidates", ylim = c(0.0, 0.5))
abline(h = 0, col = "black")

# Save image as Figure 5

# Now add faction into the Labour model
labcands <- within(labcands, labfact <- relevel(labfact, ref = "Corbynsceptic"))
labaws <- subset(labcands, aws == 1)
labmixed <- subset(labcands, aws == 0)
LabAllModel <- glmer(success ~ BAME + local + past_selection + elected_cllr + occ_party +  
                       ( 1 | contest), data = labcands, family = binomial(link = "logit"))
LabFactionsModel <- glmer(success ~ BAME + local + past_selection + elected_cllr + occ_party + labfact + 
                            ( 1 | contest), data = labcands, family = binomial(link = "logit"))
LabFactionsAWSModel <- glmer(success ~ BAME + local + past_selection + elected_cllr + occ_party + labfact + 
                               ( 1 | contest), data = labaws, family = binomial(link = "logit"))
LabFactionsMixedModel <- glmer(success ~ gender + BAME + local + past_selection + elected_cllr + occ_party + labfact + 
                                 ( 1 | contest), data = labmixed, family = binomial(link = "logit"))
screenreg(list(LabAllModel, LabFactionsModel, labmodel, LabFactionsMixedModel, 
               awsmodel, LabFactionsAWSModel))
htmlreg(list(LabAllModel, LabFactionsModel, labmodel, LabFactionsMixedModel, 
             awsmodel, LabFactionsAWSModel), file = "Output/Table 4.doc")

nulllab <- glm(labcands$success~1, family="binomial")
1-logLik(LabAllModel)/logLik(nulllab)
1-logLik(LabFactionsModel)/logLik(nulllab)

nulllab <- glm(labaws$success~1, family="binomial")
1-logLik(awsmodel)/logLik(nulllab)
1-logLik(LabFactionsAWSModel)/logLik(nulllab)

nulllab <- glm(labmixed$success~1, family="binomial")
1-logLik(labmodel)/logLik(nulllab)
1-logLik(LabFactionsMixedModel)/logLik(nulllab)

# Now add faction into the Conservative model
concands <- within(concands, confact <- relevel(confact, ref = "Remainer"))
ConFactionsModel <- glmer(success ~ gender + BAME + local + past_selection + elected_cllr + occ_party + confact + 
                            ( 1 | contest), data = concands, family = binomial(link = "logit"))
screenreg(list(conmodel, ConFactionsModel))
htmlreg(list(conmodel, ConFactionsModel), file = "Output/Table 6b.doc")

nullcon <- glm(concands$success~1, family="binomial")
1-logLik(conmodel)/logLik(nullcon)
1-logLik(ConFactionsModel)/logLik(nullcon)

# Predict based on fitted values

selectionmodel <- glmer(success ~ gender + BAME + local + past_selection + elected_cllr + 
                          occ_party + ( 1 | contest), data=candidates, family = binomial(link = "logit"))
new_data <- expand.grid(
  gender = c(0, 1),  
  BAME = c(0, 1),    
  local = 0,
  past_selection = 0,
  elected_cllr = 0,
  occ_party = 0
)

# Calculate the predicted values
predicted_values <- predict(selectionmodel, newdata = new_data, re.form = NA, type = "response")

# Add the predicted values to the new data frame
new_data$predicted_values <- predicted_values

new_data$predicted_values
#------------------------------------------------------------------------------

# Analysis of multiple shortlist candidates

library(dplyr)
multiplecands <- candidates %>%
  filter(party %in% c("Conservative", "Labour"))
table(multiplecands$multiple)
prop.table(table(multiplecands$multiple, multiplecands$local), 1)
prop.table(table(multiplecands$multiple, multiplecands$occupation_meta), 1)
prop.table(table(multiplecands$multiple, multiplecands$occ_party), 1)
prop.table(table(multiplecands$multiple, multiplecands$elected_cllr), 1)
prop.table(table(multiplecands$multiple, multiplecands$past_selection), 1)
prop.table(table(multiplecands$multiple, multiplecands$gender), 1)
prop.table(table(multiplecands$multiple, multiplecands$BAME), 1)

