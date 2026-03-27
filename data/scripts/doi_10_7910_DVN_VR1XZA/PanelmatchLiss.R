library(tidyverse)
library(PanelMatch)
library(fastDummies)


df <-read.csv("Lisspanel.csv")

df <- df %>% drop_na(tid, pid)

df <- dummy_cols(df, select_columns = 'oplcat')
df <- dummy_cols(df, select_columns = 'employ')

p <- DisplayTreatment(unit.id = "pid",
                 time.id = "tid", legend.position = "none",
                               dense.plot = TRUE,
                 treatment = "bill", data = df)
p
## GWE on Voting
PM.results <- PanelMatch(lag = 4, time.id = "tid", unit.id = "pid", 
                         treatment = "bill", refinement.method = "mahalanobis", 
                         data = df, match.missing = TRUE, 
                         covs.formula = ~ lninc + `oplcat_primary school` + `oplcat_junior high (vmbo)`+ `oplcat_hbo (higher vocational)` + `oplcat_have/vwo (HS)` + employ_Employed + employ_Housekeeper+employ_Pensioner + `employ_Self-employed` +employ_Student,
                         size.match = 5, qoi = "att" ,outcome.var = "extremeright",
                         lead = 0:4, forbid.treatment.reversal = FALSE)

mset <- PM.results



PE.results <- PanelEstimate(sets = PM.results, data = df)


s1 <- summary(PE.results)

p1 <- plot(PE.results)



## GWE on sympathy
PM.results <- PanelMatch(lag = 4, time.id = "tid", unit.id = "pid", 
                         treatment = "bill", refinement.method = "mahalanobis", 
                         data = df, match.missing = TRUE, 
                         covs.formula = ~ lninc + `oplcat_primary school` + `oplcat_junior high (vmbo)`+ `oplcat_hbo (higher vocational)` + `oplcat_have/vwo (HS)` + employ_Employed + employ_Housekeeper+employ_Pensioner + `employ_Self-employed` +employ_Student,
                         size.match = 5, qoi = "att" ,outcome.var = "PVVsympmean",
                         lead = 0:4, forbid.treatment.reversal = FALSE)

mset <- PM.results


PE.results <- PanelEstimate(sets = PM.results, data = df)


s2 <- summary(PE.results)

p2 <- plot(PE.results)


## poor on Voting
PM.results <- PanelMatch(lag = 4, time.id = "tid", unit.id = "pid", 
                         treatment = "poor", refinement.method = "mahalanobis", 
                         data = df, match.missing = TRUE, 
                         covs.formula = ~ lninc + `oplcat_primary school` + `oplcat_junior high (vmbo)`+ `oplcat_hbo (higher vocational)` + `oplcat_have/vwo (HS)` + employ_Employed + employ_Housekeeper+employ_Pensioner + `employ_Self-employed` +employ_Student,
                         size.match = 5, qoi = "att" ,outcome.var = "extremeright",
                         lead = 0:4, forbid.treatment.reversal = FALSE)

mset <- PM.results



PE.results <- PanelEstimate(sets = PM.results, data = df)


s3 <- summary(PE.results)

p3 <- plot(PE.results)

## Poor on sympathy
PM.results <- PanelMatch(lag = 4, time.id = "tid", unit.id = "pid", 
                         treatment = "poor", refinement.method = "mahalanobis", 
                         data = df, match.missing = TRUE, 
                         covs.formula = ~ lninc + `oplcat_primary school` + `oplcat_junior high (vmbo)`+ `oplcat_hbo (higher vocational)` + `oplcat_have/vwo (HS)` + employ_Employed + employ_Housekeeper+employ_Pensioner + `employ_Self-employed` +employ_Student,
                         size.match = 5, qoi = "att" ,outcome.var = "PVVsympmean",
                         lead = 0:4, forbid.treatment.reversal = FALSE)

mset <- PM.results


PE.results <- PanelEstimate(sets = PM.results, data = df)


s4 <- summary(PE.results)

p4 <- plot(PE.results)





## GWE on Voting
PM.results <- PanelMatch(lag = 4, time.id = "tid", unit.id = "pid", 
                         treatment = "bill", refinement.method = "mahalanobis", 
                         data = df, match.missing = TRUE, 
                         covs.formula = ~ lninc + `oplcat_primary school` + `oplcat_junior high (vmbo)`+ `oplcat_hbo (higher vocational)` + `oplcat_have/vwo (HS)` + employ_Employed + employ_Housekeeper+employ_Pensioner + `employ_Self-employed` +employ_Student,
                         size.match = 5, qoi = "att" ,outcome.var = "extremeright",
                         lead = 0:4, forbid.treatment.reversal = TRUE)

mset <- PM.results



PE.results <- PanelEstimate(sets = PM.results, data = df)


s5 <- summary(PE.results)

p5 <- plot(PE.results)

## GWE on sympathy
PM.results <- PanelMatch(lag = 4, time.id = "tid", unit.id = "pid", 
                         treatment = "bill", refinement.method = "mahalanobis", 
                         data = df, match.missing = TRUE, 
                         covs.formula = ~ lninc + `oplcat_primary school` + `oplcat_junior high (vmbo)`+ `oplcat_hbo (higher vocational)` + `oplcat_have/vwo (HS)` + employ_Employed + employ_Housekeeper+employ_Pensioner + `employ_Self-employed` +employ_Student,
                         size.match = 5, qoi = "att" ,outcome.var = "PVVsympmean",
                         lead = 0:4, forbid.treatment.reversal = TRUE)

mset <- PM.results


PE.results <- PanelEstimate(sets = PM.results, data = df)


s6 <- summary(PE.results)

p6 <- plot(PE.results)


## poor on Voting
PM.results <- PanelMatch(lag = 4, time.id = "tid", unit.id = "pid", 
                         treatment = "poor", refinement.method = "mahalanobis", 
                         data = df, match.missing = TRUE, 
                         covs.formula = ~ lninc + `oplcat_primary school` + `oplcat_junior high (vmbo)`+ `oplcat_hbo (higher vocational)` + `oplcat_have/vwo (HS)` + employ_Employed + employ_Housekeeper+employ_Pensioner + `employ_Self-employed` +employ_Student,
                         size.match = 5, qoi = "att" ,outcome.var = "extremeright",
                         lead = 0:4, forbid.treatment.reversal = TRUE)

mset <- PM.results



PE.results <- PanelEstimate(sets = PM.results, data = df)


s7 <- summary(PE.results)

p7 <- plot(PE.results)

## Poor on sympathy
PM.results <- PanelMatch(lag = 4, time.id = "tid", unit.id = "pid", 
                         treatment = "poor", refinement.method = "mahalanobis", 
                         data = df, match.missing = TRUE, 
                         covs.formula = ~ lninc + `oplcat_primary school` + `oplcat_junior high (vmbo)`+ `oplcat_hbo (higher vocational)` + `oplcat_have/vwo (HS)` + employ_Employed + employ_Housekeeper+employ_Pensioner + `employ_Self-employed` +employ_Student,
                         size.match = 5, qoi = "att" ,outcome.var = "PVVsympmean",
                         lead = 0:4, forbid.treatment.reversal = TRUE)

mset <- PM.results


PE.results <- PanelEstimate(sets = PM.results, data = df)


s8 <- summary(PE.results)

p8 <- plot(PE.results)


 