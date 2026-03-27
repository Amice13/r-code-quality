## Cross Tabulation

library(gmodels)
Sendai$Age <- factor(Sendai$Age)
Sendai$Gender <- factor(Sendai$Gender)
Sendai$Status <- factor(Sendai$Status)
Sendai$`Beginning of Membership` <- factor(Sendai$`Beginning of Membership`)
Sendai$`Member Origin` <- factor(Sendai$`Member Origin`)
Sendai$`Health/Safety/Vulnerability Concerns` <- factor(Sendai$`Health/Safety/Vulnerability Concerns`)
Sendai$`Money/Influence of Nuclear Village Concerns` <- factor(Sendai$`Money/Influence of Nuclear Village Concerns`)
Sendai$`Peace Issues` <- factor(Sendai$`Peace Issues`)
Sendai$`Duration of Membership` <- factor(Sendai$`Duration of Membership`)

CrossTable(Sendai$`Duration of Membership`, Sendai$`Health/Safety/Vulnerability Concerns`, chisq = TRUE, format = "SAS", simulate.p.value = TRUE)
CrossTable(Sendai$`Duration of Membership`, Sendai$`Money/Influence of Nuclear Village Concerns`, chisq = TRUE, format = "SAS", simulate.p.value = TRUE)
CrossTable(Sendai$`Duration of Membership`, Sendai$`Peace Issues`, chisq = TRUE, format = "SAS", simulate.p.value = TRUE)
CrossTable(Sendai$`Duration of Membership`, Sendai$Age, chisq = TRUE, format = "SAS", simulate.p.value = TRUE)
CrossTable(Sendai$`Duration of Membership`, Sendai$Gender, chisq = TRUE, format = "SAS", simulate.p.value = TRUE)
CrossTable(Sendai$`Duration of Membership`, Sendai$`Member Origin`, chisq = TRUE, format = "SAS", simulate.p.value = TRUE)
CrossTable(Sendai$`Duration of Membership`, Sendai$Status, chisq = TRUE, format = "SAS", simulate.p.value = TRUE)
CrossTable(Sendai$`Duration of Membership`, Sendai$`Beginning of Membership`, chisq = TRUE, format = "SAS", simulate.p.value = TRUE)

