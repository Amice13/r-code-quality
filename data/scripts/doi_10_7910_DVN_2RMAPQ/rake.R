### Categories from the data are:
### Male: Female, Male
### Age: 18-24, 25-34,35-44,45-54,55-64,65 or Over
### White: Non Whites; White
### Income: Less than $25,000, $25,000-$49,999, $50,000-$74,999,
### $75000-$99,999, $100,000-$124,999, $125,000-$149,999,
### $150,000-$174,999, ($175,000-$199,999, More than $200,000)

library(weights)
library(haven)

makito_all<-read_dta("levy_replication_weighted.dta")
makito<-na.omit(makito_all)

makito$Male<-makito$Male + 1
makito$White <- makito$White + 1

# Set percentages from the 2020 ANES.

Male<-c(.52,.48)
Age<-c(.11,.16,.17,.16,.18,.22)
White<-c(.34,.66)
Income<-c(.12,.17,.17,.12,.15,.05,.07,.14)

targets<-list(Male,Age,White,Income)
names(targets)<-c("Male","Age","White","Income")

library(anesrake)

makito<-as.data.frame(makito)

set.seed(321)

anesrakefinder(targets,makito,choosemethod="total")

outsave<-anesrake(targets, makito , caseid=makito$caseid,
  verbose=FALSE, cap=5, choosemethod="total", type="pctlim",
  pctlim=.05, nlim=5, iterate=TRUE, force1=TRUE)

summary(outsave)

makito$weightvec<-unlist(outsave[1])
n <- length(makito$Male)
((sum(makito$weightved^2)/(sum(makito$weightvec))^2)*n) -1

wpct(makito_all$Male)
wpct(makito_all$Age)
wpct(makito_all$White)
wpct(makito_all$Income)

sink("models.out",append=FALSE)
wpct(makito_all$Male)
wpct(makito_all$Age)
wpct(makito_all$White)
wpct(makito_all$Income)

print("Unweighted Percentages for Male, Age, White, and Income")
wpct(makito$Male)
wpct(makito$Age)
wpct(makito$White)
wpct(makito$Income)

print("Weighted Percentages for Male, Age, White, and Income")
wpct(makito$Male,weight=makito$weightvec)
wpct(makito$Age,weight=makito$weightvec)
wpct(makito$White,weight=makito$weightvec)
wpct(makito$Income,weight=makito$weightvec)

makito$Male<-makito$Male - 1
makito$White <- makito$White - 1

summary(lm(Approval~BackOut + Male + White + Age + Income + Democrat + Ideology,data=makito,weights=weightvec,subset=(Condition==1 | Condition==3)))
summary(lm(Approval~BackIn + Male + White + Age + Income + Democrat + Ideology,data=makito,weights=weightvec,subset=(Condition==2 | Condition==4)))
sink()
