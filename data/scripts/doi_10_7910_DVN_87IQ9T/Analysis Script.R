setwd("~/Data Availability for COVID PS Article")
set.seed(662607004)

require(tidyverse)
require(foreign)
require(haven)
require(ggplot2)
require(gmodels)
require(Hmisc)
require(car)
require(sandwich)
require(lmtest)
require(MASS)
require(stargazer)
require(pastecs)
require(writexl)

# Creating Table 1: Comparing Distributions of Masking and Social Distancing 
# between Our Sample of Republicans and Axios Sample of American Population 

# Axios Data on Masking and Social Distancing 

Axios <-read.spss("~/Data Availability for COVID PS Article/Axios-Ipsos data W19 Data.sav", to.data.frame=TRUE)

# Q27_2 Asks about masks
mask1<-as.numeric(Axios$Q27_2)
mask1<-mask1[which(mask1!=1)] # Removing option 1 as 'Skipped' is not informative
summary(mask1)

# Q27_3 asks about social distancing
distance1<-as.numeric(Axios$Q27_3)
distance1<-distance1[which(distance1!=1)]
summary(distance1)

# Rescaling Axios masking and social distancing  
# such that more frequent usage is higher coded 

mask1[mask1==2]<-10
mask1[mask1==3]<-9
mask1[mask1==4]<-8
mask1[mask1==5]<-7
mask1[mask1==10]<-4
mask1[mask1==9]<-3
mask1[mask1==8]<-2
mask1[mask1==7]<-1

distance1[distance1==2]<-10
distance1[distance1==3]<-9
distance1[distance1==4]<-8
distance1[distance1==5]<-7
distance1[distance1==10]<-4
distance1[distance1==9]<-3
distance1[distance1==8]<-2
distance1[distance1==7]<-1

w1data <- read_dta("~/Data Availability for COVID PS Article/Wave 1 Prelim Data.dta")

# Question 12 in wave 1 asks about masking

mask2<-w1data$w1q12
mask2<-mask2[which(mask2!=99)] ## 99 As non-response
summary(mask2)

# Question 14 in wave 1 asks about socially distancing

distance2<-w1data$w1q14
distance2<-distance2[which(distance2!=99)] ## 99 As non-response
hist(distance2)

# Comparing Frequencies and Proportions(percentages)

table(mask1)
prop.table(table(mask1))
table(mask2)
prop.table(table(mask2))

table(distance1)
prop.table(table(distance1))
table(distance2)
prop.table(table(distance2))

# Gauging statistical difference between the two samples

compare1a<-log(mask1)
compare1b<-log(mask2)

compare2a<-log(distance1)
compare2b<-log(distance2)

t.test(compare1a, compare1b, alternative = "less")
t.test(compare2a, compare2b, alternative = "less")

ks.test(compare1a, compare1b, alternative = "greater")
ks.test(compare2a, compare2b, alternative = "greater")

compare3a<-mask1/4
compare3b<-mask2/5

compare4a<-distance1/4
compare4b<-distance2/5

t.test(compare3a, compare3b, alternative = "greater")
t.test(compare4a, compare4b, alternative = "greater")

ks.test(compare3a, compare3b, alternative = "less")
ks.test(compare4a, compare4b, alternative = "greater")

# Creating Histograms of the Distributions

x1<-ggplot(data=NULL, aes(mask1)) + geom_histogram(bins=7) + theme_bw() + labs(title=NULL, y="Frequency", x="Mask Wearing Prevalence")
x1<-x1 + scale_x_continuous(breaks=c(1, 2, 3, 4), labels=c('Never', 'Occasionally but \nnot often', 'Sometimes, but not \nall the time', 'At all times')) + theme(axis.text.x = element_text(family="serif", size=34), axis.text.y = element_text(family="serif", size=34), axis.title = element_text(face="bold", family = "serif", size=34))
x1    

x2<-ggplot(data=NULL, aes(mask2)) + geom_histogram(bins=9) + theme_bw() + labs(title=NULL, y="Frequency", x="Mask Wearing Prevalence")
x2<-x2 + scale_x_continuous(breaks=c(1, 2, 3, 4, 5), labels=c("Never", "Rarely", "Some of \nthe Time", "Most of \nthe time", "Always")) + theme(axis.text.x = element_text(family="serif", size=34), axis.text.y = element_text(family="serif", size=34), axis.title = element_text(face="bold", family = "serif", size=34))
x2    

x3<-ggplot(data=NULL, aes(distance1)) + geom_histogram(bins=7) + theme_bw() + labs(title=NULL, y="Frequency", x="Social Distancing Prevalence")
x3<-x3 + scale_x_continuous(breaks=c(1, 2, 3, 4), labels=c("Never", "Occasionally but \nnot often", "Sometimes, but not \nall the time", "At all times")) + theme(axis.text.x = element_text(family="serif", size=34), axis.text.y = element_text(family="serif", size=34), axis.title = element_text(face="bold", family = "serif", size=34))
x3    

x4<-ggplot(data=NULL, aes(distance2)) + geom_histogram(bins=9) + theme_bw() + labs(title=NULL, y="Frequency", x="Social Distancing Prevalence")
x4<-x4 + scale_x_continuous(breaks=c(1, 2, 3, 4, 5), labels=c("Never", "Rarely", "Some of \nthe Time", "Most of \nthe time", "Always")) + theme(axis.text.x = element_text(family="serif", size=34), axis.text.y = element_text(family="serif", size=34), axis.title = element_text(face="bold", family = "serif", size=34))
x4    

# Table 2 

final_data<- read_dta("~/Data Availability for COVID PS Article/Cleaned Data.dta")
attach(final_data)

a1<-lm(BehaveFactor ~ conspir)
a2<-lm(BehaveFactor ~ conspir + female + age + income + edu + ideo + nonwhite + exper)

a3<-lm(BehaveFactor ~ instiTrustGeorgia)
a4<-lm(BehaveFactor ~ instiTrustGeorgia + female + age + income + edu + ideo + nonwhite + exper)

a5<-lm(BehaveFactor ~ instiTrustNational)
a6<-lm(BehaveFactor ~ instiTrustNational + female + age + income + edu + ideo + nonwhite + exper)

qqPlot(a1)
qqPlot(a2) 
qqPlot(a3)
qqPlot(a4)
qqPlot(a5)
qqPlot(a6) #all clear (nothing outside of |3| much less |5|)


r1 <- studres(a1)
hist(r1, freq=FALSE)

r2 <- studres(a2)
hist(r2, freq=FALSE)

r3 <- studres(a3)
hist(r3, freq=FALSE)

r4 <- studres(a4)
hist(r4, freq=FALSE)

r5 <- studres(a5)
hist(r5, freq=FALSE)

r6 <- studres(a6)
hist(r6, freq=FALSE) # No gross deviations from ~N, though r3 is a bit wonky

crPlots(a1)
crPlots(a2)
crPlots(a3)
crPlots(a4)
crPlots(a5)
crPlots(a6)

# not perfect, but no issues severe enough to warrant concern

vif(a2) 
vif(a4) 
vif(a6) 

# No variable inflation factors above 5; limited concern regarding collinearity

ncvTest(a1) # Significant at the 0.05 level
ncvTest(a2) # Significant at the 0.01 level
ncvTest(a3) # Significant at the 0.01 level
ncvTest(a4) # Not significant
ncvTest(a5) # Significant at the 0.01 level
ncvTest(a6) # Not significant

bptest(a1) # Significant at the 0.01 level
bptest(a2) # Significant at the 0.01 level
bptest(a3) # Significant at the 0.01 level
bptest(a4)
bptest(a5) # Significant at the 0.01 level
bptest(a6)

# Using the HC3 calculation of robust standard errors to compensate for non-spherical errors in a1, a2, a3, and a5

cov_a1<-vcovHC(a1, type="HC3")
rob_a1<-sqrt(diag(cov_a1))

cov_a2<-vcovHC(a2, type="HC3")
rob_a2<-sqrt(diag(cov_a2))

cov_a3<-vcovHC(a3, type="HC3")
rob_a3<-sqrt(diag(cov_a3))

cov_a5<-vcovHC(a5, type="HC3")
rob_a5<-sqrt(diag(cov_a5))


stargazer(a1, a2, a3, a4, a5, a6, type = "latex", 
          covariate.labels = c("Conspiratorial Ideation",
                               "Female", "Age", "Income", "Education", "Ideology", 
                               "Person of Color", "COVID-19 Personal Experience", 
                               "Institutional Trust: State", "Institutional Trust: National"), 
          se=list(rob_a1, rob_a2, rob_a3, NULL, rob_a5, NULL),  
          out="~/Data Availability for COVID PS Article/Psych and Trust on Behave.tex", 
          omit.stat = c("rsq", "f", "ser" ), no.space = TRUE)

################################################################################################

# Table A1

write_xlsx(stat.desc(final_data), 
           "~\\Data Availability for COVID PS Article\\DSTable.xlsx", col_names=T)

# Table A3

# Testing that exposure to treatment did not affect other variables
options(digits=5)
chisq.test(final_data$BehaveFactor, final_data$test)
chisq.test(final_data$conspir, final_data$test)
chisq.test(final_data$instiTrustGeorgia, final_data$test)
chisq.test(final_data$instiTrustNational, final_data$test)




# Appendix Table C1-C5

i1<-lm(BehaveFactor ~ conspir*instiTrustGeorgia)
i2<-lm(BehaveFactor ~ conspir*instiTrustGeorgia + female + age + income + edu + ideo + nonwhite + exper)

i3<-lm(BehaveFactor ~ conspir*instiTrustNational)
i4<-lm(BehaveFactor ~ conspir*instiTrustNational + female + age + income + edu + ideo + nonwhite + exper)

qqPlot(i1)
qqPlot(i2) 
qqPlot(i3)
qqPlot(i4) #all clear (nothing outside of |3| much less |5|)


ri1 <- studres(i1)
hist(ri1, freq=FALSE) #~N

ri2 <- studres(i2)
hist(ri2, freq=FALSE) #~N

ri3 <- studres(i3)
hist(ri3, freq=FALSE) #~N

ri4 <- studres(i4)
hist(ri4, freq=FALSE) #~N


# No significant collinearity outside of interaction terms

vif(i2)
vif(i4)

ncvTest(i1) 
ncvTest(i2) # Significant at the 0.05 level
ncvTest(i3) 
ncvTest(i4) 

bptest(i1) # Significant at the 0.05 level
bptest(i2) 
bptest(i3) # Significant at the 0.05 level
bptest(i4)

# Using the HC3 calculation of robust standard errors to compensate for non-spherical errors in a1, a2, a3, and a5

cov_i1<-vcovHC(i1, type="HC3")
rob_i1<-sqrt(diag(cov_i1))

cov_i2<-vcovHC(i2, type="HC3")
rob_i2<-sqrt(diag(cov_i2))

cov_i3<-vcovHC(i3, type="HC3")
rob_i3<-sqrt(diag(cov_i3))

# Table C1
stargazer(i1, i2, i3, i4, type = "latex", 
          se=list(rob_i1, rob_i2, rob_i3, NULL),  
          out="~/Data Availability for COVID PS Article/Interaction of Conspir and Trust on Behave.tex", 
          omit.stat = c("rsq", "f", "ser" ), no.space = TRUE)

# Table C2

c2a<-lm(masking ~ conspir)
c2b<-lm(masking ~ conspir + female + age + income + edu + ideo + nonwhite + exper)

c2c<-lm(masking ~ instiTrustGeorgia)
c2d<-lm(masking ~ instiTrustGeorgia + female + age + income + edu + ideo + nonwhite + exper)

c2e<-lm(masking ~ instiTrustNational)
c2f<-lm(masking ~ instiTrustNational + female + age + income + edu + ideo + nonwhite + exper)

qqPlot(c2a)
qqPlot(c2b)
qqPlot(c2c)
qqPlot(c2d)
qqPlot(c2e)
qqPlot(c2f) #all clear (nothing outside of |3| much less |5|)


rc2a <- studres(c2a)
hist(rc2a, freq=FALSE)

rc2b <- studres(c2b)
hist(rc2b, freq=FALSE)

rc2c <- studres(c2c)
hist(rc2c, freq=FALSE)

rc2d <- studres(c2d)
hist(rc2d, freq=FALSE)

rc2e <- studres(c2e)
hist(rc2e, freq=FALSE)

rc2f <- studres(c2f)
hist(rc2f, freq=FALSE) # most skewed right

crPlots(c2a)
crPlots(c2b)
crPlots(c2c)
crPlots(c2d)
crPlots(c2e)
crPlots(c2f)

# not perfect, but no issues severe enough to warrant concern

vif(c2b) 
vif(c2d) 
vif(c2f) 

# No variable inflation factors above 5; limited concern regarding collinearity

ncvTest(c2a) # Significant at the 0.05 level
ncvTest(c2b) # Significant at the 0.01 level
ncvTest(c2c) # Significant at the 0.01 level
ncvTest(c2d) # Significant at the 0.01 level
ncvTest(c2e) # Significant at the 0.01 level
ncvTest(c2f) # Significant at the 0.05 level

bptest(c2a) # Significant at the 0.01 level
bptest(c2b) # Significant at the 0.01 level
bptest(c2c) # Significant at the 0.01 level
bptest(c2d)
bptest(c2e) # Significant at the 0.01 level
bptest(c2f)

# Using the HC3 calculation of robust standard errors to compensate for non-spherical errors 

cov_c2a<-vcovHC(c2a, type="HC3")
rob_c2a<-sqrt(diag(cov_c2a))

cov_c2b<-vcovHC(c2b, type="HC3")
rob_c2b<-sqrt(diag(cov_c2b))

cov_c2c<-vcovHC(c2c, type="HC3")
rob_c2c<-sqrt(diag(cov_c2c))

cov_c2e<-vcovHC(c2e, type="HC3")
rob_c2e<-sqrt(diag(cov_c2e))


stargazer(c2a, c2b, c2c, c2d, c2e, c2f, type = "latex", 
          covariate.labels = c("Conspiratorial Ideation",
                               "Female", "Age", "Income", "Education", "Ideology", 
                               "Person of Color", "COVID-19 Personal Experience", 
                               "Institutional Trust: State", "Institutional Trust: National"), 
          se=list(rob_c2a, rob_c2b, rob_c2c, NULL, rob_c2e, NULL),  
          out="~/Data Availability for COVID PS Article/Psych and Trust on Masking", 
          omit.stat = c("rsq", "f", "ser" ), no.space = TRUE)



# Table c3

c3a<-lm(distancing ~ conspir)
c3b<-lm(distancing ~ conspir + female + age + income + edu + ideo + nonwhite + exper)

c3c<-lm(distancing ~ instiTrustGeorgia)
c3d<-lm(distancing ~ instiTrustGeorgia + female + age + income + edu + ideo + nonwhite + exper)

c3e<-lm(distancing ~ instiTrustNational)
c3f<-lm(distancing ~ instiTrustNational + female + age + income + edu + ideo + nonwhite + exper)

qqPlot(c3a)
qqPlot(c3b)
qqPlot(c3c)
qqPlot(c3d)
qqPlot(c3e)
qqPlot(c3f) #all clear (nothing outside of |3| much less |5|)


rc3a <- studres(c3a)
hist(rc3a, freq=FALSE)

rc3b <- studres(c3b)
hist(rc3b, freq=FALSE)

rc3c <- studres(c3c)
hist(rc3c, freq=FALSE)

rc3d <- studres(c3d)
hist(rc3d, freq=FALSE)

rc3e <- studres(c3e)
hist(rc3e, freq=FALSE)

rc3f <- studres(c3f)
hist(rc3f, freq=FALSE) # most skewed right

crPlots(c3a)
crPlots(c3b)
crPlots(c3c)
crPlots(c3d)
crPlots(c3e)
crPlots(c3f)

# not perfect, but no issues severe enough to warrant concern

vif(c3b) 
vif(c3d) 
vif(c3f) 

# No variable inflation factors above 5; limited concern regarding collinearity

ncvTest(c3a) # Significant at the 0.01 level
ncvTest(c3b) # Significant at the 0.01 level
ncvTest(c3c) # Significant at the 0.01 level
ncvTest(c3d) # Significant at the 0.1 level
ncvTest(c3e) # Significant at the 0.01 level
ncvTest(c3f) # Significant at the 0.05 level

bptest(c3a) # Significant at the 0.01 level
bptest(c3b) # Significant at the 0.01 level
bptest(c3c) # Significant at the 0.01 level
bptest(c3d)
bptest(c3e) # Significant at the 0.01 level
bptest(c3f)

# Using the HC3 calculation of robust standard errors to compensate for non-spherical errors 

cov_c3a<-vcovHC(c3a, type="HC3")
rob_c3a<-sqrt(diag(cov_c3a))

cov_c3b<-vcovHC(c3b, type="HC3")
rob_c3b<-sqrt(diag(cov_c3b))

cov_c3c<-vcovHC(c3c, type="HC3")
rob_c3c<-sqrt(diag(cov_c3c))

cov_c3e<-vcovHC(c3e, type="HC3")
rob_c3e<-sqrt(diag(cov_c3e))


stargazer(c3a, c3b, c3c, c3d, c3e, c3f, type = "latex", 
          covariate.labels = c("Conspiratorial Ideation",
                               "Female", "Age", "Income", "Education", "Ideology", 
                               "Person of Color", "COVID-19 Personal Experience", 
                               "Institutional Trust: State", "Institutional Trust: National"), 
          se=list(rob_c3a, rob_c3b, rob_c3c, NULL, rob_c3e, NULL),  
          out="~/Data Availability for COVID PS Article/Psych and Trust on Distancing", 
          omit.stat = c("rsq", "f", "ser" ), no.space = TRUE)


# Table c4

c4a<-lm(encouraging ~ conspir)
c4b<-lm(encouraging ~ conspir + female + age + income + edu + ideo + nonwhite + exper)

c4c<-lm(encouraging ~ instiTrustGeorgia)
c4d<-lm(encouraging ~ instiTrustGeorgia + female + age + income + edu + ideo + nonwhite + exper)

c4e<-lm(encouraging ~ instiTrustNational)
c4f<-lm(encouraging ~ instiTrustNational + female + age + income + edu + ideo + nonwhite + exper)

qqPlot(c4a)
qqPlot(c4b)
qqPlot(c4c)
qqPlot(c4d)
qqPlot(c4e)
qqPlot(c4f) #all clear (nothing outside of |3| much less |5|)


rc4a <- studres(c4a)
hist(rc4a, freq=FALSE)

rc4b <- studres(c4b)
hist(rc4b, freq=FALSE)

rc4c <- studres(c4c)
hist(rc4c, freq=FALSE)

rc4d <- studres(c4d)
hist(rc4d, freq=FALSE)

rc4e <- studres(c4e)
hist(rc4e, freq=FALSE)

rc4f <- studres(c4f)
hist(rc4f, freq=FALSE) # most skewed right

crPlots(c4a)
crPlots(c4b)
crPlots(c4c)
crPlots(c4d)
crPlots(c4e)
crPlots(c4f)

# not perfect, but no issues severe enough to warrant concern

vif(c4b) 
vif(c4d) 
vif(c4f) 

# No variable inflation factors above 5; limited concern regarding collinearity

ncvTest(c4a) # Significant at the 0.01 level
ncvTest(c4b) # Significant at the 0.01 level
ncvTest(c4c) # Significant at the 0.01 level
ncvTest(c4d) # Significant at the 0.1 level
ncvTest(c4e) # Significant at the 0.01 level
ncvTest(c4f) # Significant at the 0.05 level

bptest(c4a) # Significant at the 0.01 level
bptest(c4b) # Significant at the 0.01 level
bptest(c4c) # Significant at the 0.01 level
bptest(c4d)
bptest(c4e) # Significant at the 0.01 level
bptest(c4f)

# Using the Hc4 calculation of robust standard errors to compensate for non-spherical errors 

cov_c4a<-vcovHC(c4a, type="HC3")
rob_c4a<-sqrt(diag(cov_c4a))

cov_c4b<-vcovHC(c4b, type="HC3")
rob_c4b<-sqrt(diag(cov_c4b))

cov_c4c<-vcovHC(c4c, type="HC3")
rob_c4c<-sqrt(diag(cov_c4c))

cov_c4e<-vcovHC(c4e, type="HC3")
rob_c4e<-sqrt(diag(cov_c4e))


stargazer(c4a, c4b, c4c, c4d, c4e, c4f, type = "latex", 
          covariate.labels = c("Conspiratorial Ideation",
                               "Female", "Age", "Income", "Education", "Ideology", 
                               "Person of Color", "COVID-19 Personal Experience", 
                               "Institutional Trust: State", "Institutional Trust: National"), 
          se=list(rob_c4a, rob_c4b, rob_c4c, NULL, rob_c4e, NULL),  
          out="~/Data Availability for COVID PS Article/Psych and Trust on Encouraging", 
          omit.stat = c("rsq", "f", "ser" ), no.space = TRUE)


# Table c5

c5a<-lm(encouraging ~ conspir)
c5b<-lm(encouraging ~ conspir + female + age + income + edu + nonwhite + exper)

c5c<-lm(encouraging ~ instiTrustGeorgia)
c5d<-lm(encouraging ~ instiTrustGeorgia + female + age + income + edu + nonwhite + exper)

c5e<-lm(encouraging ~ instiTrustNational)
c5f<-lm(encouraging ~ instiTrustNational + female + age + income + edu + nonwhite + exper)

qqPlot(c5a)
qqPlot(c5b)
qqPlot(c5c)
qqPlot(c5d)
qqPlot(c5e)
qqPlot(c5f) #all clear (nothing outside of |3| much less |5|)


rc5a <- studres(c5a)
hist(rc5a, freq=FALSE)

rc5b <- studres(c5b)
hist(rc5b, freq=FALSE)

rc5c <- studres(c5c)
hist(rc5c, freq=FALSE)

rc5d <- studres(c5d)
hist(rc5d, freq=FALSE)

rc5e <- studres(c5e)
hist(rc5e, freq=FALSE)

rc5f <- studres(c5f)
hist(rc5f, freq=FALSE) # most skewed right

crPlots(c5a)
crPlots(c5b)
crPlots(c5c)
crPlots(c5d)
crPlots(c5e)
crPlots(c5f)

# not perfect, but no issues severe enough to warrant concern

vif(c5b) 
vif(c5d) 
vif(c5f) 

# No variable inflation factors above 5; limited concern regarding collinearity

ncvTest(c5a) # Significant at the 0.01 level
ncvTest(c5b) # Significant at the 0.01 level
ncvTest(c5c) # Significant at the 0.01 level
ncvTest(c5d) # Significant at the 0.1 level
ncvTest(c5e) # Significant at the 0.01 level
ncvTest(c5f) # Significant at the 0.05 level

bptest(c5a) # Significant at the 0.01 level
bptest(c5b) # Significant at the 0.01 level
bptest(c5c) # Significant at the 0.01 level
bptest(c5d)
bptest(c5e) # Significant at the 0.01 level
bptest(c5f)

# Using the Hc5 calculation of robust standard errors to compensate for non-spherical errors 

cov_c5a<-vcovHC(c5a, type="HC3")
rob_c5a<-sqrt(diag(cov_c5a))

cov_c5b<-vcovHC(c5b, type="HC3")
rob_c5b<-sqrt(diag(cov_c5b))

cov_c5c<-vcovHC(c5c, type="HC3")
rob_c5c<-sqrt(diag(cov_c5c))

cov_c5e<-vcovHC(c5e, type="HC3")
rob_c5e<-sqrt(diag(cov_c5e))


stargazer(c5a, c5b, c5c, c5d, c5e, c5f, type = "latex", 
          covariate.labels = c("Conspiratorial Ideation",
                              "Female", "Age", "Income", "Education",  
                               "Person of Color", "COVID-19 Personal Experience", 
                              "Institutional Trust: State", "Institutional Trust: Federal"), 
          se=list(rob_c5a, rob_c5b, rob_c5c, NULL, rob_c5e, NULL),  
          out="~/Data Availability for COVID PS Article/Psych and Trust on Behaviors without Ideology", 
          omit.stat = c("rsq", "f", "ser" ), no.space = TRUE)

# Table D1 and D2

## Getting the results for sobel's z-test 

sztest<-function(a,sda,b,sdb){
  (a*b)/sqrt(((a^2)*(sdb^2))+((b^2)*(sda^2)))}

sztest( -0.6335927, 0.098837,  0.1850627 ,  0.0453948   )

dt( -3.440033, df=187)

sztest( -0.294, 0.097,  0.076,  0.047   )

dt(-1.426681, df=187)

