###################################################
###################################################
##  Gun Control Attitudes Code
###################################################
###################################################
# Install and load the package that allows us to read in a Stata 13 or higher version file. 
install.packages("readstata13")
library(readstata13)

# Load the dataset into an object that we have called "dat".
dat <- read.dta13(file.choose(), convert.factors=FALSE)

# Install and load the packages that allows us to recode variables using the "recode" command.
install.packages("car")
library(car)



###################################################
##  Exploring and Recoding Socio-demographic Variables
###################################################
# Age
table(dat$birthyr)

dat$age <- 2022 - dat$birthyr
table(dat$age)
summary(dat$age)
sd(na.omit(dat$age))

# Gender
table(dat$gender4)

dat$gender <- recode(dat$gender4, "1=0; 2=1; else=NA")
dat$gender <- as.factor(as.numeric(dat$gender))
table(dat$gender)
prop.table(table(dat$gender))

# Race
table(dat$race)

dat$race1 <- recode(dat$race, "1 = 'White'; 2 = 'African American'; 3 = 'Hispanic'; else='Other'")
dat$race1 <- factor(dat$race1, c("White", "African American", "Hispanic", "Other"))
prop.table(table(dat$race1))

# Education
table(dat$educ)
prop.table(table(dat$educ))
summary(dat$educ)
sd(na.omit(dat$educ))

# Family Income
table(dat$faminc_new)

dat$income <- recode(dat$faminc_new, "97=NA")
summary(dat$income)
sd(na.omit(dat$income))

# Political Ideology
table(dat$CC22_340a)

dat$ideology <- recode(dat$CC22_340a, "8=NA")
table(dat$ideology)
summary(dat$ideology)
sd(na.omit(dat$ideology))

# Partisan Identification
table(dat$pid7)

dat$PID <- recode(dat$pid7, "1:3 = 'Dem'; 4 = 'Ind'; 5:7 = 'Rep'; 8 = 'Ind'")


dat$PID2 <- recode(dat$pid7, "8 = 4")

dat$PID3 <- recode(dat$pid7, "1:3=1; 4=2; 8=2; 5:7=3")

table(dat$PID)
prop.table(table(dat$PID))

testdat2 <- na.omit(data.frame(
PID2 = dat$PID2, 
ideology = dat$ideology
))

cor(testdat2)
## Correlated at .716.

testdat3 <- na.omit(data.frame(
PID3 = dat$PID3, 
ideology = dat$ideology
))

cor(testdat3)
## Correlated at .705.



###################################################
##  Exploring and Recoding Trust Variables (post-election survey)
###################################################
# How much trust do you have in the federal government in Washington when it comes to handling the nation’s problems? (9,064 NAs)
table(dat$CC22_423)

dat$trustf <- recode(dat$CC22_423, "8=0; 3=1; 2=2; 1=3")
table(dat$trustf)
prop.table(table(dat$trustf))
summary(dat$trustf)
sd(na.omit(dat$trustf))

# How much trust do you have in the government of the state where you live when it comes to handling the nation’s problems? (9,064 NAs)
table(dat$CC22_424)

dat$trusts <- recode(dat$CC22_424, "8=0; 3=1; 2=2; 1=3")
table(dat$trusts)
prop.table(table(dat$trusts))

## Checking correlation between trust variables
testdat1 <- na.omit(data.frame(
trustf = dat$trustf,
trusts = dat$trusts
))

# The results indicate a fairly moderate level of correlation at .458. Therefore, we might not want to use both variables in a model.
cor(testdat1)



###################################################
##  Exploring and Recoding Gun Control Questions (pre-election survey)
###################################################
## Prohibit state and local governments from publishing the names and addresses of all gun owners
table(dat$CC22_330a)

# Recoding the variable
dat$publish <- recode(dat$CC22_330a, "1=1; 2=0")
# Checking that the recode was correct.
table(dat$publish)
# Getting proportion for the nominal level variable.
prop.table(table(dat$publish))
# Getting proportion for the nominal level variable by partisan identification.
prop.table(table(dat$publish, dat$PID), 2)
# Estimating a bivariate model testing whether PID is a predictor of support.
summary(glm(publish ~ PID, data=dat, family=binomial))
# Getting the mean level of trust in federal government by support and opposition.
tapply(dat$trustf, dat$publish, summary)
# Estimating a bivariate model predicting trust based on support for policy.
summary(lm(trustf ~ publish, data=dat))

## Ban assault rifles 
table(dat$CC22_330b)

dat$banassault <- recode(dat$CC22_330b, "1=1; 2=0")
table(dat$banassault)
prop.table(table(dat$banassault))
prop.table(table(dat$banassault, dat$PID), 2)
summary(glm(banassault ~ PID, data=dat, family=binomial))
tapply(dat$trustf, dat$banassault, summary)
summary(lm(trustf ~ banassault, data=dat))

## Make it easier for people to obtain concealed-carry permit
table(dat$CC22_330c)

dat$ccarry <- recode(dat$CC22_330c, "1=1; 2=0")
table(dat$ccarry)
prop.table(table(dat$ccarry))
prop.table(table(dat$ccarry, dat$PID), 2)
summary(glm(ccarry ~ PID, data=dat, family=binomial))
tapply(dat$trustf, dat$ccarry, summary)
summary(lm(trustf ~ ccarry, data=dat))

## Provide federal funding to encourage states to take guns away from people who already own them but might pose a threat to themselves or others
table(dat$CC22_330d)

dat$takeguns <- recode(dat$CC22_330d, "1=1; 2=0")
table(dat$takeguns)
prop.table(table(dat$takeguns))
prop.table(table(dat$takeguns, dat$PID), 2)
summary(glm(takeguns ~ PID, data=dat, family=binomial))
tapply(dat$trustf, dat$takeguns, summary)
summary(lm(trustf ~ takeguns, data=dat))

##  Improve background checks to give authorities time to check the juvenile and mental health records of any prospective gun buyer under the age of 21
table(dat$CC22_330e)

dat$background <- recode(dat$CC22_330e, "1=1; 2=0")
table(dat$background)
prop.table(table(dat$background))
prop.table(table(dat$background, dat$PID), 2)
summary(glm(background ~ PID, data=dat, family=binomial))
tapply(dat$trustf, dat$background, summary)
summary(lm(trustf ~ background, data=dat))

## Allow teachers and school officials to carry guns in public schools
table(dat$CC22_330f)

dat$gschools <- recode(dat$CC22_330f, "1=1; 2=0")
table(dat$gschools)
prop.table(table(dat$gschools))
prop.table(table(dat$gschools, dat$PID), 2)
summary(glm(gschools ~ PID, data=dat, family=binomial))
tapply(dat$trustf, dat$gschools, summary)
summary(lm(trustf ~ gschools, data=dat))



###################################################
##  Estimating Models to Predict Gun Reform
###################################################
# We can use the "glm" command to estimate a generalized linear model. However, we must also include the "family=binomial" command to indicate that we want to estimate logistic regression in particular.
# The weight variable is "commonweight".

mod1 <- glm(publish ~ age + gender + race1 + educ + income + ideology + PID + trustf, data=dat, family=binomial, weight=commonweight)
mod2 <- glm(banassault ~ age + gender + race1 + educ + income + ideology + PID + trustf, data=dat, family=binomial, weight=commonweight)
mod3 <- glm(ccarry ~ age + gender + race1 + educ + income + ideology + PID + trustf, data=dat, family=binomial, weight=commonweight)
mod4 <- glm(takeguns ~ age + gender + race1 + educ + income + ideology + PID + trustf, data=dat, family=binomial, weight=commonweight)
mod5 <- glm(background ~ age + gender + race1 + educ + income + ideology + PID + trustf, data=dat, family=binomial, weight=commonweight)
mod6 <- glm(gschools ~ age + gender + race1 + educ + income + ideology + PID + trustf, data=dat, family=binomial, weight=commonweight)

install.packages("stargazer")
library(stargazer)
# Stargazer allows us to create tables that could be imported into word or latex. 
stargazer(mod2, mod4, mod5, mod1, mod3, mod6)

stargazer(mod2, mod4, mod5, mod1, mod3, mod6, type = "html", dep.var.labels = c("Ban <br> Assault <br> Rifles", "Take <br> Away <br> Guns", "Improve <br> Background <br> Checks", "Prohibit <br> Publish <br> Names", "Conceal <br> Carry <br> Easier", "Carry <br> Guns <br> School"), covariate.labels = c("Age", "Woman", "Race - Black", "Race - Hispanic", "Race - Other", "Education", "Income", "Political Ideology", "Party ID - Independent", "Party ID - Republican", "Trust - Federal Gov", "Constant"), no.space = TRUE, star.cutoffs = c(.05, .01), digits=2, out = "GunControlModels.html")



###################################################
##  Interactive Models
###################################################
# Here, we are estimating models with interactive effects between partisanship and trust in the federal government. The "*" symbol indicates to calculate an interaction.
# The weight variable is "commonweight".

mod7 <- glm(publish ~ age + gender + race1 + educ + income + ideology + PID*trustf, data=dat, family=binomial, weight=commonweight)
mod8 <- glm(banassault ~ age + gender + race1 + educ + income + ideology + PID*trustf, data=dat, family=binomial, weight=commonweight)
mod9 <- glm(ccarry ~ age + gender + race1 + educ + income + ideology + PID*trustf, data=dat, family=binomial, weight=commonweight)
mod10 <- glm(takeguns ~ age + gender + race1 + educ + income + ideology + PID*trustf, data=dat, family=binomial, weight=commonweight)
mod11 <- glm(background ~ age + gender + race1 + educ + income + ideology + PID*trustf, data=dat, family=binomial, weight=commonweight)
mod12 <- glm(gschools ~ age + gender + race1 + educ + income + ideology + PID*trustf, data=dat, family=binomial, weight=commonweight)

stargazer(mod8, mod10, mod11, mod7, mod9, mod12)

stargazer(mod8, mod10, mod11, mod7, mod9, mod12, type = "html", dep.var.labels = c("Ban <br> Assault <br> Rifles", "Take <br> Away <br> Guns", "Improve <br> Background <br> Checks", "Prohibit <br> Publish <br> Names", "Conceal <br> Carry <br> Easier", "Carry <br> Guns <br> School"), covariate.labels = c("Age", "Woman", "Race - Black", "Race - Hispanic", "Race - Other", "Education", "Income", "Political Ideology", "Party ID - Independent", "Party ID - Republican", "Trust - Federal Gov", "Independent*Trust Federal", "Republican*Trust Federal", "Constant"), no.space = TRUE, star.cutoffs = c(.05, .01), digits=2, out = "GunControlInteractionModels.html")



###################################################
##  Plotting Effects for Basic Models
###################################################
install.packages("lattice")
library(lattice)
install.packages("latticeExtra")
library(latticeExtra)
install.packages("effects")
library(effects)

eff1 <- effect("trustf", mod1, default.levels=1000)
trellis.device(color = FALSE)
effect1 <- print(plot(eff1, rescale.axis=F, rug=FALSE, xlab="Trust in Federal Government", ylab="Probability of Support", main="Prohibit Publish Names", ylim=c(0,1)))

eff2 <- effect("trustf", mod2, default.levels=1000)
trellis.device(color = FALSE)
effect2 <- print(plot(eff2, rescale.axis=F, rug=FALSE, xlab="Trust in Federal Government", ylab="Probability of Support", main="Ban Assault Rifles", ylim=c(0,1)))

eff3 <- effect("trustf", mod3, default.levels=1000)
trellis.device(color = FALSE)
effect3 <- print(plot(eff3, rescale.axis=F, rug=FALSE, xlab="Trust in Federal Government", ylab="Probability of Support", main="Conceal Carry Easier", ylim=c(0,1)))

eff4 <- effect("trustf", mod4, default.levels=1000)
trellis.device(color = FALSE)
effect4 <- print(plot(eff4, rescale.axis=F, rug=FALSE, xlab="Trust in Federal Government", ylab="Probability of Support", main="Take Away Guns", ylim=c(0,1)))

eff5 <- effect("trustf", mod5, default.levels=1000)
trellis.device(color = FALSE)
effect5 <- print(plot(eff5, rescale.axis=F, rug=FALSE, xlab="Trust in Federal Government", ylab="Probability of Support", main="Improve Background Checks", ylim=c(0,1)))

eff6 <- effect("trustf", mod6, default.levels=1000)
trellis.device(color = FALSE)
effect6 <- print(plot(eff6, rescale.axis=F, rug=FALSE, xlab="Trust in Federal Government", ylab="Probability of Support", main="Carry Guns Schools", ylim=c(0,1)))

TrustEffect <- ggpubr::ggarrange(effect2,  effect4, effect5, effect1, effect3, effect6, ncol = 3, nrow = 2)



###################################################
##  Plotting Effects for Interactive Models
###################################################
eff7 <- effect("PID*trustf", mod7, default.levels=1000)
trellis.device(color = FALSE)
effect7 <- print(plot(eff7, rescale.axis=F, rug=FALSE, xlab="", ylab="Probability of Support", main="Prohibit Publish Names", ylim=c(0,.85), layout=c(3,1)))

eff8 <- effect("PID*trustf", mod8, default.levels=1000)
trellis.device(color = FALSE)
effect8 <- print(plot(eff8, rescale.axis=F, rug=FALSE, xlab="", ylab="Probability of Support", main="Ban Assault Rifles", ylim=c(.2,1), layout=c(3,1)))

eff9 <- effect("PID*trustf", mod9, default.levels=1000)
trellis.device(color = FALSE)
effect9 <- print(plot(eff9, rescale.axis=F, rug=FALSE, xlab="", ylab="Probability of Support", main="Conceal Carry Easier", ylim=c(0,.85), layout=c(3,1)))

eff10 <- effect("PID*trustf", mod10, default.levels=1000)
trellis.device(color = FALSE)
effect10 <- print(plot(eff10, rescale.axis=F, rug=FALSE, xlab="", ylab="Probability of Support", main="Take Away Guns", ylim=c(.2,1), layout=c(3,1)))

eff11 <- effect("PID*trustf", mod11, default.levels=1000)
trellis.device(color = FALSE)
effect11 <- print(plot(eff11, rescale.axis=F, rug=FALSE, xlab="Trust - Federal Government", ylab="Probability of Support", main="Improve Background Checks", ylim=c(.2,1), layout=c(3,1)))

eff12 <- effect("PID*trustf", mod12, default.levels=1000)
trellis.device(color = FALSE)
effect12 <- print(plot(eff12, rescale.axis=F, rug=FALSE, xlab="Trust - Federal Government", ylab="Probability of Support", main="Carry Guns Schools", ylim=c(0,.85), layout=c(3,1)))

TrustIntEffect <- ggpubr::ggarrange(effect8, effect10, effect11, effect7, effect9, effect12, ncol = 3, nrow = 2)

ControlEffect <- ggpubr::ggarrange(effect8, effect10, effect11, ncol = 1, nrow = 3)

ExpansionEffect <- ggpubr::ggarrange(effect7, effect9, effect12, ncol = 1, nrow = 3)



###################################################
##  Alternative Plots
###################################################
install.packages("sjPlot")
library(sjPlot)
install.packages("sjmisc")
library(sjmisc)

p1 <- plot_model(mod7, type = "pred", terms = c("trustf", "PID"), axis.labels = "", title="Prohibit Publish Names", colors="bw") +
xlab("Trust in Federal Government") +
ylab("Probability of Support") +
scale_y_continuous(limits = c(.1, .9), breaks = seq(.1, .9, .1)) +
theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

p2 <- plot_model(mod8, type = "pred", terms = c("trustf", "PID"), title="Ban Assault Rifles", colors="bw") +
xlab("Trust in Federal Government") +
ylab("Probability of Support") +
scale_y_continuous(limits = c(.1, 1), breaks = seq(.1, 1, .1)) +
theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

p3 <- plot_model(mod9, type = "pred", terms = c("trustf", "PID"), title="Conceal Carry Easier", colors="bw") +
xlab("Trust in Federal Government") +
ylab("Probability of Support") +
scale_y_continuous(limits = c(.1, .9), breaks = seq(.1, .9, .1)) +
theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.lip2
ne = element_line(colour = "black"))

p4 <- plot_model(mod10, type = "pred", terms = c("trustf", "PID"), title="Take Away Guns", colors="bw") +
xlab("Trust in Federal Government") +
ylab("Probability of Support") +
scale_y_continuous(limits = c(.1, 1), breaks = seq(.1, 1, .1)) +
theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

p5 <- plot_model(mod11, type = "pred", terms = c("trustf", "PID"), title="Improve Background Checks", colors="bw") +
xlab("Trust in Federal Government") +
ylab("Probability of Support") +
scale_y_continuous(limits = c(.1, 1), breaks = seq(.1, 1, .1)) +
theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

p6 <- plot_model(mod12, type = "pred", terms = c("trustf", "PID"), title="Carry Guns School", colors="bw") +
xlab("Trust in Federal Government") +
ylab("Probability of Support") +
scale_y_continuous(limits = c(.1, .9), breaks = seq(.1, .9, .1)) +
theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))



###################################################
##  Three-way interactive effect
###################################################
mod7g <- glm(publish ~ age + gender + race1 + educ + income + PID*trustf*Ideology, data=dat, family=binomial, weight=commonweight)
mod8g <- glm(banassault ~ age + gender + race1 + educ + income + PID*trustf*Ideology, data=dat, family=binomial, weight=commonweight)
mod9g <- glm(ccarry ~ age + gender + race1 + educ + income + PID*trustf*Ideology, data=dat, family=binomial, weight=commonweight)
mod10g <- glm(takeguns ~ age + gender + race1 + educ + income + PID*trustf*Ideology, data=dat, family=binomial, weight=commonweight)
mod11g <- glm(background ~ age + gender + race1 + educ + income + PID*trustf*Ideology, data=dat, family=binomial, weight=commonweight)
mod12g <- glm(gschools ~ age + gender + race1 + educ + income + PID*trustf*Ideology, data=dat, family=binomial, weight=commonweight)

stargazer(mod8g, mod10g, mod11g, mod7g, mod9g, mod12g)

p1g <- plot_model(mod7g, type = "pred", terms = c("trustf", "PID", "Ideology"), axis.labels = "", title="Prohibit Publish Names", colors="bw") +
xlab("Trust in Federal Government") +
ylab("Probability of Support") +
scale_y_continuous(limits = c(.1, .9), breaks = seq(.1, .9, .1)) +
theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

p2g <- plot_model(mod8g, type = "pred", terms = c("trustf", "PID", "Ideology"), title="Ban Assault Rifles", colors="bw") +
xlab("Trust in Federal Government") +
ylab("Probability of Support") +
scale_y_continuous(limits = c(.1, 1), breaks = seq(.1, 1, .1)) +
theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

p3g <- plot_model(mod9g, type = "pred", terms = c("trustf", "PID", "Ideology"), title="Conceal Carry Easier", colors="bw") +
xlab("Trust in Federal Government") +
ylab("Probability of Support") +
scale_y_continuous(limits = c(.1, .9), breaks = seq(.1, .9, .1)) +
theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

p4g <- plot_model(mod10g, type = "pred", terms = c("trustf", "PID", "Ideology"), title="Take Away Guns", colors="bw") +
xlab("Trust in Federal Government") +
ylab("Probability of Support") +
scale_y_continuous(limits = c(.1, 1), breaks = seq(.1, 1, .1)) +
theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

p5g <- plot_model(mod11g, type = "pred", terms = c("trustf", "PID", "Ideology"), title="Improve Background Checks", colors="bw") +
xlab("Trust in Federal Government") +
ylab("Probability of Support") +
scale_y_continuous(limits = c(.1, 1), breaks = seq(.1, 1, .1)) +
theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

p6g <- plot_model(mod12g, type = "pred", terms = c("trustf", "PID", "Ideology"), title="Carry Guns School", colors="bw") +
xlab("Trust in Federal Government") +
ylab("Probability of Support") +
scale_y_continuous(limits = c(.1, .9), breaks = seq(.1, .9, .1)) +
theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))





# Saving the workspace 
save.image("GunControlWorkspace.RData")