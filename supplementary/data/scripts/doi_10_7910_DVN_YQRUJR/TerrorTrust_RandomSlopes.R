######################################################
########### HOW FEAR DRIVES US APART - TPV ###########
######################################################

rm(list = ls()) 
setwd(".../Replication")

library("reshape") 
library("ggplot2") 
library("haven")
library("foreign") 
library("readxl")
library("readr")
library("dplyr")
library("corrplot")
library("psych")
library("car")
library("lme4")
library("arm")
library("zoo")
library("rio")
library("lmtest")
library("tidyr")
library("gmodels")
library("base")
library("texreg")
library("gmodels")
library("MASS")
library("Matrix")
library("interplot")
require(gridExtra)

### Data ###
final <- readRDS(file="TerrorTrust_Data.Rda")


### Multilevel Modelling ###
# Model 0: Trust (ICC)
mlm.0.trust <- glmer(trust ~ 1  +
                 (1 | ccode), 
               data = final,
               family = binomial(link = "logit"))
summary(mlm.0.trust)
1.052 / (1.052 + 3.29)

# Model 0: Fear (ICC)
mlm.0.fear <- glmer(tfear ~ 1  +
                 (1 | ccode), 
               data = final,
               family = binomial(link = "logit"))
summary(mlm.0.fear)
1.344 / (1.344 + 3.29)


# Model 1: Random Intercepts (full model)
mlm.1 <- glmer(trust ~ 1  + tfear +
                 sex + age + educat + unemp + polint +
                 tvnews + nnews +
                 gti + 
                 ln_gdp + gini + polity +
                 (1 | ccode), 
               data = final,
               family = binomial(link = "logit"))
summary(mlm.1)
0.5081 /(0.5081 +3.29)


# Model 2: Random Slopes (empty model)
mlm.2 <- glmer(trust ~ 1  + tfear +
                 sex + age + educat + unemp + polint +
                 tvnews + nnews +
                 gti + 
                 ln_gdp + gini + polity +
                 (1 + tfear | ccode), 
               data = final,
               family = binomial(link = "logit"))
summary(mlm.2)
0.1413 / (0.1413 + 3.29)
anova(mlm.1, mlm.2)
ranef(mlm.2)


# Model 3: Random Slopes (Polity moderation model)
mlm.3 <- glmer(trust ~ 1  + tfear +
                 sex + age + educat + unemp + polint +
                 tvnews + nnews +
                 gti + 
                 ln_gdp + gini + polity + 
                 tfear:polity +
                 (1 + tfear | ccode), 
               data = final,
               family = binomial(link = "logit"))
summary(mlm.3)
0.1236 / (0.1236+3.29)
anova(mlm.2, mlm.3)


# Model 4: Random Slopes (GTI moderation model)
mlm.4 <- glmer(trust ~ 1  + tfear +
                 sex + age + educat + unemp + polint +
                 tvnews + nnews +
                 gti + 
                 ln_gdp + gini + polity +
                 tfear:gti +
                 (1 + tfear | ccode), 
               data = final,
               family = binomial(link = "logit"))
summary(mlm.4)
anova(mlm.2, mlm.4)
anova(mlm.3, mlm.4)


# Plotting the cross-level interactions
plot1 <- interplot(mlm.3, var1 = "tfear", var2 = "polity", hist = TRUE) +
  xlab("Democratization") +
  ylab("Estimated Effect of Fearing Terrorism on Trust") +
  theme_bw() +
  geom_hline(yintercept = 0)
plot1 <- plot1 + theme(text = element_text(size=20),
                       panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                       panel.border = element_rect(fill=NA, colour = "black", size=1),
                       panel.background = element_blank(), axis.line = element_line(colour = "black"))


plot2 <- interplot(mlm.4, var1 = "tfear", var2 = "gti", hist = TRUE, ralpha = 0.5) +
  xlab("Global Terrorism Index") +
  ylab("Estimated Effect of Fearing Terrorism on Trust") +
  theme_bw() +
  geom_hline(yintercept = 0)
plot2 <- plot2 + theme(text = element_text(size=20),
                       panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                       panel.border = element_rect(fill=NA, colour = "black", size=1),
                       panel.background = element_blank(), axis.line = element_line(colour = "black"))


grid.arrange(plot1, plot2, ncol=2)

jpeg("figure4.jpeg", width = 11.5, height = 8, units = 'in', res = 300)
grid.arrange(plot1, plot2, ncol=2)
dev.off()


# Table
htmlreg(list(mlm.1, mlm.3, mlm.4),
        file = "Table 2.doc",
        single.row=T,
        booktabs = T,
        dcolumn =T,
        no.margin = FALSE,
        caption = "Table 2. Conditional Effect of Fearing Terrorism on Trust.",
        caption.above = T)



##############################
###### Robustness Check ######
##############################

## 1) Group-Mean Centering
## preparation: group-mean centering all L1-variables (c_variablename)
##              1SD division as well (cs_variablename)
##              also dichotomouous (comparison)
c <- function (x) {
  (x - mean(x, na.rm = TRUE)) }
cs <- function (x) {
  (x - mean(x, na.rm = TRUE))/(sd(x, na.rm = TRUE)) }

# Centered variables
final_robust <- final %>%
  group_by(ccode) %>% ## L1 variables are group-mean centered
  mutate(c_tfear       = c(tfear),
         c_sex         = c(sex),
         c_age         = c(age),
         c_edu         = c(educat),
         c_unemp       = c(unemp),
         c_polint      = c(polint),
         c_tvnews      = c(tvnews),
         c_nnews       = c(nnews))

final_robust$c_gti <- c(final_robust$gti)
final_robust$c_gdp <- c(final_robust$ln_gdp)
final_robust$c_gini <- c(final_robust$gini)
final_robust$c_polity <- c(final_robust$polity)

# Centered and standardized variables 
final_robust <- final_robust %>%
  group_by(ccode) %>%
  mutate(cs_tfear       = cs(tfear),
         cs_sex         = cs(sex),
         cs_age         = cs(age),
         cs_edu         = cs(educat),
         cs_unemp       = cs(unemp),
         cs_polint      = cs(polint),
         cs_tvnews      = cs(tvnews),
         cs_nnews       = cs(nnews))

final_robust$cs_gti <- cs(final_robust$gti)
final_robust$cs_gdp <- cs(final_robust$ln_gdp)
final_robust$cs_gini <- cs(final_robust$gini)
final_robust$cs_polity <- cs(final_robust$polity)


# Centered, unstandardized model 
mlm.3.robust <- glmer(trust ~ 1  + c_tfear +
                        c_sex + c_age + c_edu + c_unemp + c_polint +
                        c_tvnews + c_nnews +
                        c_gti + 
                        c_gdp + c_gini + c_polity + 
                        c_tfear:c_polity +
                        (1 + c_tfear | ccode), 
                      data = final_robust,
                      family = binomial(link = "logit"))
summary(mlm.3.robust)

# Centered, standardized model 
mlm.3.robustst <- glmer(trust ~ 1  + cs_tfear +
                   cs_sex + cs_age + cs_edu + cs_unemp + cs_polint +
                   cs_tvnews + cs_nnews +
                   cs_gti + 
                   cs_gdp + cs_gini + cs_polity + 
                   cs_tfear:cs_polity +                 
                   (1 + cs_tfear | ccode), 
                 data = final_robust,
                 family = binomial(link = "logit"))
summary(mlm.3.robustst)


## 2) Three-way Interaction
mlm.3.test3 <- glmer(trust ~ 1  + tfear +
                       sex + age + educat + unemp + polint +
                       tvnews + nnews +
                       gti + 
                       ln_gdp + gini + polity + 
                       tvnews:tfear:polity +
                       (1 + tfear | ccode), 
                     data = final,
                     family = binomial(link = "logit"))
summary(mlm.3.test3)
