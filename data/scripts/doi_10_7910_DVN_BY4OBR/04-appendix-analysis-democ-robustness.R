library(data.table)
library(texreg)
library(plm)
library(ggplot2)
library(MASS)
library(sandwich)
library(lmtest)

#############################################
############### Functions ###############
#############################################

theme.arg <-  theme(panel.background=element_blank(),
                    panel.border=element_rect(colour = "black", fill=NA),
                    axis.text=element_text(color="black"))


make.cl.se <- function(model) {
  ct <-coeftest(model, function(x) vcovHC(x,type="HC",cluster="gwno"))
  se <- ct[, 2]
  pval <- ct[, 4]
  return(list(se, pval))
}

logitlink <- function(x) {1/(1+exp(-x))}

load("analysis-data-bmr.Rdata")
data <- data.bmr


#############################################
#############################################
#############################################
############### Additional local measures 
#############################################
#############################################
#############################################



#############################################
################# Regional government index
#############################################
## state and disaggregated
sr <- glm(state.d ~ v2xel_regelec + v2jucorrdc  +
            l.lpop + l.lrgdp + armedconf + l.pts + 
            v2x_polyarchy + v2x_polyarchy.sq + factor(year),
          family=binomial(link="logit"), data=data)


## unknown and disaggregated

ur <- glm(unknown.d ~ v2xel_regelec + v2jucorrdc  +
            l.lpop + l.lrgdp + armedconf + l.pts + 
            v2x_polyarchy + v2x_polyarchy.sq + factor(year),
          family=binomial(link="logit"), data=data)




#############################################
################# Subnational election unevenness
#############################################
## state and disaggregated
se <- glm(state.d ~ v2elsnlsff + v2jucorrdc  +
            l.lpop + l.lrgdp + armedconf + l.pts + 
            v2x_polyarchy + v2x_polyarchy.sq + factor(year),
          family=binomial(link="logit"), data=data)

## unknown and disaggregated

ue <- glm(unknown.d ~ v2elsnlsff + v2jucorrdc  +
            l.lpop + l.lrgdp + armedconf + l.pts + 
            v2x_polyarchy + v2x_polyarchy.sq + factor(year),
          family=binomial(link="logit"), data=data)



#############################################
################# Local offices relative power 
#############################################
## state and disaggregated
so <- glm(state.d ~ v2ellocpwr + v2jucorrdc  +
            l.lpop + l.lrgdp + armedconf + l.pts + 
            v2x_polyarchy + v2x_polyarchy.sq + factor(year),
          family=binomial(link="logit"), data=data)

## unknown and disaggregated

uo <- glm(unknown.d ~ v2ellocpwr + v2jucorrdc  +
            l.lpop + l.lrgdp + armedconf + l.pts + 
            v2x_polyarchy + v2x_polyarchy.sq + factor(year),
          family=binomial(link="logit"), data=data)



model.list <- list(sr,se,so,ur,ue,uo)
screenreg(model.list)


## save clustered SEs
model.ses <- list(make.cl.se(sr)[[1]], 
                  make.cl.se(se)[[1]], make.cl.se(so)[[1]],
                  make.cl.se(ur)[[1]],
                  make.cl.se(ue)[[1]], make.cl.se(uo)[[1]])

## save clustered pvalues
model.pvals <- list(make.cl.se(sr)[[2]],
                    make.cl.se(se)[[2]], make.cl.se(so)[[2]],
                    make.cl.se(ur)[[2]], 
                    make.cl.se(ue)[[2]], make.cl.se(uo)[[2]])


modelnames <- c("I-State I", "II-State", "III-State",
                "IV-Unconfirmed", "V-Unconfirmed", "VI-Unconfirmed")
coefnames <- c("Intercept", "Regional gov index", "Judicial Corr",
               "log Pop (lag)", "log rGDP (lag)","Armed conflict", "PTS (lag)", 
               "Electoral dem. index",
               "Electoral dem. index (squ)", "Subnat. election unevenness", "Local offices rel. power")


screenreg(model.list,
          custom.model.name=modelnames,
          custom.coef.names=coefnames,
          override.se = model.ses,
          override.pvalues = model.pvals,
          omit.coef = "year",
          reorder.coef = c(1,2,10,11,3,4,5,6,7,8,9))

texreg(model.list,
       table = FALSE,
       custom.coef.names=coefnames,
       caption="Determinants of journalist killings",
       custom.note="%stars. Logistic regression. Country-clustered standard errors. Year fixed effects not shown",
       custom.model.name=modelnames, scriptsize=TRUE,
       override.se = model.ses,
       override.pvalues = model.pvals,
       omit.coef = "year",
       file="logit-democracy-subnational-robust.tex", 
       stars = c(0.001, 0.01, 0.05, 0.1),
       reorder.coef = c(1,2,10,11,3,4,5,6,7,8,9),
       symbol = "+", label="logit:robust")


#############################################
#############################################
################# Rule of law measures
#############################################
#############################################

### Freedom House Rule of law
## state and disaggregated
srol1 <- glm(state.d ~ v2xel_locelec + e_fh_rol  +
            l.lpop +  armedconf + l.pts + 
            v2x_polyarchy + v2x_polyarchy.sq + factor(year),
          family=binomial(link="logit"), data=data)


## unknown and disaggregated

urol1 <- glm(unknown.d ~ v2xel_locelec + e_fh_rol  +
            l.lpop +  armedconf + l.pts + 
            v2x_polyarchy + v2x_polyarchy.sq + factor(year),
          family=binomial(link="logit"), data=data)


### World Bank Rule of law est
## state and disaggregated
srol2 <- glm(state.d ~ v2xel_locelec + rulelaw.wb  +
            l.lpop +  armedconf + l.pts + 
            v2x_polyarchy + v2x_polyarchy.sq + factor(year),
          family=binomial(link="logit"), data=data)


## unknown and disaggregated

urol2 <- glm(unknown.d ~ v2xel_locelec + rulelaw.wb  +
            l.lpop + armedconf + l.pts + 
            v2x_polyarchy + v2x_polyarchy.sq + factor(year),
          family=binomial(link="logit"), data=data)

model.list <- list(srol1,urol1, srol2, urol2)
screenreg(model.list)


## save clustered SEs
model.ses <- list(make.cl.se(srol1)[[1]], 
                  make.cl.se(urol1)[[1]], 
                  make.cl.se(srol2)[[1]],
                  make.cl.se(urol2)[[1]])

## save clustered pvalues
model.pvals <- list(make.cl.se(srol1)[[2]], 
                    make.cl.se(urol1)[[2]], 
                    make.cl.se(srol2)[[2]],
                    make.cl.se(urol2)[[2]])


modelnames <- c("I-State I", "II-Unconfirmed", "III-State",
                "IV-Unconfirmed")
coefnames <- c("Intercept", "Local gov index", "Rule of Law (Freedom House)",
               "log Pop (lag)","Armed conflict", "PTS (lag)", 
               "Electoral dem. index",
               "Electoral dem. index (squ)", "Rule of Law (World Bank)")


screenreg(model.list,
          custom.model.name=modelnames,
          custom.coef.names=coefnames,
          override.se = model.ses,
          override.pvalues = model.pvals,
          omit.coef = "year",
          reorder.coef = c(1,2,3,9,4,5,6,7,8))

texreg(model.list,
       table = FALSE,
       custom.coef.names=coefnames,
       caption="Determinants of journalist killings",
       custom.note="%stars. Logistic regression. Country-clustered standard errors. Year fixed effects not shown",
       custom.model.name=modelnames, scriptsize=TRUE,
       override.se = model.ses,
       override.pvalues = model.pvals,
       omit.coef = "year",
       file="logit-democracy-subnational-ruleoflaw.tex", 
       stars = c(0.001, 0.01, 0.05, 0.1),
       reorder.coef = c(1,2,3,9,4,5,6,7,8),
       symbol = "+", label="logit:rol")




#############################################
#############################################
################# Count model
#############################################
#############################################


counts1 <- lm(log(state+1) ~ v2xel_locelec + v2jucorrdc  +
            l.lpop + l.lrgdp + armedconf + l.pts + 
            v2x_polyarchy + v2x_polyarchy.sq + factor(year),
            data=data)

counts2 <-  lm(log(state+1) ~ v2xel_regelec + v2jucorrdc  +
                 l.lpop + l.lrgdp + armedconf + l.pts + 
                 v2x_polyarchy + v2x_polyarchy.sq + factor(year),
               data=data)

counts3 <-  lm(log(state+1) ~ v2elsnlsff + v2jucorrdc  +
                 l.lpop + l.lrgdp + armedconf + l.pts + 
                 v2x_polyarchy + v2x_polyarchy.sq + factor(year),
               data=data)

counts4 <-  lm(log(state+1) ~ v2ellocpwr + v2jucorrdc  +
                 l.lpop + l.lrgdp + armedconf + l.pts + 
                 v2x_polyarchy + v2x_polyarchy.sq + factor(year),
               data=data)
## unknown and disaggregated

countu1 <- lm(log(unknown.d+1) ~ v2xel_locelec + v2jucorrdc  +
            l.lpop + l.lrgdp + armedconf + l.pts + 
            v2x_polyarchy + v2x_polyarchy.sq + factor(year),
            data=data)

countu2 <- lm(log(unknown.d+1) ~ v2xel_regelec + v2jucorrdc  +
                l.lpop + l.lrgdp + armedconf + l.pts + 
                v2x_polyarchy + v2x_polyarchy.sq + factor(year),
              data=data)

countu3 <- lm(log(unknown.d+1) ~ v2elsnlsff + v2jucorrdc  +
               l.lpop + l.lrgdp + armedconf + l.pts + 
               v2x_polyarchy + v2x_polyarchy.sq + factor(year),
             data=data)

countu4 <- lm(log(unknown.d+1) ~ v2ellocpwr + v2jucorrdc  +
               l.lpop + l.lrgdp + armedconf + l.pts + 
               v2x_polyarchy + v2x_polyarchy.sq + factor(year),
             data=data)

modelnames <- c("I-State I", "II-State", "III-State", "IV-State",
                "V-Unconfirmed", "VI-Unconfirmed", "VII-Unconfirmed", "VII-Unconfirmed")

coefnames <- c("Intercept","Elected local gov", "Judicial Corr",
               "log Pop (lag)", "log rGDP (lag)","Armed conflict", "PTS (lag)", 
               "Electoral dem. index",
               "Electoral dem. index (squ)",  "Regional gov index", "Subnat. election unevenness", "Local offices rel. power")
## save clustered SEs
model.ses <- list(make.cl.se(counts1)[[1]], make.cl.se(counts2)[[1]], 
                  make.cl.se(counts3)[[1]],  make.cl.se(counts4)[[1]], 
                  make.cl.se(countu1)[[1]],
                  make.cl.se(countu2)[[1]], make.cl.se(countu3)[[1]],
                  make.cl.se(countu4)[[1]])

## save clustered pvalues
model.pvals <- list(make.cl.se(counts1)[[2]], make.cl.se(counts2)[[2]], 
                    make.cl.se(counts3)[[2]], make.cl.se(counts4)[[2]],
                    make.cl.se(countu1)[[2]],
                    make.cl.se(countu2)[[2]], make.cl.se(countu3)[[2]],
                    make.cl.se(countu4)[[2]])

model.list.d <- list(counts1,counts2,counts3,counts4,countu1,countu2,countu3,countu4)


screenreg(model.list.d, 
          custom.model.name=modelnames,
          custom.coef.names=coefnames,
          override.se = model.ses,
          override.pvalues = model.pvals,
          omit.coef = "year",
          reorder.coef = c(1,2,10,11,12,3,4,5,6,7,8,9))

texreg(model.list.d,
       table = FALSE,
       custom.coef.names=coefnames,
       caption="Determinants of journalist killings",
       custom.note="%stars. Linear regression. DV: Log number of journalist killings. Country-clustered standard errors. Year fixed effects not shown",
       custom.model.name=modelnames, scriptsize=TRUE,
       override.se = model.ses,
       override.pvalues = model.pvals,
       omit.coef = "year",
       reorder.coef = c(1,2,10,11,12,3,4,5,6,7,8,9),
       file="ols-democracy-bmr-counts.tex", stars = c(0.001, 0.01, 0.05, 0.1),
       symbol = "+", label="ols:count")




#############################################
#############################################
################# Non-state perpetrators
#############################################
#############################################

pol1 <- glm(polgroup.d ~ v2xel_locelec + v2jucorrdc  +
                l.lpop + l.lrgdp + armedconf + l.pts + 
                v2x_polyarchy + v2x_polyarchy.sq + factor(year),
              data=data, family=binomial(link="logit"))

pol2 <- glm(polgroup.d ~ v2xel_locelec + v2x_pubcorr  +
                l.lpop + l.lrgdp + armedconf + l.pts + 
                v2x_polyarchy + v2x_polyarchy.sq + factor(year),
              data=data, family=binomial(link="logit"))

oth1 <-  glm(other.d ~ v2xel_locelec + v2jucorrdc  +
                l.lpop + l.lrgdp + armedconf + l.pts + 
                v2x_polyarchy + v2x_polyarchy.sq + factor(year),
              data=data, family=binomial(link="logit"))

oth2 <-  glm(other.d ~ v2xel_locelec + v2x_pubcorr  +
                 l.lpop + l.lrgdp + armedconf + l.pts + 
                 v2x_polyarchy + v2x_polyarchy.sq + factor(year),
               data=data, family=binomial(link="logit"))

models.nonstate <- list(pol1, pol2, oth1, oth2 )
model.ses <- list(make.cl.se(pol1)[[1]], make.cl.se(pol2)[[1]], 
                  make.cl.se(oth1)[[1]],  make.cl.se(oth2)[[1]])
model.pvals <- list(make.cl.se(pol1)[[2]], make.cl.se(pol2)[[2]], 
                  make.cl.se(oth1)[[2]],  make.cl.se(oth2)[[2]])


modelnames <- c("Non-state pol.", "Non-state pol.", "Non-pol.", "Non-pol.")

coefnames <- c("Intercept","Elected local gov", "Judicial Corr",
               "log Pop (lag)", "log rGDP (lag)","Armed conflict", "PTS (lag)", 
               "Electoral dem. index",
               "Electoral dem. index (squ)",  "Public Sector Corr")

screenreg(models.nonstate, 
          custom.model.name=modelnames,
          custom.coef.names=coefnames,
          override.se = model.ses,
          override.pvalues = model.pvals,
          omit.coef = "year",
          reorder.coef = c(1,2,3,10,4,5,6,7,8,9))


texreg(models.nonstate,
       table = FALSE,
       custom.coef.names=coefnames,
       caption="Determinants of journalist killings by non-state perpetrators",
       custom.note="%stars. Logistic regression. Country-clustered standard errors. Year fixed effects not shown",
       custom.model.name=modelnames, scriptsize=TRUE,
       override.se = model.ses,
       override.pvalues = model.pvals,
       omit.coef = "year",
       reorder.coef =  c(1,2,3,10,4,5,6,7,8,9),
       file="logit-democracy-subnational-nonstate.tex", stars = c(0.001, 0.01, 0.05, 0.1),
       symbol = "+", label="logit:nonstate")


