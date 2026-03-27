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

#############################################
################# REGIMES OF THE WORLD - Judicial corruption
#############################################
load("analysis-data-fullsample.Rdata")
data <- data[RWI %in% c("electoral democracy", "liberal democracy"), ]

## state and disaggregated
s1 <- glm(state.d ~ v2xel_locelec + v2jucorrdc  +
            l.lpop + l.lrgdp + armedconf + l.pts + 
            v2x_polyarchy + v2x_polyarchy.sq + factor(year),
          family=binomial(link="logit"), data=data)

s2 <- glm(state.nationalreach.d ~ v2xel_locelec + v2jucorrdc  +
            l.lpop + l.lrgdp + armedconf + l.pts + 
            v2x_polyarchy + v2x_polyarchy.sq + factor(year),
          family=binomial(link="logit"), data=data)

s3 <- glm(state.localreach.d ~ v2xel_locelec + v2jucorrdc  +
            l.lpop + l.lrgdp + armedconf + l.pts + 
            v2x_polyarchy + v2x_polyarchy.sq + factor(year),
          family=binomial(link="logit"), data=data)


## unknown and disaggregated

u1 <- glm(unknown.d ~ v2xel_locelec + v2jucorrdc  +
            l.lpop + l.lrgdp + armedconf + l.pts + 
            v2x_polyarchy + v2x_polyarchy.sq + factor(year),
          family=binomial(link="logit"), data=data)

u2 <- glm(unknown.nationalreach.d ~ v2xel_locelec + v2jucorrdc  +
            l.lpop + l.lrgdp + armedconf + l.pts + 
            v2x_polyarchy + v2x_polyarchy.sq + factor(year),
          family=binomial(link="logit"), data=data)

u3 <- glm(data$unknown.localreach.d ~ v2xel_locelec + v2jucorrdc  +
            l.lpop + l.lrgdp + armedconf + l.pts + 
            v2x_polyarchy + v2x_polyarchy.sq + factor(year),
          family=binomial(link="logit"), data=data)

modelnames <- c("I-State", "II-State-national", "III-State-local", 
                "IV-Unconfirmed", "V-Unconf-national", "VI-Unconf-local")

coefnames <- c("Intercept", "Elected local gov", "Judicial Corr",
               "log Pop (lag)", "log rGDP (lag)","Armed conflict", "PTS (lag)", 
               "Electoral dem. index",
               "Electoral dem. index (squ)")

## save clustered SEs
model.ses <- list(make.cl.se(s1)[[1]], make.cl.se(s2)[[1]], 
                  make.cl.se(s3)[[1]], make.cl.se(u1)[[1]],
                  make.cl.se(u2)[[1]], make.cl.se(u3)[[1]])

## save clustered pvalues
model.pvals <- list(make.cl.se(s1)[[2]], make.cl.se(s2)[[2]], 
                    make.cl.se(s3)[[2]], make.cl.se(u1)[[2]],
                    make.cl.se(u2)[[2]], make.cl.se(u3)[[2]])

model.list.d <- list(s1,s2,s3,u1,u2,u3)


screenreg(model.list.d, custom.model.name=modelnames,
          custom.coef.names=coefnames,
          override.se = model.ses,
          override.pvalues = model.pvals,
          omit.coef = "year")

texreg(model.list.d,
       table = FALSE,
       custom.coef.names=coefnames,
       caption="Determinants of journalist killings",
       custom.note="Logistic regression. Country-clustered standard errors. Year fixed effects not shown",
       custom.model.name=modelnames, scriptsize=TRUE,
       override.se = model.ses,
       override.pvalues = model.pvals,
       omit.coef = "year",
       file="logit-democracy-locelec-disag-jucorr-Regimes-of-World.tex", stars = c(0.001, 0.01, 0.05, 0.1),
       symbol = "+", label="logit:row")




#############################################
################# POLITY 2 - Judicial corruption
#############################################
load("analysis-data-fullsample.Rdata")

# first, we remove all observations that POLITY classifies as Cases of foreign 'interruption' (-66),  'interregnum' or anarchy (-77), or 'transition' (-88)
subset.polity <- c(-66, -77, -88)
data <- data[! (data$polity %in% subset.polity), ]
data <- data[polity2 >6 , ]

## state and disaggregated
s1 <- glm(state.d ~ v2xel_locelec + v2jucorrdc  +
            l.lpop + l.lrgdp + armedconf + l.pts + 
            v2x_polyarchy + v2x_polyarchy.sq + factor(year),
          family=binomial(link="logit"), data=data)

s2 <- glm(state.nationalreach.d ~ v2xel_locelec + v2jucorrdc  +
            l.lpop + l.lrgdp + armedconf + l.pts + 
            v2x_polyarchy + v2x_polyarchy.sq + factor(year),
          family=binomial(link="logit"), data=data)

s3 <- glm(state.localreach.d ~ v2xel_locelec + v2jucorrdc  +
            l.lpop + l.lrgdp + armedconf + l.pts + 
            v2x_polyarchy + v2x_polyarchy.sq + factor(year),
          family=binomial(link="logit"), data=data)


## unknown and disaggregated

u1 <- glm(unknown.d ~ v2xel_locelec + v2jucorrdc  +
            l.lpop + l.lrgdp + armedconf + l.pts + 
            v2x_polyarchy + v2x_polyarchy.sq + factor(year),
          family=binomial(link="logit"), data=data)

u2 <- glm(unknown.nationalreach.d ~ v2xel_locelec + v2jucorrdc  +
            l.lpop + l.lrgdp + armedconf + l.pts + 
            v2x_polyarchy + v2x_polyarchy.sq + factor(year),
          family=binomial(link="logit"), data=data)

u3 <- glm(data$unknown.localreach.d ~ v2xel_locelec + v2jucorrdc  +
            l.lpop + l.lrgdp + armedconf + l.pts + 
            v2x_polyarchy + v2x_polyarchy.sq + factor(year),
          family=binomial(link="logit"), data=data)

modelnames <- c("I-State", "II-State-national", "III-State-local", 
                "IV-Unconfirmed", "V-Unconf-national", "VI-Unconf-local")

coefnames <- c("Intercept", "Elected local gov", "Judicial Corr",
               "log Pop (lag)", "log rGDP (lag)","Armed conflict", "PTS (lag)", 
                "Electoral dem. index",
               "Electoral dem. index (squ)")

## save clustered SEs
model.ses <- list(make.cl.se(s1)[[1]], make.cl.se(s2)[[1]], 
                  make.cl.se(s3)[[1]], make.cl.se(u1)[[1]],
                  make.cl.se(u2)[[1]], make.cl.se(u3)[[1]])

## save clustered pvalues
model.pvals <- list(make.cl.se(s1)[[2]], make.cl.se(s2)[[2]], 
                    make.cl.se(s3)[[2]], make.cl.se(u1)[[2]],
                    make.cl.se(u2)[[2]], make.cl.se(u3)[[2]])

model.list.d <- list(s1,s2,s3,u1,u2,u3)


screenreg(model.list.d, custom.model.name=modelnames,
          custom.coef.names=coefnames,
          override.se = model.ses,
          override.pvalues = model.pvals,
          omit.coef = "year")

texreg(model.list.d,
       table = FALSE,
       custom.coef.names=coefnames,
       caption="Determinants of journalist killings",
       custom.note="Logistic regression. Country-clustered standard errors. Year fixed effects not shown",
       custom.model.name=modelnames, scriptsize=TRUE,
       override.se = model.ses,
       override.pvalues = model.pvals,
       omit.coef = "year",
       file="logit-democracy-locelec-disag-jucorr-Polity2-7-10.tex", stars = c(0.001, 0.01, 0.05, 0.1),
       symbol = "+", label="logit:polity")


