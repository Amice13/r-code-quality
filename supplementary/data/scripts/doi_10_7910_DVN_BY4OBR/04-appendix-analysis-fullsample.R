library(plm)
library(data.table)
library(texreg)
library(dplyr)
library(stargazer)
library(readr)

load("analysis-data-fullsample.Rdata")
d <- pdata.frame(data, index = c("country","year"))

outputfile.all <- "FE-fullsample-stateunknown.tex"
outputfile.extra <- "FE-fullsample-nonstate.tex"

#############################################
######### twoway fixed effects model: demo 
#############################################

reg1 <- plm(log(stateunknown+1) ~ bmr_democracy , data=d, model="within", effects="twoway")
summary(reg1)

reg2 <- plm(log(stateunknown+1) ~ bmr_democracy + v2jucorrdc+ v2xel_locelec + armedconf + l.pts +  l.lpop + l.lrgdp, data=d, model="within", effects="twoway")
summary(reg2)

reg3 <- plm(log(stateunknown+1) ~ v2x_polyarchy + v2x_polyarchy.sq, data=d, model="within", effects="twoway")
summary(reg3)

reg4 <- plm(log(stateunknown+1) ~+ v2jucorrdc + v2xel_locelec + v2x_polyarchy + v2x_polyarchy.sq + armedconf +  l.pts +  l.lpop + l.lrgdp, data=d, model="within", effects="twoway")
summary(reg4)

reg.table <- list(reg1, reg2, reg3, reg4)
screenreg(reg.table)
modelnames <- c("I", "II", "III", "IV")

coefnames <- c("BMR Democ (0/1)", "Judicial Corr", "Elected local gov", "Armed Conflict (0/1)",  "PTS (lag)",  "log Pop (lag)", "log rGDP (lag)", "Electoral dem. index", "Electoral dem. index squ")

screenreg(reg.table, custom.model.name=modelnames,
          custom.coef.names=coefnames, 
          reorder.coef = c(1,8,9,3,2,4,5,6,7))

texreg(reg.table,
       table = FALSE,
       custom.coef.names=coefnames,
       caption="Determinants of journalist killings by state authorities",
       custom.note="%stars. Twoways fixed effects, DV: log(journalist killings)",
       custom.model.name=modelnames, scriptsize=TRUE,
       reorder.coef = c(1,8,9,3,2,4,5,6,7),
       include.rsquared = F, include.adjrs = F,
       file=outputfile.all, stars = c(0.001, 0.01, 0.05, 0.1),
       symbol = "+", label="fe:all")


#############################################
######### twoway fixed effects model: 
######### with non-state political journalist killings
######### &  non-state non-political journalist killings
#############################################
pol1 <- plm(log(polgroup+1) ~ bmr_democracy + v2xel_locelec + v2jucorrdc + armedconf +  l.pts +  l.lpop + l.lrgdp, data=d, model="within", effects="twoway")
pol2 <- plm(log(polgroup+1) ~   v2xel_locelec + v2jucorrdc + v2x_polyarchy + v2x_polyarchy.sq + armedconf +  l.pts +  l.lpop + l.lrgdp, data=d, model="within", effects="twoway")

oth1 <- plm(log(other+1) ~ bmr_democracy + v2xel_locelec + v2jucorrdc + armedconf +  l.pts +  l.lpop + l.lrgdp, data=d, model="within", effects="twoway")
oth2 <- plm(log(other+1) ~ v2xel_locelec + v2jucorrdc + v2x_polyarchy + v2x_polyarchy.sq + armedconf +  l.pts +  l.lpop + l.lrgdp, data=d, model="within", effects="twoway")

coefnames <- c("BMR Democ (0/1)", "Elected local gov", "Judicial Corr", "Armed Conflict (0/1)",  "PTS (lag)", "log Pop (lag)", "log rGDP (lag)", "Electoral dem. index", "Electoral dem. index squ")
modelnames <- c("Non-state pol", "Non-state pol", "Non-pol", "Non-pol")

extra.table <- list(pol1, pol2, oth1, oth2)
screenreg(extra.table,
          custom.model.names = modelnames,
          custom.coef.names=coefnames, 
          reorder.coef = c(1,8,9,2,3,4,5,6,7))

texreg(extra.table,
       table = FALSE,
       custom.coef.names=coefnames,
       caption="Determinants of journalist killings by non-state political actors",
       custom.note="%stars. Twoways fixed effects, DV: log(journalist killings)",
       custom.model.name=modelnames, scriptsize=TRUE,
       reorder.coef = c(1,8,9,2,3,4,5,6,7),
       include.rsquared = F, include.adjrs = F,
       file=outputfile.extra, stars = c(0.001, 0.01, 0.05, 0.1),
       symbol = "+", label="fe:all:extra")




#############################################
######### summary stats 
#############################################
#Create summary statistics using stargazer package
df_stat <- data.frame(data[, c("state.d", "unknown.d","bmr_democracy","v2xel_locelec", 
                               "v2jucorrdc", "v2x_pubcorr", "l.lpop", "l.lrgdp", 
                               "armedconf", "l.pts",
                               "v2x_polyarchy")])

names(df_stat) <- c("state journ. killings","unconfirmed journ. killings", "BMR Democ (0/1)", "Elected local gov",
                                 "Judicial corruption", "Public sector corruption", "log Pop (lag)", "log rGDP (lag)",
                                 "Armed Conflict (0/1)", "PTS (lag)",   "Electoral dem. index")

columns <- colnames(df_stat)

print_stargazer <- function(object) {
  cat(paste(object, collapse = "\n"), "\n")
}

stargazer(df_stat[,columns], type = "latex",
  summary.stat = c("min", "p25", "mean", "p75", "max", "median", "sd"), 
  style="apsr", float = FALSE,
  digits = 2, no.space=TRUE, out="summarystats.tex")

