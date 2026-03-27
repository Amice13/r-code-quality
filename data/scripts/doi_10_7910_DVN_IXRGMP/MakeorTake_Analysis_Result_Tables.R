rm(list=ls())

list.of.packages <- c("plm",
                      "survival",
                      "gdata",
                      "stargazer",
                      "pcse",
                      "readstata13",
                      "tidyverse",
                      "miceadds",
                      "panelAR",
                      "texreg")


lapply(list.of.packages, require, character.only = TRUE)

# set working directory
#dir <- "~/Dropbox/Geopolitical_Competition/data"
setwd("/Users/thereseanders/Dropbox/Geopolitical_Competition/data")

#### Setting parameters for the texreg package

# panelAR
extract.panelAR <- function(model) {
  s <- summary(model)
  names <- rownames(s$coefficients)
  co <- s$coefficients[, 1]
  se <- s$coefficients[, 2]
  pval <- s$coefficients[, 4]
  rs <- s$r2
  n <- round(length(s$residuals),0)
  gof <- c(n)
  gof.names <- c("Num. obs.")
  tr <- createTexreg(
    coef.names = names,
    coef = co,
    se = se,
    pvalues = pval,
    gof.names = gof.names,
    gof = gof
  )
  return(tr)
}
setMethod("extract", 
          signature = className("panelAR", "panelAR"),
          definition = extract.panelAR)



##read in the data
dat <- read.dta13("Make_or_Take_3Oct2017.dta")

####creating variables....these are already created
#dat$net_territorial_change_max <-  dat$area_gained_max_sum -  dat$area_lost_max_sum
#dat$territory_gained <- ifelse(dat$area_gained_max > 1,1,0)
#dat$territory_gained[is.na(dat$territory_gained)] <- 0
#dat$tpop_100k <- dat$tpop*100
#dat$milper_100k <- dat$milper/dat$tpop_100k
#dat$milper_pc <- dat$milper/dat$tpop
#dat$milper_pcLN = log(dat$milper_pc)

#filtering out NA in terms of ccode
dat <- dat[!is.na(dat$ccode),] 

# setting panel data frame
pdat <- pdata.frame(dat, index = c("ccode", "year"), drop.index = FALSE, row.names = TRUE)

#pdat$totton_pc = pdat$totton/pdat$tpop
#pdat$pec_pc = pdat$pec/pdat$tpop
#pdat$gdp_pc = pdat$gdp/pdat$tpop
#pdat$milperLN = log(pdat$milper+1)
#pdat$gdpLN = log(pdat$gdp+1)
#pdat$gdp_pc_ln = log(pdat$gdp_pc+1)
#pdat$pec_pc_ln = log(pdat$pec_pc+1)
#pdat$totton_pc_ln = log(pdat$totton_pc+1)


#####TABLE 2--TERRITORIAL MIDS##########

# model1
f1 <- as.formula(territory_binary ~ pec_pc_ln + 
                   gdp_pc_ln  + 
                   threat +
                   territory_binary_years + 
                   territory_binary_years_squared + 
                   territory_binary_years_cubed)
model1 <- glm(f1,  
              data = dat, 
              family = binomial(logit))
e1 <- extract(model1,
                include.aic = F,
                include.bic = F,
                include.loglik = F,
                include.deviance = F)

model1_c <- miceadds::glm.cluster(data=dat, 
                                  formula = f1,
                                  cluster="ccode",
                                  family="binomial")
summary(model1_c)
se1 <- unname(as.matrix(summary(model1_c))[,2])
pv1 <- unname(as.matrix(summary(model1_c))[,4])

#model2
f2 <- as.formula(territory_binary ~ pec_pc_ln + gdp_pc_ln + threat + polity2 +  territory_binary_years + territory_binary_years_squared + territory_binary_years_cubed)

model2 <- glm(f2,  
              data = dat, 
              family = binomial(logit))
e2 <- extract(model2,
              include.aic = F,
              include.bic = F,
              include.loglik = F,
              include.deviance = F)

model2_c <- miceadds::glm.cluster(data=dat, 
                                  formula = f2,
                                  cluster="ccode",
                                  family="binomial")
summary(model2_c)
se2 <- unname(as.matrix(summary(model2_c))[,2])
pv2 <- unname(as.matrix(summary(model2_c))[,4])

#model3
f3 <- as.formula(territory_binary ~ pec_pc_ln + gdp_pc_ln + threat + polity2 + land + sea + territory_binary_years + territory_binary_years_squared + territory_binary_years_cubed)

model3 <- glm(f3,  
              data = dat, 
              family = binomial(logit))
e3 <- extract(model3,
              include.aic = F,
              include.bic = F,
              include.loglik = F,
              include.deviance = F)

model3_c <- miceadds::glm.cluster(data=dat, 
                                  formula = f3,
                                  cluster="ccode",
                                  family="binomial")
summary(model3_c)
se3 <- unname(as.matrix(summary(model3_c))[,2])
pv3 <- unname(as.matrix(summary(model3_c))[,4])

#model4
f4 <- as.formula(territory_binary ~ pec_pc_ln + threat + polity2 + land + sea + territory_binary_years + territory_binary_years_squared + territory_binary_years_cubed)

model4 <- glm(f4,  
              data = dat, 
              family = binomial(logit))
e4 <- extract(model4,
              include.aic = F,
              include.bic = F,
              include.loglik = F,
              include.deviance = F)

model4_c <- miceadds::glm.cluster(data=dat, 
                                  formula = f4,
                                  cluster="ccode",
                                  family="binomial")
summary(model4_c)
se4 <- unname(as.matrix(summary(model4_c))[,2])
pv4 <- unname(as.matrix(summary(model4_c))[,4])

## Table 2
# Note: the notes section is not properly aligned. Upon pasting the output of the following code below, 
# & \multicolumn{4}{l}{\parbox[t]{5.5in} needs to be replaced: \multicolumn{5}{l}{\parbox[t]{5.5in}

texreg(list(e1, e2, e3, e4),
       override.se = list(se1, se2, se3, se4),
       override.pval = list(pv1, pv2, pv3, pv4),
       stars = c(0.001, 0.01, 0.05, 0.1),
       symbol = "\\dagger",
       digits = 4,
       custom.model.names = c("(1)", "(2)", "(3)", "(4)"),
       custom.coef.names = c("Intercept",
                             "Primary Energy Consumption pc",
                             "Gross Domestic Product pc",
                             "Threat",
                             "Time Count",
                             "Time Count$^2$",
                             "Time Count$^3$",
                             "Polity2",
                             "Land Contiguities",
                             "Sea Contiguities"),
       reorder.coef = c(2, 3, 4, 8, 9, 10, 5, 6, 7, 1),
       caption.above = T,
       booktabs = T,
       use.packages = F,
       caption = "Energy Consumption and Territorial Disputes, 1816-2001",
       custom.note = ("\\parbox{\\linewidth}{\\vspace{2pt}\\small Significance levels: $\\dagger (p\\leq 0.1)$, $^* (p\\leq 0.05)$, $^{**} (p\\leq 0.01)$, $^{***} (p\\leq 0.001)$. \\\\
       \\vspace{2pt}Notes: Results for logistic regression analysis, which assesses the likelihood that states have at least one territorial militarized interstate dispute in a given year. Standard errors clustered by country in parentheses. Polynomial time-count variables are used to account for temporal dependence. The sample is inclusive to all states in the international system from 1816-2001. Primary Energy Consumption is a per-capita measure of each state's use of energy, and is expressed terms of the number of coal ton equivalents consumed per person. Gross Domestic Product is divided by the total population of the state; this per-capita measure is a commonly used indicator of economic development. Because the measures of Power Projection, Primary Energy Consumption per capita, and Gross Domestic Product per capita are right-skewed, we transform them using the natural logarithm. Threat is a measure of how competitive each state's geopolitical environment is, as a function of interest compatibility with potential rivals, their relative economic power, and the distance between these adversaries. Polity2 measures states' relative levels of democracy versus autocracy, from -10 for fully autocratic states to +10 for fully consolidated democracies. Land is a measure of the number of neighboring states with which the state of interest shares a contiguous land border. Sea is the number of states with coastlines within 400 nautical miles of the coastline of the state of interest– the maximum distance at which states' exclusive economic zones could overlap.}"),
       file = "/Users/thereseanders/Dropbox/Make\ Or\ Take/Figures_Tables/logit_territory_180621.tex",
       label = "tab1")


#####Table 3 DISTANCE AWAY ########
# model5
model5 <- panelAR(distance_avg_ln ~ pec_pc_ln + gdp_pc_ln + threat,
                  data = dat,
                  timeVar = "year",
                  panelVar = "ccode",
                  autoCorr = "ar1",
                  panelCorrMethod = "pcse")
summary(model5)

# model6
model6 <- panelAR(distance_avg_ln ~ pec_pc_ln + gdp_pc_ln + threat + polity2,
                  data = dat,
                  timeVar = "year",
                  panelVar = "ccode",
                  autoCorr = "ar1",
                  panelCorrMethod = "pcse")
summary(model6)

# model7
model7 <- panelAR(distance_avg_ln ~ pec_pc_ln + gdp_pc_ln + threat + polity2 + land + sea,
                  data = dat,
                  timeVar = "year",
                  panelVar = "ccode",
                  autoCorr = "ar1",
                  panelCorrMethod = "pcse")
summary(model7)

# model8
model8 <- panelAR(distance_avg_ln ~ pec_pc_ln + threat + polity2 + land + sea,
                  data = dat,
                  timeVar = "year",
                  panelVar = "ccode",
                  autoCorr = "ar1",
                  panelCorrMethod = "pcse")
summary(model8)


## Table 3
texreg(list(model5, model6, model7, model8),
       stars = c(0.001, 0.01, 0.05, 0.1),
       symbol = "\\dagger",
       digits = 4,
       reorder.coef = c(seq(2,7), 1),
       custom.coef.names = c("Intercept",
                             "Primary Energy Consumption pc",
                             "Gross Domestic Product pc",
                             "Threat",
                             "Polity2",
                             "Land Contiguities",
                             "Sea Contiguities"),
       custom.model.names = c("(1)", "(2)", "(3)", "(4)"),
       caption = "Energy Consumption and Territorial Disputes, 1816-2001",
       caption.above = T,
       booktabs = T,
       use.packages = F,
       custom.note = ("\\parbox{\\linewidth}{\\vspace{2pt}\\small Significance levels: $\\dagger (p\\leq 0.1)$, $^* (p\\leq 0.05)$, $^{**} (p\\leq 0.01)$, $^{***} (p\\leq 0.001)$. \\\\
       \\vspace{2pt}Notes: Results for linear regression with panel corrected standard errors, and an AR(1) term to account for serial autocorrelation. The dependent variable measures states' attempts to project power, as determined by how far each state's disputes take place away from its capital city, on average, in a given year. The sample is inclusive to all states in the international system from 1816-2001. Primary Energy Consumption is a per-capita measure of each state's use of energy, and is expressed in terms of the number of coal ton equivalents consumed per person. Gross Domestic Product is divided by the total population of the state; this per-capita measure is a commonly used indicator of economic development. Because the measures of Power Projection, Primary Energy Consumption per capita, and Gross Domestic Product per capita are right-skewed, we transform them using the natural logarithm. Threat is a measure of how competitive each state's geopolitical environment is, as a function of interest compatibility with potential rivals, their relative economic power, and the distance between these adversaries. Polity2 measures states' relative levels of democracy versus autocracy, from -10 for fully autocratic states to +10 for fully consolidated democracies. Land is a measure of the number of neighboring states with which the state of interest shares a contiguous land border. Sea is the number of states with coastlines within 400 nautical miles of the coastline of the state of interest– the maximum distance at which states' exclusive economic zones could overlap.}"),
       file = "/Users/thereseanders/Dropbox/Make\ Or\ Take/Figures_Tables/PCSE_distance_main_180621.tex",
       label = "tab2")


#####TABLE 4--NAVAL TONNAGE*****

# model9
model9 <- panelAR(totton_pc_ln ~ pec_pc_ln + gdp_pc_ln + threat,
                  data = dat,
                  timeVar = "year",
                  panelVar = "ccode",
                  autoCorr = "ar1",
                  panelCorrMethod = "pcse")
summary(model9)

# model10
model10 <- panelAR(totton_pc_ln ~ pec_pc_ln + gdp_pc_ln + threat + polity2,
                   data = dat,
                   timeVar = "year",
                   panelVar = "ccode",
                   autoCorr = "ar1",
                   panelCorrMethod = "pcse")
summary(model10)

# model7
model11 <- panelAR(totton_pc_ln ~ pec_pc_ln + gdp_pc_ln + threat + polity2 + land + sea,
                   data = dat,
                   timeVar = "year",
                   panelVar = "ccode",
                   autoCorr = "ar1",
                   panelCorrMethod = "pcse")
summary(model11)

# model12
model12 <- panelAR(totton_pc_ln ~ pec_pc_ln + threat + polity2 + land + sea,
                   data = dat,
                   timeVar = "year",
                   panelVar = "ccode",
                   autoCorr = "ar1",
                   panelCorrMethod = "pcse")
summary(model12)

texreg(list(model9, model10, model11, model12),
       stars = c(0.001, 0.01, 0.05, 0.1),
       symbol = "\\dagger",
       digits = 4,
       reorder.coef = c(seq(2,7), 1),
       custom.coef.names = c("Intercept",
                             "Primary Energy Consumption pc",
                             "Gross Domestic Product pc",
                             "Threat",
                             "Polity2",
                             "Land Contiguities",
                             "Sea Contiguities"),
       custom.model.names = c("(1)", "(2)", "(3)", "(4)"),
       caption = "Energy Consumption per capita and Naval Power, 1816-2001",
       caption.above = T,
       booktabs = T,
       use.packages = F,
       custom.note = ("\\parbox{\\linewidth}{\\vspace{2pt}\\small Significance levels: $\\dagger (p\\leq 0.1)$, $^* (p\\leq 0.05)$, $^{**} (p\\leq 0.01)$, $^{***} (p\\leq 0.001)$. \\\\
       \\vspace{2pt}Notes: Results for linear regression with panel corrected standard errors, and an AR(1) term to account for serial autocorrelation. The dependent variable measures states' Naval Power as determined by the total tonnage of naval vessels in their armed forces at a given time. The sample is inclusive to all states in the international system from 1870-2001, and is limited by the availability of data for naval power and the threat environment. Primary Energy Consumption is a per-capita measure of each state's use of energy, and is expressed terms of the number of coal ton equivalents consumed per person. Gross Domestic Product is divided by the total population of the state; this per-capita measure is a commonly used indicator of economic development. Because the measures of Naval Power per capita, Primary Energy Consumption per capita, and Gross Domestic Product per capita are right-skewed, we transform them using the natural logarithm. Threat is a measure of how competitive each state's geopolitical environment is, as a function of interest compatibility with potential rivals, their relative economic power, and the distance between these adversaries. Polity2 measures states' relative levels of democracy versus autocracy, from -10 for fully autocratic states to +10 for fully consolidated democracies. Land is a measure of the number of neighboring states with which the state of interest shares a contiguous land border. Sea is the number of states with coastlines within 400 nautical miles of the coastline of the state of interest– the maximum distance at which states' exclusive economic zones could overlap.}"),
       file = "/Users/thereseanders/Dropbox/Make\ Or\ Take/Figures_Tables/navies_output_180621.tex",
       label = "tab3")



#####TABLE 5--SEE BELOW****


#####TABLE 6--Year FE********

#logit  territory_binary pec_pc_ln gdp_pc_ln threat polity2 territory_binary_years territory_binary_years_squared territory_binary_years_cubed i.year
model13 <- glm(territory_binary ~ pec_pc_ln +
                 gdp_pc_ln +
                 threat +
                 polity2 +
                 territory_binary_years +
                 territory_binary_years_squared +
                 territory_binary_years_cubed + factor(year),  
              data = dat, 
              family = binomial("logit"))
summary(model13)
# 
# NOTE: the xtreg regression replication files are contained in a separate STATA do file
# #eststo: xtreg distance_avg_ln pec_pc_ln  gdp_pc_ln threat  polity2 i.year
# #eststo: xtreg totton_pc_ln pec_pc_ln gdp_pc_ln threat polity2 i.year


#####TABLE 7--SEE BELOW*****


#####Table 8--MID Distance tests only on sample of state years with at least one MID****

pdat$distance_avg_ln_alt <- pdat$distance_avg_ln
table(is.na(pdat$distance_avg_ln)) #8640
table(pdat$distance_avg_ln[pdat$distance_avg_ln == 0]) #25942
pdat$distance_avg_ln_alt <- ifelse(pdat$distance_avg_ln == 0, NA, pdat$distance_avg_ln_alt)
table(is.na(pdat$distance_avg_ln_alt)) #34582 = 8640 + 25942

#eststo: xtpcse distance_avg_ln_alt pec_pc_ln upop tpop milper milex gdp_pc_ln threat, correlation(ar1) pairwise 
dat_new <- dat %>%
  mutate(distance_avg_ln_alt = distance_avg_ln) %>%
  mutate(distance_avg_ln_alt = replace(distance_avg_ln_alt, distance_avg_ln_alt == 0, NA))

# model16
model16 <- panelAR(distance_avg_ln_alt ~ pec_pc_ln + 
                     gdp_pc_ln + 
                     threat,
                  data = dat_new,
                  timeVar = "year",
                  panelVar = "ccode",
                  autoCorr = "ar1",
                  panelCorrMethod = "pcse",
                  rho.na.rm = T)
summary(model16)


# model17
model17 <- panelAR(distance_avg_ln_alt ~ pec_pc_ln + 
                     gdp_pc_ln + 
                     threat +
                     polity2,
                   data = dat_new,
                   timeVar = "year",
                   panelVar = "ccode",
                   autoCorr = "ar1",
                   panelCorrMethod = "pcse",
                   rho.na.rm = T)
summary(model17)

# model18
model18 <- panelAR(distance_avg_ln_alt ~ pec_pc_ln + 
                     gdp_pc_ln + 
                     threat +
                     polity2 + 
                     land + 
                     sea,
                   data = dat_new,
                   timeVar = "year",
                   panelVar = "ccode",
                   autoCorr = "ar1",
                   panelCorrMethod = "pcse",
                   rho.na.rm = T)
summary(model18)

# model19
model19 <- panelAR(distance_avg_ln_alt ~ pec_pc_ln + 
                     threat +
                     polity2 + 
                     land + 
                     sea,
                   data = dat_new,
                   timeVar = "year",
                   panelVar = "ccode",
                   autoCorr = "ar1",
                   panelCorrMethod = "pcse",
                   rho.na.rm = T)
summary(model19)


## Table 8
texreg(list(model16, model17, model18, model19),
       stars = c(0.001, 0.01, 0.05, 0.1),
       symbol = "\\dagger",
       digits = 4,
       reorder.coef = c(seq(2,7), 1),
       custom.coef.names = c("Intercept",
                             "Primary Energy Consumption pc",
                             "Gross Domestic Product pc",
                             "Threat",
                             "Polity2",
                             "Land Contiguities",
                             "Sea Contiguities"),
       custom.model.names = c("(1)", "(2)", "(3)", "(4)"),
       caption = "Power Projection for All Country-Year Units with at least one MID",
       caption.above = T,
       booktabs = T,
       use.packages = F,
       custom.note = ("\\parbox{\\linewidth}{\\vspace{2pt}\\small Significance levels: $\\dagger (p\\leq 0.1)$, $^* (p\\leq 0.05)$, $^{**} (p\\leq 0.01)$, $^{***} (p\\leq 0.001)$. \\\\
                      \\vspace{2pt}Notes: Results for linear regression with panel corrected standard errors, and an AR(1) term to account for serial autocorrelation. The dependent variable measures states' attempts to project power, as determined by how far each state's disputes take place away from its capital city, on average, in a given year. The sample is inclusive to all states in the international system from 1816-2001 which experienced at least one MID. Primary Energy Consumption is a per-capita measure of each state's use of energy, and is expressed in terms of the number of coal ton equivalents consumed per person. Gross Domestic Product is divided by the total population of the state; this per-capita measure is a commonly used indicator of economic development. Because the measures of Power Projection, Primary Energy Consumption per capita, and Gross Domestic Product per capita are right-skewed, we transform them using the natural logarithm. Threat is a measure of how competitive each state's geopolitical environment is, as a function of interest compatibility with potential rivals, their relative economic power, and the distance between these adversaries. Polity2 measures states' relative levels of democracy versus autocracy, from -10 for fully autocratic states to +10 for fully consolidated democracies. Land is a measure of the number of neighboring states with which the state of interest shares a contiguous land border. Sea is the number of states with coastlines within 400 nautical miles of the coastline of the state of interest– the maximum distance at which states' exclusive economic zones could overlap.}"),
       file = "/Users/thereseanders/Dropbox/Make\ Or\ Take/Figures_Tables/PCSE_distance_MIDyrsOnly_180621.tex",
       label = "tab4")






#####TABLE 9--ETHNICALLY SALIENT TERRITORY********
# model20
f20 <- as.formula(tcidenchal ~ pec_pc_ln + 
                    gdp_pc_ln +
                    threat +
                    tcidenchalyrs +
                    tcidenchalyrs_squared +
                    tcidenchalyrs_cubed)
model20 <- glm(f20,  
              data = dat, 
              family = binomial(logit))
e20 <- extract(model20,
              include.aic = F,
              include.bic = F,
              include.loglik = F,
              include.deviance = F)

model20_c <- miceadds::glm.cluster(data=dat, 
                                  formula = f20,
                                  cluster="ccode",
                                  family="binomial")
summary(model20_c)
se20 <- unname(as.matrix(summary(model20_c))[,2])
pv20 <- unname(as.matrix(summary(model20_c))[,4])

# model21
f21 <- as.formula(tcidenchal ~ pec_pc_ln + 
                    gdp_pc_ln +
                    threat + +
                    polity2 +
                    tcidenchalyrs +
                    tcidenchalyrs_squared +
                    tcidenchalyrs_cubed)
model21 <- glm(f21,  
               data = dat, 
               family = binomial(logit))
e21 <- extract(model21,
               include.aic = F,
               include.bic = F,
               include.loglik = F,
               include.deviance = F)

model21_c <- miceadds::glm.cluster(data=dat, 
                                   formula = f21,
                                   cluster="ccode",
                                   family="binomial")
summary(model21_c)
se21 <- unname(as.matrix(summary(model21_c))[,2])
pv21 <- unname(as.matrix(summary(model21_c))[,4])

# model22
f22 <- as.formula(tcidenchal ~ pec_pc_ln + 
                    gdp_pc_ln +
                    threat + +
                    polity2 +
                    land +
                    sea +
                    tcidenchalyrs +
                    tcidenchalyrs_squared +
                    tcidenchalyrs_cubed)
model22 <- glm(f22,  
               data = dat, 
               family = binomial(logit))
e22 <- extract(model22,
               include.aic = F,
               include.bic = F,
               include.loglik = F,
               include.deviance = F)

model22_c <- miceadds::glm.cluster(data=dat, 
                                   formula = f22,
                                   cluster="ccode",
                                   family="binomial")
summary(model22_c)
se22 <- unname(as.matrix(summary(model22_c))[,2])
pv22 <- unname(as.matrix(summary(model22_c))[,4])


texreg(list(e20, e21, e22),
       override.se = list(se20, se21, se22),
       override.pval = list(pv20, pv21, pv22),
       stars = c(0.001, 0.01, 0.05, 0.1),
       symbol = "\\dagger",
       digits = 4,
       custom.model.names = c("(1)", "(2)", "(3)"),
       custom.coef.names = c("Intercept",
                             "Primary Energy Consumption pc",
                             "Gross Domestic Product pc",
                             "Threat",
                             "Time Count",
                             "Time Count$^2$",
                             "Time Count$^3$",
                             "Polity2",
                             "Land Contiguities",
                             "Sea Contiguities"),
       reorder.coef = c(2, 3, 4, 8, 9, 10, 5, 6, 7, 1),
       caption.above = T,
       booktabs = T,
       use.packages = F,
       caption = "Disputes over Ethnically Salient Territory",
       custom.note = ("\\parbox{\\linewidth}{\\vspace{2pt}\\small Significance levels: $\\dagger (p\\leq 0.1)$, $^* (p\\leq 0.05)$, $^{**} (p\\leq 0.01)$, $^{***} (p\\leq 0.001)$. \\\\
                      \\vspace{2pt}Notes: Results for logistic regression analysis, which assesses the likelihood that states compete over territory that “includes significant portions of ethnic, religious, linguistic, or other identity groups linked to the target state” (Frederick, Hensel, and Macaulay 2017). Polynomial time-count variables are used to account for temporal dependence. The sample is inclusive to all states in the international system from 1816-2001. Primary Energy Consumption is a per-capita measure of each state's use of energy, and is expressed terms of the number of coal ton equivalents consumed per person. Gross Domestic Product is divided by the total population of the state; this per-capita measure is a commonly used indicator of economic development. Because the measures of Power Projection, Primary Energy Consumption per capita, and Gross Domestic Product per capita are right-skewed, we transform them using the natural logarithm. Threat is a measure of how competitive each state's geopolitical environment is, as a function of interest compatibility with potential rivals, their relative economic power, and the distance between these adversaries. Polity2 measures states' relative levels of democracy versus autocracy, from -10 for fully autocratic states to +10 for fully consolidated democracies. Land is a measure of the number of neighboring states with which the state of interest shares a contiguous land border. Sea is the number of states with coastlines within 400 nautical miles of the coastline of the state of interest– the maximum distance at which states' exclusive economic zones could overlap.}"),
       file = "/Users/thereseanders/Dropbox/Make\ Or\ Take/Figures_Tables/ethnicterritorysalience_180621.tex",
       label = "tab5")





#####TABLE 10********

# model23
f23 <- as.formula(territory_binary ~ pec_pc_ln +
                    rent_addiction_high +
                    gdp_pc_ln + 
                    threat + 
                    polity2 + 
                    land + 
                    sea + 
                    territory_binary_years + 
                    territory_binary_years_squared + 
                    territory_binary_years_cubed)
model23 <- glm(f23,  
              data = dat, 
              family = binomial(logit))
e23 <- extract(model23,
               include.aic = F,
               include.bic = F,
               include.loglik = F,
               include.deviance = F)

model23_c <- miceadds::glm.cluster(data=dat, 
                                  formula = f23,
                                  cluster="ccode",
                                  family="binomial")
summary(model23_c)
se23 <- unname(as.matrix(summary(model23_c))[,2])
pv23 <- unname(as.matrix(summary(model23_c))[,4])

# model24
f24 <- as.formula(territory_binary ~ pec_pc_ln +
                    ag_high_dummy2 +
                    gdp_pc_ln + 
                    threat + 
                    polity2 + 
                    land + 
                    sea + 
                    territory_binary_years + 
                    territory_binary_years_squared + 
                    territory_binary_years_cubed)
model24 <- glm(f24,  
               data = dat, 
               family = binomial(logit))
e24 <- extract(model24,
               include.aic = F,
               include.bic = F,
               include.loglik = F,
               include.deviance = F)

model24_c <- miceadds::glm.cluster(data=dat, 
                                   formula = f24,
                                   cluster="ccode",
                                   family="binomial")
summary(model24_c)
se24 <- unname(as.matrix(summary(model24_c))[,2])
pv24 <- unname(as.matrix(summary(model24_c))[,4])

# model25
f25 <- as.formula(territory_binary ~ pec_pc_ln +
                    rent_addiction_high +
                    ag_high_dummy2 +
                    gdp_pc_ln + 
                    threat + 
                    polity2 + 
                    land + 
                    sea + 
                    territory_binary_years + 
                    territory_binary_years_squared + 
                    territory_binary_years_cubed)
model25 <- glm(f25,  
               data = dat, 
               family = binomial(logit))
e25 <- extract(model25,
               include.aic = F,
               include.bic = F,
               include.loglik = F,
               include.deviance = F)

model25_c <- miceadds::glm.cluster(data=dat, 
                                   formula = f25,
                                   cluster="ccode",
                                   family="binomial")
summary(model25_c)
se25 <- unname(as.matrix(summary(model25_c))[,2])
pv25 <- unname(as.matrix(summary(model25_c))[,4])

# model26
f26 <- as.formula(territory_binary ~ pec_pc_ln +
                    oil_gas_gdp +
                    gdp_pc_ln + 
                    threat + 
                    polity2 + 
                    land + 
                    sea + 
                    territory_binary_years + 
                    territory_binary_years_squared + 
                    territory_binary_years_cubed)
model26 <- glm(f26,  
               data = dat, 
               family = binomial(logit))
e26 <- extract(model26,
               include.aic = F,
               include.bic = F,
               include.loglik = F,
               include.deviance = F)

model26_c <- miceadds::glm.cluster(data=dat, 
                                   formula = f26,
                                   cluster="ccode",
                                   family="binomial")
summary(model26_c)
se26 <- unname(as.matrix(summary(model26_c))[,2])
pv26 <- unname(as.matrix(summary(model26_c))[,4])

# model27
f27 <- as.formula(territory_binary ~ pec_pc_ln +
                    ag_gdp_wdi +
                    gdp_pc_ln + 
                    threat + 
                    polity2 + 
                    land + 
                    sea + 
                    territory_binary_years + 
                    territory_binary_years_squared + 
                    territory_binary_years_cubed)
model27 <- glm(f27,  
               data = dat, 
               family = binomial(logit))
e27 <- extract(model27,
               include.aic = F,
               include.bic = F,
               include.loglik = F,
               include.deviance = F)

model27_c <- miceadds::glm.cluster(data=dat, 
                                   formula = f27,
                                   cluster="ccode",
                                   family="binomial")
summary(model27_c)
se27 <- unname(as.matrix(summary(model27_c))[,2])
pv27 <- unname(as.matrix(summary(model27_c))[,4])

texreg(list(e23, e24, e25, e26, e27),
       override.se = list(se23, se24, se25, se26, se27),
       override.pval = list(pv23, pv24, pv25, pv26, pv27),
       stars = c(0.001, 0.01, 0.05, 0.1),
       symbol = "\\dagger",
       digits = 4,
       custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)"),
       custom.coef.names = c("Intercept",
                             "Primary Energy Consumption pc",
                             "Rent Addiction",
                             "Gross Domestic Product pc",
                             "Threat",
                             "Polity2",
                             "Land Contiguities",
                             "Sea Contiguities",
                             "Time Count",
                             "Time Count$^2$",
                             "Time Count$^3$",
                             "Ag Dependence",
                             "GDP from Oil/Gas",
                             "WDI GDP from Ag"),
       reorder.coef = c(2, 3, 12, 13, 14, seq(4, 11), 1),
       caption.above = T,
       booktabs = T,
       use.packages = F,
       fontsize = "small",
       caption = "Robustness Checks for Energy Consumption and Territorial Disputes",
       custom.note = ("\\parbox{\\linewidth}{\\vspace{2pt}\\small Significance levels: $\\dagger (p\\leq 0.1)$, $^* (p\\leq 0.05)$, $^{**} (p\\leq 0.01)$, $^{***} (p\\leq 0.001)$. \\\\
                      \\vspace{2pt}Notes: Results for logistic regression analysis, which assesses the likelihood that states have at least one territorial militarized interstate dispute in a given year. Polynomial time-count variables are used to account for temporal dependence. Primary Energy Consumption is a per-capita measure of each state's use of energy, and is expressed terms of the number of coal ton equivalents consumed per person. In addition to this measure, we also control for several additional measures of the structure of the economy for the country-year unit. We make use of two new variables Oil and Gas revenue as a percentage of GDP (available from 1932 to 2014) and the Agriculture revenue as a percentage of GDP (available from 1960 to 2015). We also use this Agriculture variable to create a dichotomous variable if more than 10\\% of revenue comes from agriculture. Finally, we create a rent addiction with a linear prediction model that makes use of the observed data we have available. We are working on developing a more rigorous imputation model for these indicators in another project. Gross Domestic Product is divided by the total population of the state; this per-capita measure is a commonly used indicator of economic development. Because the measures of Power Projection, Primary Energy Consumption per capita, and Gross Domestic Product per capita are right-skewed, we transform them using the natural logarithm. Threat is a measure of how competitive each state's geopolitical environment is, as a function of interest compatibility with potential rivals, their relative economic power, and the distance between these adversaries. Polity2 measures states' relative levels of democracy versus autocracy, from -10 for fully autocratic states to +10 for fully consolidated democracies. Land is a measure of the number of neighboring states with which the state of interest shares a contiguous land border. Sea is the number of states with coastlines within 400 nautical miles of the coastline of the state of interest– the maximum distance at which states' exclusive economic zones could overlap.}"),
       file = "/Users/thereseanders/Dropbox/Make\ Or\ Take/Figures_Tables/logit_territory_robustcheck_180621.tex",
       label = "tab6")


#####TABLE 11*****
# pdat$distance_max_ln <- log(pdat$distance_max+1)
# pdat$distance_avg_ln <- log(pdat$distance_avg+1)
# pdat$distance_avg_ln <- ifelse(pdat$year %in% seq(2002,2017),NA,pdat$distance_avg_ln)
# table(pdat$year[!is.na(pdat$distance_avg_ln)])

#eststo: xtpcse distance_avg_ln pec_pc_ln rent_addiction_high gdp_pc_ln threat polity2 land sea, correlation(ar1) pairwise


# model28
model28 <- panelAR(distance_avg_ln ~ pec_pc_ln +
                    rent_addiction_high +
                    gdp_pc_ln +
                    threat +
                    polity2 +
                    land +
                    sea,
                  data = dat,
                  timeVar = "year",
                  panelVar = "ccode",
                  autoCorr = "ar1",
                  panelCorrMethod = "pcse")
summary(model28)

# model29
model29 <- panelAR(distance_avg_ln ~ pec_pc_ln +
                     ag_high_dummy2 +
                     gdp_pc_ln +
                     threat +
                     polity2 +
                     land +
                     sea,
                   data = dat,
                   timeVar = "year",
                   panelVar = "ccode",
                   autoCorr = "ar1",
                   panelCorrMethod = "pcse")
summary(model29)

# model30
model30 <- panelAR(distance_avg_ln ~ pec_pc_ln +
                     rent_addiction_high +
                     ag_high_dummy2 +
                     gdp_pc_ln +
                     threat +
                     polity2 +
                     land +
                     sea,
                   data = dat,
                   timeVar = "year",
                   panelVar = "ccode",
                   autoCorr = "ar1",
                   panelCorrMethod = "pcse")
summary(model30)

# model31
model31 <- panelAR(distance_avg_ln ~ pec_pc_ln +
                     oil_gas_gdp +
                     gdp_pc_ln +
                     threat +
                     polity2 +
                     land +
                     sea,
                   data = dat,
                   timeVar = "year",
                   panelVar = "ccode",
                   autoCorr = "ar1",
                   panelCorrMethod = "pcse")

# model32
model32 <- panelAR(distance_avg_ln ~ pec_pc_ln +
                     ag_gdp_wdi +
                     gdp_pc_ln +
                     threat +
                     polity2 +
                     land +
                     sea,
                   data = dat,
                   timeVar = "year",
                   panelVar = "ccode",
                   autoCorr = "ar1",
                   panelCorrMethod = "pcse",
                   rho.na.rm = T)


## Table 11
texreg(list(model28, model29, model30, model31, model32),
       stars = c(0.001, 0.01, 0.05, 0.1),
       symbol = "\\dagger",
       digits = 4,
       reorder.coef = c(2, 3, 9, 10, 11, 4, 5, 6, 7, 8, 1),
       custom.coef.names = c("Intercept",
                             "Primary Energy Consumption pc",
                             "Rent Addiction",
                             "Gross Domestic Product pc",
                             "Threat",
                             "Polity2",
                             "Land Contiguities",
                             "Sea Contiguities",
                             "Ag Dependence",
                             "GDP from Oil/Gas",
                             "WDI GDP from Ag"),
       custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)"),
       caption = "Robustness Checks for Energy Consumption and Power Projection",
       caption.above = T,
       booktabs = T,
       use.packages = F,
       custom.note = ("\\parbox{\\linewidth}{\\vspace{2pt}\\small Significance levels: $\\dagger (p\\leq 0.1)$, $^* (p\\leq 0.05)$, $^{**} (p\\leq 0.01)$, $^{***} (p\\leq 0.001)$. \\\\
       \\vspace{2pt}Notes: Results for linear regression with panel corrected standard errors, and an AR(1) term to account for serial autocorrelation. The dependent variable measures states' attempts to project power, as determined by how far each state's disputes take place away from its capital city, on average, in a given year. Primary Energy Consumption is a per-capita measure of each state's use of energy, and is expressed in terms of the number of coal ton equivalents consumed per person. We make use of two new variables Oil and Gas revenue as a percentage of GDP (available from 1932 to 2014) and the Agriculture revenue as a percentage of GDP (available from 1960 to 2015). We also use this Agriculture variable to create a dichotomous variable if more than 10\\% of revenue comes from agriculture. Finally, we create a rent addiction with a linear prediction model that makes use of the observed data we have available. We are working on developing a more rigorous imputation model for these indicators in another project. Gross Domestic Product is divided by the total population of the state; this per-capita measure is a commonly used indicator of economic development. Because the measures of Power Projection, Primary Energy Consumption per capita, and Gross Domestic Product per capita are right-skewed, we transform them using the natural logarithm. Threat is a measure of how competitive each state's geopolitical environment is, as a function of interest compatibility with potential rivals, their relative economic power, and the distance between these adversaries. Polity2 measures states' relative levels of democracy versus autocracy, from -10 for fully autocratic states to +10 for fully consolidated democracies. Land is a measure of the number of neighboring states with which the state of interest shares a contiguous land border. Sea is the number of states with coastlines within 400 nautical miles of the coastline of the state of interest– the maximum distance at which states' exclusive economic zones could overlap.}"),
       file = "/Users/thereseanders/Dropbox/Make\ Or\ Take/Figures_Tables/PCSE_robust_check_180621.tex",
       label = "tab7")

#######TABLE 12#######
# model33
model33 <- panelAR(totton_pc_ln ~ pec_pc_ln +
                     rent_addiction_high +
                     gdp_pc_ln +
                     threat +
                     polity2 +
                     land +
                     sea,
                   data = dat,
                   timeVar = "year",
                   panelVar = "ccode",
                   autoCorr = "ar1",
                   panelCorrMethod = "pcse")
summary(model33)

# model34
model34 <- panelAR(totton_pc_ln ~ pec_pc_ln +
                     ag_high_dummy2 +
                     gdp_pc_ln +
                     threat +
                     polity2 +
                     land +
                     sea,
                   data = dat,
                   timeVar = "year",
                   panelVar = "ccode",
                   autoCorr = "ar1",
                   panelCorrMethod = "pcse")

# model35
model35 <- panelAR(totton_pc_ln ~ pec_pc_ln +
                     rent_addiction_high +
                     ag_high_dummy2 +
                     gdp_pc_ln +
                     threat +
                     polity2 +
                     land +
                     sea,
                   data = dat,
                   timeVar = "year",
                   panelVar = "ccode",
                   autoCorr = "ar1",
                   panelCorrMethod = "pcse")

# model36
model36 <- panelAR(totton_pc_ln ~ pec_pc_ln +
                     oil_gas_gdp +
                     gdp_pc_ln +
                     threat +
                     polity2 +
                     land +
                     sea,
                   data = dat,
                   timeVar = "year",
                   panelVar = "ccode",
                   autoCorr = "ar1",
                   panelCorrMethod = "pcse")
summary(model36)

# model37
model37 <- panelAR(totton_pc_ln ~ pec_pc_ln +
                     ag_gdp_wdi +
                     gdp_pc_ln +
                     threat +
                     polity2 +
                     land +
                     sea,
                   data = dat,
                   timeVar = "year",
                   panelVar = "ccode",
                   autoCorr = "ar1",
                   panelCorrMethod = "pcse",
                   rho.na.rm = T)


## Table 11
texreg(list(model33, model34, model35, model36, model37),
       stars = c(0.001, 0.01, 0.05, 0.1),
       symbol = "\\dagger",
       digits = 4,
       reorder.coef = c(2, 3, 9, 10, 11, 4, 5, 6, 7, 8, 1),
       custom.coef.names = c("Intercept",
                             "Primary Energy Consumption pc",
                             "Rent Addiction",
                             "Gross Domestic Product pc",
                             "Threat",
                             "Polity2",
                             "Land Contiguities",
                             "Sea Contiguities",
                             "Ag Dependence",
                             "GDP from Oil/Gas",
                             "WDI GDP from Ag"),
       custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)"),
       caption = "Robustness Checks for Energy Consumption and Naval Power per capita",
       caption.above = T,
       booktabs = T,
       use.packages = F,
       custom.note = ("\\parbox{\\linewidth}{\\vspace{2pt}\\small Significance levels: $\\dagger (p\\leq 0.1)$, $^* (p\\leq 0.05)$, $^{**} (p\\leq 0.01)$, $^{***} (p\\leq 0.001)$. \\\\
                      \\vspace{2pt}Notes: Results for linear regression with panel corrected standard errors, and an AR(1) term to account for serial autocorrelation. The dependent variable measures states' Naval Power as determined by the total tonnage of naval vessels in their armed forces at a given time. The sample is inclusive to all states in the international system from 1870-2001, and is limited by the availability of data for naval power and the threat environment. Primary Energy Consumption is a per-capita measure of each state's use of energy, and is expressed terms of the number of coal ton equivalents consumed per person. Gross Domestic Product is divided by the total population of the state; this per-capita measure is a commonly used indicator of economic development. Because the measures of Naval Power per capita, Primary Energy Consumption per capita, and Gross Domestic Product per capita are right-skewed, we transform them using the natural logarithm. Threat is a measure of how competitive each state's geopolitical environment is, as a function of interest compatibility with potential rivals, their relative economic power, and the distance between these adversaries. Polity2 measures states' relative levels of democracy versus autocracy, from -10 for fully autocratic states to +10 for fully consolidated democracies. Land is a measure of the number of neighboring states with which the state of interest shares a contiguous land border. Sea is the number of states with coastlines within 400 nautical miles of the coastline of the state of interest– the maximum distance at which states' exclusive economic zones could overlap.}"),
       file = "/Users/thereseanders/Dropbox/Make\ Or\ Take/Figures_Tables/navies_output_robustcheck_180621.tex",
       label = "tab8")



#######TABLE 5--POST WWII ERA#######

dat_post1945 <- dat %>%
  filter(year >= 1946)

#model38
f38 <- as.formula(territory_binary ~ pec_pc_ln + gdp_pc_ln + threat + polity2 +  territory_binary_years + territory_binary_years_squared + territory_binary_years_cubed)

model38 <- glm(f38,  
              data = dat_post1945, 
              family = binomial(logit))

model38_c <- miceadds::glm.cluster(data = dat_post1945, 
                                  formula = f38,
                                  cluster="ccode",
                                  family="binomial")
summary(model38_c)
se38 <- unname(as.matrix(summary(model38_c))[,2])
pv38 <- unname(as.matrix(summary(model38_c))[,4])


ex38 <- extract(model38,
                include.aic = F,
                include.bic = F,
                include.loglik = F,
                include.deviance = F)

# model39
model39 <- panelAR(distance_avg_ln ~ pec_pc_ln + gdp_pc_ln + threat + polity2,
                  data = dat_post1945,
                  timeVar = "year",
                  panelVar = "ccode",
                  autoCorr = "ar1",
                  panelCorrMethod = "pcse")
se39 <- unname(summary(model39)$coefficients[,2])
pv39 <- unname(summary(model39)$coefficients[,4])

# model40
model40 <- panelAR(totton_pc_ln ~ pec_pc_ln + gdp_pc_ln + threat + polity2,
                   data = dat_post1945,
                   timeVar = "year",
                   panelVar = "ccode",
                   autoCorr = "ar1",
                   panelCorrMethod = "pcse")
summary(model40)
se40 <- unname(summary(model40)$coefficients[,2])
pv40 <- unname(summary(model40)$coefficients[,4])


## Table 4
texreg(list(ex38, model39, model40),
       override.se = list(se38, se39, se40),
       override.pval = list(pv38, pv39, pv40),
       stars = c(0.001, 0.01, 0.05, 0.1),
       symbol = "\\dagger",
       digits = 4,
       reorder.coef = c(seq(2,8), 1),
       custom.coef.names = c("Intercept",
                             "Primary Energy Consumption pc",
                             "Gross Domestic Product pc",
                             "Threat",
                             "Polity2",
                             "Time Count",
                             "Time Count$^2$",
                             "Time Count$^3$"),
       custom.model.names = c("(1)", "(2)", "(3)"),
       caption = "Post-World War II Era (after 1945) Robustness Checks",
       caption.above = T,
       booktabs = T,
       use.packages = F,
       custom.note = ("\\parbox{\\linewidth}{\\vspace{2pt}\\small Significance levels: $\\dagger (p\\leq 0.1)$, $^* (p\\leq 0.05)$, $^{**} (p\\leq 0.01)$, $^{***} (p\\leq 0.001)$. \\\\
                      \\vspace{2pt}Notes: Models 1-3 analyze the robustness of our main regression results in a sample that is restricted to observations after 1945. Model 1 assesses the likelihood that states have at least one territorial militarized interstate dispute in a given year. Because the dependent variables is dichotomous, we utilize a logistic regression model with polynomial time-count variables to account for serial autocorrelation. Standard errors clustered by country in parentheses. Models 2 and 3 present results for linear regression tests with panel corrected standard errors, and an AR(1) term to account for serial autocorrelation. Model 2 analyses how far states project military power on average, by observing the average distance away from states' national capitals at which militarized interstate disputes occur in each year. The dependent variable in model 3 measures states' Naval Power as determined by the total tonnage of naval vessels in their armed forces at a given time. Primary Energy Consumption is a per-capita measure of each state's use of energy, and is expressed in terms of the number of coal ton equivalents consumed per person. Gross Domestic Product is divided by the total population of the state; this per-capita measure is a commonly used indicator of economic development. Because the measures of Power Projection, Naval Tonnage, Primary Energy Consumption per capita, and Gross Domestic Product per capita are right-skewed, we transform them using the natural logarithm. Threat is a measure of how competitive each state's geopolitical environment is, as a function of interest compatibility with potential rivals, their relative economic power, and the distance between these adversaries. Polity2 measures states' relative levels of democracy versus autocracy, from -10 for fully autocratic states to +10 for fully consolidated democracies.}"),
       file = "/Users/thereseanders/Dropbox/Make\ Or\ Take/Figures_Tables/postcolonial_180621.tex",
       label = "tab9")



#######TABLE 7--WITHOUT MAJPOWS--WONKY#######

dat_nomajpow <- dat %>%
  filter(!(ccode %in% c(2, 200, 220, 255, 300, 325, 365, 710, 740)))


#model41
f41 <- as.formula(territory_binary ~ pec_pc_ln + gdp_pc_ln + threat + polity2 +  territory_binary_years + territory_binary_years_squared + territory_binary_years_cubed)

model41 <- glm(f41,  
               data = dat_nomajpow, 
               family = binomial(logit))

model41_c <- miceadds::glm.cluster(data = dat_nomajpow, 
                                   formula = f41,
                                   cluster="ccode",
                                   family="binomial")
summary(model41_c)
se41 <- unname(as.matrix(summary(model41_c))[,2])
pv41 <- unname(as.matrix(summary(model41_c))[,4])

ex41 <- extract(model41,
                include.aic = F,
                include.bic = F,
                include.loglik = F,
                include.deviance = F)

# model42
model42 <- panelAR(distance_avg_ln ~ pec_pc_ln + gdp_pc_ln + threat + polity2,
                   data = dat_nomajpow,
                   timeVar = "year",
                   panelVar = "ccode",
                   autoCorr = "ar1",
                   panelCorrMethod = "pcse")
se42 <- unname(summary(model42)$coefficients[,2])
pv42 <- unname(summary(model42)$coefficients[,4])

# model43
model43 <- panelAR(totton_pc_ln ~ pec_pc_ln + gdp_pc_ln + threat + polity2,
                   data = dat_nomajpow,
                   timeVar = "year",
                   panelVar = "ccode",
                   autoCorr = "ar1",
                   panelCorrMethod = "pcse")
summary(model43)
se43 <- unname(summary(model43)$coefficients[,2])
pv43 <- unname(summary(model43)$coefficients[,4])


## Table 4
texreg(list(ex41, model42, model43),
       override.se = list(se41, se42, se43),
       override.pval = list(pv41, pv42, pv43),
       stars = c(0.001, 0.01, 0.05, 0.1),
       symbol = "\\dagger",
       digits = 4,
       reorder.coef = c(seq(2,8), 1),
       custom.coef.names = c("Intercept",
                             "Primary Energy Consumption pc",
                             "Gross Domestic Product pc",
                             "Threat",
                             "Polity2",
                             "Time Count",
                             "Time Count$^2$",
                             "Time Count$^3$"),
       custom.model.names = c("(1)", "(2)", "(3)"),
       caption = "Regression Results with Major Powers Excluded from Sample",
       caption.above = T,
       booktabs = T,
       use.packages = F,
       custom.note = ("\\parbox{\\linewidth}{\\vspace{2pt}\\small Significance levels: $\\dagger (p\\leq 0.1)$, $^* (p\\leq 0.05)$, $^{**} (p\\leq 0.01)$, $^{***} (p\\leq 0.001)$. \\\\
                      \\vspace{2pt}Notes: The United States, Great Britain, France, Russia, China, Germany, Austria, Japan, and Italy are excluded from the sam- ple. The first column shows logistic regression tests that assess the likelihood that states have at least one territorial militarized interstate dispute in a given year. Standard errors clustered by country in parentheses. The second column denotes tests on a dependent variable that measures states' attempts to project power, as determined by how far each state's disputes take place away from its capital city, on average, in a given year. The third column presents the results of tests on the total tonnage of naval power possessed by a state. Primary Energy Consumption is a per-capita measure of each state's use of energy, and is expressed in terms of the number of coal ton equiva- lents consumed per person. Gross Domestic Product is divided by the total population of the state; this per-capita measure is a commonly used indicator of economic development. Because the measures of Power Projection, Primary Energy Consumption per capita, and Gross Domestic Product per capita are right-skewed, we transform them using the natural logarithm. Threat is a measure of how competitive each state's geopolitical environment is, as a function of interest compatibility with potential rivals, their relative economic power, and the distance between these adversaries. Polity2 measures states' relative levels of democracy versus autocracy, from -10 for fully autocratic states to +10 for fully consolidated democracies. Land is a measure of the number of neighboring states with which the state of interest shares a contiguous land border. Sea is the number of states with coastlines within 400 nautical miles of the coastline of the state of interest– the maximum distance at which states' exclusive economic zones could overlap.}"),
       file = "/Users/thereseanders/Dropbox/Make\ Or\ Take/Figures_Tables/withoutmajpows_180621.tex",
       label = "tab10")


