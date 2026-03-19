#######################################################
# REPLICATION CODE
#######################################################

# install.packages("texreg")
# install.packages("Hmisc")
# install.packages("stargazer")

library(stargazer)
library(texreg)
library(arm)
library(foreign)
library(car)
library(xtable)
library(Hmisc)

data <- read.csv("complete.data.iv.csv")
attach(data)

head(data)
names(data)

###############################################################################
# VARIABLE DEFINITIONS
###############################################################################

# COLONIES
###########
# baseco = dummy for former colonies included in the base N=64 sample
# f_brit = BritishColony(Flopsexpsn)
# f_french = FrenchColony(Flopsexpans)
# rich4 = dummy=1 for neo-europes
# sjlofr = French legal orgin recoded
# excolony = =1 if was colony FLOPS definiti

# MORTALITY
##############
# logem4 = log of mortality rate (IV)
# mortality = mortality measure (deaths per 1000 soldiers) from Appendix Table A2, AJR
# imputedmort = imputed mortality rate from logem4 measure (=exp(logem4))
# extmort4 = corrected mortality (AJR)

# SETTLEMENT PATTERNS
######################
# euro1900 = european settlements in 1900

# INSTITUTIONS
###############
# currentinst = IFC ease of doing business index/rank, 2012 (** our measure)
# avexpr = avg. risk of expropriation, 1985-1995
# cons00a = constraint on executive in 1900
# cons1 = constraint on executive in 1st year of independence
# indtime = years independent: 1995 minus firstyr
# democ00a = democracy in 1900
# democ1 = democracy in first year of independence

# ECONOMIC PERFORMANCE
#######################
# logpgp95 = log GDP per capita PPP in 1995, World Bank 
# loghjypl = log output per worker in 1988 (US normalized to 1), Hall & Jones
# logpgp12 = log GDP per capital PPP in 2012, World Bank (** our measure)

# GEOGRAPHY/NATURAL ENVIRONMENT
###############################
# lat_abst = Abs(latitude of capital)/90
# meantemp = mean temperature (McArthur and Sachs)
# lt100km = amount of territory within 100 km of the coast (McArthur and Sachs)
# latabs = absolute latitude (McArthur and Sachs)
# africa = dummy=1 for Africa
# asia = dummy=1 for Asia
# other = dummy=1 if not in Asia, Africa, or the Americas
# temp1:temp5 = temperature indicators
# humid1:humid4 = humidity indicators
# steplow, deslow, stepmid, desmid, drystep, drywint = soil indicators
# landlock = =1 if landlocked
# goldm, iron, silv, zinc = mineral indicators
# oilres = oil reserves 

# SOCIAL/CULTURAL ENVIRONMENT
#############################
# catho80 = catho as %pop 1980 wce95
# muslim80 = muslims as % pop 1980 wce95
# no_cpm80 = 100-Cath-Protest-Muslim in 1980
# edes1975 = % of European descent in 1975
# avelf = ethno fract avg 5indic east_lev

# DISEASES
##########
# malfal94 = Falciparum malaria index 1994
# yellow = =1 if vector y.fever present today
# leb95 = life expectancy at birth (1995)
# imr95 = infant mortality rate (1995)

############################################################################################
# FIGURE 1 
############################################################################################
plot(currentinst)
plot(oilres)
plot(cons1)
plot(democ1)
plot(mortality)
plot(euro1900)
plot(logem4)
plot(imputedmort)

# Map showing mean temperatures in the former colonies

# Informality Prevalence Map
require(rworldmap)
require(classInt)
require(RColorBrewer)
data$Temperature <- data$meantemp
sPDF <- joinCountryData2Map(data,nameJoinColumn = "name", joinCode="NAME", mapResolution = "coarse") 

#getting colours
#colfunc <- colorRampPalette(c("white", "black"))
#colourPalette  <- colfunc(15)
colourPalette  <- brewer.pal(9,"Reds")

#plot map
mapDevice() #create world map shaped window
mapParams <- mapCountryData(sPDF
             ,nameColumnToPlot="Temperature"
             ,addLegend=FALSE
             ,colourPalette=colourPalette
             ) + title("")
#adding legend
do.call(addMapLegend
       ,c(mapParams
         ,legendLabels="all"
         ,legendWidth=0.5
         ,legendIntervals="data"
         ,legendMar = 2))


require(ggplot2)
cor(mortality,meantemp)
sp = ggplot(data, aes(x= meantemp, y= mortality))
sp + geom_point(shape=15)+xlab("Mean Temperature\n(degrees Celsius)") +ylab("European Settler Mortality\n(deaths per 1000 soldiers)") +  xlim(10,30) + ylim(0,3000) + theme(text = element_text(size=16), legend.text=element_text(size=18))+geom_rug(size=0.4) + annotate("text", label="r==0.31", parse=TRUE, x=15, y=2600, size=10) + geom_smooth(method="loess", se=F) + geom_text(aes(label=shortnam), position="jitter",vjust=2, size=2)

sp = ggplot(data, aes(x= meantemp, y= logem4))
sp + geom_point(shape=15)+xlab("Mean Temperature\n(degrees Celsius)") +ylab("Log European Settler Mortality\n(deaths per 1000 soldiers)") +  xlim(10,30) + ylim(0,10) + theme(text = element_text(size=16), legend.text=element_text(size=18))+geom_rug(size=0.4) + annotate("text", label="r==0.53", parse=TRUE, x=15, y=8, size=10) + geom_smooth(method="lm", se=T) + geom_text(aes(label=shortnam), position="jitter",vjust=2, size=2)


cor(logpgp95,meantemp)
sp = ggplot(data, aes(x= meantemp, y= logpgp95))
sp + geom_point(shape=15)+xlab("Mean Temperature\n(degrees Celsius)") +ylab("Log GDP, 1995") +  xlim(10,30)  + theme(text = element_text(size=16), legend.text=element_text(size=18))+geom_rug(size=0.4) + annotate("text", label="r==-0.59", x=14, y=7.2, parse=TRUE, size=10) + geom_smooth(method="loess", se=F) + geom_text(aes(label=shortnam), position="jitter",vjust=2, size=2)

sp = ggplot(data, aes(x= meantemp, y= logpgp95))
sp + geom_point(shape=15)+xlab("Mean Temperature\n(degrees Celsius)") +ylab("Log Economic Output\n(GDP in 1995)") +  xlim(10,30)  + theme(text = element_text(size=16), legend.text=element_text(size=18))+geom_rug(size=0.4) + annotate("text", label="r==-0.59", x=14, y=7.2, parse=TRUE, size=10) + geom_smooth(method="lm", se=T) + geom_text(aes(label=shortnam), position="jitter",vjust=2, size=2)

m1 =  lm(mortality ~ meantemp, data=data, which(baseco==1))
m2 =  lm(logpgp95 ~ meantemp, data=data, which(baseco==1))
m3 =  lm(cons00a ~ meantemp, data=data, which(baseco==1))
m4 =  lm(avexpr ~ meantemp, data=data, which(baseco==1))

# Output table
require(stargazer)
stargazer(m1, m2, m3, m4, 
          title="Effect of Mean Temperature on Settler Mortality, Economic Output, Constraint on the Executive, and Average Protection Against Expropriation Risk, 1985-1995",
          align=TRUE, dep.var.labels=c("Settler Mortality","Log GDP 1995",
                                       "Constraint on Executive", "Risk of Expropriation"),
          covariate.labels=c("Mean Temperature"),
          column.separate=c(1,2,3,4,5,6,7,8),
          notes.align=c("l"),
          omit.stat=c("LL", "ser","f"), no.space=TRUE, 
          font.size=c("scriptsize"),
          digits=2,
          column.sep.width=c("3pt"))

#############################################################################################
#REPLICATION OF TABLE 1 - DESCRIPTIVE STATISTICS - AJR PAPER 
#############################################################################################

# Output summary statistics tables in LATEX

# Whole world
data.world = subset(data[, c("logpgp95", "logpgp12", "loghjypl",
                             "avexpr", "currentinst", "cons00a", 
                             "cons1", "democ00a", "euro1900", 
                             "logem4", "mortality", "extmort4")])

m1 = stargazer(data.world, 
               title="Descriptive Statistics - Whole World", 
               covariate.labels=c("Log GDP per capita (PPP) in 1995",
                                  "Log GDP per capita (PPP) in 2012", 
                                  "Log output per worker in 1988 (with level of United States 
                                  normalized to 1)",
                                  "Average protection against expropriation
                                  risk, 1985-1995",
                                  "Ease of Doing Business Index, 2012",
                                  "Constraint on executive in 1900",
                                  "Constraint on executive in first year 
                                  of independence",
                                  "Democracy in 1900",
                                  "European settlements in 1900",
                                  "Log settler mortality",
                                  "Settler mortality", 
                                  "Corrected settler mortality"),
                                   font.size=c("scriptsize"),
                                   digits=2,
                                   column.sep.width=c("3pt")) 

# Base Sample
data.base = subset(data[ ,c("logpgp95", "logpgp12", "loghjypl", 
                            "avexpr", "currentinst", "cons00a", 
                            "cons1", "democ00a", "euro1900",
                            "logem4", "mortality", "extmort4")],data$baseco==1)
m2 = stargazer(data.base, title="Descriptive Statistics - Base Sample", 
               covariate.labels=c("Log GDP per capita (PPP) in 1995",
                                  "Log GDP per capita (PPP) in 2012", 
                                  "Log output per worker in 1988 (with level of United States 
                                  normalized to 1)",
                                  "Average protection against expropriation
                                  risk, 1985-1995",
                                  "Ease of Doing Business Index, 2012",
                                  "Constraint on executive in 1900",
                                  "Constraint on executive in first year 
                                  of independence",
                                  "Democracy in 1900",
                                  "European settlements in 1900",
                                  "Log settler mortality",
                                  "Settler mortality", 
                                  "Corrected settler mortality"),
                                   font.size=c("scriptsize"),
                                   digits=2,
                                   column.sep.width=c("3pt")) 

# Sample of Ex-colonies 
data.excol = subset(data[ ,c("logpgp95", "logpgp12", "loghjypl", 
                             "avexpr", "currentinst", "cons00a", 
                             "cons1", "democ00a", "euro1900", 
                             "logem4", "mortality", "extmort4")],data$excolony==1)

m3 = stargazer(data.excol, title="Descriptive Statistics - Ex-colonies Sample",
               covariate.labels=c("Log GDP per capita (PPP) in 1995",
                                  "Log GDP per capita (PPP) in 2012", 
                                  "Log output per worker in 1988 (with level of United States 
                                  normalized to 1)",
                                  "Average protection against expropriation
                                  risk, 1985-1995",
                                  "Ease of Doing Business Index, 2012",
                                  "Constraint on executive in 1900",
                                  "Constraint on executive in first year 
                                  of independence",
                                  "Democracy in 1900",
                                  "European settlements in 1900",
                                  "Log settler mortality",
                                  "Settler mortality", 
                                  "Corrected settler mortality"),
                                   font.size=c("scriptsize"),
                                   digits=2,
                                   column.sep.width=c("3pt")) 


#############################################################################################
#REPLICATION OF TABLE 2 - OLS REGRESSIONS - AJR PAPER 
#############################################################################################

# (1) Whole World - DV = logpgp95
m1 = lm(logpgp95 ~ avexpr, data=data)

# (2) Base Sample
m2 = lm(logpgp95 ~ avexpr, data=data, which(baseco==1))

# (3) Whole World
m3 = lm(logpgp95 ~ avexpr + lat_abst, data=data)

# (4) Whole World
m4 = lm(logpgp95 ~ avexpr + lat_abst + asia + africa + other, data=data)

# (5) Base Sample
m5 = lm(logpgp95 ~ avexpr + lat_abst, data=data, which(baseco==1))

# (6) Base Sample
m6 = lm(logpgp95 ~ avexpr + lat_abst + asia + africa + other, data=data, which(baseco==1))

# (7) Whole World DV = loghjypl
m7 =  lm(loghjypl ~ avexpr, data=data)
  
# (8) Base Sample
m8 =  lm(loghjypl ~ avexpr, data=data, which(baseco==1))

# Output table
stargazer(m1, m2, m3, m4, m5, m6, m7, m8, 
          title="OLS Regressions, Replication of AJR Table 2, \\ 
          Dependent Variable: Economic Performance, \\ Institutions Measure: 
          Average protection against expropriation risk, 1985-1995",
          align=TRUE, dep.var.labels=c("log GDP per capital in 1995",
                                       "log output per worker in 1988"),
          covariate.labels=c("Institutions","Latitude", "Asia dummy",
                             "Africa dummy","Other continent dummy"),
          column.labels=c("Whole World", "Base Sample", "Whole World",
                          "Whole World", "Base Sample", "Base Sample",
                          "Whole World", "Base Sample"),
          column.separate=c(1,2,3,4,5,6,7,8),
          notes.align=c("l"),
          omit.stat=c("LL", "ser","f"), no.space=TRUE, 
          font.size=c("scriptsize"),
          digits=2,
          column.sep.width=c("3pt"))

#############################################################################################
#REPLICATION OF TABLE 2 - OLS REGRESSIONS - OUR PAPER -- NEW MEASURE OF INSTITUTIONS (**)
#############################################################################################
# (1) Whole World - DV = logpgp95
m1 = lm(logpgp12 ~ currentinst, data=data)

# (2) Base Sample
m2 = lm(logpgp12 ~ currentinst, data=data, which(baseco==1))

# (3) Whole World
m3 = lm(logpgp12 ~ currentinst + lat_abst, data=data)

# (4) Whole World
m4 = lm(logpgp12 ~ currentinst + lat_abst + asia + africa + other, data=data)

# (5) Base Sample
m5 = lm(logpgp12 ~ currentinst + lat_abst, data=data, which(baseco==1))

# (6) Base Sample
m6 = lm(logpgp12 ~ currentinst + lat_abst + asia + africa + other, data=data, which(baseco==1))

# (7) Whole World DV = loghjypl
m7 =  lm(loghjypl ~ currentinst, data=data)

# (8) Base Sample
m8 =  lm(loghjypl ~ currentinst, data=data, which(baseco==1))

stargazer(m1, m2, m3, m4, m5, m6, m7, m8, 
          title="OLS Regressions, Replication of AJR Table 2, \\ 
          Dependent Variable: Economic Performance, \\ Institutions Measure: 
          IFC World Bank Ease of Doing Business Index, 2012",
          align=TRUE, dep.var.labels=c("log GDP per capital in 2012",
                                       "log output per worker in 1988"),
          covariate.labels=c("Institutions","Latitude", "Asia dummy",
                             "Africa dummy","Other continent dummy"),
          column.labels=c("Whole World", "Base Sample", "Whole World",
                          "Whole World", "Base Sample", "Base Sample",
                          "Whole World", "Base Sample"),
          column.separate=c(1,2,3,4,5,6,7,8),
          notes.align=c("l"),
          omit.stat=c("LL", "ser","f"), no.space=TRUE, 
          font.size=c("scriptsize"),
          digits=2,
          column.sep.width=c("3pt"))

#############################################################################################

#############################################################################################
#REPLICATION OF TABLE 3 - DETERMINANTS OF INSTITUTIONS
#############################################################################################

# PANEL A
# DV = avexpr 
# (1) - (10)

m1 = lm(avexpr ~ cons00a, data=data, which(baseco==1))
m2 = lm(avexpr ~ cons00a + lat_abst, data=data, which(baseco==1))
m3 = lm(avexpr ~ democ00a, data=data, which(baseco==1))
m4 = lm(avexpr ~ democ00a + lat_abst, data=data, which(baseco==1))
m5 = lm(avexpr ~ cons1 + indtime, data=data, which(baseco==1))
m6 = lm(avexpr ~ cons1 + indtime + lat_abst, data=data, which(baseco==1))
m7 = lm(avexpr ~ euro1900, data=data, which(baseco==1))
m8 = lm(avexpr ~ euro1900 + lat_abst, data=data, which(baseco==1))
m9 = lm(avexpr ~ logem4, data=data, which(baseco==1))
m10 = lm(avexpr ~ logem4 + lat_abst, data=data, which(baseco==1))

stargazer(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10,
          title="Determinants of Institutions, Replication of AJR Table 3, Panel A, \\ 
          Dependent Variable: Institutions, \\ Institutions Measure: 
          Average protection against expropriation risk, 1985-1995",
          align=TRUE, dep.var.labels=c("Average Protection Against Expropriation Risk, 1985-1995"),
          covariate.labels=c("Constraint on executive in 1900",
                             "Latitude",
                             "Democracy in 1900", 
                             "Constraint on executive in first year of independence",
                             "Years since Independence",
                             "European settlements in 1900",
                             "Log European settler mortality"),
          notes.align=c("l"),
          omit.stat=c("LL", "ser","f"), no.space=TRUE, 
          font.size=c("scriptsize"),
          digits=2,
          column.sep.width=c("1pt"))


# PANEL B
# DV = cons00a 
# (1) - (4)

euro1900.n = euro1900/100

m1 = lm(cons00a ~ euro1900.n, data=data, which(baseco==1))
m2 = lm(cons00a ~ euro1900.n + lat_abst, data=data, which(baseco==1))
m3 = lm(cons00a ~ logem4, data=data, which(baseco==1))
m4 = lm(cons00a ~ logem4 + lat_abst, data=data, which(baseco==1))

# DV = democ00a
# (5) - (8)

m5 = lm(democ00a ~ euro1900.n, data=data, which(baseco==1))
m6 = lm(democ00a ~ euro1900.n + lat_abst, data=data, which(baseco==1))
m7 = lm(democ00a ~ logem4, data=data, which(baseco==1))
m8 = lm(democ00a ~ logem4 + lat_abst, data=data, which(baseco==1))

# DV = euro1900
# (9) - (10)

m9 = lm(euro1900.n ~ logem4, data=data, which(baseco==1))
m10 = lm(euro1900.n ~ logem4 + lat_abst, data=data, which(baseco==1))

stargazer(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10,
          title="Determinants of Institutions, Replication of AJR Table 3, Panel B, \\ 
          Dependent Variable: Institutions, \\ Institutions Measures: 
          Constraint on executive in 1900, Democracy in 1900, European settlements in 1900",
          align=TRUE, dep.var.labels=c("Constraint on Executive in 1900", 
                                       "Democracy in 1900", 
                                       "European Settlements in 1900"),
          covariate.labels=c("European settlements in 1900",
                             "Latitude",
                             "Log European settler mortality"),
          notes.align=c("l"),
          omit.stat=c("LL", "ser","f"), no.space=TRUE, 
          font.size=c("scriptsize"),
          digits=2,
          column.sep.width=c("1pt"))

#############################################################################################
#REPLICATION OF TABLE 3 - DETERMINANTS OF INSTITUTIONS -- NEW MEASURE OF INSTITUTIONS (**)
#############################################################################################

# PANEL A
# DV = currentinst 
# (1) - (10)

m1 = lm(currentinst ~ cons00a, data=data, which(baseco==1))
m2 = lm(currentinst ~ cons00a + lat_abst, data=data, which(baseco==1))
m3 = lm(currentinst ~ democ00a, data=data, which(baseco==1))
m4 = lm(currentinst ~ democ00a + lat_abst, data=data, which(baseco==1))
m5 = lm(currentinst ~ cons1 + indtime, data=data, which(baseco==1))
m6 = lm(currentinst ~ cons1 + indtime + lat_abst, data=data, which(baseco==1))
m7 = lm(currentinst ~ euro1900, data=data, which(baseco==1))
m8 = lm(currentinst ~ euro1900 + lat_abst, data=data, which(baseco==1))
m9 = lm(currentinst ~ logem4, data=data, which(baseco==1))
m10 = lm(currentinst ~ logem4 + lat_abst, data=data, which(baseco==1))

stargazer(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10,
          title="Determinants of Institutions, Replication of AJR Table 3, Panel A, \\ 
          Dependent Variable: Institutions, \\ Institutions Measure: 
          IFC World Bank Ease of Doing Business Index, 2012",
          align=TRUE, dep.var.labels=c(" IFC World Bank Ease of Doing Business Index, 2012"),
          covariate.labels=c("Constraint on executive in 1900",
                             "Latitude",
                             "Democracy in 1900", 
                             "Constraint on executive in first year of independence",
                             "Years since Independence",
                             "European settlements in 1900",
                             "Log European settler mortality"),
          notes.align=c("l"),
          omit.stat=c("LL", "ser","f"), no.space=TRUE, 
          font.size=c("scriptsize"),
          digits=2,
          column.sep.width=c("1pt"))

# PANEL B
# DV = cons00a 
# (1) - (4)

euro1900.n = euro1900/100

m1 = lm(cons00a ~ euro1900.n, data=data, which(baseco==1))
m2 = lm(cons00a ~ euro1900.n + lat_abst, data=data, which(baseco==1))
m3 = lm(cons00a ~ logem4, data=data, which(baseco==1))
m4 = lm(cons00a ~ logem4 + lat_abst, data=data, which(baseco==1))

# DV = democ00a
# (5) - (8)

m5 = lm(democ00a ~ euro1900.n, data=data, which(baseco==1))
m6 = lm(democ00a ~ euro1900.n + lat_abst, data=data, which(baseco==1))
m7 = lm(democ00a ~ logem4, data=data, which(baseco==1))
m8 = lm(democ00a ~ logem4 + lat_abst, data=data, which(baseco==1))

# DV = euro1900
# (9) - (10)

m9 = lm(euro1900.n ~ logem4, data=data, which(baseco==1))
m10 = lm(euro1900.n ~ logem4 + lat_abst, data=data, which(baseco==1))

stargazer(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10,
          title="Determinants of Institutions, Replication of AJR Table 3, Panel B, \\ 
          Dependent Variable: Institutions, \\ Institutions Measures: 
          Constraint on executive in 1900, Democracy in 1900, European settlements in 1900",
          align=TRUE, dep.var.labels=c("Constraint on Executive in 1900", 
                                       "Democracy in 1900", 
                                       "European Settlements in 1900"),
          covariate.labels=c("European settlements in 1900",
                             "Latitude",
                             "Log European settler mortality"),
          notes.align=c("l"),
          omit.stat=c("LL", "ser","f"), no.space=TRUE, 
          font.size=c("scriptsize"),
          digits=2,
          column.sep.width=c("1pt"))


#############################################################################################
#REPLICATION OF TABLE 4 - IV REGRESSIONS OF LOG GDP PER CAPITA
#############################################################################################

# WRITE AN IV FUNCTION SO THAT WE CAN RUN THE REGRESSIONS MORE EASILY
#########################################################################
ivregress = function(second, first, data){
  s.names = all.vars(second)
  f.names  = all.vars(first)
  data.names = names(data)
  all.names = c(s.names,f.names)
  resp   = s.names[1]
  endog  = f.names[1]
  inst   = f.names[-1]
  explan = s.names[-1]
  exog   = explan[explan!=endog]
  exog.f = paste(exog,collapse="+")
  inst.f = paste(inst, collapse="+")
  RHS    = paste(exog.f, inst.f, sep="+")
  first.form = as.formula( paste(endog, "~", RHS))
  
  first.lm = lm(first.form, data)
  ftest    = linearHypothesis(first.lm, inst, rep(0,length(inst)))
  x.hat    = fitted(first.lm)
  data2    = cbind(data,x.hat)
  iname    = paste(endog,".hat",sep="")
  names(data2) = c(names(data), iname)
  data2.names = names(data2)
  RHS2 = paste(exog.f,iname,sep="+")
  second.form = as.formula(paste(resp, "~", RHS2))
  second.lm = lm(second.form, data2)
  Xmat  = data2[c(exog,endog)]
  Xmat2  = data2[c(exog,iname)]
  z = summary(second.lm)
  X     = as.matrix(cbind(1,Xmat))
  X2    = as.matrix(cbind(1,Xmat2))
  Y     = data[resp]
  fit   = X%*%second.lm$coefficients
  res   = Y - fit
  
  xPx     = t(X2)%*%X2
  xPx.inv = solve(xPx)
  z$cov.unscaled     = xPx.inv
  z$residuals        = res
  z$sigma            = sqrt(mean(res^2))
  varcovmat          = z$cov.unscaled*z$sigma
  coef               = z$coefficients[,1]
  IV.SE              = z$sigma*sqrt(diag(xPx.inv))
  t.iv               = coef/IV.SE
  p.val              = 2*(1-pnorm(abs(t.iv)))
  z$coefficients[,2] = IV.SE
  z$coefficients[,3] = t.iv
  z$coefficients[,4] = p.val
  
  result = list(summary(first.lm),z,ftest)
  names(result) = c("first", "second", "ftest")
  print("IV object successfully created. Use sum.iv() on object")
  print("to learn about your 2SLS Regression")
  return(invisible(result))
}
sum.iv = function(reg.iv, first=FALSE,
                  second=TRUE, ftest=FALSE) {
  x= rep(0,2)
  if(first==TRUE) x[1] = 1
  if(second==TRUE) x[2]= 2
  if(ftest==TRUE) x[3]= 3
  print(reg.iv[x])
}



#########################################################################
# PANEL A - TWO STAGE LEAST SQUARES
#########################################################################
# DV = logpgp95

# (1) Base sample 
data.base = subset(data,data$baseco==1)
# m1 = ivregress(logpgp95 ~ avexpr, avexpr ~ logem4, data.base)
# sum.iv(m1)
avexpr.p = predict(lm(avexpr ~ logem4, data.base))
m1 = lm(logpgp95 ~ avexpr.p, data.base) #alternative way to estimate 2SLS using lm function

# (2) Base sample
# m2 = ivregress(logpgp95 ~ avexpr + lat_abst, avexpr ~ logem4 + lat_abst, data.base)
# sum.iv(m2)
avexpr.p = predict(lm(avexpr ~ logem4 + lat_abst, data.base))
m2 = lm(logpgp95 ~ avexpr.p + lat_abst, data.base)

# (3) Base sample without Neo-Europes
data.base.1 = subset(data,data$baseco==1 & rich4!=1)
# m3 = ivregress(logpgp95 ~ avexpr, avexpr ~ logem4, data.base.1)
# sum.iv(m3)
avexpr.p = predict(lm(avexpr ~ logem4, data.base.1))
m3 = lm(logpgp95 ~ avexpr.p, data.base.1)

# (4) Base sample without Neo-Europes 
# m4 = ivregress(logpgp95 ~ avexpr + lat_abst, avexpr ~ logem4 + lat_abst, data.base.1)
# sum.iv(m4)
avexpr.p = predict(lm(avexpr ~ logem4 + lat_abst, data.base.1))
m4 = lm(logpgp95 ~ avexpr.p  + lat_abst, data.base.1)
  
# (5) Base sample without Africa
data.base.2 = subset(data,data$baseco==1 & africa!=1)
# m5 = ivregress(logpgp95 ~ avexpr, avexpr ~ logem4, data.base.2)
# sum.iv(m5)
avexpr.p  = predict(lm(avexpr ~ logem4, data.base.2))
m5 = lm(logpgp95 ~ avexpr.p , data.base.2)

# (6) Base sample without Africa
# m6 = ivregress(logpgp95 ~ avexpr + lat_abst, avexpr ~ logem4 + lat_abst, data.base.2)
# sum.iv(m6)
avexpr.p  = predict(lm(avexpr ~ logem4 + lat_abst, data.base.2))
m6 = lm(logpgp95 ~ avexpr.p  + lat_abst, data.base.2)

# (7) Base sample with continent dummies
# m7 = ivregress(logpgp95 ~ avexpr + asia + africa + other, avexpr ~ logem4 + asia + africa + other, data.base)
# sum.iv(m7)
avexpr.p  = predict(lm(avexpr ~ logem4 + asia + africa + other, data.base))
m7 = lm(logpgp95 ~ avexpr.p  + asia + africa + other, data.base)
  
# (8) Base sample with continent dummies
# m8 = ivregress(logpgp95 ~ avexpr + lat_abst + asia + africa + other, avexpr ~ logem4 + lat_abst + asia + africa + other, data.base)
# sum.iv(m8)
avexpr.p  = predict(lm(avexpr ~ logem4 + lat_abst + asia + africa + other, data.base))
m8 = lm(logpgp95 ~ avexpr.p  + lat_abst + asia + africa + other, data.base)

# (9) Base sample, DV = loghjypl
# m9 = ivregress(loghjypl ~ avexpr, avexpr ~ logem4, data.base)
# sum.iv(m9)
avexpr.p  = predict(lm(avexpr ~ logem4, data.base))
m9 = lm(loghjypl ~ avexpr.p , data.base)

#Output table in Latex
stargazer(m1, m2, m3, m4, m5, m6, m7, m8, m9, 
          title="IV Regressions of Log GDP per Capita, Replication of AJR Table 4, Panel A, \\ 
          Dependent Variable: Economic Performance, \\ Institutions Measures: 
          Average protection against expropriation risk, 1985-1995",
          align=TRUE, dep.var.labels=c("Log GDP per Capita (PPP) 1995", "Log output per worker, 1988"),
          covariate.labels=c("Average protection against expropriation risk, 1985-1995", 
                             "Latitude", 
                             "Asia dummy",
                             "Africa dummy",
                             "Other continent dummy"),
          notes.align=c("l"),
          omit.stat=c("LL", "ser","f"), no.space=TRUE, 
          font.size=c("scriptsize"),
          digits=2,
          column.sep.width=c("1pt"))


# PANEL B - FIRST STAGE FOR AVERAGE PROTECTION AGAINST EXPROPRIATION RISK 1985-1995
###################################################################################
# DV = avexpr

# (1) Base sample
m1 = lm(avexpr ~ logem4, data.base)

# (2) Base sample
m2 = lm(avexpr ~ logem4 + lat_abst, data.base)

# (3) Base sample without Neo-Europes
m3 = lm(avexpr ~ logem4, data.base.1)

# (4) Base sample without Neo-Europes 
m4 = lm(avexpr ~ logem4 + lat_abst, data.base.1)

# (5) Base sample without Africa
m5 = lm(avexpr ~ logem4, data.base.2)
  
# (6) Base sample without Africa
m6 = lm(avexpr ~ logem4 + lat_abst, data.base.2)

# (7) Base sample with continent dummies
m7 = lm(avexpr ~ logem4 + asia + africa + other, data.base)

# (8) Base sample with continent dummies
m8 = lm(avexpr ~ logem4 + lat_abst + asia + africa + other, data.base)
  
# (9) Base sample
m9 = lm(avexpr ~ logem4, data.base)

stargazer(m1, m2, m3, m4, m5, m6, m7, m8, m9, 
          title="First Stage IV Regressions of Average protection against expropriation risk in 1985-1995,
                Replication of AJR Table 4, Panel B, \\ 
          Dependent Variable: Average protection against expropriation risk in 1985-1995,
          \\ Instrument for Institutions: 
          Log European Settler Mortality",
          align=TRUE, dep.var.labels=c("Average protection against expropriation risk, 1985-1995"),
          covariate.labels=c("Log European Settler Mortality", 
                             "Latitude", 
                             "Asia dummy",
                             "Africa dummy",
                             "Other continent dummy"),
          notes.align=c("l"),
          omit.stat=c("LL", "ser","f"), no.space=TRUE, 
          font.size=c("scriptsize"),
          digits=2,
          column.sep.width=c("1pt"))

# PANEL C - OLS
##################################################################################
# DV = logpgp95

# (1) Base sample 
m1 = lm(logpgp95 ~ avexpr, data=data, which(baseco==1))

# (2) Base sample
m2 = lm(logpgp95 ~ avexpr, data=data, which(baseco==1))

# (3) Base sample without Neo-Europes
m3 = lm(logpgp95 ~ avexpr, data=data, which(baseco==1 & rich4!=1))

# (4) Base sample without Neo-Europes
m4 = lm(logpgp95 ~ avexpr, data=data, which(baseco==1 & rich4!=1))

# (5) Base sample without Africa
m5 = lm(logpgp95 ~ avexpr, data=data, which(baseco==1 & africa!=1))

# (6) Base sample without Africa
m6 = lm(logpgp95 ~ avexpr, data=data, which(baseco==1 & africa!=1))

# (7) Base sample with continent dummies
m7 = lm(logpgp95 ~ avexpr + africa + asia + other, data=data, which(baseco==1))

# (8) Base sample with continent dummies
m8 = lm(logpgp95 ~ avexpr + africa + asia + other, data=data, which(baseco==1))

# (9) Base sample, DV = loghjypl
m9 = lm(loghjypl ~ avexpr + africa + asia + other, data=data, which(baseco==1))

stargazer(m1, m2, m3, m4, m5, m6, m7, m8, m9, 
          title="OLS Regressions of Economic Performance,
                Replication of AJR Table 4, Panel C, \\ 
          Dependent Variable: Economic Performance (Log GDP per Capita 1995 and Log output per worker 1988),
          \\ Institutions Measure: 
          Average protection against expropriation risk, 1985-1995",
          align=TRUE, dep.var.labels=c("Log GDP per Capita (PPP) 1995", "Log output per worker, 1988"),
          covariate.labels=c("Average protection against expropriation risk, 1985-1995", 
                             "Asia dummy",
                             "Africa dummy",
                             "Other continent dummy"),
          notes.align=c("l"),
          omit.stat=c("LL", "ser","f"), no.space=TRUE, 
          font.size=c("scriptsize"),
          digits=2,
          column.sep.width=c("1pt"))



#############################################################################################
#REPLICATION OF TABLE 4 - IV REGRESSIONS OF LOG GDP PER CAPITA - MEASURE OF INSTITUTIONS (**)
#############################################################################################

#########################################################################
# PANEL A - TWO STAGE LEAST SQUARES
#########################################################################
# DV = logpgp12

# (1) Base sample 
data.base = subset(data,data$baseco==1)
currentinst.p = predict(lm(currentinst ~ logem4, data.base))
m1 = lm(logpgp12 ~ currentinst.p, data.base) #alternative way to estimate 2SLS using lm function

# (2) Base sample
currentinst.p = predict(lm(currentinst ~ logem4 + lat_abst, data.base))
m2 = lm(logpgp12 ~ currentinst.p + lat_abst, data.base)

# (3) Base sample without Neo-Europes
data.base.1 = subset(data,data$baseco==1 & rich4!=1)
currentinst.p = predict(lm(currentinst ~ logem4, data.base.1))
m3 = lm(logpgp12 ~ currentinst.p, data.base.1)

# (4) Base sample without Neo-Europes 
currentinst.p = predict(lm(currentinst ~ logem4 + lat_abst, data.base.1))
m4 = lm(logpgp12 ~ currentinst.p  + lat_abst, data.base.1)

# (5) Base sample without Africa
data.base.2 = subset(data,data$baseco==1 & africa!=1)
currentinst.p  = predict(lm(currentinst ~ logem4, data.base.2))
m5 = lm(logpgp12 ~ currentinst.p , data.base.2)

# (6) Base sample without Africa
currentinst.p  = predict(lm(currentinst ~ logem4 + lat_abst, data.base.2))
m6 = lm(logpgp12 ~ currentinst.p  + lat_abst, data.base.2)

# (7) Base sample with continent dummies
currentinst.p  = predict(lm(currentinst ~ logem4 + asia + africa + other, data.base))
m7 = lm(logpgp12 ~ currentinst.p  + asia + africa + other, data.base)

# (8) Base sample with continent dummies
currentinst.p  = predict(lm(currentinst ~ logem4 + lat_abst + asia + africa + other, data.base))
m8 = lm(logpgp12 ~ currentinst.p  + lat_abst + asia + africa + other, data.base)

# (9) Base sample, DV = loghjypl
currentinst.p  = predict(lm(currentinst ~ logem4, data.base))
m9 = lm(loghjypl ~ currentinst.p , data.base)

#Output table in Latex
stargazer(m1, m2, m3, m4, m5, m6, m7, m8, m9, 
          title="IV Regressions of Log GDP per Capita, Replication of AJR Table 4, Panel A, \\ 
          Dependent Variable: Economic Performance, \\ Institutions Measures: 
          IFC Ease of Doing Business Index, 2012",
          align=TRUE, dep.var.labels=c("Log GDP per Capita (PPP) 2012", "Log output per worker, 1988"),
          covariate.labels=c("IFC Ease of Doing Business Index, 2012", 
                             "Latitude", 
                             "Asia dummy",
                             "Africa dummy",
                             "Other continent dummy"),
          notes.align=c("l"),
          omit.stat=c("LL", "ser","f"), no.space=TRUE, 
          font.size=c("scriptsize"),
          digits=2,
          column.sep.width=c("1pt"))


# PANEL B - FIRST STAGE FOR AVERAGE PROTECTION AGAINST EXPROPRIATION RISK 1985-1995
###################################################################################
# DV = avexpr

# (1) Base sample
m1 = lm(currentinst ~ logem4, data.base)

# (2) Base sample
m2 = lm(currentinst ~ logem4 + lat_abst, data.base)

# (3) Base sample without Neo-Europes
m3 = lm(currentinst ~ logem4, data.base.1)

# (4) Base sample without Neo-Europes 
m4 = lm(currentinst ~ logem4 + lat_abst, data.base.1)

# (5) Base sample without Africa
m5 = lm(currentinst ~ logem4, data.base.2)

# (6) Base sample without Africa
m6 = lm(currentinst ~ logem4 + lat_abst, data.base.2)

# (7) Base sample with continent dummies
m7 = lm(currentinst ~ logem4 + asia + africa + other, data.base)

# (8) Base sample with continent dummies
m8 = lm(currentinst ~ logem4 + lat_abst + asia + africa + other, data.base)

# (9) Base sample
m9 = lm(currentinst ~ logem4, data.base)

stargazer(m1, m2, m3, m4, m5, m6, m7, m8, m9, 
          title="First Stage IV Regressions of IFC Ease of Doing Business Index 2012,
          Replication of AJR Table 4, Panel B, \\ 
          Dependent Variable: IFC Ease of Doing Business Index 2012,
          \\ Instrument for Institutions: 
          Log European Settler Mortality",
          align=TRUE, dep.var.labels=c("IFC Ease of Doing Business Index 2012"),
          covariate.labels=c("Log European Settler Mortality", 
                             "Latitude", 
                             "Asia dummy",
                             "Africa dummy",
                             "Other continent dummy"),
          notes.align=c("l"),
          omit.stat=c("LL", "ser","f"), no.space=TRUE, 
          font.size=c("scriptsize"),
          digits=2,
          column.sep.width=c("1pt"))

# PANEL C - OLS
##################################################################################
# DV = logpgp12

# (1) Base sample 
m1 = lm(logpgp12 ~ currentinst, data=data, which(baseco==1))

# (2) Base sample
m2 = lm(logpgp12 ~ currentinst, data=data, which(baseco==1))

# (3) Base sample without Neo-Europes
m3 = lm(logpgp12 ~ currentinst, data=data, which(baseco==1 & rich4!=1))

# (4) Base sample without Neo-Europes
m4 = lm(logpgp12 ~ currentinst, data=data, which(baseco==1 & rich4!=1))

# (5) Base sample without Africa
m5 = lm(logpgp12 ~ currentinst, data=data, which(baseco==1 & africa!=1))

# (6) Base sample without Africa
m6 = lm(logpgp12 ~ currentinst, data=data, which(baseco==1 & africa!=1))

# (7) Base sample with continent dummies
m7 = lm(logpgp12 ~ currentinst + africa + asia + other, data=data, which(baseco==1))

# (8) Base sample with continent dummies
m8 = lm(logpgp12 ~ currentinst + africa + asia + other, data=data, which(baseco==1))

# (9) Base sample, DV = loghjypl
m9 = lm(loghjypl ~ currentinst + africa + asia + other, data=data, which(baseco==1))

stargazer(m1, m2, m3, m4, m5, m6, m7, m8, m9, 
          title="OLS Regressions of Economic Performance,
                Replication of AJR Table 4, Panel C, \\ 
          Dependent Variable: Economic Performance (Log GDP per Capita 2012 and Log output per worker 1988),
          \\ Institutions Measure: 
          IFC Ease of Doing Business Index 2012",
          align=TRUE, dep.var.labels=c("Log GDP per Capita", "Log output per worker"),
          covariate.labels=c("Log European Settler Mortality", 
                             "Asia dummy",
                             "Africa dummy",
                             "Other continent dummy"),
          notes.align=c("l"),
          omit.stat=c("LL", "ser","f"), no.space=TRUE, 
          font.size=c("scriptsize"),
          digits=2,
          column.sep.width=c("1pt"))


