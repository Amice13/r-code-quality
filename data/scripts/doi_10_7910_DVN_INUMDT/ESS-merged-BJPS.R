
## INEQUALITY, IMMIGRANTS AND SELECTIVE SOLIDARITY - ESS

rm(list=ls(all=TRUE))
detach(ESS.merged0812)
detach(subset.OECD)

library(haven)
ESS.merged0812 <- read_spss("C:/Users/gmagni/OneDrive/Dissertation/Diss-LATEST/Data/ESS/ESS-merged-contextual-2008-12/ESSMDW4e4v3_F1.sav")

library(foreign) 
Rueda <- read.dta("C:/Users/gmagni/OneDrive/Dissertation/Diss-LATEST/Data/ESS/Rueda/Data.dta")
  

#Add column of REGIONAL variables from Rueda dataset to ESS dataset
ESS.merged0812$Rgini = Rueda[match(ESS.merged0812$idno, Rueda$idno),"Rgini"] # gini
ESS.merged0812$Rforeign = Rueda[match(ESS.merged0812$idno, Rueda$idno),"Rforeign"] # share of foreigners
ESS.merged0812$Ruerate = Rueda[match(ESS.merged0812$idno, Rueda$idno),"Ruerate"] # unemployment rate
ESS.merged0812$Rtech = Rueda[match(ESS.merged0812$idno, Rueda$idno),"Rtech"] # tech specialization
ESS.merged0812$Rgdp = Rueda[match(ESS.merged0812$idno, Rueda$idno),"Rgdp"] # gdp per capita
ESS.merged0812$Rpopdens = Rueda[match(ESS.merged0812$idno, Rueda$idno),"Rpopdens"] # population density

ESS.merged0812$incdist = Rueda[match(ESS.merged0812$idno, Rueda$idno),"incdist"] # income distance
ESS.merged0812$rincd = Rueda[match(ESS.merged0812$idno, Rueda$idno),"rincd"] # 

ESS.merged0812$mngr = Rueda[match(ESS.merged0812$idno, Rueda$idno),"mngr"] # 
ESS.merged0812$lowsup = Rueda[match(ESS.merged0812$idno, Rueda$idno),"lowsup"] # 
ESS.merged0812$smempl = Rueda[match(ESS.merged0812$idno, Rueda$idno),"smempl"] # 
ESS.merged0812$whcol = Rueda[match(ESS.merged0812$idno, Rueda$idno),"whcol"] # 
ESS.merged0812$blcol = Rueda[match(ESS.merged0812$idno, Rueda$idno),"blcol"] # 
ESS.merged0812$noclass = Rueda[match(ESS.merged0812$idno, Rueda$idno),"noclass"] # 
ESS.merged0812$skillshg = Rueda[match(ESS.merged0812$idno, Rueda$idno),"skillshg"] # 
ESS.merged0812$skillslg = Rueda[match(ESS.merged0812$idno, Rueda$idno),"skillslg"] # 
ESS.merged0812$skillssp = Rueda[match(ESS.merged0812$idno, Rueda$idno),"skillssp"] # 



##


library(lme4)
library(arm)
library(stargazer)
library(interplot)
library(plyr)
library(ordinal)
library(stargazer)
library(margins)
library(prediction)


####

#Subset to include only countries in which I have GINI values (OECD) at national level
  #and welfare module (ESS 2008)
subset.OECD <- subset(ESS.merged0812, cntry == "BE" |cntry == "CH" |cntry == "CZ" |
                        cntry == "DE" |cntry == "DK" |cntry == "ES" |cntry == "FI" |cntry == "FR" |
                        cntry == "GB" |cntry == "GR" |cntry == "HU" |cntry == "IE" |cntry == "NL" |
                        cntry == "NO" |cntry == "PL" |cntry == "PT" |cntry == "SE" |
                        cntry == "TR" |cntry == "IL" |cntry == "EE" |cntry == "SI")


attach(subset.OECD)
n = nrow(subset.OECD)



#################
### Variables ###
#################


#########
## DVs
#########


#Help people in need
#'Insufficient benefits in country to help people in real need'
table(insfben)
help.poor = rep(NA,n)
help.poor[insfben=="5"]=1
help.poor[insfben=="4"]=2
help.poor[insfben=="3"]=3
help.poor[insfben=="2"]=4
help.poor[insfben=="1"]=5
table(help.poor)

help.poor.f <- factor(help.poor, labels = c("0", "1", "2", "3", "4"))
table(help.poor.f)


#Social benefits/services to immigrants 
#(higher values mean getting benefits sooner)
table(imsclbn) # When should immigrants obtain rights to social benefits/services
immigr.benef = rep(NA, n)
immigr.benef[imsclbn=="5"]=1
immigr.benef[imsclbn=="4"]=2
immigr.benef[imsclbn=="3"]=3
immigr.benef[imsclbn=="2"]=4
immigr.benef[imsclbn=="1"]=5
table(immigr.benef)

immigr.benef.f <- factor(immigr.benef, labels = c("0", "1", "2", "3", "4"))
table(immigr.benef.f)

imm.ben.never.citiz <-as.numeric(immigr.benef == "1" | immigr.benef == "2")
table(imm.ben.never.citiz) 
#1=never or citizenship



#########
## IVs
########

## Main - Individual

#Income
table(hinctnta) #Household's total net income, all sources (deciles: from lowest to highest)
income=rep(NA,n)
income[hinctnta=="1"]=0
income[hinctnta=="2"]=1
income[hinctnta=="3"]=2
income[hinctnta=="4"]=3
income[hinctnta=="5"]=4
income[hinctnta=="6"]=5
income[hinctnta=="7"]=6
income[hinctnta=="8"]=7
income[hinctnta=="9"]=8
income[hinctnta=="10"]=9
table(income)


#Income insecurity (perceptions)
table(hincfel) #Feeling about household's income nowadays
inc.safe=rep(NA,n)
inc.safe[hincfel=="4"]=0
inc.safe[hincfel=="3"]=1
inc.safe[hincfel=="2"]=2
inc.safe[hincfel=="1"]=3
table(inc.safe) #higher numbers indicate economic security (reversed in the original)


#Perceptions of poverty spread
table(nmnybsc) #Of every 100 working age how many not money for basic necessities
nomoney.100 = rep(NA,n)
nomoney.100[nmnybsc=="1"]=0
nomoney.100[nmnybsc=="2"]=1
nomoney.100[nmnybsc=="3"]=2
nomoney.100[nmnybsc=="4"]=3
nomoney.100[nmnybsc=="5"]=4
nomoney.100[nmnybsc=="6"]=5
nomoney.100[nmnybsc=="7"]=6
nomoney.100[nmnybsc=="8"]=7
nomoney.100[nmnybsc=="9"]=8
nomoney.100[nmnybsc=="10"]=9
nomoney.100[nmnybsc=="11"]=10
table(nomoney.100) #higher values mean R believes more people do NOT have enough money (by 4: 1=0-4; 11=50+)

#Perceptions of number of immigrants
table(brnocnt) #Of every 100 working age how many born outside country
immigr.100 = rep(NA,n)
immigr.100[brnocnt=="1"]=0
immigr.100[brnocnt=="2"]=1
immigr.100[brnocnt=="3"]=2
immigr.100[brnocnt=="4"]=3
immigr.100[brnocnt=="5"]=4
immigr.100[brnocnt=="6"]=5
immigr.100[brnocnt=="7"]=6
immigr.100[brnocnt=="8"]=7
immigr.100[brnocnt=="9"]=8
immigr.100[brnocnt=="10"]=9
immigr.100[brnocnt=="11"]=10
table(immigr.100) #higher values mean R believes there are more immigrants


### DESERVINGNESS

#Attitudes towards migrants (sort of ethnic stereotypes / deservingness perceptions)
table(imrccon) #higher numbers mean they receive more than they contribute, i.e. they are free-riders
imm.freerid = rep(NA,n)
imm.freerid[imrccon=="10"]=0
imm.freerid[imrccon=="9"]=1
imm.freerid[imrccon=="8"]=2
imm.freerid[imrccon=="7"]=3
imm.freerid[imrccon=="6"]=4
imm.freerid[imrccon=="5"]=5
imm.freerid[imrccon=="4"]=6
imm.freerid[imrccon=="3"]=7
imm.freerid[imrccon=="2"]=8
imm.freerid[imrccon=="1"]=9
imm.freerid[imrccon=="0"]=10
table(imm.freerid) #originally reversed


#Poor would deserve mnore
#"Many with very low incomes get less benefit than legally entitled to"
table(lbenent)
poor.freer = rep(NA,n)
poor.freer[lbenent=="1"]=1
poor.freer[lbenent=="2"]=2
poor.freer[lbenent=="3"]=3
poor.freer[lbenent=="4"]=4
poor.freer[lbenent=="5"]=5
table(poor.freer)



## Main - Contextual

#Regional variables - Rescaling

table(Rpopdens)
Rpopdens<-Rpopdens/100
table(Rpopdens)

table(Rgdp)
Rgdp<-Rgdp/1000
table(Rgdp)

table(Ruerate)
Ruerate<-Ruerate/100
table(Ruerate)


#Inequality 
#Gini 2005 - country level
table(c_giaftot_2005) #Gini coeff after taxes and transfer total population
  #data not available for: Bulgaria, Cyprus, Latvia, HR-Croatia, Romania, Russia, Ukraine (not OECD countries)
inequality <- c_giaftot_2005
table(inequality)


#Ethnic context
#Percentage of foreigners
table(c_popfor_2008) #Total number of foreigners (data not available for: Israel, Ukraine, Russia)
table(c_popsz_2008) #population size
immigr.prop <- (c_popfor_2008/c_popsz_2008)
table(immigr.prop)
immigr.perc <- immigr.prop*100
table(immigr.perc)


#GDP

#GDP per capita at current prices, US Dollars 2008
table(c_gdppc_2008)
gdp_pc_08<-c_gdppc_2008/1000
table(gdp_pc_08)

gdp_pc_07<-c_gdppc_2007/1000
gdp_pc_05<-c_gdppc_2005/1000
table(gdp_pc_05)
table(c_gdppc_2005)


## Controls

#Education
table(edulvla) #Highest level of education (harmonized among countries) 
educ=rep(NA,n)
educ[edulvla=="1"]=0
educ[edulvla=="2"]=1
educ[edulvla=="3"]=2
educ[edulvla=="4"]=3
educ[edulvla=="5"]=4
table(educ)

#Political ideology: left or right scale 
table(lrscale) #Placement on left right scale: 0=left, 10=right
right=rep(NA,n)
right[lrscale=="0"]=0
right[lrscale=="1"]=1
right[lrscale=="2"]=2
right[lrscale=="3"]=3
right[lrscale=="4"]=4
right[lrscale=="5"]=5
right[lrscale=="6"]=6
right[lrscale=="7"]=7
right[lrscale=="8"]=8
right[lrscale=="9"]=9
right[lrscale=="10"]=10
table(right)

#Currently unemployed (question about last 7 days)
curr.unempl.tot=rep(NA,n)
curr.unempl.tot <- as.numeric(uempla=="1" | uempli=="1")
table(curr.unempl.tot)

#Religiousness
table(rlgdgr) #How religious are you: 0=not at all; 10=very
religious=rep(NA,n)
religious[rlgdgr=="0"]=0
religious[rlgdgr=="1"]=1
religious[rlgdgr=="2"]=2
religious[rlgdgr=="3"]=3
religious[rlgdgr=="4"]=4
religious[rlgdgr=="5"]=5
religious[rlgdgr=="6"]=6
religious[rlgdgr=="7"]=7
religious[rlgdgr=="8"]=8
religious[rlgdgr=="9"]=9
religious[rlgdgr=="10"]=10
table(religious)

#Church attendance: 0=never; 6=every day (originally reversed)
table(rlgatnd)
church.att=rep(NA,n)
church.att[rlgatnd=="7"]=0
church.att[rlgatnd=="6"]=1
church.att[rlgatnd=="5"]=2
church.att[rlgatnd=="4"]=3
church.att[rlgatnd=="3"]=4
church.att[rlgatnd=="2"]=5
church.att[rlgatnd=="1"]=6
  
#Gender
table(gndr)
female=rep(NA,n) #0=male, 1=female (originally male=1, female=2)
female[gndr=="1"]=0
female[gndr=="2"]=1
table(female)

#Citizen
table(ctzcntr) #Citizen of country
citizen=rep(NA,n) #1=yes
citizen[ctzcntr=="2"]=0
citizen[ctzcntr=="1"]=1
table(citizen) 

#Age
table(agea)
agea[agea=="999"]=NA
table(agea)

#Age squared
age.2 = agea*agea

#Union member
table(mbtru) #union member (no / yes previously / yes currently)
union=rep(NA,n)
union[mbtru=="3"]=0
union[mbtru=="2"]=1
union[mbtru=="1"]=1
table(union)

#Income differences in society
table(smdfslv) #For fair society, differences in standard of living should be small
incdiff.eval = rep(NA,n)
incdiff.eval[smdfslv=="5"]=0
incdiff.eval[smdfslv=="4"]=1
incdiff.eval[smdfslv=="3"]=2
incdiff.eval[smdfslv=="2"]=3
incdiff.eval[smdfslv=="1"]=4
table(incdiff.eval) #higher values agree, i.e. differences should be smaller for fair society (originally reversed)

#Household size
table(hhmmb)
household=rep(NA,n)
household[hhmmb=="1"]=1
household[hhmmb=="2"]=2
household[hhmmb=="3"]=3
household[hhmmb=="4"]=4
household[hhmmb=="5"]=5
household[hhmmb=="6"]=6
household[hhmmb=="7"]=7
household[hhmmb=="8"]=7
household[hhmmb=="9"]=7
household[hhmmb=="10"]=7
household[hhmmb=="11"]=7
household[hhmmb=="12"]=7
household[hhmmb=="13"]=7
household[hhmmb=="14"]=7
household[hhmmb=="15"]=7
household[hhmmb=="16"]=7
table(household)


#Immigration opinions

#Immigration bad or good for country's economy
table(imbgeco)
imm.good=rep(NA,n)
imm.good[imbgeco=="0"]=0
imm.good[imbgeco=="1"]=1
imm.good[imbgeco=="2"]=2
imm.good[imbgeco=="3"]=3
imm.good[imbgeco=="4"]=4
imm.good[imbgeco=="5"]=5
imm.good[imbgeco=="6"]=6
imm.good[imbgeco=="7"]=7
imm.good[imbgeco=="8"]=8
imm.good[imbgeco=="9"]=9
imm.good[imbgeco=="10"]=10
table(imm.good)

#Immigrants enrich cultural life
table(imueclt)
imm.good.cult=rep(NA,n)
imm.good.cult[imueclt=="0"]=0
imm.good.cult[imueclt=="1"]=1
imm.good.cult[imueclt=="2"]=2
imm.good.cult[imueclt=="3"]=3
imm.good.cult[imueclt=="4"]=4
imm.good.cult[imueclt=="5"]=5
imm.good.cult[imueclt=="6"]=6
imm.good.cult[imueclt=="7"]=7
imm.good.cult[imueclt=="8"]=8
imm.good.cult[imueclt=="9"]=9
imm.good.cult[imueclt=="10"]=10
table(imm.good.cult)


welfare.type=rep(NA,n)
welfare.type[cntry=="NO" | cntry=="FI"]="socialdem"
welfare.type[cntry=="BE" | cntry=="ES" | cntry=="FR" | cntry=="GR" | cntry=="PT" | cntry=="TR"]="conservat"
welfare.type[cntry=="CH" | cntry=="IE" | cntry=="IL"]="liberal"
welfare.type[cntry=="CZ" | cntry=="EE" | cntry=="HU" | cntry=="PL" | cntry=="SI"]="postcomm"
table(welfare.type)




##############
###ANALYSIS###
##############



#Recode refusal, don't know, etc. as missing

for(v.tmp in c("gvhlthc", "gvslvol", "gvslvue",
               "gvcldcr", "gvpdlwk", "ditxssp",
               "gvjbevn", "insfben", "gincdif", "sbprvpv", "sbeqsoc", "smdfslv",
               "lbenent", "dfincac", "earnueb", "earnpen", "txearn", 
               "imrccon", "uentrjb", "bennent", "prtsick",
               "imsmetn", "imdfetn", "impcntr",
               "imbgeco", "imueclt", "imwbcnt", "lknemny",
               "lkuemp", "lklpwcf", "lknhlcn", "slvpens", "slvuemp")){
  tmp <- subset.OECD[,v.tmp]
  subset.OECD[(tmp==7 | tmp==8 | tmp==9)&!is.na(tmp),v.tmp] <- NA
}
for(v.tmp in c("gvhlthc", "gvslvol", "gvslvue",
               "gvcldcr", "gvpdlwk", "ditxssp",
               "gvjbevn",  "insfben", "gincdif", "sbprvpv", "sbeqsoc", "smdfslv",
               "lbenent", "dfincac", "earnueb", "earnpen", "txearn",
               "imrccon", "uentrjb", "bennent","prtsick",
               "imsmetn", "imdfetn", "impcntr",
               "imbgeco", "imueclt", "imwbcnt", "lknemny",
               "lkuemp", "lklpwcf", "lknhlcn", "slvpens", "slvuemp")){
  tmp <- subset.OECD[,v.tmp]
  subset.OECD[(tmp==77 | tmp==88 | tmp==99)&!is.na(tmp),v.tmp] <- NA
}




####################################################
## MODELS IN PAPER  ##
####################################################


## INEQUALITY ON HELPING THE POOR

# Table 1 in paper

## ATTENTION!! ##
## In the paper are models 1, 3, 4 (2 in the appendix)

#1) Individual socio-demographoc and Regional variables 
mm.hpoor.a <- lmer(help.poor ~ Rgini + income + 
                       female + agea + educ + right + church.att + 
                       Rgdp + Ruerate + Rforeign + Rpopdens +
                       (1| cntry))
summary(mm.hpoor.a)


### Predicted values
summary(Rgini)
prediction(mm.hpoor.a, at = list(Rgini = c(0.223, 0.411)))
prediction(mm.hpoor.a, at = list(income = c(0, 9)))
prediction(mm.hpoor.a, at = list(right = c(0, 9)))
prediction(mm.hpoor.a, at = list(educ = c(0, 4)))
prediction(mm.hpoor.a, at = list(agea = c(15, 98)))

#2) Further individual controls
mm.hpoor.b <- lmer(help.poor ~ Rgini + income +  
                       female + agea + age.2 + educ + right + 
                       union + church.att + household + curr.unempl.tot + inc.safe + 
                       Rgdp + Ruerate + Rforeign + Rpopdens +
                       (1| cntry))
summary(mm.hpoor.b)


#3) Individual perceptions
mm.hpoor.c <- lmer(help.poor ~ Rgini + income +  
                       female + agea + age.2 + educ + right + 
                       union + church.att + household + curr.unempl.tot + inc.safe + 
                       incdiff.eval + nomoney.100 + immigr.100 + poor.freer +
                       Rgdp + Ruerate + Rforeign + Rpopdens +
                       (1| cntry))
summary(mm.hpoor.c)


#4) National controls
mm.hpoor.d <- lmer(help.poor ~ Rgini + income + 
                       female + agea + age.2 + educ + right + 
                       union + church.att + household + curr.unempl.tot + inc.safe + 
                       incdiff.eval + nomoney.100 + immigr.100 + poor.freer +
                       Rgdp + Ruerate + Rforeign + Rpopdens +
                       gdp_pc_08 + c_soexgdp_2007 + c_unraall_2008 + immigr.perc + 
                       (1| cntry))
summary(mm.hpoor.d)


## (for footnote in paper) ##
## test of impact of inquality on support for poor conditional on social expenditure

#Interaction inequality*social expenditure 
mm.hpoor.evi <- lmer(help.poor ~ Rgini + Rgini*c_soexgdp_2007 + income + 
                     female + agea + educ + right + 
                     union + religious + household + curr.unempl.tot + inc.safe + 
                     incdiff.eval + nomoney.100 + immigr.100 + poor.freer +
                     Rgdp + Ruerate + Rforeign + Rpopdens +
                     gdp_pc_08 + c_soexgdp_2007 + c_unraall_2008 + immigr.perc + 
                     (1| cntry))
summary(mm.hpoor.evi)
interplot(m = mm.hpoor.evi, var1 = "Rgini", var2 = "c_soexgdp_2007") +
  geom_hline(yintercept = 0, linetype = "dashed")

table(c_soexgdp_2007)
ddply(subset.OECD, .(cntry), summarize,  Soc.Exp=mean(c_soexgdp_2007, na.rm=TRUE), Rgini=mean(Rgini, na.rm=TRUE))


### (end of tests) ####


stargazer(mm.hpoor.a, mm.hpoor.b, mm.hpoor.c, mm.hpoor.d,  
          type="html", star.cutoffs = c(0.05, 0.01, 0.001), no.space = TRUE,
          out="mod.htm")

##
# Appedix: Table A4
# Additional national control: substracting the Gini market income minus the Gini disposable income 

progr.ws.2010 <- c_gibetot_2010 - c_giaftot_2010

mm.hpoor.app <- lmer(help.poor ~ Rgini + income + citizen +
                     female + agea + age.2 + educ + right + 
                     union + church.att + household + curr.unempl.tot + inc.safe + 
                     incdiff.eval + nomoney.100 + immigr.100 + poor.freer +
                     Rgdp + Ruerate + Rforeign + Rpopdens +
                     gdp_pc_08 + c_unraall_2008 + immigr.perc + 
                       progr.ws.2010 + 
                     (1| cntry))
summary(mm.hpoor.app)
##



## for appendix 

## FIXED EFFECTS: Table A2

#1
fixed.1 <- lm(help.poor ~ Rgini + income + 
                      female + agea + educ + right + church.att + 
                      Rgdp + Ruerate + Rforeign + Rpopdens + factor(cntry) -1) 
summary(fixed.1)

#2) 
fixed.2 <- lm(help.poor ~ Rgini + income +  
                     female + agea + age.2 + educ + right + 
                     union + church.att + household + curr.unempl.tot + inc.safe + 
                     Rgdp + Ruerate + Rforeign + Rpopdens + factor(cntry) -1)
summary(fixed.2)


#3) 
fixed.3 <- lm(help.poor ~ Rgini + income +  
                     female + agea + age.2 + educ + right + 
                     union + church.att + household + curr.unempl.tot + inc.safe + 
                     incdiff.eval + nomoney.100 + immigr.100 + poor.freer +
                     Rgdp + Ruerate + Rforeign + Rpopdens + factor(cntry) -1)
summary(fixed.3)



## SUPPORTING IMMIGRANTS (Table 1 in PAPER)

## ATTENTION!! ##
## In the paper are models 1, 3, 4 (2 in the appendix)

#1) Individual socio-demographoc and Regional variables 
mm.imm.a <- lmer(immigr.benef ~ Rgini + income + citizen + 
                     female + agea + educ + right + church.att + 
                     Rgdp + Ruerate + Rforeign + Rpopdens +
                     (1| cntry))
summary(mm.imm.a)


### Predicted values
summary(Rgini)
prediction(mm.imm.a, at = list(Rgini = c(0.223, 0.411)))
prediction(mm.imm.a, at = list(income = c(0, 9)))
prediction(mm.imm.a, at = list(right = c(0, 9)))
prediction(mm.imm.a, at = list(educ = c(0, 4)))
prediction(mm.imm.a, at = list(agea = c(15, 98)))

prediction(mm.imm.a, at = list(Rgini = c(0.223, 0.282, 0.288, 0.333, 0.411)))


library(margins)
fixed.imm.1 <- lm(immigr.benef ~ Rgini + income + citizen + church.att +
                    female + agea + educ + right +   
                    Rgdp + Ruerate + Rforeign + Rpopdens + factor(cntry) -1, data=subset.OECD)
summary(fixed.imm.1)
fixed.imm.1.try <- lm(immigr.benef ~ Rgini + income + citizen + church.att +
                    female + agea + educ + right +   
                    Rgdp + Ruerate + Rforeign + Rpopdens + cntry, data=subset.OECD)
summary(fixed.imm.1.try)
prediction(fixed.imm.1, at = list(Rgini = c(0.223, 0.282, 0.288, 0.333, 0.411)))



###

#2) Further individual controls
mm.imm.b <- lmer(immigr.benef ~ Rgini + income + citizen + 
                     female + agea + age.2 + educ + right + 
                     union + church.att + household + curr.unempl.tot + inc.safe + 
                     Rgdp + Ruerate + Rforeign + Rpopdens +
                     (1| cntry))
summary(mm.imm.b)


#3) Individual perceptions
mm.imm.c <- lmer(immigr.benef ~ Rgini + income + citizen +  
                     female + agea + age.2 + educ + right + 
                     union + church.att + household + curr.unempl.tot + inc.safe + 
                     nomoney.100 + immigr.100 +
                     imm.good + imm.good.cult + imm.freerid +
                     Rgdp + Ruerate + Rforeign + Rpopdens +
                     (1| cntry))
summary(mm.imm.c)


#4) National controls
mm.imm.d <- lmer(immigr.benef ~ Rgini + income + citizen +
                     female + agea + age.2 + educ + right + 
                     union + church.att + household + curr.unempl.tot + inc.safe + 
                     nomoney.100 + immigr.100 + 
                     imm.good + imm.good.cult + imm.freerid +
                     Rgdp + Ruerate + Rforeign + Rpopdens +
                     gdp_pc_08 + c_soexgdp_2007 +  c_unraall_2008 + immigr.perc +
                     (1| cntry))
summary(mm.imm.d)

# Appendix: Table A3
# With inequality evaluation as additional control 
mm.imm.d2 <- lmer(immigr.benef ~ Rgini + income + citizen + 
                    female + agea + age.2 + educ + right + 
                    union + church.att + household + curr.unempl.tot + inc.safe + 
                    nomoney.100 + immigr.100 + 
                    imm.good + imm.good.cult + imm.freerid + poor.freer + incdiff.eval +
                    Rgdp + Ruerate + Rforeign + c_pode_2008 +
                    gdp_pc_08 + c_soexgdp_2007 + c_unraall_2008 + immigr.perc + 
                    (1| cntry))
summary(mm.imm.d2)


stargazer(mm.imm.d2,   
          type="html", star.cutoffs = c(0.05, 0.01, 0.001), no.space = TRUE,
          out="mod.htm")


##
# Appedix, Table A4: additional national control: substracting the Gini market income minus the Gini disposable income 

mm.imm.app <- lmer(immigr.benef ~ Rgini + income + citizen +
                   female + agea + age.2 + educ + right + 
                   union + church.att + household + curr.unempl.tot + inc.safe + 
                   nomoney.100 + immigr.100 + 
                   imm.good + imm.good.cult + imm.freerid +
                   Rgdp + Ruerate + Rforeign + Rpopdens +
                   gdp_pc_08 + c_unraall_2008 + immigr.perc + 
                     progr.ws.2010 +
                   (1| cntry))
summary(mm.imm.app)

#table with (gini market - gini after taxes) as control for appendix for both poor and immigrants
stargazer(mm.hpoor.app, mm.imm.app,  
          type="html", star.cutoffs = c(0.05, 0.01, 0.001), no.space = TRUE,
          out="mod.htm")
##

stargazer(mm.imm.a, mm.imm.b, mm.imm.c, mm.imm.d,  
          type="html", star.cutoffs = c(0.05, 0.01, 0.001), no.space = TRUE,
          out="mod.htm")

stargazer(mm.hpoor.a, mm.hpoor.b, mm.hpoor.c, mm.hpoor.d, mm.imm.a, mm.imm.b, mm.imm.c, mm.imm.d, 
          type="html", star.cutoffs = c(0.05, 0.01, 0.001), no.space = TRUE,
          out="mod.htm")


stargazer(mm.imm.c2, mm.imm.d2,
          type="html", star.cutoffs = c(0.05, 0.01, 0.001), no.space = TRUE,
          out="mod.htm")


## Appendix 

## FIXED EFFECTS (Table A2)


#1)  
fixed.imm.1 <- lm(immigr.benef ~ Rgini + income + citizen + church.att +
                   female + agea + educ + right +   
                   Rgdp + Ruerate + Rforeign + Rpopdens + factor(cntry) -1)
summary(fixed.imm.1)

#2) 
fixed.imm.2 <- lm(immigr.benef ~ Rgini + income + citizen + church.att + 
                   female + agea + age.2 + educ + right +
                   union + household + curr.unempl.tot + inc.safe + 
                   Rgdp + Ruerate + Rforeign + Rpopdens + factor(cntry) -1)
summary(fixed.imm.2)

#3) 
fixed.imm.3 <- lm(immigr.benef ~ Rgini + income + citizen +  
                   female + agea + age.2 + educ + right + 
                   union + church.att + household + curr.unempl.tot + inc.safe + 
                   nomoney.100 + immigr.100 +
                   imm.good + imm.good.cult + imm.freerid +
                   Rgdp + Ruerate + Rforeign + Rpopdens + factor(cntry) -1)
summary(fixed.imm.3)


stargazer(fixed.1, fixed.2, fixed.3, fixed.imm.1, fixed.imm.2, fixed.imm.3,  
          type="html", star.cutoffs = c(0.05, 0.01, 0.001), no.space = TRUE,
          out="mod.htm")


## FURTHER CHECKS (mentioned in footnote 15)

## A] IMMIGRANTS OPERATIONALIZE DV AS 1=NEVER / BECOME CITIZENS ; 0=EVERYTHING ELSE

#1
mm.imm.nev.a <- lmer(imm.ben.never.citiz ~ Rgini + income + citizen + 
                   female + agea + educ + right + church.att + 
                   Rgdp + Ruerate + Rforeign + Rpopdens +
                   (1| cntry))
summary(mm.imm.nev.a)

#4) National controls
mm.imm.nev.d <- lmer(imm.ben.never.citiz ~ Rgini + income + citizen +
                   female + agea + age.2 + educ + right + 
                   union + church.att + household + curr.unempl.tot + inc.safe + 
                   nomoney.100 + immigr.100 + 
                   imm.good + imm.good.cult + imm.freerid +
                   Rgdp + Ruerate + Rforeign + Rpopdens +
                   gdp_pc_08 + c_soexgdp_2007 +  c_unraall_2008 + immigr.perc +
                   (1| cntry))
summary(mm.imm.nev.d)


## B] INCLUDE PARTY VOTED FOR AS CONTROL

party.voted<- ifelse((cntry == "BE"), prtvtbbe, 
              ifelse((cntry == "CH"), prtvtbch,
              ifelse((cntry == "FR"), prtvtbfr, 
              ifelse((cntry == "DK"), prtvtbdk, 
              ifelse((cntry == "FI"), prtvtafi, 
              ifelse((cntry == "DE"), prtvbde1,
              ifelse((cntry == "GB"), prtvtgb, 
              ifelse((cntry == "IE"), prtvtie, 
              ifelse((cntry == "NL"), prtvtcnl, 
              ifelse((cntry == "NO"), prtvtno, 
              ifelse((cntry == "PT"), prtvtapt,
              ifelse((cntry == "ES"), prtvtbes, 
              ifelse((cntry == "SE"), prtvtse,
              99)))))))))))))
table(party.voted)

#Native models
#1
mm.hpoor.party.a <- lmer(help.poor ~ Rgini + income + 
                     female + agea + educ + right + church.att + 
                     Rgdp + Ruerate + Rforeign + Rpopdens +
                       party.voted +
                     (1| cntry))
summary(mm.hpoor.party.a)

#4) National controls
mm.hpoor.party.d <- lmer(help.poor ~ Rgini + income + 
                     female + agea + age.2 + educ + right + 
                     union + church.att + household + curr.unempl.tot + inc.safe + 
                     incdiff.eval + nomoney.100 + immigr.100 + poor.freer +
                     Rgdp + Ruerate + Rforeign + Rpopdens +
                     gdp_pc_08 + c_soexgdp_2007 + c_unraall_2008 + immigr.perc + 
                       party.voted +
                     (1| cntry))
summary(mm.hpoor.party.d)

#Immigrant models
#1
mm.imm.party.a <- lmer(immigr.benef ~ Rgini + income + citizen + 
                       female + agea + educ + right + church.att + 
                       Rgdp + Ruerate + Rforeign + Rpopdens +
                       party.voted + (1| cntry))
summary(mm.imm.party.a)

#4) National controls
mm.imm.nev.d <- lmer(immigr.benef ~ Rgini + income + citizen +
                       female + agea + age.2 + educ + right + 
                       union + church.att + household + curr.unempl.tot + inc.safe + 
                       nomoney.100 + immigr.100 + 
                       imm.good + imm.good.cult + imm.freerid +
                       Rgdp + Ruerate + Rforeign + Rpopdens +
                       gdp_pc_08 + c_soexgdp_2007 +  c_unraall_2008 + immigr.perc +
                       party.voted + (1| cntry))
summary(mm.imm.nev.d)
