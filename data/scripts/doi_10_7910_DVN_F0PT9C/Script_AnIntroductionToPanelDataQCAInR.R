# Codes for Chapters:

## Chapters 4 and 5:

### Running packages:
library("readxl")
library("dplyr")
library("ggplot2")
library("QCA")
library("SetMethods")
library("readr")
library("rmarkdown")
library("knitr")

##Downloading dataset:
data<-read.csv("Panel_data.csv")
str(data)
head(data)
summary(data)

### To add row names:
pnames <- c("BIH99",
            "HRV99",
            "MNE99",
            "BIH00",
            "HRV00",
            "MNE00",
            "BIH01",
            "HRV01",
            "MNE01",
            "BIH02",
            "HRV02",
            "MNE02",
            "BIH03",
            "HRV03",
            "MNE03",
            "BIH04",
            "HRV04",
            "MNE04",
            "BIH05",
            "HRV05",
            "MNE05",
            "BIH06",
            "HRV06",
            "MNE06",
            "BIH07",
            "HRV07",
            "MNE07")
data <- as.data.frame(data)
row.names(data) <- pnames

## Calibrating Presence of MFIS: (Set: Countries with Presence of MFIS)
str(data$PresenceofMFIs)
Xplot(data$PresenceofMFIs, jitter=TRUE)
data$MFISR<-calibrate(data$PresenceofMFIs, type="crisp", thresholds=1)
skew.check(data$MFISR)
print(data[ ,c("Observations", "PresenceofMFIs", "MFISR")])

## Chapter 4 Theory-Guided Calibration of EUM:
data$THEUM<-NA 
data$THEUM[data$EUM >=3]<-1 
data$THEUM[data$EUM<3 & data$EUM>=2]<-0.67 
data$THEUM[data$EUM<2 & data$EUM>=1]<-0.33 
data$THEUM[data$EUM<1]<-0 
data$THEUM 
## OR
data$EUMTHR<-recode(data$EUM, cuts="0, 1, 2", values="0, 0.33, 0.67, 1") 
print(data[ ,c("EUM","THEUM", "EUMTHR")])
skew.check(data$THEUM)
skew.check(data$EUMTHR)

## Ragin's Indirect Calibration: 
Xplot(data$OWN, jitter=TRUE, at=pretty(data$OWN))
sort(data$OWN)
quantfown <- quantile(data$OWN,c(0.2,0.4,0.5,0.6,0.8))
quantfown
data$OWNB<-NA
data$OWNB[data$OWN<= quantfown [1]]<-0
data$OWNB[data$OWN > quantfown [1] & data$OWN <= quantfown [2]]<-0.2
data$OWNB[data$OWN > quantfown [2] & data$OWN <= quantfown [3]]<-0.4
data$OWNB[data$OWN > quantfown [3] & data$OWN <= quantfown [4]]<-0.6
data$OWNB[data$OWN > quantfown [4] & data$OWN <= quantfown [5]]<-0.8
data$OWNB[data$OWN > quantfown [5]]<-1
data$OWNbinom <- indirectCalibration(data$OWN, data$OWNB, binom=TRUE)
data$OWNbeta <- indirectCalibration(data$OWN, data$OWNB, binom=FALSE)
cor(data$OWNbinom,data$OWNbeta)
plot(data$OWN,data$OWNbinom, col="red")
plot(data$OWN,data$OWNbeta,col="blue")
skew.check(data$OWNbinom)
skew.check(data$OWNbeta)


## Calibrating International Human Rights: (Set of countries with high number of international human rights treaties)
hist(data$INTHRT)
str(data$INTHRT)
sort(data$INTHRT)
Xplot(data$INTHRT, jitter=TRUE)
data$HRTR<-calibrate(data$INTHRT, type="fuzzy", thresholds="e=0, c=10.5, i=14", logistic=TRUE)
skew.check(data$HRTR)
print(data[ ,c("INTHRT","HRTR")])
plot(data$INTHRT, data$HRTR, main="Calibration of HRT", xlab="Raw Score", ylab="Calibrated Score", abline(h=0.5, v=10.5))

# Calibrating GDP: Set of countries with high GDP
ggplot(data, aes(x=YEAR,y=GDP,group=COUNTRY))+geom_line(aes(col=COUNTRY)) + theme_light()
print(data$GDP)
sort(data$GDP)
hist(data$GDP)
Xplot(data$GDP, jitter=TRUE)
data$GDPR<-calibrate(data$GDP, thresholds="e=980000000, c=30000000000, i=50000000000", logistic=TRUE)
skew.check(data$GDPR)
plot(data$GDP, data$GDPR, main="Calibration of GDP", xlab="Raw Score", ylab="Calibrated Score", abline(h=0.5, v=30000000000))


# Calibrating Democracy, Set of highly democratic countries: "autocracies" (-10 to -6), "anocracies" (-5 to +5), 
# three special values: -66, -77 and -88), and "democracies" (+6 to +10)
ggplot(data, aes(x=YEAR,y=DEM,group=COUNTRY))+geom_line()+facet_wrap(.~COUNTRY) + theme_light()
print(data$DEM)
str(data$DEM)
hist(data$DEM)
Xplot(data$DEM, jitter=TRUE)
data$DEMC<-calibrate(data$DEM, thresholds="e=-67,c=5.3,i=7", logistic=TRUE)
skew.check(data$DEMC)
print(data[ ,c("DEM","DEMC")])
plot(data$DEM,data$DEMC, main="Calibration of DEM", xlab="Raw Score", ylab="Calibrated Score", abline (h=0.5,v=5.3))

# Calibrating PCL: Set of countries with high political and civil liberty
# free(1.0-2.5), partly free (3.0-5.0), not free (5.5-7.0)
#ggplot(data, aes(x=YEAR,y=PCL, group=COUNTRY)) + geom_line(aes(col=COUNTRY)) + theme_light()
ggplot(data, aes(x=YEAR,y=PCL, group=COUNTRY)) + geom_line() + facet_wrap(.~COUNTRY) + theme_light()
print(data$PCL)
sort(data$PCL)
Xplot(data$PCL, jitter=TRUE)
hist(data$PCL)
data$DPCL<-calibrate(data$PCL, type="fuzzy", thresholds="i=1.9,c=2.6, e=5.1", logistic = TRUE)
plot(data$PCL,data$DPCL, main="Calibration of PCL", xlab="Raw Score", ylab="Calibrated Score", abline (h=0.5,v=2.6))
skew.check(data$DPCL)
print(data[ ,c("PCL","DPCL")])

##Calibrating Percentage of Literate Females, Set of countries with high percentage of literate females:
print(data[ ,c("Observations","LTRCYF")])
sort(data$LTRCYF)
Xplot(data$LTRCYF, jitter=TRUE)
hist(data$LTRCYF)
data$LTRCYR<-calibrate(data$LTRCYF, type="fuzzy", thresholds="e=98.70,c=99.70,i=99.84", logistic = TRUE)
skew.check(data$LTRCYR)
print(data[ ,c("Observations","LTRCYF", "LTRCYR")])


##Calibrating Life Expectancy for Females, Set of countries with high life expectancy for females:
print(data[ ,c("Observations","LIFEXF")])
sort(data$LIFEXF)
Xplot(data$LIFEXF, jitter=TRUE)
hist(data$LIFEXF)
data$LIFEXFR<-calibrate(data$LIFEXF, type="fuzzy", thresholds="e=76.0,c=77.85,i=79.00", logistic = TRUE)
skew.check(data$LIFEXFR)
print(data[ ,c("Observations","LIFEXF", "LIFEXFR")])
plot(data$LIFEXF,data$LIFEXFR, main="Calibration of LIFEXF", xlab="Raw Score", ylab="Calibrated Score", abline (h=0.5,v=77.85))

## Calibrating access to high transparency guarantees:
# Control_of_Corruption, Set of high control of corruption:
# Researchers started collecting this data from the year 1996
# Hence, I have coded the year 1995 as zero
# Between the years 1996-2001, data was collected for every alternate year.
# Hence, I have recoded 1997, 1999, 2001 with the values of the year before.
# Between the years 2002-2015, data is available for every year
# Due to the presence of missing values (NAs), R has characterized this variable 
# as a character variable. It needs to be a numeric variable first before recoding.

print(data[ ,c("Observations","FCOR")])
data$NCOR<-as.numeric(data$FCOR)
data$NCOR[which(data$COUNTRY=="BiH" & data$YEAR %in% c(1995))]<-0
data$NCOR[which(data$COUNTRY=="HRV" & data$YEAR %in% c(1995))]<-0
data$NCOR[which(data$COUNTRY=="MNE" & data$YEAR %in% c(1995))]<-0
data$NCOR[which(data$COUNTRY=="MNE" & data$YEAR %in% c(1996))]<-0
data$NCOR[which(data$COUNTRY=="BiH" & data$YEAR %in% c(1997))]<-48.92473
data$NCOR[which(data$COUNTRY=="HRV" & data$YEAR %in% c(1997))]<-32.79570
data$NCOR[which(data$COUNTRY=="MNE" & data$YEAR %in% c(1997))]<-0

data$NCOR[which(data$COUNTRY=="BiH" & data$YEAR %in% c(1999))]<-42.783499999999997
data$NCOR[which(data$COUNTRY=="HRV" & data$YEAR %in% c(1999))]<-34.536079999999998
data$NCOR[which(data$COUNTRY=="MNE" & data$YEAR %in% c(1999))]<-71.134020000000007
data$NCOR[which(data$COUNTRY=="BiH" & data$YEAR %in% c(2001))]<-33.502540000000003
data$NCOR[which(data$COUNTRY=="HRV" & data$YEAR %in% c(2001))]<-56.345179999999999
data$NCOR[which(data$COUNTRY=="MNE" & data$YEAR %in% c(2001))]<-53.299489999999999
print(data[ ,c("Observations","FCOR", "NCOR")])
print(data$NCOR)
ggplot(data, aes(x=YEAR,y=NCOR, group=COUNTRY)) + geom_line(aes(col=COUNTRY)) + theme_light()
Xplot(data$NCOR,jitter=TRUE,at=pretty(data$NCOR))
hist(data$NCOR)
sort(data$NCOR)
data$NCORR<-calibrate(data$NCOR, type="fuzzy", thresholds="e=33.0, c=54.0, i=62.0", logistic = TRUE)
skew.check(data$NCORR)
print(data[ ,c("Observations","NCOR", "NCORR")])

# Condition: FEFFECT, Set of countries with high government effectiveness
# Government effectiveness 
# The data is in percentage format
# Researchers started collecting this data from the year 1996
# Hence, I have coded the year 1995 as zero
# Between the years 1996-2001, data was collected for every alternate year.
# Hence, I have recoded 1997, 1999, 2001 with the values of the year before.
# Between the years 2002-2015, data is available for every year
# Due to the presence of missing values (NAs), R has characterized the variable 
# as a character variable. It needs to be a numeric variable first before recoding.

print(data[ ,c("Observations","FEFFECT")])
data$NEFFECT<- as.numeric(data$FEFFECT)
data$NEFFECT[which(data$COUNTRY=="BiH" & data$YEAR %in% c(1999))]<-11.398960000000001
data$NEFFECT[which(data$COUNTRY=="HRV" & data$YEAR %in% c(1999))]<-59.58549
data$NEFFECT[which(data$COUNTRY=="MNE" & data$YEAR %in% c(1999,2000,2001,2002,2003,2004))]<-0
data$NEFFECT[which(data$COUNTRY=="BiH" & data$YEAR %in% c(2001))]<-17.948720000000002
data$NEFFECT[which(data$COUNTRY=="HRV" & data$YEAR %in% c(2001))]<-66.666659999999993
print(data$NEFFECT)
sort(data$NEFFECT)
#ggplot(data, aes(x=YEAR,y=NEFFECT, group=COUNTRY)) + geom_line(aes(col=COUNTRY)) + theme_light()
Xplot(data$NEFFECT, jitter=TRUE)
hist(data$NEFFECT)
data$NEFFECTR<-calibrate(data$NEFFECT, type="fuzzy", thresholds="e=10.0, c=55.0, i=69.0", logistic = TRUE)
skew.check(data$NEFFECTR)
plot(data$NEFFECT,data$NEFFECTR, main="Calibration of Government effectiveness", xlab="Raw Score", ylab="Calibrated Score", abline(h=0.5,v=55.0))
print(data[ ,c("Observations", "NEFFECT", "NEFFECTR")])


## Calibrating Female Representation in Parliament; Set of Countries with High Female Representation
print(data[ ,c("Observations","FPARL")])
data$FPARL[which(data$COUNTRY=="BiH" & data$YEAR %in% c(1999, 2000))]<-0
data$FPARL[which(data$COUNTRY=="BiH" & data$YEAR %in% c(2002))]<-7.142
data$FPARL[which(data$COUNTRY=="HRV" & data$YEAR %in% c(2003))]<-20.520
data$FPARL[which(data$COUNTRY=="MNE" & data$YEAR %in% c(1999, 2000, 2001, 2002, 2003, 2004, 2005))]<-0
Xplot(data$FPARL, jitter=TRUE, at=pretty(data$FPARL))
hist(data$FPARL)
sort(data$FPARL)
data$FRPR<-calibrate(data$FPARL, type="fuzzy", thresholds="e=7.00, c=17.00, i=21.00", logistic = TRUE)
skew.check(data$FRPR)
print(data[ ,c("Observations","FPARL", "FRPR")])
plot(data$FPARL,data$FRPR, main="Calibration of FRParl", xlab="Raw Score", ylab="Calibrated Score", abline(h=0.5,v=17.00))

# Calibrating Female Own-Account Workers, Set of countries with high number of female own-account workers:
Xplot(data$OWN, jitter=TRUE, at=pretty(data$OWN))
sort(data$OWN)
hist(data$OWN)
data$OWNR<-calibrate(data$OWN, type="fuzzy", thresholds="e=4000, c=77000, i=110000", logistic = TRUE)
skew.check(data$OWNR)
plot(data$OWN,data$OWNR, main="Calibration of Own-Account Workers", xlab="Raw Score", ylab="Calibrated Score", abline(h=0.5,v=77000))

## Calibrating female family workers, Set of countries with High Number of Female Family Workers:
Xplot(data$FAM, jitter=TRUE, at=pretty(data$FAM))
sort(data$FAM)
hist(data$FAM)
data$FAMR<-calibrate(data$FAM, type="fuzzy", thresholds="e=1700, c=43000, i=60000", logistic = TRUE)
skew.check(data$FAMR)
plot(data$FAM,data$FAMR, main="Calibration of Female Family Workers", xlab="Raw Score", ylab="Calibrated Score", abline(h=0.5,v=43000))

## Calibrating female employees, Set of countries with high number of female employees:
Xplot(data$EMPE, jitter=TRUE, at=pretty(data$EMPE))
sort(data$EMPE)
hist(data$EMPE)
data$EMPER<-calibrate(data$EMPE, type="fuzzy", thresholds="e=55000, c=300000, i=610000", logistic = TRUE)
skew.check(data$EMPER)
plot(data$EMPE,data$EMPER, main="Calibration of Female Employees", xlab="Raw Score", ylab="Calibrated Score", abline(h=0.5,v=300000))

## Calibrating female employers, Set of countries with a high number of female employers:
Xplot(data$EMPR,jitter=TRUE, at=pretty(data$EMPR))
sort(data$EMPR)
hist(data$EMPR)
data$EMPRR<-calibrate(data$EMPR, type="fuzzy", thresholds="e=3000, c=15000, i=25000", logistic = TRUE)
skew.check(data$EMPRR)
plot(data$EMPR,data$EMPRR, main="Calibration of Female Employers", xlab="Raw Score", ylab="Calibrated Score", abline(h=0.5,v=15000))

#Creating a macro-condition: Set of high number of women employed at the national economy
#data$HNWE<-with(data,fuzzyor(OWNR, FAMR, EMPER, EMPRR))
#print(data[ ,c("Observations", "OWNR", "FAMR", "EMPER", "EMPRR", "HNWE")])
skew.check(data$HNWE)
data$HNWEB<-with(data,fuzzyand(OWNR, FAMR, EMPER, EMPRR))
skew.check(data$HNWEB)
print(data[ ,c("Observations", "OWNR", "FAMR", "EMPER", "EMPRR", "HNWEB")])

## Calibrating access to high transparency guarantees:
# Control_of_Corruption, Set of high control of corruption:
# Researchers started collecting this data from the year 1996
# Hence, I have coded the year 1995 as zero
# Between the years 1996-2001, data was collected for every alternate year.
# Hence, I have recoded 1997, 1999, 2001 with the values of the year before.
# Between the years 2002-2015, data is available for every year
# Due to the presence of missing values (NAs), R has characterized this variable 
# as a character variable. It needs to be a numeric variable first before recoding.

print(data[ ,c("Observations","FCOR")])
data$FCOR[which(data$COUNTRY=="BiH" & data$YEAR %in% c(1999))]<-42.783499999999997
data$FCOR[which(data$COUNTRY=="HRV" & data$YEAR %in% c(1999))]<-34.536079999999998
data$FCOR[which(data$COUNTRY=="MNE" & data$YEAR %in% c(1999))]<-71.134020000000007
data$FCOR[which(data$COUNTRY=="BiH" & data$YEAR %in% c(2001))]<-33.502540000000003
data$FCOR[which(data$COUNTRY=="HRV" & data$YEAR %in% c(2001))]<-56.345179999999999
data$FCOR[which(data$COUNTRY=="MNE" & data$YEAR %in% c(2001))]<-53.299489999999999
#ggplot(data, aes(x=YEAR,y=FCOR, group=COUNTRY)) + geom_line(aes(col=COUNTRY)) + theme_light()
Xplot(data$FCOR,jitter=TRUE,at=pretty(data$FCOR))
hist(data$FCOR)
sort(data$FCOR)
data$FCORR<-calibrate(data$FCOR, type="fuzzy", thresholds="e=33.0, c=55.0, i=71.0", logistic = TRUE)
skew.check(data$FCORR)
print(data[ ,c("Observations","FCOR", "FCORR")])
plot(data$FCOR,data$FCORR, main="Calibration of Female Employers", xlab="Raw Score", ylab="Calibrated Score", abline(h=0.5,v=55.0))


##To check for presence of necessary conditions: Presence of outcome (HNWEB)
cluster(data = data, results = "MFISR", outcome = "HNWEB",unit_id = "COUNTRY", cluster_id = "YEAR", necessity=TRUE, wicons=TRUE)
cluster(data = data, results = "GDPR", outcome = "HNWEB",unit_id = "COUNTRY", cluster_id = "YEAR", necessity=TRUE, wicons=TRUE)
cluster(data = data, results = "DEMC", outcome = "HNWEB",unit_id = "COUNTRY", cluster_id = "YEAR", necessity=TRUE, wicons=TRUE)
cluster(data = data, results = "DPCL", outcome = "HNWEB",unit_id = "COUNTRY", cluster_id = "YEAR", necessity=TRUE, wicons=TRUE)
cluster(data = data, results = "LTRCYR", outcome = "HNWEB",unit_id = "COUNTRY", cluster_id = "YEAR", necessity=TRUE, wicons=TRUE)
cluster(data = data, results = "LIFEXFR", outcome = "HNWEB",unit_id = "COUNTRY", cluster_id = "YEAR", necessity=TRUE, wicons=TRUE)
cluster(data = data, results = "FCORR", outcome = "HNWEB",unit_id = "COUNTRY", cluster_id = "YEAR", necessity=TRUE, wicons=TRUE)
cluster(data = data, results = "NEFFECTR", outcome = "HNWEB",unit_id = "COUNTRY", cluster_id = "YEAR", necessity=TRUE, wicons=TRUE)
cluster(data = data, results = "FRPR", outcome = "HNWEB",unit_id = "COUNTRY", cluster_id = "YEAR", necessity=TRUE, wicons=TRUE)
cluster(data = data, results = "HRTR", outcome = "HNWEB",unit_id = "COUNTRY", cluster_id = "YEAR", necessity=TRUE, wicons=TRUE)

##To check for absence of necessary conditions: Presence of outcome (HNWEB)
cluster(data = data, results = "~MFISR", outcome = "HNWEB",unit_id = "COUNTRY", cluster_id = "YEAR", necessity=TRUE, wicons=TRUE)
cluster(data = data, results = "~GDPR", outcome = "HNWEB",unit_id = "COUNTRY", cluster_id = "YEAR", necessity=TRUE, wicons=TRUE)
cluster(data = data, results = "~DEMC", outcome = "HNWEB",unit_id = "COUNTRY", cluster_id = "YEAR", necessity=TRUE, wicons=TRUE)
cluster(data = data, results = "~DPCL", outcome = "HNWEB",unit_id = "COUNTRY", cluster_id = "YEAR", necessity=TRUE, wicons=TRUE)
cluster(data = data, results = "~LTRCYR", outcome = "HNWEB",unit_id = "COUNTRY", cluster_id = "YEAR", necessity=TRUE, wicons=TRUE)
cluster(data = data, results = "~LIFEXFR", outcome = "HNWEB",unit_id = "COUNTRY", cluster_id = "YEAR", necessity=TRUE, wicons=TRUE)
cluster(data = data, results = "~FCORR", outcome = "HNWEB",unit_id = "COUNTRY", cluster_id = "YEAR", necessity=TRUE, wicons=TRUE)
cluster(data = data, results = "~NEFFECTR", outcome = "HNWEB",unit_id = "COUNTRY", cluster_id = "YEAR", necessity=TRUE, wicons=TRUE)
cluster(data = data, results = "~FRPR", outcome = "HNWEB",unit_id = "COUNTRY", cluster_id = "YEAR", necessity=TRUE, wicons=TRUE)
cluster(data = data, results = "~HRTR", outcome = "HNWEB",unit_id = "COUNTRY", cluster_id = "YEAR", necessity=TRUE, wicons=TRUE)

#pof("MFISR<=HNWEB", data=data, relation="necessity")

QCAfit(data$MFISR, data$HNWEB, cond.lab="High Presence of MFIs", necessity = TRUE, consH = TRUE)
QCAfit(1-data$MFISR, data$HNWEB, cond.lab="Low Presence ofMFIs", necessity = TRUE, consH = TRUE)
QCAfit(data$LTRCYR, data$HNWEB, cond.lab="High Female LTRCY", necessity = TRUE, consH = TRUE)
QCAfit(1-data$LTRCYR, data$HNWEB, cond.lab="Absence of High Female LTRCY", necessity = TRUE, consH = TRUE)
QCAfit(data$HRTR, data$HNWEB, cond.lab="Presence of HRTR", necessity = TRUE, consH = TRUE)
QCAfit(1-data$HRTR, data$HNWEB, cond.lab="Absence of HRTR", necessity = TRUE, consH = TRUE)

## Creating a new data set:
write.csv(DATA, "DATA.csv")
DATA<-read.csv("data.csv")
PANEL<-DATA %>% select(Observations, MFISR, GDPR, DEMC, DPCL, LTRCYR, LIFEXFR, FCORR, NEFFECTR, FRPR, HRTR, HNWEB, COUNTRY, YEAR)

##Truth Table for HNWEB:
ttEMP<-truthTable(data=PANEL, outcome="HNWEB", 
                  conditions= c("MFISR", "GDPR", "DEMC", "DPCL", "LTRCYR", "LIFEXFR", "FCORR", "NEFFECTR", "FRPR", "HRTR"), 
                   incl.cut = 0.85, 
                   sort.by = "incl,n", pri.cut=0.51, dcc= TRUE, decreasing=FALSE,complete=TRUE, show.cases = TRUE)
ttEMP


ttemp<-truthTable(data=PANEL, outcome="~HNWEB", 
                  conditions= c("MFISR", "GDPR", "DEMC", "DPCL", "LTRCYR", "LIFEXFR", "FCORR", "NEFFECTR", "FRPR", "HRTR"), 
                  incl.cut = 0.85, 
                  sort.by = "incl,n", pri.cut=0.51, dcc= TRUE, decreasing=FALSE,complete=TRUE, show.cases = TRUE)
ttemp

## Boolean Minimization for truth table:
sol_EMPB<-minimize(input=ttEMP, details=TRUE, row.dom=TRUE)
sol_EMPB
sol_empb<-minimize(input=ttemp, details=TRUE, row.dom=TRUE)
sol_empb


##For the conservative, parsimonious, and intermediate solutions:
sol_CEMP <- minimize(ttEMP, details=TRUE,  row.dom=TRUE)
sol_CEMP
sol_PEMP <-minimize(ttEMP, include="?", details=TRUE,  row.dom=TRUE)
sol_PEMP
sol_IEMP <- minimize(ttEMP, include = "?", dir.exp = "1, 1, 1, 1, 1, 1, 1, 1, 1, 1",details=TRUE,  row.dom=TRUE)
sol_IEMP

# For the conservative, parsimonious, and intermediate solutions of the sufficiency result, absence:
sol_cemp <- minimize(ttemp, details=TRUE,  row.dom=TRUE)
sol_cemp
sol_pemp<-minimize(ttemp, include="?", details=TRUE,  row.dom=TRUE)
sol_pemp
sol_iemp <- minimize(ttemp, include = "?", dir.exp = "0, 0, 0, 0, 0, 0, 0, 0, 0, 0",details=TRUE, row.dom=TRUE)
sol_iemp


## For identifying errors, using the findRows function:
findRows(obj=ttEMP, type=0)
TYPETWO<-findRows(obj=ttEMP, type=2)
TYPETWO
findRows(obj=ttEMP, type=3)

## Identify remainder rows with CSA (type 2 error):
CSAC<-LR.intersect(sol_CEMP, sol_cemp)
CSAC
CSAP<-LR.intersect(sol_PEMP, sol_pemp)
CSAP
CSAI<-LR.intersect(sol_IEMP, sol_iemp)
CSAI

## Create a new truth table:
ttEMPN<-esa(oldtt=ttEMP, contrad_rows=c(CSAP))
ttEMPN

## For the enhanced conservative, parsimonious, and intermediate solutions:
sol_CEMPN <- minimize(ttEMP, details=TRUE, exclude=c(TYPETWO))
sol_CEMPN
sol_PEMPN <- minimize(ttEMP, include="?", details=TRUE, exclude=c(TYPETWO))
sol_PEMPN
sol_IEMPN <- minimize(ttEMP, include="?", details=TRUE, dir.exp = "1, 1, 1, 1, 1, 1, 1, 1, 1, 1", exclude=c(TYPETWO))
sol_IEMPN


## Alternatives codes for creating the enhanced conservative, parsimonious, and intermediate solutions, without creating a new truth table:
#sol_CINFRS<-minimize(ttEMPN, details=TRUE, exclude=c(TYPETWO))
#sol_CINFRS
#sol_PINFRS<-minimize(ttEMPN, include="?", details=TRUE, exclude=c(TYPETWO))
#sol_PINFRS
#sol_IINFRS<-minimize(ttEMPN, include = "?", dir.exp = "1, 1, 1, 1, 1, 1, 1, 1, 1, 1",details=TRUE, exclude=c(TYPETWO))
#sol_IINFRS

## Identifying variation:
sol<-sol_IEMPN$i.sol$C1P1$pims
sol
CLUSTERPANEL<-c(PANEL,sol)
CLUSTERPANELA<-as.data.frame(CLUSTERPANEL)
PANELA<-cluster(data = CLUSTERPANELA, results="MFISR*~DEMC*~DPCL*NEFFECTR*~HRTR", outcome = "HNWEB",unit_id = "COUNTRY", cluster_id = "YEAR", necessity=FALSE, wicons=TRUE)
PANELA

## Visualizing the pathway from C1P1:
CLUSTERPANELA$Observations<-NULL
CLUSTERPANELA$COUNTRY<-NULL
CLUSTERPANELA$YEAR<-NULL
XYplot(MFISR*~DPCL*NEFFECTR, HNWEB, data=CLUSTERPANELA, relation = "sufficiency", enhance = TRUE, jitter=TRUE, clabels = seq(nrow(CLUSTERPANELA)))

## Visualizing MFISR as a necessary condition for HNWEB:
XYplot(MFISR, HNWEB, data=CLUSTERPANELA, relation = "necessity", enhance = TRUE, jitter=TRUE, clabels = seq(nrow(CLUSTERPANELA)))


## Chapter 6

### Running packages:
library("readxl")
library("dplyr")
library("ggplot2")
library("QCA")
library("SetMethods")
library("readr")

##Downloading and opening dataset:
data_1999<-read.csv("FinalData_1999.csv")
DATA<-as.data.frame(data_1999)
str(DATA)
head(DATA)
summary(DATA)

### To add row names:
pnames <- c("BIH99",
            "HRV99",
            "MNE99")
row.names(DATA) <- pnames

## Calibrating Presence of MFIS: (Set: Presence of MFIS)
str(DATA$PresenceofMFIs)
Xplot(DATA$PresenceofMFIs, jitter=TRUE)
DATA$MFISR<-calibrate(DATA$PresenceofMFIs, type="crisp", thresholds=1)
skew.check(DATA$MFISR)

## Calibrating International Human Rights: (Set of countries with high number of international human rights treaties)
hist(DATA$INTHRT)
str(DATA$INTHRT)
sort(DATA$INTHRT)
Xplot(DATA$INTHRT, jitter=TRUE)
DATA$HRTR<-calibrate(DATA$INTHRT, type="crisp", thresholds=8)
skew.check(DATA$HRTR)
print(DATA[ ,c("INTHRT","HRTR")])

# Calibrating GDP: Set of countries with high GDP
print(DATA$GDP)
sort(DATA$GDP)
hist(DATA$GDP)
Xplot(DATA$GDP, jitter=TRUE)
DATA$GDPR<-calibrate(DATA$GDP, thresholds="e=4600000000, c=20000000000, i=23000000000", logistic=TRUE)
skew.check(DATA$GDPR)
plot(DATA$GDP, DATA$GDPR, main="Calibration of GDP", xlab="Raw Score", ylab="Calibrated Score", abline(h=0.5, v=20000000000))


# Calibrating Democracy, Set of highly democratic countries: "autocracies" (-10 to -6), "anocracies" (-5 to +5), 
# three special values: -66, -77 and -88), and "democracies" (+6 to +10)
print(DATA$DEM)
str(DATA$DEM)
hist(DATA$DEM)
Xplot(DATA$DEM, jitter=TRUE)
DATA$DEMC<-calibrate(DATA$DEM, type="crisp", thresholds=0)
skew.check(DATA$DEMC)
print(DATA[ ,c("DEM","DEMC")])

# Calibrating PCL: Set of countries with high political and civil liberty
# free(1.0-2.5), partly free (3.0-5.0), not free (5.5-7.0)
#ggplot(data, aes(x=YEAR,y=PCL, group=COUNTRY)) + geom_line(aes(col=COUNTRY)) + theme_light()
print(DATA$PCL)
sort(DATA$PCL)
Xplot(DATA$PCL, jitter=TRUE)
hist(DATA$PCL)
DATA$DPCL<-recode(DATA$PCL, rules="5=0; else=1")
skew.check(DATA$DPCL)
print(DATA[ ,c("PCL","DPCL")])

##Calibrating Percentage of Literate Females, Set of countries with high percentage of literate females:
print(DATA[ ,c("Observations","LTRCYF")])
sort(DATA$LTRCYF)
Xplot(DATA$LTRCYF, jitter=TRUE)
hist(DATA$LTRCYF)
DATA$LTRCYR<-calibrate(DATA$LTRCYF, type="fuzzy", thresholds="e=98.70, c=99.00, i=99.60", logistic = TRUE)
skew.check(DATA$LTRCYR)
print(DATA[ ,c("Observations","LTRCYF", "LTRCYR")])


##Calibrating Life Expectancy for Females, Set of countries with high life expectancy for females:
print(DATA[ ,c("Observations","LIFEXF")])
sort(DATA$LIFEXF)
Xplot(DATA$LIFEXF, jitter=TRUE)
hist(DATA$LIFEXF)
DATA$LIFEXFR<-calibrate(DATA$LIFEXF, type="fuzzy", thresholds="e=76.10, c=76.56, i=76.58", logistic = TRUE)
skew.check(DATA$LIFEXFR)
print(DATA[ ,c("Observations","LIFEXF", "LIFEXFR")])
plot(DATA$LIFEXF,DATA$LIFEXFR, main="Calibration of LIFEXF", xlab="Raw Score", ylab="Calibrated Score", abline (h=0.5,v=76.56))

## Calibrating access to high transparency guarantees:
# Control_of_Corruption, Set of high control of corruption:
# Researchers started collecting this data from the year 1996
# Between the years 1996-2001, data was collected for every alternate year.
# Hence, I have recoded 1997, 1999, 2001 with the values of the year before.
# Between the years 2002-2015, data is available for every year
# Due to the presence of missing values (NAs), R has characterized this variable 
# as a character variable. It needs to be a numeric variable first before recoding.

print(DATA[ ,c("Observations","FCOR")])
DATA$NCOR<-as.numeric(DATA$FCOR)
DATA$NCOR[which(DATA$COUNTRY=="BiH" & DATA$YEAR %in% c(1999))]<-42.783499999999997
DATA$NCOR[which(DATA$COUNTRY=="HRV" & DATA$YEAR %in% c(1999))]<-34.536079999999998
DATA$NCOR[which(DATA$COUNTRY=="MNE" & DATA$YEAR %in% c(1999))]<-71.134020000000007
print(DATA[ ,c("Observations","FCOR", "NCOR")])
print(DATA$NCOR)
Xplot(DATA$NCOR,jitter=TRUE,at=pretty(DATA$NCOR))
hist(DATA$NCOR)
sort(DATA$NCOR)
DATA$NCORR<-calibrate(DATA$NCOR, type="fuzzy", thresholds="e=34.0, c=45.0, i=71.0", logistic = TRUE)
skew.check(DATA$NCORR)
print(DATA[ ,c("Observations","NCOR", "NCORR")])

# Condition: FEFFECT, Set of countries with high government effectiveness
# Government effectiveness 
# The data is in percentage format
# Researchers started collecting this data from the year 1996
# Between the years 1996-2001, data was collected for every alternate year.
# Hence, I have recoded 1997, 1999, 2001 with the values of the year before.
# Between the years 2002-2015, data is available for every year
# Due to the presence of missing values (NAs), R has characterized the variable 
# as a character variable. It needs to be a numeric variable first before recoding.

print(DATA[ ,c("Observations","FEFFECT")])
DATA$NEFFECT<- as.numeric(DATA$FEFFECT)
DATA$NEFFECT[which(DATA$COUNTRY=="BiH" & DATA$YEAR %in% c(1999))]<-11.398960000000001
DATA$NEFFECT[which(DATA$COUNTRY=="HRV" & DATA$YEAR %in% c(1999))]<-59.58549
DATA$NEFFECT[which(DATA$COUNTRY=="MNE" & DATA$YEAR %in% c(1999))]<-0
print(DATA$NEFFECT)
sort(DATA$NEFFECT)
Xplot(DATA$NEFFECT, jitter=TRUE)
hist(DATA$NEFFECT)
DATA$NEFFECTR<-calibrate(DATA$NEFFECT, type="fuzzy", thresholds="e=0.0, c=15.0, i=59.0", logistic = TRUE)
skew.check(DATA$NEFFECTR)
plot(DATA$NEFFECT,DATA$NEFFECTR, main="Calibration of Government effectiveness", xlab="Raw Score", ylab="Calibrated Score", abline(h=0.5,v=15.0))
print(DATA[ ,c("Observations", "NEFFECT", "NEFFECTR")])


## Calibrating Female Representation in Parliament; Set of Countries with High Female Representation
print(DATA[ ,c("Observations","FPARL")])
DATA$FPARL[which(DATA$COUNTRY=="BiH" & DATA$YEAR %in% c(1999))]<-0
DATA$FPARL[which(DATA$COUNTRY=="MNE" & DATA$YEAR %in% c(1999))]<-0
Xplot(DATA$FPARL, jitter=TRUE, at=pretty(DATA$FPARL))
hist(DATA$FPARL)
sort(DATA$FPARL)
DATA$FRPR<-calibrate(DATA$FPARL, type="crisp", thresholds=7.9)
skew.check(DATA$FRPR)
print(DATA[ ,c("Observations","FPARL", "FRPR")])

# Calibrating Female Own-Account Workers, Set of countries with high number of female own-account workers:
Xplot(DATA$OWN, jitter=TRUE, at=pretty(DATA$OWN))
sort(DATA$OWN)
hist(DATA$OWN)
DATA$OWNR<-calibrate(DATA$OWN, type="fuzzy", thresholds="e=5000, c=51000, i=80000", logistic = TRUE)
skew.check(DATA$OWNR)
plot(DATA$OWN,DATA$OWNR, main="Calibration of Own-Account Workers", xlab="Raw Score", ylab="Calibrated Score", abline(h=0.5,v=51000))

## Calibrating female family workers, Set of countries with High Number of Female Family Workers:
Xplot(DATA$FAM, jitter=TRUE, at=pretty(DATA$FAM))
sort(DATA$FAM)
hist(DATA$FAM)
DATA$FAMR<-calibrate(DATA$FAM, type="fuzzy", thresholds="e=2000, c=61000, i=68000", logistic = TRUE)
skew.check(DATA$FAMR)
plot(DATA$FAM,DATA$FAMR, main="Calibration of Female Family Workers", xlab="Raw Score", ylab="Calibrated Score", abline(h=0.5,v=61000))

## Calibrating female employees, Set of countries with high number of female employees:
Xplot(DATA$EMPE, jitter=TRUE, at=pretty(DATA$EMPE))
sort(DATA$EMPE)
hist(DATA$EMPE)
DATA$EMPER<-calibrate(DATA$EMPE, type="fuzzy", thresholds="e=55000, c=260000, i=570000", logistic = TRUE)
skew.check(DATA$EMPER)
plot(DATA$EMPE,DATA$EMPER, main="Calibration of Female Employees", xlab="Raw Score", ylab="Calibrated Score", abline(h=0.5,v=260000))

## Calibrating female employers, Set of countries with a high number of female employers:
Xplot(DATA$EMPR,jitter=TRUE, at=pretty(DATA$EMPR))
sort(DATA$EMPR)
hist(DATA$EMPR)
DATA$EMPRR<-calibrate(DATA$EMPR, type="fuzzy", thresholds="e=3200, c=12000, i=24000", logistic = TRUE)
skew.check(DATA$EMPRR)
plot(DATA$EMPR,DATA$EMPRR, main="Calibration of Female Employers", xlab="Raw Score", ylab="Calibrated Score", abline(h=0.5,v=12000))

#Creating a macro-condition: Set of high number of women employed at the national economy
#data$HNWE<-with(data,fuzzyor(OWNR, FAMR, EMPER, EMPRR))
#print(data[ ,c("Observations", "OWNR", "FAMR", "EMPER", "EMPRR", "HNWE")])
DATA$HNWEB<-with(DATA,fuzzyand(OWNR, FAMR, EMPER, EMPRR))
skew.check(DATA$HNWEB)
print(DATA[ ,c("Observations", "OWNR", "FAMR", "EMPER", "EMPRR", "HNWEB")])

## To check for presence of necessary conditions and presence of outcome(HNWEB):
QCAfit(DATA$MFISR, DATA$HNWEB, cond.lab="Presence of MFIs", necessity = TRUE, consH = TRUE)
QCAfit(DATA$GDPR, DATA$HNWEB, cond.lab="High GDP", necessity = TRUE, consH = TRUE)
QCAfit(DATA$DEMC, DATA$HNWEB, cond.lab="High DEMC", necessity = TRUE, consH = TRUE)
QCAfit(DATA$DPCL, DATA$HNWEB, cond.lab="High DPCL", necessity = TRUE, consH = TRUE)
QCAfit(DATA$LTRCYR, DATA$HNWEB, cond.lab="High LTRCYR", necessity = TRUE, consH = TRUE)
QCAfit(DATA$LIFEXFR, DATA$HNWEB, cond.lab="High LIFEXFR", necessity = TRUE, consH = TRUE)
QCAfit(DATA$NCORR, DATA$HNWEB, cond.lab="High NCORR", necessity = TRUE, consH = TRUE)
QCAfit(DATA$NEFFECTR, DATA$HNWEB, cond.lab="High NEFFECT", necessity = TRUE, consH = TRUE)
QCAfit(DATA$FRPR, DATA$HNWEB, cond.lab="High FRPR", necessity = TRUE, consH = TRUE)
QCAfit(DATA$HRTR, DATA$HNWEB, cond.lab="High HRTR", necessity = TRUE, consH = TRUE)

## To check for absence of necessary conditions and presence of outcome(HNWEB):
QCAfit(1-DATA$MFISR, DATA$HNWEB, cond.lab="Absence of MFIs", necessity = TRUE, consH = TRUE)
QCAfit(1-DATA$GDPR, DATA$HNWEB, cond.lab="Absence ofHigh GDP", necessity = TRUE, consH = TRUE)
QCAfit(1-DATA$DEMC, DATA$HNWEB, cond.lab="Absence of High DEMC", necessity = TRUE, consH = TRUE)
QCAfit(1-DATA$DPCL, DATA$HNWEB, cond.lab="Absence of High DPCL", necessity = TRUE, consH = TRUE)
QCAfit(1-DATA$LTRCYR, DATA$HNWEB, cond.lab="Absence of High LTRCYR", necessity = TRUE, consH = TRUE)
QCAfit(1-DATA$LIFEXFR, DATA$HNWEB, cond.lab="Absence of High LIFEXFR", necessity = TRUE, consH = TRUE)
QCAfit(1-DATA$NCORR, DATA$HNWEB, cond.lab="Absence of High NCORR", necessity = TRUE, consH = TRUE)
QCAfit(1-DATA$NEFFECTR, DATA$HNWEB, cond.lab="Absence of High NEFFECT", necessity = TRUE, consH = TRUE)
QCAfit(1-DATA$FRPR, DATA$HNWEB, cond.lab="Absence of High FRPR", necessity = TRUE, consH = TRUE)
QCAfit(1-DATA$HRTR, DATA$HNWEB, cond.lab="Absence of High HRTR", necessity = TRUE, consH = TRUE)

## Creating a new data set:
write.csv(DATA, "DATAB.csv")
DATAB<-read.csv("DATAB.csv")
PANELB<-DATAB %>% select(X, Observations, MFISR, GDPR, DEMC, DPCL, LTRCYR, LIFEXFR, NCORR, NEFFECTR, FRPR, HRTR, HNWEB, COUNTRY, YEAR)

##Truth Table for HNWEB:
ttEMP<-truthTable(data=PANELB, outcome="HNWEB", 
                  conditions= c("MFISR", "GDPR", "DEMC", "DPCL", "LTRCYR", "LIFEXFR", "NCORR", "NEFFECTR", "FRPR", "HRTR"), 
                  incl.cut = 0.85, 
                  sort.by = "incl,n", pri.cut=0.51, dcc= TRUE, decreasing=FALSE,complete=TRUE, show.cases = TRUE)
ttEMP

ttemp<-truthTable(data=PANELB, outcome="~HNWEB", 
                  conditions= c("MFISR", "GDPR", "DEMC", "DPCL", "LTRCYR", "LIFEXFR", "NCORR", "NEFFECTR", "FRPR", "HRTR"), 
                  incl.cut = 0.85, 
                  sort.by = "incl,n", pri.cut=0.51, dcc= TRUE, decreasing=FALSE,complete=TRUE, show.cases = TRUE)
ttemp

## Boolean Minimization for truth table:
sol_EMPB<-minimize(input=ttEMP, details=TRUE, row.dom=TRUE)
sol_EMPB
sol_empb<-minimize(input=ttemp, details=TRUE, row.dom=TRUE)
sol_empb


##For the conservative, parsimonious, and intermediate solutions:
sol_CEMP <- minimize(ttEMP, details=TRUE, row.dom=TRUE)
sol_CEMP
sol_PEMP <-minimize(ttEMP, include="?", details=TRUE, row.dom=TRUE)
sol_PEMP
sol_IEMP <- minimize(ttEMP, include = "?", dir.exp = "1, 1, 1, 1, 1, 1, 1, 1, 1, 1",details=TRUE, row.dom=TRUE)
sol_IEMP

# For the conservative, parsimonious, and intermediate solutions of the sufficiency result, absence:
sol_cemp <- minimize(ttemp, details=TRUE, row.dom=TRUE)
sol_cemp
sol_pemp<-minimize(ttemp, include="?", details=TRUE, row.dom=TRUE)
sol_pemp
sol_iemp <- minimize(ttemp, include = "?", dir.exp = "0, 0, 0, 0, 0, 0, 0, 0, 0, 0",details=TRUE, row.dom=TRUE)
sol_iemp


## For identifying errors, using the findRows function:
findRows(obj=ttEMP, type=0)
TYPETWO<-findRows(obj=ttEMP, type=2)
TYPETWO
findRows(obj=ttEMP, type=3)

## For the enhanced conservative, parsimonious, and intermediate solutions:
sol_CEMPN<-minimize(ttEMP, details=TRUE, exclude=c(TYPETWO))
sol_CEMPN
sol_PEMPN<-minimize(ttEMP, include="?", details=TRUE, exclude=c(TYPETWO))
sol_PEMPN
sol_IEMPN<-minimize(ttEMP, include = "?", dir.exp = "1, 1, 1, 1, 1, 1, 1, 1, 1, 1",details=TRUE, exclude=c(TYPETWO))
sol_IEMPN

## Visualizing the enhanced intermediate solution:
PANELB$Observations<-NULL
PANELB$COUNTRY<-NULL
PANELB$YEAR<-NULL
PANELB$X<-NULL
XYplot(MFISR*GDPR*DEMC*DPCL*LTRCYR*NEFFECTR*FRPR*HRTR, HNWEB, data=PANELB, relation = "sufficiency", enhance = TRUE, jitter=TRUE, clabels = seq(nrow(PANELB)))

## Visualizing MFISR as a necessary condition for HNWEB:
XYplot(MFISR, HNWEB, data=PANELB, relation = "necessity", enhance = TRUE, jitter=TRUE, clabels = seq(nrow(PANELB)))

-------------------------------------------------------------------------------------------------------------------
## Model 3:
  
##Downloading and opening dataset_1999_2007:
data_1999_2007<-read.csv("FinalData_1999_2007.csv")
DATAC<-as.data.frame(data_1999_2007)
str(DATAC)
head(DATAC)
summary(DATAC)

### To add row names:
pnames <- c("BIH99",
             "HRV99",
             "MNE99",
             "BIH07",
             "HRV07",
             "MNE07")
row.names(DATAC) <- pnames

## Calibrating Presence of MFIS: (Set: Presence of MFIS)
str(DATAC$PresenceofMFIs)
Xplot(DATAC$PresenceofMFIs, jitter=TRUE)
DATAC$MFISR<-calibrate(DATAC$PresenceofMFIs, type="crisp", thresholds=1)
skew.check(DATAC$MFISR)

## Calibrating International Human Rights: (Set of countries with high number of international human rights treaties)
hist(DATAC$INTHRT)
str(DATAC$INTHRT)
sort(DATAC$INTHRT)
Xplot(DATAC$INTHRT, jitter=TRUE)
DATAC$HRTR<-calibrate(DATAC$INTHRT, thresholds="e=0, c=11.5, i=14", logistic=TRUE)
skew.check(DATAC$HRTR)
print(DATAC[ ,c("INTHRT","HRTR")])

# Calibrating GDP: Set of countries with high GDP
print(DATAC$GDP)
sort(DATAC$GDP)
hist(DATAC$GDP)
Xplot(DATAC$GDP, jitter=TRUE)
DATAC$GDPR<-calibrate(DATAC$GDP, thresholds="e=3500000000, c=20000000000, i=60000000000", logistic=TRUE)
skew.check(DATAC$GDPR)
plot(DATAC$GDP, DATAC$GDPR, main="Calibration of GDP", xlab="Raw Score", ylab="Calibrated Score", abline(h=0.5, v=20000000000))


# Calibrating Democracy, Set of highly democratic countries: "autocracies" (-10 to -6), "anocracies" (-5 to +5), 
# three special values: -66, -77 and -88), and "democracies" (+6 to +10)
print(DATAC$DEM)
str(DATAC$DEM)
hist(DATAC$DEM)
Xplot(DATAC$DEM, jitter=TRUE)
DATAC$DEMC<-calibrate(DATAC$DEM, type="fuzzy", thresholds="e=-66, c=1, i=9", logistic=TRUE)
skew.check(DATAC$DEMC)
print(DATAC[ ,c("DEM","DEMC")])
plot(DATAC$DEM, DATAC$DEMC, main="Calibration of DEM", xlab="Raw Score", ylab="Calibrated Score", abline(h=0.5, v=1))


# Calibrating PCL: Set of countries with high political and civil liberty
# free(1.0-2.5), partly free (3.0-5.0), not free (5.5-7.0)
#ggplot(data, aes(x=YEAR,y=PCL, group=COUNTRY)) + geom_line(aes(col=COUNTRY)) + theme_light()
print(DATAC$PCL)
sort(DATAC$PCL)
Xplot(DATAC$PCL, jitter=TRUE)
hist(DATAC$PCL)
DATAC$DPCL<-calibrate(DATAC$PCL, type="fuzzy", thresholds="i=2.0, c=3.2, e=5.0", logistic = TRUE)
skew.check(DATAC$DPCL)
print(DATAC[ ,c("PCL","DPCL")])
plot(DATAC$PCL, DATAC$DPCL, main="Calibration of PCL", xlab="Raw Score", ylab="Calibrated Score", abline(h=0.5, v=3.2))


##Calibrating Percentage of Literate Females, Set of countries with high percentage of literate females:
print(DATAC[ ,c("Observations","LTRCYF")])
sort(DATAC$LTRCYF)
Xplot(DATAC$LTRCYF, jitter=TRUE)
hist(DATAC$LTRCYF)
DATAC$LTRCYR<-calibrate(DATAC$LTRCYF, type="fuzzy", thresholds="e=98.70, c=99.62, i=99.80", logistic = TRUE)
skew.check(DATAC$LTRCYR)
print(DATA[ ,c("Observations","LTRCYF", "LTRCYR")])
plot(DATAC$LTRCYF, DATAC$LTRCYR, main="Calibration of LTRCY", xlab="Raw Score", ylab="Calibrated Score", abline(h=0.5, v=99.62))

##Calibrating Life Expectancy for Females, Set of countries with high life expectancy for females:
print(DATAC[ ,c("Observations","LIFEXF")])
sort(DATAC$LIFEXF)
Xplot(DATAC$LIFEXF, jitter=TRUE)
hist(DATAC$LIFEXF)
DATAC$LIFEXFR<-calibrate(DATAC$LIFEXF, type="fuzzy", thresholds="e=76.00, c=77.00, i=79.00", logistic = TRUE)
skew.check(DATAC$LIFEXFR)
print(DATAC[ ,c("Observations","LIFEXF", "LIFEXFR")])
plot(DATAC$LIFEXF,DATAC$LIFEXFR, main="Calibration of LIFEXF", xlab="Raw Score", ylab="Calibrated Score", abline (h=0.5,v=77.00))

## Calibrating access to high transparency guarantees:
# Control_of_Corruption, Set of high control of corruption:
# Between the years 1996-2001, data was collected for every alternate year.
# Hence, I have recoded 1999 with the values of the year before.
# Between the years 2002-2015, data is available for every year
# Due to the presence of missing values (NAs), R has characterized this variable 
# as a character variable. It needs to be a numeric variable first before recoding.

print(DATAC[ ,c("Observations","FCOR")])
DATAC$NCOR<-as.numeric(DATAC$FCOR)
DATAC$NCOR[which(DATAC$COUNTRY=="BiH" & DATAC$YEAR %in% c(1999))]<-42.783499999999997
DATAC$NCOR[which(DATAC$COUNTRY=="HRV" & DATAC$YEAR %in% c(1999))]<-34.536079999999998
DATAC$NCOR[which(DATAC$COUNTRY=="MNE" & DATAC$YEAR %in% c(1999))]<-71.134020000000007
print(DATAC[ ,c("Observations","FCOR", "NCOR")])
print(DATAC$NCOR)
Xplot(DATAC$NCOR,jitter=TRUE,at=pretty(DATAC$NCOR))
hist(DATAC$NCOR)
sort(DATAC$NCOR)
DATAC$NCORR<-calibrate(DATAC$NCOR, type="fuzzy", thresholds="e=34.0, c=52.0, i=71.0", logistic = TRUE)
skew.check(DATAC$NCORR)
print(DATAC[ ,c("Observations","NCOR", "NCORR")])

# Condition: FEFFECT, Set of countries with high government effectiveness
# Government effectiveness 
# The data is in percentage format
# Between the years 1996-2001, data was collected for every alternate year.
# Hence, I have recoded 1999 with the values of the year before.
# Between the years 2002-2015, data is available for every year
# Due to the presence of missing values (NAs), R has characterized the variable 
# as a character variable. It needs to be a numeric variable first before recoding.

print(DATAC[ ,c("Observations","FEFFECT")])
DATAC$NEFFECT<- as.numeric(DATAC$FEFFECT)
DATAC$NEFFECT[which(DATAC$COUNTRY=="BiH" & DATAC$YEAR %in% c(1999))]<-11.398960000000001
DATAC$NEFFECT[which(DATAC$COUNTRY=="HRV" & DATAC$YEAR %in% c(1999))]<-59.58549
DATAC$NEFFECT[which(DATAC$COUNTRY=="MNE" & DATAC$YEAR %in% c(1999))]<-0
print(DATAC$NEFFECT)
sort(DATAC$NEFFECT)
Xplot(DATAC$NEFFECT, jitter=TRUE)
hist(DATAC$NEFFECT)
DATAC$NEFFECTR<-calibrate(DATAC$NEFFECT, type="fuzzy", thresholds="e=0.00, c=50.0, i=68.0", logistic = TRUE)
skew.check(DATAC$NEFFECTR)
plot(DATAC$NEFFECT,DATAC$NEFFECTR, main="Calibration of Government effectiveness", xlab="Raw Score", ylab="Calibrated Score", abline(h=0.5,v=50.0))
print(DATAC[ ,c("Observations", "NEFFECT", "NEFFECTR")])


## Calibrating Female Representation in Parliament; Set of Countries with High Female Representation
print(DATAC[ ,c("Observations","FPARL")])
DATAC$FPARL[which(DATAC$COUNTRY=="BiH" & DATAC$YEAR %in% c(1999))]<-0
DATAC$FPARL[which(DATAC$COUNTRY=="MNE" & DATAC$YEAR %in% c(1999))]<-0
Xplot(DATAC$FPARL, jitter=TRUE, at=pretty(DATAC$FPARL))
hist(DATAC$FPARL)
sort(DATAC$FPARL)
DATAC$FRPR<-calibrate(DATAC$FPARL, type="fuzzy", thresholds="e=0.0, c=13.0, i=20.0", logistic=TRUE)
skew.check(DATAC$FRPR)
plot(DATAC$NEFFECT,DATAC$NEFFECTR, main="Calibration of Female Representation in Parliament", xlab="Raw Score", ylab="Calibrated Score", abline(h=0.5,v=13.0))
print(DATAC[ ,c("Observations","FPARL", "FRPR")])

# Calibrating Female Own-Account Workers, Set of countries with high number of female own-account workers:
Xplot(DATAC$OWN, jitter=TRUE, at=pretty(DATAC$OWN))
sort(DATAC$OWN)
hist(DATAC$OWN)
DATAC$OWNR<-calibrate(DATAC$OWN, type="fuzzy", thresholds="e=5000, c=60000, i=96000", logistic = TRUE)
skew.check(DATAC$OWNR)
plot(DATAC$OWN,DATAC$OWNR, main="Calibration of Own-Account Workers", xlab="Raw Score", ylab="Calibrated Score", abline(h=0.5,v=60000))

## Calibrating female family workers, Set of countries with High Number of Female Family Workers:
Xplot(DATAC$FAM, jitter=TRUE, at=pretty(DATAC$FAM))
sort(DATAC$FAM)
hist(DATAC$FAM)
DATAC$FAMR<-calibrate(DATAC$FAM, type="fuzzy", thresholds="e=2000, c=40000, i=68000", logistic = TRUE)
skew.check(DATAC$FAMR)
plot(DATAC$FAM,DATAC$FAMR, main="Calibration of Female Family Workers", xlab="Raw Score", ylab="Calibrated Score", abline(h=0.5,v=40000))

## Calibrating female employees, Set of countries with high number of female employees:
Xplot(DATAC$EMPE, jitter=TRUE, at=pretty(DATAC$EMPE))
sort(DATAC$EMPE)
hist(DATAC$EMPE)
DATAC$EMPER<-calibrate(DATAC$EMPE, type="fuzzy", thresholds="e=55000, c=300000, i=620000", logistic = TRUE)
skew.check(DATAC$EMPER)
plot(DATAC$EMPE,DATAC$EMPER, main="Calibration of Female Employees", xlab="Raw Score", ylab="Calibrated Score", abline(h=0.5,v=300000))

## Calibrating female employers, Set of countries with a high number of female employers:
Xplot(DATAC$EMPR,jitter=TRUE, at=pretty(DATAC$EMPR))
sort(DATAC$EMPR)
hist(DATAC$EMPR)
DATAC$EMPRR<-calibrate(DATAC$EMPR, type="fuzzy", thresholds="e=3200, c=12000, i=24000", logistic = TRUE)
skew.check(DATAC$EMPRR)
plot(DATAC$EMPR,DATAC$EMPRR, main="Calibration of Female Employers", xlab="Raw Score", ylab="Calibrated Score", abline(h=0.5,v=12000))

#Creating a macro-condition: Set of high number of women employed at the national economy
#data$HNWE<-with(data,fuzzyor(OWNR, FAMR, EMPER, EMPRR))
#print(data[ ,c("Observations", "OWNR", "FAMR", "EMPER", "EMPRR", "HNWE")])
DATAC$HNWEB<-with(DATAC,fuzzyand(OWNR, FAMR, EMPER, EMPRR))
skew.check(DATAC$HNWEB)
print(DATAC[ ,c("Observations", "OWNR", "FAMR", "EMPER", "EMPRR", "HNWEB")])

## To check for presence of necessary conditions and presence of outcome(HNWEB):
QCAfit(DATAC$MFISR, DATAC$HNWEB, cond.lab="Presence of MFIs", necessity = TRUE, consH = TRUE)
QCAfit(DATAC$GDPR, DATAC$HNWEB, cond.lab="High GDP", necessity = TRUE, consH = TRUE)
QCAfit(DATAC$DEMC, DATAC$HNWEB, cond.lab="High DEMC", necessity = TRUE, consH = TRUE)
QCAfit(DATAC$DPCL, DATAC$HNWEB, cond.lab="High DPCL", necessity = TRUE, consH = TRUE)
QCAfit(DATAC$LTRCYR, DATAC$HNWEB, cond.lab="High LTRCYR", necessity = TRUE, consH = TRUE)
QCAfit(DATAC$LIFEXFR, DATAC$HNWEB, cond.lab="High LIFEXFR", necessity = TRUE, consH = TRUE)
QCAfit(DATAC$NCORR, DATAC$HNWEB, cond.lab="High NCORR", necessity = TRUE, consH = TRUE)
QCAfit(DATAC$NEFFECTR, DATAC$HNWEB, cond.lab="High NEFFECT", necessity = TRUE, consH = TRUE)
QCAfit(DATAC$FRPR, DATAC$HNWEB, cond.lab="High FRPR", necessity = TRUE, consH = TRUE)
QCAfit(DATAC$HRTR, DATAC$HNWEB, cond.lab="High HRTR", necessity = TRUE, consH = TRUE)

## To check for absence of necessary conditions and presence of outcome(HNWEB):
QCAfit(1-DATAC$MFISR, DATAC$HNWEB, cond.lab="Absence of MFIs", necessity = TRUE, consH = TRUE)
QCAfit(1-DATAC$GDPR, DATAC$HNWEB, cond.lab="Absence ofHigh GDP", necessity = TRUE, consH = TRUE)
QCAfit(1-DATAC$DEMC, DATAC$HNWEB, cond.lab="Absence of High DEMC", necessity = TRUE, consH = TRUE)
QCAfit(1-DATAC$DPCL, DATAC$HNWEB, cond.lab="Absence of High DPCL", necessity = TRUE, consH = TRUE)
QCAfit(1-DATAC$LTRCYR, DATAC$HNWEB, cond.lab="Absence of High LTRCYR", necessity = TRUE, consH = TRUE)
QCAfit(1-DATAC$LIFEXFR, DATAC$HNWEB, cond.lab="Absence of High LIFEXFR", necessity = TRUE, consH = TRUE)
QCAfit(1-DATAC$NCORR, DATAC$HNWEB, cond.lab="Absence of High NCORR", necessity = TRUE, consH = TRUE)
QCAfit(1-DATAC$NEFFECTR, DATAC$HNWEB, cond.lab="Absence of High NEFFECT", necessity = TRUE, consH = TRUE)
QCAfit(1-DATAC$FRPR, DATAC$HNWEB, cond.lab="Absence of High FRPR", necessity = TRUE, consH = TRUE)
QCAfit(1-DATAC$HRTR, DATAC$HNWEB, cond.lab="Absence of High HRTR", necessity = TRUE, consH = TRUE)

## Creating a new data set:
write.csv(DATAC, "DATAC.csv")
PANELC<-DATAC %>% select(Observations, MFISR, GDPR, DEMC, DPCL, LTRCYR, LIFEXFR, NCORR, NEFFECTR, FRPR, HRTR, HNWEB, COUNTRY, YEAR)
write.csv(PANELC, "PANELC.csv")

##Truth Table for HNWEB:
ttEMPC<-truthTable(data=PANELC, outcome="HNWEB", 
                  conditions= c("MFISR", "GDPR", "DEMC", "DPCL", "LTRCYR", "LIFEXFR", "NCORR", "NEFFECTR", "FRPR", "HRTR"), 
                  incl.cut = 0.85, 
                  sort.by = "incl,n", pri.cut=0.51, dcc= TRUE, decreasing=FALSE,complete=TRUE, show.cases = TRUE)
ttEMPC

ttempc<-truthTable(data=PANELC, outcome="~HNWEB", 
                  conditions= c("MFISR", "GDPR", "DEMC", "DPCL", "LTRCYR", "LIFEXFR", "NCORR", "NEFFECTR", "FRPR", "HRTR"), 
                  incl.cut = 0.85, 
                  sort.by = "incl,n", pri.cut=0.51, dcc= TRUE, decreasing=FALSE,complete=TRUE, show.cases = TRUE)
ttempc

## Boolean Minimization for truth table:
sol_EMPC<-minimize(input=ttEMPC, details=TRUE, row.dom=TRUE)
sol_EMPC
sol_empc<-minimize(input=ttempc, details=TRUE, row.dom=TRUE)
sol_empc


##For the conservative, parsimonious, and intermediate solutions:
sol_CEMPC <- minimize(ttEMPC, details=TRUE, row.dom=TRUE)
sol_CEMPC
sol_PEMPC <-minimize(ttEMPC, include="?", details=TRUE, row.dom=TRUE)
sol_PEMPC
sol_IEMPC <- minimize(ttEMPC, include = "?", dir.exp = "1, 1, 1, 1, 1, 1, 1, 1, 1, 1",details=TRUE, row.dom=TRUE)
sol_IEMPC

# For the conservative, parsimonious, and intermediate solutions of the sufficiency result, absence:
sol_cempc <- minimize(ttempc, details=TRUE, row.dom=TRUE)
sol_cempc
sol_pempc<-minimize(ttempc, include="?", details=TRUE, row.dom=TRUE)
sol_pempc
sol_iempc <- minimize(ttempc, include = "?", dir.exp = "0, 0, 0, 0, 0, 0, 0, 0, 0, 0",details=TRUE, row.dom=TRUE)
sol_iempc


## For identifying errors, using the findRows function:
findRows(obj=ttEMPC, type=0)
TYPETWOC<-findRows(obj=ttEMPC, type=2)
TYPETWOC
findRows(obj=ttEMPC, type=3)

## For the enhanced conservative, parsimonious, and intermediate solutions:
sol_CEMPNC<-minimize(ttEMPC, details=TRUE, exclude=c(TYPETWOC))
sol_CEMPNC
sol_PEMPNC<-minimize(ttEMPC, include="?", details=TRUE, exclude=c(TYPETWOC))
sol_PEMPNC
sol_IEMPNC<-minimize(ttEMPC, include = "?", dir.exp = "1, 1, 1, 1, 1, 1, 1, 1, 1, 1",details=TRUE, exclude=c(TYPETWOC))
sol_IEMPNC

## Visualizing the enhanced intermediate solution:
PANELC$Observations<-NULL
PANELC$COUNTRY<-NULL
PANELC$YEAR<-NULL
XYplot("MFISR*GDPR*~DEMC*~DPCL*~LTRCYR*~LIFEXFR*NEFFECTR*~FRPR", HNWEB, data=PANELC, relation = "sufficiency", enhance = TRUE, jitter=TRUE, clabels = seq(nrow(PANELC)))
XYplot("MFISR*GDPR*~DPCL*~LTRCYR*~LIFEXFR*~NCORR*NEFFECTR*~FRPR", HNWEB, data=PANELC, relation = "sufficiency", enhance = TRUE, jitter=TRUE, clabels = seq(nrow(PANELC)))

## Visualizing MFISR as a necessary condition for HNWEB:
XYplot(MFISR, HNWEB, data=PANELC, relation = "necessity", enhance = TRUE, jitter=TRUE, clabels = seq(nrow(PANELC)))

------------------------------------------------------------------------------------------------------------------------------------

## Chapter 7: 
##Downloading and opening dataset:
data<-read.csv("Final_Panel_data.csv")
str(data)
head(data)
summary(data)

### To add row names:
pnames <- c("BIH99",
            "HRV99",
            "MNE99",
            "BIH00",
            "HRV00",
            "MNE00",
            "BIH01",
            "HRV01",
            "MNE01",
            "BIH02",
            "HRV02",
            "MNE02",
            "BIH03",
            "HRV03",
            "MNE03",
            "BIH04",
            "HRV04",
            "MNE04",
            "BIH05",
            "HRV05",
            "MNE05",
            "BIH06",
            "HRV06",
            "MNE06",
            "BIH07",
            "HRV07",
            "MNE07")
data <- as.data.frame(data)
row.names(data) <- pnames

## Calibrating Presence of MFIS: (Set: Presence of MFIS)
str(data$PresenceofMFIs)
Xplot(data$PresenceofMFIs, jitter=TRUE)
data$MFISR<-calibrate(data$PresenceofMFIs, type="crisp", thresholds=1)
skew.check(data$MFISR)

## Calibrating International Human Rights: (Set of countries with high number of international human rights treaties)
hist(data$INTHRT)
str(data$INTHRT)
sort(data$INTHRT)
Xplot(data$INTHRT, jitter=TRUE)
data$HRTR<-calibrate(data$INTHRT, type="fuzzy", thresholds="e=0, c=10.5, i=14", logistic=TRUE)
skew.check(data$HRTR)
print(data[ ,c("INTHRT","HRTR")])
plot(data$INTHRT, data$HRTR, main="Calibration of HRT", xlab="Raw Score", ylab="Calibrated Score", abline(h=0.5, v=10.5))

# Calibrating GDP: Set of countries with high GDP
ggplot(data, aes(x=YEAR,y=GDP,group=COUNTRY))+geom_line(aes(col=COUNTRY)) + theme_light()
print(data$GDP)
sort(data$GDP)
hist(data$GDP)
Xplot(data$GDP, jitter=TRUE)
data$GDPR<-calibrate(data$GDP, thresholds="e=980000000, c=30000000000, i=50000000000", logistic=TRUE)
skew.check(data$GDPR)
plot(data$GDP, data$GDPR, main="Calibration of GDP", xlab="Raw Score", ylab="Calibrated Score", abline(h=0.5, v=30000000000))


# Calibrating Democracy, Set of highly democratic countries: "autocracies" (-10 to -6), "anocracies" (-5 to +5), 
# three special values: -66, -77 and -88), and "democracies" (+6 to +10)
ggplot(data, aes(x=YEAR,y=DEM,group=COUNTRY))+geom_line()+facet_wrap(.~COUNTRY) + theme_light()
print(data$DEM)
str(data$DEM)
hist(data$DEM)
Xplot(data$DEM, jitter=TRUE)
data$DEMC<-calibrate(data$DEM, thresholds="e=-67,c=5.3,i=7", logistic=TRUE)
skew.check(data$DEMC)
print(data[ ,c("DEM","DEMC")])
plot(data$DEM,data$DEMC, main="Calibration of DEM", xlab="Raw Score", ylab="Calibrated Score", abline (h=0.5,v=5.3))

# Calibrating PCL: Set of countries with high political and civil liberty
# free(1.0-2.5), partly free (3.0-5.0), not free (5.5-7.0)
#ggplot(data, aes(x=YEAR,y=PCL, group=COUNTRY)) + geom_line(aes(col=COUNTRY)) + theme_light()
ggplot(data, aes(x=YEAR,y=PCL, group=COUNTRY)) + geom_line() + facet_wrap(.~COUNTRY) + theme_light()
print(data$PCL)
sort(data$PCL)
Xplot(data$PCL, jitter=TRUE)
hist(data$PCL)
data$DPCL<-calibrate(data$PCL, type="fuzzy", thresholds="i=1.9,c=2.6, e=5.1", logistic = TRUE)
plot(data$PCL,data$DPCL, main="Calibration of PCL", xlab="Raw Score", ylab="Calibrated Score", abline (h=0.5,v=2.6))
skew.check(data$DPCL)
print(data[ ,c("PCL","DPCL")])

##Calibrating Percentage of Literate Females, Set of countries with high percentage of literate females:
print(data[ ,c("Observations","LTRCYF")])
sort(data$LTRCYF)
Xplot(data$LTRCYF, jitter=TRUE)
hist(data$LTRCYF)
data$LTRCYR<-calibrate(data$LTRCYF, type="fuzzy", thresholds="e=98.70,c=99.70,i=99.84", logistic = TRUE)
skew.check(data$LTRCYR)
print(data[ ,c("Observations","LTRCYF", "LTRCYR")])

##Calibrating Life Expectancy for Females, Set of countries with high life expectancy for females:
print(data[ ,c("Observations","LIFEXF")])
sort(data$LIFEXF)
Xplot(data$LIFEXF, jitter=TRUE)
hist(data$LIFEXF)
data$LIFEXFR<-calibrate(data$LIFEXF, type="fuzzy", thresholds="e=76.0,c=77.85,i=79.00", logistic = TRUE)
skew.check(data$LIFEXFR)
print(data[ ,c("Observations","LIFEXF", "LIFEXFR")])
plot(data$LIFEXF,data$LIFEXFR, main="Calibration of LIFEXF", xlab="Raw Score", ylab="Calibrated Score", abline (h=0.5,v=77.85))

## Calibrating access to high transparency guarantees:
# Control_of_Corruption, Set of high control of corruption:
# Researchers started collecting this data from the year 1996
# Between the years 1996-2001, data was collected for every alternate year.
# Hence, I have recoded 1997, 1999, 2001 with the values of the year before.
# Between the years 2002-2015, data is available for every year
# Due to the presence of missing values (NAs), R has characterized this variable 
# as a character variable. It needs to be a numeric variable first before recoding.

print(data[ ,c("Observations","FCOR")])
data$NCOR<-as.numeric(data$FCOR)
data$NCOR[which(data$COUNTRY=="BiH" & data$YEAR %in% c(1995))]<-0
data$NCOR[which(data$COUNTRY=="HRV" & data$YEAR %in% c(1995))]<-0
data$NCOR[which(data$COUNTRY=="MNE" & data$YEAR %in% c(1995))]<-0
data$NCOR[which(data$COUNTRY=="MNE" & data$YEAR %in% c(1996))]<-0
data$NCOR[which(data$COUNTRY=="BiH" & data$YEAR %in% c(1997))]<-48.92473
data$NCOR[which(data$COUNTRY=="HRV" & data$YEAR %in% c(1997))]<-32.79570
data$NCOR[which(data$COUNTRY=="MNE" & data$YEAR %in% c(1997))]<-0
data$NCOR[which(data$COUNTRY=="BiH" & data$YEAR %in% c(1999))]<-42.783499999999997
data$NCOR[which(data$COUNTRY=="HRV" & data$YEAR %in% c(1999))]<-34.536079999999998
data$NCOR[which(data$COUNTRY=="MNE" & data$YEAR %in% c(1999))]<-71.134020000000007
data$NCOR[which(data$COUNTRY=="BiH" & data$YEAR %in% c(2001))]<-33.502540000000003
data$NCOR[which(data$COUNTRY=="HRV" & data$YEAR %in% c(2001))]<-56.345179999999999
data$NCOR[which(data$COUNTRY=="MNE" & data$YEAR %in% c(2001))]<-53.299489999999999
print(data[ ,c("Observations","FCOR", "NCOR")])
print(data$NCOR)
ggplot(data, aes(x=YEAR,y=NCOR, group=COUNTRY)) + geom_line(aes(col=COUNTRY)) + theme_light()
Xplot(data$NCOR,jitter=TRUE,at=pretty(data$NCOR))
hist(data$NCOR)
sort(data$NCOR)
data$NCORR<-calibrate(data$NCOR, type="fuzzy", thresholds="e=33.0, c=54.0, i=62.0", logistic = TRUE)
skew.check(data$NCORR)
print(data[ ,c("Observations","NCOR", "NCORR")])

# Condition: FEFFECT, Set of countries with high government effectiveness
# Government effectiveness 
# The data is in percentage format
# Researchers started collecting this data from the year 1996
# Between the years 1996-2001, data was collected for every alternate year.
# Hence, I have recoded 1997, 1999, 2001 with the values of the year before.
# Between the years 2002-2015, data is available for every year
# Due to the presence of missing values (NAs), R has characterized the variable 
# as a character variable. It needs to be a numeric variable first before recoding.

print(data[ ,c("Observations","FEFFECT")])
data$NEFFECT<- as.numeric(data$FEFFECT)
data$NEFFECT[which(data$COUNTRY=="BiH" & data$YEAR %in% c(1999))]<-11.398960000000001
data$NEFFECT[which(data$COUNTRY=="HRV" & data$YEAR %in% c(1999))]<-59.58549
data$NEFFECT[which(data$COUNTRY=="MNE" & data$YEAR %in% c(1999,2000,2001,2002,2003,2004))]<-0
data$NEFFECT[which(data$COUNTRY=="BiH" & data$YEAR %in% c(2001))]<-17.948720000000002
data$NEFFECT[which(data$COUNTRY=="HRV" & data$YEAR %in% c(2001))]<-66.666659999999993
print(data$NEFFECT)
sort(data$NEFFECT)
ggplot(data, aes(x=YEAR,y=NEFFECT, group=COUNTRY)) + geom_line(aes(col=COUNTRY)) + theme_light()
Xplot(data$NEFFECT, jitter=TRUE)
hist(data$NEFFECT)
data$NEFFECTR<-calibrate(data$NEFFECT, type="fuzzy", thresholds="e=10.0, c=55.0, i=69.0", logistic = TRUE)
skew.check(data$NEFFECTR)
plot(data$NEFFECT,data$NEFFECTR, main="Calibration of Government effectiveness", xlab="Raw Score", ylab="Calibrated Score", abline(h=0.5,v=55.0))
print(data[ ,c("Observations", "NEFFECT", "NEFFECTR")])


## Calibrating Female Representation in Parliament; Set of Countries with High Female Representation
print(data[ ,c("Observations","FPARL")])
data$FPARL[which(data$COUNTRY=="BiH" & data$YEAR %in% c(1999, 2000))]<-0
data$FPARL[which(data$COUNTRY=="BiH" & data$YEAR %in% c(2002))]<-7.142
data$FPARL[which(data$COUNTRY=="HRV" & data$YEAR %in% c(2003))]<-20.520
data$FPARL[which(data$COUNTRY=="MNE" & data$YEAR %in% c(1999, 2000, 2001, 2002, 2003, 2004, 2005))]<-0
Xplot(data$FPARL, jitter=TRUE, at=pretty(data$FPARL))
hist(data$FPARL)
sort(data$FPARL)
data$FRPR<-calibrate(data$FPARL, type="fuzzy", thresholds="e=7.00, c=17.00, i=21.00", logistic = TRUE)
skew.check(data$FRPR)
print(data[ ,c("Observations","FPARL", "FRPR")])
plot(data$FPARL,data$FRPR, main="Calibration of FRParl", xlab="Raw Score", ylab="Calibrated Score", abline(h=0.5,v=17.00))

# Calibrating Female Own-Account Workers, Set of countries with high number of female own-account workers:
Xplot(data$OWN, jitter=TRUE, at=pretty(data$OWN))
sort(data$OWN)
hist(data$OWN)
data$OWNR<-calibrate(data$OWN, type="fuzzy", thresholds="e=4000, c=77000, i=110000", logistic = TRUE)
skew.check(data$OWNR)
plot(data$OWN,data$OWNR, main="Calibration of Own-Account Workers", xlab="Raw Score", ylab="Calibrated Score", abline(h=0.5,v=77000))

## Calibrating female family workers, Set of countries with High Number of Female Family Workers:
Xplot(data$FAM, jitter=TRUE, at=pretty(data$FAM))
sort(data$FAM)
hist(data$FAM)
data$FAMR<-calibrate(data$FAM, type="fuzzy", thresholds="e=1700, c=43000, i=60000", logistic = TRUE)
skew.check(data$FAMR)
plot(data$FAM,data$FAMR, main="Calibration of Female Family Workers", xlab="Raw Score", ylab="Calibrated Score", abline(h=0.5,v=43000))

## Calibrating female employees, Set of countries with high number of female employees:
Xplot(data$EMPE, jitter=TRUE, at=pretty(data$EMPE))
sort(data$EMPE)
hist(data$EMPE)
data$EMPER<-calibrate(data$EMPE, type="fuzzy", thresholds="e=55000, c=300000, i=610000", logistic = TRUE)
skew.check(data$EMPER)
plot(data$EMPE,data$EMPER, main="Calibration of Female Employees", xlab="Raw Score", ylab="Calibrated Score", abline(h=0.5,v=300000))

## Calibrating female employers, Set of countries with a high number of female employers:
Xplot(data$EMPR,jitter=TRUE, at=pretty(data$EMPR))
sort(data$EMPR)
hist(data$EMPR)
data$EMPRR<-calibrate(data$EMPR, type="fuzzy", thresholds="e=3000, c=15000, i=25000", logistic = TRUE)
skew.check(data$EMPRR)
plot(data$EMPR,data$EMPRR, main="Calibration of Female Employers", xlab="Raw Score", ylab="Calibrated Score", abline(h=0.5,v=15000))

#Creating a macro-condition: Set of high number of women employed at the national economy
#data$HNWE<-with(data,fuzzyor(OWNR, FAMR, EMPER, EMPRR))
#print(data[ ,c("Observations", "OWNR", "FAMR", "EMPER", "EMPRR", "HNWE")])
skew.check(data$HNWE)
data$HNWEB<-with(data,fuzzyand(OWNR, FAMR, EMPER, EMPRR))
skew.check(data$HNWEB)
print(data[ ,c("Observations", "OWNR", "FAMR", "EMPER", "EMPRR", "HNWEB")])

## Calibrating access to high transparency guarantees:
# Control_of_Corruption, Set of high control of corruption:
# Researchers started collecting this data from the year 1996
# Hence, I have coded the year 1995 as zero
# Between the years 1996-2001, data was collected for every alternate year.
# Hence, I have recoded 1997, 1999, 2001 with the values of the year before.
# Between the years 2002-2015, data is available for every year
# Due to the presence of missing values (NAs), R has characterized this variable 
# as a character variable. It needs to be a numeric variable first before recoding.

print(data[ ,c("Observations","FCOR")])
data$FCOR[which(data$COUNTRY=="BiH" & data$YEAR %in% c(1999))]<-42.783499999999997
data$FCOR[which(data$COUNTRY=="HRV" & data$YEAR %in% c(1999))]<-34.536079999999998
data$FCOR[which(data$COUNTRY=="MNE" & data$YEAR %in% c(1999))]<-71.134020000000007
data$FCOR[which(data$COUNTRY=="BiH" & data$YEAR %in% c(2001))]<-33.502540000000003
data$FCOR[which(data$COUNTRY=="HRV" & data$YEAR %in% c(2001))]<-56.345179999999999
data$FCOR[which(data$COUNTRY=="MNE" & data$YEAR %in% c(2001))]<-53.299489999999999
ggplot(data, aes(x=YEAR,y=FCOR, group=COUNTRY)) + geom_line(aes(col=COUNTRY)) + theme_light()
Xplot(data$FCOR,jitter=TRUE,at=pretty(data$FCOR))
hist(data$FCOR)
sort(data$FCOR)
data$FCORR<-calibrate(data$FCOR, type="fuzzy", thresholds="e=33.0, c=55.0, i=71.0", logistic = TRUE)
skew.check(data$FCORR)
print(data[ ,c("Observations","FCOR", "FCORR")])
plot(data$FCOR,data$FCORR, main="Calibration of Female Employers", xlab="Raw Score", ylab="Calibrated Score", abline(h=0.5,v=55.0))

##To check for presence of necessary conditions: Presence of outcome (HNWEB)
cluster(data = data, results = "MFISR", outcome = "HNWEB",unit_id = "COUNTRY", cluster_id = "YEAR", necessity=TRUE, wicons=TRUE)
cluster(data = data, results = "GDPR", outcome = "HNWEB",unit_id = "COUNTRY", cluster_id = "YEAR", necessity=TRUE, wicons=TRUE)
cluster(data = data, results = "DEMC", outcome = "HNWEB",unit_id = "COUNTRY", cluster_id = "YEAR", necessity=TRUE, wicons=TRUE)
cluster(data = data, results = "DPCL", outcome = "HNWEB",unit_id = "COUNTRY", cluster_id = "YEAR", necessity=TRUE, wicons=TRUE)
cluster(data = data, results = "LTRCYR", outcome = "HNWEB",unit_id = "COUNTRY", cluster_id = "YEAR", necessity=TRUE, wicons=TRUE)
cluster(data = data, results = "LIFEXFR", outcome = "HNWEB",unit_id = "COUNTRY", cluster_id = "YEAR", necessity=TRUE, wicons=TRUE)
cluster(data = data, results = "FCORR", outcome = "HNWEB",unit_id = "COUNTRY", cluster_id = "YEAR", necessity=TRUE, wicons=TRUE)
cluster(data = data, results = "NEFFECTR", outcome = "HNWEB",unit_id = "COUNTRY", cluster_id = "YEAR", necessity=TRUE, wicons=TRUE)
cluster(data = data, results = "FRPR", outcome = "HNWEB",unit_id = "COUNTRY", cluster_id = "YEAR", necessity=TRUE, wicons=TRUE)
cluster(data = data, results = "HRTR", outcome = "HNWEB",unit_id = "COUNTRY", cluster_id = "YEAR", necessity=TRUE, wicons=TRUE)

### Since the consistency and coverage does vary across cases, I have applied two-step QCA

## Creating a new data set for two-step QCA:
TWO_STEP_PANEL<-data %>% select(Observations, MFISR, GDPR, DEMC, DPCL, LTRCYR, LIFEXFR, FCORR, NEFFECTR, FRPR, HRTR, OWNR, FAMR, EMPER, EMPRR, HNWEB, COUNTRY, YEAR)
write.csv(TWO_STEP_PANEL, "TWO_STEP_PANELB.csv")
TWO_STEP_PANEL<-read.csv("TWO_STEP_PANELB.csv")

## Remote Conditions: Given to actors, contextual, originates earlier in time and space, same as the unit of analysis
## Proximate: Closer to the outcome, different from the unit of analysis, Actors have chosen
## Step 1: Identifying GDPR, DEMC, DPCL, FCORR, NEFFECTR, LTRCYR, LIFEXFR, HRTR as remote conditions 
## Step 2: Identifying MFISR,FRPR, as proximate conditions


## Testing remote conditions as necessary SUIN conditions:

SUIN<-superSubset(data=TWO_STEP_PANEL, conditions=c("GDPR", "DEMC", "DPCL", "FCORR", "NEFFECTR", "LTRCYR", "LIFEXFR", "HRTR"),
                    outcome="HNWEB",
                    relation = "necessity",
                    incl.cut = 0.9,
                    cov.cut=0.6,
                    ron.cut=0.5)

## Since, I did not find any necessary SUIN conditions, we will not be able to apply this approach. As an alternative, we can apply the approaches discussed in chapter 5,6, and 8
## But, I have modified the outcome variable to demonstrate how to apply this approach, My new outcome variable is OWNR

##To check for presence of necessary conditions: Presence of outcome (OWNR)
cluster(data = TWO_STEP_PANEL, results = "MFISR", outcome = "OWNR",unit_id = "COUNTRY", cluster_id = "YEAR", necessity=TRUE, wicons=TRUE)
cluster(data = TWO_STEP_PANEL, results = "GDPR", outcome = "OWNR",unit_id = "COUNTRY", cluster_id = "YEAR", necessity=TRUE, wicons=TRUE)
cluster(data = TWO_STEP_PANEL, results = "DEMC", outcome = "OWNR",unit_id = "COUNTRY", cluster_id = "YEAR", necessity=TRUE, wicons=TRUE)
cluster(data = TWO_STEP_PANEL, results = "DPCL", outcome = "OWNR",unit_id = "COUNTRY", cluster_id = "YEAR", necessity=TRUE, wicons=TRUE)
cluster(data = TWO_STEP_PANEL, results = "LTRCYR", outcome = "OWNR",unit_id = "COUNTRY", cluster_id = "YEAR", necessity=TRUE, wicons=TRUE)
cluster(data = TWO_STEP_PANEL, results = "LIFEXFR", outcome = "OWNR",unit_id = "COUNTRY", cluster_id = "YEAR", necessity=TRUE, wicons=TRUE)
cluster(data = TWO_STEP_PANEL, results = "FCORR", outcome = "OWNR",unit_id = "COUNTRY", cluster_id = "YEAR", necessity=TRUE, wicons=TRUE)
cluster(data = TWO_STEP_PANEL, results = "NEFFECTR", outcome = "OWNR",unit_id = "COUNTRY", cluster_id = "YEAR", necessity=TRUE, wicons=TRUE)
cluster(data = TWO_STEP_PANEL, results = "FRPR", outcome = "OWNR",unit_id = "COUNTRY", cluster_id = "YEAR", necessity=TRUE, wicons=TRUE)
cluster(data = TWO_STEP_PANEL, results = "HRTR", outcome = "OWNR",unit_id = "COUNTRY", cluster_id = "YEAR", necessity=TRUE, wicons=TRUE)


SUIN_A<-superSubset(data=TWO_STEP_PANEL, conditions=c("GDPR", "DEMC", "DPCL", "FCORR", "NEFFECTR", "LTRCYR", "LIFEXFR", "HRTR"),
                    outcome="OWNR",
                    relation = "necessity",
                    incl.cut = 0.9,
                    cov.cut=0.6,
                    ron.cut=0.5)

## Checking for deviant consistencies in kind (DCKs):
pimplot(data=TWO_STEP_PANEL,
        results=SUIN_A,
        outcome="OWNR",
        necessity=TRUE,
        jitter=TRUE,
        all_labels = TRUE)

## Checking for skewness:
TWO_STEP_PANEL$GDPLIFE<-fuzzyor(TWO_STEP_PANEL$GDPR, TWO_STEP_PANEL$LIFEXFR)
TWO_STEP_PANEL$GDPHRT<-fuzzyor(TWO_STEP_PANEL$GDPR, TWO_STEP_PANEL$HRTR)
TWO_STEP_PANEL$DPCLNEFFECT<-fuzzyor(TWO_STEP_PANEL$DPCL, TWO_STEP_PANEL$NEFFECTR)
TWO_STEP_PANEL$FCORNEFFECT<-fuzzyor(TWO_STEP_PANEL$FCORR, TWO_STEP_PANEL$NEFFECTR)
TWO_STEP_PANEL$FCORLIFE<-fuzzyor(TWO_STEP_PANEL$FCORR, TWO_STEP_PANEL$LIFEXFR)
TWO_STEP_PANEL$NEFFECTLIFE<-fuzzyor(TWO_STEP_PANEL$NEFFECTR, TWO_STEP_PANEL$LIFEXFR)
TWO_STEP_PANEL$LIFEHRT<-fuzzyor(TWO_STEP_PANEL$LIFEXFR, TWO_STEP_PANEL$HRTR)
skew.check(TWO_STEP_PANEL$GDPLIFE)
skew.check(TWO_STEP_PANEL$GDPHRT)
skew.check(TWO_STEP_PANEL$DPCLNEFFECT)
skew.check(TWO_STEP_PANEL$FCORNEFFECT)
skew.check(TWO_STEP_PANEL$FCORLIFE)
skew.check(TWO_STEP_PANEL$NEFFECTLIFE)
skew.check(TWO_STEP_PANEL$LIFEHRT)


## Adding SUIN conditions to the data set:
#TWO_STEP_PANEL$demc<- 1- TWO_STEP_PANEL$DEMC
#TWO_STEP_PANEL$GDPdem<-fuzzyor(TWO_STEP_PANEL$GDPR,TWO_STEP_PANEL$demc)
#skew.check(TWO_STEP_PANEL$GDPdem)

#TWO_STEP_PANEL$GDPdemc<-fuzzyor(TWO_STEP_PANEL$GDPR, TWO_STEP_PANEL$NCORR)
#TWO_STEP_PANEL$GDPNEFFECT<-fuzzyor(TWO_STEP_PANEL$GDPR, TWO_STEP_PANEL$NEFFECTR)
#skew.check(TWO_STEP_PANEL$GDPNCOR)
#skew.check(TWO_STEP_PANEL$GDPNEFFECT)


##Truth Table for OWNR:
ttOWND<-truthTable(data=TWO_STEP_PANEL, outcome="OWNR", 
                  conditions= c("MFISR", "GDPR", "DEMC", "DPCL", "LTRCYR", "LIFEXFR", "FCORR", "NEFFECTR", "FRPR", "HRTR"), 
                  incl.cut = 0.85, 
                  sort.by = "incl,n", pri.cut=0.51, dcc= TRUE, decreasing=FALSE,complete=TRUE, show.cases = TRUE)
ttOWND

ttownd<-truthTable(data=TWO_STEP_PANEL, outcome="~OWNR", 
                  conditions= c("MFISR", "GDPR", "DEMC", "DPCL", "LTRCYR", "LIFEXFR", "FCORR", "NEFFECTR", "FRPR", "HRTR"), 
                  incl.cut = 0.85, 
                  sort.by = "incl,n", pri.cut=0.51, dcc= TRUE, decreasing=FALSE,complete=TRUE, show.cases = TRUE)
ttownd

## Boolean Minimization for truth table:
sol_OWND<-minimize(input=ttOWND, details=TRUE, row.dom=TRUE)
sol_OWND
sol_ownd<-minimize(input=ttownd, details=TRUE, row.dom=TRUE)
sol_ownd


##For the conservative, parsimonious, and intermediate solutions:
sol_OWNC <- minimize(ttOWND, details=TRUE, row.dom=TRUE)
sol_OWNC
sol_OWNP <-minimize(ttOWND, include="?", details=TRUE, row.dom=TRUE)
sol_OWNP
sol_OWNI <- minimize(ttOWND, include = "?", dir.exp = "1, 1, 1, 1, 1, 1, 1, 1, 1, 1",details=TRUE, row.dom=TRUE)
sol_OWNI

# For the conservative, parsimonious, and intermediate solutions of the sufficiency result, absence:
sol_ownc <- minimize(ttownd, details=TRUE, row.dom=TRUE)
sol_ownc
sol_ownp<-minimize(ttownd, include="?", details=TRUE, row.dom=TRUE)
sol_ownp
sol_owni <- minimize(ttownd, include = "?", dir.exp = "0, 0, 0, 0, 0, 0, 0, 0, 0, 0",details=TRUE, row.dom=TRUE)
sol_owni


## For identifying errors, using the findRows function:
findRows(obj=ttOWND, type=0)
TYPETWO<-findRows(obj=ttOWND, type=2)
TYPETWO
findRows(obj=ttOWND, type=3)

## Identify remainder rows with CSA:
CSAC<-LR.intersect(sol_OWNC, sol_ownc)
CSAC
CSAP<-LR.intersect(sol_OWNP, sol_ownp)
CSAP
CSAI<-LR.intersect(sol_OWNI, sol_owni)
CSAI


## For the enhanced conservative, parsimonious, and intermediate solutions:
sol_COWNC<- minimize(ttOWND, details=TRUE, row.dom = TRUE, exclude=c(TYPETWO))
sol_COWNC
sol_POWNP <- minimize(ttOWND, include="?", details=TRUE, row.dom = TRUE, exclude=c(TYPETWO))
sol_POWNP
sol_IOWNI <- minimize(ttOWND, include="?", details=TRUE, dir.exp = "1, 1, 1, 1, 1, 1, 1, 1, 1, 1", exclude=c(TYPETWO))
sol_IOWNI

### Second way: Testing for remote-proximate conditions as macro-conditions
ttOWNRE<-truthTable(TWO_STEP_PANEL, conditions= "MFISR, GDPLIFE, GDPHRT, DPCLNEFFECT, FCORNEFFECT, FCORLIFE, NEFFECTLIFE, LIFEHRT, FRPR",outcome="OWNR", incl.cut = 0.85, 
                  sort.by = "incl,n", pri.cut=0.51, dcc= TRUE, decreasing=FALSE,complete=TRUE, wicons=TRUE, show.cases = TRUE)
ttOWNRE
ttownre<-truthTable(TWO_STEP_PANEL, conditions= "MFISR, GDPLIFE, GDPHRT, DPCLNEFFECT, FCORNEFFECT, FCORLIFE, NEFFECTLIFE, LIFEHRT, FRPR",outcome="~OWNR", incl.cut = 0.85, 
                  sort.by = "incl,n", pri.cut=0.51, dcc= TRUE, decreasing=FALSE,complete=TRUE, wicons=TRUE, show.cases = TRUE)
ttownre

## Boolean Minimization for truth table 
sol_OWNE<-minimize(input=ttOWNRE, details=TRUE, row.dom=TRUE)
sol_OWNE
sol_owne<-minimize(input=ttownre, details=TRUE, row.dom=TRUE)
sol_owne

##For the conservative, parsimonious, and intermediate solutions, with the option of row.dom, to remove prime implicants:
sol_CONSE <- minimize(ttOWNRE, details=TRUE, row.dom=TRUE)
sol_CONSE
sol_PARSE<-minimize(ttOWNRE, include="?", details=TRUE, row.dom=TRUE)
sol_PARSE
sol_INTSE<- minimize(ttOWNRE, include = "?", dir.exp = "1, 1, 1, 1, 1, 1, 1, 1, 1",details=TRUE, row.dom = TRUE)
sol_INTSE


## For the conservative, parsimonious, and intermediate solutions of the sufficiency result, absence:
sol_conse <- minimize(ttownre, details=TRUE, row.dom=TRUE)
sol_conse
sol_parse<-minimize(ttownre, include="?", details=TRUE, row.dom=TRUE)
sol_parse
sol_intse<- minimize(ttownre, include = "?", dir.exp = "1, 1, 1, 1, 1, 1, 1, 1, 1",details=TRUE, row.dom = TRUE)
sol_intse


## For identifying errors:
findRows(obj=ttOWNRE, type=0)
TYPE_TWOB<-findRows(obj=ttOWNRE, type=2)
TYPE_TWOB
findRows(obj=ttOWNRE, type=3)


## For creating the enhanced conservative, parsimonious, and intermediate solutions:
sol_CS<-minimize(ttOWNRE, details=TRUE, exclude=c(TYPE_TWOB))
sol_CS
sol_PS<-minimize(ttOWNRE, include="?", details=TRUE, exclude=c(TYPE_TWOB))
sol_PS
sol_IS<-minimize(ttOWNRE, include = "?", dir.exp = "1, 1, 1, 1, 1, 1, 1,1,1",details=TRUE, exclude=c(TYPE_TWOB))
sol_IS

### Visualizing the enhanced intermediate solutions:
TWO_STEP_PANEL$Observations<-NULL
TWO_STEP_PANEL$COUNTRY<-NULL
TWO_STEP_PANEL$YEAR<-NULL
TWO_STEP_PANEL$X<-NULL
## First approach:
XYplot("MFISR*~DEMC*~DPCL*NEFFECTR", OWNR, data=TWO_STEP_PANEL, relation = "sufficiency", enhance = TRUE, jitter=TRUE, clabels = seq(nrow(TWO_STEP_PANEL)))
##Second approach:
XYplot("MFISR*GDPLIFE*DPCLNEFFECT*FCORNEFFECT*FCORLIFE*NEFFECTLIFE*LIFEHRT*FRPR", OWNR, data=TWO_STEP_PANEL, relation = "sufficiency", enhance = TRUE, jitter=TRUE, clabels = seq(nrow(TWO_STEP_PANEL)))

----------------------------------------------------------------------------------------------------------------------------
## Chapter 8:  

### Running packages:
library("readxl")
library("dplyr")
library("ggplot2")
library("QCA")
library("SetMethods")
library("readr")

##Downloading and opening dataset:
data_1999_2008<-read.csv("FinalData_1999_2008.csv")
DATA<-as.data.frame(data_1999_2008)
str(DATA)
head(DATA)
summary(DATA)
DATA$COUNTRY<-NULL
DATA$YEAR<-NULL


## Calibrating Presence of MFIS: (Set: Presence of MFIS)
str(DATA$PresenceofMFIs_1999)
Xplot(DATA$PresenceofMFIs_1999, jitter=TRUE)
DATA$MFISR<-calibrate(DATA$PresenceofMFIs_1999, type="crisp", thresholds=1)
skew.check(DATA$MFISR)
str(DATA$PresenceofMFIs_2008)
Xplot(DATA$PresenceofMFIs_2008, jitter=TRUE)
DATA$MFISRB<-calibrate(DATA$PresenceofMFIs_2008, type="crisp", thresholds=1)
skew.check(DATA$MFISRB)
DATA$MFISRC<-DATA$MFISRB-DATA$MFISR
str(DATA$MFISRC)
DATA$MFIRSD<-calibrate(DATA$MFISRC,type="fuzzy",thresholds="e=-1,c=0.5, i=1", logistic = TRUE)
skew.check(DATA$MFISRD)

## Calibrating International Human Rights: (Set of countries with high number of international human rights treaties)
hist(DATA$INTHRT_1999)
str(DATA$INTHRT_1999)
sort(DATA$INTHRT_1999)
Xplot(DATA$INTHRT_1999, jitter=TRUE)
DATA$HRTR<-calibrate(DATA$INTHRT_1999, type="crisp", thresholds=8)
skew.check(DATA$HRTR)
hist(DATA$INTHRT_2008)
str(DATA$INTHRT_2008)
sort(DATA$INTHRT_2008)
Xplot(DATA$INTHRT_2008, jitter=TRUE)
DATA$HRTRB<-calibrate(DATA$INTHRT_2008,type="fuzzy",thresholds="e=10.5,c=12.5, i=14", logistic = TRUE)
skew.check(DATA$HRTRB)
DATA$HRTRC<-DATA$HRTRB-DATA$HRTR
str(DATA$HRTRC)
DATA$HRTRD<-calibrate(DATA$HRTRC,type="fuzzy",thresholds="e=-0.30,c=0.00, i=0.99", logistic = TRUE)
skew.check(DATA$HRTRD)

# Calibrating GDP: Set of countries with high GDP
print(DATA$GDP_1999)
sort(DATA$GDP_1999)
hist(DATA$GDP_1999)
Xplot(DATA$GDP_1999, jitter=TRUE)
DATA$GDPB<-calibrate(DATA$GDP_1999, type="fuzzy", thresholds="e=4600000000, c=15000000000, i=23000000000", logistic=TRUE)
skew.check(DATA$GDPB)
plot(DATA$GDP_1999, DATA$GDPB, main="Calibration of GDP", xlab="Raw Score", ylab="Calibrated Score", abline(h=0.5, v=15000000000))
print(DATA$GDP_2008)
sort(DATA$GDP_2008)
hist(DATA$GDP_2008)
Xplot(DATA$GDP_2008, jitter=TRUE)
DATA$GDPC<-calibrate(DATA$GDP_2008, type="fuzzy", thresholds="e=4500000000, c=50000000000, i=70000000000", logistic=TRUE)
skew.check(DATA$GDPC)
DATA$GDPD<-DATA$GDPC-DATA$GDPB
str(DATA$GDPD)
DATA$GDPE<-calibrate(DATA$GDPD, type="fuzzy", thresholds="e=-0.720, c=0.010, i=0.060", logistic=TRUE)
skew.check(DATA$GDPE)

# Calibrating Democracy, Set of highly democratic countries: "autocracies" (-10 to -6), "anocracies" (-5 to +5), 
# three special values: -66, -77 and -88), and "democracies" (+6 to +10)
print(DATA$DEM_1999)
str(DATA$DEM_1999)
hist(DATA$DEM_1999)
Xplot(DATA$DEM_1999, jitter=TRUE)
DATA$DEM<-calibrate(DATA$DEM_1999, type="crisp", thresholds=0)
skew.check(DATA$DEM)
print(DATA$DEM_2008)
str(DATA$DEM_2008)
hist(DATA$DEM_2008)
Xplot(DATA$DEM_2008, jitter=TRUE)
DATA$DEMB<-calibrate(DATA$DEM_2008, type="fuzzy", thresholds="e=-66, c=1, i=9", logistic = TRUE)
skew.check(DATA$DEMB)
DATA$DEMC<-DATA$DEMB-DATA$DEM
str(DATA$DEMC)
DATA$DEMD<-calibrate(DATA$DEMC, type="fuzzy", thresholds="e=-0.05, c=0.0, i=0.05", logistic=TRUE)
skew.check(DATA$DEMD)

# Calibrating PCL: Set of countries with high political and civil liberty
# free(1.0-2.5), partly free (3.0-5.0), not free (5.5-7.0)
#ggplot(data, aes(x=YEAR,y=PCL, group=COUNTRY)) + geom_line(aes(col=COUNTRY)) + theme_light()
print(DATA$PCL_1999)
sort(DATA$PCL_1999)
Xplot(DATA$PCL_1999, jitter=TRUE)
hist(DATA$PCL_1999)
DATA$PCLB<-recode(DATA$PCL_1999, rules="5=0; else=1")
skew.check(DATA$PCLB)
print(DATA$PCL_2008)
sort(DATA$PCL_2008)
Xplot(DATA$PCL_2008, jitter=TRUE)
hist(DATA$PCL_2008)
DATA$PCLC<-calibrate(DATA$PCL_2008, type="fuzzy", thresholds="i=2.0, c=2.6, e=3.5", logistic = TRUE)
skew.check(DATA$PCLC)
DATA$PCLD<-DATA$PCLC-DATA$PCLB
str(DATA$PCLD)
DATA$PCLE<-calibrate(DATA$PCLD, type="fuzzy", thresholds="e=-0.05, c=0.1, i=0.2", logistic=TRUE)
skew.check(DATA$PCLE)

##Calibrating Percentage of Literate Females, Set of countries with high percentage of literate females:
sort(DATA$LTRCYF_1999)
Xplot(DATA$LTRCYF_1999, jitter=TRUE)
hist(DATA$LTRCYF_1999)
DATA$LTRCYFB<-calibrate(DATA$LTRCYF_1999, type="fuzzy", thresholds="e=98.70, c=99.00, i=99.60", logistic = TRUE)
skew.check(DATA$LTRCYFB)
sort(DATA$LTRCYF_2008)
Xplot(DATA$LTRCYF_2008, jitter=TRUE)
hist(DATA$LTRCYF_2008)
DATA$LTRCYFC<-calibrate(DATA$LTRCYF_2008, type="fuzzy", thresholds="e=99.30, c=99.50, i=99.80", logistic = TRUE)
skew.check(DATA$LTRCYFC)
DATA$LTRCYFD<-DATA$LTRCYFC-DATA$LTRCYFB
sort(DATA$LTRCYFD)
DATA$LTRCYFE<-calibrate(DATA$LTRCYFD, type="fuzzy", thresholds="e=-0.35, c=0.0, i=0.89", logistic = TRUE)
skew.check(DATA$LTRCYFE)


##Calibrating Life Expectancy for Females, Set of countries with high life expectancy for females:
sort(DATA$LIFEXF_1999)
Xplot(DATA$LIFEXF_1999, jitter=TRUE)
hist(DATA$LIFEXF_1999)
DATA$LIFEXFB<-calibrate(DATA$LIFEXF_1999, type="fuzzy", thresholds="e=76.10, c=76.30, i=76.58", logistic = TRUE)
skew.check(DATA$LIFEXFB)
sort(DATA$LIFEXF_2008)
Xplot(DATA$LIFEXF_2008, jitter=TRUE)
hist(DATA$LIFEXF_2008)
DATA$LIFEXFC<-calibrate(DATA$LIFEXF_2008, type="fuzzy", thresholds="e=76.70, c=77.50, i=79.60", logistic = TRUE)
skew.check(DATA$LIFEXFC)
DATA$LIFEXFD<-DATA$LIFEXFC-DATA$LIFEXFB
sort(DATA$LIFEXFD)
DATA$LIFEXFE<-calibrate(DATA$LIFEXFD, type="fuzzy", thresholds="e=-0.35, c=0.1, i=0.89", logistic = TRUE)
skew.check(DATA$LIFEXFE)


## Calibrating access to high transparency guarantees:
# Control_of_Corruption, Set of high control of corruption:
# Researchers started collecting this data from the year 1996
# Hence, I have coded the year 1995 as zero
# Between the years 1996-2001, data was collected for every alternate year.
# Hence, I have recoded 1997, 1999, 2001 with the values of the year before.
# Between the years 2002-2015, data is available for every year
# Due to the presence of missing values (NAs), R has characterized this variable 
# as a character variable. It needs to be a numeric variable first before recoding.

DATA$NCOR<-as.numeric(DATA$FCOR_1999)
DATA$NCOR[which(DATA$Observations=="BosniaandHerzegovina_1999")]<-42.783499999999997
DATA$NCOR[which(DATA$Observations=="Croatia_1999")]<-34.536079999999998
DATA$NCOR[which(DATA$Observations=="Montenegro_1999")]<-71.134020000000007
Xplot(DATA$NCOR,jitter=TRUE,at=pretty(DATA$NCOR))
hist(DATA$NCOR)
sort(DATA$NCOR)
DATA$NCORA<-calibrate(DATA$NCOR, type="fuzzy", thresholds="e=34.0, c=45.0, i=71.0", logistic = TRUE)
skew.check(DATA$NCORA)
Xplot(DATA$FCOR_2008,jitter=TRUE,at=pretty(DATA$FCOR_2008))
hist(DATA$FCOR_2008)
sort(DATA$FCOR_2008)
DATA$NCORB<-calibrate(DATA$FCOR_2008, type="fuzzy", thresholds="e=42.0, c=45.0, i=58.0", logistic = TRUE)
skew.check(DATA$NCORB)
DATA$NCORC<-DATA$NCORB-DATA$NCORA
sort(DATA$NCORC)
DATA$NCORD<-calibrate(DATA$NCORC, type="fuzzy", thresholds="e=-0.25, c=0.0, i=0.89", logistic = TRUE)
skew.check(DATA$NCORD)


# Condition: FEFFECT, Set of countries with high government effectiveness
# Government effectiveness 
# The data is in percentage format
# Researchers started collecting this data from the year 1996
# Hence, I have coded the year 1995 as zero
# Between the years 1996-2001, data was collected for every alternate year.
# Hence, I have recoded 1997, 1999, 2001 with the values of the year before.
# Between the years 2002-2015, data is available for every year
# Due to the presence of missing values (NAs), R has characterized the variable 
# as a character variable. It needs to be a numeric variable first before recoding.

DATA$NEFFECT<- as.numeric(DATA$FEFFECT_1999)
DATA$NEFFECT[which(DATA$Observations=="BosniaandHerzegovina_1999")]<-11.398960000000001
DATA$NEFFECT[which(DATA$Observations=="Croatia_1999")]<-59.58549
DATA$NEFFECT[which(DATA$Observations=="Montenegro_1999")]<-0
print(DATA$NEFFECT)
sort(DATA$NEFFECT)
Xplot(DATA$NEFFECT, jitter=TRUE)
hist(DATA$NEFFECT)
DATA$NEFFECTB<-calibrate(DATA$NEFFECT, type="fuzzy", thresholds="e=0.0, c=15.0, i=59.0", logistic = TRUE)
skew.check(DATA$NEFFECTB)
print(DATA$FEFFECT_2008)
sort(DATA$FEFFECT_2008)
Xplot(DATA$FEFFECT_2008, jitter=TRUE)
hist(DATA$FEFFECT_2008)
DATA$NEFFECTC<-calibrate(DATA$FEFFECT_2008, type="fuzzy", thresholds="e=33.0, c=55.0, i=71.0", logistic = TRUE)
skew.check(DATA$NEFFECTC)
DATA$NEFFECTD<-DATA$NEFFECTC-DATA$NEFFECTB
print(DATA$NEFFECTD)
sort(DATA$NEFFECTD)
Xplot(DATA$NEFFECTD, jitter=TRUE)
hist(DATA$NEFFECTD)
DATA$NEFFECTE<-calibrate(DATA$NEFFECTD, type="fuzzy", thresholds="e=-0.27, c=0.10, i=0.41", logistic = TRUE)
skew.check(DATA$NEFFECTE)

## Calibrating Female Representation in Parliament; Set of Countries with High Female Representation
print(DATA$FPARL_1999)
DATA$FPARL_1999[which(DATA$Observations=="BosniaandHerzegovina_1999")]<-0
DATA$FPARL_1999[which(DATA$Observations=="Montenegro_1999")]<-0
Xplot(DATA$FPARL_1999, jitter=TRUE, at=pretty(DATA$FPARL_1999))
hist(DATA$FPARL_1999)
sort(DATA$FPARL_1999)
DATA$FRPR<-calibrate(DATA$FPARL_1999, type="crisp", thresholds=7.9)
skew.check(DATA$FRPR)
sort(DATA$FPARL_2008)
DATA$FRPRB<-calibrate(DATA$FPARL_2008, type="fuzzy", thresholds="e=11.1, c=15.0, i=20.9", logistic = TRUE)
skew.check(DATA$FRPRB)
DATA$FRPRC<-DATA$FRPRB-DATA$FRPR
sort(DATA$FRPRC)
DATA$FRPRD<-calibrate(DATA$FRPRC, type="fuzzy", thresholds="e=-0.05, c=0.00, i=0.08", logistic = TRUE)
skew.check(DATA$FRPRD)

# Calibrating Female Own-Account Workers, Set of countries with high number of female own-account workers:
Xplot(DATA$OWN_1999, jitter=TRUE, at=pretty(DATA$OWN_1999))
sort(DATA$OWN_1999)
hist(DATA$OWN_1999)
DATA$OWNB<-calibrate(DATA$OWN_1999, type="fuzzy", thresholds="e=5000, c=50000, i=80000", logistic = TRUE)
skew.check(DATA$OWNB)
Xplot(DATA$OWN_2008, jitter=TRUE, at=pretty(DATA$OWN_2008))
sort(DATA$OWN_2008)
hist(DATA$OWN_2008)
DATA$OWNC<-calibrate(DATA$OWN_2008, type="fuzzy", thresholds="e=5000, c=54000, i=99000", logistic = TRUE)
skew.check(DATA$OWNC)
DATA$OWND<-DATA$OWNC-DATA$OWNB
sort(DATA$OWND)
DATA$OWNE<-calibrate(DATA$OWND, type="fuzzy", thresholds="e=-0.027, c=0.001, i=0.002", logistic = TRUE)
skew.check(DATA$OWNE)

## Calibrating female family workers, Set of countries with High Number of Female Family Workers:
Xplot(DATA$FAM_1999, jitter=TRUE, at=pretty(DATA$FAM_1999))
sort(DATA$FAM_1999)
hist(DATA$FAM_1999)
DATA$FAMA<-calibrate(DATA$FAM_1999, type="fuzzy", thresholds="e=2000, c=61000, i=68000", logistic = TRUE)
skew.check(DATA$FAMA)
sort(DATA$FAM_2008)
hist(DATA$FAM_2008)
DATA$FAMB<-calibrate(DATA$FAM_2008, type="fuzzy", thresholds="e=2000, c=30000, i=40000", logistic = TRUE)
skew.check(DATA$FAMB)
DATA$FAMC<-DATA$FAMB-DATA$FAMA
sort(DATA$FAMC)
DATA$FAMD<-calibrate(DATA$FAMC, type="fuzzy", thresholds="e=-0.500, c=0.200, i=0.400", logistic = TRUE)
skew.check(DATA$FAMD)

## Calibrating female employees, Set of countries with high number of female employees:
Xplot(DATA$EMPE_1999, jitter=TRUE, at=pretty(DATA$EMPE_1999))
sort(DATA$EMPE_1999)
hist(DATA$EMPE_1999)
DATA$EMPEA<-calibrate(DATA$EMPE_1999, type="fuzzy", thresholds="e=55000, c=260000, i=570000", logistic = TRUE)
skew.check(DATA$EMPEA)
Xplot(DATA$EMPE_2008, jitter=TRUE, at=pretty(DATA$EMPE_2008))
sort(DATA$EMPE_2008)
hist(DATA$EMPE_2008)
DATA$EMPEB<-calibrate(DATA$EMPE_2008, type="fuzzy", thresholds="e=75000, c=300000, i=640000", logistic = TRUE)
skew.check(DATA$EMPEB)
DATA$EMPED<-DATA$EMPEB-DATA$EMPEA
sort(DATA$EMPED)
DATA$EMPEF<-calibrate(DATA$EMPED, type="fuzzy", thresholds="e=-0.0015, c=-0.0007, i=-0.0004", logistic = TRUE)
skew.check(DATA$EMPEF)

## Calibrating female employers, Set of countries with a high number of female employers:
Xplot(DATA$EMPR_1999,jitter=TRUE, at=pretty(DATA$EMPR_1999))
sort(DATA$EMPR_1999)
hist(DATA$EMPR_1999)
DATA$EMPRA<-calibrate(DATA$EMPR_1999, type="fuzzy", thresholds="e=3200, c=12000, i=24000", logistic = TRUE)
skew.check(DATA$EMPRA)
sort(DATA$EMPR_2008)
hist(DATA$EMPR_2008)
DATA$EMPRB<-calibrate(DATA$EMPR_2008, type="fuzzy", thresholds="e=4500, c=15000, i=23000", logistic = TRUE)
skew.check(DATA$EMPRB)
DATA$EMPRC<-DATA$EMPRB-DATA$EMPRA
sort(DATA$EMPRC)
DATA$EMPRD<-calibrate(DATA$EMPRC, type="fuzzy", thresholds="e=-0.0007, c=0.00955, i=0.0096", logistic = TRUE)
skew.check(DATA$EMPRD)
plot(data$EMPRC,data$EMPRD, main="Calibration of Female Employers", xlab="Raw Score", ylab="Calibrated Score", abline(h=0.5,v=0.00955))

write.csv(DATA,"CALDATA1999_2008.csv")

#Creating a macro-condition: Set of high number of women employed at the national economy
DATAF$HNWEB<-with(DATAF,fuzzyand(OWNE, FAMD, EMPEF, EMPRD))
skew.check(DATAF$HNWEB)
print(DATAF[ ,c("Observations", "OWNE", "FAMD", "EMPEF", "EMPRD", "HNWEB")])

## Creating a new data set:
DATAF<-read.csv("CALDATA1999_2008.csv")
PANELF<-DATAF %>% select(X, Observations, MFIRSD, GDPE, DEMD, PCLE, LTRCYFE, LIFEXFE, NCORD, NEFFECTE, FRPRD, HRTRD, OWNE, FAMD, EMPEF, EMPRD, HNWEB)


## The truth table did not show any sufficient conditions for OWNE above 7.0, so I have changed the outcome to OWNE

## To check for presence of necessary conditions and presence of outcome(HNWEB):
QCAfit(PANELF$MFIRSD, PANELF$OWNE, cond.lab="Presence of MFIs", necessity = TRUE, consH = TRUE)
QCAfit(PANELF$GDPE, PANELF$OWNE, cond.lab="High GDP", necessity = TRUE, consH = TRUE)
QCAfit(PANELF$DEMD,  PANELF$OWNE, cond.lab="High DEMC", necessity = TRUE, consH = TRUE)
QCAfit(PANELF$PCLE, PANELF$OWNE, cond.lab="High DPCL", necessity = TRUE, consH = TRUE)
QCAfit(PANELF$LTRCYFE, PANELF$OWNE, cond.lab="High LTRCYR", necessity = TRUE, consH = TRUE)
QCAfit(PANELF$LIFEXFE, PANELF$OWNE, cond.lab="High LIFEXFR", necessity = TRUE, consH = TRUE)
QCAfit(PANELF$NCORD, PANELF$OWNE, cond.lab="High NCORR", necessity = TRUE, consH = TRUE)
QCAfit(PANELF$NEFFECTE, PANELF$OWNE, cond.lab="High NEFFECT", necessity = TRUE, consH = TRUE)
QCAfit(PANELF$FRPRD, PANELF$OWNE, cond.lab="High FRPR", necessity = TRUE, consH = TRUE)
QCAfit(PANELF$HRTRD, PANELF$OWNE, cond.lab="High HRTR", necessity = TRUE, consH = TRUE)

## To check for absence of necessary conditions and presence of outcome(HNWEB):
QCAfit(1-PANELF$MFIRSD, PANELF$OWNE, cond.lab="Absence of MFIs", necessity = TRUE, consH = TRUE)
QCAfit(1-PANELF$GDPE, PANELF$OWNE, cond.lab="Absence of High GDP", necessity = TRUE, consH = TRUE)
QCAfit(1-PANELF$DEMD,  PANELF$OWNE, cond.lab="Absence of High DEMC", necessity = TRUE, consH = TRUE)
QCAfit(1-PANELF$PCLE, PANELF$OWNE, cond.lab="Absence of High DPCL", necessity = TRUE, consH = TRUE)
QCAfit(1-PANELF$LTRCYFE, PANELF$OWNE, cond.lab="Absence of High LTRCYR", necessity = TRUE, consH = TRUE)
QCAfit(1-PANELF$LIFEXFE, PANELF$OWNE, cond.lab="Absence of High LIFEXFR", necessity = TRUE, consH = TRUE)
QCAfit(1-PANELF$NCORD, PANELF$OWNE, cond.lab="Absence of High NCORR", necessity = TRUE, consH = TRUE)
QCAfit(1-PANELF$NEFFECTE, PANELF$OWNE, cond.lab="Absence of High NEFFECT", necessity = TRUE, consH = TRUE)
QCAfit(1-PANELF$FRPRD, PANELF$OWNE, cond.lab="Absence of High FRPR", necessity = TRUE, consH = TRUE)
QCAfit(1-PANELF$HRTRD, PANELF$OWNE, cond.lab="Absence of High HRTR", necessity = TRUE, consH = TRUE)

## Creating a new data set:
write.csv(DATA, "DATAB.csv")
DATAB<-read.csv("DATAB.csv")
PANELF<-DATAF %>% select(X, Observations, MFIRSD, GDPE, DEMD, PCLE, LTRCYFE, LIFEXFE, NCORD, NEFFECTE, FRPRD, HRTRD, OWNE, FAMD, EMPEF, EMPRD, HNWEB)

##Truth Table for HNWEB:
ttF<-truthTable(data=PANELF, outcome="OWNE", 
                  conditions= c("MFIRSD", "GDPE", "DEMD", "PCLE", "LTRCYFE", "LIFEXFE", "NCORD", "NEFFECTE", "FRPRD", "HRTRD"), 
                  incl.cut = 0.85, 
                  sort.by = "incl,n", pri.cut=0.51, dcc= TRUE, decreasing=FALSE,complete=TRUE, show.cases = TRUE)
ttF

ttf<-truthTable(data=PANELF, outcome="~OWNE", 
                  conditions= c("MFIRSD", "GDPE", "DEMD", "PCLE", "LTRCYFE", "LIFEXFE", "NCORD", "NEFFECTE", "FRPRD", "HRTRD"), 
                  incl.cut = 0.85, 
                  sort.by = "incl,n", pri.cut=0.51, dcc= TRUE, decreasing=FALSE,complete=TRUE, show.cases = TRUE)
ttf

## Boolean Minimization for truth table and eliminate redundant prime implicants:
sol_F<-minimize(input=ttF, details=TRUE, row.dom=TRUE)
sol_F
sol_f<-minimize(input=ttf, details=TRUE, row.dom=TRUE)
sol_f


##For the conservative, parsimonious, and intermediate solutions, presence:
sol_CF <- minimize(ttF, details=TRUE, row.dom=TRUE)
sol_CF
sol_PF <-minimize(ttF, include="?", details=TRUE, row.dom=TRUE)
sol_PF
sol_IF <- minimize(ttF, include = "?", dir.exp = "1, 1, 1, 1, 1, 1, 1, 1, 1, 1",details=TRUE, row.dom=TRUE)
sol_IF

# For the conservative, parsimonious, and intermediate solutions of the sufficiency result, absence:
sol_cf <- minimize(ttf, details=TRUE, row.dom=TRUE)
sol_cf
sol_pf<-minimize(ttf, include="?", details=TRUE, row.dom=TRUE)
sol_pf
sol_if <- minimize(ttf, include = "?", dir.exp = "0, 0, 0, 0, 0, 0, 0, 0, 0, 0",details=TRUE, row.dom=TRUE)
sol_if


## For identifying errors, using the findRows function:
findRows(obj=ttF, type=0)
TYPETWO<-findRows(obj=ttF, type=2)
TYPETWO
findRows(obj=ttF, type=3)


## For the enhanced conservative, parsimonious, and intermediate solutions:
sol_CFN<-minimize(ttF, details=TRUE, exclude=c(TYPETWO))
sol_CFN
sol_PFN<-minimize(ttF, include="?", details=TRUE, exclude=c(TYPETWO))
sol_PFN
sol_IFN<-minimize(ttF, include = "?", dir.exp = "1, 1, 1, 1, 1, 1, 1, 1, 1, 1",details=TRUE, exclude=c(TYPETWO))
sol_IFN

## Visualizing the enhanced intermediate solution:
PANELB$Observations<-NULL
PANELB$COUNTRY<-NULL
PANELB$YEAR<-NULL
PANELB$X<-NULL
XYplot(MFISR*GDPR*DEMC*DPCL*LTRCYR*NEFFECTR*FRPR*HRTR, HNWEB, data=PANELB, relation = "sufficiency", enhance = TRUE, jitter=TRUE, clabels = seq(nrow(PANELB)))

## Visualizing NEFFECTE as a necessary condition for OWNE:
PANELF$Observations<-NULL
PANELF$X<-NULL
XYplot(NEFFECTE, OWNE, data=PANELF, relation = "necessity", enhance = TRUE, jitter=TRUE, clabels = seq(nrow(PANELF)))
## Visualizing the enhanced intermediate solution for OWNE:
XYplot("MFIRSD*PCLE*LIFEXFE*NEFFECTE*FRPRD*HRTRD", OWNE, data=PANELF, relation = "sufficiency", enhance = TRUE, jitter=TRUE, clabels = seq(nrow(PANELF)))


### Chapter 9:
## Testing Robustness:
library("readxl")
library("dplyr")
library("ggplot2")
library("QCA")
library("SetMethods")
library("readr")
data<-read.csv("data.csv")
RAWDATA<-read.csv("Final_Panel_data.csv")
RAWDATA$COUNTRY<-NULL
RAWDATA$YEAR<-NULL

CALIB<-data %>% select(Observations, MFISR, GDPR, DEMC, DPCL, LTRCYR, LIFEXFR, FCORR, NEFFECTR, FRPR, HRTR, OWNR, FAMR, EMPER, EMPRR)
RAW<-RAWDATA %>% select(Observations, PresenceofMFIs, GDP, DEM, PCL, LTRCYF, LIFEXF, FCOR, FEFFECT, FPARL, INTHRT, OWN, FAM, EMPE, EMPR)

conds<-c("MFISR", "GDPR","DEMC", "DPCL", "LTRCYR", "LIFEXFR", "FCORR", "NEFFECTR", "FRPR", "HRTR")
condsr<-c("PresenceofMFIs", "GDP","DEM", "PCL", "LTRCYF", "LIFEXF", "FCOR", "FEFFECT", "FPARL", "INTHRT")

## Sensitivity range for calibration of GDP:
rob.calibrange(raw.data=RAW,
               calib.data=CALIB,
               test.cond.raw="GDP",
               test.cond.calib = "GDPR",
               test.thresholds=c(980000000, 30000000000, 50000000000),
               type="fuzzy",
               step=50000,
               max.runs=40,
               outcome="OWNR",
               conditions = conds,
               incl.cut=0.85,
               n.cut=1,
               include="?")

PANEL<-data %>% select(Observations, MFISR, GDPR, DEMC, DPCL, LTRCYR, LIFEXFR, FCORR, NEFFECTR, FRPR, HRTR, OWNR, FAMR, EMPER, EMPRR, COUNTRY, YEAR)
PANEL$HNWEB<-with(PANEL, fuzzyor(OWNR, FAMR, EMPER, EMPRR))

smmr(results=sol_IEMPN, outcome="HNWEB", match=FALSE, cases=1, term=1)

modelFit(sol_IEMPN, "MFISR*GDPR*DEMC*DPCL*LTRCYR*LIFEXFR*FCORR*NEFFECTR*FRPR*HRTR")
t<-"MFISR*GDPR*DEMC*DPCL*LTRCYR*LIFEXFR*FCORR*NEFFECTR*FRPR*HRTR"
THEVAL<- theory.evaluation(theory=t, empirics=sol_IEMPN, outcome="HNWEB", sol="c1p1i1", print.data=TRUE, print.fit=TRUE)
THEVAL
