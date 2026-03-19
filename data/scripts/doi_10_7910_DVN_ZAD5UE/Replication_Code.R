# Replication Code for Me, Myself, and (I), (D) or (R)? Partisanship and Political Cognition through the Lens of Implicit Identity



# Generating IAT/PID/IDPG Comparison Plots



# IAT Analysis

#Load Data
library(foreign)
library(boot)
library(gplots)
library(car)
library(ggplot2)
library(survey)

IAT <- read.spss("YouGovData", use.value.labels=TRUE, max.value.labels=Inf, to.data.frame=TRUE)

# IAT <- subset(IAT, subset=pid3lean!="NA")
# IAT <- subset(IAT, subset=pid7zero!="NA")
# IAT <- subset(IAT, subset=polidd123!="NA")

# PID3LEAN
#Changing the labels for pid3lean, creating pid3leans
IAT$pid3leans <- factor(IAT$pid3lean, labels = c("Democrat" = "Democrats", "Independent" = "Independents", "Republican" = "Republicans"))

Dems <- subset(IAT, subset=pid3lean=="Democrat")
Reps <- subset(IAT, subset=pid3lean=="Republican")



# Plots WITH WEIGHTS

# Using ggplot2 and survey


#Creating the svydesign
weighted<-svydesign(id=~0, weights=~weight, data=IAT)

## Descriptives
# mat = TRUE returns results of svyby as a matrix
descriptives_matrix <- svyby(~useiat,~pid3leans, svymean, design=weighted, na.rm=TRUE, mat=TRUE)
count_matrix<-svytable(~pid3leans, design=weighted)

## Computing Naive Confidence Intervals from Naive Standard Errors
descriptives_matrix$minci <- descriptives_matrix$useiat -(1.96  * descriptives_matrix$se)
descriptives_matrix$maxci <- descriptives_matrix$useiat +(1.96  * descriptives_matrix$se)

## Making the plots:
ggplot(descriptives_matrix, aes(x = descriptives_matrix$pid3leans, y = descriptives_matrix$useiat)) + geom_point(size=3, colour="grey") + geom_errorbar(aes(ymax = descriptives_matrix$maxci, ymin=descriptives_matrix$minci), width=.1, size=1.5)+theme_bw() + geom_text(label=round(descriptives_matrix$useiat, 3), hjust=-.3, size=7) + geom_text(label=round(count_matrix), y=descriptives_matrix$minci-.025, size=7) + labs(x = "Explicit 3-Point PID", y = "Implicit Party Identity: D Score") + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + theme(axis.text=element_text(size=16), axis.title=element_text(size=20)) + ylim(-.5,.6)


# PID7ZERO
#Changing the labels for pid7zero, creating pid7zeros
IAT$pid7zeros <- factor(IAT$pid7zero, labels = c("Strong Democrat" = "Strong Dem", "Not So Strong Democrat" = "Not So Strong Dem", "Strong Democrat" = "Lean Dem", "Independent" = "Independent", "Lean Republican" = "Lean Rep", "Not So Strong Republican" = "Not So Strong Rep", "Strong Republican" = "Strong Rep"))

#Creating the svydesign
weighted<-svydesign(id=~0, weights=~weight, data=IAT)

## Descriptives
# mat = TRUE returns results of svyby as a matrix
descriptives_matrix <- svyby(~useiat,~pid7zeros,svymean,design=weighted, na.rm=TRUE, mat=TRUE)
count_matrix<-svytable(~pid7zeros, design=weighted)


## Computing Naive Confidence Intervals from Naive Standard Errors
descriptives_matrix$minci <- descriptives_matrix$useiat -(1.96  * descriptives_matrix$se)
descriptives_matrix$maxci <- descriptives_matrix$useiat +(1.96  * descriptives_matrix$se)


## Making the plots:
ggplot(descriptives_matrix, aes(x = descriptives_matrix$pid7zeros, y = descriptives_matrix$useiat)) + geom_point(size=3, colour="grey") + geom_errorbar(aes(ymax = descriptives_matrix$maxci, ymin=descriptives_matrix$minci), width=.1, size=1.5)+theme_bw() + geom_text(label=round(descriptives_matrix$useiat, 3), hjust=-.3, size=7) + geom_text(label=round(count_matrix), y=descriptives_matrix$minci-.025, size=7) + labs(x = "Explicit 7-Point PID", y = "Implicit Party Identity: D Score") + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + theme(axis.text=element_text(size=17), axis.title=element_text(size=21)) + ylim(-.5,.6)





# ANALYSIS OF SURVEY ITEMS


# IAT Analysis

#Load Data
library(foreign)
library(boot)
library(gplots)
library(car)
library(ggplot2)
library(survey)

IAT <- read.spss("YouGovData", use.value.labels=TRUE, max.value.labels=Inf, to.data.frame=TRUE)


# Generally speaking

#Creating the svydesign
weighted<-svydesign(id=~0, weights=~weight, data=IAT)

## Descriptives
# mat = TRUE returns results of svyby as a matrix
descriptives_matrix <- svyby(~useiat,~generally,svymean,design=weighted, na.rm=TRUE, mat=TRUE)
count_matrix<-svytable(~generally, design=weighted)


## Computing Naive Confidence Intervals from Naive Standard Errors
descriptives_matrix$minci <- descriptives_matrix$useiat -(1.96  * descriptives_matrix$se)
descriptives_matrix$maxci <- descriptives_matrix$useiat +(1.96  * descriptives_matrix$se)

## Making the plots:
ggplot(descriptives_matrix, aes(x = descriptives_matrix$generally, y = descriptives_matrix$useiat)) + geom_point(size=3, colour="grey") + geom_errorbar(aes(ymax = descriptives_matrix$maxci, ymin=descriptives_matrix$minci), width=.1, size=1.5)+theme_bw() + geom_text(label=round(descriptives_matrix$useiat, 3), hjust=-.3, size=7) + geom_text(label=round(count_matrix), y=descriptives_matrix$minci-.025, size=7) + labs(x = "Generally speaking do you think of yourself as a...?", y = "Implicit Party Identity: D Score") + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + theme(axis.text=element_text(size=16), axis.title=element_text(size=18)) + ylim(-.5,.6)


# leaner

#Creating the svydesign
weighted<-svydesign(id=~0, weights=~weight, data=IAT)

## Descriptives
# mat = TRUE returns results of svyby as a matrix
descriptives_matrix <- svyby(~useiat,~leaner,svymean,design=weighted, na.rm=TRUE, mat=TRUE)
count_matrix<-svytable(~leaner, design=weighted)


## Computing Naive Confidence Intervals from Naive Standard Errors
descriptives_matrix$minci <- descriptives_matrix$useiat -(1.96  * descriptives_matrix$se)
descriptives_matrix$maxci <- descriptives_matrix$useiat +(1.96  * descriptives_matrix$se)

## Making the plots:
ggplot(descriptives_matrix, aes(x = descriptives_matrix$leaner, y = descriptives_matrix$useiat)) + geom_point(size=3, colour="grey") + geom_errorbar(aes(ymax = descriptives_matrix$maxci, ymin=descriptives_matrix$minci), width=.1, size=1.5)+theme_bw() + geom_text(label=round(descriptives_matrix$useiat, 3), hjust=-.3, size=7) + geom_text(label=round(count_matrix), y=descriptives_matrix$minci-.025, size=7) + labs(x = "Do you think of yourself as closer to...?", y = "Implicit Party Identity: D Score") + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + theme(axis.text=element_text(size=16), axis.title=element_text(size=18)) + ylim(-.5,.6)



# repstrong

#Creating the svydesign
weighted<-svydesign(id=~0, weights=~weight, data=IAT)

## Descriptives
# mat = TRUE returns results of svyby as a matrix
descriptives_matrix <- svyby(~useiat,~repstrong,svymean,design=weighted, na.rm=TRUE, mat=TRUE)
count_matrix<-svytable(~repstrong, design=weighted)


## Computing Naive Confidence Intervals from Naive Standard Errors
descriptives_matrix$minci <- descriptives_matrix$useiat -(1.96  * descriptives_matrix$se)
descriptives_matrix$maxci <- descriptives_matrix$useiat +(1.96  * descriptives_matrix$se)

## Making the plots:
ggplot(descriptives_matrix, aes(x = descriptives_matrix$repstrong, y = descriptives_matrix$useiat)) + geom_point(size=3, colour="grey") + geom_errorbar(aes(ymax = descriptives_matrix$maxci, ymin=descriptives_matrix$minci), width=.1, size=1.5)+theme_bw() + geom_text(label=round(descriptives_matrix$useiat, 3), hjust=-.3, size=7) + geom_text(label=round(count_matrix), y=descriptives_matrix$minci-.005, size=7) + labs(x = "Would you call yourself a strong Republican or a not so strong...?", y = "Implicit Party Identity: D Score") + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + theme(axis.text=element_text(size=16), axis.title=element_text(size=18))




# demstrong

#Creating the svydesign
weighted<-svydesign(id=~0, weights=~weight, data=IAT)

## Descriptives
# mat = TRUE returns results of svyby as a matrix
descriptives_matrix <- svyby(~useiat,~demstrong,svymean,design=weighted, na.rm=TRUE, mat=TRUE)
count_matrix<-svytable(~demstrong, design=weighted)


## Computing Naive Confidence Intervals from Naive Standard Errors
descriptives_matrix$minci <- descriptives_matrix$useiat -(1.96  * descriptives_matrix$se)
descriptives_matrix$maxci <- descriptives_matrix$useiat +(1.96  * descriptives_matrix$se)

## Making the plots:
ggplot(descriptives_matrix, aes(x = descriptives_matrix$demstrong, y = descriptives_matrix$useiat)) + geom_point(size=3, colour="grey") + geom_errorbar(aes(ymax = descriptives_matrix$maxci, ymin=descriptives_matrix$minci), width=.1, size=1.5)+theme_bw() + geom_text(label=round(descriptives_matrix$useiat, 3), hjust=-.3, size=7) + geom_text(label=round(count_matrix), y=descriptives_matrix$minci-.005, size=7) + labs(x = "Would you call yourself a strong Democrat or a not so strong...?", y = "Implicit Party Identity: D Score") + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + theme(axis.text=element_text(size=16), axis.title=element_text(size=18))





#  Weighted Density

# IAT Analysis

#Load Data
library(foreign)
library(boot)
library(gplots)
library(car)
library(ggplot2)
library(survey)

IAT <- read.spss("YouGovData", use.value.labels=TRUE, max.value.labels=Inf, to.data.frame=TRUE)





IAT$pid3leans <- factor(IAT$pid3lean, labels = c("Democrat" = "Democrats", "Independent" = "Independents", "Republican" = "Republicans"))

#Changing the labels for pid7zero, creating pid7zeros
IAT$pid7zeros <- factor(IAT$pid7zero, labels = c("Strong Democrat" = "Strong Dem", "Not So Strong Democrat" = "Not So Strong Dem", "Strong Democrat" = "Lean Dem", "Independent" = "Independent", "Lean Republican" = "Lean Rep", "Not So Strong Republican" = "Not So Strong Rep", "Strong Republican" = "Strong Rep"))

#Creating the svydesign
weighted<-svydesign(id=~0, weights=~weight, data=IAT)

## Descriptives
# mat = TRUE returns results of svyby as a matrix
descriptives_matrix <- svyby(~useiat,~pid7zeros,svymean,design=weighted, na.rm=TRUE, mat=TRUE)
count_matrix<-svytable(~pid7zeros, design=weighted)


## Computing Naive Confidence Intervals from Naive Standard Errors
descriptives_matrix$minci <- descriptives_matrix$useiat -(1.96  * descriptives_matrix$se)
descriptives_matrix$maxci <- descriptives_matrix$useiat +(1.96  * descriptives_matrix$se)



IAT <- subset(IAT, subset=pid3lean!="NA")
IAT <- subset(IAT, subset=pid7zero!="NA")
IAT <- subset(IAT, subset=polidd123!="NA")




# Density
ggplot(IAT, aes(x=IAT$useiat, colour=IAT$pid3leans, group=IAT$pid3leans, weight=IAT$weight/sum(IAT$weight))) + scale_colour_manual(values = c("Democrats" = "darkblue","Independents" = "green","Republicans" = "firebrick1")) + geom_line(size=1, stat="density") + theme_bw() + labs(x = "Implicit PID: D Score", y = "Density") + theme(legend.title=element_blank()) + theme(legend.position=c(.9, .9)) + xlim(-2, 2) + theme(axis.text=element_text(size=12), axis.title=element_text(size=18))


# scatterplot with marginal density

density <- ggplot(IAT, aes(x=IAT$useiat, colour=IAT$pid3leans, group=IAT$pid3leans, weight=IAT$weight/sum(IAT$weight))) + scale_colour_manual(values = c("Democrats" = "darkblue","Independents" = "green","Republicans" = "firebrick1")) + geom_line(size=1, stat="density") + theme_bw() + labs(x = "Implicit Party Identity: D Score", y = "Density") + theme(legend.title=element_blank()) + theme(legend.position=c(.7, .9)) + xlim(-1.7, 1.7) + coord_flip() + theme(axis.title.y = element_blank(), axis.text.y = element_blank(), plot.margin = unit(c(2,2,2,2), "mm")) + theme(axis.text=element_text(size=18), axis.title=element_text(size=20)) + theme(legend.key = element_blank()) + theme(legend.text = element_text(size = 18))

scat <- ggplot(IAT, aes(x=IAT$pid7zeros, y=IAT$useiat, group=1, weight=IAT$weight/sum(IAT$weight))) + geom_jitter(position = position_jitter(width = .3)) + geom_smooth(method=loess, span=.1, level=.95) + theme_bw() + labs(x = "Explicit 7-Point PID", y = "Implicit Party Identity: D Score") + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + geom_rug(col=rgb(.7,0,.7,alpha=.15), sides="l") + ylim(-1.7, 1.7) + theme(plot.margin = unit(c(2,2,2,2), "mm")) + theme(axis.text=element_text(size=18), axis.title=element_text(size=20)) 

library(gridExtra)
grid.arrange(scat, density, ncol=2, nrow=1, widths=c(4, 1)
             #, heights=c(1, 4)
)




#IDPG vs PID7

# scatterplot with marginal density

density <- ggplot(IAT, aes(x=IAT$useidpg, colour=IAT$pid3leans, group=IAT$pid3leans, weight=IAT$weight/sum(IAT$weight))) + scale_colour_manual(values = c("Democrats" = "darkblue","Independents" = "green","Republicans" = "firebrick1")) + geom_line(fill=NA, size=1, stat="density") + theme_bw() + labs(x = "Mean IDPG Score", y = "Density") + theme(legend.title=element_blank()) + theme(legend.position=c(.7, .9)) + xlim(-4, 4) + coord_flip() + theme(axis.title.y = element_blank(), axis.text.y = element_blank(), plot.margin = unit(c(2,2,2,2), "mm"))

scat <- ggplot(IAT, aes(x=IAT$pid7zero, y=IAT$useidpg, group=1, weight=IAT$weight/sum(IAT$weight))) + geom_jitter(position = position_jitter(width = .3)) + geom_smooth(method=loess, level=.95) + theme_bw() + labs(x = "Explicit 7-Point PID", y = "Mean IDPG Score") + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + geom_rug(col=rgb(.7,0,.7,alpha=.15), sides="l") + ylim(-4, 4) + theme(plot.margin = unit(c(2,2,2,2), "mm"))


library(gridExtra)
grid.arrange(scat, density, ncol=2, nrow=1, widths=c(4, 1), heights=c(1, 4))




#IDPG vs IAT

# scatterplot with marginal density

IAT <- subset(IAT, subset=pid3lean!="Independent")
#Dems <- subset(IAT, subset=pid3lean=="Democrat")
#Reps <- subset(IAT, subset=pid3lean=="Republican")


density <- ggplot(IAT, aes(x=IAT$useidpg, colour=IAT$pid3leans, group=IAT$pid3leans, weight=IAT$weight/sum(IAT$weight))) + scale_colour_manual(values = c("Democrats" = "darkblue","Independents" = "green","Republicans" = "firebrick1")) + geom_line(size=1, stat="density") + theme_bw() + labs(x = "Mean IDPG Score", y = "Density") + theme(legend.title=element_blank()) + theme(legend.position=c(.7, .9)) + xlim(-4, 4) + coord_flip() + theme(axis.title.y = element_blank(), axis.text.y = element_blank(), plot.margin = unit(c(2,2,2,2), "mm")) + theme(legend.key = element_blank())

density2 <- ggplot(IAT, aes(x=IAT$useiat, colour=IAT$pid3leans, group=IAT$pid3leans, weight=IAT$weight/sum(IAT$weight))) + scale_colour_manual(values = c("Democrats" = "darkblue","Independents" = "green","Republicans" = "firebrick1")) + geom_line(size=1, stat="density") + theme_bw() + labs(x = "Implicit PID: IAT D Score", y = "Density") + theme(legend.title=element_blank()) + theme(legend.position=c(.1, .55)) + xlim(-2, 2) + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), plot.margin = unit(c(2,2,2,2), "mm")) + theme(legend.key = element_blank())

empty <- ggplot()+geom_point(aes(1,1), colour="white")+ theme(axis.ticks=element_blank(), panel.background=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())

scat <- ggplot(IAT, aes(x=IAT$useiat, y=IAT$useidpg, colour=IAT$pid3leans, weight=IAT$weight/sum(IAT$weight))) + scale_colour_manual(values = c("Democrats" = "darkblue","Independents" = "green","Republicans" = "firebrick1")) + geom_point() + geom_smooth(method=lm, span=.18, level=.95) + theme_bw() + labs(x = "Implicit Party Identity: D Score", y = "Mean IDPG Score") + theme(legend.title=element_blank()) + theme(legend.position=c(.87, .5)) + geom_rug(col=rgb(.7,0,.7,alpha=.15)) + xlim(-2, 2) + ylim(-4, 4) + theme(plot.margin = unit(c(2,2,2,3), "mm")) + theme(axis.text=element_text(size=13), axis.title=element_text(size=18)) + theme(legend.key = element_blank())

library(gridExtra)
grid.arrange(density2, empty, scat, density, ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4))







###################################################################

##################################################################







# Generating "Party Identity Over Time"



#Load Libraries and Data
library(foreign)
library(boot)
library(gplots)
library(car)
library(ggplot2)
library(survey)
library(Hmisc)

ANESData <- read.spss("ANESData.sav", use.value.labels=FALSE, max.value.labels=Inf, to.data.frame=TRUE)
PIData <- read.spss("PIData.sav", use.value.labels=FALSE, max.value.labels=Inf, to.data.frame=TRUE)
YouGovData <- read.spss("YouGovData.sav", use.value.labels=FALSE, max.value.labels=Inf, to.data.frame=TRUE)
SSIData <- read.spss("SSIData.sav", use.value.labels=FALSE, max.value.labels=Inf, to.data.frame=TRUE)

# ANES Data
# Creating DATA (wait = 1 vector if no weights)
ANES_DATA <- ANESData
ANES_DATA$D <- -(ANESData$dpol)
ANES_DATA$pid <- ANESData$pid3lean
ANES_DATA$wait <- rep(1,length(ANES_DATA$D))
ANES_DATA$year <- rep(2008,length(ANES_DATA$D))
# Clearing NAs

#ANES_DATA <- subset(ANES_DATA, subset=pid3lean!="NA")
#ANES_DATA <- subset(ANES_DATA, subset=D!="NA")

attach(ANES_DATA)

# survey design,  Adding weights
weighted <- svydesign(id=~0, weights=~wait, data=ANES_DATA)

## Descriptives
# mat = TRUE returns results of svyby as a matrix
ANES_descriptives_matrix <- svyby(~D,~pid+year, svymean, design=weighted, na.rm=TRUE, mat=TRUE)
ANES_count_matrix<-svytable(~pid, design=weighted)

## Computing Naive Confidence Intervals from Naive Standard Errors
ANES_descriptives_matrix$minci <- ANES_descriptives_matrix$D -(1.96  * ANES_descriptives_matrix$se)
ANES_descriptives_matrix$maxci <- ANES_descriptives_matrix$D +(1.96  * ANES_descriptives_matrix$se)



# PI Data
# Creating DATA (wait = 1 vector if no weights)
PI_DATA <- PIData
PI_DATA$D <- -(PIData$newpolidd)
PI_DATA$pid <- PIData$pid3lean
PI_DATA$wait <- rep(1,length(PI_DATA$D))
PI_DATA$year <- rep(2011,length(PI_DATA$D))
# Clearing NAs

#PI_DATA <- subset(PI_DATA, subset=pid3lean!="NA")
#PI_DATA <- subset(PI_DATA, subset=D!="NA")

attach(PI_DATA)

# survey design,  Adding weights
weighted <- svydesign(id=~0, weights=~wait, data=PI_DATA)

## Descriptives
# mat = TRUE returns results of svyby as a matrix
PI_descriptives_matrix <- svyby(~D,~pid+year, svymean, design=weighted, na.rm=TRUE, mat=TRUE)
PI_count_matrix<-svytable(~pid, design=weighted)

## Computing Naive Confidence Intervals from Naive Standard Errors
PI_descriptives_matrix$minci <- PI_descriptives_matrix$D -(1.96  * PI_descriptives_matrix$se)
PI_descriptives_matrix$maxci <- PI_descriptives_matrix$D +(1.96  * PI_descriptives_matrix$se)



# YouGov Data
# Creating DATA (wait = 1 vector if no weights)
YouGov_DATA <- YouGovData
YouGov_DATA$D <- YouGovData$useiat
YouGov_DATA$pid <- YouGovData$pid3lean
YouGov_DATA$wait <- YouGovData$weight
YouGov_DATA$year <- rep(2013,length(YouGov_DATA$D))
# Clearing NAs

#YouGov_DATA <- subset(YouGov_DATA, subset=pid3lean!="NA")
#YouGov_DATA <- subset(YouGov_DATA, subset=D!="NA")

attach(YouGov_DATA)

# survey design,  Adding weights
weighted <- svydesign(id=~0, weights=~wait, data=YouGov_DATA)

## Descriptives
# mat = TRUE returns results of svyby as a matrix
YouGov_descriptives_matrix <- svyby(~D,~pid+year, svymean, design=weighted, na.rm=TRUE, mat=TRUE)
YouGov_count_matrix<-svytable(~pid, design=weighted)

## Computing Naive Confidence Intervals from Naive Standard Errors
YouGov_descriptives_matrix$minci <- YouGov_descriptives_matrix$D -(1.96  * YouGov_descriptives_matrix$se)
YouGov_descriptives_matrix$maxci <- YouGov_descriptives_matrix$D +(1.96  * YouGov_descriptives_matrix$se)



# SSI Data
# Creating DATA (wait = 1 vector if no weights)
SSI_DATA <- SSIData
SSI_DATA$D <- -(SSIData$dscore)
SSI_DATA$pid <- SSIData$pid3lean
SSI_DATA$wait <- rep(1,length(SSI_DATA$D))
SSI_DATA$year <- rep(2016,length(SSI_DATA$D))
# Clearing NAs

#SSI_DATA <- subset(SSI_DATA, subset=pid3lean!="NA")
#SSI_DATA <- subset(SSI_DATA, subset=D!="NA")

attach(SSI_DATA)

# survey design,  Adding weights
weighted <- svydesign(id=~0, weights=~wait, data=SSI_DATA)

## Descriptives
# mat = TRUE returns results of svyby as a matrix
SSI_descriptives_matrix <- svyby(~D,~pid+year, svymean, design=weighted, na.rm=TRUE, mat=TRUE)
SSI_count_matrix<-svytable(~pid, design=weighted)

## Computing Naive Confidence Intervals from Naive Standard Errors
SSI_descriptives_matrix$minci <- SSI_descriptives_matrix$D -(1.96  * SSI_descriptives_matrix$se)
SSI_descriptives_matrix$maxci <- SSI_descriptives_matrix$D +(1.96  * SSI_descriptives_matrix$se)



# Combine descriptives matrices

Overall_descriptives_matrix <- rbind(ANES_descriptives_matrix, PI_descriptives_matrix, YouGov_descriptives_matrix, SSI_descriptives_matrix)

Overall_descriptives_matrix$pid <- recode(Overall_descriptives_matrix$pid,"-1='Dem'; 0='Ind';1='Rep'")


# Change plot font size
theme_set(theme_gray(base_size = 18))


# Figure

meansplot <- ggplot(Overall_descriptives_matrix, aes(x=pid, y=D, color=pid)) + geom_point(size=3) + facet_grid(. ~ year) + theme_bw() + scale_color_manual(values = c("Dem" = "darkblue", "Ind" = "mediumorchid2", "Rep" = "firebrick1")) + geom_errorbar(aes(ymax = as.numeric(maxci), ymin = as.numeric(minci)), width=.01, size=1) + labs(x = "Respondent Explicit Party Identification", y = "Mean Implicit Party Identity: D Score") + theme(legend.title=element_blank()) + theme(legend.position="none") + expand_limits(y=c(-.5,.5)) + geom_hline(aes(yintercept=0)) + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())
meansplot



###############################################

###############################################

# Analysis of Affective Polarization




#Load Libraries and Data
library(foreign)
library(boot)
library(gplots)
library(car)
library(ggplot2)
library(survey)
library(Hmisc)

DATA <- read.spss("SSIData.sav", use.value.labels=FALSE, max.value.labels=Inf, to.data.frame=TRUE)
DATA$useiat <- -(DATA$dscore)
DATA$wait <- rep(1,length(DATA$useiat))
DATA$weight <- DATA$wait


# Clearing NAs
# DATA <- subset(DATA, subset=pid3lean!="NA")
# #DATA <- subset(DATA, subset=pid3lean!="Independent")
# DATA <- subset(DATA, subset=pid7zero!="NA")
# DATA <- subset(DATA, subset=transgresstreat!="NA")
# DATA <- subset(DATA, subset=useiat!="NA")
# #DATA <- subset(DATA, subset=useidpg!="NA")


wtd.quantile(abs(DATA$useiat), weights=DATA$weight, probs=c(.33333, .666666667), na.rm=TRUE)
wtd.quantile(DATA$useiat, weights=DATA$weight, probs=c(.33333, .666666667), na.rm=TRUE)


# Create intensity categories based upon overall distribution. 
# Dem Categories
DATA$useiat3weight[DATA$useiat > wtd.quantile(abs(DATA$useiat), weights=DATA$weight, probs=.666666667, na.rm=TRUE)] <- 3
DATA$useiat3weight[DATA$useiat > wtd.quantile(abs(DATA$useiat), weights=DATA$weight, probs=.33333333, na.rm=TRUE) & DATA$useiat <= wtd.quantile(abs(DATA$useiat), weights=DATA$weight, probs=.666666667, na.rm=TRUE)] <- 2
DATA$useiat3weight[DATA$useiat <= wtd.quantile(abs(DATA$useiat), weights=DATA$weight, probs=.33333333, na.rm=TRUE) & DATA$useiat > 0] <- 1

# Rep Categories
DATA$useiat3weight[DATA$useiat < -1*wtd.quantile(abs(DATA$useiat), weights=DATA$weight, probs=.666666667, na.rm=TRUE)] <- -3
DATA$useiat3weight[DATA$useiat < -1*wtd.quantile(abs(DATA$useiat), weights=DATA$weight, probs=.33333333, na.rm=TRUE) & DATA$useiat >= -1*wtd.quantile(abs(DATA$useiat), weights=DATA$weight, probs=.666666667, na.rm=TRUE)] <- -2
DATA$useiat3weight[DATA$useiat >= -1*wtd.quantile(abs(DATA$useiat), weights=DATA$weight, probs=.33333333, na.rm=TRUE) & DATA$useiat < 0] <- -1



# If you want only partisans
#DATA <- subset(DATA, subset=pid3lean!=0)


# Change plot font size
theme_set(theme_gray(base_size = 22))



# DemTherm over IAT6

Variables <- data.frame(depvar=DATA$demtherm, pid6=DATA$useiat3weight, weight=DATA$wait)


attach(Variables)

# survey design,  Adding weights
weighted <- svydesign(id=~0, weights=~weight, data=Variables)

## Descriptives
# mat = TRUE returns results of svyby as a matrix
descriptives_matrix <- svyby(~depvar,~pid6, svymean, design=weighted, na.rm=TRUE, mat=TRUE)
count_matrix<-svytable(~pid, design=weighted)

## Computing Naive Confidence Intervals from Naive Standard Errors
descriptives_matrix$minci <- descriptives_matrix$depvar - (1.96  * descriptives_matrix$se)
descriptives_matrix$maxci <- descriptives_matrix$depvar + (1.96  * descriptives_matrix$se)


# Figure
meansplot <- ggplot(descriptives_matrix, aes(x=factor(pid6), y=depvar, color=factor(pid6))) + scale_color_manual(values = c("-3" = "darkblue", "-2" = "darkblue", "-1" = "darkblue", "3" = "firebrick1", "2" = "firebrick1", "1" = "firebrick1")) + geom_point(size=3) + theme_bw() + geom_errorbar(aes(ymax = as.numeric(maxci), ymin = as.numeric(minci)), width=.01, size=1) + labs(y = "Feeling Thermometer: Democrats", x = "Partisan Intensity Based on the Party IAT") + theme(legend.title=element_blank()) + theme(legend.position="none") + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + scale_x_discrete(breaks=c("-3", "-2", "-1", "1", "2", "3"), labels=c("High", "Medium", "Low","Low", "Medium", "High")) + annotate("text", x = 2, y = 86, label = "Democrats", color ="darkblue", size = 5)  + annotate("text", x = 5, y = 86, label = "Republicans", color ="firebrick1", size = 5) + geom_vline(xintercept = 3.5) + ylim(18,86) + theme(axis.text=element_text(size=17), axis.title=element_text(size=16)) 

meansplot




# RepTherm over IAT6


Variables <- data.frame(depvar=DATA$reptherm, pid6=DATA$useiat3weight, weight=DATA$wait)


attach(Variables)

# survey design,  Adding weights
weighted <- svydesign(id=~0, weights=~weight, data=Variables)

## Descriptives
# mat = TRUE returns results of svyby as a matrix
descriptives_matrix <- svyby(~depvar,~pid6, svymean, design=weighted, na.rm=TRUE, mat=TRUE)
count_matrix<-svytable(~pid, design=weighted)

## Computing Naive Confidence Intervals from Naive Standard Errors
descriptives_matrix$minci <- descriptives_matrix$depvar - (1.96  * descriptives_matrix$se)
descriptives_matrix$maxci <- descriptives_matrix$depvar + (1.96  * descriptives_matrix$se)


# Figure
meansplot <- ggplot(descriptives_matrix, aes(x=factor(pid6), y=depvar, color=factor(pid6))) + scale_color_manual(values = c("-3" = "darkblue", "-2" = "darkblue", "-1" = "darkblue", "3" = "firebrick1", "2" = "firebrick1", "1" = "firebrick1")) + geom_point(size=3) + theme_bw() + geom_errorbar(aes(ymax = as.numeric(maxci), ymin = as.numeric(minci)), width=.01, size=1) + labs(y = "Feeling Thermometer: Republicans", x = "Partisan Intensity Based on the Party IAT") + theme(legend.title=element_blank()) + theme(legend.position="none") + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + scale_x_discrete(breaks=c("-3", "-2", "-1", "1", "2", "3"), labels=c("High", "Medium", "Low","Low", "Medium", "High")) + annotate("text", x = 2, y = 86, label = "Democrats", color ="darkblue", size = 5)  + annotate("text", x = 5, y = 86, label = "Republicans", color ="firebrick1", size = 5) + geom_vline(xintercept = 3.5) + ylim(18,86) + theme(axis.text=element_text(size=17), axis.title=element_text(size=16)) 

meansplot





# DemTherm over PID


Variables <- data.frame(depvar=DATA$demtherm, pid6=DATA$pid7, weight=DATA$wait)
Variables <- subset(Variables, subset=pid6!=0)


attach(Variables)

# survey design,  Adding weights
weighted <- svydesign(id=~0, weights=~weight, data=Variables)

## Descriptives
# mat = TRUE returns results of svyby as a matrix
descriptives_matrix <- svyby(~depvar,~pid6, svymean, design=weighted, na.rm=TRUE, mat=TRUE)
count_matrix<-svytable(~pid, design=weighted)

## Computing Naive Confidence Intervals from Naive Standard Errors
descriptives_matrix$minci <- descriptives_matrix$depvar - (1.96  * descriptives_matrix$se)
descriptives_matrix$maxci <- descriptives_matrix$depvar + (1.96  * descriptives_matrix$se)


# Figure
meansplot <- ggplot(descriptives_matrix, aes(x=factor(pid6), y=depvar, color=factor(pid6))) + scale_color_manual(values = c("-3" = "darkblue", "-2" = "darkblue", "-1" = "darkblue", "3" = "firebrick1", "2" = "firebrick1", "1" = "firebrick1")) + geom_point(size=3) + theme_bw() + geom_errorbar(aes(ymax = as.numeric(maxci), ymin = as.numeric(minci)), width=.01, size=1) + labs(y = "Feeling Thermometer: Democrats", x = "Partisan Intensity Based on Explicit Seven-Point PID") + theme(legend.title=element_blank()) + theme(legend.position="none") + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + scale_x_discrete(breaks=c("-3", "-2", "-1", "1", "2", "3"), labels=c("High", "Medium", "Low","Low", "Medium", "High")) + annotate("text", x = 2, y = 86, label = "Democrats", color ="darkblue", size = 5)  + annotate("text", x = 5, y = 86, label = "Republicans", color ="firebrick1", size = 5) + geom_vline(xintercept = 3.5) + ylim(18,86) + theme(axis.text=element_text(size=17), axis.title=element_text(size=16)) 

meansplot




# RepTherm over PID


Variables <- data.frame(depvar=DATA$reptherm, pid6=DATA$pid7, weight=DATA$wait)
Variables <- subset(Variables, subset=pid6!=0)


attach(Variables)

# survey design,  Adding weights
weighted <- svydesign(id=~0, weights=~weight, data=Variables)

## Descriptives
# mat = TRUE returns results of svyby as a matrix
descriptives_matrix <- svyby(~depvar,~pid6, svymean, design=weighted, na.rm=TRUE, mat=TRUE)
count_matrix<-svytable(~pid, design=weighted)

## Computing Naive Confidence Intervals from Naive Standard Errors
descriptives_matrix$minci <- descriptives_matrix$depvar - (1.96  * descriptives_matrix$se)
descriptives_matrix$maxci <- descriptives_matrix$depvar + (1.96  * descriptives_matrix$se)


# Figure
meansplot <- ggplot(descriptives_matrix, aes(x=factor(pid6), y=depvar, color=factor(pid6))) + scale_color_manual(values = c("-3" = "darkblue", "-2" = "darkblue", "-1" = "darkblue", "3" = "firebrick1", "2" = "firebrick1", "1" = "firebrick1")) + geom_point(size=3) + theme_bw() + geom_errorbar(aes(ymax = as.numeric(maxci), ymin = as.numeric(minci)), width=.01, size=1) + labs(y = "Feeling Thermometer: Republicans", x = "Partisan Intensity Based on Explicit Seven-Point PID") + theme(legend.title=element_blank()) + theme(legend.position="none") + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + scale_x_discrete(breaks=c("-3", "-2", "-1", "1", "2", "3"), labels=c("High", "Medium", "Low","Low", "Medium", "High")) + annotate("text", x = 2, y = 86, label = "Democrats", color ="darkblue", size = 5)  + annotate("text", x = 5, y = 86, label = "Republicans", color ="firebrick1", size = 5) + geom_vline(xintercept = 3.5) + ylim(18,86) + theme(axis.text=element_text(size=17), axis.title=element_text(size=16)) 

meansplot













# RepTherm - DemTherm over IAT6


Variables <- data.frame(depvar=(DATA$reptherm-DATA$demtherm), pid6=DATA$useiat3weight, weight=DATA$wait)

attach(Variables)

# survey design,  Adding weights
weighted <- svydesign(id=~0, weights=~weight, data=Variables)

## Descriptives
# mat = TRUE returns results of svyby as a matrix
descriptives_matrix <- svyby(~depvar,~pid6, svymean, design=weighted, na.rm=TRUE, mat=TRUE)
count_matrix<-svytable(~pid, design=weighted)

## Computing Naive Confidence Intervals from Naive Standard Errors
descriptives_matrix$minci <- descriptives_matrix$depvar - (1.96  * descriptives_matrix$se)
descriptives_matrix$maxci <- descriptives_matrix$depvar + (1.96  * descriptives_matrix$se)


# Figure
meansplot <- ggplot(descriptives_matrix, aes(x=factor(pid6), y=depvar, color=factor(pid6))) + scale_color_manual(values = c("-3" = "darkblue", "-2" = "darkblue", "-1" = "darkblue", "3" = "firebrick1", "2" = "firebrick1", "1" = "firebrick1")) + geom_point(size=3) + theme_bw() + geom_errorbar(aes(ymax = as.numeric(maxci), ymin = as.numeric(minci)), width=.01, size=1) + labs(y = "Feeling Thermometer Diff: Republicans - Democrats", x = "Partisan Intensity Based on the Party IAT") + theme(legend.title=element_blank()) + theme(legend.position="none") + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + scale_x_discrete(breaks=c("-3", "-2", "-1", "1", "2", "3"), labels=c("High", "Medium", "Low","Low", "Medium", "High")) + annotate("text", x = 2, y = 65, label = "Democrats", color ="darkblue", size = 5)  + annotate("text", x = 5, y = 65, label = "Republicans", color ="firebrick1", size = 5) + geom_vline(xintercept = 3.5) + ylim(-65,65) + theme(axis.text=element_text(size=17), axis.title=element_text(size=16)) 

meansplot





# RepTherm - DemTherm over PID


Variables <- data.frame(depvar=(DATA$reptherm-DATA$demtherm), pid6=DATA$pid7, weight=DATA$wait)
Variables <- subset(Variables, subset=pid6!=0)


attach(Variables)

# survey design,  Adding weights
weighted <- svydesign(id=~0, weights=~weight, data=Variables)

## Descriptives
# mat = TRUE returns results of svyby as a matrix
descriptives_matrix <- svyby(~depvar,~pid6, svymean, design=weighted, na.rm=TRUE, mat=TRUE)
count_matrix<-svytable(~pid, design=weighted)

## Computing Naive Confidence Intervals from Naive Standard Errors
descriptives_matrix$minci <- descriptives_matrix$depvar - (1.96  * descriptives_matrix$se)
descriptives_matrix$maxci <- descriptives_matrix$depvar + (1.96  * descriptives_matrix$se)


# Figure
meansplot <- ggplot(descriptives_matrix, aes(x=factor(pid6), y=depvar, color=factor(pid6))) + scale_color_manual(values = c("-3" = "darkblue", "-2" = "darkblue", "-1" = "darkblue", "3" = "firebrick1", "2" = "firebrick1", "1" = "firebrick1")) + geom_point(size=3) + theme_bw() + geom_errorbar(aes(ymax = as.numeric(maxci), ymin = as.numeric(minci)), width=.01, size=1) + labs(y = "Feeling Thermometer Diff: Republicans - Democrats", x = "Partisan Intensity Based on Explicit Seven-Point PID") + theme(legend.title=element_blank()) + theme(legend.position="none") + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + scale_x_discrete(breaks=c("-3", "-2", "-1", "1", "2", "3"), labels=c("High", "Medium", "Low","Low", "Medium", "High")) + annotate("text", x = 2, y = 65, label = "Democrats", color ="darkblue", size = 5)  + annotate("text", x = 5, y = 65, label = "Republicans", color ="firebrick1", size = 5) + geom_vline(xintercept = 3.5) + ylim(-65,65) + theme(axis.text=element_text(size=17), axis.title=element_text(size=16)) 

meansplot









################################################

###############################################

# Analysis of Boosting



#Load Libraries and Data
library(foreign)
library(boot)
library(gplots)
library(car)
library(ggplot2)
library(survey)
library(Hmisc)

DATA <- read.spss("YouGovData.sav", use.value.labels=FALSE, max.value.labels=Inf, to.data.frame=TRUE)



# Recoding trait variables

# Recoding Republican Ratings
DATA$RepMoral <- (as.numeric(DATA$PartyTrait_Rep_mor) -4)
DATA$RepStrong <- (as.numeric(DATA$PartyTrait_Rep_str) -4)
DATA$RepComp <- (as.numeric(DATA$PartyTrait_Rep_com) -4)
DATA$RepCare <- (as.numeric(DATA$PartyTrait_Rep_car) -4)
DATA$RepKnow <- (as.numeric(DATA$PartyTrait_Rep_kno) -4)
DATA$RepHardWork <- (as.numeric(DATA$PartyTrait_Rep_har) -4)
DATA$RepHonest <- (as.numeric(DATA$PartyTrait_Rep_hon) -4)
DATA$RepInspiring <- (as.numeric(DATA$PartyTrait_Rep_ins) -4)

DATA$RepEval <- DATA$RepMoral + DATA$RepStrong + DATA$RepComp + DATA$RepCare + DATA$RepKnow + DATA$RepHardWork + DATA$RepHonest + DATA$RepInspiring


# Rating Democrats
DATA$DemMoral <- (as.numeric(DATA$PartyTrait_Dem_mor) -4)
DATA$DemStrong <- (as.numeric(DATA$PartyTrait_Dem_str) -4)
DATA$DemComp <- (as.numeric(DATA$PartyTrait_Dem_com) -4)
DATA$DemCare <- (as.numeric(DATA$PartyTrait_Dem_car) -4)
DATA$DemKnow <- (as.numeric(DATA$PartyTrait_Dem_kno) -4)
DATA$DemHardWork <- (as.numeric(DATA$PartyTrait_Dem_har) -4)
DATA$DemHonest <- (as.numeric(DATA$PartyTrait_Dem_hon) -4)
DATA$DemInspiring <- (as.numeric(DATA$PartyTrait_Dem_ins) -4)

DATA$DemEval <- DATA$DemMoral + DATA$DemStrong + DATA$DemComp + DATA$DemCare + DATA$DemKnow + DATA$DemHardWork + DATA$DemHonest + DATA$DemInspiring


wtd.quantile(abs(DATA$useiat), weights=DATA$weight, probs=c(.33333, .666666667), na.rm=TRUE)
wtd.quantile(DATA$useiat, weights=DATA$weight, probs=c(.33333, .666666667), na.rm=TRUE)


# Create intensity categories based upon overall distribution. 
# Dem Categories
DATA$useiat3weight[DATA$useiat > wtd.quantile(abs(DATA$useiat), weights=DATA$weight, probs=.666666667, na.rm=TRUE)] <- 3
DATA$useiat3weight[DATA$useiat > wtd.quantile(abs(DATA$useiat), weights=DATA$weight, probs=.33333333, na.rm=TRUE) & DATA$useiat <= wtd.quantile(abs(DATA$useiat), weights=DATA$weight, probs=.666666667, na.rm=TRUE)] <- 2
DATA$useiat3weight[DATA$useiat <= wtd.quantile(abs(DATA$useiat), weights=DATA$weight, probs=.33333333, na.rm=TRUE) & DATA$useiat > 0] <- 1

# Rep Categories
DATA$useiat3weight[DATA$useiat < -1*wtd.quantile(abs(DATA$useiat), weights=DATA$weight, probs=.666666667, na.rm=TRUE)] <- -3
DATA$useiat3weight[DATA$useiat < -1*wtd.quantile(abs(DATA$useiat), weights=DATA$weight, probs=.33333333, na.rm=TRUE) & DATA$useiat >= -1*wtd.quantile(abs(DATA$useiat), weights=DATA$weight, probs=.666666667, na.rm=TRUE)] <- -2
DATA$useiat3weight[DATA$useiat >= -1*wtd.quantile(abs(DATA$useiat), weights=DATA$weight, probs=.33333333, na.rm=TRUE) & DATA$useiat < 0] <- -1



# Change plot font size
theme_set(theme_gray(base_size = 18))

# Evaluative Boosting over IAT6

Variables <- data.frame(pid6=DATA$useiat3weight, weight=DATA$weight, DemEval=DATA$DemEval, RepEval= DATA$RepEval)

# Clearing NAs
Variables <- subset(Variables, subset=pid6!="NA")
Variables <- subset(Variables, subset=weight!="NA")
Variables <- subset(Variables, subset=DemEval!="NA")
Variables <- subset(Variables, subset=RepEval!="NA")


Variables$depvar <- NULL
Variables$depvar <- Variables$RepEval - Variables$DemEval

attach(Variables)

# survey design,  Adding weights
weighted <- svydesign(id=~0, weights=~weight, data=Variables)

## Descriptives
# mat = TRUE returns results of svyby as a matrix
descriptives_matrix <- svyby(~depvar,~pid6, svymean, design=weighted, na.rm=TRUE, mat=TRUE)
count_matrix<-svytable(~pid, design=weighted)

## Computing Naive Confidence Intervals from Naive Standard Errors
descriptives_matrix$minci <- descriptives_matrix$depvar - (1.96  * descriptives_matrix$se)
descriptives_matrix$maxci <- descriptives_matrix$depvar + (1.96  * descriptives_matrix$se)



# Figure
meansplot <- ggplot(descriptives_matrix, aes(x=factor(pid6), y=depvar, color=factor(pid6))) + scale_color_manual(values = c("-3" = "darkblue", "-2" = "darkblue", "-1" = "darkblue", "3" = "firebrick1", "2" = "firebrick1", "1" = "firebrick1")) + geom_point(size=3) + theme_bw() + geom_errorbar(aes(ymax = as.numeric(maxci), ymin = as.numeric(minci)), width=.01, size=1) + labs(y = "Overall Trait Ratings: Republicans - Democrats", x = "Partisan Intensity Based on the Party IAT") + theme(legend.title=element_blank()) + theme(legend.position="none") + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + scale_x_discrete(breaks=c("-3", "-2", "-1", "1", "2", "3"), labels=c("High", "Medium", "Low","Low", "Medium", "High")) + annotate("text", x = 2, y = 25, label = "Democrats", color ="darkblue", size = 5)  + annotate("text", x = 5, y = 25, label = "Republicans", color ="firebrick1", size = 5) + geom_vline(xintercept = 3.5) + ylim(-25,25) + theme(axis.text=element_text(size=17), axis.title=element_text(size=16)) 

meansplot




# Evaluative Boosting over PID 6

Variables <- data.frame(pid6=DATA$pid7zero, weight=DATA$weight, DemEval=DATA$DemEval, RepEval= DATA$RepEval)
Variables <- subset(Variables, subset=pid6!=0)

# Clearing NAs
Variables <- subset(Variables, subset=pid6!="NA")
Variables <- subset(Variables, subset=weight!="NA")
Variables <- subset(Variables, subset=DemEval!="NA")
Variables <- subset(Variables, subset=RepEval!="NA")


Variables$depvar <- NULL
Variables$depvar <- Variables$RepEval - Variables$DemEval

attach(Variables)

# survey design,  Adding weights
weighted <- svydesign(id=~0, weights=~weight, data=Variables)

## Descriptives
# mat = TRUE returns results of svyby as a matrix
descriptives_matrix <- svyby(~depvar,~pid6, svymean, design=weighted, na.rm=TRUE, mat=TRUE)
count_matrix<-svytable(~pid, design=weighted)

## Computing Naive Confidence Intervals from Naive Standard Errors
descriptives_matrix$minci <- descriptives_matrix$depvar - (1.96  * descriptives_matrix$se)
descriptives_matrix$maxci <- descriptives_matrix$depvar + (1.96  * descriptives_matrix$se)


# Figure
meansplot <- ggplot(descriptives_matrix, aes(x=factor(pid6), y=depvar, color=factor(pid6))) + scale_color_manual(values = c("-3" = "darkblue", "-2" = "darkblue", "-1" = "darkblue", "3" = "firebrick1", "2" = "firebrick1", "1" = "firebrick1")) + geom_point(size=3) + theme_bw() + geom_errorbar(aes(ymax = as.numeric(maxci), ymin = as.numeric(minci)), width=.01, size=1) + labs(y = "Overall Trait Ratings: Republicans - Democrats", x = "Partisan Intensity Based on Explicit Seven-Point PID") + theme(legend.title=element_blank()) + theme(legend.position="none") + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + scale_x_discrete(breaks=c("-3", "-2", "-1", "1", "2", "3"), labels=c("High", "Medium", "Low","Low", "Medium", "High")) + annotate("text", x = 2, y = 25, label = "Democrats", color ="darkblue", size = 5)  + annotate("text", x = 5, y = 25, label = "Republicans", color ="firebrick1", size = 5) + geom_vline(xintercept = 3.5) + ylim(-25,25) + theme(axis.text=element_text(size=17), axis.title=element_text(size=16)) 

meansplot









# Evaluative Boosting over IDPG6

Variables <- data.frame(pid6=DATA$useidpg6weight, weight=DATA$weight, DemEval=DATA$DemEval, RepEval= DATA$RepEval)

# Clearing NAs
Variables <- subset(Variables, subset=pid6!="NA")
Variables <- subset(Variables, subset=weight!="NA")
Variables <- subset(Variables, subset=DemEval!="NA")
Variables <- subset(Variables, subset=RepEval!="NA")


Variables$depvar <- NULL
Variables$depvar <- Variables$RepEval - Variables$DemEval

attach(Variables)

# survey design,  Adding weights
weighted <- svydesign(id=~0, weights=~weight, data=Variables)

## Descriptives
# mat = TRUE returns results of svyby as a matrix
descriptives_matrix <- svyby(~depvar,~pid6, svymean, design=weighted, na.rm=TRUE, mat=TRUE)
count_matrix<-svytable(~pid, design=weighted)

## Computing Naive Confidence Intervals from Naive Standard Errors
descriptives_matrix$minci <- descriptives_matrix$depvar - (1.96  * descriptives_matrix$se)
descriptives_matrix$maxci <- descriptives_matrix$depvar + (1.96  * descriptives_matrix$se)



# Figure
meansplot <- ggplot(descriptives_matrix, aes(x=factor(pid6), y=depvar, color=factor(pid6))) + scale_color_manual(values = c("-3" = "darkblue", "-2" = "darkblue", "-1" = "darkblue", "3" = "firebrick1", "2" = "firebrick1", "1" = "firebrick1")) + geom_point(size=3) + theme_bw() + geom_errorbar(aes(ymax = as.numeric(maxci), ymin = as.numeric(minci)), width=.01, size=1) + labs(y = "Overall Trait Ratings: Republicans - Democrats", x = "Partisan Intensity Based on the IDPG") + theme(legend.title=element_blank()) + theme(legend.position="none") + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + scale_x_discrete(breaks=c("-3", "-2", "-1", "1", "2", "3"), labels=c("High", "Medium", "Low","Low", "Medium", "High")) + annotate("text", x = 2, y = 25, label = "Democrats", color ="darkblue", size = 5)  + annotate("text", x = 5, y = 25, label = "Republicans", color ="firebrick1", size = 5) + geom_vline(xintercept = 3.5) + ylim(-25,25) + theme(axis.text=element_text(size=17), axis.title=element_text(size=16)) 

meansplot





###############################################

###############################################



# Analysis of Bias




# Using the IAT 6
#Load Libraries and Data
library(foreign)
library(boot)
library(gplots)
library(car)
library(ggplot2)
library(survey)
library(Hmisc)

DATA <- read.spss("YouGovData.sav", use.value.labels=FALSE, max.value.labels=Inf, to.data.frame=TRUE)

# Clearing NAs
# DATA <- subset(DATA, subset=pid3lean!="NA")
# #DATA <- subset(DATA, subset=pid3lean!="Independent")
# DATA <- subset(DATA, subset=pid7zero!="NA")
# DATA <- subset(DATA, subset=transgresstreat!="NA")
# DATA <- subset(DATA, subset=useiat!="NA")
# #DATA <- subset(DATA, subset=useidpg!="NA")


wtd.quantile(abs(DATA$useiat), weights=DATA$weight, probs=c(.33333, .666666667), na.rm=TRUE)
wtd.quantile(DATA$useiat, weights=DATA$weight, probs=c(.33333, .666666667), na.rm=TRUE)


# Create intensity categories based upon overall distribution. 
# Dem Categories
DATA$useiat3weight[DATA$useiat > wtd.quantile(abs(DATA$useiat), weights=DATA$weight, probs=.666666667, na.rm=TRUE)] <- 3
DATA$useiat3weight[DATA$useiat > wtd.quantile(abs(DATA$useiat), weights=DATA$weight, probs=.33333333, na.rm=TRUE) & DATA$useiat <= wtd.quantile(abs(DATA$useiat), weights=DATA$weight, probs=.666666667, na.rm=TRUE)] <- 2
DATA$useiat3weight[DATA$useiat <= wtd.quantile(abs(DATA$useiat), weights=DATA$weight, probs=.33333333, na.rm=TRUE) & DATA$useiat > 0] <- 1

# Rep Categories
DATA$useiat3weight[DATA$useiat < -1*wtd.quantile(abs(DATA$useiat), weights=DATA$weight, probs=.666666667, na.rm=TRUE)] <- -3
DATA$useiat3weight[DATA$useiat < -1*wtd.quantile(abs(DATA$useiat), weights=DATA$weight, probs=.33333333, na.rm=TRUE) & DATA$useiat >= -1*wtd.quantile(abs(DATA$useiat), weights=DATA$weight, probs=.666666667, na.rm=TRUE)] <- -2
DATA$useiat3weight[DATA$useiat >= -1*wtd.quantile(abs(DATA$useiat), weights=DATA$weight, probs=.33333333, na.rm=TRUE) & DATA$useiat < 0] <- -1

Variables <- data.frame(depvar=DATA$totalgrid, pid6=DATA$useiat3weight,transgress=DATA$transgresstreat, weight=DATA$weight)






# Creating Null vectors

Dembias<-NULL
Repbias<-NULL


StrongDembias<-NULL
NotStrongDembias<-NULL
LeanDembias<-NULL
LeanRepbias<-NULL
NotStrongRepbias<-NULL
StrongRepbias<-NULL



# Bootstrapping Bias

for (b in 1:10000) {
  
  bsrows <- sample(1:nrow(Variables),nrow(Variables),replace=T, prob=Variables$weight)
  
  Partisans <- Variables[bsrows,]
  
  Democrats <- subset(Partisans, subset=pid6<0)
  Republicans <- subset(Partisans, subset=pid6>0)
  
  DemsRep <- subset(Democrats, subset=transgress==1)
  RepsRep <- subset(Republicans, subset=transgress==1)
  DemsDem <- subset(Democrats, subset=transgress==-1)
  RepsDem <- subset(Republicans, subset=transgress==-1)
  #IndRep <- subset(Independents, subset=transgress==1)
  #IndDem <- subset(Independents, subset=transgress==-1)
  
  Dembias <- c(Dembias, mean(DemsRep$depvar, na.rm=T)-mean(DemsDem$depvar, na.rm=T))
  Repbias <- c(Repbias, mean(RepsDem$depvar, na.rm=T)-mean(RepsRep$depvar, na.rm=T))
  
  # Getting Subsets for each of the 6 levels
  StrongDems <- subset(Partisans, subset=pid6==-3)
  NotStrongDems <- subset(Partisans, subset=pid6==-2)
  LeanDems <- subset(Partisans, subset=pid6==-1)
  LeanReps <- subset(Partisans, subset=pid6==1)
  NotStrongReps <- subset(Partisans, subset=pid6==2)
  StrongReps <- subset(Partisans, subset=pid6==3)
  
  StrongDemsRep <- subset(StrongDems, subset=transgress==1)
  StrongRepsRep <- subset(StrongReps, subset=transgress==1)
  StrongDemsDem <- subset(StrongDems, subset=transgress==-1)
  StrongRepsDem <- subset(StrongReps, subset=transgress==-1)
  NotStrongDemsRep <- subset(NotStrongDems, subset=transgress==1)
  NotStrongRepsRep <- subset(NotStrongReps, subset=transgress==1)
  NotStrongDemsDem <- subset(NotStrongDems, subset=transgress==-1)
  NotStrongRepsDem <- subset(NotStrongReps, subset=transgress==-1)
  LeanDemsRep <- subset(LeanDems, subset=transgress==1)
  LeanRepsRep <- subset(LeanReps, subset=transgress==1)
  LeanDemsDem <- subset(LeanDems, subset=transgress==-1)
  LeanRepsDem <- subset(LeanReps, subset=transgress==-1)
  
  StrongDembias <- c(StrongDembias, mean(StrongDemsRep$depvar, na.rm=T)-mean(StrongDemsDem$depvar, na.rm=T))
  NotStrongDembias <- c(NotStrongDembias, mean(NotStrongDemsRep$depvar, na.rm=T)-mean(NotStrongDemsDem$depvar, na.rm=T))
  LeanDembias <- c(LeanDembias, mean(LeanDemsRep$depvar, na.rm=T)-mean(LeanDemsDem$depvar, na.rm=T))
  LeanRepbias <- c(LeanRepbias, mean(LeanRepsDem$depvar, na.rm=T)-mean(LeanRepsRep$depvar, na.rm=T))
  NotStrongRepbias <- c(NotStrongRepbias, mean(NotStrongRepsDem$depvar, na.rm=T)-mean(NotStrongRepsRep$depvar, na.rm=T))
  StrongRepbias <- c(StrongRepbias, mean(StrongRepsDem$depvar, na.rm=T)-mean(StrongRepsRep$depvar, na.rm=T))
  
  
}



#Plotting Bias

library(gplots) 
hh <- c(-mean(StrongDembias),-mean(NotStrongDembias),-mean(LeanDembias),mean(LeanRepbias),mean(NotStrongRepbias),mean(StrongRepbias))
mybarcol <- "gray20"
ci.l <- c(-quantile(StrongDembias,.025), -quantile(NotStrongDembias,.025), -quantile(LeanDembias,.025), quantile(LeanRepbias,.025), quantile(NotStrongRepbias,.025), quantile(StrongRepbias,.025))
ci.u <- c(-quantile(StrongDembias,.975), -quantile(NotStrongDembias,.975), -quantile(LeanDembias,.975), quantile(LeanRepbias,.975), quantile(NotStrongRepbias,.975), quantile(StrongRepbias,.975))
barplot2(hh, beside = TRUE, horiz=FALSE,
         col = c("darkblue", "darkblue", "darkblue", 
                 "firebrick1", "firebrick1", "firebrick1"), 
         names.arg=c("High", "Medium", "Low","Low", "Medium", "High"),
         xlab = "Partisan Intensity Based on the Party IAT",
         ylab = "Motivated Processing: Republican - Democrat",
         cex.lab = 1.3,
         ci.lwd = 3,
         ci.color = c("steelblue2", "steelblue2", "steelblue2", 
                      "red4", "red4", "red4"), 
         cex.names = 1.5, plot.ci = TRUE, ci.l = ci.l, ci.u = ci.u,
         plot.grid = TRUE) 










# Using PID 7
#Load Libraries and Data
library(foreign)
library(boot)
library(gplots)
library(car)
library(ggplot2)
library(survey)

DATA <- read.spss("YouGovData.sav", use.value.labels=FALSE, max.value.labels=Inf, to.data.frame=TRUE)

# Clearing NAs
# DATA <- subset(DATA, subset=pid3lean!="NA")
# #DATA <- subset(DATA, subset=pid3lean!="Independent")
# DATA <- subset(DATA, subset=pid7zero!="NA")
# DATA <- subset(DATA, subset=transgresstreat!="NA")
# DATA <- subset(DATA, subset=useiat!="NA")
# DATA <- subset(DATA, subset=useidpg!="NA")


Variables <- data.frame(depvar=DATA$totalgrid, pid6=DATA$pid7zero,transgress=DATA$transgresstreat, weight=DATA$weight)


# Creating Null vectors

Dembias<-NULL
Repbias<-NULL


StrongDembias<-NULL
NotStrongDembias<-NULL
LeanDembias<-NULL
LeanRepbias<-NULL
NotStrongRepbias<-NULL
StrongRepbias<-NULL



# Bootstrapping Bias

for (b in 1:10000) {
  
  bsrows <- sample(1:nrow(Variables),nrow(Variables),replace=T, prob=Variables$weight)
  
  Partisans <- Variables[bsrows,]
  
  Democrats <- subset(Partisans, subset=pid6<0)
  Republicans <- subset(Partisans, subset=pid6>0)
  
  DemsRep <- subset(Democrats, subset=transgress==1)
  RepsRep <- subset(Republicans, subset=transgress==1)
  DemsDem <- subset(Democrats, subset=transgress==-1)
  RepsDem <- subset(Republicans, subset=transgress==-1)
  #IndRep <- subset(Independents, subset=transgress==1)
  #IndDem <- subset(Independents, subset=transgress==-1)
  
  
  
  Dembias <- c(Dembias, mean(DemsRep$depvar, na.rm=T)-mean(DemsDem$depvar, na.rm=T))
  Repbias <- c(Repbias, mean(RepsDem$depvar, na.rm=T)-mean(RepsRep$depvar, na.rm=T))
  
  # PID7
  
  StrongDems <- subset(Partisans, subset=pid6==-3)
  NotStrongDems <- subset(Partisans, subset=pid6==-2)
  LeanDems <- subset(Partisans, subset=pid6==-1)
  LeanReps <- subset(Partisans, subset=pid6==1)
  NotStrongReps <- subset(Partisans, subset=pid6==2)
  StrongReps <- subset(Partisans, subset=pid6==3)
  
  StrongDemsRep <- subset(StrongDems, subset=transgress==1)
  StrongRepsRep <- subset(StrongReps, subset=transgress==1)
  StrongDemsDem <- subset(StrongDems, subset=transgress==-1)
  StrongRepsDem <- subset(StrongReps, subset=transgress==-1)
  NotStrongDemsRep <- subset(NotStrongDems, subset=transgress==1)
  NotStrongRepsRep <- subset(NotStrongReps, subset=transgress==1)
  NotStrongDemsDem <- subset(NotStrongDems, subset=transgress==-1)
  NotStrongRepsDem <- subset(NotStrongReps, subset=transgress==-1)
  LeanDemsRep <- subset(LeanDems, subset=transgress==1)
  LeanRepsRep <- subset(LeanReps, subset=transgress==1)
  LeanDemsDem <- subset(LeanDems, subset=transgress==-1)
  LeanRepsDem <- subset(LeanReps, subset=transgress==-1)
  
  StrongDembias <- c(StrongDembias, mean(StrongDemsRep$depvar, na.rm=T)-mean(StrongDemsDem$depvar, na.rm=T))
  NotStrongDembias <- c(NotStrongDembias, mean(NotStrongDemsRep$depvar, na.rm=T)-mean(NotStrongDemsDem$depvar, na.rm=T))
  LeanDembias <- c(LeanDembias, mean(LeanDemsRep$depvar, na.rm=T)-mean(LeanDemsDem$depvar, na.rm=T))
  LeanRepbias <- c(LeanRepbias, mean(LeanRepsDem$depvar, na.rm=T)-mean(LeanRepsRep$depvar, na.rm=T))
  NotStrongRepbias <- c(NotStrongRepbias, mean(NotStrongRepsDem$depvar, na.rm=T)-mean(NotStrongRepsRep$depvar, na.rm=T))
  StrongRepbias <- c(StrongRepbias, mean(StrongRepsDem$depvar, na.rm=T)-mean(StrongRepsRep$depvar, na.rm=T))
  
  
}


#Plotting Bias

library(gplots) 
hh <- c(-mean(StrongDembias),-mean(NotStrongDembias),-mean(LeanDembias),mean(LeanRepbias),mean(NotStrongRepbias),mean(StrongRepbias))
mybarcol <- "gray20"
ci.l <- c(-quantile(StrongDembias,.025), -quantile(NotStrongDembias,.025), -quantile(LeanDembias,.025), quantile(LeanRepbias,.025), quantile(NotStrongRepbias,.025), quantile(StrongRepbias,.025))
ci.u <- c(-quantile(StrongDembias,.975), -quantile(NotStrongDembias,.975), -quantile(LeanDembias,.975), quantile(LeanRepbias,.975), quantile(NotStrongRepbias,.975), quantile(StrongRepbias,.975))
barplot2(hh, beside = TRUE, horiz=FALSE,
         col = c("darkblue", "darkblue", "darkblue", 
                 "firebrick1", "firebrick1", "firebrick1"), 
         names.arg=c("High", "Medium", "Low","Low", "Medium", "High"),
         xlab = "Partisan Intensity Based on Explicit Seven-Point PID",
         ylab = "Motivated Processing: Republican - Democrat",
         cex.lab = 1.3,
         ci.lwd = 3, 
         ci.color = c("steelblue2", "steelblue2", "steelblue2", 
                      "red4", "red4", "red4"), 
         cex.names = 1.5, plot.ci = TRUE, ci.l = ci.l, ci.u = ci.u,
         plot.grid = TRUE) 







# Using IDPG 6
#Load Libraries and Data
library(foreign)
library(boot)
library(gplots)
library(car)
library(ggplot2)
library(survey)

DATA <- read.spss("YouGovData.sav", use.value.labels=FALSE, max.value.labels=Inf, to.data.frame=TRUE)

# Clearing NAs
# DATA <- subset(DATA, subset=pid3lean!="NA")
# #DATA <- subset(DATA, subset=pid3lean!="Independent")
# DATA <- subset(DATA, subset=pid7zero!="NA")
# DATA <- subset(DATA, subset=transgresstreat!="NA")
# DATA <- subset(DATA, subset=useiat!="NA")
# DATA <- subset(DATA, subset=useidpg!="NA")


Variables <- data.frame(depvar=DATA$totalgrid, pid6=DATA$useidpg6weight,transgress=DATA$transgresstreat, weight=DATA$weight)


# Creating Null vectors

Dembias<-NULL
Repbias<-NULL


StrongDembias<-NULL
NotStrongDembias<-NULL
LeanDembias<-NULL
LeanRepbias<-NULL
NotStrongRepbias<-NULL
StrongRepbias<-NULL



# Bootstrapping Bias

for (b in 1:10000) {
  
  bsrows <- sample(1:nrow(Variables),nrow(Variables),replace=T, prob=Variables$weight)
  
  Partisans <- Variables[bsrows,]
  
  Democrats <- subset(Partisans, subset=pid6<0)
  Republicans <- subset(Partisans, subset=pid6>0)
  
  DemsRep <- subset(Democrats, subset=transgress==1)
  RepsRep <- subset(Republicans, subset=transgress==1)
  DemsDem <- subset(Democrats, subset=transgress==-1)
  RepsDem <- subset(Republicans, subset=transgress==-1)
  #IndRep <- subset(Independents, subset=transgress==1)
  #IndDem <- subset(Independents, subset=transgress==-1)
  
  
  
  Dembias <- c(Dembias, mean(DemsRep$depvar, na.rm=T)-mean(DemsDem$depvar, na.rm=T))
  Repbias <- c(Repbias, mean(RepsDem$depvar, na.rm=T)-mean(RepsRep$depvar, na.rm=T))
  
  # PID7
  
  StrongDems <- subset(Partisans, subset=pid6==-3)
  NotStrongDems <- subset(Partisans, subset=pid6==-2)
  LeanDems <- subset(Partisans, subset=pid6==-1)
  LeanReps <- subset(Partisans, subset=pid6==1)
  NotStrongReps <- subset(Partisans, subset=pid6==2)
  StrongReps <- subset(Partisans, subset=pid6==3)
  
  StrongDemsRep <- subset(StrongDems, subset=transgress==1)
  StrongRepsRep <- subset(StrongReps, subset=transgress==1)
  StrongDemsDem <- subset(StrongDems, subset=transgress==-1)
  StrongRepsDem <- subset(StrongReps, subset=transgress==-1)
  NotStrongDemsRep <- subset(NotStrongDems, subset=transgress==1)
  NotStrongRepsRep <- subset(NotStrongReps, subset=transgress==1)
  NotStrongDemsDem <- subset(NotStrongDems, subset=transgress==-1)
  NotStrongRepsDem <- subset(NotStrongReps, subset=transgress==-1)
  LeanDemsRep <- subset(LeanDems, subset=transgress==1)
  LeanRepsRep <- subset(LeanReps, subset=transgress==1)
  LeanDemsDem <- subset(LeanDems, subset=transgress==-1)
  LeanRepsDem <- subset(LeanReps, subset=transgress==-1)
  
  StrongDembias <- c(StrongDembias, mean(StrongDemsRep$depvar, na.rm=T)-mean(StrongDemsDem$depvar, na.rm=T))
  NotStrongDembias <- c(NotStrongDembias, mean(NotStrongDemsRep$depvar, na.rm=T)-mean(NotStrongDemsDem$depvar, na.rm=T))
  LeanDembias <- c(LeanDembias, mean(LeanDemsRep$depvar, na.rm=T)-mean(LeanDemsDem$depvar, na.rm=T))
  LeanRepbias <- c(LeanRepbias, mean(LeanRepsDem$depvar, na.rm=T)-mean(LeanRepsRep$depvar, na.rm=T))
  NotStrongRepbias <- c(NotStrongRepbias, mean(NotStrongRepsDem$depvar, na.rm=T)-mean(NotStrongRepsRep$depvar, na.rm=T))
  StrongRepbias <- c(StrongRepbias, mean(StrongRepsDem$depvar, na.rm=T)-mean(StrongRepsRep$depvar, na.rm=T))
  
  
}


#Plotting Bias

library(gplots) 
hh <- c(-mean(StrongDembias),-mean(NotStrongDembias),-mean(LeanDembias),mean(LeanRepbias),mean(NotStrongRepbias),mean(StrongRepbias))
mybarcol <- "gray20"
ci.l <- c(-quantile(StrongDembias,.025), -quantile(NotStrongDembias,.025), -quantile(LeanDembias,.025), quantile(LeanRepbias,.025), quantile(NotStrongRepbias,.025), quantile(StrongRepbias,.025))
ci.u <- c(-quantile(StrongDembias,.975), -quantile(NotStrongDembias,.975), -quantile(LeanDembias,.975), quantile(LeanRepbias,.975), quantile(NotStrongRepbias,.975), quantile(StrongRepbias,.975))
barplot2(hh, beside = TRUE, horiz=FALSE,
         col = c("darkblue", "darkblue", "darkblue", 
                 "firebrick1", "firebrick1", "firebrick1"), 
         names.arg=c("High", "Medium", "Low","Low", "Medium", "High"),
         xlab = "Partisan Intensity Based on the IDPG",
         ylab = "Motivated Processing: Republican - Democrat",
         cex.lab = 1.3,
         ci.lwd = 3, 
         ci.color = c("steelblue2", "steelblue2", "steelblue2", 
                      "red4", "red4", "red4"), 
         cex.names = 1.5, plot.ci = TRUE, ci.l = ci.l, ci.u = ci.u,
         plot.grid = TRUE) 

