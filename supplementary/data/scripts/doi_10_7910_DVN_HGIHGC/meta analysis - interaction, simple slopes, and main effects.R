# meta-analytic coefficients for interaction between social class destination and origins and simple slopes

library(metafor)
library(grid)

Study <- c(1,2,3,4)
Label <- c('Sample 1','Sample 2','Sample 3','Sample 4')
SampleSize<- c('N=503','N=998','N=1036','N=568')
meta.data <-data.frame(Study,Label,SampleSize)

################# INCOME AND ENTITLEMENT #################################################################

### meta-analysis of interaction coefficient
Interaction<-c(.07,.04,.02,.13) # coefficients
Error<-c(.06,.05,.02,.04) # standard errors
fixed.meta11 = rma(yi = Interaction, sei = Error, method = "FE")
summary(fixed.meta11)
forest(fixed.meta11, slab = paste(meta.data$Label, meta.data$SampleSize, sep = ", "))
grid.text("Meta-Analysis of Interaction Coefficients", .5, .9, gp=gpar(fontsize=8,cex=2))
grid.text("Across Four Studies", .5, .85, gp=gpar(fontsize=8,cex=2))

### meta-analysis of main effect of destinations (in model with interaction)
MainEffectDests<-c(.120,.036,.077,.059) # coefficients
Error<-c(.045,.033,.037,.042) # standard errors
fixed.meta1 = rma(yi = MainEffectDests, sei = Error, method = "FE")
summary(fixed.meta1)
forest(fixed.meta1, slab = paste(meta.data$Label, meta.data$SampleSize, sep = ", "))
grid.text("Meta-Analysis of Main Effect of Destinations", .5, .9, gp=gpar(fontsize=8,cex=2))
grid.text("Across Four Studies", .5, .85, gp=gpar(fontsize=8,cex=2))

### meta-analysis of main effect of origins (in model with interaction)
MainEffectOrigins<-c(.084,.046,.020,-.015) # coefficients
Error<-c(.048,.033,.033,.045) # standard errors
fixed.meta1 = rma(yi = MainEffectOrigins, sei = Error, method = "FE")
summary(fixed.meta1)
forest(fixed.meta1, slab = paste(meta.data$Label, meta.data$SampleSize, sep = ", "))
grid.text("Meta-Analysis of Main Effect of Origins", .5, .9, gp=gpar(fontsize=8,cex=2))
grid.text("Across Four Studies", .5, .85, gp=gpar(fontsize=8,cex=2))

### meta-analysis of association between class destinations and entitlement among high class origins
Interaction<-c(.19,.08,.10,.18) # coefficients
Error<-c(.08,.04,.03,.06) # standard errors
fixed.meta12 = rma(yi = Interaction, sei = Error, method = "FE")
summary(fixed.meta12)
forest(fixed.meta12, slab = paste(meta.data$Label, meta.data$SampleSize, sep = ", "))
grid.text("Meta-Analysis of Simple Slopes", .5, .9, gp=gpar(fontsize=8,cex=2))
grid.text("Across Four Studies", .5, .85, gp=gpar(fontsize=8,cex=2))

### meta-analysis of association between class destinations and entitlement among low class origins
Interaction<-c(.09,.01,.07,-.004) # coefficients
Error<-c(.05,.04,.04,.05) # standard errors
fixed.meta13 = rma(yi = Interaction, sei = Error, method = "FE")
summary(fixed.meta13)
forest(fixed.meta13, slab = paste(meta.data$Label, meta.data$SampleSize, sep = ", "))
grid.text("Meta-Analysis of Simple Slopes", .5, .9, gp=gpar(fontsize=8,cex=2))
grid.text("Across Four Studies", .5, .85, gp=gpar(fontsize=8,cex=2))

### meta-analysis of association between class origins and entitlement among high class destinations
Interaction<-c(.15,.09,.04,.11) # coefficients
Error<-c(.06,.04,.04,.05) # standard errors
fixed.meta14 = rma(yi = Interaction, sei = Error, method = "FE")
summary(fixed.meta14)
forest(fixed.meta14, slab = paste(meta.data$Label, meta.data$SampleSize, sep = ", "))
grid.text("Meta-Analysis of Simple Slopes", .5, .9, gp=gpar(fontsize=8,cex=2))
grid.text("Across Four Studies", .5, .85, gp=gpar(fontsize=8,cex=2))

### meta-analysis of association between class origins and entitlement among low class destinations
Interaction<-c(.05,.02,.01,-.08) # coefficients
Error<-c(.06,.04,.04,.06) # standard errors
fixed.meta15 = rma(yi = Interaction, sei = Error, method = "FE")
summary(fixed.meta15)
forest(fixed.meta15, slab = paste(meta.data$Label, meta.data$SampleSize, sep = ", "))
grid.text("Meta-Analysis of Simple Slopes", .5, .9, gp=gpar(fontsize=8,cex=2))
grid.text("Across Four Studies", .5, .85, gp=gpar(fontsize=8,cex=2))

################# EDUCATION AND ENTITLEMENT #################################################################

### meta-analysis of interaction coefficient
Interaction<-c(.06,.10,.03,.08) # coefficients
Error<-c(.04,.03,.03,.04) # standard errors
fixed.meta16 = rma(yi = Interaction, sei = Error, method = "FE")
summary(fixed.meta16)
forest(fixed.meta16, slab = paste(meta.data$Label, meta.data$SampleSize, sep = ", "))
grid.text("Meta-Analysis of Interaction Coefficients", .5, .9, gp=gpar(fontsize=8,cex=2))
grid.text("Across Four Studies", .5, .85, gp=gpar(fontsize=8,cex=2))

### meta-analysis of main effect of destinations (in model with interaction)
MainEffectDests<-c(-.021,-.001,-.010,.097) # coefficients
Error<-c(.051,.034,.033,.043) # standard errors
fixed.meta1 = rma(yi = MainEffectDests, sei = Error, method = "FE")
summary(fixed.meta1)
forest(fixed.meta1, slab = paste(meta.data$Label, meta.data$SampleSize, sep = ", "))
grid.text("Meta-Analysis of Main Effect of Destinations", .5, .9, gp=gpar(fontsize=8,cex=2))
grid.text("Across Four Studies", .5, .85, gp=gpar(fontsize=8,cex=2))

### meta-analysis of main effect of origins (in model with interaction)
MainEffectOrigins<-c(.022,.024,.033,.012) # coefficients
Error<-c(.053,.034,.033,.043) # standard errors
fixed.meta1 = rma(yi = MainEffectOrigins, sei = Error, method = "FE")
summary(fixed.meta1)
forest(fixed.meta1, slab = paste(meta.data$Label, meta.data$SampleSize, sep = ", "))
grid.text("Meta-Analysis of Main Effect of Origins", .5, .9, gp=gpar(fontsize=8,cex=2))
grid.text("Across Four Studies", .5, .85, gp=gpar(fontsize=8,cex=2))

### meta-analysis of association between class destinations and entitlement among high class origins
Interaction<-c(.04,.10,.02,.18) # coefficients
Error<-c(.06,.04,.05,.06) # standard errors
fixed.meta17 = rma(yi = Interaction, sei = Error, method = "FE")
summary(fixed.meta17)
forest(fixed.meta17, slab = paste(meta.data$Label, meta.data$SampleSize, sep = ", "))
grid.text("Meta-Analysis of Simple Slopess", .5, .9, gp=gpar(fontsize=8,cex=2))
grid.text("Across Four Studies", .5, .85, gp=gpar(fontsize=8,cex=2))

### meta-analysis of association between class destinations and entitlement among low class origins
Interaction<-c(-.08,-.10,-.04,.02) # coefficients
Error<-c(.07,.05,.05,.06) # standard errors
fixed.meta18 = rma(yi = Interaction, sei = Error, method = "FE")
summary(fixed.meta18)
forest(fixed.meta18, slab = paste(meta.data$Label, meta.data$SampleSize, sep = ", "))
grid.text("Meta-Analysis of Simple Slopes", .5, .9, gp=gpar(fontsize=8,cex=2))
grid.text("Across Four Studies", .5, .85, gp=gpar(fontsize=8,cex=2))

### meta-analysis of association between class origins and entitlement among high class destinations
Interaction<-c(.08,.12,-.01,.09) # coefficients
Error<-c(.06,.04,.04,.06) # standard errors
fixed.meta19 = rma(yi = Interaction, sei = Error, method = "FE")
summary(fixed.meta19)
forest(fixed.meta19, slab = paste(meta.data$Label, meta.data$SampleSize, sep = ", "))
grid.text("Meta-Analysis of Simple Slopes", .5, .9, gp=gpar(fontsize=8,cex=2))
grid.text("Across Four Studies", .5, .85, gp=gpar(fontsize=8,cex=2))

### meta-analysis of association between class origins and entitlement among low class destinations
Interaction<-c(-.04,-.07,.01,-.07) # coefficients
Error<-c(.07,.05,.05,.06) # standard errors
fixed.meta20 = rma(yi = Interaction, sei = Error, method = "FE")
summary(fixed.meta20)
forest(fixed.meta20, slab = paste(meta.data$Label, meta.data$SampleSize, sep = ", "))
grid.text("Meta-Analysis of Simple Slopes", .5, .9, gp=gpar(fontsize=8,cex=2))
grid.text("Across Four Studies", .5, .85, gp=gpar(fontsize=8,cex=2))

################# SUBJECTIVE SOCIAL CLASS AND ENTITLEMENT #################################################################

### meta-analysis of interaction coefficient
Interaction<-c(.10,.08,.08,.08) # coefficients
Error<-c(.04,.03,.03,.04) # standard errors
fixed.meta1 = rma(yi = Interaction, sei = Error, method = "FE")
summary(fixed.meta1)
forest(fixed.meta1, slab = paste(meta.data$Label, meta.data$SampleSize, sep = ", "))
grid.text("Meta-Analysis of Interaction Coefficients", .5, .9, gp=gpar(fontsize=8,cex=2))
grid.text("Across Four Studies", .5, .85, gp=gpar(fontsize=8,cex=2))

### meta-analysis of main effect of destinations (in model with interaction)
MainEffectDests<-c(.004,.024,.084,.157) # coefficients
Error<-c(.046,.034,.033,.044) # standard errors
fixed.meta1 = rma(yi = MainEffectDests, sei = Error, method = "FE")
summary(fixed.meta1)
forest(fixed.meta1, slab = paste(meta.data$Label, meta.data$SampleSize, sep = ", "))
grid.text("Meta-Analysis of Main Effect of Destinations", .5, .9, gp=gpar(fontsize=8,cex=2))
grid.text("Across Four Studies", .5, .85, gp=gpar(fontsize=8,cex=2))

### meta-analysis of main effect of origins (in model with interaction)
MainEffectOrigins<-c(.227,.042,.055,-.002) # coefficients
Error<-c(.046,.034,.033,.044) # standard errors
fixed.meta1 = rma(yi = MainEffectOrigins, sei = Error, method = "FE")
summary(fixed.meta1)
forest(fixed.meta1, slab = paste(meta.data$Label, meta.data$SampleSize, sep = ", "))
grid.text("Meta-Analysis of Main Effect of Origins", .5, .9, gp=gpar(fontsize=8,cex=2))
grid.text("Across Four Studies", .5, .85, gp=gpar(fontsize=8,cex=2))

### meta-analysis of association between class destinations and entitlement among high class origins
Interaction<-c(.10,.11,.17,.23) # coefficients
Error<-c(.06,.04,.04,.06) # standard errors
fixed.meta2 = rma(yi = Interaction, sei = Error, method = "FE")
summary(fixed.meta2)
forest(fixed.meta2, slab = paste(meta.data$Label, meta.data$SampleSize, sep = ", "))
grid.text("Meta-Analysis of Simple Slopes", .5, .9, gp=gpar(fontsize=8,cex=2))
grid.text("Across Four Studies", .5, .85, gp=gpar(fontsize=8,cex=2))

### meta-analysis of association between class destinations and entitlement among low class origins
Interaction<-c(-.09,-.06,.004,.08) # coefficients
Error<-c(.07,.04,.05,.06) # standard errors
fixed.meta3 = rma(yi = Interaction, sei = Error, method = "FE")
summary(fixed.meta3)
forest(fixed.meta3, slab = paste(meta.data$Label, meta.data$SampleSize, sep = ", "))
grid.text("Meta-Analysis of Simple Slopes", .5, .9, gp=gpar(fontsize=8,cex=2))
grid.text("Across Four Studies", .5, .85, gp=gpar(fontsize=8,cex=2))

### meta-analysis of association between class origins and entitlement among high class destinations
Interaction<-c(.33,.13,.14,.07) # coefficients
Error<-c(.06,.04,.04,.05) # standard errors
fixed.meta4 = rma(yi = Interaction, sei = Error, method = "FE")
summary(fixed.meta4)
forest(fixed.meta4, slab = paste(meta.data$Label, meta.data$SampleSize, sep = ", "))
grid.text("Meta-Analysis of Simple Slopes", .5, .9, gp=gpar(fontsize=8,cex=2))
grid.text("Across Four Studies", .5, .85, gp=gpar(fontsize=8,cex=2))

### meta-analysis of association between class origins and entitlement among low class destinations
Interaction<-c(.13,-.04,-.03,-.08) # coefficients
Error<-c(.06,.04,.04,.05) # standard errors
fixed.meta5 = rma(yi = Interaction, sei = Error, method = "FE")
summary(fixed.meta5)
forest(fixed.meta5, slab = paste(meta.data$Label, meta.data$SampleSize, sep = ", "))
grid.text("Meta-Analysis of Simple Slopes", .5, .9, gp=gpar(fontsize=8,cex=2))
grid.text("Across Four Studies", .5, .85, gp=gpar(fontsize=8,cex=2))

################# ANALYSES WITH CONTROLS FOR AGE, GENDER, AND ETHNICITY #################

################# INCOME AND ENTITLEMENT ################################################

### meta-analysis of interaction coefficient
Interaction<-c(.05,.04,.02,.13) # coefficients
Error<-c(.05,.02,.02,.04) # standard errors
fixed.meta11 = rma(yi = Interaction, sei = Error, method = "FE")
summary(fixed.meta11)
forest(fixed.meta11, slab = paste(meta.data$Label, meta.data$SampleSize, sep = ", "))
grid.text("Meta-Analysis of Interaction Coefficients", .5, .9, gp=gpar(fontsize=8,cex=2))
grid.text("Across Four Studies", .5, .85, gp=gpar(fontsize=8,cex=2))

### meta-analysis of main effect of destinations (in model with interaction)
MainEffectDests<-c(.115,.052,.105,.061) # coefficients
Error<-c(.046,.033,.037,.042) # standard errors
fixed.meta1 = rma(yi = MainEffectDests, sei = Error, method = "FE")
summary(fixed.meta1)
forest(fixed.meta1, slab = paste(meta.data$Label, meta.data$SampleSize, sep = ", "))
grid.text("Meta-Analysis of Main Effect of Destinations", .5, .9, gp=gpar(fontsize=8,cex=2))
grid.text("Across Four Studies", .5, .85, gp=gpar(fontsize=8,cex=2))

### meta-analysis of main effect of origins (in model with interaction)
MainEffectOrigins<-c(.115,.036,-.006,-.019) # coefficients
Error<-c(.049,.033,.034,.045) # standard errors
fixed.meta1 = rma(yi = MainEffectOrigins, sei = Error, method = "FE")
summary(fixed.meta1)
forest(fixed.meta1, slab = paste(meta.data$Label, meta.data$SampleSize, sep = ", "))
grid.text("Meta-Analysis of Main Effect of Origins", .5, .9, gp=gpar(fontsize=8,cex=2))
grid.text("Across Four Studies", .5, .85, gp=gpar(fontsize=8,cex=2))

### meta-analysis of main effect of gender (in model with interaction)
MainEffectOrigins<-c(.027,.073,.038,-.232) # coefficients
Error<-c(.091,.063,.062,.083) # standard errors
fixed.meta1 = rma(yi = MainEffectOrigins, sei = Error, method = "FE")
summary(fixed.meta1)
forest(fixed.meta1, slab = paste(meta.data$Label, meta.data$SampleSize, sep = ", "))
grid.text("Meta-Analysis of Main Effect of Gender", .5, .9, gp=gpar(fontsize=8,cex=2))
grid.text("Across Four Studies", .5, .85, gp=gpar(fontsize=8,cex=2))

### meta-analysis of main effect of ethnicity (in model with interaction)
MainEffectOrigins<-c(-.536,-.513,-.501,-.409) # coefficients
Error<-c(.102,.083,.078,.103) # standard errors
fixed.meta1 = rma(yi = MainEffectOrigins, sei = Error, method = "FE")
summary(fixed.meta1)
forest(fixed.meta1, slab = paste(meta.data$Label, meta.data$SampleSize, sep = ", "))
grid.text("Meta-Analysis of Main Effect of Ethnicity", .5, .9, gp=gpar(fontsize=8,cex=2))
grid.text("Across Four Studies", .5, .85, gp=gpar(fontsize=8,cex=2))

### meta-analysis of main effect of age (in model with interaction)
MainEffectOrigins<-c(.004,-.007,-.011,-.004) # coefficients
Error<-c(.004,.002,.003,.004) # standard errors
fixed.meta1 = rma(yi = MainEffectOrigins, sei = Error, method = "FE")
summary(fixed.meta1)
forest(fixed.meta1, slab = paste(meta.data$Label, meta.data$SampleSize, sep = ", "))
grid.text("Meta-Analysis of Main Effect of Age", .5, .9, gp=gpar(fontsize=8,cex=2))
grid.text("Across Four Studies", .5, .85, gp=gpar(fontsize=8,cex=2))

### meta-analysis of association between class destinations and entitlement among high class origins
Interaction<-c(.160,.087,.123,.191) # coefficients
Error<-c(.074,.035,.033,.056) # standard errors
fixed.meta12 = rma(yi = Interaction, sei = Error, method = "FE")
summary(fixed.meta12)
forest(fixed.meta12, slab = paste(meta.data$Label, meta.data$SampleSize, sep = ", "))
grid.text("Meta-Analysis of Simple Slopes", .5, .9, gp=gpar(fontsize=8,cex=2))
grid.text("Across Four Studies", .5, .85, gp=gpar(fontsize=8,cex=2))

### meta-analysis of association between class destinations and entitlement among low class origins
Interaction<-c(.092,.034,.096,-.004) # coefficients
Error<-c(.051,.036,.042,.049) # standard errors
fixed.meta13 = rma(yi = Interaction, sei = Error, method = "FE")
summary(fixed.meta13)
forest(fixed.meta13, slab = paste(meta.data$Label, meta.data$SampleSize, sep = ", "))
grid.text("Meta-Analysis of Simple Slopes", .5, .9, gp=gpar(fontsize=8,cex=2))
grid.text("Across Four Studies", .5, .85, gp=gpar(fontsize=8,cex=2))

### meta-analysis of association between class origins and entitlement among high class destinations
Interaction<-c(.160,.071,.012,.111) # coefficients
Error<-c(.057,.036,.036,.050) # standard errors
fixed.meta14 = rma(yi = Interaction, sei = Error, method = "FE")
summary(fixed.meta14)
forest(fixed.meta14, slab = paste(meta.data$Label, meta.data$SampleSize, sep = ", "))
grid.text("Meta-Analysis of Simple Slopes", .5, .9, gp=gpar(fontsize=8,cex=2))
grid.text("Across Four Studies", .5, .85, gp=gpar(fontsize=8,cex=2))

### meta-analysis of association between class origins and entitlement among low class destinations
Interaction<-c(.092,.018,-.015,-.084) # coefficients
Error<-c(.064,.036,.036,.056) # standard errors
fixed.meta15 = rma(yi = Interaction, sei = Error, method = "FE")
summary(fixed.meta15)
forest(fixed.meta15, slab = paste(meta.data$Label, meta.data$SampleSize, sep = ", "))
grid.text("Meta-Analysis of Simple Slopes", .5, .9, gp=gpar(fontsize=8,cex=2))
grid.text("Across Four Studies", .5, .85, gp=gpar(fontsize=8,cex=2))

################# EDUCATION AND ENTITLEMENT #############################################

### meta-analysis of interaction coefficient
Interaction<-c(.06,.09,.03,.092) # coefficients
Error<-c(.04,.03,.03,.041) # standard errors
fixed.meta16 = rma(yi = Interaction, sei = Error, method = "FE")
summary(fixed.meta16)
forest(fixed.meta16, slab = paste(meta.data$Label, meta.data$SampleSize, sep = ", "))
grid.text("Meta-Analysis of Interaction Coefficients", .5, .9, gp=gpar(fontsize=8,cex=2))
grid.text("Across Four Studies", .5, .85, gp=gpar(fontsize=8,cex=2))

### meta-analysis of main effect of destinations (in model with interaction)
MainEffectDests<-c(-.022,.009,.012,.076) # coefficients
Error<-c(.050,.033,.033,.043) # standard errors
fixed.meta1 = rma(yi = MainEffectDests, sei = Error, method = "FE")
summary(fixed.meta1)
forest(fixed.meta1, slab = paste(meta.data$Label, meta.data$SampleSize, sep = ", "))
grid.text("Meta-Analysis of Main Effect of Destinations", .5, .9, gp=gpar(fontsize=8,cex=2))
grid.text("Across Four Studies", .5, .85, gp=gpar(fontsize=8,cex=2))

### meta-analysis of main effect of origins (in model with interaction)
MainEffectOrigins<-c(.037,.004,.006,.011) # coefficients
Error<-c(.053,.036,.034,.043) # standard errors
fixed.meta1 = rma(yi = MainEffectOrigins, sei = Error, method = "FE")
summary(fixed.meta1)
forest(fixed.meta1, slab = paste(meta.data$Label, meta.data$SampleSize, sep = ", "))
grid.text("Meta-Analysis of Main Effect of Origins", .5, .9, gp=gpar(fontsize=8,cex=2))
grid.text("Across Four Studies", .5, .85, gp=gpar(fontsize=8,cex=2))

### meta-analysis of main effect of gender (in model with interaction)
MainEffectOrigins<-c(-.037,.059,.007,-.243) # coefficients
Error<-c(.090,.063,.063,.084) # standard errors
fixed.meta1 = rma(yi = MainEffectOrigins, sei = Error, method = "FE")
summary(fixed.meta1)
forest(fixed.meta1, slab = paste(meta.data$Label, meta.data$SampleSize, sep = ", "))
grid.text("Meta-Analysis of Main Effect of Gender", .5, .9, gp=gpar(fontsize=8,cex=2))
grid.text("Across Four Studies", .5, .85, gp=gpar(fontsize=8,cex=2))

### meta-analysis of main effect of ethnicity (in model with interaction)
MainEffectOrigins<-c(-.527,-.524,-.499,-.407) # coefficients
Error<-c(.104,.084,.078,.106) # standard errors
fixed.meta1 = rma(yi = MainEffectOrigins, sei = Error, method = "FE")
summary(fixed.meta1)
forest(fixed.meta1, slab = paste(meta.data$Label, meta.data$SampleSize, sep = ", "))
grid.text("Meta-Analysis of Main Effect of Ethnicity", .5, .9, gp=gpar(fontsize=8,cex=2))
grid.text("Across Four Studies", .5, .85, gp=gpar(fontsize=8,cex=2))

### meta-analysis of main effect of age (in model with interaction)
MainEffectOrigins<-c(.003,-.006,-.010,-.003) # coefficients
Error<-c(.004,.002,.003,.004) # standard errors
fixed.meta1 = rma(yi = MainEffectOrigins, sei = Error, method = "FE")
summary(fixed.meta1)
forest(fixed.meta1, slab = paste(meta.data$Label, meta.data$SampleSize, sep = ", "))
grid.text("Meta-Analysis of Main Effect of Age", .5, .9, gp=gpar(fontsize=8,cex=2))
grid.text("Across Four Studies", .5, .85, gp=gpar(fontsize=8,cex=2))

### meta-analysis of association between class destinations and entitlement among high class origins
Interaction<-c(.039,.103,.045,.168) # coefficients
Error<-c(.061,.041,.045,.058) # standard errors
fixed.meta17 = rma(yi = Interaction, sei = Error, method = "FE")
summary(fixed.meta17)
forest(fixed.meta17, slab = paste(meta.data$Label, meta.data$SampleSize, sep = ", "))
grid.text("Meta-Analysis of Simple Slopes", .5, .9, gp=gpar(fontsize=8,cex=2))
grid.text("Across Four Studies", .5, .85, gp=gpar(fontsize=8,cex=2))

### meta-analysis of association between class destinations and entitlement among low class origins
Interaction<-c(-.083,-.086,-.021,-.015) # coefficients
Error<-c(.065,.046,.045,.061) # standard errors
fixed.meta18 = rma(yi = Interaction, sei = Error, method = "FE")
summary(fixed.meta18)
forest(fixed.meta18, slab = paste(meta.data$Label, meta.data$SampleSize, sep = ", "))
grid.text("Meta-Analysis of Simple Slopes", .5, .9, gp=gpar(fontsize=8,cex=2))
grid.text("Across Four Studies", .5, .85, gp=gpar(fontsize=8,cex=2))

### meta-analysis of association between class origins and entitlement among high class destinations
Interaction<-c(.098,.098,.038,.102) # coefficients
Error<-c(.056,.042,.043,.058) # standard errors
fixed.meta19 = rma(yi = Interaction, sei = Error, method = "FE")
summary(fixed.meta19)
forest(fixed.meta19, slab = paste(meta.data$Label, meta.data$SampleSize, sep = ", "))
grid.text("Meta-Analysis of Simple Slopes", .5, .9, gp=gpar(fontsize=8,cex=2))
grid.text("Across Four Studies", .5, .85, gp=gpar(fontsize=8,cex=2))

### meta-analysis of association between class origins and entitlement among low class destinations
Interaction<-c(-.024,-.09,-.027,-.081) # coefficients
Error<-c(.073,.049,.047,.061) # standard errors
fixed.meta20 = rma(yi = Interaction, sei = Error, method = "FE")
summary(fixed.meta20)
forest(fixed.meta20, slab = paste(meta.data$Label, meta.data$SampleSize, sep = ", "))
grid.text("Meta-Analysis of Simple Slopes", .5, .9, gp=gpar(fontsize=8,cex=2))
grid.text("Across Four Studies", .5, .85, gp=gpar(fontsize=8,cex=2))

################# SUBJECTIVE SOCIAL CLASS AND ENTITLEMENT ###############################

### meta-analysis of interaction coefficient
Interaction<-c(.08,.06,.07,.074) # coefficients
Error<-c(.04,.03,.03,.036) # standard errors
fixed.meta1 = rma(yi = Interaction, sei = Error, method = "FE")
summary(fixed.meta1)
forest(fixed.meta1, slab = paste(meta.data$Label, meta.data$SampleSize, sep = ", "))
grid.text("Meta-Analysis of Interaction Coefficients", .5, .9, gp=gpar(fontsize=8,cex=2))
grid.text("Across Four Studies", .5, .85, gp=gpar(fontsize=8,cex=2))

### meta-analysis of main effect of destinations (in model with interaction)
MainEffectDests<-c(-.004,.030,.081,.150) # coefficients
Error<-c(.045,.033,.032,.043) # standard errors
fixed.meta1 = rma(yi = MainEffectDests, sei = Error, method = "FE")
summary(fixed.meta1)
forest(fixed.meta1, slab = paste(meta.data$Label, meta.data$SampleSize, sep = ", "))
grid.text("Meta-Analysis of Main Effect of Destinations", .5, .9, gp=gpar(fontsize=8,cex=2))
grid.text("Across Four Studies", .5, .85, gp=gpar(fontsize=8,cex=2))

### meta-analysis of main effect of origins (in model with interaction)
MainEffectOrigins<-c(.257,.054,.063,.003) # coefficients
Error<-c(.045,.033,.032,.043) # standard errors
fixed.meta1 = rma(yi = MainEffectOrigins, sei = Error, method = "FE")
summary(fixed.meta1)
forest(fixed.meta1, slab = paste(meta.data$Label, meta.data$SampleSize, sep = ", "))
grid.text("Meta-Analysis of Main Effect of Origins", .5, .9, gp=gpar(fontsize=8,cex=2))
grid.text("Across Four Studies", .5, .85, gp=gpar(fontsize=8,cex=2))

### meta-analysis of main effect of gender (in model with interaction)
MainEffectOrigins<-c(-.002,.046,.011,-.224) # coefficients
Error<-c(.087,.062,.060,.082) # standard errors
fixed.meta1 = rma(yi = MainEffectOrigins, sei = Error, method = "FE")
summary(fixed.meta1)
forest(fixed.meta1, slab = paste(meta.data$Label, meta.data$SampleSize, sep = ", "))
grid.text("Meta-Analysis of Main Effect of Gender", .5, .9, gp=gpar(fontsize=8,cex=2))
grid.text("Across Four Studies", .5, .85, gp=gpar(fontsize=8,cex=2))

### meta-analysis of main effect of ethnicity (in model with interaction)
MainEffectOrigins<-c(-.543,-.534,-.514,-.405) # coefficients
Error<-c(.099,.082,.076,.102) # standard errors
fixed.meta1 = rma(yi = MainEffectOrigins, sei = Error, method = "FE")
summary(fixed.meta1)
forest(fixed.meta1, slab = paste(meta.data$Label, meta.data$SampleSize, sep = ", "))
grid.text("Meta-Analysis of Main Effect of Ethnicity", .5, .9, gp=gpar(fontsize=8,cex=2))
grid.text("Across Four Studies", .5, .85, gp=gpar(fontsize=8,cex=2))

### meta-analysis of main effect of age (in model with interaction)
MainEffectOrigins<-c(.005,-.006,-.008,-.001) # coefficients
Error<-c(.004,.002,.003,.004) # standard errors
fixed.meta1 = rma(yi = MainEffectOrigins, sei = Error, method = "FE")
summary(fixed.meta1)
forest(fixed.meta1, slab = paste(meta.data$Label, meta.data$SampleSize, sep = ", "))
grid.text("Meta-Analysis of Main Effect of Age", .5, .9, gp=gpar(fontsize=8,cex=2))
grid.text("Across Four Studies", .5, .85, gp=gpar(fontsize=8,cex=2))

### meta-analysis of association between class destinations and entitlement among high class origins
Interaction<-c(.076,.094,.155,.224) # coefficients
Error<-c(.057,.042,.040,.055) # standard errors
fixed.meta2 = rma(yi = Interaction, sei = Error, method = "FE")
summary(fixed.meta2)
forest(fixed.meta2, slab = paste(meta.data$Label, meta.data$SampleSize, sep = ", "))
grid.text("Meta-Analysis of Simple Slopes", .5, .9, gp=gpar(fontsize=8,cex=2))
grid.text("Across Four Studies", .5, .85, gp=gpar(fontsize=8,cex=2))

### meta-analysis of association between class destinations and entitlement among low class origins
Interaction<-c(-.084,-.034,.007,.076) # coefficients
Error<-c(.064,.043,.044,.057) # standard errors
fixed.meta3 = rma(yi = Interaction, sei = Error, method = "FE")
summary(fixed.meta3)
forest(fixed.meta3, slab = paste(meta.data$Label, meta.data$SampleSize, sep = ", "))
grid.text("Meta-Analysis of Simple Slopes", .5, .9, gp=gpar(fontsize=8,cex=2))
grid.text("Across Four Studies", .5, .85, gp=gpar(fontsize=8,cex=2))

### meta-analysis of association between class origins and entitlement among high class destinations
Interaction<-c(.337,.118,.137,.077) # coefficients
Error<-c(.059,.045,.044,.060) # standard errors
fixed.meta4 = rma(yi = Interaction, sei = Error, method = "FE")
summary(fixed.meta4)
forest(fixed.meta4, slab = paste(meta.data$Label, meta.data$SampleSize, sep = ", "))
grid.text("Meta-Analysis of Simple Slopes", .5, .9, gp=gpar(fontsize=8,cex=2))
grid.text("Across Four Studies", .5, .85, gp=gpar(fontsize=8,cex=2))

### meta-analysis of association between class origins and entitlement among low class destinations
Interaction<-c(.177,-.010,-.011,-.071) # coefficients
Error<-c(.061,.041,.041,.052) # standard errors
fixed.meta5 = rma(yi = Interaction, sei = Error, method = "FE")
summary(fixed.meta5)
forest(fixed.meta5, slab = paste(meta.data$Label, meta.data$SampleSize, sep = ", "))
grid.text("Meta-Analysis of Simple Slopes", .5, .9, gp=gpar(fontsize=8,cex=2))
grid.text("Across Four Studies", .5, .85, gp=gpar(fontsize=8,cex=2))
