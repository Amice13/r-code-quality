# Libraries
install.packages(c("ordinal","texreg","plyr"))
library(ordinal)
library(texreg)
library(plyr)

##########################
###### Read in the data and create object combining labour and conservative data
##########################

load("Study2.RData")

combined <- rbind(lab[,c("proud","hopeful","excited","positive","treat_m","angry","disgusted")], cons[,c("proud","hopeful","excited","positive","treat_m","angry","disgusted")])

##########################
###### Table SI6.1
##########################

# Turn DVs into factors
combined$proud <- as.factor(combined$proud)
combined$hopeful <- as.factor(combined$hopeful)
combined$excited <- as.factor(combined$excited)

# Run ordered logit models
proud_clm <- clm(proud~treat_m, data=combined, link="logit")
hopeful_clm <- clm(hopeful~treat_m, data=combined, link="logit")
excited_clm <- clm(excited~treat_m, data=combined, link="logit")

summary(proud_clm)
summary(hopeful_clm)
summary(excited_clm)

##########################
###### Figure SI6.1
##########################

# Run linear models for negative emotions
angry_mod <- lm(angry~treat_m, data=combined)
disgusted_mod <- lm(disgusted~treat_m, data=combined)

m_me_angry <- c(coef(summary(angry_mod))[2,1]-1.96*coef(summary(angry_mod))[2,2], coef(summary(angry_mod))[2,1], coef(summary(angry_mod))[2,1]+1.96*coef(summary(angry_mod))[2,2])
m_me_disgusted <- c(coef(summary(disgusted_mod))[2,1]-1.96*coef(summary(disgusted_mod))[2,2], coef(summary(disgusted_mod))[2,1], coef(summary(disgusted_mod))[2,1]+1.96*coef(summary(disgusted_mod))[2,2])

pdf("plot_exp_effect_combined_neg_emotions.pdf")
par(mar=c(5.1, 5.1, 4.1, 2.1))
plot(x=c(), y=c(), ylim=c(0, 1.5), xlim=c(-0.3,0.1), xlab="Effect of moral rhetoric", ylab="", main="", yaxt="n",cex.lab=1.3)
points(m_me_angry[2],1,pch=16,cex=1.3)
lines(x=c(m_me_angry[1],m_me_angry[3]), c(1,1),lwd=3)
points(m_me_disgusted[2],0.5,pch=16,cex=1.3)
lines(x=c(m_me_disgusted[1],m_me_disgusted[3]), c(0.5,0.5),lwd=3)
abline(v=0, lty="dashed")
axis(side=2, at=c(0.5, 1), labels=c("Disgust", "Anger"), las=1, cex.axis=1.3)
dev.off()

########################
###### Labour: manipulation check
########################

lab_manip <- read.csv("Copartisans_labour_manipulation.csv", stringsAsFactors=F)

lab_manip$moral <- as.numeric(mapvalues(lab_manip$moral, c("Very much pragmatic","Much pragmatic","Neither pragmatic nor moral","Much moral","Very much moral"), 1:5))
lab_manip$non.moral <- as.numeric(mapvalues(lab_manip$non.moral, c("Very much pragmatic","Much pragmatic","Neither pragmatic nor moral","Much moral","Very much moral"), 1:5))

t.test(lab_manip$moral, lab_manip$non.moral)

########################
###### Labour: believability check
########################

lab_bel <- read.csv("Copartisans_labour_believability.csv", stringsAsFactors=F)

lab_bel$moral <- as.numeric(mapvalues(lab_bel$moral, c("Very believable","Somewhat believable","Somewhat unbelievabble"), c(4,3,2)))
lab_bel$non.moral <- as.numeric(mapvalues(lab_bel$non.moral, c("Very believable","Somewhat believable","Somewhat unbelievable"), c(4,3,2)))

t.test(lab_bel$moral, lab_bel$non.moral)

##########################
###### Table SI6.2
##########################

range(lab$age)
range(lab$male)
range(lab$educ)
range(lab$white)

t.test(age ~ treat_m, lab)
t.test(male ~ treat_m, lab)
t.test(educ ~ treat_m, lab)
t.test(white ~ treat_m, lab) 

##########################
###### Table SI6.3
##########################

randomization_check_mod <- glm(treat_m ~ age + male + educ + white, data=lab, family="binomial")
texreg(randomization_check_mod) 

##########################
###### Table SI6.4
##########################

# Turn DVs into factors
lab$proud <- as.factor(lab$proud)
lab$hopeful <- as.factor(lab$hopeful)
lab$excited <- as.factor(lab$excited)

# Run ordered logit models
proud_clm <- clm(proud~treat_m, data=lab, link="logit")
hopeful_clm <- clm(hopeful~treat_m, data=lab, link="logit")
excited_clm <- clm(excited~treat_m, data=lab, link="logit")

summary(proud_clm)
summary(hopeful_clm)
summary(excited_clm)

##########################
###### Table SI6.5
##########################

range(cons$age,na.rm=T)
range(cons$male,na.rm=T)
range(cons$educ,na.rm=T)
range(cons$white,na.rm=T)
range(cons$income,na.rm=T)

t.test(age ~ treat_m, cons)
t.test(male ~ treat_m, cons)
t.test(educ ~ treat_m, cons)
t.test(white ~ treat_m, cons)
t.test(income ~ treat_m, cons)

##########################
###### Table SI6.6
##########################

randomization_check_mod <- glm(treat_m ~ age + male + educ + white + income, data=cons, family="binomial")
texreg(randomization_check_mod) 

#####################
##### Table SI6.7
#######################

# Turn DVs into factors
cons$proud <- as.factor(cons$proud)
cons$hopeful <- as.factor(cons$hopeful)
cons$excited <- as.factor(cons$excited)

# Ordered logit models
proud_clm <- clm(proud~treat_m, data=cons, link="logit")
hopeful_clm <- clm(hopeful~treat_m, data=cons, link="logit")
excited_clm <- clm(excited~treat_m, data=cons, link="logit")

summary(proud_clm)
summary(hopeful_clm)
summary(excited_clm)

########################
###### Read in and organize labour and conservative data from out-partisan experiments
########################

cons_on_lab <- read.csv("Outpartisans_labour.csv", stringsAsFactors=F)
lab_on_cons <- read.csv("Outpartisans_conservative.csv", stringsAsFactors=F)
combined <- rbind(cons_on_lab,lab_on_cons)

# Organize emotion variables:
combined$excited <- as.numeric(mapvalues(combined$excited, c("Not at all","Slightly","Moderately","Much","Very much"), 1:5))
combined$angry <- as.numeric(mapvalues(combined$angry, c("Not at all","Slightly","Moderately","Much","Very much"), 1:5))
combined$hopeful <- as.numeric(mapvalues(combined$hopeful, c("Not at all","Slightly","Moderately","Much","Very much"), 1:5))
combined$proud <- as.numeric(mapvalues(combined$proud, c("Not at all","Slightly","Moderately","Much","Very much"), 1:5))
combined$disgusted <- as.numeric(mapvalues(combined$disgusted, c("Not at all","Slightly","Moderately","Much","Very much"), 1:5))

# Organize treatment variable
combined$treat_m <- ifelse(combined$treat_m=="Moral",1,0)

# Create measure of average positive emotion
combined$positive <- apply(combined[,c("proud","hopeful","excited")],1,mean,na.rm=T)

# Organize gender
combined$male <- ifelse(combined$sex=="Male",1,0)

# Organize education
combined$educ <- as.numeric(mapvalues(combined$educ, c("College/A levels", "Graduate degree (MA/MSc/MPhil/other)", "Secondary school/GCSE", "Undergraduate degree (BA/BSc/other)", "No formal qualifications", "Doctorate degree (PhD/MD/other)"), c(3,5,2,4,1,5)))

# Organize ethnicity
combined$white <- ifelse(combined$ethnicity=="White",1,0)

# Organize income
combined$income <- as.numeric(mapvalues(combined$income, c("£40,000 - £49,999",   "Less than £10,000 " , "£30,000 - £39,999" ,  "£50,000 - £59,999",   "£20,000 - £29,999"   ,"£16,000 - £19,999"  , "£10,000 - £15,999"  , "£80,000 - £89,999"  , "£60,000 - £69,999"  ,"£70,000 - £79,999" ,  "£100,000 - £149,999" ,"£90,000 - £99,999" ,"More than £150,000" ), c(6,1,5,7,4,3,2,10,8,9,12,11,13)))

##########################
###### Table SI6.8
##########################

range(combined$age,na.rm=T)
range(combined$male,na.rm=T)
range(combined$educ,na.rm=T)
range(combined$white,na.rm=T)
range(combined$income,na.rm=T)

t.test(age ~ treat_m, combined) 
t.test(male ~ treat_m, combined)
t.test(educ ~ treat_m, combined)
t.test(white ~ treat_m, combined)
t.test(income ~ treat_m, combined)

##########################
###### Table SI6.9
##########################

randomization_check_mod <- glm(treat_m ~ age + male + educ + white + income, data=combined, family="binomial")
texreg(randomization_check_mod, stars=0.05)

##########################
###### Figure SI6.2
##########################

# Run linear out-partisan models
proud_mod <- lm(proud~treat_m, combined)
hopeful_mod <- lm(hopeful~treat_m, combined)
excited_mod <- lm(excited~treat_m, combined)
pos_mod <- lm(positive~treat_m, combined)
angry_mod <- lm(angry~treat_m, combined)
disgusted_mod <- lm(disgusted~treat_m, combined)

pdf("plot_exp_effect_combined_out_partisans_positive.pdf")
m_me_pos <- c(coef(summary(pos_mod))[2,1]-1.96*coef(summary(pos_mod))[2,2], coef(summary(pos_mod))[2,1], coef(summary(pos_mod))[2,1]+1.96*coef(summary(pos_mod))[2,2])
m_me_proud <- c(coef(summary(proud_mod))[2,1]-1.96*coef(summary(proud_mod))[2,2], coef(summary(proud_mod))[2,1], coef(summary(proud_mod))[2,1]+1.96*coef(summary(proud_mod))[2,2])
m_me_hopeful <- c(coef(summary(hopeful_mod))[2,1]-1.96*coef(summary(hopeful_mod))[2,2], coef(summary(hopeful_mod))[2,1], coef(summary(hopeful_mod))[2,1]+1.96*coef(summary(hopeful_mod))[2,2])
m_me_excited <- c(coef(summary(excited_mod))[2,1]-1.96*coef(summary(excited_mod))[2,2], coef(summary(excited_mod))[2,1], coef(summary(excited_mod))[2,1]+1.96*coef(summary(excited_mod))[2,2])
par(mar=c(5.1, 7.1, 4.1, 2.1))
plot(x=c(), y=c(), ylim=c(-0.2, 1.7), xlim=c(-0.5,0.5), xlab="Effect of moral rhetoric", ylab="", main="", yaxt="n",cex.lab=1.3)
points(m_me_proud[2],1.5,pch=16,cex=1.3)
lines(x=c(m_me_proud[1],m_me_proud[3]), c(1.5,1.5),lwd=2)
points(m_me_hopeful[2],1,pch=16,cex=1.3)
lines(x=c(m_me_hopeful[1],m_me_hopeful[3]), c(1,1),lwd=2)
points(m_me_excited[2],0.5,pch=16,cex=1.3)
lines(x=c(m_me_excited[1],m_me_excited[3]), c(0.5,0.5),lwd=2)
points(m_me_pos[2],0,pch=16,cex=1.3)
lines(x=c(m_me_pos[1],m_me_pos[3]), c(0,0),lwd=2)
abline(v=0, lty="dashed")
axis(side=2, at=c(0, 0.5, 1, 1.5), labels=c("Enthusiasm", "Excitement", "Hopefulness", "Pride"), las=1, cex.axis=1.3)
dev.off()

pdf("plot_exp_effect_combined_out_partisans_negative.pdf")
m_me_angry <- c(coef(summary(angry_mod))[2,1]-1.96*coef(summary(angry_mod))[2,2], coef(summary(angry_mod))[2,1], coef(summary(angry_mod))[2,1]+1.96*coef(summary(angry_mod))[2,2])
m_me_disgusted <- c(coef(summary(disgusted_mod))[2,1]-1.96*coef(summary(disgusted_mod))[2,2], coef(summary(disgusted_mod))[2,1], coef(summary(disgusted_mod))[2,1]+1.96*coef(summary(disgusted_mod))[2,2])
par(mar=c(5.1, 7.1, 4.1, 2.1))
plot(x=c(), y=c(), ylim=c(0, 1.5), xlim=c(-0.5,0.5), xlab="Effect of moral rhetoric", ylab="", main="", yaxt="n",cex.lab=1.3)
points(m_me_angry[2],1,pch=16,cex=1.3)
lines(x=c(m_me_angry[1],m_me_angry[3]), c(1,1),lwd=2)
points(m_me_disgusted[2],0.5,pch=16,cex=1.3)
lines(x=c(m_me_disgusted[1],m_me_disgusted[3]), c(0.5,0.5),lwd=2)
abline(v=0, lty="dashed")
axis(side=2, at=c(0.5, 1), labels=c("Disgust", "Anger"), las=1, cex.axis=1.3)
dev.off()