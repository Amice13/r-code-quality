# Libraries
install.packages("texreg")
library(texreg)

##############################
##### Read in the organized data
##############################

load("Study3.RData")

###############
##### Table 2
################

turnout_glm <- glm(turnout ~ pride+hope+ age_dec + educ + income + male+id_strength, data=study3, family="binomial") 
any_mobil_glm <- glm(any_mobil ~ pride+hope+ age_dec + educ + income + male+id_strength, data=study3, family="binomial") 

texreg(list(turnout_glm, any_mobil_glm),stars=c(0.05))

################
##### Predicted probabilities for turnout model
#######################

pred_turnout_both1 <-predict(turnout_glm, expand.grid(pride=1, hope=1,age_dec = mean(study3$age_dec, na.rm=T), educ = 8, income = 5, male=1, id_strength=2), type="response",se.fit=T)
pred_turnout_both0 <-predict(turnout_glm, expand.grid(pride=0,hope=0, age_dec = mean(study3$age_dec, na.rm=T), educ = 8, income = 5, male=1, id_strength=2), type="response",se.fit=T)
pred_turnout_both1
pred_turnout_both0

pred_turnout_hope1 <-predict(turnout_glm, expand.grid(pride=0, hope=1,age_dec = mean(study3$age_dec, na.rm=T), educ = 8, income = 5, male=1, id_strength=2), type="response",se.fit=T)
pred_turnout_hope1

pred_turnout_pride1 <-predict(turnout_glm, expand.grid(pride=1, hope=0,age_dec = mean(study3$age_dec, na.rm=T), educ = 8, income = 5, male=1, id_strength=2), type="response",se.fit=T)
pred_turnout_pride1

################
##### Predicted probabilities for activism model
#######################

pred_activism_both1 <-predict(any_mobil_glm, expand.grid(pride=1, hope=1,age_dec = mean(study3$age_dec, na.rm=T), educ = 8, income = 5, male=1, id_strength=2), type="response",se.fit=T)
pred_activism_both0 <-predict(any_mobil_glm, expand.grid(pride=0,hope=0, age_dec = mean(study3$age_dec, na.rm=T),educ = 8, income = 5, male=1, id_strength=2), type="response",se.fit=T)
pred_activism_both1
pred_activism_both0

pred_activism_hope1 <-predict(any_mobil_glm, expand.grid(pride=0, hope=1,age_dec = mean(study3$age_dec, na.rm=T), educ = 8, income = 5, male=1, id_strength=2), type="response",se.fit=T)
pred_activism_hope1

pred_activism_pride1 <-predict(any_mobil_glm, expand.grid(pride=1, hope=0,age_dec = mean(study3$age_dec, na.rm=T), educ = 8, income = 5, male=1, id_strength=2), type="response",se.fit=T)
pred_activism_pride1

###########################
######## Figure 4
###########################

pdf("plot_bes_turnout_predicted.pdf")
plot(x=c(),y=c(),ylim=c(0.94, 0.99),xlim=c(0,1), ylab="Probability of turnout", main="", xaxt="n", xlab="",cex.lab=1.5)
points(0.2,pred_turnout_both0$fit,pch=16,cex=1.3)
lines(x=c(0.2,0.2),y=c(pred_turnout_both0$fit-1.96*pred_turnout_both0$se.fit,pred_turnout_both0$fit+1.96*pred_turnout_both0$se.fit),lwd=3)
points(0.4,pred_turnout_hope1$fit,pch=16,cex=1.3)
lines(x=c(0.4,0.4),y=c(pred_turnout_hope1$fit-1.96*pred_turnout_hope1$se.fit,pred_turnout_hope1$fit+1.96*pred_turnout_hope1$se.fit),lwd=3)
points(0.6,pred_turnout_pride1$fit,pch=16,cex=1.3)
lines(x=c(0.6,0.6),y=c(pred_turnout_pride1$fit-1.96*pred_turnout_pride1$se.fit,pred_turnout_pride1$fit+1.96*pred_turnout_pride1$se.fit),lwd=3)
points(0.8,pred_turnout_both1$fit,pch=16,cex=1.3)
lines(x=c(0.8,0.8),y=c(pred_turnout_both1$fit-1.96*pred_turnout_both1$se.fit,pred_turnout_both1$fit+1.96*pred_turnout_both1$se.fit),lwd=3)
mtext("Neither proud \n nor hopeful", side=1, at=0.2, line=2, cex=1.2, srt=45)
mtext("Hopeful", side=1, at=0.4, line=1, cex=1.2)
mtext("Proud", side=1, at=0.6, line=1, cex=1.2)
mtext("Both proud \n and hopeful", side=1, at=0.8, line=2, cex=1.2)
dev.off()

pdf("plot_bes_activism_predicted.pdf")
plot(x=c(),y=c(),ylim=c(0, 0.4),xlim=c(0,1), ylab="Probability of activism", main="", xaxt="n", xlab="",cex.lab=1.5)
points(0.2,pred_activism_both0$fit,pch=16,cex=1.3)
lines(x=c(0.2,0.2),y=c(pred_activism_both0$fit-1.96*pred_activism_both0$se.fit,pred_activism_both0$fit+1.96*pred_activism_both0$se.fit),lwd=3)
points(0.4,pred_activism_hope1$fit,pch=16,cex=1.3)
lines(x=c(0.4,0.4),y=c(pred_activism_hope1$fit-1.96*pred_activism_hope1$se.fit,pred_activism_hope1$fit+1.96*pred_activism_hope1$se.fit),lwd=3)
points(0.6,pred_activism_pride1$fit,pch=16,cex=1.3)
lines(x=c(0.6,0.6),y=c(pred_activism_pride1$fit-1.96*pred_activism_pride1$se.fit,pred_activism_pride1$fit+1.96*pred_activism_pride1$se.fit),lwd=3)
points(0.8,pred_activism_both1$fit,pch=16,cex=1.3)
lines(x=c(0.8,0.8),y=c(pred_activism_both1$fit-1.96*pred_activism_both1$se.fit,pred_activism_both1$fit+1.96*pred_activism_both1$se.fit),lwd=3)
mtext("Neither proud \n nor hopeful", side=1, at=0.2, line=2, cex=1.2)
mtext("Hopeful", side=1, at=0.4, line=1, cex=1.2)
mtext("Proud", side=1, at=0.6, line=1, cex=1.2)
mtext("Both proud \n and hopeful", side=1, at=0.8, line=2, cex=1.2)
dev.off()