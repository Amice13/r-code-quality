# Libraries
install.packages(c("zeligverse","ordinal"))
library(zeligverse)
library(ordinal)

##########################
###### Read in the data and create object combining labour and conservative data
##########################

load("Study2.RData")

# Combine the data
combined <- rbind(lab[,c("proud","hopeful","excited","positive","treat_m","angry","disgusted")], cons[,c("proud","hopeful","excited","positive","treat_m","angry","disgusted")])

##########################
###### Figure 3
##########################

# combined:
proud_mod <- lm(proud~treat_m, combined)
hopeful_mod <- lm(hopeful~treat_m, combined)
excited_mod <- lm(excited~treat_m, combined)
pos_mod <- lm(positive~treat_m, combined)

m_me_pos <- c(coef(summary(pos_mod))[2,1]-1.96*coef(summary(pos_mod))[2,2], coef(summary(pos_mod))[2,1], coef(summary(pos_mod))[2,1]+1.96*coef(summary(pos_mod))[2,2])
m_me_proud <- c(coef(summary(proud_mod))[2,1]-1.96*coef(summary(proud_mod))[2,2], coef(summary(proud_mod))[2,1], coef(summary(proud_mod))[2,1]+1.96*coef(summary(proud_mod))[2,2])
m_me_hopeful <- c(coef(summary(hopeful_mod))[2,1]-1.96*coef(summary(hopeful_mod))[2,2], coef(summary(hopeful_mod))[2,1], coef(summary(hopeful_mod))[2,1]+1.96*coef(summary(hopeful_mod))[2,2])
m_me_excited <- c(coef(summary(excited_mod))[2,1]-1.96*coef(summary(excited_mod))[2,2], coef(summary(excited_mod))[2,1], coef(summary(excited_mod))[2,1]+1.96*coef(summary(excited_mod))[2,2])

# labour:
proud_mod_lab <- lm(proud~treat_m, lab)
hopeful_mod_lab <- lm(hopeful~treat_m, lab)
excited_mod_lab <- lm(excited~treat_m, lab)
pos_mod_lab <- lm(positive~treat_m, lab)

m_me_pos_lab <- c(coef(summary(pos_mod_lab))[2,1]-1.96*coef(summary(pos_mod_lab))[2,2], coef(summary(pos_mod_lab))[2,1], coef(summary(pos_mod_lab))[2,1]+1.96*coef(summary(pos_mod_lab))[2,2])
m_me_proud_lab <- c(coef(summary(proud_mod_lab))[2,1]-1.96*coef(summary(proud_mod_lab))[2,2], coef(summary(proud_mod_lab))[2,1], coef(summary(proud_mod_lab))[2,1]+1.96*coef(summary(proud_mod_lab))[2,2])
m_me_hopeful_lab <- c(coef(summary(hopeful_mod_lab))[2,1]-1.96*coef(summary(hopeful_mod_lab))[2,2], coef(summary(hopeful_mod_lab))[2,1], coef(summary(hopeful_mod_lab))[2,1]+1.96*coef(summary(hopeful_mod_lab))[2,2])
m_me_excited_lab <- c(coef(summary(excited_mod_lab))[2,1]-1.96*coef(summary(excited_mod_lab))[2,2], coef(summary(excited_mod_lab))[2,1], coef(summary(excited_mod_lab))[2,1]+1.96*coef(summary(excited_mod_lab))[2,2])

# conservative:
proud_mod_cons <- lm(proud~treat_m, cons)
hopeful_mod_cons <- lm(hopeful~treat_m, cons)
excited_mod_cons <- lm(excited~treat_m, cons)
pos_mod_cons <- lm(positive~treat_m, cons)

m_me_pos_cons <- c(coef(summary(pos_mod_cons))[2,1]-1.96*coef(summary(pos_mod_cons))[2,2], coef(summary(pos_mod_cons))[2,1], coef(summary(pos_mod_cons))[2,1]+1.96*coef(summary(pos_mod_cons))[2,2])
m_me_proud_cons <- c(coef(summary(proud_mod_cons))[2,1]-1.96*coef(summary(proud_mod_cons))[2,2], coef(summary(proud_mod_cons))[2,1], coef(summary(proud_mod_cons))[2,1]+1.96*coef(summary(proud_mod_cons))[2,2])
m_me_hopeful_cons <- c(coef(summary(hopeful_mod_cons))[2,1]-1.96*coef(summary(hopeful_mod_cons))[2,2], coef(summary(hopeful_mod_cons))[2,1], coef(summary(hopeful_mod_cons))[2,1]+1.96*coef(summary(hopeful_mod_cons))[2,2])
m_me_excited_cons <- c(coef(summary(excited_mod_cons))[2,1]-1.96*coef(summary(excited_mod_cons))[2,2], coef(summary(excited_mod_cons))[2,1], coef(summary(excited_mod_cons))[2,1]+1.96*coef(summary(excited_mod_cons))[2,2])

pdf("plot_exp_effect_combined_&_separate.pdf",width=10,height=5)
par(oma = c(1,7,0,0) + 0.1)
par(mfrow=c(1,3))
par(mar=c(5.1, 1.1, 4.1, 2.1))
plot(x=c(), y=c(), ylim=c(-0.2, 1.7), xlim=c(-0.5,1.0), xlab="", ylab="", main="Labour & Conservative", yaxt="n",cex.lab=1.5,cex.axis=1.5,cex.main=1.5)
points(m_me_proud[2],1.5,pch=16,cex=1.3)
lines(x=c(m_me_proud[1],m_me_proud[3]), c(1.5,1.5),lwd=3)
points(m_me_hopeful[2],1,pch=16,cex=1.3)
lines(x=c(m_me_hopeful[1],m_me_hopeful[3]), c(1,1),lwd=3)
points(m_me_excited[2],0.5,pch=16,cex=1.3)
lines(x=c(m_me_excited[1],m_me_excited[3]), c(0.5,0.5),lwd=3)
points(m_me_pos[2],0,pch=16,cex=1.3)
lines(x=c(m_me_pos[1],m_me_pos[3]), c(0,0),lwd=3)
abline(v=0, lty="dashed")
axis(side=2, at=c(0, 0.5, 1, 1.5), labels=c("Enthusiasm", "Excitement", "Hopefulness", "Pride"), las=1, cex.axis=1.5)

par(mar=c(5.1, 0.01, 4.1, 2.1))
plot(x=c(), y=c(), ylim=c(-0.2, 1.7), xlim=c(-0.5,1.5), xlab="Effect of moral rhetoric", ylab="", main="Labour", yaxt="n",cex.lab=1.5,cex.axis=1.5,cex.main=1.5)
points(m_me_proud_lab[2],1.5,pch=16,cex=1.3)
lines(x=c(m_me_proud_lab[1],m_me_proud_lab[3]), c(1.5,1.5),lwd=3)
points(m_me_hopeful_lab[2],1,pch=16,cex=1.3)
lines(x=c(m_me_hopeful_lab[1],m_me_hopeful_lab[3]), c(1,1),lwd=3)
points(m_me_excited_lab[2],0.5,pch=16,cex=1.3)
lines(x=c(m_me_excited_lab[1],m_me_excited_lab[3]), c(0.5,0.5),lwd=3)
points(m_me_pos_lab[2],0,pch=16,cex=1.3)
lines(x=c(m_me_pos_lab[1],m_me_pos_lab[3]), c(0,0),lwd=3)
abline(v=0, lty="dashed")

par(mar=c(5.1, 0.01, 4.1, 2.1))
plot(x=c(), y=c(), ylim=c(-0.2, 1.7), xlim=c(-0.5,1.0), xlab="", ylab="", main="Conservative", yaxt="n",cex.lab=1.5,cex.axis=1.5,cex.main=1.5)
points(m_me_proud_cons[2],1.5,pch=16,cex=1.3)
lines(x=c(m_me_proud_cons[1],m_me_proud_cons[3]), c(1.5,1.5),lwd=3)
points(m_me_hopeful_cons[2],1,pch=16,cex=1.3)
lines(x=c(m_me_hopeful_cons[1],m_me_hopeful_cons[3]), c(1,1),lwd=3)
points(m_me_excited_cons[2],0.5,pch=16,cex=1.3)
lines(x=c(m_me_excited_cons[1],m_me_excited_cons[3]), c(0.5,0.5),lwd=3)
points(m_me_pos_cons[2],0,pch=16,cex=1.3)
lines(x=c(m_me_pos_cons[1],m_me_pos_cons[3]), c(0,0),lwd=3)
abline(v=0, lty="dashed")
dev.off()

##########################
###### Predicted probabilities
##########################

set.seed(6152)

# Turn dependent variable into factor
combined$proud <- as.factor(combined$proud)

# Run ordered logit
proud_clm <- clm(proud~treat_m, data=combined, link="logit")

# Predicted probabilities
pred_nm <- predict(proud_clm, newdata=data.frame(treat_m=0), type="prob", se.fit=T)
pred_m <- predict(proud_clm, newdata=data.frame(treat_m=1), type="prob", se.fit=T)

# Calculate FD
z.out <- zelig(proud ~ treat_m, model = "ologit", data = combined)
x.low <- setx(z.out, treat_m = 0) 
x.high <- setx(z.out, treat_m = 1) 
s.out2 <- sim(z.out, x = x.low, x1 = x.high)
summary(s.out2)
