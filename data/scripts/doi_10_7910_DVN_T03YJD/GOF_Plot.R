#########################################
### Plot figures 1a and 2 in Appendix ###
#########################################


load(file="scripts/GOF_1950.RData")


# create goodness-of-fit plots

pdf("figures/figure1aAppendix.pdf", height=8.27, width=11.69)
par(mfrow=c(2,2), oma=c(0,0,2,0))
boxplot(odeg.matrix, use.col=TRUE, col="lightgrey", main="Outdegree", xlab="Outddegree Distribution", cex.axis=1.8, cex.lab=1.7, cex.main=2.5)
lines(odeg.obs, type="l", lwd=2)

boxplot(ideg.matrix, use.col=TRUE, col="lightgrey", main="Indegree", xlab="Inddegree Distribution", cex.axis=1.8, cex.lab=1.7, cex.main=2.5)
lines(ideg.obs, type="l", lwd=2)

boxplot(esp.matrix.ost, use.col=TRUE, col="lightgrey", main="Edgewise Shared Partners OTP", xlab="ESP Distribution",
        cex.axis=1.8, cex.lab=1.7, cex.main=2.5)
lines(unfixed.esp.ost, type="l", lwd=2)

boxplot(esp.matrix.osp, use.col=TRUE, col="lightgrey", main="Edgewise Shared Partners OSP", xlab="ESP Distribution",
        cex.axis=1.8, cex.lab=1.7, cex.main=2.5)
lines(unfixed.esp.osp, type="l", lwd=2)

title("Goodness-of-fit 1950",outer=TRUE , cex.main=2.5)
dev.off()

######################################
# create MCMC trace plots
######################################

sim <- li[[6]]
erg.est <- li[[7]]

pdf("figures/figure2Appendix.pdf", width=8.27, height=11.69)
par(mfrow=c(5,2), oma=c(0,0,2,0))
hist(sim[,1], main="Density of Edges", col="grey55", xlab="Edges", cex.main=1.8, cex.lab=1.5, cex.axis=1.7)
abline(v=erg.est[1],lwd=3)
plot(sim[,1], main="Edges", col="grey45", xlab="Iterations", ylab="Edges", cex.main=1.8, cex.lab=1.5, cex.axis=1.7)
abline(h=erg.est[1],lwd=2)

hist(sim[,3], main="Density of Receiver Outdegree", col="grey55", xlab="Receiver Outdegree", cex.main=1.8, cex.lab=1.5, cex.axis=1.7)
abline(v=erg.est[3],lwd=3)
plot(sim[,3], main="Receiver Outdegree", col="grey45", xlab="Iterations", ylab="Receiver Outdegree", cex.main=1.8, cex.lab=1.5, cex.axis=1.7)
abline(h=erg.est[3],lwd=2)

hist(sim[,4], main="Density of GWIDEGREE", col="grey55", xlab="GWIDEGREE", cex.main=1.8, cex.lab=1.5, cex.axis=1.7)
abline(v=erg.est[4],lwd=3)
plot(sim[,4], main="GWIDEGREE", col="grey45", xlab="Iterations", ylab="GWIDEGREE", cex.main=1.8, cex.lab=1.5, cex.axis=1.7)
abline(h=erg.est[4],lwd=2)

hist(sim[,5], main="Density of Different Term Transitivity", col="grey55", xlab="Different Term Transitivity", cex.main=1.8, cex.lab=1.5, cex.axis=1.7)
abline(v=erg.est[5],lwd=3)
plot(sim[,5], main="Different Term Transitivity", col="grey45", xlab="Iterations", ylab="Different Term Transitivity", 
     cex.main=1.8, cex.lab=1.5, cex.axis=1.7)
abline(h=erg.est[5],lwd=2)

hist(sim[,6], main="Density of GWESP", col="grey55", xlab="GWESP", cex.main=1.8, cex.lab=1.5, cex.axis=1.7)
abline(v=erg.est[6],lwd=3)
plot(sim[,6], main="GWESP", col="grey45", xlab="Iterations", ylab="GWESP", cex.main=1.8, cex.lab=1.5, cex.axis=1.7)
abline(h=erg.est[6],lwd=2)

title('Degeneracy Check 1950', outer=TRUE, cex.main=3)
dev.off()

rm(list=ls())

############################################
###  Plot figures 1b and 3 in Appendix   ###
############################################

load(file="scripts/GOF_2015.RData")



###################################
# create goodness-of-fit plots

pdf("figures/figure1bAppendix.pdf", height=8.27, width=11.69)
par(mfrow=c(2,2), oma=c(0,0,2,0))
boxplot(odeg.matrix, use.col=TRUE, col="lightgrey", main="Outdegree", xlab="Outddegree Distribution", cex.axis=1.8, cex.lab=1.7, cex.main=2.5)
lines(odeg.obs, type="l", lwd=2)

boxplot(ideg.matrix, use.col=TRUE, col="lightgrey", main="Indegree", xlab="Inddegree Distribution", cex.axis=1.8, cex.lab=1.7, cex.main=2.5)
lines(ideg.obs, type="l", lwd=2)


boxplot(esp.matrix.ost, use.col=TRUE, col="lightgrey", main="Edgewise Shared Partners OTP", xlab="ESP Distribution",
        cex.axis=1.8, cex.lab=1.7, cex.main=2.5)
lines(unfixed.esp.ost, type="l", lwd=2)

boxplot(esp.matrix.osp, use.col=TRUE, col="lightgrey", main="Edgewise Shared Partners OSP", xlab="ESP Distribution",
        cex.axis=1.8, cex.lab=1.7, cex.main=2.5)
lines(unfixed.esp.osp, type="l", lwd=2)

title("Goodness-of-fit 2015",outer=TRUE , cex.main=2.5)
dev.off()



######################################
# create MCMC trace plots

sim <- li[[6]]
erg.est <- li[[7]]

pdf("figures/figure3Appendix.pdf", width=8.27, height=11.69)
par(mfrow=c(5,2), oma=c(0,0,2,0))
hist(sim[,1], main="Density of Edges", col="grey55", xlab="Edges", cex.main=1.8, cex.lab=1.5, cex.axis=1.7)
abline(v=erg.est[1],lwd=3)
plot(sim[,1], main="Edges", col="grey45", xlab="Iterations", ylab="Edges", cex.main=1.8, cex.lab=1.5, cex.axis=1.7)
abline(h=erg.est[1],lwd=2)

hist(sim[,3], main="Density of Receiver Outdegree", col="grey55", xlab="Receiver Outdegree", cex.main=1.8, cex.lab=1.5, cex.axis=1.7)
abline(v=erg.est[3],lwd=3)
plot(sim[,3], main="Receiver Outdegree", col="grey45", xlab="Iterations", ylab="Receiver Outdegree", cex.main=1.8, cex.lab=1.5, cex.axis=1.7)
abline(h=erg.est[3],lwd=2)

hist(sim[,4], main="Density of GWIDEGREE", col="grey55", xlab="GWIDEGREE", cex.main=1.8, cex.lab=1.5, cex.axis=1.7)
abline(v=erg.est[4],lwd=3)
plot(sim[,4], main="GWIDEGREE", col="grey45", xlab="Iterations", ylab="GWIDEGREE", cex.main=1.8, cex.lab=1.5, cex.axis=1.7)
abline(h=erg.est[4],lwd=2)

hist(sim[,5], main="Density of Different Term Transitivity", col="grey55", xlab="Different Term Transitivity", cex.main=1.8, cex.lab=1.5, cex.axis=1.7)
abline(v=erg.est[5],lwd=3)
plot(sim[,5], main="Different Term Transitivity", col="grey45", xlab="Iterations", ylab="Different Term Transitivity", 
     cex.main=1.8, cex.lab=1.5, cex.axis=1.7)
abline(h=erg.est[5],lwd=2)

hist(sim[,6], main="Density of GWESP", col="grey55", xlab="GWESP", cex.main=1.8, cex.lab=1.5, cex.axis=1.7)
abline(v=erg.est[6],lwd=3)
plot(sim[,6], main="GWESP", col="grey45", xlab="Iterations", ylab="GWESP", cex.main=1.8, cex.lab=1.5, cex.axis=1.7)
abline(h=erg.est[6],lwd=2)

title('Degeneracy Check 2015', outer=TRUE, cex.main=3)
dev.off()

