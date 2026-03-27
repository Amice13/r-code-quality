###################################################################################
## Graph for our default prior for phi, the concentration parameter of the 
## Chinese restaurant process prior on partitions (Figure 1 in Appendix)

phiprior <- function(x,KK){
  if(KK<2) stop("KK must be an integer greater or equal to 2")
  z1 <- lgamma(KK) + lgamma(x+1) - lgamma(x+KK)
  z2 <- 0
  for(k in 0:(KK-2)){
    z2 <- z2 + 1/(x + k + 1)
  }
  z <- exp(z1)*z2
}

x <- seq(0,8,length=1000)
y2  <- phiprior(x, KK=2)
y19 <- phiprior(x, KK=19)
pdf(file="../results/AppendixFigure1.pdf", width=7, height=7)
plot(x,y19, xlab=expression(phi), ylab=expression(p(phi)), type="l", col="blue", lty=3, cex.lab=1.4)
lines(x,y2, type="l", col="black", lty=1)
legend(5,3.2, c("K = 2", "K = 19"), col=c("black","blue"), lty=c(1,3), bty="n", cex=1.4)
dev.off()

###################################################################################
## Table with the count of bills for each of the topics include  
## Chinese restaurant process prior on partitions (Figure 1 in Appendix)
typenames = c("Macroeconomics", "Civil Rights", "Health", 
              "Agriculture", "Labor and Employment", "Education", "Environment", 
              "Energy", "Transportation", "Law and Crime", "Housing", 
              "Banking and Finance", "Defense", "Space, Science and Communications", 
              "International Affairs", "Government Operations", "Public Lands")

chouses <- seq(97,114)
nchouses <- length(chouses)
votecount <- matrix(NA, nrow=nchouses, ncol=17)
for(i in 1:nchouses){
  group         <- scan(paste("../data/H",chouses[i],"codesPAP.txt", sep=""))
  votecount[i,] <- table(group)
}
colnames(votecount) <- typenames
rownames(votecount) <- chouses
print(votecount)
write.table(votecount, file="../results/AppendixTable1.txt", sep=",")



