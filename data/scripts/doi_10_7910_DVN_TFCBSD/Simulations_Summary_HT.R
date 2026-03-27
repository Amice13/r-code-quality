# Summary of Specification Test Results
rm(list=ls())
library(ggplot2)
library(reshape2)
library(gridExtra)

# Set working directory
# setwd()

# Load the simulations from both sets 
load("HT_SmallT_Simulations.RData")
load("HT_SmallT_Simulations2.RData")
load("HT_TSCS_Simulations.RData")
load("HT_TSCS_Simulations2.RData")


# First Merge the two separate runs. 
Ns <- c(50,100,150,200,500,1000,2000)
Ts <- c(2,3,5,10,20)


for (i in 1:length(Ns)) {
  for (j in 1:length(Ts)) {
    tryCatch({
      N <- Ns[i]
      T <- Ts[j]
 obj <- get(paste0("sim.resHT_","N",N,"T",T,sep=""))
 obj2 <- get(paste0("sim.resHT2_","N",N,"T",T,sep=""))
  
 out <- rbind(obj,obj2)
 l <- dim(out)[1]
 out <- data.frame(out)
 colnames(out) <- c("HT","pval")
 out <- out[order(out$HT),]
 out <- cbind(out,rep(N,l),rep(T,l),qchisq(ppoints(nrow(out)),4))
 
 colnames(out) <- c("HT","pval","N","T","theo")

if (exists("out2")) {out2 <- rbind(out2,out)}
 else {out2 <- out}
 
 
     }, error = function(e){} )
    
  }
}


out2 <- data.frame(out2)
out2 <- out2[complete.cases(out2),]


# For Large T

jpeg("ht_1.jpeg",width=600,height=325)
p <- ggplot(out2[out2$T %in% c(5,10,20) & out2$N %in% c(50,100,150,200),])+geom_point(aes(HT,theo))+theme_bw()+geom_abline(intercept=0,slope=1,alpha=0.5)+labs(x="Observed",y="Theoretical")+xlim(0,15)+ylim(0,15)+annotate("rect",xmin=0,xmax=9.48,ymin=0,ymax=9.48,alpha=0.4)
p <- p+facet_grid(N ~ T)
p
dev.off()

jpeg("ht_2.jpeg",width=600,height=325)
p <- ggplot(out2[out2$T %in% c(2,3,5) & out2$N %in% c(200,500,1000,2000),])+geom_point(aes(HT,theo))+theme_bw()+geom_abline(intercept=0,slope=1,alpha=0.5)+labs(x="Observed",y="Theoretical")+xlim(0,15)+ylim(0,15)+annotate("rect",xmin=0,xmax=9.48,ymin=0,ymax=9.48,alpha=0.4)
p <- p+facet_grid(N ~ T)
p
dev.off()