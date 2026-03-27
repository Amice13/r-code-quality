# Results Summary
# Requires results from Main_Board.R, stored in TSCS_Simulations.RData and SmallT_Simulations.RData

rm(list=ls())
library(ggplot2)
library(reshape2)
library(gridExtra)

# Set working directory
# setwd()

# Load simulationa 
load("SmallT_Simulations.RData")
load("TSCS_Simulations.RData")

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

# True parameter values
beta <- 1.5
ape <- 0.10745

# Large N, Small T
Ns <- c(50,100,150,200)
Ts <- c(5,10,20)

beta.panel.rmse <- matrix( , nrow=length(Ns)*length(Ts),ncol=8)
beta.panel.bias <- matrix( , nrow=length(Ns)*length(Ts),ncol=8)

ape.panel.rmse <- matrix( , nrow=length(Ns)*length(Ts),ncol=8)
ape.panel.bias <- matrix( , nrow=length(Ns)*length(Ts),ncol=8)

pr.panel.bias <- matrix( , nrow=length(Ns)*length(Ts),ncol=8)


k<- 1
for (i in 1:length(Ns)) {
  for (j in 1:length(Ts)) {
   N <- Ns[i]
   T <- Ts[j]

  beta.panel.rmse[k,1:6] <- sqrt(colMeans((get(paste0("sim.res","N",N,"T",T,sep=""))[,1:6]-beta)^2))
  beta.panel.rmse[k,7] <- N
  beta.panel.rmse[k,8] <- T

  beta.panel.bias[k,1:6] <- colMeans(get(paste0("sim.res","N",N,"T",T,sep=""))[,1:6]-beta)
  beta.panel.bias[k,7] <- N
  beta.panel.bias[k,8] <- T

  ape.panel.rmse[k,1:6] <- sqrt(colMeans((get(paste0("sim.res","N",N,"T",T,sep=""))[,c(8:12,14)]-ape)^2))
  ape.panel.rmse[k,7] <- N
  ape.panel.rmse[k,8] <- T

  ape.panel.bias[k,1:6] <- colMeans(get(paste0("sim.res","N",N,"T",T,sep=""))[,c(8:12,14)]-ape)
  ape.panel.bias[k,7] <- N
  ape.panel.bias[k,8] <- T

  pr.panel.bias[k,1:6] <- colMeans(get(paste0("sim.res","N",N,"T",T,sep=""))[,c(15:19,21)])
  pr.panel.bias[k,7] <- N
  pr.panel.bias[k,8] <- T

    k <- k+1
  }
}

colnames(beta.panel.rmse) <- c("PFCRE","Oracle","CRE","CMLE","FE","BCFE","N","T")
beta.panel.rmse <- data.frame(beta.panel.rmse)
beta.panel.rmse <- melt(beta.panel.rmse, id = c("N","T"))
colnames(beta.panel.rmse)[4] <- "RMSE"

colnames(beta.panel.bias) <- c("PFCRE","Oracle","CRE","CMLE","FE","BCFE","N","T")
beta.panel.bias <- data.frame(beta.panel.bias)
beta.panel.bias <- melt(beta.panel.bias, id = c("N","T"))
colnames(beta.panel.bias)[4] <- "Bias"

colnames(ape.panel.rmse) <- c("PFCRE","Oracle","CRE","FE","BCFE","LPM","N","T")
ape.panel.rmse <- data.frame(ape.panel.rmse)
ape.panel.rmse <- melt(ape.panel.rmse, id = c("N","T"))
colnames(ape.panel.rmse)[4] <- "RMSE"

colnames(ape.panel.bias) <- c("PFCRE","Oracle","CRE","FE","BCFE","LPM","N","T")
ape.panel.bias <- data.frame(ape.panel.bias)
ape.panel.bias <- melt(ape.panel.bias, id = c("N","T"))
colnames(ape.panel.bias)[4] <- "Bias"

colnames(pr.panel.bias) <- c("PFCRE","Oracle","CRE","FE","BCFE","LPM","N","T")
pr.panel.bias <- data.frame(pr.panel.bias)
pr.panel.bias <- melt(pr.panel.bias, id = c("N","T"))
colnames(pr.panel.bias)[4] <- "Bias"

beta.panel.rmse <- beta.panel.rmse[!(beta.panel.rmse$variable %in% c("Oracle","BCFE")),]
beta.panel.bias <- beta.panel.bias[!(beta.panel.bias$variable %in% c("Oracle","BCFE")),]
ape.panel.rmse <- ape.panel.rmse[!(ape.panel.rmse$variable %in% c("Oracle","BCFE")),]
ape.panel.bias <- ape.panel.bias[!(ape.panel.bias$variable %in% c("Oracle","BCFE")),]
pr.panel.bias <- pr.panel.bias[!(pr.panel.bias$variable %in% c("Oracle","BCFE")),]

p1 <- ggplot(data = beta.panel.rmse, aes(x=N, y = RMSE, colour = variable))+geom_line()+geom_point(aes(shape=variable))+coord_cartesian(ylim = c(0, 1.5))+theme_bw()+ggtitle("Large T")
p1 <- p1 + facet_grid(. ~ T)+theme(legend.title = element_blank())
p1 <- p1 + theme(plot.title = element_text(size = 10,hjust = 0.5),axis.title = element_text(size = 10),axis.title.y = element_blank(),axis.text.x=element_text(angle=90))+scale_x_continuous(breaks=Ns)
p1


p1b <- ggplot(data = beta.panel.bias, aes(x=N, y = Bias, colour = variable))+geom_line()+geom_point(aes(shape=variable))+coord_cartesian(ylim = c(-0.5, 1.5))+theme_bw()+ggtitle("Large T")
p1b <- p1b + facet_grid(. ~ T)+theme(legend.title = element_blank())
p1b <- p1b + theme(plot.title = element_text(size = 10,hjust = 0.5),axis.title = element_text(size = 10),axis.title.y = element_blank(),axis.text.x=element_text(angle=90))+scale_x_continuous(breaks=Ns)
p1b


fp1 <- ggplot(data = ape.panel.rmse, aes(x=N, y = RMSE, colour = variable))+geom_line()+geom_point(aes(shape=variable))+coord_cartesian(ylim = c(0, 0.10))+theme_bw()+ggtitle("Large T")
fp1 <- fp1 + facet_grid(. ~ T)+theme(legend.title = element_blank())
fp1 <- fp1 + theme(plot.title = element_text(size = 10,hjust = 0.5),axis.title = element_text(size = 10),axis.title.y = element_blank(),axis.text.x=element_text(angle=90))+scale_x_continuous(breaks=Ns)
fp1

fp1b <- ggplot(data = ape.panel.bias, aes(x=N, y = Bias, colour = variable))+geom_line()+geom_point(aes(shape=variable))+coord_cartesian(ylim = c(-0.05, 0.05))+theme_bw()+ggtitle("Large T")
fp1b <- fp1b + facet_grid(. ~ T)+theme(legend.title = element_blank())
fp1b <- fp1b + theme(plot.title = element_text(size = 10,hjust = 0.5),axis.title = element_text(size = 10),axis.title.y = element_blank(),axis.text.x=element_text(angle=90))+scale_x_continuous(breaks=Ns)
fp1b

pp1 <- ggplot(data = pr.panel.bias, aes(x=N, y = Bias, colour = variable))+geom_line()+geom_point(aes(shape=variable))+coord_cartesian(ylim = c(0, 0.3))+theme_bw()+ggtitle("Large T")
pp1 <- pp1 + facet_grid(. ~ T)+theme(legend.title = element_blank())
pp1 <- pp1 + theme(plot.title = element_text(size = 10,hjust = 0.5),axis.title = element_text(size = 10),axis.title.y = element_blank(),axis.text.x=element_text(angle=90))+scale_x_continuous(breaks=Ns)
pp1



# Small N, Large T
Ns <- c(200,500,1000,2000)
Ts <- c(2,3,5)

beta.panel.rmse <- matrix( , nrow=length(Ns)*length(Ts),ncol=8)
beta.panel.bias <- matrix( , nrow=length(Ns)*length(Ts),ncol=8)

ape.panel.rmse <- matrix( , nrow=length(Ns)*length(Ts),ncol=8)
ape.panel.bias <- matrix( , nrow=length(Ns)*length(Ts),ncol=8)

pr.panel.bias <- matrix( , nrow=length(Ns)*length(Ts),ncol=8)


k<- 1
for (i in 1:length(Ns)) {
  for (j in 1:length(Ts)) {
    N <- Ns[i]
    T <- Ts[j]

    beta.panel.rmse[k,1:6] <- sqrt(colMeans((get(paste0("sim.res","N",N,"T",T,sep=""))[,1:6]-beta)^2))
    beta.panel.rmse[k,7] <- N
    beta.panel.rmse[k,8] <- T

    beta.panel.bias[k,1:6] <- colMeans(get(paste0("sim.res","N",N,"T",T,sep=""))[,1:6]-beta)
    beta.panel.bias[k,7] <- N
    beta.panel.bias[k,8] <- T

    ape.panel.rmse[k,1:6] <- sqrt(colMeans((get(paste0("sim.res","N",N,"T",T,sep=""))[,c(8:12,14)]-ape)^2))
    ape.panel.rmse[k,7] <- N
    ape.panel.rmse[k,8] <- T

    ape.panel.bias[k,1:6] <- colMeans(get(paste0("sim.res","N",N,"T",T,sep=""))[,c(8:12,14)]-ape)
    ape.panel.bias[k,7] <- N
    ape.panel.bias[k,8] <- T

    pr.panel.bias[k,1:6] <- colMeans(get(paste0("sim.res","N",N,"T",T,sep=""))[,c(15:19,21)])
    pr.panel.bias[k,7] <- N
    pr.panel.bias[k,8] <- T

    k <- k+1
  }
}

colnames(beta.panel.rmse) <- c("PFCRE","Oracle","CRE","CMLE","FE","BCFE","N","T")
beta.panel.rmse <- data.frame(beta.panel.rmse)
beta.panel.rmse <- melt(beta.panel.rmse, id = c("N","T"))
colnames(beta.panel.rmse)[4] <- "RMSE"

colnames(beta.panel.bias) <- c("PFCRE","Oracle","CRE","CMLE","FE","BCFE","N","T")
beta.panel.bias <- data.frame(beta.panel.bias)
beta.panel.bias <- melt(beta.panel.bias, id = c("N","T"))
colnames(beta.panel.bias)[4] <- "Bias"

colnames(ape.panel.rmse) <- c("PFCRE","Oracle","CRE","FE","BCFE","LPM","N","T")
ape.panel.rmse <- data.frame(ape.panel.rmse)
ape.panel.rmse <- melt(ape.panel.rmse, id = c("N","T"))
colnames(ape.panel.rmse)[4] <- "RMSE"

colnames(ape.panel.bias) <- c("PFCRE","Oracle","CRE","FE","BCFE","LPM","N","T")
ape.panel.bias <- data.frame(ape.panel.bias)
ape.panel.bias <- melt(ape.panel.bias, id = c("N","T"))
colnames(ape.panel.bias)[4] <- "Bias"

colnames(pr.panel.bias) <- c("PFCRE","Oracle","CRE","FE","BCFE","LPM","N","T")
pr.panel.bias <- data.frame(pr.panel.bias)
pr.panel.bias <- melt(pr.panel.bias, id = c("N","T"))
colnames(pr.panel.bias)[4] <- "Bias"

beta.panel.rmse <- beta.panel.rmse[!(beta.panel.rmse$variable %in% c("Oracle","BCFE")),]
beta.panel.bias <- beta.panel.bias[!(beta.panel.bias$variable %in% c("Oracle","BCFE")),]
ape.panel.rmse <- ape.panel.rmse[!(ape.panel.rmse$variable %in% c("Oracle","BCFE")),]
ape.panel.bias <- ape.panel.bias[!(ape.panel.bias$variable %in% c("Oracle","BCFE")),]
pr.panel.bias <- pr.panel.bias[!(pr.panel.bias$variable %in% c("Oracle","BCFE")),]

p2 <- ggplot(data = beta.panel.rmse, aes(x=N, y = RMSE, colour = variable))+geom_line()+geom_point(aes(shape=variable))+coord_cartesian(ylim = c(0, 1.5))+theme_bw()+ggtitle("Small T")
p2 <- p2 + facet_grid(. ~ T)+theme(legend.title = element_blank())
p2 <- p2 + theme(plot.title = element_text(size = 10,hjust = 0.5),axis.title = element_text(size = 10),axis.title.y = element_blank(),axis.text.x=element_text(angle=90))+scale_x_continuous(breaks=Ns)
p2


p2b <- ggplot(data = beta.panel.bias, aes(x=N, y = Bias, colour = variable))+geom_line()+geom_point(aes(shape=variable))+coord_cartesian(ylim = c(-0.5, 1.5))+theme_bw()+ggtitle("Large N")
p2b <- p2b + facet_grid(. ~ T)+theme(legend.title = element_blank())
p2b <- p2b + theme(plot.title = element_text(size = 10,hjust = 0.5),axis.title = element_text(size = 10),axis.title.y = element_blank(),axis.text.x=element_text(angle=90))+scale_x_continuous(breaks=Ns)
p2b


fp2 <- ggplot(data = ape.panel.rmse, aes(x=N, y = RMSE, colour = variable))+geom_line()+geom_point(aes(shape=variable))+coord_cartesian(ylim = c(0, 0.10))+theme_bw()+ggtitle("Large N")
fp2 <- fp2 + facet_grid(. ~ T)+theme(legend.title = element_blank())
fp2 <- fp2 + theme(plot.title = element_text(size = 10,hjust = 0.5),axis.title = element_text(size = 10),axis.title.y = element_blank(),axis.text.x=element_text(angle=90))+scale_x_continuous(breaks=Ns)
fp2

fp2b <- ggplot(data = ape.panel.bias, aes(x=N, y = Bias, colour = variable))+geom_line()+geom_point(aes(shape=variable))+coord_cartesian(ylim = c(-0.05, 0.05))+theme_bw()+ggtitle("Large N")
fp2b <- fp2b + facet_grid(. ~ T)+theme(legend.title = element_blank())
fp2b <- fp2b + theme(plot.title = element_text(size = 10,hjust = 0.5),axis.title = element_text(size = 10),axis.title.y = element_blank(),axis.text.x=element_text(angle=90))+scale_x_continuous(breaks=Ns)
fp2b

pp2 <- ggplot(data = pr.panel.bias, aes(x=N, y = Bias, colour = variable))+geom_line()+geom_point(aes(shape=variable))+coord_cartesian(ylim = c(0, 0.3))+theme_bw()+ggtitle("Large N")
pp2 <- pp2 + facet_grid(. ~ T)+theme(legend.title = element_blank())
pp2 <- pp2 + theme(plot.title = element_text(size = 10,hjust = 0.5),axis.title = element_text(size = 10),axis.title.y = element_blank(),axis.text.x=element_text(angle=90))+scale_x_continuous(breaks=Ns)
pp2

mylegend<-g_legend(p1 + theme(legend.position="bottom"))
mylegend2 <-g_legend(fp2b + theme(legend.position="bottom"))


######################################################################
# PARAMETER, AVERAGE PARTIAL EFFECT, AND PREDICTED PROBABILITIES PLOTS
######################################################################

pdf("beta_rmse_2020.pdf",width=7,height=5)
f.beta.rmse <- grid.arrange(arrangeGrob(p1 + theme(legend.position="none"),
                                        p2 + theme(legend.position="none"),
                                        mylegend,nrow=3,ncol=1,heights=c(10,10,1.6)))
dev.off()


pdf("beta_bias_2020.pdf",width=7,height=5)
f.beta.bias <- grid.arrange(arrangeGrob(p1b + theme(legend.position="none"),
                                        p2b + theme(legend.position="none"),
                                        mylegend,nrow=3,ncol=1,heights=c(10,10,1.6)))
dev.off()


pdf("ape_rmse_2020.pdf",width=7,height=5)
f.ape.rmse <- grid.arrange(arrangeGrob(fp1 + theme(legend.position="none"),
                                       fp2 + theme(legend.position="none"),
                                       mylegend2,nrow=3,ncol=1,heights=c(10,10,1.6)))
dev.off()

pdf("ape_bias_2020.pdf",width=7,height=5)
f.ape.bias <- grid.arrange(arrangeGrob(fp1b + theme(legend.position="none"),
                                       fp2b + theme(legend.position="none"),
                                       mylegend2,nrow=3,ncol=1,heights=c(10,10,1.6)))
dev.off()

pdf("pr_bias_2020.pdf",width=7,height=5)
f.pr.bias <- grid.arrange(arrangeGrob(pp1 + theme(legend.position="none"),
                                      pp2 + theme(legend.position="none"),
                                      mylegend2,nrow=3,ncol=1,heights=c(10,10,1.6)))
dev.off()


### Comparing PF-CRE distributions to that from the Oracle. 
# Obtain estimates, and put in a big matrix all under each other, together with the N and T to do grid_facets later


LargeT <- data.frame(rbind(sim.resN50T5[,1:2],sim.resN50T10[,1:2],sim.resN50T20[,1:2],
                           sim.resN100T5[,1:2],sim.resN100T10[,1:2],sim.resN100T20[,1:2],
                           sim.resN150T5[,1:2],sim.resN150T10[,1:2],sim.resN150T20[,1:2],
                           sim.resN200T5[,1:2],sim.resN200T10[,1:2],sim.resN200T20[,1:2]))
colnames(LargeT) <- c("PFCRE","Oracle")
LargeT$N <- c(rep(50,nrow(sim.resN50T5)+nrow(sim.resN50T10)+nrow(sim.resN50T20)),
              rep(100,nrow(sim.resN100T5)+nrow(sim.resN100T10)+nrow(sim.resN100T20)),
              rep(150,nrow(sim.resN150T5)+nrow(sim.resN150T10)+nrow(sim.resN150T20)),
              rep(200,nrow(sim.resN200T5)+nrow(sim.resN200T10)+nrow(sim.resN200T20)))
LargeT$T <- c(rep(5,nrow(sim.resN50T5)), rep(10,nrow(sim.resN50T10)), rep(20,nrow(sim.resN50T20)),
              rep(5,nrow(sim.resN100T5)), rep(10,nrow(sim.resN100T10)), rep(20,nrow(sim.resN100T20)),
              rep(5,nrow(sim.resN150T5)), rep(10,nrow(sim.resN150T10)), rep(20,nrow(sim.resN150T20)),
              rep(5,nrow(sim.resN200T5)), rep(10,nrow(sim.resN200T10)), rep(20,nrow(sim.resN200T20)))

den.largeT <- ggplot(LargeT) + geom_density(aes(x = PFCRE),col = "blue") + geom_density(aes( x = Oracle), col = "red", lty =2) 
den.largeT <- den.largeT + xlim(c(0.5,2.5)) + theme_bw() + xlab("") + facet_grid(N ~  T)
den.largeT


SmallT <- data.frame(rbind(sim.resN200T2[,1:2],sim.resN200T3[,1:2],sim.resN200T5[,1:2],
                           sim.resN500T2[,1:2],sim.resN500T3[,1:2],sim.resN500T5[,1:2],
                           sim.resN1000T2[,1:2],sim.resN1000T3[,1:2],sim.resN1000T5[,1:2],
                           sim.resN2000T2[,1:2],sim.resN2000T3[,1:2],sim.resN2000T5[,1:2]))
colnames(SmallT) <- c("PFCRE","Oracle")
SmallT$N <- c(rep(200,nrow(sim.resN200T2)+nrow(sim.resN200T3)+nrow(sim.resN200T5)),
              rep(500,nrow(sim.resN500T2)+nrow(sim.resN500T3)+nrow(sim.resN500T5)),
              rep(1000,nrow(sim.resN1000T2)+nrow(sim.resN1000T3)+nrow(sim.resN1000T5)),
              rep(2000,nrow(sim.resN2000T2)+nrow(sim.resN2000T3)+nrow(sim.resN2000T5)))
SmallT$T <- c(rep(2,nrow(sim.resN200T2)), rep(3,nrow(sim.resN200T3)), rep(5,nrow(sim.resN200T5)),
              rep(2,nrow(sim.resN500T2)), rep(3,nrow(sim.resN500T3)), rep(5,nrow(sim.resN500T5)),
              rep(2,nrow(sim.resN1000T2)), rep(3,nrow(sim.resN1000T3)), rep(5,nrow(sim.resN1000T5)),
              rep(2,nrow(sim.resN2000T2)), rep(3,nrow(sim.resN2000T3)), rep(5,nrow(sim.resN2000T5)))


den.smallT <- ggplot(SmallT) + geom_density(aes(x = PFCRE),col = "blue") + geom_density(aes( x = Oracle), col = "red", lty =2) 
den.smallT <- den.smallT + xlim(c(0.5,2.5)) + theme_bw() + xlab("") + facet_grid(N ~  T)
den.smallT


pdf("PFCRE_Oracle_LargeT.pdf",width=7,height=7)
den.largeT
dev.off()

pdf("PFCRE_Oracle_SmallT.pdf",width=7,height=7)
den.smallT
dev.off()
