# Results Summary

rm(list=ls())
library(ggplot2)
library(reshape2)
library(gridExtra)

# Set working directory
# setwd()

load("Time_and_fcre.RData")


g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}


beta <- 1.5

# Large N, Small T
Ns <- c(50,100,150,200)
Ts <- c(5,10,20)

beta.panel.rmse <- matrix( , nrow=length(Ns)*length(Ts),ncol=7)
beta.panel.bias <- matrix( , nrow=length(Ns)*length(Ts),ncol=7)
beta.panel.stds <- matrix( , nrow=length(Ns)*length(Ts),ncol=7)


k<- 1
for (i in 1:length(Ns)) {
  for (j in 1:length(Ts)) {
    N <- Ns[i]
    T <- Ts[j]
    
    beta.panel.rmse[k,1:5] <- sqrt(colMeans((get(paste0("sim.res","N",N,"T",T,sep=""))[,1:5]-beta)^2))
    beta.panel.rmse[k,6] <- N
    beta.panel.rmse[k,7] <- T
    
    beta.panel.bias[k,1:5] <- colMeans(get(paste0("sim.res","N",N,"T",T,sep=""))[,1:5]-beta)
    beta.panel.bias[k,6] <- N
    beta.panel.bias[k,7] <- T
    
    k <- k+1
  }
}

beta.panel.stds <- beta.panel.rmse
beta.panel.stds[,1:5] <- sqrt(beta.panel.rmse[,1:5]^2 - beta.panel.bias[,1:5]^2)


colnames(beta.panel.rmse) <- c("PFCRE","Oracle","CRE","CMLE","FCRE","N","T")
beta.panel.rmse <- data.frame(beta.panel.rmse)
beta.panel.rmse <- melt(beta.panel.rmse, id = c("N","T"))
colnames(beta.panel.rmse)[4] <- "RMSE"

colnames(beta.panel.bias) <- c("PFCRE","Oracle","CRE","CMLE","FCRE","N","T")
beta.panel.bias <- data.frame(beta.panel.bias)
beta.panel.bias <- melt(beta.panel.bias, id = c("N","T"))
colnames(beta.panel.bias)[4] <- "Bias"

colnames(beta.panel.stds) <- c("PFCRE","Oracle","CRE","CMLE","FCRE","N","T")
beta.panel.stds <- data.frame(beta.panel.stds)
beta.panel.stds <- melt(beta.panel.stds, id = c("N","T"))
colnames(beta.panel.stds)[4] <- "Std"

beta.panel.rmse <- beta.panel.rmse[!(beta.panel.rmse$variable %in% c("CMLE","CRE")),]
beta.panel.bias <- beta.panel.bias[!(beta.panel.bias$variable %in% c("CMLE","CRE")),]
beta.panel.stds <- beta.panel.stds[!(beta.panel.stds$variable %in% c("CMLE","CRE")),]


p1 <- ggplot(data = beta.panel.rmse, aes(x=N, y = RMSE, colour = variable))+geom_line()+geom_point(aes(shape=variable))+coord_cartesian(ylim = c(0, 1.5))+theme_bw()+ggtitle("Large T")
p1 <- p1 + facet_grid(. ~ T)+theme(legend.title = element_blank())
p1 <- p1 + theme(plot.title = element_text(size = 10,hjust = 0.5),axis.title = element_text(size = 10),axis.title.y = element_blank(),axis.text.x=element_text(angle=90))+scale_x_continuous(breaks=Ns)
p1


p1b <- ggplot(data = beta.panel.bias, aes(x=N, y = Bias, colour = variable))+geom_line()+geom_point(aes(shape=variable))+coord_cartesian(ylim = c(-0.5, 1.5))+theme_bw()+ggtitle("Large T")
p1b <- p1b + facet_grid(. ~ T)+theme(legend.title = element_blank())
p1b <- p1b + theme(plot.title = element_text(size = 10,hjust = 0.5),axis.title = element_text(size = 10),axis.title.y = element_blank(),axis.text.x=element_text(angle=90))+scale_x_continuous(breaks=Ns)
p1b


pp1 <- ggplot(data = beta.panel.stds, aes(x=N, y = Std, colour = variable))+geom_line()+geom_point(aes(shape=variable))+coord_cartesian(ylim = c(0, 2))+theme_bw()+ggtitle("Large T")
pp1 <- pp1 + facet_grid(. ~ T)+theme(legend.title = element_blank())
pp1 <- pp1 + theme(plot.title = element_text(size = 10,hjust = 0.5),axis.title = element_text(size = 10),axis.title.y = element_blank(),axis.text.x=element_text(angle=90))+scale_x_continuous(breaks=Ns)
pp1



# Small N, Large T
Ns <- c(200,500,1000,2000)
Ts <- c(2,3,5)

beta.panel.rmse <- matrix( , nrow=length(Ns)*length(Ts),ncol=7)
beta.panel.bias <- matrix( , nrow=length(Ns)*length(Ts),ncol=7)


k<- 1
for (i in 1:length(Ns)) {
  for (j in 1:length(Ts)) {
    N <- Ns[i]
    T <- Ts[j]
    
    beta.panel.rmse[k,1:5] <- sqrt(colMeans((get(paste0("sim.res","N",N,"T",T,sep=""))[,1:5]-beta)^2))
    beta.panel.rmse[k,6] <- N
    beta.panel.rmse[k,7] <- T
    
    beta.panel.bias[k,1:5] <- colMeans(get(paste0("sim.res","N",N,"T",T,sep=""))[,1:5]-beta)
    beta.panel.bias[k,6] <- N
    beta.panel.bias[k,7] <- T
    
    k <- k+1
  }
}


beta.panel.stds <- beta.panel.rmse
beta.panel.stds[,1:5] <- sqrt(beta.panel.rmse[,1:5]^2 - beta.panel.bias[,1:5]^2)


colnames(beta.panel.rmse) <- c("PFCRE","Oracle","CRE","CMLE","FCRE","N","T")
beta.panel.rmse <- data.frame(beta.panel.rmse)
beta.panel.rmse <- melt(beta.panel.rmse, id = c("N","T"))
colnames(beta.panel.rmse)[4] <- "RMSE"

colnames(beta.panel.bias) <- c("PFCRE","Oracle","CRE","CMLE","FCRE","N","T")
beta.panel.bias <- data.frame(beta.panel.bias)
beta.panel.bias <- melt(beta.panel.bias, id = c("N","T"))
colnames(beta.panel.bias)[4] <- "Bias"

colnames(beta.panel.stds) <- c("PFCRE","Oracle","CRE","CMLE","FCRE","N","T")
beta.panel.stds <- data.frame(beta.panel.stds)
beta.panel.stds <- melt(beta.panel.stds, id = c("N","T"))
colnames(beta.panel.stds)[4] <- "Std"


beta.panel.rmse <- beta.panel.rmse[!(beta.panel.rmse$variable %in% c("CMLE","CRE")),]
beta.panel.bias <- beta.panel.bias[!(beta.panel.bias$variable %in% c("CMLE","CRE")),]
beta.panel.stds <- beta.panel.stds[!(beta.panel.stds$variable %in% c("CMLE","CRE")),]

p2 <- ggplot(data = beta.panel.rmse, aes(x=N, y = RMSE, colour = variable))+geom_line()+geom_point(aes(shape=variable))+coord_cartesian(ylim = c(0, 1.5))+theme_bw()+ggtitle("Small T")
p2 <- p2 + facet_grid(. ~ T)+theme(legend.title = element_blank())
p2 <- p2 + theme(plot.title = element_text(size = 10,hjust = 0.5),axis.title = element_text(size = 10),axis.title.y = element_blank(),axis.text.x=element_text(angle=90))+scale_x_continuous(breaks=Ns)
p2


p2b <- ggplot(data = beta.panel.bias, aes(x=N, y = Bias, colour = variable))+geom_line()+geom_point(aes(shape=variable))+coord_cartesian(ylim = c(-0.5, 1.5))+theme_bw()+ggtitle("Large N")
p2b <- p2b + facet_grid(. ~ T)+theme(legend.title = element_blank())
p2b <- p2b + theme(plot.title = element_text(size = 10,hjust = 0.5),axis.title = element_text(size = 10),axis.title.y = element_blank(),axis.text.x=element_text(angle=90))+scale_x_continuous(breaks=Ns)
p2b


pp2 <- ggplot(data = beta.panel.stds, aes(x=N, y = Std, colour = variable))+geom_line()+geom_point(aes(shape=variable))+coord_cartesian(ylim = c(0, 2))+theme_bw()+ggtitle("Large T")
pp2 <- pp2 + facet_grid(. ~ T)+theme(legend.title = element_blank())
pp2 <- pp2 + theme(plot.title = element_text(size = 10,hjust = 0.5),axis.title = element_text(size = 10),axis.title.y = element_blank(),axis.text.x=element_text(angle=90))+scale_x_continuous(breaks=Ns)
pp2

mylegend<-g_legend(p1 + theme(legend.position="bottom"))


######################################################################
# PARAMETER, AVERAGE PARTIAL EFFECT, AND PREDICTED PROBABILITIES PLOTS
######################################################################

pdf("beta_rmse_fcre.pdf",width=7,height=5)
f.beta.rmse <- grid.arrange(arrangeGrob(p1 + theme(legend.position="none"),
                                        p2 + theme(legend.position="none"),
                                        mylegend,nrow=3,ncol=1,heights=c(10,10,1.6)))
dev.off()


pdf("beta_bias_fcre.pdf",width=7,height=5)
f.beta.bias <- grid.arrange(arrangeGrob(p1b + theme(legend.position="none"),
                                        p2b + theme(legend.position="none"),
                                        mylegend,nrow=3,ncol=1,heights=c(10,10,1.6)))
dev.off()


pdf("beta_stds_fcre.pdf",width=7,height=5)
f.ape.rmse <- grid.arrange(arrangeGrob(pp1 + theme(legend.position="none"),
                                       pp2 + theme(legend.position="none"),
                                       mylegend,nrow=3,ncol=1,heights=c(10,10,1.6)))
dev.off()
