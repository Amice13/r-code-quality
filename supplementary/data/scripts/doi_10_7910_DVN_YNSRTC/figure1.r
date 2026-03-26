# Best response graphs

date()
library("ggpubr")
library("tidyverse")
library("Formula")
library("haven")
library("maxLik")
library("matrixStats")
library("ggplot2")
sessionInfo()


bootV <- read_stata("boot_res_region_t.dta")
bootV <- bootV %>% drop_na()
bootV <- as.matrix(bootV[,73:176])
Est = bootV[1,]

ggData <- data.frame(prV = Est,
                     x = rep(seq(from=0.2, to=0.8, by = 0.05), 4*2),
                     lo = pmax(Est + qnorm(0.025)*colSds(bootV), 0),
                     hi = Est + qnorm(0.975)*colSds(bootV),
                     actorY = rep(c("FARC", "AUC"), each = 13*4),
                     actorX = rep(c("AUC", "FARC"), each = 13*4),
                     typeY =  rep(rep(c("Selective", "Non-selective"), each=13), 4),
                     typeX = rep(rep(c("Selective", "Non-selective"), each=26), 2))

pFS <- ggplot(subset(ggData, actorY=="FARC" & typeX == "Selective")) + 
          geom_line(aes(x=x, y = prV, color=typeY, linetype=typeY), size=1.1) + 
           geom_ribbon(aes(x=x, ymin=lo, ymax=hi, fill=typeY),alpha=0.3) +
           theme_bw(11) + ylim(c(0,1)) + 
           ylab("FARC's probability of violence") + 
           xlab("AUC's probability of selective violence") + 
           theme(legend.position = "bottom") + 
            labs(color = "", linetype = "", fill="") + 
            scale_color_manual(values=c("#0072B2", "#D55E00")) + 
            scale_fill_manual(values=c("#0072B2", "#D55E00"))
 
pFN <- ggplot(subset(ggData, actorY=="FARC" & typeX == "Non-selective")) + 
  geom_line(aes(x=x, y = prV, color=typeY, linetype=typeY), size=1.1) + 
  geom_ribbon(aes(x=x, ymin=lo, ymax=hi, fill=typeY),alpha=0.3) +
  theme_bw(11) + ylim(c(0,1)) + 
  ylab("FARC's probability of violence") + 
  xlab("AUC's probability of non-selective violence") + 
  theme(legend.position = "bottom") + 
  labs(color = "", linetype = "", fill="") + 
  scale_color_manual(values=c("#0072B2", "#D55E00")) + 
  scale_fill_manual(values=c("#0072B2", "#D55E00"))

pAS <- ggplot(subset(ggData, actorY=="AUC" & typeX == "Selective")) + 
  geom_line(aes(x=x, y = prV, color=typeY, linetype=typeY), size=1.1) + 
  geom_ribbon(aes(x=x, ymin=lo, ymax=hi, fill=typeY),alpha=0.3) +
  theme_bw(11) + ylim(c(0,1)) + 
  ylab("AUC's probability of violence") + 
  xlab("FARC's probability of selective violence") + 
  theme(legend.position = "bottom") + 
  labs(color = "", linetype = "", fill="") + 
  scale_color_manual(values=c("#0072B2", "#D55E00")) + 
  scale_fill_manual(values=c("#0072B2", "#D55E00"))


pAN <- ggplot(subset(ggData, actorY=="AUC" & typeX == "Non-selective")) + 
  geom_line(aes(x=x, y = prV, color=typeY, linetype=typeY), size=1.1) + 
  geom_ribbon(aes(x=x, ymin=lo, ymax=hi, fill=typeY),alpha=0.3) +
  theme_bw(11) + ylim(c(0,1)) + 
  ylab("AUC's probability of violence") + 
  xlab("FARC's probability of non-selective violence") + 
  theme(legend.position = "bottom") + 
  labs(color = "", linetype = "", fill="") + 
  scale_color_manual(values=c("#0072B2", "#D55E00")) + 
  scale_fill_manual(values=c("#0072B2", "#D55E00"))

pfinal <- ggarrange(pFS, pFN, pAS, pAN, ncol=2, nrow=2, common.legend = TRUE, legend="bottom")

pdf("figure1.pdf", width=8, height=6.5)
pfinal
dev.off()


date()
