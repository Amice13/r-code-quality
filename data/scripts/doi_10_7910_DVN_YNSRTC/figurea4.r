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


#bootV <- read_stata("../../Data/boot_res_elnb.dta")
bootVyear <- read_stata("boot_res_eln_year.dta")
#bootV <- bootV %>% drop_na()
bootVyear <- bootVyear %>% drop_na()
#bootV <- as.matrix(bootV[,55:132])
bootVyear <- as.matrix(bootVyear[,73:150])
Est <- bootVyear[1,]

ggData <- data.frame(prV = Est,
                     x = rep(seq(from=0.2, to=0.8, by = 0.05), 3*2),
                     lo = pmax(Est + qnorm(0.025)*colSds(bootVyear), 0),
                     hi = pmin(Est + qnorm(0.975)*colSds(bootVyear),1),
                     actorY = rep(c("FARC", "AUC", "ELN"), each = 13*2),
                     actorX = c(rep(c("AUC", "ELN"), each = 13),
                                rep(c("FARC", "ELN"), each = 13),
                                rep(c("FARC", "AUC"), each =13)))
ggData$actorY <- factor(ggData$actorY, levels = c("FARC","AUC", "ELN"), ordered=T)
ggData$actorX <- factor(ggData$actorX, levels = c("FARC","AUC", "ELN"), ordered=T)

pfinal <- ggplot(subset(ggData)) + 
          geom_line(aes(x=x, y = prV, color=actorY, linetype=actorY), size=1.1) + 
           geom_ribbon(aes(x=x, ymin=lo, ymax=hi, fill=actorY),alpha=0.3) +
           facet_grid(. ~ actorX) + 
           theme_bw(11) + ylim(c(0,1)) + 
           ylab("Predicted probability of violence") + 
           xlab("Group's (fixed) probability of violence") + 
           theme(legend.position = "bottom") + 
            labs(color = "", linetype = "", fill="") + 
            scale_color_manual(values=c("#0072B2", "#D55E00", "#CC79A7")) + 
            scale_fill_manual(values=c("#0072B2", "#D55E00", "#CC79A7"))

pdf("figurea4.pdf", width=8, height=3.5)
pfinal
dev.off()


date()
