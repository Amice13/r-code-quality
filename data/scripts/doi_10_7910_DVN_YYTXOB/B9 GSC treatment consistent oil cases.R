#*****************************************************************************	
# - Change subsample: consistent oil (consistent rents above 500 USD per capita, any onset year) -
#*****************************************************************************

library(gsynth)
library(showtext)
library(ggplot2)
library(haven)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~	
# ---- Subsection: Prepare data and set wd ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## adjust working directory as needed in this and the following setwd("~") commands
setwd("~")
data <- data.frame(read_dta("Eibl, Hertog 2023 replication data.dta"))

data.earlystrongoilall <- subset(data, weak_cs_treat==0 & late_cs_treat ==0 & treated_centre_1900p_tight_no!=1)

data.earlystrongoilallsep <- subset(data, weak_sep_treat==0 & late_sep_treat==0 &  treated_split_1900p_tight_no!=1)

data.nonoilall <- subset(data, treated_centre_1900p!=1)
data.nonoilallsep <- subset(data, treated_split_1900p!=1)

data.alloilall <- subset(data, treated_centre_1900p_tight_no!=1)
data.alloilallsep <- subset(data, treated_split_1900p_tight_no!=1)

data.earlyoilall <- subset(data, treated_centre_1900p_tight_no!=1 & late_cs_treat==0)
data.earlyoilallsep <- subset(data, treated_split_1900p_tight_no!=1 & late_sep_treat==0)

data.strongoilall <- subset(data, treated_centre_1900p_tight_no!=1 & weak_cs_treat==0)
data.strongoilallsep <- subset(data, treated_split_1900p_tight_no!=1& weak_sep_treat==0)


setwd("~")
dir.create("priad")
setwd("~/priad")
dir.create("strong oil")

setwd("~")
dir.create("edu_equ")
setwd("~/edu_equ")
dir.create("strong oil")

setwd("~")
dir.create("health_equ")
setwd("~/health_equ")
dir.create("strong oil")

setwd("~")
dir.create("secenrol")
setwd("~/secenrol")
dir.create("strong oil")


### priad_ipo

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~	
# ---- Subsection: Centre-seeking subversions ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

setwd("~/priad/strong oil")

out <- gsynth(priad_ipo ~ treated_centre_1900p + ln_oilpc + popdensity_UNDP + v2x_polyarchy + civilwar_combined + war_combined + leftistideo + v2x_clphy + lngdppc_vdemplus_ipo + urbanisation_UNDP + hiv_WDI, data = data.strongoilall, index = c("cowcode","year"), 
              force = unit, CV = TRUE, r = c(0, 5), se = TRUE, inference = "parametric", nboots = 1000, na.rm = TRUE, min.T0 = 10, 
              parallel = TRUE, estimator = "ife", EM = TRUE)

p1 <- plot(out, type = "gap", theme.bw = TRUE) 

p1 <- plot(out, type = "gap", theme.bw = TRUE) 
data.plot <- p1$data
data.plot2 <- subset(data.plot, time<=15 & time>=-20)
data.plot2$ATT2 <- c(data.plot2$CI.lower+data.plot2$CI.upper)/2

showtext_auto()
p6 <- ggplot(data=data.plot2 , aes(x=time, y=ATT2, group=1)) +
  geom_line(color="grey40") +
  xlab("Years since subversion") + 
  ylab("Average treatment effect of subversion") +
  geom_ribbon(aes(ymin=data.plot2$CI.lower, ymax=data.plot2$CI.upper), linetype=2, alpha=0.08) +
  geom_vline(xintercept = 0, linetype="dashed", color = "red", size=0.4) +
  geom_hline(yintercept = 0, linetype="dashed", color = "red", size=0.4) +
  expand_limits(y=c(min(data.plot2$ATT2), max(data.plot2$ATT2*1.33))) +
  theme_bw() +
  theme(axis.text=element_text(size=rel(3.1)),
        axis.title=element_text(size=rel(3.1)),
        plot.margin=unit(c(1.2,1.2,1.2,1.2),"cm"),
        panel.border = element_blank(),
        axis.line = element_line(color ="black"),
        axis.title.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 8, r = 0, b = 0, l = 0)),
        axis.text.x = element_text(margin = margin(t = 8, r = 0, b = 0, l = 0)),
        axis.text.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0)),
        text=element_text(family="lat"))

showtext_auto()
pdf("priad_ipo_oc_r5strong.pdf",width=16,height=12)
print(p6)
dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~	
# ---- Subsection: Separatist subversions ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

out <- gsynth(priad_ipo~ treated_split_1900p + ln_oilpc + popdensity_UNDP + v2x_polyarchy + civilwar_combined + war_combined + leftistideo + v2x_clphy + lngdppc_vdemplus_ipo + urbanisation_UNDP + hiv_WDI, data = data.strongoilallsep, index = c("cowcode","year"), 
              force = unit, CV = TRUE, r = c(0, 5), se = TRUE, inference = "parametric", nboots = 1000, na.rm = TRUE, min.T0 = 10, 
              parallel = TRUE, estimator = "ife", EM = TRUE)

p1 <- plot(out, type = "gap", theme.bw = TRUE) 

p1 <- plot(out, type = "gap", theme.bw = TRUE) 
data.plot <- p1$data
data.plot2 <- subset(data.plot, time<=15 & time>=-20)
data.plot2$ATT2 <- c(data.plot2$CI.lower+data.plot2$CI.upper)/2

showtext_auto()
p6 <- ggplot(data=data.plot2 , aes(x=time, y=ATT2, group=1)) +
  geom_line(color="grey40") +
  xlab("Years since subversion") + 
  ylab("Average treatment effect of subversion") +
  geom_ribbon(aes(ymin=data.plot2$CI.lower, ymax=data.plot2$CI.upper), linetype=2, alpha=0.08) +
  geom_vline(xintercept = 0, linetype="dashed", color = "red", size=0.4) +
  geom_hline(yintercept = 0, linetype="dashed", color = "red", size=0.4) +
  expand_limits(y=c(min(data.plot2$ATT2), max(data.plot2$ATT2*1.33))) +
  theme_bw() +
  theme(axis.text=element_text(size=rel(3.1)),
        axis.title=element_text(size=rel(3.1)),
        plot.margin=unit(c(1.2,1.2,1.2,1.2),"cm"),
        panel.border = element_blank(),
        axis.line = element_line(color ="black"),
        axis.title.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 8, r = 0, b = 0, l = 0)),
        axis.text.x = element_text(margin = margin(t = 8, r = 0, b = 0, l = 0)),
        axis.text.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0)),
        text=element_text(family="lat"))

showtext_auto()
pdf("priad_ipo_os_r5strong.pdf",width=16,height=12)
print(p6)
dev.off()



### secenrol_combinedplus

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~	
# ---- Subsection: Centre-seeking subversions ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

setwd("~/secenrol/strong oil")

out <- gsynth(secenrol_combinedplus ~ treated_centre_1900p + ln_oilpc + popdensity_UNDP + v2x_polyarchy + civilwar_combined + war_combined + leftistideo + v2x_clphy + lngdppc_vdemplus_ipo + urbanisation_UNDP + hiv_WDI, data = data.strongoilall, index = c("cowcode","year"), 
              force = unit, CV = TRUE, r = c(0, 5), se = TRUE, inference = "parametric", nboots = 1000, na.rm = TRUE, min.T0 = 10, 
              parallel = TRUE, estimator = "ife", EM = TRUE)

p1 <- plot(out, type = "gap", theme.bw = TRUE) 

p1 <- plot(out, type = "gap", theme.bw = TRUE) 
data.plot <- p1$data
data.plot2 <- subset(data.plot, time<=15 & time>=-20)
data.plot2$ATT2 <- c(data.plot2$CI.lower+data.plot2$CI.upper)/2

showtext_auto()
p6 <- ggplot(data=data.plot2 , aes(x=time, y=ATT2, group=1)) +
  geom_line(color="grey40") +
  xlab("Years since subversion") + 
  ylab("Average treatment effect of subversion") +
  geom_ribbon(aes(ymin=data.plot2$CI.lower, ymax=data.plot2$CI.upper), linetype=2, alpha=0.08) +
  geom_vline(xintercept = 0, linetype="dashed", color = "red", size=0.4) +
  geom_hline(yintercept = 0, linetype="dashed", color = "red", size=0.4) +
  expand_limits(y=c(min(data.plot2$ATT2), max(data.plot2$ATT2*1.33))) +
  theme_bw() +
  theme(axis.text=element_text(size=rel(3.1)),
        axis.title=element_text(size=rel(3.1)),
        plot.margin=unit(c(1.2,1.2,1.2,1.2),"cm"),
        panel.border = element_blank(),
        axis.line = element_line(color ="black"),
        axis.title.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 8, r = 0, b = 0, l = 0)),
        axis.text.x = element_text(margin = margin(t = 8, r = 0, b = 0, l = 0)),
        axis.text.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0)),
        text=element_text(family="lat"))

showtext_auto()
pdf("secenrol_combinedplus_oc_r5strong.pdf",width=16,height=12)
print(p6)
dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~	
# ---- Subsection: Separatist subversions ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

out <- gsynth(secenrol_combinedplus~ treated_split_1900p + ln_oilpc + popdensity_UNDP + v2x_polyarchy + civilwar_combined + war_combined + leftistideo + v2x_clphy + lngdppc_vdemplus_ipo + urbanisation_UNDP + hiv_WDI, data = data.strongoilallsep, index = c("cowcode","year"), 
              force = unit, CV = TRUE, r = c(0, 5), se = TRUE, inference = "parametric", nboots = 1000, na.rm = TRUE, min.T0 = 10, 
              parallel = TRUE, estimator = "ife", EM = TRUE)

p1 <- plot(out, type = "gap", theme.bw = TRUE) 

p1 <- plot(out, type = "gap", theme.bw = TRUE) 
data.plot <- p1$data
data.plot2 <- subset(data.plot, time<=15 & time>=-20)
data.plot2$ATT2 <- c(data.plot2$CI.lower+data.plot2$CI.upper)/2

showtext_auto()
p6 <- ggplot(data=data.plot2 , aes(x=time, y=ATT2, group=1)) +
  geom_line(color="grey40") +
  xlab("Years since subversion") + 
  ylab("Average treatment effect of subversion") +
  geom_ribbon(aes(ymin=data.plot2$CI.lower, ymax=data.plot2$CI.upper), linetype=2, alpha=0.08) +
  geom_vline(xintercept = 0, linetype="dashed", color = "red", size=0.4) +
  geom_hline(yintercept = 0, linetype="dashed", color = "red", size=0.4) +
  expand_limits(y=c(min(data.plot2$ATT2), max(data.plot2$ATT2*1.33))) +
  theme_bw() +
  theme(axis.text=element_text(size=rel(3.1)),
        axis.title=element_text(size=rel(3.1)),
        plot.margin=unit(c(1.2,1.2,1.2,1.2),"cm"),
        panel.border = element_blank(),
        axis.line = element_line(color ="black"),
        axis.title.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 8, r = 0, b = 0, l = 0)),
        axis.text.x = element_text(margin = margin(t = 8, r = 0, b = 0, l = 0)),
        axis.text.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0)),
        text=element_text(family="lat"))

showtext_auto()
pdf("secenrol_combinedplus_os_r5strong.pdf",width=16,height=12)
print(p6)
dev.off()


### v2peedueq_osp

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~	
# ---- Subsection: Centre-seeking subversions ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


setwd("~/edu_equ/strong oil")

out <- gsynth(v2peedueq_osp ~ treated_centre_1900p + ln_oilpc + popdensity_UNDP + v2x_polyarchy + civilwar_combined + war_combined + leftistideo + v2x_clphy + lngdppc_vdemplus_ipo + urbanisation_UNDP + hiv_WDI, data = data.strongoilall, index = c("cowcode","year"), 
              force = unit, CV = TRUE, r = c(0, 5), se = TRUE, inference = "parametric", nboots = 1000, na.rm = TRUE, min.T0 = 10, 
              parallel = TRUE, estimator = "ife", EM = TRUE)

p1 <- plot(out, type = "gap", theme.bw = TRUE) 

p1 <- plot(out, type = "gap", theme.bw = TRUE) 
data.plot <- p1$data
data.plot2 <- subset(data.plot, time<=15 & time>=-20)
data.plot2$ATT2 <- c(data.plot2$CI.lower+data.plot2$CI.upper)/2

showtext_auto()
p6 <- ggplot(data=data.plot2 , aes(x=time, y=ATT2, group=1)) +
  geom_line(color="grey40") +
  xlab("Years since subversion") + 
  ylab("Average treatment effect of subversion") +
  geom_ribbon(aes(ymin=data.plot2$CI.lower, ymax=data.plot2$CI.upper), linetype=2, alpha=0.08) +
  geom_vline(xintercept = 0, linetype="dashed", color = "red", size=0.4) +
  geom_hline(yintercept = 0, linetype="dashed", color = "red", size=0.4) +
  expand_limits(y=c(min(data.plot2$ATT2), max(data.plot2$ATT2*1.33))) +
  theme_bw() +
  theme(axis.text=element_text(size=rel(3.1)),
        axis.title=element_text(size=rel(3.1)),
        plot.margin=unit(c(1.2,1.2,1.2,1.2),"cm"),
        panel.border = element_blank(),
        axis.line = element_line(color ="black"),
        axis.title.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 8, r = 0, b = 0, l = 0)),
        axis.text.x = element_text(margin = margin(t = 8, r = 0, b = 0, l = 0)),
        axis.text.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0)),
        text=element_text(family="lat"))

showtext_auto()
pdf("v2peedueq_osp_oc_r5strong.pdf",width=16,height=12)
print(p6)
dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~	
# ---- Subsection: Separatist subversions ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

out <- gsynth(v2peedueq_osp~ treated_split_1900p + ln_oilpc + popdensity_UNDP + v2x_polyarchy + civilwar_combined + war_combined + leftistideo + v2x_clphy + lngdppc_vdemplus_ipo + urbanisation_UNDP + hiv_WDI, data = data.strongoilallsep, index = c("cowcode","year"), 
              force = unit, CV = TRUE, r = c(0, 5), se = TRUE, inference = "parametric", nboots = 1000, na.rm = TRUE, min.T0 = 10, 
              parallel = TRUE, estimator = "ife", EM = TRUE)

p1 <- plot(out, type = "gap", theme.bw = TRUE) 

p1 <- plot(out, type = "gap", theme.bw = TRUE) 
data.plot <- p1$data
data.plot2 <- subset(data.plot, time<=15 & time>=-20)
data.plot2$ATT2 <- c(data.plot2$CI.lower+data.plot2$CI.upper)/2

showtext_auto()
p6 <- ggplot(data=data.plot2 , aes(x=time, y=ATT2, group=1)) +
  geom_line(color="grey40") +
  xlab("Years since subversion") + 
  ylab("Average treatment effect of subversion") +
  geom_ribbon(aes(ymin=data.plot2$CI.lower, ymax=data.plot2$CI.upper), linetype=2, alpha=0.08) +
  geom_vline(xintercept = 0, linetype="dashed", color = "red", size=0.4) +
  geom_hline(yintercept = 0, linetype="dashed", color = "red", size=0.4) +
  expand_limits(y=c(min(data.plot2$ATT2), max(data.plot2$ATT2*1.33))) +
  theme_bw() +
  theme(axis.text=element_text(size=rel(3.1)),
        axis.title=element_text(size=rel(3.1)),
        plot.margin=unit(c(1.2,1.2,1.2,1.2),"cm"),
        panel.border = element_blank(),
        axis.line = element_line(color ="black"),
        axis.title.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 8, r = 0, b = 0, l = 0)),
        axis.text.x = element_text(margin = margin(t = 8, r = 0, b = 0, l = 0)),
        axis.text.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0)),
        text=element_text(family="lat"))

showtext_auto()
pdf("v2peedueq_osp_os_r5strong.pdf",width=16,height=12)
print(p6)
dev.off()


v2pehealth_osp

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~	
# ---- Subsection: Centre-seeking subversions ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


setwd("~/health_equ/strong oil")

out <- gsynth(v2pehealth_osp ~ treated_centre_1900p + ln_oilpc + popdensity_UNDP + v2x_polyarchy + civilwar_combined + war_combined + leftistideo + v2x_clphy + lngdppc_vdemplus_ipo + urbanisation_UNDP + hiv_WDI, data = data.strongoilall, index = c("cowcode","year"), 
              force = unit, CV = TRUE, r = c(0, 5), se = TRUE, inference = "parametric", nboots = 1000, na.rm = TRUE, min.T0 = 10, 
              parallel = TRUE, estimator = "ife", EM = TRUE)

p1 <- plot(out, type = "gap", theme.bw = TRUE) 

p1 <- plot(out, type = "gap", theme.bw = TRUE) 
data.plot <- p1$data
data.plot2 <- subset(data.plot, time<=15 & time>=-20)
data.plot2$ATT2 <- c(data.plot2$CI.lower+data.plot2$CI.upper)/2

showtext_auto()
p6 <- ggplot(data=data.plot2 , aes(x=time, y=ATT2, group=1)) +
  geom_line(color="grey40") +
  xlab("Years since subversion") + 
  ylab("Average treatment effect of subversion") +
  geom_ribbon(aes(ymin=data.plot2$CI.lower, ymax=data.plot2$CI.upper), linetype=2, alpha=0.08) +
  geom_vline(xintercept = 0, linetype="dashed", color = "red", size=0.4) +
  geom_hline(yintercept = 0, linetype="dashed", color = "red", size=0.4) +
  expand_limits(y=c(min(data.plot2$ATT2), max(data.plot2$ATT2*1.33))) +
  theme_bw() +
  theme(axis.text=element_text(size=rel(3.1)),
        axis.title=element_text(size=rel(3.1)),
        plot.margin=unit(c(1.2,1.2,1.2,1.2),"cm"),
        panel.border = element_blank(),
        axis.line = element_line(color ="black"),
        axis.title.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 8, r = 0, b = 0, l = 0)),
        axis.text.x = element_text(margin = margin(t = 8, r = 0, b = 0, l = 0)),
        axis.text.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0)),
        text=element_text(family="lat"))

showtext_auto()
pdf("v2pehealth_osp_oc_r5strong.pdf",width=16,height=12)
print(p6)
dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~	
# ---- Subsection: Separatist subversions ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

out <- gsynth(v2pehealth_osp~ treated_split_1900p + ln_oilpc + popdensity_UNDP + v2x_polyarchy + civilwar_combined + war_combined + leftistideo + v2x_clphy + lngdppc_vdemplus_ipo + urbanisation_UNDP + hiv_WDI, data = data.strongoilallsep, index = c("cowcode","year"), 
              force = unit, CV = TRUE, r = c(0, 5), se = TRUE, inference = "parametric", nboots = 1000, na.rm = TRUE, min.T0 = 10, 
              parallel = TRUE, estimator = "ife", EM = TRUE)

p1 <- plot(out, type = "gap", theme.bw = TRUE) 

p1 <- plot(out, type = "gap", theme.bw = TRUE) 
data.plot <- p1$data
data.plot2 <- subset(data.plot, time<=15 & time>=-20)
data.plot2$ATT2 <- c(data.plot2$CI.lower+data.plot2$CI.upper)/2

showtext_auto()
p6 <- ggplot(data=data.plot2 , aes(x=time, y=ATT2, group=1)) +
  geom_line(color="grey40") +
  xlab("Years since subversion") + 
  ylab("Average treatment effect of subversion") +
  geom_ribbon(aes(ymin=data.plot2$CI.lower, ymax=data.plot2$CI.upper), linetype=2, alpha=0.08) +
  geom_vline(xintercept = 0, linetype="dashed", color = "red", size=0.4) +
  geom_hline(yintercept = 0, linetype="dashed", color = "red", size=0.4) +
  expand_limits(y=c(min(data.plot2$ATT2), max(data.plot2$ATT2*1.33))) +
  theme_bw() +
  theme(axis.text=element_text(size=rel(3.1)),
        axis.title=element_text(size=rel(3.1)),
        plot.margin=unit(c(1.2,1.2,1.2,1.2),"cm"),
        panel.border = element_blank(),
        axis.line = element_line(color ="black"),
        axis.title.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 8, r = 0, b = 0, l = 0)),
        axis.text.x = element_text(margin = margin(t = 8, r = 0, b = 0, l = 0)),
        axis.text.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0)),
        text=element_text(family="lat"))

showtext_auto()
pdf("v2pehealth_osp_os_r5strong.pdf",width=16,height=12)
print(p6)
dev.off()


