#*****************************************************************************	
# ---------------- Non-leftist centre-seeking subversion --------------
#*****************************************************************************

library(gsynth)
library(showtext)
library(ggplot2)
library(haven)

## adjust working directory as needed in this and the following setwd("~") commands
setwd("~")
data <- data.frame(read_dta("Eibl, Hertog 2023 replication data.dta"))

data.earlystrongoilall_left <- subset(data, weak_cs_treat==0 & late_cs_treat ==0 & treated_centre_1900p_tight_no!=1 & treated_leftist_1900p!=1)
# leaving out oil cases with “leftist” centre-seeking treatment from control group

data.nonoilall_left <- subset(data, treated_centre_1900p!=1 & treated_leftist_1900p_no!=1)
# leaving out non-oil cases with “leftist” centre-seeking treatment from control group

setwd("~")
dir.create("new rob tests")
setwd("~/new rob tests")
dir.create("nonleft")

setwd("~/new rob tests/nonleft")

### priad_ipo

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~	
# ---- Subsection: Centre-seeking subversions ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

out <- gsynth(priad_ipo ~ treated_other_1900p + ln_oilpc + popdensity_UNDP + v2x_polyarchy + civilwar_combined + war_combined + leftistideo + v2x_clphy + lngdppc_vdemplus_ipo + urbanisation_UNDP + hiv_WDI, data = data.earlystrongoilall_left, index = c("cowcode","year"), 
              force = "unit", CV = TRUE, r = c(0, 5), se = TRUE, inference = "parametric", nboots = 1000, na.rm = TRUE, min.T0 = 10, 
              parallel = TRUE, estimator = "ife", EM = TRUE)

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
pdf("priad_ipo_oc.pdf",width=16,height=12)
print(p6)
dev.off()

## Non-oil control cases
out <- gsynth(priad_ipo ~ treated_other_1900p_no + ln_oilpc + popdensity_UNDP + v2x_polyarchy + civilwar_combined + war_combined + leftistideo + v2x_clphy + lngdppc_vdemplus_ipo + urbanisation_UNDP + hiv_WDI, data = data.nonoilall_left, index = c("cowcode","year"), 
              force = "unit", CV = TRUE, r = c(0, 5), se = TRUE, inference = "parametric", nboots = 1000, na.rm = TRUE, min.T0 = 10, 
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
pdf("priad_ipo_c.pdf",width=16,height=12)
print(p6)
dev.off()


### secenrol

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~	
# ---- Subsection: Centre-seeking subversions ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

out <- gsynth(secenrol_combinedplus ~ treated_other_1900p + ln_oilpc + popdensity_UNDP + v2x_polyarchy + civilwar_combined + war_combined + leftistideo + v2x_clphy + lngdppc_vdemplus_ipo + urbanisation_UNDP + hiv_WDI, data = data.earlystrongoilall_left, index = c("cowcode","year"), 
              force = "unit", CV = TRUE, r = c(0, 5), se = TRUE, inference = "parametric", nboots = 1000, na.rm = TRUE, min.T0 = 10, 
              parallel = TRUE, estimator = "ife", EM = TRUE)

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
pdf("secenrol_cplus_oc.pdf",width=16,height=12)
print(p6)
dev.off()

## Non-oil control cases
out <- gsynth(secenrol_combinedplus ~ treated_other_1900p_no + ln_oilpc + popdensity_UNDP + v2x_polyarchy + civilwar_combined + war_combined + leftistideo + v2x_clphy + lngdppc_vdemplus_ipo + urbanisation_UNDP + hiv_WDI, data = data.nonoilall_left, index = c("cowcode","year"), 
              force = "unit", CV = TRUE, r = c(0, 5), se = TRUE, inference = "parametric", nboots = 1000, na.rm = TRUE, min.T0 = 10, 
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
pdf("secenrol_cplus_c.pdf",width=16,height=12)
print(p6)
dev.off()


### v2peedueq_osp

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~	
# ---- Subsection: Centre-seeking subversions ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

out <- gsynth(v2peedueq_osp ~ treated_other_1900p + ln_oilpc + popdensity_UNDP + v2x_polyarchy + civilwar_combined + war_combined + leftistideo + v2x_clphy + lngdppc_vdemplus_ipo + urbanisation_UNDP + hiv_WDI, data = data.earlystrongoilall_left, index = c("cowcode","year"), 
              force = "unit", CV = TRUE, r = c(0, 5), se = TRUE, inference = "parametric", nboots = 1000, na.rm = TRUE, min.T0 = 10, 
              parallel = TRUE, estimator = "ife", EM = TRUE)

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
pdf("v2peedueq_osp_oc.pdf",width=16,height=12)
print(p6)
dev.off()

## Non-oil control cases
out <- gsynth(v2peedueq_osp ~ treated_other_1900p_no + ln_oilpc + popdensity_UNDP + v2x_polyarchy + civilwar_combined + war_combined + leftistideo + v2x_clphy + lngdppc_vdemplus_ipo + urbanisation_UNDP + hiv_WDI, data = data.nonoilall_left, index = c("cowcode","year"), 
              force = "unit", CV = TRUE, r = c(0, 5), se = TRUE, inference = "parametric", nboots = 1000, na.rm = TRUE, min.T0 = 10, 
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
pdf("v2peedueq_osp_c.pdf",width=16,height=12)
print(p6)
dev.off()


### v2pehealth_osp

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~	
# ---- Subsection: Centre-seeking subversions ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

out <- gsynth(v2pehealth_osp ~ treated_other_1900p + ln_oilpc + popdensity_UNDP + v2x_polyarchy + civilwar_combined + war_combined + leftistideo + v2x_clphy + lngdppc_vdemplus_ipo + urbanisation_UNDP + hiv_WDI, data = data.earlystrongoilall_left, index = c("cowcode","year"), 
              force = "unit", CV = TRUE, r = c(0, 5), se = TRUE, inference = "parametric", nboots = 1000, na.rm = TRUE, min.T0 = 10, 
              parallel = TRUE, estimator = "ife", EM = TRUE)

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
pdf("v2pehealth_osp_oc.pdf",width=16,height=12)
print(p6)
dev.off()

## Non-oil control cases
out <- gsynth(v2pehealth_osp ~ treated_other_1900p_no + ln_oilpc + popdensity_UNDP + v2x_polyarchy + civilwar_combined + war_combined + leftistideo + v2x_clphy + lngdppc_vdemplus_ipo + urbanisation_UNDP + hiv_WDI, data = data.nonoilall_left, index = c("cowcode","year"), 
              force = "unit", CV = TRUE, r = c(0, 5), se = TRUE, inference = "parametric", nboots = 1000, na.rm = TRUE, min.T0 = 10, 
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
pdf("v2pehealth_osp_c.pdf",width=16,height=12)
print(p6)
dev.off()



