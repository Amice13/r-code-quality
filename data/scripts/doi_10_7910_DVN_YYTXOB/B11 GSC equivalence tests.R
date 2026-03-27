library(gsynth)
library(showtext)
library(ggplot2)
library(haven)

data <- data.frame(read_dta("Eibl, Hertog 2023 replication data.dta"))

data.earlystrongoilall <- subset(data, weak_cs_treat==0 & late_cs_treat ==0 & treated_centre_1900p_tight_no!=1)

data.earlystrongoilallsep <- subset(data, weak_sep_treat==0 & late_sep_treat==0 &  treated_split_1900p_tight_no!=1)

data.nonoilall <- subset(data, treated_centre_1900p!=1)
data.nonoilallsep <- subset(data, treated_split_1900p!=1)

setwd("~")
dir.create("new core")
setwd("~/new core")
dir.create("edu_equ")
dir.create("health_equ")
dir.create("priad_ipo")
dir.create("secenrol")

setwd("~/new core/edu_equ")

# centre-seeking, oil cases
out.gsynth.equiv <- fect (v2peedueq_osp ~ treated_centre_1900p + ln_oilpc + popdensity_UNDP + v2x_polyarchy + civilwar_combined + war_combined + leftistideo + v2x_clphy + lngdppc_vdemplus_ipo + urbanisation_UNDP + hiv_WDI, data = data.earlystrongoilall, index = c("cowcode","year"), 
                          force = "unit", method = "ife", CV = F, r = 5, min.T0 = 10, 
                          preperiod(-10), offperiod(0), equiTest, pre.periods =c(-10,0),
                          se = TRUE, vartype = "parametric", nboots = 1000, na.rm = TRUE,
                          parallel = TRUE)

p1 <- plot(out.gsynth.equiv, type = "equiv", ylim = c(-0.25,0.25), xlim = c(-20,0),
           cex.legend = 1.5, cex.main = 2, main = "Testing Pre-Trend (FEct)", cex.axis = 1.2, cex.lab = 1.5, cex.text = 1.8, stats = c("equiv.p"), ylab = "education equality", xlab = "Time to treatment")
pdf("_csoil_allcontrols_baseline.pdf",width=16,height=12)
print(p1)
dev.off()

# centre-seeking, non-oil cases
out.gsynth.equiv <- fect (v2peedueq_osp ~ treated_centre_1900p_tight_no + ln_oilpc + popdensity_UNDP + v2x_polyarchy + civilwar_combined + war_combined + leftistideo + v2x_clphy + lngdppc_vdemplus_ipo + urbanisation_UNDP + hiv_WDI, data = data.nonoilall, index = c("cowcode","year"), 
                          force = "unit", method = "ife", CV = F, r = 5, min.T0 = 10, 
                          preperiod(-10), offperiod(0), equiTest, pre.periods =c(-10,0),
                          se = TRUE, vartype = "parametric", nboots = 1000, na.rm = TRUE,
                          parallel = TRUE)

p1 <- plot(out.gsynth.equiv, type = "equiv", ylim = c(-0.25,0.25), xlim = c(-20,0),
           cex.legend = 1.5, cex.main = 2, main = "Testing Pre-Trend (FEct)", cex.axis = 1.2, cex.lab = 1.5, cex.text = 1.8, stats = c("equiv.p), ylab = "education equality", xlab = "Time to treatment")
pdf("_csnonoil_allcontrols_baseline.pdf",width=16,height=12)
print(p1)
dev.off()


# separatist, oil cases
out.gsynth.equiv <- fect (v2peedueq_osp ~ treated_split_1900p + ln_oilpc + popdensity_UNDP + v2x_polyarchy + civilwar_combined + war_combined + leftistideo + v2x_clphy + lngdppc_vdemplus_ipo + urbanisation_UNDP + hiv_WDI, data = data.earlystrongoilallsep, index = c("cowcode","year"), 
                          force = "unit", method = "ife", CV = F, r = 5, min.T0 = 10, 
                          preperiod(-10), offperiod(0), equiTest, pre.periods =c(-10,0),
                          se = TRUE, vartype = "parametric", nboots = 1000, na.rm = TRUE,
                          parallel = TRUE)

p1 <- plot(out.gsynth.equiv, type = "equiv", ylim = c(-0.25,0.25), xlim = c(-20,0),
           cex.legend = 1.5, cex.main = 2, main = "Testing Pre-Trend (FEct)", cex.axis = 1.2, cex.lab = 1.5, cex.text = 1.8, stats = c("equiv.p"), ylab = "education equality", xlab = "Time to treatment")
pdf("_sepoil_allcontrols_baseline.pdf",width=16,height=12)
print(p1)
dev.off()


# separatist, non-oil cases
out.gsynth.equiv <- fect (v2peedueq_osp ~ treated_split_1900p_tight_no + ln_oilpc + popdensity_UNDP + v2x_polyarchy + civilwar_combined + war_combined + leftistideo + v2x_clphy + lngdppc_vdemplus_ipo + urbanisation_UNDP + hiv_WDI, data = data.nonoilallsep, index = c("cowcode","year"), 
                          force = "unit", method = "ife", CV = F, r = 5, min.T0 = 10, 
                          preperiod(-10), offperiod(0), equiTest, pre.periods =c(-10,0),
                          se = TRUE, vartype = "parametric", nboots = 1000, na.rm = TRUE,
                          parallel = TRUE)

p1 <- plot(out.gsynth.equiv, type = "equiv", ylim = c(-0.25,0.25), xlim = c(-20,0),
           cex.legend = 1.5, cex.main = 2, main = "Testing Pre-Trend (FEct)", cex.axis = 1.2, cex.lab = 1.5, cex.text = 1.8, stats = c("equiv.p"), ylab = "education equality", xlab = "Time to treatment")
pdf("_sepnonoil_allcontrols_baseline.pdf",width=16,height=12)
print(p1)
dev.off()


##
setwd("~/new core/health_equ")
# centre-seeking, oil cases

out.gsynth.equiv <- fect (v2pehealth_osp ~ treated_centre_1900p + ln_oilpc + popdensity_UNDP + v2x_polyarchy + civilwar_combined + war_combined + leftistideo + v2x_clphy + lngdppc_vdemplus_ipo + urbanisation_UNDP + hiv_WDI, data = data.earlystrongoilall, index = c("cowcode","year"), 
                          force = "unit", method = "ife", CV = F, r = 5, min.T0 = 10, 
                          preperiod(-10), offperiod(0), equiTest, pre.periods =c(-10,0),
                          se = TRUE, vartype = "parametric", nboots = 1000, na.rm = TRUE,
                          parallel = TRUE)


p1 <- plot(out.gsynth.equiv, type = "equiv", ylim = c(-0.25,0.25), xlim = c(-20,0),
           cex.legend = 1.5, cex.main = 2, main = "Testing Pre-Trend (FEct)", cex.axis = 1.2, cex.lab = 1.5, cex.text = 1.8, stats = c("equiv.p"), ylab = "health equality", xlab = "Time to treatment")
pdf("_csoil_allcontrols_baseline.pdf",width=16,height=12)
print(p1)
dev.off()

# centre-seeking, non-oil cases
out.gsynth.equiv <- fect (v2pehealth_osp ~ treated_centre_1900p_tight_no + ln_oilpc + popdensity_UNDP + v2x_polyarchy + civilwar_combined + war_combined + leftistideo + v2x_clphy + lngdppc_vdemplus_ipo + urbanisation_UNDP + hiv_WDI, data = data.nonoilall, index = c("cowcode","year"), 
                          force = "unit", method = "ife", CV = F, r = 5, min.T0 = 10, 
                          preperiod(-10), offperiod(0), equiTest, pre.periods =c(-10,0),
                          se = TRUE, vartype = "parametric", nboots = 1000, na.rm = TRUE,
                          parallel = TRUE)

p1 <- plot(out.gsynth.equiv, type = "equiv", ylim = c(-0.25,0.25), xlim = c(-20,0),
           cex.legend = 1.5, cex.main = 2, main = "Testing Pre-Trend (FEct)", cex.axis = 1.2, cex.lab = 1.5, cex.text = 1.8, stats = c("equiv.p"), ylab = "health equality", xlab = "Time to treatment")
pdf("_csnonoil_allcontrols_baseline.pdf",width=16,height=12)
print(p1)
dev.off()

# separatist, oil cases
out.gsynth.equiv <- fect (v2pehealth_osp ~ treated_split_1900p + ln_oilpc + popdensity_UNDP + v2x_polyarchy + civilwar_combined + war_combined + leftistideo + v2x_clphy + lngdppc_vdemplus_ipo + urbanisation_UNDP + hiv_WDI, data = data.earlystrongoilallsep, index = c("cowcode","year"), 
                          force = "unit", method = "ife", CV = F, r = 4, min.T0 = 10, 
                          preperiod(-10), offperiod(0), equiTest, pre.periods =c(-10,0),
                          se = TRUE, vartype = "parametric", nboots = 1000, na.rm = TRUE,
                          parallel = TRUE)

p1 <- plot(out.gsynth.equiv, type = "equiv", ylim = c(-0.25,0.25), xlim = c(-20,0),
           cex.legend = 1.5, cex.main = 2, main = "Testing Pre-Trend (FEct)", cex.axis = 1.2, cex.lab = 1.5, cex.text = 1.8, stats = c("equiv.p"), ylab = "health equality", xlab = "Time to treatment")
pdf("_sepoil_allcontrols_baseline.pdf",width=16,height=12)
print(p1)
dev.off()

# separatist, non-oil cases
out.gsynth.equiv <- fect (v2pehealth_osp ~ treated_split_1900p_tight_no + ln_oilpc + popdensity_UNDP + v2x_polyarchy + civilwar_combined + war_combined + leftistideo + v2x_clphy + lngdppc_vdemplus_ipo + urbanisation_UNDP + hiv_WDI, data = data.nonoilallsep, index = c("cowcode","year"), 
                          force = "unit", method = "ife", CV = F, r = 4, min.T0 = 10, 
                          preperiod(-10), offperiod(0), equiTest, pre.periods =c(-10,0),
                          se = TRUE, vartype = "parametric", nboots = 1000, na.rm = TRUE,
                          parallel = TRUE)

p1 <- plot(out.gsynth.equiv, type = "equiv", ylim = c(-0.25,0.25), xlim = c(-20,0),
           cex.legend = 1.5, cex.main = 2, main = "Testing Pre-Trend (FEct)", cex.axis = 1.2, cex.lab = 1.5, cex.text = 1.8, stats = c("equiv.p"), ylab = "health equality", xlab = "Time to treatment")
pdf("_sepnonoil_allcontrols_baseline.pdf",width=16,height=12)
print(p1)
dev.off()


##
setwd("~/new core/priad_ipo")

out.gsynth.equiv <- fect (priad_ipo ~ treated_centre_1900p + ln_oilpc + popdensity_UNDP + v2x_polyarchy + civilwar_combined + war_combined + leftistideo + v2x_clphy + lngdppc_vdemplus_ipo + urbanisation_UNDP + hiv_WDI, data = data.earlystrongoilall, index = c("cowcode","year"), 
                          force = "unit", method = "ife", CV = F, r = 5, min.T0 = 10, 
                          preperiod(-10), offperiod(0), equiTest, pre.periods =c(-10,0),
                          se = TRUE, vartype = "parametric", nboots = 1000, na.rm = TRUE,
                          parallel = TRUE)

p1 <- plot(out.gsynth.equiv, type = "equiv", xlim = c(-20,0),
           cex.legend = 1.5, cex.main = 2, main = "Testing Pre-Trend (FEct)", cex.axis = 1.2, cex.lab = 1.5, cex.text = 1.8, stats = c("equiv.p"), ylab = "primary enrolment", xlab = "Time to treatment")
pdf("_csoil_allcontrols_baseline.pdf",width=16,height=12)
print(p1)
dev.off()

# centre-seeking, non-oil cases
out.gsynth.equiv <- fect (priad_ipo ~ treated_centre_1900p_tight_no + ln_oilpc + popdensity_UNDP + v2x_polyarchy + civilwar_combined + war_combined + leftistideo + v2x_clphy + lngdppc_vdemplus_ipo + urbanisation_UNDP + hiv_WDI, data = data.nonoilall, index = c("cowcode","year"), 
                          force = "unit", method = "ife", CV = F, r = 5, min.T0 = 10, 
                          preperiod(-10), offperiod(0), equiTest, pre.periods =c(-10,0),
                          se = TRUE, vartype = "parametric", nboots = 1000, na.rm = TRUE,
                          parallel = TRUE)

p1 <- plot(out.gsynth.equiv, type = "equiv", xlim = c(-20,0),
           cex.legend = 1.5, cex.main = 2, main = "Testing Pre-Trend (FEct)", cex.axis = 1.2, cex.lab = 1.5, cex.text = 1.8, stats = c("equiv.p"), ylab = "
                                                                                                                                       enrolment", xlab = "Time to treatment")
pdf("_csnonoil_allcontrols_baseline.pdf",width=16,height=12)
print(p1)
dev.off()

# separatist, oil cases
out.gsynth.equiv <- fect (priad_ipo ~ treated_split_1900p + ln_oilpc + popdensity_UNDP + v2x_polyarchy + civilwar_combined + war_combined + leftistideo + v2x_clphy + lngdppc_vdemplus_ipo + urbanisation_UNDP + hiv_WDI, data = data.earlystrongoilallsep, index = c("cowcode","year"), 
                          force = "unit", method = "ife", CV = F, r = 5, min.T0 = 10, 
                          preperiod(-10), offperiod(0), equiTest, pre.periods =c(-10,0),
                          se = TRUE, vartype = "parametric", nboots = 1000, na.rm = TRUE,
                          parallel = TRUE)

p1 <- plot(out.gsynth.equiv, type = "equiv", xlim = c(-20,0),
           cex.legend = 1.5, cex.main = 2, main = "Testing Pre-Trend (FEct)", cex.axis = 1.2, cex.lab = 1.5, cex.text = 1.8, stats = c("equiv.p"), ylab = "primary enrolment", xlab = "Time to treatment")
pdf("_sepoil_allcontrols_baseline.pdf",width=16,height=12)
print(p1)
dev.off()

# separatist, non-oil cases
out.gsynth.equiv <- fect (priad_ipo ~ treated_split_1900p_tight_no + ln_oilpc + popdensity_UNDP + v2x_polyarchy + civilwar_combined + war_combined + leftistideo + v2x_clphy + lngdppc_vdemplus_ipo + urbanisation_UNDP + hiv_WDI, data = data.nonoilallsep, index = c("cowcode","year"), 
                          force = "unit", method = "ife", CV = F, r = 5, min.T0 = 10, 
                          preperiod(-10), offperiod(0), equiTest, pre.periods =c(-10,0),
                          se = TRUE, vartype = "parametric", nboots = 1000, na.rm = TRUE,
                          parallel = TRUE)

p1 <- plot(out.gsynth.equiv, type = "equiv", xlim = c(-20,0),
           cex.legend = 1.5, cex.main = 2, main = "Testing Pre-Trend (FEct)", cex.axis = 1.2, cex.lab = 1.5, cex.text = 1.8, stats = c("equiv.p"), ylab = "primary enrolment", xlab = "Time to treatment")
pdf("_sepnonoil_allcontrols_baseline.pdf",width=16,height=12)
print(p1)
dev.off()


##
setwd("~/new core/secenrol")

out.gsynth.equiv <- fect (secenrol_combinedplus ~ treated_centre_1900p + ln_oilpc + popdensity_UNDP + v2x_polyarchy + civilwar_combined + war_combined + leftistideo + v2x_clphy + lngdppc_vdemplus_ipo + urbanisation_UNDP + hiv_WDI, data = data.earlystrongoilall, index = c("cowcode","year"), 
                          force = "unit", method = "ife", CV = F, r = 5, min.T0 = 10, 
                          preperiod(-10), offperiod(0), equiTest, pre.periods =c(-10,0),
                          se = TRUE, vartype = "parametric", nboots = 1000, na.rm = TRUE,
                          parallel = TRUE)

p1 <- plot(out.gsynth.equiv, type = "equiv", xlim = c(-20,0),
           cex.legend = 1.5, cex.main = 2, main = "Testing Pre-Trend (FEct)", cex.axis = 1.2, cex.lab = 1.5, cex.text = 1.8, stats = c("equiv.p"), ylab = "secondary enrolment", xlab = "Time to treatment")
pdf("_csoil_allcontrols_baseline.pdf",width=16,height=12)
print(p1)
dev.off()

# centre-seeking, non-oil cases
out.gsynth.equiv <- fect (secenrol_combinedplus ~ treated_centre_1900p_tight_no + ln_oilpc + popdensity_UNDP + v2x_polyarchy + civilwar_combined + war_combined + leftistideo + v2x_clphy + lngdppc_vdemplus_ipo + urbanisation_UNDP + hiv_WDI, data = data.nonoilall, index = c("cowcode","year"), 
                          force = "unit", method = "ife", CV = F, r = 5, min.T0 = 10, 
                          preperiod(-10), offperiod(0), equiTest, pre.periods =c(-10,0),
                          se = TRUE, vartype = "parametric", nboots = 1000, na.rm = TRUE,
                          parallel = TRUE)

p1 <- plot(out.gsynth.equiv, type = "equiv", xlim = c(-20,0),
           cex.legend = 1.5, cex.main = 2, main = "Testing Pre-Trend (FEct)", cex.axis = 1.2, cex.lab = 1.5, cex.text = 1.8, stats = c("equiv.p"), ylab = "secondary enrolment", xlab = "Time to treatment")
pdf("_csnonoil_allcontrols_baseline.pdf",width=16,height=12)
print(p1)
dev.off()

# separatist, oil cases
out.gsynth.equiv <- fect (secenrol_combinedplus ~ treated_split_1900p + ln_oilpc + popdensity_UNDP + v2x_polyarchy + civilwar_combined + war_combined + leftistideo + v2x_clphy + lngdppc_vdemplus_ipo + urbanisation_UNDP + hiv_WDI, data = data.earlystrongoilallsep, index = c("cowcode","year"), 
                          force = "unit", method = "ife", CV = F, r = 5, min.T0 = 10, 
                          preperiod(-10), offperiod(0), equiTest, pre.periods =c(-10,0),
                          se = TRUE, vartype = "parametric", nboots = 1000, na.rm = TRUE,
                          parallel = TRUE)

p1 <- plot(out.gsynth.equiv, type = "equiv", xlim = c(-20,0),
           cex.legend = 1.5, cex.main = 2, main = "Testing Pre-Trend (FEct)", cex.axis = 1.2, cex.lab = 1.5, cex.text = 1.8, stats = c("equiv.p"), ylab = "secondary enrolment", xlab = "Time to treatment")
pdf("_sepoil_allcontrols_baseline.pdf",width=16,height=12)
print(p1)
dev.off()

# separatist, non-oil cases
out.gsynth.equiv <- fect (secenrol_combinedplus ~ treated_split_1900p_tight_no + ln_oilpc + popdensity_UNDP + v2x_polyarchy + civilwar_combined + war_combined + leftistideo + v2x_clphy + lngdppc_vdemplus_ipo + urbanisation_UNDP + hiv_WDI, data = data.nonoilallsep, index = c("cowcode","year"), 
                          force = "unit", method = "ife", CV = F, r = 5, min.T0 = 10, 
                          preperiod(-10), offperiod(0), equiTest, pre.periods =c(-10,0),
                          se = TRUE, vartype = "parametric", nboots = 1000, na.rm = TRUE,
                          parallel = TRUE)

p1 <- plot(out.gsynth.equiv, type = "equiv",  xlim = c(-20,0),
           cex.legend = 1.5, cex.main = 2, main = "Testing Pre-Trend (FEct)", cex.axis = 1.2, cex.lab = 1.5, cex.text = 1.8, stats = c("equiv.p"), ylab = "secondary enrolment", xlab = "Time to treatment")
pdf("_sepnonoil_allcontrols_baseline.pdf",width=16,height=12)
print(p1)
dev.off()



