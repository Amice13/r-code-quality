setwd("~/Documents/Research/PDHosp/Replication")
##############################################################
#PROGRAM: WMKM_REStat_nhis_figs.do
#PURPOSE: RD figures from NHIS 1992-1996 files for 
#		  Wherry, Miller, Kaestner, and Meyer
#DATE: 	  January 30, 2017
#NOTES:	  Uses dataset created in WMKM_ReStat_nhis_regs.do
#		  To run, (1) change directory above to a local directory. 
#		  (2) Create subfolder called figures.  
#CONTACT:  Laura Wherry, lwherry@mednet.ucla.edu
###############################################################

library(rdrobust)
###NHIS figures
nhis <- read.csv("data/nhis.csv")
attach(nhis)

#Figure 1(a)
pdf("figures/medicaidall.pdf", width=6.6, height=4.68)
rdbinselect(medicaid, c, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1979 to Sep 1987)", 
y.label="Medicaid Coverage", y.lim=c(0.09,0.25))
dev.off()

#Figure 1(b)
black <- subset(nhis, racer3==1)
pdf("figures/medicaidblacks.pdf", width=6.6, height=4.68)
rdbinselect(black$medicaid, black$c, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1979 to Sep 1987)", 
y.label="Medicaid Coverage", y.lim=c(0.2,0.5))
dev.off()

#Figure 1(c)
nonblack <- subset(nhis, racer3==2)
pdf("figures/medicaidnonblacks.pdf", width=6.6, height=4.68)
rdbinselect(nonblack$medicaid, nonblack$c, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1979 to Sep 1987)", 
y.label="Medicaid Coverage", y.lim=c(0.07,0.20))
dev.off()

#Figure 1(d)
allracespoor <- subset(nhis, poverty==1)
pdf("figures/medicaidpoor.pdf", width=6.6, height=4.68)
rdbinselect(allracespoor$medicaid, allracespoor$c, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1979 to Sep 1987)", 
y.label="Medicaid Coverage", y.lim=c(0.4,0.7))
dev.off()

#Figure 1(e)
allracesnotpoor <- subset(nhis, poverty==0)
pdf("figures/medicaidnotpoor.pdf", width=6.6, height=4.68)
rdbinselect(allracesnotpoor$medicaid, allracesnotpoor$c, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1979 to Sep 1987)", 
y.label="Medicaid Coverage", y.lim=c(0.01,0.10))
dev.off()

#Appendix Figure 3(a)
pdf("figures/covall.pdf", width=6.6, height=4.68)
rdbinselect(coverage, c, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1979 to Sep 1987)", 
y.label="Any Insurance Coverage", y.lim=c(0.65,0.85))
dev.off()

#Appendix Figure 3(b)
pdf("figures/covblacks.pdf", width=6.6, height=4.68)
rdbinselect(black$coverage, black$c, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1979 to Sep 1987)", 
y.label="Any Insurance Coverage", y.lim=c(0.60,0.85))
dev.off()

#Appendix Figure 3(c)
pdf("figures/covnonblacks.pdf", width=6.6, height=4.68)
rdbinselect(nonblack$coverage, nonblack$c, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1979 to Sep 1987)", 
y.label="Any Insurance Coverage", y.lim=c(0.65,0.85))
dev.off()

#Appendix Figure 3(d)
pdf("figures/covallpoor.pdf", width=6.6, height=4.68)
rdbinselect(allracespoor$coverage, allracespoor$c, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1979 to Sep 1987)", 
y.label="Any Insurance Coverage", y.lim=c(0.55,0.85))
dev.off()

#Appendix Figure 3(e)
pdf("figures/covallnotpoor.pdf", width=6.6, height=4.68)
rdbinselect(allracesnotpoor$coverage, allracesnotpoor$c, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1979 to Sep 1987)", 
y.label="Any Insurance Coverage", y.lim=c(0.70,0.90))
dev.off()

#Appendix Figure 4(a)
pdf("figures/docall.pdf", width=6.6, height=4.68)
rdbinselect(doc12, c, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1979 to Sep 1987)", 
y.label="Doctor Visit in Last 12 Mos", y.lim=c(0.65,0.85))
dev.off()

#Appendix Figure 4(b)
pdf("figures/docblacks.pdf", width=6.6, height=4.68)
rdbinselect(black$doc12, black$c, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1979 to Sep 1987)", 
y.label="Doctor Visit in Last 12 Mos", y.lim=c(0.60,0.85))
dev.off()

#Appendix Figure 4(c)
pdf("figures/docnonblacks.pdf", width=6.6, height=4.68)
rdbinselect(nonblack$doc12, nonblack$c, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1979 to Sep 1987)", 
y.label="Doctor Visit in Last 12 Mos", y.lim=c(0.65,0.85))
dev.off()

#Appendix Figure 4(d)
pdf("figures/docallpoor.pdf", width=6.6, height=4.68)
rdbinselect(allracespoor$doc12, allracespoor$c, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1979 to Sep 1987)", 
y.label="Doctor Visit in Last 12 Mos", y.lim=c(0.55,0.85))
dev.off()

#Appendix Figure 4(e)
pdf("figures/docallnotpoor.pdf", width=6.6, height=4.68)
rdbinselect(allracesnotpoor$doc12, allracesnotpoor$c, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1979 to Sep 1987)", 
y.label="Doctor Visit in Last 12 Mos", y.lim=c(0.69,0.85))
dev.off()

#Appendix Figure 5(a)
pdf("figures/hospall.pdf", width=6.6, height=4.68)
rdbinselect(anyhosp12, c, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1979 to Sep 1987)", 
y.label="Hospital Stay in Last 12 Mos", y.lim=c(0.0,0.05))
dev.off()

#Appendix Figure 5(b)
pdf("figures/hospblacks.pdf", width=6.6, height=4.68)
rdbinselect(black$anyhosp12, black$c, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1979 to Sep 1987)", 
y.label="Hospital Stay in Last 12 Mos", y.lim=c(0.0,0.05))
dev.off()

#Appendix Figure 5(c)
pdf("figures/hospnonblacks.pdf", width=6.6, height=4.68)
rdbinselect(nonblack$anyhosp12, nonblack$c, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1979 to Sep 1987)", 
y.label="Hospital Stay in Last 12 Mos", y.lim=c(0.0,0.05))
dev.off()

#Appendix Figure 5(d)
pdf("figures/hospallpoor.pdf", width=6.6, height=4.68)
rdbinselect(allracespoor$anyhosp12, allracespoor$c, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1979 to Sep 1987)", 
y.label="Hospital Stay in Last 12 Mos", y.lim=c(0.0,0.05))
dev.off()

#Appendix Figure 5(e)
pdf("figures/hospallnotpoor.pdf", width=6.6, height=4.68)
rdbinselect(allracesnotpoor$anyhosp12, allracesnotpoor$c, numbinl=24, numbinr=24, p=2, title=" ", x.label="birth month cohort (Oct 1979 to Sep 1987)", 
y.label="Hospital Stay in Last 12 Mos", y.lim=c(0.0,0.05))
dev.off()


