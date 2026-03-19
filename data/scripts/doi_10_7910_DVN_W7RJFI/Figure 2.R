rm(list=ls())

library(plotrix)
theme_pattern <- theme(axis.text=element_text(size=12),
                       axis.title.x=element_blank(),
                       legend.text=element_text(size=12),
                       legend.title=element_blank(),
                       legend.key = element_blank(),
                       #legend.text=element_text(size = rel(1)),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.border = element_blank(),
                       axis.line = element_line(color = 'black'),
                       axis.line.y = element_line(color="black"),
                       axis.line.x = element_line(color="black"),
                       legend.background = element_rect(fill=alpha('NA', 0.2)))

#Figure 2b

#cas.total
cas.mutation_rate <- read.table(file="Attached_doc/cas.mutation_rate.txt",sep="\t")

s1 <- data.frame(mutation=cas.mutation_rate[ ,1], rate=cas.mutation_rate[ ,2], type="WT", sample=1, class="cas")
s2 <- data.frame(mutation=cas.mutation_rate[ ,1], rate=cas.mutation_rate[ ,3], type="WT", sample=2, class="cas")
s3 <- data.frame(mutation=cas.mutation_rate[ ,1], rate=cas.mutation_rate[ ,4], type="WT", sample=3, class="cas")
s4 <- data.frame(mutation=cas.mutation_rate[ ,1], rate=cas.mutation_rate[ ,5], type="WT", sample=4, class="cas")
s5 <- data.frame(mutation=cas.mutation_rate[ ,1], rate=cas.mutation_rate[ ,6], type="WT", sample=5, class="cas")
s6 <- data.frame(mutation=cas.mutation_rate[ ,1], rate=cas.mutation_rate[ ,7], type="WT", sample=6, class="cas")
s8 <- data.frame(mutation=cas.mutation_rate[ ,1], rate=cas.mutation_rate[ ,8], type="WT", sample=8, class="cas")

s9 <- data.frame(mutation=cas.mutation_rate[ ,1], rate=cas.mutation_rate[ ,9], type="4SU", sample=9, class="cas")
s10 <- data.frame(mutation=cas.mutation_rate[ ,1], rate=cas.mutation_rate[ ,10], type="4SU", sample=10, class="cas")
s11 <- data.frame(mutation=cas.mutation_rate[ ,1], rate=cas.mutation_rate[ ,11], type="4SU", sample=11, class="cas")
s12 <- data.frame(mutation=cas.mutation_rate[ ,1], rate=cas.mutation_rate[ ,12], type="4SU", sample=12, class="cas")
s13 <- data.frame(mutation=cas.mutation_rate[ ,1], rate=cas.mutation_rate[ ,13], type="4SU", sample=13, class="cas")
s15 <- data.frame(mutation=cas.mutation_rate[ ,1], rate=cas.mutation_rate[ ,14], type="4SU", sample=15, class="cas")
s16 <- data.frame(mutation=cas.mutation_rate[ ,1], rate=cas.mutation_rate[ ,15], type="4SU", sample=16, class="cas")
s17 <- data.frame(mutation=cas.mutation_rate[ ,1], rate=cas.mutation_rate[ ,16], type="4SU", sample=17, class="cas")
s18 <- data.frame(mutation=cas.mutation_rate[ ,1], rate=cas.mutation_rate[ ,17], type="4SU", sample=18, class="cas")
s19 <- data.frame(mutation=cas.mutation_rate[ ,1], rate=cas.mutation_rate[ ,18], type="4SU", sample=19, class="cas")
s20 <- data.frame(mutation=cas.mutation_rate[ ,1], rate=cas.mutation_rate[ ,19], type="4SU", sample=20, class="cas")
s21 <- data.frame(mutation=cas.mutation_rate[ ,1], rate=cas.mutation_rate[ ,20], type="4SU", sample=21, class="cas")

cas.dat <- rbind(s1,s2,s3,s4,s5,s6,s8,s9,s10,s11,s12,s13,s15,s16,s17,s18,s19,s20,s21)
rm(s1,s2,s3,s4,s5,s6,s8,s9,s10,s11,s12,s13,s15,s16,s17,s18,s19,s20,s21)


#ref.total
ref.mutation_rate <- read.table(file="Attached_doc/ref.mutation_rate.txt",sep="\t")

s1 <- data.frame(mutation=ref.mutation_rate[ ,1], rate=ref.mutation_rate[ ,2], type="WT", sample=1, class="ref")
s2 <- data.frame(mutation=ref.mutation_rate[ ,1], rate=ref.mutation_rate[ ,3], type="WT", sample=2, class="ref")
s3 <- data.frame(mutation=ref.mutation_rate[ ,1], rate=ref.mutation_rate[ ,4], type="WT", sample=3, class="ref")
s4 <- data.frame(mutation=ref.mutation_rate[ ,1], rate=ref.mutation_rate[ ,5], type="WT", sample=4, class="ref")
s5 <- data.frame(mutation=ref.mutation_rate[ ,1], rate=ref.mutation_rate[ ,6], type="WT", sample=5, class="ref")
s6 <- data.frame(mutation=ref.mutation_rate[ ,1], rate=ref.mutation_rate[ ,7], type="WT", sample=6, class="ref")
s8 <- data.frame(mutation=ref.mutation_rate[ ,1], rate=ref.mutation_rate[ ,8], type="WT", sample=8, class="ref")

s9 <- data.frame(mutation=ref.mutation_rate[ ,1], rate=ref.mutation_rate[ ,9], type="4SU", sample=9, class="ref")
s10 <- data.frame(mutation=ref.mutation_rate[ ,1], rate=ref.mutation_rate[ ,10], type="4SU", sample=10, class="ref")
s11 <- data.frame(mutation=ref.mutation_rate[ ,1], rate=ref.mutation_rate[ ,11], type="4SU", sample=11, class="ref")
s12 <- data.frame(mutation=ref.mutation_rate[ ,1], rate=ref.mutation_rate[ ,12], type="4SU", sample=12, class="ref")
s13 <- data.frame(mutation=ref.mutation_rate[ ,1], rate=ref.mutation_rate[ ,13], type="4SU", sample=13, class="ref")
s15 <- data.frame(mutation=ref.mutation_rate[ ,1], rate=ref.mutation_rate[ ,14], type="4SU", sample=15, class="ref")
s16 <- data.frame(mutation=ref.mutation_rate[ ,1], rate=ref.mutation_rate[ ,15], type="4SU", sample=16, class="ref")
s17 <- data.frame(mutation=ref.mutation_rate[ ,1], rate=ref.mutation_rate[ ,16], type="4SU", sample=17, class="ref")
s18 <- data.frame(mutation=ref.mutation_rate[ ,1], rate=ref.mutation_rate[ ,17], type="4SU", sample=18, class="ref")
s19 <- data.frame(mutation=ref.mutation_rate[ ,1], rate=ref.mutation_rate[ ,18], type="4SU", sample=19, class="ref")
s20 <- data.frame(mutation=ref.mutation_rate[ ,1], rate=ref.mutation_rate[ ,19], type="4SU", sample=20, class="ref")
s21 <- data.frame(mutation=ref.mutation_rate[ ,1], rate=ref.mutation_rate[ ,20], type="4SU", sample=21, class="ref")

ref.dat <- rbind(s1,s2,s3,s4,s5,s6,s8,s9,s10,s11,s12,s13,s15,s16,s17,s18,s19,s20,s21)
rm(s1,s2,s3,s4,s5,s6,s8,s9,s10,s11,s12,s13,s15,s16,s17,s18,s19,s20,s21)


#neu.total
neu.mutation_rate <- read.table(file="Attached_doc/neu.mutation_rate.txt",sep="\t")

s1 <- data.frame(mutation=neu.mutation_rate[ ,1], rate=neu.mutation_rate[ ,2], type="WT", sample=1, class="neu")
s2 <- data.frame(mutation=neu.mutation_rate[ ,1], rate=neu.mutation_rate[ ,3], type="WT", sample=2, class="neu")
s3 <- data.frame(mutation=neu.mutation_rate[ ,1], rate=neu.mutation_rate[ ,4], type="WT", sample=3, class="neu")
s4 <- data.frame(mutation=neu.mutation_rate[ ,1], rate=neu.mutation_rate[ ,5], type="WT", sample=4, class="neu")
s5 <- data.frame(mutation=neu.mutation_rate[ ,1], rate=neu.mutation_rate[ ,6], type="WT", sample=5, class="neu")
s6 <- data.frame(mutation=neu.mutation_rate[ ,1], rate=neu.mutation_rate[ ,7], type="WT", sample=6, class="neu")
s8 <- data.frame(mutation=neu.mutation_rate[ ,1], rate=neu.mutation_rate[ ,8], type="WT", sample=8, class="neu")

s9 <- data.frame(mutation=neu.mutation_rate[ ,1], rate=neu.mutation_rate[ ,9], type="4SU", sample=9, class="neu")
s10 <- data.frame(mutation=neu.mutation_rate[ ,1], rate=neu.mutation_rate[ ,10], type="4SU", sample=10, class="neu")
s11 <- data.frame(mutation=neu.mutation_rate[ ,1], rate=neu.mutation_rate[ ,11], type="4SU", sample=11, class="neu")
s12 <- data.frame(mutation=neu.mutation_rate[ ,1], rate=neu.mutation_rate[ ,12], type="4SU", sample=12, class="neu")
s13 <- data.frame(mutation=neu.mutation_rate[ ,1], rate=neu.mutation_rate[ ,13], type="4SU", sample=13, class="neu")
s15 <- data.frame(mutation=neu.mutation_rate[ ,1], rate=neu.mutation_rate[ ,14], type="4SU", sample=15, class="neu")
s16 <- data.frame(mutation=neu.mutation_rate[ ,1], rate=neu.mutation_rate[ ,15], type="4SU", sample=16, class="neu")
s17 <- data.frame(mutation=neu.mutation_rate[ ,1], rate=neu.mutation_rate[ ,16], type="4SU", sample=17, class="neu")
s18 <- data.frame(mutation=neu.mutation_rate[ ,1], rate=neu.mutation_rate[ ,17], type="4SU", sample=18, class="neu")
s19 <- data.frame(mutation=neu.mutation_rate[ ,1], rate=neu.mutation_rate[ ,18], type="4SU", sample=19, class="neu")
s20 <- data.frame(mutation=neu.mutation_rate[ ,1], rate=neu.mutation_rate[ ,19], type="4SU", sample=20, class="neu")
s21 <- data.frame(mutation=neu.mutation_rate[ ,1], rate=neu.mutation_rate[ ,20], type="4SU", sample=21, class="neu")

neu.dat <- rbind(s1,s2,s3,s4,s5,s6,s8,s9,s10,s11,s12,s13,s15,s16,s17,s18,s19,s20,s21)
rm(s1,s2,s3,s4,s5,s6,s8,s9,s10,s11,s12,s13,s15,s16,s17,s18,s19,s20,s21)



#make a bar plot to compare mutation rate in WT(no 4SU) or 4SU, for better comparison.
dat <- rbind(cas.dat, ref.dat, neu.dat)


#dat$rate <- as.numeric(dat$rate)

#WT sample
WT.dat <- dat[which(dat$type=="WT"), ]

TA.cas.value <- WT.dat[which(WT.dat$mutation=="TA" & WT.dat$class=="cas"), 2]
TA.WT.cas.dat <- data.frame(rateMeans=mean(TA.cas.value), mutation="TA", SE=std.error(TA.cas.value), class="cas")

TC.cas.value <- WT.dat[which(WT.dat$mutation=="TC" & WT.dat$class=="cas"), 2]
TC.WT.cas.dat <- data.frame(rateMeans=mean(TC.cas.value), mutation="TC", SE=std.error(TC.cas.value), class="cas")

TG.cas.value <- WT.dat[which(WT.dat$mutation=="TG" & WT.dat$class=="cas"), 2]
TG.WT.cas.dat <- data.frame(rateMeans=mean(TG.cas.value), mutation="TG", SE=std.error(TG.cas.value), class="cas")

AT.cas.value <- WT.dat[which(WT.dat$mutation=="AT" & WT.dat$class=="cas"), 2]
AT.WT.cas.dat <- data.frame(rateMeans=mean(AT.cas.value), mutation="AT", SE=std.error(AT.cas.value), class="cas")

AC.cas.value <- WT.dat[which(WT.dat$mutation=="AC" & WT.dat$class=="cas"), 2]
AC.WT.cas.dat <- data.frame(rateMeans=mean(AC.cas.value), mutation="AC", SE=std.error(AC.cas.value), class="cas")

AG.cas.value <- WT.dat[which(WT.dat$mutation=="AG" & WT.dat$class=="cas"), 2]
AG.WT.cas.dat <- data.frame(rateMeans=mean(AG.cas.value), mutation="AG", SE=std.error(AG.cas.value), class="cas")

CA.cas.value <- WT.dat[which(WT.dat$mutation=="CA" & WT.dat$class=="cas"), 2]
CA.WT.cas.dat <- data.frame(rateMeans=mean(CA.cas.value), mutation="CA", SE=std.error(CA.cas.value), class="cas")

CT.cas.value <- WT.dat[which(WT.dat$mutation=="CT" & WT.dat$class=="cas"), 2]
CT.WT.cas.dat <- data.frame(rateMeans=mean(CT.cas.value), mutation="CT", SE=std.error(CT.cas.value), class="cas")

CG.cas.value <- WT.dat[which(WT.dat$mutation=="CG" & WT.dat$class=="cas"), 2]
CG.WT.cas.dat <- data.frame(rateMeans=mean(CG.cas.value), mutation="CG", SE=std.error(CG.cas.value), class="cas")

GA.cas.value <- WT.dat[which(WT.dat$mutation=="GA" & WT.dat$class=="cas"), 2]
GA.WT.cas.dat <- data.frame(rateMeans=mean(GA.cas.value), mutation="GA", SE=std.error(GA.cas.value), class="cas")

GT.cas.value <- WT.dat[which(WT.dat$mutation=="GT" & WT.dat$class=="cas"), 2]
GT.WT.cas.dat <- data.frame(rateMeans=mean(GT.cas.value), mutation="GT", SE=std.error(GT.cas.value), class="cas")

GC.cas.value <- WT.dat[which(WT.dat$mutation=="GC" & WT.dat$class=="cas"), 2]
GC.WT.cas.dat <- data.frame(rateMeans=mean(GC.cas.value), mutation="GC", SE=std.error(GC.cas.value), class="cas")
#-------
TA.ref.value <- WT.dat[which(WT.dat$mutation=="TA" & WT.dat$class=="ref"), 2]
TA.WT.ref.dat <- data.frame(rateMeans=mean(TA.ref.value), mutation="TA", SE=std.error(TA.ref.value), class="ref")

TC.ref.value <- WT.dat[which(WT.dat$mutation=="TC" & WT.dat$class=="ref"), 2]
TC.WT.ref.dat <- data.frame(rateMeans=mean(TC.ref.value), mutation="TC", SE=std.error(TC.ref.value), class="ref")

TG.ref.value <- WT.dat[which(WT.dat$mutation=="TG" & WT.dat$class=="ref"), 2]
TG.WT.ref.dat <- data.frame(rateMeans=mean(TG.ref.value), mutation="TG", SE=std.error(TG.ref.value), class="ref")

AT.ref.value <- WT.dat[which(WT.dat$mutation=="AT" & WT.dat$class=="ref"), 2]
AT.WT.ref.dat <- data.frame(rateMeans=mean(AT.ref.value), mutation="AT", SE=std.error(AT.ref.value), class="ref")

AC.ref.value <- WT.dat[which(WT.dat$mutation=="AC" & WT.dat$class=="ref"), 2]
AC.WT.ref.dat <- data.frame(rateMeans=mean(AC.ref.value), mutation="AC", SE=std.error(AC.ref.value), class="ref")

AG.ref.value <- WT.dat[which(WT.dat$mutation=="AG" & WT.dat$class=="ref"), 2]
AG.WT.ref.dat <- data.frame(rateMeans=mean(AG.ref.value), mutation="AG", SE=std.error(AG.ref.value), class="ref")

CT.ref.value <- WT.dat[which(WT.dat$mutation=="CT" & WT.dat$class=="ref"), 2]
CT.WT.ref.dat <- data.frame(rateMeans=mean(CT.ref.value), mutation="CT", SE=std.error(CT.ref.value), class="ref")

CA.ref.value <- WT.dat[which(WT.dat$mutation=="CA" & WT.dat$class=="ref"), 2]
CA.WT.ref.dat <- data.frame(rateMeans=mean(CA.ref.value), mutation="CA", SE=std.error(CA.ref.value), class="ref")

CG.ref.value <- WT.dat[which(WT.dat$mutation=="CG" & WT.dat$class=="ref"), 2]
CG.WT.ref.dat <- data.frame(rateMeans=mean(CG.ref.value), mutation="CG", SE=std.error(CG.ref.value), class="ref")

GA.ref.value <- WT.dat[which(WT.dat$mutation=="GA" & WT.dat$class=="ref"), 2]
GA.WT.ref.dat <- data.frame(rateMeans=mean(GA.ref.value), mutation="GA", SE=std.error(GA.ref.value), class="ref")

GT.ref.value <- WT.dat[which(WT.dat$mutation=="GT" & WT.dat$class=="ref"), 2]
GT.WT.ref.dat <- data.frame(rateMeans=mean(GT.ref.value), mutation="GT", SE=std.error(GT.ref.value), class="ref")

GC.ref.value <- WT.dat[which(WT.dat$mutation=="GC" & WT.dat$class=="ref"), 2]
GC.WT.ref.dat <- data.frame(rateMeans=mean(GC.ref.value), mutation="GC", SE=std.error(GC.ref.value), class="ref")

#-------
TA.neu.value <- WT.dat[which(WT.dat$mutation=="TA" & WT.dat$class=="neu"), 2]
TA.WT.neu.dat <- data.frame(rateMeans=mean(TA.neu.value), mutation="TA", SE=std.error(TA.neu.value), class="neu")

TC.neu.value <- WT.dat[which(WT.dat$mutation=="TC" & WT.dat$class=="neu"), 2]
TC.WT.neu.dat <- data.frame(rateMeans=mean(TC.neu.value), mutation="TC", SE=std.error(TC.neu.value), class="neu")

TG.neu.value <- WT.dat[which(WT.dat$mutation=="TG" & WT.dat$class=="neu"), 2]
TG.WT.neu.dat <- data.frame(rateMeans=mean(TG.neu.value), mutation="TG", SE=std.error(TG.neu.value), class="neu")

AT.neu.value <- WT.dat[which(WT.dat$mutation=="AT" & WT.dat$class=="neu"), 2]
AT.WT.neu.dat <- data.frame(rateMeans=mean(AT.neu.value), mutation="AT", SE=std.error(AT.neu.value), class="neu")

AC.neu.value <- WT.dat[which(WT.dat$mutation=="AC" & WT.dat$class=="neu"), 2]
AC.WT.neu.dat <- data.frame(rateMeans=mean(AC.neu.value), mutation="AC", SE=std.error(AC.neu.value), class="neu")

AG.neu.value <- WT.dat[which(WT.dat$mutation=="AG" & WT.dat$class=="neu"), 2]
AG.WT.neu.dat <- data.frame(rateMeans=mean(AG.neu.value), mutation="AG", SE=std.error(AG.neu.value), class="neu")

CT.neu.value <- WT.dat[which(WT.dat$mutation=="CT" & WT.dat$class=="neu"), 2]
CT.WT.neu.dat <- data.frame(rateMeans=mean(CT.neu.value), mutation="CT", SE=std.error(CT.neu.value), class="neu")

CA.neu.value <- WT.dat[which(WT.dat$mutation=="CA" & WT.dat$class=="neu"), 2]
CA.WT.neu.dat <- data.frame(rateMeans=mean(CA.neu.value), mutation="CA", SE=std.error(CA.neu.value), class="neu")

CG.neu.value <- WT.dat[which(WT.dat$mutation=="CG" & WT.dat$class=="neu"), 2]
CG.WT.neu.dat <- data.frame(rateMeans=mean(CG.neu.value), mutation="CG", SE=std.error(CG.neu.value), class="neu")

GA.neu.value <- WT.dat[which(WT.dat$mutation=="GA" & WT.dat$class=="neu"), 2]
GA.WT.neu.dat <- data.frame(rateMeans=mean(GA.neu.value), mutation="GA", SE=std.error(GA.neu.value), class="neu")

GT.neu.value <- WT.dat[which(WT.dat$mutation=="GT" & WT.dat$class=="neu"), 2]
GT.WT.neu.dat <- data.frame(rateMeans=mean(GT.neu.value), mutation="GT", SE=std.error(GT.neu.value), class="neu")

GC.neu.value <- WT.dat[which(WT.dat$mutation=="GC" & WT.dat$class=="neu"), 2]
GC.WT.neu.dat <- data.frame(rateMeans=mean(GC.neu.value), mutation="GC", SE=std.error(GC.neu.value), class="neu")

plot.dat <- rbind(TA.WT.cas.dat,
             TC.WT.cas.dat,
             TG.WT.cas.dat,
             AT.WT.cas.dat,
             AC.WT.cas.dat,
             AG.WT.cas.dat,
             CA.WT.cas.dat,
             CT.WT.cas.dat,
             CG.WT.cas.dat,
             GA.WT.cas.dat,
             GT.WT.cas.dat,
             GC.WT.cas.dat,
             TA.WT.ref.dat,
             TC.WT.ref.dat,
             TG.WT.ref.dat,
             AT.WT.ref.dat,
             AC.WT.ref.dat,
             AG.WT.ref.dat,
             CA.WT.ref.dat,
             CT.WT.ref.dat,
             CG.WT.ref.dat,
             GA.WT.ref.dat,
             GT.WT.ref.dat,
             GC.WT.ref.dat,
             TA.WT.neu.dat,
             TC.WT.neu.dat,
             TG.WT.neu.dat,
             AT.WT.neu.dat,
             AC.WT.neu.dat,
             AG.WT.neu.dat,
             CA.WT.neu.dat,
             CT.WT.neu.dat,
             CG.WT.neu.dat,
             GA.WT.neu.dat,
             GT.WT.neu.dat,
             GC.WT.neu.dat)

ggplot(plot.dat, aes(x=mutation, y=rateMeans, fill=class))+
  geom_bar(stat='identity', position=position_dodge(), alpha=0.7) +
  geom_errorbar(aes(ymax = rateMeans + SE, ymin = rateMeans - SE), position = position_dodge(0.9), width = 0.2)+
  scale_fill_manual(values=c("coral3", "cornflowerblue","khaki")) +
  scale_y_continuous(limits = c(0,0.27),breaks = c(0,0.05,0.1,0.15,0.2,0.25))+
  theme_bw() +
  theme_pattern 




#----4SU sample
N4SU.dat <- dat[which(dat$type=="4SU"), ]

TA.cas.value <- N4SU.dat[which(N4SU.dat$mutation=="TA" & N4SU.dat$class=="cas"), 2]
TA.N4SU.cas.dat <- data.frame(rateMeans=mean(TA.cas.value), mutation="TA", SE=std.error(TA.cas.value), class="cas")

TC.cas.value <- N4SU.dat[which(N4SU.dat$mutation=="TC" & N4SU.dat$class=="cas"), 2]
TC.N4SU.cas.dat <- data.frame(rateMeans=mean(TC.cas.value), mutation="TC", SE=std.error(TC.cas.value), class="cas")

TG.cas.value <- N4SU.dat[which(N4SU.dat$mutation=="TG" & N4SU.dat$class=="cas"), 2]
TG.N4SU.cas.dat <- data.frame(rateMeans=mean(TG.cas.value), mutation="TG", SE=std.error(TG.cas.value), class="cas")

AT.cas.value <- N4SU.dat[which(N4SU.dat$mutation=="AT" & N4SU.dat$class=="cas"), 2]
AT.N4SU.cas.dat <- data.frame(rateMeans=mean(AT.cas.value), mutation="AT", SE=std.error(AT.cas.value), class="cas")

AC.cas.value <- N4SU.dat[which(N4SU.dat$mutation=="AC" & N4SU.dat$class=="cas"), 2]
AC.N4SU.cas.dat <- data.frame(rateMeans=mean(AC.cas.value), mutation="AC", SE=std.error(AC.cas.value), class="cas")

AG.cas.value <- N4SU.dat[which(N4SU.dat$mutation=="AG" & N4SU.dat$class=="cas"), 2]
AG.N4SU.cas.dat <- data.frame(rateMeans=mean(AG.cas.value), mutation="AG", SE=std.error(AG.cas.value), class="cas")

CA.cas.value <- N4SU.dat[which(N4SU.dat$mutation=="CA" & N4SU.dat$class=="cas"), 2]
CA.N4SU.cas.dat <- data.frame(rateMeans=mean(CA.cas.value), mutation="CA", SE=std.error(CA.cas.value), class="cas")

CT.cas.value <- N4SU.dat[which(N4SU.dat$mutation=="CT" & N4SU.dat$class=="cas"), 2]
CT.N4SU.cas.dat <- data.frame(rateMeans=mean(CT.cas.value), mutation="CT", SE=std.error(CT.cas.value), class="cas")

CG.cas.value <- N4SU.dat[which(N4SU.dat$mutation=="CG" & N4SU.dat$class=="cas"), 2]
CG.N4SU.cas.dat <- data.frame(rateMeans=mean(CG.cas.value), mutation="CG", SE=std.error(CG.cas.value), class="cas")

GA.cas.value <- N4SU.dat[which(N4SU.dat$mutation=="GA" & N4SU.dat$class=="cas"), 2]
GA.N4SU.cas.dat <- data.frame(rateMeans=mean(GA.cas.value), mutation="GA", SE=std.error(GA.cas.value), class="cas")

GT.cas.value <- N4SU.dat[which(N4SU.dat$mutation=="GT" & N4SU.dat$class=="cas"), 2]
GT.N4SU.cas.dat <- data.frame(rateMeans=mean(GT.cas.value), mutation="GT", SE=std.error(GT.cas.value), class="cas")

GC.cas.value <- N4SU.dat[which(N4SU.dat$mutation=="GC" & N4SU.dat$class=="cas"), 2]
GC.N4SU.cas.dat <- data.frame(rateMeans=mean(GC.cas.value), mutation="GC", SE=std.error(GC.cas.value), class="cas")
#-------
TA.ref.value <- N4SU.dat[which(N4SU.dat$mutation=="TA" & N4SU.dat$class=="ref"), 2]
TA.N4SU.ref.dat <- data.frame(rateMeans=mean(TA.ref.value), mutation="TA", SE=std.error(TA.ref.value), class="ref")

TC.ref.value <- N4SU.dat[which(N4SU.dat$mutation=="TC" & N4SU.dat$class=="ref"), 2]
TC.N4SU.ref.dat <- data.frame(rateMeans=mean(TC.ref.value), mutation="TC", SE=std.error(TC.ref.value), class="ref")

TG.ref.value <- N4SU.dat[which(N4SU.dat$mutation=="TG" & N4SU.dat$class=="ref"), 2]
TG.N4SU.ref.dat <- data.frame(rateMeans=mean(TG.ref.value), mutation="TG", SE=std.error(TG.ref.value), class="ref")

AT.ref.value <- N4SU.dat[which(N4SU.dat$mutation=="AT" & N4SU.dat$class=="ref"), 2]
AT.N4SU.ref.dat <- data.frame(rateMeans=mean(AT.ref.value), mutation="AT", SE=std.error(AT.ref.value), class="ref")

AC.ref.value <- N4SU.dat[which(N4SU.dat$mutation=="AC" & N4SU.dat$class=="ref"), 2]
AC.N4SU.ref.dat <- data.frame(rateMeans=mean(AC.ref.value), mutation="AC", SE=std.error(AC.ref.value), class="ref")

AG.ref.value <- N4SU.dat[which(N4SU.dat$mutation=="AG" & N4SU.dat$class=="ref"), 2]
AG.N4SU.ref.dat <- data.frame(rateMeans=mean(AG.ref.value), mutation="AG", SE=std.error(AG.ref.value), class="ref")

CT.ref.value <- N4SU.dat[which(N4SU.dat$mutation=="CT" & N4SU.dat$class=="ref"), 2]
CT.N4SU.ref.dat <- data.frame(rateMeans=mean(CT.ref.value), mutation="CT", SE=std.error(CT.ref.value), class="ref")

CA.ref.value <- N4SU.dat[which(N4SU.dat$mutation=="CA" & N4SU.dat$class=="ref"), 2]
CA.N4SU.ref.dat <- data.frame(rateMeans=mean(CA.ref.value), mutation="CA", SE=std.error(CA.ref.value), class="ref")

CG.ref.value <- N4SU.dat[which(N4SU.dat$mutation=="CG" & N4SU.dat$class=="ref"), 2]
CG.N4SU.ref.dat <- data.frame(rateMeans=mean(CG.ref.value), mutation="CG", SE=std.error(CG.ref.value), class="ref")

GA.ref.value <- N4SU.dat[which(N4SU.dat$mutation=="GA" & N4SU.dat$class=="ref"), 2]
GA.N4SU.ref.dat <- data.frame(rateMeans=mean(GA.ref.value), mutation="GA", SE=std.error(GA.ref.value), class="ref")

GT.ref.value <- N4SU.dat[which(N4SU.dat$mutation=="GT" & N4SU.dat$class=="ref"), 2]
GT.N4SU.ref.dat <- data.frame(rateMeans=mean(GT.ref.value), mutation="GT", SE=std.error(GT.ref.value), class="ref")

GC.ref.value <- N4SU.dat[which(N4SU.dat$mutation=="GC" & N4SU.dat$class=="ref"), 2]
GC.N4SU.ref.dat <- data.frame(rateMeans=mean(GC.ref.value), mutation="GC", SE=std.error(GC.ref.value), class="ref")

#-------
TA.neu.value <- N4SU.dat[which(N4SU.dat$mutation=="TA" & N4SU.dat$class=="neu"), 2]
TA.N4SU.neu.dat <- data.frame(rateMeans=mean(TA.neu.value), mutation="TA", SE=std.error(TA.neu.value), class="neu")

TC.neu.value <- N4SU.dat[which(N4SU.dat$mutation=="TC" & N4SU.dat$class=="neu"), 2]
TC.N4SU.neu.dat <- data.frame(rateMeans=mean(TC.neu.value), mutation="TC", SE=std.error(TC.neu.value), class="neu")

TG.neu.value <- N4SU.dat[which(N4SU.dat$mutation=="TG" & N4SU.dat$class=="neu"), 2]
TG.N4SU.neu.dat <- data.frame(rateMeans=mean(TG.neu.value), mutation="TG", SE=std.error(TG.neu.value), class="neu")

AT.neu.value <- N4SU.dat[which(N4SU.dat$mutation=="AT" & N4SU.dat$class=="neu"), 2]
AT.N4SU.neu.dat <- data.frame(rateMeans=mean(AT.neu.value), mutation="AT", SE=std.error(AT.neu.value), class="neu")

AC.neu.value <- N4SU.dat[which(N4SU.dat$mutation=="AC" & N4SU.dat$class=="neu"), 2]
AC.N4SU.neu.dat <- data.frame(rateMeans=mean(AC.neu.value), mutation="AC", SE=std.error(AC.neu.value), class="neu")

AG.neu.value <- N4SU.dat[which(N4SU.dat$mutation=="AG" & N4SU.dat$class=="neu"), 2]
AG.N4SU.neu.dat <- data.frame(rateMeans=mean(AG.neu.value), mutation="AG", SE=std.error(AG.neu.value), class="neu")

CT.neu.value <- N4SU.dat[which(N4SU.dat$mutation=="CT" & N4SU.dat$class=="neu"), 2]
CT.N4SU.neu.dat <- data.frame(rateMeans=mean(CT.neu.value), mutation="CT", SE=std.error(CT.neu.value), class="neu")

CA.neu.value <- N4SU.dat[which(N4SU.dat$mutation=="CA" & N4SU.dat$class=="neu"), 2]
CA.N4SU.neu.dat <- data.frame(rateMeans=mean(CA.neu.value), mutation="CA", SE=std.error(CA.neu.value), class="neu")

CG.neu.value <- N4SU.dat[which(N4SU.dat$mutation=="CG" & N4SU.dat$class=="neu"), 2]
CG.N4SU.neu.dat <- data.frame(rateMeans=mean(CG.neu.value), mutation="CG", SE=std.error(CG.neu.value), class="neu")

GA.neu.value <- N4SU.dat[which(N4SU.dat$mutation=="GA" & N4SU.dat$class=="neu"), 2]
GA.N4SU.neu.dat <- data.frame(rateMeans=mean(GA.neu.value), mutation="GA", SE=std.error(GA.neu.value), class="neu")

GT.neu.value <- N4SU.dat[which(N4SU.dat$mutation=="GT" & N4SU.dat$class=="neu"), 2]
GT.N4SU.neu.dat <- data.frame(rateMeans=mean(GT.neu.value), mutation="GT", SE=std.error(GT.neu.value), class="neu")

GC.neu.value <- N4SU.dat[which(N4SU.dat$mutation=="GC" & N4SU.dat$class=="neu"), 2]
GC.N4SU.neu.dat <- data.frame(rateMeans=mean(GC.neu.value), mutation="GC", SE=std.error(GC.neu.value), class="neu")

plot.dat <- rbind(TA.N4SU.cas.dat,
                  TC.N4SU.cas.dat,
                  TG.N4SU.cas.dat,
                  AT.N4SU.cas.dat,
                  AC.N4SU.cas.dat,
                  AG.N4SU.cas.dat,
                  CA.N4SU.cas.dat,
                  CT.N4SU.cas.dat,
                  CG.N4SU.cas.dat,
                  GA.N4SU.cas.dat,
                  GT.N4SU.cas.dat,
                  GC.N4SU.cas.dat,
                  TA.N4SU.ref.dat,
                  TC.N4SU.ref.dat,
                  TG.N4SU.ref.dat,
                  AT.N4SU.ref.dat,
                  AC.N4SU.ref.dat,
                  AG.N4SU.ref.dat,
                  CA.N4SU.ref.dat,
                  CT.N4SU.ref.dat,
                  CG.N4SU.ref.dat,
                  GA.N4SU.ref.dat,
                  GT.N4SU.ref.dat,
                  GC.N4SU.ref.dat,
                  TA.N4SU.neu.dat,
                  TC.N4SU.neu.dat,
                  TG.N4SU.neu.dat,
                  AT.N4SU.neu.dat,
                  AC.N4SU.neu.dat,
                  AG.N4SU.neu.dat,
                  CA.N4SU.neu.dat,
                  CT.N4SU.neu.dat,
                  CG.N4SU.neu.dat,
                  GA.N4SU.neu.dat,
                  GT.N4SU.neu.dat,
                  GC.N4SU.neu.dat)


ggplot(plot.dat, aes(x=mutation, y=rateMeans, fill=class))+
  geom_bar(stat='identity', position=position_dodge(), alpha=0.7) +
  geom_errorbar(aes(ymax = rateMeans + SE, ymin = rateMeans - SE), position = position_dodge(0.9), width = 0.2)+
  scale_fill_manual(values=c("coral3", "cornflowerblue","khaki")) +
  scale_y_continuous(limits = c(0,0.27),breaks = c(0,0.05,0.1,0.15,0.2,0.25))+
  theme_bw() +
  theme_pattern 


#Figure 2c-d

#find M-Z transitioned genes at late2cell
#Read files
MZgene_mutRate <- read.table(file="Attached_doc/MZgenes mutation rate.txt", sep="\t", header=T)
sample <- MZgene_mutRate[ ,1]
mutData <- MZgene_mutRate[ ,-1]
#make boxplot for overall and individual zygotic genes
zygotic.dat <- data.frame(rate=mutData[,"Zygotic" ], Treatment=sample)
zygotic.dat <- zygotic.dat[-18, ]#remove sample-20 that is very wired in both zygotic and maternal genes, one outlier 

ggplot(zygotic.dat, aes(x=Treatment, y=rate))+
  geom_boxplot(outlier.shape=NA, alpha=0.7) +
  geom_point(position=position_jitterdodge(jitter.width = 0.3, dodge.width = 1), 
             aes(fill=Treatment), size=1.6, pch=21) +
  theme_bw() +
  scale_y_continuous(limits = c(0,0.5),breaks = c(0,0.1,0.2,0.3,0.4,0.5)) +
  theme_pattern 


Ctsl.dat <- data.frame(rate=mutData[,"Ctsl" ], Treatment=sample)
Ctsl.dat <- Ctsl.dat[-8, ] #remove one strange sample
ggplot(Ctsl.dat, aes(x=Treatment, y=rate))+
  geom_boxplot(outlier.shape=NA, alpha=0.7) +
  geom_point(position=position_jitterdodge(jitter.width = 0.3, dodge.width = 1), 
             aes(fill=Treatment), size=1.6, pch=21) +
  theme_bw() +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1)) +
  theme_pattern 


Btbd18.dat <- data.frame(rate=mutData[,"Btbd18" ], Treatment=sample)
Btbd18.dat <- Btbd18.dat[-18, ]#remove sample-20 that is very wired in both zygotic and maternal genes, one outlier
ggplot(Btbd18.dat, aes(x=Treatment, y=rate))+
  geom_boxplot(outlier.shape=NA, alpha=0.7) +
  geom_point(position=position_jitterdodge(jitter.width = 0.3, dodge.width = 1), 
             aes(fill=Treatment), size=1.5, pch=21) +
  theme_bw() +
  scale_y_continuous(limits = c(0,0.6),breaks = c(0,0.2,0.4,0.6)) +
  theme_pattern 


Trim43b.dat <- data.frame(rate=mutData[,"Trim43b" ], Treatment=sample)
Trim43b.dat <- Trim43b.dat[-14, ]
ggplot(Trim43b.dat, aes(x=Treatment, y=rate))+
  geom_boxplot(outlier.shape=NA, alpha=0.7) +
  geom_point(position=position_jitterdodge(jitter.width = 0.3, dodge.width = 1), 
             aes(fill=Treatment), size=1.5, pch=21) +
  theme_bw() +
  #scale_y_continuous(limits = c(0,1),breaks = c(0,0.25,0.5,0.75,1)) +
  theme_pattern 

#make boxplot for overall and individual maternal genes

maternal.dat <- data.frame(rate=mutData[,"maternal" ], Treatment=sample)
maternal.dat <- maternal.dat[-18, ]#remove sample-20 that is very wired in both zygotic and maternal genes, one outlier

ggplot(maternal.dat, aes(x=Treatment, y=rate))+
  geom_boxplot(outlier.shape=NA, alpha=0.7) +
  geom_point(position=position_jitterdodge(jitter.width = 0.3, dodge.width = 1), 
             aes(fill=Treatment), size=1.6, pch=21) +
  theme_bw() +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1)) +
  theme_pattern 

H1foo.dat <- data.frame(rate=mutData[,"H1foo" ], Treatment=sample)
ggplot(H1foo.dat, aes(x=Treatment, y=rate))+
  geom_boxplot(outlier.shape=NA, alpha=0.7) +
  geom_point(position=position_jitterdodge(jitter.width = 0.3, dodge.width = 1), 
             aes(fill=Treatment), size=1.5, pch=21) +
  theme_bw() +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1)) +
  theme_pattern 


Bms1.dat <- data.frame(rate=mutData[,"Bms1" ], Treatment=sample)
ggplot(Bms1.dat, aes(x=Treatment, y=rate))+
  geom_boxplot(outlier.shape=NA, alpha=0.7) +
  geom_point(position=position_jitterdodge(jitter.width = 0.3, dodge.width = 1), 
             aes(fill=Treatment), size=1.5, pch=21) +
  theme_bw() +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1)) +
  theme_pattern 

Plat.dat <- data.frame(rate=mutData[,"Plat" ], Treatment=sample)
Plat.dat <- Plat.dat[-18, ]#remove sample-20 that is very wired in both zygotic and maternal genes, one outlier
ggplot(Plat.dat, aes(x=Treatment, y=rate))+
  geom_boxplot(outlier.shape=NA, alpha=0.7) +
  geom_point(position=position_jitterdodge(jitter.width = 0.3, dodge.width = 1), 
             aes(fill=Treatment), size=1.5, pch=21) +
  theme_bw() +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1)) +
  theme_pattern 

Padi6.dat <- data.frame(rate=mutData[,"Padi6" ], Treatment=sample)
ggplot(Padi6.dat, aes(x=Treatment, y=rate))+
  geom_boxplot(outlier.shape=NA, alpha=0.7) +
  geom_point(position=position_jitterdodge(jitter.width = 0.3, dodge.width = 1), 
             aes(fill=Treatment), size=1.5, pch=21) +
  theme_bw() +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1)) +
  theme_pattern 



#Figure 2e.
#calculate X:A ratio using mutant reads representing zygotic RNA transcription.
rm(list=ls())

#------Load geneInfo-----------------
GeneInfo.plus <- read.table(file="Attached_doc/GeneInfo.plus.txt", sep="\t", header=F, row.names=1)
colnames(GeneInfo.plus)=c("Chr","locus", "Strand","Length")
GeneInfo.minus <- read.table(file="Attached_doc/GeneInfo.minus.txt", sep="\t", header=F, row.names=1)
colnames(GeneInfo.minus)=c("Chr","locus", "Strand","Length")
GeneInfo <- rbind(GeneInfo.plus,GeneInfo.minus)

autoGenes <- rownames(GeneInfo[which(GeneInfo$Chr != "chrX" & GeneInfo$Chr != "chrY"), ])
chrXGenes <- rownames(GeneInfo[which(GeneInfo$Chr == "chrX"), ])

comp.All <- read.table(file="Data/4SU.All_rep.total.geneCount.txt",sep="\t",header=T, row.names=1)[ ,-1]
colnames(comp.All) <- c("s1","s2","s3","s4","s5","s6","s8",
                        "s9","s10","s11","s12","s13","s15","s16",
                        "s17","s18","s19","s20","s21")
#calculate RPKM
LibSize <- colSums(comp.All, na.rm = T) / 1000000  #Per million 
GeneLength <- GeneInfo[match(rownames(comp.All), rownames(GeneInfo)),"Length"] / 1000
comp.RPKM <- comp.All[ ,1, drop=F] / (LibSize[1] * GeneLength)
for (i in 2:ncol(comp.All)){
  comp.RPKM[,i] <- comp.All[ ,i] / (LibSize[i] * GeneLength)
  colnames(comp.RPKM)[i] <- colnames(comp.All)[i]
}

chrX.RPKM <- comp.RPKM[chrXGenes, ]
auto.RPKM <- comp.RPKM[autoGenes, ]

#X:A ratio of zygotic transcription is calculated from genes whose overall RPKM >=1 in 4SU treated samples (from s9 to s21), 

chrX.s9.genes <- chrXGenes[chrX.RPKM$`s9` >=1]
chrX.s10.genes <- chrXGenes[chrX.RPKM$`s10` >=1]
chrX.s11.genes <- chrXGenes[chrX.RPKM$`s11` >=1]
chrX.s12.genes <- chrXGenes[chrX.RPKM$`s12` >=1]
chrX.s13.genes <- chrXGenes[chrX.RPKM$`s13` >=1]
chrX.s15.genes <- chrXGenes[chrX.RPKM$`s15` >=1]
chrX.s16.genes <- chrXGenes[chrX.RPKM$`s16` >=1]
chrX.s17.genes <- chrXGenes[chrX.RPKM$`s17` >=1]
chrX.s18.genes <- chrXGenes[chrX.RPKM$`s18` >=1]
chrX.s19.genes <- chrXGenes[chrX.RPKM$`s19` >=1]
chrX.s20.genes <- chrXGenes[chrX.RPKM$`s20` >=1]
chrX.s21.genes <- chrXGenes[chrX.RPKM$`s21` >=1]

auto.s9.genes <- autoGenes[auto.RPKM$`s9` >=1]
auto.s10.genes <- autoGenes[auto.RPKM$`s10` >=1]
auto.s11.genes <- autoGenes[auto.RPKM$`s11` >=1]
auto.s12.genes <- autoGenes[auto.RPKM$`s12` >=1]
auto.s13.genes <- autoGenes[auto.RPKM$`s13` >=1]
auto.s15.genes <- autoGenes[auto.RPKM$`s15` >=1]
auto.s16.genes <- autoGenes[auto.RPKM$`s16` >=1]
auto.s17.genes <- autoGenes[auto.RPKM$`s17` >=1]
auto.s18.genes <- autoGenes[auto.RPKM$`s18` >=1]
auto.s19.genes <- autoGenes[auto.RPKM$`s19` >=1]
auto.s20.genes <- autoGenes[auto.RPKM$`s20` >=1]
auto.s21.genes <- autoGenes[auto.RPKM$`s21` >=1]

#read all T->C mutant read count
s9 <- read.table(file="Data/4SU.rep1.comp.mismatch_gene.count", sep="\t", row.names=1)
s10 <- read.table(file="Data/4SU.rep2.comp.mismatch_gene.count", sep="\t", row.names=1)
s11 <- read.table(file="Data/4SU.rep3.comp.mismatch_gene.count", sep="\t", row.names=1)
s12 <- read.table(file="Data/4SU.rep4.comp.mismatch_gene.count", sep="\t", row.names=1)
s13 <- read.table(file="Data/4SU.rep5.comp.mismatch_gene.count", sep="\t", row.names=1)
s15 <- read.table(file="Data/4SU.rep6.comp.mismatch_gene.count", sep="\t", row.names=1)
s16 <- read.table(file="Data/4SU.rep7.comp.mismatch_gene.count", sep="\t", row.names=1)
s17 <- read.table(file="Data/4SU.rep8.comp.mismatch_gene.count", sep="\t", row.names=1)
s18 <- read.table(file="Data/4SU.rep9.comp.mismatch_gene.count", sep="\t", row.names=1)
s19 <- read.table(file="Data/4SU.rep10.comp.mismatch_gene.count", sep="\t", row.names=1)
s20 <- read.table(file="Data/4SU.rep11.comp.mismatch_gene.count", sep="\t", row.names=1)
s21 <- read.table(file="Data/4SU.rep12.comp.mismatch_gene.count", sep="\t", row.names=1)

Mut.count <- data.frame(s9=s9[rownames(GeneInfo), ], 
                        s10=s10[rownames(GeneInfo), ], 
                        s11=s11[rownames(GeneInfo), ],
                        s12=s12[rownames(GeneInfo), ],
                        s13=s13[rownames(GeneInfo), ],
                        s15=s15[rownames(GeneInfo), ],
                        s16=s16[rownames(GeneInfo), ],
                        s17=s17[rownames(GeneInfo), ],
                        s18=s18[rownames(GeneInfo), ],
                        s19=s19[rownames(GeneInfo), ],
                        s20=s20[rownames(GeneInfo), ],
                        s21=s21[rownames(GeneInfo), ])
rownames(Mut.count) <- rownames(GeneInfo)

#remove Pcdhga or Pcdhgb gene counts due to the exact overlap of the last exon of these genes.
#The same set of reads were counted many times.
#The count will kept for only Pcdhga1 in each replicates.The counts for rest of Pcdhgb and Pcdhga genes will be removed.
Pcdhga_genes <- c("Pcdhga2","Pcdhga3","Pcdhgb1", "Pcdhga4", "Pcdhgb2","Pcdhga5","Pcdhga6","Pcdhga7",
                  "Pcdhgb4", "Pcdhga8","Pcdhgb5","Pcdhga9","Pcdhgb6","Pcdhga10", "Pcdhgb7","Pcdhga11",
                  "Pcdhgb8", "Pcdhga12", "Pcdhgc3")
Mut.count[Pcdhga_genes, ] <- 0




#calculate RPKM
Mut.LibSize <- LibSize[c(8:19)]  #Per million 
GeneLength <- GeneInfo[match(rownames(comp.All), rownames(GeneInfo)),"Length"] / 1000
Mut.RPKM <- Mut.count[ ,1, drop=F] / (LibSize[1] * GeneLength)
for (i in 2:ncol(Mut.count)){
  Mut.RPKM[,i] <- Mut.count[ ,i] / (LibSize[i] * GeneLength)
  colnames(Mut.RPKM)[i] <- colnames(Mut.count)[i]
}

auto.Mut.RPKM <- Mut.RPKM[autoGenes, ]
chrX.Mut.RPKM <- Mut.RPKM[chrXGenes, ]

Female <- c("s10","s12","s13","s15","s19","s20","s21")
Male <- c("s9","s11","s16","s17","s18")

#Male

#late2cell - sample9
chrX.Mut.s9.genes <- chrXGenes[complete.cases(chrX.Mut.RPKM$s9 > 0)]
auto.Mut.s9.genes <- autoGenes[complete.cases(auto.Mut.RPKM$s9 > 0)]
chrX.M.s9.genes <- intersect(chrX.Mut.s9.genes, chrX.s9.genes)
auto.M.s9.genes <- intersect(auto.Mut.s9.genes, auto.s9.genes)
chrX.M.s9.RPKM <- chrX.Mut.RPKM[chrX.M.s9.genes,"s9"]
auto.M.s9.RPKM <- auto.Mut.RPKM[auto.M.s9.genes,"s9"]

#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.s9.RPKM, length(chrX.M.s9.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.s9.RPKM) / mean(auto.random.M.RPKM)
}
#calculate mean and A/X ratio
M.XtoA <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Male", stringsAsFactors=FALSE)

#late2cell - sample11
chrX.Mut.s11.genes <- chrXGenes[complete.cases(chrX.Mut.RPKM$s11 > 0)]
auto.Mut.s11.genes <- autoGenes[complete.cases(auto.Mut.RPKM$s11 > 0)]
chrX.M.s11.genes <- intersect(chrX.Mut.s11.genes, chrX.s11.genes)
auto.M.s11.genes <- intersect(auto.Mut.s11.genes, auto.s11.genes)
chrX.M.s11.RPKM <- chrX.Mut.RPKM[chrX.M.s11.genes,"s11"]
auto.M.s11.RPKM <- auto.Mut.RPKM[auto.M.s11.genes,"s11"]

#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.s11.RPKM, length(chrX.M.s11.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.s11.RPKM) / mean(auto.random.M.RPKM)
}
#calculate mean and A/X ratio
M.XtoA[2,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Male", stringsAsFactors=FALSE)

#late2cell - sample16
chrX.Mut.s16.genes <- chrXGenes[complete.cases(chrX.Mut.RPKM$s16 > 0)]
auto.Mut.s16.genes <- autoGenes[complete.cases(auto.Mut.RPKM$s16 > 0)]
chrX.M.s16.genes <- intersect(chrX.Mut.s16.genes, chrX.s16.genes)
auto.M.s16.genes <- intersect(auto.Mut.s16.genes, auto.s16.genes)
chrX.M.s16.RPKM <- chrX.Mut.RPKM[chrX.M.s16.genes,"s16"]
auto.M.s16.RPKM <- auto.Mut.RPKM[auto.M.s16.genes,"s16"]

#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.s16.RPKM, length(chrX.M.s16.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.s16.RPKM) / mean(auto.random.M.RPKM)
}
#calculate mean and A/X ratio
M.XtoA[3,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Male", stringsAsFactors=FALSE)

#late2cell - sample17
chrX.Mut.s17.genes <- chrXGenes[complete.cases(chrX.Mut.RPKM$s17 > 0)]
auto.Mut.s17.genes <- autoGenes[complete.cases(auto.Mut.RPKM$s17 > 0)]
chrX.M.s17.genes <- intersect(chrX.Mut.s17.genes, chrX.s17.genes)
auto.M.s17.genes <- intersect(auto.Mut.s17.genes, auto.s17.genes)
chrX.M.s17.RPKM <- chrX.Mut.RPKM[chrX.M.s17.genes,"s17"]
auto.M.s17.RPKM <- auto.Mut.RPKM[auto.M.s17.genes,"s17"]

#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.s17.RPKM, length(chrX.M.s17.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.s17.RPKM) / mean(auto.random.M.RPKM)
}
#calculate mean and A/X ratio
M.XtoA[4,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Male", stringsAsFactors=FALSE)

#late2cell - sample18
chrX.Mut.s18.genes <- chrXGenes[complete.cases(chrX.Mut.RPKM$s18 > 0)]
auto.Mut.s18.genes <- autoGenes[complete.cases(auto.Mut.RPKM$s18 > 0)]
chrX.M.s18.genes <- intersect(chrX.Mut.s18.genes, chrX.s18.genes)
auto.M.s18.genes <- intersect(auto.Mut.s18.genes, auto.s18.genes)
chrX.M.s18.RPKM <- chrX.Mut.RPKM[chrX.M.s18.genes,"s18"]
auto.M.s18.RPKM <- auto.Mut.RPKM[auto.M.s18.genes,"s18"]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.s18.RPKM, length(chrX.M.s18.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.s18.RPKM) / mean(auto.random.M.RPKM)
}
#calculate mean and A/X ratio
M.XtoA[5,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Male", stringsAsFactors=FALSE)



#Female
auto.F.Mut.RPKM <- auto.Mut.RPKM[ ,Female]
chrX.F.Mut.RPKM <- chrX.Mut.RPKM[ ,Female]



#late2cell - sample10
chrX.Mut.s10.genes <- chrXGenes[complete.cases(chrX.Mut.RPKM$s10 > 0)]
auto.Mut.s10.genes <- autoGenes[complete.cases(auto.Mut.RPKM$s10 > 0)]
chrX.F.s10.genes <- intersect(chrX.Mut.s10.genes, chrX.s10.genes)
auto.F.s10.genes <- intersect(auto.Mut.s10.genes, auto.s10.genes)
chrX.F.s10.RPKM <- chrX.Mut.RPKM[chrX.F.s10.genes,"s10"]
auto.F.s10.RPKM <- auto.Mut.RPKM[auto.F.s10.genes,"s10"]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.s10.RPKM, length(chrX.F.s10.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.s10.RPKM) / mean(auto.random.F.RPKM)
}
#calculate mean and A/X ratio
F.XtoA <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Female", stringsAsFactors=FALSE)

#late2cell - sample12
chrX.Mut.s12.genes <- chrXGenes[complete.cases(chrX.Mut.RPKM$s12 > 0)]
auto.Mut.s12.genes <- autoGenes[complete.cases(auto.Mut.RPKM$s12 > 0)]
chrX.F.s12.genes <- intersect(chrX.Mut.s12.genes, chrX.s12.genes)
auto.F.s12.genes <- intersect(auto.Mut.s12.genes, auto.s12.genes)
chrX.F.s12.RPKM <- chrX.Mut.RPKM[chrX.F.s12.genes,"s12"]
auto.F.s12.RPKM <- auto.Mut.RPKM[auto.F.s12.genes,"s12"]

#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.s12.RPKM, length(chrX.F.s12.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.s12.RPKM) / mean(auto.random.F.RPKM)
}
#calculate mean and A/X ratio
F.XtoA[2, ] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Female", stringsAsFactors=FALSE)

#late2cell - sample13
chrX.Mut.s13.genes <- chrXGenes[complete.cases(chrX.Mut.RPKM$s13 > 0)]
auto.Mut.s13.genes <- autoGenes[complete.cases(auto.Mut.RPKM$s13 > 0)]
chrX.F.s13.genes <- intersect(chrX.Mut.s13.genes, chrX.s13.genes)
auto.F.s13.genes <- intersect(auto.Mut.s13.genes, auto.s13.genes)
chrX.F.s13.RPKM <- chrX.Mut.RPKM[chrX.F.s13.genes,"s13"]
auto.F.s13.RPKM <- auto.Mut.RPKM[auto.F.s13.genes,"s13"]

#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.s13.RPKM, length(chrX.F.s13.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.s13.RPKM) / mean(auto.random.F.RPKM)
}
#calculate mean and A/X ratio
F.XtoA[3, ] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Female", stringsAsFactors=FALSE)

#late2cell - sample15
chrX.Mut.s15.genes <- chrXGenes[complete.cases(chrX.Mut.RPKM$s15 > 0)]
auto.Mut.s15.genes <- autoGenes[complete.cases(auto.Mut.RPKM$s15 > 0)]
chrX.F.s15.genes <- intersect(chrX.Mut.s15.genes, chrX.s15.genes)
auto.F.s15.genes <- intersect(auto.Mut.s15.genes, auto.s15.genes)
chrX.F.s15.RPKM <- chrX.Mut.RPKM[chrX.F.s15.genes,"s15"]
auto.F.s15.RPKM <- auto.Mut.RPKM[auto.F.s15.genes,"s15"]

#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.s15.RPKM, length(chrX.F.s15.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.s15.RPKM) / mean(auto.random.F.RPKM)
}
#calculate mean and A/X ratio
F.XtoA[4, ] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Female", stringsAsFactors=FALSE)

#late2cell - sample19
chrX.Mut.s19.genes <- chrXGenes[complete.cases(chrX.Mut.RPKM$s19 > 0)]
auto.Mut.s19.genes <- autoGenes[complete.cases(auto.Mut.RPKM$s19 > 0)]
chrX.F.s19.genes <- intersect(chrX.Mut.s19.genes, chrX.s19.genes)
auto.F.s19.genes <- intersect(auto.Mut.s19.genes, auto.s19.genes)
chrX.F.s19.RPKM <- chrX.Mut.RPKM[chrX.F.s19.genes,"s19"]
auto.F.s19.RPKM <- auto.Mut.RPKM[auto.F.s19.genes,"s19"]

#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.s19.RPKM, length(chrX.F.s19.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.s19.RPKM) / mean(auto.random.F.RPKM)
}
#calculate mean and A/X ratio
F.XtoA[5, ] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Female", stringsAsFactors=FALSE)

#late2cell - sample20
chrX.Mut.s20.genes <- chrXGenes[complete.cases(chrX.Mut.RPKM$s20 > 0)]
auto.Mut.s20.genes <- autoGenes[complete.cases(auto.Mut.RPKM$s20 > 0)]
chrX.F.s20.genes <- intersect(chrX.Mut.s20.genes, chrX.s20.genes)
auto.F.s20.genes <- intersect(auto.Mut.s20.genes, auto.s20.genes)
chrX.F.s20.RPKM <- chrX.Mut.RPKM[chrX.F.s20.genes,"s20"]
auto.F.s20.RPKM <- auto.Mut.RPKM[auto.F.s20.genes,"s20"]

#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.s20.RPKM, length(chrX.F.s20.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.s20.RPKM) / mean(auto.random.F.RPKM)
}
#calculate mean and A/X ratio
F.XtoA[6, ] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Female", stringsAsFactors=FALSE)

#late2cell - sample21
chrX.Mut.s21.genes <- chrXGenes[complete.cases(chrX.Mut.RPKM$s21 > 0)]
auto.Mut.s21.genes <- autoGenes[complete.cases(auto.Mut.RPKM$s21 > 0)]
chrX.F.s21.genes <- intersect(chrX.Mut.s21.genes, chrX.s21.genes)
auto.F.s21.genes <- intersect(auto.Mut.s21.genes, auto.s21.genes)
chrX.F.s21.RPKM <- chrX.Mut.RPKM[chrX.F.s21.genes,"s21"]
auto.F.s21.RPKM <- auto.Mut.RPKM[auto.F.s21.genes,"s21"]

#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.s21.RPKM, length(chrX.F.s21.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.s21.RPKM) / mean(auto.random.F.RPKM)
}
#calculate mean and A/X ratio
F.XtoA[7, ] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Female", stringsAsFactors=FALSE)


# combine data

plot.dat <- rbind(M.XtoA, F.XtoA)

library(ggplot2)
ggplot(plot.dat, aes(x = Gender, y = Ratio, fill = Gender)) +
  geom_boxplot(alpha=0.4, outlier.shape = NA) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1, dodge.width = 0.7), 
             aes(fill=Gender), size=2, pch=21) +
  scale_fill_brewer(palette = "Accent") +
  scale_color_brewer(palette = "Accent") +
  theme_bw() +
  labs(y="X:A Ratio") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        axis.title.x=element_blank(),
        #legend.text=element_text(size=12),
        legend.title=element_blank(), 
        legend.key = element_blank(),
        axis.text.x=element_text(size = 12),
        legend.position = "top",
        legend.text=element_text(size = rel(1)), #face="bold"
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line.y = element_line(color="black"),
        axis.line.x = element_line(color="black")) +
  geom_hline(yintercept=1,  colour="grey",linetype="longdash")
