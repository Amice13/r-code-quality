rm(list=ls())

setwd("/Users/LDolan/Dropbox/Writing/Africa Globalization")

library(ggplot2)

load("Analysis/ab-output/regs.RData")

load("Analysis/milner-ghana-output/regs.RData")

regs_ghana <- as.matrix(rbind(c(se1_abr6_gha[2,c(1,2)],nobs(reg1_abr6_gha),"Ghana","edu","AB (round 6)","full"),
                              c(se2_abr6_gha[2,c(1,2)],nobs(reg2_abr6_gha),"Ghana","edu","AB (round 6)","employed"),
                              c(se1_abr8_gha[2,c(1,2)],nobs(reg1_abr8_gha),"Ghana","edu","AB (round 8)","full"),
                              c(se2_abr8_gha[2,c(1,2)],nobs(reg2_abr8_gha),"Ghana","edu","AB (round 8)","employed"),
                              c(se7[2,c(1,2)],nobs(reg7),"Ghana","edu","original","full") ,
                              c(se9[2,c(1,2)],nobs(reg9),"Ghana","edu","original","employed") ,
                              c(se14[2,c(1,2)],nobs(reg14),"Ghana","skill","original","employed")))

load("Analysis/milner-uganda-output/regs.RData")

regs_uganda <- as.matrix(rbind(c(se1_abr6_uga[2,c(1,2)],nobs(reg1_abr6_uga),"Uganda","edu","AB (round 6)","full"),
                               c(se2_abr6_uga[2,c(1,2)],nobs(reg2_abr6_uga),"Uganda","edu","AB (round 6)","employed"),
                               c(se1_abr8_uga[2,c(1,2)],nobs(reg1_abr8_uga),"Uganda","edu","AB (round 8)","full"),
                               c(se2_abr8_uga[2,c(1,2)],nobs(reg2_abr8_uga),"Uganda","edu","AB (round 8)","employed"),
                               c(se7[2,c(1,2)],nobs(reg7),"Uganda","edu","original","full") ,
                               c(se9[2,c(1,2)],nobs(reg9),"Uganda","edu","original","employed") ,
                               c(se14[2,c(1,2)],nobs(reg14),"Uganda","skill","original","employed")))

regs <- as.data.frame(rbind(regs_ghana,regs_uganda))
colnames(regs) <- c("coef","se","n","country","var","survey","subsample")
regs$coef <- as.numeric(as.character(regs$coef))
regs$se <- as.numeric(as.character(regs$se))
regs$N <- as.numeric(as.character(regs$n))
regs$x <- c(1,1.5,2,2.5,3,3.5,6,9,9.5,10,10.5,11,11.5,14)
regs$sample <- NA
regs$sample[regs$survey=="AB (round 6)"&regs$subsample=="full"] <- "Afrobarometer R6 (Full)"
regs$sample[regs$survey=="AB (round 6)"&regs$subsample=="employed"] <- "Afrobarometer R6 (Employed)"
regs$sample[regs$survey=="AB (round 8)"&regs$subsample=="full"] <- "Afrobarometer R8 (Full)"
regs$sample[regs$survey=="AB (round 8)"&regs$subsample=="employed"] <- "Afrobarometer R8 (Employed)"
regs$sample[regs$survey=="original"&regs$subsample=="full"] <- "Original (Full)"
regs$sample[regs$survey=="original"&regs$subsample=="employed"] <- "Original (Employed)"
regs$Sample <- factor(regs$sample,levels=c("Afrobarometer R6 (Full)","Afrobarometer R6 (Employed)","Afrobarometer R8 (Full)","Afrobarometer R8 (Employed)","Original (Full)","Original (Employed)"))

ggplot(regs)+geom_point(aes(x=x,y=coef,size=N,shape=Sample))+ geom_segment(aes(x = x, y = (coef-1.96*se), xend = x, yend = (coef+1.96*se))) +
  theme_bw()+scale_shape_manual(values=c(16,1,15,0,18, 5))+expand_limits(x=c(0,10))+scale_x_continuous(breaks=c(2.25,6,10.25,14),labels=c("Education","Skill","Education","Skill")) +
  geom_hline(aes(yintercept=0),linetype="dashed")+annotate("text",label="Ghana",x=(3.5),y=.05,size=5)+annotate("text",label="Uganda",x=(11.5),y=.05,size=5)+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),legend.position="right",axis.title.x=element_blank(),axis.ticks.x=element_blank())+ylab("Coefficient on education/skill")
ggsave(file="analysis/ab-output/alt-skill.pdf",height=6,width=8)

