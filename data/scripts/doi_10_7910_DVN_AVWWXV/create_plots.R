rm(list=ls())
library(foreign)
library(ggplot2)
library(ggridges)
#install.packages("remotes")
## set to output subdirectory in main replication directory 

#install.packages("viridis")  # Install
library("viridis")
library("RColorBrewer")
# The palette with black:
cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbPalette <- plasma(4)
cbPalette <- brewer.pal(4,"Dark2")


###############################
######### Figure 1
###############################

statadata <- read.dta("data/affective_polarization_fig1.dta")
statadata <- statadata[-1,]
ggdata <- data.frame(Year=rep(statadata$year,2),Measure=c(statadata$partystrength*100,statadata$partythermdiff),Series=c(rep("Strong Partisans",nrow(statadata)),rep("Thermometer Difference",nrow(statadata))))

ggdata2 <- data.frame(Year=statadata$year,Measure=(1-statadata$partyloyalty)*100,Series=rep("Partisan Defectors",nrow(statadata)))

ggdata2 <- ggdata2[is.na(ggdata2$Measure)==0,]

ggdata <- rbind(ggdata,ggdata2)

ggdata2 <- data.frame(Year=statadata$year,Measure=(statadata$modifiedmp)*100,Series=rep("Major-Party Vote Switchers",nrow(statadata)))

ggdata2 <- ggdata2[is.na(ggdata2$Measure)==0,]

ggdata <- rbind(ggdata,ggdata2)

cbPalette <- brewer.pal(9,"Greys")


plot1 <- ggplot(ggdata,aes(x=Year,y=Measure,group=Series)) + 
    geom_line(aes(color=Series,linetype=Series),linewidth=1) +
    geom_point(aes(color=Series,shape=Series),size=2) + 
    scale_y_continuous(limits=c(0,60), expand = c(0, 0), sec.axis=sec_axis(~., name="Average Difference In-Party\nOut-Party Thermometer Score")) +
    scale_x_continuous(limits=c(1950,2024), expand = c(0, 0)) + 
    labs(y = "Percent", color="", shape="") +
    scale_colour_manual(name="",values=cbPalette[c(4,4,8,8)]) +
    scale_shape_manual(name="",values=c(2,4,19,5)) +
    scale_linetype_manual(name="",values=c("solid","longdash","dashed","dotted")) +
    theme_classic() + 
    theme(text = element_text(family="Lato"),legend.position="bottom",axis.line.y.left=element_blank(),axis.line.y.right=element_blank(), panel.grid.major.y=element_line(colour="gray90",linetype=1)) + 
    guides(color = guide_legend(nrow = 1, byrow = TRUE),linetype=guide_legend(nrow=1,byrow=TRUE))
plot1
ggsave(paste(outputdir,"combpartystrength.png",sep=""),plot1,width=7,height=5)


###########################################################
############ Figure 2
###########################################################
############ Numbers are from panel log files.
###########################################################

panels <- c("56-60 ANES","72-76 ANES","92-96 ANES","00-04 ANES","12-16 TAPS","16-20 GSS/ANES")
panels_ordered <- factor(panels,ordered=TRUE, levels = c("56-60 ANES","72-76 ANES","92-96 ANES","00-04 ANES","12-16 TAPS","16-20 GSS/ANES"))

ggdata <- data.frame(Study = rep(panels_ordered,2), 
                      switch = c(26.8, 30.2, 19.3, 13.3, 14.5, 13.3, 
                                  6.77, 7.03, 10.30, 8.87, 10.46, 8.09), 
                      stderror = c(1.6,1.6,1.6,1.1, 1.9,1.6,
                                   .68,.71,1.25,1.04,1.44,1.03), 
                     Measure = c(rep("Presidential Support",6),rep("Partisanship",6))
                     )
ggdata$lowerbound <- ggdata$switch- ggdata$stderror*qnorm(.975)
ggdata$upperbound <- ggdata$switch+ggdata$stderror*qnorm(.975)
                     

panelplot <- ggplot(ggdata,aes(Study,switch,shape=Measure,group=Measure,colour=Measure),size=3) + 
    geom_errorbar(aes(ymin=lowerbound,ymax=upperbound),alpha=.75,width=.3) +
    geom_point() + 
    ylab("Percent Switching") + 
    xlab("Panel Study") +
    theme_classic()  +   
    scale_y_continuous(limits=c(0,33.5), expand = c(0, 0)) +    scale_colour_manual(values=cbPalette[c(8,5)]) +
    theme(text = element_text(family="Lato"),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),legend.position="bottom",axis.line.y.left=element_blank(), panel.grid.major.y=element_line(colour="gray90",linetype=1))
panelplot
ggsave("panelswitch.png",panelplot,height=7,width=5)



##########################################################
## Figure 3
## Comparing Category Char Curve by latent partisanship
########################################################

rm(list=ls())

library(foreign)
statadata <- read.dta("data/theta_compare_prepost.dta")

library(ggplot2)
library(RColorBrewer)
pal <- brewer.pal(3,"Spectral")
cbPalette <- brewer.pal(4,"Dark2")

## difficulties are retrieved from by estimates by dividing cut point by slope  
ggdata <- data.frame(latentpid=statadata$partystrength_free,weight=statadata$newweight,Era=ifelse(statadata$post==0,"1952-1998","2000-2020"))

## See .do file estimates
olddifficulties <- data.frame(cut=c(.853827,4.021256/.7027868,1.675745/.4364693,1.497501/.7647918),Item=c("Party ID Lean","Strong Party ID","Party Affect Lean","Diff. in Party Therm."),Era=rep("1952-1998",4))
newdifficulties <- data.frame(cut=c(.853827,3.747909/.7428152,1.791737/.5486295,1.378232/1.118468),Item=c("Party ID Lean","Strong Party ID","Party Affect Lean","Diff. in Party Therm."),Era=rep("2000-2020",4))


length <- 1000
x<- seq(0,12,length.out=length)

phat.partylean <- plogis(-.853827 + x)
phat.partytherm.pre <- plogis(-1.497501 + .7647918*x)
phat.partytherm.post <- plogis(-1.378232 + 1.118468*x)
phat.partyaffect.pre <- plogis(-1.675745 + .4364693*x)
phat.partyaffect.post <- plogis(-1.791737 + .5486295*x)
phat.strongparty.pre <- plogis(-4.021256 + .7027868*x)
phat.strongparty.post <- plogis(-3.747909 + .7428152*x)

ggdata2 <- data.frame(latentpid = rep(x,8), 
                      density = c(phat.partylean,phat.partytherm.pre,phat.partyaffect.pre,phat.strongparty.pre,phat.partylean,phat.partytherm.post,phat.partyaffect.post,phat.strongparty.post),
                      Item = rep(c(rep("Party ID Lean",length),rep("Diff. in Party Therm.",length),rep("Party Affect Lean",length),rep("Strong Party ID",length)),2),
                      Era = c(rep("1952-1998",length*4),rep("2000-2020",length*4)))
ggdata2$Item <- factor(ggdata2$Item,levels=c("Party ID Lean","Diff. in Party Therm.","Party Affect Lean","Strong Party ID"))

ggdatapre <- ggdata2[ggdata2$Era=="1952-1998",]
ggdatapost <- ggdata2[ggdata2$Era=="2000-2020",]

predifficulties <- data.frame(x1=c(4.021256/.7027868,1.675745/.4364693,1.497501/.7647918),
                              x2=c(3.747909/.7428152,1.791737/.5486295,1.378232/1.118468),
                              y1=rep(.5,3),
                              y2=rep(.5,3),
                              rep("2000-2020",3),
                              Item=rep(c("Strong Party ID","Party Affect Lean","Diff. in Party Therm."),2))

predifficulties$Item <- factor(predifficulties$Item,levels=c("Party ID Lean","Diff. in Party Therm.","Party Affect Lean","Strong Party ID"))

baseplot <- ggplot(ggdata,aes(latentpid,density,colour=Item,linetype=Item)) + 
    geom_line(data=ggdatapre,aes(latentpid,density,colour=Item,linetype=Item),alpha=.3) + 
    geom_line(data=ggdatapost,aes(latentpid,density,colour=Item,linetype=Item)) +
    geom_segment(data=predifficulties,aes(x=x1,xend=x2,y=y1,yend=y2,linetype=Item),arrow=arrow(type="closed",length=unit(0.02, "npc"))) +
    scale_color_brewer(type="qual",palette="Dark2") +
    theme_minimal() + 
    xlim(0,12.5) +
    ylab("Probability of Responding With") +
    xlab("")  +
    theme(legend.position=c(.8,.2),text = element_text(family="Lato"),axis.text.x=element_blank(),axis.title.x=element_blank())


ggdatapre <- ggdata[ggdata$Era=="1952-1998",]
ggdatapost <- ggdata[ggdata$Era=="2000-2020",]


secondplot <- ggplot(ggdata,aes(latentpid)) +
  geom_density(data=ggdatapre,aes(x=latentpid,weight=weight),bw=.5,colour="gray70") + 
  geom_density(data=ggdatapost,aes(x=latentpid,weight=weight),bw=.5)+
  annotate("text",x=.62,y=.165,label="2000-2020",family="Lato") +
  annotate("text",x=8.5,y=.08,label="1952-1998",family= "Lato",col="gray70") +
  theme_minimal() + 
  xlim(0,12.5) +
  ylab("Sample Density") + 
  xlab("Latent Partisan Strength") + 
  theme(text = element_text(family="Lato"))


library(gridExtra)

ggsave("difficultyplot.png",arrangeGrob(grobs=list(baseplot,secondplot),layout_matrix=matrix(c(1,1,2),nr=3,nc=1)),width=6,height=7)


################################
## Figure 4
## aROC estimates by panel and measure
## Taken from stata output
###################################

rm(list=ls())
library(ggplot2)

panels <- c("56-60 ANES","72-76 ANES","92-96 ANES","00-04 ANES","16-20 ANES")
panels_ordered <- factor(panels,ordered=TRUE, levels = c("56-60 ANES","72-76 ANES","92-96 ANES","00-04 ANES","16-20 ANES"))

strength <- c(.6550,.6291,.6523,.6976,.6797)
strength.se <- c(.0282,.0285,.0311,.0332, .0145)
therm <- c(NA,.5398,.6457,.6316, .7307)
therm.se <- c(NA,.0304,.0366,.0386,.0187)
latent <- c(.7155,.6886,.7413,.7478,.8420)
latent.se <- c(.0291,.0290,.0319,.0329,.0134)
ggdata <- data.frame(Study = rep(panels_ordered,3), aroc = c(strength,therm,latent), stderror = c(strength.se,therm.se,latent.se), Measure = c(rep("Subj. Strength",5),rep("|Therm. Difference|",5),rep("Latent Estimate",5)))
ggdata$Measure <- factor(ggdata$Measure,ordered=TRUE,levels=c("|Therm. Difference|","Subj. Strength","Latent Estimate"))

ggdata$lowerbound <- ggdata$aroc - qnorm(.975)*ggdata$stderror
ggdata$upperbound <- ggdata$aroc + qnorm(.975)*ggdata$stderror

panelplot <- ggplot(ggdata,aes(Study,aroc,shape=Measure,group=Measure,colour=Measure,linetype=Measure),size=3) + 
  geom_errorbar(aes(ymin=lowerbound,ymax=upperbound),position="dodge",alpha=.75,width=.3) +
  geom_point(position=position_dodge2(width=0.3, preserve="single")) + 
  ylab("AUC") + 
  xlab("Panel Study") +
  theme_classic()  +   
  scale_y_continuous(limits=c(.45,1), expand = c(0, 0)) +
  coord_flip() + 
  scale_color_brewer(type="qual",palette="Dark2") +
  theme(text = element_text(family="Lato"),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),legend.position="bottom",axis.line.y.left=element_blank(), panel.grid.major.y=element_line(colour="gray90",linetype=1))
panelplot

ggsave("aroc.png",width=5.5,height=4.5)


################################
## Figure 5, Figure 6, and Figure SI.E.1
## Replicate data using create_figure5figure6siE_estimates.do
###################################

rm(list=ls())
library(foreign)
library(ggplot2)
library(ggridges)
#install.packages("viridis")  # Install
library("viridis")
library("RColorBrewer")
# The palette with black:
cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbPalette <- plasma(4)
cbPalette <- brewer.pal(4,"Dark2")

pal <- c("#43a2cac2","#aa8dbd","#e34a33c2")


statadata <- read.dta("data/postcompareyear_gsem_grm_1952.dta")
summary(statadata)
statadata<- statadata[is.na(statadata$compareyear)==0,]

#############################
# SI E, fixed relevance image first
#############################

ggplot(statadata,aes(y=as.factor(year),x=theta_compareyears_grm_fixed_n,height=stat(density),fill=stat(x))) + 
    geom_density_ridges_gradient(aes(height=..density..,
                                     weight = VCF0009z),
                                 stat="density",bw=.6) +
    scale_fill_gradient2(
        low = pal[1], 
        mid = pal[2], 
        high = pal[3], 
        midpoint = 0
    ) +
    scale_y_discrete(expand = c(0.01, 0)) +
    scale_x_continuous(lim=c(-15,15),expand = c(0.01, 0)) +
    ylab("Year") + 
    xlab("Latent Partisanship\n(Left = Dem.; Right = Rep.; 0=Pure Independent)") + 
    labs(caption="Data: ANES Cumulative Data File (weighted)") + theme_ridges() + theme(text = element_text(family="Lato"),legend.pos="off")

ggsave("latentpid_nodif_notitle.png",width=6,height=7)

########################################
## Now Figure 5, varying relevance
#########################################

ggplot(statadata,aes(y=as.factor(year),x=theta_compareyears_grm_free_n,height=stat(density),fill=stat(x))) + 
    geom_density_ridges_gradient(aes(height=..density..,
                                     weight = VCF0009z),
                                 stat="density",bw=.6) +
    scale_fill_gradient2(
        low = pal[1], 
        mid = pal[2], 
        high = pal[3], 
        midpoint = 0
    ) +
    scale_y_discrete(expand = c(0.01, 0)) +
    scale_x_continuous(lim=c(-15,15),expand = c(0.01, 0)) +
    ylab("Year") + 
    xlab("Latent Partisanship\n(Left = Dem.; Right = Rep.; 0=Pure Independent)") + 
    labs(caption="Data: ANES Cumulative Data File (weighted)") + theme_ridges() + theme(text = element_text(family="Lato"),legend.pos="off")

ggsave(paste(outputdir,"latentpid_dif_notitle.png",sep=""),width=6,height=7)

#########################################################
## Figure 6, this is generate with compareyears_free_n in stata
###################################################

diffstrengthmean <- statadata %>% group_by(year) %>% summarize(weightedmean = weighted.mean(partystrength_compareyears_free,VCF0009z,na.rm=TRUE))
nodiffstrengthmean <- statadata %>% group_by(year) %>% summarize(weightedmean = weighted.mean(abs(theta_compareyears_grm_fixed_n),VCF0009z,na.rm=TRUE))

means <- rbind(diffstrengthmean,nodiffstrengthmean)

means$group  <- factor(c(rep("Varying Relevance",18),rep("Fixed Relevance",18)),levels=c("Varying Relevance","Fixed Relevance"))


ggplot(statadata,aes(factor(year),partystrength_compareyears_free),weight=VCF0009z) + 
    geom_boxplot(col="gray40",varwidth=TRUE,coef=0,outlier.alpha=.05,outlier.size=1) +
    geom_line(data=means,aes(factor(year),weightedmean,group=group,col=group,linetype=group)) +
#    coord_cartesian(ylim=c(0,10)) + 
    ylab("Latent Partisan Strength") + 
    xlab("Year") +
    labs(caption="Data: ANES Cumulative Data File (weighted)") +
    theme_minimal()  +
    scale_linetype(name="Mean Latent Strength") +
    scale_alpha_manual(name="Mean Latent Strength",values=c(.2,1)) +
    scale_color_manual(name="Mean Latent Strength",values=c("gray20","gray60")) +
    theme(text = element_text(family="Lato"),legend.pos="bottom")

ggsave("latentpidstrength_dif.png",width=6,height=7)


#####################
### SI FIGURES
####################

################################################################
### SI Figure B.1
############################################################

rm(list=ls())
library(ggplot2)
library(RColorBrewer)
pal <- brewer.pal(3,"Spectral")
pal <- c("#43a2ca","#8856a7","#e34a33")

set.seed(10)
population <- c(rnorm(5000,mean=-2,sd=1.5),rnorm(5000,mean=2,sd=1.5))

ggdata <- data.frame(population)

ggdensity1 <- ggplot(ggdata,aes(population)) + 
  geom_density() + 
  theme_minimal() + 
  ylab("Density") + 
  xlab("") + 
  ggtitle("Latent Partisanship Time 1 - Weak Relevance") + 
  theme(text = element_text(family="Lato"))

ggdensity2 <- ggplot(ggdata,aes(population)) +
  geom_density() + 
  theme_minimal() + 
  ylab("") + 
  xlab("") + 
  ggtitle("Latent Partisanship Time 2 - Strong Relevance") + 
  theme(text = element_text(family="Lato"))

length <- 1000
x<- seq(-7.5,7.5,length.out=length)
phat.SD <- plogis(-2 - .5*x)
phat.SR <- plogis(-2 + .5*x)
phat.other <- 1-phat.SD-phat.SR

phat.SD2 <- plogis(-2 - 2*x)
phat.SR2 <- plogis(-2 + 2*x)
phat.other2 <- 1-phat.SD2-phat.SR2


ggdata <- data.frame(PID =rep(x,3),Probability=c(phat.SD,phat.SR,phat.other),Outcome=c(rep("Strong Democrat",length),rep("Strong Republican",length),rep("In-Between",length)))
ggdata$Outcome <-  factor(ggdata$Outcome ,ordered=TRUE,levels=c("Strong Democrat","In-Between","Strong Republican"))

ccc1 <- ggplot(ggdata,aes(x=PID,y=Probability,colour=Outcome,Linetype=Outcome)) + 
  geom_line() + 
  annotate("text",x=-7,y=.55,label="a=.5\nb=+/-4",colour="gray50",family="Lato") +
  ylim(0,1) + 
  scale_colour_manual(values=pal) + 
  theme_minimal() + 
  xlab("") + 
  ylab("Category Characteristic Curve") + 
  theme(legend.position="none",text = element_text(family="Lato"))

ggdata <- data.frame(PID =rep(x,3),Probability=c(phat.SD2,phat.SR2,phat.other2),Outcome=c(rep("Strong Democrat",length),rep("Strong Republican",length),rep("In-Between",length)))
ggdata$Outcome <-  factor(ggdata$Outcome ,ordered=TRUE,levels=c("Strong Democrat","In-Between","Strong Republican"))

ccc2 <- ggplot(ggdata,aes(x=PID,y=Probability,colour=Outcome)) + 
  geom_line() +
  annotate("text",x=-7,y=.55,label="a=2\nb=+/-1",colour="gray50",family="Lato") +
  ylim(0,1) + 
  scale_colour_manual(values=pal) + 
  theme_minimal() + 
  xlab("") + 
  ylab("") + 
  theme(legend.position="none",text = element_text(family="Lato"))

test <- runif(10000)

psd.pop1 <- plogis(-2-.5*population)
psr.pop1 <- plogis(-2+.5*population)
other.pop1 <- 1-psd.pop1-psr.pop1

outcome <- ifelse(test<=psd.pop1,"Strong Democrat","In-Between")
outcome <- ifelse(test>psd.pop1 & test<=(psr.pop1+psd.pop1),"Strong Republican",outcome)
outcome <- factor(outcome,ordered=TRUE,levels=c("Strong Democrat","In-Between","Strong Republican"))
testdata <- data.frame(outcome=outcome)

observed1 <- ggplot(testdata,aes(outcome,fill=outcome)) + geom_bar() + ylim(0,7000) + scale_fill_manual("",values=pal) +xlab("") + ylab("Measured Distribution") + theme_minimal() + theme(legend.position="none",text = element_text(family="Lato"))

psd.pop2 <- plogis(-2-3*population)
psr.pop2 <- plogis(-2+3*population)
other.pop2 <- 1-psd.pop2-psr.pop2

outcome <- ifelse(test<=psd.pop2,"Strong Democrat","In-Between")
outcome <- ifelse(test>psd.pop2 & test<=(psr.pop2+psd.pop2),"Strong Republican",outcome)
outcome <- factor(outcome,ordered=TRUE,levels=c("Strong Democrat","In-Between","Strong Republican"))
testdata <- data.frame(outcome)

observed2 <- ggplot(testdata,aes(outcome,fill=outcome)) + geom_bar() + ylim(0,7000) + scale_fill_manual("",values=pal) +xlab("") + ylab("") + theme_minimal() + theme(legend.position="none",text = element_text(family="Lato"))

library(gridExtra)

grid.arrange(ggdensity1,ggdensity2,ccc1,ccc2,observed1,observed2,ncol=2)
ggsave("exampleplot.png",arrangeGrob(ggdensity1,ggdensity2,ccc1,ccc2,observed1,observed2,ncol=2),width=9,height=11)

rm(list=ls())

################################
### Figure B.2 SCREE PLOT
### See estimate_figureb2tabled2.do for results
#################################

rm(list=ls())
library(ggplot2)
library(foreign)
library(RColorBrewer)

pre2000 <- c(
  4.25663,
  0.11832,
  0.04374,
 -0.05250,
 -0.07491,
 -0.08470,
 -0.11864)

post2000 <- c(
 5.25115,
 0.10198,
 0.03358,
-0.03347,
-0.04470,
-0.05440,
-0.06825)

Era <- c(rep("Pre-2000",7),rep("2000 & After",7))
Eigenvalue <- c(pre2000,post2000)
Factor <- rep(1:7,2)

ggdata <- data.frame(Eigenvalue,Factor,Era)

screeplot <- ggplot(ggdata,aes(x=Factor,y=Eigenvalue,colour=Era)) + 
    geom_hline(yintercept=0,colour="gray90") +
    geom_point() +
    geom_line() +
    theme_classic() +
    theme(text = element_text(family="Lato"),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.line.y.left=element_blank(), panel.grid.major.y=element_line(colour="gray90",linetype=1))


ggsave("screeplot.png",screeplot,width=6,height=5.25)



#################################
### Figure C.1
### Discrimination comparisons
####################################



## discrimination parameters
fixed <- c(.6874328,.9794505,.4938649,.7804294,.4353476,.7135179)
free  <- c(.5342604,.8001228,.7647918,1.118468,.4364693,.5486295,.6017198,.9781568,.3894798,.4631288,.7027868,.7428152)
Value <- c(fixed,free) 

#Group <- c(rep("Constant",6),rep(c("1952-1998","2000-2020"),6))
Group <- c(rep(c("1952-1998","2000-2020"),6))
Group <- factor(Group,ordered=TRUE, levels =c("2000-2020","1952-1998"))
#Item <- c("Pres. Therm. Diff.","Party Therm. Diff.","Party Affect","Pres. Support","House Vote","Party Strength",rep("Pres. Therm. Diff.",2),rep("Party Therm. Diff.",2),rep("Party Affect",2),rep("Pres. Support",2),rep("House Vote",2),rep("Party Strength",2))
#,rep("House Vote",2),rep("Ideology Prox",2),rep("Abortion Prox",2),rep("Gov Services Prox",2))


Item <- c(rep("Pres. Therm. Diff.",2),rep("Party Therm. Diff.",2),rep("Party Affect",2),rep("Pres. Support",2),rep("House Vote",2),rep("Party Strength",2))

Item_ordered <- c("Party Therm. Diff.","Pres. Support","Pres. Therm. Diff.","Party Strength","Party Affect","House Vote")

ggdata <- data.frame(Value=free,Era=Group,Item)



ggplot(ggdata,aes(x=Value,y=factor(Item,Item_ordered))) + 
  geom_vline(xintercept=1,colour="gray90") +
  geom_line(arrow=arrow(type="closed",length=unit(.3,"cm")),colour="gray80") +
  geom_point(aes(colour=Era)) +
  ylab("Survey Item") + 
  xlab("Discrimination Parameter Estimate") +
  theme_classic()  +
  scale_colour_viridis_d() + scale_y_discrete(drop = FALSE) +  #annotate("text",x=1.08,y=4.5,label="Party ID\nConstant at 1",colour="gray40") +
  theme(text = element_text(family="Lato"),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.line.y.left=element_blank(), panel.grid.major.y=element_line(colour="gray90",linetype=1))

ggsave(paste(outputdir,"discrimination.png",sep=""),width=6,height=5.25)


##########################
### Figure C.2
### Difficulty comparisons
##########################

rm(list=ls())
library(foreign)
library(ggplot2)
library(RColorBrewer)

cbPalette <- brewer.pal(4,"Dark2")


statadata <- read.dta("data/postcompareyear_gsem_grm_1952.dta")
summary(statadata)

difficulty1 <- unlist(statadata[1,1110:1137])
difficulty2 <- unlist(statadata[2,1110:1137])
difficulty3 <- unlist(statadata[3,1110:1137])
difficulty4 <- unlist(statadata[4,1110:1137])

diffse1 <- unlist(sqrt(statadata[1,1138:1165]))
diffse2 <- unlist(sqrt(statadata[2,1138:1165]))
diffse3 <- unlist(sqrt(statadata[3,1138:1165]))
diffse4 <- unlist(sqrt(statadata[4,1138:1165]))

measures <- c(rep("Party ID",2),rep("Therm. Diff. Pres.",8),rep("Therm. Diff. Party",8),rep("Party Affect",4),rep("Pres. Support",2),rep("House Vote",2),rep("Strong Partisan",2))

ggdata <- data.frame(Difficulty = as.vector(c(difficulty1,difficulty2,difficulty3,difficulty4)), StdErr = as.vector(c(diffse1,diffse2,diffse3,diffse4)), Measure = rep(measures,4), Estimate = c(rep("1952-98",28),rep("2000-20",28),rep("Combined",56)),Model=c(rep("Varying Relevance",56),rep("Fixed Relevance",56)))

dotplot <- ggplot(ggdata,aes(Difficulty,Estimate,group=Model,colour=Model)) +
    geom_errorbarh(aes(xmin=Difficulty-1.96*StdErr,xmax=Difficulty+1.96*StdErr),alpha=.5,height=.3) +
    geom_point() + 
    facet_grid(Measure ~ .,scales="free",space="free") +
    scale_colour_manual(name="Model",values=cbPalette[c(2,4)]) +
    ylab("Era Estimated") +
    xlab("Difficulty Parameter Estimate") + 
    theme(strip.text.y = element_text(angle = 0)) +
    theme_minimal() +
    theme(text = element_text(family="Lato"))

ggsave("compdifficulty.png",width=7,height=8)

