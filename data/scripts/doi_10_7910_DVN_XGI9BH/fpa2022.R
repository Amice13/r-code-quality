
#####load the packages#####

library(optmatch)
library(MatchIt)
library(RColorBrewer)
library(rms)
library(pscl)
library(robust)
library(stargazer)
library(jtools)
library(sandwich)
library(lmtest)
library(foreign)
library(pcse)
library(nlme)
library(car)
library(ggthemes)
library(arm)
library(sjPlot)
library(sjmisc)
library(interplot)
library(abind)
library(interactions)
library(evir)
library(broom)
library(dplyr)
library(ggplot2)
library(rlang)
library(foreign)
library(MASS)
library(sandwich)
library(Matrix)
library(detectseparation)
library(devtools)
library(foreign)
library(lmtest)
library(mvtnorm)
#devtools::install_github("chrismeserole/obsval")
library(obsval)
library(ordinal)
library(ggpubr)
library(nnet)
library(brant)

################################
#########FPA R&R 2022 ##########
################################

#####load the data#####

data1<-read.csv("C://UMD//10th semester//FPA replication//2022.csv")
nrow(data1)

#####change the reference group#####

data1$nsl<-as.factor(data1$nsl)
data1$nsl <- relevel(data1$nsl, ref = "1")
data1$xinjiang_2020<-as.factor(data1$xinjiang_2020)
data1$xinjiang_2020<-relevel(data1$xinjiang_2020,ref = "1")

#####remove data that do not have polycharchy score#####

data11<-subset(data1,data1$X2019.v2x_polyarchy!="NA")
nrow(data11)

#####transform data to numeric forms#####

data11$log.Chinese.investment.GDP.<-as.numeric(data11$log.Chinese.investment.GDP.)
data11$log.Chinese.trade.GDP.<-as.numeric(data11$log.Chinese.trade.GDP.)
data11$log.Chinese.investment.inflows.GDP.<-as.numeric(data11$log.Chinese.investment.inflows.GDP.)
data11$log.pci.1000<-as.numeric(data11$log.pci.1000)
data11$log.population.2019.million<-as.numeric(data11$log.population.2019.million)
data11$log.aid<-as.numeric(data11$log.aid)


#####Table 1: summary of statistics#####

stargazer(data11)

#####Table A2: correlation matrix#####

names(data11)
data_cor<-data11[,c(10,16,21,23,24,25,27)]
stargazer(cor(data_cor))

#####Table 2: Multinomial Logit Analysis#####

model1<-multinom(nsl~X2019.v2x_polyarchy+log.pci.1000+
                   Asia+
                   ratified_tortureprotocol+log.Chinese.trade.GDP.+log.Chinese.investment.GDP.+
                   Formal.ROC.Diplomatic.Ties.2019,
                 data=data11)

summary(model1)

z <- summary(model1)$coefficients/summary(model1)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1))*2
p

model2<-multinom(xinjiang_2020~X2019.v2x_polyarchy+log.pci.1000+
                   Asia+
                   ratified_tortureprotocol+log.Chinese.trade.GDP.+log.Chinese.investment.GDP.+
                   Formal.ROC.Diplomatic.Ties.2019,
                 data=data11)

summary(model2)

z <- summary(model2)$coefficients/summary(model2)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1))*2
p


stargazer(model1,model2,no.space = T,notes.align = "l",report =('vc*p'),
          covariate.labels = c("Polyarchy",
                               "Log PCI","Asia","UN Protocol",
                               "Log Trade","Log Investment",
                               "ROC allies",
                               "Constant"),
          title = "Multinomial Logit Analysis of Support for HK National Security Law and China's Xinjiang Policy")

#####R square#####

pR2(model1)
pR2(model2)


#####footnote 27: ordered logit and brant test#####
#https://medium.com/evangelinelee/brant-test-for-proportional-odds-in-r-b0b373a93aa2 
#https://peopleanalytics-regression-book.org/gitbook/ord-reg.html
#A p-value of less than 0.05 on this test-particularly on the Omnibus 
#plus at least one of the variables-should be interpreted as a failure 
#of the proportional odds assumption.

model11<-polr(as.factor(nsl)~X2019.v2x_polyarchy+log.pci.1000+
                Asia+
                ratified_tortureprotocol+log.Chinese.trade.GDP.+log.Chinese.investment.GDP.+
                Formal.ROC.Diplomatic.Ties.2019,
              Hess=TRUE, method="logistic",data = data11)

summary(model11)
brant(model11)

model22<-polr(as.factor(xinjiang_2020)~X2019.v2x_polyarchy+log.pci.1000+
                Asia+
                ratified_tortureprotocol+log.Chinese.trade.GDP.+log.Chinese.investment.GDP.+
                Formal.ROC.Diplomatic.Ties.2019,
              Hess=TRUE, method="logistic",data = data11)

summary(model11)
brant(model11)


#####substantive significance#####
#####for details of observed value approach, see https://github.com/chrismeserole/obsval#####

###########Figure 1: HK NSL#############

##############polyarchy##########

mean(data11$X2019.v2x_polyarchy)
mean(data11$X2019.v2x_polyarchy)-sd(data11$X2019.v2x_polyarchy)
mean(data11$X2019.v2x_polyarchy)+sd(data11$X2019.v2x_polyarchy)

mod1<- obsval(nsl~X2019.v2x_polyarchy+log.pci.1000+
                Asia+ratified_tortureprotocol+log.Chinese.trade.GDP.+log.Chinese.investment.GDP.+
                Formal.ROC.Diplomatic.Ties.2019,
              data=data11,
              reg.model = "mlogit",
              n.draws = 1000,
              effect.var = "X2019.v2x_polyarchy",
              effect.vals = c(0.2735865,0.5263626,0.7791386), # low to high
              verbose = TRUE,
              baseline.category = "1")

mod1$low.ci
mod1$means
mod1$high.ci

pred<-c(mod1$low.ci,mod1$means,mod1$high.ci)
Value<-rep(c("Mean-SD","Mean","Mean+SD"),9)
a<-rep(c("Oppose","Oppose","Oppose","Support","Support","Support","Stay Silent","Stay Silent","Stay Silent"),3)

data_boxplot_polyarchy<-data.frame(pred,Value,a)
data_boxplot_polyarchy$Value<-as.factor(data_boxplot_polyarchy$Value)

data_boxplot_polyarchy$a=factor(data_boxplot_polyarchy$a,
                                levels =c("Oppose",
                                          "Stay Silent",
                                          "Support"))

data_boxplot_polyarchy$Value=factor(data_boxplot_polyarchy$Value,
                                    levels = c("Mean-SD","Mean","Mean+SD"))



#####################trade##########

mean(data11$log.Chinese.trade.GDP.)
mean(data11$log.Chinese.trade.GDP.)+sd(data11$log.Chinese.trade.GDP.)
mean(data11$log.Chinese.trade.GDP.)-sd(data11$log.Chinese.trade.GDP.)

mod1<- obsval(nsl~X2019.v2x_polyarchy+log.pci.1000+
                Asia+ratified_tortureprotocol+log.Chinese.trade.GDP.+log.Chinese.investment.GDP.+
                Formal.ROC.Diplomatic.Ties.2019,
              data=data11,
              reg.model = "mlogit",
              n.draws = 1000,
              effect.var = "log.Chinese.trade.GDP.",
              effect.vals = c(0.9226366,1.850778,2.778919), # low to high
              verbose = TRUE,
              baseline.category = "1")

mod1$low.ci
mod1$means
mod1$high.ci

pred<-c(mod1$low.ci,mod1$means,mod1$high.ci)
Value<-rep(c("Mean-SD","Mean","Mean+SD"),9)
a<-rep(c("Oppose","Oppose","Oppose","Support","Support","Support","Stay Silent","Stay Silent","Stay Silent"),3)

data_boxplot_trade<-data.frame(pred,Value,a)
data_boxplot_trade$Value<-as.factor(data_boxplot_trade$Value)

data_boxplot_trade$a=factor(data_boxplot_trade$a,
                            levels =c("Oppose",
                                      "Stay Silent",
                                      "Support"))

data_boxplot_trade$Value=factor(data_boxplot_trade$Value,
                                levels = c("Mean-SD","Mean","Mean+SD"))


###################log_pci############
mean(data11$log.pci.1000)-sd(data11$log.pci.1000)
mean(data11$log.pci.1000)
mean(data11$log.pci.1000)+sd(data11$log.pci.1000)

mod1<- obsval(nsl~X2019.v2x_polyarchy+log.pci.1000+
                Asia+ratified_tortureprotocol+log.Chinese.trade.GDP.+log.Chinese.investment.GDP.+
                Formal.ROC.Diplomatic.Ties.2019,
              data=data11,
              reg.model = "mlogit",
              n.draws = 1000,
              effect.var = "log.pci.1000",
              effect.vals = c(0.2912889,1.718581,3.145874), # low to high
              verbose = TRUE,
              baseline.category = "1")

mod1$low.ci
mod1$means
mod1$high.ci

pred<-c(mod1$low.ci,mod1$means,mod1$high.ci)
Value<-rep(c("Mean-SD","Mean","Mean+SD"),9)
a<-rep(c("Oppose","Oppose","Oppose","Support","Support","Support","Stay Silent","Stay Silent","Stay Silent"),3)

data_boxplot_pci<-data.frame(pred,Value,a)
data_boxplot_pci$Value<-as.factor(data_boxplot_pci$Value)

data_boxplot_pci$a=factor(data_boxplot_pci$a,
                          levels =c("Oppose",
                                    "Stay Silent",
                                    "Support"))

data_boxplot_pci$Value=factor(data_boxplot_pci$Value,
                              levels = c("Mean-SD","Mean","Mean+SD"))


########investment#########
mean(data11$log.Chinese.investment.GDP.)-sd(data11$log.Chinese.investment.GDP.)
mean(data11$log.Chinese.investment.GDP.)
mean(data11$log.Chinese.investment.GDP.)+sd(data11$log.Chinese.investment.GDP.)

mod1<- obsval(nsl~X2019.v2x_polyarchy+log.pci.1000+
                Asia+ratified_tortureprotocol+log.Chinese.trade.GDP.+log.Chinese.investment.GDP.+
                Formal.ROC.Diplomatic.Ties.2019,
              data=data11,
              reg.model = "mlogit",
              n.draws = 1000,
              effect.var = "log.Chinese.investment.GDP.",
              effect.vals = c(-4.947801,-1.074131,2.799538), # low to high
              verbose = TRUE,
              baseline.category = "1")


mod1$low.ci
mod1$means
mod1$high.ci

pred<-c(mod1$low.ci,mod1$means,mod1$high.ci)
Value<-rep(c("Mean-SD","Mean","Mean+SD"),9)
a<-rep(c("Oppose","Oppose","Oppose","Support","Support","Support","Stay Silent","Stay Silent","Stay Silent"),3)

data_boxplot_investment<-data.frame(pred,Value,a)
data_boxplot_investment$Value<-as.factor(data_boxplot_investment$Value)

data_boxplot_investment$a=factor(data_boxplot_investment$a,
                                 levels =c("Oppose",
                                           "Stay Silent",
                                           "Support"))

data_boxplot_investment$Value=factor(data_boxplot_investment$Value,
                                     levels = c("Mean-SD","Mean","Mean+SD"))




######HK plot###############

pdf("2019hkmultinomial.pdf",width = 9.5,height = 10)

plot_trade<-ggplot(data_boxplot_trade, aes(x=a, y=pred,group=interaction(a,Value),fill=Value,shape=Value))+
  geom_point(position = position_dodge(width = 0.75))+
  geom_line(position = position_dodge(width = 0.75))+theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        axis.text=element_text(size=10),
        axis.title.x=element_blank(),
        plot.caption = element_text(size=10),
        legend.text = element_text(size=9),
        legend.title = element_text(size=9),
        plot.title = element_text(hjust = 0.5,size=12,face = "bold"),
        legend.spacing.x = unit(0.1, 'cm'),
        legend.spacing.y = unit(0.1,"cm"))+
  theme(legend.key.size = unit(0.54, 'cm'))+
  ylab(" ")+
  scale_y_continuous(breaks = seq(0,1,0.1),limits = c(0,1),minor_breaks = seq(0,1,.1))+
  theme(legend.position= c(.155,.75))+
  theme(panel.grid.major.x = element_blank())+
  theme(panel.grid.major.y = element_line(colour="grey"))+
  scale_fill_brewer(palette = "Set3")+
  guides(size = guide_legend(nrow = 1))+
  theme(plot.caption = element_text(hjust = 0))+
  ggtitle("A. Trade with China (log)")


plot_investment<-ggplot(data_boxplot_investment, aes(x=a, y=pred,group=interaction(a,Value),shape=Value,fill=Value))+
  geom_point(position = position_dodge(width = 0.75))+
  geom_line(position = position_dodge(width = 0.75))+theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        axis.text=element_text(size=10),
        axis.title.x=element_blank(),
        plot.caption = element_text(size=10),
        legend.text = element_text(size=9),
        legend.title = element_text(size=9),
        plot.title = element_text(hjust = 0.5,size=12,face = "bold"),
        legend.spacing.x = unit(0.11, 'cm'),
        legend.spacing.y = unit(0.1,"cm"))+
  theme(legend.key.size = unit(0.54, 'cm'))+
  ylab(" ")+
  scale_y_continuous(breaks = seq(0,1,0.1),limits = c(0,1),minor_breaks = seq(0,1,.1))+
  theme(legend.position=c(.155,.75))+
  theme(panel.grid.major.x = element_blank())+
  theme(panel.grid.major.y = element_line(colour="grey"))+
  scale_fill_brewer(palette = "Set3")+
  guides(size = guide_legend(nrow = 1))+
  theme(plot.caption = element_text(hjust = 0))+
  ggtitle("B. Chinese investment (log)")


plot_polyarchy<-ggplot(data_boxplot_polyarchy, aes(x=a, y=pred,group=interaction(a,Value),shape=Value,fill=Value))+
  geom_point(position = position_dodge(width = 0.75))+
  geom_line(position = position_dodge(width = 0.75))+theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        axis.text=element_text(size=10),
        axis.title.x=element_blank(),
        plot.caption = element_text(size=10),
        legend.text = element_text(size=9),
        legend.title = element_text(size=9),
        plot.title = element_text(hjust = 0.5,size=12,face = "bold"),
        legend.spacing.x = unit(0.1, 'cm'),
        legend.spacing.y = unit(0.1,"cm"))+
  theme(legend.key.size = unit(0.54, 'cm'))+
  ylab(" ")+
  scale_y_continuous(breaks = seq(0,1,0.1),limits = c(0,1),minor_breaks = seq(0,1,.1))+
  theme(legend.position=c(.155,.75))+
  theme(panel.grid.major.x = element_blank())+
  theme(panel.grid.major.y = element_line(colour="grey"))+
  scale_fill_brewer(palette = "Set3")+
  guides(size = guide_legend(nrow = 1))+
  theme(plot.caption = element_text(hjust = 0)) +
  ggtitle("C. Polyarchy")


plot_pci<-ggplot(data_boxplot_pci, aes(x=a, y=pred,group=interaction(a,Value),shape=Value,fill=Value))+
  geom_point(position = position_dodge(width = 0.75))+
  geom_line(position = position_dodge(width = 0.75))+theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        axis.text=element_text(size=10),
        axis.title.x=element_blank(),
        plot.caption = element_text(size=10),
        legend.text = element_text(size=9),
        legend.title = element_text(size=9),
        plot.title = element_text(hjust = 0.5,size=12,face = "bold"),
        legend.spacing.x = unit(0.1, 'cm'),
        legend.spacing.y = unit(0.1,"cm"))+
  theme(legend.key.size = unit(0.54, 'cm'))+
  ylab(" ")+
  scale_y_continuous(breaks = seq(0,1,0.1),limits = c(0,1),minor_breaks = seq(0,1,.1))+
  theme(legend.position=c(.155,.75))+
  theme(panel.grid.major.x = element_blank())+
  theme(panel.grid.major.y = element_line(colour="grey"))+
  scale_fill_brewer(palette = "Set3")+
  guides(size = guide_legend(nrow = 1))+
  theme(plot.caption = element_text(hjust = 0)) +
  ggtitle("D. Log PCI")


hk_figure<-ggarrange(plot_trade, plot_investment,
                     #plot_usbase,
                     plot_polyarchy,
                     plot_pci,
                     ncol = 2, nrow = 2)

annotate_figure(hk_figure,
                top = text_grob("Figure 1. Statistical Simulation Results of the Predicted Probabilities of Supporting HK National \n Security Law with 95% Confidence Intervals", face = "bold", size = 14),
                bottom = text_grob("Notes: Results are based on Model (1) in Table 2. Confidence intervals are calculated from 1,000 simulations.",
                                   hjust = 1, x = 1, size = 10))



dev.off()


##############Figure 2: Xinjiang################

##############polyarchy##########
mean(data11$X2019.v2x_polyarchy)-sd(data11$X2019.v2x_polyarchy)
mean(data11$X2019.v2x_polyarchy)
mean(data11$X2019.v2x_polyarchy)+sd(data11$X2019.v2x_polyarchy)

mod1<- obsval(xinjiang_2020~X2019.v2x_polyarchy+log.pci.1000+
                Asia+ratified_tortureprotocol+log.Chinese.trade.GDP.+log.Chinese.investment.GDP.+
                Formal.ROC.Diplomatic.Ties.2019,
              data=data11,
              reg.model = "mlogit",
              n.draws = 1000,
              effect.var = "X2019.v2x_polyarchy",
              effect.vals = c(0.2735865,0.5263626,0.7791386), # low to high
              verbose = TRUE,
              baseline.category = "1")

mod1$low.ci
mod1$means
mod1$high.ci

pred<-c(mod1$low.ci,mod1$means,mod1$high.ci)
Value<-rep(c("Mean-SD","Mean","Mean+SD"),9)
a<-rep(c("Oppose","Oppose","Oppose","Support","Support","Support","Stay Silent","Stay Silent","Stay Silent"),3)

data_boxplot_polyarchy<-data.frame(pred,Value,a)
data_boxplot_polyarchy$Value<-as.factor(data_boxplot_polyarchy$Value)

data_boxplot_polyarchy$a=factor(data_boxplot_polyarchy$a,
                                levels =c("Oppose",
                                          "Stay Silent",
                                          "Support"))

data_boxplot_polyarchy$Value=factor(data_boxplot_polyarchy$Value,
                                    levels = c("Mean-SD","Mean","Mean+SD"))



#####################trade##########

mean(data11$log.Chinese.trade.GDP.)-sd(data11$log.Chinese.trade.GDP.)
mean(data11$log.Chinese.trade.GDP.)
mean(data11$log.Chinese.trade.GDP.)+sd(data11$log.Chinese.trade.GDP.)

mod1<- obsval(xinjiang_2020~X2019.v2x_polyarchy+log.pci.1000+
                Asia+ratified_tortureprotocol+log.Chinese.trade.GDP.+log.Chinese.investment.GDP.+
                Formal.ROC.Diplomatic.Ties.2019,
              data=data11,
              reg.model = "mlogit",
              n.draws = 1000,
              effect.var = "log.Chinese.trade.GDP.",
              effect.vals = c(0.9226366,1.850778,2.778919), # low to high
              verbose = TRUE,
              baseline.category = "1")

mod1$low.ci
mod1$means
mod1$high.ci

a<-mod1$preds[,3,3]-mod1$preds[,3,2]
head(sort(a),26)[26] #low end
tail(sort(a),26)[1] #high end

mod1$effect.high.ci
mod1$effect.low.ci

pred<-c(mod1$low.ci,mod1$means,mod1$high.ci)
Value<-rep(c("Mean-SD","Mean","Mean+SD"),9)
a<-rep(c("Oppose","Oppose","Oppose","Support","Support","Support","Stay Silent","Stay Silent","Stay Silent"),3)

data_boxplot_trade<-data.frame(pred,Value,a)
data_boxplot_trade$Value<-as.factor(data_boxplot_trade$Value)

data_boxplot_trade$a=factor(data_boxplot_trade$a,
                            levels =c("Oppose",
                                      "Stay Silent",
                                      "Support"))

data_boxplot_trade$Value=factor(data_boxplot_trade$Value,
                                levels = c("Mean-SD","Mean","Mean+SD"))


###################log_pci############

mean(data11$log.pci.1000)-sd(data11$log.pci.1000)
mean(data11$log.pci.1000)
mean(data11$log.pci.1000)+sd(data11$log.pci.1000)

mod1<- obsval(xinjiang_2020~X2019.v2x_polyarchy+log.pci.1000+
                Asia+ratified_tortureprotocol+log.Chinese.trade.GDP.+log.Chinese.investment.GDP.+
                Formal.ROC.Diplomatic.Ties.2019,
              data=data11,
              reg.model = "mlogit",
              n.draws = 1000,
              effect.var = "log.pci.1000",
              effect.vals = c(0.2912889,1.718581,3.145874), # low to high
              verbose = TRUE,
              baseline.category = "1")

mod1$low.ci
mod1$means
mod1$high.ci

a<-mod1$preds[,3,2]-mod1$preds[,3,1]
head(sort(a),26)[26] #low end
tail(sort(a),26)[1] #high end

mod1$effect.high.ci
mod1$effect.low.ci

pred<-c(mod1$low.ci,mod1$means,mod1$high.ci)
Value<-rep(c("Mean-SD","Mean","Mean+SD"),9)
a<-rep(c("Oppose","Oppose","Oppose","Support","Support","Support","Stay Silent","Stay Silent","Stay Silent"),3)

data_boxplot_pci<-data.frame(pred,Value,a)
data_boxplot_pci$Value<-as.factor(data_boxplot_pci$Value)

data_boxplot_pci$a=factor(data_boxplot_pci$a,
                          levels =c("Oppose",
                                    "Stay Silent",
                                    "Support"))

data_boxplot_pci$Value=factor(data_boxplot_pci$Value,
                              levels = c("Mean-SD","Mean","Mean+SD"))


########investment#########
mean(data11$log.Chinese.investment.GDP.)-sd(data11$log.Chinese.investment.GDP.)
mean(data11$log.Chinese.investment.GDP.)
mean(data11$log.Chinese.investment.GDP.)+sd(data11$log.Chinese.investment.GDP.)

mod1<- obsval(xinjiang_2020~X2019.v2x_polyarchy+log.pci.1000+
                Asia+ratified_tortureprotocol+log.Chinese.trade.GDP.+log.Chinese.investment.GDP.+
                Formal.ROC.Diplomatic.Ties.2019,
              data=data11,
              reg.model = "mlogit",
              n.draws = 1000,
              effect.var = "log.Chinese.investment.GDP.",
              effect.vals = c(-4.947801,-1.074131,2.799538), # low to high
              verbose = TRUE,
              baseline.category = "1")

mod1$low.ci
mod1$means
mod1$high.ci

a<-mod1$preds[,3,2]-mod1$preds[,3,1]
head(sort(a),26)[26] #low end
tail(sort(a),26)[1] #high end

mod1$effect.high.ci
mod1$effect.low.ci

pred<-c(mod1$low.ci,mod1$means,mod1$high.ci)
Value<-rep(c("Mean-SD","Mean","Mean+SD"),9)
a<-rep(c("Oppose","Oppose","Oppose","Support","Support","Support","Stay Silent","Stay Silent","Stay Silent"),3)

data_boxplot_investment<-data.frame(pred,Value,a)
data_boxplot_investment$Value<-as.factor(data_boxplot_investment$Value)

data_boxplot_investment$a=factor(data_boxplot_investment$a,
                                 levels =c("Oppose",
                                           "Stay Silent",
                                           "Support"))

data_boxplot_investment$Value=factor(data_boxplot_investment$Value,
                                     levels = c("Mean-SD","Mean","Mean+SD"))



######XJ plot###############

pdf("2019xjmultinomial.pdf",width = 9.5,height = 10)

plot_trade<-ggplot(data_boxplot_trade, aes(x=a, y=pred,group=interaction(a,Value),fill=Value,shape=Value))+
  geom_point(position = position_dodge(width = 0.75))+
  geom_line(position = position_dodge(width = 0.75))+theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        axis.text=element_text(size=10),
        axis.title.x=element_blank(),
        plot.caption = element_text(size=10),
        legend.text = element_text(size=9),
        legend.title = element_text(size=9),
        plot.title = element_text(hjust = 0.5,size=12,face = "bold"),
        legend.spacing.x = unit(0.1, 'cm'),
        legend.spacing.y = unit(0.1,"cm"))+
  theme(legend.key.size = unit(0.54, 'cm'))+
  ylab(" ")+
  scale_y_continuous(breaks = seq(0,1,0.1),limits = c(0,1),minor_breaks = seq(0,1,.1))+
  theme(legend.position= c(.155,.75))+
  theme(panel.grid.major.x = element_blank())+
  theme(panel.grid.major.y = element_line(colour="grey"))+
  scale_fill_brewer(palette = "Set3")+
  guides(size = guide_legend(nrow = 1))+
  theme(plot.caption = element_text(hjust = 0))+
  ggtitle("A. Trade with China (log)")


plot_investment<-ggplot(data_boxplot_investment, aes(x=a, y=pred,group=interaction(a,Value),shape=Value,fill=Value))+
  geom_point(position = position_dodge(width = 0.75))+
  geom_line(position = position_dodge(width = 0.75))+theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        axis.text=element_text(size=10),
        axis.title.x=element_blank(),
        plot.caption = element_text(size=10),
        legend.text = element_text(size=9),
        legend.title = element_text(size=9),
        plot.title = element_text(hjust = 0.5,size=12,face = "bold"),
        legend.spacing.x = unit(0.11, 'cm'),
        legend.spacing.y = unit(0.1,"cm"))+
  theme(legend.key.size = unit(0.54, 'cm'))+
  ylab(" ")+
  scale_y_continuous(breaks = seq(0,1,0.1),limits = c(0,1),minor_breaks = seq(0,1,.1))+
  theme(legend.position=c(.155,.75))+
  theme(panel.grid.major.x = element_blank())+
  theme(panel.grid.major.y = element_line(colour="grey"))+
  scale_fill_brewer(palette = "Set3")+
  guides(size = guide_legend(nrow = 1))+
  theme(plot.caption = element_text(hjust = 0))+
  ggtitle("B. Chinese investment (log)")


plot_polyarchy<-ggplot(data_boxplot_polyarchy, aes(x=a, y=pred,group=interaction(a,Value),shape=Value,fill=Value))+
  geom_point(position = position_dodge(width = 0.75))+
  geom_line(position = position_dodge(width = 0.75))+theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        axis.text=element_text(size=10),
        axis.title.x=element_blank(),
        plot.caption = element_text(size=10),
        legend.text = element_text(size=9),
        legend.title = element_text(size=9),
        plot.title = element_text(hjust = 0.5,size=12,face = "bold"),
        legend.spacing.x = unit(0.1, 'cm'),
        legend.spacing.y = unit(0.1,"cm"))+
  theme(legend.key.size = unit(0.54, 'cm'))+
  ylab(" ")+
  scale_y_continuous(breaks = seq(0,1,0.1),limits = c(0,1),minor_breaks = seq(0,1,.1))+
  theme(legend.position=c(.155,.75))+
  theme(panel.grid.major.x = element_blank())+
  theme(panel.grid.major.y = element_line(colour="grey"))+
  scale_fill_brewer(palette = "Set3")+
  guides(size = guide_legend(nrow = 1))+
  theme(plot.caption = element_text(hjust = 0)) +
  ggtitle("C. Polyarchy")


plot_pci<-ggplot(data_boxplot_pci, aes(x=a, y=pred,group=interaction(a,Value),shape=Value,fill=Value))+
  geom_point(position = position_dodge(width = 0.75))+
  geom_line(position = position_dodge(width = 0.75))+theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        axis.text=element_text(size=10),
        axis.title.x=element_blank(),
        plot.caption = element_text(size=10),
        legend.text = element_text(size=9),
        legend.title = element_text(size=9),
        plot.title = element_text(hjust = 0.5,size=12,face = "bold"),
        legend.spacing.x = unit(0.1, 'cm'),
        legend.spacing.y = unit(0.1,"cm"))+
  theme(legend.key.size = unit(0.54, 'cm'))+
  ylab(" ")+
  scale_y_continuous(breaks = seq(0,1,0.1),limits = c(0,1),minor_breaks = seq(0,1,.1))+
  theme(legend.position=c(.155,.75))+
  theme(panel.grid.major.x = element_blank())+
  theme(panel.grid.major.y = element_line(colour="grey"))+
  scale_fill_brewer(palette = "Set3")+
  guides(size = guide_legend(nrow = 1))+
  theme(plot.caption = element_text(hjust = 0)) +
  ggtitle("D. Log PCI")


xj_figure<-ggarrange(plot_trade, plot_investment,
                     #plot_usbase,
                     plot_polyarchy,
                     plot_pci,
                     ncol = 2, nrow = 2)

annotate_figure(xj_figure,
                top = text_grob("Figure 2. Statistical Simulation Results of the Predicted Probabilities of Supporting China's \n Xinjiang Policy with 95% Confidence Intervals", face = "bold", size = 14),
                bottom = text_grob("Notes: Results are based on Model (2) in Table 2. Confidence intervals are calculated from 1,000 simulations.",
                                   hjust = 1, x = 1, size = 10))



dev.off()


########################
######Table A3: aid#####
########################

data5<-subset(data11,data11$log.aid!="#N/A")
nrow(data5)

model1<-glm(nsl_support~log.aid+log.Chinese.trade.GDP.+log.Chinese.investment.GDP.+
              X2019.v2x_polyarchy+Asia+log.pci.1000+
              #Formal.ROC.Diplomatic.Ties.2019+
              ratified_tortureprotocol,
            family = binomial(link = "logit"),
            data = data5)

summary(model1)


model2<-glm(xinjiang_2020_support~log.aid+log.Chinese.trade.GDP.+log.Chinese.investment.GDP.+
              X2019.v2x_polyarchy+Asia+log.pci.1000+
              # Formal.ROC.Diplomatic.Ties.2019+
              ratified_tortureprotocol,
            family = binomial(link = "logit"),
            data = data5)

summary(model2)

stargazer(model1,model2,no.space = T,
          notes.align = "l",report =('vc*p'),
          title = "Binomial Logit Analysis of the Effect of Aid on Supporting HK National Security Law and China's Xinjiang Policy")


pR2(model1)
pR2(model2)


##################################
#########Table A4: BRI############
##################################

model1<-multinom(nsl~X2019.v2x_polyarchy+log.pci.1000+BRI+
                   Asia+ratified_tortureprotocol+log.Chinese.trade.GDP.+log.Chinese.investment.GDP.+
                   Formal.ROC.Diplomatic.Ties.2019,
                 data=data11)

summary(model1)

z <- summary(model1)$coefficients/summary(model1)$standard.errors
# 2-tailed Wald z tests to test significance of coefficients
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

model2<-multinom(xinjiang_2020~X2019.v2x_polyarchy+log.pci.1000+BRI+
                   Asia+ratified_tortureprotocol+log.Chinese.trade.GDP.+log.Chinese.investment.GDP.+
                   Formal.ROC.Diplomatic.Ties.2019,
                 data=data11)

summary(model2)

z <- summary(model2)$coefficients/summary(model2)$standard.errors
# 2-tailed Wald z tests to test significance of coefficients
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

stargazer(model1,model2,no.space = T,notes.align = "l",report =('vc*p'),
          covariate.labels = c("Polyarchy",
                               "Log PCI","BRI",
                               "Asia","UN Protocol",
                               "Log Trade","Log Investment",
                               "ROC allies",
                               "Constant"),
          title = "Multinomial Logit Analysis of the Effect of BRI on Support for HK National Security Law and China's Xinjiang Policy")

pR2(model1)
pR2(model2)


###########################################
########Table A5: Muslim population########
###########################################

model1<-multinom(xinjiang_2020~X2019.v2x_polyarchy+log.pci.1000+
                   Asia+ratified_tortureprotocol+log.Chinese.trade.GDP.+log.Chinese.investment.GDP.+
                   Formal.ROC.Diplomatic.Ties.2019+muslim_2010_1,
                 data=data11)

summary(model1)

z <- summary(model1)$coefficients/summary(model1)$standard.errors
# 2-tailed Wald z tests to test significance of coefficients
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

model2<-multinom(xinjiang_2020~X2019.v2x_polyarchy+log.pci.1000+
                   Asia+ratified_tortureprotocol+log.Chinese.trade.GDP.+log.Chinese.investment.GDP.+
                   Formal.ROC.Diplomatic.Ties.2019+muslim_majority,
                 data=data11)

summary(model2)

z <- summary(model2)$coefficients/summary(model2)$standard.errors
# 2-tailed Wald z tests to test significance of coefficients
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

stargazer(model1,model2,no.space = T,notes.align = "l",report =('vc*p'),
          
          title = "Multinomial Logit Analysis of the Effect of Muslim Population on Support for China's Xinjiang Policy")

pR2(model1)
pR2(model2)

####################################
####Table A6: FDI inflows 5 year####
####################################

model1<-multinom(nsl~X2019.v2x_polyarchy+log.pci.1000+
                   Asia+ratified_tortureprotocol+log.Chinese.trade.GDP.+
                   log.Chinese.investment.inflows.GDP.+
                   Formal.ROC.Diplomatic.Ties.2019,
                 data=data11)

summary(model1)

z <- summary(model1)$coefficients/summary(model1)$standard.errors
# 2-tailed Wald z tests to test significance of coefficients
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p


model2<-multinom(xinjiang_2020~X2019.v2x_polyarchy+log.pci.1000+
                   Asia+ratified_tortureprotocol+log.Chinese.trade.GDP.+
                   log.Chinese.investment.inflows.GDP.+
                   Formal.ROC.Diplomatic.Ties.2019,
                 data=data11)

summary(model2)

z <- summary(model2)$coefficients/summary(model2)$standard.errors
# 2-tailed Wald z tests to test significance of coefficients
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p


stargazer(model1,model2,no.space = T,notes.align = "l",report =('vc*p'),
          covariate.labels = c("Polyarchy",
                               "Log PCI","Asia","UN Protocol",
                               "Log Trade","Log Investment inflows",
                               "ROC allies",
                               "Constant"),
          title = "Multinomial Logit Analysis of Support for HK National Security Law and China's Xinjiang Policy")

pR2(model1)
pR2(model2)

#########################################
########Table A7: theorize silence#######
#########################################

model1<-glm(nsl_silence~X2019.v2x_polyarchy+log.pci.1000+
              Asia+ratified_tortureprotocol+log.Chinese.trade.GDP.+log.Chinese.investment.GDP.+
              Formal.ROC.Diplomatic.Ties.2019+
              X2019.v2x_polyarchy*log.pci.1000,
            data=data11,family = binomial(link = "logit"))

model2<-glm(xinjiang_2020_silence~X2019.v2x_polyarchy+log.pci.1000+
              Asia+ratified_tortureprotocol+log.Chinese.trade.GDP.+log.Chinese.investment.GDP.+
              Formal.ROC.Diplomatic.Ties.2019+
              X2019.v2x_polyarchy*log.pci.1000,
            data=data11,family = binomial(link = "logit"))

model3<-glm(nsl_silence~X2019.v2x_polyarchy+log.pci.1000+
              Asia+ratified_tortureprotocol+log.Chinese.trade.GDP.+log.Chinese.investment.GDP.+
              Formal.ROC.Diplomatic.Ties.2019+
              X2019.v2x_polyarchy*log.Chinese.trade.GDP.,
            data=data11,family = binomial(link = "logit"))

model4<-glm(xinjiang_2020_silence~X2019.v2x_polyarchy+log.pci.1000+
              Asia+ratified_tortureprotocol+log.Chinese.trade.GDP.+log.Chinese.investment.GDP.+
              Formal.ROC.Diplomatic.Ties.2019+
              X2019.v2x_polyarchy*log.Chinese.trade.GDP.,
            data=data11,family = binomial(link = "logit"))


model5<-glm(nsl_silence~X2019.v2x_polyarchy+log.pci.1000+
              Asia+ratified_tortureprotocol+log.Chinese.trade.GDP.+log.Chinese.investment.GDP.+
              Formal.ROC.Diplomatic.Ties.2019+
              X2019.v2x_polyarchy*log.Chinese.investment.GDP.,
            data=data11,family = binomial(link = "logit"))

model6<-glm(xinjiang_2020_silence~X2019.v2x_polyarchy+log.pci.1000+
              Asia+ratified_tortureprotocol+log.Chinese.trade.GDP.+log.Chinese.investment.GDP.+
              Formal.ROC.Diplomatic.Ties.2019+
              X2019.v2x_polyarchy*log.Chinese.investment.GDP.,
            data=data11,family = binomial(link = "logit"))


stargazer(model1,model2,model3,model4,model5,model6,no.space = T,
          notes.align = "l",report =('vc*p'),
          
          title = "Binomial Logit Analysis of Being Silent on HK National Security Law and China's Xinjiang Policy")

pR2(model1)
pR2(model2)
pR2(model3)
pR2(model4)
pR2(model5)
pR2(model6)


#####interaction predicted prob plot#####

library(interplot)

#https://cran.r-project.org/web/packages/interplot/vignettes/interplot-vignette.html


pdf("nsl_interactive_pred_probability1.pdf",width = 9.5,height = 12)

pp1 <- interplot(model1, var1 = "X2019.v2x_polyarchy", ci=.9,
                 var2 = "log.pci.1000", predPro = TRUE,
                 var2_vals = c(mean(data11$log.pci.1000)-sd(data11$log.pci.1000),
                               mean(data11$log.pci.1000)+sd(data11$log.pci.1000)))+
  xlab("Polyarchy")+
  ggtitle("a(1). Countries with Different Log PCI")+
  theme_bw()+
  theme(plot.title  = element_text(hjust = 0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey"))+
  scale_y_continuous(breaks = seq(0,100,10),limits = c(0,100),minor_breaks = seq(0,100,10))+
  scale_x_continuous(breaks = seq(0,.92,.1),limits = c(0,.92),minor_breaks = seq(0,.92,.1))+
  scale_colour_discrete(guide = guide_legend(title = "Log PCI"), labels = c("Mean-sd", "Mean+sd")) +
  scale_fill_grey(guide = guide_legend(title = "Log PCI"), labels = c("Mean-sd", "Mean+sd")) +
  theme(legend.position = c(0.67, 0.17),
        legend.title = element_text(size=8.5),
        legend.text = element_text(size=8),
        legend.key.size = unit(.5, 'cm'))

pp2<-interplot(model1,var2 = "X2019.v2x_polyarchy", ci=.9,
               var1 = "log.pci.1000", predPro = TRUE, 
               var2_vals = c(mean(data11$X2019.v2x_polyarchy)-sd(data11$X2019.v2x_polyarchy),
                             mean(data11$X2019.v2x_polyarchy)+sd(data11$X2019.v2x_polyarchy)))+
  xlab("Log PCI")+
  ggtitle("a(2). Countries with Different Polyarchy")+
  theme_bw()+
  theme(plot.title  = element_text(hjust = 0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey"))+
  scale_y_continuous(breaks = seq(0,100,10),limits = c(0,100),minor_breaks = seq(0,100,10))+
  scale_x_continuous(breaks = seq(-1.5,4.8,1),limits = c(-1.5,4.8),minor_breaks = seq(-1.5,4.8,1))+
  scale_colour_discrete(guide = guide_legend(title = "Polyarchy"), labels = c("Mean-sd", "Mean+sd")) +
  scale_fill_grey(guide = guide_legend(title = "Polyarchy"), labels = c("Mean-sd", "Mean+sd")) +
  theme(legend.position = c(0.67, 0.17),
        legend.title = element_text(size=8.5),
        legend.text = element_text(size=8),
        legend.key.size = unit(.5, 'cm'))


nsl_interaction_plot_model1<-ggarrange(pp1,pp2,ncol = 2, nrow = 1)

group1<-annotate_figure(nsl_interaction_plot_model1,
                bottom = text_grob("Notes: Plot a(1) and a(2) are based on Model (1) in Table A7."))+
                theme(plot.margin = unit(c(.9,.9,.9,.9), "lines"))


pp3 <- interplot(model3, var1 = "X2019.v2x_polyarchy", ci=.9,
                 var2 = "log.Chinese.trade.GDP.", predPro = TRUE, 
                 var2_vals = c(mean(data11$log.Chinese.trade.GDP.)-sd(data11$log.Chinese.trade.GDP.),
                               mean(data11$log.Chinese.trade.GDP.)+sd(data11$log.Chinese.trade.GDP.)))+
  xlab("Polyarchy")+
  ggtitle("b(1). Countries with Different Trade")+
  theme_bw()+
  theme(plot.title  = element_text(hjust = 0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey"))+
  scale_y_continuous(breaks = seq(0,100,10),limits = c(0,100),minor_breaks = seq(0,100,10))+
  scale_x_continuous(breaks = seq(0,.92,.1),limits = c(0,.92),minor_breaks = seq(0,.92,.1))+
  scale_colour_discrete(guide = guide_legend(title = "Log Trade"), labels = c("Mean-sd", "Mean+sd")) +
  scale_fill_grey(guide = guide_legend(title = "Log Trade"), labels = c("Mean-sd", "Mean+sd")) +
  theme(legend.position = c(0.4, 0.8),
        legend.title = element_text(size=8.5),
        legend.text = element_text(size=8),
        legend.key.size = unit(.5, 'cm'))


pp4<-interplot(model3,var2 = "X2019.v2x_polyarchy", ci=.9,
               var1 = "log.Chinese.trade.GDP.", predPro = TRUE, 
               var2_vals = c(mean(data11$X2019.v2x_polyarchy)-sd(data11$X2019.v2x_polyarchy),
                             mean(data11$X2019.v2x_polyarchy)+sd(data11$X2019.v2x_polyarchy)))+
  xlab("Log Trade")+
  ggtitle("b(2). Countries with Different Polyarchy")+
  theme_bw()+
  theme(plot.title  = element_text(hjust = 0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey"))+
  scale_y_continuous(breaks = seq(0,100,10),limits = c(0,100),minor_breaks = seq(0,100,10))+
  scale_x_continuous(breaks = seq(-.9,5,1),limits = c(-.9,5),minor_breaks = seq(-.9,5,1))+
  scale_colour_discrete(guide = guide_legend(title = "Polyarchy"), labels = c("Mean-sd", "Mean+sd")) +
  scale_fill_grey(guide = guide_legend(title = "Polyarchy"), labels = c("Mean-sd", "Mean+sd")) +
  theme(legend.position = c(0.4, 0.8),
        legend.title = element_text(size=8.5),
        legend.text = element_text(size=8),
        legend.key.size = unit(.5, 'cm'))


nsl_interaction_plot_model3<-ggarrange(pp3,pp4,ncol = 2, nrow = 1)

group2<-annotate_figure(nsl_interaction_plot_model3,
                        bottom = text_grob("Notes: Plot b(1) and b(2) are based on Model (3) in Table A7."))+
                        theme(plot.margin = unit(c(.9,.9,.9,.9), "lines"))


pp5 <- interplot(model5, var1 = "X2019.v2x_polyarchy", ci=.9,
                 var2 = "log.Chinese.investment.GDP.", predPro = TRUE, 
                 var2_vals = c(mean(data11$log.Chinese.investment.GDP.)-sd(data11$log.Chinese.investment.GDP.),
                               mean(data11$log.Chinese.investment.GDP.)+sd(data11$log.Chinese.investment.GDP.)))+
  xlab("Polyarchy")+
  ggtitle("c(1). Countries with Different Investment")+
  theme_bw()+
  theme(plot.title  = element_text(hjust = 0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey"))+
  scale_y_continuous(breaks = seq(0,100,10),limits = c(0,100),minor_breaks = seq(0,100,10))+
  scale_x_continuous(breaks = seq(0,.92,.1),limits = c(0,.92),minor_breaks = seq(0,.92,.1))+
  scale_colour_discrete(guide = guide_legend(title = "Log Investment"), labels = c("Mean-sd", "Mean+sd")) +
  scale_fill_grey(guide = guide_legend(title = "Log Investment"), labels = c("Mean-sd", "Mean+sd")) +
  theme(legend.position = c(0.57, 0.82),
        legend.title = element_text(size=8.5),
        legend.text = element_text(size=8),
        legend.key.size = unit(.5, 'cm'))


pp6<-interplot(model5,var2 = "X2019.v2x_polyarchy", ci=.9,
               var1 = "log.Chinese.investment.GDP.", predPro = TRUE, 
               var2_vals = c(mean(data11$X2019.v2x_polyarchy)-sd(data11$X2019.v2x_polyarchy),
                             mean(data11$X2019.v2x_polyarchy)+sd(data11$X2019.v2x_polyarchy)))+
  xlab("Log Investment")+
  ggtitle("c(2). Countries with Different Polyarchy")+
  theme_bw()+
  theme(plot.title  = element_text(hjust = 0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey"))+
  scale_y_continuous(breaks = seq(0,100,10),limits = c(0,100),minor_breaks = seq(0,100,10))+
  scale_x_continuous(breaks = seq(-20,4,2),limits = c(-20,4),minor_breaks = seq(-20,4,2))+
  scale_colour_discrete(guide = guide_legend(title = "Polyarchy"), labels = c("Mean-sd", "Mean+sd")) +
  scale_fill_grey(guide = guide_legend(title = "Polyarchy"), labels = c("Mean-sd", "Mean+sd")) +
  theme(legend.position = c(0.8, 0.8),
        legend.title = element_text(size=8.5),
        legend.text = element_text(size=8),
        legend.key.size = unit(.5, 'cm'))


nsl_interaction_plot_model5<-ggarrange(pp5,pp6,ncol = 2, nrow = 1)

group3<-annotate_figure(nsl_interaction_plot_model5,
                        bottom = text_grob("Notes: Plot c(1) and c(2) are based on Model (5) in Table A7."
                                           ))+
  theme(plot.margin = unit(c(.9,.9,.9,.9), "lines"))


nsl_conditional_pred_prob<-ggarrange(group1,group2,group3,
                                    ncol = 1, nrow = 3)


annotate_figure(nsl_conditional_pred_prob,
                top = text_grob("Figure A1.1. Conditional Predicted Probabilities on Being Silent on Hong Kong National \nSecurity Law with 90% Confidence Intervals", face = "bold", size = 14))


dev.off()



pdf("xj_interactive_pred_probability1.pdf",width = 9.5,height = 12)

pp1 <- interplot(model2, var1 = "X2019.v2x_polyarchy", ci=.9,
                 var2 = "log.pci.1000", predPro = TRUE, 
                 var2_vals = c(mean(data11$log.pci.1000)-sd(data11$log.pci.1000),
                               mean(data11$log.pci.1000)+sd(data11$log.pci.1000)))+
  xlab("Polyarchy")+
  ggtitle("a(1). Countries with Different Log PCI")+
  theme_bw()+
  theme(plot.title  = element_text(hjust = 0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey"))+
  scale_y_continuous(breaks = seq(0,100,10),limits = c(0,100),minor_breaks = seq(0,100,10))+
  scale_x_continuous(breaks = seq(0,.92,.1),limits = c(0,.92),minor_breaks = seq(0,.92,.1))+
  scale_colour_discrete(guide = guide_legend(title = "Log PCI"), labels = c("Mean-sd", "Mean+sd")) +
  scale_fill_grey(guide = guide_legend(title = "Log PCI"), labels = c("Mean-sd", "Mean+sd")) +
  theme(legend.position = c(0.67, 0.17),
        legend.title = element_text(size=8.5),
        legend.text = element_text(size=8),
        legend.key.size = unit(.5, 'cm'))


pp2<-interplot(model2,var2 = "X2019.v2x_polyarchy", ci=.9,
               var1 = "log.pci.1000", predPro = TRUE, 
               var2_vals = c(mean(data11$X2019.v2x_polyarchy)-sd(data11$X2019.v2x_polyarchy),
                             mean(data11$X2019.v2x_polyarchy)+sd(data11$X2019.v2x_polyarchy)))+
  xlab("Log PCI")+
  ggtitle("a(2). Countries with Different Polyarchy")+
  theme_bw()+
  theme(plot.title  = element_text(hjust = 0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey"))+
  scale_y_continuous(breaks = seq(0,100,10),limits = c(0,100),minor_breaks = seq(0,100,10))+
  scale_x_continuous(breaks = seq(-1.5,4.8,1),limits = c(-1.5,4.8),minor_breaks = seq(-1.5,4.8,1))+
  scale_colour_discrete(guide = guide_legend(title = "Polyarchy"), labels = c("Mean-sd", "Mean+sd")) +
  scale_fill_grey(guide = guide_legend(title = "Polyarchy"), labels = c("Mean-sd", "Mean+sd")) +
  theme(legend.position = c(0.17, 0.61),
        legend.title = element_text(size=8.5),
        legend.text = element_text(size=8),
        legend.key.size = unit(.5, 'cm'))

nsl_interaction_plot_model1<-ggarrange(pp1,pp2,ncol = 2, nrow = 1)

group1<-annotate_figure(nsl_interaction_plot_model1,
                        bottom = text_grob("Notes: Plot a(1) and a(2) are based on Model (2) in Table A7."))+
  theme(plot.margin = unit(c(.9,.9,.9,.9), "lines"))

pp3 <- interplot(model4, var1 = "X2019.v2x_polyarchy", ci=.9,
                 var2 = "log.Chinese.trade.GDP.", predPro = TRUE, 
                 var2_vals = c(mean(data11$log.Chinese.trade.GDP.)-sd(data11$log.Chinese.trade.GDP.),
                               mean(data11$log.Chinese.trade.GDP.)+sd(data11$log.Chinese.trade.GDP.)))+
  xlab("Polyarchy")+
  ggtitle("b(1). Countries with Different Trade")+
  theme_bw()+
  theme(plot.title  = element_text(hjust = 0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey"))+
  scale_y_continuous(breaks = seq(0,100,10),limits = c(0,100),minor_breaks = seq(0,100,10))+
  scale_x_continuous(breaks = seq(0,.92,.1),limits = c(0,.92),minor_breaks = seq(0,.92,.1))+
  scale_colour_discrete(guide = guide_legend(title = "Log Trade"), labels = c("Mean-sd", "Mean+sd")) +
  scale_fill_grey(guide = guide_legend(title = "Log Trade"), labels = c("Mean-sd", "Mean+sd")) +
  theme(legend.position = c(0.4, 0.82),
        legend.title = element_text(size=8.5),
        legend.text = element_text(size=8),
        legend.key.size = unit(.5, 'cm'))


pp4<-interplot(model4,var2 = "X2019.v2x_polyarchy", ci=.9,
               var1 = "log.Chinese.trade.GDP.", predPro = TRUE, 
               var2_vals = c(mean(data11$X2019.v2x_polyarchy)-sd(data11$X2019.v2x_polyarchy),
                             mean(data11$X2019.v2x_polyarchy)+sd(data11$X2019.v2x_polyarchy)))+
  xlab("Log Trade")+
  ggtitle("b(2). Countries with Different Polyarchy")+
  theme_bw()+
  theme(plot.title  = element_text(hjust = 0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey"))+
  scale_y_continuous(breaks = seq(0,100,10),limits = c(0,100),minor_breaks = seq(0,100,10))+
  scale_x_continuous(breaks = seq(-.9,5,1),limits = c(-.9,5),minor_breaks = seq(-.9,5,1))+
  scale_colour_discrete(guide = guide_legend(title = "Polyarchy"), labels = c("Mean-sd", "Mean+sd")) +
  scale_fill_grey(guide = guide_legend(title = "Polyarchy"), labels = c("Mean-sd", "Mean+sd")) +
  theme(legend.position = c(0.4, 0.82),
        legend.title = element_text(size=8.5),
        legend.text = element_text(size=8),
        legend.key.size = unit(.5, 'cm'))


nsl_interaction_plot_model3<-ggarrange(pp3,pp4,ncol = 2, nrow = 1)

group2<-annotate_figure(nsl_interaction_plot_model3,
                        bottom = text_grob("Notes: Plot b(1) and b(2) are based on Model (4) in Table A7."))+
  theme(plot.margin = unit(c(.9,.9,.9,.9), "lines"))


pp5 <- interplot(model6, var1 = "X2019.v2x_polyarchy", ci=.9,
                 var2 = "log.Chinese.investment.GDP.", predPro = TRUE, 
                 var2_vals = c(mean(data11$log.Chinese.investment.GDP.)-sd(data11$log.Chinese.investment.GDP.),
                               mean(data11$log.Chinese.investment.GDP.)+sd(data11$log.Chinese.investment.GDP.)))+
  xlab("Polyarchy")+
  ggtitle("c(1). Countries with Different Investment")+
  theme_bw()+
  theme(plot.title  = element_text(hjust = 0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey"))+
  scale_y_continuous(breaks = seq(0,100,10),limits = c(0,100),minor_breaks = seq(0,100,10))+
  scale_x_continuous(breaks = seq(0,.92,.1),limits = c(0,.92),minor_breaks = seq(0,.92,.1))+
  scale_colour_discrete(guide = guide_legend(title = "Log Investment"), labels = c("Mean-sd", "Mean+sd")) +
  scale_fill_grey(guide = guide_legend(title = "Log Investment"), labels = c("Mean-sd", "Mean+sd")) +
  theme(legend.position = c(0.75, 0.17),
        legend.title = element_text(size=8.5),
        legend.text = element_text(size=8),
        legend.key.size = unit(.5, 'cm'))


pp6<-interplot(model6,var2 = "X2019.v2x_polyarchy", ci=.9,
               var1 = "log.Chinese.investment.GDP.", predPro = TRUE, 
               var2_vals = c(mean(data11$X2019.v2x_polyarchy)-sd(data11$X2019.v2x_polyarchy),
                             mean(data11$X2019.v2x_polyarchy)+sd(data11$X2019.v2x_polyarchy)))+
  xlab("Log Investment")+
  ggtitle("c(2). Countries with Different Polyarchy")+
  theme_bw()+
  theme(plot.title  = element_text(hjust = 0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey"))+
  scale_y_continuous(breaks = seq(0,100,10),limits = c(0,100),minor_breaks = seq(0,100,10))+
  scale_x_continuous(breaks = seq(-20,4,2),limits = c(-20,4),minor_breaks = seq(-20,4,2))+
  scale_colour_discrete(guide = guide_legend(title = "Polyarchy"), labels = c("Mean-sd", "Mean+sd")) +
  scale_fill_grey(guide = guide_legend(title = "Polyarchy"), labels = c("Mean-sd", "Mean+sd")) +
  theme(legend.position = c(0.65, 0.17),
        legend.title = element_text(size=8.5),
        legend.text = element_text(size=8),
        legend.key.size = unit(.5, 'cm'))


nsl_interaction_plot_model5<-ggarrange(pp5,pp6,ncol = 2, nrow = 1)

group3<-annotate_figure(nsl_interaction_plot_model5,
                        bottom = text_grob("Notes: Plot c(1) and c(2) are based on Model (6) in Table A7."
                        ))+
  theme(plot.margin = unit(c(.9,.9,.9,.9), "lines"))


nsl_conditional_pred_prob<-ggarrange(group1,group2,group3,
                                     ncol = 1, nrow = 3)


annotate_figure(nsl_conditional_pred_prob,
                top = text_grob("Figure A1.2. Conditional Predicted Probabilities on Being Silent on China's Xinjiang \nPolicy with 90% Confidence Intervals", face = "bold", size = 14))


dev.off()

citation("interplot")

##############################################################
#####Table A8: Binomial logit analysis with interactions######
##############################################################

model1<-glm(nsl_support~X2019.v2x_polyarchy+log.pci.1000+
              Asia+ratified_tortureprotocol+log.Chinese.trade.GDP.+log.Chinese.investment.GDP.+
              Formal.ROC.Diplomatic.Ties.2019+
              X2019.v2x_polyarchy*log.Chinese.trade.GDP.+
              X2019.v2x_polyarchy*log.Chinese.investment.GDP.,
            data=data11,family = binomial(link = "logit"))
summary(model1)

model2<-glm(xinjiang_2020_support~X2019.v2x_polyarchy+log.pci.1000+
              Asia+ratified_tortureprotocol+log.Chinese.trade.GDP.+log.Chinese.investment.GDP.+
              Formal.ROC.Diplomatic.Ties.2019+
              X2019.v2x_polyarchy*log.Chinese.trade.GDP.+
              X2019.v2x_polyarchy*log.Chinese.investment.GDP.,
            data=data11,family = binomial(link = "logit"))

summary(model2)

nsl_support_silent<-1-data11$nsl_oppose
nsl_support_silent
#note: countries that were supportive of/silent on China's Xinjiang policy and 
#countries that were supportive of/silent on HK NSL are the same

model3<-glm(nsl_support_silent~X2019.v2x_polyarchy+log.pci.1000+
              Asia+ratified_tortureprotocol+log.Chinese.trade.GDP.+log.Chinese.investment.GDP.+
              Formal.ROC.Diplomatic.Ties.2019+
              X2019.v2x_polyarchy*log.Chinese.trade.GDP.+
              X2019.v2x_polyarchy*log.Chinese.investment.GDP.,
            data=data11,family = binomial(link = "logit"))

summary(model3)

stargazer(model1,model2,model3,no.space = T,
          notes.align = "l",report =('vc*p'),
          title = "Binomial Logit Analysis of Supporting HK National Security Law and China's Xinjiang Policy")

pR2(model1)
pR2(model2)
pR2(model3)

#####Figure A2#####

plot1<-interplot(model1,var2 = "X2019.v2x_polyarchy",ci=.9,
                 var1 = "log.Chinese.trade.GDP.")+
                 xlab("Polyarchy")+
  ggtitle("1(a). Coefficient of Log Trade")+
  theme_bw()+
  geom_hline(yintercept = 0,linetype="dashed")+
  theme(plot.title  = element_text(hjust = 0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey"))+
  scale_x_continuous(breaks = seq(0,.92,.1),limits = c(0,.92),minor_breaks = seq(0,.92,.1))

plot2<-interplot(model1,var2 = "X2019.v2x_polyarchy",ci=.9,
                 var1 = "log.Chinese.investment.GDP.")+
  xlab("Polyarchy")+
  ggtitle("1(b). Coefficient of Log Investment")+
  theme_bw()+
  geom_hline(yintercept = 0,linetype="dashed")+
  theme(plot.title  = element_text(hjust = 0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey"))+
  scale_x_continuous(breaks = seq(0,.92,.1),limits = c(0,.92),minor_breaks = seq(0,.92,.1))



nsl_support<-ggarrange(plot1,plot2,ncol = 2, nrow = 1)
group1<-annotate_figure(nsl_support,
                        bottom = text_grob("Notes: Plot 1(a) and 1(b) are based on Model (1) in Table A8."))+
  theme(plot.margin = unit(c(.9,.9,.9,.9), "lines"))


plot1<-interplot(model2,var2 = "X2019.v2x_polyarchy",ci=.9,
                 var1 = "log.Chinese.trade.GDP.")+
  xlab("Polyarchy")+
  ggtitle("2(a). Coefficient of Log Trade")+
  theme_bw()+
  geom_hline(yintercept = 0,linetype="dashed")+
  theme(plot.title  = element_text(hjust = 0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey"))+
  scale_x_continuous(breaks = seq(0,.92,.1),limits = c(0,.92),minor_breaks = seq(0,.92,.1))


plot2<-interplot(model2,var2 = "X2019.v2x_polyarchy",ci=.9,
                 var1 = "log.Chinese.investment.GDP.")+
  xlab("Polyarchy")+
  ggtitle("2(b). Coefficient of Log Investment")+
  theme_bw()+
  geom_hline(yintercept = 0,linetype="dashed")+
  theme(plot.title  = element_text(hjust = 0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey"))+
  scale_x_continuous(breaks = seq(0,.92,.1),limits = c(0,.92),minor_breaks = seq(0,.92,.1))



xinjiang_support<-ggarrange(plot1,plot2,ncol = 2, nrow = 1)
group2<-annotate_figure(xinjiang_support,
                        bottom = text_grob("Notes: Plot 2(a) and 2(b) are based on Model (2) in Table A8."))+
  theme(plot.margin = unit(c(.9,.9,.9,.9), "lines"))



plot1<-interplot(model3,var2 = "X2019.v2x_polyarchy",ci=.9,
                 var1 = "log.Chinese.trade.GDP.")+
  xlab("Polyarchy")+
  ggtitle("3(a). Coefficient of Log Trade")+
  theme_bw()+
  geom_hline(yintercept = 0,linetype="dashed")+
  theme(plot.title  = element_text(hjust = 0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey"))+
  scale_x_continuous(breaks = seq(0,.92,.1),limits = c(0,.92),minor_breaks = seq(0,.92,.1))


plot2<-interplot(model3,var2 = "X2019.v2x_polyarchy",ci=.9,
                 var1 = "log.Chinese.investment.GDP.")+
  xlab("Polyarchy")+
  ggtitle("3(b). Coefficient of Log Investment")+
  theme_bw()+
  geom_hline(yintercept = 0,linetype="dashed")+
  theme(plot.title  = element_text(hjust = 0.5),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey"))+
  scale_x_continuous(breaks = seq(0,.92,.1),limits = c(0,.92),minor_breaks = seq(0,.92,.1))


hk_xinjiang_oppose<-ggarrange(plot1,plot2,ncol = 2, nrow = 1)
group3<-annotate_figure(hk_xinjiang_oppose,
                        bottom = text_grob("Notes: Plot 3(a) and 3(b) are based on Model (3) in Table A8."))+
  theme(plot.margin = unit(c(.9,.9,.9,.9), "lines"))



pdf("economic_political_institutions.pdf",width = 9.5,height = 12)

economic_political_institutions<-ggarrange(group1,group2,group3,
                                     ncol = 1, nrow = 3)

annotate_figure(economic_political_institutions,
                top = text_grob("Figure A2. Conditional Coefficients of Economic Variables with 90% Confidence Intervals", face = "bold", size = 14))

dev.off()



###############################################################
######Table A9: Instrumental variable + linear probability#####
###############################################################

#install.packages("sem")
#install.packages("AER")
#install.packages("ivreg")
library(sem)
library(AER)
library(ivreg)

#countries opposing HK NSL and XJ are the exact same group of countries

model22<-ivreg(nsl_oppose~log.Chinese.trade.GDP.+
                 X2019.v2x_polyarchy+
                 log.pci.1000+
                 Asia+
                 ratified_tortureprotocol+
                 log.Chinese.investment.GDP.+
                 Formal.ROC.Diplomatic.Ties.2019|
               
                 landlocked+
                 
                 X2019.v2x_polyarchy+
                 log.pci.1000+
                 Asia+
                 ratified_tortureprotocol+
                 log.Chinese.investment.GDP.+
                 Formal.ROC.Diplomatic.Ties.2019,
               
               data=data11)

summary(model22)

model222<-coeftest(model22, vcov = vcovHC, type = "HC3")
model222


