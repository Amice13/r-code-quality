library(tidyverse)
library(ggplot2)
theme_set(theme_bw())
library(ggrepel)
library(readr)
library(lme4)
library(interactions)
library(patchwork)
library(ggridges)
library(jtools)
library(modelsummary)
library(colorspace)
library(interplot)
library(ggpubr)
library(margins)
library(estimatr)
library(rdlocrand)
library(performance)
library(see)
library(specr)
set.seed(167)

install.packages("ggstance")

trend <- read_csv("google.csv")
polls<- read_csv("polldata2017.csv")
polls <-polls%>% 
  mutate(party= as.factor(party))


dataframe<- read_csv("MAYdata.csv")
dataframe <-dataframe%>% 
  mutate(sex= as.factor(sex),
         treat= as.factor(treat),
         treatnum= as.numeric(treat),
         run= as.numeric(run),
         leftdum= as.factor(leftdum),
         agecat=as.factor(agecat),
         employ= as.factor(employ),
         urban= as.factor(urban),
         interest= as.factor(interest),
         nutsID= as.factor(nutsID))

rightonly <- subset(dataframe, leftdum== 0)
leftonly <- subset(dataframe, leftdum== 1)
leaver <- subset(dataframe, euneg== 1)
remainer <- subset(dataframe, euneg== 0)

##Figure 1##
F1a <- ggplot(trend, 
                   mapping = aes(x=day, y=count))+
  geom_line(size=1, color="blue2") +
  labs(x = "Day relative to snap election called", y = "Google trends score", 
       title="UK Google searches for 'election'", subtitle="Search data: April 1st-30th 2017") +
  theme(legend.position = "top") +
  geom_vline(aes(xintercept = 0), color = "grey57", linetype="dashed", size=0.8) +
  annotate(
    geom="text", x = -10, y = 90, size = 4, color = "grey57", fontface=2,
    label = "Teresa May announces \nsnap election")+
  annotate(
    geom = "curve", x = -10, y = 86, xend = 0, yend = 60, 
    curvature =.4, arrow = arrow(length = unit(2, "mm")), colour="grey57")+
  theme(
    plot.title = element_text(hjust =0, color = "grey57", face="bold"),
    plot.subtitle = element_text(hjust = 0, color = "grey57", face="bold"),
    legend.title = element_text(color = "grey57", size = 10),
    legend.text = element_text(color = "grey57"),
    legend.position="bottom")+
  theme(axis.title.x = element_text(colour = "grey57", face=2),
        axis.title.y = element_text(colour = "grey57", face=2))


F1b<- ggplot(polls, aes(x = day3, y = poll, color=party, shape=party)) +
  scale_color_manual("Party", values = c( "blue2", "red2", "orange2"), 
                     labels = c("Conservative", "Labour", "Lib Dems"))+
  scale_shape_manual("Party", values=c(18,16,17), 
                     labels = c("Conservative", "Labour", "Lib Dems"))+
  geom_point(size=3)+
  geom_smooth(data = filter(polls, party==1), method = "gam", show.legend = FALSE) +
  geom_smooth(data = filter(polls, party==2), method = "gam", show.legend = FALSE) +
  geom_smooth(data = filter(polls, party==3), method = "gam", show.legend = FALSE) +
  geom_vline(xintercept = 0)+
  labs(x = "Day relative to snap election called", y = "Public voting intentions", 
       title="Polling average of three main parties",
       subtitle="Polling data: January-December 2017") +
  labs(color  = "Party",shape = "Party")+
  theme_bw()+
  annotate(
    geom="text", x = 130, y = .33, size = 4, color = "gray57", fontface=2,
    label = "Snap election announced")+
  annotate(
    geom = "curve", x = 118, y = .32, xend = 0, yend = .2, 
    curvature = -.3, arrow = arrow(length = unit(2, "mm")), colour="gray57")+
  theme(
    plot.title = element_text(hjust =0, color = "grey57", face="bold"),
    plot.subtitle = element_text(hjust = 0, color = "grey57", face="bold"),
    legend.title = element_blank(),
    legend.text = element_text(color = "grey57", face=2, size=12),
    legend.position=c(0.8, 0.4),
    legend.background = element_rect(fill=NA))+
  theme(axis.title.x = element_text(colour = "grey57", face=2),
        axis.title.y = element_text(colour = "grey57", face=2))

F1<- F1a+F1b

ggsave("Figure1.png",width = 30, height = 26, units = "cm", dpi=320)

##Figure 2##

meanday<- lm (trustgov ~ factor(run), data=dataframe)
dataframe$predictDAY<-predict(meanday, dataframe)

F2<- ggplot(dataframe, aes(x = run, y = trustgov, color=treat)) +
  scale_color_manual(values = c("#205C8A", "#FE6DB6")) +
  geom_smooth(data = filter(dataframe, run <0), method = "lm", show.legend = FALSE, level=.8) +
  geom_smooth(data = filter(dataframe, run >= 0), method = "lm", show.legend = FALSE, level=.8) +
  geom_point(data = filter(dataframe, run < 0), aes(x=run, y=predictDAY, size=count), shape=17,  show.legend = FALSE) +
  geom_point(data = filter(dataframe, run >= 0), aes(x=run, y=predictDAY, size=count), shape=20, show.legend = FALSE) +
  geom_vline(xintercept=0, colour="gray57")+
  annotate(
    geom="text", x = 4, y = .44, size = 4, color = "gray57", fontface=1,
    label = "Snap election called")+
  annotate(
    geom = "curve", x = 4, y = .43, xend = 0, yend = .35, 
    curvature = -.25, arrow = arrow(length = unit(2, "mm")), colour="gray57")+
  theme_minimal()+
  ylab("Pr(Trust Government)")+
  xlab("Days relative to treatment")

ggsave("Figure2.png", dpi=320)


##Figure 3##
model0<- lm (trustgov ~ treat + nutsID, data=dataframe)
model1<- lm (trustgov ~ treat + rile + sex + agecat + employ + urban + nutsID, data=dataframe)
dataframe$predicted<-predict(model1, dataframe)
treat <- subset(dataframe, treatnum== 2)
control <- subset(dataframe, treatnum== 1)
coefplot<- plot_summs(model0, model1, scale = TRUE, plot.distributions = TRUE,
                      coefs = c("Treatment effect" = "treat"),
                      model.names = c("Unadjusted ITT (β=.08 | t=1.93)", "Covariate-adjusted ITT (β=.1 | t=2.36)"), 
                      ci_level = .9, colors = "Blues")+
  labs(subtitle = "ii) ITT effect")+ 
  theme(legend.position = c(.6,.9))+
  theme(axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        plot.subtitle = element_text(hjust = 0, size = 10, face="bold"),
        legend.title=element_blank(),
        legend.text = element_text(face="bold"),
        panel.border = element_rect(fill=NA))

predmeans<- effect_plot(model = model1, pred = treat, robust=TRUE, int.width =.8, 
                        cat.geom="point", cat.interval.geom="linerange",
                        colors="slategrey", cat.pred.point.size=3)+
  labs(subtitle = "i) Mean level of trust by treatment condition",
       caption= "Treatment group outcomes statistically distinct at p<0.05(**)")+
  ylab("")+
  xlab("")+
  ylim(0,1)+
  scale_x_discrete(labels=c("0" = "Control", "1" = "Treatment"))+
  geom_jitter(data=treat, aes(x=treat, y=predicted, size=agecat),
              height=.1, width=.35, alpha=.15, shape=20,
              pch=21, color="#FE6DB6")+
  geom_jitter(data=control, aes(x=treat, y=predicted,size=agecat),
              height=.1, width=.35, alpha=.15, shape=17,
              pch=21, color="#205C8A")+
  geom_bracket(
    xmin = c("0"), xmax = c("1"),
    y.position = c(.60), label = c("β:0.10**"),
    tip.length =0.03,
    color="slategrey",
    size=.5, label.size = 4)+
  theme(legend.position = "none",
        panel.border = element_rect(fill=NA),
        plot.subtitle =element_text(face="bold"))

predmeans+coefplot
ggsave("Figure3.png",width = 20, height = 14, units = "cm", dpi=320)


##Figure 4##
modelX<-lm (trustgov ~ treat*rile + sex + agecat + employ + urban + nutsID, data=dataframe)
summ(modelX, robust=TRUE)
dataframe$predictINT<-predict(modelX, dataframe)
treat <- subset(dataframe, treatnum== 2)
control <- subset(dataframe, treatnum== 1)

gg_df <-
  modelX %>%
  margins(at = list(rile = seq(1, 10, by = 1))) %>%
  summary %>%
  as.data.frame() %>%
  filter(factor == "treat1")

predplot <-
  ggplot(dataframe, aes(rile, trustgov, shape = treat, group = treat, color=treat)) +
  scale_shape_manual(values = c(20, 17))+
  scale_color_manual(values = c("#205C8A", "#FE6DB6")) +
  scale_fill_manual(values = c("#205C8A", "#FE6DB6")) +
  theme_minimal()+
  stat_smooth(method = "lm_robust", fullrange = TRUE, se=FALSE) +
  geom_jitter(data=treat, aes(x=rile, y=predictINT, fill=treat, size=agecat), 
              alpha=.2, height=.04, width=1.2, shape=20,
              pch=21, color="#FE6DB6")+
  geom_jitter(data=control, aes(x=rile, y=predictINT, fill=treat,  size=agecat), 
              alpha=.2, height=.04, width=1.2, shape=17,
              pch=21, color="#205C8A")+
  theme(legend.position = "none")+
  xlim(0, 10)+
  ylab("Pr(Trusts Government)")+
  xlab("")+
  theme(axis.text.x =element_blank())+
  labs(subtitle="ITT effect moderated by ideological position")+
  annotate(
    geom="text", x = 4.5, y =.85, size = 3, color = "#FE6DB6", fontface=2,
    label = "Slope for \ntreated respondents")+
  annotate(
    geom = "curve", x = 5.2, y = .85, xend = 8, yend = .68, 
    curvature = -.2, arrow = arrow(length = unit(2, "mm")), colour="#FE6DB6")+
  annotate(
    geom="text", x = 7.5, y =.2, size = 3, color = "#205C8A", fontface=2,
    label = "Slope for \ncontrol respondents")+
  annotate(
    geom = "curve", x = 8, y = .2, xend = 9, yend = .45, 
    curvature = .2, arrow = arrow(length = unit(2, "mm")), colour="#205C8A")+
  theme(plot.subtitle = element_text(face="bold", color="#205C8A"))

ameplot<- ggplot(gg_df, aes(rile, AME)) +
  geom_point(colour="#FE6DB6", size=2) +
  coord_cartesian(xlim = c(1, 10), ylim = c(-.4, .6)) +
  geom_errorbar(aes(ymax = lower, ymin = upper), size= 1,width = 0, colour="#FE6DB6") +
  geom_hline(yintercept = 0, linetype = "dashed", colour="#205C8A") +
  xlab("Left-right placement") +
  ylab("Conditional ITT") +
  theme_minimal()


predplot/ameplot
ggsave("Figure4.png",width =19, height = 24, units = "cm", dpi=320)

##Figure 5##
model2a<- lm (trustgov ~ treat + sex + agecat + employ + urban + nutsID, data=rightonly)
summ(model2a)
model2b<- lm (trustgov ~ treat + sex + agecat + employ + urban + nutsID, data=leftonly)
summ(model2b)

interaction_IDEO<- plot_summs(model2a, model2b, scale = TRUE, plot.distributions = TRUE,
                              coefs = c("Treatment effect" = "treat"),
                              model.names = c("Right-wing voters (β=.19 | t=3.74)", "Left-wing voters (β=-.1 | t=-1.25)"), 
                              ci_level = .9, colors = "Blues")+
  labs(subtitle="i) ITT moderated by left-right dichotomy")+
  theme(legend.position = c(.5, .9))+
  theme(axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        legend.title=element_blank(),
        legend.text=element_text(face="bold"),
        panel.border = element_rect(fill=NA),
        plot.subtitle = element_text(hjust = 0, size = 10, face="bold"))


model3a<- lm (trustgov ~ treat + sex + agecat + employ + urban + nutsID, data=leaver)
summ(model3a)
model3b<- lm (trustgov ~ treat + sex + agecat + employ + urban + nutsID, data=remainer)
summ(model3b)

interaction_EU<- plot_summs(model3a, model3b, scale = TRUE, plot.distributions = TRUE,
                            coefs = c("Treatment effect" = "treat"),
                            model.names = c("Leavers (β=.17 | t=2.39)", "Remainers (β=.03 | t=.52)"), 
                            ci_level = .9, colors = "Blues")+
  labs(subtitle="ii) ITT moderated by leaver-remainer dichotomy")+
  theme(legend.position = c(.6, .9))+
  theme(axis.title.x=element_blank(),
        legend.title=element_blank(),
        legend.text=element_text(face="bold"),
        axis.text.y=element_blank(),
        panel.border = element_rect(fill=NA),
        plot.subtitle = element_text(hjust = 0, size = 10, face="bold"))

interaction_IDEO+interaction_EU
ggsave("Figure5.png", dpi=320)

##Figure 6##
#Bandwidth window test#
window1<- subset(dataframe, window1 == 1)
window2<- subset(dataframe, window2 == 1)
window3<- subset(dataframe, window3 == 1)
wm1<- lm (trustgov ~ treat + rile + sex + agecat + employ + urban + nutsID, data=window1)
wm2<- lm (trustgov ~ treat + rile + sex + agecat + employ + urban + nutsID, data=window2)
wm3<- lm (trustgov ~ treat + rile + sex + agecat + employ + urban + nutsID, data=window3)

bandwidth<- plot_summs(wm1, wm2, wm3, model1,
                       scale = TRUE, plot.distributions = TRUE, colors = "Blues",
                       model.names = c("±1 day (β=.1 | t=1.33)", "±2 day (β=.09 | t=1.64)", "±3 day (β=.11 | =t2.53)", "Full sample (β=.1 t=2.36)"), 
                       coefs = c("ATE" = "treat"), ci_level = .9)+
  labs(title = "Bandwidth window test")+
  theme(axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        plot.subtitle = element_text(hjust = 0, size = 10, face="bold"),
        legend.position = c(0.3, 0.9),
        legend.title = element_blank(),
        legend.text = element_text(face=2),
        panel.border = element_rect(fill=NA))

#Placebo tests#
model1<- lm (trustgov ~ treat + rile + sex + agecat + employ + urban + nutsID, data=dataframe)
placebo1<- lm (trustmedia ~ treat + rile + sex + agecat + employ + urban + nutsID, data=dataframe)
placebo2<- lm (trustlegal ~ treat + rile + sex + agecat + employ + urban + nutsID, data=dataframe)
placebo3<- lm (lifesat ~ treat + rile + sex + agecat + employ + urban + nutsID, data=dataframe)
placebo4<- lm (direct_country ~ treat + rile + sex + agecat + employ + urban + nutsID, data=dataframe)
placebo5<- lm (direct_EU ~ treat + rile + sex + agecat + employ + urban + nutsID, data=dataframe)
placebo6<- lm (counts ~ treat + rile + sex + agecat + employ + urban + nutsID, data=dataframe)
placebo7<- lm (countsEU ~ treat + rile + sex + agecat + employ + urban + nutsID, data=dataframe)
placebo<- plot_summs(placebo7, placebo6, placebo5, placebo4, placebo3, placebo2, placebo1, model1,
                     scale = TRUE, plot.distributions = TRUE, colors = "Blues",
                     model.names = c("Voice counts in EU (β=.03 | t=-0.79)","Voice counts in UK (β=.05 | t=1.21)", 
                                     "EU going in right direction (β=-.01 | t=-0.31)", "Country going in right direction (β=.001 | t=-0.10)",
                                     "Life satisfaction (β=.02 | t=-0.81)", "Trust in legal system (β=-.01 | t=-0.17)",
                                     "Trust in media (β=.04 | t=1.19)","Trust in Government (β=.1 | t=2.36)"), 
                     coefs = c("ATE" = "treat"), ci_level = .95)+
  labs(title = "Treatment effect on placebo outcomes")+
  theme(axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        plot.subtitle = element_text(hjust = 0, size = 10, face="bold"),
        legend.position = c(0.49, 0.8),
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.text = element_text(face=2),
        panel.border = element_rect(fill=NA))

bandwidth+placebo
ggsave("Figure6.png",width = 20, height = 16, units = "cm", dpi=320)


