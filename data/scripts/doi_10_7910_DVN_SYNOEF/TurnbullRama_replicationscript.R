##Replication file for Turnbull-Dugarte & Rama "When the US far-right sneezes, the European far-right catches a cold. Quasi-experimental evidence of electoral contagion from Spain"
##Published in Electoral Studies, 2021


library(tidyverse)
library(ggplot2)
library(ggcharts)
library(readr)
library(broom)
library(broom.mixed)
library(estimatr)
library(interactions)
library(jtools)
library(ggplot2)
library(ggstance)
library(rdrobust)   
library(rddensity) 
library(patchwork)
library(ggpubr)
library(modelsummary)
library(ritest)
require("ggrepel")


##Figure 1##

cbp2 <- c("gray73", "gray69", "gray65", "gray61",
          "gray57", "gray53", "gray49","red2", "green4")
norris <- read_csv("data_norris.csv")
set.seed(17)

ggplot(norris,
       mapping = aes(x = V4_Scale, y = V6_Scale, color = plab, size = PartyPerVote)) +
  geom_point() +
  geom_label_repel(show.legend = FALSE, aes(label=plab, size=12))+
  scale_colour_manual(values=cbp2)+
  xlim (0, 10) +
  ylim (0, 10) +
  theme_minimal()+
  geom_vline(xintercept = 5, linetype="dashed", color="gray77", size=1, alpha=.5) +
  geom_hline(yintercept = 5, linetype="dashed", color="gray77", size=1, alpha=.5) +
  labs(x = 'Economic left-right position', y = 'Liberal-authoritarian position',
       title="GOP under Trump in a comparative context",
       subtitle="Values from Global Party Survey (Norris 2020) | Data points scaled by % vote share",
       caption="AfD: Alternative für Deutschland | GD: Golden Dawn | FPÖ: Freiheitliche Partei Österreichs\nNR: National Rally | PVV: Partij voor de Vrijheid | SD: Sverigedemokraterna") +
  theme(
    plot.title = element_text(hjust = 0, size = 14, face="bold"),
    plot.subtitle = element_text(hjust = 0, size = 12),
    axis.title.x = element_text(size=12),
    axis.title.y = element_text(size=12),
    legend.position = "none")

ggsave("Figure1.png", width = 20, height = 18, units = "cm")


##Figure 2##

prc <- read_csv("data_PRC.csv")

dumbbell_chart(
  data = prc,
  x = country,
  y1 = NoSim,
  y2 = Sim,
  top_n = 10,
  point_colors = c("#4863A0", "forestgreen"),
  legend_labels = c("Supporters of other parties", "Far-right supporters"),
  sort=TRUE) +
  labs(
    x = NULL,
    y = NULL,
    caption = "Aggregate polling data from Pew Research Centre\nreported in El Confidencial (Sep. 20th 2020)",
    title = "Comparative support for Trump in West European electorates")+
  theme_minimal()+
  theme(legend.position="bottom",
        title = element_text(hjust = 0, face="bold"),
        legend.text = element_text(hjust = 1, size = 10),
        axis.title = element_text(size=10))+
  scale_y_continuous(
    labels = scales::percent_format(scale = 1))

ggsave("Figure2.png")


##Figure 3##

google <- read_csv("data_google.csv")

lineplot <- ggplot(google, 
                   mapping = aes(x=week, y=Trump)) +
  theme_minimal()+
  geom_line(color= "steelblue", size=1) +
  labs(x = "Week relative to US election", y = "Google trends score", title="Spain Google Search Data", subtitle="Searches for 'Trump' (Nov 1st 2019 - Jan 1st 2021)") +
  geom_vline(aes(xintercept = 0), color = "forestgreen", linetype="longdash", size=0.8) +
  annotate(
    geom="text", x = -16, y = 79, size = 5, color = "forestgreen",
    label = "Trump election loss")+
  annotate(
    geom = "curve", x = -16, y = 76, xend = -0.5, yend = 60, 
    curvature = .15, arrow = arrow(length = unit(2, "mm")), colour="forestgreen")+
  theme(
    plot.title = element_text(hjust =0, face="bold"),
    plot.subtitle = element_text(hjust = 0, color = "forestgreen"),
    axis.title.x = element_text(face=2),
    axis.title.y = element_text(face=2),
    axis.text.x = element_text(face=2),
    axis.text.y = element_text(face=2))

ggsave("Figure3.png")

##Figure 4##

trumpVOX <- read_csv("data_cis.csv")

trumpVOX <- trumpVOX %>% 
  mutate(estudios = as.factor(estudios),
         treat = as.factor(treat),
         treatnumeric = as.numeric(treat),
         treat2 = as.factor(treat2),
         sex = as.factor(sex),
         civilstat = as.factor(civilstat),
         left = as.factor(left),
         churchgoer = as.factor(churchgoer),
         SITLAB = as.factor(SITLAB),
         MUN = as.factor(MUN),
         rightvote = as.factor(rightvote),
         munsize = as.numeric(munsize)
  )

rightonly <- subset(trumpVOX, left == 0)
leftonly <- subset(trumpVOX, left == 1)
rightonly2 <- subset(trumpVOX, rightvote == 1)
leftonly2 <- subset(trumpVOX, rightvote == 0)

model1 <-lm (abascal2 ~ treat, data=trumpVOX, weight=PESO)
model2 <-lm (abascal2 ~ treat*rightvote, data=trumpVOX, weight=PESO)
model1a <-lm (abascal2 ~ treat2 + sex + edad + estudios + civilstat + SITLAB + churchgoer + munsize, data=trumpVOX, weight=PESO)
model2a <-lm (abascal2 ~ treat2*rightvote + sex + edad + estudios + SITLAB + civilstat + churchgoer + munsize, data=trumpVOX, weight=PESO)
model3 <-lm (VOXintent ~ treat, data=trumpVOX, weight=PESO)
model4 <-lm (VOXintent ~ treat*rightvote, data=trumpVOX, weight=PESO)
model3a <-lm (VOXintent ~ treat2 + sex + edad + estudios + SITLAB + civilstat + churchgoer + munsize, data=trumpVOX, weight=PESO)
model4a <-lm (VOXintent ~ treat2*rightvote + sex + edad + estudios + SITLAB + civilstat + churchgoer + munsize, data=trumpVOX, weight=PESO)
cm <- c('treat1' = 'ITT')

models1 <- list(
  "Unadjusted ITT" = lm (abascal2 ~ treat, data=trumpVOX, weight=PESO),
  "Covariate adjusted  ITT" =lm (abascal2 ~ treat + sex + edad + estudios + civilstat + SITLAB + churchgoer + munsize, data=trumpVOX, weight=PESO))
models2 <- list(
  "Unadjusted ITT" = lm (VOXintent~ treat, data=trumpVOX, weight=PESO),
  "Covariate adjusted  ITT" =lm (VOXintent ~ treat + sex + edad + estudios + civilstat + SITLAB + churchgoer + munsize, data=trumpVOX, weight=PESO))

main1<- modelplot(models1, coef_map = cm)+
  labs(x = "", 
       y = "",
       title = 'Support for Abascal (1-10)')+
  scale_color_manual(values = c("green4", "green3"))+
  geom_vline(xintercept = 0, color="grey4", size=1, alpha=.5, linetype = "longdash")+
  theme(axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0, size = 12, face="bold"))

main2<- modelplot(models2, coef_map = cm)+
  labs(x = "", 
       y = "",
       title = 'Voting intentions for VOX (0-1)')+
  scale_color_manual(values = c("green4", "green3"))+
  geom_vline(xintercept = 0, color="grey4", size=1, alpha=.5, linetype = "longdash")+
  theme(axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0, size = 12, face="bold"))



mainplot <- ggarrange(main1, main2, ncol=2, nrow=1, common.legend=TRUE, 
                      legend="bottom",align = "h") 

annotate_figure(mainplot,
                bottom = text_grob("N = 3,830 | Confidence intervals at 95% \n  OLS regression with robust standard errors", hjust=1, x=1),
                top = text_grob("Estimated treatment effects of Trump loss on support for VOX \n", hjust=.62,
                                size=14, face=2))
ggsave("Figure4.png")


##Figure 5##

abc<- cat_plot(model2, pred = treat, modx = rightvote, legend.main= "Ideological voting blocks", 
               robust=TRUE, geom="bar", errorbar.width=.3,
               modx.labels=c("0" = "Vote for left-wing party", "1" = "Voted for right-wing party"))+
  scale_color_manual(values = c("#4863A0", "forestgreen"))+
  scale_fill_manual(values = c("#4863A0", "forestgreen"))+
  labs(subtitle = "Support for Abascal")+
  ylab("")+
  xlab("Treatment condition")+
  annotate(
    geom="text", x = 2.10, y =3.6, size = 3, color = "forestgreen", fontface=2,
    label = "3.44")+
  annotate(
    geom="text", x = 1.10, y =3.9, size = 3, color = "forestgreen", fontface=2,
    label = "3.80")+
  annotate(
    geom="text", x = 1.85, y =1.95, size = 3, color = "#4863A0", fontface=2,
    label = "1.69")+
  annotate(
    geom="text", x = .85, y =1.95, size = 3, color = "#4863A0", fontface=2,
    label = "1.79")+
  theme_light()+
  theme(plot.subtitle = element_text(hjust = 0.5, size = 12, face="bold"),
        legend.position="none", 
        axis.title.x = element_text(face=2),
        axis.text.x = element_text(face=2),
        axis.text.y = element_text(face=2))+ 
  scale_x_discrete(labels=c("0" = "Control", "1" = "Treatment"))

def<- cat_plot(model4, pred = treat, modx = rightvote, legend.main= "Ideological voting blocks", 
               robust=TRUE, geom="bar", errorbar.width=.3,
               modx.labels=c("0" = "Vote for left-wing party", "1" = "Voted for right-wing party"))+
  scale_color_manual(values = c("#4863A0", "forestgreen"))+
  scale_fill_manual(values = c("#4863A0", "forestgreen"))+
  labs(subtitle = "Pr(Voting for Vox)")+
  ylab("")+
  xlab("Treatment condition")+
  theme_light()+
  annotate(
    geom="text", x = 1.90, y =.265, size = 4, color = "forestgreen", fontface=2,
    label = "Right-wing party voter")+
  annotate(
    geom="text", x = 2.15, y =.135, size = 3, color = "forestgreen", fontface=2,
    label = ".12")+
  annotate(
    geom="text", x = 1.15, y =.245, size = 3, color = "forestgreen", fontface=2,
    label = ".23")+
  annotate(
    geom="text", x = 1.87, y =.28, size = 4, color = "#4863A0", fontface=2,
    label = "Left-wing party voter")+
  annotate(
    geom="text", x = 1.90, y =.02, size = 3, color = "#4863A0", fontface=2,
    label = ".004")+
  annotate(
    geom="text", x = .90, y =.04, size = 3, color = "#4863A0", fontface=2,
    label = ".02")+
  theme(plot.subtitle = element_text(hjust = 0.5, size = 12, face="bold"), 
        legend.position="none", 
        axis.title.x = element_text(face=2),
        axis.text.x = element_text(face=2),
        axis.text.y = element_text(face=2))+
  scale_x_discrete(labels=c("0" = "Control", "1" = "Treatment"))

abc+def+
  plot_annotation(title = "Predicted outcome by treatment group & left-right party vote recall (2019)",
                  theme=theme(plot.title=element_text( size = 12, face="bold", hjust=0), legend.position="bottom"))
ggsave("Figure5.png")


##Figure 6##

model5 <-lm (casado2 ~ treat, data=trumpVOX, weight=PESO)
model6 <-lm (casado2 ~ treat*rightvote, data=trumpVOX, weight=PESO)
model5a <-lm (casado2 ~ treat2 + sex + edad + estudios + SITLAB + civilstat + churchgoer + munsize, data=trumpVOX, weight=PESO)
model6a <-lm (casado2 ~ treat2*rightvote + sex + edad + estudios + SITLAB + civilstat + churchgoer + munsize, data=trumpVOX, weight=PESO)


model7 <-lm (PPintent ~ treat, data=trumpVOX, weight=PESO)
model8 <-lm (PPintent ~ treat*rightvote, data=trumpVOX, weight=PESO)
model7a <-lm (PPintent ~ treat2 + sex + edad + estudios + SITLAB + civilstat + churchgoer, data=trumpVOX, weight=PESO)
model8a <-lm (PPintent ~ treat2*rightvote + sex + edad + estudios + SITLAB + civilstat + churchgoer , data=trumpVOX, weight=PESO)

abc_PP<- cat_plot(model6, pred = treat, modx = rightvote, legend.main= "Ideological voting blocks", 
                  robust=TRUE, geom="bar", errorbar.width=.3,
                  modx.labels=c("0" = "Vote for left-wing party", "1" = "Voted for right-wing party"))+
  scale_color_manual(values = c("#4863A0", "#298acd"))+
  scale_fill_manual(values = c("#4863A0", "#298acd"))+
  labs(subtitle = "Support for Casado")+
  ylab("")+
  xlab("Treatment condition")+
  theme_light()+
  annotate(
    geom="text", x = 1.83, y =6.6, size = 4, color = "#4863A0", fontface=2,
    label = "Left-wing party voter")+
  annotate(
    geom="text", x = 1.85, y =3.4, size = 3, color = "#4863A0", fontface=2,
    label = "3.02")+
  annotate(
    geom="text", x = .85, y =3.4, size = 3, color = "#4863A0", fontface=2,
    label = "3.13")+
  annotate(
    geom="text", x = 1.86, y =6.2, size = 4, color = "#298acd", fontface=2,
    label = "Right-wing party voter")+
  annotate(
    geom="text", x = 2.05, y =5.2, size = 3, color = "#298acd", fontface=2,
    label = "5.02")+
  annotate(
    geom="text", x = 1.1, y =4.6, size = 3, color = "#298acd", fontface=2,
    label = "4.26")+
  theme(plot.subtitle = element_text(hjust = 0.5, size = 12, face="bold"),
        legend.position="none", 
        axis.title.x = element_text(face=2),
        axis.text.x = element_text(face=2),
        axis.text.y = element_text(face=2))+ 
  scale_x_discrete(labels=c("0" = "Control", "1" = "Treatment"))

def_PP<- cat_plot(model8, pred = treat, modx = rightvote, legend.main= "Ideological voting blocks", 
                  robust=TRUE, geom="bar", errorbar.width=.3,
                  modx.labels=c("0" = "Vote for left-wing party", "1" = "Voted for right-wing party"))+
  scale_color_manual(values = c("#4863A0", "#298acd"))+
  scale_fill_manual(values = c("#4863A0", "#298acd"))+
  labs(subtitle = "Pr(Voting for the PP)")+
  ylab("")+
  xlab("Treatment condition")+
  theme_light()+
  annotate(
    geom="text", x = 2.1, y =.41, size = 3, color = "#298acd", fontface=2,
    label = ".39")+
  annotate(
    geom="text", x = 1.1, y =.31, size = 3, color = "#298acd", fontface=2,
    label = ".29")+
  annotate(
    geom="text", x = 1.85, y =.04, size = 3, color = "#4863A0", fontface=2,
    label = ".01")+
  annotate(
    geom="text", x = .85, y =.05, size = 3, color = "#4863A0", fontface=2,
    label = ".03")+
  theme(plot.subtitle = element_text(hjust = 0.5, size = 12, face="bold"), 
        legend.position="none", 
        axis.title.x = element_text(face=2),
        axis.text.x = element_text(face=2),
        axis.text.y = element_text(face=2))+
  scale_x_discrete(labels=c("0" = "Control", "1" = "Treatment"))
abc_PP+def_PP+
  plot_annotation(title = "Predicted outcome by treatment group & left-right party vote recall (2019)",
                  theme=theme(plot.title=element_text( size = 12, face="bold", hjust=0), legend.position="bottom"))
ggsave("Figure6.png", dpi=600)


##Figure7##

all1 <- lm(abascal2 ~ treat, data=trumpVOX)
all2 <- lm (VOXintent ~ treat, data=trumpVOX)

ritest.all1 <- ritest(all1, 'treat', reps = 2000)
ritest.all2 <- ritest(all2, 'treat', reps = 2000)


ritest.plot1 <- ggplot(data.frame(betas = ritest.all1$betas), aes(betas)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.5, fill="darkgreen") +
  geom_vline(xintercept = all1$coefficients[2], colour="darkgreen", size=1, linetype = "longdash") +
  theme_minimal() +
  xlim(-.5, 0.5)+
  labs(y = "Density", x = "ITT coefficient", title = "Support for Abascal")+
  theme(plot.title = element_text(face=2))


ritest.plot2 <- ggplot(data.frame(betas = ritest.all2$betas), aes(betas)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.5, fill="darkgreen") +
  geom_vline(xintercept = all2$coefficients[2], colour="darkgreen", size=1, linetype = "longdash") +
  theme_minimal() +
  xlim(-.06, 0.06)+
  labs(y = "Density", x = "ITT coefficient", title = "Voting intentions for VOX")+
  theme(plot.title = element_text(face=2))


permute<- ggarrange(ritest.plot1, ritest.plot2, ncol = 2)

annotate_figure(permute,
                top = text_grob("Randomisation inference (2000 permutations) \n", hjust=.8, face=2,
                                size=14))
ggsave("Figure7.png")

##Figure8##


window1<- subset(trumpVOX, window1 == 1)
window2<- subset(trumpVOX, window2 == 1)
window3<- subset(trumpVOX, window3 == 1)
window4<- subset(trumpVOX, window4 == 1)
window5<- subset(trumpVOX, window5 == 1)

windowm1 <- list(
  "±1 day"= lm (abascal2 ~ treat, data=window1, weight=PESO),
  "±2 day"= lm (abascal2 ~ treat, data=window2, weight=PESO),
  "±3 day"= lm (abascal2 ~ treat, data=window3, weight=PESO),
  "±4 day"= lm (abascal2 ~ treat, data=window4, weight=PESO),
  "±5 day"= lm (abascal2 ~ treat, data=window5, weight=PESO))

windowm2 <- list(
  "±1 day"= lm (VOXintent ~ treat, data=window1, weight=PESO),
  "±2 day"= lm (VOXintent ~ treat, data=window2, weight=PESO),
  "±3 day"= lm (VOXintent ~ treat, data=window3, weight=PESO),
  "±4 day"= lm (VOXintent ~ treat, data=window4, weight=PESO),
  "±5 day"= lm (VOXintent ~ treat, data=window5, weight=PESO))

windowplots1<- modelplot(windowm1, coef_map = cm)+
  labs(x = "", 
       y = "",
       title = 'Support for Abascal (1-10)')+
  scale_color_manual(values = c("#008000", "#198d19","#339933", "#4da64d","#66b366"))+
  geom_vline(xintercept = 0, color="grey4", size=1, alpha=.5, linetype = "longdash")+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        plot.title = element_text(hjust = 0, size = 12, face="bold"))

windowplots2<- modelplot(windowm2, coef_map = cm)+
  labs(x = "", 
       y = "",
       title = 'Support for Abascal (1-10)')+
  scale_color_manual(values = c("#008000", "#198d19","#339933", "#4da64d","#66b366"))+
  geom_vline(xintercept = 0, color="grey4", size=1, alpha=.5, linetype = "longdash")+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        plot.title = element_text(hjust = 0, size = 12, face="bold"))


windowplot <- ggarrange(windowplots1, windowplots2, ncol=2, nrow=1, common.legend=TRUE, 
                        legend="bottom",align = "h") 

annotate_figure(windowplot,
                bottom = text_grob("Confidence intervals at 95%", hjust=1, x=1),
                top = text_grob("Robustness test with bandwidth windows \n", hjust=1.6, x=1, 
                                size=14, face=2))
ggsave("Figure8.png")




