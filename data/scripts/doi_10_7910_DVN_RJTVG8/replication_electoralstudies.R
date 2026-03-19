
## Replication. script for Turnbull-Dugarte & Devine (2023) "Support for digitising the
## ballot box: a systematic review of i-voting pilots and a conjoint experiment" 
## published in Electoral Studies 

##Analysis completed using: RStudio Version 2023.06.2+561 (2023.06.2+561)
                        ##  on MacOS Monterey 12.6.7

rm(list=ls())
library(tidyverse)
library(ggplot2)
library(haven)
library(sjPlot)
library(here)
library(stargazer)
library(ggdist)
library(cregg)
library(modelsummary)
library(patchwork)
library(ggpubr)
library(viridisLite)
library(RColorBrewer)
colors <- viridis(option = "magma", 
                  begin = 0.1, 
                  end = .8, 
                  direction = -1, 
                  n = 11)
df <- read_dta("evoting_replicationfile.dta")


df <-df%>% 
  mutate(prereg= as.factor(prereg),
         fraud= as.factor(fraud),
         method= as.factor(method),
         vwindow= as.factor(vwindow),
         trials= as.factor(trials),
         pilot= as.factor(pilot),
         platform= as.factor(platform),
         private= as.factor(private),
         cost= as.factor(cost),
         party= as.factor(party),
         endorse= as.factor(endorse),
         trustbin= as.factor(trustbin),
         viewsfraud= as.factor(viewsfraud),
         votemethod= as.factor(votemethod),
         satisfied_internet= as.factor(satisfied_internet),
         satisfied_elections= as.factor(satisfied_elections),
         IDvar= as.factor(IDvar),
         task= as.factor(task))


colorX<-c("#205C8A", "#E3256B")

conjoint1<- lm(selected ~ prereg + method + vwindow + trials + pilot + fraud + cost+ platform + private + party + endorse, data=df)
modelsummary(conjoint1, stars=TRUE, output='latex', cluster = "IDvar")

library(sjPlot)
tab_model(conjoint1)


df$prereg <- factor(df$prereg,
                    levels = c(0,1,2),
                    labels = c("Automatically pre-registered", "Must opt-in by post", "Must opt-in online"))
df$fraud <- factor(df$fraud,
                   levels = c(0,1,2),
                   labels = c("None", "Increase risk of fraud", "Decreases risk of fraud"))
df$method <- factor(df$method,
                    levels = c(0,1,2, 3),
                    labels = c("Vote via website", "Vote via SMS", "Vote via smartphone app", "Vote via telephone call"))
df$vwindow <- factor(df$vwindow,
                     levels = c(0,1,2),
                     labels = c("Available for two weeks up to\n(and including) polling day", "Available for up to two weeks\nbefore polling day", "Available on polling day only"))
df$trials <- factor(df$trials,
                    levels = c(0,1,2,3,4,5,6),
                    labels = c("No trials", "Argentina", "Estonia", "Germany", "Mozambique", "Switzerland", "USA"))
df$pilot <- factor(df$pilot,
                   levels = c(0,1,2,3,4,5,6,7,8),
                   labels = c("No change", "Decreases overall participation", "Decreases participation among elderly", "Decreases particiaption among young", "Decreases participation in socially deprived areas", "Increases overall participation", "Increases participation among elderly", "Increases particiaption among young", "Increases participation in socially deprived areas"))
df$cost <- factor(df$cost,
                  levels = c(0,1,2,3,4,5,6),
                  labels = c("£0 (none)", "-£0.50 (50p less)", "-£1 (one pound less)", "-£5 (five pounds less)", "£0.50 (50p more)", "£1 (one pound more)", "£5 (five pounds more)"))
df$platform <- factor(df$platform,
                      levels = c(0,1,2),
                      labels = c("Local council", "Central government", "Local council & central government"))
df$private <- factor(df$private,
                     levels = c(0,1),
                     labels = c("No private IT involvement", "Private sector IT involvement"))
df$party <- factor(df$party,
                   levels = c(0,1,2,3,4),
                   labels = c("Cross-party support","Conservatives", "Labour", "Lib Dems", "Greens"))
df$endorse <- factor(df$endorse,
                     levels = c(0,1,2,3,4),
                     labels = c("No endorsement", "European Union", "UK Electoral Commission", "UK Electoral Reform Society", "United Nations (UN)"))
df$queer <- factor(df$queer,
                   levels = c(0,1),
                   labels = c("Hetero", "LGB"))
df$sex <- factor(df$sex,
                 levels = c(0,1),
                 labels = c("Man", "Woman"))
df$trustbin <- factor(df$trustbin,
                      levels = c(0,1),
                      labels = c("Doesn't trust MPs", "Trusts MPs"))
df$votemethod <- factor(df$votemethod,
                        levels = c(0,1),
                        labels = c("In-person voter", "Convenience voter"))
df$satisfied_internet <- factor(df$satisfied_internet,
                                levels = c(0,1),
                                labels = c("Dissatisfied with\ninternet coverage", "Satisfied with\ninternet coverage"))
df$satisfied_elections <- factor(df$satisfied_elections,
                                 levels = c(0,1),
                                 labels = c("Dissatisfied with\nhow elections run", "Satisfied with\nhow elections run"))
df$viewsfraud <- factor(df$viewsfraud,
                        levels = c(0,1),
                        labels = c("Hasn't perceived electoral fraud", "Has perceived electoral fraud"))
df$degree <- factor(df$degree,
                    levels = c(0,1),
                    labels = c("No degree", "Degree"))
df$noncis <- factor(df$noncis,
                    levels = c(0,1),
                    labels = c("Cisgender", "Not cisgender"))
df$votemain <- factor(df$votemain,
                      levels = c(0,1,2,3),
                      labels = c("Labour", "Conservative", "Other", "Abstain"))
df$votetory <- factor(df$votetory,
                      levels = c(0,1),
                      labels = c("Labour", "Conservative"))
df$ideo <- factor(df$ideo,
                  levels = c(0,1,2),
                  labels = c("Left", "Centre", "Right"))
amce(df, selected ~ prereg + method + vwindow + trials + pilot + 
       fraud + cost + platform + private + party + endorse, id = ~ IDvar)

amce.1 <- amce(df, selected ~ prereg + method + vwindow + trials + pilot + 
                 fraud + cost + platform + private + party + endorse, id = ~ IDvar)

labels <- c(prereg = "Pre-registration", method = "Method",
            vwindow = "Voting window", trials = "Successful trials in",
            pilot = "Result of pilot", fraud = "Effect on fraud",
            cost = "Costs", platform = "Administered by",
            private = "Private sector role", party = "Proposing party",
            endorse = "Endorsed by")



##FIGURE 2##

mm1 <- mm(df, selected ~ prereg + method + vwindow + trials + pilot + 
            fraud + cost + platform + private + party + endorse, id = ~ IDvar, h0 = 0.5)

mm2 <- mm(df, trustnormal ~ prereg + method + vwindow + trials + pilot + 
            fraud + cost + platform + private + party + endorse, id = ~ IDvar, h0 = 0.5)


mm1.process <- subset(mm1, feature %in% c("prereg", "method", "vwindow"))
plot1<- plot(mm1.process, feature_headers = FALSE) +
  ggplot2::facet_wrap(~feature, ncol = 1L,
                      scales = "free_y") +
  theme_ggdist()+
  xlab("")+
  labs(subtitle="Support for reform")+
  geom_label(aes(label=sprintf("%0.2f", round(estimate, digits = 2))), 
             nudge_x =-.12, size=2.5, show.legend = FALSE)+
  xlim(.2,.6)+
  theme(
    axis.title.y =element_blank(),
    strip.text = element_blank(),
    legend.position ="none",
    legend.text = element_text(face="bold"),
    plot.subtitle = element_text(face="bold"),
    axis.title.x=element_text(face="bold"),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.title=element_blank(),
    axis.text.y = element_text(face="bold")) +
  geom_vline(xintercept = 0.50, linetype="dashed", colour="gray33")


mm2.process <- subset(mm2, feature %in% c("prereg", "method", "vwindow"))
plot2<- plot(mm2.process, feature_headers = FALSE) +
  ggplot2::facet_wrap(~feature, ncol = 1L, labeller=labeller(feature = labels),
                      scales = "free_y", strip.position = "right") +
  theme_ggdist()+
  xlab("")+
  labs(subtitle="Trustworthiness vis-à-vis\nin-person voting")+
  geom_label(aes(label=sprintf("%0.2f", round(estimate, digits = 2))), 
             nudge_x =-.1, size=2.5, show.legend = FALSE)+
  xlim(.2,.6)+
  theme(
    axis.title.y =element_blank(),
    legend.position ="none",
    legend.text = element_text(face="bold"),
    plot.subtitle = element_text(face="bold"),
    axis.title.x=element_text(face="bold"),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.title=element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()) +
  geom_vline(xintercept = 0.50, linetype="dashed", colour="gray33")

plot1+plot2
ggsave("Figure2.png", 
       path = here("publicationfigures"),
       dpi=1000, height=18, width=18, units="cm")


##FIGURE 2##
mm1.trials <- subset(mm1, feature %in% c("trials", "pilot", "fraud", "cost"))
mm2.trials <- subset(mm2, feature %in% c("trials", "pilot", "fraud", "cost"))

plot3<- plot(mm1.trials, feature_headers = FALSE) +
  ggplot2::facet_wrap(~feature, ncol = 1L,
                      scales = "free_y") +
  theme_ggdist()+
  xlab("")+
  labs(subtitle="Support for reform")+
  geom_label(aes(label=sprintf("%0.2f", round(estimate, digits = 2))), 
             nudge_x =-.12, size=2.5, show.legend = FALSE)+
  xlim(.2,.7)+
  theme(
    axis.title.y =element_blank(),
    strip.text = element_blank(),
    legend.position ="none",
    legend.text = element_text(face="bold"),
    plot.subtitle = element_text(face="bold"),
    axis.title.x=element_text(face="bold"),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.title=element_blank(),
    axis.text.y = element_text(face="bold")) +
  geom_vline(xintercept = 0.50, linetype="dashed", colour="gray33")

plot4<- plot(mm2.trials, feature_headers = FALSE) +
  ggplot2::facet_wrap(~feature, ncol = 1L, labeller=labeller(feature = labels),
                      scales = "free_y", strip.position = "right") +
  theme_ggdist()+
  xlab("")+
  labs(subtitle="Trustworthiness vis-à-vis\nin-person voting")+
  geom_label(aes(label=sprintf("%0.2f", round(estimate, digits = 2))), 
             nudge_x =-.1, size=2.5, show.legend = FALSE)+
  xlim(.2,.7)+
  theme(
    axis.title.y =element_blank(),
    legend.position ="none",
    legend.text = element_text(face="bold"),
    plot.subtitle = element_text(face="bold"),
    axis.title.x=element_text(face="bold"),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.title=element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()) +
  geom_vline(xintercept = 0.50, linetype="dashed", colour="gray33")

plot3+plot4
ggsave("Figure3.png", 
       path = here("publicationfigures"),
       dpi=1000, height=18, width=18, units="cm")


##FIGURE 4##

mm1.admin <- subset(mm1, feature %in% c("platform", "private", "party", "endorse"))
mm2.admin <- subset(mm2, feature %in% c("platform", "private", "party", "endorse"))
plot5<- plot(mm1.admin, feature_headers = FALSE) +
  ggplot2::facet_wrap(~feature, ncol = 1L,
                      scales = "free_y") +
  theme_ggdist()+
  xlab("")+
  labs(subtitle="Support for reform")+
  geom_label(aes(label=sprintf("%0.2f", round(estimate, digits = 2))), 
             nudge_x =-.12, size=2.5, show.legend = FALSE)+
  xlim(.2,.6)+
  theme(
    axis.title.y =element_blank(),
    strip.text = element_blank(),
    legend.position ="none",
    legend.text = element_text(face="bold"),
    plot.subtitle = element_text(face="bold"),
    axis.title.x=element_text(face="bold"),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.title=element_blank(),
    axis.text.y = element_text(face="bold")) +
  geom_vline(xintercept = 0.50, linetype="dashed", colour="gray33")


plot6<- plot(mm2.admin, feature_headers = FALSE) +
  ggplot2::facet_wrap(~feature, ncol = 1L, labeller=labeller(feature = labels),
                      scales = "free_y", strip.position = "right") +
  theme_ggdist()+
  xlab("")+
  labs(subtitle="Trustworthiness vis-à-vis\nin-person voting")+
  geom_label(aes(label=sprintf("%0.2f", round(estimate, digits = 2))), 
             nudge_x =-.1, size=2.5, show.legend = FALSE)+
  xlim(.2,.6)+
  theme(
    axis.title.y =element_blank(),
    legend.position ="none",
    legend.text = element_text(face="bold"),
    plot.subtitle = element_text(face="bold"),
    axis.title.x=element_text(face="bold"),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.title=element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()) +
  geom_vline(xintercept = 0.50, linetype="dashed", colour="gray33")

plot5+plot6
ggsave("Figure4.png", 
       path = here("publicationfigures"),
       dpi=1000, height=15, width=15, units="cm")


##FIGURE 5##
mm.diff.meth <- mm_diffs(df, selected ~ prereg + method + vwindow + trials + pilot + 
                           fraud + cost + platform + private + party + endorse, 
                         ~ votemethod, id = ~ IDvar)
mm.diff.meth<- mm.diff.meth%>% 
  mutate(statsig= if_else(p < 0.05, "Yes", "No"))


convenience1<- plot(mm.diff.meth, feature_headers = FALSE) +
  aes(color = statsig)+
  ggplot2::facet_wrap(~feature, ncol = 1L, labeller=labeller(feature = labels),
                      scales = "free_y", strip.position = "right") + 
  scale_color_manual(values = c("black","red2"))+
  geom_text(aes(label=sprintf("%0.2f", round(estimate, digits = 2))), 
            x = -.15, size=2, fontface="bold")+
  labs(title="Habitual convenience voter\nvs. habitual in-person voter",
       x="")+
  xlim(-.18,.15)+
  theme(legend.position = "none",
        plot.title = element_text(hjust=.5)) +
  geom_point(size=1.5)+
  theme(strip.text = element_blank())


mm.diff.elecstat<- mm_diffs(df, selected ~ prereg + method + vwindow + trials + pilot + 
                              fraud + cost + platform + private + party + endorse, 
                            ~ satisfied_elections, id = ~ IDvar)
mm.diff.elecstat<- mm.diff.elecstat%>% 
  mutate(statsig= if_else(p < 0.05, "Yes", "No"))

elecsat1<- plot(mm.diff.elecstat, feature_headers = FALSE) +
  aes(color = statsig)+
  ggplot2::facet_wrap(~feature, ncol = 1L, labeller=labeller(feature = labels),
                      scales = "free_y", strip.position = "right") + 
  scale_color_manual(values = c("black","red2"))+
  geom_text(aes(label=sprintf("%0.2f", round(estimate, digits = 2))), 
            x = -.15, size=2, fontface="bold")+
  labs(title="Citizens satisfied vs. dissatisfied\nwith electoral process",
       x="")+
  xlim(-.18,.15)+
  theme(legend.position = "none") +
  geom_point(size=1.5) +
  theme(strip.text = element_blank(),
        plot.title = element_text(hjust=.5),
        axis.text.y = element_blank(),
        axis.title.x = element_text(face="bold"),
        axis.ticks.y = element_blank())

mm.diff.internet <- mm_diffs(df, selected ~ prereg + method + vwindow + trials + pilot + 
                               fraud + cost + platform + private + party + endorse, 
                             ~ satisfied_internet, id = ~ IDvar)


mm.diff.internet<- mm.diff.internet%>% 
  mutate(statsig= if_else(p < 0.05, "Yes", "No"))

internet1<- plot(mm.diff.internet, feature_headers = FALSE) +
  aes(color = statsig)+
  ggplot2::facet_wrap(~feature, ncol = 1L, labeller=labeller(feature = labels),
                      scales = "free_y", strip.position = "right") + 
  scale_color_manual(values = c("black","red2"))+
  geom_text(aes(label=sprintf("%0.2f", round(estimate, digits = 2))), 
            x = -.15, size=2, fontface="bold")+
  labs(title="Citizens satisfied vs. dissatisfied\nwith internet coverage",
       x="")+
  xlim(-.18,.15)+
  theme(legend.position = "none") +
  geom_point(size=1.5) +
  theme(strip.text = element_blank(),
        plot.title = element_text(hjust=.5),
        axis.text.y = element_blank(),
        axis.title.x = element_text(face="bold"),
        axis.ticks.y = element_blank())



convenience1+elecsat1+internet1+ 
  plot_annotation(
    subtitle = 'Support for reform') & 
  theme(plot.subtitle = element_text(face="bold", size=14))
ggsave("Figure5a.png", 
       path = here("publicationfigures"),
       dpi=1000, height=28, width=30, units="cm")

mm.diff.meth <- mm_diffs(df, trustnormal ~ prereg + method + vwindow + trials + pilot + 
                           fraud + cost + platform + private + party + endorse, 
                         ~ votemethod, id = ~ IDvar)

mm.diff.meth<- mm.diff.meth%>% 
  mutate(statsig= if_else(p < 0.05, "Yes", "No"))


convenience1<- plot(mm.diff.meth, feature_headers = FALSE) +
  aes(color = statsig)+
  ggplot2::facet_wrap(~feature, ncol = 1L, labeller=labeller(feature = labels),
                      scales = "free_y", strip.position = "right") + 
  scale_color_manual(values = c("black","red2"))+
  geom_text(aes(label=sprintf("%0.2f", round(estimate, digits = 2))), 
            x = -.1, size=2, fontface="bold")+
  labs(title="Habitual convenience voter\nvs. habitual in-person voter",
       x="")+
  xlim(-.1,.15)+
  theme(legend.position = "none",
        plot.title = element_text(hjust=.5)) +
  geom_point(size=1.5)+
  theme(strip.text = element_blank())


mm.diff.elecstat<- mm_diffs(df, trustnormal ~ prereg + method + vwindow + trials + pilot + 
                              fraud + cost + platform + private + party + endorse, 
                            ~ satisfied_elections, id = ~ IDvar)


mm.diff.elecstat<- mm.diff.elecstat%>% 
  mutate(statsig= if_else(p < 0.05, "Yes", "No"))

elecsat1<- plot(mm.diff.elecstat, feature_headers = FALSE) +
  aes(color = statsig)+
  ggplot2::facet_wrap(~feature, ncol = 1L, labeller=labeller(feature = labels),
                      scales = "free_y", strip.position = "right") + 
  scale_color_manual(values = c("black","red2"))+
  geom_text(aes(label=sprintf("%0.2f", round(estimate, digits = 2))), 
            x = -.1, size=2, fontface="bold")+
  labs(title="Citizens satisfied vs. dissatisfied\nwith electoral process",
       x="")+
  xlim(-.1,.15)+
  theme(legend.position = "none") +
  geom_point(size=1.5) +
  theme(strip.text = element_blank(),
        plot.title = element_text(hjust=.5),
        axis.text.y = element_blank(),
        axis.title.x = element_text(face="bold"),
        axis.ticks.y = element_blank())

mm.diff.internet <- mm_diffs(df, trustnormal ~ prereg + method + vwindow + trials + pilot + 
                               fraud + cost + platform + private + party + endorse, 
                             ~ satisfied_internet, id = ~ IDvar)


mm.diff.internet<- mm.diff.internet%>% 
  mutate(statsig= if_else(p < 0.05, "Yes", "No"))

internet1<- plot(mm.diff.internet, feature_headers = FALSE) +
  aes(color = statsig)+
  ggplot2::facet_wrap(~feature, ncol = 1L, labeller=labeller(feature = labels),
                      scales = "free_y", strip.position = "right") + 
  scale_color_manual(values = c("black","red2"))+
  geom_text(aes(label=sprintf("%0.2f", round(estimate, digits = 2))), 
            x = -.1, size=2, fontface="bold")+
  labs(title="Citizens satisfied vs. dissatisfied\nwith internet coverage",
       x="")+
  xlim(-.1,.15)+
  theme(legend.position = "none") +
  geom_point(size=1.5) +
  theme(strip.text = element_blank(),
        plot.title = element_text(hjust=.5),
        axis.text.y = element_blank(),
        axis.title.x = element_text(face="bold"),
        axis.ticks.y = element_blank())

convenience1+elecsat1+internet1+ 
  plot_annotation(
    subtitle = 'Trustworthiness of reforms vis-à-vis in-person voting') & 
  theme(plot.subtitle = element_text(face="bold", size=14))
ggsave("Figure5b.png", 
       path = here("publicationfigures"),
       dpi=1000, height=28, width=30, units="cm")
