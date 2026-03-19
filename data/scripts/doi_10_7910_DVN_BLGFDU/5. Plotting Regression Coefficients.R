# Plot main coefficient plots and alternative specs in appendix
#install.packages(ggplot2)
#install.packages(dplyr)

library(ggplot2)
library(dplyr)

########
#setwd('')

######################################## plots (Figure 4 and 5)
#############inout plot
coefs <- read.csv("Macro Rolling Coefs.csv")

coefs$Model <- factor(coefs$Model, levels = c("News Visits Total", "Copartisan News Pct.", "Hard News Pct.", "Negative Pct."), labels = c("News visits (total)", "Copartisan news (percent)", "Hard news (percent)", "Negative news (percent)"))

coefsin <- filter(coefs, Sentiment == "In Party")
coefsout <- filter(coefs, Sentiment == "Out Party")

ggplot(coefsin, aes(x = Party, y = coef)) +
  geom_point(position = position_dodge(.5), size = 2.5) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                position = position_dodge(.5),
                width = 0.2) +
  scale_shape_discrete(name = "") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  ylab("") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.title.x = element_blank()) +
  facet_wrap(~Model, scales = "free") +
  theme(axis.line = element_line(color = 'black'),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.spacing = unit(1, "cm")) +  # Increases space between facets
  theme(
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.title.y = element_text(margin = margin(r = 10)))

ggsave(filename = "Fig4_In_Party_Plot.png", width = 2042, height = 1312, units = "px")


ggplot(coefsout, aes(x = Party, y = coef)) +
  geom_point(position = position_dodge(.5), size = 2.5) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                position = position_dodge(.5),
                width = 0.2) +
  scale_shape_discrete(name = "") +
  geom_hline(yintercept=0, linetype="dashed", color = "grey50")+
  ylab("")+
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.title.x = element_blank())+
  facet_wrap(~Model, scales = "free")+
  theme(axis.line = element_line(color = 'black'),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.spacing = unit(1, "cm")) +  # Increases space between facets
  theme(
    axis.text.x = element_text(color="black"),
    axis.text.y = element_text(color="black"),
    axis.ticks = element_line(color = "black"),
    axis.title.y = element_text(margin = margin(r = 10)))
ggsave(filename = "Fig5_Out_Party_Plot.png", width = 2042, height = 1312, units = "px")


######################################## Micro Plots (Figure 6 and 7)

coefs <- read.csv("micro coefs.csv")

coefs <- filter(coefs, Version == "FE")

coefs$Model <- factor(coefs$Model, levels = c("News Visits Remaining", "Copartisan News Pct.", "Hard News Pct.", "Negative Pct."), labels = c("News visits remaining", "Copartisan news (percent)", "Hard news (percent)", "Negative news (percent)"))

coefsin <- filter(coefs, Sentiment == "In Party")
coefsout <- filter(coefs, Sentiment == "Out Party")

ggplot(coefsin, aes(x = Party, y = coef)) +
  geom_point(position = position_dodge(.5), size = 2.5) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                position = position_dodge(.5),
                width = 0.2) +
  scale_shape_discrete(name = "") +
  geom_hline(yintercept=0, linetype="dashed", color = "grey50")+
  ylab("")+
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.title.x = element_blank())+
  facet_wrap(~Model, scales = "free")+
  theme(axis.line = element_line(color = 'black'),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.spacing = unit(1, "cm")) +  # Increases space between facets
  theme(
    axis.text.x = element_text(color="black"),
    axis.text.y = element_text(color="black"),
    axis.ticks = element_line(color = "black"),
    axis.title.y = element_text(margin = margin(r = 10)))
ggsave(filename = "Fig6_Micro_Coef_Plot_Inparty.png", width = 2042, height = 1312, units = "px")


ggplot(coefsout, aes(x = Party, y = coef)) +
  geom_point(position = position_dodge(.5), size = 2.5) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                position = position_dodge(.5),
                width = 0.2) +
  scale_shape_discrete(name = "") +
  geom_hline(yintercept=0, linetype="dashed", color = "grey50")+
  ylab("")+
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.title.x = element_blank())+
  facet_wrap(~Model, scales = "free")+
  theme(axis.line = element_line(color = 'black'),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.spacing = unit(1, "cm")) +  # Increases space between facets
  theme(
    axis.text.x = element_text(color="black"),
    axis.text.y = element_text(color="black"),
    axis.ticks = element_line(color = "black"),
    axis.title.y = element_text(margin = margin(r = 10)))
  ggsave(filename = "Fig7_Micro_Coef_Plot_Outparty.png", width = 2042, height = 1312, units = "px")


#################################
############################ Random Effects Appendix Plots (Macro and Micro)

###macro models (A12 and A13)  
  
coefs <- read.csv("Random Models Coefs.csv")

coefs$Model <- factor(coefs$Model, levels = c("News Visits Total", "Copartisan News Pct.", "Hard News Pct.", "Negative Pct."))

coefs <- filter(coefs, Macro == "Macro")

coefsin <- filter(coefs, Sentiment == "In Party")
coefsout <- filter(coefs, Sentiment == "Out Party")

ggplot(coefsin, aes(x = Party, y = coef, group = Version, color = Version)) +
  geom_point(position = position_dodge(.5), size = 2.5) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                position = position_dodge(.5),
                width = 0.2) +
  scale_shape_discrete(name = "") +
  scale_color_manual(values=c("grey60", "grey20"))+
  geom_hline(yintercept=0, linetype="dashed", color = "grey50")+
  ylab("Change in News Consumption (In-Party Sentiment)")+
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.title.x = element_blank())+
  facet_wrap(~Model, scales = "free")
ggsave(filename = "FigA12_Macro In Party Plot Random Appendix.png", width = 2042, height = 1312, units = "px")


ggplot(coefsout, aes(x = Party, y = coef, group = Version, color = Version)) +
  geom_point(position = position_dodge(.5), size = 2.5) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                position = position_dodge(.5),
                width = 0.2) +
  scale_shape_discrete(name = "") +
  scale_color_manual(values=c("grey60", "grey20"))+
  geom_hline(yintercept=0, linetype="dashed", color = "grey50")+
  ylab("Change in News Consumption (Out-Party Sentiment)")+
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.title.x = element_blank())+
  facet_wrap(~Model, scales = "free")
ggsave(filename = "FigA13_Macro Out Party Plot Random Appendix.png", width = 2042, height = 1312, units = "px")

######
### Micro models (A14 and A15)

coefs <- read.csv("Random Models Coefs.csv")

coefs$Model <- factor(coefs$Model, levels = c("News Visits Total", "Copartisan News Pct.", "Hard News Pct.", "Negative Pct."))

coefs <- filter(coefs, Macro == "Micro")

coefsin <- filter(coefs, Sentiment == "In Party")
coefsout <- filter(coefs, Sentiment == "Out Party")

ggplot(coefsin, aes(x = Party, y = coef, group = Version, color = Version)) +
  geom_point(position = position_dodge(.5), size = 2.5) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                position = position_dodge(.5),
                width = 0.2) +
  scale_shape_discrete(name = "") +
  scale_color_manual(values=c("grey60", "grey20"))+
  geom_hline(yintercept=0, linetype="dashed", color = "grey50")+
  ylab("Change in Subsequent News Visits (In-Party Sentiment)")+
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.title.x = element_blank())+
  facet_wrap(~Model, scales = "free")
ggsave(filename = "FigA14_Micro In Party Plot Random Appendix.png", width = 2042, height = 1312, units = "px")


ggplot(coefsout, aes(x = Party, y = coef, group = Version, color = Version)) +
  geom_point(position = position_dodge(.5), size = 2.5) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                position = position_dodge(.5),
                width = 0.2) +
  scale_shape_discrete(name = "") +
  scale_color_manual(values=c("grey60", "grey20"))+
  geom_hline(yintercept=0, linetype="dashed", color = "grey50")+
  ylab("Change in Subsequent News Visits (Out-Party Sentiment)")+
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.title.x = element_blank())+
  facet_wrap(~Model, scales = "free")
ggsave(filename = "FigA15_Micro Out Party Plot Random Appendix.png", width = 2042, height = 1362, units = "px")
