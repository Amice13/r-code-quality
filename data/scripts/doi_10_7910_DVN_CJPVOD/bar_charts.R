# This code generates bar charts for mean odds and odds ratio for the two binary outcomes:
# - Any knowledge gap
# - Any link demanded

# Please set working directory to be source file location

rm(list = ls())

library(ggplot2)
library(dplyr)

df1 <- data.frame( val = c(1,2,3,4,5,6,7,8,9,10,11,12), #used to reorder factors
                   Odds = c(0.273,0.197,0.708,0.700,0.269,0.193,0.703,0.698,0.280,0.204,0.717,0.702), 
                  lowerbound = c(0.250,0.189,0.685,0.691,0.241,0.183,0.674,0.687,0.243,0.191,0.679,0.688),
                  upperbound = c(0.295,0.204,0.732,0.709,0.296,0.202,0.733,0.710,0.318,0.216,0.756,0.717),
                  intervention = c("Control","Any Intervention",
                                   "Control","Any Intervention",
                                   "Control","Any Intervention",
                                   "Control","Any Intervention",
                                   "Control","Any Intervention",
                                   "Control","Any Intervention"),
                  Outcome = c("Any knowledge gap","Any knowledge gap",
                                        "Any link demanded","Any link demanded",
                                        "Any knowledge gap","Any knowledge gap",
                                        "Any link demanded","Any link demanded",
                                        "Any knowledge gap","Any knowledge gap",
                                        "Any link demanded","Any link demanded"),
                  Panel = c("All","All","All","All",
                            "African-American","African-American","African-American","African-American",
                            "Latinx","Latinx","Latinx","Latinx"))

df1$ci <- paste("(",df1$lowerbound,", ",df1$upperbound,")", sep="")

df1 <- df1 %>%
  arrange(val) %>%
  mutate(Panel = factor(Panel, levels=c("All", "African-American","Latinx"))) %>%
  mutate(Outcome = factor(Outcome, levels = c("Any knowledge gap","Any link demanded"))) %>%
  mutate(intervention = factor(intervention, levels = c("Control","Any Intervention")))

df2 <- data.frame( val = c(1,2,3,4,5,6), #used to reorder factors
                   OddsRatio = c(0.630,0.957,0.625,0.967,0.637,0.940), 
                   lowerbound = c(0.556,0.848,0.532,0.830,0.521,0.770),
                   upperbound = c(0.714,1.081,0.735,1.128,0.777,1.048),
                   Outcome = c("Any knowledge gap",
                               "Any link demanded",
                               "Any knowledge gap",
                               "Any link demanded",
                               "Any knowledge gap",
                               "Any link demanded"),
                   Panel = c("All","All",
                             "African-American","African-American",
                             "Latinx","Latinx"))

df2$ci <- paste("(",df2$lowerbound,", ",df2$upperbound,")", sep="")


df2 <- df2 %>%
  arrange(val) %>%
  mutate(Panel = factor(Panel, levels=c("All", "African-American","Latinx"))) %>%
  mutate(Outcome = factor(Outcome, levels = c("Any knowledge gap","Any link demanded")))


## WITH INTERVENTIONS AS FACETS
# MEAN ODDS
# clean base plot 
plot_base_clean <- ggplot(data = df1, mapping = aes(x = Outcome, y = Odds, ymin = lowerbound, ymax = upperbound, fill = Panel)) + 
  # apply basic black and white theme - this theme removes the background colour by default
  theme_bw() + 
  # remove gridlines. Panel.grid.major is for vertical lines, Panel.grid.minor is for horizontal lines
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        # remove borders
        panel.border = element_blank(),
        # removing borders also removes x and y axes. Add them back
        axis.line = element_line())

Odds_histogram <- plot_base_clean + geom_bar(stat = "identity", position = position_dodge()) +
  facet_grid(. ~ intervention) +
  scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  theme(# remove background colour from facet labels
        #strip.background  = element_blank(),
        # remove border from facet label
        panel.border = element_blank()) +
  geom_errorbar(position = position_dodge(width=0.9), width=.1) +
  geom_text(aes(label = Odds), size = 3, position = position_dodge(width=0.9), vjust = 5) +
  geom_text(aes(label = ci), size = 3, position = position_dodge(width=0.9), vjust = 7) 

last_plot()
ggsave("./Output/odds.png", width = 15, height = 5)

#ODDS RATIO
# clean base plot 
plot_base_clean <- ggplot(data = df2, mapping = aes(x = Outcome, y = OddsRatio, ymin = lowerbound, ymax = upperbound, fill = Panel)) + 
  # apply basic black and white theme - this theme removes the background colour by default
  theme_bw() + 
  # remove gridlines. Panel.grid.major is for vertical lines, Panel.grid.minor is for horizontal lines
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        # remove borders
        panel.border = element_blank(),
        # removing borders also removes x and y axes. Add them back
        axis.line = element_line())

Odds_histogram <- plot_base_clean + geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  theme(# remove background colour from facet labels
    #strip.background  = element_blank(),
    # remove border from facet label
    panel.border = element_blank()) +
  geom_errorbar(position = position_dodge(width=0.9), width=.1) +
  geom_text(aes(label = OddsRatio), size = 3, position = position_dodge(width=0.9), vjust = 11) +
  geom_text(aes(label = ci), size = 3, position = position_dodge(width=0.9), vjust = 13) +
  geom_hline(yintercept=1 , linetype = 'dashed') +
  scale_y_continuous(breaks = sort(c(0, 0.5, 1, 1.5)))


last_plot()
ggsave("./Output/oddsratios.png", width = 15, height = 5)

