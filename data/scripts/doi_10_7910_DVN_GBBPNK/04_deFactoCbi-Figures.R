require(dplyr)
require(tidyverse)
require(magrittr)
require(ggplot2)
require(ggdist)
require(cowplot)
require(readxl)

# set this to the directory where the Stata files have saved
# their output
# setwd("~/pathtofiles")


# make two overlapping plots for two different values on the left-right scale 

newcolnames <- paste0("v", 1:88)
populismsteps <- rep(seq(0,1,by=0.1), each=8)
leftrightsteps <- rep(seq(-3.5,3.5, by = 1), times = 11)

probitInteractMarginal <- read_xlsx("interactionmargins.xlsx",
                              col_names = newcolnames)
vars <- c("margin", "se", "z", "pvalue", "ll", "ul" ,"red1", "red2", "red3", "populism_steps", "leftrightsteps")

probitInteractMarginal <- rbind(probitInteractMarginal, populismsteps, leftrightsteps)
probitInteractMarginal$newnames <- vars

probitInteractMarginal %>% pivot_longer(cols = starts_with("v"),
                                  names_to = "steps",
                                  names_prefix = "v",
                                  values_to = "values") %>%
                     pivot_wider(names_from = newnames, values_from = values) %>%
                     select(-c(se,z,pvalue,red1,red2,red3)) %>%
                     filter(leftrightsteps == -3.5 | leftrightsteps == 3.5) %>%
                     mutate(leftright = as.factor(leftrightsteps)) -> probitInteractMarginalLong

rm(probitInteractMarginal)

probitIvMarginsPlot <-  ggplot(probitInteractMarginalLong, aes(x = populism_steps, y = margin, group = leftrightsteps)) +
  geom_ribbon(aes(ymin=ul, ymax=ll, fill = leftright), show.legend = FALSE) +
  geom_line(aes(linetype = leftright)) +
  ylab("Probability of Pressure on Central Bank") +
  xlab("Populism") +
  scale_y_continuous(breaks = c(0, 0.1, 0.2, 0.3)) +
  scale_x_continuous(breaks = c(0,0.25, 0.5, 0.75, 1)) +
  scale_fill_manual(values = c("gray75", "grey85"), name = "") +
  theme_bw()

probitIvMarginsPlot

# Figure A2 in the Appendix, top panel
ggsave("Figure-A2a_ProbitMarginalEffectInteraction.pdf", plot = probitIvMarginsPlot,
       width = 6, height = 4, dpi = 300)

# Repeat the same for the IV-LPM

lpmInteractMarginal <- read_xlsx("iv_interactionmargins.xlsx",
                                    col_names = newcolnames)
vars <- c("margin", "se", "z", "pvalue", "ll", "ul" ,"red1", "red2", "red3", "populism_steps", "leftrightsteps")

lpmInteractMarginal <- rbind(lpmInteractMarginal, populismsteps, leftrightsteps)
lpmInteractMarginal$newnames <- vars

lpmInteractMarginal %>% pivot_longer(cols = starts_with("v"),
                                        names_to = "steps",
                                        names_prefix = "v",
                                        values_to = "values") %>%
                      pivot_wider(names_from = newnames, values_from = values) %>%
                      select(-c(se,z,pvalue,red1,red2,red3)) %>%
                      filter(leftrightsteps == -3.5 | leftrightsteps == 3.5) %>%
                      mutate(leftright = as.factor(leftrightsteps)) -> lpmInteractMarginalLong

rm(lpmInteractMarginal)

# Figure A2 in the Appendix, bottom panel

lpmIvMarginsPlot <-  ggplot(lpmInteractMarginalLong, aes(x = populism_steps, y = margin, group = leftrightsteps)) +
  geom_ribbon(aes(ymin=ul, ymax=ll, fill = leftright), show.legend = FALSE) +
  geom_line(aes(linetype = leftright)) +
  ylab("Probability of Pressure on Central Bank") +
  xlab("Populism") +
  scale_y_continuous(breaks = c(0, 0.1, 0.2, 0.3)) +
  scale_x_continuous(breaks = c(0,0.25, 0.5, 0.75, 1)) +
  scale_fill_manual(values = c("gray75", "grey85"), name = "") +
  theme_bw()

lpmIvMarginsPlot

ggsave("Figure-A2b_IvLpmMarginalEffectInteraction.pdf", plot = lpmIvMarginsPlot,
       width = 6, height = 4, dpi = 300)

# Make histogram to plot the frequency underneath the marginal effects plot

probitHistogramData <- read.csv("PopulismHistogramData.csv", stringsAsFactors = FALSE)

probitHistogram <-  ggplot(probitHistogramData, aes(x = populism_vparty)) +
          geom_histogram(fill = "grey85", binwidth=0.01) +
  xlab("Degree of populism") +
  ylab("Count") +
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  xlim(0,1) +
  theme_bw()
  
probitHistogram

ggsave("Figure-5_ProbitMarginalHistogram.pdf", plot = probitHistogram,
       width = 6, height = 1.5, dpi = 300)

# Figure 6 in the paper, Mprobit model
MprobitMarginal1 <- read_xlsx("mprobit1margins.xlsx",
                              col_names = newcolnames)
vars <- c("dydx_pressure", "se", "z", "pvalue", "dydx_pressure_ll", "dydx_pressure_ul" ,"red1", "red2", "red3", "populism_steps", "leftrightsteps")

MprobitMarginal1 <- rbind(MprobitMarginal1, populismsteps, leftrightsteps)
MprobitMarginal1$newnames <- vars

MprobitMarginal1 %>% pivot_longer(cols = starts_with("v"),
                                  names_to = "steps",
                                  names_prefix = "v",
                                  values_to = "values") %>%
            pivot_wider(names_from = newnames, values_from = values) %>%
            select(-c(steps,se,z,pvalue,red1,red2,red3)) %>%
            filter(leftrightsteps == -3.5 | leftrightsteps == 3.5) %>%
            mutate(leftright = as.factor(leftrightsteps)) -> MprobitMarginal1Long


mprobit1 <- MprobitMarginal1Long %>%
            mutate(Outcome = 1, panel = 1) %>%
            filter(leftrightsteps == -3.5) %>%
            select(-c(leftrightsteps,leftright))

mprobit2 <- MprobitMarginal1Long %>%
            mutate(Outcome = 2, panel = 1) %>%
            filter(leftrightsteps == 3.5) %>%
            select(-c(leftrightsteps,leftright))

mprobit3 <- read.csv("Mprobit-CIs-PressureOutcome-interaction2.csv", stringsAsFactors = FALSE) %>%
            mutate(Outcome = 3, panel = 2)
mprobit4 <- read.csv("Mprobit-CIs-PressureOutcome-interaction3.csv", stringsAsFactors = FALSE) %>%
            mutate(Outcome = 4, panel = 3)

# small function to wrap the factor level text
fct_wrap <- function(f, ...) {
  levels(f) <- str_wrap(levels(f), ...)
  f
}

mprobitall <- rbind(mprobit1, mprobit2, mprobit3, mprobit4) %>%
            mutate(Outcome = as.factor(Outcome)) %>%
            # mutate(Outcome = fct_recode(Outcome,
            #                             "Economic hard left" = "1",
            #                             "Economic hard right" = "2",
            #                             "Economic left-right scale\nat observed values" = "3",
            #                             "Economic left-right scale\nat observed values"= "4")) %>%
            mutate(panel = as.factor(panel)) %>%
            mutate(panel = fct_recode(panel,
                                        "Pressure on CB" = "1",
                                        "Irregular turnover" = "2",
                                        "Weakening of CBI"= "3")) %>%
            mutate(panel = fct_wrap(panel, width = 20))

f_labels <- data.frame(panel = c("1", "2", "3"),
                       label = c("Economic hard left — vs hard right --", "", "")) %>%
            mutate(panel = as.factor(panel))


mprobitplot <-  ggplot(mprobitall, aes(x = populism_steps, y = dydx_pressure, group = Outcome, linetype = Outcome)) +
  geom_ribbon(aes(ymin=dydx_pressure_ul, ymax=dydx_pressure_ll), fill = "grey85") +
  geom_line() +
  xlab("Populism score (V-Party)") +
  ylab("Probability of Outcome relative to base category (no pressure)") +
  scale_y_continuous(breaks = c(0, 0.1, 0.2, 0.3)) +
  scale_x_continuous(breaks = c(0,0.25, 0.5, 0.75, 1)) +
  theme_bw() +
  facet_grid(rows = vars(panel),
#             scales = "free_y",
             switch = "y") +
  geom_hline(yintercept = 0) +
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        axis.title.y = element_blank(),
        legend.title=element_blank()) +
  guides(linetype = "none")

ggdraw(mprobitplot) + draw_plot_label(x = c(.2,.5,0.1,0.1), y = c(.92,.92,0.5,0.19),
                                      fontface = "plain",
                            label =  c("Economic hard left",  "Economic hard right",
                                       "Economic left-right scale at observed values",
                                       "Economic left-right scale at observed values"),
                            size = 9)
# For reasons unclear, for the annotations to show up, this plot needs to be saved manually from RStudio
# 
# 
ggsave("Figure-6_MultinProbitMarginalEffects.pdf", plot = mprobitplot,
       width = 6, height = 6, dpi = 300)

# Plots for the local projection impulse responses

inflationlps <- read.csv("D04_PressureInflation-LPgraphdata.csv", stringsAsFactors = FALSE)
# Make figures of the impulse responses in the paper and the online appendix

indicatorLevels <- c("Monetary Rate", "Inflation")
regimeTypeLevels <- c("Populist","non-Populist")

monetaryRateLpData <- inflationlps %>%
  pivot_longer(!quarterAfterShock, names_to = "variableName", values_to = "value") %>%
  mutate(regimeType = if_else(str_detect(variableName, "NonPop"), "non-Populist", "Populist")) %>%
  mutate(indicator = if_else(str_detect(variableName, "rate"), "Monetary Rate", "Inflation")) %>%
  mutate(series = case_when(
    str_detect(variableName, "b") ~ "beta",
    str_detect(variableName, "ul90") ~ "upperci90",
    str_detect(variableName, "ll90") ~ "lowerci90",
    str_detect(variableName, "ul95") ~ "upperci95",
    str_detect(variableName, "ll95") ~ "lowerci95",
    str_detect(variableName, "se") ~ "standarderror")) %>%
  select(-(variableName)) %>%
  pivot_wider(names_from = series, values_from = value) %>%
  arrange(indicator, regimeType,quarterAfterShock) %>%
  mutate(regimeType = fct_recode(regimeType,
                                 "Low Populism" = "non-Populist",
                              "High Populism" = "Populist")) %>%
  mutate(regimeType = fct_rev(regimeType)) %>%
  mutate(indicator = fct_recode(indicator,
                                 "Monetary Rate" = "Monetary Rate",
                                 "Inflation" = "Inflation")) %>%
  mutate(indicator = fct_rev(indicator))



localProjectionPlot <-  ggplot(monetaryRateLpData, aes(x = quarterAfterShock, y = beta)) +
  geom_ribbon(aes(ymin=upperci95, ymax=lowerci95), fill = "grey85") +
#  geom_ribbon(aes(ymin=upperci90, ymax=lowerci90), fill = "grey90") +
  geom_line(linetype = "dashed") +
  xlab("Quarters after pressure on CB") +
  ylab("Impulse response to pressure") +
  theme_bw() +
  facet_grid(rows = vars(indicator),
             cols = vars(regimeType),
             scales = "free_y",
             switch = "y") +
  geom_hline(yintercept = 0) +
  theme(strip.background = element_blank(),
        strip.placement = "outside",axis.title.y = element_blank())

localProjectionPlot
ggsave("Figure-7_localProjectionPlot.pdf", plot = localProjectionPlot,
       width = 6, height = 4, dpi = 300)
