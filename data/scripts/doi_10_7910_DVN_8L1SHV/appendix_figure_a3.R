## Installing necessary packages ----
packages <-c(
  "ggplot2", "foreign", "data.table", "stargazer", "lfe", "stringr", "xtable", 
  "tidyverse", "lme4", "arm", "memisc", "vegan", "MASS", "haven", "zoo")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}
## ----
#############################################
# Analysis of WVS Survey #
#############################################
rm(list = ls())
options(stringsAsFactors = FALSE)

### Libraries
library(ggplot2)
library(foreign)
library(data.table)
library(stargazer)

## 1. Load Data
wvs <- read.csv("WVS_plot_data.csv")



# being a housewife is as fullfilling
wvs$housewife <- wvs$D057
wvs$housewife[wvs$housewife == -5] <- NA
wvs$housewife[wvs$housewife == -2] <- NA
wvs$housewife[wvs$housewife == -1] <- NA
#table(wvs$housewife)
wvs$housewife <- labelled::to_factor(wvs$housewife)

wvs$men_make_better_leaders <- wvs$D059
wvs$men_make_better_leaders[wvs$men_make_better_leaders == -5] <- NA
wvs$men_make_better_leaders[wvs$men_make_better_leaders == -2] <- NA
wvs$men_make_better_leaders[wvs$men_make_better_leaders == -1] <- NA
#table(wvs$men_make_better_leaders)
wvs$men_make_better_leaders <- labelled::to_factor(wvs$men_make_better_leaders)


# uni is more important for boys 
wvs$uni_girl <- wvs$D060
wvs$uni_girl[wvs$uni_girl==-5] <- NA
wvs$uni_girl[wvs$uni_girl==-2] <- NA
wvs$uni_girl[wvs$uni_girl==-1] <- NA
#table(wvs$uni_girl)
wvs$uni_girl <- labelled::to_factor(wvs$uni_girl)


# politicians who don't believe in god are unfit for service
wvs$politician_god <- wvs$F102
wvs$politician_god[wvs$politician_god==-5] <- NA
wvs$politician_god[wvs$politician_god==-2] <- NA
wvs$politician_god[wvs$politician_god==-1] <- NA
#table(wvs$politician_god)
wvs$politician_god <- labelled::to_factor(wvs$politician_god)

setDT(wvs)
vars <- c("politician_god", "uni_girl", "men_make_better_leaders",
  "housewife", "year")
ggdat <- wvs[, vars, with = FALSE]
#ggdat <- data.table(apply(ggdat, 2, as.numeric))
ggdat$year <- as.factor(ggdat$year)
colnames(ggdat) <- c("Politicians should believe in God", 
  "Uni. more important for boys", "Men make better leaders", 
  "Being a housewife is as valuable", "year")


alt_ggdat <- melt(ggdat, measure.vars = colnames(ggdat), id.vars = "year")
#alt_ggdat$value <- as.character(alt_ggdat$value)
alt_ggdat <- alt_ggdat[complete.cases(alt_ggdat), ]
pctdat <- alt_ggdat[, .N, by = .(variable, value, year)][, 
  .(value, freq = N / sum(N)), by = .(variable, year)]
pctdat <- pctdat[-c(35:36), ]
ggplot(pctdat[variable != "year"], aes(x = value, y = freq, fill = year)) + 
  geom_bar(stat = "identity", position = position_dodge()) + theme_minimal() +
  facet_grid(~variable, scale = "free") + 
  scale_fill_brewer(palette = "Paired") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") + ylab("Frequency")

ggsave("~/Desktop/wvs_plot.png", dpi = 700, width = 10, height = 4, units = "in")

tiff("~/Desktop/wvs_plot.tiff", units="in", width=5, height=5, res=300)
ggplot(pctdat[variable != "year"], aes(x = value, y = freq, fill = year)) + 
  geom_bar(stat = "identity", position = position_dodge()) + theme_minimal() +
  facet_grid(~variable, scale = "free") + 
  scale_fill_brewer(palette = "Paired") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") + ylab("Frequency")
dev.off()




