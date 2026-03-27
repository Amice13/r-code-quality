## Eszter Hargittai & Aaron Shaw
## 2019
## 02-gen_figure.R

## Description of this file:
## Loads libraries and creates objects necessary to reproduce Figure 1
## of Hargittai & Shaw, 2019.

library(ggplot2)
library(reshape2)
library(forcats)

setwd("~/research/web_use_US_survey/mturk_norc_comparison/analysis/")

if(!exists("d")){
    source("01-import.R", echo=T)
}

### Manipulate dataset into graphing format:
std.dv <- d[, names(d) %in% c("id", "amtsample",
                              as.character(dv.map$sd.varname))]
std.dv.melt <- melt(std.dv, id=c("id", "amtsample"))
cast.std.dv <- dcast(std.dv.melt, amtsample ~ variable, mean, na.rm=T)
cast.std.dv$amtsample <- ordered(c("NORC", "AMT"))
names(cast.std.dv) <- c("source",as.character(dv.map$var.name))
### reorder
std.means <- melt(cast.std.dv, id="source")


### Call to generate the plot
fig1 <- ggplot(std.means, aes(y=value,
                            x=fct_reorder(variable,
                                          -as.numeric(variable)),
                            shape=source, color=source)) +
    geom_point(size=5) + scale_colour_brewer("Data source", type="qual",
                                             palette="Dark2",
                                             direction=-1) +
    scale_x_discrete("Variable\n", labels=rev(dv.map$var.name.pretty)) +
    scale_y_continuous("\nStandardized mean (standard deviation units relative to full distribution)", limits=c(-.5,.5)) +
    guides(color= guide_legend("Data source"),
           shape=guide_legend("Data source")) +
    theme_light() +
    coord_flip()

## Uncomment the next line to inspect output before saving
## fig1

ggsave("figures/DV_std_means.pdf", plot = fig1, device="pdf", width=8, height=6,
       units="in")
ggsave("figures/DV_std_means.png", plot = fig1, device="png", width=8, height=6,
       units="in")
