rm(list = ls())


## data "GaltonFamilies" from the R package "HistData"
## family: family ID, a factor with levels 001-204
## father: height of father
## mother: height of mother
## midparentHeight: mid-parent height, calculated as (father + 1.08*mother)/2
## children: number of children in this family
## childNum: number of this child within family. Children are listed in decreasing order of height for boys followed by girls
## gender: child gender, a factor with levels female male
## childHeight: height of child

GaltonFamilies = read.table("GaltonFamilies.txt", header = TRUE)

## This data set lists the individual observations for 934 children in 205 families 
## on which Galton (1886) based his cross-tabulation.
## The following analysis is based on Hanley (2004)'s data, 
## which are slightly different from Galton's original analysis. 

xx = GaltonFamilies$midparentHeight
yy = GaltonFamilies$childHeight

## center of the data
center_x = mean(xx)
center_y = mean(yy)
## standard deviations and correlation
sd_x   = sd(xx)
sd_y   = sd(yy)
rho_xy = cor(xx, yy)
## OLS
beta_fit  = rho_xy*sd_y/sd_x
alpha_fit = center_y - beta_fit*center_x

pdf("galton_data_ggplot.pdf", height = 5, width = 5)

library("ggplot2")
ggplot(GaltonFamilies, 
       aes(x=midparentHeight, y=jitter(childHeight))) + 
  geom_point(size=0.6, col = "grey") +
  annotate("point", x=center_x, y=center_y) +
  geom_abline(intercept = alpha_fit, 
              slope = beta_fit, alpha = 0.6) + 
  annotate("text", x=72, y=57, 
           label=paste0("fitted line: ",
                        "y=", round(alpha_fit, 2),
                        "+", round(beta_fit, 2),
                        "x", sep = "")) +
  ylab("childHeight") +
  ggtitle("Galton's regression") + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5))

dev.off()









