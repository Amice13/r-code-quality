setwd(...)

library(GGally)
library(ggplot2)
library(foreign)

populism <- read.dta("correlations.dta")

model <- lm(s_expresion ~ ipop2d+ideol+female+age+epices+knowledge, data=populism)
summary(model)

correlation <- c("pt_noleader", "s_expresion", "s_libertad", "s_minoria", "ipop2d", "ideol", "s_impuestos", "galtan")
populismsel <- populism[correlation]

?ggpairs
ggpairs(populismsel, 
        lower = list(continuous = wrap("density", alpha = 0.5 )),
        columnLabels = c("Separation", "Expression", "Freedom", "Minorities","Populism","Ideology", "Taxes", "GAL/TAN")) 
