## Hager / Hilbig - Inheritance replication code - Feb 26 2019
## hhilbig@g.harvard.edu
##
## This file : Inheritance and wages and Prussia
## Tables reproduced in this file: Figure A3
##
### ### ### ###

rm(list = ls())

## Load packages

library(ggplot2)
library(pbapply)

## Load the data

ip01 <- read.csv('IPEHD_1901.csv',
                 stringsAsFactors = F, encoding = 'UTF-8')
ip92 <- read.csv('IPEHD_1892.csv',
                 stringsAsFactors = F, encoding = 'UTF-8')

## Remove some variables

ip01 <- ip01 %>%
  dplyr::select(-one_of(c('LAND', 'RB')))
ip92 <- ip92 %>%
  dplyr::select(-one_of(c('LAND', 'RB')))

## save

write.csv(ip01, 'IPEHD_1901.csv', row.names = F)
write.csv(ip92, 'IPEHD_1892.csv', row.names = F)

## Now, we scale the dependent variables

## 1901

ip01$wage_male_ur <- scale(ip01$wage1901_ov16_m_ur)
ip01$wage_male_ru <- scale(ip01$wage1901_ov16_m_ru)

## 1892

ip92$wage_male_ur <- scale(ip92$wage1892_ov16_m_ur)
ip92$wage_male_ru <- scale(ip92$wage1892_ov16_m_ru)

## Now, we run the main panel regressions that is reported in the paper

## w/ controls

m1_c <- summary(lm(wage_male_ur ~ fair_dic + law_baden_law +
                   law_common_law + law_dan_law + law_pruss_law +
                   law_code_civil + support_expenses_total_capita +
                   childlabor_mean_1898 + lon + lat, data = ip01))
m2_c <- summary(lm(wage_male_ru ~ fair_dic + law_baden_law +
                   law_common_law + law_dan_law + law_pruss_law +
                   law_code_civil + support_expenses_total_capita +
                   childlabor_mean_1898+ lon + lat , data = ip01))
m3_c <- summary(lm(wage_male_ur ~ fair_dic+ law_baden_law +
                   law_common_law + law_dan_law + law_pruss_law +
                   law_code_civil + support_expenses_total_capita +
                   childlabor_mean_1898 + lon + lat, data = ip92))
m4_c <- summary(lm(wage_male_ru ~ fair_dic + law_baden_law +
                   law_common_law + law_dan_law + law_pruss_law +
                   law_code_civil + support_expenses_total_capita +
                   childlabor_mean_1898+ lon + lat, data = ip92))

## Prior to plotting, wo obtain coefficients and standard errors

plot_df_c <- lapply(list(m1_c, m2_c, m3_c, m4_c), function(m) {
  data.frame(coef = m$coefficients[2, 1], se = m$coefficients[2,2])
})

## Note that the second row in the results matrix is the inheritance effect

## We now convert the list to a data frame

plot_df_c <- do.call('rbind', plot_df_c)

## Rename the dependent variables 

plot_df_c$outcome <- c("Urban wages, 1901", "Rural wages, 1901",
                     "Urban wages, 1892", "Rural wages, 1892")

## We also obtain the number of observations 

nobs <- sapply(list(m1_c, m2_c, m3_c, m4_c), function(x) length(x$residuals))

## We now add the number of observations to the outcome string

plot_df_c$outcome <- sapply(1:4, function(i) {
  paste0(plot_df_c$outcome[i], "\n(N = ", nobs[i], ")")
})

## Finally, we convert the outcome variable to a factor and reorder it

plot_df_c$outcome <- factor(plot_df_c$outcome, 
                          levels = plot_df_c$outcome[4:1])

#### Figure A3 : Wages in Prussia ####

## We now plot the results

pd <- position_dodge(0.4)

## 

p33a <- ggplot(data = plot_df_c, aes(y = coef, x = outcome)) +
  theme_bw() + xlab("") + ylab("")
p33a <- p33a + geom_hline(yintercept = 0, linetype = "dashed")
p33a <- p33a + geom_errorbar(aes(ymin= coef-1.96*se, ymax= coef+1.96*se),
                           width=0, alpha = 1, size = 0.4)
p33a <- p33a + geom_point(aes(y = coef, x = outcome), fill = "white",
                        size=3.5, position = pd, shape = 21) 
p33a <- p33a + theme(legend.title=element_blank())
p33a <- p33a + theme(panel.grid.major = element_blank())
p33a <- p33a + scale_color_grey(start = 0, end = 0)
p33a <- p33a + coord_flip() +  ylab("") + theme(legend.position = "bottom")
p33a
