# This R-file is generating the Figure SI-13

library(tidyverse)
library(data.table)
library(haven)
library(estimatr)
library(latex2exp)
library(plyr)
require(RColorBrewer)

### Load data
setwd("~/Dropbox/Israeli Newspapers Study/JOP_REPLICATION/Dataverse submission")
getwd()


data <- read_dta('Data/yeshuv_period_final_Figs.dta')

std_cols = c('IHE', 'BRB', 'ltd' , 'lpop' , 'matriccert_pcnt_08' , 'europe_pcnt_08' , 'pop_share1829' , 'pop_share3049' , 'pop_share5065'  , 'pop_share66' , 'asia_pcnt_08' , 'religion_pcnt_08')
data_table_format = as.data.table(data)
data_table_format[, c(std_cols) := lapply(.SD, scale), .SDcols = std_cols]

## DV = IVsm
model4_IVsm = lm_robust(IVsm ~ ltd + lpop + matriccert_pcnt_08 + europe_pcnt_08 + pop_share1829 + pop_share3049 + pop_share5065  + pop_share66 + asia_pcnt_08 + religion_pcnt_08,
                        data = data_table_format %>% filter(period == 4),
                        clusters = area_num, se_type = 'stata',
                        weights = potential)
model4_table = broomExtra::tidy(model4_IVsm)
model4_table = model4_table %>%
  mutate(covariates = case_when(
    term == 'ltd' ~ "Distance to Tel Aviv (log)",
    term == 'lpop' ~ "Population (log)",
    term == 'matriccert_pcnt_08' ~ "High-school matriculation",
    term == 'europe_pcnt_08' ~ "Share European descent",
    term == 'pop_share1829' ~ "Pop share ages 18-29",
    term == 'pop_share3049' ~ "Pop share ages 30-49",
    term == 'pop_share5065' ~ "Pop share ages 50-65",
    term == 'pop_share66' ~ "Pop share ages 66+",
    term == 'asia_pcnt_08' ~ "Share Asian descent",
    term == 'religion_pcnt_08' ~ "Share Jews"),
    covariates = factor(covariates,
                        levels = c('Pop share ages 66+','Pop share ages 50-65','Pop share ages 30-49',
                                   'Pop share ages 18-29' ,'Distance to Tel Aviv (log)','Population (log)',
                                   'High-school matriculation', 'Share European descent','Share Asian descent','Share Jews'),
                        labels = c('Pop share ages 66+','Pop share ages 50-65','Pop share ages 30-49',
                                   'Pop share ages 18-29' ,'Distance to Tel Aviv (log)','Population (log)',
                                   'High-school matriculation', 'Share European descent','Share Asian descent','Share Jews')))

p3 <- model4_table[2:11,] %>%
  ggplot()+
  geom_linerange(aes(x = covariates, ymin = conf.low, ymax = conf.high), alpha = 0.5,
                 position = position_dodge(width=0.5)) +
  geom_point(aes(x = covariates, y = estimate),position = position_dodge(width=0.5)) +
  geom_hline(yintercept = 0,  color = "black", linetype = 2) +
  geom_text(aes(x = 9.5, y= 6.5),label = TeX(r'($R^2 = 0.67$)'), size=5.5) +
  ylab("Estimates (standardized coeff)") +
  xlab("") +
  coord_flip() +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 12))

ggsave(filename = "Figures/Fig_SI-13.pdf",
       plot = p3,
       bg = "transparent",
       width = 12, height = 8, units = "in",dpi = 1000)
