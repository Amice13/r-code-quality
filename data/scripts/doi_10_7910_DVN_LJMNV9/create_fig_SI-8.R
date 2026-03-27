# This R-file is generating the Figure SI-8

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

# Coefficients plot - Standardized DV - DV: IHE, BRB

std_cols2 = c('IHE', 'BRB', 'ltd' , 'lpop' , 'matriccert_pcnt_08' , 'europe_pcnt_08' , 'pop_share1829' , 'pop_share3049' , 'pop_share5065'  , 'pop_share66' , 'asia_pcnt_08' , 'religion_pcnt_08')
std_cols = c('ltd' , 'lpop' , 'matriccert_pcnt_08' , 'europe_pcnt_08' , 'pop_share1829' , 'pop_share3049' , 'pop_share5065'  , 'pop_share66' , 'asia_pcnt_08' , 'religion_pcnt_08')
data_table_format = as.data.table(data)
data_table_format[, c(std_cols2) := lapply(.SD, scale), .SDcols = std_cols2]

## DV = IHE
model2_IHE = lm_robust(IHE ~ ltd + lpop + matriccert_pcnt_08 + europe_pcnt_08 + pop_share1829 + pop_share3049 + pop_share5065  + pop_share66 + asia_pcnt_08 + religion_pcnt_08,
                       data = data_table_format %>% filter(period == 2), clusters = area_num, se_type = 'stata', weights = potential)
model3_IHE = lm_robust(IHE ~ ltd + lpop + matriccert_pcnt_08 + europe_pcnt_08 + pop_share1829 + pop_share3049 + pop_share5065  + pop_share66 + asia_pcnt_08 + religion_pcnt_08,
                       data = data_table_format %>% filter(period == 3), clusters = area_num, se_type = 'stata', weights = potential)
model4_IHE = lm_robust(IHE ~ ltd + lpop + matriccert_pcnt_08 + europe_pcnt_08 + pop_share1829 + pop_share3049 + pop_share5065  + pop_share66 + asia_pcnt_08 + religion_pcnt_08,
                       data = data_table_format %>% filter(period == 4), clusters = area_num, se_type = 'stata', weights = potential)
## DV = BRB
model2_BRB = lm_robust(BRB ~ ltd + lpop + matriccert_pcnt_08 + europe_pcnt_08 + pop_share1829 + pop_share3049 + pop_share5065  + pop_share66 + asia_pcnt_08 + religion_pcnt_08,
                       data = data_table_format %>% filter(period == 2), clusters = area_num, se_type = 'stata', weights = potential)
model3_BRB = lm_robust(BRB ~ ltd + lpop + matriccert_pcnt_08 + europe_pcnt_08 + pop_share1829 + pop_share3049 + pop_share5065  + pop_share66 + asia_pcnt_08 + religion_pcnt_08,
                       data = data_table_format %>% filter(period == 3), clusters = area_num, se_type = 'stata', weights = potential)
model4_BRB = lm_robust(BRB ~ ltd + lpop + matriccert_pcnt_08 + europe_pcnt_08 + pop_share1829 + pop_share3049 + pop_share5065  + pop_share66 + asia_pcnt_08 + religion_pcnt_08,
                       data = data_table_format %>% filter(period == 4), clusters = area_num, se_type = 'stata', weights = potential)

summ_table <- data.frame(matrix(0, ncol = 6, nrow = 60))
colnames(summ_table) = c("variables","estimate","conf_high","conf_low","DV","period")
summ_table$variables = rep(c('Distance to Tel Aviv (log)' , 'Population (log)' ,
                             'High-school matriculation' , 'Share European descent' ,
                             'Pop share ages 18-29' , 'Pop share ages 30-49' , 'Pop share ages 50-65'  ,
                             'Pop share ages 66+' , 'Share Asian descent' , 'Share Jews'),6)
summ_table$DV = c(rep(c("I-H exposure"), 30), rep(c("Rgiht-bloc vote share"), 30))
summ_table$period = c(rep(c(rep("period2",10) , rep("period3",10), rep("period4", 10)), 2))

model_list <- list(model2_IHE,model3_IHE,model4_IHE,model2_BRB,model3_BRB,model4_BRB)

for (i in 1:6){
  temp = broomExtra::tidy(model_list[i][[1]])
  for (j in 1:10){
    rows = (i-1)*10 + j
    cols = summ_table$variables[rows]
    summ_table$estimate[rows] <- temp[temp$term == std_cols[j], "estimate"][[1]]
    summ_table$conf_high[rows] <- temp[temp$term == std_cols[j], "conf.high"][[1]]
    summ_table$conf_low[rows] <- temp[temp$term == std_cols[j], "conf.low"][[1]]
  }
}

p1 <- summ_table %>%
  mutate(variables = factor(variables,
                            levels = c('Pop share ages 66+','Pop share ages 50-65','Pop share ages 30-49',
                                       'Pop share ages 18-29' ,'Distance to Tel Aviv (log)','Population (log)',
                                       'High-school matriculation', 'Share European descent','Share Asian descent','Share Jews'),
                            labels = c('Pop share ages 66+','Pop share ages 50-65','Pop share ages 30-49',
                                       'Pop share ages 18-29' ,'Distance to Tel Aviv (log)','Population (log)',
                                       'High-school matriculation', 'Share European descent','Share Asian descent','Share Jews'))) %>%
  ggplot()+
  geom_linerange(aes(x = variables, ymin = conf_low, ymax = conf_high, color = period), alpha = 0.5,position = position_dodge(width=0.5)) +
  geom_point(aes(x = variables, y = estimate, group = period),position = position_dodge(width=0.5)) +
  geom_hline(yintercept = 0,  color = "black", linetype = 2) +
  scale_color_discrete(name = "Year",
                       breaks = c("period2","period3","period4"),
                       labels = c("2009","2013","2015")) +
  ylab("Estimates (standardized coeff)") +
  xlab("") +
  coord_flip() +
  facet_wrap(~DV) +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        strip.text = element_text(size = 12))

ggsave(filename = "Figures/Fig_SI-8.pdf",
       plot = p1,
       bg = "transparent",
       width = 12, height = 8, units = "in",dpi = 1000)
