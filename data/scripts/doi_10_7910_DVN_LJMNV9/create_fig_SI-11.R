# This R-file is generating the Figure SI-11

# Some more packages

library(tidyverse)
library(data.table)
library(haven)
library(estimatr)
library(texreg)

### Load data
setwd("~/Dropbox/Israeli Newspapers Study/JOP_REPLICATION/Dataverse submission")
getwd()

data <- read_dta('Data/yeshuv_period_final_Figs.dta')

# prepare data
std_cols = c('IVsm', 'ltd' , 'lpop' , 'matriccert_pcnt_08' , 'europe_pcnt_08' , 'pop_share1829' , 'pop_share3049' , 'pop_share5065'  , 'pop_share66' , 'asia_pcnt_08' , 'religion_pcnt_08')
data_table_format = as.data.table(data)
data_table_format[, c(std_cols) := lapply(.SD, scale), .SDcols = std_cols]

model2_IHE = lm_robust(IHE ~ IVsm + ltd + lpop + matriccert_pcnt_08 + europe_pcnt_08 + pop_share1829 + pop_share3049 + pop_share5065  + pop_share66 + asia_pcnt_08 + religion_pcnt_08,
                       data = data_table_format %>% filter(period == 2),
                       clusters = area_num, se_type = 'stata',
                       weights = potential)
model3_IHE = lm_robust(IHE ~ IVsm + ltd + lpop + matriccert_pcnt_08 + europe_pcnt_08 + pop_share1829 + pop_share3049 + pop_share5065  + pop_share66 + asia_pcnt_08 + religion_pcnt_08,
                       data = data_table_format %>% filter(period == 3),
                       clusters = area_num, se_type = 'stata',
                       weights = potential)
model4_IHE = lm_robust(IHE ~ IVsm + ltd + lpop + matriccert_pcnt_08 + europe_pcnt_08 + pop_share1829 + pop_share3049 + pop_share5065  + pop_share66 + asia_pcnt_08 + religion_pcnt_08,
                       data = data_table_format %>% filter(period == 4),
                       clusters = area_num, se_type = 'stata',
                       weights = potential)

coeficient <- list('IVsm' = 'Yediot readership 2007',
                   'religion_pcnt_08' = 'Share Jews',
                   'asia_pcnt_08' = 'Share Asian descent',
                   'europe_pcnt_08' = 'Share European descent',
                   'matriccert_pcnt_08' = 'High-school matriculation',
                   'lpop' = 'Population (log)',
                   'ltd' = 'Distance to Tel Aviv (log)',
                   'pop_share1829' = 'Pop share ages 18-2',
                   'pop_share3049' = 'Pop share ages 30-49',
                   'pop_share5065' = 'Pop share ages 50-65',
                   'pop_share66' = 'Pop share ages 66+')

# Regression plot - y over fitted y

period2 = data_table_format %>% filter(period == 2)
period3 = data_table_format %>% filter(period == 3)
period4 = data_table_format %>% filter(period == 4)

period2$predicted_IHE = predict(model2_IHE, period2)
period3$predicted_IHE = predict(model3_IHE, period3)
period4$predicted_IHE = predict(model4_IHE, period4)

predicted = bind_rows(period2, period3, period4)

p6 = ggplot(predicted %>% mutate(period = case_when(period == 2 ~ "2009",period == 3 ~ "2013",period == 4 ~ "2015"))) +
  geom_point(aes(x = predicted_IHE, y=IHE), alpha = 0.6, color = "lightsteelblue4", show.legend = F) +
  geom_smooth(aes(x = predicted_IHE, y=IHE), method = 'lm') +
  ggpubr::stat_cor(aes(x = predicted_IHE, y=IHE), method = "spearman", cor.coef.name = "rho",p.accuracy = 0.001, label.x = 0, label.y = 55) +
  scale_size(range = c(0.05,5),name="Population Size", trans = "log10") +
  #scale_x_log10() +
  facet_wrap(~period) +
  ylab("IH exposure") +
  xlab("Predicted IH exposure") +
  theme_bw() +
  theme(axis.title.x = element_text(size=12.5, face="bold", colour = "black"),
        axis.title.y = element_text(size=12.5, face="bold", colour = "black"),
        strip.text.x = element_text(size=10, face="bold", colour = "black"))

ggsave(filename = "Figures/Fig_SI-11.pdf",
       plot = p6,
       bg = "transparent",
       width = 12, height = 6, units = "in",dpi = 1000)
