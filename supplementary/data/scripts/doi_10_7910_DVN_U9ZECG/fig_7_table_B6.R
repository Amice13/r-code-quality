rm(list = ls())
library(data.table)
#library(stargazer) stargazer does not operate estimatr
library(modelsummary)
library(estimatr)
library(ggthemes)
library(ggplot2)
library(ggpubr)
library(here)


urna <- readRDS(here("data","data_census.rds"))

urna <- urna[p_elected ==0 & lec_elected ==1]

d_plot <- data.table(var = as.character(),
                     coef = as.numeric(),
                     upper_ci = as.numeric(),
                     lower_ci = as.numeric(),
                     stringsAsFactors = FALSE
)

lmr2 <- lm_robust(no_support ~ zrich, data = urna, fixed_effects = ~ muni_code)
data_plot_lmr2 <- data.table(var = 'Rich voters',
                             coef = lmr2$coefficients[[1]],
                             upper_ci = lmr2$conf.high[[1]],
                             lower_ci = lmr2$conf.low[[1]],
                             stringsAsFactors = FALSE
)

lmr3 <- lm_robust(no_support ~ zpoor, data = urna, fixed_effects = ~ muni_code)
data_plot_lmr3 <- data.table(var = 'Poor voters',
                             coef = lmr3$coefficients[[1]],
                             upper_ci = lmr3$conf.high[[1]],
                             lower_ci = lmr3$conf.low[[1]],
                             stringsAsFactors = FALSE
)
lmr4 <- lm_robust(no_support ~ zprop_nonwhite, data = urna, fixed_effects = ~ muni_code)
data_plot_lmr4 <- data.table(var = 'Non-white voters',
                             coef = lmr4$coefficients[[1]],
                             upper_ci = lmr4$conf.high[[1]],
                             lower_ci = lmr4$conf.low[[1]],
                             stringsAsFactors = FALSE
)
lmr5 <- lm_robust(no_support ~ zprop_local_total_young_men, data = urna, fixed_effects = ~ muni_code)
data_plot_lmr5 <- data.table(var = 'Young male voters',
                             coef = lmr5$coefficients[[1]],
                             upper_ci = lmr5$conf.high[[1]],
                             lower_ci = lmr5$conf.low[[1]],
                             stringsAsFactors = FALSE
)

data_plot <- rbind(d_plot, data_plot_lmr2, data_plot_lmr3, data_plot_lmr4, data_plot_lmr5)


plot_corr <- ggplot(data_plot, aes(x = var, y = coef)) +
  geom_errorbar(aes(ymax = (upper_ci), ymin = (lower_ci)), width=0,size=.75,position=position_dodge(width=0.9)) +
  geom_point(alpha=.9,size=2.5) +
  geom_hline(yintercept=0, linetype="dotted",colour='gray20') +
  theme_minimal(base_size = 12) +
  scale_colour_grey() +
  theme(legend.position = "none") +
  coord_flip() +
  xlab("") +
  ylab('Coefficient') +
  NULL

ggsave(here("writing","img","fig_7.pdf"), plot = plot_corr, device = 'pdf',height = 10, width = 15, units = 'cm')

# TABLE B6

modelsummary(list(lmr2,lmr3,lmr4,lmr5),
             title = 'Correlates of (lack of) support for law-and-order candidates in São Paulo state.',
             gof_omit = 'R2|se_type|AIC|BIC|RMSE',
             output = 'latex',
             coef_rename = c('zrich'= 'Rich voters','zpoor'= 'Poor voters','zprop_nonwhite' = 'Nonwhite voters', 'zprop_local_total_young_men'= 'Young, male voters'))
