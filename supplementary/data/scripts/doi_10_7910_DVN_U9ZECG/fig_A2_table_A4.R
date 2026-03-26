#Please restart R
#options(scipen=999)

rm(list = ls())
library(estimatr)
library(data.table)
library(here)
library(modelsummary)
library(tidyr)
library(ggplot2)

load(here("data","all_guarda.RData"))

table.data <- function(model, dv, ...) {
  ret_a <- data.frame(
    dependent_var = paste(dv),
    group = 'Security Committee',
    avg = model$coefficients[[2]] + model$coefficients[[1]],
    u_ci = model$coefficients[[1]] + model$conf.high[[2]],
    l_ci = model$coefficients[[1]] + model$conf.low[[2]]
  )
  ret_b <-data.frame(
    dependent_var = paste(dv),
    group = 'No Security Committee',
    avg = model$coefficients[[1]],
    u_ci = model$conf.high[[1]],
    l_ci = model$conf.low[[1]]
  )
  ret <- rbind(ret_a, ret_b)
  row.names(ret) <- NULL
  ret
}

reg1 <- lm_robust(deleg_mulher ~ sc , data = all_guarda[ lec ==1 & year_match == 2012 & p_sc == 0])
reg2 <- lm_robust(fundo_sp ~ sc  , data = all_guarda[ lec ==1 & year_match == 2012 & p_sc == 0])
reg3 <- lm_robust(plano_sp ~ sc , data = all_guarda[ lec ==1 & year_match == 2012 & p_sc == 0])
reg4 <- lm_robust(conseg ~ sc , data = all_guarda[ lec ==1 & year_match == 2012 & p_sc == 0])
reg5 <- lm_robust(I(conseg + fundo_sp + plano_sp ) ~ sc , data = all_guarda[lec ==1 & year_match == 2012 & p_sc == 0])

plot_data <- rbind(#table.data(reg1, 'Women\'s Affairs\nPD'),
                   table.data(reg2, 'Dedicated\nbudget'),
                   table.data(reg3, 'Multi-year\nplan'),
                   table.data(reg4, 'Community\ncouncils'),
                   table.data(reg5, 'Sum of\nMeasures'))



table_munic <- list('Dedicated budget' = reg2, 
                         'Multi-year plan' = reg3, 
                         'Community council' = reg4, 
                         'Sum of Measures' = reg5)

# THIS PRODUCES TABLE B.2

modelsummary(table_munic, gof_omit = 'DF|Deviance|R2|AIC|BIC', output = 'latex')


reg1s <- lm_robust(deleg_mulher ~ sc , data = all_guarda[pop < 50000 & lec ==1 & year_match == 2012])
reg2s <- lm_robust(fundo_sp ~ sc  , data = all_guarda[pop < 50000 & lec ==1 & year_match == 2012])
reg3s <- lm_robust(plano_sp ~ sc , data = all_guarda[pop < 50000 & lec ==1 & year_match == 2012])
reg4s <- lm_robust(conseg ~ sc , data = all_guarda[pop < 50000 & lec ==1 & year_match == 2012])
reg5s <- lm_robust(I(conseg + fundo_sp + plano_sp ) ~ sc , data = all_guarda[pop < 50000 & lec ==1 & year_match == 2012])

plot_2 <- ggplot(plot_data, aes(x=factor(dependent_var), y = avg, fill = group, group = group)) +
  geom_bar(stat="identity", position="dodge", color="black") +
  geom_errorbar(aes(ymax = (u_ci), ymin = (l_ci)),
                width=0,size=.75, position=position_dodge(width=0.9)) +
  scale_fill_grey() +
  theme_minimal(base_size = 12) +
  xlab("") +
  coord_cartesian(y = c(0, .5)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(legend.position = "bottom") +
  theme(legend.title=element_blank()) +
  ylab('Creation of public security measure') +
  NULL

#ggsave(here("Working papers","2015 policia","writing","img","presence_local_institutions.pdf"), plot = plot_2, device = 'pdf',height = 10, width = 15, units = 'cm')

plot_data_s <- rbind(#table.data(reg1s, 'Women\'s Affairs\nPD'),
                     table.data(reg2s, 'Dedicated\nbudget'),
                     table.data(reg3s, 'Multi-year\nplan'),
                     table.data(reg4s, 'Community\ncouncils'),
                     table.data(reg5s, 'Sum'))

reg1l <- lm_robust(deleg_mulher ~ sc , data = all_guarda[  pop > 50000 & lec ==1 & year_match == 2012])
reg2l <- lm_robust(fundo_sp ~ sc  , data = all_guarda[  pop > 50000 & lec ==1 & year_match == 2012])
reg3l <- lm_robust(plano_sp ~ sc , data = all_guarda[  pop > 50000 & lec ==1 & year_match == 2012])
reg4l <- lm_robust(conseg ~ sc , data = all_guarda[  pop > 50000 & lec ==1 & year_match == 2012])
reg5l <- lm_robust(I(conseg + fundo_sp + plano_sp ) ~ sc , data = all_guarda[pop > 50000 & lec ==1 & year_match == 2012])


plot_data_l <- rbind(#table.data(reg1l, 'Women\'s Affairs\nPD'),
                     table.data(reg2l, 'Dedicated\nbudget'),
                     table.data(reg3l, 'Multi-year\nplan'),
                     table.data(reg4l, 'Community\ncouncils'),
                     table.data(reg5l, 'Sum'))



plot_small <- ggplot(plot_data_s, aes(x=factor(dependent_var), y = avg, fill = group, group = group)) +
  geom_bar(stat="identity", position="dodge", color="black") +
  geom_errorbar(aes(ymax = (u_ci), ymin = (l_ci)),
                width=0,size=.75, position=position_dodge(width=0.9)) +
  scale_fill_grey() +
  theme_minimal(base_size = 12) +
  xlab("") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(legend.position = "bottom") +
  theme(legend.title=element_blank()) +
  ylab('Presence') +
  #coord_cartesian(y=c(0,.70)) +
  NULL

#ggsave(here("Working papers","2015 policia","writing","img","presence_local_institutions_small.pdf"), plot = plot_small, device = 'pdf',height = 10, width = 15, units = 'cm')

plot_large <- ggplot(plot_data_l, aes(x=factor(dependent_var), y = avg, fill = group, group = group)) +
  geom_bar(stat="identity", position="dodge", color="black") +
  geom_errorbar(aes(ymax = (u_ci), ymin = (l_ci)),
                width=0,size=.75, position=position_dodge(width=0.9)) +
  scale_fill_grey() +
  theme_minimal(base_size = 12) +
  xlab("") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(legend.position = "bottom") +
  theme(legend.title=element_blank()) +
  ylab('Presence') +
  #coord_cartesian(y=c(0,.70)) +
  NULL

#ggsave(here("Working papers","2015 policia","writing","img","presence_local_institutions_large.pdf"), plot = plot_large, device = 'pdf',height = 10, width = 15, units = 'cm')

