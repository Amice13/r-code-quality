rm(list = ls())
library(data.table)
library(rdrobust)
library(ggplot2)
library(ggthemes)
library(here)
#packages for the tables
library(tidyr)
library(kableExtra)
library(modelsummary)

load(here("data","ver_data.RData"))

tt <- unique(ver_b[law_enforcement ==1 & abs(marginal_muni) < 0.01, .(muni_code, year, law_enforcement, marginal_muni, pop, elected_muni, p_elected_muni, sc_before, dv_homicides, y2008, y2012)])

tt_pm <- unique(ver_b[law_enforcement ==1 & abs(marginal_muni_pm) < 0.01, .(muni_code, year, law_enforcement, marginal_muni_pm, pop, elected_pm_muni, p_elected_muni, sc_before, dv_homicides, y2008, y2012)])

tt_pm_nsc <- tt_pm[sc_before == 0]

# check if there is only one muni/year pair
identical(nrow(tt[,.(muni_code,year)]), nrow(unique(tt[,.(muni_code,year)]))) # doubles-check dataset

tt_a <- tt[law_enforcement == 1 & sc_before == 1]

#plot
group_a  <- with(tt_a, rdrobust(dv_homicides, marginal_muni, all=TRUE, bwselect = 'msetwo',covs = cbind(tt_a$y2008, tt_a$y2012)))
summary(group_a)

all.together <- data.frame(conventional_coef=as.numeric(),
                           robust_coef = as.numeric(),
                           conventional_upper = as.numeric(),
                           conventional_lower = as.numeric(),
                           robust_se=as.numeric(),
                           robust_upper = as.numeric(),
                           robust_lower = as.numeric(),
                           se_robust = as.numeric(),
                           n=as.numeric(),
                           bw=as.numeric(),
                           name=as.character(),
                           presence = as.character(),
                           stringsAsFactors=FALSE)

temp <- data.frame(conventional_coef = group_a$coef[[1]],
                   robust_coef = group_a$coef[[3]],
                   robust_se = group_a$se[[3]],
                   conventional_upper = group_a$ci[[4]],
                   conventional_lower = group_a$ci[[1]],
                   robust_upper = group_a$ci[[6]],
                   robust_lower = group_a$ci[[3]],
                   se_robust = group_a$se[[3]],
                   n = group_a$N_h[[1]] + group_a$N_h[[2]],
                   bw = group_a$bws[[2]],
                   estimation = 'Committee\nPresent',
                   presence = 'yep')

all.together = rbind(all.together,temp)

tt_b <- tt[law_enforcement == 1  & sc_before == 0]
#tt_b <- tt[law_enforcement == 1  & sc_before == 0 & lag_pop > median(unique(tt$lag_pop),na.rm = TRUE)]
group_b <- with(tt_b, rdrobust(dv_homicides, marginal_muni, all=TRUE, bwselect = 'msetwo', covs = cbind(tt_b$y2008, tt_b$y2012)))
summary(group_b)

temp <- data.frame(conventional_coef = group_b$coef[[1]],
                   robust_coef = group_b$coef[[3]],
                   robust_se = group_b$se[[3]],
                   conventional_upper = group_b$ci[[4]],
                   conventional_lower = group_b$ci[[1]],
                   robust_upper = group_b$ci[[6]],
                   robust_lower = group_b$ci[[3]],
                   se_robust = group_b$se[[3]],
                   n = group_b$N_h[[1]] + group_b$N_h[[2]],
                   bw = group_b$bws[[2]],
                   estimation = 'Committee\nNot present',
                   presence = 'not')

all.together = rbind(all.together,temp)

group_c <- with(tt_b, rdrobust(dv_homicides, marginal_muni, all=TRUE , bwselect = 'msetwo', covs = cbind(tt_b$y2008, tt_b$y2012, tt_b$gini, tt_b$lag_pop, tt_b$gdp_pc, tt_b$nonwhite)))
summary(group_c)

temp <- data.frame(conventional_coef = group_c$coef[[1]],
                   robust_coef = group_c$coef[[3]],
                   robust_se = group_c$se[[3]],
                   conventional_upper = group_c$ci[[4]],
                   conventional_lower = group_c$ci[[1]],
                   robust_upper = group_c$ci[[6]],
                   robust_lower = group_c$ci[[3]],
                   se_robust = group_c$se[[3]],
                   n = group_c$N_h[[1]] + group_c$N_h[[2]],
                   bw = group_c$bws[[2]],
                   estimation = 'Committee\nNot present\n(with controls)',
                   presence = 'not')

all.together = rbind(all.together,temp)

tt_d <- tt[law_enforcement == 1  & sc_before == 0 & pop > 50000]
group_d <- with(tt_d, rdrobust(dv_homicides, marginal_muni, all=TRUE, bwselect = 'msetwo', covs = cbind(tt_d$y2008, tt_d$y2012)))
summary(group_d)

temp <- data.frame(conventional_coef = group_d$coef[[1]],
                   robust_coef = group_d$coef[[3]],
                   robust_se = group_d$se[[3]],
                   conventional_upper = group_d$ci[[4]],
                   conventional_lower = group_d$ci[[1]],
                   robust_upper = group_d$ci[[6]],
                   robust_lower = group_d$ci[[3]],
                   se_robust = group_d$se[[3]],
                   n = group_d$N_h[[1]] + group_d$N_h[[2]],
                   bw = group_d$bws[[2]],
                   estimation = 'Committee\nNot present\n(Larger municip.)',
                   presence = 'not')

all.together = rbind(all.together,temp)

tt_e <- tt[law_enforcement == 1  & sc_before == 0 & pop < 50000]
group_e <- with(tt_e, rdrobust(dv_homicides, marginal_muni, all=TRUE, bwselect = 'msetwo', covs = cbind(tt_e$y2008, tt_e$y2012)))
summary(group_e)

temp <- data.frame(conventional_coef = group_e$coef[[1]],
                   robust_coef = group_e$coef[[3]],
                   robust_se = group_e$se[[3]],
                   conventional_upper = group_e$ci[[4]],
                   conventional_lower = group_e$ci[[1]],
                   robust_upper = group_e$ci[[6]],
                   robust_lower = group_e$ci[[3]],
                   se_robust = group_e$se[[3]],
                   n = group_e$N_h[[1]] + group_e$N_h[[2]],
                   bw = group_e$bws[[2]],
                   estimation = 'Committee\nNot present\n(Smaller municip.)',
                   presence = 'not')

all.together = rbind(all.together,temp)

group_pm <- with(tt_pm_nsc, rdrobust(dv_homicides, marginal_muni_pm, all=TRUE, bwselect = 'msetwo', covs = cbind(tt_pm_nsc$y2008, tt_pm_nsc$y2012)))
summary(group_pm)

temp <- data.frame(conventional_coef = group_pm$coef[[1]],
                   robust_coef = group_pm$coef[[3]],
                   robust_se = group_pm$se[[3]],
                   conventional_upper = group_pm$ci[[4]],
                   conventional_lower = group_pm$ci[[1]],
                   robust_upper = group_pm$ci[[6]],
                   robust_lower = group_pm$ci[[3]],
                   se_robust = group_pm$se[[3]],
                   n = group_pm$N_h[[1]] + group_pm$N_h[[2]],
                   bw = group_pm$bws[[2]],
                   estimation = 'Committee\nNot present\nPolice only',
                   presence = 'not')

all.together = rbind(all.together,temp)

all.together$estimation <- factor(all.together$estimation ,
                                  levels = c('Committee\nNot present',
                                             'Committee\nNot present\n(Smaller municip.)',
                                             'Committee\nNot present\n(Larger municip.)',
                                             'Committee\nNot present\n(with controls)',
                                             'Committee\nNot present\nPolice only',
                                             'Committee\nPresent'))

plot.jobs <- ggplot(all.together, 
                    aes(x = estimation, y = robust_coef, shape = presence, color = presence)) +
  geom_errorbar(aes(ymax = (robust_upper), ymin = (robust_lower)),
                width = 0, linewidth = .75, position=position_dodge(width=0.9)) +
  geom_point(alpha=.9,size=2.5) +
  geom_hline(yintercept=0, linetype="dotted",colour='gray20') +
  theme_minimal(base_size = 12) +
  scale_colour_grey() +
  theme(legend.position = "none") +
  #theme(legend.title=element_blank()) +
  xlab("") +
  geom_text(aes(label=paste('n = ',n)), y=46,  colour="black", size=3, fontface = "italic") +
  ylab('Difference in Homicide Rate\n(Yearly deaths per 100,000 pop.)') +
  NULL

ggsave(here("writing","img","fig_4.pdf"), plot = plot.jobs, device = 'pdf',height = 10, width = 20, units = 'cm')

# TABLE

# Load functions for modelsummary 

tidy.rdrobust <- function(model, ...) {
  ret <- data.frame(
    term = 'Robust Coef.',
    estimate = model$coef[3, 1],
    std.error = model$se[3, 1],
    p.value = model$pv[3, 1]
  )
  row.names(ret) <- NULL
  ret
}

glance.rdrobust <- function(model, ...) {
  ret <- data.frame(
    #Kernel = model$kernel,
    Bandwidth = paste(round(model$bws[2,2],4) * 100,'%'),
    #Bw.Selection = model$bwselect,
    N.obs = as.character(model$N_h[[1]] + model$N_h[[2]])
  )
  ret
}

# committe present ----

# Benchmark

robust_all <- rdrobust(tt_a$dv_homicides,tt_a$marginal_muni, all = TRUE, bwselect = 'msetwo', 
                       covs = cbind(tt_a$y2008,tt_a$y2012))

# MSERD

robust_mserd <- rdrobust(tt_a$dv_homicides,tt_a$marginal_muni, all = TRUE, bwselect = 'mserd', 
                         covs = cbind(tt_a$y2008,tt_a$y2012))
# No previous LOC

tt_aa <- tt_a[p_elected_muni == 0]
robust_p <- rdrobust(tt_aa$dv_homicides,tt_aa$marginal_muni, all = TRUE, bwselect = 'msetwo', 
                     covs = cbind(tt_aa$y2008,tt_aa$y2012))

# second-degree polynomial

robust_2 <- rdrobust(tt_a$dv_homicides,tt_a$marginal_muni, all = TRUE, p = 2,
                     bwselect = 'msetwo', 
                     covs = cbind(tt_a$y2008,tt_a$y2012))


table_committee <- list('Benchmark' = robust_all, 
                  'MSE-optimal' = robust_mserd, 
                  '2nd Polynomial' = robust_2, 
                  'No previous L&O' = robust_p)

# TABLE A23
modelsummary(table_committee, 
             statistic = c("[{std.error}]",'p.value'),
             output = 'latex',
             title = 'Effect of the election of law-and-order candidates on Homicides, Municipalities with Security Committee') %>%
  footnote(general = 'Nonparametric estimations (MSE-two selection, unless noted) with year fixed effects, robust standard errors in brackets, p-values in parentheses.', threeparttable = TRUE)


# no committee present ----

# Benchmark

robust_all <- rdrobust(tt_b$dv_homicides,tt_b$marginal_muni, all = TRUE, bwselect = 'msetwo', 
                       covs = cbind(tt_b$y2008,tt_b$y2012))

# MSERD

robust_mserd <- rdrobust(tt_b$dv_homicides,tt_b$marginal_muni, all = TRUE, bwselect = 'mserd', 
                         covs = cbind(tt_b$y2008,tt_b$y2012))
# No previous LOC

tt_ba <- tt_b[p_elected_muni == 0]
robust_p <- rdrobust(tt_ba$dv_homicides,tt_ba$marginal_muni, all = TRUE, bwselect = 'msetwo', 
                     covs = cbind(tt_ba$y2008,tt_ba$y2012))

# second-degree polynomial

robust_2 <- rdrobust(tt_b$dv_homicides,tt_b$marginal_muni, all = TRUE, p = 2,
                     bwselect = 'msetwo', 
                     covs = cbind(tt_b$y2008,tt_b$y2012))


table_no_committee <- list('Benchmark' = robust_all, 
                        'MSE-optimal' = robust_mserd, 
                        '2nd Polynomial' = robust_2, 
                        'No previous L&O' = robust_p)

# Table A24
modelsummary(table_no_committee, 
             statistic = c("[{std.error}]",'p.value'),
             output = 'latex',
             title = 'Effect of the election of law-and-order candidates on Homicides, Municipalities without Security Committee') %>%
  footnote(general = 'Nonparametric estimations (MSE-two selection, unless noted) with year fixed effects, robust standard errors in brackets, p-values in parentheses.', threeparttable = TRUE)

# police ----

# Benchmark

robust_all <- rdrobust(tt_pm_nsc$dv_homicides,tt_pm_nsc$marginal_muni, all = TRUE, bwselect = 'msetwo', 
                       covs = cbind(tt_pm_nsc$y2008,tt_pm_nsc$y2012))

# MSERD

robust_mserd <- rdrobust(tt_pm_nsc$dv_homicides,tt_pm_nsc$marginal_muni, all = TRUE, bwselect = 'mserd', 
                         covs = cbind(tt_pm_nsc$y2008,tt_pm_nsc$y2012))
# No previous LOC

tt_pm_nsca <- tt_pm_nsc[p_elected_muni == 0]
robust_p <- rdrobust(tt_pm_nsca$dv_homicides,tt_pm_nsca$marginal_muni, all = TRUE, bwselect = 'msetwo', 
                     covs = cbind(tt_pm_nsca$y2008,tt_pm_nsca$y2012))

# second-degree polynomial

robust_2 <- rdrobust(tt_pm_nsc$dv_homicides,tt_pm_nsc$marginal_muni, all = TRUE, p = 2,
                     bwselect = 'msetwo', 
                     covs = cbind(tt_pm_nsc$y2008,tt_pm_nsc$y2012))


table_pm_no_committee <- list('Benchmark' = robust_all, 
                           'MSE-optimal' = robust_mserd, 
                           '2nd Polynomial' = robust_2, 
                           'No previous L&O' = robust_p)

#TABLE A25
modelsummary(table_pm_no_committee, 
             statistic = c("[{std.error}]",'p.value'),
             output = 'latex',
             title = 'Effect of the election of POLICE law-and-order candidates on Homicides, Municipalities without Security Committee') %>%
  footnote(general = 'Nonparametric estimations (MSE-two selection, unless noted) with year fixed effects, robust standard errors in brackets, p-values in parentheses.', threeparttable = TRUE)



# large munis ----

# Benchmark

robust_all <- rdrobust(tt_d$dv_homicides,tt_d$marginal_muni, all = TRUE, bwselect = 'msetwo', 
                       covs = cbind(tt_d$y2008,tt_d$y2012))

# MSERD

robust_mserd <- rdrobust(tt_d$dv_homicides,tt_d$marginal_muni, all = TRUE, bwselect = 'mserd', 
                         covs = cbind(tt_d$y2008,tt_d$y2012))
# No previous LOC

tt_da <- tt_d[p_elected_muni == 0]
robust_p <- rdrobust(tt_da$dv_homicides,tt_da$marginal_muni, all = TRUE, bwselect = 'msetwo', 
                     covs = cbind(tt_da$y2008,tt_da$y2012))

# second-degree polynomial

robust_2 <- rdrobust(tt_d$dv_homicides,tt_d$marginal_muni, all = TRUE, p = 2,
                     bwselect = 'msetwo', 
                     covs = cbind(tt_d$y2008,tt_d$y2012))


table_large_city <- list('Benchmark' = robust_all, 
                              'MSE-optimal' = robust_mserd, 
                              '2nd Polynomial' = robust_2, 
                              'No previous L&O' = robust_p)

#TABLE A26
modelsummary(table_large_city, 
             statistic = c("[{std.error}]",'p.value'),
             output = 'latex',
             title = 'Effect of the election of law-and-order candidates on Homicides, Large municipalities (greater than 50,000 pop.) without Security Committee') %>%
  footnote(general = 'Nonparametric estimations (MSE-two selection, unless noted) with year fixed effects, robust standard errors in brackets, p-values in parentheses.', threeparttable = TRUE)


# small munis ----

# Benchmark

robust_all <- rdrobust(tt_e$dv_homicides,tt_e$marginal_muni, all = TRUE, bwselect = 'msetwo', 
                       covs = cbind(tt_e$y2008,tt_e$y2012))

# MSERD

robust_mserd <- rdrobust(tt_e$dv_homicides,tt_e$marginal_muni, all = TRUE, bwselect = 'mserd', 
                         covs = cbind(tt_e$y2008,tt_e$y2012))
# No previous LOC

tt_ea <- tt_e[p_elected_muni == 0]
robust_p <- rdrobust(tt_ea$dv_homicides,tt_ea$marginal_muni, all = TRUE, bwselect = 'msetwo', 
                     covs = cbind(tt_ea$y2008,tt_ea$y2012))

# second-degree polynomial

robust_2 <- rdrobust(tt_e$dv_homicides,tt_e$marginal_muni, all = TRUE, p = 2,
                     bwselect = 'msetwo', 
                     covs = cbind(tt_e$y2008,tt_e$y2012))


table_small_city <- list('Benchmark' = robust_all, 
                              'MSE-optimal' = robust_mserd, 
                              '2nd Polynomial' = robust_2, 
                              'No previous L&O' = robust_p)

#TABLE A27
modelsummary(table_small_city, 
             statistic = c("[{std.error}]",'p.value'),
             output = 'latex',
             title = 'Effect of the election of law-and-order candidates on Homicides, small municipalities (smaller than 50,000 pop.) without Security Committee') %>%
  footnote(general = 'Nonparametric estimations (MSE-two selection, unless noted) with year fixed effects, robust standard errors in brackets, p-values in parentheses.', threeparttable = TRUE)


# controls ----

# Benchmark

robust_all <- rdrobust(tt_b$dv_homicides,tt_b$marginal_muni, all = TRUE, bwselect = 'msetwo', 
                       covs = cbind(tt_b$y2008, tt_b$y2012, tt_b$gini, tt_b$lag_pop, tt_b$gdp_pc, tt_b$nonwhite))

# MSERD

robust_mserd <- rdrobust(tt_b$dv_homicides,tt_b$marginal_muni, all = TRUE, bwselect = 'mserd', 
                         covs = cbind(tt_b$y2008, tt_b$y2012, tt_b$gini, tt_b$lag_pop, tt_b$gdp_pc, tt_b$nonwhite))
# No previous LOC

tt_ba <- tt_b[p_elected_muni == 0]
robust_p <- rdrobust(tt_ba$dv_homicides,tt_ba$marginal_muni, all = TRUE, bwselect = 'msetwo', 
                     covs = cbind(tt_ba$y2008, tt_ba$y2012, tt_ba$gini, tt_ba$lag_pop, tt_ba$gdp_pc, tt_ba$nonwhite))

# second-degree polynomial

robust_2 <- rdrobust(tt_b$dv_homicides,tt_b$marginal_muni, all = TRUE, p = 2,
                     bwselect = 'msetwo', 
                     covs = cbind(tt_b$y2008, tt_b$y2012, tt_b$gini, tt_b$lag_pop, tt_b$gdp_pc, tt_b$nonwhite))


table_controls <- list('Benchmark' = robust_all, 
                              'MSE-optimal' = robust_mserd, 
                              '2nd Polynomial' = robust_2, 
                              'No previous L&O' = robust_p)

#TABLE A28
modelsummary(table_controls, 
             statistic = c("[{std.error}]",'p.value'),
             output = 'latex',
             title = 'Effect of the election of law-and-order candidates on Homicides, without Security Committee, using controls') %>%
  footnote(general = 'Nonparametric estimations (MSE-two selection, unless noted) with year fixed effects, robust standard errors in brackets, p-values in parentheses.', threeparttable = TRUE)
