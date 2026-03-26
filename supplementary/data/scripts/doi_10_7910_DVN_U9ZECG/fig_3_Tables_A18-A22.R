rm(list = ls())
library(data.table)
library(rdrobust)
library(rddensity)
library(ggplot2)
library(ggthemes)
library(here)

#packages for the tables
library(tidyr)
library(kableExtra)
library(modelsummary)

load(here("data","ver_data.RData"))

all.together <- data.frame()

tt_b <- unique(ver_b[abs(marginal_muni_occ_law) < 0.01 & occ_law == 1 & law_enforcement == 0, .(muni_code, year, elected_occ_law_muni, marginal_muni_occ_law, p_elected_muni, sc_before, dv_finbra, y2008, y2012, p_elected_muni)])

  identical(nrow(tt_b[,.(muni_code,year)]), nrow(unique(tt_b[,.(muni_code,year)]))) # doubles-check dataset

group_b <- with(tt_b, rdrobust(dv_finbra, marginal_muni_occ_law, all=TRUE, 
                               covs = cbind(tt_b$y2008, tt_b$y2012), bwselect = 'msetwo'))

#summary(group_b)
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
                   ballot = 'not',
                   estimation = 'Not law-and-order\n(but law enforcement)')

all.together = rbind(all.together,temp)

tt_a <- unique(ver_b[abs(marginal_muni) < 0.01 & law_enforcement ==1, .(muni_code, year, marginal_muni, pop, p_elected_muni, sc_before, dv_finbra, y2008, y2012)])

  identical(nrow(tt_a[,.(muni_code,year)]),nrow(unique(tt_a[,.(muni_code,year)])))

group_a  <- with(tt_a, rdrobust(dv_finbra, marginal_muni, all=TRUE, covs = cbind(tt_a$y2008, tt_a$y2012), bwselect = 'msetwo'))
#summary(group_a)

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
                   ballot = 'yep',
                   estimation = 'Law-and-order')

all.together = rbind(all.together,temp)

tt_pm <- unique(ver_b[abs(marginal_muni_pm) < 0.01 & law_enforcement ==1 & pm ==1, .(muni_code, year, marginal_muni_pm, dv_finbra, y2008, y2012, p_elected_muni)])

identical(nrow(tt_pm[,.(muni_code,year)]),nrow(unique(tt_pm[,.(muni_code,year)])))

group_pm  <- with(tt_pm, rdrobust(dv_finbra, marginal_muni_pm, all=TRUE, covs = cbind(tt_pm$y2008, tt_pm$y2012), bwselect = 'msetwo'))
#summary(group_pm)

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
                   ballot = 'yep',
                   estimation = 'Police')

all.together = rbind(all.together,temp)

tt_civil <- unique(ver_b[abs(marginal_muni_civil) < 0.01 & law_enforcement ==1 & civil ==1, .(muni_code, year, marginal_muni_civil, pop, dv_finbra, y2008, y2012, p_elected_muni)])

identical(nrow(tt_civil[,.(muni_code,year)]),nrow(unique(tt_civil[,.(muni_code,year)])))

group_civil  <- with(tt_civil, rdrobust(dv_finbra, marginal_muni_civil, all=TRUE, covs = cbind(tt_civil$y2008, tt_civil$y2012), bwselect = 'msetwo'))
#summary(group_civil)

temp <- data.frame(conventional_coef = group_civil$coef[[1]],
                   robust_coef = group_civil$coef[[3]],
                   robust_se = group_civil$se[[3]],
                   conventional_upper = group_civil$ci[[4]],
                   conventional_lower = group_civil$ci[[1]],
                   robust_upper = group_civil$ci[[6]],
                   robust_lower = group_civil$ci[[3]],
                   se_robust = group_civil$se[[3]],
                   n = group_civil$N_h[[1]] + group_civil$N_h[[2]],
                   bw = group_civil$bws[[2]],
                   ballot = 'yep',
                   estimation = 'Investigative police')

all.together = rbind(all.together,temp)

tt_not_police <- unique(ver_b[abs(marginal_muni_not_police) < 0.01 & law_enforcement ==1 & not_police == 1, .(muni_code, year, marginal_muni_not_police, pop, dv_finbra, y2008, y2012, p_elected_muni)])

identical(nrow(tt_not_police[,.(muni_code,year)]),nrow(unique(tt_not_police[,.(muni_code,year)])))

group_not_police  <- with(tt_not_police, rdrobust(dv_finbra, marginal_muni_not_police,  all=TRUE, covs = cbind(tt_not_police$y2008, tt_not_police$y2012)), bwselect = 'msetwo')
#summary(group_not_police)

temp <- data.frame(conventional_coef = group_not_police$coef[[1]],
                   robust_coef = group_not_police$coef[[3]],
                   robust_se = group_not_police$se[[3]],
                   conventional_upper = group_not_police$ci[[4]],
                   conventional_lower = group_not_police$ci[[1]],
                   robust_upper = group_not_police$ci[[6]],
                   robust_lower = group_not_police$ci[[3]],
                   se_robust = group_not_police$se[[3]],
                   n = group_not_police$N_h[[1]] + group_not_police$N_h[[2]],
                   bw = group_not_police$bws[[2]],
                   ballot = 'yep',
                   estimation = "Not police\nlaw-and-order cand.")

all.together = rbind(all.together,temp)


all.together$estimation <- factor(all.together$estimation ,
                                  levels = c('Law-and-order',
                                             "Not police\nlaw-and-order cand.",
                                             'Investigative police',
                                             'Police',
                                             'Not law-and-order\n(but law enforcement)'))

plot.jobs <- ggplot(all.together, aes(x = estimation, y = robust_coef, color = ballot, shape = ballot)) +
  geom_errorbar(aes(ymax = (robust_upper), ymin = (robust_lower)), width = 0, linewidth = .75,position = position_dodge(width=0.9)) +
  geom_point(alpha=.9,size=2.5) +
  geom_hline(yintercept=0, linetype="dotted",colour='gray20') +
  theme_minimal(base_size = 12) +
  scale_color_manual(values=c("gray", "black")) +
  theme(legend.position = "none") +
  #theme(legend.title=element_blank()) +
  xlab("") +
  geom_text(aes(label=paste('n = ',n)), y=95,  colour="black", size=3, fontface = "italic") +
  ylab('Difference on Public Security Spending\n(in 2016 R$ per capita/year)') +
  NULL

ggsave(here("writing","img","fig_3.pdf"), plot = plot.jobs, device = 'pdf',height = 10, width = 20, units = 'cm')

# Comparing

test_data <- data.table(coef_group_a = all.together[all.together$estimation == 'Law-and-order',]$robust_coef,
                        coef_not_group_a = all.together[all.together$estimation == 'Not law-and-order\n(but law enforcement)',]$robust_coef,
                        se_group_a = all.together[all.together$estimation == 'Law-and-order',]$se_robust,
                        se_not_group_a = all.together[all.together$estimation == 'Not law-and-order\n(but law enforcement)',]$se_robust)

test_data[, difference := coef_group_a - coef_not_group_a]
test_data[, test := sqrt((se_not_group_a^2) + (se_group_a^2))]
test_data[, statistic := (difference / test)] 


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

# LAW AND ORDER ----

# Benchmark

robust_all <- rdrobust(tt_a$dv_finbra,tt_a$marginal_muni, all = TRUE, bwselect = 'msetwo', 
                       covs = cbind(tt_a$y2008,tt_a$y2012))

# MSERD

robust_mserd <- rdrobust(tt_a$dv_finbra,tt_a$marginal_muni, all = TRUE, bwselect = 'mserd', 
                         covs = cbind(tt_a$y2008,tt_a$y2012))
# No previous LOC

tt_aa <- tt_a[p_elected_muni == 0]
robust_p <- rdrobust(tt_aa$dv_finbra,tt_aa$marginal_muni, all = TRUE, bwselect = 'msetwo', 
                     covs = cbind(tt_aa$y2008,tt_aa$y2012))

# second-degree polynomial

robust_2 <- rdrobust(tt_a$dv_finbra,tt_a$marginal_muni, all = TRUE, p = 2,
                     bwselect = 'msetwo', 
                     covs = cbind(tt_a$y2008,tt_a$y2012))


table_loc <- list('Benchmark' = robust_all, 
                         'MSE-optimal' = robust_mserd, 
                         '2nd Polynomial' = robust_2, 
                         'No previous L&O' = robust_p)

#Table A18
modelsummary(table_loc, 
             statistic = c("[{std.error}]",'p.value'),
             output = 'latex',
             title = 'Effect of the election of law-and-order candidates on Spending') %>%
  footnote(general = 'Nonparametric estimations (MSE-two selection, unless noted) with year fixed effects, robust standard errors in brackets, p-values in parentheses.', threeparttable = TRUE)


# not police ----

# Benchmark

robust_all <- rdrobust(tt_not_police$dv_finbra,tt_not_police$marginal_muni, all = TRUE, bwselect = 'msetwo', 
                       covs = cbind(tt_not_police$y2008,tt_not_police$y2012))

# MSERD

robust_mserd <- rdrobust(tt_not_police$dv_finbra,tt_not_police$marginal_muni, all = TRUE, bwselect = 'mserd', 
                         covs = cbind(tt_not_police$y2008,tt_not_police$y2012))
# No previous LOC

tt_not_police_a <- tt_not_police[p_elected_muni == 0]
robust_p <- rdrobust(tt_not_police_a$dv_finbra,tt_not_police_a$marginal_muni, all = TRUE, bwselect = 'msetwo', 
                     covs = cbind(tt_not_police_a$y2008,tt_not_police_a$y2012))

# second-degree polynomial

robust_2 <- rdrobust(tt_not_police$dv_finbra,tt_not_police$marginal_muni, all = TRUE, p = 2,
                     bwselect = 'msetwo', 
                     covs = cbind(tt_not_police$y2008,tt_not_police$y2012))


table_not_police <- list('Benchmark' = robust_all, 
                  'MSE-optimal' = robust_mserd, 
                  '2nd Polynomial' = robust_2, 
                  'No previous L&O' = robust_p)

#Table A19
modelsummary(table_not_police, 
             statistic = c("[{std.error}]",'p.value'),
             output = 'latex',
             title = 'Effect of the election of not-police law-and-order candidates on Spending') %>%
  footnote(general = 'Nonparametric estimations (MSE-two selection, unless noted) with year fixed effects, robust standard errors in brackets, p-values in parentheses.', threeparttable = TRUE)

# investigative ----

# Benchmark

robust_all <- rdrobust(tt_civil$dv_finbra,tt_civil$marginal_muni, all = TRUE, bwselect = 'msetwo', 
                       covs = cbind(tt_civil$y2008,tt_civil$y2012))

# MSERD

robust_mserd <- rdrobust(tt_civil$dv_finbra,tt_civil$marginal_muni, all = TRUE, bwselect = 'mserd', 
                         covs = cbind(tt_civil$y2008,tt_civil$y2012))
# No previous LOC

tt_civil_a <- tt_civil[p_elected_muni == 0]
robust_p <- rdrobust(tt_civil_a$dv_finbra,tt_civil_a$marginal_muni, all = TRUE, bwselect = 'msetwo', 
                     covs = cbind(tt_civil_a$y2008,tt_civil_a$y2012))

# second-degree polynomial

robust_2 <- rdrobust(tt_civil$dv_finbra,tt_civil$marginal_muni, all = TRUE, p = 2,
                     bwselect = 'msetwo', 
                     covs = cbind(tt_civil$y2008,tt_civil$y2012))


table_civil <- list('Benchmark' = robust_all, 
                  'MSE-optimal' = robust_mserd, 
                  '2nd Polynomial' = robust_2, 
                  'No previous L&O' = robust_p)

#Table A20
modelsummary(table_civil, 
             statistic = c("[{std.error}]",'p.value'),
             output = 'latex',
             title = 'Effect of the election of investigative police law-and-order candidates on Spending') %>%
  footnote(general = 'Nonparametric estimations (MSE-two selection, unless noted) with year fixed effects, robust standard errors in brackets, p-values in parentheses.', threeparttable = TRUE)



# police ----

# Benchmark

robust_all <- rdrobust(tt_pm$dv_finbra,tt_pm$marginal_muni, all = TRUE, bwselect = 'msetwo', 
                       covs = cbind(tt_pm$y2008,tt_pm$y2012))

# MSERD

robust_mserd <- rdrobust(tt_pm$dv_finbra,tt_pm$marginal_muni, all = TRUE, bwselect = 'mserd', 
                         covs = cbind(tt_pm$y2008,tt_pm$y2012))
# No previous LOC

tt_pm_a <- tt_pm[p_elected_muni == 0]
robust_p <- rdrobust(tt_pm_a$dv_finbra,tt_pm_a$marginal_muni, all = TRUE, bwselect = 'msetwo', 
                     covs = cbind(tt_pm_a$y2008,tt_pm_a$y2012))

# second-degree polynomial

robust_2 <- rdrobust(tt_pm$dv_finbra,tt_pm$marginal_muni, all = TRUE, p = 2,
                     bwselect = 'msetwo', 
                     covs = cbind(tt_pm$y2008,tt_pm$y2012))


table_pm <- list('Benchmark' = robust_all, 
                    'MSE-optimal' = robust_mserd, 
                    '2nd Polynomial' = robust_2, 
                    'No previous L&O' = robust_p)

#Table A21
modelsummary(table_pm, 
             statistic = c("[{std.error}]",'p.value'),
             output = 'latex',
             title = 'Effect of the election of police law-and-order candidates on Spending') %>%
  footnote(general = 'Nonparametric estimations (MSE-two selection, unless noted) with year fixed effects, robust standard errors in brackets, p-values in parentheses.', threeparttable = TRUE)


# Not Law and Order----

# Benchmark

robust_all <- rdrobust(tt_b$dv_finbra,tt_b$marginal_muni, all = TRUE, bwselect = 'msetwo', 
                       covs = cbind(tt_b$y2008,tt_b$y2012))

# MSERD

robust_mserd <- rdrobust(tt_b$dv_finbra,tt_b$marginal_muni, all = TRUE, bwselect = 'mserd', 
                         covs = cbind(tt_b$y2008,tt_b$y2012))
# No previous LOC

tt_ba <- tt_b[p_elected_muni == 0]
robust_p <- rdrobust(tt_ba$dv_finbra,tt_ba$marginal_muni, all = TRUE, bwselect = 'msetwo', 
                     covs = cbind(tt_ba$y2008,tt_ba$y2012))

# second-degree polynomial

robust_2 <- rdrobust(tt_b$dv_finbra,tt_b$marginal_muni, all = TRUE, p = 2,
                     bwselect = 'msetwo', 
                     covs = cbind(tt_b$y2008,tt_b$y2012))


table_occ <- list('Benchmark' = robust_all, 
                  'MSE-optimal' = robust_mserd, 
                  '2nd Polynomial' = robust_2, 
                  'No previous L&O' = robust_p)
# Table A22
modelsummary(table_occ, 
             statistic = c("[{std.error}]",'p.value'),
             output = 'latex',
             title = 'Effect of the election of not law-and-order candidates (but law enforcement) on Spending') %>%
  footnote(general = 'Nonparametric estimations (MSE-two selection, unless noted) with year fixed effects, robust standard errors in brackets, p-values in parentheses.', threeparttable = TRUE)
