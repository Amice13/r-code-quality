rm(list = ls())
library(data.table)
library(rdrobust)
library(rddensity)
library(ggplot2)
library(here)
library(ggthemes)

#packages for the tables
library(tidyr)
library(kableExtra)
library(modelsummary)

load(here("data","ver_data.RData"))

# Police Candidates
tt_pm <- unique(ver_b[pm ==1 & abs(marginal_muni_pm) < 0.01 & law_enforcement == 1, .(muni_code, year, marginal_muni_pm, elected_pm_muni, p_elected_muni, dv_homicides, y2008, y2012)])

# Investigative police
tt_civil <- unique(ver_b[civil ==1 & law_enforcement == 1 & abs(marginal_muni_civil) < 0.01, .(muni_code, year, marginal_muni_civil, elected_civil_muni, p_elected_muni, dv_homicides, y2008, y2012)])

# Not police
tt_not_police <- unique(ver_b[not_police == 1 & law_enforcement == 1 & abs(marginal_muni_not_police) < 0.01, .(muni_code, year, marginal_muni_not_police, elected_not_police_muni, p_elected_muni, dv_homicides, y2008, y2012)])

# not LOC
tt_occ_law <- unique(ver_b[occ_law == 1 & law_enforcement == 0 & abs(marginal_muni_occ_law) < 0.01, .(muni_code, year, marginal_muni_occ_law, elected_occ_law_muni, p_elected_muni, dv_homicides, y2008, y2012)])

all.together <- data.frame(stringsAsFactors=FALSE)

group_a  <- with(tt_civil, rdrobust(dv_homicides, marginal_muni_civil, all=TRUE, bwselect = 'msetwo', covs = cbind(tt_civil$y2008, tt_civil$y2012)))
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
                   estimation = 'Investigative police',
                   loc = 'yep')

all.together = rbind(all.together,temp)

group_c  <- with(tt_pm, rdrobust(dv_homicides, marginal_muni_pm, all=TRUE, bwselect = 'msetwo', covs = cbind(tt_pm$y2008, tt_pm$y2012)))
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
                   estimation = 'Police',
                   loc = 'yep')

all.together = rbind(all.together,temp)


group_d <- with(tt_not_police, rdrobust(dv_homicides, marginal_muni_not_police, all=TRUE, bwselect = 'msetwo', 
                                        covs = cbind(tt_not_police$y2008, tt_not_police$y2012)))

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
                   estimation = 'Not police\nlaw-and-order cand.',
                   loc = 'yep')

all.together = rbind(all.together,temp)


# Occ law ----

group_e <- with(tt_occ_law, rdrobust(dv_homicides, marginal_muni_occ_law, all=TRUE, bwselect = 'msetwo', 
                                     covs = cbind(tt_occ_law$y2008, tt_occ_law$y2012)))

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
                   estimation = 'Not law-and-order\n(but law enforcement)',
                   loc = 'nope')

all.together = rbind(all.together,temp)


all.together$estimation <- factor(all.together$estimation ,
                                  levels = c("Not police\nlaw-and-order cand.",
                                             'Investigative police',
                                             'Police',
                                             'Not law-and-order\n(but law enforcement)'))

all.together$loc <- factor(all.together$loc ,
                                  levels = c('yep','nope'))

plot.jobs <- ggplot(all.together, aes(x = estimation, y = robust_coef, color = loc)) +
  geom_errorbar(aes(ymax = (robust_upper), ymin = (robust_lower)), width=0, linewidth=.75,position=position_dodge(width=0.9)) +
  geom_point(alpha=.9,size=2.5) +
  geom_hline(yintercept=0, linetype="dotted",colour='gray20') +
  theme_minimal(base_size = 12) +
  scale_colour_grey() +
  theme(legend.position = "none") +
  #theme(legend.title=element_blank()) +
  xlab("") +
  geom_text(aes(label=paste('n = ',n)), y=40,  colour="black", size=3, fontface = "italic") +
  ylab('Difference in Homicide Rate\n(Yearly deaths per 100 thousand pop.)') +
  NULL

ggsave(here("writing","img","fig_2.pdf"), plot = plot.jobs, device = 'pdf',height = 10, width = 20, units = 'cm')

# Comparing

test_data <- data.table(coef_group_a = all.together[all.together$estimation == 'Not law-and-order\n(but law enforcement)',]$robust_coef,
                        coef_not_group_a = all.together[all.together$estimation == 'Police',]$robust_coef,
                        se_group_a = all.together[all.together$estimation == 'Not law-and-order\n(but law enforcement)',]$se_robust,
                        se_not_group_a = all.together[all.together$estimation == 'Police',]$se_robust)

test_data[, difference := coef_group_a - coef_not_group_a]
test_data[, test := sqrt((se_not_group_a^2) + (se_group_a^2))]
test_data[, statistic := (difference / test)] 

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

# police ----

# Benchmark

robust_all <- rdrobust(tt_pm$dv_homicides,tt_pm$marginal_muni_pm, all = TRUE, bwselect = 'msetwo', 
                       covs = cbind(tt_pm$y2008,tt_pm$y2012))

# MSERD

robust_mserd <- rdrobust(tt_pm$dv_homicides,tt_pm$marginal_muni_pm, all = TRUE, bwselect = 'mserd', 
                         covs = cbind(tt_pm$y2008,tt_pm$y2012))
# No previous LOC

tt_pm_a <- tt_pm[p_elected_muni == 0]
robust_p <- rdrobust(tt_pm_a$dv_homicides,tt_pm_a$marginal_muni_pm, all = TRUE, bwselect = 'msetwo', 
                     covs = cbind(tt_pm_a$y2008,tt_pm_a$y2012))

# second-degree polynomial

robust_2 <- rdrobust(tt_pm$dv_homicides,tt_pm$marginal_muni_pm, all = TRUE, p = 2,
                     bwselect = 'msetwo', 
                     covs = cbind(tt_pm$y2008,tt_pm$y2012))

table_police <- list('Benchmark' = robust_all, 
                           'MSE-optimal' = robust_mserd, 
                           '2nd Polynomial' = robust_2, 
                           'No previous L&O' = robust_p)

modelsummary(table_police, 
             statistic = c("[{std.error}]",'p.value'),
             output = 'latex',
             title = 'Effect of the election of a police officer law-and-order candidate on Homicides') %>%
  footnote(general = 'Nonparametric estimations (MSE-two selection, unless noted) with year fixed effects, robust standard errors in brackets, p-values in parentheses.', threeparttable = TRUE)

# investigative police ----

# Benchmark

robust_all <- rdrobust(tt_civil$dv_homicides,tt_civil$marginal_muni_civil, all = TRUE, bwselect = 'msetwo', 
                       covs = cbind(tt_civil$y2008,tt_civil$y2012))

# MSERD

robust_mserd <- rdrobust(tt_civil$dv_homicides,tt_civil$marginal_muni_civil, all = TRUE, bwselect = 'mserd', 
                         covs = cbind(tt_civil$y2008,tt_civil$y2012))
# No previous LOC

tt_civil_a <- tt_civil[p_elected_muni == 0]
robust_p <- rdrobust(tt_civil_a$dv_homicides,tt_civil_a$marginal_muni_civil, all = TRUE, bwselect = 'msetwo', 
                     covs = cbind(tt_civil_a$y2008,tt_civil_a$y2012))

# second-degree polynomial

robust_2 <- rdrobust(tt_civil$dv_homicides,tt_civil$marginal_muni_civil, all = TRUE, p = 2,
                     bwselect = 'msetwo', 
                     covs = cbind(tt_civil$y2008,tt_civil$y2012))

table_civil <- list('Benchmark' = robust_all, 
                           'MSE-optimal' = robust_mserd, 
                           '2nd Polynomial' = robust_2, 
                           'No previous L&O' = robust_p)

modelsummary(table_civil, 
             statistic = c("[{std.error}]",'p.value'),
             output = 'latex',
             title = 'Effect of the election of a investigative police officer law-and-order candidate on Homicides') %>%
  footnote(general = 'Nonparametric estimations (MSE-two selection, unless noted) with year fixed effects, robust standard errors in brackets, p-values in parentheses.', threeparttable = TRUE)

# not police ----

# Benchmark

robust_all <- rdrobust(tt_not_police$dv_homicides,tt_not_police$marginal_muni_not_police, all = TRUE, bwselect = 'msetwo', 
                       covs = cbind(tt_not_police$y2008,tt_not_police$y2012))

# MSERD

robust_mserd <- rdrobust(tt_not_police$dv_homicides,tt_not_police$marginal_muni_not_police, all = TRUE, bwselect = 'mserd', 
                         covs = cbind(tt_not_police$y2008,tt_not_police$y2012))
# No previous LOC

tt_not_police_a <- tt_not_police[p_elected_muni == 0]
robust_p <- rdrobust(tt_not_police_a$dv_homicides,tt_not_police_a$marginal_muni_not_police, all = TRUE, bwselect = 'msetwo', 
                     covs = cbind(tt_not_police_a$y2008,tt_not_police_a$y2012))

# second-degree polynomial

robust_2 <- rdrobust(tt_not_police$dv_homicides,tt_not_police$marginal_muni_not_police, all = TRUE, p = 2,
                     bwselect = 'msetwo', 
                     covs = cbind(tt_not_police$y2008,tt_not_police$y2012))

table_not_police <- list('Benchmark' = robust_all, 
                    'MSE-optimal' = robust_mserd, 
                    '2nd Polynomial' = robust_2, 
                    'No previous L&O' = robust_p)

modelsummary(table_not_police, 
             statistic = c("[{std.error}]",'p.value'),
             output = 'latex',
             title = 'Effect of the election of not-police law-and-order candidates on Homicides') %>%
  footnote(general = 'Nonparametric estimations (MSE-two selection, unless noted) with year fixed effects, robust standard errors in brackets, p-values in parentheses.', threeparttable = TRUE)


# occ law----

# Benchmark

robust_all <- rdrobust(tt_occ_law$dv_homicides,tt_occ_law$marginal_muni_occ_law, all = TRUE, bwselect = 'msetwo', 
                       covs = cbind(tt_occ_law$y2008,tt_occ_law$y2012))

# MSERD

robust_mserd <- rdrobust(tt_occ_law$dv_homicides,tt_occ_law$marginal_muni_occ_law, all = TRUE, bwselect = 'mserd', 
                         covs = cbind(tt_occ_law$y2008,tt_occ_law$y2012))
# No previous LOC

tt_occ_law_a <- tt_occ_law[p_elected_muni == 0]
robust_p <- rdrobust(tt_occ_law_a$dv_homicides,tt_occ_law_a$marginal_muni_occ_law, all = TRUE, bwselect = 'msetwo', 
                     covs = cbind(tt_occ_law_a$y2008,tt_occ_law_a$y2012))

# second-degree polynomial

robust_2 <- rdrobust(tt_occ_law$dv_homicides,tt_occ_law$marginal_muni_occ_law, all = TRUE, p = 2,
                     bwselect = 'msetwo', 
                     covs = cbind(tt_occ_law$y2008,tt_occ_law$y2012))

table_occ_law <- list('Benchmark' = robust_all, 
                         'MSE-optimal' = robust_mserd, 
                         '2nd Polynomial' = robust_2, 
                         'No previous L&O' = robust_p)

modelsummary(table_occ_law, 
             statistic = c("[{std.error}]",'p.value'),
             output = 'latex',
             title = 'Effect of the election of law enforcement, but NOT law-and-order candidates on Homicides') %>%
  footnote(general = 'Nonparametric estimations (MSE-two selection, unless noted) with year fixed effects, robust standard errors in brackets, p-values in parentheses.', threeparttable = TRUE)
