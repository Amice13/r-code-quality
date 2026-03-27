rm(list = ls())
library(data.table)
library(rdrobust)
library(rddensity)
library(ggplot2)
library(here)
library(ggthemes)

load(here("data","ver_data.RData"))

tt_a <- unique(ver_b[abs(marginal_muni_civil) < 0.01 & civil == 1 & law_enforcement ==1, .(muni_code, year, elected_civil_muni, marginal_muni_civil, p_elected_muni, dv_pm_rais, y2008, y2012)])

identical(nrow(tt_a[,.(muni_code,year)]), nrow(unique(tt_a[,.(muni_code,year)]))) 

tt_b <- unique(ver_b[abs(marginal_muni_pm) < 0.01 & pm == 1 & law_enforcement ==1, .(muni_code, year, marginal_muni_pm, p_elected_muni, dv_pm_rais, y2008, y2012)])

identical(nrow(tt_b[,.(muni_code,year)]), nrow(unique(tt_b[,.(muni_code,year)]))) 

tt_c <- unique(ver_b[abs(marginal_muni_occ_law) < 0.01 & occ_law == 1 & law_enforcement ==0, .(muni_code, year, marginal_muni_occ_law, p_elected_muni, dv_pm_rais, y2008, y2012)])

identical(nrow(tt_c[,.(muni_code,year)]), nrow(unique(tt_c[,.(muni_code,year)]))) 

tt_d <- unique(ver_b[abs(marginal_muni) < 0.01 & law_enforcement ==1, .(muni_code, year, marginal_muni, p_elected_muni, dv_pm_rais, y2008, y2012)])

tt_e <- unique(ver_b[abs(marginal_muni_not_police) < 0.01 & not_police == 1 & law_enforcement ==1, .(muni_code, year, marginal_muni_not_police, p_elected_muni, dv_pm_rais, y2008, y2012)])

tt_f <- unique(ver_b[abs(marginal_muni_pm_not_loc) < 0.01 & pm == 1 & law_enforcement ==0, .(muni_code, year, marginal_muni_pm_not_loc, p_elected_muni, dv_pm_rais, y2008, y2012)])


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
                           stringsAsFactors=FALSE)

group_a  <- with(tt_a, rdrobust(dv_pm_rais, marginal_muni_civil, all=TRUE, bwselect = 'msetwo', covs = cbind(tt_a$y2008, tt_a$y2012)))
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
                   occupation = 'Investigative police',
                   ballot = 'Ballot name')

all.together = rbind(all.together,temp)

group_b <- with(tt_b, rdrobust(dv_pm_rais, marginal_muni_pm, all=TRUE, bwselect = 'msetwo', covs = cbind(tt_b$y2008, tt_b$y2012)))
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
                   occupation = 'Police',
                   ballot = 'Ballot name')

all.together = rbind(all.together,temp)

group_c <- with(tt_c, rdrobust(dv_pm_rais, marginal_muni_occ_law, all=TRUE, bwselect = 'msetwo', covs = cbind(tt_c$y2008, tt_c$y2012)))
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
                   occupation = 'Not law-and-order\n(but law enforcement)',
                   ballot = 'No Ballot name')

all.together = rbind(all.together,temp)

group_d <- with(tt_d, rdrobust(dv_pm_rais, marginal_muni, all=TRUE, bwselect = 'msetwo', covs = cbind(tt_d$y2008, tt_d$y2012)))
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
                   occupation = 'Law-and-order',
                   ballot = 'Ballot name')

all.together = rbind(all.together,temp)

group_e <- with(tt_e, rdrobust(dv_pm_rais, marginal_muni_not_police, all=TRUE, bwselect = 'msetwo', covs = cbind(tt_e$y2008, tt_e$y2012)))
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
                   occupation = 'Not police\nLaw-and-order',
                   ballot = 'Ballot name')

all.together = rbind(all.together,temp)


all.together$occupation <- factor(all.together$occupation ,
                                  levels = c('Law-and-order',
                                             'Not police\nLaw-and-order',
                                             'Investigative police',
                                             'Police',
                                             'Not law-and-order\n(but law enforcement)'))

plot.jobs <- ggplot(all.together, aes(x = occupation, y = robust_coef, color = ballot)) +
  geom_errorbar(aes(ymax = (robust_upper), ymin = (robust_lower)), width=0, linewidth=.75,position=position_dodge(width=0.9)) +
  geom_point(alpha=.9, size=2.5, position=position_dodge(width=0.9)) +
  geom_hline(yintercept=0, linetype="dotted",colour='gray20') +
  theme_minimal(base_size = 12) +
  scale_colour_grey() +
  theme(legend.position = "none") +
  theme(legend.title=element_blank()) +
  geom_text(aes(label=paste('n = ',n)), y=33,  colour="black", size=3, fontface = "italic") +
  xlab("") +
  ylab('Difference in Appointments\nPast Police Employees\n(per 100 thousand pop.)') +
  NULL

ggsave(here("writing","img","fig_5.pdf"), plot = plot.jobs, device = 'pdf',height = 10, width = 20, units = 'cm')

test_data <- data.table(coef_group_a = all.together[all.together$occupation == 'Not law-and-order\n(but law enforcement)',]$robust_coef,
                        coef_not_group_a = all.together[all.together$occupation == 'Police',]$robust_coef,
                        se_group_a = all.together[all.together$occupation == 'Not law-and-order\n(but law enforcement)',]$se_robust,
                        se_not_group_a = all.together[all.together$occupation == 'Police',]$se_robust)

test_data[, difference := coef_group_a - coef_not_group_a]
test_data[, test := sqrt((se_not_group_a^2) + (se_group_a^2))]
test_data[, statistic := (difference / test)] 

# TABLE

#packages for the tables
library(tidyr)
library(kableExtra)
library(modelsummary)

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

# law and order ----

# Benchmark

robust_all <- rdrobust(tt_d$dv_pm_rais,tt_d$marginal_muni, all = TRUE, bwselect = 'msetwo', 
                       covs = cbind(tt_d$y2008,tt_d$y2012))

# MSERD

robust_mserd <- rdrobust(tt_d$dv_pm_rais,tt_d$marginal_muni, all = TRUE, bwselect = 'mserd', 
                         covs = cbind(tt_d$y2008,tt_d$y2012))
# No previous LOC

tt_da <- tt_d[p_elected_muni == 0]
robust_p <- rdrobust(tt_da$dv_pm_rais,tt_da$marginal_muni, all = TRUE, bwselect = 'msetwo', 
                     covs = cbind(tt_da$y2008,tt_da$y2012))

# second-degree polynomial

robust_2 <- rdrobust(tt_d$dv_pm_rais,tt_d$marginal_muni, all = TRUE, p = 2,
                     bwselect = 'msetwo', 
                     covs = cbind(tt_d$y2008,tt_d$y2012))


table_loc <- list('Benchmark' = robust_all, 
                        'MSE-optimal' = robust_mserd, 
                        '2nd Polynomial' = robust_2, 
                        'No previous L&O' = robust_p)

# Table A29
modelsummary(table_loc, 
             statistic = c("[{std.error}]",'p.value'),
             output = 'latex',
             title = 'Effect of the election of law-and-order candidates on Appointments of past police employees') %>%
  footnote(general = 'Nonparametric estimations (MSE-two selection, unless noted) with year fixed effects, robust standard errors in brackets, p-values in parentheses.', threeparttable = TRUE)

# not police loc ----

# Benchmark

robust_all <- rdrobust(tt_e$dv_pm_rais,tt_e$marginal_muni, all = TRUE, bwselect = 'msetwo', 
                       covs = cbind(tt_e$y2008,tt_e$y2012))

# MSERD

robust_mserd <- rdrobust(tt_e$dv_pm_rais,tt_e$marginal_muni, all = TRUE, bwselect = 'mserd', 
                         covs = cbind(tt_e$y2008,tt_e$y2012))
# No previous LOC

tt_ea <- tt_e[p_elected_muni == 0]
robust_p <- rdrobust(tt_ea$dv_pm_rais,tt_ea$marginal_muni, all = TRUE, bwselect = 'msetwo', 
                     covs = cbind(tt_ea$y2008,tt_ea$y2012))

# second-degree polynomial

robust_2 <- rdrobust(tt_e$dv_pm_rais,tt_e$marginal_muni, all = TRUE, p = 2,
                     bwselect = 'msetwo', 
                     covs = cbind(tt_e$y2008,tt_e$y2012))


table_not_police <- list('Benchmark' = robust_all, 
                  'MSE-optimal' = robust_mserd, 
                  '2nd Polynomial' = robust_2, 
                  'No previous L&O' = robust_p)

# Table A30
modelsummary(table_not_police, 
             statistic = c("[{std.error}]",'p.value'),
             output = 'latex',
             title = 'Effect of the election of not-police law-and-order candidates on appointments of past police employees') %>%
  footnote(general = 'Nonparametric estimations (MSE-two selection, unless noted) with year fixed effects, robust standard errors in brackets, p-values in parentheses.', threeparttable = TRUE)


# civil ----

# Benchmark

robust_all <- rdrobust(tt_a$dv_pm_rais,tt_a$marginal_muni, all = TRUE, bwselect = 'msetwo', 
                       covs = cbind(tt_a$y2008,tt_a$y2012))

# MSERD

robust_mserd <- rdrobust(tt_a$dv_pm_rais,tt_a$marginal_muni, all = TRUE, bwselect = 'mserd', 
                         covs = cbind(tt_a$y2008,tt_a$y2012))
# No previous LOC

tt_aa <- tt_a[p_elected_muni == 0]
robust_p <- rdrobust(tt_aa$dv_pm_rais,tt_aa$marginal_muni, all = TRUE, bwselect = 'msetwo', 
                     covs = cbind(tt_aa$y2008,tt_aa$y2012))

# second-degree polynomial

robust_2 <- rdrobust(tt_a$dv_pm_rais,tt_a$marginal_muni, all = TRUE, p = 2,
                     bwselect = 'msetwo', 
                     covs = cbind(tt_a$y2008,tt_a$y2012))


table_civil <- list('Benchmark' = robust_all, 
                         'MSE-optimal' = robust_mserd, 
                         '2nd Polynomial' = robust_2, 
                         'No previous L&O' = robust_p)

# Table A31
modelsummary(table_civil, 
             statistic = c("[{std.error}]",'p.value'),
             output = 'latex',
             title = 'Effect of the election of investigative police law-and-order candidates on appointments of past police employees') %>%
  footnote(general = 'Nonparametric estimations (MSE-two selection, unless noted) with year fixed effects, robust standard errors in brackets, p-values in parentheses.', threeparttable = TRUE)


# police ----

# Benchmark

robust_all <- rdrobust(tt_b$dv_pm_rais,tt_b$marginal_muni, all = TRUE, bwselect = 'msetwo', 
                       covs = cbind(tt_b$y2008,tt_b$y2012))

# MSERD

robust_mserd <- rdrobust(tt_b$dv_pm_rais,tt_b$marginal_muni, all = TRUE, bwselect = 'mserd', 
                         covs = cbind(tt_b$y2008,tt_b$y2012))
# No previous LOC

tt_ba <- tt_b[p_elected_muni == 0]
robust_p <- rdrobust(tt_ba$dv_pm_rais,tt_ba$marginal_muni, all = TRUE, bwselect = 'msetwo', 
                     covs = cbind(tt_ba$y2008,tt_ba$y2012))

# second-degree polynomial

robust_2 <- rdrobust(tt_b$dv_pm_rais,tt_b$marginal_muni, all = TRUE, p = 2,
                     bwselect = 'msetwo', 
                     covs = cbind(tt_b$y2008,tt_b$y2012))


table_pm <- list('Benchmark' = robust_all, 
                    'MSE-optimal' = robust_mserd, 
                    '2nd Polynomial' = robust_2, 
                    'No previous L&O' = robust_p)
#Table A32

modelsummary(table_pm, 
             statistic = c("[{std.error}]",'p.value'),
             output = 'latex',
             title = 'Effect of the election of police law-and-order candidates on appointments of past police employees') %>%
  footnote(general = 'Nonparametric estimations (MSE-two selection, unless noted) with year fixed effects, robust standard errors in brackets, p-values in parentheses.', threeparttable = TRUE)


# occ ----

# Benchmark

robust_all <- rdrobust(tt_c$dv_pm_rais,tt_c$marginal_muni, all = TRUE, bwselect = 'msetwo', 
                       covs = cbind(tt_c$y2008,tt_c$y2012))

# MSERD

robust_mserd <- rdrobust(tt_c$dv_pm_rais,tt_c$marginal_muni, all = TRUE, bwselect = 'mserd', 
                         covs = cbind(tt_c$y2008,tt_c$y2012))
# No previous LOC

tt_ca <- tt_c[p_elected_muni == 0]
robust_p <- rdrobust(tt_ca$dv_pm_rais,tt_ca$marginal_muni, all = TRUE, bwselect = 'msetwo', 
                     covs = cbind(tt_ca$y2008,tt_ca$y2012))

# second-degree polynomial

robust_2 <- rdrobust(tt_c$dv_pm_rais,tt_c$marginal_muni, all = TRUE, p = 2,
                     bwselect = 'msetwo', 
                     covs = cbind(tt_c$y2008,tt_c$y2012))


table_occ <- list('Benchmark' = robust_all, 
                 'MSE-optimal' = robust_mserd, 
                 '2nd Polynomial' = robust_2, 
                 'No previous L&O' = robust_p)

# Table A33
modelsummary(table_occ, 
             statistic = c("[{std.error}]",'p.value'),
             output = 'latex',
             title = 'Effect of the election of not law-and-order (but law enforcement) candidates on appointments of past police employees') %>%
  footnote(general = 'Nonparametric estimations (MSE-two selection, unless noted) with year fixed effects, robust standard errors in brackets, p-values in parentheses.', threeparttable = TRUE)
