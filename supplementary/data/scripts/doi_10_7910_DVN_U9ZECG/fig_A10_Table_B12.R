rm(list = ls())
library(data.table)
library(rdrobust)
library(rddensity)
library(ggplot2)
library(ggthemes)
library(here)

load(here("data","ver_data.RData"))

all.together <- data.frame()

tt_a <- unique(ver_b[abs(marginal_muni) < 0.01 & law_enforcement ==1, .(muni_code, year, marginal_muni, pop, p_elected_muni, sc_before, dv_undetermined_assault, dv_reported, y2008, y2012)])

group_a  <- with(tt_a, rdrobust(dv_undetermined_assault, marginal_muni, all=TRUE, covs = cbind(tt_a$y2008, tt_a$y2012), bwselect = 'msetwo'))
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
                   estimation = 'Difference\nSuspicious Deaths')

all.together = rbind(all.together,temp)


group_b  <- with(tt_a, rdrobust(dv_reported, marginal_muni, all=TRUE, covs = cbind(tt_a$y2008, tt_a$y2012), bwselect = 'msetwo'))
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
                   estimation = 'Reporting of Killings by Agents (Binary)')

all.together = rbind(all.together,temp)


plot.jobs <- ggplot(all.together, aes(x = estimation, y = robust_coef)) +
  geom_errorbar(aes(ymax = (robust_upper), ymin = (robust_lower)), width=0,size=.75,position=position_dodge(width=0.9)) +
  geom_point(alpha=.9,size=2.5) +
  geom_hline(yintercept=0, linetype="dotted",colour='gray20') +
  theme_minimal(base_size = 10) +
  scale_colour_grey() +
  theme(legend.position = "none") +
  xlab("") +
  facet_wrap(. ~ estimation, scales = "free",ncol=1) +
  coord_flip() +
  ylab('') +
  NULL

ggsave(here("writing","img","fig_A10.pdf"), plot = plot.jobs, device = 'pdf',height = 10, width = 10, units = 'cm')




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


suspicious <- rdrobust(tt_a$dv_undetermined_assault, tt_a$marginal_muni, all = TRUE, bwselect = 'msetwo', 
                            covs = cbind(tt_a$y2008,tt_a$y2012))

report <- rdrobust(tt_a$dv_reported,tt_a$marginal_muni, all = TRUE, bwselect = 'msetwo', 
                         covs = cbind(tt_a$y2008,tt_a$y2012))



tampering <- list('Difference\nSuspicious Deaths' = suspicious, 
              'Diff. in Homicides \n White Women' = report)
# TABLE B12
modelsummary(tampering, 
             statistic = c("[{std.error}]",'p.value'),
             output = 'latex',
             title = 'Effect of the election of law-and-order candidates on Homicides') %>%
  footnote(general = 'Nonparametric estimations (MSE-two selection) with year fixed effects, robust standard errors in brackets, p-values in parentheses.', threeparttable = TRUE)
