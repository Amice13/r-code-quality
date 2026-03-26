rm(list = ls())
library(data.table)
library(rdrobust)
library(rddensity)
library(ggplot2)
library(ggthemes)
library(here)


load(here('data','ver_data.RData'))

tt <- unique(ver_b[law_enforcement ==1 & abs(marginal_muni) < 0.01, .(muni_code, year, law_enforcement, marginal_muni, elected_muni, p_elected_muni, dv_homicides, y2008, y2012, gini, lag_pop)])

#plot

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


tt_b <- tt[lag_pop > 50000]
group_b <- with(tt_b, rdrobust(dv_homicides, marginal_muni, bwselect = 'msetwo', all=TRUE, covs = cbind(tt_b$y2008, tt_b$y2012)))
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
                   estimation = 'Munic.\nover 50,000')

all.together = rbind(all.together,temp)

tt_c <- tt[lag_pop < 50000]
group_c <- with(tt_c, rdrobust(dv_homicides, marginal_muni, bwselect = 'msetwo', all=TRUE, covs = cbind(tt_c$y2008, tt_c$y2012)))
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
                   estimation = 'Munic.\nbelow 50,000')

all.together = rbind(all.together,temp)

tt_d <- tt[gini < median(tt$gini, na.rm = TRUE)]
group_d <- with(tt_d, rdrobust(dv_homicides, marginal_muni, bwselect = 'msetwo', all=TRUE, covs = cbind(tt_d$y2008, tt_d$y2012)))
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
                   estimation = 'Lower\ninequality')

all.together = rbind(all.together,temp)

tt_e <- tt[gini > median(tt$gini, na.rm = TRUE)]
group_e <- with(tt_e, rdrobust(dv_homicides, marginal_muni, bwselect = 'msetwo', all=TRUE, covs = cbind(tt_e$y2008, tt_e$y2012)))
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
                   estimation = 'Higher\ninequality')

all.together = rbind(all.together,temp)

tt_f <- tt
group_f <- with(tt_f, rdrobust(dv_homicides, marginal_muni, bwselect = 'msetwo', all=TRUE, covs = cbind(tt_f$y2008, tt_f$y2012)))
summary(group_f)

temp <- data.frame(conventional_coef = group_f$coef[[1]],
                   robust_coef = group_f$coef[[3]],
                   robust_se = group_f$se[[3]],
                   conventional_upper = group_f$ci[[4]],
                   conventional_lower = group_f$ci[[1]],
                   robust_upper = group_f$ci[[6]],
                   robust_lower = group_f$ci[[3]],
                   se_robust = group_f$se[[3]],
                   n = group_f$N_h[[1]] + group_f$N_h[[2]],
                   bw = group_f$bws[[2]],
                   estimation = 'Baseline\nresult')

all.together = rbind(all.together,temp)

plot.jobs <- ggplot(all.together, aes(x = estimation, y = robust_coef)) +
  geom_errorbar(aes(ymax = (robust_upper), ymin = (robust_lower)), width=0,size=.75,position=position_dodge(width=0.9)) +
  geom_point(alpha=.9,size=2.5) +
  geom_hline(yintercept=0, linetype="dotted",colour='gray20') +
  theme_minimal(base_size = 10) +
  scale_colour_grey() +
  theme(legend.position = "none") +
  xlab("") +
  ylab('Difference in homicides') +
  NULL

#ggsave(here("writing","img","fig_A7.pdf"), plot = plot.jobs, device = 'pdf',height = 7.5, width = 20, units = 'cm')

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

high_in <- rdrobust(tt_e$dv_homicides, tt_e$marginal_muni, all = TRUE, bwselect = 'msetwo', 
                    covs = cbind(tt_e$y2008,tt_e$y2012))

low_in <- rdrobust(tt_d$dv_homicides,tt_d$marginal_muni, all = TRUE, bwselect = 'msetwo', 
                   covs = cbind(tt_d$y2008,tt_d$y2012))

large_mun <- rdrobust(tt_b$dv_homicides, tt_b$marginal_muni, all = TRUE, bwselect = 'msetwo', 
                    covs = cbind(tt_b$y2008,tt_b$y2012))

small_mun <- rdrobust(tt_c$dv_homicides,tt_c$marginal_muni, all = TRUE, bwselect = 'msetwo', 
                   covs = cbind(tt_c$y2008,tt_c$y2012))


alt <- list('Higher inequality' = high_in, 
                       'Lower inequality' = low_in,
                       'Larger munic.' = large_mun, 
                       'Smaller munic.' = small_mun)
# TABLE B9
modelsummary(alt, 
             statistic = c("[{std.error}]",'p.value'),
             output = 'latex',
             title = 'Effect of the election of law-and-order candidates on Homicides, Alternate Specifications') %>%
  footnote(general = 'Nonparametric estimations (MSE-two selection) with year fixed effects, robust standard errors in brackets, p-values in parentheses.', threeparttable = TRUE)

