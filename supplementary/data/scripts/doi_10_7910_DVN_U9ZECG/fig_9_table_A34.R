rm(list = ls())
library(data.table)
library(rdrobust)
library(rddensity)
library(ggplot2)
library(ggthemes)
library(here)

load(here("data","ver_data.RData"))


tt <- unique(ver_b[abs(marginal_muni) < 0.01  & law_enforcement == 1, .(muni_code, uf, year, marginal_muni, dv_legal_killings, dv_agent_law, y2008, y2012)])


# As explained in text, we will only check ufs that have reported police killings in the previous period

uf.2004 = names(table(ver_b[lag_legal_period_1>0 & year==2004]$uf))
uf.2008 = names(table(ver_b[lag_legal_period_1>0 & year==2008]$uf))
uf.2012 = names(table(ver_b[lag_legal_period_1>0 & year==2012]$uf))

tt_a = subset(tt,(year==2004 & uf %in% uf.2004) | (year==2008 & uf %in% uf.2008) | (year==2012 & uf %in% uf.2012))

group_a  <- with(tt_a, rdrobust(dv_legal_killings, marginal_muni, all=TRUE, bwselect = 'msetwo',covs = cbind(tt_a$y2008, tt_a$y2012)))
summary(group_a)


all.together <- data.frame()

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
                   estimation = 'Difference\nKillings by law enforcement agents')

all.together = rbind(all.together,temp)

uf.2004_b = names(table(ver_b[lag_assault_agent_law_1>0 & year==2004]$uf))
uf.2008_b = names(table(ver_b[lag_assault_agent_law_1>0 & year==2008]$uf))
uf.2012_b = names(table(ver_b[lag_assault_agent_law_1>0 & year==2012]$uf))

tt_b = subset(tt,(year==2004 & uf %in% uf.2004_b) | (year==2008 & uf %in% uf.2008_b) | (year==2012 & uf %in% uf.2012_b))


group_b  <- with(tt_b, rdrobust(dv_agent_law, marginal_muni, all=TRUE, bwselect = 'msetwo',covs = cbind(tt_b$y2008, tt_b$y2012)))
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
                   estimation = 'Difference\nKillings of law enforcement agents')

all.together = rbind(all.together,temp)

plot.jobs <- ggplot(all.together, aes(x = estimation, y = robust_coef)) +
  geom_errorbar(aes(ymax = (robust_upper), ymin = (robust_lower)), 
                width=0,size=.75,position=position_dodge(width=0.9)) +
  geom_point(alpha=.9,size=2.5) +
  geom_hline(yintercept=0, linetype="dotted",colour='gray20') +
  theme_minimal(base_size = 12) +
  scale_colour_grey() +
  theme(legend.position = "none") +
  coord_flip() + 
  xlab("") +
  ylab('Yearly deaths per 100 thousand pop.') +
  NULL

ggsave(here("writing","img","fig_9.pdf"), plot = plot.jobs, device = 'pdf',height = 7.5, width = 15, units = 'cm')

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


agent_law <- rdrobust(tt_b$dv_agent_law, tt_b$marginal_muni, all = TRUE, bwselect = 'msetwo', 
                    covs = cbind(tt_b$y2008,tt_b$y2012))

legal_killings <- rdrobust(tt_a$dv_legal_killings,tt_a$marginal_muni, all = TRUE, bwselect = 'msetwo', 
                        covs = cbind(tt_a$y2008,tt_a$y2012))



armas <- list('Difference\nKillings by law enforcement agents' = legal_killings,
              'Difference\nKillings of law enforcement agents' = agent_law)
# TABLE B34
modelsummary(armas, 
             statistic = c("[{std.error}]",'p.value'),
             output = 'latex',
             title = 'Effect of the election of law-and-order candidates on Killings of or by law
enforcement agents') %>%
  footnote(general = 'Nonparametric estimations (MSE-two selection) with year fixed effects, robust standard errors in brackets, p-values in parentheses.', threeparttable = TRUE)


