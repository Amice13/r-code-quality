rm(list = ls())
library(data.table)
library(rdrobust)
library(rddensity)
library(ggplot2)
library(ggthemes)
library(here)


load(here("data","ver_data.RData"))

tt <- unique(ver_b[law_enforcement ==1 & abs(marginal_muni) < 0.01, 
                   .(muni_code, year, law_enforcement, marginal_muni, elected_muni, p_elected_muni,  dv_firearm, dv_not_firearm, y2008, y2012)])

# check if there is only one muni/year pair
  identical(nrow(tt),nrow(unique(tt)))

  
#plot

all.together <- data.frame(stringsAsFactors=FALSE)

group_a  <- with(tt, rdrobust(dv_firearm, marginal_muni, all=TRUE, bwselect = 'msetwo', covs = cbind(tt$y2008, tt$y2012)))
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
                   estimation = 'Firearm homicides')

all.together = rbind(all.together,temp)

group_b <- with(tt, rdrobust(dv_not_firearm, marginal_muni, all=TRUE, bwselect = 'msetwo', covs = cbind(tt$y2008, tt$y2012)))
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
                   estimation = 'Other means')

all.together = rbind(all.together,temp)

plot.jobs <- ggplot(all.together, aes(x = estimation, y = robust_coef)) +
  geom_errorbar(aes(ymax = (robust_upper), ymin = (robust_lower)), width=0,size=.75,position=position_dodge(width=0.9)) +
  geom_point(alpha=.9,size=2.5) +
  geom_hline(yintercept=0, linetype="dotted",colour='gray20') +
  theme_minimal(base_size = 10) +
  scale_colour_grey() +
  theme(legend.position = "none") +
  xlab("") +
  coord_flip() +
  ylab('Difference in Homicides,\n(Yearly deaths per 100,000 pop)') +
  NULL

#ggsave(here("writing","img","fig_A9.pdf"), plot = plot.jobs, device = 'pdf',height = 7.5, width = 10, units = 'cm')

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


firearm <- rdrobust(tt$dv_firearm, tt$marginal_muni, all = TRUE, bwselect = 'msetwo', 
                     covs = cbind(tt$y2008,tt$y2012))

not_firearm <- rdrobust(tt$dv_not_firearm,tt$marginal_muni, all = TRUE, bwselect = 'msetwo', 
                  covs = cbind(tt$y2008,tt$y2012))



armas <- list('Firearm' = firearm, 
              'Other means' = not_firearm)
# TABLE B11
modelsummary(armas, 
             statistic = c("[{std.error}]",'p.value'),
             output = 'latex',
             title = 'Effect of the election of law-and-order candidates on Homicides') %>%
  footnote(general = 'Nonparametric estimations (MSE-two selection) with year fixed effects, robust standard errors in brackets, p-values in parentheses.', threeparttable = TRUE)

