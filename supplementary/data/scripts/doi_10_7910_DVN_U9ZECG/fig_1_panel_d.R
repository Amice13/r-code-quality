rm(list = ls())
library(data.table)
library(rdrobust)
library(rddensity)
library(ggplot2)
library(ggthemes)
library(here)


load(here("data","ver_data.RData"))

tt <- unique(ver_b[law_enforcement ==1 & abs(marginal_muni) < 0.01, 
                   .(muni_code, year, law_enforcement, marginal_muni, elected_muni, p_elected_muni, dv_homicides, dv_white_men, dv_nonwhite_men, y2008, y2012)])

# check if there is only one muni/year pair
  identical(nrow(tt),nrow(unique(tt)))

# select same bandwidth as main estimation
  bw <- rdbwselect(tt$dv_homicides,tt$marginal_muni, bwselect = 'mserd')
  sel = mean(c(bw$bws[[3]], bw$bws[[4]])) # robust bandwidth
  
#plot

all.together <- data.frame(stringsAsFactors=FALSE)

group_a  <- with(tt, rdrobust(dv_nonwhite_men, marginal_muni, h = sel, all=TRUE, bwselect = 'msetwo', covs = cbind(tt$y2008, tt$y2012)))
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
                   estimation = 'Diff. in Homicides \n Nonwhite Men')

all.together = rbind(all.together,temp)

group_b <- with(tt, rdrobust(dv_white_men, marginal_muni, h = sel, all=TRUE, bwselect = 'msetwo', covs = cbind(tt$y2008, tt$y2012)))
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
                   estimation = 'Diff. in Homicides \n White Men')

all.together = rbind(all.together,temp)

plot.jobs <- ggplot(all.together, aes(x = estimation, y = robust_coef)) +
  geom_errorbar(aes(ymax = (robust_upper), ymin = (robust_lower)), width=0, linewidth=.75,position=position_dodge(width=0.9)) +
  geom_point(alpha=.9,size=2.5) +
  geom_hline(yintercept=0, linetype="dotted",colour='gray20') +
  theme_minimal(base_size = 10) +
  scale_colour_grey() +
  theme(legend.position = "none") +
  xlab("") +
  coord_flip() +
  ylab('Difference in homicides \n(Yearly deaths per 100,000 pop.)') +
  NULL

#ggsave(here("writing","img","figure_1_panel_d.pdf"), plot = plot.jobs, device = 'pdf',height = 7.5, width = 10, units = 'cm')


