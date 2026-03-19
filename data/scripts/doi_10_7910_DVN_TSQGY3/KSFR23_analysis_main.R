###Q###Q###Q###Q###Q###Q
###Q###Q  Main  ###Q###Q
###Q###Q###Q###Q###Q###Q

## load packages ====
# R version 4.2.3 (2023-03-15)
library(here) # v1.0.1
library(tidyverse) # v2.0.0
library(emmeans) # v1.8.8
library(ggpubr) # v0.6.0

## Multiple testing correction ====
adjust = 'holm' # argument for emmeans::contrast

## include utility functions ====
source(here("KSFR23_util.R"))

## read data ====
load(here("data", "KSFR23_data_clean.Rdata"))

## Figures ====
# Figure 2 ----
# Overview of dependent variables across treatment groups for DE & IT
# fit models
mod1 = lm(SuppCollab ~ treat*Country, data = dta)
mod2 = lm(PartyAttachChange ~ treat*Country, data = dta)
mod3 = lm(turnout ~ treat*Country, data = dta)
mod4 = lm(sticky ~ treat*Country, data = dta)
# plot support for collaboration
cont1 = emmeans(mod1, pairwise ~ treat*Country,
                at = list(treat=c("Xeno", "Auto", "Left", "Cont"),
                          Country=c("DE", "IT")))$emmeans %>% as.data.frame()
p1 = ggplot(cont1, aes(y = treat, x = emmean, 
                       xmin = lower.CL, xmax = upper.CL,
                       color = Country, shape = Country)) +
  geom_point(size = 1.5, 
             position = position_dodge(0.6)) + 
  geom_linerange(size = 0.7,
                 position = position_dodge(0.6)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9"),
                     breaks = c("DE", "IT"), labels = c("Germany", "Italy")) +
  scale_shape_manual(values = c(19, 15),
                     breaks = c("DE", "IT"), labels = c("Germany", "Italy")) +
  scale_y_discrete(labels = c("Far Right", "Autocratic", "Far Left", "Control")) +
  labs(x = 'Support for collaboration') + 
  theme_bw() +
  theme(axis.title.y = element_blank(),
        legend.title = element_blank())
# plot attachment change
cont2 = emmeans(mod2, pairwise ~ treat*Country,
                at = list(treat=c("Xeno", "Auto", "Left", "Cont"),
                          Country=c("DE", "IT")))$emmeans %>% as.data.frame()
p2 = ggplot(cont2, aes(y = treat, x = emmean, 
                       xmin = lower.CL, xmax = upper.CL,
                       color = Country, shape = Country)) +
  geom_vline(xintercept = 0, color = 1, linetype = 2) +
  geom_point(size = 1.5, 
             position = position_dodge(0.6)) + 
  geom_linerange(size = 0.7,
                 position = position_dodge(0.6)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9"),
                     breaks = c("DE", "IT"), labels = c("Germany", "Italy")) +
  scale_shape_manual(values = c(19, 15),
                     breaks = c("DE", "IT"), labels = c("Germany", "Italy")) +
  scale_y_discrete(labels = c("Far Right", "Autocratic", "Far Left", "Control")) +
  #scale_x_continuous(limits = c(0, 10)) + 
  labs(x = 'Change in attachment\nto preferred party') + 
  theme_bw() +
  theme(axis.title.y = element_blank(),
        legend.title = element_blank())
# plot turnout change
cont3 = emmeans(mod3, pairwise ~ treat*Country,
                at = list(treat=c("Xeno", "Auto", "Left", "Cont"),
                          Country=c("DE", "IT")))$emmeans %>% as.data.frame()
p3 = ggplot(cont3, aes(y = treat, x = emmean,  
                       xmin = lower.CL, xmax = upper.CL,
                       color = Country, shape = Country)) +
  geom_point(size = 1.5, 
             position = position_dodge(0.6)) + 
  geom_linerange(size = 0.7,
                 position = position_dodge(0.6)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9"),
                     breaks = c("DE", "IT"), labels = c("Germany", "Italy")) +
  scale_shape_manual(values = c(19, 15),
                     breaks = c("DE", "IT"), labels = c("Germany", "Italy")) +
  scale_y_discrete(labels = c("Far Right", "Autocratic", "Far Left", "Control")) +
  labs(x = 'Turnout propensity') + 
  theme_bw() +
  theme(axis.title.y = element_blank(),
        legend.title = element_blank())

# plot likelihood to vote for same party
cont4 = emmeans(mod4, pairwise ~ treat*Country,
                at = list(treat=c("Xeno", "Auto", "Left", "Cont"),
                          Country=c("DE", "IT")))$emmeans %>% as.data.frame()
p4 = ggplot(cont4, aes(y = treat, x = emmean,
                       xmin = lower.CL, xmax = upper.CL,
                       color = Country, shape = Country)) +
  geom_point(size = 1.5, 
             position = position_dodge(0.6)) + 
  geom_linerange(size = 0.7,
                 position = position_dodge(0.6)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9"),
                     breaks = c("DE", "IT"), labels = c("Germany", "Italy")) +
  scale_shape_manual(values = c(19, 15),
                     breaks = c("DE", "IT"), labels = c("Germany", "Italy")) +
  scale_y_discrete(labels = c("Far Right", "Autocratic", "Far Left", "Control")) +
  labs(x = 'Propensity to vote for preferred\n party in next federal election') + 
  theme_bw() +
  theme(axis.title.y = element_blank(), 
        legend.position = 'none',
        legend.title = element_blank())

# plot
fig2 = ggarrange(p1, p2, p3, p4, align = 'hv', common.legend = T, legend = 'bottom')
fig2
if(!file.exists(here("figures", "Fig2_Overview.pdf"))){
  ggsave(here("figures", "Fig2_Overview.pdf"), fig2,
         width = 6, height = 3.5)
}


# Figure 3 ----
# Difference in treatment effect at national vs. European level (foreign party): Across treatments and for DE & IT
#fit models
mod1DE = lm(SuppCollab ~ treat*level, dta, subset = (Country == 'DE'))
mod1DE.em = emmeans(mod1DE, specs = ~treat*level, data = mod1DE$model)
mod1IT = lm(SuppCollab ~ treat*level, dta, subset = (Country == 'IT'))
mod1IT.em = emmeans(mod1IT, specs = ~treat*level, data = mod1IT$model)

# contrasts to consider
intr = expand.grid(treat = c('Left', 'Auto', 'Xeno'),
                   level = c('ForEP'),
                   stringsAsFactors = F)

# compute contrasts and CIs
mod1DE.contr = contrast(mod1DE.em, 
                        method = contrasterLevel(mod1DE.em, 
                                                 intr$treat, 
                                                 intr$level), 
                        adjust = adjust)
mod1DE.contr.CI = confint(mod1DE.contr)
mod1IT.contr = contrast(mod1IT.em, 
                        method = contrasterLevel(mod1IT.em, 
                                                 intr$treat, 
                                                 intr$level), 
                        adjust = adjust)
mod1IT.contr.CI = confint(mod1IT.contr)

# combine
mod1.contr = bind_rows(mod1DE.contr.CI %>% mutate(Country = 'Germany'), 
                       mod1IT.contr.CI %>% mutate(Country = 'Italy')) %>%
  mutate(treat = substring(contrast, 1, 4),
         level = substring(contrast, 5, 100))

# relevel factors
mod1.contr$treat = factor(mod1.contr$treat, 
                          levels = c("Xeno","Auto","Left"),
                          labels = c("Far Right", "Autocratic", "Far Left"))
# plot
fig3 = ggplot(mod1.contr, 
              aes(x = estimate, y = treat,
                  xmin = lower.CL, xmax = upper.CL)) + 
  geom_vline(xintercept = 0, linetype = 2) + 
  geom_point(size = 1.5) + 
  geom_linerange(size = 0.7) +
  facet_wrap(~Country, ncol = 2,
             scales = 'free_y', drop = T,
             strip.position = 'top') +
  theme_bw() + 
  labs(x = "Contrast estimate") +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = theme_get()$text$size - 2,face = "bold"),
        axis.title.y = element_blank())
fig3
if(!file.exists(here("figures", "Fig3_ForEPLevelEstimates_treat.pdf"))){
  ggsave(here("figures", "Fig3_ForEPLevelEstimates_treat.pdf"), fig3,
         width = 6, height = 1.6)
}

# Figure 4 ----
# Difference in treatment effect at national vs. European level (national party): Across treatments and for DE & IT
# contrasts to consider
intr = expand.grid(treat = c('Left', 'Auto', 'Xeno'),
                   level = c('NatEP'),
                   stringsAsFactors = F)

# compute contrasts and CIs
mod1DE.contr = contrast(mod1DE.em, 
                        method = contrasterLevel(mod1DE.em, 
                                                 intr$treat, 
                                                 intr$level), 
                        adjust = adjust)
mod1DE.contr.CI = confint(mod1DE.contr)
mod1IT.contr = contrast(mod1IT.em, 
                        method = contrasterLevel(mod1IT.em, 
                                                 intr$treat, 
                                                 intr$level), 
                        adjust = adjust)
mod1IT.contr.CI = confint(mod1IT.contr)

# combine
mod1.contr = bind_rows(mod1DE.contr.CI %>% mutate(Country = 'Germany'), 
                       mod1IT.contr.CI %>% mutate(Country = 'Italy')) %>%
  mutate(treat = substring(contrast, 1, 4),
         level = substring(contrast, 5, 100))

# relevel factors
mod1.contr$treat = factor(mod1.contr$treat, 
                          levels = c("Xeno","Auto","Left"),
                          labels = c("Far Right", "Autocratic", "Far Left"))
# plot
fig4 = ggplot(mod1.contr, 
               aes(x = estimate, y = treat,
                   xmin = lower.CL, xmax = upper.CL)) + 
  geom_vline(xintercept = 0, linetype = 2) + 
  geom_point(size = 1.5) + 
  geom_linerange(size = 0.7) +
  facet_wrap(~Country, ncol = 2,
             scales = 'free_y', drop = T,
             strip.position = 'top') +
  theme_bw() + 
  labs(x = "Contrast estimate") +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = theme_get()$text$size - 2,face = "bold"),
        axis.title.y = element_blank())
fig4
if(!file.exists(here("figures", "Fig4_NatEPLevelEstimates_treat.pdf"))){
  ggsave(here("figures", "Fig4_NatEPLevelEstimates_treat.pdf"), fig4,
         width = 6, height = 1.4)
}

# Figure 5 ----
# Difference in treatment effect at national vs. European level (foreign party): 
# Pooled estimates for maximum power across all dependent variables
dta$treat_bin = ifelse(dta$treat == 'Cont', 'Cont', 'Treat')
fit1.pool = lm(SuppCollab ~ treat_bin*level, data = dta)
fit2.pool = lm(PartyAttachChange ~ treat_bin*level, data = dta)
fit3.pool = lm(turnout ~ treat_bin*level, data = dta)
fit4.pool = lm(sticky ~ treat_bin*level, data = dta)

fit1.pool.em = emmeans(fit1.pool, specs = ~treat_bin*level, data = fit1.pool$model)
fit2.pool.em = emmeans(fit2.pool, specs = ~treat_bin*level, data = fit2.pool$model)
fit3.pool.em = emmeans(fit3.pool, specs = ~treat_bin*level, data = fit3.pool$model)
fit4.pool.em = emmeans(fit4.pool, specs = ~treat_bin*level, data = fit4.pool$model)

# compute contrasts and CIs
fit1.pool.em = confint(contrast(fit1.pool.em, 
                                method = contrasterLevel(fit1.pool.em, 'Treat', 'ForEP')), 
                       level = 0.9)
fit2.pool.em = confint(contrast(fit2.pool.em, 
                                method = contrasterLevel(fit2.pool.em, 'Treat', 'ForEP')), 
                       level = 0.9)
fit3.pool.em = confint(contrast(fit3.pool.em, 
                                method = contrasterLevel(fit3.pool.em, 'Treat', 'ForEP')), 
                       level = 0.9)
fit4.pool.em = confint(contrast(fit4.pool.em, 
                                method = contrasterLevel(fit4.pool.em, 'Treat', 'ForEP')), 
                       level = 0.9)

CIs = bind_rows(fit1.pool.em %>% mutate(DV = 'SuppCollab'), 
                fit2.pool.em %>% mutate(DV = 'PartyAttachChange'), 
                fit3.pool.em %>% mutate(DV = 'turnout'), 
                fit4.pool.em %>% mutate(DV = 'sticky'))
CIs$DV = factor(CIs$DV, 
                levels = c('sticky', 'turnout', 'PartyAttachChange', 'SuppCollab'),
                labels = c('Vote for preferred party', 'Turnout propensity' , 'Change in attachment', 'Support for collaboration'))


fig5 = ggplot(CIs, aes(x = estimate, xmin = lower.CL, xmax = upper.CL,
                       y = DV)) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_point(size = 1.6) + 
  geom_linerange(size = 0.7) +
  scale_x_continuous(limits = c(-0.5, 0.5), breaks = seq(-.5, .5, .1)) + 
  #geom_vline(xintercept = c(-0.5, 0.5), lty = 2) + 
  labs(x = 'Pooled contrast estimate') +
  theme_bw() +
  theme(axis.title.y = element_blank())
fig5
if(!file.exists(here("figures", "Fig5_ForEPLevelEstimates_pooled.pdf"))){
  ggsave(here("figures", "Fig5_ForEPLevelEstimates_pooled.pdf"), fig5,
         width = 6, height = 1.5)
}

# Figure 6 ---- 
# Difference in treatment effect at national vs. European level (national party): 
# Pooled estimates for maximum power across all dependent variables
fit1.pool.em = emmeans(fit1.pool, specs = ~treat_bin*level, data = fit1.pool$model)
fit2.pool.em = emmeans(fit2.pool, specs = ~treat_bin*level, data = fit2.pool$model)
fit3.pool.em = emmeans(fit3.pool, specs = ~treat_bin*level, data = fit3.pool$model)
fit4.pool.em = emmeans(fit4.pool, specs = ~treat_bin*level, data = fit4.pool$model)

# compute contrasts and CIs
fit1.pool.em = confint(contrast(fit1.pool.em, 
                                method = contrasterLevel(fit1.pool.em, 'Treat', 'NatEP')), 
                       level = 0.9)
fit2.pool.em = confint(contrast(fit2.pool.em, 
                                method = contrasterLevel(fit2.pool.em, 'Treat', 'NatEP')), 
                       level = 0.9)
fit3.pool.em = confint(contrast(fit3.pool.em, 
                                method = contrasterLevel(fit3.pool.em, 'Treat', 'NatEP')), 
                       level = 0.9)
fit4.pool.em = confint(contrast(fit4.pool.em, 
                                method = contrasterLevel(fit4.pool.em, 'Treat', 'NatEP')), 
                       level = 0.9)

CIs = bind_rows(fit1.pool.em %>% mutate(DV = 'SuppCollab'), 
                fit2.pool.em %>% mutate(DV = 'PartyAttachChange'), 
                fit3.pool.em %>% mutate(DV = 'turnout'), 
                fit4.pool.em %>% mutate(DV = 'sticky'))
CIs$DV = factor(CIs$DV, 
                levels = c('sticky', 'turnout', 'PartyAttachChange', 'SuppCollab'),
                labels = c('Vote for preferred party', 'Turnout propensity' , 'Change in attachment', 'Support for collaboration'))


fig6 = ggplot(CIs, aes(x = estimate, xmin = lower.CL, xmax = upper.CL,
                        y = DV)) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_point(size = 1.6) + 
  geom_linerange(size = 0.7) +
  labs(x = 'Pooled contrast estimate') +
  theme_bw() +
  theme(axis.title.y = element_blank())
fig6
if(!file.exists(here("figures", "Fig6_NatEPLevelEstimates_pooled.pdf"))){
  ggsave(here("figures", "Fig6_NatEPLevelEstimates_pooled.pdf"), fig6,
         width = 6, height = 1.5)
}

# Figure 7 ----
mod7DE = lm(SuppCollab ~ treat*PartySupport, data = dta, subset = (Country == "DE"))
mod7DE.em = emmeans(mod7DE, specs = ~treat*PartySupport, data = mod7DE$model)

mod7IT = lm(SuppCollab ~ treat*PartySupport, data = dta, subset = (Country == "IT"))
mod7IT.em = emmeans(mod7IT, specs = ~treat*PartySupport, data = mod7IT$model)

# obtain contrasts
intrDE = expand.grid(treat = c('Left', 'Auto', 'Xeno'),
                     PartySupport = c("CDU", "GRU", "SPD", "LIN", "AfD", "FDP"),
                     stringsAsFactors = F)
intrIT = expand.grid(treat = c('Left', 'Auto', 'Xeno'),
                     PartySupport = c("PD", "M5S", "Lega", "FdI", "IV", "FI", "LeU"),
                     stringsAsFactors = F)
mod7DE.contr = contrast(mod7DE.em, method = contrasterParty(mod7DE.em, 
                                                            intrDE$treat, 
                                                            intrDE$PartySupport), 
                        adjust = adjust)
mod7DE.contrCI = confint(mod7DE.contr)
mod7IT.contr = contrast(mod7IT.em, method = contrasterParty(mod7IT.em, 
                                                            intrIT$treat, 
                                                            intrIT$PartySupport), 
                        adjust = adjust)
mod7IT.contrCI = confint(mod7IT.contr)

# combine
mod7.contr = bind_rows(mod7DE.contrCI %>% mutate(Country = "DE"),
                       mod7IT.contrCI %>% mutate(Country = "IT")) %>%
  mutate(treat = str_extract(contrast, 'Left|Auto|Xeno'),
         PartySupport = substring(contrast, 5, 10))

# relevel factors
mod7.contr$treat = factor(mod7.contr$treat, 
                          levels = c("Xeno","Auto","Left"),
                          labels = c("Far Right", "Autocratic", "Far Left"))
mod7.contr$PartySupport = factor(mod7.contr$PartySupport,
                                 levels = c("AfD", "CDU", "FDP", "SPD", 
                                            "GRU", "LIN",   
                                            "Lega", "FdI", "FI", "IV", "PD", "M5S", "LeU", "Combined"))
mod7.contr$wrapper = paste0(mod7.contr$Country, ': ', mod7.contr$treat)
mod7.contr$wrapper = factor(mod7.contr$wrapper, 
                            levels = c('DE: Far Left', 'IT: Far Left',
                                       'DE: Autocratic', 'IT: Autocratic',
                                       'DE: Far Right', 'IT: Far Right'))

fig7 = ggplot(mod7.contr, aes(x = estimate, y = PartySupport,
                              xmin = lower.CL, xmax = upper.CL))+
  geom_vline(xintercept = 0, linetype = 2) + 
  geom_pointrange(fatten = 1.4) + 
  facet_wrap(~wrapper, ncol = 2,
             scales = 'free_y', drop = T,
             strip.position = 'right') +
  theme_bw() + 
  labs(x = "Support for collaboration relative to control") +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = theme_get()$text$size - 2,face = "bold"),
        axis.title.y = element_blank())
fig7
if(!file.exists(here("figures", "Fig7_PartyEstimates.pdf"))){
  ggsave(here("figures", "Fig7_PartyEstimates.pdf"), fig7,
         width = 6, height = 4)
}
