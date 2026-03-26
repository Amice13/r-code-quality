###Q###Q###Q###Q###Q###Q
###Q###Q Robust ###Q###Q
###Q###Q###Q###Q###Q###Q

## NOT RUN:
## This script needs to be called from within KSFR23_Appendix.R
## OR 
## This script requires a suitable dataset 'dta' and the correct packages loaded (as in KSFR23_appendix)

## DV labels for plots
lab1 = c(SuppCollab = "Support for collaboration",
         PartyAttachChange = "Change in party attachment",
         turnout = "Turnout",
         sticky = "Propensity to vote for preferred party")


## Figures ====
###Q###Q###Q###Q###Q###Q###Q
###Q###Q Figure 3   ###Q###Q
###Q###Q###Q###Q###Q###Q###Q
# Difference in treatment effect at national vs. European level: Across treatments and for DE & IT
#fit models
mod1DE = lm(as.formula(paste0(DV, "~ treat*level")), dta, subset = (Country == 'DE'))
mod1DE.em = emmeans(mod1DE, specs = ~treat*level, data = mod1DE$model)
mod1IT = lm(as.formula(paste0(DV, "~ treat*level")), dta, subset = (Country == 'IT'))
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


