############################################### #
# The Psychology of Online Political Hostility  #
#                                               #
# This code analyses the data from Study 5      #
#                                               #
# 2021.03.30.                                   #
# code by Alexander Bor                         #
############################################### #



######################################################################### #
# Study 5: Vignette Experiment -------------------------------------------
######################################################################### #
if (!require("pacman")) install.packages("pacman")
pacman::p_load("stargazer", "here", "tidyverse", "lfe", "interactions")
source(here("C.code/functions.R"))

# import cleaned data
dfe <- readRDS(file = here("B.analysis.data/s5_yougov.rds"))

# N 
length(unique(dfe$ID))

# combine simple FE models to estimate online vs offline condition
stargazer(lfe::felm(offensive ~ online + public | ID + message, dfe),
          lfe::felm(inappropriate ~ online + public | ID + message, dfe), 
          lfe::felm(rare ~ online +public | ID + message, dfe), 
          
          lfe::felm(offensive ~   message | ID, dfe),
          lfe::felm(inappropriate ~ message | ID, dfe),
          lfe::felm(rare ~ message | ID, dfe),
          
          covariate.labels = c("Online", "Public", "Message - Capitol", 
                               "Message - COVID19", "Message - Immigration"),
          
          dep.var.labels = rep(c("Offensive", "Inappropriate", "Rare"), 2),
          keep.stat = c("n", "adj.rsq"), 
          digits = 2,
          title = "Table E1. Fixed effects regression results for Study 5",
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "D.documents/tables/te1_vignette-results.html",
          type = "text")


# ... interaction between online vs public
summary(lfe::felm(offensive ~ online * public | ID + message, dfe))
summary(lfe::felm(inappropriate ~ online* public | ID + message, dfe))
summary(lfe::felm(rare ~ online* public | ID + message, dfe))

# merge experimental data with self-reports
df.us3.part <- readRDS(here("B.analysis.data/s4_us_2021.rds")) %>% 
  filter(part_online > 0 & part_offline > 0) 


dfe2 <- dfe %>% 
    # group by respondents and online/offline mani
    group_by(ID, online) %>% 
    # calculate average inappropriateness for each respondent
    #   separately for online and offline context
    summarise(inappropriate = mean(inappropriate)) %>% 
    # reorganize online and offline inappropriateness as separate cols
    pivot_wider(names_from = online, values_from = inappropriate) %>% 
    ungroup() %>% 
    # calculate the difference between online and offline for each resp
    mutate(diff = arm::rescale(offline - online)) %>% 
    left_join(df.us3.part)    


# check if inappropriateness gap predicts online hostility
fit1 <- lm(hostile_online ~  diff, dfe2)
fit2 <- lm(hostile_online ~  diff * hostile_offline, dfe2)


stargazer(fit1,fit2,
          dep.var.labels = "Online Hostility", 
          covariate.labels = c("Inappropriateness gap", "Offline Hostility",
                               "Inappropriateness gap × Offline Hostility"),
          keep.stat = c("n", "adj.rsq"), 
          digits = 2, 
          single.row = T,
          title = "Table E2. No interaction between inappropriateness gap and offline hostility",
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "D.documents/tables/te2_inappr-gap.html",
          type = "text")  

# present results in a predicted values plot
interact_plot(fit2, pred = hostile_offline, modx = diff, 
                            interval = T, facet.modx = F)

# dfe %>% 
#         group_by(online) %>% 
#         summarise(offensive.m = mean(offensive), 
#                   offensive.se = se(offensive), 
#                   inappropriate.m = mean(inappropriate), 
#                   inappropriate.se =  se(inappropriate), 
#                   rare.m = mean(rare), 
#                   rare.se =  se(rare)) %>% 
#         pivot_longer(offensive.m: rare.se) %>% 
#         separate(name, into=c("var", "stat"), sep = "\\.") %>% 
#         pivot_wider(names_from = stat, values_from = value) %>% 
#         mutate(var = fct_relevel(var, "offensive")) %>% 
#         ggplot(., aes(x = online, y = m)) + 
#         geom_point() +
#         geom_errorbar(aes(ymin = m - 1.96 * se, 
#                           ymax = m + 1.96 * se), 
#                       width = 0) +
#         facet_grid(~ var) + 
#         theme_bw()
# 
# 
# 
# dfe %>% 
#         group_by(online, public) %>% 
#         summarise(offensive.m = mean(offensive), 
#                   offensive.se = se(offensive), 
#                   inappropriate.m = mean(inappropriate), 
#                   inappropriate.se =  se(inappropriate), 
#                   rare.m = mean(rare), 
#                   rare.se =  se(rare)) %>% 
#         pivot_longer(offensive.m: rare.se) %>% 
#         separate(name, into=c("var", "stat"), sep = "\\.") %>% 
#         pivot_wider(names_from = stat, values_from = value) %>% 
#         mutate(var = fct_relevel(var, "offensive")) %>% 
#         ggplot(., aes(x = online, y = m, color = public)) + 
#         geom_point(position = position_dodge(0.4)) +
#         geom_errorbar(aes(ymin = m - 1.96 * se, 
#                           ymax = m + 1.96 * se), 
#                       width = 0, 
#                       position = position_dodge(0.4)) +
#         ylim(0, 0.55) + 
#         facet_grid(~ var) + 
#         theme_bw()
# 
# dfe %>% 
#         group_by(message, online) %>% 
#         summarise(offensive.m = mean(offensive), 
#                   offensive.se = se(offensive), 
#                   inappropriate.m = mean(inappropriate), 
#                   inappropriate.se =  se(inappropriate), 
#                   rare.m = mean(rare), 
#                   rare.se =  se(rare)) %>% 
#         pivot_longer(offensive.m: rare.se) %>% 
#         separate(name, into=c("var", "stat"), sep = "\\.") %>% 
#         pivot_wider(names_from = stat, values_from = value) %>% 
#         mutate(var = fct_relevel(var, "offensive")) %>% 
#         ggplot(., aes(x = message, y = m, color = online)) + 
#         geom_point(position = position_dodge(0.4)) +
#         geom_errorbar(aes(ymin = m - 1.96 * se, 
#                           ymax = m + 1.96 * se), 
#                       width = 0, 
#                       position = position_dodge(0.4)) +
#         # ylim(0, 0.55) + 
#         facet_grid(~ var) + 
#         theme_bw()
# 
# 
# # post hoc analysis using experimental repsonses as an individual diff. meas.
# 
# # first calculate how inappropriate were the messages on average.
# dfu_avg <- dfe %>% 
#         group_by(ID) %>% 
#         summarise(inappropriate = mean(inappropriate)) %>% 
#         ungroup() %>% 
#         mutate(inappropriate = arm::rescale(inappropriate)) %>% 
#         glimpse()
# 
# # then calculate a difference between offline - online inappropriateness
# #       higher values indicate stricter offline norms.  
# dfu_byplat <-  dfe %>% 
#         group_by(ID, online) %>% 
#         summarise(inappropriate = mean(inappropriate)) %>% 
#         pivot_wider(names_from = online, values_from = inappropriate) %>% 
#         ungroup() %>% 
#         mutate(diff = arm::rescale(offline - online)) %>% 
#         glimpse
# 
# # combine the aggregate and the difference scores.
# dfu <- left_join(dfu_avg, dfu_byplat)%>% 
#         mutate(ID = as.factor(ID))
# 
# # combine with self-reports in study 4a
# foo <- left_join(df.us3, dfu) 
# 
# 
# round(cor(foo[, c("inappropriate", "diff", "hostile_online", "hostile_offline")], 
#     use = "pairwise.complete.obs"), 2)
# 
# round(cor(dfe[, c("inappropriate", "rare", "offensive")], 
#           use = "pairwise.complete.obs"), 2)
# 
# 
# bar1 <- (lm(hostile_online ~ hostile_offline * diff, foo))
# interactions::interact_plot(bar1, pred = hostile_offline, modx = diff, 
#                             interval = T, facet.modx = F)
# 
# summary(bar1, type = "text")
# 
# 
# cor.test(as.numeric(is.na(foo$hostile_online)), foo$diff)
# 
# cor.test(as.numeric(is.na(foo$hostile_offline)), foo$sdrt)
# 
# summary(lm(is.na(hostile_offline)~ sdrt, foo))
# summary(lm(is.na(hostile_online)~ sdrt, foo))
# 
# summary(lm(part_online~ sdrt, df.us21))
# summary(lm(part_offline~ sdrt, df.us21))
# 
# ggplot(foo, aes(x = inappropriate, y = hostile_online)) +
#         geom_jitter(alpha = 0.21, width= 0.1) +
#         geom_smooth() + 
#         theme_bw()
# 
# stargazer(bar1, bar2, type = "text")

