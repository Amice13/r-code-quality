### Check that our main result is robust if we use the leave-one-out starting
### values (just using nominal SEs)

date()
library("tidyverse")
library("broom")
library("foreach")
library("haven")
library("iterators")
sessionInfo()


raw_vict <- read_stata("violence_t_export.dta")
raw_phat <- read_csv("results-loo-first-stage.csv")


## Clean victimization data and merge in our preferred CCPs
data_vict <- raw_vict %>%
    select(muni_code, victim_farc, victim_paras, nbi_t, royalties_t, coca_t,
           share_left, lpobl_tot_t, time, army_dist, gini_i, dmr, evlp, gcaribe,
           gpacifica, gorinoquia, gamazonia, farc_dist, paras_dist) %>%
    rename(victim_para = victim_paras,
           para_dist = paras_dist) %>%
    na.omit() %>%
    left_join(raw_phat, by = c("muni_code", "time"))

## Create model matrices and extract other variables used to run model
formula_farc <- victim_farc ~ nbi_t + royalties_t + coca_t + share_left +
    lpobl_tot_t + factor(time) + army_dist + gini_i + dmr +
    evlp + gcaribe + gpacifica + gorinoquia + gamazonia +
    farc_dist + p_para_rf
formula_para <- update(formula_farc,
                       victim_para ~. - farc_dist - p_para_rf + para_dist + p_farc_rf)

fit_farc <- glm(formula_farc, data = data_vict, family = binomial(link = "logit"))
fit_para <- glm(formula_para, data = data_vict, family = binomial(link = "logit"))

cat("\nFARC model:\n")
tidy(fit_farc) %>%
    mutate_if(is.numeric, ~ sprintf("%.3f", .)) %>%
    print(n = Inf)
print(glance(fit_farc))
print(nobs(fit_farc))

cat("\nAUC model:\n")
tidy(fit_para) %>%
    mutate_if(is.numeric, ~ sprintf("%.3f", .)) %>%
    print(n = Inf)
print(glance(fit_para))
print(nobs(fit_para))

tab_farc <- tidy(fit_farc) %>%
    mutate(stars = case_when(
               p.value < 0.01 ~ "***",
               p.value < 0.05 ~ "**",
               p.value < 0.1 ~ "*",
               TRUE ~ ""
           )) %>%
    mutate_if(is.numeric, ~ sprintf("%.3f", .)) %>%
    mutate(estimate = paste0(estimate, stars)) %>%
    select(term, est_farc = estimate, se_farc = std.error)

tab_para <- tidy(fit_para) %>%
    mutate(stars = case_when(
               p.value < 0.01 ~ "***",
               p.value < 0.05 ~ "**",
               p.value < 0.1 ~ "*",
               TRUE ~ ""
           )) %>%
    mutate_if(is.numeric, ~ sprintf("%.3f", .)) %>%
    mutate(estimate = paste0(estimate, stars)) %>%
    select(term, est_para = estimate, se_para = std.error)

cat("\nMerged regression table\n")
print(full_join(tab_farc, tab_para, by = "term"), n = Inf)

cat("\nOverall log-likelihood:\n")
sprintf("%.2f", logLik(fit_farc) + logLik(fit_para))
