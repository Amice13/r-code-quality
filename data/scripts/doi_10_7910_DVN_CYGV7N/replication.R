library(tidyverse)
library(NipponMap)
library(sf)
library(BalanceR) # devtools::install_github("JaehyunSong/BalanceR")
library(modelsummary)
library(gt)
library(broom)

ULE <- read_csv("ule2023.csv")

# Distribution map -- Figure 1----
Nippon_map <- read_sf(system.file("shapes/jpn.shp", package = "NipponMap")[1],
                      crs = "+proj=longlat +datum=WGS84")
ELE <-read_csv("treatment.csv")
ELE$pref <- as.character(ELE$pref)

mapDB <- left_join(Nippon_map, ELE, by = c("SP_ID" = "pref"))

fig1 <- ggplot(mapDB, aes(fill = factor(ele2, labels = c("No elections", 
                                                         "Concil elections alone", 
                                                         "Both gubernatorial and council elections")))) + 
  geom_sf() +
  theme_gray (base_family = "HiraKakuPro-W3")+
  labs(x = "", y = "", fill = "Treatment") +
  theme(legend.position = c(.7,.15))
fig1
ggsave("fig1.png", plot = fig1, dpi = 400)

# Balance check -- Figure 2 & 3----
BlcChk1 <- BalanceR(data = ULE, group = ele1,
                    cov  = c(Sex, Age, Education, Income, Urban, LDP, Ideology, Risk))
BlcChk2 <- BalanceR(data = ULE, group = ele2,
                    cov  = c(Sex, Age, Education, Income, Urban, LDP, Ideology, Risk))

fig2 <- plot(BlcChk1)
fig3 <- plot(BlcChk2)
fig2
fig3
ggsave("fig2.png", plot = fig2, dpi = 400)
ggsave("fig3.png", plot = fig3, dpi = 400)

# Regression -- Table 1 & Appendix 1----
ULE2 <- ULE %>% 
  mutate(sex = factor(Sex)) %>% 
  mutate(ele2 = factor(ele2)) %>% 
  mutate(ele1 = factor(ele1)) %>% 
  mutate(urban = factor(Urban, levels = c(2,1,3))) %>% 
  mutate(wa = factor(wa)) %>% 
  mutate(wave = factor(wave))

model1_1a <- lm(dv1 ~ ele1*wave + urban + wa, data = ULE2)
model1_2a <- lm(dv2 ~ ele1*wave + urban + wa, data = ULE2)
model1_3a <- lm(dv3 ~ ele1*wave + urban + wa, data = ULE2)
model1_1b <- lm(dv1 ~ ele1*wave + urban + wa + LDP, data = ULE2)
model1_2b <- lm(dv2 ~ ele1*wave + urban + wa + LDP, data = ULE2)
model1_3b <- lm(dv3 ~ ele1*wave + urban + wa + LDP, data = ULE2)

models1 <- list()
models1[["1a"]] <- model1_1a
models1[["1b"]] <- model1_1b
models1[["2a"]] <- model1_2a
models1[["2b"]] <- model1_2b
models1[["3a"]] <- model1_3a
models1[["3b"]] <- model1_3b

var_nam = c("LDP" = "LDP support","ele11:wave10"= "Treatment * Time")
tab1 <- msummary(models1, stars = TRUE, coef_map = var_nam, gof_omit = "F|AIC|BIC|Log.Lik.", output = "gt") %>% 
  opt_table_font(font = "Cambria") %>% 
  tab_spanner(columns = 2:3, label = "External efficacy") %>% 
  tab_spanner(columns = 4:5, label = "Internal efficacy") %>% 
  tab_spanner(columns = 6:7, label = "Political trust") 
tab1
gtsave(tab1, "tab1.docx")

var_nam_app1 = c("wa1" = "Uncontested", "LDP" = "LDP support", "urban1" = "Metropolis", "urban3" = "Rural area",
                 "ele11" = "Treatment", "wave10" = "Time","ele11:wave10"= "Treatment * Time", "(Intercept)" = "Intercept")
app1 <- msummary(models1, stars = TRUE, coef_map = var_nam_app1, gof_omit = "F|AIC|BIC|Log.Lik.", output = "gt") %>% 
  opt_table_font(font = "Cambria") %>% 
  tab_spanner(columns = 2:3, label = "External efficacy") %>% 
  tab_spanner(columns = 4:5, label = "Internal efficacy") %>% 
  tab_spanner(columns = 6:7, label = "Political trust") 
app1
gtsave(app1, "app1.docx")

# Regression -- Table 2 & Appendix 2----
model2_1a <- lm(dv1 ~ ele2*wave + urban + wa, data = ULE2)
model2_1b <- lm(dv1 ~ ele2*wave + urban + wa + LDP, data = ULE2)
model2_2a <- lm(dv2 ~ ele2*wave + urban + wa, data = ULE2)
model2_2b <- lm(dv2 ~ ele2*wave + urban + wa + LDP, data = ULE2)
model2_3a <- lm(dv3 ~ ele2*wave + urban + wa, data = ULE2)
model2_3b <- lm(dv3 ~ ele2*wave + urban + wa + LDP, data = ULE2)

models2 <- list()
models2[["1a"]] <- model2_1a
models2[["1b"]] <- model2_1b
models2[["2a"]] <- model2_2a
models2[["2b"]] <- model2_2b
models2[["3a"]] <- model2_3a
models2[["3b"]] <- model2_3b

var_nam2 = c("LDP" = "LDP support","ele21:wave10"= "Treatment1 * Time","ele22:wave10"= "Treatment2 * Time")
tab2 <- msummary(models2, stars = TRUE, coef_map = var_nam2, gof_omit = "F|AIC|BIC|Log.Lik.", output = "gt") %>% 
  tab_spanner(columns = 2:3, label = "External efficacy") %>% 
  tab_spanner(columns = 4:5, label = "Internal efficacy") %>% 
  tab_spanner(columns = 6:7, label = "Political trust") 
tab2
gtsave(tab2, "tab2.docx")

var_nam_app2 = c("wa1" = "Uncontested", "LDP" = "LDP support", "urban1" = "Metropolis", "urban3" = "Rural area",
                 "ele21" = "Treatment1", "ele22" = "Treatment2", "wave10" = "Time", "ele21:wave10"= "Treatment1 * Time", 
                 "ele22:wave10"= "Treatment2 * Time", "(Intercept)" = "Intercept")
app2 <- msummary(models2, stars = TRUE, coef_map = var_nam_app2, gof_omit = "F|AIC|BIC|Log.Lik.", output = "gt") %>% 
  tab_spanner(columns = 2:3, label = "External efficacy") %>% 
  tab_spanner(columns = 4:5, label = "Internal efficacy") %>% 
  tab_spanner(columns = 6:7, label = "Political trust") 
app2
gtsave(app2, "app2.docx")
  
# Heterogeneity of effects -- Figure 4----
ULE2 <- ULE2 %>% 
  mutate(Age2 = ifelse(Age < 30, 0, 1)) %>% 
  mutate(Education2 = ifelse(Education < 4, 0, 1)) %>% 
  mutate(Income2 = ifelse(Income < 5, 0, 
                          ifelse(Income < 15, 1, NA)))

model3_1a <- lm(dv3 ~ ele2*wave + urban + wa + LDP, data = ULE2 %>% filter(Age2 ==0))
model3_1b <- lm(dv3 ~ ele2*wave + urban + wa + LDP, data = ULE2 %>% filter(Age2 ==1))
model3_2a <- lm(dv3 ~ ele2*wave + urban + wa + LDP, data = ULE2 %>% filter(Education2 ==0))
model3_2b <- lm(dv3 ~ ele2*wave + urban + wa + LDP, data = ULE2 %>% filter(Education2 ==1))
model3_3a <- lm(dv3 ~ ele2*wave + urban + wa + LDP, data = ULE2 %>% filter(Income2 ==0))
model3_3b <- lm(dv3 ~ ele2*wave + urban + wa + LDP, data = ULE2 %>% filter(Income2 ==1))
models3 <- list()
models3[["Under 30"]] <- model3_1a
models3[["Over 30"]] <- model3_1b
models3[["Education low"]] <- model3_2a
models3[["Education high"]] <- model3_2b
models3[["Income low"]] <- model3_3a
models3[["Income high"]] <- model3_3b

tab_model3 <- as.tibble(c("Under 30", "Over 30", "Education low", "Education high", "Income low","Income high"))
tab_model3 <- tab_model3 %>% 
  mutate(m = map(models3, tidy, conf.int = TRUE)) %>% 
  unnest() %>% 
  filter(term == "ele22:wave10") %>% 
  select(value, estimate, conf.high, conf.low) %>% 
  mutate(value = factor(value, levels = c("Income high", "Income low", "Education high", "Education low", "Over 30", "Under 30")))

fig4 <- tab_model3 %>% 
  ggplot() +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_pointrange(aes(x = value, y = estimate,
                      ymin = conf.low, ymax = conf.high),
                  size = 0.75, linewidth = 0.75) +
  labs(x = "", y = "estimate with 95% CI") +
  coord_flip()
fig4
ggsave("fig4.png", plot = fig4, dpi = 400)

var_nam_app3 = c("wa1" = "Uncontested", "LDP" = "LDP support", "urban1" = "Metropolis", "urban3" = "Rural area",
                 "ele21" = "Treatment1", "ele22" = "Treatment2", "wave10" = "Time", "ele21:wave10"= "Treatment1 * Time", 
                 "ele22:wave10"= "Treatment2 * Time", "(Intercept)" = "Intercept")
app3 <- msummary(models3, stars = TRUE, coef_map = var_nam_app3, gof_omit = "F|AIC|BIC|Log.Lik.", output = "gt") %>% 
  tab_spanner(columns = 2:3, label = "Age") %>% 
  tab_spanner(columns = 4:5, label = "Education") %>% 
  tab_spanner(columns = 6:7, label = "Income") 
app3
gtsave(app3, "app3.docx")

# Heterogeneity of effects -- Political efficacy----

model4_1a <- lm(dv1 ~ ele2*wave + urban + wa + LDP, data = ULE2 %>% filter(Age2 ==0))
model4_1b <- lm(dv1 ~ ele2*wave + urban + wa + LDP, data = ULE2 %>% filter(Age2 ==1))
model4_2a <- lm(dv1 ~ ele2*wave + urban + wa + LDP, data = ULE2 %>% filter(Education2 ==0))
model4_2b <- lm(dv1 ~ ele2*wave + urban + wa + LDP, data = ULE2 %>% filter(Education2 ==1))
model4_3a <- lm(dv1 ~ ele2*wave + urban + wa + LDP, data = ULE2 %>% filter(Income2 ==0))
model4_3b <- lm(dv1 ~ ele2*wave + urban + wa + LDP, data = ULE2 %>% filter(Income2 ==1))
models4 <- list()
models4[["Under 30"]]       <- model4_1a
models4[["Over 30"]]        <- model4_1b
models4[["Education low"]]  <- model4_2a
models4[["Education high"]] <- model4_2b
models4[["Income low"]]     <- model4_3a
models4[["Income high"]]    <- model4_3b

var_nam_app3 = c("wa1" = "Uncontested", "LDP" = "LDP support", "urban1" = "Metropolis", "urban3" = "Rural area",
                 "ele21" = "Treatment1", "ele22" = "Treatment2", "wave10" = "Time", "ele21:wave10"= "Treatment1 * Time", 
                 "ele22:wave10"= "Treatment2 * Time", "(Intercept)" = "Intercept")
app4 <- msummary(models4, stars = TRUE, coef_map = var_nam_app3, gof_omit = "F|AIC|BIC|Log.Lik.", output = "gt") %>% 
  tab_spanner(columns = 2:3, label = "Age") %>% 
  tab_spanner(columns = 4:5, label = "Education") %>% 
  tab_spanner(columns = 6:7, label = "Income") 
app4
gtsave(app4, "app4.docx")

model5_1a <- lm(dv2 ~ ele2*wave + urban + wa + LDP, data = ULE2 %>% filter(Age2 ==0))
model5_1b <- lm(dv2 ~ ele2*wave + urban + wa + LDP, data = ULE2 %>% filter(Age2 ==1))
model5_2a <- lm(dv2 ~ ele2*wave + urban + wa + LDP, data = ULE2 %>% filter(Education2 ==0))
model5_2b <- lm(dv2 ~ ele2*wave + urban + wa + LDP, data = ULE2 %>% filter(Education2 ==1))
model5_3a <- lm(dv2 ~ ele2*wave + urban + wa + LDP, data = ULE2 %>% filter(Income2 ==0))
model5_3b <- lm(dv2 ~ ele2*wave + urban + wa + LDP, data = ULE2 %>% filter(Income2 ==1))
models5 <- list()
models5[["Under 30"]]       <- model5_1a
models5[["Over 30"]]        <- model5_1b
models5[["Education low"]]  <- model5_2a
models5[["Education high"]] <- model5_2b
models5[["Income low"]]     <- model5_3a
models5[["Income high"]]    <- model5_3b

var_nam_app3 = c("wa1" = "Uncontested", "LDP" = "LDP support", "urban1" = "Metropolis", "urban3" = "Rural area",
                 "ele21" = "Treatment1", "ele22" = "Treatment2", "wave10" = "Time", "ele21:wave10"= "Treatment1 * Time", 
                 "ele22:wave10"= "Treatment2 * Time", "(Intercept)" = "Intercept")
app5 <- msummary(models5, stars = TRUE, coef_map = var_nam_app3, gof_omit = "F|AIC|BIC|Log.Lik.", output = "gt") %>% 
  tab_spanner(columns = 2:3, label = "Age") %>% 
  tab_spanner(columns = 4:5, label = "Education") %>% 
  tab_spanner(columns = 6:7, label = "Income") 
app5
gtsave(app5, "app5.docx")
