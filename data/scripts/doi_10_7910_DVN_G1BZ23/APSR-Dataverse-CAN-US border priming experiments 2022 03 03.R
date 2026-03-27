rm(list=ls())
setwd ("C:/.../Replication materials/")  ## Change working directory
options(scipen=999)
select <- dplyr::select
map <- purrr::map

library(tidyverse)
library(haven)
library(lubridate)
library(survey)
library(jtools)
library(tidycat)
library(cowplot)
library(stargazer)
library(openxlsx)
library(manifestoR)
library(bcaboot)
set.seed(20211105)


## United States
#usa.0a <- read_sav("SVMK_NatID_Immig_USA_20200727.sav")
#replication.usa <- usa.0 %>%
#  select(resp_id:weight, STATE:ST, condition:border_trt, q2, issue_matters_most, q8:ideology, party5, q18:age6, age)
#write_csv(replication.usa, "SVMK_APSR_USA_20210826.csv")
usa.0 <- read_csv("SVMK_APSR_USA_20210826.csv")

usa.1 <- usa.0 %>%
  mutate(mii.immig = case_when(
    issue_matters_most == 5 ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(approve.border = case_when(
    q9 == 1 ~ 0.5,
    q9 == 2 ~ 0.25,
    q9 %in% c(3,6) ~ 0,
    q9 == 4 ~ -0.25,
    q9 == 5 ~ -0.5
  )) %>%
  rename(Trudeau.prime = border_trt) %>%
  rename(nat.id.prime = nat_id_trt) %>%
  #1 = Very conservative, 5 = very liberal >> 0 = Very liberal, 1 = very conservative
  mutate(ideology = case_when(
    ideology == 1 ~ 0.5,
    ideology == 2 ~ 0.25,
    ideology %in% c(3,6) ~ 0,
    ideology == 4 ~ -0.25,
    ideology == 5 ~ -0.5
  )) %>%
  mutate(pid = as_factor(case_when(
    party5 %in% 4:5 ~ "Democrat",
    party5 %in% c(3,6) ~ "Independent",
    party5 %in% 1:2 ~ "Republican"
  ))) %>%
  mutate(pid = fct_relevel(pid, "Democrat", "Independent", "Republican")) %>%
  mutate(male = case_when(
    q18 == 1 ~ 0.5,
    q18 == 2 ~ -0.5,
    q18 == 3 ~ 0
  )) %>%
  mutate(age = as.integer(age)) %>%
  mutate(ln.age.years = log(age) / (max(log(age)) - min(log(age)))) %>%
  mutate(edu = case_when(
    education == 1 ~ -0.5,
    education == 2 ~ 1/3 - 0.5,
    education %in% 3:4 ~ 2/3 - 0.5,
    education == 5 ~ 0.5,
    education == 6 ~ 0.5,
  )) %>%
  mutate(race.ethn = as_factor(case_when(
    q19 == 1 ~ "White",
    q19 == 2 ~ "Black",
    q19 == 3 ~ "Hispanic",
    q19 == 4 ~ "Asian",
    q19 == 5 ~ "Other"
  ))) %>%
  mutate(race.ethn = fct_relevel(race.ethn, "White", "Black", "Hispanic", "Asian", "Other")) %>%
  mutate(region = as_factor(case_when(
    region == 1 ~ "Northeast",
    region == 2 ~ "Midwest",
    region == 3 ~ "South",
    region == 4 ~ "West"
  ))) %>%
  mutate(region = fct_relevel(region, "Northeast", "Midwest", "South", "West"))
  
usa.2 <- usa.1 %>%
  mutate(edu = edu - weighted.mean(edu, w = weight)) %>%
  mutate(ln.age.years = ln.age.years - weighted.mean(ln.age.years, w = weight)) %>%
  mutate(ideology = ideology - weighted.mean(ideology, w = weight))

table(usa.2$approve.border)
table(usa.2$Trudeau.prime)
table(usa.2$nat.id.prime)
table(usa.2$mii.immig)
table(usa.1$ideology)
table(usa.2$pid)
table(usa.2$male)
table(usa.2$edu)
table(usa.2$race.ethn)
table(usa.2$region)
summary(usa.2$edu)
summary(usa.2$ln.age.years)
weighted.mean(usa.2$approve.border, w = usa.2$weight)
weighted.mean(usa.2$ideology, w = usa.2$weight)
weighted.mean(usa.2$edu, w = usa.2$weight)


design.usa <- svydesign(data = usa.2, id = ~1, weights = ~weight, strata = ~ST)


## Models for Table A1
m1.0.0 <- svyglm(approve.border ~ Trudeau.prime,
                 design = design.usa, family = stats::gaussian())
m1.0.1 <- svyglm(approve.border ~ Trudeau.prime + ideology +  pid + male + ln.age.years + edu + race.ethn + region,
                 design = design.usa, family = stats::gaussian())
m1.0.2 <- svyglm(approve.border ~ Trudeau.prime +  mii.immig + male + ln.age.years + edu + race.ethn + region,
                 design = design.usa, family = stats::gaussian())
m1.1 <- svyglm(approve.border ~ Trudeau.prime + ideology +  pid + mii.immig + male + ln.age.years + edu + race.ethn + region,
               design = design.usa, family = stats::gaussian())
m1.2 <- svyglm(approve.border ~ Trudeau.prime + ideology +  pid + mii.immig + male + ln.age.years + edu + race.ethn + region
               + ideology*Trudeau.prime,
               design = design.usa, family = stats::gaussian())
m1.3 <- svyglm(approve.border ~ Trudeau.prime + ideology +  pid + mii.immig + male + ln.age.years + edu + race.ethn + region
               + mii.immig*Trudeau.prime,
               design = design.usa, family = stats::gaussian())
m1.4 <- svyglm(approve.border ~ Trudeau.prime + ideology +  pid + mii.immig + male + ln.age.years + edu + race.ethn + region
               + pid*Trudeau.prime,
               design = design.usa, family = stats::gaussian())

m1.5 <- svyglm(approve.border ~ Trudeau.prime + ideology +  pid + mii.immig + male + ln.age.years + edu + race.ethn + region
               + ideology*Trudeau.prime + mii.immig*Trudeau.prime,
               design = design.usa, family = stats::gaussian())

summ(m1.0.0, digits = 3)
summ(m1.1, digits = 3)
summ(m1.2, digits = 3)
summ(m1.3, digits = 3)
summ(m1.2, digits = 3)
anova(m1.1, m1.2, test = "F")
summ(m1.3, digits = 3)
anova(m1.1, m1.3, test = "F")
summ(m1.4, digits = 3)
anova(m1.1, m1.4, test = "F")
summ(m1.5, digits = 3)
anova(m1.2, m1.5, test = "F")
anova(m1.3, m1.5, test = "F")


## USA: ideology-party ID (for Figure A3)
m1.9 <- svyglm(ideology ~ pid + mii.immig + male + ln.age.years + edu + race.ethn + region,
               design = design.usa, family = stats::gaussian())
summ(m1.9, digits = 3)


## USA: check for interaction between treatments (for Table A5)
mA5 <- svyglm(approve.border ~ Trudeau.prime + nat.id.prime + Trudeau.prime*nat.id.prime,
              design = design.usa, family = stats::gaussian())
summ(mA5, digits = 3)


## USA: MII-immigration (for Table A7)
mA7 <- svyglm(mii.immig ~ ideology + pid + male + ln.age.years + edu + race.ethn + region,
              design = design.usa, family = stats::binomial(link = "logit"))
summ(mA7, digits = 3)

svycor(~mii.immig + ideology, design = design.usa, digits = 3, sig.stats = TRUE)
xtabs(weight ~ mii.immig + ideology, addNA = TRUE, na.action = NULL, data = usa.1) %>% prop.table(margin = 2)*100


m_A7_out <- broom::tidy(mA7) %>%
  mutate(sig = case_when(p.value <= 0.001 ~ "***",
                           p.value <= 0.01 ~ "**",
                           p.value <= 0.05 ~ "*",
                           p.value <= 0.1 ~ "+")) %>%
  bind_rows(., (as_tibble(broom::glance(mA7)) %>%
                  mutate(n = df.null + 1) %>%
                  mutate(Chi.square = deviance - null.deviance) %>%
                  mutate(L_0 = exp(null.deviance / -2)) %>%
                  mutate(L_M = exp(deviance/ -2)) %>%
                  mutate(pwr = 2/n) %>%
                  mutate(Nagelkerke.R2 = (1 - (L_0 / L_M)^pwr) / (1 - (L_0^pwr))) %>%
                  select(Chi.square, Nagelkerke.R2, n) %>%
                  pivot_longer(cols = everything(), names_to = "term", values_to = "estimate"))) %>%
  mutate(model = "A7") %>%
  mutate(estimate = round(estimate, digits = 2)) %>%
  mutate(std.error = str_c("(", as.character(round(std.error, digits = 2)), ")")) %>%
  select(model, everything()) 


## USA: Model summaries; save out results to Excel
stargazer(m1.0.0, m1.0.1, m1.0.2, m1.1, m1.2, m1.3, m1.4, m1.5,
          type = "text",
          column.labels = c("M1.0.0", "M1.0.1", "M1.0.2", "M1.1", "M1.2", "M1.3", "M1.4", "M1.5"),
          single.row = TRUE,  model.numbers = FALSE, intercept.top = TRUE, intercept.bottom = FALSE,
          report = "vcs*", digits = 2, keep.stat = c("rsq", "adj.rsq"),
          star.cutoffs = c(0.05, 0.01, 0.001), star.char = c("*", "**", "***"))

work_book <- createWorkbook()

svy.model.out <- function(model_obj, model_name){
  excel_out <- broom::tidy(model_obj) %>%
    mutate(sig = case_when(p.value <= 0.001 ~ "***",
                           p.value <= 0.01 ~ "**",
                           p.value <= 0.05 ~ "*",
                           p.value <= 0.1 ~ "+")) %>%
    bind_rows(., (as_tibble(broom::glance(model_obj)) %>%
                    mutate(R2 = 1 - (deviance / null.deviance)) %>%
                    mutate(n = df.null + 1) %>%
                    select(R2, n) %>%
                    pivot_longer(cols = everything(), names_to = "term", values_to = "estimate"))) %>%
    mutate(model = model_name) %>%
    mutate(estimate = round(estimate, digits = 2)) %>%
    mutate(std.error = str_c("(", as.character(round(std.error, digits = 2)), ")")) %>%
    select(model, everything()) 
  openxlsx::addWorksheet(work_book, sheetName = model_name)
  openxlsx::writeData(work_book, model_name, excel_out)
}

## Export results for Table A1, Table A5 & Table A7
svy.model.out(model_obj = m1.0.0, model_name = "US-m1.0.0")
svy.model.out(model_obj = m1.0.1, model_name = "US-m1.0.1")
svy.model.out(model_obj = m1.0.2, model_name = "US-m1.0.2")
svy.model.out(model_obj = m1.1, model_name = "US-m1.1")
svy.model.out(model_obj = m1.2, model_name = "US-m1.2")
svy.model.out(model_obj = m1.3, model_name = "US-m1.3")
svy.model.out(model_obj = m1.4, model_name = "US-m1.4")
svy.model.out(model_obj = m1.5, model_name = "US-m1.5")
svy.model.out(model_obj = m1.9, model_name = "US-m1.9")
svy.model.out(model_obj = mA5, model_name = "US-A5")
openxlsx::addWorksheet(work_book, sheetName = "US-A7")
openxlsx::writeData(work_book, "US-A7", m_A7_out)

saveWorkbook(work_book, "CAN-US border priming experiment 2022 03 03.xlsx", overwrite = TRUE)


## USA: predicted values
score.usa.ideology <- full_seq(-0.5:0.5, 0.01) %>% as_tibble() %>%
  mutate(ideology = value - (weighted.mean(usa.1$ideology, w = usa.1$weight))) %>%
  select(-value) %>%
  crossing(full_seq(0:1, 1) %>% as_tibble()) %>%
  rename(Trudeau.prime = value) %>%
  mutate(pid = as_factor("Independent")) %>%
  mutate(mii.immig = 0) %>%
  mutate(male = -0.5) %>%
  mutate(ln.age.years = 0) %>%
  mutate(edu = max(usa.2$edu)) %>%
  mutate(race.ethn = "White") %>%
  mutate(region = "Northeast") %>%
  arrange(Trudeau.prime)

score.usa.pid <- c("Democrat", "Independent", "Republican") %>% as_tibble() %>%
  mutate(pid = as_factor(value)) %>%
  select(-value) %>%
  crossing(full_seq(0:1, 1) %>% as_tibble()) %>%
  rename(Trudeau.prime = value) %>%
  mutate(ideology = 0) %>%
  mutate(mii.immig = 0) %>%
  mutate(male = -0.5) %>%
  mutate(ln.age.years = 0) %>%
  mutate(edu = max(usa.2$edu)) %>%
  mutate(race.ethn = "White") %>%
  mutate(region = "Northeast") %>%
  arrange(Trudeau.prime)

score.usa.prime <- full_seq(0:1, 1) %>% as_tibble() %>%
  rename(Trudeau.prime = value) %>%
  mutate(ideology = 0) %>%
  mutate(pid = as_factor("Independent")) %>%
  mutate(mii.immig = 0) %>%
  mutate(male = -0.5) %>%
  mutate(ln.age.years = 0) %>%
  mutate(edu = max(usa.2$edu)) %>%
  mutate(race.ethn = "White") %>%
  mutate(region = "Northeast") %>%
  arrange(Trudeau.prime)


## H1: main effect of ideology; left panel of Figure 3
pred.m1.1.ideo <- predict(m1.1, (score.usa.ideology %>% filter(Trudeau.prime == 0) %>%
                                   mutate(Trudeau.prime = 0.5)), interval = "confidence") %>%
  as_tibble() %>%
  bind_cols(., (score.usa.ideology %>% filter(Trudeau.prime == 0))) %>%
  #mutate(Trudeau.prime = as_factor(case_when(
  #  Trudeau.prime == 0 ~ "No prime",
  #  Trudeau.prime == 1 ~ "Trudeau prime"
  #))) %>%
  mutate(ideology = ideology + (weighted.mean(usa.1$ideology, w = usa.1$weight))) %>%
  #mutate(Trudeau.prime = fct_relevel(Trudeau.prime, "Trudeau prime", "No prime")) %>%
  mutate(Trudeau.prime = 0.5) %>%
  #filter(ideology %in% c(0, 0.5, 1) & Trudeau.prime == "No prime") %>%
  filter(ideology %in% c(-0.5, 0, 0.5)) %>%
  mutate(PRED = link) %>%
  mutate(UCL = link + (SE * (qnorm(0.975)))) %>%
  mutate(LCL = link - (SE * (qnorm(0.975)))) %>%
  select(ideology, Trudeau.prime, PRED, UCL, LCL)

print(pred.m1.1.ideo %>% select(ideology, PRED, UCL, LCL), digits = 2)

plot.usa.ideology.h1 <- ggplot(data = pred.m1.1.ideo, aes(y = PRED, x = ideology)) +
  geom_line() +
  geom_ribbon(aes(ymin = LCL, ymax = UCL), alpha = 0.1) +
  labs(y = "Support/oppose Canada-US border closure",
       x = "Ideology (liberal-conservative)") +
  coord_cartesian(ylim = c(-0.5, 0.6), xlim = c(-0.5, 0.5)) +
  scale_y_continuous(breaks = seq(-0.5, 0.5, by = 0.25),
                     labels = c("Strongly\noppose", "", "", "", "Strongly\nsupport")) +
  scale_x_continuous(breaks = seq(-0.5, 0.5, by = 0.25),
                     labels = c("Very liberal","","","","Very conservative")) +
  scale_linetype_manual(values = c("solid")) +
  #guides(colour = guide_legend(override.aes = list(fatten = 1, size = 0.5))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 25)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        text = element_text(size = 10, colour = "black"),
        legend.title = element_blank(),
        plot.margin = margin(t = 0, b = 0, l = 0, r = 0.5, "in")) +
  ggplot2::annotate("text", x = -0.475, y = -0.475, label = "United States", size = 4, fontface = "bold",
           hjust = 0, vjust = 0)

#ggsave(plot = plot.usa.ideology.h1, file = "Effect plot-USA-Ideology-Main 2022 03 03.png",
#       device = "png",  width = 5.35, height = 3.25, units = "in", dpi = 1200)


## H1: main effect of party ID (for appendix)
pred.m1.1.pid <- predict(m1.1, (score.usa.pid %>% filter(Trudeau.prime == 0) %>%
                                   mutate(Trudeau.prime = 0.5)), interval = "confidence") %>%
  as_tibble() %>%
  bind_cols(., (score.usa.pid %>% filter(Trudeau.prime == 0))) %>%
  #mutate(Trudeau.prime = as_factor(case_when(
  #  Trudeau.prime == 0 ~ "No prime",
  #  Trudeau.prime == 1 ~ "Trudeau prime"
  #))) %>%
  #mutate(Trudeau.prime = fct_relevel(Trudeau.prime, "Trudeau prime", "No prime")) %>%
  mutate(Trudeau.prime = 0.5) %>%
  #filter(Trudeau.prime == "No prime") %>%
  mutate(PRED = link) %>%
  mutate(UCL = link + (SE * (qnorm(0.975)))) %>%
  mutate(LCL = link - (SE * (qnorm(0.975)))) %>%
  select(pid, Trudeau.prime, PRED, UCL, LCL)

print(pred.m1.1.pid %>% select(pid, PRED, UCL, LCL), digits = 2)

plot.usa.pid.h1 <- ggplot(data = pred.m1.1.pid, aes(y = PRED, x = pid)) +
  geom_pointrange(aes(x = pid, y = PRED, ymin = LCL, ymax = UCL),
                  fatten = 1.2, size = 0.5) +
  geom_errorbar(aes(x = pid, y = PRED, ymin = LCL, ymax = UCL),
                width = 0.1, show.legend = FALSE) +
  labs(y = "Support/oppose Canada-US border closure",
       x = "Party identification") +
  coord_cartesian(ylim = c(-0.5, 0.6)) +
  scale_y_continuous(breaks = seq(-0.5, 0.5, by = 0.25),
                     labels = c("Strongly\noppose", "", "", "", "Strongly\nsupport")) +
  guides(colour = guide_legend(override.aes = list(fatten = 1, size = 0.5))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 25)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        text = element_text(size = 10, colour = "black"),
        legend.title = element_blank())

#ggsave(plot = plot.usa.pid.h1, file = "Effect plot-USA-PID-Main-2022 03 03.png",
#       device = "png",  width = 5, height = 3.25, units = "in", dpi = 1200)


## H2: Main effect of Trudeau prime (no covariates)
pred.m1.0.0 <- predict(m1.0.0, score.usa.prime, interval = "confidence") %>%
  as_tibble() %>%
  bind_cols(., score.usa.prime) %>%
  mutate(Trudeau.prime = as_factor(case_when(
    Trudeau.prime == 0 ~ "No prime",
    Trudeau.prime == 1 ~ "Trudeau prime"
  ))) %>%
  mutate(Trudeau.prime = fct_relevel(Trudeau.prime, "Trudeau prime", "No prime")) %>%
  mutate(PRED = link) %>%
  mutate(UCL = link + (SE * (qnorm(0.975)))) %>%
  mutate(LCL = link - (SE * (qnorm(0.975)))) %>%
  select(pid, Trudeau.prime, PRED, UCL, LCL)

print(pred.m1.0.0 %>% select(Trudeau.prime, PRED, UCL, LCL), digits = 2)


## H2: Main effect of Trudeau prime (with covariates); left panel of Figure 4
pred.m1.1.prime <- predict(m1.1, score.usa.prime, interval = "confidence") %>%
  as_tibble() %>%
  bind_cols(., score.usa.prime) %>%
  mutate(Trudeau.prime = as_factor(case_when(
    Trudeau.prime == 0 ~ "No prime",
    Trudeau.prime == 1 ~ "Trudeau prime"
  ))) %>%
  mutate(Trudeau.prime = fct_relevel(Trudeau.prime, "Trudeau prime", "No prime")) %>%
  mutate(PRED = link) %>%
  mutate(UCL = link + (SE * (qnorm(0.975)))) %>%
  mutate(LCL = link - (SE * (qnorm(0.975)))) %>%
  select(pid, Trudeau.prime, PRED, UCL, LCL)

print(pred.m1.1.prime %>% select(Trudeau.prime, PRED, UCL, LCL), digits = 2)

plot.usa.prime.h2 <- ggplot(data = pred.m1.1.prime, aes(y = PRED, x = Trudeau.prime)) +
  geom_pointrange(aes(x = Trudeau.prime, y = PRED, ymin = LCL, ymax = UCL),
                  fatten = 1.2, size = 0.5) +
  geom_errorbar(aes(x = Trudeau.prime, y = PRED, ymin = LCL, ymax = UCL),
                width = 0.1, show.legend = FALSE) +
  labs(y = "Support/oppose Canada-US border closure",
       x = "Prime") +
  coord_cartesian(ylim = c(-0.5, 0.6)) +
  scale_y_continuous(breaks = seq(-0.5, 0.5, by = 0.25),
                     labels = c("Strongly\noppose", "", "", "", "Strongly\nsupport")) +
  guides(colour = guide_legend(override.aes = list(fatten = 1, size = 0.5))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 25)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        text = element_text(size = 10, colour = "black"),
        legend.title = element_blank())

#ggsave(plot = plot.usa.prime.h2, file = "Effect plot-USA-Prime-2022 03 03.png",
#       device = "png",  width = 5, height = 3.25, units = "in", dpi = 1200)


## H3: Ideology * Trudeau prime interaction; Figure 5
pred.m1.2 <- predict(m1.2, score.usa.ideology, interval = "confidence") %>%
  as_tibble() %>%
  bind_cols(., score.usa.ideology) %>%
  mutate(Trudeau.prime = as_factor(case_when(
    Trudeau.prime == 0 ~ "No prime",
    Trudeau.prime == 1 ~ "Trudeau prime"
  ))) %>%
  mutate(ideology = ideology + (weighted.mean(usa.1$ideology, w = usa.1$weight))) %>%
  mutate(Trudeau.prime = fct_relevel(Trudeau.prime, "Trudeau prime", "No prime")) %>%
  mutate(PRED = link) %>%
  mutate(UCL = link + (SE * (qnorm(0.975)))) %>%
  mutate(LCL = link - (SE * (qnorm(0.975)))) %>%
  select(ideology, Trudeau.prime, PRED, UCL, LCL)

print(pred.m1.2 %>%
        filter(ideology %in% c(-0.5, 0, 0.5)) %>%
        mutate(PRED = round(PRED, 2)) %>%
        select(ideology, Trudeau.prime, PRED, UCL, LCL), digits = 3)

plot.usa.ideology.h3 <- ggplot(data = pred.m1.2, aes(y = PRED, x = ideology, group = Trudeau.prime)) +
  geom_line(aes(linetype = Trudeau.prime)) +
  geom_ribbon(aes(ymin = LCL, ymax = UCL), alpha = 0.1) +
  labs(y = "Support/oppose Canada-US border closure",
       x = "Ideology (liberal-conservative)") +
  coord_cartesian(ylim = c(-0.5, 0.6), xlim = c(-0.5, 0.5)) +
  scale_y_continuous(breaks = seq(-0.5, 0.5, by = 0.25),
                     labels = c("Strongly\noppose", "", "", "", "Strongly\nsupport")) +
  scale_x_continuous(breaks = seq(-0.5, 0.5, by = 0.25),
                     labels = c("Very liberal","","","","Very conservative")) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  #guides(colour = guide_legend(override.aes = list(fatten = 1, size = 0.5))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 25)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        text = element_text(size = 10, colour = "black"),
        legend.title = element_blank(),
        legend.position = c(0.2, 0.92),
        legend.text = element_text(size = 10, colour = "black"),
        plot.margin = margin(t = 0, b = 0, l = 0, r = 0.4, "in"))

ggsave(plot = plot.usa.ideology.h3, file = "Fig 5-Effect plot-USA-Ideology x Prime-2022 03 03.png",
       device = "png",  width = 5.35, height = 3.25, units = "in", dpi = 1200)


## H3: PID * Trudeau prime interaction (for appendix; Figure A1)
pred.m1.4 <- predict(m1.4, score.usa.pid, interval = "confidence") %>%
  as_tibble() %>%
  bind_cols(., score.usa.pid) %>%
  mutate(Trudeau.prime = as_factor(case_when(
    Trudeau.prime == 0 ~ "No prime",
    Trudeau.prime == 1 ~ "Trudeau prime"
  ))) %>%
  mutate(Trudeau.prime = fct_relevel(Trudeau.prime, "Trudeau prime", "No prime")) %>%
  mutate(PRED = link) %>%
  mutate(UCL = link + (SE * (qnorm(0.975)))) %>%
  mutate(LCL = link - (SE * (qnorm(0.975)))) %>%
  select(pid, Trudeau.prime, PRED, UCL, LCL)

print(pred.m1.4, digits = 2)

plot.usa.pid.h3 <- ggplot(data = pred.m1.4, aes(y = PRED, x = pid)) +
  geom_pointrange(aes(x = pid, y = PRED, ymin = LCL, ymax = UCL, colour = Trudeau.prime),
                  fatten = 1.2, size = 0.5, position = position_dodge(0.2)) +
  geom_errorbar(aes(x = pid, y = PRED, ymin = LCL, ymax = UCL, colour = Trudeau.prime),
                width = 0.1, position = position_dodge(0.2), show.legend = FALSE) +
  scale_color_manual(values = c("black", "grey60")) +
  labs(y = "Support/oppose Canada-US border closure",
       x = "Party identification") +
  coord_cartesian(ylim = c(-0.5, 0.6)) +
  scale_y_continuous(breaks = seq(-0.5, 0.5, by = 0.25),
                     labels = c("Strongly\noppose", "", "", "", "Strongly\nsupport")) +
  guides(colour = guide_legend(override.aes = list(fatten = 1, size = 0.5))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 25)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        text = element_text(size = 10, colour = "black"),
        legend.title = element_blank(),
        legend.position = c(0.2, 0.92),
        legend.text = element_text(size = 10, colour = "black"))

ggsave(plot = plot.usa.pid.h3, file = "Fig A1-Effect plot-USA-PID x Prime-2022 03 03.png",
       device = "png",  width = 5, height = 3.25, units = "in", dpi = 1200)


## US coefficient plot; Figure 1
coef.ci.m.1.1 <- m1.1 %>%
  broom::tidy(., conf.int = TRUE) %>%
  tidycat::tidy_categorical(m = m1.1, include_reference =  TRUE) %>%
  mutate(model.name = "Model 1.1") %>%
  mutate(term = case_when(
    reference == "Baseline Category" ~ str_c(variable, level),
    TRUE ~ term
  )) %>%
  rename(LCL = conf.low,
         UCL = conf.high) %>%
  select(model.name, term, reference, estimate, LCL, UCL)
coef.ci.m.1.2 <- m1.2 %>%
  broom::tidy(., conf.int = TRUE) %>%
  tidycat::tidy_categorical(m = m1.2, include_reference =  TRUE) %>%
  mutate(model.name = "Model 1.2") %>%
  mutate(term = case_when(
    reference == "Baseline Category" ~ str_c(variable, level),
    TRUE ~ term
  )) %>%
  rename(LCL = conf.low,
         UCL = conf.high) %>%
  select(model.name, term, reference, estimate, LCL, UCL)
coef.ci.m.1.3 <- m1.3 %>%
  broom::tidy(., conf.int = TRUE) %>%
  tidycat::tidy_categorical(m = m1.3, include_reference =  TRUE) %>%
  mutate(model.name = "Model 1.3") %>%
  mutate(term = case_when(
    reference == "Baseline Category" ~ str_c(variable, level),
    TRUE ~ term
  )) %>%
  rename(LCL = conf.low,
         UCL = conf.high) %>%
  select(model.name, term, reference, estimate, LCL, UCL)

coef.ci.usa <- bind_rows(coef.ci.m.1.1, coef.ci.m.1.2, coef.ci.m.1.3) %>%
  mutate(model.name = as_factor(model.name)) %>%
  mutate(model.name = fct_relevel(model.name, "Model 1.3", "Model 1.2", "Model 1.1")) %>%
  mutate(coef.label = as_factor(case_when(
    term == "(Intercept)" ~ "Intercept",
    term == "Trudeau.prime" ~ "Trudeau Prime",
    term == "ideology" ~ "Ideology (liberal-conservative)",
    term == "pidDemocrat" ~ "Party ID: Democrat (ref.)",
    term == "pidIndependent" ~ "Party ID: Independent",
    term == "pidRepublican" ~ "Party ID: Republican",
    term == "mii.immig" ~ "Most Important Issue: Immigration",
    term == "male" ~ "Sex: Male",
    term == "ln.age.years" ~ "Age (Years Logged)",
    term == "edu" ~ "Education",
    term == "race.ethnWhite" ~ "Race/Ethnicity: White (ref.)",
    term == "race.ethnBlack" ~ "Race/Ethnicity: Black",
    term == "race.ethnHispanic" ~ "Race/Ethnicity: Hispanic",
    term == "race.ethnAsian" ~ "Race/Ethnicity: Asian",
    term == "race.ethnOther" ~ "Race/Ethnicity: Other",
    term == "regionNortheast" ~ "Region: Northeast (ref.)",
    term == "regionMidwest" ~ "Region: Midwest",
    term == "regionSouth" ~ "Region: South",
    term == "regionWest" ~ "Region: West",
    term == "Trudeau.prime:ideology" ~ paste0("Trudeau Prime ", "\u00D7", " Ideology"),
    term == "Trudeau.prime:mii.immig" ~ paste0("Trudeau Prime ", "\u00D7", " MII: Immigration")
  ))) %>%
  mutate(coef.label = fct_relevel(coef.label, "Intercept", "Trudeau Prime",
                                  "Ideology (liberal-conservative)", "Party ID: Democrat (ref.)", "Party ID: Independent", "Party ID: Republican",
                                  "Most Important Issue: Immigration",
                                  "Trudeau Prime × Ideology",
                                  "Trudeau Prime × MII: Immigration",
                                  "Sex: Male", "Age (Years Logged)", "Education",
                                  "Race/Ethnicity: White (ref.)", "Race/Ethnicity: Black", "Race/Ethnicity: Hispanic",
                                  "Race/Ethnicity: Asian", "Race/Ethnicity: Other",
                                  "Region: Northeast (ref.)", "Region: Midwest", "Region: South", "Region: West")) %>%
  mutate(pt.shape = case_when(
    model.name == "Model 1.1" & reference == "Baseline Category" ~ 21,
    model.name == "Model 1.1" ~ 16,
    model.name == "Model 1.2" & reference == "Baseline Category" ~ 22,
    model.name == "Model 1.2" ~ 15,
    model.name == "Model 1.3" & reference == "Baseline Category" ~ 23,
    model.name == "Model 1.3" ~ 18)) %>%
  mutate(pt.fill = case_when(
    reference == "Baseline Category" ~ "white"
    #model.name == "Model 1.1" & reference == "Baseline Category" ~ "grey48",
    #model.name == "Model 1.2" & reference == "Baseline Category" ~ "grey32",
    #model.name == "Model 1.3" & reference == "Baseline Category" ~ "grey16"
  )) %>%
  mutate(pt.size = case_when(
    reference == "Baseline Category" ~ 2.5,
    model.name == "Model 1.1" ~ 3,
    model.name == "Model 1.2" ~ 2.5,
    model.name == "Model 1.3" ~ 3.5))

coef.plot.usa <- ggplot(data = coef.ci.usa, aes(x = estimate, y = coef.label, colour = model.name)) +
  geom_vline(xintercept = 0, colour = "grey72", size = 0.5, linetype = "longdash") +
  geom_pointrange(aes(x = estimate, y = coef.label, xmin = LCL, xmax = UCL, colour = model.name),
                  shape = coef.ci.usa$pt.shape, fatten = coef.ci.usa$pt.size,
                  fill = coef.ci.usa$pt.fill, stroke = 0.4, size = 0.5,
                  position = position_dodge(0.6)) +
  scale_color_manual(values = c("grey48", "grey32", "grey16")) +
  labs(x = "Estimate", y = "") +
  scale_y_discrete(limits = rev(levels(coef.ci.usa$coef.label))) +
  coord_cartesian(xlim = c(-0.3, 0.5)) +
  scale_x_continuous(breaks = seq(-0.2, 0.4, by = 0.2)) +
  guides(colour = guide_legend(override.aes = list(shape = c(16, 15, 18), fatten = c(1,1,4), size = 0.5), reverse = TRUE)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 10, colour = "black"), 
        text = element_text(size = 10, colour = "black"),
        legend.title = element_blank(),
        legend.position = c(0.86, 0.08),
        legend.text = element_text(size = 10, colour = "black"))

ggsave(plot = coef.plot.usa, file = "Fig 1-Coefficient plot-USA-2022 03 03.png",
       device = "png",  width = 6, height = 7, units = "in", dpi = 1200)


## Plot, MII * Trudeau prime; left panel of Figure 7
score.usa.immig <- full_seq(0:1, 1) %>% as_tibble() %>%
  rename(mii.immig = value) %>%
  crossing(full_seq(0:1, 1) %>% as_tibble()) %>%
  rename(Trudeau.prime = value) %>%
  mutate(pid = "Independent") %>%
  mutate(ideology = 0) %>%
  mutate(male = -0.5) %>%
  mutate(ln.age.years = 0) %>%
  mutate(edu = max(usa.2$edu)) %>%
  mutate(race.ethn = "White") %>%
  mutate(region = "Northeast") %>%
  arrange(Trudeau.prime)

pred.m1.3 <- predict(m1.3, score.usa.immig, interval = "confidence") %>%
  as_tibble() %>%
  bind_cols(., score.usa.immig) %>%
  mutate(mii.immig = as_factor(case_when(
    mii.immig == 0 ~ "Other issue",
    mii.immig == 1 ~ "Immigration"
  ))) %>%
  mutate(mii.immig = fct_relevel(mii.immig, "Immigration", "Other issue")) %>%
  mutate(Trudeau.prime = as_factor(case_when(
    Trudeau.prime == 0 ~ "No prime",
    Trudeau.prime == 1 ~ "Trudeau prime"
  ))) %>%
  mutate(Trudeau.prime = fct_relevel(Trudeau.prime, "Trudeau prime", "No prime")) %>%
  mutate(PRED = link) %>%
  mutate(UCL = link + (SE * (qnorm(0.975)))) %>%
  mutate(LCL = link - (SE * (qnorm(0.975)))) %>%
  select(mii.immig, Trudeau.prime, PRED, UCL, LCL)

print(pred.m1.3, digits = 2)

plot.usa.immig <- ggplot(data = pred.m1.3, aes(y = PRED, x = pid)) +
  geom_pointrange(aes(x = mii.immig, y = PRED, ymin = LCL, ymax = UCL, colour = Trudeau.prime),
                  fatten = 1.2, size = 0.5, position = position_dodge(0.2)) +
  geom_errorbar(aes(x = mii.immig, y = PRED, ymin = LCL, ymax = UCL, colour = Trudeau.prime),
                width = 0.1, position = position_dodge(0.2), show.legend = FALSE) +
  scale_color_manual(values = c("black", "grey60")) +
  labs(y = "Support/oppose Canada-US border closure",
       x = "Most important issue") +
  coord_cartesian(ylim = c(-0.5, 0.6)) +
  scale_y_continuous(breaks = seq(-0.5, 0.5, by = 0.25),
                     labels = c("Strongly\noppose", "", "", "", "Strongly\nsupport")) +
  guides(colour = guide_legend(override.aes = list(fatten = 1, size = 0.5))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 25)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        text = element_text(size = 10, colour = "black"),
        legend.title = element_blank(),
        legend.position = c(0.2, 0.92),
        legend.text = element_text(size = 10, colour = "black"))

#ggsave(plot = plot.usa.immig, file = "Effect plot-USA-Immig MII 2022 03 03.png",
#       device = "png",  width = 5, height = 3.25, units = "in", dpi = 1200)


## Figure A3
pred.m1.9 <- predict(m1.9, (distinct(score.usa.pid, pid, .keep_all = TRUE)), interval = "confidence") %>%
  as_tibble() %>%
  bind_cols(., (distinct(score.usa.pid, pid))) %>%
  mutate(ideology = link + weighted.mean(usa.1$ideology, w = usa.1$weight)) %>%
  mutate(UCL = ideology + (SE * (qnorm(0.975)))) %>%
  mutate(LCL = ideology - (SE * (qnorm(0.975)))) %>%
  select(pid, ideology, UCL, LCL)

plot.usa.ideology.pid <- ggplot(data = pred.m1.9, aes(y = ideology, x = pid)) +
  geom_pointrange(aes(x = pid, y = ideology, ymin = LCL, ymax = UCL), fatten = 1.2, size = 0.5) +
  geom_errorbar(aes(x = pid, y = ideology, ymin = LCL, ymax = UCL), width = 0.1) +
  labs(y = "Ideology (liberal-conservative)",
       x = "Party identification") +
  coord_cartesian(ylim = c(-0.5, 0.6)) +
  scale_y_continuous(breaks = seq(-0.5, 0.5, by = 0.25),
                     labels = c("Very\nliberal","","","","Very\nconservative")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 25)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        text = element_text(size = 10, colour = "black"),
        legend.title = element_blank(),
        legend.position = c(0.2, 0.92),
        legend.text = element_text(size = 10, colour = "black"))

ggsave(plot = plot.usa.ideology.pid, file = "Fig A3-Ideology by PID-USA 2022 03 03.png",
       device = "png",  width = 5, height = 2.75, units = "in", dpi = 1200)


## United States, wave 2
#usa.w2.0 <- read_sav("SVMK_For_Ldrs_USA_20210830.sav")
#replication.w2.usa <- usa.w2.0 %>%
#  select(resp_id:weight, STATE:ST, condition, q1, q2, q5, q10, q25, q12, q27, q13, q28,
#         q19, q34, q20, q35, q22, q37, q23, q38, ideology, party, lean, gender_non_binary:division)
#write_csv(replication.w2.usa, "SVMK_APSR_USA_w2_20210830.csv")
usa.w2.0 <- read_csv("SVMK_APSR_USA_w2_20210830.csv")

trudeau <- usa.w2.0 %>%
  filter(is.na(q5) == FALSE) %>%
  mutate(trudeau = str_to_lower(q5)) %>%
  group_by(trudeau) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  arrange(desc(n), trudeau)
#view(trudeau)

usa.w2.1 <- usa.w2.0 %>%
mutate(q5 = str_to_lower(q5)) %>%
  mutate(name.Trudeau = case_when(
    str_detect(q5, "justin") == TRUE ~ 1,
    str_detect(q5, "john") == TRUE ~ 0,
    str_detect(q5, "alec") == TRUE ~ 0,
    str_detect(q5, "alex") == TRUE ~ 0,
    str_detect(q5, "eric") == TRUE ~ 0,
    str_detect(q5, "trudeau") == TRUE & str_detect(q5, "pierre") == TRUE ~ 0,
    str_detect(q5, "trudeau") == TRUE & str_detect(q5, "alex") == TRUE ~ 0,
    str_detect(q5, "trudeau") == TRUE & str_detect(q5, "james") == TRUE ~ 0,
    str_detect(q5, "trudeau") == TRUE & str_detect(q5, "claude") == TRUE ~ 0,
    str_detect(q5, "trudeau") == TRUE ~ 1,
    str_detect(q5, "thear") == TRUE ~ 1,
    str_detect(q5, "throd") == TRUE ~ 1,
    str_detect(q5, "thure") == TRUE ~ 1,
    str_detect(q5, "tiro") == TRUE ~ 1,
    str_detect(q5, "toudr") == TRUE ~ 1,
    str_detect(q5, "trade") == TRUE ~ 1,
    str_detect(q5, "tradu") == TRUE ~ 1,
    str_detect(q5, "tread") == TRUE ~ 1,
    str_detect(q5, "tred") == TRUE ~ 1,
    str_detect(q5, "trod") == TRUE ~ 1,
    str_detect(q5, "troud") == TRUE ~ 1,
    str_detect(q5, "trousseau") == TRUE ~ 1,
    str_detect(q5, "trud") == TRUE ~ 1,
    str_detect(q5, "trued") == TRUE ~ 1,
    str_detect(q5, "ttude") == TRUE ~ 1,
    str_detect(q5, "tudre") == TRUE ~ 1,
    str_detect(q5, "turdeau") == TRUE ~ 1,
    str_detect(q5, "blackface") == TRUE ~ 1,
    str_detect(q5, "sophie grégoire trudeau's husband") == TRUE ~ 1,
    condition == 1 ~ 0
  )) %>%
  mutate(lr.self = case_when(
    condition == 1 ~ as.integer(q10),
    condition == 2 ~ as.integer(q25)
  )) %>%
  mutate(lr.Trump = case_when(
    condition == 1 ~ as.integer(q12),
    condition == 2 ~ as.integer(q27)
  )) %>%
  mutate(lr.PM.Can = as.integer(q13)) %>%
  mutate(lr.Trudeau = as.integer(q28)) %>%
  mutate(lr.PM.Can.Trudeau = case_when(
    condition == 1 ~ as.integer(q13),
    condition == 2 ~ as.integer(q28)
  )) %>%
  mutate(diff.lr.PM.Can.Trump = lr.PM.Can - lr.Trump) %>%
  mutate(diff.lr.Trudeau.Trump = lr.Trudeau - lr.Trump) %>%
  mutate(diff.lr.PM.Can.Trudeau.Trump = lr.PM.Can.Trudeau - lr.Trump) %>%
  mutate(therm.Trump = case_when(
    condition == 1 ~ as.integer(q19),
    condition == 2 ~ as.integer(q34)
  )) %>%
  mutate(therm.PM.Can = as.integer(q20)) %>%
  mutate(therm.Trudeau = as.integer(q35)) %>%
  mutate(therm.PM.Can.Trudeau = case_when(
    condition == 1 ~ as.integer(q20),
    condition == 2 ~ as.integer(q35)
  )) %>%
  mutate(therm.Pres.Fra = as.integer(q22)) %>%
  mutate(therm.Macron = as.integer(q37)) %>%
  mutate(therm.Pres.Fra.Macron = case_when(
    condition == 1 ~ as.integer(q22),
    condition == 2 ~ as.integer(q37)
  )) %>%
  mutate(therm.Chnc.Ger = as.integer(q23)) %>%
  mutate(therm.Merkel = as.integer(q38)) %>%
  mutate(therm.Chnc.Ger.Merkel = case_when(
    condition == 1 ~ as.integer(q23),
    condition == 2 ~ as.integer(q38)
  )) %>%
  mutate(ideology = case_when(
    ideology %in% 1:5 ~ 6 - ideology,
    TRUE ~ 3
  )) %>%
  mutate(party = case_when(
    party == 1 ~ 5,              # Republican
    party == 3 & lean == 1 ~ 4,  # Lean Republican
    party == 3 & lean == 3 ~ 3,  # Independent
    party == 3 & lean == 2 ~ 2,  # Lean Democrat
    party == 2 ~ 1,              # Democrat
    TRUE ~ 3
  )) %>%
  mutate(named = case_when(condition == 2 ~ 1, TRUE ~ 0)) %>%
  mutate(therm.PMJT = case_when(
    is.na(therm.PM.Can.Trudeau) == TRUE ~ as.integer(50),
    TRUE ~ as.integer(therm.PM.Can.Trudeau)
  )) %>%
  mutate(pid = as_factor(case_when(
    party %in% 1:2 ~ "Democrat",
    party %in% 4:5 ~ "Republican",
    party == 3 ~ "Independent",
    TRUE ~ "Independent"
  ))) %>%
  mutate(pid = fct_relevel(pid, "Democrat", "Independent", "Republican")) %>%
  mutate(male = case_when(
    gender_non_binary == 1 ~ 0.5,
    gender_non_binary == 2 ~ -0.5,
    gender_non_binary == 3 ~ 0
  )) %>%
  mutate(age = as.integer(age)) %>%
  mutate(ln.age.years = log(age) / (max(log(age)) - min(log(age)))) %>%
  mutate(edu = case_when(
    education == 1 ~ 0,
    education == 2 ~ 1/3,
    education %in% 3:4 ~ 2/3,
    education == 5 ~ 1,
    education == 6 ~ 1,
  )) %>%
  mutate(race.ethn = as_factor(case_when(
    race_ethn == 1 ~ "White",
    race_ethn == 2 ~ "Black",
    race_ethn == 3 ~ "Hispanic",
    race_ethn == 4 ~ "Asian",
    race_ethn == 5 ~ "Other"
  ))) %>%
  mutate(race.ethn = fct_relevel(race.ethn, "White", "Black", "Hispanic", "Asian", "Other")) %>%
  mutate(region = as_factor(case_when(
    region == 1 ~ "Northeast",
    region == 2 ~ "Midwest",
    region == 3 ~ "South",
    region == 4 ~ "West"
  ))) %>%
  mutate(region = fct_relevel(region, "Northeast", "Midwest", "South", "West"))

usa.w2.2 <- usa.w2.1 %>%
  mutate(edu = edu - weighted.mean(edu, w = weight)) %>%
  mutate(ln.age.years = ln.age.years - weighted.mean(ln.age.years, w = weight)) %>%
  mutate(ideology = ideology - weighted.mean(ideology, w = weight))

table(usa.w2.2$condition)

xtabs(~ ST, addNA = TRUE, na.action = NULL, data = usa.w2.1) %>% prop.table()*100
xtabs(~ name.Trudeau, addNA = FALSE, na.action = NULL, data = usa.w2.1) %>% prop.table()*100
summary(usa.w2.2$lr.PM.Can)
summary(usa.w2.2$lr.Trudeau)
summary(usa.w2.2$lr.PM.Can.Trudeau)
summary(usa.w2.2$therm.PM.Can.Trudeau)

xtabs(weight ~ name.Trudeau, addNA = FALSE, na.action = NULL, data = usa.w2.1, subset = condition == 1) %>% prop.table()*100

design.usa.w2 <- svydesign(data = usa.w2.2, id = ~1, weights = ~weight, strata = ~ST)
options(survey.lonely.psu="adjust")


## Means tests for Table A9
## Left-right ideology: PM of Canada vs. PM of Canada, Justin Trudeau
svymean(~lr.PM.Can, design.usa.w2, na.rm = TRUE)
usa.w2.2 %>% select(lr.PM.Can) %>% drop_na() %>% summarise(n = n())

svymean(~lr.Trudeau, design.usa.w2, na.rm = TRUE)
usa.w2.2 %>% select(lr.Trudeau) %>% drop_na() %>% summarise(n = n())

Trudeau.lr <- svyttest(lr.PM.Can.Trudeau ~ condition, design.usa.w2)
Trudeau.lr
Trudeau.lr$estimate/Trudeau.lr$statistic  ## SE
#Trudeau.lr$conf.int %>%
#  as_tibble() %>%
#  rename(LCL = 1, UCL = 2) %>%
#  mutate(CI = UCL - Trudeau.lr$estimate) %>%
#  mutate(SE = CI / qnorm(0.975)) %>%
#  select(SE)
#svyttest(lr.PM.Can ~ name.Trudeau, design.usa.w2)

## Left-right ideology: PM of Canada vs. Former President Donald Trump
svymean(I(lr.Trump - lr.PM.Can) ~ 0, svydesign(data = (usa.w2.2 %>% drop_na("lr.PM.Can", "lr.Trump")),
                                               id = ~1, weights = ~weight, strata = ~ST), na.rm = TRUE)
svymean(~lr.PM.Can, svydesign(data = (usa.w2.2 %>% drop_na("lr.PM.Can", "lr.Trump")),
                              id = ~1, weights = ~weight, strata = ~ST), na.rm = TRUE)
svymean(~lr.Trump, svydesign(data = (usa.w2.2 %>% drop_na("lr.PM.Can", "lr.Trump")),
                             id = ~1, weights = ~weight, strata = ~ST), na.rm = TRUE)
usa.w2.2 %>% select(lr.PM.Can, lr.Trump) %>% drop_na() %>% summarise(n = n())

svyttest(I(lr.Trump - lr.PM.Can) ~ 0, svydesign(data = (usa.w2.2 %>% drop_na("lr.PM.Can", "lr.Trump")),
                                                id = ~1, weights = ~weight, strata = ~ST), na.rm = TRUE)
svymean(I(lr.Trump - lr.PM.Can) ~ 0, svydesign(data = (usa.w2.2 %>% drop_na("lr.PM.Can", "lr.Trump")),
                                               id = ~1, weights = ~weight, strata = ~ST), na.rm = TRUE)

## Left-right ideology: PM of Canada, Justin Trudeau vs. Former President Donald Trump
svymean(~lr.Trudeau, svydesign(data = (usa.w2.2 %>% drop_na("lr.Trudeau", "lr.Trump")),
                               id = ~1, weights = ~weight, strata = ~ST), na.rm = TRUE)
svymean(~lr.Trump, svydesign(data = (usa.w2.2 %>% drop_na("lr.Trudeau", "lr.Trump")),
                             id = ~1, weights = ~weight, strata = ~ST), na.rm = TRUE)
usa.w2.2 %>% select(lr.Trudeau, lr.Trump) %>% drop_na() %>% summarise(n = n())
svyttest(I(lr.Trump - lr.Trudeau) ~ 0, svydesign(data = (usa.w2.2 %>% drop_na("lr.Trudeau", "lr.Trump")),
                                                 id = ~1, weights = ~weight, strata = ~ST), na.rm = TRUE)
svymean(I(lr.Trump - lr.Trudeau) ~ 0, svydesign(data = (usa.w2.2 %>% drop_na("lr.Trudeau", "lr.Trump")),
                                                id = ~1, weights = ~weight, strata = ~ST), na.rm = TRUE)

## Left-right ideology: PM of Canada/PM of Canada, Justin Trudeau (combined) vs. Former President Donald Trump
svymean(~lr.PM.Can.Trudeau, svydesign(data = (usa.w2.2 %>% drop_na("lr.PM.Can.Trudeau", "lr.Trump")),
                                      id = ~1, weights = ~weight, strata = ~ST), na.rm = TRUE)
svymean(~lr.Trump, svydesign(data = (usa.w2.2 %>% drop_na("lr.PM.Can.Trudeau", "lr.Trump")),
                             id = ~1, weights = ~weight, strata = ~ST), na.rm = TRUE)
usa.w2.2 %>% select(lr.PM.Can.Trudeau, lr.Trump) %>% drop_na() %>% summarise(n = n())
svyttest(I(lr.Trump - lr.PM.Can.Trudeau) ~ 0, svydesign(data = (usa.w2.2 %>% drop_na("lr.PM.Can.Trudeau", "lr.Trump")),
                                                        id = ~1, weights = ~weight, strata = ~ST), na.rm = TRUE)
svymean(I(lr.Trump - lr.PM.Can.Trudeau) ~ 0, svydesign(data = (usa.w2.2 %>% drop_na("lr.PM.Can.Trudeau", "lr.Trump")),
                                                       id = ~1, weights = ~weight, strata = ~ST), na.rm = TRUE)


## Means tests for Table A10
## Thermometer: PM of Canada vs. PM of Canada, Justin Trudeau
svymean(~therm.PM.Can, design.usa.w2, na.rm = TRUE)
usa.w2.2 %>% select(therm.PM.Can) %>% drop_na() %>% summarise(n = n())
svymean(~therm.Trudeau, design.usa.w2, na.rm = TRUE)
usa.w2.2 %>% select(therm.Trudeau) %>% drop_na() %>% summarise(n = n())
Trudeau.therm <- svyttest(therm.PM.Can.Trudeau ~ condition, design.usa.w2)
Trudeau.therm
Trudeau.therm$estimate/Trudeau.therm$statistic  ## SE

## Thermometer: PM of Canada vs. Former President Donald Trump
svymean(~therm.PM.Can, svydesign(data = (usa.w2.2 %>% drop_na("therm.PM.Can", "therm.Trump")),
                                 id = ~1, weights = ~weight, strata = ~ST), na.rm = TRUE)
svymean(~therm.Trump, svydesign(data = (usa.w2.2 %>% drop_na("therm.PM.Can", "therm.Trump")),
                                id = ~1, weights = ~weight, strata = ~ST), na.rm = TRUE)
usa.w2.2 %>% select(therm.PM.Can, therm.Trump) %>% drop_na() %>% summarise(n = n())
svyttest(I(therm.Trump - therm.PM.Can) ~ 0, svydesign(data = (usa.w2.2 %>% drop_na("therm.PM.Can", "therm.Trump")),
                                                      id = ~1, weights = ~weight, strata = ~ST), na.rm = TRUE)
svymean(I(therm.Trump - therm.PM.Can) ~ 0, svydesign(data = (usa.w2.2 %>% drop_na("therm.PM.Can", "therm.Trump")),
                                                     id = ~1, weights = ~weight, strata = ~ST), na.rm = TRUE)

## Thermometer: PM of Canada, Justin Trudeau vs. Former President Donald Trump
svymean(~therm.Trudeau, svydesign(data = (usa.w2.2 %>% drop_na("therm.Trudeau", "therm.Trump")),
                                 id = ~1, weights = ~weight, strata = ~ST), na.rm = TRUE)
svymean(~therm.Trump, svydesign(data = (usa.w2.2 %>% drop_na("therm.Trudeau", "therm.Trump")),
                                id = ~1, weights = ~weight, strata = ~ST), na.rm = TRUE)
usa.w2.2 %>% select(therm.Trudeau, therm.Trump) %>% drop_na() %>% summarise(n = n())
svyttest(I(therm.Trump - therm.Trudeau) ~ 0, svydesign(data = (usa.w2.2 %>% drop_na("therm.Trudeau", "therm.Trump")),
                                                       id = ~1, weights = ~weight, strata = ~ST), na.rm = TRUE)
svymean(I(therm.Trump - therm.Trudeau) ~ 0, svydesign(data = (usa.w2.2 %>% drop_na("therm.Trudeau", "therm.Trump")),
                                                      id = ~1, weights = ~weight, strata = ~ST), na.rm = TRUE)

## Thermometer: PM of Canada/PM of Canada, Justin Trudeau (combined) vs. Former President Donald Trump
svymean(~therm.PM.Can.Trudeau, svydesign(data = (usa.w2.2 %>% drop_na("therm.PM.Can.Trudeau", "therm.Trump")),
                                         id = ~1, weights = ~weight, strata = ~ST), na.rm = TRUE)
svymean(~therm.Trump, svydesign(data = (usa.w2.2 %>% drop_na("therm.PM.Can.Trudeau", "therm.Trump")),
                                id = ~1, weights = ~weight, strata = ~ST), na.rm = TRUE)
usa.w2.2 %>% select(therm.PM.Can.Trudeau, therm.Trump) %>% drop_na() %>% summarise(n = n())
svyttest(I(therm.Trump - therm.PM.Can.Trudeau) ~ 0, svydesign(data = (usa.w2.2 %>% drop_na("therm.PM.Can.Trudeau", "therm.Trump")),
                                                              id = ~1, weights = ~weight, strata = ~ST), na.rm = TRUE)
svymean(I(therm.Trump - therm.PM.Can.Trudeau) ~ 0, svydesign(data = (usa.w2.2 %>% drop_na("therm.PM.Can.Trudeau", "therm.Trump")),
                                                             id = ~1, weights = ~weight, strata = ~ST), na.rm = TRUE)

## PM of Canada/Trudeau thermometer regression
m7 <- svyglm(therm.PMJT ~ named + pid + ideology + male + ln.age.years + edu + race.ethn + region,
               design = design.usa.w2, family = stats::gaussian())
summ(m7)


## Canada
#can.0 <- read_sav("SVMK_NatID_Immig_CAN_20201125.sav")
#replication.can <- can.0 %>%
#  select(resp_id:weight, language, PR, condition:border_trt, q2, issue_matters_most, q8:q16, q17:q19, q22, q24, q55, age6, age)
#write_csv(replication.can, "SVMK_APSR_CAN_20210826.csv")
can.0 <- read_csv("SVMK_APSR_CAN_20210826.csv")

can.1 <- can.0 %>%
  mutate(mii.immig = case_when(
    issue_matters_most == 5 ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(approve.border = case_when(
    q9 == 1 ~ 0.5,
    q9 == 2 ~ 0.25,
    q9 %in% c(3,6) ~ 0,
    q9 == 4 ~ -0.25,
    q9 == 5 ~ -0.5
  )) %>%
  mutate(Trump.prime = border_trt) %>%
  rename(nat.id.prime = nat_id_trt) %>%
  #1 = Left, 5 = Right >> 0 = Left, 1 = Right
  mutate(ideology = case_when(
    q16 == 1 ~ -0.5,
    q16 == 2 ~ -0.25,
    q16 %in% c(3,6) ~ 0,
    q16 == 4 ~ 0.25,
    q16 == 5 ~ 0.5
  )) %>%
  mutate(pid = as_factor(case_when(
    q15 == 5 ~ "Liberal",
    q15 == 3 ~ "Conservative",
    q15 == 6 ~ "NDP",
    q15 == 4 ~ "Green",
    PR == 24 & q15 == 1 ~ "BQ",
    q15 %in% c(2,7,8) | (PR != 24 & q15 == 1) ~ "Other/no party"
  ))) %>%
  mutate(pid = fct_relevel(pid, "Liberal", "NDP", "Green", "BQ", "Conservative", "Other/no party")) %>%
  mutate(male = case_when(
    q17 == 1 ~ 0.5,
    q17 == 2 ~ -0.5,
    q17 == 3 ~ 0
  )) %>%
  mutate(age = as.integer(age)) %>%
  mutate(ln.age.years = log(age) / (max(log(age)) - min(log(age)))) %>%
  mutate(edu = case_when(
    q19 == 1 ~ -0.5,
    q19 == 2 ~ 1/3 - 0.5,
    q19 == 3 ~ 2/3 - 0.5,
    q19 == 4 ~ 0.5,
  )) %>%
  mutate(race.ethn = as_factor(case_when(
    q18 == 3 ~ "White",
    q18 == 1 ~ "Indigenous",
    q18 == 6 ~ "Black",
    q18 %in% c(4,5,7,10,12,13) ~ "Asian",
    q18 %in% c(2,8,9,11) ~ "Other"
  ))) %>%
  mutate(race.ethn = fct_relevel(race.ethn, "White", "Indigenous", "Black", "Asian", "Other")) %>%
  mutate(region = as_factor(case_when(
    q55 == 4 ~ "Ontario",
    q55 == 3 ~ "Atlantic",
    q55 == 5 ~ "Quebec",
    q55 == 1 ~ "Prairies",
    q55 == 2 ~ "BC",
  ))) %>%
  mutate(region = fct_relevel(region, "Ontario", "Atlantic", "Quebec", "Prairies", "BC"))

xtabs(~ pid + region, addNA = TRUE, na.action = NULL, data = can.1)
  
can.2 <- can.1 %>%
  mutate(edu = edu - weighted.mean(edu, w = weight)) %>%
  mutate(ln.age.years = ln.age.years - weighted.mean(ln.age.years, w = weight)) %>%
  mutate(ideology = ideology - weighted.mean(ideology, w = weight))
  
table(can.2$approve.border)
table(can.2$Trump.prime)
table(can.2$nat.id.prime)
table(can.2$mii.immig)
table(can.2$ideology)
table(can.2$pid)
table(can.2$male)
table(can.2$edu)
table(can.2$race.ethn)
table(can.2$region)
summary(can.2$edu)
summary(can.2$ln.age.years)

weighted.mean(can.2$ideology, w = can.2$weight)
weighted.mean(can.2$edu, w = can.2$weight)


design.can <- svydesign(data = can.2, id = ~1, weights = ~weight, strata = ~PR)

svyttest(ideology ~ Trump.prime, design.can)

## Models for Table A2
m2.0.0 <- svyglm(approve.border ~ Trump.prime,
                 design = design.can, family = stats::gaussian())
m2.0.1 <- svyglm(approve.border ~ Trump.prime + ideology +  pid + male + ln.age.years + edu + race.ethn + region,
                 design = design.can, family = stats::gaussian())
m2.0.2 <- svyglm(approve.border ~ Trump.prime +  mii.immig + male + ln.age.years + edu + race.ethn + region,
                 design = design.can, family = stats::gaussian())
m2.1 <- svyglm(approve.border ~ Trump.prime + ideology +  pid + mii.immig + male + ln.age.years + edu + race.ethn + region,
               design = design.can, family = stats::gaussian())
m2.2 <- svyglm(approve.border ~ Trump.prime + ideology +  pid + mii.immig + male + ln.age.years + edu + race.ethn + region
               + ideology*Trump.prime,
               design = design.can, family = stats::gaussian())
m2.3 <- svyglm(approve.border ~ Trump.prime + ideology +  pid + mii.immig + male + ln.age.years + edu + race.ethn + region
               + mii.immig*Trump.prime,
               design = design.can, family = stats::gaussian())
m2.4 <- svyglm(approve.border ~ Trump.prime + ideology +  pid + mii.immig + male + ln.age.years + edu + race.ethn + region
               + pid*Trump.prime,
               design = design.can, family = stats::gaussian())
m2.5 <- svyglm(approve.border ~ Trump.prime + ideology +  pid + mii.immig + male + ln.age.years + edu + race.ethn + region
               + ideology*Trump.prime + mii.immig*Trump.prime,
               design = design.can, family = stats::gaussian())

summ(m2.0.0, digits = 3)
summ(m2.0.1, digits = 3)
summ(m2.0.2, digits = 3)
summ(m2.1, digits = 3)
summ(m2.2, digits = 3)
anova(m2.1, m2.2, test = "F")
summ(m2.3, digits = 3)
anova(m2.1, m2.3, test = "F")
summ(m2.4, digits = 3)
anova(m2.1, m2.4, test = "F")
summ(m2.5, digits = 3)
anova(m2.2, m2.5, test = "F")
anova(m2.3, m2.5, test = "F")


## Canada: ideology*party ID; Figure A5
m2.9 <- svyglm(ideology ~ pid + mii.immig + male + ln.age.years + edu + race.ethn + region,
               design = design.can, family = stats::gaussian())
summ(m2.9, digits = 3)


## Canada: check for interaction between treatments (for Table A6)
mA6 <- svyglm(approve.border ~ Trump.prime + nat.id.prime + Trump.prime*nat.id.prime,
              design = design.can, family = stats::gaussian())
summ(mA6, digits = 3)


## Canada: MII-immigration (for Table A8)
mA8 <- svyglm(mii.immig ~ ideology + pid + male + ln.age.years + edu + race.ethn + region,
               design = design.can, family = stats::binomial(link = "logit"))
summ(mA8, digits = 3)

svycor(~mii.immig + ideology, design = design.can, digits = 3, sig.stats = TRUE)
xtabs(weight ~ mii.immig + ideology, addNA = TRUE, na.action = NULL, data = can.1) %>% prop.table(margin = 2)*100

m_A8_out <- broom::tidy(mA8) %>%
  mutate(sig = case_when(p.value <= 0.001 ~ "***",
                           p.value <= 0.01 ~ "**",
                           p.value <= 0.05 ~ "*",
                           p.value <= 0.1 ~ "+")) %>%
  bind_rows(., (as_tibble(broom::glance(mA8)) %>%
                  mutate(n = df.null + 1) %>%
                  mutate(Chi.square = deviance - null.deviance) %>%
                  mutate(L_0 = exp(null.deviance / -2)) %>%
                  mutate(L_M = exp(deviance/ -2)) %>%
                  mutate(pwr = 2/n) %>%
                  mutate(Nagelkerke.R2 = (1 - (L_0 / L_M)^pwr) / (1 - (L_0^pwr))) %>%
                  select(Chi.square, Nagelkerke.R2, n) %>%
                  pivot_longer(cols = everything(), names_to = "term", values_to = "estimate"))) %>%
  mutate(model = "A8") %>%
  mutate(estimate = round(estimate, digits = 2)) %>%
  mutate(std.error = str_c("(", as.character(round(std.error, digits = 2)), ")")) %>%
  select(model, everything()) 


## Canada: Model summaries; save out results to Excel
stargazer(m2.0.0, m2.0.1, m2.0.2, m2.1, m2.2, m2.3, m2.4, m2.5,
          type = "text",
          column.labels = c("M2.0.0", "M2.0.1", "M2.0.2", "M2.1", "M2.2", "M2.3", "M2.4", "M2.5"),
          single.row = TRUE,  model.numbers = FALSE, intercept.top = TRUE, intercept.bottom = FALSE,
          report = "vcs*", digits = 2, keep.stat = c("rsq", "adj.rsq"),
          star.cutoffs = c(0.05, 0.01, 0.001), star.char = c("*", "**", "***"))

svy.model.out(model_obj = m2.0.0, model_name = "CA-m2.0.0")
svy.model.out(model_obj = m2.0.1, model_name = "CA-m2.0.1")
svy.model.out(model_obj = m2.0.2, model_name = "CA-m2.0.2")
svy.model.out(model_obj = m2.1, model_name = "CA-m2.1")
svy.model.out(model_obj = m2.2, model_name = "CA-m2.2")
svy.model.out(model_obj = m2.3, model_name = "CA-m2.3")
svy.model.out(model_obj = m2.4, model_name = "CA-m2.4")
svy.model.out(model_obj = m2.5, model_name = "CA-m2.5")
svy.model.out(model_obj = m2.9, model_name = "CA-m2.9")
svy.model.out(model_obj = m2.9, model_name = "CA-A6")
openxlsx::addWorksheet(work_book, sheetName = "CA-A8")
openxlsx::writeData(work_book, "CA-A8", m_A8_out)

saveWorkbook(work_book, "CAN-US border priming experiment 2022 03 03.xlsx", overwrite = TRUE)


## Canada: predicted values
score.can.ideology <- full_seq(-0.5:0.5, 0.01) %>% as_tibble() %>%
  mutate(ideology = value - (weighted.mean(can.1$ideology, w = can.1$weight))) %>%
  select(-value) %>%
  crossing(full_seq(0:1, 1) %>% as_tibble()) %>%
  rename(Trump.prime = value) %>%
  mutate(pid = as_factor("Liberal")) %>%
  mutate(mii.immig = 0) %>%
  mutate(male = -0.5) %>%
  mutate(ln.age.years = 0) %>%
  mutate(edu = max(can.2$edu)) %>%
  mutate(race.ethn = "White") %>%
  mutate(region = "Ontario") %>%
  arrange(Trump.prime)

score.can.pid <- c("NDP", "Green", "Liberal", "BQ", "Conservative") %>% as_tibble() %>%
  mutate(pid = as_factor(value)) %>%
  select(-value) %>%
  crossing(full_seq(0:1, 1) %>% as_tibble()) %>%
  rename(Trump.prime = value) %>%
  mutate(ideology = 0) %>%
  mutate(mii.immig = 0) %>%
  mutate(male = -0.5) %>%
  mutate(ln.age.years = 0) %>%
  mutate(edu = max(can.2$edu)) %>%
  mutate(race.ethn = "White") %>%
  mutate(region = "Ontario") %>%
  arrange(Trump.prime)

score.can.prime <- full_seq(0:1, 1) %>% as_tibble() %>%
  rename(Trump.prime = value) %>%
  mutate(ideology = 0) %>%
  mutate(pid = as_factor("Liberal")) %>%
  mutate(mii.immig = 0) %>%
  mutate(male = -0.5) %>%
  mutate(ln.age.years = 0) %>%
  mutate(edu = max(can.2$edu)) %>%
  mutate(race.ethn = "White") %>%
  mutate(region = "Ontario") %>%
  arrange(Trump.prime)

## H1: main effect of ideology; right panel of Figure 3
pred.m2.1.ideo <- predict(m2.1, (score.can.ideology %>% filter(Trump.prime == 0) %>%
                                   mutate(Trump.prime = 0.5)), interval = "confidence") %>%
  as_tibble() %>%
  bind_cols(., (score.can.ideology %>% filter(Trump.prime == 0))) %>%
  #mutate(Trump.prime = as_factor(case_when(
  #  Trump.prime == 0 ~ "No prime",
  #  Trump.prime == 1 ~ "Trump prime"
  #))) %>%
  mutate(ideology = ideology + (weighted.mean(can.1$ideology, w = can.1$weight))) %>%
  #mutate(Trump.prime = fct_relevel(Trump.prime, "Trump prime", "No prime")) %>%
  mutate(Trump.prime = 0.5) %>%
  #filter(ideology %in% c(0, 0.5, 1) & Trump.prime == "No prime") %>%
  filter(ideology %in% c(-0.5, 0, 0.5)) %>%
  mutate(PRED = link) %>%
  mutate(UCL = link + (SE * (qnorm(0.975)))) %>%
  mutate(LCL = link - (SE * (qnorm(0.975)))) %>%
  select(ideology, Trump.prime, PRED, UCL, LCL)

print(pred.m2.1.ideo %>% select(ideology, PRED, UCL, LCL), digits = 2)

plot.can.ideology.h1 <- ggplot(data = pred.m2.1.ideo, aes(y = PRED, x = ideology)) +
  geom_line() +
  geom_ribbon(aes(ymin = LCL, ymax = UCL), alpha = 0.1) +
  labs(y = "Support/oppose Canada-US border closure",
       x = "Ideology (left-right)") +
  coord_cartesian(ylim = c(-0.5, 0.6), xlim = c(-0.5, 0.5)) +
  scale_y_continuous(breaks = seq(-0.5, 0.5, by = 0.25),
                     labels = c("Strongly\noppose", "", "", "", "Strongly\nsupport")) +
  scale_x_continuous(breaks = seq(-0.5, 0.5, by = 0.25),
                     labels = c("Left","","","","Right")) +
  scale_linetype_manual(values = c("solid")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 25)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        text = element_text(size = 10, colour = "black"),
        plot.margin = margin(t = 0, b = 0, l = 0, r = 0.5, "in")) +
  ggplot2::annotate("text", x = -0.475, y = -0.475, label = "Canada", size = 4, fontface = "bold",
           hjust = 0, vjust = 0)
  
#ggsave(plot = plot.can.ideology.h1, file = "Effect plot-CAN-Ideology-main 2022 03 03.png",
#       device = "png",  width = 5.35, height = 3.25, units = "in", dpi = 1200)

plot.usa.can.ideology.h1 <- plot_grid(plot.usa.ideology.h1, plot.can.ideology.h1)

ggsave(plot = plot.usa.can.ideology.h1, file = "Fig 3-Effect plot-USA & CAN-Ideology-main 2022 03 03.png",
       device = "png",  width = 9, height = 3.25, units = "in", dpi = 1200)


## H1: main effect of party ID (for appendix)
pred.m2.1.pid <- predict(m2.1, (score.can.pid %>% filter(Trump.prime == 0) %>%
                                   mutate(Trump.prime = 0.5)), interval = "confidence") %>%
  as_tibble() %>%
  bind_cols(., (score.can.pid %>% filter(Trump.prime == 0))) %>%
  #mutate(Trump.prime = as_factor(case_when(
  #  Trump.prime == 0 ~ "No prime",
  #  Trump.prime == 1 ~ "Trump prime"
  #))) %>%
  #mutate(Trump.prime = fct_relevel(Trump.prime, "Trump prime", "No prime")) %>%
  mutate(Trump.prime = 0.5) %>%
  #filter(Trump.prime == "No prime") %>%
  mutate(PRED = link) %>%
  mutate(UCL = link + (SE * (qnorm(0.975)))) %>%
  mutate(LCL = link - (SE * (qnorm(0.975)))) %>%
  select(pid, PRED, UCL, LCL)

print(pred.m2.1.pid)

plot.can.pid.h1 <- ggplot(data = pred.m2.1.pid, aes(pid, PRED)) +
  geom_pointrange(aes(x = pid, y = PRED, ymin = LCL, ymax = UCL),
                  fatten = 1.2, size = 0.5) +
  geom_errorbar(aes(x = pid, y = PRED, ymin = LCL, ymax = UCL),
                width = 0.1, show.legend = FALSE) +
  scale_color_manual(values = c("black")) +
  labs(y = "Support/oppose Canada-US border closure",
       x = "Party identification") +
  coord_cartesian(ylim = c(-0.5, 0.6)) +
  scale_y_continuous(breaks = seq(-0.5, 0.5, by = 0.25),
                     labels = c("Strongly\noppose", "", "", "", "Strongly\nsupport")) +
  guides(colour = guide_legend(override.aes = list(fatten = 1, size = 0.5))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 25)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        text = element_text(size = 10, colour = "black"),
        legend.title = element_blank(),
        legend.position = c(0.2, 0.92),
        legend.text = element_text(size = 10, colour = "black"))

#ggsave(plot = plot.can.pid.h1, file = "Effect plot-CAN-PID-Main-2022 03 03.png",
#       device = "png",  width = 5, height = 3.25, units = "in", dpi = 1200)


## H2: Main effect of Trump prime (no covariates)
pred.m2.0.0 <- predict(m2.0.0, score.can.prime, interval = "confidence") %>%
  as_tibble() %>%
  bind_cols(., score.can.prime) %>%
  mutate(Trump.prime = as_factor(case_when(
    Trump.prime == 0 ~ "No prime",
    Trump.prime == 1 ~ "Trump prime"
  ))) %>%
  mutate(Trump.prime = fct_relevel(Trump.prime, "Trump prime", "No prime")) %>%
  mutate(PRED = link) %>%
  mutate(UCL = link + (SE * (qnorm(0.975)))) %>%
  mutate(LCL = link - (SE * (qnorm(0.975)))) %>%
  select(pid, Trump.prime, PRED, UCL, LCL)

print(pred.m2.0.0 %>% select(Trump.prime, PRED, UCL, LCL), digits = 2)


## H2: Main effect of Trump prime (with covariates); right panel of Figure 4
pred.m2.1.prime <- predict(m2.1, score.can.prime, interval = "confidence") %>%
  as_tibble() %>%
  bind_cols(., score.can.prime) %>%
  mutate(Trump.prime = as_factor(case_when(
    Trump.prime == 0 ~ "No prime",
    Trump.prime == 1 ~ "Trump prime"
  ))) %>%
  mutate(Trump.prime = fct_relevel(Trump.prime, "Trump prime", "No prime")) %>%
  mutate(PRED = link) %>%
  mutate(UCL = link + (SE * (qnorm(0.975)))) %>%
  mutate(LCL = link - (SE * (qnorm(0.975)))) %>%
  select(pid, Trump.prime, PRED, UCL, LCL)

print(pred.m2.1.prime %>% select(Trump.prime, PRED, UCL, LCL), digits = 2)

plot.can.prime.h2 <- ggplot(data = pred.m2.1.prime, aes(y = PRED, x = Trump.prime)) +
  geom_pointrange(aes(x = Trump.prime, y = PRED, ymin = LCL, ymax = UCL),
                  fatten = 1.2, size = 0.5) +
  geom_errorbar(aes(x = Trump.prime, y = PRED, ymin = LCL, ymax = UCL),
                width = 0.1, show.legend = FALSE) +
  labs(y = "Support/oppose Canada-US border closure",
       x = "Prime") +
  coord_cartesian(ylim = c(-0.5, 0.6)) +
  scale_y_continuous(breaks = seq(-0.5, 0.5, by = 0.25),
                     labels = c("Strongly\noppose", "", "", "", "Strongly\nsupport")) +
  guides(colour = guide_legend(override.aes = list(fatten = 1, size = 0.5))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 25)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        text = element_text(size = 10, colour = "black"),
        legend.title = element_blank(),
        legend.position = c(0.2, 0.92),
        legend.text = element_text(size = 10, colour = "black"))

#ggsave(plot = plot.can.prime.h2, file = "Effect plot-CAN-Prime-2022 03 03.png",
#       device = "png",  width = 5, height = 3.25, units = "in", dpi = 1200)


plot.usa.prime.h2 <- plot.usa.prime.h2 +
  ggplot2::annotate("text", x = 0.5, y = -0.475, label = "United States", size = 4, fontface = "bold",
                    hjust = 0, vjust = 0)

plot.can.prime.h2 <- plot.can.prime.h2 +
  ggplot2::annotate("text", x = 0.5, y = -0.475, label = "Canada", size = 4, fontface = "bold",
                    hjust = 0, vjust = 0)

plot.usa.can.prime.h2 <- plot_grid(plot.usa.prime.h2, plot.can.prime.h2)

ggsave(plot = plot.usa.can.prime.h2, file = "Fig 4-Effect plot-USA & CAN-Prime 2022 03 03.png",
       device = "png",  width = 9, height = 3.25, units = "in", dpi = 1200)


## H3: Ideology * Trump prime interaction; Figure 6
pred.m2.2 <- predict(m2.2, score.can.ideology, interval = "confidence") %>%
  as_tibble() %>%
  bind_cols(., score.can.ideology) %>%
  mutate(Trump.prime = as_factor(case_when(
    Trump.prime == 0 ~ "No prime",
    Trump.prime == 1 ~ "Trump prime"
  ))) %>%
  mutate(ideology = ideology + (weighted.mean(can.1$ideology, w = can.1$weight))) %>%
  mutate(Trump.prime = fct_relevel(Trump.prime, "Trump prime", "No prime")) %>%
  filter(ideology %in% c(-0.5, 0, 0.5)) %>%
  mutate(PRED = link) %>%
  mutate(UCL = link + (SE * (qnorm(0.975)))) %>%
  mutate(LCL = link - (SE * (qnorm(0.975)))) %>%
  select(ideology, Trump.prime, PRED, UCL, LCL)

print(pred.m2.2 %>%
        mutate(PRED = round(PRED, 2)) %>%
        select(ideology, Trump.prime, PRED, UCL, LCL), digits = 3)

plot.usa.ideology.h3 <- ggplot(data = pred.m1.2, aes(y = PRED, x = ideology, group = Trudeau.prime)) +
  geom_line(aes(linetype = Trudeau.prime)) +
  geom_ribbon(aes(ymin = LCL, ymax = UCL), alpha = 0.1) +
  labs(y = "Support/oppose Canada-US border closure",
       x = "Ideology (liberal-conservative)") +
  coord_cartesian(ylim = c(-0.5, 0.6), xlim = c(-0.5, 0.5)) +
  scale_y_continuous(breaks = seq(-0.5, 0.5, by = 0.25),
                     labels = c("Strongly\noppose", "", "", "", "Strongly\nsupport")) +
  scale_x_continuous(breaks = seq(-0.5, 0.5, by = 0.25),
                     labels = c("Very liberal","","","","Very conservative")) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 25)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        text = element_text(size = 10, colour = "black"),
        legend.title = element_blank(),
        legend.position = c(0.2, 0.92),
        legend.text = element_text(size = 10, colour = "black"),
        plot.margin = margin(t = 0, b = 0, l = 0, r = 0.4, "in"))

plot.can.ideology.h3 <- ggplot(data = pred.m2.2, aes(y = PRED, x = ideology, group = Trump.prime)) +
  geom_line(aes(linetype = Trump.prime)) +
  geom_ribbon(aes(ymin = LCL, ymax = UCL), alpha = 0.1) +
  labs(y = "Support/oppose Canada-US border closure",
       x = "Ideology (left-right)") +
  coord_cartesian(ylim = c(-0.5, 0.6), xlim = c(-0.5, 0.5)) +
  scale_y_continuous(breaks = seq(-0.5, 0.5, by = 0.25),
                     labels = c("Strongly\noppose", "", "", "", "Strongly\nsupport")) +
  scale_x_continuous(breaks = seq(-0.5, 0.5, by = 0.25),
                     labels = c("Left","","","","Right")) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 25)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        text = element_text(size = 10, colour = "black"),
        legend.title = element_blank(),
        legend.position = c(0.2, 0.92),
        legend.text = element_text(size = 10, colour = "black"))
  
ggsave(plot = plot.can.ideology.h3, file = "Fig 6-Effect plot-CAN-Ideology x Prime 2022 03 03.png",
       device = "png",  width = 5, height = 3.25, units = "in", dpi = 1200)


plot.usa.ideology.h3 <- plot.usa.ideology.h3 +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 25)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        text = element_text(size = 10, colour = "black"),
        legend.title = element_blank(),
        legend.position = c(0.25, 0.92),
        legend.text = element_text(size = 10, colour = "black"),
        plot.margin = margin(t = 0, b = 0, l = 0, r = 0.5, "in")) +
  ggplot2::annotate("text", x = -0.475, y = -0.475, label = "United States", size = 4, fontface = "bold",
                    hjust = 0, vjust = 0)

plot.can.ideology.h3 <- plot.can.ideology.h3 +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 25)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        text = element_text(size = 10, colour = "black"),
        legend.title = element_blank(),
        legend.position = c(0.25, 0.92),
        legend.text = element_text(size = 10, colour = "black"),
        plot.margin = margin(t = 0, b = 0, l = 0, r = 0.5, "in")) +
  ggplot2::annotate("text", x = -0.475, y = -0.475, label = "Canada", size = 4, fontface = "bold",
                    hjust = 0, vjust = 0)

plot.usa.can.ideology.h3 <- plot_grid(plot.usa.ideology.h3, plot.can.ideology.h3)

ggsave(plot = plot.usa.can.ideology.h3, file = "Figs 5-6-Effect plot-USA & CAN-Ideology x Prime 2022 03 03.png",
       device = "png",  width = 9, height = 3.25, units = "in", dpi = 1200)


## H3: PID * Trump prime interaction (for appendix); Figure A2
pred.m2.4 <- predict(m2.4, score.can.pid, interval = "confidence") %>%
  as_tibble() %>%
  bind_cols(., score.can.pid) %>%
  mutate(Trump.prime = as_factor(case_when(
    Trump.prime == 0 ~ "No prime",
    Trump.prime == 1 ~ "Trump prime"
  ))) %>%
  mutate(Trump.prime = fct_relevel(Trump.prime, "Trump prime", "No prime")) %>%
  mutate(PRED = link) %>%
  mutate(UCL = link + (SE * (qnorm(0.975)))) %>%
  mutate(LCL = link - (SE * (qnorm(0.975)))) %>%
  select(pid, Trump.prime, PRED, UCL, LCL)

print(pred.m2.4)

plot.can.pid.h3 <- ggplot(data = pred.m2.4, aes(pid, PRED)) +
  geom_pointrange(aes(x = pid, y = PRED, ymin = LCL, ymax = UCL, colour = Trump.prime),
                  fatten = 1.2, size = 0.5, position = position_dodge(0.2)) +
  geom_errorbar(aes(x = pid, y = PRED, ymin = LCL, ymax = UCL, colour = Trump.prime),
                width = 0.1, position = position_dodge(0.2), show.legend = FALSE) +
  scale_color_manual(values = c("black", "grey60")) +
  labs(y = "Support/oppose Canada-US border closure",
       x = "Party identification") +
  coord_cartesian(ylim = c(-0.5, 0.6)) +
  scale_y_continuous(breaks = seq(-0.5, 0.5, by = 0.25),
                     labels = c("Strongly\noppose", "", "", "", "Strongly\nsupport")) +
  guides(colour = guide_legend(override.aes = list(fatten = 1, size = 0.5))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 25)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        text = element_text(size = 10, colour = "black"),
        legend.title = element_blank(),
        legend.position = c(0.2, 0.92),
        legend.text = element_text(size = 10, colour = "black"))

ggsave(plot = plot.can.pid.h3, file = "Fig A2-Effect plot-CAN-PID x Prime 2022 03 03.png",
       device = "png",  width = 5, height = 3.25, units = "in", dpi = 1200)


## Canada coefficient plot; Figure 2
coef.ci.m.2.1 <- m2.1 %>%
  broom::tidy(., conf.int = TRUE) %>%
  tidycat::tidy_categorical(m = m2.1, include_reference =  TRUE) %>%
  mutate(model.name = "Model 2.1") %>%
  mutate(term = case_when(
    reference == "Baseline Category" ~ str_c(variable, level),
    TRUE ~ term
  )) %>%
  rename(LCL = conf.low,
         UCL = conf.high) %>%
  select(model.name, term, reference, estimate, LCL, UCL)
coef.ci.m.2.2 <- m2.2 %>%
  broom::tidy(., conf.int = TRUE) %>%
  tidycat::tidy_categorical(m = m2.2, include_reference =  TRUE) %>%
  mutate(model.name = "Model 2.2") %>%
  mutate(term = case_when(
    reference == "Baseline Category" ~ str_c(variable, level),
    TRUE ~ term
  )) %>%
  rename(LCL = conf.low,
         UCL = conf.high) %>%
  select(model.name, term, reference, estimate, LCL, UCL)
coef.ci.m.2.3 <- m2.3 %>%
  broom::tidy(., conf.int = TRUE) %>%
  tidycat::tidy_categorical(m = m2.3, include_reference =  TRUE) %>%
  mutate(model.name = "Model 2.3") %>%
  mutate(term = case_when(
    reference == "Baseline Category" ~ str_c(variable, level),
    TRUE ~ term
  )) %>%
  rename(LCL = conf.low,
         UCL = conf.high) %>%
  select(model.name, term, reference, estimate, LCL, UCL)
coef.ci.can <- bind_rows(coef.ci.m.2.1, coef.ci.m.2.2, coef.ci.m.2.3) %>%
  mutate(model.name = as_factor(model.name)) %>%
  mutate(model.name = fct_relevel(model.name, "Model 2.3", "Model 2.2", "Model 2.1")) %>%
  mutate(coef.label = as_factor(case_when(
    term == "(Intercept)" ~ "Intercept",
    term == "Trump.prime" ~ "Trump Prime",
    term == "ideology" ~ "Ideology (left-right)",
    term == "pidLiberal" ~ "Party ID: Liberal (ref.)",
    term == "pidNDP" ~ "Party ID: New Democratic Party",
    term == "pidGreen" ~ "Party ID: Green Party",
    term == "pidBQ" ~ "Party ID: Bloc Québécois",
    term == "pidConservative" ~ "Party ID: Conservative Party",
    term == "pidOther/no party" ~ "Party ID: Other/no party",
    term == "mii.immig" ~ "Most Important Issue: Immigration",
    term == "male" ~ "Sex: Male",
    term == "ln.age.years" ~ "Age (Years Logged)",
    term == "edu" ~ "Education",
    term == "race.ethnWhite" ~ "Race/Ethnicity: White (ref.)",
    term == "race.ethnIndigenous" ~ "Race/Ethnicity: Indigenous",
    term == "race.ethnBlack" ~ "Race/Ethnicity: Black",
    term == "race.ethnAsian" ~ "Race/Ethnicity: Asian",
    term == "race.ethnOther" ~ "Race/Ethnicity: Other",
    term == "regionAtlantic" ~ "Region: Atlantic",
    term == "regionOntario" ~ "Region: Ontario (ref.)",
    term == "regionQuebec" ~ "Region: Quebec",
    term == "regionPrairies" ~ "Region: Prairies",
    term == "regionBC" ~ "Region: British Columbia",
    term == "Trump.prime:ideology" ~ paste0("Trump Prime ", "\u00D7", " Ideology"),
    term == "Trump.prime:mii.immig" ~ paste0("Trump Prime ", "\u00D7", " MII: Immigration")
  ))) %>%
  mutate(coef.label = fct_relevel(coef.label, "Intercept", "Trump Prime",
                                  "Ideology (left-right)",
                                  "Party ID: Liberal (ref.)", "Party ID: New Democratic Party", "Party ID: Green Party",
                                  "Party ID: Bloc Québécois", "Party ID: Conservative Party", "Party ID: Other/no party",
                                  "Most Important Issue: Immigration",
                                  "Trump Prime × Ideology",
                                  "Trump Prime × MII: Immigration",
                                  "Sex: Male", "Age (Years Logged)", "Education",
                                  "Race/Ethnicity: White (ref.)", "Race/Ethnicity: Indigenous", "Race/Ethnicity: Black",
                                  "Race/Ethnicity: Asian", "Race/Ethnicity: Other",
                                  "Region: Ontario (ref.)", "Region: Atlantic", "Region: Quebec", "Region: Prairies", "Region: British Columbia")) %>%
  mutate(pt.shape = case_when(
    model.name == "Model 2.1" & reference == "Baseline Category" ~ 21,
    model.name == "Model 2.1" ~ 16,
    model.name == "Model 2.2" & reference == "Baseline Category" ~ 22,
    model.name == "Model 2.2" ~ 15,
    model.name == "Model 2.3" & reference == "Baseline Category" ~ 23,
    model.name == "Model 2.3" ~ 18)) %>%
  mutate(pt.fill = case_when(
    reference == "Baseline Category" ~ "white"
    #model.name == "Model 2.1" & reference == "Baseline Category" ~ "grey48",
    #model.name == "Model 2.2" & reference == "Baseline Category" ~ "grey32",
    #model.name == "Model 2.3" & reference == "Baseline Category" ~ "grey16"
  )) %>%
  mutate(pt.size = case_when(
    reference == "Baseline Category" ~ 2.5,
    model.name == "Model 2.1" ~ 3,
    model.name == "Model 2.2" ~ 2.5,
    model.name == "Model 2.3" ~ 3.5))

coef.plot.can <- ggplot(data = coef.ci.can, aes(x = estimate, y = coef.label, colour = model.name)) +
  geom_vline(xintercept = 0, colour = "grey72", size = 0.5, linetype = "longdash") +
  geom_pointrange(aes(x = estimate, y = coef.label, xmin = LCL, xmax = UCL, colour = model.name),
                  shape = coef.ci.can$pt.shape, fatten = coef.ci.can$pt.size,
                  fill = coef.ci.can$pt.fill, stroke = 0.4, size = 0.5,
                  position = position_dodge(0.65)) +
  scale_color_manual(values = c("grey48", "grey32", "grey16")) +
  labs(x = "Estimate", y = "") +
  scale_y_discrete(limits = rev(levels(coef.ci.can$coef.label))) +
  coord_cartesian(xlim = c(-0.3, 0.5)) +
  scale_x_continuous(breaks = seq(-0.2, 0.4, by = 0.2)) +
  guides(colour = guide_legend(override.aes = list(shape = c(16, 15, 18), fatten = c(1,1,4), size = 0.5), reverse = TRUE)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size = 10, colour = "black"), 
        axis.text.y = element_text(size = 8, colour = "black"),
        text = element_text(size = 10, colour = "black"),
        legend.title = element_blank(),
        legend.position = c(0.86, 0.08),
        legend.text = element_text(size = 10, colour = "black"))

ggsave(plot = coef.plot.can, file = "Fig 2-Coefficient plot-CAN-2022 03 03.png",
       device = "png",  width = 6, height = 7, units = "in", dpi = 1200)


## Plot, MII * Trump prime; right panel of Figure 7
score.can.immig <- full_seq(0:1, 1) %>% as_tibble() %>%
  rename(mii.immig = value) %>%
  crossing(full_seq(0:1, 1) %>% as_tibble()) %>%
  rename(Trump.prime = value) %>%
  mutate(ideology = 0) %>%
  mutate(pid = "Liberal") %>%
  mutate(male = -0.5) %>%
  mutate(ln.age.years = 0) %>%
  mutate(edu = max(can.2$edu)) %>%
  mutate(race.ethn = "White") %>%
  mutate(region = "Ontario") %>%
  arrange(Trump.prime)

pred.m2.3 <- predict(m2.3, score.can.immig, interval = "confidence") %>%
  as_tibble() %>%
  bind_cols(., score.can.immig) %>%
  mutate(mii.immig = as_factor(case_when(
    mii.immig == 0 ~ "Other issue",
    mii.immig == 1 ~ "Immigration"
  ))) %>%
  mutate(mii.immig = fct_relevel(mii.immig, "Immigration", "Other issue")) %>%
  mutate(Trump.prime = as_factor(case_when(
    Trump.prime == 0 ~ "No prime",
    Trump.prime == 1 ~ "Trump prime"
  ))) %>%
  mutate(Trump.prime = fct_relevel(Trump.prime, "Trump prime", "No prime")) %>%
  mutate(PRED = link) %>%
  mutate(UCL = link + (SE * (qnorm(0.975)))) %>%
  mutate(LCL = link - (SE * (qnorm(0.975)))) %>%
  select(mii.immig, Trump.prime, PRED, UCL, LCL)

print(pred.m2.3, digits = 2)

plot.can.immig <- ggplot(data = pred.m2.3, aes(y = PRED, x = pid)) +
  geom_pointrange(aes(x = mii.immig, y = PRED, ymin = LCL, ymax = UCL, colour = Trump.prime),
                  fatten = 1.2, size = 0.5, position = position_dodge(0.2)) +
  geom_errorbar(aes(x = mii.immig, y = PRED, ymin = LCL, ymax = UCL, colour = Trump.prime),
                width = 0.1, position = position_dodge(0.2), show.legend = FALSE) +
  scale_color_manual(values = c("black", "grey60")) +
  labs(y = "Support/oppose Canada-US border closure",
       x = "Most important issue") +
  coord_cartesian(ylim = c(-0.5, 0.6)) +
  scale_y_continuous(breaks = seq(-0.5, 0.5, by = 0.25),
                     labels = c("Strongly\noppose", "", "", "", "Strongly\nsupport")) +
  guides(colour = guide_legend(override.aes = list(fatten = 1, size = 0.5))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 25)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        text = element_text(size = 10, colour = "black"),
        legend.title = element_blank(), legend.position = c(0.16, 0.92))

#ggsave(plot = plot.can.immig, file = "Effect plot-CAN-Immig MII 2022 03 03.png",
#       device = "png",  width = 5, height = 3.25, units = "in", dpi = 1200)


plot.usa.immig <- plot.usa.immig +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 25)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        text = element_text(size = 10, colour = "black"),
        legend.title = element_blank(),
        legend.position = c(0.22, 0.92),
        legend.text = element_text(size = 10, colour = "black")) + 
  ggplot2::annotate("text", x = 0.5, y = -0.475, label = "United States", size = 4, fontface = "bold",
                    hjust = 0, vjust = 0)

plot.can.immig <- plot.can.immig +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 25)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        text = element_text(size = 10, colour = "black"),
        legend.title = element_blank(),
        legend.position = c(0.2, 0.92),
        legend.text = element_text(size = 10, colour = "black")) + 
  ggplot2::annotate("text", x = 0.5, y = -0.475, label = "Canada", size = 4, fontface = "bold",
                    hjust = 0, vjust = 0)

plot.usa.can.immig <- plot_grid(plot.usa.immig, plot.can.immig)

plot.usa.can.immig

ggsave(plot = plot.usa.can.immig, file = "Fig 7-Effect plot-USA & CAN-Immig 2022 03 03.png",
       device = "png",  width = 9, height = 3.25, units = "in", dpi = 1200)


pred.m2.9 <- predict(m2.9, (distinct(score.can.pid, pid, .keep_all = TRUE)), interval = "confidence") %>%
  as_tibble() %>%
  bind_cols(., (distinct(score.can.pid, pid))) %>%
  mutate(ideology = link + weighted.mean(can.1$ideology, w = can.1$weight)) %>%
  mutate(UCL = ideology + (SE * (qnorm(0.975)))) %>%
  mutate(LCL = ideology - (SE * (qnorm(0.975)))) %>%
  select(pid, ideology, UCL, LCL)


## Plot, ideology * PID; Figure A5
plot.can.ideology.pid <- ggplot(data = pred.m2.9, aes(y = ideology, x = pid)) +
  geom_pointrange(aes(x = pid, y = ideology, ymin = LCL, ymax = UCL), fatten = 1.2, size = 0.5) +
  geom_errorbar(aes(x = pid, y = ideology, ymin = LCL, ymax = UCL), width = 0.1) +
  labs(y = "Ideology (left-right)",
       x = "Party identification") +
  coord_cartesian(ylim = c(-0.5, 0.6)) +
  scale_y_continuous(breaks = seq(-0.5, 0.5, by = 0.25),
                     labels = c("Left","","","","Right")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 25)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        text = element_text(size = 10, colour = "black"),
        legend.title = element_blank())

ggsave(plot = plot.can.ideology.pid, file = "Fig A5-Ideology by PID-CAN 2022 03 03.png",
       device = "png",  width = 5, height = 2.75, units = "in", dpi = 1200)


## Left-right positioning of political parties from the Manifesto Project; Figure A4 & Figure A6
## Source: https://manifesto-project.wzb.eu/datasets/MPDS2021a
MP_API_KEY <- read_tsv("Manifesto Project API key.txt", col_names = FALSE) %>%   ## Get your own
  as_vector() %>% unname()                                                       ## API key!

mp.0 <- mp_maindataset(version = "current", download_format = NULL, apikey = MP_API_KEY) %>%
  filter(countryname %in% c("United States", "Canada"))

mp.1 <- mp.0 %>%
  filter((countryname == "United States" & year(edate) >= 1990)
         | (countryname == "Canada" & year(edate) >= 1990)) %>%
  mutate(year = year(edate)) %>%
  select(countryname, year, partyname, rile, progtype, coderid) %>%
  arrange(countryname, year, partyname)

mp.2 <- mp.1 %>%
  mutate(country = case_when(
    countryname == "United States" ~ "US",
    TRUE ~ countryname,
  )) %>%
  mutate(party = case_when(
    partyname == "New Democratic Party" ~ "NDP",
    partyname == "Green Party" ~ "Green",
    partyname == "Liberal Party of Canada" ~ "Liberal",
    partyname == "Quebec Bloc" ~ "BQ",
    partyname == "Conservative Party of Canada" ~ "Conservative",
    partyname == "Democratic Party" ~ "Democrat",
    partyname == "Republican Party" ~ "Republican",
  )) %>%
  filter(is.na(party) == FALSE) %>%
  select(country, year, party, rile) %>%
  arrange(country, year, party)

mp.3 <- mp.2 %>%
  group_by(country, party) %>%
  summarise(left.right = mean(rile)) %>%
  arrange(country, left.right) %>%
  ungroup()

mp.4 <- tibble(party = character())

mean.func <- function(mp.data) {
  x_bar_mp <- mean(mp.data$rile)
}

for(i in 1:nrow(mp.3)) {
  i_party <- mp.3 %>%
    filter(row_number() == i) %>%
    select(party) %>%
    as_vector() %>%
    unname()
  mp.data <- mp.2 %>%
    filter(party == i_party) %>%
    select(rile)
  bca <- bcajack(x = mp.data, B = 10000, func = mean.func, verbose = FALSE)
  bca <- as.data.frame(bca$stats) %>%
    rownames_to_column() %>%
    as_tibble() %>%
    filter(rowname == "est") %>%
    mutate(party = i_party) %>%
    mutate(UCL = theta + (sdboot * (qnorm(0.975)))) %>%
    mutate(LCL = theta - (sdboot * (qnorm(0.975)))) %>%
    select(party, theta, sdboot, LCL, UCL)
  mp.4 <- bind_rows(mp.4, bca) %>%
    filter(is.na(party) == FALSE)
}

mp.5 <- mp.3 %>%
  select(-left.right) %>%
  left_join(mp.4, by = "party") %>%
  mutate(country = as_factor(country)) %>%
  mutate(country = fct_relevel(country, "US", "Canada")) %>%
  mutate(party = as_factor(party)) %>%
  mutate(party = fct_relevel(party, "Democrat", "Republican",
                             "NDP", "Green", "Liberal", "BQ", "Conservative")) %>%
  rename(left.right = theta) %>%
  arrange(country, left.right)

plot.manifesto.usa <- ggplot(data = (mp.5 %>% filter(country == "US")), aes(y = left.right, x = party)) +
  geom_pointrange(aes(x = party, y = left.right, ymin = LCL, ymax = UCL), fatten = 1.2, size = 0.5) +
  geom_errorbar(aes(x = party, y = left.right, ymin = LCL, ymax = UCL), width = 0.1) +
  labs(y = "Manifesto ideology (left-right)",
       x = "Party") +
  coord_cartesian(ylim = c(-30, 32)) +
  scale_y_continuous(breaks = seq(-30, 30, by = 10)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 25)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        text = element_text(size = 10, colour = "black"),
        legend.title = element_blank())

ggsave(plot = plot.manifesto.usa, file = "Fig A4-Manifesto ideology-US 2022 03 03.png",
       device = "png",  width = 5, height = 2.75, units = "in", dpi = 1200)

plot.manifesto.can <- ggplot(data = (mp.5 %>% filter(country == "Canada")), aes(y = left.right, x = party)) +
  geom_pointrange(aes(x = party, y = left.right, ymin = LCL, ymax = UCL), fatten = 1.2, size = 0.5) +
  geom_errorbar(aes(x = party, y = left.right, ymin = LCL, ymax = UCL), width = 0.1) +
  labs(y = "Manifesto ideology (left-right)",
       x = "Party") +
  coord_cartesian(ylim = c(-30, 32)) +
  scale_y_continuous(breaks = seq(-30, 30, by = 10)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 25)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        text = element_text(size = 10, colour = "black"),
        legend.title = element_blank())

ggsave(plot = plot.manifesto.can, file = "Fig A6-Manifesto ideology-CAN 2022 03 03.png",
       device = "png",  width = 5, height = 2.75, units = "in", dpi = 1200)


## Figure A7 & Figure A8
## Source: https://www.pewresearch.org/global/dataset/summer-2020-survey-data/
zip.file <- paste0("./Pew-Research-Center-Global-Attitudes-Summer-2020-Survey-Data.zip")
sav.file <- paste0("Pew Research Center Global Attitudes Summer 2020 Dataset - Public.sav")
pew.2020.0 <- read_sav(unz(zip.file, sav.file))

attr(pew.2020.0$country, "labels")
attr(pew.2020.0$confid_trump, "labels")
attr(pew.2020.0$POLITICAL_SCALE2, "labels")
attr(pew.2020.0$D_PTYID_PROXIMITY_CANADA, "labels")

xtabs(~ confid_trump, addNA = TRUE, na.action = NULL, data = pew.2020.0, subset = country == 3) %>% prop.table()*100
xtabs(~ POLITICAL_SCALE2, addNA = TRUE, na.action = NULL, data = pew.2020.0, subset = country == 3) %>% prop.table()*100
xtabs(~ D_PTYID_PROXIMITY_CANADA, addNA = TRUE, na.action = NULL, data = pew.2020.0, subset = country == 3) %>% prop.table()*100


## Source: https://www.pewresearch.org/global/dataset/spring-2019-survey-data/
zip.file <- paste0("./Pew-Research-Center-Global-Attitudes-Spring-2019-Survey-Data.zip")
sav.file <- paste0("Pew Research Center Global Attitudes Spring 2019 Dataset WEB.sav")
pew.2019.0 <- read_sav(unz(zip.file, sav.file))

attr(pew.2019.0$country, "labels")
attr(pew.2019.0$CONFID_TRUMP, "labels")
attr(pew.2019.0$POLITICAL_SCALE2, "labels")
attr(pew.2019.0$D_PTYID_PROXIMITY_CANADA, "labels")

xtabs(~ CONFID_TRUMP, addNA = TRUE, na.action = NULL, data = pew.2019.0, subset = country == 5) %>% prop.table()*100
xtabs(~ POLITICAL_SCALE2, addNA = TRUE, na.action = NULL, data = pew.2019.0, subset = country == 5) %>% prop.table()*100
xtabs(~ D_PTYID_PROXIMITY_CANADA, addNA = TRUE, na.action = NULL, data = pew.2019.0, subset = country == 5) %>% prop.table()*100


## Source: https://www.pewresearch.org/global/dataset/spring-2018-survey-data/
zip.file <- paste0("./Pew-Research-Center-Spring-2018-Global-Attitudes-Survey.zip")
sav.file <- paste0("Pew Research Global Attitudes Spring 2018 Dataset WEB FINAL.sav")
pew.2018.0 <- read_sav(unz(zip.file, sav.file))

attr(pew.2018.0$COUNTRY, "labels")
attr(pew.2018.0$confid_trump, "labels")
attr(pew.2018.0$POLITICAL_SCALE2, "labels")
attr(pew.2018.0$D_PTYID_PROXIMITY_CANADA, "labels")

xtabs(~ confid_trump, addNA = TRUE, na.action = NULL, data = pew.2018.0, subset = COUNTRY == 4) %>% prop.table()*100
xtabs(~ POLITICAL_SCALE2, addNA = TRUE, na.action = NULL, data = pew.2018.0, subset = COUNTRY == 4) %>% prop.table()*100
xtabs(~ D_PTYID_PROXIMITY_CANADA, addNA = TRUE, na.action = NULL, data = pew.2018.0, subset = COUNTRY == 4) %>% prop.table()*100


## Source: https://www.pewresearch.org/global/dataset/spring-2017-survey-data/
zip.file <- paste0("./Pew-Research-Center-Spring-2017-Global-Attitudes-Survey_September-2018.zip")
sav.file <- paste0("Pew Research Global Attitudes Spring 2017 Dataset WEB FINAL.sav")
pew.2017.0 <- read_sav(unz(zip.file, sav.file))

attr(pew.2017.0$Country, "labels")
attr(pew.2017.0$confid_trump, "labels")
attr(pew.2017.0$political_scale2, "labels")
attr(pew.2017.0$d_ptyid_proximity_canada, "labels")

xtabs(~ confid_trump, addNA = TRUE, na.action = NULL, data = pew.2017.0, subset = Country == 4) %>% prop.table()*100
xtabs(~ political_scale2, addNA = TRUE, na.action = NULL, data = pew.2017.0, subset = Country == 4) %>% prop.table()*100
xtabs(~ d_ptyid_proximity_canada, addNA = TRUE, na.action = NULL, data = pew.2017.0, subset = Country == 4) %>% prop.table()*100


pew.1 <- bind_rows(
  (pew.2020.0 %>% rename(COUNTRY = country, CONFID_TRUMP = confid_trump) %>% filter(COUNTRY == 3) %>%
     select(ID, weight, QDATE_S, CONFID_TRUMP, POLITICAL_SCALE2, D_PTYID_PROXIMITY_CANADA)),
  (pew.2019.0 %>% rename(COUNTRY = country) %>% filter(COUNTRY == 5) %>%
     select(ID, weight, QDATE_S, CONFID_TRUMP, POLITICAL_SCALE2, D_PTYID_PROXIMITY_CANADA)),
  (pew.2018.0 %>% rename(CONFID_TRUMP = confid_trump) %>% filter(COUNTRY == 4) %>%
     select(ID, weight, QDATE_S, CONFID_TRUMP, POLITICAL_SCALE2, D_PTYID_PROXIMITY_CANADA)),
  (pew.2017.0 %>% rename(COUNTRY = Country, CONFID_TRUMP = confid_trump, POLITICAL_SCALE2 = political_scale2,
                         D_PTYID_PROXIMITY_CANADA = d_ptyid_proximity_canada) %>% filter(COUNTRY == 4) %>%
     select(ID, weight, QDATE_S, CONFID_TRUMP, POLITICAL_SCALE2, D_PTYID_PROXIMITY_CANADA))
  ) %>%
  mutate(date = date(QDATE_S)) %>%
  mutate(year = as_factor(as.character(year(date)))) %>%
  mutate(year = fct_relevel(year, "2017", "2018", "2019", "2020")) %>%
  mutate(confidence.Trump = case_when(
    CONFID_TRUMP %in% 1:4 ~ 5 - CONFID_TRUMP,
    CONFID_TRUMP %in% 8:9 ~ 2.5
  )) %>%
  mutate(left.right = as_factor(case_when(
    POLITICAL_SCALE2 %in% 0:6 ~ as.integer(POLITICAL_SCALE2),
    POLITICAL_SCALE2 %in% 8:9 ~ as.integer(9)
  ))) %>%
  mutate(left.right = fct_relevel(left.right, "3", "0", "1", "2", "4", "5", "6", "9")) %>%
  mutate(lr = as_factor(case_when(
    POLITICAL_SCALE2 %in% 0:2 ~ "Left",
    POLITICAL_SCALE2 %in% c(3) ~ "Center",
    POLITICAL_SCALE2 %in% 4:6 ~ "Right",
    POLITICAL_SCALE2 %in% 8:9 ~ "DK/Ref",
  ))) %>%
  mutate(lr = fct_relevel(lr, "Center", "Left", "Right")) %>%
  mutate(pid = as_factor(case_when(
    D_PTYID_PROXIMITY_CANADA == 1 ~ "Liberal",
    D_PTYID_PROXIMITY_CANADA == 2 ~ "Conservative",
    D_PTYID_PROXIMITY_CANADA == 3 ~ "NDP",
    D_PTYID_PROXIMITY_CANADA == 4 ~ "BQ",
    D_PTYID_PROXIMITY_CANADA == 5 ~ "Green",
    TRUE ~ "Other/no party"
  ))) %>%
  mutate(pid = fct_relevel(pid, "Liberal", "NDP", "Green", "BQ", "Conservative", "Other/no party")) %>%
  select(ID, weight, date, year, confidence.Trump:pid)

xtabs(~ confidence.Trump, addNA = TRUE, na.action = NULL, data = pew.1) %>% prop.table()*100
xtabs(~ left.right, addNA = TRUE, na.action = NULL, data = pew.1) %>% prop.table()*100
xtabs(~ lr, addNA = TRUE, na.action = NULL, data = pew.1) %>% prop.table()*100
xtabs(~ pid, addNA = TRUE, na.action = NULL, data = pew.1) %>% prop.table()*100
xtabs(~ year, addNA = TRUE, na.action = NULL, data = pew.1) %>% prop.table()*100


design.pew.can <- svydesign(data = pew.1, id = ~1, weights = ~weight, strata = ~year)

pew.ideology <- svyglm(confidence.Trump ~ lr + year, design = design.pew.can, family = stats::gaussian())
summ(pew.ideology)

pew.score.can.ideology <- c("Left", "Center", "Right") %>% as_tibble() %>%
  mutate(lr = as_factor(value)) %>%
  mutate(year = as_factor(as.character("2020")))

pred.pew.ideology <- predict(pew.ideology, pew.score.can.ideology, interval = "confidence") %>%
  as_tibble() %>%
  bind_cols(., pew.score.can.ideology) %>%
  mutate(PRED = link) %>%
  mutate(UCL = link + (SE * (qnorm(0.975)))) %>%
  mutate(LCL = link - (SE * (qnorm(0.975)))) %>%
  select(lr, PRED, UCL, LCL)

plot.pew.can.ideology <- ggplot(data = pred.pew.ideology, aes(pid, PRED)) +
  geom_pointrange(aes(x = lr, y = PRED, ymin = LCL, ymax = UCL),
                  fatten = 1.2, size = 0.5) +
  geom_errorbar(aes(x = lr, y = PRED, ymin = LCL, ymax = UCL),
                width = 0.1, show.legend = FALSE) +
  scale_color_manual(values = c("black")) +
  labs(y = "Confidence in U.S. President Donald Trump\nto do the right thing regarding world affairs",
       x = "Ideology") +
  coord_cartesian(ylim = c(1, 4)) +
  scale_y_continuous(breaks = seq(1, 4, by = 1),
                     labels = c("No confidence\nat all ", "Not too much\nconfidence",
                                "Some\nconfidence", "A lot of\nconfidence")) +
  guides(colour = guide_legend(override.aes = list(fatten = 1, size = 0.5))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 25)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        text = element_text(size = 10, colour = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size = 10, colour = "black"))

ggsave(plot = plot.pew.can.ideology, file = "Fig A7-Pew GAP-CAN-Ideology & Trump confidence 2022 03 03.png",
       device = "png",  width = 5, height = 3.25, units = "in", dpi = 1200)


pew.pid <- svyglm(confidence.Trump ~ pid + year, design = design.pew.can, family = stats::gaussian())
summ(pew.pid)

pew.score.can.pid <- c("NDP", "Green", "Liberal", "BQ", "Conservative") %>% as_tibble() %>%
  mutate(pid = as_factor(value)) %>%
  mutate(year = as_factor(as.character("2020")))

pred.pew.pid <- predict(pew.pid, pew.score.can.pid, interval = "confidence") %>%
  as_tibble() %>%
  bind_cols(., pew.score.can.pid) %>%
  mutate(PRED = link) %>%
  mutate(UCL = link + (SE * (qnorm(0.975)))) %>%
  mutate(LCL = link - (SE * (qnorm(0.975)))) %>%
  select(pid, PRED, UCL, LCL)

plot.pew.can.pid <- ggplot(data = pred.pew.pid, aes(pid, PRED)) +
  geom_pointrange(aes(x = pid, y = PRED, ymin = LCL, ymax = UCL),
                  fatten = 1.2, size = 0.5) +
  geom_errorbar(aes(x = pid, y = PRED, ymin = LCL, ymax = UCL),
                width = 0.1, show.legend = FALSE) +
  scale_color_manual(values = c("black")) +
  labs(y = "Confidence in U.S. President Donald Trump\nto do the right thing regarding world affairs",
       x = "Party identification") +
  coord_cartesian(ylim = c(1, 4)) +
  scale_y_continuous(breaks = seq(1, 4, by = 1),
                     labels = c("No confidence\nat all ", "Not too much\nconfidence",
                                "Some\nconfidence", "A lot of\nconfidence")) +
  guides(colour = guide_legend(override.aes = list(fatten = 1, size = 0.5))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 25)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        text = element_text(size = 10, colour = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size = 10, colour = "black"))

ggsave(plot = plot.pew.can.pid, file = "Fig A8-Pew GAP-CAN-PID & Trump confidence 2022 03 03.png",
       device = "png",  width = 5, height = 3.25, units = "in", dpi = 1200)


#library(codebook)
#usa.w1.codebook <- read_sav("SVMK_NatID_Immig_USA_20200727.sav") %>%
#  select(resp_id:weight, STATE:ST, condition:border_trt, q2, issue_matters_most, q8:ideology, party5, q18:age6, age) %>%
#  codebook_table() %>%
#  mutate(value_labels = str_replace_all(value_labels, "\n", " ")) %>%
#  select(name:n_missing, min, max, mean, sd)
#write_csv(usa.w1.codebook, "SVMK_APSR_USA_20210826-codebook.csv")

#usa.w2.codebook <- read_sav("SVMK_For_Ldrs_USA_20210830.sav") %>%
#  select(resp_id:weight, STATE:ST, condition, q1, q2, q5, q10, q25, q12, q27, q13, q28,
#         q19, q34, q20, q35, q22, q37, q23, q38, ideology, party, lean, gender_non_binary:division) %>%
#  codebook_table() %>%
#  mutate(value_labels = str_replace_all(value_labels, "\n", " ")) %>%
#  select(name:n_missing, min, max, mean, sd)
#write_csv(usa.w2.codebook, "SVMK_APSR_USA_w2_20210830-codebook.csv")

#can.codebook <- read_sav("SVMK_NatID_Immig_CAN_20201125.sav") %>%
#  select(resp_id:weight, language, PR, condition:border_trt, q2, issue_matters_most, q8:q16, q17:q19, q22, q24, q55, age6, age) %>%
#  codebook_table() %>%
#  mutate(value_labels = str_replace_all(value_labels, "\n", " ")) %>%
#  select(name:n_missing, min, max, mean, sd)
#write_csv(can.codebook, "SVMK_APSR_CAN_20210826-codebook.csv")
