# ========================================================================= #
# ASKING THE RIGHT QUESTIONS
# - Replicate Figures 2 through 5 in main text (based on CES data)
# - Author: Patrick W. Kraft
# - Date: 03/19/2022
# ========================================================================= #

## Required packages
library(tidyverse)
library(haven)
library(survey)
library(broom)
library(marginaleffects)
library(margins)

## Custom functions
se <- function(x, na.rm = T){
  sd(x, na.rm = na.rm)/sqrt(length(na.omit(x)))
}
cilo <- function(x, na.rm = T){
  mean(x, na.rm = na.rm) - qnorm(.975) * se(x, na.rm)
}
cihi <- function(x, na.rm = T){
  mean(x, na.rm = na.rm) + qnorm(.975) * se(x, na.rm)
}
invlogit <- function(x) 1/(1+exp(-x))

## plot defaults
plot_default <- theme_classic(base_size=9) + theme(panel.border = element_rect(fill=NA))



# Prepare CCES survey responses -------------------------------------------

## Recode raw data
cces <- read_sav("data/CCES18_UWM_OUTPUT_vv.sav") %>% 
  transmute(
    weight = teamweight,
    
    ## Sociodemographics
    age = 2018 - birthyr,
    female = factor(gender - 1, labels = c("Male","Female")),
    college = as.numeric(educ > 4),
    black = as.numeric(race == 2),
    married = as.numeric(marstat == 1),
    
    ## CCES ideological placements
    ideo_dem = recode(as.numeric(CC18_334D), `8` = NA_real_),
    ideo_rep = recode(as.numeric(CC18_334E), `8` = NA_real_),
    
    ## CCES common core knowledge questions
    pk_house = as.numeric(CC18_309a == 1),
    pk_senate = as.numeric(CC18_309b == 1),
    pk_ideol = ifelse(is.na(ideo_dem) | is.na(ideo_rep), 
                      0, as.numeric(ideo_dem < ideo_rep)),
    pk_combined = (pk_house + pk_senate + pk_ideol)/3,
    
    ## UWM policy knowledge
    know_guns = as.numeric(UWM311 == 2),
    know_abortion = as.numeric(UWM314 == 4),
    know_daca = as.numeric(UWM317 == 4),
    know_health = as.numeric(UWM320 == 3),
    know_trade = as.numeric(UWM323 == 1),
    know = (know_trade + know_daca + know_abortion)/3,
    
    ## Political engagement & internal efficacy
    turnout = recode(as.numeric(CC18_350), `1` = 1, `3` = 1, .default = 0),
    polatt = (5 - UWM329)/4,
    campint = (3 - UWM330)/2,
    effic_int = (UWM331 - UWM332 + 4)/8
  )

## set survey design
cces_svy <- svydesign(id = ~1, weights = ~weight, data = cces)

## additional survey objects to avoid NA error in margins package (I should fix this and create a pull request)
cces$missing <- with(cces, is.na(female) | is.na(age) | is.na(college) | is.na(black) | is.na(married))
pk_svy <- list(
  svydesign(id = ~1, weights = ~weight, data = filter(cces, !missing, !is.na(pk_combined), !is.na(turnout))),
  svydesign(id = ~1, weights = ~weight, data = filter(cces, !missing, !is.na(pk_combined), !is.na(polatt))),
  svydesign(id = ~1, weights = ~weight, data = filter(cces, !missing, !is.na(pk_combined), !is.na(campint))),
  svydesign(id = ~1, weights = ~weight, data = filter(cces, !missing, !is.na(pk_combined), !is.na(effic_int)))
)
know_svy <- list(
  svydesign(id = ~1, weights = ~weight, data = filter(cces, !missing, !is.na(know), !is.na(turnout))),
  svydesign(id = ~1, weights = ~weight, data = filter(cces, !missing, !is.na(know), !is.na(polatt))),
  svydesign(id = ~1, weights = ~weight, data = filter(cces, !missing, !is.na(know), !is.na(campint))),
  svydesign(id = ~1, weights = ~weight, data = filter(cces, !missing, !is.na(know), !is.na(effic_int)))
)



# Figure 2: Gender difference in average proportions of correct re --------

## Variable names
varnames_know <- c("pk_house", "pk_senate", "pk_ideol",
                   "know_guns", "know_trade", "know_daca", 
                   "know_health", "know_abortion")
varnames_controls <- c("age", "college", "black", "married")

## Estimate models
m1bivariate <- map(varnames_know, ~reformulate("female", response = .)) %>% 
  map(~svyglm(., family = binomial(logit), design = cces_svy))
m1controls <- map(varnames_know, ~reformulate(c("female", varnames_controls), response = .)) %>% 
  map(~svyglm(., family = binomial(logit), design = cces_svy))

## Plot average marginal effects
c(m1bivariate, m1controls) %>% 
  map_dfr(~summary(marginaleffects(.)), .id = "model") %>% 
  as_tibble() %>% 
  filter(term == "femaleFemale") %>% 
  mutate(dv = rep(varnames_know, 2),
         Controls = rep(c("No","Yes"), each = 8),
         Question = factor(
           dv, 
           levels = rev(varnames_know),
           labels = rev(c("Majority in the House",
                          "Majority in the Senate", 
                          "Ideological Placement", 
                          "Gun Legislation",
                          "Trade Policy", "DACA",
                          "Health Care", "Abortion"))),
         Group = recode_factor(
           dv, 
           pk_house = "Traditional Items", 
           pk_senate = "Traditional Items", 
           pk_ideol = "Traditional Items",
           know_guns = "Male-dominated Policy Items", 
           know_daca = "Neutral", 
           know_abortion = "Female-dominated Policy Items", 
           know_health = "Female-dominated Policy Items", 
           know_trade = "Male-dominated Policy Items")) %>% 
  ggplot(aes(x = estimate, y = Question,
             xmin = conf.low, xmax = conf.high, 
             shape = Controls, col = Controls)) +
  plot_default + 
  geom_vline(xintercept = 0, col = "gray") +
  geom_pointrange(position = position_dodge(width = -.5)) + 
  scale_color_brewer(palette = "Dark2") +
  facet_wrap(~Group, ncol = 1, scales = "free_y") +
  labs(
    y = "", 
    x = "Gender Differences in Probability of Correct Responses"
  )
ggsave("output/fig02-knowdiff_items.png", width = 7, height = 5)



# Figure 3: Differences in knowledge levels -------------------------------

summary(svyglm(pk_combined ~ female + age + college + black + married, design = cces_svy))
summary(svyglm(know ~ female + age + college + black + married, design = cces_svy))

svyttest(pk_combined~female, design = cces_svy)$p.value

yvals <- c(.92, .35)

df_pval <- tibble(
  label = c("p < 0.001", paste("p =",round(svyttest(know~female, design = cces_svy)$p.value, 3))),
  x = "Male",
  y = yvals + .05,
  Knowledge = factor(c("Conventional", "Balanced"), 
                     levels = c("Conventional", "Balanced"))
)

df_segm <- tibble(
  x = "Male",
  y = yvals,
  xend = "Female",
  yend = yvals,
  Knowledge = factor(c("Conventional", "Balanced"), 
                     levels = c("Conventional", "Balanced"))
)

df_vert <- tibble(
  x = rep(c("Male", "Female"), 2),
  y = rep(yvals, each = 2),
  xend = rep(c("Male", "Female"), 2),
  yend = rep(yvals, each = 2) - .03,
  Knowledge = factor(rep(c("Conventional", "Balanced"), each = 2), 
                     levels = c("Conventional", "Balanced"))
)

cces %>%
  select(female, know, pk_combined) %>%
  gather(item, value, -female) %>%
  group_by(female, item) %>%
  summarize(mean = mean(value, na.rm = T), 
            cilo = cilo(value), 
            cihi = cihi(value)) %>% 
  mutate(Knowledge = recode_factor(item,
                                   `pk_combined` = "Conventional",
                                   `know` = "Balanced")) %>% 
  ggplot(aes(x = female, fill = female, y = mean, ymin = cilo, ymax = cihi)) +
  plot_default + 
  geom_col() + 
  geom_linerange() +
  facet_wrap(~Knowledge) +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Paired") +
  labs(y = "Average Knowledge",
       x = "") +
  ylim(0,1) +
  geom_segment(data = df_segm, 
               aes(x = x, y = y, yend = yend, xend = xend),
               inherit.aes=FALSE) +
  geom_segment(data = df_vert, 
               aes(x = x, y = y, yend = yend, xend = xend),
               inherit.aes=FALSE) +
  geom_text(data = df_pval,
            aes(x = x, y = y, label = label),
            inherit.aes=FALSE, 
            position = position_nudge(x = .5),
            cex = 2)
ggsave("output/fig03-knowdiff_overall.png", width = 3, height = 2)


# Figure 4/5: Political knowledge as a predictor of turnout, engagem --------

## Estimate models
m2pk <- list(
  svyglm(turnout ~ pk_combined * female + age + college + black + married, 
         family = binomial(logit), design = pk_svy[[1]]),
  svyglm(polatt ~ pk_combined * female + age + college + black + married, design = pk_svy[[2]]),
  svyglm(campint ~ pk_combined * female + age + college + black + married, design = pk_svy[[3]]),
  svyglm(effic_int ~ pk_combined * female + age + college + black + married, design = pk_svy[[4]])
)

m2know <- list(
  svyglm(turnout ~ know * female + age + college + black + married, 
         family = binomial(logit), design = know_svy[[1]]),
  svyglm(polatt ~ know * female + age + college + black + married, design = know_svy[[2]]),
  svyglm(campint ~ know * female + age + college + black + married, design = know_svy[[3]]),
  svyglm(effic_int ~ know * female + age + college + black + married, design = know_svy[[4]])
)

## Plot average marginal effects
bind_rows(
  map2(m2pk, pk_svy, 
       ~margins(.x, design = .y, variables = "pk_combined", 
                at = list(female = c("Male", "Female")))) %>% 
    map(summary) %>% 
    map_dfr(tibble),
  map2(m2know, know_svy, 
       ~margins(.x, design = .y, variables = "know", 
                at = list(female = c("Male", "Female")))) %>% 
    map(summary) %>% 
    map_dfr(tibble)
) %>% 
  mutate(
    Gender = factor(female, levels = c("Male","Female")),
    Outcome = rep(rep(c("Turnout","Attention\nto Politics",
                        "Campaign\nInterest","Internal\nEfficacy"), each = 2), 2),
    Outcome = factor(Outcome, levels = rev(c("Turnout","Attention\nto Politics",
                                             "Campaign\nInterest","Internal\nEfficacy"))),
    Knowledge = recode_factor(factor,
                              `pk_combined` = "Conventional",
                              `know` = "Balanced")
  ) %>% 
  ggplot(aes(x = AME, y = Outcome,
             xmin = lower, xmax = upper, 
             shape = Gender, col = Gender)) +
  plot_default + 
  geom_vline(xintercept = 0, col = "gray") +
  geom_pointrange(position = position_dodge(width = -.5)) + 
  scale_color_brewer(palette = "Paired") +
  facet_wrap(~Knowledge, ncol = 2) +
  labs(
    y = "", 
    x = "Conditional Average Treatment Effect of Political Knowledge"
  )
ggsave("output/fig04-know_cate.png", width = 7, height = 3)

## Plot average predictions
newdata_female <- tibble(
  pk_combined = seq(0,1,length.out = 10),
  know = seq(0,1,length.out = 10),
  female = "Female",
  age = mean(cces$age, na.rm = T),
  college = mean(cces$college, na.rm = T),
  black = mean(cces$black, na.rm = T),
  married = mean(cces$married, na.rm = T)
)
newdata_male <- newdata_female %>% 
  mutate(female = "Male")

bind_rows(
  m2pk %>% 
    map(~predict(., newdata = newdata_female)) %>% 
    map(as_tibble) %>% 
    map_dfr(~bind_cols(newdata_female, .)) %>% 
    mutate(Outcome = rep(1:4, each = 10),
           Knowledge = "Conventional"),
  m2pk %>% 
    map(~predict(., newdata = newdata_male)) %>% 
    map(as_tibble) %>% 
    map_dfr(~bind_cols(newdata_male, .)) %>% 
    mutate(Outcome = rep(1:4, each = 10),
           Knowledge = "Conventional"),
  m2know %>% 
    map(~predict(., newdata = newdata_female)) %>% 
    map(as_tibble) %>% 
    map_dfr(~bind_cols(newdata_female, .)) %>% 
    mutate(Outcome = rep(1:4, each = 10),
           Knowledge = "Balanced"),
  m2know %>% 
    map(~predict(., newdata = newdata_male)) %>% 
    map(as_tibble) %>% 
    map_dfr(~bind_cols(newdata_male, .)) %>% 
    mutate(Outcome = rep(1:4, each = 10),
           Knowledge = "Balanced")
) %>% 
  mutate(response = invlogit(link),
         cilo = invlogit(link - qnorm(.975) * SE),
         cihi = invlogit(link + qnorm(.975) * SE),
         Gender = factor(female, levels = c("Male","Female")),
         Outcome = factor(Outcome, labels = c("Turnout","Attention\nto Politics",
                                              "Campaign\nInterest","Internal\nEfficacy")),
         Knowledge = factor(Knowledge, levels = c("Conventional","Balanced"))) %>% 
  ggplot(aes(x = know, y = response, ymax = cihi, ymin = cilo, lty = Gender, fill = Gender)) +
  plot_default +
  geom_ribbon(alpha = .5) +
  geom_line() +
  scale_fill_brewer(palette = "Paired") +
  facet_grid(Outcome ~ Knowledge, scales = "free_y") +
  labs(
    y = "Expected Value", 
    x = "Political Knowledge"
  )
ggsave("output/fig05-know_expected.png", width = 5, height = 6)



# Check whether interactions are significantly different ------------------

marginaleffects(m2pk[[1]], variable = "pk_combined", 
                newdata = typical(female = "Female")) %>% 
  summary(conf.level = .86)
marginaleffects(m2pk[[1]], variable = "pk_combined", 
                newdata = typical(female = "Male")) %>% 
  summary(conf.level = .86)
marginaleffects(m2pk[[2]], variable = "pk_combined", 
                newdata = typical(female = "Female")) %>% 
  summary(conf.level = .86)
marginaleffects(m2pk[[2]], variable = "pk_combined", 
                newdata = typical(female = "Male")) %>% 
  summary(conf.level = .86)
marginaleffects(m2pk[[1]], variable = "pk_combined", 
                newdata = counterfactual(female = "Female")) %>% 
  summary(conf.level = .86)
marginaleffects(m2pk[[1]], variable = "pk_combined", 
                newdata = counterfactual(female = "Male")) %>% 
  summary(conf.level = .86)
marginaleffects(m2pk[[2]], variable = "pk_combined", 
                newdata = counterfactual(female = "Female")) %>% 
  summary(conf.level = .86)
marginaleffects(m2pk[[2]], variable = "pk_combined", 
                newdata = counterfactual(female = "Male")) %>% 
  summary(conf.level = .86)

m2sim <- c(m2pk, m2know) %>% 
  map(~tidy(., conf.int=T)) %>% 
  map(slice_tail) %>% 
  map_dfr(~mutate(., coef = list(rnorm(1000, mean = estimate, sd = std.error)),
                  cilo = quantile(unlist(coef), .025),
                  cihi = quantile(unlist(coef), .975))) %>% 
  mutate(Outcome = rep(c("Turnout","Attention\nto Politics",
                         "Campaign\nInterest","Internal\nEfficacy"), 2))

as_tibble(map_dfc(m2sim$coef[5:8], matrix) - map_dfc(m2sim$coef[1:4], matrix)) %>% 
  pivot_longer(everything()) %>% 
  group_by(name) %>% 
  summarize(mean = mean(value),
            cilo95 = quantile(value, .025),
            cilo90 = quantile(value, .05),
            cihi90 = quantile(value, .95),
            cihi95 = quantile(value, .975)) %>% 
  mutate(name = recode_factor(name,
                              `...1` = "Turnout",
                              `...2` = "Attention\nto Politics",
                              `...3` = "Campaign\nInterest",
                              `...4` = "Internal\nEfficacy"))



# Export Session Info -----------------------------------------------------

sessionInfo() %>% 
  capture.output() %>% 
  writeLines("output/02-cces_sessionInfo.txt")
