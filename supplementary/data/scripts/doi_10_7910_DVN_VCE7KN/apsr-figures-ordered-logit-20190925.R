
devtools::install_github("mvuorre/brmstools")
# Figures for APSR R&R models
library(gt)
library(psych)
library(readtext)
library(dfoptim)
library(arm)
library(here)
library(tidybayes)
library(RCurl)
library(modelr)
library(merTools)
library(reshape2)
library(xtable)
library(texreg)
library(RWmisc)
library(purrr)
library(margins)
library(brmstools)
library(dotwhisker)
library(ggcorrplot)
library(scales)
library(corrplot)
library(GGally)
library(ggstance)
library(ggpubr)
library(facetscales)
library(ggridges)
library(modelsummary)

here()

m1.df <- tidy(m1)
m1.df$model <- "US Troops"
m2.df <- tidy(m2)
m2.df$model <- "US Government"
m3.df <- tidy(m3)
m3.df$model <- "US People"

m.com <- rbind(m1.df, m2.df, m3.df) %>% 
  dplyr::filter(term == "contact_nonpersDon't know/Decline to answer" |
                  term == "contact_nonpersYes" |
                  term == "contact_persDon't know/Decline to answer" |
                  term == "contact_persYes" |
                  term == "benefit_persDon't know/Decline to answer" |
                  term == "benefit_persYes" |
                  term == "benefit_nonpersDon't know/Decline to answer" |
                  term == "benefit_nonpersYes") %>% 
  arrange(term) %>% 
  mutate(Variable = rep(c("Network Benefit", "Personal Benefit", "Network Contact", "Personal Contact"), each = 6),
         Variable = factor(Variable, levels = c("Personal Contact", "Network Contact", "Personal Benefit", "Network Benefit")),
         term = ifelse(term == "contact_nonpersDon't know/Decline to answer" | 
                         term == "contact_persDon't know/Decline to answer" |
                         term == "benefit_persDon't know/Decline to answer" |
                         term == "benefit_nonpersDon't know/Decline to answer", "Don't know/Decline to answer", term),
         term = ifelse(term == "contact_nonpersYes" |
                         term ==  "contact_persYes" |
                         term == "benefit_persYes" |
                         term == "benefit_nonpersYes", "Yes", term),
         term = factor(term, levels = c("Yes", "Don't know/Decline to answer")),
         model = factor(model, levels = c("US Troops", "US Government", "US People")),
         estimate = as.numeric(estimate))

dwplot(m.com, 
       dot_args = list(aes(shape = model), size = 2.5, color = "black"),
       whisker_args = list(color = "black"),
       conf.level = c(.90, .95)) +
  geom_vline(xintercept = 0) +
  facet_grid(Variable ~ ., scales = "free") +
  theme_bw() +
  scale_x_continuous(breaks = seq(-0.6, 0.6, .2), labels = comma_format(accuracy = 0.1)) +
  guides(color = FALSE) +
  labs(shape = "Outcome Variable",
       x = "Coefficient Estimate")

ggsave(here("Figures", "apsr-figure-coefplot-ordered-base.pdf"))



# Ordered models - Country variables
m1.df.c <- tidy(m1.cvars)
m1.df.c$model <- "US Troops"
m2.df.c <- tidy(m2.cvars)
m2.df.c$model <- "US Government"
m3.df.c <- tidy(m3.cvars)
m3.df.c$model <- "US People"

m.com.c <- rbind(m1.df.c, m2.df.c, m3.df.c) %>% 
  dplyr::filter(term == "contact_nonpersDon't know/Decline to answer" |
                  term == "contact_nonpersYes" |
                  term == "contact_persDon't know/Decline to answer" |
                  term == "contact_persYes" |
                  term == "benefit_persDon't know/Decline to answer" |
                  term == "benefit_persYes" |
                  term == "benefit_nonpersDon't know/Decline to answer" |
                  term == "benefit_nonpersYes") %>% 
  arrange(term) %>% 
  mutate(Variable = rep(c("Network Benefit", "Personal Benefit", "Network Contact", "Personal Contact"), each = 6),
         Variable = factor(Variable, levels = c("Personal Contact", "Network Contact", "Personal Benefit", "Network Benefit")),
         term = ifelse(term == "contact_nonpersDon't know/Decline to answer" | 
                         term == "contact_persDon't know/Decline to answer" |
                         term == "benefit_persDon't know/Decline to answer" |
                         term == "benefit_nonpersDon't know/Decline to answer", "Don't know/Decline to answer", term),
         term = ifelse(term == "contact_nonpersYes" |
                         term ==  "contact_persYes" |
                         term == "benefit_persYes" |
                         term == "benefit_nonpersYes", "Yes", term),
         term = factor(term, levels = c("Yes", "Don't know/Decline to answer")),
         model = factor(model, levels = c("US Troops", "US Government", "US People")),
         estimate = as.numeric(estimate))

dwplot(m.com.c, 
       dot_args = list(aes(shape = model), size = 2.5, color = "black"),
       whisker_args = list(color = "black"),
       conf.level = c(.90, .95)) +
  geom_vline(xintercept = 0) +
  facet_grid(Variable ~ ., scales = "free") +
  theme_bw() +
  scale_x_continuous(breaks = seq(-0.6, 0.6, .2), labels = comma_format(accuracy = 0.1)) +
  guides(color = FALSE) +
  labs(shape = "Outcome Variable",
       x = "Coefficient Estimate")

ggsave(here("Figures", "apsr-figure-coefplot-ordered-cvars-20190925.pdf"))


m.com.both <- rbind(m.com, m.com.c) %>% 
  mutate(version = rep(c("Base Model", "Full Model"), each = 24))

dwplot(m.com.both, 
       dot_args = list(aes(shape = model), size = 2.5, color = "black"),
       whisker_args = list(color = "black"),
       conf.level = c(.90, .95)) +
  geom_vline(xintercept = 0) +
  facet_grid(Variable ~ version, scales = "free") +
  theme_bw() +
  scale_x_continuous(breaks = seq(-0.6, 0.6, .2), labels = comma_format(accuracy = 0.1)) +
  guides(color = FALSE) +
  labs(shape = "Outcome Variable",
       x = "Coefficient Estimate")

ggsave(here("Figures", "apsr-figure-coefplot-ordered-comparison-20190925.pdf"))


# Intercept figures
# Intercept figures for orginal base models
ranef.df.1 <- data.frame(c(ranef = ranef(m1, condVar = TRUE), 
                           country = attributes(ranef(m1)$country))) %>% 
  mutate(model = "US Troops")

ranef.df.2 <- data.frame(c(ranef = ranef(m2, condVar = TRUE), 
                           country = attributes(ranef(m2)$country))) %>% 
  mutate(model = "US Government")

ranef.df.3 <- data.frame(c(ranef = ranef(m3, condVar = TRUE), 
                           country = attributes(ranef(m3)$country))) %>% 
  mutate(model = "US People")

stddev.list <- list()
stddev.list[[1]] <- data.frame(VarCorr(m1))
stddev.list[[2]] <- data.frame(VarCorr(m2))
stddev.list[[3]] <- data.frame(VarCorr(m3))
stddev.list[[1]]$model <- "US Troops"
stddev.list[[2]]$model <- "US Government"
stddev.list[[3]]$model <- "US People"

stddev.df.com <- do.call("rbind", stddev.list)
colnames(stddev.df.com)[1] <- "stddev"

ranef.df.com <- rbind(ranef.df.1, ranef.df.2, ranef.df.3) %>% 
  mutate(model = factor(model, levels = c("US Troops", "US Government", "US People"))) %>% 
  left_join(stddev.df.com) %>% 
  mutate(stddev = sqrt(stddev))

# Random effects estimate for model with country variables
ranefplot.1 <- ggplot(ranef.df.com, aes(x = country.row.names, y = X.Intercept., fill = model)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", size = 0.1) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  scale_fill_grey(limits = c("US Troops", "US Government", "US People")) +
  scale_x_discrete() +
  scale_y_continuous(breaks = seq(-1, 1.55, 0.25)) +
  theme_bw() +
  labs(y = "Varying Intercept Estimates",
       x = "Country",
       fill = "Model")

ggsave(here("Figures", "apsr-figure-ranef-base-20190925.pdf"))

# Plot of random effect standard deviation for three models with country vars
ranefplot.2 <- ggplot(ranef.df.com, aes(x = model, y = stddev, fill = model)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", size = 0.1) +
  coord_flip() +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 0.75, 0.25), limits = c(0, 0.75)) +
  scale_fill_grey(limits = c("US Troops", "US Government", "US People")) +
  labs(x = "Model",
       y = "Std. Dev.",
       fill = "Model")

ggsave(here("Figures", "apsr-figure-ranef-stddev-base-20190925.pdf"))

ggarrange(
  ranefplot.1, ranefplot.2, 
  labels = c("A", "B"),
  common.legend = TRUE, legend = "bottom"
)

ggsave(here("Figures", "apsr-figure-ranef-combined-base-20190925.pdf"))



# Intercept figures for country variable models
ranef.cvars.1 <- data.frame(c(ranef = ranef(m1.cvars, condVar = TRUE), 
                           country = attributes(ranef(m1.cvars)$country))) %>% 
mutate(model = "US Troops")

ranef.cvars.2 <- data.frame(c(ranef = ranef(m2.cvars, condVar = TRUE), 
                           country = attributes(ranef(m2.cvars)$country))) %>% 
  mutate(model = "US Government")

ranef.cvars.3 <- data.frame(c(ranef = ranef(m3.cvars, condVar = TRUE), 
                           country = attributes(ranef(m3.cvars)$country))) %>% 
  mutate(model = "US People")

stddev.cvars.list <- list()
stddev.cvars.list[[1]] <- data.frame(VarCorr(m1.cvars))
stddev.cvars.list[[2]] <- data.frame(VarCorr(m2.cvars))
stddev.cvars.list[[3]] <- data.frame(VarCorr(m3.cvars))
stddev.cvars.list[[1]]$model <- "US Troops"
stddev.cvars.list[[2]]$model <- "US Government"
stddev.cvars.list[[3]]$model <- "US People"

stddev.cvars.com <- do.call("rbind", stddev.cvars.list)
colnames(stddev.cvars.com)[1] <- "stddev"

ranef.cvars.com <- rbind(ranef.cvars.1, ranef.cvars.2, ranef.cvars.3) %>% 
  mutate(model = factor(model, levels = c("US Troops", "US Government", "US People"))) %>% 
  left_join(stddev.cvars.com) %>% 
  mutate(stddev = sqrt(stddev))

# Random effects estimate for model with country variables
 ranefplot.cvar.1 <- ggplot(ranef.cvars.com, aes(x = country.row.names, y = X.Intercept., fill = model)) +
    geom_bar(stat = "identity", position = "dodge", color = "black", size = 0.1) +
    geom_hline(yintercept = 0) +
    coord_flip() +
    scale_fill_grey(limits = c("US Troops", "US Government", "US People")) +
    scale_x_discrete() +
    scale_y_continuous(breaks = seq(-0.75, 0.75, 0.25)) +
    theme_bw() +
    labs(y = "Varying Intercept Estimates",
         x = "Country",
         fill = "Model")
  
  ggsave(here("Figures", "apsr-figure-ranef-cvars.pdf"))
  
# Plot of random effect standard deviation for three models with country vars
  ranefplot.cvar.2 <- ggplot(ranef.cvars.com, aes(x = model, y = stddev, fill = model)) +
    geom_bar(stat = "identity", position = "dodge", color = "black", size = 0.1) +
    coord_flip() +
    theme_bw() +
    scale_y_continuous(breaks = seq(0, 0.5, 0.1), limits = c(0, 0.5)) +
    scale_fill_grey(limits = c("US Troops", "US Government", "US People")) +
    labs(x = "Model",
         y = "Std. Dev.",
         fill = "Model")
  
  ggsave(here("Figures", "apsr-figure-ranef-stddev-cvars-20190925.pdf"))
  
  ggarrange(
    ranefplot.cvar.1, ranefplot.cvar.2, 
    labels = c("A", "B"),
    common.legend = TRUE, legend = "bottom"
  )
  
  ggsave(here("Figures", "apsr-figure-ranef-combined-cvars-20190925.pdf"))
  
  
  
 