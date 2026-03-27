##----------------------------------------------------------------------------##
##                   Fighting For a Better Life:
##           Protests and Public Opinion in South Africa                   
##
##              REPLICATION FILE 2: VIGNETTE ANALYSIS
##----------------------------------------------------------------------------##


##----------------------------------------------------------------
## 1. Set up                                                     -
##----------------------------------------------------------------

# A function to install the required packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE,repos = c(
      CRAN = 'https://cran.rstudio.com',
      CRANextra = 'https://macos.rbind.io'
    )
    
    
    )
  sapply(pkg, require, character.only = TRUE)
}


packages_needed <- c("foreign", 
                     "readr",
                     "plyr",
                     "dplyr",
                     "tidyverse",
                     "tidyr",
                     "tibble", 
                     "knitr",
                     "printr",
                     "stargazer",
                     "ggplot2",
                     "rmarkdown",
                     "repmis",
                     "multiwayvcov",
                     "lmtest",
                     "stringr",
                     "kableExtra",
                     "MASS",
                     "fastDummies",
                     "cjoint",
                     "cregg",
                     "haven",
                     "readr",
                     "qualtRics",
                     "report",
                     "htmlTable",
                     "arm",
                     "Hmisc",
                     "varhandle",
                     "ggmap",
                     "readxl",
                     "lessR",
                     "writexl",
                     "broom",
                     "dotwhisker",
                     "estimatr",
                     "texreg",
                     "RColorBrewer",
                     "fixest"
)

ipak(packages_needed)

### Set WD --------

#path = " "
#setwd(path)
#getwd()  

##----------------------------------------------------------------
## 2. Data Read In                                               -
##----------------------------------------------------------------

# Data for Vignette analysis
load("Rdataframe_sa_protest_vignette.RData")

# Data for metro municipal check
load("Rdataframe_sa_protest_vignette_metro_vs_other.RData")


##----------------------------------------------------------------
## 3. Vignette Analysis                                               -
##----------------------------------------------------------------

##-----------------------------------------------------------------------------------
## Outcome "Protest Sympathy": I support people who protest against lack of service delivery
##-----------------------------------------------------------------------------------
m3 <- Outcome3 ~ VignetteExp_Treatment1 + VignetteExp_Treatment2 + VignetteExp_Treatment3 + VignetteExp_Treatment4

# Figure 3 (a) - Coefficient plot 
#-----------------------------------------------------------------
model <- feols(m3, protest_SA,
               vcov = ~municipal_id,
               fixef = c("municipal_id"))
summary(model)

results <- tidy(model)
fit_cis_95 <- confint(model, level = 0.95) %>% 
  data.frame() %>%
  dplyr::rename(conf.low_95 = X2.5.., conf.high_95 = X97.5..)

results <- bind_cols(results, 
                     fit_cis_95) %>%
  dplyr::rename(Variable = term,
                Coefficient = estimate,
                SE = std.error) %>%
  filter(Variable != "(Intercept)")

results <- results %>% dplyr::select(-SE, 
                                     -statistic,
                                     -p.value)
results <- results %>%
  mutate(
    Treatments1 = case_when(
      Variable == "VignetteExp_Treatment1" ~ "Peaceful protest without blame",
      Variable == "VignetteExp_Treatment2" ~ "Peaceful protest with blame",
      Variable == "VignetteExp_Treatment3" ~ "Violent protest without blame",
      Variable == "VignetteExp_Treatment4" ~ "Violent protest with blame",
    )) %>%
  mutate(Treatments = factor(Treatments1, levels=c("Violent protest with blame",
                                                   "Violent protest without blame",
                                                   "Peaceful protest with blame", 
                                                   "Peaceful protest without blame")))

results %>% kable("html") %>% kable_styling(font_size = 10)

p <- ggplot(results, aes(x = Coefficient, y = Treatments)) +
  geom_point(size = 2, color = "royalblue4") +
  geom_errorbarh(aes(xmin = conf.low_95, xmax = conf.high_95), height = 0.2, color = "steelblue") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  labs(
    title = "I support people who protest against lack of service delivery",
    x = "",
    y = ""
  ) +
  theme_bw() + 
  theme(
    legend.position = "none",
    strip.text.y = element_text(size = 8, face = "bold"),
    panel.spacing = unit(1, "lines"),
    axis.text.y = element_text(size = 9.5),
    axis.title.x = element_text(size = 9.5)
  )
print(p)

# Appendix Table C4 - Regression tables
#-----------------------------------------------------------------

# Standard model
model1 <- lm(m3, 
             data = protest_SA)
summary(model1)

# Model with standard errors clustered at municipal level
model2 <- feols(m3, protest_SA,
                vcov = ~municipal_id)
summary(model2)


# Model with standard errors clustered at municipal level
model3 <- feols(m3, protest_SA,
                vcov = ~municipal_id,
                fixef = c("municipal_id"))
summary(model3)

texreg(list(model1, model2, model3))


##--------------------------------------------------------------------------------------------------------
## Outcome "Policy Support": The municipal govt should increase spending to improve service delivery in poor settlements
##--------------------------------------------------------------------------------------------------------

# Formulas
m1 <- Outcome1 ~ VignetteExp_Treatment1 + VignetteExp_Treatment2 + VignetteExp_Treatment3 + VignetteExp_Treatment4

# Figure 3 (b) - Coefficient plot
#-----------------------------------------------------------------

# Model with standard errors clustered at municipal level
model <- feols(m1, protest_SA,
               vcov = ~municipal_id,
               fixef = c("municipal_id"))
summary(model)

# Coefficient plot
results <- tidy(model)
fit_cis_95 <- confint(model, level = 0.95) %>% 
  data.frame() %>%
  dplyr::rename(conf.low_95 = X2.5.., conf.high_95 = X97.5..)

results <- bind_cols(results, 
                     fit_cis_95) %>%
  dplyr::rename(Variable = term,
                Coefficient = estimate,
                SE = std.error) %>%
  filter(Variable != "(Intercept)")

results <- results %>% dplyr::select(-SE, 
                                     -statistic,
                                     -p.value)
results <- results %>%
  mutate(
    Treatments1 = case_when(
      Variable == "VignetteExp_Treatment1" ~ "Peaceful protest without blame",
      Variable == "VignetteExp_Treatment2" ~ "Peaceful protest with blame",
      Variable == "VignetteExp_Treatment3" ~ "Violent protest without blame",
      Variable == "VignetteExp_Treatment4" ~ "Violent protest with blame",
    )) %>%
  mutate(Treatments = factor(Treatments1, levels=c("Violent protest with blame",
                                                   "Violent protest without blame",
                                                   "Peaceful protest with blame", 
                                                   "Peaceful protest without blame")))


results %>% kable("html") %>% kable_styling(font_size = 10)

p <- ggplot(results, aes(x = Coefficient, y = Treatments)) +
  geom_point(size = 2, color = "royalblue4") +
  geom_errorbarh(aes(xmin = conf.low_95, xmax = conf.high_95), height = 0.2, color = "steelblue") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  labs(
    title = "The municipal govt. should increase spending to improve service delivery in poor settlements",
    x = "",
    y = ""
  ) +
  theme_bw() + 
  theme(
    legend.position = "none",
    strip.text.y = element_text(size = 8, face = "bold"),
    panel.spacing = unit(1, "lines"),
    axis.text.y = element_text(size = 9.5),
    axis.title.x = element_text(size = 9.5)
  )
print(p)

# Appendix Table C3 - Regression tables 
#-----------------------------------------------------------------

# Standard model
model1 <- lm(m1, 
             data = protest_SA)
summary(model1)

# Model with standard errors clustered at municipal level
model2 <- feols(m1, protest_SA,
                vcov = ~municipal_id)
summary(model2)


# Model with standard errors clustered at municipal level
model3 <- feols(m1, protest_SA,
                vcov = ~municipal_id,
                fixef = c("municipal_id"))
summary(model3)

texreg(list(model1, model2, model3))

##--------------------------------------------------------------------------------------------------------
## Outcome "Public Disorder": People who protest against lack of service delivery are causing unnecessary public disorder
##--------------------------------------------------------------------------------------------------------
m4 <- Outcome4 ~ VignetteExp_Treatment1 + VignetteExp_Treatment2 + VignetteExp_Treatment3 + VignetteExp_Treatment4

# Appendix Figure C1
#-----------------------------------------------------------------

# Model with standard errors clustered at municipal level
model <- feols(m4, protest_SA,
               vcov = ~municipal_id,
               fixef = c("municipal_id"))
summary(model)

# Coefficient plot, ggplot2
results <- tidy(model)
fit_cis_95 <- confint(model, level = 0.95) %>% 
  data.frame() %>%
  dplyr::rename(conf.low_95 = X2.5.., conf.high_95 = X97.5..)

results <- bind_cols(results, 
                     fit_cis_95) %>%
  dplyr::rename(Variable = term,
                Coefficient = estimate,
                SE = std.error) %>%
  filter(Variable != "(Intercept)")

results <- results %>% dplyr::select(-SE, 
                                     -statistic,
                                     -p.value)
results <- results %>%
  mutate(
    Treatments1 = case_when(
      Variable == "VignetteExp_Treatment1" ~ "Peaceful protest without blame",
      Variable == "VignetteExp_Treatment2" ~ "Peaceful protest with blame",
      Variable == "VignetteExp_Treatment3" ~ "Violent protest without blame",
      Variable == "VignetteExp_Treatment4" ~ "Violent protest with blame",
      
    )) %>%
  mutate(Treatments = factor(Treatments1, levels=c("Violent protest with blame",
                                                   "Violent protest without blame",
                                                   "Peaceful protest with blame", 
                                                   "Peaceful protest without blame")))
results %>% kable("html") %>% kable_styling(font_size = 10)

p <- ggplot(results, 
       aes(x = Treatments, y = Coefficient)) +
  geom_hline(yintercept = 0, 
             colour = gray(1/2), lty = 2) +
  geom_point(aes(x = Treatments, 
                 y = Coefficient)) + 
  geom_linerange(aes(x = Treatments, 
                     ymin = conf.low_95,
                     ymax = conf.high_95),
                 lwd = 1) +
  labs(Treatments="Treatments",
       title="People who protest against lack of service delivery are causing unnecessary public disorder") +
  coord_flip()
print(p)

# Regression tables
#-----------------------------------------------------------------

# Standard model
model1 <- lm(m4, 
             data = protest_SA)
summary(model1)

# Model with standard errors clustered at municipal level
model2 <- feols(m4, protest_SA,
                vcov = ~municipal_id)
summary(model2)


# Model with standard errors clustered at municipal level
model3 <- feols(m4, protest_SA,
                vcov = ~municipal_id,
                fixef = c("municipal_id"))
summary(model3)


texreg(list(model1, model2, model3))


##-----------------------------------------------------------------------------------------------------------
## Outcome "Deservingness": People who protest against lack of service delivery deserve help from the municipal government
##-----------------------------------------------------------------------------------------------------------

# Formula
m5 <- Outcome5 ~ VignetteExp_Treatment1 + VignetteExp_Treatment2 + VignetteExp_Treatment3 + VignetteExp_Treatment4

# Appendix Figure C1
#-----------------------------------------------------------------
# Model with standard errors clustered at municipal level
model <- feols(m5, protest_SA,
               vcov = ~municipal_id,
               fixef = c("municipal_id"))
summary(model)


# Coefficient plot, ggplot2
results <- tidy(model)
fit_cis_95 <- confint(model, level = 0.95) %>% 
  data.frame() %>%
  dplyr::rename(conf.low_95 = X2.5.., conf.high_95 = X97.5..)

results <- bind_cols(results, 
                     fit_cis_95) %>%
  dplyr::rename(Variable = term,
                Coefficient = estimate,
                SE = std.error) %>%
  filter(Variable != "(Intercept)")

results <- results %>% dplyr::select(-SE, 
                                     -statistic,
                                     -p.value)
results <- results %>%
  mutate(
    Treatments1 = case_when(
      Variable == "VignetteExp_Treatment1" ~ "Peaceful protest without blame",
      Variable == "VignetteExp_Treatment2" ~ "Peaceful protest with blame",
      Variable == "VignetteExp_Treatment3" ~ "Violent protest without blame",
      Variable == "VignetteExp_Treatment4" ~ "Violent protest with blame",
      
    )) %>%
  mutate(Treatments = factor(Treatments1, levels=c("Violent protest with blame",
                                                   "Violent protest without blame",
                                                   "Peaceful protest with blame", 
                                                   "Peaceful protest without blame")))

results %>% kable("html") %>% kable_styling(font_size = 10)

p <- ggplot(results, 
       aes(x = Treatments, y = Coefficient)) +
  geom_hline(yintercept = 0, 
             colour = gray(1/2), lty = 2) +
  geom_point(aes(x = Treatments, 
                 y = Coefficient)) + 
  geom_linerange(aes(x = Treatments, 
                     ymin = conf.low_95,
                     ymax = conf.high_95),
                 lwd = 1) +
  labs(Treatments="Treatments",
       title="People who protest against lack of service delivery deserve help from the municipal government") +
  coord_flip()
print(p)

# Regression tables
#-----------------------------------------------------------------

# Standard model
model1 <- lm(m5, 
             data = protest_SA)
summary(model1)

# Model with standard errors clustered at municipal level
model2 <- feols(m5, protest_SA,
                vcov = ~municipal_id)
summary(model2)


# Model with standard errors clustered at municipal level
model3 <- feols(m5, protest_SA,
                vcov = ~municipal_id,
                fixef = c("municipal_id"))
summary(model3)


texreg(list(model1, model2, model3))

# Appendix Figure C2 - Metro vs. Other
#-----------------------------------------------------------------
subgroups <- unique(protest_SA_municipal_df$metro)

models <- lapply(subgroups, function(g) {
  feols(m3,
        data = protest_SA_municipal_df %>% filter(metro == g),
        vcov = ~municipal_id,
        fixef = c("municipal_id"))
})

names(models) <- subgroups

names(models)
# Brug broom::tidy til at lave dataframe
tidy_models <- lapply(models, function(m) tidy(m, conf.int = TRUE))
names(tidy_models) <- names(models)

# Tilføj gruppeattribut
for (g in names(tidy_models)) {
  tidy_models[[g]]$group <- g
}

# Saml i ét dataset
df_plot <- do.call(rbind, tidy_models)
df_plot <- df_plot %>%
  mutate(
    term = case_when(
      term == "VignetteExp_Treatment1" ~ "Peaceful protest without blame",
      term == "VignetteExp_Treatment2" ~ "Peaceful protest with blame",
      term == "VignetteExp_Treatment3" ~ "Violent protest without blame",
      term == "VignetteExp_Treatment4" ~ "Violent protest with blame"
    )
  ) %>%
  mutate(group = case_when(
    group == "1" ~ "Metropolitan municipalities",
    group == "0" ~ "Other municipalities"
  ))


p <- ggplot(df_plot, aes(x = estimate, y = term)) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  facet_wrap(~ group) +
  labs(title = "",
       x = "",
       y = "") +
  theme_bw() + 
  theme(
    legend.position = "none",
    strip.text.y = element_text(size = 8, face = "bold"),
    panel.spacing = unit(1, "lines"),
    axis.text.y = element_text(size = 9.5),
    axis.title.x = element_text(size = 9.5)
  )
print(p)

##----------------------------------------------------------------------------##
##                            END OF SCRIPT 
##----------------------------------------------------------------------------##