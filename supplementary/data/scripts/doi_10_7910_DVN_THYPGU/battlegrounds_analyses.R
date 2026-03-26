#### NOTES:

## This is the code to reproduce the figures that appear in the paper, using the communes data.
# Regarding variable names: I need to fix the names of the main IVs to make them more intuitive, 
# but for the meantime, here's what they mean:

# mindist_10000 = distance to the closest battlefield with < 50,000 French casualties associated
# bid_10000 = battlefield ID of the closest battlefield with < 50,000 French casualties 
# french_casualties_10000 = number of casualties associated with the closest battlefield with < 50,000 French casualties
# battlefield_community_10000 = dummy, coded as 1 if mindist_10000 < 20

# mindist_50000 = distance to the closest battlefield with 50,000-99,999 French casualties associated
# mindist_100000 = distance to the closest battlefield with 100,000-199,999 French casualties associated
# mindist_200000 = distance to the closest battlefield with >200,000 French casualties associated

## Let me know if anything else is unclear

library(tidyverse)
library(geosphere)
library(readxl)
library(writexl)
library(estimatr)
library(sjPlot)
library(betareg)
library(stringr)
library(stringi)
library(maps)
library(reporttools)
library(stargazer)
library(coefplot)
library(modelsummary)
library(fixest)

setwd("/Users/samselsky/Library/Mobile Documents/com~apple~CloudDocs/Documents/Papers/Battlefields and contemporary voting")
communes <- read_excel("communes.xlsx")

battlefield_depts_10k <- filter(communes, dept_code == 80 | dept_code == 02| dept_code == 68
                                | dept_code == 59| dept_code == 62)
battlefield_depts_50k <- filter(communes, dept_code == 80 | dept_code == 62 
                                | dept_code == 51| dept_code == 02)
battlefield_depts_100k <- filter(communes, dept_code == 62 | dept_code == 51 | dept_code == 02)
battlefield_depts_200k <- filter(communes, dept_code == 55 | dept_code == 62 |dept_code == 80)
battlefield_depts <- filter(communes, dept_code == 80 | dept_code == 02| dept_code == 68
                            | dept_code == 59| dept_code == 62| dept_code == 51| dept_code == 55)
close <- filter(communes, mindist_10000<50|mindist_50000<50|mindist_100000<50|mindist_200000<50)
## Each of these datasets include the departments that have at least one battlefield within the range of casualties.
## The "battlefield_depts" contains all of these.

## Descriptive statistics ##
stats <- subset(battlefield_depts, select=c(farright_2022_votes, mindist_10000, mindist_50000, mindist_100000, mindist_200000,
                                   population, foreigner_percent_commune, elevation, density, percent_university_degree_commune,
                                   unemployment_commune, industry_employment_commune, under_40_percent, mindist_fort))
vars <- as.data.frame(stats[, 1:ncol(stats)])
tableContinuous(vars = vars, stats = c("n", "mean", "s", "min", 
                                       "max", "na"), print.pval = "kruskal", 
                cap = "Table of continuous variables.", lab = "tab: descr stat")

## Covariate balance table ## 
battlefield_depts_test <- battlefield_depts_200k[complete.cases(battlefield_depts_200k),]
m.out <- MatchIt::matchit(battlefield_community_200000 ~ density1911 + pop1911 + elevation
                          + mindist_fort, family="binomial", data = battlefield_depts_test, 
                          method = "nearest",
                          replace = TRUE)
new.names <- c("1911 population density",
               "1911 population",
               "elevation",
               "minimum distance to military fort")
df <- as.data.frame(summary(m.out)$sum.all)[-1,]
rownames(df) <- new.names
df <- df %>% 
  mutate(difference = `Means Treated` - `Means Control`) %>% 
  mutate(commune_characteristic = rownames(df)) %>% 
  .[,-(4:7)] %>% 
  relocate(commune_characteristic) %>% 
  relocate(`Std. Mean Diff.`, .after= difference)
xtable::xtable(df)

## Regressions ## 

# distance as main IV
close$department <- close$dept_code
mod1 <- lm_robust(lepen_2022_votes ~ mindist_10000 + elevation + pop1911 + 
                    density1911 + mindist_fort, data = close)
summary(mod1) 
mod2 <- lm_robust(lepen_2022_votes ~ mindist_50000 + elevation + pop1911 + 
                    density1911 + mindist_fort, data = close)
summary(mod2) 
mod3 <- lm_robust(lepen_2022_votes ~ mindist_100000 + elevation + pop1911 + 
                    density1911 + mindist_fort, data = close)
summary(mod3) 
mod4 <- lm_robust(lepen_2022_votes ~ mindist_200000 + elevation + pop1911 + 
                    density1911 + mindist_fort, data = close)
summary(mod4) 
mod5 <- feols(lepen_2022_votes ~ mindist_200000 + elevation + pop1911 + 
                density1911 + mindist_fort | department, data = close)
summary(mod5) 

mod1 <- lm_robust(farright_2022_votes ~ mindist_10000 + elevation + pop1911 + 
             density1911 + mindist_fort, data = close)
summary(mod1) 
mod2 <- lm_robust(farright_2022_votes ~ mindist_50000 + elevation + pop1911 + 
             density1911 + mindist_fort, data = close)
summary(mod2) 
mod3 <- lm_robust(farright_2022_votes ~ mindist_100000 + elevation + pop1911 + 
             density1911 + mindist_fort, data = close)
summary(mod3) 
mod4 <- lm_robust(farright_2022_votes ~ mindist_200000 + elevation + pop1911 + 
             density1911 + mindist_fort, data = close)
summary(mod4) 
mod5 <- feols(farright_2022_votes ~ mindist_200000 + elevation + pop1911 + 
                density1911 + mindist_fort | department, data = close)
summary(mod5) 

# adjustments to modelsummary: #
exclude_vars <- c("elevation", "pop1911", "density1911", "mindist_fort")
newrow <- data.frame(matrix(nrow=1, ncol=6))
newrow[,1] <- "social/geographic controls"
newrow[, 2:6] <- "X"

# results table:
modelsummary(list(mod1, mod2, mod3, mod4, mod5), 
             exclude = c(exclude_vars, "Intercept"),
             coef_map = list(
               "mindist_10000" = "Distance to Battlefield: < 50,000 casualties", 
               "mindist_50000" = "Distance to Battlefield: 50,000-99,999 casualties", 
               "mindist_100000" = "Distance to Battlefield: 100,000-199,999 casualties", 
               "mindist_200000" = "Distance to Battlefield: ≥ 200,000 casualties"), 
             stars = TRUE, 
             star_method = "multi",
             title = "Table 2: Distance from Battlefields and far right voteshare, 2022",
             header = c("", "Model 1", "Model 2", "Model 3", "Model 4", "Model 5"), 
             gof_omit = 'DF|Deviance|AIC|R2 Within|BIC|Log.Lik.',
             fmt=5,
             add_rows=newrow)

# 'battlefield community' dummy as main IV
mod1 <- lm_robust(farright_2022_votes ~ battlefield_community_10000 + elevation + pop1911 + 
             density1911 + mindist_fort, data = close)
summary(mod1) 
mod2 <- lm_robust(farright_2022_votes ~ battlefield_community_50000 + elevation + pop1911 + 
             density1911 + mindist_fort, data = close)
summary(mod2) 
mod3 <- lm_robust(farright_2022_votes ~ battlefield_community_100000 + elevation + pop1911 + 
             density1911 + mindist_fort, data = close)
summary(mod3) 
mod4 <- lm_robust(farright_2022_votes ~ battlefield_community_200000 + elevation + pop1911 + 
             density1911 + mindist_fort, data = close)
summary(mod4) 
mod5 <- feols(farright_2022_votes ~ battlefield_community_200000 + elevation + pop1911 + 
                density1911 + mindist_fort | department, data = close) 
summary(mod5)

modelsummary(list(mod1, mod2, mod3, mod4, mod5), 
             exclude = c(exclude_vars, "Intercept"),
             coef_map = list(
               "battlefield_community_10000" = "Battlefield Community: < 50,000 casualties", 
               "battlefield_community_50000" = "Battlefield Community: 50,000-99,999 casualties", 
               "battlefield_community_100000" = "Battlefield Community: 100,000-199,999 casualties", 
               "battlefield_community_200000" = "Battlefield Community: ≥ 200,000 casualties"), 
             stars = TRUE, 
             star_method = "multi",
             title = "Table 3: 'Battlefield Communities' and far right voteshare, 2022",
             header = c("", "Model 1", "Model 2", "Model 3", "Model 4", "Model 5"), 
             gof_omit = 'DF|Deviance|AIC|R2 Within|BIC|Log.Lik.',
             add_rows=newrow)

coefplot::multiplot(mod1, mod2, mod3, mod4, coefficients=c("battlefield_community_10000", 
                                                           "battlefield_community_50000", 
                                                 "battlefield_community_100000", 
                                                 "battlefield_community_200000"),
          xlab = "Predicted vote share change",ylab = "Casualty range", legend.position = "none",
          newNames=c(battlefield_community_10000="< 50,000", battlefield_community_50000="50,000-99,999",
 battlefield_community_100000="100,000-199,999", battlefield_community_200000="≥ 200,000")) + 
  labs(title="Figure 2: 'Battlefield Communities' and predicted far right voteshare, 2022, 
       by casualty range") + theme_bw() 

## Robustness checks, to go in appendices ##

# logged distance as main IV
mod1 <- lm(farright_2022_votes ~ log(mindist_10000)  + elevation + pop1911 + 
             density1911 + mindist_fort, data = close)
summary(mod1) 
mod2 <- lm(farright_2022_votes ~ log(mindist_50000)  + elevation + pop1911 + 
             density1911 + mindist_fort, data = close)
summary(mod2) 
mod3 <- lm(farright_2022_votes ~ log(mindist_100000) + elevation + pop1911 + 
             density1911 + mindist_fort, data = close)
summary(mod3) 
mod4 <- lm(farright_2022_votes ~ log(mindist_200000) + elevation + pop1911 + 
             density1911 + mindist_fort, data = close)
summary(mod4) 

# adjustments to modelsummary: #
newrow1 <- data.frame(matrix(nrow=1, ncol=5))
newrow1[,1] <- "social/geographic controls"
newrow1[, 2:5] <- "X"

modelsummary(list(mod1, mod2, mod3, mod4), 
             exclude = c(exclude_vars, "Intercept"),
             coef_map = list(
               "log(mindist_10000)" = "Log distance to Battlefield: < 50,000 casualties", 
               "log(mindist_50000)" = "Log distance to Battlefield: 50,000-99,999 casualties", 
               "log(mindist_100000)" = "Log distance to Battlefield: 100,000-199,999 casualties", 
               "log(mindist_200000)" = "Log distance to Battlefield: ≥ 200,000 casualties"), 
             stars = TRUE, 
             star_method = "multi",
             title = "Table 4: Logged distance from Battlefields and far right voteshare, 2022",
             header = c("", "Model 1", "Model 2", "Model 3", "Model 4"), 
             gof_omit = 'DF|Deviance|AIC|R2 Within|BIC|Log.Lik.',
             add_rows=newrow1)


# 'battlefield depts' dataset - battlefield community as main IV

mod1 <- lm(farright_2022_votes ~ battlefield_community_10000 + elevation + pop1911 + 
             density1911 + mindist_fort, data = battlefield_depts)
summary(mod1) 
mod2 <- lm(farright_2022_votes ~ battlefield_community_50000 + elevation + pop1911 + 
             density1911 + mindist_fort, data = battlefield_depts)
summary(mod2) 
mod3 <- lm(farright_2022_votes ~ battlefield_community_100000 + elevation + pop1911 + 
             density1911 + mindist_fort, data = battlefield_depts)
summary(mod3) 
mod4 <- lm(farright_2022_votes ~ battlefield_community_200000 + elevation + pop1911 + 
             density1911 + mindist_fort, data = battlefield_depts)
summary(mod4) 
mod5 <- feols(farright_2022_votes ~ battlefield_community_200000 + elevation + pop1911 + 
                density1911 + mindist_fort | dept_code, data = battlefield_depts) 
summary(mod5)

stargazer(mod1,mod2,mod3,mod4,  se = starprep(mod1,mod2,mod3,mod4), type="text")


##Matching method
library(MatchIt)
library(marginaleffects)
m.out0 <- matchit(battlefield_community_200000 ~ elevation + pop1911 + 
                    density1911 + mindist_fort, data = battlefield_depts_test,
                  method = "nearest", distance = "glm")
summary(m.out0)
plot(summary(m.out0))
m.data <- match.data(m.out0)

fit <- lm(farright_2022_votes ~ battlefield_community_200000 * (elevation + pop1911 + 
                                                   density1911 + mindist_fort), 
          data = m.data, weights = weights)

avg_comparisons(fit,
                variables = "battlefield_community_200000",
                vcov = ~subclass,
                newdata = subset(m.data, battlefield_community_200000 == 1),
                wts = "weights")





# randomization test (regressing main IV on controls):
mod1 <- lm(battlefield_community_200000 ~ elevation + pop1911 + 
            density1911 + mindist_fort, data = battlefield_depts_200k)
summary(mod1) 
mod2 <- lm(battlefield_community_200000 ~ density + foreigner_percent_commune + 
               unemployment_commune + percent_university_degree_commune + population + 
               industry_employment_commune + elevation + mindist_fort, data = battlefield_depts_200k)
summary(mod2) 
modelsummary(list("pre-WWI demographic covariates"=mod1, "modern demographic covariates"=mod2),
             coef_map = list(
               "elevation" = "Elevation", 
               "pop1911" = "1911 population", 
               "density1911" = "1911 population density",
               "mindist_fort" = "Km to nearest military fort",
               "population" = "2017 population",
               "density" = "2017 population density",
               "foreigner_percent_commune" = "Foreign-born population %",
               "percent_university_degree_commune" = "% population with uni. degree",
               "unemployment_commune" = "Unemployment",
               "industry_employment_commune" = "Industrial employment rate",
               "mindist_200000" = "Distance to Battlefield: ≥ 200,000 casualties"), 
             stars = TRUE,
             star_method = "multi",
             title = "Table 1: Commune characteristics and battlefield locations",
             output = "latex",
             gof_omit = 'DF|Deviance|AIC|R2 Within|BIC|Log.Lik.|R2 Adj.|R2|RMSE')




#### For journals: including full sets of controls: ####

modelsummary(list(mod1, mod2, mod3, mod4, mod5), 
             coef_map = list(
               "mindist_10000" = "Distance to Battlefield: < 50,000 casualties", 
               "mindist_50000" = "Distance to Battlefield: 50,000-99,999 casualties", 
               "mindist_100000" = "Distance to Battlefield: 100,000-199,999 casualties", 
               "mindist_200000" = "Distance to Battlefield: ≥ 200,000 casualties",
               "elevation" ="Elevation",
               "pop1911" = "1911 population",
               "density1911" = "1911 population density",
               "mindist_fort" = "Distance to nearest fort"), 
             stars = TRUE, 
             star_method = "multi",
             title = "Table 2: Distance from Battlefields and Le Pen voteshare",
             header = c("", "Model 1", "Model 2", "Model 3", "Model 4", "Model 5"), 
             gof_omit = 'DF|Deviance|AIC|R2 Within|BIC|Log.Lik.',
             output="latex")

modelsummary(list(mod1, mod2, mod3, mod4, mod5), 
             coef_map = list(
               "battlefield_community_10000" = "Battlefield Community: < 50,000 casualties", 
               "battlefield_community_50000" = "Battlefield Community: 50,000-99,999 casualties", 
               "battlefield_community_100000" = "Battlefield Community: 100,000-199,999 casualties", 
               "battlefield_community_200000" = "Battlefield Community: ≥ 200,000 casualties",
               "elevation" ="Elevation",
               "pop1911" = "1911 population",
               "density1911" = "1911 population density",
               "mindist_fort" = "Distance to nearest fort"), 
             stars = TRUE, 
             star_method = "multi",
             title = "Table 3: 'Battlefield Communities' and Le Pen voteshare",
             header = c("", "Model 1", "Model 2", "Model 3", "Model 4", "Model 5"), 
             gof_omit = 'DF|Deviance|AIC|R2 Within|BIC|Log.Lik.',
             output="latex")








### Sensitivity analysis ###

coefficients <- numeric()

# Create an empty vector to store p-values
p_values <- numeric()

# Define a vector of distance thresholds
distance_thresholds <- seq(20, 100, by = 1)

# Create an empty data frame to store results
results <- data.frame(Distance = numeric(), Coefficient = numeric(), PValue = numeric(), Highlight = character())

# Loop through each distance threshold
for (threshold in distance_thresholds){
  # Subset the data based on the threshold
  subset_data <- communes[communes$mindist_200000 < threshold, ]
  
  # Run the linear regression model
  mod <- lm(farright_2022_votes ~ mindist_200000 + elevation + pop1911 + density1911 + mindist_fort, data = subset_data)
  
  # Save the coefficient for mindist_200000
  coefficient <- coef(mod)["mindist_200000"]
  
  # Save the p-value for mindist_200000
  p_value <- summary(mod)$coefficients["mindist_200000", "Pr(>|t|)"]
  
  # Determine if the coefficient should be highlighted based on the p-value
  highlight <- ifelse(p_value < 0.05, "p < .05", "p > .05")
  
  # Store the results in the data frame
  results <- rbind(results, data.frame(Distance = threshold, Coefficient = coefficient, PValue = p_value, Highlight = highlight))
}

# Plot the coefficients against distances with conditional formatting
ggplot(results, aes(x = Distance, y = Coefficient)) +
  geom_line() +
  geom_point(aes(color = Highlight)) +
  scale_color_manual(values = c("p < .05" = "red")) +
  labs(title = "Coefficient of 'Distance to Battlefield: ≥ 200,000 casualties' variable
       vs. Distance Threshold",
       x = "Distance Threshold",
       y = "Coefficient of 'Distance to Battlefield: ≥ 200,000 casualties' variable") +
  theme(legend.title = element_blank()) + theme_bw()


## Mutliple elections:

## 2017:
mod1 <- lm(voteshare_lepen ~ mindist_10000 + elevation + pop1911 + 
             density1911 + mindist_fort, data = close)
summary(mod1) 
mod2 <- lm(voteshare_lepen ~ mindist_50000 + elevation + pop1911 + 
             density1911 + mindist_fort, data = close)
summary(mod2) 
mod3 <- lm(voteshare_lepen ~ mindist_100000 + elevation + pop1911 + 
             density1911 + mindist_fort, data = close)
summary(mod3) 
mod4 <- lm(voteshare_lepen ~ mindist_200000 + elevation + pop1911 + 
             density1911 + mindist_fort, data = close)
summary(mod4) 

modelsummary(list(mod1, mod2, mod3, mod4, mod5), 
             exclude = c(exclude_vars, "Intercept"),
             coef_map = list(
               "mindist_10000" = "Distance to Battlefield: < 50,000 casualties", 
               "mindist_50000" = "Distance to Battlefield: 50,000-99,999 casualties", 
               "mindist_100000" = "Distance to Battlefield: 100,000-199,999 casualties", 
               "mindist_200000" = "Distance to Battlefield: ≥ 200,000 casualties"), 
             stars = TRUE, 
             star_method = "multi",
             title = "Table 6: Distance from Battlefields and Le Pen voteshare, 2017",
             header = c("", "Model 1", "Model 2", "Model 3", "Model 4", "Model 5"), 
             gof_omit = 'DF|Deviance|AIC|R2 Within|BIC|Log.Lik.',
             add_rows=newrow)

## 2012:
mod1 <- lm(lepen_2012_votes ~ mindist_10000 + elevation + pop1911 + 
             density1911 + mindist_fort, data = close)
summary(mod1) 
mod2 <- lm(lepen_2012_votes ~ mindist_50000 + elevation + pop1911 + 
             density1911 + mindist_fort, data = close)
summary(mod2) 
mod3 <- lm(lepen_2012_votes ~ mindist_100000 + elevation + pop1911 + 
             density1911 + mindist_fort, data = close)
summary(mod3) 
mod4 <- lm(lepen_2012_votes ~ mindist_200000 + elevation + pop1911 + 
             density1911 + mindist_fort, data = close)
summary(mod4) 
mod5 <- feols(lepen_2012_votes ~ mindist_200000 + elevation + pop1911 + 
                density1911 + mindist_fort | department, data = close)
summary(mod5) 

modelsummary(list(mod1, mod2, mod3, mod4, mod5), 
             exclude = c(exclude_vars, "Intercept"),
             coef_map = list(
               "mindist_10000" = "Distance to Battlefield: < 50,000 casualties", 
               "mindist_50000" = "Distance to Battlefield: 50,000-99,999 casualties", 
               "mindist_100000" = "Distance to Battlefield: 100,000-199,999 casualties", 
               "mindist_200000" = "Distance to Battlefield: ≥ 200,000 casualties"), 
             stars = TRUE, 
             star_method = "multi",
             title = "Table 7: Distance from Battlefields and Le Pen voteshare, 2012",
             header = c("", "Model 1", "Model 2", "Model 3", "Model 4", "Model 5"), 
             gof_omit = 'DF|Deviance|AIC|R2 Within|BIC|Log.Lik.',
             add_rows=newrow)

## Somme only:
battlefield_depts_somme <- filter(communes, dept_code == 62 |dept_code == 80)

mod1 <- lm_robust(farright_2022_votes ~ dist_somme + elevation + pop1911 + 
                    density1911 + mindist_fort, data = battlefield_depts_somme)
summary(mod1) 

newrow1 <- data.frame(matrix(nrow=1, ncol=2))
newrow1[,1] <- "social/geographic controls"
newrow1[,2] <- "X"

modelsummary(mod1, 
             exclude = c(exclude_vars, "Intercept"),
             coef_map = list(
               "dist_somme" = "Distance to battle of Somme"), 
             stars = TRUE, 
             star_method = "multi",
             title = "Table 8: Distance from Somme battlefield and far right voteshare, 2022",
             gof_omit = 'DF|Deviance|AIC|R2 Within|BIC|Log.Lik.',
             add_rows=newrow1)

