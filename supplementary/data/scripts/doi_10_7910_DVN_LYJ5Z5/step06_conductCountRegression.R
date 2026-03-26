library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(performance)
library(MASS)
library(DHARMa)

root <- "C:/Users/Wieczorek_W_Station/Dropbox/Arbeit Kassel/paperideen/Moltbook_Science/Data/"
path <- paste0(root,"PreparedData")
output <- paste0(root,"Outputs")
figures <- paste0(root,"Figures")


setwd(path)
list.files()
df <- read.csv("ThreadsCountRegression.csv", sep = ";")

df <- df %>%
  rename_all(.,~str_replace_all(.,"\\.+","_"))
  
df %>%
  glimpse()

df %>%
  ggplot(aes(x = log(comment_count+1),
             y = log(upvotes+1),
             color = factor(identity_consciousness),
             hue = factor(identity_consciousness))) +
  geom_point() +
  geom_smooth()

# Calculate Poisson Regressions -------------------------------------------
resCommentsPoisson <- glm(comment_count ~ factor(grouped_sentiment) +
                        identity_consciousness +
                        STEM +
                        philosophy +
                        human_culture +
                        AI_auto_ethnography_and_sociology +
                        AI_scientific_infrastructure +
                        AI_sovereignty + 
                        economics +
                        technical_discussions_on_the_architecture_of_AI_agents +
                        X_discussions_on_malicious_content,
                        family = "poisson",
                      data = df)


resUpvotesPoisson <- glm(upvotes ~ comment_count + factor(grouped_sentiment) +
                       identity_consciousness +
                       STEM +
                       philosophy +
                       human_culture +
                       AI_auto_ethnography_and_sociology +
                       AI_scientific_infrastructure +
                       AI_sovereignty + 
                       economics +
                       technical_discussions_on_the_architecture_of_AI_agents +
                       X_discussions_on_malicious_content,
                       family = "poisson",
                     data = df)

check_overdispersion(resCommentsPoisson)
check_overdispersion(resUpvotesPoisson)

# Calculate Negative Binomial Regressions ---------------------------------
resComments <- glm.nb(comment_count ~ factor(grouped_sentiment) +
                        identity_consciousness +
                        STEM +
                        philosophy +
                        human_culture +
                        AI_auto_ethnography_and_sociology +
                        AI_scientific_infrastructure +
                        AI_sovereignty + 
                        economics +
                        technical_discussions_on_the_architecture_of_AI_agents +
                        X_discussions_on_malicious_content,
                   data = df)
performance(resComments)
resComments$theta

resUpvotes <- glm.nb(upvotes ~ comment_count + factor(grouped_sentiment) +
                        identity_consciousness +
                        STEM +
                        philosophy +
                        human_culture +
                        AI_auto_ethnography_and_sociology +
                        AI_scientific_infrastructure +
                        AI_sovereignty + 
                        economics +
                        technical_discussions_on_the_architecture_of_AI_agents +
                        X_discussions_on_malicious_content,
                      data = df)
summary(resUpvotes)
performance(resUpvotes)

# Ex-post tests for model assumptions -------------------------------------
AssumptionsResidualsComments <- simulateResiduals(resComments)
AssumptionsResidualsUpvotes <- simulateResiduals(resUpvotes)
plot(AssumptionsResidualsComments) 
plot(AssumptionsResidualsUpvotes) 

testZeroInflation(AssumptionsResidualsComments) ## zero deflated
testZeroInflation(AssumptionsResidualsUpvotes)


# Apply hurdle model for comments -----------------------------------------
resCommentsHurdle <- pscl::hurdle(comment_count ~ factor(grouped_sentiment) +
               identity_consciousness +
               STEM +
               philosophy +
               human_culture +
               AI_auto_ethnography_and_sociology +
               AI_scientific_infrastructure +
               AI_sovereignty + 
               economics +
               technical_discussions_on_the_architecture_of_AI_agents +
               X_discussions_on_malicious_content,
               dist = "negbin",
             data = df)
summary(resCommentsHurdle)
performance(resCommentsHurdle)

## fit only the positives of the hurdle part
df_pos <- df[df$comment_count > 0, ]

resHurdlePositive <- glm.nb(comment_count ~ factor(grouped_sentiment) +
         identity_consciousness +
         STEM +
         philosophy +
         human_culture +
         AI_auto_ethnography_and_sociology +
         AI_scientific_infrastructure +
         AI_sovereignty + 
         economics +
         technical_discussions_on_the_architecture_of_AI_agents +
         X_discussions_on_malicious_content,
       data = df_pos)

# Create table ------------------------------------------------------------
## Number of Comments
summaryComments <- summary(resComments)
effectsTable <- summaryComments$coefficients

variables <- rownames(effectsTable)
effectsTable <- as_tibble(effectsTable) %>%
  rename_all(.,~str_replace_all(.," +","_")) %>%
  rename_all(.,~str_replace_all(.,"\\.","")) %>%
  mutate(Estimate = round(Estimate,4),
         Std_Error = round(Std_Error,4)) %>%
  mutate(vars = variables, .before = Estimate)

effectsTable <-  effectsTable %>%
  mutate(Estimate = case_when(abs(z_value) >= 3.29 ~ paste0(round(Estimate,4),"***"),
                              abs(z_value) >= 2.56 ~ paste0(round(Estimate,4),"**"),
                              abs(z_value) >= 1.96 ~ paste0(round(Estimate,4),"*"),
                              TRUE ~ as.character(round(Estimate,4)))
  ) 

effectsTable <- effectsTable %>%
  dplyr::select(vars,Estimate,Std_Error)


ModelPerformance <- performance(resComments)
effectsTable <- effectsTable %>%
  add_row(vars = c("Obs","log. Likelihood","Theta","BIC","Nagelkerke R²"),
          Estimate = c("357",
                       as.character(round(resComments$twologlik,4)),
                       as.character(round(summaryComments$theta,4)), 
                       as.character(round(ModelPerformance$BIC),4),
                       as.character(round(ModelPerformance$R2_Nagelkerke,4))), 
          Std_Error = NA ) %>%
  add_row(vars = "comment_count",
          Estimate = NA_character_,
          Std_Error = NA, .after = 1)

TableComments <- effectsTable %>%
  rename(model1_Estimate = Estimate ,
         model1_Std = Std_Error)

## hurdle model
summaryComments <- summary(resCommentsHurdle)
effectsTable <- summaryComments$coefficients

variables <- rownames(effectsTable$count)
effectsTable <- as_tibble(effectsTable$count) %>%
  rename_all(.,~str_replace_all(.," +","_")) %>%
  rename_all(.,~str_replace_all(.,"\\.","")) %>%
  mutate(Estimate = round(Estimate,4),
         Std_Error = round(Std_Error,4)) %>%
  mutate(vars = variables, .before = Estimate)

effectsTable <-  effectsTable %>%
  mutate(Estimate = case_when(abs(z_value) >= 3.29 ~ paste0(round(Estimate,4),"***"),
                              abs(z_value) >= 2.56 ~ paste0(round(Estimate,4),"**"),
                              abs(z_value) >= 1.96 ~ paste0(round(Estimate,4),"*"),
                              TRUE ~ as.character(round(Estimate,4)))
  ) 

effectsTable <- effectsTable %>%
  dplyr::select(vars,Estimate,Std_Error)



ModelPerformance <- performance(resHurdlePositive)
effectsTable <- effectsTable %>%
  add_row(vars = c("Obs","log. Likelihood","Theta","BIC","Nagelkerke R²"),
          Estimate = c("357",
                       as.character(round(resComments$twologlik,4)),
                       as.character(round(summaryComments$theta,4)), 
                       as.character(round(ModelPerformance$BIC),4),
                       as.character(round(ModelPerformance$R2_Nagelkerke,4))), 
          Std_Error = NA ) %>%
  add_row(vars = "comment_count",
          Estimate = NA_character_,
          Std_Error = NA, .after = 1)

TableCommentsHurdle <- effectsTable %>%
  rename(model1Hurdle_Estimate = Estimate ,
         model1Hurdle_Std = Std_Error)


# Create table for upvotes ------------------------------------------------
summaryUpvotes <- summary(resUpvotes)
effectsTable <- summaryUpvotes$coefficients

variables <- rownames(effectsTable)
effectsTable <- as_tibble(effectsTable) %>%
  rename_all(.,~str_replace_all(.," +","_")) %>%
  rename_all(.,~str_replace_all(.,"\\.","")) %>%
  mutate(Estimate = round(Estimate,4),
         Std_Error = round(Std_Error,4)) %>%
  mutate(vars = variables, .before = Estimate)

effectsTable <-  effectsTable %>%
  mutate(Estimate = case_when(abs(z_value) >= 3.29 ~ paste0(round(Estimate,4),"***"),
                              abs(z_value) >= 2.56 ~ paste0(round(Estimate,4),"**"),
                              abs(z_value) >= 1.96 ~ paste0(round(Estimate,4),"*"),
                              TRUE ~ as.character(round(Estimate,4)))
         ) 

effectsTable <- effectsTable %>%
  dplyr::select(vars,Estimate,Std_Error)

ModelPerformance <- performance(resUpvotes)
effectsTable <- effectsTable %>%
          add_row(vars = c("Obs","log. Likelihood","Theta","BIC","Nagelkerke R²"),
                  Estimate = c("357",
                       as.character(round(resUpvotes$twologlik,4)),
                       as.character(round(summaryUpvotes$theta,4)), 
                       as.character(round(ModelPerformance$BIC),4),
                       as.character(round(ModelPerformance$R2_Nagelkerke,4))), 
          Std_Error = NA )

TablePerformance <- effectsTable %>%
  rename(model2_Estimate = Estimate ,
         model2_Std = Std_Error)


## Combine the results
TableResults <- TableComments %>%
  left_join(TableCommentsHurdle, by = "vars") %>%
  left_join(TablePerformance, by = "vars")

setwd(output)
write.table(TableResults, file = "Results.csv", sep = ";", row.names = FALSE, dec = ".")


# Calculate E^Coefficient -------------------------------------------------
exp(resHurdlePositive$coefficients)
exp(resUpvotes$coefficients)

