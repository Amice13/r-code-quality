#### Israel populism 2023 ####
### caution!: Run this before conjoint_Israel2023 ####

library(readr)
rm(list=ls())

data1 <- read_csv("IsraelSurvey2023Populismdata.csv")
data1 <- data1[-1, ]

library(psych)
library(GPArotation)

# Extract populism-related questions from the dataset
dat2<-data1[34:42]
head(dat2)


## Construct a populism score
data1$pop_score <- rowMeans(dat2, na.rm = TRUE)
data1$pop_z <- scale(data1$pop_score)

## sub-dimensions of populism
data1$anti_elite <- rowMeans(data1[, c("Q9_1","Q9_2","Q9_3")])
data1$people     <- rowMeans(data1[, c("Q9_4","Q9_5","Q9_6")])
data1$manichean  <- rowMeans(data1[, c("Q9_7","Q9_8","Q9_9")])


## # Merge with the conjoint data and create a new dataset
cj <- read.csv("IsraelSurvey2023populismdata.csv")

## additional variables
newvars <- data1[, c("pop_z", "pop_score",
                     "anti_elite", "people", "manichean")]

## Match the number of rows with the conjoint data
newvars <- rbind(
  rep(NA, ncol(newvars)),
  newvars
)


## Determine the insertion point
pos <- match("Q21_1", names(cj))
## Insert the column immediately before the conjoint variables
cj_new <- cbind(cj[, 1:(pos-1)],
                newvars,
                cj[, pos:ncol(cj)])

library(dplyr)

cj_new <- cj_new %>%
  mutate(religiosity = Q8, .after = Q8) %>%
  mutate(Bibi = Q11_1, .after=Q11_1) %>%
  mutate(Likud = Q10_1, .after=Q10_1) %>%
  mutate(ideology = Q14_13, .after=Q14_13)



## Saving data
write.csv(cj_new,
          "IsraelSurvey2023populismdata_new.csv",
          row.names = FALSE,
          na = "")



