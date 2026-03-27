

#### Packages that are needed ####
packagesUsed <- c("tidyverse", "MatchIt", "rgenoud")
                  
sapply(packagesUsed, library, character.only = TRUE)   



#### load dataset"

df <- read.csv("Data4.csv")



L <- c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE) 

 m1 <- matchit(female ~ newid + v2x_gender + v2x_liberal + diplomat + left + 
                        right + religious + nationalist + legor_uk + legor_so + 
                        legor_fr +appointmentyear               ,
                      data = df,
                      method="genetic",
                      unif.seed = 336890,
                      wait.generations= 1,
               max.generations=10,
                      ratio = 1,
                      pop.size = 1000,
                      replace = TRUE,
                  exact = L,
                      ties = TRUE)
 save(m1,
            file = "GeneticMatchesJanuary4final.RData")
