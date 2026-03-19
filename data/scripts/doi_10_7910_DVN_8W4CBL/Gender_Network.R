# Step 0: Data loading

library(readr)

## Unzip "Gender_Netwokr.zip" file first. Use "read_tsv" command to load "Gender_Network.tsv", and save it as "total_analysis"

## Variable names

Actor_ID: nconst
Career_Years_(Panel): career_duration_actor
Degree_Centrality: Degree
Betweenness_Centrality: Betweenness
Closeness_Centrality: Closeness
Eigenvector_Centrality: Eigenvector 

Gender: gender
Age: age
Cumulative_Sum_of_Films: movies_actor   
Avg.Credit_Order:  averageBilling_year
Avg.number_of_Regions: averageGlobality_year
Avg.number_of_Ratings: averageVotes_year 
Total_number_of_Ratings: n_votes
Total_number_of_Acting_Roles: n_acting
Total_number_of_Films_Released: n_movies
  
TimeDummies_1920s: startYear_1920s
TimeDummies_1930s: startYear_1930s
TimeDummies_1940s: startYear_1940s
TimeDummies_1950s: startYear_1950s
TimeDummies_1960s: startYear_1960s
TimeDummies_1970s: startYear_1970s
TimeDummies_1980s: startYear_1980s
TimeDummies_1990s: startYear_1990s
TimeDummies_2000s: startYear_2000s
TimeDummies_2010s: startYear_2010s



# Step 1: Variable standardization


total_analysis$Degree <- scale(total_analysis$Degree)
total_analysis$Betweenness <- scale(total_analysis$Betweenness)
total_analysis$Closeness <- scale(total_analysis$Closeness)
total_analysis$Eigenvector <- scale(total_analysis$Eigenvector)

total_analysis$age <- scale(total_analysis$age)
total_analysis$movies_actor <- scale(total_analysis$movies_actor)
total_analysis$averageBilling_year <- scale(total_analysis$averageBilling_year)
total_analysis$averageGlobality_year <- scale(total_analysis$averageGlobality_year)
total_analysis$averageVotes_year <- scale(total_analysis$averageVotes_year)

total_analysis$n_votes <- scale(total_analysis$n_votes)
total_analysis$n_acting <- scale(total_analysis$n_acting)
total_analysis$n_movies <- scale(total_analysis$n_movies)



## Step 2-1: Baseline Random Effects Models 

library(plm)


randomZ_D  <- plm( Degree      ~  gender  + age + movies_actor   
                   + averageBilling_year + averageGlobality_year + averageVotes_year      
                   + n_votes  + n_acting + n_movies
                   , data=total_analysis, index=c("nconst", "career_duration_actor"), model="random")  

randomZ_BT  <- plm(Betweenness      ~ gender  + age + movies_actor    
                   + averageBilling_year + averageGlobality_year + averageVotes_year     
                   + n_votes  + n_acting + n_movies  
                   , data=total_analysis, index=c("nconst", "career_duration_actor"), model="random")  


randomZ_CL  <- plm(Closeness      ~ gender  + age + movies_actor      
                   + averageBilling_year + averageGlobality_year + averageVotes_year     
                   + n_votes  + n_acting + n_movies  
                   , data=total_analysis, index=c("nconst", "career_duration_actor"), model="random")  


randomZ_EV  <- plm(Eigenvector      ~  gender + age  + movies_actor      
                   + averageBilling_year + averageGlobality_year + averageVotes_year    
                   + n_votes  + n_acting + n_movies   
                   , data=total_analysis, index=c( "nconst", "career_duration_actor"), model="random")  


RSE_randomZ_D     <- sqrt(diag( vcovHC(randomZ_D , method="white2", type="HC4",  cluster = "group" )) )
RSE_randomZ_BT    <- sqrt(diag( vcovHC(randomZ_BT, method="white2", type="HC4",  cluster = "group" )) )
RSE_randomZ_CL    <- sqrt(diag( vcovHC(randomZ_CL, method="white2", type="HC4",  cluster = "group" )) )
RSE_randomZ_EV    <- sqrt(diag( vcovHC(randomZ_EV, method="white2", type="HC4",  cluster = "group" )) )


summary(randomZ_D,  vcov = function(x) vcovHC(x, method="white2", type="HC4",  cluster = "group" ))
summary(randomZ_BT, vcov = function(x) vcovHC(x, method="white2", type="HC4",  cluster = "group" ))
summary(randomZ_CL, vcov = function(x) vcovHC(x, method="white2", type="HC4",  cluster = "group" ))
summary(randomZ_EV, vcov = function(x) vcovHC(x, method="white2", type="HC4",  cluster = "group" ))





## Step 2-2: Random Effects Models with Time Dummies

library(plm)

randomZD_D  <- plm(Degree      ~  gender + age + movies_actor   
                   + averageBilling_year + averageGlobality_year + averageVotes_year    
                   + n_votes  + n_acting + n_movies
                   + startYear_1940s + startYear_1950s + startYear_1960s + startYear_1970s + startYear_1980s + startYear_1990s + startYear_2000s + startYear_2010s
                   , data=total_analysis, index=c("nconst", "career_duration_actor"), model="random")  

randomZD_BT  <- plm(Betweenness      ~ gender+ age  + movies_actor   
                    + averageBilling_year + averageGlobality_year + averageVotes_year     
                    + n_votes  + n_acting + n_movies  
                    + startYear_1940s + startYear_1950s + startYear_1960s + startYear_1970s + startYear_1980s + startYear_1990s + startYear_2000s + startYear_2010s
                    , data=total_analysis, index=c("nconst", "career_duration_actor"), model="random")  


randomZD_CL  <- plm(Closeness      ~ gender + age + movies_actor    
                    + averageBilling_year + averageGlobality_year + averageVotes_year     
                    + n_votes + n_acting + n_movies  
                    + startYear_1940s + startYear_1950s + startYear_1960s + startYear_1970s + startYear_1980s + startYear_1990s + startYear_2000s + startYear_2010s
                    , data=total_analysis, index=c("nconst", "career_duration_actor"), model="random")  


randomZD_EV  <- plm(Eigenvector      ~ gender + age + movies_actor      
                    + averageBilling_year + averageGlobality_year + averageVotes_year    
                    + n_votes  + n_acting + n_movies   
                    + startYear_1940s + startYear_1950s + startYear_1960s + startYear_1970s + startYear_1980s + startYear_1990s + startYear_2000s + startYear_2010s
                    , data=total_analysis, index=c( "nconst", "career_duration_actor"), model="random")    

RSE_randomZD_D     <- sqrt(diag( vcovHC(randomZD_D , method="white2", type="HC4",  cluster = "group" )) )
RSE_randomZD_BT    <- sqrt(diag( vcovHC(randomZD_BT, method="white2", type="HC4",  cluster = "group" )) )
RSE_randomZD_CL    <- sqrt(diag( vcovHC(randomZD_CL, method="white2", type="HC4",  cluster = "group" )) )
RSE_randomZD_EV    <- sqrt(diag( vcovHC(randomZD_EV, method="white2", type="HC4",  cluster = "group" )) )


summary(randomZD_D , vcov = function(x) vcovHC(x, method="white2", type="HC4",  cluster = "group" ))
summary(randomZD_BT, vcov = function(x) vcovHC(x, method="white2", type="HC4",  cluster = "group" ))
summary(randomZD_CL, vcov = function(x) vcovHC(x, method="white2", type="HC4",  cluster = "group" ))
summary(randomZD_EV, vcov = function(x) vcovHC(x, method="white2", type="HC4",  cluster = "group" ))





## Step 2-3: Random Effects Models with Time Dummies and Debut Genres

library(plm)

randomZDG_D  <- plm(Degree      ~  gender   + age + movies_actor 
                    + averageBilling_year + averageGlobality_year + averageVotes_year    
                    + n_votes  + n_acting + n_movies
                    + startYear_1940s + startYear_1950s + startYear_1960s + startYear_1970s + startYear_1980s + startYear_1990s + startYear_2000s + startYear_2010s
                    + D_drama + D_comedy + D_horror + D_action + D_thriller + D_romance + D_crime + D_adventure
                    , data=total_analysis, index=c("nconst", "career_duration_actor"), model="random")  


randomZDG_BT  <- plm(Betweenness      ~ gender  + age + movies_actor   
                     + averageBilling_year + averageGlobality_year + averageVotes_year     
                     + n_votes  + n_acting + n_movies  
                     + startYear_1940s + startYear_1950s + startYear_1960s + startYear_1970s + startYear_1980s + startYear_1990s + startYear_2000s + startYear_2010s
                     + D_drama + D_comedy + D_horror + D_action + D_thriller + D_romance + D_crime + D_adventure
                     , data=total_analysis, index=c("nconst", "career_duration_actor"), model="random")  


randomZDG_CL  <- plm(Closeness      ~ gender  + age + movies_actor  
                     + averageBilling_year + averageGlobality_year + averageVotes_year     
                     + n_votes  + n_acting + n_movies  
                     + startYear_1940s + startYear_1950s + startYear_1960s + startYear_1970s + startYear_1980s + startYear_1990s + startYear_2000s + startYear_2010s
                     + D_drama + D_comedy + D_horror + D_action + D_thriller + D_romance + D_crime + D_adventure
                     , data=total_analysis, index=c("nconst", "career_duration_actor"), model="random")  


randomZDG_EV  <- plm(Eigenvector      ~  gender + age  + movies_actor      
                     + averageBilling_year + averageGlobality_year + averageVotes_year     
                     + n_votes  + n_acting + n_movies   
                     + startYear_1940s + startYear_1950s + startYear_1960s + startYear_1970s + startYear_1980s + startYear_1990s + startYear_2000s + startYear_2010s
                     + D_drama + D_comedy + D_horror + D_action + D_thriller + D_romance + D_crime + D_adventure
                     , data=total_analysis, index=c( "nconst", "career_duration_actor"), model="random")    


RSE_randomZDG_D     <- sqrt(diag( vcovHC(randomZDG_D , method="white2", type="HC4",  cluster = "group" )) )
RSE_randomZDG_BT    <- sqrt(diag( vcovHC(randomZDG_BT, method="white2", type="HC4",  cluster = "group" )) )
RSE_randomZDG_CL    <- sqrt(diag( vcovHC(randomZDG_CL, method="white2", type="HC4",  cluster = "group" )) )
RSE_randomZDG_EV    <- sqrt(diag( vcovHC(randomZDG_EV, method="white2", type="HC4",  cluster = "group" )) )


summary(randomZDG_D,  vcov = function(x) vcovHC(x, method="white2", type="HC4",  cluster = "group" ))
summary(randomZDG_BT, vcov = function(x) vcovHC(x, method="white2", type="HC4",  cluster = "group" ))
summary(randomZDG_CL, vcov = function(x) vcovHC(x, method="white2", type="HC4",  cluster = "group" ))
summary(randomZDG_EV, vcov = function(x) vcovHC(x, method="white2", type="HC4",  cluster = "group" ))




## Step 2-4: Pooled Models

library(plm)

poolZ_D  <- plm(Degree          ~  gender + age + movies_actor   
                + averageBilling_year + averageGlobality_year + averageVotes_year     
                + n_votes  + n_acting + n_movies
                , data=total_analysis, index=c("nconst", "career_duration_actor"), model = "pooling")  

poolZ_BT  <- plm(Betweenness      ~ gender  + age  + movies_actor  
                 + averageBilling_year + averageGlobality_year + averageVotes_year      
                 + n_votes  + n_acting + n_movies  
                 , data=total_analysis, index=c("nconst", "career_duration_actor"), model= "pooling")  


poolZ_CL  <- plm(Closeness      ~ gender + age + movies_actor      
                 + averageBilling_year + averageGlobality_year + averageVotes_year    
                 + n_votes + n_acting + n_movies  
                 , data=total_analysis, index=c("nconst", "career_duration_actor"), model="pooling")  


poolZ_EV  <- plm(Eigenvector      ~ gender + age  + movies_actor      
                 + averageBilling_year + averageGlobality_year + averageVotes_year   
                 + n_votes  + n_acting + n_movies   
                 , data=total_analysis, index=c( "nconst", "career_duration_actor"), model="pooling")  


poolZD_D  <- plm(Degree      ~  gender + age + movies_actor   
                 + averageBilling_year + averageGlobality_year + averageVotes_year    
                 + n_votes  + n_acting + n_movies
                 + startYear_1940s + startYear_1950s + startYear_1960s + startYear_1970s + startYear_1980s + startYear_1990s + startYear_2000s + startYear_2010s
                 , data=total_analysis, index=c("nconst", "career_duration_actor"), model = "pooling")  

poolZD_BT  <- plm(Betweenness      ~ gender  + age  + movies_actor  
                  + averageBilling_year + averageGlobality_year + averageVotes_year      
                  + n_votes + n_acting + n_movies  
                  + startYear_1940s + startYear_1950s + startYear_1960s + startYear_1970s + startYear_1980s + startYear_1990s + startYear_2000s + startYear_2010s
                  , data=total_analysis, index=c("nconst", "career_duration_actor"), model= "pooling")  


poolZD_CL  <- plm(Closeness      ~ gender + age + movies_actor      
                  + averageBilling_year + averageGlobality_year + averageVotes_year     
                  + n_votes  + n_acting + n_movies  
                  + startYear_1940s + startYear_1950s + startYear_1960s + startYear_1970s + startYear_1980s + startYear_1990s + startYear_2000s + startYear_2010s
                  , data=total_analysis, index=c("nconst", "career_duration_actor"), model="pooling")  


poolZD_EV  <- plm(Eigenvector      ~ gender + age  + movies_actor      
                  + averageBilling_year + averageGlobality_year + averageVotes_year      
                  + n_votes + n_acting + n_movies   
                  + startYear_1940s + startYear_1950s + startYear_1960s + startYear_1970s + startYear_1980s + startYear_1990s + startYear_2000s + startYear_2010s
                  , data=total_analysis, index=c( "nconst", "career_duration_actor"), model="pooling")  

poolZDG_D  <- plm(Degree      ~  gender + age + movies_actor   
                  + averageBilling_year + averageGlobality_year + averageVotes_year    
                  + n_votes  + n_acting + n_movies
                  + startYear_1940s + startYear_1950s + startYear_1960s + startYear_1970s + startYear_1980s + startYear_1990s + startYear_2000s + startYear_2010s
                  + D_drama + D_comedy + D_horror + D_action + D_thriller + D_romance + D_crime + D_adventure
                  , data=total_analysis, index=c("nconst", "career_duration_actor"), model = "pooling")  

poolZDG_BT  <- plm(Betweenness      ~ gender  + age  + movies_actor  
                   + averageBilling_year + averageGlobality_year + averageVotes_year      
                   + n_votes  + n_acting + n_movies  
                   + startYear_1940s + startYear_1950s + startYear_1960s + startYear_1970s + startYear_1980s + startYear_1990s + startYear_2000s + startYear_2010s
                   + D_drama + D_comedy + D_horror + D_action + D_thriller + D_romance + D_crime + D_adventure
                   , data=total_analysis, index=c("nconst", "career_duration_actor"), model= "pooling")  


poolZDG_CL  <- plm(Closeness      ~ gender + age + movies_actor      
                   + averageBilling_year + averageGlobality_year + averageVotes_year    
                   + n_votes  + n_acting + n_movies  
                   + startYear_1940s + startYear_1950s + startYear_1960s + startYear_1970s + startYear_1980s + startYear_1990s + startYear_2000s + startYear_2010s
                   + D_drama + D_comedy + D_horror + D_action + D_thriller + D_romance + D_crime + D_adventure
                   , data=total_analysis, index=c("nconst", "career_duration_actor"), model="pooling")  


poolZDG_EV  <- plm(Eigenvector      ~ gender + age  + movies_actor      
                   + averageBilling_year + averageGlobality_year + averageVotes_year    
                   + n_votes + n_acting + n_movies   
                   + startYear_1940s + startYear_1950s + startYear_1960s + startYear_1970s + startYear_1980s + startYear_1990s + startYear_2000s + startYear_2010s
                   + D_drama + D_comedy + D_horror + D_action + D_thriller + D_romance + D_crime + D_adventure
                   , data=total_analysis, index=c( "nconst", "career_duration_actor"), model="pooling")  


RSE_poolZ_D      <- sqrt(diag( vcovHC(poolZ_D   , method="white2", type="HC4",  cluster = "group" )) )
RSE_poolZ_BT     <- sqrt(diag( vcovHC(poolZ_BT  , method="white2", type="HC4",  cluster = "group" )) )
RSE_poolZ_CL     <- sqrt(diag( vcovHC(poolZ_CL  , method="white2", type="HC4",  cluster = "group" )) )
RSE_poolZ_EV     <- sqrt(diag( vcovHC(poolZ_EV  , method="white2", type="HC4",  cluster = "group" )) )
RSE_poolZD_D     <- sqrt(diag( vcovHC(poolZD_D  , method="white2", type="HC4",  cluster = "group" )) )
RSE_poolZD_BT    <- sqrt(diag( vcovHC(poolZD_BT , method="white2", type="HC4",  cluster = "group" )) )
RSE_poolZD_CL    <- sqrt(diag( vcovHC(poolZD_CL , method="white2", type="HC4",  cluster = "group" )) )
RSE_poolZD_EV    <- sqrt(diag( vcovHC(poolZD_EV , method="white2", type="HC4",  cluster = "group" )) )
RSE_poolZDG_D    <- sqrt(diag( vcovHC(poolZDG_D , method="white2", type="HC4",  cluster = "group" )) )
RSE_poolZDG_BT   <- sqrt(diag( vcovHC(poolZDG_BT, method="white2", type="HC4",  cluster = "group" )) )
RSE_poolZDG_CL   <- sqrt(diag( vcovHC(poolZDG_CL, method="white2", type="HC4",  cluster = "group" )) )
RSE_poolZDG_EV   <- sqrt(diag( vcovHC(poolZDG_EV, method="white2", type="HC4",  cluster = "group" )) )



summary(poolZ_D,     vcov = function(x) vcovHC(x, method="white2", type="HC4",  cluster = "group" ))
summary(poolZ_BT,    vcov = function(x) vcovHC(x, method="white2", type="HC4",  cluster = "group" ))
summary(poolZ_CL,    vcov = function(x) vcovHC(x, method="white2", type="HC4",  cluster = "group" ))
summary(poolZ_EV,    vcov = function(x) vcovHC(x, method="white2", type="HC4",  cluster = "group" ))
summary(poolZD_D,    vcov = function(x) vcovHC(x, method="white2", type="HC4",  cluster = "group" ))
summary(poolZD_BT,   vcov = function(x) vcovHC(x, method="white2", type="HC4",  cluster = "group" ))
summary(poolZD_CL,   vcov = function(x) vcovHC(x, method="white2", type="HC4",  cluster = "group" ))
summary(poolZD_EV,   vcov = function(x) vcovHC(x, method="white2", type="HC4",  cluster = "group" ))
summary(poolZDG_D,   vcov = function(x) vcovHC(x, method="white2", type="HC4",  cluster = "group" ))
summary(poolZDG_BT,  vcov = function(x) vcovHC(x, method="white2", type="HC4",  cluster = "group" ))
summary(poolZDG_CL,  vcov = function(x) vcovHC(x, method="white2", type="HC4",  cluster = "group" ))
summary(poolZDG_EV,  vcov = function(x) vcovHC(x, method="white2", type="HC4",  cluster = "group" ))



## Step 3: Printing Model Results

library(stargazer)


list_randomZ   = list("Degree" = randomZ_D, "Closeness" = randomZ_CL, "Betweeness" = randomZ_BT, "Eigenvector" = randomZ_EV ) 
list_randomZD  = list("Degree" = randomZD_D, "Closeness" = randomZD_CL, "Betweeness" = randomZD_BT, "Eigenvector" = randomZD_EV ) 
list_randomZDG = list("Degree" = randomZDG_D, "Closeness" = randomZDG_CL, "Betweeness" = randomZDG_BT, "Eigenvector" = randomZDG_EV ) 

list_randomZ_SE   = list(RSE_randomZ_D  , RSE_randomZ_CL  , RSE_randomZ_BT  , RSE_randomZ_EV  ) 
list_randomZD_SE  = list(RSE_randomZD_D , RSE_randomZD_CL , RSE_randomZD_BT , RSE_randomZD_EV ) 
list_randomZDG_SE = list(RSE_randomZDG_D, RSE_randomZDG_CL, RSE_randomZDG_BT, RSE_randomZDG_EV) 



list_poolZ = list("Degree" = poolZ_D,   "Closeness" = poolZ_CL,   "Betweeness" = poolZ_BT,   "Eigenvector" = poolZ_EV,
                  "Degree" = poolZD_D,  "Closeness" = poolZD_CL,  "Betweeness" = poolZD_BT,  "Eigenvector" = poolZD_EV,
                  "Degree" = poolZDG_D, "Closeness" = poolZDG_CL, "Betweeness" = poolZDG_BT, "Eigenvector" = poolZDG_EV  ) 

list_poolZ_SE = list(RSE_poolZ_D  , RSE_poolZ_CL  , RSE_poolZ_BT  , RSE_poolZ_EV,   
                     RSE_poolZD_D  , RSE_poolZD_CL  , RSE_poolZD_BT  , RSE_poolZD_EV,
                     RSE_poolZDG_D  , RSE_poolZDG_CL  , RSE_poolZDG_BT  , RSE_poolZDG_EV)


stargazer(list_randomZ  , se=list_randomZ_SE  , type = "html", out="randomZ.html",   no.space = TRUE, star.cutoffs = c(0.05, 0.01, 0.001))
stargazer(list_randomZD , se=list_randomZD_SE , type = "html", out="randomZD.html",  no.space = TRUE, star.cutoffs = c(0.05, 0.01, 0.001))
stargazer(list_randomZDG, se=list_randomZDG_SE, type = "html", out="randomZDG.html", no.space = TRUE, star.cutoffs = c(0.05, 0.01, 0.001))
stargazer(list_poolZ,     se=list_poolZ_SE,     type = "html", out="poolZ.html",     no.space = TRUE, star.cutoffs = c(0.05, 0.01, 0.001))

