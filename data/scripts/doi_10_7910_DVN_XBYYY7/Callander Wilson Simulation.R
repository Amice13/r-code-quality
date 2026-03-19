## Simulation of values in Callander & Wilson
# Author: Lucy Martin
# Date last updated: October 4, 2023

## This R script estimates the equilibria for the model presented in Callander & Wilson (2007), based on the same assumptions as made in that paper. We then use that replication of results to estimate the effects of compulsory voting on equilibrium levels of party polarization under different assumptions. 

# Set up:
rm(list=ls())
library(ggplot2)
library(dplyr)
setwd("~/Simulations")

# Notes:
## Callander & Wilson show that equilibria have several properties for the area of the parameter space we examine here. We use those properties to help us find the equilibrium party positions. These properties are:
  # In equilibrium, the entrant stays out.
  # However, the policies of the 2 main parties are such that if the entrant entered and chose a policy to maximize vote share, the best they could do is tie. 
  # The model set-up assumes that ties are always won by the dominant party, and so the entrant cannot win and thus does not enter.
  # In equilibrium, the 2 main parties will be equidistant from the median voters' preferred policy of 0: that is, we can characterize the equilibrium by some d* such that party 1 chooses -d*, and party 2 chooses d* as their ideal policies. 

# The code below produces a dataset that esimates, for each possible value of delta (the tolerance for alienation) in the range examined by C&W what equilibrium will emerge. We then use this to reproduce Figure 2 from C&W, and as the basis for the calculations of how an exogenous increase in the tolerance of alienation, driven by compulsory voting, would affect the equilibrium. 

# Each iteration of the loop proceeds as follows:
  # Step 1: Set delta
  # Step 2: for that value of delta, run a loop that estimates a complete contingent ideal strategy for the Entrant. To do this, we considers 1,000 possible value of d (the distance from the median voter set by the parties), and for each level of d find the ideal policy on the flank of the distribution for the Entrant (from a set of 1,000 possible strategies). This loop generates a dataset that has all of these maximizing values of the Entrant's policy, along with their expected vote share.
  # Step 3: We then expect each party to choose the policy that maximizes their expected vote share, knowing how the entrant will respond for each possible choice. This allows us to find the equilibrium level of policy divergence (i.e. polarization for that level of delta. )
  # In particular, we rely on the finding in C&W that, in equilibrium, if the entrant entered they would tie the election. We therefore find the point where the vote shares of the entrant and Party 1 are equal (because we set the Entrant's ideal policy to be on the flank of Party 2, we know that Party 1 will have higher vote share if the entrant entered.)
  # This gives us d*, the equilibrium distance from the median voter of each party in the election. 


# First, make 1) a vector of possible delta values and 2) empty dataset for graphing. Bounds on delta are same as those in Callander & Wilson
delta_seq <- seq(0,3.5,by=.0005)

# Next, ake empty dataset to store equilibrium results as a function of delta
tograph <- data.frame(matrix(ncol = 3, nrow = 7001))
colnames(tograph) <- c("delta", "dstar", "turnout")
tograph$delta <- delta_seq

## Run loop over all possible values of delta:

for (i in delta_seq) {
  delta <- i
  # Step 1: run a loop that finds e* for each value of d 
  # Make blank dataframe to use in loop
    df <- data.frame(matrix(ncol = 5, nrow = 1000))
    colnames(df) <- c("d", "e", "v_d1", "v_d2", "v_e")

  # Make blank dataframe for output
    output <- data.frame(matrix(ncol = 5, nrow = 1000))
    colnames(output) <- c("d", "v_d1", "max_ve", "e_atvemax", "e_atfoc")

  # Set range for d* to use in loop below
  range <- seq(0.0005,0.5,by=.0005)

  # For a given value of delta, run a loop finding ideal policy for E given any d in [0,0.5].
  for (x in range) {
    n <- 2000*x
    d <- x
    d2 <- x
    d1 <- -x
    df$e <- seq(d2+2*delta/1000,d2+2*delta,by=2*delta/1000)
    df$v_e <- plogis(df$e+delta, 0,2) - plogis((.5*(d2+df$e)), 0,2)
    df$dve <- dlogis(df$e+delta,2) - .5*dlogis(.5*(d2+df$e), 0,2)
    output[n,] <- c(d, x, max(df$v_e), df$e[df$v_e==max(df$v_e)], df$e[df$dve==min(df$dve)])
  }

  # Now, take the output dataframe and calculate v(D1) for each row:
    output$v_d1 <- 0.5-plogis(-output$d-delta, 0,2)

  # Find point where they are closest to tying the election:
    output$dif <- output$v_d1-output$max_ve

    maxd <- output$d[output$dif==min(abs(output$dif)) | -output$dif==min(abs(output$dif))]

# Find turnout at that point:
  # Turnout is everyone within delta of either party
  # So, need cdf of d1-delta to 0, plus cdf of 0 to d2+delta: Same as d1-delta to d2+delta
turnout <- plogis(maxd+delta, 0, 2) - plogis(-maxd-delta, 0, 2)

  # Set row of tograph dataset
    n <- (i+.0005)*2000
    tograph[n,] <- c(delta, maxd, turnout)
    ## D=.373 WHEN delta is 1.3.
  }

## END OF LOOP

# Save output of loop as .csv
write.csv(tograph, "cw_simdata.csv", row.names=FALSE)

# Graph output to show that the results accurately replicate Figure 2 of Callander & Wilson (2007): 
rm(list=ls())
tograph <- read.csv("cw_simdata.csv")

# Graph turnout: turn into 0-100 scale and graph
tograph$turnout <- tograph$turnout*100
turnout <- ggplot(tograph, aes(delta, turnout)) + geom_line()
turnout

# Graph d* :
policy <- ggplot(tograph, aes(delta, dstar)) + geom_line()
policy


## These graphs replicate the two parts of Figure 2 of Callander & Wilson (2007). 

### It now remains to use individual elements of this dataset to produce the estimates in Table 1 of our paper. 
# To do so, we use the elements of our output data to estimate the equilibrium behavior of all actors, given different levels of turnout. 

######
## Column 4 of Table 1: estimating the predicted decrease in polarization due to higher turnout, assuming baseline turnout of 40%

# Expected level of polarization when turnout is 40%

  # In our simulated data, the value of delta at which equilibrium turnout is closest to 40% is 1.3320
  # At this point, d* = 0.3630
  # Therefore, polarization is 2*.3630
  base_pol40 <- 2*0.3630
  
  # Now consider net turnout increases of 10pp, 8pp and 4 pp
  # 10 pp increase: If turnout is ~50%, d* is 0.1780
    inc10_pol40 <- 2*.1780
    # Gives us a decrease of 51%
    1-(inc10_pol40/base_pol40)
  ##  8 pp increase: if turnout is ~48%, d* is 0.2100
    inc8_pol40 <- 2*0.2100
    # Gives us a decrease in polarization of ~42%
    1-(inc8_pol40/base_pol40)
  ## 4 pp increase: if turnout is 44%, d* is ~0.2815
    # Gives us a decrease in polarization of ~22.4%
    inc4_pol40 <- 2*0.2815
    1-(inc4_pol40/base_pol40)
    
######
## Column 5 of Table 1: estmating the predicted decrease in polarization due to higher turnout, assuming baseline turnout of 50%
    
    
    # In our simulated data, the value of d* when equilibrium turnout is closest to 50% is 0.1780. Therefor baseline polarization is:
    base_pol50 <- 2*.1780

    
    # Now consider net turnout increases of 10pp, 8pp and 4 pp
    # 10 pp increase: If turnout is ~60%, d* is 0.0535
    inc10_pol50 <- 2*0.0535
    # Gives us a decrease in polarization of ~69.9%:
    1-(inc10_pol50/base_pol50)
    ##  8 pp increase: if turnout is ~58%, d* is ~0.0735
    inc8_pol50 <- 2*0.0735
    # Gives us a decrease in polarization of ~58.7%
    1-(inc8_pol50/base_pol50)
    ## 4 pp increase: if turnout is 54%, d* is 0.1210
    inc4_pol50 <- 2*0.1210
    # Gives us a decrease in polarization of ~32.0%
    1-(inc4_pol50/base_pol50)    
    
######
## Column 6 of Table 1: estimating the predicted decrease in polarization due to higher turnout, assuming baseline turnout of 60%
    
    # In our simulated data, the value of d* when equilibrium turnout is closest to 60% is 0.0535 Therefor baseline polarization is:
    base_pol60 <- 2*0.0535
    
    # Now consider net turnout increases of 10pp, 8pp and 4 pp
    # 10 pp increase: If turnout is ~70%, d* is ~0
      # This gives us a decrease in polarization of ~100%
    
    ##  8 pp increase: if turnout is ~68%, d* is 0.0040
    inc8_pol60 <- 2*0.0040
    # Gives us a decrease in polarization of ~92.5%
    1-(inc8_pol60/base_pol60)
    ## 4 pp increase: if turnout is 64%, d* is 0.0225
    inc4_pol60 <- 2*0.0225
    # Gives us a decrease in polarization of ~58%
    1-(inc4_pol60/base_pol60)    
    
    