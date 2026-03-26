## Hager / Hilbig - Inheritance replication code - Feb 26 2019
## hhilbig@g.harvard.edu
##
## This file : Prep for the geographic matching procedure 
## Tables reproduced in this file: N/A
##
### ### ### ###

rm(list = ls())

## Packages

library(dplyr)
library(pbapply)
library(Matching)
library(fields)

## First, declare the column classes

## Load the main data

inher <- read.csv('Data_Main.csv')

#### Matching ####
## We match on longitude and latitude - this minimizes the physical distance between two municipalities

# One-to-one matching with replacement (the "M=1" option).

rr <- Match(Y = NULL, 
            Tr = inher$fair_dic, 
            X = cbind(inher$lon, inher$lat), 
            M = 1, replace = T)

## Check  the output

summary(rr)

## Now, we look at the distances between matches

t_id_repl = rr$index.treated
c_id_repl = rr$index.control

## Distance matrix

dm <- fields::rdist.earth(cbind(inher$lon, inher$lat))

## Get distances between matches in km

dist_list <- pbsapply(1:length(t_id_repl), function(i) dm[t_id_repl[i], c_id_repl[i]])
summary(dist_list); stats::quantile(dist_list, seq(0,1,0.2))

## Obtain AGS

t_id_repl_ags <- inher$AGS[t_id_repl]
c_id_repl_ags <- inher$AGS[c_id_repl]

## Make a data frame to save the matches

match_repl_dist <- data.frame(t_ags = t_id_repl_ags, c_ags = c_id_repl_ags,
                              dist_km = dist_list, stringsAsFactors = F)

## Save this - this file will be used in 'Matching_Analysis.R'

save(match_repl_dist, 
     file = "Matches.RData")
