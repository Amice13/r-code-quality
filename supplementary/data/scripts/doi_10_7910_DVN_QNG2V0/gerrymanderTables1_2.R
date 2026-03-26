sink(file="tables1_2_log.txt")

# Install and load packages
install.packages('xtable')

library(xtable)

# Set the Working Directory
#setwd("")

# Load the data
load("tc.RData")

#########################################
# Code to produce TABLE 1
#########################################
# Drop the states with only one district
g1 <- g[g$districts>1,]

# Create GOP and Dem gain differences
g1$gop_gain <- g1$gop_gerry - g1$proportional
g1$dem_gain <- g1$dem_gerry - g1$proportional

# Net seat gain is the sum of gop_gain (for Republicans) or 
# dem_gain (for Democrats)
abs(sum(g1$gop_gain))
abs(sum(g1$dem_gain))

# NET GOP ADVANTAGE
abs(sum(g1$gop_gain)) - abs(sum(g1$dem_gain))

r1 <- c("1. All states", round(abs(sum(g1$gop_gain)), 2), round(abs(sum(g1$dem_gain)), 2), round(abs(sum(g1$gop_gain)) - abs(sum(g1$dem_gain)), 2))

####
# Subset to states without non-partisan districting
g2 <- g1[g1$nonpartisan_redistricting==0,]

abs(sum(g2$gop_gain))
abs(sum(g2$dem_gain))

# NET GOP ADVANTAGE
abs(sum(g2$gop_gain)) - abs(sum(g2$dem_gain))

r2 <- c("2. States without non-partisan districting", round(abs(sum(g2$gop_gain)), 2), round(abs(sum(g2$dem_gain)), 2), round(abs(sum(g2$gop_gain)) - abs(sum(g2$dem_gain)), 2))

#### 
# Incorporate Republican advantage in controlling statehouses
abs(aggregate(gop_gain ~ reptrifecta, data = g2, FUN = sum)[2,2])
abs(aggregate(dem_gain ~ demtrifecta, data = g2, FUN = sum)[2,2])

r3 <- c("3. States in which party drew post-2010 districts", 
        round(abs(aggregate(gop_gain ~ reptrifecta, data = g2, FUN = sum)[2,2]), 2), 
        round(abs(aggregate(dem_gain ~ demtrifecta, data = g2, FUN = sum)[2,2]), 2), 
        round((abs(aggregate(gop_gain ~ reptrifecta, data = g2, FUN = sum)[2,2]) - abs(aggregate(dem_gain ~ demtrifecta, data = g2, FUN = sum)[2,2])), 2))

g2$blended_pos <- ifelse(g2$partisan_lean >0, 1, 0)
abs(aggregate(gop_gain ~ blended_pos, data = g2, FUN = sum)[2,2])
abs(aggregate(dem_gain ~ blended_pos, data = g2, FUN = sum)[1,2])

r4 <- c("4. States with partisan advantage", 
        round(abs(aggregate(gop_gain ~ blended_pos, data = g2, FUN = sum)[2,2]), 2), 
        round(abs(aggregate(dem_gain ~ blended_pos, data = g2, FUN = sum)[1,2]), 2),
        round(abs(aggregate(gop_gain ~ blended_pos, data = g2, FUN = sum)[2,2]) - abs(aggregate(dem_gain ~ blended_pos, data = g2, FUN = sum)[1,2]), 2))

tab_1 <- rbind(r1, r2, r3, r4)
print.xtable(xtable(tab_1, row.names = F), row.names = F)

rm(r1, r2, r3, r4, tab_1, g1, g2)

#########################################
# Code to produce TABLE 2
#########################################

# Limit to states with a Republican trifecta, more than one district, 
# and a partisan redistricting system
r <- g[g$reptrifecta==1 & g$districts > 1 & g$nonpartisan_redistricting == 0,]

r$max_advantage <- r$gop_gerry - r$proportional
r$actual_advantage <- r$current - r$proportional

r$difference <- r$max_advantage - r$actual_advantage

r <- r[order(r$difference),]
r <- r[c("state", 'max_advantage', 'actual_advantage', 'difference')]

# Print table
xtable(r, digits=1)

# Limit to states with a Democratic trifecta, more than one district, 
# and a partisan redistricting system
d <- g[g$demtrifecta==1 & g$districts > 1 & g$nonpartisan_redistricting == 0,]

d$max_advantage <- d$proportional - d$dem_gerry
d$actual_advantage <- d$proportional - d$current

d$difference <- d$max_advantage - d$actual_advantage

d <- d[order(d$difference),]
d <- d[c("state", 'max_advantage', 'actual_advantage', 'difference')]

# Print table
xtable(d, digits=1)
