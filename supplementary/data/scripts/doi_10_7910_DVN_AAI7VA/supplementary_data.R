"""
This script contains sample code for reading in and working with supplementary
data from paper: (1) statistics on ideological homophily and (2) audience
alignment. This code requires `R`, as well as the `dplyr`, `reshape2`, and
`foreach` packages. It has been tested with `R` version 3.1.0, `dplyr` 0.4.1,
`reshape` 1.4, and `foreach` 1.4.2.
"""

library(dplyr)
library(foreach)
library(reshape2)

### Working with ideological homophily distribution
d <- read.csv('frac_op_densities.csv')
homophily.cdf <- d %>%
  filter(
    (viewer.affiliation=='Conservatives' & friend.affiliation=='Liberal friends') |
    (viewer.affiliation=='Liberals' & friend.affiliation=='Conservative friends')
  ) %>%
  group_by(viewer.affiliation, friend.affiliation) %>%
  arrange(frac.friends) %>%
  mutate(cum.density=cumsum(density))
  
nearest.percentile <- function(df, percentile) {
  homophily.cdf %>%
    mutate(
      dist=abs(cum.density-percentile),
      approx.percentile=percentile
    ) %>%
    filter(dist==min(dist))
}

percentiles <- foreach(p=c(0.05, 0.25, 0.5, 0.75, 0.95), .combine=rbind) %do% {
  nearest.percentile(homophily.cdf, p)
}

percentiles %>%
  arrange(viewer.affiliation) %>%
  select(viewer.affiliation, friend.affiliation, approx.percentile, frac.friends)


### Working with site-level audience alignment data
top500 <- read.csv('top500.csv') %>% arrange(avg_align)
head(top500, 10)
tail(top500, 10)