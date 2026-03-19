# Aim: create features for the modeling process

# packages to be installed if not already installed 
list.of.packages <- c("ggplot2","dplyr","caret","zoo","RcppRoll")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,repos = "http://cran.us.r-project.org")
# load packages
lapply(list.of.packages, library, character.only = TRUE)

# 2. Features preparation
# load data generated from data_and_map.R
d <- get(load(paste0('results/terrordata/terrordata',mm,'.Rdata')))

#make sure the id of polygons are as character
for (i in 1: length(d))
{
d[[i]]$id <- as.character(d[[i]]$id)
}
 

# Add a week identifyier column
d_weekid <- lapply(seq_along(d), function(i) cbind(d[[i]], week = i))

# Combine data into one dataframe
d_df <- do.call(rbind, d_weekid)
rm(d,d_weekid)
d <- d_df
rm(d_df)

# Generate temporally-lagged features
# lag week-1 features

x_out <- rep(NA, length(d$id))

for(id in unique(d$id)){
  x <- d$terror[d$id == id]
  x <- c(NA, x[seq_len(length(x) - 1)])
  x_out[d$id == id] <- x
}

d$terrorl1 <- x_out

# lag week-2 features
x_out <- rep(NA, length(d$id))

for(id in unique(d$id)){
  x <- d$terror[d$id == id]
  x <- c(NA, NA,  x[seq_len(length(x) - 2)])
  x_out[d$id == id] <- x
}

d$terrorl2 <- x_out

# lag week-3 features
for(id in unique(d$id)){
  x <- d$terror[d$id == id]
  x <- c(NA, NA, NA,  x[seq_len(length(x) - 3)])
  x_out[d$id == id] <- x
}

d$terrorl3 <- x_out

# lag week-4 features
for(id in unique(d$id)){
  x <- d$terror[d$id == id]
  x <- c(NA, NA, NA, NA, x[seq_len(length(x) - 4)])
  x_out[d$id == id] <- x
}

d$terrorl4 <- x_out

# lag month-1 features
d$terrorl_month <- d$terrorl1 + d$terrorl2 + d$terrorl3 + d$terrorl4

# lagged features at country-level
library(RcppRoll)
library(dplyr)
nat <- d %>%
  dplyr::filter(!is.na(country)) %>%
  dplyr::group_by(week, country) %>%
  dplyr::summarise(total_terror = sum(terror,na.rm = TRUE)) %>%
  dplyr::group_by(country) %>%
  dplyr::mutate(
    total_terror_l1 = lag(total_terror, 1),
    total_terror_l2 = lag(total_terror, 2),
    total_terror_l3 = lag(total_terror, 3),
    total_terror_l4 = lag(total_terror, 4),
    total_terror_l52 = lag(total_terror, 52),
    total_terror_month = lag(roll_sum(total_terror, 4, align = 'right', fill = NA), 1),
    total_terror_change = lag(
      rollapply(total_terror, 8, align = 'right', fill = NA,
                FUN = function(x) lm.fit(as.matrix(cbind(1, x)), 1:8)$coef[2]),
      1) %>% ifelse(is.na(.), 0, .)
  ) %>%
  dplyr::select(-total_terror)

d <- d %>%
  dplyr::left_join(nat, by = c('country', 'week'))


# Building feature time from latest terrorist event

# Most recent event
for(id in unique(d$id)){
  x <- d$terror[d$id == id]
  x2 <- sapply(seq(2, length(x)), function(i) (i - max(which(x[1:(i-1)] > 0))) %>% ifelse(is.infinite(.), 500, .))
  x2 <- c(NA, x2)
  x2[x2 > 500] <- 500
  x_out[d$id == id] <- x2
}

d$time_since_terror <- x_out

# Spatio temporal
# identify the number of polygons in the discretized study area
npoly<-nrow(terrordata[[1]])
# temporally lagged (week-1) terrorist events within ~100km
dist_mat <- as.matrix(dist(d[1:npoly, c('x', 'y')]))
# # check distance matrix
# image(dist_mat < 1)

n <- dist_mat < 1


for(id in unique(d$id)){
  d2 <- d[d$id %in% unique(d$id)[n[as.numeric(factor(id)),]], ]
  #use as.numeric(levels(id))[id] instead of as.numeric(id) if the variable is a factor (not a character)
  d2$week <- rep(1:(nrow(d2) /  length(unique(d2$id))), each = length(unique(d2$id)))
  x <- d2 %>% 
    dplyr:: group_by(week) %>% 
    dplyr:: summarise(x = sum(terror)) %>% 
    dplyr:: pull(x)
  x <- lag(x, 1)
  x_out[d$id == id] <- x
}


d$terror_100km_l1 <- x_out

# temporally lagged (week-1) terrorist events within ~100km and in the same country
dist_mat <- as.matrix(dist(d[1:length(unique(d$id)), c('x', 'y')]))

# #check distance matrix
# image(dist_mat < 1)

n <- dist_mat < 1

n_country <- n
for(i in 1:nrow(n_country)){
  n_country[i, ] <- d$country[i] == d$country[1:nrow(n_country)]
}

# save the features
write.csv(d, file = paste0('results/featextract/terror_feature_extraction',mm,'.csv'),row.names = FALSE)
# End