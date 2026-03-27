# Alexander F. Gazmararian
# agazmararian@gmail.com

library(here)
library(tidyverse)

cty_incl <- readRDS(here("Data", "inter", "19_wrp", "countries_include.rds"))

vdem <- readRDS(here("Data", "input", "country", "vdem", "V-Dem-CY-Core-v13.rds"))
vdem <- subset(vdem, year == 2018)
vdem <- subset(vdem, select = c(country_name, v2x_polyarchy, v2x_freexp_altinf, v2x_frassoc_thick, v2xel_frefair))
vdem$country_name[vdem$country_name == "United States of America"] <- "United States"
vdem$country_name[vdem$country_name == "The Gambia"] <- "Gambia"
vdem$country_name[vdem$country_name == "Burma/Myanmar"] <- "Myanmar"
vdem$country_name[vdem$country_name == "Bosnia and Herzegovina"] <- "Bosnia Herzegovina"
vdem$country_name[vdem$country_name == "Republic of the Congo"] <- "Congo Brazzaville"
vdem <- vdem %>% filter(country_name %in% c(cty_incl$country, "Palestine/Gaza", "Palestine/West Bank"))

vdem$dem_qtl <- cut(
  vdem$v2x_polyarchy, 
  c(-Inf, quantile(vdem$v2x_polyarchy, c(1 / 5, 2 / 5, 3 / 5, 4 / 5), na.rm = TRUE), Inf),
  labels = c("Q1", "Q2", "Q3", "Q4", "Q5")
  )

vdem$dem_terc <- cut(
  vdem$v2x_polyarchy, 
  c(-Inf, quantile(vdem$v2x_polyarchy, c(1 / 3, 2 / 3), na.rm = TRUE), Inf),
  labels = c("Q1", "Q2", "Q3")
  )

vdem$dem_qtr <- cut(
  vdem$v2x_polyarchy, 
  c(-Inf, quantile(vdem$v2x_polyarchy, c(1 / 4, 1 / 2, 3 / 4), na.rm = TRUE), Inf),
  labels = c("Q1", "Q2", "Q3", "Q4")
  )

vdem$freexp_qtl <- cut(
  vdem$v2x_freexp_altinf, 
  c(-Inf, quantile(vdem$v2x_freexp_altinf, c(1 / 4, 1 / 2, 3 / 4), na.rm = TRUE), Inf),
  labels = c("Q1", "Q2", "Q3", "Q4")
)

vdem$freexp_5 <- cut(
  vdem$v2x_freexp_altinf, 
  c(-Inf, quantile(vdem$v2x_freexp_altinf, c(1 / 5, 2 / 5, 3 / 5, 4 / 5), na.rm = TRUE), Inf),
  labels = c("Q1", "Q2", "Q3", "Q4", "Q5")
)

vdem$freexp_terc <- cut(
  vdem$v2x_freexp_altinf, 
  c(-Inf, quantile(vdem$v2x_freexp_altinf, c(1 / 3, 2 / 3), na.rm = TRUE), Inf),
  labels = c("Q1", "Q2", "Q3")
)

vdem$frefair_qtl <- cut(
  vdem$v2xel_frefair, 
  c(-Inf, quantile(vdem$v2xel_frefair, c(1 / 4, 1 / 2, 3 / 4), na.rm = TRUE), Inf),
  labels = c("Q1", "Q2", "Q3", "Q4")
)

vdem$frefair_terc <- cut(
  vdem$v2xel_frefair, 
  c(-Inf, quantile(vdem$v2xel_frefair, c(1 / 3, 2 / 3), na.rm = TRUE), Inf),
  labels = c("Q1", "Q2", "Q3")
)

vdem$frassoc_qtl <- cut(
  vdem$v2x_frassoc_thick, 
  c(-Inf, quantile(vdem$v2x_frassoc_thick, c(1 / 4, 1 / 2, 3 / 4), na.rm = TRUE), Inf),
  labels = c("Q1", "Q2", "Q3", "Q4")
)

saveRDS(vdem, here("Data", "inter", "19_wrp", "vdem_country.rds"))
