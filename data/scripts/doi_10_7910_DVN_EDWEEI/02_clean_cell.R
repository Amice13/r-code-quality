# Alexander F. Gazmararian
# agazmararian@gmail.com

library(data.table)
library(here)
library(tidyverse)

co2 <- fread(here("Data", "input", "grid", "coord", "co2.csv"))
names(co2) <- "co2"

wage <- fread(here("Data", "input", "grid", "coord", "wage.csv"))
names(wage) <- "wage"

pop <- fread(here("Data", "input", "grid", "coord", "pop.csv"))
names(pop) <- "pop"

coord <- readRDS(here("Data", "inter", "grid", "coord.rds"))
coord <- coord %>% dplyr::arrange(cell_index)

cell <- cbind(coord, wage, pop, co2)
c_sum <- cell %>%
  dplyr::group_by(iso3c) %>%
  dplyr::summarise(
    c_wage = sum(wage),
    c_pop = sum(pop),
    c_co2 = sum(co2)
  )

cell <- dplyr::left_join(cell, c_sum, by = "iso3c")

cell <- cell %>%
  dplyr::mutate(
    wage_per = wage / c_wage,
    pop_per = pop / c_pop,
    co2_per = co2 / c_co2
  )

saveRDS(cell, here("Data", "inter", "grid", "cell.rds"))