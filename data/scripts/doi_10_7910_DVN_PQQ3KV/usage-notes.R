## ------------------------------------------------------------------------
#| echo: false
suppressPackageStartupMessages(
  {
    library(tidyverse)
    library(arrow)
    library(here)
  }
)


## ------------------------------------------------------------------------
#| echo: false
ds <- open_dataset(here("release"))


## ----eval=FALSE----------------------------------------------------------
## library(tidyverse)
## library(arrow)
## 
## ds <- open_dataset("cvrs")


## ├── state=ARIZONA
## │   ├── county_name=MARICOPA
## │   │   └── part-0.parquet
## │   ├── county_name=PIMA
## │   │   └── part-0.parquet
## │   ├── county_name=SANTA%20CRUZ
## │   │   └── part-0.parquet
## │   └── county_name=YUMA
## │       └── part-0.parquet
## ...
## ── state=UTAH
## │   └── county_name=SAN%20JUAN
## │       └── part-0.parquet
## └── state=WISCONSIN
##     ├── county_name=BROWN
##     │   └── part-0.parquet
##     ├── county_name=KENOSHA
##     │   └── part-0.parquet
##     ├── county_name=PIERCE
##     │   └── part-0.parquet
##     └── county_name=WAUKESHA
##         └── part-0.parquet

## ------------------------------------------------------------------------
ds |> count(office) |> collect()


## ------------------------------------------------------------------------
ds |> 
  filter(state == "WISCONSIN", office == "US PRESIDENT") |> 
  count(candidate, party, sort = TRUE) |> 
  collect()


## ------------------------------------------------------------------------
ds |> 
  filter(state == "ARIZONA", county_name == "MARICOPA") |> 
  filter(cvr_id == 1) |> 
  select(county_name, cvr_id, office, district, candidate, party) |> 
  collect()


## ------------------------------------------------------------------------
ds |> 
  filter(state == "ARIZONA", office == "STATE SENATE", district == "013") |> 
  count(candidate, party_detailed) |> 
  collect()


## ------------------------------------------------------------------------
ds_states <- ds |> 
  filter(state %in% c("WISCONSIN", "MICHIGAN", "GEORGIA", "ARIZONA", "NEVADA"))


## ------------------------------------------------------------------------
ds_contested <- ds_states |> 
  collect() |> 
  # Contested contests
  filter(any(party == "REP") & any(party == "DEM"), 
         .by = c(state, office, district)) |> 
  # Ballots with Presidential vote
  filter(any(office == "US PRESIDENT"), 
         .by = c(state, county_name, cvr_id))


## ------------------------------------------------------------------------
#| echo: false
#| eval: false
## ## This is 3x faster because we can use arrow verbs
## tictoc::tic()
## rows_contes <- ds_states |>
##   summarize(
##     contested = any(party == "REP") &
##       any(party == "DEM"),
##     .by = c(state, office, district)
##   ) |>
##   filter(contested)
## 
## rows_has_pres <- ds_states |>
##   summarize(
##     has_pres = any(office == "US PRESIDENT"),
##      .by = c(state, county_name, cvr_id)
##   ) |>
##   filter(has_pres)
## 
## ds_contested <- ds_states |>
##   semi_join(rows_contes) |>
##   semi_join(rows_has_pres) |>
##   collect()
## 
## tictoc::toc()


## ------------------------------------------------------------------------
## Voters based on President
ds_pres <- ds_contested |> 
  filter(office == "US PRESIDENT") |> 
  select(
    state, county_name, 
    cvr_id, candidate,
    pres_party = party) |> 
  mutate(pres = case_when(
    pres_party == "REP" ~ "Trump", 
    pres_party == "DEM" ~ "Biden", 
    pres_party == "LBT" ~ "Libertarian", 
    candidate == "UNDERVOTE" ~ "Undervote",
    .default = "Other"))


## ------------------------------------------------------------------------
## subset to all-Dem voters based on everything except President
ds_D <- ds_contested |> 
  filter(office != "US PRESIDENT") |> 
  filter(all(party == "DEM"), .by = c(state, county_name, cvr_id)) |> 
  distinct(state, county_name, cvr_id) |> 
  mutate(nonpres_party = "Down-ballot Democrat")

## same subset, but for all-Rep voters
ds_R <- ds_contested |> 
  filter(office != "US PRESIDENT") |> 
  filter(all(party == "REP"), .by = c(state, county_name, cvr_id)) |> 
  distinct(state, county_name, cvr_id) |> 
  mutate(nonpres_party = "Down-ballot Republican")



## ------------------------------------------------------------------------
ds_analysis <- ds_pres |> 
  left_join(
    bind_rows(ds_D, ds_R), 
    by = c("state", "county_name", "cvr_id"), relationship = "one-to-one") |> 
  mutate(nonpres_party = replace_na(nonpres_party, "Mixed"))


## ------------------------------------------------------------------------
xtabs(~ nonpres_party + pres, ds_analysis) |> 
  addmargins()


## ------------------------------------------------------------------------
xtprop <- xtabs(~ nonpres_party + pres, ds_analysis) |> 
  prop.table(margin = 1) |> 
  round(3) 

## add margins
row_Ns <- xtabs(~ nonpres_party, ds_analysis) |> 
  format(big.mark = ",")

## reorder columns and append totals
xtprop[, c("Biden", "Trump", "Libertarian", "Undervote")] |> 
  cbind(row_Ns) |> 
  kableExtra::kbl(format = "latex", booktabs = TRUE)
