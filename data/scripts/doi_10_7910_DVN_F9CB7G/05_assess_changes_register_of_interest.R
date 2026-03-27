# compare supplemental register of interest with initial coding

library(tidyverse) # CRAN v2.0.0
library(xtable)    # CRAN v1.8-4

# added after scandals
dat_added <- read.csv("data_supplements_register.csv",
                      fileEncoding = "utf-8") |> 
    rename(ownership_3_revised = ownership_3,
           ownership_2_revised = ownership_2,
           land_revised = land) |> 
    mutate(ownership_2_revised = dplyr::recode(ownership_2_revised,
                                               "Purpose not specified" = "Homeowner or Landlord")) |> 
    mutate(ownership_3_revised = dplyr::recode(ownership_3_revised,
                                               "Purpose not specified" = "Landlord"))


# initial register of interest

dat_initial <- readRDS("dat_meta_all.rds")


# only keep years since 2013

# merge datasets
dat_merged <- left_join(dat_added, dat_initial) |> 
    filter(year >= 2013) |> 
    filter(mpname != "troy robert")

# remove Robert Troy


# number of changes
nrow(dat_merged)

# number of TDs affected
length(unique(dat_merged$mpname))

# create table with TDs, and the number of years they changed

dat_overview <- dat_merged |> 
    mutate(harmonised = str_to_title(harmonised)) |> # change name to title case
    mutate(harmonised = str_replace_all(harmonised, " ", ", ")) |> # change name to title case
    mutate(harmonised = str_replace_all(harmonised, "O, Fearghail", "O'Fearghail")) |> 
    group_by(harmonised) |> 
    rename(Name = harmonised) |> 
    summarise(`Changes (Years)` = n(),
              Years = paste0(year, collapse = ", "))



dat_changeowner <- dat_merged |> 
    filter(ownership_2_revised == "Homeowner or Landlord") |> 
    filter(ownership_2 == "Not Homeowner or Landlord")

nrow(dat_changeowner)

dat_changeowner$harmonised



dat_changeowner_rev <- dat_merged |> 
    filter(ownership_2 == "Homeowner or Landlord") |> 
    filter(ownership_2_revised == "Not Homeowner or Landlord")

nrow(dat_changeowner_rev)

dat_changeowner_rev$harmonised

# print table
print(
    xtable(dat_overview,
           digits = 1,
           caption = "Overview of TDs who changed their Register of Interest entries after the revelation of the scandals",
           label = "tab:changes_roi",
           align = c(
               "p{0.03\\textwidth}",
               "p{0.2\\textwidth}",
               "p{0.1\\textwidth}",
               "p{0.5\\textwidth}"
           )
    ),
    type = "latex",
    digits = 1,
    size = "footnotesize",
    file = "tab_a11.tex",
    include.rownames = FALSE,
    caption.placement = "top"
)

print(
    xtable(dat_overview,
           digits = 1,
           caption = "Overview of TDs who changed their Register of Interest entries after the revelation of the scandals",
           label = "tab:changes_roi",
           align = c(
               "p{0.03\\textwidth}",
               "p{0.2\\textwidth}",
               "p{0.1\\textwidth}",
               "p{0.5\\textwidth}"
           )
    ),
    type = "html",
    digits = 1,
    size = "footnotesize",
    file = "tab_a11.html",
    include.rownames = FALSE,
    caption.placement = "top"
)

# check changes

dat_changes <- dat_merged |> 
    group_by(ownership_2, ownership_2_revised) |> 
    mutate(ownership_2_revised = dplyr::recode(ownership_2_revised,
                                       "Homeowner or Landlord" = "Landlord or Owner of Multiple Properties",
                                       "Not Homeowner or Landlord" = "No Landlord and No Owner of Multiple Properties")) |> 
    mutate(ownership_2 = dplyr::recode(ownership_2,
                                       "Homeowner or Landlord" = "Landlord or Owner of Multiple Properties",
                                       "Not Homeowner or Landlord" = "No Landlord and No Owner of Multiple Properties")) |> 
    count(name = "Frequency") |> 
    ungroup() |> 
    mutate(Percent = paste0(100 * Frequency / sum(Frequency), "%")) |> 
    rename(`Initial Coding` = ownership_2,
           `Revised Coding` = ownership_2_revised)
    

# print table
print(
    xtable(dat_changes,
           digits = 1,
           caption = "Comparing the initial coding of landlord status with revised coding (after corrections of their Register of Interests)",
           label = "tab:changes_roi_overview",
           align = c(
               "p{0.03\\textwidth}",
               "p{0.3\\textwidth}",
               "p{0.3\\textwidth}",
               "p{0.1\\textwidth}",
               "p{0.1\\textwidth}"
               
           )
    ),
    type = "latex",
    digits = 1,
    size = "footnotesize",
    file = "tab_a12.tex",
    include.rownames = FALSE,
    caption.placement = "top"
)
print(
    xtable(dat_changes,
           digits = 1,
           caption = "Comparing the initial coding of landlord status with revised coding (after corrections of their Register of Interests)",
           label = "tab:changes_roi_overview",
           align = c(
               "p{0.03\\textwidth}",
               "p{0.3\\textwidth}",
               "p{0.3\\textwidth}",
               "p{0.1\\textwidth}",
               "p{0.1\\textwidth}"
               
           )
    ),
    type = "html",
    digits = 1,
    size = "footnotesize",
    file = "tab_a12.html",
    include.rownames = FALSE,
    caption.placement = "top"
)


# create cross-tabs with changes (the first two are most relevant since we want full changes in homeownership status)
table(intial_coding = dat_merged$ownership_2,
      revised_coding = dat_merged$ownership_2_revised)

prop.table(table(intial_coding = dat_merged$ownership_2,
      revised_coding = dat_merged$ownership_2_revised))


table(intial_coding = dat_merged$ownership_3,
      revised_coding = dat_merged$ownership_3_revised)

prop.table(table(intial_coding = dat_merged$ownership_3,
      revised_coding = dat_merged$ownership_3_revised))

