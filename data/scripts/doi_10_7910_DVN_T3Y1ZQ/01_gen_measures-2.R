# data ####
# link to ches, update path accordingly.
ches <- read.csv("1999-2019_CHES_dataset_means(v1).csv")

cntry_info <- data.frame(country = c(1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12, 13, 14, 16, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 31, 37, 38, 40), 
                         country_label = c("BE", "DK", "DE", "GR", "ES", "FR", "IE", "IT", "NL", "UK", "PT", "AT", "FI", "SE", "BG", "CZ", "EE", "HU", "LV", "LT", "PL", "RO", "SK", "SI", "HR", "MT", "LU", "CY"))

# correct poland vote share in 2019
ches$vote[ches$country == 26 & ches$year == 2019 & ches$party == "SLD"] <- 12.56 - 4.87

df_out <- ches %>% 
  dplyr::select(country, year, electionyear) %>% 
  distinct() %>% 
  mutate(lr = NA,
         econ = NA,
         galtan = NA,
         eu = NA,
         md_shan = NA,
         md_eu_shan = NA,
         lr_v = NA,
         econ_v = NA,
         galtan_v = NA,
         eu_v = NA,
         md_v_shan = NA,
         md_eu_v_shan = NA,
         effnr = NA, 
         prop = NA,
         ed = NA,
         ed_v = NA,
         ed_eu = NA,
         ed_eu_v = NA) %>% 
  filter(!(country %in% c(31, 37, 38, 40)))

df <- ches %>% 
  dplyr::select(country, year, lrgen, galtan, lrecon, eu_position, vote, seat, electionyear) %>% 
  filter(!(country %in% c(31, 37, 38, 40)))

# parlgov data
df_parlgov_country <- read.csv("https://parlgov.org/data/parlgov-development_csv-utf-8/country.csv") %>%
  filter(chess %in% cntry_info$country) %>%
  dplyr::select(id, chess, name)

df_parlgov_election <- read.csv("https://parlgov.org/data/parlgov-development_csv-utf-8/election.csv") %>%
  filter(country_id %in% df_parlgov_country$id & type_id == 13) %>%
  mutate(date = lubridate::ymd(date),
         electionyear = lubridate::year(date)) %>%
  filter(electionyear > 1995) %>%
  merge(df_parlgov_country %>% dplyr::select(id, chess), by.x = "country_id", by.y = "id") %>%
  dplyr::select(date, electionyear, chess, seats_total) %>%
  filter(date != "2019-04-28") %>%  # CHES data based on November 2019 election
  filter(date != "2012-05-06") %>% # CHES data based on June 2012 election
  select(-date)

df_out <- merge(df_out,
                df_parlgov_election,
                all.x = TRUE,
                by.x = c("country", "electionyear"),
                by.y = c("chess", "electionyear"))

for (i in 1:nrow(df_out)){
  
  df_tmp <- dplyr::filter(df, country == df_out$country[i] & year == df_out$year[i])
  
  if (df_out$year[i] == 2019){
    df_tmp$seat <- 100 * df_tmp$seat / df_out %>% 
      filter(country == df_out$country[i] & electionyear == df_out$electionyear[i]) %>% 
      pull(seats_total)
  }
  
  cov_tmp <- cov.wt(na.omit(df_tmp[,c("lrgen", "lrecon", "galtan", "eu_position")]))
  
  df_out[i, c("lr", "econ", "galtan", "eu_position")] <- diag(cov_tmp$cov)
  
  x <- estimate.ED(cov2cor(cov_tmp$cov[2:3,2:3]), round.digits = 10)
  df_out[i, "md_shan"] <- sum(diag(cov_tmp$cov)[2:3] * x$n1/2)
  df_out$ed[i] <- x$n1
  
  x <- estimate.ED(cov2cor(cov_tmp$cov[2:4,2:4]), round.digits = 10)
  df_out[i, "md_eu_shan"] <- sum(diag(cov_tmp$cov)[2:4] * x$n1/3)
  df_out$ed_eu[i] <- x$n1
  
  df_tmp2 <- df_tmp %>%
    dplyr::select(lrgen, lrecon, galtan, eu_position, vote) %>%
    na.omit()
  
  cov_tmp <- cov.wt(na.omit(df_tmp2[,c("lrgen", "lrecon", "galtan", "eu_position")]), wt = df_tmp2$vote)
  
  df_out[i, c("lr_v", "econ_v", "galtan_v", "eu_position_v")] <- diag(cov_tmp$cov)
  
  x <- estimate.ED(cov2cor(cov_tmp$cov[2:3,2:3]), round.digits = 10)
  df_out[i, "md_v_shan"] <- sum(diag(cov_tmp$cov)[2:3] * x$n1/2)
  df_out$ed_v[i] <- x$n1
  
  x <- estimate.ED(cov2cor(cov_tmp$cov[2:4,2:4]), round.digits = 10)
  df_out[i, "md_eu_v_shan"] <- sum(diag(cov_tmp$cov)[2:4] * x$n1/3)
  df_out$ed_eu_v[i] <- x$n1
  
  df_out[i, "effnr"] <- 1/sum((df_tmp$seat/100)^2, na.rm = T)
  df_out[i, "prop"] <- sqrt(sum((df_tmp$vote/100 - df_tmp$seat/100)^2, na.rm = T)/2)
}

df_out <- merge(df_out,
                ches %>% 
                  select(country, year, party_id, vote, family, galtan_salience, lrecon_salience) %>% 
                  mutate(family_n = ifelse(family %in% c(1, 7, 10), 1, 0)) %>% 
                  group_by(country, year) %>% 
                  summarize(challengers = sum(vote*family_n, na.rm = T),
                            galtan_salience = weighted.mean(galtan_salience, vote, na.rm = T),
                            lrecon_salience = weighted.mean(lrecon_salience, vote, na.rm = T))) %>% 
  ungroup() %>% 
  select(-seats_total)

df_out <- merge(df_out, cntry_info, all.x = TRUE)

df_out$duplicate_election <- 0
df_out$duplicate_election[df_out$country == 1 & df_out$year == 2002] <- 1
df_out$duplicate_election[df_out$country == 6 & df_out$year == 2006] <- 1
df_out$duplicate_election[df_out$country == 7 & df_out$year == 2006] <- 1
df_out$duplicate_election[df_out$country == 11 & df_out$year == 2014] <- 1
df_out$duplicate_election[df_out$country == 14 & df_out$year == 2002] <- 1

dir.create("data")
write.csv(df_out, file = "data/ches_level_measures.csv")

# cses
# link to cses, update path accordingly.
load("cses_imd.rdata")
load("cses5.rdata")

cses_info <- data.frame(keep = c("Austria", "Belgium", "Bulgaria", "Croatia", "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", "Great Britain", "Greece",
                                 "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden"),
                        country = c(13, 1, 20, 31, 21, 2, 22, 14, 6, 3, 11, 4, 23, 7, 8, 24, 25, 10, 26, 12, 27, 28, 29, 5, 16))

cses_imd <- filter(cses_imd, IMD1006_NAM %in% cses_info$keep) %>% 
  select(country = IMD1006_NAM, year = IMD1008_YEAR, respid = IMD1005,
         age = IMD2001_1, # age
         gender = IMD2002, # gender
         education = IMD2003, # education
         lr = IMD3006, # lr self
         union = IMD2019_1,
         income = IMD2006,
         partisan = IMD3005_1) %>% 
  mutate(age = ifelse(age > 1000, NA, age),
         gender = ifelse(gender > 2, NA, gender),
         education = ifelse(education > 5, NA, education),
         union = ifelse(union > 1, NA, union),
         income = ifelse(income > 5, NA, income),
         partisan = ifelse(partisan > 1, NA, partisan),
         lr = ifelse(lr > 10, NA, lr))

cses5 <- filter(cses5, E1006_NAM %in% cses_info$keep)  %>% 
  mutate(E2001_Y = ifelse(E2001_Y > 9000, NA, E2001_Y),
         age = E1034 - E2001_Y) %>% 
  select(country = E1006_NAM, year = E1008, respid = E1005,
         age,
         gender = E2002, # gender
         education = E2003, # education
         lr = E3020, # lr self
         union = E2005,
         income = E2010,
         partisan = E3024_1) %>% 
  mutate(gender = ifelse(gender > 2, NA, gender),
         education = ifelse(education %in% c(1), 0,
                            ifelse(education %in% c(2, 3), 1,
                                   ifelse(education %in% c(4), 2,
                                          ifelse(education %in% c(5, 6), 3,
                                                 ifelse(education %in% c(7, 8, 9), 4, NA))))),
         union = ifelse(union > 1, NA, union),
         income = ifelse(income > 5, NA, income),
         partisan = ifelse(partisan == 1, 1, ifelse(partisan == 0, 0, NA)),
         lr = ifelse(lr > 10, NA, lr))

cses <- bind_rows(cses_imd, cses5) %>%
  rename(lr_self = lr)

rm(cses_imd, cses5)

cses <- merge(cses, cses_info, by.x = "country", by.y = "keep")

df_many <- merge(df_out %>% filter(duplicate_election == 0), cses, by.x = c("country", "electionyear"), by.y = c("country.y", "year")) %>% 
  rename(countrylong = country.y)
save(df_many, file = "data/cses_level_measures.Rdata")
rm(list = setdiff(ls(), "estimate.ED"));gc()
