#### purpose: constructing 2016 cmps dataset #### 

#### libraries #### 

suppressPackageStartupMessages(
  
  {
    
    library(readstata13)    
    library(haven)
    library(tidyverse)
    library(dplyr)
    library(estimatr)
    library(zipcode)
    library(sf)
    
  }
  
)

#### reading in data ####

# loading in dataset 

cmps16 = read_dta("cmps/CMPS2016.dta")

#### cleaning: black baseline #### 

# black baseline

cmps16b = cmps16 %>% 
  filter(s2_3 == 1) %>% 
  filter(s2_2 != 1) %>% 
  mutate(supp_blm = abs(c228 - 5),
         supp_blm = supp_blm / max(supp_blm, na.rm = TRUE),
         supp_blm2 = ifelse(c228 == 1 | c228 == 2, 1, 0),
         supp_reps = ifelse(c158 == 1, 1, 0),
         blm_eff = abs(ifelse(bl229 == 88, NA, bl229) - 4),
         blm_eff = blm_eff / max(blm_eff, na.rm = TRUE),
         blm_eff2 = ifelse(bl229 == 1 | bl229 == 2, 1, 0)) %>% 
  mutate(pd_black = abs(ifelse(c244 == 5, 4, c244) - 4)) %>% 
  mutate(pi_gop = ifelse(c25 == 1, 1, 0),
         pi_dem = ifelse(c25 == 2, 1, 0),
         pi_ind = ifelse(c25 == 3 | c25 == 4, 1, 0)) %>% 
  mutate(unemp = ifelse(c390 == 5, 1, 0),
         ownhome = ifelse(c389 == 2, 1, 0),
         inc_ref = ifelse(c383 == 99, 1, 0),
         inc = ifelse(c383 == 99, 1, c383 - 1)) %>% 
  mutate(edu = c381 - 1) %>% 
  mutate(lf_blk = ifelse(c150 == 1, 1, 0)) %>% 
  mutate(ide_miss = ifelse(c31 == 6, 1, 0),
         ide = abs(ifelse(c31 == 6, 3, c31) - 5),
         ide = ide / max(ide)) %>% 
  mutate(ed = ifelse(c251 == 1, 1, 0)) 

#### cleaning: white baseline #### 

# white baseline 

cmps16w = cmps16 %>% 
  filter(s2_1 == 1) %>% 
  mutate(supp_blm = abs(c228 - 5),
         supp_blm = supp_blm / max(supp_blm, na.rm = TRUE),
         supp_blm2 = ifelse(c228 == 1 | c228 == 2, 1, 0),
         supp_reps = ifelse(c158 == 1, 1, 0)) 

#### cleaning: latinx data #### 

cmps16 = cmps16 %>% 
  filter(s2_2 == 1) %>% 
  mutate(worry = abs(l366 - 5),
         worry = worry / max(worry, na.rm = TRUE)) %>% 
  mutate(supp_blm = abs(c228 - 5),
         supp_blm = supp_blm / max(supp_blm, na.rm = TRUE)) %>% 
  mutate(pi_gop = ifelse(c25 == 1, 1, 0),
         pi_dem = ifelse(c25 == 2, 1, 0),
         pi_ind = ifelse(c25 == 3 | c25 == 4, 1, 0)) %>% 
  mutate(supp_reps = ifelse(c158 == 1, 1, 0)) %>% 
  mutate(pol_discrim = abs(l290 - 5),
         pol_discrim = pol_discrim / max(pol_discrim, na.rm = TRUE)) %>% 
  mutate(pd_black = abs(ifelse(c244 == 5, 4, c244) - 4)) %>% 
  mutate(pd_black = pd_black / max(pd_black, na.rm = TRUE)) %>% 
  mutate(pd_latinx = abs(ifelse(c247 == 5, 4, c247) - 4)) %>% 
  mutate(pd_latinx = pd_latinx / max(pd_latinx, na.rm = TRUE)) %>% 
  mutate(rac_prof = abs(bla207 - 4),
         rac_prof = rac_prof / max(rac_prof, na.rm = TRUE)) %>% 
  mutate(ide_miss = ifelse(c31 == 6, 1, 0),
         ide = abs(ifelse(c31 == 6, 3, c31) - 5),
         ide = ide / max(ide)) %>% 
  mutate(know_undoc = ifelse(l364 == 1, 1, 0),
         blm_eff = abs(ifelse(bl229 == 88, NA, bl229) - 4),
         blm_eff = blm_eff / max(blm_eff, na.rm = TRUE)) %>% 
  mutate(common = abs(ifelse(l302 == 88, NA, l302) - 4),
         common = common / max(common, na.rm = TRUE)) %>% 
  mutate(id_cent = abs(la193 - 4),
         id_cent = id_cent / max(id_cent)) %>% 
  mutate(spanish = ifelse(s1 == 2, 1, 0),
         fobo = ifelse(s7 == 2, 1, 0)) %>% 
  mutate(cit = ifelse(c375 == 1, 1, 0)) %>% 
  mutate(cit = ifelse(is.na(cit), 1, cit)) %>% 
  mutate(usborn = ifelse(fobo == 0, 1, 0),
         english = ifelse(spanish == 0, 1, 0)) %>% 
  mutate(par_usborn = ifelse(c377 == 1 | 
                               c377 == 4 | c377 == 3, 1, 0),
         par_fobo = ifelse(c377 == 2, 1, 0),
         par_usborn = ifelse(is.na(par_usborn), 0, par_usborn),
         par_fobo = ifelse(is.na(par_fobo), 0, par_fobo)) %>% 
  mutate(second_gen = ifelse(par_fobo == 1 & usborn == 1, 1, 0),
         third_gen = ifelse(par_usborn == 1 & usborn == 1, 1, 0)) %>% 
  mutate(acc = ifelse(second_gen == 1, 1, ifelse(third_gen == 1, 2, 0))) %>% 
  mutate(acc = acc + english,
         acc2 = acc + cit) %>% 
  mutate(acc2 = acc2 / max(acc2, na.rm = TRUE)) %>% 
  mutate(no_mx = ifelse(s10 == 12, 1, 0),
         no_dr = ifelse(s10 == 7, 1, 0),
         no_cb = ifelse(s10 == 6, 1, 0),
         no_pr = ifelse(s10 == 17, 1, 0),
         no_es = ifelse(s10 == 9, 1, 0)) %>%
  mutate(ed = ifelse(c251 == 1, 1, 0)) %>%
  mutate(threat_pol = abs(l232 - 4),
         threat_pol = threat_pol / max(threat_pol)) %>% 
  mutate(amer_cent = abs(c194 - 4),
         skin = ifelse(c262 >= 6, 6, c262) - 1) %>% 
  mutate(imm_cit = abs(c141 - 4),
         imm_undoc_leave = ifelse(c38 == 3, 1, 0), 
         imm_dec_border_fund = ifelse(c337 == 1, 1, 0)) %>% 
  mutate(imm_indx = imm_cit + 
           abs(imm_undoc_leave - 1) + imm_dec_border_fund) %>% 
  mutate(plc_ban_gm = abs(c40 - 1), # agree with NOT banning gm 
         plc_cc = abs(c42 - 5), # agree with solving cc
         plc_taxrich = abs(c43 - 5), # agree with taxing rich 
         plc_vid = abs(c44 - 1), # agree with NOT requiring voter ID  
         plc_ocare = abs(c45 - 5)) %>% # agree with obamacare
  mutate(plc_indx = plc_ban_gm + plc_cc + plc_taxrich + plc_vid + plc_ocare) %>% 
  mutate(plc_ban_gm = plc_ban_gm / max(plc_ban_gm, na.rm = TRUE),
         plc_cc = plc_cc / max(plc_cc, na.rm = TRUE),
         plc_taxrich = plc_taxrich / max(plc_taxrich, na.rm = TRUE),
         plc_vid = plc_vid / max(plc_vid, na.rm = TRUE),
         plc_ocare = plc_ocare / max(plc_ocare, na.rm = TRUE),
         plc_indx = plc_indx / max(plc_indx, na.rm = TRUE)) %>% 
  mutate(street_race_black = ifelse(c373 == 2, 1, 0)) %>% 
  mutate(imm_remove = abs(bla205 - 1) / max(bla205)) %>% 
  mutate(imm_indx = imm_cit + 
           abs(imm_undoc_leave - 1) + imm_dec_border_fund + imm_remove) %>% 
  mutate(imm_indx = imm_indx / max(imm_indx, na.rm = TRUE)) %>% 
  mutate(black_cate = ifelse(s2_3 == 1, 1, 0),
         afrolat = ifelse(c380 == 2, 1, 0)) %>% 
  mutate(black_lat = ifelse(black_cate == 1 | afrolat == 1, 1, 0)) %>% 
  mutate(wom = ifelse(s3 == 1, 1, 0)) %>% 
  mutate(lf_min_dn = ifelse(bla191 == 88, 1, 0)) %>% 
  mutate(lf_min = abs(ifelse(bla191 == 88, 4, bla191) - 4)) %>% 
  mutate(polint = abs(c33 - 4)) %>% 
  mutate(cath = ifelse(c129 == 1, 1, 0),
         mar = ifelse(c385 == 3, 1, 0)) %>% 
  mutate(unemp = ifelse(c390 == 5, 1, 0),
         ownhome = ifelse(c389 == 2, 1, 0),
         inc_ref = ifelse(c383 == 99, 1, 0),
         inc = ifelse(c383 == 99, 1, c383 - 1)) %>% 
  mutate(edu = c381 - 1) %>% 
  mutate(lf_lat = ifelse(c150 == 1, 1, 0)) %>% 
  mutate(neigh_blk = c349_2 / max(c349_2, na.rm = TRUE)) %>% 
  mutate(neigh_blk_miss = ifelse(is.na(neigh_blk), 1, 0)) %>% 
  mutate(neigh_blk = ifelse(is.na(neigh_blk), 0, neigh_blk)) %>% 
  mutate(aware_blm = abs(c226 - 4)) %>% 
  mutate(econ_worse = ifelse(c23 == 3 | c23 == 4, 1, 0)) %>% 
  mutate(polint = abs(c33 - 4)) %>% 
  mutate(protest = ifelse(c66 == 1, 1, 0)) %>% 
  mutate(racism_problem = ifelse(bl155 == 1, 1, 0)) %>% 
  mutate(workwblk = abs(l209 - 4)) %>% 
  mutate(lean_gop = ifelse(c27 == 1, 1, 0),
         lean_gop = ifelse(is.na(lean_gop), 0, lean_gop),
         lean_dem = ifelse(c27 == 2, 1, 0),
         lean_dem = ifelse(is.na(lean_dem), 0, lean_dem),
         pi_ind2 = ifelse(c27 == 3 | c27 == 4 | c27 == 88, 1, 0),
         pi_ind2 = ifelse(is.na(pi_ind2), 0, pi_ind2)) %>% 
  mutate(pi_gop2 = ifelse(lean_gop == 1 | pi_gop == 1, 1, 0),
         pi_dem2 = ifelse(lean_dem == 1 | pi_dem == 1, 1, 0)) %>% # black spouse
  mutate(mar_black_spouse = ifelse(c386 == 3, 1, 0),
         mar_black_spouse = ifelse(is.na(mar_black_spouse), 0, mar_black_spouse)) %>% 
  mutate(mar_white = ifelse(c386 == 1, 1, 0)) %>% 
  mutate(polcomp = 
           (abs(bl87 - 4) + abs(bl88 - 4)) - 
           (abs(bl91 - 4) + abs(bl92 - 4))) %>% 
  mutate(
    cenfe_ws = ifelse(s4 %in% c("CA", "OR", "WA", "MT", "ID", "NV", "UT",
                                "WY", "CO", "AZ", "NM", "HI", "AK"), 1, 0),
    cenfe_nc = ifelse(s4 %in% c("ND", "SD", "MN", "NE", "KS", "IA", "MO",
                                "WI", "IL", "IN", "OH", "MI"), 1, 0),
    cenfe_st = ifelse(s4 %in% c("OK", "TX", "LA", "AR", "MS", "AL", "TN",
                                "KY", "FL", "GA", "SC", "NC",
                                "VA", "WV", "MD", "DE", "DC"), 1, 0),
    cenfe_ne = ifelse(s4 %in% c("NY", "PA", "NJ", "CT", "MA",
                                "RI", "NH", "VT", "ME"), 1, 0)
  ) 

cmps16 = 
  cmps16 %>% 
  mutate(
    miss_church = ifelse(is.na(c133_2), 1, 0),
    pbl_church = ifelse(is.na(c133_2), 0, c133_2),
    common2 = ifelse(l302 == 1 | l302 == 2, 1, 0),
    blm_eff2 = ifelse(bl229 == 1 | bl229 == 2, 1, 0),
    supp_blm2 = ifelse(c228 == 1 | c228 == 2, 1, 0)
  )

cmps16 = cmps16 %>% 
  mutate(
    oppose_lgbt = l241 - 1    
  )


# fixing zipcode 

cmps16$zipcode = cmps16$zipcode %>% str_pad(width = 5, side = "left", pad = "0")

# now, assigning zipcode to county 

data(zipcode)
zipcode = zipcode %>% rename(zipcode = zip)
zipcode = zipcode[!is.na(zipcode$latitude), ]
zipcode = 
  zipcode %>% 
  dplyr::select(zipcode, latitude, longitude) %>% 
  st_as_sf(coords = c("longitude", "latitude"))

cty_shp14 = 
  read_sf('census_data/county/COUNTY_2014_US_SL050_2020-02-24_12-50-56-120/COUNTY_2014_US_SL050_Coast_Clipped.shp')

cty_shp14 = cty_shp14 %>% 
  rename(FIPS = Geo_FIPS) %>% 
  as.data.frame() %>% 
  dplyr::select(FIPS, geometry) %>% 
  rename(fips = FIPS) %>% 
  st_as_sf()

st_crs(zipcode) = st_crs(cty_shp14)
zipcode = st_transform(zipcode, crs = st_crs(cty_shp14))
zipcode = zipcode[zipcode$zipcode %in% cmps16$zipcode , ]

sf_use_s2(FALSE)
zipcode = st_intersection(zipcode, cty_shp14)

cmps16 = merge(cmps16, zipcode, by = "zipcode", all.x = TRUE)

# okay, fixing missingness in the fips indicator 

cmps16$miss_fips = ifelse(is.na(cmps16$fips), 1, 0)
cmps16$fips = ifelse(is.na(cmps16$fips), "99999", cmps16$fips)

# loading in county and zipcode census data 

cty_16 = read_csv("census_data/county/county_cendat_16.csv")

cty_16 = 
  cty_16 %>% 
  rename(fips = FIPS) %>% 
  mutate(tpop_cty = as.numeric(`Total Population`),
         pdns_cty = as.numeric(`Population Density (Per Sq. Mile)`),
         plt_cty = as.numeric(`% Total Population: Hispanic or Latino`),
         pfb_cty = as.numeric(`% Total Population: Foreign Born`),
         pcl_cty = as.numeric(`% Population 25 Years and Over: Bachelor's Degree or More`),
         pbl_cty = as.numeric(cty_16$`% Total Population: Black or African American Alone`),
         pnc_cty = as.numeric(`% Total Population: Foreign Born: Not a Citizen`),
         mhhi_cty = as.numeric(`Median Household Income (In 2015 Inflation Adjusted Dollars)`),
         pue_cty = as.numeric(`% Civilian Population in Labor Force 16 Years and Over: Unemployed`)) %>% 
  dplyr::select(fips, tpop_cty, pdns_cty, plt_cty, pbl_cty, pcl_cty, pfb_cty, pnc_cty, mhhi_cty, pue_cty)

zipdat16 = read_csv("census_data/zip/zip_cendat16.csv")

zipdat16 = zipdat16 %>% 
  rename(zipcode = `ZIP Code Tabulation Area (5-digit)`) %>% 
  mutate(tpop = as.numeric(`Total Population`),
         pdns = as.numeric(`Population Density (Per Sq. Mile)`),
         pfb = as.numeric(`% Total Population: Foreign Born`),
         plt = as.numeric(`% Total Population: Hispanic or Latino`),
         pbl = as.numeric(zipdat16$`% Total Population: Black or African American Alone`),
         pcl = as.numeric(`% Population 25 Years and Over: Bachelor's Degree or More`),
         pnc = as.numeric(`% Total Population: Foreign Born: Not a Citizen`),
         mhhi = as.numeric(`Median Household Income (In 2015 Inflation Adjusted Dollars)`),
         pue = as.numeric(`% Civilian Population in Labor Force 16 Years and Over: Unemployed`)) %>% 
  dplyr::select(zipcode, tpop, pdns, pcl, pfb, plt, pbl, pnc, mhhi, pue)

cmps16 = merge(cmps16, cty_16, by = "fips", all.x = TRUE)
cmps16 = merge(cmps16, zipdat16, by = "zipcode", all.x = TRUE)

cmps16 = cmps16 %>% 
  mutate(tpop_cty = ifelse(is.na(tpop_cty), 0, tpop_cty),
         pdns_cty = ifelse(is.na(pdns_cty), 0, pdns_cty),
         plt_cty = ifelse(is.na(plt_cty), 0, plt_cty),
         pbl_cty = ifelse(is.na(pbl_cty), 0, pbl_cty),
         pfb_cty = ifelse(is.na(pfb_cty), 0, pfb_cty),
         pcl_cty = ifelse(is.na(pcl_cty), 0, pcl_cty),
         pnc_cty = ifelse(is.na(pnc_cty), 0, pnc_cty),
         mhhi_cty = ifelse(is.na(mhhi_cty), 0, mhhi_cty),
         pue_cty = ifelse(is.na(pue_cty), 0, pue_cty)) %>%
  mutate(tpop = ifelse(is.na(tpop), 0, tpop),
         pdns = ifelse(is.na(pdns), 0, pdns),
         pfb = ifelse(is.na(pfb), 0, pfb),
         pbl = ifelse(is.na(pbl), 0, pbl),
         pcl = ifelse(is.na(pcl), 0, pcl),
         pnc = ifelse(is.na(pnc), 0, pnc),
         mhhi = ifelse(is.na(mhhi), 0, mhhi),
         pue = ifelse(is.na(pue), 0, pue)) %>% 
  mutate(plt = ifelse(is.na(plt), 0, plt))

cmps16 = cmps16 %>% 
  mutate(ltpop = log(tpop + 1),
         lmhhi = log(mhhi + 1),
         ltpop_cty = log(tpop_cty + 1),
         lmhhi_cty = log(mhhi_cty + 1))

cmps16$miss_zip = ifelse(cmps16$zipcode == "", 1, 0)

# reading in sc data 

sc_dat = read_csv("sc_data/sc_stats_2015.csv")
sc_dat = sc_dat %>% 
  filter(State != "State") %>% 
  dplyr::select(State, County, `Convicted\rCriminals L3`, 
                `Convicted\rCriminals L2`, `Convicted\rCriminals L1`, Total_1) %>% 
  rename(total = Total_1,
         cc_l3 = `Convicted\rCriminals L3`,
         cc_l2 = `Convicted\rCriminals L2`,
         cc_l1 = `Convicted\rCriminals L1`,
         county = County,
         state = State) %>% 
  mutate(total = gsub(x = total, ",", ""),
         cc_l1 = gsub(x = cc_l1, ",", ""),
         cc_l2 = gsub(x = cc_l2, ",", ""),
         cc_l3 = gsub(x = cc_l3, ",", "")) %>% 
  mutate_at(vars(3:ncol(.)), .funs = as.numeric) %>% 
  mutate(p_l1 = cc_l1 / total,
         p_l1l2 = (cc_l1 + cc_l2) / total,
         p_l3 = cc_l3 / total)

# reading in county merge file, cleaning it a little 

cmerge = read_csv("county_fips_master.csv")

cmerge$county_name = cmerge$county_name %>% str_replace(" County", "")

sc_dat$county[!(sc_dat$county %in% cmerge$county_name)] = 
  sc_dat$county[!(sc_dat$county %in% cmerge$county_name)] %>% 
  str_replace("Saint", "St.")

sc_dat$county[!(sc_dat$county %in% cmerge$county_name)] = 
  sc_dat$county[!(sc_dat$county %in% cmerge$county_name)] %>% 
  str_replace("De ", "De") %>% 
  str_replace("La ", "La")

sc_dat = sc_dat %>% 
  filter(state != "PR") %>% 
  filter(state != "VI") %>% 
  filter(state != "AS") %>% 
  filter(state != "GU") %>% 
  filter(state != "MP")

sc_dat$county[!(sc_dat$county %in% cmerge$county_name)][1] = 
  "Anchorage Municipality"
sc_dat$county[!(sc_dat$county %in% cmerge$county_name)][1] = 
  "Juneau City and Borough"

sc_dat$county[!(sc_dat$county %in% cmerge$county_name)] = 
  sc_dat$county[!(sc_dat$county %in% cmerge$county_name)] %>% 
  str_replace("OB", "O'B") %>% 
  str_replace("Lag", "LaG") %>% 
  str_replace("DeBaca", "De Baca") %>% 
  str_replace("St. ", "Ste. ") %>% 
  str_replace("Dona", 'Do�a') %>% 
  str_replace("New York City", 'New York') %>% 
  str_replace("LeFlore", "Le Flore") %>% 
  str_replace("Mc Kean", "McKean") %>% 
  str_replace("Census Area", "Borough")

sc_dat$county[!(sc_dat$county %in% cmerge$county_name)][1] = 
  cmerge$county_name[grepl(x = cmerge$county_name, "rince of")]
sc_dat$county[!(sc_dat$county %in% cmerge$county_name)][1] = 
  cmerge$county_name[grepl(x = cmerge$county_name, "kagway")]
sc_dat$county[!(sc_dat$county %in% cmerge$county_name)][1] = 
  cmerge$county_name[grepl(x = cmerge$county_name, "rangell")]

# one cannot be merged --- oahu

# grabbing what is necesary from the merge file 

cmerge = 
  cmerge %>% 
  dplyr::select(state_abbr, county_name, fips) %>% 
  rename(state = state_abbr,
         county = county_name) %>% 
  mutate(fips = str_pad(fips, side = "left", pad = "0", width = 5))

sc_dat2 = left_join(sc_dat, cmerge, by = c("county", "state"))

# merging with cmps data 

cmps16 = merge(cmps16, sc_dat2, by = "fips", all.x = TRUE)

# quickly fixing the sc covars 

cmps16$ltotal = log(cmps16$total + 1)
cmps16$dep_rate = (cmps16$total / (cmps16$pfb_cty * cmps16$tpop_cty)) * 1000
cmps16$ltotal = ifelse(is.na(cmps16$ltotal), 0, cmps16$ltotal)
cmps16$dep_rate = ifelse(is.na(cmps16$dep_rate), 0, cmps16$dep_rate)
cmps16$p_l3 = ifelse(is.na(cmps16$p_l3), 0, cmps16$p_l3)

# loading in competition data (county-level)

comp_covars = read_csv("census_data/county_competition/comp_covars.csv")

comp_covars = comp_covars %>% 
  mutate(ppov_blk_cty = 
           as.numeric(`% Black or African American Alone Population for  Whom&nbsp; Poverty Status Is Determined: Income in 2015 Below Poverty Level`),
         ppov_lat_cty = 
           as.numeric(`% Hispanic or Latino Population for Whom Poverty  Status Is Determined: Income in 2015 Below Poverty Level`),
         pune_lat_cty = as.numeric(`% Hispanic or Latino 16 Years Old in Civilian Labor Force: Unemployed`),
         pune_blk_cty = as.numeric(`% Black or African American 16 Years Old in  Civilian Labor Force: Unemployed`)) %>%  
  mutate(ppov_lat_adv_cty = ppov_blk_cty - ppov_lat_cty,
         pune_lat_adv_cty = pune_blk_cty - pune_lat_cty) %>% 
  mutate(fips = FIPS) %>% 
  dplyr::select(fips, ppov_lat_adv_cty, pune_lat_adv_cty)

black_ed = 
  read_csv("census_data/county_competition/black_education/ACSDT5Y2015.C15002B_data_with_overlays_2021-05-03T223944.csv")
latino_ed = 
  read_csv("census_data/county_competition/latino_education/ACSDT5Y2015.C15002I_data_with_overlays_2021-05-03T223700.csv")

black_ed = black_ed[, !grepl(x = black_ed[1, ], "Margin of Err")]
black_ed = black_ed %>% slice(2:nrow(.))
black_ed$pcol_blk_cty = 
  (as.numeric(black_ed[, 8] %>% as.matrix) + 
     as.numeric(black_ed[, 13] %>% as.matrix)) / 
  as.numeric(black_ed[, 3] %>% as.matrix)
black_ed = black_ed %>% 
  dplyr::select(GEO_ID, pcol_blk_cty) %>% 
  mutate(fips = as.character(substring(GEO_ID, 10, 100))) %>% 
  dplyr::select(fips, pcol_blk_cty)

latino_ed = latino_ed[, !grepl(x = latino_ed[1, ], "Margin of Err")]
latino_ed = latino_ed %>% slice(2:nrow(.))
latino_ed$pcol_lat_cty = 
  (as.numeric(latino_ed[, 8] %>% as.matrix) + 
     as.numeric(latino_ed[, 13] %>% as.matrix)) / 
  as.numeric(latino_ed[, 3] %>% as.matrix)
latino_ed = latino_ed %>% 
  dplyr::select(GEO_ID, pcol_lat_cty) %>% 
  mutate(fips = as.character(substring(GEO_ID, 10, 100))) %>% 
  dplyr::select(fips, pcol_lat_cty)

ed_dat = merge(black_ed, latino_ed, by = "fips")
ed_dat$pcol_lat_adv_cty = ed_dat$pcol_lat_cty - ed_dat$pcol_blk_cty
ed_dat$pcol_lat_adv_miss_cty = ifelse(is.na(ed_dat$pcol_lat_adv_cty), 1, 0)
ed_dat$pcol_lat_adv_cty = ifelse(is.na(ed_dat$pcol_lat_adv_cty), 0, ed_dat$pcol_lat_adv_cty)

comp_covars = merge(ed_dat, comp_covars, by = "fips")
comp_covars = comp_covars %>% 
  mutate(ppov_lat_adv_miss_cty = ifelse(is.na(ppov_lat_adv_cty), 1, 0),
         ppov_lat_adv_cty = ifelse(is.na(ppov_lat_adv_cty), 0, ppov_lat_adv_cty)) %>% 
  mutate(pune_lat_adv_miss_cty = ifelse(is.na(pune_lat_adv_cty), 1, 0),
         pune_lat_adv_cty = ifelse(is.na(pune_lat_adv_cty), 0, pune_lat_adv_cty))

cmps16 = merge(cmps16, comp_covars, by = 'fips', all.x = TRUE)

cmps16$ppov_lat_adv_miss_cty = 
  ifelse(is.na(cmps16$ppov_lat_adv_cty), 1, cmps16$ppov_lat_adv_miss_cty)
cmps16$pune_lat_adv_miss_cty = 
  ifelse(is.na(cmps16$pune_lat_adv_cty), 1, cmps16$pune_lat_adv_miss_cty)
cmps16$pcol_lat_adv_miss_cty = 
  ifelse(is.na(cmps16$pcol_lat_adv_cty), 1, cmps16$pcol_lat_adv_miss_cty)

# incorporating competition data (zipcode level)

comp_covars = read_csv("census_data/zip_competition/comp_covars.csv")

comp_covars = 
  comp_covars %>% 
  mutate(ppov_blk = 
           as.numeric(`% Black or African American Alone Population for  Whom&nbsp; Poverty Status Is Determined: Income in 2015 Below Poverty Level`),
         ppov_lat = 
           as.numeric(`% Hispanic or Latino Population for Whom Poverty  Status Is Determined: Income in 2015 Below Poverty Level`),
         pune_lat = as.numeric(`% Hispanic or Latino 16 Years Old in Civilian Labor Force: Unemployed`),
         pune_blk = as.numeric(`% Black or African American 16 Years Old in  Civilian Labor Force: Unemployed`)) %>%  
  mutate(ppov_lat_adv = ppov_blk - ppov_lat,
         pune_lat_adv = pune_blk - pune_lat) %>% 
  mutate(zip = `ZIP Code Tabulation Area (5-digit)`) %>% 
  dplyr::select(zip, ppov_lat_adv, pune_lat_adv) %>% 
  mutate(ppov_lat_adv_miss = ifelse(is.na(ppov_lat_adv), 1, 0),
         pune_lat_adv_miss = ifelse(is.na(pune_lat_adv), 1, 0)) %>% 
  mutate(ppov_lat_adv = ifelse(is.na(ppov_lat_adv), 0, ppov_lat_adv),
         pune_lat_adv = ifelse(is.na(pune_lat_adv), 0, pune_lat_adv))

black_ed$zip = substring(black_ed$GEO_ID, 10, 100)
black_ed = 
  read_csv("census_data/zip_competition/black_education/ACSDT5Y2019.C15002B_data_with_overlays_2021-05-05T213936.csv")
latino_ed = 
  read_csv("census_data/zip_competition/latino_education/ACSDT5Y2019.C15002I_data_with_overlays_2021-05-05T213907.csv")

black_ed = black_ed[, !grepl(x = black_ed[1, ], "Margin of Err")]
black_ed = black_ed %>% slice(2:nrow(.))
black_ed$pcol_blk = 
  (as.numeric(black_ed[, 8] %>% as.matrix) + 
     as.numeric(black_ed[, 13] %>% as.matrix)) / 
  as.numeric(black_ed[, 3] %>% as.matrix)

black_ed = black_ed %>% 
  dplyr::select(GEO_ID, pcol_blk) %>% 
  mutate(zip = as.character(substring(GEO_ID, 10, 100))) %>% 
  dplyr::select(zip, pcol_blk)

latino_ed = latino_ed[, !grepl(x = latino_ed[1, ], "Margin of Err")]
latino_ed = latino_ed %>% slice(2:nrow(.))
latino_ed$pcol_lat = 
  (as.numeric(latino_ed[, 8] %>% as.matrix) + 
     as.numeric(latino_ed[, 13] %>% as.matrix)) / 
  as.numeric(latino_ed[, 3] %>% as.matrix)

latino_ed = latino_ed %>% 
  dplyr::select(GEO_ID, pcol_lat) %>% 
  mutate(zip = as.character(substring(GEO_ID, 10, 100))) %>% 
  dplyr::select(zip, pcol_lat)

ed_dat = merge(black_ed, latino_ed, by = "zip")
ed_dat$pcol_lat_adv = ed_dat$pcol_lat - ed_dat$pcol_blk
ed_dat$pcol_lat_adv_miss = ifelse(is.na(ed_dat$pcol_lat_adv), 1, 0)
ed_dat$pcol_lat_adv = ifelse(is.na(ed_dat$pcol_lat_adv), 0, ed_dat$pcol_lat_adv)

comp_covars = merge(ed_dat, comp_covars, by = "zip")

comp_covars = comp_covars %>% 
  mutate(ppov_lat_adv_miss = ifelse(is.na(ppov_lat_adv), 1, 0),
         ppov_lat_adv = ifelse(is.na(ppov_lat_adv), 0, ppov_lat_adv)) %>% 
  mutate(pune_lat_adv_miss = ifelse(is.na(pune_lat_adv), 1, 0),
         pune_lat_adv = ifelse(is.na(pune_lat_adv), 0, pune_lat_adv))
comp_covars = comp_covars %>% rename(zipcode = zip)

cmps16 = merge(cmps16, comp_covars, by = 'zipcode', all.x = TRUE)

cmps16$ppov_lat_adv_miss = 
  ifelse(is.na(cmps16$ppov_lat_adv), 1, cmps16$ppov_lat_adv_miss)
cmps16$pune_lat_adv_miss = 
  ifelse(is.na(cmps16$pune_lat_adv), 1, cmps16$pune_lat_adv_miss)
cmps16$pcol_lat_adv_miss = 
  ifelse(is.na(cmps16$pcol_lat_adv), 1, cmps16$pcol_lat_adv_miss)

# fixing some issues 

cmps16$miss_adv = ifelse(is.na(cmps16$pune_lat_adv), 1, 0)
cmps16$pune_lat_adv = ifelse(is.na(cmps16$pune_lat_adv), 0, cmps16$pune_lat_adv)
cmps16$ppov_lat_adv = ifelse(is.na(cmps16$ppov_lat_adv), 0, cmps16$ppov_lat_adv)
cmps16$pcol_lat_adv = ifelse(is.na(cmps16$pcol_lat_adv), 0, cmps16$pcol_lat_adv)

cmps16$skin_miss = ifelse(is.na(cmps16$skin), 1, 0)
cmps16$age_miss = ifelse(is.na(cmps16$age), 1, 0)
cmps16$skin = ifelse(is.na(cmps16$skin), 0, cmps16$skin)
cmps16$age = ifelse(is.na(cmps16$age), 0, cmps16$age)

#### vote choice #### 

cmps16$vote_dem = ifelse(cmps16$c14 == 1, 1, 0)

#### saving datasets #### 

save(x = cmps16, file = "cmps_lat.RData")
save(x = cmps16w, file = "cmps_wht.RData")
save(x = cmps16b, file = "cmps_blk.RData")
