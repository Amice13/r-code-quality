#### purpose: constructing 2020 cmps dataset #### 

#### loading packages #### 

suppressPackageStartupMessages(
  
  {
    
    library(readstata13)
    library(tidyverse)
    library(haven)
    library(estimatr)
    library(sf)
    library(zipcode)
    library(lwgeom)
    library(readstata13)
    library(ggthemes)
    library(foreign)
    library(gridExtra)
    library(zipcode)
    
  }
  
)

#### reading in data #### 

cmps20 = 
  read_dta("cmps2020/cmps_primary_sample/CMPS 2020 primary sample weighted.dta")

#### cleaning data #### 

# black reference dataset

cmps20b = cmps20 %>% filter(race == 3) %>% 
  mutate(blm_protest = ifelse(Q356 == 1, 1, 0)) %>% 
  mutate(blm_support1 = abs(Q358 - 5),
         blm_support2 = abs(Q359r1 - 5)) %>% 
  mutate(blm_supports = blm_support1 + blm_support2) %>% 
  mutate(blm_supports = blm_supports / max(blm_supports, na.rm = TRUE)) %>% 
  mutate(blm_ft = Q308r7 / max(Q308r7, na.rm = TRUE)) %>% 
  mutate(black_stereotype = abs(Q381_Q383r2- 1),
         black_stereotype = black_stereotype / max(black_stereotype, na.rm = TRUE),
         white_stereotype = abs(Q381_Q383r1- 1),
         white_stereotype = white_stereotype / max(white_stereotype, na.rm = TRUE),
         dtp = Q385 - 1,
         dtp = dtp /max(dtp),
         blm_eff = abs(Q99r2 - 5),
         blm_nprotest = ifelse(Q366r2 == 2, 1, 0),
         blm_unfair = abs(Q384r1 - 7),
         polbrut = abs(Q117r1 - 4),
         dtp2 = abs(Q706_Q708r1 - 5),
         dtp3 = abs(Q706_Q708r2 - 5),
         white_id = ifelse(S2_Race_Prime == 1, 1, 0),
         white_hood_rank = abs(Q76r1 - 6),
         black_hood_rank = abs(Q76r3 - 6),
         black_common = Q616_Q618r1 - 1,
         black_stereotype_diff = black_stereotype - white_stereotype,
         id_cent = abs(Q271 - 5),
         black_threat = (Q225r5 - 1) - (Q225r6 - 1),
         black_threat2 = (Q225r5 - 1),
         white_threat2 = (Q225r6 - 1)) %>% 
  mutate(black_stereotype_diff = black_stereotype_diff / 
           max(black_stereotype_diff, na.rm = TRUE),
         black_threat = black_threat / max(black_threat, na.rm = TRUE),
         dtp2 = dtp2 / max(dtp2, na.rm = TRUE),
         dtp3 = dtp3 / max(dtp3, na.rm = TRUE),
         black_hood_rank = black_hood_rank / max(black_hood_rank, na.rm = TRUE),
         white_hood_rank = white_hood_rank / max(white_hood_rank, na.rm = TRUE),
         black_threat = black_threat / max(black_threat, na.rm = TRUE),
         black_threat2 = black_threat2 / max(black_threat2, na.rm = TRUE),
         white_threat2 = white_threat2 / max(white_threat2, na.rm = TRUE),
         blm_eff = blm_eff / max(blm_eff, na.rm = TRUE)) %>% 
  mutate(rr = abs(Q213r1 - 5) + abs(Q213r2 - 1) + abs(Q213r3 - 1) + abs(Q213r4 - 5)) %>% 
  mutate(rr = rr / max(rr, na.rm = TRUE))


# white reference dataset 

cmps20w = cmps20 %>% filter(race == 1) %>% 
  mutate(blm_protest = ifelse(Q356 == 1, 1, 0)) %>% 
  mutate(blm_support1 = abs(Q358 - 5),
         blm_support2 = abs(Q359r1 - 5)) %>% 
  mutate(blm_supports = blm_support1 + blm_support2) %>% 
  mutate(blm_supports = blm_supports / max(blm_supports, na.rm = TRUE)) %>% 
  mutate(blm_ft = Q308r7 / max(Q308r7, na.rm = TRUE)) %>% 
  mutate(black_stereotype = abs(Q381_Q383r2- 1),
         black_stereotype = black_stereotype / max(black_stereotype, na.rm = TRUE),
         white_stereotype = abs(Q381_Q383r1- 1),
         white_stereotype = white_stereotype / max(white_stereotype, na.rm = TRUE),
         dtp = Q385 - 1,
         dtp = dtp /max(dtp),
         blm_eff = abs(Q99r2 - 5),
         blm_nprotest = ifelse(Q366r2 == 2, 1, 0),
         blm_unfair = abs(Q384r1 - 7),
         polbrut = abs(Q117r1 - 4),
         dtp2 = abs(Q706_Q708r1 - 5),
         dtp3 = abs(Q706_Q708r2 - 5),
         white_id = ifelse(S2_Race_Prime == 1, 1, 0),
         white_hood_rank = abs(Q76r1 - 6),
         black_hood_rank = abs(Q76r3 - 6),
         black_common = Q616_Q618r1 - 1,
         black_stereotype_diff = black_stereotype - white_stereotype,
         id_cent = abs(Q271 - 5),
         black_threat = (Q225r5 - 1) - (Q225r6 - 1)) %>% 
  mutate(black_stereotype_diff = black_stereotype_diff / 
           max(black_stereotype_diff, na.rm = TRUE),
         black_threat = black_threat / max(black_threat, na.rm = TRUE),
         dtp2 = dtp2 / max(dtp2, na.rm = TRUE),
         dtp3 = dtp3 / max(dtp3, na.rm = TRUE),
         black_hood_rank = black_hood_rank / max(black_hood_rank, na.rm = TRUE),
         white_hood_rank = white_hood_rank / max(white_hood_rank, na.rm = TRUE),
         black_threat = black_threat / max(black_threat, na.rm = TRUE),
         blm_eff = blm_eff / max(blm_eff, na.rm = TRUE)) %>% 
  mutate(rr = abs(Q213r1 - 5) + abs(Q213r2 - 1) + abs(Q213r3 - 1) + abs(Q213r4 - 5)) %>% 
  mutate(rr = rr / max(rr, na.rm = TRUE))


# latinx dataset 

cmps20l = cmps20 %>% 
  filter(race == 2) %>% 
  mutate(eng = ifelse(S1 == 1, 1, 0)) %>% 
  mutate(black_id = ifelse(S2_Racer3 == 1, 1, 0),
         afrolat = ifelse(is.na(ifelse(S2_AfroLatr2 == 1, 1, 0)), 0, ifelse(S2_AfroLatr2 == 1, 1, 0)),
         wom = ifelse(S3b == 2, 1, 0),
         age = 2021 - S5,
         usborn = ifelse(S7 == 1, 1, 0),
         no_mx = ifelse(S10 == 12, 1, 0),
         no_pr = ifelse(S10 == 17, 1, 0),
         no_dr = ifelse(S10 == 7, 1, 0),
         no_es = ifelse(S10 == 9, 1, 0),
         no_cb = ifelse(S10 == 6, 1, 0),
         edu = abs(S13 - 1),
         pi_gop = ifelse(Q21 == 1, 1, 0),
         pi_dem = ifelse(Q21 == 2, 1, 0),
         pi_ind = ifelse(Q21 == 3 | Q21 == 4, 1, 0)) %>% 
  mutate(rr_spfav = abs(Q213r1 - 5),
         rr_slavery = abs(Q213r2 - 1),
         rr_deserve = abs(Q213r3 - 1),
         rr_workhard = abs(Q213r4 - 5)) %>% 
  mutate(strong_part = ifelse(is.na(ifelse(Q22 == 1, 1, 0)), 0, ifelse(Q22 == 1, 1, 0))) %>% 
  mutate(lean_gop = ifelse(is.na(ifelse(Q23 == 1, 1, 0)), 0, ifelse(Q23 == 1, 1, 0)),
         lean_dem = ifelse(is.na(ifelse(Q23 == 2, 1, 0)), 0, ifelse(Q23 == 2, 1, 0)),
         pure_ind = ifelse(is.na(ifelse(Q23 > 2, 1, 0)), 0, ifelse(Q23 > 2, 1, 0))) %>% 
  mutate(pid7 = ifelse(pi_dem ==1 & strong_part == 1, 6,
                       ifelse(pi_dem == 1 & strong_part == 0, 5,
                              ifelse(pi_dem == 0 & lean_dem == 1, 4,
                                     ifelse(pure_ind == 1, 3,
                                            ifelse(pi_gop == 0 & lean_gop == 1, 2,
                                                   ifelse(pi_gop == 1 & strong_part == 0, 1, 0))))))) %>% 
  mutate(cath = ifelse(Q58r1 == 1, 1, 0),
         imm_ice = abs(Q183 - 4)) %>% 
  mutate(imm_bord = abs(Q184r1 - 1),
         imm_cit = abs(Q184r2 - 5),
         imm_brt = abs(Q187r3 - 5),
         imm_visa = abs(Q187r4 - 5)) %>% 
  mutate(imm_nocollab = ifelse(Q352 == 1, 1, 0)) %>% 
  mutate(rr = abs(Q213r1 - 5) + abs(Q213r2 - 1) + abs(Q213r3 - 1) + abs(Q213r4 - 5)) %>% 
  mutate(skin = Q261 - 1) %>% 
  mutate(exp_disc = ifelse(Q281r9 != 1, 1, 0)) %>% 
  mutate(imm_undoc = Q308r1 / max(Q308r1, na.rm = TRUE),
         blm_ft = Q308r7 / max(Q308r7, na.rm = TRUE)) %>% 
  mutate(blm_protest = ifelse(Q356 == 1, 1, 0)) %>% 
  mutate(blm_support1 = abs(Q358 - 5),
         blm_support2 = abs(Q359r1 - 5)) %>% 
  mutate(blm_supports = blm_support1 + blm_support2) %>% 
  mutate(know_deport = ifelse(Q490r3 == 1 | Q490r5 == 1 | Q490r7 == 1, 1, 0)) %>% 
  mutate(know_undoc = 
           ifelse(Q491r2 == 1 | Q491r3 == 1 | Q491r4 == 1 | Q491r5 == 1 | Q491r6 == 1, 1, 0)) %>% 
  mutate(threat = abs(Q492 - 4) + abs(Q493 - 4),
         threat2 = abs(Q492 - 4)) %>% 
  mutate(imm_lat_stereo = abs(Q545r7 - 7) + abs(Q546r7 - 1) + 
           abs(Q547r7 - 1) + abs(Q548r7 - 1) + 
           abs(Q549r7 - 1)
  ) %>% 
  mutate(pd_lat_ref = ifelse(Q619_Q626r6 == 5, 1, 0),
         pd_lat = abs(ifelse(Q619_Q626r6 == 5, 4, Q619_Q626r6) - 4),
         pd_blk_ref = ifelse(Q619_Q626r2 == 5, 1, 0),
         pd_blk = abs(ifelse(Q619_Q626r2 == 5, 4, Q619_Q626r2) - 4),
         ed = ifelse(Q627 == 1, 1, 0),
         mar = ifelse(Q741 == 1, 1, 0),
         cit = ifelse(is.na(ifelse(Q807 == 1, 1, 0)), 1, ifelse(Q807 == 1, 1, 0))) %>% 
  mutate(third_gen = ifelse(is.na(ifelse(Q809 == 1, 1, 0)), 0, ifelse(Q809 == 1, 1, 0))) %>% 
  mutate(inc_ref = ifelse(Q813 == 99, 1, 0),
         inc = ifelse(Q813 == 99, 12, Q813) - 1) %>% 
  mutate(unemp = ifelse(Q814 == 5, 1, 0)) %>% 
  mutate(ownhome = ifelse(Q639 == 1, 1, 0)) %>% 
  mutate(black_lat = ifelse(black_id == 1 | afrolat == 1, 1, 0)) %>% 
  mutate(genstat = ifelse(usborn == 1 & third_gen == 0, 1,
                          ifelse(usborn == 1 & third_gen == 1, 2, 0))) %>% 
  mutate(acc2 = genstat + eng + cit) %>% 
  mutate(threat = threat / max(threat, na.rm = TRUE),
         rr = rr / max(rr, na.rm = TRUE),
         acc2 = acc2 / max(acc2, na.rm = TRUE)) %>% 
  mutate(blm_supports = blm_supports / max(blm_supports, na.rm = TRUE),
         imm_indx = I(imm_ice + imm_bord + imm_cit + imm_brt + imm_visa)) %>% 
  mutate(imm_indx = imm_indx / max(imm_indx, na.rm = TRUE)) %>% 
  mutate(ide_ref = ifelse(Q43 == 6, 1, 0),
         ide = abs(ifelse(Q43 == 6, 5, Q43) - 5)) %>% 
  mutate(black_stereotype = abs(Q381_Q383r2- 1),
         black_stereotype = black_stereotype / max(black_stereotype, na.rm = TRUE),
         white_stereotype = abs(Q381_Q383r1- 1),
         white_stereotype = white_stereotype / max(white_stereotype, na.rm = TRUE),
         dtp = Q385 - 1,
         dtp = dtp /max(dtp),
         blm_eff = abs(Q99r2 - 5),
         blm_nprotest = ifelse(Q366r2 == 2, 1, 0),
         blm_unfair = abs(Q384r1 - 7),
         polbrut = abs(Q117r1 - 4),
         dtp2 = abs(Q706_Q708r1 - 5),
         dtp3 = abs(Q706_Q708r2 - 5),
         white_id = ifelse(S2_Race_Prime == 1, 1, 0),
         white_hood_rank = abs(Q76r1 - 6),
         latinx_hood_rank = abs(Q76r2 - 6),
         black_hood_rank = abs(Q76r3 - 6),
         black_common = Q616_Q618r1 - 1,
         black_stereotype_diff = black_stereotype - white_stereotype,
         id_cent = abs(Q271 - 5),
         black_threat = (Q225r5 - 1) - (Q225r6 - 1)) %>% 
  mutate(zipcode = substring(S15, 2, 100),
         pct_blk_perc = Q539r2 / max(Q539r2, na.rm = TRUE)) %>% 
  mutate(mar_white = ifelse(Q746a == 1 | Q746b == 1, 1, 0),
         mar_black = ifelse(Q746a == 3 | Q746b == 3, 1, 0)) %>% 
  mutate(mar_white = ifelse(is.na(mar_white), 0, mar_white),
         mar_black = ifelse(is.na(mar_black), 0, mar_black),
         lf_lat = Q551_Q559r2 - 1,
         amer_id = abs(Q560r8 - 8),
         econ_grp_worse = ifelse(Q350 == 3, 1, 0),
         econ_afraid_socio = abs(Q147 - 5),
         econ_afraid_pers = abs(Q150 - 5),
         polcomp = (Q575_Q581r4 + Q575_Q581r6) - (Q575_Q581r3 + Q575_Q581r5),
         polcomp = polcomp / max(polcomp, na.rm = TRUE)) %>% 
  mutate(black_stereotype_diff = black_stereotype_diff / 
           max(black_stereotype_diff, na.rm = TRUE),
         black_threat = black_threat / max(black_threat, na.rm = TRUE),
         dtp2 = dtp2 / max(dtp2, na.rm = TRUE),
         dtp3 = dtp3 / max(dtp3, na.rm = TRUE),
         black_hood_rank = black_hood_rank / max(black_hood_rank, na.rm = TRUE),
         white_hood_rank = white_hood_rank / max(white_hood_rank, na.rm = TRUE),
         black_threat = black_threat / max(black_threat, na.rm = TRUE),
         blm_eff = blm_eff / max(blm_eff, na.rm = TRUE)) %>% 
  mutate(
    cenfe_ws = ifelse(S4 %in% c("5", "38", "48", "27", "13", "29", "45",
                                "51", "6", "3", "32", "12", "1"), 1, 0),
    cenfe_nc = ifelse(S4 %in% c("35", "42", "24", "28", "17", "16", "26",
                                "50", "14", "15", "36", "23"), 1, 0),
    cenfe_st = ifelse(S4 %in% c("37", "44", "19", "4", "25", "2", "43",
                                "18", "10", "11", "41", "34",
                                "47", "49", "21", "8", "9"), 1, 0),
    cenfe_ne = ifelse(S4 %in% c("33", "39", "31", "7", "22",
                                "40", "30", "46", "20"), 1, 0)
  )


cmps20l$imm_work_ethic = 
  abs(cmps20l$Q208r1 - 5) + abs(cmps20l$Q208r2 - 5) + 
  abs(cmps20l$Q208r3 - 5) + abs(cmps20l$Q208r4 - 1)
cmps20l$imm_work_ethic = 
  cmps20l$imm_work_ethic / max(cmps20l$imm_work_ethic, na.rm = TRUE)

cmps20l$protest_work_ethic = 
  abs(cmps20l$Q62r2 - 4) + abs(cmps20l$Q62r3 - 1) + 
  abs(cmps20l$Q62r4 - 1) + abs(cmps20l$Q62r5 - 1)

# asian

cmps20a = cmps20 %>% 
  filter(race == 4) %>% 
  mutate(eng = ifelse(S1 == 1, 1, 0)) %>% 
  mutate(black_id = ifelse(S2_Racer3 == 1, 1, 0),
         afrolat = ifelse(is.na(ifelse(S2_AfroLatr2 == 1, 1, 0)), 0, ifelse(S2_AfroLatr2 == 1, 1, 0)),
         wom = ifelse(S3b == 2, 1, 0),
         age = 2021 - S5,
         usborn = ifelse(S7 == 1, 1, 0),
         no_mx = ifelse(S10 == 12, 1, 0),
         no_pr = ifelse(S10 == 17, 1, 0),
         no_dr = ifelse(S10 == 7, 1, 0),
         no_es = ifelse(S10 == 9, 1, 0),
         no_cb = ifelse(S10 == 6, 1, 0),
         edu = abs(S13 - 1),
         pi_gop = ifelse(Q21 == 1, 1, 0),
         pi_dem = ifelse(Q21 == 2, 1, 0),
         pi_ind = ifelse(Q21 == 3 | Q21 == 4, 1, 0)) %>% 
  mutate(strong_part = ifelse(is.na(ifelse(Q22 == 1, 1, 0)), 0, ifelse(Q22 == 1, 1, 0))) %>% 
  mutate(lean_gop = ifelse(is.na(ifelse(Q23 == 1, 1, 0)), 0, ifelse(Q23 == 1, 1, 0)),
         lean_dem = ifelse(is.na(ifelse(Q23 == 2, 1, 0)), 0, ifelse(Q23 == 2, 1, 0)),
         pure_ind = ifelse(is.na(ifelse(Q23 > 2, 1, 0)), 0, ifelse(Q23 > 2, 1, 0))) %>% 
  mutate(pid7 = ifelse(pi_dem ==1 & strong_part == 1, 6,
                       ifelse(pi_dem == 1 & strong_part == 0, 5,
                              ifelse(pi_dem == 0 & lean_dem == 1, 4,
                                     ifelse(pure_ind == 1, 3,
                                            ifelse(pi_gop == 0 & lean_gop == 1, 2,
                                                   ifelse(pi_gop == 1 & strong_part == 0, 1, 0))))))) %>% 
  mutate(cath = ifelse(Q58r1 == 1, 1, 0),
         imm_ice = abs(Q183 - 4)) %>% 
  mutate(imm_bord = abs(Q184r1 - 1),
         imm_cit = abs(Q184r2 - 5),
         imm_brt = abs(Q187r3 - 5),
         imm_visa = abs(Q187r4 - 5)) %>% 
  mutate(imm_nocollab = ifelse(Q352 == 1, 1, 0)) %>% 
  mutate(rr = abs(Q213r1 - 5) + abs(Q213r2 - 1) + abs(Q213r3 - 1) + abs(Q213r4 - 5)) %>% 
  mutate(skin = Q261 - 1) %>% 
  mutate(exp_disc = ifelse(Q281r9 != 1, 1, 0)) %>% 
  mutate(imm_undoc = Q308r1 / max(Q308r1, na.rm = TRUE),
         blm_ft = Q308r7 / max(Q308r7, na.rm = TRUE)) %>% 
  mutate(blm_protest = ifelse(Q356 == 1, 1, 0)) %>% 
  mutate(blm_support1 = abs(Q358 - 5),
         blm_support2 = abs(Q359r1 - 5)) %>% 
  mutate(blm_supports = blm_support1 + blm_support2) %>% 
  mutate(know_deport = ifelse(Q490r3 == 1 | Q490r5 == 1 | Q490r7 == 1, 1, 0)) %>% 
  mutate(know_undoc = 
           ifelse(Q491r2 == 1 | Q491r3 == 1 | Q491r4 == 1 | Q491r5 == 1 | Q491r6 == 1, 1, 0)) %>% 
  mutate(threat = abs(Q492 - 4) + abs(Q493 - 4),
         threat2 = abs(Q492 - 4)) %>% 
  mutate(imm_lat_stereo = abs(Q545r7 - 7) + abs(Q546r7 - 1) + 
           abs(Q547r7 - 1) + abs(Q548r7 - 1) + 
           abs(Q549r7 - 1)
  ) %>% 
  mutate(pd_lat_ref = ifelse(Q619_Q626r6 == 5, 1, 0),
         pd_lat = abs(ifelse(Q619_Q626r6 == 5, 4, Q619_Q626r6) - 4),
         pd_asn_ref = ifelse(Q619_Q626r3 == 5, 1, 0),
         pd_asn = abs(ifelse(Q619_Q626r3 == 5, 4, Q619_Q626r6) - 4),
         pd_blk_ref = ifelse(Q619_Q626r2 == 5, 1, 0),
         pd_blk = abs(ifelse(Q619_Q626r2 == 5, 4, Q619_Q626r2) - 4),
         ed = ifelse(Q627 == 1, 1, 0),
         mar = ifelse(Q741 == 1, 1, 0),
         cit = ifelse(is.na(ifelse(Q807 == 1, 1, 0)), 1, ifelse(Q807 == 1, 1, 0))) %>% 
  mutate(third_gen = ifelse(is.na(ifelse(Q809 == 1, 1, 0)), 0, ifelse(Q809 == 1, 1, 0))) %>% 
  mutate(inc_ref = ifelse(Q813 == 99, 1, 0),
         inc = ifelse(Q813 == 99, 12, Q813) - 1) %>% 
  mutate(unemp = ifelse(Q814 == 5, 1, 0)) %>% 
  mutate(ownhome = ifelse(Q639 == 1, 1, 0)) %>% 
  mutate(black_lat = ifelse(black_id == 1 | afrolat == 1, 1, 0)) %>% 
  mutate(genstat = ifelse(usborn == 1 & third_gen == 0, 1,
                          ifelse(usborn == 1 & third_gen == 1, 2, 0))) %>% 
  mutate(acc2 = genstat + eng + cit) %>% 
  mutate(threat = threat / max(threat, na.rm = TRUE),
         rr = rr / max(rr, na.rm = TRUE),
         acc2 = acc2 / max(acc2, na.rm = TRUE)) %>% 
  mutate(blm_supports = blm_supports / max(blm_supports, na.rm = TRUE),
         imm_indx = I(imm_ice + imm_bord + imm_cit + imm_brt + imm_visa)) %>% 
  mutate(imm_indx = imm_indx / max(imm_indx, na.rm = TRUE)) %>% 
  mutate(ide_ref = ifelse(Q43 == 6, 1, 0),
         ide = abs(ifelse(Q43 == 6, 5, Q43) - 5)) %>% 
  mutate(black_stereotype = abs(Q381_Q383r2- 1),
         black_stereotype = black_stereotype / max(black_stereotype, na.rm = TRUE),
         white_stereotype = abs(Q381_Q383r1- 1),
         white_stereotype = white_stereotype / max(white_stereotype, na.rm = TRUE),
         dtp = Q385 - 1,
         dtp = dtp /max(dtp),
         blm_eff = abs(Q99r2 - 5),
         blm_nprotest = ifelse(Q366r2 == 2, 1, 0),
         blm_unfair = abs(Q384r1 - 7),
         polbrut = abs(Q117r1 - 4),
         dtp2 = abs(Q706_Q708r1 - 5),
         dtp3 = abs(Q706_Q708r2 - 5),
         white_id = ifelse(S2_Race_Prime == 1, 1, 0),
         white_hood_rank = abs(Q76r1 - 6),
         black_hood_rank = abs(Q76r3 - 6),
         black_common = Q616_Q618r1 - 1,
         black_stereotype_diff = black_stereotype - white_stereotype,
         id_cent = abs(Q271 - 5),
         black_threat = (Q225r5 - 1) - (Q225r6 - 1)) %>% 
  mutate(zipcode = substring(S15, 2, 100),
         pct_blk_perc = Q539r2 / max(Q539r2, na.rm = TRUE)) %>% 
  mutate(mar_white = ifelse(Q746a == 1 | Q746b == 1, 1, 0),
         mar_black = ifelse(Q746a == 3 | Q746b == 3, 1, 0)) %>% 
  mutate(mar_white = ifelse(is.na(mar_white), 0, mar_white),
         mar_black = ifelse(is.na(mar_black), 0, mar_black),
         lf_lat = Q551_Q559r2 - 1,
         amer_id = abs(Q560r8 - 8),
         econ_grp_worse = ifelse(Q350 == 3, 1, 0),
         econ_afraid_socio = abs(Q147 - 5),
         econ_afraid_pers = abs(Q150 - 5),
         polcomp = (Q575_Q581r4 + Q575_Q581r6) - (Q575_Q581r3 + Q575_Q581r5),
         polcomp = polcomp / max(polcomp, na.rm = TRUE)) %>% 
  mutate(black_stereotype_diff = black_stereotype_diff / 
           max(black_stereotype_diff, na.rm = TRUE),
         black_threat = black_threat / max(black_threat, na.rm = TRUE),
         dtp2 = dtp2 / max(dtp2, na.rm = TRUE),
         dtp3 = dtp3 / max(dtp3, na.rm = TRUE),
         black_hood_rank = black_hood_rank / max(black_hood_rank, na.rm = TRUE),
         white_hood_rank = white_hood_rank / max(white_hood_rank, na.rm = TRUE),
         black_threat = black_threat / max(black_threat, na.rm = TRUE),
         blm_eff = blm_eff / max(blm_eff, na.rm = TRUE)) %>% 
  mutate(
    cenfe_ws = ifelse(S4 %in% c("5", "38", "48", "27", "13", "29", "45",
                                "51", "6", "3", "32", "12", "1"), 1, 0),
    cenfe_nc = ifelse(S4 %in% c("35", "42", "24", "28", "17", "16", "26",
                                "50", "14", "15", "36", "23"), 1, 0),
    cenfe_st = ifelse(S4 %in% c("37", "44", "19", "4", "25", "2", "43",
                                "18", "10", "11", "41", "34",
                                "47", "49", "21", "8", "9"), 1, 0),
    cenfe_ne = ifelse(S4 %in% c("33", "39", "31", "7", "22",
                                "40", "30", "46", "20"), 1, 0)
  )

#### reading in zipcode data #### 

cenzip = read_csv('cendat20/zipcode/R12914184_SL860.csv')

cenzip = cenzip %>% 
  mutate(zipcode = `5-digit ZIP Code Tabulation Area`,
         ltpop = log(as.numeric(`Total Population`) + 1),
         pcol = as.numeric(`% Population 25 Years and Over: Bachelor's Degree`),
         pune = as.numeric(`% Civilian Population in Labor Force 16 Years and Over: Unemployed`),
         ppov_lat = 
           as.numeric(`% Hispanic or Latino Population for Whom Poverty  Status Is Determined: Income Below Poverty Level`),
         ppov_blk = 
           as.numeric(`% Black or African American Alone Population for  Whom&nbsp; Poverty Status Is Determined: Income Below Poverty Level`),
         plat = as.numeric(`% Total Population: Hispanic or Latino`),
         pblk = as.numeric(`% Total Population: Black or African American Alone`),
         mhhi = as.numeric(`Median Household Income (In 2019 Inflation Adjusted Dollars)`)) %>% 
  dplyr::select(zipcode, ltpop, pcol, pune, ppov_lat, ppov_blk, plat, pblk, mhhi)

cenzip$ppov_lat_adv = cenzip$ppov_lat - cenzip$ppov_blk

cmps20l = merge(cmps20l, cenzip, by = "zipcode", all.x = TRUE)
cmps20l = cmps20l %>% 
  mutate(miss_zip = ifelse(is.na(ltpop), 1, 0))

cmps20l[, c("ltpop", "pcol", "pune", "ppov_lat", "ppov_blk", "ppov_lat_adv", "plat", "pblk", "mhhi")] = 
  cmps20l[, c("ltpop", "pcol", "pune", "ppov_lat", "ppov_blk",  "ppov_lat_adv", "plat", "pblk", "mhhi")] %>% 
  apply(X = ., MARGIN = 2, FUN = function(x) ifelse(is.na(x), 0, x))

# reading in foreign born data

fobo = read_csv("cendat20/fobo/R12916636_SL860.csv")

fobo = 
  fobo %>% 
  mutate(zipcode = `5-digit ZIP Code Tabulation Area`,
         pfb = as.numeric(`% Total Population: Foreign Born`),
         pnc = as.numeric(`% Total Population: Foreign Born: Not a Citizen`)) %>% 
  dplyr::select(zipcode, pfb, pnc)

cmps20l = merge(cmps20l, fobo, by = "zipcode", all.x = TRUE)
cmps20l$pfb = ifelse(is.na(cmps20l$pfb), 0, cmps20l$pfb)
cmps20l$pnc = ifelse(is.na(cmps20l$pnc), 0, cmps20l$pnc)

#### reading in county level data #### 

# reading in zipcode data that allows merge w/ county data  

data(zipcode)

zipcode = zipcode %>% 
  rename(zipcode = zip) %>% 
  dplyr::select(zipcode, latitude, longitude)

zipcode = zipcode %>% 
  filter(!is.na(longitude)) %>% 
  st_as_sf(., coords = c("longitude", "latitude"))

# reading in county shapefile

cty14 = read_sf('census_data/county/COUNTY_2014_US_SL050_2020-02-24_12-50-56-120/COUNTY_2014_US_SL050_Coast_Clipped.shp')
cty14 = cty14 %>% 
  dplyr::select(GEOID, geometry) %>% 
  rename(fips = GEOID)

# merging to acquire fips codes 
sf_use_s2(FALSE)
st_crs(zipcode) = st_crs(cty14)
zipcode2 = st_intersection(zipcode, cty14)

# merging with survey data

zipcode2$geometry = NULL
cmps20l = merge(cmps20l, zipcode2, by = "zipcode", all.x = TRUE)
cmps20l$fips = ifelse(is.na(cmps20l$fips), 0, cmps20l$fips)
cmps20l$miss_fips = ifelse(cmps20l$fips == 0, 1, 0)

# merging in county data 

ctydta = read_csv("cendat20/R12916672_SL050.csv")

ctydta = ctydta %>% 
  mutate(fips = FIPS,
         ltpop_cty = log(as.numeric(`Total Population`) + 1),
         pfb_cty = as.numeric(`% Total Population: Foreign Born`),
         pnc_cty = as.numeric(`% Total Population: Foreign Born: Not a Citizen`),
         pune_cty = as.numeric(`% Civilian Population in Labor Force 16 Years and Over: Unemployed`),
         plat_cty = as.numeric(`% Total Population: Hispanic or Latino`),
         pblk_cty = as.numeric(`% Total Population: Black or African American Alone`),
         mhhi_cty = as.numeric(`Median Household Income (In 2019 Inflation Adjusted Dollars)`)) %>% 
  dplyr::select(fips, pfb_cty, pnc_cty, ltpop_cty, pune_cty, plat_cty, pblk_cty, mhhi_cty)

cmps20l = merge(cmps20l, ctydta, by = "fips", all.x = TRUE)

cmps20l$miss_cty = ifelse(is.na(cmps20l$ltpop_cty), 1, 0)

cmps20l$ltpop_cty = ifelse(is.na(cmps20l$ltpop_cty), 0, cmps20l$ltpop_cty)
cmps20l$pfb_cty = ifelse(is.na(cmps20l$pfb_cty), 0, cmps20l$pfb_cty)
cmps20l$pune_cty = ifelse(is.na(cmps20l$pune_cty), 0, cmps20l$pune_cty)
cmps20l$plat_cty = ifelse(is.na(cmps20l$plat_cty), 0, cmps20l$plat_cty)
cmps20l$pblk_cty = ifelse(is.na(cmps20l$pblk_cty), 0, cmps20l$pblk_cty)
cmps20l$mhhi_cty = ifelse(is.na(cmps20l$mhhi_cty), 0, cmps20l$mhhi_cty)

#### reading in sc data #### 

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

cmps20l = merge(cmps20l, sc_dat2, by = "fips", all.x = TRUE)

# quickly fixing the sc covars 
cmps20l$tpop_cty = exp(cmps20l$ltpop_cty)
cmps20l$ltotal = log(cmps20l$total + 1)
cmps20l$dep_rate = (cmps20l$total / (cmps20l$pfb_cty * cmps20l$tpop_cty)) * 1000
cmps20l$ltotal = ifelse(is.na(cmps20l$ltotal), 0, cmps20l$ltotal)
cmps20l$dep_rate = ifelse(is.na(cmps20l$dep_rate), 0, cmps20l$dep_rate)
cmps20l$p_l3 = ifelse(is.na(cmps20l$p_l3), 0, cmps20l$p_l3)

#### quick fixes #### 

# quick, fixing so that + = anti-black 

cmps20l$blm_ft = 1 - cmps20l$blm_ft
cmps20l$blm_supports = 1 - cmps20l$blm_supports
cmps20l$blm_eff = 1 - cmps20l$blm_eff
cmps20l$blm_protest = 1 - cmps20l$blm_protest
cmps20l$blm_nprotest = 1 - cmps20l$blm_nprotest
cmps20l$white_hood_rank_diff = cmps20l$white_hood_rank - cmps20l$black_hood_rank
cmps20l$white_hood_rank_diff2 = cmps20l$white_hood_rank - cmps20l$latinx_hood_rank
cmps20l$latinx_hood_rank_diff = cmps20l$latinx_hood_rank - cmps20l$black_hood_rank
cmps20l$dtp = 1 - cmps20l$dtp
cmps20l$dtp3 = 1 - cmps20l$dtp3
cmps20l$dtps = (cmps20l$dtp + cmps20l$dtp3) / 2
cmps20l$polint = abs(cmps20l$Q29 - 4)
cmps20l$lmhhi = log(cmps20l$mhhi + 1)
cmps20l$lmhhi_cty = log(cmps20l$mhhi_cty + 1)
cmps20l$dep_rate = (cmps20l$total / ((cmps20l$pfb_cty / 100) * exp(cmps20l$ltpop_cty))) * 1000
cmps20l$miss_cty = ifelse(is.na(cmps20l$dep_rate), 1, 0)
cmps20l$dep_rate = ifelse(is.na(cmps20l$dep_rate), 0, cmps20l$dep_rate)

# doing same for black dataset 

cmps20b$blm_ft = 1 - cmps20b$blm_ft
cmps20b$blm_supports = 1 - cmps20b$blm_supports
cmps20b$blm_eff = 1 - cmps20b$blm_eff
cmps20b$blm_protest = 1 - cmps20b$blm_protest
cmps20b$blm_nprotest = 1 - cmps20b$blm_nprotest
cmps20b$white_hood_rank_diff = cmps20b$white_hood_rank - cmps20b$black_hood_rank
cmps20b$dtp = 1 - cmps20b$dtp
cmps20b$dtp3 = 1 - cmps20b$dtp3
cmps20b$dtps = (cmps20b$dtp + cmps20b$dtp3) / 2

# and white dataset

cmps20w$blm_ft = 1 - cmps20w$blm_ft
cmps20w$blm_supports = 1 - cmps20w$blm_supports
cmps20w$blm_eff = 1 - cmps20w$blm_eff
cmps20w$blm_protest = 1 - cmps20w$blm_protest
cmps20w$blm_nprotest = 1 - cmps20w$blm_nprotest
cmps20w$white_hood_rank_diff = cmps20w$white_hood_rank - cmps20w$black_hood_rank
cmps20w$dtp = 1 - cmps20w$dtp
cmps20w$dtp3 = 1 - cmps20w$dtp3
cmps20w$dtps = (cmps20w$dtp + cmps20w$dtp3) / 2

# adding covar for resid pref

cmps20l$neigh_service_good = 
  abs(cmps20l$Q70r1 - 5) + 
  abs(cmps20l$Q70r2 - 5) + 
  abs(cmps20l$Q70r3 - 5) + 
  abs(cmps20l$Q70r4 - 5) + 
  abs(cmps20l$Q70r5 - 5)

cmps20l$neigh_rate = abs(cmps20l$Q75 - 5)

cmps20l$neigh_rate = 
  cmps20l$neigh_rate / max(cmps20l$neigh_rate, na.rm = TRUE)

cmps20l$neigh_service_good = 
  cmps20l$neigh_service_good / max(cmps20l$neigh_service_good, na.rm = TRUE)

# falsification test outcomes

cmps20l$threat_asn = abs(cmps20l$Q225r8 - 7) 
cmps20l$threat_asn = cmps20l$threat_asn / max(cmps20l$threat_asn, na.rm = TRUE)
cmps20l$threat_jew = abs(cmps20l$Q225r3 - 7)
cmps20l$threat_jew = cmps20l$threat_jew / max(cmps20l$threat_jew, na.rm = TRUE)

cmps20l$antimus = 
  abs(cmps20l$Q239r1 - 5) + abs(cmps20l$Q239r2 - 5) + 
  abs(cmps20l$Q239r3 - 5) + abs(cmps20l$Q239r4 - 5)
cmps20l$antimus = cmps20l$antimus / max(cmps20l$antimus, na.rm = TRUE)

cmps20l$sexism = 
  cmps20l$Q513_Q516r1 - 1 + abs(cmps20l$Q513_Q516r2 - 5) + abs(cmps20l$Q513_Q516r3 - 5)
cmps20l$sexism = 
  cmps20l$sexism / max(cmps20l$sexism)
cmps20l$ambivsex = 
  abs(cmps20l$Q521_Q528r1 - 5) + abs(cmps20l$Q521_Q528r2 - 5) + 
  abs(cmps20l$Q521_Q528r3 - 5) + abs(cmps20l$Q521_Q528r4 - 5) + 
  abs(cmps20l$Q521_Q528r5 - 5) + abs(cmps20l$Q521_Q528r4 - 5)
cmps20l$ambivsex = cmps20l$ambivsex / max(cmps20l$ambivsex)

# quick edits on black threat measure 

cmps20l = cmps20l %>% 
  mutate(black_threat2 = (Q225r5 - 1),
         white_threat2 = (Q225r6 - 1)) %>% 
  mutate(black_threat2 = black_threat2 / max(black_threat2, na.rm = TRUE),
         white_threat2 = white_threat2 / max(white_threat2, na.rm = TRUE))

#### vote choice #### 

cmps20l$vote_dem = ifelse(cmps20l$Q14 == 2, 1, 0)
cmps20l$voted = ifelse(cmps20l$Q12 == 1 | cmps20l$Q12 == 2, 1, 0)

#### editing datasets for truncated replication #### 

cmps20l = cmps20l[, c(1:61, 1390:1630)]
cmps20b = cmps20b[, c(1:61, 1475:1508)]
cmps20w = cmps20w[, c(1:61, 1475:1506)]

#### saving the dataset(s) ####

save(x = cmps20w, file = "cmps20w_clean.RData")
save(x = cmps20b, file = "cmps20b_clean.RData")
save(x = cmps20l, file = "cmps20l_clean.RData")



