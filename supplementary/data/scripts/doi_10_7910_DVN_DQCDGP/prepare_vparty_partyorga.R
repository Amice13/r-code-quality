
##### 0 ToC Prepare V-Party ----

# Aim of this script: Prepare V-Party for analyses

# 1 Load main data
# 2 Generate additional variables
# 3 Prepare external data for cross-validating organizational items
# 4 Save env




##### 1 Prerequisites ----


# clean env
rm(list = ls())


# load packages
library(tidyverse)
library(summarytools)


# load v-party, version: V1
vparty  <- readRDS("./data/V-Dem-CPD-Party-V1.rds")

    # basic infos
    vparty %>% select(country_id) %>% n_distinct()                                                                          # 178 countries in total
    vparty %>% filter(year >= 1970 & !is.na(v2xpa_illiberal)) %>% select(country_id) %>% n_distinct()                       # 168 countries with expert codings after 1970
    vparty %>% filter(year >= 1970 & !is.na(v2xpa_illiberal)) %>% select(v2paid) %>% n_distinct()                           # 1941 parties with expert codings after 1970
    vparty %>% filter(year >= 1970 & !is.na(v2xpa_illiberal)) %>% group_by(country_id) %>% select(year) %>%  n_distinct()   # 1755 elections with expert codings after 1970
    vparty %>% filter(year >= 1970 & !is.na(v2xpa_illiberal)) %>% summarise(n())                                            # 6313 election-year observations
    

# load party facts (code snippet taken from https://partyfacts.herokuapp.com/download/)
partyfacts <- read_csv("./data/partyfacts-external-parties.csv", guess_max = 50000) %>% filter(!is.na(partyfacts_id))


# grab additional data from v-dem, version 10
v10 <-
    readRDS("./data/V-Dem-CY-Full+Others-v10.rds") %>% 
    select(country_id, year, v2x_regime, v2reginfo, v2elparlel, v2elloeldm, v2elthresh, v2x_polyarchy, v2ellocons, v2elparlel)


# grab system type (presidential vs. parliamentary) from quality of government, version jan19

    # run only once to save space in the repo (~ large files excluded via gitignore)
    # qog <- read.csv("./data/qog_std_ts_jan19.csv")
    # saveRDS(qog, file = "./data/qog_std_ts_jan19.rds")

qog <-
    readRDS("./data/qog_std_ts_jan19.rds") %>% 
    select(ccodecow, year, dpi_system) %>%
    filter(!is.na(dpi_system)) %>% 
    mutate(parl = if_else(dpi_system != 0, 1, 0))


# merge data
df <- vparty
df <- df %>% left_join(v10, by = c("country_id", "year"))
df <- df %>% left_join(qog, by = c("COWcode" = "ccodecow", "year" = "year"))


# drop data < 1970, independents and empty rows
df <- df %>% filter(year >= 1970 & v2paenname != "independent" & !is.na(v2paid))


# set observations that have less than three coders to missing to be excluded from analyses (cf. cautionary notes in codebook)
df <- 
    df %>% 
    mutate(
        v2palocoff = if_else(v2palocoff_nr < 3, NA_real_, v2palocoff),
        v2paactcom = if_else(v2paactcom_nr < 3, NA_real_, v2paactcom), 
        v2pasoctie = if_else(v2pasoctie_nr < 3, NA_real_, v2pasoctie), 
        v2panom    = if_else(v2panom_nr    < 3, NA_real_, v2panom), 
        v2padisa   = if_else(v2padisa_nr   < 3, NA_real_, v2padisa), 
        v2paind    = if_else(v2paind_nr    < 3, NA_real_, v2paind)
    )

# assign Israel to "Western Europe and North America" as geo-political region as it has more in common with those states than with the MENA ones
df <- df %>% mutate(e_regionpol_6C = replace(e_regionpol_6C, country_name == "Israel", 5))





##### 2 Generate additional vars ----


# add labels to regime type
df$v2x_regime_lab <- factor(df$v2x_regime,
                    levels = c(0,1,2,3),
                    labels = c("Closed Autocracy", "Electoral autocracy", "Electoral democracy", "Liberal democracy"))


df$e_regionpol_6C_lab <- factor(df$e_regionpol_6C,
                    levels = c(1,2,3,4,5,6),
                    labels=c("Eastern Europe and Central Asia",
                             "Latin America and the Caribbean",
                             "The Middle East and Northern Africa",
                             "Sub-Saharan Africa", 
                             "Western Europe and North America",
                             "Asia and Pacific"))

# election id   
df <-
    df %>% 
    arrange(country_id, year, v2paid) %>% 
    group_by(country_id, year) %>% 
    mutate(election_id = cur_group_id()) %>%
    ungroup()


# add log(consecutive number of elections)
df <- df %>% mutate(logconsecel = log(v2ellocons + 1))
#hist(df$v2ellocons) # right-skewed
#summary(df$v2ellocons)
#hist(df$logconsecel)


# democracy and electoral system dummy
df <- 
    df %>% 
    mutate(
        democracy = if_else(v2x_regime >= 2, 1, 0),    # 1 = country is at least electoral democracy
        proportional = if_else(v2elparlel == 1, 1, 0), # 1 = majoritarian
        mixed = if_else(v2elparlel == 2, 1, 0)         # 1 = plural-majoritarian
    ) 


# government support
df <- 
    df %>%
    group_by(v2paid) %>%                                 # group by party id
    arrange(v2paid, year) %>%                            # sort elections
    mutate(
        l.govsup    = lag(v2pagovsup, n = 1),            # government support at previous election (lag)
        statefund   = if_else(v2pafunds_0 > 0.50, 1, 0), # 1 = formal state subsidies in place
        seniorgov   = if_else(l.govsup == 0, 1, 0),      # 1 = party was senior partner incl. HoS 
        juniorgov   = if_else(l.govsup == 1, 1, 0),      # 1 = party was junior partner
        informalsup = if_else(l.govsup == 2, 1, 0),      # 1 = party supported gov but not officially represented
        opposition  = if_else(l.govsup == 3, 1, 0),      # 1 = party was in opposition
    ) %>% 
    ungroup()
#df %>% select(country_name, year, v2paid, v2pashname, v2pagovsup, l.govsup, seniorgov, juniorgov) %>% View() # visual inspection => works


# discrete time variable (i.e. party-election counter)
df <-
    df %>%
    group_by(v2paid) %>%
    mutate(
        counter  = row_number(),
        counter1 = counter - 1,
        logcounter1 = log(counter1 + 1),
    ) %>% 
    ungroup()
#df %>% select(country_name, year, v2paid, v2pashname, counter, counter1) %>% View() # visual inspection => works
#descr(df$counter1)


# identify events of party death
df <- 
    df %>% 
    group_by(country_id) %>% 
    mutate(last_year_observed = if_else(year == max(year), 1, 0)) %>%   # identify right-censoring 
    group_by(v2paid) %>% 
    mutate(
        death = if_else(counter1 == max(counter1), 1, 0),              # last time party observed
        death1 = case_when(last_year_observed == 1 ~ 0, TRUE ~ death)  # if right-censored no death for the party
    ) %>% 
    ungroup()

#df %>% select(country_id, year, v2paid, last_year_observed, counter1, death, death1) %>% arrange(country_id, v2paid, year) %>% View() # works
#table(df$death1)


# standardize organizational variables
df <- 
    df %>% 
    mutate(
        v2palocoffstd = as.numeric(scale(v2palocoff)),
        v2paactcomstd = as.numeric(scale(v2paactcom)),
        v2pasoctiestd = as.numeric(scale(v2pasoctie)),
        v2panomstd    = as.numeric(scale(v2panom)),
        v2padisastd   = as.numeric(scale(v2padisa)),
        v2paindrev    = v2paind * (-1), # switch v2paind so that high values denote less personalization - consistent with v2panom
        v2paindrevstd = as.numeric(scale(v2paindrev))
    )

# construct index of organizational extensiveness and intra-party power concentration
df <-
    df %>%
    mutate(
        orgext      = v2palocoffstd + v2paactcomstd + v2pasoctiestd,
        powercon    = v2paindrevstd + v2panomstd,
        cohesion    = v2padisastd,
        orgext_uw   = v2palocoff + v2paactcom + v2pasoctie,          # alt version, unstandardized 
        powercon_uw = v2paind * (-1) + v2panom,                      # alt version, unstandardized 
    )





##### 3 Prepare external data on party organizational topics ----


### load IPOD data which uses CMP party codes (Giger and Schumacher 2015)

load("./data/data_ipod.Rdata")

dataset_1 <- partyfacts %>% filter(dataset_key == "vparty")
dataset_2 <- partyfacts %>% filter(dataset_key == "manifesto")

link_table1 <- 
    dataset_1 %>% 
    inner_join(dataset_2, by = c("partyfacts_id" = "partyfacts_id")) %>% 
    mutate(
        v2paid = as.numeric(dataset_party_id.x),
        cmp = as.numeric(dataset_party_id.y)
    ) %>% 
    select(v2paid, cmp)

ipod <- data_ipod %>% right_join(link_table1, by = c("cmp" = "cmp")) %>% arrange(country_name, v2paid, year)


    # extract Rohrschneider & Whitefield (2012) data from IPOD
crossv_rw12 <- 
    ipod %>% 
    select(party_id, year, cmp, v2paid, party_name_english, country_name, q23iaimp, q23ibimp, q23icimp, q21, q22) %>%
    mutate(drop_out = rowSums(is.na(.))) %>% 
    filter(drop_out < 5) %>% 
    select(-drop_out)

       
    # extract Giger & Schumacher's own estimates from IPOD
crossv_gs15 <- 
    ipod %>% 
    select(party_id, year, cmp, v2paid, party_name_english, country_name, janda_bopla, lh_bopla, rw_bopla, lhrw_bopla, bopla) %>%
    mutate(drop_out = rowSums(is.na(.))) %>% 
    filter(drop_out < 5) %>% 
    select(-drop_out)


    # extract Katz & Mair (1992) data from IPOD
crossv_km92 <- 
    ipod %>% 
    select(party_id, year, cmp, v2paid, party_name_english, country_name, B1_partyunits, D5_candsel_const, D5_candsel_reg) %>%
    mutate(drop_out = rowSums(is.na(.))) %>% 
    filter(drop_out < 3) %>% 
    select(-drop_out)

           
    # extract Harmel & Janda (1995) data from IPOD
crossv_hj95 <- 
    ipod %>% 
    select(party_id, year, cmp, v2paid, party_name_english, country_name, candsel) %>%
    mutate(drop_out = rowSums(is.na(.))) %>% 
    filter(drop_out < 1) %>% 
    select(-drop_out)


    # extract Janda (1980) data from IPOD
crossv_ja80 <- 
    ipod %>% 
    select(party_id, year, cmp, v2paid, party_name_english, country_name, leadfact, strafact, ideofac, leadcon, formulpol, selparliacan, natofstruc, selnatlead, exnorg) %>%
    mutate(drop_out = rowSums(is.na(.))) %>% 
    filter(drop_out < 8) %>% 
    select(-drop_out)


    # extract Lundell and Bille data from IPOD
crossv_lb91 <- 
    ipod %>% 
    select(party_id, year, cmp, v2paid, party_name_english, country_name, centlund, centbille, cent) %>%
    mutate(drop_out = rowSums(is.na(.))) %>% 
    filter(drop_out < 3) %>% 
    select(-drop_out)


    # extract Laver & Hunt (1992) data from IPOD
crossv_lh92 <- 
    ipod %>% 
    select(party_id, year, cmp, v2paid, party_name_english, country_name, LeadsIPPMS, LegisIPPMS, ActsIPPMS, LeadsCPMS, LegisCPMS, ActsCPMS) %>%
    mutate(drop_out = rowSums(is.na(.))) %>% 
    filter(drop_out < 6) %>% 
    select(-drop_out)



### read Political Parties Database Project (Poguntke et al. )

data_ppdb <- read_csv("./data/PPDB_Round1a_1b_v1_CSV.csv")


    # add party facts ID (code snippet taken from https://partyfacts.herokuapp.com/download/)
dataset_3 <- partyfacts %>% filter(dataset_key == "ppdb")

link_table2 <- 
    dataset_1 %>% 
    inner_join(dataset_3, by = c("partyfacts_id" = "partyfacts_id")) %>% 
    mutate(
        v2paid = as.numeric(dataset_party_id.x),
        PTYID = as.character(dataset_party_id.y)
    ) %>% 
    select(v2paid, PTYID)

ppdb <- data_ppdb %>% right_join(link_table2, by = c("PTYID" = "PTYID")) %>% arrange(COUNTRY, v2paid, YEAR)


    # extract ppdb questions sensible for cross-validation
ppdb <- ppdb %>% mutate(across(where(is.numeric), ~na_if(., -888))) # re-set missing values
ppdb <- ppdb %>% mutate(across(where(is.numeric), ~na_if(., -999))) # re-set missing values


    # add "additive index of affiliated organizations present at party congress and leadership power
ppdb <-
    ppdb %>%
    mutate(
        A58CONWOMEN_rec  = plyr::mapvalues(A58CONWOMEN,  from=c(1, 2, NA), to=c(1, 0, 0)),
        A59CONYOUTH_rec  = plyr::mapvalues(A59CONYOUTH,  from=c(1, 2, NA), to=c(1, 0, 0)),
        A60CONSENIOR_rec = plyr::mapvalues(A60CONSENIOR, from=c(1, 2, NA), to=c(1, 0, 0)),
        A61CONBIZ_rec    = plyr::mapvalues(A61CONBIZ,    from=c(1, 2, NA), to=c(1, 0, 0)),
        A62CONFARM_rec   = plyr::mapvalues(A62CONFARM,   from=c(1, 2, NA), to=c(1, 0, 0)),
        A63CONETHNIC_rec = plyr::mapvalues(A63CONETHNIC, from=c(1, 2, NA), to=c(1, 0, 0)),
        A64CONRELIG_rec  = plyr::mapvalues(A64CONRELIG,  from=c(1, 2, NA), to=c(1, 0, 0)),
        A65CONUNION_rec  = plyr::mapvalues(A65CONUNION,  from=c(1, 2, NA), to=c(1, 0, 0)),
        A66CONCORP_rec   = plyr::mapvalues(A66CONCORP,   from=c(1, 2, NA), to=c(1, 0, 0)),
        C11DEPUTY_rec    = plyr::mapvalues(C11DEPUTY,    from=c(1, 2, NA), to=c(1, 0, 0)),
        C13LDREXC_rec    = plyr::mapvalues(C13LDREXC,    from=c(1, 2, NA), to=c(1, 0, 0)),
        C14LDRCON_rec    = plyr::mapvalues(C14LDRCON,    from=c(1, 2, NA), to=c(1, 0, 0)),
        C15LDRSUM1_rec   = plyr::mapvalues(C15LDRSUM1,   from=c(1, 2, NA), to=c(1, 0, 0)),
        C16DRSUM2_rec    = plyr::mapvalues(C16DRSUM2,    from=c(1, 2, NA), to=c(1, 0, 0)),
        C17LDRROLE1_rec  = plyr::mapvalues(C17LDRROLE1,  from=c(1, 2, NA), to=c(1, 0, 0)),
        C18LDRROLE2_rec  = plyr::mapvalues(C18LDRROLE2,  from=c(1, 2, NA), to=c(1, 0, 0)),
        C19LDRROLE3_rec  = plyr::mapvalues(C19LDRROLE3,  from=c(1, 2, NA), to=c(1, 0, 0)),
        A90EXCLDR_rec    = if_else(A90EXCLDR > 0, 1, 0),
        CON_AFFIL_ORGA   = A58CONWOMEN_rec + A59CONYOUTH_rec + A60CONSENIOR_rec + A61CONBIZ_rec + A62CONFARM_rec + A63CONETHNIC_rec + A64CONRELIG_rec + A65CONUNION_rec + A66CONCORP_rec,
        LEADER_STRENGTH  = C11DEPUTY_rec + C13LDREXC_rec + C14LDRCON_rec + C15LDRSUM1_rec + C16DRSUM2_rec + C17LDRROLE1_rec + C18LDRROLE2_rec + C19LDRROLE3_rec + A90EXCLDR_rec,
    )

    
    # reduce size of object
crossv_ppdb <- 
    ppdb %>% 
    select(PTYNAME, YEAR, PTYID, v2paid, COUNTRY, CR16ALLMBR, A26STAFHQ, A46LOWLVL, A48LOWNUM, A56UNIONNUM, A57CORPNUM, CON_AFFIL_ORGA, LEADER_STRENGTH)



### read Democratic Accountability and Linkages Project (Kitschelt 2013)

data_dalp <-
    read_csv("./data/partylevel_20130907.csv") %>%
    mutate(
        mergeid = paste0(ccodewb, "-", partynum),
        year = 2009)


    # add party facts ID (code snippet taken from https://partyfacts.herokuapp.com/download/)
dataset_4 <- partyfacts %>% filter(dataset_key == "kitschelt")

link_table3 <- 
    dataset_1 %>% 
    inner_join(dataset_4, by = c("partyfacts_id" = "partyfacts_id")) %>% 
    mutate(
        v2paid = as.numeric(dataset_party_id.x),
        mergeid = as.character(dataset_party_id.y)
    ) %>% 
    select(v2paid, mergeid)

dalp <- data_dalp %>% right_join(link_table3, by = c("mergeid" = "mergeid")) %>% arrange(country, v2paid, year)


dalp <- dalp %>% mutate(across(where(is.numeric), ~na_if(., 99))) # re-set "Don't know"


    # reduce size of object
crossv_dalp <- 
    dalp %>% 
    select(country, party, partynum, pengacro, mergeid, v2paid, year, a1, a5, a5a, a6, a6a)





##### 4 Clean and save env for analyses ----


    # clean env
rm(dataset_1, dataset_2, dataset_3, dataset_4, link_table1, link_table2, link_table3, partyfacts,
   data_ipod, data_ppdb, data_dalp, ipod, ppdb, dalp,
   qog, v10, vparty)


    # save workspace
save.image(file = "./data/vparty_for_partyorga.Rdata")

