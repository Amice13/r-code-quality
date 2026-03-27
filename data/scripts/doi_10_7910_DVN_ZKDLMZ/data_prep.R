#### Data Prep ####

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Set Working Directory


# Load the P4D Data

p4d_data <- readxl::read_excel("DNP Data (replication).xlsx")

# Make some variable names easier to work with

colnames(p4d_data)[c(2,3,4,6,8,10,12,14,16,18,20,22,24,26,27,29,34)] <- c("country","start.date","end.date","formal","domination",
                                                                            "selection","decisions","scope","agreement","provisions",
                                                                            "groups","women","mediation","mediation.id","int.mediation",
                                                                            "cr.tech.support","gov.tech.support")
# Re-code these variables so they're easier to work with

p4d_data <- p4d_data %>% 
  mutate(cowcode = countrycode::countrycode(country,"country.name","cown"),
         formal_new = case_when(formal == unique(formal)[1] ~ "formal", # Change full answers to single words
                                formal == unique(formal)[2] ~ "informal",
                                formal == unique(formal)[3] ~ "semi-formal"),
         formal_bin = if_else(formal_new == "formal",1,0), # Binary variable of whether a process was formal or not
         scope_new = case_when(scope == unique(scope)[1] ~ "revolutionary",
                               scope == unique(scope)[2] ~ "constitutional",
                               scope == unique(scope)[3] ~ "policy",
                               scope == unique(scope)[4] ~ "no mandate"),
         scope_numeric = case_when(scope_new == "revolutionary" ~ 3, # Numeric ordinal scale for scope of DNP
                                   scope_new == "constitutional" ~ 2,
                                   scope_new == "policy" ~ 1,
                                   scope_new == "no mandate" ~ 0),
         agreement_bin = if_else(agreement == unique(agreement)[1],1,0), # Binary variable whether process reached agreement
         agreement_new = case_when(agreement == unique(agreement)[1] ~ "agreement", 
                                   agreement == unique(agreement)[2] ~ "no agreement",
                                   agreement == unique(agreement)[3] ~ "informal agreement"),
         decisions_new = case_when(decisions == unique(decisions)[1] ~ "majority",
                                   decisions == unique(decisions)[2] ~ "leaders",
                                   decisions == unique(decisions)[3] ~ "none",
                                   decisions == unique(decisions)[4] ~ "consensus"),
         st.year = str_sub(start.date,-4) %>% as.numeric(), # Year DNP started
         end.year = str_sub(end.date,-4) %>% as.numeric(), # Year DNP ended
         provisions.old.elites =  if_else(str_detect(provisions,"Formal guarantees protecting the political") == T, 1,0),         # Provisions variables are binary measures of whether agreement 
         provisions.cr.leaders =  if_else(str_detect(provisions,"Formal guarantees to promote the participation of") == T, 1,0),  # contained each of everal different provisions
         provisions.pwr.share =  if_else(str_detect(provisions,"Formally-defined mechanisms for sharing power") == T, 1,0),
         provisions.implement =  if_else(str_detect(provisions,"A clear implementation process") == T, 1,0),
         provisions.key.issues =  if_else(str_detect(provisions,"Representation of the campaign's key issues") == T, 1,0),
         provisions.pop.approval =  if_else(str_detect(provisions,"A mechanism for getting the general populace") == T, 1,0),
         provisions.conf.build =  if_else(str_detect(provisions,"Formal measures to build confidence") == T, 1,0),
         selection.govt = if_else(str_detect(selection,"Participants were selected by the government") == T, 1,0),  # Binary variables below for each selection method
         selection.elected = if_else(str_detect(selection,"Participants were elected") == T,1,0),
         selection.leaders = if_else(str_detect(selection,"Participants were the leaders of existing groups") == T,1,0),
         selection.appointed = if_else(str_detect(selection,"Appointed by the leaders of previously-existing groups") == T,1,0),
         groups.oppo = if_else(str_detect(groups,"Opposition political parties") == T,1,0), # Binary variables for whether particular groups were included in the negotiations
         groups.csos = if_else(str_detect(groups,"Civil society groups") == T,1,0),
         groups.rel = if_else(str_detect(groups,"Religious groups") == T,1,0),
         groups.ethnic = if_else(str_detect(groups,"Organizations representing ethnic or racial") == T,1,0),
         groups.mil = if_else(str_detect(groups,"The military") == T,1,0),
         groups.progov = if_else(str_detect(groups,"Pro-government political parties") == T,1,0),
         groups.armed = if_else(str_detect(groups,"Armed Non-Governmental Actors") == T,1,0),
         groups.gov = if_else(str_detect(groups,"The Government") == T,1,0),
         women_bin = if_else(women %in% c(unique(women)[1],unique(women)[3]),1,0), # Binary whether women were included
         women_new = case_when(women == unique(women)[1] ~ 1, # Ordinal of whether women were included, and then whether spots set specifically aside
                               women == unique(women)[2] ~ 0,
                               women == unique(women)[3] ~ 2),
         mediation = if_else(mediation == 'Yes',1,0), # Was there domestic mediation?
         int.mediation.bin = if_else(int.mediation == unique(int.mediation)[2],0,1), # Binary of any international mediation
         int.mediation.new = case_when(int.mediation == unique(int.mediation)[1] ~ "consultation", # Types of internationalmediation
                                       int.mediation == unique(int.mediation)[2] ~ "no int mediation",
                                       int.mediation == unique(int.mediation)[3] ~ "enforcer",
                                       int.mediation == unique(int.mediation)[4] ~ "informal role",
                                       int.mediation == unique(int.mediation)[5] ~ "formal role"),
         cr.tech.support.bin = if_else(cr.tech.support == unique(cr.tech.support)[2],0,1), # Binary if CR campaign got intl technical support
         gov.tech.support.bin = if_else(gov.tech.support == unique(gov.tech.support)[2],0,1), # Binary if govt got international technical support
         total.groups = groups.oppo + groups.csos + groups.rel + groups.ethnic + groups.mil + groups.progov + groups.armed + groups.gov,
         start.date = mdy(start.date),
         end.date = mdy(end.date),
         duration = as.numeric(end.date - start.date) + 1,
         log_duration = log(duration),
         decade = case_when(st.year %in% 1940:1950 ~ "1940s",
                            st.year %in% 1951:1960 ~ "1950s",
                            st.year %in% 1961:1970 ~ "1960s",
                            st.year %in% 1971:1980 ~ "1970s",
                            st.year %in% 1981:1990 ~ "1980s",
                            st.year %in% 1991:2000 ~ "1990s",
                            st.year %in% 2001:2010 ~ "2000s",
                            st.year %in% 2011:2020 ~ "2010s",
         )
  ) %>% 
  rename(dnp_id = ID,
         description = `Briefly describe this DNP, including who the main parties were, how it related to the transition, and a short overview of its dynamics and consequences.`)

# Bring in CRT data to merge dialogue processes to transitions

crts <- readxl::read_excel("Civil Resistance Transitions.xlsx") %>% 
  mutate(cowcode = if_else(cowcode == 678 & Startyear == 2011,679,cowcode), # Change Yemen COW code to match VDEM
         id = 1:84 # Create numeric identifier for each transition
  ) %>% 
  select(-transition_id)

vdem <- vdemdata::vdem

vdem.main <- vdem %>% 
  select(country_name,COWcode,year,e_regionpol,v2x_polyarchy,v2x_libdem,v2x_partipdem,v2x_egaldem,v2x_delibdem,e_migdppcln,v2x_regime) %>% 
  filter(year > 1943) %>% 
  mutate(COWcode = case_when(COWcode == 315 ~ 316,
                             TRUE ~ COWcode)) %>% # Change COWcode for Czechoslovakia to get lead and lags
  mutate(regime_bin = if_else(v2x_regime > 1,1,0)) %>% 
  group_by(COWcode) %>% 
  mutate(dem_break = case_when(regime_bin < lag(regime_bin) ~ 1,
                               is.na(lag(regime_bin)) ~ 0,
                               TRUE ~ 0),
         break_year = if_else(dem_break == 1,year,NA_real_)) %>% 
  fill(break_year,.direction = "up") %>% 
  nest(data = -COWcode) %>%
  mutate(leads = purrr::map(data, function(dat) { # Generating 1-5 year leads and 1 year lag variable
    imap_dfc(dat[4:8], ~set_names(purrr::map(1:5, lead, x = .x), 
                                  paste0(.y, '_lead', 1:5)))
  }),
  lags = purrr::map(data, function(dat) {
    imap_dfc(dat[4:8], ~set_names(purrr::map(1, lag, x = .x), 
                                  paste0(.y, '_lag', 1)))
  })) %>% 
  unnest(cols = c(data,leads,lags)) %>% 
  group_by(e_regionpol,year) %>% 
  mutate(region_polyarchy = mean(v2x_polyarchy,na.rm = T),
         region_delibdem = mean(v2x_delibdem,na.rm = T)) %>% 
  ungroup()

crts <- left_join(crts,vdem.main, by = c("cowcode" = "COWcode","Startyear" = "year")) # Join CRTs with V-Dem Data

# Replace East Germany missing lead scores with Germany scores for same year

germany.scores <- vdem.main %>% filter(country_name == "Germany" & year == 1991) %>% 
  select(v2x_polyarchy,v2x_polyarchy_lead1:v2x_polyarchy_lead3,
         v2x_libdem,v2x_libdem_lead1:v2x_libdem_lead3,
         v2x_partipdem,v2x_partipdem_lead1:v2x_partipdem_lead3,
         v2x_egaldem,v2x_egaldem_lead1:v2x_egaldem_lead3,
         v2x_delibdem,v2x_delibdem_lead1:v2x_delibdem_lead3) %>% 
  as.list()

crts[crts$Country=="East Germany",c("v2x_polyarchy_lead2","v2x_polyarchy_lead3","v2x_polyarchy_lead4","v2x_polyarchy_lead5", 
                                    "v2x_libdem_lead2","v2x_libdem_lead3","v2x_libdem_lead4","v2x_libdem_lead5",
                                    "v2x_partipdem_lead2","v2x_partipdem_lead3","v2x_partipdem_lead4","v2x_partipdem_lead5",
                                    "v2x_egaldem_lead2","v2x_egaldem_lead3","v2x_egaldem_lead4","v2x_egaldem_lead5",
                                    "v2x_delibdem_lead2","v2x_delibdem_lead3","v2x_delibdem_lead4","v2x_delibdem_lead5")] <- germany.scores

# Add Identifier of each transition to its corresponding DNPs

p4d_data <- p4d_data %>% 
  left_join(.,select(crts,cowcode,Startyear,id),by =c("cowcode" = "cowcode","st.year" = "Startyear")) %>% 
  mutate(id = case_when(cowcode == 290 & st.year == 1988 ~ 26,
                        cowcode == 475 & st.year == 1994 ~ 28,
                        cowcode == 703 & st.year == 2006 ~ 77,
                        cowcode == 484 & st.year == 1960 ~ 31,
                        cowcode == 452 & st.year == 1954 ~ 30,
                        cowcode == 135 & st.year == 2001 ~ 76,
                        cowcode == 70 & st.year == 2001 ~ 75,
                        cowcode == 310 & st.year == 1989 ~ 8,
                        cowcode == 471 & st.year == 1952 ~ 32,
                        cowcode == 850 & st.year == 1998 ~ 74,
                        cowcode == 553 & st.year == 1957 ~ 33,
                        cowcode == 625 & st.year == 1964 ~ 40,
                        cowcode == 771 & st.year == 1991 ~ 68,
                        cowcode == 434 & st.year == 1989 ~ 34,
                        cowcode == 580 & st.year %in% 1991:1992 ~ 73,
                        cowcode == 551 & st.year == 1990 ~ 35,
                        cowcode == 482 & st.year == 1992 ~ 46,
                        cowcode == 553 & st.year == 1992 ~ 47,
                        cowcode == 475 & st.year == 1998 ~ 48,
                        cowcode == 570 & st.year == 1998 ~ 50,
                        cowcode == 100 & st.year == 1957 ~ 55,
                        cowcode == 860 & st.year == 1983 ~ 52, 
                        cowcode == 42 & st.year %in% 1961:1963 ~ 58,
                        cowcode == 368 & st.year == 1990 ~ 13,
                        cowcode == 145 & st.year == 1984 ~ 61,
                        cowcode == 160 & st.year == 1982 ~ 62,
                        cowcode == 165 & st.year == 1983 ~ 63,
                        cowcode == 560 & st.year == 1990 ~ 36,
                        cowcode == 140 & st.year %in% 1984:1987 ~ 64,
                        country == "Serbia" & st.year == 2000 ~ 18,
                        cowcode == 790 & st.year %in% 2008:2014 ~ 78,
                        cowcode == 450 ~ 53,
                        cowcode == 369 & st.year == 2013 ~ 82,
                        cowcode == 315 & st.year %in% 1989:1992 ~ 66,
                        cowcode == 101 & st.year == 1959 ~ 56,
                        cowcode == 616 & st.year %in% 2011:2015 ~ 23,
                        TRUE ~ as.numeric(id)
  )) 

p4d_summary <- p4d_data %>% 
  group_by(cowcode) %>% 
  summarize(dnps = n()) %>% 
  mutate(iso_a3 = countrycode::countrycode(cowcode,"cown","iso3c"))


dnps <- p4d_data %>% 
  nest(dnp_data = -id)


full.dataset <- left_join(crts,dnps,by = "id") %>% 
  mutate(num.dnps = map_dbl(dnp_data, function(x){ # Get number of DNPs per transition
    if(is.null(x)) {
      return(0)
    } else {
      return(nrow(x))
    }
  }),
  any.intl.mediation = map_dbl(dnp_data, function(x){ # Get whether international mediation happened in any DNP in this transition
    if(is.null(x)) {
      return(0)
    } else {
      return(max(x$int.mediation.bin,na.rm = T))
    }
  }) %>% if_else(. == -Inf,0,.),
  any.dom.mediation = map_dbl(dnp_data, function(x){ # Get whether international mediation happened in any DNP in this transition
    if(is.null(x)) {
      return(0)
    } else {
      return(max(x$mediation,na.rm = T))
    }
  }) %>% if_else(. == -Inf,0,.),
  any.cso.partip = map_dbl(dnp_data, function(x){ # Get whether CSOs participated in any DNP in this transition
    if(is.null(x)) {
      return(0)
    } else {
      return(max(x$groups.csos,na.rm = T))
    }
  }),
  any.women.partip = map_dbl(dnp_data, function(x){ # Get whether women participated in any DNP in this transition
    if(is.null(x)) {
      return(0)
    } else {
      return(max(x$women_bin,na.rm = T))
    }
  }),
  any.agreement = map_dbl(dnp_data, function(x){ # Get whether any of the DNPs in this transition ended in agreement
    if(is.null(x)) {
      return(0)
    } else {
      return(max(x$agreement_bin,na.rm = T))
    }
  }),
  domination.mean = map_dbl(dnp_data, function(x){ # Average level of old/new domination
    if(is.null(x)) {
      return(0)
    } else {
      return(mean(x$domination,na.rm = T))
    }
  }),
  domination.min = map_dbl(dnp_data, function(x){ # Lowest level of old/new domination (most prominent level of old domination)
    if(is.null(x)) {
      return(0)
    } else {
      return(min(x$domination,na.rm = T))
    }
  }),
  domination.max = map_dbl(dnp_data, function(x){ # highest level of old/new domination (most prominent level of new domination)
    if(is.null(x)) {
      return(0)
    } else {
      return(max(x$domination,na.rm = T))
    }
  }),
  non.govt.appointed = map_dbl(dnp_data, function(x){
    if(is.null(x)) {
      return(0)
    } else {
      return(max(x$selection.elected,x$selection.leaders,x$selection.appointed,na.rm = T))
    }
  }),
  non.govt.dominated = map_dbl(dnp_data, function(x){
    if(is.null(x)) {
      return(NA_real_)
    } else {
      if(max(x$domination,na.rm = T) > 1){
        return(1)
      } else {
        return(0)
      }
    }
  }),
  some.scope = map_dbl(dnp_data, function(x){
    if(is.null(x)) {
      return(0)
    } else {
      if_else(max(x$scope_numeric,na.rm = T) == 0,0,1)
    }
  }),
  inclusive.decisions = map_dbl(dnp_data, function(x){
    if(is.null(x)) {
      return(NA_real_)
    } else {
      if_else(max(str_detect(x$decisions_new,"leaders"))==1 & length(unique(x$decisions_new)) == 1,0,1)
    }
  }),
  dnp.binary = if_else(num.dnps > 0,1,0),
  decade = case_when(Startyear %in% 1940:1950 ~ "1940s",
                     Startyear %in% 1951:1960 ~ "1950s",
                     Startyear %in% 1961:1970 ~ "1960s",
                     Startyear %in% 1971:1980 ~ "1970s",
                     Startyear %in% 1981:1990 ~ "1980s",
                     Startyear %in% 1991:2000 ~ "1990s",
                     Startyear %in% 2001:2010 ~ "2000s",
                     Startyear %in% 2011:2020 ~ "2010s",
  ),
  index = any.cso.partip + any.women.partip + non.govt.appointed + non.govt.dominated + inclusive.decisions + some.scope
  )
