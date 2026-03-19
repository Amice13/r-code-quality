########################################################################################################################################
### Replication code for "Why Anxious People Lean to the Left on Economic Policy: personality, Social Exclusion, and Redistribution" ###
########################################################################################################################################

# Install/Load Packages
pacman::p_load(haven, readr, readxl, dplyr, ltm, pspearman, lme4, lmerTest, tidytext, Cairo, ggplot2, geomtextpath, ggeffects)

##################################################
############### ANES data cleaning ###############
##################################################

### 2012 ANES
ANES12 <- read_dta("ANES 2012.dta")

ANES12 <-
  ANES12 %>% 
  mutate(
  
    # demographics
    age = (ifelse(dem_age_r_x < 0, NA, dem_age_r_x) - 17) / 73,
    male = as.numeric(2 - gender_respondent_x),
    race = suppressWarnings(relevel(factor(car::recode(as.numeric(dem_raceeth_x), "-9=NA; 1='White'; 2='Black'; 3:4='Other'; 5='Hispanic'; 6='Other'")), "White")),
    education = ifelse(dem_edu %in% 1:16, (dem_edu - 1) / 15, NA),
    income = ifelse(inc_incgroup_pre > 0, (inc_incgroup_pre - 1) / 27, NA),
    own_home = ifelse(ecperil_home %in% 2:3, 1, ifelse(ecperil_home %in% c(1,4), 0, NA)),
    unemployed = ifelse(dem_empstatus_2digitfin_x %in% -9:-8, NA, ifelse(dem_empstatus_2digitfin_x %in% 20:40, 1, 0)),
    uninsured = ifelse(health_insured %in% -9:-8, NA, ifelse(health_insured==2, 1, 0)),
    lib_con = ifelse(libcpre_self %in% -8:-2, 0.5, ifelse(libcpre_self %in% 1:7, (7 - libcpre_self) / 6, NA)),
    
    # personality
    extraverted = ifelse(tipi_extra %in% 1:7, (tipi_extra - 1) / 6, NA),
    critical_r = ifelse(tipi_crit %in% 1:7, (7 - tipi_crit) / 6, NA),
    dependable = ifelse(tipi_dep %in% 1:7, (tipi_dep - 1) / 6, NA),
    anxiety = ifelse(tipi_anx %in% 1:7, (tipi_anx - 1) / 6, NA),
    open = ifelse(tipi_open %in% 1:7, (tipi_open - 1) / 6, NA),
    reserved_r = ifelse(tipi_resv %in% 1:7, (7 - tipi_resv) / 6, NA),
    sympathetic = ifelse(tipi_warm %in% 1:7, (tipi_warm - 1) / 6, NA),
    disorganized_r = ifelse(tipi_disorg %in% 1:7, (7 - tipi_disorg) / 6, NA),
    volatility = ifelse(tipi_calm %in% 1:7, (7 - tipi_calm) / 6, NA),
    conventional_r = ifelse(tipi_conv %in% 1:7, (7 - tipi_conv) / 6, NA),
    openness = (open + conventional_r) / 2,
    conscientiousness = (dependable + disorganized_r) / 2,
    extraversion = (extraverted + reserved_r) / 2,
    agreeableness = (critical_r + sympathetic) / 2,
    neuroticism = (anxiety + volatility) / 2,
    
    # economic policy
    income_equality = ifelse(cses_govtact %in% 1:5, (5 - cses_govtact) / 4, NA),
    guar_job_living = ifelse(guarpr_self == -2, 0.5, ifelse(guarpr_self %in% 1:7, (7 - guarpr_self) / 6, NA)),
    soc_sec_spend = ifelse(fedspend_ss == 3, 0.5, ifelse(fedspend_ss %in% 1:2, 2 - fedspend_ss, NA)),
    welfare_spend = ifelse(fedspend_welfare == 3, 0.5, ifelse(fedspend_welfare %in% 1:2, 2 - fedspend_welfare, NA)),
    poor_spend = ifelse(fedspend_poor == 3, 0.5, ifelse(fedspend_poor %in% 1:2, 2 - fedspend_poor, NA)),
    gov_services = ifelse(spsrvpr_ssself == -2, 0.5, ifelse(spsrvpr_ssself %in% 1:7, (spsrvpr_ssself - 1) / 6, NA)),
    public_insurance = ifelse(inspre_self == -2, 0.5, ifelse(inspre_self %in% 1:7, (7 - inspre_self) / 6, NA)),
    wealthy_tax = ifelse(milln_milltax_x %in% 1:7, (7 - milln_milltax_x) / 6, NA),
    
    econ_policy = (income_equality + guar_job_living + soc_sec_spend + welfare_spend + poor_spend + gov_services + public_insurance + wealthy_tax) / 8,
    
    # social policy
    gay_adoption = ifelse(gayrt_adopt %in% 1:2, 2 - gayrt_adopt, NA),
    gay_marriage = ifelse(gayrt_marry %in% 1:3, (3 - gayrt_marry) / 2, NA),
    abortion = ifelse(abortpre_4point %in% 1:4, (abortpre_4point - 1) / 3, NA),
    marijuana = ifelse(pot_legal == 3, 0.5, ifelse(pot_legal %in% 1:2, 2 - pot_legal, NA)),
    death_penalty = ifelse(penalty_favopp_x %in% 1:5, (penalty_favopp_x - 1) / 4, NA),
    gun_control = ifelse(gun_control %in% 1:3, ifelse(gun_control == 3, 0.5, 2 - gun_control), NA),
    immigration = ifelse(immig_policy %in% 1:4, (immig_policy - 1) / 3, NA),
    aff_action = ifelse(aa_uni_x %in% 1:7, (7 - aa_uni_x) / 6, NA),
    
    social_policy = (gay_adoption + gay_marriage + abortion + marijuana + death_penalty + gun_control + immigration + aff_action) / 8,
    
    # political engagement
    campaign_interest = ifelse(interest_following > 0, (3 - interest_following) / 2, 0),
    attention_politics = ifelse(interest_attention > 0, (5 - interest_attention) / 4, NA),
    
    political_engagement = (campaign_interest + attention_politics) / 2,
    
    # year indicator
    year = "2012",
    
    .keep = "none"
  )


### 2016 ANES
ANES16 <- read_dta("ANES 2016.dta")

ANES16 <- 
  ANES16 %>%
  mutate(
    
    # demographics
    age = (ifelse(V161267 %in% -9:-8, NA, V161267) - 18) / 72,
    male = ifelse(V161342 %in% -9:-8, NA, ifelse(V161342 == 3, 0, 2 - V161342)),
    race = suppressWarnings(relevel(factor(car::recode(as.numeric(V161310x), "-9:-2=NA; 1='White'; 2='Black'; 3:4='Other'; 5='Hispanic'; 6='Other'")), "White")),
    education = ifelse(V161270 %in% 1:16, (V161270 - 1) / 15, NA),
    income = ifelse(V161361x > 0, (V161361x - 1) / 27, NA),
    own_home = ifelse(V161334 %in% 2:3, 1, ifelse(V161334 %in% c(1, 4), 0, NA)),
    unemployed = ifelse(V161275x == -9, NA, ifelse(V161275x %in% 20:40, 1, 0)),
    uninsured = ifelse(V161112 %in% -9:-8, NA, ifelse(V161112 == 2, 1, 0)),
    lib_con = ifelse(V161126 == -8, 0.5, ifelse(V161126 %in% 1:7, (7 - V161126) / 6, NA)),
    
    # personality
    extraverted = ifelse(V162333 %in% 1:7, (V162333 - 1) / 6, NA),
    critical_r = ifelse(V162334 %in% 1:7, (7 - V162334) / 6, NA),
    dependable = ifelse(V162335 %in% 1:7, (V162335 - 1) / 6, NA),
    anxiety = ifelse(V162336 %in% 1:7, (V162336 - 1) / 6, NA),
    open = ifelse(V162337 %in% 1:7, (V162337 - 1) / 6, NA),
    reserved_r = ifelse(V162338 %in% 1:7, (7 - V162338) / 6, NA),
    sympathetic = ifelse(V162339 %in% 1:7, (V162339 - 1) / 6, NA),
    disorganized_r = ifelse(V162340 %in% 1:7, (7 - V162340) / 6, NA),
    volatility = ifelse(V162341 %in% 1:7, (7 - V162341) / 6, NA),
    conventional_r = ifelse(V162342 %in% 1:7, (7 - V162342) / 6, NA),
    openness = (open + conventional_r) / 2,
    conscientiousness = (dependable + disorganized_r) / 2,
    extraversion = (extraverted + reserved_r) / 2,
    agreeableness = (critical_r + sympathetic) / 2,
    neuroticism = (anxiety + volatility) / 2,
    
    # economic policy
    income_equality = ifelse(V162276 %in% 1:5, (5 - V162276) / 4, NA),
    guar_job_living = ifelse(V161189 == 99, 0.5, ifelse(V161189 %in% 1:7, (7 - V161189) / 6, NA)),
    soc_sec_spend = ifelse(V161205 == 3, 0.5, ifelse(V161205 %in% 1:2, 2 - V161205, NA)),
    welfare_spend = ifelse(V161209 == 3, 0.5, ifelse(V161209 %in% 1:2, 2 - V161209, NA)),
    poor_spend = ifelse(V161211 == 3, 0.5, ifelse(V161211 %in% 1:2, 2 - V161211, NA)),
    gov_services = ifelse(V161178 == 99, 0.5, ifelse(V161178 %in% 1:7, (V161178 - 1) / 6, NA)),
    public_insurance = ifelse(V161184 == 99, 0.5, ifelse(V161184 %in% 1:7, (7 - V161184) / 6, NA)),
    wealthy_tax = ifelse(V162140 == 3, 0.5, ifelse(V162140 %in% 1:2, 2 - V162140, NA)),
    
    econ_policy = (income_equality + guar_job_living + soc_sec_spend + welfare_spend + poor_spend + gov_services + public_insurance + wealthy_tax) / 8,
    
    # social policy
    gay_adoption = ifelse(V161230 %in% 1:2, 2 - V161230, NA),
    gay_marriage = ifelse(V161231 %in% 1:3, (3 - V161231) / 2, NA),
    abortion = ifelse(V161232 %in% 1:4, (V161232 - 1) / 3, NA),
    marijuana = ifelse(V162179 == 3, 0.5, ifelse(V162179 %in% 1:2, 2 - V162179, NA)),
    death_penalty = ifelse(V161233x %in% 1:4, (V161233x - 1) / 3, NA),
    gun_control = ifelse(V161187 == 3, 0.5, ifelse(V161187 %in% 1:2, 2 - V161187, NA)),
    immigration = ifelse(V161192 %in% 1:4, (V161192 - 1) / 3, NA),
    aff_action = ifelse(V161204x %in% 1:7, (7 - V161204x) / 6, NA),
    
    social_policy = (gay_adoption + gay_marriage + abortion + marijuana + death_penalty + gun_control + immigration + aff_action) / 8,
    
    # political engagement
    campaign_interest = (3 - V161004) / 2,
    attention_politics = (5 - V161003) / 4,
    
    political_engagement = (campaign_interest + attention_politics) / 2,
    
    # year indicator
    year = "2016",
    
    .keep = "none"
  )

# Merge ANES Surveys
ANES <- data.frame(rbind(ANES12, ANES16))
ANES$year <- relevel(factor(ANES$year), "2016")

# Reverse Coded Variables
ANES$`income (reversed)` = 1 - ANES$income
ANES$`do not own home` = 1 - ANES$own_home






##################################################
################ CES data cleaning ###############
##################################################

### CES Policy Longitudinal File ###
CES_Policy <- suppressMessages(read_delim("CCES_Policy.tab", delim = "\t", escape_double = FALSE, trim_ws = TRUE))

# Create caseid
CES_Policy <- 
  CES_Policy %>%
  mutate(
    
    # respondent id
    caseid = case_id,
    
    # economic policy
    aca = healthcare_aca - 1,
    welfare = ifelse(spending_welfare %in% 1:5, (5 - spending_welfare) / 4, NA),
    healthcare = ifelse(spending_healthcare %in% 1:5, (5 - spending_healthcare) / 4, NA),
    econ_policy = (aca + welfare + healthcare) / 3,
    
    # social policy
    abortion = ifelse(abortion_prohibition %in% 1:2, abortion_prohibition - 1, NA),
    gun_control = ifelse(guns_assaultban %in% 1:2, 1 - guns_assaultban, NA),
    gay_marriage = ifelse(gaymarriage_legalize %in% 1:2, 2 - gaymarriage_legalize, NA), 
    immigration = ifelse(immig_border %in% 1:2, immig_border - 1, NA),
    social_policy = (CES_Policy + abortion + gun_control) / 3,
    
    .keep = "none"
  )

### 2016 University of California, Merced CCES ###
CCES16_UCM <- read_sav("CCES16_UCM.sav")

# demographics
CCES16_UCM <- 
  CCES16_UCM %>%
  mutate(
    
    # respondent id
    caseid = V101,
    
    # demographics
    age = 2016 - birthyr,
    male = ifelse(gender %in% 1:2, 2 - gender, NA),
    race = suppressWarnings(relevel(factor(car::recode(as.numeric(race), "1='White'; 2='Black'; 3='Hispanic'; 4:8='Other'; 98:99=NA")), "White")),
    education = ifelse(educ %in% 1:6, (educ - 1) / 5, NA),
    income = ifelse(faminc %in% 1:16, (faminc - 1) / 15, NA),
    own_home = ifelse(ownhome %in% 1:3, ifelse(ownhome == 1, 1, 0), NA),
    unemployed = ifelse(employ %in% 3:4, 1, 0),
    uninsured = ifelse(healthins_6 == 1, 1, 0),
    lib_con = ifelse(CC16_340a == 8, 0.5, ifelse(CC16_340a %in% 1:7, (7 - CC16_340a) / 6, NA)),
    
    # personality
    extraverted = ifelse(UCM308 %in% 1:7, (UCM308 - 1) / 6, NA),
    critical_r = ifelse(UCM309 %in% 1:7, (7 - UCM309) / 6, NA),
    dependable = ifelse(UCM310 %in% 1:7, (UCM310 - 1) / 6, NA),
    anxiety = ifelse(UCM311 %in% 1:7, (UCM311 - 1) / 6, NA),
    open = ifelse(UCM312 %in% 1:7, (UCM312 - 1) / 6, NA),
    reserved_r = ifelse(UCM313 %in% 1:7, (7 - UCM313) / 6, NA),
    sympathetic = ifelse(UCM314 %in% 1:7, (UCM314 - 1) / 6, NA),
    disorganized_r = ifelse(UCM315 %in% 1:7, (7 - UCM315) / 6, NA),
    volatility = ifelse(UCM316 %in% 1:7, (7 - UCM316) / 6, NA),
    conventional_r = ifelse(UCM317 %in% 1:7, (7 - UCM317) / 6, NA),
    openness = (open + conventional_r) / 2,
    conscientiousness = (dependable + disorganized_r) / 2,
    extraversion = (extraverted + reserved_r) / 2,
    agreeableness = (critical_r + sympathetic) / 2,
    neuroticism = (anxiety + volatility) / 2,
    
    # social exclusion
    facebook_friends = NA,
    fb_friends_binary = NA,
    
    # political engagement
    political_engagement = ifelse(newsint %in% 1:4, (4 - newsint) / 3, NA),
    
    # survey info
    team = "UCM",
    year = "2016",
    
    .keep = "none"
  )


### 2016 University of Notre Dame CCES Module B ###
CCES16_UNDb <- read_sav("CCES16_UND_B.sav")

# demographics
CCES16_UNDb <- 
  CCES16_UNDb %>%
  mutate(
    
    # respondent id
    caseid = V101,
    
    # demographics
    age = 2016 - birthyr,
    male = ifelse(gender %in% 1:2, 2 - gender, NA),
    race = suppressWarnings(relevel(factor(car::recode(as.numeric(race), "1='White'; 2='Black'; 3='Hispanic'; 4:8='Other'; 98:99=NA")), "White")),
    education = ifelse(educ %in% 1:6, (educ - 1) / 5, NA),
    income = ifelse(faminc %in% 1:16, (faminc - 1) / 15, NA),
    own_home = ifelse(ownhome %in% 1:3, ifelse(ownhome == 1, 1, 0), NA),
    unemployed = ifelse(employ %in% 3:4, 1, 0),
    uninsured = ifelse(healthins_6 == 1, 1, 0),
    lib_con = ifelse(CC16_340a == 8, 0.5, ifelse(CC16_340a %in% 1:7, (7 - CC16_340a) / 6, NA)),
    
    # personality
    extraverted = ifelse(UND349 %in% 1:7, (UND349 - 1) / 6, NA),
    critical_r = ifelse(UND350 %in% 1:7, (7 - UND350) / 6, NA),
    dependable = ifelse(UND351 %in% 1:7, (UND351 - 1) / 6, NA),
    anxiety = ifelse(UND352 %in% 1:7, (UND352 - 1) / 6, NA),
    open = ifelse(UND353 %in% 1:7, (UND353 - 1) / 6, NA),
    reserved_r = ifelse(UND354 %in% 1:7, (7 - UND354) / 6, NA),
    sympathetic = ifelse(UND355 %in% 1:7, (UND355 - 1) / 6, NA),
    disorganized_r = ifelse(UND356 %in% 1:7, (7 - UND356) / 6, NA),
    volatility = ifelse(UND357 %in% 1:7, (7 - UND357) / 6, NA),
    conventional_r = ifelse(UND358 %in% 1:7, (7 - UND358) / 6, NA),
    openness = (open + conventional_r) / 2,
    conscientiousness = (dependable + disorganized_r) / 2,
    extraversion = (extraverted + reserved_r) / 2,
    agreeableness = (critical_r + sympathetic) / 2,
    neuroticism = (anxiety + volatility) / 2,
    
    # social exclusion
    facebook_friends = NA,
    fb_friends_binary = NA,
    
    # political engagement
    political_engagement = ifelse(newsint %in% 1:4, (4 - newsint) / 3, NA),
    
    # survey info
    team = "UND",
    year = "2016",
    
    .keep = "none"
  )


### 2016 New York University CCES ###
CCES16_NYU <- read_sav("CCES16_NYU.sav")

# demographics
CCES16_NYU <- 
  CCES16_NYU %>%
  mutate(
    
    # respondent id
    caseid = V101,
    
    # demographics
    age = 2016 - birthyr,
    male = ifelse(gender %in% 1:2, 2 - gender, NA),
    race = suppressWarnings(relevel(factor(car::recode(as.numeric(race), "1='White'; 2='Black'; 3='Hispanic'; 4:8='Other'; 98:99=NA")), "White")),
    education = ifelse(educ %in% 1:6, (educ - 1) / 5, NA),
    income = ifelse(faminc %in% 1:16, (faminc - 1) / 15, NA),
    own_home = ifelse(ownhome %in% 1:3, ifelse(ownhome == 1, 1, 0), NA),
    unemployed = ifelse(employ %in% 3:4, 1, 0),
    uninsured = ifelse(healthins_6 == 1, 1, 0),
    lib_con = ifelse(CC16_340a %in% 1:7, (7 - CC16_340a) / 6, ifelse(CC16_340a == 8, 0.5, NA)),
    
    # personality
    extraverted = ifelse(NYU353 %in% 1:7, (NYU353 - 1) / 6, NA),
    critical_r = ifelse(NYU354 %in% 1:7, (7 - NYU354) / 6, NA),
    dependable = ifelse(NYU355 %in% 1:7, (NYU355 - 1) / 6, NA),
    anxiety = ifelse(NYU356 %in% 1:7, (NYU356 - 1) / 6, NA),
    open = ifelse(NYU357 %in% 1:7, (NYU357 - 1) / 6, NA),
    reserved_r = ifelse(NYU358 %in% 1:7, (7 - NYU358) / 6, NA),
    sympathetic = ifelse(NYU359 %in% 1:7, (NYU359 - 1) / 6, NA),
    disorganized_r = ifelse(NYU360 %in% 1:7, (7 - NYU360) / 6, NA),
    volatility = ifelse(NYU361 %in% 1:7, (7 - NYU361) / 6, NA),
    conventional_r = ifelse(NYU362 %in% 1:7, (7 - NYU362) / 6, NA),
    openness = (open + conventional_r) / 2,
    conscientiousness = (dependable + disorganized_r) / 2,
    extraversion = (extraverted + reserved_r) / 2,
    agreeableness = (critical_r + sympathetic) / 2,
    neuroticism = (anxiety + volatility) / 2,
    
    # social exclusion
    facebook_friends = suppressWarnings(relevel(factor(car::recode(as.numeric(NYU395), "1='1-100'; 2='101-250'; 3='251-500'; 4='> 500'; 5=NA"), levels = c("1-100", "101-250", "251-500", "> 500")), "> 500")),
    fb_friends_binary = as.numeric(car::recode(as.numeric(NYU395), "1:3=1; 4=0; 5=NA")),
    
    # political engagement
    political_engagement = ifelse(newsint %in% 1:4, (4 - newsint) / 3, NA),
    
    # survey info
    team = "NYU",
    year = "2016",
    
    .keep = "none"
  )


### 2018 New York University CCES ###
CCES18_NYU <- read_sav("CCES18_NYU.sav")

CCES18_NYU <- 
  CCES18_NYU %>%
  mutate(
    
    # respondent id
    caseid = caseid,
    
    # demographics
    age = 2018 - birthyr,
    male = ifelse(gender %in% 1:2, 2 - gender, NA),
    race = suppressWarnings(relevel(factor(car::recode(as.numeric(race), "1='White'; 2='Black'; 3='Hispanic'; 4:8='Other'; 98:99=NA")), "White")),
    education = ifelse(educ %in% 1:6, (educ - 1) / 5, NA),
    income = ifelse(faminc_new %in% 1:16, (faminc_new - 1) / 15, NA),
    own_home = ifelse(ownhome %in% 1:3, ifelse(ownhome == 1, 1, 0), NA),
    unemployed = ifelse(employ %in% 3:4, 1, 0),
    uninsured = ifelse(healthins_7 == 1, 1, 0),
    lib_con = ifelse(CC18_334A %in% 1:7, (7 - CC18_334A) / 6, ifelse(CC18_334A == 8, 0.5, NA)),
    
    # personality
    extraverted = ifelse(NYU301 %in% 1:7, (NYU301 - 1) / 6, NA),
    critical_r = ifelse(NYU302 %in% 1:7, (7 - NYU302) / 6, NA),
    dependable = ifelse(NYU303 %in% 1:7, (NYU303 - 1) / 6, NA),
    anxiety = ifelse(NYU304 %in% 1:7, (NYU304 - 1) / 6, NA),
    open = ifelse(NYU305 %in% 1:7, (NYU305 - 1) / 6, NA),
    reserved_r = ifelse(NYU306 %in% 1:7, (7 - NYU306) / 6, NA),
    sympathetic = ifelse(NYU307 %in% 1:7, (NYU307 - 1) / 6, NA),
    disorganized_r = ifelse(NYU308 %in% 1:7, (7 - NYU308) / 6, NA),
    volatility = ifelse(NYU309 %in% 1:7, (7 - NYU309) / 6, NA),
    conventional_r = ifelse(NYU310 %in% 1:7, (7 - NYU310) / 6, NA),
    openness = (open + conventional_r) / 2,
    conscientiousness = (dependable + disorganized_r) / 2,
    extraversion = (extraverted + reserved_r) / 2,
    agreeableness = (critical_r + sympathetic) / 2,
    neuroticism = (anxiety + volatility) / 2,
    
    # Social Exclusion
    facebook_friends = suppressWarnings(relevel(factor(car::recode(as.numeric(NYU360), "1='1-100'; 2='101-250'; 3='251-500'; 4='> 500'; 5=NA"), levels = c("1-100", "101-250", "251-500", "> 500")), "> 500")),
    fb_friends_binary = as.numeric(car::recode(as.numeric(NYU360), "1:3=1; 4=0; 5=NA")),
    
    # Political Engagement
    political_engagement = ifelse(newsint %in% 1:4, (4 - newsint) / 3, NA),
    
    # Survey Info
    team = "NYU",
    year = "2018",
    
    .keep = "none"
  )

  

### 2020 New York University CES ###
CES20_NYU <- read_sav("CES20_NYU.sav")

CES20_NYU <- 
  CES20_NYU %>%
  mutate(
    
    # respondent id
    caseid = caseid,
    
    # demographics
    age = 2020 - birthyr,
    male = ifelse(gender %in% 1:2, 2 - gender, NA),
    race = suppressWarnings(relevel(factor(car::recode(as.numeric(race), "1='White'; 2='Black'; 3='Hispanic'; 4:8='Other'; 98:99=NA")), "White")),
    education = ifelse(educ %in% 1:6, (educ - 1) / 5, NA),
    income = ifelse(faminc_new %in% 1:16, (faminc_new - 1) / 15, NA),
    own_home = ifelse(ownhome %in% 1:3, ifelse(ownhome == 1, 1, 0), NA),
    unemployed = ifelse(employ %in% 3:4, 1, 0),
    uninsured = ifelse(healthins_6 == 1, 1, 0),
    lib_con = ifelse(CC20_340a %in% 1:7, (7 - CC20_340a) / 6, ifelse(CC20_340a == 8, 0.5, NA)),
    
    # personality
    extraverted = ifelse(NYU301 %in% 1:7, (NYU301 - 1) / 6, NA),
    critical_r = ifelse(NYU302 %in% 1:7, (7 - NYU302) / 6, NA),
    dependable = ifelse(NYU303 %in% 1:7, (NYU303 - 1) / 6, NA),
    anxiety = ifelse(NYU304 %in% 1:7, (NYU304 - 1) / 6, NA),
    open = ifelse(NYU305 %in% 1:7, (NYU305 - 1) / 6, NA),
    reserved_r = ifelse(NYU306 %in% 1:7, (7 - NYU306) / 6, NA),
    sympathetic = ifelse(NYU307 %in% 1:7, (NYU307 - 1) / 6, NA),
    disorganized_r = ifelse(NYU308 %in% 1:7, (7 - NYU308) / 6, NA),
    volatility = ifelse(NYU309 %in% 1:7, (7 - NYU309) / 6, NA),
    conventional_r = ifelse(NYU310 %in% 1:7, (7 - NYU310) / 6, NA),
    openness = (open + conventional_r) / 2,
    conscientiousness = (dependable + disorganized_r) / 2,
    extraversion = (extraverted + reserved_r) / 2,
    agreeableness = (critical_r + sympathetic) / 2,
    neuroticism = (anxiety + volatility) / 2,
    
    # Social Exclusion
    facebook_friends = suppressWarnings(relevel(factor(car::recode(as.numeric(NYU349), "1='1-100'; 2='101-250'; 3='251-500'; 4='> 500'; 5=NA"), levels = c("1-100", "101-250", "251-500", "> 500")), "> 500")),
    fb_friends_binary = as.numeric(car::recode(as.numeric(NYU349), "1:3=1; 4=0; 5=NA")),
    
    # Political Engagement
    political_engagement = ifelse(newsint %in% 1:4, (4 - newsint) / 3, NA),
    
    # Survey Info
    team = "NYU",
    year = "2020",
    
    .keep = "none"
  )
  

### 2020 National Cheng Kung University CES ###
CES20_NCK <- read_sav("CES20_NCK.sav")

CES20_NCK <- 
  CES20_NCK %>%
  mutate(
    
    # respondent id
    caseid = caseid,
    
    # demographics
    age = 2020 - birthyr,
    male = ifelse(gender %in% 1:2, 2 - gender, NA),
    race = suppressWarnings(relevel(factor(car::recode(as.numeric(race), "1='White'; 2='Black'; 3='Hispanic'; 4:8='Other'; 98:99=NA")), "White")),
    education = ifelse(educ %in% 1:6, (educ - 1) / 5, NA),
    income = ifelse(faminc_new %in% 1:16, (faminc_new - 1) / 15, NA),
    own_home = ifelse(ownhome %in% 1:3, ifelse(ownhome == 1, 1, 0), NA),
    unemployed = ifelse(employ %in% 3:4, 1, 0),
    uninsured = ifelse(healthins_6 == 1, 1, 0),
    lib_con = ifelse(CC20_340a %in% 1:7, (7 - CC20_340a) / 6, ifelse(CC20_340a == 8, 0.5, NA)),
    
    # personality
    extraverted = ifelse(NCK301 %in% 1:7, (NCK301 - 1) / 6, NA),
    critical_r = ifelse(NCK302 %in% 1:7, (7 - NCK302) / 6, NA),
    dependable = ifelse(NCK303 %in% 1:7, (NCK303 - 1) / 6, NA),
    anxiety = ifelse(NCK304 %in% 1:7, (NCK304 - 1) / 6, NA),
    open = ifelse(NCK305 %in% 1:7, (NCK305 - 1) / 6, NA),
    reserved_r = ifelse(NCK306 %in% 1:7, (7 - NCK306) / 6, NA),
    sympathetic = ifelse(NCK307 %in% 1:7, (NCK307 - 1) / 6, NA),
    disorganized_r = ifelse(NCK308 %in% 1:7, (7 - NCK308) / 6, NA),
    volatility = ifelse(NCK309 %in% 1:7, (7 - NCK309) / 6, NA),
    conventional_r = ifelse(NCK310 %in% 1:7, (7 - NCK310) / 6, NA),
    openness = (open + conventional_r) / 2,
    conscientiousness = (dependable + disorganized_r) / 2,
    extraversion = (extraverted + reserved_r) / 2,
    agreeableness = (critical_r + sympathetic) / 2,
    neuroticism = (anxiety + volatility) / 2,
    
    # Social Exclusion
    facebook_friends = NA,
    fb_friends_binary = NA,
    
    # Political Engagement
    political_engagement = ifelse(newsint %in% 1:4, (4 - newsint) / 3, NA),
    
    # Survey Info
    team = "NCK",
    year = "2020",
    
    .keep = "none"
  )


#### Create Combined CES

# Merge CCES waves
CES = rbind(rbind(rbind(rbind(rbind(CCES16_NYU, CCES18_NYU), CES20_NYU), CCES16_UCM), CCES16_UNDb), CES20_NCK)

CES = merge(CES, CES_Policy, by="caseid")

# Year Indicator
CES$year = relevel(factor(CES$year), "2016")

# Reverse Coded Variables
CES$`income (reversed)` = 1 - CES$income
CES$`do not own home` = 1 - CES$own_home






##################################################
############### LISS data cleaning ###############
##################################################

# Load Data
LISS = read.csv("LISS 2008.csv")

LISS <- 
  LISS %>%
  mutate(
    
    # demographics
    age = (leeftijd - 16) / 94,
    male = 2 - geslacht,
    race = suppressWarnings(relevel(factor(car::recode(ifelse(cr08a057 == 1 & cr08a060 == 1, 0, 1), "0='White'; 1='Other'")), "White")),
    education = ifelse(oplcat %in% 1:6, (oplcat - 1) / 5, NA),
    income = suppressWarnings(ifelse(ci08a228 < 9999999998, (as.numeric(factor(car::recode(as.numeric(ci08a228), "0:8000='less than 8,000 euros'; 8001:16000='8,000-16,000 euros'; 16001:24000='16,000-24,000 euros'; 24001:36000='24,000-36,000 euros'; 36001:48000='36,000-48,000 euros'; 48001:60000='48,000-60,000 euros'; 60001:3156000='60,000 euros or more'; 9999999998:9999999999=NA"))) - 1) / 6,
                                     ifelse(ci08a229 < 99, (ci08a229 - 1) / 6, NA))),
    `income (reversed)` = 1 - income,
    own_home = ifelse(woning %in% 1:4, ifelse(woning == 1, 1, 0), NA),
    `do not own home` = 1 - own_home,
    unemployed = ifelse(belbezig %in% 1:14, ifelse(belbezig %in% c(4:6, 11), 1, 0), NA),
    left_right = ifelse(cv08a101 %in% 0:10, (10 - cv08a101) / 10, NA),
    
    # personality
    rich_vocab = (cp08a024 - 1) / 4,
    dif_understand_r = (5 - cp08a029) / 4,
    vivid_imagin = (cp08a034 - 1) / 4,
    not_interested_r = (5 - cp08a039) / 4,
    excellent_ideas = (cp08a044 - 1) / 4,
    poor_imagin_r = (5 - cp08a049) / 4,
    quick_understand = (cp08a054 - 1) / 4,
    difficult_words = (cp08a059 - 1) / 4,
    reflecting = (cp08a064 - 1) / 4,
    full_of_ideas = (cp08a069 - 1) / 4,
    always_prepared = (cp08a022 - 1) / 4,
    leave_things_r = (5 - cp08a027) / 4,
    pay_attention = (cp08a032 - 1) / 4,
    make_mess_r = (5 - cp08a037) / 4,
    chores_done = (cp08a042 - 1) / 4,
    forget_to_put_r = (5 - cp08a047) / 4,
    like_order = (cp08a052 - 1) / 4,
    shirk_duties_r = (5 - cp08a057) / 4,
    follow_schedule = (cp08a062 - 1) / 4,
    exacting = (cp08a067 - 1) / 4,
    life_of_party = (cp08a020 - 1) / 4,
    dont_talk_r = (5 - cp08a025) / 4,
    comfortable = (cp08a030 - 1) / 4,
    background_r = (5 - cp08a035) / 4,
    conversations = (cp08a040 - 1) / 4,
    say_little_r = (5 - cp08a045) / 4,
    talk_parties = (cp08a050 - 1) / 4,
    dislike_attent_r = (5 - cp08a055) / 4,
    like_attention = (cp08a060 - 1) / 4,
    quiet_r = (5 - cp08a065) / 4,
    concern_ppl = (cp08a021 - 1) / 4,
    interest_ppl = (cp08a026 - 1) / 4,
    insult_ppl_r = (5 - cp08a031) / 4,
    sympathize = (cp08a036 - 1) / 4,
    ppls_prblms_r = (5 - cp08a041) / 4,
    soft_heart = (cp08a046 - 1) / 4,
    intrst_othrs_r = (5 - cp08a051) / 4,
    time_for_ppl = (cp08a056 - 1) / 4,
    feel_emotions = (cp08a061 - 1) / 4,
    ppl_at_ease = (cp08a066 - 1) / 4,
    stress_easily = (cp08a023 - 1) / 4,
    upset_easily = (cp08a048 - 1) / 4,
    relaxed_r = (5 - cp08a028) / 4,
    worry = (cp08a033 - 1) / 4,
    easily_disturbed = (cp08a043 - 1) / 4,
    seldom_blue_r = (5 - cp08a038) / 4,
    often_blue = (cp08a068 - 1) / 4,
    change_mood = (cp08a053 - 1) / 4,
    mood_swings = (cp08a058 - 1) / 4,
    irritated_easily = (cp08a063 - 1) / 4,
    
    # Neuroticism Facets
    anxiety = (stress_easily + relaxed_r + worry + easily_disturbed) / 4,
    volatility = (change_mood + mood_swings + irritated_easily + upset_easily) / 4,
    
    # Big Five Traits
    openness = (rich_vocab + dif_understand_r + vivid_imagin + not_interested_r + excellent_ideas + poor_imagin_r + quick_understand + difficult_words + reflecting + full_of_ideas) / 10,
    conscientiousness = (always_prepared + leave_things_r + pay_attention + make_mess_r + chores_done + forget_to_put_r + like_order + shirk_duties_r + follow_schedule + exacting) / 10,
    extraversion = (life_of_party + dont_talk_r + comfortable + background_r + conversations + say_little_r + talk_parties + dislike_attent_r + like_attention + quiet_r) / 10,
    agreeableness = (concern_ppl + interest_ppl + insult_ppl_r + sympathize + ppls_prblms_r + soft_heart + intrst_othrs_r + time_for_ppl + feel_emotions + ppl_at_ease) / 10,
    neuroticism = (stress_easily + relaxed_r + worry + easily_disturbed + upset_easily + change_mood + mood_swings + irritated_easily + seldom_blue_r + often_blue) / 10,
    
    # Economic Policy
    econ_policy = ifelse(cv08a103 %in% 1:5, (cv08a103 - 1) / 4, NA),
    
    # Social Policy
    euthanasia = ifelse(cv08a102 %in% 1:5, (cv08a102 - 1) / 4, NA),
    assimilation = ifelse(cv08a104 %in% 1:5, (5 - cv08a104) / 4, NA),
    eu_unification = ifelse(cv08a105 %in% 1:5, (5 - cv08a105) / 4, NA),
    social_policy = (euthanasia + assimilation + eu_unification) / 3,
    
    # Political Engagement
    news_interest = ifelse(cv08a008 %in% 1:3, (3 - cv08a008) / 2, NA),
    nat_news_read = ifelse(cv08a009 %in% 1:4, (cv08a009 - 1) / 3, NA),
    nat_news_talk = ifelse(cv08a010 %in% 1:3, (3 - cv08a010) / 2, NA),
    int_news_read = ifelse(cv08a011 %in% 1:3, (cv08a011 - 1) / 3, NA),
    pol_interest = ifelse(cv08a012 %in% 1:3, (3 - cv08a012) / 2, NA),
    political_engagement = (news_interest + nat_news_read + nat_news_talk + int_news_read + pol_interest) / 5,
    
    # Social Inclusion
    count_on = (cs08a285 - 1) / 2,
    rely_on = (cs08a286 - 1) / 2,
    social_exclusion = (count_on + rely_on) / 2,
    
    .keep = "none"
  )







##################################################
############### TAPS data cleaning ###############
##################################################

### TAPS Profile Data ###
TAPS_Profiles <- read_dta("TAPS_Profiles_12_16.dta", 
                          col_select = c("wustlid", "hhzip", "AGE_2016", "ppgender", "ppethm", "educsp"))

TAPS_Profiles <- 
  TAPS_Profiles %>%
  mutate(
    
    # respondent id
    caseid = wustlid,
    
    # Demographics
    zip = hhzip,
    age = (AGE_2016 - min(na.omit(AGE_2016))) / max(na.omit(AGE_2016 - min(na.omit(AGE_2016)))),
    male = 2 - ppgender,
    race = suppressWarnings(relevel(factor(car::recode(as.numeric(ppethm), "1='White'; 2='Black'; 4='Hispanic'; 3='Other'; 5='Other'")), "White")),
    education = ifelse(educsp %in% 1:15, (educsp - 1) / 14, NA),
    
    .keep = "none"
  )

### TAPS ###
TAPS <- read_dta("TAPS_2013.dta")

TAPS <- 
  TAPS %>%
  mutate(
    
    # respondent id
    caseid = WUSTLID,
    
    # Demographics
    income = ifelse(INCOMES25 %in% 2:17, (INCOMES25 - 2) / 15, NA),
    `income (reversed)` = 1 - income,
    own_home = ifelse(OWNS25 %in% 2:5, ifelse(OWNS25 == 2, 1, 0), NA),
    `do not own home` = 1 - own_home,
    unemployed = ifelse(JOBS25 %in% 2:3, ifelse(JOBS25 == 3, 1, 0), NA),
    uninsured = ifelse(HINS1S23 %in% 2:3, ifelse(HINS1S23 == 2, 0, 1), NA),
    lib_con = ifelse(IDEOL1S24 %in% 2:8, (8 - IDEOL1S24) / 6, NA),
    
    # Wave 18 TIPI
    extraverted.18 = ifelse(PEREXTRS18 %in% 2:8, (PEREXTRS18 - 2) / 6, NA),
    critical_r.18 = ifelse(PERCRIS18 %in% 2:8, (8 - PERCRIS18) / 6, NA),
    dependable.18 = ifelse(PERDEPS18 %in% 2:8, (PERDEPS18 - 2) / 6, NA),
    anxious.18 = ifelse(PERANXS18 %in% 2:8, (PERANXS18 - 2) / 6, NA),
    open.18 = ifelse(PEROPES18 %in% 2:8, (PEROPES18 - 2) / 6, NA),
    reserved_r.18 = ifelse(PERRESS18 %in% 2:8, (8 - PERRESS18) / 6, NA),
    sympathetic.18 = ifelse(PERSYMS18 %in% 2:8, (PERSYMS18 - 2) / 6, NA),
    disorganized_r.18 = ifelse(PERDISS18 %in% 2:8, (8 - PERDISS18) / 6, NA),
    calm_r.18 = ifelse(PERCALS18 %in% 2:8, (8 - PERCALS18) / 6, NA),
    conventional_r.18 = ifelse(PERCONS18 %in% 2:8, (8 - PERCONS18) / 6, NA),
    
    # Wave 22 TIPI
    extraverted.22 = ifelse(PEREXTRS22 %in% 2:8, (PEREXTRS22 - 2) / 6, NA),
    critical_r.22 = ifelse(PERCRIS22 %in% 2:8, (8 - PERCRIS22) / 6, NA),
    dependable.22 = ifelse(PERDEPS22 %in% 2:8, (PERDEPS22 - 2) / 6, NA),
    anxious.22 = ifelse(PERANXS22 %in% 2:8, (PERANXS22 - 2) / 6, NA),
    open.22 = ifelse(PEROPES22 %in% 2:8, (PEROPES22 - 2) / 6, NA),
    reserved_r.22 = ifelse(PERRESS22 %in% 2:8, (8 - PERRESS22) / 6, NA),
    sympathetic.22 = ifelse(PERSYMS22 %in% 2:8, (PERSYMS22 - 2) / 6, NA),
    disorganized_r.22 = ifelse(PERDISS22 %in% 2:8, (8 - PERDISS22) / 6, NA),
    calm_r.22 = ifelse(PERCALS22 %in% 2:8, (8 - PERCALS22) / 6, NA),
    conventional_r.22 = ifelse(PERCONS22 %in% 2:8, (8 - PERCONS22) / 6, NA),
    
    # Averaged TIPI Items
    extraverted = (extraverted.18 + extraverted.22) / 2,
    critical_r = (critical_r.18 + critical_r.22) / 2,
    dependable = (dependable.18 + dependable.22) / 2,
    anxiety = (anxious.18 + anxious.22) / 2,
    open = (open.18 + open.22) / 2,
    reserved_r = (reserved_r.18 + reserved_r.22) / 2,
    sympathetic = (sympathetic.18 + sympathetic.22) / 2,
    disorganized_r = (disorganized_r.18 + disorganized_r.22) / 2,
    volatility = (calm_r.18 + calm_r.22) / 2,
    conventional_r = (conventional_r.18 + conventional_r.22) / 2,
    
    # Big Five Traits
    openness = (open + conventional_r) / 2,
    conscientiousness = (dependable + disorganized_r) / 2,
    extraversion = (extraverted + reserved_r) / 2,
    agreeableness = (critical_r + sympathetic) / 2,
    neuroticism = (anxiety + volatility) / 2,
    
    # Economic Policy
    tax_rich = ifelse(ITAXESS18 %in% 2:6, (6 - ITAXESS18) / 4, NA),
    minimum_wage = ifelse(IMINWGS18 %in% 2:6, (6 - IMINWGS18) / 4, NA),
    medicaid_cover = ifelse(IMDCDS18 %in% 2:6, (6 - IMDCDS18) / 4, NA),
    medi_spending = ifelse(SPU1S18 %in% 2:4, (4 - SPU1S18) / 2, NA),
    soc_sec_spend = ifelse(SPU15S18 %in% 2:4, (4 - SPU15S18) / 2, NA),
    gov_housing = ifelse(SPU17S18 %in% 2:4, (4 - SPU17S18) / 2, NA),
    gov_spending = ifelse(SFFK1S20 %in% 2:6, (SFFK1S20 - 2) / 4, NA),
    gov_jobs = ifelse(SFFK11S20 %in% 2:5, (5 - SFFK11S20) / 3, NA),
    gov_healthcare = ifelse(SFFK13S20 %in% 2:5, (5 - SFFK13S20) / 3, NA),
    gov_elderly = ifelse(SFFK14S20 %in% 2:5, (5 - SFFK14S20) / 3, NA),
    gov_unemployed = ifelse(SFFK16S20 %in% 2:5, (5 - SFFK16S20) / 3, NA),
    income_equality = ifelse(SFFK17S20 %in% 2:5, (5 - SFFK17S20) / 3, NA),
    econ_policy = (tax_rich + minimum_wage + medicaid_cover + medi_spending + soc_sec_spend + gov_housing) / 6,
    
    # Social Policy
    abortion = ifelse(IABORTS18 %in% 2:6, (6 - IABORTS18) / 4, NA),
    gay_marriage = ifelse(IGAYMS18 %in% 2:6, (6 - IGAYMS18) / 4, NA),
    immigrants = ifelse(IIMMGS18 %in% 2:6, (6 - IIMMGS18) / 4, NA),
    gun_control = ifelse(IGUNS18 %in% 2:6, (6 - IGUNS18) / 4, NA),
    aff_action = ifelse(IAFFACTS18 %in% 2:6, (6 - IAFFACTS18) / 4, NA),
    social_policy = (abortion + gay_marriage + immigrants + gun_control + aff_action) / 5,
    
    # Political Engagement
    political_engagement = ifelse(interestpolsp == 1, 0.5, ifelse(interestpolsp %in% 2:5, (5 - interestpolsp) / 3, NA)),
    
    # Social Exclusion
    social_exclusion = ifelse(STRESS13S21 %in% 2:6, (6 - STRESS13S21) / 4, NA),
    
    .keep = "none"
  )

# Merge
TAPS <- merge(TAPS, TAPS_Profiles, by = "caseid", all = T)






##################################################
########## CloudResearch data cleaning ###########
##################################################

### CloudResearch Study 1

# Load Data
CR_Study1 <- read_excel("CR Study June 2022.xlsx")

# Remove respondents flagged as bots
CR_Study1 <- CR_Study1[is.na(CR_Study1$flagged), ]

CR_Study1 <- 
  CR_Study1 %>%
  mutate(
    
    # Demographics
    age = age,
    male = ifelse(gender == "Male", 1, 0),
    race = suppressWarnings(relevel(factor(car::recode(race, "'White'='White'; 'Black or African American'='Black'; 'Hispanic or Latino'='Other'; 'Asian'='Other'; 'American Indian or Alaska Native' = 'Other'; 'Other'='Other'")), "White")),
    education = car::recode(education, "'High School'=0; 'Some College'=1; 'Associates Degree'=2; 'Bachelors Degree'=3; 'Doctorate'=4; 'Professional Degree'=4") / 4,
    income = as.numeric(car::recode(income, "0:24999=0; 25000:49999=1; 50000:74999=2; 75000:99999=3; 100000:149999=4; 150000:5000000=5")) / 5,
    econ_lib = (econ_ideology - 1) / 6,
    social_lib = (social_ideology - 1) / 6,
    lib_con = (econ_lib + social_lib) / 2,
    party_id = (party_id - 1) / 6,
    political_engagement = (pol_engage - 1) / 6,
    
    # Personality
    angry_easily = (angry_easily - 1) / 4,
    upset_easily = (upset_easily - 1) / 4,
    change_mood = (change_mood - 1) / 4,
    mood_up_down = (mood_up_down - 1) / 4,
    easily_agitated = (agitated_easily - 1) / 4,
    stirred_up = (stirred_up - 1) / 4,
    rarely_irritated = (5 - rarely_irritated_R) / 4,
    control_emotions = (5 - control_emotions_R) / 4,
    rarely_lose_composure = (5 - lose_composure_R) / 4,
    not_easily_annoyed = (5 - not_annoyed_R) / 4,
    filled_doubt = (filled_doubt - 1) / 4,
    easily_threatened = (threatened_easily - 1) / 4,
    worry = (worry - 1) / 4,
    easily_discouraged = (easily_discouraged - 1) / 4,
    overwhelmed = (overwhelmed - 1) / 4,
    afraid = (afraid - 1) / 4,
    seldom_sad = (5 - seldom_sad_R) / 4,
    comfortable_self = (5 - comfortable_R) / 4,
    rarely_depressed = (5 - rarely_depressed_R) / 4,
    not_easily_embarrassed = (5 - not_embarrassed_R) / 4,
    neuroticism = (angry_easily + upset_easily + change_mood + mood_up_down + easily_agitated + stirred_up + rarely_irritated + control_emotions + rarely_lose_composure + not_easily_annoyed + filled_doubt + easily_threatened + worry + easily_discouraged + overwhelmed + afraid + seldom_sad + comfortable_self + rarely_depressed + not_easily_embarrassed) / 20,
    anxiety = (easily_threatened + worry + afraid) / 3,
    volatility = (angry_easily + upset_easily + easily_agitated + rarely_irritated + not_easily_annoyed + change_mood + mood_up_down + control_emotions + stirred_up + rarely_lose_composure) / 10,
    withdrawal = (filled_doubt + easily_threatened + worry + easily_discouraged + overwhelmed + afraid + seldom_sad + comfortable_self + rarely_depressed + not_easily_embarrassed) / 10,
    
    # Economic Attitudes
    gov_jobs = (7 - gov_jobs_R) / 6,
    gov_insurance = (gov_insur - 1) / 6,
    income_equality = (7 - gov_equal_R) / 6,
    min_wage = (min_wage - 1) / 6,
    econ_policy = (gov_jobs + gov_insurance + income_equality + min_wage) / 4,
    
    # Social Attitudes
    abortion = (7 - abortion_R) / 6,
    gay_adoption = (7 - gay_adopt_R) / 6,
    weed = (weed - 1) / 6,
    immigration = (immig - 1) / 6,
    social_policy = (abortion + gay_adoption + weed + immigration) / 4,
    
    # Social Inclusion Treatment
    social_exclusion = ifelse(treatment == "Excluded", 1, 0),
    
    # Study Indicator
    study = "1",
    
    # Social Group's Attitudes
    gov_jobs_group = (7 - gov_jobs_other) / 6,
    gov_insurance_group = (gov_insur_other - 1) / 6,
    income_equality_group = (7 - gov_equal_other) / 6,
    min_wage_group = (min_wage_other - 1) / 6,
    abortion_group = (7 - abortion_other) / 6,
    gay_adoption_group = (7 - gay_adopt_other) / 6,
    weed_group = (weed_other - 1) / 6,
    immigration_group = (immig_other - 1) / 6,
    
    # Social Conformity
    gov_jobs_dif = abs(gov_jobs - gov_jobs_group),
    gov_insurance_dif = abs(gov_insurance - gov_insurance_group),
    income_equality_dif = abs(income_equality - income_equality_group),
    min_wage_dif = abs(min_wage - min_wage_group),
    abortion_dif = abs(abortion - abortion_group),
    gay_adoption_dif = abs(gay_adoption - gay_adoption_group),
    weed_dif = abs(weed - weed_group),
    immigration_dif = abs(immigration - immigration_group),
    conformity = (gov_jobs_dif + gov_insurance_dif + income_equality_dif + min_wage_dif + abortion_dif + gay_adoption_dif + weed_dif + immigration_dif) / 8,
    
    .keep = "none"
  )

CR_Study1 <-
  CR_Study1 %>% 
  dplyr::select(age, male, race, education, income, lib_con, party_id, political_engagement, angry_easily, upset_easily, change_mood, mood_up_down, easily_agitated, stirred_up,
                rarely_irritated, control_emotions, rarely_lose_composure, not_easily_annoyed, filled_doubt, easily_threatened, worry, easily_discouraged, overwhelmed, afraid, 
                seldom_sad, comfortable_self, rarely_depressed, not_easily_embarrassed, neuroticism, anxiety, volatility, withdrawal, gov_jobs, gov_insurance, income_equality, 
                min_wage, econ_policy, abortion, gay_adoption, weed, immigration, social_policy, social_exclusion, study)

### CloudResearch Study 2

# Load Data
CR_Study2 <- read_excel("CR Study Jan 2023.xlsx")

# Remove respondents flagged as bots
CR_Study2 <- CR_Study2[is.na(CR_Study2$flagged), ]

CR_Study2 <- 
  CR_Study2 %>%
  mutate(
    
    # Demographics
    age = age,
    male = ifelse(gender == "Male", 1, 0),
    race = suppressWarnings(relevel(factor(car::recode(race, "'White or Caucasian'='White'; 'Black or African American'='Black'; 'Native Hawaiian or Other Pacific Islander'='Other'; 'Asian'='Other'; 'American Indian/Native American or Alaska Native' = 'Other'; 'Other'='Other'")), "White")),
    education = car::recode(education, "'Less than High School'=0; 'High School'=0; 'Started college but didnt finish'=1; 'Associates Degree'=2; 'Bachelors Degree'=3; 'Masters Degree'=4; 'Doctorate or Professional Degree'=4") / 4,
    income = as.numeric(car::recode(income, "'Less than $25,000'=0; '$25,000-$49,999'=1; '$50,000-$74,999'=2; '$75,000-$99,999'=3; '$100,000-$149,999'=4; '$150,000 or more'=5")) / 5,
    econ_lib = NA,
    social_lib = NA,
    lib_con = (ideology - 1) / 6,
    party_id = (party_id - 1) / 6,
    political_engagement = (pol_engage - 1) / 6,
    
    # Personality
    angry_easily = (angry_easily - 1) / 4,
    upset_easily = (upset_easily - 1) / 4,
    change_mood = (change_mood - 1) / 4,
    mood_up_down = (mood_up_down - 1) / 4,
    easily_agitated = (agitated_easily - 1) / 4,
    stirred_up = (stirred_up - 1) / 4,
    rarely_irritated = (5 - rarely_irritated_R) / 4,
    control_emotions = (5 - control_emotions_R) / 4,
    rarely_lose_composure = (5 - lose_composure_R) / 4,
    not_easily_annoyed = (5 - not_annoyed_R) / 4,
    filled_doubt = (filled_doubt - 1) / 4,
    easily_threatened = (threatened_easily - 1) / 4,
    worry = (worry - 1) / 4,
    easily_discouraged = (easily_discouraged - 1) / 4,
    overwhelmed = (overwhelmed - 1) / 4,
    afraid = (afraid - 1) / 4,
    seldom_sad = (5 - seldom_sad_R) / 4,
    comfortable_self = (5 - comfortable_R) / 4,
    rarely_depressed = (5 - rarely_depressed_R) / 4,
    not_easily_embarrassed = (5 - not_embarrassed_R) / 4,
    neuroticism = (angry_easily + upset_easily + change_mood + mood_up_down + easily_agitated + stirred_up + rarely_irritated + control_emotions + rarely_lose_composure + not_easily_annoyed + filled_doubt + easily_threatened + worry + easily_discouraged + overwhelmed + afraid + seldom_sad + comfortable_self + rarely_depressed + not_easily_embarrassed) / 20,
    anxiety = (easily_threatened + worry + afraid) / 3,
    volatility = (angry_easily + upset_easily + easily_agitated + rarely_irritated + not_easily_annoyed + change_mood + mood_up_down + control_emotions + stirred_up + rarely_lose_composure) / 10,
    withdrawal = (filled_doubt + easily_threatened + worry + easily_discouraged + overwhelmed + afraid + seldom_sad + comfortable_self + rarely_depressed + not_easily_embarrassed) / 10,
    
    # Economic Attitudes
    gov_jobs = (7 - gov_jobs_R) / 6,
    gov_insurance = (gov_insur - 1) / 6,
    income_equality = (7 - gov_equal_R) / 6,
    min_wage = (min_wage - 1) / 6,
    econ_policy = (gov_jobs + gov_insurance + income_equality + min_wage) / 4,
    
    # Social Attitudes
    abortion = (7 - abortion_R) / 6,
    gay_adoption = (7 - gay_adopt_R) / 6,
    weed = (weed - 1) / 6,
    immigration = (immig - 1) / 6,
    social_policy = (abortion + gay_adoption + weed + immigration) / 4,
    
    # Social Inclusion Treatment
    social_exclusion = ifelse(treatment == "Excluded", 1, 0),
    
    # Study Indicator
    study = "2",
    
    .keep = "none"
  )

CR_Study2 <-
  CR_Study2 %>% 
  dplyr::select(age, male, race, education, income, lib_con, party_id, political_engagement, angry_easily, upset_easily, change_mood, mood_up_down, easily_agitated, stirred_up,
                rarely_irritated, control_emotions, rarely_lose_composure, not_easily_annoyed, filled_doubt, easily_threatened, worry, easily_discouraged, overwhelmed, afraid, 
                seldom_sad, comfortable_self, rarely_depressed, not_easily_embarrassed, neuroticism, anxiety, volatility, withdrawal, gov_jobs, gov_insurance, income_equality, 
                min_wage, econ_policy, abortion, gay_adoption, weed, immigration, social_policy, social_exclusion, study)

#Pool Studies
CR_Pooled = rbind(CR_Study1, CR_Study2)
CR_Pooled$study = relevel(factor(CR_Pooled$study), "1")
CR_Pooled$`income (reversed)` = 1 - CR_Pooled$income











##################################################
#################### Figure 1 ####################
##################################################

# Models
ANES_Mod_1a <- lmer(income_equality ~ age + male + race + education + income + own_home + unemployed + uninsured + openness + conscientiousness + extraversion + agreeableness + anxiety + volatility + (1 | year), data = ANES)
ANES_Mod_1b <- lmer(guar_job_living ~ age + male + race + education + income + own_home + unemployed + uninsured + openness + conscientiousness + extraversion + agreeableness + anxiety + volatility + (1 | year), data = ANES)
ANES_Mod_1c <- lmer(soc_sec_spend ~ age + male + race + education + income + own_home + unemployed + uninsured + openness + conscientiousness + extraversion + agreeableness + anxiety + volatility + (1 | year), data = ANES)
ANES_Mod_1d <- lmer(welfare_spend ~ age + male + race + education + income + own_home + unemployed + uninsured + openness + conscientiousness + extraversion + agreeableness + anxiety + volatility + (1 | year), data = ANES)
ANES_Mod_1e <- lmer(poor_spend ~ age + male + race + education + income + own_home + unemployed + uninsured + openness + conscientiousness + extraversion + agreeableness + anxiety + volatility + (1 | year), data = ANES)
ANES_Mod_1f <- lmer(gov_services ~ age + male + race + education + income + own_home + unemployed + uninsured + openness + conscientiousness + extraversion + agreeableness + anxiety + volatility + (1 | year), data = ANES)
ANES_Mod_1g <- lmer(public_insurance ~ age + male + race + education + income + own_home + unemployed + uninsured + openness + conscientiousness + extraversion + agreeableness + anxiety + volatility + (1 | year), data = ANES)
ANES_Mod_1h <- lmer(wealthy_tax ~ age + male + race + education + income + own_home + unemployed + uninsured + openness + conscientiousness + extraversion + agreeableness + anxiety + volatility + (1 | year), data = ANES)
ANES_Mod_1i <- lmer(gay_adoption ~ age + male + race + education + income + own_home + unemployed + uninsured + openness + conscientiousness + extraversion + agreeableness + anxiety + volatility + (1 | year), data = ANES)
ANES_Mod_1j <- lmer(abortion ~ age + male + race + education + income + own_home + unemployed + uninsured + openness + conscientiousness + extraversion + agreeableness + anxiety + volatility + (1 | year), data = ANES)
ANES_Mod_1k <- lmer(marijuana ~ age + male + race + education + income + own_home + unemployed + uninsured + openness + conscientiousness + extraversion + agreeableness + anxiety + volatility + (1 | year), data = ANES)
ANES_Mod_1l <- lmer(death_penalty ~ age + male + race + education + income + own_home + unemployed + uninsured + openness + conscientiousness + extraversion + agreeableness + anxiety + volatility + (1 | year), data = ANES)
ANES_Mod_1m <- lmer(gun_control ~ age + male + race + education + income + own_home + unemployed + uninsured + openness + conscientiousness + extraversion + agreeableness + anxiety + volatility + (1 | year), data = ANES)
ANES_Mod_1n <- lmer(immigration ~ age + male + race + education + income + own_home + unemployed + uninsured + openness + conscientiousness + extraversion + agreeableness + anxiety + volatility + (1 | year), data = ANES)
ANES_Mod_1o <- lmer(aff_action ~ age + male + race + education + income + own_home + unemployed + uninsured + openness + conscientiousness + extraversion + agreeableness + anxiety + volatility + (1 | year), data = ANES)
ANES_Mod_1p <- lmer(gay_marriage ~ age + male + race + education + income + own_home + unemployed + uninsured + openness + conscientiousness + extraversion + agreeableness + anxiety + volatility + (1 | year), data = ANES)
ANES_Mod_1q <- lmer(lib_con ~ age + male + race + education + income + own_home + unemployed + uninsured + openness + conscientiousness + extraversion + agreeableness + anxiety + volatility + (1 | year), data = ANES)

CES_Mod_1a <- lmer(aca ~ age + male + race + education + income + own_home + unemployed + uninsured + openness + conscientiousness + extraversion + agreeableness + anxiety + volatility + (1 | year), data = CES)
CES_Mod_1b <- lmer(welfare ~ age + male + race + education + income + own_home + unemployed + uninsured + openness + conscientiousness + extraversion + agreeableness + anxiety + volatility + (1 | year), data = CES)
CES_Mod_1c <- lmer(healthcare ~ age + male + race + education + income + own_home + unemployed + uninsured + openness + conscientiousness + extraversion + agreeableness + anxiety + volatility + (1 | year), data = CES)
CES_Mod_1d <- lmer(abortion ~ age + male + race + education + income + own_home + unemployed + uninsured + openness + conscientiousness + extraversion + agreeableness + anxiety + volatility + (1 | year), data = CES)
CES_Mod_1e <- lmer(gun_control ~ age + male + race + education + income + own_home + unemployed + uninsured + openness + conscientiousness + extraversion + agreeableness + anxiety + volatility + (1 | year), data = CES)
CES_Mod_1f <- lm(gay_marriage ~ age + male + race + education + income + own_home + unemployed + uninsured + openness + conscientiousness + extraversion + agreeableness + anxiety + volatility, data = CES)
CES_Mod_1g <- lmer(immigration ~ age + male + race + education + income + own_home + unemployed + uninsured + openness + conscientiousness + extraversion + agreeableness + anxiety + volatility + (1 | year), data = CES)
CES_Mod_1h <- lmer(lib_con ~ age + male + race + education + income + own_home + unemployed + uninsured + openness + conscientiousness + extraversion + agreeableness + anxiety + volatility + (1 | year), data = CES)

LISS_Mod_1a <- lm(econ_policy ~ age + male + race + education + income + own_home + unemployed + openness + conscientiousness + extraversion + agreeableness + anxiety + volatility, data = LISS)
LISS_Mod_1b <- lm(euthanasia ~ age + male + race + education + income + own_home + unemployed + openness + conscientiousness + extraversion + agreeableness + anxiety + volatility, data = LISS)
LISS_Mod_1c <- lm(assimilation ~ age + male + race + education + income + own_home + unemployed + openness + conscientiousness + extraversion + agreeableness + anxiety + volatility, data = LISS)
LISS_Mod_1d <- lm(eu_unification ~ age + male + race + education + income + own_home + unemployed + openness + conscientiousness + extraversion + agreeableness + anxiety + volatility, data = LISS)
LISS_Mod_1e <- lm(left_right ~ age + male + race + education + income + own_home + unemployed + openness + conscientiousness + extraversion + agreeableness + anxiety + volatility, data = LISS)

TAPS_Mod_1a <- lm(tax_rich ~ age + male + race + education + income + own_home + unemployed + uninsured + openness + conscientiousness + extraversion + agreeableness + anxiety + volatility, data = TAPS)
TAPS_Mod_1b <- lm(minimum_wage ~ age + male + race + education + income + own_home + unemployed + uninsured + openness + conscientiousness + extraversion + agreeableness + anxiety + volatility, data = TAPS)
TAPS_Mod_1c <- lm(medicaid_cover ~ age + male + race + education + income + own_home + unemployed + uninsured + openness + conscientiousness + extraversion + agreeableness + anxiety + volatility, data = TAPS)
TAPS_Mod_1d <- lm(medi_spending ~ age + male + race + education + income + own_home + unemployed + uninsured + openness + conscientiousness + extraversion + agreeableness + anxiety + volatility, data = TAPS)
TAPS_Mod_1e <- lm(soc_sec_spend ~ age + male + race + education + income + own_home + unemployed + uninsured + openness + conscientiousness + extraversion + agreeableness + anxiety + volatility, data = TAPS)
TAPS_Mod_1f <- lm(gov_housing ~ age + male + race + education + income + own_home + unemployed + uninsured + openness + conscientiousness + extraversion + agreeableness + anxiety + volatility, data = TAPS)
TAPS_Mod_1g <- lm(gov_spending ~ age + male + race + education + income + own_home + unemployed + uninsured + openness + conscientiousness + extraversion + agreeableness + anxiety + volatility, data = TAPS)
TAPS_Mod_1h <- lm(gov_jobs ~ age + male + race + education + income + own_home + unemployed + uninsured + openness + conscientiousness + extraversion + agreeableness + anxiety + volatility, data = TAPS)
TAPS_Mod_1i <- lm(gov_healthcare ~ age + male + race + education + income + own_home + unemployed + uninsured + openness + conscientiousness + extraversion + agreeableness + anxiety + volatility, data = TAPS)
TAPS_Mod_1j <- lm(gov_elderly ~ age + male + race + education + income + own_home + unemployed + uninsured + openness + conscientiousness + extraversion + agreeableness + anxiety + volatility, data = TAPS)
TAPS_Mod_1k <- lm(gov_unemployed ~ age + male + race + education + income + own_home + unemployed + uninsured + openness + conscientiousness + extraversion + agreeableness + anxiety + volatility, data = TAPS)
TAPS_Mod_1l <- lm(income_equality ~ age + male + race + education + income + own_home + unemployed + uninsured + openness + conscientiousness + extraversion + agreeableness + anxiety + volatility, data = TAPS)
TAPS_Mod_1m <- lm(abortion ~ age + male + race + education + income + own_home + unemployed + uninsured + openness + conscientiousness + extraversion + agreeableness + anxiety + volatility, data = TAPS)
TAPS_Mod_1n <- lm(gay_marriage ~ age + male + race + education + income + own_home + unemployed + uninsured + openness + conscientiousness + extraversion + agreeableness + anxiety + volatility, data = TAPS)
TAPS_Mod_1o <- lm(immigrants ~ age + male + race + education + income + own_home + unemployed + uninsured + openness + conscientiousness + extraversion + agreeableness + anxiety + volatility, data = TAPS)
TAPS_Mod_1p <- lm(gun_control ~ age + male + race + education + income + own_home + unemployed + uninsured + openness + conscientiousness + extraversion + agreeableness + anxiety + volatility, data = TAPS)
TAPS_Mod_1q <- lm(aff_action ~ age + male + race + education + income + own_home + unemployed + uninsured + openness + conscientiousness + extraversion + agreeableness + anxiety + volatility, data = TAPS)
TAPS_Mod_1r <- lm(lib_con ~ age + male + race + education + income + own_home + unemployed + uninsured + openness + conscientiousness + extraversion + agreeableness + anxiety + volatility, data = TAPS)

CR_Mod_1a <- lmer(gov_jobs ~ age + male + race + education + income + anxiety + volatility + (1 | study), data = CR_Pooled)
CR_Mod_1b <- lmer(gov_insurance ~ age + male + race + education + income + anxiety + volatility + (1 | study), data = CR_Pooled)
CR_Mod_1c <- lmer(income_equality ~ age + male + race + education + income + anxiety + volatility + (1 | study), data = CR_Pooled)
CR_Mod_1d <- lmer(min_wage ~ age + male + race + education + income + anxiety + volatility + (1 | study), data = CR_Pooled)
CR_Mod_1e <- lmer(abortion ~ age + male + race + education + income + anxiety + volatility + (1 | study), data = CR_Pooled)
CR_Mod_1f <- lmer(gay_adoption ~ age + male + race + education + income + anxiety + volatility + (1 | study), data = CR_Pooled)
CR_Mod_1g <- lmer(weed ~ age + male + race + education + income + anxiety + volatility + (1 | study), data = CR_Pooled)
CR_Mod_1h <- lmer(immigration ~ age + male + race + education + income + anxiety + volatility + (1 | study), data = CR_Pooled)
CR_Mod_1i <- lmer(lib_con ~ age + male + race + education + income + anxiety + volatility + (1 | study), data = CR_Pooled)


mod1_list <- list(
  ANES_Mod_1a = list(model = ANES_Mod_1a, dataset = "ANES",          dv = "Income equality",                  domain = "Economic"),
  ANES_Mod_1b = list(model = ANES_Mod_1b, dataset = "ANES",          dv = "Guaranteed jobs/stndrd of living", domain = "Economic"),
  ANES_Mod_1c = list(model = ANES_Mod_1c, dataset = "ANES",          dv = "Social Security spending",         domain = "Economic"),
  ANES_Mod_1d = list(model = ANES_Mod_1d, dataset = "ANES",          dv = "Welfare spending",                 domain = "Economic"),
  ANES_Mod_1e = list(model = ANES_Mod_1e, dataset = "ANES",          dv = "Spending on poor",                 domain = "Economic"),
  ANES_Mod_1f = list(model = ANES_Mod_1f, dataset = "ANES",          dv = "Government services",              domain = "Economic"),
  ANES_Mod_1g = list(model = ANES_Mod_1g, dataset = "ANES",          dv = "Gov medical insurance",            domain = "Economic"),
  ANES_Mod_1h = list(model = ANES_Mod_1h, dataset = "ANES",          dv = "Raise taxes on rich",              domain = "Economic"),
  ANES_Mod_1i = list(model = ANES_Mod_1i, dataset = "ANES",          dv = "Same-sex adoption",                domain = "Non-economic"),
  ANES_Mod_1j = list(model = ANES_Mod_1j, dataset = "ANES",          dv = "Abortion",                         domain = "Non-economic"),
  ANES_Mod_1k = list(model = ANES_Mod_1k, dataset = "ANES",          dv = "Legalize cannabis",                domain = "Non-economic"),
  ANES_Mod_1l = list(model = ANES_Mod_1l, dataset = "ANES",          dv = "Death penalty",                    domain = "Non-economic"),
  ANES_Mod_1m = list(model = ANES_Mod_1m, dataset = "ANES",          dv = "Gun control",                      domain = "Non-economic"),
  ANES_Mod_1n = list(model = ANES_Mod_1n, dataset = "ANES",          dv = "Immigration",                      domain = "Non-economic"),
  ANES_Mod_1o = list(model = ANES_Mod_1o, dataset = "ANES",          dv = "Affirmative action",               domain = "Non-economic"),
  ANES_Mod_1p = list(model = ANES_Mod_1p, dataset = "ANES",          dv = "Same-sex marriage",                domain = "Non-economic"),
  ANES_Mod_1q = list(model = ANES_Mod_1q, dataset = "ANES",          dv = "Liberal identification",           domain = "Symbolic"),
  CES_Mod_1a =  list(model = CES_Mod_1a,  dataset = "CES",           dv = "Affordable Care Act",              domain = "Economic"),
  CES_Mod_1b =  list(model = CES_Mod_1b,  dataset = "CES",           dv = "Welfare spending",                 domain = "Economic"),
  CES_Mod_1c =  list(model = CES_Mod_1c,  dataset = "CES",           dv = "Healthcare spending",              domain = "Economic"),
  CES_Mod_1d =  list(model = CES_Mod_1d,  dataset = "CES",           dv = "Abortion",                         domain = "Non-economic"),
  CES_Mod_1e =  list(model = CES_Mod_1e,  dataset = "CES",           dv = "Gun control",                      domain = "Non-economic"),
  CES_Mod_1f =  list(model = CES_Mod_1f,  dataset = "CES",           dv = "Same-sex marriage (2016 only)",    domain = "Non-economic"),
  CES_Mod_1g =  list(model = CES_Mod_1g,  dataset = "CES",           dv = "Immigration (2016 & 2020 only)",   domain = "Non-economic"),
  CES_Mod_1h =  list(model = CES_Mod_1h,  dataset = "CES",           dv = "Liberal identification",           domain = "Symbolic"),
  TAPS_Mod_1a = list(model = TAPS_Mod_1a, dataset = "TAPS",          dv = "Raise taxes on rich*",             domain = "Economic"),
  TAPS_Mod_1b = list(model = TAPS_Mod_1b, dataset = "TAPS",          dv = "Minimum wage*",                    domain = "Economic"),
  TAPS_Mod_1c = list(model = TAPS_Mod_1c, dataset = "TAPS",          dv = "Medicaid expansion*",              domain = "Economic"),
  TAPS_Mod_1d = list(model = TAPS_Mod_1d, dataset = "TAPS",          dv = "Medicare and Medicaid spending*",  domain = "Economic"),
  TAPS_Mod_1e = list(model = TAPS_Mod_1e, dataset = "TAPS",          dv = "Social Security spending*",        domain = "Economic"),
  TAPS_Mod_1f = list(model = TAPS_Mod_1f, dataset = "TAPS",          dv = "Government housing*",              domain = "Economic"),
  TAPS_Mod_1g = list(model = TAPS_Mod_1g, dataset = "TAPS",          dv = "Government spending",              domain = "Economic"),
  TAPS_Mod_1h = list(model = TAPS_Mod_1h, dataset = "TAPS",          dv = "Government guaranteed jobs",       domain = "Economic"),
  TAPS_Mod_1i = list(model = TAPS_Mod_1i, dataset = "TAPS",          dv = "Public healthcare",                domain = "Economic"),
  TAPS_Mod_1j = list(model = TAPS_Mod_1j, dataset = "TAPS",          dv = "Gov care for elderly",             domain = "Economic"),
  TAPS_Mod_1k = list(model = TAPS_Mod_1k, dataset = "TAPS",          dv = "Unemployment insurance",           domain = "Economic"),
  TAPS_Mod_1l = list(model = TAPS_Mod_1l, dataset = "TAPS",          dv = "Income equality",                  domain = "Economic"),
  TAPS_Mod_1m = list(model = TAPS_Mod_1m, dataset = "TAPS",          dv = "Abortion",                         domain = "Non-economic"),
  TAPS_Mod_1n = list(model = TAPS_Mod_1n, dataset = "TAPS",          dv = "Same-sex marriage",                domain = "Non-economic"),
  TAPS_Mod_1o = list(model = TAPS_Mod_1o, dataset = "TAPS",          dv = "Immigration",                      domain = "Non-economic"),
  TAPS_Mod_1p = list(model = TAPS_Mod_1p, dataset = "TAPS",          dv = "Gun control",                      domain = "Non-economic"),
  TAPS_Mod_1q = list(model = TAPS_Mod_1q, dataset = "TAPS",          dv = "Affirmative action",               domain = "Non-economic"),
  TAPS_Mod_1r = list(model = TAPS_Mod_1r, dataset = "TAPS",          dv = "Liberal identification",           domain = "Symbolic"),
  LISS_Mod_1a = list(model = LISS_Mod_1a, dataset = "LISS",          dv = "Income equality",                  domain = "Economic"),
  LISS_Mod_1b = list(model = LISS_Mod_1b, dataset = "LISS",          dv = "Euthanasia",                       domain = "Non-economic"),
  LISS_Mod_1c = list(model = LISS_Mod_1c, dataset = "LISS",          dv = "Immigration",                      domain = "Non-economic"),
  LISS_Mod_1d = list(model = LISS_Mod_1d, dataset = "LISS",          dv = "EU Unification",                   domain = "Non-economic"),
  LISS_Mod_1e = list(model = LISS_Mod_1e, dataset = "LISS",          dv = "Left-wing identification",         domain = "Symbolic"),
  CR_Mod_1a =   list(model = CR_Mod_1a,   dataset = "CloudResearch", dv = "Guaranteed jobs/stndrd of living", domain = "Economic"),
  CR_Mod_1b =   list(model = CR_Mod_1b,   dataset = "CloudResearch", dv = "Public healthcare",                domain = "Economic"),
  CR_Mod_1c =   list(model = CR_Mod_1c,   dataset = "CloudResearch", dv = "Income equality",                  domain = "Economic"),
  CR_Mod_1d =   list(model = CR_Mod_1d,   dataset = "CloudResearch", dv = "Minimum wage",                     domain = "Economic"),
  CR_Mod_1e =   list(model = CR_Mod_1e,   dataset = "CloudResearch", dv = "Abortion",                         domain = "Non-economic"),
  CR_Mod_1f =   list(model = CR_Mod_1f,   dataset = "CloudResearch", dv = "Same-sex adoption",                domain = "Non-economic"),
  CR_Mod_1g =   list(model = CR_Mod_1g,   dataset = "CloudResearch", dv = "Legalize cannabis",                domain = "Non-economic"),
  CR_Mod_1h =   list(model = CR_Mod_1h,   dataset = "CloudResearch", dv = "Immigration",                      domain = "Non-economic"),
  CR_Mod_1i =   list(model = CR_Mod_1i,   dataset = "CloudResearch", dv = "Liberal identification",           domain = "Symbolic")
)

fig1.data <- data.frame()

for (model_name in names(mod1_list)) {
  model_info <- mod1_list[[model_name]]
  model <- model_info$model
  dataset <- factor(model_info$dataset, levels = c("ANES", "CES", "TAPS", "LISS", "CloudResearch"))
  dv <- model_info$dv
  `Attitude Domain` <- factor(model_info$domain, levels = c("Economic", "Non-economic", "Symbolic"))
  
  tidy_df1 <- broom.mixed::tidy(model)
  tidy_df1 <- tidy_df1 %>%
    mutate(
      conf.low = estimate - 1.96 * std.error,
      conf.high = estimate + 1.96 * std.error,
    )
  
  tidy_df1 <- tidy_df1 %>%
    filter(term == "anxiety") %>%
    mutate(
      dataset = dataset,
      dv = dv,
      `Attitude Domain` = `Attitude Domain`,
      model = model_name,
    )
  
  tidy_df1 <- dplyr::select(tidy_df1, term, estimate, conf.low, conf.high, dataset, dv, `Attitude Domain`, model)
  fig1.data <- rbind(fig1.data, tidy_df1)
}


# Choose where file saves
Cairo(6800, 8000, file="Figure 1.png", res = 800)

ggplot(fig1.data, aes(x = estimate, y = reorder_within(dv, estimate, dataset),
                      xmin = conf.low, xmax = conf.high,
                      shape = `Attitude Domain`)) +
  geom_pointrange(aes(shape = `Attitude Domain`), position = ggstance::position_dodgev(.85), size = .5) +
  geom_vline(xintercept = 0, linetype = "ff") +
  scale_shape_manual(values = c(19, 4, 0)) +
  guides(shape = guide_legend(title.position = "top", title.hjust=0),
         linetype = "none") +
  scale_y_reordered() +
  facet_grid(dataset ~ ., scales = "free_y", space = "free_y") +
  labs(y = "", x = "Effect of Anxiety") +
  theme_bw() +
  theme(axis.title.y = element_text(size = 16, vjust = 1),
        strip.text.y = element_text(angle = 0, size = 10.5),
        axis.text = element_text(size = 10),
        axis.title.x = element_text(size = 16, hjust = 0.55),
        legend.position = "right",
        legend.box.background = element_blank(),
        legend.box.margin = margin(-1,-1,0,0),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.key.size = unit(0.5, "cm"),
        legend.spacing.y = unit(0.5, "mm"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 11))

# Save file
dev.off()







##################################################
#################### Figure 2 ####################
##################################################

# Models
ANES_Mod_2 <- lmer(econ_policy ~ age + male + race + education + `income (reversed)` + `do not own home` + unemployed + uninsured + openness + conscientiousness + extraversion + agreeableness + anxiety + volatility + (1 | year), data = ANES)
CES_Mod_2 <- lmer(econ_policy ~ age + male + race + education + `income (reversed)` + `do not own home` + unemployed + uninsured + openness + conscientiousness + extraversion + agreeableness + anxiety + volatility + (1 | year), data = CES)
LISS_Mod_2 <- lm(econ_policy ~ age + male + race + education + `income (reversed)` + `do not own home` + unemployed + openness + conscientiousness + extraversion + agreeableness + anxiety + volatility, data = LISS)
TAPS_Mod_2 <- lm(econ_policy ~ age + male + race + education + `income (reversed)` + `do not own home` + unemployed + uninsured + openness + conscientiousness + extraversion + agreeableness + anxiety + volatility, data = TAPS)
CR_Mod_2 <- lmer(econ_policy ~ age + male + race + education + `income (reversed)` + anxiety + volatility + (1 | study), data = CR_Pooled)

mod2_list <- list(
  ANES_Mod_2 = list(model = ANES_Mod_2, dataset = "ANES", dv = "Economic"),
  CES_Mod_2 = list(model = CES_Mod_2, dataset = "CES", dv = "Economic"),
  TAPS_Mod_2 = list(model = TAPS_Mod_2, dataset = "TAPS", dv = "Economic"),
  LISS_Mod_2 = list(model = LISS_Mod_2, dataset = "LISS", dv = "Economic"),
  CR_Mod_2 = list(model = CR_Mod_2, dataset = "CloudResearch", dv = "Economic"))

fig2_vars <- c("anxiety", "volatility")

fig2.data <- data.frame()

for (model_name in names(mod2_list)) {
  model_info <- mod2_list[[model_name]]
  model <- model_info$model
  dataset <- model_info$dataset
  
  tidy_df2 <- broom.mixed::tidy(model)
  tidy_df2 <- tidy_df2 %>%
    mutate(
      conf.low = estimate - 1.96 * std.error,
      conf.high = estimate + 1.96 * std.error
    )
  
  tidy_df2 <- tidy_df2 %>%
    filter(term %in% fig2_vars) %>%
    mutate(
      dataset = dataset,
      model = model_name
    )
  
  tidy_df2 <- dplyr::select(tidy_df2, term, estimate, conf.low, conf.high, dataset, model)
  fig2.data <- rbind(fig2.data, tidy_df2)
}

fig2.data$dataset <- factor(fig2.data$dataset, levels = c("ANES", "CES", "TAPS", "LISS", "CloudResearch"))
fig2.data$term <- factor(fig2.data$term, levels = c("anxiety", "volatility"))
levels(fig2.data$term) <- c("Anxiety", "Volatility")

# Choose where file saves
Cairo(5625, 2600, file="Figure 2.png", res = 800)

# Plotting
ggplot(fig2.data, aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "ff") +
  facet_grid( ~ dataset, scales = "free_x", space = "free_x") +
  labs(y = "  Effect on Economic Attitudes", x = "") + 
  theme_bw(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        axis.title.y = element_text(hjust = 0.3),
        strip.text.y = element_text(angle = 0),
        legend.position = "bottom",
        legend.box.background = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.key.size = unit(0.5, "cm"),
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.margin = margin(-15,0,0,0))

# Save plot
dev.off()







##################################################
#################### Figure 3 ####################
##################################################

fig3_vars <- c("anxiety", "`income (reversed)`", "`do not own home`", "unemployed", "uninsured")

fig3.data <- data.frame()

for (model_name in names(mod2_list)) {
  model_info <- mod2_list[[model_name]]
  model <- model_info$model
  dataset <- model_info$dataset
  dv <- model_info$dv
  
  tidy_df3 <- broom.mixed::tidy(model)
  tidy_df3 <- tidy_df3 %>%
    mutate(
      conf.low = estimate - 1.96 * std.error,
      conf.high = estimate + 1.96 * std.error,
    )
  
  tidy_df3 <- tidy_df3 %>% 
    filter(term %in% fig3_vars) %>%
    mutate(dataset = dataset, 
           dv = dv, 
           model = model_name)
  
  tidy_df3 <- dplyr::select(
    tidy_df3, 
    term, 
    estimate, 
    conf.low, 
    conf.high, 
    dataset, 
    dv, 
    model
  )
  fig3.data <- rbind(fig3.data, tidy_df3)
}

# Set order of factor levels
fig3.data$dataset <- factor(fig3.data$dataset, levels = c("ANES", "CES", "TAPS", "LISS", "CloudResearch"))
fig3.data$term <- factor(fig3.data$term, levels = c("anxiety", "`income (reversed)`", "`do not own home`", "unemployed", "uninsured"))
levels(fig3.data$term) <- c("Anxiety", "Income (reversed)", "Do not own home", "Unemployed", "No insurance")
fig3.data$term_type <- factor(ifelse(fig3.data$term == "Anxiety", "Personality", "Socioeconomic"), levels = c("Personality", "Socioeconomic"))

# Choose where file saves
Cairo(5210, 4200, file="Figure 3.png", res = 800)

ggplot(fig3.data, aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high, shape = term_type)) +
  geom_pointrange(aes(shape = term_type), size = .5) +
  scale_shape_manual(values = c(19, 1)) +
  geom_hline(yintercept = 0, linetype = "ff") +
  facet_wrap( ~ dataset) +
  labs(y = "Effect on Economic Attitudes", x = "") + 
  theme_bw(base_size = 13) +
  theme(axis.text.x = element_text(angle = -45, vjust = 1, hjust=0),
        plot.margin = margin(0.5, 1, 0, 0.5, "cm"),
        legend.position = c(0.84, .09),
        legend.box.background = element_blank(),
        legend.box.margin = margin(-1,-1,0,0),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.key.size = unit(0.5, "cm"), 
        legend.spacing.y = unit(0.5, "mm"), 
        legend.text = element_text(size = 9),
        legend.title = element_blank())

# Save file
dev.off()







##################################################
#################### Figure 4 ####################
##################################################

# Models
# Reduced Models (Only Direct Effects of Big 5)
LISS_Mod_4a <- lm(econ_policy ~ age + male + race + education + income + own_home + unemployed + openness + conscientiousness + extraversion + agreeableness + volatility + anxiety * social_exclusion, data = LISS)
TAPS_Mod_4a <- lm(econ_policy ~ age + male + race + education + income + own_home + unemployed + uninsured + openness + conscientiousness + extraversion + agreeableness + volatility + anxiety * social_exclusion, data = TAPS)
CES_Mod_4a.1 <- lmer(econ_policy ~ age + male + race + education + income + own_home + unemployed + uninsured + openness + conscientiousness + extraversion + agreeableness + volatility + anxiety * fb_friends_binary + (1 | year), data = CES)
CES_Mod_4a.2 <- lmer(econ_policy ~ age + male + race + education + income + own_home + unemployed + uninsured + openness + conscientiousness + extraversion + agreeableness + volatility + anxiety * facebook_friends + (1 | year), data = CES)

# Full Models (Big 5 interacted with Exclusion)
LISS_Mod_4b <- lm(econ_policy ~ age + male + race + education + income + own_home + unemployed + (openness + conscientiousness + extraversion + agreeableness + volatility + anxiety) * social_exclusion, data = LISS)
TAPS_Mod_4b <- lm(econ_policy ~ age + male + race + education + income + own_home + unemployed + uninsured + (openness + conscientiousness + extraversion + agreeableness + volatility + anxiety) * social_exclusion, data = TAPS)
CES_Mod_4b.1 <- lmer(econ_policy ~ age + male + race + education + income + own_home + unemployed + uninsured + (openness + conscientiousness + extraversion + agreeableness + volatility + anxiety) * fb_friends_binary + (1 | year), data = CES)
CES_Mod_4b.2 <- lmer(econ_policy ~ age + male + race + education + income + own_home + unemployed + uninsured + (openness + conscientiousness + extraversion + agreeableness + volatility + anxiety) * facebook_friends + (1 | year), data = CES)

# Calculate 5th and 95th percentiles of social_exclusion for each model
liss_quantiles <- quantile(LISS_Mod_4b$model$social_exclusion, c(0.05, 0.95))
taps_quantiles <- quantile(TAPS_Mod_4b$model$social_exclusion, c(0.05, 0.95))

# Get predicted values
LISS_Mod_4_pred <- ggpredict(LISS_Mod_4b, terms = c("anxiety", "social_exclusion[liss_quantiles]"))
TAPS_Mod_4_pred <- ggpredict(TAPS_Mod_4b, terms = c("anxiety", "social_exclusion[taps_quantiles]"))
CES_Mod_4_pred <- ggpredict(CES_Mod_4b.1, terms = c("anxiety", "fb_friends_binary[0, 1]"))

# Combine the datasets
fig4.data <- rbind(LISS_Mod_4_pred, TAPS_Mod_4_pred, CES_Mod_4_pred)
fig4.data$dataset <- factor(c(rep("LISS", nrow(LISS_Mod_4_pred)), rep("TAPS", nrow(TAPS_Mod_4_pred)), rep("CES", nrow(CES_Mod_4_pred))), levels = c("LISS", "TAPS", "CES"))
fig4.data$group_factor <- factor(ifelse(fig4.data$dataset == "CES",
                                        ifelse(fig4.data$group == "0", "ff", "solid"),
                                        ifelse(fig4.data$group == "0", "ff", "solid")))
fig4.data$line_text <- ifelse(fig4.data$dataset == "CES",
                              ifelse(fig4.data$group == "0", "More Friends", "Fewer Friends"),
                              ifelse(fig4.data$group == "0", "Feel Included", "Feel Excluded"))
fig4.data$hjust <- ifelse(fig4.data$dataset == "LISS", 0.95, ifelse(fig4.data$dataset == "TAPS", 0.8, 0.1))

# Choose where plot saves
Cairo(5625, 3000, file="Figure 4.png", res = 800)

# Plotting
ggplot(fig4.data, aes(x = x, y = predicted, group = group_factor)) +
  geom_textline(aes(x = x, y = predicted, linetype = group_factor, label = line_text, hjust = hjust), size = 3.25) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  scale_linetype_manual(values = c("solid" = "solid", "ff" = "ff")) +
  scale_x_continuous(name='Anxiety (Neuroticism Facet)', 
                     breaks = c(0, .5, 1), labels = c("   Low", "Medium", "High   ")) +
  facet_wrap( ~ dataset, ncol = 3) +
  labs(y = "Left-Wing Economic Attitudes (0 - 1)", x = "Anxiety (Neuroticism facet)") + 
  theme_bw(base_size = 13) +
  theme(legend.position = "none",
        axis.title.x = element_text(vjust = -1.5))

# Save plot
dev.off()







##################################################
#################### Figure 5 ####################
##################################################

# Reduced Models (Only Direct Effects of Big 5)
LISS_Mod_5a <- lm(econ_policy ~ age + male + race + education + (`income (reversed)` + `do not own home` + unemployed) * anxiety + social_exclusion * anxiety + volatility + openness + conscientiousness + extraversion + agreeableness, data = LISS)
TAPS_Mod_5a <- lm(econ_policy ~ age + male + race + education + (`income (reversed)` + `do not own home` + unemployed + uninsured) * anxiety + social_exclusion * anxiety + volatility + openness + conscientiousness + extraversion + agreeableness, data = TAPS)
CES_Mod_5a <- lmer(econ_policy ~ age + male + race + education + (`income (reversed)` + `do not own home` + unemployed + uninsured) * anxiety + fb_friends_binary * anxiety + volatility + openness + conscientiousness + extraversion + agreeableness + (1 | year), data = CES)

# Full Models (Big 5 interacted with Exclusion)
LISS_Mod_5b <- lm(econ_policy ~ age + male + race + education + (`income (reversed)` + `do not own home` + unemployed) * anxiety + social_exclusion * (anxiety + volatility + openness + conscientiousness + extraversion + agreeableness), data = LISS)
TAPS_Mod_5b <- lm(econ_policy ~ age + male + race + education + (`income (reversed)` + `do not own home` + unemployed + uninsured) * anxiety + social_exclusion * (anxiety + volatility + openness + conscientiousness + extraversion + agreeableness), data = TAPS)
CES_Mod_5b <- lmer(econ_policy ~ age + male + race + education + (`income (reversed)` + `do not own home` + unemployed + uninsured) * anxiety + fb_friends_binary * (anxiety + volatility + openness + conscientiousness + extraversion + agreeableness) + (1 | year), data = CES)

mod5_list <- list(
  LISS_Mod_5 = list(model = LISS_Mod_5b, dataset = "LISS", sample = "All respondents"),
  TAPS_Mod_5 = list(model = TAPS_Mod_5b, dataset = "TAPS", sample = "All respondents"),
  CES_Mod_5 = list(model = CES_Mod_5b, dataset = "CES", sample = "All respondents")
)

vars_of_interest <- c("anxiety:social_exclusion", "anxiety:fb_friends_binary", "`income (reversed)`:anxiety", "`do not own home`:anxiety", "unemployed:anxiety", "uninsured:anxiety")

fig5.data <- data.frame()

for (model_name in names(mod5_list)) {
  model_info <- mod5_list[[model_name]]
  model <- model_info$model
  dataset <- model_info$dataset

  tidy_df5 <- broom.mixed::tidy(model)
  
  tidy_df5 <- tidy_df5 %>%
    filter(term %in% vars_of_interest) %>%
    rowwise() %>%
    mutate(
      conf.low = estimate - 1.96 * sqrt(vcov(model)[term, term]),
      conf.high = estimate + 1.96 * sqrt(vcov(model)[term, term])
    ) %>%
    ungroup()

  tidy_df5 <- tidy_df5 %>%
    mutate(
      dataset = dataset, 
      model = model_name
    ) %>%
    dplyr::select(term, estimate, conf.low, conf.high, dataset, model)

  fig5.data <- bind_rows(fig5.data, tidy_df5)
}

# Assemble data.frame
fig5.data <- dplyr::select(fig5.data, term, estimate, conf.low, conf.high, dataset, model)

# Rename terms
fig5.data[fig5.data$term == "anxiety:social_exclusion", "term"] <- "Anxiety × Social exclusion"
fig5.data[fig5.data$term == "anxiety:fb_friends_binary", "term"] <- "Anxiety × Fewer than 500 Friends"
fig5.data[fig5.data$term == "`income (reversed)`:anxiety", "term"] <- "Anxiety × income (reversed)"
fig5.data[fig5.data$term == "`do not own home`:anxiety", "term"] <- "Anxiety × do not own home"
fig5.data[fig5.data$term == "unemployed:anxiety", "term"] <- "Anxiety × unemployed"
fig5.data[fig5.data$term == "uninsured:anxiety", "term"] <- "Anxiety × No insurance"

# Set order of factor levels
fig5.data$dataset <- factor(fig5.data$dataset, levels = c("LISS", "TAPS", "CES"))
fig5.data$term <- factor(fig5.data$term, levels = c("Anxiety × Social exclusion", "Anxiety × Fewer than 500 Friends", "Anxiety × income (reversed)", "Anxiety × do not own home", "Anxiety × unemployed", "Anxiety × No insurance"))
fig5.data$term_type <- ifelse(fig5.data$term %in% c("Anxiety × Social exclusion", "Anxiety × Fewer than 500 Friends"), "Exclusion", "Material")

# Choose where file saves
Cairo(4800, 4000, file="Figure 5.png", res = 800)

ggplot(fig5.data, aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high, shape = term_type)) +
  geom_pointrange(aes(shape = term_type)) +
  scale_shape_manual(values = c(19, 1, 1, 1, 1), guide = "none") +
  scale_y_continuous(name='Effect on Economic Attitudes', 
                     breaks = c(-.5, 0, .5), 
                     labels = c("-0.5", "0.0", "0.5")) +
  geom_hline(yintercept = 0, linetype = "ff") +
  facet_grid( ~ dataset, scales = "free_x", space = "free_x") +
  xlab("") +
  theme_bw(base_size = 13) +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
        legend.position = "bottom",
        legend.box.background = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.key.size = unit(0.5, "cm"),
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.margin = margin(-15,0,0,0))

# Save file
dev.off()







##################################################
#################### Figure 7 ####################
##################################################

# Models
CR_Study1_Mod_6a <- lm(econ_policy ~ (age + male + race + education + income + volatility + anxiety) * social_exclusion, data = CR_Study1)
CR_Study2_Mod_6a <- lm(econ_policy ~ (age + male + race + education + income + volatility + anxiety) * social_exclusion, data = CR_Study2)
CR_Mod_6a <- lmer(econ_policy ~ (age + male + race + education + income + volatility + anxiety) * social_exclusion + (1 | study), data = CR_Pooled)
CR_Study1_Mod_6b <- lm(econ_policy ~ (age + male + race + education + income + volatility + anxiety) * social_exclusion, data = CR_Study1[CR_Study1$political_engagement > median(CR_Study1$political_engagement),])
CR_Study2_Mod_6b <- lm(econ_policy ~ (age + male + race + education + income + volatility + anxiety) * social_exclusion, data = CR_Study2[CR_Study2$political_engagement > median(CR_Study2$political_engagement),])
CR_Mod_6b <- lmer(econ_policy ~ (age + male + race + education + income + volatility + anxiety) * social_exclusion + (1 | study), data = CR_Pooled[CR_Pooled$political_engagement > median(CR_Pooled$political_engagement),])

# Get predicted values
CR_Study1_Mod_6a_pred <- ggpredict(CR_Study1_Mod_6a, terms = c("anxiety", "social_exclusion"))
CR_Study2_Mod_6a_pred <- ggpredict(CR_Study2_Mod_6a, terms = c("anxiety", "social_exclusion"))
CR_Mod_6a_pred <- ggpredict(CR_Mod_6a, terms = c("anxiety", "social_exclusion"))
CR_Study1_Mod_6b_pred <- ggpredict(CR_Study1_Mod_6b, terms = c("anxiety", "social_exclusion"))
CR_Study2_Mod_6b_pred <- ggpredict(CR_Study2_Mod_6b, terms = c("anxiety", "social_exclusion"))
CR_Mod_6b_pred <- ggpredict(CR_Mod_6b, terms = c("anxiety", "social_exclusion"))

# Combine the datasets
fig7.data <- rbind(CR_Study1_Mod_6a_pred, CR_Study2_Mod_6a_pred, CR_Mod_6a_pred, CR_Study1_Mod_6b_pred, CR_Study2_Mod_6b_pred, CR_Mod_6b_pred)
fig7.data$dataset <- factor(c(rep(c("Study 1", "Study 2", "Combined Studies"), c(nrow(CR_Study1_Mod_6a_pred), nrow(CR_Study2_Mod_6a_pred), nrow(CR_Mod_6a_pred))), 
                              rep(c("Study 1", "Study 2", "Combined Studies"), c(nrow(CR_Study1_Mod_6b_pred), nrow(CR_Study2_Mod_6b_pred), nrow(CR_Mod_6b_pred)))), 
                            levels = c("Study 1", "Study 2", "Combined Studies"))
fig7.data$engagement <- factor(c(rep("All Respondents", sum(c(nrow(CR_Study1_Mod_6a_pred), nrow(CR_Study2_Mod_6a_pred), nrow(CR_Mod_6a_pred)))), 
                                 rep("Politically Engaged", sum(c(nrow(CR_Study1_Mod_6b_pred), nrow(CR_Study2_Mod_6b_pred), nrow(CR_Mod_6b_pred))))))
fig7.data$group_factor <- factor(ifelse(fig7.data$group == 1, "Excluded", "Included"))
fig7.data$line_text <- ifelse(fig7.data$group == 1, "Excluded", "Included")
fig7.data$hjust <- ifelse(fig7.data$engagement == "All Respondents",
                          ifelse(fig7.data$dataset == "Study 1", 0.95, ifelse(fig7.data$dataset == "Study 2", 0.85, 0.85)),
                          ifelse(fig7.data$dataset == "Study 1", 0.1, ifelse(fig7.data$dataset == "Study 2", 0.85, 0.85)))

# Choose where plot saves
Cairo(5000, 5000, file="Figure 7.png", res = 800)

# Plotting
ggplot(fig7.data, aes(x = x, y = predicted, group = group_factor)) +
  geom_textline(aes(x = x, y = predicted, linetype = group_factor, label = line_text, hjust = hjust), size = 2.85) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  scale_linetype_manual(values = c("Included" = "ff", "Excluded" = "solid")) +
  scale_x_continuous(name = 'Anxiety (Neuroticism Facet)', 
                     breaks = c(0, .5, 1), 
                     labels = c("   Low", "Medium", "High   ")) +
  facet_wrap(engagement ~ dataset) +
  labs(y = "    Left-Wing Economic Attitudes (0 - 1)", x = "Anxiety (Neuroticism facet)") + 
  theme_bw(base_size = 13) +
  theme(legend.position = "none",
        strip.text.y = element_text(angle = 0),
        axis.title.x = element_text(vjust = -1))

# Save plot
dev.off()

