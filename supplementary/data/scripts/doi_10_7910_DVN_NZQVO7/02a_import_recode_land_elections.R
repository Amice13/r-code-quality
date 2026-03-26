####################################################################
####################################################################
## Replication Material
##
## Shaun Bowler, Gail McElroy, Stefan Müller:
## Voter Expectations of Government Formation in Coalition Systems: 
## The Importance of the Information Context.
## European Journal of Political Research

## File: 02a_import_recode_federal_elections.R

## See 000_description_replication_material.pdf for a detailed 
## overview of the required data and the outputs of this file.

####################################################################
####################################################################


## script to adjust variable coding and rename variables from each Land election survey
## to a common format

## Note: this script requires the raw survey data which 
## need to be downloaded (after a free registration) from 
## the GESIS website.
## If you include these datasets into a sub-folder called 
## data_gesis, you can rerun this script and reproduce the analysis
## The output of this file (= a merged and harmonised dataset) 
## is called data_studies_merged_land.rds, included in the Dataverse
## and used in subsequent scripts


## load required packages 

library(haven) # Import and Export 'SPSS', 'Stata' and 'SAS' Files, CRAN v2.3.1
library(foreign) # Read Data Stored by 'Minitab', 'S', 'SAS', 'SPSS', 'Stata','Systat', 'Weka', 'dBase', ..., CRAN v0.8-80
library(dplyr) # A Grammar of Data Manipulation, CRAN v1.0.2
library(tidyr) # Tidy Messy Data, CRAN v1.1.2
library(stringr) # Simple, Consistent Wrappers for Common String Operations, CRAN v1.4.0
library(car) # Companion to Applied Regression, CRAN v3.0-9


## start importing Land elections and select/rename 
## relevant variables

data_2010_nrw_raw <- foreign::read.dta("data_gesis/ZA5324_v2-0-0.dta") %>% 
    as_factor()

data_2010_nrw <- transmute(data_2010_nrw_raw,
                           study_id = "ZA5324_v2-0-0",
                           election_id = "Nordrhein-Westfalen 2010",
                           weight_microc = as.numeric(gew1_nrw),
                           weight_online = as.numeric(gew2_nrw),
                           # duration_survey = field,
                           responses_quick = Status,
                           # duration = duration,
                           gender = v_01,
                           year_of_birth = as.factor(v_02),
                           education = v_03,
                           pol_interest_fed =  L38a,
                           pol_interest_land = L38b,
                           satisf_democracy_fed = A02b_a,
                           vote_intention_land = L02,
                           scale_satisf_gov_land =  L09,
                           interest_campaign = L30,
                           internet_use_political = A34_wk,
                           vote_party_land = L03_b,
                           vote_party_land_postal = L03_brief_b,
                           vote_party_land_hyp = L04,
                           vote_party_sure = L05a,
                           importance_election_result = L16,
                           scale_cdu = L07a,
                           scale_spd = L07b,
                           scale_fdp = L07c,
                           scale_greens = L07e,
                           scale_left = L07d,
                           wish_coa_cdu = L67a,
                           wish_coa_spd = L67b,
                           wish_coa_greens = L67e,
                           wish_coa_left = L67d,
                           wish_coa_fdp = L67c,
                           wish_coa_other = L67s_c,
                           prob_coa_cdu_spd = L27a, 
                           prob_coa_cdu_fdp = L27b,
                           prob_coa_spd_greens = L27c,
                           prob_coa_spd_fdp_greens = L27d,
                           prob_coa_cdu_fdp_greens = L27e,
                           prob_coa_spd_left_greens = L27f,
                           prob_coa_cdu_greens = L27g,
                           
                           predict_gov_cdu = L68a,
                           predict_gov_spd = L68b,
                           predict_gov_fdp = L68c,
                           predict_gov_left = L68d,
                           predict_gov_greens = L68e,
                           predict_gov_other = L68f,
                           
                           lr_self = A33,
                           lr_cdu = A32a_a,
                           lr_spd = A32b_a,
                           lr_fdp = A32c_a,
                           lr_left = A32d_a,
                           lr_greens = A32e_a,
                           party_id = A36,
                           party_id_2 = A36c,
                           party_id_strength = A37,
                           job_type = A53,
                           job_type_past = A53a,
                           income = A47_a)

data_2011_sachsen_anhalt_raw <- haven::read_dta("data_gesis/ZA5325_v3-0-0.dta") %>% 
    as_factor()

data_2011_sachsen_anhalt <- transmute(data_2011_sachsen_anhalt_raw,
                                      study_id = "ZA5325_v3-0-0",
                                      election_id = "Sachsen-Anhalt 2011",
                                      weight_microc = as.numeric(gew1_st_v0),
                                      weight_online = as.numeric(gew2_st_v0),
                                      #duration_survey = field,
                                      responses_quick = Status,
                                      gender = v_01,
                                      year_of_birth = as.factor(v_02),
                                      education = v_03,
                                      pol_interest_fed =  L38a,
                                      pol_interest_land = L38b,
                                      satisf_democracy_fed = A02b_a,
                                      vote_intention_land = L02,
                                      scale_satisf_gov_land =  L09,
                                      interest_campaign = L30,
                                      internet_use_political = A34_wk,
                                      vote_party_land = L03_b,
                                      vote_party_land_postal = L03_brief_b,
                                      vote_party_land_hyp = L04,
                                      vote_party_sure = L05a,
                                      importance_election_result = L16,
                                      scale_cdu = L07a,
                                      scale_spd = L07b,
                                      scale_fdp = L07c,
                                      scale_greens = L07e,
                                      scale_left = L07d,
                                      wish_coa_cdu = L67a, 
                                      wish_coa_spd = L67b,
                                      wish_coa_greens = L67e,
                                      wish_coa_left = L67d,
                                      wish_coa_fdp = L67c,
                                      wish_coa_other = L67f,
                                      
                                      prob_coa_cdu_spd = L27a, 
                                      prob_coa_cdu_fdp = L27b,
                                      prob_coa_spd_greens = L27c,
                                      prob_coa_spd_fdp_greens = L27d,
                                      prob_coa_cdu_fdp_greens = L27e,
                                      prob_coa_spd_left_greens = L27f,
                                      prob_coa_cdu_greens = L27g,
                                      
                                      predict_gov_cdu = L68a,
                                      predict_gov_spd = L68b,
                                      predict_gov_fdp = L68c,
                                      predict_gov_left = L68d,
                                      predict_gov_greens = L68e,
                                      predict_gov_other = L68f,
                                      
                                      lr_self = A33,
                                      lr_cdu = A32a_a,
                                      lr_spd = A32c_a,
                                      lr_fdp = A32d_a,
                                      lr_left = A32e_a,
                                      lr_greens = A32f_a,
                                      party_id = A36,
                                      party_id_2 = A36c,
                                      party_id_strength = A37,
                                      job_type = A53,
                                      job_type_past = A53a,
                                      income = A47_a)


# data_2011_bremen ("ZA5326_v1-0-0") has no data on predictions 
# because survey conducted AFTER election day

data_2011_rheinland_pfalz_raw <- haven::read_dta("data_gesis/ZA5327_v3-0-0.dta") %>% 
    as_factor()

data_2011_rheinland_pfalz <- transmute(data_2011_rheinland_pfalz_raw,
                                       study_id = "ZA5327_v3-0-0",
                                       election_id = "Rheinland-Pfalz 2011",
                                       weight_microc = as.numeric(gew1_rp_v0),
                                       weight_online = as.numeric(gew2_rp_v0),
                                       # duration_survey = field,
                                       responses_quick = Status,
                                       # duration = duration,
                                       gender = v_01,
                                       year_of_birth = as.factor(v_02),
                                       education = v_03,
                                       pol_interest_fed =  L38a,
                                       pol_interest_land = L38b,
                                       satisf_democracy_fed = A02b_a,
                                       vote_intention_land = L02,
                                       scale_satisf_gov_land =  L09,
                                       interest_campaign = L30,
                                       internet_use_political = A34_wk,
                                       vote_party_land = L03_b,
                                       vote_party_land_postal = L03_brief_b,
                                       vote_party_land_hyp = L04,
                                       vote_party_sure = L05a,
                                       importance_election_result = L16,
                                       scale_cdu = L07a,
                                       scale_spd = L07b,
                                       scale_fdp = L07c,
                                       scale_greens = L07e,
                                       scale_left = L07d,
                                       wish_coa_cdu = L67a, 
                                       wish_coa_spd = L67b,
                                       wish_coa_greens = L67e,
                                       wish_coa_left = L67d,
                                       wish_coa_fdp = L67c,
                                       wish_coa_other = L67fs_c,
                                       
                                       prob_coa_cdu_spd = L27a, 
                                       prob_coa_cdu_fdp = L27b,
                                       prob_coa_spd_greens = L27c,
                                       prob_coa_spd_fdp_greens = L27d,
                                       prob_coa_cdu_fdp_greens = L27e,
                                       prob_coa_spd_left_greens = L27f,
                                       prob_coa_cdu_greens = L27g,
                                       
                                       predict_gov_cdu = L68a,
                                       predict_gov_spd = L68b,
                                       predict_gov_fdp = L68c,
                                       predict_gov_left = L68d,
                                       predict_gov_greens = L68e,
                                       predict_gov_other = L68f,
                                       
                                       lr_self = A33,
                                       lr_cdu = A32a_a,
                                       lr_spd = A32b_a,
                                       lr_fdp = A32c_a,
                                       lr_left = A32d_a,
                                       lr_greens = A32e_a,
                                       party_id = A36,
                                       party_id_2 = A36c,
                                       party_id_strength = A37,
                                       job_type = A53,
                                       job_type_past = A53a,
                                       income = A47_a)


data_2011_baden_wuerttemberg_raw <- haven::read_dta("data_gesis/ZA5328_v3-0-0.dta") %>% 
    as_factor()

data_2011_baden_wuerttemberg <- transmute(data_2011_baden_wuerttemberg_raw,
                                          study_id = "ZA5328_v3-0-0",
                                          election_id = "Baden-Württemberg 2011",
                                          weight_microc = as.numeric(gew1_bw_v0),
                                          weight_online = as.numeric(gew2_bw_v0),
                                          #duration_survey = field,
                                          responses_quick = Status,
                                          # duration = duration,
                                          gender = v_01,
                                          year_of_birth = as.factor(v_02),
                                          education = v_03,
                                          pol_interest_fed =  L38a,
                                          pol_interest_land = L38b,
                                          satisf_democracy_fed = A02b_a,
                                          vote_intention_land = L02,
                                          scale_satisf_gov_land =  L09,
                                          interest_campaign = L30,
                                          internet_use_political = A34_wk,
                                          vote_party_land = L03,
                                          vote_party_land_postal = L03_brief,
                                          vote_party_land_hyp = L04,
                                          vote_party_sure = L05a,
                                          importance_election_result = L16,
                                          scale_cdu = L07a,
                                          scale_spd = L07b,
                                          scale_fdp = L07c,
                                          scale_greens = L07e,
                                          scale_left = L07d,
                                          wish_coa_cdu = L67a, 
                                          wish_coa_spd = L67b,
                                          wish_coa_greens = L67e,
                                          wish_coa_left = L67d,
                                          wish_coa_fdp = L67c,
                                          wish_coa_other = L67fs_c,
                                          
                                          prob_coa_cdu_spd = L27a, 
                                          prob_coa_cdu_fdp = L27b,
                                          prob_coa_spd_greens = L27c,
                                          prob_coa_spd_fdp_greens = L27d,
                                          prob_coa_cdu_fdp_greens = L27e,
                                          prob_coa_spd_left_greens = L27f,
                                          prob_coa_cdu_greens = L27g,
                                          
                                          predict_gov_cdu = L68a,
                                          predict_gov_spd = L68b,
                                          predict_gov_fdp = L68c,
                                          predict_gov_left = L68d,
                                          predict_gov_greens = L68e,
                                          predict_gov_other = L68f,
                                          
                                          lr_self = A33,
                                          lr_cdu = A32a_a,
                                          lr_spd = A32b_a,
                                          lr_fdp = A32c_a,
                                          lr_left = A32d_a,
                                          lr_greens = A32e_a,
                                          party_id = A36,
                                          party_id_2 = A36c,
                                          party_id_strength = A37,
                                          job_type = A53,
                                          job_type_past = A53a,
                                          income = A47_a)


data_2011_berlin_raw <- foreign::read.dta("data_gesis/ZA5329_v1-0-0.dta") %>% 
    as_factor()

data_2011_berlin <- transmute(data_2011_berlin_raw,
                              study_id = "ZA5329_v1-0-0",
                              election_id = "Berlin 2011",
                              weight_microc = as.numeric(gew1_be_v0),
                              weight_online = as.numeric(gew2_be_v0),
                              #duration_survey = field,
                              responses_quick = Status,
                              # duration = duration,
                              gender = v_01,
                              year_of_birth = as.factor(v_02),
                              education = v_03,
                              pol_interest_fed =  l38a,
                              pol_interest_land = l38b,
                              satisf_democracy_fed = a02b_a,
                              vote_intention_land = l02,
                              scale_satisf_gov_land =  l09,
                              interest_campaign = l30,
                              internet_use_political = a34_wk,
                              vote_party_land = l03b,
                              vote_party_land_postal = l03_brief_b,
                              vote_party_land_hyp = l04a,
                              vote_party_sure = l05a,
                              importance_election_result = l16,
                              scale_cdu = l07a,
                              scale_spd = l07b,
                              scale_fdp = l07c,
                              scale_greens = l07e,
                              scale_left = l07d,
                              wish_coa_cdu = l67a, 
                              wish_coa_spd = l67b,
                              wish_coa_greens = l67e,
                              wish_coa_left = l67d,
                              wish_coa_fdp = l67c,
                              wish_coa_other = l67fs_c,
                              
                              prob_coa_cdu_spd = l27a, 
                              prob_coa_cdu_fdp = l27b,
                              prob_coa_spd_greens = l27c,
                              prob_coa_spd_fdp_greens = l27d,
                              prob_coa_cdu_fdp_greens = l27e,
                              prob_coa_spd_left_greens = l27f,
                              prob_coa_cdu_greens = l27g,
                              
                              predict_gov_cdu = l68a,
                              predict_gov_spd = l68b,
                              predict_gov_fdp = l68c,
                              predict_gov_left = l68d,
                              predict_gov_greens = l68e,
                              predict_gov_other = l68f,
                              
                              lr_self = a33,
                              lr_cdu = a32a_a,
                              lr_spd = a32c_a,
                              lr_fdp = a32d_a,
                              lr_left = a32e_a,
                              lr_greens = a32f_a,
                              party_id = a36,
                              party_id_2 = a36c,
                              party_id_strength = a37,
                              job_type = a53,
                              job_type_past = a53a,
                              income = a47_a)

data_2011_mecklenburg_vorpommern_raw <- foreign::read.dta("data_gesis/ZA5330_v1-0-0.dta") %>% 
    as_factor()

data_2011_mecklenburg_vorpommern <- transmute(data_2011_mecklenburg_vorpommern_raw,
                                              study_id = "ZA5330_v1-0-0",
                                              election_id = "Mecklenburg-Vorpommern 2011",
                                              weight_microc = as.numeric(gew1_mv_v0),
                                              weight_online = as.numeric(gew2_mv_v0),
                                              #duration_survey = field,
                                              responses_quick = Status,
                                              # duration = duration,
                                              gender = v_01,
                                              year_of_birth = as.factor(v_02),
                                              education = v_03,
                                              pol_interest_fed =  L38a,
                                              pol_interest_land = L38b,
                                              satisf_democracy_fed = A02b_a,
                                              vote_intention_land = L02,
                                              scale_satisf_gov_land =  L09,
                                              interest_campaign = L30,
                                              internet_use_political = A34_wk,
                                              vote_party_land = L03b,
                                              vote_party_land_postal = L03_brief_b,
                                              vote_party_land_hyp = L04c,
                                              vote_party_sure = L05a,
                                              importance_election_result = L16,
                                              scale_cdu = L07a,
                                              scale_spd = L07b,
                                              scale_fdp = L07c,
                                              scale_greens = L07e,
                                              scale_left = L07d,
                                              wish_coa_cdu = L67a, 
                                              wish_coa_spd = L67b,
                                              wish_coa_greens = L67e,
                                              wish_coa_left = L67d,
                                              wish_coa_fdp = L67c,
                                              wish_coa_other = L67f,
                                              
                                              prob_coa_cdu_spd = L27a, 
                                              prob_coa_cdu_fdp = L27b,
                                              prob_coa_spd_greens = L27c,
                                              prob_coa_spd_fdp_greens = L27d,
                                              prob_coa_cdu_fdp_greens = L27e,
                                              prob_coa_spd_left_greens = L27f,
                                              prob_coa_cdu_greens = L27g,
                                              
                                              predict_gov_cdu = L68a,
                                              predict_gov_spd = L68b,
                                              predict_gov_fdp = L68c,
                                              predict_gov_left = L68d,
                                              predict_gov_greens = L68e,
                                              predict_gov_other = L68f,
                                              
                                              lr_self = A33,
                                              lr_cdu = A32a_a,
                                              lr_spd = A32c_a,
                                              lr_fdp = A32d_a,
                                              lr_left = A32e_a,
                                              lr_greens = A32f_a,
                                              party_id = A36,
                                              party_id_2 = A36c,
                                              party_id_strength = A37,
                                              job_type = A53,
                                              job_type_past = A53a,
                                              income = A47_a) 





# data_2011_hamburg (ZA5331_fb") has no data on predictions 
# because survey conducted AFTER election day

data_2012_schlewsig_holstein_raw <- foreign::read.dta("data_gesis/ZA5332_v1-0-0.dta") %>% 
    as_factor()

data_2012_schlewsig_holstein <- transmute(data_2012_schlewsig_holstein_raw,
                                          study_id = "ZA5332_v1-0-0",
                                          election_id = "Schleswig-Holstein 2012",
                                          weight_microc = as.numeric(wei_mzz),
                                          weight_online = as.numeric(wei_onz),
                                          # duration_survey = field,
                                          responses_quick_num = as.numeric(speederindex),
                                          # duration = duration,
                                          gender = l1,
                                          year_of_birth = as.factor(l2),
                                          education = l3,
                                          pol_interest_fed =  l6a,
                                          pol_interest_land = l6b,
                                          satisf_democracy_fed = l7b,
                                          vote_intention_land = l8,
                                          scale_satisf_gov_land =  l102,
                                          interest_campaign = l33,
                                          internet_use_political = l67,
                                          vote_party_land = l71ba,
                                          vote_party_land_postal = l72ba,
                                          vote_party_land_hyp = l73ba,
                                          vote_party_sure = l76,
                                          importance_election_result = l80,
                                          scale_cdu = l86a,
                                          scale_spd = l86b,
                                          scale_fdp = l86c,
                                          scale_greens = l86e,
                                          scale_left = l86d,
                                          wish_coa_cdu = l105a, 
                                          wish_coa_spd = l105c,
                                          wish_coa_greens = l105f,
                                          wish_coa_left = l105e,
                                          wish_coa_fdp = l105d,
                                          wish_coa_ssw = l105h, ### add this to the options
                                          wish_coa_other = l105i,
                                          
                                          prob_coa_cdu_spd = l106a, 
                                          prob_coa_cdu_fdp = l106b,
                                          prob_coa_spd_greens = l106c,
                                          prob_coa_spd_fdp_greens = l106d,
                                          prob_coa_cdu_fdp_greens = l106e,
                                          prob_coa_spd_left_greens = l106f,
                                          prob_coa_cdu_greens = l106g,
                                          ## difference in dataset from now on!!!
                                          predict_coa_cdu_spd = l107a, 
                                          predict_coa_spd_greens = l107d,
                                          predict_coa_spd_fdp_greens = l107b,
                                          predict_coa_cdu_fdp_greens = l107c,
                                          predict_coa_spd_left_greens = l107e,
                                          lr_self = l165,
                                          lr_cdu = l164a,
                                          lr_spd = l164b,
                                          lr_fdp = l164c,
                                          lr_left = l164d,
                                          lr_greens = l164e, 
                                          lr_ssw = l164f, ## add this as well to condition
                                          party_id = l174a,
                                          party_id_2 = l174b,
                                          party_id_strength = l175,
                                          job_type = l186,
                                          job_type_past = l187,
                                          income = l200) 


data_2012_nrw_raw <- foreign::read.dta("data_gesis/ZA5333_v1-1-0.dta") %>% 
    as_factor()

data_2012_nrw <- transmute(data_2012_nrw_raw,
                           study_id = "ZA5333_v1-1-0",
                           election_id = "Nordrhein-Westfalen 2012",
                           weight_microc = as.numeric(wei_mzz),
                           weight_online = as.numeric(wei_onz),
                           duration_survey = field,
                           responses_quick_num = as.numeric(speederindex),
                           # duration = duration,
                           gender = l1,
                           year_of_birth = as.factor(l2),
                           education = l3,
                           pol_interest_fed =  l6a,
                           pol_interest_land = l6b,
                           satisf_democracy_fed = l7b,
                           vote_intention_land = l8,
                           scale_satisf_gov_land =  l102,
                           interest_campaign = l33,
                           internet_use_political = l67,
                           vote_party_land = l71ba,
                           vote_party_land_postal = l72ba,
                           vote_party_land_hyp = l73ba,
                           vote_party_sure = l76,
                           importance_election_result = l80,
                           scale_cdu = l86a,
                           scale_spd = l86b,
                           scale_fdp = l86c,
                           scale_greens = l86e,
                           scale_left = l86d,
                           wish_coa_cdu = l105a, 
                           wish_coa_spd = l105c,
                           wish_coa_greens = l105f,
                           wish_coa_left = l105e,
                           wish_coa_fdp = l105d,
                           wish_coa_pirates = l105g,
                           wish_coa_other = l105i,
                           
                           prob_coa_cdu_spd = l106a, 
                           prob_coa_cdu_fdp = l106b,
                           prob_coa_spd_greens = l106c,
                           prob_coa_spd_fdp_greens = l106d,
                           prob_coa_cdu_fdp_greens = l106e,
                           prob_coa_spd_left_greens = l106f,
                           prob_coa_cdu_greens = l106g,
                           ## difference in dataset from now on!!!
                           
                           predict_coa_cdu_spd = l107a,
                           #predict_coa_cdu_fpd,
                           predict_coa_spd_greens = l107d,
                           predict_coa_spd_fdp_greens = l107b,
                           predict_coa_cdu_fdp_greens = l107c,
                           predict_coa_spd_left_greens = l107e,
                           #predict_coa_cdu_greens,
                           lr_self = l165,
                           lr_cdu = l164a,
                           lr_spd = l164b,
                           lr_fdp = l164c,
                           lr_left = l164d,
                           lr_greens = l164e, 
                           lr_pirates = l164f,
                           party_id = l174a,
                           party_id_2 = l174b,
                           party_id_strength = l175,
                           job_type = l186,
                           job_type_past = l187,
                           income = l200)


data_2013_niedersachsen_raw <- foreign::read.dta("data_gesis/ZA5735_v2-0-0.dta") %>% 
    as_factor()

data_2013_niedersachsen <- transmute(data_2013_niedersachsen_raw,
                                     study_id = "ZA5735_v2-0-0",
                                     election_id = "Niedersachsen 2013",
                                     weight_microc = as.numeric(wei_mzz),
                                     weight_online = as.numeric(wei_onz),
                                     #duration_survey = field,
                                     responses_quick_num = as.numeric(speederindex),
                                     #duration = duration,
                                     gender = l1,
                                     year_of_birth = as.factor(l2),
                                     education = l3,
                                     pol_interest_fed =  l6a,
                                     pol_interest_land = l6b,
                                     satisf_democracy_fed = l7b,
                                     vote_intention_land = l8,
                                     scale_satisf_gov_land =  l102,
                                     interest_campaign = l33,
                                     internet_use_political = as.factor(l67),
                                     vote_party_land = l71ba,
                                     vote_party_land_postal = l72ba,
                                     vote_party_land_hyp = l73ba,
                                     vote_party_sure = l76,
                                     importance_election_result = l80,
                                     scale_cdu = l86a,
                                     scale_spd = l86b,
                                     scale_fdp = l86c,
                                     scale_greens = l86e,
                                     scale_left = l86d,
                                     wish_coa_cdu = l105a, 
                                     wish_coa_spd = l105c,
                                     wish_coa_greens = l105f,
                                     wish_coa_left = l105e,
                                     wish_coa_fdp = l105d,
                                     wish_coa_pirates = l105g,
                                     wish_coa_other = l105h,
                                     
                                     prob_coa_cdu_spd = l106a, 
                                     prob_coa_cdu_fdp = l106b,
                                     prob_coa_spd_greens = l106c,
                                     prob_coa_spd_fdp_greens = l106d,
                                     prob_coa_cdu_fdp_greens = l106e,
                                     prob_coa_spd_left_greens = l106f,
                                     prob_coa_cdu_greens = l106g,
                                     ## difference in dataset from now on!!!
                                     
                                     predict_coa_cdu_spd = l107a, 
                                     predict_coa_spd_greens = l107e,
                                     predict_coa_spd_fdp_greens = l107b,
                                     predict_coa_cdu_fdp_greens = l107c,
                                     predict_coa_spd_left_greens = l107d,
                                     predict_coa_cdu_greens = l107f,
                                     lr_self = l165,
                                     lr_cdu = l164a,
                                     lr_spd = l164b,
                                     lr_fdp = l164c,
                                     lr_left = l164d,
                                     lr_greens = l164e, 
                                     lr_pirates = l164f,
                                     party_id = l174a,
                                     party_id_2 = l174b,
                                     party_id_strength = l175,
                                     job_type = l186,
                                     job_type_past = l187,
                                     income = l200)

data_2013_bayern_raw <- foreign::read.dta("data_gesis/ZA5736_v2-0-0.dta") %>% 
    as_factor()

data_2013_bayern <- transmute(data_2013_bayern_raw,
                              study_id = "ZA5736_v2-0-0",
                              election_id = "Bayern 2013",
                              weight_microc = as.numeric(wei_mzzob),
                              weight_online = as.numeric(wei_onz),
                              #duration_survey = field,
                              responses_quick_num = as.numeric(speederindex),
                              #duration = duration,
                              gender = l1,
                              year_of_birth = as.factor(l2),
                              education = l3,
                              pol_interest_fed =  l6a,
                              pol_interest_land = l6b,
                              satisf_democracy_fed = l7b,
                              vote_intention_land = l70,
                              scale_satisf_gov_land =  l102,
                              interest_campaign = l33,
                              internet_use_political = as.factor(l67),
                              vote_party_land = l71ba,
                              vote_party_land_postal = l72ba,
                              vote_party_land_hyp = l73ba,
                              vote_party_sure = l76,
                              importance_election_result = l80,
                              scale_cdu = l86a,
                              scale_spd = l86b,
                              scale_fdp = l86c,
                              scale_greens = l86e,
                              scale_fw = l86f,
                              scale_pirates = l86g,
                              scale_left = l86d,
                              
                              ## not included!
                              # prob_coa_cdu_spd = l106a, 
                              # prob_coa_cdu = l106b,
                              # prob_coa_cdu_fw = l106f,
                              # prob_coa_spd_greens_fw = l106h,
                              
                              # prob_coa_cdu_fpd = l106d,
                              # prob_coa_spd_greens = l106c,
                              # prob_coa_spd_fdp_greens = l106j,
                              # prob_coa_cdu_fdp_greens = l106i,
                              # prob_coa_spd_left_greens = l106e,
                              # prob_coa_cdu_greens = l106h,
                              
                              predict_coa_cdu_spd = l107a, 
                              predict_coa_cdu = l107b,
                              predict_coa_cdu_fw = l107f,
                              predict_coa_spd_greens_fw = l107h,
                              
                              predict_coa_cdu_fdp = l107d,
                              predict_coa_spd_greens = l107c,
                              predict_coa_spd_fdp_greens = l107j,
                              predict_coa_cdu_fdp_greens = l107i,
                              predict_coa_spd_left_greens = l107e,
                              predict_coa_cdu_greens = l107h,
                              
                              lr_self = l165,
                              lr_cdu = l164a,
                              lr_spd = l164b,
                              lr_fdp = l164c,
                              lr_left = l164d,
                              lr_greens = l164e, 
                              lr_fw = l164f,
                              lr_pirates = l164g,
                              party_id = l174a,
                              party_id_2 = l174b,
                              party_id_strength = l175,
                              job_type = l186,
                              job_type_past = l187,
                              income = l200)


data_2013_hessen_raw <- foreign::read.dta("data_gesis/ZA5737_v3-0-0.dta") %>% 
    as_factor()

data_2013_hessen <- transmute(data_2013_hessen_raw,
                              study_id = "ZA5737_v3-0-0",
                              election_id = "Hessen 2013",
                              weight_microc = as.numeric(wei_mzz),
                              weight_online = as.numeric(wei_onz),
                              #duration_survey = field,
                              responses_quick_num = as.numeric(speederindex),
                              #duration = duration,
                              gender = l1,
                              year_of_birth = as.factor(l2),
                              education = l3,
                              pol_interest_fed =  l6a,
                              pol_interest_land = l6b,
                              satisf_democracy_fed = l7b,
                              vote_intention_land = l70,
                              scale_satisf_gov_land =  l102,
                              interest_campaign = l33,
                              internet_use_political = as.factor(l67),
                              vote_party_land = l71ba,
                              vote_party_land_postal = l72ba,
                              vote_party_land_hyp = l73ba,
                              vote_party_sure = l76,
                              importance_election_result = l80,
                              scale_cdu = as.factor(l86a),
                              scale_spd = as.factor(l86b),
                              scale_fdp = as.factor(l86c),
                              scale_greens = as.factor(l86e),
                              scale_pirates = as.factor(l86f),
                              scale_left = as.factor(l86d),
                              scale_afd = as.factor(l86g),
                              
                              ## not included!
                              # prob_coa_cdu_spd = l106a, 
                              # prob_coa_cdu = l106b,
                              # prob_coa_cdu_fw = l106f,
                              # prob_coa_spd_greens_fw = l106h,
                              
                              # prob_coa_cdu_fpd = l106d,
                              # prob_coa_spd_greens = l106c,
                              # prob_coa_spd_fdp_greens = l106j,
                              # prob_coa_cdu_fdp_greens = l106i,
                              # prob_coa_spd_left_greens = l106e,
                              # prob_coa_cdu_greens = l106h,
                              
                              wish_coa_cdu_spd = l108a,
                              wish_coa_spd_greens = l108b,
                              wish_coa_cdu_fdp = l108c,
                              wish_coa_spd_left_greens = l108d,
                              wish_coa_cdu_greens = l108e,
                              wish_coa_spd_fdp_greens = l108f,
                              wish_coa_cdu_fdp_greens = l108g,
                              
                              predict_coa_cdu_spd = l107a, 
                              predict_coa_spd_greens = l107b,
                              predict_coa_cdu_fdp = l107c,
                              predict_coa_spd_left_greens = l107d,
                              predict_coa_cdu_greens = l107e,
                              predict_coa_spd_fdp_greens = l107f,
                              predict_coa_cdu_fdp_greens = l107g,
                              
                              lr_self = l165,
                              lr_cdu = l164a,
                              lr_spd = l164b,
                              lr_fdp = l164c,
                              lr_left = l164d,
                              lr_greens = l164e, 
                              lr_pirates = l164g,
                              lr_afd = l164h,
                              party_id = l174a,
                              party_id_2 = l174b,
                              party_id_strength = l175,
                              job_type = l186,
                              job_type_past = l187,
                              income = l200) 


data_2014_sachsen_raw <- foreign::read.dta("data_gesis/ZA5738_v2-0-0.dta") %>% 
    as_factor()

data_2014_sachsen <- transmute(data_2014_sachsen_raw,
                               study_id = "ZA5738_v2-0-0",
                               election_id = "Sachsen 2014",
                               weight_microc = as.numeric(wei_mzz),
                               weight_online = as.numeric(wei_onz),
                               #duration_survey = field,
                               responses_quick_num = as.numeric(speederindex),
                               #duration = duration,
                               gender = l1,
                               year_of_birth = as.factor(l2),
                               education = l3,
                               pol_interest_fed =  l6a,
                               pol_interest_land = l6b,
                               satisf_democracy_fed = l7b,
                               vote_intention_land = l70,
                               scale_satisf_gov_land =  l102,
                               interest_campaign = l33,
                               internet_use_political = as.factor(l67),
                               vote_party_land = l71ba,
                               vote_party_land_postal = l72ba,
                               vote_party_land_hyp = l73ba,
                               vote_party_sure = l76,
                               importance_election_result = l80,
                               scale_cdu = as.factor(l86a),
                               scale_spd = as.factor(l86b),
                               scale_fdp = as.factor(l86e),
                               scale_greens = as.factor(l86d),
                               scale_npd = as.factor(l86f),
                               scale_afd = as.factor(l86g),
                               scale_left = as.factor(l86c),
                               
                               ## not included!
                               # prob_coa_cdu_spd = l106a, 
                               # prob_coa_cdu = l106b,
                               # prob_coa_cdu_fw = l106f,
                               # prob_coa_spd_greens_fw = l106h,
                               
                               # prob_coa_cdu_fpd = l106d,
                               # prob_coa_spd_greens = l106c,
                               # prob_coa_spd_fdp_greens = l106j,
                               # prob_coa_cdu_fdp_greens = l106i,
                               # prob_coa_spd_left_greens = l106e,
                               # prob_coa_cdu_greens = l106h,
                               
                               wish_coa_cdu = l108a,
                               wish_coa_cdu_spd = l108b,
                               wish_coa_cdu_fdp = l108c,
                               wish_coa_spd_left = l108d,
                               wish_coa_spd_greens = l108e,
                               wish_coa_spd_left_greens = l108f,
                               wish_coa_cdu_greens = l108g,
                               wish_coa_spd_fdp_greens = l108h,
                               wish_coa_cdu_fdp_greens = l108i,
                               wish_coa_cdu_afd = l108j,
                               
                               predict_coa_cdu = l107a,
                               predict_coa_cdu_spd = l107b,
                               predict_coa_cdu_fdp = l107c,
                               predict_coa_spd_left = l107d,
                               predict_coa_spd_greens = l107e,
                               predict_coa_spd_left_greens = l107f,
                               predict_coa_cdu_greens = l107g,
                               predict_coa_spd_fdp_greens = l107h,
                               predict_coa_cdu_fdp_greens = l107i,
                               predict_coa_cdu_afd = l107j,
                               
                               
                               lr_self = l165,
                               lr_cdu = l164a,
                               lr_spd = l164b,
                               lr_fdp = l164c,
                               lr_left = l164d,
                               lr_greens = l164e, 
                               lr_npd = l164f,
                               lr_afd = l164h,
                               party_id = l174a,
                               party_id_2 = l174b,
                               party_id_strength = l175,
                               job_type = l186,
                               job_type_past = l187,
                               income = l200) 



data_2014_brandenburg_raw <- foreign::read.dta("data_gesis/ZA5739_v2-0-0.dta") %>% 
    as_factor()

data_2014_brandenburg <- transmute(data_2014_brandenburg_raw,
                                   study_id = "ZA5739_v2-0-0",
                                   election_id = "Brandenburg 2014",
                                   weight_microc = as.numeric(wei_mzz),
                                   weight_online = as.numeric(wei_onz),
                                   #duration_survey = field,
                                   responses_quick_num = as.numeric(speederindex),
                                   #duration = duration,
                                   gender = l1,
                                   year_of_birth = as.factor(l2),
                                   education = l3,
                                   pol_interest_fed =  l6a,
                                   pol_interest_land = l6b,
                                   satisf_democracy_fed = l7b,
                                   vote_intention_land = l70,
                                   scale_satisf_gov_land =  l102,
                                   interest_campaign = l33,
                                   internet_use_political = as.factor(l67),
                                   vote_party_land = l71ba,
                                   vote_party_land_postal = l72ba,
                                   vote_party_land_hyp = l73ba,
                                   vote_party_sure = l76,
                                   importance_election_result = l80,
                                   scale_cdu = as.factor(l86a),
                                   scale_spd = as.factor(l86b),
                                   scale_fdp = as.factor(l86e),
                                   scale_greens = as.factor(l86d),
                                   scale_afd = as.factor(l86g),
                                   scale_left = as.factor(l86c),
                                   
                                   ## not included!
                                   # prob_coa_cdu_spd = l106a, 
                                   # prob_coa_cdu = l106b,
                                   # prob_coa_cdu_fw = l106f,
                                   # prob_coa_spd_greens_fw = l106h,
                                   
                                   # prob_coa_cdu_fpd = l106d,
                                   # prob_coa_spd_greens = l106c,
                                   # prob_coa_spd_fdp_greens = l106j,
                                   # prob_coa_cdu_fdp_greens = l106i,
                                   # prob_coa_spd_left_greens = l106e,
                                   # prob_coa_cdu_greens = l106h,
                                   
                                   wish_coa_cdu_spd = l108a,
                                   wish_coa_cdu_fdp = l108c,
                                   wish_coa_spd_left = l108b,
                                   wish_coa_spd_greens = l108d,
                                   wish_coa_cdu_greens = l108e,
                                   wish_coa_spd_fdp_greens = l108f,
                                   wish_coa_cdu_fdp_greens = l108g,
                                   wish_coa_cdu_afd = l108h,
                                   
                                   predict_coa_cdu_spd = l107a,
                                   predict_coa_cdu_fdp = l107c,
                                   predict_coa_spd_left = l107b,
                                   predict_coa_spd_greens = l107d,
                                   predict_coa_cdu_greens = l107e,
                                   predict_coa_spd_fdp_greens = l107f,
                                   predict_coa_cdu_fdp_greens = l107g,
                                   predict_coa_cdu_afd = l107h,
                                   
                                   lr_self = l165,
                                   lr_cdu = l164a,
                                   lr_spd = l164b,
                                   lr_fdp = l164c,
                                   lr_left = l164d,
                                   lr_greens = l164e, 
                                   lr_afd = l164h,
                                   party_id = l174a,
                                   party_id_2 = l174b,
                                   party_id_strength = l175,
                                   job_type = l186,
                                   job_type_past = l187,
                                   income = l200) 


data_2014_thueringen_raw <- foreign::read.dta("data_gesis/ZA5740_v2-0-0.dta") %>% 
    as_factor()

data_2014_thueringen <- transmute(data_2014_thueringen_raw,
                                  study_id = "ZA5740_v2-0-0",
                                  election_id = "Thüringen 2014",
                                  weight_microc = as.numeric(wei_mzz),
                                  weight_online = as.numeric(wei_onz),
                                  #duration_survey = field,
                                  responses_quick_num = as.numeric(speederindex),
                                  #duration = duration,
                                  gender = l1,
                                  year_of_birth = as.factor(l2),
                                  education = l3,
                                  pol_interest_fed =  l6a,
                                  pol_interest_land = l6b,
                                  satisf_democracy_fed = l7b,
                                  vote_intention_land = l70,
                                  scale_satisf_gov_land =  l102,
                                  interest_campaign = l33,
                                  internet_use_political = as.factor(l67),
                                  vote_party_land = l71ba,
                                  vote_party_land_postal = l72ba,
                                  vote_party_land_hyp = l73ba,
                                  vote_party_sure = l76,
                                  importance_election_result = l80,
                                  scale_cdu = as.factor(l86a),
                                  scale_spd = as.factor(l86b),
                                  scale_fdp = as.factor(l86e),
                                  scale_greens = as.factor(l86d),
                                  scale_afd = as.factor(l86g),
                                  scale_left = as.factor(l86c),
                                  
                                  ## not included!
                                  # prob_coa_cdu_spd = l106a, 
                                  # prob_coa_cdu = l106b,
                                  # prob_coa_cdu_fw = l106f,
                                  # prob_coa_spd_greens_fw = l106h,
                                  
                                  # prob_coa_cdu_fpd = l106d,
                                  # prob_coa_spd_greens = l106c,
                                  # prob_coa_spd_fdp_greens = l106j,
                                  # prob_coa_cdu_fdp_greens = l106i,
                                  # prob_coa_spd_left_greens = l106e,
                                  # prob_coa_cdu_greens = l106h,
                                  
                                  wish_coa_cdu_spd = l108a,
                                  wish_coa_cdu_fdp = l108c,
                                  wish_coa_spd_left = l108b,
                                  wish_coa_spd_greens = l108d,
                                  wish_coa_spd_left_greens = l108e,
                                  wish_coa_cdu_greens = l108f,
                                  wish_coa_spd_fdp_greens = l108g,
                                  wish_coa_cdu_fdp_greens = l108h,
                                  
                                  predict_coa_cdu_spd = l107a,
                                  predict_coa_cdu_fdp = l107c,
                                  predict_coa_spd_left = l107b,
                                  predict_coa_spd_greens = l107d,
                                  predict_coa_spd_left_greens = l107e,
                                  predict_coa_cdu_greens = l107f,
                                  predict_coa_spd_fdp_greens = l107g,
                                  predict_coa_cdu_fdp_greens = l107h,
                                  
                                  lr_self = l165,
                                  lr_cdu = l164a,
                                  lr_spd = l164b,
                                  lr_fdp = l164c,
                                  lr_left = l164d,
                                  lr_greens = l164e, 
                                  lr_afd = l164h,
                                  party_id = l174a,
                                  party_id_2 = l174b,
                                  party_id_strength = l175,
                                  job_type = l186,
                                  job_type_past = l187,
                                  income = l200) 


data_2016_baden_wuerttemberg_raw <- foreign::read.dta("data_gesis/ZA5741_v1-0-0.dta") %>% 
    as_factor()

data_2016_baden_wuerttemberg <- transmute(data_2016_baden_wuerttemberg_raw,
                                          study_id = "ZA5741_v1-0-0",
                                          election_id = "Baden-Württemberg 2016",
                                          weight_microc = as.numeric(wei_mzz),
                                          weight_online = as.numeric(wei_onz),
                                          #duration_survey = field,
                                          responses_quick_num = as.numeric(speederindex),
                                          #duration = duration,
                                          gender = l1,
                                          year_of_birth = as.factor(l2),
                                          education = l3,
                                          pol_interest_fed =  l6a,
                                          pol_interest_land = l6b,
                                          satisf_democracy_fed = l7b,
                                          vote_intention_land = l70,
                                          scale_satisf_gov_land =  l102,
                                          interest_campaign = l33,
                                          internet_use_political = as.factor(l67),
                                          vote_party_land = l470a,
                                          vote_party_land_postal = l471a,
                                          vote_party_land_hyp = l472a,
                                          vote_party_sure = l76,
                                          importance_election_result = l80,
                                          scale_cdu = as.factor(l86a),
                                          scale_spd = as.factor(l86b),
                                          scale_fdp = as.factor(l86e),
                                          scale_greens = as.factor(l86d),
                                          scale_afd = as.factor(l86g),
                                          scale_left = as.factor(l86c),
                                          
                                          ## not included!
                                          # prob_coa_cdu_spd = l106a, 
                                          # prob_coa_cdu = l106b,
                                          # prob_coa_cdu_fw = l106f,
                                          # prob_coa_spd_greens_fw = l106h,
                                          
                                          # prob_coa_cdu_fpd = l106d,
                                          # prob_coa_spd_greens = l106c,
                                          # prob_coa_spd_fdp_greens = l106j,
                                          # prob_coa_cdu_fdp_greens = l106i,
                                          # prob_coa_spd_left_greens = l106e,
                                          # prob_coa_cdu_greens = l106h,
                                          
                                          wish_coa_cdu_spd = l108b,
                                          wish_coa_cdu_fdp = l108f,
                                          wish_coa_spd_greens = l108a,
                                          wish_coa_spd_left_greens = l108g,
                                          wish_coa_cdu_greens = l108c,
                                          wish_coa_spd_fdp_greens = l108e,
                                          wish_coa_cdu_fdp_greens = l108d,
                                          wish_coa_cdu_afd = l108h,
                                          
                                          predict_coa_cdu_spd = l107b,
                                          predict_coa_cdu_fdp = l107f,
                                          predict_coa_spd_greens = l107a,
                                          predict_coa_spd_left_greens = l107g,
                                          predict_coa_cdu_greens = l107c,
                                          predict_coa_spd_fdp_greens = l107e,
                                          predict_coa_cdu_fdp_greens = l107d,
                                          predict_coa_cdu_afd = l107h,
                                          
                                          lr_self = l165,
                                          lr_cdu = l164a,
                                          lr_spd = l164b,
                                          lr_fdp = l164c,
                                          lr_left = l164d,
                                          lr_greens = l164e, 
                                          lr_afd = l164h,
                                          party_id = l174a,
                                          party_id_2 = l174b,
                                          party_id_strength = l175,
                                          job_type = l186,
                                          job_type_past = l187,
                                          income = l200) 


data_2016_rheinland_pfalz_raw <- foreign::read.dta("data_gesis/ZA5743_v1-0-0.dta") %>% 
    as_factor()

data_2016_rheinland_pfalz <- transmute(data_2016_rheinland_pfalz_raw,
                                       study_id = "ZA5743_v1-0-0",
                                       election_id = "Rheinland-Pfalz 2016",
                                       weight_microc = as.numeric(wei_mzz),
                                       weight_online = as.numeric(wei_onz),
                                       #duration_survey = field,
                                       responses_quick_num = as.numeric(speederindex),
                                       #duration = duration,
                                       gender = l1,
                                       year_of_birth = as.factor(l2),
                                       education = l3,
                                       pol_interest_fed =  l6a,
                                       pol_interest_land = l6b,
                                       satisf_democracy_fed = l7b,
                                       vote_intention_land = l70,
                                       scale_satisf_gov_land =  l102,
                                       interest_campaign = l33,
                                       internet_use_political = as.factor(l67),
                                       vote_party_land = l71ba,
                                       vote_party_land_postal = l72ba,
                                       vote_party_land_hyp = l73ba,
                                       vote_party_sure = l76,
                                       importance_election_result = l80,
                                       scale_cdu = as.factor(l86a),
                                       scale_spd = as.factor(l86b),
                                       scale_fdp = as.factor(l86e),
                                       scale_greens = as.factor(l86d),
                                       scale_afd = as.factor(l86g),
                                       scale_left = as.factor(l86c),
                                       
                                       ## not included!
                                       # prob_coa_cdu_spd = l106a, 
                                       # prob_coa_cdu = l106b,
                                       # prob_coa_cdu_fw = l106f,
                                       # prob_coa_spd_greens_fw = l106h,
                                       
                                       # prob_coa_cdu_fpd = l106d,
                                       # prob_coa_spd_greens = l106c,
                                       # prob_coa_spd_fdp_greens = l106j,
                                       # prob_coa_cdu_fdp_greens = l106i,
                                       # prob_coa_spd_left_greens = l106e,
                                       # prob_coa_cdu_greens = l106h,
                                       
                                       wish_coa_cdu_spd = l108a,
                                       wish_coa_cdu_fdp = l108b,
                                       wish_coa_spd_greens = l108c,
                                       wish_coa_spd_left_greens = l108g,
                                       wish_coa_spd_fdp_greens = l108e,
                                       wish_coa_cdu_fdp_greens = l108d,
                                       wish_coa_cdu_greens = l108f,
                                       wish_coa_cdu_afd = l108h,
                                       
                                       predict_coa_cdu_spd = l107a,
                                       predict_coa_cdu_fdp = l107b,
                                       predict_coa_spd_greens = l107c,
                                       predict_coa_spd_left_greens = l107g,
                                       predict_coa_spd_fdp_greens = l107e,
                                       predict_coa_cdu_fdp_greens = l107d,
                                       predict_coa_cdu_greens = l107f,
                                       predict_coa_cdu_afd = l107h,
                                       
                                       lr_self = l165,
                                       lr_cdu = l164a,
                                       lr_spd = l164b,
                                       lr_fdp = l164c,
                                       lr_left = l164d,
                                       lr_greens = l164e, 
                                       lr_afd = l164h,
                                       party_id = l174a,
                                       party_id_2 = l174b,
                                       party_id_strength = l175,
                                       job_type = l186,
                                       job_type_past = l187,
                                       income = l200) 



data_2016_mecklenburg_vorpommern_raw <- foreign::read.dta("data_gesis/ZA5744_v1-0-0.dta") %>% 
    as_factor()
summary(data_2016_mecklenburg_vorpommern_raw$l107a)


data_2016_mecklenburg_vorpommern <- transmute(data_2016_mecklenburg_vorpommern_raw,
                                              study_id = "ZA5744_v1-0-0",
                                              election_id = "Mecklenburg-Vorpommern 2016",
                                              weight_microc = as.numeric(wei_mzz),
                                              weight_online = as.numeric(wei_onz),
                                              responses_quick_num = as.numeric(speederindex),
                                              gender = l1,
                                              year_of_birth = as.factor(l2),
                                              education = l3,
                                              pol_interest_fed =  l6a,
                                              pol_interest_land = l6b,
                                              satisf_democracy_fed = l7b,
                                              vote_intention_land = l70,
                                              scale_satisf_gov_land =  l102,
                                              interest_campaign = l33,
                                              internet_use_political = as.factor(l67),
                                              vote_party_land = l71ba,
                                              vote_party_land_postal = l72ba,
                                              vote_party_land_hyp = l73ba,
                                              vote_party_sure = l76,
                                              importance_election_result = l80,
                                              scale_cdu = as.factor(l86a),
                                              scale_spd = as.factor(l86b),
                                              scale_fdp = as.factor(l86e),
                                              scale_greens = as.factor(l86d),
                                              scale_afd = as.factor(l86g),
                                              scale_left = as.factor(l86c),
                                              
                                              ## not included!
                                              # prob_coa_cdu_spd = l106a, 
                                              # prob_coa_cdu = l106b,
                                              # prob_coa_cdu_fw = l106f,
                                              # prob_coa_spd_greens_fw = l106h,
                                              
                                              # prob_coa_cdu_fpd = l106d,
                                              # prob_coa_spd_greens = l106c,
                                              # prob_coa_spd_fdp_greens = l106j,
                                              # prob_coa_cdu_fdp_greens = l106i,
                                              # prob_coa_spd_left_greens = l106e,
                                              # prob_coa_cdu_greens = l106h,
                                              
                                              wish_coa_cdu_spd = l108a,
                                              wish_coa_cdu_fdp = l108h,
                                              wish_coa_spd_greens = l108e,
                                              wish_coa_spd_left_greens = l108b,
                                              wish_coa_spd_left = l108c,
                                              wish_coa_spd_fdp_greens = l108d,
                                              wish_coa_cdu_fdp_greens = l108f,
                                              wish_coa_cdu_greens = l108g,
                                              wish_coa_cdu_afd = l108i,
                                              
                                              predict_coa_cdu_spd = l107a,
                                              predict_coa_cdu_fdp = l107h,
                                              predict_coa_spd_greens = l107e,
                                              predict_coa_spd_left_greens = l107b,
                                              predict_coa_spd_left = l107c,
                                              predict_coa_spd_fdp_greens = l107d,
                                              predict_coa_cdu_fdp_greens = l107f,
                                              predict_coa_cdu_greens = l107g,
                                              predict_coa_cdu_afd = l107i,
                                              
                                              lr_self = l165,
                                              lr_cdu = l164a,
                                              lr_spd = l164b,
                                              lr_fdp = l164c,
                                              lr_left = l164d,
                                              lr_greens = l164e, 
                                              lr_npd = l164f,
                                              lr_afd = l164h,
                                              party_id = l174a,
                                              party_id_2 = l174b,
                                              party_id_strength = l175,
                                              job_type = l186,
                                              job_type_past = l187,
                                              income = l200) 



data_2017_schleswig_holstein_raw <- foreign::read.dta("data_gesis/ZA6819_v1-0-0.dta") %>% 
    as_factor()

data_2017_schleswig_holstein <- transmute(data_2017_schleswig_holstein_raw,
                                          study_id = "ZA6819_v1-0-0",
                                          election_id = "Schleswig-Holstein 2017",
                                          weight_microc = as.numeric(wei_mzz),
                                          weight_online = as.numeric(wei_onz),
                                          #duration_survey = field,
                                          responses_quick_num = as.numeric(speederindex),
                                          #duration = duration,
                                          gender = l1,
                                          year_of_birth = as.factor(l2),
                                          education = l3,
                                          pol_interest_fed =  l6a,
                                          pol_interest_land = l6b,
                                          satisf_democracy_fed = l7b,
                                          vote_intention_land = l70,
                                          scale_satisf_gov_land =  l102,
                                          interest_campaign = l33,
                                          internet_use_political = as.factor(l67),
                                          vote_party_land = l71ba,
                                          vote_party_land_postal = l72ba,
                                          vote_party_land_hyp = l73ba,
                                          vote_party_sure = l76,
                                          importance_election_result = l80,
                                          scale_cdu = as.factor(l86a),
                                          scale_spd = as.factor(l86b),
                                          scale_fdp = as.factor(l86e),
                                          scale_greens = as.factor(l86d),
                                          scale_ssw = as.factor(l86f),
                                          scale_pirates = as.factor(l86h),
                                          scale_afd = as.factor(l86g),
                                          scale_left = as.factor(l86c),
                                          
                                          ## not included!
                                          # prob_coa_cdu_spd = l106a, 
                                          # prob_coa_cdu = l106b,
                                          # prob_coa_cdu_fw = l106f,
                                          # prob_coa_spd_greens_fw = l106h,
                                          
                                          # prob_coa_cdu_fpd = l106d,
                                          # prob_coa_spd_greens = l106c,
                                          # prob_coa_spd_fdp_greens = l106j,
                                          # prob_coa_cdu_fdp_greens = l106i,
                                          # prob_coa_spd_left_greens = l106e,
                                          # prob_coa_cdu_greens = l106h,
                                          
                                          wish_coa_cdu_spd = l108a,
                                          wish_coa_spd_greens = l108e,
                                          wish_coa_spd_left_greens = l108f,
                                          wish_coa_cdu_fdp = l108g,
                                          wish_coa_spd_fdp = l108h,
                                          wish_coa_spd_fdp_greens = l108c,
                                          wish_coa_cdu_fdp_greens = l108b,
                                          wish_coa_cdu_greens = l108d,
                                          wish_coa_cdu_afd = l108i,
                                          
                                          predict_coa_cdu_spd = l107a,
                                          predict_coa_spd_greens = l107e,
                                          predict_coa_spd_left_greens = l107f,
                                          predict_coa_cdu_fdp = l107g,
                                          predict_coa_spd_fdp = l107h,
                                          predict_coa_spd_fdp_greens = l107c,
                                          predict_coa_cdu_fdp_greens = l107b,
                                          predict_coa_cdu_greens = l107d,
                                          #wish_coa_cdu_afd = l108i,
                                          
                                          lr_self = l165,
                                          lr_cdu = l164a,
                                          lr_spd = l164b,
                                          lr_fdp = l164c,
                                          lr_left = l164d,
                                          lr_greens = l164e, 
                                          lr_ssw = l164g,
                                          lr_pirates = l164f,
                                          lr_afd = l164h,
                                          party_id = l174a,
                                          party_id_2 = l174b,
                                          party_id_strength = l175,
                                          job_type = l186,
                                          job_type_past = l187,
                                          income = l200) 




data_2017_nrw_raw <- foreign::read.dta("data_gesis/ZA6820_v1-0-0.dta") %>% 
    as_factor()

data_2017_nrw <- transmute(data_2017_nrw_raw,
                           study_id = "ZA6820_v1-0-0",
                           election_id = "Nordrhein-Westfalen 2017",
                           weight_microc = as.numeric(wei_mzz),
                           weight_online = as.numeric(wei_onz),
                           #duration_survey = field,
                           responses_quick_num = as.numeric(speederindex),
                           #duration = duration,
                           gender = l1,
                           year_of_birth = as.factor(l2),
                           education = l3,
                           pol_interest_fed =  l6a,
                           pol_interest_land = l6b,
                           satisf_democracy_fed = l7b,
                           vote_intention_land = l70,
                           scale_satisf_gov_land =  l102,
                           interest_campaign = l33,
                           internet_use_political = as.factor(l67),
                           vote_party_land = l71ba,
                           vote_party_land_postal = l72ba,
                           vote_party_land_hyp = l73ba,
                           vote_party_sure = l76,
                           importance_election_result = l80,
                           scale_cdu = as.factor(l86a),
                           scale_spd = as.factor(l86b),
                           scale_fdp = as.factor(l86e),
                           scale_greens = as.factor(l86d),
                           scale_pirates = as.factor(l86h),
                           scale_afd = as.factor(l86g),
                           scale_left = as.factor(l86c),
                           
                           ## not included!
                           # prob_coa_cdu_spd = l106a, 
                           # prob_coa_cdu = l106b,
                           # prob_coa_cdu_fw = l106f,
                           # prob_coa_spd_greens_fw = l106h,
                           
                           # prob_coa_cdu_fpd = l106d,
                           # prob_coa_spd_greens = l106c,
                           # prob_coa_spd_fdp_greens = l106j,
                           # prob_coa_cdu_fdp_greens = l106i,
                           # prob_coa_spd_left_greens = l106e,
                           # prob_coa_cdu_greens = l106h,
                           
                           wish_coa_cdu_spd = l108a,
                           wish_coa_spd_greens = l108c,
                           wish_coa_spd_left_greens = l108d,
                           wish_coa_cdu_fdp = l108h,
                           wish_coa_spd_fdp_greens = l108b,
                           wish_coa_spd_fdp = l108e,
                           wish_coa_cdu_greens = l108g,
                           wish_coa_cdu_fdp_greens = l108f,
                           wish_coa_cdu_afd = l108i,
                           
                           predict_coa_cdu_spd = l107a,
                           predict_coa_spd_greens = l107c,
                           predict_coa_spd_left_greens = l107d,
                           predict_coa_cdu_fdp = l107h,
                           predict_coa_spd_fdp_greens = l107b,
                           predict_coa_spd_fdp = l107e,
                           predict_coa_cdu_greens = l107g,
                           predict_coa_cdu_fdp_greens = l107f,
                           
                           lr_self = l165,
                           lr_cdu = l164a,
                           lr_spd = l164b,
                           lr_fdp = l164c,
                           lr_left = l164d,
                           lr_greens = l164e, 
                           lr_pirates = l164f,
                           lr_afd = l164h,
                           party_id = l174a,
                           party_id_2 = l174b,
                           party_id_strength = l175,
                           job_type = l186,
                           job_type_past = l187,
                           income = l200) 




dta_combined_raw <- bind_rows(data_2010_nrw, 
                              data_2011_sachsen_anhalt,
                              data_2011_rheinland_pfalz,
                              data_2011_baden_wuerttemberg,
                              data_2011_berlin,
                              data_2011_mecklenburg_vorpommern,
                              data_2012_schlewsig_holstein,
                              data_2012_nrw,
                              data_2013_niedersachsen,
                              data_2013_bayern,
                              data_2013_hessen,
                              data_2014_sachsen,
                              data_2014_brandenburg,
                              data_2014_thueringen,
                              data_2016_baden_wuerttemberg,
                              data_2016_rheinland_pfalz,
                              data_2016_mecklenburg_vorpommern,
                              data_2017_schleswig_holstein,
                              data_2017_nrw) %>% 
    separate(election_id, into = c("bundesland", "year"), sep = " ", remove = FALSE) 

nrow(dta_combined_raw)

# create unique ID for each respondent and arrange by year, bundesland, and respondent
dta_combined <- dta_combined_raw %>% 
    group_by(study_id) %>% 
    mutate(respondent_id = paste(election_id, "R", row_number(), sep = "_")) %>% 
    ungroup() %>% 
    dplyr::arrange(desc(year), bundesland, respondent_id) %>% 
    dplyr::select(study_id:year, respondent_id, everything())

nrow(dta_combined)

# check whether coalition prediction was binary (genannt/nicht genannt)
# or continuous (important for subsequent analysis)

coa_binary <- dta_combined %>% 
    filter(predict_coa_cdu_spd == "genannt") %>% 
    dplyr::select(election_id) %>% 
    unique() %>% 
    mutate(predict_coalition_type = "Coalitions: binary")

coa_binary

data_2013_niedersachsen_test <- 
    data_2013_niedersachsen %>% 
    select(starts_with("predict_coa_"))

coa_gov_parties <- dta_combined %>% 
    filter(predict_gov_cdu == "genannt") %>% 
    dplyr::select(election_id) %>% 
    unique() %>% 
    mutate(predict_coalition_type = "Government parties: binary")

coa_gov_parties

coa_continuous <- dta_combined %>% 
    filter(predict_coa_cdu_spd == "4") %>% 
    dplyr::select(election_id) %>% 
    unique() %>% 
    mutate(predict_coalition_type = "Coalitions: continuous")

coa_continuous

coa_type_df <- bind_rows(coa_binary, 
                         coa_gov_parties, 
                         coa_continuous)


dta_combined <- dta_combined %>% 
    left_join(coa_type_df, by = "election_id")

nrow(dta_combined)

# cross tables of question format
table(dta_combined$predict_coalition_type)


# now replace some characters and recode values
replace_ue <-  function(x){
    str_replace(x, 'ü', 'ue')
}

replace_ae <-  function(x){
    str_replace(x, 'ä', 'ae')
}


replace_ae2 <-  function(x){
    str_replace(x, 'mae�ig', 'maessig')
}

replace_ae3 <-  function(x){
    str_replace(x, 'm��ig', 'maessig')
}

replace_ae4 <-  function(x){
    str_replace(x, 'm�nn', 'maenn')
}


replace_ss <-  function(x){
    str_replace(x, 'ß', 'ss')
}

replace_ss2 <- function(x){
    str_replace(x, 'wei�', 'weiss')
}


replace_ueberhaupt <-  function(x){
    str_replace(x, '�berhaupt', 'ueberhaupt')
}

replace_colon <-  function(x){
    str_replace(x, ';', ',')
}

dta_combined <- mutate_if(dta_combined, is.character, as.factor)

dta_combined <- dta_combined %>% 
    mutate_all(funs(replace_ae)) %>% 
    mutate_all(funs(replace_ae2)) %>% 
    mutate_all(funs(replace_ae3)) %>% 
    mutate_all(funs(replace_ae4)) %>% 
    mutate_all(funs(replace_ue)) %>% 
    mutate_all(funs(replace_ueberhaupt)) %>% 
    mutate_all(funs(replace_ss)) %>% 
    mutate_all(funs(replace_ss2)) %>% 
    mutate_all(funs(replace_colon))


adjust_na_trifft_nicht_zu <-  function(x){
    str_replace(x, "trifft nicht zu", NA_character_)
}

adjust_na_keine_angabe <-  function(x){
    str_replace(x, "keine Angabe", "No information")
    
}

adjust_party_neg <- function(x){
    str_replace(x, pattern = '-5 halte ueberhaupt nichts von der Partei', '-5')
    
}

adjust_party_pos <- function(x){
    str_replace(x, pattern = '5 halte sehr viel von der Partei', '5')
    
}

adjust_satis_neg <- function(x){
    str_replace(x, pattern = '-5 vollstaendig unzufrieden', '-5')
    
}

adjust_satis_neg2 <- function(x){
    str_replace(x, pattern = '-5 voellig unzufrieden', '-5')
    
}


adjust_satis_neg3 <- function(x){
    str_replace(x, pattern = '-5 vollst�ndig unzufrieden', '-5')
    
}

adjust_satis_pos1 <- function(x){
    str_replace(x, pattern = fixed('+5 voll und ganz zufrieden'), '5')
    
}

adjust_satis_pos2 <- function(x){
    str_replace(x, pattern = fixed('+5 voellig zufrieden'), '5')
    
}


adjust_very_likely <- function(x){
    str_replace(x, 
                pattern = fixed('11 sehr wahrscheinlich'), '11')
    
}

adjust_very_unlikely <- function(x){
    str_replace(x, 
                pattern = fixed('1 sehr unwahrscheinlich'), '1')
    
}

adjust_very_desirable <- function(x){
    str_replace(x, 
                pattern = fixed('+5 sehr wuenschenswert'), '+5')
    
}

adjust_very_desirable2 <- function(x){
    str_replace(x, 
                pattern = fixed('+5 aeusserst wuenschenswert'), '+5')
    
}


adjust_very_undesirable <- function(x){
    str_replace(x, 
                pattern = fixed('-5 ueberhaupt nicht wuenschenswert'), '-5')
    
}

adjust_dk01 <- function(x){
    str_replace(x, 
                pattern = fixed('kann ich nicht einschaetzen'), "Don’t know")
    
}

adjust_dk02 <- function(x){
    str_replace(x, 
                pattern = fixed('kann ich nicht einsch�tzen'), "Don’t know")
    
}


adjust_dk03 <- function(x){
    str_replace(x, 
                pattern = fixed('weiss nicht'), "Don’t know")
    
}


adjust_dk04a <- function(x){
    str_replace(x, 
                pattern = fixed('nicht einzuschaetzen '), "Don’t know")
    
}

adjust_dk04b <- function(x){
    str_replace(x, 
                pattern = fixed('nicht einzuschaetzen'), "Don’t know")
    
}


adjust_dk05 <- function(x){
    str_replace(x, 
                pattern = fixed('wei� nicht'), "Don’t know")
    
}

adjust_very_left <- function(x){
    str_replace(x, 
                pattern = fixed('1 links'), '1')
    
}

adjust_very_right <- function(x){
    str_replace(x, 
                pattern = fixed('11 rechts'), '11')
    
}


dta_combined <- dta_combined %>% 
    mutate_all(funs(adjust_na_trifft_nicht_zu)) %>% 
    mutate_all(funs(adjust_na_keine_angabe)) %>% 
    mutate_all(funs(adjust_party_neg)) %>% 
    mutate_all(funs(adjust_party_pos)) %>% 
    mutate_all(funs(adjust_satis_pos1)) %>% 
    mutate_all(funs(adjust_satis_pos2)) %>% 
    mutate_all(funs(adjust_satis_neg)) %>% 
    mutate_all(funs(adjust_satis_neg2)) %>% 
    mutate_all(funs(adjust_satis_neg3)) %>% 
    mutate_all(funs(adjust_very_likely)) %>% 
    mutate_all(funs(adjust_very_unlikely)) %>% 
    mutate_all(funs(adjust_very_desirable)) %>% 
    mutate_all(funs(adjust_very_desirable2)) %>% 
    mutate_all(funs(adjust_very_undesirable)) %>% 
    mutate_all(funs(adjust_dk01)) %>% 
    mutate_all(funs(adjust_dk02)) %>% 
    mutate_all(funs(adjust_dk03)) %>% 
    mutate_all(funs(adjust_dk04a)) %>% 
    mutate_all(funs(adjust_dk04b)) %>% 
    mutate_all(funs(adjust_dk05)) %>% 
    mutate_all(funs(adjust_very_left)) %>% 
    mutate_all(funs(adjust_very_right))


# recode/harmonise income categories ---

recode_income <- c("'unter 500 Euro'='0-500';
                   'Unter 500 Euro'='0-500';
                   '500 bis unter 750 Euro'='500-1000';
                   '500 bis unter 900 Euro'='500-1000';
                   '750 bis unter 1000 Euro'='500-1000';
                   '900 bis unter 1300 Euro'='1000-1500';
                   '1000 bis unter 1250 Euro'='1000-1500';
                   '1250 bis unter 1500 Euro'='1000-1500';
                   '1300 bis unter 1500 Euro'='1000-1500';
                   '1500 bis unter 2000 Euro'='1500-3500';
                   '2000 bis unter 2500 Euro'='1500-3500';
                   '2000 bis unter 2600 Euro'='1500-3500';
                   '2500 bis unter 3000 Euro'='1500-3500';
                   '2600 bis unter 3500 Euro'='1500-3500';
                   '3000 bis unter 4000 Euro'='3500-5000';
                   '3500 bis unter 4500 Euro'='3500-5000';
                   '4000 bis unter 5000 Euro'='3500-5000';
                   '4500 bis unter 6000 Euro'='5000-8000';
                   '5000 bis unter 7500 Euro'='5000-8000';
                   '6000 bis unter 8000 Euro'='5000-8000';
                   '7500 Euro bis unter 10000 Euro'='8000-';
                   '8000 und mehr Euro'='8000-';
                   '10000 Euro und mehr'='8000-';
                   'keine Angabe'='No information'")

dta_combined <- dta_combined %>% 
    mutate(income_stand = car::recode(income, recode_income))

# also recode income into a numeric variable
recode_income_num <- c("'0-500'=1;
                        '500-1000'=2;
                        '1000-1500'=3;
                        '1500-3500'=4;
                        '3500-5000'=5;
                        '5000-8000'=6; 
                        '8000-'='7'; 
                        'No information'=NA")

dta_combined <- dta_combined %>% 
    mutate(income_num = car::recode(income_stand, recode_income_num))


table(dta_combined$income_num,
      dta_combined$income_stand)


# recode political interest

recode_interest <- c("'keine Angabe'='No information';
                     'ueberhaupt nicht'='No interest at all';
                     'weniger stark'='Not much interest';
                     'mittelmaessig'='Medium interest';
                     'stark'='Strong interest';
                     'sehr stark'='Very strong interest'")

levels_interest <- c("No interest at all", "Not much interest",
                     "Medium interest",  "Strong interest",
                     "Very strong interest", "No information")

dta_combined <- dta_combined %>% 
    mutate(pol_interest_fed_stand = car::recode(pol_interest_fed, recode_interest)) %>% 
    mutate(pol_interest_land_stand = car::recode(pol_interest_land, recode_interest)) %>% 
    mutate(interest_campaign_stand = car::recode(interest_campaign, recode_interest))

dta_combined$pol_interest_fed_stand <- factor(dta_combined$pol_interest_fed_stand,
                                              levels = levels_interest)

dta_combined$pol_interest_land_stand <- factor(dta_combined$pol_interest_land_stand,
                                               levels = levels_interest)

dta_combined$interest_campaign_stand <- factor(dta_combined$interest_campaign_stand,
                                               levels = levels_interest)

table(dta_combined$pol_interest_fed_stand,
      dta_combined$pol_interest_land_stand)


# recode satisfaction with democracy (on the federal level)

table(dta_combined$satisf_democracy_fed)

recode_satisf <- c("'keine Angabe'='No information';
                   'sehr unzufrieden'='Very dissatisfied';
                   'ziemlich unzufrieden'='Very dissatisfied';
                   'unzufrieden'='Dissatisfied';
                   'teils zufrieden, teils unzufrieden'='Undecided';
                   'zufrieden'='Satisfied';
                   'ziemlich zufrieden'='Very satisfied';
                   'sehr zufrieden'='Very satisfied'")

levels_satisf <- c("Very dissatisfied", "Dissatisfied",
                   "Undecided", "Satisfied",
                   "Very satisfied", "No information")

dta_combined <- dta_combined %>% 
    mutate(satisf_democracy_fed_stand = car::recode(satisf_democracy_fed, 
                                                    recode_satisf))

table(dta_combined$satisf_democracy_fed_stand)

dta_combined$satisf_democracy_fed_stand <- factor(dta_combined$satisf_democracy_fed_stand,
                                                  levels = levels_satisf)


# recode intention to vote (on the Land level)

table(dta_combined$vote_intention_land)

recode_vote_int <- c("'vielleicht zur Wahl gehen'='Maybe/probably vote';
                     'wahrscheinlich zur Wahl gehen'='Maybe/probably vote';
                   'wahrscheinlich nicht zur Wahl gehen'='Maybe/probably not vote';
                   'weiss ich noch nicht'='Don’t know';
                   'weiss nicht'='Don’t know';
                   'Briefwahl'='Voted already (postal)';
                   'habe bereits Briefwahl gemacht'='Voted already (postal)';
                   'bestimmt nicht zur Wahl gehen'='Definitely not vote';
                   'bestimmt zur Wahl gehen'='Definitely vote';
                     'keine Angabe'='No information'")

levels_vote_int <- c("Definitely not vote", "Maybe/probably not vote",
                     "Don’t know", "Maybe/probably vote",
                     "Definitely vote", "Voted already (postal)")

dta_combined <- dta_combined %>% 
    mutate(vote_intention_land_stand = car::recode(vote_intention_land,
                                                   recode_vote_int))

dta_combined$vote_intention_land_stand <- factor(dta_combined$vote_intention_land_stand,
                                                 levels = levels_vote_int)

table(dta_combined$vote_intention_land_stand)

# recode partyID/partyID2

table(dta_combined$party_id)

recode_party_id <- c("'andere Partei'='Other party';
                     'keine Angabe'='No information';
                     'keine Partei'='No party';
                     'keine Partei, keiner Partei'='No party';
                     'mit mehreren Parteien/Unentschieden'='Other party';
                     'sonstige Nennung'='Other party';
                     'Sonstige'='Other party'")

dta_combined <- dta_combined %>% 
    mutate(party_id_stand = car::recode(party_id, recode_party_id)) %>% 
    mutate(party_id_2_stand = car::recode(party_id_2, recode_party_id)) 

table(dta_combined$party_id_stand)

# merge and recode vote choice

dta_combined <- dta_combined %>% 
    mutate(vote_party_merged = if_else(is.na(vote_party_land),
                                       vote_party_land_postal,
                                       if_else(is.na(vote_party_land) & is.na(vote_party_land_postal),
                                               vote_party_land_hyp, vote_party_land)))


dta_combined <- dta_combined %>% 
    mutate(vote_party_type = if_else(!is.na(vote_party_land), "Entitled to vote",
                                     if_else(is.na(vote_party_land) & !is.na(vote_party_land_postal), 
                                             "Voted already", "Not entitled to vote")))

table(dta_combined$vote_party_merged)

# recode for for party
recode_vote_party <- c("'andere Partei'='Other party';
                       'CDU/CSU'='CDU'; 'CSU'='CDU';
                       'keine Angabe'='No information';
                       'noch nicht entschieden'='Don’t know';
                       'Sonstige'='Other party';
                       'sonstige Nennung'='Other party';
                       'Sonstige Nennung'='Other party';
                       'ungueltig waehlen'='Invalid vote';
                       'werde keine Zweitstimme abgeben'='Invalid vote';
                       'werde ungueltig waehlen'='Invalid vote'")

dta_combined <- dta_combined %>% 
    mutate(vote_party_merged = car::recode(vote_party_merged, recode_vote_party))

# recode education into binary variable
table(dta_combined$education, useNA = "always")

recode_education <- "'Abitur bzw. erweiterte Oberschule mit Abschluss 12. Klasse (Hochschulreife)'='A-Levels';
'Abitur oder erweiterte Oberschule mit Abschluss 12. Klasse (Hochschulreife)'='A-Levels';NA= NA;else='No A-Levels'"

dta_combined <- dta_combined %>% 
    mutate(education_stand = car::recode(education, recode_education))

# recode gender
table(dta_combined$gender)

recode_gender <- c("'maennlich'='Male';
                   ' maennlich'='Male';
                   'm<e4>nnlich'='Male';
                   'weiblich'='Female';
                   ' weiblich'='Female'")

dta_combined <- dta_combined %>% 
    mutate(gender_stand = car::recode(gender, recode_gender))

table(dta_combined$gender_stand, useNA = "always")

# recode year of birth to age
dta_combined <- dta_combined %>% 
    mutate(year_of_birth_num = as.numeric(as.character(year_of_birth))) %>% 
    mutate(age = as.numeric(year) - year_of_birth_num)

dta_combined <- dta_combined %>% 
    mutate(age_stand = as.numeric(age))


dta_combined <- mutate_if(dta_combined, is.character, as.factor)

# select the new standardised variables
dta_combined_stand <- dta_combined %>% 
    dplyr::select(predict_coalition_type:age_stand)


# select the scale and left right numeric variables
dta_recode_scale <- dta_combined %>%
    dplyr::select(starts_with("scale_"), starts_with("lr"))

dta_recode_scale <- mutate_if(dta_recode_scale, is.character, as.factor)

# recode don't know and no answer to NA
replace_scale_dk <-  function(x){
    str_replace(x, 'Don’t know', NA_character_)
}

replace_scale_no_answer <-  function(x){
    str_replace(x, 'No information', NA_character_)
}

replace_space <-  function(x){
    str_replace(x, ' ', '')
}


dta_recode_scale <- dta_recode_scale %>% 
    mutate_all(funs(replace_scale_dk)) %>% 
    mutate_all(funs(replace_scale_no_answer)) %>% 
    mutate_all(funs(replace_space))

# change character to numeric 
dta_recode_scale_num <- mutate_if(dta_recode_scale, is.character, as.numeric)
dta_recode_scale_num <- mutate_if(dta_recode_scale_num, is.factor, as.numeric)

# check that we didn't lose any observations
stopifnot(nrow(dta_combined) == nrow(dta_recode_scale_num))

# create a nicer dataset for the analysis
dta_combined <- dta_combined %>% 
    select(-c(starts_with("lr_"), starts_with("scale_"))) %>% 
    bind_cols(dta_recode_scale_num)


# transform each version of coalition predictions to long format -----

# binary coding of government party/parties

table(dta_combined$predict_coalition_type)

dta_predict_gov_binary <- dta_combined %>% 
    filter(predict_coalition_type == "Government parties: binary") %>% 
    dplyr::select(election_id, year, predict_coalition_type, respondent_id, 
                  starts_with("predict_gov"),
                  wish_coa_cdu, wish_coa_spd, wish_coa_fdp,
                  wish_coa_greens, wish_coa_left, 
                  wish_coa_ssw, wish_coa_pirates,
                  wish_coa_other)


dta_predict_gov_binary <- dta_predict_gov_binary %>% 
    mutate(predict_gov_cdu = ifelse(predict_gov_cdu == "genannt", "CDU", "")) %>% 
    mutate(predict_gov_spd = ifelse(predict_gov_spd == "genannt", "SPD", "")) %>% 
    mutate(predict_gov_fdp = ifelse(predict_gov_fdp == "genannt", "FDP", "")) %>% 
    mutate(predict_gov_greens = ifelse(predict_gov_greens == "genannt", "Greens", "")) %>% 
    mutate(predict_gov_left = ifelse(predict_gov_left == "genannt", "Left", "")) %>% 
    mutate(predict_gov_other = ifelse(predict_gov_other == "genannt", "Other", ""))

dta_predict_gov_binary <- dta_predict_gov_binary %>% 
    mutate(wish_coa_cdu = ifelse(wish_coa_cdu == "genannt", "CDU", "")) %>% 
    mutate(wish_coa_spd = ifelse(wish_coa_spd == "genannt", "SPD", "")) %>% 
    mutate(wish_coa_fdp = ifelse(wish_coa_fdp == "genannt", "FDP", "")) %>% 
    mutate(wish_coa_greens = ifelse(wish_coa_greens == "genannt", "Greens", "")) %>% 
    mutate(wish_coa_left = ifelse(wish_coa_left == "genannt", "Left", "")) %>% 
    mutate(wish_coa_other = ifelse(wish_coa_other == "genannt", "Other", "")) %>% 
    select(-c(wish_coa_pirates, wish_coa_ssw, wish_coa_other))


dta_predict_gov_binary <- dta_predict_gov_binary %>%
    mutate(predicted_coalition = stringi::stri_join(predict_gov_cdu,
                                                    predict_gov_spd,
                                                    predict_gov_fdp,
                                                    predict_gov_left,
                                                    predict_gov_greens,
                                                    predict_gov_other,
                                                    sep = "", 
                                                    ignore_null = TRUE)) %>% 
    mutate(wish_coalition_party = stringi::stri_join(wish_coa_cdu,
                                                     wish_coa_spd,
                                                     wish_coa_fdp,
                                                     wish_coa_left,
                                                     wish_coa_greens,
                                                     sep = "", 
                                                     ignore_null = TRUE))


dta_predict_gov_binary <- dta_predict_gov_binary %>% 
    mutate(predicted_coalition = car::recode(predicted_coalition, "''=NA")) %>% 
    mutate(wish_coalition_party = car::recode(wish_coalition_party, "''=NA"))


dta_predict_gov_binary <- dta_predict_gov_binary %>% 
    mutate(CDUSPD = ifelse(predicted_coalition == "CDUSPD",
                           TRUE, FALSE)) %>%
    mutate(CDUFDP = ifelse(predicted_coalition == "CDUFDP",
                           TRUE, FALSE)) %>%
    mutate(CDUGreens = ifelse(predicted_coalition == "CDUGreens",
                              TRUE, FALSE)) %>%
    mutate(CDUFDPGreens = ifelse(predicted_coalition == "CDUFDPGreens",
                                 TRUE, FALSE)) %>%
    mutate(SPDGreens = ifelse(predicted_coalition == "SPDGreens", TRUE, FALSE)) %>%
    mutate(SPDFDPGreens = ifelse(predicted_coalition == "SPDFDPGreens", TRUE, FALSE)) %>%
    mutate(SPDLeft = ifelse(predicted_coalition == "SPDLeft", TRUE, FALSE)) %>%
    mutate(SPDLeftGreens = ifelse(predicted_coalition == "SPDLeftGreens", TRUE, FALSE)) %>%
    mutate(CDU = ifelse(predicted_coalition == "CDU", TRUE, FALSE)) %>%
    mutate(SPD = ifelse(predicted_coalition == "SPD", TRUE, FALSE))

## transform to long format
dta_predict_gov_binary_long <- dta_predict_gov_binary %>% 
    select(-c(starts_with("predict_"), starts_with("wish_coa_"))) %>% 
    gather(key = coalition_option, value = predicted_coalition_dummy, 
           -c(respondent_id, election_id, year,
              predicted_coalition, wish_coalition_party)) %>% 
    arrange(election_id, respondent_id) %>% 
    mutate(predicted_coalition_dummy = as.logical(predicted_coalition_dummy))

table(dta_predict_gov_binary_long$predicted_coalition_dummy)

# recode coalition predictions
recode_coalitions_gov <- c("'CDU'='cdu'; 'CDUFDP'='cdu_fdp';
                           'CDUFDPGreens'='cdu_fdp_greens';
                           'CDUGreens'='cdu_greens';
                           'CDUFDPLeft'='cdu_fdp_left';
                           'CDUFDPOther'='cdu_fdp_other';
                           'CDULeft'='cdu_left';
                           'CDULeftGreens'='cdu_left_greens';
                           'CDUSPDLeftGreensOther'='cdu_spd_left_greens_other';
                           'CDUSPDLeftOther'='cdu_spd_left_other';
                           'CDUOther'='cdu_other';
                           'CDUSPD'='cdu_spd';
                           'CDUSPDFDP'='cdu_spd_fdp';
                           'CDUSPDFDPLeft'='cdu_spd_fdp_left';
                           'CDUSPDFDPGreens'='cdu_spd_fdp_greens';
                           'CDUSPDFDPLeftGreens'='cdu_spd_fdp_left_greens';
                           'CDUSPDFDPLeftGreensOther'='cdu_spd_fdp_left_greens_other';
                           'CDUSPDFDPOther'='cdu_spd_fdp_other';
                           'CDUSPDGreens'='cdu_spd_greens';
                           'CDUSPDLeft'='cdu_spd_left';
                           'CDUSPDLeftGreens'='cdu_spd_left_greens';
                           'CDUSPDOther'='cdu_spd_other';
                           'FDP'='fdp';
                           'FDPLeft'='fdp_left';
                           'FDPGreens'='fdp_greens';
                           'FDPGreensOther'='fdp_greens_other';
                           'FDPLeftGreens'='fdp_left_greens';
                           'Greens'='greens';
                           'GreensOther'='greens_other';
                           'Left'='left';
                           'LeftGreens'='left_greens';
                           'LeftOther'='left_other';
                           'Other'='other';
                           'SPD'='spd';
                           'SPDFDP'='spd_fdp';
                           'SPDFDPLeft'='spd_fdp_left';
                           'SPDFDPGreens'='spd_fdp_greens';
                           'SPDGreens'='spd_greens';
                           'SPDGreensOther'='spd_greens_other';
                           'SPDLeft'='spd_left';
                           'SPDLeftGreens'='spd_left_greens';
                           'SPDOther'='spd_other'")

dta_predict_gov_binary_long <- dta_predict_gov_binary_long %>% 
    mutate(coalition_option_stand = car::recode(coalition_option, 
                                                recode_coalitions_gov)) %>% 
    mutate(predicted_coalition_stand = car::recode(predicted_coalition,
                                                   recode_coalitions_gov))

table(dta_predict_gov_binary_long$coalition_option_stand)
table(dta_predict_gov_binary_long$predicted_coalition_stand)

# recode predicted and adjusted coalitions

table(dta_predict_gov_binary$predicted_coalition)

table(dta_predict_gov_binary_long$coalition_option)
table(dta_predict_gov_binary_long$predicted_coalition)

dta_predict_gov_binary_long <- dta_predict_gov_binary_long %>% 
    mutate(wish_coalition_dummy = as.logical(ifelse(wish_coalition_party == coalition_option, TRUE, FALSE))) %>% 
    mutate(dummy_wish_equals_predicted = as.logical(ifelse(predicted_coalition == wish_coalition_party, TRUE, FALSE))) %>% 
    mutate(number_predicted_coalitions = 1) %>% 
    mutate(number_predicted_coalitions = ifelse(predicted_coalition_stand == "", NA,
                                                number_predicted_coalitions))

table(dta_predict_gov_binary_long$election_id)

table(dta_predict_gov_binary_long$dummy_wish_equals_predicted)

# continuous coalition options

dta_predict_coa_continuous <- dta_combined %>% 
    filter(predict_coalition_type == "Coalitions: continuous") %>% 
    select(predict_coalition_type, year, respondent_id, election_id,
           starts_with("predict_coa"), starts_with("wish_coa"))

nrow(dta_predict_coa_continuous)

replace_scale_dk <-  function(x){
    str_replace(x, 'Don’t know', NA_character_)
}

replace_scale_no_answer <-  function(x){
    str_replace(x, 'keine Angabe', NA_character_)
}

dta_predict_coa_continuous <- dta_predict_coa_continuous %>% 
    mutate_all(funs(replace_scale_dk)) %>% 
    mutate_all(funs(replace_scale_no_answer))


# transform to long format

dta_predict_coa_continuous_long <- dta_predict_coa_continuous %>% 
    gather(key = coalition_option, value = score_option_predict, 
           -c(respondent_id, election_id, year, 
              predict_coalition_type, starts_with("wish_coa"))) %>% 
    mutate(coalition_option = as.factor(coalition_option)) %>% 
    mutate(score_option_predict = as.numeric(score_option_predict))


# filter only options that were acutally asked in a survey

dta_predict_coa_continuous_long <- dta_predict_coa_continuous_long %>% 
    group_by(election_id, coalition_option) %>% 
    mutate(sum_mentions_election = sum(score_option_predict, na.rm = TRUE)) %>% 
    ungroup()

# only keep respondents who replied to a coalition option
not_all_na <- function(x) any(!is.na(x))

dta_predict_coa_continuous_long <-  dta_predict_coa_continuous_long %>% 
    select_if(not_all_na)

table(dta_predict_coa_continuous_long$coalition_option)

dta_predict_coa_continuous_long <- dta_predict_coa_continuous_long %>% 
    mutate(score_option_wish = as.numeric(ifelse(coalition_option == "predict_coa_cdu", wish_coa_cdu,
                                                 ifelse(coalition_option == "predict_coa_cdu_afd", wish_coa_cdu_afd,
                                                        ifelse(coalition_option == "predict_coa_cdu_fdp", wish_coa_cdu_fdp,
                                                               ifelse(coalition_option == "predict_coa_cdu_fdp_greens", wish_coa_cdu_fdp_greens,
                                                                      ifelse(coalition_option == "predict_coa_cdu_greens", wish_coa_cdu_greens,
                                                                             ifelse(coalition_option == "predict_coa_cdu_spd", wish_coa_cdu_spd,
                                                                                    ifelse(coalition_option == "predict_coa_spd_fdp_greens", wish_coa_spd_fdp_greens,
                                                                                           ifelse(coalition_option == "predict_coa_spd_greens", wish_coa_spd_greens,
                                                                                                  ifelse(coalition_option == "predict_coa_spd_left", wish_coa_spd_left,
                                                                                                         ifelse(coalition_option == "predict_coa_spd_left_greens", 
                                                                                                                wish_coa_spd_left_greens, NA))))))))))))


dta_predict_coa_continuous_long <- dta_predict_coa_continuous_long %>%  
    mutate(score_option_wish = as.numeric(score_option_wish)) %>% 
    mutate(score_option_wish_0_10 = scales::rescale(score_option_wish, c(0, 10))) %>% 
    mutate(score_option_predict = as.numeric(score_option_predict)) 


table(dta_predict_coa_continuous_long$coalition_option)

dta_predict_coa_continuous_long <- dta_predict_coa_continuous_long %>% 
    arrange(election_id, respondent_id) %>% 
    group_by(election_id, respondent_id) %>% 
    mutate(score_predict_max = max(score_option_predict, na.rm = TRUE)) %>% 
    mutate(cont_pred_coalition_dummy = as.logical(ifelse(score_option_predict == score_predict_max, TRUE, FALSE))) %>% 
    mutate(cont_pred_number_evaluated = sum(!is.na(score_option_predict))) %>% 
    mutate(number_predicted_coalitions = sum(cont_pred_coalition_dummy, na.rm = TRUE)) %>% 
    mutate(cont_pred_avg_score = sum(score_option_predict, na.rm = TRUE) / cont_pred_number_evaluated) %>% 
    mutate(cont_pred_diff_avg = score_option_predict - cont_pred_avg_score)

dta_predict_coa_continuous_long <- dta_predict_coa_continuous_long %>% 
    mutate(coalition_option_stand = str_replace_all(coalition_option, "predict_coa_", ""))

nrow(dta_predict_coa_continuous_long)

table(dta_predict_coa_continuous_long$coalition_option_stand)

# create dummy that indicates which coalition option has highest "wish" score

dta_predict_coa_continuous_long <- dta_predict_coa_continuous_long %>% 
    group_by(respondent_id) %>% 
    mutate(score_option_wish_max = max(score_option_wish_0_10, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(wish_coalition_dummy = as.logical(ifelse(score_option_wish_max == score_option_wish_0_10, TRUE, FALSE))) 


dta_predict_coa_continuous_long <- dta_predict_coa_continuous_long %>% 
    mutate(predicted_coalition_dummy = as.logical(ifelse(score_option_predict == score_predict_max, TRUE, FALSE))) %>% 
    mutate(dummy_wish_equals_predicted = as.logical(ifelse(score_option_predict == score_predict_max & wish_coalition_dummy == TRUE, 
                                                           TRUE, FALSE)))


# check how many respondents chose how many options
dta_predict_coa_continuous_sum <- dta_predict_coa_continuous_long %>% 
    group_by(election_id, respondent_id) %>% 
    summarise(number_predicted_coalitions = sum(cont_pred_coalition_dummy, na.rm = TRUE))


# only keep coalitions in the dataset that were asked in a survey
dta_predict_coa_continuous_long <- dta_predict_coa_continuous_long %>% 
    filter(sum_mentions_election != 0)

nrow(dta_predict_coa_continuous_long)

dta_predict_coa_continuous_long_sum <- dta_predict_coa_continuous_long %>% 
    group_by(respondent_id) %>% 
    mutate(predicted_coalition_stand = ifelse(predicted_coalition_dummy == TRUE,
                                              coalition_option_stand, NA)) %>% 
    filter(!is.na(predicted_coalition_stand)) %>% 
    select(respondent_id, predicted_coalition_stand) %>% 
    unique()




nrow(dta_predict_coa_continuous_long_sum)

dta_predict_coa_continuous_long_merged <- left_join(
    dta_predict_coa_continuous_long,
    dta_predict_coa_continuous_long_sum, 
    by = "respondent_id")


dta_predict_coa_continuous_long_merged_no_guessers <- dta_predict_coa_continuous_long_merged %>% 
    filter(number_predicted_coalitions <= 3)



sum_answers <- dta_predict_coa_continuous_long_merged_no_guessers %>% 
    group_by(election_id, respondent_id) %>% 
    mutate(n = n()) %>% 
    arrange(-n) 
hist(sum_answers$n)

table(dta_predict_coa_continuous_long$coalition_option_stand)


## binary coalition options

dta_predict_coa_binary <- dta_combined %>% 
    filter(predict_coalition_type == "Coalitions: binary") %>% 
    select(predict_coalition_type, year, respondent_id, election_id,
           starts_with("predict_coa"), starts_with("wish_coa"))

dta_predict_coa_binary <- dta_predict_coa_binary %>%
    mutate(wish_coa_cdu = ifelse(wish_coa_cdu == "genannt", "CDU", "")) %>% 
    mutate(wish_coa_spd = ifelse(wish_coa_spd == "genannt", "SPD", "")) %>% 
    mutate(wish_coa_fdp = ifelse(wish_coa_fdp == "genannt", "FDP", "")) %>% 
    mutate(wish_coa_greens = ifelse(wish_coa_greens == "genannt", "Greens", "")) %>% 
    mutate(wish_coa_left = ifelse(wish_coa_left == "genannt", "Left", "")) %>% 
    mutate(wish_coa_other = ifelse(wish_coa_other == "genannt", "Other", ""))

# remove all columns that have only NA values

not_any_na <- function(x) all(!is.na(x))

dta_predict_coa_binary <- dta_predict_coa_binary %>% 
    select_if(not_any_na)

dta_predict_coa_binary <- dta_predict_coa_binary %>%
    mutate(wish_coalition_raw_party = stringi::stri_join(wish_coa_cdu,
                                                         wish_coa_spd,
                                                         wish_coa_fdp,
                                                         wish_coa_left,
                                                         wish_coa_greens,
                                                         wish_coa_other,
                                                         sep = "", 
                                                         ignore_null = TRUE))


recode_coalition_wish <- c("'CDU'='cdu'; 'CDUFDP'='cdu_fdp';
                           'CDUFDPGreens'='cdu_fdp_greens';
                           'CDUGreens'='cdu_greens';
                           'CDUGreensOther'='cdu_greens_other';
                           'CDULeft'='cdu_left';
                           'CDUOther'='cdu_other';
                           'CDUSPD'='cdu_spd';
                           'CDUSPDFDP'='cdu_spd_fdp';
                           'CDUSPDFDPGreens'='cdu_spd_fdp_greens';
                           'CDUSPDFDPLeftGreens'='cdu_spd_fdp_left_greens';
                           'CDUSPDGreens'='cdu_spd_greens';
                           'CDUSPDLeft'='cdu_spd_left';
                           'fdp'='FDP';
                           'FDPGreens'='fdp_greens';
                           'FDPLeftGreens'='fdp_left_greens';
                           'Greens'='greens';
                           'Left'='left';
                           'LeftGreens'='left_greens';
                           'Other'='other';
                           'SPD'='spd';
                           'SPDFDP'='spd_fdp';
                           'SPDFDPGreens'='spd_fdp_greens';
                           'SPDGreens'='spd_greens';
                           'SPDLeft'='spd_left';
                           'SPDLeftGreens'='spd_left_greens';
                           'SPDOther'='spd_other'")


dta_predict_coa_binary <- dta_predict_coa_binary %>% 
    mutate(wish_coalition_party = car::recode(wish_coalition_raw_party, 
                                              recode_coalition_wish))



dta_predict_gov_binary <- dta_predict_gov_binary %>% 
    mutate(CDUSPD = ifelse(predicted_coalition == "CDUSPD",
                           TRUE, FALSE)) %>%
    mutate(CDUFDP = ifelse(predicted_coalition == "CDUFDP",
                           TRUE, FALSE)) %>%
    mutate(CDUGreens = ifelse(predicted_coalition == "CDUGreens",
                              TRUE, FALSE)) %>%
    mutate(CDUFDPGreens = ifelse(predicted_coalition == "CDUFDPGreens",
                                 TRUE, FALSE)) %>%
    mutate(SPDGreens = ifelse(predicted_coalition == "SPDGreens", TRUE, FALSE)) %>%
    mutate(SPDFDPGreens = ifelse(predicted_coalition == "SPDFDPGreens", TRUE, FALSE)) %>%
    mutate(SPDLeft = ifelse(predicted_coalition == "SPDLeft", TRUE, FALSE)) %>%
    mutate(SPDLeftGreens = ifelse(predicted_coalition == "SPDLeftGreens", TRUE, FALSE)) %>%
    mutate(CDU = ifelse(predicted_coalition == "CDU", TRUE, FALSE)) %>%
    mutate(SPD = ifelse(predicted_coalition == "SPD", TRUE, FALSE))


replace_coa_named <-  function(x){
    str_replace(x, 'genannt', "TRUE")
}

replace_coa_not_named <-  function(x){
    str_replace(x, fixed('nicht genannt'), "FALSE")
}


dta_predict_coa_binary <- dta_predict_coa_binary %>% 
    mutate_all(funs(replace_coa_not_named)) %>% 
    mutate_all(funs(replace_coa_named)) 

dta_predict_coa_binary <- mutate_if(dta_predict_coa_binary, is.character, as.factor)

# transform to long format

summary(dta_predict_coa_binary)

dta_predict_coa_binary_long <- dta_predict_coa_binary %>% 
    select(-starts_with("wish_coa_")) %>% 
    gather(key = coalition_option, value = predicted_coalition_dummy, 
           -c(respondent_id, election_id, year, wish_coalition_party, wish_coalition_raw_party, predict_coalition_type)) %>% 
    mutate(coalition_option = as.factor(coalition_option)) %>% 
    mutate(predicted_coalition_dummy = as.logical(predicted_coalition_dummy))


dta_predict_coa_binary_long <- dta_predict_coa_binary_long %>% 
    mutate(coalition_option_stand = str_replace_all(coalition_option, "predict_coa_", "")) %>% 
    mutate(dummy_wish_equals_predicted = as.logical(ifelse(coalition_option_stand == wish_coalition_party, TRUE, FALSE)))


dta_predict_coa_binary_long_sum <- dta_predict_coa_binary_long %>% 
    mutate(predicted_coalition_stand = ifelse(predicted_coalition_dummy == TRUE, coalition_option_stand, NA)) %>% 
    dplyr::select(respondent_id, predicted_coalition_stand) %>% 
    filter(!is.na(predicted_coalition_stand)) %>% 
    group_by(respondent_id) %>%    
    mutate(number_predicted_coalitions = n()) 


dta_predict_coa_binary_long_merged <- left_join(dta_predict_coa_binary_long,
                                                dta_predict_coa_binary_long_sum, 
                                                by = c("respondent_id"))


# merge the three long datasets

dta_predicted_combination <- bind_rows(
    dta_predict_gov_binary_long,
    dta_predict_coa_continuous_long_merged, 
    dta_predict_coa_binary_long_merged) %>% 
    select(-c(wish_coa_cdu:wish_coa_spd_fdp)) %>% 
    select(-c(predict_coalition_type))


# merge metadata on each respondent
dta_merged <- left_join(dta_predicted_combination, dta_combined,
                        by = c("election_id", "year", "respondent_id"))

nrow(dta_merged)
nrow(dta_predicted_combination)

# change order of variables in data frame
dta_merged <- dta_merged %>% 
    select(election_id, predict_coalition_type, 
           year, respondent_id, coalition_option_stand,
           predicted_coalition_stand, predicted_coalition_dummy,
           dummy_wish_equals_predicted,
           everything())


dta_merged <- dta_merged %>% 
    group_by(respondent_id) %>% 
    mutate(predicted_coalition_stand = ifelse(predicted_coalition_stand == "", NA, predicted_coalition_stand)) 

# adjust left-right judgments

dta_merged <- dta_merged %>%     
    group_by(election_id) %>% 
    mutate(n_respondents = n()) %>% # problem that some have NA
    mutate(lr_avg_cdu = mean(lr_cdu, na.rm = TRUE),
           lr_avg_spd = mean(lr_spd, na.rm = TRUE),
           lr_avg_fdp = mean(lr_fdp, na.rm = TRUE),
           lr_avg_afd = mean(lr_afd, na.rm = TRUE),
           lr_avg_fw = mean(lr_fw, na.rm = TRUE),
           lr_avg_left = mean(lr_left, na.rm = TRUE),
           lr_avg_greens = mean(lr_greens, na.rm = TRUE)) %>% 
    ungroup()

table(dta_merged$lr_avg_fw)

# calculate average left-right position of the coalition
dta_merged <- dta_merged %>%
    group_by(election_id) %>%
    mutate(avg_lr_coa_respondent = case_when(
        coalition_option_stand == "cdu_fdp" ~ (lr_cdu + lr_fdp) / 2,
        coalition_option_stand == "cdu_fdp_greens" ~ (lr_cdu + lr_greens + lr_fdp) / 3,
        coalition_option_stand == "cdu_greens" ~ (lr_cdu + lr_greens) / 2,
        coalition_option_stand == "cdu_spd" ~ (lr_spd + lr_cdu) / 2,
        coalition_option_stand == "spd_fdp" ~  (lr_fdp + lr_spd) / 2,
        coalition_option_stand == "spd_fdp_greens" ~ (lr_greens + lr_fdp + lr_spd) / 3,
        coalition_option_stand == "spd_greens" ~ (lr_spd + lr_greens) / 2,
        coalition_option_stand == "spd_left" ~ (lr_spd + lr_left) / 2,
        coalition_option_stand == "spd_greens_fw" ~ (lr_greens + lr_spd  + lr_fw) / 3,
        coalition_option_stand == "spd_left" ~ (lr_spd + lr_left) / 2,
        coalition_option_stand == "cdu_afd" ~ (lr_cdu + lr_afd) / 2,
        coalition_option_stand == "cdu_fw" ~ (lr_cdu + lr_fw) / 2,
        coalition_option_stand == "spd_left_greens" ~ (lr_spd + lr_left + lr_greens) / 3,
        coalition_option_stand == "cdu" ~ lr_cdu,
        coalition_option_stand == "spd" ~ lr_spd
    )) %>%
    mutate(distance_lr_coa_self = abs(avg_lr_coa_respondent - lr_self)) %>%
    ungroup()



# calculate average left-right position of the coalition (using the average values)
dta_merged <- dta_merged %>%
    group_by(election_id) %>%
    mutate(avg_lr_coa_avg = case_when(
        coalition_option_stand == "cdu_fdp" ~ (lr_avg_cdu + lr_avg_fdp) / 2,
        coalition_option_stand == "cdu_fdp_greens" ~ (lr_avg_cdu + lr_avg_greens + lr_avg_fdp) / 3,
        coalition_option_stand == "cdu_greens" ~ (lr_avg_cdu + lr_avg_greens) / 2,
        coalition_option_stand == "cdu_spd" ~ (lr_avg_spd + lr_avg_cdu) / 2,
        coalition_option_stand == "spd_fdp" ~ (lr_avg_fdp + lr_avg_spd) / 2,
        coalition_option_stand == "spd_fdp_greens" ~ (lr_avg_greens + lr_avg_fdp + lr_avg_spd) / 3,
        coalition_option_stand == "spd_greens" ~ (lr_avg_spd + lr_avg_greens) / 2,
        coalition_option_stand == "spd_left" ~ (lr_avg_spd + lr_avg_left) / 2,
        coalition_option_stand == "cdu_afd" ~ (lr_avg_cdu + lr_avg_afd) / 2,
        coalition_option_stand == "spd_fdp" ~ (lr_avg_spd + lr_avg_fdp) / 2,
        coalition_option_stand == "spd_greens_fw" ~ (lr_avg_greens + lr_avg_spd  + lr_avg_fw) / 3,
        coalition_option_stand == "cdu_fw" ~ (lr_avg_cdu + lr_avg_fw) / 2,
        coalition_option_stand == "spd_left_greens" ~ (lr_avg_spd + lr_avg_left + lr_avg_greens) / 3,
        coalition_option_stand == "cdu" ~ lr_avg_cdu,
        coalition_option_stand == "spd" ~ lr_avg_spd
    )) %>%
    mutate(distance_lr_avg_coa_self = abs(avg_lr_coa_avg - lr_self)) %>%
    ungroup()


# if distance on respondent-level is missing use the average across all respondents
dta_merged <- dta_merged %>% 
    mutate(distance_lr_coa_self_no_missing = 
               ifelse(is.na(distance_lr_coa_self), 
                      distance_lr_avg_coa_self, 
                      distance_lr_coa_self)) %>% 
    mutate(lr_dummy_coa_missing = ifelse(is.na(distance_lr_coa_self), TRUE, FALSE))



# dta_merged <- dta_merged %>% 
##    mutate(distance_coa_respondent_no_missing = ifelse(is.na(distance_lr_coa_self), 
##                                                       distance_lr_avg_coa_self, 
##                                                       distance_lr_coa_self)) %>% 
##    mutate(lr_dummy_coa_missing = ifelse(is.na(distance_coa_respondent), TRUE, FALSE))


# estimate absolute distance between the two most extreme parties in a coalition
dta_merged <- dta_merged %>%
    group_by(election_id) %>%
    mutate(distance_coa_respondent_2_most_extreme_parties = case_when(
        coalition_option_stand == "cdu_fdp" ~ abs(lr_cdu - lr_fdp),
        coalition_option_stand == "cdu_fdp_greens" ~ abs(lr_cdu - lr_greens),
        coalition_option_stand == "cdu_greens" ~ abs(lr_cdu - lr_greens),
        coalition_option_stand == "cdu_spd" ~ abs(lr_cdu - lr_spd),
        coalition_option_stand == "spd_fdp_greens" ~ abs(lr_fdp - lr_greens),
        coalition_option_stand == "spd_greens" ~ abs(lr_spd - lr_greens),
        coalition_option_stand == "spd_fdp" ~ abs(lr_fdp - lr_spd),
        coalition_option_stand == "spd_left" ~ abs(lr_spd - lr_left),
        coalition_option_stand == "spd_greens_fw" ~ abs(lr_fw - lr_greens) ,
        coalition_option_stand == "spd_left" ~ abs(lr_spd - lr_left),
        coalition_option_stand == "cdu_afd" ~ abs(lr_afd - lr_cdu),
        coalition_option_stand == "cdu_fw" ~ abs(lr_fw - lr_cdu),
        coalition_option_stand == "spd_left_greens" ~ abs(lr_spd - lr_left),
        coalition_option_stand == "cdu" ~ 0,
        coalition_option_stand == "spd" ~ 0
    )) %>%
    ungroup()


dta_merged <- dta_merged %>%
    group_by(election_id) %>%
    mutate(distance_coa_avg_2_most_extreme_parties = case_when(
        coalition_option_stand == "cdu_fdp" ~ abs(lr_avg_cdu - lr_avg_fdp),
        coalition_option_stand == "cdu_fdp_greens" ~ abs(lr_avg_cdu - lr_avg_greens),
        coalition_option_stand == "cdu_greens" ~ abs(lr_avg_cdu - lr_avg_greens),
        coalition_option_stand == "cdu_spd" ~ abs(lr_avg_cdu - lr_avg_spd),
        coalition_option_stand == "spd_fdp_greens" ~ abs(lr_avg_fdp - lr_avg_greens),
        coalition_option_stand == "spd_greens" ~ abs(lr_avg_spd - lr_avg_greens),
        coalition_option_stand == "spd_left" ~ abs(lr_avg_spd - lr_avg_left),
        coalition_option_stand == "spd_fdp" ~ abs(lr_avg_fdp - lr_avg_spd),
        coalition_option_stand == "spd_greens_fw" ~ abs(lr_avg_fw - lr_avg_greens),
        coalition_option_stand == "spd_left" ~ abs(lr_avg_spd - lr_avg_left),
        coalition_option_stand == "cdu_afd" ~ abs(lr_avg_afd - lr_avg_cdu),
        coalition_option_stand == "cdu_fw" ~ abs(lr_avg_fw - lr_avg_cdu),
        coalition_option_stand == "spd_left_greens" ~ abs(lr_avg_spd - lr_avg_left),
        coalition_option_stand == "cdu" ~ 0,
        coalition_option_stand == "spd" ~ 0
    )) %>%
    ungroup()

dta_merged_test <- filter(dta_merged, coalition_option_stand == "cdu_fw")

table(dta_merged_test$distance_coa_avg_2_most_extreme_parties)
table(dta_merged_test$distance_coa_respondent_2_most_extreme_parties)


dta_merged <- dta_merged %>% 
    mutate(distance_coa_2_parties_no_missing = 
               ifelse(is.na(distance_coa_respondent_2_most_extreme_parties), 
                      distance_coa_avg_2_most_extreme_parties, 
                      distance_coa_respondent_2_most_extreme_parties)) 


# save file with state elections
saveRDS(dta_merged, file = "data_studies_merged_land.rds")

