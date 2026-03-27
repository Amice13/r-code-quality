# Provides the numbers for the consort chart


set.seed(302)

# Data import 
raw = resp

# Creation of some baseline variables:
raw$strat1=as.numeric(raw$stratum == "old_f_white_nongop")
raw$strat2=as.numeric(raw$stratum == "old_f_white_gop")
raw$strat3=as.numeric(raw$stratum == "old_f_nonwhite")
raw$strat4=as.numeric(raw$stratum == "young_f_white_nongop")
raw$strat5=as.numeric(raw$stratum == "young_f_white_gop")
raw$strat6=as.numeric(raw$stratum == "young_f_nonwhite")
raw$strat7=as.numeric(raw$stratum == "old_m_white_nongop")
raw$strat8=as.numeric(raw$stratum == "old_m_white_gop")
raw$strat9=as.numeric(raw$stratum == "old_m_nonwhite")
raw$strat10=as.numeric(raw$stratum == "young_m_white_nongop")
raw$strat11=as.numeric(raw$stratum == "young_m_white_gop")
raw$strat12=as.numeric(raw$stratum == "young_m_nonwhite")

# Key baseline variables (used in the hainmueller weights for follow-up outcomes):
key_baseline_variables =  c("age", "party_dem", "party_rep",
                        "hs_graduate", "hhi_above_60k",
                        "sex_female","strat1",
                        "strat2","strat3","strat4","strat5","strat6","strat7","strat8"
                        ,"strat9","strat10","strat11","strat12","safety_mask_in_often",
                        "safety_mask_out_often",
                        "safety_hands_often",   
                        "safety_distance_often","black_more_likely_to_die")


raw$complete_key_baseline = as.numeric(complete.cases(raw[,key_baseline_variables]))



#######FLOW CHART CODE STARTS HERE


raw$complete <- raw$finished
raw$complete_knowledge_followup= !is.na(raw$know_gap_count_followup) #sample who has completed knowledge questions so that we can build our knowledge index

raw$anytreat <- as.numeric(raw$covid_any)

raw$complete_knowledge <- as.numeric(!is.na(raw$know_gap_count)) #sample who has completed knowledge questions so that we can build our knowledge index
raw$complete_links <- as.numeric(!is.na(raw$timing_link_choice_page_submit)) #sample who has completed links questions

raw$targeted_followup = as.numeric(is.na(raw$not_to_recontact))

#number in treatment
raw %>% group_by(anytreat) %>% summarise(n())

placebo <- raw %>% filter(anytreat == 0)
anytreat <- raw %>% filter(anytreat == 1)

#number in black doc
anytreat %>% group_by(black_doc) %>% summarise(n())

#number in ama race statement
anytreat %>% group_by(racism_ama) %>% summarise(n())

#number in doctor racial discrepancy in covid
anytreat %>% group_by(racism_doc) %>% summarise(n())

#number from treated who did not finish
anytreat %>% group_by(complete) %>% summarise(n())

#number control who did not finish

placebo %>% group_by(complete) %>% summarise(n())


# This function is for flow chart update with knowledge question
get_new_flow_chart_numbers <- function(raw){
  #number in treatment
  message("number in treatment")
  raw %>% group_by(anytreat) %>% summarise(n()) %>% View("treatment")
  
  pure_control <- raw %>% filter(anytreat == 0)
  anytreat <- raw %>% filter(anytreat == 1)
  
  
  message("number ama racism, treated")
  anytreat %>% group_by(racism_ama) %>% summarise(n()) %>% View("ama racism treated")
  
  message("number ama racism, control")
  placebo %>% group_by(racism_ama) %>% summarise(n()) %>% View("ama racism control")
  
  message("number in black doc, treated")
  anytreat %>% group_by(racism_ama, black_doc) %>% summarise(n()) %>% View("black doc treated")
  
  message("number in black doc, control")
  placebo %>% group_by(racism_ama, black_doc) %>% summarise(n()) %>% View("black doc control")
  
  message("number assigned doc racial discrepancy in covid, treated")
  anytreat %>% group_by(racism_ama, black_doc, racism_doc) %>% summarise(n()) %>% View("doc racial discrepancy in covid treated")
  
  message("number knowledge questions, treated")
  anytreat %>% group_by(racism_ama, black_doc, racism_doc, complete_knowledge) %>% summarise(n()) %>% View("knowledge completion treated")
  
  message("number knowledge questions, control")
  placebo %>% group_by(racism_ama, black_doc,complete_knowledge) %>% summarise(n()) %>% View("knowledge completion control")
  
  message("number link questions, treated")
  anytreat %>% group_by(racism_ama, black_doc, racism_doc, complete_links) %>% summarise(n()) %>% View("links completion treated")
  
  message("number link questions, control")
  placebo %>% group_by(racism_ama, black_doc,  complete_links)  %>% summarise(n()) %>% View("links completion control")
  
  
  message("number targeted follow up, treated")
  anytreat %>% group_by(racism_ama, black_doc, racism_doc, targeted_followup) %>% summarise(n()) %>% View("targeted followup treated")
  
  message("number targeted follow up, control")
  placebo %>% group_by(racism_ama, black_doc, targeted_followup) %>% summarise(n()) %>% View("targeted followup control")


  message("number finished follow up knowledge questions, treated")
  anytreat %>% group_by(racism_ama, black_doc, racism_doc, complete_knowledge_followup) %>% summarise(n()) %>% View("finished followup treated")
  
  message("number finished follow up knowledge questions, control")
  placebo %>% group_by(racism_ama, black_doc, complete_knowledge_followup) %>% summarise(n()) %>% View("finished followup control")

  #the follow up regressions are weighted -> we only use the observations with complete key baseline variables
  message("number finished follow up knowledge questions and completed key baseline, treated")
  anytreat %>% group_by(racism_ama, black_doc, racism_doc, complete_knowledge_followup, complete_key_baseline) %>% summarise(n()) %>% View("finished followup and key baseline treated")
  
  message("number finished follow up knowledge questions and completed key baseline, control")
  placebo %>% group_by(racism_ama, black_doc, complete_knowledge_followup, complete_key_baseline) %>% summarise(n()) %>% View("finished followup and key baseline control")
  
}

get_new_flow_chart_numbers(raw)



