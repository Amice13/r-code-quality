# This code generates the summary table present in the main text, and the numbers for the balance table (Supplemet Table 1a)

# Lines 95-140 generates balance tables with the cobalt package (parameters must be selected at lines 118-131)
# Supplement Table 1a sums up all of the possible balance tables that these lines can generate


library(readr)
library(anytime)
library(cobalt)
library("WeightIt")
library(dplyr)

#Data import:

raw = resp


# Variables preprocessing:

raw$independent = (1-raw$party_dem) * (1-raw$party_rep)
raw$region1 = as.numeric(raw$region == "1")
raw$region2 = as.numeric(raw$region == "2")
raw$region3 = as.numeric(raw$region == "3")
raw$region4 = as.numeric(raw$region == "4")

# Extraction of key baseline variables:


baseline_variables <- c("age","region1","region2","region3","region4",
                                     "hs_graduate", "hhi_above_60k",
                                     "sex_female","sex_male", "party_dem", "party_rep","independent","safety_mask_in_always",
                        "safety_mask_out_always",
                        "safety_hands_always",
                        "safety_distance_always")
var_names = c("Age","region1","region2","region3","region4","High school graduate","HHI above 60k","Female","Male","Party dem.","Party rep.","Independent","Mask in (always)", "Mask out (always)",
              "Wash hands (always)", "Distance (always)")


raw2=raw


raw2$complete_key_baseline = as.numeric(complete.cases(raw2[,baseline_variables]))



#filter out people with missing baseline data on key variables
raw2 <- raw2%>% filter(complete_key_baseline == 1)

black=raw2 %>% filter(race_white==0)
white=raw2 %>% filter(race_white==1)


panels = list(raw2,black,white,raw2[raw2$covid_any==1,],black[black$covid_any==1,],white[white$covid_any==1,],raw2[raw2$covid_any==0,],black[black$covid_any==0,]
              , white[white$covid_any==0,])

continuous_variables = c("age")

#Generation of the summary table:

lines = c()
i=1
for (panel in panels){
  lines = c(lines,nrow(panel))
  for (var in baseline_variables){
    if (var %in% continuous_variables){ # for continuous variable, we compute mean and standard error
      lines = c(lines,paste0(format(round(mean(panel[[var]],na.rm=TRUE), digits=2), nsmall = 2)," (",format(round(sd(panel[[var]],na.rm=TRUE), digits=2), nsmall = 2),")"))
    }else{ # for dummy variables, we compute number of observations and percentage 
      lines = c(lines,paste0(sum(panel[[var]]==1,na.rm=TRUE)," (",format(round(100*sum(panel[[var]]==1,na.rm=TRUE)/sum(!is.na(panel[[var]])), digits=1), nsmall = 1),")"))
    }
    
  }

  i=i+1
}



tab = matrix(lines, ncol=9)

colnames(tab) <- c("All", "African-American",  "White","All", "African-American",  "White","All", "African-American",  "White")
rownames(tab) = c("Nb observations",var_names)
tab
# save Summary Table
write.csv(tab, paste0("./Output/summary_table_all.csv"))










#######################
## Cobalt Balance Table

# We build the treatment variables:


baseline_variables  <- c("age","region1","region2","region3","region4",
                         "hs_graduate", "hhi_above_60k",
                         "sex_female","sex_male", "party_dem", "party_rep","independent","safety_mask_in_always",
                         "safety_mask_out_always",
                         "safety_hands_always",
                         "safety_distance_always")
raw2=raw

# We keep those who completed key baseline variables

raw2$complete_key_baseline = as.numeric(complete.cases(raw2[,baseline_variables]))


#filter out people with missing baseline data on key variables
raw2 <- raw2%>% filter(complete_key_baseline == 1)


### PARAMETERS

### Selection of the Panel (uncomment one)

data=raw2 #Sample = All
#data = raw2[!is.na(raw2$timing_knowledge_page_submit),] #Sample = participants who answered the knowledge questions
#data = raw2[!is.na(raw2$timing_link_choice_page_submit),] #Sample = participants who answered the link choice questions
#data = raw2[!is.na(raw2$timing_knowledge_page_submit_followup),] #Sample = participants who answered the knowledge questions of the follow up survey

### Selection of the treatment:

treatment = "black_doc" # "covid_any","black_doc","racism_ama" or "racism_doc"

### END OF PARAMETERS

sum_variables = paste0(baseline_variables, collapse = "+")
formula = as.formula(paste0(treatment , "~",sum_variables))

W.out <- weightit(formula, data = data[!is.na(data[,treatment]),],
                  method = "ps", estimand = "ATT",focal=1)

# Outputs a balance table with all interactions between the baseline_variables for the selected treatment, in the selected panel
bal.tab(W.out,int=TRUE,un = FALSE,m.threshold = .1,ks.threshold = 0.05)





