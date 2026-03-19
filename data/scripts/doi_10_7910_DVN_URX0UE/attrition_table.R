# This script generates the attrition table (Supplement Table 1b)


library(readr)
library(anytime)
library(cobalt)
library("WeightIt")
library(dplyr)

#Data import:

raw = resp

treated = raw[raw$covid_any==1,]
placebo = raw[raw$covid_any==0,]


ama_racism = raw[raw$racism_ama==1,]
no_ama_racism = raw[raw$racism_ama==0,]

#Generation of the attrition table:

lines = c(nrow(treated),nrow(placebo),"",nrow(ama_racism),nrow(no_ama_racism),"")

attrited_knowledge = c(sum(is.na(treated$timing_knowledge_page_submit)),sum(is.na(placebo$timing_knowledge_page_submit)),"",
                       sum(is.na(ama_racism$timing_knowledge_page_submit)),sum(is.na(no_ama_racism$timing_knowledge_page_submit)),"")


attrited_knowledge_perc = c(paste0(format(round(100*sum(is.na(treated$timing_knowledge_page_submit))/nrow(treated), digits=1), nsmall = 1),"%"),
                            paste0(format(round(100*sum(is.na(placebo$timing_knowledge_page_submit))/nrow(placebo), digits=1), nsmall = 1),"%"),
                            ks.test(as.numeric(is.na(treated$timing_knowledge_page_submit)),as.numeric(is.na(placebo$timing_knowledge_page_submit)))$p.value,
                            paste0(format(round(100*sum(is.na(ama_racism$timing_knowledge_page_submit))/nrow(ama_racism), digits=1), nsmall = 1),"%"),
                            paste0(format(round(100*sum(is.na(no_ama_racism$timing_knowledge_page_submit))/nrow(no_ama_racism), digits=1), nsmall = 1),"%"),
                            ks.test(as.numeric(is.na(ama_racism$timing_knowledge_page_submit)),as.numeric(is.na(no_ama_racism$timing_knowledge_page_submit)))$p.value)

attrited_links = c(sum(is.na(treated$timing_link_choice_page_submit)),sum(is.na(placebo$timing_link_choice_page_submit)),
                   "",
                   sum(is.na(ama_racism$timing_link_choice_page_submit)),sum(is.na(no_ama_racism$timing_link_choice_page_submit)),
                   "")
attrited_links_perc = c(paste0(format(round(100*sum(is.na(treated$timing_link_choice_page_submit))/nrow(treated), digits=1), nsmall = 1),"%"),
                            paste0(format(round(100*sum(is.na(placebo$timing_link_choice_page_submit))/nrow(placebo), digits=1), nsmall = 1),"%"),
                        ks.test(as.numeric(is.na(treated$timing_link_choice_page_submit)),as.numeric(is.na(placebo$timing_link_choice_page_submit)))$p.value,
                        paste0(format(round(100*sum(is.na(ama_racism$timing_link_choice_page_submit))/nrow(ama_racism), digits=1), nsmall = 1),"%"),
                        paste0(format(round(100*sum(is.na(no_ama_racism$timing_link_choice_page_submit))/nrow(no_ama_racism), digits=1), nsmall = 1),"%"),
                        ks.test(as.numeric(is.na(ama_racism$timing_link_choice_page_submit)),as.numeric(is.na(no_ama_racism$timing_link_choice_page_submit)))$p.value
                        )

lines = c(lines, attrited_knowledge,attrited_knowledge_perc,attrited_links,attrited_links_perc)

# follow up :

treated_follow = treated[is.na(treated$not_to_recontact), ]
placebo_follow = placebo[is.na(placebo$not_to_recontact), ]
ama_racism_follow = ama_racism[is.na(ama_racism$not_to_recontact),]
no_ama_racism_follow = no_ama_racism[is.na(no_ama_racism$not_to_recontact),]

lines = c(lines,nrow(treated_follow),nrow(placebo_follow),"",nrow(ama_racism_follow),nrow(no_ama_racism_follow),"")

attrited_safety = c(sum(is.na(treated_follow$timing_safety_page_submit_followup)),sum(is.na(placebo_follow$timing_safety_page_submit_followup)),"",
                    sum(is.na(ama_racism_follow$timing_safety_page_submit_followup)),sum(is.na(no_ama_racism_follow$timing_safety_page_submit_followup)),"")


attrited_safety_perc = c(paste0(format(round(100*sum(is.na(treated_follow$timing_safety_page_submit_followup))/nrow(treated_follow ), digits=1), nsmall = 1),"%"),
                         paste0(format(round(100*sum(is.na(placebo_follow$timing_safety_page_submit_followup))/nrow(placebo_follow ), digits=1), nsmall = 1),"%"),
                         ks.test(as.numeric(is.na(treated_follow$timing_safety_page_submit_followup)),as.numeric(is.na(placebo_follow$timing_safety_page_submit_followup)))$p.value,
                         paste0(format(round(100*sum(is.na(ama_racism_follow$timing_safety_page_submit_followup))/nrow(ama_racism_follow ), digits=1), nsmall = 1),"%"),
                         paste0(format(round(100*sum(is.na(no_ama_racism_follow$timing_safety_page_submit_followup))/nrow(no_ama_racism_follow), digits=1), nsmall = 1),"%"),
                         ks.test(as.numeric(is.na(ama_racism_follow$timing_safety_page_submit_followup)),as.numeric(is.na(no_ama_racism_follow$timing_safety_page_submit_followup)))$p.value)

lines = c(lines, attrited_safety)
lines = c(lines, attrited_safety_perc)

attrited_know_follow = c(sum(is.na(treated_follow$timing_knowledge_page_submit_followup)),sum(is.na(placebo_follow$timing_knowledge_page_submit_followup)),"",
                    sum(is.na(ama_racism_follow$timing_knowledge_page_submit_followup)),sum(is.na(no_ama_racism_follow$timing_knowledge_page_submit_followup)),"")


attrited_safety_perc = c(paste0(format(round(100*sum(is.na(treated_follow$timing_knowledge_page_submit_followup))/nrow(treated_follow ), digits=1), nsmall = 1),"%"),
                         paste0(format(round(100*sum(is.na(placebo_follow$timing_knowledge_page_submit_followup))/nrow(placebo_follow ), digits=1), nsmall = 1),"%"),
                         ks.test(as.numeric(is.na(treated_follow$timing_knowledge_page_submit_followup)),as.numeric(is.na(placebo_follow$timing_knowledge_page_submit_followup)))$p.value,
                         paste0(format(round(100*sum(is.na(ama_racism_follow$timing_knowledge_page_submit_followup))/nrow(ama_racism_follow ), digits=1), nsmall = 1),"%"),
                         paste0(format(round(100*sum(is.na(no_ama_racism_follow$timing_knowledge_page_submit_followup))/nrow(no_ama_racism_follow), digits=1), nsmall = 1),"%"),
                         ks.test(as.numeric(is.na(ama_racism_follow$timing_knowledge_page_submit_followup)),as.numeric(is.na(no_ama_racism_follow$timing_knowledge_page_submit_followup)))$p.value)

lines = c(lines, attrited_know_follow)
lines = c(lines, attrited_safety_perc)


tab = t(matrix(lines, nrow=6))



colnames(tab) <- c("Any intervention", "Placebo","p-val KS-test","AMA anti-racism", "AMA placebo","p-val KS-test")
rownames(tab) = c("N Sample size at first randomization", "N attrited by knowledge question","%","N attrited by links","%","N of people we attempted to follow up with","N attrited by safety questions","%","N attrited by knowledge questions","%")
tab
# save Summary Table
write.csv(tab, paste0("./Output/attrition_table.csv"))
