# Install required packages 
# install.packages("remotes")
# library(remotes)
# install_version("ggplot2", version = "3.4.4");install_version("dplyr", version = "1.1.4")
# install_version("estimatr", version = "2.5.4");install_version("stringr", version = "5.2.3")
# install_version("lubridate", version = "1.2-12");install_version("ggpubr", version = "3.1-0")
# install_version("ggpubr", version = "1.2-12");install_version("tidyr", version = "3.1-0")
# install_version("stargazer", version = "5.2.3")
library(ggplot2)
library(dplyr)
library(estimatr)
library(stringr)
library(lubridate)
library(ggpubr)
library(tidyr) 
library(stargazer)
# List all objects in the environment
all_objects <- ls()

# Specify the objects to keep
objects_to_keep <- c("lib", 'wd')

# Identify objects to remove
objects_to_remove <- setdiff(all_objects, objects_to_keep)
rm(list = objects_to_remove)


# Set working directory
setwd(wd)


# Load data with treatment assignment
dta = read.csv("treatment_assignment.csv")
dta = dta[as.integer(rownames(dta))<=31,]

tab.out = "output/tables/"
fig.out = "output/figures/"

# Calculate number of male-female assigned
ac = colnames(dta)[-1]

ac.dta = data.frame(ac.name = ac, 
                   assign.mean = apply(dta[,2:ncol(dta)], MARGIN = 2, 
                                       FUN = function(x) mean(x, na.rm = T)))
ac.dta$female.treat = ifelse(ac.dta$assign.mean > 5, 1, 0)
sum(ac.dta$female.treat, na.rm = T)
ac.dta$no.treat1 = apply(dta[,2:ncol(dta)], MARGIN = 2, 
                         FUN = function(x) sum((x == 1|x== 6), na.rm = T))
ac.dta$no.treat2 = apply(dta[,2:ncol(dta)], MARGIN = 2, 
                         FUN = function(x) sum((x == 2|x== 7), na.rm = T))
ac.dta$no.treat3 = apply(dta[,2:ncol(dta)], MARGIN = 2, 
                         FUN = function(x) sum((x == 3|x== 8), na.rm = T))
ac.dta$no.treat4 = apply(dta[,2:ncol(dta)], MARGIN = 2, 
                         FUN = function(x) sum((x == 4|x== 9), na.rm = T))
ac.dta$no.treat5 = apply(dta[,2:ncol(dta)], MARGIN = 2, 
                         FUN = function(x) sum((x == 5|x== 10), na.rm = T))


##### Compliance with random treatment assignment
comp = read.csv("compliance.csv")

# Correct assembly name spellings
comp = comp %>%
  mutate(Assembly_name = ifelse(Assembly_name=="Jamshedpur East", "Jamshedpur.East", Assembly_name)) %>%
  mutate(Assembly_name = ifelse(Assembly_name=="Jamshedpur West", "Jamshedpur.West", Assembly_name)) %>%
  mutate(Assembly_name = ifelse(Assembly_name=="Jarmundih", "Jarmundi", Assembly_name)) %>%
  mutate(Assembly_name = ifelse(Assembly_name=="Khijari", "Khijri", Assembly_name)) %>%
  mutate(Assembly_name = ifelse(Assembly_name=="Jama" & Assembly_ID==32, "Jamua", Assembly_name))
  



# Figure out which assemblies had received our experiment
ac.comp = unique(comp$Assembly_name)
(unique(ac.dta$ac.name) %in% ac.comp) %>% sum()

#ac.dta$insamp = ifelse(ac.dta$ac.name %in% ac.comp, 1, 0)
ac.dta$insamp = ifelse(ac.dta$ac.name %in% ac.comp, 1, 0)

# Create a histogram of treatment assignment
comp = comp %>% group_by(Assembly_name) %>% 
  mutate(insamp.treat1 = sum(Treatment_assigned == 1| Treatment_assigned == 6),
         insamp.treat2 = sum(Treatment_assigned == 2| Treatment_assigned == 7),
         insamp.treat3 = sum(Treatment_assigned == 3| Treatment_assigned == 8),
         insamp.treat4 = sum(Treatment_assigned == 4| Treatment_assigned == 9),
         insamp.treat5 = sum(Treatment_assigned == 5| Treatment_assigned == 10))

comp.dta = aggregate(comp[,c(1, 10:14)], by = list(comp$Assembly_name), FUN = mean)
comp.dta$Assembly_name = comp.dta[,1]
comp.dta = comp.dta[,-1]

comp$female.treat.assigned = ifelse(comp$Treatment_assigned > 5, 1, 0)
comp$female.treat.takeup = ifelse(comp$Pamphlet_ID_final > 5, 1, 0)

ac.dta = merge(ac.dta, comp.dta, by.x = "ac.name", by.y = "Assembly_name",
               all.x = TRUE)




# Reshape data to Assembly constituency - treatment observations
ac.long = reshape(ac.dta[,1:9], varying = 4:8, times = "no.treat", direction = "long")
ac.long = ac.long[order(ac.long$ac.name),]
ac.long$treat.labels = factor(ac.long$time, labels = c("Candidacy", "Career", "Ideology", "Policy", "Baseline"))

ac.long.insamp = reshape(ac.dta[,c(1:3, 9, 10:14)], varying = 5:9, times = "insamp.treat", direction = "long")
ac.long.insamp = ac.long.insamp[order(ac.long.insamp$ac.name),]
ac.long.insamp$treat.labels = factor(ac.long.insamp$time, labels = c("Candidacy", "Career", "Ideology", "Policy", "Baseline"))
names(ac.long.insamp) = c("ac.name", "assign.mean", "female.treat", "insamp.treat",
                             "time", "id", "treat.labels")

ac.long.fin = merge(ac.long, ac.long.insamp[,c("ac.name","time" ,"insamp.treat")], by = c("ac.name","time"))


################# TABLE 3 ##########################
# Create a matrix for treatment takeup at the assembly level
# Number of assemblies
treat.tally = ac.long.fin[ac.long.fin$insamp==1,] %>% 
  group_by(ac.name) %>% 
  summarise_at(vars(female.treat), .funs = mean, na.rm = T) %>%
  group_by(female.treat) %>%
  tally() %>%
  mutate(female.treat = ifelse(female.treat==0, "Male", "Female")) %>%
  pivot_wider(names_from = female.treat, values_from = n)

# Number of VPs
treat.tally.vps = ac.long.fin[ac.long.fin$insamp==1,] %>% 
  group_by(female.treat) %>% 
  tally(insamp.treat) %>%
  mutate(female.treat = ifelse(female.treat==0, "Male", "Female")) %>%
  pivot_wider(names_from = female.treat, values_from = n)

T1 = bind_cols(treatment_type=c("Assemblies (T1)"), recruitment_message = "", treat.tally) %>%
  bind_rows(bind_cols(treatment_type=c("Vice Presidents (T1)"), recruitment_message = "", treat.tally.vps))



# Create matrix for treatment takeup
treat.tally.t2 = ac.long.fin[ac.long.fin$insamp==1,] %>% group_by(female.treat, treat.labels) %>% tally(insamp.treat)
treat.tally.t2 = treat.tally.t2 %>%
  mutate(female.treat = ifelse(female.treat==0, "Male", "Female")) %>%
  pivot_wider(names_from = female.treat, values_from = n) %>%
  rename(recruitment_message = treat.labels)

tab3 = bind_rows(T1, bind_cols(treatment_type = c("Vice Presidents (T2)"), treat.tally.t2))

stargazer(tab3, summary = F, title = "Random Assignment", out = paste0(tab.out, "table3_randomization_insample.tex"))



########### Table E.3: Compliance by Female Treatment and Message Types #################

comp = comp %>%
  mutate(message_treat_assigned = case_when(
    Treatment_assigned==1 | Treatment_assigned==6 ~ 1, 
    Treatment_assigned==2 | Treatment_assigned==7 ~ 2, 
    Treatment_assigned==3 | Treatment_assigned==8 ~ 3, 
    Treatment_assigned==4 | Treatment_assigned==9 ~ 4, 
    Treatment_assigned==5 | Treatment_assigned==10 ~ 5)) %>%
  mutate(message_treat_takeup = case_when(
    Pamphlet_ID_final==1 | Pamphlet_ID_final==6 ~ 1, 
    Pamphlet_ID_final==2 | Pamphlet_ID_final==7 ~ 2, 
    Pamphlet_ID_final==3 | Pamphlet_ID_final==8 ~ 3, 
    Pamphlet_ID_final==4 | Pamphlet_ID_final==9 ~ 4, 
    Pamphlet_ID_final==5 | Pamphlet_ID_final==10 ~ 5))


### COLUMN 1

comp = comp %>%
  mutate(complied = ifelse(message_treat_assigned==message_treat_takeup, 1, 0)) %>%
  mutate(complied_interact = ifelse(Treatment_assigned==Pamphlet_ID_final, 1, 0)) %>%
  mutate(complied_female = ifelse(female.treat.assigned==female.treat.takeup, 1, 0))


comply_fem = lm_robust(complied_female ~ as.factor(female.treat.assigned)-1, data = comp, 
                   se_type = "stata", cluster = Assembly_name)
h1 = lh_robust(complied_female ~ as.factor(female.treat.assigned)-1, data = comp, 
               se_type = "stata", cluster = Assembly_name, 
               linear_hypothesis = "as.factor(female.treat.assigned)0 = as.factor(female.treat.assigned)1")
### Save table
bind_rows(tidy(comply_fem), tidy(h1$lh)) %>%
  mutate(term = case_when(
    term=="as.factor(female.treat.assigned)0" ~ "Baseline",
    term=="as.factor(female.treat.assigned)1" ~ "Female Treatment",
    term=="as.factor(female.treat.assigned)0 = as.factor(female.treat.assigned)1" ~ "Baseline-Female Treatment = 0")) %>%
  dplyr::select(term, estimate, std.error, p.value) %>%
  mutate(estimate = ifelse(term=="Baseline-Female Treatment = 0", NA, estimate) %>% round(3)) %>%
  mutate(std.error = ifelse(term=="Baseline-Female Treatment = 0", NA, std.error) %>% round(3)) %>%
  mutate(p.value = round(p.value, 3)) %>%
  write.csv(paste0(tab.out, "tableE3_compliance_female_treat.csv"))


#### COLUMN 2
comply = lm_robust(complied ~ as.factor(message_treat_assigned)-1, data = comp, 
                   se_type = "stata", cluster = Assembly_name)
h1 = lh_robust(complied ~ as.factor(message_treat_assigned)-1, data = comp, 
          se_type = "stata", cluster = Assembly_name, 
          linear_hypothesis = "as.factor(message_treat_assigned)1 - as.factor(message_treat_assigned)2 = 0")
h2 = lh_robust(complied ~ as.factor(message_treat_assigned)-1, data = comp, 
          se_type = "stata", cluster = Assembly_name, 
          linear_hypothesis = "as.factor(message_treat_assigned)1 - as.factor(message_treat_assigned)3 = 0")
h3 = lh_robust(complied ~ as.factor(message_treat_assigned)-1, data = comp, 
          se_type = "stata", cluster = Assembly_name, 
          linear_hypothesis = "as.factor(message_treat_assigned)1 - as.factor(message_treat_assigned)4 = 0")
h4 = lh_robust(complied ~ as.factor(message_treat_assigned)-1, data = comp, 
          se_type = "stata", cluster = Assembly_name, 
          linear_hypothesis = "as.factor(message_treat_assigned)1 - as.factor(message_treat_assigned)5 = 0")
h5 = lh_robust(complied ~ as.factor(message_treat_assigned)-1, data = comp, 
               se_type = "stata", cluster = Assembly_name, 
               linear_hypothesis = "as.factor(message_treat_assigned)2 - as.factor(message_treat_assigned)3 = 0")
h6 = lh_robust(complied ~ as.factor(message_treat_assigned)-1, data = comp, 
               se_type = "stata", cluster = Assembly_name, 
               linear_hypothesis = "as.factor(message_treat_assigned)2 - as.factor(message_treat_assigned)4 = 0")
h7 = lh_robust(complied ~ as.factor(message_treat_assigned)-1, data = comp, 
               se_type = "stata", cluster = Assembly_name, 
               linear_hypothesis = "as.factor(message_treat_assigned)2 - as.factor(message_treat_assigned)5 = 0")
h8 = lh_robust(complied ~ as.factor(message_treat_assigned)-1, data = comp, 
               se_type = "stata", cluster = Assembly_name, 
               linear_hypothesis = "as.factor(message_treat_assigned)3 - as.factor(message_treat_assigned)4 = 0")
h9 = lh_robust(complied ~ as.factor(message_treat_assigned)-1, data = comp, 
               se_type = "stata", cluster = Assembly_name, 
               linear_hypothesis = "as.factor(message_treat_assigned)3 - as.factor(message_treat_assigned)5 = 0")
h10 = lh_robust(complied ~ as.factor(message_treat_assigned)-1, data = comp, 
              se_type = "stata", cluster = Assembly_name, 
              linear_hypothesis = "as.factor(message_treat_assigned)4 - as.factor(message_treat_assigned)5 = 0")

## Save table

bind_rows(tidy(comply), tidy(h1$lh), tidy(h2$lh), tidy(h3$lh), tidy(h4$lh), tidy(h5$lh),
          tidy(h6$lh), tidy(h7$lh), tidy(h8$lh), tidy(h9$lh), tidy(h10$lh)) %>%
  mutate(term = case_when(
    term=="as.factor(message_treat_assigned)5" ~ "Baseline",
    term=="as.factor(message_treat_assigned)4" ~ "Policy",
    term=="as.factor(message_treat_assigned)3" ~ "Ideology", 
    term=="as.factor(message_treat_assigned)2" ~ "Career", 
    term=="as.factor(message_treat_assigned)1" ~ "Candidacy",
    term=="as.factor(message_treat_assigned)3 - as.factor(message_treat_assigned)5 = 0" ~ "Ideology - Baseline",
    term=="as.factor(message_treat_assigned)1 - as.factor(message_treat_assigned)3 = 0" ~ "Ideology - Candidacy",
    term=="as.factor(message_treat_assigned)3 - as.factor(message_treat_assigned)4 = 0" ~ "Ideology - Policy",
    term=="as.factor(message_treat_assigned)2 - as.factor(message_treat_assigned)3 = 0" ~ "Ideology - Career",
    term=="as.factor(message_treat_assigned)1 - as.factor(message_treat_assigned)5 = 0" ~ "Candidacy - Baseline",
    term=="as.factor(message_treat_assigned)1 - as.factor(message_treat_assigned)4 = 0" ~ "Candidacy - Policy",
    term=="as.factor(message_treat_assigned)1 - as.factor(message_treat_assigned)2 = 0" ~ "Candidacy - Career",
    term=="as.factor(message_treat_assigned)4 - as.factor(message_treat_assigned)5 = 0" ~ "Policy - Baseline",
    term=="as.factor(message_treat_assigned)2 - as.factor(message_treat_assigned)4 = 0" ~ "Policy - Career",
    term=="as.factor(message_treat_assigned)2 - as.factor(message_treat_assigned)5 = 0" ~ "Career - Baseline")) %>%
  dplyr::select(term, estimate, std.error, p.value) %>%
  mutate(estimate = ifelse(str_detect(string = term, pattern = " - "), NA, estimate) %>% round(3)) %>%
  mutate(std.error = ifelse(str_detect(string = term, " - "), NA, std.error) %>% round(3)) %>%
  mutate(p.value = round(p.value, 3)) %>%
  write.csv(paste0(tab.out, "tableE3_compliance_messages.csv"))


## Column 3
comply2 = lm_robust(complied_interact ~ as.factor(Treatment_assigned)-1, data = comp, 
                   se_type = "stata", cluster = Assembly_name)

h1 = lh_robust(complied_interact ~ as.factor(Treatment_assigned)-1, data = comp, 
               se_type = "stata", cluster = Assembly_name, 
               linear_hypothesis = "as.factor(Treatment_assigned)5 - as.factor(Treatment_assigned)10 = 0")
h2 = lh_robust(complied_interact ~ as.factor(Treatment_assigned)-1, data = comp, 
               se_type = "stata", cluster = Assembly_name, 
               linear_hypothesis = "as.factor(Treatment_assigned)5 - as.factor(Treatment_assigned)1 = 0")
h3 = lh_robust(complied_interact ~ as.factor(Treatment_assigned)-1, data = comp, 
               se_type = "stata", cluster = Assembly_name, 
               linear_hypothesis = "as.factor(Treatment_assigned)5 - as.factor(Treatment_assigned)2 = 0")
h4 = lh_robust(complied_interact ~ as.factor(Treatment_assigned)-1, data = comp, 
               se_type = "stata", cluster = Assembly_name, 
               linear_hypothesis = "as.factor(Treatment_assigned)5 - as.factor(Treatment_assigned)3 = 0")
h5 = lh_robust(complied_interact ~ as.factor(Treatment_assigned)-1, data = comp, 
               se_type = "stata", cluster = Assembly_name, 
               linear_hypothesis = "as.factor(Treatment_assigned)5 - as.factor(Treatment_assigned)4 = 0")
h6 = lh_robust(complied_interact ~ as.factor(Treatment_assigned)-1, data = comp, 
               se_type = "stata", cluster = Assembly_name, 
               linear_hypothesis = "as.factor(Treatment_assigned)10 - as.factor(Treatment_assigned)6 = 0")
h7 = lh_robust(complied_interact ~ as.factor(Treatment_assigned)-1, data = comp, 
               se_type = "stata", cluster = Assembly_name, 
               linear_hypothesis = "as.factor(Treatment_assigned)10 - as.factor(Treatment_assigned)7 = 0")
h8 = lh_robust(complied_interact ~ as.factor(Treatment_assigned)-1, data = comp, 
               se_type = "stata", cluster = Assembly_name, 
               linear_hypothesis = "as.factor(Treatment_assigned)10 - as.factor(Treatment_assigned)8 = 0")
h9 = lh_robust(complied_interact ~ as.factor(Treatment_assigned)-1, data = comp, 
               se_type = "stata", cluster = Assembly_name, 
               linear_hypothesis = "as.factor(Treatment_assigned)10 - as.factor(Treatment_assigned)9 = 0")



bind_rows(tidy(comply2),
          tidy(h1$lh), tidy(h2$lh), tidy(h3$lh), tidy(h4$lh), tidy(h5$lh),
          tidy(h6$lh), tidy(h7$lh), tidy(h8$lh), tidy(h9$lh)) %>%
  mutate(term = case_when(
    term=="as.factor(Treatment_assigned)10" ~ "Baseline (F)",
    term=="as.factor(Treatment_assigned)9" ~ "Policy (F)",
    term=="as.factor(Treatment_assigned)8" ~ "Ideology (F)",
    term=="as.factor(Treatment_assigned)7" ~ "Career (F)",
    term=="as.factor(Treatment_assigned)6" ~ "Candidacy (F)",
    term=="as.factor(Treatment_assigned)5" ~ "Baseline (M)",
    term=="as.factor(Treatment_assigned)4" ~ "Policy (M)",
    term=="as.factor(Treatment_assigned)3" ~ "Ideology (M)", 
    term=="as.factor(Treatment_assigned)2" ~ "Career (M)", 
    term=="as.factor(Treatment_assigned)1" ~ "Candidacy (M)",
  term=="as.factor(Treatment_assigned)5 - as.factor(Treatment_assigned)10 = 0" ~ "Baseline (M) - Baseline (F)",
  term=="as.factor(Treatment_assigned)5 - as.factor(Treatment_assigned)1 = 0" ~ "Baseline (M) - Candidacy (M)",
  term=="as.factor(Treatment_assigned)5 - as.factor(Treatment_assigned)2 = 0" ~ "Baseline (M) - Career (M)",
  term=="as.factor(Treatment_assigned)5 - as.factor(Treatment_assigned)3 = 0" ~ "Baseline (M) - Ideology (M)",
  term=="as.factor(Treatment_assigned)5 - as.factor(Treatment_assigned)4 = 0" ~ "Baseline (M) - Policy (M)",
  term=="as.factor(Treatment_assigned)10 - as.factor(Treatment_assigned)6 = 0" ~ "Baseline (F) - Candidacy (F)",
  term=="as.factor(Treatment_assigned)10 - as.factor(Treatment_assigned)7 = 0" ~ "Baseline (F) - Career (F)",
  term=="as.factor(Treatment_assigned)10 - as.factor(Treatment_assigned)8 = 0" ~ "Baseline (F) - Ideology (F)",
  term=="as.factor(Treatment_assigned)10 - as.factor(Treatment_assigned)9 = 0" ~ "Baseline (F) - Policy (F)")) %>%
 dplyr::select(term, estimate, std.error, p.value) %>%
  mutate(estimate = ifelse(str_detect(string = term, pattern = " - "), NA, estimate) %>% round(3)) %>%
  mutate(std.error = ifelse(str_detect(string = term, " - "), NA, std.error) %>% round(3)) %>%
  mutate(p.value = round(p.value, 3)) %>% 
  write.csv(paste0(tab.out, "tableE3_compliance_messages_interact.csv"))



########### Table E.4: Patterns in Non-Compliance #################

### now figure out if there is a systematic way some of the messages are not being complied with
## Focus on the VPs that are not complying
non_compliers <- comp %>%
  filter(complied==0 | complied_female==0 | complied_interact ==0)

non_compliance_pattern = as.data.frame.matrix(table(non_compliers$Treatment_assigned, non_compliers$Pamphlet_ID_final))
colnames(non_compliance_pattern) = c("switched_to_1", "switched_to_2",  "switched_to_3", "switched_to_4",  
                                    "switched_to_6", "switched_to_7", "switched_to_8", "switched_to_9", "switched_to_10")
non_compliance_pattern$assigned_treat = c("Candidacy (M)", "Ideology (M)", "Policy (M)", "Baseline (M)",
                                          "Candidacy (F)", "Career (F)", "Ideology (F)", "Policy (F)", "Baseline (F)")
non_compliance_pattern <- non_compliance_pattern %>%
  dplyr::select(assigned_treat, starts_with("switched_to"))
colnames(non_compliance_pattern) <- c("assigned_treatment", "Candidacy (M)", 
                                      "Career (M)", "Ideology (M)", "Policy (M)",
                                      "Candidacy (F)", "Career (F)", "Ideology (F)", 
                                      "Policy (F)", "Baseline (F)")



write.csv(non_compliance_pattern, paste0(tab.out, "tableE4_non_compliance_pattern.csv"))


################ FIGURE E7: TIMELINE OF DISTRIBUTION #################################
# List all objects in the environment
all_objects <- ls()

# Specify the objects to keep
objects_to_keep <- c("lib", 'wd')

# Identify objects to remove
objects_to_remove <- setdiff(all_objects, objects_to_keep)
rm(list = objects_to_remove)
tab.out = "output/tables/"
fig.out = "output/figures/"
comp = read.csv("compliance_timeline.csv")
comp = comp[-nrow(comp),]

# add ID to vps
comp <- comp %>%
  mutate(vp_id_new = 1:nrow(comp))


# Create message var
comp = comp %>%
  mutate(message_treat_assigned = case_when(
    Treatment_assigned==1 | Treatment_assigned==6 ~ 1, 
    Treatment_assigned==2 | Treatment_assigned==7 ~ 2, 
    Treatment_assigned==3 | Treatment_assigned==8 ~ 3, 
    Treatment_assigned==4 | Treatment_assigned==9 ~ 4, 
    Treatment_assigned==5 | Treatment_assigned==10 ~ 5)) %>%
  mutate(message_treat_takeup = case_when(
    Pamphlet_ID_final==1 | Pamphlet_ID_final==6 ~ 1, 
    Pamphlet_ID_final==2 | Pamphlet_ID_final==7 ~ 2, 
    Pamphlet_ID_final==3 | Pamphlet_ID_final==8 ~ 3, 
    Pamphlet_ID_final==4 | Pamphlet_ID_final==9 ~ 4, 
    Pamphlet_ID_final==5 | Pamphlet_ID_final==10 ~ 5))





comp_clean <- comp %>%
  dplyr::select(vp_id_new, Assembly_name, Pamphlet_ID_final, Treatment_assigned, Pamphlet_received,
                date1, distributed1, date2, distributed2, date3, distributed3, 
                date4, distributed4, date5, distributed5, date6, distributed6)

# Clean compliance data
comp_clean <- comp_clean %>%
  mutate(distributed1 = ifelse(distributed1=="His mom no more", '0', distributed1),
         distributed1 = ifelse(distributed1=="", '0', distributed1),
         distributed2 = ifelse(distributed2=="His mom no more", '0', distributed2),
         distributed3 = ifelse(distributed3=="His mom no more", '0', distributed3),
         distributed4 = ifelse(distributed3=="His mom no more", '0', distributed4)) %>%
  mutate(distributed1 = as.numeric(distributed1)) %>%
  mutate(distributed2 = as.numeric(distributed2)) %>%
  mutate(distributed3 = as.numeric(distributed3)) %>%
  mutate(distributed4 = as.numeric(distributed4)) %>%
  mutate(distributed5 = as.numeric(distributed5)) %>%
  mutate(distributed6 = as.numeric(distributed6)) %>%
  mutate(distributed1 = ifelse(is.na(distributed1), 0, distributed1),
         distributed2 = ifelse(is.na(distributed2), 0, distributed2),
         distributed3 = ifelse(is.na(distributed3), 0, distributed3),
         distributed4 = ifelse(is.na(distributed4), 0, distributed4),
         distributed5 = ifelse(is.na(distributed5), 0, distributed5))

comp_clean <- comp_clean %>%
  mutate(date0 = "8/30/18") %>%
  mutate(distributed0 = 0) %>%
  mutate(date1 = "9/10/18") %>%
  mutate(date2 = "9/14/18") %>%
  mutate(date3 = "9/20/18") %>%
  mutate(date4 = "9/27/18") %>%
  mutate(date5 = "10/15/18") %>%
  mutate(date6 = "12/15/18") 




comp1 = pivot_longer(comp_clean %>% dplyr::select(-starts_with("distributed")),
                     cols = starts_with(c("date")), names_to = c("date")) %>%
  mutate(row = str_remove(date, "date"))
comp2 = pivot_longer(comp_clean %>% dplyr::select(-starts_with("date")),
                     cols = starts_with(c("distributed")), names_to = c("distributed"))%>%
  mutate(row = str_remove(distributed, "distributed"))

comp_clean_l <- comp1 %>%
  dplyr::select(-date) %>%
  rename(date = value) %>%
  left_join(comp2 %>% dplyr::select(vp_id_new, value, row) %>%
              rename(distributed = value)) %>%
  mutate(
    messages = case_when(
      Treatment_assigned==1|Treatment_assigned==6 ~ "Candidacy",
      Treatment_assigned==2|Treatment_assigned==7 ~ "Career",
      Treatment_assigned==3|Treatment_assigned==8 ~ "Ideology",
      Treatment_assigned==4|Treatment_assigned==9 ~ "Policy",
      Treatment_assigned==5|Treatment_assigned==10 ~ "Baseline"
    ),
    female_treat = ifelse(Treatment_assigned>5, "Female", "Male"))





# create distribution timeline by treatment
pamphlets <- comp_clean_l %>%
  group_by(vp_id_new, Pamphlet_ID_final, messages) %>%
  summarise_at(vars(Pamphlet_received), .funs = max) 


comp_sum <- comp_clean_l %>%
  mutate(date_cl = mdy(date)) %>%
  group_by(vp_id_new, messages, date_cl) %>%
  summarise_at(vars(distributed), .funs = sum) %>%
  left_join(pamphlets) 

# calculate the amount of pamphlets distributed in a certain period
comp_sum <- comp_sum %>%
  group_by(vp_id_new) %>%
  mutate(new_pamphlet_distributed = distributed-dplyr::lag(distributed, 1)) %>%
  mutate(new_pamphlet_distributed = ifelse(is.na(new_pamphlet_distributed), 0, new_pamphlet_distributed))



# create mean and standard deviation for these groups
comp_fig <- comp_sum %>%
  group_by(messages, date_cl) %>%
  summarise_at(vars(new_pamphlet_distributed), .funs = mean, na.rm = T)




# plot - messages

comp_fig = comp_fig %>%
  mutate(messages_num = case_when (
    messages=="Baseline" ~ 1, 
    messages=="Ideology" ~ 2, 
    messages=="Candidacy" ~ 3,
    messages=="Policy" ~ 4,
    messages=="Career" ~ 5
  ))


comp_fig$messages_fact = factor(comp_fig$messages_num, labels = c("Basel.", "Ideo.", "Cand.", "Pol.", "Car."))
ggplot(comp_fig, aes(x = date_cl, y = new_pamphlet_distributed))+
  geom_point(aes(shape = messages_fact, color = messages_fact), 
             size = 4, alpha = 0.7)+
  theme_pubr()+
  guides(color = guide_legend(title = "Message:"), 
         shape = guide_legend(title = "Message:"))+
  scale_x_date(date_breaks = "2 week", date_labels = "%b %d")+
  labs(x = "Date (2018)", y = "Number of New Pamphlets Distributed")+
  theme(legend.text = element_text(size = 14), 
        legend.title = element_text(size = 14), 
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))

ggsave(paste0(fig.out, "figureE7_compliance_timeline_messages.pdf"),
       width = 8.49, height = 6.4, dpi = 1000)  

################ FIGURE E6: TIMELINE OF DISTRIBUTION #################################

# plot - female encouragement treatment
pamphlets <- comp_clean_l %>%
  group_by(vp_id_new, Pamphlet_ID_final, female_treat) %>%
  summarise_at(vars(Pamphlet_received), .funs = max) 

comp_sum <- comp_clean_l %>%
  mutate(date_cl = mdy(date)) %>%
  group_by(vp_id_new, female_treat, date_cl) %>%
  summarise_at(vars(distributed), .funs = sum) %>%
  left_join(pamphlets) 

# calculate the amount of pamphlets distributed in a certain period
comp_sum <- comp_sum %>%
  group_by(vp_id_new) %>%
  mutate(new_pamphlet_distributed = distributed-dplyr::lag(distributed, 1)) %>%
  mutate(new_pamphlet_distributed = ifelse(is.na(new_pamphlet_distributed), 0, new_pamphlet_distributed))

comp_sum <- comp_sum %>% 
  mutate(fem_treat_fac = ifelse(female_treat=="Male", 1, 2)) 
comp_sum$fem_treat_fac = factor(comp_sum$fem_treat_fac, labels = c("Male", "Female"))
comp_sum$date_cl_fac = factor(comp_sum$date_cl, labels = c("2018-08-30", "2018-09-10",
                                                           "2018-09-14", "2018-09-20",
                                                           "2018-09-27", "2018-10-15",
                                                           "2018-12-15"))
# create mean and standard deviation for these groups
comp_fig <- comp_sum %>%
  group_by(female_treat, date_cl) %>%
  summarise_at(vars(new_pamphlet_distributed), .funs = mean, na.rm = T)

dif<- lm_robust(new_pamphlet_distributed ~ fem_treat_fac*date_cl_fac,
                data = comp_sum, se_type = "stata") %>% tidy() %>%
  filter(str_detect(term, "fem_treat_facFemale:")) %>%
  mutate(date_cl = str_replace(term, "fem_treat_facFemale:date_cl_fac", "")) %>%
  mutate(date_cl = ymd(date_cl)) %>%
  bind_rows(data.frame(
    date_cl = ymd("2018-08-30"), estimate = 0
  )) %>% arrange(date_cl)

comp_fig = comp_fig %>% bind_rows(dif %>% mutate(female_treat = "Difference") %>%
                                     mutate(new_pamphlet_distributed = estimate)) 
comp_fig = comp_fig %>%
  mutate(fem_treat_fac = case_when(
    female_treat=="Female" ~ 2,
    female_treat=="Male" ~ 3,
    female_treat=="Difference" ~ 1
  ))
comp_fig$fem_treat_fac = factor(comp_fig$female_treat, labels = c("Difference",  "Female", "Male"))


# plot - gender-inclusiveness
p <- ggplot(comp_fig, aes(x = date_cl, y = new_pamphlet_distributed))+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high))+
  geom_point(aes(color = as.factor(fem_treat_fac), shape = as.factor(fem_treat_fac)), 
             size = 3)+
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray48")+
  theme_pubr()+
  guides(color = guide_legend(title = "Treatment Types", reverse = T),
         shape = guide_legend(title = "Treatment Types", reverse = T))+
  scale_color_manual(values = c("black", "red", "blue"))+
  #scale_y_continuous(labels = scales::percent)+
  labs(x = "Date (2018)", y = "Number of Pamphlets Distributed")+
  theme(legend.text = element_text(size = 14), 
        legend.title = element_text(size = 14), 
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))
p
ggsave(paste0(fig.out, "figureE6_compliance_timeline_female.pdf"), 
       width = 8.49, height = 6.4, dpi = 1000)   









#################### FIGURE E5: EFFORT BY VPS ##################################
# List all objects in the environment
all_objects <- ls()

# Specify the objects to keep
objects_to_keep <- c("lib", 'wd')

# Identify objects to remove
objects_to_remove <- setdiff(all_objects, objects_to_keep)
rm(list = objects_to_remove)
tab.out = "output/tables/"
fig.out = "output/figures/"
vps <- read.csv("compliance.csv", stringsAsFactors = F) %>%
  filter(!is.na(ID))
vps$geo_number = NULL

geo_units = str_split(vps$GP_Ward_final, pattern = ",") %>%
  lapply(., FUN = function(x){length(x)}) %>%
  unlist()

vps$geo_number <- geo_units
vps <- vps %>%
  mutate(geo_number = ifelse(GP_Ward_final=="missing", NA_integer_, geo_number)) %>%
  mutate(geo_unit_new = ifelse(geo_unit=="colony"|geo_unit=="sector"|geo_unit=="ward", "Urban Area", geo_unit)) %>%
  mutate(geo_unit_new = ifelse(geo_unit=="GP", "Rural Area", geo_unit_new)) %>%
  group_by(geo_unit_new) %>%
  mutate(count_units = sum(geo_number, na.rm = T))
  


fig = vps %>%
  group_by(Treatment_assigned, geo_unit_new) %>%
  summarise_at(vars(geo_number), .funs = mean, na.rm = T) %>%
left_join(vps %>%
  group_by(Treatment_assigned, geo_unit_new) %>%
  summarise_at(vars(geo_number), .funs = sd, na.rm = T) %>%
  rename(sd = geo_number)) %>%
left_join(vps %>% 
  group_by(Treatment_assigned, geo_unit_new) %>%
  add_tally() %>%
  summarise_at(vars(n), .funs = mean, na.rm = T) %>%
  rename(count = n)) %>%
  mutate(se = sd/sqrt(count)) %>%
  filter(geo_unit_new!="" & geo_unit_new!="booths") %>%
  mutate(treatments = case_when(
    Treatment_assigned==1 ~ "Candidacy (M)",
    Treatment_assigned==2 ~ "Career (M)",
    Treatment_assigned==3 ~ "Ideology (M)",
    Treatment_assigned==4 ~ "Policy (M)",
    Treatment_assigned==5 ~ "Baseline (M)",
    Treatment_assigned==6 ~ "Candidacy (F)",
    Treatment_assigned==7 ~ "Career (F)",
    Treatment_assigned==8 ~ "Ideology (F)",
    Treatment_assigned==9 ~ "Policy (F)",
    Treatment_assigned==10 ~ "Baseline (F)"
  )) %>%
  mutate(treatments_f = case_when(
    treatments=="Baseline (M)" ~ 1,
    treatments=="Ideology (M)" ~ 2,
    treatments=="Candidacy (M)" ~ 3,
    treatments=="Policy (M)" ~ 4,
    treatments=="Career (M)" ~ 5,
    treatments=="Baseline (F)" ~ 6,
    treatments=="Ideology (F)" ~ 7,
    treatments=="Candidacy (F)" ~ 8,
    treatments=="Policy (F)" ~ 9,
    treatments=="Career (F)" ~ 10
  )) %>%
  mutate(
    text = case_when(
      treatments == "Candidacy (M)" & geo_unit_new=="Rural Area" ~ "Total Units: 1493",
      treatments == "Candidacy (M)" & geo_unit_new=="Urban Area" ~ "Total Units: 362"
    )
  ) %>%
  mutate(text_y_pos = case_when(
    treatments == "Candidacy (M)" & geo_unit_new=="Rural Area" ~ 9,
    treatments == "Candidacy (M)" & geo_unit_new=="Urban Area" ~ 9
  ))

fig$treatments_fac = factor(fig$treatments_f, labels = c("Baseline (M)", "Ideology (M)", "Candidacy (M)", "Policy (M)", "Career (M)",
                                                        "Baseline (F)", "Ideology (F)", "Candidacy (F)", "Policy (F)", "Career (F)"))
ggplot(fig, aes(x = treatments_fac))+
  geom_histogram(aes(y = geo_number), stat = "identity")+
  geom_errorbar(aes(y = geo_number, ymin = geo_number-1.96*se, ymax = geo_number+1.96*se), width = 0.2)+
  geom_text(aes(y = text_y_pos, label = text), hjust = -0.45) + 
  facet_grid(.~geo_unit_new)+
  theme_pubclean()+
  theme(axis.text.x  = element_text(angle = 90, size = 14),
        axis.title = element_text(size = 14), 
        strip.text = element_text(size = 14))+
  labs(x = "Treatment type", y = "Number of Units Assigned to VPs")
ggsave(paste0(fig.out, "figureE5_VP_geographic_assignment_desc.pdf"), 
       dpi = 1000, width = 9.81, height = 6.53)






