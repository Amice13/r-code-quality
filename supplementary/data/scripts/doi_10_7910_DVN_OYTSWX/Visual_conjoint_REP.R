
library(readxl)  
library(tidyverse)
library(cregg)

cj_1 <- choice ~  record + experience + party + ethnicity + gender + sexuality
cj_2 <- choice ~  record + experience +party + how_diverse + ethnicity + gender + sexuality
cj_3 <- choice ~  record + experience +how_diverse 
cj_4 <- choice ~  record + experience + ethnicity + gender + sexuality

############### 0 data curation ##################

#load the data
load("/Users/aloport/Dropbox/Research/faces_project/faces_clean.RData")

#subdivide the four experimental groups of each experiment
faces_experiment1 <- faces_clean %>% 
  filter(grepl('plicit', experimental_group)) %>% mutate(resp_Greens= as.factor(resp_Greens)) %>%
  mutate(gender= case_when(gender== "Nonbinary" ~ "Non-binary", TRUE ~ as.character(gender)), 
         how_diverse= factor(case_when(ethnicity== "Non-White" & gender== "Woman" & sexuality== "LGBAQ" ~ "Non-White woman LGB",
                                       ethnicity== "Non-White" & gender== "Non-binary" & sexuality== "LGBAQ" ~ "Non-White non-binary LGB",
                                       ethnicity== "Non-White" & gender== "Man" & sexuality== "LGBAQ" ~ "Non-White LGB",
                                       
                                       ethnicity== "Non-White" & gender== "Woman" ~ "Non-White woman",
                                       ethnicity== "Non-White" & gender== "Non-binary" ~ "Non-White non-binary",
                                       
                                       
                                       ethnicity== "White" & gender== "Woman" & sexuality== "LGBAQ"  ~ "Woman LGB",
                                       ethnicity== "White" & gender== "Non-binary" & sexuality== "LGBAQ"  ~ "Non-binary LGB",
                                       
                                       
                                       ethnicity== "Non-White" ~ "Non-White",
                                       sexuality== "LGBAQ" ~ "LGB",
                                       gender== "Woman" ~ "Woman",
                                       gender== "Non-binary" ~ "Non-binary",
                                       TRUE ~ "Non-diverse"),
                             levels= c("Non-diverse", "Woman", "LGB", "Non-binary", "Non-White", 
                                       "Woman LGB", "Non-binary LGB", "Non-White woman", "Non-White non-binary", "Non-White LGB",
                                       "Non-White woman LGB", "Non-White non-binary LGB"))) %>% 
  mutate(explicit_implicit= factor(case_when(experimental_group_1=="Explicit without parties" ~ "Explicit",
                                             experimental_group_1=="Explicit with parties" ~ "Explicit",
                                             experimental_group_1=="Implicit without parties" ~ "Implicit",
                                             experimental_group_1=="Implicit with parties" ~ "Implicit"
  ))) %>% 
  mutate(party_notparty= factor(case_when(experimental_group_1=="Explicit without parties" ~ "No party",
                                          experimental_group_1=="Explicit with parties" ~ "Party",
                                          experimental_group_1=="Implicit without parties" ~ "No party",
                                          experimental_group_1=="Implicit with parties" ~ "Party"
  ))) %>% 
  mutate(resp_leri_short= factor(case_when(resp_leri== "Right" ~ "Non-left",
                                           resp_leri== "Center" ~ " Non-left",
                                           resp_leri== "Left" ~ "Left"))) %>% 
  mutate(sexuality= factor(case_when(sexuality== "LGBAQ" ~ "LGB", TRUE ~ as.character(sexuality)), levels = c("Heterosexual", "LGB")),
         gender= factor(gender, levels = c("Man", "Woman", "Non-binary")),
         ethnicity= factor(ethnicity, levels = c("White", "Non-White")),
         party= factor(case_when(party== "Rightist CDU/CSU" ~ "Rightist CDU/CSU",
                                 party== "Centrist CDU/CSU"~ "Centrist CDU/CSU",
                                 party== "Centrist Greens"~ "Centrist Greens",
                                 party== "Leftist Greens"~ "Leftist Greens",
                                 TRUE~ NA_character_))) 




faces_experiment1_center <- faces_experiment1 %>%  filter(resp_leri=="Center")
faces_experiment1_left <- faces_experiment1 %>%  filter(resp_leri=="Left")
faces_experiment1_right <- faces_experiment1 %>%  filter(resp_leri=="Right")



#### mm WITH PARTY LABELS ####

faces_left_mm_party <- as_data_frame(cj(faces_experiment1_left|>  filter(party_notparty== "Party"), cj_1, id = ~id, estimate = "mm", by = ~explicit_implicit)) %>% mutate(group= "Left")

faces_center_mm_party  <- as_data_frame(cj(faces_experiment1_center|>  filter(party_notparty== "Party"),cj_1, id = ~id, estimate = "mm", by = ~explicit_implicit)) %>% mutate(group= "Center")

faces_right_mm_party  <- as_data_frame(cj(faces_experiment1_right|>  filter(party_notparty== "Party"), cj_1, id = ~id, estimate = "mm", by = ~explicit_implicit)) %>% mutate(group= "Right")
faces_all_mm_party  <- as_data_frame(cj(faces_experiment1|>  filter(party_notparty== "Party"), cj_1, id = ~id, estimate = "mm", by = ~explicit_implicit)) %>% mutate(group= "All")


faces_ideology_mm_party <- full_join(faces_left_mm_party, faces_center_mm_party) %>% full_join(., faces_right_mm_party) %>% 
  full_join(., faces_all_mm_party) %>% 
  mutate(group= factor(group, levels = c("All", "Left", "Center", "Right")), with= "With party",
         p_adjusted=p.adjust(p, method= "BH")) 


#### amce WITH PARTY LABELS ####

faces_left_amce_party <- as_data_frame(cj(faces_experiment1_left|>  filter(party_notparty== "Party"), cj_1, id = ~id, estimate = "amce", by = ~explicit_implicit)) %>% mutate(group= "Left")

faces_center_amce_party <- as_data_frame(cj(faces_experiment1_center|>  filter(party_notparty== "Party"),cj_1, id = ~id, estimate = "amce", by = ~explicit_implicit)) %>% mutate(group= "Center")

faces_right_amce_party <- as_data_frame(cj(faces_experiment1_right|>  filter(party_notparty== "Party"), cj_1, id = ~id, estimate = "amce", by = ~explicit_implicit)) %>% mutate(group= "Right")
faces_all_amce_party <- as_data_frame(cj(faces_experiment1|>  filter(party_notparty== "Party"), cj_1, id = ~id, estimate = "amce", by = ~explicit_implicit)) %>% mutate(group= "All")

faces_ideology_amce_party <- full_join(faces_left_amce_party, faces_center_amce_party) %>% full_join(., faces_right_amce_party) %>% 
  full_join(., faces_all_amce_party) %>% 
  mutate(group= factor(group, levels = c("All", "Left", "Center", "Right")), with= "With party",
         p_adjusted=p.adjust(p, method= "BH")) 

#### mm WITHOUT PARTY LABELS ####

faces_left_mm_noparty <- as_data_frame(cj(faces_experiment1_left|>  filter(party_notparty== "No party"), cj_4, id = ~id, estimate = "mm", by = ~explicit_implicit)) %>% mutate(group= "Left")

faces_center_mm_noparty <- as_data_frame(cj(faces_experiment1_center|>  filter(party_notparty== "No party"),cj_4, id = ~id, estimate = "mm", by = ~explicit_implicit)) %>% mutate(group= "Center")

faces_right_mm_noparty <- as_data_frame(cj(faces_experiment1_right|>  filter(party_notparty== "No party"), cj_4, id = ~id, estimate = "mm", by = ~explicit_implicit)) %>% mutate(group= "Right")
faces_all_mm_noparty <- as_data_frame(cj(faces_experiment1|>  filter(party_notparty== "No party"), cj_4, id = ~id, estimate = "mm", by = ~explicit_implicit)) %>% mutate(group= "All")


faces_ideology_mm_noparty <- full_join(faces_left_mm_noparty, faces_center_mm_noparty) %>% full_join(., faces_right_mm_noparty) %>% 
  full_join(., faces_all_mm_noparty) %>% 
  mutate(group= factor(group, levels = c("All", "Left", "Center", "Right")), with= "Without party",
         p_adjusted=p.adjust(p, method= "BH")) 


#### amce WITHOUT PARTY LABELS ####

faces_left_amce_noparty <- as_data_frame(cj(faces_experiment1_left|>  filter(party_notparty== "No party"), cj_4, id = ~id, estimate = "amce", by = ~explicit_implicit)) %>% mutate(group= "Left")

faces_center_amce_noparty <- as_data_frame(cj(faces_experiment1_center|>  filter(party_notparty== "No party"),cj_4, id = ~id, estimate = "amce", by = ~explicit_implicit)) %>% mutate(group= "Center")

faces_right_amce_noparty <- as_data_frame(cj(faces_experiment1_right|>  filter(party_notparty== "No party"), cj_4, id = ~id, estimate = "amce", by = ~explicit_implicit)) %>% mutate(group= "Right")
faces_all_amce_noparty <- as_data_frame(cj(faces_experiment1|>  filter(party_notparty== "No party"), cj_4, id = ~id, estimate = "amce", by = ~explicit_implicit)) %>% mutate(group= "All")


faces_ideology_amce_noparty<- full_join(faces_left_amce_noparty, faces_center_amce_noparty) %>% full_join(., faces_right_amce_noparty) %>% 
  full_join(., faces_all_amce_noparty) %>% 
  mutate(group= factor(group, levels = c("All", "Left", "Center", "Right")), with= "Without party",
         p_adjusted=p.adjust(p, method= "BH")) 

#### mm WITH and WITHOUT PARTY LABELS ####
faces_left_mm <- as_data_frame(cj(faces_experiment1_left, cj_4, id = ~id, estimate = "mm", by = ~explicit_implicit)) %>% mutate(group= "Left")

faces_center_mm <- as_data_frame(cj(faces_experiment1_center,cj_4, id = ~id, estimate = "mm", by = ~explicit_implicit)) %>% mutate(group= "Center")

faces_right_mm <- as_data_frame(cj(faces_experiment1_right, cj_4, id = ~id, estimate = "mm", by = ~explicit_implicit)) %>% mutate(group= "Right")
faces_all_mm <- as_data_frame(cj(faces_experiment1, cj_4, id = ~id, estimate = "mm", by = ~explicit_implicit)) %>% mutate(group= "All")



faces_ideology_mm <- full_join(faces_left_mm, faces_center_mm) %>% full_join(., faces_right_mm) %>% 
  full_join(., faces_all_mm) %>% 
  mutate(group= factor(group, levels = c("All", "Left", "Center", "Right")), with= "With and without party",
         p_adjusted=p.adjust(p, method= "BH")) 



#### amce WITH and WITHOUT PARTY LABELS ####
faces_left_amce <- as_data_frame(cj(faces_experiment1_left, cj_4, id = ~id, estimate = "amce", by = ~explicit_implicit)) %>% mutate(group= "Left")

faces_center_amce <- as_data_frame(cj(faces_experiment1_center,cj_4, id = ~id, estimate = "amce", by = ~explicit_implicit)) %>% mutate(group= "Center")

faces_right_amce <- as_data_frame(cj(faces_experiment1_right, cj_4, id = ~id, estimate = "amce", by = ~explicit_implicit)) %>% mutate(group= "Right")
faces_all_amce <- as_data_frame(cj(faces_experiment1, cj_4, id = ~id, estimate = "amce", by = ~explicit_implicit)) %>% mutate(group= "All")


faces_ideology_amce<- full_join(faces_left_amce, faces_center_amce) %>% full_join(., faces_right_amce) %>% 
  full_join(., faces_all_amce) %>% 
  mutate(group= factor(group, levels = c("All", "Left", "Center", "Right")), with= "With and without party",
         p_adjusted=p.adjust(p, method= "BH")) 


#### table normal ####
faces_ideology_expimp_table <- full_join(faces_ideology_mm, faces_ideology_amce) %>% 
  full_join(., faces_ideology_mm_party)  %>% 
  full_join(., faces_ideology_mm_noparty)  %>% 
  full_join(., faces_ideology_amce_party) %>% 
  full_join(., faces_ideology_amce_noparty) %>% 
  mutate(group= factor(group, levels = c("All", "Left", "Center", "Right"))) %>% 
  mutate(Significance= factor(case_when(
    p < 0.001 ~ "***",
    p < 0.01 ~ "**",
    p < 0.05 ~ "*",
    TRUE ~ ""), 
    levels= c("", "*", "**", "***")),
    Adjusted_significance= factor(case_when(
      p_adjusted < 0.001 ~ "***",
      p_adjusted < 0.01 ~ "**",
      p_adjusted < 0.05 ~ "*",
      TRUE ~ ""), 
      levels= c("", "*", "**", "***"))) |> 
  mutate(Estimate= estimate) %>% 
  mutate(Estimate= paste(round(Estimate, 2), Significance, sep = ""),
         up_lo=  paste(round(lower, 2), round(upper, 2), sep = ", "),
         up_lo= paste("[", up_lo, sep=""),
         up_lo= paste(up_lo, "]", sep=""),
         Estimate= paste(Estimate, up_lo),
         Estimate= case_when(Estimate== "0 [NA, NA]" ~ "Reference category", TRUE ~ Estimate),
         feature= str_to_title(feature),
         level= paste(paste0("(", feature, ")"), level, sep = " "),
         p= round(p, 3), 
         p_adjusted= round(p_adjusted, 3),
         BY= case_when(BY== "Explicit" ~ "Text",
                       BY== "Implicit" ~ "Visual")) %>% 
  dplyr::select(-c(outcome, std.error, z, estimate, Significance, upper, lower, up_lo, feature)) %>% 
  dplyr::select(statistic, with, group, level, BY, Estimate, p, p_adjusted)  

faces_mm_table <- faces_ideology_expimp_table |> filter(statistic== "mm") |> select(-c("p", "p_adjusted")) |> 
  pivot_wider(names_from = c("BY"), values_from = c("Estimate")) |> 
  select(with, group, level, "Estimate (text)" = Text, "Estimate (visual)" = Visual)

write_csv2(faces_mm_table , "faces_mm_table.csv")


faces_amce_table <- faces_ideology_expimp_table |> filter(statistic== "amce") |> 
  filter(Estimate != "Reference category") |> 
  pivot_wider(names_from = c("BY"), values_from = c("Estimate", "p", "p_adjusted")) |> 
  select(with, group,level, "Estimate (text)"= Estimate_Text, "P value (text)"=  p_Text, "P adj. (text)"= p_adjusted_Text, 
         "Estimate (visual)"= Estimate_Visual, "P value (visual)"= p_Visual, "P. adj. (visual)"= p_adjusted_Visual)

write_csv2(faces_amce_table , "faces_amce_table.csv")

#### mm_diff WITH PARTY LABELS ####

faces_diff_left_mm_party <- as_data_frame(cj(faces_experiment1_left |>  filter(party_notparty== "Party"), cj_1, id = ~id, estimate = "mm_diff", by = ~explicit_implicit)) %>% mutate(group= "Left")

faces_diff_center_mm_party <- as_data_frame(cj(faces_experiment1_center|>  filter(party_notparty== "Party"), cj_1, id = ~id, estimate = "mm_diff", by = ~explicit_implicit)) %>% mutate(group= "Center")

faces_diff_right_mm_party <- as_data_frame(cj(faces_experiment1_right|>  filter(party_notparty== "Party"), cj_1, id = ~id, estimate = "mm_diff", by = ~explicit_implicit)) %>% mutate(group= "Right")

faces_diff_all_mm_party <- as_data_frame(cj(faces_experiment1|>  filter(party_notparty== "Party"), cj_1, id = ~id, estimate = "mm_diff", by = ~explicit_implicit)) %>% mutate(group= "All")


faces_diff_mm_party <- full_join(faces_diff_left_mm_party, faces_diff_center_mm_party) %>% full_join(., faces_diff_right_mm_party) %>% 
  full_join(., faces_diff_all_mm_party) %>% 
  mutate(group= factor(group, levels = c("All", "Left", "Center", "Right")), with= "With party",
         p_adjusted=p.adjust(p, method= "BH")) 


#### amce_diff WITH PARTY LABELS ####

faces_diff_left_amce_party <- as_data_frame(cj(faces_experiment1_left|>  filter(party_notparty== "Party"), cj_1, id = ~id, estimate = "amce_diff", by = ~explicit_implicit)) %>% mutate(group= "Left")

faces_diff_center_amce_party <- as_data_frame(cj(faces_experiment1_center|>  filter(party_notparty== "Party"),cj_1, id = ~id, estimate = "amce_diff", by = ~explicit_implicit)) %>% mutate(group= "Center")

faces_diff_right_amce_party <- as_data_frame(cj(faces_experiment1_right|>  filter(party_notparty== "Party"), cj_1, id = ~id, estimate = "amce_diff", by = ~explicit_implicit)) %>% mutate(group= "Right")
faces_diff_all_amce_party <- as_data_frame(cj(faces_experiment1|>  filter(party_notparty== "Party"), cj_1, id = ~id, estimate = "amce_diff", by = ~explicit_implicit)) %>% mutate(group= "All")

faces_diff_amce_party <- full_join(faces_diff_left_amce_party, faces_diff_center_amce_party) %>% full_join(., faces_diff_right_amce_party) %>% 
  full_join(., faces_diff_all_amce_party) %>% 
  mutate(group= factor(group, levels = c("All", "Left", "Center", "Right")), with= "With party",
         p_adjusted=p.adjust(p, method= "BH"))  


#### mm_diff WITHOUT PARTY LABELS ####

faces_diff_left_mm_noparty <- as_data_frame(cj(faces_experiment1_left|>  filter(party_notparty== "No party"), cj_4, id = ~id, estimate = "mm_diff", by = ~explicit_implicit)) %>% mutate(group= "Left")

faces_diff_center_mm_noparty <- as_data_frame(cj(faces_experiment1_center|>  filter(party_notparty== "No party"), cj_4, id = ~id, estimate = "mm_diff", by = ~explicit_implicit)) %>% mutate(group= "Center")

faces_diff_right_mm_noparty <- as_data_frame(cj(faces_experiment1_right|>  filter(party_notparty== "No party"), cj_4, id = ~id, estimate = "mm_diff", by = ~explicit_implicit)) %>% mutate(group= "Right")

faces_diff_all_mm_noparty <- as_data_frame(cj(faces_experiment1|>  filter(party_notparty== "No party"), cj_4, id = ~id, estimate = "mm_diff", by = ~explicit_implicit)) %>% mutate(group= "All")


faces_diff_mm_noparty <- full_join(faces_diff_left_mm_noparty, faces_diff_center_mm_noparty) %>% full_join(., faces_diff_right_mm_noparty) %>% 
  full_join(., faces_diff_all_mm_noparty) %>% 
  mutate(group= factor(group, levels = c("All", "Left", "Center", "Right")), with= "Without party",
         p_adjusted=p.adjust(p, method= "BH")) 


#### amce_diff WITHOUT PARTY LABELS ####

faces_diff_left_amce_noparty <- as_data_frame(cj(faces_experiment1_left |>  filter(party_notparty== "No party"), cj_4, id = ~id, estimate = "amce_diff", by = ~explicit_implicit)) %>% mutate(group= "Left")

faces_diff_center_amce_noparty <- as_data_frame(cj(faces_experiment1_center|>  filter(party_notparty== "No party"),cj_4, id = ~id, estimate = "amce_diff", by = ~explicit_implicit)) %>% mutate(group= "Center")

faces_diff_right_amce_noparty <- as_data_frame(cj(faces_experiment1_right|>  filter(party_notparty== "No party"), cj_4, id = ~id, estimate = "amce_diff", by = ~explicit_implicit)) %>% mutate(group= "Right")
faces_diff_all_amce_noparty <- as_data_frame(cj(faces_experiment1|>  filter(party_notparty== "No party"), cj_4, id = ~id, estimate = "amce_diff", by = ~explicit_implicit)) %>% mutate(group= "All")

faces_diff_amce_noparty <- full_join(faces_diff_left_amce_noparty, faces_diff_center_amce_noparty) %>% full_join(., faces_diff_right_amce_noparty) %>% 
  full_join(., faces_diff_all_amce_noparty) %>% 
  mutate(group= factor(group, levels = c("All", "Left", "Center", "Right")), with= "Without party",
         p_adjusted=p.adjust(p, method= "BH")) 


#### mm_diff WITH and WITHOUT PARTY LABELS ####

faces_diff_left_mm <- as_data_frame(cj(faces_experiment1_left, cj_1, id = ~id, estimate = "mm_diff", by = ~explicit_implicit)) %>% mutate(group= "Left")

faces_diff_center_mm <- as_data_frame(cj(faces_experiment1_center, cj_1, id = ~id, estimate = "mm_diff", by = ~explicit_implicit)) %>% mutate(group= "Center")

faces_diff_right_mm <- as_data_frame(cj(faces_experiment1_right, cj_1, id = ~id, estimate = "mm_diff", by = ~explicit_implicit)) %>% mutate(group= "Right")

faces_diff_all_mm <- as_data_frame(cj(faces_experiment1, cj_1, id = ~id, estimate = "mm_diff", by = ~explicit_implicit)) %>% mutate(group= "All")


faces_diff_mm <- full_join(faces_diff_left_mm, faces_diff_center_mm) %>% full_join(., faces_diff_right_mm) %>% 
  full_join(., faces_diff_all_mm) %>% 
  mutate(group= factor(group, levels = c("All", "Left", "Center", "Right")), with= "With and without party",
         p_adjusted=p.adjust(p, method= "BH")) 


#### amce_diff WITH and WITHOUT PARTY LABELS ####

faces_diff_left_amce <- as_data_frame(cj(faces_experiment1_left, cj_1, id = ~id, estimate = "amce_diff", by = ~explicit_implicit)) %>% mutate(group= "Left")

faces_diff_center_amce <- as_data_frame(cj(faces_experiment1_center,cj_1, id = ~id, estimate = "amce_diff", by = ~explicit_implicit)) %>% mutate(group= "Center")

faces_diff_right_amce <- as_data_frame(cj(faces_experiment1_right, cj_1, id = ~id, estimate = "amce_diff", by = ~explicit_implicit)) %>% mutate(group= "Right")
faces_diff_all_amce <- cj(faces_experiment1, cj_1, id = ~id, estimate = "amce_diff", by = ~explicit_implicit) %>% mutate(group= "All")

faces_diff_amce <- full_join(faces_diff_left_amce, faces_diff_center_amce) %>% full_join(., faces_diff_right_amce) %>% 
  full_join(., faces_diff_all_amce) %>% 
  mutate(group= factor(group, levels = c("All", "Left", "Center", "Right")), with= "With and without party",
         p_adjusted=p.adjust(p, method= "BH")) 



#### table diff ####

faces_ideology_expimp_table_diff <- full_join(faces_diff_mm_party, faces_diff_amce_party) %>% 
  full_join(., faces_diff_amce)  %>% 
  full_join(., faces_diff_mm_noparty)  %>% 
  full_join(., faces_diff_mm) %>% 
  full_join(., faces_diff_amce_noparty) %>% 
  mutate(group= factor(group, levels = c("All", "Left", "Center", "Right"))) %>% 
  mutate(Significance= factor(case_when(
    p < 0.001 ~ "***",
    p < 0.01 ~ "**",
    p < 0.05 ~ "*",
    TRUE ~ ""), 
    levels= c("", "*", "**", "***")),
    Adjusted_significance= factor(case_when(
      p_adjusted < 0.001 ~ "***",
      p_adjusted < 0.01 ~ "**",
      p_adjusted < 0.05 ~ "*",
      TRUE ~ ""), 
      levels= c("", "*", "**", "***")),
    p= round(p, 3), 
    p_adjusted= round(p_adjusted, 3),
    BY= case_when(BY== "Explicit" ~ "Text",
                  BY== "Implicit" ~ "Visual")) |> 
  mutate(Estimate= estimate) %>% 
  mutate(Estimate= paste(round(Estimate, 2), Significance, sep = ""),
         up_lo=  paste(round(lower, 2), round(upper, 2), sep = ", "),
         up_lo= paste("[", up_lo, sep=""),
         up_lo= paste(up_lo, "]", sep=""),
         Estimate= paste(Estimate, up_lo),
         Estimate= case_when(Estimate== "0 [NA, NA]" ~ "Reference category", TRUE ~ Estimate),
         feature= str_to_title(feature),
         level= paste(paste0("(", feature, ")"), level, sep = " ")) %>% 
  dplyr::select(-c(outcome, std.error, z, estimate, Significance, upper, lower, up_lo, feature)) %>% 
  dplyr::select(statistic, group, with, level, Estimate, p, p_adjusted) 


faces_mmdiff_table <- faces_ideology_expimp_table_diff |> 
  filter(statistic== "mm_difference") |> 
  mutate(with= factor(with, levels= c("Without party", "With party", "With and without party" ))) |> 
  select(with, group, level, Estimate, "P value"=p, "P. adj." = p_adjusted)


write_csv2(faces_mmdiff_table, "faces_mmdiff_table.csv")


faces_amcediff_table <- faces_ideology_expimp_table_diff |> 
  filter(statistic== "amce_difference")


write_csv2(faces_amcediff_table, "faces_amcediff_table.csv")

#### intersectional ####

#### intersectional AMCE WITH and WITHOUT PARTY LABELS ####

faces_diverse_amce <- cj(faces_experiment1, cj_3, id = ~id, estimate = "amce", by = ~explicit_implicit) %>% mutate(party= "With and without party")


#### intersectional MM WITH and WITHOUT PARTY LABELS ####


faces_diverse_mm <- cj(faces_experiment1, cj_3, id = ~id, estimate = "mm", by = ~explicit_implicit)  %>% mutate(party= "With and without party")


#### intersectional AMCE_diff WITH and WITHOUT PARTY LABELS ####

faces_diverse_amcediff <- cj(faces_experiment1, cj_3, id = ~id, estimate = "amce_diff", by = ~explicit_implicit)  %>% mutate(party= "With and without party")


#### intersectional MM_diff WITH and WITHOUT PARTY LABELS ####

faces_diverse_mmdiff <- cj(faces_experiment1, cj_3, id = ~id, estimate = "mm_diff", by = ~explicit_implicit)  %>% mutate(party= "With and without party")


#### intersectional AMCE WITH PARTY LABELS ####

faces_diverse_amce_party <- cj(faces_experiment1 |>  filter(party_notparty== "Party"), cj_3, id = ~id, estimate = "amce", by = ~explicit_implicit) %>% mutate(party= "With party")


#### intersectional MM WITH PARTY LABELS ####


faces_diverse_mm_party <- cj(faces_experiment1|>  filter(party_notparty== "Party"), cj_3, id = ~id, estimate = "mm", by = ~explicit_implicit) %>% mutate(party= "With party")


#### intersectional AMCE_diff WITH PARTY LABELS ####

faces_diverse_amcediff_party <- cj(faces_experiment1|>  filter(party_notparty== "Party"), cj_3, id = ~id, estimate = "amce_diff", by = ~explicit_implicit) %>% mutate(party= "With party")


#### intersectional MM_diff WITH PARTY LABELS ####

faces_diverse_mmdiff_party <- cj(faces_experiment1|>  filter(party_notparty== "Party"), cj_3, id = ~id, estimate = "mm_diff", by = ~explicit_implicit) %>% mutate(party= "With party")


#### intersectional AMCE WITH PARTY LABELS ####

faces_diverse_amce_noparty <- cj(faces_experiment1 |>  filter(party_notparty== "No party"), cj_3, id = ~id, estimate = "amce", by = ~explicit_implicit) %>% mutate(party= "Without party")


#### intersectional MM WITH PARTY LABELS ####


faces_diverse_mm_noparty <- cj(faces_experiment1|>  filter(party_notparty== "No party"), cj_3, id = ~id, estimate = "mm", by = ~explicit_implicit) %>% mutate(party= "Without party")


#### intersectional AMCE_diff WITH PARTY LABELS ####

faces_diverse_amcediff_noparty <- cj(faces_experiment1|>  filter(party_notparty== "No party"), cj_3, id = ~id, estimate = "amce_diff", by = ~explicit_implicit) %>% mutate(party= "Without party")


#### intersectional MM_diff WITH PARTY LABELS ####

faces_diverse_mmdiff_noparty <- cj(faces_experiment1|>  filter(party_notparty== "No party"), cj_3, id = ~id, estimate = "mm_diff", by = ~explicit_implicit) %>% mutate(party= "Without party")



#### table diff ####

faces_diverse_table <- full_join(faces_diverse_amce, faces_diverse_mm) %>% 
  full_join(., faces_diverse_amcediff)  %>% 
  full_join(., faces_diverse_mmdiff)  %>% 
  full_join(., faces_diverse_amce_party) %>% 
  full_join(., faces_diverse_mm_party) %>% 
  full_join(., faces_diverse_amcediff_party) %>% 
  full_join(., faces_diverse_mmdiff_party) %>%
  full_join(., faces_diverse_amce_noparty) %>% 
  full_join(., faces_diverse_mm_noparty) %>% 
  full_join(., faces_diverse_amcediff_noparty) %>% 
  full_join(., faces_diverse_mmdiff_noparty) %>% 
  mutate(p_adjusted= round(p.adjust(p, method= "BH"), 2), estimate= round(estimate, 2), p= round(p, 2), z= round(z, 2)) |> 
  mutate(Significance= factor(case_when(
    p < 0.001 ~ "***",
    p < 0.01 ~ "**",
    p < 0.05 ~ "*",
    TRUE ~ ""), 
    levels= c("", "*", "**", "***")),
    Adjusted_significance= factor(case_when(
      p_adjusted < 0.001 ~ "***",
      p_adjusted < 0.01 ~ "**",
      p_adjusted < 0.05 ~ "*",
      TRUE ~ ""), 
      levels= c("", "*", "**", "***")),
    p= round(p, 4), 
    p_adjusted= round(p_adjusted, 4),
    BY= case_when(explicit_implicit== "Explicit" ~ "Text",
                  explicit_implicit== "Implicit" ~ "Visual")) |> 
  mutate(Estimate= estimate) %>% 
  mutate(Estimate= paste(round(Estimate, 2), Adjusted_significance, sep = ""),
         up_lo=  paste(round(lower, 2), round(upper, 2), sep = ", "),
         up_lo= paste("[", up_lo, sep=""),
         up_lo= paste(up_lo, "]", sep=""),
         Estimate= paste(Estimate, up_lo),
         Estimate= case_when(Estimate== "0 [NA, NA]" ~ "Reference category", TRUE ~ Estimate),
         feature= str_to_title(feature),
         level= paste(paste0("(", feature, ")"), level, sep = " ")) %>% 
  dplyr::select(-c(outcome, std.error, z, estimate, Significance, Adjusted_significance, upper, lower, up_lo, feature, explicit_implicit)) 



faces_diverse_mm_table <- faces_diverse_table |> select(-c("p", "p_adjusted")) |> 
  filter(statistic== "mm") |> 
  pivot_wider(names_from = c("BY"), values_from = c("Estimate")) |> 
  select(with=party, level, "Estimate (text)"= Text,
         "Estimate (visual)"= Visual)

write_csv2(faces_diverse_mm_table, "faces_diverse_mm_table.csv")


faces_diverse_amce_table <- faces_diverse_table |> 
  filter(statistic== "amce") |> 
  filter(Estimate!= "Reference category") |> 
  pivot_wider(names_from = c("BY"), values_from = c("Estimate", "p", "p_adjusted")) |> 
  select(with=party, level, "Estimate (text)"= Estimate_Text, "P value (text)"=  p_Text, "P adj. (text)"= p_adjusted_Text, 
         "Estimate (visual)"= Estimate_Visual, "P value (visual)"= p_Visual, "P. adj. (visual)"= p_adjusted_Visual)

write_csv2(faces_diverse_amce_table, "faces_diverse_amce_table.csv")

faces_diverse_mmdiff_table <- faces_diverse_table |> 
  filter(statistic== "mm_difference") |> 
  filter(Estimate!= "Reference category") |> 
  select(with=party, level, Estimate, "P value" = p, "P. adj." = p_adjusted)

write_csv2(faces_diverse_mmdiff_table, "faces_diverse_mmdiff_table.csv")


faces_diverse_amcediff_table <- faces_diverse_table |> 
  filter(statistic== "amce_difference") |> 
  filter(Estimate!= "Reference category") |> 
  select(with=party, level, Estimate, "P value" = p, "P. adj." = p_adjusted)

write_csv2(faces_diverse_amcediff_table, "faces_diverse_amcediff_table.csv")


############### 1 final graphs ##################



#### graph_1 ####
graph_general <- faces_ideology_mm %>% 
  filter(group=="All") %>% 
  mutate(upper90= estimate + 1.645*std.error,
         lower90= estimate - 1.645*std.error
  ) |> 
  #filter(level %in% c("Non-diverse", "Woman", "Non-binary", "LGBAQ", "Non-White")) %>% 
  ggplot(aes(level, color= feature)) +
  geom_hline(yintercept = 0.5, color="grey10", linetype="dashed" ) +
  geom_linerange(aes(ymin= lower, ymax = upper),
                 width=0.2, size=3, alpha=0.5, position=position_dodge(width = 0.9)) +
  geom_linerange(aes(ymin= lower90, ymax = upper90),
                 width=0.2, size=3, position=position_dodge(width = 0.9)) +
  geom_point(aes(y=estimate), size=3, shape=21,
             fill="white", 
             color="black", 
             position=position_dodge(width = 0.9))+
  facet_grid(feature~., scales='free', space='free', switch = 'y') +
  coord_flip() +
  theme(legend.title = element_blank()) +
  theme(strip.text = element_blank()) +
  labs(x=NULL, y='Marginal means') +
  theme_minimal() +
  theme(
    strip.placement = 'outside',
    strip.background = element_rect(color=NA, fill=NA),
    panel.spacing = unit(0,'pt'),
    panel.background = element_rect(color='black'),
    legend.position = "none"
  ) +
  scale_color_pretty_d("Relax")

ggsave(graph_general, file="graph_general.png", width=7, height=8)




#### graph_2 ####
graph_visual <- full_join(faces_ideology_mm, faces_ideology_amce) %>% 
  full_join(., faces_ideology_mm_party)  %>% 
  full_join(., faces_ideology_mm_noparty)  %>% 
  full_join(., faces_ideology_amce_party) %>% 
  full_join(., faces_ideology_amce_noparty) %>% 
  mutate(group= factor(group, levels = c("All", "Left", "Center", "Right"))) |> 
  mutate(Significance= factor(case_when(
    p < 0.001 ~ "***",
    p < 0.01 ~ "**",
    p < 0.05 ~ "*",
    TRUE ~ ""), 
    levels= c("", "*", "**", "***")),
    Adjusted_significance= factor(case_when(
      p_adjusted < 0.001 ~ "***",
      p_adjusted < 0.01 ~ "**",
      p_adjusted < 0.05 ~ "*",
      TRUE ~ ""), 
      levels= c("", "*", "**", "***")),
    p= round(p, 3), 
    p_adjusted= round(p_adjusted, 3),
    BY= case_when(BY== "Explicit" ~ "Text",
                  BY== "Implicit" ~ "Visual")) |> 
  filter(!feature %in% c("party", "record", "experience")) %>% 
  filter(group=="All") %>% 
  filter(statistic=="amce") %>% 
  pivot_wider(names_from = with, values_from = c("estimate", "lower", "upper")) |> 
  
  ggplot(aes(level, color= BY, shape= BY)) +
  geom_hline(yintercept = 0.0, color="grey10", linetype="dashed" ) +
  geom_linerange(aes(ymin= `lower_With and without party`, ymax = `upper_With and without party`),
                 width=0.2, size=3,  position=position_dodge(width = 1)) +
  geom_linerange(aes(ymin= `lower_With party`, ymax = `upper_With party`),
                 width=0.2, size=1, alpha=0.4,position=position_dodge(width = 0.5)) + 
  geom_linerange(aes(ymin= `lower_Without party`, ymax = `upper_Without party`),
                 width=0.2, size=1, alpha=0.6, position=position_dodge(width = 0.3)) +
  geom_point(aes(y=`estimate_With and without party`), size=3,              
             fill="white", 
             color="black", 
             position=position_dodge(width = 1))+
  geom_point(aes(y=`estimate_With party`), size=1,              
             fill="white", 
             color="black", 
             position=position_dodge(width = 0.5), alpha=0.4)+
  geom_point(aes(y=`estimate_Without party`), size=1,              
             fill="white", 
             color="black", 
             position=position_dodge(width = 0.3), alpha=0.6)+  
  # Add text for the first linerange:
  geom_text(aes(y= `upper_With and without party`, 
                label=if_else(Significance == "" & Adjusted_significance == "",
                              "",
                              paste(Significance, "/", Adjusted_significance, sep=""))),
            position=position_dodge(width = 1), hjust=-0.1, vjust= 0.7) +
  
  # Add text for the second linerange:
  geom_text(aes(y= `upper_With party`, 
                label=if_else(Significance == "" & Adjusted_significance == "",
                              "",
                              paste(Significance, "/", Adjusted_significance, sep=""))),
            position=position_dodge(width = 0.5), hjust=-0.1, vjust= 0.7, alpha=0.4) +
  
  # Add text for the third linerange:
  geom_text(aes(y= `upper_Without party`, 
                label=if_else(Significance == "" & Adjusted_significance == "",
                              "",
                              paste(Significance, "/", Adjusted_significance, sep=""))),
            position=position_dodge(width = 0.3), hjust=-0.1, vjust= 0.7, alpha=0.6) +
  facet_grid(feature~., scales='free', space='free', switch = 'y') +
  coord_flip() +
  theme(legend.title = element_blank()) +
  theme(strip.text = element_blank()) +
  labs(x=NULL, y='AMCE') +
  ylim(-0.13, 0.13) +
  theme_classic() +
  theme(
    strip.placement = 'outside',
    strip.background = element_rect(color=NA, fill=NA),
    panel.spacing = unit(0,'pt'),
    panel.background = element_rect(color='black'),
    legend.position = "bottom"
    
  ) +
  scale_color_pretty_d("Neon")+guides(shape = "none")



graph_visual_diff <- full_join(faces_diff_mm_party, faces_diff_amce_party) %>% 
  full_join(., faces_diff_amce)  %>% 
  full_join(., faces_diff_mm_noparty)  %>% 
  full_join(., faces_diff_mm) %>% 
  full_join(., faces_diff_amce_noparty) %>% 
  mutate(group= factor(group, levels = c("All", "Left", "Center", "Right"))) %>% 
  mutate(Significance= factor(case_when(
    p < 0.001 ~ "***",
    p < 0.01 ~ "**",
    p < 0.05 ~ "*",
    TRUE ~ ""), 
    levels= c("", "*", "**", "***")),
    Adjusted_significance= factor(case_when(
      p_adjusted < 0.001 ~ "***",
      p_adjusted < 0.01 ~ "**",
      p_adjusted < 0.05 ~ "*",
      TRUE ~ ""), 
      levels= c("", "*", "**", "***")),
    p= round(p, 3), 
    p_adjusted= round(p_adjusted, 3),
    BY= case_when(BY== "Explicit" ~ "Text",
                  BY== "Implicit" ~ "Visual")) |> 
  mutate(BY= case_when(BY== "Implicit" ~ "Visual",
                       BY== "Explicit" ~ "Text"), with2=with) |> 
  pivot_wider(names_from = with, values_from = c("estimate", "lower", "upper", "p", "p_adjusted")) |> 
  filter(!feature %in% c("party", "record", "experience")) %>% 
  filter(statistic== "mm_difference") %>% 
  filter(group== "All") %>% 
  
  ggplot(aes(level, fill= group, color=with2)) +
  
  geom_hline(yintercept = 0.0, color="grey10", linetype="dashed" ) +
  
  geom_linerange(aes(ymin= `lower_With and without party`, ymax = `upper_With and without party`),
                 width=0.2, size=3,  position=position_dodge(width = 1)) +
  geom_linerange(aes(ymin= `lower_With party`, ymax = `upper_With party`),
                 width=0.2, size=1, alpha=0.4,position=position_dodge(width = 0.5)) + 
  geom_linerange(aes(ymin= `lower_Without party`, ymax = `upper_Without party`),
                 width=0.2, size=1, alpha=0.7,position=position_dodge(width = 0.3)) +
  geom_point(aes(y=`estimate_With and without party`), size=4,              
             position=position_dodge(width = 1))+
  geom_point(aes(y=`estimate_With party`), size=1,              
             position=position_dodge(width = 0.5), alpha=0.4)+
  geom_point(aes(y=`estimate_Without party`), size=1,              
             position=position_dodge(width = 0.3), alpha=0.6)+   
  
  # Add text for the first linerange:
  geom_text(aes(y= `upper_With and without party`, 
                label=if_else(Significance == "" & Adjusted_significance == "",
                              "",
                              paste(Significance, "/", Adjusted_significance, sep=""))),
            position=position_dodge(width = 1), hjust=-0.1, vjust= 0.7) +
  
  # Add text for the second linerange:
  geom_text(aes(y= `upper_With party`, 
                label=if_else(Significance == "" & Adjusted_significance == "",
                              "",
                              paste(Significance, "/", Adjusted_significance, sep=""))),
            position=position_dodge(width = 0.5), hjust=-0.1, vjust= 0.7, alpha=0.4) +
  
  # Add text for the third linerange:
  geom_text(aes(y= `upper_Without party`, 
                label=if_else(Significance == "" & Adjusted_significance == "",
                              "",
                              paste(Significance, "/", Adjusted_significance, sep=""))),
            position=position_dodge(width = 0.3), hjust=-0.1, vjust= 0.7, alpha=0.6) +
  facet_grid(feature~group, scales='free', space='free', switch = 'y') +
  coord_flip() +
  ylim(-0.13, 0.13) +
  theme(legend.title = element_blank()) +
  theme(strip.text = element_blank()) +
  labs(x=NULL, y='Diff. in Marginal means') +
  theme_classic() +
  theme(
    strip.placement = 'outside',
    strip.background = element_rect(color=NA, fill=NA),
    panel.spacing = unit(0,'pt'),
    panel.background = element_rect(color='black'),
    legend.position = "none") + 
  scale_colour_manual(values = c("#4f92d7", "#4f92d7", "#4f92d7"))



library(patchwork)
graph_visualdiff <- graph_visual +graph_visual_diff


ggsave(graph_visualdiff, file="graph_visualdiff.png", width=7, height=6)

#### graph_3 ####

graph_visual_diff2 <- full_join(faces_diff_mm_party, faces_diff_amce_party) %>% 
  full_join(., faces_diff_amce)  %>% 
  full_join(., faces_diff_mm_noparty)  %>% 
  full_join(., faces_diff_mm) %>% 
  full_join(., faces_diff_amce_noparty) %>% 
  mutate(group= factor(group, levels = c("All", "Left", "Center", "Right"))) %>% 
  mutate(Significance= factor(case_when(
    p < 0.001 ~ "***",
    p < 0.01 ~ "**",
    p < 0.05 ~ "*",
    TRUE ~ ""), 
    levels= c("", "*", "**", "***")),
    Adjusted_significance= factor(case_when(
      p_adjusted < 0.001 ~ "***",
      p_adjusted < 0.01 ~ "**",
      p_adjusted < 0.05 ~ "*",
      TRUE ~ ""), 
      levels= c("", "*", "**", "***")),
    p= round(p, 3), 
    p_adjusted= round(p_adjusted, 3),
    BY= case_when(BY== "Explicit" ~ "Text",
                  BY== "Implicit" ~ "Visual"), with2=with) |> 
  filter(!feature %in% c("party", "record", "experience")) %>% 
  mutate(group= factor(group, levels = c("All", "Left", "Center", "Right"))) |> 
  filter(group!="All") %>% 
  filter(statistic=="mm_difference") %>% 
  pivot_wider(names_from = with, values_from = c("estimate", "lower", "upper")) |> 
  
  ggplot(aes(level, fill= group, color=with2)) +
  
  geom_hline(yintercept = 0.0, color="grey10", linetype="dashed" ) +
  
  geom_linerange(aes(ymin= `lower_With and without party`, ymax = `upper_With and without party`),
                 width=0.2, size=3,  position=position_dodge(width = 1)) +
  geom_linerange(aes(ymin= `lower_With party`, ymax = `upper_With party`),
                 width=0.2, size=1, alpha=0.4,position=position_dodge(width = 0.5)) + 
  geom_linerange(aes(ymin= `lower_Without party`, ymax = `upper_Without party`),
                 width=0.2, size=1, alpha=0.6,position=position_dodge(width = 0.3)) +
  geom_point(aes(y=`estimate_With and without party`), size=4,              
             position=position_dodge(width = 1))+
  geom_point(aes(y=`estimate_With party`), size=1,              
             position=position_dodge(width = 0.5), alpha=0.4)+
  geom_point(aes(y=`estimate_Without party`), size=1,              
             position=position_dodge(width = 0.3), alpha=0.6)+ 
  # Add text for the first linerange:
  geom_text(aes(y= `upper_With and without party`, 
                label=if_else(Significance == "" & Adjusted_significance == "",
                              "",
                              paste(Significance, "/", Adjusted_significance, sep=""))),
            position=position_dodge(width = 1), hjust=-0.1, vjust= 0.7) +
  
  # Add text for the second linerange:
  geom_text(aes(y= `upper_With party`, 
                label=if_else(Significance == "" & Adjusted_significance == "",
                              "",
                              paste(Significance, "/", Adjusted_significance, sep=""))),
            position=position_dodge(width = 0.5), hjust=-0.1, vjust= 0.7, alpha=0.4) +
  
  # Add text for the third linerange:
  geom_text(aes(y= `upper_Without party`, 
                label=if_else(Significance == "" & Adjusted_significance == "",
                              "",
                              paste(Significance, "/", Adjusted_significance, sep=""))),
            position=position_dodge(width = 0.3), hjust=-0.1, vjust= 0.7, alpha=0.6) +
  facet_grid(feature~group, scales='free', space='free', switch = 'y') +
  coord_flip() +
  ylim(-0.15, 0.15) +
  theme(legend.title = element_blank()) +
  theme(strip.text = element_blank()) +
  labs(x=NULL, y='Diff. in Marginal means') +
  theme_classic() +
  theme(
    strip.placement = 'outside',
    strip.background = element_rect(color=NA, fill=NA),
    panel.spacing = unit(0,'pt'),
    panel.background = element_rect(color='black'),
    legend.position = "none") + 
  scale_colour_manual(values = c("#4f92d7", "#4f92d7", "#4f92d7"))







graph_visual2 <- full_join(faces_ideology_mm, faces_ideology_amce) %>% 
  full_join(., faces_ideology_mm_party)  %>% 
  full_join(., faces_ideology_mm_noparty)  %>% 
  full_join(., faces_ideology_amce_party) %>% 
  full_join(., faces_ideology_amce_noparty) %>% 
  mutate(Significance= factor(case_when(
    p < 0.001 ~ "***",
    p < 0.01 ~ "**",
    p < 0.05 ~ "*",
    TRUE ~ ""), 
    levels= c("", "*", "**", "***")),
    Adjusted_significance= factor(case_when(
      p_adjusted < 0.001 ~ "***",
      p_adjusted < 0.01 ~ "**",
      p_adjusted < 0.05 ~ "*",
      TRUE ~ ""), 
      levels= c("", "*", "**", "***")),
    p= round(p, 3), 
    p_adjusted= round(p_adjusted, 3),
    BY= case_when(BY== "Explicit" ~ "Text",
                  BY== "Implicit" ~ "Visual")) |> 
  filter(!feature %in% c("party", "record", "experience")) %>% 
  mutate(group= factor(group, levels = c("All", "Left", "Center", "Right"))) |> 
  filter(group!="All") %>% 
  filter(statistic=="amce") %>% 
  pivot_wider(names_from = with, values_from = c("estimate", "lower", "upper")) |> 
  
  ggplot(aes(level, color= BY, shape= BY)) +
  geom_hline(yintercept = 0.0, color="grey10", linetype="dashed" ) +
  geom_linerange(aes(ymin= `lower_With and without party`, ymax = `upper_With and without party`),
                 width=0.2, size=3,  position=position_dodge(width = 1)) +
  geom_linerange(aes(ymin= `lower_With party`, ymax = `upper_With party`),
                 width=0.2, size=1, alpha=0.4,position=position_dodge(width = 0.5)) + 
  geom_linerange(aes(ymin= `lower_Without party`, ymax = `upper_Without party`),
                 width=0.2, size=1, alpha=0.6, position=position_dodge(width = 0.3)) +
  geom_point(aes(y=`estimate_With and without party`), size=3,              
             fill="white", 
             color="black", 
             position=position_dodge(width = 1))+
  geom_point(aes(y=`estimate_With party`), size=1,              
             fill="white", 
             color="black", 
             position=position_dodge(width = 0.5), alpha=0.4)+
  geom_point(aes(y=`estimate_Without party`), size=1,              
             fill="white", 
             color="black", 
             position=position_dodge(width = 0.3), alpha=0.6)+   
  # Add text for the first linerange:
  geom_text(aes(y= `upper_With and without party`, 
                label=if_else(Significance == "" & Adjusted_significance == "",
                              "",
                              paste(Significance, "/", Adjusted_significance, sep=""))),
            position=position_dodge(width = 1), hjust=-0.1, vjust= 0.7) +
  
  # Add text for the second linerange:
  geom_text(aes(y= `upper_With party`, 
                label=if_else(Significance == "" & Adjusted_significance == "",
                              "",
                              paste(Significance, "/", Adjusted_significance, sep=""))),
            position=position_dodge(width = 0.5), hjust=-0.1, vjust= 0.7, alpha=0.4) +
  
  # Add text for the third linerange:
  geom_text(aes(y= `upper_Without party`, 
                label=if_else(Significance == "" & Adjusted_significance == "",
                              "",
                              paste(Significance, "/", Adjusted_significance, sep=""))),
            position=position_dodge(width = 0.3), hjust=-0.1, vjust= 0.7, alpha=0.6) +
  facet_grid(feature~group, scales='free', space='free', switch = 'y') +
  coord_flip() +
  ylim(-0.32, 0.32) +
  theme(legend.title = element_blank()) +
  theme(strip.text = element_blank()) +
  labs(x=NULL, y='AMCE') +
  theme_classic() +
  theme(
    strip.placement = 'outside',
    strip.background = element_rect(color=NA, fill=NA),
    panel.spacing = unit(0,'pt'),
    panel.background = element_rect(color='black'),
    legend.position = "bottom"
    
  ) +
  scale_color_pretty_d("Neon")+guides(shape = "none")



graph_visdiff2 <-  graph_visual2 / graph_visual_diff2
ggsave(graph_visdiff2, file="graph_visdiff2.png", width=7, height=10)


#### graph_4 ####

graph_visual_diverse <- full_join(faces_diverse_amce, faces_diverse_mm) %>% 
  full_join(., faces_diverse_amcediff)  %>% 
  full_join(., faces_diverse_mmdiff)  %>% 
  full_join(., faces_diverse_amce_party) %>% 
  full_join(., faces_diverse_mm_party) %>% 
  full_join(., faces_diverse_amcediff_party) %>% 
  full_join(., faces_diverse_mmdiff_party) %>%
  full_join(., faces_diverse_amce_noparty) %>% 
  full_join(., faces_diverse_mm_noparty) %>% 
  full_join(., faces_diverse_amcediff_noparty) %>% 
  full_join(., faces_diverse_mmdiff_noparty) %>% 
  mutate(p_adjusted= round(p.adjust(p, method= "BH"), 2), estimate= round(estimate, 2), p= round(p, 2), z= round(z, 2)) |> 
  mutate(Significance= factor(case_when(
    p < 0.001 ~ "***",
    p < 0.01 ~ "**",
    p < 0.05 ~ "*",
    TRUE ~ ""), 
    levels= c("", "*", "**", "***")),
    Adjusted_significance= factor(case_when(
      p_adjusted < 0.001 ~ "***",
      p_adjusted < 0.01 ~ "**",
      p_adjusted < 0.05 ~ "*",
      TRUE ~ ""), 
      levels= c("", "*", "**", "***")),
    p= round(p, 4), 
    p_adjusted= round(p_adjusted, 4),
    BY= case_when(explicit_implicit== "Explicit" ~ "Text",
                  explicit_implicit== "Implicit" ~ "Visual")) |> 
  filter(!feature %in% c("party", "record", "experience")) %>% 
  filter(statistic=="amce") %>% 
  pivot_wider(names_from = party, values_from = c("estimate", "lower", "upper")) |> 
  filter(!level %in% c("Woman", "Non-White", "LGB")) %>%
  
  ggplot(aes(level, color= BY, shape= BY)) +
  geom_hline(yintercept = 0.0, color="grey10", linetype="dashed" ) +
  geom_linerange(aes(ymin= `lower_With and without party`, ymax = `upper_With and without party`),
                 width=0.2, size=3,  position=position_dodge(width = 1)) +
  geom_linerange(aes(ymin= `lower_With party`, ymax = `upper_With party`),
                 width=0.2, size=1, alpha=0.4,position=position_dodge(width = 0.5)) + 
  geom_linerange(aes(ymin= `lower_Without party`, ymax = `upper_Without party`),
                 width=0.2, size=1, alpha=0.6, position=position_dodge(width = 0.3)) +
  geom_point(aes(y=`estimate_With and without party`), size=3,              
             fill="white", 
             color="black", 
             position=position_dodge(width = 1))+
  geom_point(aes(y=`estimate_With party`), size=1,              
             fill="white", 
             color="black", 
             position=position_dodge(width = 0.5), alpha=0.4)+
  geom_point(aes(y=`estimate_Without party`), size=1,              
             fill="white", 
             color="black", 
             position=position_dodge(width = 0.3), alpha=0.6)+ 
  # Add text for the first linerange:
  geom_text(aes(y= `upper_With and without party`, 
                label=if_else(Significance == "" & Adjusted_significance == "",
                              "",
                              paste(Significance, "/", Adjusted_significance, sep=""))),
            position=position_dodge(width = 1), hjust=-0.1, vjust= 0.7) +
  
  # Add text for the second linerange:
  geom_text(aes(y= `upper_With party`, 
                label=if_else(Significance == "" & Adjusted_significance == "",
                              "",
                              paste(Significance, "/", Adjusted_significance, sep=""))),
            position=position_dodge(width = 0.5), hjust=-0.1, vjust= 0.7, alpha=0.4) +
  
  # Add text for the third linerange:
  geom_text(aes(y= `upper_Without party`, 
                label=if_else(Significance == "" & Adjusted_significance == "",
                              "",
                              paste(Significance, "/", Adjusted_significance, sep=""))),
            position=position_dodge(width = 0.3), hjust=-0.1, vjust= 0.7, alpha=0.6) +
  
  
  facet_grid(feature~., scales='free', space='free', switch = 'y') +
  coord_flip() +
  #ylim(0.32, 0.68) +
  theme(legend.title = element_blank()) +
  theme(strip.text = element_blank()) +
  labs(x=NULL, y='AMCE') +
  theme_classic() +
  theme(
    strip.placement = 'outside',
    strip.background = element_rect(color=NA, fill=NA),
    panel.spacing = unit(0,'pt'),
    panel.background = element_rect(color='black'),
    legend.position = "bottom"
    
  ) +
  scale_color_pretty_d("Neon")+guides(shape = "none")



graph_visual_diverse_diff <- full_join(faces_diverse_amce, faces_diverse_mm) %>% 
  full_join(., faces_diverse_amcediff)  %>% 
  full_join(., faces_diverse_mmdiff)  %>% 
  full_join(., faces_diverse_amce_party) %>% 
  full_join(., faces_diverse_mm_party) %>% 
  full_join(., faces_diverse_amcediff_party) %>% 
  full_join(., faces_diverse_mmdiff_party) %>%
  full_join(., faces_diverse_amce_noparty) %>% 
  full_join(., faces_diverse_mm_noparty) %>% 
  full_join(., faces_diverse_amcediff_noparty) %>% 
  full_join(., faces_diverse_mmdiff_noparty) %>% 
  mutate(p_adjusted= round(p.adjust(p, method= "BH"), 2), estimate= round(estimate, 2), p= round(p, 2), z= round(z, 2)) |> 
  mutate(Significance= factor(case_when(
    p < 0.001 ~ "***",
    p < 0.01 ~ "**",
    p < 0.05 ~ "*",
    TRUE ~ ""), 
    levels= c("", "*", "**", "***")),
    Adjusted_significance= factor(case_when(
      p_adjusted < 0.001 ~ "***",
      p_adjusted < 0.01 ~ "**",
      p_adjusted < 0.05 ~ "*",
      TRUE ~ ""), 
      levels= c("", "*", "**", "***")),
    p= round(p, 4), 
    p_adjusted= round(p_adjusted, 4),
    BY= case_when(explicit_implicit== "Explicit" ~ "Text",
                  explicit_implicit== "Implicit" ~ "Visual")) |> 
  filter(!feature %in% c("party", "record", "experience")) %>% 
  filter(statistic=="mm_difference") %>% 
  mutate(BY= case_when(BY== "Implicit" ~ "Visual",
                       BY== "Explicit" ~ "Text"), with2=party) |> 
  pivot_wider(names_from = party, values_from = c("estimate", "lower", "upper")) |> 
  filter(!level %in% c("Woman", "Non-White", "LGB")) %>%
  ggplot(aes(level, fill= outcome, color=with2)) +
  
  geom_hline(yintercept = 0.0, color="grey10", linetype="dashed" ) +
  
  geom_linerange(aes(ymin= `lower_With and without party`, ymax = `upper_With and without party`),
                 width=0.2, size=3,  position=position_dodge(width = 1)) +
  geom_linerange(aes(ymin= `lower_With party`, ymax = `upper_With party`),
                 width=0.2, size=1, alpha=0.4,position=position_dodge(width = 0.5)) + 
  geom_linerange(aes(ymin= `lower_Without party`, ymax = `upper_Without party`),
                 width=0.2, size=1, alpha=0.6,position=position_dodge(width = 0.3)) +
  geom_point(aes(y=`estimate_With and without party`), size=4,              
             position=position_dodge(width = 1))+
  geom_point(aes(y=`estimate_With party`), size=1,              
             position=position_dodge(width = 0.5), alpha=0.4)+
  geom_point(aes(y=`estimate_Without party`), size=1,              
             position=position_dodge(width = 0.3), alpha=0.6)+ 
  # Add text for the first linerange:
  geom_text(aes(y= `upper_With and without party`, 
                label=if_else(Significance == "" & Adjusted_significance == "",
                              "",
                              paste(Significance, "/", Adjusted_significance, sep=""))),
            position=position_dodge(width = 1), hjust=-0.1, vjust= 0.7) +
  
  # Add text for the second linerange:
  geom_text(aes(y= `upper_With party`, 
                label=if_else(Significance == "" & Adjusted_significance == "",
                              "",
                              paste(Significance, "/", Adjusted_significance, sep=""))),
            position=position_dodge(width = 0.5), hjust=-0.1, vjust= 0.7, alpha=0.4) +
  
  # Add text for the third linerange:
  geom_text(aes(y= `upper_Without party`, 
                label=if_else(Significance == "" & Adjusted_significance == "",
                              "",
                              paste(Significance, "/", Adjusted_significance, sep=""))),
            position=position_dodge(width = 0.3), hjust=-0.1, vjust= 0.7, alpha=0.6) +
  facet_grid(feature~., scales='free', space='free', switch = 'y') +
  coord_flip() +
  ylim(-0.25, 0.25) +
  theme(legend.title = element_blank()) +
  theme(strip.text = element_blank()) +
  labs(x=NULL, y='Diff. in Marginal means') +
  theme_classic() +
  theme(
    strip.placement = 'outside',
    strip.background = element_rect(color=NA, fill=NA),
    panel.spacing = unit(0,'pt'),
    panel.background = element_rect(color='black'),
    legend.position = "none") + 
  scale_colour_manual(values = c("#4f92d7", "#4f92d7", "#4f92d7"))


graph_visdiverse <-  graph_visual_diverse + graph_visual_diverse_diff
ggsave(graph_visdiverse, file="graph_visdiverse.png", width=7, height=6)



