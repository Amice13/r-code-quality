library(magrittr); library(tidyverse)

# 2016 ACS margins
ACS_2016 <- rio::import("weights/ACS_2016_1yr_LatinoCounts.csv")

ACS_2016 %<>%
  mutate(Origin = plyr::mapvalues(Origin, c("Colom", "Dom", "Guat", "Hispanic", "OtherCentral", "OtherSouth", "Salv", "Span"), rep("Oth", 8))) 

ACS_2016 %<>%
  group_by(State_Abr, Origin, ForeignBorn, SEX, age_group, Education) %>%
  dplyr::summarize(WeightedN = sum(WeightedN))

ACS_2016 %<>%
  mutate(Cub = as.numeric(Origin == "Cub"),
         Mex = as.numeric(Origin == "Mex"),
         PR = as.numeric(Origin == "PR"),
         Oth = as.numeric(Origin == "Oth")) %>%
  dplyr::select(-Origin) 

age_margins <- ACS_2016 %>%
  group_by(age_group) %>%
  dplyr::summarize(group_sum = sum(WeightedN)) %>%
  mutate(proportion = group_sum/sum(group_sum)) %>%
  dplyr::select(-group_sum) %>%
  spread(age_group, proportion)

origin_margins <- ACS_2016 %>%
  group_by(Origin) %>%
  dplyr::summarize(group_sum = sum(WeightedN)) %>%
  mutate(proportion = group_sum/sum(group_sum)) %>%
  dplyr::select(-group_sum) %>%
  spread(Origin, proportion)

education_margins <- ACS_2016 %>%
  group_by(Education) %>%
  dplyr::summarize(group_sum = sum(WeightedN)) %>%
  mutate(proportion = group_sum/sum(group_sum)) %>%
  dplyr::select(-group_sum) %>%
  spread(Education, proportion)

state_margins <- ACS_2016 %>%
  group_by(State_Abr) %>%
  dplyr::summarize(group_sum = sum(WeightedN)) %>%
  mutate(proportion = group_sum/sum(group_sum)) %>%
  dplyr::select(-group_sum) %>%
  spread(State_Abr, proportion)

SEX <- sum(as.numeric(ACS_2016$SEX == 2)*(ACS_2016$WeightedN/sum(ACS_2016$WeightedN)))
ForeignBorn <- sum(as.numeric(ACS_2016$ForeignBorn == 1)*(ACS_2016$WeightedN/sum(ACS_2016$WeightedN)))

margins <- tibble(V101 = 1, age_margins, origin_margins, education_margins, SEX, ForeignBorn) %>%
  bind_cols(state_margins, sample = 1) 

write.csv(margins, "weights/ACS_2016_margins.csv")

# 2019 ACS margins
ACS_2019 <- rio::import("weights/ACS_2019_1yr_LatinoCounts.csv")

ACS_2019 %<>%
  mutate(Origin = plyr::mapvalues(Origin, c("Colom", "Dom", "Guat", "Hispanic", "OtherCentral", "OtherSouth", "Salv", "Span"), rep("Oth", 8))) 

ACS_2019 %<>%
  group_by(State_Abr, Origin, ForeignBorn, SEX, age_group, Education) %>%
  dplyr::summarize(WeightedN = sum(WeightedN))

ACS_2019 %<>%
  mutate(Cub = as.numeric(Origin == "Cub"),
         Mex = as.numeric(Origin == "Mex"),
         PR = as.numeric(Origin == "PR"),
         Oth = as.numeric(Origin == "Oth")) %>%
  dplyr::select(-Origin) 

age_margins <- ACS_2019 %>%
  group_by(age_group) %>%
  dplyr::summarize(group_sum = sum(WeightedN)) %>%
  mutate(proportion = group_sum/sum(group_sum)) %>%
  dplyr::select(-group_sum) %>%
  spread(age_group, proportion)

origin_margins <- ACS_2019 %>%
  group_by(Origin) %>%
  dplyr::summarize(group_sum = sum(WeightedN)) %>%
  mutate(proportion = group_sum/sum(group_sum)) %>%
  dplyr::select(-group_sum) %>%
  spread(Origin, proportion)

education_margins <- ACS_2019 %>%
  group_by(Education) %>%
  dplyr::summarize(group_sum = sum(WeightedN)) %>%
  mutate(proportion = group_sum/sum(group_sum)) %>%
  dplyr::select(-group_sum) %>%
  spread(Education, proportion)

state_margins <- ACS_2019 %>%
  group_by(State_Abr) %>%
  dplyr::summarize(group_sum = sum(WeightedN)) %>%
  mutate(proportion = group_sum/sum(group_sum)) %>%
  dplyr::select(-group_sum) %>%
  spread(State_Abr, proportion)

SEX <- sum(as.numeric(ACS_2019$SEX == 2)*(ACS_2019$WeightedN/sum(ACS_2019$WeightedN)))
ForeignBorn <- sum(as.numeric(ACS_2019$ForeignBorn == 1)*(ACS_2019$WeightedN/sum(ACS_2019$WeightedN)))

margins <- tibble(V101 = 1, age_margins, origin_margins, education_margins, SEX, ForeignBorn) %>%
  bind_cols(state_margins, sample = 1) 

write.csv(margins, "weights/ACS_2019_margins.csv")
