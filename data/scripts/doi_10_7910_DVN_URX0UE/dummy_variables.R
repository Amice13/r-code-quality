## Generates some dummy variables (for DPL controls)

raw$age_65_plus = as.numeric(raw$age=="65_plus")
raw$age_45_to_64 = as.numeric(raw$age=="45_to_64")
raw$age_25_to_44 = as.numeric(raw$age=="25_to_44")
raw$age_17_to_24 = as.numeric(raw$age=="17_to_24")

raw$standard_political_party_1 = as.numeric(raw$standard_political_party=="1")
raw$standard_political_party_2 = as.numeric(raw$standard_political_party=="2")
raw$standard_political_party_3 = as.numeric(raw$standard_political_party=="3")
raw$standard_political_party_4 = as.numeric(raw$standard_political_party=="4")

raw$region_1 = as.numeric(raw$region==1)
raw$region_2 = as.numeric(raw$region==2)
raw$region_3 = as.numeric(raw$region==3)
raw$region_4 = as.numeric(raw$region==4)

raw$safety_mask_in_always = as.numeric(raw$safety_mask_in=="Always")
raw$safety_mask_in_never = as.numeric(raw$safety_mask_in=="Never")
raw$safety_mask_in_sometimes = as.numeric(raw$safety_mask_in=="Sometimes")
raw$safety_mask_in_half_time = as.numeric(raw$safety_mask_in=="About half the time")
raw$safety_mask_in_most_of_time = as.numeric(raw$safety_mask_in=="Most of the time")

raw$safety_mask_out_always = as.numeric(raw$safety_mask_out=="Always")
raw$safety_mask_out_never = as.numeric(raw$safety_mask_out=="Never")
raw$safety_mask_out_sometimes = as.numeric(raw$safety_mask_out=="Sometimes")
raw$safety_mask_out_half_time = as.numeric(raw$safety_mask_out=="About half the time")
raw$safety_mask_out_most_of_time = as.numeric(raw$safety_mask_out=="Most of the time")

raw$safety_hands_always = as.numeric(raw$safety_hands=="Always")
raw$safety_hands_never = as.numeric(raw$safety_hands=="Never")
raw$safety_hands_sometimes = as.numeric(raw$safety_hands=="Sometimes")
raw$safety_hands_half_time = as.numeric(raw$safety_hands=="About half the time")
raw$safety_hands_most_of_time = as.numeric(raw$safety_hands=="Most of the time")

raw$safety_distance_always = as.numeric(raw$safety_distance=="Always")
raw$safety_distance_never = as.numeric(raw$safety_distance=="Never")
raw$safety_distance_sometimes = as.numeric(raw$safety_distance=="Sometimes")
raw$safety_distance_half_time = as.numeric(raw$safety_distance=="About half the time")
raw$safety_distance_most_of_time = as.numeric(raw$safety_distance=="Most of the time")

raw$own_reusable_masks_yes_more_than_3 = as.numeric(raw$own_reusable_masks=="Yes, more than 3 in the home.")
raw$own_reusable_masks_yes_1_3 = as.numeric(raw$own_reusable_masks=="Yes, between 1 and 3 in the home.")
raw$own_reusable_masks_no= as.numeric(raw$own_reusable_masks=="No")

raw$persons_met_none = as.numeric(raw$persons_met=="None")
raw$persons_met_1_3 = as.numeric(raw$persons_met=="1-3")
raw$persons_met_4_10 = as.numeric(raw$persons_met=="4-10")
raw$persons_met_11_20 = as.numeric(raw$persons_met=="11-20")
raw$persons_met_21_30 = as.numeric(raw$persons_met=="21-30")
raw$persons_met_31_or_more = as.numeric(raw$persons_met=="31 or more")




lasso_controls =c("standard_political_party_1",
                  "standard_political_party_2",
                  "standard_political_party_3",
                  "standard_political_party_4",
                  "standard_education",
                  "standard_hhi",
                  "age_65_plus",
                  "age_45_to_64",
                  "age_25_to_44",
                  "age_17_to_24",
                  "safety_mask_in_always","safety_mask_in_never","safety_mask_in_sometimes","safety_mask_in_half_time","safety_mask_in_most_of_time",
                  "safety_mask_out_always","safety_mask_out_never","safety_mask_out_sometimes","safety_mask_out_half_time","safety_mask_out_most_of_time",
                  "safety_hands_always","safety_hands_never","safety_hands_sometimes","safety_hands_half_time","safety_hands_most_of_time",
                  "safety_distance_always","safety_distance_never","safety_distance_sometimes","safety_distance_half_time","safety_distance_most_of_time",
                  
                  "own_reusable_masks_yes_more_than_3",
                  "own_reusable_masks_yes_1_3",
                  "own_reusable_masks_no",
                  
                  "persons_met_none",
                  "persons_met_1_3", 
                  "persons_met_4_10",
                  "persons_met_11_20",
                  "persons_met_21_30", 
                  "persons_met_31_or_more", 
                  "region_1",
                  "region_2",
                  "region_3",
                  "region_4",
                  "time_quartile","public_eat",
                  "public_movie",
                  "public_groceries",
                  "public_shopping",
                  "public_transit_work",
                  "public_transit_personal",
                  "own_disposable_masks"
)

for (var in lasso_controls) { # we do not want to lose observations because of missing controls
  raw[[var]][is.na(raw[[var]])] <- 0
}
