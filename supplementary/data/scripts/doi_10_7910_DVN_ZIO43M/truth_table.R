#######################################################################
# Copyright (C) 2018  George Githinji

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#######################################################################


library(tidyverse)

combine_datasets <- function(reference_positions,spiked_variants,all_called_variants){
  ref_pos_with_spiked_variants <- full_join(reference_positions,spiked_variants,by ="position")
  with_variant_calls <- full_join(ref_pos_with_spiked_variants,all_called_variants,by="position") %>%
    rename(gold_variant = alternative.x,
           gold_freq = freq.x,
           called_variant = alternative.y,
           reference = reference.x,
           called_frequency = freq.y)
}

diagnostic_metrics <- function(variants_data,sample_name,variant_caller){
  truth_table <- variants_data %>%
    mutate(count_TP = if_else(as.character(gold_variant) == as.character(called_variant),1,0),
           count_FP = if_else(is.na(gold_variant) & !is.na(called_variant),1,0),
           count_FN = if_else(!is.na(gold_variant) & is.na(called_variant),1,0),
           count_TN = if_else(is.na(gold_variant) & is.na(called_variant),1,0)
    ) %>%
    summarise(TP = sum(count_TP,na.rm = T),
              TN = sum(count_TN,na.rm = T),
              FP = sum(count_FP,na.rm = T),
              FN = sum(count_FN,na.rm = T)
    ) %>%
    mutate(sensitivity = TP / (TP + FN),
           specificity = TN / (TN + FP),
           precision = TP / (TP + FP),
           FPR = FP / (TN +FP),
           TP_plus_FN = TP + FN,
           accuracy = (TP + TN) /(TP + FP + TN + FN),
           sample = sample_name,
           caller = variant_caller
    ) %>% arrange(desc(sample))
  return(truth_table)
}

samples <- c("sample1","sample2","sample3","sample4","sample5","sample6","sample7","sample8")
callers <- c("freebayes","lofreq","vardict","varscan")
all_combinations <- expand.grid(samples,callers)
reference_positions <- read_csv("data/reference_in_column.csv")

spiked_variants <- read_csv("data/variant.list.csv")

#   mutate(bin = if_else(freq < 0.05,"low",
#                        if_else(freq >=0.05 & freq <= 0.25,"moderate",
#                                if_else(freq > 0.25,"standard","error")))) %>%
#   filter(bin == "low")

spiked_variants <- read_csv("data/new_minority_datasets/minority_variant_list.csv") #%>%
 # mutate(bin = if_else(freq < 0.05,"low",
   #                    if_else(freq >=0.05 & freq <= 0.25,"moderate",
   #                           if_else(freq > 0.25,"standard","error")))) %>%
 # filter(bin == "standard")

#all_called_variants <- read_csv("data/error_profile_data/error_profile0_data/all_error_profile0_variants.csv") %>%
#all_called_variants <- read_csv("data/error_profile_data/error_profile1_data/all_error_profile1_variants.csv") %>%
#all_called_variants <- read_csv("data/error_profile_data/error_profile2_data/all_error_profile2_variants.csv") %>%

all_called_variants <- read_csv("data/new_minority_datasets/artificial_dataset3/artificial_dataset3_variants.csv") %>%
  mutate(bin = if_else(freq < 0.05,"low",
                       if_else(freq >=0.05 & freq <= 0.25,"moderate",
                               if_else(freq > 0.25,"standard","error")))) %>%
  filter(bin == "standard")

get_dataset <- function(all_called_variants,reference_positions,spiked_variants,sample_name,variant_caller){
  filtered_called_variants <- all_called_variants %>%
    filter(sampleName == sample_name & caller == variant_caller)
  dataset <- combine_datasets(reference_positions,spiked_variants,filtered_called_variants)
  return(dataset)
}

calculate_diagnostics <- function(dataset1,sample_name,variant_caller){
  return(diagnostic_metrics(dataset1,sample_name,variant_caller))
}

mylist <- list() #create an empty list
for (row in 1:nrow(all_combinations)) {
  sample_name <- droplevels(all_combinations[row,"Var1"])
  variant_caller <- droplevels(all_combinations[row,"Var2"])
  dataset <- get_dataset(all_called_variants,reference_positions,spiked_variants,sample_name,variant_caller)
  mylist[[row]] <- calculate_diagnostics(dataset,sample_name,variant_caller)
}

metrics.dta <- do.call(rbind.data.frame, mylist)

#write_csv(x=metrics.dta,path="data/dataset3_low_truth_table.csv")ß
#write_csv(x=metrics.dta,path = "data/new_minority_datasets/artificial_dataset3/minority_dataset3_standard_truth_table.csv")
