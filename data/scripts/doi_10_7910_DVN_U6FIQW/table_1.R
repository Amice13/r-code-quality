##TikTok Identity Study - Table 1
##R version 4.2.2
##Windows operating system

#library
library(readxl)
library(dplyr)

library(readxl) #version 1.4.1
library(dplyr) #version 1.0.10

#open data set
data <- read_xlsx("transcribing_tiktok_openai_with_final_dataset_0427_anonymized.xlsx")

###CAPTIONS (left column)

#selecting relevant columns 
data_identity_desc <- data %>%
  select(conservative_desc_count, conspiracy_desc_count, relational_desc_count, vaccine_injured_identity_desc_count, geographical_desc_count, partisan_desc_count, republican_desc_count, domestic_desc_count, liberal_desc_count, maternal_desc_count, professional_desc_count, gender_desc_count, international_desc_count, religious_desc_count, medical_professional_desc_count, democrat_desc_count, generational_desc_count, sexual_orientation_desc_count, race_desc_count)

identities_desc <- apply(data_identity_desc, 2, function(x) sum(x > 0))
data_identity_desc_table <- data.frame(Column = names(identities_desc), Count = identities_desc)
data_identity_desc_table
##Output was manually transcribed from R output to Table 1 in Word


####TRANSCRIPTS (right column)

#selecting relevant columns 
data_identity_trans <- data %>%
  select(relational_openai_trans_count, geographical_openai_trans_count, domestic_openai_trans_count, professional_openai_trans_count, gender_openai_trans_count, generational_openai_trans_count, conservative_openai_trans_count, maternal_openai_trans_count, medical_professional_openai_trans_count, religious_openai_trans_count, international_openai_trans_count, conspiracy_openai_trans_count, democrat_openai_trans_count, race_openai_trans_count, vaccine_injured_identity_openai_trans_count, republican_openai_trans_count, liberal_openai_trans_count, sexual_orientation_openai_trans_count)

identities_trans <- apply(data_identity_trans, 2, function(x) sum(x > 0))
data_identity_trans_table <- data.frame(Column = names(identities_trans), Count = identities_trans)
data_identity_trans_table
##Output was manually transcribed from R output to Table 1 in Word


