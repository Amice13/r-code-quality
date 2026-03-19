# Install and load required packages
library(foreach)
library(doParallel)
library(readr)
library(tidyverse)

setwd("C:/Users/wxl00/OneDrive - Dickinson College/Song Lyrics Project/How-Emotions-Are-Coded")
setwd("C:/Users/wangx/OneDrive - Dickinson College/Song Lyrics Project/How-Emotions-Are-Coded")

df <- read_csv("Final Datasets/df7_3EmotionsCH_region_singerType_Year2.csv")
df$region_coarse[is.na(df$region_coarse)] <- 'unknown'
df$region[is.na(df$region)] <- 'unknown'

dF <- read_csv('Final Datasets/df7_3EmotionsCH_split_synonym_expanded_emo scores_final.csv')
dF <- dF %>% filter(Year2 >= 1967) %>%
  filter(IntensityWithSign <= 15, IntensityWithSign >= -15)
dF$region_coarse[is.na(dF$region_coarse)] <- 'unknown'
dF$region[is.na(dF$region)] <- 'unknown'

Df <- read_csv("Final Datasets/df7_Ch_Split_region_singerType_Year2_per_100Chars.csv")
Df$region_coarse[is.na(Df$region_coarse)] <- 'unknown'
Df$region[is.na(Df$region)] <- 'unknown'

DF <- read_csv('Final Datasets/df7_ch_split_emo scores_final_per_100Chars.csv')
DF$region_coarse[is.na(DF$region_coarse)] <- 'unknown'
DF$region[is.na(DF$region)] <- 'unknown'


DF <- DF %>% filter(Year2 >= 1967)

dF <- dF %>% semi_join(DF, by = c("id" = "id"))
DF <- DF %>% semi_join(dF, by = c("id" = "id")) 

rm(df, dF, Df)


tp <- DF %>% 
  # group_by(singerType, region, Year2) %>%
  # sample_frac(0.2) %>%
  select(PA, PE, PD, PH, PG, PB, PK, PF, NPF, NAA, NJ, NB, NH, NE, ND, NN, NK, NL, NPC, PC, NI, NC, NG) %>%
  rename(happiness_PA = PA,
         contentment_PE = PE,
         respect_PD = PD,
         praise_PH = PH,
         trust_PG = PG,
         fondness_PB = PB,
         wishing_PK = PK,
         longing_PF = PF,
         missing_NPF = NPF,
         anger_NA = NAA,
         sorrow_NB = NB,
         disappointment_NJ = NJ,
         guilt_NH = NH,
         annoyance_NE = NE,
         aversion_ND =ND,
         criticism_NN = NN,
         envy_NK = NK,
         doubt_NL = NL,
         amazement_PC = PC,
         shock_NPC = NPC,
         panic_NI = NI,
         dread_NC = NC,
         shame_NG = NG
  )


# Define the function for processing a single row
process_row <- function(row) {
  from <- NULL
  to <- NULL
  temp <- names(row)[row != 0]
  if(length(temp) > 1) {
    for (i in 1:(length(temp)-1)) {
      for (j in (i+1):length(temp)) {
        from <- c(from, temp[i])
        to <- c(to, temp[j])
      }
    }
  }
  data.frame(from, to)
}

# Define the function for processing a chunk
process_chunk <- function(tp_chunk, chunk_number) {
  # Print the current chunk being processed
  print(paste("Processing chunk number:", chunk_number))
  
  # Register the parallel backend within this function
  cl <- makeCluster(detectCores() - 1)  # Use all cores except one
  registerDoParallel(cl)
  
  results <- foreach(k = 1:nrow(tp_chunk), .combine = rbind, .export = 'process_row') %dopar% {
    process_row(tp_chunk[k, ])
  }
  
  # Stop the cluster
  stopCluster(cl)
  
  # Save the result to a CSV file
  output_filename <- paste0("Final Datasets/subEmo_connections_lyric_split", chunk_number, ".csv")
  write_csv(results, output_filename)
  
  # Print completion of the current chunk
  print(paste("Finished processing chunk number:", chunk_number))
}

# Split the dataframe into 27 chunks
num_chunks <- 27
chunk_size <- ceiling(nrow(tp) / num_chunks)
tp_chunks <- split(tp, rep(1:num_chunks, each = chunk_size, length.out = nrow(tp)))

# Process each chunk sequentially
for (chunk_number in 1:num_chunks) {
  tp_chunk <- tp_chunks[[chunk_number]]
  process_chunk(tp_chunk, chunk_number)
}

print("All chunks processed.")

# Initialize an empty list to store dataframes
all_data <- list()

# Directory containing the CSV files
dir_path <- "Final Datasets/"

# Loop over each file and read it into a dataframe, then store in the list
for (i in 1:27) {
  file_name <- paste0(dir_path, "subEmo_connections_lyric_split", i, ".csv")
  temp_data <- read_csv(file_name)
  all_data[[i]] <- temp_data
}

# Combine all dataframes into a single dataframe
combined_data <- do.call(rbind, all_data)

# Save the combined dataframe to a new CSV file
output_file <- paste0(dir_path, "subEmo_connections_lyric_split.csv")
write_csv(combined_data, output_file)

print("All CSV files have been combined into subEmo_connections_lyric_split.csv")

