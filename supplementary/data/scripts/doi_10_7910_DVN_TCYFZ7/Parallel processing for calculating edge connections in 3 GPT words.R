# Install and load required packages
library(foreach)
library(doParallel)
library(readr)

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

rm(df, DF, Df)

tp <- dF %>% 
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

# Register the parallel backend
cl <- makeCluster(detectCores() - 1)  # Use all cores except one
registerDoParallel(cl)

# Process the entire dataframe in parallel
results <- foreach(k = 1:nrow(tp), .combine = rbind, .export = 'process_row') %dopar% {
  process_row(tp[k, ])
}

# Stop the cluster
stopCluster(cl)

# Save the combined results to a CSV file
output_file <- "Final Datasets/subEmo_connections_gpt.csv"
write_csv(results, output_file)

print("The entire dataframe has been processed and saved to subEmo_connections_gpt_parallel.csv")
