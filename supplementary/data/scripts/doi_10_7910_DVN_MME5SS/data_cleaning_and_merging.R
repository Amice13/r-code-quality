library(dplyr)
library(stringr)
library(stargazer)

# Load in Congressional Similarity score data
cs <- read.csv("data/congress_mayor_similarity_partisanship_topic_space_4th_full_timelimit.csv")
cs <- cs %>% rename(Twitter_handle = twitter_handler)

# Load in mayoral covariate data
mayors <- read.csv("data/mayors_data.csv")
mayors$Twitter_handle <- tolower(mayors$Twitter_handle) 

# Load in data on presidential results by state
pres16 <- read.csv("data/pres16_statelevel.csv")

# Load in city presidential proxy data
prescity <- read.csv("data/city_pres_share.csv")
prescity$City <- as.character(prescity$City)

# Merge datasets together
merged <- right_join(mayors, cs, by = "Twitter_handle")
merged <- left_join(merged, prescity, by = c("City","State"))

# Clean data
merged$X..of.Vote.in.Last.election <- str_replace_all(merged$X..of.Vote.in.Last.election, "[:alpha:]", "")
merged$X..of.Vote.in.Last.election <- str_replace_all(merged$X..of.Vote.in.Last.election, "/", "")
merged$X..of.Vote.in.Last.election <- str_replace_all(merged$X..of.Vote.in.Last.election, ":", "")
merged$X..of.Vote.in.Last.election <- str_trim(merged$X..of.Vote.in.Last.election)
merged$X..of.Vote.in.Last.election <- as.numeric(as.character(merged$X..of.Vote.in.Last.election))

merged$Election_type..1.partisan..0.nonpartisan.[merged$Election_type..1.partisan..0.nonpartisan.==4] <- NA

merged$presdiff <- abs(merged$clinton - merged$trump)
merged$presdiffcity <- abs(merged$wt_clinton_share - merged$wt_trump_share)

# Check for duplicates
duplicates_check <- merged %>%
  group_by(Full_name) %>%
  summarize(total = n())

# Remove duplicates
duplicates <- which(duplicates_check$total > 1)
duplicates_check[duplicates,]

which(merged$Full_name=="Chokwe Antar Lumumba Esq.") # 228, 229
which(merged$Full_name=="Dave Kleis") # 220, 221, 222
which(merged$Full_name=="Michael O'Connor") # 182, 183
which(merged$Full_name=="Richard C. Irvin") # 133, 134

merged <- merged[-c(228,220,221,182,133),]

write.csv(merged, file = "data/mayors_merged.csv")













