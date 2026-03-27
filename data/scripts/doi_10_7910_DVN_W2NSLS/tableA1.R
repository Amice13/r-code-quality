'''
This script compares the followers downloaded from Scripts/get_followers_persistence/02_zst_getFollowers_persistence_v1.py to those downloaded in 2020.  The goal is to measure persistence of followers.

Output: Table A1.
'''
##########################
##
##	GLOBALS
##
##########################
library(data.table)
library(dplyr)
library(tidyverse)  # For read_csv, pivot_wider
librar

setwd("~/Dropbox/Twitter_China2/")

# compareUsers <- function(user, orig_ids=orig_ids, new_ids=new_ids){
# 	# Get the corresponding file customizers
# 	orig <- orig_ids[grep(user, orig_ids)]
# 	new <- new_ids[grep(user, new_ids)]

# 	# Read original follower list, get only those in locations I care about
# 	df_orig <- fread(paste0('Data/top_followers_fastCrawl/China/', orig))
# 	df_orig <- subset(df_orig, grepl('China|Hong Kong|Taiwan', location_classified))

# 	# Read new follower list
# 	df_new <- fread(paste0('Data/top_followers_fastCrawl_persistence/', new))

# 	# Add flag
# 	df_orig$persistence <- ifelse(df_orig$user_id %in% df_new$user_id, 1, 0)

# 	return(df_orig)
# }


##########################
##
##	DATA
##
##########################
# Get filenames
orig_ids <- list.files(path='Data/top_followers_fastCrawl/China/', pattern='followerTracking_wLocation')
new_ids <- list.files(path='Data/top_followers_fastCrawl_persistence/', pattern='followerTracking')
new_ids_kc <- list.files(path='Data/top_followers_fastCrawl_persistence/kengchi', pattern='followerTracking')

# Get just ID numbers
orig_ids2 <- unlist(lapply(orig_ids, function(x) strsplit(x, '_')[[1]][3]))
new_ids2 <- unlist(lapply(new_ids, function(x) strsplit(x, '_')[[1]][2]))
new_ids_kc2 <- unlist(lapply(new_ids_kc, function(x) strsplit(x, '_')[[1]][2]))


# These IDs are in both
ids <- orig_ids2[orig_ids2 %in% new_ids2]
ids_kc <- orig_ids2[orig_ids2 %in% new_ids_kc2]
ids_kc <- ids_kc[!(ids_kc %in% ids)]  # Get rid of those Zack got

# Get index in original followers lists, keep that
# orig_index <- unlist(lapply(ids, function(x) grep(x, orig_ids)))
# orig_analyze <- orig_ids[orig_index]
# new_analyze <- new_ids # Renaming to keep the naming convention the same

# The popular accounts' category grouping
accounts = read_csv("Data/CN_popular_account_list.csv", col_types = "cnccccncccncnncc")

##########################
##
##	WORK
##
##########################

### ZACK
analyze = data.frame()

for(i in 1:length(ids)){
  print(paste('On user', as.character(i), sep=' '))
  # Get the corresponding file customizers
  orig <- orig_ids[grep(ids[i], orig_ids)]
  new <- new_ids[grep(ids[i], new_ids)]
  
  # Read original follower list, get only those in locations I care about
  df_orig <- fread(paste0('Data/top_followers_fastCrawl/China/', orig))
  df_orig <- subset(df_orig, grepl('China|Hong Kong|Taiwan', location_classified))
  
  # Read new follower list. Deduplicate because interrupted downloads would restart from beginning and therefore duplicate; this cleans that up.
  df_new <- fread(paste0('Data/top_followers_fastCrawl_persistence/', new))
  df_new <- df_new %>% distinct(user_id, .keep_all=TRUE)
  
  # Add persistence flag
  df_orig$persistence <- ifelse(as.character(df_orig$user_id) %in% as.character(df_new$user_id), 1, 0)
  
  # Add original account ID for later merging
  orig_id <- str_extract(orig, '[[:digit:]]+')
  df_orig$account <- orig_id
  
  #temp_df <- compareUsers(user=ids[i])
  analyze <- rbind(analyze, df_orig)
}

### DO FOR KENG-CHI'S
analyze_k = data.frame()

for(i in 1:length(ids_kc)){
  print(paste('On user', as.character(i), sep=' '))
  # Get the corresponding file customizers
  orig <- orig_ids[grep(ids_kc[i], orig_ids)]
  new <- new_ids_kc[grep(ids_kc[i], new_ids_kc)]
  
  # Read original follower list, get only those in locations I care about
  df_orig <- fread(paste0('Data/top_followers_fastCrawl/China/', orig))
  df_orig <- subset(df_orig, grepl('China|Hong Kong|Taiwan', location_classified))
  
  # Read new follower list. Deduplicate because interrupted downloads would restart from beginning and therefore duplicate; this cleans that up.
  df_new <- fread(paste0('Data/top_followers_fastCrawl_persistence/kengchi/', new))
  df_new <- df_new %>% distinct(user_id, .keep_all=TRUE)
  
  # Add persistence flag
  df_orig$persistence <- ifelse(as.character(df_orig$user_id) %in% as.character(df_new$user_id), 1, 0)
  
  # Add original account ID for later merging
  orig_id <- str_extract(orig, '[[:digit:]]+')
  df_orig$account <- orig_id
  
  #temp_df <- compareUsers(user=ids[i])
  analyze_k <- rbind(analyze_k, df_orig)
}

### COMBINE, DEDUPLICATE
analyze <- rbind(analyze, analyze_k)



### METADATA
# ADD IN ACCOUNT METADATA FOR THE POPULAR ACCOUNTS
analyze <- merge(x=analyze, y=accounts, by.x='account', by.y='id_str', all.x=TRUE, all.y=FALSE)

# DATA
analyze$follow_date <- substr(analyze$upper_bound, 1, 10)
analyze$weeks <- as.character(cut(as.Date(analyze$follow_date), breaks='week'))

# Drop results from NA types
analyze <- subset(analyze, is.na(type_complete) == FALSE)

##########################
##
##  MAKE PERSISTENCE TABLE
##
##########################
analyze$Period <- cut(as.Date(analyze$follow_date), breaks=c(as.Date(min(analyze$follow_date)), as.Date('2020-01-23'), as.Date('2020-03-13'), as.Date(max(analyze$follow_date))), labels=c('Pre-Lockdown', 'Lockdown', 'Post-Lockdown'), right=FALSE, include.lowest=TRUE)
result <- data.frame(analyze %>% group_by(Period, type_complete, location_classified) %>% summarize(Persistence=100*mean(persistence)))

# Reshape.  Want rows to be type_complete, columns to be hierarchical Period -> Location Classified
forpaper <- pivot_wider(data=result, id_cols=c('type_complete'), values_from='Persistence', names_from=c('Period', 'location_classified'))

xtable(forpaper, digits=2, caption='Persistence of Followers by Account Type and Period', align=c('l|cccccccccc'))
