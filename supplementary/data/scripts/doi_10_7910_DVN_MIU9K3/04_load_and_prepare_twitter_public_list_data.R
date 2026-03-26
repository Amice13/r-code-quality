#!/usr/bin/Rscript

# Replication code for:
# Bailo, F., & Vromen, A. (2016). 
# Hybrid social and news media protest events: from #MarchinMarch to #BusttheBudget in Australia, 
# Information, Communication & Society, 1–20. https://doi.org/10.1080/1369118X.2016.1252410

# 04_load_and_prepare_twitter_public_list_data.R
## Updated: 2016-11-11 16:01:53 AEDT

setwd('~/Documents/replication_packages/ics_2016')

# List of twitter lists in the database
sqliteGetTable <- function(database, table) {
  require(DBI)
  require(RSQLite)
  con <- dbConnect(RSQLite::SQLite(), dbname = database)
  query <- dbSendQuery(con, paste("SELECT * FROM ", table, ";", sep="")) 
  result <- fetch(query, n = -1)
  dbClearResult(query)
  dbDisconnect(con)
  return(result)
}

sqliteListTables <- function(database, table) {
  require(DBI)
  require(RSQLite)
  con <- dbConnect(RSQLite::SQLite(), dbname = database)
  return(dbListTables(con))
}

aus_twt_list_db <- "twitter_public_lists.sqlite"
tables <- sqliteListTables(aus_twt_list_db)

twt_lists <- list()
for (table in tables) {
  twt_lists[[table]] <- sqliteGetTable(aus_twt_list_db, table)
}

macro_labels <- c("news", "auspol", "union", "social-mov")
micro_labels <- c("news media", "journalists", "pol-independent",
                  "pol-national-party","pol-greens-party","pol-labor-party",
                  "pol-minor-party","pol-liberal-party","not-for-profit",
                  "social-change-activists","eco-movements","progress-2013")
macro_sets <- list()
macro_sets[['news']] <- 
  unique(c(twt_lists[['GregBarila_australia_journalists']]$id,
           twt_lists[['aujournos_australianjournalists']]$id,
           twt_lists[['latikambourke_australian_journalists']]$id,
           twt_lists[['latikambourke_journalistsoverseas']]$id,
           twt_lists[['latikambourke_media_programs']]$id,
           twt_lists[['latikambourke_news']]$id,
           twt_lists[['latikambourke_press_gallery']]$id,
           twt_lists[['pinglo_abc_people']]$id,
           twt_lists[['smh_fairfax_media_journalists']]$id,
           twt_lists[['captaineagle_media_oz']]$id))
macro_sets[['auspol']] <-
  unique(c(twt_lists[['GhostWhoVotes_aus_greens_party']]$id,
           twt_lists[['GhostWhoVotes_aus_independents']]$id,
           twt_lists[['GhostWhoVotes_aus_labor_party']]$id,
           twt_lists[['GhostWhoVotes_aus_liberal_party']]$id,
           twt_lists[['GhostWhoVotes_aus_minor_parties']]$id,
           twt_lists[['GhostWhoVotes_aus_national_party']]$id,
           twt_lists[['kevinperry_politicians']]$id,
           twt_lists[['latikambourke_authorities']]$id,
           twt_lists[['latikambourke_politicians']]$id,
           twt_lists[['newscomauHQ_federal_politicians']]$id,
           twt_lists[['smh_federal_politicians']]$id))          
macro_sets[['union']] <-
  unique(c(twt_lists[['ShoebridgeMLC_unions']]$id,
           twt_lists[['jamesmassola_unions']]$id))
macro_sets[['social-mov']] <-
  unique(c(twt_lists[['GreenpeaceAustP_eco_peeps']]$id,
           twt_lists[['GreenpeaceAustP_not_for_profits']]$id,
           twt_lists[['GreenpeaceAustP_social_change_activists_14']]$id))

micro_sets <- list()
micro_sets[['news_media']] <- 
  unique(c(twt_lists[['latikambourke_media_programs']]$id,
           twt_lists[['latikambourke_news']]$id))
micro_sets[['journalists']] <- 
  unique(c(twt_lists[['GregBarila_australia_journalists']]$id,
           twt_lists[['aujournos_australianjournalists']]$id,
           twt_lists[['smh_fairfax_media_journalists']]$id,
           twt_lists[['latikambourke_journalistsoverseas']]$id))
micro_sets[['pol-independent']] <- 
  unique(c(twt_lists[['GhostWhoVotes_aus_independents']]$id))
micro_sets[['pol-national-party']] <- 
  unique(c(twt_lists[['GhostWhoVotes_aus_national_party']]$id))
micro_sets[['pol-greens-party']] <- 
  unique(c(twt_lists[['GhostWhoVotes_aus_greens_party']]$id))
micro_sets[['pol-labor-party']] <- 
  unique(c(twt_lists[['GhostWhoVotes_aus_labor_party']]$id))
micro_sets[['pol-minor-party']] <- 
  unique(c(twt_lists[['GhostWhoVotes_aus_minor_parties']]$id))
micro_sets[['pol-liberal-party']] <- 
  unique(c(twt_lists[['GhostWhoVotes_aus_liberal_party']]$id))
micro_sets[['not-for-profit']] <- 
  unique(c(twt_lists[['GreenpeaceAustP_not_for_profits']]$id))
micro_sets[['social-change-activists']] <- 
  unique(c(twt_lists[['GreenpeaceAustP_social_change_activists_14']]$id))
micro_sets[['eco-movements']] <- 
  unique(c(twt_lists[['GreenpeaceAustP_eco_peeps']]$id))
micro_sets[['progress-2013']] <- 
  unique(c(twt_lists[['laramcpherson_progress2013']]$id))

# Create a list with screen names

macro_sets_screen_name <- list()
macro_sets_screen_name[['news']] <- 
  unique(c(twt_lists[['GregBarila_australia_journalists']]$screen_name,
           twt_lists[['aujournos_australianjournalists']]$screen_name,
           twt_lists[['latikambourke_australian_journalists']]$screen_name,
           twt_lists[['latikambourke_journalistsoverseas']]$screen_name,
           twt_lists[['latikambourke_media_programs']]$screen_name,
           twt_lists[['latikambourke_news']]$screen_name,
           twt_lists[['latikambourke_press_gallery']]$screen_name,
           twt_lists[['pinglo_abc_people']]$screen_name,
           twt_lists[['smh_fairfax_media_journalists']]$screen_name,
           twt_lists[['captaineagle_media_oz']]$screen_name))
macro_sets_screen_name[['auspol']] <-
  unique(c(twt_lists[['GhostWhoVotes_aus_greens_party']]$screen_name,
           twt_lists[['GhostWhoVotes_aus_independents']]$screen_name,
           twt_lists[['GhostWhoVotes_aus_labor_party']]$screen_name,
           twt_lists[['GhostWhoVotes_aus_liberal_party']]$screen_name,
           twt_lists[['GhostWhoVotes_aus_minor_parties']]$screen_name,
           twt_lists[['GhostWhoVotes_aus_national_party']]$screen_name,
           twt_lists[['kevinperry_politicians']]$screen_name,
           twt_lists[['latikambourke_authorities']]$screen_name,
           twt_lists[['latikambourke_politicians']]$screen_name,
           twt_lists[['newscomauHQ_federal_politicians']]$screen_name,
           twt_lists[['smh_federal_politicians']]$screen_name))          
macro_sets_screen_name[['union']] <-
  unique(c(twt_lists[['ShoebridgeMLC_unions']]$screen_name,
           twt_lists[['jamesmassola_unions']]$screen_name))
macro_sets_screen_name[['social-mov']] <-
  unique(c(twt_lists[['GreenpeaceAustP_eco_peeps']]$screen_name,
           twt_lists[['GreenpeaceAustP_not_for_profits']]$screen_name,
           twt_lists[['GreenpeaceAustP_social_change_activists_14']]$screen_name))

micro_sets_screen_name <- list()
micro_sets_screen_name[['news_media']] <- 
  unique(c(twt_lists[['latikambourke_media_programs']]$screen_name,
           twt_lists[['latikambourke_news']]$screen_name))
micro_sets_screen_name[['journalists']] <- 
  unique(c(twt_lists[['GregBarila_australia_journalists']]$screen_name,
           twt_lists[['aujournos_australianjournalists']]$screen_name,
           twt_lists[['smh_fairfax_media_journalists']]$screen_name,
           twt_lists[['latikambourke_journalistsoverseas']]$screen_name))
micro_sets_screen_name[['pol-independent']] <- 
  unique(c(twt_lists[['GhostWhoVotes_aus_independents']]$screen_name))
micro_sets_screen_name[['pol-national-party']] <- 
  unique(c(twt_lists[['GhostWhoVotes_aus_national_party']]$screen_name))
micro_sets_screen_name[['pol-greens-party']] <- 
  unique(c(twt_lists[['GhostWhoVotes_aus_greens_party']]$screen_name))
micro_sets_screen_name[['pol-labor-party']] <- 
  unique(c(twt_lists[['GhostWhoVotes_aus_labor_party']]$screen_name))
micro_sets_screen_name[['pol-minor-party']] <- 
  unique(c(twt_lists[['GhostWhoVotes_aus_minor_parties']]$screen_name))
micro_sets_screen_name[['pol-liberal-party']] <- 
  unique(c(twt_lists[['GhostWhoVotes_aus_liberal_party']]$screen_name))
micro_sets_screen_name[['not-for-profit']] <- 
  unique(c(twt_lists[['GreenpeaceAustP_not_for_profits']]$screen_name))
micro_sets_screen_name[['social-change-activists']] <- 
  unique(c(twt_lists[['GreenpeaceAustP_social_change_activists_14']]$screen_name))
micro_sets_screen_name[['eco-movements']] <- 
  unique(c(twt_lists[['GreenpeaceAustP_eco_peeps']]$screen_name))
micro_sets_screen_name[['progress-2013']] <- 
  unique(c(twt_lists[['laramcpherson_progress2013']]$screen_name))

save(macro_sets, micro_sets, macro_sets_screen_name, micro_sets_screen_name, 
     file="twitter_public_lists.RData")


# Print out names and summary statistics
macro_names <- names(macro_sets_screen_name)
micro_names <- names(micro_sets_screen_name)

for (macro_name in macro_names) {
  cat(paste0(macro_name," n=", length(macro_sets_screen_name[[macro_name]])), "\n")
}

for (micro_name in micro_names) {
  cat(paste0(micro_name," n=", length(micro_sets_screen_name[[micro_name]]), "\n"))
}

for (macro_name in macro_names) {
  cat(paste0(macro_name, ": ", paste(sort(macro_sets_screen_name[[macro_name]]), collapse="; ")), "\n")
}

for (micro_name in micro_names) {
  cat(paste0(micro_name, ": ", paste(sort(micro_sets_screen_name[[micro_name]]), collapse="; ")), "\n")
}
