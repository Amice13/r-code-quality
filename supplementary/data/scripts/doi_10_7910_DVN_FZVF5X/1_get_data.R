library(manifestoR)
library(arrow)
library(here)
library(data.table)
library(knitr)

print("Script now downloads from the Manifesto corpus")

mp_setapikey(key = Sys.getenv("manifesto_project_key"))

# get main dataset
meta_data1 <- mp_maindataset()
# get meta data suitable for plugging into corpus and filtering by language
meta_data2 <- mp_metadata(meta_data1)
filtered_df <- meta_data2[meta_data2$language %in% c("english", "german", "spanish", "dutch"), ]
# get original language data
my_corpus_df <- mp_corpus(filtered_df,
    as_tibble = TRUE
)
# get english translation from MARPOR
my_corpus_df_en <- mp_corpus(filtered_df,
    as_tibble = TRUE, translation = "en"
)
my_corpus_df_en$text_en <- my_corpus_df_en$text
# Add english texts to original language df
total_corpus <- merge(my_corpus_df, my_corpus_df_en[c("text_en", "pos", "manifesto_id")], by = c("manifesto_id", "pos"))
# filter out those that are not annotated by MARPOR
total_corpus <- total_corpus[!is.na(total_corpus$cmp_code), ]
# filter out headlines or sentences that are not meaningful
total_corpus <- total_corpus[!total_corpus$cmp_code %in% c("H", "0", "000"), ]

# create a manifesto_id for merger of the additional party data
create_manifesto_id <- function(party_id, date_manifesto) {
    paste0(as.character(party_id), "_", as.character(date_manifesto))
}
meta_data1$manifesto_id <- mapply(create_manifesto_id, meta_data1$party, meta_data1$date)
# which meta columns to keep
columns_of_interest_meta_data <- c("country", "countryname", "edate", "partyname", "partyabbrev", "parfam", "manifesto_id")
# merge it all
total_all_data <- merge(total_corpus, meta_data1[columns_of_interest_meta_data], by = "manifesto_id")
total_all_data$id_for_project <- seq.int(nrow(total_all_data))

languages_dict <- c(
    "english" = "en",
    "dutch" = "nl", "german" = "de",
    "spanish" = "es"
)

total_all_data$language_iso <- languages_dict[as.character(total_all_data$language)]


write_feather(total_all_data, here("data", "source", "manifesto_corpus.feather"))

print("Saved all, done.")
