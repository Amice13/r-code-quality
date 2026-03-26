library(quanteda)
library("quanteda.dictionaries")
library(arrow)
library(here)
library(dplyr)

all_sentences <- read_feather(here("data", "source", "manifesto_corpus.feather"))

score_original_languages <- function(given_language) {
    filtered_sentences <- all_sentences |>
        filter(language_iso == given_language)

    corp_filtered <- corpus(
        filtered_sentences,
        text_field = "text"
    )

    docnames(corp_filtered) <- filtered_sentences$id_for_project

    mfd_dict <- quanteda::dictionary(
        file =
            here("data", "mfd", paste0("mfd_", given_language, ".dic")),
        format = "LIWC"
    )
    # Step 1: Tokenize the corpus
    tokens_obj <- tokens(corp_filtered, remove_punct = TRUE)

    # Step 2: Remove stopwords
    tokens_cleaned <- tokens_remove(tokens_obj, pattern = stopwords(given_language, source = "nltk"))

    # Step 3: Convert the cleaned tokens back to a character vector (text)
    cleaned_text <- sapply(tokens_cleaned, paste, collapse = " ")

    # Step 4: Create a new corpus from the cleaned text, retaining the original docvars
    corpus_cleaned <- corpus(cleaned_text, docvars = docvars(corp_filtered))

    result <- liwcalike(
        corpus_cleaned,
        mfd_dict
    )
    arrow::write_feather(
        result,
        sink = here("data", "mfd", paste0("mfd_", given_language, "_results.feather"))
    )
}


for (language in c("en", "nl", "de", "es")) {
    score_original_languages(language)
}
