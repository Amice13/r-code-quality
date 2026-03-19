rm(list = ls())

################################################################################

# Packages

library('peRspective')

################################################################################

# Load data frames containing QP transcripts (as built in Transcripts.R)

load('Data\df_English.Rdata')

load('Data\df_French.Rdata')

################################################################################

API_key <- 'AIzaSyB6q9STwgXR1A_M0FccB9p1OIzoDA-UzZw'

# Generate Perspective Scores for English transcripts

prsp_English <- df_English %>%
  prsp_stream(text = Text,
              text_id = ID,
              languages = 'en',
              score_model = peRspective::prsp_models,
              key = API_key,
              safe_output = TRUE,
              verbose = TRUE)

# Save the resulting data frame

save(prsp_English, file = 'Data\prsp_English.Rdata')

# Generate Perspective Scores for French transcripts

prsp_French <- df_French %>%
  prsp_stream(text = Text,
              text_id = ID,
              languages = 'fr',
              score_model = peRspective::prsp_models,
              key = API_key,
              safe_output = TRUE,
              verbose = TRUE)

# Save the resulting data frame

save(prsp_French, file = 'Data\prsp_French.Rdata')
