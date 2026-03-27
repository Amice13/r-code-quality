# ---------------------------------- TITLE -------------------------------------
# lss_stringency_c.R
# AUTHOR: Jacob Winter & Mark Manger
# DATE: Jan 2022
# ENCODING: utf-8

# ---------------------------------- NOTES -------------------------------------
# Measures the stringency of Project Appraisal Documents based on preselected keywords.
# Uses Latent Semantic Scaling (Watanabe, 2020) to create SVD word embeddings and 
# measures similarity between each term and training terms.

# ---------------------------------- SETUP -------------------------------------
dir = "~/wckm_wb_replication_package/"
setwd(dir)

sink("log_figs.html")
library(tidyverse)
library(LSX)
library(stringr)
library(countrycode)
library(WDI)
library(quanteda)
options(scipen=999)


# ---------------------------------- Data --------------------------------------

conditions <- read_csv("data/conditions_to_share_full.csv")
keywords <- readxl::read_excel("data/stringency_keywords.xlsx", sheet="Coded_2022")

# ---------------------------------- CODE --------------------------------------

# Set keywords
scores <- keywords$Valence
names(scores) <- keywords$Term

#Set terms to be filtered in DFM creation
compound_tokens <- c("issue regulation", "present report", "submit proposal",
                     "take measures", "take steps")

custom_stops <- c("world", "bank", "project","people's","people.s",
                  "ibrd","de", "la", "des",
                  "na","us", "ii", "iii", "sector", "development",
                  "national", "program", "plan", "republika", "srpska", "orissa", "du", 
                  "punjab", "april", "mozambique", "africa",
                  "*\\)*", "(", "/", ":", ".", ",", "*-*", ";", "!", "_", "iv", "vi", "v", "\"", "////")

# ---------------------------------- SCRAPED CONDITIONS --------------------------------------
corpus <- corpus(conditions, docid_field = "new_id",
                 text_field = "conditions") 

#Get rid of sentences which have "definition content" in them (eg. "'X' mean that Y")
sentences <- corpus %>% 
  corpus_reshape(to = "sentences") %>% 
  data.frame() %>% 
  rownames_to_column() %>% 
  rename("text" = 2) %>% 
  mutate(definitions = ifelse(grepl("' means",text), TRUE, FALSE)) %>% 
  filter(definitions == FALSE) %>% 
  corpus(docid_field = "rowname",
         text_field = "text")

toks <- sentences %>% 
  tokens(remove_symbols = TRUE, remove_punct = FALSE, remove_numbers = TRUE,
         remove_separators = TRUE, remove_url = TRUE) %>% 
  tokens_compound(pattern = phrase(compound_tokens))

#Create DFM
dfm <-  dfm(toks) %>% 
  dfm_remove(pattern = c(custom_stops, stopwords(source = "smart"))) %>% 
  dfm_trim(min_termfreq = 50) %>%
  dfm_trim(max_docfreq = 0.90, docfreq_type = "prop")

#Filter out very short (overly influential) texts. Need to keep the vector separate to filter the tokens later
t2 <- ntoken(dfm) >= 5
dfm_f <-  dfm_subset(dfm, t2)

#Scaling Model
set.seed(123)
lss_PADs <- textmodel_lss(dfm_f, seeds = scores, 
                          #terms = context, 
                          k = 300, cache = TRUE)

## Visualize Terms
fig <- textplot_terms(lss_PADs,
                      keywords$Term
) +
  coord_cartesian(ylim=c(4,10), xlim=c(-.07,.11),clip="off") +
  annotate("text", x = -.05, y = 3.4, label = "Lenient", alpha=.9, size=3) +
  annotate("text", x = .1, y = 3.4, label = "Stringent", alpha=.9, size=3) +
  theme_minimal()

fig <- ggsave("output/terms_keywords.pdf", width=8, height=6)


measured <- lss_PADs[["beta"]] |> 
  data.frame() |> 
  rename("beta" =1) |> 
  arrange(desc((beta)))

extremes <- c(rownames(head(measured, 25)), rownames(tail(measured, 25))) #Identify most polarized words

fig <- textplot_terms(x = lss_PADs,
                      highlighted = extremes,

) +
  coord_cartesian(ylim=c(4,10), xlim=c(-.07,.11),clip="off") +
  annotate("text", x = -.05, y = 3.4, label = "Lenient", alpha=.9, size=3) +
  annotate("text", x = .1, y = 3.4, label = "Stringent", alpha=.9, size=3) +
  theme_minimal()
fig

fig <- ggsave("output/terms_extreme.pdf", width=8, height=6)

#### Fit Buntaine Target Texts ####
#### Measure Validation ####
#### Do conditions Match buntaine et al? ####
buntaine_in <- read_csv("data/Buntaine_Data_InSample.csv")
buntaine_out <- read_csv("data/Buntaine_Data_OutofSample.csv")

buntaine = plyr::rbind.fill(buntaine_in, buntaine_out) 
c2 <- corpus(buntaine,
                 text_field = "question_indicator") |> 
  tokens(remove_symbols = TRUE, remove_punct = FALSE, remove_numbers = TRUE,
         remove_separators = TRUE, remove_url = TRUE) %>% 
  tokens_compound(pattern = phrase(compound_tokens)) |> 
  dfm() %>% 
  dfm_remove(pattern = c(custom_stops, stopwords(source = "smart"))) %>% 
  dfm_trim(min_termfreq = 50) %>%
  dfm_trim(max_docfreq = 0.90, docfreq_type = "prop")

buntaine$stringency <- predict(lss_PADs, c2)

fig_fitted_b <- buntaine |> 
  mutate(far = case_when(far=="F" ~ "Form",
                         far=="A" ~ "Action",
                         far=="R" ~ "Result"),
         far = factor(far),
         far = relevel(far, "Form", "Action", "Result")) |> 
  ggplot()+
  geom_boxplot(aes(x=far, y=stringency, fill=far)) +
  theme_minimal() +
  labs(x="Requirement Type",
       y="Fitted Stringency") +
  theme(legend.position='none')

fig_fitted_b <- ggsave("output/fitted_buntaine.pdf", width=8, height=6)

sink()
