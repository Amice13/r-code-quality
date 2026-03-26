##call libraries
library(tidyverse)
library(tidytext)
library(readtext)
library(quanteda)
library(quanteda.textstats)


#FORMAT AND SAMPLE CORPUS 3
dat <- select(corpus1.m1, doc_id, text, ave_sentiment, combined_avg, total_hits, review_stars, review_id, )

dat$text <- dat$text %>% 
  trimws(which = "both")

quantile(dat$combined_avg)

dat$auth_extreme <- if_else(dat$combined_avg > 0.13678, "HAR", if_else(dat$combined_avg < 0.05639, "LAR", "middle"))

dat <- na.omit(dat)

##REFORMAT tibble for SKETCH ENGINE markup
formatted <- tibble(doc_id = dat$doc_id, text = str_c('<doc doc_id="', dat$doc_id, '" ', 
                                                      'review_stars="', dat$review_stars,'" ', 
                                                      'auth_count="', dat$total_hits,'" ',
                                                      'auth_score="', dat$combined_avg,'" ',
                                                      'sent_score="', dat$ave_sentiment,'" ',
                                                      'auth_extreme="', dat$auth_extreme, '"> ',
                                                      dat$text, "</doc>"))  

formatted <- formatted %>% 
  slice_sample(prop = 0.5)

###export as individual files
apply(formatted, 1, function(x) write.table(data.frame(x[2]), 
                                            file = str_c("conference prep/rest_sample_ske/", x[1], ".txt"),
                                            na = "NA",
                                            row.names = F, 
                                            col.names = F,
                                            quote = F))




#KEYWORD ANALYSIS
sub_corp <- dat

spacy_initialize()
sub_corp.dep <- spacy_parse(sub_corp$text, pos = T, tag = T, lemma = T, entity = F, multithread = T)
spacy_finalize()

subcorp_toks <- as.tokens(sub_corp.dep, concatenator = "/", include_pos = "tag", use_lemma = F)
toks <- tokens(subcorp_toks, remove_punct = T)

sum(ntoken(toks))

###Add Docvars
dv <- select(sub_corp, -doc_id, -element_id)
dv <- dv %>% 
  rownames_to_column(var = "doc_id")
dv$doc_id <- as.numeric(dv$doc_id)
dv$auth_extreme <- if_else(dv$combined_avg > 0.13678, "HAR", if_else(dv$combined_avg < 0.05639, "LAR", "NA"))
docvars(subcorp_dfm, field = "authent_extreme") <- dv$auth_extreme


###Compute Keywords
subset <- dfm_subset(subcorp_dfm, authent_extreme != "NA")

keywords <- textstat_keyness(subset, target = docvars(subset, "authent_extreme") == "LAR", measure = "lr")

keywords <- keywords %>% 
  mutate(nf_target = n_target*10^6/sum(ntoken(ah_dfm))) %>% 
  mutate(nf_reference = n_reference*10^6/sum(ntoken(sh_dfm))) %>% 
  mutate(perc_diff = (nf_target-if_else(n_reference == 0, 
                                        nf_reference+1*10^-18, #correction
                                        n_reference))*100/if_else(n_reference == 0, 
                                                                  nf_reference+1*10^-18, 
                                                                  n_reference)) %>% 
  mutate(diff_coef = ((nf_target-nf_reference)/(nf_target+nf_reference))) #added because values are 1, -1

#arrange tibble by effect size
keywords.tbl <- as_tibble(keywords) %>% 
  relocate(feature, perc_diff, diff_coef) %>% 
  arrange(desc(perc_diff))

filtered_keywords <- keywords.tbl %>% 
  filter(n_target >= 5) %>% 
  filter(G2 >= 18.81)