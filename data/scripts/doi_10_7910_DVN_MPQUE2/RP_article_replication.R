library(tidyverse) 
library(patchwork)
library(lubridate)
library(stopwords)
library(tidytext)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(quanteda.textmodels)
library(quanteda.sentiment)
library(lme4)
library(ggeffects)
set.seed(123)

# load data
load("drafts_2000_2020_v4_1.RData")

# demonstration with Haiti ####
ldocs_text_haiti <- ldocs_text |>
  select(undocsymbol, text_raw, text_ed) |>
  mutate(Haiti_text_raw = str_detect(text_raw, "Haiti"),
         Haiti_text_ed = str_detect(text_ed, "Haiti")) |>
  mutate(across(Haiti_text_raw:Haiti_text_ed, ~as.integer(.))) # to 1, 0

# merge with draft data (unweighted)
drafts_haiti <- country_draft_unw |>
  # retain ids, data, location, sponsorships
  select(1,4,13,30:225) |>
  # bring in kw_any
  left_join(ldocs_text_haiti[,c(1,4,5)],
            by=c("ldoc_or"="undocsymbol"))

# aggregate per year
drafts_haiti_ag_y <- drafts_haiti |>
  group_by(year(date_or)) |>
  summarise(Raw=sum(na.omit(Haiti_text_raw)),
            Edited=sum(na.omit(Haiti_text_ed))) |>
  pivot_longer(cols = Raw:Edited)
colnames(drafts_haiti_ag_y)[1] <- "year"
colnames(drafts_haiti_ag_y)[2] <- "Text"

## plot figure 2
ggplot(drafts_haiti_ag_y, aes(x=year, y=value, fill=Text))+
  geom_bar(stat = "identity", position = "dodge", width = 0.6)+
  scale_fill_manual(values=c("black","grey"))+
  labs(x="", y="Drafts mentioning 'Haiti'")+
  xlim(2000,2019)+
  theme_bw()+
  theme(legend.position = "bottom")


# application 1: salience #####
## dictionary keyword search and document count ####
ldocs_text_apd <- ldocs_text |>
  select(undocsymbol, text_ed) |>
  mutate(text_ed = str_to_lower(text_ed)) |>
  mutate(kw_dem = str_detect(text_ed, "democracy"), 
         kw_demz = str_detect(text_ed, "democratization"), 
         kw_elect = str_detect(text_ed, "election"),
         # any hits on the democracy dictionary
         kw_dem_any = ifelse(kw_dem==T|kw_demz==T|kw_elect==T,T,F),
         kw_sov = str_detect(text_ed, "sovereign"), 
         kw_nonintv = str_detect(text_ed, "non\\-intervention"),
         kw_nonintf = str_detect(text_ed, "non\\-interference"),
         # any hits on the sovereignty dictionary
         kw_sov_any = ifelse(kw_sov==T|kw_nonintf==T|kw_nonintv==T,T,F)) |>
  mutate(across(kw_dem:kw_sov_any, ~as.integer(.))) # to 1, 0
table(ldocs_text_apd$kw_dem_any) # 904
table(ldocs_text_apd$kw_sov_any) # 725


# merge with draft data (unweighted)
drafts <- country_draft_unw |>
  # retain ids, data, location, sponsorships
  select(1:18,30:225) |>
  # bring in kw_any
  left_join(ldocs_text_apd[,c(1,3:10)],
            by=c("ldoc_or"="undocsymbol"))
table(drafts$kw_dem_any) # 671
table(drafts$kw_sov_any) # 588

# aggregate per year
drafts_ag_s <- drafts |>
  group_by(year(date_or)) |>
  summarise(kw_dem_any_tot=sum(na.omit(kw_dem_any)),
            kw_sov_any_tot=sum(na.omit(kw_sov_any))
  ) |>
  pivot_longer(cols = 2:3,
               names_to = "keyword",
               values_to = "year_tot") |>
  mutate(keyword=ifelse(keyword=="kw_dem_any_tot","Democracy terms","Sovereignty terms"))
colnames(drafts_ag_s)[1] <- "year"
drafts_ag_s <- drafts_ag_s |>
  filter(year<2020)

## plot
p_salience <- ggplot(drafts_ag_s, aes(x=year, y=year_tot, fill=keyword))+
  geom_bar(stat = "identity", position = "dodge", width = 0.6)+
  labs(x="", y="N. Drafts w/ Keyword", fill="Dictionaries")+
  scale_fill_manual(values=c("black", "grey"))+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title = element_text(size=8),
        legend.text = element_text(size=7),
        legend.key.size = unit(5,"mm"))+
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5))



## combination of terms in documents ####
ldocs_text_apd2 <- ldocs_text |>
  select(undocsymbol, text_ed) |>
  mutate(text_ed = str_to_lower(text_ed)) |>
  mutate(kw_dem = str_detect(text_ed, "democracy"),  
         kw_elect = str_detect(text_ed, "elections"), 
         kw_nonintv = str_detect(text_ed, "non\\-intervention"),
         kw_peaceb = str_detect(text_ed, "peacebuilding"),
         comb_dem_elect = ifelse(
           kw_dem == T & kw_elect == T & kw_nonintv == F & kw_peaceb == F,
           T,F
         ),
         comb_dem_nonintv = ifelse(
           kw_dem == T & kw_elect == F & kw_nonintv == T & kw_peaceb == F,
           T,F
         ),
         comb_dem_peaceb = ifelse(
           kw_dem == T & kw_elect == F & kw_nonintv == F & kw_peaceb == T,
           T,F
         )) |> 
  mutate(across(kw_dem:comb_dem_peaceb, ~as.integer(.))) # to 1, 0

# merge with draft data (unweighted)
drafts2 <- country_draft_unw |>
  # retain ids, data, location, sponsorships
  select(1:18,30:225) |>
  # bring in kw_any
  left_join(ldocs_text_apd2[,c(1,3:9)],
            by=c("ldoc_or"="undocsymbol"))
table(drafts2$comb_dem_peace)

# aggregate per year
drafts_ag_s2 <- drafts2 |>
  group_by(year(date_or)) |>
  summarise(comb_dem_elect_tot=sum(na.omit(comb_dem_elect)),
            comb_dem_nonintv_tot=sum(na.omit(comb_dem_nonintv)),
            comb_dem_peaceb_tot=sum(na.omit(comb_dem_peaceb))) |>
  pivot_longer(cols = 2:4,
               names_to = "keyword",
               values_to = "year_tot") |>
  mutate(keyword=case_when(
    keyword=="comb_dem_elect_tot" ~ "'Democracy' +\n'Election'",
    keyword=="comb_dem_nonintv_tot" ~ "'Democracy' +\n'Non-intervention'",
    keyword=="comb_dem_peaceb_tot" ~ "'Democracy' +\n'Peacebuilding'"))
colnames(drafts_ag_s2)[1] <- "year"
drafts_ag_s2 <- drafts_ag_s2 |>
  filter(year<2020)

## plot
p_comb <- ggplot(drafts_ag_s2, aes(x=year, y=year_tot, fill=keyword))+
  geom_bar(stat = "identity", position = "stack", width = 0.9)+
  #scale_fill_manual(values=c("black", "grey"))+
  labs(x="", y="", fill="Keyword combinations")+
  scale_fill_manual(values=c("grey20", "grey50", "grey80"))+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title = element_text(size=8),
        legend.text = element_text(size=7),
        legend.key.size = unit(5,"mm"))+
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5))

## combine (figure 3)
p_salience + p_comb



# application 2: sentiment #####

# compare scores full text vs only verbs
# use committee as doc var
## join text with committee data
sent_df <- ldocs |>
  select(undocsymbol, un_bodies, date) |>
  left_join(ldocs_text[,c(1,3,4)],
            by="undocsymbol") |>
  mutate(year=year(date))

# full text corpus
colnames(sent_df)
sent_corpus_ft <- corpus(sent_df[,c(1,2,4,6)],
                         docid_field = "undocsymbol",
                         text_field = "text_ed")

## pre process
stpw <- stopwords(language = "en")
sent_tokens_ft <- sent_corpus_ft |>
  tokens() |> 
  tokens(remove_punct = T,
         remove_symbols = T, 
         remove_numbers = T,
         split_hyphens = T) |>  
  tokens_tolower() %>%
  tokens_remove(pattern = stpw, min_nchar=3)

## create dfm (document level)
sent_dfm_ft <- sent_tokens_ft |>
  dfm()

## apply dict
sent_result_ft <- sent_dfm_ft |>
  textstat_polarity(data_dictionary_LSD2015)

# verbs corpus
colnames(sent_df)
sent_corpus_vb <- corpus(sent_df[,c(1,2,5,6)],
                         docid_field = "undocsymbol",
                         text_field = "par_verbs")
## pre process
sent_tokens_vb <- sent_corpus_vb |>
  tokens() |> 
  tokens(remove_punct = T,
         remove_symbols = T, 
         remove_numbers = T,
         split_hyphens = T) |>  
  tokens_tolower() %>%
  tokens_remove(pattern = stpw, min_nchar=3)

## create dfm
sent_dfm_vb <- sent_tokens_vb |>
  dfm()

## apply dict
sent_result_vb <- sent_dfm_vb |>
  textstat_polarity(data_dictionary_LSD2015)

## compare doc level results
sent_result_ftvb <- bind_cols(sent_result_ft,
                              sent_result_vb)
sent_result_ftvb <- sent_result_ftvb |>
  select(1,2,4)
colnames(sent_result_ftvb) <- c("undocsymbol", "sent_full_text", "sent_par_verbs")

## bring in committee and year information
sent_result_ftvb <- sent_result_ftvb |>
  left_join(ldocs[,c(2,4,6)],
            by="undocsymbol") |>
  mutate(year=year(date),
         un_bodies_lab = case_when(
           un_bodies=="General Assembly; General Assembly Plenary" ~ "Plenary",
           un_bodies=="General Assembly; 1st Committee" ~ "C.1 First Committee\nDisarmament & Int. Security",
           un_bodies=="General Assembly; 2nd Committee" ~ "C.2 Second Committee\nEconomic & Financial",
           un_bodies=="General Assembly; 3rd Committee" ~ "C.3 Third Committee\nSocial, Humanit. & Cultural",
           un_bodies=="General Assembly; 4th Committee" ~ "C.4 Fourth Committee\nSpecial Polit. & Decol.",
           un_bodies=="General Assembly; 5th Committee" ~ "C.5 Fifth Committee\nAdmin. & Budgetary",
           un_bodies=="General Assembly; 6th Committee" ~ "C.6 Sixth Committee\nLegal"
         )) |>
  select(-date)


## as box plot
sent_result_ftvbl <- sent_result_ftvb |>
  pivot_longer(cols=sent_full_text:sent_par_verbs,
               values_to = "sent",
               names_to = "text_type") |>
  mutate(
    un_bodies_lab = case_when(
      un_bodies=="General Assembly; General Assembly Plenary" ~ "Plenary",
      un_bodies=="General Assembly; 1st Committee" ~ "C.1 First Committee\nDisarmament & Int. Security",
      un_bodies=="General Assembly; 2nd Committee" ~ "C.2 Second Committee\nEconomic & Financial",
      un_bodies=="General Assembly; 3rd Committee" ~ "C.3 Third Committee\nSocial, Humanit. & Cultural",
      un_bodies=="General Assembly; 4th Committee" ~ "C.4 Fourth Committee\nSpecial Polit. & Decol.",
      un_bodies=="General Assembly; 5th Committee" ~ "C.5 Fifth Committee\nAdmin. & Budgetary",
      un_bodies=="General Assembly; 6th Committee" ~ "C.6 Sixth Committee\nLegal"
    ),
    text_type=case_when(
      text_type=="sent_full_text" ~ "Full text",
      text_type=="sent_par_verbs" ~ "Verbs opening paragraphs" 
    ))

sent_result_ftvbl_p <- ggplot(sent_result_ftvbl, 
                              aes(y=sent,
                                  x=un_bodies_lab,
                                  fill=text_type))+
  geom_hline(yintercept = 0)+
  geom_boxplot()+
  scale_fill_manual(values=c("grey80", "gray40"))+
  coord_flip()+
  labs(x="Thematic committee", y="Sentiment score")+
  theme_bw() + 
  theme(legend.position = "bottom",
        legend.title = element_blank()) 

sent_result_ftvbl_p


# application 3: effects of edits #####

## global ####
# drafts that have Rev
revs <- country_draft_abs |>
  filter(str_detect(ldoc_edit1, "Rev")) |>
  select(1:14,29:225)
nrow(revs) # 1059 drafts had revisions
table(revs$draft_length) # shortest is 2 longest is 4 (but some have Adds as final ldoc)

# join with string length for each edit
## ids (undoc symbols)
revs_ids <- c(revs$ldoc_or,
              revs$ldoc_edit1,
              revs$ldoc_edit2,
              revs$ldoc_edit3)
revs_ids <- revs_ids[!is.na(revs_ids)]

## retain the texts from ldocs from selected ids
ldocs_text_revs <- ldocs_text |>
  filter(undocsymbol %in% revs_ids) |>
  filter(!is.na(text_ed))
nrow(ldocs_text_revs) # 2147 ldocs

# calculate string length
ldocs_text_revs$strl <- str_length(ldocs_text_revs$text_ed)

## join
revs <- revs |>
  left_join(ldocs_text_revs[,c(1,5)],
            by=c("ldoc_or"="undocsymbol")) |>
  left_join(ldocs_text_revs[,c(1,5)],
            by=c("ldoc_edit1"="undocsymbol")) |>
  left_join(ldocs_text_revs[,c(1,5)],
            by=c("ldoc_edit2"="undocsymbol")) |>
  left_join(ldocs_text_revs[,c(1,5)],
            by=c("ldoc_edit3"="undocsymbol")) |>
  select(1:15,212:215,16:211)

colnames(revs)[16] <- "strl_or"
colnames(revs)[17] <- "strl_edit1"
colnames(revs)[18] <- "strl_edit2"
colnames(revs)[19] <- "strl_edit3"

# calculate difs in text length
revs$strl_dif_01 <- revs$strl_edit1 - revs$strl_or
revs$strl_dif_12 <- revs$strl_edit2 - revs$strl_edit1

# calculate difs in n sponsors
revs <- revs |>
  rowwise() |>
  mutate(n_sponsors_dif_01 = n_sponsors_ed1 - n_sponsors_or,
         n_sponsors_dif_12 = ifelse(
           str_detect(ldoc_final, "Add"),
           n_sponsors_final - n_sponsors_ed1,
           n_sponsors_ed2 - n_sponsors_ed1
         ))

# df with differences and n sponsors, long format
revs_l <- revs |>
  select("id_draft",
         "ldoc_edit1", "ldoc_edit2",
         "strl_dif_01", "strl_dif_12",
         "n_sponsors_dif_01", "n_sponsors_dif_12")
revs_l <- data.frame(
  id_draft=rep(revs_l$id_draft,2),
  undocsymbol=c(revs_l$ldoc_edit1, revs_l$ldoc_edit2),
  strl_dif=c(revs_l$strl_dif_01, revs_l$strl_dif_12),
  n_sponsors_dif=c(revs_l$n_sponsors_dif_01, revs_l$n_sponsors_dif_12)
) |>
  drop_na()
revs_l$strl_dif_1k <- revs_l$strl_dif/1000

# plot the correlation between edit length and sponsor change
lm_revs_l <- lm(revs_l$n_sponsors_dif ~ revs_l$strl_dif_1k)
lmcoef <- round(coef(lm_revs_l),2)

ggplot(revs_l, aes(x = strl_dif_1k, y = n_sponsors_dif)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  geom_text(x = 35, y = 150,
            label = paste0("y = ", lmcoef[1], " + ", lmcoef[2], "x"), 
            hjust = 1)+
  theme_bw()+
  labs(x=expression(Delta * " string length (1k characters)"),
       y=expression(Delta * " number of sponsors"))




## specific words ####

### country sponsorship at each edit ####
# attribute l-doc text to l-doc id in draft
colnames(country_draft_abs)
revs_ch <- country_draft_abs |>
  select(1:14,30:225) |>
  # retain only drafts that had Rev and did not end in Add
  filter(str_detect(ldoc_edit1, "Rev") & str_detect(ldoc_final, "Add", negate=T)) |>
  # created max-min var on abs sponsorship values
  mutate(across(AFG:ZWE, ~replace(., .==-9, NA))) |> # recode non-member
  rowwise() |>
  mutate(abs_max=max(c_across(AFG:ZWE), na.rm=T),
         abs_min=min(c_across(AFG:ZWE), na.rm=T))

# see draft length and maximum possible scores
table(ABS_MAX=revs_ch$abs_max,
      DRA_LEN=revs_ch$draft_length)
table(ABS_MIN=revs_ch$abs_min,
      DRA_LEN=revs_ch$draft_length)
## a long format to help see what scores countries had for each combination
revs_ch_l <- revs_ch |>
  select(draft_length, AFG:ZWE, abs_max, abs_min) |>
  pivot_longer(cols=AFG:ZWE,
               names_to = "country",
               values_to = "value")
table(ABS_MAX=revs_ch_l$abs_max,
      CT_SC=revs_ch_l$value)
table(CT_SC=revs_ch_l$value,
      ABS_MAX=revs_ch_l$abs_max,
      DRA_LEN=revs_ch_l$draft_length)

# replace numeric scores by or, edit1, edit2
revs_ch2 <- revs_ch |> 
  select(id_draft,draft_length,abs_max,abs_min,AFG:ZWE) |>
  pivot_longer(cols=AFG:ZWE,
               names_to = "country",
               values_to = "score") |>
  drop_na() |> 
  mutate(joined = case_when(
    # if draft length is 2, country score is 1, is original
    draft_length==2 & score==1 ~ "or",
    # if draft length is 2, country score is 2, and abs_min = 2, was co-sponsor in the original and Rev did not alter list
    draft_length==2 & score==2 & abs_min==2 ~ "or",
    # if draft length is 2, country score is 2, and abs_max = 2, was co-sponsor in the original and Rev did not alter list
    draft_length==2 & score==2 & abs_max==2 ~ "or",
    # if draft length is 2, country score is greater than 2, and abs_max > 2, joined in rev1
    draft_length==2 & score>2 & abs_max>2 ~ "edit1",
    # if draft length is 2, country score is 2, but abs_max > 2, country must have been co-sponsor in original
    draft_length==2 & score==2 & abs_max>2 ~ "or",
    # if draft length is 3, country score is 1 or 2, is original
    draft_length==3 & score<3 ~ "or",
    # if draft length is 3, country score is 3, must be rev1
    draft_length==3 & score==3 ~ "edit1",
    # if draft length is 3, country score is 4 or higher, must be rev2
    draft_length==3 & score>3 ~ "edit2",
    # if country score is the abs_min, joined at original
    score==abs_min ~ "or"
  )
  )

## replace or, edit1, edit2 with the undocsymbols
revs_ch2 <- revs_ch2 |>
  left_join(revs_ch[,c(1,4,6,8)],
            by="id_draft") |>
  mutate(joined=case_when(
    joined=="or" ~ ldoc_or,
    joined=="edit1" ~ ldoc_edit1,
    joined=="edit2" ~ ldoc_edit2
  )) |>
  select(id_draft, country, joined)



### texts ####

# filter only third committee res
ldocs_text_revs_c3 <- ldocs_text_revs |>
  filter(str_detect(undocsymbol, fixed("C.3"))) 
nrow(ldocs_text_revs_c3) # 1038
# preprocess
ldocs_text_revs_c3_corpus <- ldocs_text_revs_c3 |>
  mutate(text_ed = str_remove_all(text_ed, "[[:punct:]]")) |>
  mutate(text_ed = str_remove_all(text_ed, "\\d")) |>
  mutate(text_ed = str_replace_all(text_ed, "\\W", " ")) |>
  select(text_ed)
ldocs_text_revs_c3_corpus <- corpus(ldocs_text_revs_c3_corpus$text_ed)
ldocs_text_revs_c3_toks <- ldocs_text_revs_c3_corpus |>
  tokens() |> 
  tokens(remove_punct = T,
         remove_symbols = T, 
         remove_numbers = T,
         split_hyphens = T) |>  
  tokens_tolower() |>
  tokens_remove(pattern = stpw, min_nchar=3) |>
  tokens_wordstem(language = quanteda_options("language_stemmer"))
# two word collocations
ldocs_text_revs_c3_toks_col2 <- ldocs_text_revs_c3_toks |> 
  textstat_collocations(min_count = 1000, 
                        size=2)
# three to ten word collocations
ldocs_text_revs_c3_toks_col310 <- ldocs_text_revs_c3_toks |> 
  textstat_collocations(min_count = 200, 
                        size=3:10)
# 3+ word to retain
colextra <- c("human right council", "econom social council", "unit nation offic drug crime", "commission human right", "intern coven civil polit", "intern coven econom social cultur right", "elimin form racial discrimin", "univers declar human right")
ldocs_text_revs_c3_toks <- tokens_compound(ldocs_text_revs_c3_toks,
                                           phrase(ldocs_text_revs_c3_toks_col2),
                                           join=F, window = 0)
ldocs_text_revs_c3_toks <- tokens_compound(ldocs_text_revs_c3_toks,
                                           phrase(colextra),
                                           join=F, window = 0)
# count rare tokens
low_freq_c3_toks <- textstat_frequency(dfm(ldocs_text_revs_c3_toks))
low_freq_c3_toks <- low_freq_c3_toks$feature[low_freq_c3_toks$frequency<10]
# remove
ldocs_text_revs_c3_toks <- tokens_remove(ldocs_text_revs_c3_toks,
                                         pattern = low_freq_c3_toks)
toks_list_c3 <- as.list(ldocs_text_revs_c3_toks)
ldocs_text_revs_c3$toks <- toks_list_c3

## get a draft-level id to join with ldocs
## bring id
## get a draft-level id to join with ldocs
drafts_ids <- country_draft_abs |>
  select(1,4,6,8,10) |>
  pivot_longer(cols=2:5) |>
  drop_na()

ldocs_text_revs_c3 <- ldocs_text_revs_c3 |>
  left_join(drafts_ids[,c(1,3)],
            by=c("undocsymbol"="value"))

## compare differences in versions within a draft id
compares_c3 <- ldocs_text_revs_c3 |>
  select(id_draft, undocsymbol, toks) |>
  # bring in position in draft
  left_join(ldocs[,c(2,16)],
            by="undocsymbol") |>
  select(id_draft, posit_in_draft, undocsymbol, toks) |>
  # order by id_draft and within it by position in draft
  arrange(id_draft, posit_in_draft) |>
  # get shared and exclusive terms, ed vs. previous
  mutate(shared_prev=ifelse(posit_in_draft!=0,
                            map2(toks, lag(toks), intersect),
                            NA),
         diff_prev=ifelse(posit_in_draft!=0,
                          map2(toks, lag(toks), setdiff),
                          NA),
         diff_next=ifelse(lead(posit_in_draft)!=0,
                          map2(toks, lead(toks), setdiff),
                          NA))

### place word differences as words 'deleted' or 'added'
compares_c3_cosp <- compares_c3 |>
  mutate(words_deleted=lag(diff_next), # the words on the previous document that are not in the edit
         words_added=diff_prev) |>
  # keep only revs, discard root ldocs
  filter(str_detect(undocsymbol, "Rev"))

## merge list of words with undocsymbol for each country
compares_c3_cosp_ch <- revs_ch2 |>
  filter(str_detect(joined, "Rev") & str_detect(joined, fixed("C.3"))) |>
  left_join(compares_c3_cosp[,c(3,8,9)],
            by=c("joined"="undocsymbol"))


## combine list of words per country
### words deleted
compares_c3_cosp_ch_wd <- compares_c3_cosp_ch |>
  select(country, words_deleted) |>
  unnest(words_deleted) |>
  drop_na() |>
  group_by(country, words_deleted) |>
  tally()

### words added
compares_c3_cosp_ch_wa <- compares_c3_cosp_ch |>
  select(country, words_added) |>
  unnest(words_added) |>
  drop_na() |>
  group_by(country, words_added) |>
  tally()

## tf idf
### words deleted
compares_c3_cosp_ch_wd_tfidf <- compares_c3_cosp_ch_wd |>
  bind_tf_idf(words_deleted, country, n)
### added
compares_c3_cosp_ch_wa_tfidf <- compares_c3_cosp_ch_wa |>
  bind_tf_idf(words_added, country, n)


### plots ####
## countries of interest
countrysample <- c("USA","RUS","COL","ETH","RWA","DNK") # countries to sample for plots
wordsremoveplot <- c("tion", "ing")

## plot words deleted, freq: (not used in the paper)
compares_c3_cosp_ch_wd |>
  filter(country %in% countrysample) |>
  filter(!(words_deleted %in% wordsremoveplot)) |>
  mutate(country=case_match(
    country,
    "USA" ~ "United States",
    "RUS" ~ "Russia",
    "COL" ~ "Colombia",
    "ETH" ~ "Ethiopia",
    "RWA" ~ "Rwanda",
    "DNK" ~ "Denmark"
  )) |>
  group_by(country) |>
  slice_max(n, n = 5, with_ties = F) |>
  ungroup() %>%
  ggplot(aes(n, fct_reorder(words_deleted, n))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~country, scales = "free", labeller = ) +
  labs(x = "Frequency", y = NULL)

## plot, added, tfidf: figure 6
compares_c3_cosp_ch_wa_tfidf |>
  filter(country %in% countrysample) |>
  mutate(country=case_match(
    country,
    "USA" ~ "United States",
    "RUS" ~ "Russia",
    "COL" ~ "Colombia",
    "ETH" ~ "Ethiopia",
    "RWA" ~ "Rwanda",
    "DNK" ~ "Denmark"
  )) |>
  group_by(country) |>
  slice_max(tf_idf, n = 10, with_ties = T) |>
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(words_added, tf_idf))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~country, scales = "free") +
  labs(x = "TF-IDF", y = NULL) +
  scale_x_continuous(labels = scales::number_format(scale=10))

# data for regime type at 2015
# from https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/FENWWR
dem <- read.csv("country_regime_2015_2020.csv") |>
  filter(year==2015)

# words deleted freq
dem_wd_freq <- compares_c3_cosp_ch_wd |>
  inner_join(dem[,c(2,3)],
             by=c("country"="ccodealp")) |>
  group_by(bmr_dem, words_deleted) |>
  summarise(sum_n=sum(n))

## as dfm for wordfish
dem_wd_freq_dfm <-  cast_dfm(data=dem_wd_freq,
                             document = bmr_dem,
                             term = words_deleted,
                             value = sum_n)

## wf model
dem_wd_freq_dfm_wf <- textmodel_wordfish(dem_wd_freq_dfm, dir=c(1,2))
dem_wd_freq_dfm_wf_df <- data.frame(feat=dem_wd_freq_dfm_wf$features,
                                    beta=dem_wd_freq_dfm_wf$beta,
                                    psi=dem_wd_freq_dfm_wf$psi) |>
  arrange(beta)

## plot
wd_wf_highlight <- c("christian", "islamophobia", "religion",
                     "arab", "genocid","repres","raporteur",
                     "neonazi", "wealth", "disarma", "nuclear",
                     "session", "aggress","extradit",
                     "secretari", "take_note", "welcom")

p_dem_wd_freq_dfm_wf <- textplot_scale1d(dem_wd_freq_dfm_wf,
                                         margin = "features",
                                         highlighted = wd_wf_highlight)+
  ggtitle("Words deleted")+labs(y="Estimated psi", x="Estimated beta\n(lower values: authocracies, higher values: democracies)")


# words added tfidf freq
dem_wa_freq <- compares_c3_cosp_ch_wa |>
  inner_join(dem[,c(2,3)],
             by=c("country"="ccodealp")) |>
  group_by(bmr_dem, words_added) |>
  summarise(sum_n=sum(n))

## as dfm for wordfish
dem_wa_freq_dfm <-  cast_dfm(data=dem_wa_freq,
                             document = bmr_dem,
                             term = words_added,
                             value = sum_n)

## wf model
dem_wa_freq_dfm_wf <- textmodel_wordfish(dem_wa_freq_dfm, dir=c(1,2))
dem_wa_freq_dfm_wf_df <- data.frame(feat=dem_wa_freq_dfm_wf$features,
                                    beta=dem_wa_freq_dfm_wf$beta,
                                    psi=dem_wa_freq_dfm_wf$psi) |>
  arrange(beta)

## plot
wa_wf_highlight <- c("murder","weapon", "bomb","vote",
                     "intern_coven_civil_polit","nongovernment",
                     "jewish", "religion_belief",
                     "socioeconom", "apartheid","antidiscrimin",
                     "disarma","take_note",
                     "women", "gender",
                     "peacekeep")

p_dem_wa_freq_dfm_wf <- textplot_scale1d(dem_wa_freq_dfm_wf,
                                         margin = "features",
                                         highlighted = wa_wf_highlight)+
  ggtitle("Words added")+labs(y="Estimated psi", x="Estimated beta\n(lower values: authocracies, higher values: democracies)")

## plot figure 7
p_dem_wd_freq_dfm_wf + p_dem_wa_freq_dfm_wf


### marginal effects model ####
# all country-draft combinations
grid <- expand.grid(country=unique(compares_c3_cosp_ch$country),
                    ldoc=unique(compares_c3_cosp_ch$joined))

# detect words of interest from wordfish in the list of added or deleted
grid_word <- compares_c3_cosp_ch |>
  select(joined, words_deleted, words_added) |>
  distinct() |>
  mutate(del_iccpr = str_detect(words_deleted, "intern_coven_civil_polit"),
         add_iccpr = str_detect(words_added, "intern_coven_civil_polit")) 
colnames(grid_word)[1] <- "ldoc"

# joins
grid_joins <- compares_c3_cosp_ch |>
  select(country, joined)
colnames(grid_joins)[2] <- "ldoc"
grid_joins$joined <- 1

# merge
grid_all <- grid |>
  left_join(grid_joins,
            by=c("country", "ldoc")) |>
  mutate(joined=ifelse(is.na(joined),0,joined)) |>
  left_join(grid_word[,c(1,4,5)],
            by="ldoc") |>
  left_join(dem[,2:3],
            by=c("country"="ccodealp")) |>
  drop_na() |>
  mutate(across(del_iccpr:add_iccpr, ~as.integer(.)))


# model
m1 <- glmer(
  joined ~ add_iccpr*bmr_dem + del_iccpr*bmr_dem + (1 | ldoc),
  data = grid_all,
  family = binomial(link = "logit")
)
m1
m1_pred_add <- predict_response(m1, terms=c("add_iccpr", "bmr_dem"))
p_add <- ggplot(m1_pred_add, aes(x, predicted, color=group))+
  geom_point(position = position_dodge(0.3)) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(0.3)
  )+
  ylim(0,0.8)+
  scale_color_manual(values=c("grey60", "black"))+
  scale_x_continuous(breaks = c(0,1), 
                     labels = c("ICCPR not added", "ICCPR added"))+
  labs(y="Prob. of sponsoring",
       x="",
       color="Democracy")+
  theme_bw()
m1_pred_del <- predict_response(m1, terms=c("del_iccpr", "bmr_dem"))
p_del <- ggplot(m1_pred_del, aes(x, predicted, color=group))+
  geom_point(position = position_dodge(0.3)) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(0.3)
  )+
  ylim(0,0.8)+  
  scale_color_manual(values=c("grey60", "black"))+
  scale_x_continuous(breaks = c(0,1), labels = c("ICCPR not deleted", "ICCPR deleted"))+
  labs(y="",
       x="",
       color="Democracy")+
  theme_bw()

## combine plots (figure 7)
p_add + p_del + plot_layout(guides = "collect")
