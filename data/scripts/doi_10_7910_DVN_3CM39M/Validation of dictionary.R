# Load packages
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, readxl, writexl, irr, ggplot2, cowplot)

# disable scientific notation
options(scipen=999)

################### Draw samples for human coding #################
#SOPA
## Draw random samples to create dictionary
SOPA_news = read.csv("SOPAPIPA_news_para_frames.csv")
SOPA_tweets = read.csv("tweets_frames.csv")

set.seed(1)

SOPA_news_dict = sample_n(SOPA_news, 100)
SOPA_tweets_dict = sample_n(SOPA_tweets, 100)

SOPA_news_dict = select(SOPA_news_dict, c(datetime, article_id, paragraph_id, paragraph))
SOPA_tweets_dict = select(SOPA_tweets_dict, c(tweet_id, author_id, datetime, text))

write_xlsx(SOPA_news_dict, "SOPA_Newspapers_DictionaryConstruction.xlsx")
write_xlsx(SOPA_tweets_dict, "SOPA_Tweets_DictionaryConstruction.xlsx")

## Draw random sample for human coding
SOPA_news_samp = sample_n(SOPA_news, 500)
SOPA_tweets_samp = sample_n(SOPA_tweets, 500)

SOPA_news_samp = select(SOPA_news_samp, c(datetime, article_id, paragraph_id, paragraph))
SOPA_tweets_samp = select(SOPA_tweets_samp, c(tweet_id, author_id, datetime, text))

write_xlsx(SOPA_news_samp, "SOPA_Newspapers_HumanCodingSample.xlsx")
write_xlsx(SOPA_tweets_samp, "SOPA_Tweets_HumanCodingSample.xlsx")

# EUCD
set.seed(123)

## newspapers
### english
EU_news = read.csv("EU_newspaper_frames.csv")

EU_news = subset(EU_news, language == "Englisch")

EU_news = sample_n(EU_news, size = 500)
EU_news_dict = sample_n(EU_news, size = 100)

EU_news = select(EU_news, c(datetime, article_id, doc_id, paper, paragraph))
EU_news_dict = select(EU_news_dict, c(datetime, article_id, doc_id, paper, paragraph))

write_xlsx(EU_news_dict, "EU_English_Newspapers_DictionaryCreation.xlsx")
write_xlsx(EU_news, "EU_English_Newspapers_HumanCodingSample.xlsx")

### deutsch
EU_news = read.csv("EU_newspaper_frames.csv")

EU_news = subset(EU_news, language == "Deutsch")

EU_news = sample_n(EU_news, size = 500)

EU_news = select(EU_news, c(datetime, article_id, doc_id, paper, paragraph))

write_xlsx(EU_news, "EU_German_Newspapers_HumanCodingSample.xlsx")

## tweets
### english
EU_tweets = read.csv("tweets_frames.csv")

EU_tweets = subset(EU_tweets, language == "en")

EU_tweets = sample_n(EU_tweets, size = 500)
EU_tweets_dict = sample_n(EU_tweets, size = 100)

EU_tweets = select(EU_tweets, c(datetime, tweet_id, doc_id, id, text))
EU_tweets_dict = select(EU_tweets_dict, c(datetime, tweet_id, doc_id, id, text))

write_xlsx(EU_tweets_dict, "EU_English_Tweets_DictionaryCreation.xlsx")
write_xlsx(EU_tweets, "EU_English_Tweets_HumanCodingSample.xlsx")

### deutsch
EU_tweets = read.csv("tweets_frames.csv")

EU_tweets = subset(EU_tweets, language == "de")

EU_tweets = sample_n(EU_tweets, size = 500)

EU_tweets = select(EU_tweets, c(datetime, tweet_id, doc_id, id, text))

write_xlsx(EU_tweets, "EU_German_Tweets_HumanCodingSample.xlsx")

############################ Validation of dictionary in comparison with human coded data ###############################
###### Define a bunch of functions ######
# Aggregate frames
group_frames = function(dta){
  
  # human coded data
  dta$ag_censor_filter_hc = ifelse(dta$censor_hc > 0 | dta$overly_broad_hc > 0 | dta$uploadfilter_hc > 0, 1, 0)
  dta$ag_break_internet_hc = dta$break_internet_hc
  dta$ag_fair_creators_hc = ifelse(dta$fair_pay_hc > 0 | dta$culture_creator_hc > 0 | dta$lovemusic_hc > 0, 1, 0)
  dta$ag_criminal_safety_hc = ifelse(dta$criminal_hc > 0 | dta$public_safety_hc > 0, 1, 0)
  dta$ag_astro_lobby_hc = ifelse(dta$lobby_hc > 0 | dta$astroturf_hc > 0, 1, 0)
  dta$ag_platform_power_hc = ifelse(dta$bad_platforms_hc > 0 | dta$market_power_hc > 0, 1, 0)
  dta$ag_econ_inno_hc = ifelse(dta$economy_hc > 0 | dta$inno_hc > 0, 1, 0)
  dta$ag_user_rights_hc = ifelse(dta$consumers_users_hc > 0 | dta$privacy_hc > 0, 1, 0)
  dta$ag_misinfo_hc = dta$misinfo_hc
  dta$ag_diversity_hc = dta$diversity_freepress_hc
  dta$ag_reach_hc = dta$reach_hc
  
  # dictionary coded data
  dta$ag_censor_filter = ifelse(dta$censor > 0 | dta$overly_broad > 0 | dta$uploadfilter > 0, 1, 0)
  dta$ag_break_internet = dta$break_internet
  dta$ag_fair_creators = ifelse(dta$fair_pay > 0 | dta$culture_creator > 0, 1, 0)
  dta$ag_criminal_safety = ifelse(dta$criminal > 0 | dta$public_safety > 0, 1, 0)
  dta$ag_astro_lobby = ifelse(dta$lobby > 0 | dta$astroturf > 0, 1, 0)
  dta$ag_platform_power = ifelse(dta$bad_platforms > 0 | dta$market_power > 0, 1, 0)
  dta$ag_econ_inno = ifelse(dta$economy > 0 | dta$inno > 0, 1, 0)
  dta$ag_user_rights = ifelse(dta$consumers_users > 0 | dta$privacy > 0, 1, 0)
  dta$ag_misinfo = dta$misinfo
  dta$ag_diversity = dta$diversity_freepress
  dta$ag_reach = dta$reach
  
  return(dta)
}

# Kappa - Intercoder Reliability
kappa_fleiss = function(dta, dc_var, hc_var) {
  
  vars = c(dc_var, hc_var)
  
  kappa_dta = select(dta, c(dc_var, hc_var))
  
  kappa_dta = as.matrix(kappa_dta)
  
  name = dc_var
  value = kappam.fleiss(kappa_dta, exact = T)[["value"]]
  
  result = as.data.frame(cbind(name, value))
  
  names(result)[2] = "kappa"
  
  return(result)
}

kappa_plot = function(dta){
  
  ag_censor_filter_dta = kappa_fleiss(dta = dta, dc_var = "ag_censor_filter", hc_var = "ag_censor_filter_hc")
  ag_break_internet_dta = kappa_fleiss(dta = dta, dc_var = "ag_break_internet", hc_var = "ag_break_internet_hc")
  ag_fair_creators_dta = kappa_fleiss(dta = dta, dc_var = "ag_fair_creators", hc_var = "ag_fair_creators_hc")
  ag_criminal_safety_dta = kappa_fleiss(dta = dta, dc_var = "ag_criminal_safety", hc_var = "ag_criminal_safety_hc")
  ag_astro_lobby_dta = kappa_fleiss(dta = dta, dc_var = "ag_astro_lobby", hc_var = "ag_astro_lobby_hc")
  ag_platform_power_dta = kappa_fleiss(dta = dta, dc_var = "ag_platform_power", hc_var = "ag_platform_power_hc")
  ag_econ_inno_dta = kappa_fleiss(dta = dta, dc_var = "ag_econ_inno", hc_var = "ag_econ_inno_hc")
  ag_user_rights_dta = kappa_fleiss(dta = dta, dc_var = "ag_user_rights", hc_var = "ag_user_rights_hc")
  ag_misinfo_dta = kappa_fleiss(dta = dta, dc_var = "ag_misinfo", hc_var = "ag_misinfo_hc")
  ag_diversity_dta = kappa_fleiss(dta = dta, dc_var = "ag_diversity", hc_var = "ag_diversity_hc")
  ag_reach_dta = kappa_fleiss(dta = dta, dc_var = "ag_reach", hc_var = "ag_reach_hc")
  
  kappa_dta_gr = rbind(ag_censor_filter_dta, ag_break_internet_dta, ag_fair_creators_dta, ag_criminal_safety_dta, ag_astro_lobby_dta, 
                       ag_platform_power_dta, ag_econ_inno_dta, ag_user_rights_dta, ag_misinfo_dta, ag_diversity_dta, ag_reach_dta)
  
  kappa_dta_gr$kappa = as.numeric(kappa_dta_gr$kappa)
  
  
  kappa_plot = kappa_dta_gr %>%
    mutate(name = fct_reorder(name, kappa)) %>%
    ggplot(aes(x=name, y=kappa)) +
    geom_bar(stat="identity") +
    scale_y_continuous(limits = c(0.0, 1.0), breaks = seq(0.0, 1.0, by = 0.2)) + 
    scale_x_discrete(labels = c(ag_censor_filter = "Censorship & Uploadfilter",
                                ag_break_internet = "Breaking the Internet",
                                ag_fair_creators = "Fair Pay & Creators",
                                ag_criminal_safety = "Criminality & Public Safety",
                                ag_astro_lobby = "Astroturfing & Lobbying",
                                ag_platform_power = "Bad Platforms & Market Power",
                                ag_econ_inno = "Economy & Innovation",
                                ag_user_rights = "User Rights & Cybersecurity",
                                ag_misinfo = "Misinformation",
                                ag_diversity = "Diversity & Free Press",
                                ag_reach = "Reach"
    )) +
    coord_flip() + 
    xlab("") +
    ggtitle("Fleiss' Kappa") +
    theme_bw()
  
  return(kappa_plot)
}

# Precision, Recall, and f-score
prec_rec = function(dta, hc_var, dc_var) {
  
  true_neg = nrow(subset(dta, dta[[hc_var]] == 0 & dta[[dc_var]] == 0))
  true_pos = nrow(subset(dta, dta[[hc_var]] == 1 & dta[[dc_var]] == 1))
  false_neg = nrow(subset(dta, dta[[hc_var]] == 0 & dta[[dc_var]] == 1))
  false_pos = nrow(subset(dta, dta[[hc_var]] == 1 & dta[[dc_var]] == 0))
  
  precision = true_pos/(true_pos+false_pos)
  recall = true_pos/(true_pos+false_neg)
  f = 2*((precision*recall)/(precision+recall))
  name = dc_var
  
  prec_rec = as.data.frame(cbind(name, precision, recall, f))
  
  return(prec_rec)
}

prec_rec_plot = function(dta){
  
  ag_censor_filter_dta = prec_rec(dta = dta, dc_var = "ag_censor_filter", hc_var = "ag_censor_filter_hc")
  ag_break_internet_dta = prec_rec(dta = dta, dc_var = "ag_break_internet", hc_var = "ag_break_internet_hc")
  ag_fair_creators_dta = prec_rec(dta = dta, dc_var = "ag_fair_creators", hc_var = "ag_fair_creators_hc")
  ag_criminal_safety_dta = prec_rec(dta = dta, dc_var = "ag_criminal_safety", hc_var = "ag_criminal_safety_hc")
  ag_astro_lobby_dta = prec_rec(dta = dta, dc_var = "ag_astro_lobby", hc_var = "ag_astro_lobby_hc")
  ag_platform_power_dta = prec_rec(dta = dta, dc_var = "ag_platform_power", hc_var = "ag_platform_power_hc")
  ag_econ_inno_dta = prec_rec(dta = dta, dc_var = "ag_econ_inno", hc_var = "ag_econ_inno_hc")
  ag_user_rights_dta = prec_rec(dta = dta, dc_var = "ag_user_rights", hc_var = "ag_user_rights_hc")
  ag_misinfo_dta = prec_rec(dta = dta, dc_var = "ag_misinfo", hc_var = "ag_misinfo_hc")
  ag_diversity_dta = prec_rec(dta = dta, dc_var = "ag_diversity", hc_var = "ag_diversity_hc")
  ag_reach_dta = prec_rec(dta = dta, dc_var = "ag_reach", hc_var = "ag_reach_hc")
  
  prec_rec_dta_gr = rbind(ag_censor_filter_dta, ag_break_internet_dta, ag_fair_creators_dta, ag_criminal_safety_dta, ag_astro_lobby_dta, 
                          ag_platform_power_dta, ag_econ_inno_dta, ag_user_rights_dta, ag_misinfo_dta, ag_diversity_dta, ag_reach_dta)
  
  prec_rec_dta_gr$precision = as.numeric(prec_rec_dta_gr$precision)
  prec_rec_dta_gr$recall = as.numeric(prec_rec_dta_gr$recall)
  prec_rec_dta_gr$f = as.numeric(prec_rec_dta_gr$f)
  
  # Precision Plot
  prec_plot = prec_rec_dta_gr %>%
    mutate(name = fct_reorder(name, precision)) %>%
    ggplot(aes(x=name, y=precision)) +
    geom_bar(stat="identity") +
    scale_y_continuous(limits = c(0.0, 1.0), breaks = seq(0.0, 1.0, by = 0.2)) + 
    scale_x_discrete(labels = c(ag_censor_filter = "Censorship & Uploadfilter",
                                ag_break_internet = "Breaking the Internet",
                                ag_fair_creators = "Fair Pay & Creators",
                                ag_criminal_safety = "Criminality & Public Safety",
                                ag_astro_lobby = "Astroturfing & Lobbying",
                                ag_platform_power = "Bad Platforms & Market Power",
                                ag_econ_inno = "Economy & Innovation",
                                ag_user_rights = "User Rights & Cybersecurity",
                                ag_misinfo = "Misinformation",
                                ag_diversity = "Diversity & Free Press",
                                ag_reach = "Reach"
    )) +
    coord_flip() + 
    xlab("") +
    ggtitle("Precision") +
    theme_bw()
  
  # Recall Plot
  rec_plot = prec_rec_dta_gr %>%
    mutate(name = fct_reorder(name, recall)) %>%
    ggplot(aes(x=name, y=recall)) +
    geom_bar(stat="identity") +
    scale_y_continuous(limits = c(0.0, 1.0), breaks = seq(0.0, 1.0, by = 0.2)) + 
    scale_x_discrete(labels = c(ag_censor_filter = "Censorship & Uploadfilter",
                                ag_break_internet = "Breaking the Internet",
                                ag_fair_creators = "Fair Pay & Creators",
                                ag_criminal_safety = "Criminality & Public Safety",
                                ag_astro_lobby = "Astroturfing & Lobbying",
                                ag_platform_power = "Bad Platforms & Market Power",
                                ag_econ_inno = "Economy & Innovation",
                                ag_user_rights = "User Rights & Cybersecurity",
                                ag_misinfo = "Misinformation",
                                ag_diversity = "Diversity & Free Press",
                                ag_reach = "Reach"
    )) +
    coord_flip() + 
    xlab("") +
    ggtitle("Recall") +
    theme_bw()
  
  # Precision Plot
  f_plot = prec_rec_dta_gr %>%
    mutate(name = fct_reorder(name, f)) %>%
    ggplot(aes(x=name, y=f)) +
    geom_bar(stat="identity") +
    scale_y_continuous(limits = c(0.0, 1.0), breaks = seq(0.0, 1.0, by = 0.2)) + 
    scale_x_discrete(labels = c(ag_censor_filter = "Censorship & Uploadfilter",
                                ag_break_internet = "Breaking the Internet",
                                ag_fair_creators = "Fair Pay & Creators",
                                ag_criminal_safety = "Criminality & Public Safety",
                                ag_astro_lobby = "Astroturfing & Lobbying",
                                ag_platform_power = "Bad Platforms & Market Power",
                                ag_econ_inno = "Economy & Innovation",
                                ag_user_rights = "User Rights & Cybersecurity",
                                ag_misinfo = "Misinformation",
                                ag_diversity = "Diversity & Free Press",
                                ag_reach = "Reach"
    )) +
    coord_flip() +
    ggtitle("F-Score") + 
    xlab("") +
    theme_bw()
  
  plot_out = plot_grid(prec_plot, rec_plot, f_plot, ncol = 3)
  
  return(plot_out)
}

correlate = function(dta, hc_var, dc_var) {
  
  correlation = cor(dta[[hc_var]], dta[[dc_var]], use = "pairwise.complete.obs")
  name = dc_var
  
  cor = as.data.frame(cbind(name, correlation))
  
  return(cor)
}

cor_plot = function(dta){
  
  ag_censor_filter_dta = correlate(dta = dta, dc_var = "ag_censor_filter", hc_var = "ag_censor_filter_hc")
  ag_break_internet_dta = correlate(dta = dta, dc_var = "ag_break_internet", hc_var = "ag_break_internet_hc")
  ag_fair_creators_dta = correlate(dta = dta, dc_var = "ag_fair_creators", hc_var = "ag_fair_creators_hc")
  ag_criminal_safety_dta = correlate(dta = dta, dc_var = "ag_criminal_safety", hc_var = "ag_criminal_safety_hc")
  ag_astro_lobby_dta = correlate(dta = dta, dc_var = "ag_astro_lobby", hc_var = "ag_astro_lobby_hc")
  ag_platform_power_dta = correlate(dta = dta, dc_var = "ag_platform_power", hc_var = "ag_platform_power_hc")
  ag_econ_inno_dta = correlate(dta = dta, dc_var = "ag_econ_inno", hc_var = "ag_econ_inno_hc")
  ag_user_rights_dta = correlate(dta = dta, dc_var = "ag_user_rights", hc_var = "ag_user_rights_hc")
  ag_misinfo_dta = correlate(dta = dta, dc_var = "ag_misinfo", hc_var = "ag_misinfo_hc")
  ag_diversity_dta = correlate(dta = dta, dc_var = "ag_diversity", hc_var = "ag_diversity_hc")
  ag_reach_dta = correlate(dta = dta, dc_var = "ag_reach", hc_var = "ag_reach_hc")
  
  correlate_dta_gr = rbind(ag_censor_filter_dta, ag_break_internet_dta, ag_fair_creators_dta, ag_criminal_safety_dta, ag_astro_lobby_dta, 
                           ag_platform_power_dta, ag_econ_inno_dta, ag_user_rights_dta, ag_misinfo_dta, ag_diversity_dta, ag_reach_dta)
  
  correlate_dta_gr$correlation = as.numeric(correlate_dta_gr$correlation)
  
  # Precision Plot
  cor_plot = correlate_dta_gr %>%
    mutate(name = fct_reorder(name, correlation)) %>%
    ggplot(aes(x=name, y=correlation)) +
    geom_bar(stat="identity") +
    scale_y_continuous(limits = c(0.0, 1.0), breaks = seq(0.0, 1.0, by = 0.2)) + 
    scale_x_discrete(labels = c(ag_censor_filter = "Censorship & Uploadfilter",
                                ag_break_internet = "Breaking the Internet",
                                ag_fair_creators = "Fair Pay & Creators",
                                ag_criminal_safety = "Criminality & Public Safety",
                                ag_astro_lobby = "Astroturfing & Lobbying",
                                ag_platform_power = "Bad Platforms & Market Power",
                                ag_econ_inno = "Economy & Innovation",
                                ag_user_rights = "User Rights & Cybersecurity",
                                ag_misinfo = "Misinformation",
                                ag_diversity = "Diversity & Free Press",
                                ag_reach = "Reach"
    )) + 
    xlab("") +
    ggtitle("Correlation") +
    coord_flip() +
    theme_bw()
  
  return(cor_plot)
  
}

###### English Newspapers ######
# load human coded data
hc_news_SOPA = read_xlsx("Validation/SOPA_Newspapers_HumanCodingSample_JD.xlsx")
hc_news_EU_en = read_xlsx("Validation/EU_English_Newspapers_HumanCodingSample_JD.xlsx")

#merge human coded data
hc_news_EU_en = select(hc_news_EU_en, -paper)
hc_news_en = rbind(hc_news_EU_en, hc_news_SOPA)

# load dictionary coded data
dc_news_SOPA = read_xlsx("SOPA/Frames_data_SOPAnews.xlsx")
dc_news_EU = read.csv("EUCD/Frames_data_EUCDnews_allLang.csv")

#merge dictionary coded data
dc_news_SOPA = select(dc_news_SOPA, c(datetime,	article_id,	paragraph_id, paragraph, censor,	overly_broad, break_internet, uploadfilter, 
                                              economy, inno, lobby, astroturf, diversity_freepress, privacy, consumers_users, public_safety,
                                              criminal, fair_pay, culture_creator, bad_platforms, market_power, misinfo, reach))

dc_news_EU = select(dc_news_EU, c(datetime, article_id, paragraph_id, paragraph, censor,	overly_broad, break_internet, uploadfilter, 
                                          economy, inno, lobby, astroturf, diversity_freepress, privacy, consumers_users, public_safety,
                                          criminal, fair_pay, culture_creator, bad_platforms, market_power, misinfo, reach))

dc_news = rbind(dc_news_EU, dc_news_SOPA)

# Join human coding and dictionary coding data
valid_dta_en = inner_join(hc_news_en, dc_news, by = c("article_id", "paragraph_id", "datetime"))

# Group frames
valid_dta_en = group_frames(valid_dta_en)

# Kappa
kappa_news_en = kappa_plot(valid_dta_en)

# Precision Recall
prec_rec_news_en = prec_rec_plot(valid_dta_en)

# Correlations
cor_news_en = cor_plot(valid_dta_en)

plot_news_en = plot_grid(kappa_news_en, cor_news_en, ncol = 2)
plot_news_en = plot_grid(plot_news_en, prec_rec_news_en, nrow = 2)

ggsave("Validation/English_News_Plot.png", plot_news_en, width = 16, height = 8, dpi = 640)
ggsave("Validation/English_News_Plot.pdf", plot_news_en, width = 16, height = 8, dpi = 640)

###### English Tweets ######
# load human coded data
hc_tweets_EU_en = read_xlsx("Validation/EU_English_Tweets_HumanCodingSample_JD.xlsx")

#merge dictionary coded data
dc_tweets_EU = select(dc_tweets_EU, c(datetime,	tweet_id_char, index, text, censor,	overly_broad, break_internet, uploadfilter, 
                                  economy, inno, lobby, astroturf, diversity_freepress, privacy, consumers_users, public_safety,
                                  criminal, fair_pay, culture_creator, bad_platforms, market_power, misinfo, reach, language))

tweet_EU_en = inner_join(hc_tweets_EU_en, dc_tweets_EU, by = c("id" = "index"))
tweet_EU_en = subset(tweet_EU_en, !is.na(id))

# Group frames
valid_dta_en = group_frames(tweet_EU_en)

# Kappa
kappa_tweets_en = kappa_plot(valid_dta_en)

# Precision Recall
prec_rec_tweets_en = prec_rec_plot(valid_dta_en)

# Correlations
cor_tweets_en = cor_plot(valid_dta_en)

plot_tweets_en = plot_grid(kappa_tweets_en, cor_tweets_en, ncol = 2)
plot_tweets_en = plot_grid(plot_tweets_en, prec_rec_tweets_en, nrow = 2)

ggsave("Validation/English_tweets_Plot.png", plot_tweets_en, width = 16, height = 8, dpi = 640)
ggsave("Validation/English_tweets_Plot.pdf", plot_tweets_en, width = 16, height = 8, dpi = 640)

###### German Newspapers ######
# load human coded data
hc_news_EU_de = read_xlsx("Validation/EU_German_Newspapers_HumanCodingSample_JD.xlsx")

#merge human coded data
hc_news_EU_de = select(hc_news_EU_de, -c(paper, paragraph))

# Join human coding and dictionary coding data
valid_dta_de = inner_join(hc_news_EU_de, dc_news_EU, by = c("article_id", "paragraph_id", "datetime"))

# Group frames
valid_dta_de = group_frames(valid_dta_de)

# Kappa
kappa_news_de = kappa_plot(valid_dta_de)

# Precision Recall
prec_rec_news_de = prec_rec_plot(valid_dta_de)

# Correlations
cor_news_de = cor_plot(valid_dta_de)

plot_news_de = plot_grid(kappa_news_de, cor_news_de, ncol = 2)
plot_news_de = plot_grid(plot_news_de, prec_rec_news_de, nrow = 2)

ggsave("Validation/German_News_Plot.png", width = 16, height = 8, dpi = 640)
ggsave("Validation/German_News_Plot.pdf", width = 16, height = 8, dpi = 640)

###### German Tweets ######
hc_tweets_EU_de = read_xlsx("Validation/EU_German_Tweets_HumanCodingSample_JD.xlsx")

tweet_EU_de = inner_join(hc_tweets_EU_de, dc_tweets_EU, by = c("id" = "index"))
tweet_EU_de = subset(tweet_EU_de, !is.na(id))

# Group frames
valid_dta_de = group_frames(tweet_EU_de)

# Kappa
kappa_tweets_de = kappa_plot(valid_dta_de)

# Precision Recall
prec_rec_tweets_de = prec_rec_plot(valid_dta_de)

# Correlations
cor_tweets_de = cor_plot(valid_dta_de)

plot_tweets_de = plot_grid(kappa_tweets_de, cor_tweets_de, ncol = 2)
plot_tweets_de = plot_grid(plot_tweets_de, prec_rec_tweets_de, nrow = 2)

ggsave("Validation/German_tweets_Plot.png", plot_tweets_de, width = 16, height = 8, dpi = 640)
ggsave("Validation/German_tweets_Plot.pdf", plot_tweets_de, width = 16, height = 8, dpi = 640)
