############################################################################################
#### Fig A16: Tone of tweets by popular Twitter accounts across main countries mentioned
#### Fig A17: Tone of tweets by popular Twitter accounts across topics of tweets
#### data: twitter/data/figA16_country_tone.csv
####       twitter/data/figA17_topic_tone.csv
#### outputs: twitter/figs/FigA16_country_tone_count.pdf
####          twitter/figs/FigA17_topic_tone_count.pdf
############################################################################################

library("tidyverse"); library("RColorBrewer"); library("here")

country_tone_df = read_csv(here("twitter/data/figA16_country_tone.csv"))
topic_tone_df = read_csv(here("twitter/data/figA17_topic_tone.csv"))

## Below is vestigial from when aggregation happened here. 
# 
# tweets_sample_labeled = read_csv("Data/text_analysis_r_and_r/popular_account_tweets_country_cn_weighted_sample_labeled.csv",
#                                  col_types = "cDccccccciiiiiiiiiinn") %>%
#   mutate(type_complete = case_when(type_complete == "Activists or US / Taiwan / Hong Kong Politics" |
#                                      type_complete == "Citizen Journalists / Political Bloggers" ~ "Activists or Citizen Journalists",
#                                    TRUE ~ type_complete)) %>%
#   mutate(type_complete = factor(type_complete)) %>%
#   mutate(type_complete = fct_relevel(type_complete, "International News Agencies", "Activists or Citizen Journalists", "State Media or Chinese Officials"))
# 
# unique(tweets_sample_labeled$topic)
# 
# entities_to_plot = c("China", "US", "Hong Kong", "Taiwan", "Europe", "Japan", "UK")
# 
# country_tone_df = tweets_sample_labeled %>%
#   filter(main_entity %in% entities_to_plot) %>%
#   rename(Tone = tone) %>%
#   mutate(Tone = str_to_title(Tone)) %>%
#   group_by(main_entity) %>%
#   mutate(Count=n(),
#          percentage=n()/sum(Count)) %>%
#   select(!c(author_id, created_at, name, text, username))
# 
# write_csv(country_tone_df, here("twitter/data/figA16_country_tone.csv"))
# 
# topics_to_exclude = c("General politics", "General science",
#                       "General entertainment", "General business",
#                       "General news", "General Covid")
# 
# topic_tone_df = tweets_sample_labeled %>%
#   filter(!topic %in% topics_to_exclude) %>%
#   rename(Tone = tone) %>%
#   mutate(Tone = str_to_title(Tone)) %>%
#   group_by(topic) %>%
#   mutate(Count=n()) %>%
#   mutate(topic = str_replace(topic, "Covid", "COVID")) %>%
#   mutate(topic = fct_reorder(topic, Count)) %>%
#   select(!c(author_id, created_at, name, text, username))
# 
# write_csv(topic_tone_df, here("twitter/data/figA17_topic_tone.csv"))



### Fig A16


p = country_tone_df %>% 
  mutate(type_complete = factor(type_complete)) %>%
  mutate(type_complete = fct_relevel(type_complete, "International News Agencies", "Activists or Citizen Journalists", "State Media or Chinese Officials")) %>%
  ggplot(aes(x=reorder(main_entity, Count), fill=Tone)) +
  geom_bar(stat='Count') +
  coord_flip() +
  facet_wrap(~type_complete, scales="free_x", ncol=3) +
  theme_classic() +
  theme(legend.position="top",
        axis.title.y = element_blank()) +
  ylab("Count")

pdf(here("twitter/figs/FigA16_country_tone_count.pdf"), width = 8, height = 4)
print(p)
dev.off()


### Fig A17


p = topic_tone_df %>% 
  mutate(type_complete = factor(type_complete)) %>%
  mutate(type_complete = fct_relevel(type_complete, "International News Agencies", "Activists or Citizen Journalists", "State Media or Chinese Officials")) %>%
  ggplot(aes(x=reorder(topic, Count), fill=Tone)) +
  geom_bar(stat='Count') +
  coord_flip() +
  #theme_bw() +
  facet_wrap(~type_complete, scales="free_x", ncol=3) +
  theme_classic() +
  theme(legend.position="top",
        axis.title.y = element_blank()) +
  scale_y_continuous(breaks= scales::pretty_breaks()) +
  ylab("Count")

pdf(here("twitter/figs/FigA17_topic_tone_count.pdf"), width = 10, height = 6)
print(p)
dev.off()


