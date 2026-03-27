# This code merge all toloka datasets into one and merge them with main oTree dataset

library(pacman)
setwd('~/documents/jdm/data/recipients')
p_load('haven',
       'tidyverse',
       'readxl',
       'modeest',
       'ggpubr',
       'kableExtra',
       'glue')


rdf1 <-  read_tsv("disagreed_1.tsv")
rdf2 <-  read_tsv("disagreed_404.tsv")
rdf3 <-  read_tsv("agreed_394.tsv")
rdf1<-rdf1 %>%mutate(position=0)
rdf2<-rdf2 %>%mutate(position=0)
rdf3<-rdf3 %>%mutate(position=1)

df <- bind_rows(rdf1, rdf2, rdf3)
df <- df %>%
  rename_with( ~ gsub('.*:(.*)', '\\1', .)) %>%
  select(-c(
    "accepted",
    "rejected",
    "skipped",
    "expired",
    "reward",
    "testpath"
  ))

recipient_df <-
  df %>% mutate(id = row_number()) %>% rename(recipient_id = worker_id, recipient_position=position) %>%
  select(recipient_id, id, recipient_position)


# getting data for dictators
# we use here the merged df prepared earlier in 'payment_preprocessing.R'
processed_dictators_toloka <-
  read_csv('~/documents/jdm/data/toloka_datasets/processed_toloka_df.csv')
main_df <-
  read_csv('~/documents/jdm/data/raw/polar_2021-10-13.csv') %>% rename_with( ~ gsub('player\\.(.*)', '\\1', .))
merged_df <-
  left_join(main_df,
            processed_dictators_toloka,
            by = c("participant.label" = "assignment_id"))
dictators_df <- merged_df %>% filter(status == 'SUBMITTED')

# We have 799 dictators and 799 recipieints:
dictators_df %>% dim()
recipient_df %>% dim()

# we need to sort them 
# based on the position dictators were able to observe.
dictators_df<-dictators_df %>% arrange(partner_position) %>%mutate(merge_id=row_number())
recipient_df<-recipient_df %>% arrange(recipient_position) %>%mutate(merge_id=row_number())
# check up that positions correspond
dictators_df%>%
  left_join(recipient_df, by=c('merge_id'='merge_id'))%>%
  mutate(checkup = partner_position==recipient_position)%>%
  group_by(checkup)%>%tally()


# producing df for recipient payments
dictators_df%>%
  left_join(recipient_df, by=c('merge_id'='merge_id'))%>%
  select(recipient_id, dg_decision)%>%
  mutate(bonus=(50+dg_decision)/100,
         msg=glue('Участник А принял решение: {dg_decision}. Ваш бонус составил {bonus}$'))%>%
  rename(worker_id=recipient_id)%>%
  write_tsv('recipient_bonuses.tsv')

  

