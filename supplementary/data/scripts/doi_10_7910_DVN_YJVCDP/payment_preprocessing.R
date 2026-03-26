# This code merge all toloka datasets into one and merge them with main oTree dataset

library(pacman)
setwd('~/documents/jdm/data/toloka_datasets')
p_load('haven',
       'tidyverse',
       'readxl',
       'modeest',
       'ggpubr',
       'kableExtra',
       'glue')


df1 <-
  read_tsv("assignments_from_pool_28629126__14-10-2021_ jdm_main_reveal_before_agreed.tsv")
df2 <-
  read_tsv("assignments_from_pool_28632844__14-10-2021_jdm_main_forced_reveal_agreed.tsv")
df3 <-
  read_tsv("assignments_from_pool_28633169__14-10-2021_jdm_main_reveal_after_agreed.tsv")
df4 <-
  read_tsv(
    "assignments_from_pool_28633954__14-10-2021_jdm_main_reveal_before_DISagreed.tsv"
  )
df5 <-
  read_tsv(
    "assignments_from_pool_28635494__14-10-2021_jdm_main_forced_reveal_DISagreed.tsv"
  )
df6 <-
  read_tsv(
    "assignments_from_pool_28638321__14-10-2021_jdm_main_reveal_after_DISagreed.tsv"
  )

df <- bind_rows(df1, df2, df3, df4, df5, df6)
df <- df %>%
  select(-c('GOLDEN:otree_code'), -starts_with(c('HINT', 'ACCEPT'))) %>%
  rename_with(~ gsub('.*:(.*)', '\\1', .)) %>%
  select(-c("accepted", "rejected", "skipped", "expired", "reward"))
df %>%view()
df %>% write_csv('processed_toloka_df.csv')

main_df <-read_csv('~/documents/jdm/data/raw/polar_2021-10-13.csv')%>% rename_with(~ gsub('player\\.(.*)', '\\1', .))  

main_df%>%dim()
merged_df<-left_join(main_df, df, by=c("participant.label"="assignment_id"))
# Three users out of 799 do not have 'payable' status because of minor glitch. We'll still pay them
dfp <- merged_df%>%filter(status=='SUBMITTED')  # dfp stays for df_for_payment

# let's calculate how many recipieints we need of each type (roughly)
dfp%>%group_by(partner_position)%>%tally()

# Following are belief fields. To keep our promise we'll choose one of them randomly for each
# participant
# reveal_belief = models.IntegerField(min=0, max=100)
# dg_belief_ra = models.IntegerField()
# dg_belief_rb_nonrev = models.IntegerField()
# dg_belief_rb_rev_diff = models.IntegerField()
# dg_belief_rb_rev_same = models.IntegerField()
# dg_belief_fr_diff = models.IntegerField()
# dg_belief_fr_same = models.IntegerField()

# True values:
# Share of those who reveal - reveal_belief - true value 68.7
dfp%>%filter(!is.na(reveal))%>%group_by(reveal)%>%tally()%>% mutate(freq=n/sum(n))


# Decision by dictators in reveal_after dg_belief_ra - true value 0
dfp%>%filter(subsession.treatment=='reveal_after')%>%summarise( mode = mlv(dg_decision, method='mfv') )

# Decision by dictators in reveal_before non-revealers dg_belief_rb_nonrev - true value 0
dfp%>%filter(subsession.treatment=='reveal_before', reveal==0)%>%summarise( mode = mlv(dg_decision, method='mfv') )

# Decision by dictators in reveal_before revealers conflicting dg_belief_rb_rev_diff - true value 0
dfp%>%filter(subsession.treatment=='reveal_before', reveal==1, aligned==0)%>%summarise( mode = mlv(dg_decision, method='mfv') )
 
# Decision by dictators in reveal_before revealers aligned dg_belief_rb_rev_same - true value 0
dfp%>%filter(subsession.treatment=='reveal_before', reveal==1, aligned==1)%>%summarise( mode = mlv(dg_decision, method='mfv') )

# Decision by dictators in forced_revealconflicting dg_belief_fr_diff - true value 0
dfp%>%filter(subsession.treatment=='forced_reveal',  aligned==0)%>%summarise( mode = mlv(dg_decision, method='mfv') )

# Decision by dictators in forced_revealconflicting dg_belief_fr_same - true value 0
dfp%>%filter(subsession.treatment=='forced_reveal',  aligned==1)%>%summarise( mode = mlv(dg_decision, method='mfv') )

belief_diff<-function(n,x){
  if (n=='reveal_belief') {
    return (abs(x-68.7))
  } else{
    return (abs(x))
  }
}


# compilating df with bonuses for correct guessing
extra_bonus_df <- dfp %>%
  select('participant.label',
         'reveal_belief',
         starts_with('dg_belief')) %>%
  pivot_longer(cols = c('reveal_belief', starts_with('dg_belief'))) %>%
  drop_na(value) %>%
  group_by(participant.label) %>% sample_n(1) %>%
  mutate(
    belief_diff = belief_diff(name, value),
    belief_true_value = if_else(name == 'reveal_belief', 68.7, 0),
    belief_payoff = if_else(belief_diff <= 10, 0.25, 0),
    belief_bonus_msg = glue(
      'В одном из дополнительных вопросов вы указали {value}. Истинное значение было: {belief_true_value}. Ваш дополнительный бонус: {belief_payoff}.'
    )
  ) %>%
  select(participant.label, belief_payoff, belief_bonus_msg)

dfp<-dfp %>% left_join(extra_bonus_df)
dfp%>%names()
dfp%>%mutate(recipient_payoff = (50+dg_decision)/100)%>%summarise(sum(recipient_payoff)*1.2+n()*0.12)

dfp<-dfp%>%mutate(final_payoff=if_else(participant.payoff==0, 100-dg_decision, participant.payoff))
dfp%>%
  mutate(main_bonus=final_payoff/100,
         bonus=main_bonus+belief_payoff,
         msg=glue('Ваш бонус за основную часть исследования: {main_bonus}. {belief_bonus_msg}. Общий бонус: {bonus}'))%>%
  rename(assignment_id=participant.label)%>%
  # select(assignment_id, worker_id, bonus, msg)%>%
  summarise(mean(bonus), mean(main_bonus), mean(belief_payoff),sd(bonus), sd(main_bonus), sd(belief_payoff))
  # write_tsv('bonuses.tsv')

