library(pacman)

p_load(
  'betareg',
  'datapasta',
  'dplyr',
  'estimatr',
  'tidyverse',
  'readxl',
  'modeest',
  'sjPlot',
  'emmeans',
  'olsrr',
  'writexl',
  'gtsummary',
  'stargazer',
  'magrittr',
  'ggplot2',
  'scales',
  'lemon',
  'knitr',
  'ggpmisc',
  'fastDummies',
  'gdtools',
  'kableExtra',
  'wesanderson',
  'viridis',
  'Hmisc',
  'ggpubr',
  'broom',
  'purrr',
  'car',
  'rstatix',
  'skimr',
  'haven',
  'glue',
  "lubridate",
  'DescTools',
  'labelled',
  'AER',
  'glmmTMB',
  'gtsummary'
)
setwd('/Users/philippchapkovski/Documents/war_polarization_data/FINAL/all_sessions')
df<-read_csv('polar_2022-06-02.csv')%>%
  filter(player.payable==1) %>%
  rename_at(  vars(contains("player.")), funs(str_replace(., "player.", ""))
)%>%
  filter(role=='dictator',opinion_war==1)

# Charts for LSE
# Chart 1: overall polarization (yes/no only )
# Chart 2: polarization for single treatments
# Chart 3: Polarization for intensive beliefs only
# Chart 4: beliefs


# Chart 1: overall polarization (yes/no only )
# 
df%>% 
  filter(partner_position%in%c('yes','no'))%>%
  mutate(partner_position=dplyr::recode(partner_position, `yes`='pro-war', no='anti-war'))%>%
  t_test(dg_decision~partner_position)
df%>% 
  filter(partner_position%in%c('yes','no'), opinion_war==1)%>%
  mutate(partner_position=dplyr::recode(partner_position, `yes`='pro-war', no='anti-war'))%>%group_by(partner_position,)%>%summarise(mean(dg_decision), sd(dg_decision),n())
my_comparisons = list( c("pro-war", "anti-war") )

labeller <-function(df, grouper, dv){
  t<-df%>%
    group_by(across(grouper))%>%
    summarise(m=round(mean(!!sym(dv)),2), n=n())%>%
    mutate(label=glue('{m} (N={n})'))
   t$label
}

labeller(df%>% 
           filter(partner_position%in%c('yes','no'))%>%
           mutate(partner_position=dplyr::recode(partner_position, `yes`='pro-war', no='anti-war')), 'partner_position', 'dg_decision')

df%>% 
  filter(partner_position%in%c('yes','no'))%>%
  mutate(partner_position=dplyr::recode(partner_position, `yes`='pro-war', no='anti-war'))%>%
  ggbarplot(y = "dg_decision", x = "partner_position",
            add = "mean_ci", label = labeller(.,'partner_position', 'dg_decision'), lab.vjust = -4.4)+
  stat_compare_means(comparisons = my_comparisons,label = "p.signif", method = "t.test",
                     ref.group = "0.5", label.y=10)+
  xlab('Partner\'s position')+
  ylab('Average dictator transfer')
  



# Chart 2: polarization for single treatments
my_comparisons = list( c("pro-war", "anti-war"),c("pro-war", "not shown") )
df%>%
  mutate(treat=if_else(treatment=='baseline', 'forced_reveal', treatment))%>%
  mutate(partner_position=tidyr::replace_na(partner_position, 'nr'))%>%
  mutate(partner_position=if_else(treat=='reveal_before' &dictator_reveal==T,'nr', partner_position))%>%
  filter(treat=='vl')%>%
  mutate(partner_position=dplyr::recode(partner_position, `yes`='pro-war', no='anti-war', 'nr'='not shown'))%>%
  mutate(treat=dplyr::recode(treat, forced_reveal='Forced reveal', reveal_before='Dictator reveals', vl='Recipient reveals'))%>%
  ggbarplot(y = "dg_decision", x = "partner_position",
            order=c('anti-war', 'not shown', 'pro-war'),
            add = "mean_ci", label  = labeller(.,'partner_position', 'dg_decision') , lab.vjust = -3.6)+
  stat_compare_means(comparisons = my_comparisons,label = "p.signif", method = "t.test",
                     # ref.group = "0.5",
                     label.y=c(8,13))+
  xlab('Partner\'s position')+
  ylab('Average dictator transfer')


# Chart 3: Polarization for intensive beliefs only
my_comparisons = list( c("pro-war", "anti-war"),c("pro-war", "not shown") )
df%>%
  mutate(treat=if_else(treatment=='baseline', 'forced_reveal', treatment))%>%
  mutate(partner_position=tidyr::replace_na(partner_position, 'nr'))%>%
  mutate(partner_position=if_else(treat=='reveal_before' &dictator_reveal==T,'nr', partner_position))%>%
  filter(treat=='vl')%>%
  mutate(partner_position=dplyr::recode(partner_position, `yes`='pro-war', no='anti-war', 'nr'='not shown'))%>%
  mutate(treat=dplyr::recode(treat, forced_reveal='Forced reveal', reveal_before='Dictator reveals', vl='Recipient reveals'))%>%
  mutate(opinion_intensity=dplyr::recode(opinion_intensity, `1`='Absolutely supports', `0`='Rather supports'))%>%
  ggbarplot(y = "dg_decision", x = "partner_position",
            # facet.by='opinion_intensity',
            add = "mean_se", label = TRUE, lab.vjust = -3.6,lab.nb.digits=2)+
  stat_compare_means(comparisons = my_comparisons,label = "p.signif", method = "wilcox.test",
                     label.y=c(8,13))+
  xlab('Partner\'s position')+
  ylab('Average dictator transfer')


# Chart 4: beliefs about revealers
#
my_comparisons = list( c("% of pro-war revealers", "% of anti-war revealers") )
df%>%select(vl_pro_belief,vl_contra_belief)%>%
  drop_na()%>%
  pivot_longer(cols=c(vl_pro_belief,vl_contra_belief))%>%
  mutate(name=dplyr::recode(name,vl_pro_belief="% of pro-war revealers", vl_contra_belief="% of anti-war revealers" ))%>%
  ggbarplot(y = "value", x = "name",
            add = "mean_ci", label = T, lab.vjust = -1.6)+
  stat_compare_means(comparisons = my_comparisons,label = "p.signif", method = "t.test", label.y=c(75))+
  xlab('')+
  ylab('Beliefs about share of revealers')




# Chart 5: beliefs about share
my_comparisons = list( c("pro-war", "anti-war"),c("pro-war", "not shown"),c("anti-war", "not shown") )

df%>%
  mutate(treat=if_else(treatment=='baseline', 'forced_reveal', treatment))%>%
  mutate(partner_position=replace_na(partner_position, 'nr'))%>%
  mutate(partner_position=dplyr::recode(partner_position, `yes`='pro-war', no='anti-war', 'nr'='not shown'))%>%
  ggbarplot(y = "proportion", x = "partner_position",
            add = "mean_ci", label = TRUE, lab.vjust = -1.6,lab.nb.digits=2)+
  stat_compare_means(comparisons = my_comparisons,label = "p.signif", method = "t.test", label.y=c(80,88,96))+
  xlab('Partner\'s position')+
  ylab('Beliefs about share of pro-war supporters')


# beliefs for those who is were matched with nr and no:
my_comparisons = list( c('yes','no'),c('yes','nr'),c('no','nr') )
df%>%select(vl_pro_belief,vl_contra_belief, partner_position)%>%
  drop_na()%>%
  ggbarplot(y = "vl_contra_belief", x = "partner_position",
            add = "mean_ci", label = TRUE, lab.vjust = -1.6,lab.nb.digits=2)+
  stat_compare_means(comparisons = my_comparisons,label = "p.signif", method = "t.test", label.y=c(80,88,96))
