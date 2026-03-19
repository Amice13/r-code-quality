library(pacman)
# setwd('../data/')
p_load('haven',
       'tidyverse',
       'readxl',
       'modeest',
       'ggpubr',
       'kableExtra',
       'glue',
       'VGAM',
       'Publish',
       'sjlabelled'       ,
       'descr',
       'pander',
       'gtsummary',
       'labelled',
       'misty',
       'rstatix',
       'texreg',
       'AER',
       'Zelig'
       
)


# most of the data is cleaned and processed in data_processing.R. Here we read the file and do some very minor 
# cleanings if needed during the analysis
df <- readRDS('processed_data.Rda')
var_label(df$num_age)<-'Age (ordinal)'
var_label(df$num_income)<-'Income (ordinal)'
var_label(df$num_education)<-'Education (ordinal)'
var_label(df$marital)<-'Marital status'
var_label(df$employment)<-'Employment status'
var_label(df$gender)<-'Gender'
var_label(df$reveal)<-'Revealing decision'
var_label(df$dg_decision)<-'DG decision'

# we add some variables based on combination of revealing decisions and alignment of partners' positions
df<-df%>%mutate(revaligned=case_when(
  (reveal == 'Reveal' | reveal == 'Forced_info') & aligned == 'Aligned'~ "Info - Aligned",
  (reveal == 'Reveal' | reveal == 'Forced_info') & aligned == 'Conflicting'~ "Info - Conflicting",
  (reveal == 'Not reveal') ~ "No info",
))


cdf <- df%>%drop_na(gender) # we drop three observations which make the decisions, but were dropped during the questionnaire
#due to some technical glitches
# we still use df for means comparison and anova, but when other factors are used (gender, age, etc) we will refer to cdf 
# to avoid dealing with NAs
 

## Section 3.1. Participants
## Table 1

lgbtlabels <- c('Pro' = 1, 'Contra' = 0)

df %>% 
  dplyr::select(treatment, aligned,opinion_lgbt)%>%
  tbl_strata(strata=treatment,
             ~.x %>%
               tbl_summary(by=opinion_lgbt,
                           statistic=list(all_continuous() ~ ""))
  )



## Section 3.2. Behavior
# Figure 2.
p<-df%>%  ggbarplot( x = c("aligned"), y = "dg_decision",
                     ,facet.by=c( 'treatment'),
                     add = "mean_ci", label = TRUE, lab.vjust = c(+10,-1,-7,-7,-5.5,-1), lab.nb.digits =2)
p+  rotate_x_text()+geom_hline(yintercept = 0)+ylab('DG transfer')+ xlab('Partners position')
ggsave('figure2.png')

 

## Section 3.2. Behavior
# Figure 3.
df%>% ggbarplot( x = c("revaligned"), y = "dg_decision",
                  facet.by=c( 'treatment'),
                  add = "mean_ci", label = TRUE, 
                 lab.vjust = c(-2,+9,-8,-8,-8, -2, -6, -3),
                 lab.nb.digits =2)+
  rotate_x_text()+geom_hline(yintercept = 0)+ylab('DG transfer')+ xlab('Partners position')
 
ggsave('figure3.png')

# Section Appendix
#Additional figure for Appendix
# that's for checking that opinion strength doesn't change the situation
df%>% filter( opinion_strength=='Strong')%>% ggbarplot( x = c("revaligned"), y = "dg_decision",
                  color='treatment',facet.by=c( 'treatment'),
                  add = "mean_ci", label = TRUE, lab.vjust = -1.6, lab.nb.digits =2)+
  rotate_x_text()+geom_hline(yintercept = 0)+ylab('DG transfer')+ xlab('Partners position')

 
# For appendix: checking if revealing frequency changes by intensity of beliefs
df%>%
  filter(reveal!='Forced_info')%>%group_by(opinion_strength,reveal)  %>%tally()%>%
  pivot_wider(names_from=opinion_strength, values_from=n)%>%
  mutate(Strong=Strong/sum(Strong), Weak=Weak/sum(Weak))

 
 

  
## Reasons
# Some  TOBIT regressions
# =========== TABLE 2 CODE ==================
# Tobit regressions. DV: dg_decision. 
t<-df%>%
  mutate(treatment=as_factor(treatment))%>%
  mutate(treatment=relevel(treatment, ref='reveal_after'))

fullaligned_min <- dg_decision ~  treatment*aligned
 
fullaligined_indices <-
  dg_decision ~ treatment*aligned  + sdi + ias + risk + scs
fullaligned_ses <-
  dg_decision ~ treatment*aligned + sdi + ias + risk + scs + num_age + gender +
  num_education + num_income + employment
fullaligned_max <-
  dg_decision ~ treatment*aligned + sdi + ias + risk + scs + num_age + gender + num_education + num_income +
  employment + instructions_clarity + cq_err_counter

m_fullaligned_min <- tobit(fullaligned_min, data = t,left=-50, right=50)
m_fullaligined_indices <- tobit(fullaligined_indices, data = t,left=-50, right=50)
m_fullaligned_ses <- tobit(fullaligned_ses, data = t,left=-50, right=50)
m_fullaligned_max <- tobit(fullaligned_max, data = t,left=-50, right=50)
matrixreg(list(m_fullaligned_min, m_fullaligined_indices,m_fullaligned_ses,m_fullaligned_max))%>%kbl()%>%kable_classic_2()
 
# =========== END OF TABLE 2 CODE ========================

# =========== APPENDIX (LINEAR REGRESSSIONS ============== 
# Linear regressions. DV: dg_decision. 
fullaligned_min <- dg_decision ~  treatment*aligned
 
fullaligined_indices <-
  dg_decision ~ treatment*aligned  + sdi + ias + risk + scs
fullaligned_ses <-
  dg_decision ~ treatment*aligned + sdi + ias + risk + scs + num_age + gender +
  num_education + num_income + employment
fullaligned_max <-
  dg_decision ~ treatment*aligned + sdi + ias + risk + scs + num_age + gender + num_education + num_income +
  employment + instructions_clarity + cq_err_counter


m_fullaligned_min <- lm(fullaligned_min, data = t)
m_fullaligined_indices <- lm(fullaligined_indices, data = t )
m_fullaligned_ses <- lm(fullaligned_ses, data = t)
m_fullaligned_max <- lm(fullaligned_max, data = t)



matrixreg(list(m_fullaligned_min, m_fullaligined_indices,m_fullaligned_ses,m_fullaligned_max))%>%kbl()%>%kable_classic_2()
 
# =========== END OF APPENDIX (LINEAR REGRESSSIONS)  ==============


# =========== TABLE 3 CODE ========================
# Tobit regressions. DV: dg_decision. 
t%>%group_by(dg_decision==0)%>%group_by(treatment)%>%tally%>%mutate(f=n/sum(n))
minmod<-dg_decision ~ revaligned
maxmod<-dg_decision ~ revaligned+ opinion_lgbt+sdi+ias+risk+scs+num_age+gender+num_education+num_income+employment+instructions_clarity+cq_err_counter

fr_min <- tobit(minmod, data = t%>%filter(treatment=='forced_reveal'),left=-50, right=50)
rb_min <- tobit(minmod, data = t%>%filter(treatment=='reveal_before'),left=-50, right=50)
ra_min <- tobit(minmod, data = t%>%filter(treatment=='reveal_after'),left=-50, right=50)

fr_max <- tobit(maxmod, data = t%>%filter(treatment=='forced_reveal'),left=-50, right=50)
rb_max <- tobit(maxmod, data = t%>%filter(treatment=='reveal_before'),left=-50, right=50)
ra_max <- tobit(maxmod, data = t%>%filter(treatment=='reveal_after'),left=-50, right=50)


        
matrixreg(list(fr_min,fr_max, rb_min,rb_max, ra_min,ra_max), custom.model.names=c('FR - Min', 'FR - Max',
                                                                  'RB - Min', 'RB - Max',
                                                                  'RA - Min', 'RA - Max')        )%>%kbl()%>%kable_classic_2()
 
# =========== END OF TABLE 3 CODE ========================


# ===========  APPENDIX (LINEAR REGRESSSIONS of Table 3)  ==============

minmod<-dg_decision ~ revaligned
maxmod<-dg_decision ~ revaligned+ opinion_lgbt+sdi+ias+risk+scs+num_age+gender+num_education+num_income+employment+instructions_clarity+cq_err_counter

fr_min <- lm(minmod, data = t%>%filter(treatment=='forced_reveal') )
rb_min <- lm(minmod, data = t%>%filter(treatment=='reveal_before') )
ra_min <- lm(minmod, data = t%>%filter(treatment=='reveal_after') )

fr_max <- lm(maxmod, data = t%>%filter(treatment=='forced_reveal') )
rb_max <- lm(maxmod, data = t%>%filter(treatment=='reveal_before') )
ra_max <- lm(maxmod, data = t%>%filter(treatment=='reveal_after'))


        
matrixreg(list(fr_min,fr_max, rb_min,rb_max, ra_min,ra_max), custom.model.names=c('FR - Min', 'FR - Max',
                                                                  'RB - Min', 'RB - Max',
                                                                  'RA - Min', 'RA - Max')        )%>%kbl()%>%kable_classic_2()
  
tbin<- t%>%mutate(dg_decision=dg_decision==0)
minmod<-dg_decision ~ revaligned*treatment
maxmod<-dg_decision ~ revaligned*treatment+ opinion_lgbt+sdi+ias+risk+scs+num_age+gender+num_education+num_income+employment+instructions_clarity+cq_err_counter

log_min  <- zelig(minmod,  data = tbin, model = "logit", cite=F)
log_max <- zelig(maxmod,  data = tbin, model = "logit", cite=F)
matrixreg(list(log_min, log_max))%>%kbl()%>%kable_classic_2()

# =========== END OF APPENDIX (LINEAR REGRESSSIONS of Table 3)  ==============

# =========== TABLE 4 CODE ========================
# Logistic regressions for revealing decision.DV: revealing decision (`reveal`)
# The code below uses zelig library because its summary results are knitted together by texreg.
# Zelig library for some reasons is not downloadable from CRAN repository anymore
# But the same code is reproducible using `lrm` from `rms` package - see below.  


form_for_fullboth <- reveal ~ treatment+dg_decision+sdi+
                        ias+
                        risk+scs+opinion_lgbt+instructions_clarity+cq_err_counter
form_for_min <- num_reveal ~ dg_decision+sdi+ias+risk+scs+opinion_lgbt+instructions_clarity+cq_err_counter
form_for_full<-num_reveal ~ dg_decision+sdi+ias+risk+scs+opinion_lgbt+num_age+gender+num_education+num_income+employment+instructions_clarity+cq_err_counter
log_min_full <- zelig(form_for_fullboth,  data = df%>%filter(treatment!='forced_reveal'), model = "logit", cite=F)

log_min_ra <- zelig(form_for_min,  data = df%>%filter(treatment=='reveal_after'), model = "logit", cite=F)
log_full_ra <- zelig(form_for_full,  data = df%>%filter(treatment=='reveal_after'), model = "logit", cite=F)
log_min_rb <- zelig(form_for_min,  data = df%>%filter(treatment=='reveal_before'), model = "logit", cite=F)
log_full_rb <- zelig(form_for_full,  data = df%>%filter(treatment=='reveal_before'), model = "logit", cite=F)

knitreg(c(log_min_full, log_min_rb, log_full_rb, log_min_ra, log_full_ra),
        # custom.coef.map=list("num_education"='Education categorical'),
        custom.model.names=c('MF','M1','M2', 'M3', 'M4'),
        custom.header=list('Logit regression of revealing decision'=1:5))

# TABLE 4 - lrm version
p_load('rms')

form_for_fullboth <- num_reveal ~ treatment+dg_decision+sdi+
  ias+  risk+scs+opinion_lgbt+instructions_clarity+cq_err_counter
form_for_min <- num_reveal ~ dg_decision+sdi+ias+risk+scs+opinion_lgbt+instructions_clarity+cq_err_counter
form_for_full<-num_reveal ~ dg_decision+sdi+ias+risk+scs+opinion_lgbt+num_age+gender+num_education+num_income+employment+instructions_clarity+cq_err_counter

log_min_full <- lrm(form_for_fullboth,  data = df%>%filter(treatment!='forced_reveal'))
log_min_ra <- lrm(form_for_min,  data = df%>%filter(treatment=='reveal_after'))
log_full_ra <- lrm(form_for_full,  data = df%>%filter(treatment=='reveal_after'))
log_min_rb <- lrm(form_for_min,  data = df%>%filter(treatment=='reveal_before'))
log_full_rb <- lrm(form_for_full,  data = df%>%filter(treatment=='reveal_before'))

knitreg(list(log_min_full, log_min_rb, log_full_rb, log_min_ra, log_full_ra),
        # custom.coef.map=list("num_education"='Education categorical'),
        custom.model.names=c('MF','M1','M2', 'M3', 'M4'),
        custom.header=list('Logit regression of revealing decision'=1:5))


# =========== END OF TABLE 4 CODE ========================
 
# 
 
 
## Beliefs


# Average belief in baseline vs average donation in baseline.
# The gap in beliefs about polarization in RB is 14.6 versus 10.127 of the true value.
# 
# the gap in beliefs for forced reveal is 18.88 vs 17.14.
# 
# for baseline which is reveal_after the difference between beliefs about transfers and real transfers  is 3.5 (-3.7 of beliefs, and -0.2 of the true value), so people are slightly (although unsignificantly pessimistic about transfers of others.)

# =================== FIGURE 4 ========================
 


# First plot - beliefs vs. real decision values in FR treatment
b1<-  df%>%
  filter(treatment=='forced_reveal')%>%
   mutate(belief=if_else(revaligned=='Info - Conflicting', dg_belief_fr_diff, dg_belief_fr_same))%>%
     dplyr::select(participant.label,dg_decision,belief, revaligned ,belief)%>%
  rename(decision=dg_decision)%>%
   pivot_longer(cols=c(decision,belief))%>%
   ggbarplot(x='name', y='value', 
             facet.by = 'revaligned',
            add = "mean_ci", label = TRUE, lab.vjust = c(6,6, -1,-1), lab.nb.digits =2)+
  xlab('forced_reveal')+ylab('')
b1<-ggpar(b1,ylim=c(NA, 15))
b1  
ggsave('figure4_1.png')
 

# Second plot - beliefs vs. real decision values in FR treatment
 
b2<- df%>%
  filter(treatment=='reveal_after')%>%
   mutate(belief=dg_belief_ra)%>%
     dplyr::select(participant.label,dg_decision,belief, revaligned ,belief)%>%
  rename(decision=dg_decision)%>%
   pivot_longer(cols=c(decision,belief))%>%
   ggbarplot(x='name', y='value', 
            add = "mean_ci", label = TRUE, lab.vjust = c(9,-3), lab.nb.digits =2)+
  xlab('reveal_after')+ylab('')

ggsave('figure4_2.png') 
 
# Third plot - beliefs vs. real decision values in RB treatment
 
b3<- df%>%
  filter(treatment=='reveal_before')%>%
   mutate(
     belief=case_when(revaligned=='No info'~dg_belief_rb_nonrev,
                      revaligned=='Info - Conflicting'~dg_belief_rb_rev_diff,
                      revaligned=='Info - Aligned'~dg_belief_rb_rev_same)     )%>%
     dplyr::select(participant.label,dg_decision,belief, revaligned ,belief, reveal)%>%
   rename(decision=dg_decision)%>%
   pivot_longer(cols=c(decision,belief))%>%
   ggbarplot(x='name', y='value', 
            facet.by='revaligned',
            add = "mean_ci", label = TRUE, lab.vjust = c(-3,-4,-1,-1,-3,-3), lab.nb.digits =2)+  xlab('reveal_before')+ylab('')
            
            
            
b3<-ggpar(b3,ylim=c(NA, 17))
ggarrange(b1,b2,b3,ncol=1)
ggsave('figure4.png', width=10, height=10, dpi='retina')


# =================== END OF FIGURE 4 ========================
# =================== FIGURE 5 ========================

t1<-df%>%mutate(gap=dg_belief_rb_rev_same-dg_belief_rb_rev_diff)%>%dplyr::select(treatment, reveal, gap)%>%drop_na(gap)
t2<-df%>%mutate(gap=dg_belief_fr_same-dg_belief_fr_diff)%>%dplyr::select(treatment, reveal, gap)%>%drop_na(gap)
t3<-bind_rows(t1,t2)

diffplot <- t3%>%ggbarplot( x = "reveal", y = "gap",
                            add = "mean_ci", label = TRUE, lab.vjust = c(-9,-10,-9), lab.nb.digits =2)


ggpar(diffplot, ylab='Polarization belief', xlab='Revealing decision', ylim=c(NA,25))
ggsave('figure5.png')

# =================== END OF FIGURE 4 ========================
 




# =============== APPENDIX AND OTHER TEMPORARY THINGS ================


df%>%mutate(gap=dg_belief_rb_rev_diff-dg_belief_rb_rev_same)%>%group_by(reveal)%>%
  summarise(mean(gap, na.rm=T))
rbbeliefplot<-df%>%filter(reveal!='Forced_info')%>%mutate(gap=dg_belief_rb_rev_diff-dg_belief_rb_rev_same) %>%  ggbarplot( x = "reveal", y = "gap",                      add = "mean_ci", label = TRUE, lab.vjust = -1.6, lab.nb.digits =2)

# df%>%select(starts_with('dg_belief_'))%>%names()
frbeliefplot<-df%>%mutate(gap=dg_belief_fr_diff-dg_belief_fr_same) %>%  filter(reveal=='Forced_info')%>%ggbarplot( x = "reveal", y = "gap",add = "mean_ci", label = TRUE, lab.vjust = -1.6, lab.nb.digits =2) 
frbeliefplot<-ggpar(frbeliefplot, ylim = c(-25, 0), ylab='Polarization belief')
rbbeliefplot<-ggpar(rbbeliefplot, ylim = c(-25, 0), ylab='')
ggarrange(frbeliefplot, rbbeliefplot)
 

df%>%pivot_longer(cols=starts_with('dg_belief'), names_to = 'belief_type', values_to='belief_value')%>%
  select(treatment, belief_type, belief_value)

# means and gaps of beliefs for reveal_before
df%>%summarise(mean(dg_belief_rb_rev_diff, na.rm=T), mean(dg_belief_rb_rev_same, na.rm=T))
df%>%filter(treatment=='reveal_before')%>%group_by(revaligned)%>%summarise(mean(dg_decision))
# for forced_reveal
df%>%summarise(mean(dg_belief_fr_same, na.rm=T), mean(dg_belief_fr_diff, na.rm=T))
df%>%filter(treatment=='forced_reveal')%>%group_by(revaligned)%>%summarise(mean(dg_decision))
df%>%filter(treatment=='reveal_after')%>%summarise(mean(dg_decision))
df%>%filter(treatment=='reveal_after')%>%summarise(mean(dg_belief_ra))
 
# on average revealers tend to believe that share of revealers is much higher than what non-revealers believe (57% vs 79%).

 
df%>%filter(treatment!='forced_reveal')%>%group_by(treatment, reveal)%>%summarise(m=mean(reveal_belief, na.rm=T))%>%
  pivot_wider(names_from=reveal, values_from=m)
 
p_load('skimr')
df%>%group_by(treatment)%>%
  filter(treatment=='reveal_before')%>%
  select(dg_belief_rb_rev_diff, dg_belief_rb_rev_same)%>%
  mutate(diff=dg_belief_rb_rev_diff-dg_belief_rb_rev_same)%>%
  skim(diff)

df%>%group_by(treatment)%>%
  filter(treatment=='forced_reveal')%>%
  # select(dg_belief_rb_rev_diff, dg_belief_rb_rev_same)%>%
  mutate(diff=dg_belief_fr_diff-dg_belief_fr_same)%>%
  skim(diff)

df%>%group_by(treatment)%>%
  filter(treatment=='reveal_before')%>%
  group_by(revaligned)%>%
  summarise(mean(dg_decision, na.rm=T),sd(dg_decision, na.rm=T))

 
df%>%
  t_test(proportion~treatment, detailed=T,p.adjust.method = "bonferroni")%>%kbl()%>%kable_classic_2()


df%>%filter(treatment=='forced_reveal')%>%
  t_test(proportion~revaligned, detailed=F,p.adjust.method = "bonferroni")%>%kbl()%>%kable_classic_2()


# TABLE A4
df%>%filter(treatment=='reveal_before')%>%
  wilcox_test(proportion~revaligned, detailed=T,p.adjust.method = "bonferroni")%>%kbl()%>%kable_classic_2()
 

 

t3%>%
  pairwise_wilcox_test(gap ~reveal,
                 p.adjust.method = "bonferroni")%>%kable()%>%kable_classic_2()
t3%>%
  wilcox_effsize(gap ~reveal,
                 p.adjust.method = "bonferroni")%>%kable()%>%kable_classic_2()


 



# t test for difference in beliefs about reveal_belief

df%>%filter(treatment!='forced_reveal')%>%drop_na(reveal,reveal_belief)%>%wilcox_test(reveal_belief~num_reveal)
 
# df%>%filter(treatment!='forced_reveal')%>%group_by(reveal)%>%summarise(mean(reveal_belief))
 
df%>%names()
df%>%group_by(treatment,reveal)%>%summarise(mean(proportion))
 

## Appendix

### Individual characteristics

 
 

### Individual characteristics per treatment
 
 

  df %>% 
  select(treatment,gender,num_age,num_income, num_education,  marital,employment )%>%
   tbl_summary(statistic=list(all_continuous() ~ "{mean} ({sd})"),by=treatment,
               type = list(num_age~'continuous',
                           num_income~'continuous',
                           num_education~'continuous'))

 


### Distribution of DG decisions per treatment, revealing decisions and alignment of partners' positions

 
gghistogram(df, x='dg_decision', color='revaligned', fill='revaligned', facet.by=c('treatment', 'revaligned') , position='dodge')

 
