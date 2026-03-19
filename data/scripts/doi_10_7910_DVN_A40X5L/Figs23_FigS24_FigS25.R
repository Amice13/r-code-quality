##
rm(list = ls())

library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(cowplot)
library(pscl, quietly = T) # IRT models
library(Hmisc)
library(ggpubr)

# In this script we compare ideology of imputated versus non-imputed and output Figure S23, S24, and S25


## Load in data
dat = read_csv('partisan-survey-analysis.csv')

# Set ideology levels
ideology.levels = c('Extremely Liberal','Liberal','Slightly Liberal','Moderate; middle of the road',
                   'Slightly Conservative','Conservative','Extremely Conservative')
interest.levels = c('Not at all interested','Not very interested','Somewhat interested','Very Interested')
discuss.levels = c("Never or close to never","Just a few times a year","About once a month",
                   "About once a week, but not every day","Nearly every day")


## recode variables and filter sample to respondents who verify their identity
dat = dat%>%
  mutate(imputed = ifelse(L2_recorded_pid == 'i',1,0),
         L2_recorded_pid = recode(L2_recorded_pid, "d" = "Democrat","r"= "Republican","i" = "Independent"),
         self_ideology = factor(self_ideology,levels = ideology.levels),
         ideology_numeric = recode(self_ideology,'Extremely Liberal'=1,
                                   'Liberal'=2,
                                   'Slightly Liberal'=3,
                                   'Moderate; middle of the road'=4,
                                   'Slightly Conservative'=5,
                                   'Conservative'=6,
                                   'Extremely Conservative' = 7),
         interest = factor(interest,levels = interest.levels),
         high_interest = ifelse(interest == 'Very Interested',1,0),
         discuss = factor(discuss, levels = discuss.levels),
         impute_state = ifelse(L2_state_record_pid==1&imputed==1,'State PID/ Imputed',
                               ifelse(L2_state_record_pid==1&imputed==0,'State PID/ Not Imputed',
                                      ifelse(L2_state_record_pid==0&imputed==1,'State No PID/ Imputed',
                                             'State No PID/ Not Imputed'))),
         imputed_text = ifelse(imputed == 1, "Imputed", "Not Imputed"),
         all_party = ifelse(imputed == 1, party_3,L2_recorded_pid)) %>% ##imputed and self reported party
    filter(self == 'Yes')
  

## Load custom ggplot themes
source('theme_jake.R')
  
############




ideology.plot.party = ggplot(data = dat%>%
                               filter(is.na(self_ideology)==F&is.na(party_3)==F&party_3%in%c('Democrat','Republican')), 
                             aes(x = self_ideology))+
  geom_bar(aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..])) + 
  scale_y_continuous(name="Percent") +
  theme_jake()+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
                                   axis.title.x = element_blank()) +
  facet_wrap(party_3~imputed_text,ncol=2)

ggsave('FigS23.pdf',
       width = 8.5,
       height = 11)





ideology.plot.party.state = ggplot(data = dat%>%
                                     filter(is.na(self_ideology)==F&is.na(party_3)==F&party_3%in%c('Democrat','Republican')), 
                                   aes(x = self_ideology))+
  geom_bar(aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..])) + 
  scale_y_continuous(name="Percent") +
  theme_jake()+ 
   theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        plot.title = element_text(size = 24)) +
    facet_wrap(party_3~impute_state)

ggsave('FigS24.pdf',
       width = 8.5,
       height = 11)


#########


##what about policy positions?
# rollcall matrix, cols are different policy variables
rollcall_mat <- dat%>%
  select("Cong_Affordable","Cong_Health","Cong_Choice","Cong_Kate","Cong_Countering",
         "Cong_Sanctuary","Cong_assault","Cong_impeach","Cong_pay")
# create rollcall object
rc <- rollcall(rollcall_mat, yea = "For", nay = "Against",
               legis.names = dat$ResponseId, 
               vote.names = c("Cong_Affordable","Cong_Health","Cong_Choice","Cong_Kate","Cong_Countering",
                              "Cong_Sanctuary","Cong_assault","Cong_impeach","Cong_pay"))
# create IRT model
ideals <- ideal(rc, normalize = T)

# show distribution if ideal points

ideals.dat = data.frame(ResponseId = labels(ideals$xbar)[[1]], ideals = as.numeric(ideals$xbar))
dat = left_join(dat,ideals.dat)

##two ideology plots by imputation score

dat = dat%>%
  mutate(r.post.bin = cut_number(L2_pos.imp.r,n = 5),
         d.post.bin = cut_number(L2_pos.imp.d,n = 5),
         ideals = ideals*-1)


axis.size = 7
label.size = 9


library(RColorBrewer)
colors = brewer.pal(3,"Accent")[1:3]

ideology.plot.binned.all = ggplot(data = dat%>%
                                    filter(is.na(r.post.bin)==F), 
                                  aes(y = ideology_numeric,x =d.post.bin))+
  stat_summary(fun.y='mean',aes(group = impute_state,color = impute_state),
               geom="point",
               position = position_dodge(.75), size = 5)+
  scale_color_manual(values = colors) + 
  guides(fill = FALSE,color=guide_legend("Imputation"))+
  geom_boxplot(aes(fill = impute_state), 
               alpha = .5)+
  scale_fill_manual(values = colors) + 
  theme_jake() +
  theme(legend.position="right")+ 
  scale_y_continuous(name = 'Self-reported Ideology')+
  scale_x_discrete(name = 'Pr(D)')


ideology.plot.continuous.all = ggplot(data = dat%>%filter(is.na(self_ideology)==F), 
                                      aes(y = ideals,x = L2_pos.imp.d,
                                          color = impute_state))+
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess", size = 1.5)+
  scale_color_manual(values = colors) + 
  theme_jake()+
  theme(legend.position="right")+ 
  guides(color=guide_legend("Imputation"))+
  scale_y_continuous(name = 'Issue-scaled Ideology')+
  scale_x_continuous(name = 'Pr(D)')


out.grid = ggarrange(ideology.plot.binned.all,ideology.plot.continuous.all,
                     nrow=2,
                     ncol = 1,
                     common.legend = TRUE, legend="bottom")
ggsave("FigS25.pdf",out.grid,
       width = 8.5,height = 11.5,units = 'in')
