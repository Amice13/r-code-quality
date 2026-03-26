#Set working directory to replication folder
#-----------------------------
#Packages necessary to run analyses
#library(tidyverse)
#library(latex2exp)
#library(readxl)
#library(ggpattern)
#library(marginaleffects)
#library(estimatr)
#library(texreg)
#-----------------------------------------------------------------
#Set parameters for plots
title.size <- 20
x.axis.tick.size <- 14
y.axis.tick.size <- 14
x.axis.label.size <- 18
y.axis.label.size <- 18
facet.text <- 15

#-------------------------------------------------------------------
#Load data
cofi <- read_excel('Data/Taboni_COFI_Analysis.xlsx')

cofi <- cofi %>% 
  mutate(Postion = as.numeric(Position),
         Liberal = as.numeric(Liberal),
         `Median Ideology`=as.numeric(`Median Ideology`),
         year = as.numeric(`Publication Year`),
         Issue=as.factor(Issue)) %>%
  group_by(Issue) %>%
  mutate(issue_length = max(year)-min(year),
         number_of_decisions=n())%>%
  ungroup() 




#Create dyadic data
dyads <- cofi %>%
  left_join(cofi, by = 'Issue', relationship = 'many-to-many' ) %>%
  filter(Occurrence.x>Occurrence.y) %>%
  mutate(ideological_distance = abs(`Median Ideology.x`-`Median Ideology.y`),
         bias_compatible = ifelse((Liberal.y==1&`Median Ideology.x`>`Median Ideology.y`)|(Liberal.y==0&`Median Ideology.x`<`Median Ideology.y`),1,0),
         disagreement = ifelse(Liberal.x!=Liberal.y,1,0),
         gap = year.x-year.y)

#Generate information about whether an issue had a circuit split
#and if so when it occurred
summary_stat_data <- cofi %>%
  group_by(Issue) %>%
  mutate(maj_position = mean(Liberal),
         split = ifelse(maj_position==1|maj_position==0,0,1),
         maj_position=ifelse(maj_position>0.5,1,0),
         first_pos = ifelse(Occurrence==1,Liberal,5),
         first_pos = min(first_pos),
         first_split = ifelse(Liberal!=first_pos,Occurrence,2050),
         first_split = min(first_split), 
         not_split_yet = ifelse(Occurrence<=first_split,1,0)) 


#Manipulate data for Figure 9A
decisions <- summary_stat_data %>%
  select(split,number_of_decisions,Issue)%>%
  unique() %>%
  group_by(split, number_of_decisions )%>%
  summarise(n = n())%>%
  ungroup() %>%
  group_by(split) %>%
  mutate(n= n/sum(n))

#Manipulate data for Figure 9B
second_period_splits <- summary_stat_data %>%
  filter(split==1)%>%
  select(first_split,number_of_decisions,Issue)%>%
  unique() %>%
  group_by(first_split, number_of_decisions )%>%
  summarise(n = n())%>%
  ungroup() %>%
  filter(first_split==2)%>%
  group_by(first_split) %>%
  mutate(n= n/sum(n)) %>%
  bind_rows(mutate(filter(decisions,split==1), first_split=0))

#Manipulate data for Figure 10
splits <- summary_stat_data %>%
  filter(split==1) %>%
  select(Issue,first_split) %>%
  unique() %>%
  group_by(first_split) %>%
  summarise(n = n())

hazard_rate <- summary_stat_data %>%
  group_by(Occurrence) %>%
  summarise(base=sum(not_split_yet)) %>%
  left_join(splits, by = c('Occurrence'='first_split')) %>%
  mutate(n=replace_na(n,0)) %>%
  filter(Occurrence>1) %>%
  mutate(hazard = n/base)


#Manipulate Data for Figure 13
number_minority <- summary_stat_data %>%
  ungroup() %>%
  group_by(Issue) %>%
  mutate(num_min = min(n()-sum(Liberal),sum(Liberal))) %>%
  filter(num_min==1) %>%
  filter(max(Occurrence)>2) %>%
  mutate(min_dec =ifelse(Liberal!=maj_position,1,0))

minority_dec_plot <- number_minority %>%
  ungroup() %>%
  filter(min_dec==1) %>%
  mutate(n=n())%>%
  group_by(Occurrence) %>%
  summarise(prop = n()/n) %>%
  unique() %>%
  mutate(minority=1)

all_dec_plot <- number_minority %>%
  ungroup() %>%
  mutate(n=n())%>%
  group_by(Occurrence) %>%
  summarise(prop = n()/n) %>%
  unique() %>%
  mutate(minority=0)%>%
  bind_rows(minority_dec_plot)

#Add instsances where there are no splits to dataframe for plotting
all_dec_plot[20,] <- list(8,0,1)
all_dec_plot[21,] <- list(10,0,1)
all_dec_plot[22,] <- list(11,0,1)


#--------------------------------------
#Descriptive facts mentioned in paper
#Median length of issue
issue_length <- cofi %>%
  select(Issue, issue_length) %>%
  unique()
median(issue_length$issue_length) #14 years


#Number and proportion of splits
split_prop <- summary_stat_data %>%
  select(Issue,split, number_of_decisions) %>%
  unique()

sum(split_prop$split) #83
sum(split_prop$split)/nrow(split_prop) #34.7%

#Median length of issue
split_length <- summary_stat_data %>%
  select(split,number_of_decisions,Issue)%>%
  unique() %>%
  group_by(split) %>%
  summarise(m=median(number_of_decisions))

split_length #Splits 6, Non-splits, 5

#Issues that make it to a tenth decisions without a split 
hazard_rate$base[9]/239 #5.4%

#Number of En Banc Cases
sum(as.numeric(cofi$`En Banc`)) #33


#Liberal panels
liberal_panels <- cofi %>%
  filter(`Median Ideology`<0)
#Conservative panels
conservative_panels <- cofi %>%
  filter(`Median Ideology`>0)

median(conservative_panels$`Median Ideology`)-median(liberal_panels$`Median Ideology`) #0.64

#--------------------------------------------------------------------------------
#Main Text Analysis

#Figure 8---------------------------------
ggplot(cofi, aes(x=Circuit))+geom_histogram(stat='count',fill='grey', color = 'black')+
  scale_y_continuous(name="Count")+
  scale_x_discrete(name="Circuit", limits = c(1,2,3,4,5,6,7,8,9,10,11,'DC')) +
  theme_bw()+
  theme(legend.position="none",#suppress legend
        axis.text.x = element_text(size=18), axis.title.x = element_text(size=x.axis.label.size),
        axis.text.y = element_text(size=18), axis.title.y = element_text(size=y.axis.label.size),
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  annotate('text', x='DC', y=160, label='(A)', cex = 8)
ggsave('Plots/Figure_8A.pdf',width=7, height=5)


ggplot(cofi, aes(x=year))+geom_histogram(binwidth = 5,fill='grey', color = 'black', boundary=1950)+
  scale_x_continuous('Year decided', breaks = c(1950,1960,1970,1980,1990,2000,2010,2020))+
  scale_y_continuous(name="Count")+
  theme_bw()+
  theme(legend.position="none",#suppress legend
        axis.text.x = element_text(size=18), axis.title.x = element_text(size=x.axis.label.size),
        axis.text.y = element_text(size=18), axis.title.y = element_text(size=y.axis.label.size),
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  annotate('text', x=2022.5, y=225, label='(B)', cex = 8)
ggsave('Plots/Figure_8B.pdf',width=7, height=5)


#Figure 9-----------------------------------------
ggplot(decisions, aes(x=number_of_decisions, y= n ,
                      fill = as.factor(split)))+
  geom_bar_pattern(stat='identity',
                   aes(pattern = as.factor(split)),
                   colour = "black",
                   pattern_fill = "white",
                   pattern_angle = 45,
                   pattern_density = 0.2,
                   pattern_spacing = 0.05,
                   position = position_dodge())+
  scale_pattern_manual(values = c("stripe", "none"),
                       guide = guide_legend(override.aes = list(fill = "white")) # <- make lighter
  ) +
  scale_fill_manual("legend", values = c("0" = "black", "1" = "darkgrey"))+
  scale_x_continuous(name="Number of circuits weighing in", breaks = seq(2,12,1)) +
  scale_y_continuous(name="Proportion", expand = expansion(mult = c(0, .05))) +
  theme_bw()+
  theme(legend.position="none",#suppress legend
        axis.text.x = element_text(size=18), axis.title.x = element_text(size=x.axis.label.size),
        axis.text.y = element_text(size=18), axis.title.y = element_text(size=y.axis.label.size)) +
  annotate("text", x = 4, y = 0.18, label = TeX("$\\leftarrow$Uniformity"), color = "black", cex=8) +
  annotate("text", x = 8.75, y = 0.17, label = TeX("$\\leftarrow$Splits"), color ="black",cex=8 )+
  annotate('text', x=12, y=.2, label='(A)', cex = 8)
ggsave('Plots/Figure_9A.pdf',width=7, height=5)


ggplot(second_period_splits, aes(x=number_of_decisions, y= n ,
                                 fill = as.factor(first_split)))+
  geom_bar_pattern(stat='identity',
                   aes(pattern = as.factor(first_split)),
                   colour = "black",
                   pattern_fill = "white",
                   pattern_angle = 45,
                   pattern_density = 0.5,
                   pattern_spacing = 0.05,
                   position = position_dodge())+
  scale_pattern_manual(values = c("none", "circle"),
                       guide = guide_legend(override.aes = list(fill = "white")) # <- make lighter
  ) +
  scale_fill_manual("legend", values = c("0" = "darkgrey", "2" = "black"))+
  scale_x_continuous(name="Number of circuits weighing in", breaks = seq(2,12,1)) +
  scale_y_continuous(name="Proportion", expand = expansion(mult = c(0, .05))) +
  theme_bw()+
  theme(legend.position="none",#suppress legend
        axis.text.x = element_text(size=18), axis.title.x = element_text(size=x.axis.label.size),
        axis.text.y = element_text(size=18), axis.title.y = element_text(size=y.axis.label.size)) +
  annotate("text", x = 4, y = 0.16, label = TeX("All Splits$\\rightarrow$"), color = "black", cex=8) +
  annotate("text", x = 9, y = 0.2, label = TeX("$\\leftarrow$Second"), color = "black", cex=8) +
  annotate("text", x = 9.75, y = 0.18, label = TeX("Period Splits"), color ="black",cex=8 )+
  annotate('text', x=2, y=.21, label='(B)', cex = 8)

ggsave('Plots/Figure_9B.pdf',width=7, height=5)


#Figure 10-------------------------
ggplot(hazard_rate, aes(x= Occurrence,y=hazard))+geom_bar(stat = 'identity',fill = 'grey',color='black' )+
  scale_x_continuous('Period', breaks = c(2,3,4,5,6,7,8,9,10,11,12))+
  scale_y_continuous(name="Hazard Rate")+
  theme_bw()+
  theme(legend.position="none",#suppress legend
        axis.text.x = element_text(size=18), axis.title.x = element_text(size=x.axis.label.size),
        axis.text.y = element_text(size=18), axis.title.y = element_text(size=y.axis.label.size),
        strip.background = element_blank(),
        strip.text.x = element_blank()) 
ggsave('Plots/Figure_10.pdf',width=7, height=5)


#Table 1-------------------------
m_naive1 <- lm_robust(disagreement ~ ideological_distance, dyads,
                      clusters=Issue)
m_naive2 <- lm_robust(disagreement ~ ideological_distance+Issue, dyads, 
                      clusters=Issue)

m_comb1 <- lm_robust(disagreement ~ ideological_distance*bias_compatible, dyads,
                     clusters=Issue)
m_comb2 <- lm_robust(disagreement ~ ideological_distance*bias_compatible+Issue,
                     clusters=Issue, data=dyads)  



t1=texreg(list(m_naive1,m_naive2,m_comb1, m_comb2), stars=c(0.05),
       omit.coef = c("Issue"),
       reorder.coef = c(2,3,4,1),
       custom.coef.names=c("Constant","Ideological Distance",
                           "Bias-compatible","Distance*Bias-compatible"),
       include.ci=FALSE, include.rmse=FALSE, include.rsquared=F)

print(t1, file = "Tables/t1.tex")
#-----------------------------------------
#Figure 11
pre <- predictions(m_comb2,
                   by=c('ideological_distance','bias_compatible'),
                   newdata = datagrid(Issue = unique, 
                                      ideological_distance = seq(min(dyads$ideological_distance), max(dyads$ideological_distance),
                                                                 length.out = 100),
                                      bias_compatible=c(0,1)))

ggplot(data = pre, aes(x= ideological_distance, y = estimate, ymin=conf.low,ymax=conf.high,group = bias_compatible))+
  geom_ribbon(aes(fill=as.factor(bias_compatible)),alpha = .2) +
  geom_line(aes(color=as.factor(bias_compatible)))+
  scale_fill_manual(values=c("#5D3A9B", "#FB9714"))+
  scale_color_manual(values=c("#5D3A9B", "#FB9714"))+
  scale_x_continuous(name="Ideological Distance")+
  scale_y_continuous(name="Probability of Circuit Split", limits = c(0,0.4), expand = expansion(mult = c(0, .05)))+
  theme_bw()+
  theme(legend.position="none",#suppress legend
        axis.text.x = element_text(size=18), axis.title.x = element_text(size=x.axis.label.size),
        axis.text.y = element_text(size=18), axis.title.y = element_text(size=y.axis.label.size),
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  annotate("text", x = 0.3, y = 0.1, label = "Bias-Incompatible", color = "#5D3A9B", cex=8) +
  annotate("text", x = 0.4, y = 0.27, label = "Bias-Compatible", color = "#FB9714", cex=8)+
  annotate("text", x = 1.1, y = .4, label = "(A)", color = "black", cex=10)
ggsave('Plots/Figure_11A.pdf',width=7, height=5)



plot_slopes(m_comb2,variables = 'bias_compatible',condition = c('ideological_distance'))+
  scale_x_continuous(name="Ideological Distance")+
  scale_y_continuous(name="Marginal Effect of Bias-Compatible", expand = expansion(mult = c(0, .05))) +
  theme_bw()+
  theme(legend.position="none",#suppress legend
        axis.text.x = element_text(size=18), axis.title.x = element_text(size=x.axis.label.size),
        axis.text.y = element_text(size=18), axis.title.y = element_text(size=y.axis.label.size),
        strip.background = element_blank(),
        strip.text.x = element_blank())+
  annotate("text", x = 1.1, y = 0.3, label = "(B)", color = "black", cex=10) 
ggsave('Plots/Figure_11B.pdf',width=7, height=5)

#Figure 12------------------------------
binomial_smooth <- function(...) {#for loess on binary DV
  
  geom_smooth(method = "glm", method.args = list(family = "binomial"), ...)
  
}

agree = dyads %>%
  group_by(Occurrence.y) %>%
  summarise(percent_agree = mean(1-disagreement))

ggplot(dyads, aes(x = Occurrence.y, y = 1-disagreement))+binomial_smooth(formula = y ~ splines::ns(x, 3), se=TRUE)+
  geom_hline(yintercept =mean(1-dyads$disagreement), color = "#FB9714", linetype = 'dashed', size = 1.5)+
  geom_point(data=agree, aes(x = Occurrence.y,y = percent_agree))+
  scale_x_continuous(name=TeX("Timing of Initial Decision"), breaks = seq(1,11,1)) +
  scale_y_continuous(name=TeX("Probability of Subsequent Agreement"), expand = expansion(mult = c(0.05, 0))) +
  theme_bw()+
  theme(legend.position="none",#suppress legend
        axis.text.x = element_text(size=18), axis.title.x = element_text(size=x.axis.label.size),
        axis.text.y = element_text(size=18), axis.title.y = element_text(size=y.axis.label.size))+
  annotate("text", x =8, y = 0.81, label = TeX("$\\uparrow$ Average Probability"), color = "#FB9714", cex=8)+ 
  annotate("text", x = 8.2, y = 0.79, label = TeX("of Agreement"), color = "#FB9714", cex=8) +
  annotate("text", x = 8.2, y = 1.01, label = TeX(""), color = "#FB9714", cex=8)
ggsave('Plots/Figure_12.pdf',width=7, height=5)


#Figure 13-------------------------------------
ggplot(all_dec_plot,aes(x = Occurrence,y = prop, fill=as.factor(minority)))+
  geom_bar_pattern(stat='identity',
                   aes(pattern = as.factor(minority)),
                   colour = "black",
                   pattern_fill = "white",
                   pattern_angle = 45,
                   pattern_density = 0.2,
                   pattern_spacing = 0.05,
                   position = position_dodge())+
  scale_pattern_manual(values = c("stripe", "none"),
                       guide = guide_legend(override.aes = list(fill = "white")) # <- make lighter
  ) +
  scale_fill_manual("legend", values = c("0" = "black", "1" = "darkgrey"))+
  scale_x_continuous(name="Period", breaks = seq(1,12,1)) +
  scale_y_continuous(name="Proportion", expand = expansion(mult = c(0, .05))) +
  theme_bw()+
  theme(legend.position="none",#suppress legend
        axis.text.x = element_text(size=18), axis.title.x = element_text(size=x.axis.label.size),
        axis.text.y = element_text(size=18), axis.title.y = element_text(size=y.axis.label.size))+
  annotate("text", x = 5.65, y = 0.135, label = TeX("$\\leftarrow$All cases"), color = "black", cex=8) +
  annotate("text", x = 4.75, y = 0.225, label = TeX("$\\leftarrow$When split occurred"), color ="black",cex=8 )
ggsave('Plots/Figure_13.pdf',width=7, height=5)

#Appendix C----------------------------------------- 

#Table C.1-------------------------------
mc1.1 <- lm_robust(1-disagreement ~ Occurrence.y,cluster = Issue,data=dyads)
mc1.2<- lm_robust(1-disagreement ~ Occurrence.y+Issue,cluster = Issue,data=dyads)


c.1= texreg(list(mc1.1,mc1.2), stars=c(0.05), omit.coef = c("Issue"),
       custom.coef.names=c("Constant","Period"), reorder.coef = c(2,1),
       include.ci=FALSE, include.rmse=FALSE, include.rsquared=F, digits = 3)
print(c.1, file = "Tables/c1.tex")

#Table C.2----------------------------------
mc2 <- lm_robust(min_dec~Occurrence,clusters=as.factor(Issue),data=number_minority)

c.2 = texreg(mc2,
       stars=c(0.05),
       reorder.coef = c(2,1),
       custom.coef.names=c("Constant", "Period"),
       include.ci=FALSE, include.rmse=FALSE,include.rsquared=F)
print(c.2, file = "Tables/c2.tex")

#Table C.3----------------------------------

m_current_c_fe <- lm_robust(disagreement ~ ideological_distance*bias_compatible+Issue +
                              Circuit.x, dyads, clusters=Issue)
m_past_c_fe <- lm_robust(disagreement ~ ideological_distance*bias_compatible+Issue 
                         +Circuit.y, dyads, clusters=Issue)

m_all_fe <- lm_robust(disagreement ~ideological_distance*bias_compatible+Issue +
                        Circuit.x+Circuit.y, dyads, clusters=Issue)
c.3 = texreg(list(m_comb2,m_current_c_fe,m_past_c_fe,m_all_fe), stars=c(0.05),
       omit.coef = c("Issue|Circuit.x|Circuit.y"),
       custom.coef.names=c("Constant","Ideological Distance","Bias-compatible","Distance*Compatible"),
       reorder.coef = c(2,3,4,1),
       include.ci=FALSE, include.rmse=FALSE, include.rsquared=F)
print(c.3, file = "Tables/c3.tex")
