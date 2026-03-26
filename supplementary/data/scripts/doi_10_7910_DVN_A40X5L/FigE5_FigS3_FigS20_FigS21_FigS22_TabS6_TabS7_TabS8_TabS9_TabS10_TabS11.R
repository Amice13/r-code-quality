
rm(list=ls())
gc()
require(dplyr)
require(ggplot2)
require(readr)
require(tibble)
require(tidyr)
require(data.table)
require(scoring)
require(scales)
require(spatstat)
library(gridExtra)
library(cowplot)
library(grid)


## Load in custom ggplot themes
source('theme_jake.R')
colors = c(Democratic = "#377EB8",Republican = "#E41A1C")



# Here we are going to assess the quality of our imputation based on survey results,
# where we know the self-reported (survey) partisanship/ideology and we know our imputation
# We will assess the imputation with and without survey weights, weighting it towards the
# full census population. 


# load data
survey = read_csv('partisan-survey-analysis.csv')

##### Figure E5

p1=ggplot(data=survey%>%filter(L2_recorded_pid=='i' & self=='Yes'),aes(x=L2_r.post,y=survey_rep))+
  geom_abline(aes(intercept=0, slope = 1), color = 'grey')+
  geom_histogram(aes(y=..count../sum(..count..)), fill = 'white', color = 'darkgrey')+
  stat_smooth( method = 'loess', color=colors[2])+
  stat_smooth(method = 'loess', color = 'black', aes(weight = weights.scaled))+
  geom_hline(yintercept = mean(survey$survey_rep[survey$L2_recorded_pid =='r'&survey$self=='Yes']=='1',na.rm=T), color = colors[2],linetype='dashed')+
  geom_hline(yintercept = 1-mean(survey$survey_rep[survey$L2_recorded_pid =='r'&survey$self=='Yes']=='1',na.rm=T), color = colors[2],linetype='dashed')+
  guides(size = F)+
  theme_minimal()+
  ylab('Self-reported Republican')+
  xlab('Republican Posterior Probability')+
  scale_y_continuous(breaks = seq(from = 0, to = 1, by = .25))+
  ylim(0,1)+
  theme(text=element_text(family='sans'), axis.title=element_text(size=12))



p2=ggplot(data=survey%>%filter(L2_recorded_pid=='i' & self=='Yes'),aes(x=L2_d.post,y=survey_dem))+
  geom_abline(aes(intercept=0, slope = 1), color = 'grey')+
  geom_histogram(aes(y=..count../sum(..count..)), fill = 'white', color = 'darkgrey')+
  stat_smooth( method = 'loess', color=colors[1])+
  stat_smooth(method = 'loess', color = 'black', aes(weight = weights.scaled))+
  geom_hline(yintercept = mean(survey$survey_dem[survey$L2_recorded_pid =='d'&survey$self=='Yes']=='1',na.rm=T), color = colors[1],linetype='dashed')+
  geom_hline(yintercept = 1-mean(survey$survey_dem[survey$L2_recorded_pid =='d'&survey$self=='Yes']=='1',na.rm=T), color = colors[1],linetype='dashed')+
  guides(size = F)+
  theme_minimal()+
  ylab('Self-reported Democrat')+
  xlab('Democratic Posterior Probability')+
  scale_y_continuous(breaks = seq(from = 0, to = 1, by = .25))+
  ylim(0,1)+
  theme(text=element_text(family='sans'), axis.title=element_text(size=12))

# Combine plots
g = plot_grid(p1,p2, ncol=1)

# Output Fig E5
ggsave(file = 'FigE5.jpeg', plot = g, width = 100, height = 150, units = 'mm')


#b.r = (brierscore(survey_rep ~ L2_r.post, survey%>%filter(L2_recorded_pid=='i' & self=='Yes')))
#b.d = (brierscore(survey_dem ~ L2_d.post, survey%>%filter(L2_recorded_pid=='i' & self=='Yes')))


## calculate Brier Scores for each unit

data = survey%>%filter(L2_recorded_pid=='i' & self=='Yes')

data = data %>%
  mutate(L2_i.post = 1-L2_d.post-L2_r.post,
    brier.r = if_else(party_recoded=='r', (1-L2_r.post)^2, (0-L2_r.post)^2 ),
         brier.d = if_else(party_recoded=='d', (1-L2_d.post)^2, (0-L2_d.post)^2 ),
         brier.i = if_else(party_recoded=='i', (1-L2_i.post)^2, (0-L2_i.post)^2 )
  )


# Output weighted mean for the Republican and Democratic Brier score, weighted by survey weights
weighted.mean(data$brier.r[!is.na(data$weights.scaled)],w=data$weights.scaled[!is.na(data$weights.scaled)])
weighted.mean(data$brier.d[!is.na(data$weights.scaled)],w=data$weights.scaled[!is.na(data$weights.scaled)])


### Figure S20

# Figure S20 in the Supplemental Information consists of 2 plots:
# S20a - Distribution of Brier Scores, Survey Weights
# S20b - Distribution of Brier Scores, No Survey Weights


# Format data  for ggplot for Figure S20a
l = data %>%
  filter(!is.na(weights.scaled))%>%
  select(brier.d, brier.r, weights.scaled)%>%
  pivot_longer(brier.d:brier.r)%>%
  mutate(Party = if_else(name=='brier.d', 'Democratic', 'Republican'))


vline.dat = l %>%
  group_by(Party)%>%
  mutate(v.mean = weighted.mean(value,w = weights.scaled),
         v.median = weighted.median(value, w = weights.scaled))



# Output Figure S20a
plot.name = 'FigS20a.pdf'
pdf(plot.name, width = 9, height = 5,pointsize=9 )

ggplot(l,aes(x=value,fill= Party,color = Party))+
  geom_histogram(aes(weight=weights.scaled),binwidth=.01,alpha = .5,position="identity") +
  scale_fill_manual(values=colors) +
  scale_color_manual(values=colors) +
  geom_vline(data = vline.dat,aes(xintercept =v.mean,color = Party)) +
  geom_vline(data = vline.dat,aes(xintercept =v.median,color = Party), linetype="dashed") +
  scale_x_continuous(breaks = seq(from = 0, to = 1, by = .5))+
  ylab('Count')+
  xlab('Brier Score')+
  facet_grid(Party~.)+
  theme_jake2()+
  theme(text=element_text(size=24))+
  xlab('Brier Score')

dev.off()
gc()

## Reformat data for ggplot
l = data %>%
  select(brier.d, brier.r, weights.scaled)%>%
  pivot_longer(brier.d:brier.r)%>%
  mutate(Party = if_else(name=='brier.d', 'Democratic', 'Republican'))





# Calculate means without survey weights to plot vertical lines over the historams
vline.dat = l %>%
  group_by(Party)%>%
  mutate(v.mean = mean(value),
         v.median = median(value))



## Output Figure S20b
plot.name = 'FigS20b.pdf'
pdf(plot.name, width = 9, height = 5,pointsize=9 )

ggplot(l,aes(x=value,fill= Party,color = Party))+
  geom_histogram(binwidth=.01,alpha = .5,position="identity") +
  scale_fill_manual(values=colors) +
  scale_color_manual(values=colors) +
  geom_vline(data = vline.dat,aes(xintercept =v.mean,color = Party)) +
  geom_vline(data = vline.dat,aes(xintercept =v.median,color = Party), linetype="dashed") +
  scale_x_continuous(breaks = seq(from = 0, to = 1, by = .25))+
  ylab('Count')+
  xlab('Brier Score')+
  facet_grid(Party~.)+
  theme_jake2()+
  theme(text=element_text(size=24))+
  xlab('Brier Score')

dev.off()
gc()


#
###### Figure S22 plots the same distributions by urban area and density, consisting of 4 total plots:

  # Figure S22a - Democratic Brier Score Distributions by Density, Survey Weights
  # Figure S22b - Democratic Brier Score Distributions by Density, No Survey Weights
  # Figure S22c - Republican Brier Score Distributions by Density, Survey Weights
  # Figure S22d - Republican Brier Score Distributions by Density, No Survey Weights


# We output Figures S22b and S22d first

# Format data for ggpot2
l = data %>%
  select(brier.d, brier.r, weights.scaled, L2_urban.area, L2_classification2)%>%
  filter(!is.na(L2_urban.area) & L2_urban.area!='' & !is.na(L2_classification2) & L2_classification2!='')%>%
  mutate(L2_classification2=factor(L2_classification2, levels = c('High density', 'Medium density', 'Low density', 'Very low density')))%>%
  pivot_longer(brier.d:brier.r)%>%
  mutate(Party = if_else(name=='brier.d', 'Democratic', 'Republican'))

# Calculate means and medians
vline.dat = l %>%
  group_by(Party, L2_urban.area, L2_classification2)%>%
  mutate(v.mean = mean(value),
         v.median = median(value))

# Create figure FigS22b
plot.name = 'FigS22b.pdf'


p=ggplot(l%>%filter(Party=='Democratic'),aes(x=value,fill= Party,color = Party))+
  geom_histogram(aes(y=..density..),binwidth=.01,alpha = .5,position="identity") +
  scale_fill_manual(values=colors) +
  scale_color_manual(values=colors) +
  geom_vline(data = vline.dat%>%filter(Party=='Democratic'),aes(xintercept =v.mean,color = Party)) +
  geom_vline(data = vline.dat%>%filter(Party=='Democratic'),aes(xintercept =v.median,color = Party), linetype="dashed") +
  scale_x_continuous(breaks = seq(from = 0, to = .75, by = .5))+
  ylab('Density')+
  xlab('Brier Score')+
  guides(color = F, fill=F)+
  facet_grid(rows = vars(L2_urban.area), cols=vars(L2_classification2))+
  theme_jake2()+
  theme(text=element_text(size=36))

## Format ggplot object
g = ggplotGrob(p)
gc()
rm_grobs = g$layout$name %in% c('panel-3-1')
g$grobs[rm_grobs] = NULL
g$layout = g$layout[!rm_grobs,]
g$layout[g$layout$name == 'axis-l-3', c('l', 'r')]=c('6', '6')
g$layout[g$layout$name == 'axis-b-1', c('t', 'b')]=c('11', '11')

# Output Fig S22b
pdf(plot.name, width = 18, height = 10,pointsize=9 )
grid.newpage()
grid.draw(g)
dev.off()

## Create Figure S22d
plot.name = 'FigS22d.pdf'

p=ggplot(l%>%filter(Party=='Republican'),aes(x=value,fill= Party,color = Party))+
  geom_histogram(aes(y=..density..),binwidth=.01,alpha = .5,position="identity") +
  scale_fill_manual(values=colors) +
  scale_color_manual(values=colors) +
  geom_vline(data = vline.dat%>%filter(Party=='Republican'),aes(xintercept =v.mean,color = Party)) +
  geom_vline(data = vline.dat%>%filter(Party=='Republican'),aes(xintercept =v.median,color = Party), linetype="dashed") +
  scale_x_continuous(breaks = seq(from = 0, to = .75, by = .5))+
  ylab('Density')+
  xlab('Brier Score')+
  guides(color = F, fill=F)+
  facet_grid(rows = vars(L2_urban.area), cols=vars(L2_classification2))+
  theme_jake2()+
  theme(text=element_text(size=36))

# Format ggplot object
g = ggplotGrob(p)
gc()
rm_grobs = g$layout$name %in% c('panel-3-1')
g$grobs[rm_grobs] = NULL
g$layout = g$layout[!rm_grobs,]
g$layout[g$layout$name == 'axis-l-3', c('l', 'r')]=c('6', '6')
g$layout[g$layout$name == 'axis-b-1', c('t', 'b')]=c('11', '11')

# Output Figure S22d
pdf(plot.name, width = 18, height = 10,pointsize=9 )
grid.newpage()
grid.draw(g)
dev.off()



# Next we output Figures S22a and S22c

# Format data for ggplot
l = data %>%
  filter(!is.na(weights.scaled))%>%
  select(brier.d, brier.r, weights.scaled, L2_urban.area, L2_classification2)%>%
  filter(!is.na(L2_urban.area) & L2_urban.area!='' & !is.na(L2_classification2) & L2_classification2!='')%>%
  mutate(L2_classification2=factor(L2_classification2, levels = c('High density', 'Medium density', 'Low density', 'Very low density')))%>%
  pivot_longer(brier.d:brier.r)%>%
  mutate(Party = if_else(name=='brier.d', 'Democratic', 'Republican'))

# Caculate weighted mean and medians
vline.dat = l %>%
  group_by(Party, L2_urban.area, L2_classification2)%>%
  mutate(v.mean = weighted.mean(value,w = weights.scaled),
         v.median = weighted.median(value, w = weights.scaled))

# Create Figure S22a

plot.name = 'FigS22a.pdf'

p=ggplot(l%>%filter(Party=='Democratic'),aes(x=value,fill= Party,color = Party))+
  geom_histogram(aes(weight=weights.scaled,y=..density..),binwidth=.01,alpha = .5,position="identity") +
  scale_fill_manual(values=colors) +
  scale_color_manual(values=colors) +
  geom_vline(data = vline.dat%>%filter(Party=='Democratic'),aes(xintercept =v.mean,color = Party)) +
  geom_vline(data = vline.dat%>%filter(Party=='Democratic'),aes(xintercept =v.median,color = Party), linetype="dashed") +
  scale_x_continuous(breaks = seq(from = 0, to = .75, by = .5))+
  ylab('Density')+
  xlab('Brier Score')+
  guides(color = F, fill=F)+
  facet_grid(rows = vars(L2_urban.area), cols=vars(L2_classification2))+
  theme_jake2()+
  theme(text=element_text(size=36))


# Format ggplot

g = ggplotGrob(p)
gc()
rm_grobs = g$layout$name %in% c('panel-3-1')
g$grobs[rm_grobs] = NULL
g$layout = g$layout[!rm_grobs,]
g$layout[g$layout$name == 'axis-l-3', c('l', 'r')]=c('6', '6')
g$layout[g$layout$name == 'axis-b-1', c('t', 'b')]=c('11', '11')

# Output Figure S22a
pdf(plot.name, width = 18, height = 10,pointsize=9 )
grid.newpage()
grid.draw(g)
dev.off()


# Create Figure S22c
plot.name = 'FigS22c.pdf'

p=ggplot(l%>%filter(Party=='Republican'),aes(x=value,fill= Party,color = Party))+
  geom_histogram(aes(weight=weights.scaled,y=..density..),binwidth=.01,alpha = .5,position="identity") +
  scale_fill_manual(values=colors) +
  scale_color_manual(values=colors) +
  geom_vline(data = vline.dat%>%filter(Party=='Republican'),aes(xintercept =v.mean,color = Party)) +
  geom_vline(data = vline.dat%>%filter(Party=='Republican'),aes(xintercept =v.median,color = Party), linetype="dashed") +
  scale_x_continuous(breaks = seq(from = 0, to = .75, by = .5))+
  ylab('Density')+
  xlab('Brier Score')+
  guides(color = F, fill=F)+
  facet_grid(rows = vars(L2_urban.area), cols=vars(L2_classification2))+
  theme_jake2()+
  theme(text=element_text(size=36))

# Fromat ggplot
g = ggplotGrob(p)
gc()
rm_grobs = g$layout$name %in% c('panel-3-1')
g$grobs[rm_grobs] = NULL
g$layout = g$layout[!rm_grobs,]
g$layout[g$layout$name == 'axis-l-3', c('l', 'r')]=c('6', '6')
g$layout[g$layout$name == 'axis-b-1', c('t', 'b')]=c('11', '11')

# Output Figure S22c
pdf(plot.name, width = 18, height = 10,pointsize=9 )
grid.newpage()
grid.draw(g)
dev.off()
#





##### Figure S21 plots the Democratic and Republican Brier scores by state, consisting of four plots:
  # Figure S21a - Democratic Brier Scores by State, Survey Weights
  # Figure S21b - Democratic Brier Scores by State, No Survey Weights
  # Figure S21c - Republican Brier Scores by State, No Survey Weights
  # Figure S21d - Republican Brier Scores by State, Survey Weights


# We output Fig S21b and S21d first 

### Format data for ggplot
l = data %>%
  select(brier.d, brier.r, weights.scaled, L2_res_state)%>%
  pivot_longer(brier.d:brier.r)%>%
  mutate(Party = if_else(name=='brier.d', 'Democratic', 'Republican'))

# Calculate median and mean
vline.dat = l %>%
  group_by(Party, L2_res_state)%>%
  mutate(v.mean = mean(value),
         v.median = median(value))

# Output FigS21b
plot.name = 'FigS21b.pdf'
pdf(plot.name, width = 11, height = 11,pointsize=9 )

ggplot(l%>%filter(Party=='Democratic'),aes(x=value,fill= Party,color = Party))+
  geom_histogram(aes(y=..density..),binwidth=.01,alpha = .5,position="identity") +
  scale_fill_manual(values=colors) +
  scale_color_manual(values=colors) +
  geom_vline(data = vline.dat%>%filter(Party=='Democratic'),aes(xintercept =v.mean,color = Party)) +
  geom_vline(data = vline.dat%>%filter(Party=='Democratic'),aes(xintercept =v.median,color = Party), linetype="dashed") +
  scale_x_continuous(breaks = seq(from = 0, to = .75, by = .5))+
  ylab('Density')+
  xlab('Brier Score')+
  guides(color = F, fill=F)+
  facet_wrap(L2_res_state~.)+
  theme_jake2()+
  theme(text=element_text(size=24))

dev.off()
gc()

# Output FigS21d
plot.name = 'FigS21d.pdf'
pdf(plot.name, width = 11, height = 11,pointsize=9 )

ggplot(l%>%filter(Party=='Republican'),aes(x=value,fill= Party,color = Party))+
  geom_histogram(aes(y=..density..),binwidth=.01,alpha = .5,position="identity") +
  scale_fill_manual(values=colors) +
  scale_color_manual(values=colors) +
  geom_vline(data = vline.dat%>%filter(Party=='Republican'),aes(xintercept =v.mean,color = Party)) +
  geom_vline(data = vline.dat%>%filter(Party=='Republican'),aes(xintercept =v.median,color = Party), linetype="dashed") +
  scale_x_continuous(breaks = seq(from = 0, to = .75, by = .5))+
  ylab('Density')+
  xlab('Brier Score')+
  guides(color = F, fill=F)+
  facet_wrap(L2_res_state~.)+
  theme_jake2()+
  theme(text=element_text(size=24))

dev.off()
gc()


#
# Next we output Fig S21a and S21c first 

# Format data for ggplot
l = data %>%
  filter(!is.na(weights.scaled))%>%
  select(brier.d, brier.r, weights.scaled, L2_res_state)%>%
  pivot_longer(brier.d:brier.r)%>%
  mutate(Party = if_else(name=='brier.d', 'Democratic', 'Republican'))

# Calculate weighted mean and median
vline.dat = l %>%
  group_by(Party, L2_res_state)%>%
  mutate(v.mean = weighted.mean(value,w = weights.scaled),
         v.median = weighted.median(value, w = weights.scaled))

# output Figure S21a
plot.name = 'FigS21a.pdf'
pdf(plot.name, width = 11, height = 11,pointsize=9 )

ggplot(l%>%filter(Party=='Democratic'),aes(x=value,fill= Party,color = Party))+
  geom_histogram(aes(weight=weights.scaled,y=..density..),binwidth=.01,alpha = .5,position="identity") +
  scale_fill_manual(values=colors) +
  scale_color_manual(values=colors) +
  geom_vline(data = vline.dat%>%filter(Party=='Democratic'),aes(xintercept =v.mean,color = Party)) +
  geom_vline(data = vline.dat%>%filter(Party=='Democratic'),aes(xintercept =v.median,color = Party), linetype="dashed") +
  scale_x_continuous(breaks = seq(from = 0, to = .75, by = .5))+
  ylab('Density')+
  xlab('Brier Score')+
  guides(color = F, fill=F)+
  facet_wrap(L2_res_state~.)+
  theme_jake2()+
  theme(text=element_text(size=24))

dev.off()
gc()


# output Figure S21c
plot.name = 'FigS21c.pdf'
pdf(plot.name, width = 11, height = 11,pointsize=9 )

ggplot(l%>%filter(Party=='Republican'),aes(x=value,fill= Party,color = Party))+
  geom_histogram(aes(weight=weights.scaled,y=..density..),binwidth=.01,alpha = .5,position="identity") +
  scale_fill_manual(values=colors) +
  scale_color_manual(values=colors) +
  geom_vline(data = vline.dat%>%filter(Party=='Republican'),aes(xintercept =v.mean,color = Party)) +
  geom_vline(data = vline.dat%>%filter(Party=='Republican'),aes(xintercept =v.median,color = Party), linetype="dashed") +
  scale_x_continuous(breaks = seq(from = 0, to = .75, by = .5))+
  ylab('Density')+
  xlab('Brier Score')+
  guides(color = F, fill=F)+
  facet_wrap(L2_res_state~.)+
  theme_jake2()+
  theme(text=element_text(size=24))

dev.off()
gc()
#




##### Tables S7-S11
rm(list=ls())
gc()
library(xtable)


load('survey-comps.Rdata')


print(xtable(cbind(race[,'Sample'],race[,-c('Sample')]*100), digits = 2), file = 'TabS7.tex')
print(xtable(cbind(gender[,'Sample'],gender[,-c('Sample')]*100), digits = 2), file = 'TabS8.tex')
print(xtable(cbind(density[,'Sample'],density[,-c('Sample')]*100), digits = 2), file = 'TabS9.tex')
print(xtable(cbind(urban[,'Sample'],urban[,-c('Sample')]*100), digits = 2), file = 'TabS10.tex')
print(xtable(cbind(vote2016[,'Sample'],vote2016[,-c('Sample')]*100), digits = 2), file = 'TabS11.tex')



# Figure S6 - Distribution of Survey Weights

# Remove objects
rm(list=ls())
gc()

# Load in data
survey = read_csv('partisan-survey-analysis.csv')




# Filter data to independents
data = survey%>%filter(L2_recorded_pid=='i' & self=='Yes')%>%
  select('weights.scaled')%>%
  mutate(Sample = 'Unaffiliated')

survey = survey%>%
  select('weights.scaled')%>%
  mutate(Sample = 'All')

# Recombine for plotting
l = bind_rows(data,survey)

# Load in custom ggplot themes
source('code/theme_jake.R')

# Output Figs3
plot.name = 'FigS3.pdf'
pdf(plot.name, width = 9, height = 5,pointsize=9 )

ggplot(l,aes(x=weights.scaled), color= 'black', fill='black')+
  geom_histogram(binwidth=.01,position="identity") +
  ylab('Count')+
  xlab('Survey Weights')+
  facet_grid(Sample~.)+
  theme_jake2()

dev.off()
gc()


#3 Output talbe 
survey = survey%>%filter( self=='Yes')
as_tibble(rbind(quantile(survey$weights.scaled,na.rm=T, probs = c(.01,.10,.25,.5,.75,.9,.99)),
quantile(data$weights.scaled,na.rm=T, probs = c( .01,.10,.25,.5,.75,.9,.99)))
) %>% mutate(Sample = c('All', 'Unaffiliated'))%>%
  select(Sample,1:7)%>%
  xtable(digits=2)%>%
  print(file = 'TabS6.tex')

