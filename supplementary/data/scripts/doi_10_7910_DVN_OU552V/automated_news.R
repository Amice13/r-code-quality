library(ggplot2)
require(grid)

se <- function(x, na.rm=F) { sd(x, na.rm)/sqrt(length(x)) }
cLabel <- c('unbiased (-biased)', 'fair', 'objective', 'exciting (-boring)', 'enjoyable', 'interesting', 'lively', 'pleasing', 'clear', 'coherent', 'comprehensive', 'concise', 'well-written')

#####abs
dfAbs <- read.csv('dfAbs.csv', na.strings='')
#reformat (and turn items)
dfBar <- with(dfAbs, rbind(
  data.frame(value=6-DV01_01, src=IV01_02, type='perception', item=cLabel[1], dimension='Credibility', topic=IV03_01),
  data.frame(value=DV01_02, src=IV01_02, type='perception', item=cLabel[2], dimension='Credibility', topic=IV03_01),
  data.frame(value=DV01_03, src=IV01_02, type='perception', item=cLabel[3], dimension='Credibility', topic=IV03_01),
  data.frame(value=6-DV01_04, src=IV01_02, type='perception', item=cLabel[4], dimension='Liking', topic=IV03_01),
  data.frame(value=DV01_05, src=IV01_02, type='perception', item=cLabel[5], dimension='Liking', topic=IV03_01),
  data.frame(value=DV01_06, src=IV01_02, type='perception', item=cLabel[6], dimension='Liking', topic=IV03_01),
  data.frame(value=DV01_07, src=IV01_02, type='perception', item=cLabel[7], dimension='Liking', topic=IV03_01),
  data.frame(value=DV01_08, src=IV01_02, type='perception', item=cLabel[8], dimension='Liking', topic=IV03_01),
  data.frame(value=DV01_09, src=IV01_02, type='perception', item=cLabel[9], dimension='Quality', topic=IV03_01),
  data.frame(value=DV01_10, src=IV01_02, type='perception', item=cLabel[10], dimension='Quality', topic=IV03_01),
  data.frame(value=DV01_11, src=IV01_02, type='perception', item=cLabel[11], dimension='Quality', topic=IV03_01),
  data.frame(value=DV01_12, src=IV01_02, type='perception', item=cLabel[12], dimension='Quality', topic=IV03_01),
  data.frame(value=DV01_13, src=IV01_02, type='perception', item=cLabel[13], dimension='Quality', topic=IV03_01),
  
  data.frame(value=6-CV20_01, src=IV01_02, type='expectation', item=cLabel[1], dimension='Credibility', topic=IV03_01),
  data.frame(value=CV20_02, src=IV01_02, type='expectation', item=cLabel[2], dimension='Credibility', topic=IV03_01),
  data.frame(value=CV20_03, src=IV01_02, type='expectation', item=cLabel[3], dimension='Credibility', topic=IV03_01),
  data.frame(value=6-CV20_04, src=IV01_02, type='expectation', item=cLabel[4], dimension='Liking', topic=IV03_01),
  data.frame(value=CV20_05, src=IV01_02, type='expectation', item=cLabel[5], dimension='Liking', topic=IV03_01),
  data.frame(value=CV20_06, src=IV01_02, type='expectation', item=cLabel[6], dimension='Liking', topic=IV03_01),
  data.frame(value=CV20_07, src=IV01_02, type='expectation', item=cLabel[7], dimension='Liking', topic=IV03_01),
  data.frame(value=CV20_08, src=IV01_02, type='expectation', item=cLabel[8], dimension='Liking', topic=IV03_01),
  data.frame(value=CV20_09, src=IV01_02, type='expectation', item=cLabel[9], dimension='Quality', topic=IV03_01),
  data.frame(value=CV20_10, src=IV01_02, type='expectation', item=cLabel[10], dimension='Quality', topic=IV03_01),
  data.frame(value=CV20_11, src=IV01_02, type='expectation', item=cLabel[11], dimension='Quality', topic=IV03_01),
  data.frame(value=CV20_12, src=IV01_02, type='expectation', item=cLabel[12], dimension='Quality', topic=IV03_01),
  data.frame(value=CV20_13, src=IV01_02, type='expectation', item=cLabel[13], dimension='Quality', topic=IV03_01)
))
levels(dfBar$item) <- cLabel



#####rel
dfRel <- read.csv('dfRel.csv', na.strings='')
#adjust perception/expectation scale from 1-7 scale to -3/+3 scale
moveRelScale <- function(df, by.type='expectation') {
  #use variable according to type
  sVar <- ifelse(by.type == 'expectation', 'CV21', 'DV02')
  #depending on right/left randomization (side of human-written text), rotate relative measures so that <0 always means human-written
  for(nRow in 1:nrow(df)) {
    for(i in 1:13) {
      if(!is.na(df[nRow, sprintf(paste0(sVar, '_%0.2d'), i)])) {
        df[nRow, sprintf(paste0(sVar, '_%0.2d'), i)] <- df[nRow, sprintf(paste0(sVar, '_%0.2d'), i)] - 4
        if((by.type == 'expectation' & df[nRow, 'IV04_03'] == 'R') | (by.type == 'perception' & df[nRow, 'IV04_02'] == 'J')) {
          df[nRow, sprintf(paste0(sVar, '_%0.2d'), i)] <- df[nRow, sprintf(paste0(sVar, '_%0.2d'), i)] * -1
        }
      }
    }
  }
  return(df)
}
#adjust 
dfRelPerception <- moveRelScale(dfRel, by.type='perception')
dfRelExpectation <- moveRelScale(dfRel, by.type='expectation')
#combine data
dfRelPerception$type <- 'perception'
dfRelPerception <- with(dfRelPerception, rbind(
  data.frame(value=DV02_01, type=type, item=cLabel[1], dimension='Credibility', topic=IV03_01),
  data.frame(value=DV02_02, type=type, item=cLabel[2], dimension='Credibility', topic=IV03_01),
  data.frame(value=DV02_03, type=type, item=cLabel[3], dimension='Credibility', topic=IV03_01),
  data.frame(value=DV02_04, type=type, item=cLabel[4], dimension='Liking', topic=IV03_01),
  data.frame(value=DV02_05, type=type, item=cLabel[5], dimension='Liking', topic=IV03_01),
  data.frame(value=DV02_06, type=type, item=cLabel[6], dimension='Liking', topic=IV03_01),
  data.frame(value=DV02_07, type=type, item=cLabel[7], dimension='Liking', topic=IV03_01),
  data.frame(value=DV02_08, type=type, item=cLabel[8], dimension='Liking', topic=IV03_01),
  data.frame(value=DV02_09, type=type, item=cLabel[9], dimension='Quality', topic=IV03_01),
  data.frame(value=DV02_10, type=type, item=cLabel[10], dimension='Quality', topic=IV03_01),
  data.frame(value=DV02_11, type=type, item=cLabel[11], dimension='Quality', topic=IV03_01),
  data.frame(value=DV02_12, type=type, item=cLabel[12], dimension='Quality', topic=IV03_01),
  data.frame(value=DV02_13, type=type, item=cLabel[13], dimension='Quality', topic=IV03_01)
))
#now add expectation
dfRelExpectation$type <- 'expectation'
dfRelExpectation <- with(dfRelExpectation, rbind(
  data.frame(value=CV21_01, type=type, item=cLabel[1], dimension='Credibility', topic=IV03_01),
  data.frame(value=CV21_02, type=type, item=cLabel[2], dimension='Credibility', topic=IV03_01),
  data.frame(value=CV21_03, type=type, item=cLabel[3], dimension='Credibility', topic=IV03_01),
  data.frame(value=CV21_04, type=type, item=cLabel[4], dimension='Liking', topic=IV03_01),
  data.frame(value=CV21_05, type=type, item=cLabel[5], dimension='Liking', topic=IV03_01),
  data.frame(value=CV21_06, type=type, item=cLabel[6], dimension='Liking', topic=IV03_01),
  data.frame(value=CV21_07, type=type, item=cLabel[7], dimension='Liking', topic=IV03_01),
  data.frame(value=CV21_08, type=type, item=cLabel[8], dimension='Liking', topic=IV03_01),
  data.frame(value=CV21_09, type=type, item=cLabel[9], dimension='Quality', topic=IV03_01),
  data.frame(value=CV21_10, type=type, item=cLabel[10], dimension='Quality', topic=IV03_01),
  data.frame(value=CV21_11, type=type, item=cLabel[11], dimension='Quality', topic=IV03_01),
  data.frame(value=CV21_12, type=type, item=cLabel[12], dimension='Quality', topic=IV03_01),
  data.frame(value=CV21_13, type=type, item=cLabel[13], dimension='Quality', topic=IV03_01)
))
dfRelCombination <- rbind(dfRelPerception, dfRelExpectation)
levels(dfRelCombination$type) <- c('Perception', 'Expectation')


#####descriptives
#combine
dfTotal <- rbind(
  data.frame(age=dfAbs$CV01, gender=dfAbs$CV02, time=dfAbs$TIME_SUM, type='abs'), 
  data.frame(age=dfRel$CV01, gender=dfRel$CV02, time=dfRel$TIME_SUM, type='rel')
)

#n
nrow(dfTotal)
#%
table(dfTotal$type)/nrow(dfTotal)
#gender (female = 1, male = 2)
table(dfTotal$gender)/nrow(dfTotal)
#age
mean(dfTotal$age, na.rm=T)
sd(dfTotal$age, na.rm=T)
#response time (overall and per experiment) [min]
mean(dfTotal$time, na.rm=T)/60
sd(dfTotal$time, na.rm=T)/60
mean(dfTotal[dfTotal$type == 'abs', 'time'], na.rm=T)/60
mean(dfTotal[dfTotal$type == 'rel', 'time'], na.rm=T)/60



######plot
#abs
png(filename='figure3.png', type='cairo', width=800, height=1000)
ggplot(dfBar, aes(item, value, color=src, shape=type)) +
  #png(filename='figure3_soccer.png', type='cairo', width=800, height=1000)
  #ggplot(dfBar[dfBar$topic == 'Soccer',], aes(item, value, color=src, shape=type)) +
  #png(filename='figure3_finance.png', type='cairo', width=800, height=1000)
  #ggplot(dfBar[dfBar$topic == 'Finance',], aes(item, value, color=src, shape=type)) +
  #png(filename='figure3_celebrity.png', type='cairo', width=800, height=1000)
  #ggplot(dfBar[dfBar$topic == 'Celebrity',], aes(item, value, color=src, shape=type)) +
  stat_summary(fun.y=mean, geom='point', position=position_dodge(width=.7), size=4) + 
  stat_summary(fun.data=function(x) {
    xM <- mean(x, na.rm=T)
    xSE <- se(x, na.rm=T)
    return(data.frame(
      y = xM,
      ymin = xM - (1.96*xSE),
      ymax = xM + (1.96*xSE)
    ))
  }, geom='errorbar', position=position_dodge(width=.7), width=.3) + 
  theme_bw(20, 'Arial') +
  labs(x='', y='Rating', fill='type') +
  theme(
    legend.position='bottom',
    legend.key=element_blank(),
    panel.grid.major.y=element_line(color='#EEEEEE', size=25),
    panel.grid.major.x=element_line(color='white')
  ) +
  scale_color_grey(name='Source', breaks=c('A', 'J'), labels=c('Algorithm', 'Journalist'), start=0, end=.6) +
  scale_shape_discrete(name='Type', breaks=c('expectation', 'perception'), labels=c('Expectation', 'Perception')) +
  guides(col = guide_legend(override.aes = list(shape = 15, size = 10))) +
  coord_flip(ylim=c(1, 5))
grid.text(label='Quality', x=unit(.01, 'npc'), y=unit(.95, 'npc'), just=c('left', 'center'), gp=gpar(col=rgb(.2,.2,.2)))
grid.text(label='Readability', x=unit(.01, 'npc'), y=unit(.627, 'npc'), just=c('left', 'center'), gp=gpar(col=rgb(.2,.2,.2)))
grid.text(label='Credibility', x=unit(.01, 'npc'), y=unit(.31, 'npc'), just=c('left', 'center'), gp=gpar(col=rgb(.2,.2,.2)))
dev.off()

#rel
png(filename='figure4.png', type='cairo', width=800, height=1000)
ggplot(dfRelCombination, aes(x=item, y=value, colour=type, linetype=type)) +
  #png(filename='figure4_soccer.png', type='cairo', width=800, height=1000)
  #ggplot(dfRelCombination[dfRelCombination$topic == 'Soccer',], aes(x=item, y=value, colour=type, linetype=type)) +
  #png(filename='figure4_finance.png', type='cairo', width=800, height=1000)
  #ggplot(dfRelCombination[dfRelCombination$topic == 'Finance',], aes(x=item, y=value, colour=type, linetype=type)) +
  #png(filename='figure4_celebrity.png', type='cairo', width=800, height=1000)
  #ggplot(dfRelCombination[dfRelCombination$topic == 'Celebrity',], aes(x=item, y=value, colour=type, linetype=type)) +
  stat_summary(fun.y=mean, geom='line', size=.8, aes(group=type)) +
  stat_summary(fun.y=mean, geom='point', size=3) + 
  stat_summary(fun.data=function(x) {
    xM <- mean(x, na.rm=T)
    xSE <- se(x, na.rm=T)
    return(data.frame(
      y = xM,
      ymin = xM - (1.96*xSE),
      ymax = xM + (1.96*xSE)
    ))
  }, geom='errorbar', width=.2, linetype='solid', show_guide=FALSE) + 
  scale_y_continuous(limits=c(-3, 3), breaks=c(-3, -2, -1, 1, 2, 3), minor_breaks=c(0)) +
  theme_bw(20, 'Arial') +
  labs(x='', y='in favor of journalist <  > in favor of algorithm', color='Type') +
  theme(
    panel.grid.minor=element_line(colour='#BBBBBB', size=0.5),
    legend.position='bottom',
    legend.key=element_blank(),
    legend.key.size=unit(3, 'cm')
  ) +
  scale_colour_grey(name='Type', start=0, end=.6) +
  scale_linetype_discrete(name='Type') +
  coord_flip()
grid.text(label='Quality', x=unit(.01, 'npc'), y=unit(.95, 'npc'), just=c('left', 'center'), gp=gpar(col=rgb(.2,.2,.2)))
grid.text(label='Readability', x=unit(.01, 'npc'), y=unit(.637, 'npc'), just=c('left', 'center'), gp=gpar(col=rgb(.2,.2,.2)))
grid.text(label='Credibility', x=unit(.01, 'npc'), y=unit(.325, 'npc'), just=c('left', 'center'), gp=gpar(col=rgb(.2,.2,.2)))
dev.off()

