#import
dfAbs <- read.table(file.choose(), fileEncoding='UTF-8', sep=',', header=F, row.names='CASE',
                    col.names=c('CASE', 
                                'IV01_01', 'IV01_02', 'IV03_01', 
                                sprintf('DV01_%0.2d', 1:13), 
                                'CV01_01', 
                                'CV02', 'CV03', 'CV06', 'CV07', 'CV08', 'CV09', 'CV10', 'CV11', 'CV12', 'CV13', 
                                'TIME_SUM'))
dfAbs$CV02 = factor(dfAbs$CV02, levels=c("1","2"), labels=c("weiblich","männlich"), ordered=FALSE)
dfAbs$CV03 = factor(dfAbs$CV03, levels=c("1","2","3","4"), labels=c("FC Chelsea","FC Arsenal","Manchester United","Manchester City"), ordered=FALSE)
dfAbs$CV08 = factor(dfAbs$CV08, levels=c("1","2","3","4"), labels=c("Paris Saint-Germain","FC Bayern München","Real Madrid","FC Barcelona"), ordered=FALSE)
dfAbs$CV09 = factor(dfAbs$CV09, levels=c("1","2","3","4"), labels=c("FC Ingolstadt 04","SV Darmstadt 98","Karlsruher SC","1. FC Kaiserslautern"), ordered=FALSE)
dfAbs$CV06 = factor(dfAbs$CV06, levels=c("1","2","3","4"), labels=c("HeidelbergCement","Kabel Deutschland","MAN","HOCHTIEF"), ordered=FALSE)
dfAbs$CV10 = factor(dfAbs$CV10, levels=c("1","2","3","4"), labels=c("1,01 und 1,15 US Dollar","1,16 und 1,30 US Dollar","1,31 und 1,45 US Dollar","0,86 und 1,00 US Dollar"), ordered=FALSE)
dfAbs$CV11 = factor(dfAbs$CV11, levels=c("1","2","3","4"), labels=c("Martin Winterkorn","Ferdinand Piëch","Joe Kaeser","Wendelin Wiedeking"), ordered=FALSE)
dfAbs$CV07 = factor(dfAbs$CV07, levels=c("1","2","3","4"), labels=c("Angelina Jolie","Jennifer Aniston","Gwyneth Paltrow","Cindy Crawford"), ordered=FALSE)
dfAbs$CV12 = factor(dfAbs$CV12, levels=c("1","2","3","4"), labels=c("Brigitte","Bianca","Birgit","Beatrice"), ordered=FALSE)
dfAbs$CV13 = factor(dfAbs$CV13, levels=c("1","2","3","4"), labels=c("Eddie Redmayne","Michael Keaton","Benedict Cumberbatch","Bradley Cooper"), ordered=FALSE)


#descriptives

#n
nrow(dfAbs)

#gender (female = 1, male = 2)
table(dfAbs$CV02)/nrow(dfAbs)

#age
mean(dfAbs$CV01_01, na.rm=T)
sd(dfAbs$CV01_01, na.rm=T)

#response time [min]
mean(dfAbs$TIME_SUM, na.rm=T)/60
sd(dfAbs$TIME_SUM, na.rm=T)/60


#turn items around
dfAbs$DV01_01 <- 6-dfAbs$DV01_01
dfAbs$DV01_04 <- 6-dfAbs$DV01_04

#validation
require(psy)
cronbach(dfAbs[, sprintf('DV01_%0.2d', 1:3)])
cronbach(dfAbs[, sprintf('DV01_%0.2d', 4:8)])
cronbach(dfAbs[, sprintf('DV01_%0.2d', 9:13)])

#create indices (from theoretical deduction)
dfAbs$cred <- rowMeans(dfAbs[, sprintf('DV01_%0.2d', 1:3)], na.rm=T)
dfAbs$like <- rowMeans(dfAbs[, sprintf('DV01_%0.2d', 4:8)], na.rm=T)
dfAbs$qual <- rowMeans(dfAbs[, sprintf('DV01_%0.2d', 9:13)], na.rm=T)

dfAbs$nInvolvement <- NA
for(i in 1:nrow(dfAbs)) {
  if(dfAbs[i, 'IV03_01'] == 'Soccer') {
    dfAbs[i, 'nInvolvement'] <- sum(
      ifelse(as.numeric(dfAbs[i, 'CV03']) == 1, 1, 0), #premier league (chelsea)
      ifelse(as.numeric(dfAbs[i, 'CV08']) == 1, 1, 0), #Ibrahimović (paris st. germain)
      ifelse(as.numeric(dfAbs[i, 'CV09']) == 1, 1, 0),  #2. liga (ingolstadt)
      na.rm=T
    )
  } else if(dfAbs[i, 'IV03_01'] == 'Finance') {
    dfAbs[i, 'nInvolvement'] <- sum(
      ifelse(as.numeric(dfAbs[i, 'CV06']) == 1, 1, 0), #dax (heidelbergcement)
      ifelse(as.numeric(dfAbs[i, 'CV10']) == 1, 1, 0), #usd (1,01-1,15)
      ifelse(as.numeric(dfAbs[i, 'CV11']) == 1, 1, 0),  #vw vorstand (winterkorn)
      na.rm=T
    )
  } else if(dfAbs[i, 'IV03_01'] == 'Celebrity') {
    dfAbs[i, 'nInvolvement'] <- sum(
      ifelse(as.numeric(dfAbs[i, 'CV07']) == 1, 1, 0), #brad pitt (angelina jolie)
      ifelse(as.numeric(dfAbs[i, 'CV12']) == 1, 1, 0), #zeitschrift (brigitte)
      ifelse(as.numeric(dfAbs[i, 'CV13']) == 1, 1, 0),  #oscar (redmayne)
      na.rm=T
    )
  }
}

hist(dfAbs$nInvolvement)
table(dfAbs$nInvolvement, dfAbs$IV03_01)
sd(dfAbs[dfAbs$IV03_01 == 'Celebrity', 'nInvolvement'], na.rm=T)



#plot of moderated interaction
require(ggplot2)
require(Hmisc)
dfPlot <- dfAbs
dfPlot$eg <- as.factor(paste(dfPlot$IV01_01, dfPlot$IV01_02, sep='/'))
levels(dfPlot$eg) <- c('CG/A', 'CG/J', 'MV/A', 'MV/J')
dfPlot$inv <- as.factor(ifelse(dfPlot$nInvolvement <= 1, 0, 1))
dfPlotLong <- rbind(
    dfPlot,
    dfPlot,
    dfPlot
  )
dfPlotLong$perception <- NA
dfPlotLong$type <- NA
dfPlotLong[1:624, 'perception'] <- dfPlotLong[1:624, 'cred']
dfPlotLong[1:624, 'type'] <- 'Glaubwürdigkeit'
dfPlotLong[625:1248, 'perception'] <- dfPlotLong[625:1248, 'like']
dfPlotLong[625:1248, 'type'] <- 'Lesbarkeit'
dfPlotLong[1249:1872, 'perception'] <- dfPlotLong[1249:1872, 'qual']
dfPlotLong[1249:1872, 'type'] <- 'journ. Expertise'
dfPlotLong$type <- factor(dfPlotLong$type, levels=c('Glaubwürdigkeit', 'Lesbarkeit', 'journ. Expertise'))
ggplot(dfPlotLong, aes(eg, perception, group=inv, linetype=inv, shape=inv)) +
  stat_summary(fun.y=mean, geom='point', size=2, position=position_dodge(width=.15)) +
  stat_summary(fun.y=mean, geom='line', position=position_dodge(width=.15)) +
  stat_summary(fun.data=mean_cl_boot, geom='errorbar', width=.2, position=position_dodge(width=.15)) +
  ylim(min=1, max=5) +
  ylab('Wahrnehmung [1-5]') +
  xlab('Experimentalgruppe [tatsächliche Quelle/angegebener Autor]') +
  scale_shape_discrete(name='Involvement', labels=c('niedrig', 'hoch')) +
  scale_linetype_discrete(name='Involvement', labels=c('niedrig', 'hoch')) +
  facet_grid(type ~ .) +
  theme_bw() +
  theme(
    plot.title = element_text(face="bold", size=14),
    axis.title.x = element_text(face="bold", size=12),
    axis.title.y = element_text(face="bold", size=12, angle=90),
    panel.grid.minor = element_blank(),
    legend.title = element_text(face="bold", size=12),
    legend.position = 'top',
    legend.text = element_text(size=12),
    legend.key.size = unit(1.5, "lines")
  )
ggsave('involvement_all.png', width=20, height=15, units='cm')
  



#correlations
cor.test(dfPlot$nInvolvement, dfPlot$cred, use='pairwise.complete.obs')
cor.test(dfPlot$nInvolvement, dfPlot$like, use='pairwise.complete.obs')
cor.test(dfPlot$nInvolvement, dfPlot$qual, use='pairwise.complete.obs')


#multiple regression
require(lm.beta)
fit <- lm(cred ~ IV01_01 + IV01_02 + IV01_01*IV01_02, data=dfPlot)
summary(fit)
lm.beta(fit)
layout(matrix(c(1,2,3,4),2,2))
plot(fit)

anova(fit)

fit2 <- lm(cred ~ inv + IV01_01 + IV01_02 + IV01_01*IV01_02, data=dfPlot)
summary(fit2)
lm.beta(fit2)

anova(fit, fit2)
