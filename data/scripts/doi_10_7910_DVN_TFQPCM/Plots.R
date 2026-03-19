### This file presents the code used to produce all the plots in the paper 

rm(list=ls())

##### Monte Carlo simulation on Contagion data: 
ts.mcsim.summary = readRDS(file="../Data/ContagionSummary.rds")

ggplot(ts.mcsim.summary, aes(timeseries.length, mean)) +
  geom_boxplot(size=0.4, outlier.size=0.15) +
  ggtitle("Data with Contagion")+
  xlab("Length of Timeseries") +
  ylab("Contagion Signal") +
  ylim(0.25,0.45) +
  theme_light()

ggsave("../Plots/Main-Fig 2a.png")

##### Monte Carlo Simulation on Homophily only data: 
homophilyMCsum = readRDS(file="../Data/HomophilySummary.rds")

ggplot(homophilyMCsum, aes(timeseries.length, mean)) +
  geom_boxplot(size=0.4, outlier.size=0.15) +
  ggtitle("Data with Homophily")+
  xlab("Length of Timeseries") +
  ylab("Contagion Signal") +
  ylim(-0.3,0.3) +
  theme_light()

ggsave("../Plots/Main-Fig 2b.png")

##### Monte Carlo Simulation on Contagion + Homophily data:
ch.mcsim.summary = readRDS(file="../Data/C+HSummary.rds")

ggplot(ch.mcsim.summary, aes(timeseries.length, mean)) +
  geom_boxplot(size=0.4, outlier.size=0.15) +
  ggtitle("Data with Contagion and Homphily")+
  xlab("Length of Timeseries") +
  ylab("Contagion Signal") +
  ylim(0,0.5) +
  theme_light()

ggsave("../Plots/Main-Fig 2c.png")

### Power of tests plot, homophily vs. contagionvs vs. Contagionn+Homophily vs. Homophily+TimeShock
### Estimating 2-tail p-value    ### Homophily
lpval = 1-homophilyMCsum$pval
homophilyMCsum$ttpval = c()
for(i in 1:1200){
  homophilyMCsum$ttpval[i] = 2*min(lpval[i], homophilyMCsum$pval[i])
}

tl = unique(homophilyMCsum$timeseries.length)
hom0.05 = c()
hom0.1 = c()
for (t in 1:24){
  foo = (homophilyMCsum$ttpval[homophilyMCsum$timeseries.length == tl[t]] <= 0.05)
  foop = sum(foo)/50
  foo2 = (homophilyMCsum$ttpval[homophilyMCsum$timeseries.length == tl[t]] <= 0.1)
  foo2p = sum(foo2)/50
  hom0.05 = c(hom0.05, foop)
  hom0.1 = c(hom0.1, foo2p)
}

############################# Contagion
upval = 1 - ts.mcsim.summary$pval
ts.mcsim.summary$ttpval = c()
for(i in 1:1200){
  ts.mcsim.summary$ttpval[i] = 2 * min(upval[i], ts.mcsim.summary$pval[i])
}
con0.05 = c()
con0.1 = c()
for ( t in 1:24){
  con = (ts.mcsim.summary$ttpval[ts.mcsim.summary$timeseries.length == tl[t]] <= 0.05)
  conp = sum(con)/50
  con2 = (ts.mcsim.summary$ttpval[ts.mcsim.summary$timeseries.length == tl[t]] <= 0.1)
  con2p = sum(con2)/50
  con0.05 = c(con0.05, conp)
  con0.1 = c(con0.1, con2p)
}

############################# Contagion + Homophily
chval = 1 - ch.mcsim.summary$pval
ch.mcsim.summary$ttpval = c()
for(i in 1:1200){
  ch.mcsim.summary$ttpval[i] = 2 * min(chval[i], ch.mcsim.summary$pval[i])
}
ch0.05 = c()
ch0.1 = c()
for ( t in 1:24){
  ch = (ch.mcsim.summary$ttpval[ch.mcsim.summary$timeseries.length == tl[t]] <= 0.05)
  chp = sum(ch)/50
  ch2 = (ch.mcsim.summary$ttpval[ch.mcsim.summary$timeseries.length == tl[t]] <= 0.1)
  ch2p = sum(ch2)/50
  ch0.05 = c(ch0.05, chp)
  ch0.1 = c(ch0.1, ch2p)
}

############################# Homophily + Time Shock
timeshockMCsum = readRDS(file="../Data/H+TSummary.rds")
tsval = 1 - timeshockMCsum$pval
timeshockMCsum$ttpval = c()
for(i in 1:1200){
  timeshockMCsum$ttpval[i] = 2 * min(tsval[i], timeshockMCsum$pval[i])
}
tl = unique(timeshockMCsum$timeseries.length)
ts0.05 = c()
ts0.1 = c()
for(t in 1:24){
  ts = (timeshockMCsum$ttpval[timeshockMCsum$timeseries.length == tl[t]] <= 0.05)
  tsp = sum(ts)/50
  ts2 = (timeshockMCsum$ttpval[timeshockMCsum$timeseries.length == tl[t]] <= 0.1)
  ts2p = sum(ts2)/50
  ts0.05 = c(ts0.05, tsp)
  ts0.1 = c(ts0.1, ts2p)
}
###### Summarizing type 1 errors 
fullpval = bind_cols(as.numeric(levels(tl)), hom0.05, hom0.1, con0.05, con0.1, 
                     ch0.05, ch0.1, ts0.05, ts0.1)

names(fullpval) = c("timeseries.length", "hom05", "hom01",
                    "con05", "con01","ch05", "ch01", "ts05", "ts01")

ggplot(fullpval) +
  geom_path(aes(timeseries.length, hom05,linetype="0.05")) +
  geom_point(aes(timeseries.length, hom05,shape="Homophily"),size=2) +
  geom_path(aes(timeseries.length, hom01,linetype="0.1")) +
  geom_point(aes(timeseries.length, hom01,shape="Homophily"),size=2) +
  geom_path(aes(timeseries.length, con05,linetype="0.05")) +
  geom_point(aes(timeseries.length, con05,shape="Contagion"),size=2) +
  geom_path(aes(timeseries.length, con01,linetype="0.1")) +
  geom_point(aes(timeseries.length, con01,,shape="Contagion"),size=2) +
  geom_path(aes(timeseries.length, ch05,linetype="0.05")) +
  geom_point(aes(timeseries.length, ch05,shape="Contagion + Homophily"),size=2) +
  geom_path(aes(timeseries.length, ch01,linetype="0.1")) +
  geom_point(aes(timeseries.length, ch01,shape="Contagion + Homophily"),size=2) +
  geom_path(aes(timeseries.length, ts05,linetype="0.05")) +
  geom_point(aes(timeseries.length, ts05,shape="Time Shock"),size=2) +
  geom_path(aes(timeseries.length, ts01,linetype="0.1")) +
  geom_point(aes(timeseries.length, ts01,shape="Time Shock"),size=2) +
  scale_linetype_manual("Type 1 error",values=c("0.1"=2,"0.05"=1)) +
  scale_x_continuous(breaks = seq(50,4,by=-2)) +
  scale_x_reverse() +
  scale_shape_manual(name="Data type", values=c("Contagion"=16,"Homophily"=4,"Contagion + Homophily"=17, "Time Shock"=6)) +
  ggtitle("Proportion of tests with Type 1 error of 0.1 and 0.05")+
  xlab("Timeseries length") +
  ylab("Proportion of tests") +
  theme_light() 

ggsave("../Plots/Main-Fig 3.png")
