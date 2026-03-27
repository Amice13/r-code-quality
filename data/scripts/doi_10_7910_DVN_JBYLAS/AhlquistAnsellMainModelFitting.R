## File gets and structures data
## generates figures,
## calls JAGS/WinBugs model objects,
## and, conducts post-fitting evaluation
## for Win/GeoBUGS models
## as described in the main text of
## "Taking Credit," World Politics
## John S. Ahlquist & Ben Ansell 

# nb: this is set to run on a Windows/WinBUGS with GeoBUGS
# which is required to fit a CAR prior.
library(lattice)
library(foreign)
library(MASS)
library(Hmisc)
library(arm)
library(coda)
library(R2WinBUGS)
library(mcmcplots)
library(ggplot2)

#settings
seeds<-345
set.seed(seeds)#for replication purposes
bugs.dir<-"c:/Program Files (x86)/WinBUGS14/"  #user to input BUGS location
work.dir<-"U:/AhlquistAnsell"  #user to set working directory
setwd(work.dir) 

#data
master.df<-read.csv("AhlquistAnsellWorldPoliticsReplicationData.csv", header=T)

#figure2
to.use<-master.df[!(is.na(master.df$gol.maj)),c("lag.top1pct", "BDK.pvt.credit.gdp", "cum_left_pct")]

leg.title<- "Cumulative \n left gov't"
c1 <-ggplot(to.use, aes(x=lag.top1pct, y=BDK.pvt.credit.gdp)) +  labs(x = "lag top 1% income share", 
  y = "private sector credit/GDP")
pdf("figure2.pdf") #figure 2 from main text
c1 + stat_smooth(method="loess",span = .9, na.rm=T) + geom_point(aes(alpha = cum_left_pct)) + theme_bw() +
  guides(alpha = guide_legend(leg.title))
dev.off()

#data assembly
load("SpatialWeights.Rdata")

cases.in<-c("AUS", "CAN", "CHE", "DEU", "DNK", "ESP", "FIN", "FRA", "GBR", "IRL", "ITA", "JPN",
            "NLD", "NOR", "NZL", "PRT", "SWE", "USA") #cases
years.in<-1980:2010 #time period

oecd.df<-subset(master.df, wb.code%in%cases.in)
oecd.df<-subset(oecd.df, years%in%years.in)
rm(master.df)
oecd.use<-subset(oecd.df,select=c("polity.num", "wb.code","years","singmemd_avg",
                           "effpar_leg_avg", "gov_left", "dis_gall_avg", 
                           "BDK.pvt.credit.gdp","lag.BDK.pvt.credit.gdp", "d.BDK.pvt.credit.gdp",
                           "lag.top1pct", "lag.top1pct.interp", "d.top1pct.interp", "gol.maj_avg",
                           "lag.pwt.POP", "d.pwt.log.POP","pwt.POP",
                           "lag.oecd.unemp", "d.oecd.unemp","oecd.unemp",
                           "lag.pwt.log.gdp", "d.pwt.log.gdp", "pwt.log.gdp",
                           "lag.pwt.pc.growth", "d.pwt.pc.growth","pwt.pc.growth",
                           "lag.pwt.ki", "d.pwt.ki", "pwt.ki",
                           "lag.curr.acct.gdp", "d.curr.acct.gdp", "curr.acct.gdp",
                           "lag.weo_gbds", "d.weo_gbds","weo_gbds",
                           "lag.world.abs.cab.gdp", "d.world.abs.cab.gdp","world.abs.cab.gdp", "eurozone",
                           "lag.broad.money.growth", "d.broad.money.growth", 
                           "cumul_left_yrs_wght", "lag.stir", "d.stir",
                           "lag.FIRE.empcosts.pct" , "d.FIRE.empcosts.pct",
                           "lag.hh.net.save.oecd", "d.hh.net.save.oecd", 
                           "lag.elderly.pop.oecd", "d.elderly.pop.oecd",
                           "lag.gcf.gdp.oecd","d.gcf.gdp.oecd", "cum_left", "cum_left_pct",
                           "cum_govparty", "avg_govparty"
                          )
                 )              

oecd.use$lag.pwt.POP<-log(oecd.use$lag.pwt.POP)
oecd.use$pwt.POP<-log(oecd.use$pwt.POP)
scaling.frame<-oecd.use[,5:ncol(oecd.use)]
scaling.frame<-data.frame(scale(scaling.frame)) #centering at 0 & scaling to SD=1 by column.
scaling.frame$gol.maj_avg<-oecd.use$gol.maj_avg
scaling.frame$eurozone<-as.numeric(oecd.use$eurozone)

colnames(scaling.frame)<-c("enpp", "left.gov","disprop", "credit",
                           "lag.credit", "d.credit",
                           "lag.ineq1", "lag.ineq", "d.ineq", "maj",
                           "lag.pop", "d.pop","pop",
                           "lag.unemp", "d.unemp", "unemp",
                           "lag.gdp", "d.gdp", "gdp",
                           "lag.growth", "d.growth", "growth",
                           "lag.k", "d.k", "ki",
                           "lag.cab", "d.cab","cab",
                           "lag.budg", "d.budg","budg",
                           "lag.world.save", "d.world.save","world.save","eurozone",
                           "lag.bm.growth", "d.bm.growth", "cum.left.old", "lag.stir", "d.stir",
                           "lag.FIRE", "d.FIRE", "lag.save", "d.save", "lag.old", "d.old",
                           "lag.gfcf", "d.gfcf", "cum.left.raw", "cum.left", "cum.govparty.raw", "cum.govparty" 
                           )
scaling.frame$wb.code<-oecd.use$wb.code
scaling.frame$years<-oecd.use$years
oecd.use<-scaling.frame
attach(oecd.use)

#getting data ready
n.obs<-dim(oecd.use)[1]
n.years<-length(years.in)
n.countries<-length(cases.in)
years.d<-years - (min(years)-1) #getting years in numerical order for BUGS/JAGS
country<-rep(1,n.obs) #getting countries in numerical order for BUGS/JAGS
for(i in 2:n.obs){
  if(wb.code[i] ==wb.code[i-1]){country[i]<-country[i-1]}
  if(wb.code[i] != wb.code[i-1]){country[i]<-country[i-1]+1}
}

##constructing adjeceny matrix info for GeoBUGS
##this code makes each country-year "adjacent" to geographic neighbors (as defined in SpatialWeights.R)
adj<-weights<-NULL
for(i in 1:n.countries){
  for(j in 1:n.countries){
    if(SW[i,j] == 1){
      adj<-c(adj, j)
    }
  }
}

weights<-rep(1,length(adj))
num<-NULL
for(i in 1:n.countries){
    num<-c(num, sum(SW[i,]))
  }

#making sure things look right
length(weights)
length(adj)
sum(num) #all three should be equal

### ECM

## main model
ECMmain.data<-list("country","n.obs", "n.countries",
                       "n.years",
                       "d.credit", "lag.credit", "lag.ineq","d.ineq", "cum.left",
                       "lag.pop", "d.pop",
                       "lag.unemp","d.unemp",
                       "d.gdp","lag.gdp",
                       "lag.gfcf","d.gfcf",
                       "lag.cab", "d.cab",
                       "lag.budg","d.budg",
                       "lag.world.save", "d.world.save",
                       "lag.bm.growth", "d.bm.growth",
                       "years.d", "eurozone",
                       "lag.old", "d.old"
                 )

ECMmain.params<-c("b.l.credit","b.l.top1", "b.d.top1",
                        "b.cl", "b.l.top1cl", "b.d.top1cl",
                        "b.l.unemp", "b.d.unemp",
                        "b.l.pop", "b.d.pop",
                        "b.l.gdp", "b.d.gdp",
                        "b.l.k", "b.d.k",
                        "b.l.cab", "b.d.cab",
                        "b.l.budg", "b.d.budg",
                        "b.l.world.save", "b.d.world.save",
                        "b.l.bm", "b.d.bm", "b.l.old", "b.d.old",
                        "a.unit", "g.unit","g.year","g.euro",
                        "logsigma2.y", "sigma.country", "sigma.vc", "sigma.vy",
                        "y.pred","res")

y.init<-rep(NA,n.obs)
y.init[is.na(d.credit)]<-0
a.unit.init<-rep(0, n.countries)
g.unit.init<-rep(0, n.countries)
g.year.init<-rep(0, n.years)
temp.inits<-rbeta(n.countries,1,1)
ECMmain.inits<-function(){list(
                                 "temp" = temp.inits,
                                 "b.l.top1" = rnorm(1, sd=2),
                                 "b.d.top1" = rnorm(1, sd=2),
                                 "b.cl"= rnorm(1, sd=2),
                                 "b.l.top1cl"= rnorm(1, sd=2),
                                 "b.d.top1cl"= rnorm(1, sd=2),
                                 "b.l.unemp"= rnorm(1, sd=2),
                                 "b.d.unemp"= rnorm(1, sd=2),
                                 "b.l.pop"= rnorm(1, sd=2),
                                 "b.d.pop"= rnorm(1, sd=2),
                                 "b.d.gdp"= rnorm(1, sd=2),
                                 "b.l.gdp"= rnorm(1, sd=2),
                                 "b.l.k"= rnorm(1, sd=2),
                                 "b.d.k"= rnorm(1, sd=2),
                                 "b.l.cab"= rnorm(1, sd=2),
                                 "b.d.cab"= rnorm(1, sd=2),
                                 "b.l.budg"= rnorm(1, sd=2),
                                 "b.d.budg"= rnorm(1, sd=2),
                                 "b.l.world.save"= rnorm(1, sd=2),
                                 "b.d.world.save"= rnorm(1, sd=2),
                                 "b.l.bm"= rnorm(1, sd=2),
                                 "b.d.bm"= rnorm(1, sd=2),
                                 "b.l.old"= rnorm(1, sd=2),
                                 "b.d.old"= rnorm(1, sd=2),
                                 "g.euro" = rnorm(1, sd=2),
                                 "sigma.country"=runif(1,0,50),
                                 "sigma.l.credit"=runif(1,0,50),
                                 "sigma.l.top1"=runif(1,0,50),
                                 "sigma.d.top1"=runif(1,0,50),
                                 "sigma.l.budg" = runif(1,0,50),
                                 "sigma.d.budg" = runif(1,0,50),
                                 "sigma.l.unemp" = runif(1,0,50),
                                 "sigma.d.unemp" = runif(1,0,50),
                                 "sigma.l.bm.growth"= runif(1,0,50),
                                 "sigma.d.bm.growth"= runif(1,0,50),
                                 "sigma.vc" = runif(1,0,5),
                                 "sigma.vy" = runif(1,0,5),
                                 "mu.c"=rnorm(1, sd=2),
                                 "mu.l.credit" =rnorm(1, sd=2),
                                 "mu.l.top1"=rnorm(1, sd=2),
                                 "mu.d.top1"=rnorm(1, sd=2),
                                 "mu.l.budg" = rnorm(1, sd=2),
                                 "mu.d.budg" = rnorm(1, sd=2),
                                 "mu.l.unemp" = rnorm(1, sd=2),
                                 "mu.d.unemp" = rnorm(1, sd=2),
                                 "mu.l.bm.growth"= rnorm(1, sd=2),
                                 "mu.d.bm.growth"= rnorm(1, sd=2),
                             "d.credit" = y.init, "a.unit" = a.unit.init,
                             "g.unit"= g.unit.init, "g.year" = g.year.init
                             )
                      }  
ECMmain.mod<-bugs(data = ECMmain.data, inits = ECMmain.inits,
              model.file = "AhlquistAnsellMainModel.bug",
              parameters = ECMmain.params,
              n.chains = 3, n.iter = 30000,
              n.burnin = 10000, n.thin = 30,
              bugs.directory=bugs.dir, bugs.seed=seeds,
              working.directory= work.dir, DIC=T, debug=F)
resids<-ECMmain.mod$median$res
R2.main<-1-(t(resids)%*%resids/sum((d.credit-mean(d.credit, na.rm=T))^2, na.rm=T)) #R^2 at median "residual"

m2.outlist<-as.mcmc.bugs(ECMmain.mod)
#convergence diagnotic plot, Appendix figure 1
pdf("AppendixFigure1.pdf")
mcmcplot1(m2.outlist[,"b.l.top1cl", drop=FALSE], style="plain") 
dev.off()
##

reg.pars<-c(ECMmain.params[2:8], ECMmain.params[13:24]) #leaving out GDP & pop b/c too big for plot 
var.pars<-c(ECMmain.params[30], ECMmain.params[28], 
              ECMmain.params[31], ECMmain.params[32])

mcmcplot(m2.outlist, parms=reg.pars)
parnames<-c("lag inequality", expression(paste(Delta, " inequality")),
"Left gov", "lag ineq. x Left", expression(paste(Delta, " inequality x Left")),
            "lag unemployment", expression(paste(Delta, " unemployment")), 
             "lag GFCF", expression(paste(Delta, " GFCF")), 
             "lag current acct", expression(paste(Delta, " current account")),
            "lag budget balance", expression(paste(Delta, " budget balace")),
             "lag world savings", expression(paste(Delta, " world savings")),
             "lag M3 growth", expression(paste(Delta, " M3 growth")),
             "lag Pop>64", expression(paste(Delta, " Pop>64"))
             )
varnames<-c(expression(sigma[a]), expression(xi), expression(sigma[gamma]), expression(sigma[eta]) )

pdf("figure4.pdf") #figure 4 from main text
  par(mfrow=c(1,2))
  caterplot(m2.outlist, style="plain", parms="b.l.credit",
            greek=F, labels = cases.in, cex.labels=.90, val.lim=c(-1.1,0.2))
  title(main = expression(paste(lambda[i], " with 68% & 95% BCI")))
  caterplot(m2.outlist, style="plain", parms="a.unit",
            greek=F, labels = cases.in, cex.labels=.90)#, add=T, col=gray(0.7))
title(main = expression(paste(alpha[i], " with 68% & 95% BCI")))
dev.off()

pdf("figure5.pdf") #figure 5 from main text
par(mar=c(2,7.5,4,.25))
caterplot(m2.outlist, style="plain", parms=reg.pars,
          greek=F, labels = parnames, cex.labels=.90, lwd=c(1.5,3), cex=3)
abline(v=0,lty=2)
title(main = "Posterior median with 68% & 95% BCI")
dev.off()

pdf("figure3.pdf")  #figure 3 from main text
caterplot(m2.outlist, style="plain", parms=var.pars,
          greek=F, labels = varnames, cex.labels=1.3, lwd=c(1.5,3), cex=3)
abline(v=0,lty=2)
title(main = "Posterior median with 68% & 95% BCI")
dev.off()

## interpreting interaction effects
## sample from the posterior
#using USA v Germany comparison
usa.cl<- oecd.use[oecd.use$wb.code=="USA" & oecd.use$year==2001,"cum.left"]
deu.cl<-oecd.use[oecd.use$wb.code=="DEU" & oecd.use$year==2001,"cum.left"]
ineq.lo<- oecd.use[oecd.use$wb.code=="USA" & oecd.use$year==1981,"lag.ineq"]
ineq.hi<- oecd.use[oecd.use$wb.code=="USA" & oecd.use$year==2001,"lag.ineq"]
ineq.seq<-seq(ineq.lo, ineq.hi, by = .1)

ineq.dif<-ineq.hi-ineq.lo

interp.mat<-ECMmain.mod$sims.matrix[,c("b.l.top1","b.l.top1cl")] #2001x2

usa.lr<-interp.mat%*%c(ineq.dif,ineq.dif*usa.cl)/(-ECMmain.mod$sims.matrix[,paste("b.l.credit[", which(cases.in=="USA"), "]", sep="")])
usa.lrinterp<-quantile(usa.lr, c(.025, .5, .975))
deu.lr<-interp.mat%*%c(ineq.dif,ineq.dif*deu.cl)/(-ECMmain.mod$sims.matrix[,paste("b.l.credit[", which(cases.in=="DEU"), "]", sep="")])
deu.lrinterp<-quantile(deu.lr, c(.025, .5, .975))

## long run effect.
plot.data<-data.frame(
                     ques=c("USA", "Germany"),
                     y=c(usa.lrinterp[2], deu.lrinterp[2]),
                     lower=c(usa.lrinterp[1], deu.lrinterp[1]),
                     upper=c(usa.lrinterp[3], deu.lrinterp[3])
                     )
lims <- aes(ymax = upper, ymin=lower)
mytitle<-"Predicted long-term effect of increase in inequality"


fig1<- ggplot(plot.data, aes(y=y, x=ques)) + geom_point(size=4) + geom_pointrange(lims, width=.2, size=1.25 ) +
  labs(x = NULL, title = mytitle, y = "long run change in credit/GDP (std)", color="") +
  theme_bw() + geom_hline(yintercept=0,linetype="dashed") +theme(legend.position="none", axis.text=element_text(size=14))

dat.1<-subset(oecd.use, years==2001, select = c(wb.code, cum.left)) #ncX2
ecf.fac<-ECMmain.mod$sims.matrix[,grep("b.l.credit", colnames(ECMmain.mod$sims.matrix))] #2001Xnc

cov.mat<-cbind(ineq.dif,ineq.dif*dat.1$cum.left) #ncX2
lre.mat<-(interp.mat%*%t(cov.mat))/-ecf.fac
out.mat<-apply(lre.mat,2, quantile, c(0.025, 0.5, 0.975))
colnames(out.mat)<-cases.in

sig<- as.numeric(0<out.mat[1,] | 0 >out.mat[3,])
plot.data<-data.frame(
                     ques=dat.1$cum.left,
                     y=out.mat[2,],
                     lower=out.mat[1,],
                     upper=out.mat[3,],
                     names=cases.in,
                     sig = sig
                     )
lims <- aes(ymax = upper, ymin=lower)
fig2<- ggplot(plot.data, aes(y=y, x=ques)) + geom_text(size=4, hjust = -.3, vjust=0, aes(label=names)) + geom_point(size=3, aes(color=factor(sig), shape = factor(sig))) + labs(x = "2001 cumulative Left gov't (std)", title = NULL, 
  y = "long run change in credit/GDP (std)", color="") + geom_hline(yintercept=0,linetype="dashed") + theme_bw() + 
  theme(legend.position="none", axis.text=element_text(size=14)) + coord_cartesian(xlim = c(-1.5, 3)) 

pdf("figure6.pdf") #figure 6
grid.arrange(fig1, fig2)
dev.off()

## distributed effect
t0.mat<-ECMmain.mod$sims.matrix[,c("b.d.top1","b.d.top1cl")]
t0.m<-t0.mat%*%c(ineq.dif, ineq.dif*usa.cl)
t0.pct<-t0.m/usa.lr
t1.m<-(-ECMmain.mod$sims.matrix[,paste("b.l.credit[", which(cases.in=="USA"), "]", sep="")])*(usa.lr-t0.m)
t2.m<-(-ECMmain.mod$sims.matrix[,paste("b.l.credit[", which(cases.in=="USA"), "]", sep="")])*(usa.lr-t0.m-t1.m)
t3.m<-(-ECMmain.mod$sims.matrix[,paste("b.l.credit[", which(cases.in=="USA"), "]", sep="")])*(usa.lr-t0.m-t1.m-t2.m)
t4.m<-(-ECMmain.mod$sims.matrix[,paste("b.l.credit[", which(cases.in=="USA"), "]", sep="")])*(usa.lr-t0.m-t1.m-t2.m-t3.m)
t5.m<-(-ECMmain.mod$sims.matrix[,paste("b.l.credit[", which(cases.in=="USA"), "]", sep="")])*(usa.lr-t0.m-t1.m-t2.m-t3.m-t4.m)
t6.m<-(-ECMmain.mod$sims.matrix[,paste("b.l.credit[", which(cases.in=="USA"), "]", sep="")])*(usa.lr-t0.m-t1.m-t2.m-t3.m-t4.m-t5.m)
t7.m<-(-ECMmain.mod$sims.matrix[,paste("b.l.credit[", which(cases.in=="USA"), "]", sep="")])*(usa.lr-t0.m-t1.m-t2.m-t3.m-t4.m-t5.m-t6.m)

t0.d<-t0.mat%*%c(ineq.dif, ineq.dif*deu.cl)
t1.d<-(-ECMmain.mod$sims.matrix[,paste("b.l.credit[", which(cases.in=="DEU"), "]", sep="")])*(deu.lr-t0.d)
t2.d<-(-ECMmain.mod$sims.matrix[,paste("b.l.credit[", which(cases.in=="DEU"), "]", sep="")])*(deu.lr-t0.d-t1.d)
t3.d<-(-ECMmain.mod$sims.matrix[,paste("b.l.credit[", which(cases.in=="DEU"), "]", sep="")])*(deu.lr-t0.d-t1.d-t2.d)
t4.d<-(-ECMmain.mod$sims.matrix[,paste("b.l.credit[", which(cases.in=="DEU"), "]", sep="")])*(deu.lr-t0.d-t1.d-t2.d-t3.d)
t5.d<-(-ECMmain.mod$sims.matrix[,paste("b.l.credit[", which(cases.in=="DEU"), "]", sep="")])*(deu.lr-t0.d-t1.d-t2.d-t3.d-t4.d)
t6.d<-(-ECMmain.mod$sims.matrix[,paste("b.l.credit[", which(cases.in=="DEU"), "]", sep="")])*(deu.lr-t0.d-t1.d-t2.d-t3.d-t4.d-t5.d)
t7.d<-(-ECMmain.mod$sims.matrix[,paste("b.l.credit[", which(cases.in=="DEU"), "]", sep="")])*(deu.lr-t0.d-t1.d-t2.d-t3.d-t4.d-t5.d-t6.d)


X<-0:7
Y<-c(quantile(t0.m,.5), quantile(t1.m,.5), quantile(t2.m,.5), quantile(t3.m,.5),
  quantile(t4.m,.5), quantile(t5.m,.5), quantile(t6.m,.5), quantile(t7.m,.5) )

lY<-c(quantile(t0.m,.025), quantile(t1.m,.025), quantile(t2.m,.025), quantile(t3.m,.025),
  quantile(t4.m,.025), quantile(t5.m,.025), quantile(t6.m,.025), quantile(t7.m,.025) )

hY<-c(quantile(t0.m,.975), quantile(t1.m,.975), quantile(t2.m,.975), quantile(t3.m,.975),
  quantile(t4.m,.975), quantile(t5.m,.975), quantile(t6.m,.975), quantile(t7.m,.975) )

Y2<-c(quantile(t0.d,.5), quantile(t1.d,.5), quantile(t2.d,.5), quantile(t3.d,.5),
  quantile(t4.d,.5), quantile(t5.d,.5), quantile(t6.d,.5), quantile(t7.d,.5) )

lY2<-c(quantile(t0.d,.025), quantile(t1.d,.025), quantile(t2.d,.025), quantile(t3.d,.025),
  quantile(t4.d,.025), quantile(t5.d,.025), quantile(t6.d,.025), quantile(t7.d,.025) )

hY2<-c(quantile(t0.d,.975), quantile(t1.d,.975), quantile(t2.d,.975), quantile(t3.d,.975),
  quantile(t4.d,.975), quantile(t5.d,.975), quantile(t6.d,.975), quantile(t7.d,.975) )


mytitle<-"Predicted lag distribution: effect of increasing inequality"
pdf("figure7.pdf")
ggplot() +
  geom_ribbon(aes(x= X, ymin=lY, ymax=hY, fill =2, alpha=.1)) +
  geom_line(aes(x= X, y= Y), size=1.3)+
  geom_ribbon(aes(x= X, ymin=lY2, ymax=hY2, fill=2), alpha=.1) +
  geom_line(aes(x= X,y= Y2,color=2), linetype = "longdash",size=1.3)+
  expand_limits(y=c(-.1,1.3)) +
  labs(x= "time from shock to inequality", y= 'change in credit/GDP (std)',
       title = mytitle) +
  geom_text(aes(label = "USA", x= 2.2, y = .6, size=.6)) +
  geom_text(aes(label = "Germany", x=2.2 , y = -.25, size=.6)) +
  theme_bw() + geom_hline(yintercept=0,linetype="dotted", size=.7) +theme(legend.position="none") + coord_cartesian(xlim = c(0, 4)) 
dev.off()


###Alternative models as reported in Table 1.
#using CAR prior--distance
ECM3.data<-list("country","n.obs", "n.countries",
                       "n.years", "num", "weights", "adj",
                       "d.credit", "lag.credit", "lag.ineq","d.ineq", "cum.left",
                       "lag.pop", "d.pop",
                       "lag.unemp","d.unemp",
                       "d.gdp","lag.gdp",
                       "lag.gfcf","d.gfcf",
                       "lag.cab", "d.cab",
                       "lag.budg","d.budg",
                       "lag.world.save", "d.world.save",
                       "lag.bm.growth", "d.bm.growth",
                       "years.d", "eurozone",
                       "lag.old", "d.old"
                 )

ECM3.params<-c("b0", "b.l.credit","b.l.top1", "b.d.top1",
                      "b.cl", "b.l.top1cl", "b.d.top1cl",
                      "b.l.unemp", "b.d.unemp",
                      "b.l.pop", "b.d.pop",
                      "b.l.gdp", "b.d.gdp",
                      "b.l.k", "b.d.k",
                      "b.l.cab", "b.d.cab",
                      "b.l.budg", "b.d.budg",
                      "b.l.world.save", "b.d.world.save",
                      "b.l.bm", "b.d.bm","b.l.old","b.d.old",
                      "a.unit", "g.unit","g.year","g.euro",
                      "logsigma2.y", "sigma.country", "sigma.vc", "sigma.vy",
                      "y.pred","res")

y.init<-rep(NA,n.obs)
y.init[is.na(d.credit)]<-0
a.unit.init<-rep(0, n.countries)
a.unit.init[which(num==0)]<-NA
g.unit.init<-rep(0, n.countries)
g.year.init<-rep(0, n.years)
temp.inits<-rbeta(n.countries,1,1)
ECM3.inits<-function(){list("b0" = rnorm(1, sd=2),
                                 "temp" = temp.inits,
                                 "b.l.top1" = rnorm(1, sd=2),
                                 "b.d.top1" = rnorm(1, sd=2),
                                 "b.cl"= rnorm(1, sd=2),
                                 "b.l.top1cl"= rnorm(1, sd=2),
                                 "b.d.top1cl"= rnorm(1, sd=2),
                                 "b.l.unemp"= rnorm(1, sd=2),
                                 "b.d.unemp"= rnorm(1, sd=2),
                                 "b.l.pop"= rnorm(1, sd=2),
                                 "b.d.pop"= rnorm(1, sd=2),
                                 "b.d.gdp"= rnorm(1, sd=2),
                                 "b.l.gdp"= rnorm(1, sd=2),
                                 "b.l.k"= rnorm(1, sd=2),
                                 "b.d.k"= rnorm(1, sd=2),
                                 "b.l.cab"= rnorm(1, sd=2),
                                 "b.d.cab"= rnorm(1, sd=2),
                                 "b.l.budg"= rnorm(1, sd=2),
                                 "b.d.budg"= rnorm(1, sd=2),
                                 "b.l.world.save"= rnorm(1, sd=2),
                                 "b.d.world.save"= rnorm(1, sd=2),
                                 "b.l.old" = rnorm(1, sd=2),
                                 "b.d.old" = rnorm(1, sd=2),
                                 "b.l.bm"= rnorm(1, sd=2),
                                 "b.d.bm"= rnorm(1, sd=2),
                                 "g.euro" = rnorm(1, sd=2),
                                 "tau.country"=rgamma(1,0.5, 0.0005),
                                 "sigma.l.credit"=runif(1,0,50),
                                 "sigma.l.top1"=runif(1,0,50),
                                 "sigma.d.top1"=runif(1,0,50),
                                 "sigma.l.budg" = runif(1,0,50),
                                 "sigma.d.budg" = runif(1,0,50),
                                 "sigma.l.unemp" = runif(1,0,50),
                                 "sigma.d.unemp" = runif(1,0,50),
                                 "sigma.l.bm.growth"= runif(1,0,50),
                                 "sigma.d.bm.growth"= runif(1,0,50),
                                 "sigma.vc" = runif(1,0,5),
                                 "sigma.vy" = runif(1,0,5),
                                 "mu.l.credit" =rnorm(1, sd=2),
                                 "mu.l.top1"=rnorm(1, sd=2),
                                 "mu.d.top1"=rnorm(1, sd=2),
                                 "mu.l.budg" = rnorm(1, sd=2),
                                 "mu.d.budg" = rnorm(1, sd=2),
                                 "mu.l.unemp" = rnorm(1, sd=2),
                                 "mu.d.unemp" = rnorm(1, sd=2),
                                 "mu.l.bm.growth"= rnorm(1, sd=2),
                                 "mu.d.bm.growth"= rnorm(1, sd=2),
                             "d.credit" = y.init, "a.unit" = a.unit.init,
                             "g.unit"= g.unit.init, "g.year" = g.year.init
                             )
                      }  
ECM3.mod<-bugs(data = ECM3.data, inits = ECM3.inits,
              model.file = "AhlquistAnsellCARmodel.bug",
              parameters = ECM3.params,
              n.chains = 3, n.iter = 30000,
              n.burnin = 10000, n.thin = 30,
              bugs.directory= bugs.dir, bugs.seed=seeds,
              working.directory= work.dir, DIC=T, debug=F) 
#DIC 14235
resids<-ECM3.mod$median$res
R2.ECM3<-1-(t(resids)%*%resids/sum((d.credit-mean(d.credit, na.rm=T))^2, na.rm=T)) #0.46

## different CAR prior:  common language
adj<-weights<-NULL
for(i in 1:n.countries){
  for(j in 1:n.countries){
    if(SW.l[i,j] == 1){
      adj<-c(adj, j)
    }
  }
}

weights<-rep(1,length(adj))
num<-NULL
for(i in 1:n.countries){
    num<-c(num, sum(SW.l[i,]))
  }

#making sure things look right
length(weights)
length(adj)
sum(num) #all three should be equal

ECM10.mod<-bugs(data = ECM3.data, inits = ECM3.inits,
              model.file = "AhlquistAnsellCARmodel.bug",
              parameters = ECM3.params,
              n.chains = 3, n.iter = 30000,
              n.burnin = 10000, n.thin = 30,
              bugs.directory= bugs.dir,  bugs.seed=seeds,
              working.directory= work.dir, DIC=T, debug=F)
resids<-ECM10.mod$median$res
R2.ECM10<-1-(t(resids)%*%resids/sum((d.credit-mean(d.credit, na.rm=T))^2, na.rm=T)) #0.46
ECM10.mod$DIC

  #w/o interaction
  ECM5.data<-list("country","n.obs", "n.countries",
                         "n.years",
                         "d.credit", "lag.credit", "lag.ineq","d.ineq", "cum.left",
                         "lag.pop", "d.pop",
                         "lag.unemp","d.unemp",
                         "d.gdp","lag.gdp",
                         "lag.gfcf","d.gfcf",
                         "lag.cab", "d.cab",
                         "lag.budg","d.budg",
                         "lag.world.save", "d.world.save",
                         "lag.bm.growth", "d.bm.growth",
                         "years.d", "eurozone",
                         "lag.old", "d.old"
                   )

  ECM5.params<-c("b.l.credit","b.l.top1", "b.d.top1",
                        "b.cl",
                        "b.l.unemp", "b.d.unemp",
                        "b.l.pop", "b.d.pop",
                        "b.l.gdp", "b.d.gdp",
                        "b.l.k", "b.d.k",
                        "b.l.cab", "b.d.cab",
                        "b.l.budg", "b.d.budg",
                        "b.l.world.save", "b.d.world.save",
                        "b.l.bm", "b.d.bm", "b.l.old", "b.d.old",
                        "a.unit", "g.unit","g.year","g.euro",
                        "logsigma2.y", "sigma.country", "sigma.vc", "sigma.vy",
                        "y.pred","res")

  y.init<-rep(NA,n.obs)
  y.init[is.na(d.credit)]<-0
  a.unit.init<-rep(0, n.countries)
  g.unit.init<-rep(0, n.countries)
  g.year.init<-rep(0, n.years)
  temp.inits<-rbeta(n.countries,1,1)
  ECM5.inits<-function(){list(
                                   "temp" = temp.inits,
                                   "b.l.top1" = rnorm(1, sd=2),
                                   "b.d.top1" = rnorm(1, sd=2),
                                   "b.cl"= rnorm(1, sd=2),
                                   "b.l.unemp"= rnorm(1, sd=2),
                                   "b.d.unemp"= rnorm(1, sd=2),
                                   "b.l.pop"= rnorm(1, sd=2),
                                   "b.d.pop"= rnorm(1, sd=2),
                                   "b.d.gdp"= rnorm(1, sd=2),
                                   "b.l.gdp"= rnorm(1, sd=2),
                                   "b.l.k"= rnorm(1, sd=2),
                                   "b.d.k"= rnorm(1, sd=2),
                                   "b.l.cab"= rnorm(1, sd=2),
                                   "b.d.cab"= rnorm(1, sd=2),
                                   "b.l.budg"= rnorm(1, sd=2),
                                   "b.d.budg"= rnorm(1, sd=2),
                                   "b.l.world.save"= rnorm(1, sd=2),
                                   "b.d.world.save"= rnorm(1, sd=2),
                                   "b.l.bm"= rnorm(1, sd=2),
                                   "b.d.bm"= rnorm(1, sd=2),
                                   "b.l.old"= rnorm(1, sd=2),
                                   "b.d.old"= rnorm(1, sd=2),
                                   "g.euro" = rnorm(1, sd=2),
                                   "sigma.country"=runif(1,0,50),
                                   "sigma.l.credit"=runif(1,0,50),
                                   "sigma.l.top1"=runif(1,0,50),
                                   "sigma.d.top1"=runif(1,0,50),
                                   "sigma.l.budg" = runif(1,0,50),
                                   "sigma.d.budg" = runif(1,0,50),
                                   "sigma.l.unemp" = runif(1,0,50),
                                   "sigma.d.unemp" = runif(1,0,50),
                                   "sigma.l.bm.growth"= runif(1,0,50),
                                   "sigma.d.bm.growth"= runif(1,0,50),
                                   "sigma.vc" = runif(1,0,5),
                                   "sigma.vy" = runif(1,0,5),
                                   "mu.c"=rnorm(1, sd=2),
                                   "mu.l.credit" =rnorm(1, sd=2),
                                   "mu.l.top1"=rnorm(1, sd=2),
                                   "mu.d.top1"=rnorm(1, sd=2),
                                   "mu.l.budg" = rnorm(1, sd=2),
                                   "mu.d.budg" = rnorm(1, sd=2),
                                   "mu.l.unemp" = rnorm(1, sd=2),
                                   "mu.d.unemp" = rnorm(1, sd=2),
                                   "mu.l.bm.growth"= rnorm(1, sd=2),
                                   "mu.d.bm.growth"= rnorm(1, sd=2),
                               "d.credit" = y.init, "a.unit" = a.unit.init,
                               "g.unit"= g.unit.init, "g.year" = g.year.init
                               )
                        }  

  ECM5.mod<-bugs(data = ECM5.data, inits = ECM5.inits,
                model.file = "AhlquistAnsellNoInteractionModel.bug",
                parameters = ECM5.params,
                n.chains = 3, n.iter = 30000,
                n.burnin = 10000, n.thin = 30,
                bugs.directory=bugs.dir, bugs.seed=seeds,
                working.directory= work.dir, DIC=T, debug=F)
resids<-ECM5.mod$median$res
R2.ECM5<-1-(t(resids)%*%resids/sum((d.credit-mean(d.credit, na.rm=T))^2, na.rm=T)) #0.45/0.46

## w/o variance terms.
ECM6.data<-list("country","n.obs", "n.countries",
                       "d.credit", "lag.credit", "lag.ineq","d.ineq", "cum.left",
                       "lag.pop", "d.pop",
                       "lag.unemp","d.unemp",
                       "d.gdp","lag.gdp",
                       "lag.gfcf","d.gfcf",
                       "lag.cab", "d.cab",
                       "lag.budg","d.budg",
                       "lag.world.save", "d.world.save",
                       "lag.bm.growth", "d.bm.growth",
                       "lag.old", "d.old"
                 )

ECM6.params<-c("b.l.credit","b.l.top1", "b.d.top1",
                      "b.cl", "b.l.top1cl", "b.d.top1cl",
                      "b.l.unemp", "b.d.unemp",
                      "b.l.pop", "b.d.pop",
                      "b.l.gdp", "b.d.gdp",
                      "b.l.k", "b.d.k",
                      "b.l.cab", "b.d.cab",
                      "b.l.budg", "b.d.budg",
                      "b.l.world.save", "b.d.world.save",
                      "b.l.bm", "b.d.bm", 
                      "b.l.old", "b.d.old",
                      "a.unit",
                      "y.pred","res")

y.init<-rep(NA,n.obs)
y.init[is.na(d.credit)]<-0
a.unit.init<-rep(0, n.countries)
sigma.y.init<-runif(n.countries,.5,10)
temp.inits<-rbeta(n.countries,1,1)
ECM6.inits<-function(){list(
                                 "temp" = temp.inits,
                                 "b.l.top1" = rnorm(1, sd=2),
                                 "b.d.top1" = rnorm(1, sd=2),
                                 "b.cl"= rnorm(1, sd=2),
                                 "b.l.top1cl"= rnorm(1, sd=2),
                                 "b.d.top1cl"= rnorm(1, sd=2),
                                 "b.l.unemp"= rnorm(1, sd=2),
                                 "b.d.unemp"= rnorm(1, sd=2),
                                 "b.l.pop"= rnorm(1, sd=2),
                                 "b.d.pop"= rnorm(1, sd=2),
                                 "b.d.gdp"= rnorm(1, sd=2),
                                 "b.l.gdp"= rnorm(1, sd=2),
                                 "b.l.k"= rnorm(1, sd=2),
                                 "b.d.k"= rnorm(1, sd=2),
                                 "b.l.cab"= rnorm(1, sd=2),
                                 "b.d.cab"= rnorm(1, sd=2),
                                 "b.l.budg"= rnorm(1, sd=2),
                                 "b.d.budg"= rnorm(1, sd=2),
                                 "b.l.world.save"= rnorm(1, sd=2),
                                 "b.d.world.save"= rnorm(1, sd=2),
                                 "b.l.bm"= rnorm(1, sd=2),
                                 "b.d.bm"= rnorm(1, sd=2),
                                 "b.l.old"= rnorm(1, sd=2),
                                 "b.d.old"= rnorm(1, sd=2),
                                 "sigma.country"=runif(1,0,50),
                                 "sigma.l.credit"=runif(1,0,50),
                                 "sigma.l.top1"=runif(1,0,50),
                                 "sigma.d.top1"=runif(1,0,50),
                                 "sigma.l.budg" = runif(1,0,50),
                                 "sigma.d.budg" = runif(1,0,50),
                                 "sigma.l.unemp" = runif(1,0,50),
                                 "sigma.d.unemp" = runif(1,0,50),
                                 "sigma.l.bm.growth"= runif(1,0,50),
                                 "sigma.d.bm.growth"= runif(1,0,50),
                                 "mu.c"=rnorm(1, sd=2),
                                 "mu.l.credit" =rnorm(1, sd=2),
                                 "mu.l.top1"=rnorm(1, sd=2),
                                 "mu.d.top1"=rnorm(1, sd=2),
                                 "mu.l.budg" = rnorm(1, sd=2),
                                 "mu.d.budg" = rnorm(1, sd=2),
                                 "mu.l.unemp" = rnorm(1, sd=2),
                                 "mu.d.unemp" = rnorm(1, sd=2),
                                 "mu.l.bm.growth"= rnorm(1, sd=2),
                                 "mu.d.bm.growth"= rnorm(1, sd=2),
                             "d.credit" = y.init, "a.unit" = a.unit.init, "sigma.y" = sigma.y.init
                             )
                      }  
ECM6.mod<-bugs(data = ECM6.data, inits = ECM6.inits,
              model.file = "AhlquistAnsellNoVariance.bug",
              parameters = ECM6.params,
              n.chains = 3, n.iter = 30000,
              n.burnin = 10000, n.thin = 30,
              bugs.directory=bugs.dir, bugs.seed=seeds,
              working.directory= work.dir, DIC=T, debug=F)
# 14319
resids<-ECM6.mod$median$res
R2.ECM6<-1-(t(resids)%*%resids/sum((d.credit-mean(d.credit, na.rm=T))^2, na.rm=T)) #0.52

#Table 1
model.comps<-data.frame(Model = c("Base model", "w/o interaction", "w/o varaiance terms", "CAR prior (distance)", "CAR prior (language)"),
  DIC = c(ECMmain.mod$DIC, ECM5.mod$DIC, ECM6.mod$DIC, ECM3.mod$DIC, ECM10.mod$DIC),
  R2 =  c(R2.main, R2.ECM5, R2.ECM6, R2.ECM3, R2.ECM10))
save(model.comps, file="ModelComps.RData")

## using maj
ECM4.data<-list("country","n.obs", "n.countries",
                       "n.years",
                       "d.credit", "lag.credit", "lag.ineq","d.ineq", "maj",
                       "lag.pop", "d.pop",
                       "lag.unemp","d.unemp",
                       "d.gdp","lag.gdp",
                       "lag.gfcf","d.gfcf",
                       "lag.cab", "d.cab",
                       "lag.budg","d.budg",
                       "lag.world.save", "d.world.save",
                       "lag.bm.growth", "d.bm.growth",
                       "years.d", "eurozone",
                       "lag.old", "d.old"
                 )

ECM4.params<-c("b.l.credit","b.l.top1", "b.d.top1",
                      "b.maj", "b.l.top1maj", "b.d.top1maj",
                      "b.l.unemp", "b.d.unemp",
                      "b.l.pop", "b.d.pop",
                      "b.l.gdp", "b.d.gdp",
                      "b.l.k", "b.d.k",
                      "b.l.cab", "b.d.cab",
                      "b.l.budg", "b.d.budg",
                      "b.l.world.save", "b.d.world.save",
                      "b.l.bm", "b.d.bm",
                      "b.l.old", "b.d.old", 
                      "a.unit", "g.unit","g.year","g.euro",
                      "logsigma2.y", "sigma.country", "sigma.vc", "sigma.vy",
                      "y.pred","res")

y.init<-rep(NA,n.obs)
y.init[is.na(d.credit)]<-0
a.unit.init<-rep(0, n.countries)
g.unit.init<-rep(0, n.countries)
g.year.init<-rep(0, n.years)
temp.inits<-rbeta(n.countries,1,1)
ECM4.inits<-function(){list(
                                 "temp" = temp.inits,
                                 "b.l.top1" = rnorm(1, sd=2),
                                 "b.d.top1" = rnorm(1, sd=2),
                                 "b.maj"= rnorm(1, sd=2),
                                 "b.l.top1maj"= rnorm(1, sd=2),
                                 "b.d.top1maj"= rnorm(1, sd=2),
                                 "b.l.unemp"= rnorm(1, sd=2),
                                 "b.d.unemp"= rnorm(1, sd=2),
                                 "b.l.pop"= rnorm(1, sd=2),
                                 "b.d.pop"= rnorm(1, sd=2),
                                 "b.d.gdp"= rnorm(1, sd=2),
                                 "b.l.gdp"= rnorm(1, sd=2),
                                 "b.l.k"= rnorm(1, sd=2),
                                 "b.d.k"= rnorm(1, sd=2),
                                 "b.l.cab"= rnorm(1, sd=2),
                                 "b.d.cab"= rnorm(1, sd=2),
                                 "b.l.budg"= rnorm(1, sd=2),
                                 "b.d.budg"= rnorm(1, sd=2),
                                 "b.l.world.save"= rnorm(1, sd=2),
                                 "b.d.world.save"= rnorm(1, sd=2),
                                 "b.l.bm"= rnorm(1, sd=2),
                                 "b.d.bm"= rnorm(1, sd=2),
                                 "b.l.old"= rnorm(1, sd=2),
                                 "b.d.old"= rnorm(1, sd=2),
                                 "g.euro" = rnorm(1, sd=2),
                                 "sigma.country"=runif(1,0,50),
                                 "sigma.l.credit"=runif(1,0,50),
                                 "sigma.l.top1"=runif(1,0,50),
                                 "sigma.d.top1"=runif(1,0,50),
                                 "sigma.l.budg" = runif(1,0,50),
                                 "sigma.d.budg" = runif(1,0,50),
                                 "sigma.l.unemp" = runif(1,0,50),
                                 "sigma.d.unemp" = runif(1,0,50),
                                 "sigma.l.bm.growth"= runif(1,0,50),
                                 "sigma.d.bm.growth"= runif(1,0,50),
                                 "sigma.vc" = runif(1,0,5),
                                 "sigma.vy" = runif(1,0,5),
                                 "mu.c"=rnorm(1, sd=2),
                                 "mu.l.credit" =rnorm(1, sd=2),
                                 "mu.l.top1"=rnorm(1, sd=2),
                                 "mu.d.top1"=rnorm(1, sd=2),
                                 "mu.l.budg" = rnorm(1, sd=2),
                                 "mu.d.budg" = rnorm(1, sd=2),
                                 "mu.l.unemp" = rnorm(1, sd=2),
                                 "mu.d.unemp" = rnorm(1, sd=2),
                                 "mu.l.bm.growth"= rnorm(1, sd=2),
                                 "mu.d.bm.growth"= rnorm(1, sd=2),
                             "d.credit" = y.init, "a.unit" = a.unit.init,
                             "g.unit"= g.unit.init, "g.year" = g.year.init
                             )
                      }  
ECM4.mod<-bugs(data = ECM4.data, inits = ECM4.inits,
              model.file = "AhlquistAnsellMajoritarianModel.bug",
              parameters = ECM4.params,
              n.chains = 3, n.iter = 30000,
              n.burnin = 10000, n.thin = 30,
              bugs.directory=bugs.dir, bugs.seed=seeds,
              working.directory= work.dir, DIC=T, debug=F)
# DIC  14210
resids<-ECM4.mod$median$res
R2.ECM4<-1-(t(resids)%*%resids/sum((credit-mean(credit, na.rm=T))^2, na.rm=T)) #0.46
maj.dic<-ECM4.mod$DIC

## Using disproportionality  
ECM8.data<-list("country","n.obs", "n.countries",
                       "n.years",
                       "d.credit", "lag.credit", "lag.ineq","d.ineq", "disprop",
                       "lag.pop", "d.pop",
                       "lag.unemp","d.unemp",
                       "d.gdp","lag.gdp",
                       "lag.gfcf","d.gfcf",
                       "lag.cab", "d.cab",
                       "lag.budg","d.budg",
                       "lag.world.save", "d.world.save",
                       "lag.bm.growth", "d.bm.growth",
                       "years.d", "eurozone",
                       "lag.old", "d.old"
                 )

ECM8.params<-c("b.l.credit","b.l.top1", "b.d.top1",
                      "b.dp", "b.l.top1dp", "b.d.top1dp",
                      "b.l.unemp", "b.d.unemp",
                      "b.l.pop", "b.d.pop",
                      "b.l.gdp", "b.d.gdp",
                      "b.l.k", "b.d.k",
                      "b.l.cab", "b.d.cab",
                      "b.l.budg", "b.d.budg",
                      "b.l.world.save", "b.d.world.save",
                      "b.l.bm", "b.d.bm",
                      "b.l.old","b.d.old",
                      "a.unit", "g.unit","g.year","g.euro",
                      "logsigma2.y", "sigma.country", "sigma.vc", "sigma.vy",
                      "y.pred","res")

y.init<-rep(NA,n.obs)
y.init[is.na(d.credit)]<-0
a.unit.init<-rep(0, n.countries)
g.unit.init<-rep(0, n.countries)
g.year.init<-rep(0, n.years)
temp.inits<-rbeta(n.countries,1,1)
ECM8.inits<-function(){list(
                                 "temp" = temp.inits,
                                 "b.l.top1" = rnorm(1, sd=2),
                                 "b.d.top1" = rnorm(1, sd=2),
                                 "b.dp"= rnorm(1, sd=2),
                                 "b.l.top1dp"= rnorm(1, sd=2),
                                 "b.d.top1dp"= rnorm(1, sd=2),
                                 "b.l.unemp"= rnorm(1, sd=2),
                                 "b.d.unemp"= rnorm(1, sd=2),
                                 "b.l.pop"= rnorm(1, sd=2),
                                 "b.d.pop"= rnorm(1, sd=2),
                                 "b.d.gdp"= rnorm(1, sd=2),
                                 "b.l.gdp"= rnorm(1, sd=2),
                                 "b.l.k"= rnorm(1, sd=2),
                                 "b.d.k"= rnorm(1, sd=2),
                                 "b.l.cab"= rnorm(1, sd=2),
                                 "b.d.cab"= rnorm(1, sd=2),
                                 "b.l.budg"= rnorm(1, sd=2),
                                 "b.d.budg"= rnorm(1, sd=2),
                                 "b.l.world.save"= rnorm(1, sd=2),
                                 "b.d.world.save"= rnorm(1, sd=2),
                                 "b.l.bm"= rnorm(1, sd=2),
                                 "b.d.bm"= rnorm(1, sd=2),
                                 "b.l.old" = rnorm(1, sd=2),
                                 "b.d.old"= rnorm(1, sd=2),
                                 "g.euro" = rnorm(1, sd=2),
                                 "sigma.country"=runif(1,0,50),
                                 "sigma.l.credit"=runif(1,0,50),
                                 "sigma.l.top1"=runif(1,0,50),
                                 "sigma.d.top1"=runif(1,0,50),
                                 "sigma.l.budg" = runif(1,0,50),
                                 "sigma.d.budg" = runif(1,0,50),
                                 "sigma.l.unemp" = runif(1,0,50),
                                 "sigma.d.unemp" = runif(1,0,50),
                                 "sigma.l.bm.growth"= runif(1,0,50),
                                 "sigma.d.bm.growth"= runif(1,0,50),
                                 "sigma.vc" = runif(1,0,5),
                                 "sigma.vy" = runif(1,0,5),
                                 "mu.c"=rnorm(1, sd=2),
                                 "mu.l.credit" =rnorm(1, sd=2),
                                 "mu.l.top1"=rnorm(1, sd=2),
                                 "mu.d.top1"=rnorm(1, sd=2),
                                 "mu.l.budg" = rnorm(1, sd=2),
                                 "mu.d.budg" = rnorm(1, sd=2),
                                 "mu.l.unemp" = rnorm(1, sd=2),
                                 "mu.d.unemp" = rnorm(1, sd=2),
                                 "mu.l.bm.growth"= rnorm(1, sd=2),
                                 "mu.d.bm.growth"= rnorm(1, sd=2),
                             "d.credit" = y.init, "a.unit" = a.unit.init,
                             "g.unit"= g.unit.init, "g.year" = g.year.init
                             )
                      }  

ECM8.mod<-bugs(data = ECM8.data, inits = ECM8.inits,
              model.file = "AhlquistAnsellDisproportionalityModel.bug",
              parameters = ECM8.params,
              n.chains = 3, n.iter = 30000,
              n.burnin = 10000, n.thin = 30,
              bugs.directory=bugs.dir, bugs.seed=seeds,
              working.directory= work.dir, DIC=T, debug=F)
# DIC 14208
resids<-ECM8.mod$median$res
R2.ECM8<-1-(t(resids)%*%resids/sum((credit-mean(credit, na.rm=T))^2, na.rm=T)) #0.46
disp.dic<-ECM8.mod$DIC

## using ENPP
ECM9.data<-list("country","n.obs", "n.countries",
                       "n.years",
                       "d.credit", "lag.credit", "lag.ineq","d.ineq", "enpp",
                       "lag.pop", "d.pop",
                       "lag.unemp","d.unemp",
                       "d.gdp","lag.gdp",
                       "lag.gfcf","d.gfcf",
                       "lag.cab", "d.cab",
                       "lag.budg","d.budg",
                       "lag.world.save", "d.world.save",
                       "lag.bm.growth", "d.bm.growth",
                       "years.d", "eurozone",
                       "lag.old", "d.old"
                 )

ECM9.params<-c("b.l.credit","b.l.top1", "b.d.top1",
                      "b.enpp", "b.l.top1enpp", "b.d.top1enpp",
                      "b.l.unemp", "b.d.unemp",
                      "b.l.pop", "b.d.pop",
                      "b.l.gdp", "b.d.gdp",
                      "b.l.k", "b.d.k",
                      "b.l.cab", "b.d.cab",
                      "b.l.budg", "b.d.budg",
                      "b.l.world.save", "b.d.world.save",
                      "b.l.bm", "b.d.bm",
                      "b.l.old", "b.d.old", 
                      "a.unit", "g.unit","g.year","g.euro",
                      "logsigma2.y", "sigma.country", "sigma.vc", "sigma.vy",
                      "y.pred","res")

y.init<-rep(NA,n.obs)
y.init[is.na(d.credit)]<-0
a.unit.init<-rep(0, n.countries)
g.unit.init<-rep(0, n.countries)
g.year.init<-rep(0, n.years)
temp.inits<-rbeta(n.countries,1,1)
ECM9.inits<-function(){list(
                                 "temp" = temp.inits,
                                 "b.l.top1" = rnorm(1, sd=2),
                                 "b.d.top1" = rnorm(1, sd=2),
                                 "b.enpp"= rnorm(1, sd=2),
                                 "b.l.top1enpp"= rnorm(1, sd=2),
                                 "b.d.top1enpp"= rnorm(1, sd=2),
                                 "b.l.unemp"= rnorm(1, sd=2),
                                 "b.d.unemp"= rnorm(1, sd=2),
                                 "b.l.pop"= rnorm(1, sd=2),
                                 "b.d.pop"= rnorm(1, sd=2),
                                 "b.d.gdp"= rnorm(1, sd=2),
                                 "b.l.gdp"= rnorm(1, sd=2),
                                 "b.l.k"= rnorm(1, sd=2),
                                 "b.d.k"= rnorm(1, sd=2),
                                 "b.l.cab"= rnorm(1, sd=2),
                                 "b.d.cab"= rnorm(1, sd=2),
                                 "b.l.budg"= rnorm(1, sd=2),
                                 "b.d.budg"= rnorm(1, sd=2),
                                 "b.l.world.save"= rnorm(1, sd=2),
                                 "b.d.world.save"= rnorm(1, sd=2),
                                 "b.l.bm"= rnorm(1, sd=2),
                                 "b.d.bm"= rnorm(1, sd=2),
                                 "b.l.old" = rnorm(1, sd=2),
                                 "b.d.old" = rnorm(1, sd=2),
                                 "g.euro" = rnorm(1, sd=2),
                                 "sigma.country"=runif(1,0,50),
                                 "sigma.l.credit"=runif(1,0,50),
                                 "sigma.l.top1"=runif(1,0,50),
                                 "sigma.d.top1"=runif(1,0,50),
                                 "sigma.l.budg" = runif(1,0,50),
                                 "sigma.d.budg" = runif(1,0,50),
                                 "sigma.l.unemp" = runif(1,0,50),
                                 "sigma.d.unemp" = runif(1,0,50),
                                 "sigma.l.bm.growth"= runif(1,0,50),
                                 "sigma.d.bm.growth"= runif(1,0,50),
                                 "sigma.vc" = runif(1,0,5),
                                 "sigma.vy" = runif(1,0,5),
                                 "mu.c"=rnorm(1, sd=2),
                                 "mu.l.credit" =rnorm(1, sd=2),
                                 "mu.l.top1"=rnorm(1, sd=2),
                                 "mu.d.top1"=rnorm(1, sd=2),
                                 "mu.l.budg" = rnorm(1, sd=2),
                                 "mu.d.budg" = rnorm(1, sd=2),
                                 "mu.l.unemp" = rnorm(1, sd=2),
                                 "mu.d.unemp" = rnorm(1, sd=2),
                                 "mu.l.bm.growth"= rnorm(1, sd=2),
                                 "mu.d.bm.growth"= rnorm(1, sd=2),
                             "d.credit" = y.init, "a.unit" = a.unit.init,
                             "g.unit"= g.unit.init, "g.year" = g.year.init
                             )
                      }  
ECM9.mod<-bugs(data = ECM9.data, inits = ECM9.inits,
              model.file = "AhlquistAnsellENPPmodel.bug",
              parameters = ECM9.params,
              n.chains = 3, n.iter = 30000,
              n.burnin = 10000, n.thin = 30,
              bugs.directory=bugs.dir, bugs.seed=seeds,
              working.directory= work.dir, DIC=T, debug=F)
# DIC 14205
resids<-ECM9.mod$median$res
R2.ECM9<-1-(t(resids)%*%resids/sum((credit-mean(credit, na.rm=T))^2, na.rm=T)) #0.46
enpp.dic<-ECM9.mod$DIC
m4.outlist<-as.mcmc.bugs(ECM4.mod) #maj
m8.outlist<-as.mcmc.bugs(ECM8.mod) #disprop
m9.outlist<-as.mcmc.bugs(ECM9.mod) #enpp


#figure 8
pdf("Figure8.pdf")
  par(mfrow=c(3,1), mar=c(3,7.5,4,.25))
  caterplot(m4.outlist, style="plain", parms=c("b.l.top1", "b.d.top1", "b.maj", "b.d.top1maj", "b.l.top1maj"),
            greek=F, labels = c("lag inequality", expression(paste(Delta, " inequality")),
              "Majoritarian", expression(paste(Delta, " ineq. x Maj.")), "lag ineq. x Maj."), 
            cex.labels=.90)
  abline(v=0, lty=2)
  title(main = "Majoritarian dummy")
  mtext(paste("DIC =",ECM4.mod$DIC), side=1, line=2, cex=0.75)
  caterplot(m8.outlist, style="plain", parms=c("b.l.top1", "b.d.top1", "b.dp", "b.d.top1dp", "b.l.top1dp"),
            greek=F, labels = c("lag inequality", expression(paste(Delta, " inequality")),
              "Disprop.", expression(paste(Delta, " ineq. x disprop.")), "lag ineq. x disprop."), 
            cex.labels=.90)
  abline(v=0, lty=2)
  title(main = "Gahallager index")
  mtext(paste("DIC =",ECM8.mod$DIC), side=1, line=2, cex=0.75)
  caterplot(m9.outlist, style="plain", parms=c("b.l.top1", "b.d.top1", "b.enpp", "b.d.top1enpp", "b.l.top1enpp"),
            greek=F, labels = c("lag inequality", expression(paste(Delta, " inequality")),
              "ENLP", expression(paste(Delta, " ineq. x ENLP")), "lag ineq. x ENLP"), 
            cex.labels=.90)
  abline(v=0, lty=2)
  title(main = "ENLP")
  mtext(paste("DIC =",ECM9.mod$DIC), side=1, line=2, cex=0.75)  
dev.off()
#END