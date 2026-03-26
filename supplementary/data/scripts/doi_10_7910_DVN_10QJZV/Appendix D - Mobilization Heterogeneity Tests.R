#Formation Heterogeneity Tests
rm(list=ls(all=TRUE))
options(warn=-1)
require(texreg)
require(lmtest)
require(multiwayvcov)
require(DataCombine)

df = read.csv("blattmanshocksreplication2.csv")
df <- read.csv("~/Dropbox/Dissertation/Formation/Rewrite/Shocks/Data/blattmanshocksreplication2.csv")
##########################
# CREATE ONSET FORMATION #
##########################
library(dplyr)

df2 = df %>% group_by(ccode, year) %>% mutate(min = ifelse(anynew==1, min(year), 2012))
df3 = df2 %>% group_by(ccode) %>% mutate(min2 = min(min))

df3$onset_form = ifelse(df3$year == df3$min2, 1, NA)
df3$onset_form[df3$year < df3$min2] = 0

df3$noonset_form = ifelse(df3$year == df3$min2, df3$nonew, NA)
df3$noonset_form[df3$year < df3$min2] = 0

df2 = df %>% group_by(ccode, year) %>% mutate(min = ifelse(newsep==1, min(year), 2012))
dfsep = df2 %>% group_by(ccode) %>% mutate(min2 = min(min))

dfsep$onset_sepform = ifelse(dfsep$year == dfsep$min2, 1, NA)
dfsep$onset_sepform[dfsep$year < dfsep$min2] = 0

df2 = df %>% group_by(ccode, year) %>% mutate(min = ifelse(newcenter==1, min(year), 2012))
dfcenter = df2 %>% group_by(ccode) %>% mutate(min2 = min(min))

dfcenter$onset_centerform = ifelse(dfcenter$year == dfcenter$min2, 1, NA)
dfcenter$onset_centerform[dfcenter$year < dfcenter$min2] = 0

##############################
## HETEROGENEITY ANALYSIS
##############################

##############################
## A. SHOCK TYPE
##############################

m3 = lm(anynew ~ pshock_perennial_npi_p + pshock_perennial_npi_p1  + pshock_perennial_npi_p2 + pshock_extract_npi_p + pshock_extract_npi_p1  + pshock_extract_npi_p2 + pshock_annual_npi_p + pshock_annual_npi_p1 + pshock_annual_npi_p2   + factor(ccode) + factor(year) + factor(ccode)*year, data=df)
summary(m3)
vcov_firm <- cluster.vcov(m3, df$ccode)
c1 = coeftest(m3, vcov_firm)

texreg(list(c1), omit.coef="ccode|year", caption="Shock Type", stars=c(0.01, 0.05, 0.1))
texreg(list(m3))


coefplot.gg = function(cluster1, data1){
  # data is a data frame with 4 columns
  # data$names gives variable names
  # data$modelcoef gives center point
  # data$ylo gives lower limits
  # data$yhi gives upper limits
  modelcoef1 = cluster1[c(2:4), 1]
  modelse1 = cluster1[c(2:4), 2]
  ylo1 = modelcoef1 - qt(.975, nrow(data1))*(modelse1)
  yhi1 = modelcoef1 + qt(.975, nrow(data1))*(modelse1)
  
  names1 = names(cluster1[c(2:4),1])
  names1 = c("pshock_npi_p", "pshock_npi_p1", "pshock_npi_p2")
  dfplot1 = data.frame(names1, modelcoef1, modelse1, ylo1, yhi1)
  
  modelcoef2 = cluster1[c(5:7), 1]
  modelse2 = cluster1[c(5:7), 2]
  ylo2 = modelcoef2 - qt(.975, nrow(data1))*(modelse2)
  yhi2 = modelcoef2 + qt(.975, nrow(data1))*(modelse2)
  names2 = names(cluster1[c(5:7),1])
  dfplot2 = data.frame(names2, modelcoef2, modelse2, ylo2, yhi2)
  
  
  modelcoef3 = cluster1[c(8:10), 1]
  modelse3 = cluster1[c(8:10), 2]
  ylo3 = modelcoef2 - qt(.975, nrow(data1))*(modelse3)
  yhi3 = modelcoef2 + qt(.975, nrow(data1))*(modelse3)
  names3 = names(cluster1[c(8:10),1])
  dfplot3 = data.frame(names3, modelcoef3, modelse3, ylo3, yhi3)
  
  
  #define plot
  require(ggplot2)
  p = ggplot()  +
    geom_pointrange(dfplot3, mapping=aes(x=names1, y=modelcoef3, ymin=ylo3, ymax=yhi3, lty="Annual"),  position = position_nudge(-0.1))  +
    geom_pointrange(dfplot2, mapping=aes(x=names1, y=modelcoef2, ymin=ylo2, ymax=yhi2, lty="Extractive")) + geom_pointrange(dfplot1, mapping=aes(x=names1, y=modelcoef1, ymin=ylo1, ymax=yhi1, lty="Perennial"),  position = position_nudge(0.1)) + theme_bw()    + geom_hline(aes(yintercept=0), lty=5, col="red") + ylab('') + ggtitle("Shock Type") +
    theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 20)) + scale_x_discrete(labels=c(expression("Price Shock"[t]), expression("Price Shock"[t-1]), expression("Price Shock"[t-2])))+ theme(legend.key.size = unit(4, "lines"), legend.position = "bottom")  + xlab('Variable')+ scale_linetype_manual(name="Shock Type:", values=c(3, 2, 1), labels = c("Annual","Extractive",  "Perennial"))
  return(p)
  #p = ggplot()  +   geom_pointrange(dfplot3, mapping=aes(x=names1, y=modelcoef3, ymin=ylo3, ymax=yhi3, lty="Annual"),  position = position_nudge(-0.1))  +   geom_pointrange(dfplot2, mapping=aes(x=names1, y=modelcoef2, ymin=ylo2, ymax=yhi2, lty="Extractive")) + geom_pointrange(dfplot1, mapping=aes(x=names1, y=modelcoef1, ymin=ylo1, ymax=yhi1, lty="Perennial"),  position = position_nudge(0.1)) + theme_bw()    + geom_hline(aes(yintercept=0), lty=5, col="red") + ylab('') + ggtitle("Formation Onset") +
  #  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 30)) + scale_x_discrete(labels=c(expression("Price Shock"[t]), expression("Price Shock"[t-1]), expression("Price Shock"[t-2])))+ theme(legend.key.size = unit(4, "lines"), legend.position = "bottom")  + xlab('Variable')+ scale_linetype_manual(name="Shock Type:", values=c(3, 2, 1), labels = c("Annual","Extractive",  "Perennial"))
  #return(p)
}
library(gridExtra)
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}



onsetshocktype = coefplot.gg(a1, df)
onsetshocktype 
legend <- get_legend(onsetshocktype)
legendshocktype <- get_legend(onsetshocktype)


coefplot.gg = function(cluster1, data1){
  # data is a data frame with 4 columns
  # data$names gives variable names
  # data$modelcoef gives center point
  # data$ylo gives lower limits
  # data$yhi gives upper limits
  modelcoef1 = cluster1[c(2:4), 1]
  modelse1 = cluster1[c(2:4), 2]
  ylo1 = modelcoef1 - qt(.975, nrow(data1))*(modelse1)
  yhi1 = modelcoef1 + qt(.975, nrow(data1))*(modelse1)
  
  names1 = names(cluster1[c(2:4),1])
  names1 = c("pshock_npi_p", "pshock_npi_p1", "pshock_npi_p2")
  dfplot1 = data.frame(names1, modelcoef1, modelse1, ylo1, yhi1)
  
  modelcoef2 = cluster1[c(5:7), 1]
  modelse2 = cluster1[c(5:7), 2]
  ylo2 = modelcoef2 - qt(.975, nrow(data1))*(modelse2)
  yhi2 = modelcoef2 + qt(.975, nrow(data1))*(modelse2)
  names2 = names(cluster1[c(5:7),1])
  dfplot2 = data.frame(names2, modelcoef2, modelse2, ylo2, yhi2)
  
  modelcoef3 = cluster1[c(8:10), 1]
  modelse3 = cluster1[c(8:10), 2]
  ylo3 = modelcoef2 - qt(.975, nrow(data1))*(modelse3)
  yhi3 = modelcoef2 + qt(.975, nrow(data1))*(modelse3)
  names3 = names(cluster1[c(8:10),1])
  dfplot3 = data.frame(names3, modelcoef3, modelse3, ylo3, yhi3)
  
  
  #define plot
  require(ggplot2)
  p = ggplot()  +
    geom_pointrange(dfplot3, mapping=aes(x=names1, y=modelcoef3, ymin=ylo3, ymax=yhi3, lty="Annual"),  position = position_nudge(-0.1))  +
    geom_pointrange(dfplot2, mapping=aes(x=names1, y=modelcoef2, ymin=ylo2, ymax=yhi2, lty="Extractive")) + geom_pointrange(dfplot1, mapping=aes(x=names1, y=modelcoef1, ymin=ylo1, ymax=yhi1, lty="Perennial"),  position = position_nudge(0.1)) + theme_bw()    + geom_hline(aes(yintercept=0), lty=5, col="red") + ylab('') + ggtitle("Mobilization") +
    theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 30)) + scale_x_discrete(labels=c(expression("Price Shock"[t]), expression("Price Shock"[t-1]), expression("Price Shock"[t-2]))) + theme(legend.key.size = unit(4, "lines"), legend.position = "bottom")  + xlab('Variable')+ scale_linetype_manual(name="Shock Type", values=c(3, 2, 1), labels = c("Annual","Extractive",  "Perennial"))
  return(p)
  
}

incidenceshocktype = coefplot.gg(c1, df)
incidenceshocktype 
incidenceshocktype = incidenceshocktype  + theme(legend.position="none")
# 4. Arrange ggplot2 graphs with a specific width
pdf("shocktypemobilization.pdf", 20, 10)
incidenceshocktype
dev.off()
require(gridExtra)



##############################
## B. GROUP TYPE
##############################

##########SEP VS CENTER ONSET##############
mean(dfsep$onset_sepform , na.rm=T)
mean(dfcenter$onset_centerform, na.rm=T)

##########SEP VS CENTER INCIDENCE##############
mean(df$newsep, na.rm=T)
mean(df$newcenter, na.rm=T)

m0 = lm(newsep ~ pshock_npi_p  + pshock_npi_p1 + pshock_npi_p2 + factor(ccode) + factor(year) + factor(ccode)*year, data=df)
summary(m0)
vcov_firm <- cluster.vcov(m0, df$ccode)
a1 = coeftest(m0, vcov_firm)

m2 = lm(newcenter ~ pshock_npi_p + pshock_npi_p1 + pshock_npi_p2  + factor(ccode) + factor(year) + factor(ccode)*year, data=df)
summary(m2)
vcov_firm <- cluster.vcov(m2, df$ccode)
b1 = coeftest(m2, vcov_firm)


require(texreg)
texreg(list(b1,  a1), omit.coef="ccode", caption="All", stars=c(0.01, 0.05, 0.1)) #Formation and conflict
texreg(list(m2, m0), omit.coef="ccode", caption="All", stars=c(0.01, 0.05, 0.1)) #Formation and conflict

coefplot.gg = function(cluster1, data1, cluster2, data2){
  # data is a data frame with 4 columns
  # data$names gives variable names
  # data$modelcoef gives center point
  # data$ylo gives lower limits
  # data$yhi gives upper limits
  modelcoef1 = cluster1[c(2:4), 1]
  modelse1 = cluster1[c(2:4), 2]
  ylo1 = modelcoef1 - qt(.975, nrow(data1))*(modelse1)
  yhi1 = modelcoef1 + qt(.975, nrow(data1))*(modelse1)
  names1 = names(cluster1[c(2:4),1])
  dfplot1 = data.frame(names1, modelcoef1, modelse1, ylo1, yhi1)
  
  modelcoef2 = cluster2[c(2:4), 1]
  modelse2 = cluster2[c(2:4), 2]
  ylo2 = modelcoef2 - qt(.975, nrow(data2))*(modelse2)
  yhi2 = modelcoef2 + qt(.975, nrow(data2))*(modelse2)
  names2 = names(cluster2[c(2:4),1])
  dfplot2 = data.frame(names2, modelcoef2, modelse2, ylo2, yhi2)
  
  #define plot
  require(ggplot2)
  p = ggplot() +
    geom_pointrange(dfplot2, mapping=aes(x=names2, y=modelcoef2, ymin=ylo2, ymax=yhi2, lty="Center-Seeking"),  position = position_nudge(-0.05)) + geom_pointrange(dfplot1, mapping=aes(x=names1, y=modelcoef1, ymin=ylo1, ymax=yhi1, lty="Autonomy-Seeking"),  position = position_nudge(0.05))    + theme_bw()    + geom_hline(aes(yintercept=0), lty=5, col="red") + ylab('') + ggtitle("Mobilization") +
    theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 30)) + scale_x_discrete(labels=c(expression("Price Shock"[t]), expression("Price Shock"[t-1]), expression("Price Shock"[t-2])))+ theme(legend.key.size = unit(4, "lines"), legend.position = "bottom")  + xlab('Variable')+ scale_linetype_manual(name="Group Type", values=c(2, 1), labels = c("Autonomy-Seeking", "Center-Seeking"))
  return(p)
}
incidencecentersep = coefplot.gg(a1, df, b1, df)
incidencecentersep

onsetcentersep = onsetcentersep + theme(legend.position="none")

incidencecentersep = incidencecentersep + theme(legend.position="none")
# 4. Arrange ggplot2 graphs with a specific width
pdf("grouptypemob.pdf", 20, 10)
grid.arrange(incidencecentersep)
dev.off()
require(gridExtra)



#####################
## SEP AND SHOCK TYPE
####################


m2 = lm(newsep ~ pshock_perennial_npi_p + pshock_perennial_npi_p1  + pshock_perennial_npi_p2 + pshock_extract_npi_p + pshock_extract_npi_p1  + pshock_extract_npi_p2 + pshock_annual_npi_p + pshock_annual_npi_p1 + pshock_annual_npi_p2  + factor(ccode) + factor(year) + factor(ccode)*year, data=df)
summary(m2)
vcov_firm <- cluster.vcov(m2, df$ccode)
c1 = coeftest(m2, vcov_firm)

texreg(list(c1), omit.coef="ccode", caption="Separatist and Shock Type", stars=c(0.01, 0.05, 0.1))
texreg(list(m3))


#####################
## CENTER AND SHOCK TYPE
####################


m2 = lm(newcenter ~ pshock_perennial_npi_p + pshock_perennial_npi_p1  + pshock_perennial_npi_p2 + pshock_extract_npi_p + pshock_extract_npi_p1  + pshock_extract_npi_p2 + pshock_annual_npi_p + pshock_annual_npi_p1 + pshock_annual_npi_p2  + factor(ccode) + factor(year) + factor(ccode)*year, data=df)
summary(m2)
vcov_firm <- cluster.vcov(m2, df$ccode)
c1 = coeftest(m2, vcov_firm)

texreg(list(c1), omit.coef="ccode", caption="Separatist and Shock Type", stars=c(0.01, 0.05, 0.1))
texreg(list(m3))

######################
## NONDEM
######################
df2 = subset(df, df$l3nondem==1)
df4 = subset(df3, df3$l3nondem==1)
mean(df4$onset_form, na.rm=T)
mean(df2$anynew, na.rm=T)


m3 = lm(anynew ~ pshock_npi_p + pshock_npi_p1 + pshock_npi_p2  + factor(ccode) + factor(year) + factor(ccode)*year, data=df2)
summary(m3)
vcov_firm <- cluster.vcov(m3, df2$ccode)
c1 = coeftest(m3, vcov_firm)

texreg(list(c1), omit.coef="ccode", caption="Nondemocratic Institutions", stars=c(0.01, 0.05, 0.1))
texreg(list(m3))

######################
## ANOC
######################
df2 = subset(df, df$l3anoc==1)
df4 = subset(df3, df3$l3anoc==1)
mean(df4$onset_form, na.rm=T)
mean(df2$anynew, na.rm=T)

m3 = lm(anynew ~ pshock_npi_p + pshock_npi_p1 + pshock_npi_p2  + factor(ccode) + factor(year) + factor(ccode)*year, data=df2)
summary(m3)
vcov_firm <- cluster.vcov(m3, df2$ccode)
c1 = coeftest(m3, vcov_firm)


texreg(list(c1), omit.coef="ccode", caption="Anocratic Institutions", stars=c(0.01, 0.05, 0.1))
texreg(list(m3))



######################
## LOW XCONST
######################
df2 = subset(df, df$l3lowxconst==1)
df4 = subset(df3, df3$l3lowxconst==1)
mean(df4$onset_form, na.rm=T)
mean(df2$anynew, na.rm=T)


m3 = lm(anynew ~ pshock_npi_p + pshock_npi_p1 + pshock_npi_p2  + factor(ccode) + factor(year) + factor(ccode)*year, data=df2)
summary(m3)
vcov_firm <- cluster.vcov(m3, df2$ccode)
c1 = coeftest(m3, vcov_firm)

texreg(list(c1), omit.coef="ccode", caption="Low Executive Constraints", stars=c(0.01, 0.05, 0.1))
texreg(list(m3))


######################
## HIGH GINI
######################
df2 = subset(df, df$highgini==1)
df4 = subset(df3, df3$highgini==1)
mean(df4$onset_form, na.rm=T)
mean(df2$anynew, na.rm=T)


m3 = lm(anynew ~ pshock_npi_p + pshock_npi_p1 + pshock_npi_p2  + factor(ccode) + factor(year) + factor(ccode)*year, data=df2)
summary(m3)
vcov_firm <- cluster.vcov(m3, df2$ccode)
c1 = coeftest(m3, vcov_firm)


texreg(list(c1), omit.coef="ccode", caption="High Gini", stars=c(0.01, 0.05, 0.1))
texreg(list(m3))


######################
## LOW INCOME
######################
df2 = subset(df, df$lowincome==1)
df4 = subset(df3, df3$lowincome==1)
mean(df4$onset_form, na.rm=T)
mean(df2$anynew, na.rm=T)


m3 = lm(anynew ~ pshock_npi_p + pshock_npi_p1 + pshock_npi_p2  + factor(ccode) + factor(year) + factor(ccode)*year, data=df2)
summary(m3)
vcov_firm <- cluster.vcov(m3, df2$ccode)
c1 = coeftest(m3, vcov_firm)

texreg(list(c1), omit.coef="ccode", caption="Low Income", stars=c(0.01, 0.05, 0.1))
texreg(list(m3))

