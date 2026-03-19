
###########################
## ECON SHOCKS - MAIN RESULTS
## Version: 18 November 2021
##########################

rm(list=ls(all=TRUE))
options(warn=-1)
require(texreg)
require(lmtest)
require(multiwayvcov)
require(DataCombine)
df = read.csv("blattmanshocksreplication2.csv")

################################################
### CONDITIONAL PROBABILITIES
################################################
hist(df$l1noactform)

# Probability for mobilization
mean(df$anynew, na.rm=T)

# Probability for civil conflict (25 bd)
mean(df$onset_any_prio, na.rm=T)
mean(df$any_prio, na.rm=T)

mean(df$onset_any_prio[df$l1noactform==0], na.rm=T)
mean(df$any_prio[df$l1noactform==0], na.rm=T)

mean(df$onset_any_prio[df$l1noactform > 0], na.rm=T)
mean(df$any_prio[df$l1noactform > 0], na.rm=T)

# probability for civil war (1000 bd)
mean(df$onset_war_prio_bb, na.rm=T)
mean(df$war_prio_bb, na.rm=T)

mean(df$onset_war_prio_bb[df$l1noactform==0], na.rm=T)
mean(df$war_prio_bb[df$l1noactform==0], na.rm=T)

mean(df$onset_war_prio_bb[df$l1noactform > 0], na.rm=T)
mean(df$war_prio_bb[df$l1noactform > 0], na.rm=T)

#####################
## Mobilization Onset Analysis
#####################

m1 = lm(anynew ~ pshock_npi_p + pshock_npi_p1 + pshock_npi_p2 + factor(ccode) + factor(year) + factor(ccode)*year, data=df, na.action = na.exclude)
summary(m1)
vcov_firm <- cluster.vcov(m1, df$ccode)
c1 = coeftest(m1, vcov_firm)

summary(df$pshock_npi_p)

library(margins)
m1 = lm(anynew ~ pshock_npi_p + pshock_npi_p1 + pshock_npi_p2 + factor(ccode) + factor(year), data=df, na.action = na.exclude)
summary(m1)
margins(m1)

vcov_firm <- cluster.vcov(m1, df$ccode)
c1 = coeftest(m1, vcov_firm)
#marginal effect of going from the 25th to the 75th quantile

summary(df$pshock_annual_npi_p)
margins(m1, at = list(pshock_npi_p = -0.39:0.35))
#see shifts from 25th to 75th quantile

m2 = lm(anynew ~ pshock_npi_p + pshock_npi_p1 + pshock_npi_p2 + agg_pshock_sum + agg_pshock_sum1 + agg_pshock_sum2 +        importShock1Country1+         importShock2Country1+         importShockCountry2+          importShock1Country2+        
          importShock2Country2+         importShockCountry3+          importShock1Country3+        
          importShock2Country3+         importShockCountry4+          importShock1Country4+        
          importShock2Country4+         importShockCountry5+          importShock1Country5+        
          importShock2Country5+         importShockCountry6+          importShock1Country6+        
          importShock2Country6+         importShockCountry7+          importShock1Country7+        
          importShock2Country7+         importShockCountry8+          importShock1Country8+        
          importShock2Country8+         importShockCountry9+          importShock1Country9+        
          importShock2Country9+         importShockCountry10+         importShock1Country10+       
          importShock2Country10+        importShockCountry11+         importShock1Country11+       
          importShock2Country11+        importShockCountry12+         importShock1Country12+       
          importShock2Country12+        importShockCountry13+         importShock1Country13+       
          importShock2Country13+        importShockCountry14+         importShock1Country14+       
          importShock2Country14+        importShockCountry15+         importShock1Country15+       
          importShock2Country15+        importShockCountry16+         importShock1Country16+       
          importShock2Country16+        importShockCountry17+         importShock1Country17+       
          importShock2Country17+        importShockCountry18+         importShock1Country18+       
          importShock2Country18+        importShockCountry19+         importShock1Country19+       
          importShock2Country19+        importShockCountry20+         importShock1Country20+       
          importShock2Country20+        importShockCountry21+         importShock1Country21+       
          importShock2Country21+        importShockCountry22+         importShock1Country22+       
          importShock2Country22+        importShockCountry23+         importShock1Country23+       
          importShock2Country23+        importShockCountry24+         importShock1Country24+       
          importShock2Country24+        importShockCountry25+         importShock1Country25+       
          importShock2Country25+        importShockCountry26+         importShock1Country26+       
          importShock2Country26+        importShockCountry27+         importShock1Country27+       
          importShock2Country27+        importShockCountry28+         importShock1Country28+       
          importShock2Country28+        importShockCountry29+         importShock1Country29+       
          importShock2Country29+        importShockCountry30+         importShock1Country30+       
          importShock2Country30+        importShockCountry31+         importShock1Country31+       
          importShock2Country31+        importShockCountry32+         importShock1Country32+       
          importShock2Country32+        importShockCountry33+         importShock1Country33+       
          importShock2Country33+        importShockCountry34+         importShock1Country34+       
          importShock2Country34+        importShockCountry35+         importShock1Country35+       
          importShock2Country35+        importShockCountry36+         importShock1Country36+       
          importShock2Country36+        importShockCountry37+         importShock1Country37+       
          importShock2Country37+        importShockCountry38+         importShock1Country38+       
          importShock2Country38+        importShockCountry39+         importShock1Country39+       
          importShock2Country39+        importShockCountry40+         importShock1Country40+       
          importShock2Country40+        importShockCountry41+         importShock1Country41+       
          importShock2Country41+        importShockCountry42+         importShock1Country42+       
          importShock2Country42+        importShockCountry43+         importShock1Country43+       
          importShock2Country43+        importShockCountry44+         importShock1Country44+       
          importShock2Country44+        importShockCountry45+         importShock1Country45+       
          importShock2Country45+        importShockCountry46+         importShock1Country46+       
          importShock2Country46+        importShockCountry47+         importShock1Country47+       
          importShock2Country47+        importShockCountry48+         importShock1Country48+       
          importShock2Country48+        importShockCountry49+         importShock1Country49+       
          importShock2Country49+        importShockCountry50+         importShock1Country50+       
          importShock2Country50+        importShockCountry51+         importShock1Country51+       
          importShock2Country51+        importShockCountry52+         importShock1Country52+       
          importShock2Country52+        importShockCountry53+         importShock1Country53+       
          importShock2Country53+        importShockCountry54+         importShock1Country54+       
          importShock2Country54+        importShockCountry55+         importShock1Country55+       
          importShock2Country55+        importShockCountry56+         importShock1Country56+       
          importShock2Country56+        importShockCountry57+         importShock1Country57+       
          importShock2Country57+        importShockCountry58+         importShock1Country58+       
          importShock2Country58+        importShockCountry59+         importShock1Country59+       
          importShock2Country59+        importShockCountry60+         importShock1Country60+       
          importShock2Country60+        importShockCountry61+         importShock1Country61+       
          importShock2Country61+        importShockCountry62+         importShock1Country62+       
          importShock2Country62+        importShockCountry63+         importShock1Country63+       
          importShock2Country63+        importShockCountry64+         importShock1Country64+       
          importShock2Country64+        importShockCountry65+         importShock1Country65+       
          importShock2Country65+        importShockCountry66+         importShock1Country66+       
          importShock2Country66+        importShockCountry67+         importShock1Country67+       
          importShock2Country67+        importShockCountry68+         importShock1Country68+       
          importShock2Country68+        importShockCountry69+         importShock1Country69+       
          importShock2Country69+        importShockCountry70+         importShock1Country70+       
          importShock2Country70+        importShockCountry71+         importShock1Country71+       
          importShock2Country71+        importShockCountry72+         importShock1Country72+       
          importShock2Country72+        importShockCountry73+         importShock1Country73+       
          importShock2Country73+        importShockCountry74+         importShock1Country74+       
          importShock2Country74+        importShockCountry75+         importShock1Country75+       
          importShock2Country75+        importShockCountry76+         importShock1Country76+       
          importShock2Country76+        importShockCountry77+         importShock1Country77+       
          importShock2Country77+        importShockCountry78+         importShock1Country78+       
          importShock2Country78+        importShockCountry79+         importShock1Country79+       
          importShock2Country79+        importShockCountry80+         importShock1Country80+       
          importShock2Country80+        importShockCountry81+         importShock1Country81+       
          importShock2Country81+        importShockCountry82+         importShock1Country82+       
          importShock2Country82+        importShockCountry83+         importShock1Country83+       
          importShock2Country83+        importShockCountry84+         importShock1Country84+       
          importShock2Country84+        importShockCountry85+         importShock1Country85+       
          importShock2Country85+        importShockCountry86+         importShock1Country86+       
          importShock2Country86+        importShockCountry87+         importShock1Country87+       
          importShock2Country87+        importShockCountry88+         importShock1Country88+       
          importShock2Country88+        importShockCountry89+         importShock1Country89+       
          importShock2Country89+        importShockCountry90+         importShock1Country90+       
          importShock2Country90+        importShockCountry91+         importShock1Country91+       
          importShock2Country91+        importShockCountry92+         importShock1Country92+       
          importShock2Country92+        importShockCountry93+         importShock1Country93+       
          importShock2Country93+        importShockCountry94+         importShock1Country94+       
          importShock2Country94+        importShockCountry95+         importShock1Country95+       
          importShock2Country95+        importShockCountry96+         importShock1Country96+       
          importShock2Country96+        importShockCountry97+         importShock1Country97+       
          importShock2Country97+        importShockCountry98+         importShock1Country98+       
          importShock2Country98+        importShockCountry99+         importShock1Country99+       
          importShock2Country99+        importShockCountry100+        importShock1Country100+      
          importShock2Country100+       importShockCountry101+        importShock1Country101+      
          importShock2Country101+       importShockCountry102+        importShock1Country102+      
          importShock2Country102+       importShockCountry103+        importShock1Country103+      
          importShock2Country103+       importShockCountry104+        importShock1Country104+      
          importShock2Country104+       importShockCountry105+        importShock1Country105+      
          importShock2Country105+       importShockCountry106+        importShock1Country106+      
          importShock2Country106+       importShockCountry107+        importShock1Country107+      
          importShock2Country107+       importShockCountry108+        importShock1Country108+      
          importShock2Country108+       importShockCountry109+        importShock1Country109+      
          importShock2Country109+       importShockCountry110+        importShock1Country110+      
          importShock2Country110+       importShockCountry111+        importShock1Country111+      
          importShock2Country111+       importShockCountry112+        importShock1Country112+      
          importShock2Country112+       importShockCountry113+        importShock1Country113+      
          importShock2Country113+       importShockCountry114+        importShock1Country114+      
          importShock2Country114+       importShockCountry115+        importShock1Country115+      
          importShock2Country115+       importShockCountry116+        importShock1Country116+      
          importShock2Country116+       importShockCountry117+        importShock1Country117+      
          importShock2Country117+       importShockCountry118+        importShock1Country118+      
          importShock2Country118 + factor(ccode) + factor(year) + factor(ccode)*year, data=df)
summary(m2)
vcov_firm <- cluster.vcov(m2, df$ccode)
d1 = coeftest(m2, vcov_firm)
texreg(list(c1, d1), omit.coef="ccode", caption="All", stars=c(0.01, 0.05, 0.1)) #Formation and conflict
texreg(list(m1, m2), omit.coef="ccode", caption="All", stars=c(0.01, 0.05, 0.1)) #Formation and conflict

texreg(list(c1), omit.coef="ccode", caption="All", stars=c(0.01, 0.05, 0.1)) #Formation and conflict
texreg(list(m1), omit.coef="ccode", caption="All", stars=c(0.01, 0.05, 0.1)) #Formation and conflict



yhat = predict(m1, na.action = na.exclude)
resid=residuals(m1, na.action=na.exclude)
plot(resid)
df$resid = resid
hist(df$resid)
df$yhat=yhat

#transform unweighted 
df$pctdiff = (exp(df$rpgr_npi_p) - 1)*100

#get yhat for selected shocks

dd=subset(df, df$ccode==155 & df$year==2006)
dd #copper shock
dd=subset(df, df$ccode==645 & df$year==1973)
dd #oil shock
dd=subset(df, df$ccode==475 & (df$year==1998 | df$year==1997 | df$year==1996))
dd #oil shock
dd=subset(df, df$ccode==640 & df$year==1976)
dd #wheat shock
dd=subset(df, df$ccode==91 &  df$year==1989 )
dd #coffee shock
dd=subset(df, df$ccode==850 & df$year==1999)
dd #asia financial crisis
dd=subset(df, df$ccode==437 & df$year==1991)
dd #asia financial crisis
dd=subset(df, df$ccode==437 & (df$year==1990 | df$year==1991))
dd #asia financial crisis

require(ggplot2)
pdf("predprob.pdf", 8, 6)
ggplot(data=df) + geom_smooth(aes(x=pctdiff, y=yhat)) + xlim(-85, 105) + 
  annotate("text", x = -17.8, y=0.195, label = "Copper Shock \n (Peru, 1973-1974)") + 
  annotate("text", x = 22, y=0.105, label = "Copper Shock \n (Chile, 2005-2006)") + 
  geom_point(aes(x=3.44, y=0.1065)) +
  annotate("text", x = -27, y = 0.118, label = "Oil Shock \n (Saudi Arabia, 1985-1986)") +
  geom_point(aes(x=-10.98, y = 0.12))  + 
  annotate("text", x = 65, y = 0.107, label = "Oil Shock \n (Iraq, 1972-1973)") + 
  annotate("text", x = -35.78, y=0.156, label = "Coffee Shock\n(Honduras, 1988-1989)") +
  annotate("text", x = 28, y=0.01, label = "Coffee Shock\n(Vietnam, 1993-1994)") + 
 # annotate("text", x = -27, y=0.358, label = "Coffee Shock \n(Indonesia, 1997-1998)")  +
  annotate("text", x = -20, y=0.235, label = "Cocoa Shock \n(Cote D'Ivoire, 1990-1991)") +
  geom_vline(aes(xintercept = 0), lty="dashed")   + 
  geom_point(aes(x= -35.78, y=0.146)) +
    geom_point(aes(x=-16.78, y=0.246))+
  geom_point(aes(x=-18.66, y=0.209)) +
  geom_point(aes(x=6.94, y=0.012)) +
  #geom_point(aes(x=12.72, y=0.3307)) + 
  #geom_point(aes(x=-10.94, y=0.208))  + 
#  geom_point(aes(x=-26.51, y=0.383)) + 
  #geom_point(aes(x=29, y=0.09)) + 
  geom_point(aes(x=49, y = 0.11)) +
  theme_bw() +
  geom_vline(aes(xintercept = 0), lty="dashed") + ylab(expression(hat("Pr(Mobilization)"))) + xlab("Annual Commodity Price Index Difference (%)") 
dev.off()

##########SECOND STAGE: CIVIL CONFLICT ONSET ##############
mean(df$onset_any_prio, na.rm=T)
mean(df$any_prio, na.rm=T)
m0 = lm(onset_any_prio ~ pshock_npi_p  + pshock_npi_p1 + pshock_npi_p2 + factor(ccode) + factor(year) + factor(ccode)*year, data=df)
summary(m0)
vcov_firm <- cluster.vcov(m0, df$ccode)
a0 = coeftest(m0, vcov_firm)

m1 = lm(onset_any_prio ~ pshock_npi_p  + pshock_npi_p1 + pshock_npi_p2 + log(l1noactform+1) + factor(ccode) + factor(year) + factor(ccode)*year, data=df)
summary(m1)
vcov_firm <- cluster.vcov(m1, df$ccode)
a1 = coeftest(m1, vcov_firm)

m2 = lm(onset_any_prio ~ pshock_npi_p*log(l1noactform+1)  + pshock_npi_p1 + pshock_npi_p2 + factor(ccode) + factor(year) + factor(ccode)*year, data=df)
summary(m2)
vcov_firm <- cluster.vcov(m2, df$ccode)
a2 = coeftest(m2, vcov_firm)

m3 = lm(onset_war_prio_bb ~ pshock_npi_p  + pshock_npi_p1 + pshock_npi_p2 + factor(ccode) + factor(year) + factor(ccode)*year, data=df)
summary(m3)
vcov_firm <- cluster.vcov(m3, df$ccode)
a3 = coeftest(m3, vcov_firm)

m4 = lm(onset_war_prio_bb ~ pshock_npi_p  + pshock_npi_p1 + pshock_npi_p2 + log(l1noactform+1) + factor(ccode) + factor(year) + factor(ccode)*year, data=df)
summary(m4)
vcov_firm <- cluster.vcov(m4, df$ccode)
a4 = coeftest(m4, vcov_firm)

m5 = lm(onset_war_prio_bb ~ pshock_npi_p*log(l1noactform+1)  + pshock_npi_p1 + pshock_npi_p2 + factor(ccode) + factor(year) + factor(ccode)*year, data=df)
summary(m5)
vcov_firm <- cluster.vcov(m5, df$ccode)
a5 = coeftest(m5, vcov_firm)

texreg(list(a0, a1, a2, a3, a4, a5), omit.coef="ccode", caption="Onset and Interaction", stars=c(0.01, 0.05, 0.1)) #Conflict
texreg(list(m0, m1, m2, m3, m4, m5), omit.coef="ccode", caption="All", stars=c(0.01, 0.05, 0.1)) #Formation and conflict

#Visualize the results using coefplot
require(sjPlot)
require(ggplot2)
set_theme(base = theme_bw())
m.labels=c("1", "2", "3")

plot_model(m2,
            #m.labels=c("New Campaign", "New Conflict"), 
            legend.title=c("Model"),
            rm.terms=c("factor(ccode) [2:900]", "factor(year)"),
            terms = c("pshock_npi_p", "pshock_npi_p1", "pshock_npi_p2", "log(l1noactform + 1)")) + ylim(-0.12, 0.12)

df$pricechange=df$pshock_npi_p
df$pricelag1 = df$pshock_npi_p1
df$priceback2 = df$pshock_annual_npi_p2
df$lnmilitants = log(df$l1noactform+1)

m1 = lm(onset_any_prio ~ pricechange*lnmilitants + pricelag1 + priceback2 + factor(ccode) + factor(year) + factor(ccode)*year, data=df)
summary(m1)

library(interplot)

int1 = interplot(m = m1, var1 = "pricechange", var2 = "lnmilitants") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(plot.title = element_text(hjust = 0.5)) + ggtitle("Interaction Effect of Price Change by Number of Militants \n on Civil Conflict (25-battle deaths)") + 
  theme_bw() + xlab(expression("Ln(NumMilitants)"[t-1])) + ylab("Estimated Coefficient for\nPrice Change") +
  theme(plot.title = element_text(hjust = 0.5)) 
int1


#Higher Civil War Threshold
m2 = lm(onset_war_prio_bb ~ pricechange*lnmilitants + pricelag1 + priceback2 + factor(ccode) + factor(year) + factor(ccode)*year, data=df)
summary(m2)
int2 = interplot(m = m2, var1 = "pricechange", var2 = "lnmilitants") +
  geom_hline(yintercept = 0, linetype = "dashed")+ ggtitle("Interaction Effect of Price Change by Number of Militants \n on Civil War (1000-battle deaths)") + 
  theme_bw() + xlab(expression("Ln(NumMilitants)"[t-1])) + ylab("Estimated Coefficient for\nPrice Change")  +
  theme(plot.title = element_text(hjust = 0.5)) 

require(gridExtra)
grid.arrange(int1, int2, ncol=2)

pdf("interaction.pdf", 12, 5)
grid.arrange(int1, int2, ncol=2)
dev.off()