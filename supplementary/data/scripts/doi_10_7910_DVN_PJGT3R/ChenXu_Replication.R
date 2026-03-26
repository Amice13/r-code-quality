#### Replication codes for "Partners with Benefits: When Multinational Corporations Succeed in Authoritarian Courts."
#### By Frederick R. Chen & Jian Xu

#### Results produced by R version 4.2.0 on Windows 11 Pro

#### Notes:

# plajv: plaintiff is JV
# plasoejv: plaintiff is SOE JV
# defsoejv: defendant is SOE JV
# plapc: the plaintiff has political connections 
# plapc2: the plaintiff has political connections, excluding previous work experience in the military, SOEs, and corporate social responsibility activities. 
# defpc: the defendant has political connections
# plapjv: plaintiff is private JV
# plahq: headquarter country of the plaintiff 
# defhq: headquarter country of the defendant 
# plaregister: country of registration of the plaintiff 
# defregister: country of registration of the defendant 
# plaforeign: plaintiff is a foreign entity
# plafvc: foreign plaintiff and domestic (co-)defendant
# plafvf: foreign plaintiff and foreign (co-)defendant
# placvf: domestic plaintiff and foreign (co-)defendant

# win1: favorable judgement
# win2: plaintiff pays lower court fee
# win3: plaintiff receives positive compensation
# win4: plaintiff's compensation is >1/4 of claimed amount
# win5: plaintiff's compensation is >1/2 of claimed amount
# win6: plaintiff's compensation is >= full amount of claimed amount
# province: the province of the adjudicating court
# ruleyear: ruling year
# ruleprocedure: ruling procedure
# plaind: the plaintiff's industry of operation
# casetype: the type of case
# yearchina: the plaintiff's year of operation in China
# placaptial: the plaintiff's registered capital
# plapublic: the plaintiff is a public-listed company
# resind: the restricted industries of China based on the investment guidance catalog



#### Tables
# texreg() reproduces the full table for the group of models run above

# different tables are separated by _____________


#####Figure 1#####

rm(list=ls())
load("~/jvnums.rdata")

par(mar = c(4, 4, .7,.1))  
plot(jvnums$year, jvnums$jv,col="black", ylim = c(0,35000), type = "l", xaxt = "n",xlab="Year", ylab="Number of Foreign Investment Projects",lwd=2,lty=1)
axis(1,at = jvnums$year,labels =  c(2002:2017),las=2)
points(jvnums$year,jvnums$wfo,col="black",  type="l",lty=2,lwd=2)
legend(2008, 36000,bty = "n",legend=c("Wholly foreign-owned projects","JV projects"),
       lty=c(2,1), cex=.8,lwd =c(2,2))

#_____________________________________________________________________________________________________________________________


#####Figure 2#####


load("~/repdata.rdata")
#dat = repdata

library(miceadds)
library(texreg)

hist(dat$fileyear)


#_____________________________________________________________________________________________________________________________

#####Figure 3#####

dat$country =NA
dat$country = ifelse(as.character(dat$plahq) == "日本" | as.character(dat$defhq) == "日本" | as.character(dat$plaregister) == "日本" | as.character(dat$defregister) == "日本","Japan",dat$country)
length(which(dat$country == "Japan"))

dat$country = ifelse(as.character(dat$plahq) == "韩国" | as.character(dat$defhq) == "韩国" | as.character(dat$plaregister) == "韩国" | as.character(dat$defregister) == "韩国","South Korea",dat$country)
length(which(dat$country == "South Korea"))

dat$country = ifelse(as.character(dat$plahq) == "美国" | as.character(dat$defhq) == "美国" | as.character(dat$plaregister) == "美国" | as.character(dat$defregister) == "美国","USA",dat$country)
length(which(dat$country == "USA"))

dat$country = ifelse(as.character(dat$plahq) == "德国" | as.character(dat$defhq) == "德国" | as.character(dat$plaregister) == "德国" | as.character(dat$defregister) == "德国","Germany",dat$country)
length(which(dat$country == "Germany"))

dat$country = ifelse(as.character(dat$plahq) == "法国" | as.character(dat$defhq) == "法国" | as.character(dat$plaregister) == "法国" | as.character(dat$defregister) == "法国","France",dat$country)
length(which(dat$country == "France"))

dat$country = ifelse(as.character(dat$plahq) == "新加坡" | as.character(dat$defhq) == "新加坡" | as.character(dat$plaregister) == "新加坡" | as.character(dat$defregister) == "新加坡","Singapore",dat$country)
length(which(dat$country == "Singapore"))

dat$country = ifelse(as.character(dat$plahq) == "英国" | as.character(dat$defhq) == "英国" | as.character(dat$plaregister) == "英国" | as.character(dat$defregister) == "英国","UK",dat$country)
length(which(dat$country == "UK"))

dat$country = ifelse(as.character(dat$plahq) == "澳大利亚" | as.character(dat$defhq) == "澳大利亚" | as.character(dat$plaregister) == "澳大利亚" | as.character(dat$defregister) == "澳大利亚","Australia",dat$country)
length(which(dat$country == "Australia"))

par(mar = c(2.5, 4, .4,0))  
barplot(table(dat$country),ylab = "Number of cases",xaxt = "n",ylim = c(0,1500))
text(x = seq(1, 9.4, by = 1.2),
     y = par("usr")[3] - 45,
     labels =  c("Australia","France","Germany","Japan","Singapore","South Korea","U.K.","U.S."),
     xpd = NA,
     srt = 25,
     adj = .9,
     cex = .8)


#_____________________________________________________________________________________________________________________________


##### Table 1 #####
## F v. All ##
summary(subset(dat,  plaforeign==1  )$win1) #0.547
summary(subset(dat,  plaforeign==1  )$win2) #0.237
summary(subset(dat,  plaforeign==1  )$win3) #0.313
summary(subset(dat,  plaforeign==1  )$win4) #0.210
summary(subset(dat,  plaforeign==1  )$win5) #0.182
summary(subset(dat,  plaforeign==1  )$win6) #0.119

dim(subset(dat, plaforeign==1  ))[1] #2356


## F.v.D.
summary(subset(dat,  plafvc==1  )$win1) #0.534
summary(subset(dat,  plafvc==1  )$win2) #0.241
summary(subset(dat,  plafvc==1  )$win3) #0.315
summary(subset(dat,  plafvc==1  )$win4) #0.206
summary(subset(dat,  plafvc==1  )$win5) #0.176
summary(subset(dat,  plafvc==1  )$win6) #0.115

dim(subset(dat, plafvc==1  ))[1] #2050


## F.v.F
summary(subset(dat,  plafvf==1  )$win1) #0.643
summary(subset(dat,  plafvf==1  )$win2) #0.200
summary(subset(dat,  plafvf==1  )$win3) #0.288
summary(subset(dat,  plafvf==1  )$win4) #0.225
summary(subset(dat,  plafvf==1  )$win5) #0.206
summary(subset(dat,  plafvf==1  )$win6) #0.138

dim(subset(dat, plafvf==1  ))[1] #273


## D.v.F
summary(subset(dat,  placvf==1  )$win1) #0.467
summary(subset(dat,  placvf==1  )$win2) #0.113
summary(subset(dat,  placvf==1  )$win3) #0.231
summary(subset(dat,  placvf==1  )$win4) #0.175
summary(subset(dat,  placvf==1  )$win5) #0.141
summary(subset(dat,  placvf==1  )$win6) #0.094

dim(subset(dat, placvf==1  ))[1] #1386


#_____________________________________________________________________________________________________________________________


##### Table 2 #######

## All MNCs

t.test(subset(dat,plafvc==1)$win1,subset(dat,plafvc!=1)$win1) #0.533, p-value = 0.020
t.test(subset(dat,plafvc==1)$win2,subset(dat,plafvc!=1)$win2) #0.241, p-value = 0.000
t.test(subset(dat,plafvc==1)$win3,subset(dat,plafvc!=1)$win3) #0.315, p-value = 0.000
t.test(subset(dat,plafvc==1)$win4,subset(dat,plafvc!=1)$win4) #0.206, p-value = 0.180
t.test(subset(dat,plafvc==1)$win5,subset(dat,plafvc!=1)$win5) #0.176  p-value = 0.123
t.test(subset(dat,plafvc==1)$win6,subset(dat,plafvc!=1)$win6) #0.115  p-value = 0.276

options(scipen = 999)
summary(as.numeric(as.character(subset(dat, plaforeign==1)$placlaim))) #average claimed amount: 10,554,952
dim(subset(dat, plafvc==1  ))[1] #2050


## All JVs
t.test(subset(dat,plajv==1 & plafvc==1 )$win1, subset(dat, plajv !=1  )$win1) #0.642, p-value = 0.020
t.test(subset(dat,plajv==1 & plafvc==1 )$win2, subset(dat, plajv !=1  )$win2) #0.642, p-value = 0.020
t.test(subset(dat,plajv==1 & plafvc==1 )$win3, subset(dat, plajv !=1  )$win3) #0.288, p-value = 0.6854
t.test(subset(dat,plajv==1 & plafvc==1 )$win4, subset(dat, plajv !=1  )$win4) #0.188, p-value = 0.803
t.test(subset(dat,plajv==1 & plafvc==1 )$win5, subset(dat, plajv !=1  )$win5) #0.167, p-value = 0.826
t.test(subset(dat,plajv==1 & plafvc==1 )$win6, subset(dat, plajv !=1  )$win6) #0.111, p-value = 0.774

summary(as.numeric(as.character(subset(dat, plajv==1)$placlaim))) #average claimed amount: 11,546,579
dim(subset(dat,plajv==1 & plafvc==1 ))[1] #417


## SOE JVs
t.test(subset(dat,plasoejv==1 & plafvc==1 )$win1, subset(dat, plasoejv !=1  )$win1) #0.619, p-value = 0.024
t.test(subset(dat,plasoejv==1 & plafvc==1 )$win2, subset(dat, plasoejv !=1  )$win2) #0.426, p-value = 0.001
t.test(subset(dat,plasoejv==1 & plafvc==1 )$win3, subset(dat, plasoejv !=1  )$win3) #0.591, p-value = 0.000
t.test(subset(dat,plasoejv==1 & plafvc==1 )$win4, subset(dat, plasoejv !=1  )$win4) #0.500, p-value = 0.000
t.test(subset(dat,plasoejv==1 & plafvc==1 )$win5, subset(dat, plasoejv !=1  )$win5) #0.476, p-value = 0.000
t.test(subset(dat,plasoejv==1 & plafvc==1 )$win6, subset(dat, plasoejv !=1  )$win6) #0.310, p-value = 0.007

summary(as.numeric(as.character(subset(dat, plasoejv==1)$placlaim))) #average claimed amount: 13,654,363
dim(subset(dat,plasoejv==1 & plafvc==1 ))[1] #113


## Private JVs
t.test(subset(dat,plapjv==1 & plafvc==1 )$win1, subset(dat, plapjv !=1  )$win1) #0.652, p-value = 0.000
t.test(subset(dat,plapjv==1 & plafvc==1 )$win2, subset(dat, plapjv !=1  )$win2) #0.652, p-value = 0.138
t.test(subset(dat,plapjv==1 & plafvc==1 )$win3, subset(dat, plapjv !=1  )$win3) #0.231, p-value = 0.069
t.test(subset(dat,plapjv==1 & plafvc==1 )$win4, subset(dat, plapjv !=1  )$win4) #0.135, p-value = 0.005
t.test(subset(dat,plapjv==1 & plafvc==1 )$win5, subset(dat, plapjv !=1  )$win5) #0.114, p-value = 0.013
t.test(subset(dat,plapjv==1 & plafvc==1 )$win6, subset(dat, plapjv !=1  )$win6) #0.078, p-value = 0.075

summary(as.numeric(as.character(subset(dat, plapjv==1)$placlaim))) #average claimed amount: 10,802,223
dim(subset(dat,plapjv==1 & plafvc==1 ))[1] #300


## Connected MNCs
t.test(subset(dat,plapc==1 & plafvc==1 )$win1, subset(dat, plapc !=1  )$win1) #0.652, p-value = 0.000
t.test(subset(dat,plapc==1 & plafvc==1 )$win2, subset(dat, plapc !=1  )$win2) #0.256, p-value = 0.006
t.test(subset(dat,plapc==1 & plafvc==1 )$win3, subset(dat, plapc !=1  )$win3) #0.282, p-value = 0.643
t.test(subset(dat,plapc==1 & plafvc==1 )$win4, subset(dat, plapc !=1  )$win4) #0.179, p-value = 0.676
t.test(subset(dat,plapc==1 & plafvc==1 )$win5, subset(dat, plapc !=1  )$win5) #0.157, p-value = 0.723
t.test(subset(dat,plapc==1 & plafvc==1 )$win6, subset(dat, plapc !=1  )$win6) #0.104, p-value = 0.917

summary(as.numeric(as.character(subset(dat, plapc==1)$placlaim))) #average claimed amount: 8,070,449
dim(subset(dat,plapc==1 & plafvc==1 ))[1] #404

#_____________________________________________________________________________________________________________________________


###### Table 3 ######

#Note: regression results to be produced by the texreg function


### political partnership mechanism
# Judgement (win1)
basicm1 = glm.cluster(win1 ~  plasoejv   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())


# Court fee (win2)
basicm2 = glm.cluster(win2 ~  plasoejv   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())


# Comp > 0 (win3)
basicm3 = glm.cluster(win3 ~  plasoejv   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())


# Comp > 1/4 (win4)
basicm4 = glm.cluster(win4 ~  plasoejv   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())


# Comp > 1/2 (win5)
basicm5 = glm.cluster(win5 ~  plasoejv   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())


# Comp >= full (win6)
basicm6 = glm.cluster(win6 ~  plasoejv   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())


## SOE JV
texreg(list(basicm1$glm_res,basicm2$glm_res,basicm3$glm_res,basicm4$glm_res,basicm5$glm_res,basicm6$glm_res),
       override.se = list(summary(basicm1)[,2],summary(basicm2)[,2],summary(basicm3)[,2],summary(basicm4)[,2],summary(basicm5)[,2],summary(basicm6)[,2]),
       override.pvalues = list(summary(basicm1)[,4],summary(basicm2)[,4],summary(basicm3)[,4],summary(basicm4)[,4],summary(basicm5)[,4],summary(basicm6)[,4]),digits=3,
       stars = c(0.001, 0.01, 0.05,0.1))



### political connections mechanism
# Judgement (win1)
basicm1 = glm.cluster(win1 ~  plapc   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())


# Court fee (win2)
basicm2 = glm.cluster(win2 ~  plapc   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())


# Comp > 0 (win3)
basicm3 = glm.cluster(win3 ~  plapc   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())


# Comp > 1/4 (win4)
basicm4 = glm.cluster(win4 ~  plapc   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())


# Comp > 1/2 (win5)
basicm5 = glm.cluster(win5 ~  plapc   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())


# Comp >= full (win6)
basicm6 = glm.cluster(win6 ~  plapc   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())


## political connections
texreg(list(basicm1$glm_res,basicm2$glm_res,basicm3$glm_res,basicm4$glm_res,basicm5$glm_res,basicm6$glm_res),
       override.se = list(summary(basicm1)[,2],summary(basicm2)[,2],summary(basicm3)[,2],summary(basicm4)[,2],summary(basicm5)[,2],summary(basicm6)[,2]),
       override.pvalues = list(summary(basicm1)[,4],summary(basicm2)[,4],summary(basicm3)[,4],summary(basicm4)[,4],summary(basicm5)[,4],summary(basicm6)[,4]),digits=3,
       stars = c(0.001, 0.01, 0.05,0.1))




### political connections mechanism (narrow)
# Judgement (win1)
basicm1 = glm.cluster(win1 ~  plapc2   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())

# Court fee (win2)
basicm2 = glm.cluster(win2 ~  plapc2   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())


# Comp > 0 (win3)
basicm3 = glm.cluster(win3 ~  plapc2   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())


# Comp > 1/4 (win4)
basicm4 = glm.cluster(win4 ~  plapc2   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())


# Comp > 1/2 (win5)
basicm5 = glm.cluster(win5 ~  plapc2   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())


# Comp >= full (win6)
basicm6 = glm.cluster(win6 ~  plapc2   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())



## political connections (narrow)
texreg(list(basicm1$glm_res,basicm2$glm_res,basicm3$glm_res,basicm4$glm_res,basicm5$glm_res,basicm6$glm_res),
       override.se = list(summary(basicm1)[,2],summary(basicm2)[,2],summary(basicm3)[,2],summary(basicm4)[,2],summary(basicm5)[,2],summary(basicm6)[,2]),
       override.pvalues = list(summary(basicm1)[,4],summary(basicm2)[,4],summary(basicm3)[,4],summary(basicm4)[,4],summary(basicm5)[,4],summary(basicm6)[,4]),digits=3,
       stars = c(0.001, 0.01, 0.05,0.1))



### political partnership beyond political connections
# Judgement (win1)
basicm1 = glm.cluster(win1 ~  plasoejv + plapc   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())

# Court fee (win2)
basicm2 = glm.cluster(win2 ~  plasoejv + plapc    + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())


# Comp > 0 (win3)
basicm3 = glm.cluster(win3 ~  plasoejv + plapc    + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())


# Comp > 1/4 (win4)
basicm4 = glm.cluster(win4 ~  plasoejv + plapc    + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())


# Comp > 1/2 (win5)
basicm5 = glm.cluster(win5 ~  plasoejv + plapc    + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())


# Comp >= full (win6)
basicm6 = glm.cluster(win6 ~  plasoejv + plapc    + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())


## SOE JV + political connections 
texreg(list(basicm1$glm_res,basicm2$glm_res,basicm3$glm_res,basicm4$glm_res,basicm5$glm_res,basicm6$glm_res),
       override.se = list(summary(basicm1)[,2],summary(basicm2)[,2],summary(basicm3)[,2],summary(basicm4)[,2],summary(basicm5)[,2],summary(basicm6)[,2]),
       override.pvalues = list(summary(basicm1)[,4],summary(basicm2)[,4],summary(basicm3)[,4],summary(basicm4)[,4],summary(basicm5)[,4],summary(basicm6)[,4]),digits=3,
       stars = c(0.001, 0.01, 0.05,0.1))



### Discrimination against private partnerships
# Judgement (win1)
basicm1 = glm.cluster(win1 ~  plapjv    + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())


# Court fee (win2)
basicm2 = glm.cluster(win2 ~  plapjv    + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())


# Comp > 0 (win3)
basicm3 = glm.cluster(win3 ~ plapjv  + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())


# Comp > 1/4 (win4)
basicm4 = glm.cluster(win4 ~  plapjv   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())


# Comp > 1/2 (win5)
basicm5 = glm.cluster(win5 ~  plapjv   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())


# Comp >= full (win6)
basicm6 = glm.cluster(win6 ~ plapjv   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())


## Private JV
texreg(list(basicm1$glm_res,basicm2$glm_res,basicm3$glm_res,basicm4$glm_res,basicm5$glm_res,basicm6$glm_res),
       override.se = list(summary(basicm1)[,2],summary(basicm2)[,2],summary(basicm3)[,2],summary(basicm4)[,2],summary(basicm5)[,2],summary(basicm6)[,2]),
       override.pvalues = list(summary(basicm1)[,4],summary(basicm2)[,4],summary(basicm3)[,4],summary(basicm4)[,4],summary(basicm5)[,4],summary(basicm6)[,4]),digits=3,
       stars = c(0.001, 0.01, 0.05,0.1))


#_____________________________________________________________________________________________________________________________


######## Table 4 ##########

#### Panel (1)
basicm1 = glm.cluster(win1 ~  as.factor(plasoejv)   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      + as.factor(plafvc)  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())

nd0 = as.data.frame(cbind(plahq="日本",province="北京",ruleyear ="2017",ruleprocedure = 1, casetype="民事案件",
                          plaind ="C",plafvc =1,plasoejv=0))
nd1 = as.data.frame(cbind(plahq="日本",province="北京",ruleyear ="2017",ruleprocedure = 1, casetype="民事案件",
                          plaind ="C",plafvc =1,plasoejv=1))
predict.glm(basicm1$glm_res,newdata = nd1,type = "response") - predict.glm(basicm1$glm_res,newdata = nd0,type = "response")

#Judgement: + 0.106


basicm1 = glm.cluster(win2 ~  as.factor(plasoejv)   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      + as.factor(plafvc)  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())

nd0 = as.data.frame(cbind(plahq="日本",province="北京",ruleyear ="2017",ruleprocedure = 1, casetype="民事案件",
                          plaind ="C",plafvc =1,plasoejv=0))
nd1 = as.data.frame(cbind(plahq="日本",province="北京",ruleyear ="2017",ruleprocedure = 1, casetype="民事案件",
                          plaind ="C",plafvc =1,plasoejv=1))
predict.glm(basicm1$glm_res,newdata = nd1,type = "response") - predict.glm(basicm1$glm_res,newdata = nd0,type = "response")

#Lower fee: + 0.156


basicm1 = glm.cluster(win3 ~  as.factor(plasoejv)   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      + as.factor(plafvc)  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())

nd0 = as.data.frame(cbind(plahq="日本",province="北京",ruleyear ="2017",ruleprocedure = 1, casetype="民事案件",
                          plaind ="C",plafvc =1,plasoejv=0))
nd1 = as.data.frame(cbind(plahq="日本",province="北京",ruleyear ="2017",ruleprocedure = 1, casetype="民事案件",
                          plaind ="C",plafvc =1,plasoejv=1))
predict.glm(basicm1$glm_res,newdata = nd1,type = "response") - predict.glm(basicm1$glm_res,newdata = nd0,type = "response")

#Comp > 0: + 0.115


basicm1 = glm.cluster(win4 ~  as.factor(plasoejv)   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      + as.factor(plafvc)  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())

nd0 = as.data.frame(cbind(plahq="日本",province="北京",ruleyear ="2017",ruleprocedure = 1, casetype="民事案件",
                          plaind ="C",plafvc =1,plasoejv=0))
nd1 = as.data.frame(cbind(plahq="日本",province="北京",ruleyear ="2017",ruleprocedure = 1, casetype="民事案件",
                          plaind ="C",plafvc =1,plasoejv=1))
predict.glm(basicm1$glm_res,newdata = nd1,type = "response") - predict.glm(basicm1$glm_res,newdata = nd0,type = "response")

#Comp > 1/4: + 0.168


basicm1 = glm.cluster(win5 ~  as.factor(plasoejv)   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      + as.factor(plafvc)  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())

nd0 = as.data.frame(cbind(plahq="日本",province="北京",ruleyear ="2017",ruleprocedure = 1, casetype="民事案件",
                          plaind ="C",plafvc =1,plasoejv=0))
nd1 = as.data.frame(cbind(plahq="日本",province="北京",ruleyear ="2017",ruleprocedure = 1, casetype="民事案件",
                          plaind ="C",plafvc =1,plasoejv=1))
predict.glm(basicm1$glm_res,newdata = nd1,type = "response") - predict.glm(basicm1$glm_res,newdata = nd0,type = "response")

#Comp > 1/2: + 0.167


basicm1 = glm.cluster(win6 ~  as.factor(plasoejv)   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      + as.factor(plafvc)  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())

nd0 = as.data.frame(cbind(plahq="日本",province="北京",ruleyear ="2017",ruleprocedure = 1, casetype="民事案件",
                          plaind ="C",plafvc =1,plasoejv=0))
nd1 = as.data.frame(cbind(plahq="日本",province="北京",ruleyear ="2017",ruleprocedure = 1, casetype="民事案件",
                          plaind ="C",plafvc =1,plasoejv=1))
predict.glm(basicm1$glm_res,newdata = nd1,type = "response") - predict.glm(basicm1$glm_res,newdata = nd0,type = "response")

#Comp >= full: + 0.072


#### Panel (3)
basicm1 = glm.cluster(win1 ~  as.factor(plasoejv)  + as.factor(plapc)  + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      + as.factor(plafvc)  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())

nd0 = as.data.frame(cbind(plahq="日本",province="北京",ruleyear ="2017",ruleprocedure = 1, casetype="民事案件",
                          plaind ="C",plafvc =1,plasoejv=0,plapc =0))
nd1 = as.data.frame(cbind(plahq="日本",province="北京",ruleyear ="2017",ruleprocedure = 1, casetype="民事案件",
                          plaind ="C",plafvc =1,plasoejv=1,plapc =0))
predict.glm(basicm1$glm_res,newdata = nd1,type = "response") - predict.glm(basicm1$glm_res,newdata = nd0,type = "response")

#Judgement: + 0.072


basicm1 = glm.cluster(win2 ~  as.factor(plasoejv)  + as.factor(plapc)  + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      + as.factor(plafvc)  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())

nd0 = as.data.frame(cbind(plahq="日本",province="北京",ruleyear ="2017",ruleprocedure = 1, casetype="民事案件",
                          plaind ="C",plafvc =1,plasoejv=0,plapc =0))
nd1 = as.data.frame(cbind(plahq="日本",province="北京",ruleyear ="2017",ruleprocedure = 1, casetype="民事案件",
                          plaind ="C",plafvc =1,plasoejv=1,plapc =0))
predict.glm(basicm1$glm_res,newdata = nd1,type = "response") - predict.glm(basicm1$glm_res,newdata = nd0,type = "response")

#Lower fee: + 0.147


basicm1 = glm.cluster(win3 ~  as.factor(plasoejv)  + as.factor(plapc)  + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      + as.factor(plafvc)  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())

nd0 = as.data.frame(cbind(plahq="日本",province="北京",ruleyear ="2017",ruleprocedure = 1, casetype="民事案件",
                          plaind ="C",plafvc =1,plasoejv=0,plapc =0))
nd1 = as.data.frame(cbind(plahq="日本",province="北京",ruleyear ="2017",ruleprocedure = 1, casetype="民事案件",
                          plaind ="C",plafvc =1,plasoejv=1,plapc =0))
predict.glm(basicm1$glm_res,newdata = nd1,type = "response") - predict.glm(basicm1$glm_res,newdata = nd0,type = "response")

#Comp > 0: + 0.113


basicm1 = glm.cluster(win4 ~  as.factor(plasoejv)  + as.factor(plapc)  + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      + as.factor(plafvc)  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())

nd0 = as.data.frame(cbind(plahq="日本",province="北京",ruleyear ="2017",ruleprocedure = 1, casetype="民事案件",
                          plaind ="C",plafvc =1,plasoejv=0,plapc =0))
nd1 = as.data.frame(cbind(plahq="日本",province="北京",ruleyear ="2017",ruleprocedure = 1, casetype="民事案件",
                          plaind ="C",plafvc =1,plasoejv=1,plapc =0))
predict.glm(basicm1$glm_res,newdata = nd1,type = "response") - predict.glm(basicm1$glm_res,newdata = nd0,type = "response")

#Comp>1/4: + 0.182


basicm1 = glm.cluster(win5 ~  as.factor(plasoejv)  + as.factor(plapc)  + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      + as.factor(plafvc)  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())

nd0 = as.data.frame(cbind(plahq="日本",province="北京",ruleyear ="2017",ruleprocedure = 1, casetype="民事案件",
                          plaind ="C",plafvc =1,plasoejv=0,plapc =0))
nd1 = as.data.frame(cbind(plahq="日本",province="北京",ruleyear ="2017",ruleprocedure = 1, casetype="民事案件",
                          plaind ="C",plafvc =1,plasoejv=1,plapc =0))
predict.glm(basicm1$glm_res,newdata = nd1,type = "response") - predict.glm(basicm1$glm_res,newdata = nd0,type = "response")

#Comp > 1/2: + 0.202


basicm1 = glm.cluster(win6 ~  as.factor(plasoejv)  + as.factor(plapc)  + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      + as.factor(plafvc)  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())

nd0 = as.data.frame(cbind(plahq="日本",province="北京",ruleyear ="2017",ruleprocedure = 1, casetype="民事案件",
                          plaind ="C",plafvc =1,plasoejv=0,plapc =0))
nd1 = as.data.frame(cbind(plahq="日本",province="北京",ruleyear ="2017",ruleprocedure = 1, casetype="民事案件",
                          plaind ="C",plafvc =1,plasoejv=1,plapc =0))
predict.glm(basicm1$glm_res,newdata = nd1,type = "response") - predict.glm(basicm1$glm_res,newdata = nd0,type = "response")

#Comp >= full: + 0.089



#### General PC
basicm1 = glm.cluster(win1 ~  as.factor(plapc)   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      + as.factor(plafvc)  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())

nd0 = as.data.frame(cbind(plahq="日本",province="北京",ruleyear ="2017",ruleprocedure = 1, casetype="民事案件",
                          plaind ="C",plafvc =1,plapc=0))
nd1 = as.data.frame(cbind(plahq="日本",province="北京",ruleyear ="2017",ruleprocedure = 1, casetype="民事案件",
                          plaind ="C",plafvc =1,plapc=1))
predict.glm(basicm1$glm_res,newdata = nd1,type = "response") - predict.glm(basicm1$glm_res,newdata = nd0,type = "response")

#Judgement: + 0.074


basicm1 = glm.cluster(win2 ~  as.factor(plapc)   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      + as.factor(plafvc)  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())

nd0 = as.data.frame(cbind(plahq="日本",province="北京",ruleyear ="2017",ruleprocedure = 1, casetype="民事案件",
                          plaind ="C",plafvc =1,plapc=0))
nd1 = as.data.frame(cbind(plahq="日本",province="北京",ruleyear ="2017",ruleprocedure = 1, casetype="民事案件",
                          plaind ="C",plafvc =1,plapc=1))
predict.glm(basicm1$glm_res,newdata = nd1,type = "response") - predict.glm(basicm1$glm_res,newdata = nd0,type = "response")

#Lower fee: + 0.056


basicm1 = glm.cluster(win3 ~  as.factor(plapc)   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      + as.factor(plafvc)  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())

nd0 = as.data.frame(cbind(plahq="日本",province="北京",ruleyear ="2017",ruleprocedure = 1, casetype="民事案件",
                          plaind ="C",plafvc =1,plapc=0))
nd1 = as.data.frame(cbind(plahq="日本",province="北京",ruleyear ="2017",ruleprocedure = 1, casetype="民事案件",
                          plaind ="C",plafvc =1,plapc=1))
predict.glm(basicm1$glm_res,newdata = nd1,type = "response") - predict.glm(basicm1$glm_res,newdata = nd0,type = "response")

#Comp > 0: + 0.03


basicm1 = glm.cluster(win4 ~  as.factor(plapc)   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      + as.factor(plafvc)  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())

nd0 = as.data.frame(cbind(plahq="日本",province="北京",ruleyear ="2017",ruleprocedure = 1, casetype="民事案件",
                          plaind ="C",plafvc =1,plapc=0))
nd1 = as.data.frame(cbind(plahq="日本",province="北京",ruleyear ="2017",ruleprocedure = 1, casetype="民事案件",
                          plaind ="C",plafvc =1,plapc=1))
predict.glm(basicm1$glm_res,newdata = nd1,type = "response") - predict.glm(basicm1$glm_res,newdata = nd0,type = "response")

#Comp > 1/4:  + 0.013


basicm1 = glm.cluster(win5 ~  as.factor(plapc)   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      + as.factor(plafvc)  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())

nd0 = as.data.frame(cbind(plahq="日本",province="北京",ruleyear ="2017",ruleprocedure = 1, casetype="民事案件",
                          plaind ="C",plafvc =1,plapc=0))
nd1 = as.data.frame(cbind(plahq="日本",province="北京",ruleyear ="2017",ruleprocedure = 1, casetype="民事案件",
                          plaind ="C",plafvc =1,plapc=1))
predict.glm(basicm1$glm_res,newdata = nd1,type = "response") - predict.glm(basicm1$glm_res,newdata = nd0,type = "response")

#Comp > 1/2:  -0.016


basicm1 = glm.cluster(win6 ~  as.factor(plapc)   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      + as.factor(plafvc)  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())

nd0 = as.data.frame(cbind(plahq="日本",province="北京",ruleyear ="2017",ruleprocedure = 1, casetype="民事案件",
                          plaind ="C",plafvc =1,plapc=0))
nd1 = as.data.frame(cbind(plahq="日本",province="北京",ruleyear ="2017",ruleprocedure = 1, casetype="民事案件",
                          plaind ="C",plafvc =1,plapc=1))
predict.glm(basicm1$glm_res,newdata = nd1,type = "response") - predict.glm(basicm1$glm_res,newdata = nd0,type = "response")

#Comp >= full: + 0.006



#### Narrow PC
basicm1 = glm.cluster(win1 ~  as.factor(plapc2)   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      + as.factor(plafvc)  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())

nd0 = as.data.frame(cbind(plahq="日本",province="北京",ruleyear ="2017",ruleprocedure = 1, casetype="民事案件",
                          plaind ="C",plafvc =1,plapc2=0))
nd1 = as.data.frame(cbind(plahq="日本",province="北京",ruleyear ="2017",ruleprocedure = 1, casetype="民事案件",
                          plaind ="C",plafvc =1,plapc2=1))
predict.glm(basicm1$glm_res,newdata = nd1,type = "response") - predict.glm(basicm1$glm_res,newdata = nd0,type = "response")

#Judgement: + 0.052


basicm1 = glm.cluster(win2 ~  as.factor(plapc2)   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      + as.factor(plafvc)  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())

nd0 = as.data.frame(cbind(plahq="日本",province="北京",ruleyear ="2017",ruleprocedure = 1, casetype="民事案件",
                          plaind ="C",plafvc =1,plapc2=0))
nd1 = as.data.frame(cbind(plahq="日本",province="北京",ruleyear ="2017",ruleprocedure = 1, casetype="民事案件",
                          plaind ="C",plafvc =1,plapc2=1))
predict.glm(basicm1$glm_res,newdata = nd1,type = "response") - predict.glm(basicm1$glm_res,newdata = nd0,type = "response")

#Lower fee: + 0.032


basicm1 = glm.cluster(win3 ~  as.factor(plapc2)   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      + as.factor(plafvc)  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())

nd0 = as.data.frame(cbind(plahq="日本",province="北京",ruleyear ="2017",ruleprocedure = 1, casetype="民事案件",
                          plaind ="C",plafvc =1,plapc2=0))
nd1 = as.data.frame(cbind(plahq="日本",province="北京",ruleyear ="2017",ruleprocedure = 1, casetype="民事案件",
                          plaind ="C",plafvc =1,plapc2=1))
predict.glm(basicm1$glm_res,newdata = nd1,type = "response") - predict.glm(basicm1$glm_res,newdata = nd0,type = "response")

#Comp > 0: + 0.174


basicm1 = glm.cluster(win4 ~  as.factor(plapc2)   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      + as.factor(plafvc)  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())

nd0 = as.data.frame(cbind(plahq="日本",province="北京",ruleyear ="2017",ruleprocedure = 1, casetype="民事案件",
                          plaind ="C",plafvc =1,plapc2=0))
nd1 = as.data.frame(cbind(plahq="日本",province="北京",ruleyear ="2017",ruleprocedure = 1, casetype="民事案件",
                          plaind ="C",plafvc =1,plapc2=1))
predict.glm(basicm1$glm_res,newdata = nd1,type = "response") - predict.glm(basicm1$glm_res,newdata = nd0,type = "response")

#Comp > 1/4: + 0.086


basicm1 = glm.cluster(win5 ~  as.factor(plapc2)   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      + as.factor(plafvc)  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())

nd0 = as.data.frame(cbind(plahq="日本",province="北京",ruleyear ="2017",ruleprocedure = 1, casetype="民事案件",
                          plaind ="C",plafvc =1,plapc2=0))
nd1 = as.data.frame(cbind(plahq="日本",province="北京",ruleyear ="2017",ruleprocedure = 1, casetype="民事案件",
                          plaind ="C",plafvc =1,plapc2=1))
predict.glm(basicm1$glm_res,newdata = nd1,type = "response") - predict.glm(basicm1$glm_res,newdata = nd0,type = "response")

#Comp > 1/2: + 0.008


basicm1 = glm.cluster(win6 ~  as.factor(plapc2)   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      + as.factor(plafvc)  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())

nd0 = as.data.frame(cbind(plahq="日本",province="北京",ruleyear ="2017",ruleprocedure = 1, casetype="民事案件",
                          plaind ="C",plafvc =1,plapc2=0))
nd1 = as.data.frame(cbind(plahq="日本",province="北京",ruleyear ="2017",ruleprocedure = 1, casetype="民事案件",
                          plaind ="C",plafvc =1,plapc2=1))
predict.glm(basicm1$glm_res,newdata = nd1,type = "response") - predict.glm(basicm1$glm_res,newdata = nd0,type = "response")

#Comp > 0: + 0.044

#_____________________________________________________________________________________________________________________________



######## Table 5 ###########

## interacting with the defendant's political connections

# Judgement (win1)
basicm1 = glm.cluster(win1 ~  plasoejv*defpc   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())


# Court fee (win2)
basicm2 = glm.cluster(win2 ~   plasoejv*defpc    + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())


# Comp > 0 (win3)
basicm3 = glm.cluster(win3 ~   plasoejv*defpc    + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())


# Comp > 1/4 (win4)
basicm4 = glm.cluster(win4 ~   plasoejv*defpc    + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())


# Comp > 1/2 (win5)
basicm5 = glm.cluster(win5 ~ plasoejv*defpc    + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())


# Comp >= full (win6)
basicm6 = glm.cluster(win6 ~ plasoejv*defpc    + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())


texreg(list(basicm1$glm_res,basicm2$glm_res,basicm3$glm_res,basicm4$glm_res,basicm5$glm_res,basicm6$glm_res),
       override.se = list(summary(basicm1)[,2],summary(basicm2)[,2],summary(basicm3)[,2],summary(basicm4)[,2],summary(basicm5)[,2],summary(basicm6)[,2]),
       override.pvalues = list(summary(basicm1)[,4],summary(basicm2)[,4],summary(basicm3)[,4],summary(basicm4)[,4],summary(basicm5)[,4],summary(basicm6)[,4]),digits=3,
       stars = c(0.001, 0.01, 0.05,0.1))



## interacting with the defendant's political partnership

# Judgement (win1)
basicm1 = glm.cluster(win1 ~  plasoejv*defsoejv   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())


# Court fee (win2)
basicm2 = glm.cluster(win2 ~   plasoejv*defsoejv    + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())


# Comp > 0 (win3)
basicm3 = glm.cluster(win3 ~   plasoejv*defsoejv    + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())


# Comp > 1/4 (win4)
basicm4 = glm.cluster(win4 ~   plasoejv*defsoejv    + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())



# Comp > 1/2 (win5)
basicm5 = glm.cluster(win5 ~ plasoejv*defsoejv    + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())


# Comp >= full (win6)
basicm6 = glm.cluster(win6 ~ plasoejv*defsoejv    + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())


texreg(list(basicm1$glm_res,basicm2$glm_res,basicm3$glm_res,basicm4$glm_res,basicm5$glm_res,basicm6$glm_res),
       override.se = list(summary(basicm1)[,2],summary(basicm2)[,2],summary(basicm3)[,2],summary(basicm4)[,2],summary(basicm5)[,2],summary(basicm6)[,2]),
       override.pvalues = list(summary(basicm1)[,4],summary(basicm2)[,4],summary(basicm3)[,4],summary(basicm4)[,4],summary(basicm5)[,4],summary(basicm6)[,4]),digits=3,
       stars = c(0.001, 0.01, 0.05,0.1))

#_____________________________________________________________________________________________________________________________



############## Table 6 ###############

library(MatchIt)
library(mice)
library(stargazer)

#### political partnership mechanism 

# Judgement
matchdat = dat[c("province","casetype","plahq", "ruleyear","ruleprocedure", "plaind","win1","plasoejv","plapublic")]
matchdat = na.omit(matchdat)

m.out = matchit(plasoejv  ~ as.factor(province) + as.factor(ruleyear)   #+ as.factor(contract)
                #+ as.factor(defind) #+ defpc
                #+ as.factor(ipr)+ as.factor(admin) 
                + as.factor(plapublic)
                # +as.factor(plafvc)
                + as.factor(plahq)
                +as.factor(casetype)
                + as.factor(plaind)
                #+as.factor(courttype)
                +as.factor(ruleprocedure)
                ,data = matchdat,method = "exact")

z.out1 = glm(win1 ~ plasoejv + as.factor(province) + as.factor(ruleyear) #+yearchina
             + as.factor(plapublic)
             + as.factor(plahq)
             +as.factor(casetype)
             #+ as.factor(plafvc)
             + as.factor(plaind) 
             #+as.factor(courttype)
             +as.factor(ruleprocedure)
             ,data=match.data(m.out),weights = weights,family = binomial(link = "logit"))


summary(z.out1)


# Court fee
matchdat = dat[c("province","casetype","plahq", "ruleyear","ruleprocedure", "plaind","win2","plasoejv","plapublic")]
matchdat = na.omit(matchdat)

m.out = matchit(plasoejv  ~ as.factor(province) + as.factor(ruleyear)   #+ as.factor(contract)
                #+ as.factor(defind) #+ defpc
                #+ as.factor(ipr)+ as.factor(admin) 
                + as.factor(plapublic)
                # +as.factor(plafvc)
                + as.factor(plahq)
                +as.factor(casetype)
                + as.factor(plaind)
                #+as.factor(courttype)
                +as.factor(ruleprocedure)
                ,data = matchdat,method = "exact")

z.out2 = glm(win2 ~ plasoejv + as.factor(province) + as.factor(ruleyear) #+yearchina
             + as.factor(plapublic)
             + as.factor(plahq)
             +as.factor(casetype)
             #+ as.factor(plafvc)
             + as.factor(plaind) 
             #+as.factor(courttype)
             +as.factor(ruleprocedure)
             ,data=match.data(m.out),weights = weights,family = binomial(link = "logit"))

summary(z.out2)


# Comp > 0 
matchdat = dat[c("province","casetype","plahq", "ruleyear","ruleprocedure", "plaind","win3","plasoejv","plapublic")]
matchdat = na.omit(matchdat)

m.out = matchit(plasoejv  ~ as.factor(province) + as.factor(ruleyear)   #+ as.factor(contract)
                #+ as.factor(defind) #+ defpc
                #+ as.factor(ipr)+ as.factor(admin) 
                + as.factor(plapublic)
                # +as.factor(plafvc)
                + as.factor(plahq)
                +as.factor(casetype)
                + as.factor(plaind)
                #+as.factor(courttype)
                +as.factor(ruleprocedure)
                ,data = matchdat,method = "exact")

z.out3 = glm(win3 ~ plasoejv + as.factor(province) + as.factor(ruleyear) #+yearchina
             + as.factor(plapublic)
             + as.factor(plahq)
             +as.factor(casetype)
             #+ as.factor(plafvc)
             + as.factor(plaind) 
             #+as.factor(courttype)
             +as.factor(ruleprocedure)
             ,data=match.data(m.out),weights = weights,family = binomial(link = "logit"))

summary(z.out3)


# Comp > 1/4
matchdat = dat[c("province","casetype","plahq", "ruleyear","ruleprocedure", "plaind","win4","plasoejv","plapublic")]
matchdat = na.omit(matchdat)

m.out = matchit(plasoejv  ~ as.factor(province) + as.factor(ruleyear)   #+ as.factor(contract)
                #+ as.factor(defind) #+ defpc
                #+ as.factor(ipr)+ as.factor(admin) 
                + as.factor(plapublic)
                # +as.factor(plafvc)
                + as.factor(plahq)
                +as.factor(casetype)
                + as.factor(plaind)
                #+as.factor(courttype)
                +as.factor(ruleprocedure)
                ,data = matchdat,method = "exact")

z.out4 = glm(win4 ~ plasoejv + as.factor(province) + as.factor(ruleyear) #+yearchina
             + as.factor(plapublic)
             + as.factor(plahq)
             +as.factor(casetype)
             #+ as.factor(plafvc)
             + as.factor(plaind) 
             #+as.factor(courttype)
             +as.factor(ruleprocedure)
             ,data=match.data(m.out),weights = weights,family = binomial(link = "logit"))

summary(z.out4)


# Comp > 1/2 
matchdat = dat[c("province","casetype","plahq", "ruleyear","ruleprocedure", "plaind","win5","plasoejv","plapublic")]
matchdat = na.omit(matchdat)

m.out = matchit(plasoejv  ~ as.factor(province) + as.factor(ruleyear)   #+ as.factor(contract)
                #+ as.factor(defind) #+ defpc
                #+ as.factor(ipr)+ as.factor(admin) 
                + as.factor(plapublic)
                # +as.factor(plafvc)
                + as.factor(plahq)
                +as.factor(casetype)
                + as.factor(plaind)
                #+as.factor(courttype)
                +as.factor(ruleprocedure)
                ,data = matchdat,method = "exact")

z.out5 = glm(win5 ~ plasoejv + as.factor(province) + as.factor(ruleyear) #+yearchina
             + as.factor(plapublic)
             + as.factor(plahq)
             +as.factor(casetype)
             #+ as.factor(plafvc)
             + as.factor(plaind) 
             #+as.factor(courttype)
             +as.factor(ruleprocedure)
             ,data=match.data(m.out),weights = weights,family = binomial(link = "logit"))

summary(z.out5)


# Comp >= full
matchdat = dat[c("province","casetype","plahq", "ruleyear","ruleprocedure", "plaind","win6","plasoejv","plapublic")]
matchdat = na.omit(matchdat)

m.out = matchit(plasoejv  ~ as.factor(province) + as.factor(ruleyear)   #+ as.factor(contract)
                #+ as.factor(defind) #+ defpc
                #+ as.factor(ipr)+ as.factor(admin) 
                + as.factor(plapublic)
                # +as.factor(plafvc)
                + as.factor(plahq)
                +as.factor(casetype)
                + as.factor(plaind)
                #+as.factor(courttype)
                +as.factor(ruleprocedure)
                ,data = matchdat,method = "exact")

z.out6 = glm(win6 ~ plasoejv + as.factor(province) + as.factor(ruleyear) #+yearchina
             + as.factor(plapublic)
             + as.factor(plahq)
             +as.factor(casetype)
             #+ as.factor(plafvc)
             + as.factor(plaind) 
             #+as.factor(courttype)
             +as.factor(ruleprocedure)
             ,data=match.data(m.out),weights = weights,family = binomial(link = "logit"))

summary(z.out6)


stargazer(z.out1,z.out2,z.out3,z.out4,z.out5,z.out6)



#### political connections mechanism 

# Judgement
matchdat = dat[c("province","casetype","plahq", "ruleyear","ruleprocedure", "plaind","win1","plapc","plapublic")]
matchdat = na.omit(matchdat)

m.out = matchit(plapc  ~ as.factor(province) + as.factor(ruleyear)   #+ as.factor(contract)
                #+ as.factor(defind) #+ defpc
                #+ as.factor(ipr)+ as.factor(admin) 
                + as.factor(plapublic)
                # +as.factor(plafvc)
                + as.factor(plahq)
                +as.factor(casetype)
                + as.factor(plaind)
                #+as.factor(courttype)
                +as.factor(ruleprocedure)
                ,data = matchdat,method = "exact")

z.out1 = glm(win1 ~ plapc + as.factor(province) + as.factor(ruleyear) #+yearchina
             + as.factor(plapublic)
             + as.factor(plahq)
             +as.factor(casetype)
             #+ as.factor(plafvc)
             + as.factor(plaind) 
             #+as.factor(courttype)
             +as.factor(ruleprocedure)
             ,data=match.data(m.out),weights = weights,family = binomial(link = "logit"))

summary(z.out1)


# Court fee
matchdat = dat[c("province","casetype","plahq", "ruleyear","ruleprocedure", "plaind","win2","plapc","plapublic")]
matchdat = na.omit(matchdat)

m.out = matchit(plapc  ~ as.factor(province) + as.factor(ruleyear)   #+ as.factor(contract)
                #+ as.factor(defind) #+ defpc
                #+ as.factor(ipr)+ as.factor(admin) 
                + as.factor(plapublic)
                # +as.factor(plafvc)
                + as.factor(plahq)
                +as.factor(casetype)
                + as.factor(plaind)
                #+as.factor(courttype)
                +as.factor(ruleprocedure)
                ,data = matchdat,method = "exact")

z.out2 = glm(win2 ~ plapc + as.factor(province) + as.factor(ruleyear) #+yearchina
             + as.factor(plapublic)
             + as.factor(plahq)
             +as.factor(casetype)
             #+ as.factor(plafvc)
             + as.factor(plaind) 
             #+as.factor(courttype)
             +as.factor(ruleprocedure)
             ,data=match.data(m.out),weights = weights,family = binomial(link = "logit"))

summary(z.out2)


# Comp > 0 
matchdat = dat[c("province","casetype","plahq", "ruleyear","ruleprocedure", "plaind","win3","plapc","plapublic")]
matchdat = na.omit(matchdat)

m.out = matchit(plapc  ~ as.factor(province) + as.factor(ruleyear)   #+ as.factor(contract)
                #+ as.factor(defind) #+ defpc
                #+ as.factor(ipr)+ as.factor(admin) 
                + as.factor(plapublic)
                # +as.factor(plafvc)
                + as.factor(plahq)
                +as.factor(casetype)
                + as.factor(plaind)
                #+as.factor(courttype)
                +as.factor(ruleprocedure)
                ,data = matchdat,method = "exact")

z.out3 = glm(win3 ~ plapc + as.factor(province) + as.factor(ruleyear) #+yearchina
             + as.factor(plapublic)
             + as.factor(plahq)
             +as.factor(casetype)
             #+ as.factor(plafvc)
             + as.factor(plaind) 
             #+as.factor(courttype)
             +as.factor(ruleprocedure)
             ,data=match.data(m.out),weights = weights,family = binomial(link = "logit"))

summary(z.out3)


# Comp > 1/4
matchdat = dat[c("province","casetype","plahq", "ruleyear","ruleprocedure", "plaind","win4","plapc","plapublic")]
matchdat = na.omit(matchdat)

m.out = matchit(plapc  ~ as.factor(province) + as.factor(ruleyear)   #+ as.factor(contract)
                #+ as.factor(defind) #+ defpc
                #+ as.factor(ipr)+ as.factor(admin) 
                + as.factor(plapublic)
                # +as.factor(plafvc)
                + as.factor(plahq)
                +as.factor(casetype)
                + as.factor(plaind)
                #+as.factor(courttype)
                +as.factor(ruleprocedure)
                ,data = matchdat,method = "exact")


z.out4 = glm(win4 ~ plapc + as.factor(province) + as.factor(ruleyear) #+yearchina
             + as.factor(plapublic)
             + as.factor(plahq)
             +as.factor(casetype)
             #+ as.factor(plafvc)
             + as.factor(plaind) 
             #+as.factor(courttype)
             +as.factor(ruleprocedure)
             ,data=match.data(m.out),weights = weights,family = binomial(link = "logit"))

summary(z.out4)



# Comp > 1/2 
matchdat = dat[c("province","casetype","plahq", "ruleyear","ruleprocedure", "plaind","win5","plapc","plapublic")]
matchdat = na.omit(matchdat)


m.out = matchit(plapc  ~ as.factor(province) + as.factor(ruleyear)   #+ as.factor(contract)
                #+ as.factor(defind) #+ defpc
                #+ as.factor(ipr)+ as.factor(admin) 
                + as.factor(plapublic)
                # +as.factor(plafvc)
                + as.factor(plahq)
                +as.factor(casetype)
                + as.factor(plaind)
                #+as.factor(courttype)
                +as.factor(ruleprocedure)
                ,data = matchdat,method = "exact")



z.out5 = glm(win5 ~ plapc + as.factor(province) + as.factor(ruleyear) #+yearchina
             + as.factor(plapublic)
             + as.factor(plahq)
             +as.factor(casetype)
             #+ as.factor(plafvc)
             + as.factor(plaind) 
             #+as.factor(courttype)
             +as.factor(ruleprocedure)
             ,data=match.data(m.out),weights = weights,family = binomial(link = "logit"))

summary(z.out5)



# Comp >= full
matchdat = dat[c("province","casetype","plahq", "ruleyear","ruleprocedure", "plaind","win6","plapc","plapublic")]
matchdat = na.omit(matchdat)


m.out = matchit(plapc  ~ as.factor(province) + as.factor(ruleyear)   #+ as.factor(contract)
                #+ as.factor(defind) #+ defpc
                #+ as.factor(ipr)+ as.factor(admin) 
                + as.factor(plapublic)
                # +as.factor(plafvc)
                + as.factor(plahq)
                +as.factor(casetype)
                + as.factor(plaind)
                #+as.factor(courttype)
                +as.factor(ruleprocedure)
                ,data = matchdat,method = "exact")



z.out6 = glm(win6 ~ plapc + as.factor(province) + as.factor(ruleyear) #+yearchina
             + as.factor(plapublic)
             + as.factor(plahq)
             +as.factor(casetype)
             #+ as.factor(plafvc)
             + as.factor(plaind) 
             #+as.factor(courttype)
             +as.factor(ruleprocedure)
             ,data=match.data(m.out),weights = weights,family = binomial(link = "logit"))

summary(z.out6)


stargazer(z.out1,z.out2,z.out3,z.out4,z.out5,z.out6)



#_____________________________________________________________________________________________________________________
########################################   APPENDIX     ########################################################## 



########## Table A1 ###########
table(dat$prov)
#_____________________________________________________________________________________________________________________________



########## Table A2 ###########
table(dat$courtlevel)
#_____________________________________________________________________________________________________________________________



########## Table A3 ###########
table(dat$procedure)
#_____________________________________________________________________________________________________________________________


########## Table A4 ###########
# From top to bottom:
table(dat$ipr)[2]
table(dat$contract)[2]
table(dat$admin)[2]
table(dat$infringe)[2]
table(dat$othercase)[2]
table(dat$corporate)[2]
table(dat$labor)[2]
table(dat$special)[2]
table(dat$compensation)[2]
table(dat$property)[2]
table(dat$socialecon)[2]
table(dat$bribe)[2]
table(dat$malfeasance)[2]
#_____________________________________________________________________________________________________________________________



#########  Table A5 ##########
# From top to bottom in the table
table(dat$industry)
#_____________________________________________________________________________________________________________________________




#########  Table A6 ##########
## with fixed assets
summary(subset(dat,  plafe==1 & plafvc==1  )$win1) # judgement 0.551
summary(subset(dat,  plafe==1 & plafvc==1  )$win2) # Lower court fee 0.254
summary(subset(dat,  plafe==1 & plafvc==1  )$win3) #Compensation > 0   0.341
summary(subset(dat,  plafe==1 & plafvc==1  )$win4) #Compensation > 1/4 claim   0.231
summary(subset(dat,  plafe==1 & plafvc==1  )$win5) #Compensation > 1/2 claim  0.200
summary(subset(dat,  plafe==1 & plafvc==1  )$win6) #Compensation >= full claim  0.129
dim(subset(dat,  plafe==1 & plafvc==1  ))[1] #number of cases


## without fixed assets
summary(subset(dat,  plafe==0 & plafvc==1  )$win1) # judgement 0.496
summary(subset(dat,  plafe==0 & plafvc==1  )$win2) # Lower court fee 0.219
summary(subset(dat,  plafe==0 & plafvc==1  )$win3) #Compensation > 0   0.261
summary(subset(dat,  plafe==0 & plafvc==1  )$win4) #Compensation > 1/4 claim   0.154
summary(subset(dat,  plafe==0 & plafvc==1  )$win5) #Compensation > 1/2 claim  0.128
summary(subset(dat,  plafe==0 & plafvc==1  )$win6) #Compensation >= full claim  0.090
dim(subset(dat,  plafe==0 & plafvc==1  ))[1] #number of cases
#_____________________________________________________________________________________________________________________________




#########  Table A7 ##########
## Administrative case
summary(subset(dat,  admin==1  )$win1) # judgement 0.285
summary(subset(dat,  admin==1  )$win2) # Lower court fee 0.134
summary(subset(dat,  admin==1  )$win3) #Compensation > 0   0.042
summary(subset(dat,  admin==1 )$win4) #Compensation > 1/4 claim   0.033
summary(subset(dat,  admin==1 )$win5) #Compensation > 1/2 claim  0.028
summary(subset(dat, admin==1  )$win6) #Compensation >= full claim  0.026
dim(subset(dat,  admin==1 ))[1] #number of cases

## ipr case: suing domestic
summary(subset(dat,  ipr==1 & plafvc==1 )$win1) # judgement 0.515
summary(subset(dat,  ipr==1 & plafvc==1 )$win2) # judgement 0.245
summary(subset(dat,  ipr==1 & plafvc==1 )$win3) # judgement 0.272
summary(subset(dat,  ipr==1 & plafvc==1 )$win4) # judgement 0.107
summary(subset(dat,  ipr==1 & plafvc==1 )$win5) # judgement 0.064
summary(subset(dat,  ipr==1 & plafvc==1 )$win6) # judgement 0.024
dim(subset(dat,   ipr==1 & plafvc==1 ))[1] #number of cases


## ipr case: sued by domestic
summary(subset(dat,  ipr==1 & placvf==1 )$win1) # judgement 0.182
summary(subset(dat,  ipr==1 & placvf==1 )$win2) # judgement 0.077
summary(subset(dat,  ipr==1 & placvf==1 )$win3) # judgement 0.073
summary(subset(dat,  ipr==1 & placvf==1 )$win4) # judgement 0.029
summary(subset(dat,  ipr==1 & placvf==1 )$win5) # judgement 0.029
summary(subset(dat,  ipr==1 & placvf==1 )$win6) # judgement 0.017
dim(subset(dat,   ipr==1 & placvf==1 ))[1] #number of cases
#_____________________________________________________________________________________________________________________________



#########  Table A8 ##########

# USA
length(which(as.character(dat$plahq) == "美国" | as.character(dat$defhq) == "美国" | as.character(dat$plaregister) == "美国" | as.character(dat$defregister) == "美国"))/71914
# South Korea
length(which(as.character(dat$plahq) == "韩国" | as.character(dat$defhq) == "韩国" | as.character(dat$plaregister) == "韩国" | as.character(dat$defregister) == "韩国"))/67375
# Japan
length(which(as.character(dat$plahq) == "日本" | as.character(dat$defhq) == "日本" | as.character(dat$plaregister) == "日本" | as.character(dat$defregister) == "日本"))/52834
# Singapore
length(which(as.character(dat$plahq) == "新加坡" | as.character(dat$defhq) == "新加坡" | as.character(dat$plaregister) == "新加坡" | as.character(dat$defregister) == "新加坡"))/26111 
# Germany
length(which(as.character(dat$plahq) == "德国" | as.character(dat$defhq) == "德国" | as.character(dat$plaregister) == "德国" | as.character(dat$defregister) == "德国"))/10834
#UK
length(which(as.character(dat$plahq) == "英国" | as.character(dat$defhq) == "英国" | as.character(dat$plaregister) == "英国" | as.character(dat$defregister) == "英国"))/10040
#France
length(which(as.character(dat$plahq) == "法国" | as.character(dat$defhq) == "法国" | as.character(dat$plaregister) == "法国" | as.character(dat$defregister) == "法国"))/6035

#_____________________________________________________________________________________________________________________________



#########  Table B1 ##########
dat13 = subset(dat,fileyear >=2013)

### political partnership mechanism
# Judgement (win1)
basicm1 = glm.cluster(win1 ~  plasoejv   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat13, cluster = "province",  family = binomial())


# Court fee (win2)
basicm2 = glm.cluster(win2 ~  plasoejv   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat13, cluster = "province",  family = binomial())


# Comp > 0 (win3)
basicm3 = glm.cluster(win3 ~  plasoejv   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat13, cluster = "province",  family = binomial())


# Comp > 1/4 (win4)
basicm4 = glm.cluster(win4 ~  plasoejv   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat13, cluster = "province",  family = binomial())


# Comp > 1/2 (win5)
basicm5 = glm.cluster(win5 ~  plasoejv   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat13, cluster = "province",  family = binomial())


# Comp >= full (win6)
basicm6 = glm.cluster(win6 ~  plasoejv   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat13, cluster = "province",  family = binomial())


## SOE JV
texreg(list(basicm1$glm_res,basicm2$glm_res,basicm3$glm_res,basicm4$glm_res,basicm5$glm_res,basicm6$glm_res),
       override.se = list(summary(basicm1)[,2],summary(basicm2)[,2],summary(basicm3)[,2],summary(basicm4)[,2],summary(basicm5)[,2],summary(basicm6)[,2]),
       override.pvalues = list(summary(basicm1)[,4],summary(basicm2)[,4],summary(basicm3)[,4],summary(basicm4)[,4],summary(basicm5)[,4],summary(basicm6)[,4]),digits=3,
       stars = c(0.001, 0.01, 0.05,0.1))




### political connections mechanism
# Judgement (win1)
basicm1 = glm.cluster(win1 ~  plapc   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat13, cluster = "province",  family = binomial())


# Court fee (win2)
basicm2 = glm.cluster(win2 ~  plapc   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat13, cluster = "province",  family = binomial())


# Comp > 0 (win3)
basicm3 = glm.cluster(win3 ~  plapc   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat13, cluster = "province",  family = binomial())


# Comp > 1/4 (win4)
basicm4 = glm.cluster(win4 ~  plapc   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat13, cluster = "province",  family = binomial())


# Comp > 1/2 (win5)
basicm5 = glm.cluster(win5 ~  plapc   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat13, cluster = "province",  family = binomial())


# Comp >= full (win6)
basicm6 = glm.cluster(win6 ~  plapc   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat13, cluster = "province",  family = binomial())



## political connections
texreg(list(basicm1$glm_res,basicm2$glm_res,basicm3$glm_res,basicm4$glm_res,basicm5$glm_res,basicm6$glm_res),
       override.se = list(summary(basicm1)[,2],summary(basicm2)[,2],summary(basicm3)[,2],summary(basicm4)[,2],summary(basicm5)[,2],summary(basicm6)[,2]),
       override.pvalues = list(summary(basicm1)[,4],summary(basicm2)[,4],summary(basicm3)[,4],summary(basicm4)[,4],summary(basicm5)[,4],summary(basicm6)[,4]),digits=3,
       stars = c(0.001, 0.01, 0.05,0.1))




### political connections mechanism (narrow)
# Judgement (win1)
basicm1 = glm.cluster(win1 ~  plapc2   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat13, cluster = "province",  family = binomial())

# Court fee (win2)
basicm2 = glm.cluster(win2 ~  plapc2   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat13, cluster = "province",  family = binomial())


# Comp > 0 (win3)
basicm3 = glm.cluster(win3 ~  plapc2   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat13, cluster = "province",  family = binomial())


# Comp > 1/4 (win4)
basicm4 = glm.cluster(win4 ~  plapc2   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat13, cluster = "province",  family = binomial())


# Comp > 1/2 (win5)
basicm5 = glm.cluster(win5 ~  plapc2   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat13, cluster = "province",  family = binomial())


# Comp >= full (win6)
basicm6 = glm.cluster(win6 ~  plapc2   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat13, cluster = "province",  family = binomial())


## political connections (narrow)
texreg(list(basicm1$glm_res,basicm2$glm_res,basicm3$glm_res,basicm4$glm_res,basicm5$glm_res,basicm6$glm_res),
       override.se = list(summary(basicm1)[,2],summary(basicm2)[,2],summary(basicm3)[,2],summary(basicm4)[,2],summary(basicm5)[,2],summary(basicm6)[,2]),
       override.pvalues = list(summary(basicm1)[,4],summary(basicm2)[,4],summary(basicm3)[,4],summary(basicm4)[,4],summary(basicm5)[,4],summary(basicm6)[,4]),digits=3,
       stars = c(0.001, 0.01, 0.05,0.1))




### political partnership beyond political connections
# Judgement (win1)
basicm1 = glm.cluster(win1 ~  plasoejv + plapc   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat13, cluster = "province",  family = binomial())

# Court fee (win2)
basicm2 = glm.cluster(win2 ~  plasoejv + plapc    + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat13, cluster = "province",  family = binomial())


# Comp > 0 (win3)
basicm3 = glm.cluster(win3 ~  plasoejv + plapc    + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat13, cluster = "province",  family = binomial())


# Comp > 1/4 (win4)
basicm4 = glm.cluster(win4 ~  plasoejv + plapc    + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat13, cluster = "province",  family = binomial())


# Comp > 1/2 (win5)
basicm5 = glm.cluster(win5 ~  plasoejv + plapc    + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat13, cluster = "province",  family = binomial())


# Comp >= full (win6)
basicm6 = glm.cluster(win6 ~  plasoejv + plapc    + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat13, cluster = "province",  family = binomial())



## SOE JV + political connections 
texreg(list(basicm1$glm_res,basicm2$glm_res,basicm3$glm_res,basicm4$glm_res,basicm5$glm_res,basicm6$glm_res),
       override.se = list(summary(basicm1)[,2],summary(basicm2)[,2],summary(basicm3)[,2],summary(basicm4)[,2],summary(basicm5)[,2],summary(basicm6)[,2]),
       override.pvalues = list(summary(basicm1)[,4],summary(basicm2)[,4],summary(basicm3)[,4],summary(basicm4)[,4],summary(basicm5)[,4],summary(basicm6)[,4]),digits=3,
       stars = c(0.001, 0.01, 0.05,0.1))

#____________________________________________________________________________________________________________________________________




#########  Table B2 ##########

datcoast = subset(dat, province %in% c("河北", "北京",  "天津市",  "山东","江苏","上海", "浙江", "福建","广东","海南"))


### political partnership mechanism
# Judgement (win1)
basicm1 = glm.cluster(win1 ~  plasoejv   + as.factor(plahq) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = datcoast, cluster = "province",  family = binomial())


# Court fee (win2)
basicm2 = glm.cluster(win2 ~  plasoejv   + as.factor(plahq)  + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = datcoast, cluster = "province",  family = binomial())


# Comp > 0 (win3)
basicm3 = glm.cluster(win3 ~  plasoejv   + as.factor(plahq)  + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = datcoast, cluster = "province",  family = binomial())


# Comp > 1/4 (win4)
basicm4 = glm.cluster(win4 ~  plasoejv   + as.factor(plahq)  + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = datcoast, cluster = "province",  family = binomial())


# Comp > 1/2 (win5)
basicm5 = glm.cluster(win5 ~  plasoejv   + as.factor(plahq)  + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = datcoast, cluster = "province",  family = binomial())


# Comp >= full (win6)
basicm6 = glm.cluster(win6 ~  plasoejv   + as.factor(plahq)  + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = datcoast, cluster = "province",  family = binomial())


## SOE JV
texreg(list(basicm1$glm_res,basicm2$glm_res,basicm3$glm_res,basicm4$glm_res,basicm5$glm_res,basicm6$glm_res),
       override.se = list(summary(basicm1)[,2],summary(basicm2)[,2],summary(basicm3)[,2],summary(basicm4)[,2],summary(basicm5)[,2],summary(basicm6)[,2]),
       override.pvalues = list(summary(basicm1)[,4],summary(basicm2)[,4],summary(basicm3)[,4],summary(basicm4)[,4],summary(basicm5)[,4],summary(basicm6)[,4]),digits=3,
       stars = c(0.001, 0.01, 0.05,0.1))




### political connections mechanism
# Judgement (win1)
basicm1 = glm.cluster(win1 ~  plapc   + as.factor(plahq)  + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = datcoast, cluster = "province",  family = binomial())


# Court fee (win2)
basicm2 = glm.cluster(win2 ~  plapc   + as.factor(plahq) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = datcoast, cluster = "province",  family = binomial())


# Comp > 0 (win3)
basicm3 = glm.cluster(win3 ~  plapc   + as.factor(plahq)  + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = datcoast, cluster = "province",  family = binomial())


# Comp > 1/4 (win4)
basicm4 = glm.cluster(win4 ~  plapc   + as.factor(plahq)  + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = datcoast, cluster = "province",  family = binomial())


# Comp > 1/2 (win5)
basicm5 = glm.cluster(win5 ~  plapc   + as.factor(plahq) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = datcoast, cluster = "province",  family = binomial())


# Comp >= full (win6)
basicm6 = glm.cluster(win6 ~  plapc   + as.factor(plahq)  + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = datcoast, cluster = "province",  family = binomial())



## political connections
texreg(list(basicm1$glm_res,basicm2$glm_res,basicm3$glm_res,basicm4$glm_res,basicm5$glm_res,basicm6$glm_res),
       override.se = list(summary(basicm1)[,2],summary(basicm2)[,2],summary(basicm3)[,2],summary(basicm4)[,2],summary(basicm5)[,2],summary(basicm6)[,2]),
       override.pvalues = list(summary(basicm1)[,4],summary(basicm2)[,4],summary(basicm3)[,4],summary(basicm4)[,4],summary(basicm5)[,4],summary(basicm6)[,4]),digits=3,
       stars = c(0.001, 0.01, 0.05,0.1))




### political connections mechanism (narrow)
# Judgement (win1)
basicm1 = glm.cluster(win1 ~  plapc2   + as.factor(plahq)  + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = datcoast, cluster = "province",  family = binomial())

# Court fee (win2)
basicm2 = glm.cluster(win2 ~  plapc2   + as.factor(plahq) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = datcoast, cluster = "province",  family = binomial())


# Comp > 0 (win3)
basicm3 = glm.cluster(win3 ~  plapc2   + as.factor(plahq)  + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = datcoast, cluster = "province",  family = binomial())


# Comp > 1/4 (win4)
basicm4 = glm.cluster(win4 ~  plapc2   + as.factor(plahq) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = datcoast, cluster = "province",  family = binomial())


# Comp > 1/2 (win5)
basicm5 = glm.cluster(win5 ~  plapc2   + as.factor(plahq)  + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = datcoast, cluster = "province",  family = binomial())


# Comp >= full (win6)
basicm6 = glm.cluster(win6 ~  plapc2   + as.factor(plahq)  + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = datcoast, cluster = "province",  family = binomial())


## political connections (narrow)
texreg(list(basicm1$glm_res,basicm2$glm_res,basicm3$glm_res,basicm4$glm_res,basicm5$glm_res,basicm6$glm_res),
       override.se = list(summary(basicm1)[,2],summary(basicm2)[,2],summary(basicm3)[,2],summary(basicm4)[,2],summary(basicm5)[,2],summary(basicm6)[,2]),
       override.pvalues = list(summary(basicm1)[,4],summary(basicm2)[,4],summary(basicm3)[,4],summary(basicm4)[,4],summary(basicm5)[,4],summary(basicm6)[,4]),digits=3,
       stars = c(0.001, 0.01, 0.05,0.1))




### political partnership beyond political connections
# Judgement (win1)
basicm1 = glm.cluster(win1 ~  plasoejv + plapc   + as.factor(plahq)  + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = datcoast, cluster = "province",  family = binomial())

# Court fee (win2)
basicm2 = glm.cluster(win2 ~  plasoejv + plapc    + as.factor(plahq) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = datcoast, cluster = "province",  family = binomial())


# Comp > 0 (win3)
basicm3 = glm.cluster(win3 ~  plasoejv + plapc    + as.factor(plahq)  + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = datcoast, cluster = "province",  family = binomial())


# Comp > 1/4 (win4)
basicm4 = glm.cluster(win4 ~  plasoejv + plapc    + as.factor(plahq)  + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = datcoast, cluster = "province",  family = binomial())


# Comp > 1/2 (win5)
basicm5 = glm.cluster(win5 ~  plasoejv + plapc    + as.factor(plahq)  + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = datcoast, cluster = "province",  family = binomial())


# Comp >= full (win6)
basicm6 = glm.cluster(win6 ~  plasoejv + plapc    + as.factor(plahq)  + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = datcoast, cluster = "province",  family = binomial())


## SOE JV + political connections 
texreg(list(basicm1$glm_res,basicm2$glm_res,basicm3$glm_res,basicm4$glm_res,basicm5$glm_res,basicm6$glm_res),
       override.se = list(summary(basicm1)[,2],summary(basicm2)[,2],summary(basicm3)[,2],summary(basicm4)[,2],summary(basicm5)[,2],summary(basicm6)[,2]),
       override.pvalues = list(summary(basicm1)[,4],summary(basicm2)[,4],summary(basicm3)[,4],summary(basicm4)[,4],summary(basicm5)[,4],summary(basicm6)[,4]),digits=3,
       stars = c(0.001, 0.01, 0.05,0.1))

#____________________________________________________________________________________________________________________________________



#########  Table B3 ##########
datplt = subset(dat,plaforeign==1)

### political partnership mechanism
# Judgement (win1)
basicm1 = glm.cluster(win1 ~  plasoejv   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = datplt, cluster = "province",  family = binomial())


# Court fee (win2)
basicm2 = glm.cluster(win2 ~  plasoejv   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = datplt, cluster = "province",  family = binomial())


# Comp > 0 (win3)
basicm3 = glm.cluster(win3 ~  plasoejv   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = datplt, cluster = "province",  family = binomial())


# Comp > 1/4 (win4)
basicm4 = glm.cluster(win4 ~  plasoejv   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = datplt, cluster = "province",  family = binomial())


# Comp > 1/2 (win5)
basicm5 = glm.cluster(win5 ~  plasoejv   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = datplt, cluster = "province",  family = binomial())


# Comp >= full (win6)
basicm6 = glm.cluster(win6 ~  plasoejv   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = datplt, cluster = "province",  family = binomial())



## SOE JV
texreg(list(basicm1$glm_res,basicm2$glm_res,basicm3$glm_res,basicm4$glm_res,basicm5$glm_res,basicm6$glm_res),
       override.se = list(summary(basicm1)[,2],summary(basicm2)[,2],summary(basicm3)[,2],summary(basicm4)[,2],summary(basicm5)[,2],summary(basicm6)[,2]),
       override.pvalues = list(summary(basicm1)[,4],summary(basicm2)[,4],summary(basicm3)[,4],summary(basicm4)[,4],summary(basicm5)[,4],summary(basicm6)[,4]),digits=3,
       stars = c(0.001, 0.01, 0.05,0.1))



### political connections mechanism
# Judgement (win1)
basicm1 = glm.cluster(win1 ~  plapc   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = datplt, cluster = "province",  family = binomial())


# Court fee (win2)
basicm2 = glm.cluster(win2 ~  plapc   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = datplt, cluster = "province",  family = binomial())


# Comp > 0 (win3)
basicm3 = glm.cluster(win3 ~  plapc   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = datplt, cluster = "province",  family = binomial())


# Comp > 1/4 (win4)
basicm4 = glm.cluster(win4 ~  plapc   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = datplt, cluster = "province",  family = binomial())


# Comp > 1/2 (win5)
basicm5 = glm.cluster(win5 ~  plapc   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = datplt, cluster = "province",  family = binomial())


# Comp >= full (win6)
basicm6 = glm.cluster(win6 ~  plapc   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = datplt, cluster = "province",  family = binomial())


## political connections
texreg(list(basicm1$glm_res,basicm2$glm_res,basicm3$glm_res,basicm4$glm_res,basicm5$glm_res,basicm6$glm_res),
       override.se = list(summary(basicm1)[,2],summary(basicm2)[,2],summary(basicm3)[,2],summary(basicm4)[,2],summary(basicm5)[,2],summary(basicm6)[,2]),
       override.pvalues = list(summary(basicm1)[,4],summary(basicm2)[,4],summary(basicm3)[,4],summary(basicm4)[,4],summary(basicm5)[,4],summary(basicm6)[,4]),digits=3,
       stars = c(0.001, 0.01, 0.05,0.1))



### political connections mechanism (narrow)
# Judgement (win1)
basicm1 = glm.cluster(win1 ~  plapc2   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = datplt, cluster = "province",  family = binomial())

# Court fee (win2)
basicm2 = glm.cluster(win2 ~  plapc2   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = datplt, cluster = "province",  family = binomial())


# Comp > 0 (win3)
basicm3 = glm.cluster(win3 ~  plapc2   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = datplt, cluster = "province",  family = binomial())


# Comp > 1/4 (win4)
basicm4 = glm.cluster(win4 ~  plapc2   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = datplt, cluster = "province",  family = binomial())


# Comp > 1/2 (win5)
basicm5 = glm.cluster(win5 ~  plapc2   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = datplt, cluster = "province",  family = binomial())


# Comp >= full (win6)
basicm6 = glm.cluster(win6 ~  plapc2   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = datplt, cluster = "province",  family = binomial())


## political connections (narrow)
texreg(list(basicm1$glm_res,basicm2$glm_res,basicm3$glm_res,basicm4$glm_res,basicm5$glm_res,basicm6$glm_res),
       override.se = list(summary(basicm1)[,2],summary(basicm2)[,2],summary(basicm3)[,2],summary(basicm4)[,2],summary(basicm5)[,2],summary(basicm6)[,2]),
       override.pvalues = list(summary(basicm1)[,4],summary(basicm2)[,4],summary(basicm3)[,4],summary(basicm4)[,4],summary(basicm5)[,4],summary(basicm6)[,4]),digits=3,
       stars = c(0.001, 0.01, 0.05,0.1))



### political partnership beyond political connections
# Judgement (win1)
basicm1 = glm.cluster(win1 ~  plasoejv + plapc   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = datplt, cluster = "province",  family = binomial())

# Court fee (win2)
basicm2 = glm.cluster(win2 ~  plasoejv + plapc    + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = datplt, cluster = "province",  family = binomial())


# Comp > 0 (win3)
basicm3 = glm.cluster(win3 ~  plasoejv + plapc    + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = datplt, cluster = "province",  family = binomial())


# Comp > 1/4 (win4)
basicm4 = glm.cluster(win4 ~  plasoejv + plapc    + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = datplt, cluster = "province",  family = binomial())


# Comp > 1/2 (win5)
basicm5 = glm.cluster(win5 ~  plasoejv + plapc    + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = datplt, cluster = "province",  family = binomial())


# Comp >= full (win6)
basicm6 = glm.cluster(win6 ~  plasoejv + plapc    + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = datplt, cluster = "province",  family = binomial())


## SOE JV + political connections 
texreg(list(basicm1$glm_res,basicm2$glm_res,basicm3$glm_res,basicm4$glm_res,basicm5$glm_res,basicm6$glm_res),
       override.se = list(summary(basicm1)[,2],summary(basicm2)[,2],summary(basicm3)[,2],summary(basicm4)[,2],summary(basicm5)[,2],summary(basicm6)[,2]),
       override.pvalues = list(summary(basicm1)[,4],summary(basicm2)[,4],summary(basicm3)[,4],summary(basicm4)[,4],summary(basicm5)[,4],summary(basicm6)[,4]),digits=3,
       stars = c(0.001, 0.01, 0.05,0.1))

#____________________________________________________________________________________________________________________________________



######### Table B4 #########

### Controlling for China experience
# Judgement (win1)
basicm1 = glm.cluster(win1 ~  plasoejv   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  + yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())

# Court fee (win2)
basicm2 = glm.cluster(win2 ~  plasoejv   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  + yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())


# Comp > 0 (win3)
basicm3 = glm.cluster(win3 ~  plasoejv   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  + yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())


# Comp > 1/4 (win4)
basicm4 = glm.cluster(win4 ~  plasoejv   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  + yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())


# Comp > 1/2 (win5)
basicm5 = glm.cluster(win5 ~  plasoejv   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  + yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())


# Comp >= full (win6)
basicm6 = glm.cluster(win6 ~  plasoejv   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  + yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())


## SOE JV
texreg(list(basicm1$glm_res,basicm2$glm_res,basicm3$glm_res,basicm4$glm_res,basicm5$glm_res,basicm6$glm_res),
       override.se = list(summary(basicm1)[,2],summary(basicm2)[,2],summary(basicm3)[,2],summary(basicm4)[,2],summary(basicm5)[,2],summary(basicm6)[,2]),
       override.pvalues = list(summary(basicm1)[,4],summary(basicm2)[,4],summary(basicm3)[,4],summary(basicm4)[,4],summary(basicm5)[,4],summary(basicm6)[,4]),digits=3,
       stars = c(0.001, 0.01, 0.05,0.1))




### Controlling for firm size
# Judgement (win1)
basicm1 = glm.cluster(win1 ~  plasoejv   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc # + yearchina
                      + as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())

# Court fee (win2)
basicm2 = glm.cluster(win2 ~  plasoejv   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc # + yearchina
                      + as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())


# Comp > 0 (win3)
basicm3 = glm.cluster(win3 ~  plasoejv   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc # + yearchina
                      + as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())


# Comp > 1/4 (win4)
basicm4 = glm.cluster(win4 ~  plasoejv   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc #+ yearchina
                      + as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())


# Comp > 1/2 (win5)
basicm5 = glm.cluster(win5 ~  plasoejv   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc # + yearchina
                      + as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())


# Comp >= full (win6)
basicm6 = glm.cluster(win6 ~  plasoejv   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc # + yearchina
                      + as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())


## SOE JV
texreg(list(basicm1$glm_res,basicm2$glm_res,basicm3$glm_res,basicm4$glm_res,basicm5$glm_res,basicm6$glm_res),
       override.se = list(summary(basicm1)[,2],summary(basicm2)[,2],summary(basicm3)[,2],summary(basicm4)[,2],summary(basicm5)[,2],summary(basicm6)[,2]),
       override.pvalues = list(summary(basicm1)[,4],summary(basicm2)[,4],summary(basicm3)[,4],summary(basicm4)[,4],summary(basicm5)[,4],summary(basicm6)[,4]),digits=3,
       stars = c(0.001, 0.01, 0.05,0.1))

#__________________________________________________________________________________________________________________________________



### Controlling for listing status
# Judgement (win1)
basicm1 = glm.cluster(win1 ~  plasoejv   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc # + yearchina
                      +plapublic
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())

# Court fee (win2)
basicm2 = glm.cluster(win2 ~  plasoejv   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc # + yearchina
                      +plapublic
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())


# Comp > 0 (win3)
basicm3 = glm.cluster(win3 ~  plasoejv   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc # + yearchina
                      +plapublic
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())


# Comp > 1/4 (win4)
basicm4 = glm.cluster(win4 ~  plasoejv   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc # + yearchina
                      +plapublic
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())


# Comp > 1/2 (win5)
basicm5 = glm.cluster(win5 ~  plasoejv   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc # + yearchina
                      +plapublic
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())


# Comp >= full (win6)
basicm6 = glm.cluster(win6 ~  plasoejv   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc # + yearchina
                      +plapublic
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province",  family = binomial())


## SOE JV
texreg(list(basicm1$glm_res,basicm2$glm_res,basicm3$glm_res,basicm4$glm_res,basicm5$glm_res,basicm6$glm_res),
       override.se = list(summary(basicm1)[,2],summary(basicm2)[,2],summary(basicm3)[,2],summary(basicm4)[,2],summary(basicm5)[,2],summary(basicm6)[,2]),
       override.pvalues = list(summary(basicm1)[,4],summary(basicm2)[,4],summary(basicm3)[,4],summary(basicm4)[,4],summary(basicm5)[,4],summary(basicm6)[,4]),digits=3,
       stars = c(0.001, 0.01, 0.05,0.1))

#__________________________________________________________________________________________________________________________________




#########  Table B5 ##########

### political partnership mechanism
# Judgement (win1)
basicm1 = lm.cluster(win1 ~  plasoejv   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province")

# Court fee (win2)
basicm2 =lm.cluster(win2 ~  plasoejv   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                    + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                    +plafvc  #+ yearchina
                    #+ as.numeric(as.character(placapital))
                    ,data = dat, cluster = "province")


# Comp > 0 (win3)
basicm3 = lm.cluster(win3 ~  plasoejv   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province")


# Comp > 1/4 (win4)
basicm4 = lm.cluster(win4 ~  plasoejv   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province")


# Comp > 1/2 (win5)
basicm5 = lm.cluster(win5 ~  plasoejv   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province")


# Comp >= full (win6)
basicm6 = lm.cluster(win6 ~  plasoejv   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province")


## SOE JV
texreg(list(basicm1$lm_res,basicm2$lm_res,basicm3$lm_res,basicm4$lm_res,basicm5$lm_res,basicm6$lm_res),
       override.se = list(summary(basicm1)[,2],summary(basicm2)[,2],summary(basicm3)[,2],summary(basicm4)[,2],summary(basicm5)[,2],summary(basicm6)[,2]),
       override.pvalues = list(summary(basicm1)[,4],summary(basicm2)[,4],summary(basicm3)[,4],summary(basicm4)[,4],summary(basicm5)[,4],summary(basicm6)[,4]),digits=3,
       stars = c(0.001, 0.01, 0.05,0.1))




### political connections mechanism
# Judgement (win1)
basicm1 = lm.cluster(win1 ~  plapc   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province")

# Court fee (win2)
basicm2 = lm.cluster(win2 ~  plapc   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province")


# Comp > 0 (win3)
basicm3 = lm.cluster(win3 ~  plapc   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province")


# Comp > 1/4 (win4)
basicm4 = lm.cluster(win4 ~  plapc   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province")


# Comp > 1/2 (win5)
basicm5 = lm.cluster(win5 ~  plapc   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province")


# Comp >= full (win6)
basicm6 = lm.cluster(win6 ~  plapc   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province")


## political connections
texreg(list(basicm1$lm_res,basicm2$lm_res,basicm3$lm_res,basicm4$lm_res,basicm5$lm_res,basicm6$lm_res),
       override.se = list(summary(basicm1)[,2],summary(basicm2)[,2],summary(basicm3)[,2],summary(basicm4)[,2],summary(basicm5)[,2],summary(basicm6)[,2]),
       override.pvalues = list(summary(basicm1)[,4],summary(basicm2)[,4],summary(basicm3)[,4],summary(basicm4)[,4],summary(basicm5)[,4],summary(basicm6)[,4]),digits=3,
       stars = c(0.001, 0.01, 0.05,0.1))




### political connections mechanism (narrow)
# Judgement (win1)
basicm1 = lm.cluster(win1 ~  plapc2   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province")

# Court fee (win2)
basicm2 = lm.cluster(win2 ~  plapc2   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province")


# Comp > 0 (win3)
basicm3 = lm.cluster(win3 ~  plapc2   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province")


# Comp > 1/4 (win4)
basicm4 = lm.cluster(win4 ~  plapc2   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province")


# Comp > 1/2 (win5)
basicm5 = lm.cluster(win5 ~  plapc2   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province")


# Comp >= full (win6)
basicm6 = lm.cluster(win6 ~  plapc2   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province")


## political connections (narrow)
texreg(list(basicm1$lm_res,basicm2$lm_res,basicm3$lm_res,basicm4$lm_res,basicm5$lm_res,basicm6$lm_res),
       override.se = list(summary(basicm1)[,2],summary(basicm2)[,2],summary(basicm3)[,2],summary(basicm4)[,2],summary(basicm5)[,2],summary(basicm6)[,2]),
       override.pvalues = list(summary(basicm1)[,4],summary(basicm2)[,4],summary(basicm3)[,4],summary(basicm4)[,4],summary(basicm5)[,4],summary(basicm6)[,4]),digits=3,
       stars = c(0.001, 0.01, 0.05,0.1))

#________________________________________________________________________________________________________________________________________



### political partnership beyond political connections
# Judgement (win1)
basicm1 = lm.cluster(win1 ~  plasoejv + plapc   + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province")

# Court fee (win2)
basicm2 = lm.cluster(win2 ~  plasoejv + plapc    + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province")


# Comp > 0 (win3)
basicm3 = lm.cluster(win3 ~  plasoejv + plapc    + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province")


# Comp > 1/4 (win4)
basicm4 = lm.cluster(win4 ~  plasoejv + plapc    + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province")


# Comp > 1/2 (win5)
basicm5 = lm.cluster(win5 ~  plasoejv + plapc    + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province")


# Comp >= full (win6)
basicm6 = lm.cluster(win6 ~  plasoejv + plapc    + as.factor(plahq) + as.factor(province) + as.factor(ruleyear)   + as.factor(ruleprocedure) 
                      + as.factor(plaind) + as.factor(casetype)  #+ defparty as.factor(courttype) + + as.factor(plaappeal)+ as.factor(appeal)+ as.factor(contract)
                      +plafvc  #+ yearchina
                      #+ as.numeric(as.character(placapital))
                      ,data = dat, cluster = "province")


## SOE JV + political connections 
texreg(list(basicm1$lm_res,basicm2$lm_res,basicm3$lm_res,basicm4$lm_res,basicm5$lm_res,basicm6$lm_res),
       override.se = list(summary(basicm1)[,2],summary(basicm2)[,2],summary(basicm3)[,2],summary(basicm4)[,2],summary(basicm5)[,2],summary(basicm6)[,2]),
       override.pvalues = list(summary(basicm1)[,4],summary(basicm2)[,4],summary(basicm3)[,4],summary(basicm4)[,4],summary(basicm5)[,4],summary(basicm6)[,4]),digits=3,
       stars = c(0.001, 0.01, 0.05,0.1))

#_____________________________________________________________________________________________________________________________



############## Table B6 ###############
library(MatchIt)
library(stargazer)

## For earlier versions of MatchIt, the function randomly selects matched sets when two or more observations have the same 
## propensity score. MatchIt may also randomly selects the order of treated observations for matching. Therefore, running 
## the same codes would generate different coefficient estimates each time. However, the 
## differences are fairly minor and do not affect the substantive conclusions. 
## Keep running the same set of codes will for sure obtain the same results as presented in Table B6 eventually.


#### political partnership mechanism 
# Judgement
matchdat = dat[c("province","casetype","plahq","yearchina","plapublic", "ruleyear","ruleprocedure", "plaind","win1","plasoejv")]
matchdat = na.omit(matchdat)

mod_match <- matchit(plasoejv ~ as.factor(province) + as.factor(ruleyear)   #+ as.factor(contract)
                     #+ as.factor(defind) #+ defpc
                     #+ as.factor(ipr)+ as.factor(admin) 
                     + as.factor(plapublic)
                     # +as.factor(plafvc)
                     + as.factor(plahq)
                     +as.factor(casetype)
                     + as.factor(plaind)
                     + yearchina
                     #+ placapital
                     #+as.factor(courttype)
                     +as.factor(ruleprocedure),
                     method = "nearest", data = matchdat)

lm_treat1 <- lm(win1 ~ plasoejv + as.factor(province) + as.factor(ruleyear) #+yearchina
                + as.factor(plapublic)
                + as.factor(plahq)
                +as.factor(casetype)
                #+ as.factor(plafvc)
                + as.factor(plaind) 
                + yearchina
                #+ placapital
                #+as.factor(courttype)
                +as.factor(ruleprocedure)
                , data = match.data(mod_match))

summary(lm_treat1) # If the coefficient estimate for plasoejv is different from Table B6 (SOE JV), rerun the above chunk of codes.
stargazer(lm_treat1)



# Court fee
matchdat = dat[c("province","casetype","plahq","yearchina", "ruleyear","ruleprocedure", "plaind","win2","plasoejv","plapublic")]
matchdat = na.omit(matchdat)

mod_match <- matchit(plasoejv ~ as.factor(province) + as.factor(ruleyear)   #+ as.factor(contract)
                     #+ as.factor(defind) #+ defpc
                     #+ as.factor(ipr)+ as.factor(admin) 
                     + as.factor(plapublic)
                     # +as.factor(plafvc)
                     + as.factor(plahq)
                     +as.factor(casetype)
                     + as.factor(plaind)
                     + yearchina
                     #+ placapital
                     #+as.factor(courttype)
                     +as.factor(ruleprocedure),
                     method = "nearest", data = matchdat)

lm_treat2 <- lm(win2 ~ plasoejv + as.factor(province) + as.factor(ruleyear) #+yearchina
                + as.factor(plapublic)
                + as.factor(plahq)
                +as.factor(casetype)
                #+ as.factor(plafvc)
                + as.factor(plaind) 
                + yearchina
                #+ placapital
                #+as.factor(courttype)
                +as.factor(ruleprocedure)
                , data = match.data(mod_match))


summary(lm_treat2)  # If the coefficient estimate for plasoejv is different from Table B6 (SOE JV), rerun the above chunk of codes.
stargazer(lm_treat2)


# Comp > 0 
matchdat = dat[c("province","casetype","plahq","yearchina", "ruleyear","ruleprocedure", "plaind","win3","plasoejv","plapublic")]
matchdat = na.omit(matchdat)

mod_match <- matchit(plasoejv ~ as.factor(province) + as.factor(ruleyear)   #+ as.factor(contract)
                     #+ as.factor(defind) #+ defpc
                     #+ as.factor(ipr)+ as.factor(admin) 
                     + as.factor(plapublic)
                     # +as.factor(plafvc)
                     + as.factor(plahq)
                     +as.factor(casetype)
                     + as.factor(plaind)
                     + yearchina
                     #+ placapital
                     #+as.factor(courttype)
                     +as.factor(ruleprocedure),
                     method = "nearest", data = matchdat)

lm_treat3 <- lm(win3 ~ plasoejv + as.factor(province) + as.factor(ruleyear) #+yearchina
                + as.factor(plapublic)
                + as.factor(plahq)
                +as.factor(casetype)
                #+ as.factor(plafvc)
                + as.factor(plaind) 
                + yearchina
                #+ placapital
                #+as.factor(courttype)
                +as.factor(ruleprocedure)
                , data = match.data(mod_match))


summary(lm_treat3)  # If the coefficient estimate for plasoejv is different from Table B6 (SOE JV), rerun the above chunk of codes.
stargazer(lm_treat3)



# Comp > 1/4
matchdat = dat[c("province","casetype","plahq","yearchina", "ruleyear","ruleprocedure", "plaind","win4","plasoejv","plapublic")]
matchdat = na.omit(matchdat)

mod_match <- matchit(plasoejv ~ as.factor(province) + as.factor(ruleyear)   #+ as.factor(contract)
                     #+ as.factor(defind) #+ defpc
                     #+ as.factor(ipr)+ as.factor(admin) 
                     + as.factor(plapublic)
                     # +as.factor(plafvc)
                     + as.factor(plahq)
                     +as.factor(casetype)
                     + as.factor(plaind)
                     + yearchina
                     #+ placapital
                     #+as.factor(courttype)
                     +as.factor(ruleprocedure),
                     method = "nearest", data = matchdat)

lm_treat4 <- lm(win4 ~ plasoejv + as.factor(province) + as.factor(ruleyear) #+yearchina
                + as.factor(plapublic)
                + as.factor(plahq)
                +as.factor(casetype)
                #+ as.factor(plafvc)
                + as.factor(plaind) 
                + yearchina
                #+ placapital
                #+as.factor(courttype)
                +as.factor(ruleprocedure)
                , data = match.data(mod_match))


summary(lm_treat4)  # If the coefficient estimate for plasoejv is different from Table B6 (SOE JV), rerun the above chunk of codes.
stargazer(lm_treat4)



# Comp > 1/2 
matchdat = dat[c("province","casetype","plahq","yearchina", "ruleyear","ruleprocedure", "plaind","win5","plasoejv","plapublic")]
matchdat = na.omit(matchdat)

mod_match <- matchit(plasoejv ~ as.factor(province) + as.factor(ruleyear)   #+ as.factor(contract)
                     #+ as.factor(defind) #+ defpc
                     #+ as.factor(ipr)+ as.factor(admin) 
                     + as.factor(plapublic)
                     # +as.factor(plafvc)
                     + as.factor(plahq)
                     +as.factor(casetype)
                     + as.factor(plaind)
                     + yearchina
                     #+ placapital
                     #+as.factor(courttype)
                     +as.factor(ruleprocedure),
                     method = "nearest", data = matchdat)

lm_treat5 <- lm(win5 ~ plasoejv + as.factor(province) + as.factor(ruleyear) #+yearchina
                + as.factor(plapublic)
                + as.factor(plahq)
                +as.factor(casetype)
                #+ as.factor(plafvc)
                + as.factor(plaind) 
                + yearchina
                #+ placapital
                #+as.factor(courttype)
                +as.factor(ruleprocedure)
                , data = match.data(mod_match))


summary(lm_treat5)  # If the coefficient estimate for plasoejv is different from Table B6 (SOE JV), rerun the above chunk of codes.
stargazer(lm_treat5)



# Comp >= full
matchdat = dat[c("province","casetype","plahq","yearchina", "ruleyear","ruleprocedure", "plaind","win6","plasoejv","plapublic")]
matchdat = na.omit(matchdat)

mod_match <- matchit(plasoejv ~ as.factor(province) + as.factor(ruleyear)   #+ as.factor(contract)
                     #+ as.factor(defind) #+ defpc
                     #+ as.factor(ipr)+ as.factor(admin) 
                     + as.factor(plapublic)
                     # +as.factor(plafvc)
                     + as.factor(plahq)
                     +as.factor(casetype)
                     + as.factor(plaind)
                     + yearchina
                     #+ placapital
                     #+as.factor(courttype)
                     +as.factor(ruleprocedure),
                     method = "nearest", data = matchdat)

lm_treat6 <- lm(win6 ~ plasoejv + as.factor(province) + as.factor(ruleyear) #+yearchina
                + as.factor(plapublic)
                + as.factor(plahq)
                +as.factor(casetype)
                #+ as.factor(plafvc)
                + as.factor(plaind) 
                + yearchina
                #+ placapital
                #+as.factor(courttype)
                +as.factor(ruleprocedure)
                , data = match.data(mod_match))


summary(lm_treat6)  # If the coefficient estimate for plasoejv is different from Table B6 (SOE JV), rerun the above chunk of codes.
stargazer(lm_treat6)


stargazer(lm_treat1,lm_treat2,lm_treat3,lm_treat4,lm_treat5,lm_treat6)



#### political connections mechanism 

# Judgement
matchdat = dat[c("province","casetype","plahq","yearchina", "ruleyear","ruleprocedure", "plaind","win1","plapc","plapublic")]
matchdat = na.omit(matchdat)

mod_match <- matchit(plapc ~ as.factor(province) + as.factor(ruleyear)   #+ as.factor(contract)
                     #+ as.factor(defind) #+ defpc
                     #+ as.factor(ipr)+ as.factor(admin) 
                     + as.factor(plapublic)
                     # +as.factor(plafvc)
                     + as.factor(plahq)
                     +as.factor(casetype)
                     + as.factor(plaind)
                     + yearchina
                     #+ placapital
                     #+as.factor(courttype)
                     +as.factor(ruleprocedure),
                     method = "nearest", data = matchdat)

lm_treat1 <- lm(win1 ~ plapc + as.factor(province) + as.factor(ruleyear) #+yearchina
                + as.factor(plapublic)
                + as.factor(plahq)
                +as.factor(casetype)
                #+ as.factor(plafvc)
                + as.factor(plaind) 
                + yearchina
                #+ placapital
                #+as.factor(courttype)
                +as.factor(ruleprocedure)
                , data = match.data(mod_match))


summary(lm_treat1)  # If the coefficient estimate for plapc is different from Table B6 (Political Connections), rerun the above chunk of codes.
stargazer(lm_treat1)



# Court fee
matchdat = dat[c("province","casetype","plahq","yearchina", "ruleyear","ruleprocedure", "plaind","win2","plapc","plapublic")]
matchdat = na.omit(matchdat)

mod_match <- matchit(plapc ~ as.factor(province) + as.factor(ruleyear)   #+ as.factor(contract)
                     #+ as.factor(defind) #+ defpc
                     #+ as.factor(ipr)+ as.factor(admin) 
                     + as.factor(plapublic)
                     # +as.factor(plafvc)
                     + as.factor(plahq)
                     +as.factor(casetype)
                     + as.factor(plaind)
                     + yearchina
                     #+ placapital
                     #+as.factor(courttype)
                     +as.factor(ruleprocedure),
                     method = "nearest", data = matchdat)

lm_treat2 <- lm(win2 ~ plapc + as.factor(province) + as.factor(ruleyear) #+yearchina
                + as.factor(plapublic)
                + as.factor(plahq)
                +as.factor(casetype)
                #+ as.factor(plafvc)
                + as.factor(plaind) 
                + yearchina
                #+ placapital
                #+as.factor(courttype)
                +as.factor(ruleprocedure)
                , data = match.data(mod_match))


summary(lm_treat2)  # If the coefficient estimate for plapc is different from Table B6 (Political Connections), rerun the above chunk of codes.
stargazer(lm_treat2)



# Comp > 0 
matchdat = dat[c("province","casetype","plahq","yearchina", "ruleyear","ruleprocedure", "plaind","win3","plapc","plapublic")]
matchdat = na.omit(matchdat)

mod_match <- matchit(plapc ~ as.factor(province) + as.factor(ruleyear)   #+ as.factor(contract)
                     #+ as.factor(defind) #+ defpc
                     #+ as.factor(ipr)+ as.factor(admin) 
                     + as.factor(plapublic)
                     # +as.factor(plafvc)
                     + as.factor(plahq)
                     +as.factor(casetype)
                     + as.factor(plaind)
                     + yearchina
                     #+ placapital
                     #+as.factor(courttype)
                     +as.factor(ruleprocedure),
                     method = "nearest", data = matchdat)

lm_treat3 <- lm(win3 ~ plapc + as.factor(province) + as.factor(ruleyear) #+yearchina
                + as.factor(plapublic)
                + as.factor(plahq)
                +as.factor(casetype)
                #+ as.factor(plafvc)
                + as.factor(plaind) 
                + yearchina
                #+ placapital
                #+as.factor(courttype)
                +as.factor(ruleprocedure)
                , data = match.data(mod_match))


summary(lm_treat3)  # If the coefficient estimate for plapc is different from Table B6 (Political Connections), rerun the above chunk of codes.
stargazer(lm_treat3)



# Comp > 1/4
matchdat = dat[c("province","casetype","plahq","yearchina", "ruleyear","ruleprocedure", "plaind","win4","plapc","plapublic")]
matchdat = na.omit(matchdat)

mod_match <- matchit(plapc ~ as.factor(province) + as.factor(ruleyear)   #+ as.factor(contract)
                     #+ as.factor(defind) #+ defpc
                     #+ as.factor(ipr)+ as.factor(admin) 
                     + as.factor(plapublic)
                     # +as.factor(plafvc)
                     + as.factor(plahq)
                     +as.factor(casetype)
                     + as.factor(plaind)
                     + yearchina
                     #+ placapital
                     #+as.factor(courttype)
                     +as.factor(ruleprocedure),
                     method = "nearest", data = matchdat)

lm_treat4 <- lm(win4 ~ plapc + as.factor(province) + as.factor(ruleyear) #+yearchina
                + as.factor(plapublic)
                + as.factor(plahq)
                +as.factor(casetype)
                #+ as.factor(plafvc)
                + as.factor(plaind) 
                + yearchina
                #+ placapital
                #+as.factor(courttype)
                +as.factor(ruleprocedure)
                , data = match.data(mod_match))


summary(lm_treat4)  # If the coefficient estimate for plapc is different from Table B6 (Political Connections), rerun the above chunk of codes.
stargazer(lm_treat4)



# Comp > 1/2 
matchdat = dat[c("province","casetype","plahq","yearchina", "ruleyear","ruleprocedure", "plaind","win5","plapc","plapublic")]
matchdat = na.omit(matchdat)

mod_match <- matchit(plapc ~ as.factor(province) + as.factor(ruleyear)   #+ as.factor(contract)
                     #+ as.factor(defind) #+ defpc
                     #+ as.factor(ipr)+ as.factor(admin) 
                     + as.factor(plapublic)
                     # +as.factor(plafvc)
                     + as.factor(plahq)
                     +as.factor(casetype)
                     + as.factor(plaind)
                     + yearchina
                     #+ placapital
                     #+as.factor(courttype)
                     +as.factor(ruleprocedure),
                     method = "nearest", data = matchdat)

lm_treat5 <- lm(win5 ~ plapc + as.factor(province) + as.factor(ruleyear) #+yearchina
                + as.factor(plapublic)
                + as.factor(plahq)
                +as.factor(casetype)
                #+ as.factor(plafvc)
                + as.factor(plaind) 
                + yearchina
                #+ placapital
                #+as.factor(courttype)
                +as.factor(ruleprocedure)
                , data = match.data(mod_match))


summary(lm_treat5)  # If the coefficient estimate for plapc is different from Table B6 (Political Connections), rerun the above chunk of codes.
stargazer(lm_treat5)



# Comp >= full
matchdat = dat[c("province","casetype","plahq","yearchina", "ruleyear","ruleprocedure", "plaind","win6","plapc","plapublic")]
matchdat = na.omit(matchdat)

mod_match <- matchit(plapc ~ as.factor(province) + as.factor(ruleyear)   #+ as.factor(contract)
                     #+ as.factor(defind) #+ defpc
                     #+ as.factor(ipr)+ as.factor(admin) 
                     + as.factor(plapublic)
                     # +as.factor(plafvc)
                     + as.factor(plahq)
                     +as.factor(casetype)
                     + as.factor(plaind)
                     + yearchina
                     #+ placapital
                     #+as.factor(courttype)
                     +as.factor(ruleprocedure),
                     method = "nearest", data = matchdat)

lm_treat6 <- lm(win6 ~ plapc + as.factor(province) + as.factor(ruleyear) #+yearchina
                + as.factor(plapublic)
                + as.factor(plahq)
                +as.factor(casetype)
                #+ as.factor(plafvc)
                + as.factor(plaind) 
                + yearchina
                #+ placapital
                #+as.factor(courttype)
                +as.factor(ruleprocedure)
                , data = match.data(mod_match))


summary(lm_treat6)  # If the coefficient estimate for plapc is different from Table B6 (Political Connections), rerun the above chunk of codes.
stargazer(lm_treat6)


stargazer(lm_treat1,lm_treat2,lm_treat3,lm_treat4,lm_treat5,lm_treat6)




#### political connections mechanism (narrow)

# Judgement
matchdat = dat[c("province","casetype","plahq","yearchina", "ruleyear","ruleprocedure", "plaind","win1","plapc2","plapublic")]
matchdat = na.omit(matchdat)

mod_match <- matchit(plapc2 ~ as.factor(province) + as.factor(ruleyear)   #+ as.factor(contract)
                     #+ as.factor(defind) #+ defpc
                     #+ as.factor(ipr)+ as.factor(admin) 
                     + as.factor(plapublic)
                     # +as.factor(plafvc)
                     + as.factor(plahq)
                     +as.factor(casetype)
                     + as.factor(plaind)
                     + yearchina
                     #+ placapital
                     #+as.factor(courttype)
                     +as.factor(ruleprocedure),
                     method = "nearest", data = matchdat)

lm_treat1 <- lm(win1 ~ plapc2 + as.factor(province) + as.factor(ruleyear) #+yearchina
                + as.factor(plapublic)
                + as.factor(plahq)
                +as.factor(casetype)
                #+ as.factor(plafvc)
                + as.factor(plaind) 
                + yearchina
                #+ placapital
                #+as.factor(courttype)
                +as.factor(ruleprocedure)
                , data = match.data(mod_match))


summary(lm_treat1)  # If the coefficient estimate for plapc is different from Table B6 (Political Connections (narrow)), rerun the above chunk of codes.
stargazer(lm_treat1)



# Court fee
matchdat = dat[c("province","casetype","plahq","yearchina", "ruleyear","ruleprocedure", "plaind","win2","plapc2","plapublic")]
matchdat = na.omit(matchdat)

mod_match <- matchit(plapc2 ~ as.factor(province) + as.factor(ruleyear)   #+ as.factor(contract)
                     #+ as.factor(defind) #+ defpc
                     #+ as.factor(ipr)+ as.factor(admin) 
                     + as.factor(plapublic)
                     # +as.factor(plafvc)
                     + as.factor(plahq)
                     +as.factor(casetype)
                     + as.factor(plaind)
                     + yearchina
                     #+ placapital
                     #+as.factor(courttype)
                     +as.factor(ruleprocedure),
                     method = "nearest", data = matchdat)

lm_treat2 <- lm(win2 ~ plapc2 + as.factor(province) + as.factor(ruleyear) #+yearchina
                + as.factor(plapublic)
                + as.factor(plahq)
                +as.factor(casetype)
                #+ as.factor(plafvc)
                + as.factor(plaind) 
                + yearchina
                #+ placapital
                #+as.factor(courttype)
                +as.factor(ruleprocedure)
                , data = match.data(mod_match))


summary(lm_treat2)  # If the coefficient estimate for plapc is different from Table B6 (Political Connections (narrow)), rerun the above chunk of codes.
stargazer(lm_treat2)



# Comp > 0 
matchdat = dat[c("province","casetype","plahq","yearchina", "ruleyear","ruleprocedure", "plaind","win3","plapc2","plapublic")]
matchdat = na.omit(matchdat)

mod_match <- matchit(plapc2 ~ as.factor(province) + as.factor(ruleyear)   #+ as.factor(contract)
                     #+ as.factor(defind) #+ defpc
                     #+ as.factor(ipr)+ as.factor(admin) 
                     + as.factor(plapublic)
                     # +as.factor(plafvc)
                     + as.factor(plahq)
                     +as.factor(casetype)
                     + as.factor(plaind)
                     + yearchina
                     #+ placapital
                     #+as.factor(courttype)
                     +as.factor(ruleprocedure),
                     method = "nearest", data = matchdat)

lm_treat3 <- lm(win3 ~ plapc2 + as.factor(province) + as.factor(ruleyear) #+yearchina
                + as.factor(plapublic)
                + as.factor(plahq)
                +as.factor(casetype)
                #+ as.factor(plafvc)
                + as.factor(plaind) 
                + yearchina
                #+ placapital
                #+as.factor(courttype)
                +as.factor(ruleprocedure)
                , data = match.data(mod_match))


summary(lm_treat3)  # If the coefficient estimate for plapc is different from Table B6 (Political Connections (narrow)), rerun the above chunk of codes.
stargazer(lm_treat3)



# Comp > 1/4
matchdat = dat[c("province","casetype","plahq","yearchina", "ruleyear","ruleprocedure", "plaind","win4","plapc2","plapublic")]
matchdat = na.omit(matchdat)

mod_match <- matchit(plapc2 ~ as.factor(province) + as.factor(ruleyear)   #+ as.factor(contract)
                     #+ as.factor(defind) #+ defpc
                     #+ as.factor(ipr)+ as.factor(admin) 
                     + as.factor(plapublic)
                     # +as.factor(plafvc)
                     + as.factor(plahq)
                     +as.factor(casetype)
                     + as.factor(plaind)
                     + yearchina
                     #+ placapital
                     #+as.factor(courttype)
                     +as.factor(ruleprocedure),
                     method = "nearest", data = matchdat)

lm_treat4 <- lm(win4 ~ plapc2 + as.factor(province) + as.factor(ruleyear) #+yearchina
                + as.factor(plapublic)
                + as.factor(plahq)
                +as.factor(casetype)
                #+ as.factor(plafvc)
                + as.factor(plaind) 
                + yearchina
                #+ placapital
                #+as.factor(courttype)
                +as.factor(ruleprocedure)
                , data = match.data(mod_match))


summary(lm_treat4)  # If the coefficient estimate for plapc is different from Table B6 (Political Connections (narrow)), rerun the above chunk of codes.
stargazer(lm_treat4)



# Comp > 1/2 
matchdat = dat[c("province","casetype","plahq","yearchina", "ruleyear","ruleprocedure", "plaind","win5","plapc2","plapublic")]
matchdat = na.omit(matchdat)

mod_match <- matchit(plapc2 ~ as.factor(province) + as.factor(ruleyear)   #+ as.factor(contract)
                     #+ as.factor(defind) #+ defpc
                     #+ as.factor(ipr)+ as.factor(admin) 
                     + as.factor(plapublic)
                     # +as.factor(plafvc)
                     + as.factor(plahq)
                     +as.factor(casetype)
                     + as.factor(plaind)
                     + yearchina
                     #+ placapital
                     #+as.factor(courttype)
                     +as.factor(ruleprocedure),
                     method = "nearest", data = matchdat)

lm_treat5 <- lm(win5 ~ plapc2 + as.factor(province) + as.factor(ruleyear) #+yearchina
                + as.factor(plapublic)
                + as.factor(plahq)
                +as.factor(casetype)
                #+ as.factor(plafvc)
                + as.factor(plaind) 
                + yearchina
                #+ placapital
                #+as.factor(courttype)
                +as.factor(ruleprocedure)
                , data = match.data(mod_match))


summary(lm_treat5)  # If the coefficient estimate for plapc is different from Table B6 (Political Connections (narrow)), rerun the above chunk of codes.
stargazer(lm_treat5)



# Comp >= full
matchdat = dat[c("province","casetype","plahq","yearchina", "ruleyear","ruleprocedure", "plaind","win6","plapc2","plapublic")]
matchdat = na.omit(matchdat)

mod_match <- matchit(plapc2 ~ as.factor(province) + as.factor(ruleyear)   #+ as.factor(contract)
                     #+ as.factor(defind) #+ defpc
                     #+ as.factor(ipr)+ as.factor(admin) 
                     + as.factor(plapublic)
                     # +as.factor(plafvc)
                     + as.factor(plahq)
                     +as.factor(casetype)
                     + as.factor(plaind)
                     + yearchina
                     #+ placapital
                     #+as.factor(courttype)
                     +as.factor(ruleprocedure),
                     method = "nearest", data = matchdat)

lm_treat6 <- lm(win6 ~ plapc2 + as.factor(province) + as.factor(ruleyear) #+yearchina
                + as.factor(plapublic)
                + as.factor(plahq)
                +as.factor(casetype)
                #+ as.factor(plafvc)
                + as.factor(plaind) 
                + yearchina
                #+ placapital
                #+as.factor(courttype)
                +as.factor(ruleprocedure)
                , data = match.data(mod_match))


summary(lm_treat6)  # If the coefficient estimate for plapc is different from Table B6 (Political Connections (narrow)), rerun the above chunk of codes.
stargazer(lm_treat6)


stargazer(lm_treat1,lm_treat2,lm_treat3,lm_treat4,lm_treat5,lm_treat6)

#___________________________________________________________________________________________________________________




######## Table C1 ########

resind = c("A","B","D","E","G","I","J","K","L","P","Q","R","S","T")
dat$resind = ifelse(dat$plaind %in% resind, 1,0)

# Judgement
dd1 = glm.cluster(win1~plasoejv*resind  + as.factor(province) + as.factor(casetype) # courttype
                  + as.factor(plahq) +as.factor(ruleprocedure) + + as.factor(ruleyear) + as.factor(plafvc)    #+ as.factor(plaind)  #+ as.factor(plahq) #+ as.factor(ipr)+ as.factor(admin) #+as.numeric(as.character(plarevenue)) #+ scale(yearchina)
                  , cluster="province",family = binomial(),data = dat)


# Court fee
dd2 = glm.cluster(win2~plasoejv*resind  + as.factor(province) + as.factor(casetype) # courttype
                  + as.factor(plahq) +as.factor(ruleprocedure) + + as.factor(ruleyear) + as.factor(plafvc)    #+ as.factor(plaind)  #+ as.factor(plahq) #+ as.factor(ipr)+ as.factor(admin) #+as.numeric(as.character(plarevenue)) #+ scale(yearchina)
                  , cluster="province",family = binomial(),data = dat)


# Comp > 0
dd3 = glm.cluster(win3~plasoejv*resind  + as.factor(province) + as.factor(casetype) # courttype
                  + as.factor(plahq) +as.factor(ruleprocedure) + + as.factor(ruleyear) + as.factor(plafvc)    #+ as.factor(plaind)  #+ as.factor(plahq) #+ as.factor(ipr)+ as.factor(admin) #+as.numeric(as.character(plarevenue)) #+ scale(yearchina)
                  , cluster="province",family = binomial(),data = dat)


# Comp > 1/4
dd4 = glm.cluster(win4~plasoejv*resind  + as.factor(province) + as.factor(casetype) # courttype
                  + as.factor(plahq) +as.factor(ruleprocedure) + + as.factor(ruleyear) + as.factor(plafvc)    #+ as.factor(plaind)  #+ as.factor(plahq) #+ as.factor(ipr)+ as.factor(admin) #+as.numeric(as.character(plarevenue)) #+ scale(yearchina)
                  , cluster="province",family = binomial(),data = dat)


# Comp > 1/2
dd5 = glm.cluster(win5~plasoejv*resind  + as.factor(province) + as.factor(casetype) # courttype
                  + as.factor(plahq) +as.factor(ruleprocedure) + + as.factor(ruleyear) + as.factor(plafvc)    #+ as.factor(plaind)  #+ as.factor(plahq) #+ as.factor(ipr)+ as.factor(admin) #+as.numeric(as.character(plarevenue)) #+ scale(yearchina)
                  , cluster="province",family = binomial(),data = dat)


# Comp >= full
dd6 = glm.cluster(win6~plasoejv*resind  + as.factor(province) + as.factor(casetype) # courttype
                  + as.factor(plahq) +as.factor(ruleprocedure) + + as.factor(ruleyear) + as.factor(plafvc)    #+ as.factor(plaind)  #+ as.factor(plahq) #+ as.factor(ipr)+ as.factor(admin) #+as.numeric(as.character(plarevenue)) #+ scale(yearchina)
                  , cluster="province",family = binomial(),data = dat)


texreg(list(dd1$glm_res,dd2$glm_res,dd3$glm_res,dd4$glm_res,dd5$glm_res,dd6$glm_res),
       override.se = list(summary(dd1)[,2],summary(dd2)[,2],summary(dd3)[,2],summary(dd4)[,2],summary(dd5)[,2],summary(dd6)[,2]),
       override.pvalues = list(summary(dd1)[,4],summary(dd2)[,4],summary(dd3)[,4],summary(dd4)[,4],summary(dd5)[,4],summary(dd6)[,4]),digits=3,
       stars = c(0.001, 0.01, 0.05,0.1))