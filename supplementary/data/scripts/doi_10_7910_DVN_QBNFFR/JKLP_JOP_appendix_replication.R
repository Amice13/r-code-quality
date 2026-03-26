
##------------------------------------------------------------------------------
## JOP-Short-2020 APPENDIX Replication -----------------------------------------
## Kucik/Peritz "How do third parties affect compliance in the trade regime? "
## Update 6 February 2020
##------------------------------------------------------------------------------



##------------------------------------------------------------
## Preliminaries ---------------------------------------------

rm(list=ls(all=T))

require(haven)
require(zeligverse)
require(sandwich)
require(lmtest)
require(sampleSelection)
require(stargazer)
require(dotwhisker)
require(ggplot2)
library(dplyr)
library(MASS)
library(gdata)


setwd("/Users/lperitz/Dropbox/JK_LP")

d <- as.data.frame(read_dta("jop2020_replication/Kucik_Peritz_JOP2020.dta"))

d.A1 <- as.data.frame(read_dta("jop2020_replication/Kucik_Peritz_JOP2020_tradepostdispute.dta"))

d.TRADE <- as.data.frame(read_dta("jop2020_replication/Kucik_Peritz_JOP2020_bilateraltrade.dta"))


##------------------------------------------------------------
## Appendix Part I - Discriminatory settlements ? ------------

## TABLE A1: COMPARING TRADE EFFECTS OF DISPUTES BY OUTCOME AND PARTICIPANT

# How much do non-participants get from MAS v. ruling? 
# Answer: Non-participants prefer ruings.

# 1-year
mean(d.A1$td1_nonpart[d.A1$early==1], na.rm=T)
mean(d.A1$td1_nonpart[d.A1$ruling==1], na.rm=T)

# 3-year
mean(d.A1$td3_nonpart[d.A1$early==1], na.rm=T)
mean(d.A1$td3_nonpart[d.A1$ruling==1], na.rm=T)


# How much do third parties get from MAS v. ruling?
# Answer: TPs prefer rulings.

# 1-year
mean(d.A1$td1_third[d.A1$early==1], na.rm=T)
mean(d.A1$td1_third[d.A1$ruling==1], na.rm=T)

# 3-year
mean(d.A1$td3_third[d.A1$early==1], na.rm=T)
mean(d.A1$td3_third[d.A1$ruling==1], na.rm=T)

# How much do complainants get from the average MAS v. ruling?
# Answer: Complainants prefer MAS.

# 1-year
mean(d.A1$td1_compl[d.A1$early==1], na.rm=T)
mean(d.A1$td1_compl[d.A1$ruling==1], na.rm=T)

# 3-year
mean(d.A1$td3_compl[d.A1$early==1], na.rm=T)
mean(d.A1$td3_compl[d.A1$ruling==1], na.rm=T)



##------------------------------------------------------------
## Appendix Part II - Coding Compliance ----------------------


# TABLE A2:

c.list <- as.data.frame(sort( unique(d$resp1[d$outcome=="MAS"|d$outcome=="Ruling"]))); colnames(c.list)<- c("resp1")
v1 <- as.data.frame(table(d$resp1[d$outcome=="MAS"])); colnames(v1)<- c("resp1", "MAS")
v2 <- as.data.frame(table(d$resp1[d$outcome=="Ruling"&d$comply_n==2])) ; colnames(v2)<- c("resp1", "Full")
v3 <- as.data.frame(table(d$resp1[d$outcome=="Ruling"&d$comply_n==1])) ; colnames(v3)<- c("resp1", "Partial")
v4 <- as.data.frame(table(d$resp1[d$outcome=="Ruling"&d$comply_n==0])) ; colnames(v4)<- c("resp1", "No")

tab.A2 <- merge(c.list, v1, by="resp1", all=T)
tab.A2 <- merge(tab.A2, v2, by="resp1", all=T)
tab.A2 <- merge(tab.A2, v3, by="resp1", all=T)
tab.A2 <- merge(tab.A2, v4, by="resp1", all=T)

tab.A2

##------------------------------------------------------------
## Appendix Part III - Robustness of Baseline Estimates  -----

## FIGURE A1:

par(mar=c(5,5,3,3))
boxplot(d$disputedprod_trade~d$comply_b, ylim = c(-1000,30000), frame.plot=F, xaxt="n", yaxt = "n", yaxs="i",boxwex=.75, xlab = "", ylab = "")
axis(side = 1, at = c(0,1,2,3), labels = c("","Noncompliance", "Compliance","")) 
axis(side = 2, at = c(0,10000, 20000, 30000)) 
mtext("Disputed Trade, All Members, Millions", side = 2, line = 3)

## FIGURE A2: 

varlist <- c("ds", "comp1", "resp1", "comply_b", "comply_n",
             "lnthird", "trade_share_rctp", "gdp_share", "r_greatpower",
             "times_ruled", "lnclaims", "article_xxii","remedy", "clarity",
             "dispute_combined")

d.sub1 <- na.omit(subset(d,select=varlist))                               
d.sub2 <- na.omit(subset(d,select=c("disputedprod_lntrade",varlist)))

m1a <- glm(comply_b ~ lnthird+trade_share_rctp+times_ruled+gdp_share+r_greatpower+article_xxii+remedy+clarity,
          data = d.sub1,
          family=binomial(link="probit"))

m1b <- polr(as.factor(comply_n) ~ lnthird+trade_share_rctp+times_ruled+gdp_share+r_greatpower+article_xxii+remedy+clarity,
           data = d.sub1, method = "probit")

# bilat product imports
m5.1 <- glm(comply_b ~ lnthird+trade_share_rctp+times_ruled+gdp_share+r_greatpower+article_xxii+remedy+clarity+ disputedprod_lntrade,
           data = d.sub2,
           family=binomial(link="probit"))

# Plot
#pdf("drafts/fig_tab/coefplot.pdf")
dwplot(list(m1a, m1b, m5.1), conf.level = .95,
	vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) %>% # plot line at zero _behind_ coefs
	relabel_predictors(c(lnthird = "Third Parties",                       
                         trade_share_rctp = "Trade Share", 
                         times_ruled = "Times Ruled", 
                         gdp_share = "GDP Share", 
                         article_xxii = "Article XXII", 
                         remedy = "Remedy", 
                         clarity = "Clarity",
                         r_greatpower= "EU/US Resp", 
                         disputedprod_lntrade = "Disputed Imports")) +
     theme_bw() + xlab("Coefficient Estimate") + ylab("") +
     geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
     ggtitle("Predicting Compliance in WTO Disputes") +
     theme(plot.title = element_text(face="bold"),
           legend.position = c(0.007, 0.01),
           legend.justification = c(0, 0), 
           legend.background = element_rect(colour="grey80"),
           legend.title = element_blank() )
#dev.off()


## TABLE A3:  ROBUSTNESS OF ESTIMATES TO DISPUTE STAKES
#  Repeat filing, duration (mo), compliance ratio 

d.sub <- na.omit( subset(d, select = c("ds","comply_b", "comply_n", 
                                       "lnthird",
                                       "trade_share_rctp", "gdp_share", "r_greatpower",
                                       "times_ruled",  "article_xxii","remedy","dispute_combined",
                                       "is_repeat", "ruling_ratio", "lnsued", "lnduration" )) )


a3.1 <- glm(comply_b ~ lnthird+trade_share_rctp+times_ruled+gdp_share+r_greatpower+article_xxii+remedy+is_repeat,
            data = d.sub,
            family=binomial(link="probit"))
a3.1.cluster <- coeftest(a3.1, vcov=vcovCL(a3.1, factor(d.sub$dispute_combined)))

a3.2 <- glm(comply_b ~ lnthird +trade_share_rctp+times_ruled+gdp_share+r_greatpower+article_xxii+remedy+I(ruling_ratio*1000),
            data = d.sub,
            family=binomial(link="probit"))
a3.2.cluster <- coeftest(a3.2, vcov=vcovCL(a3.2, factor(d.sub$dispute_combined)))

a3.3 <- glm(comply_b ~ lnthird +trade_share_rctp+times_ruled+gdp_share+r_greatpower+article_xxii+remedy+lnsued,
            data = d.sub,
            family=binomial(link="probit"))
a3.3.cluster <- coeftest(a3.3, vcov=vcovCL(a3.3, factor(d.sub$dispute_combined)))

a3.4 <- glm(comply_b ~ lnthird +trade_share_rctp+times_ruled+gdp_share+r_greatpower+article_xxii+remedy+lnduration,
            data = d.sub,
            family=binomial(link="probit"))
a3.4.cluster <- coeftest(a3.4, vcov=vcovCL(a3.4, factor(d.sub$dispute_combined)))


# Table A3
stargazer(a3.1.cluster, a3.2.cluster, a3.3.cluster, a3.4.cluster, 
			star.char = c(".","*","**"), type="text", no.space=T)




## TABLE A4:  COMPLIANCE TIMING and MEASURING THIRD PARTY PARTICIPATION
 
## Timely versus Delayed versus No Compliance 

d4.1 <- na.omit( subset(d, select = c("ds", "comply_b","comply_n", "comply_rpt_n",
                                       "lnthird",
                                       "trade_share_rctp", "gdp_share", "r_greatpower",
                                       "times_ruled",  "article_xxii","remedy","dispute_combined")))

# one stage
a4.1 <- polr(as.factor(comply_rpt_n) ~ lnthird + trade_share_rctp + times_ruled + gdp_share +r_greatpower+ article_xxii+remedy,
            data = d4.1, method = "probit")
a4.1.cluster <- coeftest(a4.1, vcov=vcovCL(a4.1, factor(d4.1$dispute_combined) ))


# two stage
a4.2heck <- heckit(ruling ~ lnthird+trade_share_rctp+times_ruled+gdp_share+r_greatpower+article_xxii+remedy+systemic_new+art_agric+art_sps+ art_service,
               as.factor(comply_rpt_n) ~ lnthird+trade_share_rctp+times_ruled+gdp_share+ r_greatpower +remedy + article_xxii,
               data = d, method = "2step")


## Measure Third Parties 

d4.3 <- na.omit( subset(d, select = c("ds","comply_b", "comply_n", 
                                       "lnthird", "thirdparties_b","ln_proC","ln_proR",
                                       "trade_share_rctp", "gdp_share", "r_greatpower",
                                       "times_ruled",  "article_xxii","remedy","dispute_combined")))

a4.3 <- glm(comply_b ~ thirdparties_b +trade_share_rctp+times_ruled+gdp_share+r_greatpower+article_xxii+remedy, data = d4.3,family=binomial(link="probit"))
a4.3.cluster <- coeftest(a4.3, vcov=vcovCL(a4.3, factor(d4.3$dispute_combined)))

a4.4 <- polr(as.factor(comply_n) ~ thirdparties_b + trade_share_rctp + times_ruled + gdp_share +r_greatpower+ article_xxii+remedy,data = d4.3, method = "probit")
a4.4.cluster <- coeftest(a4.4, vcov=vcovCL(a4.4, factor(d4.3$dispute_combined)))
                                      
a4.5 <- glm(comply_b ~  ln_proC + ln_proR + trade_share_rctp+times_ruled+gdp_share+r_greatpower+article_xxii+remedy, data = d4.3,family=binomial(link="probit"))
a4.5.cluster <- coeftest(a4.5, vcov=vcovCL(a4.5, factor(d4.3 $dispute_combined)))

a4.6 <- polr(as.factor(comply_n) ~ ln_proC + ln_proR + trade_share_rctp + times_ruled + gdp_share +r_greatpower+ article_xxii+remedy,data = d4.3, method = "probit")
a4.6.cluster <- coeftest(a4.6, vcov=vcovCL(a4.6, factor(d4.3$dispute_combined)))

# Table A4 (print outcome stage; print selection stage)
stargazer(a4.1.cluster, a4.2heck, a4.3.cluster, a4.4.cluster, a4.5.cluster, a4.6.cluster, no.space=T, type="text",dep.var.labels.include = TRUE,star.char = c(".","*","**"))
stargazer(a4.2heck, dep.var.labels.include = TRUE, selection.equation=TRUE, no.space=T, star.char = c(".","*","**"), type="text")



## TABLE A5:  ISSUE COMPLEXITY and DISPUTED TOPIC  
m5.ag <- glm(comply_b ~	lnthird+trade_share_rctp+times_ruled+gdp_share+r_greatpower+article_xxii+ 
							art_agric, 
							data = d, family=binomial(link="probit"))
m5.ad <- glm(comply_b ~	lnthird +trade_share_rctp+times_ruled+gdp_share+ r_greatpower +article_xxii+ 
							art_ad, 
							data = d, family=binomial(link="probit"))
m5.cvd <- glm(comply_b ~ lnthird +trade_share_rctp+times_ruled+gdp_share+ r_greatpower +article_xxii+ 
							art_cvd, 
							data = d, family=binomial(link="probit"))													
m5.sps <- glm(comply_b ~ lnthird +trade_share_rctp+times_ruled+gdp_share+ r_greatpower +article_xxii+ 
							art_sps, 
							data = d, family=binomial(link="probit"))							
m5.sg <- glm(comply_b ~	lnthird +trade_share_rctp+times_ruled+gdp_share+ r_greatpower +article_xxii+ 
							art_sg, 
							data = d, family=binomial(link="probit"))								
m5.ser <- glm(comply_b ~ lnthird +trade_share_rctp+times_ruled+gdp_share+ r_greatpower +article_xxii+ 
							art_service, 
							data = d, family=binomial(link="probit"))							
m5.sub <- glm(comply_b ~ lnthird +trade_share_rctp+times_ruled+gdp_share+ r_greatpower +article_xxii+ 
							art_subs, 
							data = d, family=binomial(link="probit"))	
m5.tbt <- glm(comply_b ~ lnthird +trade_share_rctp+times_ruled+gdp_share+ r_greatpower +article_xxii+ 
							art_tbt, 
							data = d, family=binomial(link="probit"))	
m5.trim <- glm(comply_b ~ lnthird +trade_share_rctp+times_ruled+gdp_share+ r_greatpower +article_xxii+ 
							art_trims, 
							data = d, family=binomial(link="probit"))	
m5.trip <- glm(comply_b ~ lnthird +trade_share_rctp+times_ruled+gdp_share+ r_greatpower +article_xxii+ 
							art_trips, 
							data = d, family=binomial(link="probit"))	
m5.tex <- glm(comply_b ~ lnthird +trade_share_rctp+times_ruled+gdp_share+ r_greatpower +article_xxii+ 
							art_textile, 
							data = d, family=binomial(link="probit"))	
m5.issue <- glm(comply_b ~ lnthird +trade_share_rctp+times_ruled+gdp_share+ r_greatpower +article_xxii+ 
							issue_ct, 
							data = d, family=binomial(link="probit"))	

# Table A5
stargazer(m5.ad, m5.cvd, m5.sg, m5.sub, m5.tbt, m5.trim, m5.trip, m5.tex, m5.issue, no.space=T, star.char = c(".","*","**"), type = "text")


## TABLE A6:  CODING LANGUAGE, TIME TRENDS, DEFENDANT DOMESTIC POLITICS

d.sub6 <- na.omit(subset(d, select = c("ds", "comp1", "resp1", "syear", 
				"comply_b", "comply_n","lnthird", "trade_share_rctp", 
                "gdp_share", "c_lngdp", "r_lngdp",
                "r_greatpower", "c_greatpower","EU_US_dispute",
                "times_ruled", "lnclaims", "article_xxii","remedy",
                "dispute_combined","parliamentary", "polity2","laworderfromicrg")))

## Coding bias: omit Indo, Turkey, Thailand 
d.ITT <- d.sub6[d.sub6$resp1!="Indonesia" & d.sub6$resp1!="Thailand" & d.sub6$resp1!="Turkey", ]
m6a.ITT <- glm(comply_b ~ lnthird+trade_share_rctp+times_ruled+gdp_share+r_greatpower+article_xxii+remedy,
          data = d.ITT,family=binomial(link="probit"))
m6a.ITT.cluster <- coeftest(m6a.ITT, vcov=vcovCL(m6a.ITT, factor(d.ITT$dispute_combined) ))


## Year
m6a <- glm(comply_b ~ lnthird+trade_share_rctp+times_ruled+gdp_share+r_greatpower+article_xxii+remedy+syear,
          data = d.sub6,family=binomial(link="probit"))                   
m6a.cluster <- coeftest(m6a, vcov=vcovCL(m6a, factor(d.sub6$dispute_combined)))

## Domestic factors 
m6d.1 <- glm(comply_b ~ lnthird+trade_share_rctp+times_ruled+
			r_lngdp+c_lngdp+r_greatpower+
			article_xxii+remedy+polity2,
      data = d.sub6,family=binomial(link="probit"))
m6d.1.cluster <- coeftest(m6d.1, vcov=vcovCL(m6d.1, factor(d.sub6$dispute_combined)))
          	
m6d.2 <- glm(comply_b ~ lnthird+trade_share_rctp+times_ruled+
			r_lngdp+c_lngdp+r_greatpower+
			article_xxii+remedy+ laworderfromicrg,
      data = d.sub6,family=binomial(link="probit"))
m6d.2.cluster <- coeftest(m6d.2, vcov=vcovCL(m6d.2, factor(d.sub6$dispute_combined)))
          	
m6d.3 <- glm(comply_b ~ lnthird+trade_share_rctp+times_ruled+
			r_lngdp+c_lngdp+r_greatpower+
			article_xxii+remedy+parliamentary,
      data = d.sub6,family=binomial(link="probit"))          	
m6d.3.cluster <- coeftest(m6d.3, vcov=vcovCL(m6d.3, factor(d.sub6$dispute_combined)))

m6d.4 <- glm(comply_b ~ lnthird+trade_share_rctp+times_ruled+
			r_lngdp+c_lngdp+
			article_xxii+remedy + EU_US_dispute,
          	data = d.sub6,family=binomial(link="probit"))    
m6d.4.cluster <- coeftest(m6d.4, vcov=vcovCL(m6d.4, factor(d.sub6$dispute_combined) ))

## Table A6          	
stargazer(m6a.ITT.cluster, m6a.cluster, m6d.1.cluster, m6d.2.cluster, m6d.3.cluster, m6d.4.cluster, no.space = T, type = "text",star.char = c(".","*","**"))




##------------------------------------------------------------------------------
## Appendix Part IV - Add'l Robustness Checks for Selection Effects ------------

## TABLE A7: CORRELATIONS


# Table A7.1: selection stage variables
d.s <- subset(d, select = c("ruling","lnthird","trade_share_rctp","times_ruled","gdp_share","r_greatpower","article_xxii","remedy","clarity","systemic_new","art_agric","art_sps", "art_service"))
bob <- round(cor(d.s, use="complete.obs"), digits=3)
upperTriangle(bob)<-NA
bob


# Table A7.2: outcome stage
d.o <- subset(d, select = c("comply_b","lnthird","trade_share_rctp","times_ruled","gdp_share","r_greatpower","article_xxii","remedy","clarity","systemic_new","art_agric","art_sps", "art_service"), is.na(comply_b)==F)
fred <- round(cor(d.o, use="pairwise.complete.obs"), digits=3)
upperTriangle(fred)<-NA
fred


## TABLE A8: REPLICATING SIMILAR SELECTION MODELS IN LITERATURE

## Issue areas
hsens.1 <- selection(ruling ~ lnthird+systemic_new+
						art_agric+art_sps+art_service+
						art_ad+art_cvd+art_sg+art_subs+
						art_tbt+art_trims+art_trips,
            outcome = comply_b ~ lnthird+trade_share_rctp+times_ruled+gdp_share+r_greatpower+remedy+clarity+article_xxii,
            data = d, method = "2step")

## Johns & Pelc 2014 identification 
jope2014 <- selection(ruling ~ article_xxii+thirdparties_b +c_lngdp +r_lngdp 
						+comp_EU +resp_EU +comp_US +resp_US +systemic_new 
						+art_ad +art_sps +art_agric,
            comply_b ~ lnthird +trade_share_rctp+times_ruled+gdp_share+r_greatpower+remedy+clarity+article_xxii,
            data = d, method = "2step")

## Johns & Pelc 2019 identification 
jope2019 <- selection(ruling ~ thirdparties_b+ln_trade_total+c_lngdp+r_lngdp+log(comp1_legalex),
               comply_b ~ lnthird+trade_share_rctp+times_ruled+gdp_share+r_greatpower+remedy+clarity+article_xxii,
               data = d, method = "2step")

## Busch and Pelc 2010 indentification 
bupe2010 <-selection( ruling ~ thirdparties_b+systemic_new+n_cocompl+c_lngdp+r_lngdp+article_xxii+
							+lnclaims+nonviol_23+art_sps+art_agric+disputedprod_lntrade,
               comply_b ~ lnthird+trade_share_rctp+times_ruled+gdp_share+r_greatpower+remedy+clarity+article_xxii,
               data = d, method = "2step")

## Busch and Reinhardt 2006 identification 
bure2006 <-selection( ruling ~ thirdparties_b+systemic_new+EU_US_dispute +n_cocompl+c_lngdp+r_lngdp+ article_xxii+
							+nonviol_23+lnclaims+pol_sensitive+art_agric+ln_trade_total+is_merch1,
               comply_b ~ lnthird+trade_share_rctp+times_ruled+gdp_share+r_greatpower+remedy+clarity+article_xxii,
               data = d, method = "2step")


## Table A8: Heckman selection models summary

# outcome
stargazer(hsens.1, bure2006, bupe2010, jope2014, jope2019, 
          no.space = T, selection.equation = F, type="text",
          star.char = c(".","*","**"))

# selection
stargazer(hsens.1, bure2006, bupe2010, jope2014, jope2019, 
			no.space = T, selection.equation = T, type="text",
			star.char = c(".","*","**"))

# R^2
round(c(summary(hsens.1)[1]$rSquared$R2,
  summary(jope2014)[1]$rSquared$R2,
  summary(jope2019)[1]$rSquared$R2,
  summary(bupe2010)[1]$rSquared$R2,
  summary(bure2006)[1]$rSquared$R2), digits=3)


## TABLE A9: TRADE STAKES IN DISPUTE

# Third party participation in merchandise and non-merchandise disputes
# t-test - (log) third party count
t.test(d$lnthird[d$is_merch1==1], d$lnthird[d$is_merch1==0])

# Distributions look similar for merchandise and non-merchandise
hist(d$lnthird[d$is_merch1==1], xlab ="third party count, log", main ="", xlim = c(0,4), col=rgb(0.1,0.1,0.1,0.5))
hist(d$lnthird[d$is_merch1==0], xlab ="", ylab = "", main ="", col=rgb(0.8,0.8,0.8,0.5), axes=F, add=T)

## Analyze trade stakes with bilateral disputed product trade data


# Predict selection into complainant status, compared to nonparticipants and TP
r9.1 <-lm(is_complainant ~ sum_imports_n, d.TRADE)
r9.1c <- coeftest(r9.1, vcov=vcovCL(r9.1, factor(d.TRADE$dispute)))

r9.2 <-lm(is_complainant ~ sum_imports_n + remedy + clarity, d.TRADE)
r9.2c <- coeftest(r9.2, vcov=vcovCL(r9.2, factor(d.TRADE$dispute)))

r9.3 <-lm(is_complainant ~ 	sum_imports_n + art_agric + art_ad + art_cvd + art_sps + 
            art_sg + art_service + art_tbt + art_trims + art_trips + art_textile, d.TRADE)
r9.3c <- coeftest(r9.3, vcov=vcovCL(r9.3, factor(d.TRADE$dispute)))

r9.4 <-lm(is_complainant ~ sum_imports_n + I(year-1994)  + respondent_gp, d.TRADE)
r9.4c <- coeftest(r9.4, vcov=vcovCL(r9.4, factor(d.TRADE$dispute)))



## Predict selection into third party status, compared to nonparticipants
d.TRADE.sub <- d.TRADE[d.TRADE$is_complainant==0,]

r9.5 <-lm(is_thirdparty ~ sum_imports_n, d.TRADE.sub)
r9.5c <- coeftest(r9.5, vcov=vcovCL(r9.5, factor(d.TRADE.sub$dispute)))

r9.6 <-lm(is_thirdparty ~ sum_imports_n + remedy + clarity, d.TRADE.sub)
r9.6c <- coeftest(r9.6, vcov=vcovCL(r9.6, factor(d.TRADE.sub$dispute)))

r9.7 <-lm(is_thirdparty ~ sum_imports_n + art_agric + art_ad + art_cvd + art_sps + 
            art_sg + art_service + art_tbt + art_trims + art_trips + art_textile, d.TRADE.sub)
r9.7c <- coeftest(r9.7, vcov=vcovCL(r9.7, factor(d.TRADE.sub$dispute)))

r9.8 <-lm(is_thirdparty ~ sum_imports_n + I(year-1994) +respondent_gp, d.TRADE.sub)
r9.8c <- coeftest(r9.8, vcov=vcovCL(r9.8, factor(d.TRADE.sub$dispute)))

# Table A9: comparing determinants of complainant and third party status
stargazer(r9.1c, r9.2c, r9.3c, r9.4c, r9.5c, r9.6c, r9.7c, no.space=T,  star.char = c(".","*","**"), type="text")



## TABLE A10: WHICH DISPUTES ATTRACT THIRD PARTIES?

d.sub10 <- subset(d, select = c("ds","syear","comply_b","comply_n","lnclaims","outcome","ruling", "dispute_combined",                         
                  "art_agric","art_ad","art_cvd","art_sps","art_sg","art_service",
                  "art_subs","art_tbt","art_trims","art_trips","art_textile",
                  "remedy","clarity","thirdparties","lnthird"))

# issue area as predictor of TP count
r10.1 <- lm(thirdparties~lnclaims*ruling, d=d.sub10)
r10.1c <- coeftest(r10.1, vcov=vcovCL(r10.1, factor(d.sub10$dispute_combined)))

r10.2 <- lm(thirdparties~lnclaims*ruling + 
             art_agric + art_ad + art_cvd + art_sps + 
             art_sg + art_service + art_tbt + art_trims + 
             art_trips + art_textile, d=d.sub10)
r10.2c <- coeftest(r10.2, vcov=vcovCL(r10.2, factor(d.sub10$dispute_combined)))


# issue area as predictor of compliance
r10.3 <- lm(comply_b~lnclaims, d=d.sub10)
r10.3c <- coeftest(r10.3, vcov=vcovCL(r10.3, factor(d.sub10$dispute_combined)))

r10.4 <- lm(comply_b ~lnclaims + art_agric + art_ad + 						
             art_cvd + art_sps + art_sg + art_service + 
             art_tbt + art_trims +art_trips + art_textile, d=d.sub10)
r10.4c <- coeftest(r10.4, vcov=vcovCL(r10.4, factor(d.sub10$dispute_combined)))

# Table A10
stargazer(r10.1c, r10.2c, r10.3c, r10.4c, no.space = T, star.char = c(".","*","**"), type = "text")

# dispute groups
length(unique(d.sub10$dispute_combined))
length(unique(d.sub10$dispute_combined[is.na(d.sub10$comply_b)==F]))


#### END ####






