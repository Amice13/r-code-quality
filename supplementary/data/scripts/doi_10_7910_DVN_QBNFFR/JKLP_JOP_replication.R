## JOP-Short-2020 MAIN Replication ----------------------------------
## Kucik/Peritz "How do third parties affect compliance in the trade regime? "
## Update 11 February 2020



##------------------------------------------------------------
## Preliminaries -----------------------------------

rm(list=ls(all=T))

require(haven)
require(zeligverse)
require(sandwich)
require(lmtest)
require(sampleSelection)
require(stargazer)
require(MASS)

setwd("/Users/lperitz/Dropbox/JK_LP/ThirdParties")
d <- as.data.frame(read_dta("jop2020_replication/Kucik_Peritz_JOP2020.dta"))


##------------------------------------------------------------
## Descriptive statistics ------------------------------------

table(d$ruling)				  			    # 170 rulings from DS1-415
table(d$ruling[d$npanel_w>0])			# 160 rulings were adverse 
table(d$comply_b)						      # 155 adverse rulings (not overturned on appeal)
table(d$comply_n)						      # 101 complete compliance, 14 partial compliance, 40 noncompliance

# complainants and defendants
length(unique(d$comp1[is.na(d$comply_n)==F]))
length(unique(d$resp1[is.na(d$comply_n)==F]))

# US and EU
table(d$resp1[is.na(d$comply_n)==F])

# thirdparties
table(d$thirdparties)
table(d$thirdparties[is.na(d$comply_n)==F])
table(d$thirdparties[d$ruling==1])


##------------------------------------------------------------
## Table 1 ---------------------------------------------------

d.sub <-subset(d, comply_b==1|comply_b==0,
				select = c("ds", "comp1", "resp1",
				"comply_b", "comply_n", "lnthird", 
                "trade_share_rctp", "gdp_share", "r_greatpower",
                "times_ruled", "lnclaims", "article_xxii","remedy","clarity",
                "dispute_combined")) 
                                       
## Probit model with baseline controls 
m1a <- glm(comply_b ~ lnthird+
			trade_share_rctp+times_ruled+gdp_share+r_greatpower+article_xxii+remedy+clarity,
          	data = d.sub,
          	family=binomial(link="probit"))
m1a.cluster <- coeftest(m1a, vcov=vcovCL(m1a, factor(d.sub$dispute_combined) ))

# Ordered probit model with baseline controls 
m1b <- polr(as.factor(comply_n) ~ lnthird+
			trade_share_rctp+times_ruled+gdp_share+r_greatpower+article_xxii+remedy+clarity,
           	data = d.sub, 
           	method = "probit")
m1b.cluster <- coeftest(m1b, vcov=vcovCL(m1b, factor(d.sub$dispute_combined) ))

## Linear probability models with baseline controls
lm1a <- lm(comply_b ~ lnthird+
			trade_share_rctp+times_ruled+gdp_share+r_greatpower+article_xxii+remedy+clarity, 
			data = d.sub)
lm1a.cluster <- coeftest(lm1a, vcov=vcovCL(lm1a, factor(d.sub$dispute_combined) ))

lm1b <- lm(comply_n ~ lnthird+
			trade_share_rctp+times_ruled+gdp_share+ r_greatpower +article_xxii+remedy+clarity, 
			data = d.sub)
lm1b.cluster <- coeftest(lm1b, vcov=vcovCL(lm1b, factor(d.sub$dispute_combined) )) 


## Control for bilateral imports in disputed products
d.sub5 <- na.omit( subset(d, select = c("ds","comply_b", "comply_n", 
                                       "lnthird","r_greatpower",
                                       "trade_share_rctp", "gdp_share", "disputedprod_lntrade", 
                                       "times_ruled", "lnclaims", "article_xxii","remedy","clarity",
                                       "dispute_combined")) )

m5 <- glm(comply_b ~ lnthird+trade_share_rctp+times_ruled+gdp_share+r_greatpower+article_xxii+remedy+clarity+ disputedprod_lntrade,
           data = d.sub5,
           family=binomial(link="probit"))
m5.cluster <- coeftest(m5, vcov=vcovCL(m5, factor(d.sub5$dispute_combined))) 


## Control for industry employment share (US and EU respondents only)

d.sub6 <- na.omit( subset(d, select = c("ds","comply_b", "comply_n", 
                                        "lnthird","r_greatpower",
                                        "trade_share_rctp", "gdp_share", "employ_share_tot", 
                                        "times_ruled", "lnclaims", "article_xxii","remedy","clarity",
                                        "dispute_combined")) )
                                        
m6 <- glm(comply_b ~ lnthird+trade_share_rctp+times_ruled+gdp_share+article_xxii+remedy+clarity+log(employ_share_tot),
            data = d.sub6,
            family=binomial(link="probit"))
m6.cluster <- coeftest(m6, vcov=vcovCL(m6, factor(d.sub6$dispute_combined))) 


## Heckman selection models
h2.1 <- heckit(ruling ~ lnthird+trade_share_rctp+times_ruled+gdp_share+r_greatpower+article_xxii+remedy+systemic_new+art_agric+art_sps+ art_service,
               comply_b ~ lnthird+trade_share_rctp+times_ruled+gdp_share+ r_greatpower+remedy+article_xxii,
               data = d, method = "2step")
               
h3.1 <- heckit(ruling ~ lnthird+trade_share_rctp+times_ruled+gdp_share+r_greatpower+article_xxii+remedy+systemic_new+ratio_rule_yr+adv_frequency,
               comply_b ~ lnthird+trade_share_rctp+times_ruled+gdp_share+remedy+r_greatpower+article_xxii,
               data = d, method = "2step")


## Print Table 1, single stage models
stargazer(m1a.cluster, m1b.cluster, lm1a.cluster, lm1b.cluster,
          m5.cluster, m6.cluster, h2.1, h3.1, 
          no.space=T, star.char = c(".","*","**"), type="text")

stargazer(m1a, m1b, lm1a, lm1b,
          m5, m6, h2.1, h3.1, 
          no.space=T, star.char = c(".","*","**"), type="text")


## Print Table 1, Heckman selection models
# outcome equation
stargazer(h2.1, h3.1, no.space=T, dep.var.labels.include = TRUE, selection.equation=FALSE, star.char = c(".","*","**"), type="text")
# selection equation
stargazer(h2.1, h3.1, no.space=T, dep.var.labels.include = TRUE, selection.equation=TRUE, star.char = c(".","*","**"), type="text")



##------------------------------------------------------------
## Predicted Probability -------------------------------------

d.sub1 <- na.omit( subset(d, select = c("ds","comply_b", "comply_n","lnthird",
                                       "trade_share_rctp", "gdp_share", "r_greatpower",
                                       "times_ruled",  "article_xxii","remedy","clarity","dispute_combined")) )


form <- comply_b ~ lnthird+trade_share_rctp+times_ruled+gdp_share+r_greatpower+article_xxii+remedy+clarity
z.out <- zelig(form, model = "probit", data = d.sub1)
x.out <- setx(z.out)
s.out <- sim(z.out, x = x.out)

summary(d.sub1$lnthird)

x0 <- setx(z.out,  lnthird = 0)			# no third parties
x1 <- setx1(z.out, lnthird = 2.303)		# third quartile 


sim(z.out, x=x0, x1=x1) # predicted prob compliance and CI

## bivariate correlations (footnote 15)

# correlation between systemic cases and rulings
cor(d$systemic_new, d$ruling, use = "complete.obs")

# correlation between systemic cases and compliance
temp <- d[is.na(d$comply_b)==F,]
cor(temp$systemic_new, temp$comply_b, use = "pairwise.complete.obs")

## END






