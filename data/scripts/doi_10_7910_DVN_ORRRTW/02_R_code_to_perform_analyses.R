# The following analyses were carried out using R version 4.1.0.


###############################
## Install and load packages ##
###############################

# Uncomment the line below if packages are not already installed
# install.packages(c("lfe","stargazer","miceadds","fixest","ggplot2",
#				"tidyverse","texreg"))

library(lfe)
library(stargazer)
library(miceadds)
library(fixest)
library(ggplot2)
library(tidyverse)
library(texreg)

########################
## Load analysis data ##
########################

ind_directives1956 = read.csv("individual_directives_1956.csv")
ind_directives1946 = read.csv("individual_directives_1946.csv")
annual_directives = read.csv("annual_directives.csv")
annual_directives_plo = read.csv("annual_directives_plo.csv")


##############
## Figure 1 ##
##############

all_directives_year = ind_directives1946 %>%
  group_by(year) %>% 
	summarize(prop_eo = mean(eoproc),numdocs=n(),numsig=sum(sig355)) 

sig_directives_year = ind_directives1946 %>%
  filter(sig355==1) %>%
  group_by(year) %>% 
	summarize(prop_eo = mean(eoproc))


ggplot(all_directives_year, aes(x = year, y = numdocs)) +
  	geom_line(lwd=1,color="DARKGRAY") +
	theme_bw(base_size=13) + ylab("Number of directives") + xlab("") +
	scale_x_continuous(breaks = c(1950,1960,1970,1980,1990,2000,2010,2020)) +
	theme(axis.ticks.x=element_blank()) +
	theme(axis.ticks.y=element_blank()) +
 	geom_line(data=all_directives_year,aes(x=year,y=numsig),lwd=1) +
	annotate("text", x = 2010, y = 510, size = 3,label = "All directives",col="gray45") +
	annotate("text", x = 1970, y = 110, size = 3,label = "Significant directives") +
	theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
	theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())


##############
## Figure 2 ##
##############

## Top panel ##

ggplot(all_directives_year, aes(x = year, y = prop_eo,col="DARKGRAY")) + geom_point(size=2.5) +
	theme_bw(base_size=13) + ylab("Proportion of executive orders/proclamations") + xlab("") +
	scale_x_continuous(breaks = c(1950,1960,1970,1980,1990,2000,2010,2020)) +
	scale_y_continuous(breaks = c(0.25,0.5,0.75,1),limits=c(0,1)) +
	theme(legend.position="bottom") + 
	theme(axis.ticks.x=element_blank()) +
	theme(axis.ticks.y=element_blank()) +
	theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
	scale_color_manual(values = "DARKGRAY") +
	theme(legend.margin=margin(-10, 0, 0, 0)) +
	theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank()) +
	theme(plot.title = element_text(hjust = 0.5)) + labs(title="All directives") +
	theme(legend.position="none")

## Bottom panel ##

ggplot(sig_directives_year, aes(x = year, y = prop_eo,col="DARKGRAY")) + geom_point(size=2.5) +
	theme_bw(base_size=13) + ylab("Proportion of executive orders/proclamations") + xlab("") +
	scale_x_continuous(breaks = c(1950,1960,1970,1980,1990,2000,2010,2020)) +
	scale_y_continuous(breaks = c(0.25,0.5,0.75,1),limits=c(0,1)) +
	theme(legend.position="bottom") + 
	theme(axis.ticks.x=element_blank()) +
	theme(axis.ticks.y=element_blank()) +
	theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
	scale_color_manual(values = "DARKGRAY") +
	theme(legend.margin=margin(-10, 0, 0, 0)) +
	theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank()) +
	theme(plot.title = element_text(hjust = 0.5)) + labs(title="Significant directives") +
	theme(legend.position="none")

########################################################################
## Calculate in-text statistics reported in association with Figure 2 ##
########################################################################

cs.test1 = t.test(ind_directives1946$eoproc[ind_directives1946$divided==1],ind_directives1946$eoproc[ind_directives1946$divided==0])
abs(cs.test1$statistic)
# t-statistic = 26.09727 (reported as 26.1)
cs.test1$p.value
# p-value = 3.642889e-148 (reported as < .001)
cs.test1$estimate
# mean of x = 0.5227929 (reported as 52 percent)
# mean of y = 0.6661283 (reported as 67 percent)

cs.test2 = t.test(ind_directives1946$eoproc[ind_directives1946$divided==1 & ind_directives1946$sig355==1],ind_directives1946$eoproc[ind_directives1946$divided==0 & ind_directives1946$sig355==1])
abs(cs.test2$statistic)
# t-statistic = 7.840912 (reported as 7.8)
cs.test2$p.value
# p-value = 5.527575e-15 (reported as < .001)
cs.test2$estimate
# mean of x = 0.5788798 (reported as 58 percent)
# mean of y = 0.6797170 (reported as 68 percent)

#############
## Table 1 ##
#############

lm1s = felm(eoproc ~ divided + unemp | president | 0 |congress,data=subset(ind_directives1946,sig355==1))
lm2s = felm(eoproc ~ divided + unemp | president + majortopic| 0 |congress,data=subset(ind_directives1946,sig355==1))
lm3s = felm(eoproc ~ divided + unemp | majortopic + president + pres_quarter | 0 |congress,data=subset(ind_directives1946,sig355==1))
lm4s = felm(eoproc ~ divided + unemp | majortopic + president + pres_quarter | 
				0 |congress,data=subset(ind_directives1956,sig355==1))
lm5s = felm(eoproc ~ divided + unemp + approval + mip_percent + topic_prop |
				majortopic + president + pres_quarter | 
				0 |congress,data=subset(ind_directives1956,sig355==1))

 tab1 <- stargazer(lm1s,lm2s,lm3s,lm4s,lm5s,
                     title="Divided Party Control and Choice of Unilateral Directive",
                     label="eoproc_mm",
    			   style="aer",
			   dep.var.labels   = "Issued as executive order or proclamation",
			   order=c("divided","unemp","approval","mip_percent","topic_prop"),
                     covariate.labels = c("Divided government",
							"Unemployment rate","Approval rating","Issue salience",
							"Presidential priority"),
                     add.lines = list(c("Time period","1946-2020","1946-2020","1946-2020","1956-2020","1956-2020"),
						  c("President fixed effects","\\checkmark","\\checkmark","\\checkmark",
							"\\checkmark","\\checkmark"),
                                      c("Issue area fixed effects","","\\checkmark","\\checkmark",
							"\\checkmark","\\checkmark"),
                                      c("Quarter of term fixed effects","","","\\checkmark",
							"\\checkmark","\\checkmark")),
                     keep.stat = c("n"),
                     star.cutoffs = c(0.05),star.char = c("*"),table.placement="!ht",
                     notes.append = FALSE,notes.label = "",column.sep.width="",
                     table.layout ="-ld-#-t-as-n",font.size="normalsize",digits=3,digits.extra=0,
                     notes="\\parbox[t]{.9\\textwidth}{\\footnotesize \\textit{Note}: Entries are linear regression coefficients with 
                     standard errors clustered on congress in parentheses. The dependent variable is an indicator for whether a 
			   unilateral directive was issued as an executive order or proclamation (\\emph{y}=1) or some other directive
			   type (\\emph{y}=0). $^{*}$p$<$0.05 (two-tailed tests).}")


#############
## Table 2 ##
#############

lm1p = felm(eoproc ~ unemp + divided | 
			president | 0 |congress,data=subset(ind_directives1946,sig355==1 & (doctype=="MM"|doctype=="EO")))
lm2p = felm(eoproc ~ unemp + divided | 
			president + majortopic| 0 |congress,data=subset(ind_directives1946,sig355==1 & (doctype=="MM"|doctype=="EO")))
lm3p = felm(eoproc ~ unemp + divided | 
			majortopic + president + pres_quarter | 0 |congress,data=subset(ind_directives1946,sig355==1 & (doctype=="MM"|doctype=="EO")))
lm4p = felm(eoproc ~ unemp + divided | 
			majortopic + president + pres_quarter | 0 |congress,data=subset(ind_directives1956,sig355==1 & (doctype=="MM"|doctype=="EO")))
lm5p = felm(eoproc ~ unemp + divided + approval + mip_percent + topic_prop | 
			majortopic + president + pres_quarter  | 0 |congress,data=subset(ind_directives1956,sig355==1 & (doctype=="MM"|doctype=="EO")))


  tab1p <- stargazer(lm1p,lm2p,lm3p,lm4p,lm5p,
                     title="Political Context and Directive Choice: EOs and Memos",
                     label="eo_mm",
			   style="aer",dep.var.labels   = "Issued as executive order",
			   order=c("divided","unemp","approval","mip_percent","topic_prop"),
                     covariate.labels = c("Divided government","Unemployment rate",
							"Approval rating","Issue salience","Presidential priority"),
                     add.lines = list(c("President fixed effects","\\checkmark","\\checkmark","\\checkmark",
							"\\checkmark","\\checkmark"),
                                      c("Issue area fixed effects","","\\checkmark","\\checkmark",
							"\\checkmark","\\checkmark"),
                                      c("Quarter of term fixed effects","","","\\checkmark",
							"\\checkmark","\\checkmark")),
                     keep.stat = c("n"),
                     star.cutoffs = c(0.05),star.char = c("*"),table.placement="!ht",
                     notes.append = FALSE,notes.label = "",column.sep.width="",
                     table.layout ="-ld-#-t-as-n",font.size="normalsize",digits=3,digits.extra=0,
                     notes="\\parbox[t]{\\textwidth}{\\footnotesize \\textit{Note}: Entries are linear regression coefficients with 
                     standard errors clustered on congress in parentheses. The dependent variable is an indicator for whether a significant
			   unilateral directive was issued as an executive order  (\\emph{y}=1) or a memorandum
			    (\\emph{y}=0). $^{*}$p$<$0.05 (two-tailed tests).}")


#############
## Table 3 ##
#############

tab3_m1a = fenegbin(numsigEO ~ divided | president, data=annual_directives)
summary(tab3_m1a, cluster = "congress")
tab3_m1b = fenegbin(numsigEO ~ divided + inflation + spending_percent_gdp + war+ lame_duck + administration_change + trend| president, data=annual_directives)
summary(tab3_m1b, cluster = "congress")

tab3_m2a = fenegbin(numsigPR ~ divided | president, data=annual_directives)
summary(tab3_m2a, cluster = "congress")
tab3_m2b = fenegbin(numsigPR ~ divided + inflation + spending_percent_gdp + war+ lame_duck + administration_change + trend| president, data=annual_directives)
summary(tab3_m2b, cluster = "congress")

tab3_m3a = fenegbin(numsigMM ~ divided | president, data=annual_directives)
summary(tab3_m3a, cluster = "congress")
tab3_m3b = fenegbin(numsigMM ~ divided + inflation + spending_percent_gdp + war+ lame_duck + administration_change + trend| president, data=annual_directives)
summary(tab3_m3b, cluster = "congress")

tab3_m4a = fenegbin(numsig ~ divided | president, data=annual_directives)
summary(tab3_m4a, cluster = "congress")
tab3_m4b = fenegbin(numsig ~ divided + inflation + spending_percent_gdp + war+ lame_duck + administration_change + trend| president, data=annual_directives)
summary(tab3_m4b, cluster = "congress")



###########################################################################
## Code for reproducing figures and tables in the supporting information ##
###########################################################################



###############
## Table A.1 ##
###############


lm1plo = felm(eoproc ~ unemp + divided | 
			president | 0 |congress,data=subset(ind_directives1946,sig355==1 & plo==0))
lm2plo = felm(eoproc ~ unemp + divided | 
			president + majortopic| 0 |congress,data=subset(ind_directives1946,sig355==1 & plo==0))
lm3plo = felm(eoproc ~ unemp + divided | 
			majortopic + president + pres_quarter | 0 |congress,data=subset(ind_directives1946,sig355==1 & plo==0))
lm4plo = felm(eoproc ~ unemp + divided | 
			majortopic + president + pres_quarter | 0 |congress,data=subset(ind_directives1956,sig355==1 & plo==0))
lm5plo = felm(eoproc ~ unemp + divided + approval + mip_percent + topic_prop | 
			majortopic + president + pres_quarter  | 0 |congress,data=subset(ind_directives1956,sig355==1 & plo==0))

  tab1plo <- stargazer(lm1plo,lm2plo,lm3plo,lm4plo,lm5plo,
                     title="Political Context and Directive Choice: Omitting Public Land Orders",
                     label="eoproc_mm_plo",
     			   style="aer",dep.var.labels   = "Issued as executive order",
			   order=c("divided","unemp","approval","mip_percent","topic_prop"),
                     covariate.labels = c("Divided government","Unemployment rate",
							"Approval rating","Issue salience","Presidential priority"),
                     add.lines = list(c("President fixed effects","\\checkmark","\\checkmark","\\checkmark",
							"\\checkmark","\\checkmark"),
                                      c("Issue area fixed effects","","\\checkmark","\\checkmark",
							"\\checkmark","\\checkmark"),
                                      c("Quarter of term fixed effects","","","\\checkmark",
							"\\checkmark","\\checkmark")),
                     keep.stat = c("n"),
                     star.cutoffs = c(0.05),star.char = c("*"),table.placement="!ht",
                     notes.append = FALSE,notes.label = "",column.sep.width="",
                     table.layout ="-ld-#-t-as-n",font.size="normalsize",digits=3,digits.extra=0,
                     notes="\\parbox[t]{\\textwidth}{\\footnotesize \\textit{Note}: Entries are linear regression coefficients with 
                     standard errors clustered on congress in parentheses. The dependent variable is an indicator for whether a significant
			   unilateral directive was issued as an executive order  (\\emph{y}=1) or a memorandum
			    (\\emph{y}=0). Public Land Orders are omitted from the data. $^{*}$p$<$0.05 (two-tailed tests).}")
 
###############
## Table A.2 ##
###############

lm1s.glm = miceadds::glm.cluster(formula=eoproc ~ divided + unemp + factor(president),data=subset(ind_directives1946,sig355==1),cluster="congress",family="binomial")
lm2s.glm = miceadds::glm.cluster(formula=eoproc ~ divided + unemp + factor(president) + factor(majortopic),cluster="congress",data=subset(ind_directives1946,sig355==1),family="binomial")
lm3s.glm = miceadds::glm.cluster(formula=eoproc ~ divided + unemp + factor(pres_quarter) +  factor(majortopic) + factor(president),cluster="congress",data=subset(ind_directives1946,sig355==1),family="binomial")
lm4s.glm = miceadds::glm.cluster(formula=eoproc ~ divided + unemp + factor(majortopic) + factor(president) + factor(pres_quarter), 
				cluster="congress",data=subset(ind_directives1956,sig355==1),family="binomial")
lm5s.glm = miceadds::glm.cluster(formula=eoproc ~ divided + unemp + approval + mip_percent + topic_prop +
				factor(majortopic) + factor(president) + factor(pres_quarter), 
				cluster="congress",data=subset(ind_directives1956,sig355==1),family="binomial")
extract(lm1s.glm)
extract(lm2s.glm)
extract(lm3s.glm)
extract(lm4s.glm)
extract(lm5s.glm)
 

###############
## Table A.3 ##
###############

lm1ns.glm = miceadds::glm.cluster(formula=eoproc ~ divided + unemp + factor(president),data=subset(ind_directives1946,sig355==0),cluster="congress",family="binomial")
lm2ns.glm = miceadds::glm.cluster(formula=eoproc ~ divided + unemp + factor(president) + factor(majortopic),cluster="congress",data=subset(ind_directives1946,sig355==0),family="binomial")
lm3ns.glm = miceadds::glm.cluster(formula=eoproc ~ divided + unemp + factor(pres_quarter) +  factor(majortopic) + factor(president),cluster="congress",data=subset(ind_directives1946,sig355==0),family="binomial")
lm4ns.glm = miceadds::glm.cluster(formula=eoproc ~ divided + unemp + factor(majortopic) + factor(president) + factor(pres_quarter), 
				cluster="congress",data=subset(ind_directives1956,sig355==0),family="binomial")
lm5ns.glm = miceadds::glm.cluster(formula=eoproc ~ divided + unemp + approval + mip_percent + topic_prop +
				factor(majortopic) + factor(president) + factor(pres_quarter), 
				cluster="congress",data=subset(ind_directives1956,sig355==0),family="binomial")

extract(lm1ns.glm)
extract(lm2ns.glm)
extract(lm3ns.glm)
extract(lm4ns.glm)
extract(lm5ns.glm)


###############
## Table A.4 ##
###############

lm1p.glm = miceadds::glm.cluster(formula=eoproc ~ divided + unemp + factor(president),data=subset(ind_directives1946,sig355==1 & (doctype=="MM"|doctype=="EO")),cluster="congress",family="binomial")
lm2p.glm = miceadds::glm.cluster(formula=eoproc ~ divided + unemp + factor(president) + factor(majortopic),cluster="congress",data=subset(ind_directives1946,sig355==1 & (doctype=="MM"|doctype=="EO")),family="binomial")
lm3p.glm = miceadds::glm.cluster(formula=eoproc ~ divided + unemp + factor(pres_quarter) +  factor(majortopic) + factor(president),cluster="congress",data=subset(ind_directives1946,sig355==1 & (doctype=="MM"|doctype=="EO")),family="binomial")
lm4p.glm = miceadds::glm.cluster(formula=eoproc ~ divided + unemp + factor(majortopic) + factor(president) + factor(pres_quarter), 
				cluster="congress",data=subset(ind_directives1956,sig355==1 & (doctype=="MM"|doctype=="EO")),family="binomial")
lm5p.glm = miceadds::glm.cluster(formula=eoproc ~ divided + unemp + approval + mip_percent + topic_prop +
				factor(majortopic) + factor(president) + factor(pres_quarter), 
				cluster="congress",data=subset(ind_directives1956,sig355==1 & (doctype=="MM"|doctype=="EO")),family="binomial")
extract(lm1p.glm)
extract(lm2p.glm)
extract(lm3p.glm)
extract(lm4p.glm)
extract(lm5p.glm)


###############
## Table A.5 ##
###############

lm1maj = felm(eoproc ~ pres_invmaj + unemp | president | 0 |congress,data=subset(ind_directives1946,sig355==1))
lm2maj = felm(eoproc ~ pres_invmaj + unemp | president + majortopic| 0 |congress,data=subset(ind_directives1946,sig355==1))
lm3maj = felm(eoproc ~ pres_invmaj + unemp | majortopic + president + pres_quarter | 0 |congress,data=subset(ind_directives1946,sig355==1))
lm4maj = felm(eoproc ~ pres_invmaj + unemp | majortopic + president + pres_quarter | 
				0 |congress,data=subset(ind_directives1956,sig355==1))
lm5maj = felm(eoproc ~ pres_invmaj + unemp + approval + mip_percent + topic_prop |
				majortopic + president + pres_quarter | 
				0 |congress,data=subset(ind_directives1956,sig355==1))


  tab1maj <- stargazer(lm1maj,lm2maj,lm3maj,lm4maj,lm5maj,
                     title="Congressional Composition and Choice of Unilateral Directive",
                     label="eoproc_mm_maj",
    			   style="aer",
			   dep.var.labels   = "Issued as executive order or proclamation",
			   order=c("pres_invmaj","unemp","approval","mip_percent","topic_prop"),
                     covariate.labels = c("Opposition seat share",
							"Unemployment rate","Approval rating","Issue salience",
							"Presidential priority"),
                     add.lines = list(c("Time period","1946-2020","1946-2020","1946-2020","1956-2020","1956-2020"),
						  c("President fixed effects","\\checkmark","\\checkmark","\\checkmark",
							"\\checkmark","\\checkmark"),
                                      c("Issue area fixed effects","","\\checkmark","\\checkmark",
							"\\checkmark","\\checkmark"),
                                      c("Quarter of term fixed effects","","","\\checkmark",
							"\\checkmark","\\checkmark")),
                     keep.stat = c("n"),
                     star.cutoffs = c(0.05),star.char = c("*"),table.placement="!ht",
                     notes.append = FALSE,notes.label = "",column.sep.width="",
                     table.layout ="-ld-#-t-as-n",font.size="normalsize",digits=3,digits.extra=0,
                     notes="\\parbox[t]{.9\\textwidth}{\\footnotesize \\textit{Note}: Entries are linear regression coefficients with 
                     standard errors clustered on congress in parentheses. The dependent variable is an indicator for whether a 
			   unilateral directive was issued as an executive order or proclamation (\\emph{y}=1) or some other directive
			   type (\\emph{y}=0). $^{*}$p$<$0.05 (two-tailed tests).}")


###############
## Table A.6 ##
###############

lm1ns = felm(eoproc ~ divided + unemp | president | 0 |congress,data=subset(ind_directives1946,sig355==0))
lm2ns = felm(eoproc ~ divided + unemp | president + majortopic| 0 |congress,data=subset(ind_directives1946,sig355==0))
lm3ns = felm(eoproc ~ divided + unemp | majortopic + president + pres_quarter | 0 |congress,data=subset(ind_directives1946,sig355==0))
lm4ns = felm(eoproc ~ divided + unemp | majortopic + president + pres_quarter | 
				0 |congress,data=subset(ind_directives1956,sig355==0))
lm5ns = felm(eoproc ~ divided + unemp + approval + mip_percent + topic_prop |
				majortopic + president + pres_quarter | 
				0 |congress,data=subset(ind_directives1956,sig355==0))


  tab1ns <- stargazer(lm1ns,lm2ns,lm3ns,lm4ns,lm5ns,
                     title="Divided Party Control and Choice of Unilateral Instrument: Nonsignificant Directives",
                     label="eoproc_mm_ns",
    			   style="aer",
			   dep.var.labels   = "Issued as executive order or proclamation",
			   order=c("divided","unemp","approval","mip_percent","topic_prop"),
                     covariate.labels = c("Divided government",
							"Unemployment rate","Approval rating","Issue salience",
							"Presidential priority"),
                     add.lines = list(c("Time period","1946-2020","1946-2020","1946-2020","1956-2020","1956-2020"),
						  c("President fixed effects","\\checkmark","\\checkmark","\\checkmark",
							"\\checkmark","\\checkmark"),
                                      c("Issue area fixed effects","","\\checkmark","\\checkmark",
							"\\checkmark","\\checkmark"),
                                      c("Quarter of term fixed effects","","","\\checkmark",
							"\\checkmark","\\checkmark")),
                     keep.stat = c("n"),
                     star.cutoffs = c(0.05),star.char = c("*"),table.placement="!ht",
                     notes.append = FALSE,notes.label = "",column.sep.width="",
                     table.layout ="-ld-#-t-as-n",font.size="normalsize",digits=3,digits.extra=0,
                     notes="\\parbox[t]{.9\\textwidth}{\\footnotesize \\textit{Note}: Entries are linear regression coefficients with 
                     standard errors clustered on congress in parentheses. The dependent variable is an indicator for whether a 
			   unilateral directive was issued as an executive order or proclamation (\\emph{y}=1) or some other directive
			   type (\\emph{y}=0). $^{*}$p$<$0.05 (two-tailed tests).}")
  

###############
## Table A.7 ##
###############

lm1app = felm(eoproc ~ divided*approval + unemp | president | 0 |congress,data=subset(ind_directives1946,sig355==1))
lm2app = felm(eoproc ~ divided*approval + unemp | president + majortopic| 0 |congress,data=subset(ind_directives1946,sig355==1))
lm3app = felm(eoproc ~ divided*approval + unemp | majortopic + president + pres_quarter | 0 |congress,data=subset(ind_directives1946,sig355==1))
lm4app = felm(eoproc ~ divided*approval + unemp | majortopic + president + pres_quarter | 
				0 |congress,data=subset(ind_directives1956,sig355==1))
lm5app = felm(eoproc ~ divided*approval + unemp + mip_percent + topic_prop |
				majortopic + president + pres_quarter | 
				0 |congress,data=subset(ind_directives1956,sig355==1))

tab1approval <- stargazer(lm1app,lm2app,lm3app,lm4app,lm5app,
                     title="Divided Party Control and Choice of Unilateral Directive: Variation by Presidential Approval",
                     label="eoproc_mm_app",
    			   style="aer",
			   dep.var.labels   = "Issued as executive order or proclamation",
			   order=c("divided","divided:approval","approval","unemp","approval","mip_percent","topic_prop"),
                     covariate.labels = c("Divided government","Divided x Approval rating","Approval rating",
							"Unemployment rate","Issue salience",
							"Presidential priority"),
                     add.lines = list(c("Time period","1946-2020","1946-2020","1946-2020","1956-2020","1956-2020"),
						  c("President fixed effects","\\checkmark","\\checkmark","\\checkmark",
							"\\checkmark","\\checkmark"),
                                      c("Issue area fixed effects","","\\checkmark","\\checkmark",
							"\\checkmark","\\checkmark"),
                                      c("Quarter of term fixed effects","","","\\checkmark",
							"\\checkmark","\\checkmark")),
                     keep.stat = c("n"),
                     star.cutoffs = c(0.05),star.char = c("*"),table.placement="!ht",
                     notes.append = FALSE,notes.label = "",column.sep.width="",
                     table.layout ="-ld-#-t-as-n",font.size="normalsize",digits=3,digits.extra=0,
                     notes="\\parbox[t]{.9\\textwidth}{\\footnotesize \\textit{Note}: Entries are linear regression coefficients with 
                     standard errors clustered on congress in parentheses. The dependent variable is an indicator for whether a 
			   unilateral directive was issued as an executive order or proclamation (\\emph{y}=1) or some other directive
			   type (\\emph{y}=0). $^{*}$p$<$0.05 (two-tailed tests).}")


###############
## Table A.8 ##
###############
  
lm1elect = felm(eoproc ~ divided*election_yr + unemp | president | 0 |congress,data=subset(ind_directives1946,sig355==1))
lm2elect = felm(eoproc ~ divided*election_yr + unemp | president + majortopic| 0 |congress,data=subset(ind_directives1946,sig355==1))
lm3elect = felm(eoproc ~ divided*election_yr + unemp | majortopic + president + pres_quarter | 0 |congress,data=subset(ind_directives1946,sig355==1))
lm4elect = felm(eoproc ~ divided*election_yr + unemp | majortopic + president + pres_quarter | 
				0 |congress,data=subset(ind_directives1956,sig355==1))
lm5elect = felm(eoproc ~ divided*election_yr + unemp + approval + mip_percent + topic_prop |
				majortopic + president + pres_quarter | 
				0 |congress,data=subset(ind_directives1956,sig355==1))

  tab1elect <- stargazer(lm1elect,lm2elect,lm3elect,lm4elect,lm5elect,
                     title="Divided Party Control and Choice of Unilateral Directive: Variation by Election Timing",
                     label="eoproc_mm_elect",
    			   style="aer",
			   dep.var.labels   = "Issued as executive order or proclamation",
			   order=c("divided","divided:election_yr","election_yr","unemp","approval","mip_percent","topic_prop"),
                     covariate.labels = c("Divided government","Divided x Election year","Election year",
							"Unemployment rate","Approval rating","Issue salience",
							"Presidential priority"),
                     add.lines = list(c("Time period","1946-2020","1946-2020","1946-2020","1956-2020","1956-2020"),
						  c("President fixed effects","\\checkmark","\\checkmark","\\checkmark",
							"\\checkmark","\\checkmark"),
                                      c("Issue area fixed effects","","\\checkmark","\\checkmark",
							"\\checkmark","\\checkmark"),
                                      c("Quarter of term fixed effects","","","\\checkmark",
							"\\checkmark","\\checkmark")),
                     keep.stat = c("n"),
                     star.cutoffs = c(0.05),star.char = c("*"),table.placement="!ht",
                     notes.append = FALSE,notes.label = "",column.sep.width="",
                     table.layout ="-ld-#-t-as-n",font.size="normalsize",digits=3,digits.extra=0,
                     notes="\\parbox[t]{.9\\textwidth}{\\footnotesize \\textit{Note}: Entries are linear regression coefficients with 
                     standard errors clustered on congress in parentheses. The dependent variable is an indicator for whether a 
			   unilateral directive was issued as an executive order or proclamation (\\emph{y}=1) or some other directive
			   type (\\emph{y}=0). $^{*}$p$<$0.05 (two-tailed tests).}")


###############
## Table A.9 ##
###############

lm1reelect = felm(eoproc ~ divided*reelection_yr + unemp | president | 0 |congress,data=subset(ind_directives1946,sig355==1))
lm2reelect = felm(eoproc ~ divided*reelection_yr + unemp | president + majortopic| 0 |congress,data=subset(ind_directives1946,sig355==1))
lm3reelect = felm(eoproc ~ divided*reelection_yr + unemp | majortopic + president + pres_quarter | 0 |congress,data=subset(ind_directives1946,sig355==1))
lm4reelect = felm(eoproc ~ divided*reelection_yr + unemp | majortopic + president + pres_quarter | 
				0 |congress,data=subset(ind_directives1956,sig355==1))
lm5reelect = felm(eoproc ~ divided*reelection_yr + unemp + approval + mip_percent + topic_prop |
				majortopic + president + pres_quarter | 
				0 |congress,data=subset(ind_directives1956,sig355==1))

  tab1reelect <- stargazer(lm1reelect,lm2reelect,lm3reelect,lm4reelect,lm5reelect,
                     title="Divided Party Control and Choice of Unilateral Directive: Variation by Election Timing",
                     label="eoproc_mm_reelect",
    			   style="aer",
			   dep.var.labels   = "Issued as executive order or proclamation",
			   order=c("divided","divided:reelection_yr","reelection_yr","unemp","approval","mip_percent","topic_prop"),
                     covariate.labels = c("Divided government","Divided x Re-election year","Re-election year",
							"Unemployment rate","Approval rating","Issue salience",
							"Presidential priority"),
                     add.lines = list(c("Time period","1946-2020","1946-2020","1946-2020","1956-2020","1956-2020"),
						  c("President fixed effects","\\checkmark","\\checkmark","\\checkmark",
							"\\checkmark","\\checkmark"),
                                      c("Issue area fixed effects","","\\checkmark","\\checkmark",
							"\\checkmark","\\checkmark"),
                                      c("Quarter of term fixed effects","","","\\checkmark",
							"\\checkmark","\\checkmark")),
                     keep.stat = c("n"),
                     star.cutoffs = c(0.05),star.char = c("*"),table.placement="!ht",
                     notes.append = FALSE,notes.label = "",column.sep.width="",
                     table.layout ="-ld-#-t-as-n",font.size="normalsize",digits=3,digits.extra=0,
                     notes="\\parbox[t]{.9\\textwidth}{\\footnotesize \\textit{Note}: Entries are linear regression coefficients with 
                     standard errors clustered on congress in parentheses. The dependent variable is an indicator for whether a 
			   unilateral directive was issued as an executive order or proclamation (\\emph{y}=1) or some other directive
			   type (\\emph{y}=0). $^{*}$p$<$0.05 (two-tailed tests).}")


################
## Table A.10 ##
################

taba10_m1a = fenegbin(numsigEO ~ divided | president, data=annual_directives_plo)
summary(taba10_m1a, cluster = "congress")
taba10_m1b = fenegbin(numsigEO ~ divided + inflation + spending_percent_gdp + war+ lame_duck + administration_change + trend| president, data=annual_directives_plo)
summary(taba10_m1b, cluster = "congress")

taba10_m2a = fenegbin(numsigPR ~ divided | president, data=annual_directives_plo)
summary(taba10_m2a, cluster = "congress")
taba10_m2b = fenegbin(numsigPR ~ divided + inflation + spending_percent_gdp + war+ lame_duck + administration_change + trend| president, data=annual_directives_plo)
summary(taba10_m2b, cluster = "congress")

taba10_m3a = fenegbin(numsigMM ~ divided | president, data=annual_directives_plo)
summary(taba10_m3a, cluster = "congress")
taba10_m3b = fenegbin(numsigMM ~ divided + inflation + spending_percent_gdp + war+ lame_duck + administration_change + trend| president, data=annual_directives_plo)
summary(taba10_m3b, cluster = "congress")

taba10_m4a = fenegbin(numsig ~ divided | president, data=annual_directives_plo)
summary(taba10_m4a, cluster = "congress")
taba10_m4b = fenegbin(numsig ~ divided + inflation + spending_percent_gdp + war+ lame_duck + administration_change + trend| president, data=annual_directives_plo)
summary(taba10_m4b, cluster = "congress")


################
## Table A.11 ##
################

taba11_m1 = fenegbin(numsigEO ~ divided + inflation + spending_percent_gdp + war+ lame_duck + administration_change + trend| president, data=annual_directives)
summary(taba11_m1, cluster = "congress")

taba11_m2 = fenegbin(numsigPR ~ divided + inflation + spending_percent_gdp + war+ lame_duck + administration_change + trend| president, data=annual_directives)
summary(taba11_m2, cluster = "congress")

taba11_m3 = fenegbin(numsigMM ~ divided + inflation + spending_percent_gdp + war+ lame_duck + administration_change + trend| president, data=annual_directives)
summary(taba11_m3, cluster = "congress")

taba11_m4 = fenegbin(numsig ~ divided + inflation + spending_percent_gdp + war+ lame_duck + administration_change + trend| president, data=annual_directives)
summary(taba11_m4, cluster = "congress")


################
## Table A.12 ##
################

taba12_m1a = fenegbin(numsigEO ~ pres_avgmaj | president, data=annual_directives)
summary(taba12_m1a, cluster = "congress")
taba12_m1b = fenegbin(numsigEO ~ pres_avgmaj + inflation + spending_percent_gdp + war+ lame_duck + administration_change + trend| president, data=annual_directives)
summary(taba12_m1b, cluster = "congress")

taba12_m2a = fenegbin(numsigPR ~ pres_avgmaj | president, data=annual_directives)
summary(taba12_m2a, cluster = "congress")
taba12_m2b = fenegbin(numsigPR ~ pres_avgmaj + inflation + spending_percent_gdp + war+ lame_duck + administration_change + trend| president, data=annual_directives)
summary(taba12_m2b, cluster = "congress")

taba12_m3a = fenegbin(numsigMM ~ pres_avgmaj | president, data=annual_directives)
summary(taba12_m3a, cluster = "congress")
taba12_m3b = fenegbin(numsigMM ~ pres_avgmaj + inflation + spending_percent_gdp + war+ lame_duck + administration_change + trend| president, data=annual_directives)
summary(taba12_m3b, cluster = "congress")

taba12_m4a = fenegbin(numsig ~ pres_avgmaj | president, data=annual_directives)
summary(taba12_m4a, cluster = "congress")
taba12_m4b = fenegbin(numsig ~ pres_avgmaj + inflation + spending_percent_gdp + war+ lame_duck + administration_change + trend| president, data=annual_directives)
summary(taba12_m4b, cluster = "congress")


################
## Table A.13 ##
################

taba13_m1a = fenegbin(numsigEO ~ pres_dist_house | president, data=annual_directives)
summary(taba13_m1a, cluster = "congress")
taba13_m1b = fenegbin(numsigEO ~ pres_dist_house + inflation + spending_percent_gdp + war+ lame_duck + administration_change + trend| president, data=annual_directives)
summary(taba13_m1b, cluster = "congress")

taba13_m2a = fenegbin(numsigPR ~ pres_dist_house | president, data=annual_directives)
summary(taba13_m2a, cluster = "congress")
taba13_m2b = fenegbin(numsigPR ~ pres_dist_house + inflation + spending_percent_gdp + war+ lame_duck + administration_change + trend| president, data=annual_directives)
summary(taba13_m2b, cluster = "congress")

taba13_m3a = fenegbin(numsigMM ~ pres_dist_house | president, data=annual_directives)
summary(taba13_m3a, cluster = "congress")
taba13_m3b = fenegbin(numsigMM ~ pres_dist_house + inflation + spending_percent_gdp + war+ lame_duck + administration_change + trend| president, data=annual_directives)
summary(taba13_m3b, cluster = "congress")

taba13_m4a = fenegbin(numsig ~ pres_dist_house | president, data=annual_directives)
summary(taba13_m4a, cluster = "congress")
taba13_m4b = fenegbin(numsig ~ pres_dist_house + inflation + spending_percent_gdp + war+ lame_duck + administration_change + trend| president, data=annual_directives)
summary(taba13_m4b, cluster = "congress")


################
## Table A.14 ##
################

taba14_m1a = fenegbin(numsigEO ~ pres_dist_senate | president, data=annual_directives)
summary(taba14_m1a, cluster = "congress")
taba14_m1b = fenegbin(numsigEO ~ pres_dist_senate + inflation + spending_percent_gdp + war+ lame_duck + administration_change + trend| president, data=annual_directives)
summary(taba14_m1b, cluster = "congress")

taba14_m2a = fenegbin(numsigPR ~ pres_dist_senate | president, data=annual_directives)
summary(taba14_m2a, cluster = "congress")
taba14_m2b = fenegbin(numsigPR ~ pres_dist_senate + inflation + spending_percent_gdp + war+ lame_duck + administration_change + trend| president, data=annual_directives)
summary(taba14_m2b, cluster = "congress")

taba14_m3a = fenegbin(numsigMM ~ pres_dist_senate | president, data=annual_directives)
summary(taba14_m3a, cluster = "congress")
taba14_m3b = fenegbin(numsigMM ~ pres_dist_senate + inflation + spending_percent_gdp + war+ lame_duck + administration_change + trend| president, data=annual_directives)
summary(taba14_m3b, cluster = "congress")

taba14_m4a = fenegbin(numsig ~ pres_dist_senate | president, data=annual_directives)
summary(taba14_m4a, cluster = "congress")
taba14_m4b = fenegbin(numsig ~ pres_dist_senate + inflation + spending_percent_gdp + war+ lame_duck + administration_change + trend| president, data=annual_directives)
summary(taba14_m4b, cluster = "congress")


################
## Table A.15 ##
################

taba15_m1a = fenegbin(numsigEO ~ avgpresdist | president, data=annual_directives)
summary(taba15_m1a, cluster = "congress")
taba15_m1b = fenegbin(numsigEO ~ avgpresdist + inflation + spending_percent_gdp + war+ lame_duck + administration_change + trend| president, data=annual_directives)
summary(taba15_m1b, cluster = "congress")

taba15_m2a = fenegbin(numsigPR ~ avgpresdist | president, data=annual_directives)
summary(taba15_m2a, cluster = "congress")
taba15_m2b = fenegbin(numsigPR ~ avgpresdist + inflation + spending_percent_gdp + war+ lame_duck + administration_change + trend| president, data=annual_directives)
summary(taba15_m2b, cluster = "congress")

taba15_m3a = fenegbin(numsigMM ~ avgpresdist | president, data=annual_directives)
summary(taba15_m3a, cluster = "congress")
taba15_m3b = fenegbin(numsigMM ~ avgpresdist + inflation + spending_percent_gdp + war+ lame_duck + administration_change + trend| president, data=annual_directives)
summary(taba15_m3b, cluster = "congress")

taba15_m4a = fenegbin(numsig ~ avgpresdist | president, data=annual_directives)
summary(taba15_m4a, cluster = "congress")
taba15_m4b = fenegbin(numsig ~ avgpresdist + inflation + spending_percent_gdp + war+ lame_duck + administration_change + trend| president, data=annual_directives)
summary(taba15_m4b, cluster = "congress")


################
## Table A.16 ##
################

taba16_m1a = fenegbin(numsigEO ~ divided | president, data=subset(annual_directives,year<2020))
summary(taba16_m1a, cluster = "congress")
taba16_m1b = fenegbin(numsigEO ~ divided + inflation + spending_percent_gdp + war+ lame_duck + administration_change + trend| president, data=subset(annual_directives,year<2020))
summary(taba16_m1b, cluster = "congress")

taba16_m2a = fenegbin(numsigPR ~ divided | president, data=subset(annual_directives,year<2020))
summary(taba16_m2a, cluster = "congress")
taba16_m2b = fenegbin(numsigPR ~ divided + inflation + spending_percent_gdp + war+ lame_duck + administration_change + trend| president, data=subset(annual_directives,year<2020))
summary(taba16_m2b, cluster = "congress")

taba16_m3a = fenegbin(numsigMM ~ divided | president, data=subset(annual_directives,year<2020))
summary(taba16_m3a, cluster = "congress")
taba16_m3b = fenegbin(numsigMM ~ divided + inflation + spending_percent_gdp + war+ lame_duck + administration_change + trend| president, data=subset(annual_directives,year<2020))
summary(taba16_m3b, cluster = "congress")

taba16_m4a = fenegbin(numsig ~ divided | president, data=subset(annual_directives,year<2020))
summary(taba16_m4a, cluster = "congress")
taba16_m4b = fenegbin(numsig ~ divided + inflation + spending_percent_gdp + war+ lame_duck + administration_change + trend| president, data=subset(annual_directives,year<2020))
summary(taba16_m4b, cluster = "congress")



################
## Figure A.1 ##
################

type_df = ind_directives1946 %>%
  group_by(year, doctype) %>% summarize(numsig = sum(sig355==1),
                                        numdocs = n()) 

## Top panel ##

ggplot(type_df, aes(x=year, group=doctype, y=numdocs)) + geom_line(aes(col=doctype), size=1) +
	ylab("Number of Directives") + xlab("") + theme_minimal(base_size=13) + 
	theme(legend.title=element_blank()) + scale_x_continuous(breaks = c(1950,1960,1970,1980,1990,2000,2010,2020)) +
	theme_bw(base_size=13) +
	theme(legend.position="bottom") +
	scale_color_manual(name="",values=c("#CCCCCC","#959595", "#000000"),
                         breaks=c("EO", "MM","PR"),
                         labels=c("Executive orders          ","Memoranda            ","Proclamations     ")) +
	theme(legend.title=element_blank()) +
	theme(axis.ticks.x=element_blank()) +
	theme(axis.ticks.y=element_blank()) +
	theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
	theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())

## Bottom panel ##

ggplot(type_df, aes(x=year, group=doctype, y=numsig)) + geom_line(aes(col=doctype), size=1) +
	ylab("Number of Directives") + xlab("") + theme_minimal(base_size=13) + 
	theme(legend.title=element_blank()) + scale_x_continuous(breaks = c(1950,1960,1970,1980,1990,2000,2010,2020)) +
	theme_bw(base_size=13) +
	theme(legend.position="bottom") +
	scale_color_manual(name="",values=c("#CCCCCC","#959595", "#000000"),
                         breaks=c("EO", "MM","PR"),
                         labels=c("Executive orders          ","Memoranda            ","Proclamations     ")) +
	theme(legend.title=element_blank()) +
	theme(axis.ticks.x=element_blank()) +
	theme(axis.ticks.y=element_blank()) +
	theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
	theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())


################
## Figure A.2 ##
################

figa2_c1 = fenegbin(numsig ~ divided | president, annual_directives)
figa2_c2 = fenegbin(numsig ~ divided + inflation | president, annual_directives)
figa2_c3 = fenegbin(numsig ~ divided + spending_percent_gdp | president, annual_directives)
figa2_c4 = fenegbin(numsig ~ divided + war| president, annual_directives)
figa2_c5 = fenegbin(numsig ~ divided + lame_duck| president, annual_directives)
figa2_c6 = fenegbin(numsig ~ divided + administration_change| president, annual_directives)
figa2_c7 = fenegbin(numsig ~ divided + trend| president, annual_directives)

figa2_c8 = fenegbin(numsig ~ divided + inflation + spending_percent_gdp| president, annual_directives)
figa2_c9 = fenegbin(numsig ~ divided + inflation + war| president, annual_directives)
figa2_c10 = fenegbin(numsig ~ divided + inflation + lame_duck| president, annual_directives)
figa2_c11 = fenegbin(numsig ~ divided + inflation + administration_change| president, annual_directives)
figa2_c12 = fenegbin(numsig ~ divided + inflation + trend| president, annual_directives)
figa2_c13 = fenegbin(numsig ~ divided + spending_percent_gdp + war | president, annual_directives)
figa2_c14 = fenegbin(numsig ~ divided + spending_percent_gdp + lame_duck| president, annual_directives)
figa2_c15 = fenegbin(numsig ~ divided + spending_percent_gdp+administration_change| president, annual_directives)
figa2_c16 = fenegbin(numsig ~ divided + spending_percent_gdp + trend| president, annual_directives)
figa2_c17 = fenegbin(numsig ~ divided + war + lame_duck | president, annual_directives)
figa2_c18 = fenegbin(numsig ~ divided + war + administration_change| president, annual_directives)
figa2_c19 = fenegbin(numsig ~ divided + war + trend| president, annual_directives)
figa2_c20 = fenegbin(numsig ~ divided + lame_duck + administration_change | president, annual_directives)
figa2_c21 = fenegbin(numsig ~ divided + lame_duck + trend| president, annual_directives)
figa2_c22 = fenegbin(numsig ~ divided + administration_change + trend| president, annual_directives)

figa2_c23 = fenegbin(numsig ~ divided + inflation + spending_percent_gdp + war | president, annual_directives)
figa2_c24 = fenegbin(numsig ~ divided + inflation + spending_percent_gdp + lame_duck| president, annual_directives)
figa2_c25 = fenegbin(numsig ~ divided + inflation + spending_percent_gdp + administration_change| president, annual_directives)
figa2_c26 = fenegbin(numsig ~ divided + inflation + spending_percent_gdp + trend| president, annual_directives)
figa2_c27 = fenegbin(numsig ~ divided + inflation + war + lame_duck| president, annual_directives)
figa2_c28 = fenegbin(numsig ~ divided + inflation + war + administration_change| president, annual_directives)
figa2_c29 = fenegbin(numsig ~ divided + inflation + war + trend| president, annual_directives)
figa2_c30 = fenegbin(numsig ~ divided + inflation + lame_duck + administration_change| president, annual_directives)
figa2_c31 = fenegbin(numsig ~ divided + inflation + lame_duck + trend | president, annual_directives)
figa2_c32 = fenegbin(numsig ~ divided + inflation + administration_change + trend  | president, annual_directives)
figa2_c33 = fenegbin(numsig ~ divided + spending_percent_gdp + war + lame_duck| president, annual_directives)
figa2_c34 = fenegbin(numsig ~ divided + spending_percent_gdp+ war + administration_change| president, annual_directives)
figa2_c35 = fenegbin(numsig ~ divided + spending_percent_gdp + war + trend | president, annual_directives)
figa2_c36 = fenegbin(numsig ~ divided + spending_percent_gdp + lame_duck + administration_change| president, annual_directives)
figa2_c37 = fenegbin(numsig ~ divided + spending_percent_gdp + lame_duck + trend| president, annual_directives)
figa2_c38 = fenegbin(numsig ~ divided + spending_percent_gdp + administration_change + trend| president, annual_directives)
figa2_c39 = fenegbin(numsig ~ divided + war + lame_duck + administration_change | president, annual_directives)
figa2_c40 = fenegbin(numsig ~ divided + war + lame_duck + trend | president, annual_directives)
figa2_c41 = fenegbin(numsig ~ divided + war + administration_change + trend| president, annual_directives)
figa2_c42 = fenegbin(numsig ~ divided + lame_duck + administration_change + trend| president, annual_directives)

figa2_c43 = fenegbin(numsig ~ divided + inflation + spending_percent_gdp + war + lame_duck| president, annual_directives)
figa2_c44 = fenegbin(numsig ~ divided + inflation + spending_percent_gdp + war + administration_change| president, annual_directives)
figa2_c45 = fenegbin(numsig ~ divided + inflation + spending_percent_gdp + war + trend| president, annual_directives)
figa2_c46 = fenegbin(numsig ~ divided + inflation + spending_percent_gdp + lame_duck + administration_change | president, annual_directives)
figa2_c47 = fenegbin(numsig ~ divided + inflation + spending_percent_gdp + lame_duck + trend| president, annual_directives)
figa2_c48 = fenegbin(numsig ~ divided + inflation + spending_percent_gdp + administration_change + trend| president, annual_directives)
figa2_c49 = fenegbin(numsig ~ divided + inflation + war + lame_duck + administration_change| president, annual_directives)
figa2_c50 = fenegbin(numsig ~ divided + inflation + war + lame_duck + trend | president, annual_directives)
figa2_c51 = fenegbin(numsig ~ divided + inflation + war + administration_change + trend| president, annual_directives)
figa2_c52 = fenegbin(numsig ~ divided + inflation + lame_duck + administration_change + trend| president, annual_directives)
figa2_c53 = fenegbin(numsig ~ divided + spending_percent_gdp + war + lame_duck + administration_change | president, annual_directives)
figa2_c54 = fenegbin(numsig ~ divided + spending_percent_gdp + war + lame_duck + trend| president, annual_directives)
figa2_c55 = fenegbin(numsig ~ divided + spending_percent_gdp + war + administration_change + trend| president, annual_directives)
figa2_c56 = fenegbin(numsig ~ divided + spending_percent_gdp + lame_duck + administration_change + trend| president, annual_directives)
figa2_c57 = fenegbin(numsig ~ divided + war + lame_duck + administration_change + trend| president, annual_directives)

figa2_c58 = fenegbin(numsig ~ divided + inflation + spending_percent_gdp + war + lame_duck + administration_change | president, annual_directives)
figa2_c59 = fenegbin(numsig ~ divided + inflation + spending_percent_gdp + war + lame_duck + trend| president, annual_directives)
figa2_c60 = fenegbin(numsig ~ divided + inflation + spending_percent_gdp + war + trend + administration_change | president, annual_directives)
figa2_c61 = fenegbin(numsig ~ divided + inflation + spending_percent_gdp + lame_duck + administration_change + trend| president, annual_directives)
figa2_c62 = fenegbin(numsig ~ divided + inflation + war + lame_duck + administration_change + trend| president, annual_directives)
figa2_c63 = fenegbin(numsig ~ divided + spending_percent_gdp + war + lame_duck + administration_change + trend| president, annual_directives)

figa2_c64 = fenegbin(numsig ~ divided + inflation + spending_percent_gdp + war + lame_duck + administration_change + trend| president, annual_directives)


figa2_c.coefs = c(summary(figa2_c1)$coefficients[1],summary(figa2_c2)$coefficients[1],summary(figa2_c3)$coefficients[1],
		summary(figa2_c4)$coefficients[1],summary(figa2_c5)$coefficients[1],summary(figa2_c6)$coefficients[1],
		summary(figa2_c7)$coefficients[1],summary(figa2_c8)$coefficients[1],summary(figa2_c9)$coefficients[1],
		summary(figa2_c10)$coefficients[1],summary(figa2_c11)$coefficients[1],summary(figa2_c12)$coefficients[1],
		summary(figa2_c13)$coefficients[1],summary(figa2_c14)$coefficients[1],summary(figa2_c15)$coefficients[1],
		summary(figa2_c16)$coefficients[1],summary(figa2_c17)$coefficients[1],summary(figa2_c18)$coefficients[1],
		summary(figa2_c19)$coefficients[1],summary(figa2_c20)$coefficients[1],summary(figa2_c21)$coefficients[1],
		summary(figa2_c22)$coefficients[1],summary(figa2_c23)$coefficients[1],summary(figa2_c24)$coefficients[1],
		summary(figa2_c25)$coefficients[1],summary(figa2_c26)$coefficients[1],summary(figa2_c27)$coefficients[1],
		summary(figa2_c28)$coefficients[1],summary(figa2_c29)$coefficients[1],summary(figa2_c30)$coefficients[1],
		summary(figa2_c31)$coefficients[1],summary(figa2_c32)$coefficients[1],summary(figa2_c33)$coefficients[1],
		summary(figa2_c34)$coefficients[1],summary(figa2_c35)$coefficients[1],summary(figa2_c36)$coefficients[1],
		summary(figa2_c37)$coefficients[1],summary(figa2_c38)$coefficients[1],summary(figa2_c39)$coefficients[1],
		summary(figa2_c40)$coefficients[1],summary(figa2_c41)$coefficients[1],summary(figa2_c42)$coefficients[1],
		summary(figa2_c43)$coefficients[1],summary(figa2_c44)$coefficients[1],summary(figa2_c45)$coefficients[1],
		summary(figa2_c46)$coefficients[1],summary(figa2_c47)$coefficients[1],summary(figa2_c48)$coefficients[1],
		summary(figa2_c49)$coefficients[1],summary(figa2_c50)$coefficients[1],summary(figa2_c51)$coefficients[1],
		summary(figa2_c52)$coefficients[1],summary(figa2_c53)$coefficients[1],summary(figa2_c54)$coefficients[1],
		summary(figa2_c55)$coefficients[1],summary(figa2_c56)$coefficients[1],summary(figa2_c57)$coefficients[1],
		summary(figa2_c58)$coefficients[1],summary(figa2_c59)$coefficients[1],summary(figa2_c60)$coefficients[1],
		summary(figa2_c61)$coefficients[1],summary(figa2_c62)$coefficients[1],summary(figa2_c63)$coefficients[1],
		summary(figa2_c64)$coefficients[1])

figa2_c1 = summary(figa2_c1, cluster = "congress")
figa2_c2 = summary(figa2_c2, cluster = "congress")
figa2_c3 = summary(figa2_c3, cluster = "congress")
figa2_c4 = summary(figa2_c4, cluster = "congress")
figa2_c5 = summary(figa2_c5, cluster = "congress")
figa2_c6 = summary(figa2_c6, cluster = "congress")
figa2_c7 = summary(figa2_c7, cluster = "congress")
figa2_c8 = summary(figa2_c8, cluster = "congress")
figa2_c9 = summary(figa2_c9, cluster = "congress")
figa2_c10 = summary(figa2_c10, cluster = "congress")
figa2_c11 = summary(figa2_c11, cluster = "congress")
figa2_c12 = summary(figa2_c12, cluster = "congress")
figa2_c13 = summary(figa2_c13, cluster = "congress")
figa2_c14 = summary(figa2_c14, cluster = "congress")
figa2_c15 = summary(figa2_c15, cluster = "congress")
figa2_c16 = summary(figa2_c16, cluster = "congress")
figa2_c17 = summary(figa2_c17, cluster = "congress")
figa2_c18 = summary(figa2_c18, cluster = "congress")
figa2_c19 = summary(figa2_c19, cluster = "congress")
figa2_c20 = summary(figa2_c20, cluster = "congress")
figa2_c21 = summary(figa2_c21, cluster = "congress")
figa2_c22 = summary(figa2_c22, cluster = "congress")
figa2_c23 = summary(figa2_c23, cluster = "congress")
figa2_c24 = summary(figa2_c24, cluster = "congress")
figa2_c25 = summary(figa2_c25, cluster = "congress")
figa2_c26 = summary(figa2_c26, cluster = "congress")
figa2_c27 = summary(figa2_c27, cluster = "congress")
figa2_c28 = summary(figa2_c28, cluster = "congress")
figa2_c29 = summary(figa2_c29, cluster = "congress")
figa2_c30 = summary(figa2_c30, cluster = "congress")
figa2_c31 = summary(figa2_c31, cluster = "congress")
figa2_c32 = summary(figa2_c32, cluster = "congress")
figa2_c33 = summary(figa2_c33, cluster = "congress")
figa2_c34 = summary(figa2_c34, cluster = "congress")
figa2_c35 = summary(figa2_c35, cluster = "congress")
figa2_c36 = summary(figa2_c36, cluster = "congress")
figa2_c37 = summary(figa2_c37, cluster = "congress")
figa2_c38 = summary(figa2_c38, cluster = "congress")
figa2_c39 = summary(figa2_c39, cluster = "congress")
figa2_c40 = summary(figa2_c40, cluster = "congress")
figa2_c41 = summary(figa2_c41, cluster = "congress")
figa2_c42 = summary(figa2_c42, cluster = "congress")
figa2_c43 = summary(figa2_c43, cluster = "congress")
figa2_c44 = summary(figa2_c44, cluster = "congress")
figa2_c45 = summary(figa2_c45, cluster = "congress")
figa2_c46 = summary(figa2_c46, cluster = "congress")
figa2_c47 = summary(figa2_c47, cluster = "congress")
figa2_c48 = summary(figa2_c48, cluster = "congress")
figa2_c49 = summary(figa2_c49, cluster = "congress")
figa2_c50 = summary(figa2_c50, cluster = "congress")
figa2_c51 = summary(figa2_c51, cluster = "congress")
figa2_c52 = summary(figa2_c52, cluster = "congress")
figa2_c53 = summary(figa2_c53, cluster = "congress")
figa2_c54 = summary(figa2_c54, cluster = "congress")
figa2_c55 = summary(figa2_c55, cluster = "congress")
figa2_c56 = summary(figa2_c56, cluster = "congress")
figa2_c57 = summary(figa2_c57, cluster = "congress")
figa2_c58 = summary(figa2_c58, cluster = "congress")
figa2_c59 = summary(figa2_c59, cluster = "congress")
figa2_c60 = summary(figa2_c60, cluster = "congress")
figa2_c61 = summary(figa2_c61, cluster = "congress")
figa2_c62 = summary(figa2_c62, cluster = "congress")
figa2_c63 = summary(figa2_c63, cluster = "congress")
figa2_c64 = summary(figa2_c64, cluster = "congress")

figa2_p = c(summary(figa2_c1)$coeftable[7],
	summary(figa2_c2)$coeftable[10],
	summary(figa2_c3)$coeftable[10],
	summary(figa2_c4)$coeftable[10],
	summary(figa2_c5)$coeftable[10],
	summary(figa2_c6)$coeftable[10],
	summary(figa2_c7)$coeftable[10],
	summary(figa2_c8)$coeftable[13],
	summary(figa2_c9)$coeftable[13],
	summary(figa2_c10)$coeftable[13],
	summary(figa2_c11)$coeftable[13],
	summary(figa2_c12)$coeftable[13],
	summary(figa2_c13)$coeftable[13],
	summary(figa2_c14)$coeftable[13],
	summary(figa2_c15)$coeftable[13],
	summary(figa2_c16)$coeftable[13],
	summary(figa2_c17)$coeftable[13],
	summary(figa2_c18)$coeftable[13],
	summary(figa2_c19)$coeftable[13],
	summary(figa2_c20)$coeftable[13],
	summary(figa2_c21)$coeftable[13],
	summary(figa2_c22)$coeftable[16],
	summary(figa2_c23)$coeftable[16],
	summary(figa2_c24)$coeftable[16],
	summary(figa2_c25)$coeftable[16],
	summary(figa2_c26)$coeftable[16],
	summary(figa2_c27)$coeftable[16],
	summary(figa2_c28)$coeftable[16],
	summary(figa2_c29)$coeftable[16],
	summary(figa2_c30)$coeftable[16],
	summary(figa2_c31)$coeftable[16],
	summary(figa2_c32)$coeftable[16],
	summary(figa2_c33)$coeftable[16],
	summary(figa2_c34)$coeftable[16],
	summary(figa2_c35)$coeftable[16],
	summary(figa2_c36)$coeftable[16],
	summary(figa2_c37)$coeftable[16],
	summary(figa2_c38)$coeftable[16],
	summary(figa2_c39)$coeftable[16],
	summary(figa2_c40)$coeftable[16],
	summary(figa2_c41)$coeftable[16],
	summary(figa2_c42)$coeftable[16],
	summary(figa2_c43)$coeftable[19],
	summary(figa2_c44)$coeftable[19],
	summary(figa2_c45)$coeftable[19],
	summary(figa2_c46)$coeftable[19],
	summary(figa2_c47)$coeftable[19],
	summary(figa2_c48)$coeftable[19],
	summary(figa2_c49)$coeftable[19],
	summary(figa2_c50)$coeftable[19],
	summary(figa2_c51)$coeftable[19],
	summary(figa2_c52)$coeftable[19],
	summary(figa2_c53)$coeftable[19],
	summary(figa2_c54)$coeftable[19],
	summary(figa2_c55)$coeftable[19],
	summary(figa2_c56)$coeftable[19],
	summary(figa2_c57)$coeftable[19],
	summary(figa2_c58)$coeftable[22],
	summary(figa2_c59)$coeftable[22],
	summary(figa2_c60)$coeftable[22],
	summary(figa2_c61)$coeftable[22],
	summary(figa2_c62)$coeftable[22],
	summary(figa2_c63)$coeftable[22],
	summary(figa2_c64)$coeftable[25])

par(mar = c(4, 5, 1, 1))
plot(figa2_c.coefs,figa2_p,pch=20,xlab="Coefficient estimates",ylab="", yaxt = "n")
axis(side = 2,
     las = 2,
     mgp = c(3, 0.75, 0))
title(ylab = "P-values (SEs clustered on congress)", line = 4)
box()



############################
## Figure A.3: Thresholds ##
############################

figa3_sig100 <- fenegbin(numsig100 ~ divided + inflation + spending_percent_gdp + war + lame_duck + 
			administration_change + trend | president, data = annual_directives)
figa3_sum_sig100 = summary(figa3_sig100, cluster="congress")
figa3_sig90 <- fenegbin(numsig90 ~ divided + inflation + spending_percent_gdp + war + lame_duck + 
			administration_change + trend | president, data = annual_directives)
figa3_sum_sig90 = summary(figa3_sig90, cluster="congress")
figa3_sig80 <- fenegbin(numsig80 ~ divided + inflation + spending_percent_gdp + war + lame_duck + 
			administration_change + trend | president, data = annual_directives)
figa3_sum_sig80 = summary(figa3_sig80, cluster="congress")
figa3_sig70 <- fenegbin(numsig70 ~ divided + inflation + spending_percent_gdp + war + lame_duck + 
			administration_change + trend | president, data = annual_directives)
figa3_sum_sig70 = summary(figa3_sig70, cluster="congress")
figa3_sig60 <- fenegbin(numsig60 ~ divided + inflation + spending_percent_gdp + war + lame_duck + 
			administration_change + trend | president, data = annual_directives)
figa3_sum_sig60 = summary(figa3_sig60, cluster="congress")
figa3_sig50 <- fenegbin(numsig50 ~ divided + inflation + spending_percent_gdp + war + lame_duck + 
			administration_change + trend | president, data = annual_directives)
figa3_sum_sig50 = summary(figa3_sig50, cluster="congress")
figa3_sig40 <- fenegbin(numsig40 ~ divided + inflation + spending_percent_gdp + war + lame_duck + 
			administration_change + trend | president, data = annual_directives)
figa3_sum_sig40 = summary(figa3_sig40, cluster="congress")
figa3_sig30 <- fenegbin(numsig30 ~ divided + inflation + spending_percent_gdp + war + lame_duck + 
			administration_change + trend | president, data = annual_directives)
figa3_sum_sig30 = summary(figa3_sig30, cluster="congress")
figa3_sig20 <- fenegbin(numsig20 ~ divided + inflation + spending_percent_gdp + war + lame_duck + 
			administration_change + trend | president, data = annual_directives)
figa3_sum_sig20 = summary(figa3_sig20, cluster="congress")
figa3_sig10 <- fenegbin(numsig10 ~ divided + inflation + spending_percent_gdp + war + lame_duck + 
			administration_change + trend | president, data = annual_directives)
figa3_sum_sig10 = summary(figa3_sig10, cluster="congress")

dg_coef = c(coef(figa3_sum_sig100)[1],coef(figa3_sum_sig90)[1],coef(figa3_sum_sig80)[1],coef(figa3_sum_sig70)[1],coef(figa3_sum_sig60)[1],
		coef(figa3_sum_sig50)[1],coef(figa3_sum_sig40)[1],coef(figa3_sum_sig30)[1],coef(figa3_sum_sig20)[1],coef(figa3_sum_sig10)[1])
dg_se = c(se(figa3_sum_sig100)[1],se(figa3_sum_sig90)[1],se(figa3_sum_sig80)[1],se(figa3_sum_sig70)[1],se(figa3_sum_sig60)[1],
		se(figa3_sum_sig50)[1],se(figa3_sum_sig40)[1],se(figa3_sum_sig30)[1],se(figa3_sum_sig20)[1],se(figa3_sum_sig10)[1])
threshold = rep(1:10)
label = c("All","top 90%","top 80%","top 70%","top 60%","top 50%","top 40%","top 30%","top 20%","top 10%")

figa3_coefs = data.frame(dg_coef, dg_se,threshold,label)

ggplot(data=figa3_coefs,aes(x=threshold,y=dg_coef)) +
  geom_point(size=3) + 
  scale_y_continuous(limits = c(-.1, .40)) +
  labs(y="Coefficient for divided government", x= "Threshold for significant directives",
       title="") +  theme_bw() + theme(legend.position = "none") + 
  geom_segment(x = threshold, y = dg_coef+1.96*dg_se, xend=threshold,yend = dg_coef-1.96*dg_se) +
  geom_hline(yintercept=0,linetype="dashed",size=2) + scale_x_continuous(breaks=seq(1,10,1),labels=label) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))


