# The following analyses were carried out using R version 4.1.0.

###############################
## Install and load packages ##
###############################

# Uncomment the line below if package is not already installed
# install.packages("tidyverse")

library(tidyverse)

#######################################################
## Create analysis dataset for individual directives ##
#######################################################

directives = read.csv("directives_1946_2020.csv")
cong_covars = read.csv("cong_econ_covars.csv")
unemp = read.csv("unemployment.csv")
approval = read.csv("approval.csv")


sotu = read.csv("sotu.csv")

sotu_year = sotu %>% 
	filter(filter_PolicySentence == 1) %>%
  	group_by(year) %>% 
	mutate(year_n = n()) %>%
	group_by(year, majortopic) %>%
	summarize(topic_prop = n() / year_n[1], .groups="drop")  %>%
	dplyr::select(topic_prop,year,majortopic)

mip = read.csv("mip.csv")

mip_year = mip %>% 
	filter(percent!="NA") %>%
	rename(mip_percent=percent)

ind_directives = merge(directives,cong_covars,by="year")
ind_directives = merge(ind_directives,sotu_year,by=c("year","majortopic"),all.x=TRUE)
ind_directives = merge(ind_directives,mip_year,by=c("year","majortopic"),all.x=TRUE)
ind_directives = merge(ind_directives,unemp,by=c("year","month"),all.x=TRUE)
ind_directives = merge(ind_directives,approval,by=c("year","month"),all.x=TRUE)

ind_directives1956 = ind_directives %>% filter(year>=1956) %>%
 	filter(!is.na(approval) & !is.na(unemp)) %>%
	mutate(topic_prop = ifelse(is.na(topic_prop), 0, topic_prop)) %>%
	mutate(approval = approval/100) %>%
	select(eoproc,divided,unemp,president,congress,sig355,
		majortopic,pres_quarter,mip_percent,topic_prop,
		doctype,plo,pres_invmaj,approval,election_yr,
		reelection_yr,year)

ind_directives1946 = ind_directives %>% filter(year>=1946) %>%
	mutate(approval = approval/100) %>%
	filter(!is.na(unemp))  %>%
	select(eoproc,divided,unemp,president,congress,sig355,
		majortopic,pres_quarter,mip_percent,topic_prop,
		doctype,plo,pres_invmaj,approval,election_yr,
		reelection_yr,year)

write.csv(ind_directives1956,"../data/individual_directives_1956.csv",row.names=FALSE)
write.csv(ind_directives1946,"../data/individual_directives_1946.csv",row.names=FALSE)

#######################################################
## Create analysis dataset for aggregated directives ##
#######################################################

# identify values of significance for each decile
quantile(directives$Significance_rf[directives$plo==0], probs = seq(0, 1, 0.1))

agg_directives = directives %>% 
	filter(plo==0) %>%
		group_by(year) %>% summarize(numsig = sum(Significance_rf >= .355),
									numsig100 = sum(Significance_rf >= 0),
									numsig90 = sum(Significance_rf >= 0.003579238),
									numsig80 = sum(Significance_rf >=  0.014608095),
									numsig70 = sum(Significance_rf >= 0.075238616),
									numsig60 = sum(Significance_rf >= 0.104229976),
									numsig50 = sum(Significance_rf >= 0.133960784),
									numsig40 = sum(Significance_rf >= 0.177704244),
									numsig30 = sum(Significance_rf >= 0.203406467),
									numsig20 = sum(Significance_rf >= 0.332599825),
									numsig10 = sum(Significance_rf >= 0.607020651),
									numsigEO = sum(Significance_rf >= .355 & doctype=="EO"),
									numsigPR = sum(Significance_rf >= .355 & doctype=="PR"),
									numsigMM = sum(Significance_rf >= .355 & doctype=="MM")) 


agg_directives_plo = directives %>% 
		group_by(year) %>% summarize(numsig = sum(Significance_rf >= .355),
							numsigEO = sum(Significance_rf >= .355 & doctype=="EO"),
							numsigPR = sum(Significance_rf >= .355 & doctype=="PR"),
							numsigMM = sum(Significance_rf >= .355 & doctype=="MM"))


cong_covars = read.csv("cong_econ_covars.csv")
agg_directives <- merge(agg_directives,cong_covars,by="year")
agg_directives_plo <- merge(agg_directives_plo,cong_covars,by="year")

agg_directives = agg_directives %>%
			select(numsigEO,numsigPR,numsigMM,numsig,divided,president,
				congress,inflation,spending_percent_gdp,war,lame_duck,
				administration_change,trend,pres_avgmaj,pres_dist_house,
				pres_dist_senate,avgpresdist,year,numsig100,numsig90,
				numsig80,numsig70,numsig60,numsig50,numsig40,numsig30,
				numsig20,numsig10)

agg_directives_plo = agg_directives_plo %>%
				select(numsigEO,numsigPR,numsigMM,numsig,divided,president,
					congress,inflation,spending_percent_gdp,war,lame_duck,
					administration_change,trend)

write.csv(agg_directives,"annual_directives.csv",row.names=FALSE)
write.csv(agg_directives_plo,"annual_directives_plo.csv",row.names=FALSE)


