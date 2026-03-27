setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(cowplot)
library(ggExtra)

inpartysafe = list()

marplot = function(elec, partit, thresh = 0.55){
  
  tmp = x2 %>% filter(election == elec & partition == partit)
  
  tmp2 = tmp %>% group_by(partition, district) %>%
       mutate(margin = max(value)/(max(value)+min(value))) %>%
       filter(value == max(value)) %>% ungroup() %>%
       select(-value)
  
  tmp2$party_torep = tmp2$party
  tmp2$party_todem = tmp2$party
  
  tmp2$party_torep[tmp2$margin < thresh] = 'R'
  tmp2$party_todem[tmp2$margin < thresh] = 'D'
  
  normal_firms = sum(tmp2$firms[tmp2$party == "D"])
  
  rep_bound_firms = sum(tmp2$firms[tmp2$party_torep == "D"])

  dem_bound_firms = sum(tmp2$firms[tmp2$party_todem == "D"])
  
  # 2. Generate p-values
  
  ds = ds %>% filter(election == elec)
  
  out = c(
  sum(ds$firms <= normal_firms) -1,
  sum(ds$firms == normal_firms) -1,

  sum(ds$firms <= dem_bound_firms) -1,
  sum(ds$firms == dem_bound_firms) -1,
  
  sum(ds$firms <= rep_bound_firms) -1,
  sum(ds$firms == rep_bound_firms) -1
  )

    inpartysafe[[length(inpartysafe)+1]] <<- out
  
}

#### AZ ####
load("../data_clean/state_sims/az.RData")
elec = "VOT18"
partit = "AZ"
marplot(elec = elec, partit = partit)
gdata::keep(inpartysafe, marplot, sure=T)


#### GA ####
load("../data_clean/state_sims/ga.RData")
elec = "VOT16"
partit = "GA"
marplot(elec = elec, partit = partit)
gdata::keep(inpartysafe, marplot, sure=T)

#### IA ####
load("../data_clean/state_sims/ia.RData")
elec = "VOTE12"
partit = "IA"
marplot(elec = elec, partit = partit)
gdata::keep(inpartysafe, marplot, sure=T)


#### MA ####
load("../data_clean/state_sims/ma.RData")
elec = "VOT12"
partit = "MA2012"
marplot(elec = elec, partit = partit)
gdata::keep(inpartysafe, marplot, sure=T)

#### MD ####
load("../data_clean/state_sims/md.RData")
elec = "VOT12"
partit = "MD"
marplot(elec = elec, partit = partit)
gdata::keep(inpartysafe, marplot, sure=T)


#### MN Vetoed ####
load("../data_clean/state_sims/mn.RData")
elec = "USH12"
partit = "MN_vetoed"
marplot(elec = elec, partit = partit)
gdata::keep(inpartysafe, marplot, sure=T)


#### MN ####
load("../data_clean/state_sims/mn.RData")
elec = "USH12"
partit = "MN"
marplot(elec = elec, partit = partit)
gdata::keep(inpartysafe, marplot, sure=T)



#### NC12 ####
load("../data_clean/state_sims/nc.RData")
elec = "12G_VT"
partit = "NC12"
marplot(elec = elec, partit = partit)
gdata::keep(inpartysafe, marplot, sure=T)


#### NC16 ####
load("../data_clean/state_sims/nc.RData")
elec = "12G_VT"
partit = "NC16"
marplot(elec = elec, partit = partit)
gdata::keep(inpartysafe, marplot, sure=T)


#### NM ####
load("../data_clean/state_sims/nm.RData")
elec = "VOT18"
partit = "NM2012"
marplot(elec = elec, partit = partit)
gdata::keep(inpartysafe, marplot, sure=T)

#### OH ####
load("../data_clean/state_sims/oh.RData")
elec = "VOT12"
partit = "OH"
marplot(elec = elec, partit = partit)
gdata::keep(inpartysafe, marplot, sure=T)


#### OR ####
load("../data_clean/state_sims/or.RData")
elec = "VOT12"
partit = "OR"
marplot(elec = elec, partit = partit)
gdata::keep(inpartysafe, marplot, sure=T)


#### PA12 ####
load("../data_clean/state_sims/pa.RData") 
elec = "VOT12"
partit = "PA12"
x2$election = elec
marplot(elec = elec, partit = partit)
gdata::keep(inpartysafe, marplot, sure=T)

#### PA18 ####
load("../data_clean/state_sims/pa.RData")
elec = "VOT12"
partit = "PA18"
x2$election = elec
marplot(elec = elec, partit = partit)
gdata::keep(inpartysafe, marplot, sure=T)

#### TX ####
load("../data_clean/state_sims/tx.RData")
elec = "VOT12"
partit = "TX2012"
marplot(elec = elec, partit = partit)
gdata::keep(inpartysafe, marplot, sure=T)

#### VA12 ####
load("../data_clean/state_sims/va.RData")
elec = "G16VOT"
partit = "VA12"
marplot(partit = partit, elec = elec)
gdata::keep(inpartysafe, marplot, sure=T)

#### VA16 ####
load("../data_clean/state_sims/va.RData")
elec = "G16VOT"
partit = "VA16"
marplot(partit = partit, elec = elec)
gdata::keep(inpartysafe, marplot, sure=T)

#### WI ####
load("../data_clean/state_sims/wi.RData")
elec = "VOT12"
partit = "WI"
marplot(elec = elec, partit = partit)
gdata::keep(inpartysafe, marplot, sure=T)


##### Now clean up everything ######
names(inpartysafe) = c("az", "ga", "ia", "ma", "md", "mnvetoed", "mn",
                       "nc12", "nc16", "nm", "oh", "or", "pa12", 
                       "pa18", "tx", "va12", "va16", "wi")

inpartysafe2 = do.call(rbind, inpartysafe)
rownames(inpartysafe2) = names(inpartysafe)
colnames(inpartysafe2) = c("fewerfirms", "samefirms",
                           'fewerfirms_dembonund','samefirms_dembound',
                           'fewerfirms_repbound','samefirms_repbound')


dat = inpartysafe2

dat2 = dat %>% mutate(
  lower_p = (fewerfirms_repbound)/50000,
  mid_p = (fewerfirms)/50000,
  upper_p = (fewerfirms_dembonund)/50000
) %>% select(X, lower_p, mid_p, upper_p)

## Now to fix the remainder
dat2[dat2$X == "ma",] = c("ma", 1,1,1)
dat2[dat2$X == "nm",] = c("nm", 1,1,1)
dat2[dat2$X == "md",] = c("md", 1,1,1)
dat2[dat2$X == "ia","lower_p"] = dat2[dat2$X=="ia", "mid_p"]
dat2[dat2$X == "ia","lower_p"] = dat2[dat2$X=="ia", "mid_p"]
dat2[dat2$X == "va12","lower_p"] = dat2[dat2$X=="va12", "mid_p"]
dat2[dat2$X == "pa18","upper_p"] = dat2[dat2$X=="pa18", "mid_p"]
dat2[dat2$X=="mnvetoed","upper_p"] = 1

load("../output/results/wi_results.RData")
dat2[dat2$X == "wi",] = c("wi", (results[3]+results[2])/50000,
                          (results[3]+results[2])/50000,
                          (results[3]+results[2])/50000)

## sanity check
all(dat2$lower_p > 0 & dat2$lower_p <= 1)
all(dat2$mid_p > 0 & dat2$mid_p <= 1)
all(dat2$upper_p > 0 & dat2$upper_p <= 1)


#### Table A2

dat2$X = toupper(dat2$X)
dat2$method = c("Commission", "Rep", "Commission", "Dem", "Dem", "Rep", "Court", "Rep", "Court",
                "Dem", "Rep", "Dem", "Rep", "Court", "Rep", "Rep", "Court", "Rep")
dat2 = dat2 %>% select("State" = X, "Type" = method, "Lower Bound" = lower_p,
                       "Baseline" = mid_p, "Upper Bound" = upper_p)%>%
  mutate(`Lower Bound` = round(as.numeric(`Lower Bound`), 3),
         `Upper Bound` = round(as.numeric(`Upper Bound`), 3),
         `Baseline` = round(as.numeric(`Baseline`), 3)) %>%
  arrange(`State`)

print(xtable::xtable(dat2), include.rownames=FALSE)



chisq.val = -2 * sum(log(1-tmp))
pchisq(chisq.val, df=2*(length(tmp)), lower.tail=FALSE)








### Most pessimistic global p-value
tmp = c(0.413, 0.129, 0.5 - 0.000,
        1-0.981, 0.162, 0.243,
        0.501 - 0.5, 1-0.999,
        0.5 - 0.339, 1-0.126,
        0.983, 0.5, 1- 0.113, 0.182,
        0.5 - 0.007, 0.004)

chisq.val = -2 * sum(log(tmp))
pchisq(chisq.val, df=2*(length(tmp)), lower.tail=FALSE)
