############################################
############################################
# Replication code:                       ##
# Depolarization, Repolarization          ##
# and  Redistributive Ideological Change  ##
# in Britain, 1983-2016                   ##
# Cohen and Cohen (2019)                  ##
############################################

# This file preprocesses the data from the BHPS, the BSAS, the BES and the CHES.

library(haven)
library(tidyverse)

##----bhpsfunctions----
# some utility functions for processing British Household Panel Survey Data

pid_process_bhps<-function(pid_var, pid_cls, party_support, party_var, pid_strength, party_comp, pid_type="all", strength_multiplier=FALSE, ordinal=FALSE){
  # function to create dummy (or dummy*strength) variable for party identitification
  # pid_var is indicator for whether identifies with a party
  # pid_cls is indicator for whether closer to party (asked if doesn't identity with party)
  # party_support is answer to question which party vote for tomorrow (asked if not identifying with closer to party)
  # party_var indicators which party individual identifies with/is closer to/ or supports+ (i.e. VOTE in bhps [without numeric end])
  # party_comp is indicator of which party to create the dummy (which party to compare the party_var value to)
  # pid_type:
  #     "all" - will convert any association with party into
  #     "identifiers" - only full identifiers
  #     "id+closer" - identifers and clser
  do_comp<-case_when(
    pid_type=="identifiers"~ 1 %in% c(pid_var),
    pid_type=="id+closer"~ 1 %in% c(pid_var, pid_cls),
    pid_type=="closer"~ 1 %in% c(pid_cls),
    TRUE~ 1 %in% c(pid_var, pid_cls) |party_support>0
  )
  all_na<-case_when(
    pid_type=="identifiers"~ sum(is.na(pid_var))==1,
    pid_type=="id+closer"~ sum(is.na(pid_var), is.na(pid_cls))==2,
    TRUE~ sum(is.na(pid_var), is.na(pid_cls), is.na(party_support))==3
  )
  y<-case_when(
    all_na ~ NA_real_,
    is.na(party_var) ~ NA_real_,
    is.na(party_comp) ~ NA_real_,
    do_comp==1 & party_var==party_comp ~ 1,
    TRUE ~ 0
  )
  if(strength_multiplier){
    if(pid_strength %in% 1:3){
      y<-y*pid_strength
    } else {
      y<-NA_real_
    }
  }
  if(ordinal){
    y<-factor(y, levels=c(0, 1), ordered=TRUE)
    return(y)
  }
  as.numeric(y*1.0)
}


last_wave<-function(year, waves=c(1991, 1993, 1995, 1997, 2000, 2004, 2007)){
  this_wave<-match(year, waves)
  lag_waves<-c(0, waves)
  lag_year<-lag_waves[this_wave]
  (lag_year)
}
next_wave<-function(year, waves=c(1991, 1993, 1995, 1997, 2000, 2004, 2007)){
  this_wave<-match(year, waves)
  next_waves<-c(waves, 9999)
  next_year<-next_waves[this_wave + 1]
  (next_year)
}

add_lag<-function(x, link.cols=c("PID", "wave")){
  #dont_repeat<-names(x)[!names(x) %in% link.cols]
  x %>%
    rename_at(vars(-one_of(link.cols)), funs(paste0(., "_lag"))) %>%
    mutate(wave=next_wave(wave)) %>%
    #dplyr::select(-one_of(dont_repeat))%>%
    right_join(x, link.cols)
}
add_lags<-function(x, link.cols=c("PID", "wave"), n_lags=1){
  for(l in 1:n_lags){
    x<-add_lag(x, link.cols)
  }
  x
}

##----prepbhps----
# assumes all BHPS data is downloaded from UKDA in spss format in folder 'bhps_root'
bhps_root<-dropbox.path
bhps_root <- "C:/"
years<-1991:2007
bhps_combined<-NULL
for(a in seq_along(years)){
  this_path<-paste0(bhps_root, "/BHPS data/UKDA-5151-spss/spss/spss12/", letters[a], "indresp.sav")
  print(this_path)
  this_bhps<-read_spss(this_path)
  this_prefix<-paste0("^", LETTERS[a])
  names(this_bhps)[names(this_bhps)!="PID"]<-sub(this_prefix, "", names(this_bhps)[names(this_bhps)!="PID"])
  this_bhps$wave<-years[a]
  if(is.null(bhps_combined)){
    bhps_combined<-this_bhps
  } else {
    bhps_combined<-bind_rows(bhps_combined, this_bhps)
  }
}

bhps_combined<-bhps_combined %>%
  group_by(PID, wave) %>%
  summarize() %>%
  arrange(PID, wave) %>%
  group_by(PID) %>%
  mutate(person_wave=row_number()) %>%
  right_join(bhps_combined)

bhps<-bhps_combined
bhps<-as_factor(bhps_combined)
waves<-unique(bhps$wave)


bhps <- bhps %>%
  dplyr::group_by(wave) %>%
  dplyr::summarise(useful_wave=sum(!is.na(OPSOCA))!=0) %>%
  filter(useful_wave) %>%
  inner_join(bhps)


bhps <- bhps %>%
  mutate(pid_con = pid_process_bhps(pid_var=VOTE1, pid_cls=VOTE2, party_support=VOTE3, party_var=VOTE, pid_strength=VOTE5, party_comp=1, pid_type="id+closer"),
         pid_lab = pid_process_bhps(VOTE1, VOTE2, VOTE3, VOTE, VOTE5, 2, pid_type="id+closer"),
         pid_labcon = if_else(pid_con+pid_lab==0, NA_real_, pid_lab),
         pid_conlab = if_else(pid_con+pid_lab==0, NA_real_, pid_con)
  ) %>%
  mutate(idonly_con = pid_process_bhps(VOTE1, VOTE2, VOTE3, VOTE, VOTE5, 1, pid_type="identifiers"),
         idonly_lab = pid_process_bhps(VOTE1, VOTE2, VOTE3, VOTE, VOTE5, 2, pid_type="identifiers"),
         idonly_labcon = if_else(idonly_con+idonly_lab==0, NA_real_, idonly_lab),
         idonly_conlab = if_else(idonly_con+idonly_lab==0, NA_real_, idonly_con)) %>%
  mutate(vote_con = pid_process_bhps(VOTE1, VOTE2, VOTE3, VOTE, VOTE5, 1, pid_type="all"),
         vote_lab = pid_process_bhps(VOTE1, VOTE2, VOTE3, VOTE, VOTE5, 2, pid_type="all"),
         vote_labcon = if_else(vote_con+vote_lab==0, NA_real_, vote_lab),
         vote_conlab = if_else(vote_con+vote_lab==0, NA_real_, vote_con))

bhps<-bhps %>%
  mutate(
    is_pidlabcon = pid_lab + pid_con,
    is_idonlylabcon = idonly_lab + idonly_con,
    is_votelabcon = vote_lab + vote_con
  )



# OPSOCA Ordinary people get a fair share of the nation's wealth
# OPSOCB There is one law for the rich and one for the poor
# OPSOCC Private enterprise is the best way to solve Britain's economic problems
# OPSOCD Major public services ought to be in state ownership
# OPSOCE It is the government's responsibility to provide a job for everyone who wants one
# OPSOCF Strong trade unions are needed to protect the working conditions and wages of employees

# Strong Agreement coded 1, Strong disagreement coded 5, Can't choose coded -7

# Four 'right wing' answers have higher values (more disagreement) 
# But items A and C more right wing answers have lower values (more agreement)
# Reverse these items so high values always indicate more right wing
# Also change negative values to NA

# Also create nice named version of these variable - with values reversed (higher values more left wing) so consistent with coding in BES and BSA
negative_to_na <- function(x) {ifelse(x < 0, NA, x)}
reverse_scale <- function(x) {(max(x, na.rm = TRUE) + 1) - x}

bhps<-bhps %>%
  mutate_at(vars(contains("OPSOC")), funs(negative_to_na)) %>%
  mutate_at(vars(c("OPSOCA", "OPSOCC")), funs(reverse_scale)) %>%
  mutate(fairshare=reverse_scale(OPSOCA), 
         onelaw=reverse_scale(OPSOCB), 
         privateent=reverse_scale(OPSOCC),
         stateown=reverse_scale(OPSOCD),
         gvtprovjob=reverse_scale(OPSOCE),
         strngtu=reverse_scale(OPSOCF))

# vectors for easy referencing of redistributive attitudes scale and party id variables
bhps.redist.attitudes<-c("OPSOCA", "OPSOCB", "OPSOCC", "OPSOCD", "OPSOCE", "OPSOCF")
bhps.lr<-c("fairshare", "onelaw", "privateent", "stateown", "gvtprovjob", "strngtu")
vote.vars<-c("VOTE", "VOTE1", "VOTE2", "VOTE3", "VOTE4", "VOTE5", "VOTE6", "pid_conlab", "pid_labcon", "pid_lab", "pid_con", "idonly_lab", "idonly_con", "idonly_labcon", "idonly_conlab", "vote_con", "vote_lab", "vote_conlab", "vote_labcon", "ord_pid", "is_idonlylabcon", "is_pidlabcon", "is_votelabcon")

# create year and numeric year variables in addition to wave for easy referencing
bhps$year<-factor(bhps$wave)
bhps$nyear<-bhps$wave

balanced <- bhps %>%
  dplyr::select(PID, wave, VOTE) %>%
  mutate(wave=paste0("w", wave)) %>%
  spread(wave, VOTE) %>%
  drop_na()
balanced.pid<-balanced$PID

long_dat<-bhps %>%
  dplyr::select(PID, wave, one_of(vote.vars), one_of(bhps.redist.attitudes), one_of(bhps.lr)) %>%
  gather(variable, value, one_of(bhps.redist.attitudes), one_of(bhps.lr), one_of(vote.vars)) 

value_dat<-long_dat %>%
  filter(!variable %in% vote.vars) %>%
  mutate(wavew=paste0("w", wave)) 

long_vote<-long_dat %>%
  filter(variable %in% vote.vars)%>%
  mutate(wavep=paste0("party", wave)) %>%
  dplyr::rename(party=value) %>%
  dplyr::rename(votevar=variable)

vote_dat<- long_vote %>%
  spread(votevar, party)

val_with_vote<-left_join(value_dat, vote_dat, by=c("PID", "wave"))

vote_spread <- long_vote %>%
  dplyr::select(-wavep) %>%
  unite(varwave, votevar, wave) %>%
  spread(varwave, party)

value_spread <- value_dat %>%
  dplyr::select(-wavew) %>%
  unite(varwave, variable, wave) %>%
  spread(varwave, value) 

val_long_with_votehistory<-left_join(value_dat, vote_spread)


##---- bsafunctions----
pid_process_bsa<-function(supparty, closer, whichparty, pid_strength, party_comp, pid_type="all", strength_multiplier=FALSE, ordinal=FALSE, na.values=c(-1, 8), no.values=c(5)){
  # function to create dummy (or dummy*strength) variable for party identitification
  # pid contains party identification
  # closer party closer (asked if doesn't identity with party)
  # party_support is answer to question which party vote for tomorrow (asked if not identifying with closer to party)
  # party_var indicators which party individual identifies with/is closer to/ or supports+
  # party_comp is indicator of which party to create the dummy (which party to compare the party_var value to)
  # pid_type:
  #     "all" - will convert any association with party into
  #     "identifiers" - only full identifiers
  #     "id+closer" - identifers and clser
  
  supparty<-as.numeric(supparty)
  closer<-as.numeric(closer)
  has_pid<-supparty==1
  has_cls<-closer==1
  party_var<-case_when(
    pid_type=="identifiers"&has_pid~ as.numeric(whichparty),
    pid_type=="id+closer"&has_pid~ as.numeric(whichparty),
    pid_type=="id+closer"&has_cls~ as.numeric(whichparty),
    pid_type=="all"&has_pid ~ as.numeric(whichparty),
    pid_type=="all"&has_cls ~ as.numeric(whichparty)
  )
  y<-case_when(
    party_var==party_comp ~ 1,
    TRUE ~ 0
  )
  if(strength_multiplier){
    if(pid_strength %in% 1:3){
      y<-y*pid_strength
    } else {
      y<-NA_real_
    }
  }
  if(ordinal){
    y<-factor(y, levels=c(0, 1), ordered=TRUE)
    return(y)
  }
  as.numeric(y*1.0)
}

##---- prepbsa ----

# load bsa data
ip<-"/spss/spss"
bsa16p<-paste0(bsapath, "/UKDA-8252-spss", ip, "19/bsa16_to_ukda.sav")
bsa16<-read_spss(bsa16p)

bsa15p<-paste0(bsapath, "/UKDA-8116-spss", ip, "19/bsa15_to_ukds_final.sav")
bsa15<-read_spss(bsa15p)


bsa14p<-paste0(bsapath, "/UKDA-7809-spss", ip, "19/bsa14ukds.sav")
bsa14<-read_spss(bsa14p)

bsa13p<-paste0(bsapath, "/UKDA-7500-spss", ip, "19/bsa13ukds.sav")
bsa13<-read_spss(bsa13p)

bsa12p<-paste0(bsapath, "/UKDA-7476-spss", ip, "19/bsa12.sav")
bsa12<-read_spss(bsa12p)

bsa11p<-paste0(bsapath, "/UKDA-7237-stata11", "/stata11/bsa11.dta")
bsa11<-read_dta(bsa11p)

bsa10p<-paste0(bsapath, "/UKDA-6969-stata9", "/stata9/bsa10.dta")
bsa10<-read_dta(bsa10p)

bsa09p<-paste0(bsapath, "/UKDA-6695-stata8", "/stata8/bsa09.dta")
bsa09<-read_dta(bsa09p)

bsa08p<-paste0(bsapath, "/UKDA-6390-stata8", "/stata8/bsa08.dta")
bsa08<-read_dta(bsa08p)

bsa07p<-paste0(bsapath, "/UKDA-6240-stata8", "/stata8/bsa07.dta")
bsa07<-read_dta(bsa07p)

bsa06p<-paste0(bsapath, "/UKDA-5823-stata8", "/stata8/bsa06.dta")
bsa06<-read_dta(bsa06p)

bsa05p<-paste0(bsapath, "/UKDA-5618-stata8", "/stata8/bsa05.dta")
bsa05<-read_dta(bsa05p)

bsa04p<-paste0(bsapath, "/UKDA-5329-stata8", "/stata8/bsa04.dta")
bsa04<-read_dta(bsa04p)

bsa03p<-paste0(bsapath, "/UKDA-5235-stata6", "/stata6/bsa03.dta")
bsa03<-read_dta(bsa03p)

bsa02p<-paste0(bsapath, "/UKDA-4838-stata6", "/stata6/bsa02.dta")
bsa02<-read_dta(bsa02p)

bsa01p<-paste0(bsapath, "/UKDA-4615-stata6", "/stata6/bsa01.dta")
bsa01<-read_dta(bsa01p)

bsa00p<-paste0(bsapath, "/UKDA-4486-stata8", "/stata8/bsa00.dta")
bsa00<-read_dta(bsa00p)

bsa99p<-paste0(bsapath, "/UKDA-4318-stata8", "/stata8/bsa99a.dta")
bsa99<-read_dta(bsa99p)

bsa98p<-paste0(bsapath, "/UKDA-4131-stata8", "/stata8/bsa98a.dta")
bsa98<-read_dta(bsa98p)

bsa97p<-paste0(bsapath, "/4072stata6", "/stata6/bsa97a.dta")
bsa97<-read_dta(bsa97p)

bsa96p<-paste0(bsapath, "/UKDA-3921-stata8", "/stata8/g921au.dta")
bsa96<-read_dta(bsa96p)

bsa95p<-paste0(bsapath, "/UKDA-3764-stata8", "/stata8/bsa95.dta")
bsa95<-read_dta(bsa95p)

bsa94p<-paste0(bsapath, "/UKDA-3572-stata8", "/stata8/bsa94.dta")
bsa94<-read_dta(bsa94p)

bsa93p<-paste0(bsapath, "/UKDA-3439-stata8", "/stata8/bsa93.dta")
bsa93<-read_dta(bsa93p)

bsa91p<-paste0(bsapath, "/UKDA-2952-stata8", "/stata8/f952.dta")
bsa91<-read_dta(bsa91p)

bsa90p<-paste0(bsapath, "/UKDA-2840-stata6", "/stata6/bsa90.dta")
bsa90<-read_dta(bsa90p)

bsa89p<-paste0(bsapath, "/UKDA-2723-stata8", "/stata8/2723.dta")
bsa89<-read_dta(bsa89p)

bsa87p<-paste0(bsapath, "/UKDA-2567-stata8", "/stata8/bsa87.dta")
bsa87<-read_dta(bsa87p)

path86<-paste0(bsapath,"/UKDA-2315-spss/spss/spss12")
bsa86<-read_spss(paste0(path86,"/2315.sav"))

path85<-paste0(bsapath,"/UKDA-2096-spss/spss/spss12")
bsa85<-read_spss(paste0(path85,"/bsa85.sav"))

path84<-paste0(docpath,"/BSA/UKDA-2035-spss/spss/spss12")
bsa84<-read_spss(paste0(path84,"/bsa84.sav"))

path83<-paste0(docpath,"/BSA/UKDA-1935-spss/spss/spss12")
bsa83<-read_spss(paste0(path83,"/bsa83.sav"))

bsa.scale.vars<-tolower(c("richlaw", "wealth", "redistrb", "unempjob", "BigBusnn", "indust4"))
bsa.other.vars<-tolower(c("Dole", "morewelf", "unempjob", "welffeet", "damlives", "sochelp", "dolefidl", "incomgap", "TaxSpend", "proudwlf", "FailClm", "welfhelp"))
bsa.pid.vars<-c("supparty", "closepty", "partyidn","partyid1", "partyid2", "partyid3", "idstrng")

bsa.vars<-tolower(c(bsa.scale.vars, bsa.other.vars, bsa.pid.vars))

bsa.lrscale<-c("rrichn",
               "rwealthn",    
               "rrd",
               "rbigbusnnn",
               "rind4n")
bsa.lrvars<- c("rrichn",
               "rwealthn",    
               "rrd",
               "unempjobn",
               "rbigbusnnn",
               "rdolen",
               "morewelf2n",
               "rind4n",
               "welffeetn",
               "damlives2n",
               "sochelpn",
               "dolefidln",
               "rincomgapn",
               "taxspendn",
               "proudwlf2n",
               "failclm2n",
               "welfhelpn")


survey.names<-paste0("bsa", c("83","84","85","86","87" ,"89", "90", "91", "93", "94", "95", "96", "97","98","99","00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16"))

years<-c("1983","1984","1985","1986","1987","1989", "1990", "1991", "1993", "1994", "1995", "1996","1997","1998","1999","2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016")


bsa_combined<-NULL
for(a in seq_along(survey.names)){
  this_bsa<-get(survey.names[a]) 
  names(this_bsa)<-tolower(names(this_bsa))
  # this_bsa<-this_bsa[,!names(this_bsa) %in% c("strttime", "ccname", "ownocc", "censusdc", "ward", "newdc", "intdate", "intnum", "sintdate", "sttim", "endtim")]
  this_bsa <- select(this_bsa, one_of(bsa.vars))
  this_bsa$year<-years[a]
  if(is.null(bsa_combined)){
    bsa_combined<-this_bsa
  } else {
    bsa_combined<-bind_rows(bsa_combined, this_bsa)
  }
}


bsac<-bsa_combined

not1to5_to_na <- function(x) {ifelse(x %in% 1:5, x, NA)}
notY_to_na <- function(x, Y=c(1:5)) {ifelse(x %in% Y, x, NA)}
not1to3_to_na <-function (x) {notY_to_na(x, 1:3)}
not1to2_to_na <-function (x) {notY_to_na(x, 1:2)}
reverse_scale <- function(x) {(max(x, na.rm = TRUE) + 1) - x}

bsac<-bsac %>% 
  mutate(pid_lab=pid_process_bsa(supparty, closepty, partyid2, idstrng, 2, pid_type = "all", no.values=c(0, 90))) %>%
  mutate(pid_con=pid_process_bsa(supparty, closepty, partyid2, idstrng, 1, pid_type = "all", no.values=c(0, 90))) %>%
  mutate(idonly_lab=pid_process_bsa(supparty, closepty, partyid2, idstrng, 2, pid_type = "identifiers", no.values=c(0, 90))) %>%
  mutate(idonly_con=pid_process_bsa(supparty, closepty, partyid2, idstrng, 1, pid_type = "identifiers", no.values=c(0, 90))) %>%
  mutate(pid_conlab=if_else(pid_con+pid_lab==0, NA_real_, pid_con)) %>%
  mutate(pid_labcon=if_else(pid_con+pid_lab==0, NA_real_, pid_lab)) %>%
  mutate(
    rrichn=reverse_scale(not1to5_to_na(richlaw)),
    rwealthn=reverse_scale(not1to5_to_na(wealth)),
    rrd=reverse_scale(not1to5_to_na(redistrb)),
    unempjobn=not1to5_to_na(unempjob),
    rbigbusnnn=reverse_scale(not1to5_to_na(bigbusnn)),
    rdolen=reverse_scale(not1to2_to_na(dole)),
    morewelf2n=reverse_scale(not1to5_to_na(morewelf)),
    rind4n = reverse_scale(not1to5_to_na(indust4)),
    welffeetn=not1to5_to_na(welffeet),
    damlives2n=reverse_scale(not1to5_to_na(damlives)),
    sochelpn=not1to5_to_na(sochelp),
    dolefidln=not1to5_to_na(dolefidl),
    rincomgapn=reverse_scale(not1to3_to_na(incomgap)),
    rtaxspendn=reverse_scale(not1to3_to_na(taxspend)),
    taxspendn=not1to3_to_na(taxspend),
    proudwlf2n=reverse_scale(not1to5_to_na(proudwlf)),
    failclm2n=reverse_scale(not1to2_to_na(failclm)),
    welfhelpna=not1to5_to_na(welfhelp)
  ) %>%
  mutate(useful.wave=TRUE,
         c=TRUE)


bsac$welfhelpn <- ifelse(bsac$year %in% c(1983, 1984), 6-bsac$welfhelpna, bsac$welfhelpna)

##----besfunctions----

rid_not1_to_5<-function(x){
  ifelse(x %in% (1:5), x, NA)
}
rid_not0_to_10<-function(x){
  ifelse(x %in% (0:10), x, NA)
}

rescale_1_11<-function(x){
  x<-ifelse(x %in% (1:11), x, NA)
  (x-1)/10
}
reverse_scale <- function(x) {(max(x, na.rm = TRUE) + 1) - x}

pid_process_bes<-function(pid, closer, party_strength, party_comp, pid_type="all", strength_multiplier=FALSE, ordinal=FALSE, na.values=c(-1, 96, 98, 99), no.values=c(90)){
  # function to create dummy (or dummy*strength) variable for party identitification
  # pid contains party identification
  # closer party closer (asked if doesn't identity with party)
  # party_support is answer to question which party vote for tomorrow (asked if not identifying with closer to party)
  # party_var indicators which party individual identifies with/is closer to/ or supports+
  # party_comp is indicator of which party to create the dummy (which party to compare the party_var value to)
  # pid_type:
  #     "all" - will convert any association with party into
  #     "identifiers" - only full identifiers
  #     "id+closer" - identifers and clser
  
  pid<-as.numeric(pid)
  closer<-as.numeric(closer)
  has_pid<-!pid %in% c(na.values, no.values)
  has_cls<-!closer %in% c(na.values, no.values)
  party_var<-case_when(
    pid_type=="identifiers"~ as.numeric(pid),
    pid_type=="id+closer"&has_pid~ as.numeric(pid),
    pid_type=="id+closer"&has_cls~ as.numeric(closer),
    pid_type=="all"&has_pid ~ pid,
    pid_type=="all"&has_pid ~ closer
  )
  y<-case_when(
    party_var==party_comp ~ 1,
    TRUE ~ 0
  )
  if(strength_multiplier){
    if(pid_strength %in% 1:3){
      y<-y*pid_strength
    } else {
      y<-NA_real_
    }
  }
  if(ordinal){
    y<-factor(y, levels=c(0, 1), ordered=TRUE)
    return(y)
  }
  as.numeric(y*1.0)
}



wave_to_year<-function(x){
  x<-as.character(x)
  y<-case_when(x %in% c("87", "92", "97")~paste0("19", x),
               x %in% c("01")~paste0("20", x)
  )
  y
}

which_wave<-function(x){
  y<-case_when(x %in% c("1_87", "2_92", "3_97", "1_1987", "2_1992", "3_1997")~"first",
               TRUE~"last")
  y
}

# Lab-Con feeling thermometers 
# lr1 = social services/taxation
# lr2 = nationalization
# lr3 = inflation
# lr4 = redistribution
lrvars<-c("lr1.self", "lr2.self", "lr3.self", "lr4.self")
beslrvars<-c("taxspend", "natlize", "infunemp", "redist") # reversed scales to compare with bsa, bhps and bsa

##----besdataprep----
# assumes bes data in spss format is in root folder 'bes.path'
# note: we had problems reading some files into R - we had to open in SPSS and reseave file

bes87<-read_spss(paste0(bes.path, "/cross_section_87_97/87BESresave.sav"))
bes92<-read_spss(paste0(bes.path, "/cross_section_87_97/92BESresave.sav"))
bes97<-read_spss(paste0(bes.path, "/cross_section_87_97/97BES.sav"))

bes87<-bes87 %>% 
  mutate(conth=v13a, labth=v13b, pid=v12a, clser=v12b, strength=v12c) %>%
  mutate_at(vars(c("conth", "labth")), rid_not1_to_5) %>%
  mutate(conth=reverse_scale(conth)) %>%
  mutate(lr1.self=v29a, lr1.con=v29b, lr1.lab=v29c) %>%
  mutate(lr2.self=v34a, lr2.con=v34b, lr2.lab=v34c) %>%
  mutate(lr3.self=v28a, lr3.con=v28b, lr3.lab=v28c) %>%
  mutate(lr4.self=v35a, lr4.con=v35b, lr4.lab=v35c) %>%
  mutate_at(vars(contains("lr")), rescale_1_11) %>%
  mutate(id=serialno) %>%
  mutate(year=1987) %>%
  mutate(useful_wave=TRUE)

bes92<-bes92 %>% 
  mutate(conth=v14a, labth=v14b, pid_a=va6a, clser_a=va6b, strength_a=va6c, pid_b=vb13a, clser_b=vb13b, strength_b=vb13c) %>%
  mutate(pid=ifelse(is.na(pid_a), pid_b, pid_a), clser=ifelse(is.na(clser_a), clser_b, clser_a)) %>%
  mutate_at(vars(c("conth", "labth")), rid_not1_to_5) %>%
  mutate(conth=reverse_scale(conth)) %>%
  mutate(lr1.self=va36a, lr1.con=va36b, lr1.lab=va36c) %>%
  mutate(lr2.self=va37a, lr2.con=va37b, lr2.lab=va37c) %>%
  mutate(lr3.self=va35a, lr3.con=va35b, lr3.lab=va35c) %>%
  mutate(lr4.self=va38a, lr4.con=va38b, lr4.lab=va38c) %>%
  mutate_at(vars(contains("lr")), rescale_1_11) %>%
  mutate(id=serialno) %>%
  mutate(year=1992) %>%
  mutate(useful_wave=TRUE)

bes97<-bes97 %>% 
  mutate(conth=conlike, labth=lablike, pid=partyid, clser=ptycls, strength=idstrng) %>%
  mutate_at(vars(c("conth", "labth")), rid_not0_to_10) %>%
  mutate(labth=reverse_scale(labth)) %>%
  mutate(lr1.self=rtxspd, lr1.con=contxspd, lr1.lab=labtxspd) %>%
  mutate(lr2.self=rprnat, lr2.con=conprnat, lr2.lab=labprnat) %>%
  mutate(lr3.self=rjbprc, lr3.con=conjbprc, lr3.lab=labjbprc) %>%
  mutate(lr4.self=rinceq, lr4.con=coninceq, lr4.lab=labinceq) %>%
  mutate_at(vars(contains("lr")), rescale_1_11) %>%
  mutate(id=serialno) %>%
  mutate(year=1997) %>%
  mutate(useful_wave=TRUE)

bes_x<-bind_rows(bes87, bes92, bes97)
bes_x<-bes_x %>%
  mutate(pid_lab=pid_process_bes(pid, clser, strength, 2, pid_type = "all", no.values=c(0, 90))) %>%
  mutate(pid_con=pid_process_bes(pid, clser, strength, 1, pid_type = "all", no.values=c(0, 90))) %>%
  mutate(idonly_lab=pid_process_bes(pid, clser, strength, 2, pid_type = "identifiers", no.values=c(0, 90))) %>%
  mutate(idonly_con=pid_process_bes(pid, clser, strength, 1, pid_type = "identifiers", no.values=c(0, 90))) %>%
  mutate(pid_conlab=if_else(pid_con+pid_lab==0, NA_real_, pid_con)) %>%
  mutate(pid_labcon=if_else(pid_con+pid_lab==0, NA_real_, pid_lab))

bes_x<-bes_x %>%
  mutate(taxspend=reverse_scale(lr1.self),
         natlize=reverse_scale(lr2.self),
         infunemp=reverse_scale(lr3.self),
         redist=reverse_scale(lr4.self))


bes_97_01<-read_spss(paste0(bes.path, "/panel_data_83_2001/1997-2001BESPanel.sav"))

# Lab-Con feeling thermometers (y11 (1983) and question v13(1987)) and moderate-extreme scale (question 13)
# lr1 = social services/taxation: v29a (self), (con v29b, lab v29c) 
# lr2 = nationalization
# lr3 = inflation
# lr4 = redistribution


bes3<-bes_97_01 %>% 
  mutate(conth_97=confel97, conth_01=confel01, labth_97=labfel97, labth_01=labfel01, pid_97=ptythn97, pid_01=ptythn01, closer_97=ptycls97, closer_01=ptycls01, strength_97=idstrn97, strength_01=idstrn01) %>%
  mutate(firstwave = wave1==1, lastwave= wave8==1) %>%
  mutate_at(vars(c("conth_97", "labth_97", "conth_01", "labth_01")), rid_not1_to_5) %>%
  mutate(lr1.self_97=rtxspd97, lr1.con_97=contxs97, lr1.lab_97=labtxs97) %>%
  mutate(lr1.self_01=rtxspd01, lr1.con_01=contxs01, lr1.lab_01=labtxs01) %>%
  mutate(lr2.self_97=rprn97, lr2.con_97=conprn97, lr2.lab_97=labprn97) %>%
  mutate(lr2.self_01=rprfw01, lr2.con_01=conpr01, lr2.lab_01=labpr01) %>%
  mutate(lr3.self_97=rjbprc97, lr3.con_97=conjbp97, lr3.lab_97=labjbp97) %>%
  mutate(lr3.self_01=rjbprc01, lr3.con_01=conjbp01, lr3.lab_01=labjbp01) %>%
  mutate(lr4.self_97=rieq97, lr4.con_97=conieq97, lr4.lab_97=labieq97) %>%
  mutate(lr4.self_01=rieq01, lr4.con_01=conieq01, lr4.lab_01=labieq01) %>%
  mutate_at(vars(contains("lr")), rescale_1_11) %>%
  mutate(id=paste0("pan3", serialno)) %>%
  mutate(waveresponses=paste0(firstwave, lastwave)) %>%
  mutate(panel=3) %>%
  dplyr::select(starts_with("lr"), contains("th_"), contains("fel"), contains("pid"), contains("strength"), contains("closer"), id, waveresponses, panel)

bes_3_wide<-bes3 %>% 
  dplyr::select(starts_with("lr"), contains("pid"), contains("closer"), contains("th_"), id, waveresponses, panel) %>%
  gather(variable, value, -id, -waveresponses, -panel) %>%
  separate(variable, c("variable", "wave"), "_") %>%
  spread(variable, value) %>%
  mutate(year=wave_to_year(wave)) %>%
  mutate(useful_wave=1) %>%
  unite(panwave, panel, wave, remove=FALSE) %>%
  mutate(pid_con=pid_process_bes(pid, closer, strength, 1), 
         pid_lab=pid_process_bes(pid, closer, strength, 2),
         pid_conlab=if_else(pid_con+pid_lab==0, NA_real_, pid_con),
         pid_labcon=if_else(pid_con+pid_lab==0, NA_real_, pid_lab),
         conth=reverse_scale(conth)) %>%
  mutate(taxspend=reverse_scale(lr1.self),
         natlize=reverse_scale(lr2.self),
         infunemp=reverse_scale(lr3.self),
         redist=reverse_scale(lr4.self))


bes<-bes_3_wide %>%
  dplyr::filter(year==2001) %>%
  mutate(id=as.numeric(str_replace(id, "pan", "")), year=as.numeric(year), useful_wave=TRUE) %>%
  bind_rows(bes_x) %>%
  dplyr::filter((!is.na(lr1.self))|(!is.na(lr2.self))|!is.na(lr3.self)|!is.na(lr4.self))

##----prepches----
# assumes Chapel Hill Expert Survey data in csv format is in root folder 'chtspath'

expert1<-read.csv(paste0(chtspath, "/1984-1999_dataset_means.csv"), stringsAsFactors = FALSE)
expert2<-read.csv(paste0(chtspath, "/1999-2014_CHES_dataset_means.csv"), stringsAsFactors = FALSE)
expert3<-read.csv(paste0(chtspath, "/CHES_means_2017.csv"), stringsAsFactors = FALSE)

uk.ex1<-expert1[expert1$uk==1, ]
uk.ex2<-expert2[expert2$country=="uk", ]
uk.ex3<-expert3[expert3$country=="uk", ]

uk.ex1$lrgen[is.na(uk.ex1$lrgen)]<-uk.ex1$lr_gen[is.na(uk.ex1$lrgen)]*10
v2in1<-names(uk.ex2)[names(uk.ex2) %in% names(uk.ex1)]
v2not1<-names(uk.ex2)[!names(uk.ex2) %in% names(uk.ex1)]
v1not2<-names(uk.ex1)[!names(uk.ex1) %in% names(uk.ex2)]
uk.ex1[,v2not1]<-NA

chts<-rbind(uk.ex1[, names(uk.ex2)], uk.ex2)
uk.ex3 <- mutate(uk.ex3, govt=ifelse(govt, "not in government", "in government"), position=as.character(position), eu_asylum=as.character(eu_asylum), lrgen=as.character(lrgen), lrecon=as.character(lrecon), galtan=as.character(galtan))
chts<-bind_rows(chts, uk.ex3)

chts$partyname<-chts$party
chts$lrgen[chts$lrgen=="center"]<-5
chts$lrgen<-as.numeric(chts$lrgen)
chts$partyname[chts$party=="TORY"]<-"CONS"
chts$partyname[chts$party_id==1104]<-"LDP"
chts$partyname[chts$party_id==1106]<-"PLAID"
chts$partyname[chts$party_id==1107]<-"GREEN"
chts$edate<-as.Date(paste0(chts$year, "-01-01"))
chts$party<-chts$party_id


