##############################
#
# Replication file for the mechanism analysis in:
#
# Reining in the Rascals: Challenger Parties' Path to Power
#
# For publication in the the Journal of Politics
#
# Frederik Hjorth, Jacob Nyrup, & Martin Vinæs Larsen
# 
##################

## Load packages ----

pacman::p_load(here,tidyverse,readxl,skimr,mirt,fixest,ggpubr,quanteda,quanteda.textmodels,yardstick)

# Set working dir
setwd(here(""))


# CLOSED RESPONSES ----

## read in data ----

#how many parties could we obtain data for?
#longdf0 <- read_csv("../../../analysis/Data/Mergeddata/mergedata_20190910.csv",locale = locale(encoding = "latin1"))

longdf <- read_rds("df_main.rds") %>% ungroup()

#muni codes
municodes <- read_delim("municodelist.txt",delim=":",col_names = c("municode","muniname"),locale = locale(encoding="UTF-8")) %>% 
  mutate(muniname=str_trim(muniname))

longdfr <- longdf %>% 
  transmute(year,municode=municipality,party,votes,mandates=mandates_calculated,lagmandates,
            threshold_gain=threshold_gain/totalvotes,threshold_loss=threshold_loss/totalvotes,ssi) %>% 
  filter(year %in% c(2013,2017)) %>% 
  left_join(municodes,by="municode")

longdfr2 <- longdfr %>% 
  filter(party %in% c("a","b","c","o","v","ø")) %>% 
  rename(partyabbr=party) %>% 
  mutate(party=case_when(partyabbr=="a" ~ "Socialdemokratiet",
                         partyabbr=="b" ~ "Radikale Venstre",
                         partyabbr=="c" ~ "Det Konservative Folkeparti",
                         partyabbr=="o" ~ "Dansk Folkeparti",
                         partyabbr=="v" ~ "Venstre, Danmarks Liberale Parti",
                         partyabbr=="ø" ~ "Enhedslisten - De Rød-Grønne"))

#import position data
kv13posdf <- read_xlsx("dkvaa2013.xlsx",sheet=3)
kv17posdf <- read_xlsx("dkvaa2017.xlsx")

# calculation of response rates
# for documentation on candidate numbers, see https://valg.im.dk/media/18343/2019-ny-statistik-om-kommunalbestyrelser-og-regionsraad-2017.pdf
nrow(kv13posdf) / 9083 #.99
nrow(kv17posdf) / 9556 #.99

#convert to numeric matrices
kv13posmat <- kv13posdf %>% 
  dplyr::select(CandidateID,Q1:Q11) %>% 
  pivot_longer(Q1:Q11,names_to="item",values_to = "response") %>% 
  mutate(value=case_when(response=="Helt uenig" ~ 1,
                         response=="Delvist uenig" ~ 2,
                         response=="Hverken enig eller uenig" ~ 3,
                         response=="Delvist enig" ~ 4,
                         response=="Helt uenig" ~ 5,
                         TRUE ~ NA_real_)) %>% 
  transmute(id=CandidateID,item,value) %>%
  pivot_wider(id_cols="id",names_from="item",values_from = "value") %>% 
  dplyr::select(-id)

kv17posmat <- kv17posdf %>% 
  dplyr::select(CandidateID,Q1:Q15) %>% 
  pivot_longer(Q1:Q15,names_to="item",values_to = "response") %>% 
  mutate(value=ifelse(response==0,NA,as.numeric(response))) %>% 
  transmute(id=CandidateID,item,value) %>%
  pivot_wider(id_cols="id",names_from="item",values_from = "value") %>% 
  dplyr::select(-id)

#get index of nonmissing rows
kv13nonmisrows <- which(rowSums(is.na(kv13posmat))!=11)
kv17nonmisrows <- which(rowSums(is.na(kv17posmat))!=15)

## run IRT model ----

kv13irt <- kv13posmat %>% 
  slice(kv13nonmisrows) %>% 
  mirt(.,1)
kv13irtscores <- fscores(kv13irt)[,1]

kv17irt <- kv17posmat %>% 
  slice(kv17nonmisrows) %>% 
  mirt(.,1)
kv17irtscores <- fscores(kv17irt)[,1]

#merge into position dfs
kv13posdf <- kv13posdf %>% 
  slice(kv13nonmisrows) %>% 
  transmute(year=2013,id=paste("2013",CandidateID,sep="-"),party=Party,muniname=Muncipality,score=kv13irtscores) %>% 
  mutate(domparty=ifelse(party %in% c("Socialdemokratiet","Venstre, Danmarks Liberale Parti"),1,0),
         party=ifelse(party=="Enhedslisten","Enhedslisten - De Rød-Grønne",party))

kv17posdf <- kv17posdf %>% 
  slice(kv17nonmisrows) %>% 
  transmute(year=2017,id=paste("2017",CandidateID,sep="-"),party=Party,muniname=Muncipality,score=kv17irtscores) %>% 
  mutate(domparty=ifelse(party %in% c("Socialdemokratiet","Venstre, Danmarks Liberale Parti"),1,0))

#verify that order is the same in both years
kv13posdf %>% 
  filter(domparty==1) %>% 
  group_by(party) %>% 
  summarise(posmean=mean(score))

kv17posdf %>% 
  filter(domparty==1) %>% 
  group_by(party) %>% 
  summarise(posmean=mean(score))

# get average position by party
posbyparty <- bind_rows(kv13posdf,kv17posdf) %>% 
  group_by(party) %>% 
  summarise(posmean=mean(score),nobs=n()) %>% 
  ungroup() %>% 
  filter(nobs>100)

write_rds(posbyparty,"irtposbyparty.rds")

#function for getting 'dominant party similarity' / 'extremity' for each muni-party
  dompossim <- function(partyx,muninamex,yrx) {
    if(yrx==2013){ yrdf <- kv13posdf }
    if(yrx==2017){ yrdf <- kv17posdf }
    mdf <- yrdf %>% 
      filter(muniname==muninamex)
    pmdf <- mdf %>% 
      filter(party==partyx) 
    munimeanpos <- mdf %>% 
#      filter(domparty==1) %>% 
      summarise(em=mean(score)) %>% 
      pull(em)
    pmeanpos <- pmdf %>% 
      summarise(pm=mean(score)) %>% 
      pull(pm)
    exty <- abs(pmeanpos - munimeanpos)
    simdf <- tibble(party=partyx,muniname=muninamex,
                    exty=exty,
                    year=yrx)
    return(simdf)
  }

#test
dompossim(partyx="Enhedslisten - De Rød-Grønne",muninamex = "Ballerup",yrx=2013)
dompossim(partyx="Dansk Folkeparti",muninamex = "Ballerup",yrx=2017)

pmapdf <- longdfr2 %>% 
  mutate(partyx=party,muninamex=muniname,yrx=year,.keep = "none")

dompossimdf <- pmap_dfr(pmapdf,dompossim,.progress=T)

#validate establishment positional similarity measure
dompossimdf %>% 
  mutate(moderate=ifelse(party %in% c("Dansk Folkeparti","Enhedslisten - De Rød-Grønne"),0,1)) %>% 
  lm(exty~moderate,data=.) %>% 
  summary()

## combine all data ----
closedf <- longdfr2 %>% 
  left_join(dompossimdf,by=c("party","muniname","year")) %>% 
  mutate(partytype=ifelse(partyabbr %in% c("o","ø"),"Challenger","Dominant"),
         treatstatus=ifelse(lagmandates==0,"Non-incumbents","Incumbents")) %>% 
  filter(!is.na(treatstatus)) %>% 
  mutate(pxt=factor(paste(partytype,treatstatus,sep=" - ")),
         munifac=factor(muniname),
         yearfac=factor(year)) %>% 
  filter(!is.na(exty))

closedf_cp <- closedf %>% 
  filter(partytype=="Challenger") %>% 
  mutate(incfac=relevel(factor(treatstatus),"Non-incumbents"))

closedf_dp <- closedf %>% 
  filter(partytype=="Dominant") %>% 
  mutate(incfac=relevel(factor(treatstatus),"Non-incumbents"))


# estimate TWFE models ----

fmcptwfe <- feols(exty~incfac | munifac + yearfac,data=closedf_cp,cluster="munifac")
summary(fmcptwfe)

fmdptwfe <- feols(exty~incfac | munifac + yearfac,data=closedf_dp,cluster="munifac")
summary(fmdptwfe)

#compare effect size to range of DV
abs(coef(fmcptwfe))/(max(closedf_cp$exty)-min(closedf_cp$exty))

#save predictions in data frame
poscoefdf <- bind_rows(broom::tidy(fmcptwfe),broom::tidy(fmdptwfe))

##save estimates to combine with text-based estimates
#write_rds(coefdf,"Papers/coalitionformation/analysis/04-positions-estimates.rds")


# OPEN RESPONSES ----

#read in 13 and 17 text data
kv13txtdf <- read_rds("dkvaa13txtdf.rds") %>% 
  mutate(domparty=ifelse(party %in% c("Det Konservative Folkeparti",
                                      "Radikale Venstre",
                                      "Socialdemokratiet",
                                      "Venstre, Danmarks Liberale Parti"),1,0),
         chalparty=ifelse(party %in% c("Dansk Folkeparti",
                                       "Enhedslisten - De Rød-Grønne"),1,0),
         year=2013) %>% 
  filter(fulltext!=" - NA  - NA  - NA")

kv17txtdf <- read_rds("dkvaa17txtdf.rds") %>% 
  mutate(domparty=ifelse(party %in% c("Det Konservative Folkeparti",
                                      "Radikale Venstre",
                                      "Socialdemokratiet",
                                      "Venstre, Danmarks Liberale Parti"),1,0),
         chalparty=ifelse(party %in% c("Dansk Folkeparti",
                                       "Enhedslisten - De Rød-Grønne"),1,0),
         year=2017) %>% 
  filter(fulltext!=" - NA  - NA  - NA")

## combine text data ----
fulltxtdf <- bind_rows(select(kv13txtdf,year,id,party,fulltext,municode,muniname,domparty,chalparty),
                       select(kv17txtdf,year,id,party,fulltext,municode,muniname,domparty,chalparty)) %>% 
  mutate(partytype=case_when(domparty==1 ~ "Dominant",
                             chalparty==1 ~ "Challenger",
                             TRUE ~ NA_character_)) %>% 
  filter(!is.na(partytype)) 

fulltxtcorp <- corpus(fulltxtdf,text_field = "fulltext")

fulltxtdfm <- dfm(fulltxtcorp)

dim(fulltxtdfm)

fulltxtdfm[,"enhedslisten"]

partynamespattern <- c("enhedslisten|df|dansk|folkeparti|venstre|radikale|konservative|socialdemokraterne")

fulltxtdfm_nolabels <- dfm_remove(fulltxtdfm,pattern=partynamespattern,valuetype="regex")

dim(fulltxtdfm_nolabels)

partytypelabels <- docvars(fulltxtcorp)$partytype

## fit support vector machine model ----
tmod_svm <- textmodel_svm(fulltxtdfm_nolabels,partytypelabels,type=0)

svmfeats <- summary(tmod_svm) %>% pluck(2) %>% names()
svmfeatscores <- summary(tmod_svm) %>% pluck(2) %>% as.numeric()
svmtopfeats <- tibble(feature=svmfeats,featurescore=svmfeatscores)
svmtopfeats %>% slice_min(featurescore,n=10)
svmtopfeats %>% slice_max(featurescore,n=10)

predprobs <- predict(tmod_svm,type = "probability")

## get precision and recall ----
precision_vec(as.factor(partytypelabels),as.factor(ifelse(predprobs[,2]>.5,"Challenger","Dominant")))
recall_vec(as.factor(partytypelabels),as.factor(ifelse(predprobs[,2]>.5,"Challenger","Dominant")))

#merge into full txt data
predprobsdf <- fulltxtdf %>% 
  select(year,party,municode,muniname) %>% 
  mutate(svmchalprob=predprobs[,2])

#merge into election data
#combine all data
closedf <- longdfr2 %>% 
  left_join(predprobsdf,by=c("party","muniname","year")) %>% 
  mutate(partytype=ifelse(partyabbr %in% c("o","ø"),"Challenger","Dominant"),
         treatstatus=ifelse(lagmandates==0,"Non-incumbents","Incumbents")) %>% 
  filter(!is.na(treatstatus)) %>% 
  mutate(pxt=factor(paste(partytype,treatstatus,sep=" - ")),
         munifac=factor(muniname),
         yearfac=factor(year))

closedf_cp <- closedf %>% 
  filter(partytype=="Challenger") %>% 
  mutate(incfac=relevel(factor(treatstatus),"Non-incumbents"))

closedf_dp <- closedf %>% 
  filter(partytype=="Dominant") %>% 
  mutate(incfac=relevel(factor(treatstatus),"Non-incumbents"))

## fit TWFE models ----
fmcptwfe <- feols(svmchalprob~incfac | munifac + yearfac,data=closedf_cp,cluster="munifac")
summary(fmcptwfe)

fmdptwfe <- feols(svmchalprob~incfac | munifac + yearfac,data=closedf_dp,cluster="munifac")
summary(fmdptwfe)

# get t-stat and p-val
broom::tidy(fmcptwfe)
broom::tidy(fmdptwfe)


#preds based on TWFE models
poscoefdf <- poscoefdf %>% 
  mutate(dv="pos",type=c("Challenger","Dominant"),upper=estimate+1.96*std.error,lower=estimate-1.96*std.error)

txtcoefdf <- bind_rows(broom::tidy(fmcptwfe),broom::tidy(fmdptwfe)) %>% 
  mutate(dv="txt",type=c("Challenger","Dominant"),upper=estimate+1.96*std.error,lower=estimate-1.96*std.error)

#compare coefficient sizes
1 - txtcoefdf[2,2]/txtcoefdf[1,2]

# plot estimates ----
ggplot(poscoefdf,aes(x=type,y=estimate)) +
  geom_hline(yintercept=0,linetype="dashed",alpha=.5) +
  geom_point() +
  geom_errorbar(aes(ymax=upper,ymin=lower),width=0.05) +
  theme_bw() +
  labs(x="",y="Effect of incumbency on extremity")

ggsave("output/fig6_a.pdf",width=4,height=3)

ggplot(txtcoefdf,aes(x=type,y=estimate)) +
  geom_hline(yintercept=0,linetype="dashed",alpha=.5) +
  geom_point() +
  geom_errorbar(aes(ymax=upper,ymin=lower),width=0.05) +
  theme_bw() +
  labs(x="",y="Effect of incumbency on Pr(Challenger)")

ggsave("output/fig6_b.pdf",width=4,height=3)


beepr::beep(8)
