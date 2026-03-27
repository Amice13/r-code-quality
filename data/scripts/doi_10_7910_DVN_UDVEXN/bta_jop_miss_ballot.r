##########
#### Assessing Ballot Structure and Split Ticket Voting
#### Barnes, Tchintian, Alles
#### Journal of Politics, forthcoming.
##########

#### updated: 06/25/2016

### Model
### Data

### Census Data
soc_ec <- read.csv(paste(main.dir, "precinct_census_data.csv", sep="/"))
soc_ec <- soc_ec[soc_ec$precinct %in% unique(vot.centers$precinct), ]
soc_ec <- subset(soc_ec, select=c(precinct,gis.method,pop_3orMore,edu,nbi)) 
colnames(soc_ec) <- c("precinct", "gis.method", "population", "non.educated.rate", "poverty.rate")

### Electoral Data
mod.dat <- subset(votingbooth_dat, office=="GOBERNADOR" & party.id<9998)
mod.dat <- merge(mod.dat, soc_ec, by="precinct", all=T)
mod.dat <- subset(mod.dat, electronic==1)


### Model Estimation

for(i in 1:length(unique(mod.dat$party.id))){
  
  options(warn=-1)

  ### Data
  loop.dat <- mod.dat[mod.dat$party.id %in% unique(mod.dat$party.id)[i], ]
  loop.dat <- loop.dat[loop.dat$gis.method %in% unique(mod.dat$gis.method)[m], ]
  
  party_id <- unique(mod.dat$party.id)[i]
  party_id <- ifelse(party_id<10, paste("00",party_id,sep=""), ifelse(party_id<100, paste("0",party_id,sep=""), party_id))
 
  ### Estimation
  loop.mod <- lmer(votes ~ population + non.educated.rate + poverty.rate +
                     (1 | voting.center), 
                   data=loop.dat)
  
  options(warn=0)
  
  ### Model Results
  tab.title <- "Robustness Tests to Detect the Prevalence of Missing Ballots:"
  tab.subtitle1 <- paste("Linear mixed-effects model,", gis.method)
  tab.subtitle2 <- "Salta District, 2011."
  tab.subtitle2 <- paste(tab.subtitle2, 
                         paste(subset(parties, year==2011 & 
                                        party.number==unique(mod.dat$party.id)[i])$data.label,
                               paste("#",party_id, sep="")))

  cat(tab.title, tab.subtitle1, tab.subtitle2,"",
      capture.output(display(loop.mod, digits=3, detail=T)),
      file=paste(robust.dir, paste("miss","ballots",gis.acro,
                                   paste("party",party_id,sep="_"),
                                   "txt",sep="."), sep="/"), 
      sep="\n",append=F)
  

  ### Random Intercepts
  loop.coef <- ranef(loop.mod)$voting.center
  loop.coef <- data.frame(unique(mod.dat$party.id)[i],unique(mod.dat$gis.method)[m], 
                          data.frame(loop.coef, row.names=NULL))
  colnames(loop.coef) <- c("party.id","gis.method","coef")
  
  if("ranef.store" %in% ls()==F) ranef.store <- loop.coef else ranef.store <- rbind(ranef.store, loop.coef)
  
  rm(loop.dat, loop.mod, loop.coef, party_id, tab.title, tab.subtitle1, tab.subtitle2)
  
  
}


if(m==length(na.omit(unique(dat$gis.method)))){
  
  source(paste(main.dir, "bta_jop_miss_ballot_p.r",sep="/"))
  
}


rm(mod.dat, soc_ec, i)

