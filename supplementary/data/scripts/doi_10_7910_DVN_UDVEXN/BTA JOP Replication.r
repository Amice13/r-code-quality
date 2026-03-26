##########
#### Assessing Ballot Structure and Split Ticket Voting
#### Barnes, Tchintian, Alles
#### Journal of Politics, forthcoming.
##########

#### updated: 06/25/2016

#### Required
#### Packages

require(MatchIt)
require(arm)

require(multiwayvcov)
require(lmtest)
require(stargazer)
require(sandwich)
require(lme4)

require(doBy)
require(reshape)
require(ggplot2)
require(grid)


rm(list=ls())


#### Setting
#### Directories

main.dir <- ""     ## set directory path here


final.dir <- paste(main.dir, "/Final Data", sep="")
graph.dir <- paste(main.dir, "Graphs", sep="/")

dir.create(final.dir, showWarnings=F, recursive=T)
dir.create(graph.dir, showWarnings=F, recursive=T)


#### Loading
#### Data

### Electoral Data: Gov-House

dat <- subset(read.csv(paste(main.dir,"GovHouse_election_data_2007_2011_2015.csv",sep="/")), 
              dept=="CAPITAL")

### Census Data

precincts <- subset(merge(read.csv(paste(main.dir, "precinct_census_data.csv", sep="/")),
                          read.csv(paste(main.dir, "precinct_ev_2011.csv", sep="/")),
                          by = "precinct", all=T),
                    select = c(precinct, electronic, nbi, edu, gis.method))
colnames(precincts) <- c("precinct","electronic.device", "poverty.rate", "non.educated.rate", "gis.method")

precincts <- precincts[precincts$precinct%in%unique(subset(dat, year==2011)$precinct), ]


### Precinct History

opposition.votes <- merge(summaryBy(gov.votes ~ precinct, FUN=sum, na.rm=T, 
                                    keep.names = T, var.names = "opposition.votes",
                                    data = subset(dat, year == 2007 & party.number == 2)),
                          summaryBy(gov.votes ~ precinct, FUN=sum, na.rm=T, 
                                    keep.names = T, var.names = "total.votes",
                                    data = subset(dat, year == 2007)),
                          by="precinct", all = T)
opposition.votes$opposition.vote.share <- with(opposition.votes, opposition.votes/total.votes)

precincts <- merge(precincts, subset(opposition.votes, select = c(precinct, opposition.vote.share)), 
                   by="precinct", all.x=T)

rm(opposition.votes)


### Voting Booth Data, 2011

votingbooth_dat <- read.csv(paste(main.dir,"election_data_2011_by_votingbooth.csv",sep="/"))
votingbooth_dat <- subset(votingbooth_dat, dept=="CAPITAL")

vot.centers <- read.csv(paste(main.dir,"voting_centers_directory.csv",sep="/"))
colnames(vot.centers) <- c("N", "department", "municipality", "precinct", "voting.center",
                           "address", "number.tables", "electronic")
vot.centers <- vot.centers[!names(vot.centers) %in% c("N", "address", "number.tables")]
vot.centers <- subset(vot.centers, department=="CAPITAL" & is.na(precinct)==F)

votingbooth_dat <- merge(votingbooth_dat, vot.centers, by.x=c("dept","mun","voting.center"),  by.y=c("department", "municipality", "voting.center"), all = T)
votingbooth_dat <- subset(votingbooth_dat, is.na(precinct)==F)

votingbooth_dat$party.id <- with(votingbooth_dat, ifelse(party.name=="VOTOS EN BLANCO", 9998, party.id))



### Auxiliary Data: Party Labels

parties <- data.frame(orderBy(~ year + party.number, 
                              data=merge(unique(subset(dat, select=c(year,party.number))),
                                         read.csv(paste(main.dir,"/","party.ids.csv",sep="")),
                                         by=c("year","party.number"))),
                      row.names=NULL)


### Final Dataset

dat <- data.frame(subset(orderBy(~ gis.method + year + precinct + party.number, 
                                 data = merge(dat, precincts, by="precinct", all.x=T)),
                         party.number < 9998), row.names=NULL)


#### Regression 
#### Models

### Number of Simulations
n.sims <- 50000

### Results Store
pooled.pred <- pooled.coef <- c()

### Matched Data
party.matched.data <- prect.matched.data <- c()


for(m in 1:length(na.omit(unique(dat$gis.method)))){
  
  gis.method <- as.character(na.omit(unique(dat$gis.method))[m])
  
  #### Directories
  
  match.dir <- paste(main.dir, "Matching Reports", gis.method, sep="/")
  result.dir <- paste(main.dir, "Results", gis.method, sep="/")
  robust.dir <- paste(main.dir,"Results","Robustness Tests",gis.method,sep="/")
  
  dir.create(result.dir, showWarnings=F, recursive=T)
  dir.create(robust.dir, showWarnings=F, recursive=T)
  
  if(gis.method=="Nearest Neighbor") gis.acro <- "nn"  else gis.acro <- "tp"
  
  match.method <- c("psm", "cem")
  match.folder <- c("Propensity Score Matching", "Coarsened Exact Matching")
  
  for(k in 1:length(unique(dat$year))){
    
    #### Models by year & by party
    
    #### Table 2 (Article) and Tables E1 & E2 (Appendix)
    #### Tables C1 & C2 (Appendix)
    #### Figures C1 & C2 (Appendix)
    
    source(paste(main.dir, "bta_jop_models_by_party.r",sep="/"))
    
    
    #### Precinct Level Dataset
    
    source(paste(main.dir, "bta_jop_precinct_data.r",sep="/"))
    
    
    #### Difference in Difference, by methods
    
    if(unique(dat$year)[k] %in% c(2011,2015)){
      
      #### Table 1 (Article) and Tables D1, D2 & D3 (Appendix)
      DinD.data <- prect.matched.data
      DinD.dir <- result.dir
      sample <- "All legislative tickets (including allies)"
      source(paste(main.dir, "bta_jop_DinD.r",sep="/"))
      rm(DinD.data, DinD.dir, sample)
      
      #### Table F1, F2, F3 & F4 (Appendix)
      DinD.data <- read.csv(paste(main.dir, "GovHouse_mainparties_data_2007_2011_2015.csv",sep="/"))
      DinD.dir <- robust.dir
      sample <- "Main legislative tickets (excluding allies)"
      file <- "robust"
      source(paste(main.dir, "bta_jop_DinD.r",sep="/"))
      rm(DinD.data, DinD.dir, sample, file)
      
    }

    
  } ; rm(k)
  
    
  #### Robustness
  #### Tests 
  
  #### (1) Variables in the Matching
  #### Table B1 (Appendix)
  
  selection.data <- subset(precincts, select = c(precinct, electronic.device, gis.method,
                                                 poverty.rate, non.educated.rate, opposition.vote.share),
                           gis.method == unique(dat$gis.method)[m])
  selection.data[,4:6] <- selection.data[,4:6] * 100
  
  selection.mod <- glm(electronic.device ~ poverty.rate + non.educated.rate + opposition.vote.share,
                     data = selection.data, family = binomial(link = "logit"))
  
  tab.title <- "Assignment of Electronic Machines, by Demographics and Party vote"
  tab.subtitle <- "(Salta District, 2011. Logit regression model."
  tab.subtitle <- paste(tab.subtitle, paste(gis.method, ")", sep=""))
  
  ### Selection Model
  cat(tab.title, tab.subtitle,
      capture.output(summary(selection.mod)),
      file=paste(robust.dir, paste("selection.mod", gis.acro, "txt", sep="."), sep="/"), 
      sep="\n", append=F)
  
  
  rm(selection.data, selection.mod, tab.title, tab.subtitle)
  
  #### (2) Potential Prevalence of Missing Ballots
  #### Figure F1, Table F5 & F6 (Appendix)
  
  source(paste(main.dir, "bta_jop_miss_ballot.r",sep="/"))
  
  
  rm(result.dir, robust.dir, gis.method, gis.acro, match.dir, match.method, match.folder)
  
  
} ; rm(m, n.sims)


#### Building
#### Graphs

plot.col <- c("darkgreen", "gray")

#### Descriptive Graphs
#### Figure 2 (Article)
source(paste(main.dir,"bta_jop_descriptive_plots.r", sep="/"))


#### Marginal Effects
#### Figure 3 (Article) and Figure E1 (Appendix)
source(paste(main.dir,"bta_jop_marginal_effect_plots.r", sep="/"))

rm(plot.col)


#### Descriptive
#### Data

#### Tables B2 (Appendix)

sum.dat <- merge(summaryBy(sum.voting.center ~ precinct, FUN=sum, na.rm=T, 
                           data=data.frame(unique(subset(votingbooth_dat, select=c(precinct,voting.center))), sum.voting.center=1), keep.names=T),
                 summaryBy(sum.voting.booth ~ precinct, FUN=sum, na.rm=T, 
                           data=data.frame(unique(subset(votingbooth_dat, select=c(precinct,voting.booth))), sum.voting.booth=1), keep.names=T),
                 by="precinct", all=T)

sum.dat <- merge(unique(subset(vot.centers, select=c(precinct,electronic))), sum.dat, by="precinct", all.y=T)
sum.dat$electronic <- with(sum.dat, ifelse(electronic==1,"Yes","No"))

write.csv(sum.dat, na="", row.names=F, 
          file=paste(main.dir, "Results", 
                     "Table B2. Voting Centers and Voting Booths by Precinct.csv",
                     sep="/"))

rm(sum.dat)



#### Storing
#### Model Results

write.csv(file = paste(main.dir, "Results", "BTA JOP 2016. Model Coefficients.csv", sep="/"),
          pooled.coef, na="", row.names=FALSE)

write.csv(pooled.pred, na="", row.names=FALSE, 
          file = paste(main.dir, "Results", "BTA JOP 2016. Model Predictions.csv", sep="/"))


#### Storing
#### Matched Data

write.csv(file = paste(final.dir, "BTA JOP 2016. Matched Data. Salta 2007-2015 by Party.csv", sep="/"),
          party.matched.data, na="", row.names=FALSE)
write.csv(file = paste(final.dir, "BTA JOP 2016. Matched Data. Salta 2007-2015 by Precinct.csv", sep="/"),
          prect.matched.data, na="", row.names=FALSE)


rm(vot.centers, votingbooth_dat, parties, party.labels, precincts)
