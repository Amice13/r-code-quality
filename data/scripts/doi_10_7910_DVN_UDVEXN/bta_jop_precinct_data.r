##########
#### Assessing Ballot Structure and Split Ticket Voting
#### Barnes, Tchintian, Alles
#### Journal of Politics, forthcoming.
##########

#### updated: 06/25/2016

#### Required
#### Packages

require(MatchIt)


#### Precinct 
#### Level
#### Dataset

for(j in 1:length(match.method)){
  
  #### Precinct Data
  
  prect.data <- subset(dat, gis.method == unique(dat$gis.method)[m] & year == unique(dat$year)[k],
                       select=c(precinct, party.number, gov.votes, dip.votes, electronic.device))  
  prect.data <- data.frame(prect.data, with(prect.data, abs(gov.votes-dip.votes)), row.names=NULL)
  colnames(prect.data) <- c("precinct", "party.num", "gov.votes", "dep.votes", "electronic.device", "abs.gap")
  
  prect.data <- merge(summaryBy(abs.gap ~ precinct + electronic.device, FUN=sum, keep.names=T, data=prect.data),
                      summaryBy(gov.votes+dep.votes ~ precinct + electronic.device, FUN=sum, keep.names=T, data=prect.data),
                      by = c("precinct", "electronic.device"), all = T )
  
  prect.data <- data.frame(prect.data, max.votes=apply(subset(prect.data, select=c(gov.votes, dep.votes))[, 1:2], 1, max))
  
  prect.data <- data.frame(subset(prect.data, select=c(precinct, electronic.device)),
                           with(prect.data, data.frame(split.share=(abs.gap/2)/max.votes*100)))
  
  prect.data <- merge(prect.data, unique(subset(dat, gis.method==unique(dat$gis.method)[m] & year==unique(dat$year)[k],
                                                select=c(precinct, poverty.rate, non.educated.rate, opposition.vote.share))),
                      by = "precinct", all = T)
  
  prect.data$year <- unique(dat$year)[k]
  prect.data$gis.method <- gis.method
  
  prect.data <- merge(unique(subset(dat, year==unique(dat$year)[k], select=c(precinct,dept,mun))), prect.data,
                      by="precinct", all.y=T)
  
  #### Matching Data
  
  if(match.method[j]=="psm"){
    
    match <- matchit(electronic.device ~ poverty.rate + non.educated.rate + opposition.vote.share, 
                     data = prect.data, method = "nearest", distance = "probit", replace = T)
        
    match.dat <- match.data(match, "all")
    match.weight <- match.data(match, "all")$weights
    
    prect.data <- data.frame(matching.method=match.method[j], match.dat, subclass=NA)
    
    
  } else {
    
    match <- matchit(electronic.device ~ poverty.rate + non.educated.rate + opposition.vote.share, 
                     cutpoints = with(prect.data, list(poverty.rate = hist(poverty.rate, breaks=5, plot=F)$breaks,
                                                       non.educated.rate = hist(non.educated.rate, breaks=5, plot=F)$breaks,
                                                       opposition.vote.share = hist(opposition.vote.share, breaks=5, plot=F)$breaks)), 
                     data = prect.data, method = "cem", k2k=T)
    
    match.dat <- match.data(match, "all")
    match.weight <- match.data(match, "all")$weights
    
    prect.data <- data.frame(matching.method=match.method[j], match.dat)
    
  }
  
  
  #### Building Data
  
  vars <- colnames(party.matched.data)[colnames(party.matched.data)%in%colnames(prect.data)]
  
  prect.data <- subset(prect.data, select = vars)
  
  prect.matched.data <- data.frame(subset(rbind(prect.matched.data,  prect.data), 
                                          is.na(precinct)==F), row.names = NULL)
  
  
  
  #### Cleaning Memory
  
  rm(prect.data, match, match.dat, match.weight, vars)
  
  
}

rm(j)
