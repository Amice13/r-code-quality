#Set to the path with the input data
path = 'D:\\Work\\BIDS\\data'
setwd(path)

options(scipen = 999)
options(stringsAsFactors = F)

#start by re-building changetypes
df = read.csv('crosswalk_2000_2010_household.csv')

#remove 0 contribution entries
df = df[df$HUPCT00>0,]

#all 2010 tracts
tracts10 = unique(df$GEOID10)

#create final DF
finaldf = as.data.frame(matrix(0,
                               nrow = length(tracts10),
                               ncol = 2))
colnames(finaldf) = c('TRACT10','changetype')

finaldf$TRACT10 = tracts10

#add changetypes
for (i in 1:length(tracts10)) {
  tdf = df[df$GEOID10==tracts10[i],]
  
  inCo = nrow(tdf)
  
  #for a single input 2000 tract:
  if (inCo == 1) {
    ttdf = df[df$GEOID00==tdf$GEOID00[1],]
    
    #and a single output 2010 tract for that input:
    if (nrow(ttdf)==1) {
      finaldf[which(finaldf$TRACT10==tracts10[i]),]$changetype = 1
    }
    
    #and now we check for type 3
    if (nrow(ttdf)>1) {
      finaldf[which(finaldf$TRACT10==tracts10[i]),]$changetype = 3
    }
  }
  #now checking for 2 and 4
  if (inCo > 1) {
    for (j in 1:nrow(tdf)) {
      ttdf = df[df$GEOID00==tdf$GEOID00[j],]
      if (nrow(ttdf)>1) {
        finaldf[which(finaldf$TRACT10==tracts10[i]),]$changetype = 4
        break
      }
      else if (nrow(ttdf)==1) {
        finaldf[which(finaldf$TRACT10==tracts10[i]),]$changetype = 2
        break
    }
  }
  }
}

#write results if needed
write.csv(finaldf, 'changeTypes.csv')

#### Above block for changetype assignment, below for mixed strategy (broadband provider assignment) ####
#df = finaldf
#or read in the changeTypes.csv written previously
df = read.csv('changeTypes.csv')

df$prov08 = 0
df$prov09 = 0
df$prov10 = 0

#and this is the ref df for contributions
refdf = read.csv('crosswalk_2000_2010_household.csv')

refdf$changetype = NULL
refdf$Num_GEOID00 = NULL

#the functions we'll need
dirAssign = function(tdf) {
  return(tdf[tdf$HUPCT00==max(tdf$HUPCT00),]$tprov[1])
}

maxWeight = function(tdf) {
  mw = max(tdf$HUPCT00)
  ttdf = tdf[tdf$HUPCT00==mw,]
  return(max(ttdf$tprov))
}

weightSum = function(tdf) {
  sumProv = 0
  for (k in 1:nrow(tdf)) {
    sumProv = sumProv + (tdf$HUPCT00[k]*tdf$tprov[k])
  }
  return(sumProv)
}

#starting with 2008, select year here
#also need to change the df$provX refs throughout

tractdf = read.csv('hs_mapdata_dec_2008.csv')
#tractdf = read.csv('csv_tractdata_dec_2009.csv')
#tractdf = read.csv('hs_tractdata_v2_dec_2010.csv')

for (i in 1:nrow(df)) {
  #the tract of interest for this iteration
  tid = df$TRACT10[i]
  tchange = df$changetype[i]
  
  if (tchange==-1) {
    df$prov08[i] = -1 #change this based on year
  }
  
  #find all the 2000 contribution tracts
  tdf = refdf[refdf$GEOID10==tid,]
  
  #add 2000 tract providers from tractdf
  tdf$tprov = 0
  for (j in 1:nrow(tdf)) {
    tprov = tractdf[tractdf$tract_fips==tdf$GEOID00[j],]$total_prov
    if (length(tprov)==0) {
      tprov=-1
    }
    else if (length(tprov)>0) {
      if (is.na(tprov)) {
        tprov = -1
      }
    }
    tdf$tprov[j] = tprov
  }
  
  #remove entries with -1 (if nothing left, -1 is the result)
  ttdf = tdf[tdf$tprov != -1,]
  if (nrow(ttdf)==0) {
    df$prov08[i] = -1 #change this based on year
    next
  }
  else {
    tdf = ttdf
  }
  
  ##call relevant function
  #this is just direct assignment
  if (tchange==1) {
    df$prov08[i] = dirAssign(tdf) #change this based on year
    
    next
  }
  
  #the tract has 2+ 1st order neighbors but no 2nd order neighbors
  #tie max weight strategy
  else if (tchange==2) {
    df$prov08[i] = maxWeight(tdf) #change this based on year
    
    next
  }
  
  else if (tchange==3) {
    df$prov08[i] = weightSum(tdf) #change this based on year
    
    next
  }
  
  else if (tchange==4) {
    if (nrow(tdf)==1) {
      df$prov08[i] = weightSum(tdf) #change this based on year
    }
    else if (nrow(tdf)>1) {
      df$prov08[i] = maxWeight(tdf) #change this based on year
    }
    
    next
  }
}

#change results name to reflect input year as needed
write.csv(df, 'results_mixed_2008.csv')
