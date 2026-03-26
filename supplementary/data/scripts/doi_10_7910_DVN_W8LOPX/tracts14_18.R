options(scipen = 999)
options(stringsAsFactors = F)

#point to your path containing the input data
path = 'D:\\Work\\BIDS\\Final Scripts\\2014-2018'
setwd(path)

#input data are the FCC Form 477 .csvs
#available at:
#https://www.fcc.gov/general/broadband-deployment-data-fcc-form-477

#change this file to whatever year/file of interest
df = read.csv('fbd_us_without_satellite_dec2017_v3.csv',header=T)

#remove TechCodes 0, 60
dropTechs = c(0,60)
df = df[!(df$TechCode %in% dropTechs),]

#remove providers that do not provide service to consumer or business
df = df[df$Consumer==1 | df$Business==1,]

#remove american territories
dropStates = c('AS','GU','MP','PR','VI')
df = df[!(df$StateAbbr %in% dropStates),]

#add 2010 tract ID
df$Tract = ifelse(nchar(df$BlockCode)==15, substr(df$BlockCode,1,11), substr(df$BlockCode,1,10))

#all unique tracts
tracts = unique(df$Tract)

#create final df
#adjust this for more/fewer years than 14-17
finaldf = as.data.frame(matrix(0,
                               nrow = length(tracts),
                               ncol = 6))
colnames(finaldf) = c('Tract','prov14','prov15','prov16','prov17','prov18')
finaldf$Tract = tracts

#for each tract, count number of unique providers by holding company number
for (i in 1:nrow(finaldf)) {
  tt = finaldf$Tract[i]
  
  #isolate relevant entries from FCC data
  tdf = df[df$Tract==tt,]
  
  #change this to the year of interest
  finaldf$prov17[i] = length(unique(tdf$HocoNum))
}

#write results or loop up and re-run for other years first
write.csv(finaldf,'temp_results.csv')