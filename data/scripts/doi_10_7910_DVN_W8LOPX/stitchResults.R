path = 'D:\\...' #set path
setwd(path)

options(scipen = 999)
options(stringsAsFactors = F)

#load in the five files to be stitched
df08_10 = read.csv('path//2008_2010 results.csv')

df11 = read.csv('path//hs_tractdata_v2_dec_2011.csv') #the 2011 form 477 data from the FCC
df12 = read.csv('path//hs_tractdata_v2_dec_2012.csv') #the 2012 form 477 data from the FCC
df13 = read.csv('path//hs_tractdata_v3_dec_2013.csv') #the 2013 form 477 data from the FCC

df14_18 = read.csv('path//2014_2018 results.csv')

dfTract = read.csv('path//USCensusTracts.csv') #a census tract file with tract ID and tract area

#total list of unique tracts contained
tTracts = c(unique(df08_10$TRACT10), 
            unique(df14_18$TRACT10), 
            unique(df11$tract_fips), 
            unique(df12$tract_fips), 
            unique(df13$tract_fips))

uTracts = unique(tTracts)


#create the final dataset
df = as.data.frame(matrix(0,
                          nrow = length(uTracts),
                          ncol = 20
                          ))

colnames(df) = c('TRACT10','changetype',
                 'prov08','prov09','prov10','prov11','prov12','prov13','prov14','prov15','prov16','prov17','prov18',
                 'flag3','flag4','flag5','flag13',
                 'delta11_10','delta18_08','delta14_13',
                 'area')

df$TRACT10 = uTracts

#add results from subresult files to the final df
for (i in 1:nrow(df)) {
  tt = df$TRACT10[i]
  
  #08-10
  tdf = df08_10[df08_10$TRACT10 == tt,]
  
  df$changetype[i] = tdf$changetype[1]
  df$prov08[i] = tdf$prov08[1]
  df$prov09[i] = tdf$prov09[1]
  df$prov10[i] = tdf$prov10[1]
  
  #11-13
  tdf = df11[df11$tract_fips == tt,]
  
  df$prov11[i] = tdf$total_prov[1]
  
  tdf = df12[df12$tract_fips==tt,]
  
  df$prov12[i] = tdf$total_prov[1]
  
  tdf = df13[df13$tract_fips==tt,]
  
  df$prov13[i] = tdf$total_prov[1]
  
  #14-18
  tdf = df14_18[df14_18$TRACT10 == tt,]
  
  df$prov14[i] = tdf$prov14[1]
  df$prov15[i] = tdf$prov15[1]
  df$prov16[i] = tdf$prov16[1]
  df$prov17[i] = tdf$prov17[1]
  df$prov18[i] = tdf$prov18[1]
  
  #and use this loop to add area
  tdf = dfTract[dfTract$tract_fips == tt,]
  
  df$area[i] = tdf$area[1]
}

#now derive the flag/delta columns
df$delta11_10 = df$prov11-df$prov10
df$delta14_13 = df$prov14-df$prov13
df$delta18_08 = df$prov18-df$prov08

df$flag5 = ifelse(abs(df$delta11_10) > 5, 1, 0)
df$flag4 = ifelse(abs(df$delta11_10) > 4, 1, 0)
df$flag3 = ifelse(abs(df$delta11_10) > 3, 1, 0)

df$flag13 = ifelse(df$prov13 == 1, 1, 0)

df$TRACT10 = ifelse(nchar(df$TRACT10)==10,paste(0,df$TRACT10,sep=''),df$TRACT10)

#write the final results
write.csv(df, 'path//form477_by_tract.csv')
