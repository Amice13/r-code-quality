#Set working directory to replication folder
#-----------------------------
#Packages necessary to run analyses
#library(tidyverse)
#library(readxl)
#library(estimatr)
#library(texreg)
#-----------------------------------------------------------------

#Set parameters for plots
title.size <- 20
x.axis.tick.size <- 14
y.axis.tick.size <- 14
x.axis.label.size <- 18
y.axis.label.size <- 18
facet.text <- 15

#-------------------------------------------------------------------
#Load data
judges <- read_csv('Data/Raw/Judges_in_Circuit.csv')
dime <- read_csv('Data/Raw/Bonica_Sen_Judicial_Ideology_2017.csv')
dime_updated <- read_csv('Data/Raw/Bonica_Sen_Judicial_Ideology_2021.csv')
fjc <- read_excel('Data/Raw/FJC_Judge_Data.xlsx')
fjc$`Middle Name` = ifelse(is.na(fjc$`Middle Name`),' ',fjc$`Middle Name`)
fjc$Suffix = ifelse(is.na(fjc$Suffix),'',paste(' ',trimws(fjc$Suffix),sep=''))
fjc$name = paste(fjc$`Last Name`,', ',fjc$`First Name`,' ',fjc$`Middle Name`,',', fjc$Suffix,sep='')

cofi <- read_excel('Data/Taboni_COFI_Analysis.xlsx')
cofi <- cofi %>% 
  mutate(Postion = as.numeric(Position)) %>%
  mutate(Liberal = as.numeric(Liberal)) 

#-------------------------------------------------------------------
#Calculate panel medians for en banc cases
medians <- judges %>%
  left_join(select(fjc, nid,jid), by = 'nid')%>%
  left_join(select(dime_updated, fjc.judge.idno,dime.score), by = c('jid'='fjc.judge.idno'), relationship = 'many-to-many') %>%
  rowwise() %>%
  mutate(year = list(seq(Start, End))) %>%
  unnest(cols = year) %>%
  group_by(Circuit, year) %>%
  summarise(median_cf = median(dime.score,na.rm=TRUE)) %>%
  filter(year>1950) %>%
  mutate(year=as.character(year))



#-------------------------------------------------------------------
#Merge judge ideology with with FJC data
dime <- dime %>%
  left_join(fjc, by = c('fjc.judge.idno'='jid')) %>%
  select(name, dime.cfscore,imputed.dime.cfscore) %>%
  unique()

dime_updated <- dime_updated %>%
  left_join(fjc, by = c('fjc.judge.idno'='jid')) %>%
  select(name, dime.score) %>%
  unique()



#-------------------------------------------------------------------
#Match judges to cases and calculate median ideology
case_matches <- cofi %>%
  left_join(dime_updated, by = c('Judge 1'='name')) %>%
  left_join(dime_updated, by = c('Judge 2'='name')) %>%
  left_join(dime_updated, by = c('Judge 3'='name')) 



case_matches <- case_matches %>%
  left_join(medians, by = c('Circuit','Publication Year'='year')) %>%
  rowwise()%>%
  mutate(`Median Ideology`=as.numeric(`Median Ideology`),
         Issue=as.factor(Issue),
         year = as.numeric(`Publication Year`),
         imputed_median_cf =ifelse(`En Banc`==0,
                                   median(c(dime.score,dime.score.x,dime.score.y),na.rm=T),
                                   median_cf))%>%
  drop_na(imputed_median_cf) %>%
  select(Issue,Citation,Circuit,Liberal,Occurrence,`Median Ideology`, year,imputed_median_cf)


#-------------------------------------------------------------------
#Determine how many panels where no judge's ideology is imputed

coverage <- cofi %>%
  left_join(dime, by = c('Judge 1'='name')) %>%
  left_join(dime, by = c('Judge 2'='name')) %>%
  left_join(dime, by = c('Judge 3'='name')) 

coverage <- coverage %>%
  filter(`En Banc`==0) %>%
  rowwise()%>%
  mutate(`Median Ideology`=as.numeric(`Median Ideology`),
         Issue=as.factor(Issue),
         year = as.numeric(`Publication Year`),
    median_cf =median(c(dime.cfscore,dime.cfscore.x,dime.cfscore.y)),
         imputed_median_cf =median(c(imputed.dime.cfscore,imputed.dime.cfscore.x,imputed.dime.cfscore.y), na.rm=T))%>%
  select(Issue,Citation,Circuit,Liberal,Occurrence,`Median Ideology`,imputed_median_cf, year,median_cf)

1-sum(is.na(coverage$median_cf))/nrow(coverage) #0.077
  
#-------------------------------------------------------------------
#Construct dyads for analysis
dyads <- case_matches %>%
  left_join(case_matches, by = 'Issue', relationship = 'many-to-many' ) %>%
  filter(Occurrence.x>Occurrence.y) %>%
  mutate(ideological_distance = abs(`Median Ideology.x`-`Median Ideology.y`),
         ideological_distance.cf = abs(imputed_median_cf.x-imputed_median_cf.y),
         bias_compatible = ifelse((Liberal.y==1&`Median Ideology.x`>`Median Ideology.y`)|(Liberal.y==0&`Median Ideology.x`<`Median Ideology.y`),1,0),
         bias_compatible.cf = ifelse((Liberal.y==1&imputed_median_cf.x>imputed_median_cf.y)|(Liberal.y==0&imputed_median_cf.x<imputed_median_cf.y),1,0),
         disagreement = ifelse(Liberal.x!=Liberal.y,1,0),
         gap = year.x-year.y)


#-------------------------
#Table C4

m_comb1 <- lm_robust(disagreement ~ ideological_distance*bias_compatible, dyads, clusters=Issue)
m_comb2 <- lm_robust(disagreement ~ ideological_distance*bias_compatible+Issue,clusters=Issue, 
                     data=dyads)  
m_comb3 <- lm_robust(disagreement ~ ideological_distance.cf*bias_compatible.cf, dyads, clusters=Issue)
m_comb4 <- lm_robust(disagreement ~ ideological_distance.cf*bias_compatible.cf+Issue,clusters=Issue, 
                     data=dyads)  


c.4=texreg(list(m_comb1,m_comb2,m_comb3, m_comb4), stars=c(0.05),
       omit.coef = c("Issue"),
       reorder.coef=c(2,3,4,5,6,7,1),
       custom.coef.names=c("Constant","Ideological Distance", "Bias-compatible","Distance*Bias-compatible",
                           "Ideological Distance (CF)", "Bias-compatible (CF)","Distance*Bias-compatible (CF)"),
       include.ci=FALSE, include.rmse=FALSE, include.rsquared=F)

print(c.4, file = "Tables/c4.tex")

#--------------------------------------------------------
#Look at percentage of judges that have non-imputed time scores based on Bonica and Sen (2017)
matched_judges <- cofi %>%
  left_join(dime, by = c('Judge 1'='name')) %>%
  left_join(dime, by = c('Judge 2'='name')) %>%
  left_join(dime, by = c('Judge 3'='name'))

j1 <- select(matched_judges, `Judge 1`, dime.cfscore.x,imputed.dime.cfscore.x)
colnames(j1) <- c('Judge', 'CFScore', 'Imputed_CF')
j2 <- select(matched_judges, `Judge 2`, dime.cfscore.y,imputed.dime.cfscore.y)
colnames(j2) <- c('Judge', 'CFScore', 'Imputed_CF')
j3 <- select(matched_judges, `Judge 3`, dime.cfscore,imputed.dime.cfscore)
colnames(j3) <- c('Judge', 'CFScore', 'Imputed_CF')


matched_judges <- bind_rows(j1,j2,j3) %>%
  unique()

1-sum(is.na(matched_judges$CFScore))/nrow(matched_judges) #.3367





