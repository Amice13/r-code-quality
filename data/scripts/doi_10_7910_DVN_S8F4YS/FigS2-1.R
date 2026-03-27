
rm(list=ls())

library(tidyverse)

# THIS SCRIPT OUTPUTS THE COMPARISON OF THE DISTRIBUTION OF PARTISANSHIP IN THE LINKED SAMPLES
# WITH THE DISTRIBUTION OF PARTISANSHIP IN THE LINK ELIGIBLE (i.e. ALIVE IN 1940) VOTING POPULATION IN
# THE STATES TO WHICH WE LINKED THE CENSUS DATA. THIS SCRIPT OUTPUTS FIGURE S2.

## LOAD VOTERFILE DATA AND FORMAT FOR PLOTTING
df = read_csv('02-data/full-comp-ca17.csv')%>%
  mutate(party = ifelse(Parties_Description == 'Democratic', 'Democrat',
                        ifelse(Parties_Description == 'Republican', 'Republican', 'Other')),
         vfile = 'CA17',
         vfile.year = '2017')%>%
  select(party, vfile,vfile.year)%>%
  bind_rows( read_csv('02-data/full-comp-nc17.csv')%>%
      mutate(party = ifelse(Parties_Description == 'Democratic', 'Democrat',
                            ifelse(Parties_Description == 'Republican', 'Republican', 'Other')),
             vfile = 'NC17',
             vfile.year = '2017')%>%
      select(party, vfile,vfile.year)
    )%>%
  bind_rows( read_csv('02-data/full-comp-ne17.csv')%>%
               mutate(party = ifelse(Parties_Description == 'Democratic', 'Democrat',
                                     ifelse(Parties_Description == 'Republican', 'Republican', 'Other')),
                      vfile = 'NE17',
                      vfile.year = '2017')%>%
               select(party, vfile,vfile.year)
  )%>%
  bind_rows(
    read_csv('02-data/full-comp-ca05.csv')%>%
      mutate(party = ifelse(party == 'DEM', 'Democrat',
                            ifelse(party == 'REP', 'Republican', 'Other')),
             vfile = 'CA05',
             vfile.year = '2005/2009')%>%
      select(party, vfile, vfile.year)
    
  ) %>%
  bind_rows(
    read_csv('02-data/full-comp-nc09.csv')%>%
      mutate(party = ifelse(party_cd == 'DEM', 'Democrat',
                            ifelse(party_cd == 'REP', 'Republican', 'Other')),
             vfile = 'NC09',
             vfile.year = '2005/2009')%>%
      select(party, vfile,vfile.year)
    
  )

data = df %>%
  mutate(sample = 'General')


## LOAD 2005/2009 DATA AND FORMAT FOR PLOTTING

load("02-data/befm-main-analysis.Rdata")
df0509 = df %>%
  as_tibble%>%
  mutate(party = ifelse(party == 'DEM', 'Democrat',
         ifelse(party == 'REP', 'Republican', 'Other')),
         vfile = state.vfile,
         sample = 'Linked',
         vfile.year = '2005/2009') %>%
  select(party, vfile, sample, vfile.year)
rm(df)


## LOAD 2017 DATA AND FORMAT FOR PLOTTING

load("02-data/seg_analysis_2017.Rdata")

df17 = df %>%
  as_tibble%>%
  mutate(party = ifelse(democrat==1, 'Democrat',
                        ifelse(republican == 1, 'Republican', 'Other')),
         sample = 'Linked',
         vfile.year = '2017') %>%
  select(party,  sample,vfile.year)
rm(df)


data = df17 %>%
  bind_rows(df0509)%>%
  select(party,sample,vfile.year)%>%
  bind_rows(data)


## LOAD AESTHETIC FUNCTIONS
source("01-code/r_utils.R")
require(scales)


p=data %>%
  mutate(party = ifelse(is.na(party), 'Other', party),
         denom1 = sum(sample == 'General' & vfile.year=='2005/2009'),
         denom2 = sum(sample == 'Linked' & vfile.year=='2005/2009'),
         denom3 = sum(sample == 'General' & vfile.year=='2017'),
         denom4 = sum(sample == 'Linked' & vfile.year=='2017'),
         denom = ifelse(sample=='General' & vfile.year=='2005/2009', denom1,
                        ifelse(sample=='Linked' & vfile.year=='2005/2009', denom2,
                               ifelse(sample=='General' & vfile.year=='2017', denom3,denom4))),
         sample = ifelse(sample=='General', 'Link Eligible', sample),
         party = factor(party, levels = c('Republican', 'Other', 'Democrat')))%>%
  group_by(party, sample, vfile.year)%>%
  dplyr::summarize(p = n()/denom[1])%>%
  ggplot(aes(x=party,y=p,fill = party))+
  geom_col()+
  xlab('')+
  ylab('Proportion')+
  theme_shom()+
  scale_x_discrete(limits = rev(c('Republican', 'Other', 'Democrat')))+
  facet_wrap(vfile.year~sample,
             scales = 'free_x')
  

### SAVE PLOT
ggsave(filename = "03-output/01-plots/FigS2.jpeg", plot = p, width = 9, height = 5, units = 'in', dpi=600)

