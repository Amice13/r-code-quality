require(dplyr)
require(lfe)
require(misty)

setsource()
thepath="alll1_fordataverse.csv"

l1orig<-read.csv(thepath)

########################
# Variables included
########################

# Codes and variables from IPUMS: Minnesota Population Center. 2020. Integrated Public Use Microdata Series, International: Version 7.3 [dataset]. Minneapolis, MN: IPUMS. https://doi.org/10.18128/D020.V7.3.
# 'GEOLEV0': #ISO 3 numeric code of country of destination
# 'GEO0NAME', #Name of destination country
# 'GEOLEV1': First level administrative unit
# 'GEO1NAME': First level administrative unit name
# 'MIG_GEOLEV0': Iso3n code country of origin
# 'MIG_GEO0NAME': Country of origin name
# 'MIG_GEOLEV0_aslist': List of ISO3n codes for MIG_GEOLEV0 codes that correspond to multiple countries (e.g., the EU)
# 'YEAR', #Year of census  
# 'mig_y0': 1st year of census recall period
# 'dyad_inmig_all_sum': Total migration from MIG_GEOLEV1 to GEOLEV1
# 'dyad_inmig_rate': Average annual migration from MIG_GEOLEV1 to GEOLEV1
# 'dest_pop_sum': Population of GEOLEV1
# 'origin_pop_sum': Population of MIG_GEOLEV1
# 'elf':'Ethnolinguistic fractionalization index comparing MIG_GEOLEV0 to GEOLEV0.
#
#  Variables calculcated from GIS information:
#### USGS. 2018. Global 30 Arc-Second Elevation (GTOPO30). Sioux Falls, SD: U.S. Geological Survey, EROS Data Center. https://www.usgs.gov/centers/eros/science/usgs-eros-archive-digital-elevation-global-30-arc-second-elevation-gtopo30
#### Zippenfenig, Patrick. 2023. Open-Meteo.com Weather API Version 0.2.89. Geneva: Zenodo, CERN. https://zenodo.org/records/14582479
# 'distance': distance from GEOLEV1 to MIG_GEOLEV1 in kilometers
# 'elev_change': difference between maximum and minimum elevation along straight path from GEOLEV1 to MIG_GEOLEV1 (meters)
#
#  Variables calculcated from IPUMs shape files and Dryad GDP data: Kummu, Matti; Taka, Maija; Guillaume, Joseph H. A. (2020). Data from: Gridded global datasets for Gross Domestic Product and Human Development Index over 1990-2015 [Dataset]. Dryad. https://doi.org/10.5061/dryad.dk1j0
#  'gdppc_dest', #GDP per capita of GEOLEV1
#  'gdppc_origin', #GDP per capita of MIG_GEOLEV1
#  'gdppc_relative', #gdppc_dest/gdppc_origin
#
# 'eia': Economic integration area treaty from NSF-Kellogg Institute Data Base on Economic Integration Agreements: Bergstrand, Jeffrey H., and Scott L. Baier. 2021. NSF-Kellogg Institute Data Base on Economic Integration Agreements. South Bend, IN: Kellogg Institute for International Studies, University of Notre Dame. https://kellogg.nd.edu/nsf-kellogg-institute-data-base-economic-integration-agreements
# 'overseas': 0/1 variable indicating migration between metropole and overseas territorial possessions or between territories of same metropole (coded by the author)
# Democracy variables 
# 'democracy_origin: Democracy for MIG_GEOLEV0 From V-Dem project: V-Dem [Country-Year/Country-Date] Dataset v14. Gothenburg: Varieties of Democracy (V-Dem) Project. https://doi.org/10.23696/mcwt-fr58.
# 'democracy_dest': Democracy for GEOLEV0 From V-Dem project


l1<-l1orig %>%
  mutate(aux=factor(((mig_y0) %/% 10) * 10)) %>%
  mutate(intl=ifelse(GEOLEV0==MIG_GEOLEV0,0,1)) %>%
  mutate(intl=ifelse(overseas==1,0,intl)) %>%
  mutate(eianew=case_when(
    eia==7 ~ "Domestic",
    eia==0 ~ "No treaty",
    eia>0 & eia<5 ~ "Free trade",
    eia==5 ~ "Common Market",
    eia==6 ~ "Common Market",
    eia==8 ~ "Overseas",
  )) %>% 
  mutate(eianew=factor(eianew,levels=c("Domestic","Common Market","Free trade","Overseas","No treaty"))) %>%
  mutate(
      democ2 = case_when(
      democracy_dest == 1 & democracy_origin == 1 ~ 1,
      democracy_dest == 0 ~ 0,
      democracy_origin == 0 ~ 0
    ),
    autoc2 = case_when(
      democracy_dest == 0 & democracy_origin == 0 ~ 1,
      democracy_dest == 1 ~ 0,
      democracy_origin == 1 ~ 0
    ),
    democracy_dest_only = case_when(
      democracy_dest == 1 & democracy_origin == 0 ~ 1,
      democracy_dest == 0 ~ 0,
      democracy_origin == 1 ~ 0
    )) %>%
  #Code a convenience variable in order to make Table 5.2
  mutate(specialcountries=paste0(GEO0NAME,as.character(YEAR),sep="")) %>%
  mutate(specialcountries=as.factor(ifelse(GEOLEV0 %in% c(250,840,276,643,826,724),specialcountries,'other'))) %>%
  mutate(specialcountries=relevel(specialcountries,ref='other'))

#Table A.10, 1st model
allcases<-felm(log(dyad_inmig_rate+1) ~ log(distance+1) + log(elev_change+1) +  log(origin_pop_sum)+log(dest_pop_sum) + intl + log(gdppc_origin) + log(gdppc_dest) +log(gdppc_relative+1) + elf | GEOLEV1 + MIG_GEOLEV0 + decade, data=l1,keepX=TRUE,na.action=na.omit)
summary(allcases)
allcases$N

#Figure 5.2, leftmost estimate
#Example of how to calculate estimated migration rates across international borders as a percentage of domestic rates using regression results
-(1-exp(allcases$coefficients["intl",]))*100

#Table A.10, 2nd model; Figure 5.2
bydecade<-felm(log(dyad_inmig_rate+1)~log(distance+1) + log(elev_change+1) +  log(origin_pop_sum)+log(dest_pop_sum) + intl:decade + log(gdppc_origin) + log(gdppc_dest) +log(gdppc_relative+1) + elf + decade | GEOLEV1 + MIG_GEOLEV0, data=l1,keepX=TRUE)
summary(bydecade)
bydecade$N

#Table A.10, 3rd model; Figure 5.3
byeia<-felm(log(dyad_inmig_all_sum+1)~log(distance+1) + log(elev_change+1) +  log(origin_pop_sum)+log(dest_pop_sum) + eianew + elf + log(gdppc_origin) + log(gdppc_dest) +log(gdppc_relative+1) | GEOLEV1 + MIG_GEOLEV0 + decade, data=l1,keepX=TRUE)
summary(byeia)
byeia$N

#Footnote 51, Chapter 5: International interacted with regime types and relative GDP
wregime<-felm(log(dyad_inmig_rate+1) ~ log(distance+1) + log(elev_change+1) +  log(origin_pop_sum)+log(dest_pop_sum) + intl + intl:democ2 + intl:autoc2 + intl:democracy_dest_only + log(gdppc_origin) + log(gdppc_dest) +log(gdppc_relative+1) + elf | GEOLEV1 + MIG_GEOLEV0 + decade, data=l1,keepX=TRUE,na.action=na.omit)
summary(wregime)

wrelgdp<-felm(log(dyad_inmig_rate+1) ~ log(distance+1) + log(elev_change+1) +  log(origin_pop_sum)+log(dest_pop_sum) + intl + intl:log(gdppc_relative+1) + log(gdppc_origin) + log(gdppc_dest) +log(gdppc_relative+1) + elf | GEOLEV1 + MIG_GEOLEV0 + decade, data=l1,keepX=TRUE,na.action=na.omit)
summary(wrelgdp)

#Table 5.2
#Country specific results
cntryspecific<-felm(log(dyad_inmig_rate+1) ~ log(distance+1) + log(elev_change+1) +  log(origin_pop_sum)+log(dest_pop_sum) + intl + intl:specialcountries + log(gdppc_origin) + log(gdppc_dest) +log(gdppc_relative+1) + elf | specialcountries + MIG_GEOLEV0, data=l1,keepX=TRUE,na.action=na.omit)
summary(cntryspecific)

