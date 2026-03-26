require(dplyr)
require(lfe)
require(misty)

setsource()
thepath="alll2_fordataverse.csv"

l2<-read.csv(thepath)

########################
# Variables included
########################
# 'iso3n': #ISO 3 numeric code of country
# 'Country', #Name of destination country
# 'year', #Census year
# 'mig_interval': census recall period

# Codes and variables from IPUMS: Minnesota Population Center. 2020. Integrated Public Use Microdata Series, International: Version 7.3 [dataset]. Minneapolis, MN: IPUMS. https://doi.org/10.18128/D020.V7.3.
# 'GEOLEV1': First level administrative unit
# 'GEO1NAME': First level administrative unit name
# 'GEOLEV2': 2nd level administrative unit
# 'GEO2NAME': 2nd level administrative unit name
# 'MIG_GEOLEV1': First level administrative unit of origin
# 'MIG_GEO1NAME': First level administrative unit of origin name
# 'MIG_GEOLEV2': 2nd level administrative unit of origin
# 'MIG_GEO2NAME': 2nd level administrative unit of origin name
# 'in_mig_all_sum': Average annual migration from MIG_GEOLEV2 to GEOLEV2
# 'dest_pop_sum': Population of GEOLEV2
# 'origin_pop_sum': Population of MIG_GEOLEV2
# 'elf':'Ethnolinguistic fractionalization index comparing MIG_GEOLEV2 to GEOLEV2. Missing data based on MIG_GEOLEV1 versus GEOLEV1 or country-level ELF
#
# Variables calculcated from GIS information
#### USGS. 2018. Global 30 Arc-Second Elevation (GTOPO30). Sioux Falls, SD: U.S. Geological Survey, EROS Data Center. https://www.usgs.gov/centers/eros/science/usgs-eros-archive-digital-elevation-global-30-arc-second-elevation-gtopo30
#### Zippenfenig, Patrick. 2023. Open-Meteo.com Weather API Version 0.2.89. Geneva: Zenodo, CERN. https://zenodo.org/records/14582479
# 'distance': distance from GEOLEV2 to MIG_GEOLEV2 in kilometers
# 'elev_change': difference between maximum and minimum elevation along straight path from GEOLEV2 to MIG_GEOLEV2 (meters)
#
#  Variables calculcated from IPUMs shape files and Dryad GDP data: Kummu, Matti; Taka, Maija; Guillaume, Joseph H. A. (2020). Data from: Gridded global datasets for Gross Domestic Product and Human Development Index over 1990-2015 [Dataset]. Dryad. https://doi.org/10.5061/dryad.dk1j0
# 'gdppc_dest', #GDP per capita of GEOLEV2
# 'gdppc_origin', #GDP per capita of MIG_GEOLEV2
# 'gdppc_relative', #gdppc_dest/gdppc_origin
#
# 'neoorcollective': Communal land (see discussion in main text)
# 
# 'democracy': V-Dem [Country-Year/Country-Date] Dataset v14. Gothenburg: Varieties of Democracy (V-Dem) Project. https://doi.org/10.23696/mcwt-fr58.

l2<-l2 %>%
  mutate(decade=factor(((year-mig_interval) %/% 10) * 10)) %>%
  mutate(l1border=ifelse(GEOLEV1==MIG_GEOLEV1,0,1)) %>%
  mutate(democborder=case_when(
    l1border==0 & democracy==0 ~ "NoborderAutocracy",
    l1border==1 & democracy==0 ~ "BorderAutocracy",
    l1border==0 & democracy==1 ~ "NoborderDemocracy",
    l1border==1 & democracy==1 ~ "BorderDemocracy"
           )) %>%
  mutate(myland= case_when(
    l1border==0 & democracy==0 & neoorcollective==1 ~ "NoborderGeneralAutocracy",
    l1border==1 & democracy==0 & neoorcollective==1 ~ "BorderGeneralAutocracy",
    l1border==0 & democracy==0 & neoorcollective==0 ~ "NoborderNoLandAutocracy",
    l1border==1 & democracy==0 & neoorcollective==0 ~ "BorderNoLandAutocracy",
    l1border==0 & democracy==1 & neoorcollective==1 ~ "NoborderGeneralDemocracy",
    l1border==1 & democracy==1 & neoorcollective==1 ~ "BorderGeneralDemocracy",
    l1border==0 & democracy==1 & neoorcollective==0 ~ "NoborderNoLandDemocracy",
    l1border==1 & democracy==1 & neoorcollective==0 ~ "BorderNoLandDemocracy"
  ))
  
#Regressions shown in Figure 5.4 and Table A.11
allcasesl2<-felm(log(in_mig_all_rate+1)~log(distance+1) + log(elev_change+1) +  log(origin_pop_sum)+log(dest_pop_sum) + l1border + democracy + log(gdppc_origin) + log(gdppc_dest) +log(gdppc_relative+1) + elf | GEOLEV2 + MIG_GEOLEV2 + decade, data=l2,keepX=TRUE)
summary(allcasesl2)

democ<-felm(log(in_mig_all_rate+1)~log(distance+1) + log(elev_change+1) +  log(origin_pop_sum)+log(dest_pop_sum) + l1border*democracy + log(gdppc_origin) + log(gdppc_dest) +log(gdppc_relative+1) + elf | GEOLEV2 + MIG_GEOLEV2 + decade, data=l2,keepX=TRUE)
summary(democ)

landreg<-felm(log(in_mig_all_rate+1)~log(distance+1) + log(elev_change+1) +  log(origin_pop_sum)+log(dest_pop_sum) + myland + elf + log(gdppc_origin) + log(gdppc_dest) +log(gdppc_relative+1) | decade, data=l2,keepX=TRUE)
summary(landreg)
