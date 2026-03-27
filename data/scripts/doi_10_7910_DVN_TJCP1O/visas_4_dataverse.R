  require(dplyr)
require(lfe)
require(misty)

setsource()
thepath="visas4dataverse.csv"

dyadsorig<-read.csv(thepath)

########################
# Variables included
########################
#
# 'Country of visa issuance', #Name of destination country/visa issuing country
#  'Nationality of traveller', #ISO 3 Alpha code of destination
#  'name_o', #Name of origin country/traveler's country
#  'alpha3_o',  #ISO 3 Alpha code of origin
#  'year',  
#
# Variables from DEMIG VISA database: DEMIG. 2015. Demig Visa, version 1.4, Full Edition. Oxford: International Migration Institute, University of Oxford. www.migrationinstitute.org/data/demig-data/demig-visa-data
# 'policyval', 
# Policy measure (Visa Entry and Exit Permit)
# The categories entered are the following and apply both to ENTRY VISA AND EXIT PERMIT:
# 0 Visa/Exit permit NOT needed
# 1 Visa/Exit permit needed
# 2 Individuals are not allowed to travel to this country ("blacklisted")
#
# Variables from V-Dem: V-Dem [Country-Year/Country-Date] Dataset v14. Gothenburg: Varieties of Democracy (V-Dem) Project. https://doi.org/10.23696/mcwt-fr58.
# 'democracy_o', #regime type origin
# 'democracy_d', #regime type destination
# 'v2clstown_ord_d' #state economic ownership in destination country
# 'v2exl_legitideolcr_1_d', #state ideology = socialist in destination country
# 
# Variables from VDem brought up to date with UCDP Country Year Dataset on Organized Violence within Country Borders. See: Sundberg, Ralph, and Erik Melander, 2013, “Introducing the UCDP Georeferenced Event Dataset”, Journal of Peace Research, vol.50, no.4, 523-532
# 'e_miinteco_o', #International war at origin
#  'e_miinterc_o', #Civil war at origin
#  'e_miinteco_d', #International war at destination
#  'e_miinterc_d', #Civil war at destination
#
# WORLD BANK DATA : World Bank. 2024.World Development Indicators.Washington, DC: World Bank. https://data.worldbank.org/products/wdi.
# 'gdppc_imputed_imr_o', #GDP per capita origin 
# 'gdppc_imputed_imr_d', #GDP per capita destination
#
# Variable NSF-Kellogg Institute Data Base on Economic Integration Agreements: Bergstrand, Jeffrey H., and Scott L. Baier. 2021. NSF-Kellogg Institute Data Base on Economic Integration Agreements. South Bend, IN: Kellogg Institute for International Studies, University of Notre Dame. https://kellogg.nd.edu/nsf-kellogg-institute-data-base-economic-integration-agreements
#  'eia',
# 0 denotes no existing Economic Integration Agreement
# 1 denotes a One-Way Preferential Trade Agreement
# 2 denotes a Two-Way Preferential Trade Agreement
# 3 denotes a Free Trade Agreement
# 4 denotes a Customs Union
# 5 denotes a Common Market
# 6 denotes an Economic Union


dyads <- dyadsorig %>%
  mutate(
    visafree= if_else(policyval==0, 1, 0),
    democ2 = case_when(
      democracy_d == 1 & democracy_o == 1 ~ 1,
      democracy_d == 0 ~ 0,
      democracy_o == 0 ~ 0
    ),
    autoc2 = case_when(
      democracy_d == 0 & democracy_o == 0 ~ 1,
      democracy_d == 1 ~ 0,
      democracy_o == 1 ~ 0
    ),
    democracy_d_only = case_when(
      democracy_d == 1 & democracy_o == 0 ~ 1,
      democracy_d == 0 ~ 0,
      democracy_o == 1 ~ 0
    ),
    sb_d=if_else(e_miinteco_o==1,1,e_miinterc_o),
    sb_o=if_else(e_miinteco_d==1,1,e_miinterc_d),
    stateown_rev = (4 - v2clstown_ord_d) / 4,
    socialdems = case_when(
      stateown_rev > 0.5  ~ "CommandEconomy",
      stateown_rev <= 0.5 & v2exl_legitideolcr_1_d <= 0.5 ~ "MarketOther",
      stateown_rev <= 0.5 & v2exl_legitideolcr_1_d > 0.5  ~ "MarketSocialist"
    )
  )

summary(dyads$year)

#Estimates in Figure 6.3
wpol<-felm(visafree ~ log(relwealth) + log(relpoverty) + sb_d + sb_o + democ2 + autoc2 + democracy_d_only + eia + C(socialdems) | dyadid + year, data=dyads,keepX=TRUE)
summary(wpol)
wpol$N

#Additional model reported in appendix
withoutsocialdems<-felm(visafree ~ log(relwealth) + log(relpoverty) + sb_d + sb_o + democ2 + autoc2 + democracy_d_only + eia | dyadid + year, data=dyads,keepX=TRUE)
summary(withoutsocialdems)
withoutsocialdems$N