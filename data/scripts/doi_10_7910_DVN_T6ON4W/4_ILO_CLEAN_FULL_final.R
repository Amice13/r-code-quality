library(readr)
library(dplyr)
library(tidyr)
library(openxlsx)
library(readxl)
# set working directory
setwd("~/Desktop/Ch/ASTAR-sj/")
# Import raw ILO by sex by econ activity employment raw data to R
ILOfulldata_bysex <- read_csv("_data/raw_data/ILOfulldata_bysex_030124.csv")
# change value names
ILO_Full <- subset(ILOfulldata_bysex, select = c(ref_area, ref_area.label, sex.label, classif1.label, time, obs_value))
names(ILO_Full) <- c("code","country","sex","activity","year","value")

# Change name of primary activities
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 01 - Agriculture, hunting and related service activities" = "R3.1-01"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 02 - Forestry, logging and related service activities" = "R3.1-02"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 05 - Fishing, aquaculture and service activities incidental to fishing" = "R3.1-05"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 15 - Manufacture of food products and beverages" = "R3.1-15"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 16 - Manufacture of tobacco products" = "R3.1-16"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 51 - Wholesale trade and commission trade, except of motor vehicles and motorcycles" = "R3.1-51"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 52 - Retail trade, except of motor vehicles and motorcycles; repair of personal and household goods" = "R3.1-52"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 55 - Hotels and restaurants" = "R3.1-55"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 60 - Land transport; transport via pipelines" = "R3.1-60"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 62 - Air transport" = "R3.1-62"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 63 - Supporting and auxiliary transport activities; activities of travel agencies" = "R3.1-63"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 61 - Water transport" = "R3.1-61"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 01 - Crop and animal production, hunting and related service activities" = "R4-01"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 03 - Fishing and aquaculture" = "R4-03"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 10 - Manufacture of food products" = "R4-10"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 11 - Manufacture of beverages" = "R4-11"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 12 - Manufacture of tobacco products" = "R4-12"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 46 - Wholesale trade, except of motor vehicles and motorcycles" = "R4-46"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 47 - Retail trade, except of motor vehicles and motorcycles" = "R4-47"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 49 - Land transport and transport via pipelines" = "R4-49"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 52 - Warehousing and support activities for transportation" = "R4-52"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 56 - Food and beverage service activities" = "R4-56"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 50 - Water transport" = "R4-50"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 55 - Accommodation" = "R4-55"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 02 - Forestry and logging" = "R4-02"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 51 - Air transport" = "R4-51"))

# Change name of indirect activities
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 10 - Mining of coal and lignite; extraction of peat" = "R3.1-10"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 11 - Extraction of crude petroleum and natural gas; service activities incidental to oil and gas extraction, excluding surveying" = "R3.1-11"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 12 - Mining of uranium and thorium ores" = "R3.1-12"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 13 - Mining of metal ores" = "R3.1-13"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 14 - Other mining and quarrying" = "R3.1-14"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 17 - Manufacture of textiles" = "R3.1-17"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 18 - Manufacture of wearing apparel; dressing and dyeing of fur" = "R3.1-18"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 19 - Tanning and dressing of leather; manufacture of luggage, handbags, saddlery, harness and footwear" = "R3.1-19"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 20 - Manufacture of wood and of products of wood and cork, except furniture; manufacture of articles of straw and plaiting materials" = "R3.1-20"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 21 - Manufacture of paper and paper products" = "R3.1-21"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 22 - Publishing, printing and reproduction of recorded media" = "R3.1-22"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 23 - Manufacture of coke, refined petroleum products and nuclear fuel" = "R3.1-23"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 24 - Manufacture of chemicals and chemical products" = "R3.1-24"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 25 - Manufacture of rubber and plastics products" = "R3.1-25"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 26 - Manufacture of other non-metallic mineral products" = "R3.1-26"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 27 - Manufacture of basic metals" = "R3.1-27"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 28 - Manufacture of fabricated metal products, except machinery and equipment" = "R3.1-28"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 29 - Manufacture of machinery and equipment n.e.c." = "R3.1-29"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 30 - Manufacture of office, accounting and computing machinery" = "R3.1-30"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 31 - Manufacture of electrical machinery and apparatus n.e.c." = "R3.1-31"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 32 - Manufacture of radio, television and communication equipment and apparatus" = "R3.1-32"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 33 - Manufacture of medical, precision and optical instruments, watches and clocks" = "R3.1-33"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 34 - Manufacture of motor vehicles, trailers and semi-trailers" = "R3.1-34"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 35 - Manufacture of other transport equipment" = "R3.1-35"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 36 - Manufacture of furniture; manufacturing n.e.c." = "R3.1-36"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 37 - Recycling" = "R3.1-37"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 40 - Electricity, gas, steam and hot water supply" = "R3.1-40"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 41 - Collection, purification and distribution of water" = "R3.1-41"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 45 - Construction" = "R3.1-45"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 50 - Sale, maintenance and repair of motor vehicles and motorcycles; retail sale of automotive fuel" = "R3.1-50"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 64 - Post and telecommunications" = "R3.1-64"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 65 - Financial intermediation, except insurance and pension funding" = "R3.1-65"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 66 - Insurance and pension funding, except compulsory social security" = "R3.1-66"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 67 - Activities auxiliary to financial intermediation" = "R3.1-67"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 70 - Real estate activities" = "R3.1-70"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 71 - Renting of machinery and equipment without operator and of personal and household goods" = "R3.1-71"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 72 - Computer and related activities" = "R3.1-72"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 73 - Research and development" = "R3.1-73"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 74 - Other business activities" = "R3.1-74"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 75 - Public administration and defence; compulsory social security" = "R3.1-75"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 80 - Education" = "R3.1-80"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 85 - Health and social work" = "R3.1-85"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 90 - Sewage and refuse disposal, sanitation and similar activities" = "R3.1-90"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 91 - Activities of membership organizations n.e.c." = "R3.1-91"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 92 - Recreational, cultural and sporting activities" = "R3.1-92"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 93 - Other service activities" = "R3.1-93"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 95 - Activities of private households as employers of domestic staff" = "R3.1-95"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 96 - Undifferentiated goods-producing activities of private households for own use" = "R3.1-96"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 97 - Undifferentiated service-producing activities of private households for own use" = "R3.1-97"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 99 - Extraterritorial organizations and bodies" = "R3.1-99"))

# for Rev 4
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 05 - Mining of coal and lignite" = "R4-05"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 06 - Extraction of crude petroleum and natural gas" = "R4-06"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 07 - Mining of metal ores" = "R4-07"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 08 - Other mining and quarrying" = "R4-08"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 09 - Mining support service activities" = "R4-09"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 13 - Manufacture of textiles" = "R4-13"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 14 - Manufacture of wearing apparel" = "R4-14"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 15 - Manufacture of leather and related products" = "R4-15"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 16 - Manufacture of wood and of products of wood and cork, except furniture; manufacture of articles of straw and plaiting materials" = "R4-16"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 17 - Manufacture of paper and paper products" = "R4-17"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 18 - Printing and reproduction of recorded media" = "R4-18"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 19 - Manufacture of coke and refined petroleum products" = "R4-19"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 20 - Manufacture of chemicals and chemical products" = "R4-20"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 21 - Manufacture of pharmaceuticals, medicinal chemical and botanical products" = "R4-21"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 22 - Manufacture of rubber and plastics products" = "R4-22"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 23 - Manufacture of other non-metallic mineral products" = "R4-23"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 24 - Manufacture of basic metals" = "R4-24"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 25 - Manufacture of fabricated metal products, except machinery and equipment" = "R4-25"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 26 - Manufacture of computer, electronic and optical products" = "R4-26"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 27 - Manufacture of electrical equipment" = "R4-27"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 28 - Manufacture of machinery and equipment n.e.c." = "R4-28"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 29 - Manufacture of motor vehicles, trailers and semi-trailers" = "R4-29"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 30 - Manufacture of other transport equipment" = "R4-30"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 31 - Manufacture of furniture" = "R4-31"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 32 - Other manufacturing" = "R4-32"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 33 - Repair and installation of machinery and equipment" = "R4-33"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 35 - Electricity, gas, steam and air conditioning supply" = "R4-35"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 36 - Water collection, treatment and supply" = "R4-36"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 37 - Sewerage" = "R4-37"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 38 - Waste collection, treatment and disposal activities; materials recovery" = "R4-38"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 39 - Remediation activities and other waste management services" = "R4-39"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 41 - Construction of buildings" = "R4-41"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 42 - Civil engineering" = "R4-42"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 43 - Specialized construction activities" = "R4-43"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 45 - Wholesale and retail trade and repair of motor vehicles and motorcycles" = "R4-45"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 53 - Postal and courier activities" = "R4-53"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 58 - Publishing activities" = "R4-58"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 59 - Motion picture, video and television programme production, sound recording and music publishing activities" = "R4-59"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 60 - Programming and broadcasting activities" = "R4-60"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 61 - Telecommunications" = "R4-61"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 62 - Computer programming, consultancy and related activities" = "R4-62"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 63 - Information service activities" = "R4-63"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 64 - Financial service activities, except insurance and pension funding" = "R4-64"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 65 - Insurance, reinsurance and pension funding, except compulsory social security" = "R4-65"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 66 - Activities auxiliary to financial service and insurance activities" = "R4-66"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 68 - Real estate activities" = "R4-68"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 69 - Legal and accounting activities" = "R4-69"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 70 - Activities of head offices; management consultancy activities" = "R4-70"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 71 - Architectural and engineering activities; technical testing and analysis" = "R4-71"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 72 - Scientific research and development" = "R4-72"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 73 - Advertising and market research" = "R4-73"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 74 - Other professional, scientific and technical activities" = "R4-74"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 75 - Veterinary activities" = "R4-75"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 77 - Rental and leasing activities" = "R4-77"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 78 - Employment activities" = "R4-78"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 79 - Travel agency, tour operator, reservation service and related activities" = "R4-79"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 80 - Security and investigation activities" = "R4-80"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 81 - Services to buildings and landscape activities" = "R4-81"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 82 - Office administrative, office support and other business support activities" = "R4-82"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 84 - Public administration and defence; compulsory social security" = "R4-84"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 85 - Education" = "R4-85"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 86 - Human health activities" = "R4-86"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 87 - Residential care activities" = "R4-87"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 88 - Social work activities without accommodation" = "R4-88"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 90 - Creative, arts and entertainment activities" = "R4-90"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 91 - Libraries, archives, museums and other cultural activities" = "R4-91"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 92 - Gambling and betting activities" = "R4-92"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 93 - Sports activities and amusement and recreation activities" = "R4-93"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 94 - Activities of membership organizations" = "R4-94"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 95 - Repair of computers and personal and household goods" = "R4-95"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 96 - Other personal service activities" = "R4-96"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 97 - Activities of households as employers of domestic personnel" = "R4-97"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 98 - Undifferentiated goods- and services-producing activities of private households for own use" = "R4-98"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 99 - Activities of extraterritorial organizations and bodies" = "R4-99"))
# keep the total level for regression use
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: Total" = "R3.1-total"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: Total" = "R4-total"))

# keep gender level as total only
ILO_full <- subset(ILO_Full, sex == "Sex: Total") 
unique(ILO_full$country) # 154 countries

# Aggregate activities into Eora26 sectors
aggregate_ILO <- ILO_full %>%
  replace_na(list(value = 0)) %>%
  group_by(country, code, sex, activity, year) %>%
  mutate(
    activity = ifelse(activity %in% c("R3.1-01", "R3.1-02", "R4-01", "R4-02","R3.1-05","R4-03"), "A01T02", as.character(activity)),
    activity = ifelse(activity %in% c("R3.1-15", "R3.1-16", "R4-10", "R4-11", "R4-12"), "A04", as.character(activity)),
    activity = ifelse(activity %in% c("R3.1-55", "R4-55", "R4-56"), "A18", as.character(activity)),
    activity = ifelse(activity %in% c("R3.1-60", "R3.1-61", "R3.1-62", "R3.1-63", "R4-49", "R4-50", "R4-51", "R4-52"), "A19", as.character(activity)),
    activity = ifelse(activity %in% c("R3.1-51", "R4-46"), "A16", as.character(activity)),
    activity = ifelse(activity %in% c("R3.1-52", "R4-47"), "A17", as.character(activity)),
    activity = ifelse(activity %in% c("R3.1-10","R3.1-11","R3.1-12","R3.1-13","R3.1-14","R4-05","R4-06","R4-07","R4-08","R4-09"), "A03", as.character(activity)),
    activity = ifelse(activity %in% c("R3.1-17","R3.1-18","R3.1-19","R4-13","R4-14","R4-15"), "A05", as.character(activity)),
    activity = ifelse(activity %in% c("R3.1-20","R3.1-21","R3.1-22","R4-16","R4-17","R4-18"), "A06", as.character(activity)),
    activity = ifelse(activity %in% c("R3.1-23","R3.1-24","R3.1-25","R3.1-26","R4-19","R4-20","R4-21","R4-22","R4-23"), "A07", as.character(activity)),
    activity = ifelse(activity %in% c("R3.1-27", "R3.1-28", "R4-24", "R4-25"), "A08", as.character(activity)),
    activity = ifelse(activity %in% c("R3.1-29","R3.1-30","R3.1-31","R3.1-32","R3.1-33","R4-26","R4-27","R4-28"), "A09", as.character(activity)),
    activity = ifelse(activity %in% c("R3.1-34", "R3.1-35", "R4-29", "R4-30"), "A10", as.character(activity)),
    activity = ifelse(activity %in% c("R3.1-36", "R4-31", "R4-32", "R4-33"), "A11", as.character(activity)),
    activity = ifelse(activity %in% c("R3.1-37", "R4-38", "R4-39"), "A12", as.character(activity)),
    activity = ifelse(activity %in% c("R3.1-40", "R3.1-41", "R4-35", "R4-36", "R4-37"), "A13", as.character(activity)),
    activity = ifelse(activity %in% c("R3.1-45", "R4-41", "R4-42", "R4-43"), "A14", as.character(activity)),
    activity = ifelse(activity %in% c("R3.1-50", "R4-45"), "A15", as.character(activity)),
    activity = ifelse(activity %in% c("R3.1-64", "R4-53", "R4-58", "R4-59", "R4-60", "R4-61", "R4-62", "R4-63"), "A20", as.character(activity)),
    activity = ifelse(activity %in% c("R3.1-65","R3.1-66","R3.1-67","R3.1-70","R3.1-71","R3.1-72","R3.1-73","R3.1-74", "R4-64", "R4-65", "R4-66", "R4-68", "R4-69", "R4-70", "R4-77", "R4-78", "R4-79"), "A21", as.character(activity)),
    activity = ifelse(activity %in% c("R3.1-75", "R4-82", "R4-84"), "A22", as.character(activity)),
    activity = ifelse(activity %in% c("R3.1-80","R3.1-85","R3.1-90","R3.1-91","R3.1-92","R3.1-93", "R4-71","R4-72","R4-73","R4-74","R4-75","R4-80", "R4-81", "R4-85", "R4-86", "R4-87", "R4-88", "R4-90", "R4-91", "R4-92", "R4-93", "R4-94", "R4-95", "R4-96"), "A23", as.character(activity)),
    activity = ifelse(activity %in% c("R3.1-95", "R3.1-96","R3.1-97","R4-97", "R4-98"), "A24", as.character(activity)),
    activity = ifelse(activity %in% c("R3.1-99", "R4-99"), "A25", as.character(activity)),
    activity = ifelse(activity %in% c("Economic activity (ISIC-Rev.3.1), 2 digit level: Not elsewhere classified", "Economic activity (ISIC-Rev.4), 2 digit level: Not elsewhere classified"), "A26", as.character(activity)),
    activity = ifelse(activity %in% c("R3.1-total", "R4-total"), "Total", as.character(activity)),
  ) %>%
  group_by(country, code, sex, activity, year) %>%
  summarize(value = sum(value)) %>%
  replace_na(list(value = 0))

# Clean the data columns
aggregate_ILO$code <- paste(aggregate_ILO$code, aggregate_ILO$year, sep = "-") # combine ctry code and activity index into one
aggregate_ILO <- select(aggregate_ILO, -sex, -country) %>%
  ungroup()
# Separate data into direct and indirect activities
direct_activity <- c("A01T02","A04","A16","A17","A18","A19")
ILO_direct <- subset(aggregate_ILO, activity %in% direct_activity)
ILO_indrect <- subset(aggregate_ILO,!activity %in% direct_activity)

# keep country-year pairs that have all 6 direct activities
ILO_direct_v <- ILO_direct %>% 
  filter(value != 0)  # drop all rows with value==0
ILO_direct_v <- ILO_direct_v %>%
  group_by(code) %>%
  filter(n_distinct(activity) == length(direct_activity) & all(direct_activity %in% activity)) %>%
  ungroup()
unique(ILO_direct_v$code) # 1640 country-year pairs

full_ILO <- rbind(ILO_direct_v, ILO_indrect) # combine direct and indirect data frames
full_ILO <- subset(full_ILO, code %in% ILO_direct_v$code) # keep only country_year rows that have all valid 6 direct activities data, and drop all others, based on 'index'
full_ILO <- select(full_ILO, -year)
unique(full_ILO$code) # validate 1640 pairs left
full_ILO_exp <- separate(full_ILO, code, into = c("code", "year"), sep = "-")
unique(full_ILO_exp$code) # 147 countries
unique(full_ILO_exp$year) # 1991-2021
# Export the final labor data: 1640 country-year pairs, covering 147 countries from 1991 to 2021.
write.xlsx(full_ILO_exp, file = "_data/processed_data/ILO_labor.xlsx", sheetName = "ILO_FULL")


# ------------------------------------------------- #
#     Part II: disaggregate ILO data by gender
# ------------------------------------------------- #

# Import raw ILO by sex by econ activity employment raw data
ILOfulldata_bysex <- read_csv("_data/raw_data/ILOfulldata_bysex_030124.csv")
ILOfulldata_bysex_summary <- ILOfulldata_bysex[, c("ref_area", "source.label","sex.label", "classif1.label", "time", "obs_value")]
ILOfulldata_bysex <- ILOfulldata_bysex[, c("ref_area", "sex.label", "classif1.label", "time", "obs_value")]
names(ILOfulldata_bysex) <- c("country","sex","activity","year","value")
# Rename Econ Activity code
ILO_Full <- ILOfulldata_bysex %>%
  mutate(activity = recode(activity,
                           "Economic activity (ISIC-Rev.3.1), 2 digit level: 01 - Agriculture, hunting and related service activities" = "R3.1-01",
                           "Economic activity (ISIC-Rev.3.1), 2 digit level: 02 - Forestry, logging and related service activities" = "R3.1-02",
                           "Economic activity (ISIC-Rev.3.1), 2 digit level: 05 - Fishing, aquaculture and service activities incidental to fishing" = "R3.1-05",
                           "Economic activity (ISIC-Rev.3.1), 2 digit level: 15 - Manufacture of food products and beverages" = "R3.1-15",
                           "Economic activity (ISIC-Rev.3.1), 2 digit level: 16 - Manufacture of tobacco products" = "R3.1-16",
                           "Economic activity (ISIC-Rev.3.1), 2 digit level: 51 - Wholesale trade and commission trade, except of motor vehicles and motorcycles" = "R3.1-51",
                           "Economic activity (ISIC-Rev.3.1), 2 digit level: 52 - Retail trade, except of motor vehicles and motorcycles; repair of personal and household goods" = "R3.1-52",
                           "Economic activity (ISIC-Rev.3.1), 2 digit level: 55 - Hotels and restaurants" = "R3.1-55",
                           "Economic activity (ISIC-Rev.3.1), 2 digit level: 60 - Land transport; transport via pipelines" = "R3.1-60",
                           "Economic activity (ISIC-Rev.3.1), 2 digit level: 62 - Air transport" = "R3.1-62",
                           "Economic activity (ISIC-Rev.3.1), 2 digit level: 63 - Supporting and auxiliary transport activities; activities of travel agencies" = "R3.1-63",
                           "Economic activity (ISIC-Rev.3.1), 2 digit level: 61 - Water transport" = "R3.1-61",
                           "Economic activity (ISIC-Rev.4), 2 digit level: 01 - Crop and animal production, hunting and related service activities" = "R4-01",
                           "Economic activity (ISIC-Rev.4), 2 digit level: 03 - Fishing and aquaculture" = "R4-03",
                           "Economic activity (ISIC-Rev.4), 2 digit level: 10 - Manufacture of food products" = "R4-10",
                           "Economic activity (ISIC-Rev.4), 2 digit level: 11 - Manufacture of beverages" = "R4-11",
                           "Economic activity (ISIC-Rev.4), 2 digit level: 12 - Manufacture of tobacco products" = "R4-12",
                           "Economic activity (ISIC-Rev.4), 2 digit level: 46 - Wholesale trade, except of motor vehicles and motorcycles" = "R4-46",
                           "Economic activity (ISIC-Rev.4), 2 digit level: 47 - Retail trade, except of motor vehicles and motorcycles" = "R4-47",
                           "Economic activity (ISIC-Rev.4), 2 digit level: 49 - Land transport and transport via pipelines" = "R4-49",
                           "Economic activity (ISIC-Rev.4), 2 digit level: 52 - Warehousing and support activities for transportation" = "R4-52",
                           "Economic activity (ISIC-Rev.4), 2 digit level: 56 - Food and beverage service activities" = "R4-56",
                           "Economic activity (ISIC-Rev.4), 2 digit level: 50 - Water transport" = "R4-50",
                           "Economic activity (ISIC-Rev.4), 2 digit level: 55 - Accommodation" = "R4-55",
                           "Economic activity (ISIC-Rev.4), 2 digit level: 02 - Forestry and logging" = "R4-02",
                           "Economic activity (ISIC-Rev.4), 2 digit level: 51 - Air transport" = "R4-51"
  ))
# Change name of indirect activities
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 10 - Mining of coal and lignite; extraction of peat" = "R3.1-10"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 11 - Extraction of crude petroleum and natural gas; service activities incidental to oil and gas extraction, excluding surveying" = "R3.1-11"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 12 - Mining of uranium and thorium ores" = "R3.1-12"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 13 - Mining of metal ores" = "R3.1-13"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 14 - Other mining and quarrying" = "R3.1-14"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 17 - Manufacture of textiles" = "R3.1-17"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 18 - Manufacture of wearing apparel; dressing and dyeing of fur" = "R3.1-18"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 19 - Tanning and dressing of leather; manufacture of luggage, handbags, saddlery, harness and footwear" = "R3.1-19"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 20 - Manufacture of wood and of products of wood and cork, except furniture; manufacture of articles of straw and plaiting materials" = "R3.1-20"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 21 - Manufacture of paper and paper products" = "R3.1-21"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 22 - Publishing, printing and reproduction of recorded media" = "R3.1-22"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 23 - Manufacture of coke, refined petroleum products and nuclear fuel" = "R3.1-23"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 24 - Manufacture of chemicals and chemical products" = "R3.1-24"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 25 - Manufacture of rubber and plastics products" = "R3.1-25"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 26 - Manufacture of other non-metallic mineral products" = "R3.1-26"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 27 - Manufacture of basic metals" = "R3.1-27"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 28 - Manufacture of fabricated metal products, except machinery and equipment" = "R3.1-28"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 29 - Manufacture of machinery and equipment n.e.c." = "R3.1-29"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 30 - Manufacture of office, accounting and computing machinery" = "R3.1-30"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 31 - Manufacture of electrical machinery and apparatus n.e.c." = "R3.1-31"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 32 - Manufacture of radio, television and communication equipment and apparatus" = "R3.1-32"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 33 - Manufacture of medical, precision and optical instruments, watches and clocks" = "R3.1-33"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 34 - Manufacture of motor vehicles, trailers and semi-trailers" = "R3.1-34"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 35 - Manufacture of other transport equipment" = "R3.1-35"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 36 - Manufacture of furniture; manufacturing n.e.c." = "R3.1-36"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 37 - Recycling" = "R3.1-37"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 40 - Electricity, gas, steam and hot water supply" = "R3.1-40"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 41 - Collection, purification and distribution of water" = "R3.1-41"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 45 - Construction" = "R3.1-45"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 50 - Sale, maintenance and repair of motor vehicles and motorcycles; retail sale of automotive fuel" = "R3.1-50"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 64 - Post and telecommunications" = "R3.1-64"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 65 - Financial intermediation, except insurance and pension funding" = "R3.1-65"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 66 - Insurance and pension funding, except compulsory social security" = "R3.1-66"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 67 - Activities auxiliary to financial intermediation" = "R3.1-67"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 70 - Real estate activities" = "R3.1-70"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 71 - Renting of machinery and equipment without operator and of personal and household goods" = "R3.1-71"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 72 - Computer and related activities" = "R3.1-72"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 73 - Research and development" = "R3.1-73"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 74 - Other business activities" = "R3.1-74"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 75 - Public administration and defence; compulsory social security" = "R3.1-75"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 80 - Education" = "R3.1-80"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 85 - Health and social work" = "R3.1-85"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 90 - Sewage and refuse disposal, sanitation and similar activities" = "R3.1-90"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 91 - Activities of membership organizations n.e.c." = "R3.1-91"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 92 - Recreational, cultural and sporting activities" = "R3.1-92"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 93 - Other service activities" = "R3.1-93"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 95 - Activities of private households as employers of domestic staff" = "R3.1-95"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 96 - Undifferentiated goods-producing activities of private households for own use" = "R3.1-96"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 97 - Undifferentiated service-producing activities of private households for own use" = "R3.1-97"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: 99 - Extraterritorial organizations and bodies" = "R3.1-99"))
# for Rev 4
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 05 - Mining of coal and lignite" = "R4-05"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 06 - Extraction of crude petroleum and natural gas" = "R4-06"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 07 - Mining of metal ores" = "R4-07"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 08 - Other mining and quarrying" = "R4-08"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 09 - Mining support service activities" = "R4-09"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 13 - Manufacture of textiles" = "R4-13"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 14 - Manufacture of wearing apparel" = "R4-14"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 15 - Manufacture of leather and related products" = "R4-15"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 16 - Manufacture of wood and of products of wood and cork, except furniture; manufacture of articles of straw and plaiting materials" = "R4-16"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 17 - Manufacture of paper and paper products" = "R4-17"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 18 - Printing and reproduction of recorded media" = "R4-18"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 19 - Manufacture of coke and refined petroleum products" = "R4-19"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 20 - Manufacture of chemicals and chemical products" = "R4-20"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 21 - Manufacture of pharmaceuticals, medicinal chemical and botanical products" = "R4-21"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 22 - Manufacture of rubber and plastics products" = "R4-22"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 23 - Manufacture of other non-metallic mineral products" = "R4-23"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 24 - Manufacture of basic metals" = "R4-24"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 25 - Manufacture of fabricated metal products, except machinery and equipment" = "R4-25"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 26 - Manufacture of computer, electronic and optical products" = "R4-26"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 27 - Manufacture of electrical equipment" = "R4-27"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 28 - Manufacture of machinery and equipment n.e.c." = "R4-28"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 29 - Manufacture of motor vehicles, trailers and semi-trailers" = "R4-29"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 30 - Manufacture of other transport equipment" = "R4-30"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 31 - Manufacture of furniture" = "R4-31"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 32 - Other manufacturing" = "R4-32"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 33 - Repair and installation of machinery and equipment" = "R4-33"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 35 - Electricity, gas, steam and air conditioning supply" = "R4-35"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 36 - Water collection, treatment and supply" = "R4-36"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 37 - Sewerage" = "R4-37"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 38 - Waste collection, treatment and disposal activities; materials recovery" = "R4-38"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 39 - Remediation activities and other waste management services" = "R4-39"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 41 - Construction of buildings" = "R4-41"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 42 - Civil engineering" = "R4-42"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 43 - Specialized construction activities" = "R4-43"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 45 - Wholesale and retail trade and repair of motor vehicles and motorcycles" = "R4-45"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 53 - Postal and courier activities" = "R4-53"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 58 - Publishing activities" = "R4-58"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 59 - Motion picture, video and television programme production, sound recording and music publishing activities" = "R4-59"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 60 - Programming and broadcasting activities" = "R4-60"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 61 - Telecommunications" = "R4-61"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 62 - Computer programming, consultancy and related activities" = "R4-62"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 63 - Information service activities" = "R4-63"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 64 - Financial service activities, except insurance and pension funding" = "R4-64"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 65 - Insurance, reinsurance and pension funding, except compulsory social security" = "R4-65"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 66 - Activities auxiliary to financial service and insurance activities" = "R4-66"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 68 - Real estate activities" = "R4-68"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 69 - Legal and accounting activities" = "R4-69"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 70 - Activities of head offices; management consultancy activities" = "R4-70"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 71 - Architectural and engineering activities; technical testing and analysis" = "R4-71"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 72 - Scientific research and development" = "R4-72"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 73 - Advertising and market research" = "R4-73"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 74 - Other professional, scientific and technical activities" = "R4-74"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 75 - Veterinary activities" = "R4-75"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 77 - Rental and leasing activities" = "R4-77"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 78 - Employment activities" = "R4-78"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 79 - Travel agency, tour operator, reservation service and related activities" = "R4-79"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 80 - Security and investigation activities" = "R4-80"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 81 - Services to buildings and landscape activities" = "R4-81"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 82 - Office administrative, office support and other business support activities" = "R4-82"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 84 - Public administration and defence; compulsory social security" = "R4-84"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 85 - Education" = "R4-85"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 86 - Human health activities" = "R4-86"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 87 - Residential care activities" = "R4-87"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 88 - Social work activities without accommodation" = "R4-88"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 90 - Creative, arts and entertainment activities" = "R4-90"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 91 - Libraries, archives, museums and other cultural activities" = "R4-91"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 92 - Gambling and betting activities" = "R4-92"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 93 - Sports activities and amusement and recreation activities" = "R4-93"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 94 - Activities of membership organizations" = "R4-94"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 95 - Repair of computers and personal and household goods" = "R4-95"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 96 - Other personal service activities" = "R4-96"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 97 - Activities of households as employers of domestic personnel" = "R4-97"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 98 - Undifferentiated goods- and services-producing activities of private households for own use" = "R4-98"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: 99 - Activities of extraterritorial organizations and bodies" = "R4-99"))
# keep the total level for regression use
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.3.1), 2 digit level: Total" = "R3.1-total"))
ILO_Full <- ILO_Full %>% mutate(activity = recode(activity, "Economic activity (ISIC-Rev.4), 2 digit level: Total" = "R4-total"))

# balancing data in gender level
ILO_full <- ILO_Full %>% spread(sex,value)
# fill in missing female or male data based on total data based on assumption 'Female + Male = Total'
names(ILO_full) <- c("code","activity","year","female","male","other","total")
# Drop all rows with missing data for both female and male.
ILO_full <- ILO_full %>%
  filter(!(female == 0 & male == 0 & other == 0))
ILO_full <- ILO_full %>%
  mutate(other = ifelse(is.na(other), 0, other))
ILO_full <- ILO_full %>%
  mutate(total = total - other)
ILO_full <- select(ILO_full,-other)
sum(is.na(ILO_full$female)) # 16334 rows have NA female data
sum(is.na(ILO_full$male)) # 2799 rows have NA male data
filled_ILO <- ILO_full %>%
  mutate(
    female = ifelse(is.na(female), total - male, female), 
    male = ifelse(is.na(male), total - female, male)
  )
unique(filled_ILO$code)
# change data structure back to long format --> gender values gathered into one column
names(filled_ILO) <- c("code","activity","year","Sex: Female","Sex: Male","Sex: Total")
filled_ILO <- filled_ILO %>%
  gather(key = "sex", value = "value", `Sex: Female`, `Sex: Male`, `Sex: Total`)
filled_ILO <- filled_ILO %>% select(code,sex,activity,year,value)
# Aggregate activities into Eora sectors
aggregate_ILO <- filled_ILO %>%
  replace_na(list(value = 0)) %>%
  group_by(code, sex, activity, year) %>%
  mutate(
    activity = ifelse(activity %in% c("R3.1-01", "R3.1-02", "R4-01", "R4-02","R3.1-05","R4-03"), "A01T02", as.character(activity)),
    activity = ifelse(activity %in% c("R3.1-15", "R3.1-16", "R4-10", "R4-11", "R4-12"), "A04", as.character(activity)),
    activity = ifelse(activity %in% c("R3.1-55", "R4-55", "R4-56"), "A18", as.character(activity)),
    activity = ifelse(activity %in% c("R3.1-60", "R3.1-61", "R3.1-62", "R3.1-63", "R4-49", "R4-50", "R4-51", "R4-52"), "A19", as.character(activity)),
    activity = ifelse(activity %in% c("R3.1-51", "R4-46"), "A16", as.character(activity)),
    activity = ifelse(activity %in% c("R3.1-52", "R4-47"), "A17", as.character(activity)),
    activity = ifelse(activity %in% c("R3.1-10","R3.1-11","R3.1-12","R3.1-13","R3.1-14","R4-05","R4-06","R4-07","R4-08","R4-09"), "A03", as.character(activity)),
    activity = ifelse(activity %in% c("R3.1-17","R3.1-18","R3.1-19","R4-13","R4-14","R4-15"), "A05", as.character(activity)),
    activity = ifelse(activity %in% c("R3.1-20","R3.1-21","R3.1-22","R4-16","R4-17","R4-18"), "A06", as.character(activity)),
    activity = ifelse(activity %in% c("R3.1-23","R3.1-24","R3.1-25","R3.1-26","R4-19","R4-20","R4-21","R4-22","R4-23"), "A07", as.character(activity)),
    activity = ifelse(activity %in% c("R3.1-27", "R3.1-28", "R4-24", "R4-25"), "A08", as.character(activity)),
    activity = ifelse(activity %in% c("R3.1-29","R3.1-30","R3.1-31","R3.1-32","R3.1-33","R4-26","R4-27","R4-28"), "A09", as.character(activity)),
    activity = ifelse(activity %in% c("R3.1-34", "R3.1-35", "R4-29", "R4-30"), "A10", as.character(activity)),
    activity = ifelse(activity %in% c("R3.1-36", "R4-31", "R4-32", "R4-33"), "A11", as.character(activity)),
    activity = ifelse(activity %in% c("R3.1-37", "R4-38", "R4-39"), "A12", as.character(activity)),
    activity = ifelse(activity %in% c("R3.1-40", "R3.1-41", "R4-35", "R4-36", "R4-37"), "A13", as.character(activity)),
    activity = ifelse(activity %in% c("R3.1-45", "R4-41", "R4-42", "R4-43"), "A14", as.character(activity)),
    activity = ifelse(activity %in% c("R3.1-50", "R4-45"), "A15", as.character(activity)),
    activity = ifelse(activity %in% c("R3.1-64", "R4-53", "R4-58", "R4-59", "R4-60", "R4-61", "R4-62", "R4-63"), "A20", as.character(activity)),
    activity = ifelse(activity %in% c("R3.1-65","R3.1-66","R3.1-67","R3.1-70","R3.1-71","R3.1-72","R3.1-73","R3.1-74", "R4-64", "R4-65", "R4-66", "R4-67", "R4-68", "R4-69", "R4-70", "R4-77", "R4-78", "R4-79"), "A21", as.character(activity)),
    activity = ifelse(activity %in% c("R3.1-75", "R4-82", "R4-84"), "A22", as.character(activity)),
    activity = ifelse(activity %in% c("R3.1-80","R3.1-85","R3.1-90","R3.1-91","R3.1-92","R3.1-93", "R4-71","R4-72","R4-73","R4-74","R4-75","R4-80", "R4-81", "R4-85", "R4-86", "R4-87", "R4-88", "R4-90", "R4-91", "R4-92", "R4-93", "R4-94", "R4-95", "R4-96"), "A23", as.character(activity)),
    activity = ifelse(activity %in% c("R3.1-95", "R3.1-96","R3.1-97","R4-97", "R4-98"), "A24", as.character(activity)),
    activity = ifelse(activity %in% c("R3.1-99", "R4-99"), "A25", as.character(activity)),
    activity = ifelse(activity %in% c("Economic activity (ISIC-Rev.3.1), 2 digit level: Not elsewhere classified", "Economic activity (ISIC-Rev.4), 2 digit level: Not elsewhere classified"), "A26", as.character(activity)),
    activity = ifelse(activity %in% c("R3.1-total", "R4-total"), "Total", as.character(activity)),
  ) %>%
  group_by(code, sex, activity, year) %>%
  summarize(value = sum(value))
# spread to gender level
aggregate_ILO <- aggregate_ILO %>% spread(sex,value)
names(aggregate_ILO) <- c("code","activity","year","female","male","total")
# Clean the data columns
aggregate_ILO$index <- paste(aggregate_ILO$code, aggregate_ILO$year, sep = "-") # combine ctry code and activity index into one
# Separate data into direct and indirect activities
direct_activity <- c("A01T02","A04","A16","A17","A18","A19")
ILO_direct <- subset(aggregate_ILO, activity %in% direct_activity)
ILO_indrect <- subset(aggregate_ILO,!activity %in% direct_activity)
# keep country-year that only has all 6 direct agri-food activities
unique(ILO_direct$index)  # 1698 country-year pairs
ILO_direct <- ILO_direct %>%
  group_by(index) %>%
  filter(n_distinct(activity) == length(direct_activity) & all(direct_activity %in% activity)) %>%
  ungroup()
unique(ILO_direct$index) # 1638 country-year pairs
# get all country_year rows that have all 6 direct activities data
full_ILO <- rbind(ILO_direct, ILO_indrect) # combine direct and indirect data frames
unique(full_ILO$index) # 1698 pairs
full_ILO <- subset(full_ILO, index %in% ILO_direct$index) # keep only country_year rows that have all valid 6 direct activities data, and drop all others, based on 'index'
unique(full_ILO$index) # 1638 pair
unique(full_ILO$code) # 147 countries
country_year_table <- full_ILO %>%
  select(code, year) %>%
  distinct() %>%  # Remove duplicate rows to ensure accurate counts
  group_by(year) %>%
  summarise(
    countries = toString(sort(unique(code))),
    num_countries = n_distinct(code)  # Count the number of distinct countries per year
  ) %>%
  ungroup()
# Export the final labor data: 1638 country-year pairs, covering 147 countries from 1991 to 2021.
write.xlsx(full_ILO, file = "_data/processed_data/ILO_gender.xlsx", sheetName = "ILO_gender")

