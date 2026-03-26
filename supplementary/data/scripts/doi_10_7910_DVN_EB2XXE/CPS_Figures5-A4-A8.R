# Code for CPS article replication of Figure 5, Figure A4, and Figure A8

# List of required packages
required_packages <- c("dplyr", "ggplot2", "tidyr", "reshape2", "plyr", "stringr", "janitor", "data.table", "ggpubr")

# Install missing packages
missing_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(missing_packages)) {
  install.packages(missing_packages)
}

# Load all required libraries
lapply(required_packages, library, character.only = TRUE)

setwd('~/Dropbox/CPS/CPS_Materials') # set working directory for the project

# start with the cleaned data from Step 1
HUNdata <- read.csv(file="HungarianProtests_cleaned.csv", header=TRUE)

#Let's subset to 2002 - 2010 as the analysis period (before Fidesz comes to office in spring 2010)
HUNdatapre2010<- subset(HUNdata, v004year >2001 & v004year<2010)
HUNdata2010<- subset(HUNdata, v004year==2010 & v004month<5)
HUNdata<-rbind(HUNdatapre2010, HUNdata2010)

#Now we create indicator variables for the county capital mentions
HUNdata$v003_Budapest <- stringr::str_count(HUNdata$v003_lower, stringr::str_to_lower("Budapest|Bud|Pest|kerület|f[:punct:]város|főváros|Tüzér ut|Rákóczi tér|Teve ut|Astori|Ötvenhatosok ter|Nemzetgazdasági Minisztérium|Szabad sajtó út|József nádor tér|Magyar Rádió székház|Közraktár ut|Bródy Sándor ut|Újpalota|Dózsa György tér|Belügyminisztérium|miniszterelnök ház|tétény|Károlyi szobor|Egészségügyi Minisztérium|Lehel ut|Bartók Béla út|Kerepesi út|Batthyány-örökmécses|Janikovszky|MDF-székház|Kodály körönd|FÉG Konvektorgyártó|Vörösmarty tér|Jászai Mari tér|Képviselői Irodaház|OPNI|Miniszterelnöki Hivatal|zsidónegyed|Hunyadi t|Március 15. tér|Apeh épület|Szent János kórház|Szemlőhegy utca|gödi|Gazdasági és Közlekedési Minisztérium|Nagymező utca|Parlament|Syma csarnok|Broadway jegyiroda|Verseny utca|Felvonulási tér|Főpolgármesteri hivatal|rákoscsaba|Magyar Posta székháza|Alkotmány ut|szakminisztérium|Bank Center|Hollán Ernő utca|Nagykövetség|Gizella utca|Népszava Szerkesztősége el|Oktatási Minisztérium|Somogyi Béla|Hősök ter|Külügyminisztérium|Vértanúk ter|Pázmány Péter Katolikus Egyetem|kerepes|FVM|Corvin köz|Felvonulási tér|ÁPV Rt|Szabó Ervin tér|Magyar Tudományos Akadémia épület|szabadságtér|Szabadság Kör|Kőbánya|Széll Kálmán tér|Kádár szobor|Árpádföld|Thália Színház|Markó u|Bazilika|Lágymányosi híd|Margitsziget|József Attila szobor|kistarcsa|Tűzraktér|Medgyessy Péter háza|Schöpf-Merei Ágost Kórház|Svábhegy|Kossuth t|keleti pályaudvar|parlament el[:punct:]|MOM m|parlament elé|Magyar Televízió székház|Parlamenttel szemközt|Parlamentnél|Várkerület|Nyugati tér|Magyar Nemzeti Bank|Köztársaság tér|Köztársaság-tér|Köztársaságtér|MSZP székház|Terézváros|Erzsébetváros|Erzsébet hid|Józsefváros|Gazdagrét|Ferencváros|Kőbánya|Zugló|Rákospalota|Csepel|Soroksár|Angyalföld|operaház|Terror Ház|Moszkva tér|Andrássy út|rakpart|lánchíd|Dohány út|Dohány ut|[:blank:]ELTE|[:blank:]Corvinus|Művészeti Akadémia|Eötvös|Belgrád rakpart|vajdahunyad|oktogon|ferenciek|Erzsébet tér|Szabadság tér|Deák t|Nemzeti Galéria|blaha lujza|Kerepesi|nagykövetség|Felvonulási t|Országház|MTV Rt|Margit kórház|MTV székház|Zeneakadémia|a Várban|Bajcsy-Zsilinszky"))
HUNdata$v003_Budapest <- ifelse(HUNdata$v003_Budapest>0, 1, 0)

HUNdata$v003_Debrecen <- stringr::str_count(HUNdata$v003_lower, stringr::str_to_lower("Debrecen|Hajdú-Bihar|Hajdú"))
HUNdata$v003_Debrecen <- ifelse(HUNdata$v003_Debrecen>0, 1, 0)

HUNdata$v003_Szeged <- stringr::str_count(HUNdata$v003_lower, stringr::str_to_lower("Szeged|Csongrád-Csanád|Csongrád"))
HUNdata$v003_Szeged <- ifelse(HUNdata$v003_Szeged>0, 1, 0)

HUNdata$v003_Miskolc <- stringr::str_count(HUNdata$v003_lower, stringr::str_to_lower("Miskolc|Borsod-Abaúj-Zemplén|Borsod"))
HUNdata$v003_Miskolc <- ifelse(HUNdata$v003_Miskolc>0, 1, 0)

HUNdata$v003_Pécs <- stringr::str_count(HUNdata$v003_lower, stringr::str_to_lower("Pécs|Baranya"))
HUNdata$v003_Pécs <- ifelse(HUNdata$v003_Pécs>0, 1, 0)

HUNdata$v003_Gyor <- stringr::str_count(HUNdata$v003_lower, stringr::str_to_lower("Győr|Győr-Moson-Sopron"))
HUNdata$v003_Gyor <- ifelse(HUNdata$v003_Gyor>0, 1, 0)

HUNdata$v003_Nyíregyháza <- stringr::str_count(HUNdata$v003_lower, stringr::str_to_lower("Nyíregyház|Szabolcs-Szatmár-Bereg|Szabolcs megye"))
HUNdata$v003_Nyíregyháza <- ifelse(HUNdata$v003_Nyíregyháza>0, 1, 0)

HUNdata$v003_Kecskemét <- stringr::str_count(HUNdata$v003_lower, stringr::str_to_lower("Kecskemét|Bács-Kiskun"))
HUNdata$v003_Kecskemét <- ifelse(HUNdata$v003_Kecskemét>0, 1, 0)

HUNdata$v003_Székesfehérvár <- stringr::str_count(HUNdata$v003_lower, stringr::str_to_lower("Székesfehérvár|Fejér"))
HUNdata$v003_Székesfehérvár <- ifelse(HUNdata$v003_Székesfehérvár>0, 1, 0)

HUNdata$v003_Szombathely <- stringr::str_count(HUNdata$v003_lower, stringr::str_to_lower("Szombathely|Vas megye"))
HUNdata$v003_Szombathely <- ifelse(HUNdata$v003_Szombathely >0, 1, 0)

HUNdata$v003_Szolnok <- stringr::str_count(HUNdata$v003_lower, stringr::str_to_lower("Szolnok|Jász-Nagykun-Szolnok|Jász"))
HUNdata$v003_Szolnok <- ifelse(HUNdata$v003_Szolnok>0, 1, 0)

HUNdata$v003_Tatabánya <- stringr::str_count(HUNdata$v003_lower, stringr::str_to_lower("Tatabány|Komárom-Esztergom|[:blank:]Tata|Komárom|Esztergom"))
HUNdata$v003_Tatabánya <- ifelse(HUNdata$v003_Tatabánya>0, 1, 0)

HUNdata$v003_Kaposvár <- stringr::str_count(HUNdata$v003_lower, stringr::str_to_lower("Kaposvár|Somogy"))
HUNdata$v003_Kaposvár <- ifelse(HUNdata$v003_Kaposvár>0, 1, 0)

#let's relocate the partisanship indicators to follow the city binary indicators so we can easily section this new dataset by column placement
HUNdata <- HUNdata %>% 
  relocate(leftist, rightist, radrightist, right_all, .after = v003_Kaposvár)

cities<- HUNdata[,1175:1192]
names(cities)[2:14] <- substring(names(cities)[2:14], 6)

# Function to transform city count values based on salience index, excluding specific columns for partisanship variables
transform_excluding_columns <- function(cities, salience) {
  # List of columns to exclude from transformation
  exclude_columns <- c('leftist', 'rightist', 'radrightist', 'right_all', salience)

  # Loop over all rows
  for (i in 1:nrow(cities)) {
    # Loop over all columns, excluding the ones in the exclude list
    for (j in setdiff(names(cities), exclude_columns)) {
      if (cities[i, j] == 1 & cities[i, salience] > 0) {
        # Set the value to the value in the reference column
        cities[i, j] <- cities[i, salience]
      }
    }
  }
  return(cities)
}

cities_transformed <-transform_excluding_columns(cities, "salience")
cities_totalprotest <-as.data.frame(colSums(cities_transformed[2:14]))
names(cities_totalprotest)[1] ="Count protests"
names(cities_totalprotest)[0] ="city"

# The stringi::stri_trans_general() function is applied first to convert characters to "Latin-ASCII."
# The result is then passed to stringr::str_to_lower() to convert the text to lowercase.
# The combined result is assigned back to rownames(cities_totalprotest)
rownames(cities_totalprotest) <- stringr::str_to_lower(
  stringi::stri_trans_general(rownames(cities_totalprotest), "Latin-ASCII")
)

# Merge in the electoral results data by city name
city_info <- read.csv(file="hun20cities.csv", header=TRUE, fileEncoding = "ISO-8859-2")
rownames(city_info) <- city_info[,1]
cities_fullypopulated<- merge(cities_totalprotest, city_info,  by = 'row.names', all=T)
rownames(cities_fullypopulated) <- cities_fullypopulated[,1]
# Remove the first column with city names to avoid column redundancy
cities_fullypopulated <- cities_fullypopulated[,-1]
# Calculate protests per 10K residents using 2011 city population census
cities_fullypopulated$percapitaprotest<- 10000*(cities_fullypopulated$`Count protests`/cities_fullypopulated$nepes2011)


cities_Lideology <-aggregate(cities_transformed[2:14], by=cities_transformed[15], FUN=sum)
#let's cut the top row, which shows the counts where this observation is false. Only keep "true"
cities_Lideology  <- cities_Lideology[-(1),]
cities_Lideology[1, 1] = "leftist"
colnames(cities_Lideology)[colnames(cities_Lideology) == "leftist"] ="Ideology"
cities_Lideology<-as.data.frame(t(cities_Lideology))
cities_Lideology<- janitor::row_to_names(cities_Lideology, 1, remove_rows_above = FALSE) 

cities_Rideology <-aggregate(cities_transformed[2:14], by=cities_transformed[16], FUN=sum)
cities_Rideology  <- cities_Rideology[-(1),]
cities_Rideology[1, 1] = "rightist"
colnames(cities_Rideology)[colnames(cities_Rideology) == "rightist"] ="Ideology"
cities_Rideology<-as.data.frame(t(cities_Rideology))
cities_Rideology<- janitor::row_to_names(cities_Rideology, 1, remove_rows_above = FALSE) 

cities_RRideology <-aggregate(cities_transformed[2:14], by=cities_transformed[17], FUN=sum)
cities_RRideology  <- cities_RRideology[-(1),]
cities_RRideology[1, 1] = "radrightist"
colnames(cities_RRideology)[colnames(cities_RRideology) == "radrightist"] ="Ideology"
cities_RRideology<-as.data.frame(t(cities_RRideology))
cities_RRideology<- janitor::row_to_names(cities_RRideology, 1, remove_rows_above = FALSE) 

cities_RALLideology <-aggregate(cities_transformed[2:14], by=cities_transformed[18], FUN=sum)
cities_RALLideology  <- cities_RALLideology[-(1),]
cities_RALLideology[1, 1] = "all rightist"
colnames(cities_RALLideology)[colnames(cities_RALLideology) == "all rightist"] ="Ideology"
cities_RALLideology<-as.data.frame(t(cities_RALLideology))
cities_RALLideology<- janitor::row_to_names(cities_RALLideology, 1, remove_rows_above = FALSE) 

cities_ideology <- cbind(cities_Rideology, cities_RRideology, cities_Lideology, cities_RALLideology)
cities_ideology$city <- rownames(cities_ideology)
cities_ideology<- cities_ideology %>% relocate(city, .before=rightist)

cities_ideology$city <- stringr::str_to_lower(stringi::stri_trans_general(cities_ideology$city, "Latin-ASCII"))
rownames(cities_ideology) <- cities_ideology$city
cities_fullypopulated <- cities_fullypopulated[,-1]

cities_ideology_percapita<- merge(cities_ideology, city_info,  by = 'row.names', all=T)
cities_ideology_percapita <- cities_ideology_percapita[,-1]

#rightist protests per 10K residents
cities_ideology_percapita$rightist<-as.numeric(cities_ideology_percapita$rightist)
cities_ideology_percapita$percapitaprotest_R<- 10000*(cities_ideology_percapita$rightist/cities_ideology_percapita$nepes2011)

#radrightist protests per 10K residents
cities_ideology_percapita$radrightist<-as.numeric(cities_ideology_percapita$radrightist)
cities_ideology_percapita$percapitaprotest_RR<- 10000*(cities_ideology_percapita$radrightist/cities_ideology_percapita$nepes2011)

#leftist protests per 10K residents
cities_ideology_percapita$leftist<-as.numeric(cities_ideology_percapita$leftist)
cities_ideology_percapita$percapitaprotest_L<- 10000*(cities_ideology_percapita$leftist/cities_ideology_percapita$nepes2011)

#right_all protests per 10K residents
cities_ideology_percapita$'all rightist'<-as.numeric(cities_ideology_percapita$'all rightist')
cities_ideology_percapita$percapitaprotest_Rall<- 10000*(cities_ideology_percapita$'all rightist'/cities_ideology_percapita$nepes2011)

#partisan asymmetry in protest, defined as the difference between rightist & leftist protest per capita
cities_ideology_percapita$percapitaprotest_asymmetry<- cities_ideology_percapita$percapitaprotest_R- cities_ideology_percapita$percapitaprotest_L
#also sum 02-06 and 06-10 Fidesz electoral share change for 2002 to 2010 total.
cities_ideology_percapita$fideszchange0210<- cities_ideology_percapita$fideszchange0206 + cities_ideology_percapita$fideszchange0610

cities_protest0210<- ggplot(cities_ideology_percapita, aes(x=reorder(city, -`percapitaprotest_R`), `percapitaprotest_R`,  fill=`fideszchange0210`))+ 
  geom_bar(stat='identity')+
  theme_classic()+
  scale_fill_gradient(low="lightgrey",high="black")+
  xlab("City") +
  ylab("Mainstream right protest volume per 10,000 residents, 2002-2010") +
  guides(fill=guide_legend(title="Fidesz vote % change \n2002-2010"))+
  scale_x_discrete(guide = guide_axis(angle = 60))  

cities_asymm0210<- ggplot(cities_ideology_percapita, aes(x=reorder(city, -`percapitaprotest_asymmetry`), `percapitaprotest_asymmetry`,  fill=`fideszchange0210`))+ 
  geom_bar(stat='identity')+
  theme_classic()+
  scale_fill_gradient(low="lightgrey",high="black")+
  xlab("City") +
  ylab("Partisan asymmetry in protest volume per 10,000 residents, 2002-2010") +
  guides(fill=guide_legend(title="Fidesz vote % change \n2002-2010"))+
  scale_x_discrete(guide = guide_axis(angle = 60))  

Figure5 <- ggarrange(cities_protest0210,cities_asymm0210,
                         labels = c("", ""),
                         ncol = 1, nrow = 2,
                         common.legend =F,
                         legend = "right")
Figure5
ggsave(Figure5, file="./Figure5.pdf", height = 13, width = 8, dpi = 250)

# Statistic quoted in Section 5 of MS corresponding to this statement:
# In the cities where per capita rightist protest was highest, Fidesz increased its vote share by 5-7%.
# As it concerns partisan asymmetry, the mean difference in mainstream right and left protest volume is 0.74,
# meaning 0.74 more rightist protests occurred per 10,000 residents than leftist protests in county capitals. 
# The mean Fidesz vote share change in county capitals surpassing the mean AMM metric (0.74) is 5.1 percentage points. 
# The mean Fidesz vote share change in country capitals below that AMM value is 3.4 percentage points. 
# This is a 1.7% difference in Fidesz electoral margins between county capitals that experienced higher than
# average AMM and lower than average AMM in the contentious period from 2002-2010.
mean(cities_ideology_percapita$percapitaprotest_asymmetry)
mean(cities_ideology_percapita$fideszchange0210[cities_ideology_percapita$percapitaprotest_asymmetry > 0.7368837])
mean(cities_ideology_percapita$fideszchange0210[cities_ideology_percapita$percapitaprotest_asymmetry < 0.7368837])

# Appendix robustness checks below
# First, let's exclude the salience correction for Figure A8
cities<- HUNdata[,1176:1192]
names(cities)[1:13] <- substring(names(cities)[1:13], 6)

cities_totalprotest <-as.data.frame(colSums(cities[1:13]))
names(cities_totalprotest)[1] ="Count protests"
names(cities_totalprotest)[0] ="city"
rownames(cities_totalprotest) <- stringr::str_to_lower(
  stringi::stri_trans_general(rownames(cities_totalprotest), "Latin-ASCII")
)

city_info <- read.csv(file="hun20cities.csv", header=TRUE, fileEncoding = "ISO-8859-2")
rownames(city_info) <- city_info[,1]
cities_fullypopulated<- merge(cities_totalprotest, city_info,  by = 'row.names', all=T)
rownames(cities_fullypopulated) <- cities_fullypopulated[,1]
cities_fullypopulated <- cities_fullypopulated[,-1]
#protests per 10K residents
cities_fullypopulated$percapitaprotest<- 10000*(cities_fullypopulated$`Count protests`/cities_fullypopulated$nepes2011)

cities_Lideology <-aggregate(cities[1:13], by=cities[14], FUN=sum)
cities_Lideology  <- cities_Lideology[-(1),]
cities_Lideology[1, 1] = "leftist"
colnames(cities_Lideology)[colnames(cities_Lideology) == "leftist"] ="Ideology"
cities_Lideology<-as.data.frame(t(cities_Lideology))
cities_Lideology<- janitor::row_to_names(cities_Lideology, 1, remove_rows_above = FALSE) 

cities_Rideology <-aggregate(cities[1:13], by=cities[15], FUN=sum)
cities_Rideology  <- cities_Rideology[-(1),]
cities_Rideology[1, 1] = "rightist"
colnames(cities_Rideology)[colnames(cities_Rideology) == "rightist"] ="Ideology"
cities_Rideology<-as.data.frame(t(cities_Rideology))
cities_Rideology<- janitor::row_to_names(cities_Rideology, 1, remove_rows_above = FALSE) 

cities_RRideology <-aggregate(cities[1:13], by=cities[16], FUN=sum)
cities_RRideology  <- cities_RRideology[-(1),]
cities_RRideology[1, 1] = "radrightist"
colnames(cities_RRideology)[colnames(cities_RRideology) == "radrightist"] ="Ideology"
cities_RRideology<-as.data.frame(t(cities_RRideology))
cities_RRideology<- janitor::row_to_names(cities_RRideology, 1, remove_rows_above = FALSE) 

cities_RALLideology <-aggregate(cities[1:13], by=cities[17], FUN=sum)
cities_RALLideology  <- cities_RALLideology[-(1),]
cities_RALLideology[1, 1] = "all rightist"
colnames(cities_RALLideology)[colnames(cities_RALLideology) == "all rightist"] ="Ideology"
cities_RALLideology<-as.data.frame(t(cities_RALLideology))
cities_RALLideology<- janitor::row_to_names(cities_RALLideology, 1, remove_rows_above = FALSE) 

cities_ideology <- cbind(cities_Rideology, cities_RRideology, cities_Lideology, cities_RALLideology)
cities_ideology$city <- rownames(cities_ideology)
cities_ideology<- cities_ideology %>% relocate(city, .before=rightist)

cities_ideology$city <- stringr::str_to_lower(stringi::stri_trans_general(cities_ideology$city, "Latin-ASCII"))
rownames(cities_ideology) <- cities_ideology$city
cities_fullypopulated <- cities_fullypopulated[,-1]

cities_ideology_percapita<- merge(cities_ideology, city_info,  by = 'row.names', all=T)
cities_ideology_percapita <- cities_ideology_percapita[,-1]

#rightist protests per 10K residents
cities_ideology_percapita$rightist<-as.numeric(cities_ideology_percapita$rightist)
cities_ideology_percapita$percapitaprotest_R<- 10000*(cities_ideology_percapita$rightist/cities_ideology_percapita$nepes2011)

#radrightist protests per 10K residents
cities_ideology_percapita$radrightist<-as.numeric(cities_ideology_percapita$radrightist)
cities_ideology_percapita$percapitaprotest_RR<- 10000*(cities_ideology_percapita$radrightist/cities_ideology_percapita$nepes2011)

#leftist protests per 10K residents
cities_ideology_percapita$leftist<-as.numeric(cities_ideology_percapita$leftist)
cities_ideology_percapita$percapitaprotest_L<- 10000*(cities_ideology_percapita$leftist/cities_ideology_percapita$nepes2011)

#right_all protests per 10K residents
cities_ideology_percapita$'all rightist'<-as.numeric(cities_ideology_percapita$'all rightist')
cities_ideology_percapita$percapitaprotest_Rall<- 10000*(cities_ideology_percapita$'all rightist'/cities_ideology_percapita$nepes2011)

#partisan asymmetry in protest 
cities_ideology_percapita$percapitaprotest_asymmetry<- cities_ideology_percapita$percapitaprotest_R- cities_ideology_percapita$percapitaprotest_L

cities_ideology_percapita$fideszchange0210<- cities_ideology_percapita$fideszchange0206 + cities_ideology_percapita$fideszchange0610

cities_protest0210<- ggplot(cities_ideology_percapita, aes(x=reorder(city, -`percapitaprotest_R`), `percapitaprotest_R`,  fill=`fideszchange0210`))+ 
  geom_bar(stat='identity')+
  theme_classic()+
  scale_fill_gradient(low="lightgrey",high="black")+
  xlab("City") +
  ylab("Mainstream right protest volume per 10,000 residents, 2002-2010") +
  guides(fill=guide_legend(title="Fidesz vote % change \n2002-2010"))+
  scale_x_discrete(guide = guide_axis(angle = 60))  

cities_asymm0210<- ggplot(cities_ideology_percapita, aes(x=reorder(city, -`percapitaprotest_asymmetry`), `percapitaprotest_asymmetry`,  fill=`fideszchange0210`))+ 
  geom_bar(stat='identity')+
  theme_classic()+
  scale_fill_gradient(low="lightgrey",high="black")+
  xlab("City") +
  ylab("Partisan asymmetry in protest volume per 10,000 residents, 2002-2010") +
  guides(fill=guide_legend(title="Fidesz vote % change \n2002-2010"))+
  scale_x_discrete(guide = guide_axis(angle = 60))  

FigureA8 <- ggarrange(cities_protest0210,cities_asymm0210,
                     labels = c("", ""),
                     ncol = 1, nrow = 2,
                     common.legend =F,
                     legend = "right")
FigureA8
ggsave(FigureA8, file="./FigureA8.pdf", height = 13, width = 8, dpi = 250)


# For Figure A4 robustness check, let's remove categories v02416 (Open letters, statements and appeals to authorities or community)
##and v02422 Cyber actions (e-mail petitions, partisan websites) and v02499 (data unavailable)
HUNdata<- subset(HUNdata, v02401==1 | v02402==1 | v02403==1 | v02404==1 | v02405==1 | v02406==1 | v02407==1 |
   v02408==1 | v02409==1 | v02410==1 | v02411==1 | v02412==1 |  v02413==1|  v02414==1 |  v02415==1 |
   v02417==1 | v02418==1 | v02419==1 | v02420==1 | v02421==1 | v02423==1| v02424==1 | v02425==1)

cities<- HUNdata[,1175:1192]
names(cities)[2:14] <- substring(names(cities)[2:14], 6)

# Function to transform city count values based on salience index, excluding specific columns for partisanship variables
transform_excluding_columns <- function(cities, salience) {
  # List of columns to exclude from transformation
  exclude_columns <- c('leftist', 'rightist', 'radrightist', 'right_all', salience)
  
  # Loop over all rows
  for (i in 1:nrow(cities)) {
    # Loop over all columns, excluding the ones in the exclude list
    for (j in setdiff(names(cities), exclude_columns)) {
      if (cities[i, j] == 1 & cities[i, salience] > 0) {
        # Set the value to the value in the reference column
        cities[i, j] <- cities[i, salience]
      }
    }
  }
  return(cities)
}

cities_transformed <-transform_excluding_columns(cities, "salience")
cities_totalprotest <-as.data.frame(colSums(cities_transformed[2:14]))
cities_totalprotest <-as.data.frame(colSums(cities[2:14]))
names(cities_totalprotest)[1] ="Count protests"
names(cities_totalprotest)[0] ="city"
rownames(cities_totalprotest) <- stringr::str_to_lower(
  stringi::stri_trans_general(rownames(cities_totalprotest), "Latin-ASCII")
)

city_info <- read.csv(file="hun20cities.csv", header=TRUE, fileEncoding = "ISO-8859-2")
rownames(city_info) <- city_info[,1]

cities_fullypopulated<- merge(cities_totalprotest, city_info,  by = 'row.names', all=T)
rownames(cities_fullypopulated) <- cities_fullypopulated[,1]
cities_fullypopulated <- cities_fullypopulated[,-1]
#protests per 10K residents
cities_fullypopulated$percapitaprotest<- 10000*(cities_fullypopulated$`Count protests`/cities_fullypopulated$nepes2011)


cities_Lideology <-aggregate(cities_transformed[2:14], by=cities_transformed[15], FUN=sum)
cities_Lideology  <- cities_Lideology[-(1),]
cities_Lideology[1, 1] = "leftist"
colnames(cities_Lideology)[colnames(cities_Lideology) == "leftist"] ="Ideology"
cities_Lideology<-as.data.frame(t(cities_Lideology))
cities_Lideology<- janitor::row_to_names(cities_Lideology, 1, remove_rows_above = FALSE) 

cities_Rideology <-aggregate(cities_transformed[2:14], by=cities_transformed[16], FUN=sum)
cities_Rideology  <- cities_Rideology[-(1),]
cities_Rideology[1, 1] = "rightist"
colnames(cities_Rideology)[colnames(cities_Rideology) == "rightist"] ="Ideology"
cities_Rideology<-as.data.frame(t(cities_Rideology))
cities_Rideology<- janitor::row_to_names(cities_Rideology, 1, remove_rows_above = FALSE) 

cities_RRideology <-aggregate(cities_transformed[2:14], by=cities_transformed[17], FUN=sum)
cities_RRideology  <- cities_RRideology[-(1),]
cities_RRideology[1, 1] = "radrightist"
colnames(cities_RRideology)[colnames(cities_RRideology) == "radrightist"] ="Ideology"
cities_RRideology<-as.data.frame(t(cities_RRideology))
cities_RRideology<- janitor::row_to_names(cities_RRideology, 1, remove_rows_above = FALSE) 

cities_RALLideology <-aggregate(cities_transformed[2:14], by=cities_transformed[18], FUN=sum)
cities_RALLideology  <- cities_RALLideology[-(1),]
cities_RALLideology[1, 1] = "all rightist"
colnames(cities_RALLideology)[colnames(cities_RALLideology) == "all rightist"] ="Ideology"
cities_RALLideology<-as.data.frame(t(cities_RALLideology))
cities_RALLideology<- janitor::row_to_names(cities_RALLideology, 1, remove_rows_above = FALSE) 

cities_ideology <- cbind(cities_Rideology, cities_RRideology, cities_Lideology, cities_RALLideology)
cities_ideology$city <- rownames(cities_ideology)
cities_ideology<- cities_ideology %>% relocate(city, .before=rightist)

cities_ideology$city <- stringr::str_to_lower(stringi::stri_trans_general(cities_ideology$city, "Latin-ASCII"))
rownames(cities_ideology) <- cities_ideology$city
cities_fullypopulated <- cities_fullypopulated[,-1]

cities_ideology_percapita<- merge(cities_ideology, city_info,  by = 'row.names', all=T)
cities_ideology_percapita <- cities_ideology_percapita[,-1]

#rightist protests per 10K residents
cities_ideology_percapita$rightist<-as.numeric(cities_ideology_percapita$rightist)
cities_ideology_percapita$percapitaprotest_R<- 10000*(cities_ideology_percapita$rightist/cities_ideology_percapita$nepes2011)

#radrightist protests per 10K residents
cities_ideology_percapita$radrightist<-as.numeric(cities_ideology_percapita$radrightist)
cities_ideology_percapita$percapitaprotest_RR<- 10000*(cities_ideology_percapita$radrightist/cities_ideology_percapita$nepes2011)

#leftist protests per 10K residents
cities_ideology_percapita$leftist<-as.numeric(cities_ideology_percapita$leftist)
cities_ideology_percapita$percapitaprotest_L<- 10000*(cities_ideology_percapita$leftist/cities_ideology_percapita$nepes2011)

#right_all protests per 10K residents
cities_ideology_percapita$'all rightist'<-as.numeric(cities_ideology_percapita$'all rightist')
cities_ideology_percapita$percapitaprotest_Rall<- 10000*(cities_ideology_percapita$'all rightist'/cities_ideology_percapita$nepes2011)

#partisan asymmetry in protest 
cities_ideology_percapita$percapitaprotest_asymmetry<- cities_ideology_percapita$percapitaprotest_R- cities_ideology_percapita$percapitaprotest_L
cities_ideology_percapita$fideszchange0210<- cities_ideology_percapita$fideszchange0206 + cities_ideology_percapita$fideszchange0610

cities_protest0210<- ggplot(cities_ideology_percapita, aes(x=reorder(city, -`percapitaprotest_R`), `percapitaprotest_R`,  fill=`fideszchange0210`))+ 
  geom_bar(stat='identity')+
  theme_classic()+
  scale_fill_gradient(low="lightgrey",high="black")+
  xlab("City") +
  ylab("Mainstream right protest volume per 10,000 residents, 2002-2010") +
  guides(fill=guide_legend(title="Fidesz vote % change \n2002-2010"))+
  scale_x_discrete(guide = guide_axis(angle = 60))  

cities_asymm0210<- ggplot(cities_ideology_percapita, aes(x=reorder(city, -`percapitaprotest_asymmetry`), `percapitaprotest_asymmetry`,  fill=`fideszchange0210`))+ 
  geom_bar(stat='identity')+
  theme_classic()+
  scale_fill_gradient(low="lightgrey",high="black")+
  xlab("City") +
  ylab("Partisan asymmetry in protest volume per 10,000 residents, 2002-2010") +
  guides(fill=guide_legend(title="Fidesz vote % change \n2002-2010"))+
  scale_x_discrete(guide = guide_axis(angle = 60))  

FigureA4 <- ggarrange(cities_protest0210,cities_asymm0210,
                     labels = c("", ""),
                     ncol = 1, nrow = 2,
                     common.legend =F,
                     legend = "right")
FigureA4
ggsave(FigureA4, file="./FigureA4.pdf", height = 13, width = 8, dpi = 250)




