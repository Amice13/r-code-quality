library(tidyverse)
library(spdep)
library(sf)
library(lfe)
library(stargazer)
library(aweek)
library(tmap)
options(stringsAsFactors = FALSE)

# Import Arrest Releases

releases<-read.csv("Releases.csv")

releases$Zip[releases$Zip==""]<-NA
releases$Zip[releases$Zip=="0"]<-"00000"

releases_2<-releases%>%
  mutate(zip_code = str_sub(Zip, start = 1L, end = 5L),
         date = as.Date(str_sub(releaseFromCustodyDate,start = 1L,end = 10L),"%d-%m-%Y"))%>%
  group_by(zip_code, date)%>%
  summarise(releases = n())

march_rel<-releases_2%>%
  filter(date>"2020-02-29"& date<"2020-04-01")%>%
  group_by(zip_code)%>%
  summarise(mar_releases=sum(releases,na.rm = TRUE))


weekly_rel<-releases_2%>%
  mutate(weeknum = date2week(date,numeric = TRUE))%>%
  group_by(zip_code,weeknum)%>%
  summarise(weekly_releases = sum(releases,na.rm = TRUE))

for (i in 5:32) {
  nam<-paste0("wk",i)
  split<-weekly_rel%>%filter(weeknum == i)%>%select(1,3)
  colnames(split)<-c("zip_code",paste0("weekrelease",i))
  assign(nam,split)
}

# Covid Data

chi_covid<-read.csv("COVID-19_Cases__Tests__and_Deaths_by_ZIP_Code_wk_32.csv")
chi_covid_2<-chi_covid%>%
  rename(zip_code = ZIP.Code,
         weeknum = Week.Number)
headr<-as_tibble(colnames(chi_covid_2))

fix_fn <- function(x) ifelse(is.na(x), 0 , x)
chi_covid_2[,c(5:14)]<-apply(chi_covid_2[,c(5:14)],2,fix_fn)
nafn<-function(x) sum(is.na(x))
nacount<-as.data.frame(apply(chi_covid_2,2,nafn))

chi_covid_2<-chi_covid_2%>%
  mutate(cases_per_capita=Cases...Weekly/Population,
         cumulative_cases_per_capita = Cases...Cumulative/Population,
         tests_per_capita = Tests...Weekly/Population,
         cumulative_tests_per_capita = Tests...Cumulative/Population)

cum_cases<-chi_covid_2%>%
  filter(weeknum < 14)%>%
  group_by(zip_code)%>%
  summarise(cumulative_case_rate_13 = sum(cases_per_capita,na.rm=TRUE),)

chi_covid_3<-chi_covid_2%>%
  left_join(cum_cases,by=c("zip_code"="zip_code"))

# template

zips<-chi_covid_3%>%
  distinct(zip_code)%>%
  filter(!(zip_code %in% c("Unknown","60666")))
weeks<-as_tibble(seq(from =10, to = 32))
colnames(weeks)<-"weeknum"
template<-merge(zips,weeks,all = TRUE)

# Demographic variables from ACS 2018 (selected from www.socialexplorer.com)

acs_trim<-read.csv("ACS_2018_pct_variables_socialexplorer_aug_28.csv",stringsAsFactors = TRUE)
acs_trim$ZIP.Code.Tabulation.Area..5.digit.<-as.character(acs_trim$ZIP.Code.Tabulation.Area..5.digit.)

zip_fn<-function(x) ifelse(str_length(x) == 3, paste0("00",x),
                           ifelse(str_length(x) == 4, paste0("0",x),x))
acs_trim$ZIP.Code.Tabulation.Area..5.digit.<-lapply(acs_trim$ZIP.Code.Tabulation.Area..5.digit.,zip_fn)
acs_trim$ZIP.Code.Tabulation.Area..5.digit.<-as.character(acs_trim$ZIP.Code.Tabulation.Area..5.digit.)

acs_trim<-acs_trim%>%
  select(-1)

# Prepping the final merged dataset

chi_covid_final<-template%>%
  left_join(chi_covid_3, by=c("zip_code"="zip_code","weeknum"="weeknum"))%>%
  left_join(march_rel, by=c("zip_code"="zip_code"))%>%
  mutate(mar_releases_rate=mar_releases/Population)%>%
  left_join(acs_trim,by=c("zip_code"="ZIP.Code.Tabulation.Area..5.digit."))

chi_covid_final$mar_releases_rate[is.na(chi_covid_final$mar_releases_rate)]<-0
chi_covid_final$mar_releases[is.na(chi_covid_final$mar_releases)]<-0
chi_covid_final2<-chi_covid_final[, colSums(is.na(chi_covid_final)) == 0]

# Read in shapefiles for chicago

chic_shp<-st_read(dsn = "Zip_20Codes/Zip_Codes.shp")
chic_nb<-setNames(poly2nb(pl = chic_shp,row.names = "OBJECTID", queen = TRUE), chic_shp$ZIP)
chic_ets<-nb2mat(chic_nb, zero.policy = TRUE, style = 'B')
dimnames(chic_ets)<-list(chic_shp$ZIP,chic_shp$ZIP)
chic_list <- sapply(row.names(chic_ets), function(x) names(which(chic_ets[x, ] == 1)))
chic_pairs <- data.frame(zip_code=rep(names(chic_list), sapply(chic_list, length)), 
                         neighbour=unlist(chic_list))

# Process neighbor zipcode cases data
chic_df<-chi_covid_final2

pop<-chic_df%>%count(zip_code, tot.Pop)
nb_merge<-chic_pairs%>%
  filter(neighbour != "60666")%>%
  left_join(march_rel, by=c("neighbour"="zip_code"))%>%
  left_join(pop,by=c("neighbour"="zip_code"))

nb_releases<-nb_merge%>%
  group_by(zip_code)%>%
  summarise(mar_releases_donut = mean(mar_releases, na.rm = TRUE),
            mean_donut_pop = mean(tot.Pop),
            mar_releases_donut_rate = mar_releases_donut/mean_donut_pop)
