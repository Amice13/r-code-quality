

simpleCap <- function(x) { #            Function defined to make all caps to propper
  paste(substring(x, 1,1), tolower(substring(x, 2)),
        sep="")
}

setwd("/Users/manirouhirad/Dropbox/Research/Panama Canal-My material/Submissions codes and data/") # mac

#################################################################################################
##############################################################################################
########                                  Input data
##############################################################################################
#################################################################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                  I. rainfall
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####  A) 15-minute rainfall data until 2015
rain_15=read.dta13("input_data/acp_15Minstorms_Steve_v18112016.dta")
rain_15=data.table(rain_15)
rain_15[,month:=substr(date, 4, 5)]
rain_15[,day  :=substr(date, 1, 2)]
rain_15[,total_rain_day:=sum(Totalrain), by=c("site", "Year", "month", "day")]
rain_15 = unique(rain_15, by=c("site", "Year", "month", "day"))
rain_15 = rain_15[Year > 2006]
rain_15[,foo := uniqueN(Year), by = site]
rain_15 = rain_15[foo==9]
rain_15 = rain_15[,.(site, Year, month, day, total_rain_day)]
rain_15[,site := simpleCap(site)]
rain_15[,site := gsub(" ", "", site)]

####  B) Station locations shapefile
stations = readOGR(dsn = "input_data/stations/", layer = "stations")
stations_proj=stations@proj4string
stations = data.table(stations@data$CODENAME, stations@coords)
stations = unique(stations, by="V1")
setnames(stations, "V1", "station_name")
stations[,station_name := simpleCap(station_name)]
setkey(stations, station_name)
stations[station_name == "Tranquila",  station_name := "Tranquilla"]
stations[station_name == "Pedromigel", station_name := "Pedromiguel"]

setkey(rain_15, site)
rain_15= rain_15[stations]
rain_15= rain_15[complete.cases(total_rain_day)]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                  II. rainfall 2016-2018
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####  A) Rainfall data for 2016 - 2018. These data are averaged and provided as is.
rain_1617=fread("input_data/acp_2016_2017_2.csv")
rain_1617=melt(rain_1617, id=c("year", "month"))
rain_1617=rain_1617[,.(variable, year, month, value)]

stations_1617=fread("input_data/stations_abb_full.csv")

setkey(rain_1617, variable)
setkey(stations_1617, ID)
rain_1617=rain_1617[stations_1617]
rain_1617[, NOMBRE := simpleCap(NOMBRE)]
rain_1617 = rain_1617[complete.cases(month)]
rain_1617 = rain_1617[, NOMBRE := gsub(" ", "", NOMBRE)]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                  III. rainfall station regions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# this dataset provides us with the region of each rainfall station with respect to the canal and lake. They could be on the lake ("within"), in subwatersheds that are directly 
# connected to the lake or canal ("near"), inside the watershed, but not directly connected to the lake ("far"), or outside the Panama Canal Watershed ("outside").
site_region_3 = readRDS("input_data/site_region_3.rds")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                  IV. transit data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# transit data including the number of monthly transits, tonnage and monthly toll revenues.
transit = read.csv("input_data/transit_toll_data_03222019.csv")
transit=data.table(transit)
setkey(transit, year, month)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                   V. ship size + transit time
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# tranists by ship size and also the amount of time spent traveling (TT_), and canal water time (CWT), which includes both traveling and waiting time.
# ships are categorized into those larger and those smaller than 91ft, and NeoPanamax ships which overlaps with those above 91ft.
ship_size = fread("input_data/monthly_transits_03222018.csv")
ship_size = ship_size[complete.cases(transit_under_91)]
ship_size = ship_size[,c(1:11)]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                   VI. lake levels
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Gatun Lake levels until 2015
lake_levels=read.csv("input_data/acp_lakelevel_15min.csv")
lake_levels = data.table(lake_levels)
lake_levels[,group:=.GRP, by="Station"]
lake_levels=lake_levels[group==8]
lake_levels[Avg..level..m.==264.109, Avg..level..m. := 26.4109]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                   VII. lake levels 2016-2018
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Gatun Lake levels 2016-2018
lake_levels_1617 = fread("input_data/acp_lakelevel_1617.csv")
lake_levels_1617 = data.table(lake_levels_1617)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                   VIII. Unadjusted trade
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# imports and exports from the United States
trade_data = fread("input_data/import_export_not_adjusted.csv", header = T)
trade_data[, month:=NULL]
setnames(trade_data, "month_num", "month")
setkey(trade_data, year, month)


#################################################################################################
##############################################################################################
########                                  Data Management
##############################################################################################
#################################################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#            I. Clean rainfall data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rain_15[, site:=simpleCap(site)]
rain_15fig = unique(rain_15, by="site")
rain_15fig = rain_15fig[,.(site, coords.x1, coords.x2)]
rain_15[, month:=as.numeric(as.character(month))]
rain_15[, day:=as.numeric(as.character(day))]
rain_15[, monthly_rainfall := sum(total_rain_day), by=c("site", "Year", "month")]
rain_15= unique(rain_15, by=c("site", "Year", "month"))
rain_15= rain_15[,.(site, year=Year, month, monthly_rainfall)]

rain_1617=rain_1617[,.(site=NOMBRE, year, month, monthly_rainfall=value)]
rain_1617[, site:=gsub(" ", "", site)]
rain_1617[, site:=simpleCap(site)]
rain_1617[site=="Barrocoloradoisland", site:="Bci"]
rain_1617[site=="Balboaheights", site:="Balboahts"]
rain_1617[site=="Ca?o", site:="Cano"]
rain_1617[site=="Lascascadas", site:="Cascadas"]
rain_1617[site=="Elchorro", site:="Chorro"]
rain_1617[site=="Corozaloeste", site:="Corozal"]
rain_1617[site=="Diabloheights", site:="Diablo"]
rain_1617[site=="Lahumedad", site:="Humedad"]
rain_1617[site=="Lasraices", site:="Raices"]
rain_1617[site=="Riopiedrasarriba", site:="Riopiedras"]

foo_15  =data.table(unique(rain_15$site))
foo_1617=data.table(unique(rain_1617$site))
foo_15[,  v2:=1]
foo_1617[,v3:=2]
setkey(foo_15,   V1)
setkey(foo_1617, V1)
foo_1617=foo_1617[foo_15]
foo_1617=foo_1617[,.(V1, v2)]
setkey(foo_1617,  V1)
setkey(rain_1617, site)
rain_1617=rain_1617[foo_1617]
rain_1617[, v2:=NULL]
rain_15=rbind(rain_15, rain_1617)

rain_15[, monthly_rainfall:=as.numeric(as.character(monthly_rainfall))]
rain_15=rain_15[site!="Guacha" & site!="Balboahts"]
rain_15= rain_15[complete.cases(monthly_rainfall)]

rain_15[, year  := as.numeric(year)]
rain_15[, month := as.numeric(month)]
setkey(rain_15, site)
setkey(site_region_3, site)
rain_15=rain_15[site_region_3]
setnames(rain_15, "region_3", "region")
foo_site=unique(rain_15, by="site")
setkey(foo_site, region)
foo_site[, count:= .N, by="region"]

rain_15_within_lake=rain_15[region=="within" | region=="lake",.(region, year, month, monthly_rainfall)]
rain_15_within_lake[, monthly_rainfall := mean(monthly_rainfall), by=c("year", "month")]
rain_15_within_lake=                  unique(rain_15_within_lake, by=c("year", "month"))
rain_15_within_lake[, region:="within_lake"]

rain_15_all=rain_15[,.(region, year, month, monthly_rainfall)]
rain_15_all[, monthly_rainfall := mean(monthly_rainfall), by=c("year", "month")]
rain_15_all= unique(rain_15_all, by=c("year", "month"))
rain_15_all[, region:="all"]

rain_15= rain_15[,.(region, year, month, monthly_rainfall)]
rain_15[, monthly_rainfall := mean(monthly_rainfall), by=c("region", "year", "month")]
rain_15= unique(rain_15, by=c("region", "year", "month"))

rain_15 = rbind(rain_15, rain_15_all, rain_15_within_lake)
rain_15 = rain_15[complete.cases(year)]
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#            II. Clean Lake levels
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Clean lake level data first since it is independent of the other data analysis
lake_levels[,date:=as.character(DATETIME.dd.mm.yyyy.hh.mm.)]
lake_levels[,year:=substr(date, 7, 10)]
lake_levels[,month:=substr(date, 4, 5)]
lake_levels[,day:=substr(date, 1, 2)]
lake_levels[,date:=paste(year, month, day, sep = "-")]
lake_levels[,date:=as.Date(date)]
lake_levels[,year:=as.numeric(year)]
lake_levels[,month:=as.numeric(month)]

lake_levels[,monthly_lake_level     := mean(Avg..level..m.), by=c("year", "month")] 
lake_levels[,seas:=ifelse( month < 5, "dry", "wet")]

lake_levels=unique(lake_levels, by=c("year", "month"))
avg_dry_ll=lake_levels[seas=="dry", mean(monthly_lake_level)]
lake_levels=lake_levels[year>2006]
lake_levels=lake_levels[,.(year, month, monthly_lake_level)]
lake_levels=rbind(lake_levels, lake_levels_1617)

lake_levels[,monthly_lake_level_2     := monthly_lake_level]
lake_levels[,monthly_lake_level       := monthly_lake_level - avg_dry_ll]

setkey(lake_levels, year, month)
setkey(transit,     year, month)
data_SEM_input=merge(lake_levels, transit, all=T)
setkey(data_SEM_input, year, month)

rain_15[,year :=as.integer(year)]
rain_15[,month:=as.integer(month)]
setkey(rain_15, year, month)

data_SEM_input=merge(data_SEM_input, rain_15, all=T)
setkey(data_SEM_input, year, month)
data_SEM_input=merge(data_SEM_input, trade_data, all=T)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#             III. Prepare regression data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data_SEM_input[, tolls   := tolls/1000000]
data_SEM_input[, tonnage := tonnage/1000000]
data_SEM_input[, season:=ifelse(month < 5, "dry", "wet")]

data_SEM_input[, dummy_price_11 := ifelse(year > 2010, 1, 0)]
data_SEM_input[, dummy_price_12 := ifelse(year > 2012, 1, 0)]
data_SEM_input[, dummy_price_12 := ifelse(year == 2012 & month > 9, 1, dummy_price_12)]
data_SEM_input[, dummy_price_13 := ifelse(year > 2013, 1, 0)]
data_SEM_input[, dummy_price_13 := ifelse(year == 2013 & month > 9, 1, dummy_price_13)]
data_SEM_input[, dummy_price_16 := ifelse(year > 2016, 1, 0)]
data_SEM_input[, dummy_price_16 := ifelse(year == 2016 & month > 3, 1, dummy_price_16)]

data_SEM_input = data_SEM_input[complete.cases(region)]
setkey(data_SEM_input, region, year, month)
data_SEM_input[, m_1_lagged_rainfall:=c(NA, monthly_rainfall[-.N]), by=region]
data_SEM_input[, m_2_lagged_rainfall:= data.table::shift(monthly_rainfall, 2, type = "lag"), by=region]
data_SEM_input[, group := .GRP, by=region]
data_SEM_input[,time_trend  :=.GRP, by="year"]
data_SEM_input = data_SEM_input[, date := as.Date(paste(year, month, "01", sep = "-"))]

data_SEM_tolls_3=data_SEM_input[,.(year, month, date, time_trend, monthly_lake_level_2, tolls, transits, tonnage,  dummy_price_13, dummy_price_11, dummy_price_12, dummy_price_16, 
                                   season, total_basics_ex, total_basics_im, consumer_goods_ex, consumer_goods_im
)]

data_SEM_tolls_3=unique(data_SEM_tolls_3, by=c("year", "month"))
data_SEM_tolls_2=data_SEM_input[,.(region, year, month, m_1_lagged_rainfall)]
data_SEM_tolls_2= reshape(data_SEM_tolls_2, idvar = c("year", "month"), timevar = "region", direction = "wide")
setkey(data_SEM_tolls_2, year, month)
setkey(data_SEM_tolls_3, year, month)
data_SEM_tolls_3= data_SEM_tolls_3[data_SEM_tolls_2]

data_SEM_tolls_2=data_SEM_input[,.(region, year, month, m_2_lagged_rainfall)]
data_SEM_tolls_2= reshape(data_SEM_tolls_2, idvar = c("year", "month"), timevar = "region", direction = "wide")
setkey(data_SEM_tolls_2, year, month)
setkey(data_SEM_tolls_3, year, month)
data_SEM_tolls_3= data_SEM_tolls_3[data_SEM_tolls_2]

data_SEM_tolls_2=data_SEM_input[,.(region, year, month, monthly_rainfall)]
data_SEM_tolls_2= reshape(data_SEM_tolls_2, idvar = c("year", "month"), timevar = "region", direction = "wide")
setkey(data_SEM_tolls_2, year, month)
setkey(data_SEM_tolls_3, year, month)
data_SEM_tolls_3= data_SEM_tolls_3[data_SEM_tolls_2]

data_SEM_tolls_3[, m_1_lagged_lake_level:=c(NA, monthly_lake_level_2[-.N])]
data_SEM_tolls_3[, m_2_lagged_lake_level:=c(NA, m_1_lagged_lake_level[-.N])]

data_SEM_tolls_3[, delta_lake := monthly_lake_level_2 - m_1_lagged_lake_level]
data_SEM_tolls_3=data_SEM_tolls_3[complete.cases(tolls) & complete.cases(monthly_lake_level_2)]
data_SEM_tolls_3[, time_trend_monthly := 1:.N]

old_col = data.table(colnames(data_SEM_tolls_3))
old_col = old_col[V1  %like% "\\."]
old_col[, V2 := gsub("\\.", "_", V1)]
setnames(data_SEM_tolls_3, old = old_col$V1, new = old_col$V2)


data_SEM_tolls_3[, year_2 := ifelse(year == 2008 | year==2009, 1, 
                     ifelse(year==2010, 2, 
                            ifelse(year==2011, 3, 
                                   ifelse(year==2012, 4, 
                                          ifelse(year==2013, 5, 
                                                 ifelse(year==2014, 6, 
                                                        ifelse(year==2015 | (year==2016 & month < 5), 7, 
                                                               ifelse( (year==2016 & month > 4) | year==2017, 8, 9))))))))]

data_SEM_tolls_3[, year_3 := ifelse(year == 2008 | year==2009, 1, 
                     ifelse(year==2010, 2, 
                            ifelse(year==2011, 3, 
                                   ifelse(year==2012, 4, 
                                          ifelse(year==2013, 5, 
                                                 ifelse(year==2014, 6, 
                                                        ifelse(year==2015, 7, 
                                                               ifelse( year==2016 | year==2017, 8, 9))))))))]

setkey(data_SEM_tolls_3, year, month)
setkey(ship_size, year, month)
data_SEM_tolls_3 = ship_size[data_SEM_tolls_3]

data_SEM_tolls_3[, dummy_preinvest := ifelse(time_trend_monthly > 92, 1, 0)]
data_SEM_tolls_3[, season_2 := ifelse(month < 5 | month > 11, "dry", "wet")]


save.dta13(data_SEM_tolls_3, "regression_data/regression_data.dta")





