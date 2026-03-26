library(strucchange)
library(stargazer)
if (!require("pacman")) install.packages("pacman")


# Figure 1
data_SEM_tolls_3 = read.dta13("data_SEM_tolls_3_32219.dta")
data_SEM_tolls_3 = data.table(data_SEM_tolls_3)

data_SEM_fig=data_SEM_tolls_3[,.(date, tolls, tonnage, transits, monthly_lake_level_2)]
data_SEM_fig[, tolls                := tolls                - mean(tolls)]
data_SEM_fig[, tonnage              := tonnage              - mean(tonnage)]
data_SEM_fig[, transits             := transits             - mean(transits)]
data_SEM_fig[, monthly_lake_level_2 := monthly_lake_level_2 - mean(monthly_lake_level_2)]
data_SEM_fig = melt(data_SEM_fig, id="date")
data_SEM_fig[variable=="monthly_lake_level_2", variable := "Monthly lake level"]
data_SEM_fig[variable=="tolls", variable := "Toll revenues"]
data_SEM_fig[variable=="tonnage", variable := "Tonnage"]
data_SEM_fig[variable=="transits", variable := "Transits"]

data_SEM_fig[variable=="Monthly lake level", value := value * 100]

ggplot(data = data_SEM_fig[variable == "Transits" | variable == "Monthly lake level"], aes(x=date, y=value, group=variable, col=variable)) + 
  geom_line(size=2)+
  scale_y_continuous(sec.axis = sec_axis(~.*.01, name = "Monthly lake levels [m]"))+
  geom_vline( xintercept = as.numeric(data_SEM_tolls_3$date[93]), linetype=2) +
  scale_x_date(labels = date_format("%m-%Y"))+
  xlab("Date") +
  ylab("Transits") +
  theme_bw()+
  theme(axis.text    = element_text(size=24),
        axis.title   = element_text(size=24,face="bold"),
        axis.ticks.x = element_blank(),
        legend.position   = "bottom",
        legend.background = element_blank(), 
        legend.text       = element_text(size=24),
        legend.title      = element_blank(),
        legend.key.size   = unit(0.7,"cm"))



data_SEM_fig[variable=="Monthly lake level", value := value * .10]

ggplot(data = data_SEM_fig[variable == "Toll revenues" | variable == "Monthly lake level"], aes(x=date, y=value, group=variable, col=variable)) + 
  geom_line(size=2)+
  scale_y_continuous(sec.axis = sec_axis(~.*.1, name = "Monthly lake levels [m]"))+
  geom_vline( xintercept = as.numeric(data_SEM_tolls_3$date[93]), linetype=2) +
  scale_x_date(labels = date_format("%m-%Y"))+
  xlab("Date") +
  ylab("Tolls [$M]") +
  theme_bw()+
  theme(axis.text    = element_text(size=24),
        axis.title   = element_text(size=24,face="bold"),
        axis.ticks.x = element_blank(),
        legend.position   = "bottom",
        legend.background = element_blank(), 
        legend.text       = element_text(size=24),
        legend.title      = element_blank(),
        legend.key.size   = unit(0.7,"cm"))


ggplot(data = data_SEM_fig[variable == "Tonnage" | variable == "Monthly lake level"], aes(x=date, y=value, group=variable, col=variable)) + 
  geom_line(size=2)+
  scale_y_continuous(sec.axis = sec_axis(~.*.1, name = "Monthly lake levels [m]"))+
  geom_vline( xintercept = as.numeric(data_SEM_tolls_3$date[93]), linetype=2) +
  scale_x_date(labels = date_format("%m-%Y"))+
  xlab("Date") +
  ylab("Tons") +
  theme_bw()+
  theme(axis.text    = element_text(size=24),
        axis.title   = element_text(size=24,face="bold"),
        axis.ticks.x = element_blank(),
        legend.position   = "bottom",
        legend.background = element_blank(), 
        legend.text       = element_text(size=24),
        legend.title      = element_blank(),
        legend.key.size   = unit(0.7,"cm"))










# Figure 2
data_SEM_tolls_3 = read.dta13("data_SEM_tolls_3_32219.dta")
data_SEM_tolls_3 = data.table(data_SEM_tolls_3)

### tolls
dt = data_SEM_tolls_3[year(date)>2012]
structural_break_IV_tolls = list()
first_stage = lm(monthly_lake_level_2 ~ m_1_lagged_rainfall_lake + season, data = dt)
dt[, first_stage_fitted:= first_stage$fitted.values]

for (i in 1:nrow(dt)) {
  tryCatch({
    structural_break_IV_tolls[i] = sctest(tolls ~ first_stage_fitted + season, 
                                          data = dt, type = "Chow", point = i)
  }, error=function(e){})
  # print(i)
}

dates_IV = dt[(bar+1):(nrow(dt)-(bar+1)), .(year, month)]
dates_IV= dates_IV[complete.cases(year)]
structural_break_IV_tolls=unlist(structural_break_IV_tolls)
structural_break_IV_tolls=data.table(structural_break_IV_tolls)
structural_break_IV_tolls[, row:= (bar+1):(nrow(dt)-(bar+1))]
structural_break_IV_tolls= cbind(structural_break_IV_tolls, dates_IV)
structural_break_IV_tolls[, month := ifelse(month < 10, paste0("0", month), as.character(month))]
structural_break_IV_tolls[, date_X := as.Date(paste(month, "01", year, sep = "-"), "%m-%d-%Y")]

### transits
dt = data_SEM_tolls_3[year(date)>2012]
structural_break_IV_transits = list()
first_stage = lm(monthly_lake_level_2 ~ m_1_lagged_rainfall_lake + season, data = dt)
dt[, first_stage_fitted:= first_stage$fitted.values]

library(strucchange)
for (i in 1:nrow(dt)) {
  tryCatch({
    structural_break_IV_transits[i] = sctest(transits ~ first_stage_fitted + season, 
                                             data = dt, type = "Chow", point = i)
  }, error=function(e){})
  # print(i)
}


dates_IV = dt[(bar+1):(nrow(dt)-(bar+1)), .(year, month)]
dates_IV= dates_IV[complete.cases(year)]
structural_break_IV_transits=unlist(structural_break_IV_transits)
structural_break_IV_transits=data.table(structural_break_IV_transits)
structural_break_IV_transits[, row:= (bar+1):(nrow(dt)-(bar+1))]
structural_break_IV_transits= cbind(structural_break_IV_transits, dates_IV)
structural_break_IV_transits[, month := ifelse(month < 10, paste0("0", month), as.character(month))]
structural_break_IV_transits[, date_X := as.Date(paste(month, "01", year, sep = "-"), "%m-%d-%Y")]

### tonnage
dt = data_SEM_tolls_3[year(date)>2012]
structural_break_IV_tonnage = list()
first_stage = lm(monthly_lake_level_2 ~ m_1_lagged_rainfall_lake + season, data = dt)
dt[, first_stage_fitted:= first_stage$fitted.values]

library(strucchange)
for (i in 1:nrow(dt)) {
  tryCatch({
    structural_break_IV_tonnage[i] = sctest(tonnage ~ first_stage_fitted + season, 
                                            data = dt, type = "Chow", point = i)
  }, error=function(e){})
  # print(i)
}


dates_IV = dt[(bar+1):(nrow(dt)-(bar+1)), .(year, month)]
dates_IV= dates_IV[complete.cases(year)]
structural_break_IV_tonnage=unlist(structural_break_IV_tonnage)
structural_break_IV_tonnage=data.table(structural_break_IV_tonnage)
structural_break_IV_tonnage[, row:= (bar+1):(nrow(dt)-(bar+1))]
structural_break_IV_tonnage= cbind(structural_break_IV_tonnage, dates_IV)
structural_break_IV_tonnage[, month := ifelse(month < 10, paste0("0", month), as.character(month))]
structural_break_IV_tonnage[, date_X := as.Date(paste(month, "01", year, sep = "-"), "%m-%d-%Y")]


structural_break_IV=rbind(structural_break_IV_tonnage[,. (date_X, F_stat= structural_break_IV_tonnage,  group="Tonnage")], 
                          structural_break_IV_transits[,.(date_X, F_stat= structural_break_IV_transits, group="Transits")], 
                          structural_break_IV_tolls[,.   (date_X, F_stat= structural_break_IV_tolls,    group="Tolls")])

ggplot(data = structural_break_IV, aes(x=date_X, y=F_stat, group=group, col=group, shape=group))+
  geom_point(size=3)+
  geom_hline(yintercept = qf(.90,   3, 63)) +
  geom_hline(yintercept = qf(.95,   3, 63)) +
  geom_hline(yintercept = qf(.99,   3, 63)) +
  geom_vline(xintercept = as.numeric(data_SEM_tolls_3$date[95])) +
  theme_bw()+
  scale_x_date(labels = date_format("%m-%Y")) +
  xlab("Date") +
  ylab("F Statistic")+
  theme(axis.text      = element_text(size=20),
        axis.title   = element_text(size=20,face="bold"),
        axis.ticks.x = element_blank(),
        legend.position   = "bottom",
        legend.background = element_blank(),
        legend.text       = element_text(size=20),
        legend.title      = element_blank(),
        legend.key.size   = unit(0.7,"cm"))


# Figure 3

set.seed(123456)

library(stargazer)
library(gmm)
library(data.table)
library(ggplot2)

a = readstata13::read.dta13("data_SEM_tolls_3_32219.dta")
a = data.table(a)
a[, dummy_preinvest := ifelse(time_trend_monthly > 92, 1, 0)]

krCI <- function(mean, vcm, draws){
  vc <- vcm
  c <- chol(vc) #cholesky decomposition of variance-covariance 
  k <- length(mean)
  gamma <- rep(0, draws)
  for(j in 1:draws){
    rand <- rnorm(k, mean = 0, sd = 1)
    draw <- means + c %*% rand
    gamma[j] <- draw[2]/draw[1]
  }
  gamma <- sort(gamma)
  lb <- round(0.025*draws)
  ub <- round((1-0.025)*draws)
  ci <- c(gamma[lb], gamma[ub])
  return(list(ci, gamma))
  
}

draws= 100000

dt1_iv = data.table()
dt2_iv = data.table()
i=10
for (i in -25:50) {
  a[, dummy_preinvest_2 := dummy_preinvest]
  baz = lm(monthly_lake_level_2 ~ m_1_lagged_rainfall_lake + dummy_price_11 + dummy_price_13 + season, offset = dummy_preinvest_2, 
           data = a)
  
  a[, dummy_preinvest_2 := dummy_preinvest *i]
  a[, fitted_int := baz$fitted.values]
  foo = gmm(tolls ~ monthly_lake_level_2 + monthly_lake_level_2:dummy_preinvest + offset(dummy_preinvest_2) + dummy_price_11 + dummy_price_13 + season,
            ~ m_1_lagged_rainfall_lake       + fitted_int:dummy_preinvest + offset(dummy_preinvest_2) + dummy_price_11 + dummy_price_13 + season, 
            type = "twoStep", vcov = "HAC", kernel = "Bartlett", prewhite = T, wmatrix = "optimal",
            data = a)
  vcov_foo= data.table(names(coefficients(foo)), vcov(foo))
  
  foo = data.table(names(coefficients(foo)), data.table(summary(foo)$coefficients))
  
  means = c(foo[V1== "monthly_lake_level_2", Estimate], foo[V1== "monthly_lake_level_2:dummy_preinvest", Estimate])
  
  vc <- matrix(0, 2, 2)
  vc[1,1] <- vcov_foo[V1== "monthly_lake_level_2", monthly_lake_level_2]
  vc[1,2] <- vcov_foo[V1== "monthly_lake_level_2:dummy_preinvest", monthly_lake_level_2]
  vc[2,1] <- vcov_foo[V1== "monthly_lake_level_2:dummy_preinvest", monthly_lake_level_2]
  vc[2,2] <- vcov_foo[V1== "monthly_lake_level_2:dummy_preinvest", `monthly_lake_level_2:dummy_preinvest`]
  
  result <- krCI(means, vc, draws)
  result <- result[[1]]
  
  ratio = foo[V1== "monthly_lake_level_2:dummy_preinvest", Estimate]/foo[V1== "monthly_lake_level_2", Estimate]
  
  dt2_iv = data.table(expansion = i, ratio = ratio, lower = result[1], upper = result[2])
  dt1_iv= rbind(dt1_iv, dt2_iv)
  print(i)
}


ggplot(dt1_iv, aes(x=expansion, y=ratio))+
  geom_hline(yintercept = .014, linetype="dashed", col="red", size =2)+
  geom_hline(yintercept = .25,  linetype="dashed", col="red", size =2)+
  geom_errorbar(mapping=aes(x=expansion, ymin=lower, ymax=upper), width=0.2, size=1, color="grey") +
  geom_point(col="blue")+
  geom_line()+
  annotate("text", x=-27, y=0.04, label= "ρ=3%",  fontface =2, size =6) + 
  annotate("text", x=-27, y=0.28, label= "ρ=50%", fontface =2, size =6) + 
  theme_bw()+
  xlab("Expansion dummy")+
  ylab("Gamma")







#~~~~~~~~~~~~~~~~~~~~~~~~~
# Appendix


simpleCap <- function(x) { #            Function defined to make all caps to propper
  paste(substring(x, 1,1), tolower(substring(x, 2)),
        sep="")
}

# setwd("path to folder") # mac

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                  rainfall
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####  rainfall data
rain_15=read.dta13("acp_15Minstorms_Steve_v18112016.dta")
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

####  Station locations
stations = readOGR(dsn = "stations", layer = "stations")
stations_proj=stations@proj4string
stations = data.table(stations@data$CODENAME, stations@coords)
stations = unique(stations, by="V1")
setnames(stations, "V1", "station_name")
stations[,station_name := simpleCap(station_name)]
setkey(stations, station_name)
stations[station_name == "Tranquila", station_name := "Tranquilla"]
stations[station_name == "Pedromigel", station_name := "Pedromiguel"]
data.table(stations[,unique(station_name)])

setkey(rain_15, site)
rain_15= rain_15[stations]
rain_15= rain_15[complete.cases(total_rain_day)]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                  rainfall 2016, 2017
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rain_1617=read.csv("acp_2016_2017_2.csv")
rain_1617=data.table(rain_1617)
rain_1617=melt(rain_1617, id=c("year", "month"))
rain_1617=rain_1617[,.(variable, year, month, value)]

stations_1617=read.csv("stations_abb_full.csv")
stations_1617=data.table(stations_1617)

setkey(rain_1617, variable)
setkey(stations_1617, ID)
rain_1617=rain_1617[stations_1617]
rain_1617[, NOMBRE := simpleCap(NOMBRE)]
rain_1617=rain_1617[complete.cases(month)]
data.table(rain_1617[,unique(NOMBRE)])
rain_1617 = rain_1617[, NOMBRE := gsub(" ", "", NOMBRE)]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                  III. rainfall station regions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# these are based on the code called site_region_3_05042018. It creates categories of stations based on whether they are on the lake, on a catchment next to the lake, 
# or not neighboring the lake. These are labeled lake, within and far respectively.
site_region_3 = readRDS("site_region_3.rds")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                  IV. transit data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# the data is created as follows:
# PCW publishes bi-annual monthly data on transits, tonnage, long tons and they also used to publish revenues until 2014. After FY 2014, they stopped publishing revenues.
# Fortunately, we were able to obtain their data privately.
# Now, this data is not available at this point publically because they have updated the website. However, I was able to obtain this data from the web archives:
# https://web.archive.org/web/20161221153757/https://www.pancanal.com/eng/op/transit-stats/

transit = read.csv("transit_toll_data_03222019.csv")
transit=data.table(transit)
setkey(transit, year, month)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                   V. ship size + transit time
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ship_size = fread("monthly_transits_03222018.csv")
ship_size = ship_size[complete.cases(transit_under_91)]
ship_size = ship_size[,c(1:11)]


# Figure A.1
foo=copy(ship_size)
foo[, date := as.Date(paste(year, month, "01", sep = "-"))]
foo=foo[,.(date, CWT_Avg, TT_avg)]
foo= melt(foo, "date")
setkey(foo, date)
foo[variable == "CWT_Avg", variable := "Canal water time"]
foo[variable == "TT_avg",  variable := "Transit time"]

ggplot(foo, aes(x = date, y = value, group=variable, col=variable)) + 
  geom_line(size=1.2) +
  geom_vline(xintercept = as.numeric(foo$date[190]), linetype="dashed") +
  labs(x = "Date", y = "Time (hours)") +
  scale_x_date(labels = date_format("%m-%Y"))+
  theme_bw()+
  theme(axis.text      = element_text(size=20),
        axis.title   = element_text(size=20,face="bold"),
        axis.ticks.x = element_blank(),
        legend.position   = "bottom",
        legend.background = element_blank(),
        legend.text       = element_text(size=20),
        legend.title      = element_blank(),
        legend.key.size   = unit(0.7,"cm"))




# Figure A.2

### tolls
dt = data_SEM_tolls_3[year(date)>2012]

test_result_OLS_tolls= list()
for (i in 1:nrow(dt)) {
  tryCatch({
    test_result_OLS_tolls[i] = sctest(tolls ~ monthly_lake_level_2 + season, 
                                      data = dt, type = "Chow", point = i)
  }, error=function(e){})
}


date_result_OLS = dt[(bar+1):(nrow(dt)-(bar+1)), .(year, month)]
date_result_OLS = date_result_OLS[complete.cases(year)]

test_result_OLS_tolls = unlist(test_result_OLS_tolls)
test_result_OLS_tolls = data.table(test_result_OLS_tolls)
test_result_OLS_tolls[, row:= (bar+1):(nrow(dt)-(bar+1))]
test_result_OLS_tolls=  cbind(test_result_OLS_tolls, date_result_OLS)
test_result_OLS_tolls[, month  := ifelse(month < 10, paste0("0", month), as.character(month))]
test_result_OLS_tolls[, date_X := as.Date(paste(month, "01", year, sep = "-"), "%m-%d-%Y")]

### tonnage
dt = data_SEM_tolls_3[year(date)>2012]

test_result_OLS_tonnage= list()
for (i in 1:nrow(dt)) {
  tryCatch({
    test_result_OLS_tonnage[i] = sctest(tonnage ~ monthly_lake_level_2 + season, 
                                        data = dt, type = "Chow", point = i)
  }, error=function(e){})
}


date_result_OLS = dt[(bar+1):(nrow(dt)-(bar+1)), .(year, month)]
date_result_OLS = date_result_OLS[complete.cases(year)]

test_result_OLS_tonnage = unlist(test_result_OLS_tonnage)
test_result_OLS_tonnage = data.table(test_result_OLS_tonnage)
test_result_OLS_tonnage[, row:= (bar+1):(nrow(dt)-(bar+1))]
test_result_OLS_tonnage=  cbind(test_result_OLS_tonnage, date_result_OLS)
test_result_OLS_tonnage[, month  := ifelse(month < 10, paste0("0", month), as.character(month))]
test_result_OLS_tonnage[, date_X := as.Date(paste(month, "01", year, sep = "-"), "%m-%d-%Y")]

### transits
dt = data_SEM_tolls_3[year(date)>2012]

test_result_OLS_transits= list()
for (i in 1:nrow(dt)) {
  tryCatch({
    test_result_OLS_transits[i] = sctest(transits ~ monthly_lake_level_2 + season, 
                                         data = dt, type = "Chow", point = i)
  }, error=function(e){})
}


date_result_OLS = dt[(bar+1):(nrow(dt)-(bar+1)), .(year, month)]
date_result_OLS = date_result_OLS[complete.cases(year)]

test_result_OLS_transits = unlist(test_result_OLS_transits)
test_result_OLS_transits = data.table(test_result_OLS_transits)
test_result_OLS_transits[, row:= (bar+1):(nrow(dt)-(bar+1))]
test_result_OLS_transits=  cbind(test_result_OLS_transits, date_result_OLS)
test_result_OLS_transits[, month  := ifelse(month < 10, paste0("0", month), as.character(month))]
test_result_OLS_transits[, date_X := as.Date(paste(month, "01", year, sep = "-"), "%m-%d-%Y")]

structural_break_OLS=rbind(test_result_OLS_tonnage[,. (date_X, F_stat= test_result_OLS_tonnage,  group="Tonnage")], 
                           test_result_OLS_transits[,.(date_X, F_stat= test_result_OLS_transits, group="Transits")], 
                           test_result_OLS_tolls[,.   (date_X, F_stat= test_result_OLS_tolls,    group="Tolls")])

ggplot(data = structural_break_OLS, aes(x=date_X, y=F_stat, group=group, col=group, shape=group))+
  geom_point(size=3)+
  geom_hline(yintercept = qf(.90,   3, 63)) +
  geom_hline(yintercept = qf(.95,   3, 63)) +
  geom_hline(yintercept = qf(.99,   3, 63)) +
  geom_vline(xintercept = as.numeric(data_SEM_tolls_3$date[95])) +
  theme_bw()+
  scale_x_date(labels = date_format("%m-%Y")) +
  xlab("Date") +
  ylab("F Statistic")+
  theme(axis.text      = element_text(size=20),
        axis.title   = element_text(size=20,face="bold"),
        axis.ticks.x = element_blank(),
        # axis.title.x = element_text(margin=margin(0,0,0,0)),
        legend.position   = "bottom",
        # legend.box.margin = margin(c(-10,0,0,0)),
        legend.background = element_blank(),
        legend.text       = element_text(size=20),
        legend.title      = element_blank(),
        legend.key.size   = unit(0.7,"cm"))



# Figure A.3
dt = data_SEM_tolls_3[year(date)>2012]
test_result_FS= list()
for (i in 1:nrow(data_SEM_tolls_3)) {
  tryCatch({
    test_result_FS[i] = sctest(monthly_lake_level_2 ~ m_1_lagged_rainfall_lake + season, 
                               data = dt, type = "Chow", point = i)
  }, error=function(e){})
}


date_result_FS = dt[(bar+1):(nrow(dt)-(bar+1)), .(year, month)]
date_result_FS = date_result_FS[complete.cases(year)]

test_result_FS = unlist(test_result_FS)
test_result_FS = data.table(test_result_FS)
test_result_FS[, row:= (bar+1):(nrow(dt)-(bar+1))]
test_result_FS= cbind(test_result_FS, date_result_FS)
test_result_FS[, month := ifelse(month < 10, paste0("0", month), as.character(month))]
test_result_FS[, date_X := as.Date(paste(month, "01", year, sep = "-"), "%m-%d-%Y")]

ggplot(data = test_result_FS, aes(x=date_X, y=test_result_FS))+
  geom_point()+
  geom_hline(yintercept = qf(.90,   3, 63)) +
  geom_hline(yintercept = qf(.95,   3, 63)) +
  geom_hline(yintercept = qf(.99,   3, 63)) +
  geom_vline(xintercept = as.numeric(data_SEM_tolls_3$date[95])) +
  theme_bw()+
  scale_x_date(labels = date_format("%m-%Y")) +
  xlab("Date") +
  ylab("F Statistic")+
  theme(axis.text      = element_text(size=20),
        axis.title   = element_text(size=20,face="bold"),
        axis.ticks.x = element_blank(),
        # axis.title.x = element_text(margin=margin(0,0,0,0)),
        legend.position   = "bottom",
        # legend.box.margin = margin(c(-10,0,0,0)),
        legend.background = element_blank(),
        legend.text       = element_text(size=20),
        legend.title      = element_blank(),
        legend.key.size   = unit(0.7,"cm"))



# Figure A.4
foo=copy(ship_size)
foo[, date := as.Date(paste(year, month, "01", sep = "-"))]
foo=foo[,.(date, transit_under_91, transit_over_91)]
foo= melt(foo, "date")
setkey(foo, date)
foo[variable == "transit_under_91", variable := "Under 91 ft"]
foo[variable == "transit_over_91",  variable := "Over 91 ft"]

ggplot(foo, aes(x = date, y = value, group=variable, col=variable)) + 
  geom_line(size=1.2) +
  geom_vline(xintercept = as.numeric(foo$date[190]), linetype="dashed") +
  labs(x = "Date", y = "Number of transits") +
  scale_x_date(labels = date_format("%m-%Y"))+
  theme_bw()+
  theme(axis.text      = element_text(size=20),
        axis.title   = element_text(size=20,face="bold"),
        axis.ticks.x = element_blank(),
        legend.position   = "bottom",
        legend.background = element_blank(),
        legend.text       = element_text(size=20),
        legend.title      = element_blank(),
        legend.key.size   = unit(0.7,"cm"))



# Figure A.5
a = fread("gatun_elev.csv")
a[, date := as.Date(date, "%m/%d/%y")]
as.Date(ifelse(d > "2012-12-31", format(d, "19%y-%m-%d"), format(d)))
a[, date := as.Date(ifelse(date  > "2021-01-01", format(date, "19%y-%m-%d"), format(date)))]
a[, month := month(date)]
a[, year  := year(date)]
a[, day   := as.character(date)]
a[, day   := substr(day, 9, 10)]
a[, day   := paste(month, day, sep = "-")]
a = a[year > 2004]
a[, lag_lake_level := c(NA, lake_level_ft[-.N])]
a[, diff := lake_level_ft - lag_lake_level]
a = a[complete.cases(diff)]

ggplot(a, aes(x=date, y=diff))+
  geom_point()+
  theme_bw()+
  ylab("Difference in elevation (ft)")+
  theme(axis.text=element_text(size =16,face="bold"),
        axis.title=element_text(size=16,face="bold"),
        axis.title.y=element_text(margin=margin(0,10,0,0)))

