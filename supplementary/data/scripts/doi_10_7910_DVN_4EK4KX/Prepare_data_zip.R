#This script prepares data for the zip level Covid-19 analysis: data import, general pre-processing, creation of county and zip level variables

rm(list = ls())


set.seed(789917873)

## Import Covid data (zip level)
covid_zip = read.csv("../Data/clean_cases.csv") # colnames = "state"  "zip"    "date"   "t"      "cases"  "change"
dates = c(20201112,20201116,20201119,20201123,20201126,20201130,20201203,20201207,20201210,20201214,20201217,20201221,20201224,20201228,20201231,20210104,20210107,20210111,20210114,
          20210118,20210121,20210125,20210128,20210201,20210204,20210208,20210211,20210215,20210218,20210222)

for (i in 1:30){
  covid_zip$date[covid_zip$t==i]=dates[i]
}

covid_zip$date =  as.character(covid_zip$date)
covid_zip$date =as.Date(covid_zip$date,  format = "%Y%m%d") 

covid_zip$cases=as.numeric(covid_zip$cases)
## Virginia has no data on "2021-01-04" so we fill it with 0 to avoid any problem
Virginia_zips = unique(covid_zip$zip[covid_zip$state=="VA"])
Virginia_zips = Virginia_zips[!is.na(Virginia_zips )]

for (z in Virginia_zips){
  temp = covid_zip %>% filter(date =="2020-12-31") %>% filter(zip==z)
  c = temp$cases
  covid_zip[nrow(covid_zip) + 1,] = c("VA",z,"2021-01-04",16,c,0)
}

##

# Import treatment data for both campaigns 

thanksgiving_data = read_excel("../Data/randomized_sample_thanksgiving.xlsx")

thanksgiving_data = thanksgiving_data %>% group_by(county) %>% mutate(
  
  share_urban = mean(urban)
  
)

thanksgiving_data = thanksgiving_data[,c("county","high_county","zip","treat","share_urban","urban")]
colnames(thanksgiving_data)=c("user_loc","high_county_T1","zip","treated_T1","share_urban","urban")

christmas_data = read_excel("../Data/randomized_sample_christmas.xlsx")
christmas_data = christmas_data[,c("fips","high_county","zip","treat")]
colnames(christmas_data)=c("user_loc","high_county_X","zip","treated_X")


# Thanksgiving zips 
data = merge(covid_zip,thanksgiving_data ,by=c("zip"))
data = merge(data,christmas_data ,by=c("zip","user_loc"),all.x=TRUE)




# Population data
county_pop2019 = read.dta13("../Data/county_pop2019.dta")
colnames(county_pop2019) = c("popestimate2019", "user_loc")  
county_pop2019$user_loc=as.numeric(county_pop2019$user_loc)

data = merge(data,county_pop2019,by="user_loc")

data$log_pop = log(data$popestimate2019)


# A few county covariates
county_covariates = read.dta13("../Data/county_covariates.dta")
names(county_covariates)[names(county_covariates) == 'fips'] <- 'user_loc'

# Transformation into proportion variables (e.g number of white people in county -> proportion of white people in county)
for (var in c("white","asian","islander","raceother","pop18_24","p18_24nohs",
              "p18_24hs","p18_24somecoll","p18_24bacc","g25","nohsg25","nodegreeg25","hsg25",
              "somecollg25","assocg25","baccg25","graduatedegreeg25", "g65","hsg65","baccorhigherg65",
              "povmale","povfemale","povwhite","povindig","povasian", 
              "povislander","povraceother","povsomecollege","povbaccorhigher","utotal"
)){
  county_covariates[[paste0("prop",var)]]=county_covariates[[var]]/county_covariates$population
}


covariates_list = colnames(county_covariates)[grepl("prop", colnames(county_covariates), fixed=TRUE)]
county_covariates = county_covariates[,c("user_loc",covariates_list)]

covariates_list = c(covariates_list,"log_pop")

county_covariates$high_education =as.numeric(county_covariates$prophsg25 > median(county_covariates$prophsg25 ,na.rm=TRUE))

data= merge(data,county_covariates,by="user_loc",all.x=TRUE)

data = data[order(data$date),] # we have to order the data before we define the next variables

# Data includes some rare negative cases (i.e cumulative cases are locally decreasing)
# We correct them with a linear smoothing between dates t-2 and t+2 (if t is the negative jump date)

# Detail of the correction method: let (c(i))_i be the time series of cumulative cases in a given zip
# For each date t, if t is such that "c(t) < c(t-1)":
# - Replace c(i) (i between t-2 and t+2) with: c(t-2) + (c(t+2)-c(t-2))*(i-t+2)/4 


correct_errors <- function(time_series,k){
  n = length(time_series)
  if (n-2*k-1>0){
    for (i in ((k+1):(n-k))){
      current_value = time_series[i]
      previous_value = time_series[i-1]
      if ((!is.na(current_value))*!(is.na(previous_value))){
        if (current_value< previous_value){
          
          
          time_series[(i-k):(i+k)]=linear_replacement(time_series[(i-k):(i+k)],k)
        }
      }
    }
    return(time_series)
  }else{return(time_series)}
}

linear_replacement <- function(time_series,k){
  
  start = time_series[1]
  end = time_series[1+2*k]
  result = c()
  for (i in 1:(1+2*k)){
    result = c(result,start + (end-start)*(i-1)/(2*k))
  }
  return(result)
}

data$cases=as.numeric(data$cases)

# Zip cases
data <- data %>%
  group_by(zip) %>%
  mutate(

    corrected_cases_2 = round(correct_errors(cases,2)), 
    corrected_change_2 = diff(c(NA,corrected_cases_2), lag = 1, differences = 1,na.rm=T),
  ) 

data$corrected_change_2[data$corrected_change_2<0]=0

data <- data %>%
  group_by(zip) %>%
  mutate(
    two_weeks_cases = apply(embed(c(rep(NA,3),corrected_change_2),4),1,sum),
  ) 


data$two_weeks_cases_half_min = data$two_weeks_cases
data$two_weeks_cases_half_min[data$two_weeks_cases_half_min ==0]=0.5

data$two_weeks_cases_zeros_omitted= data$two_weeks_cases
data$two_weeks_cases_zeros_omitted[data$two_weeks_cases_zeros_omitted ==0]=NA

# Definition of the outcomes
data$asinh_two_weeks_cases = asinh(data$two_weeks_cases)
data$log_two_weeks_cases_plus_1 = log(data$two_weeks_cases+1) 
data$log_two_weeks_cases_half_min = log(data$two_weeks_cases_half_min) 
data$log_two_weeks_cases_zeros_omitted = log(data$two_weeks_cases_zeros_omitted)

data$cases[data$cases==0]=0.5
data$log_cases = log(data$cases)

data = data %>%
  group_by(zip) %>% 
  mutate(
  baseline_th_log_cases = ifelse(as.numeric(as.Date("2020/11/12")) %in% as.numeric(date),log_cases[as.numeric(date)==as.numeric(as.Date("2020/11/12"))],NA),
    baseline_ch_log_cases = ifelse(as.numeric(as.Date("2020/12/14")) %in% as.numeric(date),log_cases[as.numeric(date)==as.numeric(as.Date("2020/12/14"))],NA),
  baseline_th_cases = ifelse(as.numeric(as.Date("2020/11/12")) %in% as.numeric(date),cases[as.numeric(date)==as.numeric(as.Date("2020/11/12"))],NA),
  baseline_ch_cases = ifelse(as.numeric(as.Date("2020/12/14")) %in% as.numeric(date),cases[as.numeric(date)==as.numeric(as.Date("2020/12/14"))],NA),
)



Election_data = read.dta13("../Data/Election2020.dta")

Election_data=Election_data[,c("fips2","per_gop","per_dem")]
colnames(Election_data)=c("user_loc","per_gop","per_dem")


data = merge(data,Election_data,by="user_loc",all.x=TRUE)

# Data containing the movement variables
facebook_data = read.dta13("../Data/fb_movement_data.dta")
colnames(facebook_data)=c("user_loc","county",   "movement_ch",    "stay_home", "date")
facebook_data$user_loc=as.numeric(facebook_data$user_loc)
facebook_data = facebook_data %>% filter(date >= "2020-10-01")


data = merge(data,facebook_data,by=c("user_loc","date"),all=TRUE)


data$leave_home = 1 - data$stay_home


# Add some mobility variables for heterogeneity analysis
temp =  distinct(data[,c("user_loc","movement_ch","leave_home","date","propurban")])
temp = temp %>% filter(date >= "2020/10/31") %>% filter(date <= "2020/11/13")

temp = temp %>% group_by(user_loc) %>% mutate(

  bl_movement = mean(movement_ch),
  bl_leave_home = mean(leave_home),
)

temp$high_movement = as.numeric(temp$bl_movement> median(temp$bl_movement ,na.rm=TRUE))
temp$high_leave_home = as.numeric(temp$bl_leave_home > median(temp$bl_leave_home ,na.rm=TRUE))


temp = unique(temp[,c("user_loc","high_movement","high_leave_home")])

data = merge(data,temp,by="user_loc",all.x=TRUE)

data$majority_gop = as.numeric(data$per_gop>data$per_dem)



