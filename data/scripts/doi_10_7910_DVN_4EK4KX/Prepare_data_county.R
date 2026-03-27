#This script prepares data for the county level mobility analysis: data import, general pre-processing, creation of county level variables

rm(list = ls())

######
## Import data
######

# Christmas campaign data

county_data_X = read_excel("../Data/randomized_sample_christmas.xlsx")
county_data_X= distinct(county_data_X[,c("fips","high_county")])
colnames(county_data_X)=c("user_loc","high_county_X")

# Thanksgiving campaign data

county_data_T1 = read_excel("../Data/randomized_sample_thanksgiving.xlsx")
county_data_T1 = county_data_T1 %>% group_by(county) %>% mutate(
  share_urban = mean(urban)
)
county_data_T1= distinct(county_data_T1[,c("county","high_county","share_urban")])
colnames(county_data_T1)=c("user_loc","high_county_T1","share_urban") 

# Merge both data sets

data=merge(county_data_X,county_data_T1,by="user_loc",all=TRUE)


# Add population in each county in 2019

county_pop2019 = read.dta13("../Data/county_pop2019.dta")
colnames(county_pop2019) = c("popestimate2019", "user_loc")  
county_pop2019$user_loc=as.numeric(county_pop2019$user_loc)

data = merge(data,county_pop2019,by="user_loc")



# Import Covid-19 data at county level

covid_counties = read.csv("../Data/us-counties.csv")
colnames(covid_counties) = c("date",   "county", "state",  "user_loc",   "cases",  "deaths")
covid_counties$user_loc = as.numeric(covid_counties$user_loc)
covid_counties$date = as.Date(covid_counties$date, origin = "1960-01-01") 
covid_counties = covid_counties %>%  filter(date >= "2020-10-01")

covid_counties$state[covid_counties$state=="Rhode Island"]="Rhode_Island"
covid_counties$state[covid_counties$state=="South Dakota"]="South_Dakota"
covid_counties$state[covid_counties$state=="North Carolina"]="North_Carolina"

data = merge(data,covid_counties,by = "user_loc",all.x=TRUE)
 

# Add a few control variables for the regressions

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


covariates_list_no_FE = colnames(county_covariates)[grepl("prop", colnames(county_covariates), fixed=TRUE)]
county_covariates = county_covariates[,c("user_loc",covariates_list_no_FE)]

# Build high education variable for heterogeneity analysis
county_covariates$high_education =as.numeric(county_covariates$prophsg25 > median(county_covariates$prophsg25 ,na.rm=TRUE)) 

data$log_pop = log(data$popestimate2019) #log(county population)

# Addition of log(population) in the control variables
covariates_list_no_FE = c(covariates_list_no_FE,"log_pop")

# Transform state variable into dummies for DPL controls
state_values = unique(data$state)

covariates_list =covariates_list_no_FE
for (s in state_values){
  data[[paste0("state_",s)]]=as.numeric(data$state==s)
  covariates_list = c(covariates_list,paste0("state_",s))
}
covariates_list=covariates_list[covariates_list!="state_NA"]

# Add regions in the controls
data = data %>% mutate(
  
  region1 = state_Maine+state_Rhode_Island,
  region2 = state_Illinois+state_Indiana+state_Minnesota,
  region3 = state_Florida+state_Maryland+state_North_Carolina+state_Virginia+state_Arkansas+state_Oklahoma,
  region4 = state_Arizona+state_Oregon
)

covariates_list=c(covariates_list,"region1","region2","region3","region4")

data= merge(data,county_covariates,by="user_loc",all.x=TRUE)

# Add movement variables
facebook_data = read.dta13("../Data/fb_movement_data.dta")
colnames(facebook_data)=c("user_loc","county",   "movement_ch",    "stay_home", "date") 
facebook_data$user_loc=as.numeric(facebook_data$user_loc)
facebook_data = facebook_data %>% filter(date >= "2020-10-01")


data = merge(data,facebook_data,by=c("user_loc","date"),all=TRUE)

## Define "Share Ever Left Home" outcome
data$leave_home = 1 - data$stay_home

data = data[order(data$date),] # we have to order the data before we define the next variables

## Creation of baseline variables (pre-thanksgiving baseline and pre-christmas baseline)

# Baseline movement
data = data %>%
  group_by(user_loc) %>% 
  mutate(
    baseline_th_leave_home=ifelse(as.numeric(as.Date("2020/11/13")) %in% as.numeric(date),leave_home[as.numeric(date)==as.numeric(as.Date("2020/11/13"))],NA),
    baseline_ch_leave_home=ifelse(as.numeric(as.Date("2020/12/13")) %in% as.numeric(date),leave_home[as.numeric(date)==as.numeric(as.Date("2020/12/13"))],NA),
    baseline_th_movement_ch=ifelse(as.numeric(as.Date("2020/11/13")) %in% as.numeric(date),movement_ch[as.numeric(date)==as.numeric(as.Date("2020/11/13"))],NA),
    baseline_ch_movement_ch=ifelse(as.numeric(as.Date("2020/12/13")) %in% as.numeric(date),movement_ch[as.numeric(date)==as.numeric(as.Date("2020/12/13"))],NA),

  )

# Baseline cases
data = data %>%
  group_by(user_loc) %>% 
  mutate(

    baseline_th_cumulative_cases=ifelse(as.numeric(as.Date("2020/11/12")) %in% as.numeric(date),cases[as.numeric(date)==as.numeric(as.Date("2020/11/12"))],NA),
    baseline_ch_cumulative_cases=ifelse(as.numeric(as.Date("2020/12/14")) %in% as.numeric(date),cases[as.numeric(date)==as.numeric(as.Date("2020/12/14"))],NA),

    
  )


# convert outcomes into percentage
data$leave_home = 100*data$leave_home
data$movement_ch = 100*data$movement_ch #movement_ch = "Mobility Relative to February 2020" outcome

# Add election data

Election_data = read.dta13("../Data/Election2020.dta")
Election_data=Election_data[,c("fips2","per_gop","per_dem")]
colnames(Election_data)=c("user_loc","per_gop","per_dem")

data = merge(data,Election_data,by="user_loc",all.x=TRUE)


# Create some covid variables for heterogeneity analysis

data$baseline_th_cumulative_cases_per_capita = data$baseline_th_cumulative_cases - data$log_pop
data$baseline_ch_cumulative_cases_per_capita = data$baseline_ch_cumulative_cases - data$log_pop

temp =  unique(data[,c("user_loc","baseline_th_cumulative_cases_per_capita","baseline_ch_cumulative_cases_per_capita")])

# Baseline infection rates per capita
temp$high_infection_rate_th = as.numeric(temp$baseline_th_cumulative_cases_per_capita > median(temp$baseline_th_cumulative_cases_per_capita ,na.rm=TRUE))
temp$high_infection_rate_ch = as.numeric(temp$baseline_ch_cumulative_cases_per_capita > median(temp$baseline_ch_cumulative_cases_per_capita,na.rm=TRUE))

temp = temp[,c("user_loc","high_infection_rate_th","high_infection_rate_ch")]

data = merge(data,temp,by="user_loc",all.x=TRUE)


# Add some mobility variables for heterogeneity analysis (baseline movement)
temp =  data[,c("user_loc","movement_ch","leave_home","date")]
temp = temp %>% filter(date >= "2020/10/31") %>% filter(date <= "2020/11/13")

temp = temp %>% group_by(user_loc) %>% mutate(
  
  bl_movement = mean(movement_ch),
  bl_leave_home = mean(leave_home),
)

# High baseline movement variables
temp$high_movement = as.numeric(temp$bl_movement> median(temp$bl_movement ,na.rm=TRUE))
temp$high_leave_home = as.numeric(temp$bl_leave_home > median(temp$bl_leave_home ,na.rm=TRUE))

temp = unique(temp[,c("user_loc","high_movement","high_leave_home")])


data = merge(data,temp,by="user_loc",all.x=TRUE)

