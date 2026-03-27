#This scripts generates all of the numbers for the Summary Table (Table 1)
rm(list = ls())

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

county_data = merge(county_data_X,county_data_T1,by="user_loc",all=TRUE)

# Data containing the movement variables
facebook_data = read.dta13("../Data/fb_movement_data.dta")
colnames(facebook_data)=c("user_loc","county",   "movement_ch",    "stay_home", "date") 
facebook_data$user_loc=as.numeric(facebook_data$user_loc)

# Population in each county in 2019
county_pop2019 = read.dta13("../Data/county_pop2019.dta")
colnames(county_pop2019) = c("popestimate2019", "user_loc")  
county_pop2019$user_loc=as.numeric(county_pop2019$user_loc)

# Covid data at county level
covid_counties = read.csv("../Data/us-counties.csv")
colnames(covid_counties) = c("date",   "county", "state",  "user_loc",   "cases",  "deaths")
covid_counties$user_loc = as.numeric(covid_counties$user_loc)

# A few covariates for DPL regressions
county_covariates = read.dta13("../Data/county_covariates.dta")
names(county_covariates)[names(county_covariates) == 'fips'] <- 'user_loc'

for (var in c("white","asian","islander","raceother","pop18_24","p18_24nohs",
              "p18_24hs","p18_24somecoll","p18_24bacc","g25","nohsg25","nodegreeg25","hsg25",
              "somecollg25","assocg25","baccg25","graduatedegreeg25", "g65","hsg65","baccorhigherg65",
              "povmale","povfemale","povwhite","povindig","povasian", 
              "povislander","povraceother","povsomecollege","povbaccorhigher","utotal"
)){
  county_covariates[[paste0("prop",var)]]=county_covariates[[var]]/county_covariates$population
}

covariates = colnames(county_covariates)[grepl("prop", colnames(county_covariates), fixed=TRUE)]



# Merge data
data = merge(county_data,county_pop2019,by = "user_loc")

data = merge(data,covid_counties,by = "user_loc",all.x=TRUE)
data$date = as.Date(data$date, origin = "1960-01-01") 

data = merge(data,facebook_data,by=c("user_loc","date"),all=TRUE)


data = merge(data, county_covariates, by = "user_loc",all.x=TRUE)

## We also add election data to our data frame

Election_data = read.dta13("../Data/Election2020.dta")

Election_data=Election_data[,c("fips2","per_gop","per_dem")]
colnames(Election_data)=c("user_loc","per_gop","per_dem")


data = merge(data,Election_data,by="user_loc",all.x=TRUE)



# We keep only recent data
data = data %>% filter(date >= "2020-10-01") 


## Creation of baseline variables

data$leave_home = 1 - data$stay_home



data$leave_home = 100*data$leave_home
data$movement_ch = 100*data$movement_ch

data = data %>%
  group_by(user_loc) %>% 
  mutate(
    baseline_th_leave_home=ifelse(as.numeric(as.Date("2020/11/13")) %in% as.numeric(date),leave_home[as.numeric(date)==as.numeric(as.Date("2020/11/13"))],NA),
    baseline_th_movement_ch=ifelse(as.numeric(as.Date("2020/11/13")) %in% as.numeric(date),movement_ch[as.numeric(date)==as.numeric(as.Date("2020/11/13"))],NA),

    baseline_th_cases = ifelse(as.numeric(as.Date("2020/11/13")) %in% as.numeric(date),cases[as.numeric(date)==as.numeric(as.Date("2020/11/13"))],NA),
    baseline_th_deaths = ifelse(as.numeric(as.Date("2020/11/13")) %in% as.numeric(date),deaths[as.numeric(date)==as.numeric(as.Date("2020/11/13"))],NA),
    
    pre_baseline_th_cases= ifelse(as.numeric(as.Date("2020/10/31")) %in% as.numeric(date),cases[as.numeric(date)==as.numeric(as.Date("2020/10/31"))],NA),
    pre_baseline_th_deaths= ifelse(as.numeric(as.Date("2020/10/31")) %in% as.numeric(date),deaths[as.numeric(date)==as.numeric(as.Date("2020/10/31"))],NA),
  )


data$missing_baseline_leave_home = is.na(data$baseline_th_leave_home)
data$missing_baseline_movement = is.na(data$baseline_th_movement_ch)

data$bl_fortnightly_cases = data$baseline_th_cases-data$pre_baseline_th_cases
data$bl_fortnightly_deaths = data$baseline_th_deaths-data$pre_baseline_th_deaths


variables_for_table =  c("popestimate2019",     "baseline_th_movement_ch"   ,    
  "baseline_th_leave_home" ,  "bl_fortnightly_cases","bl_fortnightly_deaths"   ,"missing_baseline_movement","share_urban","per_dem","per_gop")


labels = c("Population in 2019","Baseline Movement Metric","Baseline Leave Home","Baseline Fortnightly Cases","Baseline Fortnightly Deaths","Missing Baseline Facebook outcomes",
           "Share Urban","Share Democrats","Share Republicans")

###
# Thanksgiving sample
###

data_table = data %>% filter(!is.na(high_county_T1))
data_table = data_table[,c("user_loc","high_county_T1",variables_for_table)]
data_table =  distinct(data_table)

panels = list(data_table,data_table[data_table$high_county_T1==1,],data_table[data_table$high_county_T1==0,])


#Generation of the summary table:

lines = c()
i=1
for (panel in panels){
  lines = c(lines,nrow(panel))
  for (var in variables_for_table){
# for continuous variable, we compute mean and standard deviation
      lines = c(lines,paste0(format(round(mean(panel[[var]],na.rm=TRUE), digits=2), nsmall = 2)," (",format(round(sd(panel[[var]],na.rm=TRUE), digits=2), nsmall = 2),")"))
    
  }
  
  i=i+1
}

tab = matrix(lines, ncol=3)

colnames(tab) <- c("Study", "Treatment",  "Control")
rownames(tab) = c("Nb observations",labels)
write.csv(tab, paste0("../Output/Summary_Table/summary_table_T1.csv"))

###
# Christmas sample
###

data_table = data %>% filter(!is.na(high_county_X)) 
data_table = data_table[,c("user_loc","high_county_X",variables_for_table)]
data_table =  distinct(data_table)

panels = list(data_table,data_table[data_table$high_county_X==1,],data_table[data_table$high_county_X==0,])


#Generation of the summary table:

lines = c()
i=1
for (panel in panels){
  lines = c(lines,nrow(panel))
  for (var in variables_for_table){
    # for continuous variable, we compute mean and standard deviation
    lines = c(lines,paste0(format(round(mean(panel[[var]],na.rm=TRUE), digits=2), nsmall = 2)," (",format(round(sd(panel[[var]],na.rm=TRUE), digits=2), nsmall = 2),")"))
    
  }
  
  i=i+1
}



tab = matrix(lines, ncol=3)

colnames(tab) <- c("Study", "Treatment",  "Control")
rownames(tab) = c("Nb observations",labels)
write.csv(tab, paste0("../Output/Summary_Table/summary_table_X.csv"))






