#######
#######
####### Replication Data for: The Differential Impact of the Hong Kong National Security Law 
####### on Political Sensitivity Bias in Opinion Polls.
####### This file produces the merged data in the paper and appendix.
####### Last Updated: May. 2023
#######
#######

#setup
rm(list = ls()) # clear workspace
setwd(gsub("/64","",dirname(rstudioapi::getSourceEditorContext()$path)))
list.files()
######## PACKAGES ########

# Check system and installs packages user doesn't have, load needed packages
# install.packages("remotes")
#remotes::install_github("grantmcdermott/ggiplot") #install ggiplot
  #devtools::install_github("synth-inference/synthdid")
need <- c("foreign", "readstata13", "tidyverse", "estimatr", "csvy","synthdid", "modelsummary",
          "devtools", "dplyr", "tidyr", "lubridate","fixest", "ggiplot") # list packages needed
have <- need %in% rownames(installed.packages()) # checks packages you have
if(any(!have)) install.packages(need[!have]) # install missing packages
invisible(lapply(need, library, character.only=T)) # load needed packages

##########################merge treated data with control##################
#code treated unit period
load('cleaned data/64_students.Rdata')  #baseline treated unit

a <- subset(a, select=c(Date, proBeijing, sensitivity))
a$name <- "64_students"
JF_students <- a
rm(a)
  Treatment.date=as.Date("2020-06-30")         #set treatment date
  JF_students$Date = as.Date(JF_students$Date)
  JF_students$treated <- ifelse(JF_students$Date>Treatment.date,1,0)
  a <- 1
  while (JF_students$Date[a]>Treatment.date) {
    a=a+1
  }
  JF_students$period <- a - seq_len(nrow(JF_students))
cat("Treated unit:","64_students","\nTtr=",a-1,"\nTun=",a-nrow(JF_students)+1)
JF_students <- JF_students[1:23,]          #code start date=2000, don't execute if want full list
merge <- JF_students

#function to code period   NOTE: MANUALLY CHANGE THE TIME PERIODS!
code_period <- function(data) {
  data$period <- NA
  closest_indices <- integer(nrow(JF_students))
  
  for (i in 1:nrow(JF_students)) {
    ref_date <- JF_students$Date[i]
    ref_period <- JF_students$period[i]
    
    # Calculate the absolute difference between the ref_date and the dates in the data
    date_diffs <- abs(as.numeric(data$Date - ref_date))
    
    # Find the index of the minimum difference
    min_diff_index <- which.min(date_diffs)
    
    # Store the index of the minimum difference
    closest_indices[i] <- min_diff_index
  }
  
  # Create a separate data frame with the closest dates and their corresponding periods
  closest_data <- data[closest_indices,]
  closest_data$period <- JF_students$period
  
  return(closest_data)
}

#end function

#code other treated units
load('cleaned data/64_CHN_gov.Rdata')
a <- subset(a, select=c(Date, proBeijing, sensitivity))
a$name <- "64_CHN_gov"
a$treated <- 0
JF_CHN_gov <- code_period(a)
JF_CHN_gov$treated[JF_CHN_gov$period>0] <- 1
merge <- rbind(merge, JF_CHN_gov)
load('cleaned data/64_reverse.Rdata')
a <- subset(a, select=c(Date, proBeijing, sensitivity))
a$name <- "64_reverse"
a$treated <- 0
JF_reverse <- code_period(a)
JF_reverse$treated[JF_reverse$period>0] <- 1
merge <- rbind(merge, JF_reverse)

treat_list <- c("64_students","64_CHN_gov","64_reverse")

#load and code donor pool: medium and low sensitivity only
data.list <- gsub(".Rdata","",list.files("cleaned data",pattern = "\\.Rdata$"))
for (name in data.list){
  load (paste0("cleaned data/",name,".Rdata"))
  if (a$sensitivity[1]=="high"| a$sensitivity[1]=="extremely high"| a$Date[nrow(a)]>JF_students$Date[nrow(JF_students)]){next} #this version might exclude a lot of later
                                                                                                                              #introduced poll if the initial date is set early
  a <- subset(a, select=c(Date, proBeijing, sensitivity))
  a$name <- name
  a$treated <- 0
  a <- code_period(a)
  merge <- rbind(merge, a)
}
save(JF_CHN_gov, JF_reverse, JF_students,merge,treat_list, file = "64/merged.Rdata")
#########################merge ends#####################################
