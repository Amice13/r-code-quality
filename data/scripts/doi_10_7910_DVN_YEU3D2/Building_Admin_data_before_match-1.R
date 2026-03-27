  rm(list=ls())
  setwd("w:/02192835/incoming/maya_hisachon_chiild")
  

  (gc)
  
  library(stargazer)
  library(lubridate)
  library(ggplot2)
  library(reshape2)
  library(dplyr)
  library(scales)
  library(pastecs)
  library(Hmisc)
  #library(aod)
  library(MASS)
  library(plyr)
  library(foreign)
  library(rio)
  library(data.table)
  library(Matching)
  library(MatchIt)
  library(rmarkdown)
  library(dplyr)
  library(textclean)
  library(substr)
  library(lubridate)
  
  
  gc()
  list.files()
  

  total <- read.csv(file="C:/Users/02192835/Documents/new_hisachon",
                       header = TRUE,sep=",")

  
  save(total,file="total.R")                 
  
  seker_data <- read.csv(file="W:/02192835/incoming/maya_hisachon_chiild/new_hisachon15.10.18",
                       header = TRUE,sep=",")
  save(seker_data,file="seker_data.R")   
  
   load("total.R")
   load("seker_data.R")
  
  


  total <-data.table(total)
  

  total$father_tar_idkun_022017_date <- 
    as.Date(total$father_tar_idkun_022017,
            format="%d/%m/%Y")
  
  
  total$choose_date_date <- 
    as.Date(total$choose_date,
            format="%d/%m/%Y")
  
  total$choose_date_date1 <- total$choose_date_date
  total$choose_date_date1[is.na(total$choose_date_date1)] <- "1899-12-30"
  
  
  total$mother_tar_idkun_022017_date <- 
    as.Date(total$mother_tar_idkun_022017,
            format="%d/%m/%Y")
  
  ##date of mobile text message
  
  total$zman_father_feb1 <- as.POSIXct(total$father_zman_idkun_022017,format="%d/%m/%Y-%H:%M:%S")
  total$zman_mother_feb1 <- as.POSIXct(total$mother_zman_idkun_022017,format="%d/%m/%Y-%H:%M:%S")
  total$zman_idkun_022017 <- with (total, pmin(zman_mother_feb1,zman_father_feb1, na.rm=TRUE))
  total$zman_idkun_022017[is.na(total$zman_idkun_022017)] <- "1899-12-30"
  

###start cleaning data and building variables
  
  total$child_with_disability1 <- total$child_with_disability
  total$child_with_disability1[is.na(total$child_with_disability)] <- 0
  
                ##make variables for receiving sms
  
    ## both mother and father can receive sms - but mostly father received
    ###either one parent received SMS - minimun of both 
  total$tar_idkun_022017 <- with (total, pmin(mother_tar_idkun_022017_date,father_tar_idkun_022017_date, na.rm=TRUE))
  total$tar_idkun_022017_yes=ifelse(total$tar_idkun_022017>0,1,0)
  total$tar_idkun_022017_yes[is.na(total$tar_idkun_022017_yes)] <- 0

  
  #number of days between choice and SMS
  total$choice_days_022017=total$choose_date_date-total$tar_idkun_022017
  ##make 0/1 variable for who took action following sms - Y variables
  total$choice_3days_022017 <- ifelse(total$choice_days_022017>0 & total$choice_days_022017<4,1,0)
  total$choice_3days_022017[is.na(total$choice_3days_022017)] <- 0
  total$choice_7days_022017 <- ifelse(total$choice_days_022017>0 & total$choice_days_022017<8,1,0)
  total$choice_7days_022017[is.na(total$choice_7days_022017)] <- 0

    total$sms_022017 <- total$tar_idkun_022017_yes
   total$choose_new <- total$choose_date_date>0
   total$choose_new[is.na(total$choose_new)] <- 0
  total$feb_19 <- total$choose_date_date=="2017-02-06" |total$choose_date_date=="2017-02-07"|
    total$choose_date_date=="2017-02-08"|total$choose_date_date=="2017-02-09"|total$choose_date_date=="2017-02-10"|
    total$choose_date_date=="2017-02-11"|total$choose_date_date=="2017-02-12"|total$choose_date_date=="2017-02-13"|  
    total$choose_date_date=="2017-02-14"|total$choose_date_date=="2017-02-15"|total$choose_date_date=="2017-02-16"|
    total$choose_date_date=="2017-02-17"|total$choose_date_date=="2017-02-18"|total$choose_date_date=="2017-02-19"
  
  total$feb_19[is.na(total$feb_19)] <- 0
  total$feb_19 <- as.integer(total$feb_19)
  
  
  ######choose just for 50 NIS
  total$feb_50_20 <- total$choose_date_date=="2017-02-06"&  total$ex_50_nis==1 |total$choose_date_date=="2017-02-07"&  total$ex_50_nis==1|
    total$choose_date_date=="2017-02-08"&  total$ex_50_nis==1|total$choose_date_date=="2017-02-09"&  total$ex_50_nis==1|total$choose_date_date=="2017-02-10"&  total$ex_50_nis==1|
    total$choose_date_date=="2017-02-11"&  total$ex_50_nis==1|total$choose_date_date=="2017-02-12"&  total$ex_50_nis==1|total$choose_date_date=="2017-02-13"&  total$ex_50_nis==1|  
    total$choose_date_date=="2017-02-14"&  total$ex_50_nis==1|total$choose_date_date=="2017-02-15"&  total$ex_50_nis==1|total$choose_date_date=="2017-02-16"&  total$ex_50_nis==1|
    total$choose_date_date=="2017-02-17"&  total$ex_50_nis==1|total$choose_date_date=="2017-02-18"&  total$ex_50_nis==1|total$choose_date_date=="2017-02-19"&  total$ex_50_nis==1|
    total$choose_date_date=="2017-02-20"&  total$ex_50_nis==1
  
  total$feb_50_20[is.na(total$feb_50_20)] <- 0
  
  total$feb_50_20 <- as.integer(total$feb_50_20)
  
  
  ###############################################
  
  total$tar_idkun_022017_1 <-total$tar_idkun_022017
  total$tar_idkun_022017_1[is.na(total$tar_idkun_022017_1)] <- "1899-12-30"
  total$choose_date_date_1 <-total$choose_date_date
  total$choose_date_date_1[is.na(total$choose_date_date_1)] <-  "2020-12-30"
  total <- total[!(total$tar_idkun_022017_1=="2017-02-07"  & total$choose_date_date=="2017-02-06"),]
  
  total$sms_feb <- total$tar_idkun_022017=="2017-02-06"|total$tar_idkun_022017=="2017-02-07"
  total$sms_feb[is.na(total$sms_feb)] <- 0
  
  ######################data base partition
  total_before_prartition <-total
  save(total_before_prartition,file="total_before_prartition")
  
 load("total_before_prartition")
 total<- total_before_prartition
 #  # 
  
  ##make data base for onlychild under 18
  total_child_18 <- total[total$gill_child>=17,]
  total_child <- total[total$gill_child<15,]
  
  
  ##make data base for only first child + under 18
  total <- total_child[total_child$num_child_in_family==1,]
  disc <- disc[disc$num_child_in_family==1,]
  
    ###make new data base for academics - ran regressions with data base for robustness checks
  L=total$father_age<46 | total$mother_age<45 
  total_academic= total[L,]
  
  ##choose for who made choice onlt after SMS - used for robust specifications
#   total$k <- total$choose_date_date<"2017-02-06"
#   total$k[is.na(total$k)] <- 0
#  total_choice_after_sms1 <- total[total$k==0,]
#   
#   
#   total_academic$k <- total_academic$choose_date_date<"2017-02-06"
#   total_academic$k[is.na(total_academic$k)] <- 0
#   total_choice_after_sms1_academic <- total_academic[total_academic$k==0,]
#     
#  
# total_match_feb <- total_choice_after_sms1[!is.na(total_choice_after_sms1$socio_econ_cluster),]
# total_match_feb <- total_match_feb[!is.na(total_match_feb$periphery_cluster),]
# # names(total_match_feb)
# my_vars <- c("zman_idkun_022017_afternoon","zman_idkun_022017_evening","zman_idkun_022017_morning",
#              "zman_idkun_052017_afternoon","zman_idkun_052017_lunch","zman_idkun_052017_morning",
#              "zman_idkun_052017_lunch_sms2","zman_idkun_052017_morning_sms2",
#              "zman_idkun_052017","zman_idkun_022017","investment_fund", "ex_50_nis","child_with_disability1","child_male","num_family","num_child_in_family","kod_choice","device", "mother_work", "mother_wage","mother_academic", "father_work", "father_wage", "father_academic",  "parents_num_children", "parents_hardi", "parents_arab", "parents_married", "parents_avg_age", "socio_econ_cluster", "periphery_cluster", "rural", "gill_child", "sms_022017", "choose_new", "feb_19", "feb_10", "sms_feb","feb_1","feb_2",
#              "feb_50_1","feb_50_2","feb_50_10","feb_50_20",
#              "choose_date_date1","choose")
# 
# total_match_feb <- total_match_feb[,my_vars, with=FALSE]
# save(total_match_feb,file="total_match_feb")
 
#########data base for feb only with who got SMS

total_match_feb_only_sms <- total_choice_after_sms1[!(tar_idkun_022017_1 < "1900-01-01")]
total_match_feb_only_sms <- total_match_feb_only_sms[!is.na(total_match_feb_only_sms$socio_econ_cluster),]
total_match_feb_only_sms <- total_match_feb_only_sms[!is.na(total_match_feb_only_sms$periphery_cluster),]
my_vars <- c("zman_idkun_022017_afternoon","zman_idkun_022017_evening","zman_idkun_022017_morning",
             "zman_idkun_052017_afternoon","zman_idkun_052017_lunch","zman_idkun_052017_morning",
             "zman_idkun_052017_lunch_sms2","zman_idkun_052017_morning_sms2",
             "zman_idkun_052017","zman_idkun_022017","investment_fund", "ex_50_nis","child_with_disability1","child_male","num_family","num_child_in_family","kod_choice","device", "mother_work", "mother_wage","mother_academic", "father_work", "father_wage", "father_academic",  "parents_num_children", "parents_hardi", "parents_arab", "parents_married", "parents_avg_age", "socio_econ_cluster", "periphery_cluster", "rural", "gill_child", "sms_022017", "choose_new", "feb_19", "feb_10", "sms_feb",
             "choose_date_date1","feb_1","feb_2",
             "feb_50_1","feb_50_2","feb_50_10","feb_50_20")

total_match_feb_only_sms <- total_match_feb_only_sms[,my_vars, with=FALSE]

save("total_match_feb_only_sms",file="total_match_feb_only_sms")

        # For robust specifications - only academics
        # 
        # total_match_feb_only_sms_academic <- total_choice_after_sms1_academic[!(tar_idkun_022017_1 < "1900-01-01")]
        # total_match_feb_only_sms_academic <- total_match_feb_only_sms_academic[!is.na(total_match_feb_only_sms_academic$socio_econ_cluster),]
        # total_match_feb_only_sms_academic <- total_match_feb_only_sms_academic[!is.na(total_match_feb_only_sms_academic$periphery_cluster),]
        # my_vars <- c("zman_idkun_022017_afternoon","zman_idkun_022017_evening","zman_idkun_022017_morning",
        #              "zman_idkun_052017_afternoon","zman_idkun_052017_lunch","zman_idkun_052017_morning",
        #              "zman_idkun_052017_lunch_sms2","zman_idkun_052017_morning_sms2",
        #              "zman_idkun_052017","zman_idkun_022017","investment_fund", "ex_50_nis","child_with_disability1","child_male","num_family","num_child_in_family","kod_choice","device", "mother_work", "mother_wage","mother_academic", "father_work", "father_wage", "father_academic",  "parents_num_children", "parents_hardi", "parents_arab", "parents_married", "parents_avg_age", "socio_econ_cluster", "periphery_cluster", "rural", "gill_child", "sms_022017", "choose_new", "feb_19", "feb_10", "sms_feb",
        #              "choose_date_date1","feb_1","feb_2","mother_acad_years","father_acad_years",
        #              "feb_50_1","feb_50_2","feb_50_10","feb_50_20")
        # 
        # total_match_feb_only_sms_academic <- total_match_feb_only_sms_academic[,my_vars, with=FALSE]
        # save("total_match_feb_only_sms_academic",file="total_match_feb_only_sms_academic")
        # 
        