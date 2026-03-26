#0) Prep steps
    #a) load libraries
        library(data.table)
        library(fst)
        library(dplyr,  warn.conflicts = FALSE)
        library(Hmisc)
        library(lme4)
        library(survey)
        library(parameters)
        library(lmerTest)
        library(tidyr)
        library(ggpubr)
        library(cowplot)
        library(writexl)
        library(MASS)
        library(AER)
        library(multiwayvcov)
        library(lmtest)
        library(sandwich)
        library(ggthemes)
        library(metafolio)
        library(scales)
        library(stringr)
        library(mice)
        library(corpcor)
        
    #b) make directories
        fig.dir <- "C:/Users/admin/Desktop/IDrive-Sync/Analysis - Code/Postdoc/COVID/Figures"
        data.dir <- "C:/Users/admin/Desktop/IDrive-Sync/Analysis - Code/Postdoc/COVID/Data"


#1) Read data
    df <- fread(file.path(data.dir, "p2p_covid.csv"))


#2) Identify DVs
    bin_vars <- names(df)[df[, lapply(.SD, function(x) length(unique(x)))] == 2]
    testing_cat_dvs <- c("test_bad_symptoms", "test_near_affected", "test_afford_treatment", "test_hospitals_can_treat", "test_disable_working", "test_risk_to_others")
    econ_cat_dvs <- grep("dv", names(df), value = T)
    econ_cat_dvs <- econ_cat_dvs[!econ_cat_dvs %in% bin_vars]
    econ_cat_dvs <- setdiff(econ_cat_dvs, c("dv_worry_hosp_overwhelm", "Get_Health\nAdvice"))

    
#3) Create/transform Variables
    #a) IVs
        #i) Initial Replacements
            df[cv_marital %in% c("Not married but living with a partner", "Never been married"), cv_marital := "Never Married"]
            df[cv_marital %in% c("Divorced or annulled", "Separated"), cv_marital := "Divorced"]
            df[pre_food_insecure == "", pre_food_insecure := NA]
            df[pre_home_insecure == "", pre_home_insecure := NA]
        #ii) Detailed cleaning
            df[, ':='(
              tmp_age = cv_age,
              cv_age = scale(cv_age),
              cv_age2 = scale(cv_age^2),
              acs_income_median = acs_income_median /10000,
              Sex = ifelse(sex == "male", "Male", "Female"),
              cv_sex_female_black = iv_race_black * cv_sex_female,
              cv_sex_female_latino = iv_race_latino * cv_sex_female,
              cv_sex_female_other = iv_race_other * cv_sex_female,
              iv_race_other2 = iv_race_other + iv_race_latino,
              cv_marital = factor(cv_marital, levels = c("Married", "Never Married", "Widowed", "Divorced")),
              pre_food_insecure = factor(pre_food_insecure, levels =  c("Never", "Rarely", "Sometimes", "Often")),
              pre_home_insecure = factor(pre_home_insecure, levels =  c("Never", "Rarely", "Sometimes", "Often")),
              iv_uninsured = dv_uninsured,
              pre_diabetes = ifelse(pre_diabetes == "DON'T KNOW", NA, 
                             ifelse(pre_diabetes == "NOT SELECTED", 0,
                             1)),
              pre_smoker = ifelse(pre_smoker %in% c("NOT AT ALL", "NOT ASKED DUE TO SKIP LOGIC"), 0, 
                           ifelse(pre_smoker %in% c("EVERY DAY", "SOME DAYS"), 1, 
                           NA)),
              physical_health = ifelse(physical_health %in% c("Excellent", "Very good", "Good"), 1,
                                ifelse(physical_health %in% c("Fair", "Poor"), 0, 
                                NA)),
              covid_acquaintence = `covid_friend`,
              covid_close_friend = `Close_Friend_or\nFamily_with_COVID`
            )]
        #iii) Make food insecurity dummies
            pre_dummies <- c("pre_food_insecure", "pre_home_insecure")
            newdummies <- paste(pre_dummies, "_dum", sep = "")
            df[, (newdummies) := lapply(.SD, function(x) ifelse(x %in% c("Rarely", "Sometimes", "Often"), 1, 0)) , .SDcol = pre_dummies]
            df[, pre_food_insecure_dum := ifelse(pre_food_insecure_dum + pre_home_insecure_dum > 1, 1, pre_food_insecure_dum + pre_home_insecure_dum)]
        #iv) Make various age groupings
            df[, ':='(
              age65P = ifelse(age_group == "65P", 1, 0),
              age55.64 = ifelse(age_group == "55-64", 1, 0),
              age45.54 = ifelse(age_group == "45-54", 1, 0),
              age35.44 = ifelse(age_group == "35-44", 1, 0),
              age25.34 = ifelse(age_group == "25-34", 1, 0),
              age15.24 = ifelse(age_group == "15-24", 1, 0),
              age15.44 = ifelse(age_group %in% c("15-24", "25-34", "35-44"), 1, 0),
              age45.64 = ifelse(age_group %in% c("45-54", "55-64"), 1, 0),
              age15.34 = ifelse(age_group %in% c("15-24", "25-34"), 1, 0),
              age35.54 = ifelse(age_group %in% c("35-44", "45-54"), 1, 0),
              age15.39 = ifelse(tmp_age <= 39, 1, 0),
              age40.59 = ifelse(tmp_age > 39 & tmp_age <= 59, 1, 0)
        )]
        #v) Make COVID Worry index
            worry.vars <- c("worried_catch", "worried_ill", "worried_friends_catch", "worried_friends_ill", "worried_likely_to_catch")
            df[, (worry.vars) := lapply(.SD, function(x) factor(x, levels = rev(c("Strongly agree", "Agree", "Disagree", "Strongly disagree")))), .SDcols = worry.vars]
            df[, (worry.vars) := lapply(.SD, as.numeric), .SDcols = worry.vars]
            #impute missings - can't use factor analysis with any missings
              # df_worry <- df[, (worry.vars), with = F]
              # df_worry <- mice(df_worry, m=1, maxit=50, meth='pmm', seed=500)
              # df_worry <- complete(df_worry,1)
            #factor analysis - not good because can't weight
              # df[, covid.worry.index := factanal(x = df_worry, factors = 1, scores = 'regression')$scores]
            df[, covid.worry.index := rowMeans(df[, c(worry.vars), with = F], na.rm = T)]
            df[, covid.worry.index := (covid.worry.index - min(covid.worry.index, na.rm = T)) / (max(covid.worry.index, na.rm = T) - min(covid.worry.index, na.rm = T))]

        #vi) Make financial insecurity scale
            fin.vars <- c("dv_worry_finances", "dv_worry_food_insecure", "dv_worry_home_insecure")
            ord.fin.vars <- paste(fin.vars, "_ord", sep = "")
            df[, (ord.fin.vars) := lapply(.SD, function(x) factor(x, levels = rev(c("Strongly agree", "Agree", "Disagree", "Strongly disagree")))), .SDcols = fin.vars]
            df[, (ord.fin.vars) := lapply(.SD, as.numeric), .SDcols = ord.fin.vars]
            #factor analysis approach; not good because can't weight
              # df_financial <- df[, (ord.fin.vars), with = F]
              # df_financial <- mice(df_financial, m=1, maxit=50, meth='pmm', seed=500)
              # df_financial <- complete(df_financial,1)
              # df[, financial.index := factanal(x = df_financial, factors = 1, scores = 'regression')$scores]
            df[, financial.index := rowMeans(df[, c(ord.fin.vars), with = F], na.rm = T)]
            df[, financial.index := (financial.index - min(financial.index, na.rm = T)) / (max(financial.index, na.rm = T) - min(financial.index, na.rm = T))]

    #b) DVs
        #i) Initial cleaning
            df[, dv_fired := dv_fired + dv_unable_find_work]
            df[dv_fired > 1, dv_fired := 1]
            df[, (econ_cat_dvs) := lapply(.SD, function(x) factor(x, levels = rev(c("Strongly agree", "Agree", "Disagree", "Strongly disagree")))), .SDcols = econ_cat_dvs]
            df[, (testing_cat_dvs) := lapply(.SD, function(x) factor(x, levels = rev(c("Strongly agree", "Agree", "Disagree", "Strongly disagree")))), .SDcols = testing_cat_dvs]
        #ii) Create dummy versions of econ DVs
            cat_vars <- c(econ_cat_dvs)
            cat_dummies <- paste(cat_vars, "_dum", sep = "")
            df[, (cat_dummies) := lapply(.SD, function(x) ifelse(x %in% c("Agree", "Strongly agree"), 1, 0)) , .SDcol = cat_vars]
    
  
#4) Pull in zip codes and rural codes
    #zip codes
        tmp <- fread(file.path(data.dir, "p2p_covid_addr_2020-08-07.csv"), select = c("su_id", "p2p_zip"))
        setnames(tmp, old = c("su_id", "p2p_zip"), new = c("id", "zip"))
        df <- merge(df, tmp, by = "id")
    
    #dichotomous rural codes
        tmp <- fread(file.path(data.dir, "Other/ruralurbancodes2013.csv"))
        tmp$rucc_label <- grepl("Nonmetro", tmp$rucc_label)
        setnames(tmp, old = c("FIPS", "rucc_label"), new = c("state_county", "Rural"))
        df <-merge(df, tmp, by = "state_county", sort = F)   
        df$Rural <- as.numeric(df$Rural)
        
    #pop density at zip code level
        # #do once: remove non indiana from zip codes
        #     df.zip <- read_fst("C:/Users/admin/Desktop/Analysis - Data/Postdoc/ACS derived vars/ACS_zcta.fst")
        #     df.zip <- df.zip[df.zip$GEOID %in% df$zip & df.zip$year  == "2018", ]
        #     fwrite(df.zip, file.path(data.dir, "Other/ACS_zips.csv"))
        df.zip <- fread(file.path(data.dir, "Other/ACS_zips.csv"), select = c("GEOID", "pop.dens"))
        names(df.zip) <- c("zip", "zip.pop.dens")
        df.zip$zip <- as.character(df.zip$zip)
        df$zip <- substr(df$zip, 1, 5)
        df <- merge(df, df.zip, by = "zip", all.x = T, sort = F)
        
    #urban at zip code level
        df.zip <- fread(file.path(data.dir, "Other/nhgis0005_ds172_2010_zcta.csv"))
        df.zip <- df.zip[,.(
            zip = str_pad(ZCTA5A, 5, "left", "0"),
            zip.Urbanized_Area = H7W003 / H7W001,
            zip.Urban_Cluster = H7W004 / H7W001,
            zip.Rural = H7W005 / H7W001
        )]
        df <- merge(df, df.zip, by = "zip", sort = F, all.x = T)

        
#5) Save 
    write_fst(df, file.path(data.dir, "p2p_covid_clean.fst"))
            
    
    