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

    #b) make directories
        fig.dir <- "C:/Users/admin/Desktop/IDrive-Sync/Analysis - Code/Postdoc/COVID/Figures"
        data.dir <- "C:/Users/admin/Desktop/IDrive-Sync/Analysis - Code/Postdoc/COVID/Data"


#1) Read data
    df <- read_fst(file.path(data.dir, "p2p_covid_clean.fst"), as.data.table = T)

    
#2) Select DVs
    dvs <- grep("dv", names(df), value = T)
    bin_vars <- names(df)[df[, lapply(.SD, function(x) length(unique(x)))] == 2]
    bin_dvs <- dvs[dvs %in% bin_vars]
    cat_dvs <- dvs[!dvs %in% bin_vars]
    bin_dvs <- "dv_fired"
    cat_dvs <- setdiff(cat_dvs, c("dv_worry_hosp_overwhelm", "Get_Health\nAdvice", "dv_worry_finances_ord", "dv_worry_food_insecure_ord", "dv_worry_home_insecure_ord"))

    
#3) Omit missings for key variables
    # newvars <- c("pre_emp_full_time", "pre_emp_part_time", "pre_self_employed", "pre_food_insecure", "pre_home_insecure", "pre_child_number", "pre_occ_farming", "pre_occ_automotive", "pre_occ_construction", "pre_occ_manufacturing", "pre_occ_railroad", "pre_occ_forestry", "pre_occ_electrical", "pre_occ_beauty", "pre_occ_fire")
    keep_vars <- c(bin_dvs, cat_dvs, "pre_food_insecure", "pre_unemployed", "cv_age", "cv_sex_female", "iv_race_black", "iv_race_other", "iv_race_latino", "cv_educ_lths", "cv_educ_hs", "cv_educ_some_college")
    keeps <- !apply(df[, keep_vars, with = F], 1, function(x) any(is.na(x)))
    df <- df[keeps]

        
#4) Generate descriptives
    tmp1 <- df[, lapply(.SD, function(x) mean = wtd.mean(x, weight_state)), .SDcols = c(cat_dummies, keep_vars, "pre_food_insecure_dum", "pre_unemployed", "iv_race_white", "cv_educ_bachelors")]
    tmp2 <- df[, lapply(.SD, function(x) mean = sqrt(wtd.var(x, weight_state))), .SDcols = c(cat_dummies, keep_vars, "pre_food_insecure_dum", "pre_unemployed", "iv_race_white", "cv_educ_bachelors")]
    vars <- data.table(var = names(tmp1), mean = t(tmp1), sd = t(tmp2))
    vars <- vars[!is.na(mean.V1)]
    t.order <- fread("C:/Users/admin/Desktop/IDrive-Sync/Analysis - Code/Postdoc/COVID/Data/model var order.csv", header = F)
    names(t.order) <- c("Variable", "var")
    t.table <- merge(t.order, vars, by = "var", all = T, sort = F)
    t.table <- t.table[Variable %in% c("Race", "Education") | !is.na(mean.V1)]
    fwrite(t.table, "C:/Users/admin/Desktop/IDrive-Sync/Analysis - Code/Postdoc/COVID/Figures/raw_descriptives.csv")