# load packages and data --------------------------------------------------
library(haven)
library(tidyverse)
library(dplyr)
library(psych)
library(stargazer)
library(lme4)
library(performance)
library(nortest)

library("intsvy")
library("dplyr")
library("ggplot2")
library("tidyr")
library(BIFIEsurvey)


root <- "~PISA/"
path <- paste0(root,"prepared_data/")
output <- paste0(root,"regression_results/")
setwd(path)

y <-2000


# Define function calls ---------------------------------------------------

##Output generator
bifie_stats <- function(bifie_output){
  stats <- bifie_output$stat
  
  stats <- stats %>%
    select(parameter,est, SE,t,p) %>%
    as_tibble()
  
  stats <- stats %>%
    add_row(parameter = "no Students", est = bifie_output$N)
  
  stats <- stats %>%
    add_row(parameter = "no Schools", est = bifie_output$Nclusters)
  
  stats <- stats %>% 
    filter(substr(parameter,1,4)=="beta" |
             substr(parameter,1,3)=="ICC" |
             substr(parameter,1,2)=="R2" |
             substr(parameter,1,3) =="no "
    )#
  return(stats)
}
## Calculate Variance inflation Factor of IVs required for the Regression
## Analysis
vif_calculator <- function(bifie_postesitmation, original_dataframe){
  ##prepare the dataframe
  pred_pre <- bifie_postesitmation %>%
    filter(str_detect(parameter,"beta"))
  ##get the parameters
  parms <-  gsub("beta_","", pred_pre$parameter) %>%
    as.list()
  
  ## Create a dataframe based on the IVs
  ## But: check if an interaction term is present.
  ## Interaction terms contain the ":"-character.
  if(sum(str_count(pattern=":", as.vector(parms))) > 0){  
    ##create a reduced dataframe
    model_df_reduced <- original_dataframe[colnames(original_dataframe) %in% parms]
    model_df_reduced <- model_df_reduced %>%
      mutate(Z_ISEI_Z_MEAN_ISEI = Z_ISEI * Z_MEAN_ISEI)
  }  else{
    model_df_reduced <- original_dataframe[colnames(original_dataframe) %in% parms]
  }
  
  ##prepare data for further investigation
  Vars <- model_df_reduced %>%
    colnames() #Vector for variable names
  VIF <- c() # empty vector for VIFs
  
  vif_dataframe <- tibble(variables = Vars)
  
  ## If our dataframe got more than one variable, then perform VIF
  ## Otherwise: return NA-values
  if(nrow(vif_dataframe) >1){
    ## generate a negation to filter DV
    `%nin%` = Negate(`%in%`) 
    for(V in Vars){
      DV <- V
      IVS <- as.vector(Vars)
      IVS <- IVS[IVS %nin% DV]
      
      IV_formula <- ""
      checkpos <- length(IVS)
      pos <- 1
      
      for(i in IVS){
        if(pos < checkpos) {
          IV_formula <- paste0(IV_formula,i," + ")
          pos <- pos + 1 
        }
        else{
          IV_formula <- paste0(IV_formula,i)
        }#
      }
      
      ## cerate a formula for calculating the VIF
      m <- as.formula(paste(DV, "~", IV_formula))
      
      ## Regress one IV of the imputated model on other IVs
      vif_model <- summary(lm(m, data = model_df_reduced))
      
      ## calculate VIF as 1/(1-R?j_th variable)
      value <- as.numeric(1/(1-vif_model$r.squared))
      ##show model
      vif_model
      
      ##append VIF to variable
      VIF <- append(VIF,as.numeric(value))
      
    }
    
    ## generate VIF- Dataframe
    vif_dataframe <- vif_dataframe %>%
      mutate(VIF = VIF)
    
    ## Add mean VIF
    vif_dataframe <- vif_dataframe %>%
      add_row(variables = "MEAN_VIF", VIF = mean(VIF))
  } else{
    ## generate VIF- Dataframe
    vif_dataframe <- vif_dataframe %>%
      mutate(VIF = NA)
    
    ## Add mean VIF
    vif_dataframe <- vif_dataframe %>%
      add_row(variables = "MEAN_VIF", VIF = NA)
  }
  return(vif_dataframe)
}

# Calculate Error Terms and Shapiro-Francia Test ---------------------------
## predict values for DV
## try to fetch the error terms for each model
sf_calculator <- function(bifie_postestimation,original_dataframe){
  pred_pre <- bifie_postestimation %>% 
    filter(str_detect(parameter,"beta"))
  
  ## fetch intercept_value
  alpha <- pred_pre %>%
    filter(str_detect(parameter, "Intercept")) %>%
    select(est)
  alpha <- alpha$est
  
  ##get the parameters
  betas <-  gsub("beta_","", pred_pre$parameter) %>%
    as.vector()
  
  # exclude intercept from parameter-List
  pred_pre$parameter <- betas
  pred_pre <- pred_pre %>%
    filter(str_detect(betas, "Intercept", negate= TRUE))
  
  
  
  ## check for interaction terms
  interactions <- betas[str_detect(betas,":")]
  
  ## split interaction terms
  interactions_split <- str_split(interactions, pattern =":")
  
  ## reduce beta-values
  betas <- betas[str_detect(betas, "Intercept", negate= TRUE)]
  betas <-betas[str_detect(betas,interactions, negate=TRUE)]
  
  
  ##add DV-Variables to parameters
  dep ="MEAN"
  dv_parms <- original_dataframe %>%
    select(contains("MEAN") & contains("PV")) %>%
    colnames() 
  
  dv_iv_parms <- append(dv_parms,betas)
  
  model_df_reduced <- original_dataframe %>%
    select(dv_iv_parms)
  
  if(length(interactions > 0)){
    print(paste("interactions present for model:",deparse(substitute(bifie_postestimation))))
    for(i in length(interactions_split)){
      print(interactions_split[[i]])
      interaction_variables = interactions_split[[i]]
      model_df_reduced[paste0(interaction_variables[1],
                              "_",
                              interaction_variables[2])
      ] <- model_df_reduced[interaction_variables[1]] * 
        model_df_reduced[interaction_variables[2]]
      
      ## add interaction term to betas
      
      betas <- append(betas,paste0(interaction_variables[1],
                                   "_",
                                   interaction_variables[2]))
    }
  }
  
  pred_pre$parameter <- str_replace(as.vector(pred_pre$parameter),":","_")
  
  ## calculate beta-values * values given per model
  for(b in betas){
    beta_value <- pred_pre %>%
      filter(parameter == b) %>%
      select(est) %>%
      as.numeric()
    varname_new <- paste0(b,"_predict")
    model_df_reduced[varname_new] <- model_df_reduced[b] * beta_value
    
  }
  model_df_reduced["prediction"] <- alpha + 
    rowSums(
      model_df_reduced[
        str_detect(
          colnames(model_df_reduced), "_predict")
      ]
    )
  ##create an empty list to store error-terms
  
  
  dvs <- model_df_reduced %>%
    select(starts_with("PV") & ends_with("MEAN")) %>%
    colnames()
  
  ## generate distances over all plausible values
  
  error_count <- 1
  for(d in dvs){
    
    model_df_reduced[
      paste0("ERRORS_",as.character(error_count)
      )
    ] <- model_df_reduced["prediction"] - model_df_reduced[d]
    
    
    error_count <- error_count +1
  }
  
  model_df_reduced["MEAN_ERROR"] <- model_df_reduced %>%
    select(starts_with("ERROR")) %>%
    rowMeans()
  
  errors <- model_df_reduced$MEAN_ERROR
  
  sampling <- NA
  if(length(errors) <=5000){
    sf_testvalue <- sf.test(errors)
    sampling <- "no"
  } else{
    seed = 42
    sf_testvalue <- sf.test(sample(errors, size = 5000))
    sampling <- "yes"
  }
  
  sf_testvalue <- tibble(model = deparse(substitute(bifie_postestimation)),
                         W = sf_testvalue$statistic,
                         P = sf_testvalue$p.value,
                         imputations = length(dvs),
                         obs = nrow(original_dataframe),
                         sampling = sampling)
  
  ##H0 = Variable follows a normal distribution
  ##H1 = Variable does not follow a normal distribution
  if(sf_testvalue$P < 0.05){
    sf_testvalue <- sf_testvalue %>% 
      mutate(normally_distributed = "no")
  }else{
    sf_testvalue<- sf_testvalue %>% 
      mutate(normally_distributed = "yes")
  }
  
  return(sf_testvalue)
}

# Load Country Dataframe for 2009 -----------------------------------------
#countries <- as.list(unique(countries))

countries <- c("DEU","GBR","USA","CAN","FIN","SWE")
#country <- "DEU"
for(country in countries){
  print(paste("currently at:",country))
  
  
  path_year <- paste0(path,"2000","/")
  setwd(path_year)
  
  
  df <- read.csv(paste0(country,"with_indices_2000.csv")) 
  df <- as_tibble(df)
  df <- df %>% select(-starts_with("X"))
  
  ## rename variables
  df <- df %>% 
    rename(autonomy = schauton)
  
  ##generate mean isei-values
  df <- df %>%
    group_by(schoolid) %>%
    mutate(mean_isei = mean(isei, na.rm=T)) %>%
    ungroup()
  
  for(i in seq(1,5)){
    df[paste0("pv",as.character(i),"mean")] <- rowMeans(df[c(paste0("pv",as.character(i),"math"),
                                                             paste0("pv",as.character(i),"read"),
                                                             paste0("pv",as.character(i),"scie"))], na.rm = T)
  }
  
  
  # Group and mean Weightings -----------------------------------------------
  df["w_fstuwt"] <- df %>% 
    select(w_fstuwt_read, w_fstuwt_scie, w_fstuwt_math) %>%
    rowMeans(na.rm=T)
  
  for(i in seq(1,80)){
    df[paste0("w_fstr",as.character(i))] <- rowMeans(df[c(paste0("w_fstr_scie",as.character(i)),
                                                          paste0("w_fstr_math",as.character(i)),
                                                          paste0("w_fstr_read",as.character(i)))],
                                                     na.rm=T)
    
  }  
  
  
  
  # Select Variables relevant for regression analysis -----------------------
  
  
  model_df <-  df %>%
    select(ends_with("mean"), isei , mean_isei,
           mig_2nd , mean_mig , langn,
           disclima , mean_disclima,
           autonomy , leadership , accountability , 
           schoolid, w_fstuwt, stidstd,  schoolid,
           starts_with("w_fstr"),
           -matches("_math[1-9]+"),
           -matches("_read[1-9]+"),
           -matches("_scie[1-9]+")) %>% 
    na.omit()
  
  
  # Change Column Names to upper --------------------------------------------
  colnames(model_df) <- model_df %>%
    colnames() %>%
    toupper()  
  
  
  
  # z-standardize variables -------------------------------------------------
  
  model_df <- model_df %>% mutate(Z_ISEI = ISEI - mean(ISEI) / sd(ISEI),
                                  Z_MEAN_ISEI = MEAN_ISEI - mean(MEAN_ISEI) / sd(MEAN_ISEI),
                                  Z_MEAN_MIG = MEAN_MIG -  mean(MEAN_MIG) / sd(MEAN_MIG),
                                  Z_DISCLIMA = DISCLIMA - mean(DISCLIMA) / sd(DISCLIMA),
                                  Z_MEAN_DISCLIMA = MEAN_DISCLIMA - mean(MEAN_DISCLIMA) / sd(MEAN_DISCLIMA),
                                  Z_AUTONOMY = AUTONOMY - mean(AUTONOMY) / sd(AUTONOMY),
                                  Z_LEADERSHIP = LEADERSHIP -mean(LEADERSHIP) / sd(LEADERSHIP),
                                  Z_ACCOUNTABILITY = ACCOUNTABILITY - mean(ACCOUNTABILITY) / sd(ACCOUNTABILITY) 
  )
  
  
  
  # Generate BIFIE-Data -----------------------------------------------------
  
  
  
  
  
  RR <- 80
  test <- BIFIE.data.jack(as.data.frame(model_df), jktype = "RW_PISA",
                          pv_vars = "MEAN",
                          wgtrep="W_FSTR",
                          pvpre = paste0("PV",1:10),
                          fayfac=1 / RR / ( 1 - .5 )^2 )
  
  m0 <-BIFIE.twolevelreg(BIFIEobj = test,
                         dep = "MEAN",
                         idcluster =  "SCHOOLID",
                         formula.fixed = ~1, 
                         formula.random = ~1,
                         wgtlevel1 <- "W_FSTUWT",
                         maxiter=1000, se = TRUE)
  
  m1 <-BIFIE.twolevelreg(BIFIEobj = test,
                         dep = "MEAN",
                         idcluster =  "SCHOOLID",
                         formula.fixed = ~1 + Z_ISEI,
                         formula.random = ~1,
                         wgtlevel1 <- "W_FSTUWT",
                         maxiter=1000, se = TRUE)
  
  m2 <-BIFIE.twolevelreg(BIFIEobj = test,
                         dep = "MEAN",
                         idcluster =  "SCHOOLID",
                         formula.fixed = ~1 + Z_MEAN_ISEI,
                         formula.random = ~1,
                         wgtlevel1 <- "W_FSTUWT",
                         maxiter=1000, se = TRUE)
  
  m3 <-BIFIE.twolevelreg(BIFIEobj = test,
                         dep = "MEAN",
                         idcluster =  "SCHOOLID",
                         formula.fixed = ~1 + Z_ISEI * Z_MEAN_ISEI,
                         formula.random = ~1,
                         wgtlevel1 <- "W_FSTUWT",
                         maxiter=1000, se = TRUE) 
  
  
  m4 <-BIFIE.twolevelreg(BIFIEobj = test,
                         dep = "MEAN",
                         idcluster =  "SCHOOLID",
                         formula.fixed = ~1 + MIG_2ND + Z_MEAN_MIG + LANGN,
                         formula.random = ~1,
                         wgtlevel1 <- "W_FSTUWT",
                         maxiter=1000, se = TRUE) 
  
  m5 <-BIFIE.twolevelreg(BIFIEobj = test,
                         dep = "MEAN",
                         idcluster =  "SCHOOLID",
                         formula.fixed = ~1 + Z_DISCLIMA + Z_MEAN_DISCLIMA,
                         formula.random = ~1,
                         wgtlevel1 <- "W_FSTUWT",
                         maxiter=1000, se = TRUE)  
  
  m6 <-BIFIE.twolevelreg(BIFIEobj = test,
                         dep = "MEAN",
                         idcluster =  "SCHOOLID",
                         formula.fixed = ~1 +  Z_AUTONOMY + Z_LEADERSHIP + Z_ACCOUNTABILITY,
                         formula.random = ~1,
                         wgtlevel1 <- "W_FSTUWT",
                         maxiter=1000, se = TRUE)  
  
  m7 <-BIFIE.twolevelreg(BIFIEobj = test,
                         dep = "MEAN",
                         idcluster =  "SCHOOLID",
                         formula.fixed = ~1 + Z_ISEI * Z_MEAN_ISEI + 
                           MIG_2ND +Z_MEAN_MIG + LANGN +
                           Z_DISCLIMA + Z_MEAN_DISCLIMA,
                         formula.random = ~1,
                         wgtlevel1 <- "W_FSTUWT",
                         maxiter=1000, se = TRUE)
  
  m8 <-BIFIE.twolevelreg(BIFIEobj = test,
                         dep = "MEAN",
                         idcluster =  "SCHOOLID",
                         formula.fixed = ~1 + Z_ISEI * Z_MEAN_ISEI +
                           Z_AUTONOMY + Z_LEADERSHIP + Z_ACCOUNTABILITY,
                         formula.random = ~1,
                         wgtlevel1 <- "W_FSTUWT",
                         maxiter=1000, se = TRUE)
  
  m9 <-BIFIE.twolevelreg(BIFIEobj = test,
                         dep = "MEAN",
                         idcluster =  "SCHOOLID",
                         formula.fixed = ~1 +Z_ISEI * Z_MEAN_ISEI + 
                           MIG_2ND +Z_MEAN_MIG + LANGN + 
                           Z_DISCLIMA + Z_MEAN_DISCLIMA + 
                           Z_AUTONOMY + Z_LEADERSHIP + Z_ACCOUNTABILITY,
                         formula.random = ~1,
                         wgtlevel1 <- "W_FSTUWT",
                         maxiter=1000, se = TRUE)
  
  
  
  # Generate Outputs --------------------------------------------------------
  setwd(output)
  
  ##generate statistiscs for export
  stats_m0 <- bifie_stats(m0)
  stats_m1 <- bifie_stats(m1)
  stats_m2 <- bifie_stats(m2)
  stats_m3 <- bifie_stats(m3)
  stats_m4 <- bifie_stats(m4)
  stats_m5 <- bifie_stats(m5)
  stats_m6 <- bifie_stats(m6)
  stats_m7 <- bifie_stats(m7)
  stats_m8 <- bifie_stats(m8)
  stats_m9 <- bifie_stats(m9)
  
  varnames_statistics <- c()
  
  for(var in stats_m9$parameter){
    varnames_statistics <- append(varnames_statistics,var)
    varnames_statistics <- append(varnames_statistics,paste0(var,"_SD"))
  }
  
  
  export_data <- as.tibble(varnames_statistics) %>% 
    rename("parameter" = "value") %>%
    filter(parameter != "no Students_SD") %>%
    filter(parameter != "no Schools_SD") 
  
  ## Create Estimates with stars
  
  models <- list(stats_m0, 
                 stats_m1, 
                 stats_m2, 
                 stats_m3,
                 stats_m4, 
                 stats_m5, 
                 stats_m6, 
                 stats_m7, 
                 stats_m8,
                 stats_m9)
  
  model_count <- 0
  for(MODEL in models){
    
    ## generate model name
    MODEL_NAME <- paste0("Model_",as.character(model_count))
    
    ## generate table with parameter values and p-values
    stats_est <- MODEL %>%
      select(parameter, est,p) %>% 
      mutate(est = as.character(round(est, digits=3)),
             p = round(p, digits=3)) %>%
      mutate(p = case_when(p < 0.001 ~ "***",
                           p < 0.01 ~"**",
                           p < 0.05 ~ "*",
                           p >= 0.05 ~ "",
                           p == NA ~ ""))
    
    stats_est <-stats_est %>%
      unite(est, est,p, na.rm =T, sep="")
    
    
    colnames(stats_est) <- c("parameter", MODEL_NAME)
    
    ## generate standard error estimations
    
    stats_se <- MODEL %>% 
      select(parameter,SE) %>%
      mutate(SE = paste0("(", 
                         as.character(round(SE,digits=3)),
                         ")")) 
    colnames(stats_se) <- c("parameter", MODEL_NAME)
    
    
    ## rename Standard Error parameters
    PARMS <- c()
    for(p in stats_se$parameter){
      PARMS <- append(PARMS, paste0(p,"_SD"))
    }
    stats_se$parameter <- PARMS
    rm(PARMS)
    
    
    export_data <- export_data %>%
      left_join(rbind(stats_se,stats_est), by = c("parameter"))
    model_count <- model_count +1
  }
  
  
  
  xlsx::write.xlsx(export_data,paste0("2000_robustness_",country,".xlsx"),
                   showNA=FALSE)
  
  
  
  vif_m1 <- vif_calculator(stats_m1,model_df) %>%
    rename(VIF_m1=VIF)
  
  vif_m2 <- vif_calculator(stats_m2,model_df) %>%
    rename(VIF_m2=VIF)
  
  vif_m3 <- vif_calculator(stats_m3,model_df) %>%
    rename(VIF_m3=VIF)
  
  vif_m4 <- vif_calculator(stats_m4,model_df) %>%
    rename(VIF_m4=VIF)
  
  vif_m5 <- vif_calculator(stats_m5,model_df) %>%
    rename(VIF_m5=VIF)
  
  vif_m6 <- vif_calculator(stats_m6,model_df) %>%
    rename(VIF_m6=VIF)
  
  vif_m7 <- vif_calculator(stats_m7,model_df) %>%
    rename(VIF_m7=VIF)
  
  vif_m8 <- vif_calculator(stats_m8,model_df) %>%
    rename(VIF_m8=VIF)
  
  vif_m9 <- vif_calculator(stats_m9,model_df) %>%
    rename(VIF_m9 = VIF)
  
  vif_dataframe <- vif_m9 %>% 
    left_join(vif_m1) %>%
    left_join(vif_m2) %>%
    left_join(vif_m3) %>%
    left_join(vif_m4) %>%
    left_join(vif_m5) %>%
    left_join(vif_m6) %>%
    left_join(vif_m7) %>%
    left_join(vif_m8) %>%
    relocate(VIF_m9, .after = last_col())
  
  vif_dataframe <-    vif_dataframe %>%
    slice(3,4,11, 1,5,2,6:10,12)
  
  xlsx::write.xlsx(vif_dataframe,paste0(as.character(y),"_robustness_",country,"_VIF.xlsx"),
                   showNA=FALSE)
  
  ##Calculate Shapiro-Francia-Tests
  shapiros <- sf_calculator(stats_m1,model_df) %>%
    add_row(sf_calculator(stats_m2,model_df)) %>%
    add_row(sf_calculator(stats_m3,model_df)) %>%
    add_row(sf_calculator(stats_m4,model_df)) %>%
    add_row(sf_calculator(stats_m5,model_df)) %>%
    add_row(sf_calculator(stats_m6,model_df)) %>%
    add_row(sf_calculator(stats_m7,model_df)) %>%
    add_row(sf_calculator(stats_m8,model_df)) %>%
    add_row(sf_calculator(stats_m9,model_df))
  xlsx::write.xlsx(shapiros,paste0(as.character(y),"_robustness_",country,"_sf-test.xlsx"),
                   showNA=FALSE)
}  

