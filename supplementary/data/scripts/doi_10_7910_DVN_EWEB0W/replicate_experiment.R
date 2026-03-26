# # -- How in install BEAT
# install.packages(c("devtools", "ggplot2")) ## ggplot2 only needed for example
# devtools::install_version("RcppEigen", "0.3.3.7.0") ## beat does not work with newer RcppEigen
# devtools::install_github("ayeletis/beat")  ## do not update the RcppEigen if prompted
# # --

library(data.table)
library(stargazer)
library(grf)
library(beat)
library(ggpubr)
library(dplyr)
library(xgboost)

rm(list=ls())

# ---- Working directory  
dir <- "~/dataverse files/"
setwd(dir)

source("f_policy_evaluation.R")
source("f_counterfactual_results.R")

# Our degrees of freedom
train_prop = 0.90 # how much data to train
n_splits = 1000
target_rate = 0.5

## Model specs
num_trees = 2000
my_penalty = 5
my_tunable_params = c("sample.fraction","mtry","min.node.size",
                      "honesty.fraction","honesty.prune.leaves",
                      "alpha","imbalance.penalty")
# This list has the tunable params from regular GRF.
# We could tune "target.weight.penalty" too, using [tune.parameters='all'], but chose not too so that one can generate a tradeoff plot

## Load data (either of these will work for loading the files)
setwd(dir)
experimental_data = fread("experimental_data_for_replication.csv")
load("experimental_data_for_replication.Rdata")

# Choose a random seed
my_seed = 12345 #can change to any random number

# Get data formats for GRF
my_data = copy(experimental_data)
Y_full = as.matrix(my_data)[,1]
W_full = as.matrix(my_data)[,2]
Z_full = data.table(as.matrix(my_data)[,c(3:16)])
X_full = data.table(as.matrix(my_data)[,c(17:206)])

# Random split of the data (This code runs 1, the paper show summary statistics for 1,000 splits)
dt = sort(sample(length(Y_full), length(Y_full)*train_prop))

# Prepare the data for estimation
Y_train = Y_full[dt]
Y_test = Y_full[-dt]

W_train = W_full[dt]
W_test = W_full[-dt]

X_train = X_full[dt,]
X_test = X_full[-dt,]

Z_train = Z_full[dt,]
Z_test = Z_full[-dt,]

# With demographic variables
X_train_demog = cbind(X_train,Z_train)
X_test_demog = cbind(X_test,Z_test)

# Variables needed for functions
train_data = data.table(Y=Y_train,Z_train,W=W_train,X_train)
Xcols = names(X_train)
Zcols = names(Z_train)

# ================================================================
#            Estimate Models
# ----------------------------------------------------------------
print('Estimate models...')
setwd(dir)
source("c_run_all_models.R")
source("c_debias_tau.R")
# Plug-in XGBoost -- This part below tunes XGB via cross validation
print('tuning XGBoost...')
xg_rounds = 1500 
xg_verbose = 0 # whether XG prints rounds
source("c_optimum_params_xg.R")
source("c_plugin_xg.R")

# Estimate Taus
source("c_predict_taus.R")

# Set policies
source("c_my_targeting_policies.R")

# ================================================================
#            Compute counterfactuals for different Z
# ----------------------------------------------------------------
scenario = 999 #experiment is set as scenario 999 in the file
source("c_predict_taus_counterfactual_Z.R")

# ================================================================
#            Create table results: Tables 2, S-4
# ----------------------------------------------------------------

# IPW and imbalance
W.hat_train = mean(W_train) # We use prop(treatment) because the data comes from an experiment
W.hat_test = mean(W_test)
results = collect_benchmark_results(target_rate)
results = results[data_type=="test"]
results = results[,.(ipw,target_ratio,target_weight_imbalance)]

# Individual fairness metrics
print('Compute individual fairness metrics...')
indiv_fairness = collect_counterfactual_results(target_rate)

setwd(dir)
my_file = paste("Tables_2_S4.RData",sep='')
save(list = c('results','indiv_fairness'), file = my_file)   



# ================================================================
#          Underlying Relationships Analysis: Figure 5
# ----------------------------------------------------------------
## functions to run univariate regressions

feature_keys = c("duration",    
                 "Location", 
                 "sportswear_",  
                 "apparel_",     
                 'restaraunts_', 
                 'technology_',  
                 'activities_',  
                 'Spend_',       
                 'foodrestriction_',
                 "Pets",         
                 'MturkUsage',    
                 'Enviromental',  
                 "price_",        
                 "os_",
                 'dollar_shave',    
                 'chewy',           
                 'Amazon_subscribe',
                 'blue_apron',      
                 'hello_chef',     
                 'dropbox',       
                 'mobile_',
                 'gave.email',     
                 'education',       
                 'income',          
                 'Food_'            
) # total 115
feature_cols = lapply(feature_keys, function(x)grep(x, names(my_data), value=TRUE))
feature_cols = unlist(feature_cols)   #190 

my_data[, nonMale := 1-as.integer(Gender_1==1)]
my_data[, nonWhite := 1- as.integer(race_White==1)]
my_data[, Age_gt39 := Age>median(Age)]

data_reg = my_data[, .SD,.SDcols=c(feature_cols, 'nonMale', "nonWhite", "Age","Age_gt39")]

mobile_cols <- grep("mobile_",feature_cols,value=T) 
os_cols <- grep("os_",feature_cols,value=T)
separate_cols <- setdiff(feature_cols, c(mobile_cols, os_cols))

## OLS for Age
ols = function(formula, data){
  out = lm(formula = formula, 
           data=data,
           na.action = na.omit)
  return(out)
}

## Logit for nonMale, nonWhite
glm_logit = function(formula, data){
  out = glm(formula = formula, 
            data=data, 
            family="binomial", 
            na.action = na.omit, 
            control=list(maxit=1000))
  return(out)
}

# Collect regression coefficients
collect_regression_coefs = function(Y, X, data, reg_fn){
  fm = as.formula(paste0(Y, "~", paste0(sprintf("`%s`", X), collapse = "+")))
  res = reg_fn(fm, data)
  res_summary = summary(res)
  coef = as.data.table(res_summary$coefficients, keep.rownames = TRUE)
  #  coef = coef[!grepl(paste0("^",factor_cols, collapse = "|"),rn)]
  coef = coef[rn!='(Intercept)']
  coef[, rn:=gsub("`", '', rn)]
  coef[, chunk:= tstrsplit(rn, "_", keep=1)]
  
  setorder(coef, Estimate)
  setnames(coef, "Std. Error", "Std")
  names(coef)[5] = "Prob"
  #  coef[, significant_005 := ifelse(Prob<0.05, "True", "False")]
  coef[, `p-val<0.05` := ifelse(Prob<0.05, "True", "False")]
  coef = coef[,.(rn, Estimate,
                 Std =  Std , 
                 dep_var = Y,
                 `p-val<0.05`,
                 chunk
  )]
  return(coef)
}

compile_coefs = function(Y, reg_fn){
  data = data_reg
  coef_mobile <- collect_regression_coefs(Y=Y, X=mobile_cols, data, reg_fn=reg_fn)
  coef_os <- collect_regression_coefs(Y=Y, X=os_cols, data, reg_fn=reg_fn)
  coef_otherX <- do.call(rbind, lapply(separate_cols, function(x) collect_regression_coefs(Y=Y, X=x, data, reg_fn=reg_fn)))
  coef = rbind(coef_mobile,
               coef_os,
               coef_otherX)
  
  coef[, chunk_len:=.N, by="chunk"]
  coef[chunk_len==1, chunk:= "Misc."]
  coef = change_var_name(coef, by="rn")
  setorder(coef, Estimate)
  return(coef)
}

## Functions to assign meaningful names to variable names
change_var_name <- function(data,by){
  setnames(name_label_match, "variable", by)
  result = merge(data, name_label_match, by=by, all.x=T, sort=F)
  result[is.na(label), label:=get(by)]
  result[grep("mobile_",label), label:=gsub("mobile_", "", label)]
  result[grep("Food_",label), label:=gsub("Food_", "", label)]
  setnames(name_label_match, by, "variable")
  return(result)
}

name_label_match <- rbind(
  sportswear = data.table(
    variable = c(grep("sportswear_",names(X_full),value=T)),
    label = c("Nike", "Adidas", "Reebok", "Puma", "New Balance", "Under Armor", "Lululemon", "Fabletics", "Athleta")
  ),
  apparel = data.table(
    variable = c(grep("apparel_", names(X_full), value=T)),
    label = c("Gucci", "Zara", "Dior", "Prada", "Calvin Klein", "Ann Taylor", "Theory", "H&M", "Forever 21", "GAP", "Old Navy", "Banana Republic")
  ),
  restaurants = data.table(
    variable = c(grep("restaraunts_", names(X_full), value=T)),
    label = c("Starbucks", "Dunkin Donuts", "Sweetgreen", "Cosi", "Panera Bread", "Pizza Hut", "Domino’s Pizza", "Burger King", "McDonalds", 
              "Tasty Burger", "Shake Shack", "Chopt", "Papa John’s", "Little Caesars")
  ),
  technology = data.table(
    variable = c(grep("technology_",names(X_full),value=T)),
    label = c("Apple", "Google", "Facebook", "Amazon", "Twitter", "Instragram", "Pinterest", "Tik Tok", "Snapchat", "Etsy")
  ),
  activities = data.table(
    variable = c(grep("activities_",names(X_full),value=T)),
    label = c("Facebook", "Twitter", "Social media", "Take selfies", "Cook meals", "Use meal service", "Food delivery/pickup", "Shop online", "Return items",
              "Watch TV", "Streaming service", "Grocery store", "Online groceries", "Exercise", "Talk to friend", "Dating app", "Drive a car", "Use coupons")
  ),
  spend = data.table(
    variable = c(grep("Spend_",names(X_full), value=T)),
    label = c("Delivery $", "Grocery $", "Exercise/Sports $", "Online shopping $", "Offline shopping $")
  ),
  os = data.table(
    variable = c(grep("os_",names(X_full), value=T)),
    label = c("PC", "Mac", "Other OS")
  ),
  education = data.table(
    variable = c(grep("education_", names(X_full), value=T)),
    label = c("Less than high school", "High school", "Some college", "Two year college degree", "Four year college degree", "Master's degree", 
              "Doctoral degree", "Professional degree")
  ),
  price = data.table(
    variable = c(grep("price_", names(X_full), value=T)),
    label = c("Price.Scale_1", "Price.Scale_2", "Price.Scale_3","Price.Scale_4","Price.Scale_5","Price.Scale_6")
  ),
  income = data.table(
    variable = c(grep("income_", names(X_full), value=T)),
    label = c("Less than $10,000", "$10,000 to $19,999", "$20,000 to $29,999", "$30,000 to $39,999", "$40,000 to $49,999", "$50,000 to $59,999",
              "$60,000 to $69,999", "$70,000 to $79,999", "$80,000 to $89,999", "$90,000 to $99,999", "$100,000 to $149,999", "$150,000 or more")
  ),
  other = data.table(
    variable = c("duration","gave.email","dropbox"),
    label = c("Duration", "E-mail provided","Dropbox")
  )
)

## Collect regression coefficients for Age, nonMale, nonWhite
coef_age <- compile_coefs(Y="Age", reg_fn=ols)

coef_nonmale<- compile_coefs(Y="nonMale", reg_fn=glm_logit)
coef_nonmale[chunk=="mobile" & Std > 10, remove:=1]  #cleanup
coef_nonmale = coef_nonmale[is.na(remove)]

coef_nonwhite <- compile_coefs(Y="nonWhite", reg_fn=glm_logit)
coef_nonwhite[rn=='Food_Sodium' , Estimate := NA ]  #cleanup
coef_nonwhite[chunk=="mobile" & Std > 10, remove:=1]  #cleanup
coef_nonwhite = coef_nonwhite[is.na(remove)]

## Plot activities (can be altered to plot any chunk of the data, but swapping the "grepped" items)
activities <- rbind(
  coef_age[grep("activities_",rn,)],
  coef_nonmale[grep("activities_",rn)][,remove:=NULL],
  coef_nonwhite[grep("activities_",rn)][,remove:=NULL]
)
activities$dep_var = factor(activities$dep_var, levels=c("Age","nonMale", "nonWhite"))
levels(activities$dep_var) <- c("Age","Non-Male","Non-White")

p_activities=ggbarplot(activities, 
                       x="label", y="Estimate", 
                       color='p-val<0.05', 
                       fill = "p-val<0.05",
                       facet.by = "dep_var",
                       scales='free',
                       ncol=3,
                       add = "point",
                       add.params = list(color="black"),
                       title="Activities") +
  coord_flip() + 
  geom_hline(yintercept = 0)+ 
  geom_errorbar(mapping=aes(ymin = Estimate - Std, 
                            ymax = Estimate + Std))+ theme(plot.title = element_text(hjust = 0.5))+
  xlab("Estimates") +  theme(axis.title.y=element_blank())
print(p_activities)

setwd(dir)
ggsave("Figure_5.jpg",p_activities, width = 10, height = 6)

# ================================================================
#            Plot var importance tables: Figures 6, S-1
# ----------------------------------------------------------------
### get var importance
get_var_import = function(grf_model, x, type){
  out_var = variable_importance(grf_model)
  out = data.table(variable = names(x), 
                   var_import = as.vector(out_var),
                   type=type)
  setorder(out, -var_import)
  return(out)
}

var_import_grf_demog = get_var_import(fit_grf_demog, X_train_demog,"CF-FD")
var_import_grf = get_var_import(fit_grf, X_train,"CF-NP")
var_import_beat = get_var_import(fit_beat, X_train, "BEAT")
var_import_rfr_x = get_var_import(fit_rfr_x, X_train, "DeBiased")
var_import_all=list('var_import_grf_demog'=var_import_grf_demog,'var_import_grf'=var_import_grf,'var_import_rfr_x'=var_import_rfr_x,'var_import_beat'=var_import_beat)

## plot var importance
TOP_N=20
for(i in names(var_import_all)){
  var_import_all[[i]] = change_var_name(data=var_import_all[[i]], by="variable")
  setorder(var_import_all[[i]], -var_import)
}
p1 = ggbarplot(var_import_all$var_import_grf_demog[1:TOP_N], 
               x="label", 
               y='var_import', 
               fill="label", 
               color='label', 
               xlab="",
               ylab="",
               legend='none',
               title="CF-FD") + 
  coord_flip() + 
  scale_x_discrete(limits=rev)

p2 = ggbarplot(var_import_all$var_import_grf[1:TOP_N], 
               x="label", 
               y='var_import', 
               fill="label", 
               color='label', 
               xlab="",
               ylab="",
               legend='none',
               title="CF-NP") + 
  coord_flip() + 
  scale_x_discrete(limits=rev)


p3 =  ggbarplot(var_import_all$var_import_rfr_x[1:TOP_N], 
                x="label", 
                y='var_import', 
                fill="label", 
                color='label', 
                xlab="",
                ylab="",
                legend='none',
                title="De-Biased") + 
  coord_flip() + 
  scale_x_discrete(limits=rev)

p4 =  ggbarplot(var_import_all$var_import_beat[1:TOP_N], 
                x="label", 
                y='var_import', 
                fill="label", 
                color='label', 
                xlab="",
                ylab="",
                legend='none',
                title="CF-NP") + 
  coord_flip() + 
  scale_x_discrete(limits=rev)

p_var <- ggarrange(p1,p2,p3,p4, ncol = 4)
print(p_var)             #replicating Figure S1
setwd(dir)
ggsave("Figure_S1.jpg",p_var, width = 16, height = 12)

vars_to_keep = data.table(variable = var_import_all$var_import_grf_demog[1:TOP_N]$variable)
vars_to_keep = change_var_name(vars_to_keep, by="variable")

p2 = ggbarplot(merge(vars_to_keep, var_import_all$var_import_grf[,-4,with=F], by="variable", all.x = T,sort=F),
               x="label", 
               y='var_import', 
               fill="label", 
               color='label', 
               legend='none',
               xlab="",
               ylab="",
               title="CF-NP") + 
  coord_flip() + 
  scale_x_discrete(limits=rev)

p3 =  ggbarplot(merge(vars_to_keep, var_import_all$var_import_rfr_x[,-4,with=F], by="variable", all.x = T,sort=F), 
                x="label", 
                y='var_import', 
                fill="label", 
                color='label', 
                legend='none',
                xlab="",
                ylab="",
                title="De-biased") + 
  coord_flip() + 
  scale_x_discrete(limits=rev)

p4 = ggbarplot(merge(vars_to_keep, var_import_all$var_import_beat[,-4,with=F], by="variable", all.x = T, sort=F), 
               x="label", 
               y='var_import', 
               fill="label", 
               color='label', 
               legend='none',
               xlab="",
               ylab="",
               title="BEAT") + 
  coord_flip() + 
  scale_x_discrete(limits=rev)
p_var <- ggarrange(p1,p2,p3,p4, ncol = 4)
print(p_var)             #replicating Figure 6
setwd(dir)
ggsave("Figure_6.jpg",p_var, width = 16, height = 12)


# ================================================================
#           OLS Analysis: Table S-3
# ----------------------------------------------------------------
#for OLS analysis

fmla = as.formula(paste("Y_full ~ W_full"))
lm1 = lm(formula=fmla,data=my_data)
fmla = as.formula(paste("Y_full ~ W_full*nonMale"))
lm2 = lm(formula=fmla,data=my_data)
fmla = as.formula(paste("Y_full ~ W_full*nonWhite"))
lm3 = lm(formula=fmla,data=my_data)
fmla = as.formula(paste("Y_full ~ W_full*Age_gt39"))
lm4 = lm(formula=fmla,data=my_data)
fmla = as.formula(paste("Y_full ~ W_full*Age"))
lm5 = lm(formula=fmla,data=my_data)
stargazer(lm1, lm2, lm3, lm4, lm5, type = "text",    single.row = TRUE) #replicating Table S3




