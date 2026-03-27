### Replication file for:
### Environmental Justice in India: Incidence of Air Pollution from Coal-Fired Power Plants
### Author: Erin York
### Created on: 5/12/20
### Notes: File includes code to replicate figures and tables in the main text and appendix
### for `Environmental Justice in India.' Section 1 introduces helper functions to be
### used in analysis. Section 2 reads in the main dataset. Section 3 includes code for 
### figures and analyses included in the main text. Section 4 includes code for figures
### and analyses used as robustness checks and largely documented in the appendix.


rm(list = ls())

library(plyr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(xtable)
library(lmtest)
library(RColorBrewer)
library(scales)
library(stargazer)


# 1. functions ---------------------------------------------------------------
## create functions for plotting data and predicted values

plot_out<- function(bin, out, run, xname, yname){
  require(ggplot2, dplyr)
  # Number of bins on each side of cutoff
  num <- length(seq(min(run), max(run), bin))
  
  # Create bin identifiers -- add 1000 (or some large number) to the ones above
  # in order to distinguish from those that are below
  b1 <- cut(run, num, labels=FALSE)
  
  # Preprocess your data
  dat.binned <- data_frame(bin = b1, z = run, y = out)
  names(dat.binned) <- c("bin","z","y")
  
  # Bin the data: Compute mean Y and Z plus radii of circles
  # Define radius of each circle as a function of the number of obs in each bin
  # (effectively, visually weighting by the amount of data points in each bin)
  y.mean.binned <- dat.binned %>%
    group_by(bin) %>%
    summarise(y.mean.binned = mean(y))
  
  z.mean.binned <- dat.binned %>%
    group_by(bin) %>%
    summarise(z.mean.binned = mean(z))
  
  radius <- dat.binned %>%
    group_by(bin) %>%
    count()
  
  df<- full_join(y.mean.binned, z.mean.binned) %>%
    left_join(radius)
  
  #  colnames(df)<- c(xname, yname)
  
  ggplot(df, aes(x = z.mean.binned, y = y.mean.binned, size = n)) +
    geom_point() +
    theme_minimal() +
    scale_size_continuous(name = "Obs. per Bin") +
    xlab(xname) +
    ylab(yname) +
    ylim(20, 1.1*(max(df$y.mean.binned)))
}


plot_resid<- function(bin, out, run, xname, yname){
  require(ggplot2, dplyr)
  # Number of bins on each side of cutoff
  num <- length(seq(min(run), max(run), bin))
  
  # Create bin identifiers -- add 1000 (or some large number) to the ones above
  # in order to distinguish from those that are below
  b1 <- cut(run, num, labels=FALSE)
  
  # Preprocess your data
  dat.binned <- data_frame(bin = b1, z = run, y = out)
  names(dat.binned) <- c("bin","z","y")
  
  # Bin the data: Compute mean Y and Z plus radii of circles
  # Define radius of each circle as a function of the number of obs in each bin
  # (effectively, visually weighting by the amount of data points in each bin)
  y.mean.binned <- dat.binned %>%
    group_by(bin) %>%
    summarise(y.mean.binned = mean(y))
  
  z.mean.binned <- dat.binned %>%
    group_by(bin) %>%
    summarise(z.mean.binned = mean(z))
  
  radius <- dat.binned %>%
    group_by(bin) %>%
    count()
  
  df<- full_join(y.mean.binned, z.mean.binned) %>%
    left_join(radius)
  
  #  colnames(df)<- c(xname, yname)
  
  ggplot(df, aes(x = z.mean.binned, y = y.mean.binned, size = n)) +
    geom_point() +
    theme_minimal() +
    scale_size_continuous(name = "Obs. per Bin") +
    xlab(xname) +
    ylab(yname) 
}

dat_out<- function(bin, out, run, var){
  require(dplyr)
  # Number of bins on each side of cutoff
  num <- length(seq(min(run), max(run), bin))
  
  # Create bin identifiers -- add 1000 (or some large number) to the ones above
  # in order to distinguish from those that are below
  b1 <- cut(run, num, labels=FALSE)
  
  # Preprocess your data
  dat.binned <- data_frame(bin = b1, z = run, y = out)
  names(dat.binned) <- c("bin","z","y")
  
  # Bin the data: Compute mean Y and Z plus radii of circles
  # Define radius of each circle as a function of the number of obs in each bin
  # (effectively, visually weighting by the amount of data points in each bin)
  y.mean.binned <- dat.binned %>%
    group_by(bin) %>%
    summarise(y.mean.binned = mean(y))
  
  z.mean.binned <- dat.binned %>%
    group_by(bin) %>%
    summarise(z.mean.binned = mean(z))
  
  radius <- dat.binned %>%
    group_by(bin) %>%
    count()
  
  df<- full_join(y.mean.binned, z.mean.binned) %>%
    left_join(radius) %>%
    mutate(var = var)
  
  #  colnames(df)<- c(xname, yname)
  return(df)
  
}


return_ses <- function(model, clust_var){
  require(sandwich)
  return((coeftest(model, 
                   vcovCL(model, clust_var[as.numeric(as.character(rownames(model.matrix(model))))])))[,2])
}


predict_reg <- function(x, var, cf = T) {
  df<- select(newdat, -contains(paste0(var)))
  colnames(df)[1]<- var
  if(cf ==T){
    return(data_frame(vals = seq(from = 0, to = 1, length.out = 200),
                      pollution = predict(x, newdata = df, interval = "confidence", level=0.95)[,1],
                      lower = predict(x, newdata = df, interval = "confidence", level=0.95)[,2],
                      upper = predict(x, newdata = df, interval = "confidence", level=0.95)[,3],
                      var = gsub("_", " ", var)))
  }
  else{
    return(data_frame(vals = seq(from = 0, to = 1, length.out = 200),
                      pollution = predict(x, newdata = df),
                      var = gsub("_", " ", var)))  
  }
}

predict_reg_pollutants <- function(x, x2, x3, var) {
  df<- select(newdat, -contains(paste0(var)))
  colnames(df)[1]<- var
  
  df2<- data_frame(vals = rep(seq(from = 0, to = 1, length.out = 200), 3),
                   sum_weight = c(predict(x, newdata = df), predict(x2, newdata = df), predict(x3, newdata = df)),
                   var = gsub("_", " ", var),
                   poly = c(rep(c("lin", "quad", "cub"), each = 200)))
  no2_val<- predict(no2, newdata = df2)
  pm25_val<- (predict(pm25, newdata = df2))
  return(df2 %>% mutate(no2 = no2_val, pm25 = pm25_val))
  
}

predict_reg_temporal <- function(x, var) {
  df<- select(newdat, -contains(paste0(var)))
  colnames(df)[1]<- var
  n<- length(x)
  
  out<- sapply(x, predict, newdata = df)
  df2<- data_frame(vals = rep(seq(from = 0, to = 1, length.out = 200), n),
                   sum_weight = c(out),
                   var = gsub("_", " ", var),
                   labs = rep(1:n,each = 200)
  )
  return(df2)
  
}

num<- function(x){as.numeric(as.character(x))}

# 2. read in data ------------------------------------------------------------
## read in data and rename variables

india<- read.csv("replication_data.csv")

names<- data.frame(table(india$st_code, india$st_name)) %>%
  filter(Freq != 0) %>%
  arrange(Var1)

india<- india %>%
  mutate(State = ifelse(is.na(st_name), mapvalues(st_code, from = c(names$Var1, 35),
                                                  to = c(as.character(names$Var2), "Andaman and Nicobar Islands")),
                        as.character(st_name)))



# 3. figures and tables in main analysis -------------------------------------

## Code for Figure 3 (box plot comparison of rural/urban excluding extreme outliers)
india %>%
  filter(sum_weight <= 300) %>%
  mutate(Comm_Type = mapvalues(rural, from = c(0, 1), to = c("Urban", "Rural"))) %>%
  ggplot(aes(x = Comm_Type, y = sum_weight)) +
  geom_boxplot() +
  #  facet_grid(.~Type) +
  theme_minimal() +
  ylab("Weighted Pollution Estimate") +
  xlab("Community Type")

### socioeconomic characteristics

socio <- india %>%
  mutate(Illiteracy = illiterate/total_pop,
         Scheduled_Caste = total_sc_pop/total_pop,
         Scheduled_Tribe = total_st_pop/total_pop,
         No_Assets = assets_none/100,
         Television = assets_tv/100,
         No_TV = 1-Television,
         Bank_Account = assets_bank/100,
         No_Bank_Account = 1-Bank_Account) %>%
  filter(Illiteracy <= 1,
         Scheduled_Caste <= 1) %>%
  filter(!is.na(sum_weight))

d1<- dat_out(bin= .05, out = socio$sum_weight, run = socio$Illiteracy, var = "Illiteracy")
d2<- dat_out(bin= .05, out = socio$sum_weight, run = socio$Scheduled_Tribe, var = "Scheduled Tribe")
d3<- dat_out(bin= .05, out = socio$sum_weight, run = socio$Scheduled_Caste, var = "Scheduled Caste")
d4<- dat_out(bin= .05, out = socio$sum_weight[!is.na(socio$No_TV)], 
             run = socio$No_TV[!is.na(socio$No_TV)], var = "No TV")
d5<- dat_out(bin= .05, out = socio$sum_weight[!is.na(socio$No_Bank_Account)], 
             run = socio$No_Bank_Account[!is.na(socio$No_Bank_Account)], var = "No Bank Account")
d6<- dat_out(bin= .05, out = socio$sum_weight[!is.na(socio$No_Assets)], 
             run = socio$No_Assets[!is.na(socio$No_Assets)], var = "No Assets")

## Code for Figure 4/A30
bind_rows(d1, d2, d3, d4, d5, d6) %>%
  ggplot(aes(x = z.mean.binned, y = y.mean.binned, size = n)) +
  geom_point() +
  theme_minimal() +
  scale_size_continuous(name = "Obs. per Bin", labels = comma) +
  facet_wrap(~ var) +
  xlab("Proportion of Population with Specified Characteristic") +
  ylab("Weighted Pollution Estimate") 


### regression output

illit<- (lm(sum_weight ~ Illiteracy + rural +  State, socio))
sc<- (lm(sum_weight ~ Scheduled_Caste+ rural + State, socio))
st<- (lm(sum_weight ~ Scheduled_Tribe+rural+ State, socio))
asset<- (lm(sum_weight ~ No_Assets+rural+ State, socio))
tv<- (lm(sum_weight ~ No_TV+rural + State, socio))

all_socic_st<- (lm(sum_weight ~ Illiteracy + Scheduled_Caste+ Scheduled_Tribe+ No_TV + rural+ State, socio))
all_socic_asset<- (lm(sum_weight ~ Illiteracy + Scheduled_Caste+ Scheduled_Tribe+ No_Assets + rural+ State, socio))
all_socic_bank<- (lm(sum_weight ~ Illiteracy + Scheduled_Caste+ Scheduled_Tribe+ No_Bank_Account + rural+ State, socio))

illit2<- (lm(sum_weight ~ Illiteracy + I(Illiteracy^2) + Scheduled_Caste+ Scheduled_Tribe+ No_Assets + rural +  State, socio))
sc2<- (lm(sum_weight ~ Scheduled_Caste+ I(Scheduled_Caste^2)+ Illiteracy + Scheduled_Tribe+ No_Assets+ rural + State, socio))
st2<- (lm(sum_weight ~ Scheduled_Tribe+I(Scheduled_Tribe^2)+ Illiteracy + Scheduled_Caste+ No_Assets+rural+ State, socio))
asset2<- (lm(sum_weight ~ No_Assets+I(No_Assets^2)+Illiteracy + Scheduled_Caste+ Scheduled_Tribe+rural+ State, socio))
tv2<- (lm(sum_weight ~ No_TV+ I(No_TV^2)+Illiteracy + Scheduled_Caste+ Scheduled_Tribe +rural + State, socio))
bank2<- (lm(sum_weight ~ No_Bank_Account+ I(No_Bank_Account^2)+Illiteracy + Scheduled_Caste+ Scheduled_Tribe +rural + State, socio))

illit3<- (lm(sum_weight ~ Illiteracy + I(Illiteracy^2)+I(Illiteracy^3)  + Scheduled_Caste+ Scheduled_Tribe+ No_Assets +rural +  State, socio))
sc3<- (lm(sum_weight ~ Scheduled_Caste+ I(Scheduled_Caste^2)+ I(Scheduled_Caste^3)+Illiteracy + Scheduled_Tribe+ No_Assets + rural + State, socio))
st3<- (lm(sum_weight ~ Scheduled_Tribe+I(Scheduled_Tribe^2) +I(Scheduled_Tribe^3)+Illiteracy + Scheduled_Caste+  No_Assets +rural+ State, socio))
asset3<- (lm(sum_weight ~ No_Assets+I(No_Assets^2)+ I(No_Assets^3)+Illiteracy + Scheduled_Caste+ Scheduled_Tribe +rural+ State, socio))
tv3<- (lm(sum_weight ~ No_TV+ I(No_TV^2)+ I(No_TV^3)+Illiteracy + Scheduled_Caste+ Scheduled_Tribe +rural + State, socio))
bank3<- (lm(sum_weight ~ No_Bank_Account+ I(No_Bank_Account^2)+I(No_Bank_Account^3)+Illiteracy + Scheduled_Caste+ Scheduled_Tribe +rural + State, socio))

### clustered errors for regression
all_socio_asset_ses<- return_ses(all_socic_asset, socio$dist_code)
all_socio_st_ses<- return_ses(all_socic_st, socio$dist_code)
sc2_ses<-  return_ses(sc2, socio$dist_code)
sc3_ses<-  return_ses(sc3, socio$dist_code)
st2_ses<-  return_ses(st2, socio$dist_code)
st3_ses<-  return_ses(st3, socio$dist_code)

illit2_ses<-  return_ses(illit2, socio$dist_code)
illit3_ses<-  return_ses(illit3, socio$dist_code)
asset2_ses<-  return_ses(asset2, socio$dist_code)
asset3_ses<-  return_ses(asset3, socio$dist_code)
tv2_ses<-  return_ses(tv2, socio$dist_code)
tv3_ses<-  return_ses(tv3, socio$dist_code)

## Code for Table A2
stargazer( all_socic_asset,all_socic_st, sc2, sc3, st2, st3,
           se = list(all_socio_asset_ses, all_socio_st_ses,
                     sc2_ses, sc3_ses,
                     st2_ses, st3_ses),
           dep.var.labels   = "Weighted Pollution Estimate",
           omit = c("State", "Constant"),
           #omit.yes.no = c( "Yes", "No"),
           add.lines = list(c("State FE", rep("Yes", 16))),
           float = F,
           omit.stat = c("rsq", "f", "adj.rsq", "ser"),
           covariate.labels = c("$Illiteracy$",
                                "$Scheduled Caste$", "$SC^2$", "$SC^3$",
                                "$Scheduled Tribe$", "$ST^2$", "$ST^3$",
                                "$No Assets$", 
                                "$No Television$",
                                "$Rural$"),
           #single.row = TRUE
           no.space = T,
           notes = "Robust standard errors clustered at the district level.")

## Code for Table A3
stargazer(illit2, illit3, asset2, asset3, tv2, tv3,
          se = list(illit2_ses, illit3_ses,
                    asset2_ses, asset3_ses,
                    tv2_ses, tv3_ses),
          dep.var.labels   = "Weighted Pollution Estimate",
          omit = c("State", "Constant"),
          #omit.yes.no = c( "Yes", "No"),
          add.lines = list(c("State FE", rep("Yes", 16))),
          float = F,
          omit.stat = c("rsq", "f", "adj.rsq", "ser"),
          order = c(4,5,6,7,8,9,10,11,1,2,3,12),
          covariate.labels = c("$Illiteracy$", "$Illit^2$", "$Illit^3$",
                               "$Scheduled Caste$",
                               "$Scheduled Tribe$",
                               "$No Assets$", "$No Assets^2$","$No Assets^3$",
                               "$No Television$", "$NoTV^2$", "$NoTV^3$",
                               "$Rural$"),
          #single.row = TRUE
          no.space = T,
          notes = "Robust standard errors clustered at the district level.")


### regression predicted values
newdat<- data_frame(temp = seq(from = 0, to = 1, length.out = 200),
                    Illiteracy = mean(socio$Illiteracy),
                    Scheduled_Caste = mean(socio$Scheduled_Caste),
                    Scheduled_Tribe = mean(socio$Scheduled_Tribe),
                    State = "Puducherry",
                    rural = 1,
                    No_Assets = mean(socio$No_Assets, na.rm = T))

d1<- predict_reg(illit2, var = "Illiteracy", cf = T)
d2<- predict_reg(sc2, var = "Scheduled_Caste")
d3<- predict_reg(st2, var = "Scheduled_Tribe")
d4<- predict_reg(tv2, var = "No_TV")
d5<- predict_reg(asset2, var = "No_Assets")
d6<- predict_reg(bank2, var = "No_Bank_Account")


## Code for Figure 5/A28
bind_rows(d1, d2, d3, d4, d5, d6) %>%
  ggplot(aes(x = vals, y = pollution)) +
  geom_line() +
  geom_line(aes(y = lower), lty = 2, color = "darkgray") +
  geom_line(aes(y = upper), lty = 2, color = "darkgray") +
  theme_minimal() +
  facet_wrap(~ var) +
  xlab("Proportion of Population with Specified Characteristic") +
  ylab("Weighted Pollution Estimate") 


### pollutant predictions

pm25<- lm(PM25 ~ sum_weight, socio)
no2<- lm(NO2 ~ sum_weight, socio)

d1<- predict_reg_pollutants(all_socic_asset, illit2, illit3, var = "Illiteracy")
d2<- predict_reg_pollutants(all_socic_asset, sc2, sc3, var = "Scheduled_Caste")
d3<- predict_reg_pollutants(all_socic_asset, st2, st3, var = "Scheduled_Tribe")
d4<- predict_reg_pollutants(all_socic_st, tv2, tv3, var = "No_TV")
d5<- predict_reg_pollutants(all_socic_asset, asset2, asset3, var = "No_Assets")
d6<- predict_reg_pollutants(all_socic_bank, bank2, bank3, var = "No_Bank_Account")


## Code for Figure 6
bind_rows(d1, d2, d3, d4, d5, d6) %>%
  ggplot(aes(x = vals, y = no2, color = poly)) +
  geom_line() +
  theme_minimal() +
  facet_wrap(~ var) +
  xlab("Proportion of Population with Specified Characteristic") +
  ylab("Predicted NO2 Levels") +
  scale_color_discrete(name = "Polynomial Type", labels = c("Cubic", "Linear", "Quadratic")) 
bind_rows(d1, d2, d3, d4, d5, d6) %>%
  ggplot(aes(x = vals, y = pm25, color = poly)) +
  geom_line() +
  theme_minimal() +
  facet_wrap(~ var) +
  xlab("Proportion of Population with Specified Characteristic") +
  ylab("Predicted PM2.5 Levels") +
  scale_color_discrete(name = "Polynomial Type", labels = c("Cubic", "Linear", "Quadratic")) 


### time trends check using 2001 census data

india<- india %>%
  mutate_each(funs(num), total_pop_2001:illiterate_2001)

socio2 <- india %>%
  filter(total_pop_2001 > 0) %>%
  mutate(Illiteracy_2001 = illiterate_2001/total_pop_2001,
         Scheduled_Caste = total_sc_pop_2001/total_pop_2001,
         Scheduled_Tribe = total_st_pop_2001/total_pop_2001) 


illit<- (lm(sum_weight ~ Illiteracy_2001 + rural +  State, 
            filter(socio2, Illiteracy_2001 <= 1)))
sc<- (lm(sum_weight ~ Scheduled_Caste+ rural + State, socio2))
st<- (lm(sum_weight ~ Scheduled_Tribe+rural+ State, socio2))
all_2001<- lm(sum_weight ~ Illiteracy_2001 + Scheduled_Caste + Scheduled_Tribe + rural +  State, 
              filter(socio2, Illiteracy_2001 <= 1))

## Code for Table 2
stargazer(illit, sc, st, all_2001,
                     dep.var.labels   = "Weighted Pollution Estimate",
                     omit = c("State", "Constant"),
                     #omit.yes.no = c( "Yes", "No"),
                     add.lines = list(c("State FE", rep("Yes", 4))),
                     float = F,
                     omit.stat = c("rsq", "f", "adj.rsq", "ser"),
                     covariate.labels = c("2001 Illiteracy",
                                          "2001 Scheduled Caste",
                                          "2001 Scheduled Tribe", 
                                          "Rural"),
                     #single.row = TRUE
                     no.space = T)


# 4. additional figures and tables -------------------------------------------

## Code for Table A1
sum_var <- c(NA, "sum_weight", "total_pop",  "Scheduled_Caste", "Scheduled_Tribe",
             "Illiteracy", "No_Assets", "Television")
var.label <- c("Locations", "Weighted Pollution", "Population",
               "Scheduled Caste", "Scheduled Tribe", "Iliterate", "No Assets",
               "Has Television")

sum.tab <- data.frame(Variable = var.label,
                      Mean = NA,   # full dataset
                      Mean.r = NA, # rural
                      Mean.u = NA, # urban
                      stringsAsFactors = FALSE)

for (i in 2:nrow(sum.tab)){
  
  m1 <- mean(socio[ , which(colnames(socio) == sum_var[i])], na.rm = TRUE)
  sd1 <- sd(socio[ , which(colnames(socio) == sum_var[i])], na.rm = TRUE)
  if (m1 > 1000){
    m1 <- prettyNum(round(m1), big.mark = ",")
    sd1 <- prettyNum(round(sd1), big.mark = ",")
  } else {
    m1 <- round(m1, digits = 2)
    sd1 <- round(sd1, digits = 2)
  }
  sum.tab$Mean[i] <- paste0("\\makecell{", m1, " \\\\\n " , "(" , sd1, ")}")
  
  m2 <- mean(socio[ socio$rural == 1, which(colnames(socio) == sum_var[i])], na.rm = TRUE)
  sd2 <- sd(socio[ socio$rural == 1, which(colnames(socio) == sum_var[i])], na.rm = TRUE)
  if (m2 > 1000){
    m2 <- prettyNum(round(m2), big.mark = ",")
    sd2 <- prettyNum(round(sd2), big.mark = ",")
  } else {
    m2 <- round(m2, digits = 2)
    sd2 <- round(sd2, digits = 2)
  }
  sum.tab$Mean.r[i] <- paste0("\\makecell{", m2, " \\\\\n " , "(" , sd2, ")}")
  
  m3 <- mean(socio[ socio$rural == 0, which(colnames(socio) == sum_var[i])], na.rm = TRUE)
  sd3 <- sd(socio[ socio$rural == 0, which(colnames(socio) == sum_var[i])], na.rm = TRUE)
  if (m3 > 1000){
    m3 <- prettyNum(round(m3), big.mark = ",")
    sd3 <- prettyNum(round(sd3), big.mark = ",")
  } else {
    m3 <- round(m3, digits = 2)
    sd3 <- round(sd3, digits = 2)
  }
  sum.tab$Mean.u[i] <- paste0("\\makecell{", m3, " \\\\\n " , "(" , sd3, ")}")
}

sum.tab[1, ] <- c("Locations", prettyNum(nrow(socio), big.mark = ","), prettyNum(sum(socio$rural == 1), big.mark = ","), 
                  prettyNum(sum(socio$rural == 0), big.mark = ","))

addtorow <- list()
addtorow$pos <- list(0, 1, 3)
addtorow$command <- c("& Full Dataset & Rural & Urban \\\\\n", " \\\\\n", " \\\\\n")

print(xtable(sum.tab, auto = TRUE, align = c("c", "l", "c", "c", "c")), sanitize.text.function = function(x) {x},
      include.rownames = FALSE, include.colnames = FALSE, add.to.row = addtorow,
      floating = FALSE, booktabs = TRUE)


## Code for Figure A3
dat<- india %>%
  filter(!is.na(sum_weight), !is.na(total_pop)) %>%
  mutate(logpop = log(total_pop))

plot_out(bin= 1, out = dat$sum_weight, run = dat$logpop, 
         xname = "Log Population", yname = "Weighted Pollution Estimate")


### look at rural only communities

dat<- socio %>% filter(rural == 1)

all_socic_st<- (lm(sum_weight ~ Illiteracy + Scheduled_Caste+ Scheduled_Tribe+ No_TV + rural+ State, dat))
all_socic_asset<- (lm(sum_weight ~ Illiteracy + Scheduled_Caste+ Scheduled_Tribe+ No_Assets + rural+ State, dat))

illit2<- (lm(sum_weight ~ Illiteracy + I(Illiteracy^2) + Scheduled_Caste+ Scheduled_Tribe+ No_Assets + rural +  State, dat))
sc2<- (lm(sum_weight ~ Scheduled_Caste+ I(Scheduled_Caste^2)+ Illiteracy + Scheduled_Tribe+ No_Assets+ rural + State, dat))
st2<- (lm(sum_weight ~ Scheduled_Tribe+I(Scheduled_Tribe^2)+ Illiteracy + Scheduled_Caste+ No_Assets+rural+ State, dat))
asset2<- (lm(sum_weight ~ No_Assets+I(No_Assets^2)+Illiteracy + Scheduled_Caste+ Scheduled_Tribe+rural+ State, dat))
tv2<- (lm(sum_weight ~ No_TV+ I(No_TV^2)+Illiteracy + Scheduled_Caste+ Scheduled_Tribe +rural + State, dat))
bank2<- (lm(sum_weight ~ No_Bank_Account+ I(No_Bank_Account^2)+Illiteracy + Scheduled_Caste+ Scheduled_Tribe +rural + State, dat))

illit3<- (lm(sum_weight ~ Illiteracy + I(Illiteracy^2)+I(Illiteracy^3)  + Scheduled_Caste+ Scheduled_Tribe+ No_Assets +rural +  State, dat))
sc3<- (lm(sum_weight ~ Scheduled_Caste+ I(Scheduled_Caste^2)+ I(Scheduled_Caste^3)+Illiteracy + Scheduled_Tribe+ No_Assets + rural + State, dat))
st3<- (lm(sum_weight ~ Scheduled_Tribe+I(Scheduled_Tribe^2) +I(Scheduled_Tribe^3)+Illiteracy + Scheduled_Caste+  No_Assets +rural+ State, dat))
asset3<- (lm(sum_weight ~ No_Assets+I(No_Assets^2)+ I(No_Assets^3)+Illiteracy + Scheduled_Caste+ Scheduled_Tribe +rural+ State, dat))
tv3<- (lm(sum_weight ~ No_TV+ I(No_TV^2)+ I(No_TV^3)+Illiteracy + Scheduled_Caste+ Scheduled_Tribe +rural + State, dat))
bank3<- (lm(sum_weight ~ No_Bank_Account+ I(No_Bank_Account^2)+I(No_Bank_Account^3)+Illiteracy + Scheduled_Caste+ Scheduled_Tribe +rural + State, dat))

### clustered errors for regression
all_socio_asset_ses<- return_ses(all_socic_asset, dat$dist_code)
all_socio_st_ses<- return_ses(all_socic_st, dat$dist_code)
sc2_ses<-  return_ses(sc2, dat$dist_code)
sc3_ses<-  return_ses(sc3, dat$dist_code)
st2_ses<-  return_ses(st2, dat$dist_code)
st3_ses<-  return_ses(st3, dat$dist_code)

illit2_ses<-  return_ses(illit2, dat$dist_code)
illit3_ses<-  return_ses(illit3, dat$dist_code)
asset2_ses<-  return_ses(asset2, dat$dist_code)
asset3_ses<-  return_ses(asset3, dat$dist_code)
tv2_ses<-  return_ses(tv2, dat$dist_code)
tv3_ses<-  return_ses(tv3, dat$dist_code)

## Code for Table A4
stargazer( all_socic_asset,all_socic_st, sc2, sc3, st2, st3,
                      se = list(all_socio_asset_ses, all_socio_st_ses,
                                sc2_ses, sc3_ses,
                                st2_ses, st3_ses),
                      dep.var.labels   = "Weighted Pollution Estimate",
                      omit = c("State", "Constant", "rural"),
                      #omit.yes.no = c( "Yes", "No"),
                      add.lines = list(c("State FE", rep("Yes", 16))),
                      float = F,
                      omit.stat = c("rsq", "f", "adj.rsq", "ser"),
                      covariate.labels = c("$Illiteracy$",
                                           "$Scheduled Caste$", "$SC^2$", "$SC^3$",
                                           "$Scheduled Tribe$", "$ST^2$", "$ST^3$",
                                           "$No Assets$", 
                                           "$No Television$"),
                      #single.row = TRUE
                      no.space = T,
                      notes = "Robust standard errors clustered at the district level."
)

## Code for Table A5
stargazer(illit2, illit3, asset2, asset3, tv2, tv3,
                     se = list(illit2_ses, illit3_ses,
                               asset2_ses, asset3_ses,
                               tv2_ses, tv3_ses),
                     dep.var.labels   = "Weighted Pollution Estimate",
                     omit = c("State", "Constant", "rural"),
                     #omit.yes.no = c( "Yes", "No"),
                     add.lines = list(c("State FE", rep("Yes", 16))),
                     float = F,
                     omit.stat = c("rsq", "f", "adj.rsq", "ser"),
                     order = c(4,5,6,7,8,9,10,11,1,2,3,12),
                     covariate.labels = c("$Illiteracy$", "$Illit^2$", "$Illit^3$",
                                          "$Scheduled Caste$",
                                          "$Scheduled Tribe$",
                                          "$No Assets$", "$No Assets^2$","$No Assets^3$",
                                          "$No Television$", "$NoTV^2$", "$NoTV^3$"),
                     #single.row = TRUE
                     no.space = T,
                     notes = "Robust standard errors clustered at the district level."
)


### robustness check using only communities in coal-producing areas

coal<- c("Maharashtra", "West Bengal", "Jharkhand", "Odisha", "Andhra Pradesh", "Madhya Pradesh")
dat<- socio %>% filter(State %in% coal)


all_socic_st<- (lm(sum_weight ~ Illiteracy + Scheduled_Caste+ Scheduled_Tribe+ No_TV + rural+ State, dat))
all_socic_asset<- (lm(sum_weight ~ Illiteracy + Scheduled_Caste+ Scheduled_Tribe+ No_Assets + rural+ State, dat))

illit2<- (lm(sum_weight ~ Illiteracy + I(Illiteracy^2) + Scheduled_Caste+ Scheduled_Tribe+ No_Assets + rural +  State, dat))
sc2<- (lm(sum_weight ~ Scheduled_Caste+ I(Scheduled_Caste^2)+ Illiteracy + Scheduled_Tribe+ No_Assets+ rural + State, dat))
st2<- (lm(sum_weight ~ Scheduled_Tribe+I(Scheduled_Tribe^2)+ Illiteracy + Scheduled_Caste+ No_Assets+rural+ State, dat))
asset2<- (lm(sum_weight ~ No_Assets+I(No_Assets^2)+Illiteracy + Scheduled_Caste+ Scheduled_Tribe+rural+ State, dat))
tv2<- (lm(sum_weight ~ No_TV+ I(No_TV^2)+Illiteracy + Scheduled_Caste+ Scheduled_Tribe +rural + State, dat))
bank2<- (lm(sum_weight ~ No_Bank_Account+ I(No_Bank_Account^2)+Illiteracy + Scheduled_Caste+ Scheduled_Tribe +rural + State, dat))

illit3<- (lm(sum_weight ~ Illiteracy + I(Illiteracy^2)+I(Illiteracy^3)  + Scheduled_Caste+ Scheduled_Tribe+ No_Assets +rural +  State, dat))
sc3<- (lm(sum_weight ~ Scheduled_Caste+ I(Scheduled_Caste^2)+ I(Scheduled_Caste^3)+Illiteracy + Scheduled_Tribe+ No_Assets + rural + State, dat))
st3<- (lm(sum_weight ~ Scheduled_Tribe+I(Scheduled_Tribe^2) +I(Scheduled_Tribe^3)+Illiteracy + Scheduled_Caste+  No_Assets +rural+ State, dat))
asset3<- (lm(sum_weight ~ No_Assets+I(No_Assets^2)+ I(No_Assets^3)+Illiteracy + Scheduled_Caste+ Scheduled_Tribe +rural+ State, dat))
tv3<- (lm(sum_weight ~ No_TV+ I(No_TV^2)+ I(No_TV^3)+Illiteracy + Scheduled_Caste+ Scheduled_Tribe +rural + State, dat))
bank3<- (lm(sum_weight ~ No_Bank_Account+ I(No_Bank_Account^2)+I(No_Bank_Account^3)+Illiteracy + Scheduled_Caste+ Scheduled_Tribe +rural + State, dat))

### clustered errors for regression
all_socio_asset_ses<- return_ses(all_socic_asset, dat$dist_code)
all_socio_st_ses<- return_ses(all_socic_st, dat$dist_code)
sc2_ses<-  return_ses(sc2, dat$dist_code)
sc3_ses<-  return_ses(sc3, dat$dist_code)
st2_ses<-  return_ses(st2, dat$dist_code)
st3_ses<-  return_ses(st3, dat$dist_code)

illit2_ses<-  return_ses(illit2, dat$dist_code)
illit3_ses<-  return_ses(illit3, dat$dist_code)
asset2_ses<-  return_ses(asset2, dat$dist_code)
asset3_ses<-  return_ses(asset3, dat$dist_code)
tv2_ses<-  return_ses(tv2, dat$dist_code)
tv3_ses<-  return_ses(tv3, dat$dist_code)


## Code for Table A6
stargazer(all_socic_asset,all_socic_st, sc2, sc3, st2, st3,
                      se = list(all_socio_asset_ses, all_socio_st_ses,
                                sc2_ses, sc3_ses,
                                st2_ses, st3_ses),
                      dep.var.labels   = "Weighted Pollution Estimate",
                      omit = c("State", "Constant", "rural"),
                      #omit.yes.no = c( "Yes", "No"),
                      add.lines = list(c("State FE", rep("Yes", 16))),
                      float = F,
                      omit.stat = c("rsq", "f", "adj.rsq", "ser"),
                      covariate.labels = c("$Illiteracy$",
                                           "$Scheduled Caste$", "$SC^2$", "$SC^3$",
                                           "$Scheduled Tribe$", "$ST^2$", "$ST^3$",
                                           "$No Assets$", 
                                           "$No Television$"),
                      #single.row = TRUE
                      no.space = T,
                      notes = "Robust standard errors clustered at the district level."
)


## Code for Table A7
stargazer(illit2, illit3, asset2, asset3, tv2, tv3,
                     se = list(illit2_ses, illit3_ses,
                               asset2_ses, asset3_ses,
                               tv2_ses, tv3_ses),
                     dep.var.labels   = "Weighted Pollution Estimate",
                     omit = c("State", "Constant", "rural"),
                     #omit.yes.no = c( "Yes", "No"),
                     add.lines = list(c("State FE", rep("Yes", 16))),
                     float = F,
                     omit.stat = c("rsq", "f", "adj.rsq", "ser"),
                     order = c(4,5,6,7,8,9,10,11,1,2,3,12),
                     covariate.labels = c("$Illiteracy$", "$Illit^2$", "$Illit^3$",
                                          "$Scheduled Caste$",
                                          "$Scheduled Tribe$",
                                          "$No Assets$", "$No Assets^2$","$No Assets^3$",
                                          "$No Television$", "$NoTV^2$", "$NoTV^3$"),
                     #single.row = TRUE
                     no.space = T,
                     notes = "Robust standard errors clustered at the district level."
)


### robustness: remove observations w/in 10 km of plant

filter(india, distance < 10) %>% dim()
# 31000 observations - around 5% of the total dataset

distant<- india %>% filter(distance > 10) %>%
  mutate(Illiteracy = illiterate/total_pop,
         Scheduled_Caste = total_sc_pop/total_pop,
         Scheduled_Tribe = total_st_pop/total_pop,
         No_Assets = assets_none/100,
         Television = assets_tv/100,
         No_TV = 1-Television,
         Bank_Account = assets_bank/100,
         No_Bank_Account = 1-Bank_Account) %>%
  filter(Illiteracy <= 1,
         Scheduled_Caste <= 1) %>%
  filter(!is.na(sum_weight))

d1<- dat_out(bin= .05, out = distant$sum_weight, run = distant$Illiteracy, var = "Illiteracy")
d2<- dat_out(bin= .05, out = distant$sum_weight, run = distant$Scheduled_Tribe, var = "Scheduled Tribe")
d3<- dat_out(bin= .05, out = distant$sum_weight, run = distant$Scheduled_Caste, var = "Scheduled Caste")
d4<- dat_out(bin= .05, out = distant$sum_weight[!is.na(distant$No_TV)], 
             run = distant$No_TV[!is.na(distant$No_TV)], var = "No TV")
d5<- dat_out(bin= .05, out = distant$sum_weight[!is.na(distant$No_Bank_Account)], 
             run = distant$No_Bank_Account[!is.na(distant$No_Bank_Account)], var = "No Bank Account")
d6<- dat_out(bin= .05, out = distant$sum_weight[!is.na(distant$No_Assets)], 
             run = distant$No_Assets[!is.na(distant$No_Assets)], var = "No Assets")

## Code for Figure A27
bind_rows(d1, d2, d3, d4, d5, d6) %>%
  ggplot(aes(x = z.mean.binned, y = y.mean.binned, size = n)) +
  geom_point() +
  theme_minimal() +
  scale_size_continuous(name = "Obs. per Bin") +
  facet_wrap(~ var) +
  xlab("Proportion of Population with Specified Characteristic") +
  ylab("Weighted Pollution Estimate") 


illit2<- (lm(sum_weight ~ Illiteracy + I(Illiteracy^2) + Scheduled_Caste+ Scheduled_Tribe+ No_Assets + rural +  State, distant))
sc2<- (lm(sum_weight ~ Scheduled_Caste+ I(Scheduled_Caste^2)+ Illiteracy + Scheduled_Tribe+ No_Assets+ rural + State, distant))
st2<- (lm(sum_weight ~ Scheduled_Tribe+I(Scheduled_Tribe^2)+ Illiteracy + Scheduled_Caste+ No_Assets+rural+ State, distant))
asset2<- (lm(sum_weight ~ No_Assets+I(No_Assets^2)+Illiteracy + Scheduled_Caste+ Scheduled_Tribe+rural+ State, distant))
tv2<- (lm(sum_weight ~ No_TV+ I(No_TV^2)+Illiteracy + Scheduled_Caste+ Scheduled_Tribe +rural + State, distant))
bank2<- (lm(sum_weight ~ No_Bank_Account+ I(No_Bank_Account^2)+Illiteracy + Scheduled_Caste+ Scheduled_Tribe +rural + State, distant))


### plot regression output
newdat<- data_frame(temp = seq(from = 0, to = 1, length.out = 200),
                    Illiteracy = mean(socio$Illiteracy),
                    Scheduled_Caste = mean(socio$Scheduled_Caste),
                    Scheduled_Tribe = mean(socio$Scheduled_Tribe),
                    State = "Puducherry",
                    rural = 1,
                    No_Assets = mean(socio$No_Assets, na.rm = T))

d1<- predict_reg(illit2, var = "Illiteracy", cf = T)
d2<- predict_reg(sc2, var = "Scheduled_Caste")
d3<- predict_reg(st2, var = "Scheduled_Tribe")
d4<- predict_reg(tv2, var = "No_TV")
d5<- predict_reg(asset2, var = "No_Assets")
d6<- predict_reg(bank2, var = "No_Bank_Account")

## Code for Figure A26
bind_rows(d1, d2, d3, d4, d5, d6) %>%
  ggplot(aes(x = vals, y = pollution)) +
  geom_line() +
  geom_line(aes(y = lower), lty = 2, color = "darkgray") +
  geom_line(aes(y = upper), lty = 2, color = "darkgray") +
  theme_minimal() +
  facet_wrap(~ var) +
  xlab("Proportion of Population with Specified Characteristic") +
  ylab("Weighted Pollution Estimate") 


### robustness: half size grid cell

d1<- dat_out(bin= .05, out = socio$sum_weight_05, run = socio$Illiteracy, var = "Illiteracy")
d2<- dat_out(bin= .05, out = socio$sum_weight_05, run = socio$Scheduled_Tribe, var = "Scheduled Tribe")
d3<- dat_out(bin= .05, out = socio$sum_weight_05, run = socio$Scheduled_Caste, var = "Scheduled Caste")
d4<- dat_out(bin= .05, out = socio$sum_weight_05[!is.na(socio$No_TV)], 
             run = socio$No_TV[!is.na(socio$No_TV)], var = "No TV")
d5<- dat_out(bin= .05, out = socio$sum_weight_05[!is.na(socio$No_Bank_Account)], 
             run = socio$No_Bank_Account[!is.na(socio$No_Bank_Account)], var = "No Bank Account")
d6<- dat_out(bin= .05, out = socio$sum_weight_05[!is.na(socio$No_Assets)], 
             run = socio$No_Assets[!is.na(socio$No_Assets)], var = "No Assets")

## Code for Figure A31
bind_rows(d1, d2, d3, d4, d5, d6) %>%
  ggplot(aes(x = z.mean.binned, y = y.mean.binned, size = n)) +
  geom_point() +
  theme_minimal() +
  scale_size_continuous(name = "Obs. per Bin") +
  facet_wrap(~ var) +
  xlab("Proportion of Population with Specified Characteristic") +
  ylab("Weighted Pollution Estimate") 

### regression output - half size grid cell

illit2<- (lm(sum_weight_05 ~ Illiteracy + I(Illiteracy^2) + Scheduled_Caste+ Scheduled_Tribe+ No_Assets + rural +  State, socio))
sc2<- (lm(sum_weight_05 ~ Scheduled_Caste+ I(Scheduled_Caste^2)+ Illiteracy + Scheduled_Tribe+ No_Assets+ rural + State, socio))
st2<- (lm(sum_weight_05 ~ Scheduled_Tribe+I(Scheduled_Tribe^2)+ Illiteracy + Scheduled_Caste+ No_Assets+rural+ State, socio))
asset2<- (lm(sum_weight_05 ~ No_Assets+I(No_Assets^2)+Illiteracy + Scheduled_Caste+ Scheduled_Tribe+rural+ State, socio))
tv2<- (lm(sum_weight_05 ~ No_TV+ I(No_TV^2)+Illiteracy + Scheduled_Caste+ Scheduled_Tribe +rural + State, socio))
bank2<- (lm(sum_weight_05 ~ No_Bank_Account+ I(No_Bank_Account^2)+Illiteracy + Scheduled_Caste+ Scheduled_Tribe +rural + State, socio))


d1<- predict_reg(illit2, var = "Illiteracy", cf = T)
d2<- predict_reg(sc2, var = "Scheduled_Caste")
d3<- predict_reg(st2, var = "Scheduled_Tribe")
d4<- predict_reg(tv2, var = "No_TV")
d5<- predict_reg(asset2, var = "No_Assets")
d6<- predict_reg(bank2, var = "No_Bank_Account")

## Code for Figure A29
bind_rows(d1, d2, d3, d4, d5, d6) %>%
  ggplot(aes(x = vals, y = pollution)) +
  geom_line() +
  geom_line(aes(y = lower), lty = 2, color = "darkgray") +
  geom_line(aes(y = upper), lty = 2, color = "darkgray") +
  theme_minimal() +
  facet_wrap(~ var) +
  xlab("Proportion of Population with Specified Characteristic") +
  ylab("Weighted Pollution Estimate") 


### residual plots


st_model<- (lm(sum_weight ~  State, socio))
asset_st1<- (lm(sum_weight ~ State, filter(socio, !is.na(No_Assets))))
tv_st1<- (lm(sum_weight ~  State, filter(socio, !is.na(No_TV))))
bank_st1<- lm(sum_weight ~ State, filter(socio, !is.na(No_Bank_Account)))

illit_st<- (lm(Illiteracy ~  State, socio))
sc_st<- (lm(Scheduled_Caste ~  State, socio))
st_st<- (lm(Scheduled_Tribe ~  State, socio))
asset_st2<- (lm(No_Assets ~  State, socio))
tv_st2<- (lm(No_TV ~  State, socio))
bank_st2<- (lm(No_Bank_Account ~  State, socio))

## Code for Figure A32
plot_resid(.05, st_model$residuals, illit_st$residuals, "Fitted Residuals - Illiteracy", "Fitted Residuals - Pollution")

## Code for Figure A33
plot_resid(.05, st_model$residuals, sc_st$residuals, "Fitted Residuals - Scheduled Caste", "Fitted Residuals - Pollution")

## Code for Figure A34
plot_resid(.05, st_model$residuals, st_st$residuals, "Fitted Residuals - Scheduled Tribe", "Fitted Residuals - Pollution")

## Code for Figure A35
plot_resid(.05, asset_st1$residuals, asset_st2$residuals, "Fitted Residuals - No Assets", "Fitted Residuals - Pollution")

## Code for Figure A36
plot_resid(.05, tv_st1$residuals, tv_st2$residuals, "Fitted Residuals - Television", "Fitted Residuals - Pollution")

## Code for Figure A37
plot_resid(.05, bank_st1$residuals, bank_st2$residuals, "Fitted Residuals - Bank Account", "Fitted Residuals - Pollution")


### robustness: temporal subsets

x<- c("sum_weight", "sumwt_b01", "sumwt_a01", "sumwt_b09", "sumwt_a09")
y<- c("All", "Pre-2001", "Post-2001", "Pre-2009", "Post-2009")
dat<- select(socio, sum_weight, sumwt_b01, sumwt_a01, sumwt_a09, sumwt_b09) %>%
  gather(key = plants, value = pollution) %>% 
  mutate(plants2 = mapvalues(plants, x, y),
         temp = factor(plants2, levels= y)) %>% tbl_df()

## Code for Figure A38
ggplot(dat, aes(x = temp, y = pollution)) +
  geom_boxplot() +
  ylim(c(0, 300)) +
  ylab("Weighted Pollution Estimate") +
  xlab("Coal Plants Included") +
  theme_minimal()


illit2<- (lm(sum_weight ~ Illiteracy + I(Illiteracy^2) + Scheduled_Caste+ Scheduled_Tribe+ No_Assets + rural +  State, socio))
illit2a<- (lm(sumwt_a01 ~ Illiteracy + I(Illiteracy^2) + Scheduled_Caste+ Scheduled_Tribe+ No_Assets + rural +  State, socio))
illit2b<- (lm(sumwt_b01 ~ Illiteracy + I(Illiteracy^2) + Scheduled_Caste+ Scheduled_Tribe+ No_Assets + rural +  State, socio))
illit2c<- (lm(sumwt_a09 ~ Illiteracy + I(Illiteracy^2) + Scheduled_Caste+ Scheduled_Tribe+ No_Assets + rural +  State, socio))
illit2d<- (lm(sumwt_b09 ~ Illiteracy + I(Illiteracy^2) + Scheduled_Caste+ Scheduled_Tribe+ No_Assets + rural +  State, socio))


sc2<- (lm(sum_weight ~ Scheduled_Caste+ I(Scheduled_Caste^2)+ Illiteracy + Scheduled_Tribe+ No_Assets+ rural + State, socio))
sc2a<- (lm(sumwt_a01 ~ Scheduled_Caste+ I(Scheduled_Caste^2)+ Illiteracy + Scheduled_Tribe+ No_Assets+ rural + State, socio))
sc2b<- (lm(sumwt_b01 ~ Scheduled_Caste+ I(Scheduled_Caste^2)+ Illiteracy + Scheduled_Tribe+ No_Assets+ rural + State, socio))
sc2c<- (lm(sumwt_a09 ~ Scheduled_Caste+ I(Scheduled_Caste^2)+ Illiteracy + Scheduled_Tribe+ No_Assets+ rural + State, socio))
sc2d<- (lm(sumwt_b09 ~ Scheduled_Caste+ I(Scheduled_Caste^2)+ Illiteracy + Scheduled_Tribe+ No_Assets+ rural + State, socio))


st2<- (lm(sum_weight ~ Scheduled_Tribe+I(Scheduled_Tribe^2)+ Illiteracy + Scheduled_Caste+ No_Assets+rural+ State, socio))
st2a<- (lm(sumwt_a01 ~ Scheduled_Tribe+I(Scheduled_Tribe^2)+ Illiteracy + Scheduled_Caste+ No_Assets+rural+ State, socio))
st2b<- (lm(sumwt_b01 ~ Scheduled_Tribe+I(Scheduled_Tribe^2)+ Illiteracy + Scheduled_Caste+ No_Assets+rural+ State, socio))
st2c<- (lm(sumwt_a09 ~ Scheduled_Tribe+I(Scheduled_Tribe^2)+ Illiteracy + Scheduled_Caste+ No_Assets+rural+ State, socio))
st2d<- (lm(sumwt_b09 ~ Scheduled_Tribe+I(Scheduled_Tribe^2)+ Illiteracy + Scheduled_Caste+ No_Assets+rural+ State, socio))

asset2<- (lm(sum_weight ~ No_Assets+I(No_Assets^2)+Illiteracy + Scheduled_Caste+ Scheduled_Tribe+rural+ State, socio))
asset2a<- (lm(sumwt_a01 ~ No_Assets+I(No_Assets^2)+Illiteracy + Scheduled_Caste+ Scheduled_Tribe+rural+ State, socio))
asset2b<- (lm(sumwt_b01 ~ No_Assets+I(No_Assets^2)+Illiteracy + Scheduled_Caste+ Scheduled_Tribe+rural+ State, socio))
asset2c<- (lm(sumwt_a09 ~ No_Assets+I(No_Assets^2)+Illiteracy + Scheduled_Caste+ Scheduled_Tribe+rural+ State, socio))
asset2d<- (lm(sumwt_b09 ~ No_Assets+I(No_Assets^2)+Illiteracy + Scheduled_Caste+ Scheduled_Tribe+rural+ State, socio))

tv2<- (lm(sum_weight ~ No_TV+ I(No_TV^2)+Illiteracy + Scheduled_Caste+ Scheduled_Tribe +rural + State, socio))
tv2a<- (lm(sumwt_a01 ~ No_TV+ I(No_TV^2)+Illiteracy + Scheduled_Caste+ Scheduled_Tribe +rural + State, socio))
tv2b<- (lm(sumwt_b01 ~ No_TV+ I(No_TV^2)+Illiteracy + Scheduled_Caste+ Scheduled_Tribe +rural + State, socio))
tv2c<- (lm(sumwt_a09 ~ No_TV+ I(No_TV^2)+Illiteracy + Scheduled_Caste+ Scheduled_Tribe +rural + State, socio))
tv2d<- (lm(sumwt_b09 ~ No_TV+ I(No_TV^2)+Illiteracy + Scheduled_Caste+ Scheduled_Tribe +rural + State, socio))

bank2<- (lm(sum_weight ~ No_Bank_Account+ I(No_Bank_Account^2)+Illiteracy + Scheduled_Caste+ Scheduled_Tribe +rural + State, socio))
bank2a<- (lm(sumwt_a01 ~ No_Bank_Account+ I(No_Bank_Account^2)+Illiteracy + Scheduled_Caste+ Scheduled_Tribe +rural + State, socio))
bank2b<- (lm(sumwt_b01 ~ No_Bank_Account+ I(No_Bank_Account^2)+Illiteracy + Scheduled_Caste+ Scheduled_Tribe +rural + State, socio))
bank2c<- (lm(sumwt_a09 ~ No_Bank_Account+ I(No_Bank_Account^2)+Illiteracy + Scheduled_Caste+ Scheduled_Tribe +rural + State, socio))
bank2d<- (lm(sumwt_b09 ~ No_Bank_Account+ I(No_Bank_Account^2)+Illiteracy + Scheduled_Caste+ Scheduled_Tribe +rural + State, socio))



ill<- list(illit2, illit2a, illit2b, illit2c, illit2d)
st<- list(st2, st2a, st2b, st2c, st2d)
sc<- list(sc2, sc2a, sc2b, sc2c, sc2d)
tv<- list(tv2, tv2a, tv2b, tv2c, tv2d)
asset<- list(asset2, asset2a, asset2b, asset2c, asset2d)
bank<- list(bank2, bank2a, bank2b, bank2c, bank2d)

vars<- c("Illiteracy", "Scheduled_Tribe", "Scheduled_Caste", "No_TV", "No_Assets", "No_Bank_Account")
a<- predict_reg_temporal(ill, vars[1])
b<- predict_reg_temporal(st, vars[2])
c<- predict_reg_temporal(sc, vars[3])
d<- predict_reg_temporal(tv, vars[4])
e<- predict_reg_temporal(asset, vars[5])
f<- predict_reg_temporal(bank, vars[6])

out2<- bind_rows(a, b, c, d, e, f)

pal<- RColorBrewer::brewer.pal(6, "Paired")[2:6]

## Code for Figure A39
ggplot(out2, aes(x = vals, y = sum_weight, color = factor(labs, levels = c(1,3,2,5,4)))) +
  geom_line() +
  theme_minimal() +
  facet_wrap(~ var) +
  scale_color_manual(values = pal,
                     name = "", labels = c("All plants", "Pre-2001 plants", "Post-2001 plants", 
                                           "Pre-2009 plants","Post-2009 plants")) +
  ylab("Predicted Weight Pollution Estimate")


