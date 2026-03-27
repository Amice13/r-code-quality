### This program computes the various matrices with a 45km cut-off. 
# The matrices with the other cut-offs are easily obtained by changing the csv inputs and changing _45 in the code with the appropriate number (7_25,20,26)
# in the programs, adjust paths as needed. Do not run WC with other cut_offs. 
### This program also computes the spatial lags with a 45km and 60km cut-off for the third-order modeland generates the results for the first-stage regressions.


rm(list=ls())   # Clear memory

# load necessary libraries

library(spdep)
library(Matrix)
library(foreign)
library(here)

main_dir <- here()
main_dir <- sub("/code$", "", main_dir) # Chack that this provides the path to all replication files. Otherwise adjust by hand.

######################
# Creation of matrices
######################

# Load the data

data <- read.dta(paste0(main_dir,"/finaldata/data_stata_baseline_45.dta"))

# Tables containing the unbalanced panel structure

tableau_obs <- data.frame(data$year,data$code_insee_matlab)
names(tableau_obs)<-c("year","code_insee_matlab")
for (t in 1994:2009) {
  assign(paste0("tab", t), subset(tableau_obs, tableau_obs$year == t))
}
rm(data,tableau_obs)

# Creation of WC for each year

for (t in 1994:2009) {
  print(t)
  dim <- as.numeric(max(get(paste0("tab", t))$code_insee_matlab))
  mat_W60 <- read.csv(paste0(main_dir,"/MAT/mat60/pre_mat_ABLG_60_", t, ".csv"))
  assign(paste0("W", t, "_60"), sparseMatrix(mat_W60$code_insee_matlab, mat_W60$code_insee2, x = 1, dims = c(dim, dim)))
  assign(paste0("W", t, "_60"), get(paste0("W", t, "_60"))[get(paste0("tab", t))$code_insee_matlab, get(paste0("tab", t))$code_insee_matlab])
}
rm(mat_W60)

# Creation of W for each year

for (t in 1994:2009) {
  print(t)
  dim <- as.numeric(max(get(paste0("tab", t))$code_insee_matlab))
  mat_W45 <- read.csv(paste0(main_dir,"/MAT/mat45/pre_mat_ABLG_45_", t, ".csv"))
  assign(paste0("W", t, "_45"), sparseMatrix(mat_W45$code_insee_matlab, mat_W45$code_insee2, x = 1, dims = c(dim, dim)))
  assign(paste0("W", t, "_45"), get(paste0("W", t, "_45"))[get(paste0("tab", t))$code_insee_matlab, get(paste0("tab", t))$code_insee_matlab])
}
rm(mat_W45)

# Creation of WE for each year

for (t in 1994:2009) {
  print(t)
  dim <- as.numeric(max(get(paste0("tab", t))$code_insee_matlab))
  pre_mat_WA <- read.csv(paste0(main_dir,"/MAT/mat45/pre_mat_WA_45_", t, ".csv"))
  assign(paste0("WA", t, "_45"), sparseMatrix(pre_mat_WA$code_insee_matlab, pre_mat_WA$code_insee2, x = pre_mat_WA$member, dims = c(dim, dim)))
  assign(paste0("WA", t, "_45"), get(paste0("WA", t, "_45"))[get(paste0("tab", t))$code_insee_matlab, get(paste0("tab", t))$code_insee_matlab])
}
rm(pre_mat_WA)

# Creation of WO for each year

for (t in 1994:2009) {
  print(t)
  assign(paste0("WB", t, "_45"), get(paste0("W", t, "_45")) - get(paste0("WA", t, "_45")))
}

# Creation of WC for each year

for (t in 1994:2009) {
  print(t)
  assign(paste0("WC", t, "_45"), get(paste0("W", t, "_60")) - get(paste0("W", t, "_45")))
}

# Creation of syndicate matrices from 1955 until 1974 (CB approach)

for (t in 1955:1970) {
  print(t)
  mat_syndic <- read.csv(paste0(main_dir,"/MAT/syndic/PRE_syndic_ABLG_", t, "_andbefore.csv"))
  dim <- as.numeric(max(get(paste0("tab", t + 39))$code_insee_matlab))
  syndic <- sparseMatrix(mat_syndic$code_insee_matlab, mat_syndic$code_insee2, x = 1, dims = c(dim, dim))
  syndic <- syndic[get(paste0("tab", t + 39))$code_insee_matlab, get(paste0("tab", t + 39))$code_insee_matlab]
  assign(paste0("syndic_", t), syndic)
}
rm(syndic,mat_syndic)

# Creation of the databases to create WEhat and WOhat  with syndicate membership

for (t in 1994:2009) {
  print(t)
  PRE_mat_EPCI <- read.csv(paste0(main_dir,"/MAT/mat45/pre_mat_ABLG_45_", t, ".csv"))
  PRE_syndic <- read.csv(paste0(main_dir,"/MAT/syndic/PRE_syndic_ABLG_", t - 39, "_andbefore.csv"))
  PRE_merged <- merge(PRE_mat_EPCI, PRE_syndic, by = c("code_insee_matlab", "code_insee2"), all.x = TRUE)
  PRE_merged$member[is.na(PRE_merged$member)] <- 0
  assign(paste0("PRE_", t, "_bis2"), PRE_merged)
}
rm(PRE_mat_EPCI,PRE_merged,PRE_syndic)

# Creation of WEhat and WOhat matrices with stock syndicates starting from 1955

for (t in 1994:2009) {
  print(t)
  # Load the data for the current year
  data_pred <- get(paste0("PRE_", t, "_bis2"))
  # Fit the linear model
  lm_name <- paste0("lm", t, "_s55")
  assign(lm_name, lm(w ~ member, data = data_pred))
  # Generate a table with the first stage regression results
  assign(paste0("FS",t), summary(get(paste0("lm", t, "_s55"))))
  sink(paste0("FS",t,".txt"))
  print(summary(get(paste0("lm", t, "_s55"))))
  sink()
  # Create the predicted data frame
  predicted_name <- paste0("predictedsyndic55_", t)
  predicted <- data.frame(data_pred$code_insee_matlab, data_pred$code_insee2, data_pred$w, predict(get(lm_name)))
  names(predicted) <- c("code_insee_matlab", "code_insee_matlab2", "w", paste0("what_syndic", t - 1939))
  # Create WAhat
  tab_name <- paste0("tab", t)
  dim <- as.numeric(max(get(tab_name)$code_insee_matlab))
  matrix_name <- paste0("WAhat", t, "_45")
  assign(matrix_name, sparseMatrix(predicted$code_insee_matlab, predicted$code_insee_matlab2, x = predicted[[paste0("what_syndic", t - 1939)]], dims = c(dim, dim)))
  assign(matrix_name, get(matrix_name)[get(tab_name)$code_insee_matlab, get(tab_name)$code_insee_matlab])
  # WBhat
  diff_name <- paste0("WBhat", t, "_45")
  assign(diff_name, get(paste0("W", t, "_45")) - get(matrix_name))
  # Clean up
  rm(data_pred, lm_name, predicted_name, tab_name, matrix_name, diff_name, predicted)
}

# Creation of the databases to create WEhat and WOhat  with flows

PRE_flows <- read.csv(paste0(main_dir,"/MAT/flows/flows.csv"))
PRE_flows$code_insee2 <- PRE_flows$code_insee_matlab2
PRE_flows$code_insee_matlab2 <- NULL
for (t in 1994:2009) {
  print(t)
  PRE_mat_EPCI <- read.csv(paste0(main_dir,"/MAT/mat45/pre_mat_ABLG_45_", t, ".csv"))
  PRE_merged <- merge(PRE_mat_EPCI, PRE_flows, by = c("code_insee_matlab", "code_insee2"), all.x = TRUE)
  PRE_merged$flows[is.na(PRE_merged$flows)] <- 0
  assign(paste0("PRE_", t, "_flows"), PRE_merged)
}
rm(PRE_mat_EPCI,PRE_merged,PRE_flows)

# Creation of WEhat and WOhat matrices with flows

for (t in 1994:2009) {
  print(t)
  # Load the data for the current year
  data_pred <- get(paste0("PRE_", t, "_flows"))
  # Fit the linear model
  lm_name <- paste0("lm", t, "_flows")
  assign(lm_name, lm(w ~ flows, data = data_pred))
  # Generate a table with the first stage regression results
  assign(paste0("FS",t), summary(get(paste0("lm", t, "_flows"))))
  sink(paste0("FS_flows",t,".txt"))
  print(summary(get(paste0("lm", t, "_flows"))))
  sink()
  # Create the predicted data frame
  predicted_name <- paste0("predictedflows_", t)
  predicted <- data.frame(data_pred$code_insee_matlab, data_pred$code_insee2, data_pred$w, predict(get(lm_name)))
  names(predicted) <- c("code_insee_matlab", "code_insee_matlab2", "w", paste0("what_flows",t))
  # Create WAhat
  tab_name <- paste0("tab", t)
  dim <- as.numeric(max(get(tab_name)$code_insee_matlab))
  matrix_name <- paste0("WAhat", t, "_flows")
  assign(matrix_name, sparseMatrix(predicted$code_insee_matlab, predicted$code_insee_matlab2, x = predicted[[paste0("what_flows", t)]], dims = c(dim, dim)))
  assign(matrix_name, get(matrix_name)[get(tab_name)$code_insee_matlab, get(tab_name)$code_insee_matlab])
  # WBhat
  diff_name <- paste0("WBhat", t, "_flows")
  assign(diff_name, get(paste0("W", t, "_45")) - get(matrix_name))
  # Clean up
  rm(data_pred, lm_name, predicted_name, tab_name, matrix_name, diff_name, predicted)
}

#################
# Saving matrices
#################

# Third ring

WC <- bdiag(WC1994_45, WC1995_45, WC1996_45, WC1997_45, WC1998_45, WC1999_45, WC2000_45, WC2001_45, WC2002_45, WC2003_45, WC2004_45, WC2005_45, WC2006_45, WC2007_45, WC2008_45, WC2009_45)
save(WC, file="/MAT/mat45/WC.Rda")

# For both approaches

WA_45 <- bdiag(WA1994_45,WA1995_45,WA1996_45,WA1997_45,WA1998_45,WA1999_45,WA2000_45,WA2001_45,WA2002_45,WA2003_45,WA2004_45,WA2005_45,WA2006_45,WA2007_45,WA2008_45,WA2009_45)
save(WA_45, file="/MAT/mat45/WA_45.Rda")
WB_45 <- bdiag(WB1994_45,WB1995_45,WB1996_45,WB1997_45,WB1998_45,WB1999_45,WB2000_45,WB2001_45,WB2002_45,WB2003_45,WB2004_45,WB2005_45,WB2006_45,WB2007_45,WB2008_45,WB2009_45)
save(WB_45, file="/MAT/mat45/WB_45.Rda")
W_45 <- bdiag(W1994_45,W1995_45,W1996_45,W1997_45,W1998_45,W1999_45,W2000_45,W2001_45,W2002_45,W2003_45,W2004_45,W2005_45,W2006_45,W2007_45,W2008_45,W2009_45)
save(W_45, file="/MAT/mat45/W_45.Rda")

# For the KP approach with syndicate membership

WAhat_syndic1955_45 <- bdiag(WAhat1994_45,WAhat1995_45,WAhat1996_45,WAhat1997_45,WAhat1998_45,WAhat1999_45,WAhat2000_45,WAhat2001_45,WAhat2002_45,WAhat2003_45,WAhat2004_45,WAhat2005_45,WAhat2006_45,WAhat2007_45,WAhat2008_45,WAhat2009_45)
save(WAhat_syndic1955_45, file="/MAT/mat45/WAhat_s_stock_55_45.Rda")
WBhat_syndic1955_45 <- bdiag(WBhat1994_45,WBhat1995_45,WBhat1996_45,WBhat1997_45,WBhat1998_45,WBhat1999_45,WBhat2000_45,WBhat2001_45,WBhat2002_45,WBhat2003_45,WBhat2004_45,WBhat2005_45,WBhat2006_45,WBhat2007_45,WBhat2008_45,WBhat2009_45)
save(WBhat_syndic1955_45, file="/MAT/mat45/WBhat_s_stock_55_45.Rda")

# For the KP approach with flow membership

WAhat_flows <- bdiag(WAhat1994_flows,WAhat1995_flows,WAhat1996_flows,WAhat1997_flows,WAhat1998_flows,WAhat1999_flows,WAhat2000_flows,WAhat2001_flows,WAhat2002_flows,WAhat2003_flows,WAhat2004_flows,WAhat2005_flows,WAhat2006_flows,WAhat2007_flows,WAhat2008_flows,WAhat2009_flows)
save(WAhat_flows, file="/MAT/flows/WAhat_s_flows.Rda")
WBhat_flows <- bdiag(WBhat1994_45,WBhat1995_45,WBhat1996_45,WBhat1997_45,WBhat1998_45,WBhat1999_45,WBhat2000_45,WBhat2001_45,WBhat2002_45,WBhat2003_45,WBhat2004_45,WBhat2005_45,WBhat2006_45,WBhat2007_45,WBhat2008_45,WBhat2009_45)

# For the CB approach

syndic_1955 <- bdiag(syndic_1955,syndic_1956,syndic_1957,syndic_1958,syndic_1959,syndic_1960,syndic_1961,syndic_1962,syndic_1963, syndic_1964,syndic_1965,syndic_1966,syndic_1967,syndic_1968,syndic_1969,syndic_1970)
save(syndic_1955, file="/MAT/mat45/stock_syndic_1955_45.Rda")
