
# ************************************************************************************ #
#      HOW AGRI-FOOD VALUE CHAIN EMPLOYMENT & MIXED INCOME EVOLVE WITH STRUCTURAL     #
#                                TRANSFORMATION                                       #
# ************************************************************************************ #
#                                   INSTRUCTIONS FOR USERS                            #
# ************************************************************************************ #
# This R project is designed for public use. To run the code:  
# 1. Ensure you have installed the required libraries.  
# 2. Download any accompanying datasets.  
#  
# Note: This code is almost identical to the other R script. The only difference  
# is that this version estimates **Net Mixed Income** ('LK02') instead of **Compensation of Employees ('LH01')**.  
#  
# For questions, feedback, or collaboration, please contact  
# Dr. Jing Yi at jing.yi@wisc.edu.  
# ************************************************************************************ #

rm(list = ls())
# dir <- getwd()
dir <- "D:\\Dropbox\\BoxOld\\FEDSshare\\MasterGithub\\CornellGitHub\\ASTAR_Labor\\LaborAFVC\\Code"
dir_result <- paste(dir, '/Output/MixedIncome', sep = '')
dir_data <- paste0(dir,'/Data/Export2')

requiredPackages = c('gee','PropCIs','RVAideMemoire','ggpubr', 'broom', 'modelsummary','dplyr','readxl','tidyverse','RODBC','writexl')
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) install.packages(p)
  library(p,character.only = TRUE)
}
options(modelsummary_format_numeric_latex = "plain")

# ----------------------------------------Import data:----------------------------------
# Define year by the Python script:
# year_i <- DefinedByPython
year_i <- DefinedByPython

file_pathForR  <- paste0(dir_data,'/','ForR',year_i,'.xlsx')
# file_pathForRNewT <- paste0(dir_data,'/','NewT',year_i,'.xlsx')

file_pathRCOL <-paste0(dir_data,'/','RCOL',year_i,'.xlsx')
# file_pathRemp <-paste0(dir_data,'/','Remp',year_i,'.xlsx')
# domestic demand:
file_pathInj <-paste0(dir_data,'/','ForR_inj',year_i,'.xlsx')
# exported demand:
file_pathInjExp <-paste0(dir_data,'/','ForR_inj_Exp',year_i,'.xlsx')
file_pathfaafh <-paste0(dir_data,'/','faafh',year_i,'.xlsx')
file_pathfaafhExp <-paste0(dir_data,'/','faafh_Exp',year_i,'.xlsx')
file_pathILOCtry <-paste0(dir_data,'/','ILO_Ctry',year_i,'.xlsx')

yfah <- read_excel(file_pathInj,sheet = 'fah')
yfahExp <- read_excel(file_pathInjExp,sheet = 'fah')
yfaafh <- read_excel(file_pathfaafh,sheet = 'faafh')
yfaafhExp <- read_excel(file_pathfaafhExp,sheet = 'faafh')
# ILOCtry <- read_excel(file_pathILOCtry)
# E <- read_excel(file_pathRemp,sheet = 'Remp')
# E <- as.matrix(E)

# T <- read_excel(file_pathForR,sheet = 'T')
T <- read_excel(file_pathForR,sheet = 'T')
T  <- as.matrix(T)
X <- read_excel(file_pathForR,sheet = 'X')
X <- as.matrix(X)
L <- read_excel(file_pathForR,sheet = 'L')
L <- as.matrix(L)

XCOL <- read_excel(file_pathRCOL,sheet = 'XCOL')
LROW <- read_excel(file_pathRCOL,sheet = 'LROW')
TCOL <- read_excel(file_pathRCOL,sheet = 'TCOL')
CtryList <- read_excel(file_pathRCOL,sheet='Ctry')
ListName <- read_excel(file_pathRCOL,sheet='ListName')


# /*GROSS OUTPUT*/
y <- T %*% as.matrix(rowSums(diag(ncol(T)))) +   X %*% as.matrix(rowSums(diag(ncol(X))))  
#insert 1 where 0 among y elements
yx0 <- y 
yx0 <- ifelse(yx0 == 0, 1, yx0)

# /*TOTAL FINAL DEMAND*/
x <- X %*% as.matrix(rowSums(diag(ncol(X)))) 
# /*DIRECT REQUIREMENT MATRIX*/
A <- T %*% solve(diag(c(yx0)))
# /*TOTAL REQUIREMENT MATRIX*/
M <- solve(diag(nrow(A))-A)

# /*GDI Multiplier*/
v <- as.matrix(L) %*% solve(diag(c(yx0)))
# /*VERIFY ACCOUNTING IDENTITIES*/
# ******************************************#
#Test balance: Calculate gross activity output linked to total final demand
Ytst <- round(y - M %*% rowSums(X),3)
colSums(Ytst)
# ******************************************#

condition <- grepl("_A01T02", TCOL$COL)
ag <- which(condition)
Xag <- which(!condition)
condition <- grepl("_A16", TCOL$COL)
trd_ws <- which(condition)
condition <- grepl("_A17", TCOL$COL)
trd_rt <- which(condition)
condition <- grepl("_A19", TCOL$COL)
trans <- which(condition)
condition <- grepl("_A18", TCOL$COL)
afs <- which(condition)
condition <- grepl("_A04", TCOL$COL)
fb <- which(condition)
condition <- grepl("A01T02|A04|A16|A17|A19|A18", TCOL$COL)
sc <- which(condition)
Nsc <- which(!condition)
# condition <- grepl('LH01', LROW$COL)
condition <- grepl('LK02', LROW$COL)
Labor <- which(condition)
# Labor compensation multipliers:
W <- solve(diag(c(y))) %*% t(L)
W_labor <- as.matrix(W[,Labor]) 
Wstar_dir <- W_labor
Wstar <- W_labor
Wstar_sub <- (solve(t(M[sc,sc])) %*%t(M[Nsc,sc]))%*%(W_labor[Nsc,])
Wstar_sub[Wstar_sub < 0] <- 0
Wstar[sc,] <- W_labor[sc,] +  Wstar_sub

# # Employment multipliers:
# EMP <- t(E %*% solve(diag(c(y))))
# EMPstar <- EMP
# subcontrct<-(solve(t(M[sc,sc])) %*%t(M[Nsc,sc]))%*%(EMP[Nsc,])
# subcontrct[subcontrct <0] <-0
# EMPstar[sc,] = EMP[sc,] + subcontrct
# EMPstar_dir <- EMP

# total VA:
Wstar_total <- as.matrix(rowSums(W))
Wstar_total_dir <- Wstar_total

Wstar_total[sc,] <- Wstar_total[sc,]+(solve(t(M[sc,sc])) %*%t(M[Nsc,sc]))%*%(Wstar_total[Nsc,])


#------------ create a loop to fix negative total va multiplier:---------

# Create a list to store matrices for each country
# country_matrices <- list()
# 
# # Specify the number of countries and the number of rows/columns per country
# num_countries <- nrow(CtryList)
# rows_per_country <- 8
# cols_per_country <- 8
# 
# i = 1
# start_row <- (i - 1) * rows_per_country + 1
# end_row <- i * rows_per_country
# start_col <- (i - 1) * cols_per_country + 1
# end_col <- i * cols_per_country
# country_rows <- start_row:end_row
# sc_i <- intersect(country_rows, unlist(sc))
# Nsc_i <- intersect(country_rows, unlist(Nsc))
# 
# # Loop through each country and create a subset matrix
# for (i in 1:num_countries) {
#   start_row <- (i - 1) * rows_per_country + 1
#   end_row <- i * rows_per_country
#   start_col <- (i - 1) * cols_per_country + 1
#   end_col <- i * cols_per_country
# #   
# #   # Subset the original matrix for the current country
#   M_i <- M[start_row:end_row, start_col:end_col]
#   W_labor_i <- W_labor[start_row:end_row, ]
#   country_rows <- start_row:end_row
# 
#   M_sc_sc <- M_i[sc_i,sc_i]
#   M_Nsc_sc <- M_i[Nsc_i, sc_i]
#   W_labor_Nsc_i <- W_labor_i[Nsc_i]
#   W_labor_sc_i <- as.matrix(W_labor_i[sc_i])
# 
#   ind <- (solve(t(M_sc_sc)) %*% t(M_Nsc_sc)) %*% as.matrix(W_labor_Nsc_i)
#   tot <- W_labor_sc_i + ind
# }
#------------ create a loop to fix negative total va multiplier (ends here)---------

y_fah_ctry <- as.matrix(colSums(yfah))
row_i = dim(y_fah_ctry)[1]
# y_fahDenom <- t(matrix(rep(y_fah_ctry, times = row_i, nrow = row_i, ncol = row_i)))
y_fahDenom <- matrix(rep(t(y_fah_ctry), each = nrow(y_fah_ctry)), nrow = nrow(y_fah_ctry))

y_fah_ctryExp <- as.matrix(colSums(yfahExp))
y_fahDenomExp <- matrix(rep(t(y_fah_ctryExp), each = nrow(y_fah_ctryExp)), nrow = nrow(y_fah_ctryExp))

yhome <- M[ag,] %*% as.matrix(yfah)
yaway <- M[ag,] %*% as.matrix(yfaafh)

yhomeExp <-  M[ag,] %*% as.matrix(yfahExp)
yawayExp <- M[ag,] %*% as.matrix(yfaafhExp)

Ystar <- M %*% as.matrix(yfah)
YstarExp <- M %*% as.matrix(yfahExp)
# Ystar_out <- cbind(TCOL,Ystar)

Ystar_faafh <- M %*% as.matrix(yfaafh)
Ystar_faafhExp <- M %*% as.matrix(yfaafhExp)

yhomeNet <- yhome - (A[ag,ag]+A[ag,Xag]%*% M[Xag,Xag] %*% A[Xag,ag]) %*% yhome
yhomeNetExp <- yhomeExp - (A[ag,ag]+A[ag,Xag]%*% M[Xag,Xag] %*% A[Xag,ag]) %*% yhomeExp

yawayNet <- yaway - (A[ag,ag]+A[ag,Xag]%*% M[Xag,Xag] %*% A[Xag,ag]) %*% yaway
yawayNetExp <- yawayExp - (A[ag,ag]+A[ag,Xag]%*% M[Xag,Xag] %*% A[Xag,ag]) %*% yawayExp

yhomeNetShr <-  yhomeNet/y_fahDenom
yhomeNetShr <- diag(yhomeNetShr)
# yhomeNetShr_out <-cbind(CtryList, as.data.frame(yhomeNetShr))
# file_path <-paste0(dir_result,'/',paste0('fah_shr3_',year_i,'.xlsx' ))
# write_xlsx(yhomeNetShr_out,file_path)

yhomeNetShrExp <-  yhomeNetExp/y_fahDenomExp
yhomeNetShrExp <- diag(yhomeNetShrExp)
yhomeNetShrExp_Out <-cbind(CtryList, as.data.frame(yhomeNetShrExp))
# file_path <-paste0(dir_result,'/',paste0('fahExp_shr3_',year_i,'.xlsx' ))
# write_xlsx(yhomeNetShrExp_Out,file_path)


faafh_dem <- as.matrix(colSums(yfaafh))
faafh_dem <- matrix(rep(t(faafh_dem), each = nrow(faafh_dem)), nrow = nrow(faafh_dem))

yawayNetShr <-  yawayNet /faafh_dem
yawayNetShr <- diag(yawayNetShr)
# yawayNetShr_out <-cbind(CtryList, yawayNetShr)
# file_path <-paste0(dir_result,'/',paste0('faafh_shr3_',year_i,'.xlsx'))
# write_xlsx(yawayNetShr_out,file_path)
 
# nSize <- ncol(W_labor)

# Labor compensation share (direct)
Wstar_dir_out <- cbind(as.matrix(Wstar_dir[ag,]), as.matrix(Wstar_dir[fb,]), as.matrix(Wstar_dir[trd_ws,]),
                       as.matrix(Wstar_dir[trd_rt,]),as.matrix(Wstar_dir[trans,]),as.matrix(Wstar_dir[afs,]))  
colnames(Wstar_dir_out) <- c("ag", "fb", "ws", "rt", "trans","res_hotel")
Wstar_dir_out <- as.data.frame(Wstar_dir_out)
Wstar_dir_out$COU <- CtryList$Ctry
COU_column <- Wstar_dir_out$COU
# Wstar_dir_out <- cbind(COU_column, Wstar_dir_out[, -which(names(Wstar_dir_out) == "COU")])   
# file_path <-paste0(dir_result,'/','Wstar_dir_out_',year_i,'.xlsx')
# write_xlsx(Wstar_dir_out,file_path)


#--------------------------------------FAH:----------------------
ESC_ag <- matrix(0,nrow=nrow(CtryList),ncol =  nrow(CtryList))
ESC_ag_dir <- ESC_ag
WSC_ag <- ESC_ag
WSC_ag_dir <- WSC_ag
# Total value added: 
WSC_total_ag <- WSC_total_fb <- WSC_total_ws <- WSC_total_rt <- WSC_total_trans <-WSC_total_afs <-  ESC_ag
WSC_total_agExp <- WSC_total_fbExp <- WSC_total_wsExp <- WSC_total_rtExp <- WSC_total_transExp <-WSC_total_afsExp <-  ESC_ag

WSC_totalDir_ag <- WSC_totalDir_fb <- WSC_totalDir_ws <- WSC_totalDir_rt <- WSC_totalDir_trans <-WSC_totalDir_afs <-  ESC_ag
WSC_totalDir_agExp <- WSC_totalDir_fbExp <- WSC_totalDir_wsExp <- WSC_totalDir_rtExp <- WSC_totalDir_transExp <-WSC_totalDir_afsExp <-  ESC_ag
# total labor:
W_labor_ag <-  W_labor_ws <-W_labor_rt <- W_labor_trans <- W_labor_afs <- W_labor_fb <- WSC_total_ag
W_labor_agExp <-  W_labor_wsExp <-W_labor_rtExp <- W_labor_transExp <- W_labor_afsExp <- W_labor_fbExp <- WSC_total_ag
# total labor VA (dir):
W_labor_dir_ag <- W_labor_dir_fb <- W_labor_dir_ws <- W_labor_dir_rt <- W_labor_dir_trans <- W_labor_dir_afs <-   WSC_total_ag
W_labor_dir_agExp <-W_labor_dir_fbExp <- W_labor_dir_wsExp <- W_labor_dir_rtExp <- W_labor_dir_transExp <- W_labor_dir_afsExp <- WSC_total_ag

ESC_ag <- ESC_fb <- ESC_ws <- ESC_rt <-ESC_trans <- ESC_afs<- WSC_total_ag
ESC_dir_ag <- ESC_dir_fb <- ESC_dir_ws <-ESC_dir_rt <- ESC_dir_trans<- ESC_dir_afs<- ESC_ag
ESC_agExp <- ESC_fbExp <- ESC_wsExp <- ESC_rtExp <-ESC_transExp <- ESC_afsExp<- WSC_total_ag
ESC_dir_agExp <- ESC_dir_fbExp <- ESC_dir_wsExp <-ESC_dir_rtExp <- ESC_dir_transExp<- ESC_dir_afsExp<- ESC_ag

for (col_i in 1:ncol(Ystar)){
  W_labor_ag[,col_i] <- as.matrix(Wstar[ag,]) * as.matrix(Ystar[ag,col_i])
  W_labor_fb[,col_i] <- as.matrix(Wstar[fb,]) * as.matrix(Ystar[fb,col_i])
  W_labor_ws[,col_i] <- as.matrix(Wstar[trd_ws,]) * as.matrix(Ystar[trd_ws,col_i])
  W_labor_rt[,col_i] <- as.matrix(Wstar[trd_rt,]) * as.matrix(Ystar[trd_rt,col_i])
  W_labor_trans[,col_i] <- as.matrix(Wstar[trans,]) * as.matrix(Ystar[trans,col_i])
  W_labor_afs[,col_i] <- as.matrix(Wstar[afs,]) * as.matrix(Ystar[afs,col_i])
  # export:
  W_labor_agExp[,col_i] <- as.matrix(Wstar[ag,]) * as.matrix(YstarExp[ag,col_i])
  W_labor_fbExp[,col_i] <- as.matrix(Wstar[fb,]) * as.matrix(YstarExp[fb,col_i])
  W_labor_wsExp[,col_i] <- as.matrix(Wstar[trd_ws,]) * as.matrix(YstarExp[trd_ws,col_i])
  W_labor_rtExp[,col_i] <- as.matrix(Wstar[trd_rt,]) * as.matrix(YstarExp[trd_rt,col_i])
  W_labor_transExp[,col_i] <- as.matrix(Wstar[trans,]) * as.matrix(YstarExp[trans,col_i])
  W_labor_afsExp[,col_i] <- as.matrix(Wstar[afs,]) * as.matrix(YstarExp[afs,col_i])
  
  W_labor_dir_ag[,col_i] <- as.matrix(Wstar_dir[ag,]) * as.matrix(Ystar[ag,col_i])
  W_labor_dir_fb[,col_i] <- as.matrix(Wstar_dir[fb,]) * as.matrix(Ystar[fb,col_i])
  W_labor_dir_ws[,col_i] <- as.matrix(Wstar_dir[trd_ws,]) * as.matrix(Ystar[trd_ws,col_i])
  W_labor_dir_rt[,col_i] <- as.matrix(Wstar_dir[trd_rt,]) * as.matrix(Ystar[trd_rt,col_i])
  W_labor_dir_trans[,col_i] <- as.matrix(Wstar_dir[trans,]) * as.matrix(Ystar[trans,col_i])
  W_labor_dir_afs[,col_i] <- as.matrix(Wstar_dir[afs,]) * as.matrix(Ystar[afs,col_i])
 
   # export: 
  W_labor_dir_agExp[,col_i] <- as.matrix(Wstar_dir[ag,]) * as.matrix(YstarExp[ag,col_i])
  W_labor_dir_fbExp[,col_i] <- as.matrix(Wstar_dir[fb,]) * as.matrix(YstarExp[fb,col_i])
  W_labor_dir_wsExp[,col_i] <- as.matrix(Wstar_dir[trd_ws,]) * as.matrix(YstarExp[trd_ws,col_i])
  W_labor_dir_rtExp[,col_i] <- as.matrix(Wstar_dir[trd_rt,]) * as.matrix(YstarExp[trd_rt,col_i])
  W_labor_dir_transExp[,col_i] <- as.matrix(Wstar_dir[trans,]) * as.matrix(YstarExp[trans,col_i])
  W_labor_dir_afsExp[,col_i] <- as.matrix(Wstar_dir[afs,]) * as.matrix(YstarExp[afs,col_i])
  
  # ESC_ag[,col_i] <- as.matrix(EMPstar[ag,]) * as.matrix(Ystar[ag,col_i])
  # ESC_fb[,col_i] <- as.matrix(EMPstar[fb,]) * as.matrix(Ystar[fb,col_i])
  # ESC_ws[,col_i] <- as.matrix(EMPstar[trd_ws,]) * as.matrix(Ystar[trd_ws,col_i])
  # ESC_rt[,col_i] <- as.matrix(EMPstar[trd_rt,]) * as.matrix(Ystar[trd_rt,col_i])
  # ESC_trans[,col_i] <- as.matrix(EMPstar[trans,]) * as.matrix(Ystar[trans,col_i])
  # ESC_afs[,col_i] <- as.matrix(EMPstar[afs,]) * as.matrix(Ystar[afs,col_i])
  # #Export:
  # ESC_agExp[,col_i] <- as.matrix(EMPstar[ag,]) * as.matrix(YstarExp[ag,col_i])
  # ESC_fbExp[,col_i] <- as.matrix(EMPstar[fb,]) * as.matrix(YstarExp[fb,col_i])
  # ESC_wsExp[,col_i] <- as.matrix(EMPstar[trd_ws,]) * as.matrix(YstarExp[trd_ws,col_i])
  # ESC_rtExp[,col_i] <- as.matrix(EMPstar[trd_rt,]) * as.matrix(YstarExp[trd_rt,col_i])
  # ESC_transExp[,col_i] <- as.matrix(EMPstar[trans,]) * as.matrix(YstarExp[trans,col_i])
  # ESC_afsExp[,col_i] <- as.matrix(EMPstar[afs,]) * as.matrix(YstarExp[afs,col_i])
  # 
  # ESC_dir_ag[,col_i] <- as.matrix(EMPstar_dir[ag,]) * as.matrix(Ystar[ag,col_i])
  # ESC_dir_fb[,col_i] <- as.matrix(EMPstar_dir[fb,]) * as.matrix(Ystar[fb,col_i])
  # ESC_dir_ws[,col_i] <- as.matrix(EMPstar_dir[trd_ws,]) * as.matrix(Ystar[trd_ws,col_i])
  # ESC_dir_rt[,col_i] <- as.matrix(EMPstar_dir[trd_rt,]) * as.matrix(Ystar[trd_rt,col_i])
  # ESC_dir_trans[,col_i] <- as.matrix(EMPstar_dir[trans,]) * as.matrix(Ystar[trans,col_i])
  # ESC_dir_afs[,col_i] <- as.matrix(EMPstar_dir[afs,]) * as.matrix(Ystar[afs,col_i])
  # # export:
  # ESC_dir_agExp[,col_i] <- as.matrix(EMPstar_dir[ag,]) * as.matrix(YstarExp[ag,col_i])
  # ESC_dir_fbExp[,col_i] <- as.matrix(EMPstar_dir[fb,]) * as.matrix(YstarExp[fb,col_i])
  # ESC_dir_wsExp[,col_i] <- as.matrix(EMPstar_dir[trd_ws,]) * as.matrix(YstarExp[trd_ws,col_i])
  # ESC_dir_rtExp[,col_i] <- as.matrix(EMPstar_dir[trd_rt,]) * as.matrix(YstarExp[trd_rt,col_i])
  # ESC_dir_transExp[,col_i] <- as.matrix(EMPstar_dir[trans,]) * as.matrix(YstarExp[trans,col_i])
  # ESC_dir_afsExp[,col_i] <- as.matrix(EMPstar_dir[afs,]) * as.matrix(YstarExp[afs,col_i])
  # add total VA by sc sector (dir+subcontractors):
  WSC_total_ag[,col_i] <- as.matrix(Wstar_total[ag,])* as.matrix(Ystar[ag,col_i])
  WSC_total_fb[,col_i] <- as.matrix(Wstar_total[fb,])* as.matrix(Ystar[fb,col_i])
  WSC_total_ws[,col_i] <- as.matrix(Wstar_total[trd_ws,])* as.matrix(Ystar[trd_ws,col_i])
  WSC_total_rt[,col_i] <- as.matrix(Wstar_total[trd_rt,])* as.matrix(Ystar[trd_rt,col_i])
  WSC_total_trans[,col_i] <- as.matrix(Wstar_total[trans,])* as.matrix(Ystar[trans,col_i])
  WSC_total_afs[,col_i] <- as.matrix(Wstar_total[afs,])* as.matrix(Ystar[afs,col_i])
  
  WSC_total_agExp[,col_i] <- as.matrix(Wstar_total[ag,])* as.matrix(YstarExp[ag,col_i])
  WSC_total_fbExp[,col_i] <- as.matrix(Wstar_total[fb,])* as.matrix(YstarExp[fb,col_i])
  WSC_total_wsExp[,col_i] <- as.matrix(Wstar_total[trd_ws,])* as.matrix(YstarExp[trd_ws,col_i])
  WSC_total_rtExp[,col_i] <- as.matrix(Wstar_total[trd_rt,])* as.matrix(YstarExp[trd_rt,col_i])
  WSC_total_transExp[,col_i] <- as.matrix(Wstar_total[trans,])* as.matrix(YstarExp[trans,col_i])
  WSC_total_afsExp[,col_i] <- as.matrix(Wstar_total[afs,])* as.matrix(YstarExp[afs,col_i])
  # add total VA by sc sector (dir):
  WSC_totalDir_ag[,col_i] <- as.matrix(Wstar_total_dir[ag,])* as.matrix(Ystar[ag,col_i])
  WSC_totalDir_fb[,col_i] <- as.matrix(Wstar_total_dir[fb,])* as.matrix(Ystar[fb,col_i])
  WSC_totalDir_ws[,col_i] <- as.matrix(Wstar_total_dir[trd_ws,])* as.matrix(Ystar[trd_ws,col_i])
  WSC_totalDir_rt[,col_i] <- as.matrix(Wstar_total_dir[trd_rt,])* as.matrix(Ystar[trd_rt,col_i])
  WSC_totalDir_trans[,col_i] <- as.matrix(Wstar_total_dir[trans,])* as.matrix(Ystar[trans,col_i])
  WSC_totalDir_afs[,col_i] <- as.matrix(Wstar_total_dir[afs,])* as.matrix(Ystar[afs,col_i])
  
  WSC_totalDir_agExp[,col_i] <- as.matrix(Wstar_total_dir[ag,])* as.matrix(YstarExp[ag,col_i])
  WSC_totalDir_fbExp[,col_i] <- as.matrix(Wstar_total_dir[fb,])* as.matrix(YstarExp[fb,col_i])
  WSC_totalDir_wsExp[,col_i] <- as.matrix(Wstar_total_dir[trd_ws,])* as.matrix(YstarExp[trd_ws,col_i])
  WSC_totalDir_rtExp[,col_i] <- as.matrix(Wstar_total_dir[trd_rt,])* as.matrix(YstarExp[trd_rt,col_i])
  WSC_totalDir_transExp[,col_i] <- as.matrix(Wstar_total_dir[trans,])* as.matrix(YstarExp[trans,col_i])
  WSC_totalDir_afsExp[,col_i] <- as.matrix(Wstar_total_dir[afs,])* as.matrix(YstarExp[afs,col_i])
}
 
#----------------- Export results:--------------------------
Exportfn <- function(base_var_name, variable_names,suffix, dir_result, year_i, ExcelName ) {
  # matrices <-  matrix(nrow = 0, ncol = 0, data = numeric())
  matrices <- list()
  if (substr(base_var_name, 1,3)=='ESC') {
    for (var_name in variable_names) {
      matrix_name <- paste0(base_var_name, "_", var_name,suffix)
      matrices[[matrix_name]] <- as.matrix(diag(get(matrix_name))) *1000
    }
  }   else {
  for (var_name in variable_names) {
    matrix_name <- paste0(base_var_name, "_", var_name,suffix)
    matrices[[matrix_name]] <- as.matrix(diag(get(matrix_name)))
  }
  }
  VA_tot <- do.call(cbind, matrices)
  colnames(VA_tot) <- variable_names
  VA_tot <- as.data.frame(VA_tot)
  VA_tot$COU <- CtryList$Ctry
  COU_column <- VA_tot$COU
  VA_tot <- cbind(COU_column, VA_tot[, -which(names(VA_tot) == "COU")])   
  if (suffix ==''){
    suffix <- 'Dom'
  }
  if (substr(base_var_name,1,3) =='ESC') {
    VA_tot <- VA_tot[VA_tot$COU_column  %in% ILOCtry$Cty,]
  }
  file_path <- paste0(dir_result, '/', ExcelName,suffix, year_i, '.xlsx')
  write_xlsx(VA_tot, file_path)
}


# total compensation: 
variable_names <- c("ag", "fb", "ws", "rt", "trans", "afs")
Exportfn("W_labor", variable_names,'', dir_result, year_i,'VA_labor_tot_fah_')
Exportfn("W_labor", variable_names,'Exp', dir_result, year_i,'VA_labor_tot_fah_')

variable_names <- c("ag", "fb", "ws", "rt", "trans", "afs")
Exportfn("W_labor_dir", variable_names,'', dir_result, year_i,'VA_labor_dir_fah_')
Exportfn("W_labor_dir", variable_names,'Exp', dir_result, year_i,'VA_labor_dir_fah_')
 
# variable_names <- c("ag", "fb", "ws", "rt", "trans", "afs")
# Exportfn("ESC", variable_names,'', dir_result, year_i,'emp_fah_')
# Exportfn("ESC", variable_names,'Exp', dir_result, year_i,'emp_fah_')
# 
# variable_names <- c("ag", "fb", "ws", "rt", "trans", "afs")
# Exportfn("ESC_dir", variable_names,'', dir_result, year_i,'emp_dir_fah_')
# Exportfn("ESC_dir", variable_names,'Exp', dir_result, year_i,'emp_dir_fah_')

variable_names <- c("ag", "fb", "ws", "rt", "trans", "afs")
Exportfn("WSC_total", variable_names,'', dir_result, year_i,'Tot_VA_fah')
Exportfn("WSC_total", variable_names,'Exp', dir_result, year_i,'Tot_VA_fah')

 
variable_names <- c("ag", "fb", "ws", "rt", "trans", "afs")
Exportfn("WSC_totalDir", variable_names,'', dir_result, year_i,'Tot_VA_dir_fah')
Exportfn("WSC_totalDir", variable_names,'Exp', dir_result, year_i,'Tot_VA_dir_fah')

#-------------------- faafh total value added:#--------------------
ESC_ag <- matrix(0,nrow=nrow(CtryList),ncol =  nrow(CtryList))
ESC_ag_dir <- ESC_ag
WSC_ag <- ESC_ag
WSC_ag_dir <- WSC_ag

WSC_total_ag <- WSC_total_fb <- WSC_total_trd_ws <- WSC_total_trd_rt <- WSC_total_trans <-WSC_total_afs <-  ESC_ag
WSC_total_agExp <- WSC_total_fbExp <- WSC_total_wsExp <- WSC_total_rtExp <- WSC_total_transExp <-WSC_total_afsExp <-  ESC_ag

WSC_totalDir_ag <- WSC_totalDir_fb <- WSC_totalDir_ws <- WSC_totalDir_rt <- WSC_totalDir_trans <-WSC_totalDir_afs <-  ESC_ag
WSC_totalDir_agExp <- WSC_totalDir_fbExp <- WSC_totalDir_wsExp <- WSC_totalDir_rtExp <- WSC_totalDir_transExp <-WSC_totalDir_afsExp <-  ESC_ag

# total:
W_labor_ag <-  W_labor_ws <-W_labor_rt <- W_labor_trans <- W_labor_afs <- W_labor_fb <- WSC_total_ag
W_labor_agExp <-  W_labor_wsExp <-W_labor_rtExp <- W_labor_transExp <- W_labor_afsExp <- W_labor_fbExp <- WSC_total_ag

W_labor_dir_ag <- W_labor_dir_fb <- W_labor_dir_ws <- W_labor_dir_rt <- W_labor_dir_trans <- W_labor_dir_afs <-   W_labor_ag
W_labor_dir_agExp <-W_labor_dir_fbExp <- W_labor_dir_wsExp <- W_labor_dir_rtExp <- W_labor_dir_transExp <- W_labor_dir_afsExp <- W_labor_ag

ESC_ag <- ESC_fb <- ESC_ws <- ESC_rt <-ESC_trans <- ESC_afs<- WSC_total_ag
ESC_dir_ag <- ESC_dir_fb <- ESC_dir_ws <-ESC_dir_rt <- ESC_dir_trans<- ESC_dir_afs<- ESC_ag
ESC_agExp <- ESC_fbExp <- ESC_wsExp <- ESC_rtExp <-ESC_transExp <- ESC_afsExp<- WSC_total_ag
ESC_dir_agExp <- ESC_dir_fbExp <- ESC_dir_wsExp <-ESC_dir_rtExp <- ESC_dir_transExp<- ESC_dir_afsExp<- ESC_ag

 

for (col_i in 1:ncol(Ystar)){
  # ESC1[,col_i] <- as.matrix(EMPstar[ag,]) * as.matrix(Ystar_faafh[ag,col_i])
  # ESC1_dir[,col_i] <- as.matrix(EMPstar_dir[,ag])  as.matrix(Ystar_faafh[ag,col_i])
  # line 699 in the SAS code that Pat shared
  W_labor_ag[,col_i] <- as.matrix(Wstar[ag,]) * as.matrix(Ystar_faafh[ag,col_i])
  W_labor_fb[,col_i] <- as.matrix(Wstar[fb,]) * as.matrix(Ystar_faafh[fb,col_i])
  W_labor_ws[,col_i] <- as.matrix(Wstar[trd_ws,]) * as.matrix(Ystar_faafh[trd_ws,col_i])
  W_labor_rt[,col_i] <- as.matrix(Wstar[trd_rt,]) * as.matrix(Ystar_faafh[trd_rt,col_i])
  W_labor_trans[,col_i] <- as.matrix(Wstar[trans,]) * as.matrix(Ystar_faafh[trans,col_i])
  W_labor_afs[,col_i] <- as.matrix(Wstar[afs,]) * as.matrix(Ystar_faafh[afs,col_i])
  # export:
  W_labor_agExp[,col_i] <- as.matrix(Wstar[ag,]) * as.matrix(Ystar_faafhExp[ag,col_i])
  W_labor_fbExp[,col_i] <- as.matrix(Wstar[fb,]) * as.matrix(Ystar_faafhExp[fb,col_i])
  W_labor_wsExp[,col_i] <- as.matrix(Wstar[trd_ws,]) * as.matrix(Ystar_faafhExp[trd_ws,col_i])
  W_labor_rtExp[,col_i] <- as.matrix(Wstar[trd_rt,]) * as.matrix(Ystar_faafhExp[trd_rt,col_i])
  W_labor_transExp[,col_i] <- as.matrix(Wstar[trans,]) * as.matrix(Ystar_faafhExp[trans,col_i])
  W_labor_afsExp[,col_i] <- as.matrix(Wstar[afs,]) * as.matrix(Ystar_faafhExp[afs,col_i])
  
  W_labor_dir_ag[,col_i] <- as.matrix(Wstar_dir[ag,]) * as.matrix(Ystar_faafh[ag,col_i])
  W_labor_dir_fb[,col_i] <- as.matrix(Wstar_dir[fb,]) * as.matrix(Ystar_faafh[fb,col_i])
  W_labor_dir_ws[,col_i] <- as.matrix(Wstar_dir[trd_ws,]) * as.matrix(Ystar_faafh[trd_ws,col_i])
  W_labor_dir_rt[,col_i] <- as.matrix(Wstar_dir[trd_rt,]) * as.matrix(Ystar_faafh[trd_rt,col_i])
  W_labor_dir_trans[,col_i] <- as.matrix(Wstar_dir[trans,]) * as.matrix(Ystar_faafh[trans,col_i])
  W_labor_dir_afs[,col_i] <- as.matrix(Wstar_dir[afs,]) * as.matrix(Ystar_faafh[afs,col_i])
  
  # export: 
  W_labor_dir_agExp[,col_i] <- as.matrix(Wstar_dir[ag,]) * as.matrix(Ystar_faafhExp[ag,col_i])
  W_labor_dir_fbExp[,col_i] <- as.matrix(Wstar_dir[fb,]) * as.matrix(Ystar_faafhExp[fb,col_i])
  W_labor_dir_wsExp[,col_i] <- as.matrix(Wstar_dir[trd_ws,]) * as.matrix(Ystar_faafhExp[trd_ws,col_i])
  W_labor_dir_rtExp[,col_i] <- as.matrix(Wstar_dir[trd_rt,]) * as.matrix(Ystar_faafhExp[trd_rt,col_i])
  W_labor_dir_transExp[,col_i] <- as.matrix(Wstar_dir[trans,]) * as.matrix(Ystar_faafhExp[trans,col_i])
  W_labor_dir_afsExp[,col_i] <- as.matrix(Wstar_dir[afs,]) * as.matrix(Ystar_faafhExp[afs,col_i])
  
  # ESC_ag[,col_i] <- as.matrix(EMPstar[ag,]) * as.matrix(Ystar_faafh[ag,col_i])
  # ESC_fb[,col_i] <- as.matrix(EMPstar[fb,]) * as.matrix(Ystar_faafh[fb,col_i])
  # ESC_ws[,col_i] <- as.matrix(EMPstar[trd_ws,]) * as.matrix(Ystar_faafh[trd_ws,col_i])
  # ESC_rt[,col_i] <- as.matrix(EMPstar[trd_rt,]) * as.matrix(Ystar_faafh[trd_rt,col_i])
  # ESC_trans[,col_i] <- as.matrix(EMPstar[trans,]) * as.matrix(Ystar_faafh[trans,col_i])
  # ESC_afs[,col_i] <- as.matrix(EMPstar[afs,]) * as.matrix(Ystar_faafh[afs,col_i])
  # #Export:
  # ESC_agExp[,col_i] <- as.matrix(EMPstar[ag,]) * as.matrix(Ystar_faafhExp[ag,col_i])
  # ESC_fbExp[,col_i] <- as.matrix(EMPstar[fb,]) * as.matrix(Ystar_faafhExp[fb,col_i])
  # ESC_wsExp[,col_i] <- as.matrix(EMPstar[trd_ws,]) * as.matrix(Ystar_faafhExp[trd_ws,col_i])
  # ESC_rtExp[,col_i] <- as.matrix(EMPstar[trd_rt,]) * as.matrix(Ystar_faafhExp[trd_rt,col_i])
  # ESC_transExp[,col_i] <- as.matrix(EMPstar[trans,]) * as.matrix(Ystar_faafhExp[trans,col_i])
  # ESC_afsExp[,col_i] <- as.matrix(EMPstar[afs,]) * as.matrix(Ystar_faafhExp[afs,col_i])
  # 
  # ESC_dir_ag[,col_i] <- as.matrix(EMPstar_dir[ag,]) * as.matrix(Ystar_faafh[ag,col_i])
  # ESC_dir_fb[,col_i] <- as.matrix(EMPstar_dir[fb,]) * as.matrix(Ystar_faafh[fb,col_i])
  # ESC_dir_ws[,col_i] <- as.matrix(EMPstar_dir[trd_ws,]) * as.matrix(Ystar_faafh[trd_ws,col_i])
  # ESC_dir_rt[,col_i] <- as.matrix(EMPstar_dir[trd_rt,]) * as.matrix(Ystar_faafh[trd_rt,col_i])
  # ESC_dir_trans[,col_i] <- as.matrix(EMPstar_dir[trans,]) * as.matrix(Ystar_faafh[trans,col_i])
  # ESC_dir_afs[,col_i] <- as.matrix(EMPstar_dir[afs,]) * as.matrix(Ystar_faafh[afs,col_i])
  # # export:
  # ESC_dir_agExp[,col_i] <- as.matrix(EMPstar_dir[ag,]) * as.matrix(Ystar_faafhExp[ag,col_i])
  # ESC_dir_fbExp[,col_i] <- as.matrix(EMPstar_dir[fb,]) * as.matrix(Ystar_faafhExp[fb,col_i])
  # ESC_dir_wsExp[,col_i] <- as.matrix(EMPstar_dir[trd_ws,]) * as.matrix(Ystar_faafhExp[trd_ws,col_i])
  # ESC_dir_rtExp[,col_i] <- as.matrix(EMPstar_dir[trd_rt,]) * as.matrix(Ystar_faafhExp[trd_rt,col_i])
  # ESC_dir_transExp[,col_i] <- as.matrix(EMPstar_dir[trans,]) * as.matrix(Ystar_faafhExp[trans,col_i])
  # ESC_dir_afsExp[,col_i] <- as.matrix(EMPstar_dir[afs,]) * as.matrix(Ystar_faafhExp[afs,col_i])
  
  # add total VA by sc sector (dir+subcontractors):
  WSC_total_ag[,col_i] <- as.matrix(Wstar_total[ag,])* as.matrix(Ystar_faafh[ag,col_i])
  WSC_total_fb[,col_i] <- as.matrix(Wstar_total[fb,])* as.matrix(Ystar_faafh[fb,col_i])
  WSC_total_ws[,col_i] <- as.matrix(Wstar_total[trd_ws,])* as.matrix(Ystar_faafh[trd_ws,col_i])
  WSC_total_rt[,col_i] <- as.matrix(Wstar_total[trd_rt,])* as.matrix(Ystar_faafh[trd_rt,col_i])
  WSC_total_trans[,col_i] <- as.matrix(Wstar_total[trans,])* as.matrix(Ystar_faafh[trans,col_i])
  WSC_total_afs[,col_i] <- as.matrix(Wstar_total[afs,])* as.matrix(Ystar_faafh[afs,col_i])
  
  WSC_total_agExp[,col_i] <- as.matrix(Wstar_total[ag,])* as.matrix(Ystar_faafhExp[ag,col_i])
  WSC_total_fbExp[,col_i] <- as.matrix(Wstar_total[fb,])* as.matrix(Ystar_faafhExp[fb,col_i])
  WSC_total_wsExp[,col_i] <- as.matrix(Wstar_total[trd_ws,])* as.matrix(Ystar_faafhExp[trd_ws,col_i])
  WSC_total_rtExp[,col_i] <- as.matrix(Wstar_total[trd_rt,])* as.matrix(Ystar_faafhExp[trd_rt,col_i])
  WSC_total_transExp[,col_i] <- as.matrix(Wstar_total[trans,])* as.matrix(Ystar_faafhExp[trans,col_i])
  WSC_total_afsExp[,col_i] <- as.matrix(Wstar_total[afs,])* as.matrix(Ystar_faafhExp[afs,col_i])
  # add total VA by sc sector (dir):
  WSC_totalDir_ag[,col_i] <- as.matrix(Wstar_total_dir[ag,])* as.matrix(Ystar_faafh[ag,col_i])
  WSC_totalDir_fb[,col_i] <- as.matrix(Wstar_total_dir[fb,])* as.matrix(Ystar_faafh[fb,col_i])
  WSC_totalDir_ws[,col_i] <- as.matrix(Wstar_total_dir[trd_ws,])* as.matrix(Ystar_faafh[trd_ws,col_i])
  WSC_totalDir_rt[,col_i] <- as.matrix(Wstar_total_dir[trd_rt,])* as.matrix(Ystar_faafh[trd_rt,col_i])
  WSC_totalDir_trans[,col_i] <- as.matrix(Wstar_total_dir[trans,])* as.matrix(Ystar_faafh[trans,col_i])
  WSC_totalDir_afs[,col_i] <- as.matrix(Wstar_total_dir[afs,])* as.matrix(Ystar_faafh[afs,col_i])
  
  WSC_totalDir_agExp[,col_i] <- as.matrix(Wstar_total_dir[ag,])* as.matrix(Ystar_faafhExp[ag,col_i])
  WSC_totalDir_fbExp[,col_i] <- as.matrix(Wstar_total_dir[fb,])* as.matrix(Ystar_faafhExp[fb,col_i])
  WSC_totalDir_wsExp[,col_i] <- as.matrix(Wstar_total_dir[trd_ws,])* as.matrix(Ystar_faafhExp[trd_ws,col_i])
  WSC_totalDir_rtExp[,col_i] <- as.matrix(Wstar_total_dir[trd_rt,])* as.matrix(Ystar_faafhExp[trd_rt,col_i])
  WSC_totalDir_transExp[,col_i] <- as.matrix(Wstar_total_dir[trans,])* as.matrix(Ystar_faafhExp[trans,col_i])
  WSC_totalDir_afsExp[,col_i] <- as.matrix(Wstar_total_dir[afs,])* as.matrix(Ystar_faafhExp[afs,col_i])
  
}
variable_names <- c("ag", "fb", "ws", "rt", "trans", "afs")
Exportfn("W_labor", variable_names,'', dir_result, year_i,'VA_labor_tot_faafh_')
Exportfn("W_labor", variable_names,'Exp', dir_result, year_i,'VA_labor_tot_faafh_')

variable_names <- c("ag", "fb", "ws", "rt", "trans", "afs")
Exportfn("W_labor_dir", variable_names,'', dir_result, year_i,'VA_labor_dir_faafh_')
Exportfn("W_labor_dir", variable_names,'Exp', dir_result, year_i,'VA_labor_dir_faafh_')

# variable_names <- c("ag", "fb", "ws", "rt", "trans", "afs")
# Exportfn("ESC", variable_names,'', dir_result, year_i,'emp_faafh_')
# Exportfn("ESC", variable_names,'Exp', dir_result, year_i,'emp_faafh_')
# 
# 
# variable_names <- c("ag", "fb", "ws", "rt", "trans", "afs")
# Exportfn("ESC_dir", variable_names,'', dir_result, year_i,'emp_dir_faafh_')
# Exportfn("ESC_dir", variable_names,'Exp', dir_result, year_i,'emp_dir_faafh_')

# total value added:
variable_names <- c("ag", "fb", "ws", "rt", "trans", "afs")
Exportfn("WSC_total", variable_names,'', dir_result, year_i,'Tot_VA_faafh')
Exportfn("WSC_total", variable_names,'Exp', dir_result, year_i,'Tot_VA_faafhh')


variable_names <- c("ag", "fb", "ws", "rt", "trans", "afs")
Exportfn("WSC_totalDir", variable_names,'', dir_result, year_i,'Tot_VA_dir_faafh')
Exportfn("WSC_totalDir", variable_names,'Exp', dir_result, year_i,'Tot_VA_dir_faafh')



##-------------------- faafh total value added (ends here)#--------------------


# file_path <-paste0(dir_result,'/','L_test.xlsx')
# L_out <- as.data.frame(L)
# write_xlsx(L_out,file_path)

# SalPerWorke_accom <- WSC6/ESC6
# 
# eora_out(ESC6,'emp_accom')
# eora_out(WSC6,'wage_accom')
# eora_out(SalPerWorke_accom,'sal_accom')
# 
# TCOL_ag <- rbind(TCOL[ag,], 'ZZZ')
# WSC_ag_out <- cbind(TCOL_ag,WSC_ag)
# colnames(WSC_ag_out) <- c('ROW',colnames(yfah))
# file_path <-paste0(dir_result,'/','TestWSC_ag_out.xlsx')
# write_xlsx(WSC_ag_out,file_path)
# column_names <- names(WSC_ag_out)[-1]
# domestic_sums <- numeric(length(column_names))
# export_sums <- numeric(length(column_names))
# WSC_ag_out$Category <- apply(WSC_ag_out, 1, function(row) {
#   ifelse(substr(row['ROW'], 1, 3) == substr(column_names, 1, 3), 'Domestic', 'Export')
# })

# for (i in 1:nrow(WSC_ag_out)) {
#   row_values <- as.numeric(WSC_ag_out[i, column_names])
#   domestic_sums[i] <- sum(row_values[WSC_ag_out$Category[i] == 'Domestic'], na.rm = TRUE)
#   export_sums[i] <- sum(row_values[WSC_ag_out$Category[i] == 'Export'], na.rm = TRUE)
# }
# WSC_ag_out$DomesticSum <- domestic_sums
# WSC_ag_out$ExportSum <- export_sums
 
# 
# WSC_total_ag_out <- cbind(TCOL[ag,],WSC_total_ag)
# colnames(WSC_total_ag_out) <- c('ROW',colnames(yfah))
# file_path <-paste0(dir_result,'/','TestWSC_total_ag_out.xlsx')
# write_xlsx(WSC_total_ag_out,file_path)

# EMPstar_dir_out<-cbind(TCOL, EMPstar_dir)

# ForR_inj_out <-cbind(TCOL, yfah)
# file_path <-paste0(dir_result,'/','TestInjec.xlsx')
# write_xlsx(ForR_inj_out,file_path)

# M_out <- cbind(TCOL,M)
# file_path <-paste0(dir_result,'/','TestM.xlsx')
# write_xlsx(M_out,file_path)
# 
# file_path <-paste0(dir_result,'/','test.csv')
# write.table(T_long,file_path,sep='|')

