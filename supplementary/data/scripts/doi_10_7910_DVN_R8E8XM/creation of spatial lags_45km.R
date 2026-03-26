### This program computes the spatial lags with a 45km cut-off. 
# The spatial lags with the other cut-offs are easily obtained by changing the csv inputs and changing _45 in the code with the appropriate number
# In the programs, adjust paths as needed
### This program also computes the spatial lags with a 45km and 60km cut-off for the third-order models.

rm(list=ls())   # Clear memory


# load necessary libraries (if they are not installed, type install.packages("spdep") and install.packages("foreign"), to be done once)

library(spdep)          # to create spatial lags
library(foreign)        # to export to Stata format 
library(splines)
library(here)

main_dir <- here()
main_dir <- sub("/code$", "", main_dir) # Chack that this provides the path to all replication files. Otherwise adjust by hand.

# prelim : load function that creates standardised weight matrix allowing for negative elements

mat2listw_2   <- function (x, row.names = NULL, style = "M") 
{
  if (!(is.matrix(x) || is(x, "sparseMatrix"))) 
    stop("x is not a matrix")
  n <- nrow(x)
  if (n < 1) 
    stop("non-positive number of entities")
  m <- ncol(x)
  if (n != m) 
    stop("x must be a square matrix")
  if (any(is.na(x))) 
    stop("NA values in x not allowed")
  if (!is.null(row.names)) {
    if (length(row.names) != n) 
      stop("row.names wrong length")
    if (length(unique(row.names)) != length(row.names)) 
      stop("non-unique row.names given")
  }
  if (is.null(row.names)) {
    if (!is.null(row.names(x))) {
      row.names <- row.names(x)
    }
    else {
      row.names <- as.character(1:n)
    }
  }
  if (is(x, "sparseMatrix")) {
    xC <- as(x, "dgCMatrix")
    i <- slot(xC, "i") + 1
    p <- slot(xC, "p")
    dp <- diff(p)
    rp <- rep(seq_along(dp), dp)
    df0 <- data.frame(from = i, to = rp, weights = slot(xC, 
                                                        "x"))
    o <- order(df0$from, df0$to)
    df <- df0[o, ]
    class(df) <- c(class(df), "spatial.neighbour")
    attr(df, "region.id") <- row.names
    attr(df, "n") <- dim(xC)[1]
    res <- sn2listw(df)
    neighbours <- res$neighbours
    weights <- res$weights
  }
  else {
    neighbours <- vector(mode = "list", length = n)
    weights <- vector(mode = "list", length = n)
    for (i in 1:n) {
      nbs <- which(x[i, ] > 0)
      if (length(nbs) > 0) {
        neighbours[[i]] <- nbs
        weights[[i]] <- as.double(x[i, nbs])
      }
      else {
        neighbours[[i]] <- 0L
      }
    }
  }
  attr(weights, "mode") <- "unknown"
  class(neighbours) <- "nb"
  attr(neighbours, "region.id") <- row.names
  attr(neighbours, "call") <- NA
  attr(neighbours, "sym") <- is.symmetric.nb(neighbours, verbose = FALSE, 
                                             force = TRUE)
  res <- list(style = style, neighbours = neighbours, weights = weights)
  class(res) <- c("listw", "nb")
  attr(res, "region.id") <- attr(neighbours, "region.id")
  attr(res, "call") <- match.call()
  if (style != "M") {
    res <- nb2listw(res$neighbours, glist = res$weights, 
                    style = style, zero.policy = TRUE)
  }
  res
}


#####################################################
# Load the data and creation of block 1 (fiscal data)
#####################################################

library(foreign)

# load the database in Stata format (block 1)

data <- read.dta(paste0(main_dir,"/finaldata/data_stata_baseline_45.dta"))

# Create splines for number of communes

summary(data$nb_com_epci)
bsnb_com_epci <- as.data.frame(bs(data$nb_com_epci))
names(bsnb_com_epci) <- c("nb_com_epci_spl1","nb_com_epci_spl2","nb_com_epci_spl3")
data <- cbind(data,bsnb_com_epci)
write.dta(data,file="data.dta")
rm(bsnb_com_epci)

# Replace NA with 0 at the EICM level

data$tx_TP_EPCIM[which(is.na(data$tx_TP_EPCIM))]=0
data$tx_FNB_EPCI[which(is.na(data$tx_FNB_EPCI))]=0
data$tx_FB_EPCI[which(is.na(data$tx_FB_EPCI))]=0
data$tx_th_EPCI[which(is.na(data$tx_th_EPCI))]=0

# Create the tax index at the EPCI level and at the total level

data$index_EICM <- 0.471459*data$tx_TP_EPCIM + 0.0308044*data$tx_FNB_EPCI + 0.2702719*data$tx_FB_EPCI + 0.2274647*data$tx_th_EPCI
data$index_total <- 0.471459*(data$tx_TP_EPCIM+ data$tx_th_com) + 0.0308044*(data$tx_FNB_EPCI + data$tx_FNB_com) + 0.2702719*(data$tx_FB_EPCI +data$tx_FB_com) + 0.2274647*(data$tx_th_EPCI + data$tx_th_com)
data$tx_TP_total <- data$tx_TP_EPCIM+data$tx_th_com
data$tx_FNB_total <- data$tx_FNB_EPCI+data$tx_FNB_com
data$tx_FB_total <- data$tx_FB_EPCI+data$tx_FB_com
data$tx_th_total <- data$tx_th_EPCI+data$tx_th_com
data$tx_CFE_total <- data$tx_CFE_EPCIM+data$tx_CFE_comM

# Create a dataframe with the 5 tax variables

y <- data.frame(data$year,data$code_insee_matlab,data$tx_th_com,data$tx_FB_com,data$tx_FNB_com,data$tx_TP_comM,data$index,data$tx_CFE_comM)
names(y)<-c("year","code_insee_matlab","tx_th_com","tx_FB_com","tx_FNB_com","tx_TP_comM","index","tx_CFE_comM")
y <- y[y$year < 2010,]

# Create a dataframe with the X variables to be spatially lagged for use as an instrument

keeps_x <- c("year","code_insee_matlab",
           "pop","densite","SUPERF","droite","gauche","debt","capital_exp","current_exp","revenu_imposable","potentiel_fiscal_3T","potentiel_fiscal_4T",
           "pc_pop_0014","pc_pop_60P","pc_pop_75P","TXCHOM","EMPLT","TXCADRE","TXMONO","TXCHOM3","EMPLT3","TXCADRE3","TXMONO3")            
X <- data[,keeps_x]                 
lengthX <- ncol(X)
rm(keeps_x)

# Create a restricted dataframe with x, logxs and splines

keeps_x <- c("year","code_insee_matlab","pop","densite","SUPERF","droite","gauche","pc_pop_0014","pc_pop_60P","pc_pop_75P",
           "TXCHOM3","EMPLT3","TXCADRE3","TXMONO3")           
X<-data[,keeps_x]                

X$logpop <- log(X$pop +1)
X$logdensite <- log(X$densite+1)
X$logEMPLT3 <- log(X$EMPLT3+1)
X$logSUPERF <- log(X$SUPERF+1)

library(splines)
bspop <- as.data.frame(bs(X$pop)) ; names(bspop) <- c("pop_spl1","pop_spl2","pop_spl3")
bsdensite <- as.data.frame(bs(X$densite)) ; names(bsdensite) <- c("dens_spl1","dens_spl2","dens_spl3")
bsEMPLT3 <- as.data.frame(bs(X$EMPLT3)) ; names(bsEMPLT3) <- c("EMPLT3_spl1","EMPLT3_spl2","EMPLT3_spl3")
bsSUPERF <- as.data.frame(bs(X$SUPERF)) ; names(bsSUPERF) <- c("SUPERF_spl1","SUPERF_spl2","SUPERF_spl3")

X <- cbind(X,bspop,bsdensite,bsEMPLT3,bsSUPERF)
lengthX <- ncol(X)
rm(keeps_x,bsdensite,bsEMPLT3,bspop,bsSUPERF)
X <- X[X$year < 2010,]

# Create x with the additional tax variables at the eicm level

X_eicm <- data.frame(data$year,data$code_insee_matlab,
                     data$tx_th_EPCI,data$tx_FB_EPCI,data$tx_FNB_EPCI,data$tx_TP_EPCIM,data$index_EICM,data$tx_CFE_EPCIM,
                     data$tx_th_total,data$tx_FB_total,data$tx_FNB_total,data$tx_TP_total,data$index_total,data$tx_CFE_total,
                     data$CFEZ, data$FA, data$TPU, data$MT)
names(X_eicm) <- c("year","code_insee_matlab","tx_th_EICM","tx_FB_EICM","tx_FNB_EICM","tx_TP_EICM","index_EICM","tx_CFE_EICM",
                 "tx_th_total","tx_FB_total","tx_FNB_total","tx_TP_total","index_total","tx_CFE_total",
                 "CFEZ","FA","TPU","MT")
X_eicm <- X_eicm[X_eicm$year < 2010,]                          # save in Stata format, write.dta needs the package foreign

write.dta(data,file="/MAT/data.dta")
write.dta(X,file="/MAT/X.dta")   # Write in Stata format
write.dta(X_eicm,file="/MAT/X_eicm.dta")   # Write in Stata format

rm(data)


##################################
# Functions to create spatial lags
##################################

# These functions must be in memory. Select all the lines for each function (SL1 and SL2) and run
# Note: in these lines, the functions nb2listw and lag.listw come from the spdep package

SL1 <- function (x,W) 
  # This function creates a first-order spatial lag : Wx with W row-standardized
  # x is a dataframe
  # W is a weights matrix in the special format sent by e-mail. Program will not work with other matrices. 
{
  index <- 1
  seqyear <- seq(1994,2009)
  ncolx <- ncol(x)                                   # Number of columbs in the dataframe x
  Wsx <- data.frame(matrix(ncol = ncolx, nrow = 0))  # the dataframe that will contain the spatial lags
  for(i in seqyear){                                 # Do for each year of the panel. Necessary since the weights matrices are constructed for each year
    print(i)
    x_yeari <- subset(x,x$year==i)                                        # Variables for year i
    length <- nrow(x_yeari)                                               # number of observations for year i
    W_yeari <- W[index:(index+length-1),index:(index+length-1)]           # W matrix information for year i
    W_yeari <- mat2listw(W_yeari, style="M")                            # Conversion of W in year i in listw format (spdep format) + row-standardization
    Wsx_yeari <- data.frame(matrix(ncol = ncolx, nrow = length))          # lags for year i (row-standardized)
    for (j in 1:(ncol(x)-2)) {                                            # Computation of the lags removing the first 2 columns (year and code_insee_matlab)
      Wsx_yeari[,j+2]<-lag.listw(W_yeari,x_yeari[,j+2],NAOK=TRUE, zero.policy=TRUE)    # Computation allowing for NAs and lines of zeros
    }
    Wsx_yeari[,1]<-x_yeari$year
    Wsx_yeari[,2]<-x_yeari$code_insee_matlab
    Wsx <- rbind(Wsx,Wsx_yeari)    # Update of Wsx
    index= index+length            # Update of index to grab the correct information in W in next loop
  } 
  res <- Wsx
  res
}


SL2 <- function (x,W1,W2) 
  # This function creates a second-order spatial lag : W1W2x with W1 and W2 row-standardized
  # x is a dataframe
  # W1 is the first weights matrix in the special format sent by e-mail. Program will not work with other matrices.
  # W2 is the second weights matrix in the special format sent by e-mail. Program will not work with other matrices.
{
  index <- 1
  seqyear <- seq(1994,2009)
  ncolx <- ncol(x)
  Wsx <- data.frame(matrix(ncol = ncolx, nrow = 0))  # the dataframe that will contain the spatial lags
  for(i in seqyear){
    print(i)
    x_yeari <- subset(x,x$year==i)                                          # Variables for year i
    length <- nrow(x_yeari)                                                 # number of observations for year i
    W1_yeari <- W1[index:(index+length-1),index:(index+length-1)]           # W1 matrix information for year i
    W2_yeari <- W2[index:(index+length-1),index:(index+length-1)]           # W2 matrix information for year i
    W1_yeari <- mat2listw_2(W1_yeari, style="W")   # Conversion of W1 in year i in listw format (spdep format) + row-standardization
    W2_yeari <- mat2listw_2(W2_yeari, style="W")   # Conversion of W2 in year i in listw format (spdep format) + row-standardization
    Wsx_yeari <- data.frame(matrix(ncol = ncolx, nrow = length))             # lags for year i (row-standardized)
    for (j in 1:(ncol(x)-2)) {                                               # Computation of the lags removing the first 2 columns (year and code_insee_matlab)
      Wsx_yeari[,j+2]<-lag.listw(W1_yeari,lag.listw(W1_yeari,x_yeari[,j+2],NAOK=TRUE, zero.policy=TRUE),NAOK=TRUE, zero.policy=TRUE)    # Computation allowing for NAs and lines of zeros
    }
    Wsx_yeari[,1]<-x_yeari$year
    Wsx_yeari[,2]<-x_yeari$code_insee_matlab
    Wsx <- rbind(Wsx,Wsx_yeari)    # Update of Wsx
    index= index+length            # Update of index to grab the correct information in W in next loop
  } 
  res <- Wsx
  res
}


SL3 <- function (x,W1,W2,W3) 
  # This function creates a third-order spatial lag : W1W2x with W1 and W2 row-standardized
  # x is a dataframe
  # W1 is the first weights matrix in the special format sent by e-mail. Program will not work with other matrices.
  # W2 is the second weights matrix in the special format sent by e-mail. Program will not work with other matrices.
  # W3 is the second weights matrix in the special format sent by e-mail. Program will not work with other matrices.
{
  index <- 1
  seqyear <- seq(1994,2009)
  ncolx <- ncol(x)
  Wsx <- data.frame(matrix(ncol = ncolx, nrow = 0))  # the dataframe that will contain the spatial lags
  for(i in seqyear){
    print(i)
    x_yeari <- subset(x,x$year==i)                                          # Variables for year i
    length <- nrow(x_yeari)                                                 # number of observations for year i
    W1_yeari <- W1[index:(index+length-1),index:(index+length-1)]           # W1 matrix information for year i
    W2_yeari <- W2[index:(index+length-1),index:(index+length-1)]           # W2 matrix information for year i
    W3_yeari <- W3[index:(index+length-1),index:(index+length-1)]           # W2 matrix information for year i
    W1_yeari <- mat2listw_2(W1_yeari, style="W")   # Conversion of W1 in year i in listw format (spdep format) + row-standardization
    W2_yeari <- mat2listw_2(W2_yeari, style="W")   # Conversion of W2 in year i in listw format (spdep format) + row-standardization
    W3_yeari <- mat2listw_2(W3_yeari, style="W")   # Conversion of W2 in year i in listw format (spdep format) + row-standardization
    Wsx_yeari <- data.frame(matrix(ncol = ncolx, nrow = length))             # lags for year i (row-standardized)
    for (j in 1:(ncol(x)-2)) {                                               # Computation of the lags removing the first 2 columns (year and code_insee_matlab)
      Wsx_yeari[,j+2]<-lag.listw(W1_yeari,lag.listw(W2_yeari,lag.listw(W3_yeari,x_yeari[,j+2],NAOK=TRUE, zero.policy=TRUE),NAOK=TRUE, zero.policy=TRUE),NAOK=TRUE, zero.policy=TRUE)    # Computation allowing for NAs and lines of zeros
    }
    Wsx_yeari[,1]<-x_yeari$year
    Wsx_yeari[,2]<-x_yeari$code_insee_matlab
    Wsx <- rbind(Wsx,Wsx_yeari)    # Update of Wsx
    index= index+length            # Update of index to grab the correct information in W in next loop
  } 
  res <- Wsx
  res
}


######################################################
# Creation of spatial lags for the explained variables
######################################################

# Creation of Wsy_45

load("/MAT/mat45/W_45.Rda")

Wsy_45 <- SL1(y,W_45)                                                                                           
names(Wsy_45)<-c("year","code_insee_matlab","Ws_tx_th_com","Ws_tx_FB_com","Ws_tx_FNB_com","Ws_tx_TP_comM","Ws_index","Ws_tx_CFE_comM")  
write.dta(Wsy_45,file="/MAT/mat45/Wsy_45.dta")                                                                            

# Creation of WEsy

load("/MAT/mat45/WA_45.Rda")

WAsy_45 <- SL1(y,WA_45)
names(WAsy_45)<-c("year","code_insee_matlab","WAs_tx_th_com","WAs_tx_FB_com","WAs_tx_FNB_com","WAs_tx_TP_comM","WAs_index","WAs_tx_CFE_comM")
write.dta(WAsy_45,file="/MAT/mat45/WAsy_45.dta")   # Write in Stata format

# Creation of WOsy

load("/MAT/mat45/WB_45.Rda")

WBsy_45 <- SL1(y,WB_45)
names(WBsy_45)<-c("year","code_insee_matlab","WBs_tx_th_com","WBs_tx_FB_com","WBs_tx_FNB_com","WBs_tx_TP_comM","WBs_index","WBs_tx_CFE_comM")
write.dta(WBsy_45,file="/MAT/mat45/WBsy_45.dta")   # Write in Stata format

# Creation of WCsy

load("/MAT/mat45/WC.Rda")

WCsy <- SL1(y,WC)
names(WCsy)<-c("year","code_insee_matlab",
               "WCs_tx_th_com","WCs_tx_FB_com","WCs_tx_FNB_com","WCs_tx_TP_comM","WCs_index","WCs_tx_CFE_comM")
write.dta(WCsy,file="/MAT/mat45/WCsy.dta")   # Write in Stata format



########################################################
# Creation of spatial lags for the explanatory variables
########################################################

# Creation of WsX_45

WsX_45 <- SL1(X,W_45)
names(WsX_45)<-c("year","code_insee_matlab",
              "Ws_pop","Ws_densite","Ws_SUPERF","Ws_droite","Ws_gauche",
              "Ws_pc_pop_0014","Ws_pc_pop_60P","Ws_pc_pop_75P",
              "Ws_TXCHOM3","Ws_EMPLT3","Ws_TXCADRE3","Ws_TXMONO3",
              "Ws_logpop","Ws_logdensite","Ws_logEMPLT3","Ws_logSUPERF",
              "Ws_pop_spl1","Ws_pop_spl2","Ws_pop_spl3",
              "Ws_dens_spl1","Ws_dens_spl2","Ws_dens_spl3",
              "Ws_EMPLT3_spl1","Ws_EMPLT3_spl2","Ws_EMPLT3_spl3",
              "Ws_SUPERF_spl1","Ws_SUPERF_spl2","Ws_SUPERF_spl3")
write.dta(WsX_45,file="/MAT/mat45/WsX_45.dta")   # Write in Stata format

# Creation of WEsX_45

WAsX_45 <- SL1(X,WA_45)
names(WAsX_45)<-c("year","code_insee_matlab",
               "WAs_pop","WAs_densite","WAs_SUPERF","WAs_droite","WAs_gauche",
               "WAs_pc_pop_0014","WAs_pc_pop_60P","WAs_pc_pop_75P",
               "WAs_TXCHOM3","WAs_EMPLT3","WAs_TXCADRE3","WAs_TXMONO3",
               "WAs_logpop","WAs_logdensite","WAs_logEMPLT3","WAs_logSUPERF",
               "WAs_pop_spl1","WAs_pop_spl2","WAs_pop_spl3",
               "WAs_dens_spl1","WAs_dens_spl2","WAs_dens_spl3",
               "WAs_EMPLT3_spl1","WAs_EMPLT3_spl2","WAs_EMPLT3_spl3",
               "WAs_SUPERF_spl1","WAs_SUPERF_spl2","WAs_SUPERF_spl3")
write.dta(WAsX_45,file="/MAT/mat45/WAsX_45.dta")   # Write in Stata format

# Creation of WOsX_45

WBsX_45 <- SL1(X,WB_45)
names(WBsX_45)<-c("year","code_insee_matlab",
               "WBs_pop","WBs_densite","WBs_SUPERF","WBs_droite","WBs_gauche",
               "WBs_pc_pop_0014","WBs_pc_pop_60P","WBs_pc_pop_75P",
               "WBs_TXCHOM3","WBs_EMPLT3","WBs_TXCADRE3","WBs_TXMONO3",
               "WBs_logpop","WBs_logdensite","WBs_logEMPLT3","WBs_logSUPERF",
               "WBs_pop_spl1","WBs_pop_spl2","WBs_pop_spl3",
               "WBs_dens_spl1","WBs_dens_spl2","WBs_dens_spl3",
               "WBs_EMPLT3_spl1","WBs_EMPLT3_spl2","WBs_EMPLT3_spl3",
               "WBs_SUPERF_spl1","WBs_SUPERF_spl2","WBs_SUPERF_spl3")
write.dta(WBsX_45,file="/MAT/mat45/WBsX_45.dta")   # Write in Stata format

# Creation of WCsX

WCsX <- SL1(X,WC)
names(WCsX)<-c("year","code_insee_matlab",
               "WCs_pop","WCs_densite","WCs_SUPERF","WCs_droite","WCs_gauche",
               "WCs_pc_pop_0014","WCs_pc_pop_60P","WCs_pc_pop_75P",
               "WCs_TXCHOM3","WCs_EMPLT3","WCs_TXCADRE3","WCs_TXMONO3",
               "WCs_logpop","WCs_logdensite","WCs_logEMPLT3","WCs_logSUPERF",
               "WCs_pop_spl1","WCs_pop_spl2","WCs_pop_spl3",
               "WCs_dens_spl1","WCs_dens_spl2","WCs_dens_spl3",
               "WCs_EMPLT3_spl1","WCs_EMPLT3_spl2","WCs_EMPLT3_spl3",
               "WCs_SUPERF_spl1","WCs_SUPERF_spl2","WCs_SUPERF_spl3")
write.dta(WCsX,file="/MAT/mat45/WCsX.dta")   # Write in Stata format

# Creation of WEWOsX_45

WAWBsX_45 <- SL2(X,WA_45,WB_45)
names(WAWBsX_45)<-c("year","code_insee_matlab",
                 "WAWBs_pop","WAWBs_densite","WAWBs_SUPERF","WAWBs_droite","WAWBs_gauche",
                 "WAWBs_pc_pop_0014","WAWBs_pc_pop_60P","WAWBs_pc_pop_75P",
                 "WAWBs_TXCHOM3","WAWBs_EMPLT3","WAWBs_TXCADRE3","WAWBs_TXMONO3",
                 "WAWBs_logpop","WAWBs_logdensite","WAWBs_logEMPLT3","WAWBs_logSUPERF",
                 "WAWBs_pop_spl1","WAWBs_pop_spl2","WAWBs_pop_spl3",
                 "WAWBs_dens_spl1","WAWBs_dens_spl2","WAWBs_dens_spl3",
                 "WAWBs_EMPLT3_spl1","WAWBs_EMPLT3_spl2","WAWBs_EMPLT3_spl3",
                 "WAWBs_SUPERF_spl1","WAWBs_SUPERF_spl2","WAWBs_SUPERF_spl3")
write.dta(WAWBsX_45,file="/MAT/mat45/WAWBsX_45.dta")   # Write in Stata format

# Creation of WOWEsX_45

WBWAsX_45 <- SL2(X,WB_45,WA_45)
names(WBWAsX_45)<-c("year","code_insee_matlab",
                 "WBWAs_pop","WBWAs_densite","WBWAs_SUPERF","WBWAs_droite","WBWAs_gauche",
                 "WBWAs_pc_pop_0014","WBWAs_pc_pop_60P","WBWAs_pc_pop_75P",
                 "WBWAs_TXCHOM3","WBWAs_EMPLT3","WBWAs_TXCADRE3","WBWAs_TXMONO3",
                 "WBWAs_logpop","WBWAs_logdensite","WBWAs_logEMPLT3","WBWAs_logSUPERF",
                 "WBWAs_pop_spl1","WBWAs_pop_spl2","WBWAs_pop_spl3",
                 "WBWAs_dens_spl1","WBWAs_dens_spl2","WBWAs_dens_spl3",
                 "WBWAs_EMPLT3_spl1","WBWAs_EMPLT3_spl2","WBWAs_EMPLT3_spl3",
                 "WBWAs_SUPERF_spl1","WBWAs_SUPERF_spl2","WBWAs_SUPERF_spl3")
write.dta(WBWAsX_45,file="/MAT/mat45/WBWAsX_45.dta")   # Write in Stata format

# Do the same with eicm variables_45

WsX_eicm_45 <- SL1(X_eicm,W_45)
names(WsX_eicm_45)<-c("year","code_insee_matlab",
                      "Ws_tx_th_EICM","Ws_tx_FB_EICM","Ws_tx_FNB_EICM","Ws_tx_TP_EICM","Ws_index_EICM","Ws_tx_CFE_EICM",
                      "Ws_tx_th_total","Ws_tx_FB_total","Ws_tx_FNB_total","Ws_tx_TP_total","Ws_index_total","Ws_tx_CFE_total")
write.dta(WsX_eicm_45,file="/MAT/mat45/WsX_eicm_45.dta")   # Write in Stata format
WAsX_eicm_45 <- SL1(X_eicm,WA_45)
names(WAsX_eicm_45)<-c("year","code_insee_matlab",
                       "WAs_tx_th_EICM","WAs_tx_FB_EICM","WAs_tx_FNB_EICM","WAs_tx_TP_EICM","WAs_index_EICM","WAs_tx_CFE_EICM",
                       "WAs_tx_th_total","WAs_tx_FB_total","WAs_tx_FNB_total","WAs_tx_TP_total","WAs_index_total","WAs_tx_CFE_total")
write.dta(WAsX_eicm_45,file="/MAT/mat45/WAsX_eicm_45.dta")   # Write in Stata format
WBsX_eicm_45 <- SL1(X_eicm,WB_45)
names(WBsX_eicm_45)<-c("year","code_insee_matlab",
                       "WBs_tx_th_EICM","WBs_tx_FB_EICM","WBs_tx_FNB_EICM","WBs_tx_TP_EICM","WBs_index_EICM","WBs_tx_CFE_EICM",
                       "WBs_tx_th_total","WBs_tx_FB_total","WBs_tx_FNB_total","WBs_tx_TP_total","WBs_index_total","WBs_tx_CFE_total")
write.dta(WBsX_eicm_45,file="/MAT/mat45/WBsX_eicm_45.dta")   # Write in Stata format
WCsX_eicm_45 <- SL1(X_eicm,WC)
names(WCsX_eicm_45)<-c("year","code_insee_matlab",
                       "WCs_tx_th_EICM","WCs_tx_FB_EICM","WCs_tx_FNB_EICM","WCs_tx_TP_EICM","WCs_index_EICM","WCs_tx_CFE_EICM",
                       "WCs_tx_th_total","WCs_tx_FB_total","WCs_tx_FNB_total","WCs_tx_TP_total","WCs_index_total","WCs_tx_CFE_total")
write.dta(WCsX_eicm_45,file="/MAT/mat45/WCsX_eicm_45.dta")   # Write in Stata format
WAWBsX_eicm_45 <- SL2(X_eicm,WA_45,WB_45)
names(WAWBsX_eicm_45)<-c("year","code_insee_matlab",
                         "WAWBs_tx_th_EICM","WAWBs_tx_FB_EICM","WAWBs_tx_FNB_EICM","WAWBs_tx_TP_EICM","WAWBs_index_EICM","WAWBs_tx_CFE_EICM",
                         "WAWBs_tx_th_total","WAWBs_tx_FB_total","WAWBs_tx_FNB_total","WAWBs_tx_TP_total","WAWBs_index_total","WAWBs_tx_CFE_total")
write.dta(WAWBsX_eicm_45,file="/MAT/mat45/WAWBsX_eicm_45.dta")   # Write in Stata format
WBWAsX_eicm_45 <- SL2(X_eicm,WB_45,WA_45)
names(WBWAsX_eicm_45)<-c("year","code_insee_matlab",
                         "WBWAs_tx_th_EICM","WBWAs_tx_FB_EICM","WBWAs_tx_FNB_EICM","WBWAs_tx_TP_EICM","WBWAs_index_EICM","WBWAs_tx_CFE_EICM",
                         "WBWAs_tx_th_total","WBWAs_tx_FB_total","WBWAs_tx_FNB_total","WBWAs_tx_TP_total","WBWAs_index_total","WBWAs_tx_CFE_total")
write.dta(WBWAsX_eicm_45,file="/MAT/mat45/WBWAsX_eicm_45.dta")   # Write in Stata format





######################################################################################
# Creation of spatial lags for the explanatory variables (instruments for CB approach)
######################################################################################

# Creation of Wssyndic1955X_45

load("/MAT/mat45/stock_syndic_1955_45.Rda")

Wssyndic1955_45X <- SL1(X,syndic_1955)
names(Wssyndic1955_45X)<-c("year","code_insee_matlab",
                        "Wssyndic55_45_pop","Wssyndic55_45_densite","Wssyndic55_45_SUPERF","Wssyndic55_45_droite","Wssyndic55_45_gauche",
                        "Wssyndic55_45_pc_pop_0014","Wssyndic55_45_pc_pop_60P","Wssyndic55_45_pc_pop_75P",
                        "Wssyndic55_45_TXCHOM3","Wssyndic55_45_EMPLT3","Wssyndic55_45_TXCADRE3","Wssyndic55_45_TXMONO3",
                        "Wssyndic55_45_logpop","Wssyndic55_45_logdensite","Wssyndic55_45_logEMPLT3","Wssyndic55_45_logSUPERF",
                        "Wssyndic55_45_pop_spl1","Wssyndic55_45_pop_spl2","Wssyndic55_45_pop_spl3",
                        "Wssyndic55_45_dens_spl1","Wssyndic55_45_dens_spl2","Wssyndic55_45_dens_spl3",
                        "Wssyndic55_45_EMPLT3_spl1","Wssyndic55_45_EMPLT3_spl2","Wssyndic55_45_EMPLT3_spl3",
                        "Wssyndic55_45_SUPERF_spl1","Wssyndic55_SUPERF_spl2","Wssyndic55_SUPERF_spl3")
write.dta(Wssyndic1955_45X,file="/MAT/mat45/Wssyndic1955_45X.dta")   # Write in Stata format

# Do  the same with eimc

Wssyndic1955_45X_eicm <- SL1(X_eicm,syndic_1955)
names(Wssyndic1955_45X_eicm)<-c("year","code_insee_matlab",
                      "Wssyndic55_45__tx_th_EICM","Wssyndic55_45__tx_FB_EICM","Wssyndic55_45__tx_FNB_EICM","Wssyndic55_45__tx_TP_EICM","Wssyndic55_45__index_EICM","Wssyndic55_45__tx_CFE_EICM",
                      "Wssyndic55_45__tx_th_total","Wssyndic55_45__tx_FB_total","Wssyndic55_45__tx_FNB_total","Wssyndic55_45__tx_TP_total","Wssyndic55_45__index_total","Wssyndic55_45__tx_CFE_total")
write.dta(Wssyndic1955_45X_eicm,file="/MAT/mat45/Wssyndic1955_45X_eicm.dta")   # Write in Stata format


######################################################################################
# Creation of spatial lags for the explanatory variables (instruments for KP approach)
######################################################################################

# Creation of WEhatXs_s55

load("/MAT/mat45/WAhat_s_stock_55_45.Rda")

WAhatXs_s55_45 <- SL1(X,WAhat_syndic1955_45)
names(WAhatXs_s55_45)<-c("year","code_insee_matlab",
                      "WAhats_s55_45_pop","WAhats_s55_45_densite","WAhats_s55_45_SUPERF","WAhats_s55_45_droite","WAhats_s55_45_gauche",
                      "WAhats_s55_45_pc_pop_0014","WAhats_s55_45_pc_pop_60P","WAhats_s55_45_pc_pop_75P",
                      "WAhats_s55_45_TXCHOM3","WAhats_s55_45_EMPLT3","WAhats_s55_45_TXCADRE3","WAhats_s55_45_TXMONO3",
                      "WAhats_s55_45_logpop","WAhats_s55_45_logdensite","WAhats_s55_45_logEMPLT3","WAhats_s55_45_logSUPERF",
                      "WAhats_s55_45_pop_spl1","WAhats_s55_45_pop_spl2","WAhats_s55_45_pop_spl3",
                      "WAhats_s55_45_dens_spl1","WAhats_s55_45_dens_spl2","WAhats_s55_45_dens_spl3",
                      "WAhats_s55_45_EMPLT3_spl1","WAhats_s55_45_EMPLT3_spl2","WAhats_s55_45_EMPLT3_spl3",
                      "WAhats_s55_45_SUPERF_spl1","WAhats_s55_45_SUPERF_spl2","WAhats_s55_45_SUPERF_spl3")
write.dta(WAhatXs_s55_45,file="/MAT/mat45/WAhatXs_stock55_45.dta")   # Write in Stata format

# Creation of WOhatXs_s55

load("/MAT/mat45/WBhat_s_stock_55_45.Rda")

WBhatXs_s55_45 <- SL1(X,WBhat_syndic1955_45)
names(WBhatXs_s55_45)<-c("year","code_insee_matlab",
                      "WBhats_s55_45_pop","WBhats_s55_45_densite","WBhats_s55_45_SUPERF","WBhats_s55_45_droite","WBhats_s55_45_gauche",
                      "WBhats_s55_45_pc_pop_0014","WBhats_s55_45_pc_pop_60P","WBhats_s55_45_pc_pop_75P",
                      "WBhats_s55_45_TXCHOM3","WBhats_s55_45_EMPLT3","WBhats_s55_45_TXCADRE3","WBhats_s55_45_TXMONO3",
                      "WBhats_s55_45_logpop","WBhats_s55_45_logdensite","WBhats_s55_45_logEMPLT3","WBhats_s55_45_logSUPERF",
                      "WBhats_s55_45_pop_spl1","WBhats_s55_45_pop_spl2","WBhats_s55_45_pop_spl3",
                      "WBhats_s55_45_dens_spl1","WBhats_s55_45_dens_spl2","WBhats_s55_45_dens_spl3",
                      "WBhats_s55_45_EMPLT3_spl1","WBhats_s55_45_EMPLT3_spl2","WBhats_s55_45_EMPLT3_spl3",
                      "WBhats_s55_45_SUPERF_spl1","WBhats_s55_45_SUPERF_spl2","WBhats_s55_45_SUPERF_spl3")
write.dta(WBhatXs_s55_45,file="/MAT/mat45/WBhatXs_stock55_45.dta")   # Write in Stata format

# Creation of WEhatWOhatXs_s55

WAhatWBhatXs_s55_45 <- SL2(X,WAhat_syndic1955_45,WBhat_syndic1955_45)
names(WAhatWBhatXs_s55_45)<-c("year","code_insee_matlab",
                           "WAhatWBhats_s55_45_pop","WAhatWBhats_s55_45_densite","WAhatWBhats_s55_45_SUPERF","WAhatWBhats_s55_45_droite","WAhatWBhats_s55_45_gauche",
                           "WAhatWBhats_s55_45_pc_pop_0014","WAhatWBhats_s55_45_pc_pop_60P","WAhatWBhats_s55_45_pc_pop_75P",
                           "WAhatWBhats_s55_45_TXCHOM3","WAhatWBhats_s55_45_EMPLT3","WAhatWBhats_s55_45_TXCADRE3","WAhatWBhats_s55_45_TXMONO3",
                           "WAhatWBhats_s55_45_logpop","WAhatWBhats_s55_45_logdensite","WAhatWBhats_s55_45_logEMPLT3","WAhatWBhats_s55_45_logSUPERF",
                           "WAhatWBhats_s55_45_pop_spl1","WAhatWBhats_s55_45_pop_spl2","WAhatWBhats_s55_45_pop_spl3",
                           "WAhatWBhats_s55_45_dens_spl1","WAhatWBhats_s55_45_dens_spl2","WAhatWBhats_s55_45_dens_spl3",
                           "WAhatWBhats_s55_45_EMPLT3_spl1","WAhatWBhats_s55_45_EMPLT3_spl2","WAhatWBhats_s55_45_EMPLT3_spl3",
                           "WAhatWBhats_s55_45_SUPERF_spl1","WAhatWBhats_s55_45_SUPERF_spl2","WAhatWBhats_s55_45_SUPERF_spl3")
write.dta(WAhatWBhatXs_s55_45,file="/MAT/mat45/WAhatWBhatXs_stock55_45.dta")   # Write in Stata format

# Creation of WOhatWEhat_s55

WBhatWAhatXs_s55_45 <- SL2(X,WBhat_syndic1955_45,WAhat_syndic1955_45)
names(WBhatWAhatXs_s55_45)<-c("year","code_insee_matlab",
                           "WBhatWAhats_s55_45_pop","WBhatWAhats_s55_45_densite","WBhatWAhats_s55_45_SUPERF","WBhatWAhats_s55_45_droite","WBhatWAhats_s55_45_gauche",
                           "WBhatWAhats_s55_45_pc_pop_0014","WBhatWAhats_s55_45_pc_pop_60P","WBhatWAhats_s55_45_pc_pop_75P",
                           "WBhatWAhats_s55_45_TXCHOM3","WBhatWAhats_s55_45_EMPLT3","WBhatWAhats_s55_45_TXCADRE3","WBhatWAhats_s55_45_TXMONO3",
                           "WBhatWAhats_s55_45_logpop","WBhatWAhats_s55_45_logdensite","WBhatWAhats_s55_45_logEMPLT3","WBhatWAhats_s55_45_logSUPERF",
                           "WBhatWAhats_s55_45_pop_spl1","WBhatWAhats_s55_45_pop_spl2","WBhatWAhats_s55_45_pop_spl3",
                           "WBhatWAhats_s55_45_dens_spl1","WBhatWAhats_s55_45_dens_spl2","WBhatWAhats_s55_45_dens_spl3",
                           "WBhatWAhats_s55_45_EMPLT3_spl1","WBhatWAhats_s55_45_EMPLT3_spl2","WBhatWAhats_s55_45_EMPLT3_spl3",
                           "WBhatWAhats_s55_45_SUPERF_spl1","WBhatWAhats_s55_45_SUPERF_spl2","WBhatWAhats_s55_45_SUPERF_spl3")
write.dta(WBhatWAhatXs_s55_45,file="/MAT/mat45/WBhatWAhatXs_stock55_45.dta")   # Write in Stata format

# Creation of WEhatWC_s55

WAhatWCXs_s55 <- SL2(X,WAhat_syndic1955_45,WC)
names(WAhatWCXs_s55)<-c("year","code_insee_matlab",
                        "WAhatWCXs_s55_pop","WAhatWCXs_s55_densite","WAhatWCXs_s55_SUPERF","WAhatWCXs_s55_droite","WAhatWCXs_s55_gauche",
                        "WAhatWCXs_s55_pc_pop_0014","WAhatWCXs_s55_pc_pop_60P","WAhatWCXs_s55_pc_pop_75P",
                        "WAhatWCXs_s55_TXCHOM3","WAhatWCXs_s55_EMPLT3","WAhatWCXs_s55_TXCADRE3","WAhatWCXs_s55_TXMONO3",
                        "WAhatWCXs_s55_logpop","WAhatWCXs_s55_logdensite","WAhatWCXs_s55_logEMPLT3","WAhatWCXs_s55_logSUPERF",
                        "WAhatWCXs_s55_pop_spl1","WAhatWCXs_s55_pop_spl2","WAhatWCXs_s55_pop_spl3",
                        "WAhatWCXs_s55_dens_spl1","WAhatWCXs_s55_dens_spl2","WAhatWCXs_s55_dens_spl3",
                        "WAhatWCXs_s55_EMPLT3_spl1","WAhatWCXs_s556_EMPLT3_spl2","WAhatWCXs_s55_EMPLT3_spl3",
                        "WAhatWCXs_s55_SUPERF_spl1","WAhatWCXs_s55_SUPERF_spl2","WAhatWCXs_s55_SUPERF_spl3")
write.dta(WAhatWCXs_s55,file="/MAT/mat45/WAhatWCXs_s55.dta")   # Write in Stata format

# Creation of WOhatWC_s55

WBhatWCXs_s55 <- SL2(X,WBhat_syndic1955_45,WC)
names(WBhatWCXs_s55)<-c("year","code_insee_matlab",
                        "WBhatWCXs_s55_pop","WBhatWCXs_s55_densite","WBhatWCXs_s55_SUPERF","WBhatWCXs_s55_droite","WBhatWCXs_s55_gauche",
                        "WBhatWCXs_s55_pc_pop_0014","WBhatWCXs_s55_pc_pop_60P","WBhatWCXs_s55_pc_pop_75P",
                        "WBhatWCXs_s55_TXCHOM3","WBhatWCXs_s55_EMPLT3","WBhatWCXs_s55_TXCADRE3","WBhatWCXs_s55_TXMONO3",
                        "WBhatWCXs_s55_logpop","WBhatWCXs_s55_logdensite","WBhatWCXs_s55_logEMPLT3","WBhatWCXs_s55_logSUPERF",
                        "WBhatWCXs_s55_pop_spl1","WBhatWCXs_s55_pop_spl2","WBhatWCXs_s55_pop_spl3",
                        "WBhatWCXs_s55_dens_spl1","WBhatWCXs_s55_dens_spl2","WBhatWCXs_s55_dens_spl3",
                        "WBhatWCXs_s55_EMPLT3_spl1","WBhatWCXs_s556_EMPLT3_spl2","WBhatWCXs_s55_EMPLT3_spl3",
                        "WBhatWCXs_s55_SUPERF_spl1","WBhatWCXs_s55_SUPERF_spl2","WBhatWCXs_s55_SUPERF_spl3")
write.dta(WBhatWCXs_s55,file="/MAT/mat45/WBhatWCXs_s55.dta")   # Write in Stata format

# Creation of WEhatWOhatWC_s55

WAhatWBhatWCXs_s55 <- SL3(X,WAhat_syndic1955_45,WBhat_syndic1955_45,WC)
names(WAhatWBhatWCXs_s55)<-c("year","code_insee_matlab",
                             "WAhWBhWCXs_s55_pop","WAhWBhWCXs_s55_densite","WAhWBhWCXs_s55_SUPERF","WAhWBhWCXs_s55_droite","WAhWBhWCXs_s55_gauche",
                             "WAhWBhWCXs_s55_pc_pop_0014","WAhWBhWCXs_s55_pc_pop_60P","WAhWBhWCXs_s55_pc_pop_75P",
                             "WAhWBhWCXs_s55_TXCHOM3","WAhWBhWCXs_s55_EMPLT3","WAhWBhWCXs_s55_TXCADRE3","WAhWBhWCXs_s55_TXMONO3",
                             "WAhWBhWCXs_s55_logpop","WAhWBhWCXs_s55_logdensite","WAhWBhWCXs_s55_logEMPLT3","WAhWBhWCXs_s55_logSUPERF",
                             "WAhWBhWCXs_s55_pop_spl1","WAhWBhWCXs_s55_pop_spl2","WAhWBhWCXs_s55_pop_spl3",
                             "WAhWBhWCXs_s55_dens_spl1","WAhWBhWCXs_s55_dens_spl2","WAhWBhWCXs_s55_dens_spl3",
                             "WAhWBhWCXs_s55_EMPLT3_spl1","WAhWBhWCXs_s556_EMPLT3_spl2","WAhWBhWCXs_s55_EMPLT3_spl3",
                             "WAhWBhWCXs_s55_SUPERF_spl1","WAhWBhWCXs_s55_SUPERF_spl2","WAhWBhWCXs_s55_SUPERF_spl3")
write.dta(WAhatWBhatWCXs_s55,file="/MAT/mat45/WAhatWBhatWCXs_s55.dta")   # Write in Stata format

# Creation of WOhatWEhatWC_s55

WBhatWAhatWCXs_s55 <- SL3(X,WBhat_syndic1955_45,WAhat_syndic1955_45,WC)
names(WBhatWAhatWCXs_s55)<-c("year","code_insee_matlab",
                             "WBhWAhWCXs_s55_pop","WBhWAhWCXs_s55_densite","WBhWAhWCXs_s55_SUPERF","WBhWAhWCXs_s55_droite","WBhWAhWCXs_s55_gauche",
                             "WBhWAhWCXs_s55_pc_pop_0014","WBhWAhWCXs_s55_pc_pop_60P","WBhWAhWCXs_s55_pc_pop_75P",
                             "WBhWAhWCXs_s55_TXCHOM3","WBhWAhWCXs_s55_EMPLT3","WBhWAhWCXs_s55_TXCADRE3","WBhWAhWCXs_s55_TXMONO3",
                             "WBhWAhWCXs_s55_logpop","WBhWAhWCXs_s55_logdensite","WBhWAhWCXs_s55_logEMPLT3","WBhWAhWCXs_s55_logSUPERF",
                             "WBhWAhWCXs_s55_pop_spl1","WBhWAhWCXs_s55_pop_spl2","WBhWAhWCXs_s55_pop_spl3",
                             "WBhWAhWCXs_s55_dens_spl1","WBhWAhWCXs_s55_dens_spl2","WBhWAhWCXs_s55_dens_spl3",
                             "WBhWAhWCXs_s55_EMPLT3_spl1","WBhWAhWCXs_s556_EMPLT3_spl2","WBhWAhWCXs_s55_EMPLT3_spl3",
                             "WBhWAhWCXs_s55_SUPERF_spl1","WBhWAhWCXs_s55_SUPERF_spl2","WBhWAhWCXs_s55_SUPERF_spl3")
write.dta(WBhatWAhatWCXs_s55,file="/MAT/mat45/WBhatWAhatWCXs_s55.dta")   # Write in Stata format

# Do the same with X_eicm

WAhatX_eicms_s55_45 <- SL1(X_eicm,WAhat_syndic1955_45)
names(WAhatX_eicms_s55_45)<-c("year","code_insee_matlab",
                              "WAhats_s55_tx_th_EICM","WAhats_s55_tx_FB_EICM","WAhats_s55_tx_FNB_EICM","WAhats_s55_tx_TP_EICM","WAhats_s55_index_EICM","WAhats_s55_tx_CFE_EICM",
                              "WAhats_s55_tx_th_total","WAhats_s55_tx_FB_total","WAhats_s55_tx_FNB_total","WAhats_s55_tx_TP_total","WAhats_s55_index_total","WAhats_s55_tx_CFE_total",
                              "WAhats_s55_CFEZ","WAhats_s55_FA","WAhats_s55_TPU","WAhats_s55_MT")
write.dta(WAhatX_eicms_s55_45,file="/MAT/mat45/WAhatX_eicms_s55_45.dta")   # Write in Stata format
WBhatX_eicms_s55_45 <- SL1(X_eicm,WBhat_syndic1955_45)
names(WBhatX_eicms_s55_45)<-c("year","code_insee_matlab",
                              "WBhats_s55_tx_th_EICM","WBhats_s55_tx_FB_EICM","WBhats_s55_tx_FNB_EICM","WBhats_s55_tx_TP_EICM","WBhats_s55_index_EICM","WBhats_s55_tx_CFE_EICM",
                              "WBhats_s55_tx_th_total","WBhats_s55_tx_FB_total","WBhats_s55_tx_FNB_total","WBhats_s55_tx_TP_total","WBhats_s55_index_total","WBhats_s55_tx_CFE_total",
                              "WBhats_s55_CFEZ","WBhats_s55_FA","WBhats_s55_TPU","WBhats_s55_MT")
write.dta(WBhatX_eicms_s55_45,file="/MAT/mat45/WBhatX_eicms_s55_45.dta")   # Write in Stata format
WCX_eicms_s55_45 <- SL1(X_eicm,WC)
names(WCX_eicms_s55_45)<-c("year","code_insee_matlab",
                           "WC_s55_tx_th_EICM","WC_s55_tx_FB_EICM","WC_s55_tx_FNB_EICM","WC_s55_tx_TP_EICM","WC_s55_index_EICM","WC_s55_tx_CFE_EICM",
                           "WC_s55_tx_th_total","WC_s55_tx_FB_total","WC_s55_tx_FNB_total","WC_s55_tx_TP_total","WC_s55_index_total","WC_s55_tx_CFE_total",
                           "WC_s55_CFEZ","WC_s55_FA","WC_s55_TPU","WC_s55_MT")
write.dta(WCX_eicms_s55_45,file="/MAT/mat45/WCX_eicms_s55_45.dta")   
WAWBhatX_eicms_s55_45 <- SL2(X_eicm,WAhat_syndic1955_45,WBhat_syndic1955_45)
names(WAWBhatX_eicms_s55_45)<-c("year","code_insee_matlab",
                                "WAhWBhs_s55_tx_th_EICM","WAhWBhs_s55_tx_FB_EICM","WAhWBhs_s55_tx_FNB_EICM","WAhWBhs_s55_tx_TP_EICM","WAhWBhs_s55_index_EICM","WAhWBhs_s55_tx_CFE_EICM",
                                "WAhWBhs_s55_tx_th_total","WAhWBhs_s55_tx_FB_total","WAhWBhs_s55_tx_FNB_total","WAhWBhs_s55_tx_TP_total","WAhWBhs_s55_index_total","WAhWBhs_s55_tx_CFE_total",
                                "WAhWBhs_s55_CFEZ","WAhWBhs_s55_FA","WAhWBhs_s55_TPU","WAhWBhs_s55_MT")
write.dta(WAWBhatX_eicms_s55_45,file="/MAT/mat45/WAWBhatX_eicms_s55_45.dta")   # Write in Stata format
WBWAhatX_eicms_s55_45 <- SL2(X_eicm,WBhat_syndic1955_45,WAhat_syndic1955_45)
names(WBWAhatX_eicms_s55_45)<-c("year","code_insee_matlab",
                                "WBhWAh_s55_tx_th_EICM","WBhWAh_s55_tx_FB_EICM","WBhWAh_s55_tx_FNB_EICM","WBhWAh_s55_tx_TP_EICM","WBhWAh_s55_index_EICM","WBhWAh_s55_tx_CFE_EICM",
                                "WBhWAh_s55_tx_th_total","WBhWAh_s55_tx_FB_total","WBhWAh_s55_tx_FNB_total","WBhWAh_s55_tx_TP_total","WBhWAh_s55_index_total","WBhWAh_s55_tx_CFE_total",
                                "WBhWAh_s55_CFEZ","WBhWAh_s55_FA","WBhWAh_s55_TPU","WBhWAh_s55_MT")
write.dta(WBWAhatX_eicms_s55_45,file="/MAT/mat45/WBWAhatX_eicms_s55_45.dta")   # Write in Stata format.
WAhatWC_eicms_s55_45 <- SL2(X_eicm,WAhat_syndic1955_45,WC)
names(WAhatWC_eicms_s55_45)<-c("year","code_insee_matlab",
                               "WAhatWCs_s55_tx_th_EICM","WAhatWCs_s55_tx_FB_EICM","WAhatWCs_s55_tx_FNB_EICM","WAhatWCs_s55_tx_TP_EICM","WAhatWCs_s55_index_EICM","WAhatWCs_s55_tx_CFE_EICM",
                               "WAhatWCs_s55_tx_th_total","WAhatWCs_s55_tx_FB_total","WAhatWCs_s55_tx_FNB_total","WAhatWCs_s55_tx_TP_total","WAhatWCs_s55_index_total","WAhatWCs_s55_tx_CFE_total",
                               "WAhatWCs_s55_CFEZ","WAhatWCs_s55_FA","WAhatWCs_s55_TPU","WAhatWCs_s55_MT")
write.dta(WAhatWC_eicms_s55_45,file="/MAT/mat45/WAhatWC_eicms_s55_45.dta")   # Write in Stata format
WBhatWC_eicms_s55_45 <- SL2(X_eicm,WBhat_syndic1955_45,WC)
names(WBhatWC_eicms_s55_45)<-c("year","code_insee_matlab",
                               "WBhatWCs_s55_tx_th_EICM","WBhatWCs_s55_tx_FB_EICM","WBhatWCs_s55_tx_FNB_EICM","WBhatWCs_s55_tx_TP_EICM","WBhatWCs_s55_index_EICM","WBhatWCs_s55_tx_CFE_EICM",
                               "WBhatWCs_s55_tx_th_total","WBhatWCs_s55_tx_FB_total","WBhatWCs_s55_tx_FNB_total","WBhatWCs_s55_tx_TP_total","WBhatWCs_s55_index_total","WBhatWCs_s55_tx_CFE_total",
                               "WBhatWCs_s55_CFEZ","WBhatWCs_s55_FA","WBhatWCs_s55_TPU","WBhatWCs_s55_MT")
write.dta(WBhatWC_eicms_s55_45,file="/MAT/mat45/WBhatWC_eicms_s55_45")   # Write in Stata format
WAhatWBhatWC_eicms_s55_45 <- SL3(X_eicm,WAhat_syndic1955_45,WBhat_syndic1955_45,WC)
names(WAhatWBhatWC_eicms_s55_45)<-c("year","code_insee_matlab",
                                    "WAhWBhWCs_s55_tx_th_EICM","WAhWBhWCs_s55_tx_FB_EICM","WAhWBhWCs_s55_tx_FNB_EICM","WAhWBhWCs_s55_tx_TP_EICM","WAhWBhWCs_s55_index_EICM","WAhWBhWCs_s55_tx_CFE_EICM",
                                    "WAhWBhWCs_s55_tx_th_total","WAhWBhWCs_s55_tx_FB_total","WAhWBhWCs_s55_tx_FNB_total","WAhWBhWCs_s55_tx_TP_total","WAhWBhWCs_s55_index_total","WAhWBhWCs_s55_tx_CFE_total",
                                    "WAhWBhWCs_s55_CFEZ","WAhWBhWCs_s55_FA","WAhWBhWCs_s55_TPU","WAhWBhWsC_s55_MT")
write.dta(WAhatWBhatWC_eicms_s55_45,file="/MAT/mat45/WAhatWBhatWC_eicms_s55_45.dta")   # Write in Stata format
WBhatWAhatWC_eicms_s55_45 <- SL3(X_eicm,WAhat_syndic1955_45,WBhat_syndic1955_45,WC)
names(WBhatWAhatWC_eicms_s55_45)<-c("year","code_insee_matlab",
                                    "WBhWAhWCs_s55_tx_th_EICM","WBhWAhWCs_s55_tx_FB_EICM","WBhWAhWCs_s55_tx_FNB_EICM","WBhWAhWCs_s55_tx_TP_EICM","WBhWAhWCs_s55_index_EICM","WBhWAhWCs_s55_tx_CFE_EICM",
                                    "WBhWAhWCs_s55_tx_th_total","WBhatWBhatWCs_s55_tx_FB_total","WBhatWBhatWCs_s55_tx_FNB_total","WBhatWBhatWCs_s55_tx_TP_total","WBhatWBhatWCs_s55_index_total","WBhatWBhatWCs_s55_tx_CFE_total",
                                    "WBhWAhWCs_s55_CFEZ","WBhWAhWCs_s55_FA","WBhWAhWCs_s55_TPU","WBhWAhWCs_s55_MT")
write.dta(WBhatWAhatWC_eicms_s55_45,file="/MAT/mat45/WBhatWAhatWC_eicms_s55_45.dta")   # Write in Stata format


######################################################################################
# Creation of spatial lags for the explanatory variables (instruments for KP approach) with flows
######################################################################################

# Creation of WEhatXs_s55

load("/MAT/mat45/WAhat_s_flows.Rda")

WAhatXs_flows <- SL1(X,WAhat_flows)
names(WAhatXs_flows)<-c("year","code_insee_matlab",
                         "WAhats_flows_pop","WAhats_flows_densite","WAhats_flows_SUPERF","WAhats_flows_droite","WAhats_flows_gauche",
                         "WAhats_flows_pc_pop_0014","WAhats_flows_pc_pop_60P","WAhats_flows_pc_pop_75P",
                         "WAhats_flows_TXCHOM3","WAhats_flows_EMPLT3","WAhats_flows_TXCADRE3","WAhats_flows_TXMONO3",
                         "WAhats_flows_logpop","WAhats_flows_logdensite","WAhats_flows_logEMPLT3","WAhats_flows_logSUPERF",
                         "WAhats_flows_pop_spl1","WAhats_flows_pop_spl2","WAhats_flows_pop_spl3",
                         "WAhats_flows_dens_spl1","WAhats_flows_dens_spl2","WAhats_flows_dens_spl3",
                         "WAhats_flows_EMPLT3_spl1","WAhats_flows_EMPLT3_spl2","WAhats_flows_EMPLT3_spl3",
                         "WAhats_flows_SUPERF_spl1","WAhats_flows_SUPERF_spl2","WAhats_flows_SUPERF_spl3")
write.dta(WAhatXs_flows,file="/MAT/mat45/WAhatXs_flows.dta")   # Write in Stata format

# Creation of WOhatXs_s55

load("/MAT/mat45/WBhat_s_flows.Rda")

WBhatXs_flows <- SL1(X,WBhat_syndic1955_45)
names(WBhatXs_flows)<-c("year","code_insee_matlab",
                         "WBhats_flows_pop","WBhats_flows_densite","WBhats_flows_SUPERF","WBhats_flows_droite","WBhats_flows_gauche",
                         "WBhats_flows_pc_pop_0014","WBhats_flows_pc_pop_60P","WBhats_flows_pc_pop_75P",
                         "WBhats_flows_TXCHOM3","WBhats_flows_EMPLT3","WBhats_flows_TXCADRE3","WBhats_flows_TXMONO3",
                         "WBhats_flows_logpop","WBhats_flows_logdensite","WBhats_flows_logEMPLT3","WBhats_flows_logSUPERF",
                         "WBhats_flows_pop_spl1","WBhats_flows_pop_spl2","WBhats_flows_pop_spl3",
                         "WBhats_flows_dens_spl1","WBhats_flows_dens_spl2","WBhats_flows_dens_spl3",
                         "WBhats_flows_EMPLT3_spl1","WBhats_flows_EMPLT3_spl2","WBhats_flows_EMPLT3_spl3",
                         "WBhats_flows_SUPERF_spl1","WBhats_flows_SUPERF_spl2","WBhats_flows_SUPERF_spl3")
write.dta(WBhatXs_flows,file="/MAT/mat45/WBhatXs_flows.dta")   # Write in Stata format

# Creation of WEhatWOhatXs_s55

WAhatWBhatXs_flows <- SL2(X,WAhat_syndic1955_45,WBhat_syndic1955_45)
names(WAhatWBhatXs_flows)<-c("year","code_insee_matlab",
                              "WAhatWBhats_flows_pop","WAhatWBhats_flows_densite","WAhatWBhats_flows_SUPERF","WAhatWBhats_flows_droite","WAhatWBhats_flows_gauche",
                              "WAhatWBhats_flows_pc_pop_0014","WAhatWBhats_flows_pc_pop_60P","WAhatWBhats_flows_pc_pop_75P",
                              "WAhatWBhats_flows_TXCHOM3","WAhatWBhats_flows_EMPLT3","WAhatWBhats_flows_TXCADRE3","WAhatWBhats_flows_TXMONO3",
                              "WAhatWBhats_flows_logpop","WAhatWBhats_flows_logdensite","WAhatWBhats_flows_logEMPLT3","WAhatWBhats_flows_logSUPERF",
                              "WAhatWBhats_flows_pop_spl1","WAhatWBhats_flows_pop_spl2","WAhatWBhats_flows_pop_spl3",
                              "WAhatWBhats_flows_dens_spl1","WAhatWBhats_flows_dens_spl2","WAhatWBhats_flows_dens_spl3",
                              "WAhatWBhats_flows_EMPLT3_spl1","WAhatWBhats_flows_EMPLT3_spl2","WAhatWBhats_flows_EMPLT3_spl3",
                              "WAhatWBhats_flows_SUPERF_spl1","WAhatWBhats_flows_SUPERF_spl2","WAhatWBhats_flows_SUPERF_spl3")
write.dta(WAhatWBhatXs_flows,file="/MAT/mat45/WAhatWBhatXs_flows.dta")   # Write in Stata format

# Creation of WOhatWEhat_s55

WBhatWAhatXs_flows <- SL2(X,WBhat_syndic1955_45,WAhat_syndic1955_45)
names(WBhatWAhatXs_flows)<-c("year","code_insee_matlab",
                              "WBhatWAhats_flows_pop","WBhatWAhats_flows_densite","WBhatWAhats_flows_SUPERF","WBhatWAhats_flows_droite","WBhatWAhats_flows_gauche",
                              "WBhatWAhats_flows_pc_pop_0014","WBhatWAhats_flows_pc_pop_60P","WBhatWAhats_flows_pc_pop_75P",
                              "WBhatWAhats_flows_TXCHOM3","WBhatWAhats_flows_EMPLT3","WBhatWAhats_flows_TXCADRE3","WBhatWAhats_flows_TXMONO3",
                              "WBhatWAhats_flows_logpop","WBhatWAhats_flows_logdensite","WBhatWAhats_flows_logEMPLT3","WBhatWAhats_flows_logSUPERF",
                              "WBhatWAhats_flows_pop_spl1","WBhatWAhats_flows_pop_spl2","WBhatWAhats_flows_pop_spl3",
                              "WBhatWAhats_flows_dens_spl1","WBhatWAhats_flows_dens_spl2","WBhatWAhats_flows_dens_spl3",
                              "WBhatWAhats_flows_EMPLT3_spl1","WBhatWAhats_flows_EMPLT3_spl2","WBhatWAhats_flows_EMPLT3_spl3",
                              "WBhatWAhats_flows_SUPERF_spl1","WBhatWAhats_flows_SUPERF_spl2","WBhatWAhats_flows_SUPERF_spl3")
write.dta(WBhatWAhatXs_flows,file="/MAT/mat45/WBhatWAhatXs_flows.dta")   # Write in Stata format