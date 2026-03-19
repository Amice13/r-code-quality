## Author: Kabir Khanna
## Updated: December 20, 2015

library(foreach)
library(doMPI)

source("EM2.Precinct.R")

## Load Data
load("df.RData")

.c1 <- startMPIcluster()
registerDoMPI(.c1)

## Run EM by District (specified in job submission script)
args <- commandArgs(trailingOnly = TRUE)
i <- as.integer(args[1])

## Read in District
print(paste("FL District", i))
df.i <- df[df$District == i, ]
rm(df)

eth <- c("whi", "bla", "his", "asi", "oth")
precincts <- names(table(df.i$Precinct)[table(df.i$Precinct) != 0])

df.em <- foreach(p = 1:length(precincts), .combine = 'rbind') %dopar% {
  print(paste("Precinct ", p, ": ", precincts[p], sep = ""))
  df.prc <- df.i[df.i$Precinct == precincts[p], ]
  
  ## Check empty cells
  keep.eth <- rep(TRUE, 5)
  for (k in 1:5) {
    if (sum(df.prc[paste("r_prc", eth[k], sep = "_")], na.rm = T) == 0) {
      keep.eth[k] <- FALSE
    }
  }
  eth.temp <- eth[keep.eth]

  pid <- c(0, 1, 2)
  keep.pid <- rep(TRUE, 3)
  for (p in 1:3) {
    keep.pid[p] <- ifelse(nrow(df.prc[df.prc$PID == p - 1, ]) == 0, FALSE, TRUE)
  }
  pid.temp <- pid[keep.pid]

  ## Apply EM Algorithm by Precicnt
  if (length(eth.temp) > 0) {
    df.prc <- EM2.Precinct(df.prc, eth.temp, pid.temp) #Pr(Race | Name, Precinct, Demographics, and Party)
    ## Set missing predictions to zero
    for (k in 1:length(eth.temp)) {
      df.prc[paste("pred", eth.temp[k], sep = ".")] <- ifelse(!is.na(df.prc[, paste("pred", eth.temp[k], sep = ".")]), df.prc[, paste("pred", eth.temp[k], sep = ".")], 0)
    }
    eth.miss <- eth[keep.eth == F]
    if (length(eth.miss) > 0) {
      for (k in 1:length(eth.miss)) {
        df.prc[paste("pred", eth.miss[k], sep = ".")] <- 0
      }
    }
  }
  if (length(eth.temp) == 0) {
    df.prc$pred.oth <- df.prc$pred.asi <- df.prc$pred.his <- df.prc$pred.bla <- df.prc$pred.whi <- NA
  }

  return(df.prc)
}

## Set predictions to missing if likelihoods are missing or zero for all races
df.em[is.na(df.em$r_prc_age_sex_whi) & is.na(df.em$r_prc_age_sex_bla) & is.na(df.em$r_prc_age_sex_his) & is.na(df.em$r_prc_age_sex_asi) & is.na(df.em$r_prc_age_sex_oth), paste("pred", eth, sep = ".")] <- NA
df.em[!is.na(df.em$r_prc_age_sex_whi) & !is.na(df.em$r_prc_age_sex_bla) & !is.na(df.em$r_prc_age_sex_his) & !is.na(df.em$r_prc_age_sex_asi) & !is.na(df.em$r_prc_age_sex_oth) & df.em$r_prc_age_sex_whi == 0 & 
        df.em$r_prc_age_sex_bla == 0 & df.em$r_prc_age_sex_his == 0 & df.em$r_prc_age_sex_asi == 0 & df.em$r_prc_age_sex_oth == 0, paste("pred", eth, sep = ".")] <- NA

save(df.em, file = paste("FL", i, ".EM2.Precinct.RData", sep = ""))

closeCluster(.c1)
mpi.quit()
