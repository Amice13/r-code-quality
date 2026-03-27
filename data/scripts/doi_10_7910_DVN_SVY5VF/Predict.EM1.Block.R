## Author: Kabir Khanna
## Updated: December 20, 2015

library(foreach)
library(doMPI)

source("EM1.Block.R")

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
blocks <- names(table(df.i$Block)[table(df.i$Block) != 0])

df.em <- foreach(b = 1:length(blocks), .combine = 'rbind') %dopar% {
  print(paste("Block ", b, ": ", blocks[b], sep = ""))
  df.blk <- df.i[df.i$Block == blocks[b], ]
  
  ## Check empty cells
  keep.eth <- rep(TRUE, 5)
  for (k in 1:5) {
    if (sum(df.blk[paste("r_blk", eth[k], sep = "_")], na.rm = T) == 0) {
      keep.eth[k] <- FALSE
    }
  }
  eth.temp <- eth[keep.eth]

  pid <- c(0, 1, 2)
  keep.pid <- rep(TRUE, 3)
  for (p in 1:3) {
    keep.pid[p] <- ifelse(nrow(df.blk[df.blk$PID == p - 1, ]) == 0, FALSE, TRUE)
  }
  pid.temp <- pid[keep.pid]

  ## Apply EM Algorithm by Block
  if (length(eth.temp) > 0) {
    df.blk <- EM1.Block(df.blk, eth.temp, pid.temp) #Pr(Race | Name, Block, and Party)
    ## Set missing predictions to zero
    for (k in 1:length(eth.temp)) {
      df.blk[paste("pred", eth.temp[k], sep = ".")] <- ifelse(!is.na(df.blk[, paste("pred", eth.temp[k], sep = ".")]), df.blk[, paste("pred", eth.temp[k], sep = ".")], 0)
    }
    eth.miss <- eth[keep.eth == F]
    if (length(eth.miss) > 0) {
      for (k in 1:length(eth.miss)) {
        df.blk[paste("pred", eth.miss[k], sep = ".")] <- 0
      }
    }
  }
  if (length(eth.temp) == 0) {
    df.blk$pred.oth <- df.blk$pred.asi <- df.blk$pred.his <- df.blk$pred.bla <- df.blk$pred.whi <- NA
  }
  
  return(df.blk)
}

## Set predictions to missing if likelihoods are missing or zero for all races
df.em[is.na(df.em$r_blk_whi) & is.na(df.em$r_blk_bla) & is.na(df.em$r_blk_his) & is.na(df.em$r_blk_asi) & is.na(df.em$r_blk_oth), paste("pred", eth, sep = ".")] <- NA
df.em[!is.na(df.em$r_blk_whi) & !is.na(df.em$r_blk_bla) & !is.na(df.em$r_blk_his) & !is.na(df.em$r_blk_asi) & !is.na(df.em$r_blk_oth) & df.em$r_blk_whi == 0 & 
        df.em$r_blk_bla == 0 & df.em$r_blk_his == 0 & df.em$r_blk_asi == 0 & df.em$r_blk_oth == 0, paste("pred", eth, sep = ".")] <- NA

save(df.em, file = paste("FL", i, ".EM1.Block.RData", sep = ""))

closeCluster(.c1)
mpi.quit()
