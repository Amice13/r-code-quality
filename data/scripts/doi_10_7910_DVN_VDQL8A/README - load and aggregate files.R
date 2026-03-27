s <- Sys.time()
files2load <- c(
  'DONALD.txt.LL.rdata',
  'DONALD.txt.LB.rdata',
  'DONALD.txt.RR.rdata',
  'DONALD.txt.CT.rdata'
)

lapply(files2load,load,.GlobalEnv)

DONALD <- rbind(
  DONALD.txt.LL,
  DONALD.txt.LB,
  DONALD.txt.RR,
  DONALD.txt.CT
  )
Sys.time()-s
# Time difference of 1.084206 mins
rm(list = setdiff(ls(), c('DONALD')))