## Author: Kabir Khannna
## Updated: January 8, 2016

library(ei)

## Load Precinct-Level Data
load("prc.eth.RData")

eth <- c("whi", "bla", "his", "asi", "oth")

ei.cd <- matrix(NA, nrow = 25, ncol = 5)
ei.prc <- NULL

set.seed(3)

for (i in 1:25) {
  print(paste("District", i))
  df.temp <- ei.prc.temp <- prc.eth[prc.eth$District == i, ]
  ei.prc.temp[eth] <- NA

  ei.whi <- ei(formula = vote ~ whi, total = "n", data = df.temp)
  ei.prc.temp$whi <- eiread(ei.whi, "betab")
  
  ei.bla <- ei(formula = vote ~ bla, total = "n", data = df.temp)
  ei.prc.temp$bla <- eiread(ei.bla, "betab")
  
  ei.his <- ei(formula = vote ~ his, total = "n", data = df.temp)
  ei.prc.temp$his <- eiread(ei.his, "betab")
  
  ei.asi <- ei(formula = vote ~ asi, total = "n", data = df.temp)
  ei.prc.temp$asi <- eiread(ei.asi, "betab")
  
  ei.oth <- ei(formula = vote ~ oth, total = "n", data = df.temp)
  ei.prc.temp$oth <- eiread(ei.oth, "betab")

  ei.prc <- rbind(ei.prc, ei.prc.temp)
  
  for (k in 1:5) {
    ei.cd[i, k] <- 
      sum(df.temp[eth[k]] * df.temp$n * ei.prc.temp[eth[k]], na.rm = T) / 
      sum(df.temp[eth[k]] * df.temp$n)
  }
}

save(ei.cd, file = "ei.cd.RData") #Save district-level EI results

## Subset duplicated precincts, i.e., in more than one district
dup <- names(table(ei.prc$Precinct)[table(ei.prc$Precinct) > 1])
dup <- dup[order(dup)]

## Temporarily remove duplicate precincts from turnout matrices
ei.prc.dedup <- ei.prc[ei.prc$Precinct %in% dup == F, c("Distrct", "Precinct", "n", "vote",  eth)]

#Calculate turnout by race in duplicated precincts and add back to matrix
for (d in 1:length(dup)) {
  print(paste("Duplicate Precinct ", d, ": ", dup[d], sep = ""))
  ei.prc.dup.temp <- ei.prc[ei.prc$Precinct == dup[d], ]

  ei.prc.dedup.temp <- as.data.frame(matrix(NA, nrow = 1, ncol = 9))
  names(ei.prc.dedup.temp) <- c("District", "Precinct", "n", "vote",  eth)
  
  ei.prc.dedup.temp$District <- paste(unique(ei.prc.dup.temp$District), collapse = " ")
  ei.prc.dedup.temp$Precinct <- dup[d]
  ei.prc.dedup.temp$n <- sum(ei.prc.dup.temp$n)
  ei.prc.dedup.temp$vote <- sum(ei.prc.dup.temp$n * ei.prc.dup.temp$vote, na.rm = T) / sum(ei.prc.dup.temp$n, na.rm = T)
  ei.prc.dedup.temp$whi <- sum(ei.prc.dup.temp$n.whi * ei.prc.dup.temp$whi, na.rm = T) / sum(ei.prc.dup.temp$n.whi, na.rm = T)
  ei.prc.dedup.temp$bla <- sum(ei.prc.dup.temp$n.bla * ei.prc.dup.temp$bla, na.rm = T) / sum(ei.prc.dup.temp$n.bla, na.rm = T)
  ei.prc.dedup.temp$his <- sum(ei.prc.dup.temp$n.his * ei.prc.dup.temp$his, na.rm = T) / sum(ei.prc.dup.temp$n.his, na.rm = T)
  ei.prc.dedup.temp$asi <- sum(ei.prc.dup.temp$n.asi * ei.prc.dup.temp$asi, na.rm = T) / sum(ei.prc.dup.temp$n.asi, na.rm = T)
  ei.prc.dedup.temp$oth <- sum(ei.prc.dup.temp$n.oth * ei.prc.dup.temp$oth, na.rm = T) / sum(ei.prc.dup.temp$n.oth, na.rm = T)
  
  ei.prc.dedup <- rbind(ei.prc.dedup, ei.prc.dedup.temp)
}
ei.prc <- ei.prc.dedup[order(as.character(ei.prc.dedup$Precinct)), ]

save(ei.prc, file = "ei.prc.RData")
