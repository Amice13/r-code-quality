## Author: Kabir Khanna
## Updated: January 8, 2016
## Note: Calculates precinct-level racial counts and proportions by district using self-reported race

load("df.RData")

eth <- c("whi", "bla", "his", "asi", "oth")

prc.eth <- NULL
for (i in 1:25) {
  print(paste("District", i))
  df.i <- df[df$District == i, ]
  precincts <- unique(df.i$Precinct)[order(unique(df.i$Precinct))]
  
  prc.eth.temp <- as.data.frame(matrix(NA, nrow = length(precincts), ncol = 4))
  names(prc.eth.temp) <- c("District", "Precinct", "vote", "n")
  prc.eth.temp$District <- i
  
  for (p in 1:length(precincts)) {
    print(paste("Precinct ", p, " of ", length(precincts), ": ", precincts[p], sep = ""))
    df.prc <- df.i[df.i$Precinct == precincts[p], ]
    prc.eth.temp$Precinct[p] <- precincts[p]
    prc.eth.temp$vote[p] <- sum(df.prc$vote08) / nrow(df.prc)
    prc.eth.temp$n[p] <- nrow(df.prc)
    for (k in 1:5) {
      prc.eth.temp[p, paste("n", eth[k], sep = ".")] <- sum(df.prc[paste("SR", toupper(eth[k]), sep = ".")])
    }
  }
  
  prc.eth <- rbind(prc.eth, prc.eth.temp)
}

for (k in 1:5) {
  prc.eth[eth[k]] <- prc.eth[paste("n", eth[k], sep = ".")] / prc.eth$n
}

save(prc.eth, file = "prc.eth.RData")
