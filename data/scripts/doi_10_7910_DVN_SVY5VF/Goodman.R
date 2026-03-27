## Author: Kabir Khanna
## Updated: January 8, 2016

## Load Precinct-Level Data
load("prc.eth.RData")
prc.eth <- prc.eth[order(prc.eth$Precinct), ] #Sort by Precinct

eth <- c("whi", "bla", "his", "asi", "oth")

## Run Goodman Regression by Congressional District
good.uv.cd <- good.mv.cd <- matrix(NA, nrow = 25, ncol = 5) #Empty matrices for district-level turnout estimates
good.uv.prc <- good.mv.prc <- NULL #Empty matrices for precinct-level turnout estimates

for (i in 1:25) {
  df.temp <- prc.eth[prc.eth$District == i, ]
  
  lm.whi <- lm(vote ~ whi, data = df.temp) #Goodman regression for Whites
  lm.bla <- lm(vote ~ bla, data = df.temp) #Goodman regression for Blacks
  lm.his <- lm(vote ~ his, data = df.temp) #Goodman regression for Hispanics/Latinos
  lm.asi <- lm(vote ~ asi, data = df.temp) #Goodman regression for Asians
  lm.oth <- lm(vote ~ oth, data = df.temp) #Goodman regression for Others
  
  good.uv.cd[i, 1] <- sum(coef(lm.whi)) #Estimate of White turnout at precinct level
  good.uv.cd[i, 2] <- sum(coef(lm.bla)) #Estimate of Black turnout at precinct level
  good.uv.cd[i, 3] <- sum(coef(lm.his)) #Estimate of Hispanic/Latino turnout at precinct level
  good.uv.cd[i, 4] <- sum(coef(lm.asi)) #Estimate of Asian turnout at precinct level
  good.uv.cd[i, 5] <- sum(coef(lm.oth)) #Estimate of Other turnout at precinct level
  
  ## Multivariate Goodman Regression
  lm.mr <- lm(vote ~ 0 + whi + bla + his + asi + oth, data = df.temp)
  good.mv.cd[i, 1] <- coef(lm.mr)["whi"] #Estimate of White turnout at precinct level
  good.mv.cd[i, 2] <- coef(lm.mr)["bla"] #Estimate of Black turnout at precinct level
  good.mv.cd[i, 3] <- coef(lm.mr)["his"] #Estimate of Hispanic/Latino turnout at precinct level
  good.mv.cd[i, 4] <- coef(lm.mr)["asi"] #Estimate of Asian turnout at precinct level
  good.mv.cd[i, 5] <- coef(lm.mr)["oth"] #Estimate of Other turnout at precinct level

  ## Impute district-level turnout estimates to each precinct in district
  good.uv.prc.temp <- good.mv.prc.temp <- df.temp[, c(2, 4:13)]
  good.uv.prc.temp[eth] <- good.mv.prc.temp[eth] <- NA
  good.uv.prc.temp[, 8:12] <- matrix(good.uv.cd[i, ], nrow = nrow(good.uv.prc.temp), ncol = 5, byrow = T)
  good.mv.prc.temp[, 8:12] <- matrix(good.mv.cd[i, ], nrow = nrow(good.mv.prc.temp), ncol = 5, byrow = T)

  good.uv.prc <- rbind(good.uv.prc, good.uv.prc.temp)
  good.mv.prc <- rbind(good.mv.prc, good.mv.prc.temp)
}

save(good.uv.cd, file = "good.uv.cd.RData")
save(good.mv.cd, file = "good.mv.cd.RData")

good.uv.prc <- good.uv.prc[order(good.uv.prc$Precinct), ]
good.mv.prc <- good.mv.prc[order(good.mv.prc$Precinct), ]

## Collect duplicated precincts, i.e., in more than one district
dup <- names(table(good.uv.prc$Precinct)[table(good.uv.prc$Precinct) > 1]) #Identify precincts in more than one district
dup <- dup[order(dup)]

## Temporarily remove duplicate precincts from turnout matrices
good.uv.prc.dedup <- good.uv.prc[good.uv.prc$Precinct %in% dup == F, ]
good.mv.prc.dedup <- good.mv.prc[good.mv.prc$Precinct %in% dup == F, ]

#Calculate turnout by race in duplicated precincts and add back to matrix
for (d in 1:length(dup)) {
  print(paste("Duplicate Precinct ", d, ": ", dup[d], sep = ""))
  good.uv.prc.dup.temp <- good.uv.prc[good.uv.prc$Precinct == dup[d], ]
  good.mv.prc.dup.temp <- good.mv.prc[good.mv.prc$Precinct == dup[d], ]
  
  good.uv.prc.dedup.temp <- good.mv.prc.dedup.temp <- as.data.frame(matrix(NA, nrow = 1, ncol = 12))
  names(good.uv.prc.dedup.temp) <- names(good.mv.prc.dedup.temp) <- c("Precinct", paste("n", eth, sep = "."), "n", eth)
  
  good.uv.prc.dedup.temp$Precinct <- dup[d]
  good.uv.prc.dedup.temp$n <- sum(good.uv.prc.dup.temp$n)
  good.uv.prc.dedup.temp$n.whi <- sum(good.uv.prc.dup.temp$n.whi)
  good.uv.prc.dedup.temp$n.bla <- sum(good.uv.prc.dup.temp$n.bla)
  good.uv.prc.dedup.temp$n.his <- sum(good.uv.prc.dup.temp$n.his)
  good.uv.prc.dedup.temp$n.asi <- sum(good.uv.prc.dup.temp$n.asi)
  good.uv.prc.dedup.temp$n.oth <- sum(good.uv.prc.dup.temp$n.oth)
  good.uv.prc.dedup.temp$whi <- sum(good.uv.prc.dup.temp$n.whi * good.uv.prc.dup.temp$whi) / sum(good.uv.prc.dup.temp$n.whi)
  good.uv.prc.dedup.temp$bla <- sum(good.uv.prc.dup.temp$n.bla * good.uv.prc.dup.temp$bla) / sum(good.uv.prc.dup.temp$n.bla)
  good.uv.prc.dedup.temp$his <- sum(good.uv.prc.dup.temp$n.his * good.uv.prc.dup.temp$his) / sum(good.uv.prc.dup.temp$n.his)
  good.uv.prc.dedup.temp$asi <- sum(good.uv.prc.dup.temp$n.asi * good.uv.prc.dup.temp$asi) / sum(good.uv.prc.dup.temp$n.asi)
  good.uv.prc.dedup.temp$oth <- sum(good.uv.prc.dup.temp$n.oth * good.uv.prc.dup.temp$oth) / sum(good.uv.prc.dup.temp$n.oth)
  
  good.mv.prc.dedup.temp$Precinct <- dup[d]
  good.mv.prc.dedup.temp$n <- sum(good.mv.prc.dup.temp$n)
  good.mv.prc.dedup.temp$n.whi <- sum(good.mv.prc.dup.temp$n.whi)
  good.mv.prc.dedup.temp$n.bla <- sum(good.mv.prc.dup.temp$n.bla)
  good.mv.prc.dedup.temp$n.his <- sum(good.mv.prc.dup.temp$n.his)
  good.mv.prc.dedup.temp$n.asi <- sum(good.mv.prc.dup.temp$n.asi)
  good.mv.prc.dedup.temp$n.oth <- sum(good.mv.prc.dup.temp$n.oth)
  good.mv.prc.dedup.temp$whi <- sum(good.mv.prc.dup.temp$n.whi * good.mv.prc.dup.temp$whi) / sum(good.mv.prc.dup.temp$n.whi)
  good.mv.prc.dedup.temp$bla <- sum(good.mv.prc.dup.temp$n.bla * good.mv.prc.dup.temp$bla) / sum(good.mv.prc.dup.temp$n.bla)
  good.mv.prc.dedup.temp$his <- sum(good.mv.prc.dup.temp$n.his * good.mv.prc.dup.temp$his) / sum(good.mv.prc.dup.temp$n.his)
  good.mv.prc.dedup.temp$asi <- sum(good.mv.prc.dup.temp$n.asi * good.mv.prc.dup.temp$asi) / sum(good.mv.prc.dup.temp$n.asi)
  good.mv.prc.dedup.temp$oth <- sum(good.mv.prc.dup.temp$n.oth * good.mv.prc.dup.temp$oth) / sum(good.mv.prc.dup.temp$n.oth)

  good.uv.prc.dedup <- rbind(good.uv.prc.dedup, good.uv.prc.dedup.temp)
  good.mv.prc.dedup <- rbind(good.mv.prc.dedup, good.mv.prc.dedup.temp)
}
good.uv.prc <- good.uv.prc.dedup[order(as.character(good.uv.prc.dedup$Precinct)), ]
good.mv.prc <- good.mv.prc.dedup[order(as.character(good.mv.prc.dedup$Precinct)), ]

save(good.uv.prc, file = "good.uv.prc.RData")
save(good.mv.prc, file = "good.mv.prc.RData")
