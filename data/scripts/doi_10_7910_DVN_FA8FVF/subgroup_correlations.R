print("Running subgroup_correlations.R...")

### Subgroup correlations: which measures correlate best within subgroups?

load("../results/preds.RData")

library(tidyverse)

finalpreds$district = as.character(finalpreds$district)

temp = strsplit(finalpreds$district, "_")
temp = do.call(rbind, temp)
colnames(temp) = c("state", "chamber", "dist", "year")

dat = cbind(finalpreds, temp)

dat2 = dat %>% group_by(state, chamber, year) %>%
  summarize(polsby_cor = abs(cor(polsby, compactness)),
            hull_cor = abs(cor(hull, compactness)),
            reock_cor =abs(cor(reock, compactness)),
            boyce_cor = abs(cor(boyce, compactness)),
            lenwid_cor = abs(cor(lenwid, compactness)),
            schwartzberg_cor = abs(cor(schwartzberg, compactness)))

library(matrixStats)
dat2$polsby_best = dat2$polsby_cor == matrixStats::rowMaxs(as.matrix(dat2[,4:9]), na.rm=T)
dat2$hull_best = dat2$hull_cor == matrixStats::rowMaxs(as.matrix(dat2[,4:9]), na.rm=T)
dat2$reock_best = dat2$reock_cor == matrixStats::rowMaxs(as.matrix(dat2[,4:9]), na.rm=T)
dat2$boyce_best = dat2$boyce_cor == matrixStats::rowMaxs(as.matrix(dat2[,4:9]), na.rm=T)
dat2$lenwid_best = dat2$lenwid_cor == matrixStats::rowMaxs(as.matrix(dat2[,4:9]), na.rm=T)
dat2$schwartzberg_best = dat2$schwartzberg_cor == matrixStats::rowMaxs(as.matrix(dat2[,4:9]), na.rm=T)

colMeans(dat2[,10:15], na.rm=T)

