print("Running feature_correlation_table.R...")

load("../results/preds.RData")

library(xtable)
library(tidyverse)
set.seed(02138)

## 
cors = c()
rankspear = c()
for( i in 2:(ncol(finalpreds)-1)){
  cors = c(cors, cor(finalpreds$compactness, finalpreds[,i]))
  rankspear = c(rankspear, cor(rank(finalpreds$compactness), rank(finalpreds[,i]), method = "spearman"))
  
}

temp = data.frame(feature = colnames(finalpreds)[2:(ncol(finalpreds)-1)], Pearson = cors, Spearman = rankspear)
temp = temp[order(temp$Pearson, decreasing =T),]
temp # This is the first two columns of all but the last three rows of our feature correlation table


## What happens if we reorder than rank and naively average our measures?
temp = data.frame(feature = colnames(finalpreds)[2:(ncol(finalpreds)-1)], Pearson = cors, Spearman = rankspear)
temp

negvec = c(NA, sign(temp$Pearson))

temp2 = finalpreds
for( i in 2:(ncol(finalpreds)-1)){
  temp2[,i] = rank(finalpreds[,i]*negvec[i])
}
temp2$compactness = rank(temp2$compactness)


## Below are the first two columns of the last three rows of our feature correlation table
temp2$allmean = rowMeans(temp2[,2:28])
a = cor(temp2$allmean, temp2$compactness, method="pearson") 
b = cor(temp2$allmean, temp2$compactness, method="spear") 

temp2$smartmean = rowMeans(temp2[,c(28,29,15,16,13,14)]) 
c = cor(temp2$smartmean, temp2$compactness, method="pearson") 
d = cor(temp2$smartmean, temp2$compactness, method="spear") 

temp2$oldmean = rowMeans(temp2[,c(6,9,10,11,13,14,15,16,17)])
e = cor(temp2$oldmean, temp2$compactness, method="pearson") 
f = cor(temp2$oldmean, temp2$compactness, method="spear") 

temp = rbind(temp,data.frame(feature = c("all", "smart", "old"),
                             Pearson = c(a,c,e),
                             Spearman = c(b,d,f)))


## Do this analysis broken down by state-year
distmap = strsplit(as.character(temp2$district), "_")
temp2$statefips = sapply(distmap, FUN=function(x) x[1])
temp2$chamber = sapply(distmap, FUN=function(x) x[2])
temp2$year = sapply(distmap, FUN=function(x) x[4])
temp2$stateyear = paste(temp2$statefips, temp2$chamber, temp2$year, sep="_")

test = temp2 %>% group_by(stateyear) %>%
  filter(n() > 5) %>%
summarize(dists = n(),
          hullcor = cor(hull, compactness),
          polsbycor = cor(polsby, compactness),
          reockcor = cor(reock, compactness),
          symxcor = cor(sym_x, compactness),
          symycor = cor(sym_y, compactness),
          cornerscor = cor(corners, compactness),
          boycecor = cor(boyce, compactness),
          allcor = cor(allmean, compactness),
          smartcor = cor(smartmean, compactness),
          oldcor = cor(oldmean, compactness)
)



## Below is the last column of our feature correlation table
summs = data.frame(feature = colnames(test)[3:12], avgs = colMeans(test[,3:12]),
                   vars = matrixStats::colSds(as.matrix(test[,3:12])),
                   mins = matrixStats::colMins(as.matrix(test[,3:12])),
                   maxs = matrixStats::colMaxs(as.matrix(test[,3:12])))




temp = temp %>% filter(feature %in% c("hull", "polsby", "reock",
                                      "sym_x", "corners", "boyce",
                                      "old", "all", "smart"))
temp$Pearson_avg = summs$avgs[c(7,1,3,2,6,4,8,9,10)]
temp = temp[c(2,4,3,6,5,1,9,7,8),]
temp$Pearson = abs(temp$Pearson)
temp$Spearman = abs(temp$Spearman)
temp


##### This is the first table in the supplement
writeLines(print(xtable::xtable(temp, digits=2), include.rownames=F),
           "../results/table1_si.tex")



test2 = temp2 %>% group_by(stateyear) %>%
  filter(n() > 1) %>%
  summarize(dists = n(),
            hullcor = cor(hull, compactness),
            polsbycor = cor(polsby, compactness),
            reockcor = cor(reock, compactness),
            grofmancor = cor(bbox, compactness),
            symxcor = cor(sym_x, compactness),
            symycor = cor(sym_y, compactness),
            cornerscor = cor(corners, compactness),
            boycecor = cor(boyce, compactness),
            lenwidcor = cor(lenwid, compactness),
            jaggedcor = cor(jagged, compactness)*-1,
            partcor = cor(parts, compactness),
            circaracor = cor(circle_area, compactness),
            cornervarcor = cor(cornervar_ratio, compactness),
            oldcor = cor(oldmean, compactness),
            allcor = cor(allmean, compactness),
            smartmean = cor(smartmean, compactness)
  )


negprop = function(x){
  mean(x<0, na.rm=T)
}

test3 = test2 %>% summarize(across(hullcor:smartmean, negprop))

## This table gives the proportion of negative correlations between our compactness and a given measure       ## among every combination of state-year             
writeLines(print(xtable::xtable(t(test3), digits=3), include.rownames=T),
"../results/table2_si.tex")


## In-text number page 21 paragraph 1
tmp = test2 %>% filter(dists>3)
which(tmp$polsbycor == min(tmp$polsbycor))

tmp2 = test2 %>% dplyr::select(hullcor:cornervarcor)
table(colnames(tmp2)[apply(tmp2,1,which.max)])/nrow(tmp2)




rvec= c()
rvec2 = c()
rvec3 = c()
for(i in 3:ncol(test)){
  rvec[i-2] = max(test[,i] , na.rm=T)
  rvec2[i-2] = min(test[,i] , na.rm=T)
  rvec3[i-2] = min(abs(test[,i]), na.rm=T)
}

## This table gives the highest, lowest, and smallest absolute correlations
## Between a state-year's compactness and the indicated measures                    
writeLines(print(xtable::xtable(data.frame(`Feature` = colnames(test)[3:12],
                                `Highest Correlation` = rvec,
                                `Lowest Correlation` = rvec2,
                                `Smallest Absolute Correlation` = rvec3),
                     digits=3), include.rownames=F),
"../results/table3_si.tex")



