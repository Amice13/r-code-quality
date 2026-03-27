print("Running fig4b_si.R...")

library(tidyverse)
library(compactness) ## This should load all the remaining libraries below, but just in case:

library(sp)
library(sf)
library(rgdal)
library(raster)
library(rgeos)
library(png)
library(pracma)
library(jpeg)
library(image.CornerDetectionHarris)
library(magick)
library(geosphere)
library(shotGroups)
library(RcppRoll)
library(gbm)
library(randomForest)
library(e1071)
library(cleangeo)
library(gdata)

set.seed(02138)
setwd("../data")

# Load data for undergrads who reversed the scale. Not ultimately used in the covariance matrix
f = list.files()
f = f[grepl("j2_L", f) | grepl("j2_E", f) |grepl("j2_R", f)  |grepl("j2_S", f)]
for(i in 1:length(f)){
  temp = read.csv(f[i])
  if(ncol(temp)==1){
    temp = read.csv(f[i], sep="\t")
    if(ncol(temp)==4){
      temp = temp[,c(2,4)]
    }else {
      temp = temp[,c(2,3)]
    }  
  } else if(ncol(temp)==3){
    temp = temp[,c(2,3)]
  } else if(ncol(temp == 4)){
    temp = temp[,c(2,4)]
  }
  colnames(temp) = c("district", "rank")
  temp$rank = temp$rank* -1 + 22
  assign(f[i], temp)
}

ugrads1 = cbind(j2_E.csv[order(j2_E.csv$district),],
                j2_L.csv[order(j2_L.csv$district),],
                j2_R.csv[order(j2_R.csv$district),],
                j2_S.csv[order(j2_S.csv$district),])
ugrads1 = ugrads1[,c(1,2,4,6,8)]

gdata::keep(ugrads1, sure=T)


## Course Demo Students
dat = read.csv("law_students.csv", sep="", header=F)[,c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40)]
setwd("./Shiny Data/shiny_results")
out = list()
for(i in 1:nrow(dat)){
  a = dat[i,]
  a = as.numeric(a)
  a = data.frame(a[1:20], ranks=1:20)
  a = a[order(a$a),]
  out[[i]] = as.numeric(a$ranks)
}
out = do.call(cbind, out)
out = data.frame(out)

f = list.files()
f = f[grepl("a5725a06ce880f6a1790bc991113502f", f)]
f = f[-length(f)]

out2 = list()
for(i in 1:length(f)){
  load(f[i])
  a = as.numeric(a)
  a = data.frame(a[1:20], ranks=1:20)
  a = a[order(a$a),]
  out2[[i]] = as.numeric(a$ranks)
}
out2 = do.call(cbind, out2)
out2 = data.frame(out2)
jds = cbind(out, out2)

gdata::keep(jds, ugrads1, sure=T)


## MTurkers
setwd("../shiny_results2")
f = list.files()
f = f[grepl(".RData", f)]


out = list()
starttime = list()
stoptime = list()
count = list()
for(i in 1:length(f)){
  load(f[i])
  temp = a
  a = as.numeric(a[[1]])
  a = data.frame(a[1:20], ranks=1:20)
  a = a[order(a$a),]
  out[[i]] = as.numeric(a$ranks)
  starttime[[i]] = temp[[4]]
  stoptime[[i]] = temp[[5]]
  count[[i]] = temp[[2]] + temp[[3]]
  
}
out = do.call(cbind, out)
mturkers = data.frame(out)

gdata::keep(ugrads1, jds, mturkers, sure=T)


## other undergrads
setwd("../shiny_results")

ugs = c("14acb23e9801cbc261ce5ea36a190ab0.RData", "0bf36d47247d6297bbc2b33a3fd58d88.RData", "16aa8ecebe5d67a2b605e43266d01c9d.RData",
        "73a7cef4316053d7c637f29bd596180b.RData", "6daf4e19e1ff0e67a6e48aa509da90dc.RData", "53a9e4588c7605af09f38ce0ff07069a.RData",
        "adc6b7144da80db8ce847caaf2c7bd52.RData", "1a7d50350f4435f9d90eac0efab0bf45.RData",
        "f0d5f4ba761c75ce0a2417c6d73034cf.RData")
ugs = ugs[ugs %in% list.files()]

f = list.files()
f = f[grepl("a552", f)]
f = c(f, ugs)
f = f[-4]

out2 = list()
for(i in 1:length(f)){
  load(f[i])
  a = as.numeric(a)
  a = data.frame(a[1:20], ranks=1:20)
  a = a[order(a$a),]
  out2[[i]] = as.numeric(a$ranks)
}
out2 = do.call(cbind, out2)
ugrads2 = data.frame(out2)

gdata::keep(ugrads1, ugrads2, jds, mturkers, sure=T)

## PhD students

f = list.files()
f = f[grepl("688b47", f)]

out2 = list()
for(i in 1:length(f)){
  load(f[i])
  a = as.numeric(a)
  a = data.frame(a[1:20], ranks=1:20)
  a = a[order(a$a),]
  out2[[i]] = as.numeric(a$ranks)
}
out2 = do.call(cbind, out2)
phds = data.frame(out2)

gdata::keep(ugrads1, ugrads2, phds, jds, mturkers, sure=T)


## Profs

profs = c("1b7c971570862ee8d0a7efb8071652e4.RData", "3b181bb36518459cfc8bb5ca980f2dc4.RData", "4674a449ae0998f9e5f22aa508fbbf8a.RData",
          "48a5c46c23b6ed09586a36705467f802.RData", "59186b5e76037f5e98a6d070def5a3bd.RData", "6e4b2d2f475ec7da3da208e12439006b.RData",
          "ed5d11b9c5efeb5fd7b9f3989e542844.RData", "f9ed5d11b9c5efeb5fd7b9f3989e54284435d8245567377d5c2126181c977d9d.RData", "6e277925258d630326aed30504e2fc07.RData",
          "fa6fd1342611fc0200e8b47e36dfa6df.RData", "0ef26e679e8afcb519c82d1d35a907ed.RData", "91fa8e8b0f87bf70537e8295e4f1e897.RData",
          "ef46fdb0a2367275c1dca174f7cffe37.RData", "ed5d11b9c5efeb5fd7b9f3989e542844.RData", "6d483e2ff0112cd411b0a5f91bd553e9.RData")

f = profs[profs %in% list.files()]
out2 = list()
for(i in 1:length(f)){
  load(f[i])
  a = as.numeric(a)
  a = data.frame(a[1:20], ranks=1:20)
  a = a[order(a$a),]
  out2[[i]] = as.numeric(a$ranks)
}
out2 = do.call(cbind, out2)
profs = data.frame(out2)

gdata::keep(ugrads1, ugrads2, phds, profs, jds, mturkers, sure=T)


## Consultants

consultants = c("093db002891bcecf28da470c48833994.RData", "3b181bb36518459cfc8bb5ca980f2dc4.RData",
                "49b2ec0455323530ad154c15b8b2f580.RData", "7a0cdd5d75ec33606eaf3666bdbbfbdb.RData", "86b3229767ee46db971ce0e457713a11.RData")
f = consultants[consultants %in% list.files()]
out2 = list()
for(i in 1:length(f)){
  load(f[i])
  a = as.numeric(a)
  a = data.frame(a[1:20], ranks=1:20)
  a = a[order(a$a),]
  out2[[i]] = as.numeric(a$ranks)
}
out2 = do.call(cbind, out2)
consultants = data.frame(out2)

gdata::keep(ugrads1, ugrads2, phds, profs, jds, mturkers, consultants, sure=T)

## Public officials

pubs = c("c548ad667dbaa70e4b2c77fa60731255.RData", "7a243ab025092f099a9bf73ff931bb5a.RData", "508a8d6c6267a7f8ed373be84d4fd4b8.RData",
         "92afa8293a4ac54d8912066126044dd7.RData", "d9134eb4eb7b7a7a894b23b337ee289d.RData", "a263f4143811e508a16cfffc27b7712e.RData",
         "21a33ab30e72c3329919a3bcf3d19391.RData", "e9f8d63214077454026a25e5aa0e5c1e.RData", "9b605d3a8827335cf466a1d573d5ef6a.RData",
         "e9f8d63214077454026a25e5aa0e5c1e.RData", "49b2ec0455323530ad154c15b8b2f580.RData", "92e335bee1e6521631dcd0fe3d39447a.RData",
         "9ef0fae965a9a0275f4c1c59a92c3c22.RData", "b51187b6ca7f4a6bdb662ce85e38e80d.RData", "e6ec6017e4f99cec9895ea9f94f99542.RData",
         "52e400a9936d674e19bba23e937ac3ef.RData", "2a4f1a640e2ab4547a725b76214fc0fe.RData", "2f9f7f86b079c7863413d7502ab92445.RData",
         "3e835e45c92248211d4d58d892c2228a.RData", "aa39d96c440bb6c7a63d3d4019193730.RData", "85412b3a4d04053776fb17d869d13509.RData")


f = pubs[pubs %in% list.files()]
f = c(f, list.files()[grepl("6bc8a06c88982cbf0a83a7d428f4fc3d", list.files())])
f = c(f, list.files()[grepl("cb7085e4669fd87ddeff09aa01f47bf9", list.files())])



out2 = list()
for(i in 1:length(f)){
  load(f[i])
  a = as.numeric(a)
  a = data.frame(a[1:20], ranks=1:20)
  a = a[order(a$a),]
  out2[[i]] = as.numeric(a$ranks)
}
out2 = do.call(cbind, out2)
pubs = data.frame(out2)

gdata::keep(ugrads1, ugrads2, phds, profs, jds, mturkers, consultants, pubs, sure=T)


## Judges




judges = c("82e860a100cdf4f2b5bb9f8f3c1e3736.RData", "6293ed573ad8e51fd472640220b3215a.RData", "342c168c18d3e7c0803afff637317814.RData",
           "39edde0f0ad67c5a70e28c927a029e15.RData", "859d9af345b1d2e262b2ed908909c7e4.RData", "4b5004b2f940302c04ba3473fbb77d62.RData",
           "d01196bb34e2790bc0a7d10d04599c3e.RData")

f = list.files()
f = f[grepl("59f6d03b2500ee467a8aa7937ea58941", f)]
f2 = judges[judges %in% list.files()]
f = c(f, f2)


out2 = list()
for(i in 1:length(f)){
  load(f[i])
  a = as.numeric(a)
  a = data.frame(a[1:20], ranks=1:20)
  a = a[order(a$a),]
  out2[[i]] = as.numeric(a$ranks)
}
out2 = do.call(cbind, out2)
judges = data.frame(out2)

gdata::keep(ugrads1, ugrads2, phds, profs, jds, mturkers, consultants, pubs, judges, sure=T)


## Lawyers

lawyers = c("a85b450e89edaad831d015ea12d8b95d.RData", "7ba2c8aab3fccc2f0367c699c9f71064", "62d625b07e29b1498125396b329e7a3d.RData",
            "1fcb34832f1eead557112605016034b7.RData", "1ac63389ac1d68c3489c2d992467903f.RData", "16f07588b4ff5431851d801e6f885519.RData",
            "0a637c782de7529852083f7f30f0d1a6.RData", "rothfuss.RData", "d9134eb4eb7b7a7a894b23b337ee289d.RData",
            "d9134eb4eb7b7a7a894b23b337ee289d.RData", "9b605d3a8827335cf466a1d573d5ef6a.RData", "9dc62d6b1a871745e713ff852c4e6a44.RData",
            "a88ec46be9c0c59f75437a2356015374.RData")
f = lawyers[lawyers %in% list.files()]
out2 = list()
for(i in 1:length(f)){
  load(f[i])
  a = as.numeric(a)
  a = data.frame(a[1:20], ranks=1:20)
  a = a[order(a$a),]
  out2[[i]] = as.numeric(a$ranks)
}
out2 = do.call(cbind, out2)
lawyers = data.frame(out2)

gdata::keep(ugrads1, ugrads2, phds, profs, jds, mturkers, consultants, pubs, judges, lawyers, sure=T)



dat = cbind(consultants, jds, judges, lawyers, mturkers, phds, profs, pubs, ugrads2)
## Swap the ones that correlate strongly negatively
zeroes = c()
for(i in 1:ncol(dat)){
  hold = dat[,i]
  cm = rowMeans(dat[,-1])
  dir = cor(hold, cm)
  if(abs(dir) < 0.1){
    zeroes = c(zeroes, i)
  }
}

dat = dat[,-zeroes]

negs = c()
for(i in 1:ncol(dat)){
  hold = dat[,i]
  cm = rowMeans(dat[,-1])
  dir = cor(hold, cm)
  if(dir < 0){
    negs = c(negs, i)
  }
}

for(i in negs){
  dat[,i] = dat[,i]*-1 + 21
}

cors = c()
for(i in 1:ncol(dat)){
  hold = dat[,i]
  cm = rowMeans(dat[,-1])
  dir = cor(hold, cm)
  cors = c(cors, dir)
}

# Load Shapefiles and Predicted Compactness from District Features
setwd("../../")
load("../results/final_models.RData")

#unzip("allval3.zip") # ignore this if you've already unzipped this file!
shpname = "allval3.shp"
namecol = "name"
shp = read_shapefiles(shp = shpname, namecol = namecol, verbose=T)
feat = generate_features(shp=shp, verbose=FALSE)
feat = feat[,-2]
preds_df = generate_predictions(feat, namecol = namecol, new.models = models)

## Get the distribution of differences
idx = expand.grid(1:20, 1:20)
idx = idx[idx$Var1 != idx$Var2,]
out = c()
for(i in 1:nrow(idx)){
  x = idx$Var1[i]
  y = idx$Var2[i]
  difs = unlist(dat[x,] - dat[y,])
  #qtiles = quantile(abs(difs), c(0.025, 0.975))
  #difs = difs[difs > qtiles[1] & difs < qtiles[2]]
  out[i]  = mean(difs > 0)
}

idx$meandif = sapply(out, FUN=function(x) max(x, 1-x))
#cbind(out, idx$meandif)
preds_df = preds_df[order(preds_df$district),]
idx$pred1 = preds_df$compactness[idx$Var1]
idx$pred2 = preds_df$compactness[idx$Var2]
idx$preddif = idx$pred1 - idx$pred2
idx$preddif = abs(idx$preddif)
idx = idx[idx$Var1!=8 & idx$Var2!=8,]

idx$bin = cut(idx$preddif, breaks=seq(0, 90, 10))
idx2 = idx %>%
  group_by(bin) %>%
  summarize(mean_pctagree = median(meandif),
            lower_pctagree = quantile(meandif, 0.05))
idx2$binmean = c(5,15,25,35,45,55,65,75,85,95)-5

## Depending on your system, you may need the above line to be:
# idx2$binmean = c(5,15,25,35,45,55,65,75,85)

# Generate Plot
png("../results/difference_uncertainty.png", width=600, height=450, units="px")
plot(idx$preddif, idx$meandif, xlab="Difference in Predicted Compactness", ylab="Proportion Agreement", cex.lab=1.5)
m = lm(mean_pctagree ~ binmean + I(binmean^2), data=idx2)
meanpreds = predict(m, newdata=data.frame(binmean = seq(0,45, by=5)))
lines(seq(0,45, by=5), predict(m, newdata=data.frame(binmean = seq(0,45, by=5))), lwd=2)
m = lm(lower_pctagree ~ binmean+ I(binmean^2), data=idx2)
lowpreds = predict(m, newdata=data.frame(binmean = seq(0,60, by=5)))
lines(seq(0,60, by=5), predict(m, newdata=data.frame(binmean = seq(0,60, by=5))), col="red", lwd=2)
segments(x0=c(45,60), x1 = c(80,80),
         y0=c(meanpreds[10], lowpreds[length(lowpreds)]),
         y1=c(meanpreds[10], lowpreds[length(lowpreds)]),
         col=c("black", "red"), lwd=2)
dev.off()

setwd("../code")

## Important note: this figure is very sensitive to the users'
## system information and library versions. It may vary slightly
## from the published version, even with a set seed.