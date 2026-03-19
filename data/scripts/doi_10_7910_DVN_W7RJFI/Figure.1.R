
#Figure 1e.

#---------------PART1:Left Panel, (MC cross)-------------------
source("Read_data.R")
rm(list=setdiff(ls(), c("GeneInfo","MC.comp.RPKM","MC.F.comp.RPKM", "MC.M.comp.RPKM")))
# get total autosomal gene and chrX gene (both polyA+ and PolyA-)
autoGenes <- rownames(GeneInfo[which(GeneInfo$Chr != "chrX" & GeneInfo$Chr != "chrY"), ])
chrXGenes <- rownames(GeneInfo[which(GeneInfo$Chr == "chrX"), ])

#Female data. Only consider genes with RPKM >=1
auto.F.RPKM <- MC.F.comp.RPKM[autoGenes, ]
chrX.F.RPKM <- MC.F.comp.RPKM[chrXGenes, ]


#late2cell female: sample 2,3,4,6,8
#late2cell - sample2
chrX.F.L2_2.RPKM <- chrX.F.RPKM$L2.Comp2[chrX.F.RPKM$L2.Comp2 >= 1]
auto.F.L2_2.RPKM <- auto.F.RPKM$L2.Comp2[auto.F.RPKM$L2.Comp2 >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.L2_2.RPKM, length(chrX.F.L2_2.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.L2_2.RPKM) / mean(auto.random.F.RPKM)
}
#calculate median X/A ratio
F.XtoA <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Female", stringsAsFactors=FALSE)

#late2cell - sample3
chrX.F.L2_3.RPKM <- chrX.F.RPKM$L2.Comp3[chrX.F.RPKM$L2.Comp3 >= 1]
auto.F.L2_3.RPKM <- auto.F.RPKM$L2.Comp3[auto.F.RPKM$L2.Comp3 >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.L2_3.RPKM, length(chrX.F.L2_3.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.L2_3.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[2,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Female", stringsAsFactors=FALSE)

#late2cell - sample4
chrX.F.L2_4.RPKM <- chrX.F.RPKM$L2.Comp4[chrX.F.RPKM$L2.Comp4 >= 1]
auto.F.L2_4.RPKM <- auto.F.RPKM$L2.Comp4[auto.F.RPKM$L2.Comp4 >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.L2_4.RPKM, length(chrX.F.L2_4.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.L2_4.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[3,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Female", stringsAsFactors=FALSE)

#late2cell - sample6
chrX.F.L2_6.RPKM <- chrX.F.RPKM$L2.Comp6[chrX.F.RPKM$L2.Comp6 >= 1]
auto.F.L2_6.RPKM <- auto.F.RPKM$L2.Comp6[auto.F.RPKM$L2.Comp6 >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.L2_6.RPKM, length(chrX.F.L2_6.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.L2_6.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[4,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Female", stringsAsFactors=FALSE)

#late2cell - sample8
chrX.F.L2_8.RPKM <- chrX.F.RPKM$L2.Comp8[chrX.F.RPKM$L2.Comp8 >= 1]
auto.F.L2_8.RPKM <- auto.F.RPKM$L2.Comp8[auto.F.RPKM$L2.Comp8 >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.L2_8.RPKM, length(chrX.F.L2_8.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.L2_8.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[5,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Female", stringsAsFactors=FALSE)

#4ell females: sample 4,6,11,14,15,16
#4cell - sample4
chrX.F.4_4.RPKM <- chrX.F.RPKM$`4.Comp4`[chrX.F.RPKM$`4.Comp4` >= 1]
auto.F.4_4.RPKM <- auto.F.RPKM$`4.Comp4`[auto.F.RPKM$`4.Comp4` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.4_4.RPKM, length(chrX.F.4_4.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.4_4.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[6,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="4C", Gender="Female", stringsAsFactors=FALSE)

#4cell - sample6
chrX.F.4_6.RPKM <- chrX.F.RPKM$`4.Comp6`[chrX.F.RPKM$`4.Comp6` >= 1]
auto.F.4_6.RPKM <- auto.F.RPKM$`4.Comp6`[auto.F.RPKM$`4.Comp6` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.4_6.RPKM, length(chrX.F.4_6.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.4_6.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[7,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="4C", Gender="Female", stringsAsFactors=FALSE)

#4cell - sample11
chrX.F.4_11.RPKM <- chrX.F.RPKM$`4.Comp11`[chrX.F.RPKM$`4.Comp11` >= 1]
auto.F.4_11.RPKM <- auto.F.RPKM$`4.Comp11`[auto.F.RPKM$`4.Comp11` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.4_11.RPKM, length(chrX.F.4_11.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.4_11.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[8,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="4C", Gender="Female", stringsAsFactors=FALSE)

#4cell - sample14
chrX.F.4_14.RPKM <- chrX.F.RPKM$`4.Comp14`[chrX.F.RPKM$`4.Comp14` >= 1]
auto.F.4_14.RPKM <- auto.F.RPKM$`4.Comp14`[auto.F.RPKM$`4.Comp14` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.4_14.RPKM, length(chrX.F.4_14.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.4_14.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[9,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="4C", Gender="Female", stringsAsFactors=FALSE)

#4cell - sample15
chrX.F.4_15.RPKM <- chrX.F.RPKM$`4.Comp15`[chrX.F.RPKM$`4.Comp15` >= 1]
auto.F.4_15.RPKM <- auto.F.RPKM$`4.Comp15`[auto.F.RPKM$`4.Comp15` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.4_15.RPKM, length(chrX.F.4_15.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.4_15.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[10,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="4C", Gender="Female", stringsAsFactors=FALSE)

#4cell - sample16
chrX.F.4_16.RPKM <- chrX.F.RPKM$`4.Comp16`[chrX.F.RPKM$`4.Comp16` >= 1]
auto.F.4_16.RPKM <- auto.F.RPKM$`4.Comp16`[auto.F.RPKM$`4.Comp16` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.4_16.RPKM, length(chrX.F.4_16.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.4_16.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[11,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="4C", Gender="Female", stringsAsFactors=FALSE)

#8cell female samples: sample 1,5,7,10,14,15
#8cell - sample1
chrX.F.8_1.RPKM <- chrX.F.RPKM$`8.Comp1`[chrX.F.RPKM$`8.Comp1` >= 1]
auto.F.8_1.RPKM <- auto.F.RPKM$`8.Comp1`[auto.F.RPKM$`8.Comp1` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.8_1.RPKM, length(chrX.F.8_1.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.8_1.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[12,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="8C", Gender="Female", stringsAsFactors=FALSE)

#8cell - sampler5
chrX.F.8_5.RPKM <- chrX.F.RPKM$`8.Comp5`[chrX.F.RPKM$`8.Comp5` >= 1]
auto.F.8_5.RPKM <- auto.F.RPKM$`8.Comp5`[auto.F.RPKM$`8.Comp5` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.8_5.RPKM, length(chrX.F.8_5.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.8_5.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[13,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="8C", Gender="Female", stringsAsFactors=FALSE)

#8cell - sample7
chrX.F.8_7.RPKM <- chrX.F.RPKM$`8.Comp7`[chrX.F.RPKM$`8.Comp7` >= 1]
auto.F.8_7.RPKM <- auto.F.RPKM$`8.Comp7`[auto.F.RPKM$`8.Comp7` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.8_7.RPKM, length(chrX.F.8_7.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.8_7.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[14,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="8C", Gender="Female", stringsAsFactors=FALSE)

#8cell - sample10
chrX.F.8_10.RPKM <- chrX.F.RPKM$`8.Comp10`[chrX.F.RPKM$`8.Comp10` >= 1]
auto.F.8_10.RPKM <- auto.F.RPKM$`8.Comp10`[auto.F.RPKM$`8.Comp10` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.8_10.RPKM, length(chrX.F.8_10.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.8_10.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[15,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="8C", Gender="Female", stringsAsFactors=FALSE)

#8cell - sample14
chrX.F.8_14.RPKM <- chrX.F.RPKM$`8.Comp14`[chrX.F.RPKM$`8.Comp14` >= 1]
auto.F.8_14.RPKM <- auto.F.RPKM$`8.Comp14`[auto.F.RPKM$`8.Comp14` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.8_14.RPKM, length(chrX.F.8_14.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.8_14.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[16,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="8C", Gender="Female", stringsAsFactors=FALSE)

#8cell - sample15
chrX.F.8_15.RPKM <- chrX.F.RPKM$`8.Comp15`[chrX.F.RPKM$`8.Comp15` >= 1]
auto.F.8_15.RPKM <- auto.F.RPKM$`8.Comp15`[auto.F.RPKM$`8.Comp15` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.8_15.RPKM, length(chrX.F.8_15.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.8_15.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[17,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="8C", Gender="Female", stringsAsFactors=FALSE)

#16cell female samples: sample 2,3,5,6,7
#16cell - sample2
chrX.F.16_2.RPKM <- chrX.F.RPKM$`16.Comp2`[chrX.F.RPKM$`16.Comp2` >= 1]
auto.F.16_2.RPKM <- auto.F.RPKM$`16.Comp2`[auto.F.RPKM$`16.Comp2` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.16_2.RPKM, length(chrX.F.16_2.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.16_2.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[18,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="16C", Gender="Female", stringsAsFactors=FALSE)

#16cell - sample3
chrX.F.16_3.RPKM <- chrX.F.RPKM$`16.Comp3`[chrX.F.RPKM$`16.Comp3` >= 1]
auto.F.16_3.RPKM <- auto.F.RPKM$`16.Comp3`[auto.F.RPKM$`16.Comp3` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.16_3.RPKM, length(chrX.F.16_3.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.16_3.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[19,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="16C", Gender="Female", stringsAsFactors=FALSE)

#16cell - sample5
chrX.F.16_5.RPKM <- chrX.F.RPKM$`16.Comp5`[chrX.F.RPKM$`16.Comp5` >= 1]
auto.F.16_5.RPKM <- auto.F.RPKM$`16.Comp5`[auto.F.RPKM$`16.Comp5` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.16_5.RPKM, length(chrX.F.16_5.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.16_5.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[20,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="16C", Gender="Female", stringsAsFactors=FALSE)

#16cell - sample6
chrX.F.16_6.RPKM <- chrX.F.RPKM$`16.Comp6`[chrX.F.RPKM$`16.Comp6` >= 1]
auto.F.16_6.RPKM <- auto.F.RPKM$`16.Comp6`[auto.F.RPKM$`16.Comp6` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.16_6.RPKM, length(chrX.F.16_6.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.16_6.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[21,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="16C", Gender="Female", stringsAsFactors=FALSE)

#16cell - sample7
chrX.F.16_7.RPKM <- chrX.F.RPKM$`16.Comp7`[chrX.F.RPKM$`16.Comp7` >= 1]
auto.F.16_7.RPKM <- auto.F.RPKM$`16.Comp7`[auto.F.RPKM$`16.Comp7` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.16_7.RPKM, length(chrX.F.16_7.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.16_7.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[22,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="16C", Gender="Female", stringsAsFactors=FALSE)

#earlyBlast female samples: sample 1,4,5,8,10
#earlyBlast - sample1
chrX.F.eB_1.RPKM <- chrX.F.RPKM$eB.Comp1[chrX.F.RPKM$eB.Comp1 >= 1]
auto.F.eB_1.RPKM <- auto.F.RPKM$eB.Comp1[auto.F.RPKM$eB.Comp1 >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.eB_1.RPKM, length(chrX.F.eB_1.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.eB_1.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[23,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="eB", Gender="Female", stringsAsFactors=FALSE)

#earlyBlast - sample4
chrX.F.eB_4.RPKM <- chrX.F.RPKM$eB.Comp4[chrX.F.RPKM$eB.Comp4 >= 1]
auto.F.eB_4.RPKM <- auto.F.RPKM$eB.Comp4[auto.F.RPKM$eB.Comp4 >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.eB_4.RPKM, length(chrX.F.eB_4.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.eB_4.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[24,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="eB", Gender="Female", stringsAsFactors=FALSE)

#earlyBlast - sample5
chrX.F.eB_5.RPKM <- chrX.F.RPKM$eB.Comp5[chrX.F.RPKM$eB.Comp5 >= 1]
auto.F.eB_5.RPKM <- auto.F.RPKM$eB.Comp5[auto.F.RPKM$eB.Comp5 >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.eB_5.RPKM, length(chrX.F.eB_5.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.eB_5.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[25,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="eB", Gender="Female", stringsAsFactors=FALSE)

#earlyBlast - sample8
chrX.F.eB_8.RPKM <- chrX.F.RPKM$eB.Comp8[chrX.F.RPKM$eB.Comp8 >= 1]
auto.F.eB_8.RPKM <- auto.F.RPKM$eB.Comp8[auto.F.RPKM$eB.Comp8 >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.eB_8.RPKM, length(chrX.F.eB_8.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.eB_8.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[26,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="eB", Gender="Female", stringsAsFactors=FALSE)

#earlyBlast - sample10
chrX.F.eB_10.RPKM <- chrX.F.RPKM$eB.Comp10[chrX.F.RPKM$eB.Comp10 >= 1]
auto.F.eB_10.RPKM <- auto.F.RPKM$eB.Comp10[auto.F.RPKM$eB.Comp10 >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.eB_10.RPKM, length(chrX.F.eB_10.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.eB_10.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[27,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="eB", Gender="Female", stringsAsFactors=FALSE)


#Male data second
# get autosomal gene and chrX gene
auto.M.RPKM <- MC.M.comp.RPKM[autoGenes, ]
chrX.M.RPKM <- MC.M.comp.RPKM[chrXGenes, ]

#late2cell male samples: sample 1,5,7,9,10
#late2cell - sample1
chrX.M.L2_1.RPKM <- chrX.M.RPKM$L2.Comp1[chrX.M.RPKM$L2.Comp1 >= 1]
auto.M.L2_1.RPKM <- auto.M.RPKM$L2.Comp1[auto.M.RPKM$L2.Comp1 >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.L2_1.RPKM, length(chrX.M.L2_1.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.L2_1.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Male", stringsAsFactors=FALSE)

#late2cell - sample5
chrX.M.L2_5.RPKM <- chrX.M.RPKM$L2.Comp5[chrX.M.RPKM$L2.Comp5 >= 1]
auto.M.L2_5.RPKM <- auto.M.RPKM$L2.Comp5[auto.M.RPKM$L2.Comp5 >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.L2_5.RPKM, length(chrX.M.L2_5.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.L2_5.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[2,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Male", stringsAsFactors=FALSE)

#late2cell - sample7
chrX.M.L2_7.RPKM <- chrX.M.RPKM$L2.Comp7[chrX.M.RPKM$L2.Comp7 >= 1]
auto.M.L2_7.RPKM <- auto.M.RPKM$L2.Comp7[auto.M.RPKM$L2.Comp7 >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.L2_7.RPKM, length(chrX.M.L2_7.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.L2_7.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[3,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Male", stringsAsFactors=FALSE)

#late2cell - sample9
chrX.M.L2_9.RPKM <- chrX.M.RPKM$L2.Comp9[chrX.M.RPKM$L2.Comp9 >= 1]
auto.M.L2_9.RPKM <- auto.M.RPKM$L2.Comp9[auto.M.RPKM$L2.Comp9 >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.L2_9.RPKM, length(chrX.M.L2_9.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.L2_9.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[4,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Male", stringsAsFactors=FALSE)

#late2cell - sample10
chrX.M.L2_10.RPKM <- chrX.M.RPKM$L2.Comp10[chrX.M.RPKM$L2.Comp10 >= 1]
auto.M.L2_10.RPKM <- auto.M.RPKM$L2.Comp10[auto.M.RPKM$L2.Comp10 >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.L2_10.RPKM, length(chrX.M.L2_10.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.L2_10.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[5,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Male", stringsAsFactors=FALSE)

#4cell male samples: sample 1,2,3,5,7,8,9,10,12,13,17
#4cell - sample1
chrX.M.4_1.RPKM <- chrX.M.RPKM$`4.Comp1`[chrX.M.RPKM$`4.Comp1` >= 1]
auto.M.4_1.RPKM <- auto.M.RPKM$`4.Comp1`[auto.M.RPKM$`4.Comp1` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.4_1.RPKM, length(chrX.M.4_1.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.4_1.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[6,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="4C", Gender="Male", stringsAsFactors=FALSE)

#4cell - sample2
chrX.M.4_2.RPKM <- chrX.M.RPKM$`4.Comp2`[chrX.M.RPKM$`4.Comp2` >= 1]
auto.M.4_2.RPKM <- auto.M.RPKM$`4.Comp2`[auto.M.RPKM$`4.Comp2` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.4_2.RPKM, length(chrX.M.4_2.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.4_2.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[7,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="4C", Gender="Male", stringsAsFactors=FALSE)

#4cell - sample3
chrX.M.4_3.RPKM <- chrX.M.RPKM$`4.Comp3`[chrX.M.RPKM$`4.Comp3` >= 1]
auto.M.4_3.RPKM <- auto.M.RPKM$`4.Comp3`[auto.M.RPKM$`4.Comp3` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.4_3.RPKM, length(chrX.M.4_3.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.4_3.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[8,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="4C", Gender="Male", stringsAsFactors=FALSE)

#4cell - sample5
chrX.M.4_5.RPKM <- chrX.M.RPKM$`4.Comp5`[chrX.M.RPKM$`4.Comp5` >= 1]
auto.M.4_5.RPKM <- auto.M.RPKM$`4.Comp5`[auto.M.RPKM$`4.Comp5` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.4_5.RPKM, length(chrX.M.4_5.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.4_5.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[9,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="4C", Gender="Male", stringsAsFactors=FALSE)

#4cell - sample7
chrX.M.4_7.RPKM <- chrX.M.RPKM$`4.Comp7`[chrX.M.RPKM$`4.Comp7` >= 1]
auto.M.4_7.RPKM <- auto.M.RPKM$`4.Comp7`[auto.M.RPKM$`4.Comp7` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.4_7.RPKM, length(chrX.M.4_7.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.4_7.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[10,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="4C", Gender="Male", stringsAsFactors=FALSE)

#4cell - sample8
chrX.M.4_8.RPKM <- chrX.M.RPKM$`4.Comp8`[chrX.M.RPKM$`4.Comp8` >= 1]
auto.M.4_8.RPKM <- auto.M.RPKM$`4.Comp8`[auto.M.RPKM$`4.Comp8` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.4_8.RPKM, length(chrX.M.4_8.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.4_8.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[11,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="4C", Gender="Male", stringsAsFactors=FALSE)

#4cell - sample9
chrX.M.4_9.RPKM <- chrX.M.RPKM$`4.Comp9`[chrX.M.RPKM$`4.Comp9` >= 1]
auto.M.4_9.RPKM <- auto.M.RPKM$`4.Comp9`[auto.M.RPKM$`4.Comp9` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.4_9.RPKM, length(chrX.M.4_9.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.4_9.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[12,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="4C", Gender="Male", stringsAsFactors=FALSE)

#4cell - sample10
chrX.M.4_10.RPKM <- chrX.M.RPKM$`4.Comp10`[chrX.M.RPKM$`4.Comp10` >= 1]
auto.M.4_10.RPKM <- auto.M.RPKM$`4.Comp10`[auto.M.RPKM$`4.Comp10` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.4_10.RPKM, length(chrX.M.4_10.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.4_10.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[13,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="4C", Gender="Male", stringsAsFactors=FALSE)

#4cell - sample12
chrX.M.4_12.RPKM <- chrX.M.RPKM$`4.Comp12`[chrX.M.RPKM$`4.Comp12` >= 1]
auto.M.4_12.RPKM <- auto.M.RPKM$`4.Comp12`[auto.M.RPKM$`4.Comp12` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.4_12.RPKM, length(chrX.M.4_12.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.4_12.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[14,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="4C", Gender="Male", stringsAsFactors=FALSE)

#4cell - sample13
chrX.M.4_13.RPKM <- chrX.M.RPKM$`4.Comp13`[chrX.M.RPKM$`4.Comp13` >= 1]
auto.M.4_13.RPKM <- auto.M.RPKM$`4.Comp13`[auto.M.RPKM$`4.Comp13` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.4_13.RPKM, length(chrX.M.4_13.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.4_13.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[15,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="4C", Gender="Male", stringsAsFactors=FALSE)

#4cell - sample17
chrX.M.4_17.RPKM <- chrX.M.RPKM$`4.Comp17`[chrX.M.RPKM$`4.Comp17` >= 1]
auto.M.4_17.RPKM <- auto.M.RPKM$`4.Comp17`[auto.M.RPKM$`4.Comp17` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.4_17.RPKM, length(chrX.M.4_17.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.4_17.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[16,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="4C", Gender="Male", stringsAsFactors=FALSE)

#8cell male samples: sample 2,3,4,6,8,9,11,12,13
#8cell - sample2
chrX.M.8_2.RPKM <- chrX.M.RPKM$`8.Comp2`[chrX.M.RPKM$`8.Comp2` >= 1]
auto.M.8_2.RPKM <- auto.M.RPKM$`8.Comp2`[auto.M.RPKM$`8.Comp2` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.8_2.RPKM, length(chrX.M.8_2.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.8_2.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[17,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="8C", Gender="Male", stringsAsFactors=FALSE)

#8cell - sample3
chrX.M.8_3.RPKM <- chrX.M.RPKM$`8.Comp3`[chrX.M.RPKM$`8.Comp3` >= 1]
auto.M.8_3.RPKM <- auto.M.RPKM$`8.Comp3`[auto.M.RPKM$`8.Comp3` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.8_3.RPKM, length(chrX.M.8_3.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.8_3.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[18,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="8C", Gender="Male", stringsAsFactors=FALSE)

#8cell - sample4
chrX.M.8_4.RPKM <- chrX.M.RPKM$`8.Comp4`[chrX.M.RPKM$`8.Comp4` >= 1]
auto.M.8_4.RPKM <- auto.M.RPKM$`8.Comp4`[auto.M.RPKM$`8.Comp4` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.8_4.RPKM, length(chrX.M.8_4.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.8_4.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[19,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="8C", Gender="Male", stringsAsFactors=FALSE)

#8cell - sample6
chrX.M.8_6.RPKM <- chrX.M.RPKM$`8.Comp6`[chrX.M.RPKM$`8.Comp6` >= 1]
auto.M.8_6.RPKM <- auto.M.RPKM$`8.Comp6`[auto.M.RPKM$`8.Comp6` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.8_6.RPKM, length(chrX.M.8_6.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.8_6.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[20,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="8C", Gender="Male", stringsAsFactors=FALSE)

#8cell - sample8
chrX.M.8_8.RPKM <- chrX.M.RPKM$`8.Comp8`[chrX.M.RPKM$`8.Comp8` >= 1]
auto.M.8_8.RPKM <- auto.M.RPKM$`8.Comp8`[auto.M.RPKM$`8.Comp8` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.8_8.RPKM, length(chrX.M.8_8.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.8_8.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[21,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="8C", Gender="Male", stringsAsFactors=FALSE)

#8cell - sample9
chrX.M.8_9.RPKM <- chrX.M.RPKM$`8.Comp9`[chrX.M.RPKM$`8.Comp9` >= 1]
auto.M.8_9.RPKM <- auto.M.RPKM$`8.Comp9`[auto.M.RPKM$`8.Comp9` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.8_9.RPKM, length(chrX.M.8_9.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.8_9.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[22,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="8C", Gender="Male", stringsAsFactors=FALSE)

#8cell - sample11
chrX.M.8_11.RPKM <- chrX.M.RPKM$`8.Comp11`[chrX.M.RPKM$`8.Comp11` >= 1]
auto.M.8_11.RPKM <- auto.M.RPKM$`8.Comp11`[auto.M.RPKM$`8.Comp11` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.8_11.RPKM, length(chrX.M.8_11.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.8_11.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[23,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="8C", Gender="Male", stringsAsFactors=FALSE)

#8cell - sample12
chrX.M.8_12.RPKM <- chrX.M.RPKM$`8.Comp12`[chrX.M.RPKM$`8.Comp12` >= 1]
auto.M.8_12.RPKM <- auto.M.RPKM$`8.Comp12`[auto.M.RPKM$`8.Comp12` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.8_12.RPKM, length(chrX.M.8_12.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.8_12.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[24,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="8C", Gender="Male", stringsAsFactors=FALSE)

#8cell - sample13
chrX.M.8_13.RPKM <- chrX.M.RPKM$`8.Comp13`[chrX.M.RPKM$`8.Comp13` >= 1]
auto.M.8_13.RPKM <- auto.M.RPKM$`8.Comp13`[auto.M.RPKM$`8.Comp13` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.8_13.RPKM, length(chrX.M.8_13.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.8_13.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[25,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="8C", Gender="Male", stringsAsFactors=FALSE)

#16cell male samples: sample 1,4,8
#16cell - sample1
chrX.M.16_1.RPKM <- chrX.M.RPKM$`16.Comp1`[chrX.M.RPKM$`16.Comp1` >= 1]
auto.M.16_1.RPKM <- auto.M.RPKM$`16.Comp1`[auto.M.RPKM$`16.Comp1` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.16_1.RPKM, length(chrX.M.16_1.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.16_1.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[26,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="16C", Gender="Male", stringsAsFactors=FALSE)

#16cell - sample4
chrX.M.16_4.RPKM <- chrX.M.RPKM$`16.Comp4`[chrX.M.RPKM$`16.Comp4` >= 1]
auto.M.16_4.RPKM <- auto.M.RPKM$`16.Comp4`[auto.M.RPKM$`16.Comp4` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.16_4.RPKM, length(chrX.M.16_4.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.16_4.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[27,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="16C", Gender="Male", stringsAsFactors=FALSE)

#16cell - sample8
chrX.M.16_8.RPKM <- chrX.M.RPKM$`16.Comp8`[chrX.M.RPKM$`16.Comp8` >= 1]
auto.M.16_8.RPKM <- auto.M.RPKM$`16.Comp8`[auto.M.RPKM$`16.Comp8` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.16_8.RPKM, length(chrX.M.16_8.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.16_8.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[28,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="16C", Gender="Male", stringsAsFactors=FALSE)

#earlyBlast male samples: sample 2,3,6,7,9,11
#earlyBlast - sample2
chrX.M.eB_2.RPKM <- chrX.M.RPKM$`eB.Comp2`[chrX.M.RPKM$`eB.Comp2` >= 1]
auto.M.eB_2.RPKM <- auto.M.RPKM$`eB.Comp2`[auto.M.RPKM$`eB.Comp2` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.eB_2.RPKM, length(chrX.M.eB_2.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.eB_2.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[29,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="eB", Gender="Male", stringsAsFactors=FALSE)

#earlyBlast - sample3
chrX.M.eB_3.RPKM <- chrX.M.RPKM$`eB.Comp3`[chrX.M.RPKM$`eB.Comp3` >= 1]
auto.M.eB_3.RPKM <- auto.M.RPKM$`eB.Comp3`[auto.M.RPKM$`eB.Comp3` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.eB_3.RPKM, length(chrX.M.eB_3.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.eB_3.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[30,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="eB", Gender="Male", stringsAsFactors=FALSE)

#earlyBlast - sample6
chrX.M.eB_6.RPKM <- chrX.M.RPKM$`eB.Comp6`[chrX.M.RPKM$`eB.Comp6` >= 1]
auto.M.eB_6.RPKM <- auto.M.RPKM$`eB.Comp6`[auto.M.RPKM$`eB.Comp6` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.eB_6.RPKM, length(chrX.M.eB_6.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.eB_6.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[31,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="eB", Gender="Male", stringsAsFactors=FALSE)

#earlyBlast - sample7
chrX.M.eB_7.RPKM <- chrX.M.RPKM$`eB.Comp7`[chrX.M.RPKM$`eB.Comp7` >= 1]
auto.M.eB_7.RPKM <- auto.M.RPKM$`eB.Comp7`[auto.M.RPKM$`eB.Comp7` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.eB_7.RPKM, length(chrX.M.eB_7.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.eB_7.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[32,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="eB", Gender="Male", stringsAsFactors=FALSE)

#earlyBlast - sample9
chrX.M.eB_9.RPKM <- chrX.M.RPKM$`eB.Comp9`[chrX.M.RPKM$`eB.Comp9` >= 1]
auto.M.eB_9.RPKM <- auto.M.RPKM$`eB.Comp9`[auto.M.RPKM$`eB.Comp9` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.eB_9.RPKM, length(chrX.M.eB_9.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.eB_9.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[33,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="eB", Gender="Male", stringsAsFactors=FALSE)

#earlyBlast - sample11
chrX.M.eB_11.RPKM <- chrX.M.RPKM$`eB.Comp11`[chrX.M.RPKM$`eB.Comp11` >= 1]
auto.M.eB_11.RPKM <- auto.M.RPKM$`eB.Comp11`[auto.M.RPKM$`eB.Comp11` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.eB_11.RPKM, length(chrX.M.eB_11.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.eB_11.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[34,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="eB", Gender="Male", stringsAsFactors=FALSE)


#clean up and combine Male and Female X/A ratio
rm(list=setdiff(ls(), c("M.XtoA","F.XtoA","GeneInfo",
                        "CM.comp.RPKM","CM.F.comp.RPKM","CM.M.comp.RPKM")))


plot.dat <- rbind(M.XtoA, F.XtoA)
plot.dat$Stage <- factor(plot.dat$Stage, levels = c("l2C","4C","8C","16C","eB"))

library(ggplot2)
ggplot(plot.dat, aes(x = Stage, y = Ratio, fill = Gender)) +
  geom_boxplot(alpha=0.4, outlier.shape = NA) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1, dodge.width = 0.7), 
             aes(fill=Gender), size=2, pch=21) +
  scale_fill_brewer(palette = "Accent") +
  scale_color_brewer(palette = "Accent") +
  theme_bw() +
  labs(y="X:A Ratio") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        axis.title.x=element_blank(),
        #legend.text=element_text(size=12),
        legend.title=element_blank(), 
        legend.key = element_blank(),
        axis.text.x=element_text(size = 12),
        legend.position = "top",
        legend.text=element_text(size = rel(1)), #face="bold"
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line.y = element_line(color="black"),
        axis.line.x = element_line(color="black")) +
  geom_hline(yintercept=1,  colour="grey",linetype="longdash")







#--------------PART2:Right Panel, (CM cross)-------------------
source("Read_data.R")
rm(list=setdiff(ls(), c("GeneInfo","CM.comp.RPKM","CM.F.comp.RPKM", "CM.M.comp.RPKM")))
# get total autosomal gene and chrX gene (both polyA+ and PolyA-)
autoGenes <- rownames(GeneInfo[which(GeneInfo$Chr != "chrX" & GeneInfo$Chr != "chrY"), ])
chrXGenes <- rownames(GeneInfo[which(GeneInfo$Chr == "chrX"), ])

#Female data. Only consider genes with RPKM >=1
auto.F.RPKM <- CM.F.comp.RPKM[autoGenes, ]
chrX.F.RPKM <- CM.F.comp.RPKM[chrXGenes, ]


#late2cell female: sample 1,2,3,5,8
#late2cell - sample1
chrX.F.L2_1.RPKM <- chrX.F.RPKM$L2.Comp1[chrX.F.RPKM$L2.Comp1 >= 1]
auto.F.L2_1.RPKM <- auto.F.RPKM$L2.Comp1[auto.F.RPKM$L2.Comp1 >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.L2_1.RPKM, length(chrX.F.L2_1.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.L2_1.RPKM) / mean(auto.random.F.RPKM)
}
#calculate median X/A ratio
F.XtoA <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Female", stringsAsFactors=FALSE)

#late2cell - sample2
chrX.F.L2_2.RPKM <- chrX.F.RPKM$L2.Comp2[chrX.F.RPKM$L2.Comp2 >= 1]
auto.F.L2_2.RPKM <- auto.F.RPKM$L2.Comp2[auto.F.RPKM$L2.Comp2 >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.L2_2.RPKM, length(chrX.F.L2_2.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.L2_2.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[2,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Female", stringsAsFactors=FALSE)

#late2cell - sample3
chrX.F.L2_3.RPKM <- chrX.F.RPKM$L2.Comp3[chrX.F.RPKM$L2.Comp3 >= 1]
auto.F.L2_3.RPKM <- auto.F.RPKM$L2.Comp3[auto.F.RPKM$L2.Comp3 >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.L2_3.RPKM, length(chrX.F.L2_3.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.L2_3.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[3,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Female", stringsAsFactors=FALSE)

#late2cell - sample5
chrX.F.L2_5.RPKM <- chrX.F.RPKM$L2.Comp5[chrX.F.RPKM$L2.Comp5 >= 1]
auto.F.L2_5.RPKM <- auto.F.RPKM$L2.Comp5[auto.F.RPKM$L2.Comp5 >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.L2_5.RPKM, length(chrX.F.L2_5.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.L2_5.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[4,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Female", stringsAsFactors=FALSE)

#late2cell - sample8
chrX.F.L2_8.RPKM <- chrX.F.RPKM$L2.Comp8[chrX.F.RPKM$L2.Comp8 >= 1]
auto.F.L2_8.RPKM <- auto.F.RPKM$L2.Comp8[auto.F.RPKM$L2.Comp8 >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.L2_8.RPKM, length(chrX.F.L2_8.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.L2_8.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[5,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Female", stringsAsFactors=FALSE)

#4cell, female sample: 1,2,3,4,5
#4cell - sample1
chrX.F.4_1.RPKM <- chrX.F.RPKM$`4.Comp1`[chrX.F.RPKM$`4.Comp1` >= 1]
auto.F.4_1.RPKM <- auto.F.RPKM$`4.Comp1`[auto.F.RPKM$`4.Comp1` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.4_1.RPKM, length(chrX.F.4_1.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.4_1.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[6,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="4C", Gender="Female", stringsAsFactors=FALSE)

#4cell - sample2
chrX.F.4_2.RPKM <- chrX.F.RPKM$`4.Comp2`[chrX.F.RPKM$`4.Comp2` >= 1]
auto.F.4_2.RPKM <- auto.F.RPKM$`4.Comp2`[auto.F.RPKM$`4.Comp2` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.4_2.RPKM, length(chrX.F.4_2.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.4_2.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[7,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="4C", Gender="Female", stringsAsFactors=FALSE)

#4cell - sample3
chrX.F.4_3.RPKM <- chrX.F.RPKM$`4.Comp3`[chrX.F.RPKM$`4.Comp3` >= 1]
auto.F.4_3.RPKM <- auto.F.RPKM$`4.Comp3`[auto.F.RPKM$`4.Comp3` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.4_3.RPKM, length(chrX.F.4_3.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.4_3.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[8,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="4C", Gender="Female", stringsAsFactors=FALSE)

#4cell - sample4
chrX.F.4_4.RPKM <- chrX.F.RPKM$`4.Comp4`[chrX.F.RPKM$`4.Comp4` >= 1]
auto.F.4_4.RPKM <- auto.F.RPKM$`4.Comp4`[auto.F.RPKM$`4.Comp4` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.4_4.RPKM, length(chrX.F.4_4.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.4_4.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[9,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="4C", Gender="Female", stringsAsFactors=FALSE)

#4cell - sample5
chrX.F.4_5.RPKM <- chrX.F.RPKM$`4.Comp5`[chrX.F.RPKM$`4.Comp5` >= 1]
auto.F.4_5.RPKM <- auto.F.RPKM$`4.Comp5`[auto.F.RPKM$`4.Comp5` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.4_5.RPKM, length(chrX.F.4_5.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.4_5.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[10,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="4C", Gender="Female", stringsAsFactors=FALSE)

#8cell female samples: sample 1,2,5,6,7,8,9,10,11
#8cell - sample1
chrX.F.8_1.RPKM <- chrX.F.RPKM$`8.Comp1`[chrX.F.RPKM$`8.Comp1` >= 1]
auto.F.8_1.RPKM <- auto.F.RPKM$`8.Comp1`[auto.F.RPKM$`8.Comp1` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.8_1.RPKM, length(chrX.F.8_1.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.8_1.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[11,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="8C", Gender="Female", stringsAsFactors=FALSE)

#8cell - sample2
chrX.F.8_2.RPKM <- chrX.F.RPKM$`8.Comp2`[chrX.F.RPKM$`8.Comp2` >= 1]
auto.F.8_2.RPKM <- auto.F.RPKM$`8.Comp2`[auto.F.RPKM$`8.Comp2` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.8_2.RPKM, length(chrX.F.8_2.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.8_2.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[12,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="8C", Gender="Female", stringsAsFactors=FALSE)


#8cell - sample5
chrX.F.8_5.RPKM <- chrX.F.RPKM$`8.Comp5`[chrX.F.RPKM$`8.Comp5` >= 1]
auto.F.8_5.RPKM <- auto.F.RPKM$`8.Comp5`[auto.F.RPKM$`8.Comp5` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.8_5.RPKM, length(chrX.F.8_5.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.8_5.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[13,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="8C", Gender="Female", stringsAsFactors=FALSE)

#8cell - sample6
chrX.F.8_6.RPKM <- chrX.F.RPKM$`8.Comp6`[chrX.F.RPKM$`8.Comp6` >= 1]
auto.F.8_6.RPKM <- auto.F.RPKM$`8.Comp6`[auto.F.RPKM$`8.Comp6` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.8_6.RPKM, length(chrX.F.8_6.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.8_6.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[14,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="8C", Gender="Female", stringsAsFactors=FALSE)

#8cell - sample7
chrX.F.8_7.RPKM <- chrX.F.RPKM$`8.Comp7`[chrX.F.RPKM$`8.Comp7` >= 1]
auto.F.8_7.RPKM <- auto.F.RPKM$`8.Comp7`[auto.F.RPKM$`8.Comp7` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.8_7.RPKM, length(chrX.F.8_7.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.8_7.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[15,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="8C", Gender="Female", stringsAsFactors=FALSE)

#8cell - sample8
chrX.F.8_8.RPKM <- chrX.F.RPKM$`8.Comp8`[chrX.F.RPKM$`8.Comp8` >= 1]
auto.F.8_8.RPKM <- auto.F.RPKM$`8.Comp8`[auto.F.RPKM$`8.Comp8` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.8_8.RPKM, length(chrX.F.8_8.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.8_8.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[16,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="8C", Gender="Female", stringsAsFactors=FALSE)

#8cell - sample9
chrX.F.8_9.RPKM <- chrX.F.RPKM$`8.Comp9`[chrX.F.RPKM$`8.Comp9` >= 1]
auto.F.8_9.RPKM <- auto.F.RPKM$`8.Comp9`[auto.F.RPKM$`8.Comp9` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.8_9.RPKM, length(chrX.F.8_9.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.8_9.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[17,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="8C", Gender="Female", stringsAsFactors=FALSE)

#8cell - sample10
chrX.F.8_10.RPKM <- chrX.F.RPKM$`8.Comp10`[chrX.F.RPKM$`8.Comp10` >= 1]
auto.F.8_10.RPKM <- auto.F.RPKM$`8.Comp10`[auto.F.RPKM$`8.Comp10` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.8_10.RPKM, length(chrX.F.8_10.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.8_10.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[18,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="8C", Gender="Female", stringsAsFactors=FALSE)

#8cell - sample11
chrX.F.8_11.RPKM <- chrX.F.RPKM$`8.Comp11`[chrX.F.RPKM$`8.Comp11` >= 1]
auto.F.8_11.RPKM <- auto.F.RPKM$`8.Comp11`[auto.F.RPKM$`8.Comp11` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.8_11.RPKM, length(chrX.F.8_11.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.8_11.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[19,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="8C", Gender="Female", stringsAsFactors=FALSE)

#16cell female samples: sample 1,2,6,9,12,13,14
#16cell - sample1
chrX.F.16_1.RPKM <- chrX.F.RPKM$`16.Comp1`[chrX.F.RPKM$`16.Comp1` >= 1]
auto.F.16_1.RPKM <- auto.F.RPKM$`16.Comp1`[auto.F.RPKM$`16.Comp1` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.16_1.RPKM, length(chrX.F.16_1.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.16_1.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[20,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="16C", Gender="Female", stringsAsFactors=FALSE)

#16cell - sample2
chrX.F.16_2.RPKM <- chrX.F.RPKM$`16.Comp2`[chrX.F.RPKM$`16.Comp2` >= 1]
auto.F.16_2.RPKM <- auto.F.RPKM$`16.Comp2`[auto.F.RPKM$`16.Comp2` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.16_2.RPKM, length(chrX.F.16_2.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.16_2.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[21,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="16C", Gender="Female", stringsAsFactors=FALSE)

#16cell - sample6
chrX.F.16_6.RPKM <- chrX.F.RPKM$`16.Comp6`[chrX.F.RPKM$`16.Comp6` >= 1]
auto.F.16_6.RPKM <- auto.F.RPKM$`16.Comp6`[auto.F.RPKM$`16.Comp6` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.16_6.RPKM, length(chrX.F.16_6.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.16_6.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[22,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="16C", Gender="Female", stringsAsFactors=FALSE)

#16cell - sample9
chrX.F.16_9.RPKM <- chrX.F.RPKM$`16.Comp9`[chrX.F.RPKM$`16.Comp9` >= 1]
auto.F.16_9.RPKM <- auto.F.RPKM$`16.Comp9`[auto.F.RPKM$`16.Comp9` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.16_9.RPKM, length(chrX.F.16_9.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.16_9.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[23,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="16C", Gender="Female", stringsAsFactors=FALSE)

#16cell - sample12
chrX.F.16_12.RPKM <- chrX.F.RPKM$`16.Comp12`[chrX.F.RPKM$`16.Comp12` >= 1]
auto.F.16_12.RPKM <- auto.F.RPKM$`16.Comp12`[auto.F.RPKM$`16.Comp12` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.16_12.RPKM, length(chrX.F.16_12.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.16_12.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[24,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="16C", Gender="Female", stringsAsFactors=FALSE)

#16cell - sample13
chrX.F.16_13.RPKM <- chrX.F.RPKM$`16.Comp13`[chrX.F.RPKM$`16.Comp13` >= 1]
auto.F.16_13.RPKM <- auto.F.RPKM$`16.Comp13`[auto.F.RPKM$`16.Comp13` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.16_13.RPKM, length(chrX.F.16_13.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.16_13.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[25,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="16C", Gender="Female", stringsAsFactors=FALSE)

#16cell - sample14
chrX.F.16_14.RPKM <- chrX.F.RPKM$`16.Comp14`[chrX.F.RPKM$`16.Comp14` >= 1]
auto.F.16_14.RPKM <- auto.F.RPKM$`16.Comp14`[auto.F.RPKM$`16.Comp14` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.16_14.RPKM, length(chrX.F.16_14.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.16_14.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[26,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="16C", Gender="Female", stringsAsFactors=FALSE)

#earlyBlast female samples: sample 3,6,9,11,13
#earlyBlast - sample3
chrX.F.eB_3.RPKM <- chrX.F.RPKM$`eB.Comp3`[chrX.F.RPKM$`eB.Comp3` >= 1]
auto.F.eB_3.RPKM <- auto.F.RPKM$`eB.Comp3`[auto.F.RPKM$`eB.Comp3` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.eB_3.RPKM, length(chrX.F.eB_3.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.eB_3.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[27,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="eB", Gender="Female", stringsAsFactors=FALSE)

#earlyBlast - sample6
chrX.F.eB_6.RPKM <- chrX.F.RPKM$`eB.Comp6`[chrX.F.RPKM$`eB.Comp6` >= 1]
auto.F.eB_6.RPKM <- auto.F.RPKM$`eB.Comp6`[auto.F.RPKM$`eB.Comp6` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.eB_6.RPKM, length(chrX.F.eB_6.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.eB_6.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[28,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="eB", Gender="Female", stringsAsFactors=FALSE)

#earlyBlast - sample9
chrX.F.eB_9.RPKM <- chrX.F.RPKM$`eB.Comp9`[chrX.F.RPKM$`eB.Comp9` >= 1]
auto.F.eB_9.RPKM <- auto.F.RPKM$`eB.Comp9`[auto.F.RPKM$`eB.Comp9` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.eB_9.RPKM, length(chrX.F.eB_9.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.eB_9.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[29,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="eB", Gender="Female", stringsAsFactors=FALSE)

#earlyBlast - sample11
chrX.F.eB_11.RPKM <- chrX.F.RPKM$`eB.Comp11`[chrX.F.RPKM$`eB.Comp11` >= 1]
auto.F.eB_11.RPKM <- auto.F.RPKM$`eB.Comp11`[auto.F.RPKM$`eB.Comp11` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.eB_11.RPKM, length(chrX.F.eB_11.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.eB_11.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[30,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="eB", Gender="Female", stringsAsFactors=FALSE)

#earlyBlast - sample13
chrX.F.eB_13.RPKM <- chrX.F.RPKM$`eB.Comp13`[chrX.F.RPKM$`eB.Comp13` >= 1]
auto.F.eB_13.RPKM <- auto.F.RPKM$`eB.Comp13`[auto.F.RPKM$`eB.Comp13` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.eB_13.RPKM, length(chrX.F.eB_13.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.eB_13.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[31,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="eB", Gender="Female", stringsAsFactors=FALSE)



#Male data second
# get autosomal gene and chrX gene
auto.M.RPKM <- CM.M.comp.RPKM[autoGenes, ]
chrX.M.RPKM <- CM.M.comp.RPKM[chrXGenes, ]

#late2cell male: 4,6,7,9,10
#late2cell - sample4
chrX.M.L2_4.RPKM <- chrX.M.RPKM$L2.Comp4[chrX.M.RPKM$L2.Comp4 >= 1]
auto.M.L2_4.RPKM <- auto.M.RPKM$L2.Comp4[auto.M.RPKM$L2.Comp4 >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.L2_4.RPKM, length(chrX.M.L2_4.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.L2_4.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Male", stringsAsFactors=FALSE)

#late2cell - sample6
chrX.M.L2_6.RPKM <- chrX.M.RPKM$L2.Comp6[chrX.M.RPKM$L2.Comp6 >= 1]
auto.M.L2_6.RPKM <- auto.M.RPKM$L2.Comp6[auto.M.RPKM$L2.Comp6 >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.L2_6.RPKM, length(chrX.M.L2_6.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.L2_6.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[2,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Male", stringsAsFactors=FALSE)

#late2cell - sample7
chrX.M.L2_7.RPKM <- chrX.M.RPKM$L2.Comp7[chrX.M.RPKM$L2.Comp7 >= 1]
auto.M.L2_7.RPKM <- auto.M.RPKM$L2.Comp7[auto.M.RPKM$L2.Comp7 >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.L2_7.RPKM, length(chrX.M.L2_7.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.L2_7.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[3,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Male", stringsAsFactors=FALSE)

#late2cell - sample9
chrX.M.L2_9.RPKM <- chrX.M.RPKM$L2.Comp9[chrX.M.RPKM$L2.Comp9 >= 1]
auto.M.L2_9.RPKM <- auto.M.RPKM$L2.Comp9[auto.M.RPKM$L2.Comp9 >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.L2_9.RPKM, length(chrX.M.L2_9.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.L2_9.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[4,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Male", stringsAsFactors=FALSE)

#late2cell - sample10
chrX.M.L2_10.RPKM <- chrX.M.RPKM$L2.Comp10[chrX.M.RPKM$L2.Comp10 >= 1]
auto.M.L2_10.RPKM <- auto.M.RPKM$L2.Comp10[auto.M.RPKM$L2.Comp10 >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.L2_10.RPKM, length(chrX.M.L2_10.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.L2_10.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[5,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Male", stringsAsFactors=FALSE)

#4cell male embryo: 6,7,8
#4cell - sample6
chrX.M.4_6.RPKM <- chrX.M.RPKM$`4.Comp6`[chrX.M.RPKM$`4.Comp6` >= 1]
auto.M.4_6.RPKM <- auto.M.RPKM$`4.Comp6`[auto.M.RPKM$`4.Comp6` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.4_6.RPKM, length(chrX.M.4_6.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.4_6.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[6,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="4C", Gender="Male", stringsAsFactors=FALSE)

#4cell - sample7
chrX.M.4_7.RPKM <- chrX.M.RPKM$`4.Comp7`[chrX.M.RPKM$`4.Comp7` >= 1]
auto.M.4_7.RPKM <- auto.M.RPKM$`4.Comp7`[auto.M.RPKM$`4.Comp7` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.4_7.RPKM, length(chrX.M.4_7.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.4_7.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[7,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="4C", Gender="Male", stringsAsFactors=FALSE)

#4cell - sample8
chrX.M.4_8.RPKM <- chrX.M.RPKM$`4.Comp8`[chrX.M.RPKM$`4.Comp8` >= 1]
auto.M.4_8.RPKM <- auto.M.RPKM$`4.Comp8`[auto.M.RPKM$`4.Comp8` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.4_8.RPKM, length(chrX.M.4_8.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.4_8.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[8,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="4C", Gender="Male", stringsAsFactors=FALSE)

#8cell male 3,4
#8cell - sample3
chrX.M.8_3.RPKM <- chrX.M.RPKM$`8.Comp3`[chrX.M.RPKM$`8.Comp3` >= 1]
auto.M.8_3.RPKM <- auto.M.RPKM$`8.Comp3`[auto.M.RPKM$`8.Comp3` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.8_3.RPKM, length(chrX.M.8_3.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.8_3.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[9,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="8C", Gender="Male", stringsAsFactors=FALSE)

#8cell - sample4
chrX.M.8_4.RPKM <- chrX.M.RPKM$`8.Comp4`[chrX.M.RPKM$`8.Comp4` >= 1]
auto.M.8_4.RPKM <- auto.M.RPKM$`8.Comp4`[auto.M.RPKM$`8.Comp4` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.8_4.RPKM, length(chrX.M.8_4.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.8_4.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[10,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="8C", Gender="Male", stringsAsFactors=FALSE)


#16cell male 3,4,5,7,8,10,11
#16cell - sample 3
chrX.M.16_3.RPKM <- chrX.M.RPKM$`16.Comp3`[chrX.M.RPKM$`16.Comp3` >= 1]
auto.M.16_3.RPKM <- auto.M.RPKM$`16.Comp3`[auto.M.RPKM$`16.Comp3` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.16_3.RPKM, length(chrX.M.16_3.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.16_3.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[11,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="16C", Gender="Male", stringsAsFactors=FALSE)

#16cell - sample 4
chrX.M.16_4.RPKM <- chrX.M.RPKM$`16.Comp4`[chrX.M.RPKM$`16.Comp4` >= 1]
auto.M.16_4.RPKM <- auto.M.RPKM$`16.Comp4`[auto.M.RPKM$`16.Comp4` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.16_4.RPKM, length(chrX.M.16_4.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.16_4.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[12,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="16C", Gender="Male", stringsAsFactors=FALSE)

#16cell - sample 5
chrX.M.16_5.RPKM <- chrX.M.RPKM$`16.Comp5`[chrX.M.RPKM$`16.Comp5` >= 1]
auto.M.16_5.RPKM <- auto.M.RPKM$`16.Comp5`[auto.M.RPKM$`16.Comp5` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.16_5.RPKM, length(chrX.M.16_5.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.16_5.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[13,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="16C", Gender="Male", stringsAsFactors=FALSE)

#16cell - sample 7
chrX.M.16_7.RPKM <- chrX.M.RPKM$`16.Comp7`[chrX.M.RPKM$`16.Comp7` >= 1]
auto.M.16_7.RPKM <- auto.M.RPKM$`16.Comp7`[auto.M.RPKM$`16.Comp7` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.16_7.RPKM, length(chrX.M.16_7.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.16_7.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[14,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="16C", Gender="Male", stringsAsFactors=FALSE)

#16cell - sample 8
chrX.M.16_8.RPKM <- chrX.M.RPKM$`16.Comp8`[chrX.M.RPKM$`16.Comp8` >= 1]
auto.M.16_8.RPKM <- auto.M.RPKM$`16.Comp8`[auto.M.RPKM$`16.Comp8` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.16_8.RPKM, length(chrX.M.16_8.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.16_8.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[15,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="16C", Gender="Male", stringsAsFactors=FALSE)

#16cell - sample 10
chrX.M.16_10.RPKM <- chrX.M.RPKM$`16.Comp10`[chrX.M.RPKM$`16.Comp10` >= 1]
auto.M.16_10.RPKM <- auto.M.RPKM$`16.Comp10`[auto.M.RPKM$`16.Comp10` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.16_10.RPKM, length(chrX.M.16_10.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.16_10.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[16,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="16C", Gender="Male", stringsAsFactors=FALSE)

#16cell - sample 11
chrX.M.16_11.RPKM <- chrX.M.RPKM$`16.Comp11`[chrX.M.RPKM$`16.Comp11` >= 1]
auto.M.16_11.RPKM <- auto.M.RPKM$`16.Comp11`[auto.M.RPKM$`16.Comp11` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.16_11.RPKM, length(chrX.M.16_11.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.16_11.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[17,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="16C", Gender="Male", stringsAsFactors=FALSE)

#earlyBlast male 1 2 4 5 7 8 10 12
#earlyBlast - sample1
chrX.M.eB_1.RPKM <- chrX.M.RPKM$`eB.Comp1`[chrX.M.RPKM$`eB.Comp1` >= 1]
auto.M.eB_1.RPKM <- auto.M.RPKM$`eB.Comp1`[auto.M.RPKM$`eB.Comp1` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.eB_1.RPKM, length(chrX.M.eB_1.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.eB_1.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[18,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="eB", Gender="Male", stringsAsFactors=FALSE)

#earlyBlast - sample2
chrX.M.eB_2.RPKM <- chrX.M.RPKM$`eB.Comp2`[chrX.M.RPKM$`eB.Comp2` >= 1]
auto.M.eB_2.RPKM <- auto.M.RPKM$`eB.Comp2`[auto.M.RPKM$`eB.Comp2` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.eB_2.RPKM, length(chrX.M.eB_2.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.eB_2.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[19,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="eB", Gender="Male", stringsAsFactors=FALSE)

#earlyBlast - sample4
chrX.M.eB_4.RPKM <- chrX.M.RPKM$`eB.Comp4`[chrX.M.RPKM$`eB.Comp4` >= 1]
auto.M.eB_4.RPKM <- auto.M.RPKM$`eB.Comp4`[auto.M.RPKM$`eB.Comp4` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.eB_4.RPKM, length(chrX.M.eB_4.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.eB_4.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[20,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="eB", Gender="Male", stringsAsFactors=FALSE)

#earlyBlast - sample5
chrX.M.eB_5.RPKM <- chrX.M.RPKM$`eB.Comp5`[chrX.M.RPKM$`eB.Comp5` >= 1]
auto.M.eB_5.RPKM <- auto.M.RPKM$`eB.Comp5`[auto.M.RPKM$`eB.Comp5` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.eB_5.RPKM, length(chrX.M.eB_5.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.eB_5.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[21,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="eB", Gender="Male", stringsAsFactors=FALSE)

#earlyBlast - sample7
chrX.M.eB_7.RPKM <- chrX.M.RPKM$`eB.Comp7`[chrX.M.RPKM$`eB.Comp7` >= 1]
auto.M.eB_7.RPKM <- auto.M.RPKM$`eB.Comp7`[auto.M.RPKM$`eB.Comp7` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.eB_7.RPKM, length(chrX.M.eB_7.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.eB_7.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[22,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="eB", Gender="Male", stringsAsFactors=FALSE)

#earlyBlast - sample8
chrX.M.eB_8.RPKM <- chrX.M.RPKM$`eB.Comp8`[chrX.M.RPKM$`eB.Comp8` >= 1]
auto.M.eB_8.RPKM <- auto.M.RPKM$`eB.Comp8`[auto.M.RPKM$`eB.Comp8` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.eB_8.RPKM, length(chrX.M.eB_8.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.eB_8.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[23,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="eB", Gender="Male", stringsAsFactors=FALSE)

#earlyBlast - sample10
chrX.M.eB_10.RPKM <- chrX.M.RPKM$`eB.Comp10`[chrX.M.RPKM$`eB.Comp10` >= 1]
auto.M.eB_10.RPKM <- auto.M.RPKM$`eB.Comp10`[auto.M.RPKM$`eB.Comp10` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.eB_10.RPKM, length(chrX.M.eB_10.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.eB_10.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[24,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="eB", Gender="Male", stringsAsFactors=FALSE)

#earlyBlast - sample12
chrX.M.eB_12.RPKM <- chrX.M.RPKM$`eB.Comp12`[chrX.M.RPKM$`eB.Comp12` >= 1]
auto.M.eB_12.RPKM <- auto.M.RPKM$`eB.Comp12`[auto.M.RPKM$`eB.Comp12` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.eB_12.RPKM, length(chrX.M.eB_12.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.eB_12.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[25,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="eB", Gender="Male", stringsAsFactors=FALSE)

#In females, remove sample2 at 8C stage and sample2 at 16C, because they significantly deviated from other embryos of the same stage.
F.XtoA <- F.XtoA[c(-12,-21), ]

plot.dat <- rbind(M.XtoA, F.XtoA)
plot.dat$Stage <- factor(plot.dat$Stage, levels = c("l2C","4C","8C","16C","eB"))

library(ggplot2)
ggplot(plot.dat, aes(x = Stage, y = Ratio, fill = Gender)) +
  geom_boxplot(alpha=0.4, outlier.shape = NA) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1, dodge.width = 0.7), 
             aes(fill=Gender), size=2, pch=21) +
  scale_fill_brewer(palette = "Accent") +
  scale_color_brewer(palette = "Accent") +
  theme_bw() +
  labs(y="X:A Ratio") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        axis.title.x=element_blank(),
        #legend.text=element_text(size=12),
        legend.title=element_blank(), 
        legend.key = element_blank(),
        axis.text.x=element_text(size = 12),
        legend.position = "top",
        legend.text=element_text(size = rel(1)), #face="bold"
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line.y = element_line(color="black"),
        axis.line.x = element_line(color="black")) +
  geom_hline(yintercept=1,  colour="grey",linetype="longdash")






#Figure 1g.
#Only use PolyA RNAs.
source("Read_data.R")
rm(list=setdiff(ls(), c("GeneInfo","MC.comp.RPKM","MC.F.comp.RPKM", "MC.M.comp.RPKM")))
# get total autosomal gene and chrX gene (both polyA+ and PolyA-)
autoGenes <- rownames(GeneInfo[which(GeneInfo$Chr != "chrX" & GeneInfo$Chr != "chrY"), ])
chrXGenes <- rownames(GeneInfo[which(GeneInfo$Chr == "chrX"), ])

####### Get the list of  polyA- genes (from published data), and filter them out from this data.
H9_polyA.minus <- read.table(file="Attached_doc/H9_polyA-.txt", header=TRUE, sep="\t")[ ,c(1,8,9,14)]
Hela_polyA.minus <- read.table(file="Attached_doc/Hela_polyA-.txt", header=TRUE, sep="\t")[ ,c(1,8,9,14)]
overlap.gene <- merge(H9_polyA.minus, Hela_polyA.minus, by.x="Proper_Name", by.y="Proper_Name",all.x = T, all.y = T)
overlap.gene <- overlap.gene[-337, ]
rownames(overlap.gene) <- overlap.gene[ ,1]
polyA.minus <- overlap.gene

MC.F.comp.RPKM <- MC.F.comp.RPKM[setdiff(rownames(MC.F.comp.RPKM), rownames(polyA.minus)), ]
MC.M.comp.RPKM <- MC.M.comp.RPKM[setdiff(rownames(MC.M.comp.RPKM), rownames(polyA.minus)), ]

autoGenes <- setdiff(autoGenes, rownames(polyA.minus))
chrXGenes <- setdiff(chrXGenes, rownames(polyA.minus))


#Female data. Only consider genes with RPKM >=1
auto.F.RPKM <- MC.F.comp.RPKM[autoGenes, ]
chrX.F.RPKM <- MC.F.comp.RPKM[chrXGenes, ]


#late2cell female: sample 2,3,4,6,8
#late2cell - sample2
chrX.F.L2_2.RPKM <- chrX.F.RPKM$L2.Comp2[chrX.F.RPKM$L2.Comp2 >= 1]
auto.F.L2_2.RPKM <- auto.F.RPKM$L2.Comp2[auto.F.RPKM$L2.Comp2 >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.L2_2.RPKM, length(chrX.F.L2_2.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.L2_2.RPKM) / mean(auto.random.F.RPKM)
}
#calculate median X/A ratio
F.XtoA <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Female", stringsAsFactors=FALSE)

#late2cell - sample3
chrX.F.L2_3.RPKM <- chrX.F.RPKM$L2.Comp3[chrX.F.RPKM$L2.Comp3 >= 1]
auto.F.L2_3.RPKM <- auto.F.RPKM$L2.Comp3[auto.F.RPKM$L2.Comp3 >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.L2_3.RPKM, length(chrX.F.L2_3.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.L2_3.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[2,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Female", stringsAsFactors=FALSE)

#late2cell - sample4
chrX.F.L2_4.RPKM <- chrX.F.RPKM$L2.Comp4[chrX.F.RPKM$L2.Comp4 >= 1]
auto.F.L2_4.RPKM <- auto.F.RPKM$L2.Comp4[auto.F.RPKM$L2.Comp4 >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.L2_4.RPKM, length(chrX.F.L2_4.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.L2_4.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[3,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Female", stringsAsFactors=FALSE)

#late2cell - sample6
chrX.F.L2_6.RPKM <- chrX.F.RPKM$L2.Comp6[chrX.F.RPKM$L2.Comp6 >= 1]
auto.F.L2_6.RPKM <- auto.F.RPKM$L2.Comp6[auto.F.RPKM$L2.Comp6 >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.L2_6.RPKM, length(chrX.F.L2_6.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.L2_6.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[4,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Female", stringsAsFactors=FALSE)

#late2cell - sample8
chrX.F.L2_8.RPKM <- chrX.F.RPKM$L2.Comp8[chrX.F.RPKM$L2.Comp8 >= 1]
auto.F.L2_8.RPKM <- auto.F.RPKM$L2.Comp8[auto.F.RPKM$L2.Comp8 >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.L2_8.RPKM, length(chrX.F.L2_8.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.L2_8.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[5,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Female", stringsAsFactors=FALSE)

#4ell females: sample 4,6,11,14,15,16
#4cell - sample4
chrX.F.4_4.RPKM <- chrX.F.RPKM$`4.Comp4`[chrX.F.RPKM$`4.Comp4` >= 1]
auto.F.4_4.RPKM <- auto.F.RPKM$`4.Comp4`[auto.F.RPKM$`4.Comp4` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.4_4.RPKM, length(chrX.F.4_4.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.4_4.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[6,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="4C", Gender="Female", stringsAsFactors=FALSE)

#4cell - sample6
chrX.F.4_6.RPKM <- chrX.F.RPKM$`4.Comp6`[chrX.F.RPKM$`4.Comp6` >= 1]
auto.F.4_6.RPKM <- auto.F.RPKM$`4.Comp6`[auto.F.RPKM$`4.Comp6` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.4_6.RPKM, length(chrX.F.4_6.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.4_6.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[7,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="4C", Gender="Female", stringsAsFactors=FALSE)

#4cell - sample11
chrX.F.4_11.RPKM <- chrX.F.RPKM$`4.Comp11`[chrX.F.RPKM$`4.Comp11` >= 1]
auto.F.4_11.RPKM <- auto.F.RPKM$`4.Comp11`[auto.F.RPKM$`4.Comp11` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.4_11.RPKM, length(chrX.F.4_11.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.4_11.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[8,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="4C", Gender="Female", stringsAsFactors=FALSE)

#4cell - sample14
chrX.F.4_14.RPKM <- chrX.F.RPKM$`4.Comp14`[chrX.F.RPKM$`4.Comp14` >= 1]
auto.F.4_14.RPKM <- auto.F.RPKM$`4.Comp14`[auto.F.RPKM$`4.Comp14` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.4_14.RPKM, length(chrX.F.4_14.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.4_14.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[9,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="4C", Gender="Female", stringsAsFactors=FALSE)

#4cell - sample15
chrX.F.4_15.RPKM <- chrX.F.RPKM$`4.Comp15`[chrX.F.RPKM$`4.Comp15` >= 1]
auto.F.4_15.RPKM <- auto.F.RPKM$`4.Comp15`[auto.F.RPKM$`4.Comp15` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.4_15.RPKM, length(chrX.F.4_15.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.4_15.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[10,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="4C", Gender="Female", stringsAsFactors=FALSE)

#4cell - sample16
chrX.F.4_16.RPKM <- chrX.F.RPKM$`4.Comp16`[chrX.F.RPKM$`4.Comp16` >= 1]
auto.F.4_16.RPKM <- auto.F.RPKM$`4.Comp16`[auto.F.RPKM$`4.Comp16` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.4_16.RPKM, length(chrX.F.4_16.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.4_16.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[11,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="4C", Gender="Female", stringsAsFactors=FALSE)

#8cell female samples: sample 1,5,7,10,14,15
#8cell - sample1
chrX.F.8_1.RPKM <- chrX.F.RPKM$`8.Comp1`[chrX.F.RPKM$`8.Comp1` >= 1]
auto.F.8_1.RPKM <- auto.F.RPKM$`8.Comp1`[auto.F.RPKM$`8.Comp1` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.8_1.RPKM, length(chrX.F.8_1.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.8_1.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[12,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="8C", Gender="Female", stringsAsFactors=FALSE)

#8cell - sampler5
chrX.F.8_5.RPKM <- chrX.F.RPKM$`8.Comp5`[chrX.F.RPKM$`8.Comp5` >= 1]
auto.F.8_5.RPKM <- auto.F.RPKM$`8.Comp5`[auto.F.RPKM$`8.Comp5` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.8_5.RPKM, length(chrX.F.8_5.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.8_5.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[13,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="8C", Gender="Female", stringsAsFactors=FALSE)

#8cell - sample7
chrX.F.8_7.RPKM <- chrX.F.RPKM$`8.Comp7`[chrX.F.RPKM$`8.Comp7` >= 1]
auto.F.8_7.RPKM <- auto.F.RPKM$`8.Comp7`[auto.F.RPKM$`8.Comp7` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.8_7.RPKM, length(chrX.F.8_7.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.8_7.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[14,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="8C", Gender="Female", stringsAsFactors=FALSE)

#8cell - sample10
chrX.F.8_10.RPKM <- chrX.F.RPKM$`8.Comp10`[chrX.F.RPKM$`8.Comp10` >= 1]
auto.F.8_10.RPKM <- auto.F.RPKM$`8.Comp10`[auto.F.RPKM$`8.Comp10` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.8_10.RPKM, length(chrX.F.8_10.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.8_10.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[15,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="8C", Gender="Female", stringsAsFactors=FALSE)

#8cell - sample14
chrX.F.8_14.RPKM <- chrX.F.RPKM$`8.Comp14`[chrX.F.RPKM$`8.Comp14` >= 1]
auto.F.8_14.RPKM <- auto.F.RPKM$`8.Comp14`[auto.F.RPKM$`8.Comp14` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.8_14.RPKM, length(chrX.F.8_14.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.8_14.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[16,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="8C", Gender="Female", stringsAsFactors=FALSE)

#8cell - sample15
chrX.F.8_15.RPKM <- chrX.F.RPKM$`8.Comp15`[chrX.F.RPKM$`8.Comp15` >= 1]
auto.F.8_15.RPKM <- auto.F.RPKM$`8.Comp15`[auto.F.RPKM$`8.Comp15` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.8_15.RPKM, length(chrX.F.8_15.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.8_15.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[17,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="8C", Gender="Female", stringsAsFactors=FALSE)

#16cell female samples: sample 2,3,5,6,7
#16cell - sample2
chrX.F.16_2.RPKM <- chrX.F.RPKM$`16.Comp2`[chrX.F.RPKM$`16.Comp2` >= 1]
auto.F.16_2.RPKM <- auto.F.RPKM$`16.Comp2`[auto.F.RPKM$`16.Comp2` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.16_2.RPKM, length(chrX.F.16_2.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.16_2.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[18,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="16C", Gender="Female", stringsAsFactors=FALSE)

#16cell - sample3
chrX.F.16_3.RPKM <- chrX.F.RPKM$`16.Comp3`[chrX.F.RPKM$`16.Comp3` >= 1]
auto.F.16_3.RPKM <- auto.F.RPKM$`16.Comp3`[auto.F.RPKM$`16.Comp3` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.16_3.RPKM, length(chrX.F.16_3.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.16_3.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[19,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="16C", Gender="Female", stringsAsFactors=FALSE)

#16cell - sample5
chrX.F.16_5.RPKM <- chrX.F.RPKM$`16.Comp5`[chrX.F.RPKM$`16.Comp5` >= 1]
auto.F.16_5.RPKM <- auto.F.RPKM$`16.Comp5`[auto.F.RPKM$`16.Comp5` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.16_5.RPKM, length(chrX.F.16_5.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.16_5.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[20,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="16C", Gender="Female", stringsAsFactors=FALSE)

#16cell - sample6
chrX.F.16_6.RPKM <- chrX.F.RPKM$`16.Comp6`[chrX.F.RPKM$`16.Comp6` >= 1]
auto.F.16_6.RPKM <- auto.F.RPKM$`16.Comp6`[auto.F.RPKM$`16.Comp6` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.16_6.RPKM, length(chrX.F.16_6.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.16_6.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[21,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="16C", Gender="Female", stringsAsFactors=FALSE)

#16cell - sample7
chrX.F.16_7.RPKM <- chrX.F.RPKM$`16.Comp7`[chrX.F.RPKM$`16.Comp7` >= 1]
auto.F.16_7.RPKM <- auto.F.RPKM$`16.Comp7`[auto.F.RPKM$`16.Comp7` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.16_7.RPKM, length(chrX.F.16_7.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.16_7.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[22,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="16C", Gender="Female", stringsAsFactors=FALSE)

#earlyBlast female samples: sample 1,4,5,8,10
#earlyBlast - sample1
chrX.F.eB_1.RPKM <- chrX.F.RPKM$eB.Comp1[chrX.F.RPKM$eB.Comp1 >= 1]
auto.F.eB_1.RPKM <- auto.F.RPKM$eB.Comp1[auto.F.RPKM$eB.Comp1 >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.eB_1.RPKM, length(chrX.F.eB_1.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.eB_1.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[23,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="eB", Gender="Female", stringsAsFactors=FALSE)

#earlyBlast - sample4
chrX.F.eB_4.RPKM <- chrX.F.RPKM$eB.Comp4[chrX.F.RPKM$eB.Comp4 >= 1]
auto.F.eB_4.RPKM <- auto.F.RPKM$eB.Comp4[auto.F.RPKM$eB.Comp4 >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.eB_4.RPKM, length(chrX.F.eB_4.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.eB_4.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[24,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="eB", Gender="Female", stringsAsFactors=FALSE)

#earlyBlast - sample5
chrX.F.eB_5.RPKM <- chrX.F.RPKM$eB.Comp5[chrX.F.RPKM$eB.Comp5 >= 1]
auto.F.eB_5.RPKM <- auto.F.RPKM$eB.Comp5[auto.F.RPKM$eB.Comp5 >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.eB_5.RPKM, length(chrX.F.eB_5.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.eB_5.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[25,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="eB", Gender="Female", stringsAsFactors=FALSE)

#earlyBlast - sample8
chrX.F.eB_8.RPKM <- chrX.F.RPKM$eB.Comp8[chrX.F.RPKM$eB.Comp8 >= 1]
auto.F.eB_8.RPKM <- auto.F.RPKM$eB.Comp8[auto.F.RPKM$eB.Comp8 >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.eB_8.RPKM, length(chrX.F.eB_8.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.eB_8.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[26,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="eB", Gender="Female", stringsAsFactors=FALSE)

#earlyBlast - sample10
chrX.F.eB_10.RPKM <- chrX.F.RPKM$eB.Comp10[chrX.F.RPKM$eB.Comp10 >= 1]
auto.F.eB_10.RPKM <- auto.F.RPKM$eB.Comp10[auto.F.RPKM$eB.Comp10 >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.F.RPKM <- sample(auto.F.eB_10.RPKM, length(chrX.F.eB_10.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.F.eB_10.RPKM) / mean(auto.random.F.RPKM)
}
F.XtoA[27,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="eB", Gender="Female", stringsAsFactors=FALSE)


#Male data second
# get autosomal gene and chrX gene
auto.M.RPKM <- MC.M.comp.RPKM[autoGenes, ]
chrX.M.RPKM <- MC.M.comp.RPKM[chrXGenes, ]

#late2cell male samples: sample 1,5,7,9,10
#late2cell - sample1
chrX.M.L2_1.RPKM <- chrX.M.RPKM$L2.Comp1[chrX.M.RPKM$L2.Comp1 >= 1]
auto.M.L2_1.RPKM <- auto.M.RPKM$L2.Comp1[auto.M.RPKM$L2.Comp1 >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.L2_1.RPKM, length(chrX.M.L2_1.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.L2_1.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Male", stringsAsFactors=FALSE)

#late2cell - sample5
chrX.M.L2_5.RPKM <- chrX.M.RPKM$L2.Comp5[chrX.M.RPKM$L2.Comp5 >= 1]
auto.M.L2_5.RPKM <- auto.M.RPKM$L2.Comp5[auto.M.RPKM$L2.Comp5 >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.L2_5.RPKM, length(chrX.M.L2_5.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.L2_5.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[2,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Male", stringsAsFactors=FALSE)

#late2cell - sample7
chrX.M.L2_7.RPKM <- chrX.M.RPKM$L2.Comp7[chrX.M.RPKM$L2.Comp7 >= 1]
auto.M.L2_7.RPKM <- auto.M.RPKM$L2.Comp7[auto.M.RPKM$L2.Comp7 >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.L2_7.RPKM, length(chrX.M.L2_7.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.L2_7.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[3,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Male", stringsAsFactors=FALSE)

#late2cell - sample9
chrX.M.L2_9.RPKM <- chrX.M.RPKM$L2.Comp9[chrX.M.RPKM$L2.Comp9 >= 1]
auto.M.L2_9.RPKM <- auto.M.RPKM$L2.Comp9[auto.M.RPKM$L2.Comp9 >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.L2_9.RPKM, length(chrX.M.L2_9.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.L2_9.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[4,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Male", stringsAsFactors=FALSE)

#late2cell - sample10
chrX.M.L2_10.RPKM <- chrX.M.RPKM$L2.Comp10[chrX.M.RPKM$L2.Comp10 >= 1]
auto.M.L2_10.RPKM <- auto.M.RPKM$L2.Comp10[auto.M.RPKM$L2.Comp10 >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.L2_10.RPKM, length(chrX.M.L2_10.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.L2_10.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[5,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="l2C", Gender="Male", stringsAsFactors=FALSE)

#4cell male samples: sample 1,2,3,5,7,8,9,10,12,13,17
#4cell - sample1
chrX.M.4_1.RPKM <- chrX.M.RPKM$`4.Comp1`[chrX.M.RPKM$`4.Comp1` >= 1]
auto.M.4_1.RPKM <- auto.M.RPKM$`4.Comp1`[auto.M.RPKM$`4.Comp1` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.4_1.RPKM, length(chrX.M.4_1.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.4_1.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[6,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="4C", Gender="Male", stringsAsFactors=FALSE)

#4cell - sample2
chrX.M.4_2.RPKM <- chrX.M.RPKM$`4.Comp2`[chrX.M.RPKM$`4.Comp2` >= 1]
auto.M.4_2.RPKM <- auto.M.RPKM$`4.Comp2`[auto.M.RPKM$`4.Comp2` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.4_2.RPKM, length(chrX.M.4_2.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.4_2.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[7,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="4C", Gender="Male", stringsAsFactors=FALSE)

#4cell - sample3
chrX.M.4_3.RPKM <- chrX.M.RPKM$`4.Comp3`[chrX.M.RPKM$`4.Comp3` >= 1]
auto.M.4_3.RPKM <- auto.M.RPKM$`4.Comp3`[auto.M.RPKM$`4.Comp3` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.4_3.RPKM, length(chrX.M.4_3.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.4_3.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[8,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="4C", Gender="Male", stringsAsFactors=FALSE)

#4cell - sample5
chrX.M.4_5.RPKM <- chrX.M.RPKM$`4.Comp5`[chrX.M.RPKM$`4.Comp5` >= 1]
auto.M.4_5.RPKM <- auto.M.RPKM$`4.Comp5`[auto.M.RPKM$`4.Comp5` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.4_5.RPKM, length(chrX.M.4_5.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.4_5.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[9,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="4C", Gender="Male", stringsAsFactors=FALSE)

#4cell - sample7
chrX.M.4_7.RPKM <- chrX.M.RPKM$`4.Comp7`[chrX.M.RPKM$`4.Comp7` >= 1]
auto.M.4_7.RPKM <- auto.M.RPKM$`4.Comp7`[auto.M.RPKM$`4.Comp7` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.4_7.RPKM, length(chrX.M.4_7.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.4_7.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[10,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="4C", Gender="Male", stringsAsFactors=FALSE)

#4cell - sample8
chrX.M.4_8.RPKM <- chrX.M.RPKM$`4.Comp8`[chrX.M.RPKM$`4.Comp8` >= 1]
auto.M.4_8.RPKM <- auto.M.RPKM$`4.Comp8`[auto.M.RPKM$`4.Comp8` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.4_8.RPKM, length(chrX.M.4_8.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.4_8.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[11,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="4C", Gender="Male", stringsAsFactors=FALSE)

#4cell - sample9
chrX.M.4_9.RPKM <- chrX.M.RPKM$`4.Comp9`[chrX.M.RPKM$`4.Comp9` >= 1]
auto.M.4_9.RPKM <- auto.M.RPKM$`4.Comp9`[auto.M.RPKM$`4.Comp9` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.4_9.RPKM, length(chrX.M.4_9.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.4_9.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[12,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="4C", Gender="Male", stringsAsFactors=FALSE)

#4cell - sample10
chrX.M.4_10.RPKM <- chrX.M.RPKM$`4.Comp10`[chrX.M.RPKM$`4.Comp10` >= 1]
auto.M.4_10.RPKM <- auto.M.RPKM$`4.Comp10`[auto.M.RPKM$`4.Comp10` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.4_10.RPKM, length(chrX.M.4_10.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.4_10.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[13,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="4C", Gender="Male", stringsAsFactors=FALSE)

#4cell - sample12
chrX.M.4_12.RPKM <- chrX.M.RPKM$`4.Comp12`[chrX.M.RPKM$`4.Comp12` >= 1]
auto.M.4_12.RPKM <- auto.M.RPKM$`4.Comp12`[auto.M.RPKM$`4.Comp12` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.4_12.RPKM, length(chrX.M.4_12.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.4_12.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[14,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="4C", Gender="Male", stringsAsFactors=FALSE)

#4cell - sample13
chrX.M.4_13.RPKM <- chrX.M.RPKM$`4.Comp13`[chrX.M.RPKM$`4.Comp13` >= 1]
auto.M.4_13.RPKM <- auto.M.RPKM$`4.Comp13`[auto.M.RPKM$`4.Comp13` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.4_13.RPKM, length(chrX.M.4_13.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.4_13.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[15,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="4C", Gender="Male", stringsAsFactors=FALSE)

#4cell - sample17
chrX.M.4_17.RPKM <- chrX.M.RPKM$`4.Comp17`[chrX.M.RPKM$`4.Comp17` >= 1]
auto.M.4_17.RPKM <- auto.M.RPKM$`4.Comp17`[auto.M.RPKM$`4.Comp17` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.4_17.RPKM, length(chrX.M.4_17.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.4_17.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[16,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="4C", Gender="Male", stringsAsFactors=FALSE)

#8cell male samples: sample 2,3,4,6,8,9,11,12,13
#8cell - sample2
chrX.M.8_2.RPKM <- chrX.M.RPKM$`8.Comp2`[chrX.M.RPKM$`8.Comp2` >= 1]
auto.M.8_2.RPKM <- auto.M.RPKM$`8.Comp2`[auto.M.RPKM$`8.Comp2` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.8_2.RPKM, length(chrX.M.8_2.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.8_2.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[17,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="8C", Gender="Male", stringsAsFactors=FALSE)

#8cell - sample3
chrX.M.8_3.RPKM <- chrX.M.RPKM$`8.Comp3`[chrX.M.RPKM$`8.Comp3` >= 1]
auto.M.8_3.RPKM <- auto.M.RPKM$`8.Comp3`[auto.M.RPKM$`8.Comp3` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.8_3.RPKM, length(chrX.M.8_3.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.8_3.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[18,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="8C", Gender="Male", stringsAsFactors=FALSE)

#8cell - sample4
chrX.M.8_4.RPKM <- chrX.M.RPKM$`8.Comp4`[chrX.M.RPKM$`8.Comp4` >= 1]
auto.M.8_4.RPKM <- auto.M.RPKM$`8.Comp4`[auto.M.RPKM$`8.Comp4` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.8_4.RPKM, length(chrX.M.8_4.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.8_4.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[19,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="8C", Gender="Male", stringsAsFactors=FALSE)

#8cell - sample6
chrX.M.8_6.RPKM <- chrX.M.RPKM$`8.Comp6`[chrX.M.RPKM$`8.Comp6` >= 1]
auto.M.8_6.RPKM <- auto.M.RPKM$`8.Comp6`[auto.M.RPKM$`8.Comp6` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.8_6.RPKM, length(chrX.M.8_6.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.8_6.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[20,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="8C", Gender="Male", stringsAsFactors=FALSE)

#8cell - sample8
chrX.M.8_8.RPKM <- chrX.M.RPKM$`8.Comp8`[chrX.M.RPKM$`8.Comp8` >= 1]
auto.M.8_8.RPKM <- auto.M.RPKM$`8.Comp8`[auto.M.RPKM$`8.Comp8` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.8_8.RPKM, length(chrX.M.8_8.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.8_8.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[21,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="8C", Gender="Male", stringsAsFactors=FALSE)

#8cell - sample9
chrX.M.8_9.RPKM <- chrX.M.RPKM$`8.Comp9`[chrX.M.RPKM$`8.Comp9` >= 1]
auto.M.8_9.RPKM <- auto.M.RPKM$`8.Comp9`[auto.M.RPKM$`8.Comp9` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.8_9.RPKM, length(chrX.M.8_9.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.8_9.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[22,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="8C", Gender="Male", stringsAsFactors=FALSE)

#8cell - sample11
chrX.M.8_11.RPKM <- chrX.M.RPKM$`8.Comp11`[chrX.M.RPKM$`8.Comp11` >= 1]
auto.M.8_11.RPKM <- auto.M.RPKM$`8.Comp11`[auto.M.RPKM$`8.Comp11` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.8_11.RPKM, length(chrX.M.8_11.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.8_11.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[23,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="8C", Gender="Male", stringsAsFactors=FALSE)

#8cell - sample12
chrX.M.8_12.RPKM <- chrX.M.RPKM$`8.Comp12`[chrX.M.RPKM$`8.Comp12` >= 1]
auto.M.8_12.RPKM <- auto.M.RPKM$`8.Comp12`[auto.M.RPKM$`8.Comp12` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.8_12.RPKM, length(chrX.M.8_12.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.8_12.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[24,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="8C", Gender="Male", stringsAsFactors=FALSE)

#8cell - sample13
chrX.M.8_13.RPKM <- chrX.M.RPKM$`8.Comp13`[chrX.M.RPKM$`8.Comp13` >= 1]
auto.M.8_13.RPKM <- auto.M.RPKM$`8.Comp13`[auto.M.RPKM$`8.Comp13` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.8_13.RPKM, length(chrX.M.8_13.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.8_13.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[25,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="8C", Gender="Male", stringsAsFactors=FALSE)

#16cell male samples: sample 1,4,8
#16cell - sample1
chrX.M.16_1.RPKM <- chrX.M.RPKM$`16.Comp1`[chrX.M.RPKM$`16.Comp1` >= 1]
auto.M.16_1.RPKM <- auto.M.RPKM$`16.Comp1`[auto.M.RPKM$`16.Comp1` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.16_1.RPKM, length(chrX.M.16_1.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.16_1.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[26,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="16C", Gender="Male", stringsAsFactors=FALSE)

#16cell - sample4
chrX.M.16_4.RPKM <- chrX.M.RPKM$`16.Comp4`[chrX.M.RPKM$`16.Comp4` >= 1]
auto.M.16_4.RPKM <- auto.M.RPKM$`16.Comp4`[auto.M.RPKM$`16.Comp4` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.16_4.RPKM, length(chrX.M.16_4.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.16_4.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[27,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="16C", Gender="Male", stringsAsFactors=FALSE)

#16cell - sample8
chrX.M.16_8.RPKM <- chrX.M.RPKM$`16.Comp8`[chrX.M.RPKM$`16.Comp8` >= 1]
auto.M.16_8.RPKM <- auto.M.RPKM$`16.Comp8`[auto.M.RPKM$`16.Comp8` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.16_8.RPKM, length(chrX.M.16_8.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.16_8.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[28,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="16C", Gender="Male", stringsAsFactors=FALSE)

#earlyBlast male samples: sample 2,3,6,7,9,11
#earlyBlast - sample2
chrX.M.eB_2.RPKM <- chrX.M.RPKM$`eB.Comp2`[chrX.M.RPKM$`eB.Comp2` >= 1]
auto.M.eB_2.RPKM <- auto.M.RPKM$`eB.Comp2`[auto.M.RPKM$`eB.Comp2` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.eB_2.RPKM, length(chrX.M.eB_2.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.eB_2.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[29,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="eB", Gender="Male", stringsAsFactors=FALSE)

#earlyBlast - sample3
chrX.M.eB_3.RPKM <- chrX.M.RPKM$`eB.Comp3`[chrX.M.RPKM$`eB.Comp3` >= 1]
auto.M.eB_3.RPKM <- auto.M.RPKM$`eB.Comp3`[auto.M.RPKM$`eB.Comp3` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.eB_3.RPKM, length(chrX.M.eB_3.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.eB_3.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[30,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="eB", Gender="Male", stringsAsFactors=FALSE)

#earlyBlast - sample6
chrX.M.eB_6.RPKM <- chrX.M.RPKM$`eB.Comp6`[chrX.M.RPKM$`eB.Comp6` >= 1]
auto.M.eB_6.RPKM <- auto.M.RPKM$`eB.Comp6`[auto.M.RPKM$`eB.Comp6` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.eB_6.RPKM, length(chrX.M.eB_6.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.eB_6.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[31,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="eB", Gender="Male", stringsAsFactors=FALSE)

#earlyBlast - sample7
chrX.M.eB_7.RPKM <- chrX.M.RPKM$`eB.Comp7`[chrX.M.RPKM$`eB.Comp7` >= 1]
auto.M.eB_7.RPKM <- auto.M.RPKM$`eB.Comp7`[auto.M.RPKM$`eB.Comp7` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.eB_7.RPKM, length(chrX.M.eB_7.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.eB_7.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[32,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="eB", Gender="Male", stringsAsFactors=FALSE)

#earlyBlast - sample9
chrX.M.eB_9.RPKM <- chrX.M.RPKM$`eB.Comp9`[chrX.M.RPKM$`eB.Comp9` >= 1]
auto.M.eB_9.RPKM <- auto.M.RPKM$`eB.Comp9`[auto.M.RPKM$`eB.Comp9` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.eB_9.RPKM, length(chrX.M.eB_9.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.eB_9.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[33,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="eB", Gender="Male", stringsAsFactors=FALSE)

#earlyBlast - sample11
chrX.M.eB_11.RPKM <- chrX.M.RPKM$`eB.Comp11`[chrX.M.RPKM$`eB.Comp11` >= 1]
auto.M.eB_11.RPKM <- auto.M.RPKM$`eB.Comp11`[auto.M.RPKM$`eB.Comp11` >= 1]
#for autogene, randomly select equal number of genes and calculate the X/A ratio. repeat 1000 times and then take the median value.
Random.XtoA.ratio <- vector("numeric", 1000L)
for(i in 1:1000) {
  auto.random.M.RPKM <- sample(auto.M.eB_11.RPKM, length(chrX.M.eB_11.RPKM))
  Random.XtoA.ratio[i] <- mean(chrX.M.eB_11.RPKM) / mean(auto.random.M.RPKM)
}
M.XtoA[34,] <- data.frame(Ratio=median(Random.XtoA.ratio), Stage="eB", Gender="Male", stringsAsFactors=FALSE)


#clean up and combine Male and Female X/A ratio
rm(list=setdiff(ls(), c("M.XtoA","F.XtoA","GeneInfo",
                        "CM.comp.RPKM","CM.F.comp.RPKM","CM.M.comp.RPKM")))


plot.dat <- rbind(M.XtoA, F.XtoA)
plot.dat$Stage <- factor(plot.dat$Stage, levels = c("l2C","4C","8C","16C","eB"))

library(ggplot2)
ggplot(plot.dat, aes(x = Stage, y = Ratio, fill = Gender)) +
  geom_boxplot(alpha=0.4, outlier.shape = NA) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1, dodge.width = 0.7), 
             aes(fill=Gender), size=2, pch=21) +
  scale_fill_brewer(palette = "Accent") +
  scale_color_brewer(palette = "Accent") +
  theme_bw() +
  labs(y="X:A Ratio") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        axis.title.x=element_blank(),
        #legend.text=element_text(size=12),
        legend.title=element_blank(), 
        legend.key = element_blank(),
        axis.text.x=element_text(size = 12),
        legend.position = "top",
        legend.text=element_text(size = rel(1)), #face="bold"
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line.y = element_line(color="black"),
        axis.line.x = element_line(color="black")) +
  geom_hline(yintercept=1,  colour="grey",linetype="longdash")

