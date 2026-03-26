#####
# Supplementary code for submission to The Plant Journal
# Performs diallel analysis (Model IV) and estimates  
# variance components and heritability for a half-diallel
# composed of carrot inbred lines
####
library(here)

source("diallel4.R")
source("heritability.R")

avg.phenos <- read.csv(file = "diallel_pheno.csv", as.is = T, check.names = F, header = T)

results <- data.frame(matrix(NA, nrow = 12, ncol = 11))
rownames(results) <- c("MS.GCA", "v.GCA", "Va", "MS.SCA", "v.SCA",  "Vd", "Ve", "H2", "h2", "br", "p-GCA", "p-SCA")

for(i in 4:(ncol(avg.phenos)-1)){
  pheno <- colnames(avg.phenos)[i]
  diallel <-diallelIV(avg.phenos, yvar = pheno, progeny = "Genotype", male = "male", female = "female", rep = "Rep")
  metrics <- heritability(diallelObj = diallel, t = 1, r = 2, p = 8, F.inbr = 1)  
  colnames(results)[i-3] <- pheno
  results[,(i-3)] <- t(metrics)
}
