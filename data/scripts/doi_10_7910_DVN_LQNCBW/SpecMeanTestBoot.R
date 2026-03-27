
SpecMeanTestBoot <- function(origdat, mu0, indsym, B) {

n <- length(origdat)

testres <- SpecMeanTestRes(origdat, indsym, mu0)

z <- testres[[1]] ; mubc <- testres[[2]]



shiftdat <- origdat-mubc+mu0



if (indsym == 1) { 

refdat <- 2*mu0-shiftdat ; sampledat <- c(shiftdat, refdat)

} else

if (indsym == 0) { sampledat <- shiftdat }



nxtrm <- 1

for (b in 2:(B+1)) { 

bootdat <- sample(sampledat, size=n, replace=TRUE)

testres <- SpecMeanTestRes(bootdat, indsym, mu0)

z[b] <- testres[[1]]

if (z[b] >= z[1]) { nxtrm <- nxtrm + 1 }

}



pval <- nxtrm/(B+1)

return(pval)

}