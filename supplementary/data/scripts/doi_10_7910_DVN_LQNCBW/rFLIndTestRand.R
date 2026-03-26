rFLIndTestRand <- function(lcdat1, lcdat2, NR) {

rFLObs <- rFLCorrCoeff(lcdat1, lcdat2) ; nxtrm <- 1

for (r in 1:NR) {

lcdat1Rand <- sample(lcdat1) ; rFLRand <- rFLCorrCoeff(lcdat1Rand, lcdat2)

if (abs(rFLRand) >= abs(rFLObs)) { nxtrm <- nxtrm + 1} }

pval <- nxtrm/(NR+1) ; return(c(rFLObs, pval))

}