JSTestRand <- function(cdat1, cdat2, NR) {

CorrJSObs <- cor.circular(cdat1, cdat2) ; nxtrm <- 1

for (r in 1:NR) {

cdat1Rand <- sample(cdat1) ; CorrJSRand <- cor.circular(cdat1Rand, cdat2)

if (abs(CorrJSRand) >= abs(CorrJSObs)) { nxtrm <- nxtrm + 1} }

pval <- nxtrm/(NR+1) ; return(c(CorrJSObs, pval))

}
