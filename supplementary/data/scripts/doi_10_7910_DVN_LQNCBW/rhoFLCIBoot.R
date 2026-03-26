
rhoFLCIBoot <- function(lcdat1, lcdat2, ConfLevel, B) {

alpha <- (100-ConfLevel)/100 ; n <- length(lcdat1) ; rFL <- 0 

for (b in 1:B) {

randind <- sample(1:n, n, replace=T) 

boot1 <- lcdat1[randind] ; boot2 <- lcdat2[randind] 

rFL[b] <- rFLCorrCoeff(boot1, boot2) }

rFL[B+1] <-  rFLCorrCoeff(lcdat1, lcdat2) ; rFLsort <- sort(rFL) 

return (c(rFLsort[(alpha/2)*(B+1)], rFLsort[(1-alpha/2)*(B+1)]))

}
