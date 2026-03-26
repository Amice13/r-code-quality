tab_w_na <- function(Distribution, add_relative_row=T, horizontal=T, NAs="always", ...) {
  freqtab <- table(Distribution, useNA=NAs, ...)
  if(add_relative_row==T){
    reltab <- round(freqtab/sum(freqtab),3)
    freqtab <- rbind(freqtab, reltab)
    rownames(freqtab) <- c("Absolute", "Relative")
  }
  if(horizontal==T){
    freqtab <- t(freqtab)
  }
  return(freqtab)
}