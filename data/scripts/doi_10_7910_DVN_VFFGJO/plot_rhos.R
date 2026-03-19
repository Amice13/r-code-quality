plot_rhos <- function(stanfit, unconstrained = FALSE){
  rhos <- extract(stanfit, pars = "rho")$rho
  rho_quants <- apply(rhos, 2, quantile, probs = c(0.025, 0.5, 0.975))
  
  yrlab <- c("1958", "1960", "1964", "1968", "1970", "1972", "1974",
             "1976", "1978", "1980", "1982", "1984", "1986", "1988", "1990",
             "1992", "1994", "1996", "1998", "2000")
  
  tt <- 1:ncol(rho_quants)
  if (unconstrained == TRUE) {
    y_lim <- range(rho_quants)
  } else {
    y_lim <- c(0,1)
  }
  plot(tt, rho_quants["50%",], ylim = y_lim, ylab = expression(rho), xaxt = "n", pch  = 19, cex = 0.8, xlab = "Year")
  axis(1, at=tt, label=yrlab)
  abline(h = 1, col = "lightgray", lty = 2)
  segments(tt, rho_quants["2.5%",], tt, rho_quants["97.5%",])
}