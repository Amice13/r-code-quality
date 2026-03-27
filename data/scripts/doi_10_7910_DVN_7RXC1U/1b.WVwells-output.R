########################### APPENDIX: WV WELLS OUTPUT ##########################

# FRONT MATTER
## Clean up
#rm(list=ls())

# PRELIMINARIES
## Libraries
library(krige); library(xtable)

## Load results
load("results/wv.out.Rdata")

# Output
## Table 3
wv.tab <- summary(wv.fit1)$estimates[,c(1,5,8)]
row.names(wv.tab) <- c("$\\tau^2$", "$\\phi$", "$\\sigma^2$", "Log elevation", 
                       "Rock presssure")
colnames(wv.tab) <- c("Estimate", "5\\%", "95\\%")
print(xtable(wv.tab, digits = 5),
      include.colnames = TRUE, sanitize.text.function=function(x){x},
      file = "output/tab/wvwells-results.txt")

writeLines(readLines("output/tab/wvwells-results.txt")[9:13], 
           "output/tab/wvwells-results.txt")

## Figure 2
pdf("output/fig/semivariogram_wvwells.pdf", width = 7, height = 5.5)
semivariogram(wv.fit1)
dev.off()

## Table 4
geweke.out <- geweke(wv.fit1)
hw.out <- heidel.welch(wv.fit1)
diag.tab <- t(rbind(geweke.out, hw.out))
colnames(diag.tab)[c(1,3)] <- c("Geweke", "Cram\\'{e}r-von Mises")
rownames(diag.tab) <- rownames(wv.tab)
print(xtable(diag.tab, caption = "\\label{wvGeweke} Convergence diagnostics for 
     West Virginia gas. First a Geweke diagnostic, and second the Cram\\'{e}r-von
     Mises test of the Heidelberger-Welch diagnostic. Statistics computed with 
     \\pkg{krige} 0.6.1 in R 4.0.2."), size = "small", digits = 4, 
      sanitize.text.function=function(x){x}, file = "output/tab/wvwells-diag.txt")

## Table 5
row.names(wv.pred) <- "Putnam Co."
print(xtable(wv.pred, digits = 0), file = "output/tab/wvwells-pred.txt")
writeLines(readLines("output/tab/wvwells-pred.txt")[9], 
           "output/tab/wvwells-pred.txt")