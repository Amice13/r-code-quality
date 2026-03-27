############################# APPENDIX: NYC OUTPUT ###########################

# FRONT MATTER
## Clean up
#rm(list=ls())

# PRELIMINARIES
## Libraries
library(krige); library(xtable)

## Load results
load("results/nyc.out.Rdata")

# Output
## Table A.2.8
nyc.tab <- summary(nyc.fit1)$estimates[,c(1,5,8)]
row.names(nyc.tab) <- c("$\\tau^2$", "$\\phi$", "$\\sigma^2$", "Intercept", "Age", 
                        "Education (six categories)", "Age$\\times$education", "African-American",
                        "Nonwhite, nonblack",  "Female", "African-American female", 
                        "Nonwhite, nonblack female", "Catholic or Orthodox", "Evangelical or Mormon",
                        "Jewish or Muslim", "Mainline", "Ruralism (nine categories)", 
                        "Homeowner", "Unemployed", "Not in workforce", "Income (14 categories)")
colnames(nyc.tab) <- c("Estimate", "5\\%", "95\\%")
print(xtable(nyc.tab, digits = 5),
      include.colnames = TRUE, sanitize.text.function=function(x){x},
      file = "output/tab/nyc-results.txt")

writeLines(readLines("output/tab/nyc-results.txt")[9:29], 
           "output/tab/nyc-results.txt")

## Figure A.2.5
pdf("output/fig/semivariogram_nyc.pdf", width = 7, height = 5.5)
semivariogram(nyc.fit1)
dev.off()

## Table A.2.9
geweke.out <- geweke(nyc.fit1)
hw.out <- heidel.welch(nyc.fit1)
diag.tab <- t(rbind(geweke.out, hw.out))
colnames(diag.tab)[c(1,3)] <- c("Geweke", "Cram\\'{e}r-von Mises")
rownames(diag.tab) <- rownames(nyc.tab)
print(xtable(diag.tab, caption = "\\label{nycGeweke}Convergence diagnostics for 
    New York State ideology. First a Geweke diagnostic, and second the 
    Cram\\'{e}r-von Mises test of the Heidelberger-Welch diagnostic. Statistics 
    computed with \\pkg{krige} 0.6.1 in R 4.0.2."), size = "small", digits = 4, 
    sanitize.text.function=function(x){x}, file = "output/tab/nyc-diag.txt")

## Table A.2.10
row.names(nyc.pred) <- c("Bill DeBlasio", "Melania Trump", "Spike Lee")

print(xtable(nyc.pred, digits = 5), file = "output/tab/nyc-pred.txt")
writeLines(readLines("output/tab/nyc-pred.txt")[9:11], 
           "output/tab/nyc-pred.txt")