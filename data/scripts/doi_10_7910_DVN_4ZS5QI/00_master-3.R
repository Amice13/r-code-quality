
### PACKAGES

library("nleqslv")
library("data.table")



### PARAMETER VALUES

rm(list=ls())
gc()

#Number of periods
T0 <- 1      #initialization
T <- 30      #investment horizon
T1 <- T0+T   #total number of periods

#Number of simulations
Nsimu <- 1e5

#Average asset return
r <- 0.044

#Volatility asset return
sigx <- 0.044

#Risk-free rate
rf <- 0.03

#Insurer profit share
phi <- .15

#Demand parameters
sj <- 1
alpha <- 1e-6
theta <- 0    # theta>0 in the model extension with inflows and outflows (appendix D)
beta <- alpha/(1-theta)

#RRA
gamma <- 2



### SIMULATE THE MODEL

# Output:
#y1: contract return in baseline model
#y2: contract return in extension with separate inflow-outflow

source("01_simu.R")

# Calculate optimal portfolios and print stats for the baseline model

y <- y1
H <- 12 # 12 years is the average holding period in our sample
source("02_portfolios.R")

# Panel A of Table 11
writeLines( c(stat_asset, stat_contract, stat_optimalwithout, stat_optimalwith), "../output/welfare_A.tex")



### CERTAINTY EQUIVALENT GAIN AT DIFFERENT HORIZONS

Hvector <- c(1,5,12,20,30) # investment horizons used in Table 12
Hvector_length <- length(Hvector)
PrintCE <- rep(0,Hvector_length)

for (i in 1:Hvector_length) {
  H <- Hvector[i]
  print(paste("Simulate model horizon",H,"years"))
  # Calculate portfolio returns
  source("02_portfolios.R")
  # Save in the vector the following summary stats:
  PrintCE[i] <- paste(
    H, " years & ",
    # Ratio S.D. contract return / S.D. asset return
    round( sd(wym[,H+1])/sd(wxm[,H+1]), 2), " & ",
    # Certainty equivalent gain of annualized contract return relative to asset
    round(CEgainxyperyear,4)*100, " & ",
    # Certainty equivalent gain of annualized optimal portfolio return with contract relative to without contract
    round(CEgainperyear,4)*100,
    sep = ""
  )
}

# Save stats in .tex file

writeLines( c(PrintCE[1], PrintCE[2], PrintCE[3], PrintCE[4], PrintCE[5]), "../output/welfare-horizon.tex")     



### LIQUIDITY RISK

# H has geometric distribution censored at 30, with mean 12 years
# solve for the censored geometric distribution such that the mean is 12

rm(H)

fcn <- function(lambda) {
  pdf <- lambda*(1-lambda)^(0:29)
  pdf[30] <- 1-sum(pdf[1:29])
  return ( sum(pdf*(1:30)) - 12 )
}

lambdaH <- nleqslv ( 1/12 , fcn ) $x
pdfH <- lambdaH*(1-lambdaH)^(0:29)
pdfH[30] <- 1-sum(pdfH[1:29])

source("03_portfolios_liq.R")
share1
share2
CEgainxyperyear
CEgainperyear

#Save stats in .tex file

writeLines( c(
  "Geometric with \\\\",
  paste(
    "mean 12 years & & ",
    # Certainty equivalent gain of annualized contract return relative to asset
    round(CEgainxyperyear,4)*100, " & ",
    # Certainty equivalent gain of annualized optimal portfolio return with contract relative to without contract
    round(CEgainperyear,4)*100,
    sep = ""
  )
), "../output/welfare-horizon-liq.tex")




### VARY PARAMETER VALUES

# Panel B of Table 11
gamma <- 5
source("01_simu.R")
y <- y1
H <- 12
source("02_portfolios.R")
writeLines( c(stat_asset, stat_contract, stat_optimalwithout, stat_optimalwith), "../output/welfare_B.tex")

# Panel C of Table 11
gamma <- 2
alpha <- 5
source("01_simu.R")
y <- y1
H <- 12
source("02_portfolios.R")
writeLines( c(stat_asset, stat_contract, stat_optimalwithout, stat_optimalwith), "../output/welfare_C.tex")

