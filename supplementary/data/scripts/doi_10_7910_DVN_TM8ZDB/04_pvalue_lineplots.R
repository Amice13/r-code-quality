setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../output/results")

library(tidyverse)
library(equivalenceTest)

## Load the p-val RDatas
f = list.files()
f = f[grepl("results2.RData", f)]
statelab = str_extract(f, pattern = ".*(?=_)")

#dat = matrix(ncol=77)

out = list()
for(i in 1:length(f)){
  load(f[i])
  out[[i]] = results
  print(i)
}
dat = as.data.frame(do.call(rbind, out))

rownames(dat) = statelab
dat$statecode = statelab
dat$state = substr(dat$statecode,1,2)
rm(results, f, statelab, i, out)

write.csv(dat, file="results_29oct2023.csv")


## First get the pvals
dat2 = dat
dat2$nsims = 50000
dat2$nsims[dat2$state == "va"] = 100000
dat2$pval = dat2$fewerfirms/dat2$nsims
dat2$pval_cond = dat2$fewerfirms_sameseats/dat2$sameseats
dat2$pval_seats = dat2$fewerseats/dat2$nsims
dat2$pval_donors = dat2$donors200k_samefewer/dat2$nsims
dat2$pval_donors_cond = dat2$donors200k_samefewer_sameseats/dat2$sameseats
dat2$pval_dollarsw = dat2$dollars_weighted_samefewer/dat2$nsims
dat2$pval_dollarsw_cond = dat2$dollars_weighted_samefewer_sameseats/dat2$sameseats

## Donors robustness
dat2$pval_donorsleft = dat2$donorsleft_samefewer/dat2$nsims
dat2$pval_donorsleft_cond = dat2$donorsleft_samefewer_sameseats/dat2$sameseats
dat2$pval_donorsright = dat2$donorsright_samefewer/dat2$nsims
dat2$pval_donorsright_cond = dat2$donorsright_samefewer_sameseats/dat2$sameseats
dat2$pval_donorsswing = dat2$donorsswing_samefewer/dat2$nsims
dat2$pval_donorsswing_cond = dat2$donorsswing_samefewer_sameseats/dat2$sameseats

## Confounders
dat2$pval_medinc = dat2$medincome_samefewer/dat2$nsims
dat2$pval_medinc_cond = dat2$medincome_samefewer_sameseats/dat2$sameseats
dat2$pval_topinc = dat2$topincome_samefewer/dat2$nsims
dat2$pval_topinc_cond = dat2$topincome_samefewer_sameseats/dat2$sameseats
dat2$pval_urban = dat2$urban_samefewer/dat2$nsims
dat2$pval_urban_cond = dat2$urban_samefewer_sameseats/dat2$sameseats
dat2$pval_whitepct = dat2$whitepct_samefewer/dat2$nsims
dat2$pval_whitepct_cond = dat2$whitepct_samefewer_sameseats/dat2$sameseats
dat2$pval_blackpct = dat2$blackpct_samefewer/dat2$nsims
dat2$pval_blackpct_cond = dat2$blackpct_samefewer_sameseats/dat2$sameseats


## Placebo: Walmarts
dat2$pval_walmart = dat2$walmarts_samefewer/dat2$nsims
dat2$pval_walmart_cond = dat2$walmarts_samefewer_sameseats/dat2$sameseats
dat2$pval_airport = dat2$airports_samefewer/dat2$nsims
dat2$pval_airport_cond = dat2$airports_samefewer_sameseats/dat2$sameseats


## Get state names
statedat = data.frame(state = state.abb, statename = state.name)
dat2 = merge(dat2, statedat, by="state")
rm(statedat)
dat2$statecode = toupper(dat2$statecode)
dat2$state_full = dat2$statename
dat2$state_full = ifelse(grepl("12", dat2$statecode),
                         paste(dat2$statename, "12"),
                         dat2$statename)
dat2$state_full = ifelse(grepl("16", dat2$statecode),
                         paste(dat2$statename, "16"),
                         dat2$state_full)
dat2$state_full = ifelse(grepl("18", dat2$statecode),
                         paste(dat2$statename, "18"),
                         dat2$state_full)
dat2$state_full = ifelse(grepl("VETOED", dat2$statecode),
                         paste(dat2$statename, "Vetoed"),
                         dat2$state_full)

## Color by method
dat2$mapmaker = c("Com", "R", "Com", "D", "D", "Cou", "R", "R", "Cou", "D", "R", "D",
                  "R", "Cou", "R", "R", "Cou", "R")
dat2$color = dplyr::recode(dat2$mapmaker, Com = "darkorchid3",
                           D = "dodgerblue3",
                           R = "firebrick3",
                           Cou = "darkgreen"
)
dat2 = dat2[order(dat2$pval),]


## How many states are seat gerrymanders?
dat2 %>% select(state_full,pval_seats)


lineplot_fn = function(varname, hoffset = NULL){
  col_id = which(colnames(dat2)==varname)
  
  if(is.null(hoffset)){
    hoffset = c(0, 0,0, 0,0, 0, 0, 0, 0,0,0, 0, 0, 0, 0, 0, 0, 0)
    
  }
  
  dat2_a = dat2

  plot(NA, xlim = c(0,1), ylim=c(0,1),
       xlab = "", ylab = "", xaxt="n", yaxt="n", bty="n")
  segments(x0 = 0, x1 = 1, y0=0.5, y1=0.5, lwd=3)
  segments(x0 = c(0, 0.1, 0.9, 1),
           x1 = c(0, 0.1, 0.9, 1),
           y0 = 0.5,
           y1 = 0.56, lwd=3)
  text(x = c(0, 0.1, 0.9, 1),
       y = 0.575,
       labels = c(0, 0.1, 0.9, 1), cex=0.75,
       font=2)
  segments(x0 = dat2_a[,col_id],
           x1 = dat2_a[,col_id],
           y0 = 0.485,
           y1 = 0.55,
           col = dat2_a$color)
  text(x = dat2_a[,col_id] + hoffset + 0.01,
       y = 0.475 ,#+ dat2$offset,
       labels = dat2_a$statename,
       col = dat2_a$color,
       cex=0.85, srt=90,
       font=2, pos = 2)
  
}

### Figure 3: Firm gerrymandering Unconditional
lineplot_fn("pval", hoffset = c(-0.008, -0.003,0, 0,0, 0, 0, 0, 0,0,-0.0045, 0, 0, 0, -0.01, -0.011, 0, 0.011))

### Figure 10: Firm gerrymandering Conditional
lineplot_fn("pval_cond", hoffset = c(-0.007, +0.007, 0, 0,-0.003, -0.007, 0, 0, 0,+0.003,+0.007, 0, 0, 0, -0.01, -0.011, 0, 0.011))







######### Extra Plots: not in the Manuscript

### Urban Unconditional
lineplot_fn("pval_urban", c(0,0,0,+0.009,0,-0.009,0,+0.016,0,0,+0.009,0,-0.013,0,0,0,0,0))

# Urban conditional
lineplot_fn("pval_urban_cond",  c(0,0,0,0,-0.005,0,+0.003,-0.016,+0.007,+0.005,0,0,0,0,+0.009,0,0,0))

## White Unconditional
lineplot_fn("pval_whitepct",  c(-0.002,0,-0.009,0,+0.012,+0.006,0,+0.006,-0.016,0,+0.012,+0.022,0,+0.014,0,-0.016,0,0))

# White voters conditional
lineplot_fn("pval_whitepct_cond",  c(-0.02,0,-0.013,0,+0.004,+0.004,0,-0.006,-0.027,+0.01,0,+0.017,+0.011,+0.009,0,+0.023,0,0))

## Top Income Unconditional
lineplot_fn("pval_topinc",  c(-0.009,+0.005, -0.0025, 0,-.004, 0, 0, -0.004, -0.009,0,+0.004, -0.007, 0, +0.007, 0, -0.013, 0, 0))

## Top Income Conditional
lineplot_fn("pval_topinc_cond",  c(-0.034,+0.041,0,0,+0.016,+0.008,0,0,+0.008,-0.007,-0.025,-0.016,0,0,+0.022,0,0,0))

#### Placebo ####
lineplot_fn("pval_airport")
lineplot_fn("pval_airport_cond")
lineplot_fn("pval_walmart")
lineplot_fn("pval_walmart_cond")


#### Donors  ####
lineplot_fn("pval_donors")
lineplot_fn("pval_donors_cond")
lineplot_fn("pval_donorsleft")
lineplot_fn("pval_donorsleft_cond")
lineplot_fn("pval_donorsright")
lineplot_fn("pval_donorsright_cond")
lineplot_fn("pval_donorsswing")
lineplot_fn("pval_donorsswing_cond")


#### Dollars Weighted ####
lineplot_fn("pval_dollarsw")
lineplot_fn("pval_dollarsw_cond")
