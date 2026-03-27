# Title -------------------------------------------------------------------

# Replication script for "How to distinguish human error from election fraud: Evidence from the 2019 Malawi election"   
# Authors Johan Ahlback and Ryan Jablonski               


# Description -------------------------------------------------------------

# Use this script to replicate tables and figures presented in section 6 and the online appendix

# Section 6 includes the following analyses:

# How the number of polling streams affect result-sheet edits, presented in figure 6 and in table 1
# How the allocation of received ballots affect result-sheet edits, presented in figues X and table X


# Prerequisites -----------------------------------------------------------
# run codes in data_preparation.R prior to running the codes below

################################################################################


# Section 6.1 The number of polling streams and result-sheet edits --------

# Figure 5: RDD plots -----------------------------------------------------
# This code generates the RDD plots presented in figure 5

edits$comb_alt <- (edits$parl_alt + edits$parl_alt + edits$lc_alt)/3 # create an average edits for all 3 elections

dat1 <- subset(edits, edits$registered<=1600) # Only polling stations with 1 or 2 streams

comb19 <- rdplot(dat1$comb_alt, dat1$registered, c=800.5)
comb19g <- comb19$rdplot # transform into ggplot

figure5.1 <- comb19g +
  ggtitle("All elections combined") +
  xlab("Registered voters") +
  ylab("Share of polling stations with edits") +
  ylim(0,1) +
  theme(aspect.ratio = 1) +
  scale_x_continuous(breaks = seq(0, 1600, by=200))

pres19 <- rdplot(dat1$pres_alt, dat1$registered, c=800.5)
pres19g <- pres19$rdplot

figure5.2 <- pres19g +
  ggtitle("Presidential election") +
  xlab("Registered voters") +
  ylab("Share of polling stations with edits") +
  ylim(0,1) +
  theme(aspect.ratio = 1) +
  scale_x_continuous(breaks = seq(0, 1600, by=200))

parl19 <- rdplot(dat1$parl_alt, dat1$registered, c=800.5)
parl19g <- parl19$rdplot

figure5.3 <- parl19g +
  ggtitle("Parliamentary election") +
  xlab("Registered voters") +
  ylab("Share of polling stations with edits") +
  ylim(0,1) +
  theme(aspect.ratio = 1) +
  scale_x_continuous(breaks = seq(0, 1600, by=200))

lc19 <- rdplot(dat1$lc_alt, dat1$registered, c=800.5)
lc19g <- lc19$rdplot

figure5.4 <- lc19g +
  ggtitle("Local councillor election") +
  xlab("Registered voters") +
  ylab("Share of polling stations with edits") +
  ylim(0,1) +
  theme(aspect.ratio = 1) +
  scale_x_continuous(breaks = seq(0, 1600, by=200))

figure5 <- grid.arrange(figure5.1, figure5.2, figure5.3, figure5.4, 
             ncol = 2, nrow = 2)

figure5


# Table 1: RDD estimates: result-sheet complexity and edits ---------------
# This code generates table 1 for RDD estimates between 1 and 2 streams

tidy.rdrobust <- function(model, ...) {
  ret <- data.frame(
    term = row.names(model$coef),
    estimate = model$coef[, 1],
    std.error = model$se[, 1],
    p.value = model$pv[, 1]
  )
  row.names(ret) <- NULL
  ret
}

glance.rdrobust <- function(model, ...) {
  ret <- data.frame(
    #    Polynomial = model$p,
    Bandwidth = model$bwselect 
    #    Cutoff = model$c,
    #    Observations (left) = model$N[1],    
    #    Observations (right) = model$N[2]    
  )
  ret
}

models1 <- list(
  "Combined" = list(
    "(1)" = rdrobust(dat1$comb_alt, dat1$registered, c=800.5, p=1, bwselect = "mserd"), 
    "(2)" = rdrobust(dat1$comb_alt, dat1$registered, c=800.5, p=1, bwselect = "cerrd") 
  ),
  "Presidential" = list(
    "(1)" = rdrobust(dat1$pres_alt, dat1$registered, c=800.5, p=1, bwselect = "mserd"), 
    "(2)" = rdrobust(dat1$pres_alt, dat1$registered, c=800.5, p=1, bwselect = "cerrd") 
  ), 
  "Parliamentary" = list(
    "(1)" = rdrobust(dat1$parl_alt, dat1$registered, c=800.5, p=1, bwselect = "mserd"), 
    "(2)" = rdrobust(dat1$parl_alt, dat1$registered, c=800.5, p=1, bwselect = "cerrd") 
  ), 
  "Local Councilor" = list(
    "(1)" = rdrobust(dat1$lc_alt, dat1$registered, c=800.5, p=1, bwselect = "mserd"), 
    "(2)" = rdrobust(dat1$lc_alt, dat1$registered, c=800.5, p=1, bwselect = "cerrd") 
  )
)


modelsummary(models1, shape = "cbind", statistic = "std.error", stars = T, output = "latex", 
             title = "RDD estimates: result-sheet complexity and result-sheet edits in the 2019 elections",
             notes = "Note: This table shows estimates of result-sheet edits around the 800 registered voter cutoff in the different elections. Conventional, bias-corrected and robust estimates are presented for MSE-optimal (column 1) and CER-optimal (column 2) bandwith selectors. Standard errors are reported in parentheses. All models were estimated with the first order polynomial.") 



# Figure S16: LATE at different bandwidths --------------------------------
# This code generates figure S16 comparing the RDD effect at different bandwidths

# MSE-optimal
out1h400 <- rdrobust(dat1$pres_alt, dat1$registered, c=800.5, h=400, p=1, bwselect = "mserd")
out1h375 <- rdrobust(dat1$pres_alt, dat1$registered, c=800.5, h=375, p=1, bwselect = "mserd")
out1h350 <- rdrobust(dat1$pres_alt, dat1$registered, c=800.5, h=350, p=1, bwselect = "mserd")
out1h325 <- rdrobust(dat1$pres_alt, dat1$registered, c=800.5, h=325, p=1, bwselect = "mserd")
out1h300 <- rdrobust(dat1$pres_alt, dat1$registered, c=800.5, h=300, p=1, bwselect = "mserd")
out1h275 <- rdrobust(dat1$pres_alt, dat1$registered, c=800.5, h=275, p=1, bwselect = "mserd")
out1h250 <- rdrobust(dat1$pres_alt, dat1$registered, c=800.5, h=250, p=1, bwselect = "mserd")
out1h225 <- rdrobust(dat1$pres_alt, dat1$registered, c=800.5, h=225, p=1, bwselect = "mserd")
out1h200 <- rdrobust(dat1$pres_alt, dat1$registered, c=800.5, h=200, p=1, bwselect = "mserd")
out1h175 <- rdrobust(dat1$pres_alt, dat1$registered, c=800.5, h=175, p=1, bwselect = "mserd")
out1h150 <- rdrobust(dat1$pres_alt, dat1$registered, c=800.5, h=150, p=1, bwselect = "mserd")
out1h125 <- rdrobust(dat1$pres_alt, dat1$registered, c=800.5, h=125, p=1, bwselect = "mserd")
out1h100 <- rdrobust(dat1$pres_alt, dat1$registered, c=800.5, h=100, p=1, bwselect = "mserd")
out1h75 <- rdrobust(dat1$pres_alt, dat1$registered, c=800.5, h=75, p=1, bwselect = "mserd")
out1h50 <- rdrobust(dat1$pres_alt, dat1$registered, c=800.5, h=50, p=1, bwselect = "mserd")
out1h25 <- rdrobust(dat1$pres_alt, dat1$registered, c=800.5, h=25, p=1, bwselect = "mserd")

pdata2a <- data.frame(
  term = c(c("400", "375", "350", "325", "300", "275", "250", "225", "200", "175", "150", "125", "100", "75", "50", "25")), 
  estimate = c(out1h400$coef[1,1], out1h375$coef[1,1], out1h350$coef[1,1], out1h325$coef[1,1], 
               out1h300$coef[1,1], out1h275$coef[1,1], out1h250$coef[1,1], out1h225$coef[1,1], 
               out1h200$coef[1,1], out1h175$coef[1,1], out1h150$coef[1,1], out1h125$coef[1,1], 
               out1h100$coef[1,1], out1h75$coef[1,1], out1h50$coef[1,1], out1h25$coef[1,1])
)


pdata2a$conf.low <- c(out1h400$ci[3,1], out1h375$ci[3,1], out1h350$ci[3,1], out1h325$ci[3,1], out1h300$ci[3,1], out1h275$ci[3,1], out1h250$ci[3,1], out1h225$ci[3,1], out1h200$ci[3,1], out1h175$ci[3,1], out1h150$ci[3,1], out1h125$ci[3,1], out1h100$ci[3,1], out1h75$ci[3,1], out1h50$ci[3,1], out1h25$ci[3,1])
pdata2a$conf.high <- c(out1h400$ci[3,2], out1h375$ci[3,2], out1h350$ci[3,2], out1h325$ci[3,2], out1h300$ci[3,2], out1h275$ci[3,2], out1h250$ci[3,2], out1h225$ci[3,2], out1h200$ci[3,2], out1h175$ci[3,2], out1h150$ci[3,2], out1h125$ci[3,2], out1h100$ci[3,2], out1h75$ci[3,2], out1h50$ci[3,2], out1h25$ci[3,2])
pdata2a$model <- "MSE-otpimal"


figure.s16 <- dwplot(pdata2a, 
                dot_args = list(size=1.5), 
                whisker_args = list(size=0.75),
                vline = geom_vline(xintercept = 0, colour="black", linetype=2, size=0.5))+
  theme_bw()+  xlab("LATE (95% confidence)")+ylab("Selected Bandwith (Registered voters)")+
  #  scale_colour_discrete(name = "Model")+
  coord_cartesian(xlim=c(-0.25, 1))+
  theme(legend.position="none") +
  ggtitle("")
#  theme(text = element_text(size=12),  
#        legend.position = c(0.007, 0.1),
#        legend.justification = c(0, 0)) 


figure.s16



# Figure S17: LATE at different polynomial orders -------------------------
# This code generates figure S17 comparing RDD effect at different polynomial orders

out1p1 <- rdrobust(dat1$pres_alt, dat1$registered, c=800.5, p=1, bwselect = "mserd")
out1p2 <- rdrobust(dat1$pres_alt, dat1$registered, c=800.5, p=2, bwselect = "mserd")
out1p3 <- rdrobust(dat1$pres_alt, dat1$registered, c=800.5, p=3, bwselect = "mserd")
out1p4 <- rdrobust(dat1$pres_alt, dat1$registered, c=800.5, p=4, bwselect = "mserd")

pdata3 <- data.frame(
  term = c(c("1st", "2nd", "3rd", "4th")), 
  estimate = c(out1p1$coef[1,1], out1p2$coef[1,1], out1p3$coef[1,1], out1p4$coef[1,1])
)

pdata3$conf.low <- c(out1p1$ci[3,1],out1p2$ci[3,1], out1p3$ci[3,1], out1p4$ci[3,1])
pdata3$conf.high <- c(out1p1$ci[3,2],out1p2$ci[3,2], out1p3$ci[3,2], out1p4$ci[3,2])

figure.s17 <- dwplot(pdata3, 
                dot_args = list(size=1.5), 
                whisker_args = list(size=0.75),
                vline = geom_vline(xintercept = 0, colour="black", linetype=2, size=0.5))+
  theme_bw()+  xlab("LATE (95% confidence)")+ylab("Selected polynomial order")+
  coord_cartesian(xlim=c(-0.25, 0.75))+
  theme(legend.position="none") +
  ggtitle("")

figure.s17



# Table S12: RDD estimates at 1600.5 --------------------------------------
# This code generates table S12 showing RDD estimates between 2 and 3 streams

dat2 <- subset(edits, edits$registered>=801 & edits$registered<=2400) # only polling stations with 2 or 3 streams

models2 <- list(
  "Combined" = list(
    "(1)" = rdrobust(dat2$comb_alt, dat2$registered, c=1600.5, p=1, bwselect = "mserd"), 
    "(2)" = rdrobust(dat2$comb_alt, dat2$registered, c=1600.5, p=1, bwselect = "cerrd") 
  ),
  "Presidential" = list(
    "(1)" = rdrobust(dat2$pres_alt, dat2$registered, c=1600.5, p=1, bwselect = "mserd"), 
    "(2)" = rdrobust(dat2$pres_alt, dat2$registered, c=1600.5, p=1, bwselect = "cerrd") 
  ), 
  "Parliamentary" = list(
    "(1)" = rdrobust(dat2$parl_alt, dat2$registered, c=1600.5, p=1, bwselect = "mserd"), 
    "(2)" = rdrobust(dat2$parl_alt, dat2$registered, c=1600.5, p=1, bwselect = "cerrd") 
  ), 
  "Local Councilor" = list(
    "(1)" = rdrobust(dat2$lc_alt, dat2$registered, c=1600.5, p=1, bwselect = "mserd"), 
    "(2)" = rdrobust(dat2$lc_alt, dat2$registered, c=1600.5, p=1, bwselect = "cerrd") 
  )
)


modelsummary(models2, shape = "cbind", statistic = "std.error", stars = T, output = "latex", 
             title = "RDD estimates at 1600.5 cutoff",
             notes = "Note: This table shows estimates of result-sheet edits around the 1600 registered voter cutoff in the different elections. Conventional, bias-corrected and robust estimates are presented for MSE-optimal (column 1) and CER-optimal (column 2) bandwith selectors. Standard errors are reported in parentheses. All models were estimated with the first order polynomial.") 



# Table S13: RDD estimates at 2400.5 --------------------------------------
# This code generates table S12 showing RDD estimates between 3 and 4 streams

dat3 <- subset(edits, edits$registered>=1601 & edits$registered<3200) # only include polling stations with 3 or 4 streams

models3 <- list(
  "Combined" = list(
    "(1)" = rdrobust(dat3$comb_alt, dat3$registered, c=2400.5, p=1, bwselect = "mserd"), 
    "(2)" = rdrobust(dat3$comb_alt, dat3$registered, c=2400.5, p=1, bwselect = "cerrd") 
  ),
  "Presidential" = list(
    "(1)" = rdrobust(dat3$pres_alt, dat3$registered, c=2400.5, p=1, bwselect = "mserd"), 
    "(2)" = rdrobust(dat3$pres_alt, dat3$registered, c=2400.5, p=1, bwselect = "cerrd") 
  ), 
  "Parliamentary" = list(
    "(1)" = rdrobust(dat3$parl_alt, dat3$registered, c=2400.5, p=1, bwselect = "mserd"), 
    "(2)" = rdrobust(dat3$parl_alt, dat3$registered, c=2400.5, p=1, bwselect = "cerrd") 
  ), 
  "Local Councilor" = list(
    "(1)" = rdrobust(dat3$lc_alt, dat3$registered, c=2400.5, p=1, bwselect = "mserd"), 
    "(2)" = rdrobust(dat3$lc_alt, dat3$registered, c=2400.5, p=1, bwselect = "cerrd") 
  )
)


modelsummary(models3, shape = "cbind", statistic = "std.error", stars = T, output = "latex", 
             title = "RDD estimates at 2400.5 cutoff",
             notes = "Note: This table shows estimates of result-sheet edits around the 2400 registered voter cutoff in the different elections. Conventional, bias-corrected and robust estimates are presented for MSE-optimal (column 1) and CER-optimal (column 2) bandwith selectors. Standard errors are reported in parentheses. All models were estimated with the first order polynomial.") 



# Figure S18: McCrary test for different cutoffs --------------------------
# This code generates figure S18 showing McCrary tests for the different cutoffs

rdens1 <- rddensity(dat1$registered, c=800.5)
rdensplot1 <- rdplotdensity(rdens1, dat1$registered)
rdensplot1.1 <- rdensplot1$Estplot # transform into ggplot
figure.s18.1 <- rdensplot1.1 +
  ggtitle("Cutoff: 800.5") +
  xlab("Registered voters")

rdens2 <- rddensity(dat2$registered, c=1600.5)
rdensplot2 <- rdplotdensity(rdens2, dat2$registered)
rdensplot2.1 <- rdensplot2$Estplot # transform into ggplot
figure.s18.2 <- rdensplot2.1 +
  ggtitle("Cutoff: 1600.5") +
  xlab("Registered voters")

rdens3 <- rddensity(dat3$registered, c=2400.5)
rdensplot3 <- rdplotdensity(rdens3, dat3$registered)
rdensplot3.1 <- rdensplot3$Estplot # transform into ggplot
figure.s18.3 <- rdensplot3.1 +
  ggtitle("Cutoff: 2400.5") +
  xlab("Registered voters")

figure.s18 <- grid.arrange(figure.s18.1, figure.s18.2, figure.s18.3, 
                        ncol = 3, nrow = 1)



# Table S14: McCrary test at 800.5 ----------------------------------------
# This code generates table S14 with McCrary tests for the cutoff at 800.5 

dc.out1 <- rddensity(X = edits$registered, c = 800.5, bwselect = 'each')
dc.out2 <- rddensity(X = edits$registered, c = 800.5, h = 0.25*unlist(dc.out1$h), bwselect = 'each')
dc.out3 <- rddensity(X = edits$registered, c = 800.5, h = 0.5*unlist(dc.out1$h), bwselect = 'each')
dc.out4 <- rddensity(X = edits$registered, c = 800.5, h = 1.25*unlist(dc.out1$h), bwselect = 'each')
dc.out5 <- rddensity(X = edits$registered, c = 800.5, h = 1.5*unlist(dc.out1$h), bwselect = 'each')

dc.theta <- c(dc.out2$hat$diff, dc.out3$hat$diff, dc.out1$hat$diff, dc.out4$hat$diff, dc.out5$hat$diff)
dc.p <- c(dc.out2$test$p_jk, dc.out3$test$p_jk, dc.out1$test$p_jk, dc.out4$test$p_jk, dc.out5$test$p_jk)
mc.bw.l <- c(0.25*dc.out1$h$left, 0.5*dc.out1$h$left, dc.out1$h$left, 1.25*dc.out1$h$left, 1.5*dc.out1$h$left)
mc.bw.r <- c(0.25*dc.out1$h$right, 0.5*dc.out1$h$right, dc.out1$h$right, 1.25*dc.out1$h$right, 1.5*dc.out1$h$right)
mult <- c(0.25, 0.5, 1, 1.25, 1.5)
tab.out <- cbind(round(mc.bw.l,2), round(mc.bw.r, 2), mult, round(dc.theta, 5), round(dc.p,2))
colnames(tab.out) <- c('Bandwidth (left)', 'Bandwidth (right)', 'Multiple', 'Est. Difference', 'p-value')

xto <- xtable(tab.out)
#file.out <- paste('output/dcTable.tex',sep = "")

print.xtable(xto, caption.placement = 'top', type = 'latex',
             include.rownames = F, include.colnames = T, table.placement = '!!h',
             hline.after = c(-1,-1,0,nrow(tab.out), nrow(tab.out)))


# Table S15: McCrary test at 1600.5 ---------------------------------------
# This code generates table S14 with McCrary tests for the cutoff at 1600.5 

dc.out1 <- rddensity(X = dat2$registered, c = 1600.5, bwselect = 'each')
dc.out2 <- rddensity(X = dat2$registered, c = 1600.5, h = 0.25*unlist(dc.out1$h), bwselect = 'each')
dc.out3 <- rddensity(X = dat2$registered, c = 1600.5, h = 0.5*unlist(dc.out1$h), bwselect = 'each')
dc.out4 <- rddensity(X = dat2$registered, c = 1600.5, h = 1.25*unlist(dc.out1$h), bwselect = 'each')
dc.out5 <- rddensity(X = dat2$registered, c = 1600.5, h = 1.5*unlist(dc.out1$h), bwselect = 'each')

dc.theta <- c(dc.out2$hat$diff, dc.out3$hat$diff, dc.out1$hat$diff, dc.out4$hat$diff, dc.out5$hat$diff)
dc.p <- c(dc.out2$test$p_jk, dc.out3$test$p_jk, dc.out1$test$p_jk, dc.out4$test$p_jk, dc.out5$test$p_jk)
mc.bw.l <- c(0.25*dc.out1$h$left, 0.5*dc.out1$h$left, dc.out1$h$left, 1.25*dc.out1$h$left, 1.5*dc.out1$h$left)
mc.bw.r <- c(0.25*dc.out1$h$right, 0.5*dc.out1$h$right, dc.out1$h$right, 1.25*dc.out1$h$right, 1.5*dc.out1$h$right)
mult <- c(0.25, 0.5, 1, 1.25, 1.5)
tab.out <- cbind(round(mc.bw.l,2), round(mc.bw.r, 2), mult, round(dc.theta, 5), round(dc.p,2))
colnames(tab.out) <- c('Bandwidth (left)', 'Bandwidth (right)', 'Multiple', 'Est. Difference', 'p-value')

xto <- xtable(tab.out)
#file.out <- paste('output/dcTable.tex',sep = "")

print.xtable(xto, caption.placement = 'top', type = 'latex',
             include.rownames = F, include.colnames = T, table.placement = '!!h',
             hline.after = c(-1,-1,0,nrow(tab.out), nrow(tab.out)))


# Table S16: McCrary test at 2400.5 ---------------------------------------
# This code generates table S16 with McCrary tests for the cutoff at 2400.5 

dc.out1 <- rddensity(X = dat3$registered, c = 2400.5, bwselect = 'each')
dc.out2 <- rddensity(X = dat3$registered, c = 2400.5, h = 0.25*unlist(dc.out1$h), bwselect = 'each')
dc.out3 <- rddensity(X = dat3$registered, c = 2400.5, h = 0.5*unlist(dc.out1$h), bwselect = 'each')
dc.out4 <- rddensity(X = dat3$registered, c = 2400.5, h = 1.25*unlist(dc.out1$h), bwselect = 'each')
dc.out5 <- rddensity(X = dat3$registered, c = 2400.5, h = 1.5*unlist(dc.out1$h), bwselect = 'each')

dc.theta <- c(dc.out2$hat$diff, dc.out3$hat$diff, dc.out1$hat$diff, dc.out4$hat$diff, dc.out5$hat$diff)
dc.p <- c(dc.out2$test$p_jk, dc.out3$test$p_jk, dc.out1$test$p_jk, dc.out4$test$p_jk, dc.out5$test$p_jk)
mc.bw.l <- c(0.25*dc.out1$h$left, 0.5*dc.out1$h$left, dc.out1$h$left, 1.25*dc.out1$h$left, 1.5*dc.out1$h$left)
mc.bw.r <- c(0.25*dc.out1$h$right, 0.5*dc.out1$h$right, dc.out1$h$right, 1.25*dc.out1$h$right, 1.5*dc.out1$h$right)
mult <- c(0.25, 0.5, 1, 1.25, 1.5)
tab.out <- cbind(round(mc.bw.l,2), round(mc.bw.r, 2), mult, round(dc.theta, 5), round(dc.p,2))
colnames(tab.out) <- c('Bandwidth (left)', 'Bandwidth (right)', 'Multiple', 'Est. Difference', 'p-value')

xto <- xtable(tab.out)
#file.out <- paste('output/dcTable.tex',sep = "")

print.xtable(xto, caption.placement = 'top', type = 'latex',
             include.rownames = F, include.colnames = T, table.placement = '!!h',
             hline.after = c(-1,-1,0,nrow(tab.out), nrow(tab.out)))



# Figure S19: Balance test for RDD ----------------------------------------
# This code generates figure S19 showing balance tests of pre-treatment variables

out1b1 <- rdrobust(dat1$turnout2014, dat1$registered, c=800.5, p=1)
out1b2 <- rdrobust(dat1$dpp_share2014, dat1$registered, c=800.5, p=1)
out1b3 <- rdrobust(dat1$female, dat1$registered, c=800.5, p=1)
out1b4 <- rdrobust(dat1$youth, dat1$registered, c=800.5, p=1)
out1b5 <- rdrobust(dat1$pop_density14a, dat1$registered, c=800.5, p=1)
out1b7 <- rdrobust(dat1$elevation100, dat1$registered, c=800.5, p=1)
out1b8 <- rdrobust(dat1$dep_ratio100, dat1$registered, c=800.5, p=1)
out1b9 <- rdrobust(dat1$nightlights_2014a, dat1$registered, c=800.5, p=1)


pdata5 <- data.frame(
  term = c(c("Turnout 2014 (%)", "Mutharika vote-share 2014 (%)", "Female share of registered (%)", "Youth share of registered (%)", 
             "Population density 2014 (0-100)", "Elevation (0-100)", "Dependency Ratio 2011 (0-100)", "Nightlights 2014 (0-100)")), 
  estimate = c(out1b1$coef[1,1], out1b2$coef[1,1], out1b3$coef[1,1],out1b4$coef[1,1], out1b5$coef[1,1], out1b7$coef[1,1], out1b8$coef[1,1], out1b9$coef[1,1])
)


pdata5$conf.low <- c(out1b1$ci[3,1], out1b2$ci[3,1], out1b3$ci[3,1], out1b4$ci[3,1], out1b5$ci[3,1], out1b7$ci[3,1], out1b8$ci[3,1], out1b9$ci[3,1])
pdata5$conf.high <- c(out1b1$ci[3,2], out1b2$ci[3,2], out1b3$ci[3,2], out1b4$ci[3,2], out1b5$ci[3,2], out1b7$ci[3,2], out1b8$ci[3,2], out1b9$ci[3,2])

figure.s19 <- dwplot(pdata5, 
                dot_args = list(size=1.5), 
                whisker_args = list(size=0.75),
                vline = geom_vline(xintercept = 0, colour="black", linetype=2, size=0.5))+
  theme_bw()+  xlab("LATE (95% confidence)")+ylab("Covariates")+
  coord_cartesian(xlim=c(-10, 10))+
  theme(legend.position="none") +
  ggtitle("")

figure.s19


# Figure S20: RDD-effect on vote figures in the 2019 election -------------
# This code generates figure S20 showing the RDD-effects on vote figures

out1b1 <- rdrobust(dat1$turnout, dat1$registered, c=800.5, p=1)
out1b2 <- rdrobust(dat1$invalidation, dat1$registered, c=800.5, p=1)
out1b3 <- rdrobust(dat1$mec_mutharika_percent_mec, dat1$registered, c=800.5, p=1)
out1b4 <- rdrobust(dat1$mec_chakwera_percent_mec, dat1$registered, c=800.5, p=1)
out1b5 <- rdrobust(dat1$mec_chilima_percent_mec, dat1$registered, c=800.5, p=1)

pdata5 <- data.frame(
  term = c(c("Turnout 2019 (%)", "Invalidation 2019 (%)", "Mutharika vote-share 2019 (%)", "Chakwera vote-share 2019 (%)", "Chilima vote-share 2019 (%)")), 
  estimate = c(out1b1$coef[1,1], out1b2$coef[1,1],out1b3$coef[1,1],out1b4$coef[1,1],out1b5$coef[1,1])
)

pdata5$conf.low <- c(out1b1$ci[3,1], out1b2$ci[3,1], out1b3$ci[3,1], out1b4$ci[3,1], out1b5$ci[3,1])
pdata5$conf.high <- c(out1b1$ci[3,2], out1b2$ci[3,2], out1b3$ci[3,2], out1b4$ci[3,2], out1b5$ci[3,2])

figure.s20 <- dwplot(pdata5, 
                dot_args = list(size=1.5), 
                whisker_args = list(size=0.75),
                vline = geom_vline(xintercept = 0, colour="black", linetype=2, size=0.5))+
  theme_bw()+  xlab("LATE (95% confidence)")+ylab("Covariates")+
  coord_cartesian(xlim=c(-12, 12))+
  theme(legend.position="none") +
  ggtitle("")

figure.s20


# Figure S20: Placebo test for RDD ----------------------------------------
# This code generates figure S20 showing a placebo test of the RDD

dat1_under <- subset(dat1, registered<800.5)
dat1_over <- subset(dat1, registered>800.5)

# comb

r400 <- rdrobust(dat1_under$comb_alt, dat1_under$registered, c=400, p=1)
r500 <- rdrobust(dat1_under$comb_alt, dat1_under$registered, c=500, p=1)
r600 <- rdrobust(dat1_under$comb_alt, dat1_under$registered, c=600, p=1)
r700 <- rdrobust(dat1_under$comb_alt, dat1_under$registered, c=700, p=1)
r800.5 <- rdrobust(dat1$comb_alt, dat1$registered, c=800.5, p=1) # True cutoff
r900 <- rdrobust(dat1_over$comb_alt, dat1_over$registered, c=900, p=1)
r1000 <- rdrobust(dat1_over$comb_alt, dat1_over$registered, c=1000, p=1)
r1100 <- rdrobust(dat1_over$comb_alt, dat1_over$registered, c=1100, p=1)
r1200 <- rdrobust(dat1_over$comb_alt, dat1_over$registered, c=1200, p=1)

pdata_placebo <- data.frame(
  term = c(c("400",  
             "500",    
             "600", 
             "700",    
             "800.5",
             "900",   
             "1000", 
             "1100",    
             "1200")), 
  estimate = c(r400$coef[1,1],  
               r500$coef[1,1],
               r600$coef[1,1],
               r700$coef[1,1],
               r800.5$coef[1,1],
               r900$coef[1,1],
               r1000$coef[1,1], 
               r1100$coef[1,1], 
               r1200$coef[1,1])
)


pdata_placebo$conf.low <- c(r400$ci[3,1], r500$ci[3,1], r600$ci[3,1], r700$ci[3,1], r800.5$ci[3,1], r900$ci[3,1], r1000$ci[3,1], r1100$ci[3,1], r1200$ci[3,1])
pdata_placebo$conf.high <- c(r400$ci[3,2], r500$ci[3,2], r600$ci[3,2], r700$ci[3,2], r800.5$ci[3,2], r900$ci[3,2], r1000$ci[3,2], r1100$ci[3,2], r1200$ci[3,2])


plot_placebo_comb <- dwplot(pdata_placebo, 
                            dot_args = list(size=1.5), 
                            whisker_args = list(size=0.75),
                            vline = geom_vline(xintercept = 0, colour="black", linetype=2, size=0.5))+
  theme_bw()+  xlab("LATE (95% confidence)")+ylab("Cutoffs")+
  coord_cartesian(xlim=c(-0.50, 0.50))+
  theme(legend.position="none") +
  ggtitle("All elections combined")+
  geom_hline(yintercept=5, linetype='dotted', col = 'lightblue', size=1.1)


plot_placebo_comb

# pres

r400 <- rdrobust(dat1_under$pres_alt, dat1_under$registered, c=400, p=1)
r500 <- rdrobust(dat1_under$pres_alt, dat1_under$registered, c=500, p=1)
r600 <- rdrobust(dat1_under$pres_alt, dat1_under$registered, c=600, p=1)
r700 <- rdrobust(dat1_under$pres_alt, dat1_under$registered, c=700, p=1)
r800.5 <- rdrobust(dat1$pres_alt, dat1$registered, c=800.5, p=1) # True cutoff
r900 <- rdrobust(dat1_over$pres_alt, dat1_over$registered, c=900, p=1)
r1000 <- rdrobust(dat1_over$pres_alt, dat1_over$registered, c=1000, p=1)
r1100 <- rdrobust(dat1_over$pres_alt, dat1_over$registered, c=1100, p=1)
r1200 <- rdrobust(dat1_over$pres_alt, dat1_over$registered, c=1200, p=1)

pdata_placebo <- data.frame(
  term = c(c("400",  
             "500",    
             "600", 
             "700",    
             "800.5",
             "900",   
             "1000", 
             "1100",    
             "1200")), 
  estimate = c(r400$coef[1,1],  
               r500$coef[1,1],
               r600$coef[1,1],
               r700$coef[1,1],
               r800.5$coef[1,1],
               r900$coef[1,1],
               r1000$coef[1,1], 
               r1100$coef[1,1], 
               r1200$coef[1,1])
)


pdata_placebo$conf.low <- c(r400$ci[3,1], r500$ci[3,1], r600$ci[3,1], r700$ci[3,1], r800.5$ci[3,1], r900$ci[3,1], r1000$ci[3,1], r1100$ci[3,1], r1200$ci[3,1])
pdata_placebo$conf.high <- c(r400$ci[3,2], r500$ci[3,2], r600$ci[3,2], r700$ci[3,2], r800.5$ci[3,2], r900$ci[3,2], r1000$ci[3,2], r1100$ci[3,2], r1200$ci[3,2])



plot_placebo_pres <- dwplot(pdata_placebo, 
                            dot_args = list(size=1.5), 
                            whisker_args = list(size=0.75),
                            vline = geom_vline(xintercept = 0, colour="black", linetype=2, size=0.5))+
  theme_bw()+  xlab("LATE (95% confidence)")+ylab("Cutoffs")+
  coord_cartesian(xlim=c(-0.50, 0.50))+
  theme(legend.position="none") +
  ggtitle("Presidential election")+
  geom_hline(yintercept=5, linetype='dotted', col = 'lightblue', size=1.1)


plot_placebo_pres


# parl

r400 <- rdrobust(dat1_under$parl_alt, dat1_under$registered, c=400, p=1)
r500 <- rdrobust(dat1_under$parl_alt, dat1_under$registered, c=500, p=1)
r600 <- rdrobust(dat1_under$parl_alt, dat1_under$registered, c=600, p=1)
r700 <- rdrobust(dat1_under$parl_alt, dat1_under$registered, c=700, p=1)
r800.5 <- rdrobust(dat1$parl_alt, dat1$registered, c=800.5, p=1) # True cutoff
r900 <- rdrobust(dat1_over$parl_alt, dat1_over$registered, c=900, p=1)
r1000 <- rdrobust(dat1_over$parl_alt, dat1_over$registered, c=1000, p=1)
r1100 <- rdrobust(dat1_over$parl_alt, dat1_over$registered, c=1100, p=1)
r1200 <- rdrobust(dat1_over$parl_alt, dat1_over$registered, c=1200, p=1)

pdata_placebo <- data.frame(
  term = c(c("400",  
             "500",    
             "600", 
             "700",    
             "800.5",
             "900",   
             "1000", 
             "1100",    
             "1200")), 
  estimate = c(r400$coef[1,1],  
               r500$coef[1,1],
               r600$coef[1,1],
               r700$coef[1,1],
               r800.5$coef[1,1],
               r900$coef[1,1],
               r1000$coef[1,1], 
               r1100$coef[1,1], 
               r1200$coef[1,1])
)


pdata_placebo$conf.low <- c(r400$ci[3,1], r500$ci[3,1], r600$ci[3,1], r700$ci[3,1], r800.5$ci[3,1], r900$ci[3,1], r1000$ci[3,1], r1100$ci[3,1], r1200$ci[3,1])
pdata_placebo$conf.high <- c(r400$ci[3,2], r500$ci[3,2], r600$ci[3,2], r700$ci[3,2], r800.5$ci[3,2], r900$ci[3,2], r1000$ci[3,2], r1100$ci[3,2], r1200$ci[3,2])


plot_placebo_parl <- dwplot(pdata_placebo, 
                            dot_args = list(size=1.5), 
                            whisker_args = list(size=0.75),
                            vline = geom_vline(xintercept = 0, colour="black", linetype=2, size=0.5))+
  theme_bw()+  xlab("LATE (95% confidence)")+ylab("Cutoffs")+
  coord_cartesian(xlim=c(-0.50, 0.50))+
  theme(legend.position="none") +
  ggtitle("Parliamentary election")+
  geom_hline(yintercept=5, linetype='dotted', col = 'lightblue', size=1.1)


plot_placebo_parl


# lc

r400 <- rdrobust(dat1_under$lc_alt, dat1_under$registered, c=400, p=1)
r500 <- rdrobust(dat1_under$lc_alt, dat1_under$registered, c=500, p=1)
r600 <- rdrobust(dat1_under$lc_alt, dat1_under$registered, c=600, p=1)
r700 <- rdrobust(dat1_under$lc_alt, dat1_under$registered, c=700, p=1)
r800.5 <- rdrobust(dat1$lc_alt, dat1$registered, c=800.5, p=1) # True cutoff
r900 <- rdrobust(dat1_over$lc_alt, dat1_over$registered, c=900, p=1)
r1000 <- rdrobust(dat1_over$lc_alt, dat1_over$registered, c=1000, p=1)
r1100 <- rdrobust(dat1_over$lc_alt, dat1_over$registered, c=1100, p=1)
r1200 <- rdrobust(dat1_over$lc_alt, dat1_over$registered, c=1200, p=1)

pdata_placebo <- data.frame(
  term = c(c("400",  
             "500",    
             "600", 
             "700",    
             "800.5",
             "900",   
             "1000", 
             "1100",    
             "1200")), 
  estimate = c(r400$coef[1,1],  
               r500$coef[1,1],
               r600$coef[1,1],
               r700$coef[1,1],
               r800.5$coef[1,1],
               r900$coef[1,1],
               r1000$coef[1,1], 
               r1100$coef[1,1], 
               r1200$coef[1,1])
)




pdata_placebo$conf.low <- c(r400$ci[3,1], r500$ci[3,1], r600$ci[3,1], r700$ci[3,1], r800.5$ci[3,1], r900$ci[3,1], r1000$ci[3,1], r1100$ci[3,1], r1200$ci[3,1])
pdata_placebo$conf.high <- c(r400$ci[3,2], r500$ci[3,2], r600$ci[3,2], r700$ci[3,2], r800.5$ci[3,2], r900$ci[3,2], r1000$ci[3,2], r1100$ci[3,2], r1200$ci[3,2])



plot_placebo_lc <- dwplot(pdata_placebo, 
                          dot_args = list(size=1.5), 
                          whisker_args = list(size=0.75),
                          vline = geom_vline(xintercept = 0, colour="black", linetype=2, size=0.5))+
  theme_bw()+  xlab("LATE (95% confidence)")+ylab("Cutoffs")+
  coord_cartesian(xlim=c(-0.50, 0.50))+
  theme(legend.position="none") +
  ggtitle("Local election") +
  geom_hline(yintercept=5, linetype='dotted', col = 'lightblue', size=1.1)

plot_placebo_lc

figure.s21 <- grid.arrange(plot_placebo_comb, plot_placebo_pres, plot_placebo_parl, plot_placebo_lc, 
             ncol = 2, nrow = 2)




# Section 6.2 Allocation of received ballots and result-sheet edits -------
# The sets of code below generate the figures and tables presented in section 6.2

# prepare data

edits$reg_buffer <- edits$registered*1.03
edits$proj_ballots <- round_any(edits$reg_buffer, 100, f=ceiling)
edits$ballots_per_stream <- edits$proj_ballots / edits$streams
edits$even_allocation <- 0
edits$even_allocation[edits$ballots_per_stream==500 | edits$ballots_per_stream==600 | edits$ballots_per_stream==700 | edits$ballots_per_stream==800]<- 1
edits$even_allocation[edits$streams==1] <- NA


# Table 2: Even allocation and result-sheet edits -------------------------
# The code below generates table 2 examining how the opportunity to allocate evenly across streams affect edits

even1 <- felm(pres_a ~ even_allocation | streams | 0 | constituency, data=edits)
even2 <- felm(pres_a ~ even_allocation | streams + constituency | 0 | constituency, data=edits)
even3 <- felm(combine ~ even_allocation | streams | 0 | constituency, data=edits)
even4 <- felm(combine ~ even_allocation | streams + constituency | 0 | constituency, data=edits)

# mean rate of edits
m1 <- round(mean(edits$pres_a[edits$even_allocation==0], na.rm=T),3)
m2 <- round(mean(edits$combine[edits$even_allocation==0], na.rm=T),2)

stargazer(even1, even2, even3, even4, 
          type = "latex",
          title = "Even allocation and result-sheet edits",
          label = "even_allocation_reg",
          dep.var.labels = c("Edit in row for received ballots", "Total number of rows with edits"),
          add.lines = list(c("Mean edits in ps with uneven allocation", m1, m1, m2, m2))) 



# Figure S22: Balance test for even allocation ----------------------------
# This code generates figure S22 showing a balance test for even allocation

mod1.r1 <- felm(turnout2014 ~ even_allocation | constituency + streams | 0 | constituency, data=edits)
mod1.r2 <- felm(dpp_share2014 ~ even_allocation | constituency + streams | 0 | constituency, data=edits)
mod1.r3 <- felm(female ~ even_allocation | constituency + streams | 0 | constituency, data=edits)
mod1.r4 <- felm(youth ~ even_allocation | constituency + streams | 0 | constituency, data=edits)
mod1.r5 <- felm(pop_density14a ~ even_allocation | constituency + streams | 0 | constituency, data=edits)
mod1.r7 <- felm(elevation100 ~ even_allocation | constituency + streams | 0 | constituency, data=edits)
mod1.r9 <- felm(dep_ratio100 ~ even_allocation| constituency + streams | 0 | constituency, data=edits)
mod1.r11 <- felm(nightlights_2014a ~ even_allocation | constituency + streams | 0 | constituency, data=edits)


pdata.streams.robust <- data.frame(
  term = c(c("Turnout 2014 (%)", "Mutharika vote-share (%)", "Female share of registered (%)", "Youth share of registered (%)", "Population density 2014 (0-100)", "Elevation (0-100)", "Dependency Ratio 2011 (0-100)", "Nightlights 2014 (0-100)")), 
  estimate = c(mod1.r1$coefficients[1,1],mod1.r2$coefficients[1,1],mod1.r3$coefficients[1,1],mod1.r4$coefficients[1,1],mod1.r5$coefficients[1,1],mod1.r7$coefficients[1,1],mod1.r9$coefficients[1,1], mod1.r11$coefficients[1,1])
)
pdata.streams.robust$conf.low <- c(pdata.streams.robust$estimate - qnorm(0.975)*
                                     c(summary(mod1.r1)$coefficients[1.2],summary(mod1.r2)$coefficients[1.2],summary(mod1.r3)$coefficients[1.2],summary(mod1.r4)$coefficients[1.2],summary(mod1.r5)$coefficients[1.2],summary(mod1.r7)$coefficients[1.2],summary(mod1.r9)$coefficients[1.2],summary(mod1.r11)$coefficients[1.2]))
pdata.streams.robust$conf.high <- c(pdata.streams.robust$estimate + qnorm(0.975)*
                                      c(summary(mod1.r1)$coefficients[1.2],summary(mod1.r2)$coefficients[1.2],summary(mod1.r3)$coefficients[1.2],summary(mod1.r4)$coefficients[1.2],summary(mod1.r5)$coefficients[1.2],summary(mod1.r7)$coefficients[1.2],summary(mod1.r9)$coefficients[1.2],summary(mod1.r11)$coefficients[1.2]))

figure.s22 <- dwplot(pdata.streams.robust, 
                dot_args = list(size=1.5), 
                whisker_args = list(size=0.75),
                vline = geom_vline(xintercept = 0, colour="black", linetype=2, size=0.5))+
  theme_bw()+  xlab("Effect of even allocation (95% confidence)")+ylab("Covariates")+
  coord_cartesian(xlim=c(-2, 2))+
  theme(legend.position="none") +
  ggtitle("")

figure.s22

