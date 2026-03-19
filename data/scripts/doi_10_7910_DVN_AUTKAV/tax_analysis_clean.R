library(readr)
library(tidyverse)
library(dplyr)
library(fastLink)
library(stringr)
library(jtools)
library(gridExtra)
library(lattice)
library(interactions)
library(benford.analysis)
library(stargazer)

setwd("")

tax <- read_csv("clean_data.csv")


col <- c("0" = "#bdbdbd", "1" = "#636363")
tax$district <- factor(tax$district, levels = tax$district[order(-tax$diff)])
tax$diff_bil <- tax$diff / 1000000000
tax__ <- tax[tax$district != "Kinondoni MC", ]

#Figure A1

diff_plot_full <- ggplot(tax, aes(x = district, y = diff_bil, fill = as.factor(tax$CCMcontrol))) + geom_bar(width = 1, stat = "identity") + 
  labs(x = "", y = "Difference in Total Taxes (billion TZS)") + theme_bw() +
   theme(panel.border = element_blank(), panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(), 
         legend.position = c(0.1,0.1)) +  theme(axis.text.x=element_blank(),
                                           axis.ticks.x=element_blank()) + scale_fill_manual(values = col, name="LGA Control",
                                                                                             labels=c("Opposition", "Incumbent")) 
# Figure 1

diff_plot_ex_KMC <- ggplot(tax__, aes(x = district, y = diff_bil, fill = as.factor(tax__$CCMcontrol))) + geom_bar(width = 1, stat = "identity") + 
  labs(x = "", y = "Difference in Total Taxes (billion TZS)") + theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        legend.position = c(0.1,0.1)) +  theme(axis.text.x=element_blank(),
                                               axis.ticks.x=element_blank()) + scale_fill_manual(values = col, name="LGA Control",
                                                                                                 labels=c("Opposition", "Incumbent")) 



# regression comparison 

tax$type <- factor(tax$type)
#tax$type <- relevel(tax$type, ref = "DC")
#tax$type <- factor(tax$type, order = TRUE, levels = c("DC", "TC", "MC", "CC"))


#Table A4


reg_h <- lm(log(tax_tamisemi) ~ Region + Total2016 + type, data = tax)
reg_h_ <- lm(log(tax_open) ~ Region + Total2016 + type, data = tax)

reg_i <- lm(log(tax_tamisemi) ~ CCMcontrol + Total2016 + type + as.factor(Region), data = tax)
reg_i_ <- lm(log(tax_open) ~ CCMcontrol + Total2016 + type + as.factor(Region), data = tax)


stargazer(reg_h, reg_i, reg_h_, reg_i_, keep = c("type"), add.lines = list(c("Region fixed effects", "Y", "Y", "Y", "Y"), c("LGA majority controls", "N", "Y", "N", "Y")), 
          covariate.labels = c("Rural council", "Municipal council", "Town council"))


# Table 2 

reg_j <- lm(log(tax_tamisemi_pc) ~ CCMcontrol + Total2016 + type + as.factor(Region), data = tax)
reg_j_ <- lm(log(tax_open_pc) ~ CCMcontrol + Total2016 + type + as.factor(Region), data = tax)


reg_l <- lm(log(tax_tamisemi_pc) ~ opp_share + Total2016 + type + as.factor(Region), data = tax)
reg_l_ <- lm(log(tax_open_pc) ~ opp_share + Total2016 + type + as.factor(Region), data = tax)


stargazer(reg_j, reg_l, reg_j_, reg_l_, keep = c("CCMcontrol", "opp_share"), covariate.labels = c("CCM majority", "Share of opp councilors"), 
          dep.var.labels = c("log(Per Capita Tax) - internal", "log(Per Capita Tax) - public"), out = ".html")

#  Tables A1 and A2 



tax$negdiff_pc <- ifelse(tax$diff_pc < 0, -1, 1)
tax$logdiff_pc <- ifelse(tax$diff_pc == 0, 0.01, tax$diff_pc)
tax$logdiff_pc <- abs(tax$logdiff_pc)
tax$logdiff_pc <- log(tax$logdiff_pc)
tax$logdiff_pc <- tax$logdiff_pc * tax$negdiff_pc


reg_per_1 <- lm(diff_pc ~ opp_share  + type + Total2016 , data = tax)
reg_per_2 <- lm(diff_pc ~ CCMcontrol  + type + Total2016, data = tax)

reg_per_5 <- lm(logdiff_pc ~ opp_share  + type + Total2016 , data = tax)
reg_per_6 <- lm(logdiff_pc ~ CCMcontrol  + type + Total2016, data = tax)


stargazer(reg_per_1, reg_per_2, keep = c("CCMcontrol", "opp_share") , covariate.labels = c("Share of opp councilors", "CCM majority"), 
          dep.var.labels = c("Per Capita Difference"), out = ".html")


stargazer(reg_per_5, reg_per_6, keep = c("CCMcontrol", "opp_share") , covariate.labels = c("Share of opp councilors", "CCM majority"), 
          dep.var.labels = c("Log(Per Capita Difference"), out = ".html")


#Table A3

sd_diff <- sd(tax$diff)
sd_diff_pc <- sd(tax$diff_pc)

tax$neg_misreport <- ifelse(tax$diff < -sd_diff, 1, 0 )
tax$pos_misreport <- ifelse(tax$diff > sd_diff, 1, 0 )

tax$neg_misreport_pc <- ifelse(tax$diff_pc < -sd_diff_pc, 1, 0 )
tax$pos_misreport_pc <- ifelse(tax$diff_pc > sd_diff_pc, 1, 0 ) # no positive misreports greater than a standard deviation 

mis_reg_3 <- glm(neg_misreport_pc ~ opp_share + Total2016 ,  data = tax,  family = "binomial")
mis_reg_4 <- glm(neg_misreport_pc ~ CCMcontrol + Total2016, data = tax,  family = "binomial")
mis_reg_7 <- glm(neg_misreport_pc ~ opp_share,  data = tax,  family = "binomial")
mis_reg_8 <- glm(neg_misreport_pc ~ CCMcontrol, data = tax,  family = "binomial")


stargazer(mis_reg_7, mis_reg_8, mis_reg_3, mis_reg_4, 
          keep = c("CCMcontrol", "opp_share"), covariate.labels = c( "Share of opp councilors", "CCM majority"), 
          dep.var.labels = c("Significant per capita underestimates"), 
          add.lines = list(c("Controls", "N", "N", "Y", "Y")), out = ".html")


# Table 3 

pri_qual <- read.csv("primary_qual_clean.csv")
tax_ <- left_join(tax, pri_qual, by = "district")

tax_$tax_tamisemi_pc_th <- tax_$tax_tamisemi_pc/1000
tax_$tax_open_pc_th <- tax_$tax_open_pc/1000


qual_reg_1 <- lm(marks ~ tax_tamisemi_pc_th + type + Region + Total2016   + yrs_ed  + CCMcontrol, data = tax_)
qual_reg_2 <- lm(marks ~ tax_open_pc_th + type + Region + Total2016 +  yrs_ed  + CCMcontrol, data = tax_)
qual_reg_3 <- lm(pass_rate ~ tax_tamisemi_pc_th + type + Region + Total2016 + yrs_ed  + CCMcontrol, data = tax_)
qual_reg_4 <- lm(pass_rate ~ tax_open_pc_th + type + Region + Total2016 +  yrs_ed  + CCMcontrol, data = tax_)

stargazer(qual_reg_1, qual_reg_2, qual_reg_3, qual_reg_4, keep = c("tax_tamisemi_pc", "tax_open_pc"), dep.var.labels = c("Test scores", "Pass rate"), 
          covariate.labels = c("Per capita tax (internal)", "Per capita tax (public)"), digits = 4, out = ".html")


# Figure A2  

ben1 <- benford(tax$tax_open, number.of.digits = 1, discrete = TRUE, sign = "positive", 
        round = 3)
ben2 <- benford(tax$tax_tamisemi, number.of.digits = 1, discrete = TRUE, sign = "positive", 
                round = 3)

ben.plot1 <- plot(ben1, except = c("mantissa", "chi squared", "summation",
                                   "abs diff", "ex summation"))
ben.plot2 <- plot(ben2, except = c("mantissa", "chi squared","summation",
                                   "abs diff", "ex summation"))


#Figure A3

## Berber Scacco functions 

get.digits <- function(v, pos="last") {
  # Function to extract numerals from elements in a vector.
  # Args:
  #   v: Vector with elements from which numerals are to be extracted.
  #   pos: Position of the numeral to be extracted from each vector element.
  #     Set to "last" (the default) to get the last numeral, "penult" to
  #     extract the second-to-last digit, or a numeric value.
  # Returns:
  #   A vector containing the numerals to be retrieved.
  if(!pos %in% c("last", "penult") & !is.numeric(pos)) stop("Invalid position")
  s <- strsplit(as.character(v), c())
  if(pos == "last") out <- sapply(s, function(y) y[length(y)])
  if(pos == "penult") out <- sapply(s, function(y) ifelse(length(y) < 2, NA, y[length(y) - 1]))
  if(is.numeric(pos)) out <- sapply(s, function(y) y[pos])
  return(out)
}

digit.test <- function(v, id=NULL) {
  # Function to obtain frequencies and test statistics for last digits and digit pairs.
  # Args:
  #   v: Vector with elements to be analyzed (e.g. polling station vote counts).
  #   id: Strata within which vector elements are grouped (e.g. precincts containing
  #       polling stations). By default, elements are not grouped in strata.
  # Returns:
  #   A list with the elements listed below. Elements with prefix "all." are generated only
  #   if strata are specified and there is more than one stratum.
  #     count: A matrix containing last-digit frequencies, with one row for each stratum.
  #     prop: A matrix containing last-digit proportions, with one row for each stratum.
  #     chi: A vector containing the values of chi-square tests for deviations in last-digit
  #       frequencies, with one element for each stratum.
  #     pval: A vector containing the corresponding p-values.
  #     n: A vector containing the number of observations within each stratum.
  #     id: A vector containing stratum identifiers.
  #     distance: A matrix containing frequencies of the possible distances between last and
  #       penultimate digits, with one row for each stratum.
  #     distance.prop: A matrix containing the corresponding proportions.
  #     distance.chi: A vector with values of chi-square tests for deviations in the frequencies
  #       of distances in digit pairs, with one value per stratum.
  #     distance.pval: A vector containing the corresponding p-values.
  #     all.count: A vector with last-digit frequencies across all strata.
  #     all.prop: A vector with the corresponding proportions.
  #     all.n: The number of observations across all strata, a scalar.
  #     all.chi: Value of a chi-square test for deviations in last-digit frequencies, a scalar.
  #     all.pval: The corresponding p-value.
  #     all.distance: A vector with the frequencies of distances between last and penultimate
  #       digits across all strata.
  #     all.distance.prop: A vector containing the corresponding proportions.
  #     all.distance.chi: Value of a chi-square test for deviations in digit-pair distances,
  #       a scalar.
  #     all.distance.pval: The corresponding p-value.
  if(!is.vector(v) | (!is.vector(id) & !is.null(id))) stop("Inputs must be vectors")
  if(!identical(length(v), length(id)) & !is.null(id)) stop("Input vectors vary in length")
  if(is.null(id)) id <- rep(1, length(v))
  id.uni <- unique(id)
  count <- matrix(ncol=10, nrow=length(id.uni), dimnames=list(NULL, c(0:9)))
  distance <- matrix(ncol=6, nrow=length(id.uni), dimnames=list(NULL, c(0:5)))
  id.out <- c()
  for(i in 1:length(id.uni)){
    # Last-digit frequencies
    last <- get.digits(v[id == id.uni[i]], pos="last")
    freq.last <- table(as.numeric(last))
    count[i, dimnames(freq.last)[[1]]] <- freq.last
    if(sum(is.na(count[i, ])) < 10) count[i, is.na(count[i, ])] <- 0
    # Distance between last and penultimate digits
    penult <- get.digits(v[id == id.uni[i]], pos="penult")
    tolast <- abs(as.numeric(last) - as.numeric(penult))
    tolast[tolast > 5 & !is.na(tolast)] <- 10 - tolast[tolast > 5 & !is.na(tolast)]
    tolast <- table(tolast)
    distance[i, dimnames(tolast)[[1]]] <- tolast
    if(sum(is.na(distance[i, ])) < 6) distance[i, is.na(distance[i, ])] <- 0
    id.out <- c(id.out, id.uni[i])
  }
  # Compute proportions and test statistics
  prop <- t(apply(count, 1, function(x) x / sum(x)))
  n <- apply(count, 1, function(x) sum(x))
  chi <- apply(count, 1, function(x) sum((x - .1 * sum(x))^2 / (.1 * sum(x))))
  pval <- 1 - pchisq(chi, 9)
  distance.prop <- t(apply(distance, 1, function(x) x / sum(x)))
  expect <- c(.1, .2, .2, .2, .2, .1)
  distance.chi <- apply(distance, 1, function(x) sum((x - expect * sum(x))^2 / (expect * sum(x))))
  distance.pval <- 1 - pchisq(distance.chi, 5)
  # If there are multiple strata, compute proportions and test statistics across all of them
  if(length(id.uni) > 1) {
    all.count <- apply(count, 2, sum)
    all.prop <- all.count / sum(n)
    all.n <- sum(all.count)
    all.chi <- sum((all.count - .1 * all.n)^2 / (.1 * all.n))
    all.pval <- 1 - pchisq(all.chi, 9)
    all.distance <- apply(distance, 2, sum)
    all.distance.prop <- all.distance / sum(n)
    all.distance.chi <- sum((all.distance - expect * all.n)^2 / (expect * all.n))
    all.distance.pval <- 1 - pchisq(all.distance.chi, 5)
    out <- list(count=count, prop=prop, chi=chi, pval=pval, n=n, id=id.out,
                distance=distance, distance.prop=distance.prop, distance.chi=distance.chi,
                distance.pval=distance.pval, all.count=all.count, all.prop=all.prop,
                all.n=all.n, all.chi=all.chi, all.pval=all.pval, all.distance=all.distance,
                all.distance.prop=all.distance.prop, all.distance.chi=all.distance.chi,
                all.distance.pval=all.distance.pval)
  } else {
    out <- list(count=count, prop=prop, chi=chi, pval=pval, n=n, id=id.out,
                distance=distance, distance.prop=distance.prop,
                distance.chi=distance.chi, distance.pval=distance.pval)
  }
  return(out)
}



digits1 <- get.digits(tax$tax_open)
digits2 <- get.digits(tax$tax_tamisemi)

dig.test1 <- digit.test(digits1)
dig.test2 <- digit.test(digits2)

barplot(dig.test2$count, xlab = "Digits", ylab= "Frequency", col = "#CCCCCC", ylim = c(0,30)) 
abline(h = 18, col = "red")

 barplot(dig.test1$count, xlab = "Digits", ylab= "Frequency", col = "#CCCCCC", ylim = c(0,30)) 
 abline(h = 18, col = "red")
 

 