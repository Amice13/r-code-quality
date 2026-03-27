## Installing necessary packages ----
packages <-c(
  "ggplot2", "foreign", "data.table", "stargazer", "lfe", "stringr", "xtable", 
  "tidyverse", "lme4", "arm", "memisc", "vegan", "MASS", "haven", "zoo")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}
## ----
library(data.table)
library(xtable)
load("~/Dropbox/Projects/BKN/Data/compact_data/bkd_cd.rdata")

# tahun_cpns is cohort year
setDT(bkn_cd)
bkn_small <- bkn_cd[tahun_cpns %in% seq(1995, 2004, 1)]
bkn_small$age_entered <- bkn_small$umur_skrg - (2018 - bkn_small$tahun_cpns)
# avg age, prop men, muslim, protestant, catholic, hindu, confucian, other, avg ed
a24 <- bkn_small[, .(
  `Avg. Age` = mean(age_entered, na.rm = TRUE),
  `Prop. Men` = sum(gender == "M") / .N,
  `Prop Muslim` = sum(religion == "ISLAM") / .N,
  `Prop. Protestant` = sum(religion == "KRISTEN") / .N,
  `Prop. Catholic` = sum(religion == "KATHOLIK") / .N,
  `Prop. Hindu` = sum(religion == "HINDU") / .N,
  `Prop. Confucian` = sum(religion_cd == 6) / .N,
  `Prop. Other` = sum(religion_cd == 7) / .N,
  `Avg. Ed.` = mean(edulevel_cd, na.rm = TRUE)
), by = tahun_cpns][order(tahun_cpns)]
tab <- xtable(a24, digits = 3)
print(tab, file = "appendix_table_a24_cohort_comp.tex")

a24[, cohort1 := ifelse(tahun_cpns < 2000, 1, 0)]


ttest <- function(x){
  var <- paste0("`", paste0(noquote(x), "`"))
  var1 <- noquote(paste0("a24$", var))
  test <- t.test(eval(parse(text = var1)) ~ a24$cohort1)
  #test <- t.test(a24$`Prop. Men` ~ a24$cohort1)
  dif <- test$estimate[1] - test$estimate[2]
  pval <- test$p.value
  data.table(Variable = var, "p-value" = pval, Difference = dif)
}

cols <- paste0("a24$", paste0(paste0("`", colnames(a24)[2:10]), "`"))
a25 <- do.call(rbind, lapply(colnames(a24)[2:10], ttest))
tab <- xtable(a25, digits = 3)
print(tab, file = "appendix_table_a25_ttest.tex")