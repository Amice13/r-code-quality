library(stringdist)

dat = data.frame(str1 = c("JP Morgan Chase", "JP Morgan Chase", "JP Morgan Chase", "JP Morgan Chase"),
                 str2 = c("JP Morgan Chase", "J.P. Morgan", "JPM & Co", "Bank of America"),
                 label = c(1,1,1,0))

dat$leven = stringdist(dat$str1, dat$str2, method = "lv")
dat$jaccard = stringdist(dat$str1, dat$str2, method = "jaccard")
dat$jw = stringdist(dat$str1, dat$str2, method = "jw")
dat$cos = stringdist(dat$str1, dat$str2, method = "cosine")

writeLines(print(xtable::xtable(dat, digits=2), include.rownames=F), "table1.tex")
