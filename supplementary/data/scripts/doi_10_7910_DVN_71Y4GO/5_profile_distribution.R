data.HOR.2017 <- read.csv("data_HOR_2017.csv")

# this function rounds the distribution proportions to a third decimal place so that the total sums to 1
prop.table.sum.to.one <- function(x) {
  prop <- prop.table(table(x))
  prop.rounded <- round(prop, 3)
  margin <- prop - prop.rounded
  sum.prop.rounded <- sum(prop.rounded)
  n.adjusted <- as.integer(abs(1 - sum.prop.rounded) / 0.001)
  if (sum.prop.rounded < 1) {
    out <- prop.rounded + (length(margin) - rank(margin) < n.adjusted) * 0.001
  } else if (sum.prop.rounded > 1) {
    out <- prop.rounded - (rank(margin) < n.adjusted) * 0.001
  } else {
    out <- prop.rounded
  }
  names(out) <- names(prop)
  out
}

# gender
prop.table.sum.to.one(data.HOR.2017$gender)

# age (percentiles)
quantile(data.HOR.2017$age, probs = c(0.05, 0.2, 0.5, 0.8, 0.95), na.rm = TRUE)

# educational attainment
prop.table.sum.to.one(data.HOR.2017$education)

# prior occupation
prop.table.sum.to.one(data.HOR.2017$occupation)

# dynasty
prop.table.sum.to.one(data.HOR.2017$dynasty)

# experience as a legislator
data.HOR.2017$experience <- ifelse(data.HOR.2017$terms > 2, 
                                   3, data.HOR.2017$terms)
prop.table.sum.to.one(data.HOR.2017$experience)

# party affiliation
data.HOR.2017$party.2 <- data.HOR.2017$party
data.HOR.2017$party.2[data.HOR.2017$party.2 == "POH"] <- "CDP"
data.HOR.2017$party.2[data.HOR.2017$party.2 == "Other"] <- NA

prop.table.sum.to.one(data.HOR.2017$party.2)