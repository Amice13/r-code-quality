df <- tibble::tribble(
  ~Issue, ~moralContent_perc, ~moralFrames_perc,
  "Same-sex Marriage Ban",              17.17,                15,
  "Physician-assisted Suicide",               16.6,                22,
  "Medical Marijuana",               14.8,                 0,
  "Death Penalty",              13.09,                28,
  "Tribal Gaming",              11.96,                 0,
  "Right to Work",              11.29,                23,
  "Abortion Funds",               9.14,                 7,
  "Recreational Marijuana",               8.74,                 2,
  "English as Official Language",               7.81,                 0,
  "Minimum Wage",               5.89,                 4,
  "Property Tax Limits",               5.65,                 0
)



with(df, cor.test(moralContent_perc,moralFrames_perc), method = "spearman")
# Pearson's product-moment correlation
# 
# data:  moralContent_perc and moralFrames_perc
# t = 1.8972, df = 9, p-value = 0.09029
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.0962473  0.8589582
# sample estimates:
#      cor 
# 0.534487 

# Visualize the results
# library(ggplot2)
# 
# ggplot(df, aes(moralContent_perc, moralFrames_perc)) +
#   geom_point()
