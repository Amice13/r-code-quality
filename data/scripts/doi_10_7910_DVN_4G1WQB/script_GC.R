#Import dataset -----
library(readr)
dataset <- read_delim(
  "dados.csv",
  delim = ";", 
  escape_double = FALSE, 
  trim_ws = TRUE)

#Harman's one-factor test -----
library(psych)
pca <- pca(
  dataset [,2:10], 
  nfactor=1, 
  cor="poly", 
  fm="wls", 
  rotate = "varimax")
pca$loadings # Cumulative variance < 50%

#Descriptive -----
library(jmv)
descriptives(
  data = dataset,
  vars = vars(SA1, SA2, SA3, IC1, IC2, IC3, GK1, GK2, GK3),
  desc = "rows",
  n = FALSE,
  missing = FALSE,
  median = FALSE)

descriptives(
  data = dataset,
  vars = Group,
  freq = TRUE)

#Reliability -----
##SA
reliability(
  data = dataset,
  vars = vars(SA1, SA2, SA3),
  omegaScale = TRUE)

##IC
reliability(
  data = dataset,
  vars = vars(IC1, IC2, IC3),
  omegaScale = TRUE)

##GK
reliability(
  data = dataset,
  vars = vars(GK1, GK2, GK3),
  omegaScale = TRUE)

#Validity (outer loading) -----
pca2 <- pca(
  dataset [,2:10], 
  nfactor=3, 
  cor="poly", 
  fm="wls", 
  rotate = "varimax")
pca2$loadings

#Manova -----
##SA
mancova(
  data = dataset,
  deps = vars(SA1, SA2, SA3),
  factors = Group,
  multivar = "pillai",
  boxM = TRUE,
  shapiro = TRUE)

##IC
mancova(
  data = dataset,
  deps = vars(IC1, IC2, IC3),
  factors = Group,
  multivar = "pillai",
  boxM = TRUE,
  shapiro = TRUE)

##GK
mancova(
  data = dataset,
  deps = vars(GK1, GK2, GK3),
  factors = Group,
  multivar = "pillai",
  boxM = TRUE,
  shapiro = TRUE)

#Kruskal-Wallis and post-hoc test (IC1) -----
anovaNP(
  formula = IC1 ~ Group,
  data = dataset,
  es = TRUE,
  pairs = TRUE)

#Confidence interval of mean
descriptives(
  formula = IC1 ~ Group,
  data = dataset,
  desc = "rows",
  n = FALSE,
  missing = FALSE,
  median = FALSE,
  sd = FALSE,
  min = FALSE,
  max = FALSE,
  ci = TRUE,
  ciWidth = 95) # IC 95%

descriptives(
  formula = IC1 ~ Group,
  data = dataset,
  desc = "rows",
  n = FALSE,
  missing = FALSE,
  median = FALSE,
  sd = FALSE,
  min = FALSE,
  max = FALSE,
  ci = TRUE,
  ciWidth = 90) # IC 90%


