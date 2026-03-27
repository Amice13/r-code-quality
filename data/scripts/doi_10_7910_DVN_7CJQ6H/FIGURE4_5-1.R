# Comparison between linear and non-parametric regression
sector <- c("AGR","CHM","CRU","ELE","GOV","MFR","MIN","OIL","PPP","PRM","SER","TRD","TRN")
coef_np <- c(0.00147,-0.0197,-0.00365,-0.00528,0.00253,0.00137,0.00889,-0.00178,-0.0000231,-0.00117,-0.000824,-0.00200,-0.000994)
se_np <- c(0.00513,0.00696,0.00625,0.00345,0.00293,0.00303,0.00388,0.00694,0.00314,0.00437,0.00270,0.00304,0.00299)
np <- data.frame(cbind(sector,coef_np,se_np))

ghgint <- read.csv("../cge_results/ghg_intensity.csv")

np <- merge(np,ghgint, by="sector")

np$estimate_linear <- 100*(exp(30 * (0.0011 - 0.0052 * np$ghgint))-1)
np$estimate_np <- 100*(exp(30 * coef_np) -1)
np$se_np <- 100*(exp(30 * se_np) -1)
np$lo_np <- np$estimate_np - 1.96 * np$se_np
np$hi_np <- np$estimate_np + 1.96 * np$se_np

# With full names
sector_names <- data.frame(sector = 
                             c("GAS","CRU","COL","OIL","ELE","AGR","MIN","CON","PPP","PRM","CHM","CEM","MFR","TRD","TRN","SER","GOV"),
                           sector_name =
                             c("Natural gas","Crude oil","Coal","Refined oil products","Electricity","Agriculture, fish, forests",
                               "Mining","Construction","Pulp and paper", "Primary metals", "Chemicals","Cement", "Other manufacturing",
                               "Retail and wholesale trade", "Transport", "Services", "Government"))

np <- merge(np,sector_names, by="sector")
library(ggplot2)
library(ggrepel)
ggplot(np, aes(x=ghgint, y=estimate_np)) + geom_point() +
  geom_label_repel(data=np, aes(x=ghgint, y=estimate_np, label=sector)) +
  geom_errorbar(data=np, aes(ymin=lo_np, ymax=hi_np), colour="grey") +
  theme_bw() +
  geom_smooth(method="lm",se=F) +
  xlab("Greenhouse gas intensity (direct+indirect/GO) (t/$1000)") +
  ylab("Employment impact of carbon tax (%)")


# Graph to compmare non-parameteric to CGE
cge <- read.csv("../exportdata//cge_sensitivity.csv")
cge <- subset(cge, scenario == "rev", select = -1)
names(cge)[1] = "variable"
cge <- subset(cge, variable == "Employment", select = c("sector","value"))
dat <- merge(cge,np)
library(ggplot2)
library(ggrepel)
ggplot(dat, aes(x=estimate_np, y=value, group=sector, label=sector)) + geom_point(colour="green") + geom_text_repel() +
  theme_bw() + xlab("Percent change in employment from econometric model") +
  ylab("Percent change in employment from CGE model") +
  geom_abline(intercept=0, slope=1, linetype="dashed") +
  xlim(c(-45,40)) + ylim(c(-45,40))
