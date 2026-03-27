# CGE sensitivity analysis
dat <- read.csv("../exportdata//cge_sensitivity.csv")

dat <- subset(dat, scenario != "rev_ori_kl_e")

library(ggplot2)

dat$scenario <- factor(dat$scenario, labels=c(
  "Base",
  "High trade elas",
  "Low trade elas",
  "DKS-KE_L",
  "DKS-KL_E",
  "OB-KE_L"))

# With full names
sector_names <- data.frame(sector = 
                             c("GAS","CRU","COL","OIL","ELE","AGR","MIN","CON","PPP","PRM","CHM","CEM","MFR","TRD","TRN","SER","GOV"),
                           sector_name =
                             c("Natural gas","Crude oil","Coal","Refined oil products","Electricity","Agriculture, fish, forests",
                               "Mining","Construction","Pulp and paper", "Primary metals", "Chemicals","Cement", "Other manufacturing",
                               "Retail and wholesale trade", "Transport", "Services", "Government"))

# Just armington
datarm <- subset(dat, scenario %in% c("Base", "High trade elas", "Low trade elas"))

datarm <- subset(datarm, item == "Employment")
datarm$sector <- toupper(datarm$sector)
datarm <- merge(datarm, sector_names, by="sector")
ggplot(datarm, aes(x=sector_name, y=value, group = scenario, colour=scenario, shape=scenario)) + geom_point() + coord_flip() +
  xlab("") + ylab("Change in employment in percentage points") + theme_bw() + geom_hline(yintercept=0)

datprd <- subset(dat, scenario %in% c("Base", "DKS-KE_L", "DKS-KL_E", "OB-KE_L"))
datprd$scenario <- factor(datprd$scenario, labels=c(
  "OB-KL_E", "DKS-KE_L", "DKS-KL_E", "OB-KE_L"))
datprd$sector <- toupper(datprd$sector)
datprd <- merge(datprd,sector_names,by="sector")

datprd <- subset(datprd, item == "Employment")
ggplot(datprd, aes(x=sector_name, y=value, group = scenario, colour=scenario, shape=scenario)) + geom_point() + coord_flip() +
  xlab("") + ylab("Change in employment in percentage points") + theme_bw() + geom_hline(yintercept=0)
