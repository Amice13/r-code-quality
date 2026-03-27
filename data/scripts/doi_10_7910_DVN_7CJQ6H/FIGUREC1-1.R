# Comparison of CGE and Econometric results
cge <- read.csv("../exportdata//cge_sensitivity.csv")
cge$sector <- toupper(cge$sector)
cge <- subset(cge, scenario == "rev", select = -1)
cge <- subset(cge, item == "Employment")
ghg_int <- read.csv("../cge_results/ghg_intensity.csv")

cge <- merge(cge, ghg_int, by="sector")

library(ggplot2)
library(ggrepel)
# Compare alternative GHG intensity metrics
ghg_int_all <- read.csv("../emission_intensity/ghg_int_allmeasures.csv", stringsAsFactors = F)

cge <- read.csv("../exportdata//cge_sensitivity.csv")
cge$sector <- toupper(cge$sector)
cge <- subset(cge, scenario == "rev", select = -1)
cge <- subset(cge, item == "Employment")
cge <- merge(cge, ghg_int_all, by="sector")
library(reshape2)
cge <- melt(cge, id.vars = c(1:3), value.name = "ghgint")

# With full names
sector_names <- data.frame(sector = 
                             c("GAS","CRU","COL","OIL","ELE","AGR","MIN","CON","PPP","PRM","CHM","CEM","MFR","TRD","TRN","SER","GOV"),
                           sector_name =
                             c("Natural gas","Crude oil","Coal","Refined oil products","Electricity","Agriculture, fish, forests",
                               "Mining","Construction","Pulp and paper", "Primary metals", "Chemicals","Cement", "Other manufacturing",
                               "Retail and wholesale trade", "Transport", "Services", "Government"))

variable_names <- data.frame(variable =
                               c("dir_GDP", "dir_GO", "indir_GDP", "indir_GO"),
                             variable_name = 
                               c("Direct GHG per $GDP", "Direct GHG per $GO", "Direct plus indirect GHG per $GDP", "Direct plus indirect GHG per $GO"))

cge <- merge(cge, sector_names, by="sector")
cge <- merge(cge, variable_names, by="variable")

ggplot(cge, aes(x=ghgint, y=value, label=sector)) + geom_point(colour="red") + geom_label_repel(size=2) + theme_bw() +
  geom_smooth(method="lm", se=F) + facet_wrap(~variable_name, scales = "free_x") + ylab("Change in employment (%)") +
  xlab("Greenhouse gas intensity (t/$1,000)")