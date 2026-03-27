# Calculate predicted values from regressions
library(tidyverse)
library(gridExtra)
# Merge with emissions intensity
#     Direct plus own-province indirect emissions
ghgint <- read.csv("../cge_results/ghg_intensity.csv")

output <- read.csv("../cge_results/benchmark_output.csv")

estimates <- data.frame(cbind(l=c(0.00107,-0.00524),y=c(0.00453,-0.0152),x=c(0.004,-0.026)),row.names=c("tax","tax_times_ei"))

predict <- data.frame(cbind(
  employment=100*(exp(30 * (estimates$l[1] + estimates$l[2] *ghgint$ghgint))-1),
  output=100*(exp(30 * (estimates$y[1] + estimates$y[2] *ghgint$ghgint))-1),
  exports=100*(exp(30 * (estimates$x[1] + estimates$x[2] *ghgint$ghgint))-1)
))
predict$sector <- ghgint$sector

library(reshape2)
pl <- melt(predict)
pl <- subset(pl, variable == "employment")

cge <- read.csv("../exportdata//cge_sensitivity.csv")
cge$sector <- toupper(cge$sector)
cge <- subset(cge, scenario == "rev", select = -1)
names(cge)[1] = "variable"
cge <- subset(cge, variable == "Employment", select = c("sector","value"))
cge <- cge %>% rename(cge = value)
pl <- pl %>% select(sector, econometric = value)
dat <- inner_join(cge,pl)
dat <- inner_join(dat,output)

sector_names <- data.frame(sector = 
                             c("GAS","CRU","COL","OIL","ELE","AGR","MIN","CON","PPP","PRM","CHM","CEM","MFR","TRD","TRN","SER","GOV"),
                           sector_name =
                             c("Natural gas","Crude oil","Coal","Refined oil products","Electricity","Agriculture, fish, forests",
                               "Mining","Construction","Pulp and paper", "Primary metals", "Chemicals","Cement", "Other manufacturing",
                               "Retail and wholesale trade", "Transport", "Services", "Government"))

dat <- inner_join(dat, sector_names)

# ordered by ghg intensity
dat <- inner_join(dat, ghgint)
p1 <- ggplot(dat) + 
  geom_point(aes(x=reorder(sector_name,ghgint),y=cge), size=2, shape=1,colour="red") + 
  geom_point(aes(x=reorder(sector_name,ghgint),y=econometric),size=2, shape=2,colour="blue") +
  theme_bw() + xlab("") + ylab("Change in employment in percent") + coord_flip() +
  geom_hline(yintercept=0) +
  annotate("text", x="Primary metals", y=dat$econometric[dat$sector=="PRM"] + 1, label="Econometric", colour="blue",hjust=0) +
  annotate("text", x="Primary metals", y=dat$cge[dat$sector=="PRM"] -1, label="CGE", colour="red",hjust=1)
p2 <- ggplot(dat, aes(x=reorder(sector,ghgint),y=output)) + geom_bar(stat="identity") + coord_flip() +
  theme_bw() + xlab("") + ylab("Value of output ($)") +
  theme(axis.text.y=element_blank())
p3 <- ggplot(dat, aes(x=reorder(sector_name,ghgint),y=ghgint)) + geom_point() +
  coord_flip() + theme_bw() + xlab("") + ylab("GHG intensity (kg/$)") +
  theme(axis.text.y=element_blank())

grid.arrange(p1, p2, p3, ncol = 3, widths=c(3,1,1))
dev.copy(png,filename="cge_econ_predict_employment_order_ghgint.png",width=700,height=500)
dev.off ()
