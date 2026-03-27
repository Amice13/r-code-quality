
################
### FIGURE 2 ###
################

rm(list=ls())
library(xlsx)
library(formattable)
library(ggplot2)
library(readstata13)

setwd("/Users/samtrachtman/Dropbox/ACA Study/Projects/Public Private Field Experiment/APSR Production/replication materials/submission_output/kff/")
rm(list=ls())

data.b = read.xlsx(file = "kff_output_collected_6-8.xlsx", sheetName = "base_model", rowIndex = 2:25, colIndex = 1:13)
unins.est = data.b$Estimate[2]
unins.ci.low = data.b$Estimate[2] - 1.96*data.b$SE[2]
unins.ci.high = data.b$Estimate[2] + 1.96*data.b$SE[2]

market.est = data.b$Estimate.1[2]
market.ci.low = data.b$Estimate.1[2] - 1.96*data.b$SE.1[2]
market.ci.high = data.b$Estimate.1[2] + 1.96*data.b$SE.1[2]

priv.est = data.b$Estimate.2[2]
priv.ci.low = data.b$Estimate.2[2] - 1.96*data.b$SE.2[2]
priv.ci.high = data.b$Estimate.2[2] + 1.96*data.b$SE.2[2]

type = c("Uninsured", "Marketplace", "Private")
value = c(unins.est, market.est, priv.est)
lower.conf = c(unins.ci.low, market.ci.low, priv.ci.low)
upper.conf = c(unins.ci.high, market.ci.high, priv.ci.high)
x = c(1,2,3)
plotdata = data.frame(type, value, lower.conf, upper.conf, x)

##setwd("/Users/samtrachtman/Dropbox/ACA Study/Projects/Public Private Field Experiment/APSR Production/submission_results/kff/")
tiff(filename = "figure2.tif")
ggplot(plotdata, aes(x = x, y = value))+
  geom_point(size=3) +
  geom_linerange(aes(ymin=lower.conf, ymax=upper.conf)) + 
  scale_x_continuous(breaks = 1:3, labels = c("Uninsured", "Marketplace", "Private"))+
  labs(x = "", y = "Marginal Effect of Republican Partisanship")  +
  scale_y_continuous(limits = c(-.2,.2), labels = scales::percent)+
  theme_bw()+
  geom_line(y = 0, col = "black", linetype = 2)+
  theme(axis.title.y = element_text(size = rel(1.3), angle = 90))+
  theme(axis.text.x = element_text(size = rel(1.3)))
dev.off()


  



