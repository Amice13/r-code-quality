### This file produces Figure 5: Intensive and Extensive Margin counterfactuals 

### Data Dependencies: 
#   CounterfactualResultsExtensive.dta
#   CounterfactualResultsIntensiveAll.dta
### Outputs: 
#   Figure5.pdf

rm(list = ls())

# Required Packages
library(readstata13)
library(ggplot2)
library(xtable)
library(dplyr)
require(logr)

# Set main Working Directory 
setwd("C:/Users/lnunez6/Dropbox/Documents/Loyalty and Defection/AJPS Replication Materials")

log09 <- file.path(getwd(),"Logs","09_Figure5_log.log")
log <- log_open(log09, logdir = FALSE)

all <- read.dta13("./Intermediate Data/CounterfactualResultsExtensive.dta")
tre <- read.dta13("./Intermediate Data/CounterfactualResultsIntensive.dta")

# Create function to reorganize the data
reorg <- function(data){
d <- data
dd <- matrix(colMeans(d), nrow = 3)
dm <- dd[, 1:5]
dv <- dd[, 6:10]
# Calculate changes relative to baseline. 
diff <-  dm[, -1]  - dm[, 1] 
vdiff <-   dv[,-1] + dv[,1]
sddiff <- sqrt(vdiff/nrow(d))
# Dataframe with changes relative to baseline
res <- data.frame(diff = c(diff), sd =  c(sddiff), outcome = rep(0:2, 4), counter = rep(1:4, each = 3))
res$upper <- res$diff +1.96*res$sd
res$lower <- res$diff -1.96*res$sd
res$outcome <- as.factor(res$outcome)
levels(res$outcome) <- c("Abstain", "Sincere", "Tactical") 
res$counter <- as.factor(res$counter)
levels(res$counter) <- c("(1) No Cont. Most Pref.", "(2) No Cont. Best Alt.", "(3) No Cont. Worst Alt.", "(4) No Contacts At All")
return(res)
}
# Apply reorganizing function to both margins
res <- reorg(all)
res$type <- "Extensive Margin"
res2 <- reorg(tre)
res2$type <- "Intensive Margin"

# Bind results for both margins
res3 <- rbind(res,res2) %>% put()



# Dodge to put Intensive and Extensive Margins side-by-side in figure
dodge <- position_dodge(width = 0.5)
# Produce plot with all panels together
p <- ggplot(data = res3, aes(x = outcome, y= diff)) + geom_point(aes(shape = type), position = dodge)  +
  geom_errorbar(aes(ymin = lower, ymax = upper, group = type), position = dodge, width = 0.2) +
   facet_grid(.~counter) + theme_bw() + geom_hline(yintercept = 0, color = "red", linetype = 2) +
  ylab("Change Relative to Observed") + theme(axis.title.x = element_blank()) + theme(legend.position = "bottom", legend.title = element_blank())
p

# Rename Margins: 
res3$type[res3$type == "Extensive Margin"] = "Extensive"
res3$type[res3$type == "Intensive Margin"] = "Intensive"
# Separate panels 
p1 <- ggplot(data = res3[res3$counter == "(1) No Cont. Most Pref.", ], aes(x = outcome, y= diff)) + geom_point(aes(shape = type), position = dodge)  +
  geom_errorbar(aes(ymin = lower, ymax = upper, group = type), position = dodge, width = 0.2) +
  theme_bw() + geom_hline(yintercept = 0, color = "black", linetype = 3)  + scale_y_continuous(limits = c(-0.07,0.07), breaks = c(-0.06,-0.03,0,0.03,0.06)) + 
  ylab("Change relative to observed") + theme(axis.title.x = element_blank()) + theme(legend.position = "bottom",legend.margin = margin(t = 0, r = 0, b = 0, l = -15), legend.title = element_blank(), ) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
p1

pdf("./Main Text Figures and Tables/Figure5a.pdf", width = 3, height = 3)
p1
dev.off()

p2 <- ggplot(data = res3[res3$counter == "(2) No Cont. Best Alt.", ], aes(x = outcome, y= diff)) + geom_point(aes(shape = type), position = dodge)  +
  geom_errorbar(aes(ymin = lower, ymax = upper, group = type), position = dodge, width = 0.2) +
  theme_bw() + geom_hline(yintercept = 0, color = "black", linetype = 3)  + scale_y_continuous(limits = c(-0.07,0.07), breaks = c(-0.06,-0.03,0,0.03,0.06)) + 
  ylab("Change relative to observed") + theme(axis.title.x = element_blank()) + theme(legend.position = "bottom", legend.margin = margin(t = 0, r = 0, b = 0, l = -15), legend.title = element_blank()) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
p2

pdf("./Main Text Figures and Tables/Figure5b.pdf", width = 3, height = 3)
p2
dev.off()


p3 <- ggplot(data = res3[res3$counter == "(3) No Cont. Worst Alt.", ], aes(x = outcome, y= diff)) + geom_point(aes(shape = type), position = dodge)  +
  geom_errorbar(aes(ymin = lower, ymax = upper, group = type), position = dodge, width = 0.2) +
  theme_bw() + geom_hline(yintercept = 0, color = "black", linetype = 3) + scale_y_continuous(limits = c(-0.07,0.07), breaks = c(-0.06,-0.03,0,0.03,0.06)) + 
  ylab("Change relative to observed") + theme(axis.title.x = element_blank()) + theme(legend.position = "bottom", legend.margin = margin(t = 0, r = 0, b = 0, l = -15), legend.title = element_blank()) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
p3

pdf("./Main Text Figures and Tables/Figure5c.pdf", width = 3, height = 3)
p3
dev.off()

p4 <- ggplot(data = res3[res3$counter == "(4) No Contacts At All", ], aes(x = outcome, y= diff)) + geom_point(aes(shape = type), position = dodge)  +
  geom_errorbar(aes(ymin = lower, ymax = upper, group = type), position = dodge, width = 0.2) +
  theme_bw() + geom_hline(yintercept = 0, color = "black", linetype = 3) + scale_y_continuous(limits = c(-0.07,0.07), breaks = c(-0.06,-0.03,0,0.03,0.06)) + 
  ylab("Change relative to observed") + theme(axis.title.x = element_blank()) + theme(legend.position = "bottom", legend.margin = margin(t = 0, r = 0, b = 0, l = -15), legend.title = element_blank()) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
p4 

pdf("./Main Text Figures and Tables/Figure5d.pdf", width = 3, height = 3)
p4
dev.off()



# Export FIGURE 5
pdf("./Main Text Figures and Tables/Figure5.pdf", width = 6.99, height = 4.25)
p
dev.off()
# Print results 
res[, c(1,2, 5, 6)] <- res[, c(1,2, 5, 6)]*100
print(xtable(res)) %>% put()

# Values of extensive margin for introduction: 
# For Most Preferred Party
-round(res$diff[res$counter == "(1) No Cont. Most Pref." & res$outcome == "Tactical"],1)
# For Best Alternative
-round(res$diff[res$counter == "(2) No Cont. Best Alt." & res$outcome == "Tactical"],1)
# For Not contact by any party 
-round(res$diff[res$counter == "(4) No Contacts At All" & res$outcome == "Tactical"],0)

log_code()
log_close()