rm(list = ls())
library("readr")
library("zoo")
library("dplyr")
library("ggplot2")
library("reshape2")
library("ggpubr")

combined <- read_csv("combinedVariables.csv")
colnames(combined)[1] <- "Bank"

tickers <- c("SI","SIVB", "SBNY", "FRC")

combined_fig <- combined
combined_fig$Bank <- factor(combined_fig$Bank, levels = c(tickers, setdiff(unique(combined_fig$Bank), tickers)))

tb <- summarise(group_by(combined, date),
                mean = mean(concentration, na.rm = TRUE),
                stdev = sd(concentration, na.rm = TRUE),
                n = length(unique(Bank[!is.na(concentration)])))
tb$lower <- tb$mean -2.576*sqrt(tb$mean * (1 - tb$mean))/sqrt(tb$n)
tb$upper <- tb$mean +2.576*sqrt(tb$mean * (1 - tb$mean))/sqrt(tb$n)


gp1 <- ggplot(combined_fig[combined_fig$Bank %in% tickers,], 
             aes(date, concentration, group = Bank, colour = Bank, pch = Bank, lty = Bank)) + 
  geom_point() + geom_line() + theme_bw() + ylab("Concentration") + xlab("Date") + theme(legend.position = "bottom") + 
  geom_ribbon(aes(x=date, y=mean, ymax=upper, ymin=lower, fill = "99% CI for\nMedium Banks"), alpha=0.2, inherit.aes = FALSE, data = tb) + 
  scale_fill_manual(values=c("grey10"), name="fill")  + guides(fill=guide_legend(title=""), lty=guide_legend(title=""), 
                                                               pch=guide_legend(title=""), colour=guide_legend(title=""))

tb <- summarise(group_by(combined, date),
                mean = mean(similarity, na.rm = TRUE),
                stdev = sd(similarity, na.rm = TRUE),
                n = length(unique(Bank[!is.na(similarity)])))
tb$lower <- tb$mean -2.576*sqrt(tb$mean * (1 - tb$mean))/sqrt(tb$n)
tb$upper <- tb$mean +2.576*sqrt(tb$mean * (1 - tb$mean))/sqrt(tb$n)

gp2 <- ggplot(combined_fig[combined_fig$Bank %in% tickers,], 
             aes(date, similarity, group = Bank, colour = Bank, pch = Bank, lty = Bank)) + 
  geom_point() + geom_line() + theme_bw() + ylab("Similarity") + xlab("Date") + theme(legend.position = "bottom") + 
  geom_ribbon(aes(x=date, y=mean, ymax=upper, ymin=lower, fill = "99% CI for\nMedium Banks"), alpha=0.2, inherit.aes = FALSE, data = tb) + 
  scale_fill_manual(values=c("grey10"), name="fill")  + guides(fill=guide_legend(title=""), lty=guide_legend(title=""), 
                                                               pch=guide_legend(title=""), colour=guide_legend(title=""))
  

tb <- summarise(group_by(combined, date),
                mean = mean(frepo, na.rm = TRUE),
                stdev = sd(frepo, na.rm = TRUE),
                n = length(unique(Bank[!is.na(frepo)])))
tb$lower <- tb$mean -2.576*sqrt(tb$mean * (1 - tb$mean))/sqrt(tb$n)
tb$upper <- tb$mean +2.576*sqrt(tb$mean * (1 - tb$mean))/sqrt(tb$n)

gp3 <- ggplot(combined_fig[combined_fig$Bank %in% tickers,], 
             aes(date, frepo, group = Bank, colour = Bank, pch = Bank, lty = Bank)) + 
  geom_point() + geom_line() + theme_bw() + ylab("Repo") + xlab("Date") + theme(legend.position = "bottom") + 
  geom_ribbon(aes(x=date, y=mean, ymax=upper, ymin=lower, fill = "99% CI for\nMedium Banks"), alpha=0.2, inherit.aes = FALSE, data = tb) + 
  scale_fill_manual(values=c("grey10"), name="fill")  + guides(fill=guide_legend(title=""), lty=guide_legend(title=""), 
                                                               pch=guide_legend(title=""), colour=guide_legend(title=""))

tb <- summarise(group_by(combined, date),
                mean = mean(lnlsnet, na.rm = TRUE),
                stdev = sd(lnlsnet, na.rm = TRUE),
                n = length(unique(Bank[!is.na(lnlsnet)])))
tb$lower <- tb$mean -2.576*sqrt(tb$mean * (1 - tb$mean))/sqrt(tb$n)
tb$upper <- tb$mean +2.576*sqrt(tb$mean * (1 - tb$mean))/sqrt(tb$n)

gp4 <- ggplot(combined_fig[combined_fig$Bank %in% tickers,], 
             aes(date, lnlsnet, group = Bank, colour = Bank, pch = Bank, lty = Bank)) + 
  geom_point() + geom_line() + theme_bw() + ylab("Loans") + xlab("Date") + theme(legend.position = "bottom") + 
  geom_ribbon(aes(x=date, y=mean, ymax=upper, ymin=lower, fill = "99% CI for\nMedium Banks"), alpha=0.2, inherit.aes = FALSE, data = tb) + 
  scale_fill_manual(values=c("grey10"), name="fill")  + guides(fill=guide_legend(title=""), lty=guide_legend(title=""), 
                                                               pch=guide_legend(title=""), colour=guide_legend(title=""))

tb <- summarise(group_by(combined, date),
                mean = mean(chbal, na.rm = TRUE),
                stdev = sd(chbal, na.rm = TRUE),
                n = length(unique(Bank[!is.na(chbal)])))
tb$lower <- tb$mean -2.576*sqrt(tb$mean * (1 - tb$mean))/sqrt(tb$n)
tb$upper <- tb$mean +2.576*sqrt(tb$mean * (1 - tb$mean))/sqrt(tb$n)

gp5 <- ggplot(combined_fig[combined_fig$Bank %in% tickers,], 
             aes(date, chbal, group = Bank, colour = Bank, pch = Bank, lty = Bank)) + 
  geom_point() + geom_line() + theme_bw() + ylab("Cash") + xlab("Date") + theme(legend.position = "bottom") + 
  geom_ribbon(aes(x=date, y=mean, ymax=upper, ymin=lower, fill = "99% CI for\nMedium Banks"), alpha=0.2, inherit.aes = FALSE, data = tb) + 
  scale_fill_manual(values=c("grey10"), name="fill")  + guides(fill=guide_legend(title=""), lty=guide_legend(title=""), 
                                                               pch=guide_legend(title=""), colour=guide_legend(title=""))

tb <- summarise(group_by(combined, date),
                mean = mean(sc, na.rm = TRUE),
                stdev = sd(sc, na.rm = TRUE),
                n = length(unique(Bank[!is.na(sc)])))
tb$lower <- tb$mean -2.576*sqrt(tb$mean * (1 - tb$mean))/sqrt(tb$n)
tb$upper <- tb$mean +2.576*sqrt(tb$mean * (1 - tb$mean))/sqrt(tb$n)

gp6 <- ggplot(combined_fig[combined_fig$Bank %in% tickers,], 
             aes(date, sc, group = Bank, colour = Bank, pch = Bank, lty = Bank)) + 
  geom_point() + geom_line() + theme_bw() + ylab("Securities") + xlab("Date") + theme(legend.position = "bottom") + 
  geom_ribbon(aes(x=date, y=mean, ymax=upper, ymin=lower, fill = "99% CI for\nMedium Banks"), alpha=0.2, inherit.aes = FALSE, data = tb) + 
  scale_fill_manual(values=c("grey10"), name="fill")  + guides(fill=guide_legend(title=""), lty=guide_legend(title=""), 
                                                               pch=guide_legend(title=""), colour=guide_legend(title=""))

figure <- ggarrange(gp1, gp2,nrow = 2, common.legend = TRUE, legend = "bottom")
pdf("Index_caseStudy.pdf", width = 6.5, height = 4)
print(figure)
dev.off()

figure <- ggarrange(gp5, gp4,gp3, gp6,ncol=2, nrow = 2, 
                    common.legend = TRUE, legend = "bottom")
pdf("bsPerc_caseStudy.pdf", width = 6.5, height = 4)
print(figure)
dev.off()

