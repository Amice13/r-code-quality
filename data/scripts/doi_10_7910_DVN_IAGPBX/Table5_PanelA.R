rm(list = ls())
library("readr")
library("zoo")
library("dplyr")
library("ggplot2")
library("reshape2")
library("ggpubr")

combined <- read_csv("combinedVariables.csv")
colnames(combined)[1] <- "Bank"

tb <- summarise(group_by(combined, date),
                mean_c = mean(concentration, na.rm = TRUE),
                stdev_c = sd(concentration, na.rm = TRUE),
                mean_s = mean(similarity, na.rm = TRUE),
                stdev_s = sd(similarity, na.rm = TRUE),
                mean_ch = mean(chbal, na.rm = TRUE),
                stdev_ch = sd(chbal, na.rm = TRUE),
                mean_r = mean(ret, na.rm = TRUE),
                stdev_r = sd(ret, na.rm = TRUE),
                mean_l = mean(lnlsnet, na.rm = TRUE),
                stdev_l = sd(lnlsnet, na.rm = TRUE),
                mean_f = mean(frepo, na.rm = TRUE),
                stdev_f = sd(frepo, na.rm = TRUE),
                mean_sc = mean(sc, na.rm = TRUE),
                stdev_sc = sd(sc, na.rm = TRUE),
                n = length(unique(Bank[!is.na(concentration)])))

tb$lower_c <- tb$mean_c -3*sqrt(tb$mean_c * (1 - tb$mean_c))/sqrt(tb$n)
tb$upper_c <- tb$mean_c +3*sqrt(tb$mean_c * (1 - tb$mean_c))/sqrt(tb$n)
tb$lower_s <- tb$mean_s -3*sqrt(tb$mean_s * (1 - tb$mean_s))/sqrt(tb$n)
tb$upper_s <- tb$mean_s +3*sqrt(tb$mean_s * (1 - tb$mean_s))/sqrt(tb$n)
tb$lower_ch <- tb$mean_ch -3*sqrt(tb$mean_ch * (1 - tb$mean_ch))/sqrt(tb$n)
tb$upper_ch <- tb$mean_ch +3*sqrt(tb$mean_ch * (1 - tb$mean_ch))/sqrt(tb$n)
tb$lower_sc <- tb$mean_sc -3*sqrt(tb$mean_sc * (1 - tb$mean_sc))/sqrt(tb$n)
tb$upper_sc <- tb$mean_sc +3*sqrt(tb$mean_sc * (1 - tb$mean_sc))/sqrt(tb$n)
tb$lower_l <- tb$mean_l -3*sqrt(tb$mean_l * (1 - tb$mean_l))/sqrt(tb$n)
tb$upper_l <- tb$mean_l +3*sqrt(tb$mean_l * (1 - tb$mean_l))/sqrt(tb$n)
tb$lower_f <- tb$mean_f -3*sqrt(tb$mean_f * (1 - tb$mean_f))/sqrt(tb$n)
tb$upper_f <- tb$mean_f +3*sqrt(tb$mean_f * (1 - tb$mean_f))/sqrt(tb$n)
tb$lower_r <- tb$mean_r -3*tb$stdev_r/sqrt(tb$n)
tb$upper_r <- tb$mean_r +3*tb$stdev_r/sqrt(tb$n)

combined <- inner_join(combined, tb)

combined <- combined[combined$date >= "2023-01-01" & combined$date <= "2023-03-08",]

tb <- summarise(group_by(combined, Bank),#
                ret = mean(ret, na.rm = TRUE),
                combined_sim_conc = mean((concentration - upper_c) + (lower_s - similarity), na.rm = TRUE),
                combined_sim_conc_chbal = mean((concentration - upper_c) + (lower_ch - chbal) + (lower_s - similarity), na.rm = TRUE),
                combined_all = mean((concentration - upper_c) + (lower_ch - chbal) + (lower_s - similarity) + max((lnlsnet - upper_l), (sc - upper_sc), (frepo - upper_f)), na.rm = TRUE),
                # concentration = mean(concentration, na.rm = TRUE),
                # disimilarity = mean(1-similarity, na.rm = TRUE),
                # chbal = mean(chbal, na.rm = TRUE),
                concentration_dev = mean(concentration - upper_c, na.rm = TRUE),
                disimilarity_dev = mean(lower_s-similarity, na.rm = TRUE),
                chbal_dev = mean(lower_ch - chbal, na.rm = TRUE),
                lnlsnet_dev = mean(lnlsnet - upper_l, na.rm = TRUE),
                sc_dev = mean(sc - upper_sc, na.rm = TRUE),
                frepo_dev = mean(frepo - upper_f, na.rm = TRUE)
)
tb[is.na(tb)] <- 0
colnames(tb)[2] <- "Average Returns"
write.table(tb, "riskyBanks_20230308.csv", sep=",", row.names = FALSE, col.names = TRUE)

