## Author: Kabir Khanna
## Updated: December 19, 2015
## Note: First run Precincts.R

eth <- c("whi", "bla", "his", "asi", "oth")

## Census Racial Proportions by Gender by Precinct
load("df.RData")
keep.census.prc <- c("Precinct", paste("p_prc", eth[1:4], sep = "_"), 
                     paste("p_prc_fem", eth[1:4], sep = "_"), 
                     paste("p_prc_mal", eth[1:4], sep = "_"))
census.prc <- df[!duplicated(df$Precinct), keep.census.prc]
census.prc <- census.prc[order(census.prc$Precinct)] #8,828 precincts

## L2 Racial Proportions by Precinct
precincts <- unique(df$Precinct)
precincts <- precincts[order(precincts)]

tot <- tapply(!is.na(df$Precinct), df$Precinct, sum)[!is.na(tapply(!is.na(df$Precinct), df$Precinct, sum))]
whi <- tapply(df$SR.WHI, df$Precinct, sum)[!is.na(tapply(!is.na(df$Precinct), df$Precinct, sum))] / tot
bla <- tapply(df$SR.BLA, df$Precinct, sum)[!is.na(tapply(!is.na(df$Precinct), df$Precinct, sum))] / tot
his <- tapply(df$SR.HIS, df$Precinct, sum)[!is.na(tapply(!is.na(df$Precinct), df$Precinct, sum))] / tot
asi <- tapply(df$SR.ASI, df$Precinct, sum)[!is.na(tapply(!is.na(df$Precinct), df$Precinct, sum))] / tot

l2.prc <- as.data.frame(cbind(as.character(precincts), whi, bla, his, asi))
names(l2.prc) <- c("Precinct", eth[1:4])

## Females
df.fem <- df[df$Voters_Gender == "F", ]
precincts.fem <- unique(df.fem$Precinct)
precincts.fem <- precincts.fem[order(precincts.fem)] #8,769 precincts

fem.tot <- tapply(!is.na(df.fem$Precinct), df.fem$Precinct, sum)[!is.na(tapply(!is.na(df.fem$Precinct), df.fem$Precinct, sum))]
fem.whi <- tapply(df.fem$SR.WHI, df.fem$Precinct, sum)[!is.na(tapply(!is.na(df.fem$Precinct), df.fem$Precinct, sum))] / fem.tot
fem.bla <- tapply(df.fem$SR.BLA, df.fem$Precinct, sum)[!is.na(tapply(!is.na(df.fem$Precinct), df.fem$Precinct, sum))] / fem.tot
fem.his <- tapply(df.fem$SR.HIS, df.fem$Precinct, sum)[!is.na(tapply(!is.na(df.fem$Precinct), df.fem$Precinct, sum))] / fem.tot
fem.asi <- tapply(df.fem$SR.ASI, df.fem$Precinct, sum)[!is.na(tapply(!is.na(df.fem$Precinct), df.fem$Precinct, sum))] / fem.tot

l2.fem.prc <- as.data.frame(cbind(as.character(precincts.fem), fem.whi, fem.bla, fem.his, fem.asi))
names(l2.fem.prc) <- c("Precinct", paste("fem", eth[1:4], sep = "."))

## Males
df.mal <- df[df$Voters_Gender == "M", ]
precincts.mal <- unique(df.mal$Precinct)
precincts.mal <- precincts.mal[order(precincts.mal)] #8,787 precincts

mal.tot <- tapply(!is.na(df.mal$Precinct), df.mal$Precinct, sum)[!is.na(tapply(!is.na(df.mal$Precinct), df.mal$Precinct, sum))]
mal.whi <- tapply(df.mal$SR.WHI, df.mal$Precinct, sum)[!is.na(tapply(!is.na(df.mal$Precinct), df.mal$Precinct, sum))] / mal.tot
mal.bla <- tapply(df.mal$SR.BLA, df.mal$Precinct, sum)[!is.na(tapply(!is.na(df.mal$Precinct), df.mal$Precinct, sum))] / mal.tot
mal.his <- tapply(df.mal$SR.HIS, df.mal$Precinct, sum)[!is.na(tapply(!is.na(df.mal$Precinct), df.mal$Precinct, sum))] / mal.tot
mal.asi <- tapply(df.mal$SR.ASI, df.mal$Precinct, sum)[!is.na(tapply(!is.na(df.mal$Precinct), df.mal$Precinct, sum))] / mal.tot

l2.mal.prc <- as.data.frame(cbind(as.character(precincts.mal), mal.whi, mal.bla, mal.his, mal.asi))
names(l2.mal.prc) <- c("Precinct", paste("mal", eth[1:4], sep = "."))

## Merge L2 proportions together
l2.prc <- merge(l2.prc, l2.fem.prc, by = "Precinct", all.x = T)
l2.prc <- merge(l2.prc, l2.mal.prc, by = "Precinct", all.x = T)
l2.prc <- merge(l2.prc, census.prc, by = "Precinct", all.x = T)

for (j in 2:length(names(l2.prc))) {
  l2.prc[, names(l2.prc)[j]] <- as.numeric(as.character(l2.prc[, names(l2.prc)[j]]))
}


## Figure 3 Plots

png('figure3.png', width = 1600, height = 1300, res = 180)
par(mfrow = c(3, 4), mar = c(4.1, 4.6, 4.6, 2.1)) #bottom, left, top, right

plot(l2.prc$whi, l2.prc$p_prc_whi, pch = 20, cex = .5, col = rgb(0, 0, 0, alpha = .2), main = "", xlab = "", ylab = "")
lines(c(0, 1), c(0, 1), lty = 2)
mtext("Voter File", side = 1, line = 2, font = 1, adj = .5, cex = .75)
mtext("Census", side = 2, line = 2, font = 1, adj = .5, cex = .75)
mtext(bquote("R"^"2" ~ "=" ~ .(sub("0.", ".", sprintf("%.2f", round(cor(l2.prc$whi, l2.prc$p_prc_whi, use = "pairwise.complete.obs") ^ 2, 2))))), side = 3, line = 1, adj = .5, cex = .75)
mtext("Whites", side = 3, line = 3, font = 2)
mtext("All Observations", side = 2, line = 3, font = 2)

plot(l2.prc$bla, l2.prc$p_prc_bla, pch = 20, cex = .5, col = rgb(0, 0, 0, alpha = .2), main = "", xlab = "", ylab = "")
lines(c(0, 1), c(0, 1), lty = 2)
mtext("Voter File", side = 1, line = 2, font = 1, adj = .5, cex = .75)
mtext("Census", side = 2, line = 2, font = 1, adj = .5, cex = .75)
mtext(bquote("R"^"2" ~ "=" ~ .(sub("0.", ".", sprintf("%.2f", round(cor(l2.prc$bla, l2.prc$p_prc_bla, use = "pairwise.complete.obs") ^ 2, 2))))), side = 3, line = 1, adj = .5, cex = .75)
mtext("Blacks", side = 3, line = 3, font = 2)

plot(l2.prc$his, l2.prc$p_prc_his, pch = 20, cex = .5, col = rgb(0, 0, 0, alpha = .2), main = "", xlab = "", ylab = "")
lines(c(0, 1), c(0, 1), lty = 2)
mtext("Voter File", side = 1, line = 2, font = 1, adj = .5, cex = .75)
mtext("Census", side = 2, line = 2, font = 1, adj = .5, cex = .75)
mtext(bquote("R"^"2" ~ "=" ~ .(sub("0.", ".", sprintf("%.2f", round(cor(l2.prc$his, l2.prc$p_prc_his, use = "pairwise.complete.obs") ^ 2, 2))))), side = 3, line = 1, adj = .5, cex = .75)
mtext("Latinos", side = 3, line = 3, font = 2)

plot(l2.prc$asi, l2.prc$p_prc_asi, pch = 20, cex = .5, col = rgb(0, 0, 0, alpha = .2), main = "", xlab = "", ylab = "")
lines(c(0, 1), c(0, 1), lty = 2)
mtext("Voter File", side = 1, line = 2, font = 1, adj = .5, cex = .75)
mtext("Census", side = 2, line = 2, font = 1, adj = .5, cex = .75)
mtext(bquote("R"^"2" ~ "=" ~ .(sub("0.", ".", sprintf("%.2f", round(cor(l2.prc$asi, l2.prc$p_prc_asi, use = "pairwise.complete.obs") ^ 2, 2))))), side = 3, line = 1, adj = .5, cex = .75)
mtext("Asians", side = 3, line = 3, font = 2)

## Females
plot(l2.prc$fem.whi, l2.prc$p_prc_fem_whi, pch = 20, cex = .5, col = rgb(0, 0, 0, alpha = .2), main = "", xlab = "", ylab = "")
lines(c(0, 1), c(0, 1), lty = 2)
mtext("Voter File", side = 1, line = 2, font = 1, adj = .5, cex = .75)
mtext("Census", side = 2, line = 2, font = 1, adj = .5, cex = .75)
mtext(bquote("R"^"2" ~ "=" ~ .(sub("0.", ".", sprintf("%.2f", round(cor(l2.prc$fem.whi, l2.prc$p_prc_fem_whi, use = "pairwise.complete.obs") ^ 2, 2))))), side = 3, line = 1, adj = .5, cex = .75)
mtext("Females", side = 2, line = 3, font = 2)

plot(l2.prc$fem.bla, l2.prc$p_prc_fem_bla, pch = 20, cex = .5, col = rgb(0, 0, 0, alpha = .2), main = "", xlab = "", ylab = "")
lines(c(0, 1), c(0, 1), lty = 2)
mtext("Voter File", side = 1, line = 2, font = 1, adj = .5, cex = .75)
mtext("Census", side = 2, line = 2, font = 1, adj = .5, cex = .75)
mtext(bquote("R"^"2" ~ "=" ~ .(sub("0.", ".", sprintf("%.2f", round(cor(l2.prc$fem.bla, l2.prc$p_prc_fem_bla, use = "pairwise.complete.obs") ^ 2, 2))))), side = 3, line = 1, adj = .5, cex = .75)

plot(l2.prc$fem.his, l2.prc$p_prc_fem_his, pch = 20, cex = .5, col = rgb(0, 0, 0, alpha = .2), main = "", xlab = "", ylab = "")
lines(c(0, 1), c(0, 1), lty = 2)
mtext("Voter File", side = 1, line = 2, font = 1, adj = .5, cex = .75)
mtext("Census", side = 2, line = 2, font = 1, adj = .5, cex = .75)
mtext(bquote("R"^"2" ~ "=" ~ .(sub("0.", ".", sprintf("%.2f", round(cor(l2.prc$fem.his, l2.prc$p_prc_fem_his, use = "pairwise.complete.obs") ^ 2, 2))))), side = 3, line = 1, adj = .5, cex = .75)

plot(l2.prc$fem.asi, l2.prc$p_prc_fem_asi, pch = 20, cex = .5, col = rgb(0, 0, 0, alpha = .2), main = "", xlab = "", ylab = "")
lines(c(0, 1), c(0, 1), lty = 2)
mtext("Voter File", side = 1, line = 2, font = 1, adj = .5, cex = .75)
mtext("Census", side = 2, line = 2, font = 1, adj = .5, cex = .75)
mtext(bquote("R"^"2" ~ "=" ~ .(sub("0.", ".", sprintf("%.2f", round(cor(l2.prc$fem.asi, l2.prc$p_prc_fem_asi, use = "pairwise.complete.obs") ^ 2, 2))))), side = 3, line = 1, adj = .5, cex = .75)

## Males
plot(l2.prc$mal.whi, l2.prc$p_prc_mal_whi, pch = 20, cex = .5, col = rgb(0, 0, 0, alpha = .2), main = "", xlab = "", ylab = "")
lines(c(0, 1), c(0, 1), lty = 2)
mtext("Voter File", side = 1, line = 2, font = 1, adj = .5, cex = .75)
mtext("Census", side = 2, line = 2, font = 1, adj = .5, cex = .75)
mtext(bquote("R"^"2" ~ "=" ~ .(sub("0.", ".", sprintf("%.2f", round(cor(l2.prc$mal.whi, l2.prc$p_prc_mal_whi, use = "pairwise.complete.obs") ^ 2, 2))))), side = 3, line = 1, adj = .5, cex = .75)
mtext("Males", side = 2, line = 3, font = 2)

plot(l2.prc$mal.bla, l2.prc$p_prc_mal_bla, pch = 20, cex = .5, col = rgb(0, 0, 0, alpha = .2), main = "", xlab = "", ylab = "")
lines(c(0, 1), c(0, 1), lty = 2)
mtext("Voter File", side = 1, line = 2, font = 1, adj = .5, cex = .75)
mtext("Census", side = 2, line = 2, font = 1, adj = .5, cex = .75)
mtext(bquote("R"^"2" ~ "=" ~ .(sub("0.", ".", sprintf("%.2f", round(cor(l2.prc$mal.bla, l2.prc$p_prc_mal_bla, use = "pairwise.complete.obs") ^ 2, 2))))), side = 3, line = 1, adj = .5, cex = .75)

plot(l2.prc$mal.his, l2.prc$p_prc_mal_his, pch = 20, cex = .5, col = rgb(0, 0, 0, alpha = .2), main = "", xlab = "", ylab = "")
lines(c(0, 1), c(0, 1), lty = 2)
mtext("Voter File", side = 1, line = 2, font = 1, adj = .5, cex = .75)
mtext("Census", side = 2, line = 2, font = 1, adj = .5, cex = .75)
mtext(bquote("R"^"2" ~ "=" ~ .(sub("0.", ".", sprintf("%.2f", round(cor(l2.prc$mal.his, l2.prc$p_prc_mal_his, use = "pairwise.complete.obs") ^ 2, 2))))), side = 3, line = 1, adj = .5, cex = .75)

plot(l2.prc$mal.asi, l2.prc$p_prc_mal_asi, pch = 20, cex = .5, col = rgb(0, 0, 0, alpha = .2), main = "", xlab = "", ylab = "")
lines(c(0, 1), c(0, 1), lty = 2)
mtext("Voter File", side = 1, line = 2, font = 1, adj = .5, cex = .75)
mtext("Census", side = 2, line = 2, font = 1, adj = .5, cex = .75)
mtext(bquote("R"^"2" ~ "=" ~ .(sub("0.", ".", sprintf("%.2f", round(cor(l2.prc$mal.asi, l2.prc$p_prc_mal_asi, use = "pairwise.complete.obs") ^ 2, 2))))), side = 3, line = 1, adj = .5, cex = .75)

dev.off()
