setwd("C:/Users/ivan.petrusek/Desktop/Public Opinion Quarterly/REPLICATION FILES")

### Reading the key year data frame
years <- read.csv("poq_key_years.csv", sep = ";")
dim(years)

lapply(years, table)
year_labels <- c("Round 1: 2002/03", "Round 2: 2004/05",
                 "Round 3: 2006/07", "Round 4: 2008/09",
                 "Round 5: 2010/11", "Round 6: 2012/13",
                 "Round 7: 2014/15", "Round 8: 2016/17",
                 "Round 9: 2018/19")


########## FIGURE 1: Average support for income redistribution in Europe by welfare regime
# (PDF Size = Device Size: 10.60 x 5.57)

support_reg <- read.csv("support_by_round_aggregated_by_regimes.csv", sep = ";")
dim(support_reg)
support_reg[, 1]

par(mar = c(3, 2.5, 0.25, 1), mfrow = c(1, 1))
plot(x = seq(from = 1, to = 9, by = 1), y = as.numeric(support_reg[1, 2:10])*100,
     xlim = c(1, 9), ylim = c(50, 90), type = "p",
     xaxp = c(1, 9, 8), las = 1, xlab = "", ylab = "",
     main = "",  xaxt = "n", pch = 19, col = "dodgerblue3", cex = 1.5)

axis(side = 4, at = seq(from = 50, to = 90, by = 5), tck = 0.03, labels = FALSE)
axis(side = 2, at = seq(from = 50, to = 90, by = 5), tck = -0.03, labels = FALSE)
axis(side = 1, at = seq(from = 1, to = 9, by = 1), labels = year_labels, cex.axis = 0.75)

# Conservative regime (this only adds the lines)
points(x = seq(from = 1, to = 9, by = 1), y = as.numeric(support_reg[1, 2:10])*100,
       pch = 19, type = "l", lty = 3, lwd = 2, col = "dodgerblue3")
mean(as.numeric(support_reg[1, 2:10])*100)

# Social_democratic
points(x = seq(from = 1, to = 9, by = 1), y = as.numeric(support_reg[2, 2:10])*100,
       pch = 17, type = "p", lwd = 2, col = "salmon", cex = 1.4)

points(x = seq(from = 1, to = 9, by = 1), y = as.numeric(support_reg[2, 2:10])*100,
       pch = 17, type = "l", lty = 5, lwd = 2, col = "salmon")
mean(as.numeric(support_reg[2, 2:10])*100)

# Liberal
points(x = seq(from = 1, to = 9, by = 1), y = as.numeric(support_reg[3, 2:10])*100,
       pch = 18, type = "p", lwd = 2, col = "gold", cex = 1.6)

points(x = seq(from = 1, to = 9, by = 1), y = as.numeric(support_reg[3, 2:10])*100,
       pch = 18, type = "l", lty = 2, lwd = 2, col = "gold")
mean(as.numeric(support_reg[3, 2:10])*100)

# Mediterranean
points(x = seq(from = 1, to = 9, by = 1), y = as.numeric(support_reg[4, 2:10])*100,
       pch = 17, type = "p", lwd = 2, col = "lightskyblue", cex = 1.4)

points(x = seq(from = 1, to = 9, by = 1), y = as.numeric(support_reg[4, 2:10])*100,
       pch = 17, type = "l", lty = 3, lwd = 2, col = "lightskyblue")
mean(as.numeric(support_reg[4, 2:10])*100)

# Post-Communist
points(x = seq(from = 1, to = 9, by = 1), y = as.numeric(support_reg[5, 2:10])*100,
       pch = 19, type = "p", lwd = 2, col = "red3", cex = 1.5)

points(x = seq(from = 1, to = 9, by = 1), y = as.numeric(support_reg[5, 2:10])*100,
       pch = 19, type = "l", lty = 2, lwd = 2, col = "red3")
mean(as.numeric(support_reg[5, 2:10])*100)

# Total
points(x = seq(from = 1, to = 9, by = 1), y = as.numeric(support_reg[6, 2:10])*100,
       pch = 15, type = "p", lwd = 2, col = "black", cex = 1.5)

points(x = seq(from = 1, to = 9, by = 1), y = as.numeric(support_reg[6, 2:10])*100,
       pch = 15, type = "l", lty = 1, lwd = 2, col = "black")
mean(as.numeric(support_reg[6, 2:10])*100)

legend("bottom", legend = c("Conservative (67.0%)", "Social-democratic (59.9%)", "Liberal (66.2%)", "Mediterranean (83.9%)", "Post-communist (74.3%)", "ESS Round average (69.2%)"), 
       bty = "n", ncol = 3, pch = c(19, 17, 18, 17, 19, 15), lty = c(3, 5, 2, 3, 2, 1), cex = 1.1,
       col = c("dodgerblue3", "salmon", "gold", "lightskyblue", "red3", "black"), x.intersp = 1, text.width = 2, y.intersp = 1.3,
       text.col = c("dodgerblue3", "salmon", "gold", "lightskyblue", "red3", "black"), pt.cex =  c(1.5, 1.4, 1.6, 1.4, 1.5, 1.5),
       lwd = c(2, 2, 2, 2, 2, 2))



########## Figure A1. Average support for income redistribution in Europe by welfare regime, without two outlying countries (the Czech Republic and Denmark)
# (PDF Size = Device Size: 10.60 x 5.57)
support_reg <- read.csv("support_by_round_aggregated_by_regimes_without_DNK_and_CZE.csv", sep = ";")
dim(support_reg)
support_reg[, 1]

par(mar = c(3, 2.5, 0.25, 1), mfrow = c(1, 1))
plot(x = seq(from = 1, to = 9, by = 1), y = as.numeric(support_reg[1, 2:10])*100,
     xlim = c(1, 9), ylim = c(50, 90), type = "p",
     xaxp = c(1, 9, 8), las = 1, xlab = "", ylab = "",
     main = "",  xaxt = "n", pch = 19, col = "dodgerblue3", cex = 1.5)

axis(side = 4, at = seq(from = 50, to = 90, by = 5), tck = 0.03, labels = FALSE)
axis(side = 2, at = seq(from = 50, to = 90, by = 5), tck = -0.03, labels = FALSE)
axis(side = 1, at = seq(from = 1, to = 9, by = 1), labels = year_labels, cex.axis = 0.75)

# Conservative regime (this only adds the lines)
points(x = seq(from = 1, to = 9, by = 1), y = as.numeric(support_reg[1, 2:10])*100,
       pch = 19, type = "l", lty = 3, lwd = 2, col = "dodgerblue3")
mean(as.numeric(support_reg[1, 2:10])*100)

# Social_democratic
points(x = seq(from = 1, to = 9, by = 1), y = as.numeric(support_reg[2, 2:10])*100,
       pch = 17, type = "p", lwd = 2, col = "salmon", cex = 1.4)

points(x = seq(from = 1, to = 9, by = 1), y = as.numeric(support_reg[2, 2:10])*100,
       pch = 17, type = "l", lty = 5, lwd = 2, col = "salmon")
mean(as.numeric(support_reg[2, 2:10])*100)

# Liberal
points(x = seq(from = 1, to = 9, by = 1), y = as.numeric(support_reg[3, 2:10])*100,
       pch = 18, type = "p", lwd = 2, col = "gold", cex = 1.6)

points(x = seq(from = 1, to = 9, by = 1), y = as.numeric(support_reg[3, 2:10])*100,
       pch = 18, type = "l", lty = 2, lwd = 2, col = "gold")
mean(as.numeric(support_reg[3, 2:10])*100)

# Mediterranean
points(x = seq(from = 1, to = 9, by = 1), y = as.numeric(support_reg[4, 2:10])*100,
       pch = 17, type = "p", lwd = 2, col = "lightskyblue", cex = 1.4)

points(x = seq(from = 1, to = 9, by = 1), y = as.numeric(support_reg[4, 2:10])*100,
       pch = 17, type = "l", lty = 3, lwd = 2, col = "lightskyblue")
mean(as.numeric(support_reg[4, 2:10])*100)

# Post_Communist
points(x = seq(from = 1, to = 9, by = 1), y = as.numeric(support_reg[5, 2:10])*100,
       pch = 19, type = "p", lwd = 2, col = "red3", cex = 1.5)

points(x = seq(from = 1, to = 9, by = 1), y = as.numeric(support_reg[5, 2:10])*100,
       pch = 19, type = "l", lty = 2, lwd = 2, col = "red3")
mean(as.numeric(support_reg[5, 2:10])*100)

# Total
points(x = seq(from = 1, to = 9, by = 1), y = as.numeric(support_reg[6, 2:10])*100,
       pch = 15, type = "p", lwd = 2, col = "black", cex = 1.5)

points(x = seq(from = 1, to = 9, by = 1), y = as.numeric(support_reg[6, 2:10])*100,
       pch = 15, type = "l", lty = 1, lwd = 2, col = "black")
mean(as.numeric(support_reg[6, 2:10])*100)

legend("bottom", legend = c("Conservative (67.0%)", "Social-democratic (66.1%)", "Liberal (66.2%)", "Mediterranean (83.9%)", "Post-communist (79.1%)", "ESS Round average (71.8%)"), 
       bty = "n", ncol = 3, pch = c(19, 17, 18, 17, 19, 15), lty = c(3, 5, 2, 3, 2, 1), cex = 1,
       col = c("dodgerblue3", "salmon", "gold", "lightskyblue", "red3", "black"), x.intersp = 1, text.width = 2, y.intersp = 1.75,
       text.col = c("dodgerblue3", "salmon", "gold", "lightskyblue", "red3", "black"), pt.cex =  c(1.5, 1.4, 1.6, 1.4, 1.5, 1.5),
       lwd = c(2, 2, 2, 2, 2, 2))



########## Figure 2: Support for income redistribution in Europe (by five welfare regime types)
support <- read.csv("support_by_country_and_round.csv", sep = ";")
dim(support)

par(mar = c(3, 2.5, 2.5, 1), mfrow = c(3, 2))
### 1 Conservative regime (PDF Size = Device Size: 10.60 x 8.04)
plot(x = seq(from = 2002, to = 2019, by = 1), xlim = c(2002, 2019), ylim = c(30, 95), 
     xaxp = c(2002, 2019, 17), type = "n", las = 1, xlab = "", ylab = "",
     main = "Conservative regime", cex.main = 1.5)
axis(side = 4, at = seq(from = 30, to = 90, by = 10), tck = 0.03, labels = FALSE)

# Belgium
points(x = as.integer(years[years$cntry == "BE", 2:10]), 
       y = as.numeric(support[support$cntry == "BE", 2:10])*100,
       pch = 19, cex = 1.15)
points(x = as.integer(years[years$cntry == "BE", 2:10]), 
       y = as.numeric(support[support$cntry == "BE", 2:10])*100,
       pch = 19, type = "l", lty = 3, lwd = 2)

# Germany
points(x = as.integer(years[years$cntry == "DE", 2:10]), 
       y = as.numeric(support[support$cntry == "DE", 2:10])*100,
       pch = 15, col = "seagreen", cex = 1.1)
points(x = as.integer(years[years$cntry == "DE", 2:10]), 
       y = as.numeric(support[support$cntry == "DE", 2:10])*100,
       pch = 15, col = "seagreen", type = "l", lty = 1, lwd = 1.75)

# France
points(x = as.integer(years[years$cntry == "FR", 2:10]), 
       y = as.numeric(support[support$cntry == "FR", 2:10])*100,
       pch = 17, col = "salmon", cex = 1.1)
points(x = as.integer(years[years$cntry == "FR", 2:10]), 
       y = as.numeric(support[support$cntry == "FR", 2:10])*100,
       pch = 17, col = "salmon", type = "l", lty = 5, lwd = 1.75)

# Switzerland
points(x = as.integer(years[years$cntry == "CH", 2:10]), 
       y = as.numeric(support[support$cntry == "CH", 2:10])*100,
       pch = 18, col = "steelblue1", cex = 1.3)
points(x = as.integer(years[years$cntry == "CH", 2:10]), 
       y = as.numeric(support[support$cntry == "CH", 2:10])*100,
       pch = 18, col = "steelblue1", type = "l", lty = 2, lwd = 1.75)

# Netherlands
points(x = as.integer(years[years$cntry == "NL", 2:10]), 
       y = as.numeric(support[support$cntry == "NL", 2:10])*100,
       pch = 1, col = "gray50", cex = 1.2)
points(x = as.integer(years[years$cntry == "NL", 2:10]), 
       y = as.numeric(support[support$cntry == "NL", 2:10])*100,
       pch = 1, col = "gray50", type = "l", lty = 6, lwd = 1.75)

mean(as.numeric(support[support$cntry == "BE", 2:ncol(support)]), na.rm = TRUE)*100
mean(as.numeric(support[support$cntry == "DE", 2:ncol(support)]), na.rm = TRUE)*100
mean(as.numeric(support[support$cntry == "FR", 2:ncol(support)]), na.rm = TRUE)*100
mean(as.numeric(support[support$cntry == "CH", 2:ncol(support)]), na.rm = TRUE)*100
mean(as.numeric(support[support$cntry == "NL", 2:ncol(support)]), na.rm = TRUE)*100

legend("bottom", legend = c("Belgium (70.3%)", "Germany (65.4%)", "France (78.2%)", "Switzerland (64.6%)", "Netherlands (56.2%)"), 
       bty = "n", ncol = 3, pch = c(19, 15, 17, 18, 1), lty = c(3, 1, 5, 2, 6), cex = 1.1,
       col = c("black", "seagreen", "salmon", "steelblue1", "gray50"), x.intersp = 1.35, text.width = 4.15, y.intersp = 1.15,
       text.col = c("black", "seagreen", "salmon", "steelblue1", "gray50"), pt.cex =  c(1.15, 1.1, 1.1, 1.3, 1.2),
       lwd = c(1.5, 1.5, 1.5, 1.5, 1.5))

### 2 Social-democratic regime
plot(x = seq(from = 2002, to = 2019, by = 1), xlim = c(2002, 2019), ylim = c(30, 95), 
     xaxp = c(2002, 2019, 17), type = "n", las = 1, xlab = "", ylab = "",
     main = "Social-democratic regime", cex.main = 1.5)
axis(side = 4, at = seq(from = 30, to = 90, by = 10), tck = 0.03, labels = FALSE)

# Denmark
points(x = na.omit(as.integer(years[years$cntry == "DK", 2:10])), 
       y = na.omit(as.numeric(support[support$cntry == "DK", 2:10]))*100,
       pch = 19, cex = 1.15)
points(x = na.omit(as.integer(years[years$cntry == "DK", 2:10])), 
       y = na.omit(as.numeric(support[support$cntry == "DK", 2:10]))*100,
       pch = 19, type = "l", lty = 3, lwd = 2)

# Finland
points(x = as.integer(years[years$cntry == "FI", 2:10]), 
       y = as.numeric(support[support$cntry == "FI", 2:10])*100,
       pch = 15, col = "seagreen", cex = 1.1)
points(x = as.integer(years[years$cntry == "FI", 2:10]), 
       y = as.numeric(support[support$cntry == "FI", 2:10])*100,
       pch = 15, col = "seagreen", type = "l", lty = 1, lwd = 1.75)

# Norway
points(x = as.integer(years[years$cntry == "NO", 2:10]), 
       y = as.numeric(support[support$cntry == "NO", 2:10])*100,
       pch = 17, col = "salmon", cex = 1.1)
points(x = as.integer(years[years$cntry == "NO", 2:10]), 
       y = as.numeric(support[support$cntry == "NO", 2:10])*100,
       pch = 17, col = "salmon", type = "l", lty = 5, lwd = 1.75)

# Sweden
points(x = as.integer(years[years$cntry == "SE", 2:10]), 
       y = as.numeric(support[support$cntry == "SE", 2:10])*100,
       pch = 18, col = "steelblue1", cex = 1.3)
points(x = as.integer(years[years$cntry == "SE", 2:10]), 
       y = as.numeric(support[support$cntry == "SE", 2:10])*100,
       pch = 18, col = "steelblue1", type = "l", lty = 2, lwd = 1.75)

mean(as.numeric(support[support$cntry == "DK", 2:ncol(support)]), na.rm = TRUE)*100
mean(as.numeric(support[support$cntry == "FI", 2:ncol(support)]), na.rm = TRUE)*100
mean(as.numeric(support[support$cntry == "NO", 2:ncol(support)]), na.rm = TRUE)*100
mean(as.numeric(support[support$cntry == "SE", 2:ncol(support)]), na.rm = TRUE)*100

legend("top", legend = c("Denmark (39.1%)", "Finland (71.9%)", "Norway (61.8%)", "Sweden (64.0%)"), 
       bty = "n", ncol = 2, pch = c(19, 15, 17, 18), lty = c(3, 1, 5, 2), cex = 1.1,
       col = c("black", "seagreen", "salmon", "steelblue1"), x.intersp = 1.35, text.width = 4.5, y.intersp = 1.15,
       text.col = c("black", "seagreen", "salmon", "steelblue1"), pt.cex =  c(1.15, 1.1, 1.1, 1.3),
       lwd = c(1.5, 1.5, 1.5, 1.5))

### 3. Liberal regime
plot(x = seq(from = 2002, to = 2019, by = 1), xlim = c(2002, 2019), ylim = c(30, 95), 
     xaxp = c(2002, 2019, 17), type = "n", las = 1, xlab = "", ylab = "",
     main = "Liberal regime", cex.main = 1.5)
axis(side = 4, at = seq(from = 30, to = 90, by = 10), tck = 0.03, labels = FALSE)

# United Kingdom
points(x = na.omit(as.integer(years[years$cntry == "GB", 2:10])), 
       y = na.omit(as.numeric(support[support$cntry == "GB", 2:10]))*100,
       pch = 19, cex = 1.15)
points(x = na.omit(as.integer(years[years$cntry == "GB", 2:10])), 
       y = na.omit(as.numeric(support[support$cntry == "GB", 2:10]))*100,
       pch = 19, type = "l", lty = 3, lwd = 2)

# Ireland
points(x = as.integer(years[years$cntry == "IE", 2:10]), 
       y = as.numeric(support[support$cntry == "IE", 2:10])*100,
       pch = 15, col = "gray50", cex = 1.1)
points(x = as.integer(years[years$cntry == "IE", 2:10]), 
       y = as.numeric(support[support$cntry == "IE", 2:10])*100,
       pch = 15, col = "gray50", type = "l", lty = 1, lwd = 1.75)

legend("bottom", legend = c("United Kingdom (60.7%)", "Ireland (71.8%)"), 
       bty = "n", ncol = 2, pch = c(19, 15), lty = c(3, 1), cex = 1.1,
       col = c("black", "gray50"), x.intersp = 1.35, text.width = 6, y.intersp = 1.15,
       text.col = c("black", "gray50"), pt.cex =  c(1.15, 1.1),
       lwd = c(1.5, 1.5))

mean(as.numeric(support[support$cntry == "GB", 2:ncol(support)]), na.rm = TRUE)*100
mean(as.numeric(support[support$cntry == "IE", 2:ncol(support)]), na.rm = TRUE)*100

### 4. Mediterranean regime
plot(x = seq(from = 2002, to = 2019, by = 1), xlim = c(2002, 2019), ylim = c(30, 95), 
     xaxp = c(2002, 2019, 17), type = "n", las = 1, xlab = "", ylab = "",
     main = "Mediterranean regime", cex.main = 1.5)
axis(side = 4, at = seq(from = 30, to = 90, by = 10), tck = 0.03, labels = FALSE)

# Spain
points(x = as.integer(years[years$cntry == "ES", 2:10]), 
       y = as.numeric(support[support$cntry == "ES", 2:10])*100,
       pch = 17, col = "salmon", cex = 1.1)
points(x = as.integer(years[years$cntry == "ES", 2:10]), 
       y = as.numeric(support[support$cntry == "ES", 2:10])*100,
       pch = 17, col = "salmon", type = "l", lty = 5, lwd = 1.75)

# Portugal
points(x = as.integer(years[years$cntry == "PT", 2:10]), 
       y = as.numeric(support[support$cntry == "PT", 2:10])*100,
       pch = 18, col = "steelblue1", cex = 1.3)
points(x = as.integer(years[years$cntry == "PT", 2:10]), 
       y = as.numeric(support[support$cntry == "PT", 2:10])*100,
       pch = 18, col = "steelblue1", type = "l", lty = 2, lwd = 1.75)

mean(as.numeric(support[support$cntry == "ES", 2:ncol(support)]), na.rm = TRUE)*100
mean(as.numeric(support[support$cntry == "PT", 2:ncol(support)]), na.rm = TRUE)*100

legend("bottom", legend = c("Spain (79.7%)", "Portugal (88.4%)"), 
       bty = "n", ncol = 2, pch = c(17, 18), lty = c(5, 2), cex = 1.1,
       col = c("salmon", "steelblue1"), x.intersp = 1.35, text.width = 4.15, y.intersp = 1.15,
       text.col = c("salmon", "steelblue1"), pt.cex =  c(1.1, 1.3), lwd = c(1.5, 1.5))

### 5. Post-Communist regime
plot(x = seq(from = 2002, to = 2019, by = 1), xlim = c(2002, 2019), ylim = c(30, 95), 
     xaxp = c(2002, 2019, 17), type = "n", las = 1, xlab = "", ylab = "",
     main = "Post-communist regime", cex.main = 1.5)
axis(side = 4, at = seq(from = 30, to = 90, by = 10), tck = 0.03, labels = FALSE)

# Czech Republic
points(x = as.integer(years[years$cntry == "CZ", 2:10]), 
       y = as.numeric(support[support$cntry == "CZ", 2:10])*100,
       pch = 19, cex = 1.15)
points(x = na.omit(as.integer(years[years$cntry == "CZ", 2:10])), 
       y = na.omit(as.numeric(support[support$cntry == "CZ", 2:10]))*100,
       pch = 19, type = "l", lty = 3, lwd = 2)

# Estonia
points(x = as.integer(years[years$cntry == "EE", 2:10]), 
       y = as.numeric(support[support$cntry == "EE", 2:10])*100,
       pch = 15, col = "seagreen", cex = 1.1)
points(x = as.integer(years[years$cntry == "EE", 2:10]), 
       y = as.numeric(support[support$cntry == "EE", 2:10])*100,
       pch = 15, col = "seagreen", type = "l", lty = 1, lwd = 1.75)

# Hungary
points(x = as.integer(years[years$cntry == "HU", 2:10]), 
       y = as.numeric(support[support$cntry == "HU", 2:10])*100,
       pch = 17, col = "salmon", cex = 1.1)
points(x = as.integer(years[years$cntry == "HU", 2:10]), 
       y = as.numeric(support[support$cntry == "HU", 2:10])*100,
       pch = 17, col = "salmon", type = "l", lty = 5, lwd = 1.75)

# Poland
points(x = as.integer(years[years$cntry == "PL", 2:10]), 
       y = as.numeric(support[support$cntry == "PL", 2:10])*100,
       pch = 18, col = "steelblue1", cex = 1.3)
points(x = as.integer(years[years$cntry == "PL", 2:10]), 
       y = as.numeric(support[support$cntry == "PL", 2:10])*100,
       pch = 18, col = "steelblue1", type = "l", lty = 2, lwd = 1.75)

# Slovenia
points(x = as.integer(years[years$cntry == "SI", 2:10]), 
       y = as.numeric(support[support$cntry == "SI", 2:10])*100,
       pch = 1, col = "gray50", cex = 1.2)
points(x = as.integer(years[years$cntry == "SI", 2:10]), 
       y = as.numeric(support[support$cntry == "SI", 2:10])*100,
       pch = 1, col = "gray50", type = "l", lty = 6, lwd = 1.75)

mean(as.numeric(support[support$cntry == "CZ", 2:ncol(support)]), na.rm = TRUE)*100
mean(as.numeric(support[support$cntry == "EE", 2:ncol(support)]), na.rm = TRUE)*100
mean(as.numeric(support[support$cntry == "HU", 2:ncol(support)]), na.rm = TRUE)*100
mean(as.numeric(support[support$cntry == "PL", 2:ncol(support)]), na.rm = TRUE)*100
mean(as.numeric(support[support$cntry == "SI", 2:ncol(support)]), na.rm = TRUE)*100


legend("bottom", legend = c("Czech R. (54.0%)", "Estonia (71.8%)", "Hungary (84.2%)", "Poland (75.5%)", "Slovenia (83.9%)"), 
       bty = "n", ncol = 3, pch = c(19, 15, 17, 18, 1), lty = c(3, 1, 5, 2, 6), cex = 1.1,
       col = c("black", "seagreen", "salmon", "steelblue1", "gray50"), x.intersp = 1.35, text.width = 4.15, y.intersp = 1.15,
       text.col = c("black", "seagreen", "salmon", "steelblue1", "gray50"), pt.cex =  c(1.15, 1.1, 1.1, 1.3, 1.2),
       lwd = c(1.5, 1.5, 1.5, 1.5, 1.5))





##### Relationship between household income and support for income redistribution (Figures 3-7)
### Reading the data for income quintile groups
income_1 <- read.csv("support_first_quintile.csv", sep = ";")
dim(income_1)
range(income_1[, 2:10], na.rm = TRUE)

income_2 <- read.csv("support_middle_quintiles.csv", sep = ";")
dim(income_2)
range(income_2[, 2:10], na.rm = TRUE)

income_3 <- read.csv("support_last_quintile.csv", sep = ";")
dim(income_3)
range(income_3[, 2:10], na.rm = TRUE)


########## FIGURE 3: Support for income redistribution by income group: conservative regime
### CONSERVATIVE REGIME (PDF Size = Device Size: 10.60 x 8.04)
par(mar = c(3.25, 2.75, 2, 1), mfrow = c(3, 2))
# Belgium
plot(x = seq(from = 2002, to = 2019, by = 1), xlim = c(2002, 2019), ylim = c(30, 100), 
     xaxp = c(2002, 2019, 17), type = "n", las = 1, xlab = "", ylab = "",
     main = "Belgium", cex.main = 1.5)
axis(side = 4, at = seq(from = 30, to = 100, by = 10), tck = 0.025, labels = FALSE)

points(x = as.integer(years[years$cntry == "BE", 2:10]), 
       y = as.numeric(income_1[income_1$cntry == "BE", 2:10])*100,
       pch = 19, cex = 1.2, col = "salmon")
points(x = as.integer(years[years$cntry == "BE", 2:10]), 
       y = as.numeric(income_1[income_1$cntry == "BE", 2:10])*100,
       pch = 19, type = "l", lty = 5, lwd = 1.5, col = "salmon")

points(x = as.integer(years[years$cntry == "BE", 2:10]), 
       y = as.numeric(income_2[income_2$cntry == "BE", 2:10])*100,
       pch = 15, cex = 1.15)
points(x = as.integer(years[years$cntry == "BE", 2:10]), 
       y = as.numeric(income_2[income_2$cntry == "BE", 2:10])*100,
       pch = 15, type = "l", lty = 3, lwd = 1.5)

points(x = as.integer(years[years$cntry == "BE", 2:10]), 
       y = as.numeric(income_3[income_3$cntry == "BE", 2:10])*100,
       pch = 17, cex = 1.2, col = "steelblue1")
points(x = as.integer(years[years$cntry == "BE", 2:10]), 
       y = as.numeric(income_3[income_3$cntry == "BE", 2:10])*100,
       pch = 17, type = "l", lty = 1, lwd = 1.5, col = "steelblue1")

text(x = as.integer(years[years$cntry == "BE", 2:10]), col = "gray60",
     y = (as.numeric(income_1[income_1$cntry == "BE", 2:10])*100)+7.5,
     labels = round(as.numeric(income_1[income_1$cntry == "BE", 2:10])*100 - as.numeric(income_3[income_3$cntry == "BE", 2:10])*100, digits = 0))
# mean: 15.45
mean(as.numeric(income_1[income_1$cntry == "BE", 2:10])*100 - as.numeric(income_3[income_3$cntry == "BE", 2:10])*100)


# Germany
plot(x = seq(from = 2002, to = 2019, by = 1), xlim = c(2002, 2019), ylim = c(30, 100), 
     xaxp = c(2002, 2019, 17), type = "n", las = 1, xlab = "", ylab = "",
     main = "Germany", cex.main = 1.5)
axis(side = 4, at = seq(from = 30, to = 100, by = 10), tck = 0.025, labels = FALSE)

points(x = as.integer(years[years$cntry == "DE", 2:10]), 
       y = as.numeric(income_1[income_1$cntry == "DE", 2:10])*100,
       pch = 19, cex = 1.2, col = "salmon")
points(x = as.integer(years[years$cntry == "DE", 2:10]), 
       y = as.numeric(income_1[income_1$cntry == "DE", 2:10])*100,
       pch = 19, type = "l", lty = 5, lwd = 1.5, col = "salmon")

points(x = as.integer(years[years$cntry == "DE", 2:10]), 
       y = as.numeric(income_2[income_2$cntry == "DE", 2:10])*100,
       pch = 15, cex = 1.15)
points(x = as.integer(years[years$cntry == "DE", 2:10]), 
       y = as.numeric(income_2[income_2$cntry == "DE", 2:10])*100,
       pch = 15, type = "l", lty = 3, lwd = 1.5)

points(x = as.integer(years[years$cntry == "DE", 2:10]), 
       y = as.numeric(income_3[income_3$cntry == "DE", 2:10])*100,
       pch = 17, cex = 1.2, col = "steelblue1")
points(x = as.integer(years[years$cntry == "DE", 2:10]), 
       y = as.numeric(income_3[income_3$cntry == "DE", 2:10])*100,
       pch = 17, type = "l", lty = 1, lwd = 1.5, col = "steelblue1")

text(x = as.integer(years[years$cntry == "DE", 2:10]), col = "gray60",
     y = (as.numeric(income_1[income_1$cntry == "DE", 2:10])*100)+7.5,
     labels = round(as.numeric(income_1[income_1$cntry == "DE", 2:10])*100 - as.numeric(income_3[income_3$cntry == "DE", 2:10])*100, digits = 0))
# mean: 19.4
mean(as.numeric(income_1[income_1$cntry == "DE", 2:10])*100 - as.numeric(income_3[income_3$cntry == "DE", 2:10])*100)


# France
plot(x = seq(from = 2002, to = 2019, by = 1), xlim = c(2002, 2019), ylim = c(30, 100), 
     xaxp = c(2002, 2019, 17), type = "n", las = 1, xlab = "", ylab = "",
     main = "France", cex.main = 1.5)
axis(side = 4, at = seq(from = 30, to = 100, by = 10), tck = 0.025, labels = FALSE)

points(x = as.integer(years[years$cntry == "FR", 2:10]), 
       y = as.numeric(income_1[income_1$cntry == "FR", 2:10])*100,
       pch = 19, cex = 1.2, col = "salmon")
points(x = as.integer(years[years$cntry == "FR", 2:10]), 
       y = as.numeric(income_1[income_1$cntry == "FR", 2:10])*100,
       pch = 19, type = "l", lty = 5, lwd = 1.5, col = "salmon")

points(x = as.integer(years[years$cntry == "FR", 2:10]), 
       y = as.numeric(income_2[income_2$cntry == "FR", 2:10])*100,
       pch = 15, cex = 1.15)
points(x = as.integer(years[years$cntry == "FR", 2:10]), 
       y = as.numeric(income_2[income_2$cntry == "FR", 2:10])*100,
       pch = 15, type = "l", lty = 3, lwd = 1.5)

points(x = as.integer(years[years$cntry == "FR", 2:10]), 
       y = as.numeric(income_3[income_3$cntry == "FR", 2:10])*100,
       pch = 17, cex = 1.2, col = "steelblue1")
points(x = as.integer(years[years$cntry == "FR", 2:10]), 
       y = as.numeric(income_3[income_3$cntry == "FR", 2:10])*100,
       pch = 17, type = "l", lty = 1, lwd = 1.5, col = "steelblue1")

text(x = as.integer(years[years$cntry == "FR", 2:10]), col = "gray60",
     y = (as.numeric(income_1[income_1$cntry == "FR", 2:10])*100)+7.5,
     labels = round(as.numeric(income_1[income_1$cntry == "FR", 2:10])*100 - as.numeric(income_3[income_3$cntry == "FR", 2:10])*100, digits = 0))

# mean: 18.4
mean(as.numeric(income_1[income_1$cntry == "FR", 2:10])*100 - as.numeric(income_3[income_3$cntry == "FR", 2:10])*100, na.rm = TRUE)


# Switzerland
plot(x = seq(from = 2002, to = 2019, by = 1), xlim = c(2002, 2019), ylim = c(30, 100), 
     xaxp = c(2002, 2019, 17), type = "n", las = 1, xlab = "", ylab = "",
     main = "Switzerland", cex.main = 1.5)
axis(side = 4, at = seq(from = 30, to = 100, by = 10), tck = 0.025, labels = FALSE)

points(x = as.integer(years[years$cntry == "CH", 2:10]), 
       y = as.numeric(income_1[income_1$cntry == "CH", 2:10])*100,
       pch = 19, cex = 1.2, col = "salmon")
points(x = as.integer(years[years$cntry == "CH", 2:10]), 
       y = as.numeric(income_1[income_1$cntry == "CH", 2:10])*100,
       pch = 19, type = "l", lty = 5, lwd = 1.5, col = "salmon")

points(x = as.integer(years[years$cntry == "CH", 2:10]), 
       y = as.numeric(income_2[income_2$cntry == "CH", 2:10])*100,
       pch = 15, cex = 1.15)
points(x = as.integer(years[years$cntry == "CH", 2:10]), 
       y = as.numeric(income_2[income_2$cntry == "CH", 2:10])*100,
       pch = 15, type = "l", lty = 3, lwd = 1.5)

points(x = as.integer(years[years$cntry == "CH", 2:10]), 
       y = as.numeric(income_3[income_3$cntry == "CH", 2:10])*100,
       pch = 17, cex = 1.2, col = "steelblue1")
points(x = as.integer(years[years$cntry == "CH", 2:10]), 
       y = as.numeric(income_3[income_3$cntry == "CH", 2:10])*100,
       pch = 17, type = "l", lty = 1, lwd = 1.5, col = "steelblue1")

text(x = as.integer(years[years$cntry == "CH", 2:10]), col = "gray60",
     y = (as.numeric(income_1[income_1$cntry == "CH", 2:10])*100)+7.5,
     labels = round(as.numeric(income_1[income_1$cntry == "CH", 2:10])*100 - as.numeric(income_3[income_3$cntry == "CH", 2:10])*100, digits = 0))

# mean: 26.4
mean(as.numeric(income_1[income_1$cntry == "CH", 2:10])*100 - as.numeric(income_3[income_3$cntry == "CH", 2:10])*100)


# Netherlands
plot(x = seq(from = 2002, to = 2019, by = 1), xlim = c(2002, 2019), ylim = c(30, 100), 
     xaxp = c(2002, 2019, 17), type = "n", las = 1, xlab = "", ylab = "",
     main = "Netherlands", cex.main = 1.5)
axis(side = 4, at = seq(from = 30, to = 100, by = 10), tck = 0.025, labels = FALSE)

points(x = as.integer(years[years$cntry == "NL", 2:10]), 
       y = as.numeric(income_1[income_1$cntry == "NL", 2:10])*100,
       pch = 19, cex = 1.2, col = "salmon")
points(x = as.integer(years[years$cntry == "NL", 2:10]), 
       y = as.numeric(income_1[income_1$cntry == "NL", 2:10])*100,
       pch = 19, type = "l", lty = 5, lwd = 1.5, col = "salmon")

points(x = as.integer(years[years$cntry == "NL", 2:10]), 
       y = as.numeric(income_2[income_2$cntry == "NL", 2:10])*100,
       pch = 15, cex = 1.15)
points(x = as.integer(years[years$cntry == "NL", 2:10]), 
       y = as.numeric(income_2[income_2$cntry == "NL", 2:10])*100,
       pch = 15, type = "l", lty = 3, lwd = 1.5)

points(x = as.integer(years[years$cntry == "NL", 2:10]), 
       y = as.numeric(income_3[income_3$cntry == "NL", 2:10])*100,
       pch = 17, cex = 1.2, col = "steelblue1")
points(x = as.integer(years[years$cntry == "NL", 2:10]), 
       y = as.numeric(income_3[income_3$cntry == "NL", 2:10])*100,
       pch = 17, type = "l", lty = 1, lwd = 1.5, col = "steelblue1")

text(x = as.integer(years[years$cntry == "NL", 2:10]), col = "gray60",
     y = (as.numeric(income_1[income_1$cntry == "NL", 2:10])*100)+7.5,
     labels = round(as.numeric(income_1[income_1$cntry == "NL", 2:10])*100 - as.numeric(income_3[income_3$cntry == "NL", 2:10])*100, digits = 0))
# mean: 29.4
mean(as.numeric(income_1[income_1$cntry == "NL", 2:10])*100 - as.numeric(income_3[income_3$cntry == "NL", 2:10])*100)

plot.new()
legend("topleft", legend = c("low income (1st quintile)", "medium income (2nd, 3rd and 4th)", "high income (5th quintile)"), 
       bty = "n", ncol = 1, pch = c(19, 15, 17), lty = c(5, 3, 1),
       col = c("salmon", "black", "steelblue1"), x.intersp = 1.25, text.width = 2.25, y.intersp = 1.3,
       text.col = c("salmon", "black", "steelblue1"), pt.cex =  c(1.2, 1.15, 1.2), cex = 1.4,
       lwd = c(1.5, 1.5, 1.5))



########## FIGURE 4: Support for income redistribution by income group: Social-democratic regime
### SOCIAL-DEMOCRATIC (PDF Size = Device Size: 10.60 x 5.57)
par(oma = c(1.5, 0, 0, 0), mar = c(3.25, 2.75, 2, 1), mfrow = c(2, 2))
# Denmark
plot(x = seq(from = 2002, to = 2019, by = 1), xlim = c(2002, 2019), ylim = c(25, 90), 
     xaxp = c(2002, 2019, 17), type = "n", las = 1, xlab = "", ylab = "",
     main = "Denmark")
axis(side = 4, at = seq(from = 30, to = 90, by = 10), tck = 0.025, labels = FALSE)

points(x = as.integer(years[years$cntry == "DK", 2:10]), 
       y = as.numeric(income_1[income_1$cntry == "DK", 2:10])*100,
       pch = 19, cex = 1.2, col = "salmon")
points(x = na.omit(as.integer(years[years$cntry == "DK", 2:10])), 
       y = na.omit(as.numeric(income_1[income_1$cntry == "DK", 2:10]))*100,
       pch = 19, type = "l", lty = 5, lwd = 1.5, col = "salmon")

points(x = as.integer(years[years$cntry == "DK", 2:10]), 
       y = as.numeric(income_2[income_2$cntry == "DK", 2:10])*100,
       pch = 15, cex = 1.15)
points(x = na.omit(as.integer(years[years$cntry == "DK", 2:10])), 
       y = na.omit(as.numeric(income_2[income_2$cntry == "DK", 2:10]))*100,
       pch = 15, type = "l", lty = 3, lwd = 1.5)

points(x = as.integer(years[years$cntry == "DK", 2:10]), 
       y = as.numeric(income_3[income_3$cntry == "DK", 2:10])*100,
       pch = 17, cex = 1.2, col = "steelblue1")
points(x = na.omit(as.integer(years[years$cntry == "DK", 2:10])), 
       y = na.omit(as.numeric(income_3[income_3$cntry == "DK", 2:10]))*100,
       pch = 17, type = "l", lty = 1, lwd = 1.5, col = "steelblue1")

text(x = as.integer(years[years$cntry == "DK", 2:10]), col = "gray60",
     y = (as.numeric(income_1[income_1$cntry == "DK", 2:10])*100)+7.5,
     labels = round(as.numeric(income_1[income_1$cntry == "DK", 2:10])*100 - as.numeric(income_3[income_3$cntry == "DK", 2:10])*100, digits = 0))
# mean: 14.7
mean(as.numeric(income_1[income_1$cntry == "DK", 2:10])*100 - as.numeric(income_3[income_3$cntry == "DK", 2:10])*100, na.rm = TRUE)


# Finland
plot(x = seq(from = 2002, to = 2019, by = 1), xlim = c(2002, 2019), ylim = c(25, 90), 
     xaxp = c(2002, 2019, 17), type = "n", las = 1, xlab = "", ylab = "",
     main = "Finland")
axis(side = 4, at = seq(from = 30, to = 90, by = 10), tck = 0.025, labels = FALSE)

points(x = as.integer(years[years$cntry == "FI", 2:10]), 
       y = as.numeric(income_1[income_1$cntry == "FI", 2:10])*100,
       pch = 19, cex = 1.2, col = "salmon")
points(x = as.integer(years[years$cntry == "FI", 2:10]), 
       y = as.numeric(income_1[income_1$cntry == "FI", 2:10])*100,
       pch = 19, type = "l", lty = 5, lwd = 1.5, col = "salmon")

points(x = as.integer(years[years$cntry == "FI", 2:10]), 
       y = as.numeric(income_2[income_2$cntry == "FI", 2:10])*100,
       pch = 15, cex = 1.15)
points(x = as.integer(years[years$cntry == "FI", 2:10]), 
       y = as.numeric(income_2[income_2$cntry == "FI", 2:10])*100,
       pch = 15, type = "l", lty = 3, lwd = 1.5)

points(x = as.integer(years[years$cntry == "FI", 2:10]), 
       y = as.numeric(income_3[income_3$cntry == "FI", 2:10])*100,
       pch = 17, cex = 1.2, col = "steelblue1")
points(x = as.integer(years[years$cntry == "FI", 2:10]), 
       y = as.numeric(income_3[income_3$cntry == "FI", 2:10])*100,
       pch = 17, type = "l", lty = 1, lwd = 1.5, col = "steelblue1")

text(x = as.integer(years[years$cntry == "FI", 2:10]), col = "gray60",
     y = (as.numeric(income_1[income_1$cntry == "FI", 2:10])*100)+ c(7.5, 9.5, 12, 7.5, 7.5, 7.5, 8.5, 7.5, 7.5),
     labels = round(as.numeric(income_1[income_1$cntry == "FI", 2:10])*100 - as.numeric(income_3[income_3$cntry == "FI", 2:10])*100, digits = 0))
# mean: 17.8
mean(as.numeric(income_1[income_1$cntry == "FI", 2:10])*100 - as.numeric(income_3[income_3$cntry == "FI", 2:10])*100, na.rm = TRUE)


# Norway
plot(x = seq(from = 2002, to = 2019, by = 1), xlim = c(2002, 2019), ylim = c(25, 90), 
     xaxp = c(2002, 2019, 17), type = "n", las = 1, xlab = "", ylab = "",
     main = "Norway")
axis(side = 4, at = seq(from = 30, to = 90, by = 10), tck = 0.025, labels = FALSE)

points(x = as.integer(years[years$cntry == "NO", 2:10]), 
       y = as.numeric(income_1[income_1$cntry == "NO", 2:10])*100,
       pch = 19, cex = 1.2, col = "salmon")
points(x = as.integer(years[years$cntry == "NO", 2:10]), 
       y = as.numeric(income_1[income_1$cntry == "NO", 2:10])*100,
       pch = 19, type = "l", lty = 5, lwd = 1.5, col = "salmon")

points(x = as.integer(years[years$cntry == "NO", 2:10]), 
       y = as.numeric(income_2[income_2$cntry == "NO", 2:10])*100,
       pch = 15, cex = 1.15)
points(x = as.integer(years[years$cntry == "NO", 2:10]), 
       y = as.numeric(income_2[income_2$cntry == "NO", 2:10])*100,
       pch = 15, type = "l", lty = 3, lwd = 1.5)

points(x = as.integer(years[years$cntry == "NO", 2:10]), 
       y = as.numeric(income_3[income_3$cntry == "NO", 2:10])*100,
       pch = 17, cex = 1.2, col = "steelblue1")
points(x = as.integer(years[years$cntry == "NO", 2:10]), 
       y = as.numeric(income_3[income_3$cntry == "NO", 2:10])*100,
       pch = 17, type = "l", lty = 1, lwd = 1.5, col = "steelblue1")

text(x = as.integer(years[years$cntry == "NO", 2:10]), col = "gray60",
     y = (as.numeric(income_1[income_1$cntry == "NO", 2:10])*100)+ c(7.5, 7.5, 7.5, 9, 7.5, 7.5, 7.5, 7.5, 7.5),
     labels = round(as.numeric(income_1[income_1$cntry == "NO", 2:10])*100 - as.numeric(income_3[income_3$cntry == "NO", 2:10])*100, digits = 0))
# mean: 16.2
mean(as.numeric(income_1[income_1$cntry == "NO", 2:10])*100 - as.numeric(income_3[income_3$cntry == "NO", 2:10])*100, na.rm = TRUE)


# Sweden
plot(x = seq(from = 2002, to = 2019, by = 1), xlim = c(2002, 2019), ylim = c(25, 90), 
     xaxp = c(2002, 2019, 17), type = "n", las = 1, xlab = "", ylab = "",
     main = "Sweden")
axis(side = 4, at = seq(from = 30, to = 90, by = 10), tck = 0.025, labels = FALSE)

points(x = as.integer(years[years$cntry == "SE", 2:10]), 
       y = as.numeric(income_1[income_1$cntry == "SE", 2:10])*100,
       pch = 19, cex = 1.2, col = "salmon")
points(x = as.integer(years[years$cntry == "SE", 2:10]), 
       y = as.numeric(income_1[income_1$cntry == "SE", 2:10])*100,
       pch = 19, type = "l", lty = 5, lwd = 1.5, col = "salmon")

points(x = as.integer(years[years$cntry == "SE", 2:10]), 
       y = as.numeric(income_2[income_2$cntry == "SE", 2:10])*100,
       pch = 15, cex = 1.15)
points(x = as.integer(years[years$cntry == "SE", 2:10]), 
       y = as.numeric(income_2[income_2$cntry == "SE", 2:10])*100,
       pch = 15, type = "l", lty = 3, lwd = 1.5)

points(x = as.integer(years[years$cntry == "SE", 2:10]), 
       y = as.numeric(income_3[income_3$cntry == "SE", 2:10])*100,
       pch = 17, cex = 1.2, col = "steelblue1")
points(x = as.integer(years[years$cntry == "SE", 2:10]), 
       y = as.numeric(income_3[income_3$cntry == "SE", 2:10])*100,
       pch = 17, type = "l", lty = 1, lwd = 1.5, col = "steelblue1")

text(x = as.integer(years[years$cntry == "SE", 2:10]), col = "gray60",
     y = (as.numeric(income_1[income_1$cntry == "SE", 2:10])*100)+7.5,
     labels = round(as.numeric(income_1[income_1$cntry == "SE", 2:10])*100 - as.numeric(income_3[income_3$cntry == "SE", 2:10])*100, digits = 0))
# mean: 20.2
mean(as.numeric(income_1[income_1$cntry == "SE", 2:10])*100 - as.numeric(income_3[income_3$cntry == "SE", 2:10])*100, na.rm = TRUE)


par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 2.5, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
# Finally, the legend is inserted to the outer margin
legend("bottom", legend = c("low income (1st quintile)", "medium income (2nd, 3rd, 4th)", "high income (5th quintile)"), 
       bty = "n", ncol = 3, pch = c(19, 15, 17), lty = c(5, 3, 1),
       col = c("salmon", "black", "steelblue1"), x.intersp = 0.975, text.width = 0.61, y.intersp = 1.25,
       text.col = c("salmon", "black", "steelblue1"), pt.cex =  c(1.2, 1.15, 1.2), cex = 1.2,
       lwd = c(1.5, 1.5, 1.5))



########## FIGURE 5: Support for income redistribution by income group: liberal regime
### LIBERAL (PDF Size = Device Size: 10.60 x 3.18)
par(oma = c(1.5, 0, 0, 0), mar = c(3.25, 2.5, 2, 0.5), mfrow = c(1, 2))
# United Kingdom
plot(x = seq(from = 2002, to = 2019, by = 1), xlim = c(2002, 2019), ylim = c(40, 100), 
     xaxp = c(2002, 2019, 17), type = "n", las = 1, xlab = "", ylab = "",
     main = "United Kingdom")
axis(side = 4, at = seq(from = 30, to = 100, by = 10), tck = 0.025, labels = FALSE)

points(x = as.integer(years[years$cntry == "GB", 2:10]), 
       y = as.numeric(income_1[income_1$cntry == "GB", 2:10])*100,
       pch = 19, cex = 1.2, col = "salmon")
points(x = as.integer(years[years$cntry == "GB", 2:10]), 
       y = as.numeric(income_1[income_1$cntry == "GB", 2:10])*100,
       pch = 19, type = "l", lty = 5, lwd = 1.5, col = "salmon")

points(x = as.integer(years[years$cntry == "GB", 2:10]), 
       y = as.numeric(income_2[income_2$cntry == "GB", 2:10])*100,
       pch = 15, cex = 1.15)
points(x = as.integer(years[years$cntry == "GB", 2:10]), 
       y = as.numeric(income_2[income_2$cntry == "GB", 2:10])*100,
       pch = 15, type = "l", lty = 3, lwd = 1.5)

points(x = as.integer(years[years$cntry == "GB", 2:10]), 
       y = as.numeric(income_3[income_3$cntry == "GB", 2:10])*100,
       pch = 17, cex = 1.2, col = "steelblue1")
points(x = as.integer(years[years$cntry == "GB", 2:10]), 
       y = as.numeric(income_3[income_3$cntry == "GB", 2:10])*100,
       pch = 17, type = "l", lty = 1, lwd = 1.5, col = "steelblue1")

text(x = as.integer(years[years$cntry == "GB", 2:10]), col = "gray60",
     y = (as.numeric(income_1[income_1$cntry == "GB", 2:10])*100)+7.5,
     labels = round(as.numeric(income_1[income_1$cntry == "GB", 2:10])*100 - as.numeric(income_3[income_3$cntry == "GB", 2:10])*100, digits = 0))
# mean: 22.6
mean(as.numeric(income_1[income_1$cntry == "GB", 2:10])*100 - as.numeric(income_3[income_3$cntry == "GB", 2:10])*100, na.rm = TRUE)


# Ireland
plot(x = seq(from = 2002, to = 2019, by = 1), xlim = c(2002, 2019), ylim = c(40, 100), 
     xaxp = c(2002, 2019, 17), type = "n", las = 1, xlab = "", ylab = "",
     main = "Ireland")
axis(side = 4, at = seq(from = 30, to = 100, by = 10), tck = 0.025, labels = FALSE)

points(x = as.integer(years[years$cntry == "IE", 2:10]), 
       y = as.numeric(income_1[income_1$cntry == "IE", 2:10])*100,
       pch = 19, cex = 1.2, col = "salmon")
points(x = as.integer(years[years$cntry == "IE", 2:10]), 
       y = as.numeric(income_1[income_1$cntry == "IE", 2:10])*100,
       pch = 19, type = "l", lty = 5, lwd = 1.5, col = "salmon")

points(x = as.integer(years[years$cntry == "IE", 2:10]), 
       y = as.numeric(income_2[income_2$cntry == "IE", 2:10])*100,
       pch = 15, cex = 1.15)
points(x = as.integer(years[years$cntry == "IE", 2:10]), 
       y = as.numeric(income_2[income_2$cntry == "IE", 2:10])*100,
       pch = 15, type = "l", lty = 3, lwd = 1.5)

points(x = as.integer(years[years$cntry == "IE", 2:10]), 
       y = as.numeric(income_3[income_3$cntry == "IE", 2:10])*100,
       pch = 17, cex = 1.2, col = "steelblue1")
points(x = as.integer(years[years$cntry == "IE", 2:10]), 
       y = as.numeric(income_3[income_3$cntry == "IE", 2:10])*100,
       pch = 17, type = "l", lty = 1, lwd = 1.5, col = "steelblue1")

text(x = as.integer(years[years$cntry == "IE", 2:10]), col = "gray60",
     y = (as.numeric(income_1[income_1$cntry == "IE", 2:10])*100) + c(7.5, 7.5, 7.5, 7.5, 5, 7.5, 7.5, 7.5, 7.5),
     labels = round(as.numeric(income_1[income_1$cntry == "IE", 2:10])*100 - as.numeric(income_3[income_3$cntry == "IE", 2:10])*100, digits = 0))
# mean: 12.6
mean(as.numeric(income_1[income_1$cntry == "IE", 2:10])*100 - as.numeric(income_3[income_3$cntry == "IE", 2:10])*100, na.rm = TRUE)

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 2.5, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
# Finally, the legend is inserted to the outer margin
legend("bottom", legend = c("low income (1st quintile)", "medium income (2nd, 3rd, 4th)", "high income (5th quintile)"), 
       bty = "n", ncol = 3, pch = c(19, 15, 17), lty = c(5, 3, 1),
       col = c("salmon", "black", "steelblue1"), x.intersp = 0.975, text.width = 0.61, y.intersp = 1.25,
       text.col = c("salmon", "black", "steelblue1"), pt.cex =  c(1.2, 1.15, 1.2), cex = 1.2,
       lwd = c(1.5, 1.5, 1.5))


########## FIGURE 6: Support for income redistribution by income group: Mediterranean regime
### MEDITERRANEAN (PDF Size = Device Size: 10.60 x 3.18)
par(oma = c(1.5, 0, 0, 0), mar = c(3.25, 2.5, 2, 0.5), mfrow = c(1, 2))
# Spain
plot(x = seq(from = 2002, to = 2019, by = 1), xlim = c(2002, 2019), ylim = c(40, 100), 
     xaxp = c(2002, 2019, 17), type = "n", las = 1, xlab = "", ylab = "",
     main = "Spain")
axis(side = 4, at = seq(from = 30, to = 100, by = 10), tck = 0.025, labels = FALSE)

points(x = as.integer(years[years$cntry == "ES", 2:10]), 
       y = as.numeric(income_1[income_1$cntry == "ES", 2:10])*100,
       pch = 19, cex = 1.2, col = "salmon")
points(x = as.integer(years[years$cntry == "ES", 2:10]), 
       y = as.numeric(income_1[income_1$cntry == "ES", 2:10])*100,
       pch = 19, type = "l", lty = 5, lwd = 1.5, col = "salmon")

points(x = as.integer(years[years$cntry == "ES", 2:10]), 
       y = as.numeric(income_2[income_2$cntry == "ES", 2:10])*100,
       pch = 15, cex = 1.15)
points(x = as.integer(years[years$cntry == "ES", 2:10]), 
       y = as.numeric(income_2[income_2$cntry == "ES", 2:10])*100,
       pch = 15, type = "l", lty = 3, lwd = 1.5)

points(x = as.integer(years[years$cntry == "ES", 2:10]), 
       y = as.numeric(income_3[income_3$cntry == "ES", 2:10])*100,
       pch = 17, cex = 1.2, col = "steelblue1")
points(x = as.integer(years[years$cntry == "ES", 2:10]), 
       y = as.numeric(income_3[income_3$cntry == "ES", 2:10])*100,
       pch = 17, type = "l", lty = 1, lwd = 1.5, col = "steelblue1")

text(x = as.integer(years[years$cntry == "ES", 2:10]), col = "gray60",
     y = (as.numeric(income_1[income_1$cntry == "ES", 2:10])*100) + c(-5, 7.5, 5, 7.5, 7.5, 7.5, 7.5, 7.5, 7.5),
     labels = round(as.numeric(income_1[income_1$cntry == "ES", 2:10])*100 - as.numeric(income_3[income_3$cntry == "ES", 2:10])*100, digits = 0))
# mean: 8.2
mean(as.numeric(income_1[income_1$cntry == "ES", 2:10])*100 - as.numeric(income_3[income_3$cntry == "ES", 2:10])*100, na.rm = TRUE)


# Portugal
plot(x = seq(from = 2002, to = 2019, by = 1), xlim = c(2002, 2019), ylim = c(40, 100), 
     xaxp = c(2002, 2019, 17), type = "n", las = 1, xlab = "", ylab = "",
     main = "Portugal")
axis(side = 4, at = seq(from = 30, to = 100, by = 10), tck = 0.025, labels = FALSE)

points(x = as.integer(years[years$cntry == "PT", 2:10]), 
       y = as.numeric(income_1[income_1$cntry == "PT", 2:10])*100,
       pch = 19, cex = 1.2, col = "salmon")
points(x = as.integer(c(2002, 2005, 2007, 2009, 2013, 2015, 2017, 2019)), 
       y = na.omit(as.numeric(income_1[income_1$cntry == "PT", 2:10]))*100,
       pch = 19, type = "l", lty = 5, lwd = 1.5, col = "salmon")

points(x = as.integer(years[years$cntry == "PT", 2:10]), 
       y = as.numeric(income_2[income_2$cntry == "PT", 2:10])*100,
       pch = 15, cex = 1.15)
points(x = as.integer(c(2002, 2005, 2007, 2009, 2013, 2015, 2017, 2019)), 
       y = na.omit(as.numeric(income_2[income_2$cntry == "PT", 2:10]))*100,
       pch = 15, type = "l", lty = 3, lwd = 1.5)

points(x = as.integer(years[years$cntry == "PT", 2:10]), 
       y = as.numeric(income_3[income_3$cntry == "PT", 2:10])*100,
       pch = 17, cex = 1.2, col = "steelblue1")
points(x = as.integer(c(2002, 2005, 2007, 2009, 2013, 2015, 2017, 2019)), 
       y = na.omit(as.numeric(income_3[income_3$cntry == "PT", 2:10]))*100,
       pch = 17, type = "l", lty = 1, lwd = 1.5, col = "steelblue1")

text(x = as.integer(years[years$cntry == "PT", 2:10]), col = "gray60",
     y = (as.numeric(income_1[income_1$cntry == "PT", 2:10])*100) + c(3.25, -5, 7.5, 7.5, -5, -5, 7.5, 7.5, 6.5),
     labels = round(as.numeric(income_1[income_1$cntry == "PT", 2:10])*100 - as.numeric(income_3[income_3$cntry == "PT", 2:10])*100, digits = 0))
# mean: 3.8
mean(as.numeric(income_1[income_1$cntry == "PT", 2:10])*100 - as.numeric(income_3[income_3$cntry == "PT", 2:10])*100, na.rm = TRUE)


par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 2.5, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
# Finally, the legend is inserted to the outer margin
legend("bottom", legend = c("low income (1st quintile)", "medium income (2nd, 3rd, 4th)", "high income (5th quintile)"), 
       bty = "n", ncol = 3, pch = c(19, 15, 17), lty = c(5, 3, 1),
       col = c("salmon", "black", "steelblue1"), x.intersp = 0.975, text.width = 0.61, y.intersp = 1.25,
       text.col = c("salmon", "black", "steelblue1"), pt.cex =  c(1.2, 1.15, 1.2), cex = 1.2,
       lwd = c(1.5, 1.5, 1.5))



########## FIGURE 7: Support for income redistribution by income group: post-communist regime
### POST-COMMUNIST REGIME (PDF Size = Device Size: 9.27 x 8.04)
par(mar = c(3.25, 2.75, 2, 1), mfrow = c(3, 2))
# Czech Republic
plot(x = seq(from = 2002, to = 2019, by = 1), xlim = c(2002, 2019), ylim = c(30, 100), 
     xaxp = c(2002, 2019, 17), type = "n", las = 1, xlab = "", ylab = "",
     main = "Czech Republic", cex.main = 1.5)
axis(side = 4, at = seq(from = 30, to = 100, by = 10), tck = 0.025, labels = FALSE)

points(x = as.integer(years[years$cntry == "CZ", 2:10]), 
       y = as.numeric(income_1[income_1$cntry == "CZ", 2:10])*100,
       pch = 19, cex = 1.2, col = "salmon")
points(x = na.omit(as.integer(years[years$cntry == "CZ", 2:10])), 
       y = na.omit(as.numeric(income_1[income_1$cntry == "CZ", 2:10]))*100,
       pch = 19, type = "l", lty = 5, lwd = 1.5, col = "salmon")

points(x = as.integer(years[years$cntry == "CZ", 2:10]), 
       y = as.numeric(income_2[income_2$cntry == "CZ", 2:10])*100,
       pch = 15, cex = 1.15)
points(x = na.omit(as.integer(years[years$cntry == "CZ", 2:10])), 
       y = na.omit(as.numeric(income_2[income_2$cntry == "CZ", 2:10]))*100,
       pch = 15, type = "l", lty = 3, lwd = 1.5)

points(x = as.integer(years[years$cntry == "CZ", 2:10]), 
       y = as.numeric(income_3[income_3$cntry == "CZ", 2:10])*100,
       pch = 17, cex = 1.2, col = "steelblue1")
points(x = na.omit(as.integer(years[years$cntry == "CZ", 2:10])), 
       y = na.omit(as.numeric(income_3[income_3$cntry == "CZ", 2:10]))*100,
       pch = 17, type = "l", lty = 1, lwd = 1.5, col = "steelblue1")

text(x = as.integer(years[years$cntry == "CZ", 2:10]), col = "gray60",
     y = (as.numeric(income_1[income_1$cntry == "CZ", 2:10])*100)+7.5,
     labels = round(as.numeric(income_1[income_1$cntry == "CZ", 2:10])*100 - as.numeric(income_3[income_3$cntry == "CZ", 2:10])*100, digits = 0))
# mean: 24.38
mean(as.numeric(income_1[income_1$cntry == "CZ", 2:10])*100 - as.numeric(income_3[income_3$cntry == "CZ", 2:10])*100, na.rm = TRUE)


# Estonia
plot(x = seq(from = 2002, to = 2019, by = 1), xlim = c(2002, 2019), ylim = c(30, 100), 
     xaxp = c(2002, 2019, 17), type = "n", las = 1, xlab = "", ylab = "",
     main = "Estonia", cex.main = 1.5)
axis(side = 4, at = seq(from = 30, to = 100, by = 10), tck = 0.025, labels = FALSE)

points(x = as.integer(years[years$cntry == "EE", 2:10]), 
       y = as.numeric(income_1[income_1$cntry == "EE", 2:10])*100,
       pch = 19, cex = 1.2, col = "salmon")
points(x = as.integer(c(2009, 2010, 2012, 2016, 2018)), 
       y = na.omit(as.numeric(income_1[income_1$cntry == "EE", 2:10]))*100,
       pch = 19, type = "l", lty = 5, lwd = 1.5, col = "salmon")

points(x = as.integer(years[years$cntry == "EE", 2:10]), 
       y = as.numeric(income_2[income_2$cntry == "EE", 2:10])*100,
       pch = 15, cex = 1.15)
points(x = as.integer(c(2009, 2010, 2012, 2016, 2018)), 
       y = na.omit(as.numeric(income_2[income_2$cntry == "EE", 2:10]))*100,
       pch = 15, type = "l", lty = 3, lwd = 1.5)

points(x = as.integer(years[years$cntry == "EE", 2:10]), 
       y = as.numeric(income_3[income_3$cntry == "EE", 2:10])*100,
       pch = 17, cex = 1.2, col = "steelblue1")
points(x = as.integer(c(2009, 2010, 2012, 2016, 2018)), 
       y = na.omit(as.numeric(income_3[income_3$cntry == "EE", 2:10]))*100,
       pch = 17, type = "l", lty = 1, lwd = 1.5, col = "steelblue1")

text(x = as.integer(years[years$cntry == "EE", 2:10]), col = "gray60",
     y = (as.numeric(income_1[income_1$cntry == "EE", 2:10])*100)+7.5,
     labels = round(as.numeric(income_1[income_1$cntry == "EE", 2:10])*100 - as.numeric(income_3[income_3$cntry == "EE", 2:10])*100, digits = 0))
# mean: 26.2
mean(as.numeric(income_1[income_1$cntry == "EE", 2:10])*100 - as.numeric(income_3[income_3$cntry == "EE", 2:10])*100, na.rm = TRUE)


# Hungary
plot(x = seq(from = 2002, to = 2019, by = 1), xlim = c(2002, 2019), ylim = c(30, 100), 
     xaxp = c(2002, 2019, 17), type = "n", las = 1, xlab = "", ylab = "",
     main = "Hungary", cex.main = 1.5)
axis(side = 4, at = seq(from = 30, to = 100, by = 10), tck = 0.025, labels = FALSE)

points(x = as.integer(years[years$cntry == "HU", 2:10]), 
       y = as.numeric(income_1[income_1$cntry == "HU", 2:10])*100,
       pch = 19, cex = 1.2, col = "salmon")
points(x = as.integer(c(2009, 2010, 2012, 2015, 2017, 2019)), 
       y = na.omit(as.numeric(income_1[income_1$cntry == "HU", 2:10]))*100,
       pch = 19, type = "l", lty = 5, lwd = 1.5, col = "salmon")

points(x = as.integer(years[years$cntry == "HU", 2:10]), 
       y = as.numeric(income_2[income_2$cntry == "HU", 2:10])*100,
       pch = 15, cex = 1.15)
points(x = as.integer(c(2009, 2010, 2012, 2015, 2017, 2019)), 
       y = na.omit(as.numeric(income_2[income_2$cntry == "HU", 2:10]))*100,
       pch = 15, type = "l", lty = 3, lwd = 1.5)

points(x = as.integer(years[years$cntry == "HU", 2:10]), 
       y = as.numeric(income_3[income_3$cntry == "HU", 2:10])*100,
       pch = 17, cex = 1.2, col = "steelblue1")
points(x = as.integer(c(2009, 2010, 2012, 2015, 2017, 2019)), 
       y = na.omit(as.numeric(income_3[income_3$cntry == "HU", 2:10]))*100,
       pch = 17, type = "l", lty = 1, lwd = 1.5, col = "steelblue1")

text(x = as.integer(years[years$cntry == "HU", 2:10]), col = "gray60",
     y = (as.numeric(income_1[income_1$cntry == "HU", 2:10])*100) + c(7.5, 7.5, 7.5, 7.5, 7.5, 7.5, 10, 7.5, 5.5),
     labels = round(as.numeric(income_1[income_1$cntry == "HU", 2:10])*100 - as.numeric(income_3[income_3$cntry == "HU", 2:10])*100, digits = 0))
# mean: 6.7
mean(as.numeric(income_1[income_1$cntry == "HU", 2:10])*100 - as.numeric(income_3[income_3$cntry == "HU", 2:10])*100, na.rm = TRUE)


# Poland
plot(x = seq(from = 2002, to = 2019, by = 1), xlim = c(2002, 2019), ylim = c(30, 100), 
     xaxp = c(2002, 2019, 17), type = "n", las = 1, xlab = "", ylab = "",
     main = "Poland", cex.main = 1.5)
axis(side = 4, at = seq(from = 30, to = 100, by = 10), tck = 0.025, labels = FALSE)

points(x = as.integer(years[years$cntry == "PL", 2:10]), 
       y = as.numeric(income_1[income_1$cntry == "PL", 2:10])*100,
       pch = 19, cex = 1.2, col = "salmon")
points(x = as.integer(years[years$cntry == "PL", 2:10]), 
       y = as.numeric(income_1[income_1$cntry == "PL", 2:10])*100,
       pch = 19, type = "l", lty = 5, lwd = 1.5, col = "salmon")

points(x = as.integer(years[years$cntry == "PL", 2:10]), 
       y = as.numeric(income_2[income_2$cntry == "PL", 2:10])*100,
       pch = 15, cex = 1.15)
points(x = as.integer(years[years$cntry == "PL", 2:10]), 
       y = as.numeric(income_2[income_2$cntry == "PL", 2:10])*100,
       pch = 15, type = "l", lty = 3, lwd = 1.5)

points(x = as.integer(years[years$cntry == "PL", 2:10]), 
       y = as.numeric(income_3[income_3$cntry == "PL", 2:10])*100,
       pch = 17, cex = 1.2, col = "steelblue1")
points(x = as.integer(years[years$cntry == "PL", 2:10]), 
       y = as.numeric(income_3[income_3$cntry == "PL", 2:10])*100,
       pch = 17, type = "l", lty = 1, lwd = 1.5, col = "steelblue1")

text(x = as.integer(years[years$cntry == "PL", 2:10]), col = "gray60",
     y = (as.numeric(income_1[income_1$cntry == "PL", 2:10])*100)+7.5,
     labels = round(as.numeric(income_1[income_1$cntry == "PL", 2:10])*100 - as.numeric(income_3[income_3$cntry == "PL", 2:10])*100, digits = 0))
# mean: 20.8
mean(as.numeric(income_1[income_1$cntry == "PL", 2:10])*100 - as.numeric(income_3[income_3$cntry == "PL", 2:10])*100, na.rm = TRUE)


# Slovenia
plot(x = seq(from = 2002, to = 2019, by = 1), xlim = c(2002, 2019), ylim = c(30, 100), 
     xaxp = c(2002, 2019, 17), type = "n", las = 1, xlab = "", ylab = "",
     main = "Slovenia", cex.main = 1.5)
axis(side = 4, at = seq(from = 30, to = 100, by = 10), tck = 0.025, labels = FALSE)

points(x = as.integer(years[years$cntry == "SI", 2:10]), 
       y = as.numeric(income_1[income_1$cntry == "SI", 2:10])*100,
       pch = 19, cex = 1.2, col = "salmon")
points(x = as.integer(years[years$cntry == "SI", 2:10]), 
       y = as.numeric(income_1[income_1$cntry == "SI", 2:10])*100,
       pch = 19, type = "l", lty = 5, lwd = 1.5, col = "salmon")

points(x = as.integer(years[years$cntry == "SI", 2:10]), 
       y = as.numeric(income_2[income_2$cntry == "SI", 2:10])*100,
       pch = 15, cex = 1.15)
points(x = as.integer(years[years$cntry == "SI", 2:10]), 
       y = as.numeric(income_2[income_2$cntry == "SI", 2:10])*100,
       pch = 15, type = "l", lty = 3, lwd = 1.5)

points(x = as.integer(years[years$cntry == "SI", 2:10]), 
       y = as.numeric(income_3[income_3$cntry == "SI", 2:10])*100,
       pch = 17, cex = 1.2, col = "steelblue1")
points(x = as.integer(years[years$cntry == "SI", 2:10]), 
       y = as.numeric(income_3[income_3$cntry == "SI", 2:10])*100,
       pch = 17, type = "l", lty = 1, lwd = 1.5, col = "steelblue1")

text(x = as.integer(years[years$cntry == "SI", 2:10]), col = "gray60",
     y = (as.numeric(income_1[income_1$cntry == "SI", 2:10])*100)+7.5,
     labels = round(as.numeric(income_1[income_1$cntry == "SI", 2:10])*100 - as.numeric(income_3[income_3$cntry == "SI", 2:10])*100, digits = 0))
# mean: 16.9
mean(as.numeric(income_1[income_1$cntry == "SI", 2:10])*100 - as.numeric(income_3[income_3$cntry == "SI", 2:10])*100, na.rm = TRUE)


plot.new()
legend("topleft", legend = c("low income (1st quintile)", "medium income (2nd, 3rd and 4th)", "high income (5th quintile)"), 
       bty = "n", ncol = 1, pch = c(19, 15, 17), lty = c(5, 3, 1),
       col = c("salmon", "black", "steelblue1"), x.intersp = 1.25, text.width = 2.25, y.intersp = 1.3,
       text.col = c("salmon", "black", "steelblue1"), pt.cex =  c(1.2, 1.15, 1.2), cex = 1.4,
       lwd = c(1.5, 1.5, 1.5))



########## FIGURE 8: Belief in meritocracy in Europe (by five welfare regime types)
dfincac <- read.csv("dfincac_bin.csv", sep = ";")

######### (PDF Size = Device Size: 10.60 x 5.57)
# Conservative regime
par(oma = c(0, 0, 0, 0), mar = c(3.25, 2.5, 2, 0.5), mfrow = c(3, 2))

cons_cntry <- dfincac[1:5, 1]
dfincac_cons_bar <- barplot(as.matrix(t(dfincac[1:5, 2:3])), ylim = c(0, 70),
                            main = "Conservative regime", cex.main = 1.5,
                            names.arg = cons_cntry,
                            col = c("gray", "steelblue1"),
                            beside = TRUE, las = 1,
                            space = c(0.2, 1))

text(dfincac_cons_bar, y = as.matrix(t(dfincac[1:5, 2:3])), 
     labels = round(as.numeric(c(dfincac[1, 2:3], dfincac[2, 2:3], dfincac[3, 2:3], dfincac[4, 2:3], dfincac[5, 2:3])), digits = 0), 
     cex = 0.9, pos = 1)

# Social-democratic regime
soc_dem_cntry <- c(dfincac[6:9, 1], "")
dfincac_soc_bar <- barplot(cbind(as.matrix(t(dfincac[6:9, 2:3])), c(NA, NA)), ylim = c(0, 70),
                           main = "Social-democratic regime", cex.main = 1.5,
                           names.arg = soc_dem_cntry,
                           col = c("gray", "steelblue1"),
                           beside = TRUE, las = 1,
                           space = c(0.2, 1))

text(dfincac_soc_bar, y = as.matrix(t(dfincac[6:9, 2:3])), 
     labels = round(as.numeric(c(dfincac[6, 2:3], dfincac[7, 2:3], dfincac[8, 2:3], dfincac[9, 2:3], c(NA, NA))), digits = 0), 
     cex = 0.9, pos = 1)

# Liberal
liberal_cntry <- c(dfincac[10:11, 1], "", "", "")
dfincac_lib_bar <- barplot(cbind(as.matrix(t(dfincac[10:11, 2:3])), matrix(c(NA, NA, NA, NA, NA, NA), nrow = 2)), ylim = c(0, 70),
                           main = "Liberal regime", cex.main = 1.5,
                           names.arg = liberal_cntry,
                           col = c("gray", "steelblue1"),
                           beside = TRUE, las = 1,
                           space = c(0.2, 1))

text(dfincac_lib_bar, y = as.matrix(t(dfincac[10:11, 2:3])), 
     labels = round(as.numeric(c(dfincac[10, 2:3], dfincac[11, 2:3], c(NA, NA, NA, NA, NA, NA))), digits = 0), 
     cex = 0.9, pos = 1)

# Mediterranean
mediter_cntry <- c(dfincac[12:13, 1], "", "", "")
dfincac_med_bar <- barplot(cbind(as.matrix(t(dfincac[12:13, 2:3])), matrix(c(NA, NA, NA, NA, NA, NA), nrow = 2)), ylim = c(0, 70),
                           main = "Mediterranean regime", cex.main = 1.5,
                           names.arg = mediter_cntry,
                           col = c("gray", "steelblue1"),
                           beside = TRUE, las = 1,
                           space = c(0.2, 1))

text(dfincac_med_bar, y = as.matrix(t(dfincac[12:13, 2:3])), 
     labels = round(as.numeric(c(dfincac[12, 2:3], dfincac[13, 2:3], c(NA, NA, NA, NA, NA, NA))), digits = 0), 
     cex = 0.9, pos = 1)



# Post-Communist regime
post_com_cntry <- dfincac[14:18, 1]
dfincac_com_bar <- barplot(as.matrix(t(dfincac[14:18, 2:3])), ylim = c(0, 70),
                           main = "Post-communist regime", cex.main = 1.5,
                           names.arg = post_com_cntry,
                           col = c("gray", "steelblue1"),
                           beside = TRUE, las = 1,
                           space = c(0.2, 1))

text(dfincac_com_bar, y = as.matrix(t(dfincac[14:18, 2:3])), 
     labels = round(as.numeric(c(dfincac[14, 2:3], dfincac[15, 2:3], dfincac[16, 2:3], dfincac[17, 2:3], dfincac[18, 2:3])), digits = 0), 
     cex = 0.9, pos = 1)

plot.new()
legend("topleft", ncol = 1, pch = c(15, 15), col = c("gray", "steelblue1"), 
       legend = c("Round 4: 2008/09", "Round 8: 2016/17"), cex = 1.5, bty = "n", pt.cex = 5,
       x.intersp = 0.975, text.width = 0.45, y.intersp = 2)



########## FIGURE 9: Belief in egalitarianism in Europe (by five welfare regime types)
smdfslv <- read.csv("smdfslv_bin.csv", sep = ";")

######### (PDF Size = Device Size: 10.60 x 5.57)
# Conservative regime
par(oma = c(0, 0, 0, 0), mar = c(3.25, 2.5, 2, 0.5), mfrow = c(3, 2))

cons_cntry <- smdfslv[1:5, 1]
smdfslv_cons_bar <- barplot(as.matrix(t(smdfslv[1:5, 2:3])), ylim = c(0, 85),
                            main = "Conservative regime", cex.main = 1.5,
                            names.arg = cons_cntry,
                            col = c("gray", "steelblue1"),
                            beside = TRUE, las = 1,
                            space = c(0.2, 1))

text(smdfslv_cons_bar, y = as.matrix(t(smdfslv[1:5, 2:3])), 
     labels = round(as.numeric(c(smdfslv[1, 2:3], smdfslv[2, 2:3], smdfslv[3, 2:3], smdfslv[4, 2:3], smdfslv[5, 2:3])), digits = 0), 
     cex = 0.9, pos = 1)

# Social-democratic regime
soc_dem_cntry <- c(smdfslv[6:9, 1], "")
smdfslv_soc_bar <- barplot(cbind(as.matrix(t(smdfslv[6:9, 2:3])), c(NA, NA)), ylim = c(0, 85),
                           main = "Social-democratic regime", cex.main = 1.5,
                           names.arg = soc_dem_cntry,
                           col = c("gray", "steelblue1"),
                           beside = TRUE, las = 1,
                           space = c(0.2, 1))

text(smdfslv_soc_bar, y = as.matrix(t(smdfslv[6:9, 2:3])), 
     labels = round(as.numeric(c(smdfslv[6, 2:3], smdfslv[7, 2:3], smdfslv[8, 2:3], smdfslv[9, 2:3], c(NA, NA))), digits = 0), 
     cex = 0.9, pos = 1)

# Liberal
liberal_cntry <- c(smdfslv[10:11, 1], "", "", "")
smdfslv_lib_bar <- barplot(cbind(as.matrix(t(smdfslv[10:11, 2:3])), matrix(c(NA, NA, NA, NA, NA, NA), nrow = 2)), ylim = c(0, 85),
                           main = "Liberal regime", cex.main = 1.5,
                           names.arg = liberal_cntry,
                           col = c("gray", "steelblue1"),
                           beside = TRUE, las = 1,
                           space = c(0.2, 1))

text(smdfslv_lib_bar, y = as.matrix(t(smdfslv[10:11, 2:3])), 
     labels = round(as.numeric(c(smdfslv[10, 2:3], smdfslv[11, 2:3], c(NA, NA, NA, NA, NA, NA))), digits = 0), 
     cex = 0.9, pos = 1)

# Mediterranean
mediter_cntry <- c(smdfslv[12:13, 1], "", "", "")
smdfslv_med_bar <- barplot(cbind(as.matrix(t(smdfslv[12:13, 2:3])), matrix(c(NA, NA, NA, NA, NA, NA), nrow = 2)), ylim = c(0, 85),
                           main = "Mediterranean regime", cex.main = 1.5,
                           names.arg = mediter_cntry,
                           col = c("gray", "steelblue1"),
                           beside = TRUE, las = 1,
                           space = c(0.2, 1))

text(smdfslv_med_bar, y = as.matrix(t(smdfslv[12:13, 2:3])), 
     labels = round(as.numeric(c(smdfslv[12, 2:3], smdfslv[13, 2:3], c(NA, NA, NA, NA, NA, NA))), digits = 0), 
     cex = 0.9, pos = 1)



# Post-Communist regime
post_com_cntry <- smdfslv[14:18, 1]
smdfslv_com_bar <- barplot(as.matrix(t(smdfslv[14:18, 2:3])), ylim = c(0, 85),
                           main = "Post-communist regime", cex.main = 1.5,
                           names.arg = post_com_cntry,
                           col = c("gray", "steelblue1"),
                           beside = TRUE, las = 1,
                           space = c(0.2, 1))

text(smdfslv_com_bar, y = as.matrix(t(smdfslv[14:18, 2:3])), 
     labels = round(as.numeric(c(smdfslv[14, 2:3], smdfslv[15, 2:3], smdfslv[16, 2:3], smdfslv[17, 2:3], smdfslv[18, 2:3])), digits = 0), 
     cex = 0.9, pos = 1)

plot.new()
legend("topleft", ncol = 1, pch = c(15, 15), col = c("gray", "steelblue1"), 
       legend = c("Round 4: 2008/09", "Round 8: 2016/17"), cex = 1.5, bty = "n", pt.cex = 5,
       x.intersp = 0.975, text.width = 0.45, y.intersp = 2)





########## FIGURE A2: Market income and disposable income inequality by country, 2002-2018/19
### Loading the SWIID database data, Version 9.1 
load("C:/Users/ivan.petrusek/Desktop/Public Opinion Quarterly/REPLICATION FILES/swiid9_1.rda")

class(swiid_summary)
dim(swiid_summary) ### 5788 by 12
colnames(swiid_summary)

# adding an empty column "cntry" as a placeholder for ESS country variable
swiid_summary$cntry <- rep(NA)

# assigning two-digit ISO country code to OECD_data$cntry column
for(i in 1:dim(swiid_summary)[1]){
        if(swiid_summary[i,1] == "Albania"){
                swiid_summary[i,dim(swiid_summary)[2]] <- "AL"
        }else if(swiid_summary[i,1] == "Austria"){
                swiid_summary[i,dim(swiid_summary)[2]] <- "AT"
        }else if(swiid_summary[i,1] == "Belgium"){
                swiid_summary[i,dim(swiid_summary)[2]] <- "BE"
        }else if(swiid_summary[i,1] == "Bulgaria"){
                swiid_summary[i,dim(swiid_summary)[2]] <- "BG"
        }else if(swiid_summary[i,1] == "Croatia"){
                swiid_summary[i,dim(swiid_summary)[2]] <- "HR"
        }else if(swiid_summary[i,1] == "Cyprus"){
                swiid_summary[i,dim(swiid_summary)[2]] <- "CY"
        }else if(swiid_summary[i,1] == "Czech Republic"){
                swiid_summary[i,dim(swiid_summary)[2]] <- "CZ"
        }else if(swiid_summary[i,1] == "Denmark"){
                swiid_summary[i,dim(swiid_summary)[2]] <- "DK"
        }else if(swiid_summary[i,1] == "Estonia"){
                swiid_summary[i,dim(swiid_summary)[2]] <- "EE"
        }else if(swiid_summary[i,1] == "Finland"){
                swiid_summary[i,dim(swiid_summary)[2]] <- "FI"
        }else if(swiid_summary[i,1] == "France"){
                swiid_summary[i,dim(swiid_summary)[2]] <- "FR"
        }else if(swiid_summary[i,1] == "Germany"){
                swiid_summary[i,dim(swiid_summary)[2]] <- "DE"
        }else if(swiid_summary[i,1] == "Greece"){
                swiid_summary[i,dim(swiid_summary)[2]] <- "GR"
        }else if(swiid_summary[i,1] == "Hungary"){
                swiid_summary[i,dim(swiid_summary)[2]] <- "HU"
        }else if(swiid_summary[i,1] == "Iceland"){
                swiid_summary[i,dim(swiid_summary)[2]] <- "IS"
        }else if(swiid_summary[i,1] == "Ireland"){
                swiid_summary[i,dim(swiid_summary)[2]] <- "IE"
        }else if(swiid_summary[i,1] == "Israel"){
                swiid_summary[i,dim(swiid_summary)[2]] <- "IL"
        }else if(swiid_summary[i,1] == "Italy"){
                swiid_summary[i,dim(swiid_summary)[2]] <- "IT"
        }else if(swiid_summary[i,1] == "Kosovo"){
                swiid_summary[i,dim(swiid_summary)[2]] <- "XK"
        }else if(swiid_summary[i,1] == "Latvia"){
                swiid_summary[i,dim(swiid_summary)[2]] <- "LV"
        }else if(swiid_summary[i,1] == "Lithuania"){
                swiid_summary[i,dim(swiid_summary)[2]] <- "LT"
        }else if(swiid_summary[i,1] == "Luxembourg"){
                swiid_summary[i,dim(swiid_summary)[2]] <- "LU"
        }else if(swiid_summary[i,1] == "Netherlands"){
                swiid_summary[i,dim(swiid_summary)[2]] <- "NL"
        }else if(swiid_summary[i,1] == "Norway"){
                swiid_summary[i,dim(swiid_summary)[2]] <- "NO"
        }else if(swiid_summary[i,1] == "Poland"){
                swiid_summary[i,dim(swiid_summary)[2]] <- "PL"
        }else if(swiid_summary[i,1] == "Portugal"){
                swiid_summary[i,dim(swiid_summary)[2]] <- "PT"
        }else if(swiid_summary[i,1] == "Romania"){
                swiid_summary[i,dim(swiid_summary)[2]] <- "RO"
        }else if(swiid_summary[i,1] == "Russia"){
                swiid_summary[i,dim(swiid_summary)[2]] <- "RU"
        }else if(swiid_summary[i,1] == "Slovakia"){
                swiid_summary[i,dim(swiid_summary)[2]] <- "SK"
        }else if(swiid_summary[i,1] == "Slovenia"){
                swiid_summary[i,dim(swiid_summary)[2]] <- "SI"
        }else if(swiid_summary[i,1] == "Spain"){
                swiid_summary[i,dim(swiid_summary)[2]] <- "ES"
        }else if(swiid_summary[i,1] == "Sweden"){
                swiid_summary[i,dim(swiid_summary)[2]] <- "SE"
        }else if(swiid_summary[i,1] == "Switzerland"){
                swiid_summary[i,dim(swiid_summary)[2]] <- "CH"
        }else if(swiid_summary[i,1] == "Turkey"){
                swiid_summary[i,dim(swiid_summary)[2]] <- "TR"
        }else if(swiid_summary[i,1] == "Ukraine"){
                swiid_summary[i,dim(swiid_summary)[2]] <- "UA"
        }else if(swiid_summary[i,1] == "United Kingdom"){
                swiid_summary[i,dim(swiid_summary)[2]] <- "GB"
        }else{
                swiid_summary[i,dim(swiid_summary)[2]] <- ""
        }
}

# checking the assignment of country codes
table(swiid_summary$cntry)
length(table(swiid_summary$cntry))

# Subsetting only those 36 countries that are part of ESS
swiid_ess <- swiid_summary[swiid_summary$cntry != "", ]
table(swiid_ess$cntry)
dim(swiid_ess)

library(data.table)
setDT(swiid_ess)

# Subsetting only those 18 countries that are part of the POQ analysis
swiid_poq <- swiid_ess[(cntry == "BE" | cntry == "CZ" | cntry == "DE"
                        | cntry == "DK" | cntry == "EE" | cntry == "ES"
                        | cntry == "FI" | cntry == "FR" | cntry == "GB"
                        | cntry == "HU" | cntry == "CH" | cntry == "IE"
                        | cntry == "NL" | cntry == "NO" | cntry == "PL"
                        | cntry == "PT" | cntry == "SE" | cntry == "SI") & (year >= 2002 & year <= 2019)]

dim(swiid_poq)
table(swiid_poq$cntry)
tapply(swiid_poq$year, swiid_poq$cntry, range)

# Computing average Gini coefficients by countries (across time)
swiid_poq[, .(prumer_market = mean(gini_mkt), prumer_disp = mean(gini_disp)), by = "cntry"][order(prumer_disp)]
swiid_poq[, .(prumer_market = mean(gini_mkt), prumer_disp = mean(gini_disp)), by = "cntry"][order(prumer_market)]


### Range of vertical axes: 23.0 - 56.3
range(swiid_poq$gini_disp)
range(swiid_poq$gini_mkt)

years <- 2002:2019

# Belgium
BE_disp <- c(swiid_poq$gini_disp[swiid_poq$cntry == "BE"], NA)
BE_mkt <- c(swiid_poq$gini_mkt[swiid_poq$cntry == "BE"], NA)

# Germany
DE_disp <- c(swiid_poq$gini_disp[swiid_poq$cntry == "DE"], NA)
DE_mkt <- c(swiid_poq$gini_mkt[swiid_poq$cntry == "DE"], NA)

# France
FR_disp <- c(swiid_poq$gini_disp[swiid_poq$cntry == "FR"], NA)
FR_mkt <- c(swiid_poq$gini_mkt[swiid_poq$cntry == "FR"], NA)

# Switzerland
CH_disp <- c(swiid_poq$gini_disp[swiid_poq$cntry == "CH"], NA)
CH_mkt <- c(swiid_poq$gini_mkt[swiid_poq$cntry == "CH"], NA)

# Netherlands
NL_disp <- c(swiid_poq$gini_disp[swiid_poq$cntry == "NL"])
NL_mkt <- c(swiid_poq$gini_mkt[swiid_poq$cntry == "NL"])


# Denmark
DK_disp <- c(swiid_poq$gini_disp[swiid_poq$cntry == "DK"])
DK_mkt <- c(swiid_poq$gini_mkt[swiid_poq$cntry == "DK"])

# Finland
FI_disp <- c(swiid_poq$gini_disp[swiid_poq$cntry == "FI"])
FI_mkt <- c(swiid_poq$gini_mkt[swiid_poq$cntry == "FI"])

# Norway
NO_disp <- c(swiid_poq$gini_disp[swiid_poq$cntry == "NO"])
NO_mkt <- c(swiid_poq$gini_mkt[swiid_poq$cntry == "NO"])

# Sweden
SE_disp <- c(swiid_poq$gini_disp[swiid_poq$cntry == "SE"], NA)
SE_mkt <- c(swiid_poq$gini_mkt[swiid_poq$cntry == "SE"], NA)


# United Kingdom
GB_disp <- c(swiid_poq$gini_disp[swiid_poq$cntry == "GB"])
GB_mkt <- c(swiid_poq$gini_mkt[swiid_poq$cntry == "GB"])

# Ireland
IE_disp <- c(swiid_poq$gini_disp[swiid_poq$cntry == "IE"])
IE_mkt <- c(swiid_poq$gini_mkt[swiid_poq$cntry == "IE"])


# Spain
ES_disp <- c(swiid_poq$gini_disp[swiid_poq$cntry == "ES"], NA)
ES_mkt <- c(swiid_poq$gini_mkt[swiid_poq$cntry == "ES"], NA)

# Portugal
PT_disp <- c(swiid_poq$gini_disp[swiid_poq$cntry == "PT"], NA)
PT_mkt <- c(swiid_poq$gini_mkt[swiid_poq$cntry == "PT"], NA)


# Czech Republic
CZ_disp <- c(swiid_poq$gini_disp[swiid_poq$cntry == "CZ"], NA)
CZ_mkt <- c(swiid_poq$gini_mkt[swiid_poq$cntry == "CZ"], NA)

# Estonia
EE_disp <- c(swiid_poq$gini_disp[swiid_poq$cntry == "EE"])
EE_mkt <- c(swiid_poq$gini_mkt[swiid_poq$cntry == "EE"])

# Hungary
HU_disp <- c(swiid_poq$gini_disp[swiid_poq$cntry == "HU"])
HU_mkt <- c(swiid_poq$gini_mkt[swiid_poq$cntry == "HU"])

# Poland
PL_disp <- c(swiid_poq$gini_disp[swiid_poq$cntry == "PL"])
PL_mkt <- c(swiid_poq$gini_mkt[swiid_poq$cntry == "PL"])

# Slovenia
SI_disp <- c(swiid_poq$gini_disp[swiid_poq$cntry == "SI"], NA)
SI_mkt <- c(swiid_poq$gini_mkt[swiid_poq$cntry == "SI"], NA)



##### Defining the parameters for the entire figure
par(las = 1, mar = c(2.5, 2.75, 1.75, 2), mfrow = c(5, 4))

# Belgium
plot(x = years, y = BE_disp, ylim = c(20, 60), type = "o", las = 1, pch = 19, lty = 1, xaxt = "n",
     main = "Belgium (conservative)", cex = 1.1)
axis(1, at = c(2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018), 
     labels = c(2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018))
points(x = years, y = BE_mkt, type = "o", lty = 1, pch = 17, col = "gray", cex = 1.25)
axis(side = 2, at = c(25, 35, 45, 55), labels = FALSE, tck=-0.0325)

# Germany
plot(x = years, y = DE_disp, ylim = c(20, 60), type = "o", las = 1, pch = 19, lty = 1, xaxt = "n",
     main = "Germany (conservative)", cex = 1.1)
axis(1, at = c(2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018), 
     labels = c(2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018))
points(x = years, y = DE_mkt, type = "o", lty = 1, pch = 17, col = "gray", cex = 1.25)
axis(side = 2, at = c(25, 35, 45, 55), labels = FALSE, tck=-0.0325)

# France
plot(x = years, y = FR_disp, ylim = c(20, 60), type = "o", las = 1, pch = 19, lty = 1, xaxt = "n",
     main = "France (conservative)", cex = 1.1)
axis(1, at = c(2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018), 
     labels = c(2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018))
points(x = years, y = FR_mkt, type = "o", lty = 1, pch = 17, col = "gray", cex = 1.25)
axis(side = 2, at = c(25, 35, 45, 55), labels = FALSE, tck=-0.0325)

# Switzerland
plot(x = years, y = CH_disp, ylim = c(20, 60), type = "o", las = 1, pch = 19, lty = 1, xaxt = "n",
     main = "Switzerland (conservative)", cex = 1.1)
axis(1, at = c(2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018), 
     labels = c(2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018))
points(x = years, y = CH_mkt, type = "o", lty = 1, pch = 17, col = "gray", cex = 1.25)
axis(side = 2, at = c(25, 35, 45, 55), labels = FALSE, tck=-0.0325)

# Netherlands
plot(x = years, y = NL_disp, ylim = c(20, 60), type = "o", las = 1, pch = 19, lty = 1, xaxt = "n",
     main = "Netherlands (conservative)", cex = 1.1)
axis(1, at = c(2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018), 
     labels = c(2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018))
points(x = years, y = NL_mkt, type = "o", lty = 1, pch = 17, col = "gray", cex = 1.25)
axis(side = 2, at = c(25, 35, 45, 55), labels = FALSE, tck=-0.0325)



# Denmark
plot(x = years, y = DK_disp, ylim = c(20, 60), type = "o", las = 1, pch = 19, lty = 1, xaxt = "n",
     main = "Denmark (social-democratic)", cex = 1.1)
axis(1, at = c(2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018), 
     labels = c(2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018))
points(x = years, y = DK_mkt, type = "o", lty = 1, pch = 17, col = "gray", cex = 1.25)
axis(side = 2, at = c(25, 35, 45, 55), labels = FALSE, tck=-0.0325)

# Finland
plot(x = years, y = FI_disp, ylim = c(20, 60), type = "o", las = 1, pch = 19, lty = 1, xaxt = "n",
     main = "Finland (social-democratic)", cex = 1.1)
axis(1, at = c(2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018), 
     labels = c(2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018))
points(x = years, y = FI_mkt, type = "o", lty = 1, pch = 17, col = "gray", cex = 1.25)
axis(side = 2, at = c(25, 35, 45, 55), labels = FALSE, tck=-0.0325)

# Norway
plot(x = years, y = NO_disp, ylim = c(20, 60), type = "o", las = 1, pch = 19, lty = 1, xaxt = "n",
     main = "Norway (social-democratic)", cex = 1.1)
axis(1, at = c(2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018), 
     labels = c(2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018))
points(x = years, y = NO_mkt, type = "o", lty = 1, pch = 17, col = "gray", cex = 1.25)
axis(side = 2, at = c(25, 35, 45, 55), labels = FALSE, tck=-0.0325)

# Sweden
plot(x = years, y = SE_disp, ylim = c(20, 60), type = "o", las = 1, pch = 19, lty = 1, xaxt = "n",
     main = "Sweden (social-democratic)", cex = 1.1)
axis(1, at = c(2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018), 
     labels = c(2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018))
points(x = years, y = SE_mkt, type = "o", lty = 1, pch = 17, col = "gray", cex = 1.25)
axis(side = 2, at = c(25, 35, 45, 55), labels = FALSE, tck=-0.0325)



# United Kingdom
plot(x = years, y = GB_disp, ylim = c(20, 60), type = "o", las = 1, pch = 19, lty = 1, xaxt = "n",
     main = "United Kingdom (liberal)", cex = 1.1)
axis(1, at = c(2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018), 
     labels = c(2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018))
points(x = years, y = GB_mkt, type = "o", lty = 1, pch = 17, col = "gray", cex = 1.25)
axis(side = 2, at = c(25, 35, 45, 55), labels = FALSE, tck=-0.0325)

# Ireland
plot(x = years, y = IE_disp, ylim = c(20, 60), type = "o", las = 1, pch = 19, lty = 1, xaxt = "n",
     main = "Ireland (liberal)", cex = 1.1)
axis(1, at = c(2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018), 
     labels = c(2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018))
points(x = years, y = IE_mkt, type = "o", lty = 1, pch = 17, col = "gray", cex = 1.25)
axis(side = 2, at = c(25, 35, 45, 55), labels = FALSE, tck=-0.0325)



# Spain
plot(x = years, y = ES_disp, ylim = c(20, 60), type = "o", las = 1, pch = 19, lty = 1, xaxt = "n",
     main = "Spain (Mediterranean)", cex = 1.1)
axis(1, at = c(2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018), 
     labels = c(2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018))
points(x = years, y = ES_mkt, type = "o", lty = 1, pch = 17, col = "gray", cex = 1.25)
axis(side = 2, at = c(25, 35, 45, 55), labels = FALSE, tck=-0.0325)

# Portugal
plot(x = years, y = PT_disp, ylim = c(20, 60), type = "o", las = 1, pch = 19, lty = 1, xaxt = "n",
     main = "Portugal (Mediterranean)", cex = 1.1)
axis(1, at = c(2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018), 
     labels = c(2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018))
points(x = years, y = PT_mkt, type = "o", lty = 1, pch = 17, col = "gray", cex = 1.25)
axis(side = 2, at = c(25, 35, 45, 55), labels = FALSE, tck=-0.0325)



# Czech Republic
plot(x = years, y = CZ_disp, ylim = c(20, 60), type = "o", las = 1, pch = 19, lty = 1, xaxt = "n",
     main = "Czech Republic (post-communist)", cex = 1.1)
axis(1, at = c(2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018), 
     labels = c(2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018))
points(x = years, y = CZ_mkt, type = "o", lty = 1, pch = 17, col = "gray", cex = 1.25)
axis(side = 2, at = c(25, 35, 45, 55), labels = FALSE, tck=-0.0325)

# Estonia
plot(x = years, y = EE_disp, ylim = c(20, 60), type = "o", las = 1, pch = 19, lty = 1, xaxt = "n",
     main = "Estonia (post-communist)", cex = 1.1)
axis(1, at = c(2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018), 
     labels = c(2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018))
points(x = years, y = EE_mkt, type = "o", lty = 1, pch = 17, col = "gray", cex = 1.25)
axis(side = 2, at = c(25, 35, 45, 55), labels = FALSE, tck=-0.0325)

# Hungary
plot(x = years, y = HU_disp, ylim = c(20, 60), type = "o", las = 1, pch = 19, lty = 1, xaxt = "n",
     main = "Hungary (post-communist)", cex = 1.1)
axis(1, at = c(2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018), 
     labels = c(2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018))
points(x = years, y = HU_mkt, type = "o", lty = 1, pch = 17, col = "gray", cex = 1.25)
axis(side = 2, at = c(25, 35, 45, 55), labels = FALSE, tck=-0.0325)

# Poland
plot(x = years, y = PL_disp, ylim = c(20, 60), type = "o", las = 1, pch = 19, lty = 1, xaxt = "n",
     main = "Poland (post-communist)", cex = 1.1)
axis(1, at = c(2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018), 
     labels = c(2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018))
points(x = years, y = PL_mkt, type = "o", lty = 1, pch = 17, col = "gray", cex = 1.25)
axis(side = 2, at = c(25, 35, 45, 55), labels = FALSE, tck=-0.0325)


# Slovenia
plot(x = years, y = SI_disp, ylim = c(20, 60), type = "o", las = 1, pch = 19, lty = 1, xaxt = "n",
     main = "Slovenia (post-communist)", cex = 1.1)
axis(1, at = c(2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018), 
     labels = c(2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018))
points(x = years, y = SI_mkt, type = "o", lty = 1, pch = 17, col = "gray", cex = 1.25)
axis(side = 2, at = c(25, 35, 45, 55), labels = FALSE, tck=-0.0325)

plot.new()
par(las = 1, mar = c(2.5, 0, 1.75, 0))
legend("topleft", legend = c("Gini: household market income", "Gini: household disposable income"), 
       bty = "n", ncol = 1, pch = c(17, 19), lty = c(1, 1),
       col = c("gray", "black"), x.intersp = 1.3, text.width = 2.2, y.intersp = 1.5,
       text.col = c("gray", "black"), pt.cex =  c(1.4, 1.4), cex = 1.07,
       lwd = c(1.5, 1.5))





########## FIGURE A3: Public social spending (% of GDP) by five welfare regime types, 2002-2018/19
### Loading the OECD data on Social spending, 2022 (downloaded from: https://data.oecd.org/socialexp/social-spending.htm)
soc_exp_sub_full <- read.csv("DP_LIVE_12042022195739284.csv", sep = ",")



### subsetting the 18 countries
soc_exp_sub_sub <- soc_exp_sub_full[soc_exp_sub_full[, 1] == "BEL" | soc_exp_sub_full[, 1] == "FRA" |
                                            soc_exp_sub_full[, 1] == "NLD" | soc_exp_sub_full[, 1] == "DEU" |
                                            soc_exp_sub_full[, 1] == "CHE" |
                                            soc_exp_sub_full[, 1] == "DNK" | soc_exp_sub_full[, 1] == "NOR" |
                                            soc_exp_sub_full[, 1] == "FIN" | soc_exp_sub_full[, 1] == "SWE" |
                                            soc_exp_sub_full[, 1] == "GBR" | soc_exp_sub_full[, 1] == "IRL" |
                                            soc_exp_sub_full[, 1] == "ESP" | soc_exp_sub_full[, 1] == "PRT" |
                                            soc_exp_sub_full[, 1] == "CZE" | soc_exp_sub_full[, 1] == "HUN" |
                                            soc_exp_sub_full[, 1] == "SVN" | soc_exp_sub_full[, 1] == "EST" |
                                            soc_exp_sub_full[, 1] == "POL",
                                    c(1, 6, 7)]

dim(soc_exp_sub_sub)
setDT(soc_exp_sub_sub)

check <- soc_exp_sub_sub[, .(minimum = min(TIME), maximum = max(TIME)), by = "ï.¿LOCATION"]
table(check$minimum)
# CHE (Switzerland ends in 2018)
table(check$maximum)

soc_exp_sub <- soc_exp_sub_sub
colnames(soc_exp_sub) <- c("cntry", "TIME", "Value")

# Range of public expenditure
range(soc_exp_sub_sub$Value)
mean_soc_exp <- soc_exp_sub_sub[, .(mean_expenditure = mean(Value)), by = "ï.¿LOCATION"]


##### PDF Size = Device Size: 10.60 x 8.04

par(mar = c(3, 2.5, 2.5, 1), mfrow = c(3, 2))
### 1 Conservative regime
plot(x = seq(from = 2002, to = 2019, by = 1), xlim = c(2002, 2019), ylim = c(9, 33), 
     xaxp = c(2002, 2019, 17), type = "n", las = 1, xlab = "", ylab = "",
     main = "Conservative regime")
axis(side = 4, at = seq(from = 10, to = 35, by = 5), tck = 0.03, labels = FALSE)

# Belgium
points(x = seq(from = 2002, to = 2019, by = 1), 
       y = as.numeric(as.matrix(soc_exp_sub[soc_exp_sub$cntry == "BEL", 3])),
       pch = 19, cex = 1.15)
points(x = seq(from = 2002, to = 2019, by = 1), 
       y = as.numeric(as.matrix(soc_exp_sub[soc_exp_sub$cntry == "BEL", 3])),
       pch = 19, type = "l", lty = 3, lwd = 2)

# Germany
points(x = seq(from = 2002, to = 2019, by = 1), 
       y = as.numeric(as.matrix(soc_exp_sub[soc_exp_sub$cntry == "DEU", 3])),
       pch = 15, col = "seagreen", cex = 1.1)
points(x = seq(from = 2002, to = 2019, by = 1), 
       y = as.numeric(as.matrix(soc_exp_sub[soc_exp_sub$cntry == "DEU", 3])),
       pch = 15, col = "seagreen", type = "l", lty = 1, lwd = 1.75)

# France
points(x = seq(from = 2002, to = 2019, by = 1),
       y = as.numeric(as.matrix(soc_exp_sub[soc_exp_sub$cntry == "FRA", 3])),
       pch = 17, col = "salmon", cex = 1.1)
points(x = seq(from = 2002, to = 2019, by = 1), 
       y = as.numeric(as.matrix(soc_exp_sub[soc_exp_sub$cntry == "FRA", 3])),
       pch = 17, col = "salmon", type = "l", lty = 5, lwd = 1.75)

# Switzerland
points(x = seq(from = 2002, to = 2019, by = 1), 
       y = c(as.numeric(as.matrix(soc_exp_sub[soc_exp_sub$cntry == "CHE", 3])), NA),
       pch = 18, col = "steelblue1", cex = 1.3)
points(x = seq(from = 2002, to = 2019, by = 1), 
       y = c(as.numeric(as.matrix(soc_exp_sub[soc_exp_sub$cntry == "CHE", 3])), NA),
       pch = 18, col = "steelblue1", type = "l", lty = 2, lwd = 1.75)

# Netherlands
points(x = seq(from = 2002, to = 2019, by = 1), 
       y = as.numeric(as.matrix(soc_exp_sub[soc_exp_sub$cntry == "NLD", 3])),
       pch = 1, col = "gray50", cex = 1.2)
points(x = seq(from = 2002, to = 2019, by = 1), 
       y = as.numeric(as.matrix(soc_exp_sub[soc_exp_sub$cntry == "NLD", 3])),
       pch = 1, col = "gray50", type = "l", lty = 6, lwd = 1.75)

legend("bottom", legend = c("Belgium (27.5%)", "Germany (25.4%)", "France (30.3%)", "Switzerland (16.0%)", "Netherlands (17.7%)"), 
       bty = "n", ncol = 3, pch = c(19, 15, 17, 18, 1), lty = c(3, 1, 5, 2, 6), cex = 1.1,
       col = c("black", "seagreen", "salmon", "steelblue1", "gray50"), x.intersp = 1.35, text.width = 4.15, y.intersp = 1.25,
       text.col = c("black", "seagreen", "salmon", "steelblue1", "gray50"), pt.cex =  c(1.15, 1.1, 1.1, 1.3, 1.2),
       lwd = c(1.5, 1.5, 1.5, 1.5, 1.5))


### 2 Social-democratic regime
plot(x = seq(from = 2002, to = 2019, by = 1), xlim = c(2002, 2019), ylim = c(9, 33), 
     xaxp = c(2002, 2019, 17), type = "n", las = 1, xlab = "", ylab = "",
     main = "Social-democratic regime")
axis(side = 4, at = seq(from = 10, to = 35, by = 5), tck = 0.03, labels = FALSE)

# Denmark
points(x = seq(from = 2002, to = 2019, by = 1), 
       y = as.numeric(as.matrix(soc_exp_sub[soc_exp_sub$cntry == "DNK", 3])),
       pch = 19, cex = 1.15)
points(x = seq(from = 2002, to = 2019, by = 1),
       y = as.numeric(as.matrix(soc_exp_sub[soc_exp_sub$cntry == "DNK", 3])),
       pch = 19, type = "l", lty = 3, lwd = 2)

# Finland
points(x = seq(from = 2002, to = 2019, by = 1), 
       y = as.numeric(as.matrix(soc_exp_sub[soc_exp_sub$cntry == "FIN", 3])),
       pch = 15, col = "seagreen", cex = 1.1)
points(x = seq(from = 2002, to = 2019, by = 1), 
       y = as.numeric(as.matrix(soc_exp_sub[soc_exp_sub$cntry == "FIN", 3])),
       pch = 15, col = "seagreen", type = "l", lty = 1, lwd = 1.75)

# Norway
points(x = seq(from = 2002, to = 2019, by = 1), 
       y = as.numeric(as.matrix(soc_exp_sub[soc_exp_sub$cntry == "NOR", 3])),
       pch = 17, col = "salmon", cex = 1.1)
points(x = seq(from = 2002, to = 2019, by = 1), 
       y = as.numeric(as.matrix(soc_exp_sub[soc_exp_sub$cntry == "NOR", 3])),
       pch = 17, col = "salmon", type = "l", lty = 5, lwd = 1.75)

# Sweden
points(x = seq(from = 2002, to = 2019, by = 1), 
       y = as.numeric(as.matrix(soc_exp_sub[soc_exp_sub$cntry == "SWE", 3])),
       pch = 18, col = "steelblue1", cex = 1.3)
points(x = seq(from = 2002, to = 2019, by = 1), 
       y = as.numeric(as.matrix(soc_exp_sub[soc_exp_sub$cntry == "SWE", 3])),
       pch = 18, col = "steelblue1", type = "l", lty = 2, lwd = 1.75)

legend("bottom", legend = c("Denmark (27.9%)", "Finland (26.8%)", "Norway (22.6%)", "Sweden (26.4%)"), 
       bty = "n", ncol = 2, pch = c(19, 15, 17, 18), lty = c(3, 1, 5, 2), cex = 1.1,
       col = c("black", "seagreen", "salmon", "steelblue1"), x.intersp = 1.35, text.width = 4.25, y.intersp = 1.25,
       text.col = c("black", "seagreen", "salmon", "steelblue1"), pt.cex =  c(1.15, 1.1, 1.1, 1.3),
       lwd = c(1.5, 1.5, 1.5, 1.5))



### Liberal regime
plot(x = seq(from = 2002, to = 2019, by = 1), xlim = c(2002, 2019), ylim = c(9, 33), 
     xaxp = c(2002, 2019, 17), type = "n", las = 1, xlab = "", ylab = "",
     main = "Liberal regime")
axis(side = 4, at = seq(from = 10, to = 35, by = 5), tck = 0.03, labels = FALSE)

# United Kingdom
points(x = seq(from = 2002, to = 2019, by = 1), 
       y = as.numeric(as.matrix(soc_exp_sub[soc_exp_sub$cntry == "GBR", 3])),
       pch = 19, cex = 1.15)
points(x = seq(from = 2002, to = 2019, by = 1),
       y = as.numeric(as.matrix(soc_exp_sub[soc_exp_sub$cntry == "GBR", 3])),
       pch = 19, type = "l", lty = 3, lwd = 2)

# Ireland
points(x = seq(from = 2002, to = 2019, by = 1), 
       y = as.numeric(as.matrix(soc_exp_sub[soc_exp_sub$cntry == "IRL", 3])),
       pch = 15, col = "gray50", cex = 1.1)
points(x = seq(from = 2002, to = 2019, by = 1), 
       y = as.numeric(as.matrix(soc_exp_sub[soc_exp_sub$cntry == "IRL", 3])),
       pch = 15, col = "gray50", type = "l", lty = 1, lwd = 1.75)

legend("bottom", legend = c("United Kingdom (20.9%)", "Ireland (17.5%)"), 
       bty = "n", ncol = 2, pch = c(19, 15), lty = c(3, 1), cex = 1.15,
       col = c("black", "gray50"), x.intersp = 1.35, text.width = 6, y.intersp = 1.15,
       text.col = c("black", "gray50"), pt.cex =  c(1.15, 1.1),
       lwd = c(1.5, 1.5))



### Mediterranean regime
plot(x = seq(from = 2002, to = 2019, by = 1), xlim = c(2002, 2019), ylim = c(9, 33), 
     xaxp = c(2002, 2019, 17), type = "n", las = 1, xlab = "", ylab = "",
     main = "Mediterranean regime")
axis(side = 4, at = seq(from = 10, to = 35, by = 5), tck = 0.03, labels = FALSE)

# Spain
points(x = seq(from = 2002, to = 2019, by = 1), 
       y = as.numeric(as.matrix(soc_exp_sub[soc_exp_sub$cntry == "ESP", 3])),
       pch = 17, col = "salmon", cex = 1.1)
points(x = seq(from = 2002, to = 2019, by = 1),
       y = as.numeric(as.matrix(soc_exp_sub[soc_exp_sub$cntry == "ESP", 3])),
       pch = 17, col = "salmon", type = "l", lty = 5, lwd = 1.75)

# Portugal
points(x = seq(from = 2002, to = 2019, by = 1), 
       y = as.numeric(as.matrix(soc_exp_sub[soc_exp_sub$cntry == "PRT", 3])),
       pch = 18, col = "steelblue1", cex = 1.3)
points(x = seq(from = 2002, to = 2019, by = 1), 
       y = as.numeric(as.matrix(soc_exp_sub[soc_exp_sub$cntry == "PRT", 3])),
       pch = 18, col = "steelblue1", type = "l", lty = 2, lwd = 1.75)

legend("bottom", legend = c("Spain (23.2%)", "Portugal (23.8%)"), 
       bty = "n", ncol = 2, pch = c(17, 18), lty = c(5, 2), cex = 1.15,
       col = c("salmon", "steelblue1"), x.intersp = 1.35, text.width = 4.15, y.intersp = 1.15,
       text.col = c("salmon", "steelblue1"), pt.cex =  c(1.1, 1.3), lwd = c(1.5, 1.5))



### Post-Communist regime
plot(x = seq(from = 2002, to = 2019, by = 1), xlim = c(2002, 2019), ylim = c(9, 33), 
     xaxp = c(2002, 2019, 17), type = "n", las = 1, xlab = "", ylab = "",
     main = "Post-Communist regime")
axis(side = 4, at = seq(from = 10, to = 35, by = 5), tck = 0.03, labels = FALSE)

# Czech Republic
points(x = seq(from = 2002, to = 2019, by = 1), 
       y = as.numeric(as.matrix(soc_exp_sub[soc_exp_sub$cntry == "CZE", 3])),
       pch = 19, cex = 1.15)
points(x = seq(from = 2002, to = 2019, by = 1), 
       y = as.numeric(as.matrix(soc_exp_sub[soc_exp_sub$cntry == "CZE", 3])),
       pch = 19, type = "l", lty = 3, lwd = 2)

# Estonia
points(x = seq(from = 2002, to = 2019, by = 1), 
       y = as.numeric(as.matrix(soc_exp_sub[soc_exp_sub$cntry == "EST", 3])),
       pch = 15, col = "seagreen", cex = 1.1)
points(x = seq(from = 2002, to = 2019, by = 1), 
       y = as.numeric(as.matrix(soc_exp_sub[soc_exp_sub$cntry == "EST", 3])),
       pch = 15, col = "seagreen", type = "l", lty = 1, lwd = 1.75)

# Hungary
points(x = seq(from = 2002, to = 2019, by = 1), 
       y = as.numeric(as.matrix(soc_exp_sub[soc_exp_sub$cntry == "HUN", 3])),
       pch = 17, col = "salmon", cex = 1.1)
points(x = seq(from = 2002, to = 2019, by = 1), 
       y = as.numeric(as.matrix(soc_exp_sub[soc_exp_sub$cntry == "HUN", 3])),
       pch = 17, col = "salmon", type = "l", lty = 5, lwd = 1.75)

# Poland
points(x = seq(from = 2002, to = 2019, by = 1), 
       y = as.numeric(as.matrix(soc_exp_sub[soc_exp_sub$cntry == "POL", 3])),
       pch = 18, col = "steelblue1", cex = 1.3)
points(x = seq(from = 2002, to = 2019, by = 1), 
       y = as.numeric(as.matrix(soc_exp_sub[soc_exp_sub$cntry == "POL", 3])),
       pch = 18, col = "steelblue1", type = "l", lty = 2, lwd = 1.75)

# Slovenia
points(x = seq(from = 2002, to = 2019, by = 1), 
       y = as.numeric(as.matrix(soc_exp_sub[soc_exp_sub$cntry == "SVN", 3])),
       pch = 1, col = "gray50", cex = 1.2)
points(x = seq(from = 2002, to = 2019, by = 1), 
       y = as.numeric(as.matrix(soc_exp_sub[soc_exp_sub$cntry == "SVN", 3])),
       pch = 1, col = "gray50", type = "l", lty = 6, lwd = 1.75)

legend("top", legend = c("Czech R. (18.9%)", "Estonia (15.5%)", "Hungary (21.4%)", "Poland (20.7%)", "Slovenia (22.0%)"), 
       bty = "n", ncol = 3, pch = c(19, 15, 17, 18, 1), lty = c(3, 1, 5, 2, 6), cex = 1.1,
       col = c("black", "seagreen", "salmon", "steelblue1", "gray50"), x.intersp = 1.5, text.width = 4.45, y.intersp = 1.25,
       text.col = c("black", "seagreen", "salmon", "steelblue1", "gray50"), pt.cex =  c(1.15, 1.1, 1.1, 1.3, 1.2),
       lwd = c(1.5, 1.5, 1.5, 1.5, 1.5))

conservative <- rbind(as.numeric(as.matrix(soc_exp_sub[soc_exp_sub$cntry == "BEL", 3])),
                      as.numeric(as.matrix(soc_exp_sub[soc_exp_sub$cntry == "DEU", 3])),
                      as.numeric(as.matrix(soc_exp_sub[soc_exp_sub$cntry == "FRA", 3])),
                      c(as.numeric(as.matrix(soc_exp_sub[soc_exp_sub$cntry == "CHE", 3])), NA),
                      as.numeric(as.matrix(soc_exp_sub[soc_exp_sub$cntry == "NLD", 3])))

social_dem <- rbind(as.numeric(as.matrix(soc_exp_sub[soc_exp_sub$cntry == "DNK", 3])),
                    as.numeric(as.matrix(soc_exp_sub[soc_exp_sub$cntry == "FIN", 3])),
                    as.numeric(as.matrix(soc_exp_sub[soc_exp_sub$cntry == "NOR", 3])),
                    as.numeric(as.matrix(soc_exp_sub[soc_exp_sub$cntry == "SWE", 3])))

lib <- rbind(as.numeric(as.matrix(soc_exp_sub[soc_exp_sub$cntry == "GBR", 3])),
             as.numeric(as.matrix(soc_exp_sub[soc_exp_sub$cntry == "IRL", 3])))

medit <- rbind(as.numeric(as.matrix(soc_exp_sub[soc_exp_sub$cntry == "ESP", 3])),
               as.numeric(as.matrix(soc_exp_sub[soc_exp_sub$cntry == "PRT", 3])))

commun <- rbind(as.numeric(as.matrix(soc_exp_sub[soc_exp_sub$cntry == "CZE", 3])),
                as.numeric(as.matrix(soc_exp_sub[soc_exp_sub$cntry == "EST", 3])),
                as.numeric(as.matrix(soc_exp_sub[soc_exp_sub$cntry == "HUN", 3])),
                as.numeric(as.matrix(soc_exp_sub[soc_exp_sub$cntry == "POL", 3])),
                as.numeric(as.matrix(soc_exp_sub[soc_exp_sub$cntry == "SVN", 3])))


### 6 Regime Averages
plot(x = seq(from = 2002, to = 2019, by = 1), xlim = c(2002, 2019), ylim = c(9, 33), 
     xaxp = c(2002, 2019, 17), type = "n", las = 1, xlab = "", ylab = "",
     main = "Average public social spending by welfare regimes")
axis(side = 4, at = seq(from = 10, to = 35, by = 5), tck = 0.03, labels = FALSE)

# Conservative
points(x = seq(from = 2002, to = 2019, by = 1),
       y = apply(X = conservative, MARGIN = 2, mean, na.rm = TRUE),
       pch = 19, col = "dodgerblue3", cex = 1.1)
points(x = seq(from = 2002, to = 2019, by = 1), 
       y = apply(X = conservative, MARGIN = 2, mean, na.rm = TRUE),
       pch = 19, col = "dodgerblue3", type = "l", lty = 3, lwd = 1.75)

# Social-Democratic
points(x = seq(from = 2002, to = 2019, by = 1),
       y = apply(X = social_dem, MARGIN = 2, mean, na.rm = TRUE),
       pch = 17, col = "salmon", cex = 1.1)
points(x = seq(from = 2002, to = 2019, by = 1), 
       y = apply(X = social_dem, MARGIN = 2, mean, na.rm = TRUE),
       pch = 17, col = "salmon", type = "l", lty = 5, lwd = 1.75)

# Liberal
points(x = seq(from = 2002, to = 2019, by = 1),
       y = apply(X = lib, MARGIN = 2, mean, na.rm = TRUE),
       pch = 18, col = "gold", cex = 1.1)
points(x = seq(from = 2002, to = 2019, by = 1), 
       y = apply(X = lib, MARGIN = 2, mean, na.rm = TRUE),
       pch = 18, col = "gold", type = "l", lty = 2, lwd = 1.75)

# Mediterranean
points(x = seq(from = 2002, to = 2019, by = 1),
       y = apply(X = medit, MARGIN = 2, mean, na.rm = TRUE),
       pch = 17, col = "lightskyblue", cex = 1.1)
points(x = seq(from = 2002, to = 2019, by = 1), 
       y = apply(X = medit, MARGIN = 2, mean, na.rm = TRUE),
       pch = 17, col = "lightskyblue", type = "l", lty = 3, lwd = 1.75)

# Post-Communist
points(x = seq(from = 2002, to = 2019, by = 1),
       y = apply(X = commun, MARGIN = 2, mean, na.rm = TRUE),
       pch = 19, col = "red3", cex = 1.1)
points(x = seq(from = 2002, to = 2019, by = 1), 
       y = apply(X = commun, MARGIN = 2, mean, na.rm = TRUE),
       pch = 19, col = "red3", type = "l", lty = 2, lwd = 1.75)

legend("bottom", legend = c("Conservative (23.5%)", "Social-dem. (25.9%)", "Liberal (19.2%)", "Mediterranean (23.2%)", "Post-communist (19.7%)"), 
       bty = "n", ncol = 3, pch = c(19, 17, 18, 17, 19), lty = c(3, 5, 2, 3, 2), cex = 1.05,
       col = c("dodgerblue3", "salmon", "gold", "lightskyblue", "red3"), x.intersp = 1, text.width = 4.7, y.intersp = 1.3,
       text.col = c("dodgerblue3", "salmon", "gold", "lightskyblue", "red3"), pt.cex =  c(1.5, 1.4, 1.6, 1.4, 1.5),
       lwd = c(2, 2, 2, 2, 2, 2))

mean(apply(X = conservative, MARGIN = 2, mean, na.rm = TRUE))
mean(apply(X = social_dem, MARGIN = 2, mean, na.rm = TRUE))
mean(apply(X = lib, MARGIN = 2, mean, na.rm = TRUE))
mean(apply(X = medit, MARGIN = 2, mean, na.rm = TRUE))
mean(apply(X = commun, MARGIN = 2, mean, na.rm = TRUE))

