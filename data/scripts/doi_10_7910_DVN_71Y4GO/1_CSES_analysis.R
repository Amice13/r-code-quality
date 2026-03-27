require(survey)

CSES5 <- read.csv("cses5.csv")

## create populist attitude scores
## align the direction of items so that a higher value represents higher populist attitudes
# Q1: What people call compromise in politics is really just selling out on one's principles
CSES5$PAS1 <- ifelse(CSES5$E3004_1 > 5, NA, 6 - CSES5$E3004_1)
# Q2: Most politicians do not care about the people.
CSES5$PAS2 <- ifelse(CSES5$E3004_2 > 5, NA, 6 - CSES5$E3004_2)
# Q3: Most politicians are trustworthy. (reversed)
CSES5$PAS3 <- ifelse(CSES5$E3004_3 > 5, NA, CSES5$E3004_3)
# Q4: Politicians are the main problem in [COUNTRY].
CSES5$PAS4 <- ifelse(CSES5$E3004_4 > 5, NA, 6 - CSES5$E3004_4)
# Q5: The people, and not politicians, should make our most important policy decisions.
CSES5$PAS5 <- ifelse(CSES5$E3004_6 > 5, NA, 6 - CSES5$E3004_6)
# Q6: Most politicians care only about the interests of the rich and powerful.
CSES5$PAS6 <- ifelse(CSES5$E3004_7 > 5, NA, 6 - CSES5$E3004_7)
# calculate the average score from Q1 to Q6
CSES5$PAS7 <- rowMeans(cbind(CSES5$PAS1, CSES5$PAS2, CSES5$PAS3, 
                             CSES5$PAS4, CSES5$PAS5, CSES5$PAS6), na.rm = TRUE)

## estimate the mean and 95% confidence interval
# remove surveys that are not the latest for each country
survey.list <- unique(CSES5$E1004)
survey.list <- survey.list[! (survey.list %in% c("ISL_2016", "NZL_2017", "TWN_2016", "USA_2016"))]
# count the number of countries and regions
n.countries <- length(survey.list)
n.countries

variable.list <- paste0("PAS", 1:7)

PAS.mean.list <- list()
for (j in 1:7) {
  PAS.mean.list[[j]] <- data.frame(survey = survey.list, 
                                   mean = NA, lower = NA, upper = NA)
}

for (i in 1:length(survey.list)) {
  country.data <- subset(CSES5, E1004 == survey.list[i])
  # use weights if available
  survey.design <- svydesign(~ 1, 
                             weights = country.data$E1010_1 * 
                               country.data$E1010_2, 
                             data = country.data)
  for (j in 1:7) {
    survey.mean <- svymean(formula(paste("~", variable.list[j])), 
                           survey.design, na.rm = TRUE)
    survey.confint <- confint(survey.mean)
    PAS.mean.list[[j]]$mean[i] <- survey.mean[1]
    PAS.mean.list[[j]]$lower[i] <- survey.confint[1]
    PAS.mean.list[[j]]$upper[i] <- survey.confint[2]
  }
}

## Figure 1
country.order <- order(PAS.mean.list[[7]]$mean)
country.name <- c("AU", "AT", "BF", "BW", "BR", "CA", "CH", "CL", "CR", 
                  "DE", "DK", "FI", "FR", "GB", "GR", "HK", "HU", "IE", 
                  "IS", "IL", "IT", "JP", "KR", "LT", "ME", "NL", "NO", 
                  "NZ", "PT", "SK", "SE", "TH", "TN", "TR", "TW", "UY", "US")
questions <- c("Q1: What people call compromise in politics is\nreally just selling out on one's principles.", 
               "Q2: Most politicians do not care about the people.\n", 
               "Q3: Most politicians are trustworthy. (reversed)\n", 
               "Q4: Politicians are the main problem in [COUNTRY].\n", 
               "Q5: The people, and not politicians, should make\nour most important policy decisions.", 
               "Q6: Most politicians care only about the interests of\nthe rich and powerful.", 
               "Average score of Q1 to Q6\n")

png("Figure_1.png", width = 6.5, height = 4, units = "in", pointsize = 7, res = 1200)
layout(matrix(1:8, 4, 2, byrow = TRUE))
par(mar = c(4, 2, 3, 2), lwd = 0.5)
for (i in 1:7) {
  plot(NULL, NULL, type = "n", bty = "n", xlim = c(1, n.countries), ylim = c(1, 5), 
       xlab = "", ylab = "", xaxt = "n", yaxt = "n")
  abline(h = c(1, 2, 4, 5), lty = 3, col = "gray")
  abline(h = 3, col = "gray")
  segments(1:n.countries, PAS.mean.list[[i]]$lower[country.order], 
           1:n.countries, PAS.mean.list[[i]]$upper[country.order])
  points(1:n.countries, PAS.mean.list[[i]]$mean[country.order], 
         pch = ifelse(country.order == 22, 21, 19), 
         bg = ifelse(country.order == 22, "white", NA))
  axis(1, at = 1:n.countries, labels = NA, lwd = 0.5)
  mtext(country.name[country.order], side = 1, at = 1:n.countries, line = 1, cex = 0.8, las = 2)
  axis(2, lwd = 0.5)
  mtext(questions[i], font = 2)
}
dev.off()

## description of the results in the main text
# count the number of countries where the overall average score exceeds the midpoint
sum(PAS.mean.list[[7]]$mean > 3)

# Japan's rank
which(country.order == which(country.name == "JP"))