##############################################
### A Survey Inquiry into Behavioral Foundations of Hate Speech Regulations: Evidence from Japan
### Kentaro Hirose, Hae Kim, Masaru Kohno
### Japanese Journal of Political Science
##############################################
library(scales)
library(lattice)
library(gridExtra)
library(sandwich)
library(robust)
library(mediation)
library(stargazer)
##############################################
data <- read.csv("hate.csv")
data.sub <- data[data$satisficer == 0, ] ## data without satisficers
##############################################



################################################
### Figure 1
### Correlation between the perceived harm to feelings and the perceived harm to dignity
################################################
data.sub.x <- with(data.sub, data.frame(discomfort, indignity))
data.sub.x <- na.omit(data.sub.x)
################################################
COL.VEC <- seq(0, 0.55, by = 0.001)
x <- seq(1, 5, length.out = 5)
y <- seq(1, 5, length.out = 5)
tab1 <- table(data.sub.x$indignity, data.sub.x$discomfort)/nrow(data.sub.x)
DATA <- expand.grid(X=x, Y=y)
DATA$Z <- as.vector(tab1)
XLAB <- list("Harm to Dignity", cex = 1.2, font = 1)
YLAB <- list("Harm to Feelings", cex = 1.2, font = 1)
MAIN <- list("Proportion", cex = 1.2, font = 1)
plot <- levelplot(Z ~ X * Y, data = DATA, main = MAIN, xlab = XLAB, ylab = YLAB, col.regions = colorRampPalette(c("white", "darkgray")), at = COL.VEC,
                  panel = function(x, y, ...){
                      panel.levelplot(x, y, ...)
                      panel.abline(v = 1.5, lty = 1)
                      panel.abline(v = 2.5, lty = 1)
                      panel.abline(v = 3.5, lty = 1)
                      panel.abline(v = 4.5, lty = 1)
                      panel.abline(h = 1.5, lty = 1)
                      panel.abline(h = 2.5, lty = 1)
                      panel.abline(h = 3.5, lty = 1)
                      panel.abline(h = 4.5, lty = 1)
                      ltext(x = x, y = y, label = round(tab1, 2))
                  }
                  )
plot
################################################





################################################
### Table 1
### Sources of the perceived harms of hate speech
################################################
MODEL <- list(NA, 2)
SE <- list(NA, 2)
################################################
model <- lm(discomfort ~ male + age + ideology + free.speech + victim + college + rich, data = data.sub)
MODEL[[1]] <- model
SE[[1]] <- sqrt(diag(vcovHC(model)))   

model <- lm(indignity ~ male + age + ideology + free.speech + victim + college + rich, data = data.sub)
MODEL[[2]] <- model
SE[[2]] <- sqrt(diag(vcovHC(model)))   
################################################
stargazer(MODEL[[1]], MODEL[[2]], se = list(SE[[1]], SE[[2]]), star.char = c("*", "**", "***"), star.cutoffs = c(.05, .01, .001))
################################################





################################################
### Figure 2
### Distribution of support for hate speech regulations
################################################
barplot(table(data.sub$regulation), xlab = "Support for Hate Speech Regulations", ylab = "Frequency")
################################################






################################################
### Figure 3
### Correlation between societal concern and the sense of injustice
################################################
data.sub.x <- with(data.sub, data.frame(bad.influence, injustice))
data.sub.x <- na.omit(data.sub.x)
################################################
COL.VEC <- seq(0, 0.5, by = 0.001)
x <- seq(1, 5, length.out = 5)
y <- seq(1, 5, length.out = 5)
tab1 <- table(data.sub.x$bad.influence, data.sub.x$injustice)/nrow(data.sub.x)
DATA <- expand.grid(X=x, Y=y)
DATA$Z <- as.vector(tab1)
XLAB <- list("Societal Concern", cex = 1.2, font = 1)
YLAB <- list("Sense of Injustice", cex = 1.2, font = 1)
MAIN <- list("Proportion", cex = 1.2, font = 1)
plot <- levelplot(Z ~ X * Y, data = DATA, main = MAIN, xlab = XLAB, ylab = YLAB, col.regions = colorRampPalette(c("white", "darkgray")), at = COL.VEC,
                  panel = function(x, y, ...){
                      panel.levelplot(x, y, ...)
                      panel.abline(v = 1.5, lty = 1)
                      panel.abline(v = 2.5, lty = 1)
                      panel.abline(v = 3.5, lty = 1)
                      panel.abline(v = 4.5, lty = 1)
                      panel.abline(h = 1.5, lty = 1)
                      panel.abline(h = 2.5, lty = 1)
                      panel.abline(h = 3.5, lty = 1)
                      panel.abline(h = 4.5, lty = 1)
                      ltext(x = x, y = y, label = round(tab1, 2))
                  }
                  )
plot
################################################







################################################
### Table 2
### Perceived harms, support for regulations, and mechanisms
################################################
MODEL <- list(NA, 4)
SE <- list(NA, 4)
################################################
model <- lm(regulation ~ discomfort + indignity + male + age + ideology + free.speech + victim + college + rich, data = data.sub)
MODEL[[1]] <- model
SE[[1]] <- sqrt(diag(vcovHC(model)))        
################################################
model <- lm(bad.influence ~ discomfort + indignity + male + age + ideology + free.speech + victim + college + rich, data = data.sub)
MODEL[[2]] <- model
SE[[2]] <- sqrt(diag(vcovHC(model)))        
################################################
model <- lm(injustice ~ discomfort + indignity + male + age + ideology + free.speech + victim + college + rich, data = data.sub)
MODEL[[3]] <- model
SE[[3]] <- sqrt(diag(vcovHC(model)))       
################################################
model <- lm(regulation ~ discomfort + indignity + bad.influence + injustice + male + age + ideology + free.speech + victim + college + rich, data = data.sub)
MODEL[[4]] <- model
SE[[4]] <- sqrt(diag(vcovHC(model)))        
################################################
stargazer(MODEL[[1]], MODEL[[2]], MODEL[[3]], MODEL[[4]], se = list(SE[[1]], SE[[2]], SE[[3]], SE[[4]]), star.char = c("*", "**", "***"), star.cutoffs = c(.05, .01, .001))
################################################






################################################
## Footnote 13
## mediation analysis
################################################
set.seed(1)
################################################
data.sub.x <- with(data.sub, data.frame(male, age, college, rich, victim, ideology, free.speech, discomfort, indignity, bad.influence, injustice, regulation))
data.sub.x <- na.omit(data.sub.x)
################################################      
model.out <- lm(regulation ~ discomfort + indignity + injustice + bad.influence + male + age + college + rich + victim + ideology + free.speech, data = data.sub.x)
        
model.med.1 <- lm(injustice ~ discomfort + indignity + male + age + college + rich + victim + ideology + free.speech, data = data.sub.x)
        
model.med.2 <- lm(bad.influence ~ discomfort + indignity + male + age + college + rich + victim + ideology + free.speech, data = data.sub.x)
################################################
## indignity -> injustice
out.indignity.injustice <- mediate(model.m = model.med.1, model.y = model.out, treat = "indignity", mediator = "injustice", control.value = 4, treat.value = 5, robustSE = TRUE, sims = 1000)
        
## indignity -> bad.influence
out.indignity.bad.influence <- mediate(model.m = model.med.2, model.y = model.out, treat = "indignity", mediator = "bad.influence", control.value = 4, treat.value = 5, robustSE = TRUE, sims = 1000)
################################################
summary(out.indignity.bad.influence)
summary(out.indignity.injustice)
################################################






################################################
## Table A1 (Appendix) 
## Results of linear regression models including satisficers
################################################
MODEL <- list(NA, 4)
SE <- list(NA, 4)
################################################
model <- lm(regulation ~ discomfort + indignity + male + age + ideology + free.speech + victim + college + rich, data = data)
MODEL[[1]] <- model
SE[[1]] <- sqrt(diag(vcovHC(model)))        
################################################
model <- lm(bad.influence ~ discomfort + indignity + male + age + ideology + free.speech + victim + college + rich, data = data)
MODEL[[2]] <- model
SE[[2]] <- sqrt(diag(vcovHC(model)))        
################################################
model <- lm(injustice ~ discomfort + indignity + male + age + ideology + free.speech + victim + college + rich, data = data)
MODEL[[3]] <- model
SE[[3]] <- sqrt(diag(vcovHC(model)))       
################################################
model <- lm(regulation ~ discomfort + indignity + bad.influence + injustice + male + age + ideology + free.speech + victim + college + rich, data = data)
MODEL[[4]] <- model
SE[[4]] <- sqrt(diag(vcovHC(model)))        
################################################
stargazer(MODEL[[1]], MODEL[[2]], MODEL[[3]], MODEL[[4]], se = list(SE[[1]], SE[[2]], SE[[3]], SE[[4]]), star.char = c("*", "**", "***"), star.cutoffs = c(.05, .01, .001))
################################################







################################################
## Table A2 (Appendix)
## Results of linear regression models excluding variables for ideology and free speech
################################################
MODEL <- list(NA, 4)
SE <- list(NA, 4)
################################################
model <- lm(regulation ~ discomfort + indignity + male + age + victim + college + rich, data = data.sub)
MODEL[[1]] <- model
SE[[1]] <- sqrt(diag(vcovHC(model)))        
################################################
model <- lm(bad.influence ~ discomfort + indignity + male + age + victim + college + rich, data = data.sub)
MODEL[[2]] <- model
SE[[2]] <- sqrt(diag(vcovHC(model)))        
################################################
model <- lm(injustice ~ discomfort + indignity + male + age + victim + college + rich, data = data.sub)
MODEL[[3]] <- model
SE[[3]] <- sqrt(diag(vcovHC(model)))       
################################################
model <- lm(regulation ~ discomfort + indignity + bad.influence + injustice + male + age + victim + college + rich, data = data.sub)
MODEL[[4]] <- model
SE[[4]] <- sqrt(diag(vcovHC(model)))        
################################################
stargazer(MODEL[[1]], MODEL[[2]], MODEL[[3]], MODEL[[4]], se = list(SE[[1]], SE[[2]], SE[[3]], SE[[4]]), star.char = c("*", "**", "***"), star.cutoffs = c(.05, .01, .001))
################################################






################################################
## Figure A1 (Appendix)
## Sample and population distributions of income and education
################################################
population.education <- c(0.188, 0.465, 0.148, 0.199) ## 2010 

population.income <- c(5.6 + 12.3,
                13.3 + 13.8,
                10.6 + 8.9,
                7.4 + 6.2,
                5.6 + 3.6,
                3 + 2.2,
                1.9 + 1.1,
                1.1 + 0.7,
                0.5 + 0.4,
                0.2 + 0.2,
                1.3)/100 ## 2017

sample.education <- prop.table(table(data.sub$education))
sample.income <- prop.table(table(data.sub$income))
################################################
par(mfrow = c(2, 2))

x <- barplot(sample.income, ylab = "Proportion", main = "Sample Distribution of Income", xaxt="n", ylim = c(0, 0.3))
text(x, par("usr")[3], labels = c("Less Than 2M Yen",
                                  "2M-4M Yen",
                                  "4M-6M Yen",
                                  "6M-8M Yen",
                                  "8M-10M Yen",
                                  "10M-12M Yen",
                                  "12M-14M Yen",
                                  "14M-16M Yen",
                                  "16M-18M Yen",
                                  "18M-20M Yen",
                                  "More Than 20M Yen"), srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.8)

x <- barplot(population.income, ylab = "Proportion", main = "Population Distribution of Income (2017)", xaxt="n", ylim = c(0, 0.3))
text(x, par("usr")[3], labels = c("Less Than 2M Yen",
                                  "2M-4M Yen",
                                  "4M-6M Yen",
                                  "6M-8M Yen",
                                  "8M-10M Yen",
                                  "10M-12M Yen",
                                  "12M-14M Yen",
                                  "14M-16M Yen",
                                  "16M-18M Yen",
                                  "18M-20M Yen",
                                  "More Than 20M Yen"), srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.8)


x <- barplot(sample.education, ylab = "Proportion", main = "Sample Distribution of Education", xaxt="n", ylim = c(0, 0.6))
text(x, par("usr")[3], labels = c("Middle School\n or Below", "High School", "Two-Year\n or Vocational College", "Four-Year College\n or Above"), srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)

x <- barplot(population.education, ylab = "Proportion", main = "Population Distribution of Education (2010)", xaxt="n", ylim = c(0, 0.6))
text(x, par("usr")[3], labels = c("Middle School\n or Below", "High School", "Two-Year\n or Vocational College", "Four-Year College\n or Above"), srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
################################################









################################################
## Figure A2 (Appendix)
## Associations between the harms of hate speech and support for hate speech regulations
################################################
discomfort.education.coef <- rep(NA, 2)
discomfort.education.se <- rep(NA, 2)
indignity.education.coef <- rep(NA, 2)
indignity.education.se <- rep(NA, 2)

discomfort.income.coef <- rep(NA, 2)
discomfort.income.se <- rep(NA, 2)
indignity.income.coef <- rep(NA, 2)
indignity.income.se <- rep(NA, 2)
################################################
data.sub.0 <- data.sub[data.sub$college == 0, ]

model <- lm(regulation ~ discomfort + indignity + male + age + rich + victim + ideology + free.speech, data = data.sub.0)
    
discomfort.education.coef[1] <- coef(model)[2]
discomfort.education.se[1] <- sqrt(diag(vcovHC(model)))[2]

indignity.education.coef[1] <- coef(model)[3]
indignity.education.se[1] <- sqrt(diag(vcovHC(model)))[3]
################################################
data.sub.1 <- data.sub[data.sub$college == 1, ]

model <- lm(regulation ~ discomfort + indignity + male + age + rich + victim + ideology + free.speech, data = data.sub.1)
    
discomfort.education.coef[2] <- coef(model)[2]
discomfort.education.se[2] <- sqrt(diag(vcovHC(model)))[2]

indignity.education.coef[2] <- coef(model)[3]
indignity.education.se[2] <- sqrt(diag(vcovHC(model)))[3]
################################################
data.sub.0 <- data.sub[data.sub$rich == 0, ]

model <- lm(regulation ~ discomfort + indignity + male + age + college + victim + ideology + free.speech, data = data.sub.0)
    
discomfort.income.coef[1] <- coef(model)[2]
discomfort.income.se[1] <- sqrt(diag(vcovHC(model)))[2]

indignity.income.coef[1] <- coef(model)[3]
indignity.income.se[1] <- sqrt(diag(vcovHC(model)))[3]
################################################
data.sub.1 <- data.sub[data.sub$rich == 1, ]

model <- lm(regulation ~ discomfort + indignity + male + age + college + victim + ideology + free.speech, data = data.sub.1)
    
discomfort.income.coef[2] <- coef(model)[2]
discomfort.income.se[2] <- sqrt(diag(vcovHC(model)))[2]

indignity.income.coef[2] <- coef(model)[3]
indignity.income.se[2] <- sqrt(diag(vcovHC(model)))[3]
################################################
par(mfrow = c(2, 2))

LABEL.edu <- c("Others", "Four-Year College or Above")

LABEL.income <- c("Others",
                  "Median Income or Above")

MAIN <- "IV: Harm to Feelings\nDV: Support for Hate Speech Regulation"
COEF <- discomfort.education.coef
SE <- discomfort.education.se
MIN <- min(COEF - 1.96 * SE)
MAX <- max(COEF + 1.96 * SE)
plot(COEF, 1:2, xlim = c(MIN, MAX), ylim = c(0.5, 2.5), ylab = "", yaxt = "n", xlab = "Coefficients", main = MAIN, pch = 20)
for (i in 1:2){
    lines(c(COEF[i] - 1.96 * SE[i], COEF[i] + 1.96 * SE[i]), c(i, i))
    text(COEF[i], i + 0.2, labels = LABEL.edu[i], cex = 1)
}

MAIN <- "IV: Harm to Dignity\nDV: Support for Hate Speech Regulation"
COEF <- indignity.education.coef
SE <- indignity.education.se
MIN <- min(COEF - 1.96 * SE)
MAX <- max(COEF + 1.96 * SE)
plot(COEF, 1:2, xlim = c(MIN, MAX), ylim = c(0.5, 2.5), ylab = "", yaxt = "n", xlab = "Coefficients", main = MAIN, pch = 20)
for (i in 1:2){
    lines(c(COEF[i] - 1.96 * SE[i], COEF[i] + 1.96 * SE[i]), c(i, i))
    text(COEF[i], i + 0.2, labels = LABEL.edu[i], cex = 1)
}

MAIN <- "IV: Harm to Feelings\nDV: Support for Hate Speech Regulation"
COEF <- discomfort.income.coef
SE <- discomfort.income.se
MIN <- min(COEF - 1.96 * SE)
MAX <- max(COEF + 1.96 * SE)
plot(COEF, 1:2, xlim = c(MIN, MAX), ylim = c(0.5, 2.5), ylab = "", yaxt = "n", xlab = "Coefficients", main = MAIN, pch = 20)
for (i in 1:2){
    lines(c(COEF[i] - 1.96 * SE[i], COEF[i] + 1.96 * SE[i]), c(i, i))
    text(COEF[i], i + 0.2, labels = LABEL.income[i], cex = 1)
}

MAIN <- "IV: Harm to Dignity\nDV: Support for Hate Speech Regulation"
COEF <- indignity.income.coef
SE <- indignity.income.se
MIN <- min(COEF - 1.96 * SE)
MAX <- max(COEF + 1.96 * SE)
plot(COEF, 1:2, xlim = c(MIN, MAX), ylim = c(0.5, 2.5), ylab = "", yaxt = "n", xlab = "Coefficients", main = MAIN, pch = 20)
for (i in 1:2){
    lines(c(COEF[i] - 1.96 * SE[i], COEF[i] + 1.96 * SE[i]), c(i, i))
    text(COEF[i], i + 0.2, labels = LABEL.income[i], cex = 1)
}
################################################
