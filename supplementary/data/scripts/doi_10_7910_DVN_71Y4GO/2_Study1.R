#### setting environment ####
require(cjoint)
require(estimatr)
require(lavaan)

attr.names <- c("gender", "age", "education", "occupation", 
                "dynasty", "experience", "party")
level.names <- list(c("男性", "女性"), 
                    c("34歳", "42歳", "53歳", "64歳", "70歳"), 
                    c("高校卒", "私立大学卒", "地方国立大学卒", 
                      "東京大学卒", "大学院卒", "海外の大学卒"), 
                    c("会社員", "会社役員", "弁護士", "記者", "公務員", 
                      "国会議員秘書", "タレント", "地方政治家"), 
                    c("近親者に政治家はいない", "親が元地方政治家", 
                      "親が元国会議員", "親が元大臣"), 
                    c("経験なし", "衆議院議員1期", 
                      "衆議院議員2期", "衆議院議員3期以上"), 
                    c("無所属", "自由民主党", "立憲民主党", 
                      "公明党", "日本維新の会", "日本共産党"))
attr.labels <- c("Gender", "Age", "Education", 
                 "Prior occupation", "Dynastic status", 
                 "Experience", "Party affiliation")
level.labels <- list(c("Male", "Female"), 
                     c("34", "42", "53", "64", "70"), 
                     c("High school", "Private university", 
                       "Local national university", 
                       "The University of Tokyo", 
                       "Graduate school", "Foreign university"), 
                     c("Business employee", "Business executive", 
                       "Lawyer", "Reporter", "Government employee", 
                       "Secretary", 
                       "Celebrity", "Local politician"), 
                     c("No dynastic status", "Former local politician", 
                       "Former Diet member", "Former minister"), 
                     c("No experience", "One term", 
                       "Two terms", "Three or more terms"), 
                     c("Independent", "LDP", "CDP", 
                       "Komeito", "JIP", "JCP"))
n.levels <- c(2, 5, 6, 8, 4, 4, 6)
cumsum.n.levels <- c(0, cumsum(n.levels))

# this function computes marginal means for given attributes
MM <- function(data, attribute, outcome, id) {
  lm.data <- data.frame(id = data[, id], 
                        A = data[, attribute], 
                        Y = data[, outcome])
  l <- nlevels(lm.data$A)
  result.matrix <- matrix(NA, l, 3)
  for (i in 1:l) {
    level.label <- levels(lm.data$A)[i]
    result.matrix[i, ] <- unlist(lm_robust(Y ~ 1, data = lm.data, 
                                           subset = A == level.label, 
                                           clusters = id)[c(1, 6, 7)])
  }
  result.matrix
}

# this function conducts the F-test to check the joint significance of coefficients for each attribute
attr.F.test <- function(attr.name, data, party) {
  if (party == TRUE) {
    lm.model.1 <- lm(selected ~ gender + age + education + occupation + 
                       dynasty + experience + party, data = data)
  } else {
    lm.model.1 <- lm(selected ~ gender + age + education + occupation + 
                       dynasty + experience, data = data)
  }
  if (party == TRUE) {
    lm.formula <- as.formula(paste0("selected ~ gender + age + education + occupation + dynasty + experience + party - ", attr.name))
  } else {
    lm.formula <- as.formula(paste0("selected ~ gender + age + education + occupation + dynasty + experience - ", attr.name))
  }
  lm.model.2 <- lm(lm.formula, data = data)
  anova(lm.model.1, lm.model.2)$"Pr(>F)"[2]
}

# this function conducts the F-test to check the joint significance of coefficients for interaction terms
interaction.F.test <- function(attr.name, covariate, data, party) {
  if (party == TRUE) {
    lm.formula <- as.formula(paste0("selected ~ gender + age + education + occupation + dynasty + experience + party + ", covariate))
  } else {
    lm.formula <- as.formula(paste0("selected ~ gender + age + education + occupation + dynasty + experience + ", covariate))
  }
  lm.model.1 <- lm(lm.formula, data = data)
  if (party == TRUE) {
    lm.formula <- as.formula(paste0("selected ~ gender + age + education + occupation + dynasty + experience + party + ", covariate, "+", attr.name, " : ", covariate))
  } else {
    lm.formula <- as.formula(paste0("selected ~ gender + age + education + occupation + dynasty + experience + ", covariate, "+", attr.name, " : ", covariate))
  }
  lm.model.2 <- lm(lm.formula, data = data)
  anova(lm.model.1, lm.model.2)$"Pr(>F)"[2]
}

#### compute populist attitudes score ####
respondent.data <- read.csv("data_Study1.csv")

## number of respondents
nrow(respondent.data)
## count respondents displayed in conjoint tables with and without party affiliation
table(respondent.data$condition)

## confirmatory factor analysis without the acquiescence factor
CFA.model.1 <-
  'POP =~ -1 * POP1 + POP2 + POP3
ANT =~ -1 * ANT1 + ANT2 + ANT3
MAN =~ -1 * MAN1 + MAN2 + MAN3'
CFA.result.1 <- sem(model = CFA.model.1, 
                    data = respondent.data[, c(paste0("POP", 1:3), 
                                               paste0("ANT", 1:3), 
                                               paste0("MAN", 1:3))], 
                    estimator = "MLM")
# results for the naive model presented in Table A.10
standardizedSolution(CFA.result.1, type = "std.lv")
# fit indices reported in Appendix F.2
fitMeasures(CFA.result.1, fit.measures = "cfi.robust")
fitMeasures(CFA.result.1, fit.measures = "srmr")
fitMeasures(CFA.result.1, fit.measures = "rmsea.robust")

## confirmatory factor analysis with the acquiescence factor
CFA.model.2 <-
  'ACQ =~ 1 * POP1 + 1 * POP2 + 1 * POP3 + 1 * ANT1 + 1 * ANT2 + 
1 * ANT3 + 1 * MAN1 + 1 * MAN2 + 1 * MAN3
POP =~ -1 * POP1 + POP2 + POP3
ANT =~ -1 * ANT1 + ANT2 + ANT3
MAN =~ -1 * MAN1 + MAN2 + MAN3
POP ~~ 0 * ACQ
ANT ~~ 0 * ACQ
MAN ~~ 0 * ACQ'
CFA.result.2 <- sem(model = CFA.model.2, 
                    data = respondent.data[, c(paste0("POP", 1:3), 
                                               paste0("ANT", 1:3), 
                                               paste0("MAN", 1:3))], 
                    estimator = "MLM")
# results for the AQ model presented in Table A.10
standardizedSolution(CFA.result.2, type = "std.lv")
# fit indices reported in Appendix F.2
fitMeasures(CFA.result.2, fit.measures = "cfi.robust")
fitMeasures(CFA.result.2, fit.measures = "srmr")
fitMeasures(CFA.result.2, fit.measures = "rmsea.robust")

## compute factor scores based on the AQ model
factor.scores <- lavPredict(CFA.result.2)
respondent.data$POP <- factor.scores[, "POP"]
respondent.data$ANT <- factor.scores[, "ANT"]
respondent.data$MAN <- factor.scores[, "MAN"]

# calculate the populist attitude score as the minimum of subdimension scores
respondent.data$populist.score <- apply(cbind(respondent.data$POP, 
                                              respondent.data$ANT, 
                                              respondent.data$MAN), 1, min)

# dichotomize scores for subgroup analysis
respondent.data$populist.bin <- (respondent.data$populist.score > 
                                   mean(respondent.data$populist.score)) * 1
respondent.data$POP.bin <- (respondent.data$POP > mean(respondent.data$POP)) * 1
respondent.data$ANT.bin <- (respondent.data$ANT > mean(respondent.data$ANT)) * 1

#### data for the outcome of candidates' perceived anti-elitism ####
## with party affiliation
# convert data from Qualtrics format to R readable format
AE.P.data <- read.qualtrics("data_Study1_conjoint_with_party.csv", 
                            ranks = c("Q1_AE_A_1", "Q1_AE_A_2", 
                                      "Q2_AE_A_1", "Q2_AE_A_2", 
                                      "Q3_AE_A_1", "Q3_AE_A_2", 
                                      "Q4_AE_A_1", "Q4_AE_A_2", 
                                      "Q5_AE_A_1", "Q5_AE_A_2"), 
                            covariates = "rid", new.format = TRUE)
# rename variables
colnames(AE.P.data)[seq(6, 18, 2)] <- c("education", "dynasty", "experience", 
                                        "party", "gender", "occupation", "age")
# redefine factor levels
for (i in 1:7) {
  AE.P.data[, attr.names[i]] <- factor(AE.P.data[, attr.names[i]], 
                                       level.names[[i]])
}
# merge conjoint data with populist attitudes score data
merged.AE.P.data <- merge(AE.P.data, 
                          respondent.data[, c("ANT", "populist.score", 
                                              "populist.bin", "ANT.bin", "rid")], 
                          by = "rid")

## without party affiliation
# convert data from Qualtrics format to R readable format
AE.NP.data <- read.qualtrics("data_Study1_conjoint_wo_party.csv", 
                             ranks = c("Q1_AE_B_1", "Q1_AE_B_2", 
                                       "Q2_AE_B_1", "Q2_AE_B_2", 
                                       "Q3_AE_B_1", "Q3_AE_B_2", 
                                       "Q4_AE_B_1", "Q4_AE_B_2", 
                                       "Q5_AE_B_1", "Q5_AE_B_2"), 
                             covariates ="rid", new.format = TRUE)
# rename variables
colnames(AE.NP.data)[seq(6, 16, 2)] <- c("education", "dynasty", "experience", 
                                         "gender", "occupation", "age")
# redefine factor levels
for (i in 1:6) {
  AE.NP.data[, attr.names[i]] <- factor(AE.NP.data[, attr.names[i]], 
                                        level.names[[i]])
}
# merge conjoint data with populist attitudes score data
merged.AE.NP.data <- merge(AE.NP.data, 
                           respondent.data[, c("ANT", "populist.score", 
                                               "populist.bin", "ANT.bin", "rid")], 
                           by = "rid")

#### data for the outcome of candidates' perceived people-centrism ####
## with party affiliation
# convert data from Qualtrics format to R readable format
PC.P.data <- read.qualtrics("data_Study1_conjoint_with_party.csv", 
                            ranks = c("Q1_PC_A_1", "Q1_PC_A_2", 
                                      "Q2_PC_A_1", "Q2_PC_A_2", 
                                      "Q3_PC_A_1", "Q3_PC_A_2", 
                                      "Q4_PC_A_1", "Q4_PC_A_2", 
                                      "Q5_PC_A_1", "Q5_PC_A_2"), 
                            covariates = "rid", new.format = TRUE)
# rename variables
colnames(PC.P.data)[seq(6, 18, 2)] <- c("education", "dynasty", "experience", 
                                        "party", "gender", "occupation", "age")
# redefine factor levels
for (i in 1:7) {
  PC.P.data[, attr.names[i]] <- factor(PC.P.data[, attr.names[i]], 
                                       level.names[[i]])
}
# merge conjoint data with populist attitudes score data
merged.PC.P.data <- merge(PC.P.data, 
                          respondent.data[, c("POP", "populist.score", 
                                              "populist.bin", "POP.bin", "rid")], 
                          by = "rid")

## without party affiliation
# convert data from Qualtrics format to R readable format
PC.NP.data <- read.qualtrics("data_Study1_conjoint_wo_party.csv", 
                             ranks = c("Q1_PC_B_1", "Q1_PC_B_2", 
                                       "Q2_PC_B_1", "Q2_PC_B_2", 
                                       "Q3_PC_B_1", "Q3_PC_B_2", 
                                       "Q4_PC_B_1", "Q4_PC_B_2", 
                                       "Q5_PC_B_1", "Q5_PC_B_2"), 
                             covariates = "rid", new.format = TRUE)
# rename variables
colnames(PC.NP.data)[seq(6, 16, 2)] <- c("education", "dynasty", "experience", 
                                         "gender", "occupation", "age")
# redefine factor levels
for (i in 1:6) {
  PC.NP.data[, attr.names[i]] <- factor(PC.NP.data[, attr.names[i]], 
                                        level.names[[i]])
}
# merge conjoint data with populist attitudes score data
merged.PC.NP.data <- merge(PC.NP.data, 
                           respondent.data[, c("POP", "populist.score", 
                                               "populist.bin", "POP.bin", "rid")], 
                           by = "rid")

#### marginal means (w/ party affiliation) ####
## marginal means
# perceived anti-elitism
AE.P.MM <- matrix(NA, 35, 3)
for (i in 1:7) {
  AE.P.MM[(cumsum.n.levels[i] + 1):cumsum.n.levels[i + 1], ] <- 
    MM(merged.AE.P.data, attr.names[i], "selected", "respondent")
}
# perceived people-centrism
PC.P.MM <- matrix(NA, 35, 3)
for (i in 1:7) {
  PC.P.MM[(cumsum.n.levels[i] + 1):cumsum.n.levels[i + 1], ] <- 
    MM(merged.PC.P.data, attr.names[i], "selected", "respondent")
}

## F-tests of joint significance
P.attr.F.test.result <- matrix(NA, 7, 2)
# perceived anti-elitism
for (i in 1:7) {
  P.attr.F.test.result[i, 1] <- 
    attr.F.test(attr.names[i], merged.AE.P.data, TRUE)
}
# perceived people-centrism
for (i in 1:7) {
  P.attr.F.test.result[i, 2] <- 
    attr.F.test(attr.names[i], merged.PC.P.data, TRUE)
}
# Table A.12
round(P.attr.F.test.result, 3)

## calculate standard deviations of outcomes to interpret effect sizes
round(sd(AE.P.data$selected), 2)
round(sd(PC.P.data$selected), 2)

## Figure 2
png("Figure_2.png", width = 6, height = 7.5, units = "in", pointsize = 9, res = 1200)
par(mar = c(4, 0, 2, 0), lwd = 0.5, xpd = TRUE)
plot(NULL, NULL, type = "n", bty = "n", xlim = c(-1.6, 3.6), ylim = c(1, 42), 
     xlab = "", ylab = "", xaxt = "n", yaxt = "n")
segments(seq(0, 1.6, 0.4), 0, seq(0, 1.6, 0.4), 42.5, lwd = 0.4, col = "gray")
segments(seq(2, 3.6, 0.4), 0, seq(2, 3.6, 0.4), 42.5, lwd = 0.4, col = "gray")
for (i in 1:7) {
  text(-1.6, 42 - cumsum.n.levels[i] - i + 1, 
       labels = attr.labels[i], pos = 4, font = 2)
  for (j in 1:n.levels[i]) {
    text(-0.15, 42 - cumsum.n.levels[i] - i - j + 1, 
         labels = level.labels[[i]][j], cex = 0.9, pos = 2)
    segments(-0.04, 42 - cumsum.n.levels[i] - i - j + 1, 
             1.64, 42 - cumsum.n.levels[i] - i - j + 1, lty = 3, col = "gray")
    # for perceived anti-elitism, MM = 2.4 is plotted at x = 0
    points(AE.P.MM[cumsum.n.levels[i] + j, 1] - 2.4, 
           42 - cumsum.n.levels[i] - i - j + 1, pch = 19, 
           col = ifelse(P.attr.F.test.result[i, 1] < 0.05, 
                        "black", "gray"))
    segments(AE.P.MM[cumsum.n.levels[i] + j, 2] - 2.4, 
             42 - cumsum.n.levels[i] - i - j + 1, 
             AE.P.MM[cumsum.n.levels[i] + j, 3] - 2.4, 
             42 - cumsum.n.levels[i] - i - j + 1, 
             col = ifelse(P.attr.F.test.result[i, 1] < 0.05, 
                          "black", "gray"))
  }
}
for (i in 1:7) {
  for (j in 1:n.levels[i]) {
    segments(1.96, 42 - cumsum.n.levels[i] - i - j + 1, 
             3.64, 42 - cumsum.n.levels[i] - i - j + 1, lty = 3, col = "gray")
    # for perceived people-centrism, MM = 2.4 is plotted at x = 2
    points(PC.P.MM[cumsum.n.levels[i] + j, 1] - 0.4, 
           42 - cumsum.n.levels[i] - i - j + 1, pch = 19, 
           col = ifelse(P.attr.F.test.result[i, 2] < 0.05, 
                        "black", "gray"))
    segments(PC.P.MM[cumsum.n.levels[i] + j, 2] - 0.4, 
             42 - cumsum.n.levels[i] - i - j + 1, 
             PC.P.MM[cumsum.n.levels[i] + j, 3] - 0.4, 
             42 - cumsum.n.levels[i] - i - j + 1, 
             col = ifelse(P.attr.F.test.result[i, 2] < 0.05, 
                          "black", "gray"))
  }
}
legend(-1.5, par()$usr[3], 
       legend = c("Significant at 5%", "Not significant"), 
       pch = 19, col = c("black", "gray"), 
       title = "Attribute-level F-test", inset = c(0.035, 0.01))
axis(1, at = seq(0, 1.6, 0.4), 
     labels = c("2.4", "2.8", "3.2", "3.6", "4.0"), lwd = 0.5)
axis(1, at = seq(2, 3.6, 0.4), 
     labels = c("2.4", "2.8", "3.2", "3.6", "4.0"), lwd = 0.5)
mtext("Anti-elitism", at = 0.8, cex = 1.2, font = 2)
mtext("People-centrism", at = 2.8, cex = 1.2, font = 2)
mtext("Marginal mean", side = 1, at = 0.8, line = 3)
mtext("Marginal mean", side = 1, at = 2.8, line = 3)
dev.off()

#### marginal means (w/o party affiliation) ####
## marginal means
# perceived anti-elitism
AE.NP.MM <- matrix(NA, 29, 3)
for (i in 1:6) {
  AE.NP.MM[(cumsum.n.levels[i] + 1):cumsum.n.levels[i + 1], ] <- 
    MM(merged.AE.NP.data, attr.names[i], "selected", "respondent")
}
# perceived people-centrism
PC.NP.MM <- matrix(NA, 29, 3)
for (i in 1:6) {
  PC.NP.MM[(cumsum.n.levels[i] + 1):cumsum.n.levels[i + 1], ] <- 
    MM(merged.PC.NP.data, attr.names[i], "selected", "respondent")
}

## F-tests of joint significance
NP.attr.F.test.result <- matrix(NA, 6, 2)
# perceived anti-elitism
for (i in 1:6) {
  NP.attr.F.test.result[i, 1] <- 
    attr.F.test(attr.names[i], merged.AE.NP.data, FALSE)
}
# perceived people-centrism
for (i in 1:6) {
  NP.attr.F.test.result[i, 2] <- 
    attr.F.test(attr.names[i], merged.PC.NP.data, FALSE)
}
# Table A.12
round(NP.attr.F.test.result, 3)

## Figure A.4
cairo_pdf("Figure_A4.pdf", width = 6, height = 6.3, pointsize = 9)
par(mar = c(4, 0, 2, 0), lwd = 0.5, xpd = TRUE)
plot(NULL, NULL, type = "n", bty = "n", xlim = c(-1.6, 3.6), ylim = c(1, 35), 
     xlab = "", ylab = "", xaxt = "n", yaxt = "n")
segments(seq(0, 1.6, 0.4), 0, seq(0, 1.6, 0.4), 35.5, lwd = 0.4, col = "gray")
segments(seq(2, 3.6, 0.4), 0, seq(2, 3.6, 0.4), 35.5, lwd = 0.4, col = "gray")
for (i in 1:6) {
  text(-1.6, 35 - cumsum.n.levels[i] - i + 1, 
       labels = attr.labels[i], pos = 4, font = 2)
  for (j in 1:n.levels[i]) {
    text(-0.15, 35 - cumsum.n.levels[i] - i - j + 1, 
         labels = level.labels[[i]][j], cex = 0.9, pos = 2)
    segments(-0.04, 35 - cumsum.n.levels[i] - i - j + 1, 
             1.64, 35 - cumsum.n.levels[i] - i - j + 1, lty = 3, col = "gray")
    # for perceived anti-elitism, MM = 2.4 is plotted at x = 0
    points(AE.NP.MM[cumsum.n.levels[i] + j, 1] - 2.4, 
           35 - cumsum.n.levels[i] - i - j + 1, pch = 19, 
           col = ifelse(NP.attr.F.test.result[i, 1] < 0.05, 
                        "black", "gray"))
    segments(AE.NP.MM[cumsum.n.levels[i] + j, 2] - 2.4, 
             35 - cumsum.n.levels[i] - i - j + 1, 
             AE.NP.MM[cumsum.n.levels[i] + j, 3] - 2.4, 
             35 - cumsum.n.levels[i] - i - j + 1, 
             col = ifelse(NP.attr.F.test.result[i, 1] < 0.05, 
                          "black", "gray"))
  }
}
for (i in 1:6) {
  for (j in 1:n.levels[i]) {
    segments(1.96, 35 - cumsum.n.levels[i] - i - j + 1, 
             3.64, 35 - cumsum.n.levels[i] - i - j + 1, lty = 3, col = "gray")
    # for perceived people-centrism, MM = 2.4 is plotted at x = 2
    points(PC.NP.MM[cumsum.n.levels[i] + j, 1] - 0.4, 
           35 - cumsum.n.levels[i] - i - j + 1, pch = 19, 
           col = ifelse(NP.attr.F.test.result[i, 2] < 0.05, 
                        "black", "gray"))
    segments(PC.NP.MM[cumsum.n.levels[i] + j, 2] - 0.4, 
             35 - cumsum.n.levels[i] - i - j + 1, 
             PC.NP.MM[cumsum.n.levels[i] + j, 3] - 0.4, 
             35 - cumsum.n.levels[i] - i - j + 1, 
             col = ifelse(NP.attr.F.test.result[i, 2] < 0.05, 
                          "black", "gray"))
  }
}
legend(-1.5, par()$usr[3], 
       legend = c("Significant at 5%", "Not significant"), 
       pch = 19, col = c("black", "gray"), 
       title = "Attribute-level F-test", inset = c(0.035, 0.01))
axis(1, at = seq(0, 1.6, 0.4), 
     labels = c("2.4", "2.8", "3.2", "3.6", "4.0"), lwd = 0.5)
axis(1, at = seq(2, 3.6, 0.4), 
     labels = c("2.4", "2.8", "3.2", "3.6", "4.0"), lwd = 0.5)
mtext("Anti-elitism", at = 0.8, cex = 1.2, font = 2)
mtext("People-centrism", at = 2.8, cex = 1.2, font = 2)
mtext("Marginal mean", side = 1, at = 0.8, line = 3)
mtext("Marginal mean", side = 1, at = 2.8, line = 3)
dev.off()

#### populist score interaction (w/ party affiliation) ####
## marginal means
# perceived anti-elitism
AE.P.score.low.MM <- AE.P.score.high.MM <- matrix(NA, 35, 3)
for (i in 1:7) {
  AE.P.score.low.MM[(cumsum.n.levels[i] + 1):cumsum.n.levels[i + 1], ] <- 
    MM(subset(merged.AE.P.data, populist.bin == 0), 
       attr.names[i], "selected", "respondent")
  AE.P.score.high.MM[(cumsum.n.levels[i] + 1):cumsum.n.levels[i + 1], ] <- 
    MM(subset(merged.AE.P.data, populist.bin == 1), 
       attr.names[i], "selected", "respondent")
}
# perceived people-centrism
PC.P.score.low.MM <- PC.P.score.high.MM <- matrix(NA, 35, 3)
for (i in 1:7) {
  PC.P.score.low.MM[(cumsum.n.levels[i] + 1):cumsum.n.levels[i + 1], ] <- 
    MM(subset(merged.PC.P.data, populist.bin == 0), 
       attr.names[i], "selected", "respondent")
  PC.P.score.high.MM[(cumsum.n.levels[i] + 1):cumsum.n.levels[i + 1], ] <- 
    MM(subset(merged.PC.P.data, populist.bin == 1), 
       attr.names[i], "selected", "respondent")
}

## F-tests of joint significance for interaction
P.int.score.F.test.result <- matrix(NA, 7, 2)
# perceived anti-elitism
for (i in 1:7) {
  P.int.score.F.test.result[i, 1] <- 
    interaction.F.test(attr.names[i], "populist.score", 
                       merged.AE.P.data, TRUE)
}
# perceived people-centrism
for (i in 1:7) {
  P.int.score.F.test.result[i, 2] <- 
    interaction.F.test(attr.names[i], "populist.score", 
                       merged.PC.P.data, TRUE)
}

## Figure A.5
cairo_pdf("Figure_A5.pdf", width = 6, height = 7.5, pointsize = 9)
par(mar = c(4, 0, 2, 0), lwd = 0.5, xpd = TRUE)
plot(NULL, NULL, type = "n", bty = "n", xlim = c(-1.6, 3.6), ylim = c(1, 42), 
     xlab = "", ylab = "", xaxt = "n", yaxt = "n")
segments(seq(0, 1.6, 0.4), 0, seq(0, 1.6, 0.4), 42.5, lwd = 0.4, col = "gray")
segments(seq(2, 3.6, 0.4), 0, seq(2, 3.6, 0.4), 42.5, lwd = 0.4, col = "gray")
for (i in 1:7) {
  text(-1.6, 42 - cumsum.n.levels[i] - i + 1, 
       labels = attr.labels[i], pos = 4, font = 2)
  for (j in 1:n.levels[i]) {
    text(-0.15, 42 - cumsum.n.levels[i] - i - j + 1, 
         labels = level.labels[[i]][j], cex = 0.9, pos = 2)
    segments(-0.04, 42 - cumsum.n.levels[i] - i - j + 1, 
             1.64, 42 - cumsum.n.levels[i] - i - j + 1, lty = 3, col = "gray")
    # for perceived anti-elitism, MM = 2.4 is plotted at x = 0
    points(AE.P.score.low.MM[cumsum.n.levels[i] + j, 1] - 2.4, 
           42 - cumsum.n.levels[i] - i - j + 1.15, pch = 19, 
           col = ifelse(P.int.score.F.test.result[i, 1] < 0.05, 
                        "black", "gray50"))
    segments(AE.P.score.low.MM[cumsum.n.levels[i] + j, 2] - 2.4, 
             42 - cumsum.n.levels[i] - i - j + 1.15, 
             AE.P.score.low.MM[cumsum.n.levels[i] + j, 3] - 2.4, 
             42 - cumsum.n.levels[i] - i - j + 1.15, 
             col = ifelse(P.int.score.F.test.result[i, 1] < 0.05, 
                          "black", "gray50"))
    segments(AE.P.score.high.MM[cumsum.n.levels[i] + j, 2] - 2.4, 
             42 - cumsum.n.levels[i] - i - j + 0.85, 
             AE.P.score.high.MM[cumsum.n.levels[i] + j, 3] - 2.4, 
             42 - cumsum.n.levels[i] - i - j + 0.85, 
             col = ifelse(P.int.score.F.test.result[i, 1] < 0.05, 
                          "black", "gray50"))
    points(AE.P.score.high.MM[cumsum.n.levels[i] + j, 1] - 2.4, 
           42 - cumsum.n.levels[i] - i - j + 0.85, pch = 21, 
           col = ifelse(P.int.score.F.test.result[i, 1] < 0.05, 
                        "black", "gray50"), 
           bg = "white")
  }
}
for (i in 1:7) {
  for (j in 1:n.levels[i]) {
    segments(1.96, 42 - cumsum.n.levels[i] - i - j + 1, 
             3.64, 42 - cumsum.n.levels[i] - i - j + 1, lty = 3, col = "gray")
    # for perceived people-centrism, MM = 2.4 is plotted at x = 2
    points(PC.P.score.low.MM[cumsum.n.levels[i] + j, 1] - 0.4, 
           42 - cumsum.n.levels[i] - i - j + 1.15, pch = 19, 
           col = ifelse(P.int.score.F.test.result[i, 2] < 0.05, 
                        "black", "gray50"))
    segments(PC.P.score.low.MM[cumsum.n.levels[i] + j, 2] - 0.4, 
             42 - cumsum.n.levels[i] - i - j + 1.15, 
             PC.P.score.low.MM[cumsum.n.levels[i] + j, 3] - 0.4, 
             42 - cumsum.n.levels[i] - i - j + 1.15, 
             col = ifelse(P.int.score.F.test.result[i, 2] < 0.05, 
                          "black", "gray50"))
    segments(PC.P.score.high.MM[cumsum.n.levels[i] + j, 2] - 0.4, 
             42 - cumsum.n.levels[i] - i - j + 0.85, 
             PC.P.score.high.MM[cumsum.n.levels[i] + j, 3] - 0.4, 
             42 - cumsum.n.levels[i] - i - j + 0.85, 
             col = ifelse(P.int.score.F.test.result[i, 2] < 0.05, 
                          "black", "gray50"))
    points(PC.P.score.high.MM[cumsum.n.levels[i] + j, 1] - 0.4, 
           42 - cumsum.n.levels[i] - i - j + 0.85, pch = 21, 
           col = ifelse(P.int.score.F.test.result[i, 2] < 0.05, 
                        "black", "gray50"), 
           bg = "white")
  }
}
legend(-0.5, 0.1, legend = c("Low", "High"), 
       pch = c(19, 21), bty = "n", pt.bg = c(NA, "white"), 
       cex = 0.9, xjust = 1, ncol = 2, 
       title = "Populist attitudes")
legend(-0.35, -2.2, legend = c(expression(p<0.05), "n.s."), 
       pch = 15, bty = "n", col = c("black", "gray50"), 
       cex = 0.9, xjust = 1, ncol = 2, 
       title = "Interaction F-test")
polygon(c(-1.5, -1.5, -0.48, -0.48), c(-4.6, 0.2, 0.2, -4.6))
axis(1, at = seq(0, 1.6, 0.4), 
     labels = c("2.4", "2.8", "3.2", "3.6", "4.0"), lwd = 0.5)
axis(1, at = seq(2, 3.6, 0.4), 
     labels = c("2.4", "2.8", "3.2", "3.6", "4.0"), lwd = 0.5)
mtext("Anti-elitism", at = 0.8, cex = 1.2, font = 2)
mtext("People-centrism", at = 2.8, cex = 1.2, font = 2)
mtext("Marginal mean", side = 1, at = 0.8, line = 3)
mtext("Marginal mean", side = 1, at = 2.8, line = 3)
dev.off()

#### populist score interaction (w/o party affiliation) ####
## marginal means
# perceived anti-elitism
AE.NP.score.low.MM <- AE.NP.score.high.MM <- matrix(NA, 29, 3)
for (i in 1:6) {
  AE.NP.score.low.MM[(cumsum.n.levels[i] + 1):cumsum.n.levels[i + 1], ] <- 
    MM(subset(merged.AE.NP.data, populist.bin == 0), 
       attr.names[i], "selected", "respondent")
  AE.NP.score.high.MM[(cumsum.n.levels[i] + 1):cumsum.n.levels[i + 1], ] <- 
    MM(subset(merged.AE.NP.data, populist.bin == 1), 
       attr.names[i], "selected", "respondent")
}
# perceived people-centrism
PC.NP.score.low.MM <- PC.NP.score.high.MM <- matrix(NA, 29, 3)
for (i in 1:6) {
  PC.NP.score.low.MM[(cumsum.n.levels[i] + 1):cumsum.n.levels[i + 1], ] <- 
    MM(subset(merged.PC.NP.data, populist.bin == 0), 
       attr.names[i], "selected", "respondent")
  PC.NP.score.high.MM[(cumsum.n.levels[i] + 1):cumsum.n.levels[i + 1], ] <- 
    MM(subset(merged.PC.NP.data, populist.bin == 1), 
       attr.names[i], "selected", "respondent")
}

## F-tests of joint significance for interaction
NP.int.score.F.test.result <- matrix(NA, 6, 2)
# perceived anti-elitism
for (i in 1:6) {
  NP.int.score.F.test.result[i, 1] <- 
    interaction.F.test(attr.names[i], "populist.score", 
                       merged.AE.NP.data, FALSE)
}
# perceived people-centrism
for (i in 1:6) {
  NP.int.score.F.test.result[i, 2] <- 
    interaction.F.test(attr.names[i], "populist.score", 
                       merged.PC.NP.data, FALSE)
}

## Figure A.6
cairo_pdf("Figure_A6.pdf", width = 6, height = 6.3, pointsize = 9)
par(mar = c(4, 0, 2, 0), lwd = 0.5, xpd = TRUE)
plot(NULL, NULL, type = "n", bty = "n", xlim = c(-1.6, 3.6), ylim = c(1, 35), 
     xlab = "", ylab = "", xaxt = "n", yaxt = "n")
segments(seq(0, 1.6, 0.4), 0, seq(0, 1.6, 0.4), 35.5, lwd = 0.4, col = "gray")
segments(seq(2, 3.6, 0.4), 0, seq(2, 3.6, 0.4), 35.5, lwd = 0.4, col = "gray")
for (i in 1:6) {
  text(-1.6, 35 - cumsum.n.levels[i] - i + 1, 
       labels = attr.labels[i], pos = 4, font = 2)
  for (j in 1:n.levels[i]) {
    text(-0.15, 35 - cumsum.n.levels[i] - i - j + 1, 
         labels = level.labels[[i]][j], cex = 0.9, pos = 2)
    segments(-0.04, 35 - cumsum.n.levels[i] - i - j + 1, 
             1.64, 35 - cumsum.n.levels[i] - i - j + 1, lty = 3, col = "gray")
    # for perceived anti-elitism, MM = 2.4 is plotted at x = 0
    points(AE.NP.score.low.MM[cumsum.n.levels[i] + j, 1] - 2.4, 
           35 - cumsum.n.levels[i] - i - j + 1.15, pch = 19, 
           col = ifelse(NP.int.score.F.test.result[i, 1] < 0.05, 
                        "black", "gray50"))
    segments(AE.NP.score.low.MM[cumsum.n.levels[i] + j, 2] - 2.4, 
             35 - cumsum.n.levels[i] - i - j + 1.15, 
             AE.NP.score.low.MM[cumsum.n.levels[i] + j, 3] - 2.4, 
             35 - cumsum.n.levels[i] - i - j + 1.15, 
             col = ifelse(NP.int.score.F.test.result[i, 1] < 0.05, 
                          "black", "gray50"))
    segments(AE.NP.score.high.MM[cumsum.n.levels[i] + j, 2] - 2.4, 
             35 - cumsum.n.levels[i] - i - j + 0.85, 
             AE.NP.score.high.MM[cumsum.n.levels[i] + j, 3] - 2.4, 
             35 - cumsum.n.levels[i] - i - j + 0.85, 
             col = ifelse(NP.int.score.F.test.result[i, 1] < 0.05, 
                          "black", "gray50"))
    points(AE.NP.score.high.MM[cumsum.n.levels[i] + j, 1] - 2.4, 
           35 - cumsum.n.levels[i] - i - j + 0.85, pch = 21, 
           col = ifelse(NP.int.score.F.test.result[i, 1] < 0.05, 
                        "black", "gray50"), 
           bg = "white")
  }
}
for (i in 1:6) {
  for (j in 1:n.levels[i]) {
    segments(1.96, 35 - cumsum.n.levels[i] - i - j + 1, 
             3.64, 35 - cumsum.n.levels[i] - i - j + 1, lty = 3, col = "gray")
    # for perceived people-centrism, MM = 2.4 is plotted at x = 2
    points(PC.NP.score.low.MM[cumsum.n.levels[i] + j, 1] - 0.4, 
           35 - cumsum.n.levels[i] - i - j + 1.15, pch = 19, 
           col = ifelse(NP.int.score.F.test.result[i, 2] < 0.05, 
                        "black", "gray50"))
    segments(PC.NP.score.low.MM[cumsum.n.levels[i] + j, 2] - 0.4, 
             35 - cumsum.n.levels[i] - i - j + 1.15, 
             PC.NP.score.low.MM[cumsum.n.levels[i] + j, 3] - 0.4, 
             35 - cumsum.n.levels[i] - i - j + 1.15, 
             col = ifelse(NP.int.score.F.test.result[i, 2] < 0.05, 
                          "black", "gray50"))
    segments(PC.NP.score.high.MM[cumsum.n.levels[i] + j, 2] - 0.4, 
             35 - cumsum.n.levels[i] - i - j + 0.85, 
             PC.NP.score.high.MM[cumsum.n.levels[i] + j, 3] - 0.4, 
             35 - cumsum.n.levels[i] - i - j + 0.85, 
             col = ifelse(NP.int.score.F.test.result[i, 2] < 0.05, 
                          "black", "gray50"))
    points(PC.NP.score.high.MM[cumsum.n.levels[i] + j, 1] - 0.4, 
           35 - cumsum.n.levels[i] - i - j + 0.85, pch = 21, 
           col = ifelse(NP.int.score.F.test.result[i, 2] < 0.05, 
                        "black", "gray50"), 
           bg = "white")
  }
}
legend(-0.5, 0.1, legend = c("Low", "High"), 
       pch = c(19, 21), bty = "n", pt.bg = c(NA, "white"), 
       cex = 0.9, xjust = 1, ncol = 2, 
       title = "Populist attitudes")
legend(-0.35, -2.2, legend = c(expression(p<0.05), "n.s."), 
       pch = 15, bty = "n", col = c("black", "gray50"), 
       cex = 0.9, xjust = 1, ncol = 2, 
       title = "Interaction F-test")
polygon(c(-1.5, -1.5, -0.48, -0.48), c(-4.4, 0.1, 0.1, -4.4))
axis(1, at = seq(0, 1.6, 0.4), 
     labels = c("2.4", "2.8", "3.2", "3.6", "4.0"), lwd = 0.5)
axis(1, at = seq(2, 3.6, 0.4), 
     labels = c("2.4", "2.8", "3.2", "3.6", "4.0"), lwd = 0.5)
mtext("Anti-elitism", at = 0.8, cex = 1.2, font = 2)
mtext("People-centrism", at = 2.8, cex = 1.2, font = 2)
mtext("Marginal mean", side = 1, at = 0.8, line = 3)
mtext("Marginal mean", side = 1, at = 2.8, line = 3)
dev.off()

#### specific dimension's score interaction (w/ party affiliation) ####
## marginal means
# perceived anti-elitism
AE.P.ANT.low.MM <- AE.P.ANT.high.MM <- matrix(NA, 35, 3)
for (i in 1:7) {
  AE.P.ANT.low.MM[(cumsum.n.levels[i] + 1):cumsum.n.levels[i + 1], ] <- 
    MM(subset(merged.AE.P.data, ANT.bin == 0), 
       attr.names[i], "selected", "respondent")
  AE.P.ANT.high.MM[(cumsum.n.levels[i] + 1):cumsum.n.levels[i + 1], ] <- 
    MM(subset(merged.AE.P.data, ANT.bin == 1), 
       attr.names[i], "selected", "respondent")
}
# perceived people-centrism
PC.P.POP.low.MM <- PC.P.POP.high.MM <- matrix(NA, 35, 3)
for (i in 1:7) {
  PC.P.POP.low.MM[(cumsum.n.levels[i] + 1):cumsum.n.levels[i + 1], ] <- 
    MM(subset(merged.PC.P.data, POP.bin == 0), 
       attr.names[i], "selected", "respondent")
  PC.P.POP.high.MM[(cumsum.n.levels[i] + 1):cumsum.n.levels[i + 1], ] <- 
    MM(subset(merged.PC.P.data, POP.bin == 1), 
       attr.names[i], "selected", "respondent")
}

## F-tests of joint significance for interaction
P.int.specific.F.test.result <- matrix(NA, 7, 2)
# perceived anti-elitism
for (i in 1:7) {
  P.int.specific.F.test.result[i, 1] <- 
    interaction.F.test(attr.names[i], "ANT", 
                       merged.AE.P.data, TRUE)
}
# perceived people-centrism
for (i in 1:7) {
  P.int.specific.F.test.result[i, 2] <- 
    interaction.F.test(attr.names[i], "POP", 
                       merged.PC.P.data, TRUE)
}

## Figure A.7
cairo_pdf("Figure_A7.pdf", width = 6, height = 7.5, pointsize = 9)
par(mar = c(4, 0, 2, 0), lwd = 0.5, xpd = TRUE)
plot(NULL, NULL, type = "n", bty = "n", xlim = c(-1.6, 3.6), ylim = c(1, 42), 
     xlab = "", ylab = "", xaxt = "n", yaxt = "n")
segments(seq(0, 1.6, 0.4), 0, seq(0, 1.6, 0.4), 42.5, lwd = 0.4, col = "gray")
segments(seq(2, 3.6, 0.4), 0, seq(2, 3.6, 0.4), 42.5, lwd = 0.4, col = "gray")
for (i in 1:7) {
  text(-1.6, 42 - cumsum.n.levels[i] - i + 1, 
       labels = attr.labels[i], pos = 4, font = 2)
  for (j in 1:n.levels[i]) {
    text(-0.15, 42 - cumsum.n.levels[i] - i - j + 1, 
         labels = level.labels[[i]][j], cex = 0.9, pos = 2)
    segments(-0.04, 42 - cumsum.n.levels[i] - i - j + 1, 
             1.64, 42 - cumsum.n.levels[i] - i - j + 1, lty = 3, col = "gray")
    # for perceived anti-elitism, MM = 2.4 is plotted at x = 0
    points(AE.P.ANT.low.MM[cumsum.n.levels[i] + j, 1] - 2.4, 
           42 - cumsum.n.levels[i] - i - j + 1.15, pch = 19, 
           col = ifelse(P.int.specific.F.test.result[i, 1] < 0.05, 
                        "black", "gray50"))
    segments(AE.P.ANT.low.MM[cumsum.n.levels[i] + j, 2] - 2.4, 
             42 - cumsum.n.levels[i] - i - j + 1.15, 
             AE.P.ANT.low.MM[cumsum.n.levels[i] + j, 3] - 2.4, 
             42 - cumsum.n.levels[i] - i - j + 1.15, 
             col = ifelse(P.int.specific.F.test.result[i, 1] < 0.05, 
                          "black", "gray50"))
    segments(AE.P.ANT.high.MM[cumsum.n.levels[i] + j, 2] - 2.4, 
             42 - cumsum.n.levels[i] - i - j + 0.85, 
             AE.P.ANT.high.MM[cumsum.n.levels[i] + j, 3] - 2.4, 
             42 - cumsum.n.levels[i] - i - j + 0.85, 
             col = ifelse(P.int.specific.F.test.result[i, 1] < 0.05, 
                          "black", "gray50"))
    points(AE.P.ANT.high.MM[cumsum.n.levels[i] + j, 1] - 2.4, 
           42 - cumsum.n.levels[i] - i - j + 0.85, pch = 21, 
           col = ifelse(P.int.specific.F.test.result[i, 1] < 0.05, 
                        "black", "gray50"), 
           bg = "white")
  }
}
for (i in 1:7) {
  for (j in 1:n.levels[i]) {
    segments(1.96, 42 - cumsum.n.levels[i] - i - j + 1, 
             3.64, 42 - cumsum.n.levels[i] - i - j + 1, lty = 3, col = "gray")
    # for perceived people-centrism, MM = 2.4 is plotted at x = 2
    points(PC.P.POP.low.MM[cumsum.n.levels[i] + j, 1] - 0.4, 
           42 - cumsum.n.levels[i] - i - j + 1.15, pch = 19, 
           col = ifelse(P.int.specific.F.test.result[i, 2] < 0.05, 
                        "black", "gray50"))
    segments(PC.P.POP.low.MM[cumsum.n.levels[i] + j, 2] - 0.4, 
             42 - cumsum.n.levels[i] - i - j + 1.15, 
             PC.P.POP.low.MM[cumsum.n.levels[i] + j, 3] - 0.4, 
             42 - cumsum.n.levels[i] - i - j + 1.15, 
             col = ifelse(P.int.specific.F.test.result[i, 2] < 0.05, 
                          "black", "gray50"))
    segments(PC.P.POP.high.MM[cumsum.n.levels[i] + j, 2] - 0.4, 
             42 - cumsum.n.levels[i] - i - j + 0.85, 
             PC.P.POP.high.MM[cumsum.n.levels[i] + j, 3] - 0.4, 
             42 - cumsum.n.levels[i] - i - j + 0.85, 
             col = ifelse(P.int.specific.F.test.result[i, 2] < 0.05, 
                          "black", "gray50"))
    points(PC.P.POP.high.MM[cumsum.n.levels[i] + j, 1] - 0.4, 
           42 - cumsum.n.levels[i] - i - j + 0.85, pch = 21, 
           col = ifelse(P.int.specific.F.test.result[i, 2] < 0.05, 
                        "black", "gray50"), 
           bg = "white")
  }
}
legend(-0.5, 0.1, legend = c("Low", "High"), 
       pch = c(19, 21), bty = "n", pt.bg = c(NA, "white"), 
       cex = 0.9, xjust = 1, ncol = 2, 
       title = "Populist attitudes")
legend(-0.35, -2.2, legend = c(expression(p<0.05), "n.s."), 
       pch = 15, bty = "n", col = c("black", "gray50"), 
       cex = 0.9, xjust = 1, ncol = 2, 
       title = "Interaction F-test")
polygon(c(-1.5, -1.5, -0.48, -0.48), c(-4.6, 0.2, 0.2, -4.6))
axis(1, at = seq(0, 1.6, 0.4), 
     labels = c("2.4", "2.8", "3.2", "3.6", "4.0"), lwd = 0.5)
axis(1, at = seq(2, 3.6, 0.4), 
     labels = c("2.4", "2.8", "3.2", "3.6", "4.0"), lwd = 0.5)
mtext("Anti-elitism", at = 0.8, cex = 1.2, font = 2)
mtext("People-centrism", at = 2.8, cex = 1.2, font = 2)
mtext("Marginal mean", side = 1, at = 0.8, line = 3)
mtext("Marginal mean", side = 1, at = 2.8, line = 3)
dev.off()

#### specific dimension's score interaction (w/o party affiliation) ####
## marginal means
# perceived anti-elitism
AE.NP.ANT.low.MM <- AE.NP.ANT.high.MM <- matrix(NA, 29, 3)
for (i in 1:6) {
  AE.NP.ANT.low.MM[(cumsum.n.levels[i] + 1):cumsum.n.levels[i + 1], ] <- 
    MM(subset(merged.AE.NP.data, ANT.bin == 0), 
       attr.names[i], "selected", "respondent")
  AE.NP.ANT.high.MM[(cumsum.n.levels[i] + 1):cumsum.n.levels[i + 1], ] <- 
    MM(subset(merged.AE.NP.data, ANT.bin == 1), 
       attr.names[i], "selected", "respondent")
}
# perceived people-centrism
PC.NP.POP.low.MM <- PC.NP.POP.high.MM <- matrix(NA, 29, 3)
for (i in 1:6) {
  PC.NP.POP.low.MM[(cumsum.n.levels[i] + 1):cumsum.n.levels[i + 1], ] <- 
    MM(subset(merged.PC.NP.data, POP.bin == 0), 
       attr.names[i], "selected", "respondent")
  PC.NP.POP.high.MM[(cumsum.n.levels[i] + 1):cumsum.n.levels[i + 1], ] <- 
    MM(subset(merged.PC.NP.data, POP.bin == 1), 
       attr.names[i], "selected", "respondent")
}

## F-tests of joint significance for interaction
NP.int.specific.F.test.result <- matrix(NA, 6, 2)
# perceived anti-elitism
for (i in 1:6) {
  NP.int.specific.F.test.result[i, 1] <- 
    interaction.F.test(attr.names[i], "ANT", 
                       merged.AE.NP.data, FALSE)
}
# perceived people-centrism
for (i in 1:6) {
  NP.int.specific.F.test.result[i, 2] <- 
    interaction.F.test(attr.names[i], "POP", 
                       merged.PC.NP.data, FALSE)
}

## Figure A.8
cairo_pdf("Figure_A8.pdf", width = 6, height = 6.3, pointsize = 9)
par(mar = c(4, 0, 2, 0), lwd = 0.5, xpd = TRUE)
plot(NULL, NULL, type = "n", bty = "n", xlim = c(-1.6, 3.6), ylim = c(1, 35), 
     xlab = "", ylab = "", xaxt = "n", yaxt = "n")
segments(seq(0, 1.6, 0.4), 0, seq(0, 1.6, 0.4), 35.5, lwd = 0.4, col = "gray")
segments(seq(2, 3.6, 0.4), 0, seq(2, 3.6, 0.4), 35.5, lwd = 0.4, col = "gray")
for (i in 1:6) {
  text(-1.6, 35 - cumsum.n.levels[i] - i + 1, 
       labels = attr.labels[i], pos = 4, font = 2)
  for (j in 1:n.levels[i]) {
    text(-0.15, 35 - cumsum.n.levels[i] - i - j + 1, 
         labels = level.labels[[i]][j], cex = 0.9, pos = 2)
    segments(-0.04, 35 - cumsum.n.levels[i] - i - j + 1, 
             1.64, 35 - cumsum.n.levels[i] - i - j + 1, lty = 3, col = "gray")
    # for perceived anti-elitism, MM = 2.4 is plotted at x = 0
    points(AE.NP.ANT.low.MM[cumsum.n.levels[i] + j, 1] - 2.4, 
           35 - cumsum.n.levels[i] - i - j + 1.15, pch = 19, 
           col = ifelse(NP.int.score.F.test.result[i, 1] < 0.05, 
                        "black", "gray50"))
    segments(AE.NP.ANT.low.MM[cumsum.n.levels[i] + j, 2] - 2.4, 
             35 - cumsum.n.levels[i] - i - j + 1.15, 
             AE.NP.ANT.low.MM[cumsum.n.levels[i] + j, 3] - 2.4, 
             35 - cumsum.n.levels[i] - i - j + 1.15, 
             col = ifelse(NP.int.score.F.test.result[i, 1] < 0.05, 
                          "black", "gray50"))
    segments(AE.NP.ANT.high.MM[cumsum.n.levels[i] + j, 2] - 2.4, 
             35 - cumsum.n.levels[i] - i - j + 0.85, 
             AE.NP.ANT.high.MM[cumsum.n.levels[i] + j, 3] - 2.4, 
             35 - cumsum.n.levels[i] - i - j + 0.85, 
             col = ifelse(NP.int.score.F.test.result[i, 1] < 0.05, 
                          "black", "gray50"))
    points(AE.NP.ANT.high.MM[cumsum.n.levels[i] + j, 1] - 2.4, 
           35 - cumsum.n.levels[i] - i - j + 0.85, pch = 21, 
           col = ifelse(NP.int.score.F.test.result[i, 1] < 0.05, 
                        "black", "gray50"), 
           bg = "white")
  }
}
for (i in 1:6) {
  for (j in 1:n.levels[i]) {
    segments(1.96, 35 - cumsum.n.levels[i] - i - j + 1, 
             3.64, 35 - cumsum.n.levels[i] - i - j + 1, lty = 3, col = "gray")
    # for perceived people-centrism, MM = 2.4 is plotted at x = 2
    points(PC.NP.POP.low.MM[cumsum.n.levels[i] + j, 1] - 0.4, 
           35 - cumsum.n.levels[i] - i - j + 1.15, pch = 19, 
           col = ifelse(NP.int.score.F.test.result[i, 2] < 0.05, 
                        "black", "gray50"))
    segments(PC.NP.POP.low.MM[cumsum.n.levels[i] + j, 2] - 0.4, 
             35 - cumsum.n.levels[i] - i - j + 1.15, 
             PC.NP.POP.low.MM[cumsum.n.levels[i] + j, 3] - 0.4, 
             35 - cumsum.n.levels[i] - i - j + 1.15, 
             col = ifelse(NP.int.score.F.test.result[i, 2] < 0.05, 
                          "black", "gray50"))
    segments(PC.NP.POP.high.MM[cumsum.n.levels[i] + j, 2] - 0.4, 
             35 - cumsum.n.levels[i] - i - j + 0.85, 
             PC.NP.POP.high.MM[cumsum.n.levels[i] + j, 3] - 0.4, 
             35 - cumsum.n.levels[i] - i - j + 0.85, 
             col = ifelse(NP.int.score.F.test.result[i, 2] < 0.05, 
                          "black", "gray50"))
    points(PC.NP.POP.high.MM[cumsum.n.levels[i] + j, 1] - 0.4, 
           35 - cumsum.n.levels[i] - i - j + 0.85, pch = 21, 
           col = ifelse(NP.int.score.F.test.result[i, 2] < 0.05, 
                        "black", "gray50"), 
           bg = "white")
  }
}
legend(-0.5, 0.1, legend = c("Low", "High"), 
       pch = c(19, 21), bty = "n", pt.bg = c(NA, "white"), 
       cex = 0.9, xjust = 1, ncol = 2, 
       title = "Populist attitudes")
legend(-0.35, -2.2, legend = c(expression(p<0.05), "n.s."), 
       pch = 15, bty = "n", col = c("black", "gray50"), 
       cex = 0.9, xjust = 1, ncol = 2, 
       title = "Interaction F-test")
polygon(c(-1.5, -1.5, -0.48, -0.48), c(-4.4, 0.1, 0.1, -4.4))
axis(1, at = seq(0, 1.6, 0.4), 
     labels = c("2.4", "2.8", "3.2", "3.6", "4.0"), lwd = 0.5)
axis(1, at = seq(2, 3.6, 0.4), 
     labels = c("2.4", "2.8", "3.2", "3.6", "4.0"), lwd = 0.5)
mtext("Anti-elitism", at = 0.8, cex = 1.2, font = 2)
mtext("People-centrism", at = 2.8, cex = 1.2, font = 2)
mtext("Marginal mean", side = 1, at = 0.8, line = 3)
mtext("Marginal mean", side = 1, at = 2.8, line = 3)
dev.off()