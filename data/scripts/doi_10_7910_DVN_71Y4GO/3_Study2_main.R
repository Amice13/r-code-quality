#### setting environment ####
require(cjoint)
require(estimatr)
require(lavaan)

attr.names <- c("gender", "age", "education", "occupation",
                "dynasty", "experience", "party")
favorable.levels <- list("女性", "34歳", "東京大学卒", "地方政治家", 
                         "近親者に政治家はいない", 
                         "衆議院議員2期", "自由民主党")

# this function estimates average marginal component effects (AMCEs)
# int.pos is the position of the interaction term
AMCE.est <- function(result, int.pos) {
  est <- result$coefficients[1] + result$coefficients[int.pos]
  se <- sqrt(result$vcov[1, 1] + result$vcov[int.pos, int.pos] + 
               2 * result$vcov[1, int.pos])
  lower <- est + se * qt(0.025, result$df.residual)
  upper <- est + se * qt(0.975, result$df.residual)
  out <- c(est, lower, upper)
  names(out) <- c("est", "lower", "upper")
  out
}

# this function visualizes the results
EE.plot <- function(result.1, result.2, int.pos, main, sub, xlim, line) {
  AMCE.estimate.1 <- AMCE.est(result.1, int.pos)
  AMCE.estimate.2 <- AMCE.est(result.2, int.pos)
  plot(NULL, NULL, type = "n", bty = "n", xlim = xlim, ylim = c(0.5, 3.5), 
       xlab = "", ylab = "", xaxt = "n", yaxt = "n")
  abline(v = 0, col = "gray")
  abline(v = line[line != 0], lty = 3, col = "gray")
  # AMCEs
  segments(AMCE.estimate.1[2], 3.1, AMCE.estimate.1[3], 3.1)
  points(AMCE.estimate.1[1], 3.1, pch = 19)
  segments(AMCE.estimate.2[2], 2.9, AMCE.estimate.2[3], 2.9)
  points(AMCE.estimate.2[1], 2.9, pch = 21, bg = "white")
  # average controlled direct effects (ACDEs)
  segments(result.1$conf.low[1], 2.1, result.1$conf.high[1], 2.1)
  points(result.1$coefficients[1], 2.1, pch = 19)
  segments(result.2$conf.low[1], 1.9, result.2$conf.high[1], 1.9)
  # eliminated effects (EEs)
  points(result.2$coefficients[1], 1.9, pch = 21, bg = "white")
  segments(result.1$conf.low[int.pos], 1.1, result.1$conf.high[int.pos], 1.1)
  points(result.1$coefficients[int.pos], 1.1, pch = 19)
  segments(result.2$conf.low[int.pos], 0.9, result.2$conf.high[int.pos], 0.9)
  points(result.2$coefficients[int.pos], 0.9, pch = 21, bg = "white")
  axis(1, lwd = 0.5, at = line)
  mtext("AMCE", side = 2, at = 3, las = 1, cex = 0.8)
  mtext("ACDE", side = 2, at = 2, las = 1, cex = 0.8)
  mtext("EE", side = 2, at = 1, las = 1, cex = 0.8)
  if (is.na(sub)) {
    mtext(main, line = 1, cex = 1, font = 2)
  } else {
    mtext(main, line = 2, cex = 1, font = 2)
    mtext(sub, line = 1, cex = 0.8)
  }
}

#### compute populist attitudes score ####
respondent.data <- read.csv("data_Study2_main.csv")

## number of respondents
nrow(respondent.data)

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
# results for the naive model presented in Table A.11
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
# results for the AQ model presented in Table A.11
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

#### data ####
## data of the manipulated mediator arm (MMA)
MMA.data <- read.qualtrics("data_Study2_main_conjoint.csv", 
                           ranks = c("Q1_M_1", "Q1_M_2", "Q2_M_1", "Q2_M_2"), 
                           covariates = c("rid", "LR", "block"), new.format = TRUE)
# combine variables for the "special notes" attribute
MMA.data$notes <- unlist(respondent.data[, paste0("note.", 1:4)])

## data of the natural mediator arm (NMA)
NMA.data <- read.qualtrics("data_Study2_main_conjoint.csv", 
                           ranks = c("Q1_N_1", "Q1_N_2", "Q2_N_1", "Q2_N_2"), 
                           covariates = c("rid", "LR", "block"), new.format = TRUE)
# variables for the "special notes" attribute are missing for the NMA
NMA.data$notes <- "なし"

## combine the MMA and NMA data
Study2.data <- subset(rbind(MMA.data, NMA.data), ! is.na(selected))

# rename variables
colnames(Study2.data)[seq(8, 20, 2)] <- c("education", "dynasty", "experience", 
                                          "party", "gender", "occupation", "age")
# recode attribute-levels as dummy variables for levels expected to be preferred 
for (i in 1:7) {
  Study2.data[, attr.names[i]] <- (Study2.data[, attr.names[i]] == 
                                     favorable.levels[i]) * 1
}
# recode the "special notes" attribute
Study2.data$notes <- factor(Study2.data$notes, 
                            levels = c("なし", 
                                       "中立的な行政監視団体に所属し，政治家や公務員の汚職を告発する活動をしていた", 
                                       "先祖代々続く資産家の生まれで，土地や株式を多数保有しており，選挙に出る前から政界に顔が利く人物であった", 
                                       "普通の人々の目線を大切にするという信念をもち，街頭や商業施設に繰り出して人々の意見を聞いている", 
                                       "近年の政治は一般有権者の感情的な意見に流されすぎだと考え，専門家と政治家を中心とした政策形成を訴えている"), 
                            labels = c("NMA", "AE+", "AE-", "PC+", "PC-"))

# dummy variable for the analysis of high-populist notes
Study2.data$high.data <- (Study2.data$notes %in% c("NMA", "AE+", "PC+"))
# dummy variable for the analysis of high-anti-elitist notes
Study2.data$high.AE.data <- (Study2.data$notes %in% c("NMA", "AE+"))
# dummy variable for the analysis of high-people-centrist notes
Study2.data$high.PC.data <- (Study2.data$notes %in% c("NMA", "PC+"))
# dummy variable for the analysis of low-populist notes
Study2.data$low.data <- (Study2.data$notes %in% c("NMA", "AE-", "PC-"))
# dummy variable for the analysis of low-anti-elitist notes
Study2.data$low.AE.data <- (Study2.data$notes %in% c("NMA", "AE-"))
# dummy variable for the analysis of low-people-centrist notes
Study2.data$low.PC.data <- (Study2.data$notes %in% c("NMA", "PC-"))
# dummy variable for the NMA
Study2.data$NMA <- (Study2.data$notes == "NMA") * 1

# dummy variable for liberal respondents
Study2.data$liberal <- (Study2.data$LR < 3) * 1
# dummy variable for conservative respondents
Study2.data$conservative <- (Study2.data$LR > 3) * 1

# merge conjoint data with populist attitudes score data
merged.Study2.data <- merge(Study2.data, 
                            respondent.data[, c("ANT", "POP", "populist.score", 
                                                "ANT.bin", "POP.bin", 
                                                "populist.bin", "rid")], 
                            by = "rid")

#### main results ####
## estimates for gender using high-populist notes
gender.high.main.result <- lm_robust(selected ~ gender * NMA + 
                                       age + education + occupation + 
                                       dynasty + experience + party, 
                                     data = Study2.data, subset = high.data, 
                                     fixed_effects = block, 
                                     cluster = respondent)
round(summary(gender.high.main.result)$coefficients, 2)

## estimates for gender using low-populist notes
gender.low.main.result <- lm_robust(selected ~ gender * NMA + 
                                      age + education + occupation + 
                                      dynasty + experience + party, 
                                    data = Study2.data, subset = low.data, 
                                    fixed_effects = block, 
                                    cluster = respondent)
round(summary(gender.low.main.result)$coefficients, 2)

## estimates for age using high-populist notes
age.high.main.result <- lm_robust(selected ~ age * NMA + 
                                    gender + education + occupation + 
                                    dynasty + experience + party, 
                                  data = Study2.data, subset = high.data, 
                                  fixed_effects = block, 
                                  cluster = respondent)
round(summary(age.high.main.result)$coefficients, 2)

## estimates for age using low-populist notes
age.low.main.result <- lm_robust(selected ~ age * NMA + 
                                   gender + education + occupation + 
                                   dynasty + experience + party, 
                                 data = Study2.data, subset = low.data, 
                                 fixed_effects = block, 
                                 cluster = respondent)
round(summary(age.low.main.result)$coefficients, 2)

# AMCE of age
round(AMCE.est(age.low.main.result, 9), 2)
# ACDE of age
round(age.low.main.result$coefficients["age"], 2)
# EE of age
round(age.low.main.result$coefficients["age:NMA"], 2)
# elimination ratio
round(age.low.main.result$coefficients["age:NMA"] / 
        AMCE.est(age.low.main.result, 9)[1], 2)

## estimates for education using high-populist notes
education.high.main.result <- lm_robust(selected ~ education * NMA + 
                                          gender + age + occupation + 
                                          dynasty + experience + party, 
                                        data = Study2.data, subset = high.data, 
                                        fixed_effects = block, 
                                        cluster = respondent)
round(summary(education.high.main.result)$coefficients, 2)

## estimates for education using low-populist notes
education.low.main.result <- lm_robust(selected ~ education * NMA + 
                                         gender + age + occupation + 
                                         dynasty + experience + party, 
                                       data = Study2.data, subset = low.data, 
                                       fixed_effects = block, 
                                       cluster = respondent)
round(summary(education.low.main.result)$coefficients, 2)

# AMCE of education
round(AMCE.est(education.low.main.result, 9), 2)
# ACDE of education
round(education.low.main.result$coefficients["education"], 2)
# EE of education
round(education.low.main.result$coefficients["education:NMA"], 2)
# elimination ratio
round(education.low.main.result$coefficients["education:NMA"] / 
        AMCE.est(education.low.main.result, 9)[1], 2)

## estimates for occupation using high-populist notes
occupation.high.main.result <- lm_robust(selected ~ occupation * NMA + 
                                           gender + age + education + 
                                           dynasty + experience + party, 
                                         data = Study2.data, subset = high.data, 
                                         fixed_effects = block, 
                                         cluster = respondent)
round(summary(occupation.high.main.result)$coefficients, 2)

## estimates for occupation using low-populist notes
occupation.low.main.result <- lm_robust(selected ~ occupation * NMA + 
                                          gender + age + education + 
                                          dynasty + experience + party, 
                                        data = Study2.data, subset = low.data, 
                                        fixed_effects = block, 
                                        cluster = respondent)
round(summary(occupation.low.main.result)$coefficients, 2)

# AMCE of occupation
round(AMCE.est(occupation.low.main.result, 9), 2)
# ACDE of occupation
round(occupation.low.main.result$coefficients["occupation"], 2)
# EE of occupation
round(occupation.low.main.result$coefficients["occupation:NMA"], 2)
# elimination ratio
round(occupation.low.main.result$coefficients["occupation:NMA"] / 
        AMCE.est(occupation.low.main.result, 9)[1], 2)

## estimates for dynasty using high-populist notes
dynasty.high.main.result <- lm_robust(selected ~ dynasty * NMA + 
                                        gender + age + education + 
                                        occupation + experience + party, 
                                      data = Study2.data, subset = high.data, 
                                      fixed_effects = block, 
                                      cluster = respondent)
round(summary(dynasty.high.main.result)$coefficients, 2)

## estimates for dynasty using low-populist notes
dynasty.low.main.result <- lm_robust(selected ~ dynasty * NMA + 
                                       gender + age + education + 
                                       occupation + experience + party, 
                                     data = Study2.data, subset = low.data, 
                                     fixed_effects = block, 
                                     cluster = respondent)
round(summary(dynasty.low.main.result)$coefficients, 2)

## estimates for experience using high-populist notes
experience.high.main.result <- lm_robust(selected ~ experience * NMA + 
                                           gender + age + education + 
                                           occupation + dynasty + party, 
                                         data = Study2.data, subset = high.data, 
                                         fixed_effects = block, 
                                         cluster = respondent)
round(summary(experience.high.main.result)$coefficients, 2)

## estimates for experience using low-populist notes
experience.low.main.result <- lm_robust(selected ~ experience * NMA + 
                                          gender + age + education + 
                                          occupation + dynasty + party, 
                                        data = Study2.data, subset = low.data, 
                                        fixed_effects = block, 
                                        cluster = respondent)
round(summary(experience.low.main.result)$coefficients, 2)

## estimates for party using high-populist notes
party.high.main.result <- lm_robust(selected ~ party * NMA + 
                                      gender + age + education + 
                                      occupation + dynasty + experience, 
                                    data = Study2.data, subset = high.data, 
                                    fixed_effects = block, 
                                    cluster = respondent)
round(summary(party.high.main.result)$coefficients, 2)

## estimates for party using low-populist notes
party.low.main.result <- lm_robust(selected ~ party * NMA + 
                                     gender + age + education + 
                                     occupation + dynasty + experience, 
                                   data = Study2.data, subset = low.data, 
                                   fixed_effects = block, 
                                   cluster = respondent)
round(summary(party.low.main.result)$coefficients, 2)

## Figure 3
png("Figure_3.png", width = 6, height = 3, units = "in", pointsize = 8, res = 1200)
layout(matrix(1:8, 2, 4, byrow = TRUE))
par(mar = c(2, 4, 4, 0.5), lwd = 0.5)
EE.plot(gender.high.main.result, gender.low.main.result, 9, 
        main = "Gender", sub = "female (v. male)", 
        xlim = c(-0.2, 0.6), line = seq(-0.2, 0.6, 0.2))
EE.plot(age.high.main.result, age.low.main.result, 9, 
        main = "Age", sub = "34 (v. 70)", 
        xlim = c(-0.2, 0.6), line = seq(-0.2, 0.6, 0.2))
EE.plot(education.high.main.result, education.low.main.result, 9, 
        main = "Education", sub = "UTokyo (v. high school)", 
        xlim = c(-0.2, 0.6), line = seq(-0.2, 0.6, 0.2))
EE.plot(occupation.high.main.result, occupation.low.main.result, 9, 
        main = "Prior occupation", sub = "local politician (v. celebrity)", 
        xlim = c(-0.2, 0.6), line = seq(-0.2, 0.6, 0.2))
EE.plot(dynasty.high.main.result, dynasty.low.main.result, 9, 
        main = "Dynastic status", sub = "non-dynastic (v. minister's child)", 
        xlim = c(-0.2, 0.6), line = seq(-0.2, 0.6, 0.2))
EE.plot(experience.high.main.result, experience.low.main.result, 9, 
        main = "Experience", sub = "two terms (v. no experience)", 
        xlim = c(-0.2, 0.6), line = seq(-0.2, 0.6, 0.2))
EE.plot(party.high.main.result, party.low.main.result, 9, 
        main = "Party affiliation", sub = "LDP (v. CDP)", 
        xlim = c(-0.2, 0.6), line = seq(-0.2, 0.6, 0.2))
# legend
par(mar = c(0, 4, 4, 0.5), lwd = 0.5)
plot(NULL, NULL, type = "n", bty = "n", xlim = c(-0.2, 0.6), ylim = c(0.5, 3.5), 
     xlab = "", ylab = "", xaxt = "n", yaxt = "n")
segments(0, 3.7, 0, 1.3, col = "gray")
segments(c(-0.2, 0.2, 0.4, 0.6), 3.7, 
         c(-0.2, 0.2, 0.4, 0.6), 1.3, lty = 3, col = "gray")
points(0.3, 3.2, pch = 4)
points(0.1, 2.6, pch = 4)
points(0.2, 1.6, pch = 4)
segments(0.3, 3.15, 0.3, 2.4, lwd = 0.25)
segments(0.1, 2.55, 0.1, 2.4, lwd = 0.25)
arrows(0.1, 2.4, 0.3, 2.4, length = 0.02, code = 3, lwd = 0.25)
segments(0.1, 2.4, 0, 1.6, lty = 3, lwd = 0.25)
segments(0.3, 2.4, 0.2, 1.6, lty = 3, lwd = 0.25)
arrows(0, 1.6, 0.2, 1.6, length = 0.02, code = 3, lwd = 0.25)
text(0.3, 3.2, "Effect for the NMA\n(w/o special notes)", pos = 3, cex = 0.6)
text(0.1, 2.6, "Effect for the MMA\n(w/ special notes)", pos = 3, cex = 0.6)
text(0.25, 1.9, "EE is the difference\nb/w AMCE and ACDE", pos = 4, cex = 0.6)
mtext("AMCE", side = 2, at = 3.2, las = 1, cex = 0.8)
mtext("ACDE", side = 2, at = 2.6, las = 1, cex = 0.8)
mtext("EE", side = 2, at = 1.6, las = 1, cex = 0.8)
mtext("Notes and legends", line = 1, cex = 1, font = 2)
legend("bottom", bty = "n", 
       legend = c("High-populist notes", "Low-populist notes"), 
       pch = c(19, 21), pt.bg = c(NA, "white"), cex = 1.2)
dev.off()

#### populist score interaction ####
## estimates for gender using high-populist notes
gender.high.int.result <- lm_robust(selected ~ gender * NMA * populist.score + 
                                      age + education + occupation + 
                                      dynasty + experience + party, 
                                    data = merged.Study2.data, subset = high.data, 
                                    fixed_effects = block, 
                                    cluster = respondent)
round(summary(gender.high.int.result)$coefficients, 2)

## estimates for gender using low-populist notes
gender.low.int.result <- lm_robust(selected ~ gender * NMA * populist.score + 
                                     age + education + occupation + 
                                     dynasty + experience + party, 
                                   data = merged.Study2.data, subset = low.data, 
                                   fixed_effects = block, 
                                   cluster = respondent)
round(summary(gender.low.int.result)$coefficients, 2)

## estimates for age using high-populist notes
age.high.int.result <- lm_robust(selected ~ age * NMA * populist.score + 
                                   gender + education + occupation + 
                                   dynasty + experience + party, 
                                 data = merged.Study2.data, subset = high.data, 
                                 fixed_effects = block, 
                                 cluster = respondent)
round(summary(age.high.int.result)$coefficients, 2)

## estimates for age using low-populist notes
age.low.int.result <- lm_robust(selected ~ age * NMA * populist.score + 
                                  gender + education + occupation + 
                                  dynasty + experience + party, 
                                data = merged.Study2.data, subset = low.data, 
                                fixed_effects = block, 
                                cluster = respondent)
round(summary(age.low.int.result)$coefficients, 2)

## estimates for education using high-populist notes
education.high.int.result <- lm_robust(selected ~ education * NMA * populist.score + 
                                         gender + age + occupation + 
                                         dynasty + experience + party, 
                                       data = merged.Study2.data, subset = high.data, 
                                       fixed_effects = block, 
                                       cluster = respondent)
round(summary(education.high.int.result)$coefficients, 2)

## estimates for education using low-populist notes
education.low.int.result <- lm_robust(selected ~ education * NMA * populist.score + 
                                        gender + age + occupation + 
                                        dynasty + experience + party, 
                                      data = merged.Study2.data, subset = low.data, 
                                      fixed_effects = block, 
                                      cluster = respondent)
round(summary(education.low.int.result)$coefficients, 2)

## estimates for occupation using high-populist notes
occupation.high.int.result <- lm_robust(selected ~ occupation * NMA * populist.score + 
                                          gender + age + education + 
                                          dynasty + experience + party, 
                                        data = merged.Study2.data, subset = high.data, 
                                        fixed_effects = block, 
                                        cluster = respondent)
round(summary(occupation.high.int.result)$coefficients, 2)

## estimates for occupation using low-populist notes
occupation.low.int.result <- lm_robust(selected ~ occupation * NMA * populist.score + 
                                         gender + age + education + 
                                         dynasty + experience + party, 
                                       data = merged.Study2.data, subset = low.data, 
                                       fixed_effects = block, 
                                       cluster = respondent)
round(summary(occupation.low.int.result)$coefficients, 2)

## estimates for dynasty using high-populist notes
dynasty.high.int.result <- lm_robust(selected ~ dynasty * NMA * populist.score + 
                                       gender + age + education + 
                                       occupation + experience + party, 
                                     data = merged.Study2.data, subset = high.data, 
                                     fixed_effects = block, 
                                     cluster = respondent)
round(summary(dynasty.high.int.result)$coefficients, 2)

## estimates for dynasty using low-populist notes
dynasty.low.int.result <- lm_robust(selected ~ dynasty * NMA * populist.score + 
                                      gender + age + education + 
                                      occupation + experience + party, 
                                    data = merged.Study2.data, subset = low.data, 
                                    fixed_effects = block, 
                                    cluster = respondent)
round(summary(dynasty.low.int.result)$coefficients, 2)

## estimates for experience using high-populist notes
experience.high.int.result <- lm_robust(selected ~ experience * NMA * populist.score + 
                                          gender + age + education + 
                                          occupation + dynasty + party, 
                                        data = merged.Study2.data, subset = high.data, 
                                        fixed_effects = block, 
                                        cluster = respondent)
round(summary(experience.high.int.result)$coefficients, 2)

## estimates for experience using low-populist notes
experience.low.int.result <- lm_robust(selected ~ experience * NMA * populist.score + 
                                         gender + age + education + 
                                         occupation + dynasty + party, 
                                       data = merged.Study2.data, subset = low.data, 
                                       fixed_effects = block, 
                                       cluster = respondent)
round(summary(experience.low.int.result)$coefficients, 2)

## estimates for party using high-populist notes
party.high.int.result <- lm_robust(selected ~ party * NMA * populist.score + 
                                     gender + age + education + 
                                     occupation + dynasty + experience, 
                                   data = merged.Study2.data, subset = high.data, 
                                   fixed_effects = block, 
                                   cluster = respondent)
round(summary(party.high.int.result)$coefficients, 2)

## estimates for party using low-populist notes
party.low.int.result <- lm_robust(selected ~ party * NMA * populist.score + 
                                    gender + age + education + 
                                    occupation + dynasty + experience, 
                                  data = merged.Study2.data, subset = low.data, 
                                  fixed_effects = block, 
                                  cluster = respondent)
round(summary(party.low.int.result)$coefficients, 2)

## Table A.14
# high-populist notes
round(cbind(summary(gender.high.int.result)$coefficients[c(1:3, 10:13), 1:2], 
            summary(age.high.int.result)$coefficients[c(1:3, 10:13), 1:2], 
            summary(education.high.int.result)$coefficients[c(1:3, 10:13), 1:2], 
            summary(occupation.high.int.result)$coefficients[c(1:3, 10:13), 1:2], 
            summary(dynasty.high.int.result)$coefficients[c(1:3, 10:13), 1:2], 
            summary(experience.high.int.result)$coefficients[c(1:3, 10:13), 1:2], 
            summary(party.high.int.result)$coefficients[c(1:3, 10:13), 1:2]), 2)
# low-populist notes
round(cbind(summary(gender.low.int.result)$coefficients[c(1:3, 10:13), 1:2], 
            summary(age.low.int.result)$coefficients[c(1:3, 10:13), 1:2], 
            summary(education.low.int.result)$coefficients[c(1:3, 10:13), 1:2], 
            summary(occupation.low.int.result)$coefficients[c(1:3, 10:13), 1:2], 
            summary(dynasty.low.int.result)$coefficients[c(1:3, 10:13), 1:2], 
            summary(experience.low.int.result)$coefficients[c(1:3, 10:13), 1:2], 
            summary(party.low.int.result)$coefficients[c(1:3, 10:13), 1:2]), 2)

#### AMCEs of special notes ####
notes.result <- lm_robust(selected ~ notes + gender + 
                            age + education + occupation + 
                            dynasty + experience + party, 
                          data = Study2.data, 
                          fixed_effects = block, 
                          cluster = respondent)
# Table A.13, column (1)
round(summary(notes.result)$coefficients[, c(1, 2, 4)], 3)

#### AMCEs of party affiliation (LDP) conditioned by respondents' ideology ####
party.LR.result <- lm_robust(selected ~ notes + gender + 
                               age + education + occupation + 
                               dynasty + experience + party + 
                               party : (liberal + conservative), 
                             data = Study2.data, 
                             fixed_effects = block, 
                             cluster = respondent)
# Table A.13, column (2)
round(summary(party.LR.result)$coefficients[, c(1, 2, 4)], 3)

## AMCE of party affiliation (LDP) for liberal respondents
# point estimate
round(sum(party.LR.result$coefficients[c("party", "party:liberal")]), 3)
# standard error
round(sqrt(party.LR.result$vcov["party", "party"] + 
             party.LR.result$vcov["party:liberal", "party:liberal"] + 
             2 * party.LR.result$vcov["party", "party:liberal"]), 3)

## AMCE of party affiliation (LDP) for conservative respondents
# point estimate
round(sum(party.LR.result$coefficients[c("party", "party:conservative")]), 3)
# standard error
round(sqrt(party.LR.result$vcov["party", "party"] + 
             party.LR.result$vcov["party:conservative", "party:conservative"] + 
             2 * party.LR.result$vcov["party", "party:conservative"]), 3)

#### analysis of age separated by candidates' party affiliation ####
## estimates for LDP candidates' age using high-populist notes
age.high.LDP.result <- lm_robust(selected ~ age * NMA + 
                                   gender + education + occupation + 
                                   dynasty + experience, 
                                 data = Study2.data, 
                                 subset = high.data & party == 1, 
                                 fixed_effects = block, 
                                 cluster = respondent)
round(summary(age.high.LDP.result)$coefficients, 2)

## estimates for LDP candidates' age using low-populist notes
age.low.LDP.result <- lm_robust(selected ~ age * NMA + 
                                  gender + education + occupation + 
                                  dynasty + experience, 
                                data = Study2.data, 
                                subset = low.data & party == 1, 
                                fixed_effects = block, 
                                cluster = respondent)
round(summary(age.low.LDP.result)$coefficients, 2)

## estimates for CDP candidates' age using high-populist notes
age.high.CDP.result <- lm_robust(selected ~ age * NMA + 
                                   gender + education + occupation + 
                                   dynasty + experience, 
                                 data = Study2.data, 
                                 subset = high.data & party == 0, 
                                 fixed_effects = block, 
                                 cluster = respondent)
round(summary(age.high.CDP.result)$coefficients, 2)

## estimates for CDP candidates' age using low-populist notes
age.low.CDP.result <- lm_robust(selected ~ age * NMA + 
                                  gender + education + occupation + 
                                  dynasty + experience, 
                                data = Study2.data, 
                                subset = low.data & party == 0, 
                                fixed_effects = block, 
                                cluster = respondent)
round(summary(age.low.CDP.result)$coefficients, 2)

## Figure A.9
cairo_pdf("Figure_A9.pdf", width = 5, height = 2, pointsize = 9)
layout(matrix(c(1, 1, 2, 2, 3), 1, 5, byrow = TRUE))
par(mar = c(2, 4, 3, 0.5), lwd = 0.5)
EE.plot(age.high.LDP.result, age.low.LDP.result, 8, 
        main = "LDP candidates", sub = NA, 
        xlim = c(-0.2, 0.6), line = seq(-0.2, 0.6, 0.2))
EE.plot(age.high.CDP.result, age.low.CDP.result, 8, 
        main = "CDP candidates", sub = NA, 
        xlim = c(-0.2, 0.6), line = seq(-0.2, 0.6, 0.2))
par(mar = c(0, 0, 0, 0), lwd = 0.5)
# legend
plot(NULL, NULL, type = "n", bty = "n", xlim = c(0, 1), ylim = c(0, 1), 
     xlab = "", ylab = "", xaxt = "n", yaxt = "n")
legend("center", bty = "n", 
       legend = c("High-populist\nnotes", "", "Low-populist\nnotes"), 
       pch = c(19, NA, 21), pt.bg = c(NA, NA, "white"), cex = 1.2)
dev.off()

## three-way interaction of age, the manipulation arm, and party affiliation
# high-populist notes
age.high.party.int.result <- lm_robust(selected ~ age * NMA * party + 
                                         gender + education + occupation + 
                                         dynasty + experience, 
                                       data = Study2.data, subset = high.data, 
                                       fixed_effects = block, 
                                       cluster = respondent)
round(summary(age.high.party.int.result)$coefficients, 2)
# low-populist notes
age.low.party.int.result <- lm_robust(selected ~ age * NMA * party + 
                                        gender + education + occupation + 
                                        dynasty + experience, 
                                      data = Study2.data, subset = low.data, 
                                      fixed_effects = block, 
                                      cluster = respondent)
round(summary(age.low.party.int.result)$coefficients, 2)

#### analysis using special notes about anti-elitism ####
## estimates for gender using high-populist notes
gender.high.AE.result <- lm_robust(selected ~ gender * NMA + 
                                     age + education + occupation + 
                                     dynasty + experience + party, 
                                   data = Study2.data, subset = high.AE.data, 
                                   fixed_effects = block, 
                                   cluster = respondent)
round(summary(gender.high.AE.result)$coefficients, 2)

## estimates for gender using low-populist notes
gender.low.AE.result <- lm_robust(selected ~ gender * NMA + 
                                    age + education + occupation + 
                                    dynasty + experience + party, 
                                  data = Study2.data, subset = low.AE.data, 
                                  fixed_effects = block, 
                                  cluster = respondent)
round(summary(gender.low.AE.result)$coefficients, 2)

## estimates for  using high-populist notes
age.high.AE.result <- lm_robust(selected ~ age * NMA + 
                                  gender + education + occupation + 
                                  dynasty + experience + party, 
                                data = Study2.data, subset = high.AE.data, 
                                fixed_effects = block, 
                                cluster = respondent)
round(summary(age.high.AE.result)$coefficients, 2)

## estimates for  using low-populist notes
age.low.AE.result <- lm_robust(selected ~ age * NMA + 
                                 gender + education + occupation + 
                                 dynasty + experience + party, 
                               data = Study2.data, subset = low.AE.data, 
                               fixed_effects = block, 
                               cluster = respondent)
round(summary(age.low.AE.result)$coefficients, 2)

## estimates for  using high-populist notes
education.high.AE.result <- lm_robust(selected ~ education * NMA + 
                                        gender + age + occupation + 
                                        dynasty + experience + party, 
                                      data = Study2.data, subset = high.AE.data, 
                                      fixed_effects = block, 
                                      cluster = respondent)
round(summary(education.high.AE.result)$coefficients, 2)

## estimates for party using low-populist notes
education.low.AE.result <- lm_robust(selected ~ education * NMA + 
                                       gender + age + occupation + 
                                       dynasty + experience + party, 
                                     data = Study2.data, subset = low.AE.data, 
                                     fixed_effects = block, 
                                     cluster = respondent)
round(summary(education.low.AE.result)$coefficients, 2)

## estimates for  using high-populist notes
occupation.high.AE.result <- lm_robust(selected ~ occupation * NMA + 
                                         gender + age + education + 
                                         dynasty + experience + party, 
                                       data = Study2.data, subset = high.AE.data, 
                                       fixed_effects = block, 
                                       cluster = respondent)
round(summary(occupation.high.AE.result)$coefficients, 2)

## estimates for party using low-populist notes
occupation.low.AE.result <- lm_robust(selected ~ occupation * NMA + 
                                        gender + age + education + 
                                        dynasty + experience + party, 
                                      data = Study2.data, subset = low.AE.data, 
                                      fixed_effects = block, 
                                      cluster = respondent)
round(summary(occupation.low.AE.result)$coefficients, 2)

## estimates for  using high-populist notes
dynasty.high.AE.result <- lm_robust(selected ~ dynasty * NMA + 
                                      gender + age + education + 
                                      occupation + experience + party, 
                                    data = Study2.data, subset = high.AE.data, 
                                    fixed_effects = block, 
                                    cluster = respondent)
round(summary(dynasty.high.AE.result)$coefficients, 2)

## estimates for party using low-populist notes
dynasty.low.AE.result <- lm_robust(selected ~ dynasty * NMA + 
                                     gender + age + education + 
                                     occupation + experience + party, 
                                   data = Study2.data, subset = low.AE.data, 
                                   fixed_effects = block, 
                                   cluster = respondent)
round(summary(dynasty.low.AE.result)$coefficients, 2)

## estimates for  using high-populist notes
experience.high.AE.result <- lm_robust(selected ~ experience * NMA + 
                                         gender + age + education + 
                                         occupation + dynasty + party, 
                                       data = Study2.data, subset = high.AE.data, 
                                       fixed_effects = block, 
                                       cluster = respondent)
round(summary(experience.high.AE.result)$coefficients, 2)

## estimates for party using low-populist notes
experience.low.AE.result <- lm_robust(selected ~ experience * NMA + 
                                        gender + age + education + 
                                        occupation + dynasty + party, 
                                      data = Study2.data, subset = low.AE.data, 
                                      fixed_effects = block, 
                                      cluster = respondent)
round(summary(experience.low.AE.result)$coefficients, 2)

## estimates for  using high-populist notes
party.high.AE.result <- lm_robust(selected ~ party * NMA + 
                                    gender + age + education + 
                                    occupation + dynasty + experience, 
                                  data = Study2.data, subset = high.AE.data, 
                                  fixed_effects = block, 
                                  cluster = respondent)
round(summary(party.high.AE.result)$coefficients, 2)

## estimates for party using low-populist notes
party.low.AE.result <- lm_robust(selected ~ party * NMA + 
                                   gender + age + education + 
                                   occupation + dynasty + experience, 
                                 data = Study2.data, subset = low.AE.data, 
                                 fixed_effects = block, 
                                 cluster = respondent)
round(summary(party.low.AE.result)$coefficients, 2)

## Figure A.10
cairo_pdf("Figure_A10.pdf", width = 6, height = 3, pointsize = 8)
layout(matrix(1:8, 2, 4, byrow = TRUE))
par(mar = c(2, 4, 4, 0.5), lwd = 0.5)
EE.plot(gender.high.AE.result, gender.low.AE.result, 9, 
        main = "Gender", sub = "(female v. male)", 
        xlim = c(-0.2, 0.6), line = seq(-0.2, 0.6, 0.2))
EE.plot(age.high.AE.result, age.low.AE.result, 9, 
        main = "Age", sub = "(34 v. 70)", 
        xlim = c(-0.2, 0.6), line = seq(-0.2, 0.6, 0.2))
EE.plot(education.high.AE.result, education.low.AE.result, 9, 
        main = "Education", sub = "(UTokyo v. high school)", 
        xlim = c(-0.2, 0.6), line = seq(-0.2, 0.6, 0.2))
EE.plot(occupation.high.AE.result, occupation.low.AE.result, 9, 
        main = "Prior occupation", sub = "(local politician v. celebrity)", 
        xlim = c(-0.2, 0.6), line = seq(-0.2, 0.6, 0.2))
EE.plot(dynasty.high.AE.result, dynasty.low.AE.result, 9, 
        main = "Dynastic status", sub = "(non-dynastic v. minister's child)", 
        xlim = c(-0.2, 0.6), line = seq(-0.2, 0.6, 0.2))
EE.plot(experience.high.AE.result, experience.low.AE.result, 9, 
        main = "Experience", sub = "(two terms v. no experience)", 
        xlim = c(-0.2, 0.6), line = seq(-0.2, 0.6, 0.2))
EE.plot(party.high.AE.result, party.low.AE.result, 9, 
        main = "Party affiliation", sub = "(LDP v. CDP)", 
        xlim = c(-0.2, 0.6), line = seq(-0.2, 0.6, 0.2))
plot(NULL, NULL, type = "n", bty = "n", xlim = c(0, 1), ylim = c(0, 1), 
     xlab = "", ylab = "", xaxt = "n", yaxt = "n")
legend("center", bty = "n", 
       legend = c("High-populist notes", "Low-populist notes"), 
       pch = c(19, 21), pt.bg = c(NA, "white"), cex = 1.2)
dev.off()

#### analysis using special notes about people-centrism ####
## estimates for  using high-populist notes
gender.high.PC.result <- lm_robust(selected ~ gender * NMA + 
                                     age + education + occupation + 
                                     dynasty + experience + party, 
                                   data = Study2.data, subset = high.PC.data, 
                                   fixed_effects = block, 
                                   cluster = respondent)
round(summary(gender.high.PC.result)$coefficients, 2)

## estimates for party using low-populist notes
gender.low.PC.result <- lm_robust(selected ~ gender * NMA + 
                                    age + education + occupation + 
                                    dynasty + experience + party, 
                                  data = Study2.data, subset = low.PC.data, 
                                  fixed_effects = block, 
                                  cluster = respondent)
round(summary(gender.low.PC.result)$coefficients, 2)

## estimates for  using high-populist notes
age.high.PC.result <- lm_robust(selected ~ age * NMA + 
                                  gender + education + occupation + 
                                  dynasty + experience + party, 
                                data = Study2.data, subset = high.PC.data, 
                                fixed_effects = block, 
                                cluster = respondent)
round(summary(age.high.PC.result)$coefficients, 2)

## estimates for party using low-populist notes
age.low.PC.result <- lm_robust(selected ~ age * NMA + 
                                 gender + education + occupation + 
                                 dynasty + experience + party, 
                               data = Study2.data, subset = low.PC.data, 
                               fixed_effects = block, 
                               cluster = respondent)
round(summary(age.low.PC.result)$coefficients, 2)

## estimates for  using high-populist notes
education.high.PC.result <- lm_robust(selected ~ education * NMA + 
                                        gender + age + occupation + 
                                        dynasty + experience + party, 
                                      data = Study2.data, subset = high.PC.data, 
                                      fixed_effects = block, 
                                      cluster = respondent)
round(summary(education.high.PC.result)$coefficients, 2)

## estimates for party using low-populist notes
education.low.PC.result <- lm_robust(selected ~ education * NMA + 
                                       gender + age + occupation + 
                                       dynasty + experience + party, 
                                     data = Study2.data, subset = low.PC.data, 
                                     fixed_effects = block, 
                                     cluster = respondent)
round(summary(education.low.PC.result)$coefficients, 2)

## estimates for  using high-populist notes
occupation.high.PC.result <- lm_robust(selected ~ occupation * NMA + 
                                         gender + age + education + 
                                         dynasty + experience + party, 
                                       data = Study2.data, subset = high.PC.data, 
                                       fixed_effects = block, 
                                       cluster = respondent)
round(summary(occupation.high.PC.result)$coefficients, 2)

## estimates for party using low-populist notes
occupation.low.PC.result <- lm_robust(selected ~ occupation * NMA + 
                                        gender + age + education + 
                                        dynasty + experience + party, 
                                      data = Study2.data, subset = low.PC.data, 
                                      fixed_effects = block, 
                                      cluster = respondent)
round(summary(occupation.low.PC.result)$coefficients, 2)

## estimates for  using high-populist notes
dynasty.high.PC.result <- lm_robust(selected ~ dynasty * NMA + 
                                      gender + age + education + 
                                      occupation + experience + party, 
                                    data = Study2.data, subset = high.PC.data, 
                                    fixed_effects = block, 
                                    cluster = respondent)
round(summary(dynasty.high.PC.result)$coefficients, 2)

## estimates for party using low-populist notes
dynasty.low.PC.result <- lm_robust(selected ~ dynasty * NMA + 
                                     gender + age + education + 
                                     occupation + experience + party, 
                                   data = Study2.data, subset = low.PC.data, 
                                   fixed_effects = block, 
                                   cluster = respondent)
round(summary(dynasty.low.PC.result)$coefficients, 2)

## estimates for  using high-populist notes
experience.high.PC.result <- lm_robust(selected ~ experience * NMA + 
                                         gender + age + education + 
                                         occupation + dynasty + party, 
                                       data = Study2.data, subset = high.PC.data, 
                                       fixed_effects = block, 
                                       cluster = respondent)
round(summary(experience.high.PC.result)$coefficients, 2)

## estimates for party using low-populist notes
experience.low.PC.result <- lm_robust(selected ~ experience * NMA + 
                                        gender + age + education + 
                                        occupation + dynasty + party, 
                                      data = Study2.data, subset = low.PC.data, 
                                      fixed_effects = block, 
                                      cluster = respondent)
round(summary(experience.low.PC.result)$coefficients, 2)

## estimates for  using high-populist notes
party.high.PC.result <- lm_robust(selected ~ party * NMA + 
                                    gender + age + education + 
                                    occupation + dynasty + experience, 
                                  data = Study2.data, subset = high.PC.data, 
                                  fixed_effects = block, 
                                  cluster = respondent)
round(summary(party.high.PC.result)$coefficients, 2)

## estimates for party using low-populist notes
party.low.PC.result <- lm_robust(selected ~ party * NMA + 
                                   gender + age + education + 
                                   occupation + dynasty + experience, 
                                 data = Study2.data, subset = low.PC.data, 
                                 fixed_effects = block, 
                                 cluster = respondent)
round(summary(party.low.PC.result)$coefficients, 2)

## Figure A.11
cairo_pdf("Figure_A11.pdf", width = 6, height = 3, pointsize = 8)
layout(matrix(1:8, 2, 4, byrow = TRUE))
par(mar = c(2, 4, 4, 0.5), lwd = 0.5)
EE.plot(gender.high.PC.result, gender.low.PC.result, 9, 
        main = "Gender", sub = "(female v. male)", 
        xlim = c(-0.2, 0.6), line = seq(-0.2, 0.6, 0.2))
EE.plot(age.high.PC.result, age.low.PC.result, 9, 
        main = "Age", sub = "(34 v. 70)", 
        xlim = c(-0.2, 0.6), line = seq(-0.2, 0.6, 0.2))
EE.plot(education.high.PC.result, education.low.PC.result, 9, 
        main = "Education", sub = "(UTokyo v. high school)", 
        xlim = c(-0.2, 0.6), line = seq(-0.2, 0.6, 0.2))
EE.plot(occupation.high.PC.result, occupation.low.PC.result, 9, 
        main = "Prior occupation", sub = "(local politician v. celebrity)", 
        xlim = c(-0.2, 0.6), line = seq(-0.2, 0.6, 0.2))
EE.plot(dynasty.high.PC.result, dynasty.low.PC.result, 9, 
        main = "Dynastic status", sub = "(non-dynastic v. minister's child)", 
        xlim = c(-0.2, 0.6), line = seq(-0.2, 0.6, 0.2))
EE.plot(experience.high.PC.result, experience.low.PC.result, 9, 
        main = "Experience", sub = "(two terms v. no experience)", 
        xlim = c(-0.2, 0.6), line = seq(-0.2, 0.6, 0.2))
EE.plot(party.high.PC.result, party.low.PC.result, 9, 
        main = "Party affiliation", sub = "(LDP v. CDP)", 
        xlim = c(-0.2, 0.6), line = seq(-0.2, 0.6, 0.2))
plot(NULL, NULL, type = "n", bty = "n", xlim = c(0, 1), ylim = c(0, 1), 
     xlab = "", ylab = "", xaxt = "n", yaxt = "n")
legend("center", bty = "n", 
       legend = c("High-populist notes", "Low-populist notes"), 
       pch = c(19, 21), pt.bg = c(NA, "white"), cex = 1.2)
dev.off()

#### anti-elitism score interaction ####
## estimates for gender using high-populist notes
gender.high.AE.int.result <- lm_robust(selected ~ gender * NMA * ANT + 
                                         age + education + occupation + 
                                         dynasty + experience + party, 
                                       data = merged.Study2.data, subset = high.AE.data, 
                                       fixed_effects = block, 
                                       cluster = respondent)
round(summary(gender.high.AE.int.result)$coefficients, 2)

## estimates for gender using low-populist notes
gender.low.AE.int.result <- lm_robust(selected ~ gender * NMA * ANT + 
                                        age + education + occupation + 
                                        dynasty + experience + party, 
                                      data = merged.Study2.data, subset = low.AE.data, 
                                      fixed_effects = block, 
                                      cluster = respondent)
round(summary(gender.low.AE.int.result)$coefficients, 2)

## estimates for age using high-populist notes
age.high.AE.int.result <- lm_robust(selected ~ age * NMA * ANT + 
                                      gender + education + occupation + 
                                      dynasty + experience + party, 
                                    data = merged.Study2.data, subset = high.AE.data, 
                                    fixed_effects = block, 
                                    cluster = respondent)
round(summary(age.high.AE.int.result)$coefficients, 2)

## estimates for age using low-populist notes
age.low.AE.int.result <- lm_robust(selected ~ age * NMA * ANT + 
                                     gender + education + occupation + 
                                     dynasty + experience + party, 
                                   data = merged.Study2.data, subset = low.AE.data, 
                                   fixed_effects = block, 
                                   cluster = respondent)
round(summary(age.low.AE.int.result)$coefficients, 2)

## estimates for education using high-populist notes
education.high.AE.int.result <- lm_robust(selected ~ education * NMA * ANT + 
                                            gender + age + occupation + 
                                            dynasty + experience + party, 
                                          data = merged.Study2.data, subset = high.AE.data, 
                                          fixed_effects = block, 
                                          cluster = respondent)
round(summary(education.high.AE.int.result)$coefficients, 2)

## estimates for education using low-populist notes
education.low.AE.int.result <- lm_robust(selected ~ education * NMA * ANT + 
                                           gender + age + occupation + 
                                           dynasty + experience + party, 
                                         data = merged.Study2.data, subset = low.AE.data, 
                                         fixed_effects = block, 
                                         cluster = respondent)
round(summary(education.low.AE.int.result)$coefficients, 2)

## estimates for occupation using high-populist notes
occupation.high.AE.int.result <- lm_robust(selected ~ occupation * NMA * ANT + 
                                             gender + age + education + 
                                             dynasty + experience + party, 
                                           data = merged.Study2.data, subset = high.AE.data, 
                                           fixed_effects = block, 
                                           cluster = respondent)
round(summary(occupation.high.AE.int.result)$coefficients, 2)

## estimates for occupation using low-populist notes
occupation.low.AE.int.result <- lm_robust(selected ~ occupation * NMA * ANT + 
                                            gender + age + education + 
                                            dynasty + experience + party, 
                                          data = merged.Study2.data, subset = low.AE.data, 
                                          fixed_effects = block, 
                                          cluster = respondent)
round(summary(occupation.low.AE.int.result)$coefficients, 2)

## estimates for dynasty using high-populist notes
dynasty.high.AE.int.result <- lm_robust(selected ~ dynasty * NMA * ANT + 
                                          gender + age + education + 
                                          occupation + experience + party, 
                                        data = merged.Study2.data, subset = high.AE.data, 
                                        fixed_effects = block, 
                                        cluster = respondent)
round(summary(dynasty.high.AE.int.result)$coefficients, 2)

## estimates for dynasty using low-populist notes
dynasty.low.AE.int.result <- lm_robust(selected ~ dynasty * NMA * ANT + 
                                         gender + age + education + 
                                         occupation + experience + party, 
                                       data = merged.Study2.data, subset = low.AE.data, 
                                       fixed_effects = block, 
                                       cluster = respondent)
round(summary(dynasty.low.AE.int.result)$coefficients, 2)

## estimates for experience using high-populist notes
experience.high.AE.int.result <- lm_robust(selected ~ experience * NMA * ANT + 
                                             gender + age + education + 
                                             occupation + dynasty + party, 
                                           data = merged.Study2.data, subset = high.AE.data, 
                                           fixed_effects = block, 
                                           cluster = respondent)
round(summary(experience.high.AE.int.result)$coefficients, 2)

## estimates for experience using low-populist notes
experience.low.AE.int.result <- lm_robust(selected ~ experience * NMA * ANT + 
                                            gender + age + education + 
                                            occupation + dynasty + party, 
                                          data = merged.Study2.data, subset = low.AE.data, 
                                          fixed_effects = block, 
                                          cluster = respondent)
round(summary(experience.low.AE.int.result)$coefficients, 2)

## estimates for party using high-populist notes
party.high.AE.int.result <- lm_robust(selected ~ party * NMA * ANT + 
                                        gender + age + education + 
                                        occupation + dynasty + experience, 
                                      data = merged.Study2.data, subset = high.AE.data, 
                                      fixed_effects = block, 
                                      cluster = respondent)
round(summary(party.high.AE.int.result)$coefficients, 2)

## estimates for party using low-populist notes
party.low.AE.int.result <- lm_robust(selected ~ party * NMA * ANT + 
                                       gender + age + education + 
                                       occupation + dynasty + experience, 
                                     data = merged.Study2.data, subset = low.AE.data, 
                                     fixed_effects = block, 
                                     cluster = respondent)
round(summary(party.low.AE.int.result)$coefficients, 2)

## Table A.15
# high-populist notes
round(cbind(summary(gender.high.AE.int.result)$coefficients[c(1:3, 10:13), 1:2], 
            summary(age.high.AE.int.result)$coefficients[c(1:3, 10:13), 1:2], 
            summary(education.high.AE.int.result)$coefficients[c(1:3, 10:13), 1:2], 
            summary(occupation.high.AE.int.result)$coefficients[c(1:3, 10:13), 1:2], 
            summary(dynasty.high.AE.int.result)$coefficients[c(1:3, 10:13), 1:2], 
            summary(experience.high.AE.int.result)$coefficients[c(1:3, 10:13), 1:2], 
            summary(party.high.AE.int.result)$coefficients[c(1:3, 10:13), 1:2]), 2)
# low-populist notes
round(cbind(summary(gender.low.AE.int.result)$coefficients[c(1:3, 10:13), 1:2], 
            summary(age.low.AE.int.result)$coefficients[c(1:3, 10:13), 1:2], 
            summary(education.low.AE.int.result)$coefficients[c(1:3, 10:13), 1:2], 
            summary(occupation.low.AE.int.result)$coefficients[c(1:3, 10:13), 1:2], 
            summary(dynasty.low.AE.int.result)$coefficients[c(1:3, 10:13), 1:2], 
            summary(experience.low.AE.int.result)$coefficients[c(1:3, 10:13), 1:2], 
            summary(party.low.AE.int.result)$coefficients[c(1:3, 10:13), 1:2]), 2)

#### people-centrism score interaction ####
## estimates for gender using high-populist notes
gender.high.PC.int.result <- lm_robust(selected ~ gender * NMA * POP + 
                                         age + education + occupation + 
                                         dynasty + experience + party, 
                                       data = merged.Study2.data, subset = high.PC.data, 
                                       fixed_effects = block, 
                                       cluster = respondent)
round(summary(gender.high.PC.int.result)$coefficients, 2)

## estimates for gender using low-populist notes
gender.low.PC.int.result <- lm_robust(selected ~ gender * NMA * POP + 
                                        age + education + occupation + 
                                        dynasty + experience + party, 
                                      data = merged.Study2.data, subset = low.PC.data, 
                                      fixed_effects = block, 
                                      cluster = respondent)
round(summary(gender.low.PC.int.result)$coefficients, 2)

## estimates for age using high-populist notes
age.high.PC.int.result <- lm_robust(selected ~ age * NMA * POP + 
                                      gender + education + occupation + 
                                      dynasty + experience + party, 
                                    data = merged.Study2.data, subset = high.PC.data, 
                                    fixed_effects = block, 
                                    cluster = respondent)
round(summary(age.high.PC.int.result)$coefficients, 2)

## estimates for age using low-populist notes
age.low.PC.int.result <- lm_robust(selected ~ age * NMA * POP + 
                                     gender + education + occupation + 
                                     dynasty + experience + party, 
                                   data = merged.Study2.data, subset = low.PC.data, 
                                   fixed_effects = block, 
                                   cluster = respondent)
round(summary(age.low.PC.int.result)$coefficients, 2)

## estimates for education using high-populist notes
education.high.PC.int.result <- lm_robust(selected ~ education * NMA * POP + 
                                            gender + age + occupation + 
                                            dynasty + experience + party, 
                                          data = merged.Study2.data, subset = high.PC.data, 
                                          fixed_effects = block, 
                                          cluster = respondent)
round(summary(education.high.PC.int.result)$coefficients, 2)

## estimates for education using low-populist notes
education.low.PC.int.result <- lm_robust(selected ~ education * NMA * POP + 
                                           gender + age + occupation + 
                                           dynasty + experience + party, 
                                         data = merged.Study2.data, subset = low.PC.data, 
                                         fixed_effects = block, 
                                         cluster = respondent)
round(summary(education.low.PC.int.result)$coefficients, 2)

## estimates for occupation using high-populist notes
occupation.high.PC.int.result <- lm_robust(selected ~ occupation * NMA * POP + 
                                             gender + age + education + 
                                             dynasty + experience + party, 
                                           data = merged.Study2.data, subset = high.PC.data, 
                                           fixed_effects = block, 
                                           cluster = respondent)
round(summary(occupation.high.PC.int.result)$coefficients, 2)

## estimates for occupation using low-populist notes
occupation.low.PC.int.result <- lm_robust(selected ~ occupation * NMA * POP + 
                                            gender + age + education + 
                                            dynasty + experience + party, 
                                          data = merged.Study2.data, subset = low.PC.data, 
                                          fixed_effects = block, 
                                          cluster = respondent)
round(summary(occupation.low.PC.int.result)$coefficients, 2)

## estimates for dynasty using high-populist notes
dynasty.high.PC.int.result <- lm_robust(selected ~ dynasty * NMA * POP + 
                                          gender + age + education + 
                                          occupation + experience + party, 
                                        data = merged.Study2.data, subset = high.PC.data, 
                                        fixed_effects = block, 
                                        cluster = respondent)
round(summary(dynasty.high.PC.int.result)$coefficients, 2)

## estimates for dynasty using low-populist notes
dynasty.low.PC.int.result <- lm_robust(selected ~ dynasty * NMA * POP + 
                                         gender + age + education + 
                                         occupation + experience + party, 
                                       data = merged.Study2.data, subset = low.PC.data, 
                                       fixed_effects = block, 
                                       cluster = respondent)
round(summary(dynasty.low.PC.int.result)$coefficients, 2)

## estimates for experience using high-populist notes
experience.high.PC.int.result <- lm_robust(selected ~ experience * NMA * POP + 
                                             gender + age + education + 
                                             occupation + dynasty + party, 
                                           data = merged.Study2.data, subset = high.PC.data, 
                                           fixed_effects = block, 
                                           cluster = respondent)
round(summary(experience.high.PC.int.result)$coefficients, 2)

## estimates for experience using low-populist notes
experience.low.PC.int.result <- lm_robust(selected ~ experience * NMA * POP + 
                                            gender + age + education + 
                                            occupation + dynasty + party, 
                                          data = merged.Study2.data, subset = low.PC.data, 
                                          fixed_effects = block, 
                                          cluster = respondent)
round(summary(experience.low.PC.int.result)$coefficients, 2)

## estimates for party using high-populist notes
party.high.PC.int.result <- lm_robust(selected ~ party * NMA * POP + 
                                        gender + age + education + 
                                        occupation + dynasty + experience, 
                                      data = merged.Study2.data, subset = high.PC.data, 
                                      fixed_effects = block, 
                                      cluster = respondent)
round(summary(party.high.PC.int.result)$coefficients, 2)

## estimates for party using low-populist notes
party.low.PC.int.result <- lm_robust(selected ~ party * NMA * POP + 
                                       gender + age + education + 
                                       occupation + dynasty + experience, 
                                     data = merged.Study2.data, subset = low.PC.data, 
                                     fixed_effects = block, 
                                     cluster = respondent)
round(summary(party.low.PC.int.result)$coefficients, 2)

## Table A.16
# high-populist notes
round(cbind(summary(gender.high.PC.int.result)$coefficients[c(1:3, 10:13), 1:2], 
            summary(age.high.PC.int.result)$coefficients[c(1:3, 10:13), 1:2], 
            summary(education.high.PC.int.result)$coefficients[c(1:3, 10:13), 1:2], 
            summary(occupation.high.PC.int.result)$coefficients[c(1:3, 10:13), 1:2], 
            summary(dynasty.high.PC.int.result)$coefficients[c(1:3, 10:13), 1:2], 
            summary(experience.high.PC.int.result)$coefficients[c(1:3, 10:13), 1:2], 
            summary(party.high.PC.int.result)$coefficients[c(1:3, 10:13), 1:2]), 2)
# low-populist notes
round(cbind(summary(gender.low.PC.int.result)$coefficients[c(1:3, 10:13), 1:2], 
            summary(age.low.PC.int.result)$coefficients[c(1:3, 10:13), 1:2], 
            summary(education.low.PC.int.result)$coefficients[c(1:3, 10:13), 1:2], 
            summary(occupation.low.PC.int.result)$coefficients[c(1:3, 10:13), 1:2], 
            summary(dynasty.low.PC.int.result)$coefficients[c(1:3, 10:13), 1:2], 
            summary(experience.low.PC.int.result)$coefficients[c(1:3, 10:13), 1:2], 
            summary(party.low.PC.int.result)$coefficients[c(1:3, 10:13), 1:2]), 2)