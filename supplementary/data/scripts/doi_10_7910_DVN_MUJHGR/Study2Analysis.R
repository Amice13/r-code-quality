# MTurk re-weighting based on January 2014 CPS data
# 2015-05-22

# CPS Data: http://thedataweb.rm.census.gov/pub/cps/basic/201401-/jan14pub.zip
## file is called `jan14pub.dat`, a fixed-width format file
## dictionary is: http://thedataweb.rm.census.gov/pub/cps/basic/201401-/January_2014_Record_Layout.txt
## CPS data is itself weighted. Variable `PWCMPWGT` (Composited Final Weight; location 846-855) is used here.

# According to the "Knowledge Networks Methodology" document (from 2009), GfK KnowledgePanel weighting is based on:
# - gender (male/female)
# - age (18-29/30-44/45-59/60+)
# - race (white, non-hispanic / black non-hispanic / other, non-hispanic / 2+ races, non-hispanic / hispanic)
# - education (< high school / high school / some college / bachelor + )
# - region (NE / MW / S / W)
#   - Categorization here: http://www2.census.gov/geo/docs/maps-data/maps/reg_div.txt
#   - Codes do not match GfK state codes
# - Metro area (yes / no)
# - Internet access
#
# Note: 
# - metro area is not part of the TESS/GfK standard battery, so it was not measured in the MTurk surveys
# - Internet access is not measured on CPS (GfK draws it from their internal records)
# Neither of these measures were used in weighting; all other variables are used


# Poststratification weighting process:
# - Weights are generated based on iterative proportional fitting (raking)
# - Outliers are trimmed
# - Weights rescaled to sum to sample N

# Weighting has to be applied to each study individually


# ------------------------------ #
# ------------ CODE ------------ #
# ------------------------------ #

# setwd("C:/Users/thomas/Documents/Parallel/Combined/")
# setwd("C:/Users/Thomas/Documents/Dropbox/Parallel Experiments/CombinedPaper/")

library("rio")    # 0.2.2
library("survey") # 3.30-3
library("car")    # 2.0-25
library("rtf")    # 0.4-11

# CPS data
## data are fixed-width format; define a `wid` vector to extract only the necessary columns
## gender (PESEX, 129-130)
##  1: Male
##  2: Female
## age (PRTAGE, 122-123)
## race (PTDTRACE, 139-140)
##  01  White Only
##  02  Black Only 
##  03  American Indian, Alaskan Native Only   
##  04  Asian Only 
##  05  Hawaiian/Pacific Islander Only      
##  06  White-Black       
##  07  White-AI   
##  08  White-Asian      
##  09  White-HP      
##  10  Black-AI      
##  11  Black-Asian      
##  12  Black-HP        
##  13  AI-Asian        
##  14  AI-HP
##  15  Asian-HP      
##  16  W-B-AI      
##  17  W-B-A
##  18  W-B-HP        
##  19  W-AI-A        
##  20  W-AI-HP
##  21  W-A-HP
##  22  B-AI-A        
##  23  W-B-AI-A
##  24  W-AI-A-HP        
##  25  Other 3 Race Combinations        
##  26  Other 4 and 5 Race Combinations
## education (PEEDUCA, 137-138)
##  31  LESS THAN 1ST GRADE
##  32  1ST, 2ND, 3RD OR 4TH GRADE
##  33  5TH OR 6TH GRADE
##  34  7TH OR 8TH GRADE
##  35  9TH GRADE
##  36  10TH GRADE
##  37  11TH GRADE
##  38  12TH GRADE NO DIPLOMA
##  39  HIGH SCHOOL GRAD-DIPLOMA OR EQUIV (GED)
##  40  SOME COLLEGE BUT NO DEGREE
##  41  ASSOCIATE DEGREE-OCCUPATIONAL/VOCATIONAL
##  42  ASSOCIATE DEGREE-ACADEMIC PROGRAM
##  43  BACHELOR'S DEGREE (EX: BA, AB, BS)
##  44  MASTER'S DEGREE (EX: MA, MS, MEng, MEd, MSW)
##  45  PROFESSIONAL SCHOOL DEG (EX: MD, DDS, DVM)
##  46  DOCTORATE DEGREE (EX: PhD, EdD)
## region (GEREG; 89-90)
## final weight (PWCMPWGT, 846-855)
##  weight has 4 implied decimal places



# import the data from: http://thedataweb.rm.census.gov/pub/cps/basic/201401-/jan14pub.zip
# variable locations: 89-90, 122-123, 129-130, 137-138, 139-140, 846-855
# wid <- c(-88, 2, -31, 2, -5, 2, -6, 2, 2, -705, 10, -95)
# cumsum(abs(wid)) # check that `wid` matches locations
# cps <- setNames(import("jan14pub.dat", format = "fwf", widths = wid), 
#                 c("region", "age", "sex", "educ", "race", "weight"))
# saveRDS(cps, "cps.RDS")
cps <- readRDS("cps.RDS") # reload saved CPS data
cps$weight <- cps$weight/1e4 # account for implicit decimal places
cps[] <- lapply(cps, function(x) `[<-`(x, x == -1, NA))
cps <- cps[complete.cases(cps),]
cps <- cps[cps$age >= 18, ]

# recode CPS into GfK strata
## age
  # 1: 18-29
  # 2: 30-44
  # 3: 45-59
  # 4: else
cps$age <- recode(cps$age, "18:29=1;30:44=2;45:59=3;else=4")
## education
  # 1: < high school
  # 2: high school degree/GED
  # 3: some college or associates degree
  # 4: bachelor +
cps$educ <- recode(cps$educ, "31:38=1;39:40=2;41:42=3;else=4")
## race
  # 1: white, non-hispanic (CPS: white only)
  # 2: black, non-hispanic (CPS: black only)
  # 3: other non-hispanic
  # 4: 2+ races, non-hispanic
  # 5: hispanic
cps$race <- recode(cps$race, "1=1;2=2;3:5=3;c(6,7,8,10,11,13,16,17,19,22,23,25,26)=4;c(9,12,14,15,18,20,21,24)=5")



# load MTurk/TESS data and create stratification variables
d <- read.csv("Study2Data.csv", stringsAsFactors = FALSE)

# Stratification variables: `sex`, `qeduc`, `ppage`, `qstate`, `ppethm`, `QRACE1`, `QRACE2_*`
## `sex` already coded correctly
## `ppage`
d$age <- recode(d$ppage, "NA=NA;18:29=1;30:44=2;45:59=3;else=4")
### MTurk race (`QRACE1` and `QRACE2_*`)
hisp <- recode(d$QRACE1, "NA=NA;1=0;else=1")
white <- recode(d$QRACE2_1, "NA=NA;1=1;else=0")
black <- recode(d$QRACE2_2, "NA=NA;1=1;else=0")
races <- d[,names(d)[grepl("QRACE2", names(d))]]
races[] <- lapply(races, function(x) as.numeric(as.character(x)))
nraces <- recode(rowSums(races, na.rm = TRUE), "0=0;1=1;else=2")
white[nraces > 1 | hisp == 1] <- 0
black[nraces > 1 | hisp == 1] <- 0
twoplus <- ifelse(nraces == 2 & hisp != 1, 1, 0)
other <- ifelse(nraces == 1 & white != 1 & black != 1 & twoplus != 1, 1, 0)
hisp[white == 1 | black == 1 | other == 1 | twoplus == 1] <- 0
d$race <- NA
d$race[white == 1] <- 1
d$race[black == 1] <- 2
d$race[other == 1] <- 3
d$race[twoplus == 1] <- 4
d$race[hisp == 1] <- 5
### TESS race
d$race[d$sample == "TESS" & d$ppethm == "White, Non-Hispanic"] <- 1
d$race[d$sample == "TESS" & d$ppethm == "Black, Non-Hispanic"] <- 2
d$race[d$sample == "TESS" & d$ppethm == "Black/African-American, Non-Hispanic"] <- 2
d$race[d$sample == "TESS" & d$ppethm == "Other, Non-Hispanic"] <- 3
d$race[d$sample == "TESS" & d$ppethm == "2+ Races, Non-Hispanic"] <- 4
d$race[d$sample == "TESS" & d$ppethm == "Hispanic"] <- 5

## `qeduc`
d$educ <- recode(d$qeduc, "NA=NA;1:8=1;9=2;10:11=3;12:14=4")
## `qstate`
 ## Alabama (1) 
 ## Alaska (2) 
 ## Arizona (3) 
 ## Arkansas (4) 
 ## California (5) 
 ## Colorado (6) 
 ## Connecticut (7) 
 ## Delaware (8) 
 ## District of Columbia (9) 
 ## Florida (10) 
 ## Georgia (11) 
 ## Hawaii (12) 
 ## Idaho (13) 
 ## Illinois (14) 
 ## Indiana (15) 
 ## Iowa (16) 
 ## Kansas (17) 
 ## Kentucky (18) 
 ## Louisiana (19) 
 ## Maine (20) 
 ## Maryland (21) 
 ## Massachusetts (22) 
 ## Michigan (23) 
 ## Minnesota (24) 
 ## Mississippi (25) 
 ## Missouri (26) 
 ## Montana (27) 
 ## Nebraska (28) 
 ## Nevada (29) 
 ## New Hampshire (30) 
 ## New Jersey (31) 
 ## New Mexico (32) 
 ## New York (33) 
 ## North Carolina (34) 
 ## North Dakota (35) 
 ## Ohio (36) 
 ## Oklahoma (37) 
 ## Oregon (38) 
 ## Pennsylvania (39) 
 ## Puerto Rico (40) 
 ## Rhode Island (41) 
 ## South Carolina (42) 
 ## South Dakota (43) 
 ## Tennessee (44) 
 ## Texas (45) 
 ## Utah (46) 
 ## Vermont (47) 
 ## Virginia (48) 
 ## Washington (49) 
 ## West Virginia (50) 
 ## Wisconsin (51) 
 ## Wyoming (52) 
 ## I do not reside in the United States (53) 
d$region <- recode(d$qstate, "c(7,20,22,30,31,33,39,41,47)=1;
                              c(14,15,16,17,18,19,23,24,26,28,35,36,43,51)=2;
                              c(1,4,8,9,10,11,21,25,34,37,42,44,45,48,50)=3;
                              c(2,3,5,6,12,13,27,29,32,38,46,49,52)=4;
                              c(40,53,NA)=NA;")


# --------------------------------------------------- #
# weight based on CPS (using Composited Final Weight) #
# --------------------------------------------------- #

# function to reweight MTurk data (via raking)
genweights <- function(dat, studyvar, pop) {
    
    # trim missing data in stratifying variables
    d <- dat[!is.na(dat[,studyvar]) & dat$sample == "MTurk",]
    cc <- complete.cases(d[,c("sex","age","race","educ","region")])
    d <- d[cc,]
    
    # create a survey sample design using `svydesign`
    samp <- svydesign(id = ~0, weights = ~0, data = d)
    
    # we actually have the joint distribution, so we could directly poststratify
    #ps <- postStratify(design = samp, strata = ~ sex + age + race + educ + region, population = pop)
    
    # but given missingness in some CPS cells, raking will be easier
    # this also sounds closer to what GfK does
    r <- rake(design = samp, 
              sample.margins = list(~sex, ~age, ~race, ~educ, ~region), 
              population.margins = list(svytable(~sex, design = pop), 
                                        svytable(~age, design = pop), 
                                        svytable(~race, design = pop), 
                                        svytable(~educ, design = pop), 
                                        svytable(~region, design = pop)),
              control = list(maxit = 30)) # allow 30 iteration
    w <- weights(r)
    
    # trim outlier weights (top/bottom 1% become top/bottom-coded)
    qw <- quantile(w, c(0.01,0.99))
    w[w < qw[1]] <- qw[1]
    w[w > qw[2]] <- qw[2]
    
    # rescale to sum to total observed cases in (subsampled/non-missing) data
    # this preserves sample size while changing relative weight of observations
    n <- nrow(d)
    w <- w/(sum(w)/n)
    
    # store weight back into data and return
    d$mturkpsweight <- w
    # return data with new weighted variable
    return(d)
}

# function to estimate treatment effects
TEs <- function(fulldat, mturkdat, trvar, ovar, ctrl = 1, tr = 2) {
    m <- matrix(NA, ncol = 2, nrow = 5)
    rownames(m) <- c("TESS Unweighted", "TESS Weighted", 
                     "MTurk Unweighted (Full)", "MTurk Unweighted (Subsample)", 
                     "MTurk Weighted (Subsample)")
    n <- numeric(5)
    se <- numeric(5)
    
    perm <- 5000 # 5000 permutation samples
    
    # function to perform permutation test
    TE <- function(trvar, ovar, w, ctrl = 1, tr = 2) {
        ovar <- ovar[trvar %in% c(ctrl, tr)]
        if(!missing(w))
            w <- w[trvar %in% c(ctrl, tr)]
        trvar <- trvar[trvar %in% c(ctrl, tr)]
        s <- trvar[sample(seq_along(trvar), length(trvar), FALSE)]
        if(missing(w)) {
            x <- weighted.mean(x = ovar[s == ctrl], na.rm = TRUE)
            y <- weighted.mean(x = ovar[s == tr], na.rm = TRUE)
        } else {
            x <- weighted.mean(x = ovar[s == ctrl], w = w[s == ctrl], na.rm = TRUE)
            y <- weighted.mean(x = ovar[s == tr], w = w[s == tr], na.rm = TRUE)
        }
        return(x - y)
    }
    
    # original (unweighted) TESS data
    m[1,1] <- 
    weighted.mean(x = fulldat[fulldat[, trvar] == ctrl & fulldat[,"sample"] == "TESS", ovar], 
                  na.rm = TRUE)
    m[1,2] <- 
    weighted.mean(x = fulldat[fulldat[, trvar] == tr & fulldat[,"sample"] == "TESS", ovar], 
                  na.rm = TRUE)
    n[1] <- nrow(fulldat[fulldat[, trvar] %in% c(ctrl,tr) & fulldat[,"sample"] == "TESS" & !is.na(fulldat[,ovar]), ])
    p1 <- replicate(perm, TE(trvar = fulldat[fulldat$sample == "TESS", trvar], 
                             ovar = fulldat[fulldat$sample == "TESS", ovar]))
    se[1] <- sd(p1)
    
    # original (weighted) TESS data
    m[2,1] <- 
    weighted.mean(x = fulldat[fulldat[, trvar] == ctrl & fulldat[,"sample"] == "TESS", ovar], 
                  w = fulldat[fulldat[, trvar] == ctrl & fulldat[,"sample"] == "TESS","weight"],
                  na.rm = TRUE)
    m[2,2] <- 
    weighted.mean(x = fulldat[fulldat[, trvar] == tr & fulldat[,"sample"] == "TESS", ovar], 
                  w = fulldat[fulldat[, trvar] == tr & fulldat[,"sample"] == "TESS","weight"],
                  na.rm = TRUE)
    n[2] <- nrow(fulldat[fulldat[, trvar] %in% c(ctrl,tr) & fulldat[,"sample"] == "TESS" & !is.na(fulldat[,ovar]), ])
    p2 <- replicate(perm, TE(trvar = fulldat[fulldat$sample == "TESS", trvar], 
                             ovar = fulldat[fulldat$sample == "TESS", ovar],
                             w = fulldat[fulldat$sample == "TESS", "weight"]))
    se[2] <- sd(p2)
    
    # unweighted full MTurk data
    m[3,1] <- 
    weighted.mean(x = fulldat[fulldat[, trvar] == ctrl & fulldat[,"sample"] == "MTurk", ovar], 
                  na.rm = TRUE)
    m[3,2] <- 
    weighted.mean(x = fulldat[fulldat[, trvar] == tr & fulldat[,"sample"] == "MTurk", ovar], 
                  na.rm = TRUE)
    n[3] <- nrow(fulldat[fulldat[, trvar] %in% c(ctrl,tr) & fulldat[,"sample"] == "MTurk" & !is.na(fulldat[,ovar]), ])
    p3 <- replicate(perm, TE(trvar = fulldat[fulldat$sample == "MTurk", trvar], 
                             ovar = fulldat[fulldat$sample == "MTurk", ovar]))
    se[3] <- sd(p3)
    
    # unweighted, subsample MTurk data
    m[4,1] <- 
    weighted.mean(x = mturkdat[mturkdat[, trvar] == ctrl & mturkdat[,"sample"] == "MTurk", ovar], 
                  na.rm = TRUE)
    m[4,2] <- 
    weighted.mean(x = mturkdat[mturkdat[, trvar] == tr & mturkdat[,"sample"] == "MTurk", ovar], 
                  na.rm = TRUE)
    n[4] <- nrow(mturkdat[mturkdat[, trvar] %in% c(ctrl,tr) & mturkdat[,"sample"] == "MTurk" & !is.na(mturkdat[,ovar]), ])
    p4 <- replicate(perm, TE(trvar = mturkdat[mturkdat$sample == "MTurk", trvar], 
                             ovar = mturkdat[mturkdat$sample == "MTurk", ovar]))
    se[4] <- sd(p4)
    
    # weighted subsample MTurk data
    m[5,1] <- 
    weighted.mean(x = mturkdat[mturkdat[, trvar] == ctrl & mturkdat[,"sample"] == "MTurk", ovar], 
                  w = mturkdat[mturkdat[, trvar] == ctrl & mturkdat[,"sample"] == "MTurk","mturkpsweight"],
                  na.rm = TRUE)
    m[5,2] <- 
    weighted.mean(x = mturkdat[mturkdat[, trvar] == tr & mturkdat[,"sample"] == "MTurk", ovar], 
                  w = mturkdat[mturkdat[, trvar] == tr & mturkdat[,"sample"] == "MTurk","mturkpsweight"],
                  na.rm = TRUE)
    n[5] <- nrow(mturkdat[mturkdat[, trvar] %in% c(ctrl,tr) & mturkdat[,"sample"] == "MTurk" & !is.na(mturkdat[,ovar]), ])
    p5 <- replicate(perm, TE(trvar = mturkdat[mturkdat$sample == "MTurk", trvar], 
                             ovar = mturkdat[mturkdat$sample == "MTurk", ovar],
                             w = mturkdat[mturkdat$sample == "MTurk", "mturkpsweight"]))
    se[5] <- sd(p5)
    
    # combine and return
    effs <- apply(m, 1, diff)
    ## DID is difference-in-differences between weighted TESS and full unweighted MTurk
    m <- cbind(m, effs, se, effs[3]-effs[2], sd(p3-p2), n)
    colnames(m) <- c("Control", "Treatment", "Effect", "SE", "DID", "DIDSE", "N")
    return(m)
}



# population survey design
pop <- svydesign(id = ~0, weights = ~weight, data = cps)
# weighted MTurk data for each study
w <- lapply(paste0("study_number_S", 1:20), genweights, dat = d, pop = pop)


# calculate TEs for each study (Treatment Group 1 vs Control)
set.seed(12345)
out1 <- list()
for(i in 1:20) {
   out1[[i]] <- TEs(fulldat = d, mturkdat = w[[i]], trvar = paste0("group_S", i), ovar = paste0("DV_S", i))
}

# write human-readable results
#sink("ResultsTreatmentGroup1-full.txt")
#out1
#sink()
# write R-readable results
#saveRDS(out1, "ResultsTreatmentGroup1-full.RDS")
#out1 <- readRDS("ResultsTreatmentGroup1-full.RDS")


# calculate TEs for each study (Treatment Group 2 vs Control)
set.seed(12345)
out2 <- list()
for(i in 1:20) {
   out2[[i]] <- TEs(fulldat = d, mturkdat = w[[i]], trvar = paste0("group_S", i), ovar = paste0("DV_S", i), tr = 3)
}

# write human-readable results
#sink("ResultsTreatmentGroup2-full.txt")
#out2
#sink()
# write R-readable results
#saveRDS(out2, "ResultsTreatmentGroup2-full.RDS")
#out2 <- readRDS("ResultsTreatmentGroup2-full.RDS")

# calculate TEs for each study (Treatment Group 2 vs Treatment Group 1)
set.seed(12345)
out3 <- list()
for(i in 1:20) {
   out3[[i]] <- TEs(fulldat = d, mturkdat = w[[i]], trvar = paste0("group_S", i), ovar = paste0("DV_S", i), ctrl = 2, tr = 3)
}

# write human-readable results
#sink("ResultsTreatmentTreatment-full.txt")
#out3
#sink()
# write R-readable results
#saveRDS(out3, "ResultsTreatmentTreatment-full.RDS")
#out3 <- readRDS("ResultsTreatmentTreatment-full.RDS")


# plot results
addest <- function(x, n, which = c(2,5), pos = c(0.3, -0.3), col = c("black", "gray"), pch = 23) {
    e <- x[[n]][which,"Effect"]
    if(e[1] < 0) {
        e <- (-1 * e)
    }
    se <- x[[n]][which,"SE"]
    n <- n*2
    n <- n + pos
    points(e, n, col = col, bg = col, pch = pch, cex = 0.75)
    segments(e - se, n, e + se, n, lwd = 2, col = col)
    segments(e - 2 * se, n, e + 2 * se, n, lwd = 1, col = col)
}

# Study 2, Treatment Group 1 (TESS Weighted, MTurk Unweighted)
s2t1 <- abs(sapply(out1, `[`, 2, "Effect"))
s2t1order <- order(s2t1)
png(filename = "ResultsTreatmentGroup1.png", height = 6, width = 7.5, units = "in", res = 300)
plot(NA, xlim = c(-1.1, 2.1), ylim = c(0, (2*length(s2t1order) + 2)), yaxt = "n", yaxs = "i",
     xlab = "Average Treatment Effect", ylab = "Experiment Number", main = "")
abline(v = 0, col = "gray")
legend("bottomright", legend = c("TESS", "MTurk"), fill = c("black", "gray"), bty = "n")
axis(2, seq_along(s2t1order)*2, s2t1order, las = 2)
invisible(sapply(seq_along(s2t1order), addest, x = out1[s2t1order], which = c(2,3)))
dev.off()

# Study 2, Treatment Group 2 (TESS Weighted, MTurk Unweighted)
s2t2 <- abs(sapply(out2, `[`, 2, "Effect"))
s2t2order <- order(s2t2)[!is.nan(s2t2)[order(s2t2)]]
png(filename = "ResultsTreatmentGroup2.png", height = 6, width = 7.5, units = "in", res = 300)
plot(NA, xlim = c(-1.1, 2.1), ylim = c(0, (2*length(s2t2order) + 2)), yaxt = "n", yaxs = "i",
     xlab = "Average Treatment Effect", ylab = "Experiment Number", main = "")
abline(v = 0, col = "gray")
legend("bottomright", legend = c("TESS", "MTurk"), fill = c("black", "gray"), bty = "n")
axis(2, seq_along(s2t2order)*2, s2t2order, las = 2)
invisible(sapply(seq_along(s2t2order), addest, x = out2[s2t2order], which = c(2,3)))
dev.off()

# Study 2, Treatment Group 2 versus Treatment Group 1 (TESS Weighted, MTurk Unweighted)
s2t3 <- abs(sapply(out3, `[`, 2, "Effect"))
s2t3order <- order(s2t3)[!is.nan(s2t3)[order(s2t3)]]
png(filename = "ResultsTreatmentTreatment.png", height = 6, width = 7.5, units = "in", res = 300)
plot(NA, xlim = c(-1.1, 2.1), ylim = c(0, (2*length(s2t3order) + 2)), yaxt = "n", yaxs = "i",
     xlab = "Average Treatment Effect", ylab = "Experiment Number", main = "")
abline(v = 0, col = "gray")
legend("bottomright", legend = c("TESS", "MTurk"), fill = c("black", "gray"), bty = "n")
axis(2, seq_along(s2t3order)*2, s2t3order, las = 2)
invisible(sapply(seq_along(s2t3order), addest, x = out3[s2t3order], which = c(2,3)))
dev.off()


# All groups combined
cl <- c("black", "black", "gray", "gray") # colors
p <- c(21,23,21,23)  # symbols
ps <- c(0.6,0.3,-0.3,-0.6)  # placement
wh <- c(2,1,5,3)  # which effects (leave out unweighted MTurk, full sample)
leg <- c("TESS (Weighted)", "TESS (Unweighted)", "MTurk (Weighted)", "MTurk (Unweighted)")

# Study 2, Treatment Group 1 (All Groups)
png(filename = "ResultsTreatmentGroup1-Appendix.png", height = 10, width = 7.5, units = "in", res = 300)
plot(NA, xlim = c(-1.1, 2.1), ylim = c(0, (2*length(s2t1order) + 2)), yaxt = "n", yaxs = "i",
     xlab = "Average Treatment Effect", ylab = "Experiment Number", main = "")
abline(v = 0, col = "gray")
legend("bottomright", legend = leg, col = cl, pt.bg = cl, pch = p, bty = "n")
axis(2, seq_along(s2t1order)*2, s2t1order, las = 2)
invisible(sapply(seq_along(s2t1order), addest, x = out1[s2t1order], which = wh, pos = ps, col = cl, pch = p))
dev.off()

# Study 2, Treatment Group 2 (All Groups)
png(filename = "ResultsTreatmentGroup2-Appendix.png", height = 10, width = 7.5, units = "in", res = 300)
plot(NA, xlim = c(-1.1, 2.1), ylim = c(0, (2*length(s2t2order) + 2)), yaxt = "n", yaxs = "i",
     xlab = "Average Treatment Effect", ylab = "Experiment Number", main = "")
abline(v = 0, col = "gray")
legend("bottomright", legend = leg, col = cl, pt.bg = cl, pch = p, bty = "n")
axis(2, seq_along(s2t2order)*2, s2t2order, las = 2)
invisible(sapply(seq_along(s2t2order), addest, x = out2[s2t2order], which = wh, pos = ps, col = cl, pch = p))
dev.off()

# Study 2, Treatment Group 2 versus Treatment Group 1 (All Groups)
png(filename = "ResultsTreatmentTreatment-Appendix.png", height = 10, width = 7.5, units = "in", res = 300)
plot(NA, xlim = c(-1.1, 2.1), ylim = c(0, (2*length(s2t3order) + 2)), yaxt = "n", yaxs = "i",
     xlab = "Average Treatment Effect", ylab = "Experiment Number", main = "")
abline(v = 0, col = "gray")
legend("bottomright", legend = leg, col = cl, pt.bg = cl, pch = p, bty = "n")
axis(2, seq_along(s2t3order)*2, s2t3order, las = 2)
invisible(sapply(seq_along(s2t3order), addest, x = out3[s2t3order], which = wh, pos = ps, col = cl, pch = p))
dev.off()




# Supplemental Results Document
local({
    # results table function
    resultstab <- function(x, a, b, did = TRUE) {
        f <- function(z) sprintf("%0.2f", z)
        a <- c(f(x[a,"Control"]), f(x[a,"Treatment"]), f(x[a,"Effect"]), x[a,"N"], 
               f(x[b,"Control"]), f(x[b,"Treatment"]), f(x[b,"Effect"]), x[b,"N"])
        if(did) {
            if(!is.nan(x[1,"DID"])) {
                c(a, paste0(f(x[1,"DID"]), " (", f(x[1,"DIDSE"]), ")"))
            } else {
                c(a, "")
            }
        } else {
            a
        }
    }

    rtf <- RTF("Study2SupplementalResults.doc")

    # treatment group 1 means and effects
    addParagraph(rtf, "Study 2 Treatment Group 1 (TESS Weighted and TESS Unweighted)\n\n")
    a1 <- `rownames<-`(`colnames<-`(t(sapply(out1, resultstab, a = 2, b = 1)), c(rep(c("Control", "Treatment", "Effect", "N"), 2), "DID (SE)")), 1:20)
    addTable(rtf, a1, row.names = TRUE)

    addPageBreak(rtf)
    addParagraph(rtf, "Study 2 Treatment Group 1 (MTurk Weighted and MTurk Unweighted)\n\n")
    b1 <- `rownames<-`(`colnames<-`(t(sapply(out1, resultstab, a = 5, b = 3, did = FALSE)), c(rep(c("Control", "Treatment", "Effect", "N"), 2))), 1:20)
    addTable(rtf, b1, row.names = TRUE)

    # treatment group 2 means and effects
    addPageBreak(rtf)
    addParagraph(rtf, "Study 2 Treatment Group 2 (TESS Weighted and MTurk Unweighted)\n\n")
    a2 <- `rownames<-`(`colnames<-`(t(sapply(out2, resultstab, a = 2, b = 3)), c(rep(c("Control", "Treatment", "Effect", "N"), 2), "DID (SE)")), 1:20)
    addTable(rtf, a2, row.names = TRUE)

    addPageBreak(rtf)
    addParagraph(rtf, "Study 2 Treatment Group 2 (MTurk Weighted and MTurk Unweighted)\n\n")
    b2 <- `rownames<-`(`colnames<-`(t(sapply(out2, resultstab, a = 5, b = 3, did = FALSE)), c(rep(c("Control", "Treatment", "Effect", "N"), 2))), 1:20)
    addTable(rtf, b2, row.names = TRUE)


    # treatment group 2 versus 1 means and effects
    addPageBreak(rtf)
    addParagraph(rtf, "Study 2 Treatment Group 2 versus 1 (TESS Weighted and MTurk Unweighted)\n\n")
    a2 <- `rownames<-`(`colnames<-`(t(sapply(out3, resultstab, a = 2, b = 3)), c(rep(c("Control", "Treatment", "Effect", "N"), 2), "DID (SE)")), 1:20)
    addTable(rtf, a2, row.names = TRUE)

    addPageBreak(rtf)
    addParagraph(rtf, "Study 2 Treatment Group 2 versus 1 (MTurk Weighted and MTurk Unweighted)\n\n")
    b2 <- `rownames<-`(`colnames<-`(t(sapply(out3, resultstab, a = 5, b = 3, did = FALSE)), c(rep(c("Control", "Treatment", "Effect", "N"), 2))), 1:20)
    addTable(rtf, b2, row.names = TRUE)


    # demographics (TESS weighted/unweighted, MTurk weighted/unweighted)
    demos <- function(study) {
        tage <- sprintf("%0.2f", 100*prop.table(table(d$age[d$sample == "TESS" & !is.na(d[,study])])))
        mage <- sprintf("%0.2f", 100*prop.table(table(d$age[d$sample == "MTurk" & !is.na(d[,study])])))
        
        trace <- sprintf("%0.2f", 100*prop.table(table(d$race[d$sample == "TESS" & !is.na(d[,study])]))[c(1:2,5)])
        mrace <- sprintf("%0.2f", 100*prop.table(table(d$race[d$sample == "MTurk" & !is.na(d[,study])]))[c(1:2,5)])
        
        teduc <- sprintf("%0.2f", 100*prop.table(table(d$educ[d$sample == "TESS" & !is.na(d[,study])])))
        meduc <- sprintf("%0.2f", 100*prop.table(table(d$educ[d$sample == "MTurk" & !is.na(d[,study])])))
        
        c(  # - gender (male/female)
            sprintf("%0.2f", 100*prop.table(table(d$sex[d$sample == "TESS" & !is.na(d[,study])]))[2]),
            sprintf("%0.2f", 100*prop.table(table(d$sex[d$sample == "MTurk" & !is.na(d[,study])]))[2]),
            # - age (18-29/30-44/45-59/60+)
            mapply(c, tage, mage),
            # - race (white, non-hispanic / black non-hispanic / other, non-hispanic / 2+ races, non-hispanic / hispanic)
            mapply(c, trace, mrace),
            # - education (< high school / high school / some college / bachelor + )
            mapply(c, teduc, meduc)
        )
    }

    cn <- c("Female (%)", "Female (%)", "18-29 (%)", "18-29 (%)", "30-44 (%)", "30-44 (%)", "45-59 (%)", "45-59 (%)", "60+ (%)", "60+ (%)",
            "White, Non-Hispanic (%)", "White, Non-Hispanic (%)", "Black, Non-Hispanic (%)", "Black, Non-Hispanic (%)", "Hispanic (%)", "Hispanic (%)", 
            "<HS (%)", "<HS (%)", "HS (%)", "HS (%)", "Some College (%)", "Some College (%)", "Bachelor+ (%)", "Bachelor+ (%)")
    cn <- paste0(c("TESS ", "MTurk "), cn)
    d1 <- `rownames<-`(`colnames<-`(t(sapply(paste0("study_number_S", 1:20), demos)), cn), 1:20)

    d1 <- rbind(d1, c(sprintf("%0.2f", 100*prop.table(svytable(~sex, design = pop))[2]), "",
              mapply(c, sprintf("%0.2f", 100*prop.table(svytable(~age, design = pop))), ""),
              mapply(c, sprintf("%0.2f", 100*prop.table(svytable(~race, design = pop))[c(1,2,5)]), ""),
              mapply(c, sprintf("%0.2f", 100*prop.table(svytable(~educ, design = pop))), "")))
    rownames(d1)[21] <- "CPS"

    addPageBreak(rtf)
    addParagraph(rtf, "Study 2 Demographics (Sex and Age)\n")
    addTable(rtf, d1[,1:10], row.names = TRUE)
    addPageBreak(rtf)
    addParagraph(rtf, "Study 2 Demographics (Race and Ethnicity)\n")
    addTable(rtf, d1[,11:16], row.names = TRUE)
    addPageBreak(rtf)
    addParagraph(rtf, "Study 2 Demographics (Education)\n")
    addTable(rtf, d1[,17:24], row.names = TRUE)

    done(rtf)
}) 

