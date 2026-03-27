# Thomas J. Leeper and Kevin J. Mullinix
# Created 2013-03-15
# Last Updated 2015-06-12

# PARALLEL EXPERIMENTS ANALYSIS

## LOAD PACKAGES ##
library("car")
library("coin")
library("xtable")
library("devtools")
library("rtf")
source("Analysis-Functions.R")

if (FALSE) {
    ## COMBINE DATASETS ##
    source('Analysis-Data-Combining.R')
    ## READ IN DATA ##
    mydat <- read.csv("data-combined.csv") 	# non-anonymized
    ## RECODE DATA ##
    source('Analysis-Recoding.R')
    ## ANONYMIZE DATA FOR REPLICATION FILES ##
    repdata <- mydat
    repdata <- repdata[,!names(repdata) %in% c('WorkerId','AssignmentId','BQ1.1','BQ1.2')]
    write.csv(repdata,'Study1Data.csv')
    rm(repdata)
}
mydat <- read.csv("Study1Data.csv") 	# anonymized

## MAIN ANALYSIS FOR EACH EXPERIMENT ##
# one plot for all studies


# put all estimates (for samples 1:5) into one list
## Samples: 
#- 1.	TESS: Nationally representative sample provided by GfK/formerly Knowledge Networks
#- 2.	Exit Poll
#- 3.	Students in a laboratory
#- 4.	Staff in a laboratory
#- 5.	Amazon Mechanical Turk workers
#- 6.	Craigslist ads (we leave this out because sample is too small
#- 7.	Facebook ads (we leave this out because sample is too small

# rescale 0-1
permdat <- mydat[, c("LoanGroupNum", "LoanSuppR", "RallyGroup", "RallyAllowR", "DREAMGroup", "DREAMSuppR", "party3", "Sample", "weight")]
permdat$LoanSuppR <- (permdat$LoanSuppR-1)/6
permdat$RallyAllowR <- (permdat$RallyAllowR-1)/6
permdat$DREAMSuppR <- (permdat$DREAMSuppR-1)/6
# student loans (Tr: 1; Ctrl: 2)
## permdat$LoanSuppR[permdat$LoanGroupNum==1]
## permdat$LoanSuppR[permdat$LoanGroupNum==2]

# hate rally (collapse distant and local; Tr: 1,3; Ctrl: 2,4)
## permdat$RallyAllowR[permdat$RallyGroup %in% c(1,3)]
## permdat$RallyAllowR[permdat$RallyGroup %in% c(2,4)]

# DREAM Act (Tr: 4; Ctr: 5)
## permdat$DREAMSuppR[permdat$DREAMGroup==4]
## permdat$DREAMSuppR[permdat$DREAMGroup==5]


s1TE <- function(trvar, ovar, w, ctrl = 1, tr = 2) {
    ovar <- ovar[trvar %in% c(ctrl, tr)]
    if(!missing(w))
        w <- w[trvar %in% c(ctrl, tr)]
    trvar <- trvar[trvar %in% c(ctrl, tr)]
    s <- trvar[sample(seq_along(trvar), length(trvar), FALSE)]
    if(missing(w)) {
        x <- weighted.mean(x = ovar[s %in% ctrl], na.rm = TRUE)
        y <- weighted.mean(x = ovar[s %in% tr], na.rm = TRUE)
    } else {
        x <- weighted.mean(x = ovar[s %in% ctrl], w = w[s %in% ctrl], na.rm = TRUE)
        y <- weighted.mean(x = ovar[s %in% tr], w = w[s %in% tr], na.rm = TRUE)
    }
    return(y-x)
}

set.seed(12345)

# put TESS last (so that it appears on top of figures)
study1plot <- lapply(c(2:5,1), function(z) {
    
    perm <- 5000 # 5000 permutation samples
    tmean <- numeric(3)
    cmean <- numeric(3)
    te <- numeric(3)
    n <- numeric(3)
    
    if(z == 1) { # use weighted means
        # student loans
        p_loan <- replicate(perm, s1TE(trvar = permdat[permdat$Sample == z, "LoanGroupNum"], 
                                       ovar = permdat[permdat$Sample == z, "LoanSuppR"],
                                       w = permdat[permdat$Sample == z, "weight"],
                                       tr = 2, ctrl = 1)) # reversed tr and ctrl, so effect is positive
        tmean[1] <- mean(permdat$LoanSuppR[permdat$Sample == z & permdat$LoanGroupNum == 2], na.rm = TRUE)
        cmean[1] <- mean(permdat$LoanSuppR[permdat$Sample == z & permdat$LoanGroupNum == 1], na.rm = TRUE)
        te[1] <- tmean[1] - cmean[1]
        n[1] <- length(permdat$LoanSuppR[permdat$Sample == z & permdat$LoanGroupNum %in% 1:2])
        
        # hate rally
        p_rally <- replicate(perm, s1TE(trvar = permdat[permdat$Sample == z, "RallyGroup"], 
                                        ovar = permdat[permdat$Sample == z, "RallyAllowR"],
                                        w = permdat[permdat$Sample == z, "weight"],
                                        tr = c(1,3), ctrl = c(2,4)))
        tmean[2] <- mean(permdat$RallyAllowR[permdat$Sample == z & permdat$RallyGroup %in% c(1,3)], na.rm = TRUE)
        cmean[2] <- mean(permdat$RallyAllowR[permdat$Sample == z & permdat$RallyGroup %in% c(2,4)], na.rm = TRUE)
        te[2] <- tmean[2] - cmean[2]
        n[2] <- length(permdat$RallyAllowR[permdat$Sample == z & permdat$RallyGroup %in% 1:4])
        
        # DREAM Act
        p_dream <- replicate(perm, s1TE(trvar = permdat[permdat$Sample == z, "DREAMGroup"], 
                                        ovar = permdat[permdat$Sample == z, "DREAMSuppR"],
                                        w = permdat[permdat$Sample == z, "weight"],
                                        tr = 5, ctrl = 4)) # reversed tr and ctrl, so effect is positive
        tmean[3] <- mean(permdat$DREAMSuppR[permdat$Sample == z & permdat$DREAMGroup == 5], na.rm = TRUE)
        cmean[3] <- mean(permdat$DREAMSuppR[permdat$Sample == z & permdat$DREAMGroup == 4], na.rm = TRUE)
        te[3] <- tmean[3] - cmean[3]
        n[3] <- length(permdat$DREAMSuppR[permdat$Sample == z & permdat$DREAMGroup %in% 4:5])
        
    } else {
        # student loans
        p_loan <- replicate(perm, s1TE(trvar = permdat[permdat$Sample == z, "LoanGroupNum"], 
                                       ovar = permdat[permdat$Sample == z, "LoanSuppR"],
                                       tr = 2, ctrl = 1)) # reversed tr and ctrl, so effect is positive
        tmean[1] <- mean(permdat$LoanSuppR[permdat$Sample == z & permdat$LoanGroupNum == 2], na.rm = TRUE)
        cmean[1] <- mean(permdat$LoanSuppR[permdat$Sample == z & permdat$LoanGroupNum == 1], na.rm = TRUE)
        te[1] <- tmean[1] - cmean[1]
        n[1] <- length(permdat$LoanSuppR[permdat$Sample == z & permdat$LoanGroupNum %in% 1:2])
        
        # hate rally
        p_rally <- replicate(perm, s1TE(trvar = permdat[permdat$Sample == z, "RallyGroup"], 
                                        ovar = permdat[permdat$Sample == z, "RallyAllowR"],
                                        tr = c(1,3), ctrl = c(2,4)))
        tmean[2] <- mean(permdat$RallyAllowR[permdat$Sample == z & permdat$RallyGroup %in% c(1,3)], na.rm = TRUE)
        cmean[2] <- mean(permdat$RallyAllowR[permdat$Sample == z & permdat$RallyGroup %in% c(2,4)], na.rm = TRUE)
        te[2] <- tmean[2] - cmean[2]
        n[2] <- length(permdat$RallyAllowR[permdat$Sample == z & permdat$RallyGroup %in% 1:4])
        
        # DREAM Act
        p_dream <- replicate(perm, s1TE(trvar = permdat[permdat$Sample == z, "DREAMGroup"], 
                                        ovar = permdat[permdat$Sample == z, "DREAMSuppR"],
                                        tr = 5, ctrl = 4)) # reversed tr and ctrl, so effect is positive
        tmean[3] <- mean(permdat$DREAMSuppR[permdat$Sample == z & permdat$DREAMGroup == 5], na.rm = TRUE)
        cmean[3] <- mean(permdat$DREAMSuppR[permdat$Sample == z & permdat$DREAMGroup == 4], na.rm = TRUE)
        te[3] <- tmean[3] - cmean[3]
        n[3] <- length(permdat$DREAMSuppR[permdat$Sample == z & permdat$DREAMGroup %in% 4:5])
    }
    # combine and output
    out <- cbind(tmean, cmean, te, c(sd(p_loan), sd(p_rally), sd(p_dream)), n)
    colnames(out) <- c("Treatment", "Control", "Effect", "SE", "N")
    out
})
study1plot <- setNames(study1plot, c("Exit Poll", "Student", "Staff", "MTurk", "TESS"))

# plot combined results
png(filename = "../../CombinedPaper/ResultsOnePlot.png", height = 6, width = 7.5, units = "in", res = 300)
par(mar = c(5.1, 7.1, 1.1, 2.1))
plot(NA, xlim = c(-0.2, 0.5), ylim = c(0, 20), yaxt = "n", yaxs = "i",
     xlab = "Average Treatment Effect", ylab = "", main = "")
abline(v = 0, col = "gray")
axis(2, c(1:5, 8:12, 15:19), rep(c("Exit Poll", "Student", "Staff", "MTurk", "TESS"), 3), las = 2)
mtext(c("Student Loans", "Hate Rally", "DREAM Act"), side = 2, line = 5, at = c(17, 10, 3))
local({
    e <- unlist(lapply(study1plot, `[`, 1:3, "Effect"))
    se <- unlist(lapply(study1plot, `[`, 1:3, "SE"))
    n <- c(15,8,1, 16,9,2, 17,10,3, 18,11,4, 19,12,5)
    points(e, n, col = "black", bg = "black", pch = 23)
    segments(e - se, n, e + se, n, lwd = 2, col = "black")
    segments(e - 2 * se, n, e + 2 * se, n, lwd = 1, col = "black")
})
dev.off()

# Supplemental Results Document
local({
    rtf <- RTF("../../CombinedPaper/Study1SupplementalResults.doc")

    # Treatment Group Means
    tn <- c("Exit Poll", "Student", "Staff", "MTurk", "TESS")
    cn <- c("Treatment", "Control", "Effect", "SE", "N")

    addParagraph(rtf, "Study 1 Student Loans\n")
    a1 <- `colnames<-`(`rownames<-`(t(sapply(study1plot, function(x) sprintf("%0.2f", x[1, 1:5]))), tn), cn)
    addTable(rtf, a1, row.names = TRUE)

    addParagraph(rtf, "\n\nStudy 1 Hate Rally\n")
    a2 <- `colnames<-`(`rownames<-`(t(sapply(study1plot, function(x) sprintf("%0.2f", x[2, 1:5]))), tn), cn)
    addTable(rtf, a2, row.names = TRUE)

    addParagraph(rtf, "\n\nStudy 1 DREAM Act\n")
    a3 <- `colnames<-`(`rownames<-`(t(sapply(study1plot, function(x) sprintf("%0.2f", x[3, 1:5]))), tn), cn)
    addTable(rtf, a3, row.names = TRUE)

    a3b <- sapply(c(2:5,1), function(z) {
        pp <- replicate(5000, s1TE(permdat$DREAMGroup[permdat$Sample == z], permdat$RallyAllowR[permdat$Sample == z], tr = 5, ctrl = 1))
        ap <- replicate(5000, s1TE(permdat$DREAMGroup[permdat$Sample == z], permdat$RallyAllowR[permdat$Sample == z], tr = 5, ctrl = 2))
        pc <- replicate(5000, s1TE(permdat$DREAMGroup[permdat$Sample == z], permdat$RallyAllowR[permdat$Sample == z], tr = 5, ctrl = 3))
        ac <- replicate(5000, s1TE(permdat$DREAMGroup[permdat$Sample == z], permdat$RallyAllowR[permdat$Sample == z], tr = 5, ctrl = 4))
        c(
        # 1: Polarized Pro
        PolarizedPro = coefpaste(mean(permdat$DREAMSuppR[permdat$Sample == z & permdat$DREAMGroup == 5], na.rm = TRUE) -
                                   mean(permdat$DREAMSuppR[permdat$Sample == z & permdat$DREAMGroup == 1], na.rm = TRUE), 
                                 sd(pp)),
        # 2: Agreement Pro
        AgreementPro = coefpaste(mean(permdat$DREAMSuppR[permdat$Sample == z & permdat$DREAMGroup == 5], na.rm = TRUE) -
                                   mean(permdat$DREAMSuppR[permdat$Sample == z & permdat$DREAMGroup == 2], na.rm = TRUE), 
                                 sd(ap)),
        # 3: Polarized Con
        PolarizedCon = coefpaste(mean(permdat$DREAMSuppR[permdat$Sample == z & permdat$DREAMGroup == 5], na.rm = TRUE) -
                                   mean(permdat$DREAMSuppR[permdat$Sample == z & permdat$DREAMGroup == 3], na.rm = TRUE), 
                                 sd(pc)),
        # 4: Agreement Con
        AgreementCon = coefpaste(mean(permdat$DREAMSuppR[permdat$Sample == z & permdat$DREAMGroup == 5], na.rm = TRUE) -
                                   mean(permdat$DREAMSuppR[permdat$Sample == z & permdat$DREAMGroup == 4], na.rm = TRUE), 
                                 sd(ac))
        )
    })

    addParagraph(rtf, "\nStudy 1 DREAM Act Treatment Effects for All Conditions (Relative to Control)\n")
    a3b <- `colnames<-`(`rownames<-`(t(a3b), tn), rownames(a3b))
    addTable(rtf, a3b, row.names = TRUE)

    
    
    # Demographics

    # - gender (male/female)
    # - age 
    ## TESS is coded: (18-29/30-44/45-59/60+)
    ## Our surveys coded: 18:24 =2; 25:34=3; 35:50=4; 51:65=5; 65:89=6; else=NA
    agetmp <- numeric()
    agetmp[mydat$AgeR == 2] <- 1 # 18-24 range
    agetmp[mydat$AgeR %in% 3] <- 2 # 25-34 range
    agetmp[mydat$AgeR %in% 4] <- 3 # 35-50 range
    agetmp[agetmp == 5] <- 4 # 51-65 range
    agetmp[agetmp %in% c(6,7)] <- 5 # >65
    ### TESS
    agetmp[mydat$Sample == 1 & mydat$ppage %in% 18:24] <- 1
    agetmp[mydat$Sample == 1 & mydat$ppage %in% 25:34] <- 2
    agetmp[mydat$Sample == 1 & mydat$ppage %in% 35:50] <- 3
    agetmp[mydat$Sample == 1 & mydat$ppage %in% 51:65] <- 4
    agetmp[mydat$Sample == 1 & mydat$ppage > 65] <- 5
    ### Other samples
    # - race (white, non-hispanic / black non-hispanic / other, non-hispanic / 2+ races, non-hispanic / hispanic)
    racetmp <- mydat$ppethm
    racetmp[racetmp == 5] <- 3
    racetmp[racetmp %in% c(3,4)] <- 99
    racetmp[mydat$Sample != 1 & !is.na(mydat$Race)] <- 99
    racetmp[mydat$Race == "1"] <- 1
    racetmp[mydat$Race == "2"] <- 2
    racetmp[mydat$Race == "4"] <- 3

    # - education (< high school / high school / some college / bachelor + )
    # we didn't measure it

    d1 <- cbind(
        apply(100*prop.table(table(mydat$Sample, mydat$SexR), 1), 2, sprintf, fmt = "%0.2f")[,2], # sex
        apply(100*prop.table(table(mydat$Sample, agetmp), 1), 2, sprintf, fmt = "%0.2f"), # age
        apply(100*prop.table(table(mydat$Sample, racetmp), 1)[,1:3], 2, sprintf, fmt = "%0.2f"))# race
    colnames(d1) <- c("Female (%)", "18-24 (%)", "25-34 (%)", "35-50 (%)", "51-65 (%)", "65+ (%)", "White, Non-Hispanic (%)", "Black, Non-Hispanic (%)", "Hispanic (%)")
    rownames(d1) <- c("TESS","Exit Poll", "Student", "Staff", "MTurk", "Ads")

    addParagraph(rtf, "\n\nStudy 1 Demographics\n")
    addTable(rtf, d1[1:5,], row.names = TRUE)
    done(rtf)
})


## END ##
