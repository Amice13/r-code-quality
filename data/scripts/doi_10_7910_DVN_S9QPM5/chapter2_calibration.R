#~~~~~  Qualitative Comparative Analysis (QCA) Using R: A Beginner’s Guide     ~~~~~

#            Ioana-Elena Oana, Carsten Q. Schneider, Eva Thomann 


#~~~~~  Chapter 2 - Calibration and combining sets  ~~~~~


# Load packages and data:

library(SetMethods)
data("STUR")


#### 1. Crisp sets #### 

# Calibrating crisp set with ifelse():

STUR$GOOD1 <- ifelse(STUR$MARK <= 59, 0, 1)

# Calibrating crisp set with calibrate():

STUR$GOOD2 <- calibrate(STUR$MARK, 
                        type = "crisp", 
                        thresholds = c(60))

# Look up the calibrated set:

STUR$GOOD1

# Look up the raw values:

STUR$MARK

# Compare raw and calibrated scores:

STUR[c("MARK",  "GOOD1",  "GOOD2")]


#### 2. Fuzzy sets #### 


# Recoding method with a single raw variable:

STUR$GOOD3 <- NA #empty variable
STUR$GOOD3[STUR$MARK >=70] <-1
STUR$GOOD3[STUR$MARK <70 & STUR$MARK>=60]<-0.67
STUR$GOOD3[STUR$MARK <60 & STUR$MARK>=40]<-0.33
STUR$GOOD3[STUR$MARK <40]<-0
STUR$GOOD3

# Recoding method with several raw variables:

STUR$GOOD4 <- NA #empty variable
STUR$GOOD4[STUR$MARK >=70 & STUR$PARTICIPATION> 1 & STUR$PEERS > 1] <-1
STUR$GOOD4[STUR$MARK >=70 & (STUR$PARTICIPATION == 1 | STUR$PEERS == 1)] <-0.67
STUR$GOOD4[STUR$MARK <70 & STUR$MARK>=60]<-0.67
STUR$GOOD4[STUR$MARK <60 & STUR$MARK>=40]<-0.33
STUR$GOOD4[STUR$MARK <50 & STUR$MARK>=40 & STUR$PARTICIPATION == 1 & STUR$PEERS == 1]<-0
STUR$GOOD4[STUR$MARK <40]<-0
STUR$GOOD4

# Visualize with histogram - combine 2 graphs:

par(mfrow=c(1, 2)) # rule to put 2 graphs in 1 row
hist(STUR$GOOD3, 
     main = "GOOD3",
     xlab = "Set Membership")
hist (STUR$GOOD4, 
      main = "GOOD4",
      xlab = "Set Membership")
par(mfrow=c(1, 1)) # undo rule

# Recoding method with index of two raw variables:

# Create added index:

STUR$INCLASS <- STUR$PARTICIPATION + STUR$PEERS 
STUR$INCLASS

# Calibrate 5-value fuzzy set:

STUR$CLASS <- NA #empty variable
STUR$CLASS[STUR$INCLASS ==2] <-0
STUR$CLASS[STUR$INCLASS ==3] <-0.33
STUR$CLASS[STUR$INCLASS ==4] <-0.67
STUR$CLASS[STUR$INCLASS ==5] <-0.9
STUR$CLASS[STUR$INCLASS ==6] <-1
STUR$CLASS

# Direct method:

STUR$GOOD5 <- calibrate(STUR$MARK, 
                        type = "fuzzy", 
                        thresholds = c(39, 59.5, 70), 
                        logistic = TRUE)
STUR$GOOD5

# Visualize with plot:

plot(STUR$MARK, 
     STUR$GOOD5, 
     main = "Calibration of GOOD5",
     xlab = "Raw score",
     ylab = "Calibrated score")
abline(h = 0.5, 
       v = 59.5)

# Rounding:

STUR$GOOD5rounded <- round(STUR$GOOD5, digits = 2)
STUR$GOOD5rounded


#### 3. Saving calibrated data #### 


# Select relevant sets:

STUC <- STUR[, c("GOOD1", "GOOD3",  "GOOD4",  "CLASS",  "GOOD5")]

# Save:

write.csv(STUC, "STUC.csv")


#### 4. Calibration diagnostics #### 


# Skewness checks: 

skew.check(STUC$GOOD5)

skew.check(STUC$GOOD5, hist = TRUE)

skew.check(STUC)

skew.check(STUC[,c("GOOD5","CLASS")])

# Case names in and out of set:

rownames(subset(STUC, CLASS > 0.5))

# Cases on crossover point:

ambig.cases(STUC$GOOD5)

rownames(subset(STUC, GOOD5 == 0.5))


#### 5. Combining sets #### 


# Rename set GOOD5 into GOOD:

STUC$GOOD <- STUC$GOOD5

# Logical AND:

STUC$EXCELLENT <- fuzzyand(STUC$GOOD, STUC$CLASS)

# Logical OR:

STUC$TALENT <- fuzzyor(STUC$GOOD, STUC$CLASS)

# Logical NOT:

STUC$NOTALENT <- 1- STUC$TALENT

# Compare the set membership scores:

STUC[c("GOOD", "CLASS", "EXCELLENT", "TALENT", "NOTALENT")]


#### 6. Concept structures #### 


# Calibrate component sets GOOD, PART, PEER using STUR dataset:

data("STUR")

STUR$GOODFS <- calibrate(STUR$MARK, 
                            type = "fuzzy", 
                            thresholds = c(39,59.5,70), 
                            logistic = TRUE)

# Calibrate 3-value fuzzy sets:

STUR$PARTFS <- NA #empty variable
STUR$PARTFS[STUR$PARTICIPATION ==1] <-0
STUR$PARTFS[STUR$PARTICIPATION ==2] <-0.67
STUR$PARTFS[STUR$PARTICIPATION ==3] <-1

STUR$PEERFS <- NA #empty variable
STUR$PEERFS[STUR$PEERS ==1] <-0
STUR$PEERFS[STUR$PEERS ==2] <-0.67
STUR$PEERFS[STUR$PEERS ==3] <-1

# Save sets in new dataset:

PERFDT <- STUR[,c("GOODFS","PARTFS", "PEERFS")]

# Classic approach:

PERFDT$PERF1 <- compute("GOODFS*PARTFS*PEERFS", data=PERFDT)

# Substitutability:

PERFDT$PERF2 <- compute("GOODFS + PARTFS + PEERFS", data=PERFDT)

# Family resemblance:

PERFDT$PERF3 <- compute("GOODFS*PARTFS + GOODFS*PEERFS + PARTFS*PEERFS", data=PERFDT)

# Compare concept extension:

skew.check(PERFDT[,c("PERF1","PERF2", "PERF3")])


#### 7. Operations on complex sets #### 


# Negating complex sets:

negate(input = "GOOD*CLASS", 
       snames="GOOD, CLASS", 
       use.tilde=TRUE)

# Simplifying complex combined sets:

simplify(expression = "~GOOD*GOOD + ~GOOD*CLASS + 
         ~CLASS *GOOD + ~CLASS*CLASS", 
         snames = "GOOD, CLASS", 
         use.tilde = TRUE)

# Intersecting complex sets:

intersection("~GOOD + ~CLASS", "GOOD + CLASS", 
             snames="GOOD, CLASS")

# Factoring out:

factorize(input = "GOOD*PART + GOOD*PEER + PART*PEER",
          snames = "GOOD, PART, PEER")

