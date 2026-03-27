#~~~~~  Qualitative Comparative Analysis (QCA) Using R: A Beginner’s Guide     ~~~~~ 

#            Ioana-Elena Oana, Carsten Q. Schneider, Eva Thomann 


#~~~~~  Chapter 3 - Necessary Conditions ~~~~~ 



library(SetMethods)


# Building hypothetical example:


Name <- c("Amanda","Bob","Carl","Cecilia","Ana","Alex","Olivia")
HS <- c(0.7,0.6,0.9,0.45,1,0.92,0.8)
HM <- c(0.6,0.6,0.8,0.1,0.9,0.7,0.3)
HMHS <- data.frame(HS, HM)
row.names(HMHS) <- Name

xy.plot("HS", "HM", 
        data = HMHS, 
        main = "XY plot - necessity", 
        ylab = "High Motivation",
        xlab = "High Salary",
        necessity = TRUE,
        jitter=TRUE,
        fontsize = 6)

HMHS <- rbind(HMHS, c(0.6, 0.8), c(0.4, 0.6),  c(0.2, 0.4))
row.names(HMHS)[8:10] <- c("Mark","Dee", "Fiona")

xy.plot("HS", "HM", 
        data = HMHS, 
        main = "XY plot - necessity", 
        ylab = "High Motivation",
        xlab = "High Salary",
        necessity = TRUE,
        jitter=TRUE,
        fontsize = 6)


#### 1. Single necessary conditions: #### 


# Load data:

data("JOBF")

# Analysis of necessity for the presence of the outcome (Y):

# Obtain parameters of fit:

QCAfit(JOBF$HS, 
       JOBF$HM,
       cond.lab = "High Salary")

# Produce XY plot:

xy.plot(x = "HS",
        y = "HM",
        data = JOBF, 
        necessity = TRUE, 
        jitter = TRUE,
        xlab = "High salary",
        ylab = "High motivation",
        crisp = FALSE)

# Looking at the data as crisp:

xy.plot(x = "HS",
        y = "HM",
        data = JOBF, 
        necessity = TRUE, 
        jitter = TRUE,
        xlab = "High salary",
        ylab = "High motivation",
        crisp = TRUE)

# Obtain parameters of fit for negated condition:

QCAfit(1-JOBF$LJS, 
       JOBF$HM,
       cond.lab = "~ Low job security")

# Produce  XY plot for negated condition:

xy.plot(x = "~LJS",
        y = "HM",
        data = JOBF, 
        necessity = TRUE, 
        jitter = TRUE,
        xlab = "~LJS",
        ylab = "HM")

# Obtain parameters of fit for an entire dataframe:

QCAfit(x = JOBF[,c("HS", "FS", "LS", "LJS", "PP")], 
       y = JOBF$HM)

# Obtain parameters of fit for the negated outcome:

QCAfit(x = JOBF[,1:5], 
       y = 1-JOBF$HM)



#### 2. SUIN conditions: ####


# Detect SUIN conditions:

SS_Y <- superSubset(data = JOBF, 
                    outcome = "HM", 
                    conditions = c("HS", "FS", "LS", "LJS", "PP"), 
                    incl.cut = 0.9,
                    cov.cut=0.6,
                    ron.cut=0.6 )
SS_Y


# Produce XY plots for SUIN conditions:

pimplot(data = JOBF,
        results = SS_Y,
        outcome = "HM",
        necessity = TRUE,
        jitter = TRUE, 
        all_labels = TRUE)

# Add SUIN conditions to data set:

JOBF$FSorLS <- fuzzyor(JOBF$FS, JOBF$LS)

# Skewness check:

skew.check(JOBF$FSorLS, 
           hist = TRUE, 
           main = "FS or  LS")

