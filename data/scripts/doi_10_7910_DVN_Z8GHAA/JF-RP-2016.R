################################################
# Analysis for:
#
# Jordan, Soren and Grant Ferguson. 2016. "Extremism in Survey Measures of Ideology." 
#  Research & Politics 3 (3): DOI: 10.1177/2053168016669743
#
# Required files: NESPIL89.dta
#
################################################

rm(list=ls())
library(foreign)
library(scatterplot3d)
library(car)
library(faraway)
library(tseries)
library(systemfit)
library(sem)
library(Zelig)
library(AER)
library("dynlm") 
library(ggplot2)
library(ade4)
library(xtable)
library(mvabund)
library(lme4)
library(Matrix)
library(arm)
library(diptest)
library(MSBVAR)
library(lattice)
library(MASS)
library(effects)
library(splines)
library(nnet)
library(pscl)
library(spatcounts)
library(survival)
library(coda)
library(stats)
library(sandwich)
library(HH)

#### Table of Contents for what is done in this R file:
#### Part I: Self-Placement Question Type
### (1) Wave 1
## (a) OLS on dummy for very anchor
## (b) ANOVA on OLS results
## (c) Chi-squared on very/extremely
## (d) Bartlett's chi-squared on very/extremely
## (e) F-test to compare variances
## (f) Levene test
## (g) Brown-Forsythe test
### (2) Wave 2
## (a) OLS on dummy for very anchor
## (b) ANOVA on OLS results
## (c) Chi-squared on very/extremely
## (d) Bartlett's chi-squared on very/extremely
## (e) F-test to compare variances
## (f) Levene test
## (g) Brown-Forsythe test

#### Part II: Tests for Significant Differences Across Question Types (self-placement/branching)
####  (making sure we can pool together)
### (1) Pool across very/extremely anchors
## (a) Test Wave 1 (dummy for self-placement)
## (b) Test Wave 2 (dummy for self-placement)
### (2) Separate by anchor type 
## (a) Test Wave 1
# (i) Subset of very anchor (dummy for self-placement)
# (ii) Subset of extremely anchor (dummy for self-placement)
## (b) Test Wave 2
# (i) Subset of very anchor (dummy for self-placement)
# (ii) Subset of extremely anchor (dummy for self-placement)

#### Part III: Branching Question Type
### (1) Wave 1
## (a) OLS on dummy for very anchor
## (b) ANOVA on OLS results
## (c) Chi-squared on very/extremely
## (d) Bartlett's chi-squared on very/extremely
## (e) F-test to compare variances
## (f) Levene test
## (g) Brown-Forsythe test
### (2) Wave 2
## (a) OLS on dummy for very anchor
## (b) ANOVA on OLS results
## (c) Chi-squared on very/extremely
## (d) Bartlett's chi-squared on very/extremely
## (e) F-test to compare variances
## (f) Levene test
## (g) Brown-Forsythe test

#### Part IV: Pooling Self-Placement/Branching Question Types
### (1) Wave 1
## (a) OLS on dummy for very anchor
## (b) ANOVA on OLS results
## (c) Chi-squared on very/extremely
## (d) Bartlett's chi-squared on very/extremely
## (e) F-test to compare variances
### (2) Wave 2
## (a) OLS on dummy for very anchor
## (b) ANOVA on OLS results
## (c) Chi-squared on very/extremely
## (d) Bartlett's chi-squared on very/extremely
## (e) F-test to compare variances
### (3) Other tests
## (a) Wave 1
# (i) Levene test
# (ii) Brown-Forsythe test
## (b) Wave 2
# (i) Levene test
# (ii) Brown-Forsythe test

#### Part V: Histograms

#### Part VI: Relating ideology to preferences
### (1) Wave 1
## (a) Ideology
## (b) Affirmative action
## (c) Defense spending
## (d) Jobs and living standards
## (e) Government health insurance
## (f) Government services and spending
## (g) Abortion
### (2) Wave 2
## (a) Ideology
## (b) Affirmative action
## (c) Defense spending
## (d) Jobs and living standards
## (e) Government health insurance
## (f) Government services and spending
## (g) Abortion
### (3) Abramowitz and Saunders ideology scores (Wave 2)
### (4) Who is polarized?! Compare ideology scores to preferences (Wave 2)
### (5) Abramowitz and Saunders ideology scores (Wave 1)
### (6) Who is polarized?! Compare ideology scores to preferences (Wave 1)
### (7) Predicting ideology with preferences

#### Part VII: Code graveyard
# Old preference stuff
# Bootsrapping stuff
# Combining waves stuff
# Ordered logit stuff


##########################################################################
############################# BEGIN ANALYSIS ############################# 
##########################################################################
# The next chunk is just getting the data in. 
nes89 <- read.dta("C:/MyFiles/Conferences/Southern 2012/NESPIL89.dta")
labels(nes89)

names(nes89)
length(nes89[,1])
nes89$VVERSION
nes89$V897301

    # Reading in the 4 ideology variables:
        # There are also some branching ideology questions as well, which we should take a look at but that
        # I don't think Knight's 1990 report discusses at all (see pg. 47 and pg. 148 of the codebook).  These branching
        # questions seem to have the same structure as the branching partisan questions, which is something
        # we had talked about looking at and seeing how they compared and making ideology and partisanship
        # more comparable.
    
    # V897301 (uses "very" for endpoints, I think this is the 1st wave, pg. 47), 
    # V898401 (uses "very" for endpoints, I think this is the 2nd wave, pg. 147),
    # but there's also a V898401 that seems to use "extremely" for the endpoints on pg. 147 as well,
    # I might have to subset these by form.  Thus, they might be coded so that the first half of
    # the variables V897301 and V898401 are form A and the second half are form B, since there is
    # no a's and b's at the end of the variable names in R.
    # According to the documentation on pg. 8 of the codebook, my guess about this appears to be 
    # correct.
    # On pg. 27, the codebook notes that variable 7007, or (I think) variable V897007 in the R variable
    # names, measures which form of the survey the respondent received.  Pgs. 10-11 and 15 provide further
    # information on which parts of the ideology variables correspond to which survey form.

    # Thus, all you should need to do is subset the ideology variables properly by form or 
    # create new ideology variables based on form before you see if you can replicate Knight's
    # 1990 report results.

        # OK, below I code up a variable that measures the survey form that each respondent received.
        # The numbers seem to match up with what Knight talks about.

Survey.Form <- nes89$V897007
table(Survey.Form)

# Pilot says that pepole got the same form in both waves. True?
table(Survey.Form, nes89$V898007)
# Yes!

Form <- recode(Survey.Form, "'1' = 'A'; '2' = 'B'; '3' = 'C'; '4' = 'D'")
table(Form)

#### Part I: Self-Placement Question Type
    # Coding the Ideology questions:
        # Wave 1 1989:

Wave.1.Ideology <- rep(NA, length(nes89$V897301))
    Wave.1.Ideology[nes89$V897301 == 1] <- 1
    Wave.1.Ideology[nes89$V897301 == 2] <- 2
    Wave.1.Ideology[nes89$V897301 == 3] <- 3
    Wave.1.Ideology[nes89$V897301 == 4] <- 4
    Wave.1.Ideology[nes89$V897301 == 5] <- 5
    Wave.1.Ideology[nes89$V897301 == 6] <- 6
    Wave.1.Ideology[nes89$V897301 == 7] <- 7

table(Wave.1.Ideology)

            # Wave 1 1989, "Very" endpoints:
            
Wave.1.Ideo.Very <- ifelse(Form=="A", Wave.1.Ideology, NA)
table(Wave.1.Ideo.Very)

            # Wave 1 1989, "Extremely" endpoints:
            
Wave.1.Ideo.Extremely <- ifelse(Form=="B", Wave.1.Ideology, NA)
table(Wave.1.Ideo.Extremely)

        # Wave 2 1989:
        
Wave.2.Ideology <- rep(NA, length(nes89$V898401))
    Wave.2.Ideology[nes89$V898401 == 1] <- 1
    Wave.2.Ideology[nes89$V898401 == 2] <- 2
    Wave.2.Ideology[nes89$V898401 == 3] <- 3
    Wave.2.Ideology[nes89$V898401 == 4] <- 4
    Wave.2.Ideology[nes89$V898401 == 5] <- 5
    Wave.2.Ideology[nes89$V898401 == 6] <- 6
    Wave.2.Ideology[nes89$V898401 == 7] <- 7

table(Wave.2.Ideology)

            # Wave 2 1989, "Very" endpoints:
            
Wave.2.Ideo.Very <- ifelse(Form=="A", Wave.2.Ideology, NA)
table(Wave.2.Ideo.Very)

Wave.2.Ideo.Extremely <- ifelse(Form=="B", Wave.2.Ideology, NA)
table(Wave.2.Ideo.Extremely)

# Thus, I have 4 variables which measure ideology on a 7-point scale.
# 2 of them (Wave.1.Ideo.Very and Wave.2.Ideo.Very) measure it with
# the endpoints of the scale labeled "Very" and 2 of them (Wave.1.Ideo.Extremely
# and Wave.2.Ideo.Extremely) measure it with the endpoints of the scale
# labeled "Extremely."

# Now, I check that the tabled results of each variable is the same as it
# is depicted on pgs. 6-7 of Knight 1990:

    # On pg. 6 of Knight 1990:
        
        # Wave 1:
    
table(Wave.1.Ideo.Extremely)    # Matches.
mean(Wave.1.Ideo.Extremely, na.rm=TRUE) # Matches.
sd(Wave.1.Ideo.Extremely, na.rm=TRUE)   # Matches.
table(Wave.1.Ideo.Extremely!="NA")      # Matches

        # Wave 2:
        
table(Wave.2.Ideo.Extremely)    # Matches.
mean(Wave.2.Ideo.Extremely, na.rm=TRUE) # Matches.
sd(Wave.2.Ideo.Extremely, na.rm=TRUE)   # Matches.
table(Wave.2.Ideo.Extremely!="NA")      # Matches

    # On pg. 7 of Knight 1990:
    
        # Wave 1:
    
table(Wave.1.Ideo.Very)    # Matches.
mean(Wave.1.Ideo.Very, na.rm=TRUE) # Matches.
sd(Wave.1.Ideo.Very, na.rm=TRUE)   # Matches.
table(Wave.1.Ideo.Very!="NA")      # Matches

        # Wave 2:
        
table(Wave.2.Ideo.Very)    # Matches.
mean(Wave.2.Ideo.Very, na.rm=TRUE) # Matches.
sd(Wave.2.Ideo.Very, na.rm=TRUE)   # Matches.
table(Wave.2.Ideo.Very!="NA")      # Matches.


# OK, so I have coded Knight 1990's data correctly
# and replicated her tables on pgs. 6-7.
# Now I will proceed to replicating her anova and chi-square
# test results on pg. 8 (these were obviously done in Stata).


# First, I'll create some new data frames so that doing anova is easier:

W1.Ideo.Very <- as.matrix(na.omit(Wave.1.Ideo.Very))
W1.Ideo.Very

W1.Ideo.Extremely <- as.matrix(na.omit(Wave.1.Ideo.Extremely))
W1.Ideo.Extremely

W1.Data.Frame.A<-as.data.frame(cbind(W1.Ideo.Very,  as.matrix(rep(1, length(1:length(W1.Ideo.Very))))))
W1.Data.Frame.A

W1.Data.Frame.B<-as.data.frame(cbind(W1.Ideo.Extremely,  as.matrix(rep(0, length(1:length(W1.Ideo.Extremely))))))
W1.Data.Frame.B

W1.Data.Frame <- as.matrix(rbind(W1.Data.Frame.A, W1.Data.Frame.B))
W1.Data.Frame
colnames(W1.Data.Frame) <- c("Ideology", "Very.Dummy")
W1.Data.Frame

# OK, now I have a data frame that has the Wave 1 Ideology questions in both forms combined
# into 1 variable and a second dummy variable that differentiates them so that all observations
# with the "Very" endpoints are a 1 and all observations with the "Extremely" endpoints are a 0.
#  Now I can do the F-test and ANOVA.

## (a) OLS on dummy for very anchor
Wave1.OLS <- lm(W1.Data.Frame[,"Ideology"] ~ W1.Data.Frame[,"Very.Dummy"])
summary(Wave1.OLS)

    # The F-test for the Wave 1 Ideology question matches that on pg. 8 of Knight 1990.
    # The F statistic is ~ .35 and the p-value for it is .5573, which means we are unable to
    # reject the null hypothesis that the all the coefficients in the model are actually
    # equal to 0.  Since there is only 1 coefficient, which is the dummy for whether
    # the respondent received the Ideology question with the "Very" endpoints or 
    # "Extremely" endpoints, we can conclude that there is no statistically
    # significant difference between the 2 distributions of Ideology question
    # results.  Now for the ANOVA and chi-square test:

## (b) ANOVA on OLS results
Anova.W1 <- anova(Wave1.OLS)
Anova.W1             
        # These ANOVA results match what Knight 1990 has on pg. 8.

## (c) Chi-squared on very/extremely
        # I get a warning message from R with the results
        # that I get from this Chi-Square test, so I'm not
        # sure if these results are correct.  They should be,
        # but this is Pearson's Chi-Square Test, which is not 
        # what Knight 1990 uses.  She uses Bartlett's 
        # Chi-Square Test, so I need to go out and
        # find that.
   
            # Pearson's Chi-Square Test for these distributions:
    
Chi.Table.W1 <- table(W1.Ideo.Very, W1.Ideo.Extremely)

chisq.test(Chi.Table.W1)
            # The p-value of .2236 indicates that 
            # we CAN'T reject the null that 
            # the joint distribution of the cell counts in the
            # 2-dimensional contingency table is the product 
            # of the row and column marginals.
            # This means we cannot reject the null that 
            # the underlying distributions are equivalent.
            # This is the result that I want, of course, 
            # but Knight 1990 uses a different Chi-Square Test,
            # Bartlett's Chi-Square Test.

## (d) Bartlett's chi-squared on very/extremely
            # Bartlett's Chi-Square Test for these distributions,
            # the one Knight 1990 runs:
            
bartlett.test(W1.Ideo.Very, W1.Ideo.Extremely)  

            # These results, at first, do not appear to 
            # match Knight 1990's on pg. 9.  However, she
            # is reporting a p-value in a weird way (I think
            # she is reporting 1-p instead of p).  If you subtract
            # the p that she is reporting (.679) from 1, 
            # you get .321, which is roughly equal to .3107
            # that I get.  So maybe she is just reporting the
            # results of the test in a weird way.
            # Regardless, my results indicate that I 
            # CAN'T reject the null that the variances in 
            # each of the groups (samples) are the same.
            

     
## (e) F-test to compare variances       
var.test(W1.Ideo.Very, W1.Ideo.Extremely)   # This is an F-Test to compare the
                                            # the variances of 2 samples from
                                            # normal populations.
                                            # This gives the same p-value as in
                                            # Knight 1990's analysis on pg. 8, 
                                            # but is not the test she is supposedly
                                            # running.  These results indicate
                                            # that we cannot reject the null
                                            # that the 2 distributions have 
                                            # the same variance, but the populations
                                            # are assumed to be normally distributed,
                                            # which I don't know is a good assumption.
                                            # It seems like a normal distribution of 
                                            # ideology might be what you'd expect among the
                                            # public in 1989, but probably not in 2011.
                                            
# Apparently, at least according to the Wikipedia 
# article for Barlett's test, Bartlett's test
# for equal variances is somewhat sensitive to
# non-normal distributions, and there are other
# tests to use in this situation.  Also, Wikipedia (under F-Test
# of Equality of Variances) notes that
# the Bartlett's test is to some degree a generalization
# of the F-Test of equality of variances for 
# more than 2 populations.  In other words,
# doing the Bartlett's test for 2 groups should
# give very similar results as the F-Test of 
# equality of variances.
# However, apparently the F-test for equality
# of variances is VERY sensitive to the assumption
# that the underlying distribution that the populations
# are drawn from is normal.

## (f) Levene test
leveneTest(W1.Data.Frame[,"Ideology"], as.factor(W1.Data.Frame[,"Very.Dummy"]))

# F = 0.0246 with p = 0.87. No significant differences

## (g) Brown-Forsythe test
hov(W1.Data.Frame[,"Ideology"] ~ as.factor(W1.Data.Frame[,"Very.Dummy"]), method = "bf")

# Same as above
                                          
### Wave 2                                   
# OK, so I have basically replicated Knight 1990's results from Wave 1 of the 1989
# Pilot Study, even if she mislabeled one of the tests she was using or reported her
# results in a weird way.  Now I will replicate her results from Wave 2 in the same way.

# First, I'll create some new data frames so that doing anova is easier:

W2.Ideo.Very <- as.matrix(na.omit(Wave.2.Ideo.Very))
W2.Ideo.Very

W2.Ideo.Extremely <- as.matrix(na.omit(Wave.2.Ideo.Extremely))
W2.Ideo.Extremely

W2.Data.Frame.A<-as.data.frame(cbind(W2.Ideo.Very,  as.matrix(rep(1, length(1:length(W2.Ideo.Very))))))
W2.Data.Frame.A

W2.Data.Frame.B<-as.data.frame(cbind(W2.Ideo.Extremely,  as.matrix(rep(0, length(1:length(W2.Ideo.Extremely))))))
W2.Data.Frame.B


W2.Data.Frame <- as.matrix(rbind(W2.Data.Frame.A, W2.Data.Frame.B))
W2.Data.Frame
colnames(W2.Data.Frame) <- c("Ideology", "Very.Dummy")
W2.Data.Frame

# OK, now I have a data frame that has the Wave 2 Ideology questions in both forms combined
# into 1 variable and a second dummy variable that differentiates them so that all observations
# with the "Very" endpoints are a 1 and all observations with the "Extremely" endpoints are a 0.
#  Now I can do the F-test and ANOVA.

## (a) OLS on dummy for very anchor
Wave2.OLS <- lm(W2.Data.Frame[,"Ideology"] ~ W2.Data.Frame[,"Very.Dummy"])
summary(Wave2.OLS)

    # The F-test for the Wave 2 Ideology question matches that on pg. 8 of Knight 1990.
    # The F statistic is ~ .55 and the p-value for it is .4604, which means we are unable to
    # reject the null hypothesis that the all the coefficients in the model are actually
    # equal to 0.  Since there is only 1 coefficient, which is the dummy for whether
    # the respondent received the Ideology question with the "Very" endpoints or 
    # "Extremely" endpoints, we can conclude that there is no statistically
    # significant difference between the 2 distributions of Ideology question
    # results.  Now for the ANOVA and chi-square test:

## (b) ANOVA on OLS results
Anova.W2 <- anova(Wave2.OLS)
Anova.W2             
        # These ANOVA results match what Knight 1990 has on pg. 8.

        # I get a warning message from R with the results
        # that I get from this Chi-Square test, so I'm not
        # sure if these results are correct.  They should be,
        # but this is Pearson's Chi-Square Test, which is not 
        # what Knight 1990 uses.  She uses Bartlett's 
        # Chi-Square Test, so I need to go out and
        # find that.

## (c) Chi-squared on very/extremely    
            # I would do Pearson's Chi-Square Test for these distributions,
            # as used in the code below, but they have different lengths.
            # One has a length of 84, and one has a length of 90, which is
            # what they are supposed to have, so I can't run the rest.
   
#Chi.Table.W2 <- table(W2.Ideo.Very, W2.Ideo.Extremely)
#
#chisq.test(Chi.Table.W2)

## (d) Bartlett's chi-squared on very/extremely
            # Bartlett's Chi-Square Test for these distributions,
            # the one Knight 1990 runs:
            
#bartlett.test(W2.Ideo.Very, W2.Ideo.Extremely)  

            # This test also won't run because the lengths of each 
            # distribution are unequal.

## (e) F-test to compare variances
var.test(W2.Ideo.Very, W2.Ideo.Extremely, alternative="two.sided")  

# This is an F-Test to compare the  the variances of 2 samples from
# normal populations.
# The estimated ratio of the variances for these 2 populations 
# is ~ 1.45.
# This gives roughly same p-value (.08821 versus .089) as in
# Knight 1990's analysis on pg. 8, 
# but is not the test she is supposedly
# running.  

## (f) Levene test
leveneTest(W2.Data.Frame[,"Ideology"], as.factor(W2.Data.Frame[,"Very.Dummy"]))

# F = 0.0246 with p = 0.87. No significant differences

## (g) Brown-Forsythe test
hov(W2.Data.Frame[,"Ideology"] ~ as.factor(W2.Data.Frame[,"Very.Dummy"]), method = "bf")

# Same as above

# These results indicate
# that under a critical value of .05,
# we cannot reject the null
# that the 2 distributions have 
# the same variance, but the populations
# are assumed to be normally distributed,
# which I don't know is a good assumption.
# It seems like a normal distribution of 
# ideology might be what you'd expect among the
# public in 1989, but probably not in 2011.

# HOWEVER, under a critical value of .1, we CAN reject the null 
# in that the 2 distributions of responses from the ideology question in Wave 2 have 
# the same variance, since the p-value we get is .088.  Knight 1990 notes that 
# "all of these results suggest that the 'very' and 'extremely' labelling of the endpoints
# can be used interchangably," but that is NOT true.  

# If you take as a critical value .1 instead of .05, which is somewhat common
# in political science work today, the results from the F-Test of 
# equal variance for the 2 distributions from Wave 2 tell us that in fact, these underlying
# supposedly normal variances of the distribution from which these 2 populations
# are drawn from are NOT equal, and therefore the data-generating process that 
# these 2 populations are coming from are DIFFERENT!

# Knight 1990's argument is only true if you obsessively use a critical value 
# of .05. Furthermore, the sample sizes here are very small, so rejecting the
# null hypothesis under these conditions is more difficult than it might 
# be under a more typical population size for political science analysis.

# Therefore, I have more-or-less replicated the important part of 
# Knight's analysis for us.  Her results, when interpreted correctly,
# actually suggest that maybe these 2 different endpoints labels
# DO create different kinds of responses.

# Knight 1990's data could actually provide with some additional 
# insight.  I suggest doing the following:

# First, treat Wave 1 and Wave 2 as if they asked these questions
# to completely different people, not (mostly?) the same people.
# Since people's ideology can change over time, and there is response
# instability and measurement error, we could make some bad argument
# in favor of treating Wave 1 and Wave 2 of the 1989 study as 
# independent samples of people.  Now, we know that is obviously
# not true, and indeed Knight shows that, but if we did treat
# them as 2 independent samples, we could combine the ideology
# question results for Wave 1 and Wave 2 to get a larger N
# to perform ANOVA and F-Tests on.  The resulting tests could
# provide a large enough N to suggest further that the 2 
# question responses come from different underlying distributions.

# Second, we could run tests of equal variance of distributions
# that do not assume normality or work best under it like the 
# F-Test of Equal Variance or Bartlett's Test.  Wikipedia mentions
# several of these tests, and I'm willing to bet that they are 
# canned in R.  These tests would provide robustness checks for
# Knight 1990's findings and potentially reduce that .088 p-value
# for Wave 2 down to a more suggestive value below .05.

# Third, we could use non-parametric bootstrapping/simulation to increase the sample
# size of the 2 Ideology question results.  With a larger,
# more normal survey sample size, we could find significant
# differences, particularly with Wave 2.

# Fourth, we should DEFINITELY take a look at the branching form
# of the Ideology questions that was asked with both "Very"
# and "Extremely" endpoints for both waves.  Knight does not say
# anything about them, I think (check this), and they could yield
# interesting results.  Also, we wanted to look at branching 
# ideology question results as compared to branching partisanship
# question results for our own purposes of suggesting a better question
# form for the Ideology question.

#### Part II: Tests for Significant Differences Across Question Types (self-placement/branching)
# Let's pull the branching questions from the data as well.
# Just a reminder, in the Pilot survey, the form/endpoint combinations 
# are as follows:
#
# A: Placement (Simple 7-point) | Very 
# B: Placement (Simple 7-point) | Extremely
# C: Branching | Very
# D: Branching | Extremely
#
# Knight only deals with the first two. We can get much more information 
# on the differences between extremely/very if we can pool the responses
# from the placement and branching questions together. 
#
# Accordingly, let's make the branching variables, see if they differ 
# significantly from the placement variables, and go from there.

# Recall: 
  Form <- recode(Survey.Form, "'1' = 'A'; '2' = 'B'; '3' = 'C'; '4' = 'D'")
  Wave.1.Ideology <- rep(NA, length(nes89$V897301))
    Wave.1.Ideology[nes89$V897301 == 1] <- 1
    Wave.1.Ideology[nes89$V897301 == 2] <- 2
    Wave.1.Ideology[nes89$V897301 == 3] <- 3
    Wave.1.Ideology[nes89$V897301 == 4] <- 4
    Wave.1.Ideology[nes89$V897301 == 5] <- 5
    Wave.1.Ideology[nes89$V897301 == 6] <- 6
    Wave.1.Ideology[nes89$V897301 == 7] <- 7
        Wave.1.Ideo.Very <- ifelse(Form=="A", Wave.1.Ideology, NA)
        Wave.1.Ideo.Extremely <- ifelse(Form=="B", Wave.1.Ideology, NA)
  Wave.2.Ideology <- rep(NA, length(nes89$V898401))
    Wave.2.Ideology[nes89$V898401 == 1] <- 1
    Wave.2.Ideology[nes89$V898401 == 2] <- 2
    Wave.2.Ideology[nes89$V898401 == 3] <- 3
    Wave.2.Ideology[nes89$V898401 == 4] <- 4
    Wave.2.Ideology[nes89$V898401 == 5] <- 5
    Wave.2.Ideology[nes89$V898401 == 6] <- 6
    Wave.2.Ideology[nes89$V898401 == 7] <- 7
        Wave.2.Ideo.Very <- ifelse(Form=="A", Wave.2.Ideology, NA)
        Wave.2.Ideo.Extremely <- ifelse(Form=="B", Wave.2.Ideology, NA)

# So what we'll do is first name these four variables as .SP for ``self-placement"
Wave.1.Ideo.Very.SP <- Wave.1.Ideo.Very
Wave.1.Ideo.Extremely.SP <- Wave.1.Ideo.Extremely
Wave.2.Ideo.Very.SP <- Wave.2.Ideo.Very
Wave.2.Ideo.Extremely.SP <- Wave.2.Ideo.Extremely

# Now, let's deal with the branching questions! 
# We'll use the same strategy as before to code these variables.
# The branching responses are stored in two different variables, one for
# each wave. We'll add a .B to indicate these are branching responses. So . . .

Wave.1.Ideology.B <- rep(NA, length(nes89$V897307))
    Wave.1.Ideology.B[nes89$V897307 == 1] <- 1
    Wave.1.Ideology.B[nes89$V897307 == 2] <- 2
    Wave.1.Ideology.B[nes89$V897307 == 3] <- 3
    Wave.1.Ideology.B[nes89$V897307 == 4] <- 4
    Wave.1.Ideology.B[nes89$V897307 == 5] <- 5
    Wave.1.Ideology.B[nes89$V897307 == 6] <- 6
    Wave.1.Ideology.B[nes89$V897307 == 7] <- 7

table(Wave.1.Ideology.B)

Wave.2.Ideology.B <- rep(NA, length(nes89$V898407))
    Wave.2.Ideology.B[nes89$V898407 == 1] <- 1
    Wave.2.Ideology.B[nes89$V898407 == 2] <- 2
    Wave.2.Ideology.B[nes89$V898407 == 3] <- 3
    Wave.2.Ideology.B[nes89$V898407 == 4] <- 4
    Wave.2.Ideology.B[nes89$V898407 == 5] <- 5
    Wave.2.Ideology.B[nes89$V898407 == 6] <- 6
    Wave.2.Ideology.B[nes89$V898407 == 7] <- 7

table(Wave.2.Ideology.B)

# These make sense. A lot less ``true" moderates though.

# Now we need to recode some shizzle to make it identify if the anchors are very or extremely.
# Just need a few more ifelse statements.
# Remember: we need one variable per anchor type per wave. We already have four variables for
# self-placement from above. We'll end with eight variables, representing the 
# four form combinations above repeated across two waves.
# We'll add .B for ``branching" to dilineate them from the self-placement variables

Wave.1.Ideo.Very.B <- ifelse(Form=="C", Wave.1.Ideology.B, NA)
Wave.1.Ideo.Extremely.B <- ifelse(Form=="D", Wave.1.Ideology.B, NA)
Wave.2.Ideo.Very.B <- ifelse(Form=="C", Wave.2.Ideology.B, NA)
Wave.2.Ideo.Extremely.B <- ifelse(Form=="D", Wave.2.Ideology.B, NA)

# We now have our eight variables. Format:
# 		Wave.XX.Ideo.YYY.Z
# Where:
#   XX = 1 or 2 (for Wave 1 or Wave 2)
#   YYY = Very or Extremely
#   Z = B (for Branching) or SP (for self-placement)
#
# They are:
#        Wave.1.Ideo.Very.SP
#        Wave.1.Ideo.Extremely.SP
#        Wave.2.Ideo.Very.SP
#        Wave.2.Ideo.Extremely.SP
#        Wave.1.Ideo.Very.B
#        Wave.1.Ideo.Extremely.B
#        Wave.2.Ideo.Very.B
#        Wave.2.Ideo.Extremely.B



# Okay. We now need to test whether the branching and self-placement questions are significantly
# different from one another. Ideally, we would like to pool these data, but we can't
# if they come from different data-generating processes.

# Basically, I'm going to replicate what Knight/Grant do in their analyses, but my aim
# is to see if the branching questions are different from the placement questions,
# not to see if very differs from extremely (yet)

# First, let's look at the tables of each response.

table(Wave.1.Ideo.Very.B)
table(Wave.1.Ideo.Extremely.B)
table(Wave.1.Ideo.Very.SP)
table(Wave.1.Ideo.Extremely.SP)
table(Wave.2.Ideo.Very.B)
table(Wave.2.Ideo.Extremely.B)
table(Wave.2.Ideo.Very.SP)
table(Wave.2.Ideo.Extremely.SP)

# Just a little bit of curiousity: how stable is ideology across these two waves?
# Overall ideology
all.wave.1 <- cbind(Wave.1.Ideo.Very.B, Wave.1.Ideo.Extremely.B, Wave.1.Ideo.Very.SP, 
	Wave.1.Ideo.Extremely.SP)
Wave.1.Ideology.Together <- rowSums(all.wave.1, na.rm = T)
Wave.1.Ideology.Together[Wave.1.Ideology.Together == 0] <- NA

all.wave.2 <- cbind(Wave.2.Ideo.Very.B, Wave.2.Ideo.Extremely.B, Wave.2.Ideo.Very.SP, 
	Wave.2.Ideo.Extremely.SP)
Wave.2.Ideology.Together <- rowSums(all.wave.2, na.rm = T)
Wave.2.Ideology.Together[Wave.2.Ideology.Together == 0] <- NA

ideo.diff <- rep(NA, length(Wave.1.Ideology))
for(i in 1:length(ideo.diff)) {
	ideo.diff[i] <- Wave.2.Ideology.Together[i] - Wave.1.Ideology.Together[i]
	}
table(ideo.diff)

plot(jitter(Wave.1.Ideology.Together), jitter(Wave.2.Ideology))
# What's the difference of just respondents who are very cons/lib in wave 2?
table(subset(ideo.diff, Wave.2.Ideo.Very.SP == 1))
table(subset(ideo.diff, Wave.2.Ideo.Very.SP == 7))
table(subset(ideo.diff, Wave.2.Ideo.Very.B == 1))
table(subset(ideo.diff, Wave.2.Ideo.Very.B == 7))

### (1) Pool across very/extremely anchors
## (a) Test Wave 1 (dummy for self-placement)
# Let's do it for wave 1 first.
# First, I'll create some new data frames so that doing anova is easier:

# A variable for the wave 1 self-placement responses together (combining very/extremely)
W1.Ideo.SP.temp <- as.matrix(c(na.omit(Wave.1.Ideo.Very.SP), na.omit(Wave.1.Ideo.Extremely.SP)))
W1.Ideo.SP <- as.data.frame(cbind(W1.Ideo.SP.temp, as.matrix(rep(1, length(1:length(W1.Ideo.SP.temp))))))

# Now a variable for the wave 1 branching responses together (combining very/extremely)
W1.Ideo.B.temp <- as.matrix(c(na.omit(Wave.1.Ideo.Very.B), na.omit(Wave.1.Ideo.Extremely.B)))
W1.Ideo.B <- as.data.frame(cbind(W1.Ideo.B.temp, as.matrix(rep(0, length(1:length(W1.Ideo.B.temp))))))

W1.combined.Data.Frame <- as.matrix(rbind(W1.Ideo.SP, W1.Ideo.B))
W1.combined.Data.Frame
colnames(W1.combined.Data.Frame) <- c("Ideology", "SP.Dummy")
W1.combined.Data.Frame

# OK, now I have a data frame that has the Wave 1 Ideology questions in both forms combined
# into 1 variable and a second dummy variable that differentiates them so that all observations
# with the "self-placement" structure are a 1 and all observations with the "branching" structure are a 0.
#  Now I can do the F-test and ANOVA.

Wave1.OLS <- lm(W1.combined.Data.Frame[,"Ideology"] ~ W1.combined.Data.Frame[,"SP.Dummy"])
summary(Wave1.OLS)

    # The F statistic is ~ 1.99 and the p-value for it is .1594, which means we are unable to
    # reject the null hypothesis that the all the coefficients in the model are actually
    # equal to 0.  Since there is only 1 coefficient, which is the dummy for whether
    # the respondent received the Ideology question with the "self-placement" structure or 
    # "branching" structure, we can conclude that there is no statistically
    # significant difference between the 2 distributions of Ideology question
    # results.

## (b) Test Wave 2 (dummy for self-placement)
# Now, wave 2.
# First, I'll create some new data frames so that doing anova is easier:

# A variable for the wave 2 self-placement responses together (combining very/extremely)
W2.Ideo.SP.temp <- as.matrix(c(na.omit(Wave.2.Ideo.Very.SP), na.omit(Wave.2.Ideo.Extremely.SP)))
W2.Ideo.SP <- as.data.frame(cbind(W2.Ideo.SP.temp, as.matrix(rep(1, length(1:length(W2.Ideo.SP.temp))))))

# Now a variable for the wave 2 branching responses together (combining very/extremely)
W2.Ideo.B.temp <- as.matrix(c(na.omit(Wave.2.Ideo.Very.B), na.omit(Wave.2.Ideo.Extremely.B)))
W2.Ideo.B <- as.data.frame(cbind(W2.Ideo.B.temp, as.matrix(rep(0, length(1:length(W2.Ideo.B.temp))))))

W2.combined.Data.Frame <- as.matrix(rbind(W2.Ideo.SP, W2.Ideo.B))
W2.combined.Data.Frame
colnames(W2.combined.Data.Frame) <- c("Ideology", "SP.Dummy")
W2.combined.Data.Frame

# OK, now I have a data frame that has the Wave 2 Ideology questions in both forms combined
# into 1 variable and a second dummy variable that differentiates them so that all observations
# with the "self-placement" structure are a 1 and all observations with the "branching" structure are a 0.
#  Now I can do the F-test and ANOVA.

Wave2.OLS <- lm(W2.combined.Data.Frame[,"Ideology"] ~ W2.combined.Data.Frame[,"SP.Dummy"])
summary(Wave2.OLS)

    # The F statistic is ~ 0.85 and the p-value for it is .36, which means we are unable to
    # reject the null hypothesis that the all the coefficients in the model are actually
    # equal to 0.  Since there is only 1 coefficient, which is the dummy for whether
    # the respondent received the Ideology question with the "self-placement" structure or 
    # "branching" structure, we can conclude that there is no statistically
    # significant difference between the 2 distributions of Ideology question
    # results.

# That was for pooling responses together across very/extremely in each wave. Since the paper is about
# how these two questions are different substantively, this might be disinegunous to the data-generating
# process. So we repeat this process across the waves, dilineating by question type.

### (2) Separate by anchor type
## (a) Test Wave 1
# (i) Subset of very anchor (dummy for self-placement)
# A variable for the wave 1 self-placement and very responses
W1.Ideo.SP.very.temp <- as.matrix(c(na.omit(Wave.1.Ideo.Very.SP)))
W1.Ideo.very.SP <- as.data.frame(cbind(W1.Ideo.SP.very.temp, as.matrix(rep(1, length(1:length(W1.Ideo.SP.very.temp))))))

# Now a variable for the wave 1 branching and very responses
W1.Ideo.B.very.temp <- as.matrix(c(na.omit(Wave.1.Ideo.Very.B)))
W1.Ideo.very.B <- as.data.frame(cbind(W1.Ideo.B.very.temp, as.matrix(rep(0, length(1:length(W1.Ideo.B.very.temp))))))

W1.combined.very.Data.Frame <- as.matrix(rbind(W1.Ideo.very.SP, W1.Ideo.very.B))
W1.combined.very.Data.Frame
colnames(W1.combined.very.Data.Frame) <- c("Ideology", "SP.Dummy")
W1.combined.very.Data.Frame

# OK, now I have a data frame that has the Wave 1 Ideology questions for only very responses
# into 1 variable and a second dummy variable that differentiates them so that all observations
# with the "self-placement" structure are a 1 and all observations with the "branching" structure are a 0.
# Now I can do the F-test and ANOVA.

Wave1.OLS <- lm(W1.combined.very.Data.Frame[,"Ideology"] ~ W1.combined.very.Data.Frame[,"SP.Dummy"])
summary(Wave1.OLS)

    # The F statistic is ~ 1.26 and the p-value for it is 0.2627, which means we are unable to
    # reject the null hypothesis that the all the coefficients in the model are actually
    # equal to 0.  Since there is only 1 coefficient, which is the dummy for whether
    # the respondent received the Ideology question with the "self-placement" structure or 
    # "branching" structure, we can conclude that there is no statistically
    # significant difference between the 2 distributions of Ideology question
    # results.

# (ii) Subset of extremely anchor (dummy for self-placement)
# A variable for the wave 1 self-placement and extremely responses
W1.Ideo.SP.extremely.temp <- as.matrix(c(na.omit(Wave.1.Ideo.Extremely.SP)))
W1.Ideo.extremely.SP <- as.data.frame(cbind(W1.Ideo.SP.extremely.temp, as.matrix(rep(1, length(1:length(W1.Ideo.SP.extremely.temp))))))

# Now a variable for the wave 1 branching and extremely responses
W1.Ideo.B.extremely.temp <- as.matrix(c(na.omit(Wave.1.Ideo.Extremely.B)))
W1.Ideo.extremely.B <- as.data.frame(cbind(W1.Ideo.B.extremely.temp, as.matrix(rep(0, length(1:length(W1.Ideo.B.extremely.temp))))))

W1.combined.extremely.Data.Frame <- as.matrix(rbind(W1.Ideo.extremely.SP, W1.Ideo.extremely.B))
W1.combined.extremely.Data.Frame
colnames(W1.combined.extremely.Data.Frame) <- c("Ideology", "SP.Dummy")
W1.combined.extremely.Data.Frame

# OK, now I have a data frame that has the Wave 1 Ideology questions for only extremely responses
# into 1 variable and a second dummy variable that differentiates them so that all observations
# with the "self-placement" structure are a 1 and all observations with the "branching" structure are a 0.
# Now I can do the F-test and ANOVA.

Wave1.OLS <- lm(W1.combined.extremely.Data.Frame[,"Ideology"] ~ W1.combined.extremely.Data.Frame[,"SP.Dummy"])
summary(Wave1.OLS)

    # The F statistic is ~ 0.7941 and the p-value for it is 0.3738, which means we are unable to
    # reject the null hypothesis that the all the coefficients in the model are actually
    # equal to 0.  Since there is only 1 coefficient, which is the dummy for whether
    # the respondent received the Ideology question with the "self-placement" structure or 
    # "branching" structure, we can conclude that there is no statistically
    # significant difference between the 2 distributions of Ideology question
    # results.

## (b) Test Wave 2
# (i) Subset of very anchor (dummy for self-placement)
# A variable for the wave 2 self-placement and very responses
W2.Ideo.SP.very.temp <- as.matrix(c(na.omit(Wave.2.Ideo.Very.SP)))
W2.Ideo.very.SP <- as.data.frame(cbind(W2.Ideo.SP.very.temp, as.matrix(rep(1, length(1:length(W2.Ideo.SP.very.temp))))))

# Now a variable for the wave 1 branching and very responses
W2.Ideo.B.very.temp <- as.matrix(c(na.omit(Wave.2.Ideo.Very.B)))
W2.Ideo.very.B <- as.data.frame(cbind(W2.Ideo.B.very.temp, as.matrix(rep(0, length(1:length(W2.Ideo.B.very.temp))))))

W2.combined.very.Data.Frame <- as.matrix(rbind(W2.Ideo.very.SP, W2.Ideo.very.B))
W2.combined.very.Data.Frame
colnames(W2.combined.very.Data.Frame) <- c("Ideology", "SP.Dummy")
W2.combined.very.Data.Frame

# OK, now I have a data frame that has the Wave 2 Ideology questions for only very responses
# into 1 variable and a second dummy variable that differentiates them so that all observations
# with the "self-placement" structure are a 1 and all observations with the "branching" structure are a 0.
# Now I can do the F-test and ANOVA.

Wave2.OLS <- lm(W2.combined.very.Data.Frame[,"Ideology"] ~ W2.combined.very.Data.Frame[,"SP.Dummy"])
summary(Wave2.OLS)

    # The F statistic is ~ 0.07303 and the p-value for it is 0.7873, which means we are unable to
    # reject the null hypothesis that the all the coefficients in the model are actually
    # equal to 0.  Since there is only 1 coefficient, which is the dummy for whether
    # the respondent received the Ideology question with the "self-placement" structure or 
    # "branching" structure, we can conclude that there is no statistically
    # significant difference between the 2 distributions of Ideology question
    # results.

# (ii) Subset of extremely anchor (dummy for self-placement)
# A variable for the wave 2 self-placement and extremely responses
W2.Ideo.SP.extremely.temp <- as.matrix(c(na.omit(Wave.2.Ideo.Extremely.SP)))
W2.Ideo.extremely.SP <- as.data.frame(cbind(W2.Ideo.SP.extremely.temp, as.matrix(rep(1, length(1:length(W2.Ideo.SP.extremely.temp))))))

# Now a variable for the wave 1 branching and extremely responses
W2.Ideo.B.extremely.temp <- as.matrix(c(na.omit(Wave.2.Ideo.Extremely.B)))
W2.Ideo.extremely.B <- as.data.frame(cbind(W2.Ideo.B.extremely.temp, as.matrix(rep(0, length(1:length(W2.Ideo.B.extremely.temp))))))

W2.combined.extremely.Data.Frame <- as.matrix(rbind(W2.Ideo.extremely.SP, W2.Ideo.extremely.B))
W2.combined.extremely.Data.Frame
colnames(W2.combined.extremely.Data.Frame) <- c("Ideology", "SP.Dummy")
W2.combined.extremely.Data.Frame

# OK, now I have a data frame that has the Wave 2 Ideology questions for only extremely responses
# into 1 variable and a second dummy variable that differentiates them so that all observations
# with the "self-placement" structure are a 1 and all observations with the "branching" structure are a 0.
# Now I can do the F-test and ANOVA.

Wave2.OLS <- lm(W2.combined.extremely.Data.Frame[,"Ideology"] ~ W2.combined.extremely.Data.Frame[,"SP.Dummy"])
summary(Wave2.OLS)

    # The F statistic is ~ 2.861 and the p-value for it is 0.09231, which means we are unable to
    # reject the null hypothesis that the all the coefficients in the model are actually
    # equal to 0.  Since there is only 1 coefficient, which is the dummy for whether
    # the respondent received the Ideology question with the "self-placement" structure or 
    # "branching" structure, we can conclude that there is no statistically
    # significant difference between the 2 distributions of Ideology question
    # results.

#### Part III: Branching Question Type
### (1) Wave 1
## (a) OLS on dummy for very anchor

# First, I'll create some new data frames so that doing anova is easier:
W1.Ideo.Very.B <- as.matrix(na.omit(Wave.1.Ideo.Very.B))
W1.Ideo.Very.B

W1.Ideo.Extremely.B <- as.matrix(na.omit(Wave.1.Ideo.Extremely.B))
W1.Ideo.Extremely.B

W1.Data.Frame.A<-as.data.frame(cbind(W1.Ideo.Very.B,  as.matrix(rep(1, length(1:length(W1.Ideo.Very.B))))))
W1.Data.Frame.A

W1.Data.Frame.B<-as.data.frame(cbind(W1.Ideo.Extremely.B,  as.matrix(rep(0, length(1:length(W1.Ideo.Extremely.B))))))
W1.Data.Frame.B

W1.Data.Frame <- as.matrix(rbind(W1.Data.Frame.A, W1.Data.Frame.B))
W1.Data.Frame
colnames(W1.Data.Frame) <- c("Ideology", "Very.Dummy")
W1.Data.Frame

# OK, now I have a data frame that has the Wave 1 Ideology questions in both forms combined
# into 1 variable and a second dummymfr variable that differentiates them so that all observations
# with the "Very" endpoints are a 1 and all observations with the "Extremely" endpoints are a 0.
#  Now I can do the F-test and ANOVA.

Wave1.OLS <- lm(W1.Data.Frame[,"Ideology"] ~ W1.Data.Frame[,"Very.Dummy"])
summary(Wave1.OLS)
   
    # This is the F test for Wave 1 of the branching question.
    # The F statistic is ~ .83 and the p-value for it is .36, which means we are unable to
    # reject the null hypothesis that the all the coefficients in the model are actually
    # equal to 0.  Since there is only 1 coefficient, which is the dummy for whether
    # the respondent received the Ideology question with the "Very" endpoints or 
    # "Extremely" endpoints, we can conclude that there is no statistically
    # significant difference between the 2 distributions of Ideology question
    # results.  Now for the ANOVA and chi-square test:


## (b) ANOVA on OLS results
Anova.W1 <- anova(Wave1.OLS)
Anova.W1             
        # These ANOVA results match the above results
        # but this is Pearson's Chi-Square Test, which is not 
        # what Knight 1990 uses.  She uses Bartlett's 
        # Chi-Square Test, so I need to go out and
        # find that.
    
            # Pearson's Chi-Square Test for these distributions:
    
## (c) Chi-squared on very/extremely
# Chi.Table.W1 <- table(W1.Ideo.Very.B, W1.Ideo.Extremely.B)
# chisq.test(Chi.Table.W1)
            # With unequal variable lengths, we cannot perform this test.

            # Bartlett's Chi-Square Test for these distributions,
            # the one Knight 1990 runs:
            

## (d) Bartlett's chi-squared on very/extremely
# bartlett.test(W1.Ideo.Very.B, W1.Ideo.Extremely.B)  
           
	     # With unequal variable lengths, we cannot perform this test.
            
## (e) F-test to compare variances
            
var.test(W1.Ideo.Very.B, W1.Ideo.Extremely.B)   
	# This is an F-Test to compare the the variances of 2 samples from
	# normal populations. F = 1.11; p = 0.56.
	# These results indicate that we cannot reject the null that the 2 
	# distributions have the same variance, but the populations are assumed 
	# to be normally distributed, which I don't know is a good assumption.
	# It seems like a normal distribution of ideology might be what you'd 
	# expect among the public in 1989, but probably not in 2011.

## (f) Levene test
leveneTest(W1.Data.Frame[,"Ideology"], as.factor(W1.Data.Frame[,"Very.Dummy"]))

# F = 0.764 with p = 0.383. No significant differences

## (g) Brown-Forsythe test
hov(W1.Data.Frame[,"Ideology"] ~ as.factor(W1.Data.Frame[,"Very.Dummy"]), method = "bf")

# F = 0.764 with p = 0.383. No significant differences


### Wave 2
## (a) OLS on dummy for very anchor

# First, I'll create some new data frames so that doing anova is easier:
W2.Ideo.Very.B <- as.matrix(na.omit(Wave.2.Ideo.Very.B))
W2.Ideo.Very.B

W2.Ideo.Extremely.B <- as.matrix(na.omit(Wave.2.Ideo.Extremely.B))
W2.Ideo.Extremely.B

W2.Data.Frame.A<-as.data.frame(cbind(W2.Ideo.Very.B,  as.matrix(rep(1, length(1:length(W2.Ideo.Very.B))))))
W2.Data.Frame.A

W2.Data.Frame.B<-as.data.frame(cbind(W2.Ideo.Extremely.B,  as.matrix(rep(0, length(1:length(W2.Ideo.Extremely.B))))))
W2.Data.Frame.B

W2.Data.Frame <- as.matrix(rbind(W2.Data.Frame.A, W2.Data.Frame.B))
W2.Data.Frame
colnames(W2.Data.Frame) <- c("Ideology", "Very.Dummy")
W2.Data.Frame

# OK, now I have a data frame that has the Wave 2 Ideology questions in both forms combined
# into 1 variable and a second dummy variable that differentiates them so that all observations
# with the "Very" endpoints are a 1 and all observations with the "Extremely" endpoints are a 0.
#  Now I can do the F-test and ANOVA.

Wave2.OLS <- lm(W2.Data.Frame[,"Ideology"] ~ W2.Data.Frame[,"Very.Dummy"])
summary(Wave2.OLS)
   
    # This is the F test for Wave 2 of the branching question.
    # The F statistic is ~ 6.798 and the p-value for it is .009768, which means we are able to
    # reject the null hypothesis that the all the coefficients in the model are actually
    # equal to 0.  Since there is only 1 coefficient, which is the dummy for whether
    # the respondent received the Ideology question with the "Very" endpoints or 
    # "Extremely" endpoints, we can conclude that there is a statistically
    # significant difference between the 2 distributions of Ideology question
    # results. Hooray! Now for the ANOVA and chi-square test:


## (b) ANOVA on OLS results
    
Anova.W2 <- anova(Wave2.OLS)
Anova.W2             
        # These ANOVA results match the above results
        # but this is Pearson's Chi-Square Test, which is not 
        # what Knight 1990 uses.  She uses Bartlett's 
        # Chi-Square Test, so I need to go out and
        # find that.
    
            # Pearson's Chi-Square Test for these distributions:
    

## (c) Chi-squared on very/extremely
# Chi.Table.W2 <- table(W2.Ideo.Very.B, W2.Ideo.Extremely.B)
# chisq.test(Chi.Table.W2)
            # With unequal variable lengths, we cannot perform this test.

            # Bartlett's Chi-Square Test for these distributions,
            # the one Knight 1990 runs:
            

## (d) Bartlett's chi-squared on very/extremely
# bartlett.test(W2.Ideo.Very.B, W2.Ideo.Extremely.B)  
          
	     # With unequal variable lengths, we cannot perform this test.
            
## (e) F-test to compare variances
            
var.test(W2.Ideo.Very.B, W2.Ideo.Extremely.B)   
	# This is an F-Test to compare the the variances of 2 samples from
	# normal populations. F = 1.40; p = 0.08.
	# These results indicate that we can reject the null hypothesis
	# that the 2 distributions have the same variance, but the populations 
	# are assumed to be normally distributed, which I don't know is a good
	# assumption. This also requires a p value of 0.10, which is fine.
	# It seems like a normal distribution of ideology might be what you'd 
	# expect among the public in 1989, but probably not in 2011.

## (f) Levene test
leveneTest(W2.Data.Frame[,"Ideology"], as.factor(W2.Data.Frame[,"Very.Dummy"]))

        # The Levene test returns a p-value of .006, so I would reject the null I were
        # using a critical value of .1.  Thus, at a .1 level, I reject the null that 
        # the population variances are equal and conclude that the obtained differences in 
        # sample variances are unlikely to have occurred based on random sampling.
        
        # I now run 2 other related tests that are canned in the same function,
        # the robust Brown-Forsythe test that uses group medians instead
        # of means for the test, and the robust Levene test that uses the group
        # trimmed mean.  
        # 
        # I don't fully understand how the robust Levene test 
        # is different that the regular Levene test, except I that I think it
        # drops some observations.  
        # 
        # According to Wikipedia (Levene's test), the Brown-Forsythe performs better than the 
        # regular Levene test when the underlying data follow a Chi-Square
        # distribution with 4 degrees of freedom (which is a heavily skewed
        # distribution).  Again according to the same Wikipedia article,
        # the robust Levene test with the trimmed mean performs better than
        # the regular Levene test when the underlying distribution is a 
        # Cauchy distribution (a distribution with fat tails).
        # So here are the Brown-Forsythe test and 
        # the robust Levene test with the trimmed mean:    

## (g) Brown-Forsythe test
hov(W2.Data.Frame[,"Ideology"] ~ as.factor(W2.Data.Frame[,"Very.Dummy"]), method = "bf")

# It's weird that these Levene and Brown-Forsythe tests return the same values, but regardless,
# these equal variance tests also indicate that at a .01 level, I reject the null that 
# the population variances are equal and conclude that the obtained differences in 
# sample variances are unlikely to have occurred based on random sampling.
# These test further suggest that Knight was wrong to decisively conclude that there 
# was no difference between the 2 Ideology question result populations in 
# Knight 1990.


#### Part IV: Pooling Self-Placement/Branching Question Types
### (1) Wave 1
## (a) OLS on dummy for very anchor
# Now we need to create some new data frames that pool the two question types
# (branching and self-placement) together within each Wave. Recall the variables
# we have created
#        length(na.omit(Wave.1.Ideo.Very.SP))
#        length(na.omit(Wave.1.Ideo.Extremely.SP))
#        length(na.omit(Wave.2.Ideo.Very.SP))
#        length(na.omit(Wave.2.Ideo.Extremely.SP))
#        length(na.omit(Wave.1.Ideo.Very.B))
#        length(na.omit(Wave.1.Ideo.Extremely.B))
#        length(na.omit(Wave.2.Ideo.Very.B))
#        length(na.omit(Wave.2.Ideo.Extremely.B))

# First, I'll create some new data frames so that doing anova is easier:
W1.Ideo.Very.Total <- as.matrix(rbind(na.omit(as.matrix(Wave.1.Ideo.Very.B)), na.omit(as.matrix(Wave.1.Ideo.Very.SP))))
W1.Ideo.Very.Total

W1.Ideo.Extremely.Total <- as.matrix(rbind(na.omit(as.matrix(Wave.1.Ideo.Extremely.B)), na.omit(as.matrix(Wave.1.Ideo.Extremely.SP))))
W1.Ideo.Extremely.Total

W1.Data.Frame.A<-as.data.frame(cbind(W1.Ideo.Very.Total,  as.matrix(rep(1, length(1:length(W1.Ideo.Very.Total))))))
W1.Data.Frame.A

W1.Data.Frame.B<-as.data.frame(cbind(W1.Ideo.Extremely.Total,  as.matrix(rep(0, length(1:length(W1.Ideo.Extremely.Total))))))
W1.Data.Frame.B

W1.Data.Frame <- as.matrix(rbind(W1.Data.Frame.A, W1.Data.Frame.B))
W1.Data.Frame
colnames(W1.Data.Frame) <- c("Ideology", "Very.Dummy")
W1.Data.Frame

# OK, now I have a data frame that has the Wave 1 Ideology questions in both achors and forms combined
# into 1 variable and a second dummy variable that differentiates them so that all observations
# with the "Very" endpoints are a 1 and all observations with the "Extremely" endpoints are a 0.
#  Now I can do the F-test and ANOVA.

Wave1.OLS <- lm(W1.Data.Frame[,"Ideology"] ~ W1.Data.Frame[,"Very.Dummy"])
summary(Wave1.OLS)
   
    # This is the F test for Wave 1 of the branching question.
    # The F statistic is ~ 1.11 and the p-value for it is .29, which means we are unable to
    # reject the null hypothesis that the all the coefficients in the model are actually
    # equal to 0.  Since there is only 1 coefficient, which is the dummy for whether
    # the respondent received the Ideology question with the "Very" endpoints or 
    # "Extremely" endpoints, we can conclude that there is no statistically
    # significant difference between the 2 distributions of Ideology question
    # results.  Now for the ANOVA and chi-square test:

## (b) ANOVA on OLS results
    
Anova.W1 <- anova(Wave1.OLS)
Anova.W1             
        # These ANOVA results match the above results
        # but this is Pearson's Chi-Square Test, which is not 
        # what Knight 1990 uses.  She uses Bartlett's 
        # Chi-Square Test, so I need to go out and
        # find that.
    
            # Pearson's Chi-Square Test for these distributions:
    

## (c) Chi-squared on very/extremely

# Chi.Table.W1 <- table(W1.Ideo.Very.Total, W1.Ideo.Extremely.Total)
# chisq.test(Chi.Table.W1)
            # With unequal variable lengths, we cannot perform this test.

            # Bartlett's Chi-Square Test for these distributions,
            # the one Knight 1990 runs:
            

## (d) Bartlett's chi-squared on very/extremely

# bartlett.test(W1.Ideo.Very.Total, W1.Ideo.Extremely.Total)
           
	     # With unequal variable lengths, we cannot perform this test.
            
## (e) F-test to compare variances
            
var.test(W1.Ideo.Very.Total, W1.Ideo.Extremely.Total)
	# This is an F-Test to compare the the variances of 2 samples from
	# normal populations. F = 1.03; p = 0.84.
	# These results indicate that we cannot reject the null that the 2 
	# distributions have the same variance, but the populations are assumed 
	# to be normally distributed, which I don't know is a good assumption.
	# It seems like a normal distribution of ideology might be what you'd 
	# expect among the public in 1989, but probably not in 2011.

### (2) Wave 2
## (a) OLS on dummy for very anchor

# First, I'll create some new data frames so that doing anova is easier:
W2.Ideo.Very.Total <- as.matrix(rbind(na.omit(as.matrix(Wave.2.Ideo.Very.B)), na.omit(as.matrix(Wave.2.Ideo.Very.SP))))
W2.Ideo.Very.Total

W2.Ideo.Extremely.Total <- as.matrix(rbind(na.omit(as.matrix(Wave.2.Ideo.Extremely.B)), na.omit(as.matrix(Wave.2.Ideo.Extremely.SP))))
W2.Ideo.Extremely.Total

W2.Data.Frame.A<-as.data.frame(cbind(W2.Ideo.Very.Total,  as.matrix(rep(1, length(1:length(W2.Ideo.Very.Total))))))
W2.Data.Frame.A

W2.Data.Frame.B<-as.data.frame(cbind(W2.Ideo.Extremely.Total,  as.matrix(rep(0, length(1:length(W2.Ideo.Extremely.Total))))))
W2.Data.Frame.B

W2.Data.Frame <- as.matrix(rbind(W2.Data.Frame.A, W2.Data.Frame.B))
W2.Data.Frame
colnames(W2.Data.Frame) <- c("Ideology", "Very.Dummy")
W2.Data.Frame

# OK, now I have a data frame that has the Wave 2 Ideology questions in both forms combined
# into 1 variable and a second dummy variable that differentiates them so that all observations
# with the "Very" endpoints are a 1 and all observations with the "Extremely" endpoints are a 0.
#  Now I can do the F-test and ANOVA.

Wave2.OLS <- lm(W2.Data.Frame[,"Ideology"] ~ W2.Data.Frame[,"Very.Dummy"])
summary(Wave2.OLS)
   
    # This is the F test for Wave 2 of the branching question.
    # The F statistic is ~ 6.2 and the p-value for it is .0.01, which means we are able to
    # reject the null hypothesis that the all the coefficients in the model are actually
    # equal to 0.  Since there is only 1 coefficient, which is the dummy for whether
    # the respondent received the Ideology question with the "Very" endpoints or 
    # "Extremely" endpoints, we can conclude that there is a statistically
    # significant difference between the 2 distributions of Ideology question
    # results. Hooray! Now for the ANOVA and chi-square test:

## (b) ANOVA on OLS results
    
Anova.W2 <- anova(Wave2.OLS)
Anova.W2             
        # These ANOVA results match the above results
        # but this is Pearson's Chi-Square Test, which is not 
        # what Knight 1990 uses.  She uses Bartlett's 
        # Chi-Square Test, so I need to go out and
        # find that.
    
            # Pearson's Chi-Square Test for these distributions:
    
## (c) Chi-squared on very/extremely

# Chi.Table.W2 <- table(W2.Ideo.Very.Total, W2.Ideo.Extremely.Total)
# chisq.test(Chi.Table.W2)
            # With unequal variable lengths, we cannot perform this test.

            # Bartlett's Chi-Square Test for these distributions,
            # the one Knight 1990 runs:
            
## (d) Bartlett's chi-squared on very/extremely

# bartlett.test(W2.Ideo.Very.Total, W2.Ideo.Extremely.Total)
           
	     # With unequal variable lengths, we cannot perform this test.
            
## (e) F-test to compare variances 
var.test(W2.Ideo.Very.Total, W2.Ideo.Extremely.Total)
	# This is an F-Test to compare the the variances of 2 samples from
	# normal populations. F = 1.40; p = 0.08.
	# These results indicate that we can reject the null hypothesis
	# that the 2 distributions have the same variance, but the populations 
	# are assumed to be normally distributed, which I don't know is a good
	# assumption. This also requires a p value of 0.10, which is fine.
	# It seems like a normal distribution of ideology might be what you'd 
	# expect among the public in 1989, but probably not in 2011.

### (3) Other tests
## (a) Wave 1
# (i) Levene test

leveneTest(W1.Data.Frame[,"Ideology"], as.factor(W1.Data.Frame[,"Very.Dummy"]))

        # The Levene test returns a p-value of .54, so I would accept the null I were
        # using a critical value of .1.  Thus, at a .1 level, I accept the null that 
        # the population variances are equal and conclude that the obtained differences in 
        # sample variances are unlikely to have occurred based on random sampling in the pooled wave 1 test.
	 # There's some weird wave 1/2 differences here        

        # I now run 2 other related tests that are canned in the same function,
        # the robust Brown-Forsythe test that uses group medians instead
        # of means for the test, and the robust Levene test that uses the group
        # trimmed mean.  
        # 
        # I don't fully understand how the robust Levene test 
        # is different that the regular Levene test, except I that I think it
        # drops some observations.  
        # 
        # According to Wikipedia (Levene's test), the Brown-Forsythe performs better than the 
        # regular Levene test when the underlying data follow a Chi-Square
        # distribution with 4 degrees of freedom (which is a heavily skewed
        # distribution).  Again according to the same Wikipedia article,
        # the robust Levene test with the trimmed mean performs better than
        # the regular Levene test when the underlying distribution is a 
        # Cauchy distribution (a distribution with fat tails).
        # So here are the Brown-Forsythe test and 
        # the robust Levene test with the trimmed mean:
        

# (ii) Brown-Forsythe test
hov(W1.Data.Frame[,"Ideology"] ~ as.factor(W1.Data.Frame[,"Very.Dummy"]), method = "bf")

# It's weird that these Levene and Brown-Forsythe tests return the same values, but regardless,
# These equal variance tests returns a p-value of .54, so I would accept the null I were
        # using a critical value of .1.  Thus, at a .1 level, I accept the null that 
        # the population variances are equal and conclude that the obtained differences in 
        # sample variances are unlikely to have occurred based on random sampling in the pooled wave 1 test.
 

## (b) Wave 2
# (i) Levene test
leveneTest(W2.Data.Frame[,"Ideology"], as.factor(W2.Data.Frame[,"Very.Dummy"]))

        # The Levene test returns a p-value of .00002, so I would reject the null I were
        # using a critical value of .1.  Thus, at a .1 level, I reject the null that 
        # the population variances are equal and conclude that the obtained differences in 
        # sample variances are unlikely to have occurred based on random sampling in the pooled wave 2 test.
	 # There's some weird wave 1/2 differences here        

# (ii) Brown-Forsythe test
hov(W2.Data.Frame[,"Ideology"] ~ as.factor(W2.Data.Frame[,"Very.Dummy"]), method = "bf")

# It's weird that these Levene and Brown-Forsythe tests return the same values, but regardless,
        # The equal variance test returns a p-value of .00002, so I would reject the null I were
        # using a critical value of .1.  Thus, at a .1 level, I reject the null that 
        # the population variances are equal and conclude that the obtained differences in 
        # sample variances are unlikely to have occurred based on random sampling in the pooled wave 2 test.

#### Part V: Histograms (Must run all code above to create these variables)

# question types, by wave
par(mfrow = c(4,1), mar = c(2.5, 2.5, 2.5, 2.5))
barplot(table(W1.combined.extremely.Data.Frame[, "Ideology"]), names.arg = c("Extremely L", "Liberal", "Somewhat L", "Moderate", "Somewhat C", "Conservative", "Extremely C"), main = "Wave 1")
barplot(table(W1.combined.very.Data.Frame[, "Ideology"]), names.arg = c("Very L", "Liberal", "Somewhat L", "Moderate", "Somewhat C", "Conservative", "Very C"), main = "Wave 1")
barplot(table(W2.combined.extremely.Data.Frame[, "Ideology"]), names.arg = c("Extremely L", "Liberal", "Somewhat L", "Moderate", "Somewhat C", "Conservative", "Extremely C"), main = "Wave 2")
barplot(table(W2.combined.very.Data.Frame[, "Ideology"]), names.arg = c("Very L", "Liberal", "Somewhat L", "Moderate", "Somewhat C", "Conservative", "Very C"), main = "Wave 2")

# create combined, across waves
combined.extremely <- rbind(as.matrix(W1.combined.extremely.Data.Frame[, "Ideology"]), as.matrix(W2.combined.extremely.Data.Frame[, "Ideology"]))
combined.very <- rbind(as.matrix(W1.combined.very.Data.Frame[, "Ideology"]), as.matrix(W2.combined.very.Data.Frame[, "Ideology"]))

# combined scores, across waves
par(mfrow = c(2,1), mar = c(1,1,1,1))
barplot(table(combined.extremely))
barplot(table(combined.very))

# combined scores (across waves), folded
par(mfrow = c(2,1), mar = c(1,1,1,1))
barplot(table(abs(combined.extremely - 4)))
barplot(table(abs(combined.very - 4)))

# folded, by wave
par(mfrow = c(4,1), mar = c(2, 2, 2, 2))
barplot(table(abs(W1.combined.extremely.Data.Frame[, "Ideology"] - 4)))
barplot(table(abs(W1.combined.very.Data.Frame[, "Ideology"] - 4)))
barplot(table(abs(W2.combined.extremely.Data.Frame[, "Ideology"] - 4)))
barplot(table(abs(W2.combined.very.Data.Frame[, "Ideology"] - 4)))


#### Part VI: Relating ideology to preferences
# Some of our friends (and reviewers, not friends!) have wondered what this means
#  practically for political science. That is, who cares if one question type filters
#  individuals better?
# Or, alternatively, does the use of one question type over another lead to different
#  answers in the polarization debate?

# This analysis is meant to get at both of those questions. Specifically, we will
#  try to see what ``kinds'' of people (preference-wise) are binned into the poles
#  of each question. And we'll try and see what implication this might have for
#  polarization.

# Recall the data we have

# We now have our eight variables. Format:
# 		Wave.XX.Ideo.YYY.Z
# Where:
#   XX = 1 or 2 (for Wave 1 or Wave 2)
#   YYY = Very or Extremely
#   Z = B (for Branching) or SP (for self-placement)
#
# They are:
#        Wave.1.Ideo.Very.SP		# Form A
#        Wave.1.Ideo.Extremely.SP	# Form B
#        Wave.1.Ideo.Very.B		# Form C
#        Wave.1.Ideo.Extremely.B		# Form D
#
#        Wave.2.Ideo.Very.SP		# Form A
#        Wave.2.Ideo.Extremely.SP	# Form B
#        Wave.2.Ideo.Very.B		# Form C
#        Wave.2.Ideo.Extremely.B		# Form D

### Now let's get to some preference variables
# Abramowtiz and Saunders (2008) mention 7 questions in particular:
#	(1) Ideology (which we have already)
#	(2) Aid to blacks
#	(3) Defense spending
#	(4) Jobs and living standards
#	(5) Government health insurance
#	(6) Government services and spending
#	(7) Abortion

### (1) Wave 1 
## (a) We already have the ideology question

## (b) Affirmative action: ``Are you for or against employers favoring 
# 	blacks when thev decide who to hire and promote?
# 	1. FOR FAVORING BLACKS; STRONGLY
#	2. FOR FAVORING BLACKS; NOT STRONGLY
#	4. AGAINST FAVORING BLACKS: STRONGLY	# Note: the codebook says this,
#	5. AGAINST FAVORING BLACKS; NOT STRONGLY	# but tabbed against the other
#	8. DK							# responses, it is still correct
#	9. NA
#	0 . Inapt 0, 9 in 7308
# Note: 	FORM A/B got "advantages not earned" frame
#		C/D got "discriminates against whites" frame
# summary(lm(aff.action.wave.1 ~ as.factor(Form))) Shows no significant
#	coefficients by form, so we'll assume it's fine to pool.
aff.action.wave.1 <- nes89$V897311
aff.action.wave.1[aff.action.wave.1 == 0] <- NA
aff.action.wave.1[aff.action.wave.1 > 5] <- NA
aff.action.wave.1[aff.action.wave.1 == 4] <- 3
aff.action.wave.1[aff.action.wave.1 == 5] <- 4
table(aff.action.wave.1, nes89$V897311)

# Track liberal/conservative responses.
aff.action.wave.1.dummy <- aff.action.wave.1
aff.action.wave.1.dummy[aff.action.wave.1.dummy > 0] <- 0
aff.action.wave.1.cons <- aff.action.wave.1.lib <- aff.action.wave.1.dummy
aff.action.wave.1.lib[aff.action.wave.1 == 1] <- 1
aff.action.wave.1.cons[aff.action.wave.1 == 4] <- 1

## (c) Defense spending: ``Do you think the US should spend less on 
#	defense...''
#	1. A lot less spending on defense
#	...
#	7. A lot more money spending on defense
#	8. DK							
#	9. NA
#	0. Inapt 3-4 in 7007
# Note: 	FORM A got "self-placement"
#		B got "self-placement" and haven't thought much about this
#		A/B are in V897331
#		C got "branching"
#		D got "branching" and haven't thought much about this
#		C/D are in V897341
# summary(lm(defense.wave.1 ~ as.factor(Form))) Shows significant
#	coefficients for the branching questions, so we pool at our own risk.
defense.wave.1.ab <- nes89$V897331
defense.wave.1.ab[defense.wave.1.ab == 0] <- NA
defense.wave.1.ab[defense.wave.1.ab > 7] <- NA
table(defense.wave.1.ab, nes89$V897331)
defense.wave.1.cd <- nes89$V897341
defense.wave.1.cd[defense.wave.1.cd == 0] <- NA
defense.wave.1.cd[defense.wave.1.cd > 7] <- NA
table(defense.wave.1.cd, nes89$V897341)

defense.wave.1 <- rowSums(cbind(defense.wave.1.ab, defense.wave.1.cd), na.rm = T)
defense.wave.1[defense.wave.1 == 0] <- NA

# Track liberal/conservative responses. A/S style: anything not moderate = lib/cons
defense.wave.1.dummy <- defense.wave.1
defense.wave.1.dummy[defense.wave.1.dummy > 0] <- 0
defense.wave.1.cons <- defense.wave.1.lib <- defense.wave.1.dummy
defense.wave.1.lib[defense.wave.1 < 4] <- 1
defense.wave.1.cons[defense.wave.1 > 4] <- 1

## (d) Jobs and living standards: ``ONE, the government should try to ensure that all
#	Americans have such things as jobs, health care, and housing or, TWO, the
#	government should not be involved in this.''
#	1. GOVERNMENT ENSURE
#	3. BOTH, DEPENDS
#	5. GOVERNMENT NOT INVOLVED
#	8. DK
#	9. NA
# We recognize that this is a bad replacement for the jobs/living standards question,
#  but the ideal question isn't asked until Wave 2
# Note: 	No differences by form.
govt.jobs.living.wave.1 <- nes89$V897365
govt.jobs.living.wave.1[govt.jobs.living.wave.1 == 3] <- 2
govt.jobs.living.wave.1[govt.jobs.living.wave.1 == 5] <- 3
govt.jobs.living.wave.1[govt.jobs.living.wave.1 > 5] <- NA
table(nes89$V897365, govt.jobs.living.wave.1)

# Track liberal/conservative responses.
govt.jobs.living.wave.1.dummy <- govt.jobs.living.wave.1
govt.jobs.living.wave.1.dummy[govt.jobs.living.wave.1.dummy > 0] <- 0
govt.jobs.living.wave.1.cons <- govt.jobs.living.wave.1.lib <- govt.jobs.living.wave.1.dummy
govt.jobs.living.wave.1.lib[govt.jobs.living.wave.1 == 1] <- 1
govt.jobs.living.wave.1.cons[govt.jobs.living.wave.1 == 3] <- 1

## (e) Government health insurance. There is no clear analog in Wave 1. Oh no!!!

## (f) Government services and spending. There is no clear analog in Wave 1. Oh no!!!

## (g) Abortion: ``There has been some discussion about abortion during recent 
#	years. Which one of the opinions I am about to read you best agrees with 
#	your view on abortion?
# 	1. ONE: By law abortion should never be permitted.
#	2. TWO: The law should permit abortion only in the case of 
#		rape, incest or when the woman's life is in danger.
#	3. THREE: The law should permit abortion for reasons other 
#		than rape, incest, or danger to the woman's life, but 
#		only after the need for the abortionhas been clearly established.
#	4. FOUR: By law, a woman should always be able to obtain an abortion
#		as a matter of personal choice.
#	5. OTHER:
#	8. DK
#	9. NA
# We're going to flip these so that the higher response is more liberal like all
#  the other ones.
# Note: 	FORM A/C got no frame
#		B/D got "pro-choice" frame
# summary(lm(abortion.wave.1 ~ as.factor(Form))) Shows no significant
#	coefficients by form, so we'll assume it's fine to pool.
abortion.wave.1 <- nes89$V897401
abortion.wave.1[abortion.wave.1 > 4] <- NA
abortion.wave.1 <- (abortion.wave.1*(-1)) + 5
table(abortion.wave.1, nes89$V897401)

# Track liberal/conservative responses.
abortion.wave.1.dummy <- abortion.wave.1
abortion.wave.1.dummy[abortion.wave.1.dummy > 0] <- 0
abortion.wave.1.cons <- abortion.wave.1.lib <- abortion.wave.1.dummy
abortion.wave.1.lib[abortion.wave.1 == 1] <- 1
abortion.wave.1.cons[abortion.wave.1 == 4] <- 1

### (2) Wave 2
## (a) We already have the ideology question

## (b) Affirmative action: ``Are you for or against employers favoring 
# 	blacks when thev decide who to hire and promote?
# 	1. FOR FAVORING BLACKS; STRONGLY
#	2. FOR FAVORING BLACKS; NOT STRONGLY
#	4. OPPOSE FAVORING BLACKS: NOT STRONGLY		
#	5. OPPOSE FAVORING BLACKS; STRONGLY	
#	8. DK							
#	9. NA
#	0 . Inapt 0, 9 in 7308
# Note: 	FORM A/B got "discriminates against whites" frame
#		C/D got "advantages not earned" frame
# summary(lm(aff.action.wave.1 ~ as.factor(Form))) Shows no significant
#	coefficients by form, so we'll assume it's fine to pool.
aff.action.wave.2 <- nes89$V898411
aff.action.wave.2[aff.action.wave.2 == 0] <- NA
aff.action.wave.2[aff.action.wave.2 > 5] <- NA
aff.action.wave.2[aff.action.wave.2 == 4] <- 3
aff.action.wave.2[aff.action.wave.2 == 5] <- 4
table(aff.action.wave.2, nes89$V898411)

# Track liberal/conservative responses.
aff.action.wave.2.dummy <- aff.action.wave.2
aff.action.wave.2.dummy[aff.action.wave.2.dummy > 0] <- 0
aff.action.wave.2.cons <- aff.action.wave.2.lib <- aff.action.wave.2.dummy
aff.action.wave.2.lib[aff.action.wave.2 == 1] <- 1
aff.action.wave.2.cons[aff.action.wave.2 == 4] <- 1

## (c) Defense spending: ``Do you think the US should spend less on 
#	defense...''
#	1. A lot less spending on defense
#	...
#	7. A lot more money spending on defense
#	8. DK							
#	9. NA
#	0. Inapt 3-4 in 7007
# Note: 	FORM A got "self-placement"
#		B got "self-placement" and haven't thought much about this
#		A/B are in V898412
#		C got "branching"
#		D got "branching" and haven't thought much about this
#		C/D are in V898417
# summary(lm(defense.wave.1 ~ as.factor(Form))) Shows significant
#	coefficients for the branching questions, so we pool at our own risk.
defense.wave.2.ab <- nes89$V898412
defense.wave.2.ab[defense.wave.2.ab == 0] <- NA
defense.wave.2.ab[defense.wave.2.ab > 7] <- NA
table(defense.wave.2.ab, nes89$V898412)
defense.wave.2.cd <- nes89$V898417
defense.wave.2.cd[defense.wave.2.cd == 0] <- NA
defense.wave.2.cd[defense.wave.2.cd > 7] <- NA
table(defense.wave.2.cd, nes89$V898417)

defense.wave.2 <- rowSums(cbind(defense.wave.2.ab, defense.wave.2.cd), na.rm = T)
defense.wave.2[defense.wave.2 == 0] <- NA

# Track liberal/conservative responses. A/S style: anything not moderate = lib/cons
defense.wave.2.dummy <- defense.wave.2
defense.wave.2.dummy[defense.wave.2.dummy > 0] <- 0
defense.wave.2.cons <- defense.wave.2.lib <- defense.wave.2.dummy
defense.wave.2.lib[defense.wave.2 < 4] <- 1
defense.wave.2.cons[defense.wave.2 > 4] <- 1


## (d) Jobs and living standards: 
# Okay, so the cross-form experiments the ANES did mutilated response
#  comparability pretty badly here.
# FORM A/B got probably the ``true'' question. Branching format,
# ``Should the government see to it that every person has (A,B) a job and a 
#	good standard of living, should it let each person get ahead on their own, 
#	or is your position somewhere in between?''
#	1. GOVERNMENT SHOULD SEE TO IT/ STRONGLY
#	2. GOVERNMENT SHOULD SEE TO IT/ NOT STRONGLY
#	3. IN BETWEEN
#	4. LET EACH GET AHEAD ON THEIR OWN/ NOT STRONGLY
#	5. LET EACH GET AHEAD ON THEIR OWN/ STRONGLY
#	8. DK
#	9. NA
#	0. Inap, no Wave 2; 3,4 in 8007; 0,5,8-g in 8505; 8-9 in 8506
govt.jobs.living.wave.2.ab <- nes89$V898508
govt.jobs.living.wave.2.ab[govt.jobs.living.wave.2.ab == 0] <- NA
table(govt.jobs.living.wave.2.ab, nes89$V898508)

# P.S. The fun typos are brought to you courtesy of 1989 typewriter 
#  status and my laziness in fixing them. 
# FORM C/D got a self-placement question with more limited options.
# ``The government in Washington should see to it that every person has a 
#	job and a good standard of living. Do you approve strongly, approve 
#	somewhat, disapprove somewhat, or disanorove stronal?''
#	1. APPROVE STRONGLY
#	2. APPROVE SOMEWHAT
#	3. DISAPPROVE SOMEWHAT
#	4. DISAPPROVE STRONGLY
#	8. DK
#	9. NA
#	0. Inap; no Wave 2; l-2 in 8007
govt.jobs.living.wave.2.cd <- nes89$V898631
govt.jobs.living.wave.2.cd[govt.jobs.living.wave.2.cd == 0] <- NA
govt.jobs.living.wave.2.cd[govt.jobs.living.wave.2.cd > 5] <- NA
table(govt.jobs.living.wave.2.cd, nes89$V898631)

# Make two versions. One pooling the lib/con across form types, one keeping them
#  separate.
govt.jobs.living.wave.2.dummy.ab <- govt.jobs.living.wave.2.ab
govt.jobs.living.wave.2.dummy.cd <- govt.jobs.living.wave.2.cd
govt.jobs.living.wave.2.dummy.ab[govt.jobs.living.wave.2.dummy.ab > 0] <- 0
govt.jobs.living.wave.2.dummy.cd[govt.jobs.living.wave.2.dummy.cd > 0] <- 0
# Now the full dummy.
govt.jobs.living.wave.2.dummy <- rep(NA, length(govt.jobs.living.wave.2.dummy.ab))
for(i in 1:length(govt.jobs.living.wave.2.dummy)) {
	govt.jobs.living.wave.2.dummy[i] <- ifelse((govt.jobs.living.wave.2.dummy.ab[i] == 0 | 
		govt.jobs.living.wave.2.dummy.cd[i] == 0), 0, NA)
	}

govt.jobs.living.wave.2.cons <- govt.jobs.living.wave.2.lib <- govt.jobs.living.wave.2.dummy
govt.jobs.living.wave.2.cons.ab <- govt.jobs.living.wave.2.lib.ab <- govt.jobs.living.wave.2.dummy.ab
govt.jobs.living.wave.2.cons.cd <- govt.jobs.living.wave.2.lib.cd <- govt.jobs.living.wave.2.dummy.cd

# For the overall one, A/S would say that A/B forms, anything not a 3 is a
#  response. C/D, there is no middle.
govt.jobs.living.wave.2.cons[govt.jobs.living.wave.2.ab > 3] <- 1
govt.jobs.living.wave.2.cons[govt.jobs.living.wave.2.cd == 4] <- 1
govt.jobs.living.wave.2.lib[govt.jobs.living.wave.2.ab < 3] <- 1
govt.jobs.living.wave.2.lib[govt.jobs.living.wave.2.cd == 1] <- 1

# The separate form coding is just breaking those lines into new objects
#  We'll also recode the opposite forms to NA to remember that they're NA
govt.jobs.living.wave.2.cons.ab[govt.jobs.living.wave.2.ab > 3] <- 1
govt.jobs.living.wave.2.lib.ab[govt.jobs.living.wave.2.ab < 3] <- 1
govt.jobs.living.wave.2.cons.ab[Form == "C" | Form == "D"] <- NA
govt.jobs.living.wave.2.lib.ab[Form == "C" | Form == "D"] <- NA
govt.jobs.living.wave.2.cons.cd[govt.jobs.living.wave.2.cd == 4] <- 1
govt.jobs.living.wave.2.lib.cd[govt.jobs.living.wave.2.cd == 1] <- 1
govt.jobs.living.wave.2.cons.cd[Form == "A" | Form == "B"] <- NA
govt.jobs.living.wave.2.lib.cd[Form == "A" | Form == "B"] <- NA

## (e) Government health insurance:
# Again, so the cross-form experiments the ANES did mutilated response
#  comparability pretty badly here.
# FORM A/B got probably the ``true'' question. Branching format,
# ``Do you think that there should be a government insurance plan or that 
#	all medical expenses should be Daid bv individuals and Drivate insurance 
#	Plans?''
#	1. GOVT INSURANCE PLAN STRONGLY
#	...
#	5. EXPENSES BY INDIVIDUALS NOT STRONGLY
#	8. DK
#	9. NA
#	0. Inap I no Wave 2; 3-4 in 8007;0,5,8-9 in 8509; 8-9 in 8510
govt.insure.wave.2.ab <- nes89$V898512
govt.insure.wave.2.ab[govt.insure.wave.2.ab == 0] <- NA
govt.insure.wave.2.ab[govt.insure.wave.2.ab == 8] <- NA
# Recode to where there isn't a gap at 3
govt.insure.wave.2.ab[govt.insure.wave.2.ab == 4] <- 3
govt.insure.wave.2.ab[govt.insure.wave.2.ab == 5] <- 4
table(govt.insure.wave.2.ab, nes89$V898512)

# FORM C/D got a self-placement question with more limited options.
# ``There should be a government insurance plan which would cover all medical 
#	and hospital expenses for everyone.''
#	1. APPROVE STRONGLY
#	2. APPROVE SOMEWHAT
#	3. DISAPPROVE SOMEWHAT
#	4. DISAPPROVE STRONGLY
#	8. DK
#	9. NA
#	0. Inap; no Wave 2; l-2 in 8007
govt.insure.wave.2.cd <- nes89$V898633
govt.insure.wave.2.cd[govt.insure.wave.2.cd == 0] <- NA
govt.insure.wave.2.cd[govt.insure.wave.2.cd > 5] <- NA
table(govt.insure.wave.2.cd, nes89$V898633)

# Make two versions. One pooling the lib/con across form types, one keeping them
#  separate.
govt.insure.wave.2.dummy.ab <- govt.insure.wave.2.ab
govt.insure.wave.2.dummy.cd <- govt.insure.wave.2.cd
govt.insure.wave.2.dummy.ab[govt.insure.wave.2.dummy.ab > 0] <- 0
govt.insure.wave.2.dummy.cd[govt.insure.wave.2.dummy.cd > 0] <- 0
# Now the full dummy.
govt.insure.wave.2.dummy <- rep(NA, length(govt.insure.wave.2.dummy.ab))
for(i in 1:length(govt.insure.wave.2.dummy)) {
	govt.insure.wave.2.dummy[i] <- ifelse((govt.insure.wave.2.dummy.ab[i] == 0 | 
		govt.insure.wave.2.dummy.cd[i] == 0), 0, NA)
	}

govt.insure.wave.2.cons <- govt.insure.wave.2.lib <- govt.insure.wave.2.dummy
govt.insure.wave.2.cons.ab <- govt.insure.wave.2.lib.ab <- govt.insure.wave.2.dummy.ab
govt.insure.wave.2.cons.cd <- govt.insure.wave.2.lib.cd <- govt.insure.wave.2.dummy.cd

# For the overall one, this time only 1s and 4s are polarizing
govt.insure.wave.2.cons[govt.insure.wave.2.ab == 4] <- 1
govt.insure.wave.2.cons[govt.insure.wave.2.cd == 4] <- 1
govt.insure.wave.2.lib[govt.insure.wave.2.ab == 1] <- 1
govt.insure.wave.2.lib[govt.insure.wave.2.cd == 1] <- 1

# The separate form coding is just breaking those lines into new objects
#  We'll also recode the opposite forms to NA to remember that they're NA
govt.insure.wave.2.cons.ab[govt.insure.wave.2.ab == 4] <- 1
govt.insure.wave.2.lib.ab[govt.insure.wave.2.ab == 1] <- 1
govt.insure.wave.2.cons.ab[Form == "C" | Form == "D"] <- NA
govt.insure.wave.2.lib.ab[Form == "C" | Form == "D"] <- NA
govt.insure.wave.2.cons.cd[govt.insure.wave.2.cd == 4] <- 1
govt.insure.wave.2.lib.cd[govt.insure.wave.2.cd == 1] <- 1
govt.insure.wave.2.cons.cd[Form == "A" | Form == "B"] <- NA
govt.insure.wave.2.lib.cd[Form == "A" | Form == "B"] <- NA

## (f) Government services and spending:
# Again, so the cross-form experiments the ANES did mutilated response
#  comparability pretty badly here.
# FORM A/B got probably the ``true'' question. Branching format,
# ``Some people think the government should provide fewer services, 
#	even in areas such as health and education, in order to reduce 
#	spending. Other people feel it is important for the government 
#	to provide more services even if it means an increase in spending.
#	1. REDUCE SPENDING/ LARGE
#	...
#	5. PROVIDE MORE SERVICES/ LARGE
#	8. DK
#	9. NA
#	0. Inap I no Wave 2; 3-4 in 8007; 0,5, 8-9 in 8501; 8-9 in 8502
govt.serve.spend.wave.2.ab <- nes89$V898504
govt.serve.spend.wave.2.ab[govt.serve.spend.wave.2.ab == 0] <- NA
govt.serve.spend.wave.2.ab[govt.serve.spend.wave.2.ab == 8] <- NA
# This guy needs to be flipped.
govt.serve.spend.wave.2.ab <- govt.serve.spend.wave.2.ab*(-1) + 6
table(govt.serve.spend.wave.2.ab, nes89$V898504)

# FORM C/D got a self-placement question with more limited options.
# ``The federal government should provide fewer services even in areas 
#	such as health and education in order to reduce spending.''
#	1. APPROVE STRONGLY
#	2. APPROVE SOMEWHAT
#	3. DISAPPROVE SOMEWHAT
#	4. DISAPPROVE STRONGLY
#	8. DK
#	9. NA
#	0. Inap; no Wave 2; l-2 in 8007
govt.serve.spend.wave.2.cd <- nes89$V898635
govt.serve.spend.wave.2.cd[govt.serve.spend.wave.2.cd == 0] <- NA
govt.serve.spend.wave.2.cd[govt.serve.spend.wave.2.cd > 5] <- NA
# This guy has to be flipped, too. 
govt.serve.spend.wave.2.cd <- govt.serve.spend.wave.2.cd*(-1) + 5
table(govt.serve.spend.wave.2.cd, nes89$V898635)

# Make two versions. One pooling the lib/con across form types, one keeping them
#  separate.
govt.serve.spend.wave.2.dummy.ab <- govt.serve.spend.wave.2.ab
govt.serve.spend.wave.2.dummy.cd <- govt.serve.spend.wave.2.cd
govt.serve.spend.wave.2.dummy.ab[govt.serve.spend.wave.2.dummy.ab > 0] <- 0
govt.serve.spend.wave.2.dummy.cd[govt.serve.spend.wave.2.dummy.cd > 0] <- 0
# Now the full dummy.
govt.serve.spend.wave.2.dummy <- rep(NA, length(govt.serve.spend.wave.2.dummy.ab))
for(i in 1:length(govt.serve.spend.wave.2.dummy)) {
	govt.serve.spend.wave.2.dummy[i] <- ifelse((govt.serve.spend.wave.2.dummy.ab[i] == 0 | 
		govt.serve.spend.wave.2.dummy.cd[i] == 0), 0, NA)
	}

govt.serve.spend.wave.2.cons <- govt.serve.spend.wave.2.lib <- govt.serve.spend.wave.2.dummy
govt.serve.spend.wave.2.cons.ab <- govt.serve.spend.wave.2.lib.ab <- govt.serve.spend.wave.2.dummy.ab
govt.serve.spend.wave.2.cons.cd <- govt.serve.spend.wave.2.lib.cd <- govt.serve.spend.wave.2.dummy.cd

# For the overall one, A/S would say that A/B forms, anything not a 3 is a
#  response. C/D, there is no middle.
govt.serve.spend.wave.2.cons[govt.serve.spend.wave.2.ab > 3] <- 1
govt.serve.spend.wave.2.cons[govt.serve.spend.wave.2.cd == 4] <- 1
govt.serve.spend.wave.2.lib[govt.serve.spend.wave.2.ab < 3] <- 1
govt.serve.spend.wave.2.lib[govt.serve.spend.wave.2.cd == 1] <- 1

# The separate form coding is just breaking those lines into new objects
#  We'll also recode the opposite forms to NA to remember that they're NA
govt.serve.spend.wave.2.cons.ab[govt.serve.spend.wave.2.ab > 3] <- 1
govt.serve.spend.wave.2.lib.ab[govt.serve.spend.wave.2.ab < 3] <- 1
govt.serve.spend.wave.2.cons.ab[Form == "C" | Form == "D"] <- NA
govt.serve.spend.wave.2.lib.ab[Form == "C" | Form == "D"] <- NA
govt.serve.spend.wave.2.cons.cd[govt.serve.spend.wave.2.cd == 4] <- 1
govt.serve.spend.wave.2.lib.cd[govt.serve.spend.wave.2.cd == 1] <- 1
govt.serve.spend.wave.2.cons.cd[Form == "A" | Form == "B"] <- NA
govt.serve.spend.wave.2.lib.cd[Form == "A" | Form == "B"] <- NA

## (g) Abortion: ``There has been some discussion about abortion during 
#	recent years. Which one of the opinions I am about to read you 
#	best agrees with your view on abortion?''
# 	1. ONE: By law abortion should never be permitted.
#	2. TWO: The law should permit abortion only in the case of 
#		rape, incest or when the woman's life is in danger.
#	3. THREE: The law should permit abortion for reasons other 
#		than rape, incest, or danger to the woman's life, but 
#		only after the need for the abortionhas been clearly established.
#	4. FOUR: By law, a woman should always be able to obtain an abortion
#		as a matter of personal choice.
#	5. OTHER:
#	8. DK
#	9. NA
# We're going to flip these so that the higher response is more conservative like all
#  the other ones.
# Note: 	FORM A/C got no frame
#		B/D got "pro-choice" frame
# summary(lm(abortion.wave.2 ~ as.factor(Form))) Shows no significant
#	coefficients by form, so we'll assume it's fine to pool.
abortion.wave.2 <- nes89$V898534
abortion.wave.2[abortion.wave.2 > 4] <- NA
abortion.wave.2[abortion.wave.2 == 0] <- NA
abortion.wave.2 <- (abortion.wave.2*(-1)) + 5
table(abortion.wave.2, nes89$V898534)

# Track liberal/conservative responses.
abortion.wave.2.dummy <- abortion.wave.2
abortion.wave.2.dummy[abortion.wave.2.dummy > 0] <- 0
abortion.wave.2.cons <- abortion.wave.2.lib <- abortion.wave.2.dummy
abortion.wave.2.lib[abortion.wave.2 == 1] <- 1
abortion.wave.2.cons[abortion.wave.2 == 4] <- 1

### (3) Create Abramowitz/Saunders ideology scores (Wave 2)
# Try to create an ideology score
#
# We now have our eight variables. Format:
# 		Wave.XX.Ideo.YYY.Z
# Where:
#   XX = 1 or 2 (for Wave 1 or Wave 2)
#   YYY = Very or Extremely
#   Z = B (for Branching) or SP (for self-placement)
#
# A: Placement (Simple 7-point) | Very 
# B: Placement (Simple 7-point) | Extremely
# C: Branching | Very
# D: Branching | Extremely

# Because we have a small N problem, I'm gonna combine the veries and the
#  self-placements

Wave.2.Ideo.Very.combo <- rep(NA, length(Wave.2.Ideo.Very.B))
Wave.2.Ideo.Extremely.combo <- rep(NA, length(Wave.2.Ideo.Very.B))
Wave.2.Ideo.combo <- rep(NA, length(Wave.2.Ideo.Very.B))

for(i in 1:length(Wave.2.Ideo.Very.combo)) {
	Wave.2.Ideo.Very.combo[i] <- ifelse(Form[i] == "A", Wave.2.Ideo.Very.SP[i], 
		Wave.2.Ideo.Very.combo[i])
	Wave.2.Ideo.Very.combo[i] <- ifelse(Form[i] == "C", Wave.2.Ideo.Very.B[i], 
		Wave.2.Ideo.Very.combo[i])
	Wave.2.Ideo.Extremely.combo[i] <- ifelse(Form[i] == "B", Wave.2.Ideo.Extremely.SP[i], 
		Wave.2.Ideo.Extremely.combo[i])
	Wave.2.Ideo.Extremely.combo[i] <- ifelse(Form[i] == "D", Wave.2.Ideo.Extremely.B[i], 
		Wave.2.Ideo.Extremely.combo[i])
	Wave.2.Ideo.combo[i] <- ifelse(Form[i] == "A", Wave.2.Ideo.Very.SP[i], 
		Wave.2.Ideo.combo[i])
	Wave.2.Ideo.combo[i] <- ifelse(Form[i] == "C", Wave.2.Ideo.Very.B[i], 
		Wave.2.Ideo.combo[i])
	Wave.2.Ideo.combo[i] <- ifelse(Form[i] == "B", Wave.2.Ideo.Extremely.SP[i], 
		Wave.2.Ideo.combo[i])
	Wave.2.Ideo.combo[i] <- ifelse(Form[i] == "D", Wave.2.Ideo.Extremely.B[i], 
		Wave.2.Ideo.combo[i])
	}

table(Wave.2.Ideo.Very.SP, Wave.2.Ideo.Very.combo)
table(Wave.2.Ideo.Very.SP, Wave.2.Ideo.combo)
table(Wave.2.Ideo.Very.B, Wave.2.Ideo.Very.combo)
table(Wave.2.Ideo.Very.B, Wave.2.Ideo.combo)
table(Wave.2.Ideo.Extremely.SP, Wave.2.Ideo.Extremely.combo)
table(Wave.2.Ideo.Extremely.SP, Wave.2.Ideo.combo)
table(Wave.2.Ideo.Extremely.B, Wave.2.Ideo.Extremely.combo)
table(Wave.2.Ideo.Extremely.B, Wave.2.Ideo.combo)

# And, let's fold them
Wave.2.Ideo.Very.combo.fold <- abs(Wave.2.Ideo.Very.combo - 4)
Wave.2.Ideo.Extremely.combo.fold <- abs(Wave.2.Ideo.Extremely.combo - 4)
Wave.2.Ideo.combo.fold <- abs(Wave.2.Ideo.combo - 4)

table(Wave.2.Ideo.Very.combo, Wave.2.Ideo.Very.combo.fold)
table(Wave.2.Ideo.Extremely.combo, Wave.2.Ideo.Extremely.combo.fold)
table(Wave.2.Ideo.combo, Wave.2.Ideo.combo.fold)

# So these are the opinion pointers that we have
#	aff.action.wave.2.cons/lib
#	defense.wave.2.cons/lib
#	govt.jobs.living.wave.2.cons/lib [as well as .ab/.cd variations]
#	govt.insure.wave.2.cons/lib [as well as .ab/.cd variations]
#	govt.serve.spend.wave.2.cons/lib [as well as .ab/.cd variations]
#	abortion.wave.2.cons/lib

# Start with Wave 2. We're gonna build three different versions.
#  Overall. ab alone. cd alone.
# Liberal scores first
num.lib.wave.2.all <- rowSums(cbind(aff.action.wave.2.lib,
	defense.wave.2.lib, govt.jobs.living.wave.2.lib,
	govt.insure.wave.2.lib, govt.serve.spend.wave.2.lib,
	abortion.wave.2.lib))
	
num.lib.wave.2.ab <- rowSums(cbind(aff.action.wave.2.lib,
	defense.wave.2.lib, govt.jobs.living.wave.2.lib.ab,
	govt.insure.wave.2.lib.ab, govt.serve.spend.wave.2.lib.ab,
	abortion.wave.2.lib))

num.lib.wave.2.cd <- rowSums(cbind(aff.action.wave.2.lib,
	defense.wave.2.lib, govt.jobs.living.wave.2.lib.cd,
	govt.insure.wave.2.lib.cd, govt.serve.spend.wave.2.lib.cd,
	abortion.wave.2.lib))

# Now conservative scores
num.cons.wave.2.all <- rowSums(cbind(aff.action.wave.2.cons,
	defense.wave.2.cons, govt.jobs.living.wave.2.cons,
	govt.insure.wave.2.cons, govt.serve.spend.wave.2.cons,
	abortion.wave.2.cons))
	
num.cons.wave.2.ab <- rowSums(cbind(aff.action.wave.2.cons,
	defense.wave.2.cons, govt.jobs.living.wave.2.cons.ab,
	govt.insure.wave.2.cons.ab, govt.serve.spend.wave.2.cons.ab,
	abortion.wave.2.cons))

num.cons.wave.2.cd <- rowSums(cbind(aff.action.wave.2.cons,
	defense.wave.2.cons, govt.jobs.living.wave.2.cons.cd,
	govt.insure.wave.2.cons.cd, govt.serve.spend.wave.2.cons.cd,
	abortion.wave.2.cons))

polarized.wave.2 <- abs(num.lib.wave.2.all - num.cons.wave.2.all)
any.prefs.wave.2 <- num.lib.wave.2.all + num.cons.wave.2.all
polarized.wave.2.ab <- abs(num.lib.wave.2.ab - num.cons.wave.2.ab)
any.prefs.wave.2.ab <- num.lib.wave.2.ab + num.cons.wave.2.ab
polarized.wave.2.cd <- abs(num.lib.wave.2.cd - num.cons.wave.2.cd)
any.prefs.wave.2.cd <- num.lib.wave.2.cd + num.cons.wave.2.cd

### (4) Who is polarized?! Compare ideology scores to preferences (Wave 2)
# Table each of the polarization scores against the combinatoins
#  of very and extremely
# We're paying special care here because of the weirdness between
#  forms C/D versus A/B (the lack of preference dynamics, etc.
table(polarized.wave.2.ab, Wave.2.Ideo.Very.combo.fold)  # Basically just form A
table(polarized.wave.2.ab, Wave.2.Ideo.Extremely.combo.fold) # Form B
table(polarized.wave.2.cd, Wave.2.Ideo.Very.combo.fold) 	# Form C
table(polarized.wave.2.cd, Wave.2.Ideo.Extremely.combo.fold) # Form D

# Interestingly: the ``more'' respondents who are filtered into
#  the top ideology category on the ``Very'' anchor versus the
#  ``extremely'' one are all, well, not very extremely preferenced!
#  So we're getting fewer responses, but most political scientists
#  would probably argue that they're the ``true'' polarizers
# Also of note: C/D branching ideology basically makes it impossible
#  to register as a true moderate. So we observe no one on the 0 column
#  on ideology who registers on the polarization measure (they drop out
#  of at least one of the questions)

# Together, we observe the same thing
table(polarized.wave.2, Wave.2.Ideo.Very.combo.fold)
table(polarized.wave.2, Wave.2.Ideo.Extremely.combo.fold)

prop.table(as.matrix(table(polarized.wave.2.ab, Wave.2.Ideo.Very.combo.fold)))

plot(jitter(polarized.wave.2.ab), 
	jitter(Wave.2.Ideo.Very.combo.fold))
points(jitter(polarized.wave.2.ab), 
	jitter(Wave.2.Ideo.Extremely.combo.fold), pch = 2)
points(jitter(polarized.wave.2.cd), 
	jitter(Wave.2.Ideo.Very.combo.fold), pch = 3)
points(jitter(polarized.wave.2.cd), 
	jitter(Wave.2.Ideo.Extremely.combo.fold), pch = 4)

table(num.cons.wave.2.all, num.lib.wave.2.all)
# Are the more liberal or more conservative people the ones assigning
#  themselves the labels?
table(num.cons.wave.2.all, Wave.2.Ideo.Very.combo.fold)
table(num.lib.wave.2.all, Wave.2.Ideo.Very.combo.fold)
table(num.cons.wave.2.ab, Wave.2.Ideo.Very.combo.fold)
table(num.lib.wave.2.ab, Wave.2.Ideo.Very.combo.fold)
table(num.cons.wave.2.cd, Wave.2.Ideo.Very.combo.fold)
table(num.lib.wave.2.cd, Wave.2.Ideo.Very.combo.fold)

# Let's pull a few sets out
lib.con.ideo.very3.polr0.w2 <- cbind(subset(num.cons.wave.2.all, 
	polarized.wave.2 == 0 & Wave.2.Ideo.Very.combo.fold == 3),
	subset(num.lib.wave.2.all, 
	polarized.wave.2 == 0 & Wave.2.Ideo.Very.combo.fold == 3),
	subset(Wave.2.Ideo.Very.combo, 
	polarized.wave.2 == 0 & Wave.2.Ideo.Very.combo.fold == 3))
lib.con.ideo.very3.polr0.w2

lib.con.ideo.very3.polr1.w2 <- cbind(subset(num.cons.wave.2.all, 
	polarized.wave.2 == 1 & Wave.2.Ideo.Very.combo.fold == 3),
	subset(num.lib.wave.2.all, 
	polarized.wave.2 == 1 & Wave.2.Ideo.Very.combo.fold == 3),
	subset(Wave.2.Ideo.Very.combo, 
	polarized.wave.2 == 1 & Wave.2.Ideo.Very.combo.fold == 3))
lib.con.ideo.very3.polr1.w2

lib.con.ideo.very3.polr2.w2 <- cbind(subset(num.cons.wave.2.all, 
	polarized.wave.2 == 2 & Wave.2.Ideo.Very.combo.fold == 3),
	subset(num.lib.wave.2.all, 
	polarized.wave.2 == 2 & Wave.2.Ideo.Very.combo.fold == 3),
	subset(Wave.2.Ideo.Very.combo, 
	polarized.wave.2 == 2 & Wave.2.Ideo.Very.combo.fold == 3))
lib.con.ideo.very3.polr2.w2

who.pole.very.w2 <- rbind(cbind(lib.con.ideo.very3.polr0.w2, 0),
	cbind(lib.con.ideo.very3.polr1.w2, 1),
	cbind(lib.con.ideo.very3.polr2.w2, 2))
colnames(who.pole.very.w2) <- c("Num.Cons", "Num.Lib", "Ideo", "Polr")
who.pole.very

# Mostly conservative people! 

# Of the ``wrong'' extremely identifiers:
lib.con.ideo.ext3.polr0.w2 <- cbind(subset(num.cons.wave.2.all, 
	polarized.wave.2 == 0 & Wave.2.Ideo.Extremely.combo.fold == 3),
	subset(num.lib.wave.2.all, 
	polarized.wave.2 == 0 & Wave.2.Ideo.Extremely.combo.fold == 3),
	subset(Wave.2.Ideo.Extremely.combo, 
	polarized.wave.2 == 0 & Wave.2.Ideo.Extremely.combo.fold == 3))
lib.con.ideo.ext3.polr0.w2

lib.con.ideo.ext3.polr1.w2 <- cbind(subset(num.cons.wave.2.all, 
	polarized.wave.2 == 1 & Wave.2.Ideo.Extremely.combo.fold == 3),
	subset(num.lib.wave.2.all, 
	polarized.wave.2 == 1 & Wave.2.Ideo.Extremely.combo.fold == 3),
	subset(Wave.2.Ideo.Extremely.combo, 
	polarized.wave.2 == 1 & Wave.2.Ideo.Extremely.combo.fold == 3))
lib.con.ideo.ext3.polr1.w2

lib.con.ideo.ext3.polr2.w2 <- cbind(subset(num.cons.wave.2.all, 
	polarized.wave.2 == 2 & Wave.2.Ideo.Extremely.combo.fold == 3),
	subset(num.lib.wave.2.all, 
	polarized.wave.2 == 2 & Wave.2.Ideo.Extremely.combo.fold == 3),
	subset(Wave.2.Ideo.Extremely.combo, 
	polarized.wave.2 == 2 & Wave.2.Ideo.Extremely.combo.fold == 3))
lib.con.ideo.ext3.polr2.w2

who.pole.ext.w2 <- rbind(cbind(lib.con.ideo.ext3.polr0.w2, 0),
	cbind(lib.con.ideo.ext3.polr1.w2, 1),
	cbind(lib.con.ideo.ext3.polr2.w2, 2))
colnames(who.pole.ext.w2) <- c("Num.Cons", "Num.Lib", "Ideo", "Polr")
who.pole.ext.w2


plot(jitter(polarized.wave.2), jitter(Wave.2.Ideo.Very.combo.fold))
chisq.test(polarized.wave.2, Wave.2.Ideo.Very.combo.fold, correct = F)
cor(polarized.wave.2, Wave.2.Ideo.Very.combo.fold, use = "complete.obs")

plot(jitter(polarized.wave.2), jitter(Wave.2.Ideo.Extremely.combo.fold))
chisq.test(polarized.wave.2, Wave.2.Ideo.Extremely.combo.fold, correct = F)
cor(polarized.wave.2, Wave.2.Ideo.Extremely.combo.fold, use = "complete.obs")

# Only with Extremely is polarization related to ideology? Cray.

# Let's look at polarization with the individual form types
Wave.2.Ideo.Very.B.fold <- abs(Wave.2.Ideo.Very.B - 4)
Wave.2.Ideo.Very.SP.fold <- abs(Wave.2.Ideo.Very.SP - 4)
Wave.2.Ideo.Extremely.B.fold <- abs(Wave.2.Ideo.Extremely.B - 4)
Wave.2.Ideo.Extremely.SP.fold <- abs(Wave.2.Ideo.Extremely.SP - 4)

table(Wave.2.Ideo.Very.B.fold)	# 103
table(Wave.2.Ideo.Extremely.B.fold)	# 114
table(Wave.2.Ideo.Very.SP.fold)	# 84
table(Wave.2.Ideo.Extremely.SP.fold)# 90

table(polarized.wave.2, Wave.2.Ideo.Very.B.fold)	# C
table(polarized.wave.2, Wave.2.Ideo.Extremely.B.fold)	# D
table(polarized.wave.2, Wave.2.Ideo.Very.SP.fold)	# A
table(polarized.wave.2, Wave.2.Ideo.Extremely.SP.fold)# B
# These look pretty identical

plot(jitter(polarized.wave.2), jitter(Wave.2.Ideo.Very.SP.fold),
	col = "blue")
points(jitter(polarized.wave.2), jitter(Wave.2.Ideo.Very.B.fold),
	pch = 2)




### (5) Create Abramowitz/Saunders ideology scores (Wave 1)
# Same deal. Create the ideology scores first
Wave.1.Ideo.Very.combo <- rep(NA, length(Wave.1.Ideo.Very.B))
Wave.1.Ideo.Extremely.combo <- rep(NA, length(Wave.1.Ideo.Very.B))
Wave.1.Ideo.combo <- rep(NA, length(Wave.1.Ideo.Very.B))

for(i in 1:length(Wave.1.Ideo.Very.combo)) {
	Wave.1.Ideo.Very.combo[i] <- ifelse(Form[i] == "A", Wave.1.Ideo.Very.SP[i], 
		Wave.1.Ideo.Very.combo[i])
	Wave.1.Ideo.Very.combo[i] <- ifelse(Form[i] == "C", Wave.1.Ideo.Very.B[i], 
		Wave.1.Ideo.Very.combo[i])
	Wave.1.Ideo.Extremely.combo[i] <- ifelse(Form[i] == "B", Wave.1.Ideo.Extremely.SP[i], 
		Wave.1.Ideo.Extremely.combo[i])
	Wave.1.Ideo.Extremely.combo[i] <- ifelse(Form[i] == "D", Wave.1.Ideo.Extremely.B[i], 
		Wave.1.Ideo.Extremely.combo[i])
	Wave.1.Ideo.combo[i] <- ifelse(Form[i] == "A", Wave.1.Ideo.Very.SP[i], 
		Wave.1.Ideo.combo[i])
	Wave.1.Ideo.combo[i] <- ifelse(Form[i] == "C", Wave.1.Ideo.Very.B[i], 
		Wave.1.Ideo.combo[i])
	Wave.1.Ideo.combo[i] <- ifelse(Form[i] == "B", Wave.1.Ideo.Extremely.SP[i], 
		Wave.1.Ideo.combo[i])
	Wave.1.Ideo.combo[i] <- ifelse(Form[i] == "D", Wave.1.Ideo.Extremely.B[i], 
		Wave.1.Ideo.combo[i])
	}

table(Wave.1.Ideo.Very.SP, Wave.1.Ideo.Very.combo)
table(Wave.1.Ideo.Very.SP, Wave.1.Ideo.combo)
table(Wave.1.Ideo.Very.B, Wave.1.Ideo.Very.combo)
table(Wave.1.Ideo.Very.B, Wave.1.Ideo.combo)
table(Wave.1.Ideo.Extremely.SP, Wave.1.Ideo.Extremely.combo)
table(Wave.1.Ideo.Extremely.SP, Wave.1.Ideo.combo)
table(Wave.1.Ideo.Extremely.B, Wave.1.Ideo.Extremely.combo)
table(Wave.1.Ideo.Extremely.B, Wave.1.Ideo.combo)

# And, let's fold them
Wave.1.Ideo.Very.combo.fold <- abs(Wave.1.Ideo.Very.combo - 4)
Wave.1.Ideo.Extremely.combo.fold <- abs(Wave.1.Ideo.Extremely.combo - 4)
Wave.1.Ideo.combo.fold <- abs(Wave.1.Ideo.combo - 4)

table(Wave.1.Ideo.Very.combo, Wave.1.Ideo.Very.combo.fold)
table(Wave.1.Ideo.Extremely.combo, Wave.1.Ideo.Extremely.combo.fold)
table(Wave.1.Ideo.combo, Wave.1.Ideo.combo.fold)

# Now the polarization scores
# We're gonna build just one version (ignoring the forms on defense)
# Liberal scores first
num.lib.wave.1.all <- rowSums(cbind(aff.action.wave.1.lib,
	defense.wave.1.lib, govt.jobs.living.wave.1.lib,
	abortion.wave.1.lib))

# Now conservative scores
num.cons.wave.1.all <- rowSums(cbind(aff.action.wave.1.cons,
	defense.wave.1.cons, govt.jobs.living.wave.1.cons,
	abortion.wave.1.cons))

polarized.wave.1 <- abs(num.lib.wave.1.all - num.cons.wave.1.all)
any.prefs.wave.1 <- num.lib.wave.1.all + num.cons.wave.1.all

### (6) Who is polarized?! Compare ideology scores to preferences (Wave 1)
# Table each of the polarization scores against the combinatoins
#  of very and extremely
table(polarized.wave.1, Wave.1.Ideo.Very.combo.fold)  # Form A/C
table(polarized.wave.1, Wave.1.Ideo.Extremely.combo.fold) # Form B/D

# Interestingly: the ``more'' respondents who are filtered into
#  the top ideology category on the ``Very'' anchor versus the
#  ``extremely'' one are all, well, not very extremely preferenced!
# But this isn't nearly as bad as it was in Wave 2. 

# Let's pull a few sets out
lib.con.ideo.very3.polr0.w1 <- cbind(subset(num.cons.wave.1.all, 
	polarized.wave.1 == 0 & Wave.1.Ideo.Very.combo.fold == 3),
	subset(num.lib.wave.1.all, 
	polarized.wave.1 == 0 & Wave.1.Ideo.Very.combo.fold == 3),
	subset(Wave.1.Ideo.Very.combo, 
	polarized.wave.1 == 0 & Wave.1.Ideo.Very.combo.fold == 3))
lib.con.ideo.very3.polr0.w1

lib.con.ideo.very3.polr1.w1 <- cbind(subset(num.cons.wave.1.all, 
	polarized.wave.1 == 1 & Wave.1.Ideo.Very.combo.fold == 3),
	subset(num.lib.wave.1.all, 
	polarized.wave.1 == 1 & Wave.1.Ideo.Very.combo.fold == 3),
	subset(Wave.1.Ideo.Very.combo, 
	polarized.wave.1 == 1 & Wave.1.Ideo.Very.combo.fold == 3))
lib.con.ideo.very3.polr1.w1

lib.con.ideo.very3.polr2.w1 <- cbind(subset(num.cons.wave.1.all, 
	polarized.wave.1 == 2 & Wave.1.Ideo.Very.combo.fold == 3),
	subset(num.lib.wave.1.all, 
	polarized.wave.1 == 2 & Wave.1.Ideo.Very.combo.fold == 3),
	subset(Wave.1.Ideo.Very.combo, 
	polarized.wave.1 == 2 & Wave.1.Ideo.Very.combo.fold == 3))
lib.con.ideo.very3.polr2.w1

who.pole.very.w1 <- rbind(cbind(lib.con.ideo.very3.polr0.w1, 0),
	cbind(lib.con.ideo.very3.polr1.w1, 1),
	cbind(lib.con.ideo.very3.polr2.w1, 2))
colnames(who.pole.very.w1) <- c("Num.Cons", "Num.Lib", "Ideo", "Polr")
who.pole.very.w1

# Mostly conservative people! 

# Of the ``wrong'' extremely identifiers:
lib.con.ideo.ext3.polr0.w1 <- cbind(subset(num.cons.wave.1.all, 
	polarized.wave.1 == 0 & Wave.1.Ideo.Extremely.combo.fold == 3),
	subset(num.lib.wave.1.all, 
	polarized.wave.1 == 0 & Wave.1.Ideo.Extremely.combo.fold == 3),
	subset(Wave.1.Ideo.Extremely.combo, 
	polarized.wave.1 == 0 & Wave.1.Ideo.Extremely.combo.fold == 3))
lib.con.ideo.ext3.polr0.w1

lib.con.ideo.ext3.polr1.w1 <- cbind(subset(num.cons.wave.1.all, 
	polarized.wave.1 == 1 & Wave.1.Ideo.Extremely.combo.fold == 3),
	subset(num.lib.wave.1.all, 
	polarized.wave.1 == 1 & Wave.1.Ideo.Extremely.combo.fold == 3),
	subset(Wave.1.Ideo.Extremely.combo, 
	polarized.wave.1 == 1 & Wave.1.Ideo.Extremely.combo.fold == 3))
lib.con.ideo.ext3.polr1.w1

lib.con.ideo.ext3.polr2.w1 <- cbind(subset(num.cons.wave.1.all, 
	polarized.wave.1 == 2 & Wave.1.Ideo.Extremely.combo.fold == 3),
	subset(num.lib.wave.1.all, 
	polarized.wave.1 == 2 & Wave.1.Ideo.Extremely.combo.fold == 3),
	subset(Wave.1.Ideo.Extremely.combo, 
	polarized.wave.1 == 2 & Wave.1.Ideo.Extremely.combo.fold == 3))
lib.con.ideo.ext3.polr2.w1

who.pole.ext.w1 <- rbind(cbind(lib.con.ideo.ext3.polr0.w1, 0),
	cbind(lib.con.ideo.ext3.polr1.w1, 1),
	cbind(lib.con.ideo.ext3.polr2.w1, 2))
colnames(who.pole.ext.w1) <- c("Num.Cons", "Num.Lib", "Ideo", "Polr")
who.pole.ext.w1

# Okay, even more evidence here that we're dealing with extreme 
#  conservatives who may not be that extreme
# Remind ourselves: just how many people said they were extremely 
#  ideologically anyway?

table(Wave.1.Ideo.Extremely.combo) 	# 5 EL; 16 EC
table(Wave.1.Ideo.Very.combo) 	# 8 VL; 10 VC
table(Wave.2.Ideo.Extremely.combo)	# 3 EL; 10 EC
table(Wave.2.Ideo.Very.combo) 	# 10 VL; 15 VC

who.pole.ext.w1	# POLR score <2	# 1 EL; 10 EC
						# 1/5 = 20%; 10/16 = 63% 
who.pole.very.w1	# POLR score <2	# 3 VL; 8 VC
						# 3/8 = 38%; 8/10 = 80%
who.pole.ext.w2	# POLR score <2	# 0 EL; 5 EC
						# 0/3 = 0%; 5/10 = 50%
who.pole.very.w2	# POLR score <2	# 4 VL; 9 VC
						# 4/10 = 40%; 9/15 = 60%

# Finally, let's look at the relationship between ideology and
#  preferences
plot(jitter(polarized.wave.1), jitter(Wave.1.Ideo.Very.combo.fold))
chisq.test(polarized.wave.1, Wave.1.Ideo.Very.combo.fold, correct = F)
cor(polarized.wave.1, Wave.1.Ideo.Very.combo.fold, use = "complete.obs")

plot(jitter(polarized.wave.1), jitter(Wave.1.Ideo.Extremely.combo.fold))
chisq.test(polarized.wave.1, Wave.1.Ideo.Extremely.combo.fold, correct = F)
cor(polarized.wave.1, Wave.1.Ideo.Extremely.combo.fold, use = "complete.obs")

table(polarized.wave.1, polarized.wave.2)
cor(polarized.wave.1, polarized.wave.2, use = "complete.obs")
cor(Wave.1.Ideo.Very.combo.fold, 
	Wave.2.Ideo.Very.combo.fold, use = "complete.obs")
cor(Wave.1.Ideo.Extremely.combo.fold, 
	Wave.2.Ideo.Extremely.combo.fold, use = "complete.obs")

table(Wave.1.Ideo.Very.combo, Wave.2.Ideo.Very.combo)
table(Wave.1.Ideo.Extremely.combo, Wave.2.Ideo.Extremely.combo)

cor(Wave.1.Ideo.Very.combo, 
	Wave.2.Ideo.Very.combo, use = "complete.obs")
cor(Wave.1.Ideo.Extremely.combo, 
	Wave.2.Ideo.Extremely.combo, use = "complete.obs")

table(polarized.wave.2, Wave.1.Ideo.Very.combo.fold)
table(polarized.wave.2, Wave.1.Ideo.Extremely.combo.fold)



### (7) Predicting ideology with preferences
add.prefs.wave.1 <- rowSums(cbind(aff.action.wave.1,
	defense.wave.1, govt.jobs.living.wave.1,
	abortion.wave.1))

summary(lm(Wave.1.Ideo.Very.combo ~ add.prefs.wave.1))	#R2 .14
summary(lm(Wave.1.Ideo.Extremely.combo ~ add.prefs.wave.1))	#R2 .176

add.prefs.wave.2.ab <- rowSums(cbind(aff.action.wave.1,
	defense.wave.1, govt.jobs.living.wave.2,
	abortion.wave.1))

summary(lm(Wave.1.Ideo.Very.combo ~ add.prefs.wave.1))	#R2 .14
summary(lm(Wave.1.Ideo.Extremely.combo ~ add.prefs.wave.1))	#R2 .176

add.prefs.wave.2.ab <- rowSums(cbind(aff.action.wave.2,
	defense.wave.2, govt.jobs.living.wave.2.ab,
	govt.insure.wave.2.ab, govt.serve.spend.wave.2.ab,
	abortion.wave.2))

add.prefs.wave.2.cd <- rowSums(cbind(aff.action.wave.2,
	defense.wave.2, govt.jobs.living.wave.2.cd,
	govt.insure.wave.2.cd, govt.serve.spend.wave.2.cd,
	abortion.wave.2))

summary(lm(Wave.2.Ideo.Very.combo ~ add.prefs.wave.2.ab))	#R2 .169
summary(lm(Wave.2.Ideo.Extremely.combo ~ add.prefs.wave.2.ab))	#R2 .50
summary(lm(Wave.2.Ideo.Very.combo ~ add.prefs.wave.2.cd))	#R2 .122
summary(lm(Wave.2.Ideo.Extremely.combo ~ add.prefs.wave.2.cd))	#R2 .13
summary(lm(Wave.2.Ideo.Very.combo ~ add.prefs.wave.1))		#R2 .13
summary(lm(Wave.2.Ideo.Extremely.combo ~ add.prefs.wave.1))	#R2 .20



# To include:
# Cor/chisq of each wave to anchor
# 

# Polarization in Wave 1 versus ideo wave 2?

Wave.1.Ideo.Very.B.fold <- abs(Wave.1.Ideo.Very.B - 4)
Wave.1.Ideo.Very.SP.fold <- abs(Wave.1.Ideo.Very.SP - 4)
Wave.1.Ideo.Extremely.B.fold <- abs(Wave.1.Ideo.Extremely.B - 4)
Wave.1.Ideo.Extremely.SP.fold <- abs(Wave.1.Ideo.Extremely.SP - 4)

table(Wave.1.Ideo.Very.B.fold)
table(Wave.1.Ideo.Extremely.B.fold)
table(Wave.1.Ideo.Very.SP.fold)
table(Wave.1.Ideo.Extremely.SP.fold)

table(Wave.1.Ideo.Very.B, Wave.2.Ideo.Very.B)
table(Wave.1.Ideo.Extremely.B, Wave.2.Ideo.Extremely.B)
table(Wave.1.Ideo.Very.SP, Wave.2.Ideo.Very.SP)
table(Wave.1.Ideo.Extremely.SP, Wave.2.Ideo.Extremely.SP)
table(Wave.1.Ideo.Very.B.fold, Wave.2.Ideo.Very.B.fold)
table(Wave.1.Ideo.Extremely.B.fold, Wave.2.Ideo.Extremely.B.fold)
table(Wave.1.Ideo.Very.SP.fold, Wave.2.Ideo.Very.SP.fold)
table(Wave.1.Ideo.Extremely.SP.fold, Wave.2.Ideo.Extremely.SP.fold)




