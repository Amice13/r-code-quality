############################# APPENDIX: NYC IDEOLOGY ###########################

### WARNING: This example takes 8.2 hours to run 150,000 iterations with a optimized
### AWS EC2 T3 instance.

# FRONT MATTER
## Clean up
#rm(list=ls())

## Load packages
library(foreign)
library(krige)

## Load data
nyc <- NYcity_subset

## Data cleaning
nyc$cathOrth<-nyc$catholic+nyc$orthodox
nyc$consRelig<-nyc$mormon+nyc$evangelical
nyc$jewMus<-nyc$jewish+nyc$islam
nyc$black <- as.numeric(nyc$race==2)
nyc$nonBlack <- as.numeric(nyc$race==3)
nyc$unemployed <- as.numeric(nyc$empstat==2)
nyc$notWorkforce <- as.numeric(nyc$empstat==3)

nycData <- subset(nyc, select = c(ideology, age, educ, black, nonBlack, female,
                                  cathOrth, consRelig, jewMus, mainline, rural,
                                  ownership, unemployed, notWorkforce, inc14))

dimnames(nycData)[[2]] <- c("Ideology", "Age", "Education", "African.American", 
                            "Nonwhite.nonblack", "Female", "Catholic.Orthodox", 
                            "Evang.Mormon", "Jewish.Muslim", "Mainline", "Ruralism", 
                            "Homeowner", "Unemployed", "Not.in.workforce","Income")

# RUN THE SAMPLER
# Set Number of Iterations:
M<-150000
#M<-200 #Use fewer iterations for verification
set.seed(1,kind="Mersenne-Twister")

# Estimate the Model
nyc.fit<- metropolis.krige(formula = Ideology ~ Age + Education + I(Age*Education) + 
                             African.American + Nonwhite.nonblack + Female +
                             I(African.American*Female) + I(Nonwhite.nonblack*Female) +
                             Catholic.Orthodox + Evang.Mormon + Jewish.Muslim +
                             Mainline + Ruralism + Homeowner + Unemployed +
                             Not.in.workforce + Income, data = nycData,
                           coords = cbind(nyc$eastings, nyc$northings), 
                           powered.exp=2, n.iter=M, spatial.share=0.1, 
                           range.share=0.3, beta.var=1000, range.tol=0.01, 
                           b.tune=0.1, nugget.tune=20, psill.tune=1)

# SUMMARY AND POST PROCESSING
## Discard first 20% of Iterations as Burn-In (User Discretion Advised).
nyc.fit1 <- burnin(nyc.fit, M/5)

## Summarize Results
summary(nyc.fit1)

## Convergence Diagnostics: Geweke and Heidelberger-Welch
geweke(nyc.fit1)
heidel.welch(nyc.fit1)

## Draw Semivariogram
semivariogram(nyc.fit1)

# PREDICTION
## Predictive data for three prominent New Yorkers:
bill.deblasio<-c(58,5,0,0,0,0,0,0,0,0,1,0,0,14)
melania.trump<-c(49,2,0,0,1,1,0,0,0,0,1,0,0,14)
spike.lee<-c(63,5,1,0,0,0,0,0,1,0,1,0,0,14)
new.yorkers<-rbind(bill.deblasio,melania.trump,spike.lee)
colnames(new.yorkers) <- c("Age", "Education", "African.American", 
                           "Nonwhite.nonblack", "Female", "Catholic.Orthodox",
                           "Evang.Mormon", "Jewish.Muslim", "Mainline", "Ruralism",
                           "Homeowner", "Unemployed", "Not.in.workforce", "Income")
gracie.mansion<-c(1829.802,580.4355)
trump.tower<-c(1827.654,578.3515)
hatch.house<-c(1828.273,578.7542)
new.locations<-rbind(gracie.mansion,trump.tower,hatch.house)
colnames(new.locations)<-c("east","north")
ny.newdata <- as.data.frame(cbind(new.yorkers,new.locations))

## Make predictions with 90\% credible intervals:
set.seed(1,kind="Mersenne-Twister")
nyc.pred <- predict(nyc.fit1, newdata=ny.newdata, credible=0.9)
nyc.pred

# SAVE OUTPUT
save(nyc.fit, nyc.fit1, nyc.pred, file = "nyc.out.Rdata")