#####################################################################
#Analysis for "The effect of images of Michelle Obama’s face 
#on trick-or-treaters’ dietary choices: a randomized control trial"
#Submitted for review
#####################################################################

rm(list=ls(all = TRUE))

#load libraries
library(foreign)
library(ggplot2)
library(sandwich)
library(readstata13)

setwd("~/Downloads/")
hall <- read.dta13("Halloween2012-2014-2015_PLOS.dta")

#create function to generate plot;
gen.forest <- function(names, means, lowers, uppers, 
                       title = "", perc = FALSE, scale = NULL, 
                       ymin = min(lowers), ymax = max(uppers)) {
  
  par(mfrow = c(1, 1), family = "Avenir Next Condensed", font = 1)
  ylabel <- "Proportion Choosing Fruit"
  
  df1 <- data.frame(names, means, lowers, uppers)
  df2 <- data.frame(df1[1, ])
  df3 <- data.frame(df1[2, ])
  df4 <- data.frame(df1[3, ])
  df5 <- data.frame(df1[4, ])
  df6 <- data.frame(df1[5, ])
    df7 <- data.frame(df1[6, ])
  df8 <- data.frame(df1[7, ])
    df9 <- data.frame(df1[8, ])
     df10 <- data.frame(df1[9, ])
  df11 <- data.frame(df1[10, ])
    df12 <- data.frame(df1[11, ])
   
  
  print(ggplot(df1, aes(x = names, y = means, ymin = lowers, ymax = uppers)) 
        + geom_pointrange(data = df2, linetype = 1, size = 1, colour = "red") 
        + geom_pointrange(data = df3, linetype = 1, size = 1, colour = "black")
        + geom_pointrange(data = df4, linetype = 1, size = 1, colour = "red") 
        + geom_pointrange(data = df5, linetype = 1, size = 1, colour = "black") 
        + geom_pointrange(data = df6, linetype = 1, size = 1, colour = "red") 
        + geom_pointrange(data = df7, linetype = 1, size = 1, colour = "black")
                + geom_pointrange(data = df8, linetype = 1, size = 1, colour = "red") 
        + geom_pointrange(data = df9, linetype = 1, size = 1, colour = "black") 

+geom_segment(x = .5, y = -1, xend = .5, yend = 25,colour="gray",size=1.5)
 +geom_segment(x = 2.5, y = -1, xend = 2.5, yend = 25,colour="gray",size=1.5)
 +geom_segment(x = 4.5, y = -1, xend = 4.5, yend = 25,colour="gray",size=1.5)
+geom_segment(x = 6.5, y = -1, xend = 6.5, yend = 25,colour="gray",size=1.5)
 +geom_segment(x = 8.5, y = -1, xend = 8.5, yend = 25,colour="gray",size=1.5)

        + ylim(0.0,.5) +
          xlab('') + ylab(ylabel) + theme(text = element_text(family = "Avenir Next Condensed"))  + 
          theme(axis.text.x = element_text(colour = 'black', size = 10)) +
          theme(axis.text.y = element_text(colour = 'black', size = 12)) +
          theme_bw() + 
          theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 15), axis.title.x = element_text(angle = 0, size = 13)) + theme(axis.text.y = element_text(colour = 'black', size = 13)) + coord_flip()
  )
}


# Construct weights for inverse probability weighting
pscore <- lm(obama~factor(year),data=hall)$fit
wt <- 1/pscore
wt[hall$obama==0] <- (1/(1-pscore))[hall$obama==0]

notna <- !is.na(hall$fruit) #remove NAs

#calculate mean of outcome variable (choosing fruit), by year and treatment assignment
means0 <- c(by(hall$fruit,hall$treat_year,mean,na.rm=TRUE)) 
#calculate standard errors
ses0 <- (c(by(hall$fruit,hall$treat_year,var,na.rm=TRUE))/
          c(by(notna,hall$treat_year,sum,na.rm=TRUE)))^.5 
#ns <- c(by(notna,hall$treat_year,sum,na.rm=TRUE))

fitO <- lm(fruit~neob,weights=wt,data=hall) #weighted fit, treatment; equiv to just weighted.mean(hall$fruit[hall$obama==1],wt[hall$obama==1],na.rm=T)
fitC <- lm(fruit~obama,weights=wt,data=hall) #weighted fit, comparison
# note that neob = 1-obama

mean1 <- coef(fitO)[1] # estimated mean of outcome variable, treatment
mean2 <- coef(fitC)[1] # estimated mean of outcome variable, comparison group

se1 <- vcovHC(fitO)[1,1]^.5 #calculate robust se for pooled treatment group
se2 <- vcovHC(fitC)[1,1]^.5 #calculate robust se for pooled comparison group

means <- c(mean1,mean2,means0)
ses <- c(se1,se2,ses0)

lowers <- means - 1.96*ses #create lower
uppers <- means + 1.96*ses #create upper

#label plot
names <- c("Pooled: Obama", " Pooled: Comparison","  2012: Obama","   2012: Comparison","    2014: Obama","     2014: Comparison","      2015: Obama","       2015: Comparison")#,"            Control (2015)")

gen.forest(names,means,lowers,uppers) #generate plot

# pvalues

lmfit1 <- fitC
2*pnorm(-abs(coef(lmfit1)[2]/vcovHC(lmfit1)[2,2]^.5))

lmfit2 <- lm(fruit~obama, subset=year==2012,data=hall)
2*pnorm(-abs(coef(lmfit2)[2]/vcovHC(lmfit2)[2,2]^.5))

lmfit3 <- lm(fruit~obama, subset=year==2014,data=hall)
2*pnorm(-abs(coef(lmfit3)[2]/vcovHC(lmfit3)[2,2]^.5))

lmfit4 <- lm(fruit~obama, subset=year==2015,data=hall)
2*pnorm(-abs(coef(lmfit4)[2]/vcovHC(lmfit4)[2,2]^.5))

#quoted in article -- correct the numbers

#Obama treatment
means[1] # % of participants in the Obama treatment that select fruit instead of candy
lowers[1] #lower bound of 95% CI, Obama treatment, select fruit instead of candy
uppers[1] #upper bound of 95% CI, Obama treatment, select fruit instead of candy

#Comparison treatment
means[2] # % of participants in comparison group that select fruit instead of candy
lowers[2] #lower bound of 95% CI, comparison group, selected fruit instead of candy
uppers[2] #upper bound of 95% CI, comparison group, select fruit instead of candy

