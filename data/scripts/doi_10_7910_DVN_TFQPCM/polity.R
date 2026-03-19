### SHALIZI THOMAS TEST ON POLITY DATA -- Run on first differences to correct for non stationarity

polity <- read.csv("../Data/p4v2018.csv",head=TRUE)
polity <- polity[,c("country","year","democ")]
polity90 <- subset(polity, polity$year >= 1969) # change year depending on timeseries length 

countries <- table(polity90$country)

polity90 <- polity90[(polity90$country %in% names(countries[countries==50])),]
polity90 <- spread(polity90,year,democ)

### Below are the countries that have been deleted due to incomplete or no data
unique(polity$country)[!(unique(polity$country) %in% names(countries[countries==50]))]

#### -66 (Interuption Period), -77 (Interreggnum Period) or -88 (Transition Period)
### Changing these period scores to 0? 

sum(polity90 == -77)
sum(polity90 == -66)
sum(polity90 == -88)
## below loops converts all the aberrant values to a 0. 
for(i in 1:dim(polity90)[1]){
  polity90[i,c(2:51)] = ifelse(polity90[i,c(2:51)] == -66, as.numeric(0), polity90[i,c(2:51)])
  polity90[i,c(2:51)] = ifelse(polity90[i,c(2:51)] == -77, as.numeric(0), polity90[i,c(2:51)])
  polity90[i,c(2:51)] = ifelse(polity90[i,c(2:51)] == -88, as.numeric(0), polity90[i,c(2:51)])
}

#### Transposing the dataframe to fit the previous code
polity90 <- as.data.frame(t(as.matrix(polity90)))
politynames <- polity90[1,]
polity90 = polity90[-1,]
polity90 = apply(polity90, 2, as.numeric)

####### Testing for Stationarity of the polity data before running test
####### Both return errors due to zeros
##  pCADFtest(polity90, crosscorr = 0.1)
##  purtest(polity90)
## Running the test on Yt - Yt-1 diff aggregated random panel halfs 
pvals = c()
for(a in 1:200){
  binindex = sample(c(1,2),size=118,replace=T)
  bin1 = polity90[,binindex==1]
  bin2 = polity90[,binindex==2]
  bindat= cbind(apply(bin1,1,mean),apply(bin2,1,mean))
  bindat = diff(bindat,1)
  t = pCADFtest(bindat, crosscorr = 0.1)
  pvals = c(pvals, t$p.value)
  print(a)
}

summary(pvals)  ### THIS DATA is Stationary
# hist(pvals)

#### Seed to replicate the Shalizi Thomas test on polity data
set.seed(24354657)

##### Running the Shalizi Thomas idea on the polity90 dataset (first differences), with 50 years of democ score
simmodels = NULL
for(i in 1:10000){
  j = sample(c(1,2), size=118,replace=TRUE) ## produces unequal bins
  Yj1 = polity90[,j==1]
  Yj2 = polity90[,j==2]
  
  j1mean = apply(Yj1, 1, mean)
  j2mean = apply(Yj2, 1, mean)
  j1mean = diff(j1mean,1)    ### taking diff between Y1 and Y0 for stationarity
  j2mean = diff(j2mean,1)    ### taking diff between Y1 and Y0 for stationarity
  j1mean.t = j1mean[2:(length(j1mean))]
  j2mean.t = j2mean[2:(length(j2mean))]
  
  j1mean.tm1 = j1mean[1:(length(j1mean)-1)]
  j2mean.tm1 = j2mean[1:(length(j2mean)-1)]
  
  regmod <- lm(c(j1mean.t,j2mean.t) ~ c(j1mean.tm1,j2mean.tm1) + 
                 c(j2mean.tm1, j1mean.tm1))
  simmodels <- rbind(simmodels, coefficients(regmod))
}


### Plots 
simmodels = as.data.frame(simmodels)
names(simmodels) = c("intercept","t-1coef","counterpart")


### Below is the PLOT USED IN THE PAPER's APPENDIX
xmean = mean(simmodels$counterpart) ## input this in the plot below
xmean = round(xmean, digits = 4)
pval = sum(simmodels$counterpart < 0) / 10000  ## pvalue 
pval = round(pval, digits = 3)

ggplot(simmodels, aes(x=counterpart)) + 
  ggtitle("Contagion Signal Density on Polity IV data, 1969-2018") +
  xlab("Contagion Signal") +
  ylab("Density") +
  theme_light() +
  geom_density(fill="lightblue") + 
  geom_vline(aes(xintercept=mean(counterpart)),colour="blue") +
  annotate("text", x = xmean, y=6.7, label = paste("mean=", xmean)) +
  geom_vline(aes(xintercept=0),color="black") +
  annotate("text", x=0, y=0, label= paste("p-value=",pval)) +
  ylim(0,7)

ggsave("../Plots/Appendix-Fig 3.png")


