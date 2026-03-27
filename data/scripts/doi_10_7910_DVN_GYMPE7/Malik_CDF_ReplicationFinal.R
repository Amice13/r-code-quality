######################################################################
###### Rabia Malik
###### Replication Code for "(A)political Constituency Development
###### Funds: Evidence from Pakistan"
###### August 18, 2019
######################################################################

rm(list=ls())
library(foreign)
library(MASS)
library(stargazer)
library(plyr)
library(rms)
library(mgcv)
library(pastecs)
library(rdrobust)
library(gridExtra)
library(rdd)

# load dataset using the file path being used ("Malik_Pakistan_CDF_replicationData.csv")
cdf<-read.csv("INSERT FILE PATH HERE.csv")

#############
##### Table 1: Descriptive Statistics
#############
fordesc1 <- data.frame(cbind(cdf$fund_perc,
                             cdf$Ruling_Party_Margin,
                             cdf$same_party, 
                             cdf$Previous_MNA,
                             cdf$Previous_MNA_Terms, 
                             cdf$Min_CurrentOrPast,
                             cdf$Min_CurrentOrPast_Imp, 
                             cdf$Election_Year,
                             cdf$Turnout_perc,
                             cdf$Reg_Voters,
                             cdf$Num_Cand,
                             cdf$ENP_Method1))
                   
stargazer(fordesc1,
          covariate.labels=c("Fund Access %", 
                             "Margin of Victory (Ruling Party)",
                             "Ruling Party", 
                             "Previous MNA",
                             "Previous MNA Terms", 
                             "Federal Minister",
                             "Federal Minister Imp.", 
                             "Election Year",
                             "Turnout",
                             "Num. Registered Voters",
                             "Num. Candidates",
                             "Effective Num. Parties"))

## for medians
stat.desc(fordesc1)

##############
#### Figure 1: Effect of Ruling Party Legislator on Development Fund Access
# (NOTE: Data subsetted to 0.4 Margin of Victory on either side for 
#symmetry of the figure)
##############

cdf.plot <- subset(cdf, abs(cdf$Ruling_Party_Margin) <= .4)

# make the binned averages, using an interval of 2% votes; first the bins to the left
count <- 1
bin.size <- .02
binx.left <- vector(length=length(seq(-.4, -0.02, bin.size)))
biny.left <- vector(length=length(binx.left))
last <- -.4
for(j in seq(-.4, -0.02, bin.size)) {
  biny.left[count] <- mean(cdf.plot$fund_perc[cdf.plot$Ruling_Party_Margin >= j-bin.size & cdf.plot$Ruling_Party_Margin < j],na.rm=T)
  binx.left[count] <- (j+last)/2
  last <- j
  count <- count + 1
  print(j)
}

# now the bins on the right
count <- 1
bin.size <- .02
binx.right <- vector(length=length(seq(0.02,.4, bin.size)))
biny.right <- vector(length=length(binx.right))
last <- 0
for(j in seq(0.02,.4, bin.size)) {
  biny.right[count] <- mean(cdf.plot$fund_perc[cdf.plot$Ruling_Party_Margin >= j-bin.size & cdf.plot$Ruling_Party_Margin < j],
                            na.rm=T)
  binx.right[count] <- (j+last)/2
  last <- j
  count <- count + 1
  print(j)
}

### ols regs for winners and losers separately
reg1 <- lm(cdf.plot$fund_perc[cdf.plot$Ruling_Party_Margin < 0] ~ cdf.plot$Ruling_Party_Margin[cdf.plot$Ruling_Party_Margin < 0])
reg2 <- lm(cdf.plot$fund_perc[cdf.plot$Ruling_Party_Margin > 0] ~ cdf.plot$Ruling_Party_Margin[cdf.plot$Ruling_Party_Margin > 0])

fits1 <- reg1$coefficients[1] + reg1$coefficients[2] * cdf.plot$Ruling_Party_Margin[cdf.plot$Ruling_Party_Margin<0]
fits2 <- reg2$coefficients[1] + reg2$coefficients[2] * cdf.plot$Ruling_Party_Margin[cdf.plot$Ruling_Party_Margin>0]

plot(x=cdf.plot$Ruling_Party_Margin, y=cdf.plot$fund_perc, col="white", xlab="(Ruling Party) Margin of Victory", yaxt="n", ylab="Fund Percentage", cex.lab=1.5, cex.axis=.9, ylim=c(0,120), main="", cex.main=1.8, cex.axis=1.4, xaxt="n", xlim=c(-0.4, 0.4))
abline(v=0, col="gray10", lty=2)
points(x=cdf.plot$Ruling_Party_Margin, y=cdf.plot$fund_perc, pch=16, col="gray50", cex=.6)
points(x=binx.left, y=biny.left, cex=1, col="black", pch=16)
points(x=binx.right, y=biny.right, cex=1, pch=16)
lines(x=cdf.plot$Ruling_Party_Margin[cdf.plot$Ruling_Party_Margin<0], y=fits1, lwd=2, col="black")
lines(x=cdf.plot$Ruling_Party_Margin[cdf.plot$Ruling_Party_Margin>0], y=fits2, lwd=2, col="black")
axis(side=2, las=1, cex.axis=.7, at=seq(0,120,20), labels=seq(0, 120, 20), cex.axis=1)
axis(side=1, at=seq(-0.4, 0.4, 0.1), labels=seq(-0.4, 0.4, 0.1), cex.axis=1)

#########
###### Table 2: Effect of Ruling Party Legislator on Development Fund Access
# Note: Table made in latex directly
#########
# Model 1:
democ.0.05 <- rdrobust(cdf$fund_perc, cdf$Ruling_Party_Margin, h=0.05,
                       all=TRUE)
democ.0.05$Nh # for N
summary(democ.0.05)

# Model 2:
democ.CCT <- rdrobust(cdf$fund_perc, cdf$Ruling_Party_Margin,
                      all=TRUE)
democ.CCT$Nh
summary(democ.CCT) # h gives the bandwidth

# Model 3:
democ_cubic <- ols(fund_perc ~ same_party + 
                     Ruling_Party_Margin +
                     MofV_sq + 
                     MofV_cu +
                     Ruling_Party_Margin*same_party +
                     MofV_sq*same_party +
                     MofV_cu*same_party,# +
                   data=cdf, x=TRUE, y=TRUE)
democ_cubic 
robcov(democ_cubic, cluster=cdf$District) 

##########
### Table 3: Election Cycles
##########
election_cycles1 <- (lm(fund_perc ~ same_party +
                          Ruling_Party_Margin +
                          zero_to_elec +
                          same_party*zero_to_elec +
                          Previous_MNA +
                          Min_CurrentOrPast +
                          Num_Cand +
                          Turnout_perc +
                          as.factor(District),
                        data=cdf))
summary(election_cycles1)


################
#### Figure 2: Access to Development Funds by Year - Raw Data
################
cdf$RP_Leg <- ifelse(cdf$Ruling_Party_Margin>0,1,0)
rd.data_R <- subset(cdf, RP_Leg==1)
rd.data_O <- subset(cdf, RP_Leg==0)

# calculate yearly average for RP legislators
ruling_funds <- NA
for (i in unique(rd.data_R$Start_Year)){
  temp <- rd.data_R[rd.data_R$Start_Year==i,]
  fund_avg <- cbind(temp$Start_Year[1], mean(temp$fund_perc))
  ruling_funds <- rbind(ruling_funds, fund_avg)
}
ruling_funds2 <- data.frame(ruling_funds[2:13,])
colnames(ruling_funds2) <- c("Year", "avg_fund")
ruling_funds3 <- arrange(ruling_funds2, Year)

# calculate yearly average for Opposition Legislators
opp_funds <- NA
for (i in unique(rd.data_O$Start_Year)){
  temp <- rd.data_O[rd.data_O$Start_Year==i,]
  fund_avg <- cbind(temp$Start_Year[1], mean(temp$fund_perc))
  opp_funds <- rbind(opp_funds, fund_avg)
}
opp_funds2 <- data.frame(opp_funds[2:13,])
colnames(opp_funds2) <- c("Year", "avg_fund")
opp_funds3 <- arrange(opp_funds2, Year)

# merge both averages to one dataset for plotting
year_funds_data <- merge(ruling_funds3, opp_funds3, by=c("Year"))

# code variables to divide pre and post military years (since there is a 
# gap in the data for the military period)
year_funds_data$RP_funds_premil <- ifelse(year_funds_data$Year<1999,
                                          year_funds_data$avg_fund.x,
                                          "NA")
year_funds_data$RP_funds_postmil <- ifelse(year_funds_data$Year>1998,
                                           year_funds_data$avg_fund.x,
                                           "NA")
year_funds_data$Opp_funds_premil <- ifelse(year_funds_data$Year<1999,
                                           year_funds_data$avg_fund.y,
                                           "NA")
year_funds_data$Opp_funds_postmil <- ifelse(year_funds_data$Year>1998,
                                            year_funds_data$avg_fund.y,
                                            "NA")

# need to fix 1997 manually since it gives an NA for all legislators due to no funds
# being released
year_funds_data[13,] <- c(1997,NA,NA,0,NA,0,NA)

year_fundsPlot <- ggplot(year_funds_data, 
                         aes(x = Year, as.numeric(paste(y = RP_funds_premil))))+
  geom_vline(xintercept=1993, lty=1, size=1, col="gray") +
  geom_vline(xintercept=1997, lty=1, size=1, col="gray") +
  geom_vline(xintercept=2008, lty=1, size=1, col="gray") +
  geom_point() + geom_line() +
  geom_line(aes(x = Year, as.numeric(paste(y = RP_funds_postmil)))) +
  geom_point(aes(x = Year, as.numeric(paste(y = RP_funds_postmil)))) +
  geom_point(aes(x = Year, as.numeric(paste(y = Opp_funds_premil)))) +
  geom_line(aes(x = Year, as.numeric(paste(y = Opp_funds_premil))), lty=2) +
  geom_line(aes(x = Year, as.numeric(paste(y = Opp_funds_postmil))), lty=2) +
  geom_point(aes(x = Year, as.numeric(paste(y = Opp_funds_postmil)))) +
  theme_bw() +
  annotate("rect", xmin = 1998.5, xmax = 2007.6, ymin = 0, ymax = 200, alpha = .2) +
  annotate("text", x = 2003, y = 180, label = "Military control") +
  scale_x_continuous("\n Year", limits=c(1990,2012)) +
  scale_y_continuous("Avg. Fund Percentage\n", limits = c(0,200)) +
  theme(axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10),
        plot.title = element_text(lineheight= 0.5, size = 14, face="bold"),
        strip.text = element_text(size=13),
        legend.position = ("none"))

year_fundsPlot

##########
### Table 4: Different Ruling Parties
##########

mod_pmlngov <- lm(fund_perc ~ same_party +
                    Ruling_Party_Margin +
                    pmln_gov + 
                    pmln_gov*same_party +
                    Previous_MNA +
                    Min_CurrentOrPast+
                    Num_Cand +
                    Turnout_perc +
                    as.factor(District)-1,
                  data=democ2)
summary(mod_pmlngov)

############
### Figure 3: Access to Development Funds - Raw Data
############

# separate data for Ruling Party and Opp. Legislators
rd.data_R <- subset(cdf, same_party==1)
rd.data_O <- subset(cdf, same_party==0)

# Calculate average fund access by Margin of Vic for Ruling legislators
# (NOTE: The x axis is chosen based on the level of Margin of Vic that occurs for both
# ruling and opposition legislators for symmetry of the figure)
count <- 1
bin.size <- .04
binxR <- vector(length=length(seq(0.04, 0.48, bin.size)))
binyR <- vector(length=length(binxR))
last <- -0.04
for(j in seq(0.04, 0.48, bin.size)) {
  binyR[count] <- mean(rd.data_R$fund_perc[rd.data_R$Vic_Mar >= j-bin.size & rd.data_R$Vic_Mar < j],na.rm=T)
  binxR[count] <- (j+last)/2
  last <- j
  count <- count + 1
  print(j)
}
cbind(binxR, binyR)

### Calculate average fund access by Margin of Vic for Opposition legislators
countO <- 1
bin.sizeO <- .04
binxO <- vector(length=length(seq(0.04, 0.48, bin.sizeO)))
binyO <- vector(length=length(binxO))
last <- -0.04
for(j in seq(0.04, 0.48, bin.size)) {
  binyO[countO] <- mean(rd.data_O$fund_perc[rd.data_O$Vic_Mar >= j-bin.size & rd.data_O$Vic_Mar < j],na.rm=T)
  binxO[countO] <- (j+last)/2
  last <- j
  countO <- countO + 1
  print(j)
}
cbind(binxO, binyO)

# plot both
plot(x=rd.data_R$Vic_Mar, y=rd.data_R$fund_perc, col="white", xlab="Margin of Victory", yaxt="n", ylab="Fund Percentage", cex.lab=1, cex.axis=.9, ylim=c(0,140), main="", cex.main=1, cex.axis=1, xaxt="n", xlim=c(0,0.48))
points(x=binxR, y=binyR, cex=1.2, pch=16, type="l", lty=1, lwd=2)
points(x=binxO, y=binyO, cex=1.2, pch=16, type="l", lty=2, lwd=2)
axis(side=2, las=1, cex.axis=.7, at=seq(0, 140,20), labels=seq(0, 140, 20), cex.axis=0.8)
axis(side=1, at=seq(0,0.7,0.1), labels=seq(0,0.7,0.1), cex.axis=0.8)
legend(0.35,35, c("RP Leg.", "Opp. Leg."), lty=c(1,2), cex=0.7)

##########
#### Table A1 (Appendix A.1): Constraining Dependent Variable to 100%
#########

# model 1: bw=0.05
cap100.05 <- rdrobust(cdf$fund_perc_alt, cdf$Ruling_Party_Margin, h=0.05,
                      all=TRUE)
cap100.05 # for N
summary(cap100.05)

# model 2: bw=CCT
cap100.CCT <- rdrobust(cdf$fund_perc_alt, cdf$Ruling_Party_Margin,
                       all=TRUE)
cap100.CCT 
summary(cap100.CCT)

# model 3: cubic polynomial
cap100_cubic <- ols(fund_perc_alt ~ same_party + 
                      Ruling_Party_Margin +
                      MofV_sq + 
                      MofV_cu +
                      Ruling_Party_Margin*same_party +
                      MofV_sq*same_party +
                      MofV_cu*same_party,# +
                    data=cdf, x=TRUE, y=TRUE)
cap100_cubic 
robcov(cap100_cubic, cluster=cdf$District) 

##########
### Table A2 (Appendix A.1): Dependent Variable in Pakistani Rupees (Millions)
##########

# model 1
pkr.05 <- rdrobust(cdf$Total_Rs, cdf$Ruling_Party_Margin, h=0.05,
                   all=TRUE)
pkr.05
summary(pkr.05) 

# model 2
pkr.CCT <- rdrobust(cdf$Total_Rs, cdf$Ruling_Party_Margin, all=TRUE)
pkr.CCT 
summary(pkr.CCT)

# model 3
pkr_cubic <- ols(Total_Rs ~ same_party + 
                   Ruling_Party_Margin +
                   MofV_sq + 
                   MofV_cu +
                   Ruling_Party_Margin*same_party +
                   MofV_sq*same_party +
                   MofV_cu*same_party,
                 data=cdf, x=TRUE, y=TRUE)
pkr_cubic 
robcov(pkr_cubic, cluster=cdf$Start_Year) 


###########
### Figure A1 (Appendix A.2): RD Estimate with Different Bandwidths
###########

# calculate RD estimate and Std Error for range of BWs
BWs_data <- data.frame(bws=seq(0.025,0.25,by=0.005),
                       bw_estimate=NA, bw_stderr=NA)

for (i in 1:nrow(BWs_data)){
  temp_model <- rdrobust(cdf$fund_perc, cdf$Ruling_Party_Margin,
                         h=BWs_data$bws[i],
                         b=0.23)
  BWs_data$bw_estimate[i] <- temp_model$coef[3]
  BWs_data$bw_stderr[i] <- temp_model$se[3]
  print(i)
}

BWs_data$CI_upper = BWs_data$bw_estimate + 1.96*BWs_data$bw_stderr
BWs_data$CI_lower = BWs_data$bw_estimate - 1.96*BWs_data$bw_stderr

# plot
plot1_new <- ggplot(BWs_data, aes(x = bws)) +
  
  geom_pointrange(aes(y = bw_estimate, ymax = CI_upper, ymin = CI_lower)) +
  
  geom_hline(yintercept = 0, lty = 2, lwd = 1, col = "red") + 
  
  scale_x_continuous("\n Bandwidth") +
  
  scale_y_continuous("RDD Estimate\n (95% CI)", breaks = c(0, 25, 50, 75, 100, 125, 150)) +
  
  theme_bw() +
  
  theme(axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.text.x = element_text(size=12),
        plot.title = element_text(lineheight= 0.5, size = 15, face="bold")) 

plot1_new

##############
### Table A3 (Appendix A.2): Alternative RD Specifications (varying BWs and polynomial
# control functions)
##############

# BW=1 (i.e., all data)
# poly order = 0
non_bw1 <- lm(fund_perc ~ 
                same_party,
              data=cdf)
summary(non_bw1)

# poly order = 1
lin_bw1 <- lm(fund_perc ~ Ruling_Party_Margin +
                same_party,
              data=cdf)
summary(lin_bw1)

# poly order = 2
quad_bw1 <- lm(fund_perc ~ Ruling_Party_Margin +
                 MofV_sq +
                 same_party,
               data=cdf)
summary(quad_bw1)

# poly order = 3
cub_bw1 <- lm(fund_perc ~ Ruling_Party_Margin +
                MofV_sq +
                MofV_cu +
                same_party,
              data=cdf)
summary(cub_bw1)

# poly order = 4
quar_bw1 <- lm(fund_perc ~ Ruling_Party_Margin +
                 MofV_sq +
                 MofV_cu +
                 MofV_quar +
                 same_party,
               data=cdf)
summary(quar_bw1)

# BW = 0.5
bw_half <- subset(cdf, abs(Ruling_Party_Margin)<0.5000001)

non_bw05 <- lm(fund_perc ~ 
                 same_party,
               data=bw_half)
summary(non_bw05)

lin_bw05 <- lm(fund_perc ~ Ruling_Party_Margin +
                 same_party,
               data=bw_half)
summary(lin_bw05)

quad_bw05 <- lm(fund_perc ~ Ruling_Party_Margin +
                  MofV_sq +
                  same_party,
                data=bw_half)
summary(quad_bw05)

cub_bw05 <- lm(fund_perc ~ Ruling_Party_Margin +
                 MofV_sq +
                 MofV_cu +
                 same_party,
               data=bw_half)
summary(cub_bw05)

quar_bw05 <- lm(fund_perc ~ Ruling_Party_Margin +
                  MofV_sq +
                  MofV_cu +
                  MofV_quar +
                  same_party,
                data=bw_half)
summary(quar_bw05)

# BW = 0.25
bw_quarter <- subset(cdf, abs(Ruling_Party_Margin)<0.250000001)

non_bw25 <- lm(fund_perc ~ 
                 same_party,
               data=bw_quarter)
summary(non_bw25)

lin_bw25 <- lm(fund_perc ~ Ruling_Party_Margin +
                 same_party,
               data=bw_quarter)
summary(lin_bw25)

quad_bw25 <- lm(fund_perc ~ Ruling_Party_Margin +
                  MofV_sq +
                  same_party,
                data=bw_quarter)
summary(quad_bw25)

cub_bw25 <- lm(fund_perc ~ Ruling_Party_Margin +
                 MofV_sq +
                 MofV_cu +
                 same_party,
               data=bw_quarter)
summary(cub_bw25)

quar_bw25 <- lm(fund_perc ~ Ruling_Party_Margin +
                  MofV_sq +
                  MofV_cu +
                  MofV_quar +
                  same_party,
                data=bw_quarter)
summary(quar_bw25)

# BW = 0.1
bw_tenth <- subset(cdf, abs(Ruling_Party_Margin)<0.1000000001)

non_bw10 <- lm(fund_perc ~ 
                 same_party,
               data=bw_tenth)
summary(non_bw10)

lin_bw10 <- lm(fund_perc ~ Ruling_Party_Margin +
                 same_party,
               data=bw_tenth)
summary(lin_bw10)

quad_bw10 <- lm(fund_perc ~ Ruling_Party_Margin +
                  MofV_sq +
                  same_party,
                data=bw_tenth)
summary(quad_bw10)

cub_bw10 <- lm(fund_perc ~ Ruling_Party_Margin +
                 MofV_sq +
                 MofV_cu +
                 same_party,
               data=bw_tenth)
summary(cub_bw10)

quar_bw10 <- lm(fund_perc ~ Ruling_Party_Margin +
                  MofV_sq +
                  MofV_cu +
                  MofV_quar +
                  same_party,
                data=bw_tenth)
summary(quar_bw10)


# BW = 0.05
bw_fifth <- subset(cdf, abs(Ruling_Party_Margin)<0.0500000001)
summary(bw_fifth$Ruling_Party_Margin)

non_bw005 <- lm(fund_perc ~ 
                  same_party,
                data=bw_fifth)
summary(non_bw005)

lin_bw005 <- lm(fund_perc ~ Ruling_Party_Margin +
                  same_party,
                data=bw_fifth)
summary(lin_bw005)

quad_bw005 <- lm(fund_perc ~ Ruling_Party_Margin +
                   MofV_sq +
                   same_party,
                 data=bw_fifth)
summary(quad_bw005)

cub_bw005 <- lm(fund_perc ~ Ruling_Party_Margin +
                  MofV_sq +
                  MofV_cu +
                  same_party,
                data=bw_fifth)
summary(cub_bw005)

quar_bw005 <- lm(fund_perc ~ Ruling_Party_Margin +
                   MofV_sq +
                   MofV_cu +
                   MofV_quar +
                   same_party,
                 data=bw_fifth)
summary(quar_bw005)

###########
### Figure A2 (Appendix A.2): Polynomial smoother graphs for different Bandwidths
###########

## (a) BW=0.5
# Polynomial order = 1 (i.e., Linear)
rd.data <- cdf
plot_lin0.5 <- ggplot(rd.data, aes(x = Ruling_Party_Margin, y = fund_perc)) +
  
  geom_point(alpha=0.3, size=0.6) +
  
  geom_smooth(aes(colour = factor(same_party)), method = lm,
              formula = y ~ poly(x, 1), se = TRUE, level = 0.95,
              size = 0.8, alpha = 0.5) +
  
  geom_vline(xintercept = 0, linetype = 2) +
  ggtitle("Linear") +
  
  scale_colour_manual(values = c("blue","blue")) +
  
  scale_shape_manual(values = c(1,5)) +
  
  scale_x_continuous("\n RP Margin of Victory", limits = c(-0.5,0.5)) +
  
  scale_y_continuous("Fund Percentage\n", limits = c(0,200)) +
  
  theme_bw() +
  
  theme(axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10),
        plot.title = element_text(lineheight= 0.5, size = 14, face="bold"),
        strip.text = element_text(size=13),
        legend.position = "none")

plot_lin0.5
# (NOTE: The dropped observations are those outside of the -0.5 to 0.5 range;
# the first figure is using a BW of 0.5 because there are no observations with
# a margin lower than -0.5, so those are dropped to preserve x axis symmetry of
# the figure.)


# Polynomial Order = 2 (i.e., Quadratic)
plot_quad0.5 <- ggplot(rd.data, aes(x = Ruling_Party_Margin, y = fund_perc)) +
  
  geom_point(alpha=0.3, size=0.6) +
  
  geom_smooth(aes(colour = factor(same_party)), method = lm,
              formula = y ~ poly(x, 2), se = TRUE, level = 0.95,
              size = 0.8, alpha = 0.5) +
  
  geom_vline(xintercept = 0, linetype = 2) +
  ggtitle("Quadratic") +
  
  scale_colour_manual(values = c("blue","blue")) +
  
  scale_shape_manual(values = c(1,5)) +
  
  scale_x_continuous("\n RP Margin of Victory", limits = c(-0.5,0.5)) +
  
  scale_y_continuous("\n", limits = c(0,200)) +
  
  theme_bw() +
  
  theme(axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10),
        plot.title = element_text(lineheight= 0.5, size = 14, face="bold"),
        strip.text = element_text(size=13),
        legend.position = "none")

plot_quad0.5

# Polynomial Order = 3 (i.e., Cubic)
plot_cub0.5 <- ggplot(rd.data, aes(x = Ruling_Party_Margin, y = fund_perc)) +
  
  geom_point(alpha=0.3, size=0.6) +
  
  geom_smooth(aes(colour = factor(same_party)), method = lm,
              formula = y ~ poly(x, 3), se = TRUE, level = 0.95,
              size = 0.8, alpha = 0.5) +
  
  geom_vline(xintercept = 0, linetype = 2) +
  ggtitle("Cubic") +
  
  scale_colour_manual(values = c("blue","blue")) +
  
  scale_shape_manual(values = c(1,5)) +
  
  scale_x_continuous("\n RP Margin of Victory", limits = c(-0.5,0.5)) +
  
  scale_y_continuous("\n", limits = c(0,200)) +
  
  theme_bw() +
  
  theme(axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10),
        plot.title = element_text(lineheight= 0.5, size = 14, face="bold"),
        strip.text = element_text(size=13),
        legend.position = "none")

plot_cub0.5


# Polynomial Order = 4 (i.e., Quartic)
plot_quar0.5 <- ggplot(rd.data, aes(x = Ruling_Party_Margin, y = fund_perc)) +
  
  geom_point(alpha=0.3, size=0.6) +
  
  geom_smooth(aes(colour = factor(same_party)), method = lm,
              formula = y ~ poly(x, 4), se = TRUE, level = 0.95,
              size = 0.8, alpha = 0.5) +
  
  geom_vline(xintercept = 0, linetype = 2) +
  ggtitle("Quartic") +
  
  scale_colour_manual(values = c("blue","blue")) +
  
  scale_shape_manual(values = c(1,5)) +
  
  scale_x_continuous("\n RP Margin of Victory", limits = c(-0.5,0.5)) +
  
  scale_y_continuous("\n", limits = c(0,200)) +
  
  theme_bw() +
  
  theme(axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10),
        plot.title = element_text(lineheight= 0.5, size = 14, face="bold"),
        strip.text = element_text(size=13),
        legend.position = "none")

plot_quar0.5

grid.arrange(plot_lin0.5, plot_quad0.5, plot_cub0.5, plot_quar0.5, ncol=4)

## (b) BW=0.25

# Linear
plot_lin0.25 <- ggplot(rd.data, aes(x = Ruling_Party_Margin, y = fund_perc)) +
  
  geom_point(alpha=0.3, size=0.6) +
  
  geom_smooth(aes(colour = factor(same_party)), method = lm,
              formula = y ~ poly(x, 1), se = TRUE, level = 0.95,
              size = 0.8, alpha = 0.5) +
  
  geom_vline(xintercept = 0, linetype = 2) +
  ggtitle("Linear") +
  
  scale_colour_manual(values = c("blue","blue")) +
  
  scale_shape_manual(values = c(1,5)) +
  
  scale_x_continuous("\n RP Margin of Victory", limits = c(-0.25,0.25)) +
  
  scale_y_continuous("Fund Percentage\n", limits = c(0,200)) +
  
  theme_bw() +
  
  theme(axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10),
        plot.title = element_text(lineheight= 0.5, size = 14, face="bold"),
        strip.text = element_text(size=13),
        legend.position = "none")

plot_lin0.25

# Quadratic
plot_quad0.25 <- ggplot(rd.data, aes(x = Ruling_Party_Margin, y = fund_perc)) +
  
  geom_point(alpha=0.3, size=0.6) +
  
  geom_smooth(aes(colour = factor(same_party)), method = lm,
              formula = y ~ poly(x, 2), se = TRUE, level = 0.95,
              size = 0.8, alpha = 0.5) +
  
  geom_vline(xintercept = 0, linetype = 2) +
  ggtitle("Quadratic") +
  
  scale_colour_manual(values = c("blue","blue")) +
  
  scale_shape_manual(values = c(1,5)) +
  
  scale_x_continuous("\n RP Margin of Victory", limits = c(-0.25,0.25)) +
  
  scale_y_continuous("\n", limits = c(0,200)) +
  
  theme_bw() +
  
  theme(axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10),
        plot.title = element_text(lineheight= 0.5, size = 14, face="bold"),
        strip.text = element_text(size=13),
        legend.position = "none")

plot_quad0.25

# Cubic
plot_cub0.25 <- ggplot(rd.data, aes(x = Ruling_Party_Margin, y = fund_perc)) +
  
  geom_point(alpha=0.3, size=0.6) +
  
  geom_smooth(aes(colour = factor(same_party)), method = lm,
              formula = y ~ poly(x, 3), se = TRUE, level = 0.95,
              size = 0.8, alpha = 0.5) +
  
  geom_vline(xintercept = 0, linetype = 2) +
  ggtitle("Cubic") +
  
  scale_colour_manual(values = c("blue","blue")) +
  
  scale_shape_manual(values = c(1,5)) +
  
  scale_x_continuous("\n RP Margin of Victory", limits = c(-0.25,0.25)) +
  
  scale_y_continuous("\n", limits = c(0,200)) +
  
  theme_bw() +
  
  theme(axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10),
        plot.title = element_text(lineheight= 0.5, size = 14, face="bold"),
        strip.text = element_text(size=13),
        legend.position = "none")

plot_cub0.25

# Quartic
plot_quar0.25 <- ggplot(rd.data, aes(x = Ruling_Party_Margin, y = fund_perc)) +
  
  geom_point(alpha=0.3, size=0.6) +
  
  geom_smooth(aes(colour = factor(same_party)), method = lm,
              formula = y ~ poly(x, 4), se = TRUE, level = 0.95,
              size = 0.8, alpha = 0.5) +
  
  geom_vline(xintercept = 0, linetype = 2) +
  ggtitle("Quartic") +
  
  scale_colour_manual(values = c("blue","blue")) +
  
  scale_shape_manual(values = c(1,5)) +
  
  scale_x_continuous("\n RP Margin of Victory", limits = c(-0.25,0.25)) +
  
  scale_y_continuous("\n", limits = c(0,200)) +
  
  theme_bw() +
  
  theme(axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10),
        plot.title = element_text(lineheight= 0.5, size = 14, face="bold"),
        strip.text = element_text(size=13),
        legend.position = "none")

plot_quar0.25


grid.arrange(plot_lin0.25, plot_quad0.25, plot_cub0.25, plot_quar0.25, ncol=4)

############
### Figure A3 (Appendix A.2): Polynomial smoother graphs for different Bandwidths
### (continued)
############

## BW=0.10
# Linear
plot_lin0.1 <- ggplot(rd.data, aes(x = Ruling_Party_Margin, y = fund_perc)) +
  
  geom_point(alpha=0.3, size=0.6) +
  
  geom_smooth(aes(colour = factor(same_party)), method = lm,
              formula = y ~ poly(x, 1), se = TRUE, level = 0.95,
              size = 0.8, alpha = 0.5) +
  
  geom_vline(xintercept = 0, linetype = 2) +
  ggtitle("Linear") +
  
  scale_colour_manual(values = c("blue","blue")) +
  
  scale_shape_manual(values = c(1,5)) +
  
  scale_x_continuous("\n RP Margin of Victory", limits = c(-0.1,0.1)) +
  
  scale_y_continuous("Fund Percentage\n", limits = c(0,200)) +
  
  theme_bw() +
  
  theme(axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10),
        plot.title = element_text(lineheight= 0.5, size = 14, face="bold"),
        strip.text = element_text(size=13),
        legend.position = "none")

plot_lin0.1

# Quadratic
plot_quad0.1 <- ggplot(rd.data, aes(x = Ruling_Party_Margin, y = fund_perc)) +
  
  geom_point(alpha=0.3, size=0.6) +
  
  geom_smooth(aes(colour = factor(same_party)), method = lm,
              formula = y ~ poly(x, 2), se = TRUE, level = 0.95,
              size = 0.8, alpha = 0.5) +
  
  geom_vline(xintercept = 0, linetype = 2) +
  ggtitle("Quadratic") +
  
  scale_colour_manual(values = c("blue","blue")) +
  
  scale_shape_manual(values = c(1,5)) +
  
  scale_x_continuous("\n RP Margin of Victory", limits = c(-0.1,0.1)) +
  
  scale_y_continuous("\n", limits = c(0,200)) +
  
  theme_bw() +
  
  theme(axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10),
        plot.title = element_text(lineheight= 0.5, size = 14, face="bold"),
        strip.text = element_text(size=13),
        legend.position = "none")

plot_quad0.1

# Cubic
plot_cub0.1 <- ggplot(rd.data, aes(x = Ruling_Party_Margin, y = fund_perc)) +
  
  geom_point(alpha=0.3, size=0.6) +
  
  geom_smooth(aes(colour = factor(same_party)), method = lm,
              formula = y ~ poly(x, 3), se = TRUE, level = 0.95,
              size = 0.8, alpha = 0.5) +
  
  geom_vline(xintercept = 0, linetype = 2) +
  ggtitle("Cubic") +
  
  scale_colour_manual(values = c("blue","blue")) +
  
  scale_shape_manual(values = c(1,5)) +
  
  scale_x_continuous("\n RP Margin of Victory", limits = c(-0.1,0.1)) +
  
  scale_y_continuous("\n", limits = c(0,200)) +
  
  theme_bw() +
  
  theme(axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10),
        plot.title = element_text(lineheight= 0.5, size = 14, face="bold"),
        strip.text = element_text(size=13),
        legend.position = "none")

plot_cub0.1

# Quartic
plot_quar0.1 <- ggplot(rd.data, aes(x = Ruling_Party_Margin, y = fund_perc)) +
  
  geom_point(alpha=0.3, size=0.6) +
  
  geom_smooth(aes(colour = factor(same_party)), method = lm,
              formula = y ~ poly(x, 4), se = TRUE, level = 0.95,
              size = 0.8, alpha = 0.5) +
  
  geom_vline(xintercept = 0, linetype = 2) +
  ggtitle("Quartic") +
  
  scale_colour_manual(values = c("blue","blue")) +
  
  scale_shape_manual(values = c(1,5)) +
  
  scale_x_continuous("\n RP Margin of Victory", limits = c(-0.1,0.1)) +
  
  scale_y_continuous("\n", limits = c(0,200)) +
  
  theme_bw() +
  
  theme(axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10),
        plot.title = element_text(lineheight= 0.5, size = 14, face="bold"),
        strip.text = element_text(size=13),
        legend.position = "none")

plot_quar0.1

grid.arrange(plot_lin0.1, plot_quad0.1, plot_cub0.1, plot_quar0.1, ncol=4)

## BW=0.05
# Linear
plot_lin0.05 <- ggplot(rd.data, aes(x = Ruling_Party_Margin, y = fund_perc)) +
  
  geom_point(alpha=0.3, size=0.6) +
  
  geom_smooth(aes(colour = factor(same_party)), method = lm,
              formula = y ~ poly(x, 1), se = TRUE, level = 0.9,
              size = 0.8, alpha = 0.5) +
  
  geom_vline(xintercept = 0, linetype = 2) +
  ggtitle("Linear") +
  
  scale_colour_manual(values = c("blue","blue")) +
  
  scale_shape_manual(values = c(1,5)) +
  
  scale_x_continuous("\n RP Margin of Victory", limits = c(-0.05,0.05)) +
  
  scale_y_continuous("Fund Percentage\n", limits = c(0,200)) +
  
  theme_bw() +
  
  theme(axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10),
        plot.title = element_text(lineheight= 0.5, size = 14, face="bold"),
        strip.text = element_text(size=13),
        legend.position = "none")

plot_lin0.05

# Quadratic
plot_quad0.05 <- ggplot(rd.data, aes(x = Ruling_Party_Margin, y = fund_perc)) +
  
  geom_point(alpha=0.3, size=0.6) +
  
  geom_smooth(aes(colour = factor(same_party)), method = lm,
              formula = y ~ poly(x, 2), se = TRUE, level = 0.9,
              size = 0.8, alpha = 0.5) +
  
  geom_vline(xintercept = 0, linetype = 2) +
  ggtitle("Quadratic") +
  
  scale_colour_manual(values = c("blue","blue")) +
  
  scale_shape_manual(values = c(1,5)) +
  
  scale_x_continuous("\n RP Margin of Victory", limits = c(-0.05,0.05)) +
  
  scale_y_continuous("\n", limits = c(0,200)) +
  
  theme_bw() +
  
  theme(axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10),
        plot.title = element_text(lineheight= 0.5, size = 14, face="bold"),
        strip.text = element_text(size=13),
        legend.position = "none")

plot_quad0.05

# Cubic
plot_cub0.05 <- ggplot(rd.data, aes(x = Ruling_Party_Margin, y = fund_perc)) +
  
  geom_point(alpha=0.3, size=0.6) +
  
  geom_smooth(aes(colour = factor(same_party)), method = lm,
              formula = y ~ poly(x, 3), se = TRUE, level = 0.9,
              size = 0.8, alpha = 0.5) +
  
  geom_vline(xintercept = 0, linetype = 2) +
  ggtitle("Cubic") +
  
  scale_colour_manual(values = c("blue","blue")) +
  
  scale_shape_manual(values = c(1,5)) +
  
  scale_x_continuous("\n RP Margin of Victory", limits = c(-0.05,0.05)) +
  
  scale_y_continuous("\n", limits = c(0,200)) +
  
  theme_bw() +
  
  theme(axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10),
        plot.title = element_text(lineheight= 0.5, size = 14, face="bold"),
        strip.text = element_text(size=13),
        legend.position = "none")

plot_cub0.05

# Quartic
plot_quar0.05 <- ggplot(rd.data, aes(x = Ruling_Party_Margin, y = fund_perc)) +
  
  geom_point(alpha=0.3, size=0.6) +
  
  geom_smooth(aes(colour = factor(same_party)), method = lm,
              formula = y ~ poly(x, 4), se = TRUE, level = 0.9,
              size = 0.8, alpha = 0.5) +
  
  geom_vline(xintercept = 0, linetype = 2) +
  ggtitle("Quartic") +
  
  scale_colour_manual(values = c("blue","blue")) +
  
  scale_shape_manual(values = c(1,5)) +
  
  scale_x_continuous("\n RP Margin of Victory", limits = c(-0.05,0.05)) +
  
  scale_y_continuous("\n", limits = c(0,200)) +
  
  theme_bw() +
  
  theme(axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10),
        plot.title = element_text(lineheight= 0.5, size = 14, face="bold"),
        strip.text = element_text(size=13),
        legend.position = "none")

plot_quar0.05

grid.arrange(plot_lin0.05, plot_quad0.05, plot_cub0.05, plot_quar0.05, ncol=4)

#################
#### Figure A4 (Appendix A.3): Global Histogram of forcing variable in 2 percent
# bins to show density
#################

hist_forcing <- ggplot(cdf, aes(x=Ruling_Party_Margin)) +
  geom_histogram(binwidth=0.02, color="black", fill="gray") +
  geom_vline(aes(xintercept=0),
             color="black", linetype="dashed")
hist_forcing

################
#### Figure A5 (Appendix A.3): McCrary Test for various bandwidths of the forcing
# variable
###############

# bw=2.5% (i.e., 0.025)
mccrary_025 <- DCdensity(cdf$Ruling_Party_Margin, 
                         cutpoint=0,
                         bw=0.025,
                         ext.out=FALSE)
abline(v=0, col="gray10", lty=2)
title(main="BW=0.025",xlab="(RP) Margin of Victory", 
      ylab="Forcing Var. Density", cex.lab=1.5)

mccrary_025 # for pvalue

#bw = 5% (i.e., 0.05)
mccrary_05 <- DCdensity(cdf$Ruling_Party_Margin, 
                        cutpoint=0,
                        bw=0.05,
                        ext.out=FALSE)
abline(v=0, col="gray10", lty=2)
title(main="BW=0.05",xlab="(RP) Margin of Victory", 
      ylab="Forcing Variable Density", cex.lab=1.5)

mccrary_05

# BW=7.5% (i.e., 0.075)
mccrary_075 <- DCdensity(cdf$Ruling_Party_Margin, 
                         cutpoint=0,
                         bw=0.075,
                         ext.out=FALSE)
abline(v=0, col="gray10", lty=2)
title(main="BW=0.075",xlab="(RP) Margin of Victory", 
      ylab="Forcing Variable Density", cex.lab=1.5)

mccrary_075

# BW=10% (i.e., 0.1)
mccrary_10 <- DCdensity(cdf$Ruling_Party_Margin, 
                        cutpoint=0,
                        bw=0.10,
                        ext.out=FALSE)
abline(v=0, col="gray10", lty=2)
title(main="BW=0.1",xlab="(RP) Margin of Victory", 
      ylab="Forcing Variable Density", cex.lab=1.5)

mccrary_10

# BW=10.8% (i.e., 0.108)
mccrary_108 <- DCdensity(cdf$Ruling_Party_Margin, 
                         cutpoint=0,
                         bw=0.108,
                         ext.out=FALSE)
abline(v=0, col="gray10", lty=2)
title(main="BW=0.108",xlab="(RP) Margin of Victory", 
      ylab="Forcing Variable Density", cex.lab=1.5)

mccrary_108

# BW=15% (i.e., 0.15)
mccrary_15 <- DCdensity(cdf$Ruling_Party_Margin, 
                        cutpoint=0,
                        bw=0.15,
                        ext.out=FALSE)
abline(v=0, col="gray10", lty=2)
title(main="BW=0.15",xlab="(RP) Margin of Victory", 
      ylab="Forcing Variable Density", cex.lab=1.5)

mccrary_15

##############
### Table A4 (Appendix A.3): Difference-in-mean tests on covariates
##############

# BW=0.05 (Top panel of Table 8)
# subset to relevant races within 5% vote margin
democ_data05 <- subset(cdf, abs(Ruling_Party_Margin)<0.05) 
democ_loss05 <- subset(democ_data05, Ruling_Party_Margin<0) 
democ_win05 <- subset(democ_data05, Ruling_Party_Margin>0) 

t.test(democ_loss05$Previous_MNA, democ_win05$Previous_MNA) 
t.test(democ_loss05$Previous_MNA_Terms, democ_win05$Previous_MNA_Terms) 
t.test(democ_loss05$Min_CurrentOrPast, democ_win05$Min_CurrentOrPast) 
t.test(democ_loss05$ENP_Method1, democ_win05$ENP_Method1)
t.test(democ_loss05$Turnout, democ_win05$Turnout) 
t.test(democ_loss05$Rej_Votes, democ_win05$Rej_Votes) 

# BW=0.108 (Bottom panel of Table 8)
# subset to relevant races within 10.8% vote margin
democ_data108 <- subset(cdf, abs(Ruling_Party_Margin)<0.108)
democ_loss108 <- subset(democ_data108, Ruling_Party_Margin<0)
democ_win108 <- subset(democ_data108, Ruling_Party_Margin>0)

t.test(democ_loss108$Previous_MNA, democ_win108$Previous_MNA)
t.test(democ_loss108$Previous_MNA_Terms, democ_win108$Previous_MNA_Terms)
t.test(democ_loss108$Min_CurrentOrPast, democ_win108$Min_CurrentOrPast) #insig
t.test(democ_loss108$ENP_Method1, democ_win108$ENP_Method1) #
t.test(democ_loss108$Turnout, democ_win108$Turnout) 
t.test(democ_loss108$Rej_Votes, democ_win108$Rej_Votes) 

################
### Table A5 (Appendix 3.2): Covariates as Outcomes
# (Note: I report the coefficient on "same_party" as the "treatment coefficient")
################

summary(glm(Previous_MNA ~ same_party + 
              Ruling_Party_Margin +
              same_party*Ruling_Party_Margin +
              MofV_sq +
              MofV_cu +
              MofV_sq*same_party +
              MofV_cu*same_party, 
            data=cdf,
            family="binomial"))

summary(lm(Previous_MNA_Terms ~ same_party + 
             Ruling_Party_Margin +
             same_party*Ruling_Party_Margin +
             MofV_sq +
             MofV_cu +
             MofV_sq*same_party +
             MofV_cu*same_party, 
           data=cdf))

summary(lm(Min_CurrentOrPast ~ same_party + 
             Ruling_Party_Margin +
             same_party*Ruling_Party_Margin +
             MofV_sq +
             MofV_cu +
             MofV_sq*same_party +
             MofV_cu*same_party, 
           data=cdf))

summary(lm(ENP_Method1 ~ same_party + 
             Ruling_Party_Margin +
             same_party*Ruling_Party_Margin +
             MofV_sq +
             MofV_cu +
             MofV_sq*same_party +
             MofV_cu*same_party, 
           data=cdf))

summary(lm(Turnout_perc ~ same_party + 
             Ruling_Party_Margin +
             same_party*Ruling_Party_Margin +
             MofV_sq +
             MofV_cu +
             MofV_sq*same_party +
             MofV_cu*same_party, 
           data=cdf))

summary(lm(Rej_Votes ~ same_party + 
             Ruling_Party_Margin +
             same_party*Ruling_Party_Margin +
             MofV_sq +
             MofV_cu +
             MofV_sq*same_party +
             MofV_cu*same_party, 
           data=cdf))

###############
### Table A6: RD estimates controlling for covariates
###############
RD_controls_05 <- RDestimate(fund_perc ~ Ruling_Party_Margin | Previous_MNA +
                               Previous_MNA_Terms + Min_CurrentOrPast +
                               Turnout_perc + Rej_Votes, 
                             data=cdf, 
                             cutpoint=0,
                             model=TRUE,
                             bw=0.05)    
summary(RD_controls_05)

RD_controls_108 <- RDestimate(fund_perc ~ Ruling_Party_Margin | Previous_MNA +
                               Previous_MNA_Terms + Min_CurrentOrPast  +
                               Turnout_perc + Rej_Votes, 
                             data=cdf, 
                             cutpoint=0,
                             model=TRUE,
                             bw=0.108)    
summary(RD_controls_108)

RD_controls_cubic <- (lm( fund_perc ~ same_party + 
                            Ruling_Party_Margin +
                            same_party*Ruling_Party_Margin +
                            MofV_sq +
                            MofV_cu +
                            MofV_sq*same_party +
                            MofV_cu*same_party +
                            Min_CurrentOrPast +
                            Previous_MNA +
                            Previous_MNA_Terms +
                            Turnout_perc + 
                            Rej_Votes,
                          data=cdf))
summary(RD_controls_cubic)

###############
### Table A7 (Appendix A.4): Placebo Test 1
###############

# treatment cutoff = -0.3
test_neg0.3 <- rdrobust(cdf$fund_perc, cdf$Ruling_Party_Margin, c=-0.3,
                        all=TRUE) 
summary(test_neg0.3)

# treatment cutoff = -0.2
test_neg0.2 <- rdrobust(cdf$fund_perc, cdf$Ruling_Party_Margin, c=-0.2,
                        all=TRUE) 
summary(test_neg0.2)

# treatment cutoff = -0.1
test_neg0.1 <- rdrobust(cdf$fund_perc, cdf$Ruling_Party_Margin, c=-0.1,
                        all=TRUE)
summary(test_neg0.1)

# treatment cutoff = 0.1
test_0.1 <- rdrobust(cdf$fund_perc, cdf$Ruling_Party_Margin, c=0.1,
                     all=TRUE) 
summary(test_0.1)

# treatment cutoff = 0.2
test_0.2 <- rdrobust(cdf$fund_perc, cdf$Ruling_Party_Margin, c=0.2,
                     all=TRUE) 
summary(test_0.2)

# treatment cutoff = 0.3
test_0.3 <- rdrobust(cdf$fund_perc, cdf$Ruling_Party_Margin, c=0.3,
                     all=TRUE)
summary(test_0.3)

# treatment cutoff = 0.4
test_0.4 <- rdrobust(cdf$fund_perc, cdf$Ruling_Party_Margin, c=0.4,
                     all=TRUE)
summary(test_0.4)

###############
### Figure A6 (Appendix A.4): Lagged Dependent Variable: No Disconuity
##############

### Load lagged dependent variable dataset ("Malik_Pakistan_CDF_LaggedRepData.csv")
cdf_lagY<-read.csv("INSERT FILE PATH")

# subsetting to 0.3 to keep it symmetric on both sides
rd.data <- subset(cdf_lagY, abs(cdf_lagY$MofVic_future) <= .3)

# calculating averages for intervals of length 0.02 of the forcing variable
count <- 1
bin.size <- .02
binx.left <- vector(length=length(seq(-.3, -0.02, bin.size)))
biny.left <- vector(length=length(binx.left))
last <- -.3
for(j in seq(-.3, -0.02, bin.size)) {
  biny.left[count] <- mean(rd.data$fund_perc[rd.data$MofVic_future >= j-bin.size & rd.data$MofVic_future < j],na.rm=T)
  binx.left[count] <- (j+last)/2
  last <- j
  count <- count + 1
  print(j)
}

count <- 1
bin.size <- .02
binx.right <- vector(length=length(seq(0.02,.3, bin.size)))
biny.right <- vector(length=length(binx.right))
last <- 0
for(j in seq(0.02,.3, bin.size)) {
  biny.right[count] <- mean(rd.data$fund_perc[rd.data$MofVic_future >= j-bin.size & rd.data$MofVic_future < j],
                            na.rm=T)
  binx.right[count] <- (j+last)/2
  last <- j
  count <- count + 1
  print(j)
}

reg1 <- lm(rd.data$fund_perc[rd.data$MofVic_future < 0] ~ rd.data$MofVic_future[rd.data$MofVic_future < 0])
reg2 <- lm(rd.data$fund_perc[rd.data$MofVic_future > 0] ~ rd.data$MofVic_future[rd.data$MofVic_future > 0])

fits1 <- reg1$coefficients[1] + reg1$coefficients[2] * rd.data$MofVic_future[rd.data$MofVic_future<0]
fits2 <- reg2$coefficients[1] + reg2$coefficients[2] * rd.data$MofVic_future[rd.data$MofVic_future>0]

plot(x=rd.data$MofVic_future, y=rd.data$fund_perc, col="white", xlab="(Ruling Party) Margin of Victory_t", yaxt="n", ylab="Fund Percentage_t-1", cex.lab=1.5, cex.axis=.9, ylim=c(0,200), main="", cex.main=1.8, cex.axis=1.4, xaxt="n", xlim=c(-0.3, 0.3))
abline(v=0, col="gray50", lty=2)
points(x=rd.data$MofVic_future, y=rd.data$fund_perc, pch=16, col="lightgrey", cex=.6)
points(x=binx.left, y=biny.left, cex=1.7, col="black", pch=16)
points(x=binx.right, y=biny.right, cex=1.7, pch=16)
lines(x=rd.data$MofVic_future[rd.data$MofVic_future<0], y=fits1, lwd=2, col="black")
lines(x=rd.data$MofVic_future[rd.data$MofVic_future>0], y=fits2, lwd=2, col="black")
axis(side=2, las=1, cex.axis=.7, at=seq(0, 200,40), labels=seq(0, 200, 40), cex.axis=1)
axis(side=1, at=seq(-0.3, 0.3, 0.1), labels=seq(-0.3, 0.3, 0.1), cex.axis=1)
text(x=0.25,y=10, paste("N=", nrow(rd.data), sep=""), cex=1.2)


###############
### Table A8 (Appendix A.4): RD Robustness Estimates with Lagged Dependent Variable
###############
# NOTE: Dataset for the followign model loaded above under code for Figure 9

cdf_lagY_PlaceboTest05 <- rdrobust(cdf_lagY$fund_perc, cdf_lagY$MofVic_future,
                                     h=0.05, all=TRUE)
summary(cdf_lagY_PlaceboTest05) 

cdf_lagY_PlaceboTest108 <- rdrobust(cdf_lagY$fund_perc, cdf_lagY$MofVic_future,
                                     h=0.108, all=TRUE)
summary(cdf_lagY_PlaceboTest108) 

cdf_lagY_PlaceboTestCCT <- rdrobust(cdf_lagY$fund_perc, cdf_lagY$MofVic_future,all=TRUE)
summary(cdf_lagY_PlaceboTestCCT) 

