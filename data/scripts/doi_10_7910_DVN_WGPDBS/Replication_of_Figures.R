####Tucker and Smith Replication Figures, POBE
library(ggplot2)
####Changes in Issue Don't Knows 
##Figure 1
primary_means<-c( 6.45, 6.02, 2.74)
general_means<-c(5.78, 5.04, 2.07)
other_mean_tab<-rbind(primary_means, general_means)
rownames(other_mean_tab)<-c("Pre-general Election Campaign","Post-general Election Campaign")
colnames(other_mean_tab)<-c("House 2016", "Senate 2016", "President 2016")
primary_se<-c(.09, .09, .05)
general_se<-c(.08, .09,.05)
other_se_tab<-rbind(primary_se, general_se)
rownames(other_se_tab)<-c("Pre-general Election Campaign","Post-general Election Campaign")
colnames(other_se_tab)<-c("House 2016", "Senate 2016", "President 2016")

barCenters <- barplot(height = other_mean_tab,
                      beside = TRUE, las = 1,
                      ylim = c(0, 7),
                      cex.names = 1,
                      xlab = "Race",
                      border = "black", axes = TRUE,
                      legend.text = TRUE,
                      args.legend = list(title = "Race Type", 
                                         x = "topright",
                                         cex = .9))

segments(barCenters, other_mean_tab - other_se_tab * 1.97, barCenters,
         other_mean_tab + other_se_tab * 1.97, lwd = 1.5)

arrows(barCenters, other_mean_tab - other_se_tab * 1.97, barCenters,
       other_mean_tab + other_se_tab * 1.97, lwd = 1.5, angle = 90,
       code = 3, length = 0.05)

###Figure 2
####Ideological Location DKs
primary_means<-c(.49, .39, .46, .41,.16)
primary_se<-c(.01, .01, .01, .01, .01)
general_means<-c(.36, .27, .35, .27,.13)
general_se<-c(.01, .01, .01, .01, .01 )
other_mean_tab<-rbind(primary_means, general_means)

other_se_tab<-rbind(primary_se, general_se)
colnames(other_mean_tab)<-c("House 2014", "Senate 2014", "House 2016", "Senate 2016", "President 2016")
rownames(other_mean_tab)<-c("Pre-general Election Campaign","Post-general Election Campaign")
rownames(other_se_tab)<-c("Pre-general Election Campaign","Post-general Election Campaign")
colnames(other_se_tab)<-c("House 2014", "Senate 2014", "House 2016", "Senate 2016", "President 2016")

barCenters <- barplot(height = other_mean_tab,
                      beside = TRUE, las = 1,
                      ylim = c(0, .6),
                      cex.names = .9,
                      xlab = "Race",
                      border = "black", axes = TRUE,
                      legend.text = TRUE,
                      args.legend = list(title = "Race Type", 
                                         x = "topright",
                                         cex = .9))

segments(barCenters, other_mean_tab - other_se_tab * 1.97, barCenters,
         other_mean_tab + other_se_tab * 1.97, lwd = 1.5)

arrows(barCenters, other_mean_tab - other_se_tab * 1.97, barCenters,
       other_mean_tab + other_se_tab * 1.97, lwd = 1.5, angle = 90,
       code = 3, length = 0.05)


####Figure 3
###Initial Dem
pid_values<-c(0.98, .97, .62)
pid_lo<-c(0.95, .95, .25)
pid_hi<-c(1, .99, .989)

obama<-c(.946, .949, .951, .953, .955)
o_lo<-c(.90, .91, .924, .922, .913)
o_hi<-c(.998, .984, .978, .985, .997)

sophsq<-c(0.956, .954, .953, .952, .941, .949)
soph_lo<-c(.884, .90, .915, .924, .92, .90)
soph_hi<-c(1, 1, .992, .978, .980, .994)

close<-c(.963, .96, .956, .953, .949, .944)
close_lo<-c(.929, .929, .929, .926, .920, .910)
close_hi<-c(.998, .991, .984, .979, .977, .979)

par(mfrow=c(1,3))
ticks<-c(-2:2)
plot(c(-2:2), obama, main="Initial Democrat Supporter", xlab="Presidential Approval", ylab="Predicted Probability of Voting for Democratic Candidate", pch=19, col="blue", ylim=c(0,1))
for (i in 1:5){
  segments(ticks[i], o_lo[i], ticks[i], o_hi[i], col="blue")
}

###Initial REP
pid_values<-c(0.17, .19, .001)
pid_lo<-c(0.05, .05, 0)
pid_hi<-c(.28, .34, .02)

obama<-c(.048, .094, .166, .258, .355)
o_lo<-c(.01, .041, .075, .111, .158)
o_hi<-c(.08, .15, .26, .41, .55)

plot(c(-2:2), obama, main="Initial Republican Supporter", xlab="Presidential Approval", ylab="Predicted Probability of Voting for Democratic Candidate", pch=19, col="red", ylim=c(0,1))
for (i in 1:5){
  segments(ticks[i], o_lo[i], ticks[i], o_hi[i], col="red")
}

sophsq<-c(0.15, .15, .14, .13, .12, .11)
soph_lo<-c(0, .01, .05, .07, .07, .04)
soph_hi<-c(.34, .28, .23, .18, .17, .18)

close<-c(.207, .181, .157, .136, .117, .099)
close_lo<-c(.096, .096, .092, .085, .072, .056)
close_hi<-c(.317, .265, .222, .188, .161, .142)

####initial other

pid_values<-c(0.503, .76, .39)
pid_lo<-c(0.38, .65, .24)
pid_hi<-c(.63, .88, .55)

obama<-c(.285, .469, .664, .82, .92)
o_lo<-c(.18, .38, .55, .70, .83)
o_hi<-c(.38, .55, .78, .95, 1)

plot(c(-2:2), obama, main="Initial Undecided Supporter", xlab="Presidential Approval", ylab="Predicted Probability of Voting for Democratic Candidate", pch=19, col="black", ylim=c(0,1))
for (i in 1:5){
  segments(ticks[i], o_lo[i], ticks[i], o_hi[i], col="black")
}

sophsq<-c(0.63, .61, .59, .57, .55, .53)
soph_lo<-c(0.45, .48, .49, .51, .499, .463)
soph_hi<-c(.809, .746, .686, .634, .603, .601)

close<-c(.567, .563, .558, .554, .549, .545)
close_lo<-c(.478, .493, .504, .506, .497, .479)
close_hi<-c(.656, .632, .613, .602, .602, .612)


###Figure 4
###Predicted Probabilities of Learning
###Change in a 3 issue don't know swing from -3 to 3
###pres
clinton_voter_prob<-c(.933, .951, .965, .975, .982, .987, .991)
clinton_voter_low<-c(.879, .912, .931, .944, .954, .963, .970)
clinton_voter_high<-c(.989, .991, .998, 1, 1, 1, 1)

trump_voter_prob<-c(.001, .002, .003, .005, .009, .015, .024)
trump_voter_low<-c(0,0,0,0,0,0,0)
trump_voter_high<-c(.003, .005, .008, .013, .023, .040, .070)

other_voter_prob<-c(.440, .452, .465, .478, .491, .504, .517)
other_voter_low<-c(.268, .298, .326, .346, .360, .366, .365)
other_voter_high<-c(.611, .606, .605, .610, .622, .643, .670)

###sen
demsen_voter_prob<-c(.744, .747, .750, .753, .755, .758, .761)
demsen_voter_low<-c(.661, .670, .677, .683, .686, .688, .688)
demsen_voter_high<-c(.826, .823, .822, .822, .825, .829, .834)

repsen_voter_prob<-c(.100, .112, .125, .140, .156, .174, .193)
repsen_voter_low<-c(.008, .024, .042, .061, .080, .097, .108)
repsen_voter_high<-c(.191, .200, .209, .219, .233, .252, .279)

othersen_voter_prob<-c(.370, .387, .404, .421, .439, .456, .474)
othersen_voter_low<-c(.168, .192, .214, .234, .253, .270, .284)
othersen_voter_high<-c(.572, .582, .594, .608, .624, .643, .664)

###House
demhou_voter_prob<-c(.931, .936, .941, .946, .950, .954, .958)
demhou_voter_high<-c(1, 1, 1, 1, 1, 1, 1)
demhou_voter_low<-c(.845, .856, .865, .874, .882, .888, .895)

rephou_voter_prob<-c(.040, .040, .040, .040, .040, .040, .040)
rephou_voter_low<-c(.017, .015, .014, .013, .012, .012, .012)
rephou_voter_high<-c(.073, .071, .068, .066, .064, .063, .063)

otherhou_voter_prob<-c(.327, .356, .386, .417, .449, .481, .513)
otherhou_voter_low<-c(.123, .154, .187, .217, .252, .283, .314)
otherhou_voter_high<-c(.531, .558, .586, .615, .646, .678, .713)

xes<-c(-3:3)
#par(mfrow=c(1,3),mar=c(3, 3, 1, 1), oma=c(1,2,1,1))
par(mfrow=c(1,3))

plot(xes, demhou_voter_prob, pch=16, ylim=c(0,1), col="blue", xlab="Change in Democrat Knowledge Advantage", ylab="Probability of Voting Democrat", main="House", cex=1.4, cex.lab=1.2)
points(xes, rephou_voter_prob, pch=17, col="red", cex=1.4)
points(xes, otherhou_voter_prob, pch=15, col="green", cex=1.4)

for(i in 1:7){
  segments(xes[i], demhou_voter_high[i], xes[i], demhou_voter_low[i], col="blue", lty=1)
}

for(i in 1:7){
  segments(xes[i], rephou_voter_high[i], xes[i], rephou_voter_low[i], col="red", lty=1)
}

for(i in 1:7){
  segments(xes[i], otherhou_voter_high[i], xes[i], otherhou_voter_low[i], col="green", lty=1)
}


plot(xes, demsen_voter_prob, pch=16, ylim=c(0,1), col="blue", xlab="Change in Democrat Knowledge Advantage", ylab="Probability of Voting Democrat", main="Senate", cex=1.4, cex.lab=1.2)
points(xes, repsen_voter_prob, pch=17, col="red", cex=1.4)
points(xes, othersen_voter_prob, pch=15, col="green", cex=1.4)

for(i in 1:7){
  segments(xes[i], demsen_voter_high[i], xes[i], demsen_voter_low[i], col="blue", lty=1)
}

for(i in 1:7){
  segments(xes[i], repsen_voter_high[i], xes[i], repsen_voter_low[i], col="red", lty=1)
}

for(i in 1:7){
  segments(xes[i], othersen_voter_high[i], xes[i], othersen_voter_low[i], col="green", lty=1)
}



plot(xes, clinton_voter_prob, pch=16, ylim=c(0,1), col="blue", xlab="Change in Democrat Knowledge Advantage", ylab="Probability of Voting Democrat", main="President", cex=1.4, cex.lab=1.2)
points(xes, trump_voter_prob, pch=17, col="red", cex=1.4)
points(xes, other_voter_prob, pch=15, col="green", cex=1.4)

for(i in 1:7){
  segments(xes[i], clinton_voter_high[i], xes[i], clinton_voter_low[i], col="blue", lty=1)
}

for(i in 1:7){
  segments(xes[i], trump_voter_high[i], xes[i], trump_voter_low[i], col="red", lty=1)
}

for(i in 1:7){
  segments(xes[i], other_voter_high[i], xes[i], other_voter_low[i], col="green", lty=1)
}
#Legend
par(mar = c(0,0,0,0), mfrow=c(1,1))
plot(0,0, type="n", axes = FALSE, xlab = "", ylab = "")
legend("center", ncol = 3, 
       legend = c("Democrat","Undecided", "Republican"),col=c("blue", "green", "red"), pch=c(16, 15, 17), cex=1.4, lty=c(1), title="Pre-Campaign Candidate Choice", bty="n") 
