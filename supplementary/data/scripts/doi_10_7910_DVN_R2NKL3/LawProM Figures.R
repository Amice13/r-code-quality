##CREDITING INVISIBLE WORK: CONGRESS AND THE LAWMAKING PRODUCTIVITY METRIC (LAWPROM)- EATOUGH & PREECE
#FIGURES

action_data <- read.csv("filepath/Legislative Action Figures.csv")
  #(subset of LawProM.dta)

#Bill Sponsorship (Figure 1)
#Introduced
billintro_data <- subset(action_data, model=="numbillintro")
plot (billintro_data$coefficient, billintro_data$covariate_numeric, family="serif", pch=NA, cex=1.2, main = "Introduced", xlab=NA, bty="n", ylab=NA, yaxt="n",xlim = c(-6, 6), ylim=c(1,14))
segments(0,0,0,15, col="red", lty=2)
points(billintro_data$coefficient, billintro_data$covariate_numeric, pch=18, cex=1.2)
segments(billintro_data$lower_ci, billintro_data$covariate_numeric, billintro_data$upper_ci, billintro_data$covariate_numeric)
text (-6, 14, labels="Majority", pos=2, family="serif", xpd=NA)
text (-6, 13, labels="Democrat", pos=2, family="serif", xpd=NA)
text (-6, 12, labels="Female", pos=2, family="serif", xpd=NA)
text (-6, 11, labels="Black", pos=2, family="serif", xpd=NA)
text (-6, 10, labels="Hispanic", pos=2, family="serif", xpd=NA)
text (-6, 9, labels="AAPI", pos=2, family="serif", xpd=NA)
text (-6, 8, labels="Native American", pos=2, family="serif", xpd=NA)
text (-6, 7, labels="Education", pos=2, family="serif", xpd=NA)
text (-6, 6, labels="Seniority", pos=2, family="serif", xpd=NA)
text (-6, 5, labels="Chair", pos=2, family="serif", xpd=NA)
text (-6, 4, labels="Power Committee", pos=2, family="serif", xpd=NA)
text (-6, 3, labels="State Leg Experience", pos=2, family="serif", xpd=NA)
text (-6, 2, labels="Majority Leadership", pos=2, family="serif", xpd=NA)
text (-6, 1, labels="Minority Leadership", pos=2, family="serif", xpd=NA)
#Engrossed
billengross_data <- subset(action_data, model=="numbillengross")
plot (billengross_data$coefficient, billengross_data$covariate_numeric, family="serif", pch=NA, cex=1.2, main = "Engrossed", xlab=NA, bty="n", ylab=NA, yaxt="n", xlim = c(-1, 4.5), ylim=c(1,14))
segments(0,0,0,15, col="red", lty=2)
points(billengross_data$coefficient, billengross_data$covariate_numeric, pch=18, cex=1.2)
segments(billengross_data$lower_ci, billengross_data$covariate_numeric, billengross_data$upper_ci, billengross_data$covariate_numeric)
#Enacted
billenacted_data <- subset(action_data, model=="numbillenacted")
plot (billenacted_data$coefficient, billenacted_data$covariate_numeric, family="serif", pch=NA, cex=1.2, main = "Enacted", xlab=NA, bty="n", ylab=NA, yaxt="n", xlim = c(-.5, 2.5), ylim=c(1,14))
segments(0,0,0,15, col="red", lty=2)
points(billenacted_data$coefficient, billenacted_data$covariate_numeric, pch=18, cex=1.2)
segments(billenacted_data$lower_ci, billenacted_data$covariate_numeric, billenacted_data$upper_ci, billenacted_data$covariate_numeric)

#Bill Cosponsorship (Figure 2)
#ORIGINAL COSPONSORSHIP
ogcosponintro_data <- subset(action_data, model=="numogcospon")
plot (ogcosponintro_data$coefficient, ogcosponintro_data$covariate_numeric, family="serif", pch=NA, cex=1.2, main = "Introduced", xlab=NA, bty="n", ylab=NA, yaxt="n",xlim = c(-20, 25), ylim=c(1,14))
segments(0,0,0,15, col="red", lty=2)
points(ogcosponintro_data$coefficient, ogcosponintro_data$covariate_numeric, pch=18, cex=1.2)
segments(ogcosponintro_data$lower_ci, ogcosponintro_data$covariate_numeric, ogcosponintro_data$upper_ci, ogcosponintro_data$covariate_numeric)
text (-21, 14, labels="Majority", pos=2, family="serif", xpd=NA)
text (-21, 13, labels="Democrat", pos=2, family="serif", xpd=NA)
text (-21, 12, labels="Female", pos=2, family="serif", xpd=NA)
text (-21, 11, labels="Black", pos=2, family="serif", xpd=NA)
text (-21, 10, labels="Hispanic", pos=2, family="serif", xpd=NA)
text (-21, 9, labels="AAPI", pos=2, family="serif", xpd=NA)
text (-21, 8, labels="Native American", pos=2, family="serif", xpd=NA)
text (-21, 7, labels="Education", pos=2, family="serif", xpd=NA)
text (-21, 6, labels="Seniority", pos=2, family="serif", xpd=NA)
text (-21, 5, labels="Chair", pos=2, family="serif", xpd=NA)
text (-21, 4, labels="Power Committee", pos=2, family="serif", xpd=NA)
text (-21, 3, labels="State Leg Experience", pos=2, family="serif", xpd=NA)
text (-21, 2, labels="Majority Leadership", pos=2, family="serif", xpd=NA)
text (-21, 1, labels="Minority Leadership", pos=2, family="serif", xpd=NA)
#Engrossed
ogcosponengross_data <- subset(action_data, model=="numogcosponengross")
plot (ogcosponengross_data$coefficient, ogcosponengross_data$covariate_numeric, family="serif", pch=NA, cex=1.2, main = "Engrossed", xlab=NA, bty="n", ylab=NA, yaxt="n", xlim = c(-4, 6), ylim=c(1,14))
segments(0,0,0,15, col="red", lty=2)
points(ogcosponengross_data$coefficient, ogcosponengross_data$covariate_numeric, pch=18, cex=1.2)
segments(ogcosponengross_data$lower_ci, ogcosponengross_data$covariate_numeric, ogcosponengross_data$upper_ci, ogcosponengross_data$covariate_numeric)
#Enacted
ogcosponenacted_data <- subset(action_data, model=="numogcosponenacted")
plot (ogcosponenacted_data$coefficient, ogcosponenacted_data$covariate_numeric, family="serif", pch=NA, cex=1.2, main = "Enacted", xlab=NA, bty="n", ylab=NA, yaxt="n", xlim = c(-2, 2), ylim=c(1,14))
segments(0,0,0,15, col="red", lty=2)
points(ogcosponenacted_data$coefficient, ogcosponenacted_data$covariate_numeric, pch=18, cex=1.2)
segments(ogcosponenacted_data$lower_ci, ogcosponenacted_data$covariate_numeric, ogcosponenacted_data$upper_ci, ogcosponenacted_data$covariate_numeric)
#ADDITIONAL COSPONSORSHIP 
cosponintro_data <- subset(action_data, model=="numcospon")
plot (cosponintro_data$coefficient, cosponintro_data$covariate_numeric, family="serif", pch=NA, cex=1.2, main = "Introduced", xlab=NA, bty="n", ylab=NA, yaxt="n", xlim = c(-50, 50), ylim=c(1,14))
segments(0,0,0,15, col="red", lty=2)
points(cosponintro_data$coefficient, cosponintro_data$covariate_numeric, pch=18, cex=1.2)
segments(cosponintro_data$lower_ci, cosponintro_data$covariate_numeric, cosponintro_data$upper_ci, cosponintro_data$covariate_numeric)
text (-50, 14, labels="Majority", pos=2, family="serif", xpd=NA)
text (-50, 13, labels="Democrat", pos=2, family="serif", xpd=NA)
text (-50, 12, labels="Female", pos=2, family="serif", xpd=NA)
text (-50, 11, labels="Black", pos=2, family="serif", xpd=NA)
text (-50, 10, labels="Hispanic", pos=2, family="serif", xpd=NA)
text (-50, 9, labels="AAPI", pos=2, family="serif", xpd=NA)
text (-50, 8, labels="Native American", pos=2, family="serif", xpd=NA)
text (-50, 7, labels="Education", pos=2, family="serif", xpd=NA)
text (-50, 6, labels="Seniority", pos=2, family="serif", xpd=NA)
text (-50, 5, labels="Chair", pos=2, family="serif", xpd=NA)
text (-50, 4, labels="Power Committee", pos=2, family="serif", xpd=NA)
text (-50, 3, labels="State Leg Experience", pos=2, family="serif", xpd=NA)
text (-50, 2, labels="Majority Leadership", pos=2, family="serif", xpd=NA)
text (-50, 1, labels="Minority Leadership", pos=2, family="serif", xpd=NA)
#Engrossed
cosponengross_data <- subset(action_data, model=="numcosponengross")
plot (cosponengross_data$coefficient, cosponengross_data$covariate_numeric, family="serif", pch=NA, cex=1.2, main = "Engrossed", xlab=NA, bty="n", ylab=NA, yaxt="n", xlim = c(-5, 10), ylim=c(1,14))
segments(0,0,0,15, col="red", lty=2)
points(cosponengross_data$coefficient, cosponengross_data$covariate_numeric, pch=18, cex=1.2)
segments(cosponengross_data$lower_ci, cosponengross_data$covariate_numeric, cosponengross_data$upper_ci, cosponengross_data$covariate_numeric)
#Enacted
cosponenacted_data <- subset(action_data, model=="numcosponenacted")
plot (cosponenacted_data$coefficient, cosponenacted_data$covariate_numeric, family="serif", pch=NA, cex=1.2, main = "Enacted", xlab=NA, bty="n", ylab=NA, yaxt="n", xlim = c(-2, 3), ylim=c(1,14))
segments(0,0,0,15, col="red", lty=2)
points(cosponenacted_data$coefficient, cosponenacted_data$covariate_numeric, pch=18, cex=1.2)
segments(cosponenacted_data$lower_ci, cosponenacted_data$covariate_numeric, cosponenacted_data$upper_ci, cosponenacted_data$covariate_numeric)

#Amendments (Figure 3)
amendintro_data <- subset(action_data, model=="numamendintro")
plot (amendintro_data$coefficient, amendintro_data$covariate_numeric, family="serif", pch=NA, cex=1.2, main = "Introduced", xlab=NA, bty="n", ylab=NA, yaxt="n", xlim = c(-2, 2), ylim=c(1,14))
segments(0,0,0,15, col="red", lty=2)
points(amendintro_data$coefficient, amendintro_data$covariate_numeric, pch=18, cex=1.2)
segments(amendintro_data$lower_ci, amendintro_data$covariate_numeric, amendintro_data$upper_ci, amendintro_data$covariate_numeric)
text (-2, 14, labels="Majority", pos=2, family="serif", xpd=NA)
text (-2, 13, labels="Democrat", pos=2, family="serif", xpd=NA)
text (-2, 12, labels="Female", pos=2, family="serif", xpd=NA)
text (-2, 11, labels="Black", pos=2, family="serif", xpd=NA)
text (-2, 10, labels="Hispanic", pos=2, family="serif", xpd=NA)
text (-2, 9, labels="AAPI", pos=2, family="serif", xpd=NA)
text (-2, 8, labels="Native American", pos=2, family="serif", xpd=NA)
text (-2, 7, labels="Education", pos=2, family="serif", xpd=NA)
text (-2, 6, labels="Seniority", pos=2, family="serif", xpd=NA)
text (-2, 5, labels="Chair", pos=2, family="serif", xpd=NA)
text (-2, 4, labels="Power Committee", pos=2, family="serif", xpd=NA)
text (-2, 3, labels="State Leg Experience", pos=2, family="serif", xpd=NA)
text (-2, 2, labels="Majority Leadership", pos=2, family="serif", xpd=NA)
text (-2, 1, labels="Minority Leadership", pos=2, family="serif", xpd=NA)
#Engrossed
amendengross_data <- subset(action_data, model=="numamendengross")
plot (amendengross_data$coefficient, amendengross_data$covariate_numeric, family="serif", pch=NA, cex=1.2, main = "Engrossed", xlab=NA, bty="n", ylab=NA, yaxt="n", xlim = c(-1, 2.5), ylim=c(1,14))
segments(0,0,0,15, col="red", lty=2)
points(amendengross_data$coefficient, amendengross_data$covariate_numeric, pch=18, cex=1.2)
segments(amendengross_data$lower_ci, amendengross_data$covariate_numeric, amendengross_data$upper_ci, amendengross_data$covariate_numeric)
#Enacted
amendenacted_data <- subset(action_data, model=="numamendenacted")
plot (amendenacted_data$coefficient, amendenacted_data$covariate_numeric, family="serif", pch=NA, cex=1.2, main = "Enacted", xlab=NA, bty="n", ylab=NA, yaxt="n", xlim = c(-.5, 1), ylim=c(1,14))
segments(0,0,0,15, col="red", lty=2)
points(amendenacted_data$coefficient, amendenacted_data$covariate_numeric, pch=18, cex=1.2)
segments(amendenacted_data$lower_ci, amendenacted_data$covariate_numeric, amendenacted_data$upper_ci, amendenacted_data$covariate_numeric)


#Bill Influence (Figure 5 )
influintro_data <- subset(action_data, model=="numinfluintro")
plot (influintro_data$coefficient, influintro_data$covariate_numeric, family="serif", pch=NA, cex=1.2, main = "Introduced", xlab=NA, bty="n", ylab=NA, yaxt="n", xlim = c(-1, 4), ylim=c(1,14))
segments(0,0,0,15, col="red", lty=2)
points(influintro_data$coefficient, influintro_data$covariate_numeric, pch=18, cex=1.2)
segments(influintro_data$lower_ci, influintro_data$covariate_numeric, influintro_data$upper_ci, influintro_data$covariate_numeric)
text (-1, 14, labels="Majority", pos=2, family="serif", xpd=NA)
text (-1, 13, labels="Democrat", pos=2, family="serif", xpd=NA)
text (-1, 12, labels="Female", pos=2, family="serif", xpd=NA)
text (-1, 11, labels="Black", pos=2, family="serif", xpd=NA)
text (-1, 10, labels="Hispanic", pos=2, family="serif", xpd=NA)
text (-1, 9, labels="AAPI", pos=2, family="serif", xpd=NA)
text (-1, 8, labels="Native American", pos=2, family="serif", xpd=NA)
text (-1, 7, labels="Education", pos=2, family="serif", xpd=NA)
text (-1, 6, labels="Seniority", pos=2, family="serif", xpd=NA)
text (-1, 5, labels="Chair", pos=2, family="serif", xpd=NA)
text (-1, 4, labels="Power Committee", pos=2, family="serif", xpd=NA)
text (-1, 3, labels="State Leg Experience", pos=2, family="serif", xpd=NA)
text (-1, 2, labels="Majority Leadership", pos=2, family="serif", xpd=NA)
text (-1, 1, labels="Minority Leadership", pos=2, family="serif", xpd=NA)
#Engrossed
influengross_data <- subset(action_data, model=="numinfluengross")
plot (influengross_data$coefficient, influengross_data$covariate_numeric, family="serif", pch=NA, cex=1.2, main = "Engrossed", xlab=NA, bty="n", ylab=NA, yaxt="n", xlim = c(-.2, .6), ylim=c(1,14))
segments(0,0,0,15, col="red", lty=2)
points(influengross_data$coefficient, influengross_data$covariate_numeric, pch=18, cex=1.2)
segments(influengross_data$lower_ci, influengross_data$covariate_numeric, influengross_data$upper_ci, influengross_data$covariate_numeric)
#Enacted
influenacted_data <- subset(action_data, model=="numinfluenacted")
plot (influenacted_data$coefficient, influenacted_data$covariate_numeric, family="serif", pch=NA, cex=1.2, main = "Enacted", xlab=NA, bty="n", ylab=NA, yaxt="n", xlim = c(-.2, .4), ylim=c(1,14))
segments(0,0,0,15, col="red", lty=2)
points(influenacted_data$coefficient, influenacted_data$covariate_numeric, pch=18, cex=1.2)
segments(influenacted_data$lower_ci, influenacted_data$covariate_numeric, influenacted_data$upper_ci, influenacted_data$covariate_numeric)


#Appendix Figures
##Figure A1. Frequency of Lawmaking Actions
library(ggplot2)
library(haven)
data <- read_dta("filepath/Action Counts.dta")
#(subset of LawProM.dta)

#Sponsorship - Introduced
ggplot(data, aes(x = numbillintro))+ geom_histogram(bins = 40, color = "black", fill = "gray") +
  geom_vline(aes(xintercept = mean(numbillintro)), 
             linetype = "dashed", size = 0.6) +ggtitle("Sponsorship - Introduced") +xlab("Frequency of Action") +ylab("Number of Legislators") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                      panel.background = element_blank(), axis.line = element_line(colour = "black"), plot.title = element_text(hjust = 0.5))
#Sponsorship - Engrossed
ggplot(data, aes(x = numbillengrossed))+ geom_histogram(bins = 40, color = "black", fill = "gray") +
  geom_vline(aes(xintercept = mean(numbillengrossed)), 
             linetype = "dashed", size = 0.6) +ggtitle("Sponsorship - Engrossed") +xlab("Frequency of Action") +ylab("Number of Legislators") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                                       panel.background = element_blank(), axis.line = element_line(colour = "black"), plot.title = element_text(hjust = 0.5))
#Sponsorship - Enacted
ggplot(data, aes(x = numbillenacted))+ geom_histogram(bins = 40, color = "black", fill = "gray") +
  geom_vline(aes(xintercept = mean(numbillenacted)), 
             linetype = "dashed", size = 0.6) +ggtitle("Sponsorship - Enacted") +xlab("Frequency of Action") +ylab("Number of Legislators") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                                      panel.background = element_blank(), axis.line = element_line(colour = "black"), plot.title = element_text(hjust = 0.5))

#Original Cosponsorship - Introduced
ggplot(data, aes(x = numcosponOG))+ geom_histogram(bins = 40, color = "black", fill = "gray") +
  geom_vline(aes(xintercept = mean(numcosponOG)), 
             linetype = "dashed", size = 0.6) +ggtitle("Original Cosponsorship - Introduced") +xlab("Frequency of Action") +ylab("Number of Legislators") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                                       panel.background = element_blank(), axis.line = element_line(colour = "black"), plot.title = element_text(hjust = 0.5))
#Original Cosponsorship - Engrossed
ggplot(data, aes(x = numcosponengrossedOG))+ geom_histogram(bins = 40, color = "black", fill = "gray") +
  geom_vline(aes(xintercept = mean(numcosponengrossedOG)), 
             linetype = "dashed", size = 0.6) +ggtitle("Original Cosponsorship - Engrossed") +xlab("Frequency of Action") +ylab("Number of Legislators") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                                      panel.background = element_blank(), axis.line = element_line(colour = "black"), plot.title = element_text(hjust = 0.5))
#Original Cosponsorship - Enacted
ggplot(data, aes(x = numcosponenactedOG))+ geom_histogram(bins = 40, color = "black", fill = "gray") +
  geom_vline(aes(xintercept = mean(numcosponenactedOG)), 
             linetype = "dashed", size = 0.6) +ggtitle("Original Cosponsorship - Enacted") +xlab("Frequency of Action") +ylab("Number of Legislators") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                                    panel.background = element_blank(), axis.line = element_line(colour = "black"), plot.title = element_text(hjust = 0.5))

#Additional Cosponsorship - Introduced
ggplot(data, aes(x = numcospon))+ geom_histogram(bins = 40, color = "black", fill = "gray") +
  geom_vline(aes(xintercept = mean(numcospon)), 
             linetype = "dashed", size = 0.6) +ggtitle("Additional Cosponsorship - Introduced") +xlab("Frequency of Action") +ylab("Number of Legislators") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                                                  panel.background = element_blank(), axis.line = element_line(colour = "black"), plot.title = element_text(hjust = 0.5))
#Additional Cosponsorship - Engrossed
ggplot(data, aes(x = numcosponengrossed))+ geom_histogram(bins = 40, color = "black", fill = "gray") +
  geom_vline(aes(xintercept = mean(numcosponengrossed)), 
             linetype = "dashed", size = 0.6) +ggtitle("Additional Cosponsorship - Engrossed") +xlab("Frequency of Action") +ylab("Number of Legislators") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                                                 panel.background = element_blank(), axis.line = element_line(colour = "black"), plot.title = element_text(hjust = 0.5))
#Additional Cosponsorship - Enacted
ggplot(data, aes(x = numcosponenacted))+ geom_histogram(bins = 40, color = "black", fill = "gray") +
  geom_vline(aes(xintercept = mean(numcosponenacted)), 
             linetype = "dashed", size = 0.6) +ggtitle("Additional Cosponsorship - Enacted") +xlab("Frequency of Action") +ylab("Number of Legislators") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                                               panel.background = element_blank(), axis.line = element_line(colour = "black"), plot.title = element_text(hjust = 0.5))

#Amendment - Introduced
ggplot(data, aes(x = numamendintro))+ geom_histogram(bins = 40, color = "black", fill = "gray") +
  geom_vline(aes(xintercept = mean(numamendintro)), 
             linetype = "dashed", size = 0.6) +ggtitle("Amendment - Introduced") +xlab("Frequency of Action") +ylab("Number of Legislators") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                                                    panel.background = element_blank(), axis.line = element_line(colour = "black"), plot.title = element_text(hjust = 0.5))
#Amendment - Engrossed
ggplot(data, aes(x = numamendengrossed))+ geom_histogram(bins = 40, color = "black", fill = "gray") +
  geom_vline(aes(xintercept = mean(numamendengrossed)), 
             linetype = "dashed", size = 0.6) +ggtitle("Amendment - Engrossed") +xlab("Frequency of Action") +ylab("Number of Legislators") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                                                   panel.background = element_blank(), axis.line = element_line(colour = "black"), plot.title = element_text(hjust = 0.5))
#Amendment - Enacted
ggplot(data, aes(x = numamendenacted))+ geom_histogram(bins = 40, color = "black", fill = "gray") +
  geom_vline(aes(xintercept = mean(numamendenacted)), 
             linetype = "dashed", size = 0.6) +ggtitle("Amendment - Enacted") +xlab("Frequency of Action") +ylab("Number of Legislators") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                                                 panel.background = element_blank(), axis.line = element_line(colour = "black"), plot.title = element_text(hjust = 0.5))
#Influence - Introduced
ggplot(data, aes(x = numinfluintrohouse))+ geom_histogram(bins = 40, color = "black", fill = "gray") +
  geom_vline(aes(xintercept = mean(numinfluintrohouse)), 
             linetype = "dashed", size = 0.6) +ggtitle("Influence - Introduced") +xlab("Frequency of Action") +ylab("Number of Legislators") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                                     panel.background = element_blank(), axis.line = element_line(colour = "black"), plot.title = element_text(hjust = 0.5))
#Influence - Engrossed
ggplot(data, aes(x = numinfluengrosshouse))+ geom_histogram(bins = 40, color = "black", fill = "gray") +
  geom_vline(aes(xintercept = mean(numinfluengrosshouse)), 
             linetype = "dashed", size = 0.6) +ggtitle("Influence - Engrossed") +xlab("Frequency of Action") +ylab("Number of Legislators") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                                    panel.background = element_blank(), axis.line = element_line(colour = "black"), plot.title = element_text(hjust = 0.5))
#Influence - Enacted
ggplot(data, aes(x = numinfluenactedhouse))+ geom_histogram(bins = 40, color = "black", fill = "gray") +
  geom_vline(aes(xintercept = mean(numinfluenactedhouse)), 
             linetype = "dashed", size = 0.6) +ggtitle("Influence - Enacted") +xlab("Frequency of Action") +ylab("Number of Legislators") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                                  panel.background = element_blank(), axis.line = element_line(colour = "black"), plot.title = element_text(hjust = 0.5))



##Figure A2. Correlation of Measures of Productivity
install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
library(haven)
my_data <- read_stata("filepath/Scores for Correlation.dta")
#(subset of LawProM.dta)

chart.Correlation(my_data, histogram=FALSE, pch=19)
