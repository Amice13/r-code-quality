#########################################################
#### Disrupting Regulation and Regulating Disrupting ####
#########################################################

################################################
## Ruth Collier, VB Dubal, Christopher Carter ##
################################################


library(foreign)
library(ggplot2)
library(scales)


##################
#### Table 3 #####
##################

setwd("/Users/Chris/Downloads/")
cat <- read.csv("Disrupting regulation/Data/perc_cat.csv")

##Create df with categories of regulation##
cat_df <- cbind.data.frame(cat$X..of.consumer.protection, 
                           cat$X..of.Other.competition, 
                           cat$X..of.safety.w..insurance)

tab1 <- as.table(cor(cat_df))


rownames(tab1) <- c("Consumer protection", "Other competition", "Safety")
colnames(tab1) <- c("Consumer protection", "Other competition", "Safety")

tab1 <- round(tab1,2)

grid.table(tab1)

##############
## Figure 1 ##
##############

##Data from Warshaw and Tausanovitch 
#http://americanideologyproject.com/

##Read in state data
states <- read.csv("Disrupting regulation/Data/states_estimates.csv")

##Ideology score for states
score_s <- c(states$mrp_estimate[states$abb=="NY"], 
             states$mrp_estimate[states$abb=="PA"], 
             states$mrp_estimate[states$abb=="PA"], 
             states$mrp_estimate[states$abb=="TX"], 
             states$mrp_estimate[states$abb=="TX"], 
             states$mrp_estimate[states$abb=="TX"], 
             states$mrp_estimate[states$abb=="WA"], 
             states$mrp_estimate[states$abb=="CA"], 
             states$mrp_estimate[states$abb=="IL"]) 

##Read in city data   
cities <- read.csv("Disrupting regulation/Data/cities_estimates.csv")

##Create city names
city <- c("New York City", "Philadelphia", "Pittsburgh", 
          "Austin", "San Antonio", "Houston", "Seattle", 
          "San Francisco", "Chicago")

##Ideology score for cities
score_c <- c(cities$mrp_estimate[cities$city=="New York"], 
             cities$mrp_estimate[cities$city=="Philadelphia"],
             cities$mrp_estimate[cities$city=="Pittsburgh"], 
             cities$mrp_estimate[cities$city=="Austin"], 
             cities$mrp_estimate[cities$city=="San Antonio"], 
             cities$mrp_estimate[cities$city=="Houston"], 
             cities$mrp_estimate[cities$city=="Seattle"], 
             cities$mrp_estimate[cities$city=="San Francisco"], 
             cities$mrp_estimate[cities$city=="Chicago"]) 

##Combine city and state dataframe

combined_df <- cbind.data.frame(score_s, city, score_c)


combined_df$city <- factor(combined_df$city, levels = combined_df$city)

combined_df$diff <- round(score_s-score_c,3)

combined_df$diff <- factor(combined_df$diff, levels = combined_df$diff)


p=ggplot(combined_df, aes(x=combined_df$city)) + 
  geom_point(aes(y=as.numeric(as.character(combined_df$score_s)), shape="State"), size=5) +
  geom_point(aes(y=as.numeric(as.character(combined_df$score_c)), shape="City"), size=5) + 
  scale_y_continuous(limits=c(-1,1)) + 

##Add axes and main label
  labs(title="", x="", y="Conservatism score") + 

##Change theme
  theme_bw()+geom_blank()+theme(panel.grid.major.x = element_blank(),
                                  panel.grid.minor.x = element_blank()) + 

# Remove title for all legends
  theme(legend.title=element_blank(), text = element_text(size=20)) + 


##Plot vertical line at 0
  geom_hline(yintercept=0, lty=2, col="slateblue") + 

  coord_flip()

  print(p)

  
################
### Figure 2 ###
################
  
# Read in total regulation csv
total <- read.csv("Disrupting regulation/Data/total_regulations.csv")

  
par(mar=c(5,6,4,1)+0.1)
  
total <- hist(total$total_regs, breaks=15, main="", 
            ylab="Number of states", 
            xlab="Total number of regulations", 
            xaxt="n", cex.lab=2, cex.axis=1.5)

# Add x axis labels
axis(side=1, at=total$mids, labels=seq(1,15), cex.axis=1.25)


################
### Figure 3 ###
################

##Read in party support csv

party_support <- read.csv("Disrupting regulation/Data/rep_dem.csv")

# Plot bipartisan support

par(mar=c(5,6,4,1)+0.5)

plot(party_support$Proportion.of.Democrats.supporting, 
     party_support$Proportion.of.Republicans.supporting, cex=1, 
     pch=19, xlab="Proportion of Democrats Supporting", 
     ylab="Proportion of\n Republicans Supporting", 
     xlim=c(0, 1), cex.axis=1.45, cex.lab=1.75)
abline(v=0.7, lty=3)
abline(h=0.7, lty=3)


##############
## Figure 4 ##
##############

# Read in insurance file

insurance <- read.csv("Disrupting regulation/Data/insurance.csv")

insurance$more_less[insurance$more_less==-1] <- "Less"
insurance$more_less[insurance$more_less==0] <- "Same"
insurance$more_less[insurance$more_less==1] <- "More"

insurance$more_less <- factor(insurance$more_less, 
                              c("Less", "Same", "More"))

# Generate bar plot
barplot(table(insurance$more_less), width= 1, space=3, xlab="", 
        ylab="Number of states", cex.lab=1, cex.axis=1)

################
## Appendix 1 ##
################
#Partial data from Shor, Boris; McCarty, Nolan, 2015, 
#"Aggregate State Legislator Shor-McCarty Ideology Data, 
#June 2015 update", https://doi.org/10.7910/DVN/K7ELHW, 
#Harvard Dataverse, V1, UNF:6:l5O+/whNdgWGB1Vt4nEheA==


app <- read.csv("Disrupting regulation/Data/appendix.csv")


par(mfrow=c(1,2))
par(xpd=NA)
plot(app$Total_regs~app$house_id, cex=0.5, pch=19, 
     ylab="Total Regulations", 
     xlab="2014 House Ideology\n (liberal-conservative)")

r <- cor(app$Total_regs, app$house_id, use="complete")

mylabel = bquote(r == .(format(r, digits = 2)))
text(x = 0.9, y = 17, labels = mylabel)
plot(app$Total_regs~app$cit_pref, cex=0.5, pch=19, 
     ylab="Total Regulations", 
     xlab="Citizen Ideology\n (liberal-conservative)")

r <- cor(app$Total_regs, app$cit_pref, use="complete")

mylabel = bquote(r == .(format(r, digits = 2)))
text(x = .25, y = 17, labels = mylabel)
dev.off()

################
## Appendix 1 ##
################

#Read in appendix df


par(mfrow=c(1,2))
par(xpd=NA)


plot(app$Total_regs~app$house_id, cex=0.5, pch=19, 
     ylab="Total Regulations", 
     xlab="2014 House Ideology\n (liberal-conservative)")

r <- cor(app$Total_regs, app$house_id, use="complete")

mylabel = bquote(r == .(format(r, digits = 2)))

text(x = 0.9, y = 17, labels = mylabel)

plot(app$Total_regs~app$cit_pref, cex=0.5, pch=19, 
     ylab="Total Regulations", 
     xlab="Citizen Ideology\n (liberal-conservative)")

r <- cor(app$Total_regs, app$cit_pref, use="complete")

mylabel = bquote(r == .(format(r, digits = 2)))
text(x = .25, y = 17, labels = mylabel)

################
## Appendix 2 ##
################


par(mfrow=c(1,2))
par(xpd=NA)
plot(app$Total_regs~app$urbanization, cex=0.5, pch=19, 
     ylab="Total Regulations", xlab="Urbanization rate", cex.lab=1.1)
r <- cor(app$Total_regs, app$urbanization, use="complete")

mylabel = bquote(r == .(format(r, digits = 2)))
text(x = 0.9, y = 17, labels = mylabel)

plot(app$Total_regs~app$primacy, cex=0.5, pch=19, 
     ylab="Total Regulations", xlab="Largest city as %\n of 
     total state pop.", cex.lab=1.1)
r <- cor(app$Total_regs, app$primacy, use="complete")

mylabel = bquote(r == .(format(r, digits = 2)))
text(x = 0.4, y = 17, labels = mylabel)
dev.off()

################
## Appendix 3 ##
################

#Remove NAs from dataset

app <- app[is.na(app$Total_regs)==FALSE & is.na(app$uni)==FALSE,]


par(mfrow=c(1,2), mar = c(10, 6, 2, 2), mgp = c(5, 1, 0))
par(xpd=NA)

plot(app$Total_regs~app$perc_rep, cex=0.5, pch=19, 
     ylab="Total Regulations", 
     xlab="Percent of Republicans in state house (2015)")

r <- cor(app$Total_regs, app$perc_rep, use="complete")


## Create mean values
uni_cat <- unique(app$uni)

uni_cat <- uni_cat[-7]

ur <- mean(app$Total_regs[app$uni=="UR"], na.rm=TRUE)

twor_nogov <- mean(app$Total_regs[app$uni=="2R-No gov"], na.rm=TRUE)

ud <- mean(app$Total_regs[app$uni=="UD"], na.rm=TRUE)

twod_nogov <- mean(app$Total_regs[app$uni=="2D-No gov"], na.rm=TRUE)

twod <- mean(app$Total_regs[app$uni=="2D"], na.rm=TRUE)

twor <- mean(app$Total_regs[app$uni=="2R"], na.rm=TRUE)

means_uni <- c(ur, twor, twor_nogov, twod_nogov, twod, ud)

names_uni <- c("Unified (R)", "Gov and 1 House (R)", 
               "2 Houses, No Gov. (R)", "2 Houses, No Gov. (D)", 
               "Gov and 1 House (D)", "Unified (D)")

mylabel = bquote(r == .(format(r, digits = 3)))
text(x = 0.7, y = 17, labels = mylabel)

mp <- barplot(means_uni, ylab="Total Regulations", xlab="", ylim=c(0,14))
mtext("Unified/divided government by party (2015)\n(number 
      of cases above bars)", side=1, line=5.5)

app$uni <- as.character(app$uni)

##Add labels 

text(mp, par("usr")[3] - 0.025, srt = 45, adj = 1, 
     labels = names_uni, 
     xpd = TRUE, font = 1, cex=0.75)
text (0.7,8.75, length(app$Total_regs[app$uni=="UR"]))
text (1.95,10.5, length(app$Total_regs[app$uni=="2R"]))
text (3.07,6.85, length(app$Total_regs[app$uni=="2R-No gov"]))
text (4.3,14.2, length(app$Total_regs[app$uni=="2D-No gov"]))
text (5.5, 3, length(app$Total_regs[app$uni=="2D"]))
text (6.7,6.35, length(app$Total_regs[app$uni=="UD"]))

