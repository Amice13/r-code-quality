library(foreign)
library(MASS)
library(lmtest)
library(car)
library(survey) #used for anlyses with survey weights
library(oglmx)
library(vcd)
library(vcdExtra)



#read in csv file
d <- read.csv('rp_data_rr.csv')


#sets up data based on survey design and weights 
s <- svydesign(id = ~0, data = d, weight = ~weight)

##########################
#PANEL 1: All RESPONDENTS#
##########################

#calculate proprotion of respondents in each category of veto approval, uses survey weights 
t1 <- prop.table(svytable(~veto_app, s))*100

#categories
r <- 1:5
nm <- c('Strongly\nDisagree', 'Somewhat\nDisagree', 'Neither Agree\nnor Disagree', 'Somewhat\nAgree', 'Strongly\nAgree')

#create barplot
plot(r, t1, ylim = c(0,50), xlab = '', ylab = '', xaxt='n', yaxt ='n', type = 'n')
lines(r, t1, lwd = 3)


#draw in axes
axis(side = 1, at = 1:5, labels = nm, line = 0.5, tick = FALSE)
axis(side = 1, at = 1:5, labels = FALSE, tick = TRUE)
axis(side = 2, at = seq(0,50, 5), labels = seq(0,50, 5), las = 2)

#add margin text
mtext("Proportion", side = 2, line = 2.75)
mtext(side = 3, text = "All Respondents", line = 0.5, font=2)


#####################################
#PANEL 2: DEMOCRATS AND REPUBLICANS#
#####################################

#republicans
t2 <- prop.table(svytable(~veto_app, subset(s, rep == 1)))*100

#democrats
t3 <- prop.table(svytable(~veto_app, subset(s, dem == 1)))*100

r <- 1:5

#create plot
plot(r, t2, ylim = c(0,50), xlab = '', ylab = '', xaxt='n', yaxt ='n', type = 'n')

#draw line for republicans
lines(r, t2, lwd = 3)

#draw line for democrats
lines(r, t3, lwd = 3, col ='grey')

#categories
nm <- c('Strongly\nDisagree', 'Somewhat\nDisagree', 'Neither Agree\nnor Disagree', 'Somewhat\nAgree', 'Strongly\nAgree')

#draw axes
axis(side = 1, at = 1:5, labels = nm, line = 0.5, tick = FALSE)
axis(side = 1, at = 1:5, labels = FALSE, tick = TRUE)
axis(side = 2, at = seq(0,50, 5), labels = seq(0,50, 5), las = 2)

#create legend
legend('topleft', legend = c('Rep.', 'Dem.'), lwd = c(3,3), lty= c(1,1), bty = 'n', col = c('black', 'grey'))

#add margin text
mtext("Proportion", side = 2, line = 2.75)
mtext(side = 3, text = "Partisanship", line = 0.5, font=2)


#################################
#PAENL 3: PRESIDENTIAL APPROVAL#
################################

#presidential approval 
t4 <- prop.table(svytable(~veto_app + pres_bin, s), 2)*100

r <- 1:5

#creates plot
plot(r, t4[,1], ylim = c(0,50), xlab = '', ylab = '', xaxt='n', yaxt ='n', type = 'n')
#lines(r, t1, lwd = 3)

#draw line for respondetns who disapprove of pres job performance 
lines(r, t4[,1], lwd = 3)

#draw line for respondetns who approve of pres job performance
lines(r, t4[,2], lwd = 3, col ='grey')

#categories
nm <- c('Strongly\nDisagree', 'Somewhat\nDisagree', 'Neither Agree\nnor Disagree', 'Somewhat\nAgree', 'Strongly\nAgree')

#draw axes
axis(side = 1, at = 1:5, labels = nm, line = 0.5, tick = FALSE)
axis(side = 1, at = 1:5, labels = FALSE, tick = TRUE)
axis(side = 2, at = seq(0,50, 5), labels = seq(0,50, 5), las = 2)


#create legend
legend('topleft', legend = c('Approve', 'Disapprove'), lwd = c(3,3), lty= c(1,1), bty = 'n', col = c('grey', 'black') )

#add margin text
mtext("Proportion", side = 2, line = 2.75)
mtext(side = 3, text = "Presidential Approval", line = 0.5, font=2)

