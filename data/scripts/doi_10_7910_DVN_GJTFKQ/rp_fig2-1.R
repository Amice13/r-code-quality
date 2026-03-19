library(foreign)
library(MASS)
library(lmtest)
library(car)
library(survey) #used for anlyses with survey weights



#read in csv file
d <- read.csv('rp_data_rr.csv')

###########################################
#model with all cases, column 1 of table 1#
###########################################

#pooled model, survey weights assigned via weight command
m1 <- lm(veto_app ~ pres_cong_diff + dem + rep + ideol + col_grad + news_int, weights = weight, d)

#calculate robust standard errors
o1 <- coeftest(m1, vcov = hccm(m1))


##############################################
#comparison across knowledge of party control#
##############################################


#do not know
m2 <- lm(veto_app ~ pres_cong_diff + dem + rep + ideol + col_grad + news_int, weights = weight, subset(d, chamber_know < 2 ))


#know
m3 <- lm(veto_app ~ pres_cong_diff  + dem + rep + ideol + col_grad + news_int , weights = weight, subset(d, chamber_know == 2) )


#calculate robust standard errors
o2 <- coeftest(m2, vcov = hccm(m2))
o3 <- coeftest(m3, vcov = hccm(m3))


##################
#create dot plots#
##################

#model 1
c1 <- o1[,1]
se1 <- o1[,2]
lo1 <- c1 - se1*qnorm(0.95)
hi1 <- c1 + se1*qnorm(0.95)

#model 2
c2 <- o2[,1]
se2 <- o2[,2]
lo2 <- c2 - se2*qnorm(0.95)
hi2 <- c2 + se2*qnorm(0.95)

#model 3
c3 <- o3[,1]
se3 <- o3[,2]
lo3 <- c3 - se3*qnorm(0.95)
hi3 <- c3 + se3*qnorm(0.95)

#adjust plot margins 
par(mar = c(4.5, 7, 1, 1))

#create ruler for plot
r <- seq(2, 12, 2)
adj <- 0.5

#create plot
plot(c2[2:7], r, type = 'n', xaxt='n', yaxt = 'n', xlab='', ylab='', ylim = c(1,13), xlim = c(-1, 1.4), bty= 'n')
abline(v = 0)

#add coefs from model 1
points(x=c1[2:7], y=rev(r)+adj, pch = 16)
points(x=c2[2:7], y=rev(r), pch = 15, col ='grey')
points(x=c3[2:7], y=rev(r)-adj, pch = 18)
#points(x=c1[8], y=2, pch = 16) #know party control

#add CIS
segments(x0 = hi1[2:7], x1 = lo1[2:7], y0 = rev(r)+adj, y1 = rev(r)+adj)
segments(x0 = hi2[2:7], x1 = lo2[2:7], y0 = rev(r), y1 = rev(r), col = "grey")
segments(x0 = hi3[2:7], x1 = lo3[2:7], y0 = rev(r)-adj, y1 = rev(r)-adj)
segments(x0 = hi3[2:7], x1 = lo3[2:7], y0 = rev(r)-adj, y1 = rev(r)-adj)
#segments(x0 = hi1[8], x1 = lo1[8], y0 = 2, y1 = 2)


#add x-axis
axis(at = seq(-1, 1.4, 0.2) , side = 1)


#add margin text
mtext("Coefficients", side = 1, font = 2, line = 2.75)
lab <- c("Approval\nDifference", "Democrat", "Republican", "Ideology", "College\nGraduate", "News\nConsumption")
axis(at = rev(r), labels = lab, las = 2, side = 2, tick = FALSE)

#add legend
legend('bottomright', bty = 'n', legend = c('All Respondents', 'None or One Correct', 'Both Correct'), pch = c(16, 15, 18), col = c('black', 'grey', 'black'), lty = c(1,1,1))

