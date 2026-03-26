##########################################################################################################################
# Replication Code for:
# Gonzalez, B. & Traunmueller, R. The Political Consequences of Wartime Sexual Violence. Evidence from a List Experiment
# Journal of Peace Research
#
# Contact: traunmueller@uni-mannheim.de
# 4.4.23
##########################################################################################################################

library(foreign)
library(list)
library(denstrip)
library(psych)
library(RColorBrewer)

setwd("/YOURPATH/")
data <- read.dta("replication_code_gonzalez_traunmueller.dta", convert.factors=F)

#######################################################################################################################################################
# The List Experiment
#######################################################################################################################################################
# Sexual Assault
#a.  I won money in a lottery or competition. 
#b.  I was involved in an accident. 
#c.	I received help from a stranger.  
#d.	I was personally sexually assaulted. X

data$sexaussault <- ifelse(data$Questionnaire==2, data$J7, NA) 
data$sexaussault <- ifelse(data$Questionnaire==1, data$I7, data$J7)
data$treatment <- ifelse(data$Questionnaire==2, 1, 0)

#######################################################################################################
# TABLE A.1

design.sri <- ict.test(data$sexaussault, data$treatment, J=3) # test for design effect
xtable(design.sri$pi.table)
design.sri$p

#######################################################################################################
# TABLE A.4.1

mean(data$sexaussault[data$Questionnaire==2])
mean(data$sexaussault[data$Questionnaire==1])

mean(data$sexaussault[data$Questionnaire==2])-mean(data$sexaussault[data$Questionnaire==1])

# direct
data$sexaussault_d <- ifelse(data$D4h==77, NA, data$D4h)
data$sexaussault_d <- ifelse(data$sexaussault_d==2, 0, data$sexaussault_d)

mean(data$sexaussault_d, na.rm=T)

# direct2
data$sexaussault_d2 <- ifelse(data$D4i==77, NA, data$D4i)
data$sexaussault_d2 <- ifelse(data$sexaussault_d2==2, 0, data$sexaussault_d2)

mean(data$sexaussault_d2, na.rm=T)

mean(data$sexaussault[data$Questionnaire==2 & is.na(data$sexaussault_d)==FALSE], na.rm=T)
mean(data$sexaussault[data$Questionnaire==1 & is.na(data$sexaussault_d)==FALSE], na.rm=T)

ictreg(sexaussault ~ 1, data=data, treat="treatment", J=3, method="lm")

#######################################################################################################################################################
# Recode Covariates
#######################################################################################################################################################

data$age <- data$B1a/10
data$female <- ifelse(data$B1b==2, 1, 0)
data$tamil <- ifelse(data$B7==2 | data$B7==3, 1, 0)
data$tamil.2 <- ifelse(data$B7==2, 1, 0)
data$tamil.3 <- ifelse(data$B7==2 | data$B7==3 | data$B7==4, 1, 0)

data$edu <- data$B3a
data$edu.3[data$edu==0 |data$edu==1] <- "low"
data$edu.3[data$edu==2 |data$edu==3] <- "medium"
data$edu.3[data$edu>=4] <- "high"
data$edu.3 <- factor(data$edu.3, levels=c("low", "medium", "high"))
data$y.edu <- data$B3b

par(mfrow=c(2,1), las=1, mgp=c(1.7, .5, 0), mar=c(3,3,1,1), cex.lab=.8, cex.axis=.8, tck=-.02)
plot(jitter(data$y.edu), jitter(data$sexaussault), pch=19, cex=.3, xlab="Years of Education", ylab="Number of Items")
plot(log(data$inc), jitter(data$sexaussault), pch=19, cex=.3, xlab="Log Household Income", ylab="Number of Items")
cor(data$sexaussault, data$y.edu, use="complete.obs")
cor(data$sexaussault, data$inc, use="complete.obs")
cor.test(data$sexaussault, data$y.edu, use="complete.obs")
cor.test(data$sexaussault, data$inc, use="complete.obs")

round(prop.table(table(data$edu.3, data$sexaussault), 1), 3)
chisq.test(table( data$edu.3, data$sexaussault))

t.test(A4 ~ tamil, data=data)
t.test(y.edu ~ tamil, data=data)
round(prop.table(table( data$edu.3, data$tamil),2),2)
chisq.test(table( data$edu.3, data$tamil))


data$inc <- data$A4/10000
data$married <- ifelse(data$B2a==1 | data$B2a==2, 1, 0)

data$buddhist <- ifelse(data$B9b==1, 1, 0)
data$christian <- ifelse(data$B9b==4 | data$B9b==5, 1, 0)

# War Experience
data$H3a <- ifelse(data$H3a==1, 1, 0)
data$H3b <- ifelse(data$H3b==1, 1, 0)
data$H3c <- ifelse(data$H3c==1, 1, 0)
data$H3e <- ifelse(data$H3e==1, 1, 0)

data$clergy <- ifelse(data$H3d==1, 1, 0)
data$army <- ifelse(data$D2a==1, 1, 0) 
data$assist.army <- ifelse(data$D2b==1, 1, 0) 

data$displace <- ifelse(data$D7==1, 1, 0) 
data$n.displace <- ifelse(data$D8==999, NA, data$D8) 
data$n.displace <- ifelse(data$displace==0, 0, data$n.displace) 

data$y.displace <- ifelse(data$displace==0, 0, data$D14a)
data$y.displace <- ifelse(data$y.displace==777 | data$y.displace==999, NA, data$y.displace)

data$IDP <- ifelse(data$displace==0, 0, data$D14a)
data$IDP <- ifelse(data$IDP==777 | data$IDP==999, NA, data$IDP)

data$northern <- ifelse(data$Province==9, 1, 0)
data$eastern <- ifelse(data$Province==8, 1, 0)

data$captured <- ifelse(data$D4n==77, NA, data$D4n)
data$captured <- ifelse(data$captured==2, 0, 1)

data$D3a <- ifelse(data$D3a==1, 1, 0)
data$D3b <- ifelse(data$D3b==1, 1, 0)
data$D3c <- ifelse(data$D3c==1, 1, 0)

######################################################################################################################
# Outcomes
######################################################################################################################
data$active_a <- ifelse(data$C21a==1, 1, 0)
data$active_b <- ifelse(data$C21b==1, 1, 0)
data$active_c <- ifelse(data$C21c==1, 1, 0)
data$active_d <- ifelse(data$C21d==1, 1, 0)
data$active_e <- ifelse(data$C21e==1, 1, 0)
data$active_f <- ifelse(data$C21f==1, 1, 0)
data$active_g <- ifelse(data$C21g==1, 1, 0)
data$active_h <- ifelse(data$C21h==1, 1, 0)
data$active_i <- ifelse(data$C21i==1, 1, 0)
data$active_j <- ifelse(data$C21j==1, 1, 0)

#"Sports club or outdoor activities club",
#"Religious organisation",
#"Political parties",
#"Interest group or trade union",
#"Charity or social-welfare organisation \n (Women’s societies, Samurdhi)",
#"Environmental or human rights organisation",
#"Cultural organisation (music etc. )",
#"Leisure or hobby organisation",
#"Youth societies",
#"Other \n (e.g. Donation society, community development society)"

mean((data$active_a))
mean((data$active_b))
mean((data$active_c))
mean((data$active_d))
mean((data$active_e))
mean((data$active_f))
mean((data$active_g))
mean((data$active_h))
mean((data$active_i))
mean((data$active_j))


data$active_count <- data$active_a + data$active_b +   # Exclude Political Parties
                     data$active_d + data$active_e + data$active_f +
                     data$active_g + data$active_h + data$active_i # Exclude Other

############################################################################################
### FIGURE A.7.1.
par(mfrow=c(1,1), mar=c(3,3,3,3), mgp=c(2, .7, 0))
barplot(table(data$active_count), xlab="Number of active memberships", border="grey")

mean(data$active_count, na.rm=T)
sd(data$active_count, na.rm=T)
############################################################################################

data$vote_pres <- ifelse(data$C23_a==1, 1, 0)
data$vote_parl <- ifelse(data$C23_b==1, 1, 0)
data$vote_loc <- ifelse(data$C23_c==1, 1, 0)

data$part_a <- ifelse(data$C26_a==1, 1, 0)
data$part_b <- ifelse(data$C26_b==1, 1, 0)
data$part_c <- ifelse(data$C26_c==1, 1, 0)
data$part_d <- ifelse(data$C26_d==1, 1, 0)
data$part_e <- ifelse(data$C26_e==1, 1, 0)
data$part_f <- ifelse(data$C26_f==1, 1, 0)
data$part_g <- ifelse(data$C26_g==1, 1, 0)
data$part_h <- ifelse(data$C26_h==1, 1, 0)
data$part_i <- ifelse(data$C26_i==1, 1, 0)
data$part_j <- ifelse(data$C26_j==1, 1, 0)
data$part_k <- ifelse(data$C26_k==1, 1, 0)
data$part_l <- ifelse(data$C26_L==1, 1, 0)
data$part_m <- ifelse(data$C26_m==1, 1, 0)
data$part_n <- ifelse(data$C26_n==1, 1, 0)
data$part_o <- ifelse(data$C26_o==1, 1, 0)
data$part_p <- ifelse(data$C26_p==1, 1, 0)
data$part_q <- ifelse(data$C26_q==1, 1, 0)
data$part_r <- ifelse(data$C26_r==1, 1, 0)
data$part_s <- ifelse(data$C26_s==1, 1, 0)
data$part_t <- ifelse(data$C26_t==1, 1, 0)
data$part_u <- ifelse(data$C26_u==1, 1, 0)
data$part_v <- ifelse(data$C26_v==1, 1, 0)

round(prop.table(table(data$part_a))*100, 1)


data$part_count <- data$part_a + data$part_b + data$part_c +
  data$part_d + data$part_e + data$part_f +
  data$part_g + data$part_h + data$part_i +
  data$part_j + data$part_k + data$part_l +
  data$part_m + data$part_n + data$part_o +
  data$part_p + data$part_q + data$part_r +
  data$part_s + data$part_t + data$part_u +
  data$part_v

############################################################################################
### FIGURE A.2.1
par(mfrow=c(1,1), mar=c(3,2,3,1), mgp=c(2, .7, 0))
barplot(table(data$part_count), xlab="Number of political actions", border="grey")

mean(data$part_count, na.rm=T)
sd(data$part_count, na.rm=T)
###########################################################################################

data$C8a <- ifelse(data$C8a==77, NA, data$C8a)
data$C8b <- ifelse(data$C8b==77, NA, data$C8b)
data$C8c <- ifelse(data$C8c==77, NA, data$C8c)
data$C8d <- ifelse(data$C8d==77, NA, data$C8d)

data$C34a <- ifelse(data$C34a==77, NA, data$C34a)
data$C34b <- ifelse(data$C34b==77, NA, data$C34b)
data$C34c <- ifelse(data$C34c==77, NA, data$C34c)
data$C34d <- ifelse(data$C34d==77, NA, data$C34d)
data$C34e <- ifelse(data$C34e==77, NA, data$C34e)

data$C35a <- ifelse(data$C35a==77, NA, data$C35a)
data$C35b <- ifelse(data$C35b==77, NA, data$C35b)
data$C35c <- ifelse(data$C35c==77, NA, data$C35c)
data$C35d <- ifelse(data$C35d==77, NA, data$C35d)


############################################################################################
### FIGURE A.7.3

par(mfrow=c(1,2), mar=c(3,2,3,1), mgp=c(2, .7, 0), cex.axis=.6, cex.main=.8, font.main=1)

barplot(prop.table(table(data$C35a)), names.arg = c("Strongly \n disagree",	"Disagree",
                                                    "Agree",	"Strongly \n agree"), border="grey", ylim=c(0, .6),
        main="Everyone needs to use their political voice  \n to solve problems peacefully. ")

barplot(prop.table(table(data$C34d)), names.arg = c("Strongly \n disagree",	"Disagree",
        "Agree",	"Strongly \n agree"), border="grey", ylim=c(0, .6), 
        main="Time to improve relations \n with other ethnic groups")

############################################################################################
# PTG

data$E2a <- ifelse(data$E2a==88, NA, data$E2a)
data$E2b <- ifelse(data$E2b==88, NA, data$E2b)
data$E2c <- ifelse(data$E2c==88, NA, data$E2c)
data$E2d <- ifelse(data$E2d==88, NA, data$E2d)
data$E2e <- ifelse(data$E2e==88, NA, data$E2e)
data$E2f <- ifelse(data$E2f==88, NA, data$E2f)
data$E2g <- ifelse(data$E2g==88, NA, data$E2g)
data$E2h <- ifelse(data$E2h==88, NA, data$E2h)
data$E2i <- ifelse(data$E2i==88, NA, data$E2i)
data$E2j <- ifelse(data$E2j==88, NA, data$E2j)

data$PTG <- data$E2a + data$E2b + data$E2c + data$E2d + data$E2e + data$E2f +
            data$E2g + data$E2h + data$E2i + data$E2j 

data$PTG <- data$PTG/10


###
cronbach <- alpha(cbind(data$E2a, data$E2b, data$E2c, data$E2d, data$E2e, data$E2f,
  data$E2g, data$E2h, data$E2i, data$E2j)) 

ptg.labs <- c("Changed my priorities about what is important in life",
              "Greater appreciation for the value of my own life",
              "Able to do better things with my life", 
              "Better understanding of spiritual matters", 
              "Greater sense of closeness with others", 
              "Established a new path for my life", 
              "Know better that I can handle difficulties",
              "Have a stronger religious faith",
              "Discovered that I am stronger than I thought I was", 
              "Learned a great deal about how wonderful people are") 

cronbach$response.freq[,1:5]

means <- apply(cbind(data$E2a, data$E2b, data$E2c, data$E2d, data$E2e, data$E2f,
                     data$E2g, data$E2h, data$E2i, data$E2j), 2, mean, na.rm=T)
ord <- order(means, decreasing=F)

############################################################################################
### FIGURE A.7.4

par(family="Gill Sans", mgp=c(2, .1, 0), mar=c(3,16,2,1), mfrow=c(1,1))
barplot(t(cronbach$response.freq[ord,1:5]), horiz=T, 
        col=brewer.pal(n=5, "YlGn"), border="lightgrey", 
        axes=F,
        names.arg=ptg.labs[ord], cex.names=.8)
axis(1, col="white")
axis(3, at=c(0), labels=c("No change"), col="white", col.axis="black")
axis(3, at=c(.9), labels=c("Very great change"), col="white", col.axis="black")

###############################################################################################################
## Dimensionality of Political Participation Items

part.label <-c(
"Participate in party activities",
"Do voluntary work for a party",
"Donate money to a party",
"Contacted a politician", 
"Contacted a local government official",
"Contacted an political organisation",
"Worked in a political party or action group",
"Worked for the campaign of a candidate for office",
"Worked in another political organization or association",
"Worn or displayed a campaign badge/sticker",
"Signed a petition",
"Taken part in a lawful public demonstration",
"Taken part in a strike",

"Cancel vote at the voting station",
"Boycotted certain products",
"Deliberately bought certain products",
"Raised funds",
"Participated in illegal protest activities",

"Visited websites of political organizations or candidates",
"Twitter political content",
"Participated in political activities over the internet",	
"Attended a political meeting/rally"
)


f.dat <- data[,409:430]
colnames(f.dat) <- part.label
plot(eigen(cor(f.dat))$values, type="b")
abline(h=1, lty=2)

#######################################################################################################
# TABLE 1
f.3c <- fa(f.dat, nfactors=3, cor="tet", rotate="oblimin")
print(f.3c, digits=2, cutoff=.3, sort=TRUE)

############################################################################################
# Dimensionality of Including Social Participation Items

active.label <- c(
  "Sports club",
  "Religious organisation",
  "Political parties",
  "Interest group",
  "Charity or social-welfare organisation",
  "Environmental or human rights organisation",
  "Cultural organisation",
  "Leisure or hobby organisation",
  "Youth societies",
  "Other"
)

f.dat <- data[,c(389:398, 409:430)]
colnames(f.dat) <- c(c(active.label, part.label))
plot(eigen(cor(f.dat))$values, type="b")
abline(h=1, lty=2)

f.dat <- f.dat[, -c(3, 10)] # drop political parties and other

########################################################################################################
# TABLE A.4.2
f.4 <- fa(f.dat, nfactors=4, cor="tet",  rotate="promax")
print(f.4, digits=2, cutoff=.3, sort=TRUE)

################################################################################################################
# MAIN RESULTS: Political Participation
################################################################################################################

m.part_count.1 <- ictreg.joint(sexaussault ~ female + I(age/10) + factor(edu.3) + factor(B7) +  factor(Province) + assist.army + D3a + displace + H2 + H3e , 
                    J=3, 
                    data=data, 
                    treat="treatment",
                    outcome="part_count",
                    outcome.reg="linear",
                    constrained=TRUE, 
                    maxIter=20000)
summary(m.part_count.1)

part_count.1.pred <- predict.ictreg.joint(m.part_count.1, se.fit = TRUE, interval = "confidence", 
                                          level = 0.95, avg = TRUE, 
                                          sensitive.value = "both", 
                                          sensitive.diff = TRUE, return.draws = TRUE,
                                          predict.sensitive = TRUE)

part_count.1.pred$fit
part_count.1.pred$fitsens

#  Logged Dependent Variable
data$log.part_count <- log(data$part_count+1)
m.part_count.1.log <- ictreg.joint(sexaussault ~ female + I(age/10) + factor(edu.3) + factor(B7) +  factor(Province) + assist.army + D3a + displace + H2 + H3e, 
                               J=3, 
                               data=data, 
                               treat="treatment",
                               outcome="log.part_count",
                               outcome.reg="linear",
                               constrained=TRUE, 
                               maxIter=20000)
summary(m.part_count.1.log)

# Compare to Direct Item
m.part_count.1.d <- lm(part_count ~ female + I(age/10) + factor(edu.3) + factor(B7) +  factor(Province) + assist.army + D3a + displace +  H2 + H3e + sexaussault_d, data=data)
summary(m.part_count.1.d)

m.part_count.1.d2 <- lm(part_count ~ female + I(age/10) + factor(edu.3) + factor(B7) +  factor(Province) + assist.army + D3a + captured + displace +  H2 + H3e + sexaussault_d2 + PTG + PTSD, data=data)
summary(m.part_count.1.d2)

################################################################################################################
# TABLE A.4.3
# Outcome Equation
xtable(round(cbind(m.part_count.1$par.outcome, m.part_count.1$se.outcome), 2))

# Sensitive and Control Item Equations
xtable(round(cbind(m.part_count.1$par.treat, m.part_count.1$se.treat, m.part_count.1$par.control, m.part_count.1$se.control), 2))

# Direct Item Equation
xtable(round(cbind(coef(m.part_count.1.d), se.coef(m.part_count.1.d)), 2))

################################################################################################################
# FIGURE 1

lab.vec <- c("(Intercept)", "Female", "Age/10", "Education: Medium", "Education: High",
             "Sri Lankan Tamil", "Indian Tamil", "Moor", "Province 2", "Province 3", "Province 4", "Province 5",
             "Province 6", "Province 7", "Province 8", "Province 9",
             "Assisted Military Group", "Other Traumatic Experience",
             "Displaced", "Prewar: Political Involvement", "Prewar: Work for NGO",
             "")

coefs <- m.part_count.1$par.outcome[-c(1,9:16)]
ses <- m.part_count.1$se.outcome[-c(1,9:16)]
labs <- lab.vec[-c(1,9:16)]
ord <- order(coefs)

par(mfrow=c(1,1), mar=c(3, 12, 2, 2), las=1, cex.lab=.8, cex.axis=.8, family="GillSans")
plot(0,0, pch="", axes=F, ylab="", xlim=c(-4,6), ylim=c(0,length(coefs)), main="Political Participation Index", cex.main=.8, xlab="Coefficient")
grid(col="lightgrey", lty=1, lwd=.3)
axis(2, at=1:length(coefs), label=labs[ord], col="white")
axis(1, col="white")
abline(v=0, col="grey")

for(i in 1:length(coefs)){
  
  x <- rnorm(1000, mean=coefs[ord][i], sd=ses[ord][i])
  denstrip(x, horiz=TRUE, at=i, width=.1)
  points(coefs[ord][i], i, pch=19, col="black", cex=.8)
}
points(coefs[ord][13], 13, pch=19, col="red", cex=.8)

axis(2, at=13, label="Experience: Sexual Violence", col="white", col.axis="red", cex=.8)
axis(2, at=0, label="Provincial-Level Fixed Effects: yes", col="white", col.axis="grey")
text(coefs[ord][13], 13, round(coefs[ord][13], 2), pos=1, cex=.8, col="red")
text(coefs[ord][13], 13-.5, paste("[", round(coefs[ord][13]-1.96*ses[ord][13], 2), " ", round(coefs[ord][13]+1.96*ses[ord][13], 2), "]",sep="") , pos=1, cex=.8, col="red")

########################################################################################################
# FIGURE A.4.3

coefs <- coef(m.part_count.1.d)[-c(1,9:16)]
ses <- se.coef(m.part_count.1.d)[-c(1,9:16)]
labs <- lab.vec[-c(1,9:16)]
ord <- order(coefs)

par(mfrow=c(1,1), mar=c(3, 12, 2, 2), las=1, cex.lab=.8, cex.axis=.8, family="GillSans")
plot(0,0, pch="", axes=F, ylab="", xlim=c(-4,6), ylim=c(0,length(coefs)), main="Political Participation Index", cex.main=.8, xlab="Coefficient")
grid(col="lightgrey", lty=1, lwd=.3)
axis(2, at=1:length(coefs), label=labs[ord], col="white")
axis(1, col="white")
abline(v=0, col="grey")

for(i in 1:length(coefs)){
  
  x <- rnorm(1000, mean=coefs[ord][i], sd=ses[ord][i])
  denstrip(x, horiz=TRUE, at=i, width=.1)
  points(coefs[ord][i], i, pch=19, col="black", cex=.8)
}
points(coefs[ord][13], 13, pch=19, col="red", cex=.8)

axis(2, at=13, label="Experience: Sexual Violence", col="white", col.axis="red", cex=.8)
axis(2, at=0, label="Provincial-Level Fixed Effects: yes", col="white", col.axis="grey")
text(coefs[ord][13], 13, round(coefs[ord][13], 2), pos=1, cex=.8, col="red")
text(coefs[ord][13], 13-.5, paste("[", round(coefs[ord][13]-1.96*ses[ord][13], 2), " ", round(coefs[ord][13]+1.96*ses[ord][13], 2), "]",sep="") , pos=1, cex=.8, col="red")

########################################################################################################
# SENSITIVITY ANALYSES
#######################################################################################################
# SENSITIVITY: No Liars Assumption

# No Liars (Robustness to Floor Effects)
mle.base <- ictreg(sexaussault ~ 1, data=data, treat="treatment", J=3, method="ml")
summary(mle.base)

mle.floor <- ictreg(sexaussault ~ 1, data=data, treat="treatment", J=3, method="ml", floor=TRUE, floor.fit="glm", floor.formula=~1)
summary(mle.floor)

invlogit(mle.base$par.treat) - invlogit(mle.floor$par.treat)

#> invlogit(-1.96327)
#[1] 0.1231136
#> invlogit(-1.96327 - 1.96*0.19897)
#[1] 0.0868077
#> invlogit(-1.96327 + 1.96*0.19897)
#[1] 0.1717481

#> invlogit(-1.32614)
#[1] 0.2097986
#> invlogit(-1.32614 - 1.96*0.23835)
#[1] 0.1426679
#> invlogit(-1.32614 + 1.96*0.23835)
#[1] 0.2975538

invlogit(-1.96327)-invlogit(-1.32614)

#######################################################################################################
# TABLE A.5.1

xtable(
  rbind(c(mle.base$par.treat, mle.base$se.treat, mle.floor$par.treat, mle.floor$se.treat),
             c(invlogit(mle.base$par.treat), NA, invlogit(mle.floor$par.treat), NA),
             c(NA, NA, mle.floor$par.floor, mle.floor$se.floor),
             c(NA, NA, invlogit(mle.floor$par.floor), NA),
            c(dim(mle.base$x)[1], dim(mle.floor$x)[1]),
             c(mle.base$llik, NA, mle.floor$llik, NA)
        ), digits=3)

#######################################################################################################

mle.base <- ictreg(sexaussault ~ 1 + part_count, data=data, treat="treatment", J=3, method="ml")
summary(mle.base)

mle.floor <- ictreg(sexaussault ~ 1 + part_count, data=data, treat="treatment", J=3, method="ml", floor=TRUE, floor.fit="glm", floor.formula=~1+part_count)
summary(mle.floor)

########################################################################################################
# Simulation-based Sensitivity Analysis

sim.coefs <- rep(NA, 1000)
sim.ses <- rep(NA, 1000)

for(i in 1:1000){
  data$liar <-  rbinom(1800, 1, .087)
  data$sexaussalt.sim <- ifelse(data$liar==1 & data$treatment==1 & data$sexaussault!=4, data$sexaussault+1, data$sexaussault)
  
 try(m.part_count.1 <- ictreg.joint(sexaussalt.sim ~ female + I(age/10) + factor(edu.3) + factor(B7) +  factor(Province) + assist.army + D3a + displace + H2 + H3e, 
                                 J=3, 
                                 data=data, 
                                 treat="treatment",
                                 outcome="part_count",
                                 outcome.reg="linear",
                                 constrained=TRUE, 
                                 maxIter=20000), silent=TRUE)
  
  sim.coefs[i]  <- m.part_count.1$par.outcome[22]
  sim.ses[i]  <- m.part_count.1$se.outcome[22]
}

round(quantile(sim.coefs, c(.025, .975)), 2)

########################################################################################################
# FIGURE 2

par(mar=c(3,3,1,1), mfrow=c(1,1), mgp=c(1.5, .5, 0), cex.axis=.8)
hist(sim.coefs[sim.coefs>5], ylim=c(0,200), xlim=c(5,6), main="", ylab="Frequency", xlab="Simulated Effects of Wartime Sexual Violence on Political Participation", border=F)
box()
abline(v=c(5.52), lty=2, col="red")
abline(v=5.42, lty=2)
text(5.52, 180, "Original Estimate: 5.52 [5.09, 5.96]", col="red", cex=.6, pos=4)
text(5.42, 180, "Average of 1000 Simulated Estimates: 5.42",  cex=.6, pos=2)

#######################################################################################################
# SENSITIVITY: No Unobserved Confounding
# Use method by Cinelli & Hazlett (2020)

coefs <- m.part_count.1$par.outcome[-c(1,9:16)]
ses <- m.part_count.1$se.outcome[-c(1,9:16)]
est <- coefs[13]
se <- ses[13]
df <- 914 - 23 - 1

bias <- function(R2.yz, R2.dz){  
  bias = se * sqrt( (R2.yz * R2.dz)/(1-R2.dz) * df )
  return(est-bias)
}

x <- seq(0, 1, by=.01)
y <- seq(0, 1, by=.01)
sens <- outer(x, y, FUN=bias)

par(mfrow=c(1,1), mar=c(3, 3, 2, 2))
contour(sens, nlevels=50, xlab=expression(paste("Hypothetical Partial ", R^2, " of Unobserved Confounder(s) with Sexual Violence")), 
        ylab=expression(paste("Hypothetical Partial ", R^2, "of Unobserved Confounder(s) with Participation")),
        labcex=1, method="edge", axes=F, col="black", lwd=.5)
contour(sens, levels=0, labcex=1, col="red", lwd=2, add=T)
axis(1, col="white")
axis(2, col="white")
grid(col="grey", lwd=.5, lty=1)

# Compare to Observed Confounders

lm.1 <- ictreg(sexaussault ~ female + I(age/10) + factor(edu.3) + factor(B7) +  factor(Province) + assist.army + D3a + displace + H2 + H3e, treat="treatment", method="lm", J=3, data=data)
summary(lm.1)

t <- lm.1$par.treat/lm.1$se.treat
p.R2.dz <- t^2/(t^2 + lm.1$resid.df)

t <- m.part_count.1$par.outcome/m.part_count.1$se.outcome
p.R2.yz <- t^2/(t^2 + df)
p.R2.yz <- p.R2.yz[-c(length(p.R2.yz))]

points(p.R2.dz[-1], p.R2.yz[-1], pch=19, cex=.5)

par(mgp=c(2, .5, 0))
plot(p.R2.dz[-1], p.R2.yz[-1], pch=19, axes=F, cex=.8, xlim=c(0, .004),
     xlab=expression(paste("Partial ", R^2, " of Observed Confounder(s) with Sexual Violence")), 
     ylab=expression(paste("Partial ", R^2, "of Observed Confounder(s) with Participation")))
text(p.R2.dz[-1], p.R2.yz[-1], lab.vec[-c(1, 23)], pos=4, cex=.8)
axis(1, col="white")
axis(2, col="white")
grid(col="grey", lwd=.5, lty=1)


#########################################################################################################
# FIGURE 3

contour(sens, nlevels=50, xlab=expression(paste("Hypothetical Partial ", R^2, " of Unobserved Confounder(s) with Sexual Violence")), 
        ylab=expression(paste("Hypothetical Partial ", R^2, "of Unobserved Confounder(s) with Participation")),
        labcex=.8, method="edge", axes=F, col="black", lwd=.5)
contour(sens, levels=0, labcex=.8, col="red", lwd=2, add=T)
axis(1, col="white")
axis(2, col="white")
grid(col="grey", lwd=.5, lty=1)


points(p.R2.dz[2],  p.R2.yz[2], pch=19, cex=.5, col="red") # Female
points(10*p.R2.dz[2],  10*p.R2.yz[2], pch=19, cex=.5, col="red") # Female
points(20*p.R2.dz[2],  20*p.R2.yz[2], pch=19, cex=.5, col="red") # Female
points(30*p.R2.dz[2],  30*p.R2.yz[2], pch=19, cex=.5, col="red") # Female

text(p.R2.dz[2],  p.R2.yz[2], "Female", cex=.8, col="red", pos=3) # Female
text(10*p.R2.dz[2],  10*p.R2.yz[2], "10 x Female", cex=.8, col="red", pos=3) # Female
text(20*p.R2.dz[2],  20*p.R2.yz[2], "20 x Female", cex=.8, col="red", pos=3) # Female
text(30*p.R2.dz[2],  30*p.R2.yz[2], "30 x Female", cex=.8, col="red", pos=3) # Female


points(10*p.R2.dz[17],  10*p.R2.yz[17], pch=19, cex=.5, col="orange") # Assisted Military Group
points(50*p.R2.dz[17],  50*p.R2.yz[17], pch=19, cex=.5, col="orange") # Assisted Military Group
points(100*p.R2.dz[17],  100*p.R2.yz[17], pch=19, cex=.5, col="orange") # Assisted Military Group
points(200*p.R2.dz[17],  200*p.R2.yz[17], pch=19, cex=.5, col="orange") # Assisted Military Group
points(300*p.R2.dz[17],  300*p.R2.yz[17], pch=19, cex=.5, col="orange") # Assisted Military Group

text(10*p.R2.dz[17],  10*p.R2.yz[17], "10 x Assisted Military Group", cex=.8, col="orange", pos=4) # Assisted Military Group
text(50*p.R2.dz[17],  50*p.R2.yz[17], "50 x Assisted Military Group", cex=.8, col="orange", pos=4) # Assisted Military Group
text(100*p.R2.dz[17],  100*p.R2.yz[17], "100 x Assisted Military Group", cex=.8, col="orange", pos=4) # Assisted Military Group
text(200*p.R2.dz[17],  200*p.R2.yz[17], "200 x Assisted Military Group", cex=.8, col="orange", pos=4) # Assisted Military Group
text(300*p.R2.dz[17],  300*p.R2.yz[17], "300 x Assisted Military Group", cex=.8, col="orange", pos=4) # Assisted Military Group


points(50*p.R2.dz[19],  50*p.R2.yz[19], pch=19, cex=.5, col="purple") # Displaced
points(100*p.R2.dz[19],  100*p.R2.yz[19], pch=19, cex=.5, col="purple") # Displaced
points(200*p.R2.dz[19],  200*p.R2.yz[19], pch=19, cex=.5, col="purple") # Displaced
points(300*p.R2.dz[19],  300*p.R2.yz[19], pch=19, cex=.5, col="purple") # Displaced

text(50*p.R2.dz[19],  50*p.R2.yz[19], "50 x Displaced", cex=.8, col="purple", pos=3) # Displaced
text(100*p.R2.dz[19],  100*p.R2.yz[19], "100 x Displaced", cex=.8, col="purple", pos=3) # Displaced
text(200*p.R2.dz[19],  200*p.R2.yz[19], "200 x Displaced", cex=.8, col="purple", pos=3) # Displaced
text(300*p.R2.dz[19],  300*p.R2.yz[19], "300 x Displaced", cex=.8, col="purple", pos=3) # Displaced

########################################################################################################  
# SENSITIVITY: Sample Selection

sum(is.na(data$H2))
sum(is.na(data$H3e))
sum(is.na(data$displace))
sum(is.na(data$D3a))
sum(is.na(data$assist.army))

# Assess Victimization Across Subsamples

v.1 <- ictreg(sexaussault ~ 1, data=data, treat="treatment", J=3, method="lm")
v.2 <- ictreg(sexaussault ~ 1, data=data[is.na(data$H2)==T & is.na(data$H3e)==T,], treat="treatment", J=3, method="lm")
v.3 <- ictreg(sexaussault ~ 1, data=data[is.na(data$H2)==F & is.na(data$H3e)==F,], treat="treatment", J=3, method="lm")

#######################################################################################################
# Table A.5.2.
xtable(round(rbind(cbind(length(v.1$y), v.1$par.treat, v.1$se.treat),
                   cbind(length(v.2$y), v.2$par.treat, v.2$se.treat),
                   cbind(length(v.3$y), v.3$par.treat, v.3$se.treat)), 2))

#######################################################################################################

# Alternative 1: without pre-war controls
m.part_count.1.a <- ictreg.joint(sexaussault ~ female + I(age/10) + factor(edu.3) + factor(B7) +  factor(Province) + assist.army + D3a + displace, 
                                 J=3, 
                                 data=data, 
                                 treat="treatment",
                                 outcome="part_count",
                                 outcome.reg="linear",
                                 constrained=TRUE, 
                                 maxIter=20000)
summary(m.part_count.1.a)

# Alternative 2: without pre-war controls but on restricted sample = those who answered the module
m.part_count.1.b <- ictreg.joint(sexaussault ~ female + I(age/10) + factor(edu.3) + factor(B7) +  factor(Province) + assist.army + D3a + displace, 
                                 J=3, 
                                 data=data[is.na(data$H2)==F & is.na(data$H3e)==F,], 
                                 treat="treatment",
                                 outcome="part_count",
                                 outcome.reg="linear",
                                 constrained=TRUE, 
                                 maxIter=20000)
summary(m.part_count.1.b)

# Alternative 3: without pre-war controls but on restricted sample = those who did not answer the module
m.part_count.1.c <- ictreg.joint(sexaussault ~ 1, 
                                 J=3, 
                                 data=data[is.na(data$H2)==T & is.na(data$H3e)==T,], 
                                 treat="treatment",
                                 outcome="part_count",
                                 outcome.reg="linear",
                                 constrained=TRUE, 
                                 maxIter=20000)
summary(m.part_count.1.c)

m.part_count.1.d <- ictreg.joint(sexaussault ~ 1, 
                                 J=3, 
                                 data=data[is.na(data$H2)==F & is.na(data$H3e)==F,], 
                                 treat="treatment",
                                 outcome="part_count",
                                 outcome.reg="linear",
                                 constrained=TRUE, 
                                 maxIter=20000)
summary(m.part_count.1.d)

m.part_count.1.e <- ictreg.joint(sexaussault ~ 1, 
                                 J=3, 
                                 data=data, 
                                 treat="treatment",
                                 outcome="part_count",
                                 outcome.reg="linear",
                                 constrained=TRUE, 
                                 maxIter=20000)
summary(m.part_count.1.e)


length(summary(m.part_count.1)$y)
length(summary(m.part_count.1.a)$y)
length(summary(m.part_count.1.b)$y)
length(summary(m.part_count.1.c)$y)
length(summary(m.part_count.1.d)$y)
length(summary(m.part_count.1.e)$y)

##########################################################################################################
# TABLE A.5.3
xtable(round(cbind(m.part_count.1.b$par.outcome, m.part_count.1.b$se.outcome, m.part_count.1.a$par.outcome, m.part_count.1.a$se.outcome), 2))

##########################################################################################################
# TABLE A.5.4
xtable(round(rbind(cbind(length(summary(m.part_count.1.c)$y), m.part_count.1.c$par.outcome, m.part_count.1.c$se.outcome),
                   cbind(length(summary(m.part_count.1.d)$y), m.part_count.1.d$par.outcome, m.part_count.1.d$se.outcome),
                   cbind(length(summary(m.part_count.1.e)$y), m.part_count.1.e$par.outcome, m.part_count.1.e$se.outcome)), 2))

##########################################################################################################
# TYPES of Political Participation

data$traditional <- data$part_v + data$part_a + data$part_b + data$part_c + data$part_d + data$part_e + data$part_f + data$part_g + data$part_h + data$part_i + data$part_j + data$part_h + data$part_l + data$part_m
data$activist <- data$part_k + data$part_n + data$part_o + data$part_p + data$part_q + data$part_r
data$online <- data$part_u + data$part_t + data$part_s

data$trad_part.c <- data$trad_part.c/sd(data$trad_part.c)
data$act_part.c <- data$act_part/sd(data$act_part.c)
data$on_part.c <- data$on_part.c/sd(data$on_part.c)

m.trad_part <- ictreg.joint(sexaussault ~ female + I(age/10) + factor(edu.3) + tamil +  eastern + assist.army + D3a + displace + H2 + H3e, 
                            J=3, 
                            data=data, 
                            treat="treatment",
                            outcome="trad_part.c",
                            outcome.reg="linear",
                            constrained=TRUE, 
                            maxIter=10000)
summary(m.trad_part)

m.act_part <- ictreg.joint(sexaussault ~ female + I(age/10) + factor(edu.3) + tamil + eastern + assist.army + D3a + displace + H2 + H3e, 
                           J=3, 
                           data=data, 
                           treat="treatment",
                           outcome="act_part.c",
                           outcome.reg="linear",
                           constrained=TRUE, 
                           maxIter=10000)
summary(m.act_part)

m.on_part <- ictreg.joint(sexaussault ~ female + I(age/10) + factor(edu.3) + tamil +  eastern + assist.army + D3a + displace + H2 + H3e + C34d, 
                          J=3, 
                          data=data, 
                          treat="treatment",
                          outcome="on_part.c",
                          outcome.reg="linear",
                          constrained=TRUE, 
                          maxIter=10000)
summary(m.on_part)

##########################################################################################################
# TABLE 2

round(cbind(m.trad_part$par.outcome, m.trad_part$se.outcome,
            m.act_part$par.outcome, m.act_part$se.outcome,
            m.on_part$par.outcome, m.on_part$se.outcome
), 2)

##########################################################################################################
# Voting Behavior
m.1 <- ictreg.joint(sexaussault ~ female + I(age/10) + factor(edu.3) + tamil +  eastern + assist.army + D3a + displace + H2 + H3e, 
                    J=3, 
                    data=data, 
                    treat="treatment",
                    outcome="vote_pres",
                    outcome.reg="linear",
                    constrained=TRUE)
summary(m.1)

m.2 <- ictreg.joint(sexaussault ~ female + I(age/10) + factor(edu.3) + tamil +  eastern + assist.army + D3a + displace + H2 + H3e, 
                    J=3, 
                    data=data, 
                    treat="treatment",
                    outcome="vote_parl",
                    outcome.reg="linear",
                    constrained=TRUE)
summary(m.2)

m.3 <- ictreg.joint(sexaussault ~ female + I(age/10) + factor(edu.3) + tamil +  eastern + assist.army + D3a + displace + H2 + H3e, 
                    J=3, 
                    data=data, 
                    treat="treatment",
                    outcome="vote_loc",
                    outcome.reg="linear",
                    constrained=TRUE)
summary(m.3)

##########################################################################################################
# TABLE A.4.4
xtable(round(cbind(m.1$par.outcome, m.1$se.outcome, m.2$par.outcome, m.2$se.outcome, m.3$par.outcome, m.3$se.outcome), 2))

##########################################################################################################
# TESTING THE MECHANISMS
##########################################################################################################
#  Causal Mediation Analysis
#  Mechanism: Social Participation

data$age10 <- data$age/10
data$edu.m <- ifelse(data$edu.3=="medium", 1, 0)
data$edu.h <- ifelse(data$edu.3=="high", 1, 0)


# Mediator Equation
m.sv.ptg <- ictreg.joint(sexaussault ~ female + age10 + edu.m + edu.h + tamil + eastern + assist.army + D3a + displace + H2 + H3e, 
                         J=3, 
                         data=data[is.na(data$H2)==F & is.na(data$H3e)==F,], 
                         treat="treatment",
                         outcome="active_count",
                         outcome.reg="linear",
                         constrained=TRUE,
                         maxIter = 20000)
summary(m.sv.ptg)


# Outcome Equation
m.ptg.sp <- ictreg.joint(sexaussault ~ female + age10 + edu.m + edu.h + tamil + eastern + assist.army + D3a + displace + H2 + H3e + active_count, 
                         J=3, 
                         data=data, 
                         treat="treatment",
                         outcome="part_count",
                         outcome.reg="linear",
                         constrained=TRUE,
                         maxIter = 30000)
summary(m.ptg.sp)

######################################################################################################
# TABLE A.6.1
xtable(round(cbind(m.sv.ptg$par.outcome, m.sv.ptg$se.outcome), 2))
xtable(round(cbind(m.ptg.sp$par.outcome, m.ptg.sp$se.outcome), 2))

######################################################################################################

# Predicted Values
predict.ptg <- predict.ictreg.joint(m.sv.ptg, sensitive.value="both", return.draws=T)

X.1 <- as.data.frame(m.ptg.sp$x)
X.1$active_count<- predict.ptg[[1]]$Z1[,1]

X.0 <- as.data.frame(m.ptg.sp$x)
X.0$active_count <- predict.ptg[[1]]$Z0[,1]

predict.sp.1 <- predict.ictreg.joint(m.ptg.sp, newdata=X.1, sensitive.value="both", return.draws=T)
predict.sp.0 <- predict.ictreg.joint(m.ptg.sp, newdata=X.0, sensitive.value="both", return.draws=T)

# Y(M=1, T=1)
Y.11 <- mean(predict.sp.1[[1]]$Z1[,1])

# Y(M=1, T=0)
Y.10 <- mean(predict.sp.1[[1]]$Z0[,1])

# Y(M=0, T=1)
Y.01 <- mean(predict.sp.0[[1]]$Z1[,1])

# Y(M=0, T=0)
Y.00 <- mean(predict.sp.0[[1]]$Z0[,1])

# ACME
Y.11-Y.01
Y.10-Y.00
((Y.11-Y.01)+(Y.10-Y.00))/2

# ANDE
Y.11-Y.10
Y.01-Y.00
((Y.11-Y.10)+(Y.01-Y.00))/2

# With Uncertainties
# Y(M(1), T=1)
Y.11 <- apply(predict.sp.1$draws.predict[[2]], 2, mean)
mean(Y.11)
quantile(Y.11, c(.025, .975))

# Y(M(1), T=0)
Y.10 <-  apply(predict.sp.1$draws.predict[[1]], 2, mean)
mean(Y.10)
quantile(Y.10, c(.025, .975))

# Y(M(0), T=1)
Y.01 <- apply(predict.sp.0$draws.predict[[2]], 2, mean)
mean(Y.01)
quantile(Y.01, c(.025, .975))

# Y(M(0), T=0)
Y.00 <- apply(predict.sp.0$draws.predict[[1]], 2, mean)
mean(Y.00)
quantile(Y.00, c(.025, .975))

# ACME
mean(Y.11-Y.01)
quantile((Y.11-Y.01), c(.025, .975))

mean(Y.10-Y.00)
quantile((Y.10-Y.00), c(.025, .975))

mean(((Y.11-Y.01)+(Y.10-Y.00))/2)
quantile((((Y.11-Y.01)+(Y.10-Y.00))/2), c(.025, .975))

# ANDE
mean(Y.11-Y.10)
quantile((Y.11-Y.10), c(.025, .975))

mean(Y.01-Y.00)
quantile((Y.01-Y.00), c(.025, .975))

mean(((Y.11-Y.10)+(Y.01-Y.00))/2)
quantile((((Y.11-Y.10)+(Y.01-Y.00))/2), c(.025, .975))


##########################################################################################################
# FIGURE 4

par(mar=c(3,4,1,3))
plot( c(mean(((Y.11-Y.01)+(Y.10-Y.00))/2), mean(((Y.11-Y.10)+(Y.01-Y.00))/2)), c(1,2), pch="", axes=F, ylim=c(0.5, 2.5), xlim=c(-2, 6), ylab="", xlab="Effect on Political Participation Index")
axis(1, col="white")
grid(col="lightgrey", lwd=.5, lty=1)
abline(v=0, col="grey", lwd=1.5)
axis(2, at=c(1:2), labels=c("ACME", "ANDE"), col="white")

denstrip((((Y.11-Y.10)+(Y.01-Y.00))/2), horiz=TRUE, at=2, width=.03)
denstrip((((Y.11-Y.01)+(Y.10-Y.00))/2), horiz=TRUE, at=1, width=.03)
points(c(mean(((Y.11-Y.01)+(Y.10-Y.00))/2), mean(((Y.11-Y.10)+(Y.01-Y.00))/2)), c(1,2), pch=19)

text(mean(((Y.11-Y.10)+(Y.01-Y.00))/2), 2, round(mean(((Y.11-Y.10)+(Y.01-Y.00))/2), 2), pos=1, cex=.8)
text(mean(((Y.11-Y.10)+(Y.01-Y.00))/2), 2-.1, paste("[", round(quantile((((Y.11-Y.10)+(Y.01-Y.00))/2), c(.025)), 2), " ", round(quantile((((Y.11-Y.10)+(Y.01-Y.00))/2), c(.975)),2), "]",sep="") , pos=1, cex=.8)

text(mean(((Y.11-Y.01)+(Y.10-Y.00))/2), 1, round(mean(((Y.11-Y.01)+(Y.10-Y.00))/2), 2), pos=1, cex=.8)
text(mean(((Y.11-Y.01)+(Y.10-Y.00))/2), 1-.1, paste("[", round(quantile((((Y.11-Y.01)+(Y.10-Y.00))/2), c(.025)), 2), " ", round(quantile((((Y.11-Y.01)+(Y.10-Y.00))/2), c(.975)),2), "]",sep="") , pos=1, cex=.8)

mtext(" Social Participation ", side=3, line=0, cex=.8, at=0)

##########################################################################################################
# Mechanism: Ingroup Favoritism

data$ingrouptrust <- NA
data$ingrouptrust[data$B7==1] <- data$C18a[data$B7==1]
data$ingrouptrust[data$B7==2] <- data$C18b[data$B7==2]
data$ingrouptrust[data$B7==3] <- data$C18c[data$B7==3]
data$ingrouptrust[data$B7==4] <- data$C18d[data$B7==4]

data$outgrouptrust <- NA
data$outgrouptrust[data$B7==1] <- (data$C18b[data$B7==1] + data$C18c[data$B7==1] + data$C18d[data$B7==1])/3
data$outgrouptrust[data$B7==2] <- (data$C18a[data$B7==2] + data$C18c[data$B7==2] + data$C18d[data$B7==2])/3
data$outgrouptrust[data$B7==3] <- (data$C18a[data$B7==3] + data$C18b[data$B7==3] + data$C18d[data$B7==3])/3
data$outgrouptrust[data$B7==4] <- (data$C18a[data$B7==4] + data$C18b[data$B7==4] + data$C18c[data$B7==4])/3

data$inoutrust <- data$ingrouptrust - data$outgrouptrust 

##########################################################################################################
# FIGURE A.7.2
par(mfrow=c(1,1), yaxs="i")
hist(data$inoutrust,  axes=F, ylab="", main="", xlab="Difference",  col="grey", border="white", ylim=c(0, 450), xlim=c(-6, 6))
hist(data$inoutrust[data$inoutrust>0], axes=F, xlim=c(0, 6), add=T, breaks=5, border=F, col="pink")
abline(v=0, lty=2)
axis(1)
mtext("Ethnic In-Group Trust - Ethnic Out-Group Trust", side=3, line=-1, at=-4, cex=.8)

##########################################################################################################
# Mediator Equation
m.sv.ptg <- ictreg.joint(sexaussault ~ female + age10 + edu.m + edu.h + tamil + eastern + assist.army + displace + D3a, 
                         J=3, 
                         data=data[is.na(data$H2)==F & is.na(data$H3e)==F, ], 
                         treat="treatment",
                         outcome="inoutrust",
                         outcome.reg="linear",
                         constrained=TRUE,
                         maxIter = 20000)
summary(m.sv.ptg)

# Outcome Equation
m.ptg.sp <- ictreg.joint(sexaussault ~ female + age10 + edu.m + edu.h + tamil + eastern + assist.army + displace + D3a + H2 + H3e + inoutrust, 
                         J=3, 
                         data=data, 
                         treat="treatment",
                         outcome="part_count",
                         outcome.reg="linear",
                         constrained=TRUE,
                         maxIter = 30000)
summary(m.ptg.sp)

########################################################################################################
# TABLE A.6.2
xtable(cbind(m.sv.ptg$par.outcome, m.sv.ptg$se.outcome))
xtable(cbind(m.ptg.sp$par.outcome, m.ptg.sp$se.outcome))

########################################################################################################
# Predicted Values
predict.ptg <- predict.ictreg.joint(m.sv.ptg, sensitive.value="both", return.draws=T)

X.1 <- as.data.frame(m.ptg.sp$x)
X.1$inoutrust <- predict.ptg[[1]]$Z1[,1]

X.0 <- as.data.frame(m.ptg.sp$x)
X.0$inoutrust <- predict.ptg[[1]]$Z0[,1]

predict.sp.1 <- predict.ictreg.joint(m.ptg.sp, newdata=X.1, sensitive.value="both", return.draws=T)
predict.sp.0 <- predict.ictreg.joint(m.ptg.sp, newdata=X.0, sensitive.value="both", return.draws=T)

# Y(M=1, T=1)
Y.11 <- mean(predict.sp.1[[1]]$Z1[,1])

# Y(M=1, T=0)
Y.10 <- mean(predict.sp.1[[1]]$Z0[,1])

# Y(M=0, T=1)
Y.01 <- mean(predict.sp.0[[1]]$Z1[,1])

# Y(M=0, T=0)
Y.00 <- mean(predict.sp.0[[1]]$Z0[,1])

# ACME
Y.11-Y.01
Y.10-Y.00
((Y.11-Y.01)+(Y.10-Y.00))/2

# ANDE
Y.11-Y.10
Y.01-Y.00
((Y.11-Y.10)+(Y.01-Y.00))/2

# With Uncertainties
# Y(M(1), T=1)
Y.11 <- apply(predict.sp.1$draws.predict[[2]], 2, mean)
mean(Y.11)
quantile(Y.11, c(.025, .975))

# Y(M(1), T=0)
Y.10 <-  apply(predict.sp.1$draws.predict[[1]], 2, mean)
mean(Y.10)
quantile(Y.10, c(.025, .975))

# Y(M(0), T=1)
Y.01 <- apply(predict.sp.0$draws.predict[[2]], 2, mean)
mean(Y.01)
quantile(Y.01, c(.025, .975))

# Y(M(0), T=0)
Y.00 <- apply(predict.sp.0$draws.predict[[1]], 2, mean)
mean(Y.00)
quantile(Y.00, c(.025, .975))

# ACME
mean(Y.11-Y.01)
quantile((Y.11-Y.01), c(.025, .975))

mean(Y.10-Y.00)
quantile((Y.10-Y.00), c(.025, .975))

mean(((Y.11-Y.01)+(Y.10-Y.00))/2)
quantile((((Y.11-Y.01)+(Y.10-Y.00))/2), c(.025, .975))

# ANDE
mean(Y.11-Y.10)
quantile((Y.11-Y.10), c(.025, .975))

mean(Y.01-Y.00)
quantile((Y.01-Y.00), c(.025, .975))

mean(((Y.11-Y.10)+(Y.01-Y.00))/2)
quantile((((Y.11-Y.10)+(Y.01-Y.00))/2), c(.025, .975))

########################################################################################################
# FIGURE A.6.1

par(mar=c(3,4,1,3))
plot( c(mean(((Y.11-Y.01)+(Y.10-Y.00))/2), mean(((Y.11-Y.10)+(Y.01-Y.00))/2)), c(1,2), pch="", axes=F, ylim=c(0.5, 2.5), xlim=c(-2, 7), ylab="", xlab="Effect on Political Participation Index")
axis(1, col="white")
grid(col="lightgrey", lwd=.5, lty=1)
abline(v=0, col="grey", lwd=1.5)
axis(2, at=c(1:2), labels=c("ACME", "ANDE"), col="white")

denstrip((((Y.11-Y.10)+(Y.01-Y.00))/2), horiz=TRUE, at=2, width=.03)
denstrip((((Y.11-Y.01)+(Y.10-Y.00))/2), horiz=TRUE, at=1, width=.03)
points(c(mean(((Y.11-Y.01)+(Y.10-Y.00))/2), mean(((Y.11-Y.10)+(Y.01-Y.00))/2)), c(1,2), pch=19)

text(mean(((Y.11-Y.10)+(Y.01-Y.00))/2), 2, round(mean(((Y.11-Y.10)+(Y.01-Y.00))/2), 2), pos=1, cex=.8)
text(mean(((Y.11-Y.10)+(Y.01-Y.00))/2), 2-.1, paste("[", round(quantile((((Y.11-Y.10)+(Y.01-Y.00))/2), c(.025)), 2), " ", round(quantile((((Y.11-Y.10)+(Y.01-Y.00))/2), c(.975)),2), "]",sep="") , pos=1, cex=.8)

text(mean(((Y.11-Y.01)+(Y.10-Y.00))/2), 1, round(mean(((Y.11-Y.01)+(Y.10-Y.00))/2), 4), pos=1, cex=.8)
text(mean(((Y.11-Y.01)+(Y.10-Y.00))/2), 1-.1, paste("[", round(quantile((((Y.11-Y.01)+(Y.10-Y.00))/2), c(.025)), 2), " ", round(quantile((((Y.11-Y.01)+(Y.10-Y.00))/2), c(.975)),2), "]",sep="") , pos=1, cex=.8)

############################################################################################################################
# Mechanism: Use political voice to solve problems peacefully. 

# Mediator Equation
m.sv.ptg <- ictreg.joint(sexaussault ~ female + age10 + edu.m + edu.h + tamil + eastern + assist.army + displace + D3a, 
                         J=3, 
                         data=data[is.na(data$H2)==F & is.na(data$H3a)==F,], 
                         treat="treatment",
                         outcome="C35a",
                         outcome.reg="linear",
                         constrained=TRUE,
                         maxIter = 20000)
summary(m.sv.ptg)

# Outcome Equation
m.ptg.sp <- ictreg.joint(sexaussault ~ female + age10 + edu.m + edu.h + tamil + eastern + assist.army + displace + D3a + H2 + H3a + C35a, 
                         J=3, 
                         data=data, 
                         treat="treatment",
                         outcome="part_count",
                         outcome.reg="linear",
                         constrained=TRUE,
                         maxIter = 30000)
summary(m.ptg.sp)

########################################################################################################
# TABLE A.6.3
xtable(cbind(m.sv.ptg$par.outcome, m.sv.ptg$se.outcome))
xtable(cbind(m.ptg.sp$par.outcome, m.ptg.sp$se.outcome))

########################################################################################################

# Predicted Values
predict.ptg <- predict.ictreg.joint(m.sv.ptg, sensitive.value="both", return.draws=T)

X.1 <- as.data.frame(m.ptg.sp$x)
X.1$C35a<- predict.ptg[[1]]$Z1[,1]

X.0 <- as.data.frame(m.ptg.sp$x)
X.0$C35a <- predict.ptg[[1]]$Z0[,1]

predict.sp.1 <- predict.ictreg.joint(m.ptg.sp, newdata=X.1, sensitive.value="both", return.draws=T)
predict.sp.0 <- predict.ictreg.joint(m.ptg.sp, newdata=X.0, sensitive.value="both", return.draws=T)

# Y(M=1, T=1)
Y.11 <- mean(predict.sp.1[[1]]$Z1[,1])

# Y(M=1, T=0)
Y.10 <- mean(predict.sp.1[[1]]$Z0[,1])

# Y(M=0, T=1)
Y.01 <- mean(predict.sp.0[[1]]$Z1[,1])

# Y(M=0, T=0)
Y.00 <- mean(predict.sp.0[[1]]$Z0[,1])

# ACME
Y.11-Y.01
Y.10-Y.00
((Y.11-Y.01)+(Y.10-Y.00))/2

# ANDE
Y.11-Y.10
Y.01-Y.00
((Y.11-Y.10)+(Y.01-Y.00))/2

# With Uncertainties
# Y(M(1), T=1)
Y.11 <- apply(predict.sp.1$draws.predict[[2]], 2, mean)
mean(Y.11)
quantile(Y.11, c(.025, .975))

# Y(M(1), T=0)
Y.10 <-  apply(predict.sp.1$draws.predict[[1]], 2, mean)
mean(Y.10)
quantile(Y.10, c(.025, .975))

# Y(M(0), T=1)
Y.01 <- apply(predict.sp.0$draws.predict[[2]], 2, mean)
mean(Y.01)
quantile(Y.01, c(.025, .975))

# Y(M(0), T=0)
Y.00 <- apply(predict.sp.0$draws.predict[[1]], 2, mean)
mean(Y.00)
quantile(Y.00, c(.025, .975))

# ACME
mean(Y.11-Y.01)
quantile((Y.11-Y.01), c(.025, .975))

mean(Y.10-Y.00)
quantile((Y.10-Y.00), c(.025, .975))

mean(((Y.11-Y.01)+(Y.10-Y.00))/2)
quantile((((Y.11-Y.01)+(Y.10-Y.00))/2), c(.025, .975))

# ANDE
mean(Y.11-Y.10)
quantile((Y.11-Y.10), c(.025, .975))

mean(Y.01-Y.00)
quantile((Y.01-Y.00), c(.025, .975))

mean(((Y.11-Y.10)+(Y.01-Y.00))/2)
quantile((((Y.11-Y.10)+(Y.01-Y.00))/2), c(.025, .975))

##########################################################################################################
# Figure A.6.2 (Upper Panel)

par(mar=c(3,4,1,3))
plot( c(mean(((Y.11-Y.01)+(Y.10-Y.00))/2), mean(((Y.11-Y.10)+(Y.01-Y.00))/2)), c(1,2), pch="", axes=F, ylim=c(0.5, 2.5), xlim=c(-2, 7), ylab="", xlab="Effect on Political Participation Index")
axis(1, col="white")
grid(col="lightgrey", lwd=.5, lty=1)
abline(v=0, col="grey", lwd=1.5)
axis(2, at=c(1:2), labels=c("ACME", "ANDE"), col="white")

denstrip((((Y.11-Y.10)+(Y.01-Y.00))/2), horiz=TRUE, at=2, width=.03)
denstrip((((Y.11-Y.01)+(Y.10-Y.00))/2), horiz=TRUE, at=1, width=.03)
points(c(mean(((Y.11-Y.01)+(Y.10-Y.00))/2), mean(((Y.11-Y.10)+(Y.01-Y.00))/2)), c(1,2), pch=19)

text(mean(((Y.11-Y.10)+(Y.01-Y.00))/2), 2, round(mean(((Y.11-Y.10)+(Y.01-Y.00))/2), 2), pos=1, cex=.8)
text(mean(((Y.11-Y.10)+(Y.01-Y.00))/2), 2-.1, paste("[", round(quantile((((Y.11-Y.10)+(Y.01-Y.00))/2), c(.025)), 2), " ", round(quantile((((Y.11-Y.10)+(Y.01-Y.00))/2), c(.975)),2), "]",sep="") , pos=1, cex=.8)

text(mean(((Y.11-Y.01)+(Y.10-Y.00))/2), 1, round(mean(((Y.11-Y.01)+(Y.10-Y.00))/2), 2), pos=1, cex=.8)
text(mean(((Y.11-Y.01)+(Y.10-Y.00))/2), 1-.1, paste("[", round(quantile((((Y.11-Y.01)+(Y.10-Y.00))/2), c(.025)), 2), " ", round(quantile((((Y.11-Y.01)+(Y.10-Y.00))/2), c(.975)),2), "]",sep="") , pos=1, cex=.8)

mtext(" ''Use Political Voice to Solve Problems Peacefully'' ", side=3, line=0, cex=.8, at=0)


########################################################################################################
# Mechanism: Time to Improve Relations with Other Ethnic Groups. 

# Mediator Equation
m.sv.ptg <- ictreg.joint(sexaussault ~ female + age10 + edu.m + edu.h + tamil + eastern + assist.army + displace + D3a, 
                         J=3, 
                         data=data[is.na(data$H2)==F & is.na(data$H3a)==F,], 
                         treat="treatment",
                         outcome="C34d",
                         outcome.reg="linear",
                         constrained=TRUE,
                         maxIter = 20000)
summary(m.sv.ptg)

# Outcome Equation
m.ptg.sp <- ictreg.joint(sexaussault ~ female + age10 + edu.m + edu.h + tamil + eastern + assist.army + displace + D3a + H2 + H3a + C34d, 
                         J=3, 
                         data=data, 
                         treat="treatment",
                         outcome="part_count",
                         outcome.reg="linear",
                         constrained=TRUE,
                         maxIter = 30000)
summary(m.ptg.sp)

########################################################################################################
# TABLE A.6.4
xtable(cbind(m.sv.ptg$par.outcome, m.sv.ptg$se.outcome))
xtable(cbind(m.ptg.sp$par.outcome, m.ptg.sp$se.outcome))

########################################################################################################

# Predicted Values
predict.ptg <- predict.ictreg.joint(m.sv.ptg, sensitive.value="both", return.draws=T)

X.1 <- as.data.frame(m.ptg.sp$x)
X.1$C34d<- predict.ptg[[1]]$Z1[,1]

X.0 <- as.data.frame(m.ptg.sp$x)
X.0$C34d <- predict.ptg[[1]]$Z0[,1]

predict.sp.1 <- predict.ictreg.joint(m.ptg.sp, newdata=X.1, sensitive.value="both", return.draws=T)
predict.sp.0 <- predict.ictreg.joint(m.ptg.sp, newdata=X.0, sensitive.value="both", return.draws=T)

# Y(M=1, T=1)
Y.11 <- mean(predict.sp.1[[1]]$Z1[,1])

# Y(M=1, T=0)
Y.10 <- mean(predict.sp.1[[1]]$Z0[,1])

# Y(M=0, T=1)
Y.01 <- mean(predict.sp.0[[1]]$Z1[,1])

# Y(M=0, T=0)
Y.00 <- mean(predict.sp.0[[1]]$Z0[,1])

# ACME
Y.11-Y.01
Y.10-Y.00
((Y.11-Y.01)+(Y.10-Y.00))/2

# ANDE
Y.11-Y.10
Y.01-Y.00
((Y.11-Y.10)+(Y.01-Y.00))/2

# With Uncertainties
# Y(M(1), T=1)
Y.11 <- apply(predict.sp.1$draws.predict[[2]], 2, mean)
mean(Y.11)
quantile(Y.11, c(.025, .975))

# Y(M(1), T=0)
Y.10 <-  apply(predict.sp.1$draws.predict[[1]], 2, mean)
mean(Y.10)
quantile(Y.10, c(.025, .975))

# Y(M(0), T=1)
Y.01 <- apply(predict.sp.0$draws.predict[[2]], 2, mean)
mean(Y.01)
quantile(Y.01, c(.025, .975))

# Y(M(0), T=0)
Y.00 <- apply(predict.sp.0$draws.predict[[1]], 2, mean)
mean(Y.00)
quantile(Y.00, c(.025, .975))

# ACME
mean(Y.11-Y.01)
quantile((Y.11-Y.01), c(.025, .975))

mean(Y.10-Y.00)
quantile((Y.10-Y.00), c(.025, .975))

mean(((Y.11-Y.01)+(Y.10-Y.00))/2)
quantile((((Y.11-Y.01)+(Y.10-Y.00))/2), c(.025, .975))

# ANDE
mean(Y.11-Y.10)
quantile((Y.11-Y.10), c(.025, .975))

mean(Y.01-Y.00)
quantile((Y.01-Y.00), c(.025, .975))

mean(((Y.11-Y.10)+(Y.01-Y.00))/2)
quantile((((Y.11-Y.10)+(Y.01-Y.00))/2), c(.025, .975))

##########################################################################################################
# Figure A.6.2 (Lower Panel)

par(mar=c(3,4,1,3))
plot( c(mean(((Y.11-Y.01)+(Y.10-Y.00))/2), mean(((Y.11-Y.10)+(Y.01-Y.00))/2)), c(1,2), pch="", axes=F, ylim=c(0.5, 2.5), xlim=c(-2, 7), ylab="", xlab="Effect on Political Participation Index")
axis(1, col="white")
grid(col="lightgrey", lwd=.5, lty=1)
abline(v=0, col="grey", lwd=1.5)
axis(2, at=c(1:2), labels=c("ACME", "ANDE"), col="white")

denstrip((((Y.11-Y.10)+(Y.01-Y.00))/2), horiz=TRUE, at=2, width=.03)
denstrip((((Y.11-Y.01)+(Y.10-Y.00))/2), horiz=TRUE, at=1, width=.03)
points(c(mean(((Y.11-Y.01)+(Y.10-Y.00))/2), mean(((Y.11-Y.10)+(Y.01-Y.00))/2)), c(1,2), pch=19)

text(mean(((Y.11-Y.10)+(Y.01-Y.00))/2), 2, round(mean(((Y.11-Y.10)+(Y.01-Y.00))/2), 2), pos=1, cex=.8)
text(mean(((Y.11-Y.10)+(Y.01-Y.00))/2), 2-.1, paste("[", round(quantile((((Y.11-Y.10)+(Y.01-Y.00))/2), c(.025)), 2), " ", round(quantile((((Y.11-Y.10)+(Y.01-Y.00))/2), c(.975)),2), "]",sep="") , pos=1, cex=.8)

text(mean(((Y.11-Y.01)+(Y.10-Y.00))/2), 1, round(mean(((Y.11-Y.01)+(Y.10-Y.00))/2), 2), pos=1, cex=.8)
text(mean(((Y.11-Y.01)+(Y.10-Y.00))/2), 1-.1, paste("[", round(quantile((((Y.11-Y.01)+(Y.10-Y.00))/2), c(.025)), 2), " ", round(quantile((((Y.11-Y.01)+(Y.10-Y.00))/2), c(.975)),2), "]",sep="") , pos=1, cex=.8)

mtext(" ''Time to Improve Relations with Other Ethnic Groups.'' ", side=3, line=0, cex=.8, at=0)

########################################################################################################  
# Mechanism: Post-Traumatic Growth

# Mediator Equation
m.sv.ptg <- ictreg.joint(sexaussault ~ female + age10 + edu.m + edu.h + tamil + eastern + assist.army + D3a + displace, 
                         J=3, 
                         data=data[is.na(data$H2)==F & is.na(data$H3e)==F,], 
                         treat="treatment",
                         outcome="PTG",
                         outcome.reg="linear",
                         constrained=TRUE,
                         maxIter = 20000)
summary(m.sv.ptg)


# Outcome Equation
m.ptg.sp <- ictreg.joint(sexaussault ~ female + age10 + edu.m + edu.h + tamil + eastern + assist.army + D3a +  displace + H2 + H3e + PTG, 
                         J=3, 
                         data=data, 
                         treat="treatment",
                         outcome="part_count",
                         outcome.reg="linear",
                         constrained=TRUE,
                         maxIter = 30000)
summary(m.ptg.sp)

################################################################################################################
# TABLE A.6.5

xtable(round(cbind(m.sv.ptg$par.outcome, m.sv.ptg$se.outcome), 2))
xtable(round(cbind(m.ptg.sp$par.outcome, m.ptg.sp$se.outcome), 2))

################################################################################################################
# Predicted Values
predict.ptg <- predict.ictreg.joint(m.sv.ptg, sensitive.value="both", return.draws=T)

X.1 <- as.data.frame(m.ptg.sp$x)
X.1$PTG <- predict.ptg[[1]]$Z1[,1]

X.0 <- as.data.frame(m.ptg.sp$x)
X.0$PTG <- predict.ptg[[1]]$Z0[,1]

predict.sp.1 <- predict.ictreg.joint(m.ptg.sp, newdata=X.1, sensitive.value="both", return.draws=T)
predict.sp.0 <- predict.ictreg.joint(m.ptg.sp, newdata=X.0, sensitive.value="both", return.draws=T)

# Y(M=1, T=1)
Y.11 <- mean(predict.sp.1[[1]]$Z1[,1])

# Y(M=1, T=0)
Y.10 <- mean(predict.sp.1[[1]]$Z0[,1])

# Y(M=0, T=1)
Y.01 <- mean(predict.sp.0[[1]]$Z1[,1])

# Y(M=0, T=0)
Y.00 <- mean(predict.sp.0[[1]]$Z0[,1])

# ACME
Y.11-Y.01
Y.10-Y.00
((Y.11-Y.01)+(Y.10-Y.00))/2

# ANDE
Y.11-Y.10
Y.01-Y.00
((Y.11-Y.10)+(Y.01-Y.00))/2

# With Uncertainties
# Y(M(1), T=1)
Y.11 <- apply(predict.sp.1$draws.predict[[2]], 2, mean)
mean(Y.11)
quantile(Y.11, c(.025, .975))

# Y(M(1), T=0)
Y.10 <-  apply(predict.sp.1$draws.predict[[1]], 2, mean)
mean(Y.10)
quantile(Y.10, c(.025, .975))

# Y(M(0), T=1)
Y.01 <- apply(predict.sp.0$draws.predict[[2]], 2, mean)
mean(Y.01)
quantile(Y.01, c(.025, .975))

# Y(M(0), T=0)
Y.00 <- apply(predict.sp.0$draws.predict[[1]], 2, mean)
mean(Y.00)
quantile(Y.00, c(.025, .975))

# ACME
mean(Y.11-Y.01)
quantile((Y.11-Y.01), c(.025, .975))

mean(Y.10-Y.00)
quantile((Y.10-Y.00), c(.025, .975))

mean(((Y.11-Y.01)+(Y.10-Y.00))/2)
quantile((((Y.11-Y.01)+(Y.10-Y.00))/2), c(.025, .975))

# ANDE
mean(Y.11-Y.10)
quantile((Y.11-Y.10), c(.025, .975))

mean(Y.01-Y.00)
quantile((Y.01-Y.00), c(.025, .975))

mean(((Y.11-Y.10)+(Y.01-Y.00))/2)
quantile((((Y.11-Y.10)+(Y.01-Y.00))/2), c(.025, .975))

##########################################################################################################
# Figure A.6.3 

par(mar=c(3,4,1,3))
plot( c(mean(((Y.11-Y.01)+(Y.10-Y.00))/2), mean(((Y.11-Y.10)+(Y.01-Y.00))/2)), c(1,2), pch="", axes=F, ylim=c(0.5, 2.5), xlim=c(-2, 7), ylab="", xlab="Effect on Political Participation Index")
axis(1, col="white")
grid(col="lightgrey", lwd=.5, lty=1)
abline(v=0, col="grey", lwd=1.5)
axis(2, at=c(1:2), labels=c("ACME", "ANDE"), col="white")

denstrip((((Y.11-Y.10)+(Y.01-Y.00))/2), horiz=TRUE, at=2, width=.03)
denstrip((((Y.11-Y.01)+(Y.10-Y.00))/2), horiz=TRUE, at=1, width=.03)
points(c(mean(((Y.11-Y.01)+(Y.10-Y.00))/2), mean(((Y.11-Y.10)+(Y.01-Y.00))/2)), c(1,2), pch=19)

text(mean(((Y.11-Y.10)+(Y.01-Y.00))/2), 2, round(mean(((Y.11-Y.10)+(Y.01-Y.00))/2), 2), pos=1, cex=.8)
text(mean(((Y.11-Y.10)+(Y.01-Y.00))/2), 2-.1, paste("[", round(quantile((((Y.11-Y.10)+(Y.01-Y.00))/2), c(.025)), 2), " ", round(quantile((((Y.11-Y.10)+(Y.01-Y.00))/2), c(.975)),2), "]",sep="") , pos=1, cex=.8)

text(mean(((Y.11-Y.01)+(Y.10-Y.00))/2), 1, round(mean(((Y.11-Y.01)+(Y.10-Y.00))/2), 2), pos=1, cex=.8)
text(mean(((Y.11-Y.01)+(Y.10-Y.00))/2), 1-.1, paste("[", round(quantile((((Y.11-Y.01)+(Y.10-Y.00))/2), c(.025)), 2), " ", round(quantile((((Y.11-Y.01)+(Y.10-Y.00))/2), c(.975)),2), "]",sep="") , pos=1, cex=.8)

##############################################################################################################################################
