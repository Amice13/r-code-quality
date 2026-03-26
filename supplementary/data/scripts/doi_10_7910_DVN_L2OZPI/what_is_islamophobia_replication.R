# Replication File for 
# Helbling, M. & Traunmüller: What is Islamophobia? Disentangling Citizens' Feelings
# Towards Ethnicity, Religion, and Religiosity Using a Survey Experiment
#

setwd("/YOURPATH/")

library(foreign)
library(arm)
data <- read.dta("what_is_islamophobia_replication.dta")

####################################################################################
# Re-code and Create Variables

# Demographics
data$fem <- ifelse(data$E730=="male", 0, 1)
data$age <- data$E731

data$leftright <- droplevels(data$leftright)
data$leftright  <- as.numeric(data$leftright)

# Should be allowed to vote
data$rel_vote <- droplevels(data$rel_vote)
data$rel_vote <- as.numeric(data$rel_vote)*-1 + 6

# public office
data$rel_office <- droplevels(data$rel_office)
data$rel_office <- as.numeric(data$rel_office)*-1 + 6

# Attitude toward Ban/Permit
data$rel_posban <- droplevels(data$rel_posban)
data$rel_posper <- droplevels(data$rel_posper)
data$liberal <- as.numeric(data$rel_posban)
data$liberal.2 <- as.numeric(data$rel_posper)*-1 + 6
data$liberal <- ifelse(is.na(data$liberal)==T, data$liberal.2, data$liberal)
data$permission <- ifelse(data$rel_banperm=="Authorities propose to permit", 1, 0)

# vignette characteristics
data$rel_dev <- ifelse(data$rel_devotion=="devout", 1, 0)
data$rel_rad <- ifelse(data$rel_devotion=="radical", 1, 0)

data$bulg <- ifelse(data$rel_country=="immigrants from Bulgaria", 1, 0)
data$nig <- ifelse(data$rel_country=="immigrants from Nigeria", 1, 0)

data$rel_immigrant <- ifelse(data$rel_britain==1, 0, 1)

####################################################################################
#  descriptive stats
data$inter <- interaction(data$rel_country, data$rel_devotion)
data$inter <- interaction(data$inter, data$rel_religion)

feel.means <- aggregate(data$rel_feel, by=list(g=data$inter), mean, na.rm=T)
feel.sd <- aggregate(data$rel_feel, by=list(g=data$inter), sd, na.rm=T)

#####################################################################################
# Run Models

m.0 <- lm(rel_feel ~ rel_muslim, data=data[data$ethn_brit==1,])

m.1 <- lm(rel_feel ~ rel_muslim + rel_immigrant + rel_nonpract + rel_funda, data=data[data$ethn_brit==1,])

m.2 <- lm(rel_feel ~ rel_muslim*rel_immigrant + rel_nonpract + rel_funda, data=data[data$ethn_brit==1,])

m.3 <- lm(rel_feel ~ rel_muslim*bulg + rel_muslim*nig, data=data[data$ethn_brit==1,])

sim.3 <- sim(m.3, 1000)

mean(coef(sim.3)%*%c(1, 1, 1, 0, 1, 0)) # Bulgarian Muslim
mean(coef(sim.3)%*%c(1, 0, 1, 0, 0, 0)) # Bulgarian  Christian
mean((coef(sim.3)%*%c(1, 1, 1, 0, 1, 0))-(coef(sim.3)%*%c(1, 0, 1, 0, 0, 0)))
quantile((coef(sim.3)%*%c(1, 1, 1, 0, 1, 0))-(coef(sim.3)%*%c(1, 0, 1, 0, 0, 0)), c(.025, .975))

mean(coef(sim.3)%*%c(1, 1, 0, 1, 0, 1)) # Nigerian Muslim
mean(coef(sim.3)%*%c(1, 0, 0, 1, 0, 0)) # Nigerian Christian
mean((coef(sim.3)%*%c(1, 1, 0, 1, 0, 1))-(coef(sim.3)%*%c(1, 0, 0, 1, 0, 0)))
quantile((coef(sim.3)%*%c(1, 1, 0, 1, 0, 1))-(coef(sim.3)%*%c(1, 0, 0, 1, 0, 0)), c(.025, .975))

mean(coef(sim.3)%*%c(1, 1, 0, 0, 0, 0)) # Native Muslim
mean(coef(sim.3)%*%c(1, 0, 1, 0, 0, 0)) # Native Christian
mean((coef(sim.3)%*%c(1, 1, 0, 0, 0, 0))-(coef(sim.3)%*%c(1, 0, 0, 0, 0, 0)))
quantile((coef(sim.3)%*%c(1, 1, 0, 0, 0, 0))-(coef(sim.3)%*%c(1, 0, 0, 0, 0, 0)), c(.025, .975))


### Immigrants
m.4 <- lm(rel_feel ~ rel_muslim + rel_muslim*rel_nonpract + rel_muslim*rel_funda, data=data[data$ethn_brit==1 & data$rel_immigrant==1,])

sim.4 <- sim(m.4, 1000)

im.1 <- mean(coef(sim.4)%*%c(1, 1, 1, 0, 1, 0)) # Non-practicing Muslim
ic.1 <- mean(coef(sim.4)%*%c(1, 0, 1, 0, 0, 0)) # Non-practicing Christian
id.1 <- mean((coef(sim.4)%*%c(1, 1, 1, 0, 1, 0))-(coef(sim.4)%*%c(1, 0, 1, 0, 0, 0)))
ici.1 <- quantile((coef(sim.4)%*%c(1, 1, 1, 0, 1, 0))-(coef(sim.4)%*%c(1, 0, 1, 0, 0, 0)), c(.025, .975))

im.2 <- mean(coef(sim.4)%*%c(1, 1, 0, 0, 0, 0)) # Devout Muslim
ic.2 <- mean(coef(sim.4)%*%c(1, 0, 0, 0, 0, 0)) # Devout Christian
id.2 <- mean((coef(sim.4)%*%c(1, 1, 0, 0, 0, 0))-(coef(sim.4)%*%c(1, 0, 0, 0, 0, 0)))
ici.2 <- quantile((coef(sim.4)%*%c(1, 1, 0, 0, 0, 0))-(coef(sim.4)%*%c(1, 0, 0, 0, 0, 0)), c(.025, .975))

im.3 <- mean(coef(sim.4)%*%c(1, 1, 0, 1, 0, 1)) # Radical Muslim
ic.3 <- mean(coef(sim.4)%*%c(1, 0, 0, 1, 0, 0)) # Radical Christian
id.3 <- mean((coef(sim.4)%*%c(1, 1, 0, 1, 0, 1))-(coef(sim.4)%*%c(1, 0, 0, 1, 0, 0)))
ici.3 <- quantile((coef(sim.4)%*%c(1, 1, 0, 1, 0, 1))-(coef(sim.4)%*%c(1, 0, 0, 1, 0, 0)), c(.025, .975))

### Natives
m.5 <- lm(rel_feel ~ rel_muslim + rel_muslim*rel_nonpract + rel_muslim*rel_funda, data=data[data$ethn_brit==1 & data$rel_immigrant==0,])

sim.5 <- sim(m.5, 1000)

nm.1 <- mean(coef(sim.5)%*%c(1, 1, 1, 0, 1, 0)) # Non-practicing Muslim
nc.1 <- mean(coef(sim.5)%*%c(1, 0, 1, 0, 0, 0)) # Non-practicing Christian
nd.1 <- mean((coef(sim.5)%*%c(1, 1, 1, 0, 1, 0))-(coef(sim.5)%*%c(1, 0, 1, 0, 0, 0)))
nci.1 <- quantile((coef(sim.5)%*%c(1, 1, 1, 0, 1, 0))-(coef(sim.5)%*%c(1, 0, 1, 0, 0, 0)), c(.025, .975))

nm.2 <- mean(coef(sim.5)%*%c(1, 1, 0, 0, 0, 0)) # Devout Muslim
nc.2 <-  mean(coef(sim.5)%*%c(1, 0, 0, 0, 0, 0)) # Devout Christian
nd.2 <-  mean((coef(sim.5)%*%c(1, 1, 0, 0, 0, 0))-(coef(sim.5)%*%c(1, 0, 0, 0, 0, 0)))
nci.2 <-  quantile((coef(sim.5)%*%c(1, 1, 0, 0, 0, 0))-(coef(sim.5)%*%c(1, 0, 0, 0, 0, 0)), c(.025, .975))

nm.3 <- mean(coef(sim.5)%*%c(1, 1, 0, 1, 0, 1)) # Radical Muslim
nc.3 <-  mean(coef(sim.5)%*%c(1, 0, 0, 1, 0, 0)) # Radical Christian
nd.3 <-  mean((coef(sim.5)%*%c(1, 1, 0, 1, 0, 1))-(coef(sim.5)%*%c(1, 0, 0, 1, 0, 0)))
nci.3 <-  quantile((coef(sim.5)%*%c(1, 1, 0, 1, 0, 1))-(coef(sim.5)%*%c(1, 0, 0, 1, 0, 0)), c(.025, .975))

###################################################################################
### Figure 1

labs <- c("Non-practicing", "Devout", "Radical")
par(las=1, mar=c(3,7,1,1), mgp=c(1.7,.5, 0), cex.lab=.8, cex.axis=.8, family="Helvetica Light")
plot(c(nm.1, nm.2, nm.3), c(3:1), pch=17, axes=F, ylab="", xlab="Feeling Thermometer Scores             ", xlim=c(0, 110), ylim=c(0.5,3))
axis(2, at=c(3:1), label=labs, col="white", font.axis=2)
axis(1, at=seq(10, 80, by=10))
segments(c(nm.1, nm.2, nm.3), c(3:1), c(nc.1, nc.2, nc.3), c(3:1), col="maroon3", lwd=1)
points(c(nc.1, nc.2, nc.3), c(3:1), pch=21)
points(c(nc.1, nc.2, nc.3), c(3:1), pch=19, col="white", cex=.8)
points(c(nm.1, nm.2, nm.3), c(3:1), pch=17, cex=.8)
#text(c(nc.1, nc.2, nc.3),  c(3:1), paste(round(c(nd.1, nd.2, nd.3),1), " [",
#                                   round(c(nci.1[1], nci.2[1], nci.3[1]),2), ", ",
#                                   round(c(nci.1[2], nci.2[2], nci.3[2]),2),"]",
#                                  sep="")   , pos=4, cex=.8, col="grey")

                                  
points(c(im.1, im.2, im.3), c(3:1)-.3, pch=17)
segments(c(im.1, im.2, im.3), c(3:1)-.3, c(ic.1, ic.2, ic.3), c(3:1)-.3, col="maroon3", lwd=1)
points(c(ic.1, ic.2, ic.3), c(3:1)-.3, pch=21)
points(c(ic.1, ic.2, ic.3), c(3:1)-.3, pch=19, col="white", cex=.8)
points(c(im.1, im.2, im.3), c(3:1)-.3, pch=17, cex=.8)
#text(c(im.1, im.2, ic.3),  c(3:1)-.3, paste(round(c(id.1, id.2, id.3),1), " [",
#                                         round(c(ici.1[1], ici.2[1], ici.3[1]),2), ", ",
#                                         round(c(ici.1[2], ici.2[2], ici.3[2]),2),"]",
#                                         sep="")   , pos=4, cex=.8, col="grey")

text(c(nc.1, nc.2, nc.3),  c(3:1), c("", "", "*") , pos=4)
text(c(im.1, ic.2, ic.3),  c(3:1)-.3, c("*", "*", "*") , pos=4)


text(c(nm.1, ic.1), c(3, 3-.3), c("Native", "Immigrant"), pos=2, cex=.8, col="grey")
text(c(nm.2, im.2), c(2, 2-.3), c("Native", "Immigrant"), pos=2, cex=.8, col="grey")
text(c(nm.3, im.3), c(1, 1-.3), c("Native", "Immigrant"), pos=2, cex=.8, col="grey")

points(92, 2.5, pch=21)
points(92, 2.5, pch=19, cex=.8, col="white")
points(92, 2.2, pch=17)
segments(90, 1.9, 93, 1.9, col="maroon3", lwd=1)
points(92, 1.6, pch="*")
text(93, 2.5, "Christian", pos=4, cex=.8)
text(93, 2.2, "Muslim", pos=4, cex=.8)
text(93, 1.9, "Difference", pos=4, cex=.8)
text(93, 1.6, "p < .05", pos=4, cex=.8)

abline(v=37.58917, lty=1, lwd=1, col="grey92")


####################################################################################
## "Test" for difference between radical Christian native and devout Muslim immigrant
vec.im.2 <- coef(sim.4)%*%c(1, 1, 0, 0, 0, 0)
vec.nc.3 <- coef(sim.5)%*%c(1, 0, 0, 1, 0, 0)

mean(vec.nc.3 - vec.im.2)
quantile(vec.nc.3 - vec.im.2, c(.025, .975))
#####################################################################################
# Re-Code and Create More Variables

##Liberal values
data$val_child <- data$E527
data$val_inter <- data$E528
data$val_homo <- data$E529

data$val_child <- ifelse(data$val_child=="Don\x92t know", NA, data$val_child)
data$val_inter <- ifelse(data$val_inter=="Don\x92t know", NA, data$val_inter)
data$val_homo <- ifelse(data$val_homo=="Don\x92t know", NA, data$val_homo)

data$val_child <- data$val_child-1 
data$val_inter <- data$val_inter-1
data$val_homo <- data$val_homo-1

data$libval <- (data$val_child + data$val_inter + data$val_homo)/3

data$leftright.2 <- ifelse(data$leftright>4, 1, NA)
data$leftright.2 <- ifelse(data$leftright<4, 0, data$leftright.2)

data$libval.2 <- ifelse(data$libval>2.6, 1, NA)
data$libval.2 <- ifelse(data$libval<2.6, 0, data$libval.2)


######################################################################################
# Run Models
# Left-Right-Ideology 
m.1 <- lm(rel_feel ~ rel_immigrant + rel_muslim + rel_devote + rel_funda, data=data[data$leftright<4,])
m.2 <- lm(rel_feel ~ rel_immigrant + rel_muslim + rel_devote + rel_funda, data=data[data$leftright>4,])
m.3 <- lm(rel_feel ~ rel_immigrant*leftright.2 + rel_muslim*leftright.2 + rel_devote*leftright.2 + rel_funda*leftright.2, data=data)
m.4 <- lm(rel_feel ~ rel_immigrant*leftright + rel_muslim*leftright + rel_devote*leftright + rel_funda*leftright, data=data)

m.5 <- lm(rel_feel ~ rel_immigrant*libval + rel_muslim*libval + rel_devote*libval + rel_funda*libval, data=data)
m.6 <- lm(rel_feel ~ rel_immigrant*libval.2 + rel_muslim*libval.2 + rel_devote*libval.2 + rel_funda*libval.2, data=data)


# Immigrant
li.1 <- coef(m.3)[1] + coef(m.3)[2] 
ri.1 <- coef(m.3)[1] + coef(m.3)[2] + coef(m.3)[3] + coef(m.3)[7] 
li.2 <- coef(m.6)[1] + coef(m.6)[2] 
ri.2 <- coef(m.6)[1] + coef(m.6)[2] + coef(m.6)[3] + coef(m.6)[7] 

# Muslim  
lm.1 <- coef(m.3)[1] + coef(m.3)[4] 
rm.1 <- coef(m.3)[1] + coef(m.3)[4] + coef(m.3)[3] + coef(m.3)[8] 
lm.2 <- coef(m.6)[1] + coef(m.6)[4] 
rm.2 <- coef(m.6)[1] + coef(m.6)[4] + coef(m.6)[3] + coef(m.6)[8] 

# Devout
ld.1 <- coef(m.3)[1] + coef(m.3)[5] 
rd.1 <- coef(m.3)[1] + coef(m.3)[5] + coef(m.3)[3] + coef(m.3)[9] 
ld.2 <- coef(m.6)[1] + coef(m.6)[5] 
rd.2 <- coef(m.6)[1] + coef(m.6)[5] + coef(m.6)[3] + coef(m.6)[9] 

# Radical   
lr.1 <- coef(m.3)[1] + coef(m.3)[6] 
rr.1 <- coef(m.3)[1] + coef(m.3)[6] + coef(m.3)[3] + coef(m.3)[10] 
lr.2 <- coef(m.6)[1] + coef(m.6)[6] 
rr.2 <- coef(m.6)[1] + coef(m.6)[6] + coef(m.6)[3] + coef(m.6)[10] 

#####################################################################################
### Figure 2
labs <- c("Immigrant", "Muslim", "Devout", "Radical")
par(las=1, mar=c(3,6,1,0), mgp=c(1.7,.5, 0), cex.lab=.8, cex.axis=.8, family="Helvetica Light", tck=-.005)
plot(c(li.1, lm.1, ld.1, lr.1), c(4:1), pch=17, axes=F, ylab="", xlab="            ", xlim=c(0, 125), ylim=c(0.5,4))
segments(c(ri.1, rm.1, rd.1, rr.1), c(4:1), c(li.1, lm.1, ld.1, lr.1), c(4:1), col="maroon3", lwd=1)
points(c(ri.1, rm.1, rd.1, rr.1), c(4:1), pch=21)
points(c(li.1, lm.1, ld.1, lr.1), c(4:1), pch=17)
points(c(ri.1, rm.1, rd.1, rr.1), c(4:1), pch=19, col="white", cex=.8)
mtext("Feeling Thermometer Scores", side=1, line=1.7, cex=.8, at=45)

segments(c(ri.2, rm.2, rd.2, rr.2), c(4:1)-.3, c(li.2, lm.2, ld.2, lr.2), c(4:1)-.3, col="maroon3", lwd=1)
points(c(ri.2, rm.2, rd.2, rr.2), c(4:1)-.3, pch=21)
points(c(li.2, lm.2, ld.2, lr.2), c(4:1)-.3, pch=17)
points(c(ri.2, rm.2, rd.2, rr.2), c(4:1)-.3, pch=19, cex=.8, col="white")

axis(2, at=c(4:1), label=labs, col="white", font.axis=2)
axis(1, at=seq(10, 80, by=10))

text(c(ri.1, ri.2), c(4, 4-.3), c("Left-Right Ideology", "Cultural Liberalism"), pos=2, cex=.8, col="grey")
text(c(rm.1, rm.2), c(3, 3-.3), c("Left-Right Ideology", "Cultural Liberalism"), pos=2, cex=.8, col="grey")
text(c(ld.1, ld.2), c(2, 2-.3), c("Left-Right Ideology", "Cultural Liberalism"), pos=2, cex=.8, col="grey")
text(c(lr.1, lr.2), c(1, 1-.3), c("Left-Right", "Liberalism"), pos=2, cex=.8, col="grey")

#text(c(li.1, lm.1, rd.1, rr.1),  c(4:1), paste(round(c(coef(m.3)[7], coef(m.3)[8], coef(m.3)[9], coef(m.3)[10]),2),
#                                               " [",
#                                               round(c(coef(m.3)[7]-1.96*se.coef(m.3)[7], coef(m.3)[8]-1.96*se.coef(m.3)[8], coef(m.3)[9]-1.96*se.coef(m.3)[9], coef(m.3)[10]-1.96*se.coef(m.3)[10]),2),
#                                               ", ",
#                                               round(c(coef(m.3)[7]+1.96*se.coef(m.3)[7], coef(m.3)[8]+1.96*se.coef(m.3)[8], coef(m.3)[9]+1.96*se.coef(m.3)[9], coef(m.3)[10]+1.96*se.coef(m.3)[10]),2),
#                                               "]", sep=""),
#                                               pos=4, cex=.8, col="grey")
     
#text(c(li.2, lm.2, rd.2, rr.2),  c(4:1)-.3, paste(round(c(coef(m.6)[7], coef(m.6)[8], coef(m.6)[9], coef(m.6)[10]),2),
#                                               " [",
#                                               round(c(coef(m.6)[7]-1.96*se.coef(m.6)[7], coef(m.6)[8]-1.96*se.coef(m.6)[8], coef(m.6)[9]-1.96*se.coef(m.6)[9], coef(m.6)[10]-1.96*se.coef(m.6)[10]),2),
#                                               ", ",
#                                               round(c(coef(m.6)[7]+1.96*se.coef(m.6)[7], coef(m.6)[8]+1.96*se.coef(m.6)[8], coef(m.6)[9]+1.96*se.coef(m.6)[9], coef(m.6)[10]+1.96*se.coef(m.6)[10]),2),
#                                               "]", sep=""),
#                                                pos=4, cex=.8, col="grey")


text(c(li.1, lm.1, rd.1, rr.1),  c(4:1), c("*", "*", "*", "*"), pos=4)
text(c(li.2, lm.2, rd.2, rr.2),  c(4:1)-.3, c("*", "*", "*", "*"), pos=4)     
     
     
points(90, 2.5, pch=21)
points(90, 2.5, pch=19, cex=.8, col="white")
points(90, 2.2, pch=17)
segments(88, 1.9, 91, 1.9, col="maroon3", lwd=1)
points(90, 1.6, pch="*")
text(91, 2.8, "Respondents:", pos=4, cex=.8, font=2)
text(91, 2.5, "Right/Conservative", pos=4, cex=.8)
text(91, 2.2, "Left/Liberal", pos=4, cex=.8)
text(91, 1.9, "Difference", pos=4, cex=.8)
text(91, 1.6, "p < .05", pos=4, cex=.8)
####################################################################################
# Run Models

m.3.b <- lm(rel_feel ~  rel_muslim*rel_devote + rel_muslim*rel_funda, data=data[data$leftright.2==1,])
m.3.c <- lm(rel_feel ~  rel_muslim*rel_devote + rel_muslim*rel_funda, data=data[data$leftright.2==0,])
m.3.e <- lm(rel_feel ~  rel_muslim*rel_devote + rel_muslim*rel_funda, data=data[data$libval.2==0,])
m.3.d <- lm(rel_feel ~  rel_muslim*rel_devote + rel_muslim*rel_funda, data=data[data$libval.2==1,])


# Secular Muslims
rsm.1 <- coef(m.3.b)[1] + coef(m.3.b)[2]
lsm.1 <- coef(m.3.c)[1] + coef(m.3.c)[2]
lsm.2 <- coef(m.3.e)[1] + coef(m.3.e)[2]
rsm.2 <- coef(m.3.d)[1] + coef(m.3.d)[2]

# Secular Christians
rsc.1 <- coef(m.3.b)[1] 
lsc.1 <- coef(m.3.c)[1]
lsc.2 <- coef(m.3.e)[1]
rsc.2 <- coef(m.3.d)[1]

# Devout Muslims
rdm.1 <- coef(m.3.b)[1] + coef(m.3.b)[2] + coef(m.3.b)[3] + coef(m.3.b)[5] 
ldm.1 <- coef(m.3.c)[1] + coef(m.3.c)[2] + coef(m.3.c)[3] + coef(m.3.c)[5] 
ldm.2 <- coef(m.3.e)[1] + coef(m.3.e)[2] + coef(m.3.e)[3] + coef(m.3.e)[5] 
rdm.2 <- coef(m.3.d)[1] + coef(m.3.d)[2] + coef(m.3.d)[3] + coef(m.3.d)[5] 

# Devout Christians
rdc.1 <- coef(m.3.b)[1] + coef(m.3.b)[3] 
ldc.1 <- coef(m.3.c)[1] + coef(m.3.c)[3] 
ldc.2 <- coef(m.3.e)[1] + coef(m.3.e)[3] 
rdc.2 <- coef(m.3.d)[1] + coef(m.3.d)[3] 

# Radical Muslims
rrm.1 <- coef(m.3.b)[1] + coef(m.3.b)[2] + coef(m.3.b)[4] + coef(m.3.b)[6]
lrm.1 <- coef(m.3.c)[1] + coef(m.3.c)[2] + coef(m.3.c)[4] + coef(m.3.c)[6]
lrm.2 <- coef(m.3.e)[1] + coef(m.3.e)[2] + coef(m.3.e)[4] + coef(m.3.e)[6]
rrm.2 <- coef(m.3.d)[1] + coef(m.3.d)[2] + coef(m.3.d)[4] + coef(m.3.d)[6]

# Radical Christians
rrc.1 <- coef(m.3.b)[1] + coef(m.3.b)[4] 
lrc.1 <- coef(m.3.c)[1] + coef(m.3.c)[4] 
lrc.2 <- coef(m.3.e)[1] + coef(m.3.e)[4] 
rrc.2 <- coef(m.3.d)[1] + coef(m.3.d)[4] 

m.3.f <- lm(rel_feel ~  rel_muslim*rel_devote*leftright.2 + rel_muslim*rel_funda*leftright.2, data=data)
sim.3.f <- sim(m.3.f, 1000)

slsm.1 <- coef(sim.3.f)[,1]+coef(sim.3.f)[,2]+coef(sim.3.f)[,4]+coef(sim.3.f)[,7]
srsm.1 <- coef(sim.3.f)[,1]+coef(sim.3.f)[,2]

slsc.1 <- coef(sim.3.f)[,1]+coef(sim.3.f)[,4]
srsc.1 <- coef(sim.3.f)[,1]

sldm.1 <- coef(sim.3.f)[,1]+coef(sim.3.f)[,2]+coef(sim.3.f)[,3]+coef(sim.3.f)[,6]
srdm.1 <- coef(sim.3.f)[,1]+coef(sim.3.f)[,2]+coef(sim.3.f)[,3]+coef(sim.3.f)[,4]+coef(sim.3.f)[,6]+coef(sim.3.f)[,7]+coef(sim.3.f)[,8]+coef(sim.3.f)[,11]

sldc.1 <- coef(sim.3.f)[,1]+coef(sim.3.f)[,3]
srdc.1 <- coef(sim.3.f)[,1]+coef(sim.3.f)[,3]+coef(sim.3.f)[,4]+coef(sim.3.f)[,8]

slrm.1 <- coef(sim.3.f)[,1]+coef(sim.3.f)[,2]+coef(sim.3.f)[,5]+coef(sim.3.f)[,9]
srrm.1 <- coef(sim.3.f)[,1]+coef(sim.3.f)[,2]+coef(sim.3.f)[,4]+coef(sim.3.f)[,5]+coef(sim.3.f)[,7]+coef(sim.3.f)[,9]+coef(sim.3.f)[,10]+coef(sim.3.f)[,12]

slrc.1 <- coef(sim.3.f)[,1]+coef(sim.3.f)[,5]
srrc.1 <- coef(sim.3.f)[,1]+coef(sim.3.f)[,4]+coef(sim.3.f)[,5]+coef(sim.3.f)[,10]


mean(srsm.1-slsm.1)
quantile(srsm.1-slsm.1, c(.025, .975))

mean(srsc.1-slsc.1)
quantile(srsc.1-slsc.1, c(.025, .975))

mean(srdm.1-sldm.1)
quantile(srdm.1-sldm.1, c(.025, .975))

mean(srdc.1-sldc.1)
quantile(srdc.1-sldc.1, c(.025, .975))

mean(srrm.1-slrm.1)
quantile(srrm.1-slrm.1, c(.025, .975))

mean(srrc.1-slrc.1)
quantile(srrc.1-slrc.1, c(.025, .975))

m.3.g <- lm(rel_feel ~  rel_muslim*rel_devote*libval.2 + rel_muslim*rel_funda*libval.2, data=data)
sim.3.g <- sim(m.3.g, 1000)

slsm.2 <- coef(sim.3.g)[,1]+coef(sim.3.g)[,2]+coef(sim.3.g)[,4]+coef(sim.3.g)[,7]
srsm.2 <- coef(sim.3.g)[,1]+coef(sim.3.g)[,2]

slsc.2 <- coef(sim.3.g)[,1]+coef(sim.3.g)[,4]
srsc.2 <- coef(sim.3.g)[,1]

sldm.2 <- coef(sim.3.g)[,1]+coef(sim.3.g)[,2]+coef(sim.3.g)[,3]+coef(sim.3.g)[,6]
srdm.2 <- coef(sim.3.g)[,1]+coef(sim.3.g)[,2]+coef(sim.3.g)[,3]+coef(sim.3.g)[,4]+coef(sim.3.g)[,6]+coef(sim.3.g)[,7]+coef(sim.3.g)[,8]+coef(sim.3.g)[,11]

sldc.2 <- coef(sim.3.g)[,1]+coef(sim.3.g)[,3]
srdc.2 <- coef(sim.3.g)[,1]+coef(sim.3.g)[,3]+coef(sim.3.g)[,4]+coef(sim.3.g)[,8]

slrm.2 <- coef(sim.3.g)[,1]+coef(sim.3.g)[,2]+coef(sim.3.g)[,5]+coef(sim.3.g)[,9]
srrm.2 <- coef(sim.3.g)[,1]+coef(sim.3.g)[,2]+coef(sim.3.g)[,4]+coef(sim.3.g)[,5]+coef(sim.3.g)[,7]+coef(sim.3.g)[,9]+coef(sim.3.g)[,10]+coef(sim.3.g)[,12]

slrc.2 <- coef(sim.3.g)[,1]+coef(sim.3.g)[,5]
srrc.2 <- coef(sim.3.g)[,1]+coef(sim.3.g)[,4]+coef(sim.3.g)[,5]+coef(sim.3.g)[,10]

mean(srsm.2-slsm.2)
quantile(srsm.2-slsm.2, c(.025, .975))

mean(srsc.2-slsc.2)
quantile(srsc.2-slsc.2, c(.025, .975))

mean(srdm.2-sldm.2)
quantile(srdm.2-sldm.2, c(.025, .975))

mean(srdc.2-sldc.2)
quantile(srdc.2-sldc.2, c(.025, .975))

mean(srrm.2-slrm.2)
quantile(srrm.2-slrm.2, c(.025, .975))

mean(srrc.2-slrc.2)
quantile(srrc.2-slrc.2, c(.025, .975))

####################################################################################
### Figure 3
labs <- c("Secular Muslim", "Secular Christian", "Devout Muslim", "Devout Christian", "Radical Muslim", "Radical Christian")
par(las=1, mar=c(3,8,3,1), mgp=c(1.7,.5, 0), cex.lab=.8, cex.axis=.8, family="Helvetica Light", tck=-.005)
plot(c(lsm.1, lsc.1, ldm.1, ldc.1, lrm.1, lrc.1), c(6:1), pch=17, axes=F, ylab="", xlab=" ", xlim=c(0, 125), ylim=c(0.5,6.5))
mtext("Feeling Thermometer Scores", side=1, line=1.7, cex=.8, at=45)

points(c(rsm.1, rsc.1, rdm.1, rdc.1, rrm.1, rrc.1), c(6:1), pch=21)
segments(c(rsm.1, rsc.1, rdm.1, rdc.1, rrm.1, rrc.1), c(6:1), c(lsm.1, lsc.1, ldm.1, ldc.1, lrm.1, lrc.1), c(6:1), col="maroon3", lwd=1)
points(c(lsm.1, lsc.1, ldm.1, ldc.1, lrm.1, lrc.1), c(6:1), pch=17)
points(c(rsm.1, rsc.1, rdm.1, rdc.1, rrm.1, rrc.1), c(6:1), pch=19, cex=.8, col="white")

points(c(rsm.2, rsc.2, rdm.2, rdc.2, rrm.2, rrc.2), c(6:1)-.3, pch=21)
segments(c(rsm.2, rsc.2, rdm.2, rdc.2, rrm.2, rrc.2), c(6:1)-.3, c(lsm.2, lsc.2, ldm.2, ldc.2, lrm.2, lrc.2), c(6:1)-.3, col="maroon3", lwd=1)
points(c(lsm.2, lsc.2, ldm.2, ldc.2, lrm.2, lrc.2), c(6:1)-.3, pch=17)
points(c(rsm.2, rsc.2, rdm.2, rdc.2, rrm.2, rrc.2), c(6:1)-.3, pch=19, cex=.8, col="white")

axis(2, at=c(6:1), label=labs, col="white", font.axis=2)
axis(1, at=seq(10, 80, by=10))
axis(3, at=seq(10, 80, by=10))

text(c(rsm.1, rsm.2), c(6, 6-.3), c("Left-Right Ideology", "Cultural Liberalism"), pos=2, cex=.8, col="grey")
text(c(rsc.1, rsc.2), c(5, 5-.3), c("Left-Right Ideology", "Cultural Liberalism"), pos=2, cex=.8, col="grey")
text(c(rdm.1, rdm.2), c(4, 4-.3), c("Left-Right Ideology", "Cultural Liberalism"), pos=2, cex=.8, col="grey")
text(c(rdc.1, ldc.2), c(3, 3-.3), c("Left-Right Ideology", "Cultural Liberalism"), pos=2, cex=.8, col="grey")
text(c(rrm.1, rrm.2), c(2, 2-.3), c("Ideol.", "Liberal."), pos=2, cex=.8, col="grey")
text(c(lrc.1, lrc.2), c(1, 1-.3), c("Left-Right", "Liberalism"), pos=2, cex=.8, col="grey")

#text(c(ldm.1, ldc.1, lrm.1, rrc.1),  c(4:1), paste(round(c(mean(srdm.1-sldm.1), mean(srdc.1-sldc.1), mean(srrm.1-slrm.1), mean(srrc.1-slrc.1)),2),
#                                                   " [",
#                                                   round(c(quantile(srdm.1-sldm.1, c(.025)), quantile(srdc.1-sldc.1, c(.025)), quantile(srrm.1-slrm.1, c(.025)), quantile(srrc.1-slrc.1, c(.025))),2),
#                                                   ", ",
#                                                   round(c(quantile(srdm.1-sldm.1, c(.0975)), quantile(srdc.1-sldc.1, c(.0975)), quantile(srrm.1-slrm.1, c(.0975)), quantile(srrc.1-slrc.1, c(.0975))),2),
#                                                   "]", sep=""),
#     pos=4, cex=.8, col="grey")

# text(c(ldm.2, rdc.2, lrm.2, rrc.2),  c(4:1)-.3, paste(round(c(mean(srdm.2-sldm.2), mean(srdc.2-sldc.2), mean(srrm.2-slrm.2), mean(srrc.2-slrc.2)),2),
#                                                   " [",
#                                                   round(c(quantile(srdm.2-sldm.2, c(.025)), quantile(srdc.2-sldc.2, c(.025)), quantile(srrm.2-slrm.2, c(.025)), quantile(srrc.2-slrc.2, c(.025))),2),
#                                                   ", ",
#                                                   round(c(quantile(srdm.2-sldm.2, c(.0975)), quantile(srdc.2-sldc.2, c(.0975)), quantile(srrm.2-slrm.2, c(.0975)), quantile(srrc.2-slrc.2, c(.0975))),2),
#                                                   "]", sep=""),
#     pos=4, cex=.8, col="grey")


text(c(lsm.1, lsc.1, ldm.1, ldc.1, lrm.1, rrc.1),  c(6:1), c("*", "*", "*", "", "*", ""), pos=4)
text(c(lsm.2, lsc.2, ldm.2, rdc.2, lrm.2, rrc.2),  c(6:1)-.3, c("*", "*", "*", "*", "", "*"), pos=4)


points(92, 4.5, pch=21)
points(92, 4.5, pch=19, cex=.8, col="white")
points(92, 4.2, pch=17)
segments(90, 3.9, 93, 3.9, col="maroon3", lwd=1)
points(92, 3.6, pch="*")
text(93, 4.8, "Respondents:", pos=4, cex=.8, font=2)
text(93, 4.5, "Right/Conserv.", pos=4, cex=.8)
text(93, 4.2, "Left/Liberal", pos=4, cex=.8)
text(93, 3.9, "Difference", pos=4, cex=.8)
text(93, 3.6, "p < .05", pos=4, cex=.8)
####################################################################################
#### Run Models
#### Respondents' Religiosity

nop.1 <-  lm(rel_feel ~ rel_immigrant + rel_muslim + rel_devote + rel_funda, data=data[data$relig_both<1.1,])
dev.1 <-  lm(rel_feel ~ rel_immigrant + rel_muslim + rel_devote + rel_funda, data=data[data$relig_both>1.1 & data$funda_both<3.1,])
fun.1 <-  lm(rel_feel ~ rel_immigrant + rel_muslim + rel_devote + rel_funda, data=data[data$funda_both>3,])

sim.nop.1 <- sim(nop.1, 1000)
sim.dev.1 <- sim(dev.1, 1000)
sim.fun.1 <- sim(fun.1, 1000)

nop.1.i <- coef(sim.nop.1)[,1] + coef(sim.nop.1)[,2]  
nop.1.m <- coef(sim.nop.1)[,1] + coef(sim.nop.1)[,3]  
nop.1.d <- coef(sim.nop.1)[,1] + coef(sim.nop.1)[,4]  
nop.1.r <- coef(sim.nop.1)[,1] + coef(sim.nop.1)[,5]  

dev.1.i <- coef(sim.dev.1)[,1] + coef(sim.dev.1)[,2]  
dev.1.m <- coef(sim.dev.1)[,1] + coef(sim.dev.1)[,3]  
dev.1.d <- coef(sim.dev.1)[,1] + coef(sim.dev.1)[,4]  
dev.1.r <- coef(sim.dev.1)[,1] + coef(sim.dev.1)[,5]  

fun.1.i <- coef(sim.fun.1)[,1] + coef(sim.fun.1)[,2]  
fun.1.m <- coef(sim.fun.1)[,1] + coef(sim.fun.1)[,3]  
fun.1.d <- coef(sim.fun.1)[,1] + coef(sim.fun.1)[,4]  
fun.1.r <- coef(sim.fun.1)[,1] + coef(sim.fun.1)[,5]  

#######################################################################################
### Figure 4
labs <- c("Immigrant", "Muslim", "Devout", "Radical")
par(las=1, mar=c(3,6,1,1), mgp=c(1.7,.5, 0), cex.lab=.8, cex.axis=.8, family="Helvetica Light", tck=-.005)
plot(c(mean(nop.1.i), mean(nop.1.m), mean(nop.1.d), mean(nop.1.r)), c(4:1), pch=21, axes=F, ylab="", xlab=" ", xlim=c(0, 125), ylim=c(0.5,4))
mtext("Feeling Thermometer Scores", side=1, line=1.7, cex=.8, at=45)

points(c(mean(dev.1.i), mean(dev.1.m), mean(dev.1.d), mean(dev.1.r)), c(4:1)-.1, pch=19, col="grey")
points(c(mean(fun.1.i), mean(fun.1.m), mean(fun.1.d), mean(fun.1.r)), c(4:1)-.2, pch=19)
axis(2, at=c(4:1), label=labs, col="white", font.axis=2)
axis(1, at=seq(10, 80, by=10))

segments(c(quantile(nop.1.i, .025), quantile(nop.1.m, .025), quantile(nop.1.d, .025), quantile(nop.1.r, .025)), c(4:1), c(quantile(nop.1.i, .975), quantile(nop.1.m, .975), quantile(nop.1.d, .975), quantile(nop.1.r, .975)), c(4:1))
segments(c(quantile(dev.1.i, .025), quantile(dev.1.m, .025), quantile(dev.1.d, .025), quantile(dev.1.r, .025)), c(4:1)-.1, c(quantile(dev.1.i, .975), quantile(dev.1.m, .975), quantile(dev.1.d, .975), quantile(dev.1.r, .975)), c(4:1)-.1, col="grey")
segments(c(quantile(fun.1.i, .025), quantile(fun.1.m, .025), quantile(fun.1.d, .025), quantile(fun.1.r, .025)), c(4:1)-.2, c(quantile(fun.1.i, .975), quantile(fun.1.m, .975), quantile(fun.1.d, .975), quantile(fun.1.r, .975)), c(4:1)-.2, col="black")
points(c(mean(nop.1.i), mean(nop.1.m), mean(nop.1.d), mean(nop.1.r)), c(4:1), pch=19, cex=.8, col="white")

points(92, 2.5, pch=21)
points(92, 2.5, pch=19, cex=.8, col="white")
points(92, 2.2, pch=19, col="grey")
points(92, 1.9, pch=19, col="black")
text(93, 2.8, "Respondents:", pos=4, cex=.8, font=2)
text(93, 2.5, "Non-practicing", pos=4, cex=.8)
text(93, 2.2, "Devout", pos=4, cex=.8)
text(93, 1.9, "Radical", pos=4, cex=.8)

###########################################################################################

nop.2 <-  lm(rel_feel ~ rel_muslim*rel_devote + rel_muslim*rel_funda, data=data[data$relig_both<1.1,])
dev.2 <-  lm(rel_feel ~ rel_muslim*rel_devote + rel_muslim*rel_funda, data=data[data$relig_both>1.1 & data$funda_both<3.1,])
fun.2 <-  lm(rel_feel ~ rel_muslim*rel_devote + rel_muslim*rel_funda, data=data[data$funda_both>3,])

sim.nop.2 <- sim(nop.2, 1000)
sim.dev.2 <- sim(dev.2, 1000)
sim.fun.2 <- sim(fun.2, 1000)

nop.2.sm <- coef(sim.nop.2)[,1] + coef(sim.nop.2)[,2]  
nop.2.sc <- coef(sim.nop.2)[,1]  
nop.2.dm <- coef(sim.nop.2)[,1] + coef(sim.nop.2)[,2] + coef(sim.nop.2)[,3] + coef(sim.nop.2)[,5]   
nop.2.dc <- coef(sim.nop.2)[,1] + coef(sim.nop.2)[,3]  
nop.2.rm <- coef(sim.nop.2)[,1] + coef(sim.nop.2)[,2] + coef(sim.nop.2)[,4] + coef(sim.nop.2)[,6]
nop.2.rc <- coef(sim.nop.2)[,1] + coef(sim.nop.2)[,4] 

dev.2.sm <- coef(sim.dev.2)[,1] + coef(sim.dev.2)[,2]  
dev.2.sc <- coef(sim.dev.2)[,1]  
dev.2.dm <- coef(sim.dev.2)[,1] + coef(sim.dev.2)[,2] + coef(sim.dev.2)[,3] + coef(sim.dev.2)[,5]   
dev.2.dc <- coef(sim.dev.2)[,1] + coef(sim.dev.2)[,3]  
dev.2.rm <- coef(sim.dev.2)[,1] + coef(sim.dev.2)[,2] + coef(sim.dev.2)[,4] + coef(sim.dev.2)[,6]
dev.2.rc <- coef(sim.dev.2)[,1] + coef(sim.dev.2)[,4] 

fun.2.sm <- coef(sim.fun.2)[,1] + coef(sim.fun.2)[,2]  
fun.2.sc <- coef(sim.fun.2)[,1]  
fun.2.dm <- coef(sim.fun.2)[,1] + coef(sim.fun.2)[,2] + coef(sim.fun.2)[,3] + coef(sim.fun.2)[,5]   
fun.2.dc <- coef(sim.fun.2)[,1] + coef(sim.fun.2)[,3]  
fun.2.rm <- coef(sim.fun.2)[,1] + coef(sim.fun.2)[,2] + coef(sim.fun.2)[,4] + coef(sim.fun.2)[,6]
fun.2.rc <- coef(sim.fun.2)[,1] + coef(sim.fun.2)[,4] 


#######################################################################################
### Figure 5
labs <- c("Secular Muslim", "Secular Christian", "Devout Muslim", "Devout Christian", "Radical Muslim", "Radical Christian")
par(las=1, mar=c(3,6,3,1), mgp=c(1.7,.5, 0), cex.lab=.8, cex.axis=.8, family="Helvetica Light", tck=-.02, lwd=1)
plot(c(mean(nop.2.sm), mean(nop.2.sc), mean(nop.2.dm), mean(nop.2.dc), mean(nop.2.rm), mean(nop.2.rc)), c(6:1), pch=21, axes=F, ylab="", xlab="Feeling Thermometer Scores                 ", xlim=c(0, 125), ylim=c(0.5,6.5))
points(c(mean(dev.2.sm), mean(dev.2.sc), mean(dev.2.dm), mean(dev.2.dc), mean(dev.2.rm), mean(dev.2.rc)), c(6:1)-.1, pch=19, col="grey")
points(c(mean(fun.2.sm), mean(fun.2.sc), mean(fun.2.dm), mean(fun.2.dc), mean(fun.2.rm), mean(fun.2.rc)), c(6:1)-.2, pch=19)
axis(2, at=c(6:1), label=labs, col="white", font.axis=2)
axis(1, at=seq(10, 80, by=10))
axis(3, at=seq(10, 80, by=10))

segments(c(quantile(nop.2.sm, .025), quantile(nop.2.sc, .025), quantile(nop.2.dm, .025), quantile(nop.2.dc, .025), quantile(nop.2.rm, .025), quantile(nop.2.rc, .025)), c(6:1), c(quantile(nop.2.sm, .975), quantile(nop.2.sc, .975), quantile(nop.2.dm, .975), quantile(nop.2.dc, .975), quantile(nop.2.rm, .975), quantile(nop.2.rc, .975)), c(6:1))
segments(c(quantile(dev.2.sm, .025), quantile(dev.2.sc, .025), quantile(dev.2.dm, .025), quantile(dev.2.dc, .025), quantile(dev.2.rm, .025), quantile(dev.2.rc, .025)), c(6:1)-.1, c(quantile(dev.2.sm, .975), quantile(dev.2.sc, .975), quantile(dev.2.dm, .975), quantile(dev.2.dc, .975), quantile(dev.2.rm, .975), quantile(dev.2.rc, .975)), c(6:1)-.1, col="grey")
segments(c(quantile(fun.2.sm, .025), quantile(fun.2.sc, .025), quantile(fun.2.dm, .025), quantile(fun.2.dc, .025), quantile(fun.2.rm, .025), quantile(fun.2.rc, .025)), c(6:1)-.2, c(quantile(fun.2.sm, .975), quantile(fun.2.sc, .975), quantile(fun.2.dm, .975), quantile(fun.2.dc, .975), quantile(fun.2.rm, .975), quantile(fun.2.rc, .975)), c(6:1)-.2)
points(c(mean(nop.2.sm), mean(nop.2.sc), mean(nop.2.dm), mean(nop.2.dc), mean(nop.2.rm), mean(nop.2.rc)), c(6:1), pch=19, cex=.8, col="white")


points(92, 4.5, pch=21)
points(92, 4.5, pch=19, cex=.8, col="white")
points(92, 4.2, pch=19, col="grey")
points(92, 3.9, pch=19, col="black")
text(93, 4.8, "Respondents:", pos=4, cex=.8, font=2)
text(93, 4.5, "Non-practicing", pos=4, cex=.8)
text(93, 4.2, "Devout", pos=4, cex=.8)
text(93, 3.9, "Radical", pos=4, cex=.8)
#####################################################################################








