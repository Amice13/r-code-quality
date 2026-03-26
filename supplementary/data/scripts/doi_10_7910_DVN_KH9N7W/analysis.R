
rm(list=ls(all=TRUE))

# path needs to be adjusted. Please set the working directory to one level higher than the subfolder for the code so that subsequent code to load data and save files to "output" folder executes
setwd("")

# install required packages
#install.packages(c("lfe", "MCMCpack", "MASS", "xtable"))

# load packages
library(lfe)
library(MCMCpack)
library(MASS)
library(xtable)


# Functions
makeTransparent<-function(someColor, alpha=100){
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
    blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
}

siglevel <- function(i){
	sig <- ifelse(sign(msum$quantiles[i,3])==sign(msum$quantiles[i,5]), "†", "")
	sig <- ifelse(sign(msum$quantiles[i,2])==sign(msum$quantiles[i,6]), "*", sig)
	sig <- ifelse(sign(msum$quantiles[i,1])==sign(msum$quantiles[i,7]), "**", sig)
	sig
}


# load data
load("data/data_combined.rda")
load("data/data_qpr_uk.rda")


# set seed
set.seed(123)




### FIGURE 1

usedata <- data[data$country=="uk",]
usedata$name <- paste0(usedata$name.1, " - ", usedata$name.2)

pdf(width=5*1.62, height=5, file="output/graphs/fig_1.pdf")
par(mar = c(3,3,1,2), mgp=c(2,0.5,0))
plot(qpr$year, qpr$qpr.mean, type="n", xlab="Year", ylab="Relationship Score", xlim=c(2000, 2015))
text(qpr$year, qpr$qpr.mean, qpr$name, cex=0.5, col="grey60")
text(usedata$year, usedata$qpr.mean, usedata$name, cex=0.65)
dev.off()




### TABLE 1 A

tab <- NULL

# perceived distance
collect <- NULL
for(i in 1:1000){
	collect <- c(collect, data[[paste0("lr.avg.diff.", i)]])
}
tab <- rbind(tab, c("Perceived distance between parties i,j", round(mean(collect), 2), round(sd(collect), 2), round(min(collect), 2), round(max(collect), 2)))

# QPR
collect <- NULL
for(i in 1:1000){
	collect <- c(collect, data[[paste0("qpr.", i)]])
}
tab <- rbind(tab, c("Dyadic relationshipscore for parties i,j", round(mean(collect), 2), round(sd(collect), 2), round(min(collect), 2), round(max(collect), 2)))

# Manifesto difference
collect <- NULL
for(i in 1:1000){
	collect <- c(collect, data[[paste0("lr.cmp.diff.", i)]])
}
tab <- rbind(tab, c("Manifesto-based distance between parties i,j", round(mean(collect), 2), round(sd(collect), 2), round(min(collect), 2), round(max(collect), 2)))

# Coalition
tab <- rbind(tab, c("i,j are coalition partners", round(mean(data$coalition), 2), round(sd(data$coalition), 2), round(min(data$coalition), 2), round(max(data$coalition), 2)))

# Opposition
tab <- rbind(tab, c("i,j are both in opposition", round(mean(data$opposition), 2), round(sd(data$opposition), 2), round(min(data$opposition), 2), round(max(data$opposition), 2)))

colnames(tab) <- c("Variable", "Mean", "Std. Dev.", "Min", "Max")

outtable <- xtable(tab)
print(outtable, file="output/tables/tab_1a.tex")


### TABLE 1 B

tab <- NULL

# QPR for coalition
usedata <- data[data$coalition==1,]
collect <- NULL
for(i in 1:1000){
	collect <- c(collect, usedata[[paste0("qpr.", i)]][])
}
n.coal <- dim(usedata)[1]
n.coal
tab <- rbind(tab, c("Scores for coalition partners (N=60)", round(mean(collect), 2), round(sd(collect), 2), round(min(collect), 2), round(max(collect), 2)))
collect.coal <- collect


# QPR for non-coalition
usedata <- data[data$coalition==0,]
collect <- NULL
for(i in 1:1000){
	collect <- c(collect, usedata[[paste0("qpr.", i)]][])
}
n.nocoal <- dim(usedata)[1]
n.nocoal
tab <- rbind(tab, c("Scores for other party pairs (N=326)", round(mean(collect), 2), round(sd(collect), 2), round(min(collect), 2), round(max(collect), 2)))
collect.nocoal <- collect

colnames(tab) <- c("Variable: [Dyadic rel.score for parties i,j]", "Mean", "Std. Dev.", "Min", "Max")

outtable <- xtable(tab)
print(outtable, file="output/tables/tab_1b.tex")

# Difference of means t-test
(mean(collect.coal)-mean(collect.nocoal))/(sqrt(((sd(collect.coal)^2)/n.coal) + ((sd(collect.nocoal)^2)/n.nocoal) ))




### TABLE 1 C

tab <- NULL

# QPR for election years
usedata <- data[data$elec==1,]
collect <- NULL
for(i in 1:1000){
	collect <- c(collect, usedata[[paste0("qpr.", i)]][])
}
n.elec <- dim(usedata)[1]
n.elec
tab <- rbind(tab, c("Scores for national election periods (N=237)", round(mean(collect), 2), round(sd(collect), 2), round(min(collect), 2), round(max(collect), 2)))
collect.elec <- collect


# QPR for non-election years
usedata <- data[data$elec==0,]
collect <- NULL
for(i in 1:1000){
	collect <- c(collect, usedata[[paste0("qpr.", i)]][])
}
n.noelec <- dim(usedata)[1]
n.noelec
tab <- rbind(tab, c("Scores for non-national election periods (N=149)", round(mean(collect), 2), round(sd(collect), 2), round(min(collect), 2), round(max(collect), 2)))
collect.noelec <- collect

colnames(tab) <- c("Variable: [Dyadic rel.score for parties i,j]", "Mean", "Std. Dev.", "Min", "Max")

outtable <- xtable(tab)
print(outtable, file="output/tables/tab_1c.tex")

# Difference of means t-test
(mean(collect.elec)-mean(collect.noelec))/(sqrt(((sd(collect.elec)^2)/n.elec) + ((sd(collect.noelec)^2)/n.noelec) ))





### TABLE 2

# first column
draw <- r2 <- ar2 <- N <- NULL
for(i in 1:1000){
	formula <- paste0("lr.avg.diff.", i, " ~ qpr.", i, " + lr.cmp.diff.", i, " + coalition + opposition | year.month.country | 0 | dyadid")
	m <- felm(as.formula(formula),  data=data[data$elec==1,], exactDOF=T)
	
	add <- mvrnorm(500, m$beta, m$clustervcv)
	draw <- rbind(draw, add)

	r2 <- c(r2, summary(m)$r.squared)
	ar2 <- c(ar2, summary(m)$adj.r.squared)
	N <- c(N, summary(m)$N)
}

m1 <- as.mcmc(draw)
msum <- summary(m1, quantiles=c(0.005, 0.025, 0.05, 0.5, 0.95, 0.975, 0.995))

tab <- NULL
for(i in 1:4){
	add <- matrix(c(rownames(msum$statistics)[i], paste0(format(round(msum$statistics[i,1], 2), nsmall=2, scientific=10), siglevel(i)), "", paste0("(",format(round(msum$statistics[i,2], 2), nsmall=2, scientific=10), ")")), nrow=2, byrow=T)
	tab <- rbind(tab, add)
}

tab <- rbind(tab, c("N", unique(N)))
tab <- rbind(tab, c("R2", round(mean(r2), 2)))
tab <- rbind(tab, c("AR2", round(mean(ar2), 2)))

tab.1 <- tab



# second column
draw <- r2 <- ar2 <- N <- NULL
for(i in 1:1000){
	formula <- paste0("lr.avg.diff.", i, " ~ qpr.", i, " + lr.cmp.diff.", i, " + coalition + opposition | year.month.country | 0 | dyadid")
	m <- felm(as.formula(formula),  data=data[data$elec==0,], exactDOF=T)
	
	add <- mvrnorm(500, m$beta, m$clustervcv)
	draw <- rbind(draw, add)

	r2 <- c(r2, summary(m)$r.squared)
	ar2 <- c(ar2, summary(m)$adj.r.squared)
	N <- c(N, summary(m)$N)
}

m2 <- as.mcmc(draw)
msum <- summary(m2, quantiles=c(0.005, 0.025, 0.05, 0.5, 0.95, 0.975, 0.995))

tab <- NULL
for(i in 1:4){
	add <- matrix(c(rownames(msum$statistics)[i], paste0(format(round(msum$statistics[i,1], 2), nsmall=2, scientific=10), siglevel(i)), "", paste0("(",format(round(msum$statistics[i,2], 2), nsmall=2, scientific=10), ")")), nrow=2, byrow=T)
	tab <- rbind(tab, add)
}

tab <- rbind(tab, c("N", unique(N)))
tab <- rbind(tab, c("R2", round(mean(r2), 2)))
tab <- rbind(tab, c("AR2", round(mean(ar2), 2)))

tab.2 <- tab


# third column
draw <- r2 <- ar2 <- N <- NULL
for(i in 1:1000){
	formula <- paste0("lr.sophia.diff.", i, " ~ qpr.", i, " + lr.cmp.diff.", i, " + coalition + opposition | year.month.country | 0 | dyadid")
	m <- felm(as.formula(formula),  data=data[data$elec==1,], exactDOF=T)
	
	add <- mvrnorm(500, m$beta, m$clustervcv)
	draw <- rbind(draw, add)

	r2 <- c(r2, summary(m)$r.squared)
	ar2 <- c(ar2, summary(m)$adj.r.squared)
	N <- c(N, summary(m)$N)
}

m3 <- as.mcmc(draw)
msum <- summary(m3, quantiles=c(0.005, 0.025, 0.05, 0.5, 0.95, 0.975, 0.995))

tab <- NULL
for(i in 1:4){
	add <- matrix(c(rownames(msum$statistics)[i], paste0(format(round(msum$statistics[i,1], 2), nsmall=2, scientific=10), siglevel(i)), "", paste0("(",format(round(msum$statistics[i,2], 2), nsmall=2, scientific=10), ")")), nrow=2, byrow=T)
	tab <- rbind(tab, add)
}

tab <- rbind(tab, c("N", unique(N)))
tab <- rbind(tab, c("R2", round(mean(r2), 2)))
tab <- rbind(tab, c("AR2", round(mean(ar2), 2)))

tab.3 <- tab


# fourth column
draw <- r2 <- ar2 <- N <- NULL
for(i in 1:1000){
	formula <- paste0("lr.sophia.diff.", i, " ~ qpr.", i, " + lr.cmp.diff.", i, " + coalition + opposition | year.month.country | 0 | dyadid")
	m <- felm(as.formula(formula),  data=data[data$elec==0,], exactDOF=T)
	
	add <- mvrnorm(500, m$beta, m$clustervcv)
	draw <- rbind(draw, add)

	r2 <- c(r2, summary(m)$r.squared)
	ar2 <- c(ar2, summary(m)$adj.r.squared)
	N <- c(N, summary(m)$N)
}

m4 <- as.mcmc(draw)
msum <- summary(m4, quantiles=c(0.005, 0.025, 0.05, 0.5, 0.95, 0.975, 0.995))

tab <- NULL
for(i in 1:4){
	add <- matrix(c(rownames(msum$statistics)[i], paste0(format(round(msum$statistics[i,1], 2), nsmall=2, scientific=10), siglevel(i)), "", paste0("(",format(round(msum$statistics[i,2], 2), nsmall=2, scientific=10), ")")), nrow=2, byrow=T)
	tab <- rbind(tab, add)
}

tab <- rbind(tab, c("N", unique(N)))
tab <- rbind(tab, c("R2", round(mean(r2), 2)))
tab <- rbind(tab, c("AR2", round(mean(ar2), 2)))

tab.4 <- tab


# save table
vars <- c("Dyadic relationshipscore for parties i,j(t)", "", "Manifesto-based distance between parties i,j (t)", "", "i,j are coalition partners(t)", "", "i,j are both in opposition(t)", "", "N", "R2", "Adjusted R2")
outtable <- xtable(cbind(vars, tab.1[,2], tab.2[,2], tab.3[,2], tab.4[,2]))
print(outtable, file="output/tables/tab_2.tex")




## SUBSTANTIVE EFFECTS

# Election years: Effect of one SD change in QPR
collect <- NULL
for(i in 1:1000){
	collect <- c(collect, data[[paste0("qpr.", i)]])
}
sd(collect)
sd(collect)*summary(m1)$statistics[1,1]

# Election years: Effect of one SD change in QPR for coalition partners
usedata <- data[data$coalition==1,]
collect <- NULL
for(i in 1:1000){
	collect <- c(collect, usedata[[paste0("qpr.", i)]])
}
sd(collect)
sd(collect)*summary(m1)$statistics[1,1]




# Figure 2

## election years

# get an intercept: mean of all country FEs
m.int.1 <- felm(lr.avg.diff.mean ~ qpr.mean + lr.cmp.diff.mean + coalition + opposition | year.month.country | 0 | dyadid,  data=data[data$elec==1,], exactDOF=T)
m1.fe <- getfe(m.int.1)
fe <- mean(m1.fe$effect)


# coalition
vec <- seq(min(data$qpr.mean), max(data$qpr.mean), length.out=100)
values <- cbind(fe, vec, mean(data$lr.cmp.diff.mean), 1, 0)
preddist.coal.elec <- apply(values, 1, function(x)  c(fe, colMeans(m1)) %*% x)
predmat.coal.elec <- apply(values, 1, function(x)  cbind(fe, m1) %*% x)
lowervec.coal.elec <- apply(predmat.coal.elec, 2, quantile, 0.025) 
uppervec.coal.elec <- apply(predmat.coal.elec, 2, quantile, 0.975) 


## non-election years

# get an intercept: mean of all country FEs
m.int.2 <- felm(lr.avg.diff.mean ~ qpr.mean + lr.cmp.diff.mean + coalition + opposition | year.month.country | 0 | dyadid,  data=data[data$elec==0,], exactDOF=T)
m2.fe <- getfe(m.int.2)
fe <- mean(m2.fe$effect)


# coalition
vec <- seq(min(data$qpr.mean), max(data$qpr.mean), length.out=100)
values <- cbind(fe, vec, mean(data$lr.cmp.diff.mean), 1, 0)
preddist.coal.noelec <- apply(values, 1, function(x)  c(fe, colMeans(m2)) %*% x)
predmat.coal.noelec <- apply(values, 1, function(x)  cbind(fe, m2) %*% x)
lowervec.coal.noelec <- apply(predmat.coal.noelec, 2, quantile, 0.025) 
uppervec.coal.noelec <- apply(predmat.coal.noelec, 2, quantile, 0.975) 




# Figure 2 a
pdf(width=5, height=5, file="output/graphs/fig_2a.pdf")
par(mar = c(3,3,1,2), mgp=c(2,0.5,0))
plot(vec, preddist.coal.elec, type="n", ylim=c(min(c(lowervec.coal.elec, lowervec.coal.noelec)), max(c(uppervec.coal.elec, uppervec.coal.noelec))), xlab="Relationship Score", ylab="Perceived Left-Right Distance")
points(vec, lowervec.coal.elec, type="l", lty=2, lwd=2)
points(vec, uppervec.coal.elec, type="l", lty=2, lwd=2)
points(vec, preddist.coal.elec, type="l", lwd=3)
dev.off()


# Figure 2 b
pdf(width=5, height=5, file="output/graphs/fig_2b.pdf")
par(mar = c(3,3,1,2), mgp=c(2,0.5,0))
plot(vec, preddist.coal.noelec, type="n", ylim=c(min(c(lowervec.coal.elec, lowervec.coal.noelec)), max(c(uppervec.coal.elec, uppervec.coal.noelec))), xlab="Relationship Score", ylab="Perceived Left-Right Distance")
points(vec, lowervec.coal.noelec, type="l", lty=2, lwd=2)
points(vec, uppervec.coal.noelec, type="l", lty=2, lwd=2)
points(vec, preddist.coal.noelec, type="l", lwd=3)
dev.off()





