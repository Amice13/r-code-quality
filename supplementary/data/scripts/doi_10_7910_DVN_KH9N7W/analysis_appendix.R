
rm(list=ls(all=TRUE))

# path needs to be adjusted. Please set the working directory to one level higher than the subfolder for the code so that subsequent code to load data and save files to "output" folder executes
setwd("~")

# install required packages
#install.packages(c("lfe", "MCMCpack", "MASS", "xtable"))

# load packages
library(lfe)
library(MCMCpack)
library(MASS)
library(xtable)
library(data.table)



# Functions
siglevel <- function(i){
	sig <- ifelse(sign(msum$quantiles[i,3])==sign(msum$quantiles[i,5]), "*", "")
	sig <- ifelse(sign(msum$quantiles[i,2])==sign(msum$quantiles[i,6]), "**", sig)
	sig <- ifelse(sign(msum$quantiles[i,1])==sign(msum$quantiles[i,7]), "***", sig)
	sig
}


# set seed
set.seed(123)



#############
#### SECTION A.2
#############

load("data/data_publishers.rda")

outtable <- xtable(data.frame(sort(table(publ)/length(publ), decreasing=T)[1:10]), digits=3)
print(outtable, file="output/tables_appendix/tab_a1.tex")



#############
#### SECTION A.3
#############

load("data/data_combined.rda")

tab <- data.table(data)
tab$ctylong <- unlist(lapply(strsplit(tab$dyadid, "-"), `[[`, 1))
tab <- tab[,list(count.events=unique(count.events), count.actors=unique(count.actors)), by=c("ctylong", "year", "month")]

outtable <- xtable(tab)
print(outtable, file="output/tables_appendix/tab_a2_a3.tex")



#############
#### SECTION C.1
#############


# elec==1
draw <- r2 <- ar2 <- N <- NULL
for(i in 1:1000){
	formula <- paste0("lr.avg.diff.", i, " ~ qpr.", i, " + lr.cmp.diff.", i, " + coalition + opposition | year.month.country | 0 | dyadid")
	m <- felm(as.formula(formula),  data=data[data$elec==1 & (data$country!="austria" & data$country!="netherlands" & data$country!="denmark" & data$country!="finland" ),], exactDOF=T)
	
	add <- mvrnorm(500, m$beta, m$clustervcv)
	draw <- rbind(draw, add)

	r2 <- c(r2, summary(m)$r.squared)
	ar2 <- c(ar2, summary(m)$adj.r.squared)
	N <- c(N, summary(m)$N)
}

m1b.u <- as.mcmc(draw)
msum <- summary(m1b.u, quantiles=c(0.005, 0.025, 0.05, 0.5, 0.95, 0.975, 0.995))


tab <- NULL
for(i in 1:4){
	add <- matrix(c(rownames(msum$statistics)[i], paste0(format(round(msum$statistics[i,1], 3), nsmall=3, scientific=10), siglevel(i)), "", paste0("(",format(round(msum$statistics[i,2], 3), nsmall=3, scientific=10), ")")), nrow=2, byrow=T)
	tab <- rbind(tab, add)
}

tab <- rbind(tab, c("N", unique(N)))
tab <- rbind(tab, c("R2", round(mean(r2), 2)))
tab <- rbind(tab, c("AR2", round(mean(ar2), 2)))

tab.drop.1 <- tab


# elec==0
draw <- r2 <- ar2 <- N <- NULL
for(i in 1:1000){
	formula <- paste0("lr.avg.diff.", i, " ~ qpr.", i, " + lr.cmp.diff.", i, " + coalition + opposition | year.month.country | 0 | dyadid")
	m <- felm(as.formula(formula),  data=data[data$elec==0 & (data$country!="austria" & data$country!="netherlands" & data$country!="denmark" & data$country!="finland" ),], exactDOF=T)
	
	add <- mvrnorm(500, m$beta, m$clustervcv)
	draw <- rbind(draw, add)

	r2 <- c(r2, summary(m)$r.squared)
	ar2 <- c(ar2, summary(m)$adj.r.squared)
	N <- c(N, summary(m)$N)
}

m1b.u <- as.mcmc(draw)
msum <- summary(m1b.u, quantiles=c(0.005, 0.025, 0.05, 0.5, 0.95, 0.975, 0.995))


tab <- NULL
for(i in 1:4){
	add <- matrix(c(rownames(msum$statistics)[i], paste0(format(round(msum$statistics[i,1], 3), nsmall=3, scientific=10), siglevel(i)), "", paste0("(",format(round(msum$statistics[i,2], 3), nsmall=3, scientific=10), ")")), nrow=2, byrow=T)
	tab <- rbind(tab, add)
}

tab <- rbind(tab, c("N", unique(N)))
tab <- rbind(tab, c("R2", round(mean(r2), 2)))
tab <- rbind(tab, c("AR2", round(mean(ar2), 2)))

tab.drop.2 <- tab



# using Sophia, # elec==1
draw <- r2 <- ar2 <- N <- NULL
for(i in 1:1000){
	formula <- paste0("lr.sophia.diff.", i, " ~ qpr.", i, " + lr.cmp.diff.", i, " + coalition + opposition | year.month.country | 0 | dyadid")
	m <- felm(as.formula(formula),  data=data[data$elec==1 & (data$country!="austria" & data$country!="netherlands" & data$country!="denmark" & data$country!="finland" ),], exactDOF=T)
	
	add <- mvrnorm(500, m$beta, m$clustervcv)
	draw <- rbind(draw, add)

	r2 <- c(r2, summary(m)$r.squared)
	ar2 <- c(ar2, summary(m)$adj.r.squared)
	N <- c(N, summary(m)$N)
}

m1b.u <- as.mcmc(draw)
msum <- summary(m1b.u, quantiles=c(0.005, 0.025, 0.05, 0.5, 0.95, 0.975, 0.995))


tab <- NULL
for(i in 1:4){
	add <- matrix(c(rownames(msum$statistics)[i], paste0(format(round(msum$statistics[i,1], 3), nsmall=3, scientific=10), siglevel(i)), "", paste0("(",format(round(msum$statistics[i,2], 3), nsmall=3, scientific=10), ")")), nrow=2, byrow=T)
	tab <- rbind(tab, add)
}

tab <- rbind(tab, c("N", unique(N)))
tab <- rbind(tab, c("R2", round(mean(r2), 2)))
tab <- rbind(tab, c("AR2", round(mean(ar2), 2)))

tab.drop.3 <- tab

# using Sophia, # elec==0
draw <- r2 <- ar2 <- N <- NULL
for(i in 1:1000){
	formula <- paste0("lr.sophia.diff.", i, " ~ qpr.", i, " + lr.cmp.diff.", i, " + coalition + opposition | year.month.country | 0 | dyadid")
	m <- felm(as.formula(formula),  data=data[data$elec==0 & (data$country!="austria" & data$country!="netherlands" & data$country!="denmark" & data$country!="finland" ),], exactDOF=T)
	
	add <- mvrnorm(500, m$beta, m$clustervcv)
	draw <- rbind(draw, add)

	r2 <- c(r2, summary(m)$r.squared)
	ar2 <- c(ar2, summary(m)$adj.r.squared)
	N <- c(N, summary(m)$N)
}

m1c.u <- as.mcmc(draw)
msum <- summary(m1c.u, quantiles=c(0.005, 0.025, 0.05, 0.5, 0.95, 0.975, 0.995))


tab <- NULL
for(i in 1:4){
	add <- matrix(c(rownames(msum$statistics)[i], paste0(format(round(msum$statistics[i,1], 3), nsmall=3, scientific=10), siglevel(i)), "", paste0("(",format(round(msum$statistics[i,2], 3), nsmall=3, scientific=10), ")")), nrow=2, byrow=T)
	tab <- rbind(tab, add)
}

tab <- rbind(tab, c("N", unique(N)))
tab <- rbind(tab, c("R2", round(mean(r2), 2)))
tab <- rbind(tab, c("AR2", round(mean(ar2), 2)))

tab.drop.4 <- tab


# save table A.4
vars <- c("Relationship Score", "", "Manifesto Difference", "", "Coalition Partners", "", "Opposition Partners", "", "N", "R2", "Adjusted R2")
outtable <- xtable(cbind(vars, tab.drop.1[,2], tab.drop.2[,2], tab.drop.3[,2], tab.drop.4[,2]))
print(outtable, file="output/tables_appendix/tab_a4.tex")





#############
#### SECTION C.2
#############


load("data/data_directinteractions.rda")


# elec=1
draw <- r2 <- ar2 <- N <- NULL
for(i in 1:1000){
	formula <- paste0("lr.avg.diff.", i, " ~ rawscore + lr.cmp.diff.", i, " + coalition + opposition | year.month.country | 0 | dyadid")
	m <- felm(as.formula(formula),  data=data[data$elec==1,], exactDOF=T)
	
	add <- mvrnorm(500, m$beta, m$clustervcv)
	draw <- rbind(draw, add)

	r2 <- c(r2, summary(m)$r.squared)
	ar2 <- c(ar2, summary(m)$adj.r.squared)
	N <- c(N, summary(m)$N)
}

m1b.u <- as.mcmc(draw)
msum <- summary(m1b.u, quantiles=c(0.005, 0.025, 0.05, 0.5, 0.95, 0.975, 0.995))

tab <- NULL
for(i in 1:4){
	add <- matrix(c(rownames(msum$statistics)[i], paste0(format(round(msum$statistics[i,1], 3), nsmall=3, scientific=10), siglevel(i)), "", paste0("(",format(round(msum$statistics[i,2], 3), nsmall=3, scientific=10), ")")), nrow=2, byrow=T)
	tab <- rbind(tab, add)
}

tab <- rbind(tab, c("N", unique(N)))
tab <- rbind(tab, c("R2", round(mean(r2), 2)))
tab <- rbind(tab, c("AR2", round(mean(ar2), 2)))

tab.direct.1 <- tab



# elec=0
draw <- r2 <- ar2 <- N <- NULL
for(i in 1:1000){
	formula <- paste0("lr.avg.diff.", i, " ~ rawscore + lr.cmp.diff.", i, " + coalition + opposition | year.month.country | 0 | dyadid")
	m <- felm(as.formula(formula),  data=data[data$elec==0,], exactDOF=T)
	
	add <- mvrnorm(500, m$beta, m$clustervcv)
	draw <- rbind(draw, add)

	r2 <- c(r2, summary(m)$r.squared)
	ar2 <- c(ar2, summary(m)$adj.r.squared)
	N <- c(N, summary(m)$N)
}

m1c.u <- as.mcmc(draw)
msum <- summary(m1c.u, quantiles=c(0.005, 0.025, 0.05, 0.5, 0.95, 0.975, 0.995))

tab <- NULL
for(i in 1:4){
	add <- matrix(c(rownames(msum$statistics)[i], paste0(format(round(msum$statistics[i,1], 3), nsmall=3, scientific=10), siglevel(i)), "", paste0("(",format(round(msum$statistics[i,2], 3), nsmall=3, scientific=10), ")")), nrow=2, byrow=T)
	tab <- rbind(tab, add)
}

tab <- rbind(tab, c("N", unique(N)))
tab <- rbind(tab, c("R2", round(mean(r2), 2)))
tab <- rbind(tab, c("AR2", round(mean(ar2), 2)))

tab.direct.2 <- tab



# using Sophia, # elec==1
draw <- r2 <- ar2 <- N <- NULL
for(i in 1:1000){
	formula <- paste0("lr.sophia.diff.", i, " ~ rawscore + lr.cmp.diff.", i, " + coalition + opposition | year.month.country | 0 | dyadid")
	m <- felm(as.formula(formula),  data=data[data$elec==1,], exactDOF=T)
	
	add <- mvrnorm(500, m$beta, m$clustervcv)
	draw <- rbind(draw, add)

	r2 <- c(r2, summary(m)$r.squared)
	ar2 <- c(ar2, summary(m)$adj.r.squared)
	N <- c(N, summary(m)$N)
}

m1b.u <- as.mcmc(draw)
msum <- summary(m1b.u, quantiles=c(0.005, 0.025, 0.05, 0.5, 0.95, 0.975, 0.995))

tab <- NULL
for(i in 1:4){
	add <- matrix(c(rownames(msum$statistics)[i], paste0(format(round(msum$statistics[i,1], 3), nsmall=3, scientific=10), siglevel(i)), "", paste0("(",format(round(msum$statistics[i,2], 3), nsmall=3, scientific=10), ")")), nrow=2, byrow=T)
	tab <- rbind(tab, add)
}

tab <- rbind(tab, c("N", unique(N)))
tab <- rbind(tab, c("R2", round(mean(r2), 2)))
tab <- rbind(tab, c("AR2", round(mean(ar2), 2)))

tab.direct.3 <- tab




# using Sophia, # elec==0
draw <- r2 <- ar2 <- N <- NULL
for(i in 1:1000){
	formula <- paste0("lr.sophia.diff.", i, " ~ rawscore + lr.cmp.diff.", i, " + coalition + opposition | year.month.country | 0 | dyadid")
	m <- felm(as.formula(formula),  data=data[data$elec==0,], exactDOF=T)
	
	add <- mvrnorm(500, m$beta, m$clustervcv)
	draw <- rbind(draw, add)

	r2 <- c(r2, summary(m)$r.squared)
	ar2 <- c(ar2, summary(m)$adj.r.squared)
	N <- c(N, summary(m)$N)
}

m1c.u <- as.mcmc(draw)
msum <- summary(m1c.u, quantiles=c(0.005, 0.025, 0.05, 0.5, 0.95, 0.975, 0.995))


tab <- NULL
for(i in 1:4){
	add <- matrix(c(rownames(msum$statistics)[i], paste0(format(round(msum$statistics[i,1], 3), nsmall=3, scientific=10), siglevel(i)), "", paste0("(",format(round(msum$statistics[i,2], 3), nsmall=3, scientific=10), ")")), nrow=2, byrow=T)
	tab <- rbind(tab, add)
}

tab <- rbind(tab, c("N", unique(N)))
tab <- rbind(tab, c("R2", round(mean(r2), 2)))
tab <- rbind(tab, c("AR2", round(mean(ar2), 2)))

tab.direct.4 <- tab


# save table A.5
vars <- c("Relationship Score (Direct)", "", "Manifesto Difference", "", "Coalition Partners", "", "Opposition Partners", "", "N", "R2", "Adjusted R2")
outtable <- xtable(cbind(vars, tab.direct.1[,2], tab.direct.2[,2], tab.direct.3[,2], tab.direct.4[,2]))
print(outtable, file="output/tables_appendix/tab_a5.tex")






#############
#### SECTION C.3
#############

load("data/data_9months.rda")


### 9 months

# elec=1
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

mod <- as.mcmc(draw)
mod.3.1 <- msum <- summary(mod, quantiles=c(0.005, 0.025, 0.05, 0.5, 0.95, 0.975, 0.995))
info.3.1 <- c(unique(N), mean(r2), mean(ar2))


tab <- NULL
for(i in 1:4){
	add <- matrix(c(rownames(msum$statistics)[i], paste0(format(round(msum$statistics[i,1], 3), nsmall=3, scientific=10), siglevel(i)), "", paste0("(",format(round(msum$statistics[i,2], 3), nsmall=3, scientific=10), ")")), nrow=2, byrow=T)
	tab <- rbind(tab, add)
}

tab <- rbind(tab, c("N", unique(N)))
tab <- rbind(tab, c("R2", round(mean(r2), 2)))
tab <- rbind(tab, c("AR2", round(mean(ar2), 2)))

tab.3.1 <- tab


# elec=0
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

mod <- as.mcmc(draw)
mod.3.2 <- msum <- summary(mod, quantiles=c(0.005, 0.025, 0.05, 0.5, 0.95, 0.975, 0.995))
info.3.2 <- c(unique(N), mean(r2), mean(ar2))


tab <- NULL
for(i in 1:4){
	add <- matrix(c(rownames(msum$statistics)[i], paste0(format(round(msum$statistics[i,1], 3), nsmall=3, scientific=10), siglevel(i)), "", paste0("(",format(round(msum$statistics[i,2], 3), nsmall=3, scientific=10), ")")), nrow=2, byrow=T)
	tab <- rbind(tab, add)
}

tab <- rbind(tab, c("N", unique(N)))
tab <- rbind(tab, c("R2", round(mean(r2), 2)))
tab <- rbind(tab, c("AR2", round(mean(ar2), 2)))

tab.3.2 <- tab


# using Sophia, # elec==1
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

mod <- as.mcmc(draw)
mod.3.3 <- msum <- summary(mod, quantiles=c(0.005, 0.025, 0.05, 0.5, 0.95, 0.975, 0.995))
info.3.3 <- c(unique(N), mean(r2), mean(ar2))


tab <- NULL
for(i in 1:4){
	add <- matrix(c(rownames(msum$statistics)[i], paste0(format(round(msum$statistics[i,1], 3), nsmall=3, scientific=10), siglevel(i)), "", paste0("(",format(round(msum$statistics[i,2], 3), nsmall=3, scientific=10), ")")), nrow=2, byrow=T)
	tab <- rbind(tab, add)
}

tab <- rbind(tab, c("N", unique(N)))
tab <- rbind(tab, c("R2", round(mean(r2), 2)))
tab <- rbind(tab, c("AR2", round(mean(ar2), 2)))

tab.3.3 <- tab


# using Sophia, # elec==0
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

mod <- as.mcmc(draw)
mod.3.4 <- msum <- summary(mod, quantiles=c(0.005, 0.025, 0.05, 0.5, 0.95, 0.975, 0.995))
info.3.4 <- c(unique(N), mean(r2), mean(ar2))

tab <- NULL
for(i in 1:4){
	add <- matrix(c(rownames(msum$statistics)[i], paste0(format(round(msum$statistics[i,1], 3), nsmall=3, scientific=10), siglevel(i)), "", paste0("(",format(round(msum$statistics[i,2], 3), nsmall=3, scientific=10), ")")), nrow=2, byrow=T)
	tab <- rbind(tab, add)
}

tab <- rbind(tab, c("N", unique(N)))
tab <- rbind(tab, c("R2", round(mean(r2), 2)))
tab <- rbind(tab, c("AR2", round(mean(ar2), 2)))

tab.3.4 <- tab


# save table A.6
vars <- c("Relationship Score", "", "Manifesto Difference", "", "Coalition Partners", "", "Opposition Partners", "", "N", "R2", "Adjusted R2")
outtable <- xtable(cbind(vars, tab.3.1[,2], tab.3.2[,2], tab.3.3[,2], tab.3.4[,2]))
print(outtable, file="output/tables_appendix/tab_a6.tex")



### 6 months

load("data/data_6months.rda")

# elec=1
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

mod <- as.mcmc(draw)
mod.3.1 <- msum <- summary(mod, quantiles=c(0.005, 0.025, 0.05, 0.5, 0.95, 0.975, 0.995))
info.3.1 <- c(unique(N), mean(r2), mean(ar2))


tab <- NULL
for(i in 1:4){
	add <- matrix(c(rownames(msum$statistics)[i], paste0(format(round(msum$statistics[i,1], 3), nsmall=3, scientific=10), siglevel(i)), "", paste0("(",format(round(msum$statistics[i,2], 3), nsmall=3, scientific=10), ")")), nrow=2, byrow=T)
	tab <- rbind(tab, add)
}

tab <- rbind(tab, c("N", unique(N)))
tab <- rbind(tab, c("R2", round(mean(r2), 2)))
tab <- rbind(tab, c("AR2", round(mean(ar2), 2)))

tab.3.1 <- tab


# elec=0
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

mod <- as.mcmc(draw)
mod.3.2 <- msum <- summary(mod, quantiles=c(0.005, 0.025, 0.05, 0.5, 0.95, 0.975, 0.995))
info.3.2 <- c(unique(N), mean(r2), mean(ar2))


tab <- NULL
for(i in 1:4){
	add <- matrix(c(rownames(msum$statistics)[i], paste0(format(round(msum$statistics[i,1], 3), nsmall=3, scientific=10), siglevel(i)), "", paste0("(",format(round(msum$statistics[i,2], 3), nsmall=3, scientific=10), ")")), nrow=2, byrow=T)
	tab <- rbind(tab, add)
}

tab <- rbind(tab, c("N", unique(N)))
tab <- rbind(tab, c("R2", round(mean(r2), 2)))
tab <- rbind(tab, c("AR2", round(mean(ar2), 2)))

tab.3.2 <- tab


# using Sophia, # elec==1
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

mod <- as.mcmc(draw)
mod.3.3 <- msum <- summary(mod, quantiles=c(0.005, 0.025, 0.05, 0.5, 0.95, 0.975, 0.995))
info.3.3 <- c(unique(N), mean(r2), mean(ar2))


tab <- NULL
for(i in 1:4){
	add <- matrix(c(rownames(msum$statistics)[i], paste0(format(round(msum$statistics[i,1], 3), nsmall=3, scientific=10), siglevel(i)), "", paste0("(",format(round(msum$statistics[i,2], 3), nsmall=3, scientific=10), ")")), nrow=2, byrow=T)
	tab <- rbind(tab, add)
}

tab <- rbind(tab, c("N", unique(N)))
tab <- rbind(tab, c("R2", round(mean(r2), 2)))
tab <- rbind(tab, c("AR2", round(mean(ar2), 2)))

tab.3.3 <- tab


# using Sophia, # elec==0
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

mod <- as.mcmc(draw)
mod.3.4 <- msum <- summary(mod, quantiles=c(0.005, 0.025, 0.05, 0.5, 0.95, 0.975, 0.995))
info.3.4 <- c(unique(N), mean(r2), mean(ar2))

tab <- NULL
for(i in 1:4){
	add <- matrix(c(rownames(msum$statistics)[i], paste0(format(round(msum$statistics[i,1], 3), nsmall=3, scientific=10), siglevel(i)), "", paste0("(",format(round(msum$statistics[i,2], 3), nsmall=3, scientific=10), ")")), nrow=2, byrow=T)
	tab <- rbind(tab, add)
}

tab <- rbind(tab, c("N", unique(N)))
tab <- rbind(tab, c("R2", round(mean(r2), 2)))
tab <- rbind(tab, c("AR2", round(mean(ar2), 2)))

tab.3.4 <- tab


# save table A.7
vars <- c("Relationship Score", "", "Manifesto Difference", "", "Coalition Partners", "", "Opposition Partners", "", "N", "R2", "Adjusted R2")
outtable <- xtable(cbind(vars, tab.3.1[,2], tab.3.2[,2], tab.3.3[,2], tab.3.4[,2]))
print(outtable, file="output/tables_appendix/tab_a7.tex")





#############
#### SECTION C.4
#############

load("data/data_combined.rda")



## JUST COUNTRY FEs

# elec==1
draw <- r2 <- ar2 <- N <- NULL
for(i in 1:1000){
	formula <- paste0("lr.avg.diff.", i, " ~ qpr.", i, " + lr.cmp.diff.", i, " + coalition + opposition | country | 0 | dyadid")
	m <- felm(as.formula(formula),  data=data[data$elec==1,], exactDOF=T)
	
	add <- mvrnorm(500, m$beta, m$clustervcv)
	draw <- rbind(draw, add)

	r2 <- c(r2, summary(m)$r.squared)
	ar2 <- c(ar2, summary(m)$adj.r.squared)
	N <- c(N, summary(m)$N)
}

m1b.u <- as.mcmc(draw)
msum <- summary(m1b.u, quantiles=c(0.005, 0.025, 0.05, 0.5, 0.95, 0.975, 0.995))


tab <- NULL
for(i in 1:4){
	add <- matrix(c(rownames(msum$statistics)[i], paste0(format(round(msum$statistics[i,1], 3), nsmall=3, scientific=10), siglevel(i)), "", paste0("(",format(round(msum$statistics[i,2], 3), nsmall=3, scientific=10), ")")), nrow=2, byrow=T)
	tab <- rbind(tab, add)
}

tab <- rbind(tab, c("N", unique(N)))
tab <- rbind(tab, c("R2", round(mean(r2), 2)))
tab <- rbind(tab, c("AR2", round(mean(ar2), 2)))

tab.drop.1 <- tab


# elec==0
draw <- r2 <- ar2 <- N <- NULL
for(i in 1:1000){
	formula <- paste0("lr.avg.diff.", i, " ~ qpr.", i, " + lr.cmp.diff.", i, " + coalition + opposition | country | 0 | dyadid")
	m <- felm(as.formula(formula),  data=data[data$elec==0,], exactDOF=T)
	
	add <- mvrnorm(500, m$beta, m$clustervcv)
	draw <- rbind(draw, add)

	r2 <- c(r2, summary(m)$r.squared)
	ar2 <- c(ar2, summary(m)$adj.r.squared)
	N <- c(N, summary(m)$N)
}

m1b.u <- as.mcmc(draw)
msum <- summary(m1b.u, quantiles=c(0.005, 0.025, 0.05, 0.5, 0.95, 0.975, 0.995))


tab <- NULL
for(i in 1:4){
	add <- matrix(c(rownames(msum$statistics)[i], paste0(format(round(msum$statistics[i,1], 3), nsmall=3, scientific=10), siglevel(i)), "", paste0("(",format(round(msum$statistics[i,2], 3), nsmall=3, scientific=10), ")")), nrow=2, byrow=T)
	tab <- rbind(tab, add)
}

tab <- rbind(tab, c("N", unique(N)))
tab <- rbind(tab, c("R2", round(mean(r2), 2)))
tab <- rbind(tab, c("AR2", round(mean(ar2), 2)))

tab.drop.2 <- tab



# using Sophia, # elec==1
draw <- r2 <- ar2 <- N <- NULL
for(i in 1:1000){
	formula <- paste0("lr.sophia.diff.", i, " ~ qpr.", i, " + lr.cmp.diff.", i, " + coalition + opposition | country | 0 | dyadid")
	m <- felm(as.formula(formula),  data=data[data$elec==1,], exactDOF=T)
	
	add <- mvrnorm(500, m$beta, m$clustervcv)
	draw <- rbind(draw, add)

	r2 <- c(r2, summary(m)$r.squared)
	ar2 <- c(ar2, summary(m)$adj.r.squared)
	N <- c(N, summary(m)$N)
}

m1b.u <- as.mcmc(draw)
msum <- summary(m1b.u, quantiles=c(0.005, 0.025, 0.05, 0.5, 0.95, 0.975, 0.995))


tab <- NULL
for(i in 1:4){
	add <- matrix(c(rownames(msum$statistics)[i], paste0(format(round(msum$statistics[i,1], 3), nsmall=3, scientific=10), siglevel(i)), "", paste0("(",format(round(msum$statistics[i,2], 3), nsmall=3, scientific=10), ")")), nrow=2, byrow=T)
	tab <- rbind(tab, add)
}

tab <- rbind(tab, c("N", unique(N)))
tab <- rbind(tab, c("R2", round(mean(r2), 2)))
tab <- rbind(tab, c("AR2", round(mean(ar2), 2)))

tab.drop.3 <- tab

# using Sophia, # elec==0
draw <- r2 <- ar2 <- N <- NULL
for(i in 1:1000){
	formula <- paste0("lr.sophia.diff.", i, " ~ qpr.", i, " + lr.cmp.diff.", i, " + coalition + opposition | country | 0 | dyadid")
	m <- felm(as.formula(formula),  data=data[data$elec==0,], exactDOF=T)
	
	add <- mvrnorm(500, m$beta, m$clustervcv)
	draw <- rbind(draw, add)

	r2 <- c(r2, summary(m)$r.squared)
	ar2 <- c(ar2, summary(m)$adj.r.squared)
	N <- c(N, summary(m)$N)
}

m1c.u <- as.mcmc(draw)
msum <- summary(m1c.u, quantiles=c(0.005, 0.025, 0.05, 0.5, 0.95, 0.975, 0.995))


tab <- NULL
for(i in 1:4){
	add <- matrix(c(rownames(msum$statistics)[i], paste0(format(round(msum$statistics[i,1], 3), nsmall=3, scientific=10), siglevel(i)), "", paste0("(",format(round(msum$statistics[i,2], 3), nsmall=3, scientific=10), ")")), nrow=2, byrow=T)
	tab <- rbind(tab, add)
}

tab <- rbind(tab, c("N", unique(N)))
tab <- rbind(tab, c("R2", round(mean(r2), 2)))
tab <- rbind(tab, c("AR2", round(mean(ar2), 2)))

tab.drop.4 <- tab


# save table A.8
vars <- c("Relationship Score", "", "Manifesto Difference", "", "Coalition Partners", "", "Opposition Partners", "", "N", "R2", "Adjusted R2")
outtable <- xtable(cbind(vars, tab.drop.1[,2], tab.drop.2[,2], tab.drop.3[,2], tab.drop.4[,2]))
print(outtable, file="output/tables_appendix/tab_a8.tex")






## JUST COUNTRY and YEAR FEs

# elec==1
draw <- r2 <- ar2 <- N <- NULL
for(i in 1:1000){
	formula <- paste0("lr.avg.diff.", i, " ~ qpr.", i, " + lr.cmp.diff.", i, " + coalition + opposition | country + year | 0 | dyadid")
	m <- felm(as.formula(formula),  data=data[data$elec==1,], exactDOF=T)
	
	add <- mvrnorm(500, m$beta, m$clustervcv)
	draw <- rbind(draw, add)

	r2 <- c(r2, summary(m)$r.squared)
	ar2 <- c(ar2, summary(m)$adj.r.squared)
	N <- c(N, summary(m)$N)
}

m1b.u <- as.mcmc(draw)
msum <- summary(m1b.u, quantiles=c(0.005, 0.025, 0.05, 0.5, 0.95, 0.975, 0.995))


tab <- NULL
for(i in 1:4){
	add <- matrix(c(rownames(msum$statistics)[i], paste0(format(round(msum$statistics[i,1], 3), nsmall=3, scientific=10), siglevel(i)), "", paste0("(",format(round(msum$statistics[i,2], 3), nsmall=3, scientific=10), ")")), nrow=2, byrow=T)
	tab <- rbind(tab, add)
}

tab <- rbind(tab, c("N", unique(N)))
tab <- rbind(tab, c("R2", round(mean(r2), 2)))
tab <- rbind(tab, c("AR2", round(mean(ar2), 2)))

tab.drop.1 <- tab


# elec==0
draw <- r2 <- ar2 <- N <- NULL
for(i in 1:1000){
	formula <- paste0("lr.avg.diff.", i, " ~ qpr.", i, " + lr.cmp.diff.", i, " + coalition + opposition | country + year | 0 | dyadid")
	m <- felm(as.formula(formula),  data=data[data$elec==0,], exactDOF=T)
	
	add <- mvrnorm(500, m$beta, m$clustervcv)
	draw <- rbind(draw, add)

	r2 <- c(r2, summary(m)$r.squared)
	ar2 <- c(ar2, summary(m)$adj.r.squared)
	N <- c(N, summary(m)$N)
}

m1b.u <- as.mcmc(draw)
msum <- summary(m1b.u, quantiles=c(0.005, 0.025, 0.05, 0.5, 0.95, 0.975, 0.995))


tab <- NULL
for(i in 1:4){
	add <- matrix(c(rownames(msum$statistics)[i], paste0(format(round(msum$statistics[i,1], 3), nsmall=3, scientific=10), siglevel(i)), "", paste0("(",format(round(msum$statistics[i,2], 3), nsmall=3, scientific=10), ")")), nrow=2, byrow=T)
	tab <- rbind(tab, add)
}

tab <- rbind(tab, c("N", unique(N)))
tab <- rbind(tab, c("R2", round(mean(r2), 2)))
tab <- rbind(tab, c("AR2", round(mean(ar2), 2)))

tab.drop.2 <- tab



# using Sophia, # elec==1
draw <- r2 <- ar2 <- N <- NULL
for(i in 1:1000){
	formula <- paste0("lr.sophia.diff.", i, " ~ qpr.", i, " + lr.cmp.diff.", i, " + coalition + opposition | country + year | 0 | dyadid")
	m <- felm(as.formula(formula),  data=data[data$elec==1,], exactDOF=T)
	
	add <- mvrnorm(500, m$beta, m$clustervcv)
	draw <- rbind(draw, add)

	r2 <- c(r2, summary(m)$r.squared)
	ar2 <- c(ar2, summary(m)$adj.r.squared)
	N <- c(N, summary(m)$N)
}

m1b.u <- as.mcmc(draw)
msum <- summary(m1b.u, quantiles=c(0.005, 0.025, 0.05, 0.5, 0.95, 0.975, 0.995))


tab <- NULL
for(i in 1:4){
	add <- matrix(c(rownames(msum$statistics)[i], paste0(format(round(msum$statistics[i,1], 3), nsmall=3, scientific=10), siglevel(i)), "", paste0("(",format(round(msum$statistics[i,2], 3), nsmall=3, scientific=10), ")")), nrow=2, byrow=T)
	tab <- rbind(tab, add)
}

tab <- rbind(tab, c("N", unique(N)))
tab <- rbind(tab, c("R2", round(mean(r2), 2)))
tab <- rbind(tab, c("AR2", round(mean(ar2), 2)))

tab.drop.3 <- tab

# using Sophia, # elec==0
draw <- r2 <- ar2 <- N <- NULL
for(i in 1:1000){
	formula <- paste0("lr.sophia.diff.", i, " ~ qpr.", i, " + lr.cmp.diff.", i, " + coalition + opposition | country + year | 0 | dyadid")
	m <- felm(as.formula(formula),  data=data[data$elec==0,], exactDOF=T)
	
	add <- mvrnorm(500, m$beta, m$clustervcv)
	draw <- rbind(draw, add)

	r2 <- c(r2, summary(m)$r.squared)
	ar2 <- c(ar2, summary(m)$adj.r.squared)
	N <- c(N, summary(m)$N)
}

m1c.u <- as.mcmc(draw)
msum <- summary(m1c.u, quantiles=c(0.005, 0.025, 0.05, 0.5, 0.95, 0.975, 0.995))


tab <- NULL
for(i in 1:4){
	add <- matrix(c(rownames(msum$statistics)[i], paste0(format(round(msum$statistics[i,1], 3), nsmall=3, scientific=10), siglevel(i)), "", paste0("(",format(round(msum$statistics[i,2], 3), nsmall=3, scientific=10), ")")), nrow=2, byrow=T)
	tab <- rbind(tab, add)
}

tab <- rbind(tab, c("N", unique(N)))
tab <- rbind(tab, c("R2", round(mean(r2), 2)))
tab <- rbind(tab, c("AR2", round(mean(ar2), 2)))

tab.drop.4 <- tab


# save table A.9
vars <- c("Relationship Score", "", "Manifesto Difference", "", "Coalition Partners", "", "Opposition Partners", "", "N", "R2", "Adjusted R2")
outtable <- xtable(cbind(vars, tab.drop.1[,2], tab.drop.2[,2], tab.drop.3[,2], tab.drop.4[,2]))
print(outtable, file="output/tables_appendix/tab_a9.tex")





