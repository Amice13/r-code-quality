#####################
#Time Series Analysis for the Social Sciences
#Box-Steffensmeier, Freeman, Hitt, and Pevehouse
#
#Appendix Replication
#####################

#####################
#Data for Figures A.2-A.5
#####################


remove(list = ls())
library(foreign)

time <- as.ts(c(0,1,2,3,4,5))

diffeqA2 <- function(init,t){ 	#example A.1 
			result <- matrix(rep(NA,length(t)))
			result[1] <- init
				for(i in 2:length(t)){
					result[i] <- result[i-1]*2 + 1
					i <- i + 1
				}
			return(as.ts(result))
			} 

figA2 <- diffeqA2(5,time)			

diffeqA3 <- function(init,t){		#Example A.2
			result <- matrix(rep(NA,length(t)))
			result[1] <- init
				for(i in 2:length(t)){
					result[i] <- result[i-1]*-2 + 1
					i <- i + 1
				}
			return(as.ts(result))
			} 

figA3 <- diffeqA3(0,time)

diffeqA4 <- function(init0, init1, t){		#Example A.3
			result <- matrix(rep(NA,length(t)))
			result[1] <- init0
			result[2] <- init1
				for(i in 3:length(t)){
					result[i] <- result[i-1]*3 - 2*result[i-2] - 1
					i <- i + 1
				}
			return(as.ts(result))
			}  

figA4 <- diffeqA4(-3,5,time)

diffeqA5 <- function(init,t){		#Example A.4
			result <- matrix(rep(NA,length(t)))
			result[1] <- init
				for(i in 2:length(t)){
					result[i] <- result[i-1]*.1 
					i <- i + 1
				}
			return(as.ts(result))
			}

figA5_0 <-   (diffeqA5(0,time))
figA5_1 <-   (diffeqA5(1,time))
figA5_100 <- (diffeqA5(100,time))

sims.1 <- data.frame(cbind(time,figA2,figA3,figA4,figA5_0,figA5_1,figA5_100))
setwd("C:/Users/matthitt/Dropbox/TSASS Book/data")
write.dta(sims.1, file = "appendix_figures_1.dta")
###################################
remove(list=ls())
library(foreign)

set.seed(10221985)

time.2 <- as.ts(seq(0,50,by=1))

draw.1 <- as.ts(cumsum(rnorm(50)))
draw.2 <- as.ts(cumsum(rnorm(50)))

sims.2 <- data.frame(cbind(time.2,draw.1,draw.2))

setwd("C:/Users/matthitt/Dropbox/TSASS Book/data")
write.dta(sims.2, file = "appendix_figures_2.dta")
#################################
time.3 <- seq(0,9,by = 1)

diffeqA7 <- function(t){		#Example A.7
			china <- matrix(rep(NA,length(t)))
			usa <- matrix(rep(NA,length(t)))
				for(i in 1:length(t)){
					china[i] <- 21.287*(1.2162)^t[i] + 1.182*(.5838)^t[i] - 1.1112*t[i] -2.469
					usa[i] <- 67.3168*(1.2162)^t[i] -3.738*(.5838)^t[i] - 1.1112*t[i] - 13.58
					i <- i + 1
				}
			return(as.ts(cbind(china,usa)))
			}

A7 <- diffeqA7(time.3)

sims.3 <- data.frame(cbind(as.ts(time.3),A7))

setwd("C:/Users/matthitt/Dropbox/TSASS Book/data")
write.dta(sims.3, file = "appendix_figures_3.dta")
#############################
remove(list=ls())
library(foreign)

set.seed(8112012)

time.4 <- as.ts(seq(0,50,by=1))

diffeqA8 <- function(init,t){ 	
			result <- matrix(rep(NA,length(t)))
			result[1] <- init
				for(i in 2:length(t)){
					result[i] <- result[i-1] + 1 + rnorm(1)
					i <- i + 1
				}
			return(as.ts(result))
			} 
draw.1.trend <- diffeqA8(0,time.4)
draw.2.trend <- diffeqA8(0,time.4)

sims.4 <- data.frame(cbind(time.4,draw.1.trend,draw.2.trend))

setwd("C:/Users/matthitt/Dropbox/TSASS Book/data")
write.dta(sims.4, file = "appendix_figures_4.dta")
##############################

remove(list=ls())
library(foreign)

time.5 <- as.ts(seq(0,15,by=1))

diffeqA9 <- function(t){ 	#example A.8
			result <- matrix(rep(NA,length(t)))
				for(i in 1:length(t)){
					result[i] <- 2.3125*(.2)^t[i] + 1.25*t[i] - .3125
					i <- i + 1
				}
			return(as.ts(result))
			} 

figA9 <- diffeqA9(time.5)

diffeqA10 <- function(init0, init1, t){		#Example A.9
			result <- matrix(rep(NA,length(t)))
			result[1] <- init0
			result[2] <- init1
				for(i in 3:length(t)){
					result[i] <- .375*.1^t[i] - 1.345*(-.7)^t[i] +1
				i <- i + 1
				}
			return(as.ts(result))
			}  

figA10 <- diffeqA10(0,2,time.5)

diffeqA11 <- function(init0, init1, t){		#Example A.10
			result <- matrix(rep(NA,length(t)))
			result[1] <- init0
			result[2] <- init1
				for(i in 3:length(t)){
					result[i] <- (18.52+ 16.405*t[i])*.4^t[i]+5.55*t[i] - 18.52
				i <- i + 1
				}
			return(as.ts(result))
			} 

figA11 <- diffeqA11(0,1,time.5)

diffeqA12 <- function(init0, init1, t){		#Example A.11, -.5,1
			result <- matrix(rep(NA,length(t)))
			result[1] <- init0
			result[2] <- init1
				for(i in 3:length(t)){
					result[i] <- sin((pi*t[i])/2) + .5*t[i] -.5
				i <- i + 1
				}
			return(as.ts(result))
			} 

figA12 <- diffeqA12(-.5,1,time.5)

sims.5 <- data.frame(cbind(time.5,figA9,figA10,figA11,figA12))

setwd("C:/Users/matthitt/Dropbox/TSASS Book/data")
write.dta(sims.5, file = "appendix_figures_5.dta")
##################################
remove(list=ls())
library(foreign)
set.seed(10221985)

#10, 5 init cond

time.6 <- as.ts(seq(0,20,by=1))

alphas <- c(1,.9,rep(NA,19))
for(i in 3:length(alphas)){
	alphas[i] <- .9*alphas[i-1] - .2* alphas[i-2]
	i <- i + 1
	} #Properly iterates the weights on the shocks

errors1 <- c(0,0,rep(NA,19))
for(i in 3:length(errors1)){
		errors1[i] <- rnorm(1)
		i <- i + 1
		}

errors2 <- c(0,0,rep(NA,19))
for(i in 3:length(errors2)){
		errors2[i] <- rnorm(1)
		i <- i + 1
		}

errors3 <- c(0,0,rep(NA,19))
for(i in 3:length(errors3)){
		errors3[i] <- rnorm(1)
		i <- i + 1
		}

errors4 <- c(0,0,rep(NA,19))
for(i in 3:length(errors4)){
		errors4[i] <- rnorm(1)
		i <- i + 1
		}

errors5 <- c(0,0,rep(NA,19))
for(i in 3:length(errors5)){
		errors5[i] <- rnorm(1)
		i <- i + 1
		}


diffeqA12 <- function(init0,init1,t){		#Example A.13
			result <- matrix(rep(NA,length(t)))
			error.t <- matrix(rep(NA,length(t)))
			result[1] <- init0
			result[2] <- init1
			error.t[1] <- 0
			error.t[2] <- 0	
				for(i in 3:length(t)){
					error.t[i] <- errors5[i]*alphas[1] + errors5[i-1]*alphas[2] + errors5[i-2]*alphas[3] 
						if(i == 4)
							error.t[i] <- error.t[i] + errors5[i-3] * alphas[4]
						if(i == 5)
							error.t[i] <- error.t[i] + errors5[i-3] * alphas[4]  + errors5[i-4] * alphas[5]
						if(i == 6)
							error.t[i] <- error.t[i] + errors5[i-3] * alphas[4]  + errors5[i-4] * alphas[5] + errors5[i-5]*alphas[6]
						if(i == 7)
							error.t[i] <- error.t[i] + errors5[i-3] * alphas[4]  + errors5[i-4] * alphas[5] + errors5[i-5]*alphas[6] + errors5[i-6]*alphas[7]
						if(i > 7)
							error.t[i] + errors5[i-3] * alphas[4]  + errors5[i-4] * alphas[5] + errors5[i-5]*alphas[6] + errors5[i-6]*alphas[7] + errors5[i-7]*alphas[8]
									
					result[i] <- 10 + .4^t[i]*(5*init0 - 10*init1 +50) + .5^t[i]*(10*init1 - 4*init0 -60) + error.t[i]
				i <- i + 1
				}
			return(as.ts(result))
			}



figA13.1 <- diffeqA12(10,5,time.6) #Change errors1 to errors2 etc for each iteration up to errors5
figA13.2 <- diffeqA12(10,5,time.6) 
figA13.3 <- diffeqA12(10,5,time.6)
figA13.4 <- diffeqA12(10,5,time.6)
figA13.5 <- diffeqA12(10,5,time.6)

sims.6 <- data.frame(cbind(time.6,figA13.1,figA13.2,figA13.3,figA13.4,figA13.5))


setwd("C:/Users/matthitt/Dropbox/TSASS Book/data")
write.dta(sims.6, file = "appendix_figures_6.dta")