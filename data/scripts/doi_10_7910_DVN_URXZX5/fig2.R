# Code for producing the second figure in the paper.


library(gridExtra)
library(ggplot2)
library(arm)
#library(binom)
require(reshape2)
library(gridExtra)
library(ggplot2)
library(MCMCpack)
library(cowplot)

theme_set(theme_bw())


# Set name of the to-be-loaded data file
cached_data_file <- "data/Lift_Data_Clean"

# Load Data Frame "data" 
load(paste0(cached_data_file,".Rda"))


# Bunch of helper functions to compute meand and confidence etc.
# Computes the posterior beta for the control condition:
summary.control <- function(dc, prior=c(1,1), perc=95){
	
	# Compute posterior Beta
	alpha <- nrow(dc[dc$choice=="T", ] ) + prior[1]
	beta <-nrow(dc) + prior[2] - nrow(dc[dc$choice=="T", ] ) 
	
	# Results
	result <- list()
	
	# Add point estimate (posterior mean)
	result["point"] <- alpha / (alpha+beta)
	
	# Add upper and lower bound
	p.low <- (1-(perc/100))/2
	p.up <- p.low + perc/100
	result["low"] <- qbeta(p.low,alpha,beta)
	result["up"] <- qbeta(p.up,alpha,beta)
	return(result)
}


summary.random <- function(dr, perc=95, pred = seq(min(dr$x),max(dr$x),length=100), draws=10000, order=8, auto.select=FALSE){
	
	dr <- dr[dr$choice!="D",]
	
	# PROBIT
	mod <- MCMCprobit(dr$y ~ poly(dr$x, order, raw=TRUE), mcmc=draws)
	result <- data.frame(x=pred, point=NA, low=NA, up=NA)
	
	# Attempt for BF for model selection, but unclear...
	if(auto.select){
		# Model comparisons using Bayes factros
		mod0 <- MCMCprobit(dr$y ~ poly(dr$x, 1, raw=TRUE), b0=rep(0,2), B0=.0001, mcmc=draws, marginal.likelihood="Chib95")
		mod1 <- MCMCprobit(dr$y ~ poly(dr$x, 2, raw=TRUE), b0=rep(0,3), B0=.0001, mcmc=draws, marginal.likelihood="Chib95")
		mod2 <- MCMCprobit(dr$y ~ poly(dr$x, 3, raw=TRUE), b0=rep(0,4), B0=.0001, mcmc=draws, marginal.likelihood="Chib95")
	
		# BF
		BF <- BayesFactor(mod0,mod1,mod2)
		probs <- PostProbMod(BF, c(.01,.9,.09))			# With prior belief
		select <- order <- as.numeric(which.max(probs))
		mod <- list(mod0,mod1,mod2)[[select]]
	}
	
	for(x in pred){
		x.vec <- c(1, x, x^2, x^3, x^4, x^5, x^6, x^7, x^8, x^9, x^10)
		x.vec <- x.vec[1:(order+1)] 		
		p.predicted <- invlogit(mod %*% x.vec)		# LOGIT VERSION
		result[result$x==x,]$point <- mean(p.predicted)
		result[result$x==x,]$low <- quantile(p.predicted, (1-(perc/100))/2)
		result[result$x==x,]$up <- quantile(p.predicted, (1-(perc/100))/2 + perc/100)
	}
	
	return(result)
	
}

# Plot a scenario by data
plotForTask <-function(scenario, data, order=3){
  
  # Select the data for this task only
  data <- data[data$scenario==scenario,]

  # Split the data by experiment 
  data.LI  <- data[data$experiment=="LiFI" , ]
  data.LII <- data[data$experiment=="LiFII" , ]
  data.R   <- data[data$experiment=="Random", ]
  data.C   <- data[data$experiment=="Control", ]
  
	# Focus first on the control condition and compute mean and CI
	p.control <- summary.control(data.C)

	# Next generate data frame of predictions for the random condition:
	# Returns a data frame with point, upper, and lower for multiple values of x
	p.random <- summary.random(data.R, order=order)
	
  
	# Create the upper graph
	p1 <- ggplot(aes(x, y), data=data.R) +
      	geom_hline(yintercept=p.control$point,color="blue") + 
      	geom_ribbon(aes(ymin=p.control$low, ymax=p.control$up), alpha=0.3) +
      	geom_line(data=p.random, aes(y=point, x=x), colour="red" ) + 
      	geom_ribbon(data=p.random, aes(ymin=low, ymax=up, x=x), alpha=0.3, inherit.aes = FALSE) +
        geom_vline(aes(xintercept=mean(data.LI[data.LI$t<150,]$x)),color="blue",linetype="dashed") +
      	geom_vline(aes(xintercept=mean(c(data.LI[data.LI$t>2400,]$x, data.LII[data.LII$t>2400,]$x))),color="blue") + 
      	ylim(0, max(p.random$up)+.05) +
      	ylab("Probablity of selecting target") +
	      xlim(p.random$x[1], tail(p.random$x, n=1)) +
	      theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank()) +
	      theme(axis.title.y = element_text(size=11), axis.text.y = element_text(size=9))
	
	

	# Create the bottom graph, plotting x0 for LiFI and LiFII experiments
	p2 <- ggplot(NULL, aes(x=t, y=x0 ) ) + 
      	geom_smooth(data = data.LI, aes(x=t, y=x0 ), size=.5) + 
      	geom_smooth(data = data.LII, aes(x=t, y=x0 ), size=.5, color="red") +
		#geom_line(data = data.LI, aes(x=t, y=x0 ), size=.5) + 
	    #geom_line(data = data.LII, aes(x=t, y=x0 ), size=.5, color="red") +
      	coord_flip() +
      	xlab("Time") + 
	      ylab("Value attribute two") +
	      theme(axis.title.x = element_text(size=11), axis.text.x = element_text(size=9)) +
	      theme(axis.title.y = element_text(size=11), axis.text.y = element_text(size=9))
	      

	ggp1 <- ggplot_build(p1)
	ggp2 <- ggplot_build(p2)
	r_left <- min(ggp1$panel$ranges[[1]]$x.range[1],ggp2$panel$ranges[[1]]$x.range[1])
	r_right <- max(ggp1$panel$ranges[[1]]$x.range[2],ggp2$panel$ranges[[1]]$x.range[2])
	p1 <- p1 + scale_x_continuous(expand = c(0, 0), limits = c(r_left, r_right)) 
	p2 <- p2 + scale_y_continuous(expand = c(0, 0), limits = c(r_left, r_right)) 
	
  # Position both plots together
	p3 <- plot_grid(p1, p2, ncol=1, nrow = 2,align="v",rel_heights =c(3,1))
	
	# Store plot:
	#save_plot(paste0("figs/Figure2_",scenario,"_",order,".pdf"), p3)
	save_plot(paste0("figs/Figure2_logit_",scenario,"_auto",".pdf"), p3)
	
	
	# return
	return(p3)
 
}

# Plot each scenario
scenarios = c("Laptop", "Beer", "Hotel", "Economist", "Juice","Pizza", "Soda can", "Wineshop")
#scenarios = c("Laptop", "Hotel", "Economist", "Beer", "Juice")
plots <- lapply(scenarios,plotForTask,data,3)

p1 <- plotForTask("Laptop", data)
p2 <- plotForTask("Hotel", data)
p3 <- plotForTask("Economist", data)
p4 <- plotForTask("Beer", data)
p5 <- plotForTask("Juice", data)
pp1 <- plot_grid(p2,p3,p4,p5, ncol=2, nrow = 2,align="v")	
meta <- plot_grid(p1, pp1, ncol=1, nrow=2)
save_plot("Figure2.pdf", meta)
save_plot("Figure2.svg", meta)

pa1 <- plotForTask("Soda can", data)
pa2 <- plotForTask("Pizza", data)
pa3 <- plotForTask("Wineshop", data)


