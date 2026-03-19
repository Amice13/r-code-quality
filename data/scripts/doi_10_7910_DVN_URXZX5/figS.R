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


# Plot a scenario by data
plotLIFT <-function(scenario, data, order=3){
  
  # Select the data for this task only
  data <- data[data$scenario==scenario,]

  # Split the data by experiment 
  data.LI  <- data[data$experiment=="LiFI" , ]
  data.LII <- data[data$experiment=="LiFII" , ]

	# Create the bottom graph, plotting x0 for LiFI and LiFII experiments
	p2 <- ggplot(NULL, aes(x=t, y=x0 ) ) + 
      	#geom_smooth(data = data.LI, aes(x=t, y=x0 ), size=.5) + 
      	#geom_smooth(data = data.LII, aes(x=t, y=x0 ), size=.5, color="red") +
		geom_line(data = data.LI, aes(x=t, y=x0 ), size=.5) + 
	    geom_line(data = data.LII, aes(x=t, y=x0 ), size=.5, color="red") +
      	xlab("Time") + 
	      ylab("Value A2") +
	      theme(axis.title.x = element_text(size=11), axis.text.x = element_text(size=9)) +
	      theme(axis.title.y = element_text(size=11), axis.text.y = element_text(size=9))
	     
	# Store plot:
	save_plot(paste0("figs/S",scenario,".pdf"), p2, base_width=4, base_height=2)
	
	# return
	return(p2)
 
}

# Plot each scenario
scenarios = c("Laptop", "Wineshop", "Beer", "Pizza", "Hotel", "Economist", "Soda can", "Juice")
#scenarios = c("Laptop", "Hotel", "Economist", "Beer", "Juice")
plots <- lapply(scenarios,plotLIFT,data)

