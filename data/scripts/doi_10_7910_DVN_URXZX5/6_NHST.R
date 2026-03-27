# Set name of the to-be-loaded data file
cached_data_file <- "data/Lift_Data_Clean"

# Load Data Frame "data" 
load(paste0(cached_data_file,".Rda"))

head(data)
summary(data)

data <- data[data$choice!="D",]
summary(data)

# Ok, sorry, ugly and fast:

tableRow <- function(scenario){
	dscen <- data[data$scenario==scenario,]
	ncontrol <- nrow(dscen[dscen$experiment=="Control",])
	ycontrol <- sum(dscen[dscen$experiment=="Control",]$y)

	ntreat <- 400
	ytreat <- sum(tail(dscen[dscen$experiment=="LiFI",],200)$y) +
			sum(tail(dscen[dscen$experiment=="LiFII",],200)$y)

			M <- as.table(rbind(c(ncontrol-ycontrol, ycontrol), c(ntreat-ytreat, ytreat)))
			dimnames(M) <- list(condition = c("Control", "Treatment"), count = c("competitor", "target"))
			M <- t(M)

			Test <- chisq.test(M)
			p <- format.pval(Test$p.value, digits=2, eps=0.001)

			print(
				paste(
					scenario,
					ycontrol,
					ncontrol-ycontrol,
					ytreat,
					ntreat-ytreat,
					p, sep="&"
					))
	}

tableRow("Laptop")
tableRow("Hotel")
tableRow("Economist")
tableRow("Beer")
tableRow("Juice")