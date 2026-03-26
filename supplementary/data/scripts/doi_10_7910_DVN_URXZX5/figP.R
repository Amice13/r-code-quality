
library(plyr)
library(Hmisc)


colpercentages <- function (tab,showtotal) 
{
  cperc <- t(round(100 * apply(tab, 1, function(y) y/colSums(tab)), 
                   2))
  if (showtotal) cperc <- rbind(cperc, rep(100, ncol(tab)))
  colnames(cperc) <- colnames(tab)
  if (showtotal) rownames(cperc) <- c(rownames(tab), "Total")
  names(dimnames(cperc)) <- names(dimnames(tab))
  cperc
}

# Set name of the to-be-loaded data file
cached_data_file <- "data/Lift_Data_Clean"

# Load Data Frame "data" 
load(paste0(cached_data_file,".Rda"))

laptopdata <- data[which(data$scenario == "Laptop" & data$experiment == "LiFI"),]
laptopdata <- head(laptopdata,2550)
laptopdata <- laptopdata[order(laptopdata$t) , ]

laptopdata$per150 <- as.numeric(cut2(laptopdata$t, g=3))
laptopdata$per150 <- as.numeric(laptopdata$per150)
laptoptable <- xtabs(~choice+per150, data=laptopdata)
laptoptable <- colpercentages(laptoptable,FALSE)
print(laptoptable)
barplot(laptoptable,args.legend = list(x = "bottomright"),xlab="t (x850)", ylab="Percentage of choices",  main = "Laptop", legend.text = c("C", "D", "T"))


laptopdata$per150 <- as.numeric(cut2(laptopdata$t, g=17))
laptopdata$per150 <-  as.numeric(laptopdata$per150)
laptoptable <- xtabs(~choice+per150, data=laptopdata)
laptoptable <- colpercentages(laptoptable,FALSE)
print(laptoptable)
barplot(laptoptable,args.legend = list(x = "bottomright"),xlab="t (x150)", ylab="Percentage of choices",  main = "Laptop", legend.text = c("C", "D", "T"))
