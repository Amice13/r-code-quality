#############################################

#######Plot Demethy kinetics for ranked motifs####################
##########    a.k.a the chicken plot          ################

##also need to figure out if I want to have total loss or slope

#############################################

#load dplyr package to allow select function
library(dplyr)

setwd("~/Dropbox/DemethylationKinetics_Scripts/Oscar/Datasets/")

data <- read.delim("V65_Dec_non_CGI_meth.csv_DemethVeloc.txt")
data <- data[order(data$slope),]

#load in the methylation  data of interest
#the . is a wildcard representing A, B or C replicate, 
zero.h <- t(select(data, matches("V65_Dec_0h_")))
six.h <- t(select(data, matches("V65_Dec_6h_")))
twelve.h <- t(select(data, matches("V65_Dec_12h_")))
eighteen.h <- t(select(data, matches("V65_Dec_18h_")))
twenty4.h <- t(select(data, matches("V65_Dec_24h_")))
thirty.h <- t(select(data, matches("V65_Dec_30h_")))
thirty6.h <- t(select(data, matches("V65_Dec_36h_")))
forty2.h <- t(select(data, matches("V65_Dec_42h_")))
forty8.h <- t(select(data, matches("V65_Dec_48h_")))

#This is to examine all 256 motifs
#Also becareful to choose error bars or not (see "arrows" below)
for (Motif.col in c(1:256)){  

    command.a <- paste("Motif1 <- zero.h[,",Motif.col,"]")
    eval(parse(text=command.a))
    command.a <- paste("Motif1 <- cbind(Motif1, six.h[,",Motif.col,"])")
    eval(parse(text=command.a))
    command.a <- paste("Motif1 <- cbind(Motif1, twelve.h[,",Motif.col,"])")
    eval(parse(text=command.a))
    command.a <- paste("Motif1 <- cbind(Motif1, eighteen.h[,",Motif.col,"])")
    eval(parse(text=command.a))
    command.a <- paste("Motif1 <- cbind(Motif1, twenty4.h[,",Motif.col,"])")
    eval(parse(text=command.a))
    command.a <- paste("Motif1 <- cbind(Motif1, thirty.h[,",Motif.col,"])")
    eval(parse(text=command.a))
    command.a <- paste("Motif1 <- cbind(Motif1, thirty6.h[,",Motif.col,"])")
    eval(parse(text=command.a))
    command.a <- paste("Motif1 <- cbind(Motif1, forty2.h[,",Motif.col,"])")
    eval(parse(text=command.a))
    command.a <- paste("Motif1 <- cbind(Motif1, forty8.h[,",Motif.col,"])")
    eval(parse(text=command.a))

    #add colum names to Motif table
    colnames(Motif1)<- c("0 h", "6 h", "12 h", "18 h", "24 h" , "30 h", "36 h", "42 h", "48 h")
    h <- c(0,6,12,18,24,30,36,42,48)
    
    
    #Convert the table from factors to characters to numbers
    Motif1 <- as.data.frame(Motif1, stringsAsFactors = FALSE)
    Motif1 <- as.data.frame(sapply(Motif1, as.numeric))    

    #Create difference data
    Loss.pp <- Motif1 - mean(Motif1$`0 h`) 
    Loss.pp
    
    #Plot line graph with error bars
    mean.data = colMeans(Loss.pp, na.rm=T)
    no.of.samples = apply(!is.na(Loss.pp), 2, sum)
    st.dev = sapply(Loss.pp, sd, na.rm=T)
    par(new=TRUE)
    midpoints = plot(x=h, y=mean.data, type="l", ylim = c(-70,0), xlim = c(0,50), las=1, frame.plot=F, ylab = "CG methylation (%)")
    
    #Either comment in or out if you want to plot error bars
    #arrows (x0 = h, y0 = mean.data - st.dev, x1 = h, y1 = mean.data + st.dev, angle = 90, length = 0.02, code = 3)
    
    #Add motif labels if you want them
    #command.b <- paste("text(x=55, y=mean.data[7], labels = row.names(data[",Motif.col,",]), cex=1)")
    #eval(parse(text=command.b))
    
}
