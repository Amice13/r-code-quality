#uncomment to install packages

#install.packages("foreign")
#install.packages("Hmisc")
library(foreign)
library(Hmisc)

#uncomment this to set your working directory, replacing "~/mypath/" with your working directory
#mywd <- "~/mypath/"
#setwd(mywd)

data <- read.dta("Barber_Canes_Wrone_Thrower_Replication_AJPS.dta")

saveRDS(data[,c("donor_id", "peragsen", "per2agchal", "total_donation", "investor2", "matchcommf")], file="Schnakenberg_Turner_Replication_Data.rds")

