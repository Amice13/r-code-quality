
setwd("~/Dropbox/io_endogenous_replication/main_text_do/figures/figure_1/")

library(sp);library(rgdal)


grid_1200 <- readOGR(dsn = ".", layer = "grid_1200")

grid_1500 <- readOGR(dsn = ".", layer = "grid_1500")

grid_1800 <- readOGR(dsn = ".", layer = "grid_1800")



cities_1200<- readOGR(dsn = ".", layer = "cit_1200")

cities_1500<- readOGR(dsn = ".", layer = "cit_1500")

cities_1800<- readOGR(dsn = ".", layer = "cit_1800")


plot(grid_1200,main="1200")

points(cities_1200,cex=log(as.numeric(as.character(cities_1200$u)))/2,pch=16,col="dark blue")



plot(grid_1500,main="1500")

points(cities_1500,cex=log(as.numeric(as.character(cities_1500$Urban)))/2,pch=16,col="dark blue")



plot(grid_1800,main="1800")

points(cities_1800,cex=log(as.numeric(as.character(cities_1800$Urban)))/2,pch=16,col="dark blue")







