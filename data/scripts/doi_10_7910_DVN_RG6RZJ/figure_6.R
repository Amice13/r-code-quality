
setwd("~/Dropbox/io_endogenous_replication/main_text_do/figures/figure_6/")


Iron<-readOGR(dsn = ".", layer = "Iron")

grid_1500<-readOGR(dsn=".",layer="grid_1500")


plot(grid_1500)

points(Iron,pch=16,cex=1.5,col="dark blue")