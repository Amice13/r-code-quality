setwd("~/Dropbox/io_endogenous_replication/main_text_do/figures/figure_7")



Textiles<-readOGR(dsn = ".", layer = "Textiles")

grid_1500<-readOGR(dsn=".",layer="grid_1500")


plot(grid_1500)

points(Textiles,pch=17,cex=1.5,col="dark blue")