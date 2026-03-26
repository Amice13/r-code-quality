# This R-file is generating the Figure 5

library(sp)
library(maptools)
library(readstata13)
library(RColorBrewer)



setwd("~/Dropbox/Israeli Newspapers Study/JOP_REPLICATION/Dataverse submission")

load("Data/media_markets_yshuv.rdata")
yshuv_lcode <- readShapePoly("Data/Yshuv_lcode/yshuv_lcode.shp")


yeshuv = read.dta13("Data/yeshuvim_areas_final.dta")
yeshuv = yeshuv[order(yeshuv$yeshuv_english),]

yshuv_lcode@data$CITYCODE2 = as.numeric(as.character(yshuv_lcode@data$CITYCODE))
yeshuv$semel_yeshuv = as.numeric(as.character(yeshuv$semel_yeshuv))

names(media_markets_yshuv) = c("Id", "gridcode", "num_area", "IH08")

## Left panel

pdf(file = "Figures/Fig_5_left_panel.pdf",width = 13, height =26)
plot(media_markets_yshuv, col="black", bor="white")
points(yshuv_lcode@data[yshuv_lcode@data$CITYCODE2 %in% yeshuv$semel_yeshuv ,c("X", "Y")], col = rgb(red=0, green=0.7, blue=1, alpha=0.5), pch=19, cex=1.4)
dev.off()


## Right panel
pdf(file = "Figures/Fig_5_right_panel.pdf",width = 4, height =7.5)
spplot(media_markets_yshuv, "IH08", col.regions = brewer.pal(n = 7, name = "OrRd"), 
       cuts =6, col = "transparent")
dev.off()
