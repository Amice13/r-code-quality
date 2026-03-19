library(ggplot2)
library(data.table)
library(GGally)

# This R File contains code neded to replicate Figures 2 & 3 of the PB manuscript that validates the estimated ideal points with standing measures of elite and mass public ideological preferences.

setwd("/Final Data Replication Files")

load("Figure2_Figure3_ideal_point_validation.Rdata")

##### Figure 2 Perceptual & Roll-Call Based Ideal Points by Self-Placements #####  

x <- cces
x <- na.omit(x)

my_dens <- function(data, mapping, ...) {
  ggplot(data = data, mapping=mapping) +
    geom_histogram(..., alpha = 0.5, color = "black") + scale_fill_manual("",values=c("blue","purple","red")) 
}

y <- x
colnames(y) <- c("pid3","Liberal-Conservative Self Placement","Aldrich-McKelvey Ideal Points","Roll-Call Scaling Ideal Points")
custom_car <- ggpairs(y,columns = c("Liberal-Conservative Self Placement","Aldrich-McKelvey Ideal Points","Roll-Call Scaling Ideal Points"),ggplot2::aes(fill = y$pid3),diag = list(continuous = my_dens,lower = "blank"),upper = list(continuous = wrap("cor", size = 8))) + theme_minimal() + theme(legend.position = "bottom")

plot1 <- ggplot(x, ggplot2::aes(x=selfplace,fill=pid3)) + scale_fill_manual("",values=c("blue","purple","red")) + theme_minimal() + geom_histogram(alpha = 0.5, color = "black") + theme(legend.position="none") + scale_x_continuous("",breaks=seq(1,7,1))
custom_car[1, 1] <- plot1
plot1 <- ggplot(x, ggplot2::aes(x=selfplace,y=idealpts_linear_map_overtime,group=selfplace)) + geom_boxplot(colour = "black",outlier.shape = NA)+ scale_y_continuous("",limits=c(-2.5,2.5),breaks=seq(-2,2,1)) + scale_x_continuous("",breaks=seq(1,7,1)) + scale_fill_discrete(guide=F) + stat_summary(fun.y = mean, geom="point",colour="black", size=2.00, shape= 17) + scale_shape_discrete("") + theme_minimal()
custom_car[2, 1] <- plot1
plot1 <- ggplot(x, ggplot2::aes(x=selfplace,y=joint_scaling_ideal_pt,group=selfplace)) + geom_boxplot(colour = "black",outlier.shape = NA)+ scale_y_continuous("",limits=c(-2.5,2.5),breaks=seq(-2,2,1)) + scale_x_continuous("",breaks=seq(1,7,1)) + scale_fill_discrete(guide=F) + stat_summary(fun.y = mean, geom="point",colour="black", size=2.00, shape= 17) + scale_shape_discrete("") + theme_minimal()
custom_car[3, 1] <- plot1
plot1 <- ggplot(x, ggplot2::aes(x=joint_scaling_ideal_pt, y=idealpts_linear_map_overtime,color=pid3)) + ggplot2::geom_point(show.legend =F,size=1,alpha = 0.3) + scale_color_manual("",values=c("blue","red","purple")) + theme_minimal() 
custom_car[3, 2] <- plot1
ggsave(file="citizens_ideal_pts_scatter.png", custom_car, width = 10, height = 8, units = "in")

rm(x,y,plot1,custom_car)

##### Figure 3A Elite Ideal Points by Conventional Ideological Measures, 2008-2016: U.S. House #####  

bonica_scores <- bonica_scores_original

x <- house

bonica_scores <- subset(bonica_scores,select=c("cycle","ICPSR2","Name","recipient_cfscore"))
bonica_scores2 <- bonica_scores
bonica_scores2$cycle <- bonica_scores2$cycle-1
bonica_scores <- rbind(bonica_scores,bonica_scores2)
rm(bonica_scores2)

colnames(bonica_scores) <- c("year","icpsr","name","recipient_cfscore")
bonica_scores$name <- NULL
x$n <- 1
x <- merge(bonica_scores,x,by=c("year","icpsr"),all=T)
x <- subset(x,!is.na(x$mc_name))

x <- data.table(x)
x <-  x[, duplicates := 1:.N , by = c("year","icpsr")]
x <- subset(x,x$duplicates == 1)
x <- subset(x,x$year < 2017)

#x <- subset(x,x$joint_scaling_ideal_pt > -2)

y <- x[,c(3,7,9,10,6)]
y <- na.omit(y)
y$party_code <- factor(y$party_code)

colnames(y) <- c("Bonica CFC Scores", "DW-Nominate 1st Dimension", "Roll-Call Scaling Ideal Points", "Aldrich-McKelvey Ideal Points","party_code")

my_dens <- function(data, mapping, ...) {
  ggplot(data = data, mapping=mapping) +
    geom_density(..., alpha = 0.5, color = "black") + scale_fill_manual("",values=c("blue","red")) 
}

custom_car <- ggpairs(y,columns = c("Bonica CFC Scores", "DW-Nominate 1st Dimension", "Roll-Call Scaling Ideal Points", "Aldrich-McKelvey Ideal Points"),ggplot2::aes(fill = y$party_code),diag = list(continuous = my_dens,lower = "blank"),upper = list(continuous = wrap("cor", size = 8))) + theme_minimal() + theme(legend.position = "bottom")
y1 <- x[,c(3,7,9,10,6)]
y1 <- na.omit(y1)
plot1 <- ggplot(y1, ggplot2::aes(x=recipient_cfscore, y=nominate.dim1, label=party_code,color=party_code)) + ggplot2::geom_text(show.legend =F,size=3,alpha = 0.3) + scale_color_manual("",values=c("blue","red")) + theme_minimal()
plot2 <- ggplot(y1, ggplot2::aes(x=recipient_cfscore, y=joint_scaling_ideal_pt, label=party_code,color=party_code)) + ggplot2::geom_text(show.legend =F,size=3,alpha = 0.3) + scale_color_manual("",values=c("blue","red")) + theme_minimal()
plot3 <- ggplot(y1, ggplot2::aes(x=nominate.dim1, y=joint_scaling_ideal_pt, label=party_code,color=party_code)) + ggplot2::geom_text(show.legend =F,size=3,alpha = 0.3) + scale_color_manual("",values=c("blue","red")) + theme_minimal()
plot4 <- ggplot(y1, ggplot2::aes(x=recipient_cfscore, y=placement.mc_libcon_placement_re, label=party_code,color=party_code)) + ggplot2::geom_text(show.legend =F,size=3,alpha = 0.3) + scale_color_manual("",values=c("blue","red")) + theme_minimal()
plot5 <- ggplot(y1, ggplot2::aes(x=nominate.dim1, y=placement.mc_libcon_placement_re, label=party_code,color=party_code)) + ggplot2::geom_text(show.legend =F,size=3,alpha = 0.3) + scale_color_manual("",values=c("blue","red")) + theme_minimal()
plot6 <- ggplot(y1, ggplot2::aes(x=joint_scaling_ideal_pt, y=placement.mc_libcon_placement_re, label=party_code,color=party_code)) + ggplot2::geom_text(show.legend =F,size=3,alpha = 0.3) + scale_color_manual("",values=c("blue","red")) + theme_minimal()

custom_car[2, 1] <- plot1
custom_car[3, 1] <- plot2
custom_car[3, 2] <- plot3
custom_car[4, 1] <- plot4
custom_car[4, 2] <- plot5
custom_car[4, 3] <- plot6
custom_car[4, 4] <-  ggplot(y1,aes(x=placement.mc_libcon_placement_re,fill=party_code)) + geom_density(alpha = 0.5, color = "black") + scale_fill_manual("",values=c("blue","red")) + theme_minimal() + scale_y_continuous("") + scale_x_continuous("",breaks=seq(-1,1,1),limits=c(-1,1.70)) + theme(legend.position="none")

ggsave(file="house_ideal_pts_scatter.png", custom_car, width = 10, height = 8, units = "in")

rm(plot1,plot2,plot3,plot4,plot5,plot6,x,y,y1,custom_car)

##### Figure 3B Elite Ideal Points by Conventional Ideological Measures, 2008-2016: U.S. Senate #####  

bonica_scores <- bonica_scores_original

x <- subset(senator_ideal_pts,select=c(year,icpsr,mc_name,state,party,nominate.dim1,nominate.dim2,joint_scaling_ideal_pt,senator_ideo_idealpt))

bonica_scores <- subset(bonica_scores,select=c("cycle","ICPSR2","Name","recipient_cfscore"))
bonica_scores <- data.table(bonica_scores)
bonica_scores <-  bonica_scores[, duplicates := 1:.N , by = c("ICPSR2","Name")]
bonica_scores <- subset(bonica_scores,bonica_scores$duplicates == 1)
bonica_scores$cycle <- NULL
bonica_scores$duplicates <- NULL
bonica_scores <- data.frame(bonica_scores)

colnames(bonica_scores) <- c("icpsr","name","recipient_cfscore")
bonica_scores$name <- NULL
x$n <- 1
x <- merge(bonica_scores,x,by=c("icpsr"),all=T)
x <- subset(x,!is.na(x$mc_name))

x <- data.table(x)
x <-  x[, duplicates := 1:.N , by = c("year","icpsr")]
x <- subset(x,x$duplicates == 1)

x <- subset(x,x$year < 2017)

#x <- subset(x,x$joint_scaling_ideal_pt > -2)

y <- x[,c(2,7,9,10,6)]
y <- na.omit(y)
y$party <- ifelse(y$party %in% c("D","I"),"D",ifelse(y$party %in% c("R"),"R",NA))
y$party_code <- factor(y$party)

y$party <- NULL
colnames(y) <- c("Bonica CFC Scores", "DW-Nominate 1st Dimension", "Roll-Call Scaling Ideal Points", "Aldrich-McKelvey Ideal Points","party_code")

my_dens <- function(data, mapping, ...) {
  ggplot(data = data, mapping=mapping) +
    geom_density(..., alpha = 0.5, color = "black") + scale_fill_manual("",values=c("blue","red")) 
}

custom_car <- ggpairs(y,columns = c("Bonica CFC Scores", "DW-Nominate 1st Dimension", "Roll-Call Scaling Ideal Points", "Aldrich-McKelvey Ideal Points"),ggplot2::aes(fill = y$party_code),diag = list(continuous = my_dens,lower = "blank"),upper = list(continuous = wrap("cor", size = 8))) + theme_minimal() + theme(legend.position = "bottom")
#ggpairs(y, lower = "blank", title = "Custom Example")
y1 <- x[,c(2,7,9,10,6)]
y1 <- na.omit(y1)
y1$party <- ifelse(y1$party %in% c("D","I"),"D",ifelse(y1$party %in% c("R"),"R",NA))
y1$party_code <- factor(y1$party)
plot1 <- ggplot(y1, ggplot2::aes(x=recipient_cfscore, y=nominate.dim1, label=party_code,color=party_code)) + ggplot2::geom_text(show.legend =F,size=3,alpha = 0.3) + scale_color_manual("",values=c("blue","red")) + theme_minimal()
plot2 <- ggplot(y1, ggplot2::aes(x=recipient_cfscore, y=joint_scaling_ideal_pt, label=party_code,color=party_code)) + ggplot2::geom_text(show.legend =F,size=3,alpha = 0.3) + scale_color_manual("",values=c("blue","red")) + theme_minimal()
plot3 <- ggplot(y1, ggplot2::aes(x=nominate.dim1, y=joint_scaling_ideal_pt, label=party_code,color=party_code)) + ggplot2::geom_text(show.legend =F,size=3,alpha = 0.3) + scale_color_manual("",values=c("blue","red")) + theme_minimal()
plot4 <- ggplot(y1, ggplot2::aes(x=recipient_cfscore, y=senator_ideo_idealpt, label=party_code,color=party_code)) + ggplot2::geom_text(show.legend =F,size=3,alpha = 0.3) + scale_color_manual("",values=c("blue","red")) + theme_minimal()
plot5 <- ggplot(y1, ggplot2::aes(x=nominate.dim1, y=senator_ideo_idealpt, label=party_code,color=party_code)) + ggplot2::geom_text(show.legend =F,size=3,alpha = 0.3) + scale_color_manual("",values=c("blue","red")) + theme_minimal()
plot6 <- ggplot(y1, ggplot2::aes(x=joint_scaling_ideal_pt, y=senator_ideo_idealpt, label=party_code,color=party_code)) + ggplot2::geom_text(show.legend =F,size=3,alpha = 0.3) + scale_color_manual("",values=c("blue","red")) + theme_minimal()

custom_car[2, 1] <- plot1
custom_car[3, 1] <- plot2
custom_car[3, 2] <- plot3
custom_car[4, 1] <- plot4
custom_car[4, 2] <- plot5
custom_car[4, 3] <- plot6
custom_car[4, 4] <-  ggplot(y1,aes(x=senator_ideo_idealpt,fill=party_code)) + geom_density(alpha = 0.5, color = "black") + scale_fill_manual("",values=c("blue","red")) + theme_minimal() + scale_y_continuous("") + scale_x_continuous("",breaks=seq(-1,1,1),limits=c(-1,1.11)) + theme(legend.position="none")

ggsave(file="senate_ideal_pts_scatter.png", custom_car, width = 10, height = 8, units = "in")