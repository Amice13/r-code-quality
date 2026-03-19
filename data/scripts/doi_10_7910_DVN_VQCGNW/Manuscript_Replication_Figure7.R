library(ggplot2)
library(plyr)
library(descr)
library(psychometric)

# The following code recreates Figure 2 showing the increasing correlation between partisanship and ideology using the ANES

setwd("/Final Data Replication Files")

load("Figure7_pid_ideo_correlation.Rdata")

ideo <- subset(nes,select=c(VCF0004,VCF0009x,VCF0803,VCF0301))
ideo2 <- subset(nes2016,select=c(V160102,V161158x,V161126))
ideo2$year <- 2016
colnames(ideo2) <- c("weight","pid7","libcon","year")
colnames(ideo) <- c("year","weight","libcon","pid7")

ideo[] <- lapply(ideo, as.character)
ideo2[] <- lapply(ideo2, as.character)

ideo <- rbind(ideo,ideo2)

na <- data.frame(freq(ideo$libcon,plot=F))
na$category <- rownames(na)
rownames(na) <- NULL
ideo$libcon[ideo$libcon %in% as.character(na[c(4:5),4])] <- 1
ideo$libcon[ideo$libcon %in% as.character(na[c(6:7),4])] <- 2
ideo$libcon[ideo$libcon %in% as.character(na[c(8:9),4])] <- 3
ideo$libcon[ideo$libcon %in% as.character(na[c(10:11),4])] <- 4
ideo$libcon[ideo$libcon %in% as.character(na[c(12:13),4])] <- 5
ideo$libcon[ideo$libcon %in% as.character(na[c(14:15),4])] <- 6
ideo$libcon[ideo$libcon %in% as.character(na[c(16:17),4])] <- 7
ideo$libcon[ideo$libcon %in% as.character(na[c(1:3,18:20),4])] <- NA

na <- data.frame(freq(ideo$pid7,plot=F))
na$category <- rownames(na)
rownames(na) <- NULL
ideo$pid7[ideo$pid7 %in% as.character(na[c(4:5),4])] <- 1
ideo$pid7[ideo$pid7 %in% as.character(na[c(6:7),4])] <- 2
ideo$pid7[ideo$pid7 %in% as.character(na[c(8:9),4])] <- 3
ideo$pid7[ideo$pid7 %in% as.character(na[c(10:11),4])] <- 4
ideo$pid7[ideo$pid7 %in% as.character(na[c(12:13),4])] <- 5
ideo$pid7[ideo$pid7 %in% as.character(na[c(14:15),4])] <- 6
ideo$pid7[ideo$pid7 %in% as.character(na[c(16:17),4])] <- 7
ideo$pid7[ideo$pid7 %in% as.character(na[c(1:3,18),4])] <- NA

ideo <- na.omit(ideo)
ideo[] <- lapply(ideo, as.numeric)

x <- unique(ideo$year)
corr <- list()
for(i in unique(ideo$year)){
  a <- data.frame(ideo=ideo$libcon[ideo$year == i],pid7=ideo$pid7[ideo$year == i],weight=ideo$weight[ideo$year == i])
  n <- nrow(a)
  a <- cor(a$ideo, a$pid7,use="complete.obs")
  a <- data.frame(a)
  a$year <- i
  colnames(a) <- c("corr","year")
  x <- CIr(r=a$corr, n = n, level = .95)
  a$lower <- x[1]
  a$upper <- x[2]
  a$sample_n <- n
  corr[[i]] <- a
}
corr <- ldply(corr, data.frame)

corr$survey <- "ANES"

plot <- ggplot(corr,aes(x=year,y=corr,ymax=upper,ymin=lower)) + geom_line(size = 1) + scale_x_continuous("Data: American National Election Study (ANES)",breaks=seq(1972,2016,4)) + geom_pointrange() + scale_y_continuous("Pearson's Correlation Coefficient",breaks=seq(0.3,0.7,0.10)) + theme_minimal()
ggsave(file="correlation_pid7_libcon7.png", plot, width = 7.5, height = 5, units = "in") 
