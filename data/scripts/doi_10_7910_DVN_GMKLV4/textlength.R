dat <- read.delim("textlength.csv")
means <- aggregate(dat$Textlength ~ dat$Rating, textlength, mean)
colnames(means) <- c("Rating","Mean")
medians <- aggregate(dat$Textlength ~ dat$Rating, textlength, median)
means$Median <- medians$`dat$Textlength`
write.csv(means, file = "averagetextlength.csv", row.names=FALSE)