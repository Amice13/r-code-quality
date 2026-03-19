library(gdata)
library(betareg)
library(memisc)
library(descr)
library(MASS)

data <- read.xls("dataset1.xlsx")
str(data)

data$location <- as.factor(data$Place.of.origin)
data$location <- relevel(data$location, ref="mainland")
levels(data$location) <- c("Mainland","Hong Kong","Taiwan")

data$gender <- as.factor(data$Gender)
levels(data$gender) <- c("Female", "Male")

data$college <- as.factor(data$College)
levels(data$college) <- c("No College Degree", "College Degree")

data$age <- 2021-data$Birth.year

##table 4
summary(lm(官媒转发.转发均值 ~ age +Followers+gender+college+location, data=data))
summary(lm(官媒转发.评论均值 ~ age +Followers+gender+college+location, data=data))
summary(lm(官媒转发.点赞均值 ~ age +Followers+gender+college+location, data=data))

##table 3
summary(lm(转发差别 ~ Followers+gender+age+college+location, data=data))
summary(lm(评论差别 ~ Followers+gender+age+college+location, data=data))
summary(lm(点赞差别 ~ Followers+gender+age+college+location, data=data))

##figure 2
pdf(file="difference.pdf")
par(mfrow=c(1,3))
plot(data$转发差别, ylim=c(-400000, 300000), ylab="Share Difference", xlab="Celebrities", pch=20)
abline(h=0, col="black")
plot(data$评论差别, ylim=c(-400000, 300000), ylab="Comment Difference", xlab="Celebrities", pch=20)
abline(h=0, col="black")
plot(data$点赞差别, ylim=c(-400000, 300000), ylab="Like Difference", xlab="Celebrities", pch=20)
abline(h=0, col="black")
dev.off()

###message as unit of analysis
data <- read.csv("dataset2.csv")

data$politics <- NA
data$politics[data$code==1] <- 3
data$politics[data$code<8 & data$code>1] <- 2
data$politics[data$code>7] <- 1
table(data$politics)


##table 5
summary(lm(retweets~politics, data=data))
summary(lm(comments~politics, data=data))
summary(lm(likes~politics, data=data))

##figure 1
pdf(file="categories.pdf")
par(mfrow=c(1, 2))
barplot(table(data$politics), ylim=c(0, 3500), xlab="Level of Politics (low to high)", ylab="Frequency")
barplot(table(data$code), ylim=c(0, 1200), xlab="Coding Categories", ylab="Frequency")
dev.off()

code1 <- subset(data, data$code==1)
code2 <- subset(data, data$code==2)
code3 <- subset(data, data$code==3)
code4 <- subset(data, data$code==4)
code5 <- subset(data, data$code==5)
code6 <- subset(data, data$code==6)
code7 <- subset(data, data$code==7)
code8 <- subset(data, data$code==8)
code9 <- subset(data, data$code==9)
code10 <- subset(data, data$code==10)
code11 <- subset(data, data$code==11)

retweet1 <- mean(code1$retweets)
retweet2 <- mean(code2$retweets)
retweet3 <- mean(code3$retweets)
retweet4 <- mean(code4$retweets)
retweet5 <- mean(code5$retweets)
retweet6 <- mean(code6$retweets)
retweet7 <- mean(code7$retweets)
retweet8 <- mean(code8$retweets)
retweet9 <- mean(code9$retweets)
retweet10 <- mean(code10$retweets)
retweet11 <- mean(code11$retweets)

comment1 <- mean(code1$comments)
comment2 <- mean(code2$comments)
comment3 <- mean(code3$comments)
comment4 <- mean(code4$comments)
comment5 <- mean(code5$comments)
comment6 <- mean(code6$comments)
comment7 <- mean(code7$comments)
comment8 <- mean(code8$comments)
comment9 <- mean(code9$comments)
comment10 <- mean(code10$comments)
comment11 <- mean(code11$comments)

like1 <- mean(code1$likes)
like2 <- mean(code2$likes)
like3 <- mean(code3$likes)
like4 <- mean(code4$likes)
like5 <- mean(code5$likes)
like6 <- mean(code6$likes)
like7 <- mean(code7$likes)
like8 <- mean(code8$likes)
like9 <- mean(code9$likes)
like10 <- mean(code10$likes)
like11 <- mean(code11$likes)

retweetpol1 <- mean(retweet8, retweet9, retweet10, retweet11)
retweetpol2 <- mean(retweet2, retweet3, retweet4, retweet5, retweet6, retweet7)
retweetpol3 <- retweet1

commentpol1 <- mean(comment8, comment9, comment10, comment11)
commentpol2 <- mean(comment2, comment3, comment4, comment5, comment6, comment7)
commentpol3 <- comment1

likepol1 <- mean(like8, like9, like10, like11)
likepol2 <- mean(like2, like3, like4, like5, like6, like7)
likepol3 <- like1

retweetavepol3 <- data.frame(name=c(1, 2, 3), value=c(retweetpol1, retweetpol2, retweetpol3))
commentavepol3 <- data.frame(name=c(1, 2, 3), value=c(commentpol1, commentpol2, commentpol3))
likeavepol3 <- data.frame(name=c(1, 2, 3), value=c(likepol1, likepol2, likepol3))

##figure 3
pdf(file="3politicsinteraction.pdf")
par(mfrow=c(1, 3))
barplot(height=retweetavepol3$value, width=c(0.5, 0.5, 0.5), names=retweetavepol3$name, ylim=c(0, 200000), ylab="Share Averages")
barplot(height=commentavepol3$value, names=commentavepol3$name, ylim=c(0, 200000), xlab="Level of Politics (low to high)", ylab="Comment Averages")
barplot(height=likeavepol3$value, names=likeavepol3$name, ylim=c(0, 200000), ylab="Like Averages")
dev.off()

