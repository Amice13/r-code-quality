#ch8
id<-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
pa<-c(65,74,56,93,51,87,86,73,95,81,71,84,84,68,59)
bmi<-c(30,22,34,21,30,24,26,24,22,21,30,21,24,25,29)
dt<-data.frame(id,pa,bmi)
#test normality
shapiro.test(dt$pa)
shapiro.test(dt$bmi)
#correlation test
cor.test(pa, bmi, method=c("pearson"))

#graph
library(ggplot2)
g0<-ggplot(dt, aes(x=pa, y=bmi)) + 
  geom_point(shape = 21,size=4)+
  geom_smooth(method=lm, se=TRUE)
g1 <- g0+labs(x = "Physical activity scores", y = "Body mass index")
g1 + theme_light()

library(ggplot2)
g0<-ggplot(dt, aes(x=pa, y=bmi)) + 
  geom_point(shape = 21,size=4)
g1 <- g0+labs(x = "Physical activity scores", y = "Body mass index")
g1 + theme_light()

#ex 8.2
apgar1min<-c(10,3,8,9,8,9,8,8,8,8,7,8,7)
apgar5min<-c(10,6,9,10,9,10,9,9,9,9,9,9,9)
dt<-data.frame(apgar1min,apgar5min)
#test normality
shapiro.test(dt$apgar1min)
shapiro.test(dt$apgar5min)

cor.test(dt$apgar1min,dt$apgar5min, method=c("spearman"))

#simple linear
id<-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
pa<-c(65,74,56,93,51,87,86,73,95,81,71,84,84,68,59)
bmi<-c(30,22,34,21,30,24,26,24,22,21,30,21,24,25,29)
dt<-data.frame(id,pa,bmi)
m0<-lm(bmi~pa,data=dt)
summary(m0)
