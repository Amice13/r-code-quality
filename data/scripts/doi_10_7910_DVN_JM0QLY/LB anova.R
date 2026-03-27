
data4<-Final_Length
summary(data4)

data4$Trt<- as.factor(data4$Trt)
data4$Rep<-as.factor(data4$Rep)

str(data4)


#performing anova
anova<- aov(LB~Trt, data=data4)
summary(anova)

#information for means
m<-model.tables(anova, "means")


#for LSD test
comparison<-LSD.test(anova, "Trt", alpha=0.05, group=TRUE)
comparison


#to get row names in column 
LBdata<-comparison$means
LBdata
LBdata <- add_rownames(LBdata, "Trt")
LBdata
LBdata<-as.data.frame(LBdata)
LBdata

#calculating standard error
LBdata$GBse<- LBdata$std/ sqrt(LBdata$r)
LBdata


#delete unnessesary column
LBdata<-select(LBdata, -5:-11)
LBdata

#edit column name

names(LBdata)<- c("Trt","LBmean","LBsd","n", "LBse")
LBdata

# Rounding off
LBdata$LBmean<-round(LBdata$LBmean, 2)
LBdata$LBsd<-round(LBdata$LBsd, 2)
LBdata$LBse<-round(LBdata$LBse, 3)

LBdata
