
data3<-Final_Length
summary(data3)

data3$Trt<- as.factor(data3$Trt)
data3$Rep<-as.factor(data3$Rep)

str(data3)

####FOR GRAIN LEGTH-------------------------------

#performing anova
anova1<- aov(GL~Trt, data=data3)
summary(anova1)

#information for means
m1<-model.tables(anova1, "means")


#for LSD test
comparison1<-LSD.test(anova1, "Trt", alpha=0.05, group=TRUE)
comparison1


#to get row names in column 
GLdata<-comparison1$means
GLdata
GLdata <- add_rownames(GLdata, "Trt")
GLdata
GLdata<-as.data.frame(GLdata)
GLdata


GLdata$GLse<- GLdata$std/ sqrt(GLdata$r)
GLdata

#delete unnessesary column
GLdata<-select(GLdata, -5:-11)
GLdata

#edit column name

names(GLdata)<- c("Trt","GLmean","GLsd","n", "GLse")
GLdata

# Rounding off
GLdata$GLmean<-round(GLdata$GLmean, 2)
GLdata$GLsd<-round(GLdata$GLsd, 2)
GLdata$GLse<-round(GLdata$GLse, 3)

GLdata


#####FOR GRAIN BREADTH______________________
#performing anova
anova2<- aov(GB~Trt, data=data3)
summary(anova2)

#information for means
m2<-model.tables(anova2, "means")


#for LSD test
comparison2<-LSD.test(anova2, "Trt", alpha=0.05, group=TRUE)
comparison2


#to get row names in column 
GBdata<-comparison2$means
GBdata
GBdata <- add_rownames(GBdata, "Trt")
GBdata
GBdata<-as.data.frame(GBdata)
GBdata

#calculating standard error
GBdata$GBse<- GBdata$std/ sqrt(GBdata$r)
GBdata


#delete unnessesary column
GBdata<-select(GBdata, -5:-11)
GBdata

#edit column name

names(GBdata)<- c("Trt","GBmean","GBsd","n", "GBse")
GBdata

# Rounding off
GBdata$GBmean<-round(GBdata$GBmean, 2)
GBdata$GBsd<-round(GBdata$GBsd, 2)
GBdata$GBse<-round(GBdata$GBse, 3)

GBdata


#Merging two data frame 
GLBdata<-merge(GLdata,GBdata,by ="Trt")
GLBdata

