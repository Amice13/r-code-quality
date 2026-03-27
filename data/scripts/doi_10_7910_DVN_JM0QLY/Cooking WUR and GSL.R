data3<-Final_Cooking 

summary(data3)
data3$Trt<- as.factor(data3$Trt)
data3$Rep<- as.factor(data3$Rep)

#performing anova for GSL
anova<- aov(GSL~Trt, data=data3)
summary(anova)

#information for means
m<-model.tables(anova, "means")
m

#for LSD test
comparison<-LSD.test(anova, "Trt", alpha=0.05, group=TRUE)
comparison


#to get row names in column 
GSLdata<-comparison$means
GSLdata
GSLdata <- add_rownames(GSLdata, "Trt")
GSLdata
GSLdata<-as.data.frame(GSLdata)
GSLdata

#calculating standard error
GSLdata$GSLse<- GSLdata$std/ sqrt(GSLdata$r)
GSLdata


#delete unnessesary column
GSLdata<-select(GSLdata, -5:-11)
GSLdata

#edit column name

names(GSLdata)<- c("Trt","GSLmean","GSLsd","n", "GSLse")
GSLdata

# Rounding off
GSLdata$GSLmean<-round(GSLdata$GSLmean, 2)
GSLdata$GSLsd<-round(GSLdata$GSLsd, 2)
GSLdata$GSLse<-round(GSLdata$GSLse, 3)

GSLdata

#Arranging 

#Trtorder<- GSLdata$Trt[order(GSLdata$GSLmean)]


#GSLdata$Trt <- factor(GSLdata$Trt, levels = Trtorder)

#performing anova for WUR
anova1<- aov(WUR~Trt, data=data3)
summary(anova1)

#information for means
m1<-model.tables(anova1, "means")
m1

#for LSD test
comparison1<-LSD.test(anova1, "Trt", alpha=0.05, group=TRUE)
comparison1


#to get row names in column 
WURdata<-comparison1$means
WURdata
WURdata <- add_rownames(WURdata, "Trt")
WURdata
WURdata<-as.data.frame(WURdata)
WURdata

#calculating standard error
WURdata$WURse<- WURdata$std/ sqrt(WURdata$r)
WURdata


#delete unnessesary column
WURdata<-select(WURdata, -5:-11)
WURdata

#edit column name

names(WURdata)<- c("Trt","WURmean","WURsd","n", "WURse")
WURdata

# Rounding off
WURdata$WURmean<-round(WURdata$WURmean, 2)
WURdata$WURsd<-round(WURdata$WURsd, 2)
WURdata$WURse<-round(WURdata$WURse, 3)

WURdata

#Arranging 

#Trtorder1<- WURdata$Trt[order(WURdata$WURmean)]


#WURdata$Trt <- factor(WURdata$Trt, levels = Trtorder1)

#Merging two data frame 
GSLWURdata<-merge(GSLdata,WURdata,by ="Trt")
GSLWURdata

#Plotting 

ggplot(GSLWURdata, aes(x = Trt))+
  geom_col(aes(y= GSLmean),fill="#008080" , alpha= 0.6)+
  geom_errorbar(aes(ymin=GSLmean-GSLse, ymax=GSLmean+GSLse),size=0.6, width=.5)+
  geom_line(aes(y=WURmean), group=FALSE, colour="#6A5ACD", size=1.2, linetype="dashed")+
  geom_errorbar(aes(ymin=WURmean-WURse, ymax=WURmean+WURse),size=1, width=.5)+
  geom_point(aes(y=WURmean), colour="#FF6347", size=2)+
  scale_y_continuous(sec.axis = sec_axis(~., name = "Gruel Solid loss"))+
  scale_fill_discrete(guide= FALSE)+
  theme(panel.grid.major.y = element_line(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle=50, hjust=1,size=rel(1.4)),
        axis.title.x = element_text(size = rel(2)),
        axis.text.y = element_text( size=rel(1.4)),
        axis.title.y = element_text(size = rel(2), colour = "#FF6347"),
        axis.title.y.right = element_text(colour = "#008080"))+
  ylab("Water Uptake Ratio")+ xlab("Treatment")
