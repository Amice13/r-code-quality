data3<-Final_Cooking

summary(data3)
data3$Trt<- as.factor(data3$Trt)
data3$Rep<- as.factor(data3$Rep)

#performing anova for ER
anova<- aov(ER~Trt, data=data3)
summary(anova)

#information for means
m<-model.tables(anova, "means")
m

#for LSD test
comparison<-LSD.test(anova, "Trt", alpha=0.05, group=TRUE)
comparison


#to get row names in column 
ERdata<-comparison$means
ERdata
ERdata <- add_rownames(ERdata, "Trt")
ERdata
ERdata<-as.data.frame(ERdata)
ERdata

#calculating standard error
ERdata$ERse<- ERdata$std/ sqrt(ERdata$r)
ERdata


#delete unnessesary column
ERdata<-select(ERdata, -5:-11)
ERdata

#edit column name

names(ERdata)<- c("Trt","ERmean","ERsd","n", "ERse")
ERdata

# Rounding off
ERdata$ERmean<-round(ERdata$ERmean, 2)
ERdata$ERsd<-round(ERdata$ERsd, 2)
ERdata$ERse<-round(ERdata$ERse, 3)

ERdata

#Arranging 

#Trtorder<- ERdata$Trt[order(ERdata$ERmean)]


#ERdata$Trt <- factor(ERdata$Trt, levels = Trtorder)

#performing anova for CLBR
anova1<- aov(CLBR~Trt, data=data3)
summary(anova1)

#information for means
m1<-model.tables(anova1, "means")
m1

#for LSD test
comparison1<-LSD.test(anova1, "Trt", alpha=0.05, group=TRUE)
comparison1


#to get row names in column 
CLBRdata<-comparison1$means
CLBRdata
CLBRdata <- add_rownames(CLBRdata, "Trt")
CLBRdata
CLBRdata<-as.data.frame(CLBRdata)
CLBRdata

#calculating standard error
CLBRdata$CLBRse<- CLBRdata$std/ sqrt(CLBRdata$r)
CLBRdata


#delete unnessesary column
CLBRdata<-select(CLBRdata, -5:-11)
CLBRdata

#edit column name

names(CLBRdata)<- c("Trt","CLBRmean","CLBRsd","n", "CLBRse")
CLBRdata

# Rounding off
CLBRdata$CLBRmean<-round(CLBRdata$CLBRmean, 2)
CLBRdata$CLBRsd<-round(CLBRdata$CLBRsd, 2)
CLBRdata$CLBRse<-round(CLBRdata$CLBRse, 3)

CLBRdata

#Arranging 

#Trtorder1<- CLBRdata$Trt[order(CLBRdata$CLBRmean)]


#CLBRdata$Trt <- factor(CLBRdata$Trt, levels = Trtorder1)

#Merging two data frame 
ERCLBRdata<-merge(ERdata,CLBRdata,by ="Trt")
ERCLBRdata

#Plotting 

ggplot(ERCLBRdata, aes(x = Trt))+
  geom_col(aes(y= CLBRmean),fill="#800000" , alpha= 0.6)+
  geom_errorbar(aes(ymin=CLBRmean-CLBRse, ymax=CLBRmean+CLBRse),size=0.3, width=.5)+
  geom_area(aes(y=ERmean), group=FALSE, fill="#4682B4", alpha=0.4)+
  geom_point(aes(y=ERmean))+
  scale_y_continuous(sec.axis = sec_axis(~., name = "Elongation Ratio"))+
  scale_fill_discrete(guide= FALSE)+
  theme(panel.grid.major.y = element_line(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle=50, hjust=1,size=rel(1.4)),
        axis.title.x = element_text(size = rel(2)),
        axis.text.y = element_text( size=rel(1.4)),
        axis.title.y = element_text(size = rel(2), colour = "#800000"),
        axis.title.y.right = element_text(colour = "#4682B4"))+
  ylab("Cooking Length Breadth Ratio")+ xlab("Treatment")


