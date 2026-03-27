data3<-Final_Cooking 

summary(data3)
data3$Trt<- as.factor(data3$Trt)
data3$Rep<- as.factor(data3$Rep)

#performing anova for OCT
anova<- aov(OCT~Trt, data=data3)
summary(anova)

#information for means
m<-model.tables(anova, "means")
m

#for LSD test
comparison<-LSD.test(anova, "Trt", alpha=0.05, group=TRUE)
comparison


#to get row names in column 
OCTdata<-comparison$means
OCTdata
OCTdata <- add_rownames(OCTdata, "Trt")
OCTdata
OCTdata<-as.data.frame(OCTdata)
OCTdata

#calculating standard error
OCTdata$OCTse<- OCTdata$std/ sqrt(OCTdata$r)
OCTdata


#delete unnessesary column
OCTdata<-select(OCTdata, -5:-11)
OCTdata

#edit column name

names(OCTdata)<- c("Trt","OCTmean","OCTsd","n", "OCTse")
OCTdata

# Rounding off
OCTdata$OCTmean<-round(OCTdata$OCTmean, 2)
OCTdata$OCTsd<-round(OCTdata$OCTsd, 2)
OCTdata$OCTse<-round(OCTdata$OCTse, 3)

OCTdata

#Arranging 

#Trtorder<- OCTdata$Trt[order(OCTdata$OCTmean)]


#OCTdata$Trt <- factor(OCTdata$Trt, levels = Trtorder)

data<-Final_Chemicalq
#Plotting 

ggplot(OCTdata, aes(x = Trt))+
  geom_col(aes(y= OCTmean),fill="#6B8E23" , alpha= 0.6)+
  geom_text(aes(label= OCTmean, y=OCTmean), size=3.5,vjust=2.5, colour="black")+
  geom_errorbar(aes(ymin=OCTmean-OCTse, ymax=OCTmean+OCTse),size=0.3, width=.5)+
  scale_fill_discrete(guide= FALSE)+
  facet_grid(data$GT~., scales="free_y", space="free_x")+
  theme(panel.grid.major.y = element_line(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle=50, hjust=1,size=rel(1.5)),
        axis.title.x = element_text(size = rel(2)),
        axis.text.y = element_text( size=rel(1.4)),
        axis.title.y = element_text(size = rel(2)))+
  ylab("Optimum Cooking Time")+ xlab("Treatment")



