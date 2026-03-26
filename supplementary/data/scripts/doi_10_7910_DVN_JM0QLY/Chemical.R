

data1<-Final_Chemical

#Gel consistency................................

data2Q<-Final_Chemicalq #Qualitative data 

summary(data1)
data1$Trt<- as.factor(data1$Trt)
data1$Rep<- as.factor(data1$Rep)

#performing anova
anova<- aov(GC~Trt, data=data1)
summary(anova)

#information for means
m<-model.tables(anova, "means")


#for LSD test
comparison<-LSD.test(anova, "Trt", alpha=0.05, group=TRUE)
comparison


#to get row names in column 
GCdata<-comparison$means
GCdata
GCdata <- add_rownames(GCdata, "Trt")
GCdata
GCdata<-as.data.frame(GCdata)
GCdata

#calculating standard error
GCdata$GCse<- GCdata$std/ sqrt(GCdata$r)
GCdata


#delete unnessesary column
GCdata<-select(GCdata, -5:-11)
GCdata

#edit column name

names(GCdata)<- c("Trt","GCmean","GCsd","n", "GCse")
GCdata

# Rounding off
GCdata$GCmean<-round(GCdata$GCmean, 2)
GCdata$GCsd<-round(GCdata$GCsd, 2)
GCdata$GCse<-round(GCdata$GCse, 3)

GCdata

#Merging two data frame 
GCdata<-merge(GCdata,data2Q,by ="Trt")
GCdata

#Arranging 

Trtorder<- GCdata$Trt[order(GCdata$GCmean)]


GCdata$Trt <- factor(GCdata$Trt, levels = Trtorder)



#Plotting 

ggplot(GCdata, aes(x = Trt))+
  geom_col(aes(y= GCmean, fill=GB), alpha= 0.6)+
  geom_text(aes(label= GCmean, y=GCmean), size=3.8,vjust=-1.5, colour="Red")+
  geom_errorbar(aes(ymin=GCmean-GCse, ymax=GCmean+GCse),size=0.3, width=.5)+
  theme(panel.grid.major.y = element_line(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle=50, hjust=1,size=rel(1.4)),
        axis.title.x = element_text(size = rel(2)),
        axis.text.y = element_text( size=rel(1.4)),
        axis.title.y = element_text(size = rel(2)))+
  labs(fill="Gelatinisation Behaviour")+
  ylab("Gel Consistency(cm)")+ xlab("Treatment")+
  theme(legend.position=c(-0.2,1), legend.justification=c(-1,1),
        legend.key = element_rect(size = rel(1)), 
        legend.title = element_text(size = rel(1.2)),legend.text = element_text(size = rel(1.2)))


#Amylose Content_________________________________

data1<-Final_Chemical
data1$AC<- round(data1$AC,2)
#performing anova
anova<- aov(AC~Trt, data=data1)
summary(anova)

#information for means
m<-model.tables(anova, "means")


#for LSD test
comparison<-LSD.test(anova, "Trt", alpha=0.05, group=TRUE)
comparison


#to get row names in column 
ACdata<-comparison$means
ACdata
ACdata <- add_rownames(ACdata, "Trt")
ACdata
ACdata<-as.data.frame(ACdata)
ACdata

#calculating standard error
ACdata$ACse<- ACdata$std/ sqrt(ACdata$r)
ACdata


#delete unnessesary column
ACdata<-select(ACdata, -5:-11)
ACdata

#edit column name

names(ACdata)<- c("Trt","ACmean","ACsd","n", "ACse")
ACdata

# Rounding off
ACdata$ACmean<-round(ACdata$ACmean, 2)
ACdata$ACsd<-round(ACdata$ACsd, 2)
ACdata$ACse<-round(ACdata$ACse, 3)
ACdata

#Arranging
Trtorder2<- ACdata$Trt[order(ACdata$ACmean)]


ACdata$Trt <- factor(ACdata$Trt, levels = Trtorder2)

#plotting data
ggplot(ACdata, aes(x=Trt, y=ACdata$ACmean))+ 
  geom_segment(aes(xend=Trt, yend=0), colour="#009E73",alpha=0.7, size=1.3)+
  geom_point(size=3, colour="#D55E00")+
  geom_errorbar(aes(ymin=ACmean-ACse, ymax=ACmean+ACse),size=0.9,alpha=0.5, width=.5)+
  scale_color_discrete(guide= FALSE)+
  theme_bw()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x= element_line(colour ="grey60", linetype = "dashed"),
        axis.text.x = element_text(angle=50, hjust=1,size=rel(1.4)),
        axis.title.x = element_text(size = rel(2)),
        axis.text.y = element_text( size=rel(1.7)),
        axis.title.y = element_text(size = rel(2)))+
  ylab("Amylose Content(%)")+ xlab("Treatment")
