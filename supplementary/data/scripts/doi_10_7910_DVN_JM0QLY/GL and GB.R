data1<-FINAL_pHYSICAL

summary(data1)

data1$GD<-as.factor(data1$GD)
data1$GS<-as.factor(data1$GS)
summary(data1)
str(data1)

#Arranging in increasing of GL
Trtorder<- data1$Trt[order(data1$GL)]


data1$Trt <- factor(data1$Trt, levels = Trtorder)

#plotting data
ggplot(data1, aes(x = Trt))+
  geom_col(aes(y= GL, fill="Red"), alpha= 0.6)+
  geom_text(aes(label= GL, y=GL), size=4.5,vjust=1.5, colour="black")+
  geom_errorbar(aes(ymin=GL-GLSE, ymax=GL+GLSE),size=0.3, width=.5)+
  geom_col(aes(y=GB, fill= "Yellow"), alpha =0.7)+
  geom_text(aes(label= GB, y=GB), size=4.5, vjust=1.5, colour="black")+
  geom_errorbar(aes(ymin=GB-GBSE, ymax=GB+GBSE),size=0.3, width=.5)+
  facet_grid(GD~., scales="free_y", space="free_x")+
  theme(panel.grid.major.y = element_line(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle=50, hjust=1,size=rel(2)),
        axis.title.x = element_text(size = rel(2)),
        axis.text.y = element_text( size=rel(1.4)),
        axis.title.y = element_text(size = rel(2)))+
  labs(fill="Grain Dimension")+
  scale_fill_discrete(labels=c("Grain Length", "Grain Breadth"))+
  ylab("Grain Breadth and Grain Length (mm)")+ xlab("Treatment")+
  theme(legend.position=c(0,1), legend.justification=c(-0,1),
        legend.key = element_rect(size = rel(1.4)), 
        legend.title = element_text(size = rel(1.4)),legend.text = element_text(size = rel(1.4)))+
  theme(strip.text = element_text(face="bold", size=rel(1.5)),
        strip.background = element_rect(fill="lightblue", colour="black",
                                        size=1))

