data2<-FINAL_pHYSICAL
summary(data2)
data2$GS<- as.factor(data2$GS)

# Arranging data
Trtorder<- data2$Trt[order(data2$LB)]


data2$Trt <- factor(data2$Trt, levels = Trtorder)

#plotting data
ggplot(data2, aes(x=Trt, y=data2$LB))+ 
  geom_segment(aes(xend=Trt, yend=0))+
  geom_point(size=2.5, aes(colour= data2$GS))+
  geom_text(aes(label= LB, y=LB), size=5,hjust=-1.5, colour="black")+
  geom_errorbar(aes(ymin=LB-LBSE, ymax=LB+LBSE),size=0.9,alpha=0.5, width=.5)+
  scale_fill_brewer(palette = "Pastel1")+
  scale_color_discrete(guide= FALSE)+
  facet_grid(GS~., scales="free_y", space="free_y")+
    theme_bw()+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y= element_line(colour ="grey60", linetype = "dashed"),
        axis.text.x = element_text(size=rel(1.7)),
        axis.title.x = element_text(size = rel(2)),
        axis.text.y = element_text( size=rel(1.7)),
        axis.title.y = element_text(size = rel(2)))+
  labs(colour="Grain Shape")+
  ylab("Length-Breadth Ratio")+ xlab("Treatment")+
  theme(strip.text = element_text(face="bold", size=rel(1.5)),
        strip.background = element_rect(fill="lightgreen", colour="black",
                                        size=1))+
  coord_flip()

