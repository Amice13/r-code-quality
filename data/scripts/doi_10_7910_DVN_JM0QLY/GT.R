data2Q<-Final_Chemicalq
data<-data2Q

library(ggplot2)
m<-table(data$GT)
n<-as.data.frame(m)
n

# Create a basic bar
pie<-ggplot(data = n, aes(x=n$Var1, y=n$Freq, fill= n$Var1)) + geom_bar(stat="identity", width=1)+
  scale_fill_brewer(palette = "Accent")

pie
# Convert to pie (polar coordinates) and add labels
pie + coord_polar("x", start=0) + geom_text(aes(label = paste0(round((n$Freq/sum(n$Freq))*100), "%")),size=6, position = position_stack(vjust = 0.5))+
  labs(x = NULL, y = NULL, fill = "Gelatinisation Temperature")+
  theme(axis.line = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5))+
  theme(legend.key = element_rect(size = rel(1.4)), 
        legend.title = element_text(size = rel(2)),legend.text = element_text(size = rel(1.4)))


