setwd("")
library(ggplot2)
library(readstata13)

a <- read.dta13("paralleltrends_forR.dta",convert.factors=F)

############################

myvalues <- function(clasch,lastclass){
  myvals <- c(mean(a$ideol_ch[a$class_ch==clasch & a$lastclass==lastclass],na.rm=T),
              sd(a$ideol_ch[a$class_ch==clasch & a$lastclass==lastclass],na.rm=T),
              length(a$ideol_ch[a$class_ch==clasch & a$lastclass==lastclass & !is.na(a$ideol_ch)])
              )
  return(myvals)
}

dataset <- as.data.frame(rbind(
  c("Working Class", myvalues(3,1), "Upward Class Mobility"),  #rh panel
  c("Working Class", myvalues(2,1), "Upward Class Mobility"),
  c("Working Class", myvalues(1,1), "Upward Class Mobility"),  
  c("Routine Non-Manual",myvalues(2,2), "Upward Class Mobility"),
  c("Routine Non-Manual",myvalues(1,2), "Upward Class Mobility"), 
  c("Lower Service",myvalues(1,3), "Upward Class Mobility"),  
  
  c("Working Class",myvalues(0,1), "No Change"),  # middle panel
  c("Routine Non-Manual",myvalues(0,2), "No Change"),
  c("Lower Service",myvalues(0,3), "No Change"),
  c("Upper Service",myvalues(0,4), "No Change"),
  
  c("Routine Non-Manual",myvalues(-1,2), "Downward Class Mobility"),  # lh panel
  c("Lower Service",myvalues(-2,3), "Downward Class Mobility"),  
  c("Lower Service",myvalues(-1,3), "Downward Class Mobility"),
  c("Upper Service",myvalues(-3,4), "Downward Class Mobility"),
  c("Upper Service",myvalues(-2,4), "Downward Class Mobility"), 
  c("Upper Service",myvalues(-1,4), "Downward Class Mobility")
))

colnames(dataset)=c("startpoint","change","sd","n","type")

dataset$startpoint1 <- c(4,4,4,3,3,2,4,3,2,1,3,2,2,1,1,1)
dataset$amount <- factor(c(3,2,1,2,1,1,0,0,0,0,1,2,1,3,2,1))  #will now be 2,1or 0 change
dataset$change <- as.numeric(as.character(dataset$change)) 
dataset$sd <- as.numeric(as.character(dataset$sd)) 
dataset$n <- as.numeric(as.character(dataset$n)) 

dataset$se <- dataset$sd/sqrt(dataset$n)
dataset$upper <- dataset$change + (1.96*dataset$se)
dataset$lower <- dataset$change - (1.96*dataset$se)

png(file="figurea1.jpg",units="in",height=5,width=8,res=500)
ggplot(data = dataset, aes(x = factor(startpoint1), y = change, ymin = lower, 
                           ymax = upper, colour = amount, shape=amount)) +
  facet_wrap(~type) +
  geom_point(position = position_dodge(width = 0.3),size=2) +
  geom_errorbar(position = position_dodge(width = 0.3), width = 0.1) +
  coord_flip() +
  scale_x_discrete(name="Starting Class",labels=c("Upper\n Service","Lower\n Service","Routine\n Non-Manual","Working\n Class")) + 
  scale_y_continuous(name="Ideological Change") +
  geom_hline(aes(yintercept=0),size=0.5) +
  theme(axis.text.x = element_text(size =9,colour="black"))+
  theme(axis.text.y = element_text(size = 8,colour="black")) +
  scale_colour_discrete(name  ="Extent of\n Mobility",
                        labels=c("No Change","One Class","Two Classes","Three Classes")) +
  scale_shape_discrete(name  ="Extent of\n Mobility",
                       labels=c("No Change","One Class","Two Classes","Three Classes")) +
  theme(legend.title=element_text(size=8)) +
  theme(axis.title=element_text(size=9))
dev.off()

