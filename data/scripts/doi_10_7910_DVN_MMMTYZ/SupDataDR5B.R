####TITLE: Fossilization potential of marine assemblages and environments
####AUTHOR: Jack O. Shaw
####PERMANENT EMAIL: jackolivershaw@gmail.com

############INSTRUCTIONS

#This code generates the substrate data given in SupDataDR5C.RDS and sourced in SupDataDR4B.R

############INSTRUCTIONS


####Map theme----
theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Ubuntu Regular", color = "#22211d"),
      # remove all axes
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # add a subtle grid
      panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#f5f5f2", color = NA), 
      plot.margin = unit(c(.5, .5, .2, .5), "cm"),
      panel.border = element_blank(),
      panel.background = element_rect(fill = "#f5f5f2", color = NA), 
      panel.spacing = unit(c(-.1, 0.2, .2, 0.2), "cm"),
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      legend.title = element_text(size = 13),
      legend.text = element_text(size = 11, hjust = 0, color = "#4e4d47"),
      plot.title = element_text(size = 16, hjust = 0.5, color = "#4e4d47"),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "#4e4d47", 
                                   margin = margin(b = -0.1, 
                                                   t = -0.1, 
                                                   l = 2, 
                                                   unit = "cm"), 
                                   debug = F),
      plot.caption = element_text(size = 9, 
                                  hjust = .5, 
                                  margin = margin(t = 0.2, 
                                                  b = 0, 
                                                  unit = "cm"), 
                                  color = "#939184"),
      ...
    )
}


####Training data----
library(rsample)
dbs<-read.csv("S5A_dbs_data_from_shapefile.csv")

##Set up data
Data<-dbs[,c("lat","lon","substrate")]
#Data<-sample_n(Data,5000)
#Data<-Data[complete.cases(Data), ]
Data<-subset(Data,substrate!="")
Data[,3]<-(as.factor(Data[,3]))
Data<-Data %>% mutate_all(na_if,"")

c1<-subset(Data, lon<=(-170))
c1$lon<-c1$lon+360
c2<-subset(Data, lon>=(170))
c2$lon<-c2$lon-360
Data<-rbind(Data,c1,c2)

sampn<-sample(1:nrow(Data),(0.8*nrow(Data)))
Data_train<-Data[sampn,]
Data_test<-Data[-sampn,]

##Indicate best kernal to use
train_dialects_kknn <- kknn::train.kknn(substrate ~ ., 
                                        data= Data_train, 
                                        kernel =  c("rectangular", "triangular", "epanechnikov", "gaussian",
                                                    "rank", "optimal"),kmax=20)
plot(train_dialects_kknn)
#Best parameters
b2<-which(train_dialects_kknn$MISCLASS == min(train_dialects_kknn$MISCLASS, na.rm = TRUE), arr.ind = TRUE)
b3<-c(rownames(train_dialects_kknn$MISCLASS)[(b2[1])],colnames(train_dialects_kknn$MISCLASS)[b2[2]])
print(b3)

saveRDS(train_dialects_kknn,"SubstrateInterpolationModel.RDS")