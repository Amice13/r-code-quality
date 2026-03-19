library(reshape2)
library(ggplot2)
library(scales)

#load("~/Dropbox/PDparty/rcode/replication/merged-2020-n2471-01132021.Rdata")
setwd("~/Dropbox/PDparty/rcode/replication/")
load("merged-2020-n2471-replication.Rdata")
### only Black Americans

dta22 <- dta22sub
rm(dta22sub)
dta23 <- dta22[dta22$BLACK6==1,]

lout <- lm(I(PID13-PID10) ~ INCOME1+EDYEARS6,data=dta23)

dta.sub <- dta23[! dta23$PID10 %in% c(NA) &
                   ! dta23$PID13 %in% c(NA),]

tt10 <- table(dta.sub$TRUMPVC10B,dta.sub$GOPREPVD18B)
tt20 <- tt10/sum(tt10)
sum(diag(tt20))
#[1] 83

tt1 <- table(dta23$PID10,dta23$PID13)
tt3 <- tt1/sum(tt1)
sum(diag(tt3))

1-0.01204819 -0.01204819-0.01204819

sum(tt1[4,])/sum(tt1)
sum(tt1[,4])/sum(tt1)

sum(apply(tt1[3:5,],1,sum))/sum(tt1)
sum(apply(tt1[,3:5],2,sum))/sum(tt1)

sum(tt1)
sum(diag(tt1))
sum(diag(tt1))/sum(tt1)
#[1] 0.7228916

t1p <- table(dta23$PID13-dta23$PID10)
tt2a <- round(prop.table(tt1),digits=3)
tt2 <- cbind(tt2a[,1:4],rep(0,7),tt2a[,5:6])

colnames(tt2) <- rownames(tt2) <- c("SD","WD","ID","I","IR","WR","SR")

co=melt(tt2)
head(co)
colnames(co) <- c("Wave1","Wave3","value")

#pdf("partisan-transition-black-06152022.pdf")

ggplot(co, aes(Wave1, Wave3)) + # x and y axes => Var1 and Var2
  geom_tile(aes(fill = value)) + # background colours are mapped according to the value column
  #theme_gray(base_size=24) +
  geom_text(aes(fill = value, label = round(value, 3)),size=4) + # write the values
  scale_fill_gradient2(low = "#F2F2F2", 
                       #mid = "#B7B7B7", 
                       high = "#7F7F7F")+#, 
  #midpoint = mean(co$value)) + # determine the colour
  #scale_colour_grey()+theme_bw()+
  theme(panel.grid.major.x=element_blank(), #no gridlines
        panel.grid.minor.x=element_blank(), 
        panel.grid.major.y=element_blank(), 
        panel.grid.minor.y=element_blank(),
        panel.background=element_rect(fill="white"), # background=white
        axis.text.x = element_text(angle=60, hjust = 1,vjust=1,size = 16,face = "bold"),
        axis.title.x = element_text(size=18,face="bold"),
        plot.title = element_text(size=20,face="bold"),
        axis.text.y = element_text(size = 16,face = "bold"),
        axis.title.y = element_text(size=18,face="bold"))+
  ggtitle("Partisan Transitions: Black Americans") + 
  theme(legend.title=element_text(face="bold", size=12)) + 
  scale_x_discrete(name="2016") +
  scale_y_discrete(name="2018") +
  labs(fill="Share of Black Americans")
#dev.off()

#####

dta23 <- dta22[dta22$WHITE6==1,]

lout <- lm(I(PID13-PID10) ~ INCOME1+EDYEARS6,data=dta23)

dta.sub <- dta23[! dta23$PID10 %in% c(NA) &
                   ! dta23$PID13 %in% c(NA),]

tt10 <- table(dta.sub$TRUMPVC10B,dta.sub$GOPREPVD18B)
tt20 <- tt10/sum(tt10)
sum(diag(tt20))


tt1 <- table(dta23$PID10,dta23$PID13)
tt3 <- tt1/sum(tt1)

sum(tt1[4,])/sum(tt1)
sum(tt1[,4])/sum(tt1)

sum(apply(tt1[3:5,],1,sum))/sum(tt1)
sum(apply(tt1[,3:5],2,sum))/sum(tt1)


sum(tt1)
sum(diag(tt1))
sum(diag(tt1))/sum(tt1)
#[1] 0.7145791
#[1] 487

t1p <- table(dta23$PID13-dta23$PID10)
(49+348+46)/sum(t1p)

tt2 <- round(prop.table(tt1),digits=3)
colnames(tt2) <- rownames(tt2) <- c("SD","WD","ID","I","IR","WR","SR")

co=melt(tt2)
head(co)
colnames(co) <- c("Wave1","Wave3","value")

#pdf("/users/danhop/Dropbox/PDparty/figures/partisan-transition-white-06152022.pdf")

ggplot(co, aes(Wave1, Wave3)) + # x and y axes => Var1 and Var2
  geom_tile(aes(fill = value)) + # background colours are mapped according to the value column
  #theme_gray(base_size=24) +
  geom_text(aes(fill = value, label = round(value, 3)),size=4) + # write the values
  scale_fill_gradient2(low = "#F2F2F2", 
                       #mid = "#B7B7B7", 
                       high = "#7F7F7F")+#, 
  #midpoint = mean(co$value)) + # determine the colour
  #scale_colour_grey()+theme_bw()+
  theme(panel.grid.major.x=element_blank(), #no gridlines
        panel.grid.minor.x=element_blank(), 
        panel.grid.major.y=element_blank(), 
        panel.grid.minor.y=element_blank(),
        panel.background=element_rect(fill="white"), # background=white
        axis.text.x = element_text(angle=60, hjust = 1,vjust=1,size = 16,face = "bold"),
        axis.title.x = element_text(size=18,face="bold"),
        plot.title = element_text(size=20,face="bold"),
        axis.text.y = element_text(size = 16,face = "bold"),
        axis.title.y = element_text(size=18,face="bold"))+
  ggtitle("Partisan Transitions: White Am.") + 
  theme(legend.title=element_text(face="bold", size=12)) + 
  scale_x_discrete(name="2016") +
  scale_y_discrete(name="2018") +
  labs(fill="Share of White Americans")


dev.off()