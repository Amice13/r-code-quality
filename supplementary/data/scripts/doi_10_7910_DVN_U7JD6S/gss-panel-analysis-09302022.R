library(readstata13)
library(reshape2)
library(ggplot2)
library(scales)

#### must download GSS 2020 panel gss2020panel_r1a.dta
setwd("/users/danhop/Dropbox/PDparty/rcode/replication/")
dta <- read.dta13("gss2020panel_r1a.dta",generate.factors=TRUE)

dta$PARTYID1A <- NA
dta$PARTYID1A[dta$partyid_1a=="strong democrat"] <- 1
dta$PARTYID1A[dta$partyid_1a=="not very strong democrat"] <- 2
dta$PARTYID1A[dta$partyid_1a=="independent, close to democrat"] <- 3
dta$PARTYID1A[dta$partyid_1a=="independent (neither, no response)"] <- 4
dta$PARTYID1A[dta$partyid_1a=="independent, close to republican"] <- 5
dta$PARTYID1A[dta$partyid_1a=="not very strong republican"] <- 6
dta$PARTYID1A[dta$partyid_1a=="strong republican"] <- 7
dta$PARTYID1A[dta$partyid_1a=="other party"] <- 4

dta$PARTYID1B <- NA
dta$PARTYID1B[dta$partyid_1b=="strong democrat"] <- 1
dta$PARTYID1B[dta$partyid_1b=="not very strong democrat"] <- 2
dta$PARTYID1B[dta$partyid_1b=="independent, close to democrat"] <- 3
dta$PARTYID1B[dta$partyid_1b=="independent (neither, no response)"] <- 4
dta$PARTYID1B[dta$partyid_1b=="independent, close to republican"] <- 5
dta$PARTYID1B[dta$partyid_1b=="not very strong republican"] <- 6
dta$PARTYID1B[dta$partyid_1b=="strong republican"] <- 7
dta$PARTYID1B[dta$partyid_1b=="other party"] <- 4

table(dta$PARTYID1B)

dta$PARTYID2 <- NA
dta$PARTYID2[dta$partyid_2=="strong democrat"] <- 1
dta$PARTYID2[dta$partyid_2=="not very strong democrat"] <- 2
dta$PARTYID2[dta$partyid_2=="independent, close to democrat"] <- 3
dta$PARTYID2[dta$partyid_2=="independent (neither, no response)"] <- 4
dta$PARTYID2[dta$partyid_2=="independent, close to republican"] <- 5
dta$PARTYID2[dta$partyid_2=="not very strong republican"] <- 6
dta$PARTYID2[dta$partyid_2=="strong republican"] <- 7
dta$PARTYID2[dta$partyid_2=="other party"] <- 4

dta.blk <- dta[dta$racecen1_1a=="black or african american" | dta$racecen1_1b=="black or african american",]
dta.his <- dta[dta$racecen1_1a=="hispanic" | dta$racecen1_1b=="hispanic",]
dta.wht <- dta[dta$racecen1_1a=="white" | dta$racecen1_1b=="white",]

##### 2018 - 2020
tt1 <- table(dta.wht$PARTYID1B,dta.wht$PARTYID2)
sum(tt1)
sum(diag(tt1))
sum(diag(tt1))/sum(tt1)
# 0.5492578

sum(tt1[4,])/sum(tt1)
sum(tt1[,4])/sum(tt1)

tt2 <- round(prop.table(tt1),digits=3)
colnames(tt2) <- rownames(tt2) <- c("SD","WD","ID","I","IR","WR","SR")

#tt2[3:7,1+]

#rownames(tt2) <- c("Dem 16","Neither 16","GOP 16")
#colnames(tt2) <- c("Dem 18","Neither 18","GOP 18")

co=melt(tt2)
head(co)
colnames(co) <- c("w2018","w2020","value")

sum(tt2[3:7,1])+sum(tt2[4:7,2])+sum(tt2[c(1,5:7),3])+
  sum(tt2[c(1:2,6:7),4])+sum(tt2[c(1:3,7),5])+
  sum(tt2[c(1:4),6])+sum(tt2[c(1:5),7])

pdf("gss-wht-partisan-transition-18-20-06152022.pdf")

ggplot(co, aes(w2018,w2020)) + # x and y axes => Var1 and Var2
  geom_tile(aes(fill = value)) + # background colours are mapped according to the value column
  geom_text(aes(fill = co$value, label = round(co$value, 3))) + # write the values
  scale_fill_gradient2(low = "#F2F2F2", 
                       #mid = "#B7B7B7", 
                       high = "#7F7F7F") + # determine the colour
  theme(panel.grid.major.x=element_blank(), #no gridlines
        panel.grid.minor.x=element_blank(), 
        panel.grid.major.y=element_blank(), 
        panel.grid.minor.y=element_blank(),
        panel.background=element_rect(fill="white"), # background=white
        axis.text.x = element_text(angle=90, hjust = 1,vjust=1,size = 12,face = "bold"),
        plot.title = element_text(size=20,face="bold"),
        axis.text.y = element_text(size = 12,face = "bold")) + 
  ggtitle("Partisan Transitions: White Respondents") + 
  theme(legend.title=element_text(face="bold", size=14)) + 
  scale_x_discrete(name="2016") +
  scale_y_discrete(name="2020") +
  labs(fill="Share of White Rs")
dev.off()

###### Black 2018-2020
##### 2018 - 2020
tt1 <- table(dta.blk$PARTYID1B,dta.blk$PARTYID2)
sum(tt1)
sum(diag(tt1))
sum(diag(tt1))/sum(tt1)
# 0.5492578

sum(tt1[4,])/sum(tt1)
sum(tt1[,4])/sum(tt1)

tt2 <- round(prop.table(tt1),digits=3)
colnames(tt2) <- rownames(tt2) <- c("SD","WD","ID","I","IR","WR","SR")

#rownames(tt2) <- c("Dem 16","Neither 16","GOP 16")
#colnames(tt2) <- c("Dem 18","Neither 18","GOP 18")

co=melt(tt2)
head(co)
colnames(co) <- c("w2018","w2020","value")

sum(tt2[3:7,1])+sum(tt2[4:7,2])+sum(tt2[c(1,5:7),3])+
  sum(tt2[c(1:2,6:7),4])+sum(tt2[c(1:3,7),5])+
  sum(tt2[c(1:4),6])+sum(tt2[c(1:5),7])

pdf("gss-blk-partisan-transition-18-20-06152022.pdf")

ggplot(co, aes(w2018,w2020)) + # x and y axes => Var1 and Var2
  geom_tile(aes(fill = value)) + # background colours are mapped according to the value column
  geom_text(aes(fill = co$value, label = round(co$value, 3))) + # write the values
  scale_fill_gradient2(low = "#F2F2F2", 
                       #mid = "#B7B7B7", 
                       high = "#7F7F7F") + # determine the colour
  theme(panel.grid.major.x=element_blank(), #no gridlines
        panel.grid.minor.x=element_blank(), 
        panel.grid.major.y=element_blank(), 
        panel.grid.minor.y=element_blank(),
        panel.background=element_rect(fill="white"), # background=white
        axis.text.x = element_text(angle=90, hjust = 1,vjust=1,size = 12,face = "bold"),
        plot.title = element_text(size=20,face="bold"),
        axis.text.y = element_text(size = 12,face = "bold")) + 
  ggtitle("Partisan Transitions: Black Respondents") + 
  theme(legend.title=element_text(face="bold", size=14)) + 
  scale_x_discrete(name="2016") +
  scale_y_discrete(name="2020") +
  labs(fill="Share of Black Rs")
dev.off()

#####
