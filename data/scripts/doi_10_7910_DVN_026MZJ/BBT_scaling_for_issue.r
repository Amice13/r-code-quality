

#0. set up
rm(list=ls())
getwd()
setwd("~/replication")


library(foreign)
library(basicspace)
library(xlsx)
library(MCMCpack)
library(foreign)
library(basicspace)
library(xlsx)
library(foreign)
library(xlsx)
library(pscl) 
library(basicspace)
library(foreign)
library(xlsx)
library(xlsxjars)
library(rJava)
library(rJavax)
library(foreign)
library(xlsx)
library(basicspace)
library(corrplot)
library(ggplot2)
library(grDevices)
library(basicspace)
library(MCMCpack)
library(gridExtra)
library(haven)

colvect <- c("#EFDECD","#CD9575","#FDD9B5","#78DBE2","#87A96B", "#FFA474", "#FAE7B5", "#9F8170", "#FD7C6E", "#000000", "#ACE5EE", "#1F75FE", "#A2A2D0", "#6699CC", "#0D98BA", "#7366BD", "#DE5D83", "#CB4154", "#B4674D", "#FF7F49", "#EA7E5D", "#B0B7C6", "#FFFF99", "#1CD3A2", "#FFAACC", "#DD4492", "#1DACD6", "#BC5D58", "#DD9475", "#9ACEEB", "#FFBCD9", "#FDDB6D", "#2B6CC4", "#EFCDB8", "#6E5160",  "#CEFF1D", "#71BC78", "#6DAE81", "#C364C5", "#CC6666", "#E7C697","#FCD975", "#A8E4A0", "#95918C", "#1CAC78", "#1164B4", "#F0E891","#FF1DCE", "#B2EC5D", "#5D76CB", "#CA3767", "#3BB08F", "#FEFE22","#FCB4D5", "#FFF44F", "#FFBD88", "#F664AF", "#AAF0D1", "#CD4A4C","#EDD19C", "#979AAA", "#FF8243", "#C8385A", "#EF98AA", "#FDBCB4","#1A4876", "#30BA8F", "#C54B8C", "#1974D2", "#FFA343", "#BAB86C","#FF7538", "#FF2B2B", "#F8D568", "#E6A8D7", "#414A4C", "#FF6E4A","#1CA9C9", "#FFCFAB", "#C5D0E6", "#FDDDE6", "#158078", "#FC74FD",  "#F78FA7", "#8E4585", "#7442C8", "#9D81BA", "#FE4EDA", "#FF496C","#D68A59", "#714B23", "#FF48D0", "#E3256B", "#EE204D", "#FF5349","#C0448F", "#1FCECB", "#7851A9", "#FF9BAA", "#FC2847", "#76FF7A","#9FE2BF", "#A5694F", "#8A795D", "#45CEA2", "#FB7EFD","#CDC5C2","#80DAEB", "#ECEABE", "#FFCF48", "#FD5E53", "#FAA76C","#18A7B5","#EBC7DF", "#FC89AC", "#DBD7D2", "#17806D", "#DEAA88", "#77DDE7","#FFFF66","#926EAE", "#324AB2","#F75394","#FFA089","#8F509D","#FFFFFF", "#A2ADD0","#FF43A4","#FC6C85","#CDA4DE","#FCE883","#C5E384","#FFAE42")
cray <- c("#EFDECD","#CD9575","#FDD9B5","#78DBE2","#87A96B","#FFA474","#FAE7B5","#9F8170","#FD7C6E","#000000","#ACE5EE","#1F75FE","#A2A2D0","#6699CC","#0D98BA","#7366BD","#DE5D83","#CB4154","#B4674D","#FF7F49","#EA7E5D","#B0B7C6","#FFFF99","#00CC99","#FFAACC","#DD4492","#1DACD6","#BC5D58","#DD9475","#9ACEEB","#FFBCD9","#FDDB6D","#2B6CC4","#EFCDB8","#6E5160","#CEFF1D","#71BC78","#6DAE81","#C364C5","#CC6666","#E7C697","#FCD975","#A8E4A0","#95918C","#1CAC78","#1164B4","#F0E891","#FF1DCE","#B2EC5D","#5D76CB","#CA3767","#3BB08F","#FEFE22","#FCB4D5","#FFF44F","#FFBD88","#F664AF","#AAF0D1","#CD4A4C","#EDD19C","#979AAA","#FF8243","#C8385A","#EF98AA","#FDBCB4","#1A4876","#30BA8F","#C54B8C","#1974D2","#FFA343","#BAB86C","#FF7538","#FF2B2B","#F8D568","#E6A8D7","#414A4C","#FF6E4A","#1CA9C9","#FFCFAB","#C5D0E6","#FDDDE6","#158078","#FC74FD","#F78FA7","#8E4585","#7442C8","#9D81BA","#FE4EDA","#FF496C","#D68A59","#714B23","#FF48D0","#E3256B","#EE204D","#FF5349","#C0448F","#1FCECB","#7851A9","#FF9BAA","#FC2847","#76FF7A","#93DFB8","#A5694F","#8A795D","#45CEA2","#FB7EFD","#CDC5C2","#80DAEB","#ECEABE","#FFCF48","#FD5E53","#FAA76C","#18A7B5","#EBC7DF","#FC89AC","#DBD7D2","#17806D","#DEAA88","#77DDE7","#FFFF66","#926EAE","#324AB2","#F75394","#FFA089","#8F509D","#FFFFFF","#A2ADD0","#FF43A4","#FC6C85","#CDA4DE","#FCE883","#C5E384","#FFAE42")
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#pie(rep(1, 30), col = rainbow(30))
#pie(rep(1, 30), col = gray(1:30 / 30))
#pie(rep(1, 3), col = gray(1:4 / 4))






Data<-read.dta("expertpanel_indiv2.dta")


table(Data$year)

Data_2017<-subset(Data,year==2017)




# 4 _ev

Data_2017_ev<-Data_2017[,c("cdp_p_ev", "cgp_p_ev", "hope_p_ev", "jcp_p_ev", "jip_p_ev", "ldp_p_ev",
                           "minshin_p_ev","sdp_p_ev","self_p_ev")]
colnames(Data_2017_ev)<-c("CDP", "KOMEI", "HOPE", "JCP", "JIP", "LDP",
                          "MINSHIN","SDP","SELF")

Data_2017_ev<-Data_2017_ev[,-9]

result <- blackbox_transpose(Data_2017_ev,dims=3,minscale=4,verbose=TRUE)
plot(result)
summary(result)

result2 <- result$stimuli[[2]]


pdf(file="Blackbox_Transpose_Scaling_Party_Stimuli_1D_2D_environment.pdf",width=4.5,height=4.5)
scatter3b <- ggplot(result2, aes(-coord1D, -coord2D
                                ,label=rownames(result2)
)) + 
  geom_point(colour = "black", size = 1.5) +theme_bw() + 
  #facet_wrap(~country_year, ncol=3)+ 
  guides(col = guide_legend(ncol =5)) +
  #scale_fill_manual(values=colvect)+ 
  scale_shape_identity()+
  xlim(-1,1)+ ylim(-1,1)+
  ggtitle("Environment")+
  xlab("Party Stimuli (1D) Left - Right")+ylab("Party Stimuli (2D) Status Quo - Reform (Orientation)")
#scatter3
scatter1<-scatter3b+geom_text(aes(-coord1D+c(0,0,0,0,-0.025,0,0,0), -coord2D-0.05,label=rownames(result2)), size = 3)
scatter1
dev.off()
rm(scatter4)



pdf(file="Blackbox_Transpose_Scaling_Party_Stimuli_1D_2D_the_first_a_graph.pdf",width=4.5,height=4.5)

grid.arrange(scatter1)
dev.off()


# 5 _fr

Data_2017_fr<-Data_2017[,c("cdp_p_fr", "cgp_p_fr", "hope_p_fr", "jcp_p_fr", "jip_p_fr", "ldp_p_fr",
                           "minshin_p_fr","sdp_p_fr","self_p_fr")]
colnames(Data_2017_fr)<-c("CDP", "KOMEI", "HOPE", "JCP", "JIP", "LDP",
                          "MINSHIN","SDP","SELF")
Data_2017_fr<-Data_2017_fr[,-9]

result <- blackbox_transpose(Data_2017_fr,dims=3,minscale=4,verbose=TRUE)
plot(result)
summary(result)

result2 <- result$stimuli[[2]]


pdf(file="Blackbox_Transpose_Scaling_Party_Stimuli_1D_2D_foreign.pdf",width=4.5,height=4.5)
scatter3b <- ggplot(result2, aes(-coord1D, coord2D
                                ,label=rownames(result2)
)) + 
  geom_point(colour = "black", size = 1.5) +theme_bw() + 
  #facet_wrap(~country_year, ncol=3)+ 
  guides(col = guide_legend(ncol =2.5)) +
  #scale_fill_manual(values=colvect)+ 
  scale_shape_identity()+
  xlim(-1,1)+ ylim(-1,1)+
  ggtitle("US Affairs")+
  xlab("Party Stimuli (1D) Left - Right")+ylab("Party Stimuli (2D) Status Quo - Reform (Orientation)")
#scatter3
scatter2<-scatter3b+geom_text(aes(-coord1D+c(0,0,-0.03,0,0.05,0,0,0), coord2D-0.05,label=rownames(result2)), size = 3)
scatter2
dev.off()
rm(scatter4)




Data_2017_ig<-Data_2017[,c("cdp_p_ig", "cgp_p_ig", "hope_p_ig", "jcp_p_ig", "jip_p_ig", "ldp_p_ig",
                           "minshin_p_ig","sdp_p_ig","self_p_ig")]
colnames(Data_2017_ig)<-c("CDP", "KOMEI", "HOPE", "JCP", "JIP", "LDP",
                          "MINSHIN","SDP","SELF")






# 3 _df


Data_2017_df<-Data_2017[,c("cdp_p_df", "cgp_p_df", "hope_p_df", "jcp_p_df", "jip_p_df", "ldp_p_df",
                           "minshin_p_df","sdp_p_df","self_p_df")]

colnames(Data_2017_df)<-c("CDP", "KOMEI", "HOPE", "JCP", "JIP", "LDP",
                          "MINSHIN","SDP","SELF")
Data_2017_df<-Data_2017_df[,-9]

result <- blackbox_transpose(Data_2017_df,dims=3,minscale=4,verbose=TRUE)
plot(result)
summary(result)

result2 <- result$stimuli[[2]]


pdf(file="Blackbox_Transpose_Scaling_Party_Stimuli_1D_2D_defense.pdf",width=4.5,height=4.5)
scatter3b <- ggplot(result2, aes(-coord1D, coord2D
                                ,label=rownames(result2)
)) + 
  geom_point(colour = "black", size = 1.5) +theme_bw() + 
  #facet_wrap(~country_year, ncol=3)+ 
  guides(col = guide_legend(ncol =2.5)) +
  #scale_fill_manual(values=colvect)+ 
  scale_shape_identity()+
  xlim(-1,1)+ ylim(-1,1)+
  ggtitle("Defense")+
  xlab("Party Stimuli (1D) Left - Right")+ylab("Party Stimuli (2D) Status Quo - Reform (Orientation)")
#scatter3
scatter3<-scatter3b+geom_text(aes(-coord1D+c(0,0,-0.05,-0.05,0.05,0.075,0.06,0.075), coord2D-0.05,label=rownames(result2)), size = 3)
scatter3
dev.off()
rm(scatter4)




pdf(file="Blackbox_Transpose_Scaling_Party_Stimuli_1D_2D_the_first_b_graph.pdf",width=9,height=4.5)

grid.arrange(scatter2,scatter3,
             ncol = 2)
dev.off()




# 8 _rg

Data_2017_rg<-Data_2017[,c("cdp_p_rg", "cgp_p_rg", "hope_p_rg", "jcp_p_rg", "jip_p_rg", "ldp_p_rg",
                           "minshin_p_rg","sdp_p_rg","self_p_rg")]
colnames(Data_2017_rg)<-c("CDP", "KOMEI", "HOPE", "JCP", "JIP", "LDP",
                          "MINSHIN","SDP","SELF")
Data_2017_rg<-Data_2017_rg[,-9]

result <- blackbox_transpose(Data_2017_rg,dims=3,minscale=4,verbose=TRUE)
plot(result)
summary(result)

result2 <- result$stimuli[[2]]


pdf(file="Blackbox_Transpose_Scaling_Party_Stimuli_1D_2D_regulation.pdf",width=4.5,height=4.5)
scatter3b <- ggplot(result2, aes(-coord1D, -coord2D
                                ,label=rownames(result2)
)) + 
  geom_point(colour = "black", size = 1.5) +theme_bw() + 
  #facet_wrap(~country_year, ncol=3)+ 
  guides(col = guide_legend(ncol =2.5)) +
  #scale_fill_manual(values=colvect)+ 
  scale_shape_identity()+
  xlim(-1,1)+ ylim(-1,1)+
  ggtitle("Deregulation")+
  xlab("Party Stimuli (1D) Left - Right")+ylab("Party Stimuli (2D) Status Quo - Reform (Orientation)")
#scatter3
scatter4<-scatter3b+geom_text(aes(-coord1D+c(0,0,0,0,0,0,0,0.075), -coord2D-0.05,label=rownames(result2)), size = 3)
scatter4
dev.off()
#rm(scatter4)



# 2 dc

Data_2017_dc<-Data_2017[,c("cdp_p_dc", "cgp_p_dc", "hope_p_dc", "jcp_p_dc", "jip_p_dc", "ldp_p_dc",
                           "minshin_p_dc","sdp_p_dc","self_p_dc")]
colnames(Data_2017_dc)<-c("CDP", "KOMEI", "HOPE", "JCP", "JIP", "LDP",
                          "MINSHIN","SDP","SELF")

Data_2017_dc<-Data_2017_dc[,-9]

result <- blackbox_transpose(Data_2017_dc,dims=3,minscale=4,verbose=TRUE)
plot(result)
summary(result)

result2 <- result$stimuli[[2]]


pdf(file="Blackbox_Transpose_Scaling_Party_Stimuli_1D_2D_decentralization.pdf",width=4.5,height=4.5)
scatter3b <- ggplot(result2, aes(coord1D, coord2D
                                ,label=rownames(result2)
)) + 
  geom_point(colour = "black", size = 1.5) +theme_bw() + 
  #facet_wrap(~country_year, ncol=3)+ 
  guides(col = guide_legend(ncol =2.5)) +
  #scale_fill_manual(values=colvect)+ 
  scale_shape_identity()+
  xlim(-1,1)+ ylim(-1,1)+
  ggtitle("Decentralization")+
  xlab("Party Stimuli (1D) Left - Right")+ylab("Party Stimuli (2D) Status Quo - Reform (Orientation)")
#scatter3
scatter5<-scatter3b+geom_text(aes(coord1D, coord2D-0.05,label=rownames(result2)), size = 3)
scatter5
dev.off()
#rm(scatter4)


pdf(file="Blackbox_Transpose_Scaling_Party_Stimuli_1D_2D_the_first_c_graph.pdf",width=9,height=4.5)

grid.arrange(scatter4,scatter5,
             ncol = 2)
dev.off()




pdf(file="Blackbox_Transpose_Scaling_Party_Stimuli_1D_2D_the_first_graph.pdf",width=13.5,height=9)

grid.arrange(scatter1, scatter2,scatter3,scatter4,scatter5,
             ncol = 3)
dev.off()






###################################################################################################





# 9 _sc

Data_2017_sc<-Data_2017[,c("cdp_p_sc", "cgp_p_sc", "hope_p_sc", "jcp_p_sc", "jip_p_sc", "ldp_p_sc",
                           "minshin_p_sc","sdp_p_sc","self_p_sc")]
colnames(Data_2017_sc)<-c("CDP", "KOMEI", "HOPE", "JCP", "JIP", "LDP",
                          "MINSHIN","SDP","SELF")
Data_2017_sc<-Data_2017_sc[,-9]

result <- blackbox_transpose(Data_2017_sc,dims=3,minscale=4,verbose=TRUE)
plot(result)
summary(result)

result2 <- result$stimuli[[2]]


pdf(file="Blackbox_Transpose_Scaling_Party_Stimuli_1D_2D_Social.pdf",width=4.5,height=4.5)
scatter3 <- ggplot(result2, aes(coord1D, -coord2D
                                ,label=rownames(result2)
)) + 
  geom_point(colour = "black", size = 1.5) +theme_bw() + 
  #facet_wrap(~country_year, ncol=3)+ 
  guides(col = guide_legend(ncol =2.5)) +
  #scale_fill_manual(values=colvect)+ 
  scale_shape_identity()+
  xlim(-1,1)+ ylim(-1,1)+
  ggtitle("Social")+
  xlab("Party Stimuli (1D) Left - Right")+ylab("Party Stimuli (2D) Status Quo - Reform (Orientation)")
#scatter3
scatter6<-scatter3+geom_text(aes(coord1D, -coord2D-0.05,label=rownames(result2)), size = 3)
scatter6
dev.off()
rm(scatter4)

# 6 _ig

Data_2017_ig<-Data_2017[,c("cdp_p_ig", "cgp_p_ig", "hope_p_ig", "jcp_p_ig", "jip_p_ig", "ldp_p_ig",
                           "minshin_p_ig","sdp_p_ig","self_p_ig")]
colnames(Data_2017_ig)<-c("CDP", "KOMEI", "HOPE", "JCP", "JIP", "LDP",
                          "MINSHIN","SDP","SELF")
Data_2017_ig<-Data_2017_ig[,-9]
Data_2017_ig<-as.matrix(Data_2017_ig)
mode(Data_2017_ig)<-"double"

result <- blackbox_transpose(Data_2017_ig,dims=3,minscale=4,verbose=TRUE)
plot(result)
summary(result)

result2 <- result$stimuli[[2]]


pdf(file="Blackbox_Transpose_Scaling_Party_Stimuli_1D_2D_immigration.pdf",width=4.5,height=4.5)
scatter3 <- ggplot(result2, aes(-coord1D, -coord2D
                                ,label=rownames(result2)
)) + 
  geom_point(colour = "black", size = 1.5) +theme_bw() + 
  #facet_wrap(~country_year, ncol=3)+ 
  guides(col = guide_legend(ncol =2.5)) +
  #scale_fill_manual(values=colvect)+ 
  scale_shape_identity()+
  xlim(-1,1)+ ylim(-1,1)+
  ggtitle("Immigration")+
  xlab("Party Stimuli (1D) Left - Right")+ylab("Party Stimuli (2D) Status Quo - Reform (Orientation)")
#scatter3
scatter7<-scatter3+geom_text(aes(-coord1D+c(-0.05,0,0,0,0,0,0.02,0), -coord2D-0.05,label=rownames(result2)), size = 3)
scatter7
dev.off()
rm(scatter4)


pdf(file="Blackbox_Transpose_Scaling_Party_Stimuli_1D_2D_the_second_a_graph.pdf",width=9,height=4.5)

grid.arrange(scatter6, scatter7,
             ncol = 2)
dev.off()


# 7 _ni

Data_2017_ni<-Data_2017[,c("cdp_p_ni", "cgp_p_ni", "hope_p_ni", "jcp_p_ni", "jip_p_ni", "ldp_p_ni",
                           "minshin_p_ni","sdp_p_ni","self_p_ni")]
colnames(Data_2017_ni)<-c("CDP", "KOMEI", "HOPE", "JCP", "JIP", "LDP",
                          "MINSHIN","SDP","SELF")
Data_2017_ni<-Data_2017_ni[,-9]

result <- blackbox_transpose(Data_2017_ni,dims=3,minscale=4,verbose=TRUE)
plot(result)
summary(result)

result2 <- result$stimuli[[2]]


pdf(file="Blackbox_Transpose_Scaling_Party_Stimuli_1D_2D_identity.pdf",width=4.5,height=4.5)
scatter3 <- ggplot(result2, aes(-coord1D, coord2D
                                ,label=rownames(result2)
)) + 
  geom_point(colour = "black", size = 1.5) +theme_bw() + 
  #facet_wrap(~country_year, ncol=3)+ 
  guides(col = guide_legend(ncol =2.5)) +
  #scale_fill_manual(values=colvect)+ 
  scale_shape_identity()+
  xlim(-1,1)+ ylim(-1,1)+
  ggtitle("National Identity")+
  xlab("Party Stimuli (1D) Left - Right")+ylab("Party Stimuli (2D) Status Quo - Reform (Orientation)")
#scatter3
scatter8<-scatter3+geom_text(aes(-coord1D+c(0,0,-0.075,0,0.05,0,0,0), coord2D+c(-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05),label=rownames(result2)), size = 3)
scatter8
dev.off()
rm(scatter4)


pdf(file="Blackbox_Transpose_Scaling_Party_Stimuli_1D_2D_the_second_b_graph.pdf",width=4.5,height=4.5)

grid.arrange(scatter8)
dev.off()


pdf(file="Blackbox_Transpose_Scaling_Party_Stimuli_1D_2D_the_second_graph.pdf",width=9,height=9)

grid.arrange(scatter6, scatter7,scatter8,
             ncol = 2)
dev.off()




###################################################################################################

# 10 _tx



Data_2017_tx<-Data_2017[,c("cdp_p_tx", "cgp_p_tx", "hope_p_tx", "jcp_p_tx", "jip_p_tx", "ldp_p_tx",
                           "minshin_p_tx","sdp_p_tx","self_p_tx")]
colnames(Data_2017_tx)<-c("CDP", "KOMEI", "HOPE", "JCP", "JIP", "LDP",
                          "MINSHIN","SDP","SELF")
Data_2017_tx<-Data_2017_tx[,-9]

result <- blackbox_transpose(Data_2017_tx,dims=3,minscale=4,verbose=TRUE)
#plot(result)
summary(result)

result2 <- result$stimuli[[2]]


pdf(file="Blackbox_Transpose_Scaling_Party_Stimuli_1D_2D_tax.pdf",width=4.5,height=4.5)
scatter3 <- ggplot(result2, aes(-coord1D, coord2D
                                ,label=rownames(result2)
)) + 
  geom_point(colour = "black", size = 1.5) +theme_bw() + 
  #facet_wrap(~country_year, ncol=3)+ 
  guides(col = guide_legend(ncol =2.5)) +
  #scale_fill_manual(values=colvect)+ 
  scale_shape_identity()+
  xlim(-1,1)+ ylim(-1,1)+
  ggtitle("Spending vs. Taxes")+
  xlab("Party Stimuli (1D) Left - Right")+ylab("Party Stimuli (2D) Status Quo - Reform (Orientation)")
#scatter3
scatter9<-scatter3+geom_text(aes(-coord1D+c(0,0,0,0,0,0,0.02,0.02), coord2D-0.05,label=rownames(result2)), size = 3)
scatter9
dev.off()
rm(scatter4)




# 1 bd



Data_2017_bd<-Data_2017[,c("cdp_p_bd", "cgp_p_bd", "hope_p_bd", "jcp_p_bd", "jip_p_bd", "ldp_p_bd",
                           "minshin_p_bd","sdp_p_bd","self_p_bd")]
colnames(Data_2017_bd)<-c("CDP", "KOMEI", "HOPE", "JCP", "JIP", "LDP",
                          "MINSHIN","SDP","SELF")
Data_2017_bd<-Data_2017_bd[,-9]
Data_2017_bd<-as.matrix(Data_2017_bd)
mode(Data_2017_bd)<-"double"

result <- blackbox_transpose(Data_2017_bd,dims=3,minscale=4,verbose=TRUE)
#plot(result)
summary(result)

result2 <- result$stimuli[[2]]


pdf(file="Blackbox_Transpose_Scaling_Party_Stimuli_1D_2D_budget.pdf",width=4.5,height=4.5)
scatter3 <- ggplot(result2, aes(-coord1D, -coord2D
                                ,label=rownames(result2)
)) + 
  geom_point(colour = "black", size = 1.5) +theme_bw() + 
  #facet_wrap(~country_year, ncol=3)+ 
  guides(col = guide_legend(ncol =2.5)) +
  #scale_fill_manual(values=colvect)+ 
  scale_shape_identity()+
  xlim(-1,1)+ ylim(-1,1)+
  ggtitle("Deficit Bonds")+
  xlab("Party Stimuli (1D) Left - Right")+ylab("Party Stimuli (2D) Status Quo - Reform (Orientation)")
#scatter3
scatter10<-scatter3+geom_text(aes(-coord1D+c(-0.025,0,0,-0.03,0,0,0.05,0.03), -coord2D-0.05,label=rownames(result2)), size = 3)
scatter10
dev.off()

pdf(file="Blackbox_Transpose_Scaling_Party_Stimuli_1D_2D_the_third_graph.pdf",width=9,height=4.5)

grid.arrange(scatter9, scatter10,
             ncol = 2)
dev.off()



