


#0. set up
rm(list=ls())
#getwd()
#setwd()


library(foreign)
library(basicspace)
library(xlsx)
library(MCMCpack)
library(haven)

Data<-read.dta("expertpanel_indiv2.dta")

table(Data$year)

Data_2017<-subset(Data,year==2017)





Data_2017_bd<-Data_2017[,c("cdp_i_bd", "cgp_i_bd", "hope_i_bd", "jcp_i_bd", "jip_i_bd", "ldp_i_bd",
                           "minshin_i_bd","sdp_i_bd","self_i_bd")]
colnames(Data_2017_bd)<-c("cdp", "cgp", "hope", "jcp", "jip", "ldp",
                          "minshin","sdp","self")
Data_2017_bd<-as.matrix(Data_2017_bd)
mode(Data_2017_bd)<-"double"
result <- aldmck(data=Data_2017_bd, polarity=3, respondent=9,verbose=TRUE)
summary(result)
pdf(file="Data_2017_bd_i.pdf",width=12,height=9)
plot.aldmck(result)
dev.off()



Data_2017_dc<-Data_2017[,c("cdp_i_dc", "cgp_i_dc", "hope_i_dc", "jcp_i_dc", "jip_i_dc", "ldp_i_dc",
                           "minshin_i_dc","sdp_i_dc","self_i_dc")]
colnames(Data_2017_dc)<-c("cdp", "cgp", "hope", "jcp", "jip", "ldp",
                          "minshin","sdp","self")
result <- aldmck(data=Data_2017_dc, polarity=3, respondent=9,verbose=TRUE)
summary(result)

pdf(file="Data_2017_dc_i.pdf",width=12,height=9)
plot.aldmck(result)
dev.off()







Data_2017_df<-Data_2017[,c("cdp_i_df", "cgp_i_df", "hope_i_df", "jcp_i_df", "jip_i_df", "ldp_i_df",
                           "minshin_i_df","sdp_i_df","self_i_df")]

colnames(Data_2017_df)<-c("cdp", "cgp", "hope", "jcp", "jip", "ldp",
                          "minshin","sdp","self")
result <- aldmck(data=Data_2017_df, polarity=4, respondent=9,verbose=TRUE)
summary(result)
pdf(file="Data_2017_df_i.pdf",width=12,height=9)
plot.aldmck(result)
dev.off()



Data_2017_ev<-Data_2017[,c("cdp_i_ev", "cgp_i_ev", "hope_i_ev", "jcp_i_ev", "jip_i_ev", "ldp_i_ev",
                           "minshin_i_ev","sdp_i_ev","self_i_ev")]
colnames(Data_2017_ev)<-c("cdp", "cgp", "hope", "jcp", "jip", "ldp",
                          "minshin","sdp","self")
result <- aldmck(data=Data_2017_ev, polarity=4, respondent=9,verbose=TRUE)
summary(result)
pdf(file="Data_2017_ev_i.pdf",width=12,height=9)
plot.aldmck(result)
dev.off()



Data_2017_fr<-Data_2017[,c("cdp_i_fr", "cgp_i_fr", "hope_i_fr", "jcp_i_fr", "jip_i_fr", "ldp_i_fr",
                           "minshin_i_fr","sdp_i_fr","self_i_fr")]
colnames(Data_2017_fr)<-c("cdp", "cgp", "hope", "jcp", "jip", "ldp",
                          "minshin","sdp","self")
result <- aldmck(data=Data_2017_fr, polarity=4, respondent=9,verbose=TRUE)
summary(result)
pdf(file="Data_2017_fr_i.pdf",width=12,height=9)
plot.aldmck(result)
dev.off()



Data_2017_ig<-Data_2017[,c("cdp_i_ig", "cgp_i_ig", "hope_i_ig", "jcp_i_ig", "jip_i_ig", "ldp_i_ig",
                           "minshin_i_ig","sdp_i_ig","self_i_ig")]
colnames(Data_2017_ig)<-c("cdp", "cgp", "hope", "jcp", "jip", "ldp",
                          "minshin","sdp","self")
summary(Data_2017_ig)
Data_2017_ig<-as.matrix(Data_2017_ig)
mode(Data_2017_ig)<-"double"
result <- aldmck(data=Data_2017_ig, polarity=5, respondent=9,verbose=TRUE)
summary(result)
pdf(file="Data_2017_ig_i.pdf",width=12,height=9)
plot.aldmck(result)
dev.off()




Data_2017_ni<-Data_2017[,c("cdp_i_ni", "cgp_i_ni", "hope_i_ni", "jcp_i_ni", "jip_i_ni", "ldp_i_ni",
                           "minshin_i_ni","sdp_i_ni","self_i_ni")]
colnames(Data_2017_ni)<-c("cdp", "cgp", "hope", "jcp", "jip", "ldp",
                          "minshin","sdp","self")
result <- aldmck(data=Data_2017_ni, polarity=5, respondent=9,verbose=TRUE)
summary(result)
pdf(file="Data_2017_ni_i.pdf",width=12,height=9)
plot.aldmck(result)
dev.off()




Data_2017_rg<-Data_2017[,c("cdp_i_rg", "cgp_i_rg", "hope_i_rg", "jcp_i_rg", "jip_i_rg", "ldp_i_rg",
                           "minshin_i_rg","sdp_i_rg","self_i_rg")]
colnames(Data_2017_rg)<-c("cdp", "cgp", "hope", "jcp", "jip", "ldp",
                          "minshin","sdp","self")
Data_2017_rg<-as.matrix(Data_2017_rg)
mode(Data_2017_rg)<-"double"
result <- aldmck(data=Data_2017_rg, polarity=5, respondent=9,verbose=TRUE)
summary(result)
pdf(file="Data_2017_rg_i.pdf",width=12,height=9)
plot.aldmck(result)
dev.off()




Data_2017_sc<-Data_2017[,c("cdp_i_sc", "cgp_i_sc", "hope_i_sc", "jcp_i_sc", "jip_i_sc", "ldp_i_sc",
                           "minshin_i_sc","sdp_i_sc","self_i_sc")]
colnames(Data_2017_sc)<-c("cdp", "cgp", "hope", "jcp", "jip", "ldp",
                          "minshin","sdp","self")
result <- aldmck(data=Data_2017_sc, polarity=4, respondent=9,verbose=TRUE)
summary(result)
pdf(file="Data_2017_sc_i.pdf",width=12,height=9)
plot.aldmck(result)
dev.off()



#cdp_p_sm cgp_p_sm hope_p_sm jcp_p_sm jip_p_sm ldp_p_sm minshin_p_sm sdp_p_sm

Data_2017_tx<-Data_2017[,c("cdp_i_tx", "cgp_i_tx", "hope_i_tx", "jcp_i_tx", "jip_i_tx", "ldp_i_tx",
                           "minshin_i_tx","sdp_i_tx","self_i_tx")]
colnames(Data_2017_tx)<-c("cdp", "cgp", "hope", "jcp", "jip", "ldp",
                          "minshin","sdp","self")
Data_2017_tx<-as.matrix(Data_2017_tx)
mode(Data_2017_tx)<-"double"
result <- aldmck(data=Data_2017_tx, polarity=5, respondent=9,verbose=TRUE)
summary(result)
pdf(file="Data_2017_tx_i.pdf",width=12,height=9)
plot.aldmck(result)
dev.off()




#########


Data_2017_general <-rbind(Data_2017_bd,
                          Data_2017_dc,
                          Data_2017_df,
                          Data_2017_ev,
                          Data_2017_fr,
                          Data_2017_ig,
                          Data_2017_ni,
                          Data_2017_rg,
                          Data_2017_sc,
                          Data_2017_tx)

result <- aldmck(data=Data_2017_general, polarity=5, respondent=9,verbose=TRUE)
summary(result)
pdf(file="Data_2017_general_i.pdf",width=12,height=9)
plot.aldmck(result)
dev.off()


