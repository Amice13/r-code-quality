


#0. set up
rm(list=ls())
getwd()
#setwd()


library(foreign)
library(basicspace)
library(xlsx)
library(MCMCpack)
library(haven)

Data<-read.dta("expertpanel_indiv2.dta")


table(Data$year)

Data_2017<-subset(Data,year==2017)




Data_2017_bd<-Data_2017[,c("cdp_p_bd", "cgp_p_bd", "hope_p_bd", "jcp_p_bd", "jip_p_bd", "ldp_p_bd",
                             "minshin_p_bd","sdp_p_bd","self_p_bd")]
colnames(Data_2017_bd)<-c("cdp", "cgp", "hope", "jcp", "jip", "ldp",
                          "minshin","sdp","self")
result <- aldmck(data=Data_2017_bd, polarity=4, respondent=9,verbose=TRUE)
summary(result)
pdf(file="Data_2017_bd.pdf",width=12,height=9)
plot.aldmck(result)
dev.off()

result_bd<-result

result_bd_respondents<-result_bd$respondents

Data_2017b<-cbind(result_bd_respondents,Data_2017)

write.dta(Data_2017b,"Data_2017b.dta")



Data_2017_dc<-Data_2017[,c("cdp_p_dc", "cgp_p_dc", "hope_p_dc", "jcp_p_dc", "jip_p_dc", "ldp_p_dc",
                           "minshin_p_dc","sdp_p_dc","self_p_dc")]
colnames(Data_2017_dc)<-c("cdp", "cgp", "hope", "jcp", "jip", "ldp",
                          "minshin","sdp","self")
result <- aldmck(data=Data_2017_dc, polarity=3, respondent=9,verbose=TRUE)
summary(result)

pdf(file="Data_2017_dc.pdf",width=12,height=9)
plot.aldmck(result)
dev.off()




        


Data_2017_df<-Data_2017[,c("cdp_p_df", "cgp_p_df", "hope_p_df", "jcp_p_df", "jip_p_df", "ldp_p_df",
                           "minshin_p_df","sdp_p_df","self_p_df")]

colnames(Data_2017_df)<-c("cdp", "cgp", "hope", "jcp", "jip", "ldp",
                          "minshin","sdp","self")
result <- aldmck(data=Data_2017_df, polarity=4, respondent=9,verbose=TRUE)
summary(result)
pdf(file="Data_2017_df.pdf",width=12,height=9)
plot.aldmck(result)
dev.off()
        


Data_2017_ev<-Data_2017[,c("cdp_p_ev", "cgp_p_ev", "hope_p_ev", "jcp_p_ev", "jip_p_ev", "ldp_p_ev",
                           "minshin_p_ev","sdp_p_ev","self_p_ev")]
colnames(Data_2017_ev)<-c("cdp", "cgp", "hope", "jcp", "jip", "ldp",
                          "minshin","sdp","self")
result <- aldmck(data=Data_2017_ev, polarity=4, respondent=9,verbose=TRUE)
summary(result)
pdf(file="Data_2017_ev.pdf",width=12,height=9)
plot.aldmck(result)
dev.off()



Data_2017_fr<-Data_2017[,c("cdp_p_fr", "cgp_p_fr", "hope_p_fr", "jcp_p_fr", "jip_p_fr", "ldp_p_fr",
                           "minshin_p_fr","sdp_p_fr","self_p_fr")]
colnames(Data_2017_fr)<-c("cdp", "cgp", "hope", "jcp", "jip", "ldp",
                          "minshin","sdp","self")
result <- aldmck(data=Data_2017_fr, polarity=4, respondent=9,verbose=TRUE)
summary(result)
pdf(file="Data_2017_fr.pdf",width=12,height=9)
plot.aldmck(result)
dev.off()
        


Data_2017_ig<-Data_2017[,c("cdp_p_ig", "cgp_p_ig", "hope_p_ig", "jcp_p_ig", "jip_p_ig", "ldp_p_ig",
                           "minshin_p_ig","sdp_p_ig","self_p_ig")]
colnames(Data_2017_ig)<-c("cdp", "cgp", "hope", "jcp", "jip", "ldp",
                          "minshin","sdp","self")
summary(Data_2017_ig)
Data_2017_ig<-as.matrix(Data_2017_ig)
mode(Data_2017_ig)<-"double"
result <- aldmck(data=Data_2017_ig, polarity=4, respondent=9,verbose=TRUE)
summary(result)
pdf(file="Data_2017_ig.pdf",width=12,height=9)
plot.aldmck(result)
dev.off()
        



Data_2017_ni<-Data_2017[,c("cdp_p_ni", "cgp_p_ni", "hope_p_ni", "jcp_p_ni", "jip_p_ni", "ldp_p_ni",
                           "minshin_p_ni","sdp_p_ni","self_p_ni")]
colnames(Data_2017_ni)<-c("cdp", "cgp", "hope", "jcp", "jip", "ldp",
                          "minshin","sdp","self")
result <- aldmck(data=Data_2017_ni, polarity=4, respondent=9,verbose=TRUE)
summary(result)
pdf(file="Data_2017_ni.pdf",width=12,height=9)
plot.aldmck(result)
dev.off()
        



Data_2017_rg<-Data_2017[,c("cdp_p_rg", "cgp_p_rg", "hope_p_rg", "jcp_p_rg", "jip_p_rg", "ldp_p_rg",
                           "minshin_p_rg","sdp_p_rg","self_p_rg")]
colnames(Data_2017_rg)<-c("cdp", "cgp", "hope", "jcp", "jip", "ldp",
                          "minshin","sdp","self")
result <- aldmck(data=Data_2017_rg, polarity=4, respondent=9,verbose=TRUE)
summary(result)
pdf(file="Data_2017_rg.pdf",width=12,height=9)
plot.aldmck(result)
dev.off()
        



Data_2017_sc<-Data_2017[,c("cdp_p_sc", "cgp_p_sc", "hope_p_sc", "jcp_p_sc", "jip_p_sc", "ldp_p_sc",
                           "minshin_p_sc","sdp_p_sc","self_p_sc")]
colnames(Data_2017_sc)<-c("cdp", "cgp", "hope", "jcp", "jip", "ldp",
                          "minshin","sdp","self")
result <- aldmck(data=Data_2017_sc, polarity=4, respondent=9,verbose=TRUE)
summary(result)
pdf(file="Data_2017_sc.pdf",width=12,height=9)
plot.aldmck(result)
dev.off()
   


#cdp_p_sm cgp_p_sm hope_p_sm jcp_p_sm jip_p_sm ldp_p_sm minshin_p_sm sdp_p_sm

Data_2017_tx<-Data_2017[,c("cdp_p_tx", "cgp_p_tx", "hope_p_tx", "jcp_p_tx", "jip_p_tx", "ldp_p_tx",
                           "minshin_p_tx","sdp_p_tx","self_p_tx")]
colnames(Data_2017_tx)<-c("cdp", "cgp", "hope", "jcp", "jip", "ldp",
                          "minshin","sdp","self")
result <- aldmck(data=Data_2017_tx, polarity=4, respondent=9,verbose=TRUE)
summary(result)
pdf(file="Data_2017_tx.pdf",width=12,height=9)
plot.aldmck(result)
dev.off()



result_tx<-result

result_tx_respondents<-result_tx$respondents

Data_2017c<-cbind(result_tx_respondents,Data_2017)

write.dta(Data_2017c,"Data_2017c.dta")



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

result <- aldmck(data=Data_2017_general, polarity=4, respondent=9,verbose=TRUE)
summary(result)
pdf(file="Data_2017_general.pdf",width=12,height=9)
plot.aldmck(result)
dev.off()


