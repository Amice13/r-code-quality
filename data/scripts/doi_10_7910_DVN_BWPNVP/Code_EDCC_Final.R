## Incomplete information and product quality in rural markets: Evidence from an experimental auction for maize in Senegal
## Prieto, Ricker-Gilbert, Bauchet, and Sall

# Sets up file and reads in data ----                                                        

#Clears all objects from memory
rm(list=ls()) 

#Sets the working directory
setwd("C:\\Users\\stacy.prieto\\OneDrive - Catholic Relief Services\\Experiment\\EDCC Submission\\Code")
 
#Reads text file and names it
trader <- read.table("Export_Trader.txt", sep = ",", header = TRUE)

library('plm')
library('lmtest')
library('car')
library('stats')

# Creates necessary variables ----

## Creates new variable of trader offers per kg and preps trader data for merge
trader <- cbind(0,0,0,0,0,0,0,0,0,0,0,0,trader[,'Offer']/50,trader[,-1])
colnames(trader)[1:13] <- c('Plant_Maize','FarmExperience','Consumer_Improve_Maize_Quality3','Consume_Storage_Time',
                            'Qty_Store','LvH_MC','Why_LvH1','Why_LvH2','Why_LvH3','Why_LvH5','Why_LvH7','Why_LvH9','Offer')

## Merges in consumer data
consumer <- read.table("Export_Consumer.txt", sep = ",", header = TRUE)
consumer = cbind(consumer,0,0,0,0,0,0,0)
colnames(consumer)[26:32] = colnames(trader)[26:32]
alldata <- rbind(trader,consumer)

## Combines duplicated columns from Consumer survey
YrsExperienceSaleFarm = rowSums(alldata[,c('FarmExperience','Years_Trade')], na.rm = T)
ImpMaizeDry = rowSums(alldata[,c('Consumer_Improve_Maize_Quality3','Dry_YN')], na.rm = T)
StoreWeeks = rowSums(alldata[,c('Consume_Storage_Time','Storage_Time')], na.rm = T)
KgSold2014Harvest = rowSums(alldata[,c('Qty_Store','Qty_Sold')], na.rm = T)

alldata = cbind(YrsExperienceSaleFarm,ImpMaizeDry,StoreWeeks,KgSold2014Harvest,alldata)
alldata = alldata[,-c(6,7,8,9,27,30,31,36)]
colnames(alldata)[5:12] = c('Plant_Maize_C','Prefer_LvH_C','Prefer_LessInsect_C','Prefer_LessMold_C','Prefer_Color_C',
                            'Prefer_Storage_C','Prefer_Health_C','Prefer_Process_C')
colnames(alldata)[18] = c('Will sell')
colnames(alldata)[25:28] = c('More_Less_Dry_T','More_Less_Weight_T','More_Less_Safe_T','More_Less_ConsumerPrefer_T')

#Characterization variable to trader/ consumer binary. 
#Note (NA)Consumer,(1)Banabana,(2)Small wholesaler,(3)Large wholesaler
colnames(alldata)[23] = c('Trader')
alldata$Trader[is.na(alldata$Trader)] = 0
alldata$Trader[alldata$Trader>0] = 1

#Education variable to binary. 
#Note (0)None,(1)Elementary,(2)Secondary,(3)B.A.C.,(4)Bachelor's,(6)Koranic school,(99)Other (specify)
Table.1.footnote = table(alldata[,c('Education','Trader')])

alldata$Education[alldata$Education>5] = 0
alldata$Education[alldata$Education<1] = 0
alldata$Education[alldata$Education!=0] = 1

#Trader charging more or less for dry maize with labels
alldata$More_Less_Dry_T = factor(alldata$More_Less_Dry_T,
                                 levels = c(1,2),
                                 labels = c('More','Less'))

#Consumer prefers low or high moisture maize with labels
alldata$Prefer_LvH_C = factor(alldata$Prefer_LvH_C,
                              levels = c(1,2,4),
                              labels = c('Low','High','Dont know'))

## Creates maize type binary variables and interaction terms
X_L15 <- alldata['MoistureContent_ID']==5
X_L18 <- alldata['MoistureContent_ID']==6
X_L <- alldata['MoistureContent_ID']>3
X_15 = vector(length = nrow(alldata))
X_15[alldata['MoistureContent_ID']!=1 & 
     alldata['MoistureContent_ID']!=3 &
     alldata['MoistureContent_ID']!=4 &
     alldata['MoistureContent_ID']!=6] = 1
X_18 = vector(length = nrow(alldata))
X_18[alldata['MoistureContent_ID']!=1 & 
     alldata['MoistureContent_ID']!=2 &
     alldata['MoistureContent_ID']!=4 &
     alldata['MoistureContent_ID']!=5] = 1

type <- cbind(X_15,X_18,X_L,X_L15,X_L18)
colnames(type) <- c('U_15','U_18','L_13','L_15','L_18')

## Creates "type" by consumer and reseller - then fills them in
Xc_15 = vector(length = nrow(alldata))
Xc_18 = vector(length = nrow(alldata))
Xc_L = vector(length = nrow(alldata))
Xc_L15 = vector(length = nrow(alldata))
Xc_L18 = vector(length = nrow(alldata))

Xr_u13 = vector(length = nrow(alldata))
Xr_15 = vector(length = nrow(alldata))
Xr_18 = vector(length = nrow(alldata))
Xr_L = vector(length = nrow(alldata))
Xr_L15 = vector(length = nrow(alldata))
Xr_L18 = vector(length = nrow(alldata))

Xc_15[alldata['MoistureContent_ID']!=1 & alldata['MoistureContent_ID']!=3 &
        alldata['MoistureContent_ID']!=4 & alldata['MoistureContent_ID']!=6 & 
        alldata['Trader']==0] = 1
Xc_18[alldata['MoistureContent_ID']!=1 & alldata['MoistureContent_ID']!=2 &
        alldata['MoistureContent_ID']!=4 & alldata['MoistureContent_ID']!=5 & 
        alldata['Trader']==0] = 1
Xc_L[alldata['MoistureContent_ID']>3 & alldata['Trader']==0] = 1
Xc_L15[alldata['MoistureContent_ID']==5 & alldata['Trader']==0] = 1
Xc_L18[alldata['MoistureContent_ID']==6 & alldata['Trader']==0] = 1

Xr_15[alldata['MoistureContent_ID']!=1 & alldata['MoistureContent_ID']!=3 &
      alldata['MoistureContent_ID']!=4 & alldata['MoistureContent_ID']!=6 & 
      alldata['Trader']==1] = 1
Xr_18[alldata['MoistureContent_ID']!=1 & alldata['MoistureContent_ID']!=2 &
      alldata['MoistureContent_ID']!=4 & alldata['MoistureContent_ID']!=5 & 
      alldata['Trader']==1] = 1
Xr_L[alldata['MoistureContent_ID']>3 & alldata['Trader']==1] = 1
Xr_L15[alldata['MoistureContent_ID']==5 & alldata['Trader']==1] = 1
Xr_L18[alldata['MoistureContent_ID']==6 & alldata['Trader']==1] = 1

Xr_u13[alldata['MoistureContent_ID']==1 & alldata['Trader']==1] = 1

type_participant_tc = cbind(Xc_15,Xc_18,Xc_L,Xc_L15,Xc_L18,
                     Xr_u13,Xr_15,Xr_18,Xr_L,Xr_L15,Xr_L18)
colnames(type_participant_tc) <- c('Uc_15','Uc_18','Lc_13','Lc_15','Lc_18',
                           'Ur_13','Ur_15','Ur_18','Lr_13','Lr_15','Lr_18')

alldata = cbind(alldata,type,type_participant_tc)

#Create trader and consumer datasets
consumerdata = subset(alldata,Trader==0)
traderdata = subset(alldata,Trader==1)

# Narrative regarding reasons for WTP (near Figures 2-3) ----
  #Traders
ChargeMoreDry_T = round(table(traderdata$More_Less_Dry_T)/6/166,2)

Weight = round(table(traderdata$More_Less_Weight_T)/6/166,2)
Safer = round(table(traderdata$More_Less_Safe_T)/6/166,2)
ConsumerPref = round(table(traderdata$More_Less_ConsumerPrefer_T)/6/166,2)

  #Consumers
PreferLowHighMoisture_C = round(table(consumerdata$Prefer_LvH_C)/6/182,2)

Insect = round(table(consumerdata$Prefer_LessInsect_C)/6/182,2)
Health = round(table(consumerdata$Prefer_Health_C)/6/182,2)
Color = round(table(consumerdata$Prefer_Color_C)/6/182,2)
Process = round(table(consumerdata$Prefer_Process_C)/6/182,2)
Mold = round(table(consumerdata$Prefer_LessMold_C)/6/182,2)
Storage = round(table(consumerdata$Prefer_Storage_C)/6/182,2)

# Figure 3 ----

  #Data for Figure 3, that gets exported and used in Excel workbook
##Consumer and trader density and quality premiums
cDP = matrix(nrow=182,ncol=2)
consumerUnique = consumerdata[!duplicated(consumerdata[,'Unique_ID']),]
cDP = cbind(consumerUnique[,'Unique_ID'],cDP)
colnames(cDP) = c('hhid','OfferLHigh','OfferLLow')

for(i in 1:nrow(consumerdata)) {
  for(j in 1:nrow(cDP)) {
    if(consumerdata[i,'Unique_ID']==cDP[j,'hhid']){
      if(consumerdata[i,'MoistureContent_ID']==4){
        cDP[j,'OfferLLow']=consumerdata[i,'Offer']
                                                }
          else{
            if(consumerdata[i,'MoistureContent_ID']==6){
              cDP[j,'OfferLHigh']=consumerdata[i,'Offer']
              }
          }
        }
      }
    }
cDP = cbind(cDP,cDP[,'OfferLHigh']*.105,cDP[,'OfferLLow']-cDP[,'OfferLHigh']*1.105)
colnames(cDP)[4:5] = c('DP_CFA','QP_CFA')
cDP = cDP[order(cDP[,'QP_CFA']),]
write.csv(cDP,'cDP.csv')

tDP = matrix(nrow=166,ncol=2)
traderUnique = traderdata[!duplicated(traderdata[,'Unique_ID']),]
tDP = cbind(traderUnique[,'Unique_ID'],tDP)
colnames(tDP) = c('hhid','OfferLHigh','OfferLLow')

for(i in 1:nrow(traderdata)) {
  for(j in 1:nrow(tDP)) {
    if(traderdata[i,'Unique_ID']==tDP[j,'hhid']){
      if(traderdata[i,'MoistureContent_ID']==4){
        tDP[j,'OfferLLow']=traderdata[i,'Offer']
      }
      else{
        if(traderdata[i,'MoistureContent_ID']==6){
          tDP[j,'OfferLHigh']=traderdata[i,'Offer']
        }
      }
    }
  }
}
tDP = cbind(tDP,tDP[,'OfferLHigh']*.105,tDP[,'OfferLLow']-tDP[,'OfferLHigh']*1.105)
colnames(tDP)[4:5] = c('DP_CFA','QP_CFA')
tDP = tDP[order(tDP[,'QP_CFA']),]
write.csv(tDP,'tDP.csv')

# Figure 4 ----
WTPdiff = matrix(nrow=348,ncol=6)
dataUnique = alldata[!duplicated(alldata[,'Unique_ID']),]
WTPdiff = cbind(dataUnique[,'Unique_ID'],WTPdiff)
colnames(WTPdiff) = c('hhid','Un_low','Un_med','Un_hi','L_low','L_med','L_hi')

for(i in 1:nrow(alldata)) {
  for(j in 1:nrow(WTPdiff)) {
    if(alldata[i,'Unique_ID']==WTPdiff[j,'hhid']){
      if(alldata[i,'MoistureContent_ID']==1){
        WTPdiff[j,'Un_low']=alldata[i,'Offer']
      }
      else{
        if(alldata[i,'MoistureContent_ID']==2){
          WTPdiff[j,'Un_med']=alldata[i,'Offer']
        }
        else{
          if(alldata[i,'MoistureContent_ID']==3){
            WTPdiff[j,'Un_hi']=alldata[i,'Offer']
          }
          else{
            if(alldata[i,'MoistureContent_ID']==4){
              WTPdiff[j,'L_low']=alldata[i,'Offer']
            }
            else{
              if(alldata[i,'MoistureContent_ID']==5){
                WTPdiff[j,'L_med']=alldata[i,'Offer']
              }
              else{
                if(alldata[i,'MoistureContent_ID']==6){
                  WTPdiff[j,'L_hi']=alldata[i,'Offer']
                }
      }
    }
  }
}
      }
    }
  }
}
WTPdiff = cbind(WTPdiff,
                WTPdiff[,'L_low']-WTPdiff[,'Un_low'],
                WTPdiff[,'L_med']-WTPdiff[,'Un_med'],
                WTPdiff[,'L_hi']-WTPdiff[,'Un_hi'],
                WTPdiff[,'Un_low']-WTPdiff[,'Un_hi'],
                WTPdiff[,'L_low']-WTPdiff[,'L_hi'])
colnames(WTPdiff)[8:12] = c('Diff_low','Diff_med','Diff_hi','Un','L')

  #Each MC against other
plot(density(WTPdiff[,'Diff_low']),family = "serif",ps = 12, cex = 1,lwd = 1.5,xlab = "WTP difference (labeled bid - unlabeled bid) in CFA/ kg")
legend(100,0.030,legend=c("Low","Medium","High"),col = c(1,34,28),lty=1:3,cex = .8)
lines(density(WTPdiff[,'Diff_med']),lty = 2,col = 34)
lines(density(WTPdiff[,'Diff_hi']),lty = 3,col = 28)

  #Each label slope span against each other
plot(density(WTPdiff[,'Un']),main = "",family = "serif",ps = 12, cex = 1,lwd = 1.5,xlab = "WTP difference (Low MC - High MC) in CFA/ kg")
legend(150,0.008,legend=c("Unlabeled","Labeled"),col = c(1,34),lty=1:3,cex = .8)
lines(density(WTPdiff[,'L']),lty = 2,col = 34)

# Table 1 ----
Table.1 = aggregate(alldata[,c('Gender','Age','Education',"StoreWeeks",'Plant_Maize_C','YrsExperienceSaleFarm',
                                     'KgSold2014Harvest')],
          list(alldata[,'Trader']),sum,na.rm = T)/6
Table.1 = t(Table.1)
Table.1 = Table.1[-1,]
Table.1 = round(matrix(c(Table.1[1:5,1]/182,Table.1[6:7,1]/Table.1[5,1],Table.1[,2]/166),nrow = 7, ncol = 2),2)
colnames(Table.1) = c('Consumers','Traders')
rownames(Table.1) = c('Female (%)','Age (yrs)','Completed Elementary (%)','Storage Time (wks)','Grew 2015 Maize (%)',
                      'Experience (yrs)','Stored/ Sold (kg)')

  #Table 1 notes
Table.1.footnote = cbind(Table.1.footnote[6,1]/6/182,Table.1.footnote[6,2]/6/166)
Table.1.header = as.matrix(table(alldata$Trader)/6)
Table.1.header = cbind(Table.1.header[1],Table.1.header[2])
Table.1.notes = round(rbind(Table.1.header,Table.1.footnote),2)
rownames(Table.1.notes) = c('N_total','Koranic school (%)')
colnames(Table.1.notes) = c('Consumer','Trader')

# Table 2 ----
M.part = round(aggregate(alldata$Offer, by = list(Trader = alldata$Trader, MoistureContent = alldata$MoistureContent_ID),
                    FUN = mean),0)
M.all = round(aggregate(alldata[,'Offer'],list(alldata[,c('MoistureContent_ID')]),mean),0)

Table.2 = matrix(c(M.part[2,3],M.part[8,3],M.part[4,3],M.part[10,3],M.part[6,3],M.part[12,3],
                   M.part[1,3],M.part[7,3],M.part[3,3],M.part[9,3],M.part[5,3],M.part[11,3],
                   M.all[1,2],M.all[4,2],M.all[2,2],M.all[5,2],M.all[3,2],M.all[6,2]),
                 nrow = 3, ncol = 6, byrow = T)
rownames(Table.2) = c('Traders','Consumers','All Participants')
colnames(Table.2) = c('Low_UnL','Low_L','Med_UnL','Med_L','Hi_UnL','Hi_L')

# Table 3 ----
  #3.1.a
Table.3.1.a = cbind(alldata[,c('Unique_ID','MoistureContent_ID',
                               'Offer','U_15','U_18','L_13','L_15','L_18')])
write.table(Table.3.1.a,       'Table_3_1_a.txt', sep=',')
Table.3.1.a <- as.data.frame(read.table("Table_3_1_a.txt", sep = ",", header = TRUE))
Table.3.1.a.Form = Offer ~ U_15 + U_18 + L_13 + L_15 + L_18

Table.3.1.a.N = nrow(Table.3.1.a)
Table.3.1.a.N.unique = length(unique(Table.3.1.a$Unique_ID))
WTP.Low_Un.3.1.a = Table.2['All Participants','Low_UnL']

Table.3.1.a = plm(Table.3.1.a.Form, data = Table.3.1.a, model = 'within')
Var.3.1.a = vcovHC(Table.3.1.a,type="HC0",cluster="group")
results.3.1.a = round(coeftest(Table.3.1.a,Var.3.1.a),1)
rownames(results.3.1.a) = c('Unlabeled medium','Unlabeled high','Labeled low','Labeled*Medium','Labeled*High')

  #3.1.b
    #Coefficients for interaction terms
results.3.1.b = round(rbind(Table.3.1.a$coefficients['U_15']+Table.3.1.a$coefficients['L_13']+Table.3.1.a$coefficients['L_15'],
                            Table.3.1.a$coefficients['U_18']+Table.3.1.a$coefficients['L_13']+Table.3.1.a$coefficients['L_18']),1)

    #SE for interaction terms via Delta method
g.15 = c(-1,0,-1,-1,0)
se.3.1.b.L15 = round(sqrt(t(g.15)%*%Var.3.1.a%*%g.15),1)
g.18 = c(0,-1,-1,0,-1)
se.3.1.b.L18 = round(sqrt(t(g.18)%*%Var.3.1.a%*%g.18),1)

  #3.2.a
Table.3.2.a = cbind(alldata[,c('Unique_ID','MoistureContent_ID','Offer',
                               'Ur_15','Ur_18','Lr_13','Lr_15','Lr_18',
                               'Uc_15','Uc_18','Lc_13','Lc_15','Lc_18')])
write.table(Table.3.2.a, 'Table_3_2_a.txt', sep=',')
Table.3.2.a <- as.data.frame(read.table("Table_3_2_a.txt", sep = ",", header = TRUE))
Table.3.2.a.Form = Offer ~ Uc_15 + Uc_18 + Lc_13 + Lc_15 + Lc_18 + 
                           Ur_15 + Ur_18 + Lr_13 + Lr_15 + Lr_18

Table.3.2.a.N = nrow(Table.3.2.a)
Table.3.2.a.N.unique = length(unique(Table.3.2.a$Unique_ID))
WTP.Low_Un.3.2.a = Table.2[c('Traders','Consumers'),'Low_UnL']

Table.3.2.a = plm(Table.3.2.a.Form, data = Table.3.2.a, model = 'within')
Var.3.2.a = vcovHC(Table.3.2.a,type="HC0",cluster="group")
results.3.2.a = round(coeftest(Table.3.2.a,Var.3.2.a),1)
rownames(results.3.2.a) = c('Unlabeled medium_C','Unlabeled high_C','Labeled low_C','Labeled*Medium_C','Labeled*High_C',
                            'Unlabeled medium_T','Unlabeled high_T','Labeled low_T','Labeled*Medium_T','Labeled*High_T')

  #3.2.b
    #Coefficients for interaction terms and labeling
results.3.2.b = round(
  rbind(Table.3.2.a$coefficients['Uc_15']+Table.3.2.a$coefficients['Lc_13']+Table.3.2.a$coefficients['Lc_15'],
        Table.3.2.a$coefficients['Uc_18']+Table.3.2.a$coefficients['Lc_13']+Table.3.2.a$coefficients['Lc_18'],
        
        Table.3.2.a$coefficients['Ur_15']+Table.3.2.a$coefficients['Lr_13']+Table.3.2.a$coefficients['Lr_15'],
        Table.3.2.a$coefficients['Ur_18']+Table.3.2.a$coefficients['Lr_13']+Table.3.2.a$coefficients['Lr_18']),1)

    #SE for interaction terms via Delta method
g.15.3.2.b.C = c(-1,0,-1,-1,0,rep(0,(nrow(Var.3.2.a)-5)))
se.3.2.b.L15.C = round(sqrt(t(g.15.3.2.b.C)%*%Var.3.2.a%*%g.15.3.2.b.C),1)
g.18.3.2.b.C = c(0,-1,-1,0,-1,rep(0,(nrow(Var.3.2.a)-5)))
se.3.2.b.L18.C = round(sqrt(t(g.18.3.2.b.C)%*%Var.3.2.a%*%g.18.3.2.b.C),1)

g.15.3.2.b.R = c(0,0,0,0,0,-1,0,-1,-1,0)
se.3.2.b.L15.R = round(sqrt(t(g.15.3.2.b.R)%*%Var.3.2.a%*%g.15.3.2.b.R),1)
g.18.3.2.b.R = c(0,0,0,0,0,0,-1,-1,0,-1)
se.3.2.b.L18.R = round(sqrt(t(g.18.3.2.b.R)%*%Var.3.2.a%*%g.18.3.2.b.R),1)

 #Panel b
b.1 = rbind(results.3.1.b[1],se.3.1.b.L15,results.3.1.b[2],se.3.1.b.L18)
b.2.C = rbind(results.3.2.b[1],se.3.2.b.L15.C,results.3.2.b[2],se.3.2.b.L18.C)
b.2.T = rbind(results.3.2.b[3],se.3.2.b.L15.R,results.3.2.b[4],se.3.2.b.L18.R)
Panel.3.b = round(cbind(b.1,b.2.C,b.2.T),1)
rownames(Panel.3.b) = c('Labeled medium','SE','Labeled high','SE')
colnames(Panel.3.b) = c('(1)','(2).Consumer','(2).Trader')

# Table 4 ----

  #Q1
q1.all = linearHypothesis(Table.3.1.a,c('U_15 + L_15 = 0','U_18 + L_18 = 0'))
q1.C = linearHypothesis(Table.3.2.a,c('Uc_15 + Lc_15 = 0','Uc_18 + Lc_18 = 0'))
q1.T = linearHypothesis(Table.3.2.a,c('Ur_15 + Lr_15 = 0','Ur_18 + Lr_18 = 0'))

  #Q2
q2.LvH.all = linearHypothesis(Table.3.1.a,'1.105*U_18 + 1.105*L_18 + 0.105*L_13 + 0.105*145 = 0', test = 'F')
#Is LHS >= 0? If so, reject. Use F distribution to get t distribution e.g. pt((q2.all[2,'F']),df=q2.all[2,'Res.Df']) 
#or use p-value of F-test to get p-value of one-tailed test
#https://www.stata.com/support/faqs/statistics/one-sided-tests-for-coefficients/
q2.LvH.all.1_tail = q2.LvH.all[2,'Pr(>F)']/2
q2.LvH.C = linearHypothesis(Table.3.2.a,'1.105*Uc_18 + 1.105*Lc_18 + 0.105*Lc_13 + 0.105*169 = 0', test = 'F')
q2.LvH.C.1_tail = q2.LvH.C [2,'Pr(>F)']/2
q2.LvH.T = linearHypothesis(Table.3.2.a,'1.105*Ur_18 + 1.105*Lr_18 + 0.105*Lr_13 + 0.105*119 = 0', test = 'F')
q2.LvH.T.1_tail = q2.LvH.T[2,'Pr(>F)']/2

q2.MvH.all = linearHypothesis(Table.3.1.a,'1.059*U_18 + 1.059*L_18 + 0.059*145 + 0.059*L_13 - U_15 - L_15 = 0', test = 'F')
q2.MvH.all.1_tail = q2.MvH.all[2,'Pr(>F)']/2
q2.MvH.C = linearHypothesis(Table.3.2.a,'1.059*Uc_18 + 1.059*Lc_18 + 0.059*145 + 0.059*Lc_13 - Uc_15 - Lc_15 = 0', test = 'F')
q2.MvH.C.1_tail = q2.MvH.C[2,'Pr(>F)']/2
q2.MvH.T = linearHypothesis(Table.3.2.a,'1.059*Ur_18 + 1.059*Lr_18 + 0.059*145 + 0.059*Lr_13 - Ur_15 - Lr_15 = 0', test = 'F')
q2.MvH.T.1_tail = q2.MvH.T[2,'Pr(>F)']/2

q2.LvM.all = linearHypothesis(Table.3.1.a,'1.043*U_15 + 1.043*L_15 + 0.043*L_13 + 0.043*145 = 0', test = 'F')
q2.LvM.all.1_tail = q2.LvM.all[2,'Pr(>F)']/2
q2.LvM.C = linearHypothesis(Table.3.2.a,'1.043*Uc_15 + 1.043*Lc_15 + 0.043*Lc_13 + 0.043*145 = 0', test = 'F')
q2.LvM.C.1_tail = q2.LvM.C[2,'Pr(>F)']/2
q2.LvM.T = linearHypothesis(Table.3.2.a,'1.043*Ur_15 + 1.043*Lr_15 + 0.043*Lr_13 + 0.043*145 = 0', test = 'F')
q2.LvM.T.1_tail = q2.LvM.T[2,'Pr(>F)']/2

  #Q3
q3.all = linearHypothesis(Table.3.1.a,c('U_15 = 0','U_18 = 0'))
q3.C = linearHypothesis(Table.3.2.a,c('Uc_15 = 0','Uc_18 = 0'))
q3.T = linearHypothesis(Table.3.2.a,c('Ur_15 = 0','Ur_18 = 0'))

  #Q4
q4.LvH = linearHypothesis(Table.3.2.a,'Uc_18 = Ur_18')
q4.LvM = linearHypothesis(Table.3.2.a,'Uc_15 = Ur_15')

# Table 5 ----

  #5.1                                                      
Table.5.1 = cbind(alldata[,c('Unique_ID','MoistureContent_ID','Offer','U_15','U_18','L_13','L_15','L_18','Age')],
                  alldata[,'Age']*alldata[,c('U_15','U_18','L_13','L_15','L_18')])
colnames(Table.5.1)[10:14] = paste(colnames(Table.5.1)[10:14],"_Age",sep="")
write.table(Table.5.1, 'Table_5_1.txt', sep=',')
Table.5.1 <- as.data.frame(read.table("Table_5_1.txt", sep = ",", header = TRUE))

Table.5.1.Form = Offer ~ U_15 + U_18 + L_13 + L_15 + L_18 + Age + U_15_Age + U_18_Age + L_13_Age + L_15_Age + L_18_Age           

Table.5.1 = plm(Table.5.1.Form, data = Table.5.1, model = 'random')
results.5.1 = round(coeftest(Table.5.1,vcov=vcovHC(Table.5.1,type="HC0",cluster="group")),2)
rownames(results.5.1) = c('Intercept','Unlabeled medium','Unlabeled high','Labeled low','Labeled*Medium','Labeled*High','Age',
                          'Unlabeled medium*Age','Unlabeled high*Age','Labeled low*Age','Labeled*Medium*Age','Labeled*High*Age')

  #5.2                                                      
Table.5.2 = cbind(alldata[,c('Unique_ID','MoistureContent_ID','Offer','U_15','U_18','L_13','L_15','L_18','Education')],
                  alldata[,'Education']*alldata[,c('U_15','U_18','L_13','L_15','L_18')])
colnames(Table.5.2)[10:14] = paste(colnames(Table.5.2)[10:14],"_Ed",sep="")
write.table(Table.5.2, 'Table_5_2.txt', sep=',')
Table.5.2 <- as.data.frame(read.table("Table_5_2.txt", sep = ",", header = TRUE))

Table.5.2.Form = Offer ~ U_15 + U_18 + L_13 + L_15 + L_18 + Education + U_15_Ed + U_18_Ed + L_13_Ed + L_15_Ed + L_18_Ed           

Table.5.2 = plm(Table.5.2.Form, data = Table.5.2, model = 'random')
results.5.2 = round(coeftest(Table.5.2,vcov=vcovHC(Table.5.2,type="HC0",cluster="group")),2)
rownames(results.5.2) = c('Intercept','Unlabeled medium','Unlabeled high','Labeled low','Labeled*Medium','Labeled*High','Ed',
                          'Unlabeled medium*Ed','Unlabeled high*Ed','Labeled low*Ed','Labeled*Medium*Ed','Labeled*High*Ed')

  #5.3                                                       
Table.5.3 = cbind(alldata[,c('Unique_ID','MoistureContent_ID','Offer','U_15','U_18','L_13','L_15','L_18','YrsExperienceSaleFarm')],
                  alldata[,"YrsExperienceSaleFarm"]*alldata[,c('U_15','U_18','L_13','L_15','L_18')])
colnames(Table.5.3)[10:14] = paste(colnames(Table.5.3)[10:14],"_Exp",sep="")
write.table(Table.5.3, 'Table_5_3.txt', sep=',')
Table.5.3 <- as.data.frame(read.table("Table_5_3.txt", sep = ",", header = TRUE))

Table.5.3.Form = Offer ~ U_15 + U_18 + L_13 + L_15 + L_18 + YrsExperienceSaleFarm + U_15_Exp + U_18_Exp + L_13_Exp + L_15_Exp + L_18_Exp           

Table.5.3 = plm(Table.5.3.Form, data = Table.5.3, model = 'random')
results.5.3 = round(coeftest(Table.5.3,vcov=vcovHC(Table.5.3,type="HC0",cluster="group")),2)
rownames(results.5.3) = c('Intercept','Unlabeled medium','Unlabeled high','Labeled low','Labeled*Medium','Labeled*High','Exp',
                          'Unlabeled medium*Exp','Unlabeled high*Exp','Labeled low*Exp','Labeled*Medium*Exp','Labeled*High*Exp')

  #5.4                                                       
Table.5.4 = cbind(alldata[,c('Unique_ID','MoistureContent_ID','Offer','U_15','U_18','L_13','L_15','L_18','StoreWeeks')],
                  alldata[,'StoreWeeks']*alldata[,c('U_15','U_18','L_13','L_15','L_18')])
colnames(Table.5.4)[10:14] = paste(colnames(Table.5.4)[10:14],"_StoreWeeks",sep="")
write.table(Table.5.4, 'Table_5_4.txt', sep=',')
Table.5.4 <- as.data.frame(read.table("Table_5_4.txt", sep = ",", header = TRUE))

Table.5.4.Form = Offer ~ U_15 + U_18 + L_13 + L_15 + L_18 + StoreWeeks + 
  U_15_StoreWeeks + U_18_StoreWeeks + L_13_StoreWeeks + L_15_StoreWeeks + L_18_StoreWeeks           

Table.5.4 = plm(Table.5.4.Form, data = Table.5.4, model = 'random')
results.5.4 = round(coeftest(Table.5.4,vcov=vcovHC(Table.5.4,type="HC0",cluster="group")),2)
rownames(results.5.4) = c('Intercept','Unlabeled medium','Unlabeled high','Labeled low','Labeled*Medium','Labeled*High','StoreWeeks',
                          'Unlabeled medium*Weeks','Unlabeled high*Weeks','Labeled low*Weeks','Labeled*Medium*Weeks','Labeled*High*Weeks')

  #5.5                                                      
Table.5.5 = cbind(alldata[,c('Unique_ID','MoistureContent_ID','Offer','U_15','U_18','L_13','L_15','L_18','ImpMaizeDry')],
                  alldata[,'ImpMaizeDry']*alldata[,c('U_15','U_18','L_13','L_15','L_18')])
colnames(Table.5.5)[10:14] = paste(colnames(Table.5.5)[10:14],"_ImpMaizeDry",sep="")
write.table(Table.5.5, 'Table_5_5.txt', sep=',')
Table.5.5 <- as.data.frame(read.table("Table_5_5.txt", sep = ",", header = TRUE))

Table.5.5.Form = Offer ~ U_15 + U_18 + L_13 + L_15 + L_18 + ImpMaizeDry + 
  U_15_ImpMaizeDry + U_18_ImpMaizeDry + L_13_ImpMaizeDry + L_15_ImpMaizeDry + L_18_ImpMaizeDry           

Table.5.5 = plm(Table.5.5.Form, data = Table.5.5, model = 'random')
results.5.5 = round(coeftest(Table.5.5,vcov=vcovHC(Table.5.5,type="HC0",cluster="group")),2)
rownames(results.5.5) = c('Intercept','Unlabeled medium','Unlabeled high','Labeled low','Labeled*Medium','Labeled*High','ImpMaizeDry',
                          'Unlabeled medium*Dry','Unlabeled high*Dry','Labeled low*Dry','Labeled*Medium*Dry','Labeled*High*Dry')

# Table A.1 ----
#Where we solve the Brusewitz equation for kg and get kg = 4,810*V*(MC^2 - 0.617672*MC + 0.225842)
MC = as.matrix(c(.19,.18,.17,.16,.15,.14,.13))
V = (0.000208*50)/(.19^2-0.617672*0.19+0.225842)
A.1.kg = 4810*V*(MC^2 - 0.617672*MC + 0.225842)
A.1.D = A.1.kg/V
Table.A.1 = cbind(round(A.1.D,1),round(A.1.kg,1))
rownames(Table.A.1) = MC*100
colnames(Table.A.1) = c('Density','Weight')
  
# Appendix B ----
  #B.1                                                      
Table.B.1 = cbind(traderdata[,c('Unique_ID','MoistureContent_ID','Offer','U_15','U_18','L_13','L_15','L_18','Age')],
                  traderdata[,'Age']*traderdata[,c('U_15','U_18','L_13','L_15','L_18')])
colnames(Table.B.1)[10:14] = paste(colnames(Table.B.1)[10:14],"_Age",sep="")
write.table(Table.B.1, 'Table_B_1.txt', sep=',')
Table.B.1 <- as.data.frame(read.table("Table_B_1.txt", sep = ",", header = TRUE))

Table.B.1.Form = Offer ~ U_15 + U_18 + L_13 + L_15 + L_18 + Age + U_15_Age + U_18_Age + L_13_Age + L_15_Age + L_18_Age           

Table.B.1 = plm(Table.B.1.Form, data = Table.B.1, model = 'random')
results.B.1 = round(coeftest(Table.B.1,vcov=vcovHC(Table.B.1,type="HC0",cluster="group")),2)
rownames(results.B.1) = c('Intercept','Unlabeled medium','Unlabeled high','Labeled low','Labeled*Medium','Labeled*High','Age',
                          'Unlabeled medium*Age','Unlabeled high*Age','Labeled low*Age','Labeled*Medium*Age','Labeled*High*Age')

  #B.2                                                      
Table.B.2 = cbind(traderdata[,c('Unique_ID','MoistureContent_ID','Offer','U_15','U_18','L_13','L_15','L_18','Education')],
                  traderdata[,'Education']*traderdata[,c('U_15','U_18','L_13','L_15','L_18')])
colnames(Table.B.2)[10:14] = paste(colnames(Table.B.2)[10:14],"_Ed",sep="")
write.table(Table.B.2, 'Table_B_2.txt', sep=',')
Table.B.2 <- as.data.frame(read.table("Table_B_2.txt", sep = ",", header = TRUE))

Table.B.2.Form = Offer ~ U_15 + U_18 + L_13 + L_15 + L_18 + Education + U_15_Ed + U_18_Ed + L_13_Ed + L_15_Ed + L_18_Ed           

Table.B.2 = plm(Table.B.2.Form, data = Table.B.2, model = 'random')
results.B.2 = round(coeftest(Table.B.2,vcov=vcovHC(Table.B.2,type="HC0",cluster="group")),2)
rownames(results.B.2) = c('Intercept','Unlabeled medium','Unlabeled high','Labeled low','Labeled*Medium','Labeled*High','Ed',
                          'Unlabeled medium*Ed','Unlabeled high*Ed','Labeled low*Ed','Labeled*Medium*Ed','Labeled*High*Ed')

  #B.3                                                       
Table.B.3 = cbind(traderdata[,c('Unique_ID','MoistureContent_ID','Offer','U_15','U_18','L_13','L_15','L_18','YrsExperienceSaleFarm')],
                  traderdata[,"YrsExperienceSaleFarm"]*traderdata[,c('U_15','U_18','L_13','L_15','L_18')])
colnames(Table.B.3)[10:14] = paste(colnames(Table.B.3)[10:14],"_Exp",sep="")
write.table(Table.B.3, 'Table_B_3.txt', sep=',')
Table.B.3 <- as.data.frame(read.table("Table_B_3.txt", sep = ",", header = TRUE))

Table.B.3.Form = Offer ~ U_15 + U_18 + L_13 + L_15 + L_18 + YrsExperienceSaleFarm + U_15_Exp + U_18_Exp + L_13_Exp + L_15_Exp + L_18_Exp           

Table.B.3 = plm(Table.B.3.Form, data = Table.B.3, model = 'random')
results.B.3 = round(coeftest(Table.B.3,vcov=vcovHC(Table.B.3,type="HC0",cluster="group")),2)
rownames(results.B.3) = c('Intercept','Unlabeled medium','Unlabeled high','Labeled low','Labeled*Medium','Labeled*High','Exp',
                          'Unlabeled medium*Exp','Unlabeled high*Exp','Labeled low*Exp','Labeled*Medium*Exp','Labeled*High*Exp')

  #B.4                                                       
Table.B.4 = cbind(traderdata[,c('Unique_ID','MoistureContent_ID','Offer','U_15','U_18','L_13','L_15','L_18','StoreWeeks')],
                  traderdata[,'StoreWeeks']*traderdata[,c('U_15','U_18','L_13','L_15','L_18')])
colnames(Table.B.4)[10:14] = paste(colnames(Table.B.4)[10:14],"_StoreWeeks",sep="")
write.table(Table.B.4, 'Table_B_4.txt', sep=',')
Table.B.4 <- as.data.frame(read.table("Table_B_4.txt", sep = ",", header = TRUE))

Table.B.4.Form = Offer ~ U_15 + U_18 + L_13 + L_15 + L_18 + StoreWeeks + 
  U_15_StoreWeeks + U_18_StoreWeeks + L_13_StoreWeeks + L_15_StoreWeeks + L_18_StoreWeeks           

Table.B.4 = plm(Table.B.4.Form, data = Table.B.4, model = 'random')
results.B.4 = round(coeftest(Table.B.4,vcov=vcovHC(Table.B.4,type="HC0",cluster="group")),2)
rownames(results.B.4) = c('Intercept','Unlabeled medium','Unlabeled high','Labeled low','Labeled*Medium','Labeled*High','StoreWeeks',
                          'Unlabeled medium*Weeks','Unlabeled high*Weeks','Labeled low*Weeks','Labeled*Medium*Weeks','Labeled*High*Weeks')

  #B.5                                                      
Table.B.5 = cbind(traderdata[,c('Unique_ID','MoistureContent_ID','Offer','U_15','U_18','L_13','L_15','L_18','ImpMaizeDry')],
                  traderdata[,'ImpMaizeDry']*traderdata[,c('U_15','U_18','L_13','L_15','L_18')])
colnames(Table.B.5)[10:14] = paste(colnames(Table.B.5)[10:14],"_ImpMaizeDry",sep="")
write.table(Table.B.5, 'Table_B_5.txt', sep=',')
Table.B.5 <- as.data.frame(read.table("Table_B_5.txt", sep = ",", header = TRUE))

Table.B.5.Form = Offer ~ U_15 + U_18 + L_13 + L_15 + L_18 + ImpMaizeDry + 
  U_15_ImpMaizeDry + U_18_ImpMaizeDry + L_13_ImpMaizeDry + L_15_ImpMaizeDry + L_18_ImpMaizeDry           

Table.B.5 = plm(Table.B.5.Form, data = Table.B.5, model = 'random')
results.B.5 = round(coeftest(Table.B.5,vcov=vcovHC(Table.B.5,type="HC0",cluster="group")),2)
rownames(results.B.5) = c('Intercept','Unlabeled medium','Unlabeled high','Labeled low','Labeled*Medium','Labeled*High','ImpMaizeDry',
                          'Unlabeled medium*Dry','Unlabeled high*Dry','Labeled low*Dry','Labeled*Medium*Dry','Labeled*High*Dry')

# Appendix C ----
  #C.1                                                      
Table.C.1 = cbind(consumerdata[,c('Unique_ID','MoistureContent_ID','Offer','U_15','U_18','L_13','L_15','L_18','Age')],
                  consumerdata[,'Age']*consumerdata[,c('U_15','U_18','L_13','L_15','L_18')])
colnames(Table.C.1)[10:14] = paste(colnames(Table.C.1)[10:14],"_Age",sep="")
write.table(Table.C.1, 'Table_C_1.txt', sep=',')
Table.C.1 <- as.data.frame(read.table("Table_C_1.txt", sep = ",", header = TRUE))

Table.C.1.Form = Offer ~ U_15 + U_18 + L_13 + L_15 + L_18 + Age + U_15_Age + U_18_Age + L_13_Age + L_15_Age + L_18_Age           

Table.C.1 = plm(Table.C.1.Form, data = Table.C.1, model = 'random')
results.C.1 = round(coeftest(Table.C.1,vcov=vcovHC(Table.C.1,type="HC0",cluster="group")),2)
rownames(results.C.1) = c('Intercept','Unlabeled medium','Unlabeled high','Labeled low','Labeled*Medium','Labeled*High','Age',
                          'Unlabeled medium*Age','Unlabeled high*Age','Labeled low*Age','Labeled*Medium*Age','Labeled*High*Age')

  #C.2                                                      
Table.C.2 = cbind(consumerdata[,c('Unique_ID','MoistureContent_ID','Offer','U_15','U_18','L_13','L_15','L_18','Education')],
                  consumerdata[,'Education']*consumerdata[,c('U_15','U_18','L_13','L_15','L_18')])
colnames(Table.C.2)[10:14] = paste(colnames(Table.C.2)[10:14],"_Ed",sep="")
write.table(Table.C.2, 'Table_C_2.txt', sep=',')
Table.C.2 <- as.data.frame(read.table("Table_C_2.txt", sep = ",", header = TRUE))

Table.C.2.Form = Offer ~ U_15 + U_18 + L_13 + L_15 + L_18 + Education + U_15_Ed + U_18_Ed + L_13_Ed + L_15_Ed + L_18_Ed           

Table.C.2 = plm(Table.C.2.Form, data = Table.C.2, model = 'random')
results.C.2 = round(coeftest(Table.C.2,vcov=vcovHC(Table.C.2,type="HC0",cluster="group")),2)
rownames(results.C.2) = c('Intercept','Unlabeled medium','Unlabeled high','Labeled low','Labeled*Medium','Labeled*High','Ed',
                          'Unlabeled medium*Ed','Unlabeled high*Ed','Labeled low*Ed','Labeled*Medium*Ed','Labeled*High*Ed')

  #C.3                                                       
Table.C.3 = cbind(consumerdata[,c('Unique_ID','MoistureContent_ID','Offer','U_15','U_18','L_13','L_15','L_18','YrsExperienceSaleFarm')],
                  consumerdata[,"YrsExperienceSaleFarm"]*consumerdata[,c('U_15','U_18','L_13','L_15','L_18')])
colnames(Table.C.3)[10:14] = paste(colnames(Table.C.3)[10:14],"_Exp",sep="")
write.table(Table.C.3, 'Table_C_3.txt', sep=',')
Table.C.3 <- as.data.frame(read.table("Table_C_3.txt", sep = ",", header = TRUE))

Table.C.3.Form = Offer ~ U_15 + U_18 + L_13 + L_15 + L_18 + YrsExperienceSaleFarm + U_15_Exp + U_18_Exp + L_13_Exp + L_15_Exp + L_18_Exp           

Table.C.3 = plm(Table.C.3.Form, data = Table.C.3, model = 'random')
results.C.3 = round(coeftest(Table.C.3,vcov=vcovHC(Table.C.3,type="HC0",cluster="group")),2)
rownames(results.C.3) = c('Intercept','Unlabeled medium','Unlabeled high','Labeled low','Labeled*Medium','Labeled*High','Exp',
                          'Unlabeled medium*Exp','Unlabeled high*Exp','Labeled low*Exp','Labeled*Medium*Exp','Labeled*High*Exp')

  #C.4                                                       
Table.C.4 = cbind(consumerdata[,c('Unique_ID','MoistureContent_ID','Offer','U_15','U_18','L_13','L_15','L_18','StoreWeeks')],
                  consumerdata[,'StoreWeeks']*consumerdata[,c('U_15','U_18','L_13','L_15','L_18')])
colnames(Table.C.4)[10:14] = paste(colnames(Table.C.4)[10:14],"_StoreWeeks",sep="")
write.table(Table.C.4, 'Table_C_4.txt', sep=',')
Table.C.4 <- as.data.frame(read.table("Table_C_4.txt", sep = ",", header = TRUE))

Table.C.4.Form = Offer ~ U_15 + U_18 + L_13 + L_15 + L_18 + StoreWeeks + 
  U_15_StoreWeeks + U_18_StoreWeeks + L_13_StoreWeeks + L_15_StoreWeeks + L_18_StoreWeeks           

Table.C.4 = plm(Table.C.4.Form, data = Table.C.4, model = 'random')
results.C.4 = round(coeftest(Table.C.4,vcov=vcovHC(Table.C.4,type="HC0",cluster="group")),2)
rownames(results.C.4) = c('Intercept','Unlabeled medium','Unlabeled high','Labeled low','Labeled*Medium','Labeled*High','StoreWeeks',
                          'Unlabeled medium*Weeks','Unlabeled high*Weeks','Labeled low*Weeks','Labeled*Medium*Weeks','Labeled*High*Weeks')

  #C.5                                                      
Table.C.5 = cbind(consumerdata[,c('Unique_ID','MoistureContent_ID','Offer','U_15','U_18','L_13','L_15','L_18','ImpMaizeDry')],
                  consumerdata[,'ImpMaizeDry']*consumerdata[,c('U_15','U_18','L_13','L_15','L_18')])
colnames(Table.C.5)[10:14] = paste(colnames(Table.C.5)[10:14],"_ImpMaizeDry",sep="")
write.table(Table.C.5, 'Table_C_5.txt', sep=',')
Table.C.5 <- as.data.frame(read.table("Table_C_5.txt", sep = ",", header = TRUE))

Table.C.5.Form = Offer ~ U_15 + U_18 + L_13 + L_15 + L_18 + ImpMaizeDry +
  U_15_ImpMaizeDry + U_18_ImpMaizeDry + L_13_ImpMaizeDry + L_15_ImpMaizeDry + L_18_ImpMaizeDry           

Table.C.5 = plm(Table.C.5.Form, data = Table.C.5, model = 'random')
results.C.5 = round(coeftest(Table.C.5,vcov=vcovHC(Table.C.5,type="HC0",cluster="group")),2)
rownames(results.C.5) = c('Intercept','Unlabeled medium','Unlabeled high','Labeled low','Labeled*Medium','Labeled*High','ImpMaizeDry',
                          'Unlabeled medium*Dry','Unlabeled high*Dry','Labeled low*Dry','Labeled*Medium*Dry','Labeled*High*Dry')

# Appendix D ----
Table.D = cbind(alldata,(alldata$U_15 + alldata$U_18),(alldata$L_15 + alldata$L_18))
colnames(Table.D)[(ncol(Table.D)-1):ncol(Table.D)] = c('U_Combo','L_Combo')

  #D.1                                                       
Table.D.1 = cbind(Table.D[,c('Unique_ID','MoistureContent_ID','Offer','U_Combo','L_13','L_Combo','Age')],
                  Table.D[,"Age"]*Table.D[,c('U_Combo','L_13','L_Combo')])
colnames(Table.D.1)[8:10] = paste(colnames(Table.D.1)[8:10],"_Age",sep="")
write.table(Table.D.1,'Table_D_1.txt', sep=',')
Table.D.1 <- as.data.frame(read.table('Table_D_1.txt', sep = ",", header = TRUE))

Table.D.1.Form = Offer ~ U_Combo + L_13 + L_Combo + Age + U_Combo_Age + L_13_Age + L_Combo_Age         

Table.D.1 = plm(Table.D.1.Form, data = Table.D.1, model = 'random')
results.D.1 = round(coeftest(Table.D.1,vcov=vcovHC(Table.D.1,type="HC0",cluster="group")),2)
rownames(results.D.1) = c('Intercept','Unlabeled pooled','Labeled low','Labeled*Pooled','Age',
                          'Unlabeled pooled*Age','Labeled low*Age','Labeled*Pooled*Age')

  #D.2                                                      
Table.D.2 = cbind(Table.D[,c('Unique_ID','MoistureContent_ID','Offer','U_Combo','L_13','L_Combo','Education')],
                  Table.D[,'Education']*Table.D[,c('U_Combo','L_13','L_Combo')])
colnames(Table.D.2)[8:10] = paste(colnames(Table.D.2)[8:10],"_Ed",sep="")
write.table(Table.D.2, 'Table_D_2.txt', sep=',')
Table.D.2 <- as.data.frame(read.table("Table_D_2.txt", sep = ",", header = TRUE))

Table.D.2.Form = Offer ~ U_Combo + L_13 + L_Combo + Education + U_Combo_Ed + L_13_Ed + L_Combo_Ed           

Table.D.2 = plm(Table.D.2.Form, data = Table.D.2, model = 'random')
results.D.2 = round(coeftest(Table.D.2,vcov=vcovHC(Table.D.2,type="HC0",cluster="group")),2)
rownames(results.D.2) = c('Intercept','Unlabeled pooled','Labeled low','Labeled*Pooled','Education',
                          'Unlabeled pooled*Ed','Labeled low*Ed','Labeled*Pooled*Ed')

  #D.3                                                      
Table.D.3 = cbind(Table.D[,c('Unique_ID','MoistureContent_ID','Offer','U_Combo','L_13','L_Combo','YrsExperienceSaleFarm')],
                  Table.D[,"YrsExperienceSaleFarm"]*Table.D[,c('U_Combo','L_13','L_Combo')])
colnames(Table.D.3)[8:10] = paste(colnames(Table.D.3)[8:10],"_Exp",sep="")
write.table(Table.D.3, 'Table_D_3.txt', sep=',')
Table.D.3 <- as.data.frame(read.table("Table_D_3.txt", sep = ",", header = TRUE))

Table.D.3.Form = Offer ~ U_Combo + L_13 + L_Combo + YrsExperienceSaleFarm + U_Combo_Exp + L_13_Exp + L_Combo_Exp           

Table.D.3 = plm(Table.D.3.Form, data = Table.D.3, model = 'random')
results.D.3 = round(coeftest(Table.D.3,vcov=vcovHC(Table.D.3,type="HC0",cluster="group")),2)
rownames(results.D.3) = c('Intercept','Unlabeled pooled','Labeled low','Labeled*Pooled','Experience',
                          'Unlabeled pooled*Exp','Labeled low*Exp','Labeled*Pooled*Exp')

  #D.4                                                       
Table.D.4 = cbind(Table.D[,c('Unique_ID','MoistureContent_ID','Offer','U_Combo','L_13','L_Combo','StoreWeeks')],
                  Table.D[,"StoreWeeks"]*Table.D[,c('U_Combo','L_13','L_Combo')])
colnames(Table.D.4)[8:10] = paste(colnames(Table.D.4)[8:10],"_StoreWeeks",sep="")
write.table(Table.D.4, 'Table_D_4.txt', sep=',')
Table.D.4 <- as.data.frame(read.table("Table_D_4.txt", sep = ",", header = TRUE))

Table.D.4.Form = Offer ~ U_Combo + L_13 + L_Combo + StoreWeeks + U_Combo_StoreWeeks + L_13_StoreWeeks + L_Combo_StoreWeeks           

Table.D.4 = plm(Table.D.4.Form, data = Table.D.4, model = 'random')
results.D.4 = round(coeftest(Table.D.4,vcov=vcovHC(Table.D.4,type="HC0",cluster="group")),2)
rownames(results.D.4) = c('Intercept','Unlabeled pooled','Labeled low','Labeled*Pooled','Storeweeks',
                          'Unlabeled pooled*Weeks','Labeled low*Weeks','Labeled*Pooled*Weeks')

  #D.5                                                      
Table.D.5 = cbind(Table.D[,c('Unique_ID','MoistureContent_ID','Offer','U_Combo','L_13','L_Combo','ImpMaizeDry')],
                  Table.D[,"ImpMaizeDry"]*Table.D[,c('U_Combo','L_13','L_Combo')])
colnames(Table.D.5)[8:10] = paste(colnames(Table.D.5)[8:10],"_ImpMaizeDry",sep="")
write.table(Table.D.5, 'Table_D_5.txt', sep=',')
Table.D.5 <- as.data.frame(read.table("Table_D_5.txt", sep = ",", header = TRUE))

Table.D.5.Form = Offer ~ U_Combo + L_13 + L_Combo + ImpMaizeDry + U_Combo_ImpMaizeDry + L_13_ImpMaizeDry + L_Combo_ImpMaizeDry           

Table.D.5 = plm(Table.D.5.Form, data = Table.D.5, model = 'random')
results.D.5 = round(coeftest(Table.D.5,vcov=vcovHC(Table.D.5,type="HC0",cluster="group")),2)
rownames(results.D.5) = c('Intercept','Unlabeled pooled','Labeled low','Labeled*Pooled','ImpMaizeDry',
                          'Unlabeled pooled*Dry','Labeled low*Dry','Labeled*Pooled*Dry')

# Appendix E ----
Table.E = subset(Table.D, Trader==1)

  #E.1                                                       
Table.E.1 = cbind(Table.E[,c('Unique_ID','MoistureContent_ID','Offer','U_Combo','L_13','L_Combo','Age')],
                  Table.E[,"Age"]*Table.E[,c('U_Combo','L_13','L_Combo')])
colnames(Table.E.1)[8:10] = paste(colnames(Table.E.1)[8:10],"_Age",sep="")
write.table(Table.E.1,'Table_E_1.txt', sep=',')
Table.E.1 <- as.data.frame(read.table('Table_E_1.txt', sep = ",", header = TRUE))

Table.E.1.Form = Offer ~ U_Combo + L_13 + L_Combo + Age + U_Combo_Age + L_13_Age + L_Combo_Age         

Table.E.1 = plm(Table.E.1.Form, data = Table.E.1, model = 'random')
results.E.1 = round(coeftest(Table.E.1,vcov=vcovHC(Table.E.1,type="HC0",cluster="group")),2)
rownames(results.E.1) = c('Intercept','Unlabeled pooled','Labeled low','Labeled*Pooled','Age',
                          'Unlabeled pooled*Age','Labeled low*Age','Labeled*Pooled*Age')

  #E.2                                                      
Table.E.2 = cbind(Table.E[,c('Unique_ID','MoistureContent_ID','Offer','U_Combo','L_13','L_Combo','Education')],
                  Table.E[,'Education']*Table.E[,c('U_Combo','L_13','L_Combo')])
colnames(Table.E.2)[8:10] = paste(colnames(Table.E.2)[8:10],"_Ed",sep="")
write.table(Table.E.2, 'Table_E_2.txt', sep=',')
Table.E.2 <- as.data.frame(read.table("Table_E_2.txt", sep = ",", header = TRUE))

Table.E.2.Form = Offer ~ U_Combo + L_13 + L_Combo + Education + U_Combo_Ed + L_13_Ed + L_Combo_Ed           

Table.E.2 = plm(Table.E.2.Form, data = Table.E.2, model = 'random')
results.E.2 = round(coeftest(Table.E.2,vcov=vcovHC(Table.E.2,type="HC0",cluster="group")),2)
rownames(results.E.2) = c('Intercept','Unlabeled pooled','Labeled low','Labeled*Pooled','Education',
                          'Unlabeled pooled*Ed','Labeled low*Ed','Labeled*Pooled*Ed')

  #E.3                                                      
Table.E.3 = cbind(Table.E[,c('Unique_ID','MoistureContent_ID','Offer','U_Combo','L_13','L_Combo','YrsExperienceSaleFarm')],
                  Table.E[,"YrsExperienceSaleFarm"]*Table.E[,c('U_Combo','L_13','L_Combo')])
colnames(Table.E.3)[8:10] = paste(colnames(Table.E.3)[8:10],"_Exp",sep="")
write.table(Table.E.3, 'Table_E_3.txt', sep=',')
Table.E.3 <- as.data.frame(read.table("Table_E_3.txt", sep = ",", header = TRUE))

Table.E.3.Form = Offer ~ U_Combo + L_13 + L_Combo + YrsExperienceSaleFarm + U_Combo_Exp + L_13_Exp + L_Combo_Exp           

Table.E.3 = plm(Table.E.3.Form, data = Table.E.3, model = 'random')
results.E.3 = round(coeftest(Table.E.3,vcov=vcovHC(Table.E.3,type="HC0",cluster="group")),2)
rownames(results.E.3) = c('Intercept','Unlabeled pooled','Labeled low','Labeled*Pooled','Experience',
                          'Unlabeled pooled*Exp','Labeled low*Exp','Labeled*Pooled*Exp')

  #E.4                                                       
Table.E.4 = cbind(Table.E[,c('Unique_ID','MoistureContent_ID','Offer','U_Combo','L_13','L_Combo','StoreWeeks')],
                  Table.E[,"StoreWeeks"]*Table.E[,c('U_Combo','L_13','L_Combo')])
colnames(Table.E.4)[8:10] = paste(colnames(Table.E.4)[8:10],"_StoreWeeks",sep="")
write.table(Table.E.4, 'Table_E_4.txt', sep=',')
Table.E.4 <- as.data.frame(read.table("Table_E_4.txt", sep = ",", header = TRUE))

Table.E.4.Form = Offer ~ U_Combo + L_13 + L_Combo + StoreWeeks + U_Combo_StoreWeeks + L_13_StoreWeeks + L_Combo_StoreWeeks           

Table.E.4 = plm(Table.E.4.Form, data = Table.E.4, model = 'random')
results.E.4 = round(coeftest(Table.E.4,vcov=vcovHC(Table.E.4,type="HC0",cluster="group")),2)
rownames(results.E.4) = c('Intercept','Unlabeled pooled','Labeled low','Labeled*Pooled','Storeweeks',
                          'Unlabeled pooled*Weeks','Labeled low*Weeks','Labeled*Pooled*Weeks')

  #E.5                                                      
Table.E.5 = cbind(Table.E[,c('Unique_ID','MoistureContent_ID','Offer','U_Combo','L_13','L_Combo','ImpMaizeDry')],
                  Table.E[,"ImpMaizeDry"]*Table.E[,c('U_Combo','L_13','L_Combo')])
colnames(Table.E.5)[8:10] = paste(colnames(Table.E.5)[8:10],"_ImpMaizeDry",sep="")
write.table(Table.E.5, 'Table_E_5.txt', sep=',')
Table.E.5 <- as.data.frame(read.table("Table_E_5.txt", sep = ",", header = TRUE))

Table.E.5.Form = Offer ~ U_Combo + L_13 + L_Combo + ImpMaizeDry + U_Combo_ImpMaizeDry + L_13_ImpMaizeDry + L_Combo_ImpMaizeDry           

Table.E.5 = plm(Table.E.5.Form, data = Table.E.5, model = 'random')
results.E.5 = round(coeftest(Table.E.5,vcov=vcovHC(Table.E.5,type="HC0",cluster="group")),2)
rownames(results.E.5) = c('Intercept','Unlabeled pooled','Labeled low','Labeled*Pooled','ImpMaizeDry',
                          'Unlabeled pooled*Dry','Labeled low*Dry','Labeled*Pooled*Dry')

# Appendix F ----
Table.F = subset(Table.D, Trader==0)

#F.1                                                       
Table.F.1 = cbind(Table.F[,c('Unique_ID','MoistureContent_ID','Offer','U_Combo','L_13','L_Combo','Age')],
                  Table.F[,"Age"]*Table.F[,c('U_Combo','L_13','L_Combo')])
colnames(Table.F.1)[8:10] = paste(colnames(Table.F.1)[8:10],"_Age",sep="")
write.table(Table.F.1,'Table_F_1.txt', sep=',')
Table.F.1 <- as.data.frame(read.table('Table_F_1.txt', sep = ",", header = TRUE))

Table.F.1.Form = Offer ~ U_Combo + L_13 + L_Combo + Age + U_Combo_Age + L_13_Age + L_Combo_Age         

Table.F.1 = plm(Table.F.1.Form, data = Table.F.1, model = 'random')
results.F.1 = round(coeftest(Table.F.1,vcov=vcovHC(Table.F.1,type="HC0",cluster="group")),2)
rownames(results.F.1) = c('Intercept','Unlabeled pooled','Labeled low','Labeled*Pooled','Age',
                          'Unlabeled pooled*Age','Labeled low*Age','Labeled*Pooled*Age')

#F.2                                                      
Table.F.2 = cbind(Table.F[,c('Unique_ID','MoistureContent_ID','Offer','U_Combo','L_13','L_Combo','Education')],
                  Table.F[,'Education']*Table.F[,c('U_Combo','L_13','L_Combo')])
colnames(Table.F.2)[8:10] = paste(colnames(Table.F.2)[8:10],"_Ed",sep="")
write.table(Table.F.2, 'Table_F_2.txt', sep=',')
Table.F.2 <- as.data.frame(read.table("Table_F_2.txt", sep = ",", header = TRUE))

Table.F.2.Form = Offer ~ U_Combo + L_13 + L_Combo + Education + U_Combo_Ed + L_13_Ed + L_Combo_Ed           

Table.F.2 = plm(Table.F.2.Form, data = Table.F.2, model = 'random')
results.F.2 = round(coeftest(Table.F.2,vcov=vcovHC(Table.F.2,type="HC0",cluster="group")),2)
rownames(results.F.2) = c('Intercept','Unlabeled pooled','Labeled low','Labeled*Pooled','Education',
                          'Unlabeled pooled*Ed','Labeled low*Ed','Labeled*Pooled*Ed')

#F.3                                                      
Table.F.3 = cbind(Table.F[,c('Unique_ID','MoistureContent_ID','Offer','U_Combo','L_13','L_Combo','YrsExperienceSaleFarm')],
                  Table.F[,"YrsExperienceSaleFarm"]*Table.F[,c('U_Combo','L_13','L_Combo')])
colnames(Table.F.3)[8:10] = paste(colnames(Table.F.3)[8:10],"_Exp",sep="")
write.table(Table.F.3, 'Table_F_3.txt', sep=',')
Table.F.3 <- as.data.frame(read.table("Table_F_3.txt", sep = ",", header = TRUE))

Table.F.3.Form = Offer ~ U_Combo + L_13 + L_Combo + YrsExperienceSaleFarm + U_Combo_Exp + L_13_Exp + L_Combo_Exp           

Table.F.3 = plm(Table.F.3.Form, data = Table.F.3, model = 'random')
results.F.3 = round(coeftest(Table.F.3,vcov=vcovHC(Table.F.3,type="HC0",cluster="group")),2)
rownames(results.F.3) = c('Intercept','Unlabeled pooled','Labeled low','Labeled*Pooled','Experience',
                          'Unlabeled pooled*Exp','Labeled low*Exp','Labeled*Pooled*Exp')

#F.4                                                       
Table.F.4 = cbind(Table.F[,c('Unique_ID','MoistureContent_ID','Offer','U_Combo','L_13','L_Combo','StoreWeeks')],
                  Table.F[,"StoreWeeks"]*Table.F[,c('U_Combo','L_13','L_Combo')])
colnames(Table.F.4)[8:10] = paste(colnames(Table.F.4)[8:10],"_StoreWeeks",sep="")
write.table(Table.F.4, 'Table_F_4.txt', sep=',')
Table.F.4 <- as.data.frame(read.table("Table_F_4.txt", sep = ",", header = TRUE))

Table.F.4.Form = Offer ~ U_Combo + L_13 + L_Combo + StoreWeeks + U_Combo_StoreWeeks + L_13_StoreWeeks + L_Combo_StoreWeeks           

Table.F.4 = plm(Table.F.4.Form, data = Table.F.4, model = 'random')
results.F.4 = round(coeftest(Table.F.4,vcov=vcovHC(Table.F.4,type="HC0",cluster="group")),2)
rownames(results.F.4) = c('Intercept','Unlabeled pooled','Labeled low','Labeled*Pooled','Storeweeks',
                          'Unlabeled pooled*Weeks','Labeled low*Weeks','Labeled*Pooled*Weeks')

#F.5                                                      
Table.F.5 = cbind(Table.F[,c('Unique_ID','MoistureContent_ID','Offer','U_Combo','L_13','L_Combo','ImpMaizeDry')],
                  Table.F[,"ImpMaizeDry"]*Table.F[,c('U_Combo','L_13','L_Combo')])
colnames(Table.F.5)[8:10] = paste(colnames(Table.F.5)[8:10],"_ImpMaizeDry",sep="")
write.table(Table.F.5, 'Table_F_5.txt', sep=',')
Table.F.5 <- as.data.frame(read.table("Table_F_5.txt", sep = ",", header = TRUE))

Table.F.5.Form = Offer ~ U_Combo + L_13 + L_Combo + ImpMaizeDry + U_Combo_ImpMaizeDry + L_13_ImpMaizeDry + L_Combo_ImpMaizeDry           

Table.F.5 = plm(Table.F.5.Form, data = Table.F.5, model = 'random')
results.F.5 = round(coeftest(Table.F.5,vcov=vcovHC(Table.F.5,type="HC0",cluster="group")),2)
rownames(results.F.5) = c('Intercept','Unlabeled pooled','Labeled low','Labeled*Pooled','ImpMaizeDry',
                          'Unlabeled pooled*Dry','Labeled low*Dry','Labeled*Pooled*Dry')