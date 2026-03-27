################################################################################
########################// COBBLE REEF RESTORATION //#############################
################################################################################
#################//SCRIPT 1.2 MODEL POTTING + COMPARISONS//#####################

#open the datasets (baited data)
dfbo_group<-(readRDS("abundance_data_04.07"))
View(dfbo_group)
data_cod<-BRUVS_cod
data_her<-BRUVS_her
data_lab<-BRUVS_lab
data_gob<-BRUVS_gob
data_crab<-BRUVS_crab
data_flat<-BRUVS_flat

#Install necessary packages
if(!require("DHARMa")) {
  install.packages("DHARMa")
  library("DHARMa")
}
if(!require("emmeans")) {
  install.packages("emmeans")
  library("emmeans")
}
  if(!require("glmmTMB")) {
    install.packages("glmmTMB")
    library("glmmTMB")
  }
if(!require("ggplot2")) {
  install.packages("ggplot2")
  library("ggplot2")
}
install.packages("openslsx")
library(openxlsx)
install.packages("writexl")
library(writexl)
install.packages("glmmTMB")

install.packages("rio")
install.packages("xlsx")
library(rio)
library(xlsx)
export(data, "exportname.xlsx")

#MODELS 
#COD
C23<-glmmTMB(Cod~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_group_log+Macroalgae+(1|Site),
             data=BRUVS_cod,
             family=nbinom2)
summary(C23)
#HERRING
H22<-glmmTMB(Herrings~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_group_log+(1|Site),
             data=BRUVS_her,
             family=nbinom1)
summary(H22)
#WRASSE WITHOUT OUTLIERS
modified_WRASSE_dfbo_group <- BRUVS_lab[BRUVS_lab$Wrasse != 26 & BRUVS_lab$Wrasse != 20 & BRUVS_lab$Wrasse != 15, ]

W24<-glmmTMB(Wrasse~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_group_log+Macroalgae+(1|Site),
             data=BRUVS_lab,
             family=nbinom2)
summary(W24)
#GOBY
G21<-glmmTMB(Goby~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_group_log+Macroalgae+(1|Site),
             data=BRUVS_gob,
             family=nbinom2)
summary(G21)
#SHORE CRAB
SC22<-glmmTMB(Shore_crab~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_group_log+Macroalgae+(1|Site),
              data=BRUVS_crab,
              family=nbinom1)
summary(SC22)
#FLATFISH
F24<-glmmTMB(Flatfish~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_group_log+Macroalgae+(1|Site),
             data=BRUVS_flat,
             family=nbinom1)
summary(F24)
#Conducting estimated marginal response and pairwise comparisons
#COD=GADUS MORHUA --> EMMEANS, SUMMARY + CONTRASTS, EXPORT AS DATAFRAME 
library(emmeans)
emC23<-emmeans(C23,specs=pairwise~Year:Treatment,type="response",combine=TRUE)
emC23
contrasts_C23<-as.data.frame(summary(emC23$contrasts))
View(contrasts_C23)
responses_C23<-as.data.frame(summary(emC23$emmeans))
View(responses_C23)
new_emC23<-responses_C23
#CHECK THE STRUCTURE AND ADD THE YEAR UMERIC COLUMN
str(new_emC23)
new_emC23$Year_num<-new_emC23$Year
new_emC23$Year_num<- as.numeric(as.character(new_emC23$Year_num))
file_path <- "/Users/elisenda/Desktop/COBBLE REEF PROJECT 23/DATA/SCRIPTS/EMMEANS OUTPUTS/new_emC23_good.xlsx"
write_xlsx(new_emC23, file_path)
export(new_emC23, "BRU_C23_emmeans.xlsx")
#EXPORT CONTRASTS
library(openxlsx)
library(writexl)
library(readxl)
file_path <- "/Users/elisenda/Desktop/COBBLE_2023/DATA/SCRIPTS/EMMEANS OUTPUTS/CONTRASTS/contrasts_C23_good.xlsx"
write_xlsx(contrasts_C23, file_path) 
export(contrasts_C23, "BRU_C23_contrasts.xlsx")
#HERRING=CLUPEA HARENGUS --> EMMEANS, SUMMARY + CONTRASTS, EXPORT AS DATAFRAME 
emH22<-emmeans(H22,specs=pairwise~Year:Treatment,type="response",combine=TRUE)
emH22
contrasts_H22<-as.data.frame(summary(emH22$contrasts))
View(contrasts_H22)
library(write_xlsx)
file_path <- "/Users/elisenda/Desktop/COBBLE REEF PROJECT 23/DATA/SCRIPTS/EMMEANS OUTPUTS/CONTRASTS/contrasts_H22_good.xlsx"
write_xlsx(contrasts_H22, file_path) 
responses_H22<-as.data.frame(summary(emH22$emmeans))
View(responses_H22)
new_emH22<-responses_H22
#CHECK THE STRUCTURE AND ADD THE YEAR NUMERIC COLUMN
str(new_emH22)
new_emH22$Year_num<-new_emH22$Year
new_emH22$Year_num<- as.numeric(as.character(new_emH22$Year_num))
file_path <- "/Users/elisenda/Desktop/COBBLE REEF PROJECT 23/DATA/SCRIPTS/EMMEANS OUTPUTS/new_emH22_good.xlsx"
write_xlsx(new_emH22, file_path)
saveRDS(contrasts_H22,file="BRU_H22_contrasts14.10",ascii=TRUE)
saveRDS(new_emH22,file="BRU_H22_emmeans14.10",ascii=TRUE)
write_xlsx(new_emH22, path = "BRU_H22_emmeans14.10.xlsx")
write_xlsx(contrasts_H22, path = "BRU_H22_contrasts14.10.xlsx")
###EXPORT RESULTS
export(new_emH22, "BRU_H22_emmeans.xlsx")
export(contrasts_H22, "BRU_H22_contrasts.xlsx")
#GOLDSINNY WRASSE=CTENOLABRUS RUPESTRIS--> EMMEANS, SUMMARY + CONTRASTS, EXPORT AS DATAFRAME 
emW24<-emmeans(W24,specs=pairwise~Year:Treatment, type="response",combine=TRUE)
emW24
contrasts_W24<-as.data.frame(summary(emW24$contrasts))
View(contrasts_W24)
file_path <- "/Users/elisenda/Desktop/COBBLE REEF PROJECT 23/DATA/SCRIPTS/EMMEANS OUTPUTS/CONTRASTS/contrasts_W24_good_16.08.xlsx"
write_xlsx(contrasts_W24, file_path) 
responses_W24<-as.data.frame(summary(emW24$emmeans))
View(responses_W24)
new_emW24<-responses_W24
#CHECK THE STRUCTURE AND ADD THE YEAR NUMERIC COLUMN
str(new_emW24)
new_emW24$Year_num<-new_emW24$Year
new_emW24$Year_num<- as.numeric(as.character(new_emW24$Year_num))
file_path <- "/Users/elisenda/Desktop/COBBLE REEF PROJECT 23/DATA/SCRIPTS/EMMEANS OUTPUTS/new_emW24_good_16.08.xlsx"
write_xlsx(new_emW24, file_path)
#TWO SPOTTED GOBY=GOBIUSCUS FLAVSCENS --> EMMEANS, SUMMARY + CONTRASTS, EXPORT AS DATAFRAME 
emG21<-emmeans(G21,specs=pairwise~Year:Treatment,type="response",combine=TRUE)
emG21
contrasts_G21<-as.data.frame(summary(emG21$contrasts))
View(contrasts_G21)
file_path <- "/Users/elisenda/Desktop/COBBLE REEF PROJECT 23/DATA/SCRIPTS/EMMEANS OUTPUTS/CONTRASTS/contrasts_G21_good.xlsx"
write_xlsx(contrasts_G21, file_path) 
responses_G21<-as.data.frame(summary(emG21$emmeans))
View(responses_G21)
new_emG21<-responses_G21
#CHECK THE STRUCTURE AND ADD THE YEAR NUMERIC COLUMN
str(new_emG21)
new_emG21$Year_num<-new_emG21$Year
new_emG21$Year_num<- as.numeric(as.character(new_emG21$Year_num))
file_path <- "/Users/elisenda/Desktop/COBBLE REEF PROJECT 23/DATA/SCRIPTS/EMMEANS OUTPUTS/new_emG21_good.xlsx"
write_xlsx(new_emG21, file_path)
#SHORE CRAB CARCINUS MAENAS --> EMMEANS, SUMMARY + CONTRASTS, EXPORT AS DATAFRAME 
emSC22<-emmeans(SC22,specs=pairwise~Year:Treatment,type="response",combine=TRUE)
emSC22
contrasts_SC22<-as.data.frame(summary(emSC22$contrasts))
View(contrasts_SC22)
file_path <- "/Users/elisenda/Desktop/COBBLE REEF PROJECT 23/DATA/SCRIPTS/EMMEANS OUTPUTS/CONTRASTS/contrasts_SC22_good.xlsx"
write_xlsx(contrasts_SC22, file_path) 
responses_SC22<-as.data.frame(summary(emSC22$emmeans))
View(responses_SC22)
new_emSC22<-responses_SC22
#CHECK THE STRUCTURE AND ADD THE YEAR NUMERIC COLUMN
str(new_emSC22)
new_emSC22$Year_num<-new_emSC22$Year
new_emSC22$Year_num<- as.numeric(as.character(new_emSC22$Year_num))
file_path <- "/Users/elisenda/Desktop/COBBLE REEF PROJECT 23/DATA/SCRIPTS/EMMEANS OUTPUTS/new_emSC22_good.xlsx"
write_xlsx(new_emSC22, file_path)
#FLATFISH --> EMMEANS, SUMMARY + CONTRASTS, EXPORT AS DATAFRAME 
emF24<-emmeans(F24,specs=pairwise~Year:Treatment,type="response",combine=TRUE)
emF24
contrasts_F24<-as.data.frame(summary(emF24$contrasts))
View(contrasts_F24)
file_path <- "/Users/elisenda/Desktop/COBBLE REEF PROJECT 23/DATA/SCRIPTS/EMMEANS OUTPUTS/CONTRASTS/contrasts_F24_good.xlsx"
write_xlsx(contrasts_F24, file_path) 
responses_F24<-as.data.frame(summary(emF24$emmeans))
View(responses_F24)
new_emF24<-responses_F24
#LET'S CHECK THE STRUCTURE AND ADD THE YEAR NUMERIC COLUMN
str(new_emF24)
new_emF24$Year_num<-new_emF24$Year
new_emF24$Year_num<- as.numeric(as.character(new_emF24$Year_num))
file_path <- "/Users/elisenda/Desktop/COBBLE REEF PROJECT 23/DATA/SCRIPTS/EMMEANS OUTPUTS/new_emF24_good.xlsx"
write_xlsx(new_emF24, file_path)
###############// Slope comparison (interaction contrasts) //###################
## Atlantic cod
interactionC23<-contrast(emC23, interaction="pairwise",type="response")
interactionC23
emtrendsC23<-as.data.frame(interactionC23)
export(emtrendsC23, "BRU_C23_emtrends.xlsx")
## Atlantic herring
interactionH22<-contrast(emH22, interaction="pairwise",type="response")
interactionH22
emtrendsH22<-as.data.frame(interactionH22)
export(emtrendsH22, "BRU_H22_emtrends.xlsx")
saveRDS(emtrendsH22,file="BRU_H22_emtrends",ascii=TRUE)
write_xlsx(emtrendsH22, path = "BRU_H22_emtrends.xlsx")
## Goldsinny wrasse
interactionW24<-contrast(emW24, interaction="pairwise",type="response")
interactionW24
emtrendsW24<-as.data.frame(interactionW24)
file_path <- "/Users/elisenda/Desktop/COBBLE REEF PROJECT 23/DATA/SCRIPTS/EMMEANS OUTPUTS/interactionW24_16.08.2023.xlsx"
write_xlsx(emtrendsW24, file_path)
## Two-spotted goby
interactionG21<-contrast(emG21, interaction="pairwise",type="response")
interactionG21
emtrendsG21<-as.data.frame(interactionG21)
file_path <- "/Users/elisenda/Desktop/COBBLE REEF PROJECT 23/DATA/SCRIPTS/EMMEANS OUTPUTS/interactionG21.xlsx"
write_xlsx(emtrendsG21, file_path)
## Shorecrab
interactionSC22<-contrast(emSC22, interaction="pairwise",type="response")
interactionSC22
emtrendsSC22<-as.data.frame(interactionSC22)
file_path <- "/Users/elisenda/Desktop/COBBLE REEF PROJECT 23/DATA/SCRIPTS/EMMEANS OUTPUTS/interactionSC22.xlsx"
write_xlsx(emtrendsSC22, file_path)
## Flatfish
interactionF24<-contrast(emF24, interaction="pairwise",type="response")
interactionF24
emtrendsF24<-as.data.frame(interactionF24)
file_path <- "/Users/elisenda/Desktop/COBBLE REEF PROJECT 23/DATA/SCRIPTS/EMMEANS OUTPUTS/interactionF24.xlsx"
write_xlsx(emtrendsF24, file_path)
### Plots of the estimated marginal means 
library(ggplot2)
library(cowplot)
G.morhua_PLOT_REV<-ggplot()+
  scale_color_manual(values=c("#FF524B", "#7CA1CC", "#1F449C"))+
  scale_fill_manual(values = c("#FF524B", "#7CA1CC", "#1F449C"))+
  geom_point(data=new_emC23,aes(x=Year_num, y=response, group=Treatment, color=Treatment, shape = Treatment),size=3,position=position_dodge(0.5))+
  geom_errorbar(data=new_emC23,aes(x=Year_num, y=response,ymin=asymp.LCL, ymax=asymp.UCL,group=Treatment, color=Treatment), width=0.2, size=0.7,
                position=position_dodge(0.5),alpha=0.7)+
  geom_vline(xintercept=2017.5, linetype = "dashed", color = "black", linewidth = 0.8)+
  labs(title="A) Atlantic cod", x = "Year", y = "MaxN", fill = "Treatment")+
  theme(panel.background = element_rect(fill = "white"))+
  theme(panel.background = element_rect(fill = "white"))+
  theme(panel.border = element_rect(fill=NA,colour="black",size=0.4))+
  theme(legend.key = element_rect(fill="white"),legend.position = "none")+
  scale_x_continuous(breaks = c(2017, 2018, 2021))
G.morhua_PLOT_REV
C.harengus_PLOT_REV<-ggplot()+
  scale_color_manual(values=c("#FF524B", "#7CA1CC", "#1F449C"))+
  scale_fill_manual(values = c("#FF524B", "#7CA1CC", "#1F449C"))+
  geom_point(data=new_emH22,aes(x=Year_num, y=response, group=Treatment, color=Treatment, shape = Treatment),size=3,position=position_dodge(0.5))+
  geom_errorbar(data=new_emH22,aes(x=Year_num, y=response,ymin=asymp.LCL, ymax=asymp.UCL,group=Treatment, color=Treatment), width=0.2, size=0.7,
                position=position_dodge(0.5),alpha=0.7)+
  geom_vline(xintercept=2017.5, linetype = "dashed", color = "black", linewidth = 0.8)+
  labs(title="B) Atlantic herring", x = "Year", y = "MaxN", fill = "Treatment")+
  theme(panel.background = element_rect(fill = "white"))+
  theme(panel.background = element_rect(fill = "white"))+
  theme(panel.border = element_rect(fill=NA,colour="black",size=0.4))+
  theme(legend.key = element_rect(fill="white"),legend.position = "none")+
  scale_x_continuous(breaks = c(2017, 2018, 2021))
C.harengus_PLOT_REV
C.rupestris_PLOT_REV<-ggplot()+
  scale_color_manual(values=c("#FF524B", "#7CA1CC", "#1F449C"))+
  scale_fill_manual(values = c("#FF524B", "#7CA1CC", "#1F449C"))+
  geom_point(data=new_emW24,aes(x=Year_num, y=response, group=Treatment, color=Treatment, shape = Treatment),size=3,position=position_dodge(0.5))+
  geom_errorbar(data=new_emW24,aes(x=Year_num, y=response,ymin=asymp.LCL, ymax=asymp.UCL,group=Treatment, color=Treatment), width=0.2, size=0.7,
                position=position_dodge(0.5),alpha=0.7)+
  geom_vline(xintercept=2017.5, linetype = "dashed", color = "black", linewidth = 0.8)+
  labs(title="C) Goldsinny wrasse", x = "Year", y = "MaxN", fill = "Treatment")+
  theme(panel.background = element_rect(fill = "white"))+
  theme(panel.background = element_rect(fill = "white"))+
  theme(panel.border = element_rect(fill=NA,colour="black",size=0.4))+
  theme(legend.key = element_rect(fill="white"),legend.position = "none")+
  scale_x_continuous(breaks = c(2017, 2018, 2021))
C.rupestris_PLOT_REV
G.flavescens_PLOT_REV<-ggplot()+
  scale_color_manual(values=c("#FF524B", "#7CA1CC", "#1F449C"))+
  scale_fill_manual(values = c("#FF524B", "#7CA1CC", "#1F449C"))+  
  geom_point(data=new_emG21,aes(x=Year_num, y=response, group=Treatment, color=Treatment, shape = Treatment),size=3,position=position_dodge(0.5))+
  geom_errorbar(data=new_emG21,aes(x=Year_num, y=response,ymin=asymp.LCL, ymax=asymp.UCL,group=Treatment, color=Treatment), width=0.2, size=0.7,
                position=position_dodge(0.5),alpha=0.7)+
  geom_vline(xintercept=2017.5, linetype = "dashed", color = "black", linewidth = 0.8)+
  labs(title="D) Two-spotted goby", x = "Year", y = "MaxN", fill = "Treatment")+
  theme(panel.background = element_rect(fill = "white"))+
  theme(panel.background = element_rect(fill = "white"))+
  theme(panel.border = element_rect(fill=NA,colour="black",size=0.4))+
  theme(legend.key = element_rect(fill="white"),legend.position = "none")+
  scale_x_continuous(breaks = c(2017, 2018, 2021))
G.flavescens_PLOT_REV
C.maenas_PLOT_REV<-ggplot()+
  scale_color_manual(values=c("#FF524B", "#7CA1CC", "#1F449C"))+
  scale_fill_manual(values = c("#FF524B", "#7CA1CC", "#1F449C"))+
  geom_point(data=new_emSC22,aes(x=Year_num, y=response, group=Treatment, color=Treatment, shape = Treatment),size=3,position=position_dodge(0.5))+
  geom_errorbar(data=new_emSC22,aes(x=Year_num, y=response,ymin=asymp.LCL, ymax=asymp.UCL,group=Treatment, color=Treatment), width=0.2, size=0.7,
                position=position_dodge(0.5),alpha=0.7)+
  geom_vline(xintercept=2017.5, linetype = "dashed", color = "black", linewidth = 0.8)+
  labs(title="E) Shorecrab", x = "Year", y = "MaxN", fill = "Treatment")+
  theme(panel.background = element_rect(fill = "white"))+
  theme(panel.background = element_rect(fill = "white"))+
  theme(panel.border = element_rect(fill=NA,colour="black",size=0.4))+
  theme(legend.key = element_rect(fill="white"),legend.position = "bottom")+
  scale_x_continuous(breaks = c(2017, 2018, 2021))
C.maenas_PLOT_REV
Flatfish_PLOT_REV<-ggplot()+
  scale_color_manual(values=c("#FF524B", "#7CA1CC", "#1F449C"))+
  scale_fill_manual(values = c("#FF524B", "#7CA1CC", "#1F449C"))+
  geom_point(data=new_emF24,aes(x=Year_num, y=response, group=Treatment, color=Treatment,shape = Treatment),size=3,position=position_dodge(0.5))+
  geom_errorbar(data=new_emF24,aes(x=Year_num, y=response,ymin=asymp.LCL, ymax=asymp.UCL,group=Treatment, color=Treatment), width=0.2, size=0.7,
                position=position_dodge(0.5),alpha=0.7)+
  geom_vline(xintercept=2017.5, linetype = "dashed", color = "black", linewidth = 0.8)+
  labs(title="F) Flatfish", x = "Year", y = "MaxN", fill = "Treatment")+
  theme(panel.background = element_rect(fill = "white"))+
  theme(panel.background = element_rect(fill = "white"))+
  theme(panel.border = element_rect(fill=NA,colour="black",size=0.4))+
  theme(legend.key = element_rect(fill="white"),legend.position = "none")+
  scale_x_continuous(breaks = c(2017, 2018, 2021))
Flatfish_PLOT_REV
#Packages needed to combine plots
library(cowplot)
library(ggarrange)
library(patchwork)
#Combine plots
Abundances_combined <- G.morhua_PLOT_REV+
  C.harengus_PLOT_REV+
  C.rupestris_PLOT_REV+
  G.flavescens_PLOT_REV+
  C.maenas_PLOT_REV+
  Flatfish_PLOT_REV & theme(legend.position = "bottom", legend.text = element_text(size = 13))
Abundance_emmeans_plot_rev<-Abundances_combined+plot_layout(guides="collect")
Abundance_emmeans_plot_rev
ggsave(Abundance_emmeans_plot_rev, filename = "BAITED_Abundance_common_names.png", width = 12, height = 10, dpi = 300) #MAKE A PLOTS FOLDER IN THE WD
################################################################################
##############// UNBAITED EMMEANS and CONTRAST ANALYSIS //######################
########################// emmeans package //###################################
###------------------------------ UN_COD, EMMEANS
library(emmeans)
emUN_COD<-emmeans(UN_COD,specs=pairwise~Year:Treatment,type="response",combine=TRUE)
emUN_COD
contrasts_UN_COD<-as.data.frame(summary(emUN_COD$contrasts))
View(contrasts_UN_COD)
responses_UN_COD<-as.data.frame(summary(emUN_COD$emmeans))
View(responses_UN_COD)
new_emUN_COD<-responses_UN_COD
#CHECK THE STRUCTURE AND ADD THE YEAR NUMERIC COLUMN
str(new_emUN_COD)
View(new_emUN_COD)
new_emUN_COD$Year_num<-new_emUN_COD$Year
new_emUN_COD$Year_num<- as.numeric(as.character(new_emUN_COD$Year_num))
#Saving as a datasets: estimated marginal response + contrasts
export(new_emUN_COD, "UN_COD_emmeans.xlsx")
export(contrasts_UN_COD, "UN_COD_contrasts.xlsx")
library(emmeans)
###------------------------------ UN_HER, EMMEANS
emUN_HER<-emmeans(UN_HER,specs=pairwise~Year:Treatment,type="response",combine=TRUE)
emUN_HER
contrasts_UN_HER<-as.data.frame(summary(emUN_HER$contrasts))
summary(contrasts_UN_HER)
responses_UN_HER<-as.data.frame(summary(emUN_HER$emmeans))
summary(responses_UN_HER)
new_emUN_HER<-responses_UN_HER
#CHECK THE STRUCTURE AND ADD THE YEAR NUMERIC COLUMN
str(new_emUN_HER)
new_emUN_HER$Year_num<-new_emUN_HER$Year
new_emUN_HER$Year_num<- as.numeric(as.character(new_emUN_HER$Year_num))
str(new_emUN_HER)
#Saving as a datasets: estimated marginal response + contrasts
library(openxlsx)
library(writexl)
saveRDS(new_emUN_HER,file="UN_HER_emmeans",ascii=TRUE)
saveRDS(contrasts_UN_HER,file="UN_HER_contrasts",ascii=TRUE)
#save as spreadsheet
write_xlsx(new_emUN_HER, path = "UN_HER_emmeans.xlsx")
write_xlsx(contrasts_UN_HER, path = "UN_HER_contrasts.xlsx")
###------------------------------ UN_LAB, EMMEANS
emUN_LAB<-emmeans(UN_LAB,specs=pairwise~Year:Treatment,type="response",combine=TRUE)
emUN_LAB
contrasts_UN_LAB<-as.data.frame(summary(emUN_LAB$contrasts))
View(contrasts_UN_LAB)
responses_UN_LAB<-as.data.frame(summary(emUN_LAB$emmeans))
summary(responses_UN_LAB)
new_emUN_LAB<-responses_UN_LAB
#CHECK THE STRUCTURE AND ADD THE YEAR NUMERIC COLUMN
str(new_emUN_LAB)
str(new_emUN_LAB)
new_emUN_LAB$Year_num<-new_emUN_LAB$Year
new_emUN_LAB$Year_num<- as.numeric(as.character(new_emUN_LAB$Year_num))
#Saving as a datasets: estimated marginal response + contrasts
export(new_emUN_LAB, "UN_LAB_emmeans.xlsx")
export(contrasts_UN_LAB, "UN_LAB_contrasts.xlsx")
###------------------------------ UN_GOB, EMMEANS
emUN_GOB<-emmeans(UN_GOB,specs=pairwise~Year:Treatment,type="response",combine=TRUE)
emUN_GOB
contrasts_UN_GOB<-as.data.frame(summary(emUN_GOB$contrasts))
summary(contrasts_UN_GOB)
responses_UN_GOB<-as.data.frame(summary(emUN_GOB$emmeans))
summary(responses_UN_GOB)
new_emUN_GOB<-responses_UN_GOB
#CHECK THE STRUCTURE AND ADD THE YEAR NUMERIC COLUMN
str(new_emUN_GOB)
new_emUN_GOB$Year_num<-new_emUN_GOB$Year
new_emUN_GOB$Year_num<- as.numeric(as.character(new_emUN_GOB$Year_num))
str(new_emUN_GOB)
#Saving as a datasets: estimated marginal response + contrasts
export(new_emUN_GOB, "UN_GOB_emmeans.xlsx")
export(contrasts_UN_GOB, "UN_GOB_contrasts.xlsx")
###------------------------------ UN_CRAB, EMMEANS
emUN_CRAB<-emmeans(UN_CRAB,specs=pairwise~Year:Treatment,type="response",combine=TRUE)
emUN_CRAB
contrasts_UN_CRAB<-as.data.frame(summary(emUN_CRAB$contrasts))
head(contrasts_UN_CRAB)
responses_UN_CRAB<-as.data.frame(summary(emUN_CRAB$emmeans))
head(responses_UN_CRAB)
new_emUN_CRAB<-responses_UN_CRAB
#CHECK THE STRUCTURE AND ADD THE YEAR NUMERIC COLUMN
str(new_emUN_CRAB)
new_emUN_CRAB$Year_num<-new_emUN_CRAB$Year
new_emUN_CRAB$Year_num<- as.numeric(as.character(new_emUN_CRAB$Year_num))
str(new_emUN_CRAB)
#Saving as a datasets: estimated marginal response + contrasts
export(new_emUN_CRAB, "UN_CRAB_emmeans.xlsx")
export(contrasts_UN_CRAB, "UN_CRAB_contrasts.xlsx")
###------------------------------ UN_FLAT, EMMEANS
emUN_FLAT<-emmeans(UN_FLAT,specs=pairwise~Year:Treatment,type="response",combine=TRUE)
emUN_FLAT
contrasts_UN_FLAT<-as.data.frame(summary(emUN_FLAT$contrasts))
head(contrasts_UN_FLAT)
responses_UN_FLAT<-as.data.frame(summary(emUN_FLAT$emmeans))
head(responses_UN_FLAT)
new_emUN_FLAT<-responses_UN_FLAT
#CHECK THE STRUCTURE AND ADD THE YEAR NUMERIC COLUMN
str(new_emUN_FLAT)
head(new_emUN_FLAT)
new_emUN_FLAT$Year_num<-new_emUN_FLAT$Year
new_emUN_FLAT$Year_num<- as.numeric(as.character(new_emUN_FLAT$Year_num))
str(new_emUN_FLAT)
#Saving as a datasets: estimated marginal response + contrasts
export(new_emUN_FLAT, "UN_FLAT_emmeans.xlsx")
export(contrasts_UN_FLAT, "UN_FLAT_contrasts.xlsx")
################################################################################
#####################// opening emmeans from wd //##############################
new_emUN_COD <- readRDS("new_emUN_COD")
new_emUN_HER <- readRDS("new_emUN_HER")
new_emUN_LAB <- readRDS("new_emUN_LAB")
new_emUN_GOB <- readRDS("new_emUN_GOB")
new_emUN_CRAB <- readRDS("new_emUN_CRAB")
new_emUN_FLAT <- readRDS("new_emUN_FLAT")

################################################################################
#########// PLOTTING EMMEANS RESPONSES WITH IC FROM UNBAITED DATA //############
library(ggplot2)
UN_G.morhua_PLOT_REV<-ggplot()+
  scale_color_manual(values=c("#FF524B", "#7CA1CC", "#1F449C"))+
  scale_fill_manual(values = c("#FF524B", "#7CA1CC", "#1F449C"))+
  geom_point(data=new_emUN_COD,aes(x=Year_num, y=response, group=Treatment, color=Treatment, shape = Treatment),size=3,position=position_dodge(0.5))+
  geom_errorbar(data=new_emUN_COD,aes(x=Year_num, y=response,ymin=asymp.LCL, ymax=asymp.UCL,group=Treatment, color=Treatment), width=0.2, size=0.7,
                position=position_dodge(0.5),alpha=0.7)+
  geom_vline(xintercept=2017.5, linetype = "dashed", color = "black", linewidth = 0.8)+
  labs(title="A) Atlantic cod", x = "Year", y = "MaxN / 2min.", fill = "Treatment") +
  theme(panel.background = element_rect(fill = "white"))+
  theme(panel.background = element_rect(fill = "white"))+
  theme(panel.border = element_rect(fill=NA,colour="black",size=0.4))+
  theme(legend.key = element_rect(fill="white"),legend.position = "none")+
  scale_x_continuous(breaks = c(2017, 2018, 2021))
UN_G.morhua_PLOT_REV
UN_C.harengus_PLOT_REV<-ggplot()+
  scale_color_manual(values=c("#FF524B", "#7CA1CC", "#1F449C"))+
  scale_fill_manual(values = c("#FF524B", "#7CA1CC", "#1F449C"))+
  geom_point(data=new_emUN_HER,aes(x=Year_num, y=response, group=Treatment, color=Treatment, shape = Treatment),size=3,position=position_dodge(0.5))+
  geom_errorbar(data=new_emUN_HER,aes(x=Year_num, y=response,ymin=asymp.LCL, ymax=asymp.UCL,group=Treatment, color=Treatment), width=0.2, size=0.7,
                position=position_dodge(0.5),alpha=0.7)+
  geom_vline(xintercept=2017.5, linetype = "dashed", color = "black", linewidth = 0.8)+
  labs(title="B) Atlantic herring", x = "Year", y = "MaxN / 2min.", fill = "Treatment")+
  theme(panel.background = element_rect(fill = "white"))+
  theme(panel.background = element_rect(fill = "white"))+
  theme(panel.border = element_rect(fill=NA,colour="black",size=0.4))+
  theme(legend.key = element_rect(fill="white"),legend.position = "none")+
  scale_x_continuous(breaks = c(2017, 2018, 2021))
UN_C.harengus_PLOT_REV
UN_C.rupestris_PLOT_REV<-ggplot()+
  scale_color_manual(values=c("#FF524B", "#7CA1CC", "#1F449C"))+
  scale_fill_manual(values = c("#FF524B", "#7CA1CC", "#1F449C"))+
  geom_point(data=new_emUN_LAB,aes(x=Year_num, y=rate, group=Treatment, color=Treatment, shape = Treatment),size=3,position=position_dodge(0.5))+
  geom_errorbar(data=new_emUN_LAB,aes(x=Year_num, y=rate,ymin=asymp.LCL, ymax=asymp.UCL,group=Treatment, color=Treatment), width=0.2, size=0.7,
                position=position_dodge(0.5),alpha=0.7)+
  geom_vline(xintercept=2017.5, linetype = "dashed", color = "black", linewidth = 0.8)+
  labs(title="C) Goldsinny wrasse", x = "Year", y = "MaxN / 2min.", fill = "Treatment") +
  theme(panel.background = element_rect(fill = "white"))+
  theme(panel.background = element_rect(fill = "white"))+
  theme(panel.border = element_rect(fill=NA,colour="black",size=0.4))+
  theme(legend.key = element_rect(fill="white"),legend.position = "none")+
  scale_x_continuous(breaks = c(2017, 2018, 2021))
UN_C.rupestris_PLOT_REV #rate instead of response? why I just do not know?
UN_G.flavescens_PLOT_REV<-ggplot()+
  scale_color_manual(values=c("#FF524B", "#7CA1CC", "#1F449C"))+
  scale_fill_manual(values = c("#FF524B", "#7CA1CC", "#1F449C"))+
  geom_point(data=new_emUN_GOB,aes(x=Year_num, y=response, group=Treatment, color=Treatment, shape = Treatment),size=3,position=position_dodge(0.5))+
  geom_errorbar(data=new_emUN_GOB,aes(x=Year_num, y=response,ymin=asymp.LCL, ymax=asymp.UCL,group=Treatment, color=Treatment), width=0.2, size=0.7,
                position=position_dodge(0.5),alpha=0.7)+
  geom_vline(xintercept=2017.5, linetype = "dashed", color = "black", linewidth = 0.8)+
  labs(title="D) Two-spotted goby", x = "Year", y = "MaxN / 2min.", fill = "Treatment")+
  theme(panel.background = element_rect(fill = "white"))+
  theme(panel.background = element_rect(fill = "white"))+
  theme(panel.border = element_rect(fill=NA,colour="black",size=0.4))+
  theme(legend.key = element_rect(fill="white"),legend.position = "none")+
  scale_x_continuous(breaks = c(2017, 2018, 2021))
UN_G.flavescens_PLOT_REV
UN_C.maenas_PLOT_REV<-ggplot()+
  scale_color_manual(values=c("#FF524B", "#7CA1CC", "#1F449C"))+
  scale_fill_manual(values = c("#FF524B", "#7CA1CC", "#1F449C"))+
  geom_point(data=new_emUN_CRAB,aes(x=Year_num, y=rate, group=Treatment, color=Treatment, shape = Treatment),size=3,position=position_dodge(0.5))+
  geom_errorbar(data=new_emUN_CRAB,aes(x=Year_num, y=rate,ymin=asymp.LCL, ymax=asymp.UCL,group=Treatment, color=Treatment), width=0.2, size=0.7,
                position=position_dodge(0.5),alpha=0.7)+
  geom_vline(xintercept=2017.5, linetype = "dashed", color = "black", linewidth = 0.8)+
  labs(title="E) Shorecrab", x = "Year", y = "MaxN / 2min.", fill = "Treatment") +
  theme(panel.background = element_rect(fill = "white"))+
  theme(panel.background = element_rect(fill = "white"))+
  theme(panel.border = element_rect(fill=NA,colour="black",size=0.4))+
  theme(legend.key = element_rect(fill="white"),legend.position = "none")+
  scale_x_continuous(breaks = c(2017, 2018, 2021))
UN_C.maenas_PLOT_REV
UN_Flatfish_PLOT_REV<-ggplot()+
  scale_color_manual(values=c("#FF524B", "#7CA1CC", "#1F449C"))+
  scale_fill_manual(values = c("#FF524B", "#7CA1CC", "#1F449C"))+
  geom_point(data=new_emUN_FLAT,aes(x=Year_num, y=rate, group=Treatment, color=Treatment, shape = Treatment),size=3,position=position_dodge(0.5))+
  geom_errorbar(data=new_emUN_FLAT,aes(x=Year_num, y=rate,ymin=asymp.LCL, ymax=asymp.UCL,group=Treatment, color=Treatment), width=0.2, size=0.7,
                position=position_dodge(0.5),alpha=0.7)+
  geom_vline(xintercept=2017.5, linetype = "dashed", color = "black", linewidth = 0.8)+
  labs(title="F) Flatfish", x = "Year", y = "MaxN / 2min.", fill = "Treatment") +
  theme(panel.background = element_rect(fill = "white"))+
  theme(panel.background = element_rect(fill = "white"))+
  theme(panel.border = element_rect(fill=NA,colour="black",size=0.4))+
  theme(legend.key = element_rect(fill="white"),legend.position = "none")+
  scale_x_continuous(breaks = c(2017, 2018, 2021))
UN_Flatfish_PLOT_REV
################################################################################
#################// PLOT COMBINATION OF THE UNBAITED DATA //###################
library(cowplot)
library(ggarrange)
library(patchwork)
UN_Abundances_combined_REV <- UN_G.morhua_PLOT_REV+
  UN_C.harengus_PLOT_REV+
  UN_C.rupestris_PLOT_REV+
  UN_G.flavescens_PLOT_REV+
  UN_C.maenas_PLOT_REV+
  UN_Flatfish_PLOT_REV & theme(legend.position = "bottom",legend.text = element_text(size = 13))
UN_Abundance_emmeans_plot<-UN_Abundances_combined_REV+plot_layout(guides="collect")
UN_Abundance_emmeans_plot
################################################################################
###############// Slope comparison (interaction contrasts) //###################
### Atlantic cod
interactionUN_COD<-contrast(emUN_COD, interaction="pairwise",type="response")
interactionUN_COD
UN_COD_emtrends<-as.data.frame(interactionUN_COD)
export(UN_COD_emtrends, "UN_COD_emtrends.xlsx")
### Atlantic herring 
interactionUN_HER<-contrast(emUN_HER, interaction="pairwise",type="response")
interactionUN_HER
UN_HER_emtrends<-as.data.frame(interactionUN_HER)
saveRDS(UN_HER_emtrends,file="UN_HER_emtrends",ascii=TRUE)
export(UN_HER_emtrends, "UN_HER_emtrends.xlsx")
### Goldsinny wrasse
interactionUN_LAB<-contrast(emUN_LAB, interaction="pairwise",type="response")
interactionUN_LAB
UN_LAB_emtrends<-as.data.frame(interactionUN_LAB)
export(UN_LAB_emtrends, "UN_LAB_emtrends.xlsx")
### Two-spotted goby 
interactionUN_GOB<-contrast(emUN_GOB, interaction="pairwise",type="response")
interactionUN_GOB
UN_GOB_emtrends<-as.data.frame(interactionUN_GOB)
export(UN_GOB_emtrends, "UN_GOB_emtrends.xlsx")
### Shorecrab 
interactionUN_CRAB<-contrast(emUN_CRAB, interaction="pairwise",type="response")
interactionUN_CRAB
UN_CRAB_emtrends<-as.data.frame(interactionUN_CRAB)
export(UN_CRAB_emtrends, "UN_CRAB_emtrends.xlsx")
### Flatfish
interactionUN_FLAT<-contrast(emUN_FLAT, interaction="pairwise",type="response")
interactionUN_FLAT
UN_FLAT_emtrends<-as.data.frame(interactionUN_FLAT)
export(UN_FLAT_emtrends, "UN_FLAT_emtrends.xlsx")
##################################### || END OF SCRIPT || ##########################################