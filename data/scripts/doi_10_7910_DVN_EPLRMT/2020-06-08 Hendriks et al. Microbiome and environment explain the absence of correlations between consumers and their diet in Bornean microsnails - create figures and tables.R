### R scipt to create figures and tables with Hendriks et al. (2020) Microbiome and environment explain the absence of correlations between
### consumers and their diet in Bornean microsnails, Ecology.

#We first load all results as previously arrived at by the authors.
load(file="saved_R_objects/Hendriks_Bisschop_et_al_2019.rda")

#CREATE FIGURE 2 FOR PUBLICATION.
#We use package ggpubr to quickly check some regressions and focus on Shannon diversity.
library(ggpubr)
Figure2<-plot_grid(plot_grid(ggscatter(data=data_byPlot_allSpecies_summary, x="Shannon.comm.shells", y="Shannon.diet.mean", add="reg.line", add.params = list(linetype="dashed"))+
                               stat_cor()+
                               geom_errorbar(aes(ymax=Shannon.diet.upperCI, ymin=Shannon.diet.lowerCI))+
                               stat_regline_equation(label.y = 3.2)+
                               labs(x="Consumer community", y="Diet")+
                               scale_y_continuous(labels=function(x) sprintf("%.1f", x))+
                               scale_x_continuous(labels=function(x) sprintf("%.1f", x)),
                             ggscatter(data=data_byPlot_allSpecies_summary, x="Shannon.comm.shells", y="Shannon.micr.mean", add="reg.line", add.params = list(linetype="dashed"))+
                               stat_cor()+
                               geom_errorbar(aes(ymax=Shannon.micr.upperCI, ymin=Shannon.micr.lowerCI))+
                               stat_regline_equation(label.y = 6.2)+
                               labs(x="Consumer community", y="Microbiome")+
                               scale_y_continuous(labels=function(x) sprintf("%.1f", x))+
                               scale_x_continuous(labels=function(x) sprintf("%.1f", x)),
                             ggscatter(data=data_byPlot_allSpecies_summary, x="Shannon.diet.mean", y="Shannon.micr.mean", add="reg.line", add.params = list(linetype="dashed"))+
                               stat_cor()+
                               geom_errorbar(aes(ymax=Shannon.micr.upperCI, ymin=Shannon.micr.lowerCI))+
                               geom_errorbarh(aes(xmax=Shannon.diet.upperCI, xmin=Shannon.diet.lowerCI))+
                               stat_regline_equation(label.y = 6.2)+
                               labs(x="Diet", y="Microbiome")+
                               scale_y_continuous(labels=function(x) sprintf("%.1f", x))+
                               scale_x_continuous(labels=function(x) sprintf("%.1f", x)),
                             align = 'hv',
                             rows = 1, cols = 3, labels = c("(A)","(B)","(C)")),
                   plot_grid(ggscatter(data=data, x="Shannon.comm.shells", y="Shannon.diet", add="reg.line", size=1, add.params = list(linetype="dashed"))+
                               stat_cor()+
                               stat_regline_equation(label.y = 3.2)+
                               labs(x="Consumer community", y="Diet")+
                               scale_y_continuous(labels=function(x) sprintf("%.1f", x))+
                               scale_x_continuous(labels=function(x) sprintf("%.1f", x)),
                             ggscatter(data=data, x="Shannon.comm.shells", y="Shannon.micr", add="reg.line", size=1, add.params = list(linetype="dashed"))+
                               stat_cor()+
                               stat_regline_equation(label.y = 6.2)+
                               labs(x="Consumer community", y="Microbiome")+
                               scale_y_continuous(labels=function(x) sprintf("%.1f", x))+
                               scale_x_continuous(labels=function(x) sprintf("%.1f", x)),
                             ggscatter(data=data, x="Shannon.diet", y="Shannon.micr", add="reg.line", size=1, add.params = list(linetype="dashed"))+
                               stat_cor()+
                               stat_regline_equation(label.y = 6.2)+
                               labs(x="Diet", y="Microbiome")+
                               scale_y_continuous(labels=function(x) sprintf("%.1f", x))+
                               scale_x_continuous(labels=function(x) sprintf("%.1f", x)),
                             align = 'hv',
                             rows = 1, cols = 3, labels = c("(D)","(E)","(F)")),
                   rows=2, align='hv')
ggsave(Figure2, file = "export_for_publication/Figure2.pdf", width = 10, height = 6.5)


#CREATE FIGURE 4 FOR PUBLICATION.
(Figure4<-cowplot::plot_grid(diet_byPlot_UniFrac.plot + theme(legend.position="none"),
                             microbiome_byPlot_UniFrac.plot + theme(legend.position="none") ,
                             get_legend(diet_byPlot_UniFrac.plot),
                             rel_widths = c(3,3,1.1),
                             labels= c("(A)","(B)"),
                             nrow=1))
ggsave(Figure4, file = "export_for_publication/Figure4.pdf", width = 12, height = 5)


#CREATE FIGURE S5 FOR PUBLICATION.
(FigureS5<-cowplot::plot_grid(diet_byPlot_Bray.plot + theme(legend.position="none"),
                             microbiome_byPlot_Bray.plot + theme(legend.position="none"),
                             get_legend(diet_byPlot_Bray.plot),
                             diet_byPlot_Jaccard.plot + theme(legend.position="none"),
                             microbiome_byPlot_Jaccard.plot + theme(legend.position="none"),
                             ggplot() + theme_void(),
                             diet_byPlot_WeightedUniFrac.plot + theme(legend.position="none"),
                             microbiome_byPlot_WeightedUniFrac.plot + theme(legend.position="none"),
                             ggplot() + theme_void(),
                             diet_byPlot_UniFrac.plot + theme(legend.position="none"),
                             microbiome_byPlot_UniFrac.plot + theme(legend.position="none"),
                             ggplot() + theme_void(),
                             rel_widths = c(3,3,1.1),
                             labels= c("(A)","(E)","","(B)","(F)","","(C)","(G)","","(D)","(H)",""),
                             nrow=4,
                             ncol=3))
ggsave(FigureS5, file = "export_for_publication/FigureS5.pdf", width = 16, height = 21)


#CREATE TABLE 1 FOR PUBLICATION.
Table1<-data.frame(table(data$GENUS_SPECIES,data$LOCATION_PLOT))
Table1$Family<-data.frame(microbiome.ph@sam_data)$FAMILY[match(Table1$Var1,data.frame(microbiome.ph@sam_data)$GENUS_SPECIES)]
Table1<-reshape(Table1,idvar = c("Family","Var1"),timevar = "Var2",direction = "wide")
Table1$TOTALS<-rowSums(Table1[,3:ncol(Table1)])
Table1<-Table1[order(Table1$Family, Table1$Var1),]
colnames(Table1)<-ifelse(substr(colnames(Table1),1,5)=="Freq.",substr(colnames(Table1),6,100),colnames(Table1))
names(Table1)[1]<-"Species"
Table1<-Table1[,c(2,1,3:ncol(Table1))]
Table1<-rbind(Table1,as.character(c("","",colSums(sapply(Table1, as.numeric))[3:ncol(Table1)])))
Table1<-rbind(sapply(strsplit(names(Table1),"_"),'[',2),Table1)
Table1<-rbind(sapply(strsplit(names(Table1),"_"),'[',1),Table1)
Table1[Table1==0]<-""
#And save the table.
write.csv(Table1, file = "export_for_publication/Table1.csv", na="")


#CREATE TABLE 2 FOR PUBLICATION, A SUMMARY OF FINAL MODELS CORRELATING MICROBIOME/DIET WITH host community (shells) AND SPECIES.
Table2<-as.data.frame(rbind(summary(model.Shannon.micr1)$coefficients$cond,
                            summary(model.PD.micr1)$coefficients$cond,
                            summary(model.Chao1.micr1)$coefficients$cond,
                            summary(model.Shannon.diet3)$coefficients$cond,
                            summary(model.PD.diet1)$coefficients$cond,
                            summary(model.Chao1.diet1)$coefficients$cond))
Table2$Coefficient<-rownames(Table2)
#Add response and metrics details for the different models; "1" and "2" serve as annotations to be specified below the table.
Table2$Response<-c("Microbiome", rep("",17),"Diet",rep("",14))
Table2$Metric<-c("Diversity (Shannon)1",rep("",5),"Phylogenetic diversity (Faith's PD)2",rep("",5),"Richness (Chao1)1",rep("",5),"Diversity (Shannon)3",rep("",2),"Phylogenetic diversity (Faith's PD)1",rep("",5),"Richness (Chao1)1",rep("",5))
Table2<-Table2[,c(6:7,5,1:4)]
rownames(Table2)<-NULL
#Update the names of the coefficients to publication-ready strings.
Table2$Coefficient[Table2$Coefficient=="Shannon.comm.shells.GENUS_SPECIESG_similis"]<-"host community (shells) diversity (Shannon) x Species (G. similis s.l.)"
Table2$Coefficient[Table2$Coefficient=="Shannon.comm.shells.GENUS_SPECIESP_concinnum"]<-"host community (shells) diversity (Shannon) x Species (P. concinnum)"
Table2$Coefficient[Table2$Coefficient=="PD.comm.shells.GENUS_SPECIESG_similis"]<-"host community (shells) phylogenetic diversity (PD) x Species (G. similis s.l.)"
Table2$Coefficient[Table2$Coefficient=="PD.comm.shells.GENUS_SPECIESP_concinnum"]<-"host community (shells) phylogenetic diversity (PD) x Species (P. concinnum)"
Table2$Coefficient[Table2$Coefficient=="PD.comm.shells.GENUS_SPECIESG_similis.1"]<-"host community (shells) phylogenetic diversity (PD) x Species (G. similis s.l.)"
Table2$Coefficient[Table2$Coefficient=="PD.comm.shells.GENUS_SPECIESP_concinnum.1"]<-"host community (shells) phylogenetic diversity (PD) x Species (P. concinnum)"
Table2$Coefficient[Table2$Coefficient=="Chao1.comm.shells.GENUS_SPECIESG_similis.1"]<-"host community (shells) richness (Chao1) x Species (G. similis s.l.)"
Table2$Coefficient[Table2$Coefficient=="Chao1.comm.shells.GENUS_SPECIESP_concinnum.1"]<-"host community (shells) richnes (Chao1) x Species (P. concinnum)"
Table2$Coefficient[grep("Intercept", Table2$Coefficient)]<-"Intercept"
Table2$Coefficient[grep("Shannon.comm.shells", Table2$Coefficient)]<-"host community (shells) diversity (Shannon)"
Table2$Coefficient[grep("PD.comm.shells", Table2$Coefficient)]<-"host community (shells) phylogenetic diversity (PD)"
Table2$Coefficient[grep("Chao1.comm.shells", Table2$Coefficient)]<-"host community (shells) richness (Chao1)"
Table2$Coefficient[grep("GENUS_SPECIESG_similis", Table2$Coefficient)]<-"Species (G. similis s.l.)"
Table2$Coefficient[grep("GENUS_SPECIESP_concinnum", Table2$Coefficient)]<-"Species (P. concinnum)"
#Round numbers.
Table2[,4:6]<-round(Table2[,4:6],2)
Table2$`Pr(>|z|)`[Table2$`Pr(>|z|)`<0.001]<- "<0.001"
Table2$`Pr(>|z|)`[Table2$`Pr(>|z|)`>0.001]<-round(as.numeric(Table2$`Pr(>|z|)`[Table2$`Pr(>|z|)`>0.001]),3)
#Add definitions of annotations.  
Table2<-rbind(Table2, c("1 Response variable modelled as a lognormal distribition",rep("",6)))
Table2<-rbind(Table2, c("2 Response variable modelled as a gamma distribition",rep("",6)))
Table2<-rbind(Table2, c("3 Response variable modelled as a normal distribition",rep("",6)))
#And save the table.
write.csv(Table2, file = "export_for_publication/Table2.csv", na="")


#CREATE TABLE 3 FOR PUBLICATION, PERANOVA AND BETADISPER RESULTS WITH FIGURE 4.

#We collect these results, and put these in a dataframe table for publication.
Table3<-rbind(cbind(diet_byPlot_permanovaUniFrac$aov.tab,
                    rbind(diet_byPlot_betadisperUniFrac_species$tab[1,],
                          diet_byPlot_betadisperUniFrac_location$tab[1,],
                          rep("",ncol(diet_byPlot_betadisperUniFrac_location$tab[1,])),
                          rep("",ncol(diet_byPlot_betadisperUniFrac_location$tab[1,])),
                          rep("",ncol(diet_byPlot_betadisperUniFrac_location$tab[1,])))),
              cbind(microbiome_byPlot_permanovaUniFrac$aov.tab,
                    rbind(microbiome_byPlot_betadisperUniFrac_species$tab[1,],
                          microbiome_byPlot_betadisperUniFrac_location$tab[1,],
                          rep("",ncol(microbiome_byPlot_betadisperUniFrac_location$tab[1,])),
                          rep("",ncol(microbiome_byPlot_betadisperUniFrac_location$tab[1,])),
                          rep("",ncol(microbiome_byPlot_betadisperUniFrac_location$tab[1,])))))
Table3[is.na(Table3)==T]<-""
Table3<-cbind(c("Diet",rep("",4),"Microbiome",rep("",4)),rep(c("Species", "Location", "Species*Location", "Residuals", "Totals"),2),Table3[,c(1,2,4,5,6)],rep("",nrow(Table3)),Table3[,c(8,10,12)])
colnames(Table3)<-c("Response variable", "Explanatory variable", "df", "SS", "pseudo-F", "R2", "Pr (>F)", "", "SS", "pseudo-F", "Pr (>F)")
#And save the table.
write.csv(Table3, file = "export_for_publication/Table3.csv", na="")


#CREATE TABLE 4, SPECIES INTERACTIONS FROM MODELS OF CORRELATION BETWEEN MICROBIOME/DIET, HOST COMMUNITY (SHELLS) AND SPECIES.
#First put results in dataframes with same names.
model.Shannon.diet1.trends.GENUS_SPECIES<-as.data.frame(summary(model.Shannon.diet1.contrast.GENUS_SPECIES, infer=c(TRUE,TRUE),null=0)$emtrends)
model.PD.diet1.trends.GENUS_SPECIES<-as.data.frame(summary(model.PD.diet1.contrast.GENUS_SPECIES, infer=c(TRUE,TRUE),null=0)$emtrends)
model.Chao1.diet1.trends.GENUS_SPECIES<-as.data.frame(summary(model.Chao1.diet1.contrast.GENUS_SPECIES, infer=c(TRUE,TRUE),null=0)$emtrends)
model.Shannon.micr1.trends.GENUS_SPECIES<-as.data.frame(summary(model.Shannon.micr1.contrast.GENUS_SPECIES, infer=c(TRUE,TRUE),null=0)$emtrends)
model.PD.micr1.trends.GENUS_SPECIES<-as.data.frame(summary(model.PD.micr1.contrast.GENUS_SPECIES, infer=c(TRUE,TRUE),null=0)$emtrends)
model.Chao1.micr1.trends.GENUS_SPECIES<-as.data.frame(summary(model.Chao1.micr1.contrast.GENUS_SPECIES, infer=c(TRUE,TRUE),null=0)$emtrends)
names(model.Shannon.diet1.trends.GENUS_SPECIES)[2]=names( model.PD.diet1.trends.GENUS_SPECIES)[2]=names(model.Chao1.diet1.trends.GENUS_SPECIES)[2]=names(model.Shannon.micr1.trends.GENUS_SPECIES)[2]=names(model.PD.micr1.trends.GENUS_SPECIES)[2]=names(model.Chao1.micr1.trends.GENUS_SPECIES)[2]<-"trend"
#Combine results in a table.
Table4<-rbind(model.Shannon.diet1.trends.GENUS_SPECIES,model.PD.diet1.trends.GENUS_SPECIES,model.Chao1.diet1.trends.GENUS_SPECIES,
              model.Shannon.micr1.trends.GENUS_SPECIES,model.PD.micr1.trends.GENUS_SPECIES,model.Chao1.micr1.trends.GENUS_SPECIES)
#Add columns with details.
Table4$Response<-c("Diet",rep("",nrow(Table4)/2-1),"Microbiome",rep("",nrow(Table4)/2-1))
Table4$Metric<-c("Diversity (Shannon)",rep("",nrow(Table4)/6-1),"Phylogenetic diversity (Faith's PD)",rep("",nrow(Table4)/6-1),"Richness (Chao1)",rep("",nrow(Table4)/6-1),
                 "Diversity (Shannon)",rep("",nrow(Table4)/6-1),"Phylogenetic diversity (Faith's PD)",rep("",nrow(Table4)/6-1),"Richness (Chao1)",rep("",nrow(Table4)/6-1))
#Re-order columns.
Table4<-Table4[,c(9:10,1:4,8)]
#Update names to correct spelling in print.
Table4$GENUS_SPECIES<-gsub("A_jagori","A. jagori",Table4$GENUS_SPECIES)
Table4$GENUS_SPECIES<-gsub("G_similis","G. similis s.l.",Table4$GENUS_SPECIES)
Table4$GENUS_SPECIES<-gsub("P_concinnum","P. concinnum",Table4$GENUS_SPECIES)
#Round numbers.
Table4[,c(4,5,7)]<-round(Table4[,c(4,5,7)],3)
Table4$p.value[Table4$p.value<0.001]<- "<0.001"
Table4$p.value[Table4$p.value>0.001]<-round(as.numeric(Table4$p.value[Table4$p.value>0.001]),3)
colnames(Table4)<-c("Response","Metric","Species","Trend","SE","df","p-value")
#And save the table.
write.csv(Table4, file = "export_for_publication/Table4.csv", na="", row.names = FALSE)


#CREATE Table S2 FOR PUBLICATION, AN OVERVIEW OF COMMUNITY DATA FROM SHELLS AND LIVE SNAILS
#Get the community data from the PhyloSeq objects.
TableS2<-rbind(community.shells.ph@otu_table[,order(colnames(community.shells.ph@otu_table))],
               rbind(colnames(community.live.ph@otu_table),community.live.ph@otu_table)[,order(colnames(community.live.ph@otu_table))])
#Include rownames in table.
TableS2<-cbind(rownames(TableS2),TableS2)
rownames(TableS2)<-NULL
#Include full location name and plot in a separate row, plus distinction between (A) and (B).
TableS2<-rbind(sapply(strsplit(colnames(TableS2),"_"),'[',2),
               c("(A)",rep("",ncol(TableS2)-1)),
               TableS2[1:69,],
               c("(B)",rep("",ncol(TableS2)-1)),
               TableS2[71:139,])
colnames(TableS2)<-sapply(strsplit(colnames(TableS2),"_"),'[',1)
colnames(TableS2)[1]<-"Location"
TableS2[1,1]<-"Plot"
#Annotate table with details on Batangan plots 5 & 6.
TableS2[1,3:4]<-paste0(TableS2[1,3:4],"1")
TableS2<-rbind(TableS2,
               c("1 Host community data (shells) for Batangan plots 5 and 6 have been pooled during fieldwork; here we have taken the same data for either plot.",rep("",ncol(TableS2)-1)),
               c("2 Originally described (and collected by us) as Georissa similis E. A. Smith, 1894, but recently split into a radiation of highly similar and closely related taxa (Khalik, Hendriks, Vermeulen, & Schilthuizen, 2019). With all phylogenetic relations within the radiation being much closer than those among all other taxa considered within this study, we treat G. similis s.l. as a single species in this study.",rep("",ncol(TableS2)-1)))
#Update location names.
colnames(TableS2)<-gsub("TanduBatu","Tandu Batu",colnames(TableS2))
colnames(TableS2)<-gsub("Tomanggong2","Tomanggong 2",colnames(TableS2))
colnames(TableS2)<-gsub("TomanggongKecil","Tomanggong Kecil",colnames(TableS2))
#Update species names to full names.
TableS2[,1]<-gsub("Geo.sca","Georissa kinabatanganensis Khalik, Hendriks, Vermeulen & Schilthuizen, 2018",TableS2[,1])
TableS2[,1]<-gsub("Geo.gom","Georissa gomantongensis E. A. Smith, 1894",TableS2[,1])
TableS2[,1]<-gsub("Geo.nep","Georissa nephrostoma Vermeulen, Liew & Schilthuizen, 2015",TableS2[,1])
TableS2[,1]<-gsub("Geo.sim","Georissa similis E. A. Smith, 1894 s.l.2",TableS2[,1])
TableS2[,1]<-gsub("Sul.mar","Sulfurina martensi (Issel, 1874)",TableS2[,1])
TableS2[,1]<-gsub("Acm.str","Acmella striata Vermeulen, Liew & Schilthuizen, 2015",TableS2[,1])
TableS2[,1]<-gsub("Acm.cyr","Acmella cyrtoglyphe Vermeulen, Liew & Schilthuizen, 2015",TableS2[,1])
TableS2[,1]<-gsub("Acm.ovo","Acmella ovoidea Vermeulen, Liew & Schilthuizen, 2015",TableS2[,1])
TableS2[,1]<-gsub("Acm.pol","Acmella polita Von Moellendorff, 1887",TableS2[,1])
TableS2[,1]<-gsub("Dip.asy","Diplommatina asynaimos Vermeulen, 1993",TableS2[,1])
TableS2[,1]<-gsub("Pup.hos","Pupina hosei Godwin Austen, 1889",TableS2[,1])
TableS2[,1]<-gsub("Jap.sp.","Japonia sp. ",TableS2[,1])
TableS2[,1]<-gsub("Aly.jag","Alycaeus jagori Von Martens, 1859",TableS2[,1])
TableS2[,1]<-gsub("Cha.V15","Chamalycaeus sp.",TableS2[,1])
TableS2[,1]<-gsub("Cha.spe","Chamalycaeus specus (Godwin Austen, 1889)",TableS2[,1])
TableS2[,1]<-gsub("Pte.sp.","Pterocyclos/Opisthoporus sp. ",TableS2[,1])
TableS2[,1]<-gsub("Lep.ser","Leptopoma sericatum (Pfeiffer, 1851)",TableS2[,1])
TableS2[,1]<-gsub("Lep.pel","Leptopoma pellucidum (Grateloup, 1840)",TableS2[,1])
TableS2[,1]<-gsub("Lep.und","Leptopoma undatum (Metcalfe, 1851)",TableS2[,1])
TableS2[,1]<-gsub("Dip.gom","Diplommatina gomantongensis (E. A. Smith, 1894)",TableS2[,1])
TableS2[,1]<-gsub("Dip.cal","Diplommatina calvula Vermeulen, 1993",TableS2[,1])
TableS2[,1]<-gsub("Dip.rub","Diplommatina rubicunda (Von Martens, 1864)",TableS2[,1])
TableS2[,1]<-gsub("Opi.hai","Opisthostoma hailei Solem, 1964",TableS2[,1])
TableS2[,1]<-gsub("Ple.con","Plectostoma concinnum (Fulton, 1901)",TableS2[,1])
TableS2[,1]<-gsub("Ple.sim","Plectostoma simplex (Fulton, 1901)",TableS2[,1])
TableS2[,1]<-gsub("Ari.bor","Arinia borneensis E. A. Smith, 1894",TableS2[,1])
TableS2[,1]<-gsub("Ari.bip","Arinia biplicata Vermeulen, 1996",TableS2[,1])
TableS2[,1]<-gsub("Ari.tur","Arinia turgida Vermeulen, 1996",TableS2[,1])
TableS2[,1]<-gsub("Ari.per","Arinia pertusa Vermeulen, 1996",TableS2[,1])
TableS2[,1]<-gsub("Ari.ste","Arinia stenotrochus strenotrochus Vermeulen, 1996",TableS2[,1])
TableS2[,1]<-gsub("Hut.bic","Huttonella bicolor (Hutton, 1834)",TableS2[,1])
TableS2[,1]<-gsub("Sub.oct","Subulina octana (Bruguiere, 1789)",TableS2[,1])
TableS2[,1]<-gsub("All.cla","Allopeas clavulinum (Potiez & Michaud, 1838)",TableS2[,1])
TableS2[,1]<-gsub("All.gra","Allopeas gracile (Hutton, 1834)",TableS2[,1])
TableS2[,1]<-gsub("Phi.kus","Philalanka kusana (Aldrich, 1889)",TableS2[,1])
TableS2[,1]<-gsub("Phi.mol","Philalanka moluensis (E.A. Smith, 1893)",TableS2[,1])
TableS2[,1]<-gsub("Bei.phi","Beilania philippinensis (Semper, 1874)",TableS2[,1])
TableS2[,1]<-gsub("Ela.glo","Elasmias globulosum Quadras & Moellendorff ex Zilch, 1962",TableS2[,1])
TableS2[,1]<-gsub("Chl.kin","Chloritis kinibalensis (Kobelt, 1894)",TableS2[,1])
TableS2[,1]<-gsub("Gan.acr","Ganesella acris (Benson, 1859)",TableS2[,1])
TableS2[,1]<-gsub("Bra.sim","Bradybaena similaris Ferussac, 1821",TableS2[,1])
TableS2[,1]<-gsub("Amp.sp","Amphidromus sp.",TableS2[,1])
TableS2[,1]<-gsub("Lan.rot","Landouria rotatoria (Von Dem Busch, 1842)",TableS2[,1])
TableS2[,1]<-gsub("Nes.mor","Nesopupa moreleti (Brown, 1870)",TableS2[,1])
TableS2[,1]<-gsub("Pty.orc","Ptychopatula orcula (Benson, 1850)",TableS2[,1])
TableS2[,1]<-gsub("Rah.del","Rahula delopleura Vermeulen, Liew & Schilthuizen, 2015",TableS2[,1])
TableS2[,1]<-gsub("Kal.acc","Kaliella accepta (Smith, 1895)",TableS2[,1])
TableS2[,1]<-gsub("Eve.sp","Everettia sp.",TableS2[,1])
TableS2[,1]<-gsub("Cha.jug","Charopa jugalis sp. n.",TableS2[,1])
TableS2[,1]<-gsub("Mac.ter","Macrochlamys tersa (Issel, 1874)",TableS2[,1])
TableS2[,1]<-gsub("Cha.lis","Charopa lissobasis sp. n.",TableS2[,1])
TableS2[,1]<-gsub("Cha.arg","Charopa argos sp. n.",TableS2[,1])
TableS2[,1]<-gsub("Mic.mus","Microcystina muscorum Van Benthem Jutting, 1959",TableS2[,1])
TableS2[,1]<-gsub("Mic.app","Microcystina appendiculata (Von Moellendorff, 1893)",TableS2[,1])
TableS2[,1]<-gsub("Kal.sca","Kaliella scandens (Cox, 1872)",TableS2[,1])
TableS2[,1]<-gsub("Kal.dol","Kaliella doliolum (Pfeiffer, 1846)",TableS2[,1])
TableS2[,1]<-gsub("Vid.met","Videna metcalfei (Pfeiffer, 1845)",TableS2[,1])
TableS2[,1]<-gsub("Geo.whi","Geotrochus whiteheadi (E.A. Smith, 1895)",TableS2[,1])
TableS2[,1]<-gsub("Geo.lab","Geotrochus labuanensis (Pfeiffer, 1863)",TableS2[,1])
TableS2[,1]<-gsub("Vid.tim","Videna timorensis (Von Martens, 1867)",TableS2[,1])
TableS2[,1]<-gsub("Vid.fro","Videna froggatti (Iredale, 1941)",TableS2[,1])
TableS2[,1]<-gsub("Kal.cal","Kaliella calculosa (Gould, 1852)",TableS2[,1])
TableS2[,1]<-gsub("Kal.bar","Kaliella barrakporensis (Pfeiffer, 1852)",TableS2[,1])
TableS2[,1]<-gsub("Kal.pun","Kaliella punctata Vermeulen, Liew & Schilthuizen, 2015",TableS2[,1])
TableS2[,1]<-gsub("Kal.den","Kaliella dendrophila (Van Benthem Jutting, 1950)",TableS2[,1])
TableS2[,1]<-gsub("Kal.mic","Kaliella microconus (Mousson, 1865)",TableS2[,1])
TableS2[,1]<-gsub("Mic.str","Microcystina striatula Vermeulen, Liew & Schilthuizen, 2015",TableS2[,1])
TableS2[,1]<-gsub("Mic.mic","Microcystina microrhynchus Vermeulen, Liew & Schilthuizen, 2015",TableS2[,1])
TableS2[,1]<-gsub("Mic.sin","Microcystina sinica Von Moellendorff, 1885",TableS2[,1])
#Change all zeros to empty cells.
TableS2[TableS2==0]<-""
#Re-order rows for table parts (A) and (B) by name.
TableS2<-rbind(TableS2[1:2,],
               TableS2[3:71,][order(TableS2[3:71,1]),],
               TableS2[72,],
               TableS2[73:141,][order(TableS2[73:141,1]),],
               TableS2[142:143,])
#And save the table.
write.csv(TableS2, file = "export_for_publication/TableS2.csv", na="")


#CREATE Table S3 FOR PUBLICATION.
TableS3<-data[,c("LOCATION_PLOT","LOCATION","PLOT",
                 "Chao1.comm.shells","Shannon.comm.shells","Simpson.comm.shells","InvSimpson.comm.shells","ShannonEvenness.comm.shells","PD.comm.shells",
                 "Chao1.comm.live","Shannon.comm.live","Simpson.comm.live","InvSimpson.comm.live","ShannonEvenness.comm.live","PD.comm.live",
                 "island.size","distance.cave","distance.next.outcrop",
                 "distance.river","altitude","distance.main.road","distance.plantation","time.since.rain","humidity.level")]
TableS3$Location<-""
TableS3$CommunityShells<-""
TableS3$CommunityLive<-""
TableS3$Environment<-""
TableS3<-TableS3[!duplicated(TableS3),]
TableS3<-merge.data.frame(TableS3,environment[,c("location_plot","lat","long")],by=1)
TableS3[,c("distance.cave")]<-round(TableS3[,c("distance.cave")],0)
TableS3[,c("Chao1.comm.shells","Chao1.comm.live")]<-round(TableS3[,c("Chao1.comm.shells","Chao1.comm.live")],1)
TableS3[,c("Shannon.comm.shells","PD.comm.shells","Simpson.comm.shells","InvSimpson.comm.shells","Shannon.comm.live","PD.comm.live","Simpson.comm.live","InvSimpson.comm.live","altitude")]<-round(TableS3[,c("Shannon.comm.shells","PD.comm.shells","Simpson.comm.shells","InvSimpson.comm.shells","Shannon.comm.live","PD.comm.live","Simpson.comm.live","InvSimpson.comm.live","altitude")],3)
TableS3[,c("ShannonEvenness.comm.shells","ShannonEvenness.comm.live","lat","long")]<-round(TableS3[,c("ShannonEvenness.comm.shells","ShannonEvenness.comm.live","lat","long")],3)
TableS3<-t(TableS3)
colnames(TableS3)<-TableS3[1,]
TableS3<-TableS3[c(25,2:3,(nrow(TableS3)-1):nrow(TableS3),26,4:9,27,10:15,28,16:24),]
TableS3<-cbind(rownames(TableS3),TableS3);rownames(TableS3)<-NULL
TableS3<-cbind(c(rep("Location",5),rep("host community (shells)",7),rep("host community (live)",7),"Environment","Island size", "Caves", "Isolation", rep("Flooding risk",2),rep("Anthropogenic proximity",2),rep("Humidity",2)),TableS3)
#And save the table.
write.csv(TableS3, file = "export_for_publication/TableS3.csv", na="")


#CREATE Table S5 FOR PUBLICATION, SAMPLE METRIC DATA AS USED IN (G)LMM AND PLS-PM MODELS.
TableS5<-data[,which(names(data) %in% c("MUSEUM_ID","SAMPLE_ID","GENUS_SPECIES","LOCATION","PLOT",
                                        "Chao1.micr","Shannon.micr","Simpson.micr","InvSimpson.micr","ShannonEvenness.micr","PD.micr",
                                        "Chao1.diet","Shannon.diet","Simpson.diet","InvSimpson.diet","ShannonEvenness.diet","PD.diet"))]
#If Shannon evenness is negative, then this is due to running code for the PLS-PM analysis, and we take the positive again.
TableS5$ShannonEvenness.micr<-ifelse(TableS5$ShannonEvenness.micr<0,-1*TableS5$ShannonEvenness.micr,TableS5$ShannonEvenness.micr)
TableS5$ShannonEvenness.diet<-ifelse(TableS5$ShannonEvenness.diet<0,-1*TableS5$ShannonEvenness.diet,TableS5$ShannonEvenness.diet)
#Round data.
TableS5[,c(7:11,13:17)]<-round(TableS5[,c(7:11,13:17)],3)
#Insert columns that can hold GenBank accession number for fastq files for microbiome and diet, plus empty columns for proper presenting.
TableS5<-cbind(TableS5[,1:5],rep("",nrow(TableS5)),rep("",nrow(TableS5)),rep("",nrow(TableS5)),TableS5[,6:11],rep("",nrow(TableS5)),TableS5[,12:17])
#Update column names.
colnames(TableS5)<-c("Museum ID","Sample ID", "Species", "Location","Plot","Microbiome","Diet","","Chao1","Shannon","Simpson","Inv. Simpson","Shannon evenness","Faith's phylogenetic diversity","","Chao1","Shannon","Simpson","Inv. Simpson","Shannon evenness","Faith's phylogenetic diversity")
#Change hyphens and underscores from code style to dot style.
TableS5$`Museum ID`<-gsub("_",".",TableS5$`Museum ID`)
TableS5$`Sample ID`<-gsub("-",".",TableS5$`Sample ID`)
#Sort table by Museum ID.
TableS5<-TableS5[order(TableS5$`Museum ID`),]
#Update location names.
TableS5$Location<-gsub("TanduBatu","Tandu Batu",TableS5$Location)
TableS5$Location<-gsub("Tomanggong2","Tomanggong 2",TableS5$Location)
TableS5$Location<-gsub("TomanggongKecil","Tomanggong Kecil",TableS5$Location)
#Update species names.
TableS5$Species<-gsub("P_concinnum","Plectostoma concinnum (Fulton, 1901)",TableS5$Species)
TableS5$Species<-gsub("G_scalinella","Georissa kinabatanganensis Khalik, Hendriks, Vermeulen & Schilthuizen, 2018",TableS5$Species)
TableS5$Species<-gsub("G_similis","Georissa similis E. A. Smith, 1894 s.l.",TableS5$Species)
TableS5$Species<-gsub("K_scandens","Kaliella scandens (Cox, 1872)",TableS5$Species)
TableS5$Species<-gsub("Japonia_sp","Japonia sp.",TableS5$Species)
TableS5$Species<-gsub("D_calvula","Diplommatina calvula Vermeulen, 1993",TableS5$Species)
TableS5$Species<-gsub("D_rubicunda","Diplommatina rubicunda (Von Martens, 1864)",TableS5$Species)
TableS5$Species<-gsub("A_jagori","Alycaeus jagori Von Martens, 1859",TableS5$Species)
TableS5$Species<-gsub("K_accepta","Kaliella accepta (Smith, 1895)",TableS5$Species)
TableS5$Species<-gsub("V_metcalfei","Videna metcalfei (Pfeiffer, 1845)",TableS5$Species)
TableS5$Species<-gsub("L_sericatum","Leptopoma sericatum (Pfeiffer, 1851)",TableS5$Species)
TableS5$Species<-gsub("D_gomantongensis","Diplommatina gomantongensis (E. A. Smith, 1894)",TableS5$Species)
TableS5$Species<-gsub("Sulfurina_sp","Sulfurina martensi (Issel, 1874)",TableS5$Species)
TableS5$Species<-gsub("M_tersa","Macrochlamys tersa (Issel, 1874)",TableS5$Species)
TableS5$Species<-gsub("Chamalycaeus_V1599","Chamalycaeus sp.",TableS5$Species)
TableS5$Species<-gsub("Pterocyclos_sp","Pterocyclos/Opisthoporus sp.",TableS5$Species)
TableS5$Species<-gsub("Videna_sp","Videna sp.",TableS5$Species)
TableS5$Species<-gsub("Everettia_sp","Everettia sp.",TableS5$Species)
TableS5$Species<-gsub("M_appendiculata","Microcystina appendiculata (Von Moellendorff, 1893)",TableS5$Species)
TableS5$Species<-gsub("D_asynaimos","Diplommatina asynaimos Vermeulen, 1993",TableS5$Species)
TableS5$Species<-gsub("J_kinabaluensis","Japonia kinabaluensis (E.A. Smith, 1895)",TableS5$Species)
TableS5$Species<-gsub("P_simplex","Plectostoma simplex (Fulton, 1901)",TableS5$Species)
TableS5$Species<-gsub("L_pellucidum","Leptopoma pellucidum (Grateloup, 1840)",TableS5$Species)
TableS5$Species<-gsub("Atopos_sp","Atopos sp.",TableS5$Species)
TableS5$Species<-gsub("K_calculosa","Kaliella calculosa (Gould, 1852)",TableS5$Species)
TableS5$Species<-gsub("K_barrakporensis","Kaliella barrakporensis (Pfeiffer, 1852)",TableS5$Species)
TableS5$Species<-gsub("A_striata","Acmella striata Vermeulen, Liew & Schilthuizen, 2015",TableS5$Species)
TableS5$Species<-gsub("A_cyrtoglyphe","Acmella cyrtoglyphe Vermeulen, Liew & Schilthuizen, 2015",TableS5$Species)
#And save the table.
write.csv(TableS5, file = "export_for_publication/TableS5.csv", na="")



#CREATE Table S6 FOR PUBLICATION, AN OVERVIEW OF MODEL SELECTION FOR MODELS CORRELATING MICROBIOME/DIET WITH CONSUMER COMMUNITY (SHELLS) AND SPECIES.
#Get the anova results.
TableS6<-rbind(as.data.frame(anova(model.Chao1.diet1,model.Chao1.diet2,model.Chao1.diet3,model.Chao1.diet4,model.Chao1.diet5))[,c(1:2,4,6:8)],
               as.data.frame(anova(model.PD.diet1,model.PD.diet2,model.PD.diet3,model.PD.diet4,model.PD.diet5))[,c(1:2,4,6:8)],
               as.data.frame(anova(model.Shannon.diet1,model.Shannon.diet2,model.Shannon.diet3,model.Shannon.diet4,model.Shannon.diet5))[,c(1:2,4,6:8)],
               as.data.frame(anova(model.Chao1.micr1,model.Chao1.micr2,model.Chao1.micr3,model.Chao1.micr4,model.Chao1.micr5))[,c(1:2,4,6:8)],
               as.data.frame(anova(model.PD.micr1,model.PD.micr2,model.PD.micr3,model.PD.micr4,model.PD.micr5))[,c(1:2,4,6:8)],
               as.data.frame(anova(model.Shannon.micr1,model.Shannon.micr2,model.Shannon.micr3,model.Shannon.micr4,model.Shannon.micr5))[,c(1:2,4,6:8)])
#Reorder the table.
TableS6<-TableS6[seq(dim(TableS6)[1],1),]
#Add the model formulae.
TableS6<-cbind(as.data.frame(rbind(matrix(strsplit(as.character(model.Shannon.micr1$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.Shannon.micr2$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.Shannon.micr3$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.Shannon.micr4$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.Shannon.micr5$call)[2],"~")[[1]],nrow=1),
                                   matrix(strsplit(as.character(model.PD.micr1$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.PD.micr2$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.PD.micr3$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.PD.micr4$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.PD.micr5$call)[2],"~")[[1]],nrow=1),
                                   matrix(strsplit(as.character(model.Chao1.micr1$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.Chao1.micr2$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.Chao1.micr3$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.Chao1.micr4$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.Chao1.micr5$call)[2],"~")[[1]],nrow=1),
                                   matrix(strsplit(as.character(model.Shannon.diet1$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.Shannon.diet2$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.Shannon.diet3$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.Shannon.diet4$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.Shannon.diet5$call)[2],"~")[[1]],nrow=1),
                                   matrix(strsplit(as.character(model.PD.diet1$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.PD.diet2$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.PD.diet3$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.PD.diet4$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.PD.diet5$call)[2],"~")[[1]],nrow=1),
                                   matrix(strsplit(as.character(model.Chao1.diet1$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.Chao1.diet2$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.Chao1.diet3$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.Chao1.diet4$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.Chao1.diet5$call)[2],"~")[[1]],nrow=1))),
               TableS6)
#Add response and metric variable definitions.
TableS6<-cbind(rep(c("Diversity (Shannon)",rep("",4),"Phylogenetic diversity (Faith's PD)",rep("",4),"Richness (Chao1)",rep("",4)),2),TableS6)
TableS6<-cbind(c("Microbiome",rep("",14),"Diet",rep("",14)),TableS6)
#After having checked the above names are placed correctly in the table, we can delete column 3.
TableS6<-TableS6[,-3]
#Update some text for publication-ready style.
TableS6$V2<-gsub("Shannon.comm.shells","Host community (shells) diversity (Shannon)",TableS6$V2)
TableS6$V2<-gsub("PD.comm.shells","Host community (shells) phylogenetic diversity (PD)",TableS6$V2)
TableS6$V2<-gsub("Chao1.comm.shells","Host community (shells) richness (Chao1)",TableS6$V2)
TableS6$V2<-gsub("GENUS_SPECIES","Species",TableS6$V2)
TableS6$V2<-gsub(" + (1 | LOCATION_PLOT)","",TableS6$V2,fixed = TRUE)
TableS6$V2<-gsub("(1 | LOCATION_PLOT)","Empty",TableS6$V2,fixed = TRUE)
#Round numbers.
TableS6[,5]<-round(as.numeric(TableS6[,5]),2);TableS6[,6]<-round(as.numeric(TableS6[,6]),2);TableS6[,7]<-round(as.numeric(TableS6[,7]),2)
TableS6[,9]<-round(as.numeric(TableS6[,9]),3)
TableS6[is.na(TableS6$`Pr(>Chisq)`)==FALSE & TableS6$`Pr(>Chisq)`<0.001,9]<-"<0.001"
#Remove rownames and set proper column names.
rownames(TableS6)<-NULL
colnames(TableS6)<-c("Response","Metric","Fixed effect(s)","df","AIC","Log-lik","Chi-square","df","P")
#And save the table.
write.csv(TableS6, file = "export_for_publication/TableS6.csv", na="")


#CREATE FIGURE S2 FOR PUBLICATION.
#We use package ggpubr to quickly check some regressions and focus on PD diversity.
library(ggpubr)
FigureS2<-plot_grid(ggscatter(data=data_byPlot_allSpecies_summary, x="PD.comm.shells", y="PD.diet.mean", add="reg.line", add.params = list(linetype="dashed"))+
                               stat_cor()+
                               geom_errorbar(aes(ymax=PD.diet.upperCI, ymin=PD.diet.lowerCI))+
                               stat_regline_equation(label.y = 3.2)+
                               labs(x="Consumer community PD", y="Diet PD"),
                             ggscatter(data=data_byPlot_allSpecies_summary, x="PD.comm.shells", y="PD.micr.mean", add="reg.line", add.params = list(linetype="dashed"))+
                               stat_cor()+
                               geom_errorbar(aes(ymax=PD.micr.upperCI, ymin=PD.micr.lowerCI))+
                               stat_regline_equation(label.y = 100)+
                               labs(x="Consumer community PD", y="Microbiome PD"),
                             ggscatter(data=data_byPlot_allSpecies_summary, x="PD.diet.mean", y="PD.micr.mean", add="reg.line", add.params = list(linetype="dashed"))+
                               stat_cor()+
                               geom_errorbar(aes(ymax=PD.micr.upperCI, ymin=PD.micr.lowerCI))+
                               geom_errorbarh(aes(xmax=PD.diet.upperCI, xmin=PD.diet.lowerCI))+
                               stat_regline_equation(label.y = 100)+
                               labs(x="Diet PD", y="Microbiome PD"),
                             ggscatter(data=data, x="PD.comm.shells", y="PD.diet", add="reg.line", size=1, add.params = list(linetype="dashed"))+
                               stat_cor()+
                               stat_regline_equation(label.y = 3.2)+
                               labs(x="Consumer community PD", y="Diet PD"),
                             ggscatter(data=data, x="PD.comm.shells", y="PD.micr", add="reg.line", size=1, add.params = list(linetype="dashed"))+
                               stat_cor()+
                               stat_regline_equation(label.y = 55)+
                               labs(x="Consumer community PD", y="Microbiome PD"),
                             ggscatter(data=data, x="PD.diet", y="PD.micr", add="reg.line", size=1, add.params = list(linetype="dashed"))+
                               stat_cor()+
                               stat_regline_equation(label.y = 55)+
                               labs(x="Diet PD", y="Microbiome PD"),
                             ggscatter(data=data_byPlot_allSpecies_summary, x="Chao1.comm.shells", y="Chao1.diet.mean", add="reg.line", add.params = list(linetype="dashed"))+
                               stat_cor()+
                               geom_errorbar(aes(ymax=Chao1.diet.upperCI, ymin=Chao1.diet.lowerCI))+
                               stat_regline_equation(label.y = 60)+
                               labs(x="Consumer community Chao1", y="Diet Chao1"),
                             ggscatter(data=data_byPlot_allSpecies_summary, x="Chao1.comm.shells", y="Chao1.micr.mean", add="reg.line", add.params = list(linetype="dashed"))+
                               stat_cor()+
                               geom_errorbar(aes(ymax=Chao1.micr.upperCI, ymin=Chao1.micr.lowerCI))+
                               stat_regline_equation(label.y = 1500)+
                               labs(x="Consumer community Chao1", y="Microbiome Chao1"),
                             ggscatter(data=data_byPlot_allSpecies_summary, x="Chao1.diet.mean", y="Chao1.micr.mean", add="reg.line", add.params = list(linetype="dashed"))+
                               stat_cor()+
                               geom_errorbar(aes(ymax=Chao1.micr.upperCI, ymin=Chao1.micr.lowerCI))+
                               geom_errorbarh(aes(xmax=Chao1.diet.upperCI, xmin=Chao1.diet.lowerCI))+
                               stat_regline_equation(label.y = 1500)+
                               labs(x="Diet Chao1", y="Microbiome Chao1"),
                             ggscatter(data=data, x="Chao1.comm.shells", y="Chao1.diet", add="reg.line", size=1, add.params = list(linetype="dashed"))+
                               stat_cor()+
                               stat_regline_equation(label.y = 22)+
                               labs(x="Consumer community Chao1", y="Diet Chao1"),
                             ggscatter(data=data, x="Chao1.comm.shells", y="Chao1.micr", add="reg.line", size=1, add.params = list(linetype="dashed"))+
                               stat_cor()+
                               stat_regline_equation(label.y = 390)+
                               labs(x="Consumer community Chao1", y="Microbiome Chao1"),
                             ggscatter(data=data, x="Chao1.diet", y="Chao1.micr", add="reg.line", size=1, add.params = list(linetype="dashed"))+
                               stat_cor()+
                               stat_regline_equation(label.y = 390)+
                               labs(x="Diet Chao1", y="Microbiome Chao1"),
                             align = 'hv',
                             rows = 4, cols = 3, labels = c("(A)","(B)","(C)","","","","(D)","(E)","(F)","","",""))
ggsave(FigureS2, file = "export_for_publication/FigureS2.pdf", width = 10, height = 13)


#CREATE Figure S3 FOR PUBLICATION, A GRAPHICAL REPRESENTATION OF THE BEST MODELS FOUND FROM GLMM.

#First create a table with unique combinations of colour and linetype to represent the 19 different plots in our study.
color_lty_cross<-expand.grid(ltypes = 1:4, colors = c("#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"),stringsAsFactors = F)
color_lty_cross<-color_lty_cross[c(-4,-12,-16,-20,-24),]

#Figure S3A: model fit of diet~host community (shells) diversity (Shannon)

#Use model results from best model, model.Shannon.diet1 to predict.
model.Shannon.diet.predict<-expand.grid(Shannon.comm.shells= seq(from=min(data$Shannon.comm.shells), to=max(data$Shannon.comm.shells), by=0.01),
                                        GENUS_SPECIES=c("A_jagori","P_concinnum","G_similis"),
                                        LOCATION_PLOT=unique(data$LOCATION_PLOT))
model.Shannon.diet.predict$predict<-predict(model.Shannon.diet3, type="response",newdata=model.Shannon.diet.predict)

#And plots observed data with model predictions on top.
(FigureS3A<-ggplot(data=model.Shannon.diet1$frame, aes(x=Shannon.comm.shells, y=Shannon.diet))+
    geom_point()+
    scale_color_manual(values = color_lty_cross$colors[1:19])+
    scale_linetype_manual(values = color_lty_cross$ltypes[1:19]) +
    facet_wrap(.~GENUS_SPECIES)+
    geom_line(data=model.Shannon.diet.predict, aes(x=Shannon.comm.shells,y=predict,colour=LOCATION_PLOT, linetype=LOCATION_PLOT), lwd=1)+
    ggtitle("Diet vs. consumer community diversity (Shannon)")+
    theme_bw()+labs(x="Shell consumer community diversity", y= "Diet diversity")+
    theme(legend.key.width = unit(2,"cm")))

#Figure S3B: model fit of diet~host community (shells) phylogenetic diversity (Faith's PD)

#Use model results from best model, model.PD.diet1 to predict.
model.PD.diet.predict<-expand.grid(PD.comm.shells= seq(from=min(data$PD.comm.shells), to=max(data$PD.comm.shells), by=0.01),
                                   GENUS_SPECIES=c("A_jagori","P_concinnum","G_similis"),
                                   LOCATION_PLOT=unique(data$LOCATION_PLOT))
model.PD.diet.predict$predict<-predict(model.PD.diet1, type="response",newdata=model.PD.diet.predict)

#And plots observed data with model predictions on top.
(FigureS3B<-ggplot(data=model.PD.diet1$frame, aes(x=PD.comm.shells, y=log10(PD.diet)))+
    geom_point()+
    scale_color_manual(values = color_lty_cross$colors[1:19])+
    scale_linetype_manual(values = color_lty_cross$ltypes[1:19]) +
    facet_wrap(.~GENUS_SPECIES)+
    geom_line(data=model.PD.diet.predict, aes(x=PD.comm.shells,y=log10(predict),colour=LOCATION_PLOT, linetype=LOCATION_PLOT), lwd=1)+
    ggtitle("Diet vs. consumer community phylogenetic diversity (Faith's PD)")+
    theme_bw()+labs(x="Shell consumer community phylogenetic diversity", y= "Log10(diet phylogenetic diversity)")+
    theme(legend.key.width = unit(2,"cm")))

#Figure S3C: model fit of diet~host community (shells) richness (Chao1)

#Use model results from best model, model.Chao1.diet1 to predict.
model.Chao1.diet.predict<-expand.grid(Chao1.comm.shells= seq(from=min(data$Chao1.comm.shells), to=max(data$Chao1.comm.shells), by=0.01),
                                      GENUS_SPECIES=c("A_jagori","P_concinnum","G_similis"),
                                      LOCATION_PLOT=unique(data$LOCATION_PLOT))
model.Chao1.diet.predict$predict<-predict(model.Chao1.diet1, type="response",newdata=model.Chao1.diet.predict)

#And plots observed data with model predictions on top.
(FigureS3C<-ggplot(data=model.Chao1.diet1$frame, aes(x=Chao1.comm.shells, y=log10(Chao1.diet)))+
    geom_point()+
    scale_color_manual(values = color_lty_cross$colors[1:19])+
    scale_linetype_manual(values = color_lty_cross$ltypes[1:19]) +
    facet_wrap(.~GENUS_SPECIES)+
    geom_line(data=model.Chao1.diet.predict, aes(x=Chao1.comm.shells,y=log10(predict),colour=LOCATION_PLOT, linetype=LOCATION_PLOT), lwd=1)+
    ggtitle("Diet vs. consumer community richness (Chao1)")+
    theme_bw()+labs(x="Shell consumer community richness", y= "Log10(diet richness)")+
    theme(legend.key.width = unit(2,"cm")))

#Figure S3D: model fit of microbiome~host community (shells) diversity (Shannon)

#Use model results from best model, model.Shannon.micr1 to predict.
model.Shannon.micr.predict<-expand.grid(Shannon.comm.shells= seq(from=min(data$Shannon.comm.shells), to=max(data$Shannon.comm.shells), by=0.01),
                                        GENUS_SPECIES=c("A_jagori","P_concinnum","G_similis"),
                                        LOCATION_PLOT=unique(data$LOCATION_PLOT))
model.Shannon.micr.predict$predict<-predict(model.Shannon.micr1, type="response",newdata=model.Shannon.micr.predict)

#And plots observed data with model predictions on top.
(FigureS3D<-ggplot(data=model.Shannon.micr1$frame, aes(x=Shannon.comm.shells, y=log10(Shannon.micr)))+
  geom_point()+
  scale_color_manual(values = color_lty_cross$colors[1:19])+
  scale_linetype_manual(values = color_lty_cross$ltypes[1:19]) +
  facet_wrap(.~GENUS_SPECIES)+
  geom_line(data=model.Shannon.micr.predict, aes(x=Shannon.comm.shells,y=log10(predict),colour=LOCATION_PLOT, linetype=LOCATION_PLOT), lwd=1)+
  ggtitle("Microbiome vs. consumer community diversity (Shannon)")+
  theme_bw()+labs(x="Shell consumer community diversity", y= "Log10(microbiome diversity)")+
  theme(legend.key.width = unit(2,"cm")))

#Figure S3E: model fit of microbiome~host community (shells) phylogenetic diversity (Faith's PD)

#Use model results from best model, model.PD.micr1 to predict.
model.PD.micr.predict<-expand.grid(PD.comm.shells= seq(from=min(data$PD.comm.shells), to=max(data$PD.comm.shells), by=0.01),
                                        GENUS_SPECIES=c("A_jagori","P_concinnum","G_similis"),
                                        LOCATION_PLOT=unique(data$LOCATION_PLOT))
model.PD.micr.predict$predict<-predict(model.PD.micr1, type="response",newdata=model.PD.micr.predict)

#And plots observed data with model predictions on top.
(FigureS3E<-ggplot(data=model.PD.micr1$frame, aes(x=PD.comm.shells, y=PD.micr))+
    geom_point()+
    scale_color_manual(values = color_lty_cross$colors[1:19])+
    scale_linetype_manual(values = color_lty_cross$ltypes[1:19]) +
    facet_wrap(.~GENUS_SPECIES)+
    geom_line(data=model.PD.micr.predict, aes(x=PD.comm.shells,y=predict,colour=LOCATION_PLOT, linetype=LOCATION_PLOT), lwd=1)+
    ggtitle("Microbiome vs. consumer community phylogenetic diversity (Faith's PD)")+
    theme_bw()+labs(x="Shell consumer community phylogenetic diversity", y= "Microbiome phylogenetic diversity")+
    theme(legend.key.width = unit(2,"cm")))

#Figure S3F: model fit of microbiome~host community (shells) richness (Chao1)

#Use model results from best model, model.Chao1.micr1 to predict.
model.Chao1.micr.predict<-expand.grid(Chao1.comm.shells= seq(from=min(data$Chao1.comm.shells), to=max(data$Chao1.comm.shells), by=0.01),
                                   GENUS_SPECIES=c("A_jagori","P_concinnum","G_similis"),
                                   LOCATION_PLOT=unique(data$LOCATION_PLOT))
model.Chao1.micr.predict$predict<-predict(model.Chao1.micr1, type="response",newdata=model.Chao1.micr.predict)

#And plots observed data with model predictions on top.
(FigureS3F<-ggplot(data=model.Chao1.micr1$frame, aes(x=Chao1.comm.shells, y=Chao1.micr))+
    geom_point()+
    scale_color_manual(values = color_lty_cross$colors[1:19])+
    scale_linetype_manual(values = color_lty_cross$ltypes[1:19]) +
    facet_wrap(.~GENUS_SPECIES)+
    geom_line(data=model.Chao1.micr.predict, aes(x=Chao1.comm.shells,y=predict,colour=LOCATION_PLOT, linetype=LOCATION_PLOT), lwd=1)+
    ggtitle("Microbiome vs. consumer community richness (Chao1)")+
    theme_bw()+labs(x="Shell consumer community richness", y= "Microbiome richness")+
    theme(legend.key.width = unit(2,"cm")))

#And export figures to be printed on two pages.
ggsave(plot_grid(FigureS3A, FigureS3B, FigureS3C, ncol=1, labels = c("(A)","(B)","(C)"), align = 'v'), file = "export_for_publication/FigureS3ABC.pdf", width = 9, height = 12)
ggsave(plot_grid(FigureS3D, FigureS3E, FigureS3F, ncol=1, labels = c("(D)","(E)","(F)"), align = 'v'), file = "export_for_publication/FigureS3DEF.pdf", width = 9, height = 12)




#CREATE Figure S4 FOR PUBLICATION, A GRAPHICAL REPRESENTATION OF THE BEST SPECIES MODELS FOUND FROM GLMM.

#Figure S4A: model fit of diet~host community (shells) diversity (Shannon) by target species.

#For A. jagori.

  #Use model results from best model, model.Shannon.diet1, to predict.
  model.Shannon.A_jag.diet.predict<-expand.grid(Shannon.comm.shells= seq(from=min(data$Shannon.comm.shells[data$GENUS_SPECIES=="A_jagori"]), to=max(data$Shannon.comm.shells[data$GENUS_SPECIES=="A_jagori"]), by=0.01),
                                                LOCATION_PLOT=unique(model.Shannon.A_jag.diet4$frame$LOCATION_PLOT))
  model.Shannon.A_jag.diet.predict$LOCATION<-sapply(strsplit(as.character(model.Shannon.A_jag.diet.predict$LOCATION_PLOT), "_"),'[', 1)
  model.Shannon.A_jag.diet.predict$predict<-predict(model.Shannon.A_jag.diet5, type="response",newdata=model.Shannon.A_jag.diet.predict)
  model.Shannon.A_jag.diet.predict$PLOT<-sapply(strsplit(as.character(model.Shannon.A_jag.diet.predict$LOCATION_PLOT), "_"),'[', 2)
  
#For G. similis s.l.
  
  #Use model results from best model, model.Shannon.diet1, to predict.
  model.Shannon.G_sim.diet.predict<-expand.grid(Shannon.comm.shells= seq(from=min(data$Shannon.comm.shells[data$GENUS_SPECIES=="G_similis"]), to=max(data$Shannon.comm.shells[data$GENUS_SPECIES=="G_similis"]), by=0.01),
                                                LOCATION_PLOT=unique(model.Shannon.G_sim.diet5$frame$LOCATION_PLOT))
  model.Shannon.G_sim.diet.predict$LOCATION<-sapply(strsplit(as.character(model.Shannon.G_sim.diet.predict$LOCATION_PLOT), "_"),'[', 1)
  model.Shannon.G_sim.diet.predict$predict<-predict(model.Shannon.G_sim.diet5, type="response",newdata=model.Shannon.G_sim.diet.predict)
  model.Shannon.G_sim.diet.predict$PLOT<-sapply(strsplit(as.character(model.Shannon.G_sim.diet.predict$LOCATION_PLOT), "_"),'[', 2)

#For P. concinnum.
  
  #Use model results from best model, model.Shannon.diet1, to predict.
  model.Shannon.P_con.diet.predict<-expand.grid(Shannon.comm.shells= seq(from=min(data$Shannon.comm.shells[data$GENUS_SPECIES=="P_concinnum"]), to=max(data$Shannon.comm.shells[data$GENUS_SPECIES=="P_concinnum"]), by=0.01),
                                                LOCATION_PLOT=unique(model.Shannon.P_con.diet3$frame$LOCATION_PLOT))
  model.Shannon.P_con.diet.predict$LOCATION<-sapply(strsplit(as.character(model.Shannon.P_con.diet.predict$LOCATION_PLOT), "_"),'[', 1)
  model.Shannon.P_con.diet.predict$predict<-predict(model.Shannon.P_con.diet3, type="response",newdata=model.Shannon.P_con.diet.predict)
  model.Shannon.P_con.diet.predict$PLOT<-sapply(strsplit(as.character(model.Shannon.P_con.diet.predict$LOCATION_PLOT), "_"),'[', 2)

#Collect data for the three target species.
  model.Shannon.species.diet.observed<-rbind(cbind(GENUS_SPECIES="A. jagori",data[data$GENUS_SPECIES=="A_jagori" & is.na(data$Shannon.diet)==F & data$Shannon.diet>0 & data$LOCATION %in% names(table(data[data$GENUS_SPECIES=="A_jagori",]$LOCATION)[(table(data[data$GENUS_SPECIES=="A_jagori",]$LOCATION)>31)]),]),
                                             cbind(GENUS_SPECIES="G. similis s.l.",data[data$GENUS_SPECIES=="G_similis" & is.na(data$Shannon.diet)==F & data$Shannon.diet>0 & data$LOCATION %in% names(table(data[data$GENUS_SPECIES=="G_similis",]$LOCATION)[(table(data[data$GENUS_SPECIES=="G_similis",]$LOCATION)>16)]),]),
                                             cbind(GENUS_SPECIES="P. concinnum",data[data$GENUS_SPECIES=="P_concinnum" & is.na(data$Shannon.diet)==F & data$Shannon.diet>0 & data$LOCATION %in% names(table(data[data$GENUS_SPECIES=="P_concinnum",]$LOCATION)[(table(data[data$GENUS_SPECIES=="P_concinnum",]$LOCATION)>9)]),]))
  model.Shannon.species.diet.observed<-model.Shannon.species.diet.observed[,-4]
  
  model.Shannon.species.diet.predict<-rbind(cbind(GENUS_SPECIES="A. jagori",model.Shannon.A_jag.diet.predict),
                                            cbind(GENUS_SPECIES="G. similis s.l.",model.Shannon.G_sim.diet.predict),
                                            cbind(GENUS_SPECIES="P. concinnum",model.Shannon.P_con.diet.predict))
#Create plot.
(FigureS4A<-ggplot()+
    geom_point(data=model.Shannon.species.diet.observed, aes(x=Shannon.comm.shells, y=Shannon.diet), size=0.5)+
    geom_line(data=model.Shannon.species.diet.predict, aes(x=Shannon.comm.shells,y=predict,linetype=PLOT), lwd=0.5)+
    facet_grid(GENUS_SPECIES~LOCATION)+
    ggtitle("Diet vs. consumer community diversity (Shannon)")+
    theme_bw()+labs(x="Shell consumer community diversity", y= "Diet diversity")+
    theme(legend.key.width = unit(2,"cm")))

#Figure S4B: model fit of diet~host community (shells) phylogenetic diversity (Faith's PD) by target species.

#For A. jagori.
  
  #Use model results from best model, model.PD.diet1, to predict.
  model.PD.A_jag.diet.predict<-expand.grid(PD.comm.shells= seq(from=min(data$PD.comm.shells[data$GENUS_SPECIES=="A_jagori"]), to=max(data$PD.comm.shells[data$GENUS_SPECIES=="A_jagori"]), by=0.01),
                                           LOCATION_PLOT=unique(model.PD.A_jag.diet4$frame$LOCATION_PLOT))
  model.PD.A_jag.diet.predict$LOCATION<-sapply(strsplit(as.character(model.PD.A_jag.diet.predict$LOCATION_PLOT), "_"),'[', 1)
  model.PD.A_jag.diet.predict$predict<-predict(model.PD.A_jag.diet4, type="response",newdata=model.PD.A_jag.diet.predict)
  model.PD.A_jag.diet.predict$PLOT<-sapply(strsplit(as.character(model.PD.A_jag.diet.predict$LOCATION_PLOT), "_"),'[', 2)

#For G. similis s.l.
  
  #Use model results from best model, model.PD.diet1, to predict.
  model.PD.G_sim.diet.predict<-expand.grid(PD.comm.shells= seq(from=min(data$PD.comm.shells[data$GENUS_SPECIES=="G_similis"]), to=max(data$PD.comm.shells[data$GENUS_SPECIES=="G_similis"]), by=0.01),
                                           LOCATION_PLOT=unique(model.PD.G_sim.diet4$frame$LOCATION_PLOT))
  model.PD.G_sim.diet.predict$LOCATION<-sapply(strsplit(as.character(model.PD.G_sim.diet.predict$LOCATION_PLOT), "_"),'[', 1)
  model.PD.G_sim.diet.predict$predict<-predict(model.PD.G_sim.diet4, type="response",newdata=model.PD.G_sim.diet.predict)
  model.PD.G_sim.diet.predict$PLOT<-sapply(strsplit(as.character(model.PD.G_sim.diet.predict$LOCATION_PLOT), "_"),'[', 2)

#For P. concinnum.
  
  #Use model results from best model, model.PD.diet1, to predict.
  model.PD.P_con.diet.predict<-expand.grid(PD.comm.shells= seq(from=min(data$PD.comm.shells[data$GENUS_SPECIES=="P_concinnum"]), to=max(data$PD.comm.shells[data$GENUS_SPECIES=="P_concinnum"]), by=0.01),
                                           LOCATION_PLOT=unique(model.PD.P_con.diet5$frame$LOCATION_PLOT))
  model.PD.P_con.diet.predict$LOCATION<-sapply(strsplit(as.character(model.PD.P_con.diet.predict$LOCATION_PLOT), "_"),'[', 1)
  model.PD.P_con.diet.predict$predict<-predict(model.PD.P_con.diet5, type="response",newdata=model.PD.P_con.diet.predict)
  model.PD.P_con.diet.predict$PLOT<-sapply(strsplit(as.character(model.PD.P_con.diet.predict$LOCATION_PLOT), "_"),'[', 2)

#Collect data for the three target species.
  model.PD.species.diet.observed<-rbind(cbind(GENUS_SPECIES="A. jagori",data[data$GENUS_SPECIES=="A_jagori" & is.na(data$PD.diet)==F & data$PD.diet>0 & data$LOCATION %in% names(table(data[data$GENUS_SPECIES=="A_jagori",]$LOCATION)[(table(data[data$GENUS_SPECIES=="A_jagori",]$LOCATION)>31)]),]),
                                        cbind(GENUS_SPECIES="G. similis s.l.",data[data$GENUS_SPECIES=="G_similis" & is.na(data$PD.diet)==F & data$PD.diet>0 & data$LOCATION %in% names(table(data[data$GENUS_SPECIES=="G_similis",]$LOCATION)[(table(data[data$GENUS_SPECIES=="G_similis",]$LOCATION)>16)]),]),
                                        cbind(GENUS_SPECIES="P. concinnum",data[data$GENUS_SPECIES=="P_concinnum" & is.na(data$PD.diet)==F & data$PD.diet>0 & data$LOCATION %in% names(table(data[data$GENUS_SPECIES=="P_concinnum",]$LOCATION)[(table(data[data$GENUS_SPECIES=="P_concinnum",]$LOCATION)>9)]),]))
  model.PD.species.diet.observed<-model.PD.species.diet.observed[,-4]
  
  model.PD.species.diet.predict<-rbind(cbind(GENUS_SPECIES="A. jagori",model.PD.A_jag.diet.predict),
                                       cbind(GENUS_SPECIES="G. similis s.l.",model.PD.G_sim.diet.predict),
                                       cbind(GENUS_SPECIES="P. concinnum",model.PD.P_con.diet.predict))
#Create plot.
(FigureS4B<-ggplot()+
    geom_point(data=model.PD.species.diet.observed, aes(x=PD.comm.shells, y=PD.diet), size=0.5)+
    geom_line(data=model.PD.species.diet.predict, aes(x=PD.comm.shells,y=predict,linetype=PLOT), lwd=0.5)+
    facet_grid(GENUS_SPECIES~LOCATION)+
    ggtitle("Diet vs. consumer community phylogenetic diversity (Faith's PD)")+
    theme_bw()+
    theme_bw()+labs(x="Shell consumer community phylogenetic diversity", y= "Diet phylogenetic diversity")+
    theme(legend.key.width = unit(2,"cm")))

#Figure S4C: model fit of diet~host community (shells) richness (Chao1) by target species.

#For A. jagori.
  
  #Use model results from best model, model.Chao1.diet1, to predict.
  model.Chao1.A_jag.diet.predict<-expand.grid(Chao1.comm.shells= seq(from=min(data$Chao1.comm.shells[data$GENUS_SPECIES=="A_jagori"]), to=max(data$Chao1.comm.shells[data$GENUS_SPECIES=="A_jagori"]), by=0.01),
                                              LOCATION_PLOT=unique(model.Chao1.A_jag.diet1$frame$LOCATION_PLOT))
  model.Chao1.A_jag.diet.predict$LOCATION<-sapply(strsplit(as.character(model.Chao1.A_jag.diet.predict$LOCATION_PLOT), "_"),'[', 1)
  model.Chao1.A_jag.diet.predict$predict<-predict(model.Chao1.A_jag.diet1, type="response",newdata=model.Chao1.A_jag.diet.predict)
  model.Chao1.A_jag.diet.predict$PLOT<-sapply(strsplit(as.character(model.Chao1.A_jag.diet.predict$LOCATION_PLOT), "_"),'[', 2)

#For G. similis s.l.
  
  #Use model results from best model, model.Chao1.diet1, to predict.
  model.Chao1.G_sim.diet.predict<-expand.grid(Chao1.comm.shells= seq(from=min(data$Chao1.comm.shells[data$GENUS_SPECIES=="G_similis"]), to=max(data$Chao1.comm.shells[data$GENUS_SPECIES=="G_similis"]), by=0.01),
                                              LOCATION_PLOT=unique(model.Chao1.G_sim.diet1$frame$LOCATION_PLOT))
  model.Chao1.G_sim.diet.predict$LOCATION<-sapply(strsplit(as.character(model.Chao1.G_sim.diet.predict$LOCATION_PLOT), "_"),'[', 1)
  model.Chao1.G_sim.diet.predict$predict<-predict(model.Chao1.G_sim.diet1, type="response",newdata=model.Chao1.G_sim.diet.predict)
  model.Chao1.G_sim.diet.predict$PLOT<-sapply(strsplit(as.character(model.Chao1.G_sim.diet.predict$LOCATION_PLOT), "_"),'[', 2)

#For P. concinnum.
  
  #Use model results from best model, model.Chao1.diet1, to predict.
  model.Chao1.P_con.diet.predict<-expand.grid(Chao1.comm.shells= seq(from=min(data$Chao1.comm.shells[data$GENUS_SPECIES=="P_concinnum"]), to=max(data$Chao1.comm.shells[data$GENUS_SPECIES=="P_concinnum"]), by=0.01),
                                              LOCATION_PLOT=unique(model.Chao1.P_con.diet3$frame$LOCATION_PLOT))
  model.Chao1.P_con.diet.predict$LOCATION<-sapply(strsplit(as.character(model.Chao1.P_con.diet.predict$LOCATION_PLOT), "_"),'[', 1)
  model.Chao1.P_con.diet.predict$predict<-predict(model.Chao1.P_con.diet3, type="response",newdata=model.Chao1.P_con.diet.predict)
  model.Chao1.P_con.diet.predict$PLOT<-sapply(strsplit(as.character(model.Chao1.P_con.diet.predict$LOCATION_PLOT), "_"),'[', 2)

#Collect data for the three target species.
  model.Chao1.species.diet.observed<-rbind(cbind(GENUS_SPECIES="A. jagori", data[data$GENUS_SPECIES=="A_jagori" & is.na(data$Chao1.diet)==F & data$Chao1.diet>0 & data$LOCATION %in% names(table(data[data$GENUS_SPECIES=="A_jagori",]$LOCATION)[(table(data[data$GENUS_SPECIES=="A_jagori",]$LOCATION)>9)]),]),
                                           cbind(GENUS_SPECIES="G. similis s.l.", data[data$GENUS_SPECIES=="G_similis" & is.na(data$Chao1.diet)==F & data$Chao1.diet>0 & data$LOCATION %in% names(table(data[data$GENUS_SPECIES=="G_similis",]$LOCATION)[(table(data[data$GENUS_SPECIES=="G_similis",]$LOCATION)>16)]),]),
                                           cbind(GENUS_SPECIES="P. concinnum", data[data$GENUS_SPECIES=="P_concinnum" & is.na(data$Chao1.diet)==F & data$Chao1.diet>0 & data$LOCATION %in% names(table(data[data$GENUS_SPECIES=="P_concinnum",]$LOCATION)[(table(data[data$GENUS_SPECIES=="P_concinnum",]$LOCATION)>9)]),]))
  model.Chao1.species.diet.observed<-model.Chao1.species.diet.observed[,-4]
  
  model.Chao1.species.diet.predict<-rbind(cbind(GENUS_SPECIES="A. jagori",model.Chao1.A_jag.diet.predict),
                                          cbind(GENUS_SPECIES="G. similis s.l.",model.Chao1.G_sim.diet.predict),
                                          cbind(GENUS_SPECIES="P. concinnum",model.Chao1.P_con.diet.predict))

#Create plot.
  (FigureS4C<-ggplot()+
    geom_point(data=model.Chao1.species.diet.observed, aes(x=Chao1.comm.shells, y=log10(Chao1.diet)), size=0.5)+
    geom_line(data=model.Chao1.species.diet.predict, aes(x=Chao1.comm.shells,y=log10(predict),linetype=PLOT), lwd=0.5)+
    facet_grid(GENUS_SPECIES~LOCATION)+
    ggtitle("Diet vs. consumer community richness (Chao1)")+
    theme_bw()+
    theme_bw()+labs(x="Shell consumer community richness", y= "Log10(diet richness)")+
    theme(legend.key.width = unit(2,"cm")))

#Figure S4D: model fit of microbiome~host community (shells) diversity (Shannon) by target species.

#For A. jagori.

  #Use model results from best model, model.Shannon.micr1, to predict.
  model.Shannon.A_jag.micr.predict<-expand.grid(Shannon.comm.shells= seq(from=min(data$Shannon.comm.shells[data$GENUS_SPECIES=="A_jagori"]), to=max(data$Shannon.comm.shells[data$GENUS_SPECIES=="A_jagori"]), by=0.01),
                                          LOCATION_PLOT=unique(model.Shannon.A_jag.micr1$frame$LOCATION_PLOT))
  model.Shannon.A_jag.micr.predict$LOCATION<-sapply(strsplit(as.character(model.Shannon.A_jag.micr.predict$LOCATION_PLOT), "_"),'[', 1)
  model.Shannon.A_jag.micr.predict$predict<-predict(model.Shannon.A_jag.micr1, type="response",newdata=model.Shannon.A_jag.micr.predict)
  model.Shannon.A_jag.micr.predict$PLOT<-sapply(strsplit(as.character(model.Shannon.A_jag.micr.predict$LOCATION_PLOT), "_"),'[', 2)

#For G. similis s.l.
  
  #Use model results from best model, model.Shannon.micr1, to predict.
  model.Shannon.G_sim.micr.predict<-expand.grid(Shannon.comm.shells= seq(from=min(data$Shannon.comm.shells[data$GENUS_SPECIES=="G_similis"]), to=max(data$Shannon.comm.shells[data$GENUS_SPECIES=="G_similis"]), by=0.01),
                                                LOCATION_PLOT=unique(model.Shannon.G_sim.micr1$frame$LOCATION_PLOT))
  model.Shannon.G_sim.micr.predict$LOCATION<-sapply(strsplit(as.character(model.Shannon.G_sim.micr.predict$LOCATION_PLOT), "_"),'[', 1)
  model.Shannon.G_sim.micr.predict$predict<-predict(model.Shannon.G_sim.micr1, type="response",newdata=model.Shannon.G_sim.micr.predict)
  model.Shannon.G_sim.micr.predict$PLOT<-sapply(strsplit(as.character(model.Shannon.G_sim.micr.predict$LOCATION_PLOT), "_"),'[', 2)

#For P. concinnum.
  
  #Use model results from best model, model.Shannon.micr1, to predict.
  model.Shannon.P_con.micr.predict<-expand.grid(Shannon.comm.shells= seq(from=min(data$Shannon.comm.shells[data$GENUS_SPECIES=="P_concinnum"]), to=max(data$Shannon.comm.shells[data$GENUS_SPECIES=="P_concinnum"]), by=0.01),
                                                LOCATION_PLOT=unique(model.Shannon.P_con.micr2$frame$LOCATION_PLOT))
  model.Shannon.P_con.micr.predict$LOCATION<-sapply(strsplit(as.character(model.Shannon.P_con.micr.predict$LOCATION_PLOT), "_"),'[', 1)
  model.Shannon.P_con.micr.predict$predict<-predict(model.Shannon.P_con.micr2, type="response",newdata=model.Shannon.P_con.micr.predict)
  model.Shannon.P_con.micr.predict$PLOT<-sapply(strsplit(as.character(model.Shannon.P_con.micr.predict$LOCATION_PLOT), "_"),'[', 2)
  
#Collect data for the three target species.
  model.Shannon.species.micr.observed<-rbind(cbind(GENUS_SPECIES="A. jagori",model.Shannon.A_jag.micr1$frame),
                                             cbind(GENUS_SPECIES="G. similis s.l.",model.Shannon.G_sim.micr1$frame),
                                             cbind(GENUS_SPECIES="P. concinnum",model.Shannon.P_con.micr2$frame))
  model.Shannon.species.micr.observed$PLOT<-sapply(strsplit(as.character(model.Shannon.species.micr.observed$LOCATION_PLOT), "_"),'[', 2)

  model.Shannon.species.micr.predict<-rbind(cbind(GENUS_SPECIES="A. jagori",model.Shannon.A_jag.micr.predict),
                                          cbind(GENUS_SPECIES="G. similis s.l.",model.Shannon.G_sim.micr.predict),
                                          cbind(GENUS_SPECIES="P. concinnum",model.Shannon.P_con.micr.predict))
#Create plot.
  (FigureS4D<-ggplot()+
      geom_point(data=model.Shannon.species.micr.observed, aes(x=Shannon.comm.shells, y=log10(Shannon.micr)), size=0.5)+
      geom_line(data=model.Shannon.species.micr.predict, aes(x=Shannon.comm.shells,y=log10(predict),linetype=PLOT), lwd=0.5)+
      facet_grid(GENUS_SPECIES~LOCATION)+
      ggtitle("Microbiome vs. consumer community diversity (Shannon)")+
      theme_bw()+labs(x="Shell consumer community diversity", y= "Log10(microbiome diversity)")+
      theme(legend.key.width = unit(2,"cm")))

#Figure S4E: model fit of microbiome~host community (shells) phylogenetic diversity (Faith's PD) by target species.
  
#For A. jagori.
  
  #Use model results from best model, model.PD.micr1, to predict.
  model.PD.A_jag.micr.predict<-expand.grid(PD.comm.shells= seq(from=min(data$PD.comm.shells[data$GENUS_SPECIES=="A_jagori"]), to=max(data$PD.comm.shells[data$GENUS_SPECIES=="A_jagori"]), by=0.01),
                                                LOCATION_PLOT=unique(model.PD.A_jag.micr1$frame$LOCATION_PLOT))
  model.PD.A_jag.micr.predict$LOCATION<-sapply(strsplit(as.character(model.PD.A_jag.micr.predict$LOCATION_PLOT), "_"),'[', 1)
  model.PD.A_jag.micr.predict$predict<-predict(model.PD.A_jag.micr1, type="response",newdata=model.PD.A_jag.micr.predict)
  model.PD.A_jag.micr.predict$PLOT<-sapply(strsplit(as.character(model.PD.A_jag.micr.predict$LOCATION_PLOT), "_"),'[', 2)
  
#For G. similis s.l.
  
  #Use model results from best model, model.PD.micr1, to predict.
  model.PD.G_sim.micr.predict<-expand.grid(PD.comm.shells= seq(from=min(data$PD.comm.shells[data$GENUS_SPECIES=="G_similis"]), to=max(data$PD.comm.shells[data$GENUS_SPECIES=="G_similis"]), by=0.01),
                                                LOCATION_PLOT=unique(model.PD.G_sim.micr1$frame$LOCATION_PLOT))
  model.PD.G_sim.micr.predict$LOCATION<-sapply(strsplit(as.character(model.PD.G_sim.micr.predict$LOCATION_PLOT), "_"),'[', 1)
  model.PD.G_sim.micr.predict$predict<-predict(model.PD.G_sim.micr1, type="response",newdata=model.PD.G_sim.micr.predict)
  model.PD.G_sim.micr.predict$PLOT<-sapply(strsplit(as.character(model.PD.G_sim.micr.predict$LOCATION_PLOT), "_"),'[', 2)
  
#For P. concinnum.
  
  #Use model results from best model, model.PD.micr1, to predict.
  model.PD.P_con.micr.predict<-expand.grid(PD.comm.shells= seq(from=min(data$PD.comm.shells[data$GENUS_SPECIES=="P_concinnum"]), to=max(data$PD.comm.shells[data$GENUS_SPECIES=="P_concinnum"]), by=0.01),
                                                LOCATION_PLOT=unique(model.PD.P_con.micr1$frame$LOCATION_PLOT))
  model.PD.P_con.micr.predict$LOCATION<-sapply(strsplit(as.character(model.PD.P_con.micr.predict$LOCATION_PLOT), "_"),'[', 1)
  model.PD.P_con.micr.predict$predict<-predict(model.PD.P_con.micr1, type="response",newdata=model.PD.P_con.micr.predict)
  model.PD.P_con.micr.predict$PLOT<-sapply(strsplit(as.character(model.PD.P_con.micr.predict$LOCATION_PLOT), "_"),'[', 2)
  
#Collect data for the three target species.
  model.PD.species.micr.observed<-rbind(cbind(GENUS_SPECIES="A. jagori",model.PD.A_jag.micr1$frame),
                                             cbind(GENUS_SPECIES="G. similis s.l.",model.PD.G_sim.micr1$frame),
                                             cbind(GENUS_SPECIES="P. concinnum",model.PD.P_con.micr1$frame))
  model.PD.species.micr.observed$PLOT<-sapply(strsplit(as.character(model.PD.species.micr.observed$LOCATION_PLOT), "_"),'[', 2)
  
  model.PD.species.micr.predict<-rbind(cbind(GENUS_SPECIES="A. jagori",model.PD.A_jag.micr.predict),
                                            cbind(GENUS_SPECIES="G. similis s.l.",model.PD.G_sim.micr.predict),
                                            cbind(GENUS_SPECIES="P. concinnum",model.PD.P_con.micr.predict))

#Create plot.
  (FigureS4E<-ggplot()+
      geom_point(data=model.PD.species.micr.observed, aes(x=PD.comm.shells, y=PD.micr), size=0.5)+
      geom_line(data=model.PD.species.micr.predict, aes(x=PD.comm.shells,y=predict,linetype=PLOT), lwd=0.5)+
      facet_grid(GENUS_SPECIES~LOCATION)+
      ggtitle("Microbiome vs. consumer community phylogenetic diversity (Faith's PD)")+
      theme_bw()+
      theme_bw()+labs(x="Shell consumer community phylogenetic diversity", y= "Microbiome phylogenetic diversity")+
      theme(legend.key.width = unit(2,"cm")))

#Figure S4F: model fit of microbiome~host community (shells) richness (Chao1) by target species.
  
#For A. jagori.
  
  #Use model results from best model, model.Chao1.micr1, to predict.
  model.Chao1.A_jag.micr.predict<-expand.grid(Chao1.comm.shells= seq(from=min(data$Chao1.comm.shells[data$GENUS_SPECIES=="A_jagori"]), to=max(data$Chao1.comm.shells[data$GENUS_SPECIES=="A_jagori"]), by=0.01),
                                           LOCATION_PLOT=unique(model.Chao1.A_jag.micr3$frame$LOCATION_PLOT))
  model.Chao1.A_jag.micr.predict$LOCATION<-sapply(strsplit(as.character(model.Chao1.A_jag.micr.predict$LOCATION_PLOT), "_"),'[', 1)
  model.Chao1.A_jag.micr.predict$predict<-predict(model.Chao1.A_jag.micr3, type="response",newdata=model.Chao1.A_jag.micr.predict)
  model.Chao1.A_jag.micr.predict$PLOT<-sapply(strsplit(as.character(model.Chao1.A_jag.micr.predict$LOCATION_PLOT), "_"),'[', 2)
  
#For G. similis s.l.
  
  #Use model results from best model, model.Chao1.micr1, to predict.
  model.Chao1.G_sim.micr.predict<-expand.grid(Chao1.comm.shells= seq(from=min(data$Chao1.comm.shells[data$GENUS_SPECIES=="G_similis"]), to=max(data$Chao1.comm.shells[data$GENUS_SPECIES=="G_similis"]), by=0.01),
                                           LOCATION_PLOT=unique(model.Chao1.G_sim.micr4$frame$LOCATION_PLOT))
  model.Chao1.G_sim.micr.predict$LOCATION<-sapply(strsplit(as.character(model.Chao1.G_sim.micr.predict$LOCATION_PLOT), "_"),'[', 1)
  model.Chao1.G_sim.micr.predict$predict<-predict(model.Chao1.G_sim.micr4, type="response",newdata=model.Chao1.G_sim.micr.predict)
  model.Chao1.G_sim.micr.predict$PLOT<-sapply(strsplit(as.character(model.Chao1.G_sim.micr.predict$LOCATION_PLOT), "_"),'[', 2)
  
#For P. concinnum.
  
#Use model results from best model, model.Chao1.micr1, to predict.
  model.Chao1.P_con.micr.predict<-expand.grid(Chao1.comm.shells= seq(from=min(data$Chao1.comm.shells[data$GENUS_SPECIES=="P_concinnum"]), to=max(data$Chao1.comm.shells[data$GENUS_SPECIES=="P_concinnum"]), by=0.01),
                                           LOCATION_PLOT=unique(model.Chao1.P_con.micr1$frame$LOCATION_PLOT))
  model.Chao1.P_con.micr.predict$LOCATION<-sapply(strsplit(as.character(model.Chao1.P_con.micr.predict$LOCATION_PLOT), "_"),'[', 1)
  model.Chao1.P_con.micr.predict$predict<-predict(model.Chao1.P_con.micr1, type="response",newdata=model.Chao1.P_con.micr.predict)
  model.Chao1.P_con.micr.predict$PLOT<-sapply(strsplit(as.character(model.Chao1.P_con.micr.predict$LOCATION_PLOT), "_"),'[', 2)
  
#Collect data for the three target species.
  model.Chao1.species.micr.observed<-rbind(cbind(GENUS_SPECIES="A. jagori", data[data$GENUS_SPECIES=="A_jagori" & is.na(data$Chao1.micr)==F & data$LOCATION %in% names(table(data[data$GENUS_SPECIES=="A_jagori",]$LOCATION)[(table(data[data$GENUS_SPECIES=="A_jagori",]$LOCATION)>9)]),]),
                                        cbind(GENUS_SPECIES="G. similis s.l.", data[data$GENUS_SPECIES=="G_similis" & is.na(data$Chao1.micr)==F & data$LOCATION %in% names(table(data[data$GENUS_SPECIES=="G_similis",]$LOCATION)[(table(data[data$GENUS_SPECIES=="G_similis",]$LOCATION)>16)]),]),
                                        cbind(GENUS_SPECIES="P. concinnum", data[data$GENUS_SPECIES=="P_concinnum" & is.na(data$Chao1.micr)==F & data$LOCATION %in% names(table(data[data$GENUS_SPECIES=="P_concinnum",]$LOCATION)[(table(data[data$GENUS_SPECIES=="P_concinnum",]$LOCATION)>9)]),]))
  model.Chao1.species.micr.observed<-model.Chao1.species.micr.observed[,-4]
  
  model.Chao1.species.micr.predict<-rbind(cbind(GENUS_SPECIES="A. jagori",model.Chao1.A_jag.micr.predict),
                                       cbind(GENUS_SPECIES="G. similis s.l.",model.Chao1.G_sim.micr.predict),
                                       cbind(GENUS_SPECIES="P. concinnum",model.Chao1.P_con.micr.predict))
#Create plot.
  (FigureS4F<-ggplot()+
      geom_point(data=model.Chao1.species.micr.observed, aes(x=Chao1.comm.shells, y=Chao1.micr), size=0.5)+
      geom_line(data=model.Chao1.species.micr.predict, aes(x=Chao1.comm.shells,y=predict,linetype=PLOT), lwd=0.5)+
      facet_grid(GENUS_SPECIES~LOCATION)+
      ggtitle("Microbiome vs. consumer community richness (Chao1)")+
      theme_bw()+
      theme_bw()+labs(x="Shell consumer community richness", y= "Microbiome richness")+
      theme(legend.key.width = unit(2,"cm")))

#Save plots togther.
  ggsave(plot_grid(FigureS4A, FigureS4B, FigureS4C , align='v', ncol=1, labels = c("(A)","(B)","(C)")), file = "export_for_publication/FigureS4ABC.pdf", width = 9, height = 12)
  ggsave(plot_grid(FigureS4D, FigureS4E, FigureS4F , align='v', ncol=1, labels = c("(D)","(E)","(F)")), file = "export_for_publication/FigureS4DEF.pdf", width = 9, height = 12)
  
  
  

#CREATE Table S8 FOR PUBLICATION, AN OVERVIEW OF MODEL SELECTION FOR SPECIES MODELS CORRELATING MICROBIOME/DIET WITH host community (shells) AND LOCATION.
#Get the anova results.
TableS8<-rbind(as.data.frame(anova(model.Chao1.P_con.diet1,model.Chao1.P_con.diet2,model.Chao1.P_con.diet3,model.Chao1.P_con.diet4,model.Chao1.P_con.diet5))[,c(1:2,4,6:8)],
               as.data.frame(anova(model.Chao1.G_sim.diet1,model.Chao1.G_sim.diet2,model.Chao1.G_sim.diet3,model.Chao1.G_sim.diet4,model.Chao1.G_sim.diet5))[,c(1:2,4,6:8)],
               as.data.frame(anova(model.Chao1.A_jag.diet1,model.Chao1.A_jag.diet2,model.Chao1.A_jag.diet3,model.Chao1.A_jag.diet4,model.Chao1.A_jag.diet5))[,c(1:2,4,6:8)],
               as.data.frame(anova(model.PD.P_con.diet1,model.PD.P_con.diet2,model.PD.P_con.diet3,model.PD.P_con.diet4,model.PD.P_con.diet5))[,c(1:2,4,6:8)],
               as.data.frame(anova(model.PD.G_sim.diet1,model.PD.G_sim.diet2,model.PD.G_sim.diet3,model.PD.G_sim.diet4,model.PD.G_sim.diet5))[,c(1:2,4,6:8)],
               as.data.frame(anova(model.PD.A_jag.diet1,model.PD.A_jag.diet2,model.PD.A_jag.diet3,model.PD.A_jag.diet4,model.PD.A_jag.diet5))[,c(1:2,4,6:8)],
               as.data.frame(anova(model.Shannon.P_con.diet1,model.Shannon.P_con.diet2,model.Shannon.P_con.diet3,model.Shannon.P_con.diet4,model.Shannon.P_con.diet5))[,c(1:2,4,6:8)],
               as.data.frame(anova(model.Shannon.G_sim.diet1,model.Shannon.G_sim.diet2,model.Shannon.G_sim.diet3,model.Shannon.G_sim.diet4,model.Shannon.G_sim.diet5))[,c(1:2,4,6:8)],
               as.data.frame(anova(model.Shannon.A_jag.diet1,model.Shannon.A_jag.diet2,model.Shannon.A_jag.diet3,model.Shannon.A_jag.diet4,model.Shannon.A_jag.diet5))[,c(1:2,4,6:8)],
               as.data.frame(anova(model.Chao1.P_con.micr1,model.Chao1.P_con.micr2,model.Chao1.P_con.micr3,model.Chao1.P_con.micr4,model.Chao1.P_con.micr5))[,c(1:2,4,6:8)],
               as.data.frame(anova(model.Chao1.G_sim.micr1,model.Chao1.G_sim.micr2,model.Chao1.G_sim.micr3,model.Chao1.G_sim.micr4,model.Chao1.G_sim.micr5))[,c(1:2,4,6:8)],
               as.data.frame(anova(model.Chao1.A_jag.micr1,model.Chao1.A_jag.micr2,model.Chao1.A_jag.micr3,model.Chao1.A_jag.micr4,model.Chao1.A_jag.micr5))[,c(1:2,4,6:8)],
               as.data.frame(anova(model.PD.P_con.micr1,model.PD.P_con.micr2,model.PD.P_con.micr3,model.PD.P_con.micr4,model.PD.P_con.micr5))[,c(1:2,4,6:8)],
               as.data.frame(anova(model.PD.G_sim.micr1,model.PD.G_sim.micr2,model.PD.G_sim.micr3,model.PD.G_sim.micr4,model.PD.G_sim.micr5))[,c(1:2,4,6:8)],
               as.data.frame(anova(model.PD.A_jag.micr1,model.PD.A_jag.micr2,model.PD.A_jag.micr3,model.PD.A_jag.micr4,model.PD.A_jag.micr5))[,c(1:2,4,6:8)],
               as.data.frame(anova(model.Shannon.P_con.micr1,model.Shannon.P_con.micr2,model.Shannon.P_con.micr3,model.Shannon.P_con.micr4,model.Shannon.P_con.micr5))[,c(1:2,4,6:8)],
               as.data.frame(anova(model.Shannon.G_sim.micr1,model.Shannon.G_sim.micr2,model.Shannon.G_sim.micr3,model.Shannon.G_sim.micr4,model.Shannon.G_sim.micr5))[,c(1:2,4,6:8)],
               as.data.frame(anova(model.Shannon.A_jag.micr1,model.Shannon.A_jag.micr2,model.Shannon.A_jag.micr3,model.Shannon.A_jag.micr4,model.Shannon.A_jag.micr5))[,c(1:2,4,6:8)])
#Reorder the table.
TableS8<-TableS8[seq(dim(TableS8)[1],1),]
#Add the model formulae.
TableS8<-cbind(as.data.frame(rbind(matrix(strsplit(as.character(model.Shannon.A_jag.micr1$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.Shannon.A_jag.micr2$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.Shannon.A_jag.micr3$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.Shannon.A_jag.micr4$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.Shannon.A_jag.micr5$call)[2],"~")[[1]],nrow=1),
                                   matrix(strsplit(as.character(model.Shannon.G_sim.micr1$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.Shannon.G_sim.micr2$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.Shannon.G_sim.micr3$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.Shannon.G_sim.micr4$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.Shannon.G_sim.micr5$call)[2],"~")[[1]],nrow=1),
                                   matrix(strsplit(as.character(model.Shannon.P_con.micr1$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.Shannon.P_con.micr2$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.Shannon.P_con.micr3$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.Shannon.P_con.micr4$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.Shannon.P_con.micr5$call)[2],"~")[[1]],nrow=1),
                                   matrix(strsplit(as.character(model.PD.A_jag.micr1$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.PD.A_jag.micr2$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.PD.A_jag.micr3$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.PD.A_jag.micr4$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.PD.A_jag.micr5$call)[2],"~")[[1]],nrow=1),
                                   matrix(strsplit(as.character(model.PD.G_sim.micr1$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.PD.G_sim.micr2$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.PD.G_sim.micr3$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.PD.G_sim.micr4$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.PD.G_sim.micr5$call)[2],"~")[[1]],nrow=1),
                                   matrix(strsplit(as.character(model.PD.P_con.micr1$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.PD.P_con.micr2$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.PD.P_con.micr3$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.PD.P_con.micr4$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.PD.P_con.micr5$call)[2],"~")[[1]],nrow=1),
                                   matrix(strsplit(as.character(model.Chao1.A_jag.micr1$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.Chao1.A_jag.micr2$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.Chao1.A_jag.micr3$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.Chao1.A_jag.micr4$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.Chao1.A_jag.micr5$call)[2],"~")[[1]],nrow=1),
                                   matrix(strsplit(as.character(model.Chao1.G_sim.micr1$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.Chao1.G_sim.micr2$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.Chao1.G_sim.micr3$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.Chao1.G_sim.micr4$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.Chao1.G_sim.micr5$call)[2],"~")[[1]],nrow=1),
                                   matrix(strsplit(as.character(model.Chao1.P_con.micr1$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.Chao1.P_con.micr2$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.Chao1.P_con.micr3$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.Chao1.P_con.micr4$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.Chao1.P_con.micr5$call)[2],"~")[[1]],nrow=1),
                                   matrix(strsplit(as.character(model.Shannon.A_jag.diet1$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.Shannon.A_jag.diet2$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.Shannon.A_jag.diet3$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.Shannon.A_jag.diet4$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.Shannon.A_jag.diet5$call)[2],"~")[[1]],nrow=1),
                                   matrix(strsplit(as.character(model.Shannon.G_sim.diet1$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.Shannon.G_sim.diet2$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.Shannon.G_sim.diet3$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.Shannon.G_sim.diet4$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.Shannon.G_sim.diet5$call)[2],"~")[[1]],nrow=1),
                                   matrix(strsplit(as.character(model.Shannon.P_con.diet1$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.Shannon.P_con.diet2$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.Shannon.P_con.diet3$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.Shannon.P_con.diet4$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.Shannon.P_con.diet5$call)[2],"~")[[1]],nrow=1),
                                   matrix(strsplit(as.character(model.PD.A_jag.diet1$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.PD.A_jag.diet2$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.PD.A_jag.diet3$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.PD.A_jag.diet4$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.PD.A_jag.diet5$call)[2],"~")[[1]],nrow=1),
                                   matrix(strsplit(as.character(model.PD.G_sim.diet1$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.PD.G_sim.diet2$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.PD.G_sim.diet3$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.PD.G_sim.diet4$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.PD.G_sim.diet5$call)[2],"~")[[1]],nrow=1),
                                   matrix(strsplit(as.character(model.PD.P_con.diet1$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.PD.P_con.diet2$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.PD.P_con.diet3$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.PD.P_con.diet4$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.PD.P_con.diet5$call)[2],"~")[[1]],nrow=1),
                                   matrix(strsplit(as.character(model.Chao1.A_jag.diet1$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.Chao1.A_jag.diet2$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.Chao1.A_jag.diet3$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.Chao1.A_jag.diet4$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.Chao1.A_jag.diet5$call)[2],"~")[[1]],nrow=1),
                                   matrix(strsplit(as.character(model.Chao1.G_sim.diet1$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.Chao1.G_sim.diet2$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.Chao1.G_sim.diet3$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.Chao1.G_sim.diet4$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.Chao1.G_sim.diet5$call)[2],"~")[[1]],nrow=1),
                                   matrix(strsplit(as.character(model.Chao1.P_con.diet1$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.Chao1.P_con.diet2$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.Chao1.P_con.diet3$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.Chao1.P_con.diet4$call)[2],"~")[[1]],nrow=1),matrix(strsplit(as.character(model.Chao1.P_con.diet5$call)[2],"~")[[1]],nrow=1))),
               TableS8)
#Add response and metric variable definitions.
TableS8<-cbind(rep(c("A. jagori",rep("",4),"G. similis s.l.",rep("",4),"P. concinnum",rep("",4)),6),TableS8)
TableS8<-cbind(rep(c("Diversity (Shannon)",rep("",14),"Phylogenetic diversity (Faith's PD)",rep("",14),"Richness (Chao1)",rep("",14)),2),TableS8)
TableS8<-cbind(c("Microbiome",rep("",44),"Diet",rep("",44)),TableS8)
#After having checked the above names are placed correctly in the table, we can delete column 3.
TableS8<-TableS8[,-4]
#Update some text for publication-ready style.
TableS8$V2<-gsub("Shannon.comm.shells","Host community (shells) diversity (Shannon)",TableS8$V2)
TableS8$V2<-gsub("PD.comm.shells","Host community (shells) phylogenetic diversity (PD)",TableS8$V2)
TableS8$V2<-gsub("Chao1.comm.shells","Host community (shells) richness (Chao1)",TableS8$V2)
TableS8$V2<-gsub(" + (1 | LOCATION_PLOT)","",TableS8$V2,fixed = TRUE)
TableS8$V2<-gsub("(1 | LOCATION_PLOT)","Empty",TableS8$V2,fixed = TRUE)
TableS8$V2<-gsub("LOCATION","Location",TableS8$V2)
#Round numbers.
TableS8[,6]<-round(as.numeric(TableS8[,6]),2);TableS8[,7]<-round(as.numeric(TableS8[,7]),2);TableS8[,8]<-round(as.numeric(TableS8[,8]),2)
TableS8[,10]<-round(as.numeric(TableS8[,10]),3)
TableS8[is.na(TableS8$`Pr(>Chisq)`)==FALSE & TableS8$`Pr(>Chisq)`<0.001,10]<-"<0.001"
#Remove rownames and set proper column names.
rownames(TableS8)<-NULL
colnames(TableS8)<-c("Response","Metric","Species","Fixed effect(s)","df","AIC","Log-lik","Chi-square","df","P")
#And save the table.
write.csv(TableS8, file = "export_for_publication/TableS8.csv", na="")


#CREATE Table S9 FOR PUBLICATION, A SUMMARY OF FINAL MODELS FOR EACH SPECIES, CORRELATING MICROBIOME/DIET WITH host community (shells).
TableS9<-as.data.frame(rbind(summary(model.Shannon.A_jag.micr1)$coefficients$cond,
                             summary(model.Shannon.G_sim.micr1)$coefficients$cond,
                             summary(model.Shannon.P_con.micr2)$coefficients$cond,
                             summary(model.PD.A_jag.micr1)$coefficients$cond,
                             summary(model.PD.G_sim.micr1)$coefficients$cond,
                             summary(model.PD.P_con.micr1)$coefficients$cond,
                             summary(model.Chao1.A_jag.micr3)$coefficients$cond,
                             summary(model.Chao1.G_sim.micr4)$coefficients$cond,
                             summary(model.Chao1.P_con.micr1)$coefficients$cond,
                             summary(model.Shannon.A_jag.diet4)$coefficients$cond, #<<was 3
                             summary(model.Shannon.G_sim.diet4)$coefficients$cond,
                             summary(model.Shannon.P_con.diet3)$coefficients$cond,
                             summary(model.PD.A_jag.diet4)$coefficients$cond,
                             summary(model.PD.G_sim.diet4)$coefficients$cond, #<was 1
                             summary(model.PD.P_con.diet1)$coefficients$cond, #was 3
                             summary(model.Chao1.A_jag.diet1)$coefficients$cond,
                             summary(model.Chao1.G_sim.diet1)$coefficients$cond,
                             summary(model.Chao1.P_con.diet3)$coefficients$cond)) #was 2
TableS9$Coefficient<-rownames(TableS9)
#Add response and metrics details for the different models; "1" and "2" serve as annotations to be specified below the table.
TableS9$Response<-c("Microbiome", rep("",68),"Diet",rep("",47))
TableS9$Metric<-c("Diversity (Shannon)",rep("",22),"Phylogenetic diversity (Faith's PD)",rep("",27),"Richness (Chao1)",rep("",17),"Diversity (Shannon)",rep("",9),"Phylogenetic diversity (Faith's PD)",rep("",15),"Richness (Chao1)",rep("",21))
TableS9$Species<-c("A. jagori",rep("",7),"G. similis s.l.",rep("",7),"P. concinnum",rep("",6),
                   "A. jagori",rep("",7),"G. similis s.l.",rep("",7),"P. concinnum",rep("",11),
                   "A. jagori",rep("",3),"G. similis s.l.1",rep("",1),"P. concinnum",rep("",11),
                   "A. jagori1",rep("",1),"G. similis s.l.1",rep("",1),"P. concinnum",rep("",5),
                   "A. jagori",rep("",1),"G. similis s.l.",rep("",1),"P. concinnum1",rep("",11),
                   "A. jagori",rep("",7),"G. similis s.l.1",rep("",7),"P. concinnum",rep("",5))
TableS9<-TableS9[,c(6:8,5,1:4)]
rownames(TableS9)<-NULL
#Update the names of the coefficients to publication-ready strings.
TableS9$Coefficient[grep("Shannon.comm.shells.LOCATIONKeruak",TableS9$Coefficient)]<-"Host community (shells) diversity (Shannon) x Location (Keruak)"
TableS9$Coefficient[grep("Shannon.comm.shells.LOCATIONPangi",TableS9$Coefficient)]<-"Host community (shells) diversity (Shannon) x Location (Pangi)"
TableS9$Coefficient[grep("Shannon.comm.shells.LOCATIONTanduBatu",TableS9$Coefficient)]<-"Host community (shells) diversity (Shannon) x Location (Tandu Batu)"
TableS9$Coefficient[grep("Shannon.comm.shells.LOCATIONTomanggong2",TableS9$Coefficient)]<-"Host community (shells) diversity (Shannon) x Location (Tomanggong 2)"
TableS9$Coefficient[grep("Shannon.comm.shells.LOCATIONTomanggongKecil",TableS9$Coefficient)]<-"Host community (shells) diversity (Shannon) x Location (Tomanggong Kecil)"
TableS9$Coefficient[grep("PD.comm.shells.LOCATIONKeruak",TableS9$Coefficient)]<-"Host community (shells) phylogenetic diversity (PD) x Location (Keruak)"
TableS9$Coefficient[grep("PD.comm.shells.LOCATIONPangi",TableS9$Coefficient)]<-"Host community (shells) phylogenetic diversity (PD) x Location (Pangi)"
TableS9$Coefficient[grep("PD.comm.shells.LOCATIONTanduBatu",TableS9$Coefficient)]<-"Host community (shells) phylogenetic diversity (PD) x Location (Tandu Batu)"
TableS9$Coefficient[grep("PD.comm.shells.LOCATIONTomanggong2",TableS9$Coefficient)]<-"Host community (shells) phylogenetic diversity (PD) x Location (Tomanggong 2)"
TableS9$Coefficient[grep("PD.comm.shells.LOCATIONTomanggongKecil",TableS9$Coefficient)]<-"Host community (shells) phylogenetic diversity (PD) x Location (Tomanggong Kecil)"
TableS9$Coefficient[grep("Chao1.comm.shells.LOCATIONKeruak",TableS9$Coefficient)]<-"Host community (shells) richness (Chao1) x Location (Keruak)"
TableS9$Coefficient[grep("Chao1.comm.shells.LOCATIONPangi",TableS9$Coefficient)]<-"Host community (shells) richness (Chao1) x Location (Pangi)"
TableS9$Coefficient[grep("Chao1.comm.shells.LOCATIONTanduBatu",TableS9$Coefficient)]<-"Host community (shells) richness (Chao1) x Location (Tandu Batu)"
TableS9$Coefficient[grep("Chao1.comm.shells.LOCATIONTomanggong2",TableS9$Coefficient)]<-"Host community (shells) richness (Chao1) x Location (Tomanggong 2)"
TableS9$Coefficient[grep("Chao1.comm.shells.LOCATIONTomanggongKecil",TableS9$Coefficient)]<-"Host community (shells) richness (Chao1) x Location (Tomanggong Kecil)"
TableS9$Coefficient[grep("LOCATIONTanduBatu",TableS9$Coefficient)]<-"Location (Tandu Batu)"
TableS9$Coefficient[grep("LOCATIONTomanggong2",TableS9$Coefficient)]<-"Location (Tomanggong 2)"
TableS9$Coefficient[grep("LOCATIONTomanggongKecil",TableS9$Coefficient)]<-"Location (Tomanggong Kecil)"
TableS9$Coefficient[grep("LOCATIONPangi",TableS9$Coefficient)]<-"Location (Pangi)"
TableS9$Coefficient[grep("LOCATIONKeruak",TableS9$Coefficient)]<-"Location (Keruak)"
TableS9$Coefficient[grep("Intercept", TableS9$Coefficient)]<-"Intercept"
TableS9$Coefficient[grep("Shannon.comm.shells", TableS9$Coefficient)]<-"Host community (shells) diversity (Shannon)"
TableS9$Coefficient[grep("PD.comm.shells", TableS9$Coefficient)]<-"Host community (shells) phylogenetic diversity (PD)"
TableS9$Coefficient[grep("Chao1.comm.shells", TableS9$Coefficient)]<-"Host community (shells) richness (Chao1)"
#Round numbers.
TableS9[,5:7]<-round(TableS9[,5:7],2)
TableS9$`Pr(>|z|)`[TableS9$`Pr(>|z|)`<0.001]<- "<0.001"
TableS9$`Pr(>|z|)`[TableS9$`Pr(>|z|)`>0.001]<-round(as.numeric(TableS9$`Pr(>|z|)`[TableS9$`Pr(>|z|)`>0.001]),3)
#Add definitions of annotations.  
TableS9<-rbind(TableS9, c("1 For these models, the empty model actually scored slightly better; results for the second best model (deltaAIC < 2) are shown here, as the empty model does not convey any information.",rep("",7)))
#And save the table.
write.csv(TableS9, file = "export_for_publication/TableS9.csv", na="")


#CREATE Table S10, LOCATION TRENDS FROM SPECIES MODELS OF RESPONSES BY SPECIES AND LOCATION IN MICROBIOME/DIET TO HOST COMMUNITY.
#First create new objects with trend results from complete models.
model.Shannon.A_jag.diet1.trend.LOCATION<-as.data.frame(summary(model.Shannon.A_jag.diet1.contrast.LOCATION, infer=c(TRUE,TRUE),null=0)$emtrends)
model.Shannon.G_sim.diet1.trend.LOCATION<-as.data.frame(summary(model.Shannon.G_sim.diet1.contrast.LOCATION, infer=c(TRUE,TRUE),null=0)$emtrends)
model.Shannon.P_con.diet1.trend.LOCATION<-as.data.frame(summary(model.Shannon.P_con.diet1.contrast.LOCATION, infer=c(TRUE,TRUE),null=0)$emtrends)
model.PD.A_jag.diet1.trend.LOCATION<-as.data.frame(summary(model.PD.A_jag.diet1.contrast.LOCATION, infer=c(TRUE,TRUE),null=0)$emtrends)
model.PD.G_sim.diet1.trend.LOCATION<-as.data.frame(summary(model.PD.G_sim.diet1.contrast.LOCATION, infer=c(TRUE,TRUE),null=0)$emtrends)
model.PD.P_con.diet1.trend.LOCATION<-as.data.frame(summary(model.PD.P_con.diet1.contrast.LOCATION, infer=c(TRUE,TRUE),null=0)$emtrends)
model.Chao1.A_jag.diet1.trend.LOCATION<-as.data.frame(summary(model.Chao1.A_jag.diet1.contrast.LOCATION, infer=c(TRUE,TRUE),null=0)$emtrends)
model.Chao1.G_sim.diet1.trend.LOCATION<-as.data.frame(summary(model.Chao1.G_sim.diet1.contrast.LOCATION, infer=c(TRUE,TRUE),null=0)$emtrends)
model.Chao1.P_con.diet1.trend.LOCATION<-as.data.frame(summary(model.Chao1.P_con.diet1.contrast.LOCATION, infer=c(TRUE,TRUE),null=0)$emtrends)
model.Shannon.A_jag.micr1.trend.LOCATION<-as.data.frame(summary(model.Shannon.A_jag.micr1.contrast.LOCATION, infer=c(TRUE,TRUE),null=0)$emtrends)
model.Shannon.G_sim.micr1.trend.LOCATION<-as.data.frame(summary(model.Shannon.G_sim.micr1.contrast.LOCATION, infer=c(TRUE,TRUE),null=0)$emtrends)
model.Shannon.P_con.micr1.trend.LOCATION<-as.data.frame(summary(model.Shannon.P_con.micr1.contrast.LOCATION, infer=c(TRUE,TRUE),null=0)$emtrends)
model.PD.A_jag.micr1.trend.LOCATION<-as.data.frame(summary(model.PD.A_jag.micr1.contrast.LOCATION, infer=c(TRUE,TRUE),null=0)$emtrends)
model.PD.G_sim.micr1.trend.LOCATION<-as.data.frame(summary(model.PD.G_sim.micr1.contrast.LOCATION, infer=c(TRUE,TRUE),null=0)$emtrends)
model.PD.P_con.micr1.trend.LOCATION<-as.data.frame(summary(model.PD.P_con.micr1.contrast.LOCATION, infer=c(TRUE,TRUE),null=0)$emtrends)
model.Chao1.A_jag.micr1.trend.LOCATION<-as.data.frame(summary(model.Chao1.A_jag.micr1.contrast.LOCATION, infer=c(TRUE,TRUE),null=0)$emtrends)
model.Chao1.G_sim.micr1.trend.LOCATION<-as.data.frame(summary(model.Chao1.G_sim.micr1.contrast.LOCATION, infer=c(TRUE,TRUE),null=0)$emtrends)
model.Chao1.P_con.micr1.trend.LOCATION<-as.data.frame(summary(model.Chao1.P_con.micr1.contrast.LOCATION, infer=c(TRUE,TRUE),null=0)$emtrends)
names(model.Shannon.A_jag.diet1.trend.LOCATION)[2]=names(model.Shannon.G_sim.diet1.trend.LOCATION)[2]=names(model.Shannon.P_con.diet1.trend.LOCATION)[2]=
  names(model.PD.A_jag.diet1.trend.LOCATION)[2]=names(model.PD.G_sim.diet1.trend.LOCATION)[2]=names(model.PD.P_con.diet1.trend.LOCATION)[2]=
  names(model.Chao1.A_jag.diet1.trend.LOCATION)[2]=names(model.Chao1.G_sim.diet1.trend.LOCATION)[2]=names(model.Chao1.P_con.diet1.trend.LOCATION)[2]=
  names(model.Shannon.A_jag.micr1.trend.LOCATION)[2]=names(model.Shannon.G_sim.micr1.trend.LOCATION)[2]=names(model.Shannon.P_con.micr1.trend.LOCATION)[2]=
  names(model.PD.A_jag.micr1.trend.LOCATION)[2]=names(model.PD.G_sim.micr1.trend.LOCATION)[2]=names(model.PD.P_con.micr1.trend.LOCATION)[2]=
  names(model.Chao1.A_jag.micr1.trend.LOCATION)[2]=names(model.Chao1.G_sim.micr1.trend.LOCATION)[2]=names(model.Chao1.P_con.micr1.trend.LOCATION)[2]<-"trend"
TableS10<-rbind(model.Shannon.A_jag.diet1.trend.LOCATION,model.Shannon.G_sim.diet1.trend.LOCATION,model.Shannon.P_con.diet1.trend.LOCATION,
                 model.PD.A_jag.diet1.trend.LOCATION,model.PD.G_sim.diet1.trend.LOCATION,model.PD.P_con.diet1.trend.LOCATION,
                 model.Chao1.A_jag.diet1.trend.LOCATION,model.Chao1.G_sim.diet1.trend.LOCATION,model.Chao1.P_con.diet1.trend.LOCATION,
                 model.Shannon.A_jag.micr1.trend.LOCATION,model.Shannon.G_sim.micr1.trend.LOCATION,model.Shannon.P_con.micr1.trend.LOCATION,
                 model.PD.A_jag.micr1.trend.LOCATION,model.PD.G_sim.micr1.trend.LOCATION,model.PD.P_con.micr1.trend.LOCATION,
                 model.Chao1.A_jag.micr1.trend.LOCATION,model.Chao1.G_sim.micr1.trend.LOCATION,model.Chao1.P_con.micr1.trend.LOCATION)
TableS10$Response<-c("Diet",rep("",39),"Microbiome",rep("",41))
TableS10$Metric<-c("Diversity (Shannon)",rep("",12),"Phylogenetic diversity (Faith's PD)",rep("",12),"Richness (Chao1)",rep("",13),
                  "Diversity (Shannon)",rep("",13),"Phylogenetic diversity (Faith's PD)",rep("",13),"Richness (Chao1)",rep("",13))
TableS10$Species<-c(rep(c("A. jagori",rep("",2),"G. similis s.l.",rep("",3),"P. concinnum",rep("",5)),2),
                   rep(c("A. jagori",rep("",3),"G. similis s.l.",rep("",3),"P. concinnum",rep("",5)),4))
TableS10<-TableS10[,c(9:11,1:4,7,8)]
#Update names to correct spelling in print.
TableS10$LOCATION<-gsub("TanduBatu","Tandu Batu",TableS10$LOCATION)
TableS10$LOCATION<-gsub("Tomanggong2","Tomanggong 2",TableS10$LOCATION)
TableS10$LOCATION<-gsub("TomanggongKecil","Tomanggong Kecil",TableS10$LOCATION)
#Round numbers.
TableS10[,c(5,6,8)]<-round(TableS10[,c(5,6,8)],3)
TableS10$p.value[TableS10$p.value<0.001]<- "<0.001"
TableS10$p.value[TableS10$p.value>0.001]<-round(as.numeric(TableS10$p.value[TableS10$p.value>0.001]),3)
colnames(TableS10)<-c("Response","Metric","Species","Location","Trend","SE","df","t-ratio","p-value")
#And save the table.
write.csv(TableS10, file = "export_for_publication/TableS10.csv", na="")



#CREATE Table S20 FOR PUBLICATION, AN OVERVIEW OF RESULTS FROM PLS-PM PATH MODELLING.

#First Table S20A, for the results from shell data as proxy for community of consumers.
#Combine results from core and full models, based on both complete and normalized data.
TableS20A<-rbind(merge.data.frame(rbind(c("gof",unlist(plspm_model_core_complete_shells_gof)),plspm_model_core_complete_shells_results),rbind(c("gof",unlist(plspm_model_core_norm_shells_gof)),plspm_model_core_norm_shells_results),by=1),
               merge.data.frame(rbind(c("gof",unlist(plspm_model_full_complete_shells_gof)),plspm_model_full_complete_shells_results),rbind(c("gof",unlist(plspm_model_full_norm_shells_gof)),plspm_model_full_norm_shells_results),by=1))
TableS20A[,-1]<-apply(TableS20A[,-1], 2, function(x) as.numeric(as.character(x)))
#Round values.
TableS20A[,-1]<-format(round(TableS20A[,-1],3),nsmall=3)
#Rewrite columns to present 95% confidence ratio.
TableS20A$'95% CI.x'<-paste0(format(TableS20A$perc.025.x,nsmall=3),"-",format(TableS20A$perc.975.x,nsmall=3))
TableS20A$'95% CI.y'<-paste0(format(TableS20A$perc.025.y,nsmall=3),"-",format(TableS20A$perc.975.y,nsmall=3))
#Keep only columns to be printed.
TableS20A<-TableS20A[,c("paths","Mean.Boot.x","95% CI.x","Mean.Boot.y","95% CI.y")]
#Add column names to define models.
colnames(TableS20A)<-c("","Complete model","","Normalized model","")
#And change the order of the rows.
TableS20A<-rbind(c("","mean","95% CI","mean","95% CI"),
               c("Core model", rep("",ncol(TableS20A)-1)),
               TableS20A[c(3,1,2,4),],
               c("Full model", rep("",ncol(TableS20A)-1)),
               TableS20A[c(13,11,12,20,5:10,14:19,21:26),])


#Second Table S20B, for the results from live snail data as proxy for community of consumers.
#Combine results from core and full models, based on both complete and normalized data.
TableS20B<-rbind(merge.data.frame(rbind(c("gof",unlist(plspm_model_core_complete_live_gof)),plspm_model_core_complete_live_results),rbind(c("gof",unlist(plspm_model_core_norm_live_gof)),plspm_model_core_norm_live_results),by=1),
                merge.data.frame(rbind(c("gof",unlist(plspm_model_full_complete_live_gof)),plspm_model_full_complete_live_results),rbind(c("gof",unlist(plspm_model_full_norm_live_gof)),plspm_model_full_norm_live_results),by=1))
TableS20B[,-1]<-apply(TableS20B[,-1], 2, function(x) as.numeric(as.character(x)))
#Round values.
TableS20B[,-1]<-format(round(TableS20B[,-1],3),nsmall=3)
#Rewrite columns to present 95% confidence ratio.
TableS20B$'95% CI.x'<-paste0(format(TableS20B$perc.025.x,nsmall=3),"-",format(TableS20B$perc.975.x,nsmall=3))
TableS20B$'95% CI.y'<-paste0(format(TableS20B$perc.025.y,nsmall=3),"-",format(TableS20B$perc.975.y,nsmall=3))
#Keep only columns to be printed.
TableS20B<-TableS20B[,c("paths","Mean.Boot.x","95% CI.x","Mean.Boot.y","95% CI.y")]
#Add column names to define models.
colnames(TableS20B)<-c("","Complete model","","Normalized model","")
TableS20B<-rbind(c("","mean","95% CI","mean","95% CI"),
                c("Core model", rep("",ncol(TableS20B)-1)),
                TableS20B[c(3,1,2,4),],
                c("Full model", rep("",ncol(TableS20B)-1)),
                TableS20B[c(13,11,12,20,5:10,14:19,21:26),])

#Bind tables A and B together.
TableS20<-rbind(TableS20A,TableS20B)
TableS20[1,1]<-"(A)";TableS20[30,1]<-"(B)"

#Update some terms in the first column.
TableS20[,1]<-gsub("gof","g.o.f.",TableS20[,1])
TableS20[,1]<-gsub("river.distance","river distance",TableS20[,1])
TableS20[,1]<-gsub("cave.distance","cave distance",TableS20[,1])
TableS20[,1]<-gsub("island.size","habitat size",TableS20[,1])
TableS20[,1]<-gsub("next.outcrop.distance","next outcrop distance",TableS20[,1])
TableS20[,1]<-gsub("anthropogenic.distance","anthropogenic distance",TableS20[,1])
#Export the table as csv file to be formatted for publication in Excel.
write.csv(TableS20, file = "export_for_publication/TableS20.csv", na="", row.names = FALSE)


#CREATE Table S21 FOR PUBLICATION, PERANOVA AND BETADISPER RESULTS WITH FIGURE S4

#We collect these results, and put these in a dataframe table for publication.
TableS21<-rbind(cbind(diet_byPlot_permanovaBray$aov.tab,
                    rbind(diet_byPlot_betadisperBray_species$tab[1,],
                          diet_byPlot_betadisperBray_location$tab[1,],
                          rep("",ncol(diet_byPlot_betadisperBray_location$tab[1,])),
                          rep("",ncol(diet_byPlot_betadisperBray_location$tab[1,])),
                          rep("",ncol(diet_byPlot_betadisperBray_location$tab[1,])))),
                cbind(diet_byPlot_permanovaJaccard$aov.tab,
                      rbind(diet_byPlot_betadisperJaccard_species$tab[1,],
                            diet_byPlot_betadisperJaccard_location$tab[1,],
                            rep("",ncol(diet_byPlot_betadisperJaccard_location$tab[1,])),
                            rep("",ncol(diet_byPlot_betadisperJaccard_location$tab[1,])),
                            rep("",ncol(diet_byPlot_betadisperJaccard_location$tab[1,])))),
                cbind(diet_byPlot_permanovaWeightedUniFrac$aov.tab,
                      rbind(diet_byPlot_betadisperWeightedUniFrac_species$tab[1,],
                            diet_byPlot_betadisperWeightedUniFrac_location$tab[1,],
                            rep("",ncol(diet_byPlot_betadisperWeightedUniFrac_location$tab[1,])),
                            rep("",ncol(diet_byPlot_betadisperWeightedUniFrac_location$tab[1,])),
                            rep("",ncol(diet_byPlot_betadisperWeightedUniFrac_location$tab[1,])))),
                cbind(diet_byPlot_permanovaUniFrac$aov.tab,
                      rbind(diet_byPlot_betadisperUniFrac_species$tab[1,],
                            diet_byPlot_betadisperUniFrac_location$tab[1,],
                            rep("",ncol(diet_byPlot_betadisperUniFrac_location$tab[1,])),
                            rep("",ncol(diet_byPlot_betadisperUniFrac_location$tab[1,])),
                            rep("",ncol(diet_byPlot_betadisperUniFrac_location$tab[1,])))),
                cbind(microbiome_byPlot_permanovaBray$aov.tab,
                      rbind(microbiome_byPlot_betadisperBray_species$tab[1,],
                            microbiome_byPlot_betadisperBray_location$tab[1,],
                            rep("",ncol(microbiome_byPlot_betadisperBray_location$tab[1,])),
                            rep("",ncol(microbiome_byPlot_betadisperBray_location$tab[1,])),
                            rep("",ncol(microbiome_byPlot_betadisperBray_location$tab[1,])))),
                cbind(microbiome_byPlot_permanovaJaccard$aov.tab,
                      rbind(microbiome_byPlot_betadisperJaccard_species$tab[1,],
                            microbiome_byPlot_betadisperJaccard_location$tab[1,],
                            rep("",ncol(microbiome_byPlot_betadisperJaccard_location$tab[1,])),
                            rep("",ncol(microbiome_byPlot_betadisperJaccard_location$tab[1,])),
                            rep("",ncol(microbiome_byPlot_betadisperJaccard_location$tab[1,])))),
                cbind(microbiome_byPlot_permanovaWeightedUniFrac$aov.tab,
                      rbind(microbiome_byPlot_betadisperWeightedUniFrac_species$tab[1,],
                            microbiome_byPlot_betadisperWeightedUniFrac_location$tab[1,],
                            rep("",ncol(microbiome_byPlot_betadisperWeightedUniFrac_location$tab[1,])),
                            rep("",ncol(microbiome_byPlot_betadisperWeightedUniFrac_location$tab[1,])),
                            rep("",ncol(microbiome_byPlot_betadisperWeightedUniFrac_location$tab[1,])))),
                cbind(microbiome_byPlot_permanovaUniFrac$aov.tab,
                      rbind(microbiome_byPlot_betadisperUniFrac_species$tab[1,],
                            microbiome_byPlot_betadisperUniFrac_location$tab[1,],
                            rep("",ncol(microbiome_byPlot_betadisperUniFrac_location$tab[1,])),
                            rep("",ncol(microbiome_byPlot_betadisperUniFrac_location$tab[1,])),
                            rep("",ncol(microbiome_byPlot_betadisperUniFrac_location$tab[1,])))))
TableS21[is.na(TableS21)==T]<-""
TableS21<-cbind(c("Diet",rep("",19),"Microbiome",rep("",19)),rep(c("Species", "Location", "Species*Location", "Residuals", "Totals"),8),TableS21[,c(1,2,4,5,6)],rep("",nrow(TableS21)),TableS21[,c(8,10,12)])
colnames(TableS21)<-c("Response variable", "Explanatory variable", "df", "SS", "pseudo-F", "R2", "Pr (>F)", "", "SS", "pseudo-F", "Pr (>F)")
#And save the table.
write.csv(TableS21, file = "export_for_publication/TableS21.csv", na="")


