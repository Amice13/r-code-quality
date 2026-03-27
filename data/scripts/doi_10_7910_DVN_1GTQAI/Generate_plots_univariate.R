library("RColorBrewer")
library("gplots")
library("ggplot2")
require("ggrepel")



load("./Sepsis_D0_milipore.RData")
load("./STD_log2_transformed_Sepsis_D0_Millipore.RData")

cyto_mili<-std_datanorm
tmp<-str_replace(colnames(cyto_mili),"sepsis.","S")
tmp<-str_replace(tmp,"control.","C")
tmp<-str_replace(tmp,"\\.0","")
Name_sepsiscyto_mili<-tmp
colnames(cyto_mili)<-tmp
rownames(cyto_mili)<-paste0("M_",rownames(cyto_mili))

#Cytokine
load("./STD_log2_transformed_Sepsis_D0_RandD.RData")
cyto_RD<-std_datanorm
tmp<-str_replace(colnames(cyto_RD),"sepsis.","S")
tmp<-str_replace(tmp,"control.","C")
tmp<-str_replace(tmp,"\\.0","")

Name_sepsiscyto_RD<-tmp
colnames(cyto_RD)<-tmp
rownames(cyto_RD)<-paste0("R_",rownames(cyto_RD))

common<-intersect(colnames(cyto_RD),colnames(cyto_mili))

req_cyto_mili<-cyto_mili[,match(common,Name_sepsiscyto_mili)]
req_cyto_RD<-cyto_RD[,match(common,Name_sepsiscyto_RD)]

data<-rbind(req_cyto_mili,req_cyto_RD)
Y<-Y
tmp<-data[,which(Y==0)]
tmp2<-data[,which(Y==1)]
data<-cbind(tmp,tmp2)
Y<-c(Y[which(Y==0)],Y[which(Y==1)])



filename<-'Cytokine_lg2_RD_milipore_2_univariate.pdf'


  tmp<-gsub("_N","",colnames(data))
  tmp<-gsub("props_","",tmp)
  tmp<-gsub("C","Control",tmp)
  tmp<-gsub("S","Sepsis",tmp)
  colnames(data)<-tmp
  
  tmp<-c()
  for(tmp2 in rownames(data)){
    tmp<-c(tmp,strsplit(tmp2,"_")[[1]][2])
  } 
  rownames(data)<-tmp

  pdf(filename, width=7, height=7)
  
  # Calcualting t.test and raw pvalue
  ttestdata <- function(df, grp1, grp2) {
    x = df[grp1]
    y = df[grp2]
    x = as.numeric(x)
    y = as.numeric(y)  
    results = t.test(x, y)
    results$p.value
  }
  rawpvalue = apply(data, 1, ttestdata, grp1 = which(Y==0), grp2 = which(Y==1))

  

  #computing fold change
  #datalog<-log2(rawdata)
  control=apply(data[,which(Y==0)],1,mean)
  control[which(is.infinite(control))]=0
  sepsis=apply(data[,which(Y==1)],1,mean)
  sepsis[which(is.infinite(sepsis))]=0
  foldchange <- sepsis-control

  results = data.frame("features"=names(rawpvalue),"EffectSize"=foldchange, "rawpvalue"=rawpvalue)
  #rownames(results)<-names(rawpvalue)
  results$padj<-p.adjust(rawpvalue, method = 'fdr')
  results$Significant <- ifelse(results$padj < 0.05, "FDR < 0.05", "Not Sig")

  rawpvalue_fdr_0.05<-approx(x=results$padj,y=results$rawpvalue,xout=0.05)$y

  results$Sig<-results$padj*0
  results$Sig[which(results$padj<0.05)]=1

  
  L<-c('Pentraxin3','Angiopoietin1','Angiopoietin2','E-Selectin','L-Selectin','Fractalkine','Procalcitonin','Endocan')
  R<-c('PTX3','Ang-1','Ang-2','E-Sel','L-Sel','CX3CL1','PCT','ESM-1')

#C-reactive protein CRP
   ind_L<-na.omit(match(L,results$features))
   ind_R<-na.omit(match(results$features[ind_L],L))
   if(length(na.omit(ind_L))!=0 & length(na.omit(ind_R))!=0){
    tmp<-as.character(results$features)
    tmp[ind_L]<-R[ind_R]
   }
  results$features<-as.factor(tmp)

  ind_L<-na.omit(match(L,rownames(data)))
  ind_R<-na.omit(match(rownames(data)[ind_L],L))
  if(length(na.omit(ind_L))!=0 & length(na.omit(ind_R))!=0){
    rownames(data)[ind_L]<-R[ind_R]
   }
 
  volcano = ggplot(results, aes(x = EffectSize, y = -log10(rawpvalue)))
  volcano  + geom_point(aes(color = Significant)) +
    scale_color_manual(values = c("magenta", "magenta")) +
    theme_bw(base_size = 12) + theme(legend.position = "orange") +
    #geom_hline(yintercept=-log10(1.2e-02), color="orange")+####<---- this is hard coded
    geom_hline(yintercept=-log10(rawpvalue_fdr_0.05), color="orange") +
    geom_text_repel(
      data = subset(results,Sig==1),
      aes(label = features),
      size = 4,
      color ='magenta',
      box.padding = unit(0.03, "lines"),
      point.padding = unit(0.2, "lines")
    ) +
    theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))

  
  
  col=rep("#4e004e",length(Y))
  col[Y==0]="#72bcd4"

  par(mar=c(15,4,4,1)+0.9) 
  #bottom, left, top, and right.
  heatmap.2(t(data[which(results$Sig==1),]), scale = "none", col = bluered(100),trace = "none", 
            density.info = "none",key=TRUE,symkey = FALSE,margin =c(10,8),RowSideColors=col,labRow = FALSE)
  
  dev.off()
