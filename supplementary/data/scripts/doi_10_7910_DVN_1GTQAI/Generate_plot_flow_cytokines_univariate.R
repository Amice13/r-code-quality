library("RColorBrewer")
library("gplots")
library("ggplot2")
require("ggrepel")
library('stringr')

####Cytokines

load("../Cytokines/STD_log2_transformed_Sepsis_D0_Millipore.RData")

cyto_mili<-std_datanorm
tmp<-str_replace(colnames(cyto_mili),"sepsis.","S")
tmp<-str_replace(tmp,"control.","C")
tmp<-str_replace(tmp,"\\.0","")
Name_sepsiscyto<-tmp
colnames(cyto_mili)<-tmp

#Cytokine
load("../Cytokines/STD_log2_transformed_Sepsis_D0_RandD.RData")
cyto_RD<-std_datanorm
tmp<-str_replace(colnames(cyto_RD),"sepsis.","S")
tmp<-str_replace(tmp,"control.","C")
tmp<-str_replace(tmp,"\\.0","")

Name_sepsiscyto_RD<-tmp
colnames(cyto_RD)<-tmp

##Bulk ow data
load("../Flow_data/STD_tanh_transformed_Sep_control.RData")
std_flow=t(std_flow)
Name_flow=colnames(std_flow)

####common
tmp<-intersect(intersect(Name_sepsiscyto_RD,Name_sepsiscyto),Name_flow)

req_cyto_RD<-cyto_RD[,match(tmp,Name_sepsiscyto_RD)]
req_cyto_mili<-cyto_mili[,match(tmp,Name_sepsiscyto)]
req_std_flow<-std_flow[,match(tmp,Name_flow)]
################



data<-rbind(req_std_flow,req_cyto_mili,req_cyto_RD)

Y=c(rep(0,21),rep(1,17))
#0 is Sepsis
#1 is Normal


filename<-'Bulk_Flow_Cyto_RD_mili_integration_univariate.pdf'

pdf(filename)
#changing the Y to make it consistent with other plots
#i.e calling Y=0 as Normal and Y=1 as Sepsis instead of otherway round
Y=c(rep(1,21),rep(0,17))


col2=rep("#4e004e",length(Y))
col2[Y==0]="#72bcd4"


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
  results$padj<-p.adjust(rawpvalue, method = 'fdr')
  results$Significant <- ifelse(results$padj < 0.05, "FDR < 0.05", "Not Sig")

  rawpvalue_fdr_0.05<-approx(x=results$padj,y=results$rawpvalue,xout=0.05)$y

  #names(which(sort(abs(rowMeans(C0[-1,])))>0.01))
  ## Volcano plot using lasso regression features req_met_0_01
  results$Sig<-results$padj*0
  results$Sig[which(results$padj<0.05)]=1

  data_type=c(1,3)
  nview=length(dim_data)

  
  tmp=c()
  for(i in 1:nview){
      tmp<-c(tmp,rep(data_type[i],dim_data[i]))
  }
  results$view<-as.character(tmp)
  
  L<-c('Pentraxin3','Angiopoietin1','Angiopoietin2','E-Selectin','L-Selectin','Fractalkine','Procalcitonin','Endocan')
  R<-c('PTX3','Ang-1','Ang-2','E-Sel','L-Sel','CX3CL1','PCT','ESM-1')
#C-reactive protein CRP
tmp<-as.character(results$features)
    ind_L<-na.omit(match(L,results$features))
   ind_R<-na.omit(match(results$features[ind_L],L))
   if(length(na.omit(ind_L))!=0 & length(na.omit(ind_R))!=0){
    
    tmp[ind_L]<-R[ind_R]
   }
  results$features<-as.factor(tmp)

  ind_L<-na.omit(match(L,rownames(data)))
  ind_R<-na.omit(match(rownames(data)[ind_L],L))
  if(length(na.omit(ind_L))!=0 & length(na.omit(ind_R))!=0){
    rownames(data)[ind_L]<-R[ind_R]
   }

  volcano = ggplot(results, aes(x = EffectSize, y = -log10(rawpvalue)))
  volcano   + geom_point(aes(color = view)) +
    scale_color_manual(values = c("#277306","magenta","magenta","magenta")) +
    theme_bw(base_size = 12) + theme(legend.position = "orange") +
    geom_hline(yintercept=-log10(rawpvalue_fdr_0.05), color="red")+
    #geom_vline(xintercept=-1, color="orange")+
    #geom_vline(xintercept=1, color="orange") +
    geom_text_repel(
      data = subset(results,Sig==1 &  view ==1),
      aes(label = features),
      size = 4,
      color ='#277306',
      box.padding = unit(0.35, "lines"),
      point.padding = unit(0.3, "lines")
    ) + 
    geom_text_repel(
      data = subset(results,Sig==1 &  view ==2),
      aes(label = features),
      size = 4,
      color ='orange',
      box.padding = unit(0.35, "lines"),
      point.padding = unit(0.3, "lines")
    ) +
    geom_text_repel(
      data = subset(results,Sig==1 &  view ==3),
      aes(label = features),
      size = 4,
      color ='magenta',
      box.padding = unit(0.35, "lines"),
      point.padding = unit(0.3, "lines")
    ) +
    geom_text_repel(
      data = subset(results,Sig==1 &  view ==4),
      aes(label = features),
      size = 4,
      color ='magenta',
      box.padding = unit(0.35, "lines"),
      point.padding = unit(0.3, "lines")
    ) 
    theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))
  
  col2=rep("#4e004e",length(Y))
  col2[Y==0]="#72bcd4"

#par(mar=c(15,4,4,1)+0.9) 
  #bottom, left, top, and right.
  col_tick<-rep('magenta',nrow(data[which(results$Sig==1),]))
  index<-match(subset(results,Sig==1 &  view ==1)$features,row.names(data[which(results$Sig==1),]))
  col_tick[index]='#277306'

  heatmap.2(t(data[which(results$Sig==1),]), scale = "none", col = bluered(100),trace = "none",cexCol=0.6, colCol=col_tick,
            density.info = "none",key=TRUE,symkey = FALSE,margin =c(10,8),RowSideColors=col2,labRow = FALSE)


 Cor_mat<-cor(t(data[which(results$Sig==1),]),method="spearman")

 col_tick<-rep('magenta',nrow(Cor_mat))
  index<-match(subset(results,Sig==1 &  view ==1)$features,row.names(Cor_mat))
  col_tick[index]='#277306'

    heatmap.2(Cor_mat, scale = "none", col = bluered(100),trace = "none", cexRow=0.6,cexCol =0.6,colRow=col_tick,colCol=col_tick,
              density.info = "none",key=TRUE,symkey = T,dendrogram="col",margins=c(6.3,6.3),keysize = 0.9)
  
  dev.off()
