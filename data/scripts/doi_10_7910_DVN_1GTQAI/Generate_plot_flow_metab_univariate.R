remove(list = ls())

library("RColorBrewer")
library("gplots")
library("ggplot2")
require("ggrepel")

####Metabolities Data
load("../Metabolites/STD_VST_Normalized_Sepsis_D0_2.RData")

Pdata<-std_datanorm

colnames(Pdata)<-str_remove(colnames(Pdata)," ")

ext1 <- function(x) {  
  ( unlist(strsplit(x,"_"))[3])}
  N1<- sapply(colnames(Pdata)[1:11], ext1)
  N1<-paste0("C",N1)
  ext2 <- function(x) {  
    ( unlist(strsplit(x,"_"))[2])}
    N2<- sapply(colnames(Pdata)[12:25], ext2)
    N2<-paste0("S",N2)
    N2[4]<-"S13"
    N2[9]<-"S4"
    N2[12]<-"S7"
    colnames(Pdata)<-c(N1,N2)

    meta<-Pdata
    Name_meta<-colnames(Pdata)

##Bulk ow data
load("../Flow_data/STD_tanh_transformed_Sep_control.RData")
std_flow=t(std_flow)
Name_flow=colnames(std_flow)

####common
tmp<-intersect(Name_meta,Name_flow)
req_meta<-Pdata[,match(tmp,Name_meta)]
req_std_flow<-std_flow[,match(tmp,Name_flow)]
################


data<-rbind(req_std_flow,req_meta)
dim_data=c(nrow(req_std_flow),nrow(req_meta))

Y=c(rep(0,10),rep(1,12))


filename<-'Bulk_Flow_metabolites_integration_univariate_2.pdf'

pdf(filename)

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
  data_type=c(1,2)
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
  volcano  + geom_point(aes(color = view)) +
  scale_color_manual(values = c("#277306","orange","magenta","magenta")) +
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

    col_tick<-rep('orange',nrow(data[which(results$Sig==1),]))
    index<-match(subset(results,Sig==1 &  view ==1)$features,row.names(data[which(results$Sig==1),]))
    col_tick[index]='#277306'

    heatmap.2(t(data[which(results$Sig==1),]), scale = "none", col = bluered(100),trace = "none", cexCol=0.6, colCol=col_tick,
      density.info = "none",key=TRUE,symkey = FALSE,margin =c(10,8),RowSideColors=col2,labRow = FALSE)

    heatmap.2(t(data[which(results$Sig==1),]), scale = "none", col = bluered(100),trace = "none", cexCol=0.6, colCol=col_tick,
      density.info = "none",key=TRUE,symkey = FALSE,margin =c(10,8),RowSideColors=col2)

    Cor_mat<-cor(t(data[which(results$Sig==1),]),method="spearman")

    col_tick<-rep('orange',nrow(Cor_mat))
    index<-match(subset(results,Sig==1 &  view ==1)$features,row.names(Cor_mat))
    col_tick[index]='#277306'

    heatmap.2(Cor_mat, scale = "none", col = bluered(100),trace = "none", cexRow=0.6,cexCol =0.6,colRow=col_tick,colCol=col_tick,
      density.info = "none",key=TRUE,symkey = T,dendrogram="col",margins=c(6.3,6.3),keysize = 0.9)
    dev.off()
