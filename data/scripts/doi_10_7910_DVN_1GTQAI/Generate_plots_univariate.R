library("RColorBrewer")
library("gplots")
library("ggplot2")
require("ggrepel")

load("./STD_tanh_transformed_Sep_control.RData")
std_flow=t(std_flow)

data<-std_flow
filename<-'ONly_Bulk_marker_flow data_univariate.pdf'



  tmp<-gsub("_N","",colnames(data))
  tmp<-gsub("props_","",tmp)
  tmp<-gsub("C","Control",tmp)
  tmp<-gsub("S","Sepsis",tmp)
  colnames(data)<-tmp
  
  pdf(filename)
  
 
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

  req_pval<-sort(results$padj[which((results$padj-0.05)<0)],decreasing =T)[1]
  ind<-which(results$padj==req_pval)[1]
  
  rawpvalue_fdr_0.05<-approx(x=results$padj,y=results$rawpvalue,xout=0.05)$y

  results$Sig<-results$padj*0
  results$Sig[which(results$padj<0.05)]=1
 
  volcano = ggplot(results, aes(x = EffectSize, y = -log10(rawpvalue)))
  volcano  + geom_point(aes(color = Significant)) +
    scale_color_manual(values = c("#277306", "#277306")) +
    theme_bw(base_size = 12) + theme(legend.position = "orange") +
    geom_hline(yintercept=-log10(rawpvalue_fdr_0.05), color="orange")+
    geom_text_repel(
      data = subset(results,Sig==1),
      aes(label = features),
      size = 4,
      color ='#277306',
      box.padding = unit(0.35, "lines"),
      point.padding = unit(0.3, "lines")
    ) + 
   
    theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))
  
col=rep("#4e004e",length(Y))
  col[Y==0]="#72bcd4"


  
  heatmap.2(t(data[which(results$Sig==1),]), scale = "none", col = bluered(100),trace = "none", cexRow = 0.5,
            density.info = "none",key=TRUE,symkey = FALSE,margin =c(5,8),RowSideColors=col,labRow = FALSE)
  
  
  
  dev.off()

