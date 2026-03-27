remove(list = ls())

load('meta_all_bulk_Flow_Cyto_RD_mili_integration_Lasso.RData')
#save.image('GSVA_meta_onto_bulk_Flow_Cyto_RD_mili_integration_Lasso.RData')

#filename<-'meta_all_bulk_Flow_Cyto_RD_mili_integration_Lasso.pdf'
filename<-'meta_all_bulk_Flow_Cyto_RD_mili_integration_Lasso_2.pdf'

types=rep(1,length(req_met_0_1))
types[match(rownames(req_meta),req_met_0_1)]=2
types[match(rownames(req_cyto_mili),req_met_0_1)]=3
types[match(rownames(req_cyto_RD),req_met_0_1)]=4
types_0_1<-types

tmp_met_0_01<-names(which(sort(abs(rowMeans(C[-1,])))>0.01))
types=rep(1,length(tmp_met_0_01))
types[match(rownames(req_meta),tmp_met_0_01)]=2
types[match(rownames(req_cyto_mili),tmp_met_0_01)]=3
types[match(rownames(req_cyto_RD),tmp_met_0_01)]=4

types_0_01<-types


  
  pdf(filename)
  
  col=rep("#277306",length(types_0_01))#flow
  col[types_0_01==3]="magenta"#cytokines
  col[types_0_01==2]="orange"#metab
  col[types_0_01==4]="magenta"#"red"#cytokines

  #col2=rep("#72bcd4",length(Y))
  #col2[Y==0]="#4e004e"

  col2=rep("#4e004e",length(Y))
  col2[Y==0]="#72bcd4"


  heatmap.2(t(data[tmp_met_0_01,]), scale = "none", col = bluered(100),trace = "none", 
            density.info = "none",key=TRUE,symkey = FALSE,ColSideColors=col,RowSideColors=col2,margins=c(8,8),labRow = FALSE)
 
  col=rep("#277306",length(types_0_1))#flow
  col[types_0_1==3]="magenta"#cytokines
  col[types_0_1==2]="orange"#metab
  col[types_0_1==4]="magenta"#"red"#cytokines
  heatmap.2(t(data[req_met_0_1,]), scale = "none", col = bluered(100),trace = "none",
            density.info = "none",key=TRUE,symkey = FALSE,ColSideColors=col,RowSideColors=col2,margins=c(8,8),labRow = FALSE)
  
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
  results$Lasso<-results$padj*0
  tmp<-cbind(match(tmp_met_0_01,results$features),types_0_01)
  results$Lasso[tmp[,1]]=1
  nview=length(dim_data)

  data_type=c(1,2,3,4)
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
  volcano  + ggtitle('Volcano Plot: Beta>0.01') + geom_point(aes(color = view)) +
    scale_color_manual(values = c("#277306","orange","magenta","magenta")) +
    theme_bw(base_size = 12) + theme(legend.position = "orange") +
    geom_hline(yintercept=-log10(rawpvalue_fdr_0.05), color="red")+
    #geom_vline(xintercept=-1, color="orange")+
    #geom_vline(xintercept=1, color="orange") +
    geom_text_repel(
      data = subset(results,Lasso==1 &  view ==1),
      aes(label = features),
      size = 4,
      color ='#277306',
      box.padding = unit(0.35, "lines"),
      point.padding = unit(0.3, "lines")
    ) + 
    geom_text_repel(
      data = subset(results,Lasso==1 &  view ==2),
      aes(label = features),
      size = 4,
      color ='orange',
      box.padding = unit(0.35, "lines"),
      point.padding = unit(0.3, "lines")
    ) +
    geom_text_repel(
      data = subset(results,Lasso==1 &  view ==3),
      aes(label = features),
      size = 4,
      color ='magenta',
      box.padding = unit(0.35, "lines"),
      point.padding = unit(0.3, "lines")
    ) +
    geom_text_repel(
      data = subset(results,Lasso==1 &  view ==4),
      aes(label = features),
      size = 4,
      color ='magenta',
      box.padding = unit(0.35, "lines"),
      point.padding = unit(0.3, "lines")
    ) 
    theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))
  
  ## Volcano plot using lasso regression features req_met_0_1
  results$Lasso<-results$padj*0
  tmp<-cbind(match(req_met_0_1,results$features),types_0_1)
  results$Lasso[tmp[,1]]=1
  nview=length(dim_data)

  data_type=c(1,2,3,4)
  tmp=c()
  for(i in 1:nview){
      tmp<-c(tmp,rep(data_type[i],dim_data[i]))
  }
  results$view<-as.character(tmp)
  
  
  
  volcano = ggplot(results, aes(x = EffectSize, y = -log10(rawpvalue)))
  volcano  + ggtitle('Volcano Plot: Beta>0.1') + geom_point(aes(color = view)) +
    scale_color_manual(values = c("#277306","orange","magenta","magenta")) +
    theme_bw(base_size = 12) + theme(legend.position = "orange") +
    geom_hline(yintercept=-log10(rawpvalue_fdr_0.05), color="red")+
    #geom_vline(xintercept=-1, color="orange")+
    #geom_vline(xintercept=1, color="orange") +
    geom_text_repel(
      data = subset(results,Lasso==1 &  view ==1),
      aes(label = features),
      size = 4,
      color ='#277306',
      box.padding = unit(0.35, "lines"),
      point.padding = unit(0.3, "lines")
    ) + 
    geom_text_repel(
      data = subset(results,Lasso==1 &  view ==2),
      aes(label = features),
      size = 4,
      color ='orange',
      box.padding = unit(0.35, "lines"),
      point.padding = unit(0.3, "lines")
    ) +
    geom_text_repel(
      data = subset(results,Lasso==1 &  view ==3),
      aes(label = features),
      size = 4,
      color ='magenta',
      box.padding = unit(0.35, "lines"),
      point.padding = unit(0.3, "lines")
    ) +
    geom_text_repel(
      data = subset(results,Lasso==1 &  view ==4),
      aes(label = features),
      size = 4,
      color ='magenta',
      box.padding = unit(0.35, "lines"),
      point.padding = unit(0.3, "lines")
    ) 
    theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))


    ind_L<-na.omit(match(L,tmp_met_0_01))
    ind_R<-na.omit(match(tmp_met_0_01[ind_L],L))
    if(length(na.omit(ind_L))!=0 & length(na.omit(ind_R))!=0){
    tmp_met_0_01[ind_L]<-R[ind_R]
    }
    
    
    cor_map<-cor(t(data[tmp_met_0_01,]),method="spearman")
    #diag(cor_map)=0
    heatmap.2(cor_map, scale = "none", col = bluered(100),trace = "none", 
              density.info = "none",key=TRUE,symkey = FALSE,dendrogram="col",margins=c(5,8))
    
    tmp<-which(abs(cor_map)>0.7, arr.ind=TRUE)
    tmp2<-which(tmp[,1]<tmp[,2])
    
    
    Cor_mat<-cor(t(data[tmp_met_0_01,]),method="spearman")

    col_tick<-rep('orange',nrow(Cor_mat))
    index<-match(subset(results, view ==1)$features,row.names(Cor_mat))
    col_tick[index]='#277306'
    index<-match(subset(results,view ==3)$features,row.names(Cor_mat))
    col_tick[index]='magenta'
    index<-match(subset(results,view ==4)$features,row.names(Cor_mat))
    col_tick[index]='magenta'


    heatmap.2(Cor_mat, scale = "none", col = bluered(100),trace = "none", cexRow=0.8,cexCol =0.8,colRow=col_tick,colCol=col_tick,
              density.info = "none",key=TRUE,symkey = T,dendrogram="col",margins=c(6,6),keysize = 1.2)

      dev.off()