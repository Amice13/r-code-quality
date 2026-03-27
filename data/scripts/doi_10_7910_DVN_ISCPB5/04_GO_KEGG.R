MEexp <- rbind(ME2exp, ME3exp, ME7exp)
library(clusterProfiler)
library(org.Hs.eg.db)
library(AnnotationDbi)
keytypes(org.Hs.eg.db)
AnnotationDbi::select(org.Hs.eg.db,keys=rownames(MEexp),columns = "ENTREZID",keytype = "SYMBOL") -> MEgeneEntrezid
na.omit(MEgeneEntrezid$ENTREZID)->MEgeneEntrezid
resgoBP <- enrichGO(MEgeneEntrezid,"org.Hs.eg.db",ont = "BP")
resgoBP <- clusterProfiler::simplify(resgoBP, cutoff=0.7, by="pvalue", select_fun=min)
dotplot(resgoBP,font.size =12)+ 
  facet_grid( scale="free") + 
  scale_y_discrete(labels=function(x) str_wrap(x, width=40)) 
ggsave(filename = 'GO_BP.pdf', width = 8, height = 8, dpi = 600)

resgoMF <- enrichGO(MEgeneEntrezid,"org.Hs.eg.db",ont = "MF")
resgoMF <- clusterProfiler::simplify(resgoMF, cutoff=0.7, by="pvalue", select_fun=min)
dotplot(resgoMF,font.size =12)+ 
  facet_grid(scale="free") + 
  scale_y_discrete(labels=function(x) str_wrap(x, width=40)) 
ggsave(filename = 'GO_MF.pdf', width = 8, height = 8, dpi = 600)

resgoCC <- enrichGO(MEgeneEntrezid,"org.Hs.eg.db",ont = "CC")
resgoCC <- clusterProfiler::simplify(resgoCC, cutoff=0.7, by="pvalue", select_fun=min)
dotplot(resgoCC,font.size =12)+ 
  facet_grid(scale="free") + 
  scale_y_discrete(labels=function(x) str_wrap(x, width=40)) 
ggsave(filename = 'GO_CC.pdf', width = 8, height = 8, dpi = 600)

resKEGG <- enrichKEGG(MEgeneEntrezid,organism = "hsa")
dotplot(resKEGG,font.size =12)+ 
  facet_grid(scale="free") + 
  scale_y_discrete(labels=function(x) str_wrap(x, width=40)) 
ggsave(filename = 'KEGG.pdf', width = 8, height = 8, dpi = 600)
