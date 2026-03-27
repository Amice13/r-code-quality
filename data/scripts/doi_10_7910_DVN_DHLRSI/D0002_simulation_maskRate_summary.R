setwd("..")
source("../FUNCTIONS.R")  
outpath =".."
Rcpp::sourceCpp('../FUNCTIONS_CPP_JGNSC.cpp')

library(pheatmap)
library(umap)
library(huge)
library(JGL)
library(PRROC)

datapath = "../D0002mask"
date = gsub("-","_",Sys.Date())
# -------------------------

scenariov = "DI20"
metricv = c("auc","auprc","pcor","SSE")
nsample = c("_50_","_100_", "_200_", "_300_","_500_")
tpv = "AIC"
mrates = paste("_",seq(5,95, by=5),".txt", sep = "")

table.all <- NULL
 
ss=1
  scenario = scenariov[ss]
  cat(scenario,"\n")
  dtpath <- paste(datapath, sep = "/")
  
  allfiles <- list.files(dtpath)
  head(allfiles)
  res <- list()
  res[[1]] <- allfiles[grep("_AIC_", allfiles)]
  # for (tt in 1:length(tpv)){
  tt = 1
    cat(tpv[tt],"\n")
    for (mm in 1:length(metricv)){
      res1 <- res[[tt]][grep(metricv[mm], res[[tt]])]
      head(res1)
      for (rr in 1:length(mrates)){
        res2 <- res1[grep(mrates[rr], res1)]
        for (nn in 1:length(nsample)){
          res3 <- res2[grep(nsample[nn], res2)]
          tab <- do.call(rbind, lapply(res3, function(x){
            read.table(paste(dtpath,x, sep = "/"), sep = "\t", header = F)
          }))
          colnames(tab) <- c("values", "method")
          tab$nsample <- gsub("_","",nsample[nn])
          tab$metric <- metricv[mm]
          tab$TP <- tpv[tt]
          tab$scenario <- scenariov[ss]
          tab$rmask <- mrates[rr]
          table.all <- rbind(table.all, tab)
        }
      }
      
    } 

table.all$maskrate <- gsub(".txt","",table.all$rmask)
table.all$maskrate <- gsub(".txt","",table.all$rmask)    
table.all$maskrate <- gsub("_","",table.all$maskrate)
table.all$maskrate <- as.factor(table.all$maskrate)
saveRDS(table.all, "D0002mask_summary_tableall.rds")


# ===============
head(table.all) 
table.all <- readRDS("D0002mask_summary_tableall.rds") 
for (xx in 1:length(unique(table.all$nsample))){
  select.nsample = unique(table.all$nsample)[xx]
  dt = table.all[table.all$method == "JGNsc + iter" & table.all$nsample == select.nsample,]
  p1 = ggplot(dt, aes(x = maskrate, y= values)) + geom_boxplot(outlier.shape = NA) +
    facet_grid( metric ~., scales = "free_y") +
    ggtitle(paste("N sample: ", select.nsample, sep = "")) +
    xlab("Mask percentage (%)") + 
    theme(legend.position = "none")
  
  pdf(paste("D0002mask_summary_",select.nsample,"_",date, ".pdf", sep = ""), height = 8, width = 6)
  print(p1)
  dev.off()
  
}

library(ggthemes)
dt = table.all[table.all$method == "JGNsc + iter",]
dt$nsample <- factor(dt$nsample, levels = c("50", "100", "200", "300","500"))
p1 = ggplot(dt, aes(x = maskrate, y= values)) + geom_tufteboxplot() +
  facet_grid( metric ~ nsample, scales = "free_y") +
  # ggtitle(paste("N sample: ", select.nsample, sep = "")) +
  xlab("Mask percentage (%)") + 
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90))
p1
dev.off()
pdf(paste("D0002mask_summary_all_",date, ".pdf", sep = ""), height = 8, width = 10)
print(p1)
dev.off()
