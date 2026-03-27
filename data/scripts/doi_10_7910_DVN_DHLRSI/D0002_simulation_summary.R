# ------------------------------------
# summarize results from simulation
# ------------------------------------
setwd("..")
source("../FUNCTIONS.R")  
outpath =".."
Rcpp::sourceCpp('../FUNCTIONS_CPP_JGNSC.cpp')

library(pheatmap)
library(umap)
library(huge)
library(JGL)
library(PRROC)

# -------------------------
date = gsub("-","_",Sys.Date()) #20210309
scenariov = c("DD","DI20","DI50","ID")
 metricv = c("auc","auprc","pcor","SSE")
nsample = c("_50_","_100_", "_200_", "_300_","_500_")
 
tpv = c("AIC","BIC","EBIC","STARS")
 

table.all <- NULL

for (ss in 1:length(scenariov)){
  scenario = scenariov[ss]
  cat(scenario,"\n")
  # dtpath <- paste("C:/Users/meichen/OneDrive - University of North Carolina at Chapel Hill/994/medulloblastoma/data/D0002v2",scenario, sep = "/")
  # dtpath <- paste("C:/Users/meichen/OneDrive - University of North Carolina at Chapel Hill/994/medulloblastoma/data/D0002v3",scenario, sep = "/")
  # dtpath <- paste("C:/Users/meichen/OneDrive - University of North Carolina at Chapel Hill/994/medulloblastoma/data/D0002",scenario, sep = "/")
  dtpath <- paste("C:/Users/meichen/OneDrive - University of North Carolina at Chapel Hill/994/medulloblastoma/data/D0002cpp",scenario, sep = "/")
  
  allfiles <- list.files(dtpath)
  res <- list()
  res[[1]] <- allfiles[grep("_AIC_", allfiles)]
  res[[2]] <- allfiles[grep("_BIC_", allfiles)]
  res[[3]] <- allfiles[grep("_EBIC_", allfiles)]
  res[[4]] <- allfiles[grep("_STARS_", allfiles)]
  for (tt in 1:length(tpv)){
    cat(tpv[tt],"\n")
    for (mm in 1:length(metricv)){
      res1 <- res[[tt]][grep(metricv[mm], res[[tt]])]
      for (nn in 1:length(nsample)){
        temp <- res1[grep(paste(nsample[nn], scenario, sep = ""), res1)] 
        tab <- do.call(rbind, lapply(temp, function(x){
          read.table(paste(dtpath,x, sep = "/"), sep = "\t", header = F)
        }))
        colnames(tab) <- c("values", "method")
        tab$nsample <- gsub("_","",nsample[nn])
        tab$metric <- metricv[mm]
        tab$TP <- tpv[tt]
        tab$scenario <- scenariov[ss]
        table.all <- rbind(table.all, tab)
      }
    }
  }
}
 
saveRDS(table.all, "D0002_summary_tableall_0419.rds")


# ===============
head(table.all) 
table.all0 <- readRDS("D0002_summary_tableall.rds") 
method.index <- 1:5

hist(table.all$values[table.all$nsample==50 & table.all$method =="JGNsc" & table.all$TP =="STARS" & table.all$metric =="SSE"])
table.all <- table.all0[table.all0$TP =="STARS" & table.all0$metric =="SSE" & table.all0$values < 100,]
table.all1 <- table.all0[!(table.all0$TP =="STARS" & table.all0$metric =="SSE"),]
table.all2 <- rbind(table.all, table.all1)

for (m in method.index){
  for (s in 1:4){
    select.method = unique(table.all2$method)[m]
    # select.n = unique(table.all2$nsample)[4]
    # select.metric = unique(table.all2$metric)[1]
    select.scenario = unique(table.all2$scenario)[s]
    benchtp <- table.all2[(table.all2$method == select.method) 
                          # & (table.all2$nsample == select.n)
                          # & (table.all2$metric == select.metric)
                          & (table.all2$scenario == select.scenario),]
    benchtp$nsample <- factor(benchtp$nsample, levels = c("50","100","200","300","500"))
    benchtp$metric <- toupper(benchtp$metric)
    p1 = ggplot(benchtp, aes(x = TP, y= values, color = TP)) + geom_boxplot(outlier.shape = NA) +
      facet_grid( metric ~ nsample, scales = "free_y") + 
      ggtitle(paste("Scenario: ", select.scenario, ", method: ", select.method, sep = "")) + 
      xlab("") + 
      theme(legend.position = "none")
    
    pdf(paste("D0002_summary_TPbenchmark",select.scenario, select.method,date, ".pdf", sep = ""), height = 8, width = 8)
    print(p1)
    dev.off()
  }
}


# ================================
# use AIC and compare the methods
# ================================
 
method.index <- 1:5
showlevels <- c("NoDropout","IterativeJGNsc", "JGNsc", "McImpute", "Observed")

for (s in 1:4){
  select.scenario = unique(table.all2$scenario)[s] 
  select.methods = unique(table.all2$method)[method.index]
  benchmethod <-  table.all2[(table.all2$TP == "AIC") 
                             # & (table.all2$nsample == select.n)
                             # & (table.all2$metric == select.metric)
                             & (table.all2$scenario == select.scenario)
                             & (table.all2$method %in% select.methods),]
  
  benchmethod$nsample <- factor(benchmethod$nsample, levels = c("50","100","200","300","500"))
  benchmethod$metric <- toupper(benchmethod$metric) 
  
  benchmethod$method2 <- benchmethod$method
  benchmethod$method2[benchmethod$method == "JGNsc + iter"] <- "IterativeJGNsc"
  benchmethod$method2 <- factor(benchmethod$method2, levels = showlevels)
  p1 = ggplot(benchmethod, aes(x = method2, y= values, color = method)) + geom_boxplot() +
    facet_grid( metric ~ nsample, scales = "free_y") + 
    ggtitle(paste("Scenario: ", select.scenario, sep = "")) + 
    xlab("") + 
    theme(legend.position = "top",
          axis.text.x = element_text(angle = 70, hjust = 1))
  # p1
  pdf(paste("D0002_summary_METHODbenchmark",select.scenario,date, ".pdf", sep = ""), height = 8, width = 7)
  print(p1)
  dev.off()
}
