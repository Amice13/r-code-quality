setwd("..")
source("../FUNCTIONS.R")  
outpath =".."
Rcpp::sourceCpp('../FUNCTIONS_CPP_JGNSC.cpp')


# scenarios = c("DD","DI20","DI50","ID")
scenarios = c("DI20")

lf = list.files()
# lf.dd = lf[grep("DD", lf)]
lf.di20 = lf[grep("DI20", lf)]
# lf.di50 = lf[grep("DI50", lf)]
# lf.id = lf[grep("ID", lf)]

 
# allres.DD = NULL
# for (i in 1:length(lf.dd)){
#   tmp = read.table(lf.dd[i], sep = "/t", header = 1)
#   allres.DD = rbind(allres.DD, tmp)
# }

allres.DI20 = NULL
for (i in 1:length(lf.di20)){
  tmp = read.table(lf.di20[i], sep = "/t", header = 1)
  allres.DI20 = rbind(allres.DI20, tmp)
}
# allres.DI50 = NULL
# for (i in 1:length(lf.di50)){
#   tmp = read.table(lf.di50[i], sep = "/t", header = 1)
#   allres.DI50 = rbind(allres.DI50, tmp)
# }
# allres.ID = NULL
# for (i in 1:length(lf.id)){
#   tmp = read.table(lf.id[i], sep = "/t", header = 1)
#   allres.ID = rbind(allres.ID, tmp)
# }

library(ggplot2) 
# allres.DD$nn = factor(allres.DD$nn, levels = c(100,200,500,1000))
allres.DI20$nn = factor(allres.DI20$nn, levels = c(50,100,200,300,500,1000))
# allres.ID$nn = factor(allres.ID$nn, levels = c(100,200,500,1000))
# allres.DI50$nn = factor(allres.DI50$nn, levels = c(100,200,500,1000))

# ggplot(allres.DD, aes(x = nn, y = AUC, color = methods)) + geom_boxplot()
# ggplot(allres.ID, aes(x = nn, y = AUC, color = methods)) + geom_boxplot()
ggplot(allres.DI20, aes(x = nn, y = AUC, color = methods)) + geom_boxplot()
# ggplot(allres.DI50, aes(x = nn, y = AUPRC, color = methods)) + geom_boxplot()

# p1 = ggplot(allres.DI20, aes(x = nn, y = AUC, color = methods)) + geom_boxplot() 
# p2 = ggplot(allres.DI20, aes(x = nn, y = AUPRC, color = methods)) + geom_boxplot()
# p3 = ggplot(allres.DI20, aes(x = nn, y = SSE, color = methods)) + geom_boxplot()
# p4 = ggplot(allres.DI20, aes(x = nn, y = Pearson, color = methods)) + geom_boxplot()

library(reshape2)
colnames(allres.DI20)[2] = "AUROC"
allres.DI20 = allres.DI20[allres.DI20$nn != 1000,]
allres.DI20.long = melt(allres.DI20)
allres.DI20.long = allres.DI20.long[allres.DI20.long$variable != "simulation",]
allres.DI20.long$variable = factor(allres.DI20.long$variable, levels = c("AUROC","AUPRC","Pearson","SSE"))

p1 = ggplot(allres.DI20, aes(x = nn, y = AUROC, color = methods)) + geom_boxplot() + theme(legend.position = c(0.1,0.8))+ xlab("Nsamples")
p2 = ggplot(allres.DI20, aes(x = nn, y = AUPRC, color = methods)) + geom_boxplot() + theme(legend.position = "none")+ xlab("Nsamples")
p3 = ggplot(allres.DI20, aes(x = nn, y = Pearson, color = methods)) + geom_boxplot() + theme(legend.position =  "none")+ xlab("Nsamples")
p4 = ggplot(allres.DI20, aes(x = nn, y = SSE, color = methods)) + geom_boxplot() + theme(legend.position =  "none")+ xlab("Nsamples")

library(cowplot)
plot_grid(p1, p2, p3, p4, ncol = 4)


pdf("C:/Users/meichen/OneDrive - University of North Carolina at Chapel Hill/994/medulloblastoma/data/Benchmark_existingGRN_GENIE3_SUMMARY_20211006.pdf", height = 2, width = 7)
# ggplot(allres.DI20.long, aes(x = value, y = nn, color = methods)) + geom_boxplot()+
#   facet_grid(. ~ variable, scales = "free_x") + xlab("Nsamples") + ylab("")
plot_grid(p1, p2, p3, p4, ncol = 4)
dev.off()



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~``
# combine with figure 2 -- D0002_simulation_summary.R
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ------------------------------------
# summarize results from simulation
# ------------------------------------
setwd("C:/Users/meichen/OneDrive - University of North Carolina at Chapel Hill/994/medulloblastoma/data")
library(pheatmap)
library(umap)
library(huge)
library(JGL)
library(PRROC)

# -------------------------
date = gsub("-","_",Sys.Date()) #20210309

# ===============
table.all0 <- readRDS("D0002_summary_tableall.rds") 
method.index <- 1:5
table.all <- table.all0[table.all0$TP =="STARS" & table.all0$metric =="SSE" & table.all0$values < 100,]
table.all1 <- table.all0[!(table.all0$TP =="STARS" & table.all0$metric =="SSE"),]
table.all2 <- rbind(table.all, table.all1)



# ================================
# use AIC and compare the methods
# ================================
method.index <- 1:5
# method.index2 <- c(1,3)
showlevels <- c("NoDropout","JGNsc_Hybrid", "JGNsc_Bayesian", "McImpute", "Observed", "GENIE3")

s=2
select.scenario = unique(table.all2$scenario)[s] 
select.methods = unique(table.all2$method)[method.index]
benchmethod <-  table.all2[(table.all2$TP == "AIC") 
                           & (table.all2$scenario == select.scenario)
                           & (table.all2$method %in% select.methods),]

benchmethod$nsample <- factor(benchmethod$nsample, levels = c("50","100","200","300","500"))
benchmethod$metric <- toupper(benchmethod$metric) 

benchmethod$method2 <- benchmethod$method
benchmethod$method2[benchmethod$method == "JGNsc + iter"] <- "JGNsc_Hybrid"
benchmethod$method2[benchmethod$method == "JGNsc"] <- "JGNsc_Bayesian"
benchmethod$method2 <- factor(benchmethod$method2, levels = showlevels)
table(benchmethod$metric)
benchmethod$metric[benchmethod$metric == "AUC"] = "AUROC"
benchmethod$metric[benchmethod$metric == "PCOR"] = "Pearson"


resgenie = data.frame(method2 = allres.DI20.long$methods, nsample = allres.DI20.long$nn,
                      values = allres.DI20.long$value,
                      metric = allres.DI20.long$variable)
resgenie = resgenie[resgenie$method2 != "JGNsc",]
# resgenie$method2[resgenie$method2 == "JGNsc"] = "JGNsc_Hybrid"

benchmethod2 = rbind.data.frame(benchmethod[,c(7,3,1,4)], resgenie)
benchmethod2$nsample = factor(benchmethod2$nsample, levels = c("50","100","200","300","500"))

p1 = ggplot(benchmethod2, aes(x = method2, y= values, color = method2)) + geom_boxplot() +
  facet_grid( metric ~ nsample, scales = "free_y") + 
  ggtitle(paste("Scenario: ", select.scenario, sep = "")) + 
  xlab("") + labs(color = "Method") +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 70, hjust = 1))


pdf(paste("Benchmark_exsitingGRN_GENIE3_D0002_summary_",date, ".pdf", sep = ""), height = 8, width = 7)
print(p1)
dev.off()


# old
# # shared edge detection rate
# setwd("C:/Users/meichen/OneDrive - University of North Carolina at Chapel Hill/994/medulloblastoma/data/GENIE3_s2sldr")
# lf = list.files()
# sldr = NULL
# for (i in 1:length(lf)){
#   dt1 = t(read.table(lf[i], header = T))
#   sldr = rbind(dt1, sldr)
# }
# colnames(sldr) =c("sldr_JGNSC","sldr_GENIE3","nsample","simulation","scenario")
# dtlong1 = sldr[,-1]
# colnames(dtlong1)[1] = "value"
# dtlong1 = as.data.frame(dtlong1)
# dtlong1$method = "GENIE3"
# dtlong2 = sldr[,-2]
# colnames(dtlong2)[1] = "value"
# dtlong2 = as.data.frame(dtlong2)
# dtlong2$method = "JGNsc"
# dtlong = rbind(dtlong1, dtlong2)
# dtlong$value = as.numeric(dtlong$value)
# dtlong$nsample = factor(dtlong$nsample, levels = c(50,100,200,300,500))
# dtlong$scenario2 = "Scenario i) case 1"
# dtlong$scenario2[dtlong$scenario == "DI50"] = "Scenario i) case 2"
# dtlong$scenario2[dtlong$scenario == "ID"] = "Scenario ii)"
# dtlong$scenario2[dtlong$scenario == "DD"] = "Scenario iii)"
# 
# 
# pdf("C:/Users/meichen/OneDrive - University of North Carolina at Chapel Hill/994/medulloblastoma/data/Benchmark_existingGRN_GENIE3_SUMMARY_SharedLinkDetection.pdf", height = 6, width = 7)
# ggplot(dtlong, aes(x = nsample, y = value, color = method)) + geom_boxplot() + 
#   facet_wrap(scenario2 ~.) + ylab("Shared Link Detection Rate") + 
#   theme(legend.position = "top") + xlab("N sample per group")
# dev.off()


# shared edge detection rate
setwd("C:/Users/meichen/OneDrive - University of North Carolina at Chapel Hill/994/medulloblastoma/data/GENIE3_s2sldr")
lf = list.files()
scenarios = c("DI20","DI50","ID","DD")

allt = NULL
for(s in scenarios){
  sldr = NULL
  lf1 = lf[grep(s, lf)]
  for (i in 1:length(lf1)){
    dt1 = read.table(lf1[i], header = T)
    sldr = rbind(dt1, sldr)
  }
  sldr.mean = aggregate(x = sldr[,1:2], by = list(thresh = sldr$X3, s = sldr$s, nsample = sldr$nn, method = sldr$method), FUN = "mean")
  allt = rbind(sldr.mean, allt)
}

a1 = expand.grid(c("DI20","DI50","ID","DD"), c(50,100,200,300,500))
a1$thresh = 0
a1$method = "JGNsc"
a1$X1 = 1
a1$X2 = 1
colnames(a1)[1:2] = c("s","nsample")
a1$nsample = as.factor(a1$nsample)
allt0 = rbind(allt, a1)
allt0$nsample = factor(allt0$nsample, levels = c(50,100,200,300,500))
allt0$s = factor(allt0$s, levels = c("DI20","DI50","ID","DD"))

# ggplot(allt[allt$s =="DD",], aes(x = X2, y = X1, color = method)) + geom_point() + 
#   facet_wrap(nsample ~.) + ylab("TPR") + 
#   theme(legend.position = "top") + xlab("FPR")
ggplot(allt0, aes(x = X2, y = X1, color = method)) + geom_line() + #geom_point() + 
  facet_grid(nsample ~ s) + ylab("TPR") + 
  theme(legend.position = "top") + xlab("FPR")

pdf("C:/Users/meichen/OneDrive - University of North Carolina at Chapel Hill/994/medulloblastoma/data/Benchmark_existingGRN_GENIE3_SUMMARY_SharedLinkDetection_v2.pdf", height = 8, width = 8)
ggplot(allt0, aes(x = X2, y = X1, color = method)) + geom_line() + #geom_point() + 
  facet_grid(nsample ~ s) + ylab("TPR") + 
  theme(legend.position = "top") + xlab("FPR")
dev.off()
