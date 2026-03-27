rm(list = ls())  
load(file = "step1output.Rdata")
library(tinyarray)
find_anno(gpl_number1)
ids1 <- AnnoProbe::idmap('GPL570')
find_anno(gpl_number2)
ids2 <- AnnoProbe::idmap('GPL6884',type = "soft")

ids1$probe_id %in% rownames(exp1) %>% table
ids1[ids1$probe_id %in% rownames(exp1),] -> ids1
ids1 <- data.frame(probe_id = ids1$probe_id,symbol = ids1$symbol)
exp1[exp1 < 0] <- 0  
ids1 <- na.omit(ids1)
ids1[!ids1$symbol=="",] -> ids1
exp1[as.character(ids1$probe_id),]->exp1
apply(exp1,1,median)->ids1$median
ids1[order(ids1$symbol,ids1$median,decreasing = T),] ->ids1
ids1[!duplicated(ids1$symbol),]->ids1
exp1 <- exp1[as.character(ids1$probe_id),] 
rownames(exp1) <- ids1$symbol

table(group)
!str_detect(group,"other")->keep
exp[,keep]->exp
group[keep]->group

batch <- c(rep("A",times=length(group1)),rep("B",times=length(group2)),rep("C",times=length(group3)))
batch <- batch[keep]
limma::removeBatchEffect(exp,batch = batch,group=group) -> dat
exp=dat  
boxplot(exp)

save(exp,group,file = "step_2_output.RData")
