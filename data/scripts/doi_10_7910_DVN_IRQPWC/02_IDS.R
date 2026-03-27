rm(list = ls())  
load(file = "step1output.Rdata")
library(tinyarray)
find_anno(gpl_number1)
ids1 <- AnnoProbe::idmap('GPL6244')
ids2 <- ids1
ids1$probe_id %in% rownames(exp1) %>% table
ids1[ids1$probe_id %in% rownames(exp1),] -> ids1
ids1 <- data.frame(probe_id = ids1$probe_id,symbol = ids1$symbol)
ids1 <- na.omit(ids1)
ids1[!ids1$symbol=="",] -> ids1
exp1[as.character(ids1$probe_id),]->exp1
apply(exp1,1,median)->ids1$median
ids1[order(ids1$symbol,ids1$median,decreasing = T),] ->ids1
ids1[!duplicated(ids1$symbol),]->ids1
exp1 <- exp1[as.character(ids1$probe_id),] 
rownames(exp1) <- ids1$symbol
exp1[1:4,1:4]
ids2$probe_id %in% rownames(exp2) %>% table
ids2[ids2$probe_id %in% rownames(exp2),] -> ids2
ids2 <- data.frame(probe_id = ids2$probe_id,symbol = ids2$symbol)
ids2 <- na.omit(ids2)
ids2[!ids2$symbol=="",] -> ids2
exp2[as.character(ids2$probe_id),]->exp2
apply(exp2,1,median)->ids2$median
ids2[order(ids2$symbol,ids2$median,decreasing = T),] ->ids2
ids2[!duplicated(ids2$symbol),]->ids2
exp2 <- exp2[as.character(ids2$probe_id),] 
rownames(exp2) <- ids2$symbol
exp2[1:4,1:4]

table(c(rownames(exp1),rownames(exp2))) -> geneinexp
names(geneinexp[geneinexp==2])->geneinexp
cbind(exp1[geneinexp,],exp2[geneinexp,])->exp
dim(exp)
length(c(group1,group2))
c(group1,group2)->group
table(group)
!str_detect(group,"other")->keep
exp[,keep]->exp
group[keep]->group
table(group)
boxplot(exp)
batch <- c(rep("A",times=length(group1)),rep("B",times=length(group2)))
batch <- batch[keep]
limma::removeBatchEffect(exp,batch = batch,group=group) -> dat
exp=dat  
boxplot(exp)
save(exp,group,file = "step_2_output.RData")
