rm(list = ls())  
library(ggvenn)
library(ggplot2)
library(VennDiagram)
load("D:/R data/AMI_time/07_Muzz.Rdata")
load("D:/R data/AMI_time/drug_gene.Rdata")
cl$cluster[cl$cluster == 1] 
cl$membership 
Cluster1 = names(cl$cluster[cl$cluster == 1])
Cluster3 = names(cl$cluster[cl$cluster == 3])
AMI_gene <- unique(c(Cluster1, Cluster3))
GeneCardlist <- read.csv("GeneCardsSTEMI.csv")
Drug_gene <- drug_gene
GeneCard <- GeneCardlist$Gene.Symbol

library(dplyr)
venn_dat <- bind_rows(
  tibble(STEMI = AMI_gene),
  tibble(GeneCard = GeneCard),
  tibble(`Radix Salviae` = Drug_gene))
venn_list <- as.list(venn_dat)              
venn_list <- purrr::map(venn_list, na.omit) 
venn_list <- purrr::map(venn_list, function(x){x[x!=""]}) 

ggvenn(
  data = venn_list,        
  columns = NULL,           
  show_elements = F,       
  label_sep = "\n",         
  show_percentage = F,      
  digits = 1,              
  fill_color = c("#E41A1C", "#1E90FF", "#FF8C00", "#80FF00"), 
  fill_alpha = 0.5,         
  stroke_color = "white",   
  stroke_alpha = 0.5,       
  stroke_size = 0.5,        
  stroke_linetype = "solid", 
  set_name_color = "black", 
  set_name_size = 5,        
  text_color = "black",     
  text_size = 4             
)
df_inter <- get.venn.partitions(venn_list)

for (i in 1:nrow(df_inter)) df_inter[i,'values'] <- paste(df_inter[[i,'..values..']], collapse = ', ')
df <- df_inter[1,5]
genes_of_interest <- unlist(df)
save(genes_of_interest,file="geneoutput.Rdata")
