rm(list = ls())  
library(ggvenn)
library(ggplot2)
library(VennDiagram)
load(file = "step_4_MEexpcombine.RData")
mito <- read.csv("mito_gene.csv")
fero <- read.csv("fero_gene.csv")
AMI_gene <- rownames(MEexp)
mito_gene <- mito$Symbol
Fero_gene <- fero$Symbol
library(dplyr)
venn_dat <- bind_rows(
  tibble(AMI = AMI_gene),
    tibble(MitoCarta = mito_gene)
)
#tibble(FerrDb = Fero_gene),
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
df <- df_inter[1,4]
genes_of_interest <- unlist(df)
MEexp[genes_of_interest,] -> ME54
save(ME54,group,file = "step_5ME54.RData")
