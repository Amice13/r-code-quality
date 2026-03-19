rm(list = ls())
load(file="step2output.Rdata")
library(IOBR)
tpm_eset[1:5,1:5]
library(WGCNA)
library(impute)
res <- find_outlier_samples(eset = tpm_eset,
                            project = "ACRG",
                            show_plot = T)
#character(0)
eset1 <- tpm_eset[, !colnames(exp)%in%res]

Group_new = group
im_cibersort <- deconvo_tme(eset = eset1,
                            method = "cibersort",
                            arrays = F,
                            perm = 1000
)

save(eset1,tpm_eset,im_cibersort,Group_new,file = "step4IBOR.Rdata")
library(tidyHeatmap)
library(tidyverse)
library(RColorBrewer)
im_cibersort = cbind(im_cibersort,Group = Group_new)

cols_to_remove <- apply(im_cibersort, 2, function(x) all(x == 0))
ed <- im_cibersort[, !cols_to_remove]
im_cibersort = im_cibersort_cleaned
cibersort_long <- im_cibersort %>% 
  select(Correlation_CIBERSORT, RMSE_CIBERSORT,ID,Group,everything()) %>% 
  pivot_longer(- c(1:4),names_to = "cell_type",values_to = "fraction") %>% 
  dplyr::mutate(cell_type = gsub("_CIBERSORT","",cell_type),
                cell_type = gsub("_"," ",cell_type))

p1 <- cibersort_long %>% 
  ggplot(aes(ID,fraction))+
  geom_bar(stat = "identity",position = "stack",aes(fill=cell_type))+
  labs(x=NULL)+
  scale_y_continuous(expand = c(0,0))+
  scale_fill_manual(values = palette4,name=NULL)+ 
  theme_bw()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom"
  )
p1

