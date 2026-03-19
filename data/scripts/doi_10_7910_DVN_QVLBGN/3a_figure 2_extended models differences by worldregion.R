### ----------------------------------------------------------------------------
### CLIMATE CHANGE, ARIDITY, AND INTERNAL MIGRATION 
### ----------------------------------------------------------------------------


rm(list=ls())

### 
### EXTENDES MODELS: TESTING FOR HETEROGENEITY BY DIFFERENT COUNTRIES ---------- 
###  


##
## PACKAGES AND DATA -----------------------------------------------------------
## 

library(tidyverse)
library(fixest)
library(ggpubr)

citation("tidyverse")
citation("fixest")
citation("ggpubr")

load(file="full migration data.RData")


##
## EXTENDED MODELS: DIFFERENCES IN ARIDITY IMPACTS BY REGIONS -----------------------
##


m1a <-  fepois(flow_out_rate_annual ~ 
                 orig_ai_mean10_stan1:worldregion +
                 dest_ai_mean10_stan1+
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m1a, se = "cluster")

m2a <-  fepois(flow_out_rate_annual ~ 
                 orig_pdsi_mean10_stan1:worldregion +
                 dest_pdsi_mean10_stan1+
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m2a, se = "cluster")

m3a <-  fepois(flow_out_rate_annual ~ 
                 orig_spei03_mean10_stan1:worldregion +
                 dest_spei03_mean10_stan1+
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m3a, se = "cluster")


m4a <-  fepois(flow_out_rate_annual ~ 
                 orig_spei12_mean10_stan1:worldregion +
                 dest_spei12_mean10_stan1+
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m4a, se = "cluster")

models <- etable(m1a,
                 se="cluster",
                 coefstat="confint",
                 ci = 0.9,
                 keep=c("orig_ai_mean10_stan1","orig_spei12_mean10_stan1",
                          "orig_spei03_mean10_stan1", "orig_pdsi_mean10_stan1"),
                 signif.code = c(`***` = 0.01, `**` = 0.05, `*` = 0.1))
models

models <- etable(m1a, m2a,m3a,m4a,
                 se="cluster",
                 ci = 0.9,
                 signif.code = c(`***` = 0.01, `**` = 0.05, `*` = 0.1),
                 coefstat = "se")
models
write.csv(models, "table_extended models_heterogeneity by regions.csv")


###
### PREPARE COEFFICIENT PLOT BY DIFFERENT REGIONS & ARIDITY ZONES ----------------------------
### 

m.sum <- summary(m1a, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "region")
m.coef <- m.coef[grep("orig_ai_mean10_stan1", m.coef$region), ]
m.coef$region <- gsub("orig_ai_mean10_stan1", "", m.coef$region)
m.coef$region <- gsub(":worldregion", "", m.coef$region)

m.coef1 <- m.coef %>% 
  rename("SE"="Std. Error") %>%  
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE) %>% 
  mutate("climatic" = "Aridity Index",
         "order" = 1)


m.sum <- summary(m2a, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "region")
m.coef <- m.coef[grep("orig_pdsi_mean10_stan1", m.coef$region), ]
m.coef$region <- gsub("orig_pdsi_mean10_stan1", "", m.coef$region)
m.coef$region <- gsub(":worldregion", "", m.coef$region)

m.coef2 <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE) %>% 
  mutate("climatic" = "PDSI",
         "order" = 2)


m.sum <- summary(m3a, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "region")
m.coef <- m.coef[grep("orig_spei03_mean10_stan1", m.coef$region), ]
m.coef$region <- gsub("orig_spei03_mean10_stan1", "", m.coef$region)
m.coef$region <- gsub(":worldregion", "", m.coef$region)

m.coef3 <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef =Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE) %>% 
  mutate("climatic" = "SPEI03",
         "order" = 3)

m.sum <- summary(m4a, se = "cluster")
m.coef <- as.data.frame(m.sum$coeftable)
m.coef <- rownames_to_column(m.coef, "region")
m.coef <- m.coef[grep("orig_spei12_mean10_stan1", m.coef$region), ]
m.coef$region <- gsub("orig_spei12_mean10_stan1", "", m.coef$region)
m.coef$region <- gsub(":worldregion", "", m.coef$region)

m.coef4 <- m.coef %>% 
  rename("SE"="Std. Error") %>% 
  mutate(coef = Estimate ,
         ci_up = coef + 1.64*SE ,
         ci_low= coef - 1.64*SE) %>% 
  mutate("climatic" = "SPEI48",
         "order" = 4)

m.coef <- rbind(m.coef1, m.coef2, m.coef3, m.coef4)

g1 <- m.coef %>% 
  mutate(order2=recode(region,
                      "Central America & Caribbean"=1L,
                      "North America"=2L,
                      "Northeastern Europe & Central Asia"=3L,
                      "East Asia & Pacific"=4L,
                      "South America"=5L,
                      "Africa & Middle East"=6L,
                      "South Asia"=7L,
                      "Southern Europe"=8L)) %>% 
  ggplot(aes(x=coef, y=fct_reorder(region, -order2)))+ 
  geom_vline(aes(xintercept=0), color="Black", linetype="dashed")+
  geom_pointrange(
    aes(xmin = ci_low, xmax = ci_up), # , colour = climatic, shape = climatic, fill = climatic
    position = position_dodge(0.5), 
    shape = 18,
    size = 0.6, 
    color="#1aa156"
  ) +
  facet_grid(cols = vars(fct_reorder(climatic, order)))+
  scale_x_continuous(labels = scales::percent)+
  coord_cartesian(xlim=c(-0.25, 0.7))+
  xlab("Migration impacts")+ylab("")+
  theme_bw() +
  ggtitle("")+
  theme(
    strip.text = element_text(face = "bold", size=11),
    strip.background = element_rect(fill = "#daebdd"),
    legend.position = "bottom",
    legend.text = element_text(size=12),
    axis.text=element_text(size=11),
    axis.title=element_text(size=12),
    legend.background = element_blank(),
    legend.box.background = element_rect(fill="#daebdd",colour = "Grey"))
g1

save(g1, file="figure_effect differences by worldregions.RData")

