### ----------------------------------------------------------------------------
### CLIMATE CHANGE, ARIDITY, AND INTERNAL MIGRATION 
### ----------------------------------------------------------------------------

### 
### EXTENDES MODELS: HETEROGENEITY BY WEALTH AND AGRICULTURAL DEPENDENCE COUNTRY LEVEL  ------ 
###  

rm(list=ls())

##
## PACKAGES --------------------------------------------------------------------
## 

library(tidyverse)
library(fixest)
library(ggpubr)

citation("tidyverse")
citation("fixest")
citation("ggpubr")

##
## LOAD DATA -------------------------------------------------------------------
## 

load(file="full migration data.RData")

##
## EXTENDED MODELS: DIFFERENCES IN MIGRATION IMPACTS BY COUNTRY GDP PC ---------
##

m1a <-  fepois(flow_out_rate_annual ~ 
                 orig_ai_mean10_stan1*log(country_gdp_pc)+
                 dest_ai_mean10_stan1+
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+as.factor(mig_interval),
               fixef = "orig_dest_dyad",
               data=d) 
summary(m1a, se = "cluster")


m2a <-  fepois(flow_out_rate_annual ~ 
                 orig_pdsi_mean10_stan1*log(country_gdp_pc)+
                 dest_pdsi_mean10_stan1+
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+as.factor(mig_interval),
               fixef = "orig_dest_dyad",
               data=d) 
summary(m2a, se = "cluster")

m3a <-  fepois(flow_out_rate_annual ~ 
                 orig_spei03_mean10_stan1*log(country_gdp_pc)+
                 dest_spei03_mean10_stan1+
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+as.factor(mig_interval),
               fixef = "orig_dest_dyad",
               data=d) 
summary(m3a, se = "cluster")


m4a <-  fepois(flow_out_rate_annual ~ 
                 orig_spei12_mean10_stan1*log(country_gdp_pc)+
                 dest_spei12_mean10_stan1+
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+as.factor(mig_interval),
               fixef = "orig_dest_dyad",
               data=d) 
summary(m4a, se = "cluster")


models1 <- etable(m1a, m2a,m3a,m4a,
                  se="cluster",
                  order = c("orig_ai",  "spei", "orig_ndvi","dist", "pop", "mig_interval", "year" ),
                  signif.code = c(`***` = 0.01, `**` = 0.05, `*` = 0.1))
models1

write.csv(models1, "table_extended_heterogeneity by gdp country level.csv")


##
## EXTENDED MODELS: DIFFERENCES IN MIGRATION IMPACTS BY AGRICULTURAL EMPLOYMENT -----
##

m1b <-  fepois(flow_out_rate_annual ~ 
                 orig_ai_mean10_stan1*log(country_gdp_pc)+
                 orig_ai_mean10_stan1*log(country_agr_landshare)+
                 dest_ai_mean10_stan1+
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+as.factor(mig_interval),
               fixef = "orig_dest_dyad",
               data=d) 
summary(m1b, se = "cluster")

m2b <-  fepois(flow_out_rate_annual ~ 
                 orig_pdsi_mean10_stan1*log(country_gdp_pc)+
                 orig_pdsi_mean10_stan1*log(country_agr_landshare)+
                 dest_pdsi_mean10_stan1+
               worldregion*as.factor(year_cat10)+
                 log(dist)+contig+as.factor(mig_interval),
               fixef = "orig_dest_dyad",
               data=d) 
summary(m2b, se = "cluster")


m3b <-  fepois(flow_out_rate_annual ~ 
                 orig_spei03_mean10_stan1*log(country_gdp_pc)+
                 orig_spei03_mean10_stan1*log(country_agr_landshare)+
                 dest_spei03_mean10_stan1+
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+as.factor(mig_interval),
               fixef = "orig_dest_dyad",
               data=d) 
summary(m3b, se = "cluster")


m4b <-  fepois(flow_out_rate_annual ~ 
                 orig_spei12_mean10_stan1*log(country_gdp_pc)+
                 orig_spei12_mean10_stan1*log(country_agr_landshare)+
                 dest_spei12_mean10_stan1+
                 worldregion*as.factor(year_cat10)+
                 log(dist)+contig+as.factor(mig_interval),
               fixef = "orig_dest_dyad",
               data=d) 
summary(m4b, se = "cluster")


models2 <- etable(m1b, m2b,m3b,m4b,
                  se="cluster",
                  order = c("orig_ai",  "spei", "orig_ndvi","dist", "pop", "mig_interval", "year" ),
                  signif.code = c(`***` = 0.01, `**` = 0.05, `*` = 0.1))
models2

write.csv(models2, "table_extended_heterogeneity by gdp and agricultural dependency country level.csv")


##
## TRANSLATE EVERYTHING INTO FIGURES -------------------------------------------
## 


#> displaying models and extracting key parameters for interaction functions 
models1

fun.1 <- function(x)  -0.0421 +0.0171*log(x)
fun.2 <- function(x) -0.1246 +0.0214*log(x)
fun.3 <- function(x) -0.1153 +0.0221*log(x)
fun.4 <- function(x) -0.0008 +0.0106*log(x)

summary(d$country_gdp_pc)

g1 <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) +
  stat_function(fun=fun.1,  linewidth=1, linetype="solid", aes(color="Aridity Index"))+
  stat_function(fun=fun.2, linewidth=1, linetype="solid", aes(color="SPEI03")) +
  stat_function(fun=fun.3, linewidth=1, linetype="solid", aes(color="SPEI12")) +
  stat_function(fun=fun.4, linewidth=1, linetype="solid", aes(color="PDSI")) +
  xlim(0,50000)+
  theme_bw()+
  xlab("GDP per capita in country")+
  ylab("Migration impact")+
  scale_y_continuous(labels = scales::percent, limits=c(0,0.2))+
  scale_color_brewer(name="Climate measure", palette="Paired")+
  ggtitle("Income level")+
  theme(plot.title =  element_text(hjust=0.5),
        axis.title = element_text(size=12),
        axis.text = element_text(size=10),
        legend.text = element_text(size=11),
        legend.title = element_text(size=12),
        legend.background = element_blank(),
        legend.box.background = element_rect(fill="#daebdd",colour = "Grey"))
g1


#> displaying models and extracting key parameters for interaction functions 
models2
summary(d$country_gdp_pc)
summary(d$country_agr_landshare)
# Mean GDP: 15393.9  

fun.1 <- function(x) 0.0648 + 0.0118 *log(8438.43 ) +0.0644*log(x)
fun.2 <- function(x) -0.0311  + 0.0170*log(8438.43 ) +0.0602*log(x)
fun.3 <- function(x) -0.1254 + 0.0231*log(8438.43 ) +0.0034  *log(x)
fun.4 <- function(x) 0.0802  + 0.0067 *log(8438.43 ) +0.0500 *log(x)

g2 <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) +
  stat_function(fun=fun.1,  linewidth=1, linetype="solid", aes(color="Aridity Index"))+
  stat_function(fun=fun.2, linewidth=1, linetype="solid", aes(color="SPEI03")) +
  stat_function(fun=fun.3, linewidth=1, linetype="solid", aes(color="SPEI12")) +
  stat_function(fun=fun.4, linewidth=1, linetype="solid", aes(color="PDSI")) +
  theme_bw()+
  xlab("Agricultural employment in country")+
  ylab("Migration impact")+
  scale_y_continuous(labels = scales::percent, limits=c(0,0.2))+
  scale_x_continuous(labels = scales::percent, limits=c(0.01,1))+
  scale_color_brewer(name="Climate measure", palette="Paired")+
  ggtitle("Agricultural dependency")+
  theme(plot.title = element_text(hjust=0.5),
        axis.title = element_text(size=12),
        axis.text = element_text(size=10),
        legend.text = element_text(size=11),
        legend.title = element_text(size=12),
        legend.background = element_blank(),
        legend.box.background = element_rect(fill="#daebdd",colour = "Grey"))
g2


#> combining panels in one figure
g12 <- ggarrange(g1,g2,ncol=2, nrow=1,
                  common.legend = TRUE, legend = "bottom",
                  labels=c("A", "B"),
                  font.label=list(size=18))
g12


#> save final figure
ggsave(g12, filename="figure_supplement_marginal effects by country background characteristics.png", width=8, height=4.0)









