### ----------------------------------------------------------------------------
### CLIMATE CHANGE, ARIDITY, AND INTERNAL MIGRATION 
### ----------------------------------------------------------------------------

### 
### EXTENDES MODELS: hETEROGENEITY BY WEALTH AND AGRICULTURAL DEPENDENY -------- 
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

#> calculating mean and standard deviation for regional characteristics

# 1 origin region
d.grouped.orig <- d %>% 
  group_by(cntry_name, alpha3, year, orig) %>% 
  summarize(orig_gdp_per_capita = unique(orig_gdp_per_capita),
         orig_indgen_afaf = unique(orig_indgen_afaf),
         orig_urban = unique(orig_urban)) %>% 
  group_by(cntry_name, alpha3, year) %>% 
  mutate(orig_gdp_per_capita_mean = mean(orig_gdp_per_capita, na.rm=T),
         orig_indgen_afaf_mean  = mean(orig_indgen_afaf, na.rm=T),
         orig_urban_mean  = mean(orig_urban, na.rm=T),
         orig_gdp_per_capita_sd = sd(orig_gdp_per_capita, na.rm=T),
         orig_indgen_afaf_sd  = sd(orig_indgen_afaf, na.rm=T),
         orig_urban_sd  = sd(orig_urban, na.rm=T))
 
# 2 destination region
d.grouped.dest <- d %>% 
  group_by(cntry_name, alpha3, year, dest) %>% 
  summarize(dest_gdp_per_capita = unique(dest_gdp_per_capita),
            dest_indgen_afaf = unique(dest_indgen_afaf),
            dest_urban = unique(dest_urban)) %>% 
  group_by(cntry_name, alpha3, year) %>% 
  mutate(dest_gdp_per_capita_mean = mean(dest_gdp_per_capita, na.rm=T),
         dest_indgen_afaf_mean  = mean(dest_indgen_afaf, na.rm=T),
         dest_urban_mean  = mean(dest_urban, na.rm=T),
         dest_gdp_per_capita_sd = sd(dest_gdp_per_capita, na.rm=T),
         dest_indgen_afaf_sd  = sd(dest_indgen_afaf, na.rm=T),
         dest_urban_sd  = sd(dest_urban, na.rm=T))

#> binding everything together
d <- d %>% 
  left_join(d.grouped.orig) %>% 
  left_join(d.grouped.dest)
  
#> calculating standardized deviations from country-specifc means
d <- d %>% 
  mutate(orig_gdp_per_capita_stan = (orig_gdp_per_capita-orig_gdp_per_capita_mean)/orig_gdp_per_capita_sd,
         orig_indgen_afaf_stan = (orig_indgen_afaf-orig_indgen_afaf_mean)/orig_indgen_afaf_sd,
         orig_urban_stan = (orig_urban-orig_urban_mean)/orig_urban_sd,
         dest_gdp_per_capita_stan = (dest_gdp_per_capita-dest_gdp_per_capita_mean)/dest_gdp_per_capita_sd,
         dest_indgen_afaf_stan = (dest_indgen_afaf-dest_indgen_afaf_mean)/dest_indgen_afaf_sd,
         dest_urban_stan = (dest_urban-dest_urban_mean)/dest_urban_sd)

summary(d$orig_gdp_per_capita)
summary(d$orig_gdp_per_capita_stan)
summary(d$dest_gdp_per_capita_stan)

summary(d$orig_indgen_afaf)
summary(d$orig_indgen_afaf_stan)
summary(d$dest_indgen_afaf_stan)

summary(d$orig_urban)
summary(d$orig_urban_stan)
summary(d$dest_urban_stan)


##
## EXTENDED MODELS: DIFFERENCES IN MIGRATION IMPACTS BY GDP PC -----------------
##

m1a <-  fepois(flow_out_rate_annual ~ 
                 orig_ai_mean10_stan1*log(orig_gdp_per_capita)+
                 dest_ai_mean10_stan1+
                 worldregion*as.factor(year_cat10)+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m1a, se = "cluster")

m2a <-  fepois(flow_out_rate_annual ~ 
                 orig_pdsi_mean10_stan1*log(orig_gdp_per_capita) +
                 dest_pdsi_mean10_stan1+
                 worldregion*as.factor(year_cat10)+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m2a, se = "cluster")

m3a <-  fepois(flow_out_rate_annual ~ 
                 orig_spei03_mean10_stan1*log(orig_gdp_per_capita) +
                 dest_spei03_mean10_stan1+
                 worldregion*as.factor(year_cat10)+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m3a, se = "cluster")


m4a <-  fepois(flow_out_rate_annual ~ 
                 orig_spei12_mean10_stan1*log(orig_gdp_per_capita) +
                 dest_spei12_mean10_stan1 +
                 worldregion*as.factor(year_cat10)+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m4a, se = "cluster")



models1 <- etable(m1a, m2a,m3a,m4a,
                  se="cluster",
                  order = c("orig_ai",  "orig_pdsi" , "orig_spei", "dest", "gdp", "dist", "contig", "mig_interval", "year" ),
                  signif.code = c(`***` = 0.01, `**` = 0.05, `*` = 0.1))
models1

write.csv(models1, file="table_extended models by regional background characteristics 1.csv")

##
## EXTENDED MODELS: DIFFERENCES IN MIGRATION IMPACTS BY AGRICULTURAL EMPLOYMENT ------
##

m1b <-  fepois(flow_out_rate_annual ~ 
                 orig_ai_mean10_stan1*log(orig_gdp_per_capita)+
                 orig_ai_mean10_stan1*log(orig_indgen_afaf)+
                 dest_ai_mean10_stan1+
                 worldregion*as.factor(year_cat10)+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m1b, se = "cluster")

m2b <-  fepois(flow_out_rate_annual ~ 
                 orig_pdsi_mean10_stan1*log(orig_gdp_per_capita) +
                 orig_pdsi_mean10_stan1*log(orig_indgen_afaf) +
                 dest_pdsi_mean10_stan1+
               worldregion*as.factor(year_cat10)+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m2b, se = "cluster")

m3b <-  fepois(flow_out_rate_annual ~ 
                 orig_spei03_mean10_stan1*log(orig_gdp_per_capita) +
                 orig_spei03_mean10_stan1*log(orig_indgen_afaf)+
                 dest_spei03_mean10_stan1+
                 worldregion*as.factor(year_cat10)+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m3b, se = "cluster")


m4b<-  fepois(flow_out_rate_annual ~ 
                orig_spei12_mean10_stan1*log(orig_gdp_per_capita) +
                orig_spei12_mean10_stan1*log(orig_indgen_afaf) +
                dest_spei12_mean10_stan1+
                worldregion*as.factor(year_cat10)+
                as.factor(mig_interval),
              fixef = c("orig_dest_dyad"),
              data=d) 
summary(m4b, se = "cluster")


models2 <- etable(m1b, m2b,m3b,m4b,
                  se="cluster",
                  order = c("orig_ai",  "orig_pdsi" , "orig_spei", "dest", "gdp", "dist", "contig", "mig_interval", "year"),
                  signif.code = c(`***` = 0.01, `**` = 0.05, `*` = 0.1))
models2

write.csv(models2, file="table_extended models by regional background characteristics 2.csv")


##
## EXTENDED MODELS: DIFFERENCES IN MIGRATION IMPACTS BY URBAN POPULATION -------
##

m1c <-  fepois(flow_out_rate_annual ~ 
                 orig_ai_mean10_stan1*log(orig_gdp_per_capita)+
                 orig_ai_mean10_stan1*log(orig_urban)+
                 dest_ai_mean10_stan1+
                 worldregion*as.factor(year_cat10)+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m1c, se = "cluster")

m2c <-  fepois(flow_out_rate_annual ~ 
                 orig_pdsi_mean10_stan1*log(orig_gdp_per_capita) +
                 orig_pdsi_mean10_stan1*log(orig_urban) +
                 dest_pdsi_mean10_stan1+
                 worldregion*as.factor(year_cat10)+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m2c, se = "cluster")

m3c <-  fepois(flow_out_rate_annual ~ 
                 orig_spei03_mean10_stan1*log(orig_gdp_per_capita) +
                 orig_spei03_mean10_stan1*log(orig_urban)+
                 dest_spei03_mean10_stan1+
                 worldregion*as.factor(year_cat10)+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m3c, se = "cluster")


m4c<-  fepois(flow_out_rate_annual ~ 
                 orig_spei12_mean10_stan1*log(orig_gdp_per_capita) +
                 orig_spei12_mean10_stan1*log(orig_urban) +
                dest_spei12_mean10_stan1+
                worldregion*as.factor(year_cat10)+
                as.factor(mig_interval),
              fixef = c("orig_dest_dyad"),
               data=d) 
summary(m4c, se = "cluster")


models3 <- etable(m1c, m2c,m3c,m4c,
                 se="cluster",
                 order = c("orig_ai",  "orig_pdsi" , "orig_spei", "dest", "gdp", "dist", "contig", "mig_interval", "year"),
                 signif.code = c(`***` = 0.01, `**` = 0.05, `*` = 0.1))
models3

write.csv(models3, file="table_extended models by regional background characteristics 3.csv")


##
## EXTENDED MODELS: STANDARDIZED DEVIATIONS IN GDP PC FOR ORIGIN AND DESTINATION -------
##


m1d <-  fepois(flow_out_rate_annual ~ 
                 orig_ai_mean10_stan1*orig_gdp_per_capita_stan+
                 orig_ai_mean10_stan1*dest_gdp_per_capita_stan+
                 dest_ai_mean10_stan1+
                 worldregion*as.factor(year_cat10)+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m1d, se = "cluster")

m2d <-  fepois(flow_out_rate_annual ~ 
                 orig_pdsi_mean10_stan1*orig_gdp_per_capita_stan+
                 orig_pdsi_mean10_stan1*dest_gdp_per_capita_stan+
                 dest_pdsi_mean10_stan1+
                 worldregion*as.factor(year_cat10)+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m2d, se = "cluster")

m3d <-  fepois(flow_out_rate_annual ~ 
                 orig_spei03_mean10_stan1*orig_gdp_per_capita_stan+
                 orig_spei03_mean10_stan1*dest_gdp_per_capita_stan+
                 dest_spei03_mean10_stan1+
                 worldregion*as.factor(year_cat10)+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m3d, se = "cluster")


m4d <-  fepois(flow_out_rate_annual ~ 
                 orig_spei12_mean10_stan1*orig_gdp_per_capita_stan+
                 orig_spei12_mean10_stan1*dest_gdp_per_capita_stan+
                 dest_spei12_mean10_stan1+
                 worldregion*as.factor(year_cat10)+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m4d, se = "cluster")

models <- etable(m1d, m2d,m3d,m4d,
                 se="cluster",
                 coefstat="confint",
                 ci = 0.9,
                 keep=c("orig_ai_mean10_stan1","orig_spei12_mean10_stan1",
                        "orig_spei03_mean10_stan1", "orig_pdsi_mean10_stan1"),
                 signif.code = c(`***` = 0.01, `**` = 0.05, `*` = 0.1))
models

models4 <- etable(m1d, m2d,m3d,m4d,
                  se="cluster",
                  order = c("orig_ai",  "orig_pdsi" , "orig_spei", "dest", "gdp", "dist", "contig", "mig_interval", "year"),
                  signif.code = c(`***` = 0.01, `**` = 0.05, `*` = 0.1))
models4

write.csv(models4, file="table_extended models by regional background characteristics 4.csv")


##
## EXTENDED MODELS: STANDARDIZED DEVIATIONS IN AGRICULTURAL EMPLOYMENT FOR ORIGIN AND DESTINATION -------
##


m1e <-  fepois(flow_out_rate_annual ~ 
                 orig_ai_mean10_stan1*orig_indgen_afaf_stan+
                 orig_ai_mean10_stan1*dest_indgen_afaf_stan+
                 dest_ai_mean10_stan1+
                 worldregion*as.factor(year_cat10)+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m1e, se = "cluster")

m2e <-  fepois(flow_out_rate_annual ~ 
                 orig_pdsi_mean10_stan1*orig_indgen_afaf_stan+
                 orig_pdsi_mean10_stan1*dest_indgen_afaf_stan+
                 dest_pdsi_mean10_stan1+
                 worldregion*as.factor(year_cat10)+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m2e, se = "cluster")

m3e <-  fepois(flow_out_rate_annual ~ 
                 orig_spei03_mean10_stan1*orig_indgen_afaf_stan+
                 orig_spei03_mean10_stan1*dest_indgen_afaf_stan+
                 dest_spei03_mean10_stan1+
                 worldregion*as.factor(year_cat10)+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m3e, se = "cluster")


m4e <-  fepois(flow_out_rate_annual ~ 
                 orig_spei12_mean10_stan1*orig_indgen_afaf_stan+
                 orig_spei12_mean10_stan1*dest_indgen_afaf_stan+
                 dest_spei12_mean10_stan1+
                 worldregion*as.factor(year_cat10)+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m4e, se = "cluster")



models5 <- etable(m1e, m2e,m3e,m4e,
                  se="cluster",
                  order = c("orig_ai",  "orig_pdsi" , "orig_spei", "dest", "gdp", "dist", "contig", "mig_interval", "year" ),
                  signif.code = c(`***` = 0.01, `**` = 0.05, `*` = 0.1))
models5

write.csv(models5, file="table_extended models by regional background characteristics 5.csv")


##
## EXTENDED MODELS: STANDARDIZED DEVIATIONS IN URBAN POPULATION FOR ORIGIN AND DESTINATION -------
##

m1f <-  fepois(flow_out_rate_annual ~
                 orig_ai_mean10_stan1*orig_urban_stan+
                 orig_ai_mean10_stan1*dest_urban_stan+
                 dest_ai_mean10_stan1+
                 worldregion*as.factor(year_cat10)+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m1f, se = "cluster")

m2f <-  fepois(flow_out_rate_annual  ~
                 orig_pdsi_mean10_stan1*orig_urban_stan+
                 orig_pdsi_mean10_stan1*dest_urban_stan+
                 dest_pdsi_mean10_stan1+
                 worldregion*as.factor(year_cat10)+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m2f, se = "cluster")

m3f <-  fepois(flow_out_rate_annual ~
                 orig_spei03_mean10_stan1*orig_urban_stan+
                 orig_spei03_mean10_stan1*dest_urban_stan+
                 dest_spei03_mean10_stan1+
                 worldregion*as.factor(year_cat10)+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m3f, se = "cluster")


m4f <-  fepois(flow_out_rate_annual ~
                 orig_spei12_mean10_stan1*orig_urban_stan+
                 orig_spei12_mean10_stan1*dest_urban_stan+
                 dest_spei12_mean10_stan1+
                 worldregion*as.factor(year_cat10)+
                 as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m4f, se = "cluster")


models6 <- etable(m1f, m2f,m3f,m4f,
                  se="cluster",
                  order = c("orig_ai",  "orig_pdsi" , "orig_spei", "dest", "gdp", "dist", "contig", "mig_interval", "year"),
                  signif.code = c(`***` = 0.01, `**` = 0.05, `*` = 0.1))
models6

write.csv(models6, file="table_extended models by regional background characteristics 6.csv")



##
## TRANSLATE EVERYTHING INTO FIGURES -------------------------------------------
## 

#> define common theme
theme_defined <- theme_set(theme_bw()+
                             theme(plot.title=element_text(hjust=0.5),
                                   legend.text = element_text(size=12),
                                   axis.text=element_text(size=10.7),
                                   axis.title=element_text(size=12),
                                   legend.background = element_blank(),
                                   legend.box.background = element_rect(fill="#daebdd",colour = "Grey"),
                                   plot.background = element_blank()))

#> displaying models and extracting key parameters for interaction functions 
models1

fun.1 <- function(x) -0.4424 +0.0525*log(x)
fun.2 <- function(x) -0.3518 +0.0415*log(x)
fun.3 <- function(x) -0.5270 +0.0592*log(x)
fun.4 <- function(x) -0.3825 +0.0456*log(x)

summary(d$orig_gdp_per_capita)

g1 <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) +
  stat_function(fun=fun.1,  linewidth=1, linetype="solid", aes(color="Aridity Index"), alpha=0.9)+
  stat_function(fun=fun.2, linewidth=1, linetype="solid", aes(color="PDSI"), alpha=0.9) +
  stat_function(fun=fun.3, linewidth=1, linetype="solid", aes(color="SPEI03"), alpha=0.9) +
  stat_function(fun=fun.4, linewidth=1, linetype="solid", aes(color="SPEI12"), alpha=0.9) +
  xlim(0,100000)+
  xlab("GDP per capita in origin")+
  ylab("Migration impact")+
  scale_y_continuous(labels = scales::percent, limits=c(0,0.20))+
  scale_color_brewer(name="Climate measure", palette="Paired")+
  ggtitle("Income level")+
  theme_defined
g1


#> displaying models and extracting key parameters for interaction functions 
models2

summary(d$orig_gdp_per_capita)
# Mean GDP: 15393.9  

fun.1 <- function(x) -0.7809 +0.1034*log(15393.9) +0.0575*log(x)
fun.2 <- function(x) -0.5423 + 0.0765*log(15393.9) +0.0528*log(x)
fun.3 <- function(x) -0.8712 + 0.1151*log(15393.9) +0.0713*log(x)
fun.4 <- function(x) -0.7191 + 0.1012*log(15393.9) +0.0735*log(x)

summary(d$orig_indgen_afaf)

g2 <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) +
  stat_function(fun=fun.1,  linewidth=1, linetype="solid", aes(color="Aridity Index"), alpha=0.9)+
  stat_function(fun=fun.2, linewidth=1, linetype="solid", aes(color="PDSI"), alpha=0.9) +
  stat_function(fun=fun.3, linewidth=1, linetype="solid", aes(color="SPEI03"), alpha=0.9) +
  stat_function(fun=fun.4, linewidth=1, linetype="solid", aes(color="SPEI12"), alpha=0.9) +
  xlab("Agricultural employment share in origin")+
  ylab("Migration impact")+
  scale_y_continuous(labels = scales::percent, limits=c(0,0.3))+
  scale_x_continuous(labels = scales::percent, limits=c(0,1))+
  scale_color_brewer(name="Climate measure", palette="Paired")+
  ggtitle("Agricultural dependency")+
  theme_defined
g2


#> displaying models and extracting key parameters for interaction functions 
models3

summary(d$orig_gdp_per_capita)
# Mean GDP: 15393.9  
table(d$grp_un_develop)

fun.1 <- function(x)  -1.404 +0.1593*log(15393.9)  -0.0661*log(x)
fun.2 <- function(x)  -1.457  + 0.1660*log(15393.9) -0.0762*log(x)
fun.3 <- function(x) -1.380 + 0.1542*log(15393.9) -0.0613*log(x)
fun.4 <- function(x)  -1.467 + 0.1670*log(15393.9) -0.0661*log(x)


g3 <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) +
  stat_function(fun=fun.1,  linewidth=1, linetype="solid", aes(color="Aridity Index"), alpha=0.9)+
  stat_function(fun=fun.2, linewidth=1, linetype="solid", aes(color="PDSI"), alpha=0.9) +
  stat_function(fun=fun.3, linewidth=1, linetype="solid", aes(color="SPEI03"), alpha=0.9) +
  stat_function(fun=fun.4, linewidth=1, linetype="solid", aes(color="SPEI12"), alpha=0.9) +
  xlab("Urban population share in origin")+
  ylab("Migration impact")+
  scale_y_continuous(labels = scales::percent, limits=c(0,0.85))+
  scale_x_continuous(labels = scales::percent, limits=c(0.01,1))+
  scale_color_brewer(name="Climate measure", palette="Paired")+
  ggtitle("Urban population")+
  theme_defined

g3


#> displaying models and extracting key parameters for interaction functions 
models4

fun.1 <- function(x) 0.0459 -0.0228*x
fun.2 <- function(x) 0.0306  -0.0200*x
fun.3 <- function(x) 0.0069   -0.0259*x
fun.4 <- function(x) 0.0323 -0.0207*x

fun.5 <- function(x) 0.0459 +0.0120  *x
fun.6 <- function(x) 0.0306   +0.0047 *x
fun.7 <- function(x) 0.0069   +0.0509*x
fun.8 <- function(x) 0.0323 +0.0302*x

g4 <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) +
  stat_function(fun=fun.1,  linewidth=1, aes(color="Aridity Index", linetype="dashed"), alpha=0.8)+
  stat_function(fun=fun.2, linewidth=1, aes(color="PDSI", linetype="dashed"), alpha=0.8) +
  stat_function(fun=fun.3, linewidth=1, aes(color="SPEI03", linetype="dashed"), alpha=0.8) +
  stat_function(fun=fun.4, linewidth=1, aes(color="SPEI12", linetype="dashed"), alpha=0.8) +
  
  stat_function(fun=fun.5,  linewidth=1, aes(color="Aridity Index", linetype="solid"), alpha=0.8)+
  stat_function(fun=fun.6, linewidth=1, aes(color="PDSI", linetype="solid"), alpha=0.8) +
  stat_function(fun=fun.7, linewidth=1, aes(color="SPEI03", linetype="solid"), alpha=0.8) +
  stat_function(fun=fun.8, linewidth=1, aes(color="SPEI12", linetype="solid"), alpha=0.8) +
  xlab("Deviation in GDP per capita")+
  ylab("Migration impact")+
  scale_y_continuous(labels = scales::percent, limits=c(-0.05,.2))+
  scale_x_continuous(limits=c(-2.5,2.5, breaks=seq(-2.5,2.5,0.5), labels=seq(-2.5,2.5,0.5)))+
  scale_color_brewer(name="Climate measure", palette="Paired")+
  scale_linetype_manual(name="Measure for origin
or destination", values=c(1,2), labels=c( "Origin", "Destination"))+
  ggtitle("Income level")+
  theme_defined

g4


#> displaying models and extracting key parameters for interaction functions 
models5

fun.1 <- function(x) 0.0828 +0.0169  *x
fun.2 <- function(x) 0.0407 +0.0183 *x
fun.3 <- function(x) 0.0608 +0.0531*x
fun.4 <- function(x) 0.0768 +0.0399 *x

fun.5 <- function(x) 0.0828 +0.0002    *x
fun.6 <- function(x) 0.0407 +0.0009   *x
fun.7 <- function(x) 0.0608 -0.0019     *x
fun.8 <- function(x) 0.0768 -0.0015    *x


g5 <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) +
  stat_function(fun=fun.1,  linewidth=1, aes(color="Aridity Index", linetype="dashed"), alpha=0.8)+
  stat_function(fun=fun.2, linewidth=1, aes(color="PDSI", linetype="dashed"), alpha=0.8) +
  stat_function(fun=fun.3, linewidth=1, aes(color="SPEI03", linetype="dashed"), alpha=0.8) +
  stat_function(fun=fun.4, linewidth=1, aes(color="SPEI12", linetype="dashed"), alpha=0.8) +
  
  stat_function(fun=fun.5,  linewidth=1, aes(color="Aridity Index", linetype="solid"), alpha=0.8)+
  stat_function(fun=fun.6, linewidth=1, aes(color="PDSI", linetype="solid"), alpha=0.8) +
  stat_function(fun=fun.7, linewidth=1, aes(color="SPEI03", linetype="solid"), alpha=0.8) +
  stat_function(fun=fun.8, linewidth=1, aes(color="SPEI12", linetype="solid"), alpha=0.8) +
  xlab("Deviation in agricultural employment")+
  ylab("Migration impact")+
  scale_y_continuous(labels = scales::percent, limits=c(-0.05,0.20))+
  scale_x_continuous(limits=c(-2.5,2.5, breaks=seq(-2.5,2.5,0.5), labels=seq(-2.5,2.5,0.5)))+
  scale_color_brewer(name="Climate measure", palette="Paired")+
  scale_linetype_manual(name="Measure for origin
or destination", values=c(1,2), labels=c( "Origin", "Destination"))+
  ggtitle("Agricultural dependency")+
  theme_defined

g5


#> displaying models and extracting key parameters for interaction functions 
models6

fun.1 <- function(x) 0.0733  -0.0272*x
fun.2 <- function(x) 0.0387    -0.0323 *x
fun.3 <- function(x) 0.0381     -0.0326 *x
fun.4 <- function(x)  0.0717   -0.0386*x

fun.5 <- function(x) 0.0733 +0.0282 *x
fun.6 <- function(x) 0.0387    +0.0118  *x
fun.7 <- function(x) 0.0381     +0.0387 *x
fun.8 <- function(x)  0.0717   +0.0310  *x


g6 <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) +
  stat_function(fun=fun.1,  linewidth=1, aes(color="Aridity Index", linetype="dashed"), alpha=0.8)+
  stat_function(fun=fun.2, linewidth=1, aes(color="PDSI", linetype="dashed"), alpha=0.8) +
  stat_function(fun=fun.3, linewidth=1, aes(color="SPEI03", linetype="dashed"), alpha=0.8) +
  stat_function(fun=fun.4, linewidth=1, aes(color="SPEI12", linetype="dashed"), alpha=0.8) +
  
  stat_function(fun=fun.5,  linewidth=1, aes(color="Aridity Index", linetype="solid"), alpha=0.8)+
  stat_function(fun=fun.6, linewidth=1, aes(color="PDSI", linetype="solid"), alpha=0.8) +
  stat_function(fun=fun.7, linewidth=1, aes(color="SPEI03", linetype="solid"), alpha=0.8) +
  stat_function(fun=fun.8, linewidth=1, aes(color="SPEI12", linetype="solid"), alpha=0.8) +
  xlab("Deviation in urban population share")+
  ylab("Migration impact")+
  scale_y_continuous(labels = scales::percent, limits=c(-0.05,0.20))+
  scale_x_continuous(limits=c(-2.5,2.5, breaks=seq(-2.5,2.5,0.5), labels=seq(-2.5,2.5,0.5)))+
  scale_color_brewer(name="Climate measure", palette="Paired")+
  scale_linetype_manual(name="Measure for origin
or destination", values=c(1,2), labels=c( "Origin", "Destination"))+
  ggtitle("Urban population")+
  theme_defined
  
g6

#> combining panels in one figure
g123 <- ggarrange(g1, g2, g3, nrow=1, ncol=3, legend="right", common.legend = TRUE)
g123

#> combining panels in one figure
g456 <- ggarrange(g4, g5, g6, nrow=1, ncol=3, legend="right", common.legend = TRUE)
g456

#> combining panels in one figure
g16 <- ggarrange(g123, g456, labels=c("A", "B"), ncol=1, nrow=2,
                 font.label=list(size=18))

g16

#> save final plots
ggsave(g16, filename="figure 3.png", width=12.2, height=7)

ggsave(g16, filename="figure 3.pdf", width=12.2, height=7)

