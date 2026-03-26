rm(list=ls())
# Function to load packages
loadPkg=function(toLoad){
  for(lib in toLoad){
    if(! lib %in% installed.packages()[,1])
    {install.packages(lib, repos='http://cran.rstudio.com/')}
    suppressMessages( library(lib, character.only=TRUE))}}

# Load libraries
packs=c("readr", "readxl", "dplyr", "countrycode", "ggplot2","MASS","arm", "VGAM",
        "purrr", "texreg", "reshape2", "likert", "sp","cshapes","foreign","extrafont",
        "tidyr","ggthemes","viridis","ggridges","ggpubr", "scales",
        "haven", "readstata13")
loadPkg(packs)

source("Code/set_up.R")

load("Replication_data/afrov5_nigeria.RData")
load("Replication_data/afrov4_mali.RData")
load("Replication_data/df_pool.RData")

sort(unique(afrov5_nigeria$DATEINTR))
sort(unique(afrov4_mali$DATEINTR))

## look at other women related DV
############################################################################################
############################################################################################

paper_df7 <- afrov5_nigeria %>% 
  dplyr::mutate(Y_Q56C = ifelse(Q56C == 9, NA, Q56C),
                Y_Q56E = ifelse(Q56E == 9, NA, Q56E)) %>% 
  dplyr::filter(!is.na(Y_Q56C), !is.na(Y_Q56E)) %>% 
  dplyr::mutate(Y_Q56C = Y_Q56C + 1,
                Y_Q56E = Y_Q56E + 1)

paper_df7$Y_Q56E = as.factor(paper_df7$Y_Q56E)
paper_df7$Y_Q56C = as.factor(paper_df7$Y_Q56C)

Y_Q56C <- polr(Y_Q56C ~  treatment + urban + female + poverty + ownership + age + 
             education_dummy + radio_news + ethnic_major +president_coethnic + factor(REGION),
           data = paper_df7, method="logistic", Hess=TRUE)


Y_Q56E<- polr(Y_Q56E ~  treatment + urban + female + poverty + ownership + age + 
             education_dummy + radio_news + ethnic_major +president_coethnic + factor(REGION),
           data = paper_df7, method="logistic", Hess=TRUE)

save(Y_Q56C,Y_Q56E, paper_df7, file = "Replication_data/Results_robust_Y22.RData")



## look at overall 
paper_df8 <- afrov5_nigeria %>% 
    dplyr::filter(!is.na(Y_Q65A), !is.na(Y_Q65A))

paper_df8$Y_Q65A = as.factor(paper_df8$Y_Q65A)
paper_df8$Y_Q65B = as.factor(paper_df8$Y_Q65B)

r1_econ <- polr(Y_Q65A ~  treatment + urban + female + poverty + ownership + age + 
             education_dummy + radio_news + ethnic_major +president_coethnic +factor(REGION),
           data = paper_df8, method="logistic", Hess=TRUE)

r1_living <- polr(Y_Q65B ~  treatment + urban + female + poverty + ownership + age + 
             education_dummy + radio_news + ethnic_major +president_coethnic +factor(REGION),
           data = paper_df8, method="logistic", Hess=TRUE)

save(r1_econ, r1_living, paper_df8, file = "Replication_data/Results_overallnigeria.RData")
############################################################################################


## look at overall: afrov4_mali
paper_df9 <- afrov4_mali %>% 
  dplyr::filter(!is.na(Y_Q57A), !is.na(Y_Q57A))

paper_df9$Y_Q57B = as.factor(paper_df9$Y_Q57B)
paper_df9$Y_Q57A = as.factor(paper_df9$Y_Q57A)

r2_econ <- polr(Y_Q57A ~  treatment + urban + female + poverty + ownership + age + 
                  education_dummy + radio_news + ethnic_major +president_coethnic +factor(REGION),
                data = paper_df9, method="logistic", Hess=TRUE)

r2_living <- polr(Y_Q57B ~  treatment + urban + female + poverty + ownership + age + 
                    education_dummy + radio_news + ethnic_major +president_coethnic +factor(REGION),
                  data = paper_df9, method="logistic", Hess=TRUE)

save(r2_econ, r2_living, paper_df9, file = "Replication_data/Results_overallmali.RData")
############################################################################################
############################################################################################

##### Figure: Appendix Figure 1

##
load("Replication_data/Results_robust_Y22.RData")
r0_coef_nigeria <- scaled_coef_polr(ModelResults = list(Y_Q56C,Y_Q56E), data = paper_df7, subvar = TRUE, 
                                    vars = c('cut1','cut2','cut3', 'president_coethnic',
                                             'ethnic_major','radio_news','education_dummy',
                                             'age', 'ownership','poverty','female','urban','treatment'))

Y_Q56C$n

r0_coef_nigeria <- r0_coef_nigeria + theme(text = element_text(size=12, family ='STSong')) +
  ggtitle("尼日利亚")+
  scale_colour_manual(values = c("black", "blue"),labels = c("被传统领导不平等对待", "被雇主不平等对待"))+
  scale_shape_manual(values = c(17,22), labels = c("被传统领导不平等对待", "被雇主不平等对待"))+
  scale_x_discrete(labels = c("阈值1","阈值2","阈值3",
                              "受访者与总统相同民族",
                              "受访者属于主导民族",
                              "收听收音机新闻",
                              "受访者教育水平",
                              "受访者年龄",
                              "受访者财产所有",
                              "受访者贫困状态",
                              "女性受访者",
                              "城市受访者",
                              "武装袭击"
  )) +  labs(caption = "注：为节省空间，此处略去地区固定效应系数（N = 2206）")

ggsave("Tex/r0_coef_nigeria.png", dpi = 400, width = 8, height = 6, units = "in")

###############################################################

##### Figure: Appendix Figure 3
## for the women-related variable
### coef plot
load("Replication_data/Results_overallnigeria.RData")
r1_econ$n
r1_coef_nigeria <-scaled_coef_polr(ModelResults = list(r1_econ, r1_living), data = paper_df8, subvar = TRUE, 
                                vars = c('cut1','cut2','cut3', 'president_coethnic',
                                         'ethnic_major','radio_news','education_dummy',
                                         'age', 'ownership','poverty','female','urban','treatment'))

r1_coef_nigeria + theme(text = element_text(size=14, face = "bold", family ='Hei')) +
  ggtitle("尼日利亚")+
  scale_colour_manual(values = c("black", "blue"),labels = c("管理经济", "提供穷人生活水平"))+
  scale_shape_manual(values = c(17,22), labels = c("管理经济", "提供穷人生活水平"))+
  scale_x_discrete(labels = c("阈值1","阈值2","阈值3",
                              "受访者与总统相同民族",
                              "受访者属于主导民族",
                              "收听收音机新闻",
                              "受访者教育水平",
                              "受访者年龄",
                              "受访者财产所有",
                              "受访者贫困状态",
                              "女性受访者",
                              "城市受访者",
                              "武装袭击"
  )) +  labs(caption = "注：为节省空间，此处略去地区固定效应系数（N = 2326）")

ggsave("Tex/r1_coef_nigeria.png", dpi = 400, width = 8, height = 6, units = "in")

load("Replication_data/Results_overallmali.RData")
r2_econ$n
r2_coef_nigeria <-scaled_coef_polr(ModelResults = list(r2_econ, r2_living), data = paper_df9, subvar = TRUE, 
                                   vars = c('cut1','cut2','cut3', 'president_coethnic',
                                            'ethnic_major','radio_news','education_dummy',
                                            'age', 'ownership','poverty','female','urban','treatment'))

r2_coef_nigeria + theme(text = element_text(size=14, face = "bold", family ='Hei')) +
  ggtitle("马里共和国")+
  scale_colour_manual(values = c("black", "blue"),labels = c("管理经济", "提供穷人生活水平"))+
  scale_shape_manual(values = c(17,22), labels = c("管理经济", "提供穷人生活水平"))+
  scale_x_discrete(labels = c("阈值1","阈值2","阈值3",
                              "受访者与总统相同民族",
                              "受访者属于主导民族",
                              "收听收音机新闻",
                              "受访者教育水平",
                              "受访者年龄",
                              "受访者财产所有",
                              "受访者贫困状态",
                              "女性受访者",
                              "城市受访者",
                              "武装袭击"
  )) +  labs(caption = "注：为节省空间，此处略去地区固定效应系数（N = 1150）")

ggsave("Tex/r2_coef_mali.png", dpi = 400, width = 8, height = 6, units = "in")

