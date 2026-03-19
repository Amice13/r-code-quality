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

#################################### main models for Figures 4-5 ####################################
## Model with nigeria: 
paper_df6 <- afrov5_nigeria %>% 
  dplyr::filter(!is.na(Y_Q65P)) 
paper_df6$Y_Q65P = as.factor(paper_df6$Y_Q65P)

afrov4_mali <- afrov4_mali %>% 
        dplyr::filter(!is.na(Y_Q57P)) 

n1 <- polr(Y_Q65P ~  treatment + urban + female + poverty + ownership + age + 
             education_dummy + radio_news + ethnic_major +president_coethnic,
           data = paper_df6, method="logistic", Hess=TRUE)

n2 <- polr(Y_Q65P ~  treatment + urban + female + poverty + ownership + age + 
             education_dummy + radio_news + ethnic_major +president_coethnic + factor(REGION),
           data = paper_df6, method="logistic", Hess=TRUE)

m1 <- polr(Y_Q57P ~  treatment + urban + female + poverty + ownership + age + 
             education_dummy + radio_news + ethnic_major +president_coethnic,
           data = afrov4_mali, method="logistic", Hess=TRUE)

m2 <- polr(Y_Q57P ~  treatment + urban + female + poverty + ownership + age + 
             education_dummy + radio_news + ethnic_major +president_coethnic + factor(REGION),
           data = afrov4_mali, method="logistic", Hess=TRUE)

save(n1, n2, m1, m2, file = "Replication_data/Results_main.RData")



############ Figure 2

## balance test
library(broom)
library(purrr)

t0 <- t.test(urban ~ treatment,  data = paper_df6) 
t1 <- t.test(female ~ treatment, data = paper_df6) 
t2 <- t.test(age ~ treatment, data = paper_df6)
t3 <- t.test(education_dummy ~ treatment, data = paper_df6) ##education_dummy by treatment p-value = 3.392e-13
t5 <- t.test(poverty ~ treatment, data = paper_df6)
t6 <- t.test(radio_news ~ treatment,  data = paper_df6)  #**  radio_news by treatment  p-value = 0.006121
t7 <- t.test(ethnic_major ~ treatment,data = paper_df6)
t8 <- t.test(president_coethnic  ~ treatment,  data = paper_df6)
t9 <- t.test(ownership ~ treatment, data = paper_df6)

tab1 <- map_df(list(t0, t1, t2, t3, t5, t6, t7, t8, t9), tidy)

tab1  <- tab1 %>% 
  dplyr::mutate(sample = "Nigeria",
                variable = c("urban", "female", "age", "education_dummy", "poverty", "radio_news","ethnic_major", "president_coethnic", "ownership")) %>% 
  dplyr::select(variable, estimate, statistic, conf.low, conf.high, sample)

t0 <- t.test(urban ~ treatment,  data = afrov4_mali) 
t1 <- t.test(female ~ treatment, data = afrov4_mali) 
t2 <- t.test(age ~ treatment, data = afrov4_mali)
t3 <- t.test(education_dummy ~ treatment, data = afrov4_mali) ##education_dummy by treatment p-value = 3.392e-13
t5 <- t.test(poverty ~ treatment, data = afrov4_mali)
t6 <- t.test(radio_news ~ treatment,  data = afrov4_mali)  #**  radio_news by treatment  p-value = 0.006121
t7 <- t.test(ethnic_major ~ treatment,data = afrov4_mali)
t8 <- t.test(president_coethnic  ~ treatment,  data = afrov4_mali)
t9 <- t.test(ownership ~ treatment, data = afrov4_mali)
tab2 <- map_df(list(t0, t1, t2, t3, t5, t6, t7, t8, t9), tidy)

tab2  <- tab2 %>% 
  dplyr::mutate(sample = "Mali",
                variable = c("urban", "female", "age", "education_dummy", "poverty", "radio_news","ethnic_major", "president_coethnic", "ownership")) %>% 
  dplyr::select(variable, estimate, statistic, conf.low, conf.high, sample)

tab <- bind_rows(tab1, tab2)

tab$variable <- factor(tab$variable, levels = c("urban", "female", "age", "education_dummy", "poverty", "radio_news","ethnic_major", "president_coethnic", "ownership"))

ggplot(tab, aes(colour =sample)) + 
  geom_hline(yintercept = 0, lty = 2) +
  geom_pointrange(aes(x = variable, y = estimate, ymin = conf.low,
                      ymax = conf.high, shape = sample),
                  lwd = 1, position = position_dodge(width = 1/2),
                  fill = "WHITE") +
  coord_flip() + 
  theme_bw() + xlab("") + ylab("平均值的差值(T检验)") + 
  theme(legend.position="bottom",
        legend.title=element_blank(),
        axis.text = element_text(size=14),
        text = element_text(size=14, face = "bold", family ='Hei'),
        plot.title = element_text(hjust = .5, size = 16, face = "bold")) + 
  scale_colour_manual(values = c("black", "blue"),labels = c("马里共和国", "尼日利亚"))+
  scale_shape_manual(values = c(17,22), labels = c("马里共和国", "尼日利亚")) + 
  scale_x_discrete(labels = c( "城市受访者","女性受访者","受访者年龄", "受访者教育水平",
                               "受访者贫困状态","收听收音机新闻",  "受访者属于主导民族",
                               "受访者与总统相同民族", "受访者财产所有" ))

ggsave("Tex/t_test.png", dpi = 400, width = 8, height = 6, units = "in")





tab <- map_df(list(t0, t1, t2, t3, t4, t5, t6, t7, t8, t9), rbind)
library(xtable)
xtable(tab)
t1 <- compare_means(urban ~ treatment, method = "t.test", data =afrov4_mali) 
t1 <- compare_means(female ~ treatment, method = "t.test", data =afrov4_mali) 
t2 <- compare_means(age ~ treatment, method = "t.test", data = afrov4_mali)
t3 <- compare_means(education_dummy ~ treatment,method = "t.test",  data = afrov4_mali) ##education_dummy by treatment p-value = 3.392e-13
t4 <- compare_means(female ~ treatment, method = "t.test",  data = afrov4_mali)
t5 <- compare_means(poverty ~ treatment,method = "t.test",  data =afrov4_mali)
t6 <- compare_means(radio_news ~ treatment, method = "t.test", data = afrov4_mali)  #**  radio_news by treatment  p-value = 0.006121
t7 <- compare_means(ethnic_major ~ treatment, method = "t.test", data = afrov4_mali)
t8 <- compare_means(president_coethnic  ~ treatment, method = "t.test", data = afrov4_mali)
t9 <- compare_means(ownership ~ treatment, method = "t.test", data = afrov4_mali)


tab <- map_df(list(t1, t2, t3, t4, t5, t6, t7, t8, t9), rbind)
library(xtable)
xtable(tab)

#### regression selection

b1 <- glm(treatment ~ urban + female + poverty + ownership + age + 
            education_dummy + radio_news + ethnic_major +president_coethnic,
          data = paper_df6, family = binomial(link = "logit"))

b2 <- glm(treatment ~ urban + female + poverty + ownership + age + 
            education_dummy + radio_news + ethnic_major +president_coethnic,
          data = afrov4_mali, family = binomial(link = "logit"))


plot_b1 <- scaled_coef_cluster(ModelResults = list(b1), data = paper_df6, clusterid = "REGION",
                               subvar = TRUE,
                               vars = c('(Intercept)','president_coethnic',
                                        'ethnic_major','radio_news','education_dummy',
                                        'age', 'ownership','poverty','female','urban'))

plot_b1 <- plot_b1 %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(mod = "Nigeria")

plot_b2 <- scaled_coef_cluster(ModelResults = list(b2), data = afrov4_mali, clusterid = "REGION",
                               subvar = TRUE,
                               vars = c('(Intercept)','president_coethnic',
                                        'ethnic_major','radio_news','education_dummy',
                                        'age', 'ownership','poverty','female','urban'))

plot_b2 <- plot_b2 %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(mod = "Mali")

plot_b <- bind_rows(plot_b1, plot_b2)


p <- ggplot(plot_b, aes(colour = mod)) + 
  geom_hline(yintercept = 0, lty = 2) +
  geom_linerange(aes(x = Variables, ymin = Low90CI,
                     ymax = High90CI),
                 lwd = 2, position = position_dodge(width = 1/2)) +
  geom_pointrange(aes(x = Variables, y = Coefficients, ymin = Low95CI,
                      ymax = High95CI, shape = mod),
                  lwd = 1, position = position_dodge(width = 1/2),
                  fill = "WHITE") +
  coord_flip() + 
  theme_bw() + xlab("") + ylab("标准化回归系数") + 
  theme(legend.position="bottom",
        legend.title=element_blank(),
        axis.text = element_text(size=14),
        text = element_text(size=14, face = "bold", family ='Hei'),
        plot.title = element_text(hjust = .5, size = 16, face = "bold"))


p +  scale_colour_manual(values = c("black", "blue"),labels = c("马里共和国", "尼日利亚"))+
  scale_shape_manual(values = c(17,25), labels = c("马里共和国", "尼日利亚"))+
  scale_x_discrete(labels = c("常数项",
                              "受访者与总统相同民族",
                              "受访者属于主导民族",
                              "收听收音机新闻",
                              "受访者教育水平",
                              "受访者年龄",
                              "受访者财产所有",
                              "受访者贫困状态",
                              "女性受访者",
                              "城市受访者")) + 
  labs(caption = "注：标准差根据地区进行聚类")
ggsave("Tex/coef_balancetest.png", dpi = 400, width = 8, height = 6, units = "in")


#############################################################

### summary table
library(stargazer)
df1 <- paper_df6 %>% 
  dplyr::select(Y_Q65P, treatment, urban, female,poverty,ownership, age, 
                education_dummy, radio_news, ethnic_major,president_coethnic ) %>% 
  as.data.frame()

df2 <- afrov4_mali %>% 
  dplyr::select(Y_Q57P, treatment, urban, female,poverty,ownership, age, 
                education_dummy, radio_news, ethnic_major,president_coethnic ) %>% 
  as.data.frame()

stargazer(df1, header=FALSE, out = "Tex/table_ni.html",  type = "text", title = "描述性统计结果(尼日利亚)",
          summary.stat = c("n", "mean", "median", "sd", "min", "max"),
          digit.separator = "")

stargazer(df2, header=FALSE, out = "Tex/table_mali.html",  type = "text", title = "描述性统计结果（马里）",
          summary.stat = c("n", "mean", "median", "sd", "min", "max"),
          digit.separator = "")

