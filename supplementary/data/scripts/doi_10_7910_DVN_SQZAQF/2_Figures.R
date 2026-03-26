rm(list = ls())
library(dplyr)
library(showtext)
library(ggthemes)
library(scales)
library(extrafont)
showtext_auto(enable = TRUE)
font_add('KaiTi', 'KaiTi.ttf')
## working on the plotting
source("Code/set_up.R")

load("Replication_data/Results_main.RData")
load("Replication_data/afrov5_nigeria.RData")
load("Replication_data/afrov4_mali.RData")
load("Replication_data/df_pool.RData")

sort(unique(afrov5_nigeria$DATEINTR))
sort(unique(afrov4_mali$DATEINTR))

#################################### ################## ##################
## Figure 3
################## ################## ####################################

df <- afrov4_mali %>% 
  dplyr::filter(!is.na(Y_Q57P)) %>% 
  dplyr::group_by(DATEINTR, Y_Q57P) %>% 
  dplyr::summarise(n = n())
#generate a number of most distinctive colors in R
library(randomcoloR)
n <- 4
palette <- distinctColorPalette(n, runTsne = TRUE)

mali <- ggplot(df, aes(x =DATEINTR , y =n , fill = factor(Y_Q57P, labels = c("非常差","比较差","比较好","非常好")))) + 
  geom_bar(position="stack", stat="identity") + scale_x_date(date_breaks = "2 day", date_labels = "%m-%d") + 
  scale_fill_manual(name = "", values = c("#94926C", "#B564D9", "#B7D871", "#D990A6")) +
  theme_bw() + xlab("日期") + ylab("频次")+
  geom_vline(xintercept = as.Date("2008-12-20"), colour = "black", lty = 1, size = 1) +
  theme(text = element_text(size=14, face = "bold", family ='Hei'),
        strip.text.x = element_text(size = 14, face = "bold"))
ggsave("Tex/y_mali.png", plot = mali, width = 7, height = 3, dpi = 600, units = "in")

df2 <- afrov5_nigeria %>% 
  dplyr::filter(!is.na(Y_Q65P)) %>% 
  dplyr::group_by(DATEINTR,Y_Q65P) %>% 
  dplyr::summarise(n = n())  

nigeria <- ggplot(df2, aes(x =DATEINTR , y =n , fill = factor(Y_Q65P, labels = c("非常差","比较差","比较好","非常好")))) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(name = "", values = c("#94926C", "#B564D9", "#B7D871", "#D990A6")) +
  theme_bw() + xlab("日期") + ylab("频次") +
  geom_vline(xintercept = as.Date("2012-11-05"), colour = "black", lty = 1, size = 1) +
  scale_x_date(date_breaks = "2 day", date_labels = "%m-%d") + 
  theme(text = element_text(size=14, face = "bold", family ='Hei'),
        strip.text.x = element_text(size = 14, face = "bold"))

ggsave("Tex/y_nigeria.png", plot = nigeria, width = 7, height = 3, dpi = 600, units = "in")



################## ################## ################## 
## Figure 4 
################## ################## ################## 
## Model with nigeria: 
paper_df6 <- afrov5_nigeria %>% 
  dplyr::filter(!is.na(Y_Q65P)) 
paper_df6$Y_Q65P = as.factor(paper_df6$Y_Q65P)

### coef plot
coef_nigeria <-scaled_coef_polr(ModelResults = list(n1, n2), data = paper_df6, subvar = TRUE, 
                                vars = c('cut1','cut2','cut3', 'president_coethnic',
                                         'ethnic_major','radio_news','education_dummy',
                                         'age', 'ownership','poverty','female','urban','treatment'))

coef_nigeria + theme(text = element_text(size=14, face = "bold", family ='Hei')) +
  ggtitle("尼日利亚")+
  scale_colour_manual(values = c("black", "blue"),labels = c("固定效应：否", "固定效应：是"))+
  scale_shape_manual(values = c(17,22), labels = c("固定效应：否", "固定效应：是"))+
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
  )) +  labs(caption = "注：为节省空间，此处略去地区固定效应系数（N = 2304）")

ggsave("Tex/coef_nigeria.png", dpi = 600, width = 8, height = 6, units = "in")


coef_mali <-scaled_coef_polr(ModelResults = list(m1, m2), data = afrov4_mali, subvar = TRUE, 
                             vars = c('cut1','cut2','cut3', 'president_coethnic',
                                      'ethnic_major','radio_news','education_dummy',
                                      'age', 'ownership','poverty','female','urban','treatment'))
m2$n
coef_mali + theme(text = element_text(size=14, face = "bold", family ='Hei')) +
  ggtitle("马里共和国")+
  scale_colour_manual(values = c("black", "blue"),labels = c("固定效应：否", "固定效应：是"))+
  scale_shape_manual(values = c(17,22), labels = c("固定效应：否", "固定效应：是"))+
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
  )) +  labs(caption = "注：为节省空间，此处略去地区固定效应系数（N = 1164）")

ggsave("Tex/coef_mali.png", dpi = 600, width = 8, height = 6, units = "in")

################## ################## ################## ################## 
## Figure 5
############边际效应################## ################## ################## 

n2_fd <- polr_fd(ModelResult = n2, n_sim = 1000, varible = "treatment")
plot_df <- gather(n2_fd, level, value)

# we can only show some outocme
plot_df <- plot_df %>% 
  dplyr::filter(level == "V3" | level == "V4")

ggplot(plot_df, aes(x=value)) + 
  geom_density(aes(y = ..density.., group = level, fill = level), alpha=0.4) +
  facet_wrap(~level, scales = "fixed",
             labeller = labeller(level = c("V3" = "比较好",
                                           "V4" = "非常好"))) + 
  geom_vline(xintercept = 0, colour = gray(1/2), lty = 2) + theme_tufte() +
  scale_fill_manual(values = c("gold","blue")) + 
  theme(legend.position = "none",
        text = element_text(size=14, face = "bold", family ='Hei'),
        strip.text.x = element_text(size = 14, face = "bold")) +
  xlab("边际效应（概率变化）分布") + ylab("分布密度") + 
  labs(caption = "注：固定效应模型（尼日利亚）")
ggsave("Tex/fd_nigeria.png", dpi = 600, width = 6, height = 3, units = "in")


m2_fd <- polr_fd(ModelResult = m2, n_sim = 1000, varible = "treatment")
plot_df2 <- gather(m2_fd, level, value)

# we can only show some outocme
plot_df2 <- plot_df2 %>% 
  dplyr::filter(level == "V3" | level == "V4")

ggplot(plot_df2, aes(x=value)) + 
  geom_density(aes(y = ..density.., group = level, fill = level), alpha=0.4) +
  facet_wrap(~level, scales = "fixed",
             labeller = labeller(level = c("V3" = "比较好",
                                           "V4" = "非常好"))) + 
  geom_vline(xintercept = 0, colour = gray(1/2), lty = 2) + theme_tufte() +
  scale_fill_manual(values = c("gold","blue")) + 
  theme(legend.position = "none",
        text = element_text(size=14, face = "bold", family ='Hei'),
        strip.text.x = element_text(size = 14, face = "bold")) +
  xlab("边际效应（概率变化）分布") + ylab("分布密度") + 
  labs(caption = "注：固定效应模型（马里共和国）")
ggsave("Tex/fd_mali.png", dpi = 600, width = 6, height = 3, units = "in")

########################################################################################
####################### Figure 1 #####################################
########################################################################################
load("Replication_data/attacks.RData")
## make a map
map_mali <- cshp(date = as.Date("2008-12-31"))
map_nigeria  <- cshp(date = as.Date("2012-12-31"))
map_mali <- map_mali[map_mali@data$COWCODE == 432,]
map_nigeria  <- map_nigeria[map_nigeria@data$COWCODE == 475,]

attacks[, c("country", "latitude","longitude")]


map_mali <- sf::st_read("Replication_data/gadm36_MLI_shp/gadm36_MLI_1.shp")
map_nga <- sf::st_read("Replication_data/gadm36_NGA_shp/gadm36_NGA_1.shp")

### balance test
library(ggpubr)

library("ggspatial")
library(ggplot2)
library(ggrepel)
ggplot(map_mali) +     
  geom_sf() +
  geom_sf_label(aes(label = NAME_1))+
  geom_point(data = attacks[attacks$country == "Mali",], 
             aes(longitude, latitude),shape = 17, color = "blue",
             size = 4)+ xlab("") + ylab("") + theme_bw()+
  theme(legend.position = "none", 
        panel.spacing = unit(0, "lines"),
        plot.title = element_text(size = 30, face = "bold"),
        plot.margin = unit(c(0, 0, 0, 0), "lines"),
        complete = F,
        axis.text = element_text(size=14),
        legend.text=element_text(size=14),
        axis.title=element_text(size=16)) + 
  coord_sf()
ggsave("Tex/map_mali.png", width = 6, height = 6, dpi = 400, units = "in")


ggplot(map_nga) +     
  geom_sf() +
  geom_sf_label(aes(label = NAME_1))+
  geom_point(data = attacks[attacks$country == "Nigeria",], 
             aes(longitude, latitude),shape = 17, color = "blue",
             size = 4)+ xlab("") + ylab("") + theme_bw()+
  theme(legend.position = "none", 
        panel.spacing = unit(0, "lines"),
        plot.title = element_text(size = 30, face = "bold"),
        plot.margin = unit(c(0, 0, 0, 0), "lines"),
        complete = F,
        axis.text = element_text(size=14),
        legend.text=element_text(size=14),
        axis.title=element_text(size=16)) + 
  coord_sf()
ggsave("Tex/map_nigeria.png", width = 6, height = 6, dpi = 400, units = "in")

