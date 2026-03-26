########################################################
## This R-file produces Figure 7 in SI
########################################################

## Packages
rm(list = ls())

install.packages(c("tidyverse", "ggplot2", "ggmosaic", "lattice", "haven", "lfe"))
library(tidyverse);library(ggplot2);library(ggmosaic);library(lattice);library(haven);library(lfe)

##
set.seed(123)
## set working directory as root = "

## Read in data
df = read_dta("10_Data_analysis_final.dta",
              col_select = c('master_district',
                             'total_score_2013_2014','total_score_2011_2012','PA3_11',
                             'id','persig12','persig14','PA3A','PA3B',
                             'win_again_ofall_noind','win_again_noind')) %>%
  filter(!is.na(win_again_ofall_noind)) %>%
  mutate(persig12 = case_when(persig12==0 ~ "Low",
                              persig12==1 ~ "High"),
         persig14 = case_when(persig14==0 ~ "Low",
                              persig14==1 ~ "High")) %>%
  mutate(persig12 = factor(persig12, levels = c("Low","High"), labels = c("Low","High")),
         persig14 = factor(persig14, levels = c("Low","High"), labels = c("Low","High")))

df_control = df %>% filter(id == 0)
df_treat = df %>% filter(id == 1)


########################################################################
## Mosaic plot
########################################################################
mosaic_data = bind_rows(df%>%mutate(sample = "Full"),
                        df_control%>%mutate(sample = "Control"),
                        df_treat%>%mutate(sample = "Treatment"))
mosaic_data_share = data.frame(sample = c(rep("Full",4), rep("Treatment",4), rep("Control",4)),
                               levels = c(rep(c("HH",'HL','LH','LL'),3)),
                               values = c(round(table(df$persig12, df$persig14)[2,2]/nrow(df),2),
                                          round(table(df$persig12, df$persig14)[2,1]/nrow(df),2),
                                          round(table(df$persig12, df$persig14)[1,2]/nrow(df),2),
                                          round(table(df$persig12, df$persig14)[1,1]/nrow(df),2),
                                          round(table(df_treat$persig12, df_treat$persig14)[2,2]/nrow(df_treat),2),
                                          round(table(df_treat$persig12, df_treat$persig14)[2,1]/nrow(df_treat),2),
                                          round(table(df_treat$persig12, df_treat$persig14)[1,2]/nrow(df_treat),2),
                                          round(table(df_treat$persig12, df_treat$persig14)[1,1]/nrow(df_treat),2),
                                          round(table(df_control$persig12, df_control$persig14)[2,2]/nrow(df_control),2),
                                          round(table(df_control$persig12, df_control$persig14)[2,1]/nrow(df_control),2),
                                          round(table(df_control$persig12, df_control$persig14)[1,2]/nrow(df_control),2),
                                          round(table(df_control$persig12, df_control$persig14)[1,1]/nrow(df_control),2)))%>%
  mutate(sample=factor(sample, levels = c("Full","Treatment","Control"), 
                       labels = c("Full","Treatment","Control")))

mosaic_plot = ggplot(data = mosaic_data%>%mutate(sample=factor(sample, levels = c("Full","Treatment","Control"), 
                                                               labels = c("Full","Treatment","Control")))) +
  geom_mosaic(aes(x = product(persig14, persig12), fill = persig14, group = sample)) +
  scale_fill_manual(values = c("lightsalmon4", "lightcyan4"),
                    breaks = c("High","Low")) + 
  geom_text(data = mosaic_data_share%>%filter(levels=="LH"),
            aes(x=0.25,y=0.83,label=values), size=3.5) +
  geom_text(data = mosaic_data_share%>%filter(levels=="LL"),
            aes(x=0.25,y=0.32,label=values), size=3.5) +
  geom_text(data = mosaic_data_share%>%filter(levels=="HL"),
            aes(x=0.75,y=0.17,label=values), size=3.5) +
  geom_text(data = mosaic_data_share%>%filter(levels=="HH"),
            aes(x=0.75,y=0.68,label=values), size=3.5) +
  xlab("Performance Signal in 2012") +
  ylab("Performance Signal in 2014") +
  facet_wrap(~sample) +
  theme_mosaic() +
  theme(legend.position = "none")

ggsave("Figure-SI7.pdf", mosaic_plot, width = 8, height = 6)
