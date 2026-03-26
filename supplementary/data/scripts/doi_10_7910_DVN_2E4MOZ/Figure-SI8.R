########################################################
## This R-file produces Figure 8 in SI
########################################################

## Packages
rm(list = ls())

install.packages(c("tidyverse", "ggplot2", "ggmosaic", "lattice", "haven", "lfe", "gghalves", "Rmisc"))
library(tidyverse);library(ggplot2);library(ggmosaic);library(lattice);library(haven)
library(lfe);library(gghalves);library(Rmisc)

##
set.seed(123)
## set working directory as root = ""

## Read in data
df = read_dta("10_Data_analysis_final.dta",
              col_select = c('counid','master_district',
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
## Line plot
########################################################################  

df_long_cont = df %>% 
  dplyr::select(counid, total_score_2011_2012, total_score_2013_2014, id) %>% 
  pivot_longer(-c(counid, id), names_to = "pre_post", values_to = "values") %>%
  mutate(pre_post = case_when(pre_post == "total_score_2011_2012" ~ "pre",
                              pre_post == "total_score_2013_2014" ~ "post"),
         id = factor(id, levels = c(0,1), labels = c("Control", "Treatment"))) %>%
  mutate(pre_post = as.numeric(factor(pre_post, levels = c("pre", "post")))) %>%
  ## Add jitter
  mutate(prepost_jitter = jitter(pre_post, amount = 0.09))

## add descriptive stats
mean_pre_control = mean(df_control$total_score_2011_2012)
mean_post_control = mean(df_control$total_score_2013_2014)
median_pre_control = median(df_control$total_score_2011_2012)
median_post_control = median(df_control$total_score_2013_2014)
sd_pre_control = stats::sd(df_control$total_score_2011_2012)
sd_post_control = stats::sd(df_control$total_score_2013_2014)
se_pre_control = sd_pre_control/sqrt(173)
se_post_control = sd_post_control/sqrt(173)
ci_pre_control = CI(df_control$total_score_2011_2012, ci = 0.95)
ci_post_control = CI(df_control$total_score_2013_2014, ci = 0.95)

mean_pre_treat = mean(df_treat$total_score_2011_2012)
mean_post_treat = mean(df_treat$total_score_2013_2014)
median_pre_treat = median(df_treat$total_score_2011_2012)
median_post_treat = median(df_treat$total_score_2013_2014)
sd_pre_treat = stats::sd(df_treat$total_score_2011_2012)
sd_post_treat = stats::sd(df_treat$total_score_2013_2014)
se_pre_treat = sd_pre_treat/sqrt(181)
se_post_treat = sd_post_treat/sqrt(181)
ci_pre_treat = CI(df_treat$total_score_2011_2012, ci = 0.95)
ci_post_treat = CI(df_treat$total_score_2013_2014, ci = 0.95)

## create data frame with 2 rows and 7 columns containing descriptive stats
group = c("pre_control","post_control", "pre_treat","post_treat")
N = c(173, 173, 181, 181)
mean = c(mean_pre_control, mean_post_control,mean_pre_treat, mean_post_treat)
median = c(median_pre_control, median_post_control,median_pre_treat, median_post_treat)
sd = c(sd_pre_control, sd_post_control,sd_pre_treat, sd_post_treat)
se = c(se_pre_control, se_post_control,se_pre_treat, se_post_treat)
ci = c(ci_pre_control[1]-ci_pre_control[3], ci_post_control[1]-ci_post_control[3],
       ci_pre_treat[1]-ci_pre_treat[3], ci_post_treat[1]-ci_post_treat[3])

summary_df_cont_tc = data.frame(group, N, mean, median, sd, se, ci, colors = c("control","control","treatment","treatment"))

base_plot = ggplot(data = df_long_cont, aes(y=values)) +
  geom_point(data = df_long_cont %>% filter(pre_post=="1"), aes(x=prepost_jitter, color=id),size = 1.5, alpha=.6) +
  scale_color_manual(values = c("dodgerblue", "darkorange"),
                     breaks = c("Treatment","Control")) +
  geom_point(data = df_long_cont %>% filter(pre_post=="2"), aes(x=prepost_jitter, color=id),size = 1.5, alpha=.6) +
  scale_color_manual(values = c("dodgerblue", "darkorange"),
                     breaks = c("Treatment","Control")) +
  geom_line(aes(x= prepost_jitter, group = counid, color = id), alpha = .3) +
  geom_half_boxplot(
    data = df_long_cont %>% filter(pre_post=="1", id == "Control"), aes(x = pre_post, y=values), 
    position = position_nudge(x=-0.38), side = "r", outlier.shape = NA, center=TRUE,
    errorbar.draw = FALSE, width=.2, alpha = 0.5, fill = "darkorange") + 
  geom_half_boxplot(
    data = df_long_cont %>% filter(pre_post=="1", id == "Treatment"), aes(x = pre_post, y=values), 
    position = position_nudge(x=-0.25), side = "r", outlier.shape = NA, center=TRUE,
    errorbar.draw = FALSE, width=.2, alpha = 0.5, fill = "dodgerblue") +
  geom_half_boxplot(
    data = df_long_cont %>% filter(pre_post=="2", id == "Control"), aes(x = pre_post, y=values), 
    position = position_nudge(x=0.31), side = "r", outlier.shape = NA, center=TRUE,
    errorbar.draw = FALSE, width=.2, alpha = 0.5, fill = "darkorange") + 
  geom_half_boxplot(
    data = df_long_cont %>% filter(pre_post=="2", id == "Treatment"), aes(x = pre_post, y=values), 
    position = position_nudge(x=0.18), side = "r", outlier.shape = NA, center=TRUE,
    errorbar.draw = FALSE, width=.2, alpha = 0.5, fill = "dodgerblue") +
  geom_half_violin(
    data = df_long_cont %>% filter(pre_post=="1"), aes(x = pre_post, y=values, fill = id), 
    position = position_nudge(x=-0.45), side = "l", alpha = 0.5) +
  scale_fill_manual(values = c("dodgerblue", "darkorange"),
                    breaks = c("Treatment","Control")) +
  geom_half_violin(
    data = df_long_cont %>% filter(pre_post=="2"), aes(x = pre_post, y=values, fill = id), 
    position = position_nudge(x=0.45), side = "r", alpha = 0.5) +
  scale_fill_manual(values = c("dodgerblue", "darkorange"),
                    breaks = c("Treatment","Control")) +
  scale_x_continuous(breaks = c(1,2), labels = c("Pre treatment", "Post treatment"), limits = c(0,3)) +
  xlab("") + 
  ylab("Performance Score (continous measure)") +
  labs(color = NULL, fill = NULL) +
  ggtitle("Performance score at 2011/2012 versus 2013/2014 with medians") +
  theme_classic() +
  theme(legend.title = element_text("")) + ## Add a line to connect two medians
  geom_line(data = summary_df_cont_tc %>% filter(colors == "control"), aes(x =c(.87, 2.13), y = median), 
            color = "darkorange", size = 2) +
  geom_line(data = summary_df_cont_tc %>% filter(colors == "treatment"), aes(x =c(.87, 2.13), y = median), 
            color = "dodgerblue", size = 2) + ## Add a line to connect two medians
  geom_line(data = summary_df_cont_tc %>% filter(colors == "control"), aes(x =c(.87, 2.13), y = median), 
            color = "darkorange", size = 2) +
  geom_line(data = summary_df_cont_tc %>% filter(colors == "treatment"), aes(x =c(.87, 2.13), y = median), 
            color = "dodgerblue", size = 2)


ggsave("Figure-SI8.pdf", base_plot, width = 8, height = 6)

















