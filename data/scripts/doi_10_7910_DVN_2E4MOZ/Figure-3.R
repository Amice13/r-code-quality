#############################################################################
## This R-file produces Figure 3 in the paper 
#############################################################################

rm(list = ls())

install.packages(c("tidyverse","haven","ggplot2","ggmosaic"))
library(tidyverse);library(haven);library(ggplot2);library(ggmosaic)

##
set.seed(123)

## set working directory as root = ""

dataRaw = read_dta("10_Data_analysis_final.dta",
                   col_select = c('master_district',
                                  'total_score_2013_2014','total_score_2011_2012','PA3_11',
                                  'id','persig14','PA3A','PA3B',
                                  'win_again_ofall_noind','win_again_noind')) %>%
  filter(!is.na(win_again_ofall_noind)) %>%
  mutate(persig14 = case_when(persig14==0 ~ "Low Performance",
                              persig14==1 ~ "High Performance"),
         id = case_when(id==0 ~ "Control",
                        id==1 ~ "Treatment")) %>%
  mutate(persig14 = factor(persig14, levels = c("Low Performance",'High Performance')),
         id = factor(id, levels = c("Control",'Treatment')))

PrA = dataRaw %>% select(persig14, id, win_again_ofall_noind) %>% group_by(persig14, id) %>% 
  summarize_at(.vars = 'win_again_ofall_noind', .funs = mean, na.rm=TRUE) %>%
  mutate(win_again_ofall_noind = round(win_again_ofall_noind, 2),
         panels = "Pr(won again (unconditional))") %>%
  dplyr::rename(`Pr(won again)` = win_again_ofall_noind)


PrB = dataRaw %>% select(persig14, id, win_again_noind) %>% group_by(persig14, id) %>% 
  summarize_at(.vars = 'win_again_noind', .funs = mean, na.rm=TRUE) %>%
  mutate(win_again_noind = round(win_again_noind, 2),
         panels = "Pr(won again (conditional))") %>%
  dplyr::rename(`Pr(won again)` = win_again_noind)

Pr = PrA %>% bind_rows(PrB) %>%
  mutate(panels = factor(panels, levels = c("Pr(won again (unconditional))","Pr(won again (conditional))")))

####################### Try Bar-plot

barPlot = ggplot(data = Pr, aes(x = persig14, y = `Pr(won again)`, fill = id)) +
  geom_bar(stat = "identity", position = position_dodge(0.55), width = 0.4) +
  geom_text(aes(label=`Pr(won again)`),color="black",hjust = 1.2,
            position = position_dodge(0.55), size=2.5) +
  scale_fill_manual(values = c("gray70", "gray90"),
                    breaks = c("Treatment","Control")) + 
  scale_x_discrete(expand=c(0.1, 0.1)) +
  scale_y_discrete(expand=c(0, 0)) +
  guides(fill=guide_legend(title="")) +
  
  ylab("") +
  xlab("") +
  facet_wrap(~ panels, scales = 'free', strip.position = "bottom") +
  coord_flip() + 
  #labs(title = "Did the incumbent win reelection (H3a)?") + 
  theme_mosaic() + 
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.text = element_text(size = 8),
        axis.text.y = element_text(size = 8, color = "black"),
        strip.background = element_blank(),
        strip.text = element_text(size = 9),
        strip.switch.pad.grid = unit(-0.01,"cm"), strip.placement = "outside",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.line.y = element_line(color="black"),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        panel.spacing = unit(1.5, "cm"),
        plot.title = element_text(hjust = 0.5,face = "bold", size = 10, vjust = 10))


ggsave("Figure-3.pdf", barPlot, width = 8, height = 8)




