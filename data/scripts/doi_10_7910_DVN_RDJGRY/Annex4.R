# Open packages 

source('scripts/cleaning_and_management/libraries.R') 

# Code for split violin plots and adjusted labels

source('scripts/cleaning_and_management/split_violin_and_labels.R') 

# Load data 

# Delphi survey
expanel <- readRDS("data/Delphi survey/expanel.rds") 


##############################################################
#### APPENDIX A4. Variation and convergence among experts ####

total_var <- expanel %>% 
  filter(variable=="total",
         type =="flow",
         analised_sample==1,
         panel==1) %>% 
  
  ggplot(aes(scenario_name, value, fill = wave)) +
  geom_hline(yintercept = 0, color="black", size=.5) +
  geom_split_violin(alpha=.5, color=NA)+
  geom_boxplot(width=.3, outlier.shape=NA, coef = 0
               , color=NA
  ) +
  stat_summary(fun.y = median, fun.ymin = median, fun.ymax = median,
               geom = "crossbar", width = 0.25, aes(group = wave, x = scenario_name, y = value),
               position = position_dodge(width=.3)) +
  scale_x_discrete(position = "top")+
  labs(y=NULL,x=NULL,title = "Total inflows (millions)")+
  scale_y_continuous(breaks=seq(0, 6000000, by = 1000000), 
                     labels = scaleFUN_million)+
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor=element_blank(),
        #        legend.position =  "bottom",
        legend.title = element_blank(),
        text=element_text(family="Times New Roman", size = 30/.pt)) +
  scale_fill_manual(labels = c("Wave 1", "Wave 2"), values = c("#d6604d", "#4393c3"))+
  coord_cartesian(ylim=c(0, 5000000)) 

# Labour

labour_var <- expanel %>% 
  filter(variable=="labour",
         type =="flow",
         analised_sample==1,
         panel == 1) %>%
  ggplot(aes(scenario_name, value, fill = wave)) +
  geom_split_violin(alpha=.5, color=NA)+
  geom_boxplot(width=.3, outlier.shape=NA, coef = 0, color=NA) +
  stat_summary(fun.y = median, fun.ymin = median, fun.ymax = median,
               geom = "crossbar", width = 0.25, aes(group = wave, x = scenario_name, y = value),
               position = position_dodge(width=.3)) +
  labs(y=NULL,x=NULL,title = "Labour inflows (thousands)")+
  scale_y_continuous(breaks=seq(0, 2500000, by = 500000), 
                     labels = scaleFUN_th)+
  scale_x_discrete(position = "top")+
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor=element_blank(),
        legend.position =  "none",
        legend.title = element_blank(),
        text=element_text(family="Times New Roman", size = 30/.pt)) +
  scale_fill_manual(labels = c("Wave 1", "Wave 2"), values = c("#d6604d", "#4393c3"))+
  geom_hline(yintercept = 0, color="black", size=.5) +
  coord_cartesian(ylim=c(0, 1800000))

# high skilled

high_var <- expanel %>% 
  filter(variable=="high",
         type =="flow",
         analised_sample==1,
         panel ==1) %>%
  ggplot(aes(scenario_name, value, fill = wave)) +
  geom_split_violin(alpha=.5, color=NA)+
  geom_boxplot(width=.3, outlier.shape=NA, coef = 0
               , color=NA
  ) +
  stat_summary(fun.y = median, fun.ymin = median, fun.ymax = median,
               geom = "crossbar", width = 0.25, aes(group = wave, x = scenario_name, y = value),
               position = position_dodge(width=.3)) +
  labs(y=NULL,x=NULL,title = "High-skilled inflows (thousands)")+
  scale_y_continuous(breaks=seq(0, 250000, by = 50000), 
                     labels = scaleFUN_th)+
  scale_x_discrete(position = "top")+
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor=element_blank(),
        legend.position =  "none",
        legend.title = element_blank(),
        text=element_text(family="Times New Roman", size = 30/.pt)) +
  scale_fill_manual(labels = c("Wave 1", "Wave 2"), values = c("#d6604d", "#4393c3"))+
  geom_hline(yintercept = 0, color="black", size=.5) +
  coord_cartesian(ylim=c(0, 200000))

# asylum

asylum_var <- expanel %>% 
  filter(variable=="asylum",
         type =="flow",
         analised_sample==1,
         panel == 1) %>%
  ggplot(aes(scenario_name, value, fill = wave)) +
  geom_split_violin(alpha=.5, color=NA)+
  geom_boxplot(width=.3, outlier.shape=NA, coef = 0
               , color=NA
  ) +
  stat_summary(fun.y = median, fun.ymin = median, fun.ymax = median,
               geom = "crossbar", width = 0.25, aes(group = wave, x = scenario_name, y = value),
               position = position_dodge(width=.3)) +
  labs(y=NULL,x=NULL,title = "First-time asylum applications (thousands)")+
  scale_y_continuous(breaks=seq(0, 1200000, by = 200000), 
                     labels = scaleFUN_th)+
  scale_x_discrete(position = "top")+
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor=element_blank(),
        legend.position =  "none",
        legend.title = element_blank(),
        text=element_text(family="Times New Roman", size = 30/.pt)) +
  scale_fill_manual(labels = c("Wave 1", "Wave 2"), values = c("#d6604d", "#4393c3"))+
  geom_hline(yintercept = 0, color="black", size=.5) +
  coord_cartesian(ylim=c(0, 1200000))

# irregular

irregular_var <- expanel %>% 
  filter(variable=="irregular",
         type =="flow",
         analised_sample==1, 
         panel == 1) %>%
  ggplot(aes(scenario_name, value, fill = wave)) +
  geom_split_violin(alpha=.5, color=NA)+
  geom_boxplot(width=.3, outlier.shape=NA, coef = 0
               , color=NA
  ) +
  stat_summary(fun.y = median, fun.ymin = median, fun.ymax = median,
               geom = "crossbar", width = 0.25, aes(group = wave, x = scenario_name, y = value),
               position = position_dodge(width=.3)) +
  labs(x=NULL,y=NULL,title = "Irregular border crossings (thousands)")+
  scale_y_continuous(breaks=seq(0, 1000000, by = 100000), 
                     labels = scaleFUN_th)+
  scale_x_discrete(position = "top")+
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor=element_blank(),
        legend.position =  "none",
        legend.title = element_blank(),
        text=element_text(family="Times New Roman", size = 30/.pt)) +
  scale_fill_manual(labels = c("Wave 1", "Wave 2"), values = c("#d6604d", "#4393c3"))+
  geom_hline(yintercept = 0, color="black", size=.5) +
  coord_cartesian(ylim=c(0, 550000))


cairo_ps("output/figures/Annex4.eps", 
         family = "Times New Roman",
         width = 7.5,
         height = 8,
         fallback_resolution = 500)

absolut <- total_var + labour_var + high_var + asylum_var + irregular_var + 
  guide_area() + 
  plot_layout(guides = 'collect',
              ncol = 2)

absolut

dev.off()





