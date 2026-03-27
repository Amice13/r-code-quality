# Open packages 

source('scripts/cleaning_and_management/libraries.R') 

# Code for split violin plots and adjusted labels

source('scripts/cleaning_and_management/split_violin_and_labels.R') 

# Load data 

# Delphi survey
expanel <- readRDS("data/Delphi survey/expanel.rds") 

cairo_ps("output/figures/Annex3_Variation_Convergence.eps", 
         family = "Times New Roman",
         width = 6.8,
         height = 4,
         fallback_resolution = 500)

expanel %>% 
  dplyr::mutate(scenario_name = fct_recode(scenario_name,  
                                           "1: Economic convergence\nand unilateralism" = "Scenario 1",
                                           "2: Economic convergence\n    and multilateralism" = "Scenario 2",
                                           "3: Economic divergence\n    and unilateralism" = "Scenario 3",
                                           "4: Economic divergence\n    and mutilateralism" = "Scenario 4")) %>% 
  filter(variable=="total", 
         type=="flow",
         panel=="1", 
         analised_sample==1 ) %>%
  ggplot(aes(scenario_name, value, fill = wave)) +
  geom_hline(yintercept = 0, color="black", size=.5) +
  geom_split_violin(alpha=.5, color=NA)+
  geom_boxplot(width=.3, outlier.shape=NA, coef = 0, color=NA) +
  stat_summary(fun.y = median, fun.ymin = median, fun.ymax = median,
               geom = "crossbar", width = 0.25, aes(group = wave, x = scenario_name, y = value),
               position = position_dodge(width=.3)) +
  geom_curve(aes(x = 0.95, y =2950000, xend = 0.85, yend =3300000 ), colour = "grey40",curvature = .5)+
  geom_curve(aes(x = 0.95, y = 1500000, xend = 0.85, yend = 1150000), colour = "grey40",curvature = -.5)+
  geom_segment(aes(x = 0.75, y = 2000000, xend = 0.9, yend = 2000000), colour = "grey40") +
  
  geom_text(x = .65, y = 1150000, aes(label = c("25%"), family="Times New Roman", fontface = "plain"), colour = "grey40", size = 9/.pt)+
  geom_text(x = .65, y = 2000000, aes(label = c("50%"),family="Times New Roman", fontface = "plain"), colour = "grey40", size= 9/.pt)+
  geom_text(x = .65, y = 3300000, aes(label = c("75%\nof experts"),family="Times New Roman", fontface = "plain"), colour = "grey40", size= 9/.pt) +
  scale_x_discrete(position = "top")+
  labs(y=NULL,x=NULL,title = "Total inflows (millions)")+
  scale_y_continuous(breaks=seq(0, 6000000, by = 1000000), 
                     labels = scaleFUN_million)+
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor=element_blank(),
        legend.position =  "bottom",
        legend.title = element_blank(),
        text=element_text(family="Times New Roman", size = 30/.pt)) +
  scale_fill_manual(labels = c("Wave 1", "Wave 2"), values = c("#d6604d", "#4393c3"))+
  coord_cartesian(ylim=c(0, 5000000))

dev.off()


