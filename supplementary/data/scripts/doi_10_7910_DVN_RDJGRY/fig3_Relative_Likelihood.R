# define colors of scenarios

cols_new <- c("1: Economic convergence\n and unilateralism" = "#418FDE",
              "2: Economic convergence\n and multilateralism" = "#5CB8B2",
              "3: Economic divergence\n and unilateralism" = "#FFB81C",
              "4: Economic divergence\n and multilateralism" = "#D22630")


# Load packages 

source('scripts/cleaning_and_management/libraries.R') 

# Load packages 

expanel <- readRDS("data/Delphi survey/expanel.rds") 

# fixing names and calculating data. 

prob_plot <- expanel %>% 
  dplyr::mutate(scenario_name = fct_recode(scenario_name,
                                           "1: Economic convergence\n and unilateralism" = "Scenario 1",
                                           "2: Economic convergence\n and multilateralism" = "Scenario 2",
                                           "3: Economic divergence\n and unilateralism" = "Scenario 3",
                                           "4: Economic divergence\n and multilateralism" = "Scenario 4")) %>% 
  
  # filter to wave 2 net sample and complete panel 
  
  dplyr::filter(wave=="wave2",analised_sample==1, panel==1,type=="prob") %>% 
  dplyr::group_by(scenario_name) %>% 
  summarise(val=mean(value,na.rm = T))

f_labels <- data.frame(label = c("Less likely"))
arrow_f_labels <- data.frame(label = c("◄"))

c_labels <- data.frame(label = c("More likely"))
arrow_c_labels <- data.frame(label = c("►"))

# first open cairo_ps and then draw plot

cairo_ps("output/figures/fig3_Relative_Likelihood.eps", 
         family = "Times New Roman",
         width = 9, 
         height = 5.5,
         fallback_resolution = 500)

prob_plot %>% 
  ggplot(aes(x=reorder(scenario_name,val), y= val))+
  geom_col(aes(fill=scenario_name), width=.5) +
  geom_hline(yintercept = 0, color="black", size=.5) +
  geom_hline(aes(yintercept = 25), color="black", size=.7,linetype="dotted") +
  geom_text(aes(x = scenario_name, y = val-0.8, label = round(val, 1), family="Times New Roman"),color="black") +
  scale_color_manual(values = cols_new,aesthetics = c("color", "fill")) +
  scale_y_continuous(breaks = c(0, 10, 20, 25))+
  labs(x="Migration scenario" , 
       y="Likelihood (%)",
       fill="Scenario")+
  theme_bw()+ 
  theme(
    axis.text = element_text(colour = "black"),
    text=element_text(family="Times New Roman", size = 35 / .pt),
    legend.position = "none",
    panel.border = element_blank(),
    panel.grid.major.y = element_blank(), 
    panel.grid.major = element_line(size = 0.7, colour = "grey80"),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(face = "bold")
    # ,
    # plot.caption = element_text(hjust = 0,color = "black",size = 11 / .pt, family="Times New Roman")
  ) +
  coord_flip()+
  guides(fill = guide_legend(keywidth=.2,
                             keyheight=.4,
                             default.unit="inch"))+
  geom_text(x = 4.5, y = 28, aes(label = label), data = c_labels,family="Times New Roman")+
  geom_text(x = 4.5, y = 22, aes(label = label), data = f_labels,family="Times New Roman")+
  
  geom_text(x = 4.5, y = 26, aes(label = label), data = arrow_c_labels)+
  geom_text(x = 4.5, y = 24, aes(label = label), data = arrow_f_labels)

dev.off()


