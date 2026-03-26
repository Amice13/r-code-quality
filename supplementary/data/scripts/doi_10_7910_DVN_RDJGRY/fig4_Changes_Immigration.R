# Open packages 

source('scripts/cleaning_and_management/libraries.R') 

# Code for split violin plots and adjusted labels

source('scripts/cleaning_and_management/split_violin_and_labels.R') 

# Load data 

# Delphi survey
expanel <- readRDS("data/Delphi survey/expanel.rds") 

# Migration data from Frontex and Eurostat

# all flows
flows_forecast <- readRDS("data/Migration data/clean(Frontex+Eurostat)/forecast.rds")

# total flows

non_eu <- flows_forecast %>% 
  filter(flow=="total") 

# labour migration 

mig_res_labor <- flows_forecast %>% 
  filter(flow=="labour") 

# high skilled migration

high <- flows_forecast %>% 
  filter(flow=="high") 

# irregular migration
irregular <- flows_forecast %>% 
  filter(flow=="irregular") 

# asylum applications
mig_res_asylum <- flows_forecast %>% 
  filter(flow=="asylum")


mig_res_labor <- mig_res_labor %>%
  filter(year %in% 2009:2018)

mig_res_asylum <- mig_res_asylum %>% 
  filter(year %in% 2009:2018)

irregular <- irregular %>% 
  filter(year %in% 2009:2018)

high <- high %>% 
  filter(year %in% 2009:2018)

per <- expanel %>%
  dplyr::filter(type=="flow",wave=="wave2" ,panel=="1", analised_sample==1) %>% 
  dplyr::group_by(variable,scenario_name) %>% 
  dplyr::summarise(val=mean(value,na.rm = T)) %>% 
  dplyr::mutate(scenario_name = fct_recode(scenario_name,  
                                           "1: Economic convergence\n    and unilateralism" = "Scenario 1",
                                           "2: Economic convergence\n    and multilateralism" = "Scenario 2",
                                           "3: Economic divergence\n    and unilateralism" = "Scenario 3",
                                           "4: Economic divergence\n    and multilateralism" = "Scenario 4")) %>%
  
  mutate(
    per_chan = case_when(
      variable == "total" ~ (val -  mean(non_eu$val,na.rm = T)) / mean(non_eu$val,na.rm = T)*100,
      variable == "labour" ~ (val - mean(mig_res_labor$val,na.rm = T)) / mean(mig_res_labor$val,na.rm = T)*100,
      variable == "high" ~  (val - mean(high$val,na.rm = T)) / mean(high$val,na.rm = T)*100,
      variable == "asylum" ~ (val - mean(mig_res_asylum$val, na.rm = T)) / mean(mig_res_asylum$val, na.rm = T)*100,
      variable == "irregular" ~ (val - mean(irregular$val , na.rm = T)) / mean(irregular$val , na.rm = T)*100),
    
    variable = case_when(
      variable=="asylum" ~ "Asylum",
      variable=="total" ~ "Total inflows",
      variable=="high" ~ "High-skilled",
      variable=="labour" ~ "Labour",
      variable=="irregular" ~ "Irregular"),
    
    variable = factor(variable, levels = c('High-skilled',
                                           'Labour',
                                           'Total inflows',
                                           'Irregular',
                                           'Asylum'))) %>% 
  ungroup()

# plot

cairo_ps("output/figures/fig4_Changes_Immigration.eps", 
         family = "Times New Roman",
         width = 8,
         height = 4,
         fallback_resolution = 500)

per %>% 
  ggplot(aes(scenario_name,per_chan)) +
  geom_col(aes(fill=scenario_name)) +
  geom_hline(yintercept = 0, color="black", size=.5) +
  ggfittext::geom_bar_text(
    place = "top",
    col = "black",
    contrast = F,
    aes(x=scenario_name,
        y=per_chan,
        family="Times New Roman",
        label = scales::number(per_chan,accuracy = 1,suffix = NULL)),
    outside=T)+
  facet_wrap(~variable,nrow = 1)+
  panel_border() +
  background_grid()+
  labs(x=NULL,
       y=NULL,
       fill = "Migration scenario")+
  theme_bw()+  
  theme(
    text=element_text(family="Times New Roman", size = 30/.pt),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0, face = "bold"))+
  scale_color_manual(values = cols_new,aesthetics = c("color", "fill"))


dev.off()


