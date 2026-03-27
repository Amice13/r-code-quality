source('scripts/cleaning_and_management/libraries.R') 
# Code for split violin plots and adjusted labels

source('scripts/cleaning_and_management/split_violin_and_labels.R') 


# read data

expanel <- readRDS("data/Delphi survey/expanel.rds") 

# transform 

expert <- expanel %>% 
  
  # filter(type != "prob") %>% 
  
  tidyr::pivot_wider(names_from = c(wave), values_from = value) %>% 
  mutate(convergence = case_when(wave1!=wave2 ~ 1,
                                 # !is.na(wave2) & is.na(wave1)~ 1,
                                 is.na(wave2) & is.na(wave1) ~ NA_real_,
                                 TRUE ~ 0)) %>% 
  
  group_by(type, variable, scenario_name) %>% 
  
  mutate(mean_1 = mean(wave1, na.rm = T),
         sub_1 = mean_1 - wave1,
         sub_2 = mean_1 - wave2,
         
         convergence2 = case_when(
           sub_2 < sub_1  ~ 1,
           sub_1 <= sub_2 ~ 0,
           T ~ NA_real_)) %>% 
  
  pivot_longer(wave1:wave2, names_to = "wave", values_to = "value") %>% 
  mutate(
    
    stakeholder = case_when(
      stakeholder %in% c("Practitioner (civil servant, policy-maker)") ~ "Practitioner",
      stakeholder %in% c("Scholar (university professor, researcher)") ~ "Scholar",
      stakeholder %in% c("Other") ~ "Other")) %>%
  
  filter(type == "flow",
         wave == "wave2") %>% 
  
  pivot_longer(cols = c(sex,
                        years_cat,
                        africa_regexp,
                        americas_regexp,
                        asia_regexp,
                        europe_regexp,
                        oceania_regexp,
                        migsce_exp,
                        migfor_exp,
                        migdri_exp,
                        stakeholder,
                        polsci_aca,
                        sociol_aca,
                        demogr_aca,
                        econom_aca,
                        lawlaw_aca),
               
               names_to = "variables",
               values_to = "categories") %>% 
  
  
  group_by(variables, categories, convergence2) %>% 
  
  summarise(total = n()) %>% 
  drop_na() %>% 
  mutate(frq = round(100*(total/sum(total)), 1)) %>% 
  filter(categories != "No",
         convergence2 == 1) %>% 
  mutate(variables = gsub(pattern = ".*_regexp.*",
                          "Region of expertise",
                          variables),
         
         variables =gsub(pattern = ".*_aca.*",
                         "Academic background",
                         variables),
         
         variables =gsub(pattern = ".*_exp.*",
                         "Expertise in future migration",
                         variables),
         
         variables =  case_when(
           variables %in% c("sex") ~ "Sex",
           variables %in% c("years_cat") ~ "Years of experience",
           variables %in% c("stakeholder") ~ "Stakeholder",
           T ~ variables)) %>% 
  select(-total)



cairo_ps("output/figures/Annex6_Convergence.eps", 
         family = "Times New Roman",
         width = 15, 
         height = 13,
         fallback_resolution = 500)

ggplot(data = expert,
       aes(x = frq,
           y = reorder(stringr::str_wrap(categories,30), frq),
           fill = frq)) +
  geom_bar(stat = 'identity') +
  geom_bar_text(aes(label=frq, 
                    x = frq,
                    y = reorder(stringr::str_wrap(categories,30), frq)),
                family = "Times New Roman",
                fontface = "bold",
                
                color = "white",
                outside = T)+
  
  facet_grid(variables ~., scales = "free", space = "free") +
  geom_vline(xintercept = 0, color="black", size=1) +
  scale_x_continuous(position = 'bottom',
                     labels = scales::label_number(scale = 1, suffix = "%")) +
  scale_fill_gradient(low = "#8099D0", high = "#0033A0")+
  coord_cartesian(clip="off") +
  labs(caption = "Includes only experts that participated in wave 1 and 2.",
       y = NULL,
       x = "% of experts that changed their estimate of wave 2 closer to the mean of wave1")+
  # theme_bw() +
  theme(
    
    ## Text
    strip.text.y = element_text(angle = 0,
                                # color = blue_IOM
    ),
    
    axis.text.y = element_text(margin = margin(r=5)),
    text=element_text(family="Times New Roman", size = 30/.pt),
    
    
    ## Panel
    panel.background = element_blank(),
    panel.grid.minor = element_blank(),
    
    
    ## Legend
    legend.position = "none",
    
    axis.ticks = element_blank())


dev.off()
