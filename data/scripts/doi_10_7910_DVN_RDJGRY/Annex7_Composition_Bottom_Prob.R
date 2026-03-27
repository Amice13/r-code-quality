source('scripts/cleaning_and_management/libraries.R') 


# read data

expanel <- readRDS("data/Delphi survey/expanel.rds") 

# transform 

expert <- expanel %>% 
  
  # Convergence - change of estimate when wave 1 estimate is different from wave 2 estimate, including when expert did not provide estimate in wave 1 but did so in wave 2 by putting the two waves next to "each other".
  
  tidyr::pivot_wider(names_from = c(wave), values_from = value) %>% 
  mutate(convergence = case_when(wave1!=wave2 ~ 1,
                                 # !is.na(wave2) & is.na(wave1)~ 1,
                                 is.na(wave2) & is.na(wave1) ~ NA_real_,
                                 TRUE ~ 0)) %>% 
  pivot_longer(wave1:wave2, names_to = "wave", values_to = "value") %>% 
  
  # create deviation from median per scenario and flow
  
  group_by(type, variable, scenario_name, wave) %>%
  mutate(
    # make percentiles per type (flow or confidence), variable (type of flow), scenario and wave
    value_perrank = ntile(value,100),
    # dummy for those in percentile 25 or below and 75 or above
    per_25 = case_when(
      value_perrank <= 25 ~ 1,
      value_perrank > 25 ~ 0),
    per_75 = case_when(
      value_perrank >= 75 ~ 1,
      value_perrank < 75 ~ 0),
    
    stakeholder = case_when(
      stakeholder %in% c("Practitioner (civil servant, policy-maker)") ~ "Practitioner",
      stakeholder %in% c("Scholar (university professor, researcher)") ~ "Scholar",
      stakeholder %in% c("Other") ~ "Other")) %>% 
  
  ungroup() %>% 
  mutate(
    years_2 = years_1^2) %>% 
  
  select(value,
         per_25,
         per_75,
         convergence,
         everything()) %>% 
  pivot_longer(value:convergence, names_to = "model", values_to = "values") %>% 
  
  # select and turn into factor
  
  select(
    
    #indepvars 
    sex,
    years_cat,
    # years_1,
    # years_2,
    africa_regexp,
    americas_regexp,
    asia_regexp,
    europe_regexp,
    oceania_regexp,
    migsce_exp,
    migfor_exp,
    migdri_exp,
    stakeholder,
    # scholar_stake,
    # practitioner_stake,
    polsci_aca,
    sociol_aca,
    demogr_aca,
    econom_aca,
    lawlaw_aca,
    
    # depvar
    values,
    
    # model
    id,
    wave,
    panel,
    scenario_name,
    variable,
    
    # iteration
    
    type,
    model) %>% 
  
  mutate(across(sex:lawlaw_aca, ~factor(.x)))



# function
delphimodel <- function(df){
  
  lm_robust(values ~
              sex +
              
              # age categorical or continuous
              years_cat+
              # years_1+
              # years_2+
              
              # regional experience
              africa_regexp+
              americas_regexp+
              asia_regexp+
              europe_regexp+
              oceania_regexp+
              
              # type of research
              migsce_exp+
              migfor_exp+
              migdri_exp+
              
              # type of stakeholder
              # scholar_stake+
              # practitioner_stake+
              stakeholder+
              
              # academic background
              polsci_aca+
              sociol_aca+
              demogr_aca+
              econom_aca+
              lawlaw_aca+
              
              wave+
              panel+
              scenario_name+
              variable,
            clusters = id,
            se_type = "stata",
            data = df) }

expert <- expert %>% 
  filter(type != "prob")

dataset_large <- split(expert, list(expert$type, expert$model))

model <- map(dataset_large, delphimodel)

fitted_vals <- map2(.x = model, .y = dataset_large, 
                    ~ predict(.x,
                              newdata = .y,
                              interval = "confidence"))

dataset_large_fit <- map2(.x = dataset_large, 
                          .y = fitted_vals,
                          ~ cbind(.x,.y))

pivoted_dataset_large_fit <- map(dataset_large_fit, ~ pivot_longer(., cols = c(sex,
                                                                               years_cat,
                                                                               # years_1,
                                                                               # years_2,
                                                                               africa_regexp,
                                                                               americas_regexp,
                                                                               asia_regexp,
                                                                               europe_regexp,
                                                                               oceania_regexp,
                                                                               migsce_exp,
                                                                               migfor_exp,
                                                                               migdri_exp,
                                                                               stakeholder,
                                                                               # scholar_stake,
                                                                               # practitioner_stake,
                                                                               polsci_aca,
                                                                               sociol_aca,
                                                                               demogr_aca,
                                                                               econom_aca,
                                                                               lawlaw_aca),
                                                                   names_to = "val",
                                                                   values_to = "term"))

pivoted_dataset_large_fit_gr <- map(pivoted_dataset_large_fit, ~ group_by(., 
                                                                          model,
                                                                          type,
                                                                          term,
                                                                          val))


sum <- pivoted_dataset_large_fit_gr %>% map(., 
                                            ~ summarise(.,
                                                        estimate = mean(fit.fit, na.rm = T),
                                                        conf.low  = mean(fit.lwr, na.rm = T),
                                                        conf.high  = mean(fit.upr,na.rm = T)))

# brakets

brackets <- list(c("Sex","Man","Woman"),
                 c("Age","0 - 4",">=20"),
                 c("Region of expertise","Africa","Oceania"),
                 c("Expertise", "Scenarios", "Drivers"),
                 c("Stakeholder","Other","Scholar"),
                 c("Academic", "Political science","Law"))


# bottom25 


# values
conf_val <- sum[["flow.per_25"]] %>% 
  filter(term != "No",
         term != 0)  

plot <- conf_val %>%
  mutate(val = gsub(pattern = ".*_regexp.*",
                    "Region of expertise",
                    val),
         
         val =gsub(pattern = ".*_aca.*",
                   "Academic background",
                   val),
         
         val =gsub(pattern = ".*_exp.*",
                   "Expertise in future migration",
                   val),
         
         val =  case_when(
           val %in% c("sex") ~ "Sex",
           val %in% c("years_cat") ~ "Years of experience",
           val %in% c("stakeholder") ~ "Stakeholder",
           T ~ val)) %>% 
  
  ggplot(aes(y=term,x=estimate)) +
  geom_errorbar(aes(xmin=conf.low, xmax=conf.high), width=0.4, alpha=0.5) +
  geom_point(aes(color=estimate),
             size=3.5) +
  facet_grid(stringr::str_wrap(val,15) ~., scales = "free", space = "free") +
  labs(
    x = "Predicted probability (%)",
    y = NULL) +
  theme_fira() +
  theme(
    
    ## Text
    strip.text.y = element_text(angle = 0, size = 28/.pt),
    
    # strip.background = element_rect(fill=blue_lightest),
    # axis.text.y = element_text(margin = margin(r=5)),
    text=element_text(family="Times New Roman", size = 30/.pt),
    
    ## Panel
    # panel.background = element_blank(),
    panel.grid.minor = element_blank(),
    
    ## Legend
    legend.position = "none",
    axis.ticks = element_blank())


cairo_ps("output/figures/Annex7_Composition_Bottom_Prob.eps", 
         family = "Times New Roman",
         width = 15, 
         height = 13,
         fallback_resolution = 500)

plot

dev.off()

