# ReadMe ----------------------------------------------------------------------------
# Project - Covid19 WB info campaign
# Author  - Vasu Chaudhary
# Purpose - Effects by content plot

# libraries -------------------------------------------------------------------------

library(tidyverse)
library(estimatr)
library(fixest)
library(broom)
#library(lubridate)
library(modelsummary)
library(DescTools)
library(ggdist)
library(haven)

# Setup paths ----------------------------------------------------------------------

path_LM = "/Users/louis-maeljean/Dropbox (MIT)/West Bengal Information Campaign/AER_I/for_submission"
path = path_LM        #other users change this

# Loading Data ----------------------------------------------------------------------

df_main = read_dta(paste0(path,"/data/outcomes_reg_input.dta")) ## 1989 obs


# Helper Functions ----------------------------------------------------------------------------

fit_model = function(outcome, treatments, sample, controls, data, ci) {
  
  model_data = switch(sample,
                      jio      = filter(data, jio_yn == 1),
                      non_jio  = filter(data, jio_yn == 0),
                      pooled   = data)
  
  model_controls = switch(sample,
                          pooled = c(controls, "jio_yn"),
                          controls)
  
  tmt_form  = reformulate(response = outcome,
                          termlabels = c(treatments, model_controls))
  
  reg = lm_robust(formula  = tmt_form,
                  data     = model_data,
                  clusters = pincode,
                  se_type  = "stata",
                  alpha = (1 - ci)/2)
  
  p_test = summary(
    multcomp::glht(reg, linfct = c(paste0(treatments[1],
                                          " - ", treatments[2]," = 0"))))$test$pvalues[1]
  
  return(list( reg %>% 
                 tidy(conf.int = T, conf.level = ci) %>% 
                 filter(term %in% treatments),
               c(term = treatments[1],
                 outcome = outcome,
                 p_diff = p_test %>% round(3))))
  
}


plot_out = function(smp) { 
  
  input_config = crossing(outcome,
                          treatments = list(c("SD", "Hyg"),
                                            c("Ext", "Int"),
                                            c("NO", "Neut")),
                          sample = smp)
  
  df_res_90 = pmap(input_config, fit_model, controls = controls, data = df_main,
                   ci = 0.9) %>% map(1) %>% bind_rows() %>% mutate(.width = 0.9)
  
  df_res_95 = pmap(input_config, fit_model, controls = controls, data = df_main,
                   ci = 0.95) %>% map(1) %>% bind_rows() %>% mutate(.width = 0.95)
  
  df_res = bind_rows(df_res_90, df_res_95) %>% 
    mutate(arm = case_when(
      term %in% c("SD", "Hyg") ~ "1",
      term %in% c("Ext", "Int") ~ "2",
      term %in% c("NO", "Neut") ~ "3"
    ),
    arm = as.factor(arm))
  
  df_pdiff = pmap(input_config, fit_model, controls = controls, data = df_main,
                  ci = 0.9) %>% map(2) %>% bind_rows()
  
  df_res = df_res %>% left_join(df_pdiff, by = c("outcome", "term"))
  
  df_res$term = factor(df_res$term, levels = c("SD", "Hyg", "Ext", "Int", "NO", "Neut"))
  
  df_res$outcome = factor(df_res$outcome, levels = c('travel_own', 'W2total_interactions_vill', 'typical_handwash',
                                                     'resp_mask_wear', 'W2ever_talk', 'net_knowledge'))
  
  pp = ggplot(data = df_res, mapping = aes(
    y = term, x = estimate,
    xmin = conf.low, xmax = conf.high,
    color = arm)) +
    geom_pointinterval(
      position = position_dodge(width = 1.5),
      interval_size_range = c(0.5,1.2),
      fatten_point = 0.1,
      show.legend = F
    ) + 
    geom_point(aes(shape = arm), size = 2, show.legend = F) +
    scale_color_grey(start = 0.1, end = 0.7) +
    scale_shape_manual(values = c(0,1,2)) + geom_hline(yintercept = c(2.5, 4.5), col= "darkslategray")+
    facet_wrap(. ~ outcome, scales = "free_x", labeller = as_labeller(
      c("travel_own" = "Travel outside own village?",
        "W2total_interactions_vill" = "No. interactions with people within 2 arms' length",
        "typical_handwash" = "% of time handwash upon returning home",
        "resp_mask_wear" = "Did you use a mask?",
        "W2ever_talk" = "No. of COVID-19 related conversations",
        "net_knowledge" = "COVID-19 knowledge index"
      ))) +
    geom_vline(xintercept = 0, linetype = "dotted", size = 1, alpha = 0.7) +
    scale_y_discrete(limits = unique(df_res$term) %>% rev()) +
    theme(panel.grid = element_blank(),
          panel.background = element_rect(fill = "white", colour = "black"), 
          panel.border = element_rect(fill = NA, colour = "white"), 
          axis.line = element_line(),
          strip.background = element_blank(),
          panel.spacing = unit(2, "lines"),
          strip.text.x = element_text(size = 11)) +
    xlab("Estimate")
  
  pp + 
    geom_text(data = df_res %>% drop_na(p_diff) %>% filter(term %in% c("SD", "hyg")),  mapping = aes(
      y =term, x = estimate, label = paste0("SD = Hyg : ", p_diff)),
      vjust = 2.1, size = 3.5, color = "black",
      show.legend = F) +
    geom_text(data = df_res %>% drop_na(p_diff) %>% filter(term %in% c("Ext", "Int")),  mapping = aes(
      y =term, x = estimate, label = paste0("Ext = Int : ", p_diff)),
      vjust = 2.1, size = 3.5, color = "black",
      show.legend = F) +
    geom_text(data = df_res %>% drop_na(p_diff) %>% filter(term %in% c("NO", "Neut")),  mapping = aes(
      y =term, x = estimate, label = paste0("NO = Neut : ", p_diff)),
      vjust = 2.1, size = 3.5, color = "black",
      show.legend = F) + 
    labs(y = "Treatment")
  
  }

# setup ----------------------------------------------------------------------------------

controls = c("district", "id_date", "resp_age", "resp_gender", "smartphone")

outcome = c('travel_own', 'W2total_interactions_vill', 'typical_handwash',
              'resp_mask_wear', 'W2ever_talk', 'net_knowledge')

plot_out(smp = "pooled")

ggsave(paste0(path,"/output/Figures/Figure3_effects_by_content_pooled.png"), units = "in", width = 12, height = 7)



# plot_out(smp = "jio")
# 
# ggsave(paste0(path,"/output/Figures/TableA8_effects_by_content_jio.png"), units = "in", width = 12, height = 7)
# 
# 
# plot_out(smp = "non_jio")
# 
# ggsave(paste0(path,"/output/Figures/TableA9_effects_by_content_nonjio.png"), units = "in", width = 12, height = 7)



###############################
          ## END ##
###############################

