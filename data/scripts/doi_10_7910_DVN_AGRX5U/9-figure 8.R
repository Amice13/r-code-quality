library(plyr)
library(tidyverse)
library(magrittr)
library(lsmeans)
library(broom)
library(ggjoy)

lt <- "elusive-tab 9.csv" %>% 
  read_csv

mt <- "elusive-tab 10.csv" %>% 
  read_csv

lt2 <- 1000 %>% 
  rdply(lt %>% 
          select(accordance, item) %>% 
          sample_frac(1, T) %>% 
          group_by(item) %>% 
          summarize(mu = accordance %>% 
                      mean),
        .progress = "time") %>% 
  tbl_df %>% 
  left_join(lt %>% 
              select(item, source) %>% 
              group_by(item) %>% 
              slice(1)) 


lt_lab <- lt2 %>% 
  group_by(item,
           source) %>% 
  summarize(mu_2 = mu %>% 
              mean,
            mu_lab = mu_2 %>% 
              round(1))

lt_lab$it_join <- lt_lab$item %>% 
  mapvalues(lt_lab$item %>%
              factor %>% 
              levels %>%
              sort,
            c(
              "carson-spiraling_teen_pregnancy",
              "trump-us_high_taxes",
              "trump-ue_30_perc",
              "trump-undocumented_immigrants",
              "trump-us_high_taxes",
              
              "longoria-whites_imminently_minority",
              "palin-obama_passed_tarp",
              "romney-china_holds_most_debt",
              "bush-tax_cuts",
              "bush-iraq_wmd",
              
              "obama-china_holds_most_debt",
              "obama-wage_gap", 
              "obama-drug_crime",
              "obama-obama_passed_tarp",                        
              "obama-obama_drones",
              
              "obama-spiraling_abortion",                  
              "obama-chicago_homicides",
              "gutierrez-obama_accomodates_undoc",          
              "lee-spiraling_teen_pregnancy",
              "ryan-spiraling_abortion",                 
              
              "clinton-hedge_fund",
              "clinton-solar_jobs",            
              "clinton-gun_violence",
              "kerry-bush_stem_cell",            
              "cruz-obama_accomodates_undoc",          
              
              "cruz-police_killing",      
              "graham-obama_drones",
              "graham-whites_imminently_minority",
              "rubio-obama_defense_spending",
              "sanders-epa_drinking_water",
              
              "sanders-healthcare_expensive",    
              "lapierre-chicago_homicides")) %>% 
  as.character

lmm1 <- lmer(ans ~ ideol * corr * it_join + (1|ResponseID),
             data = mt)

lml <- lmm1 %>% 
  lsmeans(~ corr|it_join,
          at = list(corr = mt$corr %>% 
                      unique,
                    it_join = mt$it_join %>% 
                      unique)
          ) %>% 
  pairs %>% 
  tidy

lml_lab <- lml %>% 
  select(it_join, estimate, std.error, p.value) %>% 
  mutate(est_lab = estimate %>% 
           round(1) %>% 
           as.character %>% 
           str_c(c("***",
                   "**",
                   "*") %>% 
                   extract(p.value %>% 
                             findInterval(c(-Inf,
                                            .001,
                                            .01,
                                            .05,
                                            Inf)))) %>% 
           str_replace(fixed("-1*"),
                       fixed("-1.0*"))) %>% 
  select(-p.value) %>% 
  left_join(lt_lab %>% 
              select(item, source, it_join),
            "it_join") %>% 
  tbl_df

lt_com <- lml_lab %>% 
  nrow %>% 
  seq %>% 
  ldply(function(i)
    data.frame(.n = 1:1000,
               mu = rnorm(1000,
                          lml_lab$estimate %>%
                            extract(i),
                          lml_lab$std.error %>%
                            extract(i))) %>% 
      mutate(it_join = lml_lab$it_join[[i]]),
    .progress = "time") %>% 
  tbl_df %>%
  left_join(lt_lab %>% 
              select(item, source, it_join),
            "it_join") %>% 
  select(.n, item, mu, source) %>% 
  mutate(fac = "Average Correction Effects") %>% 
  rbind.fill(lt2 %>%
               mutate(fac = "Perceived Accordance")) %>% 
  tbl_df %>% 
  mutate(fac = fac %>% 
           factor(c("Perceived Accordance",
                    "Average Correction Effects")))

lt_com$item %<>%
  factor(
    lt_com %>% 
      filter(
        fac %>% 
          str_detect("Average Correction Effects") %>% 
          not
        ) %>% 
      group_by(item) %>% 
      summarize(
        mu2 = mu %>% 
          mean
      ) %>% 
      arrange(
        mu2
      ) %>% 
      use_series(item)
  )

ggplot() +
  geom_joy(
    aes(y = item,
        x = mu,
        fill = source),
    color = "grey5",
    scale = 1,
    size = .25,
    alpha = .3,
    rel_min_height = .025,
    data = lt_com %>%
      filter(fac == "Perceived Accordance")
  ) +
  geom_joy(
    aes(y = item,
        x = mu,
        fill = source),
    color = "grey98",
    scale = 2,
    size = .25,
    alpha = .3,
    rel_min_height = .005,
    data = lt_com %>%
      filter(fac != "Perceived Accordance")) +
  geom_text(aes(mu_2, item, label = mu_lab),
            data = lt_lab %>% 
              mutate(fac = "Perceived Accordance" %>% 
                       factor(c("Perceived Accordance",
                                "Average Correction Effects"))), 
            vjust = 0,
            family = "Roboto",
            color = "grey10",
            size = 2.6,
            show.legend = F) +
  geom_text(aes(estimate, item, label = est_lab),
            data = lml_lab %>% 
              mutate(fac = "Average Correction Effects" %>% 
                       factor(c("Perceived Accordance",
                                "Average Correction Effects"))) %>% 
              filter(item %>% 
                       str_detect("Tax cuts increase revenue|Bush banned stem cell research") %>% 
                       not),
            color = "grey10",
            vjust = 0,
            family = "Roboto",
            size = 2.6,
            show.legend = F) +
  geom_vline(aes(xintercept = 0),
             linetype = 2,
             data = data.frame(fac = "Average Correction Effects" %>%
                                 factor(c("Perceived Accordance",
                                          "Average Correction Effects")))) + 
  facet_grid(. ~ fac, 
             scales = "free_x") +
  scale_fill_manual(values = c("grey1",
                               "grey70"),
                    guide = guide_legend()) +
  scale_color_manual(values = c("grey1",
                                "grey70")) +
  theme(
    legend.position = "bottom"
  )
