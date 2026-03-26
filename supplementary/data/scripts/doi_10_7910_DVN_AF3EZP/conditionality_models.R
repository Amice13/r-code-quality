# ---------------------------------- TITLE -------------------------------------
# NAME: models_resubmission.R
# AUTHOR: Jacob Winter & Mark Manger
# DATE: October 2023
# ENCODING: utf-8
# ---------------------------------- NOTES -------------------------------------
# Creates output tables for manuscript and all figures in the body
# ---------------------------------- SETUP -------------------------------------
dir = "~/wckm_wb_replication_package/"
setwd(dir)
sink("log_main.html")
library(tidyverse)
library(stargazer)
library(sandwich)
library(splines)
library(here)
# load the script that creates the time splines with default values
source("btscs.R")

library(sampleSelection)
library(car)
options(scipen=999)

# ---------------------------------- DATA -------------------------------------
# load the conditions data
dir = "~/repos/wbenvironmentaltextsr/replication_package/"
setwd(dir)


p_merged <- read_csv("data/data_wb_conditions.csv")
# read the panel data for the first stage
panel <- read_csv("data/data_wb_panel.csv") %>%
  btscs(event="selected", tvar="year", csunit="iso3c") #Prep time splines
# ---------------------------------- MODELS -------------------------------------
set.seed(123) # important to retain this seed to replicate the exact results in the paper

panel_reserve <- panel |> 
  select(country_year, iso3c, selected, shift_share_replen, probability, replenishments_log, spell) |>
  drop_na()
myProbit_replen <- glm(selected ~ shift_share_replen +  probability + replenishments_log + bs(spell, knots=4)
                       ,family = binomial( link = "probit" ), data=panel_reserve)


vcov = vcovBS(myProbit_replen, cluster = ~country_year, R=1000) #This will take a while.
linearHypothesis(myProbit_replen, c("probability=0", 
                                    "shift_share_replen=0"
                                    ), test="F",
                 vcov = vcov)
panel_reserve$IMRreplen <- invMillsRatio(myProbit_replen)$IMR1 #Mills ratio

p_merged$IMRreplen <- panel_reserve$IMRreplen[match(p_merged$country_year,panel_reserve$country_year)] #Add mills ratio if exists


#### Table OLS  ####
us_i_bivar_t1 <-  lm(stringency ~ us_ideal_distance_important +
                         factor(iso3c),
                       data=p_merged)

us_i_some_t1 <-  lm(stringency ~ us_ideal_distance_important + unsc_member +
                   growth + inflation + log_gdp_pc +
                   ieg_borr_filled + v2x_corr + v2x_polyarchy +
                   factor(iso3c),
                  data=p_merged)

g5_i_some_t1 <-  lm(stringency ~ g5_ideal_distance_important + unsc_member +
                   growth + inflation + log_gdp_pc +
                   ieg_borr_filled + v2x_corr + v2x_polyarchy +
                   factor(iso3c),
                 data=p_merged)

us_un_t1 <-  lm(stringency ~ us_unga_agree + unsc_member +
                     urbandruralworking_d  + log_amt + fy_end_board + imf_program + 
                     cumulative_projects_5 + credit_rating + china_share_3 + ibrd_d + DPL_adjustment +
                     growth + inflation + log_gdp_pc +
                     ieg_borr_filled + v2x_corr + v2x_polyarchy +
                     post_12 + post_05 +
                     factor(iso3c) + factor(topic_keyATM),
                   data=p_merged)

g5_un_t1 <-  lm(stringency ~ g5_unga_agree + unsc_member +
                     urbandruralworking_d  + log_amt + fy_end_board + imf_program + 
                     cumulative_projects_5 + credit_rating + china_share_3 + ibrd_d + DPL_adjustment +
                     growth + inflation + log_gdp_pc +
                     ieg_borr_filled + v2x_corr + v2x_polyarchy +
                     post_12 + post_05 +
                     factor(iso3c) + factor(topic_keyATM),
                   data=p_merged)

us_post0512_i_t1 <-  lm(stringency ~ us_ideal_distance_important + unsc_member +
                           urbandruralworking_d  + log_amt + fy_end_board + imf_program + 
                           cumulative_projects_5 + credit_rating + china_share_3 + ibrd_d + DPL_adjustment +
                           growth + inflation + log_gdp_pc +
                           ieg_borr_filled + v2x_corr + v2x_polyarchy + 
                           post_12 + post_05 + factor(iso3c) + factor(topic_keyATM),
                         data=p_merged)

g5_post0512_i_t1 <-  lm(stringency ~ g5_ideal_distance_important + unsc_member +
                           urbandruralworking_d  + log_amt + fy_end_board + imf_program + 
                           cumulative_projects_5 + credit_rating + china_share_3 + ibrd_d + DPL_adjustment +
                           growth + inflation + log_gdp_pc +
                           ieg_borr_filled + v2x_corr + v2x_polyarchy +
                           post_12 + post_05 + factor(iso3c) + factor(topic_keyATM),
                         data=p_merged)

us_base_i_DPL_t1 <-  lm(stringency ~ us_ideal_distance_important + unsc_member +
                            urbandruralworking_d  + log_amt + fy_end_board + imf_program + 
                            cumulative_projects_5 + credit_rating + china_share_3 + ibrd_d + DPL_adjustment +
                            growth + inflation + log_gdp_pc +
                            ieg_borr_filled + v2x_corr + v2x_polyarchy + post_12 + post_05 +
                            factor(iso3c) + factor(topic_keyATM),
                          data=p_merged, subset=(DPL_adjustment==TRUE))

g5_base_i_DPL_t1 <-  lm(stringency ~ g5_ideal_distance_important + unsc_member +
                           urbandruralworking_d  + log_amt + fy_end_board + imf_program + 
                           cumulative_projects_5 + credit_rating + china_share_3 + ibrd_d + DPL_adjustment +
                           growth + inflation + log_gdp_pc +
                           ieg_borr_filled + v2x_corr + v2x_polyarchy + post_12 + post_05 +
                           factor(iso3c) + factor(topic_keyATM),
                         data=p_merged, subset=(DPL_adjustment==TRUE))

models_table_OLS <- list(us_i_bivar_t1,
                     us_i_some_t1,
                     g5_i_some_t1,
                     us_post0512_i_t1,
                     g5_post0512_i_t1,
                     us_un_t1,
                     g5_un_t1,
                     us_base_i_DPL_t1,
                     g5_base_i_DPL_t1)

errors_table_OLS <- lapply(models_table_OLS, FUN = function(x)
  sqrt(diag(vcovBS(x, cluster = ~country_year, R=1000, type='wild')))
)


varmap1 <- c("UNGA Distance, (US, Important)",
             "UNGA Distance, (G5, Important)",
             "UNGA Agreement, (US, All)",
             "UNGA Agreement, (G5, All)",
             "UNSC Membership",
             "Working Class Support",
             "Total Loan Size",
             "Final Fiscal Quarter",
             "IMF Program",
             "Number Loans 5Yrs",
             "Credit Rating",
             "Chinese lending",
             "IBRD",
             "Policy Lending",
             "Economic Growth",
             "Inflation",
             "ln GDP per capita",
             "IEG evaluations",
             "Corruption",
             "Democracy",
             "Post 2012",
             "Post 2005")

stargazer(models_table_OLS,
          se = errors_table_OLS,
          covariate.labels = varmap1,
          omit=c("year", "iso3c", "topic_keyATM", "Constant"),
          omit.stat = c("adj.rsq", "ser", 'f'),
          add.lines=list(
            c("Country and Sector Effects", "Y", "Y", "Y", "Y", "Y", "Y","Y", "Y", "Y")#,
            # c("Year Effects", "N",  "N", "N", "Y", "Y", "Y","Y", "N", "N", "N", "N"),
            # c("Sector Effects", "N", "N", "N", "Y", "Y", "Y","Y", "Y", "Y", "Y", "Y")
            ),
          type='html',
          star.cutoffs = c(0.05, 0.01, 0.001),
          no.space=TRUE,
          notes = "* p<0.05 ** p<0.01 *** p<0.001",
          notes.append = F,
          notes.align="r",
          column.sep.width = "0pt",
          out="output/table_OLS_wDPL_main_results.doc"
)
#### 2 Stage ####
us_i_bivar_t2 <-  lm(stringency ~ us_ideal_distance_important +
                    IMRreplen + 
                    factor(iso3c),
                  data=p_merged)

us_i_some_t2 <-  lm(stringency ~ us_ideal_distance_important + unsc_member +
                   growth + inflation + log_gdp_pc +
                   ieg_borr_filled + v2x_corr + v2x_polyarchy +
                   IMRreplen + 
                   factor(iso3c),
                 data=p_merged)

g5_i_some_t2 <-  lm(stringency ~ g5_ideal_distance_important + unsc_member +
                   growth + inflation + log_gdp_pc +
                   ieg_borr_filled + v2x_corr + v2x_polyarchy +
                   IMRreplen + 
                   factor(iso3c),
                 data=p_merged)

us_post0512_i_t2 <-  lm(stringency ~ us_ideal_distance_important + unsc_member +
                       urbandruralworking_d  + log_amt + fy_end_board + imf_program + 
                       cumulative_projects_5 + credit_rating + china_share_3 + ibrd_d + DPL_adjustment +
                       growth + inflation + log_gdp_pc +
                       ieg_borr_filled + v2x_corr + v2x_polyarchy +
                       IMRreplen + 
                       post_12 + post_05 + factor(iso3c) + factor(topic_keyATM),
                     data=p_merged)

g5_post0512_i_t2 <-  lm(stringency ~ g5_ideal_distance_important + unsc_member +
                       urbandruralworking_d  + log_amt + fy_end_board + imf_program + 
                       cumulative_projects_5 + credit_rating + china_share_3 + ibrd_d + DPL_adjustment +
                       growth + inflation + log_gdp_pc +
                       ieg_borr_filled + v2x_corr + v2x_polyarchy +
                       IMRreplen + 
                       post_12 + post_05 + factor(iso3c) + factor(topic_keyATM),
                     data=p_merged)

us_un_t2 <-  lm(stringency ~ us_unga_agree + unsc_member +
                  urbandruralworking_d  + log_amt + fy_end_board + imf_program + 
                  cumulative_projects_5 + credit_rating + china_share_3 + ibrd_d + DPL_adjustment +
                  growth + inflation + log_gdp_pc +
                  ieg_borr_filled + v2x_corr + v2x_polyarchy +
                  IMRreplen + 
                  
                  post_12 + post_05 + factor(iso3c) + factor(topic_keyATM),
                data=p_merged)

g5_un_t2 <-  lm(stringency ~ g5_unga_agree + unsc_member +
                  urbandruralworking_d  + log_amt + fy_end_board + imf_program + 
                  cumulative_projects_5 + credit_rating + china_share_3 + ibrd_d + DPL_adjustment +
                  growth + inflation + log_gdp_pc +
                  ieg_borr_filled + v2x_corr + v2x_polyarchy +
                  IMRreplen + 
                  post_12 + post_05 + factor(iso3c) + factor(topic_keyATM),
                data=p_merged)

us_base_i_DPL_t2 <-  lm(stringency ~ us_ideal_distance_important + unsc_member +
                       urbandruralworking_d  + log_amt + fy_end_board + imf_program + 
                       cumulative_projects_5 + credit_rating + china_share_3 + ibrd_d + DPL_adjustment +
                       growth + inflation + log_gdp_pc +
                       ieg_borr_filled + v2x_corr + v2x_polyarchy +
                         
                       IMRreplen + 
                         post_12 + post_05 + factor(iso3c) + factor(topic_keyATM),
                     data=p_merged, subset=(DPL_adjustment==TRUE))

g5_base_i_DPL_t2 <-  lm(stringency ~ g5_ideal_distance_important + unsc_member +
                       urbandruralworking_d  + log_amt + fy_end_board + imf_program + 
                       cumulative_projects_5 + credit_rating + china_share_3 + ibrd_d + DPL_adjustment +
                       growth + inflation + log_gdp_pc +
                       ieg_borr_filled + v2x_corr + v2x_polyarchy +
                       IMRreplen + 
                         post_12 + post_05 + factor(iso3c) + factor(topic_keyATM),
                     data=p_merged, subset=(DPL_adjustment==TRUE))

models_table_2stage <- list(us_i_bivar_t2,
                         us_i_some_t2,
                         g5_i_some_t2,
                         us_post0512_i_t2,
                         g5_post0512_i_t2,
                         us_un_t2,
                         g5_un_t2,
                         us_base_i_DPL_t2,
                         g5_base_i_DPL_t2)

errors_table_O2stage <- lapply(models_table_2stage, FUN = function(x)
  sqrt(diag(vcovBS(x, cluster = ~country_year, R=1000, type='wild')))
)

varmap2 <- c("UNGA Distance, (US, Important)",
             "UNGA Distance, (G5, Important)",
             "UNGA Agreement, (US, All)",
             "UNGA Agreement, (G5, All)",
             "UNSC Membership",
             "Working Class Support",
             "Total Loan Size",
             "Final Fiscal Quarter",
             "IMF Program",
             "Number Loans 5Yrs",
             "Credit Rating",
             "Chinese lending",
             "IBRD",
             "Policy Lending",
             "Economic Growth",
             "Inflation",
             "ln GDP per capita",
             "IEG evaluations",
             "Corruption",
             "Democracy",
             "IMR",
             "Post 2012",
             "Post 2005")

stargazer(models_table_2stage,
          se = errors_table_O2stage,
           covariate.labels = varmap2,
          omit=c("year", "iso3c", "topic_keyATM", "Constant"),
          omit.stat = c("adj.rsq", "ser","f"),
          add.lines=list(
            c("Country and Sector Effects", "Y", "Y", "Y", "Y", "Y", "Y","Y", "Y", "Y", "Y", "Y")#,
            # c("Year Effects", "N",  "N", "N", "Y", "Y", "Y","Y", "N", "N", "N", "N"),
            # c("Sector Effects", "N", "N", "N", "Y", "Y", "Y","Y", "Y", "Y", "Y", "Y")
            ),
          type='html',
          star.cutoffs = c(0.05, 0.01, 0.001),
          no.space=TRUE,
          notes = "* p<0.05, ** p<0.01, *** p<0.001",
          notes.append = F,
          notes.align="r",
          column.sep.width = "0pt",
          out="output/table_2stage_wDPL_main_results.doc"
)

##### Figures #####

#### Figure 1: Project approvals over the fiscal cycle ####
months = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

p3 <- p_merged %>%
  group_by(month) %>%
  summarize(number_projects = n(),
            ave_stringency = mean(stringency_sum)) %>%
  drop_na() %>%
  mutate(month_name = factor(month.abb[month], levels=months))

max <- max(p3$number_projects)


figure_fiscal <- ggplot(drop_na(p3)) +
  geom_col(aes(x=month_name, y=number_projects), alpha =.9) +
  #geom_line(aes(x=month_name, y=scales::rescale(ave_stringency, to=c(0,max)), group=1), linetype=2, colour='firebrick') +
  geom_vline(aes(xintercept=6.5), color='black') +
  annotate("text", label="Fiscal Year End", x=8.5, y=max*.8, size=4, color="black") +
  #annotate("text", label="Average Loan Stringency \n (Rescaled)", x=3, y=max*.8, size=2.5, color="firebrick") +
  xlab("Month") +
  ylab("Projects Approved") +
  theme_bw()
# 
# 
figure_fiscal

ggsave(plot = figure_fiscal, "output/month_approvals.pdf", width=5, height=2)


#### Figure 2: Loans per country (map) ####### 
library(rnaturalearth)
library(rnaturalearthdata)
library(ggthemes)
world <- ne_countries(scale = "medium", returnclass = "sf")

p <- p_merged %>%
  group_by(iso3c) %>%
  summarize(number_projects= n())

map_merged <- left_join(world,  p, by=c("iso_a3"='iso3c')) %>% replace_na(list(number_projects=0))

gray_palette <- scale_fill_gradient(low = "white", high = "black")

map <- ggplot(data = map_merged) +
  geom_sf(aes(fill = number_projects), colour = "gray", alpha = 0.9) +
  gray_palette +
  labs(title = NULL,
       subtitle = NULL,
       fill = "Number of Loans") +
  theme_map() +
  theme(plot.subtitle=element_text(size=8, color="black", lineheight=.9, face="italic"),
        #plot.caption=element_text(size=7, color="grey40"),
        legend.title = element_text(size=10, color="black"),
        legend.position="right")

map

ggsave("output/conditions_map.pdf", width=10, height=5)

#### Figure 3: Term polarity ##### 


### Figure 4: Condition count vs stringency ####
fig_countvsscale <- ggplot(filter(p_merged, stringency_sum > -50, stringency_sum < 50)) +
  geom_jitter(aes(y=stringency_sum, x=ALCID_number), width=.05) +
  xlab("Number of Conditions (ALCID)") +
  ylab("Measured Stringency") +
  theme_bw() 
fig_countvsscale

ggsave(plot = fig_countvsscale, "output/countvsscale.pdf", width=6, height=6)

### Stringency vs Number since 2012 - Figure 4 in the Appendix ####
fig_countvsscale_2012 <- ggplot(filter(p_merged, stringency_sum > -50, stringency_sum < 50, year > 2012)) +
  geom_jitter(aes(y=stringency_sum, x=ALCID_number), width=.05) +  
  xlab("Number of Conditions (ALCID)") +
  ylab("Measured Stringency") +
  theme_bw() 
fig_countvsscale_2012

ggsave(plot = fig_countvsscale_2012, "output/countvsscale_2012.pdf", width=6, height=6)

#### Figure 5: Standardized coefficient plots ##########
multiplier = 1.96

model <- us_post0512_i_t1

deviations <- model[["model"]] %>%
  data.frame() %>%
  dplyr::select(-starts_with("factor")) %>%
  summarise_all(sd, na.rm=T) %>%
  t()

se = sqrt(diag(vcovBS(model, cluster = ~country_year, R=1000, type='wild')))

model_data_1 <- data.frame(variable = names(model[["coefficients"]]), coef = model[["coefficients"]], se = se) %>%
  filter(!grepl("factor",.$variable)) %>%
  mutate(sd = deviations[,1],
         std.est = coef * sd,
         std.se = se * sd,
         xmin = std.est - (multiplier * std.se),
         xmax = std.est + (multiplier * std.se),
         i = 1,
         id = cumsum(i)) %>%
  filter(!grepl("Intercept",.$variable))

model <- g5_post0512_i_t1

deviations <- model[["model"]] %>%
  data.frame() %>%
  dplyr::select(-starts_with("factor")) %>%
  summarise_all(sd, na.rm=T) %>%
  t()

se = sqrt(diag(vcovBS(model, cluster = ~country_year, R=1000, type='wild')))

model_data_important <- data.frame(variable = names(model[["coefficients"]]), coef = model[["coefficients"]], se = se) %>%
  filter(!grepl("factor",.$variable)) %>%
  mutate(sd = deviations[,1],
         std.est = coef * sd,
         std.se = se * sd,
         xmin = std.est - (multiplier * std.se),
         xmax = std.est + (multiplier * std.se),
         i = 1,
         id = cumsum(i)) %>%
  filter(!grepl("Intercept",.$variable))

model <- us_base_i_DPL_t1

deviations <- model[["model"]] %>%
  data.frame() %>%
  dplyr::select(-starts_with("factor")) %>%
  summarise_all(sd, na.rm=T) %>%
  t()

se = sqrt(diag(vcovBS(model, cluster = ~country_year, R=1000, type='wild')))

model_data_DPL <- data.frame(variable = names(model[["coefficients"]]), coef = model[["coefficients"]], se = se) %>%
  filter(!grepl("factor",.$variable)) %>%
  mutate(sd = deviations[,1],
         std.est = coef * sd,
         std.se = se * sd,
         xmin = std.est - (multiplier * std.se),
         xmax = std.est + (multiplier * std.se),
         i = 1,
         id = cumsum(i)) %>%
  filter(!grepl("Intercept",.$variable))

varnames <- c("UN voting distance",
              "UNSC membership",
              "Working Class Support",
              "Total Loan Size",
              "Final Fiscal Quarter",
              "IMF Program",
              "Number Loans 5Yrs",
              "Credit Rating",
              "Chinese lending",
              "IBRD",
              "Policy Lending",
              "Economic Growth",
              "Inflation",
              "ln GDP per capita",
              "IEG evaluations",
              "Corruption",
              "Democracy")

model_data_1$model <- "US (Model 4)"
model_data_1$id_2 <- model_data_1$id

model_data_important$model <- "G5 (Model 5)"
model_data_important$id_2 <- model_data_important$id + .2

model_data_DPL$model <- "US DPL (Model 8)"
model_data_DPL$id_2 <- model_data_DPL$id + .4

m <- model_data_1 %>%
  rbind(model_data_important) %>%
  rbind(model_data_DPL) %>%
  filter(!variable %in% c(#"IMRreplen", 
                          "post_12","post_05"
         )) |> 
  mutate(model = factor(model, levels = c("US (Model 4)", "G5 (Model 5)", "US DPL (Model 8)")))

figure <-
  ggplot(m, aes(x = std.est, y = 20.3-id_2, group=model)) +
  geom_vline(xintercept = 0, colour = "#000000", linewidth = 0.5, alpha=.7, linetype=2) +  # Line at 0
  geom_pointrange(aes(xmin = xmin, xmax = xmax, fill=model, shape=model, linetype=model), fatten = 2, linewidth = 0.5, show.legend = T) +  # Ranges for each coefficient
  annotate("text", x = -4.8, y = seq(1:17)+1.2,
           label = rev(varnames),
           size=4, hjust = 1) +
  annotate("text", x = 5, y = 0.35, label = "Stringent", alpha=.7, size=3) +
  annotate("text", x = -2.5, y = 0.35, label = "Lenient", alpha=.7, size=3) +
  coord_cartesian(ylim=c(2,18), xlim=c(-4,5),clip="off") +
  labs(x="Standardized Estimate", y=NULL) +  # Labels
  theme_minimal() +
  theme(legend.position="right", legend.box = "vertical", legend.title = element_blank(),
        plot.margin=margin(t = 1, r = 1, b = 1, l = 5, unit = "cm"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(vjust = -4)
  )
#Will return error for DPL dummy for DPL only model, but that is okay.
figure

ggsave(plot=figure, "output/coef_plot.pdf", width=8, height=8)

sink()
