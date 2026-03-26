##############################
#
# Replication file for the appendix in:
#
# Reining in the Rascals: Challenger Parties' Path to Power
#
# For publication in the the Journal of Politics
#
# Frederik Hjorth, Jacob Nyrup, & Martin Vinæs Larsen
# 
##################

## Load packages ## ----

Sys.setlocale(category = "LC_ALL", locale = "")

pacman::p_load(here,tidyverse,openxlsx,stringr,rdrobust,interplot,patchwork,
               broom,GGally,ggeffects,estimatr,wesanderson,rddensity,ggthemes,margins,janitor,huxtable,
               rd.categorical,modelsummary,rio)

# Load helper functions

source("00-helperfunctions.r")

#### Load data ---

df <- read_rds("df_main.rds") %>% 
  mutate(coalition = ifelse(lead_chairmen >0,1,0),
         lag_coalition = ifelse(lag_sharechairmen >0,1,0),
         elected = ifelse(mandates_calculated > 0,1,0),
         lead_elected = ifelse(lead_mandates > 0,1,0),
         coalition_t3 =  ifelse(lead_lead_chairmen >0,1,0),
         coalition_t4 =  ifelse(lead_lead_lead_chairmen >0,1,0),
         lead_teknikudvalget = ifelse(lead_teknikudvalget > 0,1,0),
         lead_beskæftigelsesudvalget = ifelse(lead_beskæftigelsesudvalget > 0,1,0),
         lead_kulturudvalget = ifelse(lead_kulturudvalget > 0,1,0),
         lead_socialudvalget = ifelse(lead_socialudvalget > 0,1,0),
         lead_børneudvalget = ifelse(lead_børneudvalget > 0,1,0),
         andet = ifelse(andet > 0,1,0),
         mayor = ifelse(mayor_municipality == party,1,0),
         year1970 = if_else(year == 1970,1,0),
         year1974 = if_else(year == 1974,1,0),
         year1978 = if_else(year == 1978,1,0),
         year1981 = if_else(year == 1981,1,0),
         year1985 = if_else(year == 1985,1,0),
         year1989 = if_else(year == 1989,1,0),
         year1993 = if_else(year == 1993,1,0),
         year1997 = if_else(year == 1997,1,0),
         year2001 = if_else(year == 2001,1,0),
         year2005 = if_else(year == 2005,1,0),
         year2009 = if_else(year == 2009,1,0),
         year2013 = if_else(year == 2013,1,0),
         year2017 = if_else(year == 2017,1,0),
         )

df <- df %>% 
         group_by(municipality,party) %>% 
         arrange(year) %>%
         mutate(
         lead_share_chairmen = ifelse(dplyr::lead(year)-year < 5,lead(share_chairmen),0)
         )

df <- df %>% mutate(challenger_subset1 = 
                      case_when(party == "o" & year >= 1997 & year <= 2021 ~ 1,
                                party == "ø" & year >= 1993 & year <= 2021 ~ 1,
                                party == "d" & year >= 2017 & year <= 2017 ~ 1,
                                party == "e" & year >= 1970 & year <= 1989 ~ 1,
                                party == "g" & year >= 1989 & year <= 1989 ~ 1,
                                party == "i" & year >= 2009 & year <= 2013 ~ 1,
                                party == "h" & year >= 1989 & year <= 1989 ~ 1,
                                party == "k" & year >= 1970 & year <= 1985 ~ 1,
                                party == "m" & year >= 1974 & year <= 1981 ~ 1,
                                party == "p" & year >= 1989 & year <= 1989 ~ 1,
                                party == "q" & year >= 1974 & year <= 1981 ~ 1,
                                party == "y" & year >= 1970 & year <= 1985 ~ 1,
                                party == "z" & year >= 1973 & year <= 2001 ~ 1,
                                party == "å" & year >= 2017 & year <= 2017 ~ 1,
                                TRUE ~ 0),
                    challenger_subset2 = 
                      case_when(party == "o" & year >= 1997 & year <= 2021 ~ 1,
                                party == "ø" & year >= 1993 & year <= 2021 ~ 1,
                                party == "e" & year >= 1970 & year <= 1989 ~ 1,
                                party == "i" & year >= 2009 & year <= 2013 ~ 1,
                                party == "k" & year >= 1970 & year <= 1985 ~ 1,
                                party == "m" & year >= 1974 & year <= 1981 ~ 1,
                                party == "q" & year >= 1974 & year <= 1981 ~ 1,
                                party == "y" & year >= 1970 & year <= 1985 ~ 1,
                                party == "z" & year >= 1973 & year <= 2001 ~ 1,
                                TRUE ~ 0),
                    challenger_subset3 = 
                      case_when(
                        party == "m" & year >= 1974 & year <= 1981 ~ 1,
                        party == "q" & year >= 1974 & year <= 1981 ~ 1,
                        party == "z" & year >= 1973 & year <= 2001 ~ 1,
                        TRUE ~ 0)
)

###
# Appendix B: Other demarcations of challenger and dominant parties ---
###

# Group 1

df_subset1 <- df %>% dplyr::filter(challenger_subset1 == 1)

rd_subset1 <- rdd_full(df_subset1)

(out_g1 <- rd_subset1[1]) # Output regression

# Group 2

df_subset2 <- df %>% dplyr::filter(challenger_subset2 == 1)

rd_subset2 <- rdd_full(df_subset2)

(out_g2 <- rd_subset2[1]) # Output regression

# Group 3 - Throughout

rd_cdz <- df %>% dplyr::mutate(party = case_when(party == "k" & year < 2004 ~ "kommunist", # K is communist until 2004
                                                 party == "q" & year > 2004 ~ "other", # Q is Christian Democratic until 2004
                                                 party == "q" & year < 2004 ~ "k", # Change Q to K
                                                 TRUE ~ party)) %>%
  dplyr::filter((party %in% c("d") & year > 1989 & year < 2006) |
                  (party %in% c("m") & year < 1989) |
                  (party %in% c("k")) |
                  (party %in% c("z") & year < 2002)) %>% 
  rdd_full()

(out_g3_throughout <- rd_cdz[1]) # Output regression

# Group 3 - Before Firkløverregeringen

rd_cdz_before <-  df %>% dplyr::mutate(party = case_when(party == "k" & year < 2004 ~ "kommunist", # K is communist until 2004
                                                         party == "q" & year > 2004 ~ "other", # Q is Christian Democratc until 2004
                                                         party == "q" & year < 2004 ~ "k", # Change Q to K
                                                         TRUE ~ party)) %>%
  dplyr::filter((party %in% c("d") & year > 1989 & year < 2006) |
                  (party %in% c("m") & year < 1989) |
                  (party %in% c("k"))  |
                  (party %in% c("z") & year < 2002)) %>% 
  filter(year < 1984) %>% 
  rdd_full()

(out_g3_before <- rd_cdz_before[1]) # Output regression

# Group 3 - After Firkløverregeringen with Z

rd_cdz_after <-  df %>% dplyr::mutate(party = case_when(party == "k" & year < 2004 ~ "kommunist", # K is communist until 2004
                                                        party == "q" & year > 2004 ~ "other", # Q is Christian Democratc until 2004
                                                        party == "q" & year < 2004 ~ "k", # Change Q to K
                                                        TRUE ~ party)) %>%
  dplyr::filter((party %in% c("d") & year > 1989 & year < 2006) |
                  (party %in% c("m") & year < 1989) |
                  (party %in% c("k")) |
                  (party %in% c("z") & year < 2002)) %>% 
  filter(year > 1984) %>% 
  rdd_full()

(out_g3_after <- rd_cdz_after[1]) # Output regression

### Table ---- 

g1_tab <- out_g1 %>% 
  pluck(1) %>% 
  mutate(CI = paste0("[",round(conf.low,3),";",round(conf.high,3),"]"),
         party = "Group 1") %>%
  filter(term == "Robust") %>%
  dplyr::select(party,estimate,p.value,CI,bandwidth,nwithinbandwidth_control,nwithinbandwidth_treatment)

g2_tab <- out_g2 %>% 
  pluck(1) %>% 
  mutate(CI = paste0("[",round(conf.low,3),";",round(conf.high,3),"]"),
         party = "Group 2") %>%
  filter(term == "Robust") %>%
  dplyr::select(party,estimate,p.value,CI,bandwidth,nwithinbandwidth_control,nwithinbandwidth_treatment)

g3_througout_tab <- out_g3_throughout %>% 
  pluck(1) %>% 
  mutate(CI = paste0("[",round(conf.low,3),";",round(conf.high,3),"]"),
         party = "Group 3 (Whole period)") %>%
  filter(term == "Robust") %>%
  dplyr::select(party,estimate,p.value,CI,bandwidth,nwithinbandwidth_control,nwithinbandwidth_treatment)

g3_before_tab <- out_g3_before %>% 
  pluck(1) %>% 
  mutate(CI = paste0("[",round(conf.low,3),";",round(conf.high,3),"]"),
         party = "Group 3 (Before 1982)") %>%
  filter(term == "Robust") %>%
  dplyr::select(party,estimate,p.value,CI,bandwidth,nwithinbandwidth_control,nwithinbandwidth_treatment)

g3_after_tab <- out_g3_after %>% 
  pluck(1) %>% 
  mutate(CI = paste0("[",round(conf.low,3),";",round(conf.high,3),"]"),
         party = "Group 3 (After 1982)") %>%
  filter(term == "Robust") %>%
  dplyr::select(party,estimate,p.value,CI,bandwidth,nwithinbandwidth_control,nwithinbandwidth_treatment)

bind_tab <- rbind(g1_tab,g2_tab,g3_througout_tab,g3_before_tab,g3_after_tab)

rddtabcolnames <- c("Time period","Estimate","p-value","95% CI","h","Obs. control","Obs. treatment")

names(bind_tab) <- rddtabcolnames

bind_tab %>% 
  as_hux() %>% 
  set_bold(row=1, col=1:7, value=T) %>%
  set_top_border(row = 1,col=1:7,value=1) %>%
  set_bottom_border(row = 1,col=1:7,value=1) %>%
  set_bottom_border(row = 6,col=1:7,value=1) %>%
  set_label("rdtab_otherchallenger") %>% 
  set_caption("RD effect of being elected to city council at t-1 on being in coalition at t for the different demarcations of challenger parties") %>% 
  map_align(by_cols("left",rep("right",6))) %>% 
  add_footnote("Note: Running variable is party's margin to get represented in the city council in the last election, outcome is joining the coalition (dummy = 1) or not (dummy = 0) in this election. Estimate is the average treatment effect at the cutoff estimated with local linear regression with triangular kernel and MSE-optimal bandwidth. Column 3-7 report 95% heteroskedasticity-robust confidence intervals, heteroskedasticity-robust p-value, main optimal bandwidth, control observations within bandwidth, and treated observations within bandwidth.") %>%
  set_latex_float("htbp") %>% 
  set_width(.99) %>% 
  set_col_width(c(0.2, 0.1, 0.1, 0.2, 0.1, 0.17, 0.17)) %>%
  set_font_size(10) %>%
  to_latex() %>% 
  write_lines(path="output/appendix_b_tab2.tex")

### Figure: Counting Center Democrats + Christian Democrats as dominant ----

df_cdcddom <- df %>% dplyr::filter(party %in% c("b","c","f","a","v","d","q") & year > 1995)
rd_cdcddom <- rdd_full(df_cdcddom)
(out_cdcddom <- rd_cdcddom[1]) # Output regression
(plot_cdcddom <- rd_cdcddom[2]) # Output plot
ggsave("output/appendix_b_fig1a.pdf",width=4,height=4)

df_cdcddomsmall <- df %>% dplyr::filter(party %in% c("b","c","d","q") & year > 1995)
rd_cdcddomsmall <- rdd_full(df_cdcddomsmall) #,title = "Mainstream parties")
(out_cdcddomsmall <- rd_cdcddomsmall[1]) # Output regression
(plot_cdcddomsmall <- rd_cdcddomsmall[2]) # Output plot
ggsave("output/appendix_b_fig1b.pdf",width=4,height=4)

### Figure: Not counting SPP as dominant ----

df_sppnondom <- df %>% dplyr::filter(party %in% c("b","c","a","v") & year > 1995)
rd_sppnondom <- rdd_full(df_sppnondom)
(out_sppnondom <- rd_sppnondom[1]) # Output regression
(plot_sppnondom <- rd_sppnondom[2]) # Output plot
ggsave("output/appendix_b_fig2a.pdf",width=4,height=4)

df_sppnondomsmall <- df %>% dplyr::filter(party %in% c("b","c") & year > 1995)
rd_sppnondomsmall <- rdd_full(df_sppnondomsmall) #,title = "Mainstream parties")
(out_sppnondomsmall <- rd_sppnondomsmall[1]) # Output regression
(plot_sppnondomsmall <- rd_sppnondomsmall[2]) # Output plot
ggsave("output/appendix_b_fig2b.pdf",width=4,height=4)

###
# Appendix C: Distribution of type of chairs by party group ---
###

dfchair <- df %>% 
  dplyr::filter(party %in% c("o","b","c","v","a","f","ø") & mandates_calculated > 0 & year > 1995) %>%
  mutate(chal_kat = case_when(party %in% c("o","ø") ~ "Challenger",
                              party %in% c("a","b","c","v","f") ~ "Dominant")) %>%
  group_by(chal_kat) %>%
  summarize(chair_total = sum(chairmen_party),
            mayor_total = sum(mayor,na.rm=TRUE),
            zoning_total = sum(teknikudvalget,na.rm=TRUE),
            employment_total = sum(beskæftigelsesudvalget,na.rm=TRUE),
            culture_total = sum(kulturudvalget,na.rm=TRUE),
            socialaffairs_total = sum(socialudvalget,na.rm=TRUE),
            education_total = sum(børneudvalget,na.rm=TRUE),
            other_total = sum(andet,na.rm=TRUE),
  ) %>%
  mutate(`Mayor` = mayor_total/chair_total,
         `Zoning Committee` = zoning_total/chair_total,
         `Employment Committee` = employment_total/chair_total,
         `Committee of Culture` = culture_total/chair_total,
         `Committee of Social Affairs` = socialaffairs_total/chair_total,
         `Committee of Education` = education_total/chair_total,
         `Other Committee` = other_total/chair_total) %>%
  dplyr::select(1,10:16) %>%
  pivot_longer(cols = 2:8)

# Plot

order <- 
  dfchair %>% 
  filter(chal_kat == "Challenger") %>% 
  arrange(desc(-value)) %>% 
  mutate(name = factor(name))

dfchair <- dfchair %>% 
  mutate(name = factor(name, levels = order$name, ordered = TRUE),
         chal_kat = factor(chal_kat,levels = c("Dominant","Challenger")))

ggplot(dfchair,aes(x=name,y=value,group=chal_kat,fill=chal_kat)) +
  geom_col(position="dodge") +
  theme_bw() +
  labs(x="",y="Share of chairs by type (pct.)") +
  scale_y_continuous(labels = function(x) paste0(x*100, "%"), lim=c(0,0.5)) +
  coord_flip() +
  theme(legend.position="right",
        legend.title=element_blank()) +
  scale_fill_grey(start = 0.8, end = 0.2) +
  guides(fill = guide_legend(reverse=TRUE))

ggsave("Output/appendix_c.pdf",width=8,height=5)

###
# Appendix D: Descriptive statistics ---
###

Observations <- function(x) sum((!is.na(x)))

descstats <- datasummary(year + coalition + voteshare + share_threshold_gain + share_threshold_loss + 
                           lag_coalition + mandates_calculated + mandates_municipality + chairmen_party + chairmen_total + parties_running + population + area + shareimmigrants + serviceexpenses + taxes + nettoexpenses
                         ~ 
                           Observations + Mean*Arguments(fmt = "%.2f") + Median*Arguments(fmt = "%.2f") + SD*Arguments(na.rm=TRUE,fmt = "%.2f") + Min*Arguments(fmt = "%.2f") + Max*Arguments(fmt = "%.2f"),
                         data = df,
                         fmt=0,
                         title="Descriptive statistics",
                         output = 'huxtable'
)

descstats %>% .[,-1] %>% set_latex_float("htbp") %>% 
  insert_column(c("","Year","Coalition membership in t+1","Vote share","Dist. to threshold (gain)","Dist. to threshold (loss)","Coalition membership (t-1)",
                  "Mandates per party","Mandates in the municipality","Chairmen per party","Chairmen in the municipality",
                  "Number of parties running","Population","Area (sq km)","Share immigrants (pct.)",
                  "Operating expenses per person (DKK)","Expenses to service per person (DKK)","Average taxes per person (DKK)"),after = 0) %>%
  set_col_width(c(0.2, 0.08, 0.08, 0.08, 0.08, 0.08, 0.08)) %>%
  set_top_border(row = 1,col=1:7,value=1) %>%
  set_bottom_border(row = 1,col=1:7,value=1) %>%
  set_bottom_border(row = 18,col=1:7,value=1) %>%
  set_font_size(10) %>%
  to_latex() %>% 
  write_lines(path="Output/appendix_d.tex")

###
# Appendix F: Balance tests ---
###

### Table ---

## Year ---

# Challenger

rd_challenger_balance_year <- df %>% 
  dplyr::filter(party %in% c("o","ø") & year>1995) %>% 
  rdd_full_balance(.,depvar="year") %>% pluck(1) %>% 
  filter(term == "Robust") %>%
  mutate(group = "Challenger Parties",
         variable = "Year")

# Dominant

rd_all_balance_year <- df %>% 
  dplyr::filter(party %in% c("b","c","f","a","v") & year>1995) %>% 
  rdd_full_balance(.,depvar="year") %>% pluck(1) %>% 
  filter(term == "Robust") %>%
  mutate(group = "Dominant Parties",
         variable = "")

# Small Dominant

rd_dominant_balance_year <- df %>% 
  dplyr::filter(party %in% c("b","c") & year>1995) %>% 
  rdd_full_balance(.,depvar="year") %>% pluck(1) %>% 
  filter(term == "Robust") %>%
  mutate(group = "Small Dominant",
         variable = "")

## Lagged dependent ---

# Challenger

rd_challenger_balance_lagcoalition <- df %>% 
  mutate(lag_lag_coalition=ifelse(lag_lag_sharechairmen>0,1,0)) %>% 
  dplyr::filter(party %in% c("o","ø") & year>1995) %>% 
  rdd_full_balance(.,depvar="lag_coalition") %>% pluck(1) %>% 
  filter(term == "Robust") %>%
  mutate(group = "Challenger Parties",
         variable = "Previously in coalition")

# Dominant

rd_all_balance_lagcoalition <- df %>% 
  dplyr::filter(party %in% c("b","c","f","a","v") & year>1995) %>% 
  mutate(lag_lag_coalition=ifelse(lag_lag_sharechairmen>0,1,0)) %>% 
  rdd_full_balance(.,depvar="lag_coalition") %>% pluck(1) %>% 
  filter(term == "Robust") %>%
  mutate(group = "Dominant Parties",
         variable = "")

# Small Dominant

rd_dominant_balance_lagcoalition <- df %>% 
  mutate(lag_lag_coalition=ifelse(lag_lag_sharechairmen>0,1,0)) %>% 
  dplyr::filter(party %in% c("b","c") & year>1995) %>% 
  rdd_full_balance(.,depvar="lag_coalition") %>% pluck(1) %>% 
  filter(term == "Robust") %>%
  mutate(group = "Small Dominant",
         variable = "")

## Mandates in the municipality ---

# Challenger Parties

rd_challenger_balance_mandates <- df %>% 
  dplyr::filter(party %in% c("o","ø") & year>1995) %>% 
  rdd_full_balance(.,depvar="mandates_municipality") %>% pluck(1) %>% 
  filter(term == "Robust") %>%
  mutate(group = "Challenger Parties",
         variable = "Seats in the council")

# Dominant Parties

rd_all_balance_mandates <- df %>% 
  dplyr::filter(party %in% c("b","c","f","a","v") & year>1995) %>% 
  rdd_full_balance(.,depvar="mandates_municipality") %>% pluck(1) %>% 
  filter(term == "Robust") %>%
  mutate(group = "Dominant Parties",
         variable = "")

# Small Dominant

rd_dominant_balance_mandates <- df %>% 
  dplyr::filter(party %in% c("b","c") & year>1995) %>% 
  rdd_full_balance(.,depvar="mandates_municipality") %>% pluck(1) %>% 
  filter(term == "Robust") %>%
  mutate(group = "Small Dominant",
         variable = "")

## Number of Chairmen ---

# Challenger Parties

rd_challenger_balance_chairmen <- df %>% 
  dplyr::filter(party %in% c("o","ø") & year>1995) %>% 
  rdd_full_balance(.,depvar="chairmen_total") %>% pluck(1) %>% 
  filter(term == "Robust") %>%
  mutate(group = "Challenger Parties",
         variable = "Number of chairmen")

# Dominant Parties

rd_all_balance_chairmen <- df %>% 
  dplyr::filter(party %in% c("b","c","f","a","v") & year>1995) %>% 
  rdd_full_balance(.,depvar="chairmen_total") %>% pluck(1) %>% 
  filter(term == "Robust") %>%
  mutate(group = "Dominant Parties",
         variable = "")

# Small Dominant

rd_dominant_balance_chairmen <- df %>% 
  dplyr::filter(party %in% c("b","c") & year>1995) %>% 
  rdd_full_balance(.,depvar="chairmen_total") %>% pluck(1) %>% 
  filter(term == "Robust") %>%
  mutate(group = "Small Dominant",
         variable = "")

## Number of Parties ---

# Challenger Parties

rd_challenger_balance_parties <- df %>% 
  dplyr::filter(party %in% c("o","ø") & year>1995) %>% 
  rdd_full_balance(.,depvar="parties_running") %>% pluck(1) %>% 
  filter(term == "Robust") %>%
  mutate(group = "Challenger Parties",
         variable = "Number of parties")

# Dominant Parites

rd_all_balance_parties <- df %>% 
  dplyr::filter(party %in% c("b","c","f","a","v") & year>1995) %>% 
  rdd_full_balance(.,depvar="parties_running") %>% pluck(1) %>% 
  filter(term == "Robust") %>%
  mutate(group = "Dominant Parties",
         variable = "")

# Small Dominant

rd_dominant_balance_parties <- df %>% 
  dplyr::filter(party %in% c("b","c") & year>1995) %>% 
  rdd_full_balance(.,depvar="parties_running") %>% pluck(1) %>% 
  filter(term == "Robust") %>%
  mutate(group = "Small Dominant",
         variable = "")

## Population ---

# Challenger Parties

rd_challenger_balance_population <- df %>% 
  dplyr::filter(party %in% c("o","ø") & year>1995) %>% 
  rdd_full_balance(.,depvar="population") %>% pluck(1) %>% 
  filter(term == "Robust") %>%
  mutate(group = "Challenger Parties",
         variable = "Population")

# Dominant Parties

rd_all_balance_population <- df %>% 
  dplyr::filter(party %in% c("b","c","f","a","v") & year>1995) %>% 
  rdd_full_balance(.,depvar="population") %>% pluck(1) %>% 
  filter(term == "Robust") %>%
  mutate(group = "Dominant Parties ",
         variable = "")

# Small Dominant

rd_dominant_balance_population <- df %>% 
  dplyr::filter(party %in% c("b","c") & year>1995) %>% 
  rdd_full_balance(.,depvar="population") %>% pluck(1) %>% 
  filter(term == "Robust") %>%
  mutate(group = "Small Dominant",
         variable = "")

## Area ---

# Challenger Parties

rd_challenger_balance_area <- df %>% 
  dplyr::filter(party %in% c("o","ø") & year>1995) %>% 
  rdd_full_balance(.,depvar="area") %>% pluck(1) %>% 
  filter(term == "Robust") %>%
  mutate(group = "Challenger Parties",
         variable = "Area (sq km)")

# Dominant Parties

rd_all_balance_area <- df %>% 
  dplyr::filter(party %in% c("b","c","f","a","v") & year>1995) %>% 
  rdd_full_balance(.,depvar="area") %>% pluck(1) %>% 
  filter(term == "Robust") %>%
  mutate(group = "Dominant Parties",
         variable = "")

# Small Dominant

rd_dominant_balance_area <- df %>% 
  dplyr::filter(party %in% c("b","c") & year>1995) %>% 
  rdd_full_balance(.,depvar="area") %>% pluck(1) %>% 
  filter(term == "Robust") %>%
  mutate(group = "Small Dominant",
         variable = "")

## Share immigrants ---

# Challenger Parties

rd_challenger_balance_shareimmigrants <- df %>% 
  dplyr::filter(party %in% c("o","ø") & year>1995) %>% 
  rdd_full_balance(.,depvar="shareimmigrants") %>% pluck(1) %>% 
  filter(term == "Robust") %>%
  mutate(group = "Challenger Parties",
         variable = "Share immigrants (pct.)")

# Dominant Parties

rd_all_balance_shareimmigrants <- df %>% 
  dplyr::filter(party %in% c("b","c","f","a","v") & year>1995) %>% 
  rdd_full_balance(.,depvar="shareimmigrants") %>% pluck(1) %>% 
  filter(term == "Robust") %>%
  mutate(group = "Dominant Parties",
         variable = "")

# Small Dominant

rd_dominant_balance_shareimmigrants <- df %>% 
  dplyr::filter(party %in% c("b","c") & year>1995) %>% 
  rdd_full_balance(.,depvar="shareimmigrants") %>% pluck(1) %>% 
  filter(term == "Robust") %>%
  mutate(group = "Small Dominant",
         variable = "")

## Operating expenses ---

# Challenger Parties

rd_challenger_balance_nettoexpenses <- df %>% 
  dplyr::filter(party %in% c("o","ø") & year>1995) %>% 
  rdd_full_balance(.,depvar="nettoexpenses") %>% pluck(1) %>% 
  filter(term == "Robust") %>%
  mutate(group = "Challenger Parties",
         variable = "Operating expenses per person (DKK)")

# Dominant Parties

rd_all_balance_nettoexpenses <- df %>% 
  dplyr::filter(party %in% c("b","c","f","a","v") & year>1995) %>% 
  rdd_full_balance(.,depvar="nettoexpenses") %>% pluck(1) %>% 
  filter(term == "Robust") %>%
  mutate(group = "Dominant Parties",
         variable = "")

# Small Dominant

rd_dominant_balance_nettoexpenses <- df %>% 
  dplyr::filter(party %in% c("b","c") & year>1995) %>% 
  rdd_full_balance(.,depvar="nettoexpenses") %>% pluck(1) %>% 
  filter(term == "Robust") %>%
  mutate(group = "Small Dominant",
         variable = "")

## Expenses to services ---

# Challenger Parties

rd_challenger_balance_serviceexpenses <- df %>% 
  dplyr::filter(party %in% c("o","ø") & year>1995) %>% 
  rdd_full_balance(.,depvar="serviceexpenses") %>% pluck(1) %>% 
  filter(term == "Robust") %>%
  mutate(group = "Challenger Parties",
         variable = "Expenses to service per person (DKK)")

# Dominant parties

rd_all_balance_serviceexpenses <- df %>% 
  dplyr::filter(party %in% c("b","c","f","a","v") & year>1995) %>% 
  rdd_full_balance(.,depvar="serviceexpenses") %>% pluck(1) %>% 
  filter(term == "Robust") %>%
  mutate(group = "Dominant parties",
         variable = "")

# Small Dominant

rd_dominant_balance_serviceexpenses <- df %>% 
  dplyr::filter(party %in% c("b","c") & year>1995) %>% 
  rdd_full_balance(.,depvar="serviceexpenses") %>% pluck(1) %>% 
  filter(term == "Robust") %>%
  mutate(group = "Small Dominant",
         variable = "")

## Average taxes ---

# Challenger Parties

rd_challenger_balance_taxes <- df %>% 
  dplyr::filter(party %in% c("o","ø") & year>1995) %>% 
  rdd_full_balance(.,depvar="taxes") %>% pluck(1) %>% 
  filter(term == "Robust") %>%
  mutate(group = "Challenger Parties",
         variable = "Average taxes per person (DKK)")

# Dominant Parties

rd_all_balance_taxes <- df %>% 
  dplyr::filter(party %in% c("b","c","f","a","v") & year>1995) %>% 
  rdd_full_balance(.,depvar="taxes") %>% pluck(1) %>% 
  filter(term == "Robust") %>%
  mutate(group = "Dominant Parties",
         variable = "")

# Small Dominant

rd_dominant_balance_taxes <- df %>% 
  dplyr::filter(party %in% c("b","c") & year>1995) %>% 
  rdd_full_balance(.,depvar="taxes") %>% pluck(1) %>% 
  filter(term == "Robust") %>%
  mutate(group = "Small Dominant",
         variable = "")

# Write table

df_balance <- rbind(
  rd_challenger_balance_year,rd_all_balance_year,rd_dominant_balance_year,
  rd_challenger_balance_lagcoalition,rd_all_balance_lagcoalition,rd_dominant_balance_lagcoalition,
  rd_challenger_balance_mandates,rd_all_balance_mandates,rd_dominant_balance_mandates,
  rd_challenger_balance_chairmen,rd_all_balance_chairmen,rd_dominant_balance_chairmen,
  rd_challenger_balance_parties,rd_all_balance_parties,rd_dominant_balance_parties,
  rd_challenger_balance_population,rd_all_balance_population,rd_dominant_balance_population,
  rd_challenger_balance_area,rd_all_balance_area,rd_dominant_balance_area,
  rd_challenger_balance_shareimmigrants,rd_all_balance_shareimmigrants,rd_dominant_balance_shareimmigrants,
  rd_challenger_balance_nettoexpenses,rd_all_balance_nettoexpenses,rd_dominant_balance_nettoexpenses,
  rd_challenger_balance_serviceexpenses,rd_all_balance_serviceexpenses,rd_dominant_balance_serviceexpenses,
  rd_challenger_balance_taxes,rd_all_balance_taxes,rd_dominant_balance_taxes
) %>% 
  dplyr::select(variable, group, estimate, std.error, p.value, -conf.low, -conf.high, -statistic, -term)

balancecolnames <- c("Variable","Group","Estimate","Std.error","p-value")
names(df_balance) <- balancecolnames

df_balance %>% 
  as_hux() %>% 
  set_bold(row=1, col=1:5, value=T) %>%
  set_bold(row=1:34, col=1, value=T) %>%
  set_top_border(row = 1,col=1:5,value=1) %>%
  set_bottom_border(row = 1,col=1:5,value=1) %>%
  set_bottom_border(row = 34,col=1:5,value=1) %>%
  set_caption("RDD Balance Tests") %>% 
  set_label("rdtab_balance") %>% 
  set_align("right") %>% 
  set_font_size(9) %>% 
  set_latex_float("!htbp") %>% 
  set_width(0.85) %>% 
  set_top_padding(0) %>% 
  set_bottom_padding(0) %>% 
  set_left_padding(0) %>%
  set_right_padding(0) %>%
  set_number_format("%5.2f") %>%
  set_col_width(c(0.4, 0.2, 0.2, 0.2, 0.2)) %>%
  map_align(by_cols("left",rep("right",4))) %>% 
  to_latex() %>% 
  write_lines(path="output/appendix_f.tex")

###
# Appendix G: Influential parties only ---
###

rd_extreme_ssi <- df %>% 
  dplyr::filter(party %in% c("o","ø") & year>1995 & lead_ssi > 0) %>% 
  rdd_full()

plot_extreme_ssi <- rd_extreme_ssi[2]

rd_mainpost95_ssi <- df %>% 
  dplyr::filter(party %in% c("b","c") & year>1995 & lead_ssi > 0) %>% 
  rdd_full()

plot_mainpost95_ssi <- rd_mainpost95_ssi[2] # Output plot

plot_extreme_ssi[[1]] + plot_mainpost95_ssi[[1]]

ggsave("output/appendix_g.pdf",width=8,height=4)


###
# Appendix H: New parties ---
###

## Challenger parties (O and Ø)

rd_chal_notrepresentedprev <- df %>% 
  dplyr::filter(party %in% c("o","ø") & lagmandates %in% c(0,NA) & lagmandates_2 %in% c(0,NA) & year > 1995) %>% 
  rdd_full()

(out_chal_notrep <- rd_chal_notrepresentedprev[1]) # Output regression
(plot_chal_notrep <- rd_chal_notrepresentedprev[2]) # Output plot

ggsave("output/appendix_h_a.pdf",width=4,height=4)

# Dominant parties

## A, B, C, F and V without representation in T-2 and T-3Æ

rd_notrepresentedprev <- df %>% 
  dplyr::filter(party %in% c("b","c","f","a","v") & lagmandates %in% c(0,NA) & lagmandates_2 %in% c(0,NA) & year > 1995) %>% 
  rdd_full()

(out_notrep <- rd_notrepresentedprev[1]) # Output regression
(plot_notrep <- rd_notrepresentedprev[2]) # Output plot

ggsave("output/appendix_h_b.pdf",width=4,height=4)

###
# Appendix I: Not in coalition before ---
###

## Challenger parties (O and Ø) not in coalition in last election

nic_challenger <- df %>% 
  dplyr::filter(party %in% c("o","ø") & year > 1995 & (lag_coalition == 0 | is.na(lag_coalition))) %>%
  rdd_full(.)

(out_nic_challenger <- nic_challenger[1]) # Output regression
(plot_nic_challenger <- nic_challenger[2]) # Output plot

ggsave("output/appendix_i_a.pdf",width=4,height=4)

## Dominant parties: A, B, C, F and V not in coalition in last election

nic_dominant <- df %>% 
  dplyr::filter(party %in% c("b","c","f","a","v") & year > 1995 & (lag_coalition == 0 | is.na(lag_coalition))) %>%
  rdd_full(.)

(out_nic_dominant <- nic_dominant[1]) # Output regression
(plot_nic_dominant <- nic_dominant[2]) # Output plot

ggsave("output/appendix_i_b.pdf",width=4,height=4)

###
# Appendix J: Exploring alternative causal paths ---
###

df_retrospective <- read_rds("df_mainretroactive.rds") %>% 
  mutate(coalition = ifelse(chairmen_party >0,1,0))

### Retroactive test ----

# Graphs

### Extreme parties -----

df_challengers <- df_retrospective %>% dplyr::filter(party %in% c("o","ø") & year > 1995) 

rd_challengers <- rdd_full_retro(df_challengers) #,title = "Extreme parties")

(out_challengers <- rd_challengers[1]) # Output regression
(plot_challengers <- rd_challengers[2]) # Output plot

### Challenger parties post-1995 ----

df_dominantpost95 <- df_retrospective %>% dplyr::filter(party %in% c("b","c","f","a","v") & year>1995)

rd_dominantpost95 <- rdd_full_retro(df_dominantpost95)

(out_dominantpost95 <- rd_dominantpost95[1]) # Output regression
(plot_dominantpost95 <- rd_dominantpost95[2]) # Output plot

### Dominant parties post-1995 ----

df_mainpost95 <- df_retrospective %>% dplyr::filter(party %in% c("b","c") & year>1995)

rd_mainpost95 <- rdd_full_retro(df_mainpost95) #,title = "Mainstream parties")

(out_mainpost95 <- rd_mainpost95[1]) # Output regression
(plot_mainpost95 <- rd_mainpost95[2]) # Output plot

pluck(plot_challengers,1)

ggsave("output/appendix_j_fig1a.pdf",width=5,height=3)

pluck(plot_dominantpost95,1)

ggsave("output/appendix_j_fig1b.pdf",width=4,height=4)

pluck(plot_mainpost95,1)

ggsave("output/appendix_j_fig1c.pdf",width=4,height=4)

# Tabel ----

out_extreme_tab <- out_extreme %>% 
  pluck(1) %>% 
  mutate(CI = paste0("[",round(conf.low,3),";",round(conf.high,3),"]"),
         party = "Challenger") %>%
  filter(term == "Robust") %>%
  dplyr::select(party,estimate,p.value,CI,bandwidth,nwithinbandwidth_control,nwithinbandwidth_treatment)

out_nonex_tab <- out_nonexpost95 %>% 
  pluck(1) %>% 
  mutate(CI = paste0("[",round(conf.low,3),";",round(conf.high,3),"]"),
         party = "Dominant parties") %>%
  filter(term == "Robust") %>%
  dplyr::select(party,estimate,p.value,CI,bandwidth,nwithinbandwidth_control,nwithinbandwidth_treatment)

out_main_tab <- out_mainpost95 %>% 
  pluck(1) %>% 
  mutate(CI = paste0("[",round(conf.low,3),";",round(conf.high,3),"]"),
         party = "Small Dominant") %>%
  filter(term == "Robust") %>%
  dplyr::select(party,estimate,p.value,CI,bandwidth,nwithinbandwidth_control,nwithinbandwidth_treatment)

bind_tab <- rbind(out_extreme_tab,out_nonex_tab,out_main_tab)

rddtabcolnames <- c("Party group","Estimate","p-value","95% CI","h","Obs. control","Obs. treatment")

names(bind_tab) <- rddtabcolnames

bind_tab %>% 
  as_hux() %>% 
  set_bold(row=1, col=1:7, value=T) %>%
  set_top_border(row = 1,col=1:7,value=1) %>%
  set_bottom_border(row = 1,col=1:7,value=1) %>%
  set_bottom_border(row = 4,col=1:7,value=1) %>%
  set_label("rdtab_extreme") %>% 
  set_caption("RD effect of being elected to city council at t-1 on being in coalition at t for groups of parties") %>% 
  map_align(by_cols("left",rep("right",6))) %>% 
  add_footnote("Note: Running variable is party's margin to get represented in the city council in the last election, outcome is joining the coalition (dummy = 1) or not (dummy = 0) in this election. Estimate is the average treatment effect at the cutoff estimated with local linear regression with triangular kernel and MSE-optimal bandwidth. Column 3-7 report 95% heteroskedasticity-robust confidence intervals, heteroskedasticity-robust p-value, main optimal bandwidth, control observations within bandwidth, and treated observations within bandwidth.") %>%
  set_latex_float("htbp") %>% 
  set_width(.99) %>% 
  set_col_width(c(0.2, 0.1, 0.1, 0.2, 0.1, 0.17, 0.17)) %>%
  set_font_size(10) %>%
  to_latex() %>% 
  write_lines(path="output/appendix_j_tab1.tex")

### Test of alternative outcomes ---

## Mandates t+1 ----

## Extreme 

rd_challenger_balance_leadmandates <- df %>% 
  dplyr::filter(party %in% c("o","ø") & year>1995) %>% 
  rdd_full_balance(.,depvar="lead_mandates") %>% pluck(1) %>% 
  filter(term == "Robust") %>%
  mutate(group = "Challenger Parties",
         variable = "Mandates in t+1")

## Dominant 

rd_all_balance_leadmandates <- df %>% 
  dplyr::filter(party %in% c("b","c","f","a","v") & year>1995) %>% 
  rdd_full_balance(.,depvar="lead_mandates") %>% pluck(1) %>% 
  filter(term == "Robust") %>%
  mutate(group = "Dominant Parties",
         variable = "")

## Small Dominant 

rd_dominant_balance_leadmandates <- df %>% 
  dplyr::filter(party %in% c("b","c") & year>1995) %>% 
  rdd_full_balance(.,depvar="lead_mandates") %>% pluck(1) %>% 
  filter(term == "Robust") %>%
  mutate(group = "Small Dominant",
         variable = "")

## Share mandates t+1 ----

# Challenger

rd_challenger_balance_leadsharemandates <- df %>% 
  dplyr::filter(party %in% c("o","ø") & year>1995) %>% 
  rdd_full_balance(.,depvar="lead_sharemandates") %>% pluck(1) %>% 
  filter(term == "Robust") %>%
  mutate(group = "Challenger Parties",
         variable = "Share of mandates in t+1")

# Dominant

rd_all_balance_leadsharemandates <- df %>% 
  dplyr::filter(party %in% c("b","c","f","a","v") & year>1995) %>% 
  rdd_full_balance(.,depvar="lead_sharemandates") %>% pluck(1) %>% 
  filter(term == "Robust") %>%
  mutate(group = "Dominant Parties",
         variable = "")


# Small Dominant

rd_dominant_balance_leadsharemandates <- df %>% 
  dplyr::filter(party %in% c("b","c") & year>1995) %>% 
  rdd_full_balance(.,depvar="lead_sharemandates") %>% pluck(1) %>% 
  filter(term == "Robust") %>%
  mutate(group = "Small Dominant",
         variable = "")

## Elected t+1 ----

# Extreme

rd_challenger_balance_leadelected <- df %>% 
  dplyr::filter(party %in% c("o","ø") & year>1995) %>% 
  rdd_full_balance(.,depvar="lead_elected") %>% pluck(1) %>% 
  filter(term == "Robust") %>%
  mutate(group = "Challenger Parties",
         variable = "Elected in t+1")

# Dominant Parties

rd_all_balance_leadelected <- df %>% 
  dplyr::filter(party %in% c("b","c","f","a","v") & year>1995) %>% 
  rdd_full_balance(.,depvar="lead_elected") %>% pluck(1) %>% 
  filter(term == "Robust") %>%
  mutate(group = "Dominant Parties",
         variable = "")

# Small Dominant

rd_dominant_balance_leadelected <- df %>% 
  dplyr::filter(party %in% c("b","c") & year>1995) %>% 
  rdd_full_balance(.,depvar="lead_elected") %>% pluck(1) %>% 
  filter(term == "Robust") %>%
  mutate(group = "Small Dominant",
         variable = "")

## Vote share t+1 ---- 

# Extreme

rd_challenger_balance_leadvoteshare <- df %>% 
  dplyr::filter(party %in% c("o","ø") & year>1995) %>% 
  rdd_full_balance(.,depvar="lead_voteshare") %>% pluck(1) %>% 
  filter(term == "Robust") %>%
  mutate(group = "Challenger Parties",
         variable = "Vote share in t+1")

# Dominant

rd_all_balance_leadvoteshare <- df %>% 
  dplyr::filter(party %in% c("b","c","f","a","v") & year>1995) %>% 
  rdd_full_balance(.,depvar="lead_voteshare") %>% pluck(1) %>% 
  filter(term == "Robust") %>%
  mutate(group = "Dominant Parties",
         variable = "")

# Small Dominant

rd_dominant_balance_leadvoteshare <- df %>% 
  dplyr::filter(party %in% c("b","c") & year>1995) %>% 
  rdd_full_balance(.,depvar="lead_voteshare") %>% pluck(1) %>% 
  filter(term == "Robust") %>%
  mutate(group = "Small Dominant",
         variable = "")

## Votes t+1 ---

# Extreme

rd_challenger_balance_leadvotes <- df %>% 
  dplyr::filter(party %in% c("o","ø") & year>1995) %>% 
  rdd_full_balance(.,depvar="lead_votes") %>% pluck(1) %>% 
  filter(term == "Robust") %>%
  mutate(group = "Challenger Parties",
         variable = "Votes in t+1")

# Dominant

rd_all_balance_leadvotes <- df %>% 
  dplyr::filter(party %in% c("b","c","f","a","v") & year>1995) %>% 
  rdd_full_balance(.,depvar="lead_votes") %>% pluck(1) %>% 
  filter(term == "Robust") %>%
  mutate(group = "Dominant Parties",
         variable = "")

# Small Dominant

rd_dominant_balance_leadvotes <- df %>% 
  dplyr::filter(party %in% c("b","c") & year>1995) %>% 
  rdd_full_balance(.,depvar="lead_votes") %>% pluck(1) %>% 
  filter(term == "Robust") %>%
  mutate(group = "Small Dominant",
         variable = "")

## Runs in t+1 ---- xwxwxw

df_runsagain <- df %>% group_by(municipality,party) %>% arrange(year) %>% mutate(runsagain = ifelse(year-lead(year) > -5,lead(party,1),NA),
                                                                                 runsagain = ifelse(!is.na(runsagain),1,0)) %>% ungroup() 

# Extreme

rd_challenger_balance_runsagain <- df_runsagain %>% 
  dplyr::filter(party %in% c("o","ø") & year>1995) %>% 
  rdd_full_balance(.,depvar="runsagain") %>% pluck(1) %>% 
  filter(term == "Robust") %>%
  mutate(group = "Challenger Parties",
         variable = "Runs again in t+1")

# Dominant Parties

rd_all_balance_runsagain <- df_runsagain %>% 
  dplyr::filter(party %in% c("b","c","f","a","v") & year>1995) %>% 
  rdd_full_balance(.,depvar="runsagain") %>% pluck(1) %>% 
  filter(term == "Robust") %>%
  mutate(group = "Dominant Parties",
         variable = "")

# Small Dominant

rd_dominant_balance_runsagain <- df_runsagain %>% 
  dplyr::filter(party %in% c("b","c") & year>1995) %>% 
  rdd_full_balance(.,depvar="runsagain") %>% pluck(1) %>% 
  filter(term == "Robust") %>%
  mutate(group = "Small Dominant",
         variable = "")

## Electoral alliances t+1 ---- 

# Extreme

rd_challenger_balance_leadinelectoralalliance <- df %>% 
  dplyr::filter(party %in% c("o","ø") & year>1995) %>% 
  rdd_full_balance(.,depvar="lead_inelectoralalliance") %>% pluck(1) %>% 
  filter(term == "Robust") %>%
  mutate(group = "Challenger Parties",
         variable = "In electoral alliance in t+1")

# Dominant Parties

rd_all_balance_leadinelectoralalliance <- df %>% 
  dplyr::filter(party %in% c("b","c","f","a","v") & year>1995) %>% 
  rdd_full_balance(.,depvar="lead_inelectoralalliance") %>% pluck(1) %>% 
  filter(term == "Robust") %>%
  mutate(group = "Dominant Parties",
         variable = "")

# Small Dominant

rd_dominant_balance_leadinelectoralalliance <- df %>% 
  dplyr::filter(party %in% c("b","c") & year>1995) %>% 
  rdd_full_balance(.,depvar="lead_inelectoralalliance") %>% pluck(1) %>% 
  filter(term == "Robust") %>%
  mutate(group = "Small Dominant",
         variable = "")

## Number of parties running in t+1 ---- 

# Extreme

rd_challenger_balance_leadpartiesrunning <- df %>% 
  dplyr::filter(party %in% c("o","ø") & year>1995) %>% 
  rdd_full_balance(.,depvar="lead_parties_running") %>% pluck(1) %>% 
  filter(term == "Robust") %>%
  mutate(group = "Challenger Parties",
         variable = "Parties running in t+1")

# All

rd_all_balance_leadpartiesrunning <- df %>% 
  dplyr::filter(party %in% c("b","c","f","a","v") & year>1995) %>% 
  rdd_full_balance(.,depvar="lead_parties_running") %>% pluck(1) %>% 
  filter(term == "Robust") %>%
  mutate(group = "Dominant Parties",
         variable = "")

# Dominant

rd_dominant_balance_leadpartiesrunning <- df %>% 
  dplyr::filter(party %in% c("b","c") & year>1995) %>% 
  rdd_full_balance(.,depvar="lead_parties_running") %>% pluck(1) %>% 
  filter(term == "Robust") %>%
  mutate(group = "Small Dominant",
         variable = "")


# Print

df_alt <- rbind(
  rd_challenger_balance_leadmandates,rd_all_balance_leadmandates,rd_dominant_balance_leadmandates,
  rd_challenger_balance_leadsharemandates,rd_all_balance_leadsharemandates,rd_dominant_balance_leadsharemandates,
  rd_challenger_balance_leadelected,rd_all_balance_leadelected,rd_dominant_balance_leadelected,
  rd_challenger_balance_leadvoteshare,rd_all_balance_leadvoteshare,rd_dominant_balance_leadvoteshare,
  rd_challenger_balance_leadvotes,rd_all_balance_leadvotes,rd_dominant_balance_leadvotes,
  rd_challenger_balance_runsagain,rd_all_balance_runsagain,rd_dominant_balance_runsagain,
  rd_challenger_balance_leadinelectoralalliance,rd_all_balance_leadinelectoralalliance,rd_dominant_balance_leadinelectoralalliance,
  rd_challenger_balance_leadpartiesrunning,rd_all_balance_leadpartiesrunning,rd_dominant_balance_leadpartiesrunning) %>% 
  dplyr::select(variable, group, estimate, std.error, p.value, -conf.low, -conf.high, -statistic, -term)

balancecolnames <- c("Variable","Group","Estimate","Std.error","p-value")
names(df_alt) <- balancecolnames

df_alt %>% 
  as_hux() %>% 
  set_bold(row=1, col=1:5, value=T) %>%
  set_bold(row=1:25, col=1, value=T) %>%
  set_top_border(row = 1,col=1:5,value=1) %>%
  set_bottom_border(row = 1,col=1:5,value=1) %>%
  set_bottom_border(row = 25,col=1:5,value=1) %>%
  set_caption("RDD Alternative Paths") %>% 
  set_label("rdtab_altpath") %>% 
  set_align("right") %>% 
  set_font_size(9) %>% 
  set_latex_float("!htbp") %>% 
  set_width(0.85) %>% 
  set_top_padding(0) %>% 
  set_bottom_padding(0) %>% 
  set_left_padding(0) %>%
  set_right_padding(0) %>%
  #  set_number_format(c("","%5.3f","%5.2f","%5.2f","%5.2f")) %>%
  set_col_width(c(0.4, 0.2, 0.2, 0.2, 0.2)) %>%
  map_align(by_cols("left",rep("right",4))) %>% 
  to_latex() %>% 
  write_lines(path="output/appendix_j_tab2.tex")

###
# Appendix K: Effect by type of committee ---
###

rd_extreme_teknikudvalget <- df %>% 
  dplyr::filter(party %in% c("o","ø") & year > 1995) %>% 
  rdd_full_balance(.,depvar="lead_teknikudvalget",ylab="Prob. of getting the Zoning Committee")

rd_extreme_beskæftigelsesudvalget <- df %>% 
  dplyr::filter(party %in% c("o","ø") & year > 1995) %>% 
  rdd_full_balance(.,depvar="lead_beskæftigelsesudvalget",ylab="Prob. of getting the Employment Committee")

rd_extreme_kulturudvalget <- df %>% 
  dplyr::filter(party %in% c("o","ø") & year > 1995) %>% 
  rdd_full_balance(.,depvar="lead_kulturudvalget",ylab="Prob. of getting the Committee of Culture")

rd_extreme_socialudvalget <- df %>% 
  dplyr::filter(party %in% c("o","ø") & year > 1995) %>% 
  rdd_full_balance(.,depvar="lead_socialudvalget",ylab="Prob. of getting the Committee of Social Affairs")

rd_extreme_børneudvalget <- df %>% 
  dplyr::filter(party %in% c("o","ø") & year > 1995) %>% 
  rdd_full_balance(.,depvar="lead_børneudvalget",ylab="Prob. of getting the Committee of Education")

df_typeofchairman <- tibble(name = c("Zoning Committee","Employment Committee","Committee of Culture",
                                     "Committee of Social Affairs","Committee of Education"),
                            estimate = c(rd_extreme_teknikudvalget[[1]]$estimate[3],
                                         rd_extreme_beskæftigelsesudvalget[[1]]$estimate[3],
                                         rd_extreme_kulturudvalget[[1]]$estimate[3],
                                         rd_extreme_socialudvalget[[1]]$estimate[3],
                                         rd_extreme_børneudvalget[[1]]$estimate[3]),
                            se = c(rd_extreme_teknikudvalget[[1]]$std.error[3],
                                   rd_extreme_beskæftigelsesudvalget[[1]]$std.error[3],
                                   rd_extreme_kulturudvalget[[1]]$std.error[3],
                                   rd_extreme_socialudvalget[[1]]$std.error[3],
                                   rd_extreme_børneudvalget[[1]]$std.error[3]
                            ))


plot_type <- ggplot(df_typeofchairman, aes(x=reorder(name,estimate), y=estimate)) +
  geom_point() +
  theme_bw() + coord_flip() +
  geom_errorbar(aes(ymin=estimate-se*1.96, ymax=estimate+se*1.96), width=.2,
                position=position_dodge(.9)) + xlab("") + ylab("") +
  geom_hline(yintercept = 0, linetype="dotted",
             color = "black", size=1) + theme(panel.grid.major = element_blank(), 
                                              panel.grid.minor = element_blank(), 
                                              panel.border = element_blank(),
                                              text = element_text(size=15)) + ylab("Effect of incumbency status")

ggsave("output/appendix_k.pdf",plot_type,width=8,height=4)

###
# Appendix L: Effect by party ---
###

# Danish People's Party

results_o <- df %>% dplyr::filter(party %in% c("o") & year > 1995) %>%
  rdd_full(.)

# Unity List

results_ø <- df %>% dplyr::filter(party %in% c("ø") & year > 1995) %>%
  rdd_full(.)

# Social Liberals

results_b <- df %>% dplyr::filter(party %in% c("b") & year > 1995) %>%
  rdd_full(.)

# Conservative People's Party

results_c <- df %>% dplyr::filter(party %in% c("c") & year > 1995) %>%
  rdd_full(.)

# Socialist People's Party

results_f <- df %>% dplyr::filter(party %in% c("f") & year > 1995) %>%
  rdd_full(.)

# Other Parties

results_other <- df %>% dplyr::filter(party %!in% c("o","ø","b","c","f") & year > 1995) %>%
  rdd_full(.)

df_party <- tibble(name = c("Danish People's Party","Unity List","Social Liberals",
                            "Conservative People's Party","Socialist People's Party",
                            "Other"),
                   estimate = c(results_o[[1]]$estimate[3],
                                results_ø[[1]]$estimate[3],
                                results_b[[1]]$estimate[3],
                                results_c[[1]]$estimate[3],
                                results_f[[1]]$estimate[3],
                                results_other[[1]]$estimate[3]),
                   se = c(results_o[[1]]$std.error[3],
                          results_ø[[1]]$std.error[3],
                          results_b[[1]]$std.error[3],
                          results_c[[1]]$std.error[3],
                          results_f[[1]]$std.error[3],
                          results_other[[1]]$std.error[3]
                   ))

ggplot(df_party, aes(x=reorder(name,estimate), y=estimate)) +
  geom_point() +
  theme_bw() + coord_flip() +
  geom_errorbar(aes(ymin=estimate-se*1.96, ymax=estimate+se*1.96), width=.2,
                position=position_dodge(.9)) + xlab("") + ylab("") +
  geom_hline(yintercept = 0, linetype="dotted",
             color = "black", size=1) + theme(panel.grid.major = element_blank(), 
                                              panel.grid.minor = element_blank(), 
                                              panel.border = element_blank(),
                                              text = element_text(size=15)) + ylab("Effect of incumbency status")

ggsave("output/appendix_l.pdf",width=8,height=4)

###
# Appendix M: Additional effects of incumbency in later elections ---
###

rd_extreme_lead_2 <- df %>% 
  dplyr::filter(party %in% c("o","ø") & year > 1995) %>% 
  rdd_full_balance(.,depvar="lead_lead_chairmen",ylab="Prob. of joining coalition at t+2")

rd_extreme_lead_3 <- df %>% 
  dplyr::filter(party %in% c("o","ø") & year > 1995) %>% 
  rdd_full_balance(.,depvar="lead_lead_lead_chairmen",ylab="Prob. of joining coalition at t+3")

# Print

pluck(rd_extreme_lead_2,2)

ggsave("output/appendix_m_a.pdf",width=4,height=4)

pluck(rd_extreme_lead_3,2)

ggsave("output/appendix_m_b.pdf",width=4,height=4)

###
# Appendix N: Share of chairs as alternative dependent variable ---
###

# Challenger parties

rd_challenger_leadsharemandates <- df %>% 
  dplyr::filter(party %in% c("o","ø") & year > 1995) %>% 
  rdd_full_balance(.,depvar="lead_share_chairmen",ylab="Share of chairs at t+1",title = "",ylim=c(-0.05,0.1))

(out_challenger_leadsharemandates <- rd_challenger_leadsharemandates[1]) # Output regression
(plot_challenger_leadsharemandates <- rd_challenger_leadsharemandates[2]) # Output plot

### Dominant post-1995 ----

rd_dominant_leadsharemandates <- df %>% 
  dplyr::filter(party %in% c("b","c","f","a","v") & year > 1995) %>% 
  rdd_full_balance(.,depvar="lead_share_chairmen",ylab="Share of chairs at t+1",title = "",ylim=c(-0.05,0.1))

(out_dominant_leadsharemandates <- rd_dominant_leadsharemandates[1]) # Output regression
(plot_dominant_leadsharemandates <- rd_dominant_leadsharemandates[2]) # Output plot

### Small dominant post-1995 ----

rd_smalldominant_leadsharemandates <- df %>% 
  dplyr::filter(party %in% c("b","c") & year > 1995) %>% 
  rdd_full_balance(.,depvar="lead_share_chairmen",ylab="Share of chairs at t+1",title = "",ylim=c(-0.05,0.1))

(out_smalldominant_leadsharemandates <- rd_smalldominant_leadsharemandates[1]) # Output regression
(plot_smalldominant_leadsharemandates <- rd_smalldominant_leadsharemandates[2]) # Output plot

### Plot ----

pluck(plot_challenger_leadsharemandates,1)

ggsave("output/appendix_n_fig1a.pdf",width=5,height=3)

pluck(plot_dominant_leadsharemandates,1)

ggsave("output/appendix_n_fig1b.pdf",width=4,height=4)

pluck(plot_smalldominant_leadsharemandates,1)

ggsave("output/appendix_n_fig1c.pdf",width=4,height=4)

### RDD tables -----

out_extreme_tab <- rd_challenger_leadsharemandates %>% 
  pluck(1) %>% 
  mutate(CI = paste0("[",round(conf.low,3),";",round(conf.high,3),"]"),
         party = "Challenger") %>%
  filter(term == "Robust") %>%
  dplyr::select(party,estimate,p.value,CI,bandwidth,nwithinbandwidth_control,nwithinbandwidth_treatment)

out_nonex_tab <- rd_dominant_leadsharemandates %>% 
  pluck(1) %>% 
  mutate(CI = paste0("[",round(conf.low,3),";",round(conf.high,3),"]"),
         party = "Dominant") %>%
  filter(term == "Robust") %>%
  dplyr::select(party,estimate,p.value,CI,bandwidth,nwithinbandwidth_control,nwithinbandwidth_treatment)

out_main_tab <- rd_smalldominant_leadsharemandates %>% 
  pluck(1) %>% 
  mutate(CI = paste0("[",round(conf.low,3),";",round(conf.high,3),"]"),
         party = "Small dominant") %>%
  filter(term == "Robust") %>%
  dplyr::select(party,estimate,p.value,CI,bandwidth,nwithinbandwidth_control,nwithinbandwidth_treatment)

bind_tab <- rbind(out_extreme_tab,out_nonex_tab,out_main_tab)

rddtabcolnames <- c("Party group","Estimate","p-value","95% CI","h","Obs. control","Obs. treatment")

names(bind_tab) <- rddtabcolnames

bind_tab %>%  
  as_hux() %>% 
  set_bold(row=1, col=1:7, value=T) %>%
  set_top_border(row = 1,col=1:7,value=1) %>%
  set_bottom_border(row = 1,col=1:7,value=1) %>%
  set_bottom_border(row = 4,col=1:7,value=1) %>%
  set_label("rdtab_extreme") %>% 
  set_caption("RD effect of being elected to city council at t on being in coalition at t+1 for groups of parties") %>% 
  map_align(by_cols("left",rep("right",6))) %>% 
  add_footnote("Note: Running variable is party's margin to get represented in the city council, outcome is share of chairs in the following election. Estimate is the average treatment effect at the cutoff estimated with local linear regression with triangular kernel and MSE-optimal bandwidth. Column 3-7 report 95% heteroskedasticity-robust confidence intervals, heteroskedasticity-robust p-value, main optimal bandwidth, control observations within bandwidth, and treated observations within bandwidth.") %>%
  set_latex_float("htbp") %>% 
  set_width(.99) %>% 
  set_col_width(c(0.2, 0.1, 0.1, 0.2, 0.1, 0.17, 0.17)) %>%
  set_font_size(10) %>%
  to_latex() %>% 
  write_lines(path="output/appendix_n_tab.tex")

###
# Appendix O: Bandwidth tests ---
###

# Create df for challenger

df_challenger_placebo <- df  %>% 
  dplyr::filter(party %in% c("o","ø") & year>1995) %>%
  dplyr::filter(mandates_calculated < 2) %>%
  dplyr::mutate(X = ifelse(mandates_calculated==1,share_threshold_loss,-share_threshold_gain)) %>%
  dplyr::mutate(Z = as.factor(ifelse(X > 0,1,0))) %>% 
  dplyr::filter(!is.na(X)) %>% 
  dplyr::rename(Y = coalition)

# Create df for small dominant

df_dominant_placebo <- df %>% 
  dplyr::filter(party %in% c("b","c") & year>1995) %>% 
  dplyr::filter(mandates_calculated < 2) %>%
  dplyr::mutate(X = ifelse(mandates_calculated==1,share_threshold_loss,-share_threshold_gain)) %>%
  dplyr::mutate(Z = as.factor(ifelse(X > 0,1,0))) %>% 
  dplyr::filter(!is.na(X)) %>% 
  dplyr::rename(Y = coalition)

# Create df for all dominant

df_all_placebo <- df %>% 
  dplyr::filter(party %in% c("b","c","f","a","v") & year>1995) %>% 
  dplyr::filter(mandates_calculated < 2) %>%
  dplyr::mutate(X = ifelse(mandates_calculated==1,share_threshold_loss,-share_threshold_gain)) %>%
  dplyr::mutate(Z = as.factor(ifelse(X > 0,1,0))) %>%
  dplyr::filter(!is.na(X)) %>% 
  dplyr::rename(Y = coalition)

# Challenger

bandwidths_challenger <- tibble(bandwidth = c(seq(0.001,0.03,by=0.0001)),estimate = rep(NA,291),se = rep(NA,291))

for(i in 1:nrow(bandwidths_challenger)){
  reg_bandwidths <- rdrobust(df_challenger_placebo$Y,df_challenger_placebo$X, kernel = "triangular", p = 1, h = bandwidths_challenger$bandwidth[i], c=0)
  bandwidths_challenger$estimate[i] <- reg_bandwidths$coef[1]
  bandwidths_challenger$se[i] <- reg_bandwidths$se[1]
}

# Dominant

bandwidths_dominant <- tibble(bandwidth = c(seq(0.001,0.03,by=0.0001)),estimate = rep(NA,291),se = rep(NA,291))

for(i in 1:nrow(bandwidths_dominant)){
  reg_bandwidths <- rdrobust(df_dominant_placebo$Y,df_dominant_placebo$X, kernel = "triangular", p = 1, h = bandwidths_dominant$bandwidth[i], c=0)
  bandwidths_dominant$estimate[i] <- reg_bandwidths$coef[1]
  bandwidths_dominant$se[i] <- reg_bandwidths$se[1]
}

# All

bandwidths_all <- tibble(bandwidth = c(seq(0.001,0.03,by=0.0001)),estimate = rep(NA,291),se = rep(NA,291))

for(i in 1:nrow(bandwidths_dominant)){
  reg_bandwidths <- rdrobust(df_all_placebo$Y,df_all_placebo$X, kernel = "triangular", p = 1, h = bandwidths_all$bandwidth[i], c=0)
  bandwidths_all$estimate[i] <- reg_bandwidths$coef[1]
  bandwidths_all$se[i] <- reg_bandwidths$se[1]
}

rdrobust(df_all_placebo$Y,df_all_placebo$X, kernel = "triangular", p = 1, bwselect = "mserd", c=0)

canvas_bandwidths(bandwidths_challenger, bwused = 0.017)

ggsave("output/appendix_o_1.pdf",width=8,height=2.0)

canvas_bandwidths(bandwidths_all, bwused = 0.020)

ggsave("output/appendix_o_2.pdf",width=8,height=2.0)

canvas_bandwidths(bandwidths_dominant, bwused = 0.017)

ggsave("output/appendix_o_3.pdf",width=8,height=2.0)

###
# Appendix P: Placebo cut-offs ---
###

# Extreme

cutoffs_challenger <- tibble(cutoff = c(seq(-0.01,0.01,by=0.0001)),estimate = rep(NA,201),se = rep(NA,201))

for(i in 1:nrow(cutoffs_challenger)){
  reg_placebo <- rdrobust(df_challenger_placebo$Y,df_challenger_placebo$X, kernel = "triangular", p = 1, bwselect = "mserd",c=cutoffs_challenger$cutoff[i]) # Standard RD to be sure
  cutoffs_challenger$estimate[i] <- reg_placebo$coef[3]
  cutoffs_challenger$se[i] <- reg_placebo$se[3]
}

# Dominant

cutoffs_dominant <- tibble(cutoff = c(seq(-0.01,0.01,by=0.0001)),estimate = rep(NA,201),se = rep(NA,201))

for(i in 1:nrow(cutoffs_dominant)){
  reg_placebo <- rdrobust(df_dominant_placebo$Y,df_dominant_placebo$X, kernel = "triangular", p = 1, bwselect = "mserd",c=cutoffs_dominant$cutoff[i]) # Standard RD to be sure
  cutoffs_dominant$estimate[i] <- reg_placebo$coef[3]
  cutoffs_dominant$se[i] <- reg_placebo$se[3]
}

# All

cutoffs_all <- tibble(cutoff = c(seq(-0.01,0.01,by=0.0001)),estimate = rep(NA,201),se = rep(NA,201))

for(i in 1:nrow(cutoffs_all)){
  reg_placebo <- rdrobust(df_all_placebo$Y,df_all_placebo$X, kernel = "triangular", p = 1, bwselect = "mserd",c=cutoffs_all$cutoff[i]) # Standard RD to be sure
  cutoffs_all$estimate[i] <- reg_placebo$coef[3]
  cutoffs_all$se[i] <- reg_placebo$se[3]
}

# Calculate P-value

sharesig_challenger <- cutoffs_challenger %>% mutate(significant = case_when(abs(estimate/se) > 1.96 ~ 1,
                                                                             TRUE ~ 0)) %>% 
  summarize(n = n(), sumsig = sum(significant)) %>%
  mutate(sharesig = sumsig/n)

sharesig_dominant <- cutoffs_dominant %>% mutate(significant = case_when(abs(estimate/se) > 1.96 ~ 1,
                                                                         TRUE ~ 0)) %>% 
  summarize(n = n(), sumsig = sum(significant)) %>%
  mutate(sharesig = sumsig/n)


sharesig_all <- cutoffs_all %>% mutate(significant = case_when(abs(estimate/se) > 1.96 ~ 1,
                                                               TRUE ~ 0)) %>% 
  summarize(n = n(), sumsig = sum(significant)) %>%
  mutate(sharesig = sumsig/n)

# Print

canvas_placebo(cutoffs_challenger)

ggsave("output/appendix_p_1.pdf",width=8,height=2.0)

canvas_placebo(cutoffs_all)

ggsave("output/appendix_p_2.pdf",width=8,height=2.0)

canvas_placebo(cutoffs_dominant)

ggsave("output/appendix_p_3.pdf",width=8,height=2.0)

###
# Appendix Q: Using alternative specifications ---
####

### Polynomial ---

# 0

poly_0_challenger <- rd_wrapper_alt(data=df_challenger_placebo, p=0) %>% 
  mutate(CI = paste0("[",round(conf.low,3),";",round(conf.high,3),"]"),
         party = "Challenger Parties",
         `Polynomial order` = "0") %>%
  filter(term == "Robust") %>%
  dplyr::select(`Polynomial order`,party,estimate,p.value,CI,bandwidth,nwithinbandwidth_control,nwithinbandwidth_treatment)

poly_0_dominant <- rd_wrapper_alt(data=df_all_placebo, p=0) %>% 
  mutate(CI = paste0("[",round(conf.low,3),";",round(conf.high,3),"]"),
         party = "Dominant Parties",
         `Polynomial order` = "0") %>%
  filter(term == "Robust") %>%
  dplyr::select(`Polynomial order`,party,estimate,p.value,CI,bandwidth,nwithinbandwidth_control,nwithinbandwidth_treatment)

poly_0_all <- rd_wrapper_alt(data=df_dominant_placebo, p=0) %>% 
  mutate(CI = paste0("[",round(conf.low,3),";",round(conf.high,3),"]"),
         party = "Small Dominant",
         `Polynomial order` = "0") %>%
  filter(term == "Robust") %>%
  dplyr::select(`Polynomial order`,party,estimate,p.value,CI,bandwidth,nwithinbandwidth_control,nwithinbandwidth_treatment)

# 2

poly_2_challenger <- rd_wrapper_alt(data=df_challenger_placebo, p=2) %>% 
  mutate(CI = paste0("[",round(conf.low,3),";",round(conf.high,3),"]"),
         party = "Challenger Parties",
         `Polynomial order` = "2") %>%
  filter(term == "Robust") %>%
  dplyr::select(`Polynomial order`,party,estimate,p.value,CI,bandwidth,nwithinbandwidth_control,nwithinbandwidth_treatment)

poly_2_dominant <- rd_wrapper_alt(data=df_all_placebo, p=2) %>% 
  mutate(CI = paste0("[",round(conf.low,3),";",round(conf.high,3),"]"),
         party = "Dominant Parties",
         `Polynomial order` = "2") %>%
  filter(term == "Robust") %>%
  dplyr::select(`Polynomial order`,party,estimate,p.value,CI,bandwidth,nwithinbandwidth_control,nwithinbandwidth_treatment)

poly_2_all <- rd_wrapper_alt(data=df_dominant_placebo, p=2) %>% 
  mutate(CI = paste0("[",round(conf.low,3),";",round(conf.high,3),"]"),
         party = "Small Dominant",
         `Polynomial order` = "2") %>%
  filter(term == "Robust") %>%
  dplyr::select(`Polynomial order`,party,estimate,p.value,CI,bandwidth,nwithinbandwidth_control,nwithinbandwidth_treatment)

## Table ---

bind_tab_poly <- rbind(poly_0_challenger,poly_0_dominant,poly_0_all,
                       poly_2_challenger,poly_2_dominant,poly_2_all)

rddtabcolnames <- c("Polynomial order","Party Group","Estimate","p-value","95% CI","h","Obs. control","Obs. treatment")

names(bind_tab_poly) <- rddtabcolnames

bind_tab_poly %>% 
  as_hux() %>% 
  set_bold(row=1, col=1:8, value=T) %>%
  set_top_border(row = 1,col=1:8,value=1) %>%
  set_bottom_border(row = 1,col=1:8,value=1) %>%
  set_bottom_border(row = 7,col=1:8,value=1) %>%
  set_label("tab_poly") %>% 
  set_caption("RD effect of being elected to city council at t on being in coalition at t+1 for groups of parties") %>% 
  map_align(by_cols("left","left",rep("right",6))) %>% 
  add_footnote("Note: Running variable is party's margin to get represented in the city council, outcome is joining the coalition (dummy = 1) or not (dummy = 0) in the following election. Estimate is the average treatment effect at the cutoff estimated with the polynomial specified in polynomial order using triangular kernel and MSE-optimal bandwidth. Column 4-8 report 95% heteroskedasticity-robust confidence intervals, heteroskedasticity-robust p-value, main optimal bandwidth, control observations within bandwidth, and treated observations within bandwidth.") %>%
  set_latex_float("htbp") %>% 
  set_width(.99) %>% 
  set_col_width(c(0.1, 0.2, 0.1, 0.1, 0.15, 0.15, 0.15,0.15)) %>%
  set_font_size(8) %>%
  set_top_padding(0) %>% 
  set_bottom_padding(0) %>% 
  set_left_padding(0) %>%
  set_right_padding(0) %>%
  to_latex() %>% 
  write_lines(path="output/appendix_q_tab1.tex")

### Kernel ---

# Epanechnikov

kernel_epanechnikov_challenger <- rd_wrapper_alt(data=df_challenger_placebo, kernel_spec = "epanechnikov") %>% 
  mutate(CI = paste0("[",round(conf.low,3),";",round(conf.high,3),"]"),
         party = "Challenger Parties",
         kernel = "Epanechnikov") %>%
  filter(term == "Robust") %>%
  dplyr::select(kernel,party,estimate,p.value,CI,bandwidth,nwithinbandwidth_control,nwithinbandwidth_treatment)

kernel_epanechnikov_dominant <- rd_wrapper_alt(data=df_all_placebo, kernel_spec = "epanechnikov") %>% 
  mutate(CI = paste0("[",round(conf.low,3),";",round(conf.high,3),"]"),
         party = "Dominant Parties",
         kernel = "Epanechnikov") %>%
  filter(term == "Robust") %>%
  dplyr::select(kernel,party,estimate,p.value,CI,bandwidth,nwithinbandwidth_control,nwithinbandwidth_treatment)

kernel_epanechnikov_all <- rd_wrapper_alt(data=df_dominant_placebo, kernel_spec = "epanechnikov") %>% 
  mutate(CI = paste0("[",round(conf.low,3),";",round(conf.high,3),"]"),
         party = "Small Dominant",
         kernel = "Epanechnikov") %>%
  filter(term == "Robust") %>%
  dplyr::select(kernel,party,estimate,p.value,CI,bandwidth,nwithinbandwidth_control,nwithinbandwidth_treatment)

# Uniform

kernel_uniform_challenger <- rd_wrapper_alt(data=df_challenger_placebo, kernel_spec = "uniform") %>% 
  mutate(CI = paste0("[",round(conf.low,3),";",round(conf.high,3),"]"),
         party = "Challenger Parties",
         kernel = "Uniform") %>%
  filter(term == "Robust") %>%
  dplyr::select(kernel,party,estimate,p.value,CI,bandwidth,nwithinbandwidth_control,nwithinbandwidth_treatment)

kernel_uniform_dominant <- rd_wrapper_alt(data=df_dominant_placebo, kernel_spec = "uniform") %>% 
  mutate(CI = paste0("[",round(conf.low,3),";",round(conf.high,3),"]"),
         party = "Dominant Parties",
         kernel = "Uniform") %>%
  filter(term == "Robust") %>%
  dplyr::select(kernel,party,estimate,p.value,CI,bandwidth,nwithinbandwidth_control,nwithinbandwidth_treatment)

kernel_uniform_all <- rd_wrapper_alt(data=df_all_placebo, kernel_spec = "uniform") %>% 
  mutate(CI = paste0("[",round(conf.low,3),";",round(conf.high,3),"]"),
         party = "Small Dominant",
         kernel = "Uniform") %>%
  filter(term == "Robust") %>%
  dplyr::select(kernel,party,estimate,p.value,CI,bandwidth,nwithinbandwidth_control,nwithinbandwidth_treatment)

## Table ---

bind_tab_kernel <- rbind(kernel_epanechnikov_challenger,kernel_epanechnikov_dominant,kernel_epanechnikov_all,
                         kernel_uniform_challenger,kernel_uniform_dominant,kernel_uniform_all)

rddtabcolnames_kernel <- c("Kernel","Party Group","Estimate","p-value","95% CI","h","Obs. control","Obs. treatment")

names(bind_tab_kernel) <- rddtabcolnames_kernel

bind_tab_kernel %>% 
  as_hux() %>% 
  set_bold(row=1, col=1:8, value=T) %>%
  set_top_border(row = 1,col=1:8,value=1) %>%
  set_bottom_border(row = 1,col=1:8,value=1) %>%
  set_bottom_border(row = 7,col=1:8,value=1) %>%
  set_label("tab_kernel") %>% 
  set_caption("RD effect of being elected to city council at t on being in coalition at t+1 for groups of parties") %>% 
  map_align(by_cols("left","left",rep("right",6))) %>% 
  add_footnote("Note: Running variable is party's margin to get represented in the city council, outcome is joining the coalition (dummy = 1) or not (dummy = 0) in the following election. Estimate is the average treatment effect at the cutoff estimated with the polynomial specified in polynomial order using triangular kernel and MSE-optimal bandwidth. Column 4-8 report 95% heteroskedasticity-robust confidence intervals, heteroskedasticity-robust p-value, main optimal bandwidth, control observations within bandwidth, and treated observations within bandwidth.") %>%
  set_latex_float("htbp") %>% 
  set_width(.99) %>% 
  set_col_width(c(0.11, 0.2, 0.1, 0.1, 0.15, 0.15, 0.15,0.15)) %>%
  set_font_size(8) %>%
  set_top_padding(0) %>% 
  set_bottom_padding(0) %>% 
  set_left_padding(0) %>%
  set_right_padding(0) %>%
  to_latex() %>% 
  write_lines(path="output/appendix_q_tab2.tex")

###
# Appendix R: Including control variables ---
###

z_1 <- cbind(df_challenger_placebo$year1997,df_challenger_placebo$year2001,df_challenger_placebo$year2005,
             df_challenger_placebo$year2009)

z_2 <- cbind(df_challenger_placebo$mandates_municipality, df_challenger_placebo$chairmen_total, df_challenger_placebo$parties_running)

z_3 <- cbind(df_challenger_placebo$population,df_challenger_placebo$area,df_challenger_placebo$shareimmigrants,df_challenger_placebo$nettoexpenses,
             df_challenger_placebo$serviceexpenses,df_challenger_placebo$taxes)

z_4 <- cbind(df_challenger_placebo$lead_mandates,df_challenger_placebo$lead_sharemandates,df_challenger_placebo$lead_elected,
             df_challenger_placebo$lead_voteshare,df_challenger_placebo$lead_votes,df_challenger_placebo$lead_inelectoralalliance,
             df_challenger_placebo$lead_parties_running)

## Challenger

# Model 1 - Year

m_1_challenger <- rd_wrapper_control(data=df_challenger_placebo, control = z_1) %>%   
  mutate(CI = paste0("[",round(conf.low,3),";",round(conf.high,3),"]"),
         party = "Challenger Parties",
         model = "Year") %>%
  filter(term == "Robust") %>%
  dplyr::select(model,party,estimate,p.value,CI,bandwidth,nwithinbandwidth_control,nwithinbandwidth_treatment)

# Model 2 - Electoral variables

m_2_challenger <- rd_wrapper_control(data=df_challenger_placebo, control = z_2) %>%   
  mutate(CI = paste0("[",round(conf.low,3),";",round(conf.high,3),"]"),
         party = "Challenger Parties",
         model = "Electoral variables") %>%
  filter(term == "Robust") %>%
  dplyr::select(model,party,estimate,p.value,CI,bandwidth,nwithinbandwidth_control,nwithinbandwidth_treatment)

# Model 3 - Background variables

m_3_challenger <- rd_wrapper_control(data=df_challenger_placebo, control = z_3) %>%   
  mutate(CI = paste0("[",round(conf.low,3),";",round(conf.high,3),"]"),
         party = "Challenger Parties",
         model = "Background variables") %>%
  filter(term == "Robust") %>%
  dplyr::select(model,party,estimate,p.value,CI,bandwidth,nwithinbandwidth_control,nwithinbandwidth_treatment)

# Model 4 - Lead variables

m_4_challenger <- rd_wrapper_control(data=df_challenger_placebo, control = z_4) %>%   
  mutate(CI = paste0("[",round(conf.low,3),";",round(conf.high,3),"]"),
         party = "Challenger Parties",
         model = "Lead variables") %>%
  filter(term == "Robust") %>%
  dplyr::select(model,party,estimate,p.value,CI,bandwidth,nwithinbandwidth_control,nwithinbandwidth_treatment)

## Dominant

z_1 <- cbind(df_all_placebo$year1997,df_all_placebo$year2001,df_all_placebo$year2005,
             df_all_placebo$year2009)

z_2 <- cbind(df_all_placebo$mandates_municipality, df_all_placebo$chairmen_total, df_all_placebo$parties_running)

z_3 <- cbind(df_all_placebo$population,df_all_placebo$area,df_all_placebo$shareimmigrants,df_all_placebo$nettoexpenses,
             df_all_placebo$serviceexpenses,df_all_placebo$taxes)

z_4 <- cbind(df_all_placebo$lead_mandates,df_all_placebo$lead_sharemandates,df_all_placebo$lead_elected,
             df_all_placebo$lead_voteshare,df_all_placebo$lead_votes,df_all_placebo$lead_inelectoralalliance,
             df_all_placebo$lead_parties_running)

## Challenger

# Model 1 - Year

m_1_dominant <- rd_wrapper_control(data=df_all_placebo, control = z_1) %>%   
  mutate(CI = paste0("[",round(conf.low,3),";",round(conf.high,3),"]"),
         party = "Dominant Parties",
         model = "Year") %>%
  filter(term == "Robust") %>%
  dplyr::select(model,party,estimate,p.value,CI,bandwidth,nwithinbandwidth_control,nwithinbandwidth_treatment)

# Model 2 - Electoral variables

m_2_dominant <- rd_wrapper_control(data=df_all_placebo, control = z_2) %>%   
  mutate(CI = paste0("[",round(conf.low,3),";",round(conf.high,3),"]"),
         party = "Dominant Parties",
         model = "Electoral variables") %>%
  filter(term == "Robust") %>%
  dplyr::select(model,party,estimate,p.value,CI,bandwidth,nwithinbandwidth_control,nwithinbandwidth_treatment)

# Model 3 - Background variables

m_3_dominant <- rd_wrapper_control(data=df_all_placebo, control = z_3) %>%   
  mutate(CI = paste0("[",round(conf.low,3),";",round(conf.high,3),"]"),
         party = "Dominant Parties",
         model = "Background variables") %>%
  filter(term == "Robust") %>%
  dplyr::select(model,party,estimate,p.value,CI,bandwidth,nwithinbandwidth_control,nwithinbandwidth_treatment)

# Model 4 - Lead variables

m_4_dominant <- rd_wrapper_control(data=df_all_placebo, control = z_4) %>%   
  mutate(CI = paste0("[",round(conf.low,3),";",round(conf.high,3),"]"),
         party = "Dominant Parties",
         model = "Lead variables") %>%
  filter(term == "Robust") %>%
  dplyr::select(model,party,estimate,p.value,CI,bandwidth,nwithinbandwidth_control,nwithinbandwidth_treatment)

## Small dominant

z_1 <- cbind(df_dominant_placebo$year1997,df_dominant_placebo$year2001,df_dominant_placebo$year2005,
             df_dominant_placebo$year2009)

z_2 <- cbind(df_dominant_placebo$mandates_municipality, df_dominant_placebo$chairmen_total, df_dominant_placebo$parties_running)

z_3 <- cbind(df_dominant_placebo$population,df_dominant_placebo$area,df_dominant_placebo$shareimmigrants,df_dominant_placebo$nettoexpenses,
             df_dominant_placebo$serviceexpenses,df_dominant_placebo$taxes)

z_4 <- cbind(df_dominant_placebo$lead_mandates,df_dominant_placebo$lead_sharemandates,df_dominant_placebo$lead_elected,
             df_dominant_placebo$lead_voteshare,df_dominant_placebo$lead_votes,df_dominant_placebo$lead_inelectoralalliance,
             df_dominant_placebo$lead_parties_running)

## Challenger

# Model 1 - Year

m_1_smalldominant <- rd_wrapper_control(data=df_dominant_placebo, control = z_1) %>%   
  mutate(CI = paste0("[",round(conf.low,3),";",round(conf.high,3),"]"),
         party = "Small Dominant",
         model = "Year") %>%
  filter(term == "Robust") %>%
  dplyr::select(model,party,estimate,p.value,CI,bandwidth,nwithinbandwidth_control,nwithinbandwidth_treatment)

# Model 2 - Electoral variables

m_2_smalldominant <- rd_wrapper_control(data=df_dominant_placebo, control = z_2) %>%   
  mutate(CI = paste0("[",round(conf.low,3),";",round(conf.high,3),"]"),
         party = "Small Dominant",
         model = "Electoral variables") %>%
  filter(term == "Robust") %>%
  dplyr::select(model,party,estimate,p.value,CI,bandwidth,nwithinbandwidth_control,nwithinbandwidth_treatment)

# Model 3 - Background variables

m_3_smalldominant <- rd_wrapper_control(data=df_dominant_placebo, control = z_3) %>%   
  mutate(CI = paste0("[",round(conf.low,3),";",round(conf.high,3),"]"),
         party = "Small Dominant",
         model = "Background variables") %>%
  filter(term == "Robust") %>%
  dplyr::select(model,party,estimate,p.value,CI,bandwidth,nwithinbandwidth_control,nwithinbandwidth_treatment)

# Model 4 - Lead variables

m_4_smalldominant <- rd_wrapper_control(data=df_dominant_placebo, control = z_4) %>%   
  mutate(CI = paste0("[",round(conf.low,3),";",round(conf.high,3),"]"),
         party = "Small Dominant",
         model = "Lead variables") %>%
  filter(term == "Robust") %>%
  dplyr::select(model,party,estimate,p.value,CI,bandwidth,nwithinbandwidth_control,nwithinbandwidth_treatment)

## Table ---

bind_tab_control <- rbind(m_1_challenger,m_2_challenger,m_3_challenger,m_4_challenger,
                          m_1_dominant,m_2_dominant,m_3_dominant,m_4_dominant,
                          m_1_smalldominant,m_2_smalldominant,m_3_smalldominant,m_4_smalldominant)

rddtabcolnames_control <- c("Controls","Party Group","Estimate","p-value","95% CI","h","Obs. control","Obs. treatment")

names(bind_tab_control) <- rddtabcolnames_control

bind_tab_control %>% 
  as_hux() %>% 
  set_bold(row=1, col=1:8, value=T) %>%
  set_top_border(row = 1,col=1:8,value=1) %>%
  set_bottom_border(row = 1,col=1:8,value=1) %>%
  set_bottom_border(row = 13,col=1:8,value=1) %>%
  set_label("tab_kernel") %>% 
  set_caption("RD effect of being elected to city council at t on being in coalition at t+1 for groups of parties") %>% 
  map_align(by_cols("left","left",rep("right",6))) %>% 
  add_footnote("Note: Running variable is party's margin to get represented in the city council, outcome is joining the coalition (dummy = 1) or not (dummy = 0) in the following election. Estimate is the average treatment effect at the cutoff estimated with the polynomial specified in polynomial order using triangular kernel and MSE-optimal bandwidth. Column 4-8 report 95% heteroskedasticity-robust confidence intervals, heteroskedasticity-robust p-value, main optimal bandwidth, control observations within bandwidth, and treated observations within bandwidth.") %>%
  set_latex_float("htbp") %>% 
  set_width(.99) %>% 
  set_col_width(c(0.15, 0.17, 0.1, 0.1, 0.15, 0.13, 0.13,0.13)) %>%
  set_font_size(8) %>%
  set_top_padding(0) %>% 
  set_bottom_padding(0) %>% 
  set_left_padding(0) %>%
  set_right_padding(0) %>%
  to_latex() %>% 
  write_lines(path="output/appendix_r.tex")

###
# Appendix T: Validation of VAA party position estimates ---
###

ches2019dk <- import("https://www.chesdata.eu/s/CHES2019V3.csv") %>% 
  as_tibble() %>% 
  filter(country==2) %>% 
  mutate(partyletter=toupper(party))

irtests <- read_rds("irtposbyparty.rds") %>% 
  mutate(partyletter=c("A","DF","KF","EL","KD","LA","NB","RV",NA,"SD","SF","V"))

chirt <- ches2019dk %>% 
  dplyr::select(.,partyletter,lrgen) %>% 
  left_join(irtests,by="partyletter")

ggplot(chirt,aes(lrgen,posmean,label=party)) +
  geom_smooth(method="lm",alpha=0,linetype=2,size=.5,color=1) +
  geom_point() +
  ggrepel::geom_text_repel() +
  theme_bw() +
  labs(x="CHES General L/R Estimate",y="IRT Position Estimate")

ggsave("Output/appendix_t.pdf",width=7,height=4)

lm(posmean~lrgen,data=chirt) %>% summary() %>% pluck("r.squared") #r^2 = .97!
