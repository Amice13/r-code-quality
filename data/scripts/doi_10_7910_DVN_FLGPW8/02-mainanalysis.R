##############################
#
# Replication file for the main analysis in:
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

pacman::p_load(here,tidyverse,rdrobust,broom,GGally,estimatr,rddensity,ggthemes,huxtable,rsample)

# Load helper functions

source("00-helperfunctions.r")

#### Load data ---

df <- read_rds("df_main.rds") %>%
  mutate(coalition = ifelse(lead_chairmen >0,1,0))

###
# Figure 3 ---
###

pletters <- c("o","b","c","v","a","f","ø")
yearnums <- seq(1997,2017,by=4)

pnames <- c("Danish People's Party",
            "Social Liberals",
            "Conservatives",
            "Liberals",
            "Social Democrats",
            "Socialist People's Party",
            "Unity List")

dfex <- df %>% 
  dplyr::filter(party %in% c("o","b","c","v","a","f","ø") & mandates_calculated > 0 & year > 1995) %>% 
  group_by(year,party) %>%
  summarize(mandates_per_year = sum(chairmen_party)/sum(mandates_calculated)) %>% 
  ungroup() %>% 
  mutate(yearfac=fct_inorder(factor(year))) %>% 
  mutate(extreme=factor(ifelse(party %in% c("o","ø"),1,0)))

ggplot(dfex,aes(x=yearfac,y=mandates_per_year,group=party,color=extreme)) +
  geom_line() +
  geom_point() +
  theme_bw() + 
  xlab("") + 
  ylab("Chairs per seat") +
  theme(legend.position = "none") +
  scale_color_manual(values=c("grey70","grey20")) +
  annotate("text",x=5.8,y=.1,label="Red/Green Alliance",size=3) +
  annotate("text",x=5.9,y=.18,label="Danish People's Party",size=3) +
  annotate("text",x=5.7,y=.4,label="The Social Liberal Party",size=3) +
  annotate("text",x=4.0,y=.2,label="The Liberal Party",size=3) +
  annotate("text",x=5.95,y=.27,label="Conservative Party",size=3) +
  annotate("text",x=5.0,y=0.32,label="Socialist People’s Party",size=3) +
  annotate("text",x=5.8,y=0.30,label="The Social Democrats",size=3)

ggsave("output/fig3_a.pdf",width=6,height=6)

extchairs <- df %>% 
  filter(party %in% c("o","ø") & year>1995) %>% 
  group_by(year) %>% 
  summarise(partychairsum=sum(chairmen_party,na.rm=T),
            chairsum=sum(chairmen_total,na.rm=T)) %>% 
  mutate(chairpct=100*partychairsum/chairsum,
         yearfac=fct_inorder(factor(year)))

ggplot(extchairs,aes(x=yearfac,y=chairpct)) +
  geom_col() +
  theme_bw() +
  labs(x="",y="Share of chairs held by challenger parties (pct.)") +
  scale_y_continuous(labels = function(x) paste0(x, "%"), lim=c(0,5))

ggsave("output/fig3_b.pdf",width=6,height=6)

###
# Figure 4 ---
###

df_challenger_placebo <- df  %>% 
  dplyr::filter(party %in% c("o","ø") & year>1995) %>%
  dplyr::filter(mandates_calculated < 2) %>%
  dplyr::mutate(X = ifelse(mandates_calculated==1,share_threshold_loss,-share_threshold_gain)) %>% # X is the running variable
  dplyr::mutate(Z = as.factor(ifelse(X > 0,1,0))) %>% # Z is the cutoff
  dplyr::filter(!is.na(X)) %>% 
  dplyr::rename(Y = coalition)

df_dominant_placebo <- df %>% 
  dplyr::filter(party %in% c("b","c") & year>1995) %>% 
  dplyr::filter(mandates_calculated < 2) %>%
  dplyr::mutate(X = ifelse(mandates_calculated==1,share_threshold_loss,-share_threshold_gain)) %>% # X is the running variable
  dplyr::mutate(Z = as.factor(ifelse(X > 0,1,0))) %>% # Z is the cutoff
  dplyr::filter(!is.na(X)) %>% 
  dplyr::rename(Y = coalition)

df_all_placebo <- df %>% 
  dplyr::filter(party %in% c("b","c","f","a","v") & year>1995) %>% 
  dplyr::filter(mandates_calculated < 2) %>%
  dplyr::mutate(X = ifelse(mandates_calculated==1,share_threshold_loss,-share_threshold_gain)) %>% # X is the running variable
  dplyr::mutate(Z = as.factor(ifelse(X > 0,1,0))) %>% # Z is the cutoff
  dplyr::filter(!is.na(X)) %>% 
  dplyr::rename(Y = coalition)

# Challenger

dens_challenger <- rddensity(df_challenger_placebo$X, c = 0, p = 1)
dens_plot_all <- rdplotdensity(dens_challenger,df_challenger_placebo$X, plotN = 30,type="both",xlab="Score relative to threshold",ylab="Density",plotRange = c(-0.04, 0.04),
                               CIcol = 1, lcol = 1, histFillCol = 1)

ggsave("Output/fig4_a.pdf",width=4,height=4)

# Dominant parties

dens_all <- rddensity(df_all_placebo$X, c = 0, p = 1)
dens_plot_all <- rdplotdensity(dens_all,df_all_placebo$X, plotN = 30,type="both",xlab="Score relative to threshold",ylab="Density",title="Dominant Parties",plotRange = c(-0.04, 0.04),
                               CIcol = 1, lcol = 1, histFillCol = 1)

ggsave("Output/fig4_b.pdf",width=4,height=4)

# Dominant

dens_dominant <- rddensity(df_dominant_placebo$X, c = 0, p = 1)
dens_plot_all <- rdplotdensity(dens_dominant,df_dominant_placebo$X, plotN = 30,type="both",xlab="Score relative to threshold",ylab="Density",plotRange = c(-0.04, 0.04),
                               CIcol = 1, lcol = 1, histFillCol = 1)

ggsave("Output/fig4_c.pdf",width=4,height=4)

###
# Figure 5 ---
###

### Challenger ----

df_extreme <- df %>% dplyr::filter(party %in% c("o","ø") & year > 1995)

rd_extreme <- rdd_full(df_extreme) 

(out_extreme <- rd_extreme[1]) # Output regression
(plot_extreme <- rd_extreme[2]) # Output plot

### Dominant post-1995 ----

df_nonexpost95 <- df %>% dplyr::filter(party %in% c("b","c","f","a","v") & year > 1995)

rd_nonexpost95 <- rdd_full(df_nonexpost95)

(out_nonexpost95 <- rd_nonexpost95[1]) # Output regression
(plot_nonexpost95 <- rd_nonexpost95[2]) # Output plot

### Small dominant post-1995 ----

df_mainpost95 <- df %>% dplyr::filter(party %in% c("b","c") & year > 1995)

rd_mainpost95 <- rdd_full(df_mainpost95) #,title = "Mainstream parties")

(out_mainpost95 <- rd_mainpost95[1]) # Output regression
(plot_mainpost95 <- rd_mainpost95[2]) # Output plot

### Plot ----

pluck(plot_extreme,1)

ggsave("Output/fig5_a.pdf",width=5,height=3)

pluck(plot_mainpost95,1)

ggsave("Output/fig5_b.pdf",width=4,height=4)

pluck(plot_nonexpost95,1)

ggsave("Output/fig5_c.pdf",width=4,height=4)

###
# Table 1 ---
###

### RDD tables -----

out_extreme_tab <- out_extreme %>% 
  pluck(1) %>% 
  mutate(CI = paste0("[",round(conf.low,3),";",round(conf.high,3),"]"),
         party = "Challenger") %>%
  filter(term == "Robust") %>%
  dplyr::select(party,estimate,p.value,CI,bandwidth,nwithinbandwidth_control,nwithinbandwidth_treatment)

out_nonex_tab <- out_nonexpost95 %>% 
  pluck(1) %>% 
  mutate(CI = paste0("[",round(conf.low,3),";",round(conf.high,3),"]"),
         party = "Dominant") %>%
  filter(term == "Robust") %>%
  dplyr::select(party,estimate,p.value,CI,bandwidth,nwithinbandwidth_control,nwithinbandwidth_treatment)

out_main_tab <- out_mainpost95 %>% 
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
  add_footnote("Note: Running variable is party's margin to get represented in the city council, outcome is joining the coalition (dummy = 1) or not (dummy = 0) in the following election. Estimate is the average treatment effect at the cutoff estimated with local linear regression with triangular kernel and MSE-optimal bandwidth. Column 3-7 report 95% heteroskedasticity-robust confidence intervals, heteroskedasticity-robust p-value, main optimal bandwidth, control observations within bandwidth, and treated observations within bandwidth.") %>%
  set_latex_float("htbp") %>% 
  set_width(.99) %>% 
  set_col_width(c(0.2, 0.1, 0.1, 0.2, 0.1, 0.17, 0.17)) %>%
  set_font_size(10) %>%
  to_latex() %>% 
  write_lines(path="Output/table1.tex")

###
# Bootstrap analysis ---
###

### Bootstrapping to check for significance between two estimates---

df_bootstrap <- df %>% dplyr::select(municipality,year,party,mandates_calculated,share_threshold_loss,share_threshold_gain,coalition) # Reduce size of dataset to ease computation

# Setting up the bootstrap

# Pick random number for the seed:

set.seed(1)

alpha = 0.05 # P-value
sim = 1000 # Number of simulations

df_out <- tibble(est_challenger = rep(NA,sim+1))

bootstrap_distribution <-
  bootstraps(df_bootstrap, times = sim, apparent = TRUE) # Create samples

for (i in 1:nrow(bootstrap_distribution)){
  
                # Run it on the samples

                challenger <- as.data.frame(bootstrap_distribution$splits[i]) %>% dplyr::filter(party %in% c("o","ø") & year>1995) %>% 
                              rdd_full(.) %>% pluck(1)

                smalldominant <- as.data.frame(bootstrap_distribution$splits[i]) %>% dplyr::filter(party %in% c("b","c") & year>1995) %>% 
                                 rdd_full(.) %>% pluck(1)
                
                alldominant <- as.data.frame(bootstrap_distribution$splits[i]) %>% dplyr::filter(party %in% c("b","c","f","a","v") & year>1995) %>% 
                  rdd_full(.) %>% pluck(1)
                
                # Save estimates

                df_out$est_challenger[i] <- challenger[3,2]

                df_out$est_smalldominant[i] <- smalldominant[3,2]
                
                df_out$est_alldominant[i] <- alldominant[3,2]

}

df_out <- df_out %>% mutate(dif_chal_small = est_challenger-est_smalldominant,
                            dif_chal_all = est_challenger-est_alldominant)

# Create output

df_bootstrap <- tibble(
  group = c("Challenger v. Small Dominant","Challenger v. All Dominant"),
  estimate = c(pluck(out_extreme,1,2,3)-pluck(out_mainpost95,1,2,3),pluck(out_extreme,1,2,3)-pluck(out_nonexpost95,1,2,3)),
  std.error = c(sd(df_out$dif_chal_small),sd(df_out$dif_chal_all)),
  t.statistic = c(estimate[1] / std.error[1],estimate[2] / std.error[2]),
  p.value = c(pnorm(abs(t.statistic[1]), lower.tail = FALSE) * 1.96,pnorm(abs(t.statistic[2]), lower.tail = FALSE) * 1.96),
  conf.low = c(quantile(df_out$dif_chal_small, alpha / 1.96),quantile(df_out$dif_chal_all, alpha / 1.96)),
  conf.high = c(quantile(df_out$dif_chal_small, 1 - (alpha / 1.96)),quantile(df_out$dif_chal_all, 1 - (alpha / 1.96)))
)

# Histogram

df_out_quantiles <- df_out %>% 
  summarize(lower = quantile(dif_chal_small, probs = .05),
            upper = quantile(dif_chal_small, probs = .95))

###
# Calculate the number of votes associated with the optimal bandwidth ---
###

optimalbandwidth <- df %>% filter(year > 1995) %>% 
                           dplyr::distinct(year,municipality,.keep_all = TRUE) %>% 
                           ungroup() %>%
                           dplyr::summarize(meanvotes = mean(totalvotes,na.rm=TRUE),
                                            medianvotes = median(totalvotes,na.rm=TRUE)) %>%
                           mutate(optimalbw_mean = meanvotes*0.0175,
                                  optimalbw_median = medianvotes*0.0175)
