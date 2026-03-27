
##### AJPS Analysis v2024-11 ----

# Xs We Share, or: Context Similarity, Culture, and the Diffusion of Populism
# Nina Wiesehomeier, Nils Düpont and Saskia Ruth-Lovell

# All scripts were written with R versions >4.1 with the latest run under R 4.3.3.
# We recommend to run the script on a machine with at least 64 GB RAM because the margins command in particular temporarily occupies a lot of memory.
# Likewise, the regression models (lme4 objects) take up quite some RAM, so the script stores them in-between to free some RAM; yet, these Rdata files will occupy up to 16GB RAM when called in again for Figure 3
# If you have a machine with less RAM, you can still run the code block-wise by saving some model and plot objects separately in-between.



# TOC                                           Estimated runtime
# ----------------------------------------------------------------
# Prerequisites                                         < 5m
# ---
# Examples  - Code for examples given in main text      ~ 10m
# Figure 1  - Party and Regional Development
# Table 1   - Descriptive Statistics
# Misc. (pre-) estimations for the Appendix
# ---
# Figure 2  - Aggregated Effect Coefficient Plots       ~ 8-10h
# ---
# Figure 3  - Interaction Plots                         ~ 50h





##### Prerequisites ----


    # clean env
rm(list = ls())
set.seed(1)
gc()
section1_start <- Sys.time()


    # check if required packages are present and load; else install
    # note: function adapted from https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them)
required_pkgs <- function(x){
    for( i in x ){
        if( ! require( i , character.only = TRUE ) ){    #  returns TRUE if able to load package
            install.packages( i , dependencies = TRUE )  #  else (re-) install
            require( i , character.only = TRUE )         #  and load packages again
        }
    }
}
    # required packages
required_pkgs(c("tidyverse", "easystats", "skimr", "kableExtra", "ggpubr", "lme4", "texreg", "jtools", "export", "margins", "ggeffects"))


    # load data
load("dyadic_data_for_populism.Rdata")
#dyadic_df <- dyadic_df %>% sample_n(1000)     # for testing/debugging purposes
#gc()                                          # for testing/debugging purposes


    # set variables to factors as later on ggeffect calls Effect which cannot handle as.factor() within model specification...
dyadic_df$v2x_regime.i <- as.factor(dyadic_df$v2x_regime.i) 
dyadic_df$decade.i     <- as.factor(dyadic_df$decade.i)


    # reduce size of env and delete objects that are required for the supplement only
rm(crossv_dalp, crossv_gps)
gc()


    # set skim "defaults"
my_skim <- skim_with(base = sfl(n=n_complete), numeric = sfl(hist=NULL, n_complete=NULL, n_missing=NULL))
options(digits = 3)
options(knitr.kable.NA = '')


    # set readable labels
monadic_df$e_regionpol <- 
    factor(monadic_df$e_regionpol,
           levels = c(1,2,5),
           labels=c("Eastern Europe",
                    "Latin America",
                    "Western Europe and North America")
    )


section1_end <- Sys.time()
difftime(section1_end, section1_start, units="mins")





#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


section2_start <- Sys.time()



##### Data for Text Examples ----


    ##### Similarity of Spain to ...
dyadic_df %>% select(id.i, v2paid.i, country_text_id.i, year, id.j, v2paid.j, country_text_id.j, cultural_similarity) %>% filter(country_text_id.i == "ESP" & year == "2000") %>% arrange(country_text_id.j) %>% View()



    ##### Effect size spatial lag political exclusion


    # get mode of factor variables
summary(dyadic_df$v2x_regime.i)
summary(dyadic_df$decade.i)

    # model without culture
m_all <-
    lmer(pop_gmean_m.i ~                                                                                    # depvar: level of populism of party i at time t
     l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
     v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
     KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
     e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
     splag_all_wnone + splag_all_wpsplats + splag_all_wpubcorr + splag_all_wexlecon + splag_all_wexlpol + splag_all_wgini + splag_all_wKOFFi + splag_all_wKOFTr + # Aggregated diffusion effects
     pop_gmean_m.j +                                			                                            # direct diffusion effects (interaction)
     v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

    # model with culture
m_wcult <-
    lmer(pop_gmean_m.i ~                                                                                    # depvar: level of populism of party i at time t
     l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
     v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
     KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
     e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
     splag_wcult_wnone + splag_wcult_wpsplats + splag_wcult_wpubcorr + splag_wcult_wexlecon + splag_wcult_wexlpol + splag_wcult_wgini + splag_wcult_wKOFFi + splag_wcult_wKOFTr + # Aggregated diffusion effects
     pop_gmean_m.j * cultural_similarity +                                			                        # direct diffusion effects (interaction)
     v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs


    # predicted values of pop_gmean_m for spatial lag
msplag_all_mem   <- ggpredict(m_all,   terms = c("splag_all_wexlpol [meansd]")  , condition = c("v2x_regime.i" = "Liberal Democracy", "decade.i" = "2000"), type = "fe")
msplag_wcult_mem <- ggpredict(m_wcult, terms = c("splag_wcult_wexlpol [meansd]"), condition = c("v2x_regime.i" = "Liberal Democracy", "decade.i" = "2000"), type = "fe")


    # predicted values of domestic controls
msplag_compare1_mem <- ggpredict(m_all,   terms = c("powercon.i [meansd]")               , condition = c("v2x_regime.i" = "Liberal Democracy", "decade.i" = "2000"), type = "fe")
msplag_compare2_mem <- ggpredict(m_all,   terms = c("gini_mkt_ipol_ma3.i [meansd]")      , condition = c("v2x_regime.i" = "Liberal Democracy", "decade.i" = "2000"), type = "fe")
msplag_compare3_mem <- ggpredict(m_all,   terms = c("KOFTrGIdf_ma3.i [meansd]")          , condition = c("v2x_regime.i" = "Liberal Democracy", "decade.i" = "2000"), type = "fe")
msplag_compare4_mem <- ggpredict(m_all,   terms = c("e_migdpgro_ln_ipol_ma3.i [meansd]") , condition = c("v2x_regime.i" = "Liberal Democracy", "decade.i" = "2000"), type = "fe")

msplag_compare5_mem <- ggpredict(m_wcult, terms = c("powercon.i [meansd]")               , condition = c("v2x_regime.i" = "Liberal Democracy", "decade.i" = "2000"), type = "fe")
msplag_compare6_mem <- ggpredict(m_wcult, terms = c("gini_mkt_ipol_ma3.i [meansd]")      , condition = c("v2x_regime.i" = "Liberal Democracy", "decade.i" = "2000"), type = "fe")
msplag_compare7_mem <- ggpredict(m_wcult, terms = c("KOFTrGIdf_ma3.i [meansd]")          , condition = c("v2x_regime.i" = "Liberal Democracy", "decade.i" = "2000"), type = "fe")
msplag_compare8_mem <- ggpredict(m_wcult, terms = c("e_migdpgro_ln_ipol_ma3.i [meansd]") , condition = c("v2x_regime.i" = "Liberal Democracy", "decade.i" = "2000"), type = "fe")


msplag_all_mem          # pred from mean-sd -> mean+sd: 14.16 -> 14.89, diff = 0.73
msplag_wcult_mem        # pred from mean-sd -> mean+sd: 14.03 -> 14.85, diff = 0.82

msplag_compare1_mem     # pred from mean-sd -> mean+sd: 13.67 -> 15.38, diff = 1.71
msplag_compare2_mem     # pred from mean-sd -> mean+sd: 14.10 -> 14.95, diff = 0.85
msplag_compare3_mem     # pred from mean-sd -> mean+sd: 13.89 -> 15.16, diff = 1.27
msplag_compare4_mem     # pred from mean-sd -> mean+sd: 14.57 -> 14.48, diff = -0.09

msplag_compare5_mem     # pred from mean-sd -> mean+sd: 13.58 -> 15.29, diff = 1.71
msplag_compare6_mem     # pred from mean-sd -> mean+sd: 14.03 -> 14.85, diff = 0.82
msplag_compare7_mem     # pred from mean-sd -> mean+sd: 13.78 -> 15.10, diff = 1.32
msplag_compare8_mem     # pred from mean-sd -> mean+sd: 14.50 -> 14.38, diff = -0.12



    ##### Effect size direct effect from populist party (model: m_pfamwcult4)


    # (average) number of connections per party-election
dyadic_df %>% select(v2paid.i, year, v2paid.j) %>% group_by(v2paid.i, year) %>% summarise(no_of_ties = n()) %>% summary(no_of_ties)


    # grab sensible values for prediction
monadic_df %>% select(id, v2paid, pop_gmean_m, year) %>% filter(year > 2012 & v2paid %in% c(433, 809, 301, 442, 1754, 5094, 463, 1691, 623, 2530, 1565)) %>% arrange(pop_gmean_m) %>% View()
quantile(dyadic_df$dysim_v2xpe_exlecon_ma3, probs = c(0.2, 0.8)) # grievance similiarity economic exclusion 20th 2.04 80th 21.14

    # model
m_pfamwcult4 <-
    lmer(pop_gmean_m.i ~                                                                                    # depvar: level of populism of party i at time t
     l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
     v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
     KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
     e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
     splag_pfamwcult_wexlecon +                                                                             # Aggregated diffusion effects
     pop_gmean_m.j * dysim_v2xpe_exlecon_ma3 * cultural_similarity * same_pfam +	                        # direct diffusion effects (interaction)
     v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

    # predicted values of pop_gmean_m.i for different combinations
    # sender level: fix at 34 ~ Fidesz, FPÖ, Syriza, or MAS around 2014
    # culture 3 -> 6
    # pfam 0 -> 1

mdirect <- ggpredict(m_pfamwcult4,
            terms = c("dysim_v2xpe_exlecon_ma3 [2.04, 21.14]", "same_pfam [0, 1]", "cultural_similarity [3, 6]"),
            condition = c("pop_gmean_m.j" = 34, "v2x_regime.i" = "Liberal Democracy", "decade.i" = "2000"), type = "fe")
mdirect

#        # Predicted values of pop_gmean_m.i
#        
#        #           same_pfam = 0
#        # cultural_similarity = 3
#        
#        dysim_v2xpe_exlecon_ma3 | Predicted |         95% CI
#        ----------------------------------------------------
#        2.04  |     14.50 | [13.43, 15.56]
#        21.14 |     14.50 | [13.43, 15.56]
#        
#        #           same_pfam = 1
#       # cultural_similarity = 3
#        
#        dysim_v2xpe_exlecon_ma3 | Predicted |         95% CI
#        ----------------------------------------------------
#        2.04  |     14.55 | [13.48, 15.61]
#        21.14 |     14.54 | [13.48, 15.61]
#        
#        #           same_pfam = 0
#        # cultural_similarity = 6
#        
#        dysim_v2xpe_exlecon_ma3 | Predicted |         95% CI
#        ----------------------------------------------------
#        2.04  |     14.54 | [13.47, 15.60]
#        21.14 |     14.54 | [13.47, 15.60]
#        
#        #           same_pfam = 1
#        # cultural_similarity = 6
#        
#        dysim_v2xpe_exlecon_ma3 | Predicted |         95% CI
#        ----------------------------------------------------
#        2.04  |     14.59 | [13.52, 15.65]
#        21.14 |     14.58 | [13.52, 15.65]
#        
#        Adjusted for:
#        *    l_enemy_pop_gmean_m.i =     13.11
#        *               powercon.i =     -0.82
#        *                 orgext.i =      0.73
#        *        v2pariglef_mean.i =      3.11
#        *          v2psplats_ma3.i =      1.75
#        *        v2x_pubcorr_ma3.i =      0.31
#        *      v2xpe_exlecon_ma3.i =      0.25
#        *       v2xpe_exlpol_ma3.i =      0.18
#        *      gini_mkt_ipol_ma3.i =     46.83
#        *          KOFFiGIdf_ma3.i =     62.25
#        *          KOFTrGIdf_ma3.i =     52.44
#        * e_migdpgro_ln_ipol_ma3.i =     -3.47
#        * e_miinflat_ln_ipol_ma3.i =      1.84
#        *  e_migdppc_ln_ipol_ma3.i =      9.60
#        * splag_pfamwcult_wexlecon = 127250.58
#        *                 v2paid.i = 0 (population-level)
#        *             country_id.i = 0 (population-level)





#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



##### Figure 1 - Line Plots of Dependent Variable ----


    # estimate country averages
yearly_ds <- 
    monadic_df %>% 
    group_by(country_id, year) %>%                  # group by country and election year
    summarise(                                      # estimate means for each election
        elecavg_uw = mean(pop_gmean_m),
        elecavg_wv = weighted.mean(pop_gmean_m, v2pavote)
    ) %>%     
    ungroup() %>% 
    complete(country_id, year) %>%                  # create a full-time series for every country
    group_by(country_id) %>%
    fill(c("elecavg_uw", "elecavg_wv"), .direction = "down") %>% # set constant values per country up until the next election
    ungroup() %>% 
    left_join(., 
              monadic_df %>% select(country_id, e_regionpol) %>% distinct(),
              by = "country_id") %>%                 # re-join region
    mutate(
        elecavg_uw = replace(elecavg_uw, e_regionpol == "Eastern Europe" & year < 1989, NA),
        elecavg_wv = replace(elecavg_wv, e_regionpol == "Eastern Europe" & year < 1989, NA),
    )

    # plot regional trend over time
depvar_scat_region <- 
    yearly_ds %>% 
    filter(year >= 1975) %>% 
    ggplot(aes(year, elecavg_wv)) + 
    geom_point(aes(color = factor(e_regionpol)), alpha = 0.1) + 
    geom_smooth(aes(color = factor(e_regionpol)), method = "loess", span = 0.2, na.rm = T, linewidth = 1.0, se = FALSE) +
    geom_smooth(aes(color = factor(e_regionpol)), method = "loess", span = 0.2, na.rm = T, linewidth = 1.0, show_guide = FALSE) +
    scale_x_continuous(limits = c(1975, 2020), breaks = c(1975, 1985, 1995, 2005, 2015)) + 
    scale_color_manual(values = c("red3", "blue3", "green4")) +
    coord_cartesian(ylim = c(0, 40)) + 
    theme_classic(base_size = 8) +
    theme(
        legend.position = "bottom", plot.margin = margin(r = 10), legend.location = "panel", legend.justification = "top",
        axis.text = element_text(color = "black"), axis.title.y = element_text(margin = margin(r = 10)), axis.title.x = element_text(margin = margin(t = 10)),
        text = element_text(size = 10)
    ) +
    guides(color = guide_legend(ncol = 1, byrow = TRUE, title.position = "top")) +
    labs(x = "Year", y = "Level of populism (weighted by vote share)", color = "Region")
#print(depvar_scat_region)

    # plot selected parties

    # party ids
    # FRA: FN 433
    # USA: Republicans 809
    # GRC: Syriza 301
    # BOL: Mas 442
    # VEN: MVR 1754 + PSUV 5094
    # AUT: FPÖ 463
    # HUN: Fidesz 1691
    # ARG: PJ 623 + PFPJ 8059
    # POL: PiS 1565


df_lineplot <- 
    monadic_df %>% 
    select(v2paid, v2pashname, year, pop_gmean_m) %>% 
    filter(v2paid %in% c(433, 809, 301, 442, 1754, 5094, 463, 1691, 623, 2530, 1565)) %>% 
    mutate(
        v2paid_for_plot = case_when(
            (v2paid == 623  | v2paid == 2530 ~ "Partido Justicialista/Frente para la Victoria"),
            v2paid == 463  ~ "Freiheitliche Partei Österreichs",
            v2paid == 442  ~ "Movimiento al Socialismo",
            v2paid == 433  ~ "Front National",
            v2paid == 301  ~ "Synaspismós Rizospastikís Aristerás",
            v2paid == 1691 ~ "Fidesz — Magyar Polgári Szövetség",
            v2paid == 1565 ~ "Prawo i Sprawiedliwość",
            v2paid == 809  ~ "Republicans",
            (v2paid == 1754 | v2paid == 5094 ~ "Partido Socialista Unido de Venezuela")
        )
    )

depvar_line_parties <- 
    df_lineplot %>% 
    ggplot(aes(x = year, y = pop_gmean_m, color = as.factor(v2paid_for_plot))) +
    geom_line(linewidth = 1.0) +
    scale_y_continuous(limits = c(0, 40), breaks = waiver()) +
    scale_x_continuous(limits = c(1975, 2020), breaks = c(1975, 1985, 1995, 2005, 2015)) + 
    scale_color_manual(values = c("royalblue4", "olivedrab3", "dodgerblue2", "darkgreen", "green2", "darkred", "red2", "green4", "skyblue2")) +
    coord_cartesian(ylim = c(0, 40)) + 
    theme_classic(base_size = 8) +
    theme(
        legend.position = "bottom", plot.margin = margin(r = 10), legend.location = "panel", legend.justification = "top",
        axis.text = element_text(color = "black"), axis.title.y = element_text(margin = margin(r = 10)), axis.title.x = element_text(margin = margin(t = 10)),
        text = element_text(size = 10)
    ) +
    guides(color = guide_legend(ncol = 1, byrow = TRUE, title.position = "top")) +
    labs(x = "Year", y = "Level of populism", color = "Party")
#print(depvar_line_parties)

    # output Figure 1
combo_out <- ggarrange(depvar_line_parties, depvar_scat_region, ncol = 2, nrow = 1, align = "h")
#print(combo_out)

#graph2svg(x = combo_out, file = "figure1_trends.svg", width = 8, height = 8) # svg for print
graph2png(x = combo_out, file = "figure1_trends.png", width = 8, height = 8) # png for submission





#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



##### Table 1 - Descriptive Statistics ----


    # descriptive stats
descriptive_stats <- 
    monadic_df %>%
    select(pop_gmean_m, l_enemy_pop_gmean_m, powercon, orgext, v2pariglef_mean,                      # party level
           v2psplats_ma3, v2x_pubcorr_ma3, v2xpe_exlecon_ma3, v2xpe_exlpol_ma3, gini_mkt_ipol_ma3,   # grievances
           KOFFiGIdf_ma3, KOFTrGIdf_ma3,                                                             # globalization
           e_migdpgro_ln_ipol_ma3, e_miinflat_ln_ipol_ma3, e_migdppc_ln_ipol_ma3) %>%                # economic situation
    my_skim() %>%
    yank(., "numeric") %>% 
    select(skim_variable, n, mean, sd, p0, p100)
    
    # output as tex file
kbl(descriptive_stats, "latex", longtable = FALSE, booktabs = TRUE) %>% save_kable(file = "table1_descriptives.tex")





#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



##### Appendix - Country Table ----


    # country level information
country_info <- 
  monadic_df %>%
  group_by(country_id) %>%
  summarize(
    obs = n(),
    first_year = min(year),
    last_year = max(year),
    no_of_parties = n_distinct(v2paid),
    no_of_elections = n_distinct(election_no)
  ) %>% 
  left_join(monadic_df, by = "country_id") %>%
  select(country_id, country_text_id, obs, no_of_parties, no_of_elections, first_year, last_year) %>%
  distinct(country_id, .keep_all = TRUE)

    # output as tex file
kbl(country_info, "latex", longtable = TRUE, booktabs = TRUE) %>% save_kable(file = "countrytable.tex")



##### SI - Dyadic Dissimilarities ----

  # In order to save time when compiling the SI, the distribution of dyadic dissimilarities is estimated here beforehand
hist1 <- dyadic_df %>% ggplot(aes(x = dysim_v2psplats_ma3)) 	+ geom_density(fill = "cornflowerblue", alpha=0.7, color="black") + geom_rug(sides = "b", alpha = 0.05) + coord_cartesian(xlim = c(0, 200)) + theme_minimal()
hist2 <- dyadic_df %>% ggplot(aes(x = dysim_v2x_pubcorr_ma3)) 	+ geom_density(fill = "cornflowerblue", alpha=0.7, color="black") + geom_rug(sides = "b", alpha = 0.05) + coord_cartesian(xlim = c(0, 200)) + theme_minimal()
hist3 <- dyadic_df %>% ggplot(aes(x = dysim_v2xpe_exlecon_ma3)) + geom_density(fill = "cornflowerblue", alpha=0.7, color="black") + geom_rug(sides = "b", alpha = 0.05) + coord_cartesian(xlim = c(0, 200)) + theme_minimal()
hist4 <- dyadic_df %>% ggplot(aes(x = dysim_v2xpe_exlpol_ma3))  + geom_density(fill = "cornflowerblue", alpha=0.7, color="black") + geom_rug(sides = "b", alpha = 0.05) + coord_cartesian(xlim = c(0, 200)) + theme_minimal()
hist5 <- dyadic_df %>% ggplot(aes(x = dysim_gini_mkt_ipol_ma3)) + geom_density(fill = "cornflowerblue", alpha=0.7, color="black") + geom_rug(sides = "b", alpha = 0.05) + coord_cartesian(xlim = c(0,  10)) + theme_minimal()
hist6 <- dyadic_df %>% ggplot(aes(x = dysim_KOFFiGIdf_ma3)) 	+ geom_density(fill = "cornflowerblue", alpha=0.7, color="black") + geom_rug(sides = "b", alpha = 0.05) + coord_cartesian(xlim = c(0,  20)) + theme_minimal()
hist7 <- dyadic_df %>% ggplot(aes(x = dysim_KOFTrGIdf_ma3)) 	+ geom_density(fill = "cornflowerblue", alpha=0.7, color="black") + geom_rug(sides = "b", alpha = 0.05) + coord_cartesian(xlim = c(0,  20)) + theme_minimal()
hist8 <- dyadic_df %>% ggplot(aes(x = cultural_similarity)) 	+ geom_density(fill = "cornflowerblue", alpha=0.7, color="black") + geom_rug(sides = "b", alpha = 0.05) + theme_minimal()

combo <- ggarrange(hist1, hist2, hist3, hist4, hist5, hist6, hist7, hist8, ncol = 2, nrow = 4) %>%
  annotate_figure(., bottom = text_grob("Note: Density plots cut at >95th percentile (except for cultural similarity) for a more informative presentation.", face = "italic", size = 10))

  # output figure for SI
graph2png(x = combo, file = "suppl_fig_dissim.png", width = 8, height = 10, dpi = 1200)



    # clean env to free RAM for further analyses
section2_end <- Sys.time()
difftime(section2_end, section2_start, units="mins")
rm(list=setdiff(ls(), "dyadic_df"))
gc()





#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


section3_start <- Sys.time()



##### Figure 2  - Spatial Lag Coefficient Plots ----


    ##### Fig2 1 Aggregated effects: all parties are sender ----


m_all1 <-
    lmer(pop_gmean_m.i ~                                                                                    # depvar: level of populism of party i at time t
     l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
     v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
     KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
     e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
     splag_all_wnone +                                                                                      # Aggregated diffusion effects
     pop_gmean_m.j +                                                      			                        # direct diffusion effects (interaction)
     v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

m_all2 <-
    lmer(pop_gmean_m.i ~                                                                                    # depvar: level of populism of party i at time t
     l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
     v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
     KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
     e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
     splag_all_wpsplats +                                                                                   # Aggregated diffusion effects
     pop_gmean_m.j * dysim_v2psplats_ma3 +                                			                        # direct diffusion effects (interaction)
     v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

m_all3 <-
    lmer(pop_gmean_m.i ~                                                                                    # depvar: level of populism of party i at time t
     l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
     v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
     KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
     e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
     splag_all_wpubcorr +                                                                                   # Aggregated diffusion effects
     pop_gmean_m.j * dysim_v2x_pubcorr_ma3 +										                        # direct diffusion effects (interaction)
     v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

m_all4 <-
    lmer(pop_gmean_m.i ~                                                                                    # depvar: level of populism of party i at time t
     l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
     v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
     KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
     e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
     splag_all_wexlecon +                                                                                   # Aggregated diffusion effects
     pop_gmean_m.j * dysim_v2xpe_exlecon_ma3 +                            			                        # direct diffusion effects (interaction)
     v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

m_all5 <-
    lmer(pop_gmean_m.i ~                                                                                    # depvar: level of populism of party i at time t
     l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
     v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
     KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
     e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
     splag_all_wexlpol +                                                                                    # Aggregated diffusion effects
     pop_gmean_m.j * dysim_v2xpe_exlpol_ma3 +                             			                        # direct diffusion effects (interaction)
     v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

m_all6 <-
    lmer(pop_gmean_m.i ~                                                                                    # depvar: level of populism of party i at time t
     l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
     v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
     KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
     e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
     splag_all_wgini +                                                                                      # Aggregated diffusion effects
     pop_gmean_m.j * dysim_gini_mkt_ipol_ma3 +                            			                        # direct diffusion effects (interaction)
     v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

m_all7 <-
    lmer(pop_gmean_m.i ~                                                                                    # depvar: level of populism of party i at time t
     l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
     v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
     KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
     e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
     splag_all_wKOFFi +                                                                                     # Aggregated diffusion effects
     pop_gmean_m.j * dysim_KOFFiGIdf_ma3 +                                			                        # direct diffusion effects (interaction)
     v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

m_all8 <-
    lmer(pop_gmean_m.i ~                                                                                    # depvar: level of populism of party i at time t
     l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
     v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
     KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
     e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
     splag_all_wKOFTr +                                                                                     # Aggregated diffusion effects
     pop_gmean_m.j * dysim_KOFTrGIdf_ma3 +                                			                        # direct diffusion effects (interaction)
     v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

m_all9 <-
    lmer(pop_gmean_m.i ~                                                                                    # depvar: level of populism of party i at time t
     l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
     v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
     KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
     e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
     splag_all_wnone + splag_all_wpsplats + splag_all_wpubcorr + splag_all_wexlecon + splag_all_wexlpol + splag_all_wgini + splag_all_wKOFFi + splag_all_wKOFTr + # Aggregated diffusion effects
     pop_gmean_m.j +                                			                                            # direct diffusion effects (interaction)
     v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs


    ##### Fig2 2 Aggregated effects: all parties are sender + weighted by cultural similarity ----


m_wcult1 <-
    lmer(pop_gmean_m.i ~                                                                                    # depvar: level of populism of party i at time t
     l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
     v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
     KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
     e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
     splag_wcult_wnone +                                                                                    # Aggregated diffusion effects
     pop_gmean_m.j * cultural_similarity +                                                      			# direct diffusion effects (interaction)
     v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

m_wcult2 <-
    lmer(pop_gmean_m.i ~                                                                                    # depvar: level of populism of party i at time t
     l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
     v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
     KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
     e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
     splag_wcult_wpsplats +                                                                                 # Aggregated diffusion effects
     pop_gmean_m.j * dysim_v2psplats_ma3 * cultural_similarity +           			                        # direct diffusion effects (interaction)
     v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

m_wcult3 <-
    lmer(pop_gmean_m.i ~                                                                                    # depvar: level of populism of party i at time t
     l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
     v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
     KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
     e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
     splag_wcult_wpubcorr +                                                                                 # Aggregated diffusion effects
     pop_gmean_m.j * dysim_v2x_pubcorr_ma3 * cultural_similarity +					                        # direct diffusion effects (interaction)
     v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

m_wcult4 <-
    lmer(pop_gmean_m.i ~                                                                                    # depvar: level of populism of party i at time t
     l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
     v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
     KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
     e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
     splag_wcult_wexlecon +                                                                                 # Aggregated diffusion effects
     pop_gmean_m.j * dysim_v2xpe_exlecon_ma3 * cultural_similarity +       			                        # direct diffusion effects (interaction)
     v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

m_wcult5 <-
    lmer(pop_gmean_m.i ~                                                                                    # depvar: level of populism of party i at time t
     l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
     v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
     KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
     e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
     splag_wcult_wexlpol +                                                                                  # Aggregated diffusion effects
     pop_gmean_m.j * dysim_v2xpe_exlpol_ma3 * cultural_similarity +      			                        # direct diffusion effects (interaction)
     v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

m_wcult6 <-
    lmer(pop_gmean_m.i ~                                                                                    # depvar: level of populism of party i at time t
     l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
     v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
     KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
     e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
     splag_wcult_wgini +                                                                                    # Aggregated diffusion effects
     pop_gmean_m.j * dysim_gini_mkt_ipol_ma3 * cultural_similarity +    			                        # direct diffusion effects (interaction)
     v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

m_wcult7 <-
    lmer(pop_gmean_m.i ~                                                                                    # depvar: level of populism of party i at time t
     l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
     v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
     KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
     e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
     splag_wcult_wKOFFi +                                                                                   # Aggregated diffusion effects
     pop_gmean_m.j * dysim_KOFFiGIdf_ma3 * cultural_similarity +        			                        # direct diffusion effects (interaction)
     v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

m_wcult8 <-
    lmer(pop_gmean_m.i ~                                                                                    # depvar: level of populism of party i at time t
     l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
     v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
     KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
     e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
     splag_wcult_wKOFTr +                                                                                   # Aggregated diffusion effects
     pop_gmean_m.j * dysim_KOFTrGIdf_ma3 * cultural_similarity +        			                        # direct diffusion effects (interaction)
     v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

m_wcult9 <-
    lmer(pop_gmean_m.i ~                                                                                    # depvar: level of populism of party i at time t
     l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
     v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
     KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
     e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
     splag_wcult_wnone + splag_wcult_wpsplats + splag_wcult_wpubcorr + splag_wcult_wexlecon + splag_wcult_wexlpol + splag_wcult_wgini + splag_wcult_wKOFFi + splag_wcult_wKOFTr + # Aggregated diffusion effects
     pop_gmean_m.j * cultural_similarity +                                			                        # direct diffusion effects (interaction)
     v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs


    ##### Fig2 3 Aggregated effects: all parties are sender - plot ----

    # load after initial run when tweaking the plot so to avoid re-running the models on a machine with less RAM
    #load("populism_replication_models_all.Rdata") 

	  # set scale = TRUE to obtain standardized coeffs
plot_all <- 
    plot_summs(m_all1, m_all2, m_all3, m_all4, m_all5, m_all6, m_all7, m_all8,
               m_wcult1, m_wcult2, m_wcult3, m_wcult4, m_wcult5, m_wcult6, m_wcult7, m_wcult8,
               scale = TRUE, plot.distributions = FALSE, ci_level = .999,
               coefs = c("Distinct party platforms" = "splag_all_wpsplats",
                         "Public sector corruption" = "splag_all_wpubcorr",
                         "Economic exclusion"       = "splag_all_wexlecon",
                         "Political exclusion"      = "splag_all_wexlpol",
                         "Income inequality"        = "splag_all_wgini",
                         "Financial globalization"  = "splag_all_wKOFFi",
                         "Trade globalization"      = "splag_all_wKOFTr",
                         "Unweighted"               = "splag_all_wnone",
                         "Distinct party platforms" = "splag_wcult_wpsplats",
                         "Public sector corruption" = "splag_wcult_wpubcorr",
                         "Economic exclusion"       = "splag_wcult_wexlecon",
                         "Political exclusion"      = "splag_wcult_wexlpol",
                         "Income inequality"        = "splag_wcult_wgini",
                         "Financial globalization"  = "splag_wcult_wKOFFi",
                         "Trade globalization"      = "splag_wcult_wKOFTr",
                         "Unweighted"               = "splag_wcult_wnone"),
               colors = c("dodgerblue",  "dodgerblue",  "dodgerblue",  "dodgerblue",  "dodgerblue",  "dodgerblue",  "dodgerblue",  "dodgerblue",
                          "gold",  "gold",  "gold",  "gold",  "gold",  "gold",  "gold",  "gold"),
               point.shape = c(1:8, 1:8),
               model.names = c("SM1", "SM2", "SM3", "SM4", "SM5", "SM6", "SM7", "SM8",
                               "SM1 w/ culture", "SM2 w/ culture", "SM3 w/ culture", "SM4 w/ culture", "SM5 w/ culture", "SM6 w/ culture", "SM7 w/ culture", "SM8 w/ culture"))
#print(plot_all)

    # store away models then clean env to free RAM for further analyses
save(list = ls(pattern = "m_all|m_wcult"), file = "populism_replication_models_all.Rdata")
rm(list = ls(pattern = "m_all|m_wcult"))
gc()


    ##### Fig2 4 Aggregated effects: parties of same ideological bloc are sender ----


m_pfam1 <-
    lmer(pop_gmean_m.i ~                                                                                    # depvar: level of populism of party i at time t
     l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
     v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
     KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
     e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
     splag_pfam_wnone +                                                                                     # Aggregated diffusion effects
     pop_gmean_m.j * same_pfam +                                                      			            # direct diffusion effects (interaction)
     v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

m_pfam2 <-
    lmer(pop_gmean_m.i ~                                                                                    # depvar: level of populism of party i at time t
     l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
     v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
     KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
     e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
     splag_pfam_wpsplats +                                                                                  # Aggregated diffusion effects
     pop_gmean_m.j * dysim_v2psplats_ma3 * same_pfam +                    			                        # direct diffusion effects (interaction)
     v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

m_pfam3 <-
    lmer(pop_gmean_m.i ~                                                                                    # depvar: level of populism of party i at time t
     l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
     v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
     KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
     e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
     splag_pfam_wpubcorr +                                                                                  # Aggregated diffusion effects
     pop_gmean_m.j * dysim_v2x_pubcorr_ma3 * same_pfam +							                        # direct diffusion effects (interaction)
     v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

m_pfam4 <-
    lmer(pop_gmean_m.i ~                                                                                    # depvar: level of populism of party i at time t
     l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
     v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
     KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
     e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
     splag_pfam_wexlecon +                                                                                  # Aggregated diffusion effects
     pop_gmean_m.j * dysim_v2xpe_exlecon_ma3 * same_pfam +                			                        # direct diffusion effects (interaction)
     v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

m_pfam5 <-
    lmer(pop_gmean_m.i ~                                                                                    # depvar: level of populism of party i at time t
     l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
     v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
     KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
     e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
     splag_pfam_wexlpol +                                                                                   # Aggregated diffusion effects
     pop_gmean_m.j * dysim_v2xpe_exlpol_ma3 * same_pfam +                			                        # direct diffusion effects (interaction)
     v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

m_pfam6 <-
    lmer(pop_gmean_m.i ~                                                                                    # depvar: level of populism of party i at time t
     l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
     v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
     KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
     e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
     splag_pfam_wgini +                                                                                     # Aggregated diffusion effects
     pop_gmean_m.j * dysim_gini_mkt_ipol_ma3 * same_pfam +                			                        # direct diffusion effects (interaction)
     v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

m_pfam7 <-
    lmer(pop_gmean_m.i ~                                                                                    # depvar: level of populism of party i at time t
     l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
     v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
     KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
     e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
     splag_pfam_wKOFFi +                                                                                    # Aggregated diffusion effects
     pop_gmean_m.j * dysim_KOFFiGIdf_ma3 * same_pfam +                    			                        # direct diffusion effects (interaction)
     v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

m_pfam8 <-
    lmer(pop_gmean_m.i ~                                                                                    # depvar: level of populism of party i at time t
     l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
     v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
     KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
     e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
     splag_pfam_wKOFTr +                                                                                    # Aggregated diffusion effects
     pop_gmean_m.j * dysim_KOFTrGIdf_ma3 * same_pfam +                    			                        # direct diffusion effects (interaction)
     v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

m_pfam9 <-
    lmer(pop_gmean_m.i ~                                                                                    # depvar: level of populism of party i at time t
     l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
     v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
     KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
     e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
     splag_pfam_wnone + splag_pfam_wpsplats + splag_pfam_wpubcorr + splag_pfam_wexlecon + splag_pfam_wexlpol + splag_pfam_wgini + splag_pfam_wKOFFi + splag_pfam_wKOFTr + # Aggregated diffusion effects
     pop_gmean_m.j * same_pfam +                      			                                            # direct diffusion effects (interaction)
     v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs


    ##### Fig2 5 Aggregated effects: parties of same ideological bloc are sender + weighted by cultural similarity ----


m_pfamwcult1 <-
    lmer(pop_gmean_m.i ~                                                                                    # depvar: level of populism of party i at time t
     l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
     v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
     KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
     e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
     splag_pfamwcult_wnone +                                                                                # Aggregated diffusion effects
     pop_gmean_m.j * cultural_similarity * same_pfam +                                             			# direct diffusion effects (interaction)
     v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

m_pfamwcult2 <-
    lmer(pop_gmean_m.i ~                                                                                    # depvar: level of populism of party i at time t
     l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
     v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
     KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
     e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
     splag_pfamwcult_wpsplats +                                                                             # Aggregated diffusion effects
     pop_gmean_m.j * dysim_v2psplats_ma3 * cultural_similarity * same_pfam +		                        # direct diffusion effects (interaction)
     v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

m_pfamwcult3 <-
    lmer(pop_gmean_m.i ~                                                                                    # depvar: level of populism of party i at time t
     l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
     v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
     KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
     e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
     splag_pfamwcult_wpubcorr +                                                                             # Aggregated diffusion effects
     pop_gmean_m.j * dysim_v2x_pubcorr_ma3 * cultural_similarity * same_pfam +		                        # direct diffusion effects (interaction)
     v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

m_pfamwcult4 <-
    lmer(pop_gmean_m.i ~                                                                                    # depvar: level of populism of party i at time t
     l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
     v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
     KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
     e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
     splag_pfamwcult_wexlecon +                                                                             # Aggregated diffusion effects
     pop_gmean_m.j * dysim_v2xpe_exlecon_ma3 * cultural_similarity * same_pfam +	                        # direct diffusion effects (interaction)
     v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

m_pfamwcult5 <-
    lmer(pop_gmean_m.i ~                                                                                    # depvar: level of populism of party i at time t
     l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
     v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
     KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
     e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
     splag_pfamwcult_wexlpol +                                                                              # Aggregated diffusion effects
     pop_gmean_m.j * dysim_v2xpe_exlpol_ma3 * cultural_similarity * same_pfam +		                        # direct diffusion effects (interaction)
     v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

m_pfamwcult6 <-
    lmer(pop_gmean_m.i ~                                                                                    # depvar: level of populism of party i at time t
     l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
     v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
     KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
     e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
     splag_pfamwcult_wgini +                                                                                # Aggregated diffusion effects
     pop_gmean_m.j * dysim_gini_mkt_ipol_ma3 * cultural_similarity * same_pfam +	                        # direct diffusion effects (interaction)
     v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

m_pfamwcult7 <-
    lmer(pop_gmean_m.i ~                                                                                    # depvar: level of populism of party i at time t
     l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
     v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
     KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
     e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
     splag_pfamwcult_wKOFFi +                                                                               # Aggregated diffusion effects
     pop_gmean_m.j * dysim_KOFFiGIdf_ma3 * cultural_similarity * same_pfam +		                        # direct diffusion effects (interaction)
     v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

m_pfamwcult8 <-
    lmer(pop_gmean_m.i ~                                                                                    # depvar: level of populism of party i at time t
     l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
     v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
     KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
     e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
     splag_pfamwcult_wKOFTr +                                                                               # Aggregated diffusion effects
     pop_gmean_m.j * dysim_KOFTrGIdf_ma3 * cultural_similarity * same_pfam +		                        # direct diffusion effects (interaction)
     v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

m_pfamwcult9 <-
    lmer(pop_gmean_m.i ~                                                                                    # depvar: level of populism of party i at time t
     l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
     v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
     KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
     e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
     splag_pfamwcult_wnone + splag_pfamwcult_wpsplats + splag_pfamwcult_wpubcorr + splag_pfamwcult_wexlecon + splag_pfamwcult_wexlpol + splag_pfamwcult_wgini + splag_pfamwcult_wKOFFi + splag_pfamwcult_wKOFTr + # Aggregated diffusion effects
     pop_gmean_m.j * cultural_similarity * same_pfam +                     			                        # direct diffusion effects (interaction)
     v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs


    ##### Fig2 6 Aggregated effects: parties of same ideological bloc are sender - plot ----

    # load after initial run when tweaking the plot so to avoid re-running the models on a machine with less RAM
    #load("populism_replication_models_pfam.Rdata") 

    # set scale = TRUE to obtain standardized coeffs
plot_pfam <- 
    plot_summs(m_pfam1, m_pfam2, m_pfam3, m_pfam4, m_pfam5, m_pfam6, m_pfam7, m_pfam8,
               m_pfamwcult1, m_pfamwcult2, m_pfamwcult3, m_pfamwcult4, m_pfamwcult5, m_pfamwcult6, m_pfamwcult7, m_pfamwcult8,
               scale = TRUE, plot.distributions = FALSE, ci_level = .999,
               coefs = c("Distinct party platforms" = "splag_pfam_wpsplats",
                         "Public sector corruption" = "splag_pfam_wpubcorr",
                         "Economic exclusion"       = "splag_pfam_wexlecon",
                         "Political exclusion"      = "splag_pfam_wexlpol",
                         "Income inequality"        = "splag_pfam_wgini",
                         "Financial globalization"  = "splag_pfam_wKOFFi",
                         "Trade globalization"      = "splag_pfam_wKOFTr",
                         "Unweighted"               = "splag_pfam_wnone",
                         "Distinct party platforms" = "splag_pfamwcult_wpsplats",
                         "Public sector corruption" = "splag_pfamwcult_wpubcorr",
                         "Economic exclusion"       = "splag_pfamwcult_wexlecon",
                         "Political exclusion"      = "splag_pfamwcult_wexlpol",
                         "Income inequality"        = "splag_pfamwcult_wgini",
                         "Financial globalization"  = "splag_pfamwcult_wKOFFi",
                         "Trade globalization"      = "splag_pfamwcult_wKOFTr",
                         "Unweighted"               = "splag_pfamwcult_wnone"),
               colors = c("dodgerblue",  "dodgerblue",  "dodgerblue",  "dodgerblue",  "dodgerblue",  "dodgerblue",  "dodgerblue",  "dodgerblue",
                          "gold",  "gold",  "gold",  "gold",  "gold",  "gold",  "gold",  "gold"),
               point.shape = c(1:8, 1:8),
               model.names = c("SM1", "SM2", "SM3", "SM4", "SM5", "SM6", "SM7", "SM8",
                               "SM1 w/ culture", "SM2 w/ culture", "SM3 w/ culture", "SM4 w/ culture", "SM5 w/ culture", "SM6 w/ culture", "SM7 w/ culture", "SM8 w/ culture"))
#print(plot_pfam)

    # store away models then clean env to free RAM for further analyses
save(list = ls(pattern = "m_pfam"), file = "populism_replication_models_pfam.Rdata")
rm(list = ls(pattern = "m_pfam"))
gc()


    ##### Fig2 7 Aggregated effects: governing parties are sender ----


m_gov1 <-
    lmer(pop_gmean_m.i ~                                                                                    # depvar: level of populism of party i at time t
     l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
     v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
     KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
     e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
     splag_gov_wnone +                                                                                      # Aggregated diffusion effects
     pop_gmean_m.j * sender_gov +                                                      			            # direct diffusion effects (interaction)
     v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

m_gov2 <-
    lmer(pop_gmean_m.i ~                                                                                    # depvar: level of populism of party i at time t
     l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
     v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
     KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
     e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
     splag_gov_wpsplats +                                                                                   # Aggregated diffusion effects
     pop_gmean_m.j * dysim_v2psplats_ma3 * sender_gov +                    			                        # direct diffusion effects (interaction)
     v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

m_gov3 <-
    lmer(pop_gmean_m.i ~                                                                                    # depvar: level of populism of party i at time t
     l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
     v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
     KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
     e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
     splag_gov_wpubcorr +                                                                                   # Aggregated diffusion effects
     pop_gmean_m.j * dysim_v2x_pubcorr_ma3 * sender_gov +							                        # direct diffusion effects (interaction)
     v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

m_gov4 <-
    lmer(pop_gmean_m.i ~                                                                                    # depvar: level of populism of party i at time t
     l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
     v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
     KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
     e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
     splag_gov_wexlecon +                                                                                   # Aggregated diffusion effects
     pop_gmean_m.j * dysim_v2xpe_exlecon_ma3 * sender_gov +                			                        # direct diffusion effects (interaction)
     v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

m_gov5 <-
    lmer(pop_gmean_m.i ~                                                                                    # depvar: level of populism of party i at time t
     l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
     v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
     KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
     e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
     splag_gov_wexlpol +                                                                                    # Aggregated diffusion effects
     pop_gmean_m.j * dysim_v2xpe_exlpol_ma3 * sender_gov +                			                        # direct diffusion effects (interaction)
     v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

m_gov6 <-
    lmer(pop_gmean_m.i ~                                                                                    # depvar: level of populism of party i at time t
     l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
     v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
     KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
     e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
     splag_gov_wgini +                                                                                      # Aggregated diffusion effects
     pop_gmean_m.j * dysim_gini_mkt_ipol_ma3 * sender_gov +                			                        # direct diffusion effects (interaction)
     v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

m_gov7 <-
    lmer(pop_gmean_m.i ~                                                                                    # depvar: level of populism of party i at time t
     l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
     v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
     KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
     e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
     splag_gov_wKOFFi +                                                                                     # Aggregated diffusion effects
     pop_gmean_m.j * dysim_KOFFiGIdf_ma3 * sender_gov +                    			                        # direct diffusion effects (interaction)
     v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

m_gov8 <-
    lmer(pop_gmean_m.i ~                                                                                    # depvar: level of populism of party i at time t
     l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
     v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
     KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
     e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
     splag_gov_wKOFTr +                                                                                     # Aggregated diffusion effects
     pop_gmean_m.j * dysim_KOFTrGIdf_ma3 * sender_gov +                    			                        # direct diffusion effects (interaction)
     v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

m_gov9 <-
    lmer(pop_gmean_m.i ~                                                                                    # depvar: level of populism of party i at time t
     l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
     v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
     KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
     e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
     splag_gov_wnone + splag_gov_wpsplats + splag_gov_wpubcorr + splag_gov_wexlecon + splag_gov_wexlpol + splag_gov_wgini + splag_gov_wKOFFi + splag_gov_wKOFTr + # Aggregated diffusion effects
     pop_gmean_m.j * sender_gov +                      			                                            # direct diffusion effects (interaction)
     v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs


    ##### Fig2 8 Aggregated effects: governing parties are sender + weighted by cultural similarity ----


m_govwcult1 <-
    lmer(pop_gmean_m.i ~                                                                                    # depvar: level of populism of party i at time t
     l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
     v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
     KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
     e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
     splag_govwcult_wnone +                                                                                 # Aggregated diffusion effects
     pop_gmean_m.j * cultural_similarity * sender_gov +                                             		# direct diffusion effects (interaction)
     v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

m_govwcult2 <-
    lmer(pop_gmean_m.i ~                                                                                    # depvar: level of populism of party i at time t
     l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
     v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
     KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
     e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
     splag_govwcult_wpsplats +                                                                              # Aggregated diffusion effects
     pop_gmean_m.j * dysim_v2psplats_ma3 * cultural_similarity * sender_gov +		                        # direct diffusion effects (interaction)
     v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

m_govwcult3 <-
    lmer(pop_gmean_m.i ~                                                                                    # depvar: level of populism of party i at time t
     l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
     v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
     KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
     e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
     splag_govwcult_wpubcorr +                                                                              # Aggregated diffusion effects
     pop_gmean_m.j * dysim_v2x_pubcorr_ma3 * cultural_similarity * sender_gov +		                        # direct diffusion effects (interaction)
     v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

m_govwcult4 <-
    lmer(pop_gmean_m.i ~                                                                                    # depvar: level of populism of party i at time t
     l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
     v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
     KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
     e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
     splag_govwcult_wexlecon +                                                                              # Aggregated diffusion effects
     pop_gmean_m.j * dysim_v2xpe_exlecon_ma3 * cultural_similarity * sender_gov +	                        # direct diffusion effects (interaction)
     v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

m_govwcult5 <-
    lmer(pop_gmean_m.i ~                                                                                    # depvar: level of populism of party i at time t
     l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
     v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
     KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
     e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
     splag_govwcult_wexlpol +                                                                               # Aggregated diffusion effects
     pop_gmean_m.j * dysim_v2xpe_exlpol_ma3 * cultural_similarity * sender_gov +		                    # direct diffusion effects (interaction)
     v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

m_govwcult6 <-
    lmer(pop_gmean_m.i ~                                                                                    # depvar: level of populism of party i at time t
     l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
     v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
     KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
     e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
     splag_govwcult_wgini +                                                                                 # Aggregated diffusion effects
     pop_gmean_m.j * dysim_gini_mkt_ipol_ma3 * cultural_similarity * sender_gov +	                        # direct diffusion effects (interaction)
     v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

m_govwcult7 <-
    lmer(pop_gmean_m.i ~                                                                                    # depvar: level of populism of party i at time t
     l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
     v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
     KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
     e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
     splag_govwcult_wKOFFi +                                                                                # Aggregated diffusion effects
     pop_gmean_m.j * dysim_KOFFiGIdf_ma3 * cultural_similarity * sender_gov +		                        # direct diffusion effects (interaction)
     v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

m_govwcult8 <-
    lmer(pop_gmean_m.i ~                                                                                    # depvar: level of populism of party i at time t
     l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
     v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
     KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
     e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
     splag_govwcult_wKOFTr +                                                                                # Aggregated diffusion effects
     pop_gmean_m.j * dysim_KOFTrGIdf_ma3 * cultural_similarity * sender_gov +		                        # direct diffusion effects (interaction)
     v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

m_govwcult9 <-
    lmer(pop_gmean_m.i ~                                                                                    # depvar: level of populism of party i at time t
     l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
     v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
     KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
     e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
     splag_govwcult_wnone + splag_govwcult_wpsplats + splag_govwcult_wpubcorr + splag_govwcult_wexlecon + splag_govwcult_wexlpol + splag_govwcult_wgini + splag_govwcult_wKOFFi + splag_govwcult_wKOFTr + # Aggregated diffusion effects
     pop_gmean_m.j * cultural_similarity * sender_gov +                     			                    # direct diffusion effects (interaction)
     v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs


    ##### Fig2 9 Aggregated effects: governing parties are sender - plot ----

    # load after initial run when tweaking the plot so to avoid re-running the models on a machine with less RAM
    #load("populism_replication_models_gov.Rdata") 
    
    # set scale = TRUE to obtain standardized coeffs
plot_gov <- 
    plot_summs(m_gov1, m_gov2, m_gov3, m_gov4, m_gov5, m_gov6, m_gov7, m_gov8,
               m_govwcult1, m_govwcult2, m_govwcult3, m_govwcult4, m_govwcult5, m_govwcult6, m_govwcult7, m_govwcult8,
               scale = TRUE, plot.distributions = FALSE, ci_level = .999,
               coefs = c("Distinct party platforms" = "splag_gov_wpsplats",
                         "Public sector corruption" = "splag_gov_wpubcorr",
                         "Economic exclusion"       = "splag_gov_wexlecon",
                         "Political exclusion"      = "splag_gov_wexlpol",
                         "Income inequality"        = "splag_gov_wgini",
                         "Financial globalization"  = "splag_gov_wKOFFi",
                         "Trade globalization"      = "splag_gov_wKOFTr",
                         "Unweighted"               = "splag_gov_wnone",
                         "Distinct party platforms" = "splag_govwcult_wpsplats",
                         "Public sector corruption" = "splag_govwcult_wpubcorr",
                         "Economic exclusion"       = "splag_govwcult_wexlecon",
                         "Political exclusion"      = "splag_govwcult_wexlpol",
                         "Income inequality"        = "splag_govwcult_wgini",
                         "Financial globalization"  = "splag_govwcult_wKOFFi",
                         "Trade globalization"      = "splag_govwcult_wKOFTr",
                         "Unweighted"               = "splag_govwcult_wnone"),
               colors = c("dodgerblue",  "dodgerblue",  "dodgerblue",  "dodgerblue",  "dodgerblue",  "dodgerblue",  "dodgerblue",  "dodgerblue",
                          "gold",  "gold",  "gold",  "gold",  "gold",  "gold",  "gold",  "gold"),
               point.shape = c(1:8, 1:8),
               model.names = c("SM1", "SM2", "SM3", "SM4", "SM5", "SM6", "SM7", "SM8",
                               "SM1 w/ culture", "SM2 w/ culture", "SM3 w/ culture", "SM4 w/ culture", "SM5 w/ culture", "SM6 w/ culture", "SM7 w/ culture", "SM8 w/ culture"))
#print(plot_gov)

    # store away models then clean env to free RAM for further analyses
save(list = ls(pattern = "m_gov"), file = "populism_replication_models_gov.Rdata")
rm(list = ls(pattern = "m_gov"))
gc()


    ##### Fig2 10 Aggregated effects: pariah parties are sender ----


m_pariah1 <-
    lmer(pop_gmean_m.i ~                                                                                   # depvar: level of populism of party i at time t
    l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
    v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
    KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
    e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
    splag_pariah_wnone +                                                                                   # Aggregated diffusion effects
    pop_gmean_m.j * sender_pariah +                                                      			       # direct diffusion effects (interaction)
    v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

m_pariah2 <-
    lmer(pop_gmean_m.i ~                                                                                   # depvar: level of populism of party i at time t
    l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
    v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
    KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
    e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
    splag_pariah_wpsplats +                                                                                # Aggregated diffusion effects
    pop_gmean_m.j * dysim_v2psplats_ma3 * sender_pariah +                    			                   # direct diffusion effects (interaction)
    v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

m_pariah3 <-
    lmer(pop_gmean_m.i ~                                                                                   # depvar: level of populism of party i at time t
    l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
    v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
    KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
    e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
    splag_pariah_wpubcorr +                                                                                # Aggregated diffusion effects
    pop_gmean_m.j * dysim_v2x_pubcorr_ma3 * sender_pariah +							                       # direct diffusion effects (interaction)
    v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

m_pariah4 <-
    lmer(pop_gmean_m.i ~                                                                                   # depvar: level of populism of party i at time t
    l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
    v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
    KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
    e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
    splag_pariah_wexlecon +                                                                                # Aggregated diffusion effects
    pop_gmean_m.j * dysim_v2xpe_exlecon_ma3 * sender_pariah +                			                   # direct diffusion effects (interaction)
    v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

m_pariah5 <-
    lmer(pop_gmean_m.i ~                                                                                   # depvar: level of populism of party i at time t
    l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
    v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
    KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
    e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
    splag_pariah_wexlpol +                                                                                 # Aggregated diffusion effects
    pop_gmean_m.j * dysim_v2xpe_exlpol_ma3 * sender_pariah +                			                   # direct diffusion effects (interaction)
    v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

m_pariah6 <-
    lmer(pop_gmean_m.i ~                                                                                   # depvar: level of populism of party i at time t
    l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
    v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
    KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
    e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
    splag_pariah_wgini +                                                                                   # Aggregated diffusion effects
    pop_gmean_m.j * dysim_gini_mkt_ipol_ma3 * sender_pariah +                			                   # direct diffusion effects (interaction)
    v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

m_pariah7 <-
    lmer(pop_gmean_m.i ~                                                                                   # depvar: level of populism of party i at time t
    l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
    v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
    KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
    e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
    splag_pariah_wKOFFi +                                                                                  # Aggregated diffusion effects
    pop_gmean_m.j * dysim_KOFFiGIdf_ma3 * sender_pariah +                    			                   # direct diffusion effects (interaction)
    v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

m_pariah8 <-
    lmer(pop_gmean_m.i ~                                                                                   # depvar: level of populism of party i at time t
    l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
    v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
    KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
    e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
    splag_pariah_wKOFTr +                                                                                  # Aggregated diffusion effects
    pop_gmean_m.j * dysim_KOFTrGIdf_ma3 * sender_pariah +                    			                   # direct diffusion effects (interaction)
    v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

m_pariah9 <-
    lmer(pop_gmean_m.i ~                                                                                   # depvar: level of populism of party i at time t
    l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
    v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
    KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
    e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
    splag_pariah_wnone + splag_pariah_wpsplats + splag_pariah_wpubcorr + splag_pariah_wexlecon + splag_pariah_wexlpol + splag_pariah_wgini + splag_pariah_wKOFFi + splag_pariah_wKOFTr + # Aggregated diffusion effects
    pop_gmean_m.j * sender_pariah +                      			                                       # direct diffusion effects (interaction)
    v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs


##### Fig2 11 Aggregated effects: pariah parties are sender + weighted by cultural similarity ----


m_pariahwcult1 <-
    lmer(pop_gmean_m.i ~                                                                                   # depvar: level of populism of party i at time t
    l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
    v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
    KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
    e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
    splag_pariahwcult_wnone +                                                                              # Aggregated diffusion effects
    pop_gmean_m.j * cultural_similarity * sender_pariah +                                             	   # direct diffusion effects (interaction)
    v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

m_pariahwcult2 <-
    lmer(pop_gmean_m.i ~                                                                                   # depvar: level of populism of party i at time t
    l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
    v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
    KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
    e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
    splag_pariahwcult_wpsplats +                                                                           # Aggregated diffusion effects
    pop_gmean_m.j * dysim_v2psplats_ma3 * cultural_similarity * sender_pariah +		                       # direct diffusion effects (interaction)
    v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

m_pariahwcult3 <-
    lmer(pop_gmean_m.i ~                                                                                   # depvar: level of populism of party i at time t
    l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
    v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
    KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
    e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
    splag_pariahwcult_wpubcorr +                                                                           # Aggregated diffusion effects
    pop_gmean_m.j * dysim_v2x_pubcorr_ma3 * cultural_similarity * sender_pariah +		                   # direct diffusion effects (interaction)
    v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

m_pariahwcult4 <-
    lmer(pop_gmean_m.i ~                                                                                   # depvar: level of populism of party i at time t
    l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
    v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
    KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
    e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
    splag_pariahwcult_wexlecon +                                                                           # Aggregated diffusion effects
    pop_gmean_m.j * dysim_v2xpe_exlecon_ma3 * cultural_similarity * sender_pariah +	                       # direct diffusion effects (interaction)
    v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

m_pariahwcult5 <-
    lmer(pop_gmean_m.i ~                                                                                   # depvar: level of populism of party i at time t
    l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
    v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
    KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
    e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
    splag_pariahwcult_wexlpol +                                                                            # Aggregated diffusion effects
    pop_gmean_m.j * dysim_v2xpe_exlpol_ma3 * cultural_similarity * sender_pariah +		                   # direct diffusion effects (interaction)
    v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

m_pariahwcult6 <-
    lmer(pop_gmean_m.i ~                                                                                   # depvar: level of populism of party i at time t
    l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
    v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
    KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
    e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
    splag_pariahwcult_wgini +                                                                              # Aggregated diffusion effects
    pop_gmean_m.j * dysim_gini_mkt_ipol_ma3 * cultural_similarity * sender_pariah +	                       # direct diffusion effects (interaction)
    v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

m_pariahwcult7 <-
    lmer(pop_gmean_m.i ~                                                                                   # depvar: level of populism of party i at time t
    l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
    v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
    KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
    e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
    splag_pariahwcult_wKOFFi +                                                                             # Aggregated diffusion effects
    pop_gmean_m.j * dysim_KOFFiGIdf_ma3 * cultural_similarity * sender_pariah +		                       # direct diffusion effects (interaction)
    v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

m_pariahwcult8 <-
    lmer(pop_gmean_m.i ~                                                                                   # depvar: level of populism of party i at time t
    l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
    v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
    KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
    e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
    splag_pariahwcult_wKOFTr +                                                                             # Aggregated diffusion effects
    pop_gmean_m.j * dysim_KOFTrGIdf_ma3 * cultural_similarity * sender_pariah +		                       # direct diffusion effects (interaction)
    v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs

m_pariahwcult9 <-
    lmer(pop_gmean_m.i ~                                                                                   # depvar: level of populism of party i at time t
    l_enemy_pop_gmean_m.i + powercon.i + orgext.i + v2pariglef_mean.i +                                    # party level
    v2psplats_ma3.i + v2x_pubcorr_ma3.i + v2xpe_exlecon_ma3.i + v2xpe_exlpol_ma3.i + gini_mkt_ipol_ma3.i + # grievances
    KOFFiGIdf_ma3.i + KOFTrGIdf_ma3.i +                                                                    # globalization
    e_migdpgro_ln_ipol_ma3.i + e_miinflat_ln_ipol_ma3.i + e_migdppc_ln_ipol_ma3.i +                        # economic situation
    splag_pariahwcult_wnone + splag_pariahwcult_wpsplats + splag_pariahwcult_wpubcorr + splag_pariahwcult_wexlecon + splag_pariahwcult_wexlpol + splag_pariahwcult_wgini + splag_pariahwcult_wKOFFi + splag_pariahwcult_wKOFTr + # Aggregated diffusion effects
    pop_gmean_m.j * cultural_similarity * sender_pariah +                     			                   # direct diffusion effects (interaction)
    v2x_regime.i + decade.i + (1 | country_id.i/v2paid.i), data = dyadic_df)                               # regime, decade FEs, and ML specs


    ##### Fig2 12 Aggregated effects: pariah parties are sender - plot ----

    # load after initial run when tweaking the plot so to avoid re-running the models on a machine with less RAM
    #load("populism_replication_models_pariah.Rdata") 
    
    # set scale = TRUE to obtain standardized coeffs
plot_pariah <- 
    plot_summs(m_pariah1, m_pariah2, m_pariah3, m_pariah4, m_pariah5, m_pariah6, m_pariah7, m_pariah8,
               m_pariahwcult1, m_pariahwcult2, m_pariahwcult3, m_pariahwcult4, m_pariahwcult5, m_pariahwcult6, m_pariahwcult7, m_pariahwcult8,
               scale = TRUE, plot.distributions = FALSE, ci_level = .999,
               coefs = c("Distinct party platforms" = "splag_pariah_wpsplats",
                         "Public sector corruption" = "splag_pariah_wpubcorr",
                         "Economic exclusion"       = "splag_pariah_wexlecon",
                         "Political exclusion"      = "splag_pariah_wexlpol",
                         "Income inequality"        = "splag_pariah_wgini",
                         "Financial globalization"  = "splag_pariah_wKOFFi",
                         "Trade globalization"      = "splag_pariah_wKOFTr",
                         "Unweighted"               = "splag_pariah_wnone",
                         "Distinct party platforms" = "splag_pariahwcult_wpsplats",
                         "Public sector corruption" = "splag_pariahwcult_wpubcorr",
                         "Economic exclusion"       = "splag_pariahwcult_wexlecon",
                         "Political exclusion"      = "splag_pariahwcult_wexlpol",
                         "Income inequality"        = "splag_pariahwcult_wgini",
                         "Financial globalization"  = "splag_pariahwcult_wKOFFi",
                         "Trade globalization"      = "splag_pariahwcult_wKOFTr",
                         "Unweighted"               = "splag_pariahwcult_wnone"),
               colors = c("dodgerblue",  "dodgerblue",  "dodgerblue",  "dodgerblue",  "dodgerblue",  "dodgerblue",  "dodgerblue",  "dodgerblue",
                          "gold",  "gold",  "gold",  "gold",  "gold",  "gold",  "gold",  "gold"),
               point.shape = c(1:8, 1:8),
               model.names = c("SM1", "SM2", "SM3", "SM4", "SM5", "SM6", "SM7", "SM8",
                               "SM1 w/ culture", "SM2 w/ culture", "SM3 w/ culture", "SM4 w/ culture", "SM5 w/ culture", "SM6 w/ culture", "SM7 w/ culture", "SM8 w/ culture"))
#print(plot_pariah)

# store away models then clean env to free RAM for further analyses
save(list = ls(pattern = "m_pariah"), file = "populism_replication_models_pariah.Rdata")
rm(list = ls(pattern = "m_pariah"))
gc()



    ##### Fig2 13 Spatial lags: combine plots ----

plot_all_mod    <- plot_all    + labs(title = "Sender: All parties")                    + theme(axis.text.y = element_text(hjust = 0, size = 8, color = "black"), axis.text.x = element_text(size = 6, color = "black"), axis.title.x = element_text(size = 8, color = "black"), plot.title = element_text(hjust = 0.5, size = 10, color = "black"), plot.title.position = 'panel') + coord_cartesian(xlim = c(-0.4, 0.4))
plot_pfam_mod   <- plot_pfam   + labs(title = "Sender: Parties from same party family") + theme(axis.text.y = element_text(hjust = 0, size = 8, color = "black"), axis.text.x = element_text(size = 6, color = "black"), axis.title.x = element_text(size = 8, color = "black"), plot.title = element_text(hjust = 0.5, size = 10, color = "black"), plot.title.position = 'panel') + coord_cartesian(xlim = c(-0.4, 0.4))
plot_gov_mod    <- plot_gov    + labs(title = "Sender: Governing parties")              + theme(axis.text.y = element_text(hjust = 0, size = 8, color = "black"), axis.text.x = element_text(size = 6, color = "black"), axis.title.x = element_text(size = 8, color = "black"), plot.title = element_text(hjust = 0.5, size = 10, color = "black"), plot.title.position = 'panel') + coord_cartesian(xlim = c(-0.4, 0.4))
plot_pariah_mod <- plot_pariah + labs(title = "Sender: Pariah parties")                 + theme(axis.text.y = element_text(hjust = 0, size = 8, color = "black"), axis.text.x = element_text(size = 6, color = "black"), axis.title.x = element_text(size = 8, color = "black"), plot.title = element_text(hjust = 0.5, size = 10, color = "black"), plot.title.position = 'panel') + coord_cartesian(xlim = c(-0.4, 0.4))


    # output Figure 2
plot_out     <- ggarrange(plot_all_mod, plot_pfam_mod, ncol = 1, align = "hv", common.legend = TRUE, legend = "none")
plot_out_si1 <- ggarrange(plot_gov_mod,                ncol = 1, align = "hv", common.legend = TRUE, legend = "none")
plot_out_si2 <- ggarrange(plot_pariah_mod,             ncol = 1, align = "hv", common.legend = TRUE, legend = "none")

#graph2svg(x = plot_out, file = "figure2_coeffplot.svg", width = 8, height = 8) # svg for print
graph2png(x = plot_out,     file = "figure2_coeffplot.png", width = 8, height = 8) # png for submission
graph2png(x = plot_out_si1, file = "suppl_fig_gov.png"    , width = 8, height = 4) # png for submission
graph2png(x = plot_out_si2, file = "suppl_fig_pariah.png" , width = 8, height = 4) # png for submission



    # clean env to free RAM for further analyses
section3_end <- Sys.time()
difftime(section3_end, section3_start, units="mins")
rm(list=setdiff(ls(), "dyadic_df"))
gc()





#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



##### Figure 3  - Interaction Plots (Dyadic) ----


    # load pfam models saved above so they do not have to be estimated again...
load("populism_replication_models_pfam.Rdata")


    # keep only necessary objects to reduce RAM usage
rm(list = setdiff(ls(), c("dyadic_df", "m_pfamwcult3", "m_pfamwcult4", "m_pfamwcult5", "m_pfamwcult6")))
section4_start <- Sys.time()
gc()


    ##### Fig3 1 Direct effect: public sector corruption (model: m_pfamwcult3) - marginal effects ----


        ## set context similarity constant
mdyad_v2x_pubcorr_cult_fm_margins1 <-
    margins(m_pfamwcult3,
            variables = "pop_gmean_m.j",
            at = list(
                dysim_v2x_pubcorr_ma3 = round(quantile(dyadic_df$dysim_v2x_pubcorr_ma3, c(0.2, 0.8)), 1), # set constant: 20th and 80th decile in full sample
                cultural_similarity = seq(0, 10, by = 2),   											  # over levels of: every two steps to reduce estimation time
                same_pfam = 0:1)                                                                          # by party family
    ) %>%
    tidy(conf.int = TRUE, conf.level = 0.9) %>%
    pivot_wider(names_from = at.variable, values_from = c(at.value, estimate, conf.low, conf.high)) %>%   # re-arrange for better plotting
    rename(
        dydx_pop_gmean_m.j = estimate_cultural_similarity,
        dydx_conf_low      = conf.low_cultural_similarity,
        dydx_conf_high     = conf.high_cultural_similarity
    ) %>%
    select(term, starts_with(c("dydx", "at"))) %>%
    mutate(
        tmp_char = as.character(at.value_dysim_v2x_pubcorr_ma3),
        tmp_char = if_else(tmp_char == as.character(round(quantile(dyadic_df$dysim_v2x_pubcorr_ma3, 0.2), 1)), "20th decile", tmp_char),
        tmp_char = if_else(tmp_char == as.character(round(quantile(dyadic_df$dysim_v2x_pubcorr_ma3, 0.8), 1)), "80th decile", tmp_char),
        at.values_combined = paste0("Same Pfam: ", as.character(at.value_same_pfam), "; Context ", tmp_char)
    )


        ## set cultural similarity constant
mdyad_v2x_pubcorr_cult_fm_margins2 <-
    margins(m_pfamwcult3,
            variables = "pop_gmean_m.j",
            at = list(
                dysim_v2x_pubcorr_ma3 = quantile(dyadic_df$dysim_v2x_pubcorr_ma3, seq(0, 0.9, by = .1)),   # over levels of: Min to 90th decile to reduce estimation time (and avoid outliers)
                cultural_similarity = c(3, 6),                                                             # set constant: 80th and 95th decile in full sample
                same_pfam = 0:1)                                                                           # by party family
    ) %>%
    tidy(conf.int = TRUE, conf.level = 0.9) %>%
    pivot_wider(names_from = at.variable, values_from = c(at.value, estimate, conf.low, conf.high)) %>%    # re-arrange for better plotting
    rename(
        dydx_pop_gmean_m.j = estimate_dysim_v2x_pubcorr_ma3,
        dydx_conf_low      = conf.low_dysim_v2x_pubcorr_ma3,
        dydx_conf_high     = conf.high_dysim_v2x_pubcorr_ma3
    ) %>%
    select(term, starts_with(c("dydx", "at"))) %>%
    mutate(at.values_combined = paste0("Same Pfam: ", as.character(at.value_same_pfam), "; Cultural ", as.character(at.value_cultural_similarity)))


    ##### Fig3 2 Direct effect: economic exclusion (model: m_pfamwcult4) - marginal effects ----


        ## set context similarity constant
mdyad_v2xpe_exlecon_cult_fm_margins1 <-
  margins(m_pfamwcult4,
          variables = "pop_gmean_m.j",
          at = list(
            dysim_v2xpe_exlecon_ma3 = round(quantile(dyadic_df$dysim_v2xpe_exlecon_ma3, c(0.2, 0.8)), 1),  # set constant: 20th and 80th decile in full sample
            cultural_similarity = seq(0, 10, by = 2),   											       # over levels of: every two steps to reduce estimation time
            same_pfam = 0:1)                                                                               # by party family
  ) %>%
  tidy(conf.int = TRUE, conf.level = 0.9) %>%
  pivot_wider(names_from = at.variable, values_from = c(at.value, estimate, conf.low, conf.high)) %>%
  rename(
    dydx_pop_gmean_m.j = estimate_cultural_similarity,
    dydx_conf_low      = conf.low_cultural_similarity,
    dydx_conf_high     = conf.high_cultural_similarity
  ) %>%
  select(term, starts_with(c("dydx", "at"))) %>%
  mutate(
    tmp_char = as.character(at.value_dysim_v2xpe_exlecon_ma3),
    tmp_char = if_else(tmp_char == as.character(round(quantile(dyadic_df$dysim_v2xpe_exlecon_ma3, 0.2), 1)), "20th decile", tmp_char),
    tmp_char = if_else(tmp_char == as.character(round(quantile(dyadic_df$dysim_v2xpe_exlecon_ma3, 0.8), 1)), "80th decile", tmp_char),
    at.values_combined = paste0("Same Pfam: ", as.character(at.value_same_pfam), "; Context ", tmp_char)
  )


        ## set cultural similarity constant
mdyad_v2xpe_exlecon_cult_fm_margins2 <-
  margins(m_pfamwcult4,
          variables = "pop_gmean_m.j",
          at = list(
            dysim_v2xpe_exlecon_ma3 = quantile(dyadic_df$dysim_v2xpe_exlecon_ma3, seq(0, 0.9, by = .1)),  # over levels of: Min to 90th decile to reduce estimation time (and avoid outliers)
            cultural_similarity = c(3, 6),                                                                # set constant: 80th and 95th decile in full sample
            same_pfam = 0:1)                                                                              # by party family
  ) %>%
  tidy(conf.int = TRUE, conf.level = 0.9) %>%
  pivot_wider(names_from = at.variable, values_from = c(at.value, estimate, conf.low, conf.high)) %>%
  rename(
    dydx_pop_gmean_m.j = estimate_dysim_v2xpe_exlecon_ma3,
    dydx_conf_low      = conf.low_dysim_v2xpe_exlecon_ma3,
    dydx_conf_high     = conf.high_dysim_v2xpe_exlecon_ma3
  ) %>%
  select(term, starts_with(c("dydx", "at"))) %>%
  mutate(at.values_combined = paste0("Same Pfam: ", as.character(at.value_same_pfam), "; Cultural ", as.character(at.value_cultural_similarity)))


    ##### Fig3 3 Direct effect: political exclusion (model: m_pfamwcult5) - marginal effects ----


        ## set context similarity constant
mdyad_v2xpe_exlpol_cult_fm_margins1 <-
  margins(m_pfamwcult5,
          variables = "pop_gmean_m.j",
          at = list(
            dysim_v2xpe_exlpol_ma3 = round(quantile(dyadic_df$dysim_v2xpe_exlpol_ma3, c(0.2, 0.8)), 1),	   # set constant: 20th and 80th decile in full sample
            cultural_similarity = seq(0, 10, by = 2),   											       # over levels of: every two steps to reduce estimation time
            same_pfam = 0:1)                                                                               # by party family
  ) %>%
  tidy(conf.int = TRUE, conf.level = 0.9) %>%
  pivot_wider(names_from = at.variable, values_from = c(at.value, estimate, conf.low, conf.high)) %>%
  rename(
    dydx_pop_gmean_m.j = estimate_cultural_similarity,
    dydx_conf_low      = conf.low_cultural_similarity,
    dydx_conf_high     = conf.high_cultural_similarity
  ) %>%
  select(term, starts_with(c("dydx", "at"))) %>%
  mutate(
    tmp_char = as.character(at.value_dysim_v2xpe_exlpol_ma3),
    tmp_char = if_else(tmp_char == as.character(round(quantile(dyadic_df$dysim_v2xpe_exlpol_ma3, 0.2), 1)), "20th decile", tmp_char),
    tmp_char = if_else(tmp_char == as.character(round(quantile(dyadic_df$dysim_v2xpe_exlpol_ma3, 0.8), 1)), "80th decile", tmp_char),
    at.values_combined = paste0("Same Pfam: ", as.character(at.value_same_pfam), "; Context ", tmp_char)
  )


        ## set cultural similarity constant
mdyad_v2xpe_exlpol_cult_fm_margins2 <-
  margins(m_pfamwcult5,
          variables = "pop_gmean_m.j",
          at = list(
            dysim_v2xpe_exlpol_ma3 = quantile(dyadic_df$dysim_v2xpe_exlpol_ma3, seq(0, 0.9, by = .1)),   # over levels of: Min to 90th decile to reduce estimation time (and avoid outliers)
            cultural_similarity = c(3, 6),                                                               # set constant: 80th and 95th decile in full sample
            same_pfam = 0:1)                                                                             # by party family
  ) %>%
  tidy(conf.int = TRUE, conf.level = 0.9) %>%
  pivot_wider(names_from = at.variable, values_from = c(at.value, estimate, conf.low, conf.high)) %>%
  rename(
    dydx_pop_gmean_m.j = estimate_dysim_v2xpe_exlpol_ma3,
    dydx_conf_low      = conf.low_dysim_v2xpe_exlpol_ma3,
    dydx_conf_high     = conf.high_dysim_v2xpe_exlpol_ma3
  ) %>%
  select(term, starts_with(c("dydx", "at"))) %>%
  mutate(at.values_combined = paste0("Same Pfam: ", as.character(at.value_same_pfam), "; Cultural ", as.character(at.value_cultural_similarity)))


    ##### Fig3 4 Direct effect: income inequality (model: m_pfamwcult6) - marginal effects ----


        ## set context similarity constant
mdyad_gini_mkt_ipol_cult_fm_margins1 <-
  margins(m_pfamwcult6,
          variables = "pop_gmean_m.j",
          at = list(
            dysim_gini_mkt_ipol_ma3 = round(quantile(dyadic_df$dysim_gini_mkt_ipol_ma3, c(0.2, 0.8)), 1),  # set constant: 20th and 80th decile in full sample
            cultural_similarity = seq(0, 10, by = 2),   											       # over levels of: every two steps to reduce estimation time
            same_pfam = 0:1)                                                                               # by party family
  ) %>%
  tidy(conf.int = TRUE, conf.level = 0.9) %>%
  pivot_wider(names_from = at.variable, values_from = c(at.value, estimate, conf.low, conf.high)) %>%
  rename(
    dydx_pop_gmean_m.j = estimate_cultural_similarity,
    dydx_conf_low      = conf.low_cultural_similarity,
    dydx_conf_high     = conf.high_cultural_similarity
  ) %>%
  select(term, starts_with(c("dydx", "at"))) %>%
  mutate(
    tmp_char = as.character(at.value_dysim_gini_mkt_ipol_ma3),
    tmp_char = if_else(tmp_char == as.character(round(quantile(dyadic_df$dysim_gini_mkt_ipol_ma3, 0.2), 1)), "20th decile", tmp_char),
    tmp_char = if_else(tmp_char == as.character(round(quantile(dyadic_df$dysim_gini_mkt_ipol_ma3, 0.8), 1)), "80th decile", tmp_char),
    at.values_combined = paste0("Same Pfam: ", as.character(at.value_same_pfam), "; Context ", tmp_char)
  )


        ## set cultural similarity constant
mdyad_gini_mkt_ipol_cult_fm_margins2 <-
  margins(m_pfamwcult6,
          variables = "pop_gmean_m.j",
          at = list(
            dysim_gini_mkt_ipol_ma3 = quantile(dyadic_df$dysim_gini_mkt_ipol_ma3, seq(0, 0.9, by = .1)),  # over levels of: Min to 90th decile to reduce estimation time (and avoid outliers)
            cultural_similarity = c(3, 6),                                                                # set constant: 80th and 95th decile in full sample
            same_pfam = 0:1)                                                                              # by party family
  ) %>%
  tidy(conf.int = TRUE, conf.level = 0.9) %>%
  pivot_wider(names_from = at.variable, values_from = c(at.value, estimate, conf.low, conf.high)) %>%
  rename(
    dydx_pop_gmean_m.j = estimate_dysim_gini_mkt_ipol_ma3,
    dydx_conf_low      = conf.low_dysim_gini_mkt_ipol_ma3,
    dydx_conf_high     = conf.high_dysim_gini_mkt_ipol_ma3
  ) %>%
  select(term, starts_with(c("dydx", "at"))) %>%
  mutate(at.values_combined = paste0("Same Pfam: ", as.character(at.value_same_pfam), "; Cultural ", as.character(at.value_cultural_similarity)))



    # store away objects and clean env to free RAM for plotting
save(list = ls(pattern = "mdyad"), file = "populism_replication_models_margins.Rdata")
rm(dyadic_df, m_pfamwcult3, m_pfamwcult4, m_pfamwcult5, m_pfamwcult6)
gc()


  # load margin objects saved above so they do not have to be estimated again in case of changes to the figure...
#load("populism_replication_models_margins.Rdata")


    ##### Fig3 5 Direct effect: public sector corruption - plot ----


        ## with context similarity constant
mdyad_v2x_pubcorr_cult_fm_plot1 <-
    mdyad_v2x_pubcorr_cult_fm_margins1 %>%
    ggplot(aes(
        x = at.value_cultural_similarity,
        y = dydx_pop_gmean_m.j)) +
    geom_ribbon(aes(
        ymin = dydx_conf_low,
        ymax = dydx_conf_high,
        fill = factor(at.values_combined)),
        alpha = 0.5) +
    geom_line(aes(
        color = factor(at.values_combined)),
        show.legend = FALSE,
        size = 2) +
    coord_cartesian(ylim = c(-0.01, 0.01)) +
    geom_hline(yintercept = 0) +
    theme_classic(base_size = 8) +
    scale_color_brewer(palette="Set1")


        ## with cultural similarity constant
mdyad_v2x_pubcorr_cult_fm_plot2 <-
    mdyad_v2x_pubcorr_cult_fm_margins2 %>%
    ggplot(aes(
        x = at.value_dysim_v2x_pubcorr_ma3,
        y = dydx_pop_gmean_m.j)) +
    geom_ribbon(aes(
        ymin = dydx_conf_low,
        ymax = dydx_conf_high,
        fill = factor(at.values_combined)),
        alpha = 0.5) +
    geom_line(aes(
        color = factor(at.values_combined)),
        show.legend = FALSE,
        size = 2) +
    coord_cartesian(ylim = c(-0.01, 0.01)) +
    geom_hline(yintercept = 0) +
    theme_classic(base_size = 8) +
    scale_color_brewer(palette="Set1")


    ##### Fig3 6 Direct effect: economic exclusion - plot ----


        ## with context similarity constant
mdyad_v2xpe_exlecon_cult_fm_plot1 <-
    mdyad_v2xpe_exlecon_cult_fm_margins1 %>%
    ggplot(aes(
        x = at.value_cultural_similarity,
        y = dydx_pop_gmean_m.j)) +
    geom_ribbon(aes(
        ymin = dydx_conf_low,
        ymax = dydx_conf_high,
        fill = factor(at.values_combined)),
        alpha = 0.5) +
    geom_line(aes(
        color = factor(at.values_combined)),
        show.legend = FALSE,
        size = 2) +
    coord_cartesian(ylim = c(-0.01, 0.01)) +
    geom_hline(yintercept = 0) +
    theme_classic(base_size = 8) +
    scale_color_brewer(palette="Set1")


        ## with cultural similarity constant
mdyad_v2xpe_exlecon_cult_fm_plot2 <-
    mdyad_v2xpe_exlecon_cult_fm_margins2 %>%
    ggplot(aes(
        x = at.value_dysim_v2xpe_exlecon_ma3,
        y = dydx_pop_gmean_m.j)) +
    geom_ribbon(aes(
        ymin = dydx_conf_low,
        ymax = dydx_conf_high,
        fill = factor(at.values_combined)),
        alpha = 0.5) +
    geom_line(aes(
        color = factor(at.values_combined)),
        show.legend = FALSE,
        size = 2) +
    coord_cartesian(ylim = c(-0.01, 0.01)) +
    geom_hline(yintercept = 0) +
    theme_classic(base_size = 8) +
    scale_color_brewer(palette="Set1")


    ##### Fig3 7 Direct effect: political exclusion - plot ----


        ## with context similarity constant
mdyad_v2xpe_exlpol_cult_fm_plot1 <-
    mdyad_v2xpe_exlpol_cult_fm_margins1 %>%
    ggplot(aes(
        x = at.value_cultural_similarity,
        y = dydx_pop_gmean_m.j)) +
    geom_ribbon(aes(
        ymin = dydx_conf_low,
        ymax = dydx_conf_high,
        fill = factor(at.values_combined)),
        alpha = 0.5) +
    geom_line(aes(
        color = factor(at.values_combined)),
        show.legend = FALSE,
        size = 2) +
    coord_cartesian(ylim = c(-0.01, 0.01)) +
    geom_hline(yintercept = 0) +
    theme_classic(base_size = 8) +
    scale_color_brewer(palette="Set1")


        ## with cultural similarity constant
mdyad_v2xpe_exlpol_cult_fm_plot2 <-
    mdyad_v2xpe_exlpol_cult_fm_margins2 %>%
    ggplot(aes(
        x = at.value_dysim_v2xpe_exlpol_ma3,
        y = dydx_pop_gmean_m.j)) +
    geom_ribbon(aes(
        ymin = dydx_conf_low,
        ymax = dydx_conf_high,
        fill = factor(at.values_combined)),
        alpha = 0.5) +
    geom_line(aes(
        color = factor(at.values_combined)),
        show.legend = FALSE,
        size = 2) +
    coord_cartesian(ylim = c(-0.01, 0.01)) +
    geom_hline(yintercept = 0) +
    theme_classic(base_size = 8) +
    scale_color_brewer(palette="Set1")


    ##### Fig3 8 Direct effect: income inequality - plot ----


        ## with context similarity constant
mdyad_gini_mkt_ipol_cult_fm_plot1 <-
    mdyad_gini_mkt_ipol_cult_fm_margins1 %>%
    ggplot(aes(
        x = at.value_cultural_similarity,
        y = dydx_pop_gmean_m.j)) +
    geom_ribbon(aes(
        ymin = dydx_conf_low,
        ymax = dydx_conf_high,
        fill = factor(at.values_combined)),
        alpha = 0.5) +
    geom_line(aes(
        color = factor(at.values_combined)),
        show.legend = FALSE,
        size = 2) +
    coord_cartesian(ylim = c(-0.01, 0.01)) +
    geom_hline(yintercept = 0) +
    theme_classic(base_size = 8) +
    scale_color_brewer(palette="Set1")


        ## with cultural similarity constant
mdyad_gini_mkt_ipol_cult_fm_plot2 <-
    mdyad_gini_mkt_ipol_cult_fm_margins2 %>%
    ggplot(aes(
        x = at.value_dysim_gini_mkt_ipol_ma3,
        y = dydx_pop_gmean_m.j)) +
    geom_ribbon(aes(
        ymin = dydx_conf_low,
        ymax = dydx_conf_high,
        fill = factor(at.values_combined)),
        alpha = 0.5) +
    geom_line(aes(
        color = factor(at.values_combined)),
        show.legend = FALSE,
        size = 2) +
    coord_cartesian(ylim = c(-0.01, 0.01)) +
    geom_hline(yintercept = 0) +
    theme_classic(base_size = 8) +
    scale_color_brewer(palette="Set1")


    ##### Fig3 9 Direct effect: combine plots ----


    ## top row -> with cultural similarity constant
top1 <-
    mdyad_v2x_pubcorr_cult_fm_plot2 +
    labs(title = "Public sector corruption", x = "Context similarity", y = "", fill = "") +
    guides(fill = guide_legend(nrow = 4, byrow = TRUE)) +
    theme(
        legend.position = 'none',
        axis.title.x    = element_text(size = 8, margin = margin(t = 5)),
        axis.title.y    = element_blank(),
        axis.text.x     = element_text(color = "black"),
        axis.text.y     = element_blank(),
        axis.ticks.y    = element_line(color = "black"),
        plot.title      = element_text(hjust = 0.5, size = 12)
    )
#print(top1)

top2 <-
    mdyad_v2xpe_exlecon_cult_fm_plot2 +
    labs(title = "Economic exclusion", x = "Context similarity", y = "", fill = "") +
    guides(fill = guide_legend(nrow = 4, byrow = TRUE)) +
    theme(
        legend.position = 'none',
        axis.title.x    = element_text(size = 8, margin = margin(t = 5)),
        axis.title.y    = element_blank(),
        axis.text.x     = element_text(color = "black"),
        axis.text.y     = element_blank(),
        axis.ticks.y    = element_line(color = "black"),
        plot.title      = element_text(hjust = 0.5, size = 12)
    )
#print(top2)

top3 <-
    mdyad_v2xpe_exlpol_cult_fm_plot2 +
    labs(title = "Political exclusion", x = "Context similarity", y = "", fill = "") +
    guides(fill = guide_legend(nrow = 4, byrow = TRUE)) +
    theme(
        legend.position = 'none',
        axis.title.x    = element_text(size = 8, margin = margin(t = 5)),
        axis.title.y    = element_blank(),
        axis.text.x     = element_text(color = "black"),
        axis.text.y     = element_blank(),
        axis.ticks.y    = element_line(color = "black"),
        plot.title      = element_text(hjust = 0.5, size = 12)
    )
#print(top3)

top4 <-
    mdyad_gini_mkt_ipol_cult_fm_plot2 +
    labs(title = "Income inequality", x = "Context similarity", y = "", fill = "") +
    guides(fill = guide_legend(nrow = 4, byrow = TRUE)) +
    theme(
        legend.position = 'none',
        axis.title.x    = element_text(size = 8, margin = margin(t = 5)),
        axis.title.y    = element_blank(),
        axis.text.x     = element_text(color = "black"),
        axis.text.y     = element_blank(),
        axis.ticks.y    = element_line(color = "black"),
        plot.title      = element_text(hjust = 0.5, size = 12)
    )
#print(top4)


    # trick: create empty plots with y axis and legend only so it better aligns when put together
top_empty_yaxis <-
    mdyad_gini_mkt_ipol_cult_fm_margins2 %>%
    mutate(
        at.value_dysim_gini_mkt_ipol_ma3 = 0.0005,
        dydx_pop_gmean_m.j = 0.0005
    ) %>%
    ggplot(aes(x = at.value_dysim_gini_mkt_ipol_ma3, y = dydx_pop_gmean_m.j)) +
    coord_cartesian(ylim = c(-0.01, 0.01), xlim = c(-0.001, 0.001)) +
    theme_classic(base_size = 8) +
    theme(
        legend.position = 'none',
        axis.title.x    = element_text(size = 8, margin = margin(t = 5), color = "white"),
        axis.text.x     = element_text(color = "white"),
        axis.ticks.x    = element_line(color = "white"),
        axis.line.x     = element_line(color = "white"), 
        axis.title.y    = element_text(size = 10, margin = margin(r = 10)),
        axis.text.y     = element_text(color = "black"),
        axis.ticks.y    = element_line(color = "white"),
        axis.line.y     = element_line(color = "white"), 
        plot.title      = element_text(hjust = 0.5, size = 12)
    ) +
    labs(title = "", x = "", y = "Effect of sender level of populism (dydx)", fill = "") +
    guides(fill = guide_legend(nrow = 4, byrow = TRUE))
#print(top_empty_yaxis)   
    

top_empty_legend <-
    mdyad_gini_mkt_ipol_cult_fm_margins2 %>%
    mutate(
        at.values_combined = str_replace(at.values_combined, "Same Pfam:", "Party family:"),
        at.values_combined = str_replace(at.values_combined, "Cultural Similarity:", "culture:"),
    ) %>%
    ggplot(aes(x = at.value_dysim_gini_mkt_ipol_ma3, y = dydx_pop_gmean_m.j, color = factor(at.values_combined))) +
    geom_point() +
    lims(x = c(0,0), y = c(0,0)) +
    scale_x_discrete(expand = c(0,0)) +
    theme_void(base_size = 8) +
    theme(
        legend.position = "inside",
        legend.position.inside = c(0.4, 0.25),
        legend.text = element_text(size = 7),
        legend.title = element_blank()
    ) +
    guides(colour = guide_legend(override.aes = list(size = 5)))
#print(top_empty_legend)

    # arrange top row
plot_top <- ggarrange(top_empty_yaxis, top1, top2, top3, top4, top_empty_legend, nrow = 1, widths = c(0.3, 1, 1, 1, 1, 0.7))
#print(plot_top)


    ## bottom row -> with context similarity constant
bottom1 <-
    mdyad_v2x_pubcorr_cult_fm_plot1 +
    labs(title = "", x = "Cultural similarity", y = "", fill = "") +
    theme(
        legend.position = 'none',
        axis.title.x    = element_text(size = 8, margin = margin(t = 5)),
        axis.title.y    = element_blank(),
        axis.text.x     = element_text(color = "black"),
        axis.text.y     = element_blank(),
        axis.ticks.y    = element_line(color = "black"),
        plot.title      = element_text(hjust = 0.5, size = 12)
    )
#print(bottom1)

bottom2 <-
    mdyad_v2xpe_exlecon_cult_fm_plot1 +
    labs(title = "", x = "Cultural similarity", y = "", fill = "") +
    theme(
        legend.position = 'none',
        axis.title.x    = element_text(size = 8, margin = margin(t = 5)),
        axis.title.y    = element_blank(),
        axis.text.x     = element_text(color = "black"),
        axis.text.y     = element_blank(),
        axis.ticks.y    = element_line(color = "black"),
        plot.title      = element_text(hjust = 0.5, size = 12)
    )
#print(bottom2)

bottom3 <-
    mdyad_v2xpe_exlpol_cult_fm_plot1 +
    labs(title = "", x = "Cultural similarity", y = "", fill = "") +
    theme(
        legend.position = 'none',
        axis.title.x    = element_text(size = 8, margin = margin(t = 5)),
        axis.title.y    = element_blank(),
        axis.text.x     = element_text(color = "black"),
        axis.text.y     = element_blank(),
        axis.ticks.y    = element_line(color = "black"),
        plot.title      = element_text(hjust = 0.5, size = 12)
    )
#print(bottom3)

bottom4 <-
    mdyad_gini_mkt_ipol_cult_fm_plot1 +
    labs(title = "", x = "Cultural similarity", y = "", fill = "") +
    theme(
        legend.position = 'none',
        axis.title.x    = element_text(size = 8, margin = margin(t = 5)),
        axis.title.y    = element_blank(),
        axis.text.x     = element_text(color = "black"),
        axis.text.y     = element_blank(),
        axis.ticks.y    = element_line(color = "black"),
        plot.title      = element_text(hjust = 0.5, size = 12)
    )
#print(bottom4)

    # trick: create empty plot with legend only so it better aligns when put together
bottom_empty_yaxis <-
    mdyad_gini_mkt_ipol_cult_fm_margins2 %>%
    mutate(
        at.value_dysim_gini_mkt_ipol_ma3 = 0.0005,
        dydx_pop_gmean_m.j = 0.0005
    ) %>%
    ggplot(aes(x = at.value_dysim_gini_mkt_ipol_ma3, y = dydx_pop_gmean_m.j)) +
    coord_cartesian(ylim = c(-0.01, 0.01), xlim = c(-0.001, 0.001)) +
    theme_classic(base_size = 8) +
    theme(
        legend.position = 'none',
        axis.title.x    = element_text(size = 8, margin = margin(t = 5), color = "white"),
        axis.text.x     = element_text(color = "white"),
        axis.ticks.x    = element_line(color = "white"),
        axis.line.x     = element_line(color = "white"), 
        axis.title.y    = element_text(size = 10, margin = margin(r = 10)),
        axis.text.y     = element_text(color = "black"),
        axis.ticks.y    = element_line(color = "white"),
        axis.line.y     = element_line(color = "white"), 
        plot.title      = element_text(hjust = 0.5, size = 12)
    ) +
    labs(title = "", x = "", y = "Effect of sender level of populism (dydx)", fill = "") +
    guides(fill = guide_legend(nrow = 4, byrow = TRUE))
#print(bottom_empty_yaxis)  

bottom_empty_legend <-
    mdyad_gini_mkt_ipol_cult_fm_margins1 %>%
    mutate(
        at.values_combined = str_replace(at.values_combined, "Same Pfam:", "Party family:"),
        at.values_combined = str_replace(at.values_combined, "Context Similarity:", "context:"),
        at.values_combined = str_replace(at.values_combined, " decile", ""),    
    ) %>%
    ggplot(aes(x = at.value_dysim_gini_mkt_ipol_ma3, y = dydx_pop_gmean_m.j, color = factor(at.values_combined))) +
    geom_point() +
    lims(x = c(0,0), y = c(0,0)) +
    scale_x_discrete(expand = c(0,0)) +
    theme_void(base_size = 8) +
    theme(
        legend.position = "inside",
        legend.position.inside = c(0.4, 0.25),
        legend.text = element_text(size = 7),
        legend.title = element_blank()
    )+
    guides(colour = guide_legend(override.aes = list(size = 5)))
#print(bottom_empty_legend)

    # arrange bottom row
plot_bottom <- ggarrange(bottom_empty_yaxis, bottom1, bottom2, bottom3, bottom4, bottom_empty_legend, nrow = 1, widths = c(0.3, 1, 1, 1, 1, 0.7))
#print(plot_bottom)


    ## output Figure 3
plot_out <- ggarrange(plot_top, plot_bottom, nrow = 2, align = "hv")
#print(plot_out)

#graph2svg(x = plot_out, file = "figure3_margeff.svg", width = 11, height = 8) # svg for print
graph2png(x = plot_out, file = "figure3_margeff.png", width = 11, height = 8) # png for submission



    # clean env to free RAM for further analyses
section4_end <- Sys.time()
difftime(section4_end, section4_start, units="mins")
rm(list = ls())
gc()


	# write session info
devtools::session_info()