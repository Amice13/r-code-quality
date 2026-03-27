##############################
#
# Replication file for the appendix in:
#
# Consolidating Progress:
# The Selection of Female Ministers in Autocracies and Democracies
#
# For publication in the the American Political Science Review
#
# Jacob Nyrup, Hikaru Yamagishi, & Stuart Bramwell
# 
##################

###############################
## Load packages & functions ##
###############################

pacman::p_load(here,tidyverse,Amelia,texreg,lme4,stargazer,lfe,
               compiler,broom,haven,purrr, 
               janitor,xtable)

# Functions

'%!in%' <- function(x,y)!('%in%'(x,y))

theme_custom <- function () {
  
  theme_minimal() +
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black"),
          legend.title = element_blank(),
          legend.position = "bottom",
          plot.title = element_text(size=12),
          plot.subtitle = element_text(size=10),
          axis.title.x = element_text(size=12),
          axis.title.y = element_text(size=12)
    )
}

###############
## Load data ##
###############

df_cross <- read.csv("../1_data/df_consolidatingprogress_V1.csv")

#####
# Appendix C: Alternative measures of democracy ---
#####

# Plot --------------------------------------------------------------------

## Boix ---

# Model 1 - Democracy

bmrlm_1 <- felm(share_female ~ lag_democracy_bmr  | 0 | 0 | country_isocode ,data=df_cross) %>% summary()
df_bmrlm_1 <- tibble(model="Baseline (level)",estimate = bmrlm_1$coefficients[2],stderror = bmrlm_1$coefficients[2,2])

# Model 2 - Democracy (year fixed)

bmrlm_2 <- felm(share_female ~ lag_democracy_bmr | year | 0 | country_isocode ,data=df_cross) %>% summary()
df_bmrlm_2 <- tibble(model="Year (level)",estimate = bmrlm_2$coefficients[1],stderror = bmrlm_2$coefficients[1,2])

# Model 3 - Democracy (year and country)

bmrlm_3 <- felm(share_female ~ lag_democracy_bmr | year + country_isocode | 0 | country_isocode,data=df_cross) %>% summary()
df_bmrlm_3 <- tibble(model="Year and country (level)",estimate = bmrlm_3$coefficients[1],stderror = bmrlm_3$coefficients[1,2])

# Model 4 - Democracy (stock) - 90

bmrlm_4 <- felm(share_female ~ lag_democracy_stock_bmr90 | year + country_isocode | 0 | country_isocode,data=df_cross) %>% summary()
df_bmrlm_4 <- tibble(model="Year and country (Stock, 90 percent)",estimate = bmrlm_4$coefficients[1],stderror = bmrlm_4$coefficients[1,2])

# Model 5 - Democracy (stock) - 95

bmrlm_5 <- felm(share_female ~ lag_democracy_stock_bmr95 | year + country_isocode | 0 | country_isocode,data=df_cross) %>% summary()
df_bmrlm_5 <- tibble(model="Year and country (Stock, 95 percent)",estimate = bmrlm_5$coefficients[1],stderror = bmrlm_5$coefficients[1,2])

bmr_collected <- rbind(df_bmrlm_1,df_bmrlm_2,df_bmrlm_3,df_bmrlm_4,df_bmrlm_5) %>% mutate("democracy" = "Boix, Miller, and Rosato")

## Polity ---

# Model 1 - Democracy

politylm_1 <- felm(share_female ~ lag_polity2  | 0 | 0 | country_isocode ,data=df_cross) %>% summary()
df_politylm_1 <- tibble(model="Baseline (level)",estimate = politylm_1$coefficients[2],stderror = politylm_1$coefficients[2,2])

# Model 2 - Democracy (year fixed)

politylm_2 <- felm(share_female ~ lag_polity2 | year | 0 | country_isocode ,data=df_cross) %>% summary()
df_politylm_2 <- tibble(model="Year (level)",estimate = politylm_2$coefficients[1],stderror = politylm_2$coefficients[1,2])

# Model 3 - Democracy (year and country)

politylm_3 <- felm(share_female ~ lag_polity2 | year + country_isocode | 0 | country_isocode,data=df_cross) %>% summary()
df_politylm_3 <- tibble(model="Year and country (level)",estimate = politylm_3$coefficients[1],stderror = politylm_3$coefficients[1,2])

# Model 4 - Democracy (stock) - 90

politylm_4 <- felm(share_female ~ lag_polity2_stock90 | year + country_isocode | 0 | country_isocode,data=df_cross) %>% summary()
df_politylm_4 <- tibble(model="Year and country (Stock, 90 percent)",estimate = politylm_4$coefficients[1],stderror = politylm_4$coefficients[1,2])

# Model 5 - Democracy (stock) - 95

politylm_5 <- felm(share_female ~ lag_polity2_stock95 | year + country_isocode | 0 | country_isocode,data=df_cross) %>% summary()
df_politylm_5 <- tibble(model="Year and country (Stock, 95 percent)",estimate = politylm_5$coefficients[1],stderror = politylm_5$coefficients[1,2])

polity_collected <- rbind(df_politylm_1,df_politylm_2,df_politylm_3,df_politylm_4,df_politylm_5) %>% mutate("democracy" = "Polity IV")

## DD index ---

# Model 1 - Democracy

ddlm_1 <- felm(share_female ~ lag_democracy_dd  | 0 | 0 | country_isocode ,data=df_cross) %>% summary()
df_ddlm_1 <- tibble(model="Baseline (level)",estimate = ddlm_1$coefficients[2],stderror = ddlm_1$coefficients[2,2])

# Model 2 - Democracy (year fixed)

ddlm_2 <- felm(share_female ~ lag_democracy_dd | year | 0 | country_isocode ,data=df_cross) %>% summary()
df_ddlm_2 <- tibble(model="Year (level)",estimate = ddlm_2$coefficients[1],stderror = ddlm_2$coefficients[1,2])

# Model 3 - Democracy (year and country)

ddlm_3 <- felm(share_female ~ lag_democracy_dd | year + country_isocode | 0 | country_isocode,data=df_cross) %>% summary()
df_ddlm_3 <- tibble(model="Year and country (level)",estimate = ddlm_3$coefficients[1],stderror = ddlm_3$coefficients[1,2])

# Model 4 - Democracy (stock) - 90

ddlm_4 <- felm(share_female ~ lag_democracy_stock_dd90 | year + country_isocode | 0 | country_isocode,data=df_cross) %>% summary()
df_ddlm_4 <- tibble(model="Year and country (Stock, 90 percent)",estimate = ddlm_4$coefficients[1],stderror = ddlm_4$coefficients[1,2])

# Model 5 - Democracy (stock) - 95

ddlm_5 <- felm(share_female ~ lag_democracy_stock_dd95 | year + country_isocode | 0 | country_isocode,data=df_cross) %>% summary()
df_ddlm_5 <- tibble(model="Year and country (Stock, 95 percent)",estimate = ddlm_5$coefficients[1],stderror = ddlm_5$coefficients[1,2])

dd_collected <- rbind(df_ddlm_1,df_ddlm_2,df_ddlm_3,df_ddlm_4,df_ddlm_5) %>% mutate("democracy" = "DD index")

# Bind indices

df_coef_democracy <- rbind(bmr_collected,polity_collected,dd_collected) %>% 
  mutate(std.effect=estimate/stderror,term="Different indices",
         model = fct_relevel(model,"Year and country (Stock, 95 percent)","Year and country (Stock, 90 percent)","Year and country (level)","Year (level)",
                             "Baseline (level)"),
         independent = fct_relevel(democracy,
                                   "Polity IV",
                                   "Boix, Miller, and Rosato",
                                   "DD index"))


pd <- position_dodge(0.75)

appendixc <- ggplot(df_coef_democracy, aes(y = estimate, x = independent, color = model)) + 
  geom_point(position = pd) + 
  geom_linerange(aes(ymin = estimate-1.96*stderror, ymax = estimate+1.96*stderror),
                 lwd = 1/2,
                 position = pd) +
  geom_linerange(aes(ymin = estimate-1.645*stderror, ymax = estimate+1.645*stderror),
                 lwd = 1,
                 position = pd) +
  coord_flip(ylim=c(-10,30)) +
  # ylim(-10,40) +
  labs(x = "", 
       y = "Coefficient estimate",
       color = "") +
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "",plot.title = element_text(size=10),
        axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=10)) +
  scale_color_manual(values=c("#CB2314","#273046","#354823","#FAD510","black")) +
  guides(col=guide_legend(ncol=1,nrow=6)) +
  annotate("text", x = 3.3, y = 15, label = "Baseline (level)",size=3.5) +
  annotate("text", x = 3.15, y = 12, label = "Year (level)",size=3.5) +
  annotate("text", x = 3.0, y = 8.5, label = "Year and country (level)",size=3.5) +
  annotate("text", x = 2.85, y = 15, label = "Year and country (stock, 90%)",size=3.5) +
  annotate("text", x = 2.7, y = 20.5, label = "Year and country (stock, 95%)",size=3.5)

ggsave(
  "../3_output/appendixc.pdf",
  appendixc,
  width = 8,
  height = 6,
  dpi = 1200
)

# Tables ---

## DD index ---

# Model 1 - Democracy

ddlm_1_tab <- felm(share_female ~ lag_democracy_dd  | 0 | 0 | country_isocode, data=df_cross)

# Model 2 - Democracy (year fixed)

ddlm_2_tab <- felm(share_female ~ lag_democracy_dd | year | 0 | country_isocode, data=df_cross)

# Model 3 - Democracy (year and country)

ddlm_3_tab <- felm(share_female ~ lag_democracy_dd | year + country_isocode | 0 | country_isocode, data=df_cross)

# Model 4 - Democracy (stock) - 90

ddlm_4_tab <- felm(share_female ~ lag_democracy_stock_dd90 | year + country_isocode | 0 | country_isocode, data=df_cross)

# Model 5 - Democracy (stock) - 95

ddlm_5_tab <- felm(share_female ~ lag_democracy_stock_dd95 | year + country_isocode | 0 | country_isocode, data=df_cross)

## Print

gof.names <- c("Observations", "R-squared", "Years","Countries")

texreg(list(ddlm_1_tab,ddlm_2_tab,ddlm_3_tab,ddlm_4_tab,ddlm_5_tab),stars = c(0.05),
       file="../3_output/appendixc_tab1.tex",
       custom.model.names=c("(1)","(2)","(3)","(4)","(5)"),
       custom.coef.names=c("Intercept","Polyarchy","Stock of Polyarchy (90%)","Stock of Polyarchy (95%)"),
       include.adjrs = FALSE,
       include.rmse = FALSE,
       include.aic = FALSE,
       include.bic = FALSE,
       include.dic = FALSE,
       include.deviance = FALSE,
       include.loglik = FALSE,
       include.variance = FALSE,
       use.packages = FALSE,
       include.proj.stats = FALSE,
       label = "appendixc_tab1",
       caption ="Table for \\Cref{fig:appendixdifferentdem} - DD index",
       caption.above=TRUE,
       #  single.row = TRUE,
       scalebox = 0.8,
       custom.gof.names = gof.names,
       custom.header = list("Dependent variable: Share of female ministers" = 1:5),
       float.pos = "!htbp",
       custom.gof.rows = list("Within country:"= c("No","No","Yes","Yes","Yes"),
                              "Within year:"= c("No","Yes","Yes","Yes","Yes")),
       custom.note = ("\\parbox{0.75\\textwidth}{\\vspace{2pt}. \\\\
                      \\normalsize %stars. All right-side variables lagged by one year. Country-clustered standard errors in parentheses. Estimator: OLS (ordinary least squares).}"))

## Boix ---

# Model 1 - Democracy

bmrlm_1_tab <- felm(share_female ~ lag_democracy_bmr  | 0 | 0 | country_isocode ,data=df_cross) 

# Model 2 - Democracy (year fixed)

bmrlm_2_tab <- felm(share_female ~ lag_democracy_bmr | year | 0 | country_isocode ,data=df_cross)

# Model 3 - Democracy (year and country)

bmrlm_3_tab <- felm(share_female ~ lag_democracy_bmr | year + country_isocode | 0 | country_isocode,data=df_cross)

# Model 4 - Democracy (stock) - 90

bmrlm_4_tab <- felm(share_female ~ lag_democracy_stock_bmr90 | year + country_isocode | 0 | country_isocode,data=df_cross) 

# Model 5 - Democracy (stock) - 95

bmrlm_5_tab <- felm(share_female ~ lag_democracy_stock_bmr95 | year + country_isocode | 0 | country_isocode,data=df_cross)

## Print

gof.names <- c("Observations", "R-squared", "Years","Countries")

texreg(list(bmrlm_1_tab,bmrlm_2_tab,bmrlm_3_tab,bmrlm_4_tab,bmrlm_5_tab),stars = c(0.05),
       file="../3_output/appendixc_tab2.tex",
       custom.model.names=c("(1)","(2)","(3)","(4)","(5)"),
       custom.coef.names=c("Intercept","Polyarchy","Stock of Polyarchy (90%)","Stock of Polyarchy (95%)"),
       include.adjrs = FALSE,
       include.rmse = FALSE,
       include.aic = FALSE,
       include.bic = FALSE,
       include.dic = FALSE,
       include.deviance = FALSE,
       include.loglik = FALSE,
       include.variance = FALSE,
       use.packages = FALSE,
       include.proj.stats = FALSE,
       label = "appendixc_tab2",
       caption ="Table for \\Cref{fig:appendixdifferentdem} - Boix, Miller, and Rosato",
       caption.above=TRUE,
       #  single.row = TRUE,
       scalebox = 0.8,
       custom.gof.names = gof.names,
       custom.header = list("Dependent variable: Share of female ministers" = 1:5),
       float.pos = "!htbp",
       custom.gof.rows = list("Within country:"= c("No","No","Yes","Yes","Yes"),
                              "Within year:"= c("No","Yes","Yes","Yes","Yes")),
       custom.note = ("\\parbox{0.75\\textwidth}{\\vspace{2pt}. \\\\
                      \\normalsize %stars. All right-side variables lagged by one year. Country-clustered standard errors in parentheses. Estimator: OLS (ordinary least squares).}"))

## Polity ---

politylm_1_tab <- felm(share_female ~ lag_polity2  | 0 | 0 | country_isocode ,data=df_cross)

# Model 2 - Democracy (year fixed)

politylm_2_tab <- felm(share_female ~ lag_polity2 | year | 0 | country_isocode ,data=df_cross)

# Model 3 - Democracy (year and country)

politylm_3_tab <- felm(share_female ~ lag_polity2 | year + country_isocode | 0 | country_isocode,data=df_cross)

# Model 4 - Democracy (stock) - 90

politylm_4_tab <- felm(share_female ~ lag_polity2_stock90 | year + country_isocode | 0 | country_isocode,data=df_cross)

# Model 5 - Democracy (stock) - 95

politylm_5_tab <- felm(share_female ~ lag_polity2_stock95 | year + country_isocode | 0 | country_isocode,data=df_cross)

## Print

gof.names <- c("Observations", "R-squared", "Years","Countries")

texreg(list(politylm_1_tab,politylm_2_tab,politylm_3_tab,politylm_4_tab,politylm_5_tab),stars = c(0.05),
       file="../3_output/appendixc_tab3.tex",
       custom.model.names=c("(1)","(2)","(3)","(4)","(5)"),
       custom.coef.names=c("Intercept","Polyarchy","Stock of Polyarchy (90%)","Stock of Polyarchy (95%)"),
       include.adjrs = FALSE,
       include.rmse = FALSE,
       include.aic = FALSE,
       include.bic = FALSE,
       include.dic = FALSE,
       include.deviance = FALSE,
       include.loglik = FALSE,
       include.variance = FALSE,
       use.packages = FALSE,
       include.proj.stats = FALSE,
       label = "appendixc_tab3",
       caption ="Table for \\Cref{fig:appendixdifferentdem} - Polity IV",
       caption.above=TRUE,
       #  single.row = TRUE,
       scalebox = 0.8,
       custom.gof.names = gof.names,
       custom.header = list("Dependent variable: Share of female ministers" = 1:5),
       float.pos = "!htbp",
       custom.gof.rows = list("Within country:"= c("No","No","Yes","Yes","Yes"),
                              "Within year:"= c("No","Yes","Yes","Yes","Yes")),
       custom.note = ("\\parbox{0.75\\textwidth}{\\vspace{2pt}. \\\\
                      \\normalsize %stars. All right-side variables lagged by one year. Country-clustered standard errors in parentheses. Estimator: OLS (ordinary least squares).}"))


#####
# Appendix D: Stock of democracy
#####

calc_stock_continuous <- function(x, alpha) {
  
  out <- x[1]
  
  for (i in 2:length(x)) {
    
    out[i] <- out[i-1] * alpha + x[i]
    
  }
  
  out/max(out, na.rm = TRUE)
  
}

n_time <- 300

set.seed(123)

simulation <- tibble(
  
  time = seq(1, n_time, 1),
  
  bind_rows(
    
    as_tibble(runif(n_time/3, 0.1, 0.2)), # Exclusive hegemony
    as_tibble(runif(n_time/3, 0.8, 0.9)), # Polyarchy
    as_tibble(runif(n_time/3, 0.4, 0.5)), # Inclusive hegemony
    .id = NULL
    
    
  )
  
) %>% 
  rename(poly = 2)

simulation %>% 
  mutate(stock_poly_95 = calc_stock_continuous(poly, 0.95)) %>% 
  pivot_longer(-time, names_to = "variable") %>%
  mutate(variable = case_when(variable == "poly" ~ "Polyarchy score",
                              TRUE ~ "Stock of Polyarchy")) %>% 
  ggplot(aes(x = time, y = value, colour = variable)) +
  geom_line(size = 1) +
  theme_custom() +
  scale_colour_manual(values = c("#273046","#CB2314")) +
  labs(x = "Time", y = "Proportion")

ggsave('../3_output/appendixd_1.pdf',
       width = 5,
       height = 5,
       dpi = 1200)

alpha <- c(seq(0.15, 0.95, by = 0.2), 0.99)

# This creates a list of six identical data frames.

simulation_list <- simulation %>% 
  list(.,.,.,.,.,.)

for (i in 1:length(alpha)) {
  
  simulation_list[[i]] <- simulation %>% 
    mutate(stock_poly = calc_stock_continuous(poly, alpha[i]))
  
}

simulation_list %>% 
  set_names(alpha) %>% 
  bind_rows(.id = "rate") %>% 
  pivot_longer(-c(rate, time), names_to = "variable") %>%
  mutate(variable = case_when(variable == "poly" ~ "Polyarchy score",
                              TRUE ~ "Stock of Polyarchy")) %>% 
  ggplot(aes(x = time, y = value, colour = variable)) +
  geom_line(size = 1) +
  theme_minimal() +
  theme_custom() +
  facet_wrap(. ~ rate, labeller = label_bquote(alpha ~ "=" ~ .(rate)), ncol = 2) +
  scale_colour_manual(values = c("#273046","#CB2314")) +
  labs(x = "Time", y = "Proportion")

ggsave('../3_output/appendixd_2.pdf',
       width = 6,
       height = 6,
       dpi = 1200)

#####
# Appendix F: Descriptive statistics ---
#####

df_desc <- df_cross %>% select(share_female,importance_weight,share_female_high,poly, # Dependent variable
                               democracy_stock_poly_90,democracy_stock_poly_95,democracy_stock_poly_99, # Independent variable
                               polity2,democracy_bmr,democracy_dd,
                               stock_v2x_freexp_altinf,stock_v2x_suffr,stock_v2xel_frefair,stock_v2x_elecoff,stock_v2x_frassoc_thick, # Other measures of democracy
                               gdp_cap_pwt_ln,wb_oilrev,growth_pwt,wdi_popurb,pop_pwt_ln,
                               e_pelifeex,wb_infantmortality,wb_primaryschoolenrolment,v2x_gender,v2lgfemleg,ciri_wopol,ciri_wecon,female_leader,
                               v2xcl_rol,v2xcl_prpty,v2x_rule,v2x_jucon,v2xlg_legcon,v2x_corr,v2clstown,v2xcs_ccsi,v2xps_party,
                               lp_lat_abst,lp_muslim80,lp_protmg80,al_ethnic2000,wdi_area,sai_statehiste0
                               # Control variables
)

stargazer(df_desc, title="Descriptive statistics", digits=2,
          covariate.labels = c("Share of female ministers","Share of female ministers (Weighted)","Share of female ministers (High prestige)",
                               "Polyarchy","Stock of Polyarchy (90 percent)","Stock of Polyarchy (95 percent)","Stock of Polyarchy (99 percent)",
                               "Polity IV","BMR Index","DD index",
                               "Stock of Freedom of Expression","Stock of Suffrage","Stock of Clean Elections","Stock of Elected Officials","Stock of Associational Autonomy",
                               "Log of GDP per capita","Oil rents (percent of GDP)","GDP Growth","Urbanization","Log of Population",
                               "Life expectancy","Infant mortality","Primary school enrolment",
                               "Women political empowerment index","Lower chamber female legislators",
                               "Women's Political Rights","Women's Economic Rights","Female leader",
                               "Individual liberties","Property Rights","Rule of law","Judicial Constraints","Legislative Constraints","Political Corruption",
                               "State ownership of economy","Core civil society","Party institutionalization",
                               "Latitude (ln)","Muslim","Protestant","Ethnic fractionalization","Land area","State history"
          ),
          type = "latex", out="../3_output/appendixf.tex",
          omit.summary.stat = c("p25","p75"),font.size="scriptsize"
)

#####
# Appendix G: Table for Figure 3 ---
#####

## Share female ---

# Model 1 - Democracy

polylm_1 <- felm(share_female ~ lag_poly  | 0 | 0 | country_isocode, data=df_cross)

# Model 2 - Democracy (year fixed)

polylm_2 <- felm(share_female ~ lag_poly | year | 0 | country_isocode, data=df_cross)

# Model 3 - Democracy (year and country)

polylm_3 <- felm(share_female ~ lag_poly | year + country_isocode | 0 | country_isocode, data=df_cross)

# Model 4 - Democracy (stock) - 90

polylm_4 <- felm(share_female ~ lag_democracy_stock_poly_90 | year + country_isocode | 0 | country_isocode, data=df_cross)

# Model 5 - Democracy (stock) - 95

polylm_5 <- felm(share_female ~ lag_democracy_stock_poly_95 | year + country_isocode | 0 | country_isocode,data=df_cross)

## Print

gof.names <- c("Observations", "R-squared", "Years","Countries")

texreg(list(polylm_1,polylm_2,polylm_3,polylm_4,polylm_5),stars = c(0.05),
       file="../3_output/appendixg1.tex",
       custom.model.names=c("(1)","(2)","(3)","(4)","(5)"),
       custom.coef.names=c("Intercept","Polyarchy","Stock of Polyarchy (90%)","Stock of Polyarchy (95%)"),
       include.adjrs = FALSE,
       include.rmse = FALSE,
       include.aic = FALSE,
       include.bic = FALSE,
       include.dic = FALSE,
       include.deviance = FALSE,
       include.loglik = FALSE,
       include.variance = FALSE,
       include.proj.stats = FALSE,
       label = "appendixf",
       caption ="Table for Figure 3",
       caption.above=TRUE,
       #  single.row = TRUE,
       custom.gof.names = gof.names,
       custom.header = list("Dependent variable: Share of female ministers" = 1:5),
       float.pos = "!htbp",
       custom.gof.rows = list("Within country:"= c("No","No","Yes","Yes","Yes"),
                              "Within year:"= c("No","Yes","Yes","Yes","Yes")),
       custom.note = ("\\parbox{.75\\linewidth}{\\vspace{2pt}. \\\\
                      \\normalsize %stars. All right-side variables lagged by one year. Country-clustered standard errors in parentheses. Estimator: OLS (ordinary least squares).}"))


## Share female - weighted ---

# Model 1 - Democracy

polylm_1_w <- felm(importance_weight ~ lag_poly  | 0 | 0 | country_isocode ,data=df_cross)

# Model 2 - Democracy (year fixed)

polylm_2_w <- felm(importance_weight ~ lag_poly | year | 0 | country_isocode ,data=df_cross)

# Model 3 - Democracy (year and country)

polylm_3_w <- felm(importance_weight ~ lag_poly | year + country_isocode | 0 | country_isocode,data=df_cross)

# Model 4 - Democracy (stock) - 90

polylm_4_w <- felm(importance_weight ~ lag_democracy_stock_poly_90 | year + country_isocode | 0 | country_isocode,data=df_cross)

# Model 5 - Democracy (stock) - 95

polylm_5_w <- felm(importance_weight ~ lag_democracy_stock_poly_95 | year + country_isocode | 0 | country_isocode,data=df_cross)

## Print

gof.names <- c("Observations", "R-squared", "Years","Countries")

texreg(list(polylm_1_w,polylm_2_w,polylm_3_w,polylm_4_w,polylm_5_w),stars = c(0.05),
       file="../3_output/appendixg2.tex",
       custom.model.names=c("(1)","(2)","(3)","(4)","(5)"),
       custom.coef.names=c("Intercept","Polyarchy","Stock of Polyarchy (90%)","Stock of Polyarchy (95%)"),
       include.adjrs = FALSE,
       include.rmse = FALSE,
       include.aic = FALSE,
       include.bic = FALSE,
       include.dic = FALSE,
       include.deviance = FALSE,
       include.loglik = FALSE,
       include.variance = FALSE,
       include.proj.stats = FALSE,
       label = "appendixf",
       caption ="Table for Figure 3",
       caption.above=TRUE,
       #  single.row = TRUE,
       custom.gof.names = gof.names,
       custom.header = list("Dependent variable: Weighted share of female ministers" = 1:5),
       custom.gof.rows = list("Within country:"= c("No","No","Yes","Yes","Yes"),
                              "Within year:"= c("No","Yes","Yes","Yes","Yes")),
       custom.note = ("\\parbox{.9\\linewidth}{\\vspace{2pt}. \\\\
                      \\normalsize %stars. All right-side variables lagged by one year. Country-clustered standard errors in parentheses. Estimator: OLS (ordinary least squares).}"))


### Share female - high ---

# Model 1 - Democracy

polylm_1_h <- felm(share_female_high ~ lag_poly | 0 | 0 | country_isocode ,data=df_cross)

# Model 2 - Democracy (year fixed)

polylm_2_h <- felm(share_female_high ~ lag_poly | year | 0 | country_isocode ,data=df_cross)

# Model 3 - Democracy (year and country)

polylm_3_h <- felm(share_female_high ~ lag_poly | year + country_isocode | 0 | country_isocode,data=df_cross)

# Model 4 - Democracy (stock) - 90

polylm_4_h <- felm(share_female_high ~ lag_democracy_stock_poly_90 | year + country_isocode | 0 | country_isocode,data=df_cross)

# Model 5 - Democracy (stock) - 95

polylm_5_h <- felm(share_female_high ~ lag_democracy_stock_poly_95 | year + country_isocode | 0 | country_isocode,data=df_cross)

## Print

gof.names <- c("Observations", "R-squared", "Years","Countries")

texreg(list(polylm_1_h,polylm_2_h,polylm_3_h,polylm_4_h,polylm_5_h),stars = c(0.05),
       file="../3_output/appendixg3.tex",
       custom.model.names=c("(1)","(2)","(3)","(4)","(5)"),
       custom.coef.names=c("Intercept","Polyarchy","Stock of Polyarchy (90%)","Stock of Polyarchy (95%)"),
       include.adjrs = FALSE,
       include.rmse = FALSE,
       include.aic = FALSE,
       include.bic = FALSE,
       include.dic = FALSE,
       include.deviance = FALSE,
       include.loglik = FALSE,
       include.variance = FALSE,
       include.proj.stats = FALSE,
       label = "appendixe",
       caption ="Table for Figure 3",
       caption.above=TRUE,
       #  single.row = TRUE,
       # column.spacing = ,
       custom.gof.names = gof.names,
       custom.header = list("Dependent variable: Share of female ministers in high prestige portfolios" = 1:5),
       custom.gof.rows = list("Within country:"= c("No","No","Yes","Yes","Yes"),
                              "Within year:"= c("No","Yes","Yes","Yes","Yes")),
       custom.note = ("\\parbox{.85\\linewidth}{\\vspace{2pt}. \\\\
                      \\normalsize %stars. All right-side variables lagged by one year. Country-clustered standard errors in parentheses. Estimator: OLS (ordinary least squares).}"))

#####
# Appendix H: Table 1 with weighted share of female ministers ---
#####

# Model 1 - Basic

m1_w <- felm(importance_weight ~ lag_democracy_stock_poly_95 | year + country_isocode | 0 | country_isocode,data=df_cross)

# Model 2 - Year trend

m2_w <- felm(importance_weight ~ lag_democracy_stock_poly_95 + year | country_isocode | 0 | country_isocode,data=df_cross)

# Model 3 - Growth and economic indicators

m3_w <- felm(importance_weight ~ lag_democracy_stock_poly_95 + lag_gdp_cap_pwt_ln + lag_wb_oilrev + lag_growth_pwt + lag_wdi_popurb + lag_pop_pwt_ln | year + country_isocode | 0 | country_isocode,data=df_cross)

# Model 4 - Human development (controls)

m4_w <- felm(importance_weight ~ lag_democracy_stock_poly_95 + lag_e_pelifeex + lag_wb_infantmortality + lag_wb_primaryschoolenrolment | year + country_isocode | 0 | country_isocode,data=df_cross)

# Model 5 - Women's representation

m5_w <- felm(importance_weight ~ lag_democracy_stock_poly_95 + lag_v2x_gender + lag_v2lgfemleg + lag_ciri_wopol + lag_ciri_wecon + lag_female_leader | year + country_isocode | 0 | country_isocode,data=df_cross)

# Model 6 - Institutional quality

m6_w <- felm(importance_weight ~ lag_democracy_stock_poly_95 + lag_v2xcl_rol + lag_v2xcl_prpty + lag_v2x_rule + lag_v2x_jucon +
               lag_v2xlg_legcon + lag_v2x_corr + lag_v2clstown + lag_v2xcs_ccsi + lag_v2xps_party | year + country_isocode | 0 | country_isocode,data=df_cross)

# Model 7 - Lagged dependent

m7_w <- felm(importance_weight ~ lag_democracy_stock_poly_95 + lag_importance_weight | country_isocode + year | 0 | country_isocode,data=df_cross)

# Model 8 - Static (random effects)

m8_w <- lmer(importance_weight ~ year + lag_democracy_stock_poly_95 + lp_lat_abst + lp_muslim80 + lp_protmg80 + al_ethnic2000 + wdi_area + sai_statehiste0 + continent + (1 + year | country_isocode),data=df_cross)

#####
### Print table ---
#####

gof.names <- c("Observations", "R-squared", "Years","Countries")

texreg(list(m1_w,m2_w,m3_w,m4_w,m5_w,m6_w,m7_w,m8_w),stars = c(0.05),
       file="../3_output/appendixh.tex",
       custom.model.names=c("(1)","(2)","(3)","(4)","(5)","(6)","(7)","(8)"),
       custom.coef.names=c("Stock of Polyarchy (95%)","Trend","Log of GDP per capita","Oil rents (% of GDP)","GDP growth","Urbanization","Log of Population",
                           "Life expectancy","Infant mortality","Primary school enrolment","Women political empowerment index","Lower chamber female legislators","Women's Political Rights",
                           "Women's Economic Rights","Female leader","Individual liberties","Property Rights","Rule of law","Judicial Constraints","Legislative Constraints",
                           "Political Corruption","State ownership of economy","Core civil society","Party institutionalization","Lagged dependent","Constant","Latitude (ln)","Muslim","Protestant","Ethnic fractionalization",
                           "Land area","State history","Americas (ref: Africa)","Asia (ref: Africa)","Europe (ref: Africa)","Oceania (ref: Africa)"),
       scalebox = 0.65,
       single.row = TRUE,
       include.adjrs = FALSE,
       include.rmse = FALSE,
       include.aic = FALSE,
       include.bic = FALSE,
       include.dic = FALSE,
       include.deviance = FALSE,
       include.loglik = FALSE,
       include.variance = FALSE,
       use.packages = FALSE,
       include.proj.stats = FALSE,
       float.pos = "!htbp",
       label = "appendixg",
       caption ="Specification tests",
       caption.above=TRUE,
       custom.gof.names = gof.names,
       custom.header = list("Dependent variable: Weighted share of female ministers" = 1:8),
       custom.gof.rows = list("Estimation method:"= c("FE","FE (only country)","FE","FE","FE","FE","FE","RE")),
       custom.note = ("\\parbox{1.8\\linewidth}{\\vspace{2pt}. \\\\
                      \\normalsize %stars. All right-side variables lagged by one year. Country-clustered standard errors in parentheses. Estimator: OLS (ordinary least squares). FE = Fixed effects (country and year), RE = Random effects.}"),
)

#####
# Appendix I: Table 1 with share of female ministers in high prestige positions ---
#####

# Model 1 - Basic

m1_h <- felm(share_female_high ~ lag_democracy_stock_poly_95 | year + country_isocode | 0 | country_isocode,data=df_cross)

# Model 2 - Year trend

m2_h <- felm(share_female_high ~ lag_democracy_stock_poly_95 + year | country_isocode | 0 | country_isocode,data=df_cross)

# Model 3 - Growth and economic indicators

m3_h <- felm(share_female_high ~ lag_democracy_stock_poly_95 + lag_gdp_cap_pwt_ln + lag_wb_oilrev + lag_growth_pwt + lag_wdi_popurb + lag_pop_pwt_ln | year + country_isocode | 0 | country_isocode,data=df_cross)

# Model 4 - Human development (controls)

m4_h <- felm(share_female_high ~ lag_democracy_stock_poly_95 + lag_e_pelifeex + lag_wb_infantmortality + lag_wb_primaryschoolenrolment | year + country_isocode | 0 | country_isocode,data=df_cross)

# Model 5 - Women's representation

m5_h <- felm(share_female_high ~ lag_democracy_stock_poly_95 + lag_v2x_gender + lag_v2lgfemleg + lag_ciri_wopol + lag_ciri_wecon + lag_female_leader | year + country_isocode | 0 | country_isocode,data=df_cross)

# Model 6 - Institutional quality

m6_h <- felm(share_female_high ~ lag_democracy_stock_poly_95 + lag_v2xcl_rol + lag_v2xcl_prpty + lag_v2x_rule + lag_v2x_jucon +
               lag_v2xlg_legcon + lag_v2x_corr + lag_v2clstown + lag_v2xcs_ccsi + lag_v2xps_party | year + country_isocode | 0 | country_isocode,data=df_cross)

# Model 7 - Lagged dependent

m7_h <- felm(share_female_high ~ lag_democracy_stock_poly_95 + lag_share_female_high | country_isocode + year | 0 | country_isocode,data=df_cross)

# Model 8 - Static (random effects)

m8_h <- lmer(share_female_high ~ year + lag_democracy_stock_poly_95 + lp_lat_abst + lp_muslim80 + lp_protmg80 + al_ethnic2000 + wdi_area + sai_statehiste0 + continent + (1 + year | country_isocode),data=df_cross)

#####
### Print table ---
#####

gof.names <- c("Observations", "R-squared", "Years","Countries")

texreg(list(m1_h,m2_h,m3_h,m4_h,m5_h,m6_h,m7_h,m8_h),stars = c(0.05),
       file="../3_output/appendixi.tex",
       custom.model.names=c("(1)","(2)","(3)","(4)","(5)","(6)","(7)","(8)"),
       custom.coef.names=c("Stock of Polyarchy (95%)","Trend","Log of GDP per capita","Oil rents (% of GDP)","GDP growth","Urbanization","Log of Population",
                           "Life expectancy","Infant mortality","Primary school enrolment","Women political empowerment index","Lower chamber female legislators","Women's Political Rights",
                           "Women's Economic Rights","Female leader","Individual liberties","Property Rights","Rule of law","Judicial Constraints","Legislative Constraints",
                           "Political Corruption","State ownership of economy","Core civil society","Party institutionalization","Lagged dependent","Constant","Latitude (ln)","Muslim","Protestant","Ethnic fractionalization",
                           "Land area","State history","Americas (ref: Africa)","Asia (ref: Africa)","Europe (ref: Africa)","Oceania (ref: Africa)"),
       scalebox = 0.65,
       single.row = TRUE,
       include.adjrs = FALSE,
       include.rmse = FALSE,
       include.aic = FALSE,
       include.bic = FALSE,
       include.dic = FALSE,
       include.deviance = FALSE,
       include.loglik = FALSE,
       include.variance = FALSE,
       use.packages = FALSE,
       include.proj.stats = FALSE,
       float.pos = "!htbp",
       label = "appendixg",
       caption ="Specification tests",
       caption.above=TRUE,
       custom.gof.names = gof.names,
       custom.header = list("Dependent variable: Share of female ministers in high prestige portfolios" = 1:8),
       custom.gof.rows = list("Estimation method:"= c("FE","FE (only country)","FE","FE","FE","FE","FE","RE")),
       custom.note = ("\\parbox{1.8\\linewidth}{\\vspace{2pt}. \\\\
                      \\normalsize %stars. All right-side variables lagged by one year. Country clustered standard errors in parentheses. Estimator: OLS (ordinary least squares). FE = Fixed effects (country and year), RE = Random effects.}"),
)

#####
# Appendix J: Different depreciation rates ---
#####

models <- df_cross %>% 
  dplyr::select(starts_with("lag_democracy_stock_poly")) %>%  # exclude outcome, leave only predictors 
  map(~felm(df_cross$share_female ~ .x | year + country_isocode | 0 | country_isocode,data=df_cross)) %>%
  map(summary)

df_names <- models %>% 
  map_dbl(~.$coefficients[1])

df_coefficient <- models %>% 
  map_dbl(~.$coefficients[1])

df_stderror <- models %>% 
  map_dbl(~.$coefficients[1,2])

df_models <- data.frame(df_coefficient,df_stderror,depreciation = seq(0,99,1)) %>% dplyr::rename(coefficient = df_coefficient,stderror = df_stderror) %>% 
  filter(depreciation > -1)

appendixi <- ggplot(df_models,aes(x=depreciation, y=coefficient)) + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_point() + 
  geom_linerange(aes(x = depreciation, 
                     ymin = coefficient+stderror*1.645,
                     ymax = coefficient-stderror*1.645),
                 lwd = 1) +
  geom_linerange(aes(x = depreciation, 
                     ymin = coefficient+stderror*1.96,
                     ymax = coefficient-stderror*1.96),
                 lwd = 1/2) + 
  theme_custom() +
  ylab("Coefficient Estimate\nfor stock of democracy") +
  xlab("Depreciation rate")

ggsave(
  "../3_output/appendixj.pdf",
  appendixi,
  width = 6,
  height = 3,
  dpi = 1200
)

#####
# Appendix K: Table 2 with imputed data ---
#####

# Create a list of subsetted data frames

model_data <- list(
  
  m1 = df_cross %>% select(share_female, 
                           lag_democracy_stock_poly_95,
                           year, 
                           country_isocode),
  m2 = df_cross %>% select(share_female, 
                           lag_democracy_stock_poly_95,
                           year, 
                           country_isocode),
  m3 = df_cross %>% select(share_female, 
                           lag_democracy_stock_poly_95,
                           lag_gdp_cap_pwt_ln, 
                           lag_wb_oilrev, 
                           lag_growth_pwt, 
                           lag_wdi_popurb, 
                           lag_pop_pwt_ln,
                           year, 
                           country_isocode),
  m4 = df_cross %>% select(share_female, 
                           lag_democracy_stock_poly_95,
                           lag_e_pelifeex, 
                           lag_wb_infantmortality,
                           lag_wb_primaryschoolenrolment,
                           year, 
                           country_isocode),
  m5 = df_cross %>% select(share_female, 
                           lag_democracy_stock_poly_95,
                           lag_v2x_gender,
                           lag_v2lgfemleg, 
                           lag_ciri_wopol, 
                           lag_ciri_wecon, 
                           lag_female_leader,
                           year, 
                           country_isocode),
  m6 = df_cross %>% select(share_female, 
                           lag_democracy_stock_poly_95,
                           lag_v2xcl_rol, 
                           lag_v2xcl_prpty, 
                           lag_v2x_rule, 
                           lag_v2x_jucon,
                           lag_v2xlg_legcon, 
                           lag_v2x_corr, 
                           lag_v2clstown, 
                           lag_v2xcs_ccsi, 
                           lag_v2xps_party,
                           year, 
                           country_isocode)
)

# Create a list of model formulae

model_formulae <- list(
  
  m1 = as.formula(share_female ~ lag_democracy_stock_poly_95 | year + country_isocode | 0 | country_isocode),
  m2 = as.formula(share_female ~ lag_democracy_stock_poly_95 + year | country_isocode | 0 | country_isocode),
  m3 = as.formula(share_female ~ lag_democracy_stock_poly_95 + lag_gdp_cap_pwt_ln + lag_wb_oilrev + lag_growth_pwt + lag_wdi_popurb + lag_pop_pwt_ln | year + country_isocode | 0 | country_isocode),
  m4 = as.formula(share_female ~ lag_democracy_stock_poly_95 + lag_e_pelifeex + lag_wb_infantmortality + lag_wb_primaryschoolenrolment | year + country_isocode | 0 | country_isocode),
  m5 = as.formula(share_female ~ lag_democracy_stock_poly_95 + lag_v2x_gender + lag_v2lgfemleg + lag_ciri_wopol + lag_ciri_wecon + lag_female_leader | year + country_isocode | 0 | country_isocode),
  m6 = as.formula(share_female ~ lag_democracy_stock_poly_95 + lag_v2xcl_rol + lag_v2xcl_prpty + lag_v2x_rule + lag_v2x_jucon + lag_v2xlg_legcon + lag_v2x_corr + lag_v2clstown + lag_v2xcs_ccsi + lag_v2xps_party | year + country_isocode | 0 | country_isocode)
  
)

# Identify number of imputations

n_imps <- map(model_data,
              . %>% 
                select(-c(country_isocode, year)) %>% 
                is.na() %>% 
                as_tibble() %>% 
                summarise_all(function(var) round(mean(var), 5)*100) %>% 
                pivot_longer(everything()) %>% 
                summarise(value = mean(value)) %>% 
                pull(value) %>% 
                round(0)) %>%
  map(., function(m) if_else(m < 5, 5, m))

# Run imputations

imp_amelia <- list()
all_imputations <- list()
models_imputations <- list()

# Set seed for reproducability
set.seed(123)

for (i in 1:length(model_data)){
  
  # Imputation model
  imp_amelia[[i]] <- amelia(as.data.frame(model_data[[i]]), m = n_imps[[i]], 
                            ts = "year", cs = "country_isocode",
                            polytime = 3, 
                            empri = 0.01*nrow(model_data[[i]]))
  
  # Nested tibbles
  all_imputations[[i]] <- imp_amelia[[i]] %>%
    .$imputations %>% 
    map_dfr(as_tibble, .id = "m") %>% 
    group_by(m) %>% 
    nest()
  
  # Regression models
  models_imputations[[i]] <- all_imputations[[i]] %>% 
    mutate(model = map(.x = data, ~felm(model_formulae[[i]], data = .x)),
           tidied = map(.x = model, ~tidy(.x)),
           glance = map(.x = model, ~glance(.x)))
  
}

# Create coefficient/standard error tibbles

params <- models_imputations %>% 
  map(. %>% 
        unnest(tidied) %>% 
        clean_names() %>% 
        select(m, term, estimate, std_error) %>% 
        pivot_longer(cols = c(estimate, std_error), names_to = "key") %>%
        pivot_wider(names_from = term, values_from = value) %>% 
        ungroup()
  )

# Isolate coefficients/standard errors

just_coefs <- params %>% 
  map(. %>% 
        filter(key == "estimate") %>% 
        select(-c(1,2))
  )

just_ses <- params %>% 
  map(. %>% 
        filter(key == "std_error") %>% 
        select(-c(1,2))
  )

# Combine coefficients/standard errors as per Rubin's rules

coefs_melded <- map2(.x = just_coefs, .y = just_ses, ~mi.meld(.x, .y))

# Find degrees of freedom and number of observations for each model

model_degree_freedom <- models_imputations %>% 
  map(. %>% 
        unnest(glance) %>% 
        filter(m == "imp1") %>% 
        pull(df.residual)
  )

model_observations <- models_imputations %>% 
  map(. %>% 
        unnest(glance) %>% 
        filter(m == "imp1") %>% 
        pull(nobs)
  )

# Combine and calculate quantities of interest

imputed_summary <- list()

for (i in 1:length(coefs_melded)) {
  
  imputed_summary[[i]] <- as.data.frame(cbind(t(coefs_melded[[i]]$q.mi),
                                              t(coefs_melded[[i]]$se.mi))) %>% 
    rename(estimate = 1, std_error = 2) %>% 
    mutate(term = rownames(.)) %>% 
    select(term, everything()) %>% 
    mutate(statistic = estimate / std_error,
           conf_low_95 = estimate + std_error * qt(0.025, model_degree_freedom[[i]]),
           conf_high_95 = estimate + std_error * qt(0.975, model_degree_freedom[[i]]),
           conf_low_90 = estimate + std_error * qt(0.05, model_degree_freedom[[i]]),
           conf_high_90 = estimate + std_error * qt(0.95, model_degree_freedom[[i]]),
           p_value = 2 * pt(abs(statistic), 
                            model_degree_freedom[[i]], 
                            lower.tail = FALSE)) %>% 
    as_tibble()
  
}

df_imputed_summary <- imputed_summary %>% 
  bind_rows(.id = "model") %>% 
  filter(term == "lag_democracy_stock_poly_95") %>% 
  bind_cols(n_imps %>% bind_cols(.id = NULL) %>% gather(key, n_imps) %>% .[,2]) %>% 
  mutate(model = glue::glue('{model} ({n_imps})'),
         model = fct_rev(as.factor(model)))

# Plot 

df_imputed_summary %>% 
  ggplot(aes(y = model, x = estimate)) +
  geom_point() +
  geom_linerange(aes(xmin = conf_low_95, xmax = conf_high_95), lwd = 1/2) +
  geom_linerange(aes(xmin = conf_low_90, xmax = conf_high_90), lwd = 1) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(y = "Model (number of imputations)", x = 'Coefficient estimate for stock of democracy (95%)') +
  theme_custom()

ggsave('../3_output/appendixk.pdf',
       width = 5,
       height = 5,
       dpi = 1200)

# Values for Table J1

df_imputed_summary %>% 
  mutate(estimate = round(estimate, 2),
         std_error = round(std_error, 2)) %>% 
  select(estimate, std_error, p_value, n_imps) 

#####
# Appendix L: Table for Figure 4 ---
#####

# Model 1 - Stock of clean elections
polylm_1_freefair <- felm(share_female ~ lag_stock_v2xel_frefair | year + country_isocode | 0 | country_isocode,data=df_cross)

# Model 2 - Stock of freedom of expression
polylm_2_freedomofexp <- felm(share_female ~ lag_stock_v2x_freexp_altinf | year + country_isocode | 0 | country_isocode,data=df_cross)

# Model 3 - Stock of assoc autonomy
polylm_3_assoc <- felm(share_female ~ lag_stock_v2x_frassoc_thick | year + country_isocode | 0 | country_isocode,data=df_cross)

# Model 4 - Stock of elected officials
polylm_4_elecoff <- felm(share_female ~ lag_stock_v2x_elecoff | year + country_isocode | 0 | country_isocode,data=df_cross)

# Model 5 - Stock of suffrage
polylm_5_suffrage <- felm(share_female ~ lag_stock_v2x_suffr | year + country_isocode | 0 | country_isocode,data=df_cross)

## Print

# Same as gof.names object defined before
gof.names <- c("Observations", "R-squared", "Years","Countries")

texreg(list(polylm_1_freefair, polylm_2_freedomofexp,  polylm_3_assoc,
            polylm_4_elecoff, polylm_5_suffrage), 
       stars = c(0.05),
       file="../3_output/appendixl.tex",
       custom.model.names=c("(1)","(2)","(3)","(4)","(5)"),
       custom.coef.names=c("Stock of Clean Elections",
                           "Stock of Freedom of Expression",
                           "Stock of Associational Autonomy",
                           "Stock of Elected Officials",
                           "Stock of Suffrage"),
       include.adjrs = FALSE,
       include.rmse = FALSE,
       include.aic = FALSE,
       include.bic = FALSE,
       include.dic = FALSE,
       include.deviance = FALSE,
       include.loglik = FALSE,
       include.variance = FALSE,
       include.proj.stats = FALSE,
       float.pos = "!htbp",
       label = "appendixi",
       caption ="Table for Figure 4 - Components of polyarchy",
       caption.above=TRUE,
       #  single.row = TRUE,
       custom.gof.names = gof.names,
       custom.note = ("\\parbox{0.8\\linewidth}{\\vspace{2pt}.
                      \\normalsize %stars. Dependent variable: Share of female ministers (WhoGov). All right-side variables lagged by one year. Country clustered standard errors in parentheses. Estimator: OLS (ordinary least squares) with country and year fixed effects.}"))

#####
# Appendix M: Table for Figure 5 ---
#####

df_transformation <- df_cross %>% group_by(country_isocode) %>% mutate(
  transition_dem = if_else(year-lag(year) == 1 & bmr_new == 1 & lag(bmr_new == 0),1,0),
  share_yeartransition = case_when(
    lead(transition_dem,1) == 1 ~ lead(share_female,1),
    lead(transition_dem,2) == 1 ~ lead(share_female,2),
    lead(transition_dem,3) == 1 ~ lead(share_female,3),
    lag(transition_dem,0) == 1 ~ lag(share_female,0),
    lag(transition_dem,1) == 1 ~ lag(share_female,1),
    lag(transition_dem,2) == 1 ~ lag(share_female,2),
    lag(transition_dem,3) == 1 ~ lag(share_female,3),
    lag(transition_dem,4) == 1 ~ lag(share_female,4),
    lag(transition_dem,5) == 1 ~ lag(share_female,5),
    lag(transition_dem,6) == 1 ~ lag(share_female,6),
    lag(transition_dem,7) == 1 ~ lag(share_female,7),
    lag(transition_dem,8) == 1 ~ lag(share_female,8),
    TRUE ~ 99),
  counter_dem = case_when(lag(transition_dem,0) == 1 ~ 0,
                          lead(transition_dem,1) == 1 ~ -1,
                          lag(transition_dem,1) == 1 ~ 1,
                          lead(transition_dem,2) == 1 ~ -2,
                          lag(transition_dem,2) == 1 ~ 2,
                          lead(transition_dem,3) == 1 ~ -3,
                          lag(transition_dem,3) == 1 ~ 3,
                          lag(transition_dem,4) == 1 ~ 4,
                          lag(transition_dem,5) == 1 ~ 5,
                          lag(transition_dem,6) == 1 ~ 6,
                          lag(transition_dem,7) == 1 ~ 7,
                          lag(transition_dem,8) == 1 ~ 8,
                          TRUE ~ 99)) %>%
  filter(counter_dem != 99) %>%
  mutate(share_femaledemeaned = share_female-share_yeartransition)

df_transition_dem <- df_transformation %>% 
  group_by(counter_dem) %>%
  summarise(sharefemale_mean = mean(share_femaledemeaned, na.rm=TRUE),
            sharefemale_se=sd(share_femaledemeaned,na.rm=TRUE)/sqrt(n()),
            sharefemale_n = n()) %>% filter(counter_dem != 99)

table_df_transition_dem <- df_transition_dem %>% 
  mutate(counter_dem = round(counter_dem,0),
         sharefemale_mean = round(sharefemale_mean,2),
         sharefemale_se = round(sharefemale_se,2)) %>%
  rename(`Years to democratization` =  1,
         `Share female (relative to transition)` = 2,
         `Standard error` = 3,
         `N` = 4) %>%
  as_tabyl(.)

print(xtable(table_df_transition_dem, caption = 'Table for Figure 5 - Democratic transitions and the share of female ministers'),
      type = "latex", digits=c(0,0,2,2,0),
      caption.placement = 'top',
      tabular.environment = "longtable",floating=FALSE,include.rownames = FALSE,
      file="../3_output/appendixm.tex"
)

#####
# Appendix N: Table for Figure 6 ---
#####

# Model 1 - OECD
OECD <- c("AUS","AUT","BEL","CAN","CHL","CZE","DNK","EST","FIN","FRA","DEU","GRC","HUN","ISL","IRL","ITA","JPN","KOR","LUX","MEX","NLD","NZL","NOR",
          "POL","PRT","SVK","SVN","ESP","SWE","CHE","TUR","GBR","USA")

df_oecd <- df_cross %>% filter(country_isocode %in% OECD)
polylm_1_reg_oecd <- felm(share_female ~ lag_democracy_stock_poly_95 | year + country_isocode | 0 | country_isocode,data=df_oecd)

# Model 2 - Non-OECD
df_notoecd <- df_cross %>% filter(country_isocode %!in% OECD)
polylm_2_reg_notoecd <- felm(share_female ~ lag_democracy_stock_poly_95 | year + country_isocode | 0 | country_isocode,data=df_notoecd) 

# Model 3 - High
df_high <- df_cross %>% filter(gdp_cap_pwt_ln >= log(7000))
polylm_3_reg_high <- felm(share_female ~ lag_democracy_stock_poly_95 | year + country_isocode | 0 | country_isocode,data=df_high)

# Model 4 - Low
df_low <- df_cross %>% filter(gdp_cap_pwt_ln < log(7000))
polylm_4_reg_low <- felm(share_female ~ lag_democracy_stock_poly_95 | year + country_isocode | 0 | country_isocode,data=df_low)

# Model 5 - Before 1993
df_before <- df_cross %>% filter(year < 1993)
polylm_5_reg_before <- felm(share_female ~ lag_democracy_stock_poly_95 | year + country_isocode | 0 | country_isocode,data=df_before)

# Model 6 - After 1993
df_after <- df_cross %>% filter(year >= 1993)
polylm_6_reg_after <- felm(share_female ~ lag_democracy_stock_poly_95 | year + country_isocode | 0 | country_isocode,data=df_after)

## Print

# Same as gof.names object defined before
gof.names <- c("Observations", "R-squared", "Years","Countries")

texreg(list(polylm_1_reg_oecd, polylm_2_reg_notoecd,
            polylm_3_reg_high, polylm_4_reg_low,
            polylm_5_reg_before, polylm_6_reg_after), 
       stars = c(0.05),
       file="../3_output/appendixn.tex",
       custom.model.names=c("(1)","(2)","(3)","(4)","(5)","(6) \\\\ {\\tiny{& OECD & Non-OECD & GDP high & GDP low & Pre-1993 & Post-1993}}"),
       custom.coef.names=c("Stock of Polyarchy"),
       include.adjrs = FALSE,
       include.rmse = FALSE,
       include.aic = FALSE,
       include.bic = FALSE,
       include.dic = FALSE,
       include.deviance = FALSE,
       include.loglik = FALSE,
       include.variance = FALSE,
       use.packages = FALSE,
       include.proj.stats = FALSE,
       float.pos = "!htbp",
       label = "appendixk",
       caption ="Table for Figure 6 - Split sample tests",
       caption.above=TRUE,
       #  single.row = TRUE,
       custom.gof.names = gof.names,
       custom.note = ("\\parbox{1\\linewidth}{\\vspace{2pt}.\\\\
                      \\normalsize %stars. Dependent variable: Share of female ministers (WhoGov). The stock of polyarchy measure is lagged by one year. Column 3 is a split sample of countries with GDP per capita over USD 7000. Column 4 is a split sample of countries with GDP per capita under USD 7000. Country clustered standard errors in parentheses. Estimator: OLS (ordinary least squares) with country and year fixed effects.}"))
