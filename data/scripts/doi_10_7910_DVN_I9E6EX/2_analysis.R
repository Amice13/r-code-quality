##############################
#
# Replication file for the main analysis in:
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

pacman::p_load(tidyverse,lfe,lme4,texreg)

### Helper function

'%!in%' <- function(x,y)!('%in%'(x,y))

###############
## Load data ##
###############

df_cross <- read.csv("../1_data/df_consolidatingprogress_V1.csv")

################
### Figure 3 ###
################

## DV: Share female ---

# Model 1 - Democracy

polylm_1 <- felm(share_female ~ lag_poly | 0 | 0 | country_isocode ,data=df_cross) %>% summary()
df_polylm_1 <- tibble(model="Baseline (level)",estimate = polylm_1$coefficients[2],stderror = polylm_1$coefficients[2,2])

# Model 2 - Democracy (year fixed)

polylm_2 <- felm(share_female ~ lag_poly | year | 0 | country_isocode ,data=df_cross) %>% summary()
df_polylm_2 <- tibble(model="Year (level)",estimate = polylm_2$coefficients[1],stderror = polylm_2$coefficients[1,2])

# Model 3 - Democracy (year and country)

polylm_3 <- felm(share_female ~ lag_poly | year + country_isocode | 0 | country_isocode,data=df_cross) %>% summary()
df_polylm_3 <- tibble(model="Year and country (level)",estimate = polylm_3$coefficients[1],stderror = polylm_3$coefficients[1,2])

# Model 4 - Democracy (stock) - 90

polylm_4 <- felm(share_female ~ lag_democracy_stock_poly_90 | year + country_isocode | 0 | country_isocode,data=df_cross) %>% summary()
df_polylm_4 <- tibble(model="Year and country (Stock, 90 percent)",estimate = polylm_4$coefficients[1],stderror = polylm_4$coefficients[1,2])

# Model 5 - Democracy (stock) - 95

polylm_5 <- felm(share_female ~ lag_democracy_stock_poly_95 | year + country_isocode | 0 | country_isocode,data=df_cross) %>% summary()
df_polylm_5 <- tibble(model="Year and country (Stock, 95 percent)",estimate = polylm_5$coefficients[1],stderror = polylm_5$coefficients[1,2])

## Bind models together

poly_share <- rbind(df_polylm_1,df_polylm_2,df_polylm_3,df_polylm_4,df_polylm_5) %>% mutate("independent" = "Share female")

## DV: Share female - weighted ---

# Model 1 - Democracy

polylm_1_w <- felm(importance_weight ~ lag_poly | 0 | 0 | country_isocode ,data=df_cross) %>% summary()
df_polylm_1_w <- tibble(model="Baseline (level)",estimate = polylm_1_w$coefficients[2],stderror = polylm_1_w$coefficients[2,2])

# Model 2 - Democracy (year fixed)

polylm_2_w <- felm(importance_weight ~ lag_poly | year | 0 | country_isocode ,data=df_cross) %>% summary()
df_polylm_2_w <- tibble(model="Year (level)",estimate = polylm_2_w$coefficients[1],stderror = polylm_2_w$coefficients[1,2])

# Model 3 - Democracy (year and country)

polylm_3_w <- felm(importance_weight ~ lag_poly | year + country_isocode | 0 | country_isocode,data=df_cross) %>% summary()
df_polylm_3_w <- tibble(model="Year and country (level)",estimate = polylm_3_w$coefficients[1],stderror = polylm_3_w$coefficients[1,2])

# Model 4 - Democracy (stock) - 90

polylm_4_w <- felm(importance_weight ~ lag_democracy_stock_poly_90 | year + country_isocode | 0 | country_isocode,data=df_cross) %>% summary()
df_polylm_4_w <- tibble(model="Year and country (Stock, 90 percent)",estimate = polylm_4_w$coefficients[1],stderror = polylm_4_w$coefficients[1,2])

# Model 5 - Democracy (stock) - 95

polylm_5_w <- felm(importance_weight ~ lag_democracy_stock_poly_95 | year + country_isocode | 0 | country_isocode,data=df_cross) %>% summary()
df_polylm_5_w <- tibble(model="Year and country (Stock, 95 percent)",estimate = polylm_5_w$coefficients[1],stderror = polylm_5_w$coefficients[1,2])

## Bind models together

poly_shareweight <- rbind(df_polylm_1_w,df_polylm_2_w,df_polylm_3_w,df_polylm_4_w,df_polylm_5_w) %>% mutate("independent" = "Share female weighted")

## DV: Share female - high ---

# Model 1 - Democracy

polylm_1_h <- felm(share_female_high ~ lag_poly | 0 | 0 | country_isocode ,data=df_cross) %>% summary()
df_polylm_1_h <- tibble(model="Baseline (level)",estimate = polylm_1_h$coefficients[2],stderror = polylm_1_h$coefficients[2,2])

# Model 2 - Democracy (year fixed)

polylm_2_h <- felm(share_female_high ~ lag_poly | year | 0 | country_isocode ,data=df_cross) %>% summary()
df_polylm_2_h <- tibble(model="Year (level)",estimate = polylm_2_h$coefficients[1],stderror = polylm_2_h$coefficients[1,2])

# Model 3 - Democracy (year and country)

polylm_3_h <- felm(share_female_high ~ lag_poly | year + country_isocode | 0 | country_isocode,data=df_cross) %>% summary()
df_polylm_3_h <- tibble(model="Year and country (level)",estimate = polylm_3_h$coefficients[1],stderror = polylm_3_h$coefficients[1,2])

# Model 4 - Democracy (stock) - 90

polylm_4_h <- felm(share_female_high ~ lag_democracy_stock_poly_90 | year + country_isocode | 0 | country_isocode,data=df_cross) %>% summary()
df_polylm_4_h <- tibble(model="Year and country (Stock, 90 percent)",estimate = polylm_4_h$coefficients[1],stderror = polylm_4_h$coefficients[1,2])

# Model 5 - Democracy (stock) - 95

polylm_5_h <- felm(share_female_high ~ lag_democracy_stock_poly_95 | year + country_isocode | 0 | country_isocode,data=df_cross) %>% summary()
df_polylm_5_h <- tibble(model="Year and country (Stock, 95 percent)",estimate = polylm_5_h$coefficients[1],stderror = polylm_5_h$coefficients[1,2])

## Bind models together

poly_sharehigh <- rbind(df_polylm_1_h,df_polylm_2_h,df_polylm_3_h,df_polylm_4_h,df_polylm_5_h) %>% mutate("independent" = "Share female high prestige")

# Bind the four

df_coefplot_poly <- rbind(poly_share,poly_shareweight,poly_sharehigh) %>% 
  mutate(std.effect=estimate/stderror,term="Polyarchy",
         model = fct_relevel(model,"Year and country (Stock, 95 percent)","Year and country (Stock, 90 percent)","Year and country (level)","Year (level)",
                             "Baseline (level)"),
         independent = fct_relevel(independent,"Share female high prestige",
                                   "Share female weighted","Share female"))

###
# Create the figure ---
###

pd <- position_dodge(0.75)

figure3 <- ggplot(df_coefplot_poly, aes(y = estimate, x = independent, color = model, shape = model)) + 
  geom_point(position = pd, shape = 19) + 
  geom_linerange(aes(ymin = estimate-1.96*stderror, ymax = estimate+1.96*stderror),
                lwd = 1/2,
                position = pd) +
  geom_linerange(aes(ymin = estimate-1.645*stderror, ymax = estimate+1.645*stderror),
                lwd = 1,
                position = pd) +
  coord_flip(ylim=c(-10,40)) +
  labs(x = "", 
       y = "Coefficient estimate",
       color = "") +
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  theme_bw(base_size = 14) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "",
        axis.title=element_text(size=12),
        axis.text.x=element_text(colour="black"),
        axis.text.y=element_text(colour="black")) +
  scale_color_manual(values=c("#CB2314","#273046","#354823","#FAD510","black")) +
  guides(col=guide_legend(ncol=1,nrow=6)) +
  annotate("text", x = 3.3, y = 27, label = "Baseline (level)",size=3.5) +
  annotate("text", x = 3.15, y = 21, label = "Year (level)",size=3.5) +
  annotate("text", x = 3.0, y = 13.5, label = "Year and country (level)",size=3.5) +
  annotate("text", x = 2.85, y = 24, label = "Year and country (stock, 90%)",size=3.5) +
  annotate("text", x = 2.7, y = 33.5, label = "Year and country (stock, 95%)",size=3.5)

# Print figure ---

ggsave(
  "../3_output/figure3.pdf",
  figure3,
  width = 8,
  height = 6,
  dpi = 1200
)

###############
### Table 2 ###
###############

### Variables --

## Economic growth

# GDP per capita: gdp_cap_pwt_ln
# Oil: wb_oilrev
# Growth: growth_pwt
# Urbanization: wdi_popurb
# Population: pop_pwt_ln

## Human development

# Life expectancy: e_pelifeex
# Infant mortality rate: wb_infantmortality
# Primary school enrollment: wb_primaryschoolenrolment

## Women's representation

# Women political empowerment index: v2x_gender
# Lower chamber female legislators: v2lgfemleg
# Women’s Political Rights: ciri_wopol
# Women’s Economic Rights: ciri_wecon 
# Female leader: female_leader

## Institutional

# Individual Liberties: v2xcl_rol
# Private property: v2xcl_prpty
# Public administration: v2x_rule
# Judicial Constraints: v2x_jucon
# Legislative Constraints: v2xlg_legcon
# Political corruption index: v2x_corr
# State ownership of economy: v2clstown
# Core Civil Society: v2xcs_ccsi
# Party strength: v2xps_party

# Model 1 - Basic

m1 <- felm(share_female ~ lag_democracy_stock_poly_95 | year + country_isocode | 0 | country_isocode, data=df_cross)
summary(m1)

# Model 2 - Year trend

m2 <- felm(share_female ~ lag_democracy_stock_poly_95 + year | country_isocode | 0 | country_isocode, data=df_cross)
summary(m2)

# Model 3 - Growth and economic indicators

m3 <- felm(share_female ~ lag_democracy_stock_poly_95 + lag_gdp_cap_pwt_ln + lag_wb_oilrev + lag_growth_pwt + lag_wdi_popurb + lag_pop_pwt_ln | year + country_isocode | 0 | country_isocode, data=df_cross)
summary(m3)

# Model 4 - Human development (controls)

m4 <- felm(share_female ~ lag_democracy_stock_poly_95 + lag_e_pelifeex + lag_wb_infantmortality + lag_wb_primaryschoolenrolment | year + country_isocode | 0 | country_isocode, data=df_cross)
summary(m4)

# Model 5 - Women's representation

m5 <- felm(share_female ~ lag_democracy_stock_poly_95 + lag_v2x_gender + lag_v2lgfemleg + lag_ciri_wopol + lag_ciri_wecon + lag_female_leader | year + country_isocode | 0 | country_isocode, data=df_cross)
summary(m5)

# Model 6 - Institutional quality

m6 <- felm(share_female ~ lag_democracy_stock_poly_95 + lag_v2xcl_rol + lag_v2xcl_prpty + lag_v2x_rule + lag_v2x_jucon +
              lag_v2xlg_legcon + lag_v2x_corr + lag_v2clstown + lag_v2xcs_ccsi + lag_v2xps_party | year + country_isocode | 0 | country_isocode,data=df_cross)
summary(m6)

# Model 7 - Lagged dependent

m7 <- felm(share_female ~ lag_democracy_stock_poly_95 + lag_share_female | country_isocode + year | 0 | country_isocode,data=df_cross)
summary(m7)

# Model 8 - Static (random effects)

m8 <- lmer(share_female ~ year + lag_democracy_stock_poly_95 + lp_lat_abst + lp_muslim80 + lp_protmg80 + al_ethnic2000 + wdi_area + sai_statehiste0 + continent + (1 + year | country_isocode),data=df_cross)
summary(m8)

# Print table ---

gof.names <- c("Observations", "R-squared", "Years","Countries")

texreg(list(m1,m2,m3,m4,m5,m6,m7,m8),stars = c(0.05),
       file="../3_output/table2.tex",
       custom.model.names=c("(1)","(2)","(3)","(4)","(5)","(6)","(7)","(8)"),
       custom.coef.names=c("Stock of Polyarchy (95%)","Trend","Log of GDP per capita","Oil rents (% of GDP)","GDP growth","Urbanization","Log of population",
                           "Life expectancy","Infant mortality","Primary school enrolment","Women political empowerment index","Lower chamber female legislators","Women's political rights",
                           "Women's economic rights","Female leader","Individual liberties","Property rights","Rule of law","Judicial constraints","Legislative constraints",
                           "Political corruption","State ownership of economy","Core civil society","Party institutionalization","Lagged dependent","Constant","Latitude (ln)","Muslim","Protestant","Ethnic fractionalization",
                           "Land area","State history","Americas (ref: Africa)","Asia (ref: Africa)","Europe (ref: Africa)","Oceania (ref: Africa)"),
       no.margin = TRUE,
       include.adjrs = FALSE,
       include.rmse = FALSE,
       include.aic = FALSE,
       include.bic = FALSE,
       include.dic = FALSE,
       include.deviance = FALSE,
       include.loglik = FALSE,
       include.variance = FALSE,
       include.proj.stats = FALSE,
       use.packages = FALSE,
       label = "table1",
       caption ="Specification tests",
       caption.above=TRUE,
       custom.gof.names = gof.names,
       custom.header = list("Dependent variable: Share of female ministers" = 1:8),
       longtable = TRUE,
       custom.gof.rows = list("Estimation method:"= c("FE","FE (only country)","FE","FE","FE","FE","FE","RE")),
       custom.note = ("\\parbox{\\textwidth}{\\vspace{2pt}. \\\\
                      \\normalsize %stars. All right-side variables lagged by one year. Country clustered standard errors in parentheses. Estimator: OLS (ordinary least squares). FE = Fixed effects (country and year), RE = Random effects.}")
       )

################
### Figure 4 ###
################

# Composite measures of democracy

lm_freedomofexp <- felm(share_female ~ lag_stock_v2x_freexp_altinf | year + country_isocode | 0 | country_isocode,data=df_cross) %>% summary()
df_lm_freedomofexp <- tibble(model="Stock of freedom of expression (95%)",estimate = lm_freedomofexp$coefficients[1],stderror = lm_freedomofexp$coefficients[1,2])

lm_suffrage <- felm(share_female ~ lag_stock_v2x_suffr | year + country_isocode | 0 | country_isocode,data=df_cross) %>% summary()
df_lm_suffrage <- tibble(model="Stock of suffrage (95%)",estimate = lm_suffrage$coefficients[1],stderror = lm_suffrage$coefficients[1,2])

lm_freefair <- felm(share_female ~ lag_stock_v2xel_frefair | year + country_isocode | 0 | country_isocode,data=df_cross) %>% summary()
df_lm_freefair <- tibble(model="Stock of clean elections (95%)",estimate = lm_freefair$coefficients[1],stderror = lm_freefair$coefficients[1,2])

lm_elecoff <- felm(share_female ~ lag_stock_v2x_elecoff | year + country_isocode | 0 | country_isocode,data=df_cross) %>% summary()
df_lm_elecoff <- tibble(model="Stock of elected officials (95%)",estimate = lm_elecoff$coefficients[1],stderror = lm_elecoff$coefficients[1,2])

lm_assoc <- felm(share_female ~ lag_stock_v2x_frassoc_thick | year + country_isocode | 0 | country_isocode,data=df_cross) %>% summary()
df_lm_assoc <- tibble(model="Stock of associational autonomy (95%)",estimate = lm_assoc$coefficients[1],stderror = lm_assoc$coefficients[1,2])

## Bind models together

composite_result <- rbind(df_lm_freedomofexp,df_lm_suffrage,df_lm_freefair,df_lm_elecoff,df_lm_assoc) %>% 
  mutate(model = fct_relevel(model,"Stock of suffrage (95%)","Stock of elected officials (95%)","Stock of associational autonomy (95%)",
                             "Stock of freedom of expression (95%)","Stock of clean elections (95%)"))

figure4 <- ggplot(composite_result, aes(y = estimate, x = model)) + 
  geom_point(position = pd) + 
  geom_linerange(aes(ymin = estimate-1.96*stderror, ymax = estimate+1.96*stderror),
                 lwd = 1/2,
                 position = pd,colour="#273046") +
  geom_linerange(aes(ymin = estimate-1.645*stderror, ymax = estimate+1.645*stderror),
                 lwd = 1,
                 position = pd,colour="#273046") +
  coord_flip() +
  labs(x = "", 
       y = "Coefficient estimate",
       color = "") +
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  theme_bw(base_size = 14) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "right",plot.title = element_text(size=14),
        axis.title=element_text(size=12),
        axis.text.x=element_text(colour="black"),
        axis.text.y=element_text(colour="black"))

# Print figure ---

ggsave(
  "../3_output/figure4.pdf",
  figure4,
  width = 6,
  height = 3,
  dpi = 1200
)

################
### Figure 5 ###
################

# Create running variable

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

pd <- position_dodge(0.75)

figure5 <- ggplot(df_transition_dem,aes(x=counter_dem,y=sharefemale_mean)) +
  geom_point(position = pd,colour="#273046") + 
  geom_errorbar(aes(ymin = sharefemale_mean-1.96*sharefemale_se, ymax = sharefemale_mean+1.96*sharefemale_se),
                width = 0,
                position = pd,
                size = 1,
                colour = "#273046") +
  labs(x = "Years from democratization", 
       y = "% women in cabinet\n(relative to the year of democratic transition)",
       color = "black") +
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "",plot.title = element_text(size=14),
        axis.text.y = element_text(colour = "black",size=14),
        axis.text.x = element_text(colour = "black",size=14),
        text=element_text(size=14,color="black")) +
  geom_vline(xintercept=0) +
  scale_x_continuous(breaks=c(-3,0, 3, 6)) +
  scale_y_continuous(breaks=c(-6,-3, 0, 3, 6),labels=  function(x) paste0(x, "%"), limits = c(-7,7))

# Print figure ---

ggsave(
  "../3_output/figure5.pdf",
  figure5,
  width = 8,
  height = 5,
  dpi = 1200
)

################
### Figure 6 ###
################

### OECD & Non-OECD ---

# OECD

OECD <- c("AUS","AUT","BEL","CAN","CHL","CZE","DNK","EST","FIN","FRA","DEU","GRC","HUN","ISL","IRL","ITA","JPN","KOR","LUX","MEX","NLD","NZL","NOR",
          "POL","PRT","SVK","SVN","ESP","SWE","CHE","TUR","GBR","USA")

df_oecd <- df_cross %>% filter(country_isocode %in% OECD)

reg_oecd_95 <- felm(share_female ~ lag_democracy_stock_poly_95 | year + country_isocode | 0 | country_isocode,data=df_oecd) %>% summary()
df_subset_1 <- tibble(model="Stock of Polyarchy (95%)",estimate = reg_oecd_95$coefficients[1],stderror = reg_oecd_95$coefficients[1,2])

oecd <- rbind(df_subset_1) %>% mutate("Subset" = "OECD")

# Not OECD

df_notoecd <- df_cross %>% filter(country_isocode %!in% OECD)

reg_notwestern_95 <- felm(share_female ~ lag_democracy_stock_poly_95 | year + country_isocode | 0 | country_isocode,data=df_notoecd) %>% summary()
df_subset_2 <- tibble(model="Stock of Polyarchy (95%)",estimate = reg_notwestern_95$coefficients[1],stderror = reg_notwestern_95$coefficients[1,2])

nonoecd <- rbind(df_subset_2) %>% mutate("Subset" = "Non-OECD")

### Low GDPpc & High GDPpc ---

# Low

df_low <- df_cross %>% filter(gdp_cap_pwt_ln < log(7000))

reg_low <- felm(share_female ~ lag_democracy_stock_poly_95 | year + country_isocode | 0 | country_isocode,data=df_low) %>% summary()
df_subset_3 <- tibble(model="Stock of Polyarchy (95%)",estimate = reg_low$coefficients[1],stderror = reg_low$coefficients[1,2])

low <- rbind(df_subset_3) %>% mutate("Subset" = "GDP per capita < $7000")

# High countries

df_high <- df_cross %>% filter(gdp_cap_pwt_ln >= log(7000))

reg_high <- felm(share_female ~ lag_democracy_stock_poly_95 | year + country_isocode | 0 | country_isocode,data=df_high) %>% summary()
df_subset_4 <- tibble(model="Stock of Polyarchy (95%)",estimate = reg_high$coefficients[1],stderror = reg_high$coefficients[1,2])

high <- rbind(df_subset_4) %>% mutate("Subset" = "GDP per capita > $7000")

### Before and after ---

# Before

df_before <- df_cross %>% filter(year < 1993)

reg_before <- felm(share_female ~ lag_democracy_stock_poly_95 | year + country_isocode | 0 | country_isocode,data=df_before) %>% summary()
df_subset_5 <- tibble(model="Stock of Polyarchy (95%)",estimate = reg_before$coefficients[1],stderror = reg_before$coefficients[1,2])

before <- rbind(df_subset_5) %>% mutate("Subset" = "Before 1993")

# After

df_after <- df_cross %>% filter(year >= 1993)

reg_after <- felm(share_female ~ lag_democracy_stock_poly_95 | year + country_isocode | 0 | country_isocode,data=df_after) %>% summary()
df_subset_6 <- tibble(model="Stock of Polyarchy (95%)",estimate = reg_after$coefficients[1],stderror = reg_after$coefficients[1,2])

after <- rbind(df_subset_6) %>% mutate("Subset" = "After 1993")

# Bind

df_coefplot_subset <- rbind(low,high,oecd,nonoecd,before,after) %>% 
  mutate(std.effect=estimate/stderror,term="Polyarchy",
         model = fct_relevel(model,"Stock of Polyarchy (99%)","Stock of Polyarchy (95%)"),
         Subset = fct_relevel(Subset,"After 1993","Before 1993",
                                          "GDP per capita < $7000","GDP per capita > $7000",
                                          "Non-OECD","OECD")) %>% filter(model == "Stock of Polyarchy (95%)")

# Create plot

figure6 <- ggplot(df_coefplot_subset, aes(y = estimate, x = Subset)) + 
  geom_point(position = pd) + 
  geom_linerange(aes(ymin = estimate-1.96*stderror, ymax = estimate+1.96*stderror),
                 lwd = 1/2,
                 position = pd,
                 colour = "#273046") +
  geom_linerange(aes(ymin = estimate-1.645*stderror, ymax = estimate+1.645*stderror),
                 lwd = 1,
                 position = pd,
                 colour = "#273046") +
  coord_flip() +
  labs(x = "", 
       y = "Coefficient estimate",
       color = "") +
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  theme_bw(base_size = 14) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "",plot.title = element_text(size=12),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12)) +
  ylim(-40,50) +

  guides(col=guide_legend(ncol=1,nrow=6))

# Print figure ---

ggsave(
  "../3_output/figure6.pdf",
  figure6,
  width = 8,
  height = 6,
  dpi = 1200
)
