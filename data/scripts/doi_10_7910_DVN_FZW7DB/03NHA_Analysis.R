#################
# load packages #
#################

library(stargazer)
library(ggplot2)
library(estimatr)
library(lme4)
library(rdrobust)
library(rdd)

sessionInfo()
#R version 4.2.0 (2022-04-22 ucrt)
#Platform: x86_64-w64-mingw32/x64 (64-bit)
#Running under: Windows 10 x64 (build 22000)

#Matrix products: default

#locale:
#[1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8    LC_MONETARY=English_United States.utf8
#[4] LC_NUMERIC=C                           LC_TIME=English_United States.utf8    

#attached base packages:
#[1] stats     graphics  grDevices utils     datasets  methods   base     

#other attached packages:
#[1] rdd_0.57        Formula_1.2-4   AER_1.2-10      survival_3.3-1  car_3.1-0       carData_3.0-5   lmtest_0.9-40  
#[8] zoo_1.8-10      sandwich_3.0-2  rdrobust_2.0.2  lme4_1.1-29     Matrix_1.4-1    estimatr_0.30.6 ggplot2_3.3.6  
#[15] stargazer_5.2.3

#loaded via a namespace (and not attached):
#[1] zip_2.2.0        Rcpp_1.0.8.3     nloptr_2.0.3     pillar_1.7.0     compiler_4.2.0   tools_4.2.0     
#[7] boot_1.3-28      nlme_3.1-157     lifecycle_1.0.1  tibble_3.1.7     gtable_0.3.0     lattice_0.20-45 
#[13] pkgconfig_2.0.3  rlang_1.0.2      openxlsx_4.2.5   cli_3.3.0        withr_2.5.0      dplyr_1.0.9     
#[19] generics_0.1.2   vctrs_0.4.1      grid_4.2.0       tidyselect_1.1.2 glue_1.6.2       R6_2.5.1        
#[25] fansi_1.0.3      minqa_1.2.4      purrr_0.3.4      magrittr_2.0.3   MASS_7.3-56      scales_1.2.0    
#[31] ellipsis_0.3.2   splines_4.2.0    abind_1.4-5      colorspace_2.0-3 utf8_1.2.2       stringi_1.7.6   
#[37] munsell_0.5.0    crayon_1.5.1  




#############
# load data #
#############

load("NHAData_October2022_JJPS.RData")




###########
# Table 1 #
###########

# model 1, outcome = trust 国
t1 <- lmer(V4303rev ~ snow_treat + log(V0201s)
           + log(mun_population_density) 
           + log(income_pc) + log(primary_pc) + dependent_pc + mun_ceif
           + factor(district) + (1|pmg), 
           data=df.sub)
summary(t1)


# model 2, outcome = trust 政党・国会議員
t2 <- lmer(V4304rev ~ snow_treat + log(V0201s)
           + log(mun_population_density) 
           + log(income_pc) + log(primary_pc) + dependent_pc + mun_ceif
           + factor(district) + (1|pmg), 
           data=df.sub)
summary(t2)


# model 3, outcome = 選挙での候補者支持
m9r2 <- lmer(V1732 ~ snow_treat + log(V0201s)
             + log(mun_population_density) 
             + log(income_pc) + log(primary_pc) + dependent_pc + mun_ceif
             + factor(district) + (1|pmg), 
             data=df.sub)
summary(m9r2)




###################################
# Figure 4 and Appendix Table G.1 #
###################################

# Table G.1 model 1, outcome = モニタリング
m10r2 <- lmer(V2600rev ~ snow_treat + log(V0201s)
              + log(mun_population_density) 
              + log(income_pc) + log(primary_pc) + dependent_pc + mun_ceif
              + factor(district) + (1|pmg), 
              data=df.sub)
summary(m10r2)


#  Table G.1 model 2, outcome = 市町村担当課への要望
m11r2 <- lmer(V2701rev ~ snow_treat + log(V0201s)
              + log(mun_population_density) 
              + log(income_pc) + log(primary_pc) + dependent_pc + mun_ceif
              + factor(district) + (1|pmg), 
              data=df.sub)
summary(m11r2)


#  Table G.1 model 3, outcome = 市町村幹部への要望
m12r2 <- lmer(V2702rev ~ snow_treat + log(V0201s)
              + log(mun_population_density) 
              + log(income_pc) + log(primary_pc) + dependent_pc + mun_ceif
              + factor(district) + (1|pmg), 
              data=df.sub)
summary(m12r2)


#  Table G.1 model 4, outcome = 市町村議員への要望
m13r2 <- lmer(V2703rev ~ snow_treat + log(V0201s)
              + log(mun_population_density) 
              + log(income_pc) + log(primary_pc) + dependent_pc + mun_ceif
              + factor(district) + (1|pmg), 
              data=df.sub)
summary(m13r2)


#  Table G.1 model 5, outcome = 懇談会への出席
m14r2 <- lmer(V2706rev ~ snow_treat + log(V0201s)
              + log(mun_population_density) 
              + log(income_pc) + log(primary_pc) + dependent_pc + mun_ceif
              + factor(district) + (1|pmg), 
              data=df.sub)
summary(m14r2)


#  Table G.1 model 6, outcome = 請願・陳情
m15r2 <- lmer(V2707rev ~ snow_treat + log(V0201s)
              + log(mun_population_density) 
              + log(income_pc) + log(primary_pc) + dependent_pc + mun_ceif
              + factor(district) + (1|pmg), 
              data=df.sub)
summary(m15r2)


#  Table G.1 model 7, outcome = 自治会の影響力
m16r2 <- lmer(V3000rev ~ snow_treat + log(V0201s)
              + log(mun_population_density) 
              + log(income_pc) + log(primary_pc) + dependent_pc + mun_ceif
              + factor(district) + (1|pmg), 
              data=df.sub)
summary(m16r2)


# make a summary table
bargain_summary <- data.frame(outcome = c("Monitor Local Government", "Consult Local Government",
                                          "Consult Senior Officials", "Consult Local Politicians",
                                          "Attend Government Meetings", "Lobby Local Assembly",
                                          "Influence on Local Government"),
                              est = c(fixef(m10r2)[2], fixef(m11r2)[2],
                                      fixef(m12r2)[2], fixef(m13r2)[2],
                                      fixef(m14r2)[2], fixef(m15r2)[2],
                                      fixef(m16r2)[2]),
                              lwr = NA,
                              upr = NA)
bargain_summary$outcome <- factor(bargain_summary$outcome, levels = rev(bargain_summary$outcome))


# function to get CIs
getCI <- function(model){
  mcoef <- fixef(model)[2]
  mse <- sqrt(diag(vcov(model)))[2]
  mn <- nrow(model@frame)
  mqt <- qt(0.975, mn)
  c(mcoef - mqt * mse, mcoef + mqt * mse)
}

bargain_summary[,3:4] <- rbind(getCI(m10r2), getCI(m11r2),
                               getCI(m12r2), getCI(m13r2),
                               getCI(m14r2), getCI(m15r2),
                               getCI(m16r2))


# plot results
ggplot(data = bargain_summary, aes(x = est, y = outcome)) +
  geom_vline(xintercept=0, lty=3) +
  geom_point(cex = 3) + 
  geom_errorbar(aes(xmin = lwr, xmax = upr),
                lwd=0.8, width = 0.4) +
  xlab("Effect of Snow Subsidy on NHA Heads' Relationship with Local Government Actors") + 
  ylab("") +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13))
#ggsave("nha_local_government_relationship.pdf", width = 10, height = 8)
