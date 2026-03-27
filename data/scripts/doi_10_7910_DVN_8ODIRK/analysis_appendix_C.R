############################
## Replication Material: Script for replicating robustness checks: Appendix C
## Paltra, Sältzer, Stecker
## Affective States - Cultural and Affective Polarization in a Multilevel-Multiparty System 
## 2025
## Political Behavior
############################

# Loading all required definitions and datasets--------------------------------
source("_loadlibraries.R")
source("_definitions.R", encoding = "UTF-8")
source("toolbox.R")
load("party_placement_ltw.rdata")
load("reg_data.rdata")
load("lr_polarization_citizens.rdata")
load("migration_saliency.rdata")


lr_sd_landyear$state<-substr(lr_sd_landyear$landyear,1,2)

reg_data$year<-as.numeric(as.character(reg_data$year.x))

lr_sd_landyear$state<-substr(lr_sd_landyear$landyear,1,2)
lr_sd_landyear<-lr_sd_landyear[-1,]
states<-unique(lr_sd_landyear$state)

reg_data2<-merge(reg_data,lr_sd_landyear,by="landyear",all.x=T)
reg_data2$has_afd<-ifelse(is.na(reg_data2$lr_afd),0,1)
reg_data2$year<-reg_data2$year-1990

reg_data2$land<-reg_data2$land.x

# Calculation of Lags for later models -----------------------------------------
var<-"lr_sd"
get_lags<-function(data,var){
  ncols<-ncol(data)
  data<-data[order(data$year),]
  dfs<-split(data,data$land)
  dfs<-lapply(dfs,function(x) cbind(x,dplyr::lag(x[,var],1),dplyr::lag(x[,var],2),dplyr::lag(x[,var],3)))
  lagged<-do.call(rbind,dfs)
  names(lagged)[(ncols+1):(ncols+3)]<-paste0(var,"_",c("lag1","lag2","lag3"))
  return(lagged)  
}


reg_data2<-merge(reg_data2,mig_sal,by="landyear")

data<-reg_data2
reg_data2$compare_sd<-reg_data2$econ_sd-reg_data2$soci_sd
reg_data2$lr_sd

reg_data2 <- reg_data2[reg_data2$lr_ego < 3,]

lagged<-get_lags(data=reg_data2,"lr_sd")

lagged<-get_lags(data=lagged,"wap_spread")
lagged<-get_lags(data=lagged,"has_afd")

lagged<-get_lags(data=lagged,"soci_sd")
lagged<-get_lags(data=lagged,"econ_sd")
lagged<-get_lags(data=lagged,"immi_sd")
lagged<-get_lags(data=lagged,"wap_distance")
lagged<-get_lags(data=lagged,"compare_sd")
lagged<-get_lags(data=lagged,"lr_ego")

lagged$Economic_Elite_Polarization<-lagged$econ_sd
lagged$Cultural_Elite_Polarization<-lagged$soci_sd
lagged$AfD_Present<-lagged$has_afd
lagged$Elite_Polarization<-lagged$lr_sd
lagged$Population_Polarization<-lagged$lr_ego
lagged$Affective_Polarization<-lagged$wap_spread


lagged$Economic_Elite_Polarization_Lag1<-lagged$econ_sd_lag1
lagged$Cultural_Elite_Polarization_Lag1<-lagged$soci_sd_lag1
lagged$AfD_Present_Lag1<-lagged$has_afd_lag1
lagged$Elite_Polarization_Lag1<-lagged$lr_sd_lag1
lagged$Population_Polarization_Lag1<-lagged$lr_ego_lag1
lagged$Affective_Polarization_Lag1<-lagged$wap_spread_lag1
lagged$Migration_Saliency<-lagged$mig
lagged_sub<-lagged[!is.na(lagged$Population_Polarization),]

m0<-lm(Affective_Polarization~lr_sd+AfD_Present,data=lagged)
m1<-lm(Affective_Polarization~Elite_Polarization+AfD_Present+Population_Polarization,data=lagged)

summary(m0)
summary(m1)

m0<-lm(Affective_Polarization~Elite_Polarization,data=lagged)
m1<-lmer(Affective_Polarization~Elite_Polarization+AfD_Present+Population_Polarization+(1|land),data=lagged_sub)
m2<-lm(Affective_Polarization~Elite_Polarization+Elite_Polarization_Lag1+Affective_Polarization_Lag1,data=lagged_sub)
m3<-lm(Affective_Polarization~Elite_Polarization+Elite_Polarization_Lag1+Affective_Polarization_Lag1,data=lagged_sub)
summary(m3)
m3a<-lm(Affective_Polarization~Elite_Polarization+Elite_Polarization_Lag1+Affective_Polarization_Lag1+AfD_Present_Lag1+AfD_Present+Population_Polarization+Population_Polarization_Lag1,data=lagged_sub[lagged_sub$lr_ego<3,])
summary(m3a)

s_m0<-texreg::extract(m0)
s_m1<-texreg::extract(m0)
s_m2<-texreg::extract(m2)
s_m3<-texreg::extract(m3)
s_m4<-texreg::extract(m3a)

# Regression Table C4 ------------------------------------------------------------
texreg(list(s_m0,s_m1,s_m2, s_m4),file = "mixed_models_spread_tab.tex",label = "table:coefficients_mm",
       custom.coef.names = c("(Intercept)",
                             "Party Polarization",
                             "First Lag Party Polarization",
                             "First Lag Affective Polarization",
                             "First Lag AfD Presence",
                             "AfD Presence",
                             "Population Polarization",
                             "First Lag Population Polarization"),
       custom.model.names = c("Naive",
                              "AfD Presence",
                              "Pol Lagged",
                              "Full Lagged"))


dw1<-dwplot(list("Naive"=m0,"AfD Presence" = m1,"Lagged, Polarization"=m2,"Lagged, all Variables"=m3a),effect="fixed",dot_args = list(size = 5),
            whisker_args = list(size = 2)) +
  theme_bw() +
  scale_y_discrete(labels = c("Elite_Polarization" = "Party Polarization",
                              "AfD_Present" = "AfD Presence",
                              "Population_Polarization" = "Population Polarization",
                              "Elite_Polarization_Lag1" = "First Lag Party Polarization",
                              "Affective_Polarization_Lag1" = "First Lag Affective Polarization",
                              "AfD_Present_Lag1" = "First Lag AfD Presence",
                              "Population_Polarization_Lag1" = "First Lag Population Polarization",
                              "Migration_Saliency" = "Migration Saliency")) +
  geom_vline(xintercept=0,lty=2)+theme(text = element_text(size = 24),
                                       axis.text.x = element_text(angle = 90, hjust = 1))  +
  labs(color = "Models")

# Robustness Check with WAP (Spread) ---------------------------------
e1<-lmer(Affective_Polarization~Economic_Elite_Polarization+Economic_Elite_Polarization_Lag1+Affective_Polarization_Lag1+AfD_Present_Lag1+Population_Polarization_Lag1+Population_Polarization+AfD_Present+(1|land),data=lagged_sub)
summary(e1)

e2<-lmer(Affective_Polarization~Cultural_Elite_Polarization+Cultural_Elite_Polarization_Lag1+Affective_Polarization_Lag1+AfD_Present_Lag1+Population_Polarization_Lag1+Population_Polarization+
           AfD_Present+(1|land),data=lagged_sub)
summary(e2)

e3<-lmer(Affective_Polarization~Economic_Elite_Polarization+Economic_Elite_Polarization_Lag1+Affective_Polarization_Lag1+AfD_Present_Lag1+Population_Polarization_Lag1+Population_Polarization+
           AfD_Present+(1|land)+Cultural_Elite_Polarization+Cultural_Elite_Polarization_Lag1,data=lagged_sub)
summary(e3)



s_e1<-texreg::extract(e1)
s_e2<-texreg::extract(e2)
s_e3<-texreg::extract(e3)

# Regression Table C5 ------------------------------------------------------------
texreg(list(s_e1,s_e2,s_e3),file = "econ_soci_spread_tab.tex",label = "table:coefficients_econ",
       custom.coef.names = c("(Intercept)",
                             "Economic Party Polarization",
                             "First Lag Economic Party Polarization",
                             "First Lag Affective Polarization",
                             "First Lag AfD Presence",
                             "First Lag Population Polarization",
                             "Population Polarization",
                             "AfD Presence",
                             "Cultural Party Polarization",
                             "First Lag Cultural Party Polarization"),
       custom.model.names = c("Economic",
                              "Cultural",
                              "Combined"))


dw2<-dwplot(list("Economic"=e1,"Cultural"=e2,"Combined"=e3),effect="fixed",dot_args = list(size = 5),
            whisker_args = list(size = 2)) + theme_bw() +
  scale_y_discrete(labels = c("AfD_Present" = "AfD Presence",
                              "Population_Polarization" = "Population Polarization",
                              "Economic_Elite_Polarization" = "Economic Party Polarization",
                              "Economic_Elite_Polarization_Lag1" = "First Lag Economic Party Polarization",
                              "Cultural_Elite_Polarization" = "Cultural Party Polarization",
                              "Cultural_Elite_Polarization_Lag1" = "First Lag Cultural Party Polarization",
                              "Affective_Polarization_Lag1" = "First Lag Affective Polarization",
                              "AfD_Present_Lag1" = "First Lag AfD Presence",
                              "Population_Polarization_Lag1" = "First Lag Population Polarization")) +
  geom_vline(xintercept=0,lty=2)+theme(text = element_text(size = 24),
                                       axis.text.x = element_text(angle = 90, hjust = 1))  +
  labs(color = "Models")

# Calculation of Interaction Plots with WAP (Spread) ---------------------------


i1<-lmer(Affective_Polarization~Economic_Elite_Polarization*AfD_Present+Cultural_Elite_Polarization+Population_Polarization+year+(1|land),data=lagged)
summary(i1)

i2<-lmer(Affective_Polarization~Cultural_Elite_Polarization*AfD_Present+Economic_Elite_Polarization+Population_Polarization+year+(1|land),data=lagged)
summary(i2)


lagged_f<-data

lagged_f$Economic_Elite_Polarization<-lagged_f$econ_sd
lagged_f$Cultural_Elite_Polarization<-lagged_f$soci_sd
lagged_f$AfD_Present<-lagged_f$has_afd
lagged_f$Elite_Polarization<-lagged_f$lr_sd
lagged_f$Population_Polarization<-lagged_f$lr_ego
lagged_f$Affective_Polarization<-lagged_f$wap_spread
lagged_f$Migration_Saliency<-lagged_f$mig



i3<-lmer(Affective_Polarization~Economic_Elite_Polarization*AfD_Present+Cultural_Elite_Polarization+year+(1|land),data=lagged_f)
summary(i3)


i4<-lmer(Affective_Polarization~Cultural_Elite_Polarization*AfD_Present+Economic_Elite_Polarization+year+(1|land),data=lagged_f)
summary(i4)

i1m<-lmer(Affective_Polarization~Economic_Elite_Polarization*AfD_Present+Cultural_Elite_Polarization+Migration_Saliency+year+(1|land),data=lagged_f)
summary(i1m)


i2m<-lmer(Affective_Polarization~Cultural_Elite_Polarization*AfD_Present+Economic_Elite_Polarization+year+Migration_Saliency+(1|land),data=lagged_f)
summary(i2m)

s_i1m<-texreg::extract(i1m)
s_i2m<-texreg::extract(i2m)

s_i1<-texreg::extract(i1)
s_i2<-texreg::extract(i2)
s_i3<-texreg::extract(i3)
s_i4<-texreg::extract(i4)

# Regression Table C6 ----------------------------------------------------------
texreg(list(s_i1,s_i2,s_i3,s_i4),file = "interactions_spread_tab.tex",label = "table:coefficients_interaction_spread",
       custom.coef.names = c("(Intercept)",
                             "Economic Party Polarization",
                             "AfD Presence",
                             "Cultural Party Polarization",
                             "Population Polarization",
                             "Year",
                             "Economic Party Polarization x AfD Presence",
                             "Cultural Party Polarization x AfD Presence"),
       custom.model.names = c("Economic Model", "Cultural Model","Econ Full Sample","Cult Full Sample"))

texreg(list(s_i1m,s_i2m))

# Regression Table C7 -----------------------------------------------------------
texreg(list(s_i1m,s_i2m),file = "interactions_spread_tab_mig.tex",label = "table:coefficients_interaction_mig_spread",
       custom.coef.names = c("(Intercept)",
                             "Economic Party Polarization",
                             "AfD Presence",
                             "Cultural Party Polarization",
                             "Migration Saliency",
                             "Year",
                             "Economic Party Polarization x AfD Presence",
                             "Cultural Party Polarization x AfD Presence"
       ),
       custom.model.names = c("Economic Model", "Cultural Model"))
