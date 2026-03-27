##### ##### ##### ##### ##### ##### ##### 
##### Disaggregating Resources
##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 

## prelims
rm(list = setdiff(c(ls(),lsf.str()),c("scrdir","ab")))
source("regression_functions.R")
tab.path <- "results/"

# define control variables
controls.ind <- c("sex", "age", "age2", "urban", "f.educ",  "f.food", "f.income", "f.water",  "ea_sex", "ea_age"  )
controls.geo <- c("ruggedness_nunn_puga" ,  "elevation_mean"  ,  "malaria_suit_max",
                  "TSI_CRU_mean_1901_1920" , "temperature_fao" , "precipitation_fao",  "coast_log"  ,
                  "longitude", "agric_suit", "latitude", "lpopc_hyde_meanprecol_poly")
controls.hist <- c("explorers_log", "cities_log", "capital_log" , "dist.prot_log" , "dist.print_log"  )

base.x <-  c(controls.ind, controls.geo,controls.hist)

## join in disaggregated crops
crop.df <- readRDS("afrobarometer/other_crops2ab.rds")
ab <- left_join(ab,crop.df)


### code smallholder and plantation crops

## coffee in Kenya and Angola as plantation crops
ab$coffee_sum_15km_afr <- ab$coffee_sum_15km 
ab$coffee_sum_15km_afr[ab$country_iso3c%in%c("KEN","AGO","COD")] <- 0
ab$coffee_sum_15km_eur <- ab$coffee_sum_15km 
ab$coffee_sum_15km_eur[!ab$country_iso3c%in%c("KEN","AGO","COD")] <- 0

# rubber in COD and LBR, sisal in TZA, rubber in cameroon
ab$ind_crops_sum_15km_afr <- ab$ind_crops_sum_15km 
ab$ind_crops_sum_15km_afr[ab$country_iso3c%in%c("TZA","LBR","COD","CMR")] <- 0
ab$ind_crops_sum_15km_eur <- ab$ind_crops_sum_15km 
ab$ind_crops_sum_15km_eur[!ab$country_iso3c%in%c("TZA","LBR","COD","CMR")] <- 0


# palm in COD and CMR
ab$palm_sum_15km_afr <- ab$palm_sum_15km 
ab$palm_sum_15km_afr[ab$country_iso3c%in%c("COD","CMR")] <- 0
ab$palm_sum_15km_eur <- ab$palm_sum_15km 
ab$palm_sum_15km_eur[!ab$country_iso3c%in%c("COD","CMR")] <- 0


# Tobacco in Nyasaland and the Rhodesias, Tea in KEN, TZA, UGA, MOZ
ab$stimulant_sum_15km_afr <- ab$stimulant_sum_15km 
ab$stimulant_sum_15km_afr[ab$country_iso3c%in%c("ZWE","ZMB","MWI","KEN","TZA","UGA","MOZ")] <- 0
ab$stimulant_sum_15km_eur <- ab$stimulant_sum_15km 
ab$stimulant_sum_15km_eur[!ab$country_iso3c%in%c("ZWE","ZMB","MWI","KEN","TZA","UGA","MOZ")] <- 0

ab$crops_sum_15_km_afr <- ab$cocoa_sum_15km + ab$cotton_sum_15km + ab$coffee_sum_15km_afr + ab$groundut_sum_15km + ab$palm_sum_15km_afr +
  ab$ind_crops_sum_15km_afr + ab$stimulant_sum_15km_afr + ab$other_oils_sum_15km

ab$crops_sum_15_km_eur <- ab$coffee_sum_15km_eur + ab$palm_sum_15km_eur +
  ab$ind_crops_sum_15km_eur + ab$stimulant_sum_15km_eur + ab$food_sum_15km

ab$crops_sum_15_km_afr_std <- scale(ab$crops_sum_15_km_afr)[,1] 
ab$crops_sum_15_km_eur_std <- scale(ab$crops_sum_15_km_eur)[,1] 

ab$crops_other_sum_15km <- ab$stimulant_sum_15km + ab$food_sum_15km + ab$other_oils_sum_15km + ab$ind_crops_sum_15km
ab$crops_other_sum_15km_std <- scale(ab$crops_other_sum_15km)[,1] 

ab$minerals_15km_std <- scale(ab$hance_minerals_sum_15km)[,1]



m1 <- felm(as.formula(RegFor( y = "identity_more_ethnic_sc", x = c( "pubspc.23_poly_sc", "crop5_sum_15_km_std", base.x) ,
                              FE = "country_survey_round" , IV="0", clust = "loc.id" )),
           data=ab )
summary(m1)

m2 <- felm(as.formula(RegFor( y = "identity_more_ethnic_sc", x = c( "pubspc.23_poly_sc", "crop_sum_15_km_std", base.x) ,
                              FE = "country_survey_round" , IV="0", clust = "loc.id" )),
           data=ab)
summary(m2)

m3 <- felm(as.formula(RegFor( y = "identity_more_ethnic_sc", x = c( "pubspc.23_poly_sc", "crop5_sum_15_km_std","crops_other_sum_15km_std","minerals_15km_std",base.x) ,
                              FE = "country_survey_round" , IV="0", clust = "loc.id" )),
           data=ab)
summary(m3)

m4 <- felm(as.formula(RegFor( y = "identity_more_ethnic_sc", x = c( "pubspc.23_poly_sc", "crops_sum_15_km_afr_std","crops_sum_15_km_eur_std","minerals_15km_std",base.x) ,
                              FE = "country_survey_round" , IV="0", clust = "loc.id" )),
           data=ab)
summary(m4)


### prepare output table
keepvars <- c("pubspc.23_poly_sc","crop5_sum_15_km_std","crop_sum_15_km_std","crops_other_sum_15km_std","crops_sum_15_km_afr_std","crops_sum_15_km_eur_std","minerals_15km_std")
keeplabs <- c("In bibliographies 23","Top 5 Cash Crops","All Cash Crops","Other Cash Crops","Smallholder Cash Crops","Plantation Cash Crops","Minerals")

yss <- ab %>%
  summarize_at( "identity_more_ethnic" , funs(mean( ., na.rm =T)
                              , sd(. , na.rm =T))) %>%
  mutate_all(~round(.,4))



m.list <- list(m1,m2,m3,m4)



add.lines <- list(
  AddLines( length(m.list), "Individual controls" , rep(LatMc("yes",1), 5) ) ,
  AddLines( length(m.list), "Historical and Geo controls" , rep(LatMc("yes",1), 5) ) ,
  AddLines( length(m.list), "FE" , rep(LatMc("Country-Round",1),5) ) ,
  AddLines( length(m.list), "Ethnic Stayer/Leaver" , c(rep(LatMc("Both",1),5) )) ,
  AddLines( length(m.list), "Mean dep. var." , rep(LatMc(yss[1],1), 5))
)

tab.name <- "ab_tableA10.tex"
fileConn<-file(paste0(tab.path,tab.name))
print(fileConn)
writeLines(stargazer(m.list,
                     float = FALSE ,
                     keep=keepvars,
                     order = keepvars ,
                     covariate.labels= keeplabs ,
                     multicolumn=F,# se = se,
                     column.sep.width="0pt",
                     dep.var.caption = paste("Geographic- level - Ethnic vs National Id" ) ,
                     dep.var.labels.include = F,
                     #column.labels = c("OLS","IV"), column.separate = c(3,1),
                     font.size = "scriptsize",
                     omit.table.layout = "n",
                     align =T,
                     add.lines = add.lines,
                     digits = 3, intercept.top = T,intercept.bottom = F,
                     keep.stat = c("rsq" , "n") ,
                     omit.stat = c("res.dev","ser") ),
           fileConn)
close(fileConn)
