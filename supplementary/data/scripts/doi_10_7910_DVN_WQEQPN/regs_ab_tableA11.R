##### ##### ##### ##### ##### ##### ##### 
##### Controlling for Local Ethnic Diversity
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

##### add wlms_based fractionalization raster
##### load african prio grid 
prio.afr <- st_read("afrobarometer/prio_afr.geojson") 

### make ab.clusters as SpatialPoints
ab.sp <- unique(ab[,c("loc.id","longitude","latitude")])
ab.sp <- ab.sp[ab.sp$longitude!=0 & !is.na(ab.sp$longitude),]
coordinates(ab.sp) <- ab.sp[,c("longitude","latitude")]

### assign Prio grid IDs to Ab clusters
ab.sf <- st_as_sf(ab.sp)
st_crs(ab.sf) <- st_crs(prio.afr)
ab2prio <- sapply(st_intersects(ab.sf,prio.afr), function(z) if (length(z)==0) NA_integer_ else z[1])
ab.sf$gid <- prio.afr$gid[ab2prio]

ab <- left_join(ab,st_drop_geometry(ab.sf))
names(ab)

## load fractionalization raster
terr_frac <- readRDS("afrobarometer/terr_frac_wlms.rds")
ab <- left_join(ab,terr_frac)


m1 <- felm(as.formula(RegFor( y = "identity_more_ethnic_sc" , x = c("crop_poly_sc", base.x) ,
                              FE = "country_survey_round" , IV="0", clust = "loc.id" )),
           data=ab )
m1b <- felm(as.formula(RegFor( y = "identity_more_ethnic_sc" , x = c("crop_poly_sc", base.x,"terr_frac_cell_alt") ,
                               FE = "country_survey_round" , IV="0", clust = "loc.id" )),
            data=ab )
m2 <- felm(as.formula(RegFor( y = "identity_more_ethnic_sc" , x = c( "pubspc.23_poly_sc", base.x) ,
                              FE = "country_survey_round " , IV="0", clust = "loc.id" )),
           data=ab )
m2b <- felm(as.formula(RegFor( y = "identity_more_ethnic_sc" , x = c( "pubspc.23_poly_sc", base.x,"terr_frac_cell_alt") ,
                               FE = "country_survey_round " , IV="0", clust = "loc.id" )),
            data=ab )
m3 <- felm(as.formula(RegFor( y = "identity_more_ethnic_sc", x = c( "pubspc.23_poly_sc", "crop_poly_sc", base.x) ,
                              FE = "country_survey_round" , IV="0", clust = "loc.id" )),
           data=ab)
m3b <- felm(as.formula(RegFor( y = "identity_more_ethnic_sc", x = c( "pubspc.23_poly_sc", "crop_poly_sc", base.x,"terr_frac_cell_alt") ,
                               FE = "country_survey_round" , IV="0", clust = "loc.id" )),
            data=ab)




## Output tables
keepvars <- c("crop_poly_sc","pubspc.23_poly_sc","terr_frac_cell_alt")
keeplabs <- c( "Cash crops USD pkm2", "Pubs pth pop (1923)", "Map-based ELF")

yss <- ab %>%
  summarize_at( "identity_more_ethnic" , funs(mean( ., na.rm =T)
                              , sd(. , na.rm =T))) %>%
  mutate_all(~round(.,4))



m.list <- list(m1,m1b,m2,m2b,m3,m3b)



add.lines <- list(
  AddLines( length(m.list), "Individual controls" , rep(LatMc("yes",1), 6) ) ,
  AddLines( length(m.list), "Historical and Geo controls" , rep(LatMc("yes",1), 6) ) ,
  AddLines( length(m.list), "FE" , rep(LatMc("Country-Round",1),6) ) ,
  AddLines( length(m.list), "Ethnic Stayer/Leaver" , c(rep(LatMc("Both",1),6) )) ,
  AddLines( length(m.list), "Mean dep. var." , rep(LatMc(yss[1],1), 6))
)

tab.name <- "ab_tableA11.tex"
fileConn<-file(paste0(tab.path,tab.name))
print(fileConn)
writeLines(stargazer(m.list,
                     float = FALSE ,
                     keep=keepvars,
                     order = keepvars ,
                     covariate.labels= keeplabs ,
                     multicolumn=F,# se = se,
                     column.sep.width="-5pt",
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
