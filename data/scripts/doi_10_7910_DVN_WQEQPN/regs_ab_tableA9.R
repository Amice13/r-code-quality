## prelims
rm(list = setdiff(c(ls(),lsf.str()),c("scrdir","ab")))
source("regression_functions.R")
tab.path <- "results/"

#####
#####
yvars<- c( "identity_more_ethnic_sc" ) ## focus on ethnic identity atm


## trim outlier

yvars1<- c("identity_more_ethnic")
yvars <- paste0(yvars1,"_sc")
cropvar <- c( "crop_lang_sc",  "crop_poly_sc")
croplab <- c( "Cash crops USD pkm2"  )
pubvar <- c( "pubspc.23_lang_sc" , "pubspc.23_poly_sc")
publab <- c( "Pubs pth pop (1923)" )


outtabname <- "ab_reg_geonlang_rbst_trust"
ab$mrdv.lang <- ab$mrdk_precol_centr_mean_lang/sd(ab$mrdk_precol_centr_mean_lang, na.rm = T)
ab$mrdv.geo <- scale(ab$mrdk_precol_centr_mean_poly)
##scale
mrdvars <-c("mrdv.geo", "mrdv.lang")
mrdlab <- c("Murdock Centralisation")

controls.ind <- c("sex", "age", "age2", "urban", "ea_sex", "ea_age"  )

controls.geo <- c("ruggedness_nunn_puga" ,  "elevation_mean"  ,  "malaria_suit_max",
"TSI_CRU_mean_1901_1920" , "temperature_fao" ,  "coast_log"  ,
"longitude", "agric_suit", "latitude", "dist.prot_log")
controls.hist <- c("explorers_log", "cities_log", "capital_log" , "dist.prot_log" , "dist.print_log"  )

base.x <-  c(controls.ind, controls.geo,controls.hist)
treat.poly <- c("crop_poly_sc", "pubspc.23_poly_sc", "mrdv.geo"  )
treat.lang <- c("crop_lang_sc", "pubspc.23_lang_sc", "mrdv.lang" )

abtoreg <- ab
base.x <-  c(controls.ind, controls.geo,controls.hist)
# controls.geo,controls.hist)

##educ
m1<- felm(as.formula(RegFor( y = yvars[1] , x = c( treat.poly,    base.x) ,
                FE = "country_survey_round" , IV="0", clust = "loc.id" )),
                data=abtoreg   )
m1.bis<- felm(as.formula(RegFor( y = yvars[1] , x = c( treat.poly,    base.x) ,
                FE = "country_survey_round" , IV="0", clust = "loc.id" )),
                data=abtoreg  , subset = in.soas.23_poly == 1 )
m2<- felm(as.formula(RegFor( y = yvars[1] , x = c( treat.lang,  controls.ind) ,
          FE = "loc.id" , IV="0", clust = "loc.id"  )),
          data=abtoreg  )
m2.bis<- felm(as.formula(RegFor( y = yvars[1] , x = c( treat.lang,  controls.ind) ,
          FE = "loc.id" , IV="0", clust = "loc.id"  )),
          data=abtoreg , subset = in.soas.23_lang == 1 )



getwd()
keepvars <- c(treat.poly, treat.lang)
keeplabs <- c(croplab[1],  publab[1], mrdlab[1], croplab[1],  publab[1],  mrdlab[1])

# means all sample
df <- subset(abtoreg, select = yvars1)
m.all <- df %>% summarise_all(mean, na.rm = T) %>% mutate_all(round, 3) %>% as.numeric()
sd.all <- df %>% summarise_all(sd, na.rm = T) %>% mutate_all(round, 3) %>% as.numeric()
# means selected sample
df <- subset(abtoreg, select = yvars1, in.soas.23_poly  == 1)
m.sel <- df %>% summarise_all(mean, na.rm = T) %>% mutate_all(round, 3) %>% as.numeric()
sd.sel <- df %>% summarise_all(sd, na.rm = T) %>% mutate_all(round, 3) %>% as.numeric()


####OUT educ
add.lines <- list( AddLines( 10, "Individual controls" , rep(LatMc("Yes",1), 8) ) ,
  AddLines( 10, "Historical and Geo controls" , rep(LatMc("No",1), 8) ) ,
  AddLines( 10, "Fixed Effect" , c(rep(LatMc("Country-Round",1), 4),  rep(LatMc("Town",1), 4) ) ) ,
  AddLines( 10, "Ethnic Stayer/Leaver" , rep(LatMc("Both",1) , 8) ),
  AddLines( 10, "Sample" , rep(c(LatMc("All",1), LatMc("In Biblio",1)), 4) )  ,
  AddLines( 10, "Mean dep. var." , rep(c(LatMc(m.all[1],1), LatMc(m.sel[1],1),
  LatMc(m.all[2],1), LatMc(m.sel[2],1)),2))
)

m.list <- list(m1, m1.bis, m2, m2.bis)
tab.name <- paste0("ab_tableA9", ".tex")
fileConn<-file(paste0(tab.path,tab.name))
print(fileConn)
writeLines(stargazer(m.list,
                     float = FALSE ,
                     keep=keepvars,
                     order = keepvars ,
                     covariate.labels= keeplabs ,
                     star.cutoffs = c(0.109, 0.0509, 0.019) ,
                     digits.extra = 1,
                     multicolumn=F,# se = se,
                     column.sep.width="10pt",
                     dep.var.caption = "",
                     dep.var.labels.include = F,
                     column.labels = c("Geographic-level", "Ethnic-level"),
                     column.separate = c(2,2),
                     font.size = "scriptsize",
                     omit.table.layout = "n",
                     align =T,
                     add.lines = add.lines,digits = 3, intercept.top = T,intercept.bottom = F,
                     keep.stat = c("rsq" , "n") ,
                     omit.stat = c("res.dev","ser") ) ,
                     fileConn)
  close(fileConn)
