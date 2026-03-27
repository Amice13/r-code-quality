## Functions
source("regression_functions.R")
tab.path <- "results/"

#####
#####
yvars<- c( "identity_more_ethnic_sc" ) ## focus on ethnic identity atm
yvars1<- c( "trust_coethnics_yn",  "trust_general" , "trust_ethnic_premium2" )
yvars <- paste0(yvars1,"_sc")
cropvar <- c( "crop_lang_sc",  "crop_poly_sc")
croplab <- c( "Cash crops USD pkm2"  )
pubvar <- c( "pubspc.23_lang_sc" , "pubspc.23_poly_sc")
publab <- c( "Pubs pth pop (1923)" )

int <- apply(ab[,c(cropvar, pubvar)] , 2 , function(x) x * ab$eth.stayer)  %>% data.frame()
names(int) <- paste0("int." , c(cropvar,pubvar))

treat.poly <- c("crop_poly_sc", "pubspc.23_poly_sc"  )
treat.lang <- c("crop_lang_sc", "pubspc.23_lang_sc" )

tot.poly <- c(treat.poly, paste0("int.", treat.poly), "eth.stayer")
tot.lang <- c(treat.lang, paste0("int." ,treat.lang), "eth.stayer")

abtoreg  <- cbind(ab, int)
outtabname <- "ab_reg_geonlang_rbst_trust"

controls.ind <- c("sex", "age", "age2", "urban", "f.educ",
    "ea_sex", "ea_age"  )
controls.geo <- c("ruggedness_nunn_puga" ,  "elevation_mean"  ,  "malaria_suit_max",
"TSI_CRU_mean_1901_1920" , "temperature_fao" ,  "coast_log"  ,
"longitude", "agric_suit", "latitude", "lpopc_hyde_meanprecol_poly")
controls.hist <- c("explorers_log", "cities_log", "capital_log" , "dist.prot_log" , "dist.print_log"  )

base.x <-  c(controls.ind, controls.geo, controls.hist)

# controls.geo,controls.hist)

m1<- felm(as.formula(RegFor( y = yvars[1] , x = c( treat.poly,  base.x) ,
                FE = "country_survey_round" , IV="0", clust = "loc.id" )),
                data=abtoreg   )
m2<- felm(as.formula(RegFor( y = yvars[2] , x = c( treat.poly,   base.x) ,
            FE = "country_survey_round", IV="0", clust = "loc.id" )),
            data=abtoreg  )
m1.prem<- felm(as.formula(RegFor( y = yvars[3] , x = c( treat.poly,  base.x) ,
                FE = "country_survey_round" , IV="0", clust = "loc.id" )),
                data=abtoreg   )


m3<- felm(as.formula(RegFor( y = yvars[1] , x = c( treat.lang,  controls.ind, "lpopc_hyde_meanprecol_lang") ,
          FE = "loc.id" , IV="0", clust = "loc.id"  )),
          data=abtoreg  )
m4<- felm(as.formula(RegFor( y = yvars[2] , x = c( treat.lang,   controls.ind, "lpopc_hyde_meanprecol_lang") ,
      FE = "loc.id", IV="0", clust = "loc.id" )),
      data=abtoreg )
m2.prem<- felm(as.formula(RegFor( y = yvars[3] , x = c( treat.lang,  controls.ind, "lpopc_hyde_meanprecol_lang") ,
          FE = "loc.id" , IV="0", clust = "loc.id"  )),
          data=abtoreg  )

getwd()
keepvars <- c(treat.poly, treat.lang )
keeplabs <- c(croplab[1],  publab[1], croplab[1],  publab[1])


df <- subset(abtoreg, select = yvars1)
m.all <- df %>% summarise_all(mean, na.rm = T) %>% mutate_all(round, 3) %>% as.numeric()
sd.all <- df %>% summarise_all(sd, na.rm = T) %>% mutate_all(round, 3) %>% as.numeric()

####OUT
add.lines <- list( AddLines( 10, "Individual controls" , rep(LatMc("Yes",1), 6) ) ,
  AddLines( 10, "Historical and Geo controls" , rep(LatMc("No",1), 6) ) ,
  AddLines( 10, "Fixed Effect" , c(rep(LatMc("Country-Round",1), 3),  rep(LatMc("Town",1), 3) ) ) ,
  AddLines( 10, "Ethnic Stayer/Leaver" , rep(LatMc("Both",1) , 6) ) ,
  AddLines( 10, "Mean dep. var." , rep(m.all , 2) )
 )

m.list <- list(m1,m2, m1.prem, m3,m4, m2.prem)


tab.name <- paste0("ab_tableA14", ".tex")
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
                     dep.var.caption = "Trust",
                     dep.var.labels.include = F,
                     column.labels = c("Geographic-Level", "Ethnic-Level"),
                     column.separate = c(3,3),
                     font.size = "scriptsize",
                     omit.table.layout = "n",
                     align =T,
                     add.lines = add.lines,digits = 3, intercept.top = T,intercept.bottom = F,
                     keep.stat = c("rsq" , "n") ,
                     omit.stat = c("res.dev","ser") ),
                     fileConn)
  close(fileConn)
