  ## prelims
rm(list = setdiff(c(ls(),lsf.str()),c("scrdir","ab")))
source("regression_functions.R")
tab.path <- "results/"

#####
#####

ab$suitab <- Winsorize(ab$suit5_std, probs = c(0.00000001, 0.995), na.rm = F) ### trim suitability
yvars1 <- c( "identity_more_ethnic")
yvars <- paste0(yvars1, "_sc")
cropvar <- c("crop_poly_sc")
croplab <- c( "Cash crops USD pkm2")
pubvar <- c("pubspc.23_poly_sc")
publab <- c("In bibliographies 23" )

##
controls.ind <- c("sex", "age", "age2", "urban", "f.educ",  "f.food", "f.income", "f.water",  "ea_sex", "ea_age"  )
controls.geo <- c("ruggedness_nunn_puga" ,  "elevation_mean"  ,  "malaria_suit_max",
"TSI_CRU_mean_1901_1920" , "temperature_fao" , "precipitation_fao",  "coast_log"  ,
"latitude", "agric_suit", "latitude", "lpopc_hyde_meanprecol_poly", "larea_sqkm_mean_lang")
controls.hist <- c("explorers_log", "cities_log", "capital_log" , "dist.prot_log" , "dist.print_log"  )

base.x <-  c(controls.ind, controls.geo,controls.hist)


m1 <- felm(as.formula(RegFor( y = yvars , x = c(cropvar, base.x) ,
      FE = "country_survey_round" , IV="0", clust = "loc.id" )),
           data=ab )
m2 <- felm(as.formula(RegFor( y = yvars , x = c( pubvar, base.x) ,
      FE = "country_survey_round " , IV="0", clust = "loc.id" )),
     data=ab )
m3 <- felm(as.formula(RegFor( y = yvars , x = c( pubvar, cropvar, base.x) ,
      FE = "country_survey_round" , IV="0", clust = "loc.id" )),
      data=ab )
m4 <- felm(as.formula(RegFor( y = yvars , x = c( pubvar, "suit5_std", base.x) ,
                FE = "country_survey_round" , IV="0", clust = "loc.id" )),
                data=ab )
m5 <-  felm(as.formula(RegFor( y = yvars , x = c( pubvar, cropvar, base.x) ,
      FE = "country_survey_round" , IV="0", clust = "loc.id" )),
      data=ab , subset = eth.stayer == FALSE )  #

## Output tables
getwd()
  keepvars <- c(cropvar , "suit5_std",  pubvar)
  keeplabs <- c( croplab, "Cash Crop Suitability" , publab)

  yss <- ab %>%
      summarize_at( yvars1 , funs(mean( ., na.rm =T)
      , sd(. , na.rm =T))) %>%
      mutate_all(~round(.,4))


  ####OUT table
  m.list <- list(m1,m2, m3, m4, m5)
  add.lines <- list(
    AddLines( length(m.list), "Individual controls" , rep(LatMc("yes",1), 5) ) ,
    AddLines( length(m.list), "Historical and Geo controls" , rep(LatMc("yes",1), 5) ) ,
    AddLines( length(m.list), "FE" , rep(LatMc("Country-Round",1),5) ) ,
    AddLines( 4, "Ethnic Stayer/Leaver" , c(rep(LatMc("Both",1),4), LatMc("Leaver",1) ) ) ,
    AddLines( length(m.list), "Mean dep. var." , rep(LatMc(yss[1],1), 5))
   )

  tab.name <- paste0("ab_table1", ".tex")
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
