## prelims
rm(list = setdiff(c(ls(),lsf.str()),c("scrdir","ab")))
source("regression_functions.R")
tab.path <- "results/"

#####
#####

yvars1 <- c( "identity_more_ethnic")
yvars <- paste0(yvars1, "_sc")

geovar <- c("crop_poly_sc", "pubspc.soas_poly_sc")
langvar <- c("crop_lang_sc", "pubspc.soas_lang_sc" )
labs <- c("Cash crops USD pkm2 ", "Pubs pth today (Mann and Sanders)")

##

controls.ind <- c("sex", "age", "age2", "urban", "f.educ",  "f.food", "f.income", "f.water",  "ea_sex", "ea_age"  )
controls.geo <- c("ruggedness_nunn_puga" ,  "elevation_mean"  ,  "malaria_suit_max",
"TSI_CRU_mean_1901_1920" , "temperature_fao" , "precipitation_fao",  "coast_log"  ,
"longitude", "agric_suit", "latitude")
controls.hist <- c("explorers_log", "cities_log", "capital_log" , "dist.prot_log" , "dist.print_log"  )

i <- 1
base.x1 <- c(controls.ind,"lpopc_hyde_meanprecol_lang")
base.x2 <- c(controls.ind, controls.geo, controls.hist, "lpopc_hyde_meanprecol_poly")
# controls.geo,controls.hist)



m1 <- felm(as.formula(RegFor( y = yvars[i] , x = c( geovar, base.x2) ,
          FE = "country_survey_round" , IV="0", clust = "loc.id" )),
          data=ab  )
m2 <- felm(as.formula(RegFor( y = yvars[i] , x = c( geovar, base.x2) ,
          FE = " country_survey_round" , IV="0", clust = "loc.id" )),
          data=ab , subset = in.soas.23_poly  == 1   )

m3 <- felm(as.formula(RegFor( y = yvars[i] , x = c( langvar, base.x1) ,
                      FE = "loc.id" , IV="0", clust = "loc.id" )),
                      data=ab  )

m4<- felm(as.formula(RegFor( y = yvars[i] , x = c( langvar, base.x1) ,
                      FE = "loc.id" , IV="0", clust = "loc.id" )),
                      data=ab , subset =  in.soas.23_poly  == 1   )


## Output tables
    getwd()
    keepvars <- c(geovar, langvar)
    keeplabs <- c( labs , labs)

m.all <- round(mean(ab[,yvars1[i]], na.rm = TRUE), 4)
sd.all <- round(sd(ab[,yvars1[i]], na.rm = TRUE), 4)

yss <- ab %>%
        filter(in.soas.23_poly  == 1 ) %>%
        summarize_at( yvars1[i] , funs(mean( ., na.rm =T)
        , sd(. , na.rm =T))) %>%
        mutate_all(~round(.,2))
yss2 <- ab %>%
        filter(in.soas.23_poly  == 1 & missions_yn_max_lang == 1 ) %>%
        summarize_at( yvars1[i] , funs(mean( ., na.rm =T)
        , sd(. , na.rm =T))) %>%
        mutate_all(~round(.,2))

    ####OUT
    add.lines <- list( AddLines( 4, "Individual controls" , rep(LatMc("Yes",1), 4) ) ,
      AddLines( 4, "Historical and Geo controls" , c(rep(LatMc("No",1), 2) , rep(LatMc("Yes",1), 2 ) ) ) ,
      AddLines( 4, "Fixed Effect" , c(rep(LatMc("Country-Round",1),2) , rep(LatMc("Town",1),2) )) ,
      AddLines( 4, "Sample" , rep( c(LatMc("All",1), LatMc("In Biblio",1)), 2)  ) ,
      AddLines( 4, "Mean dep. var." , rep(c(LatMc(m.all,1), LatMc(yss[i],1)), 2) )
     )

    m.list <- list(m1,m2, m3, m4 )

    tab.name <- paste0("ab_tableA8", ".tex")
    fileConn<-file(paste0(tab.path,tab.name))
    print(fileConn)
    writeLines(stargazer(m.list,
                         float = FALSE ,
                         keep=keepvars,
                         order = keepvars ,
                         covariate.labels= keeplabs ,
                         star.cutoffs = c(0.109, 0.0509, 0.019) ,
                         digits.extra = 0,
                         multicolumn=F,# se = se,
                         column.sep.width="10pt",
                         dep.var.caption = "",
                         dep.var.labels.include = F,
                         column.labels = c("Geograhic-level", "Ethnic-level"),
                         column.separate = c(2,2),
                         font.size = "scriptsize",
                         omit.table.layout = "n",
                         align =T,
                         add.lines = add.lines,digits = 3, intercept.top = T,intercept.bottom = F,
                         keep.stat = c("rsq" , "n") ,
                         omit.stat = c("res.dev","ser") ),
                         fileConn)
  close(fileConn)
