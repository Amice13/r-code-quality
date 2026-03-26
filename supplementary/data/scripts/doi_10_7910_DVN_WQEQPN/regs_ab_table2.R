## prelims
rm(list = setdiff(c(ls(),lsf.str()),c("scrdir","ab")))
source("regression_functions.R")
tab.path <- "results/"

#####

yvars1 <- c( "identity_more_ethnic")
yvars <- paste0(yvars1, "_sc")
cropvar <- c( "crop_lang_sc")
croplab <- c( "Cash crops USD pkm2")
pubvar <- c("pubspc.23_lang_sc")
publab <- c("Pubs pth pop (1923)", "Pubs pth today pop SOAS", "Language in SOAS or Row23", "In Row 23")

controls.ind <- c("sex", "age", "age2" , "f.educ", "f.water",
 "f.food", "f.income", "f.health",  "ea_sex", "ea_age"  )

k <- 1
j <- 1
i <- 1

base.x <- c(controls.ind, "lpopc_hyde_meanprecol_lang")

m1 <- felm(as.formula(RegFor( y = yvars[i] , x = c(cropvar[k], base.x) ,
              FE = "loc.id" , IV="0", clust = "loc.id" )),
           data=ab  )

m2<- felm(as.formula(RegFor( y = yvars[i] , x = c( pubvar[j], base.x) ,
              FE = "loc.id" , IV="0", clust = "loc.id" )),
           data=ab  )

m3<- felm(as.formula(RegFor( y = yvars[i] , x = c( cropvar[k], pubvar[j], base.x) ,
              FE = "loc.id" , IV="0", clust = "loc.id" )),
          data=ab  )

m4<- felm(as.formula(RegFor( y = yvars[i] , x = c( cropvar[k], pubvar[j], base.x) ,
          FE = "loc.id" , IV="0", clust = "loc.id" )),
          data=ab , subset = eth.stayer == FALSE )


m5<- felm(as.formula(RegFor( y = yvars[i] , x = c( cropvar[k], pubvar[j], base.x) ,
          FE = "loc.id" , IV="0", clust = "loc.id" )),
            data=ab , subset =  eth.stayer == FALSE & in.soas.23_lang  == 1  )

getwd()
keepvars <- c(cropvar[k] , pubvar[j])
keeplabs <- c( croplab[k] , publab[j] )

m.all <- round(mean(ab[,yvars1[i]], na.rm = TRUE), 4)
sd.all <- round(sd(ab[,yvars1[i]], na.rm = TRUE), 4)

yss <- ab %>%
        filter(eth.stayer == FALSE ) %>%
        summarize_at( yvars1[i] , funs(mean( ., na.rm =T)
        , sd(. , na.rm =T))) %>%
        mutate_all(~round(.,2))
yss2 <- ab %>%
        filter(eth.stayer == FALSE & missions_yn_max_lang == 1 ) %>%
        summarize_at( yvars1[i] , funs(mean( ., na.rm =T)
        , sd(. , na.rm =T))) %>%
        mutate_all(~round(.,2))

####OUT
add.lines <- list( AddLines( 4, "Individual controls" , rep(LatMc("Yes",1), 5) ) ,
  AddLines( 4, "Historical and Geo controls" , rep(LatMc("No",1), 5) ) ,
  AddLines( 4, "Fixed Effect" , rep(LatMc("Town",1),5) ) ,
  AddLines( 4, "Ethnic Stayer/Leaver" , c(rep(LatMc("Both",1),3), LatMc("Leaver",1), LatMc("Leaver",1) ) ) ,
  AddLines( 4, "Mean dep. var." , c(rep(LatMc(m.all,1), 3), LatMc(yss[i],1), LatMc(yss2[i],1)) )
 )

m.list <- list(m1,m2, m3,m4, m5 )

tab.name <- paste0("ab_table2", ".tex")
fileConn<-file(paste0(tab.path,tab.name))
print(fileConn)
writeLines(stargazer(m.list,
                     float = FALSE ,
                     keep=keepvars,
                     order = keepvars ,
                     covariate.labels= keeplabs ,
                     star.cutoffs = c(0.11, 0.0509, 0.019) ,
                     digits.extra = 0,
                     multicolumn=F,# se = se,
                     column.sep.width="10pt",
                     dep.var.caption = paste("Ethnic-level - " , "Ethnic vs National Id" ),
                     dep.var.labels.include = F,
                     column.labels = c("All", "In Biblio"),
                     column.separate = c(4,1),
                     font.size = "scriptsize",
                     omit.table.layout = "n",
                     align =T,
                     add.lines = add.lines,digits = 3, intercept.top = T,intercept.bottom = F,
                     keep.stat = c("rsq" , "n") ,
                     omit.stat = c("res.dev","ser") ),
                     fileConn)
  close(fileConn)
