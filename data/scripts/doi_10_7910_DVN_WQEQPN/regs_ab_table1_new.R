## prelims
rm(list = setdiff(c(ls(),lsf.str()),c("scrdir","ab")))
source("regression_functions.R")
tab.path <- "results/"

#####
#####

yvars1 <- c( "identity_more_ethnic")
yvars <- paste0(yvars1, "_sc")
cropvar <- c("crop_poly_sc")
croplab <- c( "Cash crops USD pkm2")
pubvar <- c("pubspc.23_poly_sc")
publab <- c("Pubs pth pop (1923)")

##
controls.ind <- c("sex", "age", "age2", "urban", "f.educ",  "f.food", "f.income", "f.water",  "ea_sex", "ea_age"  )
controls.geo <- c("ruggedness_nunn_puga" ,  "elevation_mean"  ,  "malaria_suit_max",
                  "TSI_CRU_mean_1901_1920" , "temperature_fao" , "precipitation_fao",  "coast_log"  ,
                  "agric_suit", "longitude", "latitude", "lpopc_hyde_meanprecol_poly")
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
m5 <-  felm(as.formula(RegFor( y = yvars , x = c( pubvar, cropvar, base.x) ,
                               FE = "country_survey_round" , IV="0", clust = "loc.id" )),
            data=ab , subset = eth.stayer == FALSE )  

m6 <-  felm(as.formula(RegFor( y = yvars , x = c( pubvar, cropvar, base.x) ,
                               FE = "country_survey_round" , IV="0", clust = "loc.id" )),
            data=ab , subset = in.soas.23_poly  == 1 )  

####### ####### ####### ####### ####### ####### ####### ####### ####### 
####### Prepare and run spatial IV

####### Aggregate to EA level and code spatial lags

#aggregate to ea
out_means_ea <- ab%>%group_by(loc.id)%>%dplyr::summarise(
  identity_more_ethnic_sc = mean(identity_more_ethnic_sc,na.rm=T),
  identity_more_ethnic = mean(identity_more_ethnic,na.rm=T),
)

out_means_ea <- out_means_ea[!is.na(out_means_ea$loc.id),]

treatments <- c("pubspc.23_poly_sc","crop_poly_sc","suit5_std")

controls_ea <- unique(ab[which(!is.na(ab$loc.id)),c("loc.id",base.x[c(11:length(base.x))],treatments,"country_survey_round","country_iso3c")])
controls_ea <- controls_ea[!duplicated(controls_ea$loc.id),]

out_means_ea <- left_join(out_means_ea,controls_ea)

#### create contiguity matrices
# prepare spatial connectivity
controls <- base.x[c(11:length(base.x))]

data.complete <- out_means_ea[which(complete.cases(out_means_ea[,c("identity_more_ethnic_sc",controls,"suit5_std")])),]

points.ab <- SpatialPoints(data.complete[,c("longitude","latitude")],
                          proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))


t0 <- Sys.time()
distm_km <- distm(points.ab)/1000
t1 <- Sys.time()
t1-t0

distm_km_100 <- distm_km
distm_km_100[distm_km_100<100] <- 1 
distm_km_100[distm_km_100>=100] <- 0

mat_100_B <- mat2listw(distm_km_100,style = "B")

### make spatial lags
data.complete <- as.data.frame(data.complete)

dat <- as.data.frame(dummy_cols(data.complete,select_columns = "country_survey_round",remove_first_dummy = T))  
du <- names(dat)[(ncol(data.complete)+1):ncol(dat)]

splags <- lapply(names(dat[,c("identity_more_ethnic_sc",controls,du)]),
                 function(n){
                   lag <- lag.listw(mat_100_B,dat[,n],zero.policy = T)
                   lag.sq <- lag.listw(mat_100_B,lag,zero.policy = T)
                   temp <- data.frame(lag, lag.sq)
                   names(temp) <- paste0(c("lag_","lag.sq_"),n)
                   return(temp)
                 })
splags <- data.frame(do.call(cbind,splags))
names(splags)
splags <- splags[,-which(names(splags)=="lag.sq_identity_more_ethnic_sc")]
dat <- cbind(dat,splags)


### join to ab data
ab.sp <- left_join(ab,dat[,c(1,which(names(dat)=="country_survey_round"):ncol(dat))])

### put together felm formula
control.str <- paste(base.x,collapse=" + ")

names(splags)
##### run spatial IV
form.str <- paste("identity_more_ethnic_sc", "~ pubspc.23_poly_sc +",control.str, " + ", paste(du, collapse = " + "))

add.felm <- paste("|", "0", "|(", names(splags)[grep("identity_more_ethnic_sc",names(splags))], "|", "crop_poly_sc", "~", 
                  paste(names(splags)[c(which(names(splags)=="lag_ruggedness_nunn_puga"):ncol(splags))], collapse = " + "), "+", "suit5_std", ")|", 
                  "loc.id")

form <- as.formula(paste(form.str,add.felm))

m.siv <- felm(form,data=ab.sp)
summary(m.siv)

# check first stage F stats
lfe::waldtest(m.siv$stage1,lhs="crop_poly_sc","suit5_std",type="cluster")["F"]
lfe::waldtest(m.siv$stage1,lhs="lag_identity_more_ethnic_sc",grep("lag",rownames(coef(m.siv$stage1))),type="cluster")["F"]


## Output tables
keepvars <- c(cropvar , "`crop_poly_sc(fit)`",  pubvar)
keeplabs <- c( croplab, "Cash Crops (S2SLS)" , publab)

yss <- ab %>%
  summarize_at( yvars1 , funs(mean( ., na.rm =T)
                              , sd(. , na.rm =T))) %>%
  mutate_all(~round(.,4))
yss5 <- ab[ab$eth.stayer==FALSE,] %>%
  summarize_at( yvars1 , funs(mean( ., na.rm =T)
                              , sd(. , na.rm =T))) %>%
  mutate_all(~round(.,4))
yss6 <- ab[ab$in.soas.23_poly==1,] %>%
  summarize_at( yvars1 , funs(mean( ., na.rm =T)
                              , sd(. , na.rm =T))) %>%
  mutate_all(~round(.,4))


####OUT table
m.list <- list(m1,m2, m3, m.siv, m5, m6)
add.lines <- list(
  AddLines( length(m.list), "Individual controls" , rep(LatMc("yes",1), 6) ) ,
  AddLines( length(m.list), "Historical and Geo controls" , rep(LatMc("yes",1), 6) ) ,
  AddLines( length(m.list), "FE" , rep(LatMc("Country-Round",1),6) ) ,
  AddLines( 4, "Ethnic Stayer/Leaver" , c(rep(LatMc("Both",1),4), LatMc("Leaver",1), LatMc("Both",1) ) ) ,
  AddLines( length(m.list), "Mean dep. var." , c(rep(LatMc(yss[1],1), 4),LatMc(yss5[1],1),LatMc(yss6[1],1)))
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
