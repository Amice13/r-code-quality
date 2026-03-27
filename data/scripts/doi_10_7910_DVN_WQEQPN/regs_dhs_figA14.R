### Addressing Endogeneity: Baseline geographic model

source("regression_functions.R")
fig.path <- "results/"



### define outcomes and control variables
yvars<- paste0("exogamy_l",c(1:11,15:16)) ## 
yvars_std <- paste0(yvars,"_std")
controls.geo <- c("ruggedness_nunn_puga" ,  "elevation"  ,  "malaria_suit_max",
                  "TSI_CRU_mean_1901_1920" , "temperature_fao" , "precipitation_fao",  "coast_log"  ,
                  "LONGNUM", "agric_suit", "LATNUM")
controls.hist <- c("explorers_log", "cities_log", "capital_log","dist.prot_log" , "dist.print_log","popc_mean_1720_1890_log")
controls.ind <- c("age","age.sq","age.m","age.m.sq")
base.x <-  c(controls.ind, controls.geo,controls.hist)

### define analysis countries
sample_countries_hance <- unique(dhs$iso3c[dhs$sample_country_hance==1])


####### Aggregate outcomes to EA level and code spatial lags
#aggregate to ea
exogamy_means_ea <- dhs%>%group_by(loc.id_pubs_poly)%>%dplyr::summarise(
  exogamy_l1_std = mean(exogamy_l1_std,na.rm=T),
  exogamy_l2_std = mean(exogamy_l2_std,na.rm=T),
  exogamy_l3_std = mean(exogamy_l3_std,na.rm=T),
  exogamy_l4_std = mean(exogamy_l4_std,na.rm=T),
  exogamy_l5_std = mean(exogamy_l5_std,na.rm=T),
  exogamy_l6_std = mean(exogamy_l6_std,na.rm=T),
  exogamy_l7_std = mean(exogamy_l7_std,na.rm=T),
  exogamy_l8_std = mean(exogamy_l8_std,na.rm=T),
  exogamy_l9_std = mean(exogamy_l9_std,na.rm=T),
  exogamy_l10_std = mean(exogamy_l10_std,na.rm=T),
  exogamy_l11_std = mean(exogamy_l11_std,na.rm=T),
  exogamy_l15_std = mean(exogamy_l15_std,na.rm=T),
  exogamy_l16_std = mean(exogamy_l16_std,na.rm=T),
)

exogamy_means_ea <- exogamy_means_ea[!is.na(exogamy_means_ea$loc.id_pubs_poly),]

## add geographic treatment and control variables
treatments <- c("pubspc.23_poly_std","hance_crops5_sum_15km_std","suit5_std")
controls_ea <- unique(dhs[which(!is.na(dhs$loc.id_pubs_poly)),c("loc.id_pubs_poly",base.x[c(5:length(base.x))],treatments,"country_survey_round","iso3c")])
exogamy_means_ea <- left_join(exogamy_means_ea,controls_ea)




#### create contiguity matrices
# prepare spatial connectivity
controls <- base.x[5:length(base.x)]

# subset to non-missing obs
data.complete <- exogamy_means_ea[which(complete.cases(exogamy_means_ea[,c("exogamy_l1_std",controls,"suit5_std")])),]

points.dhs <- SpatialPoints(data.complete[,c("LONGNUM","LATNUM")],
                            proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

distm_km <- distm(points.dhs)/1000 # takes about 5 mins

distm_km_100 <- distm_km
distm_km_100[distm_km_100<100] <- 1 
distm_km_100[distm_km_100>=100] <- 0

mat_100_B <- mat2listw(distm_km_100,style = "B")


######### ######### ######### ######### ######### ######### ######### ######### 
######### prepare spatial lags for individual-level spatial IVs

# add fixed effect dummies to ea-level dataset
dat <- as.data.frame(dummy_cols(data.complete,select_columns = "country_survey_round",remove_first_dummy = T))  
du <- names(dat)[(ncol(data.complete)+1):ncol(dat)]

# build spatial lags of all baseline controls and fixed effects dummies (first and 2nd degree)
splags <- lapply(names(dat[,c(yvars_std,controls,du,"pubspc.23_poly_std")]),
                 function(n){
                   print(n)
                   lag <- lag.listw(mat_100_B,dat[,n],zero.policy = T)
                   lag.sq <- lag.listw(mat_100_B,lag,zero.policy = T)
                   temp <- data.frame(lag, lag.sq)
                   names(temp) <- paste0(c("lag_","lag.sq_"),n)
                   return(temp)
                 })
splags <- data.frame(do.call(cbind,splags))

# add to ea-level data.frame
dat <- cbind(dat,splags)

### join to dhs data
names(dat)
dhs.sp <- left_join(dhs,dat[,c(1,which(names(dat)==du[1]):ncol(dat))])


################# Run Spatial IV model by hand

### put together control variable string
control.str <- paste(base.x,collapse=" + ")


##### run spatial IVs: takes about 12 minutes to compute
t0 <- Sys.time()
m.list.siv <- lapply(yvars_std, function(dv){
  form.str <- paste(dv, "~ pubspc.23_poly_std +",control.str, " + ", paste(du, collapse = " + "))
  
  add.felm <- paste("|", "0", "|(", names(splags)[grep(dv,names(splags))], "|", "hance_crops5_sum_15km_std", "~", 
                    paste(names(splags)[which(names(splags)=="lag_ruggedness_nunn_puga"):(ncol(splags)-2)], collapse = " + "), "+", "suit5_std", ")|", 
                    "loc.id_pubs_poly")
  
  form <- as.formula(paste(form.str,add.felm))
  
  m.siv <- felm(form,data=dhs.sp,subset=iso3c%in%sample_countries_hance)
  print(dv)
  return(m.siv)
})
t1 <- Sys.time()
t1-t0 


####### Reduced Form
m.list.rf <- lapply(yvars_std, function(dv){
  mdlr_dummy_plus(y=dv, x="suit5_std",cntr=c("pubspc.23_poly_std",base.x),
                  cl="loc.id_pubs_poly",fe="country_survey_round",iv=NULL,
                  dat=dhs[dhs$iso3c%in%sample_countries_hance,])
})

###### OLS
m.list.ols <- lapply(yvars_std, function(dv){
  mdlr_dummy_plus(y=dv, x="hance_crops5_sum_15km_std",cntr=c("pubspc.23_poly_std",base.x),
                  cl="loc.id_pubs_poly",fe="country_survey_round",iv=NULL,
                  dat=dhs[dhs$iso3c%in%sample_countries_hance,])
})


###### OLS, spatially lagged: 8-9 min
t0 <- Sys.time()
m.list.ols.sl <- lapply(yvars_std, function(dv){
  form.str <- paste(dv, "~ pubspc.23_poly_std +",control.str, " + ", paste(du, collapse = " + "),"+ hance_crops5_sum_15km_std")
  
  add.felm <- paste("|", "0", "|(", names(splags)[grep(dv,names(splags))], "~", 
                    paste(names(splags)[which(names(splags)=="lag_ruggedness_nunn_puga"):(ncol(splags)-2)], collapse = " + "), ")|", 
                    "loc.id_pubs_poly")
  
  form <- as.formula(paste(form.str,add.felm))
  
  m.siv <- felm(form,data=dhs.sp,subset=LONGNUM!=0 & iso3c%in%sample_countries_hance)
  print(dv)
  return(m.siv)
})
t1 <- Sys.time()
t1-t0



######### ######### ######### ######### ######### ######### ######### ######### 
#### Results Output
#### combined:
#### prepare regression results for visual output
ticks <- rev(seq(19.5,1,-1.5))

n.coef <- length(coef(m.list.siv[[13]]))
out.ls <- lapply(m.list.siv, function(x){
  get_plot_data(x,coef_no=c(n.coef))
})
out.df.iv <- do.call(rbind,out.ls)
out.df.iv$dv <- c(paste0("Exogamy L",c(1:10)),paste0("Exogamy L","11-14"),paste0("Exogamy L",c(15:16)))
ord. <- ticks - 0.3
out.df.iv$order <- ord.[order(ord.,decreasing=T)]
out.df.iv$what <- rep(c("Cash Crops (S2SLS)"),13)


n.coef <- length(coef(m.list.ols.sl[[13]]))-1
out.ls <- lapply(m.list.ols.sl, function(x){
  get_plot_data(x,coef_no=c(n.coef))
})
out.df.sl <- do.call(rbind,out.ls)
out.df.sl$dv <- c(paste0("Exogamy L",c(1:10)),paste0("Exogamy L","11-14"),paste0("Exogamy L",c(15:16)))
ord. <- ticks + 0.1
out.df.sl$order <- ord.[order(ord.,decreasing=T)]
out.df.sl$what <- rep(c("Cash Crops (SAR)"),13)


n.coef <- length(coef(m.list.rf[[13]]))
out.ls <- lapply(m.list.rf, function(x){
  get_plot_data(x,coef_no=c(n.coef))
})
out.df.rf <- do.call(rbind,out.ls)
out.df.rf$dv <- c(paste0("Exogamy L",c(1:10)),paste0("Exogamy L","11-14"),paste0("Exogamy L",c(15:16)))
ord. <- ticks-0.1
out.df.rf$order <- ord.[order(ord.,decreasing=T)]
out.df.rf$what <- rep(c("Cash Crop Suit. (RF)"),13)

n.coef <- length(coef(m.list.ols[[13]]))
out.ls <- lapply(m.list.ols, function(x){
  get_plot_data(x,coef_no=c(n.coef))
})
out.df.ols <- do.call(rbind,out.ls)
out.df.ols$dv <- c(paste0("Exogamy L",c(1:10)),paste0("Exogamy L","11-14"),paste0("Exogamy L",c(15:16)))
ord. <- ticks+0.3
out.df.ols$order <- ord.[order(ord.,decreasing=T)]
out.df.ols$what <- rep(c("Cash Crops (OLS)"),13)


out.df <- rbind(out.df.ols,out.df.sl,out.df.rf,out.df.iv)



# define colors
col1 <- wes_palette("Darjeeling1",5,"discrete")[2]
col2 <- wes_palette("Darjeeling1",5,"discrete")[3]
col3 <- wes_palette("Darjeeling1",5,"discrete")[5]
col4 <- wes_palette("Darjeeling1",15,"continuous")[2] 



out.df$what <- factor(out.df$what,levels=c("Cash Crops (OLS)","Cash Crops (SAR)","Cash Crop Suit. (RF)","Cash Crops (S2SLS)"))



# do the plot
p1 <- ggplot(out.df)
p1 <- p1 + geom_point(size=2.25,aes(x=beta,y=order,color=what,shape=what,fill=what)) + 
  geom_errorbarh(aes(x=beta,y=order, xmin=lb, xmax=ub,color=what), size=1, height=0.0) +
  geom_vline(xintercept=0,linetype="dotted", size=0.6) +
  scale_y_continuous(breaks=rev(ticks),minor_breaks = NULL, 
                     labels=c(paste0("Exogamy L",c(1:10)),"Exogamy L11-14",paste0("Exogamy L",c(15:16)))) + 
  #scale_x_continuous(minor_breaks = seq(0 , 0.55, 0.05), breaks = seq(0, 0.55, 0.1)) +
  labs(x = "Coefficients and 95% Confidence Intervals", y = "Exogamy at Ethnologue Levels",
       title="Cash Crops & Inter-Ethnic Marriages",
       subtitle="Treatment defined geographically"
       #,caption="Cash crop value per sqkm in 1960 USD (Poly.)\nCash Crops (SAR)\nCash Crop Suitability (Poly)\nCash Crops (Instrumented with Suitability)"
  ) +
  theme_minimal(base_size=14) +  
  theme(axis.text.y = element_text(size=10),legend.position = "bottom") +
  scale_color_manual(values=c("Cash Crops (OLS)" = col1, "Cash Crops (SAR)" = col2, "Cash Crop Suit. (RF)" =col3, "Cash Crops (S2SLS)" =col4),name="Treatment/Model") + 
  scale_fill_manual(values=c("Cash Crops (OLS)" = col1, "Cash Crops (SAR)" = col2, "Cash Crop Suit. (RF)" =col3, "Cash Crops (S2SLS)" =col4),name="Treatment/Model") +
  scale_shape_manual(values=c("Cash Crops (OLS)" = 24, "Cash Crops (SAR)" = 22, "Cash Crop Suit. (RF)" = 23, "Cash Crops (S2SLS)" =25),name="Treatment/Model")
p1 <- p1 + guides(color=guide_legend(ncol=2,nrow=2,byrow=TRUE),
                fill=guide_legend(ncol=2,nrow=2,byrow=TRUE),
                shape=guide_legend(ncol=2,nrow=2,byrow=TRUE))
p1





#### get spatial lag and cash crop coef for further analysis
n.coef <- length(coef(m.list.siv[[13]]))
out.ls <- lapply(m.list.siv, function(x){
  get_plot_data(x,coef_no=c(n.coef,n.coef-1))
})
out.df.iv <- do.call(rbind,out.ls)
out.df.iv$dv <- rep(c(paste0("Exogamy L",c(1:10)),paste0("Exogamy L","11-14"),paste0("Exogamy L",c(15:16))),each=2)
out.df.iv$what <- rep(c("Cash Crops (S2SLS)","Rho"),13)


n.coef <- length(coef(m.list.ols.sl[[13]]))
out.ls <- lapply(m.list.ols.sl, function(x){
  get_plot_data(x,coef_no=c(n.coef-1,n.coef))
})
out.df.sl <- do.call(rbind,out.ls)
out.df.sl$dv <- rep(c(paste0("Exogamy L",c(1:10)),paste0("Exogamy L","11-14"),paste0("Exogamy L",c(15:16))),each=2)
out.df.sl$what <- rep(c("Cash Crops (SAR)","Rho"),13)

out.df.impacts <- rbind(out.df.iv,out.df.sl)
saveRDS(out.df.impacts,"dhs/dhs_spatial_coefs.rds")


### #### #### #### #### #### #### #### #### #### #### #### 
#### #### Geographic Treatment: Cash crops within 15km, 1923 pubs in polygon, Intensive margin
#### #### #### #### #### #### #### #### #### #### #### #### 
source("regression_functions.R")
fig.path <- "results/"



### define outcomes and control variables
yvars<- paste0("exogamy_l",c(1:11,15:16)) ## 
yvars_std <- paste0(yvars,"_std")
controls.geo <- c("ruggedness_nunn_puga" ,  "elevation"  ,  "malaria_suit_max",
                  "TSI_CRU_mean_1901_1920" , "temperature_fao" , "precipitation_fao",  "coast_log"  ,
                  "LONGNUM", "agric_suit", "LATNUM")
controls.hist <- c("explorers_log", "cities_log", "capital_log","dist.prot_log" , "dist.print_log","popc_mean_1720_1890_log")
controls.ind <- c("age","age.sq","age.m","age.m.sq")
base.x <-  c(controls.ind, controls.geo,controls.hist)

### define analysis countries
sample_countries_hance <- unique(dhs$iso3c[dhs$sample_country_hance==1])


m.list <- lapply(yvars_std, function(dv){
  felm(as.formula(RegFor( y = dv , x = c("hance_crops5_sum_15km_std","pubspc.23_poly_std", base.x) ,
                          FE = "country_survey_round" , 
                          IV="0", clust = "loc.id_pubs_poly")),
       data=dhs,subset=iso3c%in%sample_countries_hance & pubspc.23_poly!=0)
})



#### prepare regression results for visual output
out.ls <- lapply(m.list, function(x){
  get_plot_data(x,coef_no=c(1,2))
})
out.df <- do.call(rbind,out.ls)
out.df$dv <- c(rep(paste0("Exogamy L",c(1:10)),each=2),rep(paste0("Exogamy L","11-14"),each=2),rep(paste0("Exogamy L",c(15:16)),each=2))
ord. <- c(c(13:1)+0.2,c(13:1)-0.2)
out.df$order <- ord.[order(ord.,decreasing=T)]
out.df$what <- factor(rep(c("Cash Crops","Publications"),13))

col1 <- wes_palette("Darjeeling1",5,"discrete")[2]
col2 <- wes_palette("Darjeeling1",5,"discrete")[3]



# do the plots
p2 <- ggplot(out.df)
p2 <- p2 + geom_point(size=2.75,aes(x=beta,y=order,color=what,shape=what,fill=what)) + 
  geom_errorbarh(aes(x=beta,y=order, xmin=lb, xmax=ub,color=what), size=1, height=0.0) +
  geom_vline(xintercept=0,linetype="dotted", size=0.6) +
  scale_y_continuous(breaks=c(13:1),minor_breaks = NULL, 
                     labels=c(paste0("Exogamy L",c(1:10)),"Exogamy L11-14",paste0("Exogamy L",c(15:16))),
                     limits = c(0.6,13.4)) + 
  #scale_x_continuous(minor_breaks = seq(0 , 0.55, 0.05), breaks = seq(0, 0.55, 0.1)) +
  labs(x = "Coefficients and 95% Confidence Intervals", y = "Exogamy at Ethnologue Levels",
       title="Cash Crops, Publications & Inter-Ethnic Marriages",
       subtitle="Treatment defined geographically",
       caption="Cash crop value per sqkm in 1960 USD (within 15km)\n1923 Publications per capita (WLMS Polygon)") +
  theme_minimal(base_size = 14) +
  theme(axis.text.y = element_text(size=10),legend.position = "bottom") +
  scale_color_manual(values=c("Cash Crops" = col1, "Publications" = col2),name="Treatment") + 
  scale_fill_manual(values=c("Cash Crops" = col1, "Publications" = col2),name="Treatment") +
  scale_shape_manual(values=c("Cash Crops" = 24, "Publications" = 25),name="Treatment")
p2

# combine with patchwork
p.out <- p1 | p2
#save
p.out + ggsave(paste0(fig.path,"dhs_figA14.pdf"),width=16,height=9)
