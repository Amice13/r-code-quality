## load regression functions and set output folder
source("regression_functions.R")
tab.path <- "results/"
fig.path <- "results/"

## code treatment and control variables
wlms_data$pub23_dummy <- ifelse(wlms_data$in.chr23=="TRUE",1,0)
wlms_data$pub_soas_dummy <- ifelse(wlms_data$in.soas=="TRUE",1,0)
wlms_data$pub_both_dummy <- ifelse(wlms_data$in.soas.23=="TRUE",1,0)

# code mean population 1720-1800
wlms_data$popc_mean_1720_1890 <- apply(wlms_data[,paste0("popc_",seq(1720,1890,10),"AD")],1,mean)
wlms_data$popc_mean_1720_1890_log <- log(1+wlms_data$popc_mean_1720_1890)

# standardize FAO suitability instrument to mean 0 and sd 1
wlms_data$suit5_std <- scale(wlms_data$suit_low_mean5)[,1]

# log distance to missions and printing presses
wlms_data$dist.prot_log <- log(1+wlms_data$dist_mission)
wlms_data$dist.print_log <- log(1+wlms_data$dist_mission_print)


# define control variables, fixed effects, and se clustering
controls.geo <-c("TSI_CRU_mean_1901_1920", "malaria_suit_max", "elevation_mean",
                 "ruggedness_nunn_puga", "rivers_log", "precipitation_fao", "temperature_fao","coast_log" ,
                 "agric_suit","Longitude","Latitude")
controls.hist <- c("explorers_log", "cities_log", "capital_log" , "dist.prot_log" , "dist.print_log")
add.felm <- ("| iso3c | 0 | iso3c") # country fixed effects and clustered se (maybe use Conley errors instead)
base.x <-  c(controls.geo,controls.hist)


### get right subsets of data
hance_countries <- unique(wlms_data$iso3c[wlms_data$hance_sum!=0])
epr_countries <- unique(wlms_data$iso3c[wlms_data$epr_country==1])
preg_countries <- unique(wlms_data$iso3c[wlms_data$preg_country==1])
hance_epr_countries <- hance_countries[which(hance_countries%in%epr_countries)]
hance_preg_countries <- hance_countries[which(hance_countries%in%preg_countries)]


wlms_data_epr <- wlms_data[wlms_data$iso3c%in%hance_epr_countries,]
wlms_data_epr <- wlms_data_epr[complete.cases(wlms_data_epr[,c("epr_link",base.x)]),]

wlms_data_preg <- wlms_data[wlms_data$iso3c%in%hance_preg_countries,]
wlms_data_preg <- wlms_data_preg[complete.cases(wlms_data_preg[,c("preg_link",base.x)]),]


#### prepare spatial connectivity based on distance cutoff at 100 km
# this will be needed for the spatial 2SLS models further below

## make spatial points from wlms polygon centroids
points.wlms <- SpatialPoints(wlms_data_epr[,c("Longitude","Latitude")],
                             proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))


### get correct distance matrix
distm_km <- distm(points.wlms)/1000

distm_km_100 <- distm_km
distm_km_100[distm_km_100<100] <- 1 
distm_km_100[distm_km_100>=100] <- 0

# transform into listw format
mat_100_B_epr <- mat2listw(distm_km_100,style = "B")



##### same for PREG
points.wlms <- SpatialPoints(wlms_data_preg[,c("Longitude","Latitude")],
                             proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))


### get correct distance matrix
distm_km <- distm(points.wlms)/1000

distm_km_100 <- distm_km
distm_km_100[distm_km_100<100] <- 1 
distm_km_100[distm_km_100>=100] <- 0

# transform into listw format
mat_100_B_preg <- mat2listw(distm_km_100,style = "B")


#### #### #### #### #### #### #### #### #### #### #### #### #### 
#### #### Run all models
#### #### #### #### #### #### #### #### #### #### #### #### #### 

#### PREG Any Link Outcome
## baseline
m.ols <- mdlr_dummy_plus(y="preg_link_narrow", x="hance_crops5_dummy",cntr=c("pub23_dummy",base.x),
                         cl="iso3c",fe="iso3c",dat=wlms_data_preg)
summary(m.ols)

## subset to wlms polygons with missions
m.missions <- mdlr_dummy_plus(y="preg_link_narrow", x="hance_crops5_dummy",cntr=c("pub23_dummy",base.x),
                              cl="iso3c",fe="iso3c",dat=wlms_data_preg[wlms_data_preg$missions_yn==1,])
summary(m.missions)

## spatial IV
m.iv <- mdlr_dummy_plus(y="preg_link_narrow", x="hance_crops5_dummy",cntr=c("pub23_dummy",base.x),
                        cl="iso3c",fe="iso3c", iv="suit5_std", dat=wlms_data_preg,listw = mat_100_B_preg,lag_dummies = T)
summary(m.iv)

# check first stage F in both first stage models
lfe::waldtest(m.iv$stage1,lhs="hance_crops5_dummy","suit5_std",type="cluster")["F"]
lfe::waldtest(m.iv$stage1,lhs="lag_preg_link_narrow",grep("lag",rownames(coef(m.iv$stage1))),type="cluster")["F"]

# save coefficients for post processing
post_est_preg <- m.iv$coefficients[c(length(m.iv$coefficients),length(m.iv$coefficients)-1)]

## OLS with logged polygon population averaged from decadal HYDE rasters 1720-1890
m.ols.cntr <- mdlr_dummy_plus(y="preg_link_narrow", x="hance_crops5_dummy",cntr=c("pub23_dummy",base.x,"popc_mean_1720_1890_log"),
                              cl="iso3c",fe="iso3c", dat=wlms_data_preg)
summary(m.ols.cntr)

## put four models in list to facilitate plotting below
m.list.preg <- list(m.ols,m.iv,m.missions,m.ols.cntr)


#### EPR Link Outcome
m.ols <- mdlr_dummy_plus(y="epr_link_narrow", x="hance_crops5_dummy",cntr=c("pub23_dummy",base.x),
                         cl="iso3c",fe="iso3c",dat=wlms_data_epr)
summary(m.ols)

## subset to wlms polygons with missions
m.missions <- mdlr_dummy_plus(y="epr_link_narrow", x="hance_crops5_dummy",cntr=c("pub23_dummy",base.x),
                              cl="iso3c",fe="iso3c",dat=wlms_data_epr[wlms_data_epr$missions_yn==1,])
summary(m.missions)

m.iv <- mdlr_dummy_plus(y="epr_link_narrow", x="hance_crops5_dummy",cntr=c("pub23_dummy",base.x),
                        cl="iso3c",fe="iso3c", iv="suit5_std", dat=wlms_data_epr,listw = mat_100_B_epr,lag_dummies = T)
summary(m.iv)
# check first stage F in both first stage models
lfe::waldtest(m.iv$stage1,lhs="hance_crops5_dummy","suit5_std",type="cluster")["F"]
lfe::waldtest(m.iv$stage1,lhs="lag_epr_link_narrow",grep("lag",rownames(coef(m.iv$stage1))),type="cluster")["F"]

# save coefficients for post processing
post_est_epr <- m.iv$coefficients[c(length(m.iv$coefficients),length(m.iv$coefficients)-1)]

## OLS with logged polygon population averaged from decadal HYDE rasters 1720-1890
m.ols.cntr <- mdlr_dummy_plus(y="epr_link_narrow", x="hance_crops5_dummy",cntr=c("pub23_dummy",base.x,"popc_mean_1720_1890_log"),
                              cl="iso3c",fe="iso3c", dat=wlms_data_epr)
summary(m.ols.cntr)

## put four models in list to facilitate plotting below
m.list.epr<- list(m.ols,m.iv,m.missions,m.ols.cntr)


#### #### #### #### #### #### #### #### #### #### #### #### #### 
#### #### Prepare plot data frame and produce results graph
#### #### #### #### #### #### #### #### #### #### #### #### #### 

plot.df.preg <- do.call(rbind,lapply(m.list.preg,function(x){
  get_plot_data(x,coef_no=c(grep("hance_crops5_dummy",names(coef(x))),grep("pub23_dummy",names(coef(x)))))
}))
plot.df.preg$model <- rep(c("OLS" , "Spatial IV", "Subsample Missions","More Controls"),each=2)
plot.df.preg$outcome <- rep(c("Excl. PREG Link"),8)
plot.df.preg$treat <- rep(c("Cash Crops (Y/N)", "Publications (1923, Y/N)"),4)

plot.df.epr <- do.call(rbind,lapply(m.list.epr,function(x){
  get_plot_data(x,coef_no=c(grep("hance_crops5_dummy",names(coef(x))),grep("pub23_dummy",names(coef(x)))))
}))
plot.df.epr$model <- rep(c("OLS" , "Spatial IV", "Subsample Missions","More Controls"),each=2)
plot.df.epr$outcome <- rep(c("Excl. EPR Link"),8)
plot.df.epr$treat <- rep(c("Cash Crops (Y/N)", "Publications (1923, Y/N)"),4)

plot.df <- rbind(plot.df.preg,plot.df.epr)
plot.df$order <- rep(c(4.2,3.8,3.2,2.8,2.2,1.8,1.2,0.8),2)
plot.df$what <- c("Cash Crops (OLS)", "Publications (OLS)", "Cash Crops (S2SLS)", "Publications (S2SLS)",
                  "Cash Crops (Subsample)", "Publications (Subsample)","Cash Crops (Size Controls)", "Publications (Size Controls)")

plot.df$outcome <- factor(plot.df$outcome,levels=c("Excl. PREG Link", "Excl. EPR Link"))

col1 <- wes_palette("Darjeeling1",5,"discrete")[2]
col2 <- wes_palette("Darjeeling1",5,"discrete")[3]

p <- ggplot(plot.df)
p <- p + geom_point(size=2.75,aes(x=beta,y=order,color=treat,shape=treat,fill=treat)) + 
  geom_errorbarh(aes(x=beta,y=order, xmin=lb, xmax=ub,color=treat), size=1, height=0.0) +
  geom_vline(xintercept=0,linetype="dotted", size=0.6) +
  scale_y_continuous(breaks=plot.df$order[1:8],minor_breaks = NULL, labels=plot.df$what[1:8],limits = c(0.5,4.5)) + 
  labs(x = "Coefficients and 95% Confidence Intervals", y = "Treatment (Model)",
       title="Cash Crops, Publications & Political Relevance",
       subtitle="UoA: Ethnologue languages, DV: Exclusive Match in PREG/EPR") +
  facet_wrap(~ outcome,nrow=1,scales="free_x")  +
  theme_minimal(base_size=16) +  
  theme(axis.text.y = element_text(size=10),legend.position = "bottom") +
  scale_color_manual(values=c("Cash Crops (Y/N)" = col1, "Publications (1923, Y/N)" = col2),name="Treatment") + 
  scale_fill_manual(values=c("Cash Crops (Y/N)" = col1, "Publications (1923, Y/N)" = col2),name="Treatment") +
  scale_shape_manual(values=c("Cash Crops (Y/N)" = 24, "Publications (1923, Y/N)" = 25),name="Treatment")
p
p + ggsave(paste0(fig.path,"wlms_figA11.pdf"),width=9,height=7)





