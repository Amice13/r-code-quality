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



conflict <- readRDS("wlms/conflict2wlms.rds")



## plot order
# acled id militia
# acled id militia onset
# scad ethnic issue
# scad non-state violence
# ucdp non-state violence


########### ACLED 
wlms_data <- wlms_data[,c(1: which(names(wlms_data)=="dist.print_log"))]

wlms_data <- left_join(wlms_data,conflict[["acled2wlms"]])

## continuous outcomes
m1a_cont <- felm(as.formula(RegFor( y = "log(1+identity_mil)" , x = c("hance_crops5_dummy","in.chr23", base.x, "area_sqkm","log(1+popd_2000AD)") ,
                                    FE = "iso3c" , IV="0", clust = "iso3c" )),
                 data=wlms_data,subset = hance_country==1)

m1b_cont <- felm(as.formula(RegFor( y = "log(1+identity_mil_onset)" , x = c("hance_crops5_dummy","in.chr23", base.x, "area_sqkm","log(1+popd_2000AD)") ,
                                    FE = "iso3c" , IV="0", clust = "iso3c" )),
                 data=wlms_data,subset = hance_country==1)

### dummies only
m1a <- felm(as.formula(RegFor( y = "identity_mil_dummy" , x = c("hance_crops5_dummy","in.soas.23", base.x, "area_sqkm","log(1+popd_2000AD)") ,
                               FE = "iso3c" , IV="0", clust = "iso3c" )),
            data=wlms_data,subset = hance_country==1)

m1b <- felm(as.formula(RegFor( y = "identity_mil_onset_dummy" , x = c("hance_crops5_dummy","in.soas.23", base.x, "area_sqkm","log(1+popd_2000AD)") ,
                               FE = "iso3c" , IV="0", clust = "iso3c" )),
            data=wlms_data,subset = hance_country==1)




########### SCAD
wlms_data <- wlms_data[,c(1: which(names(wlms_data)=="dist.print_log"))]

wlms_data <- left_join(wlms_data,conflict[["scad2wlms"]])

## continuous outcomes
m2a_cont <- felm(as.formula(RegFor( y = "log(1+scad_event_com_vio)" , x = c("hance_crops5_dummy","in.chr23", base.x, "area_sqkm","log(1+popd_2000AD)") ,
                                    FE = "iso3c" , IV="0", clust = "iso3c" )),
                 data=wlms_data,subset = hance_country==1)

m2b_cont <- felm(as.formula(RegFor( y = "log(1+issue_ethnic_rel)" , x = c("hance_crops5_dummy","in.chr23", base.x, "area_sqkm","log(1+popd_2000AD)") ,
                                    FE = "iso3c" , IV="0", clust = "iso3c" )),
                 data=wlms_data,subset = hance_country==1)


### dummies only
m2a <- felm(as.formula(RegFor( y = "scad_event_com_vio_dummy" , x = c("hance_crops5_dummy","in.soas.23", base.x, "area_sqkm","log(1+popd_2000AD)") ,
                               FE = "iso3c" , IV="0", clust = "iso3c" )),
            data=wlms_data,subset = hance_country==1)


m2b <- felm(as.formula(RegFor( y = "issue_ethnic_rel_dummy" , x = c("hance_crops5_dummy","in.soas.23", base.x, "area_sqkm","log(1+popd_2000AD)") ,
                               FE = "iso3c" , IV="0", clust = "iso3c" )),
            data=wlms_data,subset = hance_country==1)




########### UCDP
wlms_data <- wlms_data[,c(1: which(names(wlms_data)=="dist.print_log"))]

wlms_data <- left_join(wlms_data,conflict[["ucdp2wlms"]])


## continuous outcomes
m3_cont <- felm(as.formula(RegFor( y = "log(1+ucdp_ns)" , x = c("hance_crops5_dummy","in.chr23", base.x, "area_sqkm","log(1+popd_2000AD)") ,
                                   FE = "iso3c" , IV="0", clust = "iso3c" )),
                data=wlms_data,subset = hance_country==1)




### dummies only
m3 <- felm(as.formula(RegFor( y = "ucdp_ns_dummy" , x = c("hance_crops5_dummy","in.soas.23", base.x, "area_sqkm","log(1+popd_2000AD)") ,
                              FE = "iso3c" , IV="0", clust = "iso3c" )),
           data=wlms_data,subset = hance_country==1)

m.list <- list(m1a,m1b,m2a,m2b,m3)
m.list.cont <- list(m1a_cont,m1b_cont,m2a_cont,m2b_cont,m3_cont)





###########  Only precisely geocoded events
########### ACLED 
wlms_data <- wlms_data[,c(1: which(names(wlms_data)=="dist.print_log"))]

wlms_data <- left_join(wlms_data,conflict[["acled2wlms_precise"]])

## continuous outcomes
m1a_cont <- felm(as.formula(RegFor( y = "log(1+identity_mil)" , x = c("hance_crops5_dummy","in.chr23", base.x, "area_sqkm","log(1+popd_2000AD)") ,
                                    FE = "iso3c" , IV="0", clust = "iso3c" )),
                 data=wlms_data,subset = hance_country==1)

m1b_cont <- felm(as.formula(RegFor( y = "log(1+identity_mil_onset)" , x = c("hance_crops5_dummy","in.chr23", base.x, "area_sqkm","log(1+popd_2000AD)") ,
                                    FE = "iso3c" , IV="0", clust = "iso3c" )),
                 data=wlms_data,subset = hance_country==1)

### dummies only
m1a <- felm(as.formula(RegFor( y = "identity_mil_dummy" , x = c("hance_crops5_dummy","in.soas.23", base.x, "area_sqkm","log(1+popd_2000AD)") ,
                               FE = "iso3c" , IV="0", clust = "iso3c" )),
            data=wlms_data,subset = hance_country==1)

m1b <- felm(as.formula(RegFor( y = "identity_mil_onset_dummy" , x = c("hance_crops5_dummy","in.soas.23", base.x, "area_sqkm","log(1+popd_2000AD)") ,
                               FE = "iso3c" , IV="0", clust = "iso3c" )),
            data=wlms_data,subset = hance_country==1)




########### SCAD
wlms_data <- wlms_data[,c(1: which(names(wlms_data)=="dist.print_log"))]

wlms_data <- left_join(wlms_data,conflict[["scad2wlms_precise"]])

## continuous outcomes
m2a_cont <- felm(as.formula(RegFor( y = "log(1+scad_event_com_vio)" , x = c("hance_crops5_dummy","in.chr23", base.x, "area_sqkm","log(1+popd_2000AD)") ,
                                    FE = "iso3c" , IV="0", clust = "iso3c" )),
                 data=wlms_data,subset = hance_country==1)

m2b_cont <- felm(as.formula(RegFor( y = "log(1+issue_ethnic_rel)" , x = c("hance_crops5_dummy","in.chr23", base.x, "area_sqkm","log(1+popd_2000AD)") ,
                                    FE = "iso3c" , IV="0", clust = "iso3c" )),
                 data=wlms_data,subset = hance_country==1)


### dummies only
m2a <- felm(as.formula(RegFor( y = "scad_event_com_vio_dummy" , x = c("hance_crops5_dummy","in.soas.23", base.x, "area_sqkm","log(1+popd_2000AD)") ,
                               FE = "iso3c" , IV="0", clust = "iso3c" )),
            data=wlms_data,subset = hance_country==1)


m2b <- felm(as.formula(RegFor( y = "issue_ethnic_rel_dummy" , x = c("hance_crops5_dummy","in.soas.23", base.x, "area_sqkm","log(1+popd_2000AD)") ,
                               FE = "iso3c" , IV="0", clust = "iso3c" )),
            data=wlms_data,subset = hance_country==1)




########### UCDP
wlms_data <- wlms_data[,c(1: which(names(wlms_data)=="dist.print_log"))]

wlms_data <- left_join(wlms_data,conflict[["ucdp2wlms_precise"]])


## continuous outcomes
m3_cont <- felm(as.formula(RegFor( y = "log(1+ucdp_ns)" , x = c("hance_crops5_dummy","in.chr23", base.x, "area_sqkm","log(1+popd_2000AD)") ,
                                   FE = "iso3c" , IV="0", clust = "iso3c" )),
                data=wlms_data,subset = hance_country==1)




### dummies only
m3 <- felm(as.formula(RegFor( y = "ucdp_ns_dummy" , x = c("hance_crops5_dummy","in.soas.23", base.x, "area_sqkm","log(1+popd_2000AD)") ,
                              FE = "iso3c" , IV="0", clust = "iso3c" )),
           data=wlms_data,subset = hance_country==1)

m.list_precise <- list(m1a,m1b,m2a,m2b,m3)
m.list.cont_precise <- list(m1a_cont,m1b_cont,m2a_cont,m2b_cont,m3_cont)




###########  Only intense and precisely geocoded events
########### ACLED 
wlms_data <- wlms_data[,c(1: which(names(wlms_data)=="dist.print_log"))]
names(conflict)

wlms_data <- left_join(wlms_data,conflict[["acled2wlms_fatal"]])

## continuous outcomes
m1a_cont <- felm(as.formula(RegFor( y = "log(1+identity_mil)" , x = c("hance_crops5_dummy","in.chr23", base.x, "area_sqkm","log(1+popd_2000AD)") ,
                                    FE = "iso3c" , IV="0", clust = "iso3c" )),
                 data=wlms_data,subset = hance_country==1)

m1b_cont <- felm(as.formula(RegFor( y = "log(1+identity_mil_onset)" , x = c("hance_crops5_dummy","in.chr23", base.x, "area_sqkm","log(1+popd_2000AD)") ,
                                    FE = "iso3c" , IV="0", clust = "iso3c" )),
                 data=wlms_data,subset = hance_country==1)

### dummies only
m1a <- felm(as.formula(RegFor( y = "identity_mil_dummy" , x = c("hance_crops5_dummy","in.soas.23", base.x, "area_sqkm","log(1+popd_2000AD)") ,
                               FE = "iso3c" , IV="0", clust = "iso3c" )),
            data=wlms_data,subset = hance_country==1)

m1b <- felm(as.formula(RegFor( y = "identity_mil_onset_dummy" , x = c("hance_crops5_dummy","in.soas.23", base.x, "area_sqkm","log(1+popd_2000AD)") ,
                               FE = "iso3c" , IV="0", clust = "iso3c" )),
            data=wlms_data,subset = hance_country==1)




########### SCAD
wlms_data <- wlms_data[,c(1: which(names(wlms_data)=="dist.print_log"))]

wlms_data <- left_join(wlms_data,conflict[["scad2wlms_intense"]])

## continuous outcomes
m2a_cont <- felm(as.formula(RegFor( y = "log(1+scad_event_com_vio)" , x = c("hance_crops5_dummy","in.chr23", base.x, "area_sqkm","log(1+popd_2000AD)") ,
                                    FE = "iso3c" , IV="0", clust = "iso3c" )),
                 data=wlms_data,subset = hance_country==1)

m2b_cont <- felm(as.formula(RegFor( y = "log(1+issue_ethnic_rel)" , x = c("hance_crops5_dummy","in.chr23", base.x, "area_sqkm","log(1+popd_2000AD)") ,
                                    FE = "iso3c" , IV="0", clust = "iso3c" )),
                 data=wlms_data,subset = hance_country==1)


### dummies only
m2a <- felm(as.formula(RegFor( y = "scad_event_com_vio_dummy" , x = c("hance_crops5_dummy","in.soas.23", base.x, "area_sqkm","log(1+popd_2000AD)") ,
                               FE = "iso3c" , IV="0", clust = "iso3c" )),
            data=wlms_data,subset = hance_country==1)


m2b <- felm(as.formula(RegFor( y = "issue_ethnic_rel_dummy" , x = c("hance_crops5_dummy","in.soas.23", base.x, "area_sqkm","log(1+popd_2000AD)") ,
                               FE = "iso3c" , IV="0", clust = "iso3c" )),
            data=wlms_data,subset = hance_country==1)




########### UCDP
wlms_data <- wlms_data[,c(1: which(names(wlms_data)=="dist.print_log"))]

wlms_data <- left_join(wlms_data,conflict[["ucdp2wlms_intense"]])


## continuous outcomes
m3_cont <- felm(as.formula(RegFor( y = "log(1+ucdp_ns)" , x = c("hance_crops5_dummy","in.chr23", base.x, "area_sqkm","log(1+popd_2000AD)") ,
                                   FE = "iso3c" , IV="0", clust = "iso3c" )),
                data=wlms_data,subset = hance_country==1)




### dummies only
m3 <- felm(as.formula(RegFor( y = "ucdp_ns_dummy" , x = c("hance_crops5_dummy","in.soas.23", base.x, "area_sqkm","log(1+popd_2000AD)") ,
                              FE = "iso3c" , IV="0", clust = "iso3c" )),
           data=wlms_data,subset = hance_country==1)

m.list_intense <- list(m1a,m1b,m2a,m2b,m3)
m.list.cont_intense <- list(m1a_cont,m1b_cont,m2a_cont,m2b_cont,m3_cont)






###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### 
###### Prepare Plot
###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### #####
plot.ls <- c(m.list,m.list_precise,m.list_intense)

plot.df <- lapply(plot.ls,function(x){
  get_plot_data(x,coef_no = c(1,2))
})
plot.df <- do.call(rbind,plot.df)


plot.df$what <- factor(rep(c("Cash Crops","Publications"),15))
plot.df$order <- rep(c(c(5:1)+0.1,c(5:1),c(5:1)-0.1),each=2)
plot.df$order[plot.df$what=="Publications"] <- plot.df$order[plot.df$what=="Publications"]-0.5 

plot.df$sample <- rep(c("All Events", "Precise Geocodes", "High Intensity"),each=10)

plot.df$sample <- factor(plot.df$sample,levels=c("All Events","Precise Geocodes","High Intensity"))

labs <- c("ACLED Identity Militia", "Ident. Mil (Onset)", "SCAD Non-State", "SACD Ethnic", "UCDP Non-State")

col1 <- wes_palette("Darjeeling1",5,"discrete")[2]
col2 <- wes_palette("Darjeeling1",5,"discrete")[3]
col3 <- wes_palette("Darjeeling1",15,"continuous")[2] 




# do the plot
p2 <- ggplot(plot.df)
p2 <- p2 + geom_point(size=2.75,aes(x=beta,y=order,color=sample,shape=what,fill=sample)) + 
  geom_errorbarh(aes(x=beta,y=order, xmin=lb, xmax=ub,color=sample), size=1, height=0.0) +
  geom_vline(xintercept=0,linetype="dotted", size=0.6) +
  scale_y_continuous(breaks=c(5:1)-0.25,minor_breaks = NULL, labels=labs,limits = c(0,5.4)) + 
  #scale_x_continuous(minor_breaks = seq(0 , 0.55, 0.05), breaks = seq(0, 0.55, 0.1)) +
  labs(x = "Coefficients and 95% Confidence Intervals", y = "Conflict Event Type",
       title="Cash Crops, Publications & Ethnic Conflict",
       subtitle="Treatment geographically defined") +
  theme_minimal(base_size=14) +  
  theme(axis.text.y = element_text(size=10),legend.position = "bottom",legend.box = 'vertical') +
  scale_color_manual(values=c("All Events" = col1, "Precise Geocodes" = col2, "High Intensity" = col3),name="Sample") + 
  scale_fill_manual(values=c("All Events" = col1, "Precise Geocodes" = col2,"High Intensity" = col3),name="Sample") +
  scale_shape_manual(values=c("Cash Crops" = 24, "Publications" = 25),name="Treatment")

p2


##########
plot.ls <- c(m.list.cont,m.list.cont_precise,m.list.cont_intense)

plot.df <- lapply(plot.ls,function(x){
  get_plot_data(x,coef_no = c(1,2))
})
plot.df <- do.call(rbind,plot.df)



plot.df$what <- factor(rep(c("Cash Crops","Publications"),15))
plot.df$order <- rep(c(c(5:1)+0.1,c(5:1),c(5:1)-0.1),each=2)
plot.df$order[plot.df$what=="Publications"] <- plot.df$order[plot.df$what=="Publications"]-0.5 

plot.df$sample <- rep(c("All Events", "Precise Geocodes", "High Intensity"),each=10)

plot.df$sample <- factor(plot.df$sample,levels=c("All Events","Precise Geocodes","High Intensity"))

labs <- c("ACLED Identity Militia", "Ident. Mil (Onset)", "SCAD Non-State", "SACD Ethnic", "UCDP Non-State")

col1 <- wes_palette("Darjeeling1",5,"discrete")[2]
col2 <- wes_palette("Darjeeling1",5,"discrete")[3]
col3 <- wes_palette("Darjeeling1",15,"continuous")[2] 




# do the plot
p1 <- ggplot(plot.df)
p1 <- p1 + geom_point(size=2.75,aes(x=beta,y=order,color=sample,shape=what,fill=sample)) + 
  geom_errorbarh(aes(x=beta,y=order, xmin=lb, xmax=ub,color=sample), size=1, height=0.0) +
  geom_vline(xintercept=0,linetype="dotted", size=0.6) +
  scale_y_continuous(breaks=c(5:1)-0.25,minor_breaks = NULL, labels=labs,limits = c(0,5.4)) + 
  #scale_x_continuous(minor_breaks = seq(0 , 0.55, 0.05), breaks = seq(0, 0.55, 0.1)) +
  labs(x = "Coefficients and 95% Confidence Intervals", y = "Conflict Event Type",
       title="Cash Crops, Publications & Ethnic Conflict",
       subtitle="Treatment geographically defined") +
  theme_minimal(base_size=14) +  
  theme(axis.text.y = element_text(size=10),legend.position = "bottom",legend.box = 'vertical') +
  scale_color_manual(values=c("All Events" = col1, "Precise Geocodes" = col2, "High Intensity" = col3),name="Sample") + 
  scale_fill_manual(values=c("All Events" = col1, "Precise Geocodes" = col2,"High Intensity" = col3),name="Sample") +
  scale_shape_manual(values=c("Cash Crops" = 24, "Publications" = 25),name="Treatment")

p1

p.out <- (p1 | p2)
p.out + ggsave(paste0(fig.path,"wlms_figA41.pdf"),width=16,height=9)
