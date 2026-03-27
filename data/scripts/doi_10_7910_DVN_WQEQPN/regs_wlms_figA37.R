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



############################## Control for religion

m1 <- mdlr_dummy_plus(y="preg_link", x="hance_crops5_dummy + pub23_dummy",cntr=c(base.x,"popc_mean_1720_1890_log"),
                         cl="iso3c",fe="iso3c",dat=wlms_data_preg)
summary(m1)

m2 <- mdlr_dummy_plus(y="preg_link", x="hance_crops5_dummy + pub23_dummy",cntr=c(base.x,"pct.chris.jp","popc_mean_1720_1890_log"),
                         cl="iso3c",fe="iso3c",dat=wlms_data_preg)
summary(m1)

m3 <- mdlr_dummy_plus(y="epr_link", x="hance_crops5_dummy + pub23_dummy",cntr=c(base.x,"popc_mean_1720_1890_log"),
                         cl="iso3c",fe="iso3c",dat=wlms_data_epr)
summary(m3)

m4 <- mdlr_dummy_plus(y="epr_link", x="hance_crops5_dummy + pub23_dummy",cntr=c(base.x,"popc_mean_1720_1890_log","pct.chris.jp"),
                         cl="iso3c",fe="iso3c",dat=wlms_data_epr)
summary(m4)

m5 <- mdlr_dummy_plus(y="preg_link_narrow", x="hance_crops5_dummy + pub23_dummy",cntr=c(base.x,"popc_mean_1720_1890_log"),
                         cl="iso3c",fe="iso3c",dat=wlms_data_preg)
summary(m5)

m6 <- mdlr_dummy_plus(y="preg_link_narrow", x="hance_crops5_dummy + pub23_dummy",cntr=c(base.x,"popc_mean_1720_1890_log","pct.chris.jp"),
                         cl="iso3c",fe="iso3c",dat=wlms_data_preg)
summary(m6)

m7 <- mdlr_dummy_plus(y="epr_link_narrow", x="hance_crops5_dummy + pub23_dummy",cntr=c(base.x,"popc_mean_1720_1890_log"),
                         cl="iso3c",fe="iso3c",dat=wlms_data_epr)
summary(m7)

m8 <- mdlr_dummy_plus(y="epr_link_narrow", x="hance_crops5_dummy + pub23_dummy",cntr=c(base.x,"popc_mean_1720_1890_log","pct.chris.jp"),
                         cl="iso3c",fe="iso3c",dat=wlms_data_epr)
summary(m8)

plot.df <- rbind(
  deltaMethod(m1,"hance_crops5_dummy"),
  deltaMethod(m2,"hance_crops5_dummy"),
  deltaMethod(m1,"pub23_dummy"),
  deltaMethod(m2,"pub23_dummy"),
  deltaMethod(m3,"hance_crops5_dummy"),
  deltaMethod(m4,"hance_crops5_dummy"),
  deltaMethod(m3,"pub23_dummy"),
  deltaMethod(m4,"pub23_dummy"),
  deltaMethod(m5,"hance_crops5_dummy"),
  deltaMethod(m6,"hance_crops5_dummy"),
  deltaMethod(m5,"pub23_dummy"),
  deltaMethod(m6,"pub23_dummy"),
  deltaMethod(m7,"hance_crops5_dummy"),
  deltaMethod(m8,"hance_crops5_dummy"),
  deltaMethod(m7,"pub23_dummy"),
  deltaMethod(m8,"pub23_dummy")
)

names(plot.df) <- c("beta", "se", "lb", "ub")

plot.df$outcome <- factor(rep(c("PREG Link", "EPR Link", "Excl. PREG Link","Excl. EPR Link"),each=4),
                             levels=c("PREG Link", "EPR Link", "Excl. PREG Link", "Excl. EPR Link"))
plot.df$what <- rep(c("Cash Crops","Cash Crops (Religion Control)","Publications", "Publications (Religion Control)"),4)
plot.df$what  <- factor(plot.df$what,levels=c("Cash Crops","Cash Crops (Religion Control)","Publications", "Publications (Religion Control)"))

ord <- c(c(4:1)+0.2,c(4:1)+0.1,c(4:1)-0.1,c(4:1)-0.2)
ord <- ord[order(ord,decreasing = T)]

plot.df$order <- ord 

col1 <- wes_palette("Darjeeling1",5,"discrete")[2]
col2 <- wes_palette("Darjeeling1",5,"discrete")[3]
col3 <- wes_palette("Darjeeling1",5,"discrete")[5]
col4 <- wes_palette("Darjeeling1",15,"continuous")[2] 




brks <- c(4.35,3.35,2.35,1.35,ord)
brks <- brks[order(brks,decreasing = T)]
lbs <- c("PREG Link", "Cash Crops", "Cash Crops (Religion Control)","Publications","Publications (Religion Control)",
         "EPR Link", "Cash Crops", "Cash Crops (Religion Control)","Publications","Publications (Religion Control)",
         "Excl. PREG Link", "Cash Crops", "Cash Crops (Religion Control)","Publications","Publications (Religion Control)",
         "Excl. EPR Link","Cash Crops", "Cash Crops (Religion Control)","Publications","Publications (Religion Control)")

p <- ggplot(plot.df)
p <- p + geom_point(size=2.75,aes(x=beta,y=order,color=what,shape=what,fill=what)) + 
  geom_errorbarh(aes(x=beta,y=order, xmin=lb, xmax=ub,color=what), size=1, height=0.0) +
  geom_vline(xintercept=0,linetype="dotted", size=0.6) +
  #geom_text(aes(x=p.x,y=order,label=p.lab)) +
  scale_y_continuous(breaks=brks,minor_breaks = NULL, labels=lbs,limits = c(0.5,4.7)) + 
  #scale_x_continuous(minor_breaks = seq(0 , 0.55, 0.05), breaks = seq(0, 0.55, 0.1)) +
  labs(x = "Coefficients and 95% Confidence Intervals", y = "Treatment(s)",
       title="Cash Crops, Publications & Political Relevance",
       subtitle="Controlling for Share of Christians per Group") +
  #facet_wrap(~ outcome,nrow=1,scales="free_x")  +
  theme_minimal(base_size=16) +
  theme(axis.text.y = element_text(size=10, face = rep(c("bold",rep("plain",4)),4)),legend.position = "bottom") +
  scale_color_manual(values=c("Cash Crops" = col1, "Cash Crops (Religion Control)" = col2, "Publications" =col3, "Publications (Religion Control)" =col4),name="Treatment/Model") + 
  scale_fill_manual(values=c("Cash Crops" = col1, "Cash Crops (Religion Control)" = col2, "Publications" =col3, "Publications (Religion Control)" =col4),name="Treatment/Model") +
  scale_shape_manual(values=c("Cash Crops" = 24, "Cash Crops (Religion Control)" = 22, "Publications" = 23, "Publications (Religion Control)" =25),name="Treatment/Model")
p <- p + guides(color=guide_legend(ncol=2,nrow=2,byrow=TRUE),
                fill=guide_legend(ncol=2,nrow=2,byrow=TRUE),
                shape=guide_legend(ncol=2,nrow=2,byrow=TRUE))
p
p + ggsave(paste0(fig.path,"wlms_figA37.pdf"),width=8.5,height=10)
