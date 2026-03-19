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


#### #### #### #### #### #### #### #### #### #### #### #### #### 
#### #### Run all models
#### #### #### #### #### #### #### #### #### #### #### #### #### 

m.ia1 <- mdlr_dummy_plus(y="preg_link", x="hance_crops5_dummy*pub23_dummy",cntr=base.x,
                         cl="iso3c",fe="iso3c",dat=wlms_data_preg)
summary(m.ia1)

m.ia2 <- mdlr_dummy_plus(y="epr_link", x="hance_crops5_dummy*pub23_dummy",cntr=base.x,
                         cl="iso3c",fe="iso3c",dat=wlms_data_epr)
summary(m.ia2)

m.ia3 <- mdlr_dummy_plus(y="preg_link_narrow", x="hance_crops5_dummy*pub23_dummy",cntr=base.x,
                         cl="iso3c",fe="iso3c",dat=wlms_data_preg)
summary(m.ia3)

m.ia4 <- mdlr_dummy_plus(y="epr_link_narrow", x="hance_crops5_dummy*pub23_dummy",cntr=base.x,
                         cl="iso3c",fe="iso3c",dat=wlms_data_epr)
summary(m.ia4)


plot.df.ia <- rbind(
  deltaMethod(m.ia1,"hance_crops5_dummy"),
  deltaMethod(m.ia1,"pub23_dummy"),
  deltaMethod(m.ia1,"hance_crops5_dummy + pub23_dummy + `hance_crops5_dummy:pub23_dummy`"),
  deltaMethod(m.ia2,"hance_crops5_dummy"),
  deltaMethod(m.ia2,"pub23_dummy"),
  deltaMethod(m.ia2,"hance_crops5_dummy + pub23_dummy + `hance_crops5_dummy:pub23_dummy`"),
  deltaMethod(m.ia3,"hance_crops5_dummy"),
  deltaMethod(m.ia3,"pub23_dummy"),
  deltaMethod(m.ia3,"hance_crops5_dummy + pub23_dummy + `hance_crops5_dummy:pub23_dummy`"),
  deltaMethod(m.ia4,"hance_crops5_dummy"),
  deltaMethod(m.ia4,"pub23_dummy"),
  deltaMethod(m.ia4,"hance_crops5_dummy + pub23_dummy + `hance_crops5_dummy:pub23_dummy`")
)

names(plot.df.ia) <- c("beta", "se", "lb", "ub")

plot.df.ia$outcome <- factor(rep(c("PREG Link", "EPR Link", "Excl. PREG Link","Excl. EPR Link"),each=3),
                             levels=c("PREG Link", "EPR Link", "Excl. PREG Link", "Excl. EPR Link"))
plot.df.ia$what <- rep(c("Cash Crops", "Publications","Both"),4)
plot.df.ia$what  <- factor(plot.df.ia$what,levels=c("Cash Crops", "Publications","Both"))

ord <- c(c(4:1)+0.2,c(4:1),c(4:1)-0.2)
ord <- ord[order(ord,decreasing = T)]

plot.df.ia$order <- ord 

col1 <- wes_palette("Darjeeling1",5,"discrete")[2]
col2 <- wes_palette("Darjeeling1",5,"discrete")[3]
col3 <- wes_palette("Darjeeling1",15,"continuous")[2] 


brks <- c(4.4,3.4,2.4,1.4,ord)
brks <- brks[order(brks,decreasing = T)]
lbs <- c("PREG Link", "Cash Crops", "Publications","Both",
         "EPR Link", "Cash Crops", "Publications","Both",
         "Excl. PREG Link", "Cash Crops", "Publications","Both",
         "Excl. EPR Link","Cash Crops", "Publications","Both")

p <- ggplot(plot.df.ia)
p <- p + geom_point(size=2.75,aes(x=beta,y=order,color=what,shape=what,fill=what)) + 
  geom_errorbarh(aes(x=beta,y=order, xmin=lb, xmax=ub,color=what), size=1, height=0.0) +
  geom_vline(xintercept=0,linetype="dotted", size=0.6) +
  #geom_text(aes(x=p.x,y=order,label=p.lab)) +
  scale_y_continuous(breaks=brks,minor_breaks = NULL, labels=lbs,limits = c(0.5,4.5)) + 
  #scale_x_continuous(minor_breaks = seq(0 , 0.55, 0.05), breaks = seq(0, 0.55, 0.1)) +
  labs(x = "Coefficients and 95% Confidence Intervals", y = "Treatment(s)",
       title="Cash Crops, Publications & Political Relevance",
       subtitle="Interacting Cash Crops and Publications") +
  #facet_wrap(~ outcome,nrow=1,scales="free_x")  +
  theme_minimal(base_size=16) +  
  theme(axis.text.y = element_text(size=10, face = rep(c("bold",rep("plain",3)),4)),legend.position = "bottom") +
  scale_color_manual(values=c("Cash Crops" = col1, "Publications" = col2, "Both" = col3),name="Treatment") + 
  scale_fill_manual(values=c("Cash Crops" = col1, "Publications" = col2, "Both" = col3),name="Treatment") +
  scale_shape_manual(values=c("Cash Crops" = 24, "Publications" = 25, "Both" = 23),name="Treatment") 
p
p + ggsave(paste0(fig.path,"wlms_figA27.pdf"),width=7.5,height=10)



