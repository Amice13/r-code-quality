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

# code colonizer dummies
cntrs <- unique(wlms_data$iso3c)
cntrs <- cntrs[order(cntrs)]
empires <- c("Portugal","Belgium","France","France","UK","France","France","France","Belgium","France","France","Portugal","France","France",
             "UK","Italy","Italy","France","UK","France","UK","Portugal","Spain","UK","none","Italy","UK","France","France",
             "France","Portugal","France","UK","UK","UK","France","UK","Belgium","UK","France","UK","Italy","Portugal","UK","UK","France","France","France","UK","UK","UK","UK","UK")
cntrs <- data.frame(iso3c=cntrs,colonizer=empires)
names(cntrs)
wlms_data <- left_join(wlms_data,cntrs)
wlms_data$col_brit <- ifelse(wlms_data$colonizer=="UK",1,0)
wlms_data$col_frnc <- ifelse(wlms_data$colonizer=="France",1,0)
wlms_data$col_other <- ifelse(wlms_data$col_brit==0 & wlms_data$col_frnc==0,1,0)


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



############################################################################################
## Interactive models empires
############################################################################################

m.ia1 <- mdlr_dummy_plus(y="preg_link", x="hance_crops5_dummy:col_brit + hance_crops5_dummy:col_frnc + hance_crops5_dummy:col_other +
                         pub23_dummy:col_brit +pub23_dummy:col_frnc + pub23_dummy:col_other",cntr=base.x,
                         cl="iso3c",fe="iso3c",dat=wlms_data_preg)
summary(m.ia1)

m.ia2 <- mdlr_dummy_plus(y="epr_link", x="hance_crops5_dummy:col_brit + hance_crops5_dummy:col_frnc + hance_crops5_dummy:col_other +
                         pub23_dummy:col_brit +pub23_dummy:col_frnc + pub23_dummy:col_other",cntr=base.x,
                         cl="iso3c",fe="iso3c",dat=wlms_data_epr)
summary(m.ia2)

m.ia3 <- mdlr_dummy_plus(y="preg_link_narrow", x="hance_crops5_dummy:col_brit + hance_crops5_dummy:col_frnc + hance_crops5_dummy:col_other +
                         pub23_dummy:col_brit +pub23_dummy:col_frnc + pub23_dummy:col_other",cntr=base.x,
                         cl="iso3c",fe="iso3c",dat=wlms_data_preg)
summary(m.ia3)

m.ia4 <- mdlr_dummy_plus(y="epr_link_narrow", x="hance_crops5_dummy:col_brit + hance_crops5_dummy:col_frnc + hance_crops5_dummy:col_other +
                         pub23_dummy:col_brit +pub23_dummy:col_frnc + pub23_dummy:col_other",cntr=base.x,
                         cl="iso3c",fe="iso3c",dat=wlms_data_epr)
summary(m.ia4)

plot.df.ia <- rbind(
  deltaMethod(m.ia1,"`hance_crops5_dummy:col_other`"),
  deltaMethod(m.ia1,"`hance_crops5_dummy:col_brit`"),
  deltaMethod(m.ia1,"`hance_crops5_dummy:col_frnc`"),
  deltaMethod(m.ia1,"`col_other:pub23_dummy`"),
  deltaMethod(m.ia1,"`col_brit:pub23_dummy`"),
  deltaMethod(m.ia1,"`col_frnc:pub23_dummy`"),
  deltaMethod(m.ia2,"`hance_crops5_dummy:col_other`"),
  deltaMethod(m.ia2,"`hance_crops5_dummy:col_brit`"),
  deltaMethod(m.ia2,"`hance_crops5_dummy:col_frnc`"),
  deltaMethod(m.ia2,"`col_other:pub23_dummy`"),
  deltaMethod(m.ia2,"`col_brit:pub23_dummy`"),
  deltaMethod(m.ia2,"`col_frnc:pub23_dummy`"),
  deltaMethod(m.ia3,"`hance_crops5_dummy:col_other`"),
  deltaMethod(m.ia3,"`hance_crops5_dummy:col_brit`"),
  deltaMethod(m.ia3,"`hance_crops5_dummy:col_frnc`"),
  deltaMethod(m.ia3,"`col_other:pub23_dummy`"),
  deltaMethod(m.ia3,"`col_brit:pub23_dummy`"),
  deltaMethod(m.ia3,"`col_frnc:pub23_dummy`"),
  deltaMethod(m.ia4,"`hance_crops5_dummy:col_other`"),
  deltaMethod(m.ia4,"`hance_crops5_dummy:col_brit`"),
  deltaMethod(m.ia4,"`hance_crops5_dummy:col_frnc`"),
  deltaMethod(m.ia4,"`col_other:pub23_dummy`"),
  deltaMethod(m.ia4,"`col_brit:pub23_dummy`"),
  deltaMethod(m.ia4,"`col_frnc:pub23_dummy`")
)

names(plot.df.ia) <- c("beta", "se", "lb", "ub")

plot.df.ia$outcome <- factor(rep(c("PREG Link", "EPR Link", "Excl. PREG Link","Excl. EPR Link"),each=6),
                             levels=c("PREG Link", "EPR Link", "Excl. PREG Link", "Excl. EPR Link"))
plot.df.ia$what <- factor((rep(c("Cash Crops (other)", "Cash Crops (UK)","Cash Crops (FRA)","Publications (other)", "Publications (UK)","Publications (FRA)"),4)),
                          levels=c("Cash Crops (other)", "Cash Crops (UK)","Cash Crops (FRA)","Publications (other)", "Publications (UK)","Publications (FRA)"))



ord <- c(3.2,2.8,2.4,1.6,1.2,0.8)
ord <- ord[order(ord,decreasing = T)]

plot.df.ia$order <- ord 

col1 <- wes_palette("Darjeeling1",5,"discrete")[2]
col2 <- wes_palette("Darjeeling1",5,"discrete")[3]
col3 <- wes_palette("Darjeeling1",15,"continuous")[2] 



p <- ggplot(plot.df.ia)
p <- p + geom_point(size=2.75,aes(x=beta,y=order,color=what,shape=what,fill=what)) + 
  geom_errorbarh(aes(x=beta,y=order, xmin=lb, xmax=ub,color=what), size=1, height=0.0) +
  geom_vline(xintercept=0,linetype="dotted", size=0.6) +
  #geom_text(aes(x=p.x,y=order,label=p.lab)) +
  scale_y_continuous(breaks=plot.df.ia$order[1:6],minor_breaks = NULL, labels=plot.df.ia$what[1:6],limits = c(0.5,3.5)) + 
  #scale_x_continuous(minor_breaks = seq(0 , 0.55, 0.05), breaks = seq(0, 0.55, 0.1)) +
  labs(x = "Coefficients and 95% Confidence Intervals", y = "Treatment(s)",
       title="Cash Crops, Publications & Political Relevance",
       subtitle="Interacting Cash Crops and Publications with Colonizer") +
  facet_wrap(~ outcome,nrow=1,scales="free_x")  +
  theme_minimal(base_size=16) +  
  theme(legend.position = "none")+
  scale_color_manual(values=c("Cash Crops (other)" = col1, "Cash Crops (UK)" = col2, "Cash Crops (FRA)" = col3,
                              "Publications (other)" = col1, "Publications (UK)" = col2, "Publications (FRA)" = col3),name="Marginal Effect") + 
  scale_fill_manual(values=c("Cash Crops (other)" = col1, "Cash Crops (UK)" = col2, "Cash Crops (FRA)" = col3,
                             "Publications (other)" = col1, "Publications (UK)" = col2, "Publications (FRA)" = col3),name="Marginal Effect") + 
  scale_shape_manual(values=c("Cash Crops (other)" = 24, "Cash Crops (UK)" = 24, "Cash Crops (FRA)" =24, 
                              "Publications (other)" = 23,"Publications (UK)" = 23,"Publications (FRA)" = 23),name="Marginal Effect") 
p
p + ggsave(paste0(fig.path,"wlms_figA30.pdf"),width=14,height=6)
