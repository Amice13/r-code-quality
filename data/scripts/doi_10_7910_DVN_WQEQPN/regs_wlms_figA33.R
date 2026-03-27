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


# add different resource types
add <- readRDS("wlms/hance2wlms.rds")
names(add)
wlms_data <- left_join(wlms_data,add)



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
## Distinguishing resource types
############################################################################################

m1 <- mdlr_dummy_plus(y="preg_link", x="pub23_dummy + crop5_yn + crops_other_yn + hance_minerals_dummy + popc_mean_1720_1890_log",cntr=base.x,
                         cl="iso3c",fe="iso3c",dat=wlms_data_preg)
summary(m1)

m2 <- mdlr_dummy_plus(y="epr_link", x="pub23_dummy +crop5_yn + crops_other_yn+ hance_minerals_dummy + popc_mean_1720_1890_log",cntr=base.x,
                         cl="iso3c",fe="iso3c",dat=wlms_data_epr)
summary(m2)

m3 <- mdlr_dummy_plus(y="preg_link", x="pub23_dummy + crops_afr_yn + crops_eur_yn + hance_minerals_dummy + popc_mean_1720_1890_log",cntr=base.x,
                         cl="iso3c",fe="iso3c",dat=wlms_data_preg)
summary(m3)

m4 <- mdlr_dummy_plus(y="epr_link", x="pub23_dummy +crops_afr_yn + crops_eur_yn+ hance_minerals_dummy + popc_mean_1720_1890_log",cntr=base.x,
                         cl="iso3c",fe="iso3c",dat=wlms_data_epr)
summary(m4)

plot.df.ia <- rbind(
  deltaMethod(m1,"pub23_dummy"),
  deltaMethod(m1,"crop5_yn"),
  deltaMethod(m1,"crops_other_yn"),
  deltaMethod(m1,"hance_minerals_dummy"),
  deltaMethod(m2,"pub23_dummy"),
  deltaMethod(m2,"crop5_yn"),
  deltaMethod(m2,"crops_other_yn"),
  deltaMethod(m2,"hance_minerals_dummy"),
  deltaMethod(m3,"pub23_dummy"),
  deltaMethod(m3,"crops_afr_yn"),
  deltaMethod(m3,"crops_eur_yn"),
  deltaMethod(m3,"hance_minerals_dummy"),
  deltaMethod(m4,"pub23_dummy"),
  deltaMethod(m4,"crops_afr_yn"),
  deltaMethod(m4,"crops_eur_yn"),
  deltaMethod(m4,"hance_minerals_dummy")
)

names(plot.df.ia) <- c("beta", "se", "lb", "ub")

plot.df.ia$outcome <- factor(rep(c("PREG Link", "EPR Link", "Excl. PREG Link","Excl. EPR Link"),each=4),
                             levels=c("PREG Link", "EPR Link", "Excl. PREG Link", "Excl. EPR Link"))
plot.df.ia$what <- c(rep(c("Publications","Top 5 Cash Crops","Other Cash Crops","Minerals"),2),
                     rep(c("Publications","Smallholder Cash Crops","Plantation Cash Crops","Minerals"),2))

plot.df1 <- plot.df.ia[c(1:8),]
plot.df2 <- plot.df.ia[c(9:16),]

plot.df1$what  <- factor(plot.df1$what,levels=c("Publications","Top 5 Cash Crops","Other Cash Crops","Minerals"))
plot.df2$what  <- factor(plot.df2$what,levels=c("Publications","Smallholder Cash Crops","Plantation Cash Crops","Minerals"))


ord <- c(0.3,0.5,0.7,0.9,1.5,1.7,1.9,2.1)

ord <- ord[order(ord,decreasing = T)]

plot.df1$order <- ord 
plot.df2$order <- ord 

col1 <- wes_palette("Darjeeling1",5,"discrete")[3]
col2 <- wes_palette("Darjeeling1",5,"discrete")[2]
col3 <- wes_palette("Darjeeling1",5,"discrete")[5]
col4 <- wes_palette("Darjeeling1",15,"continuous")[2] 



brks <- c(0.3,0.5,0.7,0.9,1.1,1.5,1.7,1.9,2.1,2.3)

brks <- brks[order(brks,decreasing = T)]
lbs1 <- c("PREG Link", "Publications","Top 5 Cash Crops","Other Cash Crops","Minerals",
          "EPR Link", "Publications","Top 5 Cash Crops","Other Cash Crops","Minerals")
lbs2 <- c("PREG Link", "Publications","Smallholder Cash Crops","Plantation Cash Crops","Minerals",
          "EPR Link","Publications","Smallholder Cash Crops","Plantation Cash Crops","Minerals")

as.character(plot.df1$what)

p1 <- ggplot(plot.df1)
p1 <- p1 + geom_point(size=2.75,aes(x=beta,y=order,color=what,shape=what,fill=what)) + 
  geom_errorbarh(aes(x=beta,y=order, xmin=lb, xmax=ub,color=what), size=1, height=0.0) +
  geom_vline(xintercept=0,linetype="dotted", size=0.6) +
  #geom_text(aes(x=p.x,y=order,label=p.lab)) +
  scale_y_continuous(breaks=brks,minor_breaks = NULL, labels=lbs1,limits = c(0,2.5)) + 
  #scale_x_continuous(minor_breaks = seq(0 , 0.55, 0.05), breaks = seq(0, 0.55, 0.1)) +
  labs(x = "Coefficients and 95% Confidence Intervals", y = "Outcome & Treatment(s)",
       title="Cash Crops, Publications & Political Relevance",
       subtitle="Adding other Crops & Minerals") +
  #facet_wrap(~ outcome,nrow=1,scales="free_x")  +
  theme_minimal(base_size=16) +  
  theme(axis.text.y = element_text(size=10, face = rep(c("bold",rep("plain",4)),2)),legend.position = "bottom") +
  scale_color_manual(values=c("Publications" = col1, "Top 5 Cash Crops" = col2, "Other Cash Crops" = col3, "Minerals"=col4),name="Treatment") + 
  scale_fill_manual(values=c("Publications" = col1, "Top 5 Cash Crops" = col2, "Other Cash Crops" = col3,"Minerals"=col4),name="Treatment") +
  scale_shape_manual(values=c("Publications" = 24, "Top 5 Cash Crops" = 22, "Other Cash Crops" = 25, "Minerals"=23),name="Treatment") 
p1 <-  p1 + guides(color=guide_legend(ncol=2,nrow=2,byrow=TRUE),
                 fill=guide_legend(ncol=2,nrow=2,byrow=TRUE),
                 shape=guide_legend(ncol=2,nrow=2,byrow=TRUE))
p1


p2 <- ggplot(plot.df2)
p2 <- p2 + geom_point(size=2.75,aes(x=beta,y=order,color=what,shape=what,fill=what)) + 
  geom_errorbarh(aes(x=beta,y=order, xmin=lb, xmax=ub,color=what), size=1, height=0.0) +
  geom_vline(xintercept=0,linetype="dotted", size=0.6) +
  #geom_text(aes(x=p.x,y=order,label=p.lab)) +
  scale_y_continuous(breaks=brks,minor_breaks = NULL, labels=lbs2,limits = c(0,2.5)) + 
  #scale_x_continuous(minor_breaks = seq(0 , 0.55, 0.05), breaks = seq(0, 0.55, 0.1)) +
  labs(x = "Coefficients and 95% Confidence Intervals", y = "Outcome & Treatment(s)",
       title="Cash Crops, Publications & Political Relevance",
       subtitle="Disaggregating Cash Crops") +
  #facet_wrap(~ outcome,nrow=1,scales="free_x")  +
  theme_minimal(base_size=16) +  
  theme(axis.text.y = element_text(size=10, face = rep(c("bold",rep("plain",4)),4)),legend.position = "bottom") +
  scale_color_manual(values=c("Publications" = col1, "Smallholder Cash Crops" = col2, "Plantation Cash Crops" = col3, "Minerals"=col4),name="Treatment") + 
  scale_fill_manual(values=c("Publications" = col1, "Smallholder Cash Crops" = col2, "Plantation Cash Crops" = col3,"Minerals"=col4),name="Treatment") +
  scale_shape_manual(values=c("Publications" = 24, "Smallholder Cash Crops" = 22, "Plantation Cash Crops" = 25, "Minerals"=23),name="Treatment") 
p2 <-  p2 + guides(color=guide_legend(ncol=2,nrow=2,byrow=TRUE),
                 fill=guide_legend(ncol=2,nrow=2,byrow=TRUE),
                 shape=guide_legend(ncol=2,nrow=2,byrow=TRUE))
p2

# combine with patchwork
p.out <- p1 | p2
#save
p.out + ggsave(paste0(fig.path,"wlms_figA33.pdf"),width=15,height=9)
