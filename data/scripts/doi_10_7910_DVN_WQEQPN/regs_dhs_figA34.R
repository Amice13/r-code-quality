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


######### Big 5 vs rest regardless of mode of production
m.list<- lapply(yvars_std, function(dv){
  felm(as.formula(RegFor( y = dv , x = c("pubspc.23_poly_std","hance_crops5_sum_15km_std","crops_other_sum_15km_std","hance_minerals_sum_15km_std", 
                                         base.x) ,
                          FE = "country_survey_round" , 
                          IV="0", clust = "loc.id_pubs_poly")),
       data=dhs,subset=LONGNUM!=0 & iso3c%in%sample_countries_hance)
})


#### prepare regression results for visual output
out.ls <- lapply(m.list, function(x){
  get_plot_data(x,coef_no=c(1:4))
})
out.df <- do.call(rbind,out.ls)
out.df$dv <- c(rep(paste0("Exogamy L",c(1:10)),each=4),rep(paste0("Exogamy L","11-14"),each=4),rep(paste0("Exogamy L",c(15:16)),each=4))
ticks <- seq(19.5,1,-1.5)
ord. <- c(c(13:1)+0.3,c(13:1)+0.1,c(13:1)-0.1,c(13:1)-0.3)
out.df$order <- ord.[order(ord.,decreasing=T)]
out.df$what <- factor(rep(c("Publications", "Top 5 Cash Crops","Other Cash Crops","Minerals"),13))

col1 <- wes_palette("Darjeeling1",5,"discrete")[2]
col2 <- wes_palette("Darjeeling1",5,"discrete")[3]
col3 <- wes_palette("Darjeeling1",5,"discrete")[5]
col4 <- wes_palette("Darjeeling1",15,"continuous")[2]



out.df$what <- factor(out.df$what,levels=c("Publications", "Top 5 Cash Crops","Other Cash Crops","Minerals"))
unique(hance$resource)

dhs$palm_dummy <- ifelse(dhs$palm_sum_15km>0,1,0)
table(dhs$palm_dummy,dhs$iso3c)

# do the plot from both models
p1 <- ggplot(out.df)
p1 <- p1 + geom_point(size=2.25,aes(x=beta,y=order,color=what,shape=what,fill=what)) +
  geom_errorbarh(aes(x=beta,y=order, xmin=lb, xmax=ub,color=what), size=1, height=0.0) +
  geom_vline(xintercept=0,linetype="dotted", size=0.6) +
  scale_y_continuous(breaks=rev(c(1:13)),minor_breaks = NULL,
                     labels=c(paste0("Exogamy L",c(1:10)),"Exogamy L11-14",paste0("Exogamy L",c(15:16)))) +
  #scale_x_continuous(minor_breaks = seq(0 , 0.55, 0.05), breaks = seq(0, 0.55, 0.1)) +
  labs(x = "Coefficients and 95% Confidence Intervals", y = "Exogamy at Ethnologue Levels",
       title="Cash Crops, Publications & Inter-Ethnic Marriages",
       subtitle="Geographic Specifications"
       #,caption="Cash crop value per sqkm in 1960 USD (Poly.)\nCash Crops (SAR)\nCash Crop Suitability (Poly)\nCash Crops (Instrumented with Suitability)"
  ) +
  theme_minimal(base_size=14) +
  theme(axis.text.y = element_text(size=10),legend.position = "bottom") +
  scale_color_manual(values=c("Publications" = col2, "Top 5 Cash Crops" = col1, "Other Cash Crops" =col4, "Minerals" =col3),name="Treatment") +
  scale_fill_manual(values=c("Publications" = col2, "Top 5 Cash Crops" = col1, "Other Cash Crops" =col4, "Minerals" =col3),name="Treatment") +
  scale_shape_manual(values=c("Publications" = 24, "Top 5 Cash Crops" = 22, "Other Cash Crops" = 25, "Minerals" =23),name="Treatment")
p1 <- p1 + guides(color=guide_legend(ncol=2,nrow=2,byrow=TRUE),
                fill=guide_legend(ncol=2,nrow=2,byrow=TRUE),
                shape=guide_legend(ncol=2,nrow=2,byrow=TRUE))
p1




######## Smallholder versus Plantation Crops
m.list <- lapply(yvars_std, function(dv){
  felm(as.formula(RegFor( y = dv , x = c("pubspc.23_poly_std","hance_crops5_sum_15km_std","crops_sum_15_km_eur_std","hance_minerals_sum_15km_std", 
                                         base.x) ,
                          FE = "country_survey_round" , 
                          IV="0", clust = "loc.id_pubs_poly")),
       data=dhs,subset=LONGNUM!=0 & iso3c%in%sample_countries_hance)
})

#### prepare regression results for visual output
out.ls <- lapply(m.list, function(x){
  get_plot_data(x,coef_no=c(1:4))
})
out.df <- do.call(rbind,out.ls)
out.df$dv <- c(rep(paste0("Exogamy L",c(1:10)),each=4),rep(paste0("Exogamy L","11-14"),each=4),rep(paste0("Exogamy L",c(15:16)),each=4))
ticks <- seq(19.5,1,-1.5)
ord. <- c(c(13:1)+0.3,c(13:1)+0.1,c(13:1)-0.1,c(13:1)-0.3)
out.df$order <- ord.[order(ord.,decreasing=T)]
out.df$what <- factor(rep(c("Publications", "Smallholder Cash Crops","Plantation Cash Crops","Minerals"),13))

col1 <- wes_palette("Darjeeling1",5,"discrete")[2]
col2 <- wes_palette("Darjeeling1",5,"discrete")[3]
col3 <- wes_palette("Darjeeling1",5,"discrete")[5]
col4 <- wes_palette("Darjeeling1",15,"continuous")[2]



out.df$what <- factor(out.df$what,levels=c("Publications", "Smallholder Cash Crops","Plantation Cash Crops","Minerals"))

# do the plot from both models
p2 <- ggplot(out.df)
p2 <- p2 + geom_point(size=2.25,aes(x=beta,y=order,color=what,shape=what,fill=what)) +
  geom_errorbarh(aes(x=beta,y=order, xmin=lb, xmax=ub,color=what), size=1, height=0.0) +
  geom_vline(xintercept=0,linetype="dotted", size=0.6) +
  scale_y_continuous(breaks=rev(c(1:13)),minor_breaks = NULL,
                     labels=c(paste0("Exogamy L",c(1:10)),"Exogamy L11-14",paste0("Exogamy L",c(15:16)))) +
  #scale_x_continuous(minor_breaks = seq(0 , 0.55, 0.05), breaks = seq(0, 0.55, 0.1)) +
  labs(x = "Coefficients and 95% Confidence Intervals", y = "Exogamy at Ethnologue Levels",
       title="Cash Crops, Publications & Inter-Ethnic Marriages",
       subtitle="Geographic Specifications"
       #,caption="Cash crop value per sqkm in 1960 USD (Poly.)\nCash Crops (SAR)\nCash Crop Suitability (Poly)\nCash Crops (Instrumented with Suitability)"
  ) +
  theme_minimal(base_size=14) +
  theme(axis.text.y = element_text(size=10),legend.position = "bottom") +
  scale_color_manual(values=c("Publications" = col2, "Smallholder Cash Crops" = col1, "Plantation Cash Crops" =col4, "Minerals" =col3),name="Treatment") +
  scale_fill_manual(values=c("Publications" = col2, "Smallholder Cash Crops" = col1, "Plantation Cash Crops" =col4, "Minerals" =col3),name="Treatment") +
  scale_shape_manual(values=c("Publications" = 24, "Smallholder Cash Crops" = 22, "Plantation Cash Crops" = 25, "Minerals" =23),name="Treatment")
p2 <- p2 + guides(color=guide_legend(ncol=2,nrow=2,byrow=TRUE),
                fill=guide_legend(ncol=2,nrow=2,byrow=TRUE),
                shape=guide_legend(ncol=2,nrow=2,byrow=TRUE))
p2


# combine with patchwork
p.out <- p1 | p2
#save
p.out + ggsave(paste0(fig.path,"dhs_figA34.pdf"),width=16,height=9)
