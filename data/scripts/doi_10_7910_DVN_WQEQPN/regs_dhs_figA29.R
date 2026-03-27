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



##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
##### Interact cont crops with publication bins
# get median publications in relevant analysis sample and code dichotomous low/high indicator
med.pub <- median(dhs$pubspc.23_poly[dhs$pubspc.23_poly>0 & dhs$LONGNUM!=0  & dhs$iso3c%in%sample_countries_hance & !is.na(dhs$exogamy_l11)],na.rm=T)
med.pub
dhs$pubs_low <- ifelse(dhs$pubspc.23_poly>0 & dhs$pubspc.23_poly<=med.pub,1,0)
dhs$pubs_high <- ifelse(dhs$pubspc.23_poly>0 & dhs$pubspc.23_poly>med.pub,1,0)


m.list.ia <- lapply(c(1:13), function(i){
  felm(as.formula(RegFor( y = yvars_std[i] , x = c("hance_crops5_sum_15km_std*pubs_low","hance_crops5_sum_15km_std*pubs_high", base.x) ,
                          FE = "country_survey_round" , 
                          IV="0", clust = "loc.id_pubs_poly")),
       data=dhs,subset=LONGNUM!=0 & iso3c%in%sample_countries_hance)
})


plot.df <- do.call(rbind,lapply(m.list.ia,function(m){
  rbind(
    deltaMethod(m,"hance_crops5_sum_15km_std"),
    deltaMethod(m,"hance_crops5_sum_15km_std + `hance_crops5_sum_15km_std:pubs_low`"),
    deltaMethod(m,"hance_crops5_sum_15km_std + `hance_crops5_sum_15km_std:pubs_high`")
  )
}))


ticks <- rev(seq(1,19.5,1.5))

plot.df$dv <- rep(c(paste0("Exogamy L",c(1:10)),paste0("Exogamy L","11-14"),paste0("Exogamy L",c(15:16))),each=3)
plot.df$what <- rep(c("None","Below Med.","Above Med."),13)

ord. <- c(ticks + 0.2,ticks,ticks-0.2)
plot.df$order <- ord.[order(ord.,decreasing=T)]

plot.df$what <- factor(plot.df$what,levels=c("None","Below Med.","Above Med."))


col1 <- wes_palette("Darjeeling1",5,"discrete")[2]
col2 <- wes_palette("Darjeeling1",5,"discrete")[3]
col3 <-wes_palette("Darjeeling1",15,"continuous")[2] 

names(plot.df)[c(1:4)] <- c("coef","se","CI_lower", "CI_upper")

# do the plot
p1 <- ggplot(plot.df)
p1 <- p1 + geom_point(size=2.25,aes(x=coef,y=order,color=what,shape=what,fill=what)) + 
  geom_errorbarh(aes(x=coef,y=order, xmin=CI_lower, xmax=CI_upper,color=what), size=1, height=0.0) +
  geom_vline(xintercept=0,linetype="dotted", size=0.6) +
  scale_y_continuous(breaks=ticks,minor_breaks = NULL, 
                     labels=c(paste0("Exogamy L",c(1:10)),"Exogamy L11-14",paste0("Exogamy L",c(15:16)))) + 
  #scale_x_continuous(minor_breaks = seq(0 , 0.55, 0.05), breaks = seq(0, 0.55, 0.1)) +
  labs(x = "Marginal Effect of Cash Crops", y = "Exogamy at Ethnologue Levels",
       title="Interactive Effects?",
       subtitle="Cash Crops conditional on Publications"
       #,caption="Cash crop value per sqkm in 1960 USD (Poly.)\nCash Crops (SAR)\nCash Crop Suitability (Poly)\nCash Crops (Instrumented with Suitability)"
  ) +
  theme_minimal(base_size=14) +  
  theme(axis.text.y = element_text(size=10),legend.position = "bottom") +
  scale_color_manual(values=c("None" = col1, "Below Med." = col2, "Above Med." = col3), name="Number of Publications") + 
  scale_fill_manual(values=c("None" = col1, "Below Med." = col2, "Above Med." = col3),name="Number of Publications") +
  scale_shape_manual(values=c("None" = 24, "Below Med." = 22, "Above Med." =25),name="Number of Publications")
p1




##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
##### Interact cont pubs with crop bins

med.crops <- median(dhs$hance_crops5_sum_15km[dhs$hance_crops5_sum_15km>0 & dhs$LONGNUM!=0  & 
                                                dhs$iso3c%in%sample_countries_hance & !is.na(dhs$exogamy_l11)],na.rm=T)
med.crops
dhs$crops_low <- ifelse(dhs$hance_crops5_sum_15km>0 & dhs$hance_crops5_sum_15km<=med.crops,1,0)
dhs$crops_high <- ifelse(dhs$hance_crops5_sum_15km>0 & dhs$hance_crops5_sum_15km>med.crops,1,0)


m.list.ia <- lapply(c(1:13), function(i){
  felm(as.formula(RegFor( y = yvars_std[i] , x = c("pubspc.23_poly_std*crops_low","pubspc.23_poly_std*crops_high", base.x) ,
                          FE = "country_survey_round" , 
                          IV="0", clust = "loc.id_pubs_poly")),
       data=dhs,subset=LONGNUM!=0 & iso3c%in%sample_countries_hance)
})


plot.df <- do.call(rbind,lapply(m.list.ia,function(m){
  rbind(
    deltaMethod(m,"pubspc.23_poly_std"),
    deltaMethod(m,"pubspc.23_poly_std + `pubspc.23_poly_std:crops_low`"),
    deltaMethod(m,"pubspc.23_poly_std + `pubspc.23_poly_std:crops_high`")
  )
}))


ticks <- rev(seq(1,19.5,1.5))

plot.df$dv <- rep(c(paste0("Exogamy L",c(1:10)),paste0("Exogamy L","11-14"),paste0("Exogamy L",c(15:16))),each=3)
plot.df$what <- rep(c("None","Below Med.","Above Med."),13)

ord. <- c(ticks + 0.2,ticks,ticks-0.2)
plot.df$order <- ord.[order(ord.,decreasing=T)]

plot.df$what <- factor(plot.df$what,levels=c("None","Below Med.","Above Med."))


col1 <- wes_palette("Darjeeling1",5,"discrete")[2]
col2 <- wes_palette("Darjeeling1",5,"discrete")[3]
col3 <-wes_palette("Darjeeling1",15,"continuous")[2] 

names(plot.df)[c(1:4)] <- c("coef","se","CI_lower", "CI_upper")

# do the plot
p2 <- ggplot(plot.df)
p2 <- p2 + geom_point(size=2.25,aes(x=coef,y=order,color=what,shape=what,fill=what)) + 
  geom_errorbarh(aes(x=coef,y=order, xmin=CI_lower, xmax=CI_upper,color=what), size=1, height=0.0) +
  geom_vline(xintercept=0,linetype="dotted", size=0.6) +
  scale_y_continuous(breaks=ticks,minor_breaks = NULL, 
                     labels=c(paste0("Exogamy L",c(1:10)),"Exogamy L11-14",paste0("Exogamy L",c(15:16)))) + 
  #scale_x_continuous(minor_breaks = seq(0 , 0.55, 0.05), breaks = seq(0, 0.55, 0.1)) +
  labs(x = "Marginal Effect of Pubs p.c.", y = "Exogamy at Ethnologue Levels",
       title="Interactive Effects?",
       subtitle="Publications conditional on Cash Crops"
       #,caption="Cash crop value per sqkm in 1960 USD (Poly.)\nCash Crops (SAR)\nCash Crop Suitability (Poly)\nCash Crops (Instrumented with Suitability)"
  ) +
  theme_minimal(base_size=14) +  
  theme(axis.text.y = element_text(size=10),legend.position = "bottom") +
  scale_color_manual(values=c("None" = col1, "Below Med." = col2, "Above Med." = col3), name="Cash Crops") + 
  scale_fill_manual(values=c("None" = col1, "Below Med." = col2, "Above Med." = col3),name="Cash Crops") +
  scale_shape_manual(values=c("None" = 24, "Below Med." = 22, "Above Med." =25),name="Cash Crops")
p2


# combine with patchwork
p.out <- p1 | p2
#save
p.out + ggsave(paste0(fig.path,"dhs_figA29.pdf"),width=14,height=7.5)
