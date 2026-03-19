### Baseline Analysis: geographic

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


#### #### #### #### #### #### #### #### #### #### #### #### 
#### #### Geographic Treatment: Cash crops within 15km, 1923 pubs in polygon
#### #### #### #### #### #### #### #### #### #### #### #### 
m.list <- lapply(yvars_std, function(dv){
  felm(as.formula(RegFor( y = dv , x = c("hance_crops5_sum_15km_std","pubspc.23_poly_std", base.x, "mrdk_precol_state") ,
                          FE = "country_survey_round " , 
                          IV="0", clust = "loc.id_pubs_poly")),
       data=dhs, subset=iso3c%in%sample_countries_hance)
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


# do the plot
p <- ggplot(out.df)
p <- p + geom_point(size=2.75,aes(x=beta,y=order,color=what,shape=what,fill=what)) + 
  geom_errorbarh(aes(x=beta,y=order, xmin=lb, xmax=ub,color=what), size=1, height=0.0) +
  geom_vline(xintercept=0,linetype="dotted", size=0.6) +
  scale_y_continuous(breaks=c(13:1),minor_breaks = NULL, 
                     labels=c(paste0("Exogamy L",c(1:10)),"Exogamy L11-14",paste0("Exogamy L",c(15:16))),
                     limits = c(0.6,13.4)) + 
  #scale_x_continuous(minor_breaks = seq(0 , 0.55, 0.05), breaks = seq(0, 0.55, 0.1)) +
  labs(x = "Coefficients and 95% Confidence Intervals", y = "Exogamy at Ethnologue Levels",
       title="Cash Crops, Publications & Inter-Ethnic Marriages",
       subtitle="Controlling for precolonial statehood",
       caption="Cash crop value per sqkm in 1960 USD (within 15km)\n1923 Publications per capita (WLMS Polygon)") +
  theme_minimal(base_size = 14) +
  theme(axis.text.y = element_text(size=10),legend.position = "bottom") +
  scale_color_manual(values=c("Cash Crops" = col1, "Publications" = col2),name="Treatment") + 
  scale_fill_manual(values=c("Cash Crops" = col1, "Publications" = col2),name="Treatment") +
  scale_shape_manual(values=c("Cash Crops" = 24, "Publications" = 25),name="Treatment")
p
p + ggsave(paste0(fig.path,"dhs_figA18.pdf"),width=7,height=8)
