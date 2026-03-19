#### #### #### #### #### #### #### #### #### #### #### #### 
# Ethnic Treatments
#### #### #### #### #### #### #### #### #### #### #### #### 
source("regression_functions.R")
fig.path <- "results/"
source("dhs/dhs_standardize_fun.R")


### define outcomes and control variables
yvars<- paste0("exogamy_l",c(1:11,15:16)) ## 
yvars_std <- paste0(yvars,"_std")
controls.hist <- c("popc_mean_1720_1890_mean_ethn_log", "popc_mean_1720_1890_mean_ethn.m_log")
controls.ind <- c("age","age.sq","age.m","age.m.sq")
base.x <-  c(controls.ind,controls.hist)

### define analysis countries
sample_countries_hance <- unique(dhs$iso3c[dhs$sample_country_hance==1])

###### prepare datasets and standardize variables 

### subset to non-missings on 1923 pubs & redefine standardized vars
dhs.ethn.23<- dhs[which(!is.na(dhs$hance_crops5_sqkm_ethn_std.m) & !is.na(dhs$pubspc.23_ethn_std.m) & 
                          !is.na(dhs$hance_crops5_sqkm_ethn_std) & !is.na(dhs$pubspc.23_ethn_std) & 
                          dhs$LONGNUM!=0 & dhs$iso3c%in%sample_countries_hance),]   

#drop previously defined standardize variables
dhs.ethn.23 <- standardize_dhs(dhs.ethn.23)




#### #### #### #### #### #### #### #### #### #### #### #### 
#### #### Ethnic Treatment: 1923 pubs & hance crops ethnically defined
#### #### #### #### #### #### #### #### #### #### #### #### 
m.list <- lapply(yvars_std, function(dv){
  felm(as.formula(RegFor( y = dv , x = c("hance_crops5_sqkm_ethn_std",
                                         "hance_crops5_sqkm_ethn_std.m",
                                         "pubspc.23_ethn_std", 
                                         "pubspc.23_ethn_std.m",
                                         base.x) ,
                          FE = "loc.id_pubs_poly" , 
                          IV="0", clust = "loc.id_pubs_poly")),
       data=dhs.ethn.23,subset=LONGNUM!=0 & iso3c%in%sample_countries_hance)
})


out.ls <- lapply(m.list, function(x){
  get_plot_data(x,coef_no=c(2,4))
})
out.df <- do.call(rbind,out.ls)
out.df$dv <- c(rep(paste0("Exogamy L",c(1:10)),each=2),rep(paste0("Exogamy L","11-14"),each=2),rep(paste0("Exogamy L",c(15:16)),each=2))
ord. <- c(c(13:1)+0.2,c(13:1)-0.2)
out.df$order <- ord.[order(ord.,decreasing=T)]
out.df$what <- factor(rep(c("Cash Crops","Publications"),13))


col1 <- wes_palette("Zissou1",20,"continuous")[5]
col2 <- wes_palette("Zissou1",20,"continuous")[18]

# do the plot
p1 <- ggplot(out.df)
p1 <- p1 + geom_point(size=2.75,aes(x=beta,y=order,color=what,shape=what,fill=what)) + 
  geom_errorbarh(aes(x=beta,y=order, xmin=lb, xmax=ub,color=what), size=1, height=0.0) +
  geom_vline(xintercept=0,linetype="dotted", size=0.6) +
  scale_y_continuous(breaks=c(13:1),minor_breaks = NULL, labels=c(paste0("Exogamy L",c(1:10)),"Exogamy L11-14",paste0("Exogamy L",c(15:16))),limits = c(0.6,13.4)) + 
  #scale_x_continuous(minor_breaks = seq(0 , 0.55, 0.05), breaks = seq(0, 0.55, 0.1)) +
  labs(x = "Coefficients and 95% Confidence Intervals", y = "Exogamy at Ethnologue Levels",
       title="Cash Crops, Publications & Inter-Ethnic Marriages",
       subtitle="Treatment defined via husband's ethnic group",
       caption="Cash crop value per sqkm in 1960 USD (WLMS Poly. matched to repondent's group)\n1923 Publications per capita (respondent's ethnic group)") +
  theme_minimal(base_size=14) +  
  theme(axis.text.y = element_text(size=10),legend.position = "bottom") +
  scale_color_manual(values=c("Cash Crops" = col1, "Publications" = col2),name="Treatment") + 
  scale_fill_manual(values=c("Cash Crops" = col1, "Publications" = col2),name="Treatment") +
  scale_shape_manual(values=c("Cash Crops" = 24, "Publications" = 25),name="Treatment")
p1





#### #### #### #### #### #### #### #### #### #### #### #### 
# Ethnic Treatments: Leavers Only
#### #### #### #### #### #### #### #### #### #### #### #### 
source("regression_functions.R")
fig.path <- "results/"
source("dhs/dhs_standardize_fun.R")


### define outcomes and control variables
yvars<- paste0("exogamy_l",c(1:11,15:16)) ## 
yvars_std <- paste0(yvars,"_std")
controls.hist <- c("popc_mean_1720_1890_mean_ethn_log", "popc_mean_1720_1890_mean_ethn.m_log")
controls.ind <- c("age","age.sq","age.m","age.m.sq")
base.x <-  c(controls.ind,controls.hist)

### define analysis countries
sample_countries_hance <- unique(dhs$iso3c[dhs$sample_country_hance==1])

###### prepare datasets and restandardize variables in subsample
dhs.leavers.23.m<- dhs[which(dhs$stayer_wlms.m==0 & 
                               !is.na(dhs$hance_crops5_sqkm_ethn_std.m) & !is.na(dhs$pubspc.23_ethn_std.m) & 
                               dhs$LONGNUM!=0 & dhs$iso3c%in%sample_countries_hance),]
dhs.leavers.23.m <- standardize_dhs(dhs.leavers.23.m)



#### Male treatment and leavers
m.list<- lapply(yvars_std, function(dv){
  felm(as.formula(RegFor( y = dv , x = c("hance_crops5_sqkm_ethn_std.m",
                                         "pubspc.23_ethn_std.m",
                                         "hance_crops5_sqkm_ethn_std",
                                         "pubspc.23_ethn_std",
                                         base.x) ,
                          FE = "loc.id_pubs_poly" , 
                          IV="0", clust = "loc.id_pubs_poly")),
       data=dhs.leavers.23.m,subset=LONGNUM!=0 & iso3c%in%sample_countries_hance & stayer_wlms.m==0)
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


col1 <- wes_palette("Zissou1",20,"continuous")[5]
col2 <- wes_palette("Zissou1",20,"continuous")[18]

# do the plot
p2 <- ggplot(out.df)
p2 <- p2 + geom_point(size=2.75,aes(x=beta,y=order,color=what,shape=what,fill=what)) + 
  geom_errorbarh(aes(x=beta,y=order, xmin=lb, xmax=ub,color=what), size=1, height=0.0) +
  geom_vline(xintercept=0,linetype="dotted", size=0.6) +
  scale_y_continuous(breaks=c(13:1),minor_breaks = NULL, labels=c(paste0("Exogamy L",c(1:10)),"Exogamy L11-14",paste0("Exogamy L",c(15:16))),limits = c(0.6,13.4)) + 
  #scale_x_continuous(minor_breaks = seq(0 , 0.55, 0.05), breaks = seq(0, 0.55, 0.1)) +
  labs(x = "Coefficients and 95% Confidence Intervals", y = "Exogamy at Ethnologue Levels",
       title="Cash Crops, Publications & Inter-Ethnic Marriages",
       subtitle="Treatment defined by husband's ethnic group; male movers only",
       caption="Cash crop value per sqkm in 1960 USD (WLMS Poly. matched to repondent's group)\n1923 Publications per capita (respondent's ethnic group)") +
  theme_minimal(base_size=14) +  
  theme(axis.text.y = element_text(size=10),legend.position = "bottom") +
  scale_color_manual(values=c("Cash Crops" = col1, "Publications" = col2),name="Treatment") + 
  scale_fill_manual(values=c("Cash Crops" = col1, "Publications" = col2),name="Treatment") +
  scale_shape_manual(values=c("Cash Crops" = 24, "Publications" = 25),name="Treatment")
p2


# combine with patchwork
p.out <- p1 | p2
#save
p.out + ggsave(paste0(fig.path,"dhs_fig4.pdf"),width=14,height=7.5)

