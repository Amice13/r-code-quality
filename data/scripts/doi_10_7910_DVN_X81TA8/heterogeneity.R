#### heterogeneity



data.g25$hance_stimulants_dummy <- ifelse(data.g25$hance_stimulants_sum!=0,1,0)
data.g25$hance_industrial_dummy <- ifelse(data.g25$hance_industrial_crops_sum!=0,1,0)
data.g25$hance_food_dummy <- ifelse(data.g25$hance_food_sum!=0,1,0)
data.g25$hance_oil_dummy <- ifelse(data.g25$hance_oils_sum!=0,1,0)


out.vars <- c("road_dens_2015_log","urb_dens_2015_log","nl_2015_pc_log","hh_wealth")
m.list <- lapply(out.vars,function(y){
  out25 <- mdlr(y=y, x=c("hance_cocoa_dummy","hance_coffee_dummy","hance_palm_dummy", "hance_cotton_dummy", "hance_groundnut_dummy",
                         "hance_minerals_dummy"), cntr=controls.std, fe="iso3c_col", cl="iso3c_col", dat = data.g25)
  return(out25)
})
summary(m.list[[4]])


#### get coefficients for all models
labs <- c("Cocoa", "Coffee", "Palm", "Cotton", "Groundnut","Minerals")
out.labs <- c("Road Density 1998 (log)","Urb. Pop. Dens. 2015 (log)","Night Lights 2015 pc (log)","DHS Household Wealth")


plot.df <- do.call(rbind,lapply(m.list,function(x){
  out <- get_plot_data(x,coef_no = c(1:6))
}))

plot.df$outcome <- rep(out.labs, each=6)
plot.df$outcome <- factor(plot.df$outcome,levels=out.labs)
plot.df$order <- rep(c(6:1),4)

# do the plot
p <- ggplot(plot.df)
p <- p + geom_point(size=2.75,aes(x=beta,y=order),shape=19) + 
  geom_errorbarh(aes(x=beta,y=order, xmin=lb, xmax=ub), size=1, height=0.0) +
  geom_vline(xintercept=0,linetype="dotted", size=0.6) +
  scale_y_continuous(breaks=c(length(labs):1),minor_breaks = NULL, labels=labs,limits = c(0.8,length(labs)+0.2)) + 
  labs(x = "Coefficients and 95% Confidence Intervals", y = "Cash Crop Dummy",
       title="Disaggregating Cash Crops",
       subtitle="Results for the five most important agricultural exports") +
  facet_wrap(~ outcome,nrow=1,scales="free_x")  +
  theme_minimal(base_size=16) +  
  theme(axis.text.y = element_text(size=10))
p
p + ggsave(paste0("output/tables_wd/","het_crop_by_crop.pdf"),width=16,height=6)







#### heterogeneity by imperial power
out.vars <- c("road_dens_2015_log","urb_dens_2015_log","nl_2015_pc_log","hh_wealth")
out.labs <- c("Road Density 1998 (log)","Urb. Pop. Dens. 2015 (log)","Night Lights 2015 pc (log)","DHS Household Wealth")

### estimate baseline models
m.list <- lapply(out.vars, function(y){
  crops_br <- mdlr(y=y, x=c("hance_crops_dummy","hance_minerals_dummy"), cntr=controls.std, fe="iso3c_col", cl="iso3c_col", dat = data.g25[data.g25$empire=="Britain",])
  crops_fr <- mdlr(y=y, x=c("hance_crops_dummy","hance_minerals_dummy"), cntr=controls.std, fe="iso3c_col", cl="iso3c_col", dat = data.g25[data.g25$empire=="France",])
  crops_pt <- mdlr(y=y, x=c("hance_crops_dummy","hance_minerals_dummy"), cntr=controls.std, fe="iso3c_col", cl="iso3c_col", dat = data.g25[data.g25$empire=="Portugal",])
  crops_be <- mdlr(y=y, x=c("hance_crops_dummy","hance_minerals_dummy"), cntr=controls.std, fe="iso3c_col", cl="iso3c_col", dat = data.g25[data.g25$empire=="Belgium",])
  out <- list(crops_br,crops_fr,crops_pt,crops_be)
  return(out)  
})


#### prepare regression results for visual output

#### get last coefficient for all models
labs <- c("Britain", "France", "Portugal", "Belgium")
out.labs <- c("Quality Road 1998 (Y/N)","Night Lights 2015 (Y/N)","City 2015 (Y/N)","DHS Household Wealth")


plot.ls <- lapply(c(1:length(out.vars)),function(i){
  out.ls <- m.list[[i]]
  out.ls <- lapply(out.ls, function(x){
    get_plot_data(x,coef_no=c(1,2))
  })
  out.df <- do.call(rbind,out.ls)
  out.df$model <- rep(labs,each=2)
  out.df$outcome <- out.labs[i]
  return(out.df)
})

plot.df <- do.call(rbind,plot.ls)
plot.df$outcome <- factor(plot.df$outcome,levels=out.labs)
plot.df.crops <- plot.df[grep("crops",rownames(plot.df)),]
plot.df.minerals<- plot.df[grep("minerals",rownames(plot.df)),]
plot.df.crops$order <- plot.df.minerals$order <- rep(c(4:1),4)

# do the plots
p <- ggplot(plot.df.crops)
p <- p + geom_point(size=2.75,aes(x=beta,y=order),shape=19) + 
  geom_errorbarh(aes(x=beta,y=order, xmin=lb, xmax=ub), size=1, height=0.0) +
  geom_vline(xintercept=0,linetype="dotted", size=0.6) +
  scale_y_continuous(breaks=c(length(labs):1),minor_breaks = NULL, labels=labs,limits = c(0.8,length(labs)+0.2)) + 
  labs(x = "Coefficients and 95% Confidence Intervals", y = "Colonizer",
       title="Heterogeneity Across Imperial Powers",
       subtitle="Baseline Models with Imperial Subsamples: Cash Crop Coefficients") +
  facet_wrap(~ outcome,nrow=1,scales="free_x")  +
  theme_minimal(base_size=16) +  
  theme(axis.text.y = element_text(size=10)) 
p
p + ggsave(paste0("output/tables_wd/","het_empires_crops.pdf"),width=16,height=6)



p <- ggplot(plot.df.minerals)
p <- p + geom_point(size=2.75,aes(x=beta,y=order),shape=19) + 
  geom_errorbarh(aes(x=beta,y=order, xmin=lb, xmax=ub), size=1, height=0.0) +
  geom_vline(xintercept=0,linetype="dotted", size=0.6) +
  scale_y_continuous(breaks=c(length(labs):1),minor_breaks = NULL, labels=labs,limits = c(0.8,length(labs)+0.2)) + 
  labs(x = "Coefficients and 95% Confidence Intervals", y = "Colonizer",
       title="Heterogeneity Across Imperial Powers",
       subtitle="Baseline Models with Imperial Subsamples: Mineral Coefficients") +
  facet_wrap(~ outcome,nrow=1,scales="free_x")  +
  theme_minimal(base_size=16) +  
  theme(axis.text.y = element_text(size=10)) 
p
p + ggsave(paste0("output/tables_wd/","het_empires_minerals.pdf"),width=16,height=6)






###############################################################
######################################################################
###############################################################
####### country-by-country regressions: crops
crop_countries <- unique(data.g25$iso3c_col[data.g25$hance_crops_dummy==1])
crop_countries_dhs <- unique(data.g25$iso3c_col[data.g25$hance_crops_dummy==1 & !is.na(data.g25$hh_wealth)])


m.list.nl <- lapply(crop_countries,function(cntr){
  out <- mdlr(y="nl_2015_pc_log", x=c("hance_crops_dummy","hance_minerals_dummy"), cntr=controls.std[c(1:10,18:21)], fe="0", cl="cl9", dat = data.g25[data.g25[,"iso3c_col"]==cntr,])
})

out.ls <- lapply(m.list.nl, function(x){
  get_plot_data(x,coef_no="hance_crops_dummy")
})
out.df <- do.call(rbind,out.ls)
out.df$country <- crop_countries
out.df <- out.df[order(-out.df$beta),]
out.df <- out.df[out.df$country!="GMB",]
out.df$order <- rev(c(1:nrow(out.df)))

p <- ggplot(out.df)
p.nl <- p + geom_point(size=2.75,aes(x=beta,y=order),shape=19) + 
  geom_errorbarh(aes(x=beta,y=order, xmin=lb, xmax=ub), size=1, height=0.0) +
  geom_vline(xintercept=0,linetype="dotted", size=0.6) +
  scale_y_continuous(breaks=c(nrow(out.df):1),minor_breaks = NULL, labels=out.df$country,limits = c(1,31)) + 
  #scale_x_continuous(minor_breaks = seq(0 , 0.55, 0.05), breaks = seq(0, 0.55, 0.1)) +
  labs(x = "Coefficients and 95% Confidence Intervals", y = "Country",
       title="Night Lights 2015 pc (log)")  +
  theme_minimal(base_size=12) +  
  theme(axis.text.y = element_text(size=10)) 
p.nl
#p.nl + ggsave(paste0("output/tables_wd/","countries_nl2015.pdf"),width=8,height=8)


m.list.roads <- lapply(crop_countries,function(cntr){
  out <- mdlr(y="road_dens_2015_log", x=c("hance_crops_dummy","hance_minerals_dummy"), cntr=controls.std[c(1:10,18:21)], fe="0", cl="cl9", dat = data.g25[data.g25[,"iso3c_col"]==cntr,])
})

out.ls <- lapply(m.list.roads, function(x){
  get_plot_data(x,coef_no="hance_crops_dummy")
})
out.df <- do.call(rbind,out.ls)
out.df$country <- crop_countries
out.df <- out.df[order(-out.df$beta),]
out.df <- out.df[out.df$country!="GMB",]
out.df$order <- rev(c(1:nrow(out.df)))

p <- ggplot(out.df)
p.roads <- p + geom_point(size=2.75,aes(x=beta,y=order),shape=19) + 
  geom_errorbarh(aes(x=beta,y=order, xmin=lb, xmax=ub), size=1, height=0.0) +
  geom_vline(xintercept=0,linetype="dotted", size=0.6) +
  scale_y_continuous(breaks=c(nrow(out.df):1),minor_breaks = NULL, labels=out.df$country,limits = c(1,31)) + 
  #scale_x_continuous(minor_breaks = seq(0 , 0.55, 0.05), breaks = seq(0, 0.55, 0.1)) +
  labs(x = "Coefficients and 95% Confidence Intervals", y = "Country",
       title="Road Density 1998 (log)")  +
  theme_minimal(base_size=12) +  
  theme(axis.text.y = element_text(size=10)) 
p.roads
#p.roads + ggsave(paste0("output/tables_wd/","countries_roads_1998.pdf"),width=8,height=8)


m.list.cities <- lapply(crop_countries,function(cntr){
  out <- mdlr(y="urb_dens_2015_log", x=c("hance_crops_dummy","hance_minerals_dummy"), cntr=controls.std[c(1:10,18:21)], fe="0", cl="cl9", dat = data.g25[data.g25[,"iso3c_col"]==cntr,])
})

out.ls <- lapply(m.list.cities, function(x){
  get_plot_data(x,coef_no="hance_crops_dummy")
})
out.df <- do.call(rbind,out.ls)
out.df$country <- crop_countries
out.df <- out.df[order(-out.df$beta),]
out.df <- out.df[out.df$country!="GMB",]
out.df$order <- rev(c(1:nrow(out.df)))

p <- ggplot(out.df)
p.cities <- p + geom_point(size=2.75,aes(x=beta,y=order),shape=19) + 
  geom_errorbarh(aes(x=beta,y=order, xmin=lb, xmax=ub), size=1, height=0.0) +
  geom_vline(xintercept=0,linetype="dotted", size=0.6) +
  scale_y_continuous(breaks=c(nrow(out.df):1),minor_breaks = NULL, labels=out.df$country,limits = c(1,31)) + 
  #scale_x_continuous(minor_breaks = seq(0 , 0.55, 0.05), breaks = seq(0, 0.55, 0.1)) +
  labs(x = "Coefficients and 95% Confidence Intervals", y = "Country",
       title="Urban Population Density 2015 (log)")  +
  theme_minimal(base_size=12) +  
  theme(axis.text.y = element_text(size=10)) 
p.cities
#p.cities + ggsave(paste0("output/tables_wd/","countries_cities_2015.pdf"),width=8,height=8)


m.list.wealth <- lapply(crop_countries_dhs,function(cntr){
  out <- mdlr(y="hh_wealth", x=c("hance_crops_dummy","hance_minerals_dummy"), cntr=controls.std[c(1:10,18:21)], fe="0", cl="cl9", dat = data.g25[data.g25[,"iso3c_col"]==cntr,])
})

out.ls <- lapply(m.list.wealth, function(x){
  get_plot_data(x,coef_no="hance_crops_dummy")
})
out.df <- do.call(rbind,out.ls)
out.df$country <- crop_countries_dhs
out.df <- out.df[order(-out.df$beta),]
out.df <- out.df[!is.na(out.df$se),]
out.df$order <- rev(c(1:nrow(out.df)))

p <- ggplot(out.df)
p.wealth <- p + geom_point(size=2.75,aes(x=beta,y=order),shape=19) + 
  geom_errorbarh(aes(x=beta,y=order, xmin=lb, xmax=ub), size=1, height=0.0) +
  geom_vline(xintercept=0,linetype="dotted", size=0.6) +
  scale_y_continuous(breaks=c(nrow(out.df):1),minor_breaks = NULL, labels=out.df$country,limits = c(1,nrow(out.df))) + 
  #scale_x_continuous(minor_breaks = seq(0 , 0.55, 0.05), breaks = seq(0, 0.55, 0.1)) +
  labs(x = "Coefficients and 95% Confidence Intervals", y = "Country",
       title="DHS Household Wealth")  +
  theme_minimal(base_size=12) +  
  theme(axis.text.y = element_text(size=10)) 
p.wealth
#p.wealth + ggsave(paste0("output/tables_wd/","countries_wealth.pdf"),width=8,height=8)

library(patchwork)
p.out <- (p.roads | p.cities)/
  (p.nl | p.wealth) 

p.out + plot_annotation(title="Cash Crop Effects by Country",
                        subtitle="Baseline regressions with country-specific subsamples",
                        theme = theme(plot.title = element_text(size = 20),
                        plot.subtitle = element_text(size = 16)))
                   
p.out + ggsave(paste0("output/tables_wd/","countries_all.pdf"),width=10,height=12)


##### Hetreogeneity analysis with interactions
mod <- read.csv("data/hance_moderators.csv",stringsAsFactors = F)
names(mod)

library(countrycode)
mod$iso3c <- countrycode(mod$cowcode,"cown","iso3c")

mod <- rbind(mod,mod[mod$iso3c=="SDN",])
mod$iso3c[nrow(mod)] <- "SSD"


crop_countries <- unique(data.g25$iso3c[data.g25$hance_crops_dummy!=0])

mod <- mod[mod$iso3c%in%hance_countries,]

mod$polity_iv_score_terciles <- as.numeric(cut(mod$polity_iv_score,
                                               breaks=quantile(mod$polity_iv_score,seq(0,1,1/3),na.rm=T)))

mod$vdem_polyarchy_terciles <- as.numeric(cut(mod$vdem_polyarchy_index,
                                              breaks=quantile(mod$vdem_polyarchy_index,seq(0,1,1/3),na.rm=T)))

mod$incidence_all_terciles <- as.numeric(cut(mod$incidence_all,
                                             breaks=quantile(mod$incidence_all,seq(0,1,1/3),na.rm=T)))

mod$incidence_gov_terciles <- as.numeric(cut(mod$incidence_gov,
                                             breaks=quantile(mod$incidence_gov,seq(0,1,1/3),na.rm=T)))

mod$incidence_terciles_mining <- as.numeric(cut(mod$incidence_all,
                                                breaks=quantile(mod$incidence_all[mod$iso3c%in%mineral_countries],seq(0,1,1/3),na.rm=T)))

mod$exp_agr_terciles <- as.numeric(cut(mod$wdi_expaggrw,
                                       breaks=quantile(mod$wdi_expaggrw,seq(0,1,1/3),na.rm=T)))

mod$exp_min_terciles <- as.numeric(cut(mod$wdi_expom,
                                       breaks=quantile(mod$wdi_expom,seq(0,1,1/3),na.rm=T)))

data.g25 <- left_join(data.g25,mod,by="iso3c")
names(data.g25)
describe(data.g25$polity_iv_score_terciles)

data.g25$landlocked <- ifelse(data.g25$country%in%c("Mali", "Burkina Faso", "Chad", "Niger"," Central
                                                                  African Republic", "Ethiopia", "Burundi", "Rwanda-Burundi", 
                                                              "Malawi", "Zambia", "Zimbabwe", "Botswana", "Lesotho", 
                                                              "Swaziland",
                                                              "Uganda","South Sudan"),1,0)
data.g25$landlocked2 <- data.g25$landlocked
data.g25$landlocked2[data.g25$iso3c=="COD"] <- 1



moderators <- c("polity_iv_score_terciles", "vdem_polyarchy_terciles", "incidence_all_terciles", "exp_agr_terciles", "landlocked")

out.vars <- c("road_dens_2015_log","urb_dens_2015_log","nl_2015_pc_log","hh_wealth")
out.labs <- c("Road Density 1998 (log)","Urb. Pop. Dens. 2015 (log)","Night Lights 2015 pc (log)","DHS Household Wealth")

#### Crops
out.ls <- lapply(c(1:length(out.vars)),function(i){
  ia.list <- list()
  for(mod in moderators){
    control.str <- paste(controls.std, collapse = " + ")
    form.str <- paste(out.vars[i], "~" , control.str, "+hance_crops_dummy +",
                      "hance_crops_dummy:factor(", mod, ") ", 
                      "| iso3c_col | 0 |iso3c_col")
    form <- as.formula(form.str)
    
    m <- felm(form, data=data.g25)
    ia.list[[mod]] <- m
  }
  
  
  het.list <- lapply(ia.list[c(1:4)],function(m){
    beta <- as.vector(m$beta)
    names(beta) <- paste0("b",c(1:length(coef(m))))
    
    est <- c(beta[22],deltaMethod(beta,"b22 + b23",vcov.=m$clustervcv)$Estimate,deltaMethod(beta,"b22 + b24",vcov.=m$clustervcv)$Estimate)
    se <- c(sqrt(diag(m$clustervcv)[22]),deltaMethod(beta,"b22 + b23",vcov.=m$clustervcv)$SE,deltaMethod(beta,"b22 + b24",vcov.=m$clustervcv)$SE)
    
    het <- data.frame(beta=est, se=se, lb=NA, ub=NA)
    het$lb=het$beta -1.96*het$se
    het$ub=het$beta +1.96*het$se
    return(het)
  })
  
  het.coef <- do.call(rbind,het.list)
  
  ## add landlocked
  m <- ia.list[[5]]
  beta <- as.vector(m$beta)
  names(beta) <- paste0("b",c(1:length(coef(m))))
  
  est <- c(beta[22],deltaMethod(beta,"b22 + b23",vcov.=m$clustervcv)$Estimate)
  se <- c(sqrt(diag(m$clustervcv)[22]),deltaMethod(beta,"b22 + b23",vcov.=m$clustervcv)$SE)
  
  het <- data.frame(beta=est, se=se, lb=NA, ub=NA)
  het$lb=het$beta -1.96*het$se
  het$ub=het$beta +1.96*het$se
  
  het.coef <- rbind(het.coef,het)
  
  rownames(het.coef) <- NULL
  
  het.coef$order <- c(18,17,16,14,13,12,10,9,8,6,5,4,2,1)
  marks <- rev(het.coef$order)
  het.coef$outcome <- out.labs[i]
  
  return(het.coef)
})

plot.df <-do.call(rbind,out.ls)
plot.df$outcome <- factor(plot.df$outcome,levels=out.labs)
plot.df$name <- c("Polity IV (low)", "Polity IV (med.)", "Polity IV (high)",
                  "VDEM (low)", "VDEM (med.)", "VDEM (high)",
                  "Conflict (low)", "Conflict (med.)", "Conflict (high)",
                  "Agr. Exports (low)", "Agr. Exports (med.)", "Agr. Exports (high)",
                  "Coastal Countries", "Landlocked Countries")


p <- ggplot(plot.df)
p <- p + geom_errorbarh(aes(x=beta,y=order, xmin=lb, xmax=ub), size=1.2, height=0)  +
  geom_point(size=3.5,aes(beta,order), pch=rep(c(rep(21,3),rep(22,3),rep(23,3),rep(24,3),rep(25,2)),4),
             fill=rep(c(rep("black",3),rep("white",3),rep("black",3),rep("white",3),rep("black",2)),4)) +
  geom_vline(xintercept=0,linetype="dotted", size=0.6) +
  scale_y_continuous(breaks=rev(c(18,17,16,14,13,12,10,9,8,6,5,4,2,1)),labels=plot.df$name[c(14:1)]) +
  theme_minimal(base_size=16) +
  labs(x = "Marginal Effect and 95 % Confidence Intervals", y = "Moderator Variable & Value",
       title = "Heterogeneity Analysis",
       subtitle = "OLS Interaction Models with Colonial Cash Crop Dummy as Main Predictor") +
  facet_wrap(~ outcome,nrow=1,scales="free_x")
p
p + ggsave(paste0("output/tables_wd/","het_ia_crops.pdf"),width=16,height=7)



######## Colonialism and settlers
quantile(data.g25$settler_per_100k,seq(0,1,1/3),na.rm=T)
data.g25$settler_share_high <-  ifelse(data.g25$settler_per_100k>159.81792 ,1,0)
data.g25$settler_share_medium <- ifelse(data.g25$settler_per_100k>36.65372  & data.g25$settler_per_100k<=159.81792 ,1,0)
data.g25$settler_share_above <-  ifelse(data.g25$settler_per_100k>71.285 ,1,0)

data.g25$settler_per_100k_log <- log(1+data.g25$settler_per_100k)

data.g25$settler_col <- ifelse(data.g25$iso3c_col%in%c("KEN","ZWE","ZMB","MWI","AGO","MOZ","COD","COG","GAB","CAF","TCD","RWA"),1,0)
data.g25$concession_col <- ifelse(data.g25$iso3c_col%in%c("COD","COG","GAB","CAF","RWA","TCD"),1,0)
data.g25$labor_res_col <- ifelse(data.g25$iso3c_col%in%c("KEN","ZWE","ZMB","MWI","AGO","MOZ"),1,0)
data.g25$cash_crop_col <- ifelse(data.g25$iso3c_col%in%c("BEN","BFA","CIV","CMR","GMB","GHA","GIN","GNB","MLI","MRT",
                                                                   "NER","NGA","SEN","SLE","TZA","UGA","TGO"),1,0)

describe(data.g25$time_since_col)
quantile(data.g25$time_since_col,seq(0,1,1/3),na.rm=T)

data.g25$time_since_col_high <-  ifelse(data.g25$time_since_col>67 ,1,0)
data.g25$time_since_col_medium <- ifelse(data.g25$time_since_col>61  & data.g25$time_since_col<=67,1,0)


out.vars <- c("road_dens_2015_log","urb_dens_2015_log","nl_2015_pc_log","hh_wealth")

m.list <- lapply(out.vars,function(y){
  out25 <- mdlr(y=y, x=c("hance_crops_dummy*concession_col","hance_crops_dummy*labor_res_col",
                         "hance_minerals_dummy*concession_col","hance_minerals_dummy*labor_res_col"), cntr=controls.std, fe="iso3c_col", cl="iso3c_col", dat = data.g25)
  return(out25)
})
summary(m.list[[1]])


latex.notes.long <- function(width = ""){
  paste0("\\parbox[t]{",width,"\\textwidth}{\\textbf{Notes:} OLS regressions with 0.25 degree lat/lon grid cells as units of analysis.
  The dependent variables in Columns 1-3 are defined as the natural logarithm of 1 plus (i) paved or improved road length in km per 1000 sqkm land area in 1998, 
  (ii) the urban population per sqkm land area in 2015, (iii) total night lights per 100'000 capita in 2015. 
The dependent variable in Column 4 is the asset-based household wealth score as reported in the DHS surveys averaged across all households and survey rounds per cell.
Cells without any geocoded DHS surveys are dropped. 
The following countries are coded as concession colonies: Congo-Kinshasa, Congo-Brazzavile, Gabon, Central African Republic, Rwanda, Burundi, Tchad.
Labor reserve colonies are Kenia, Malawi, Zimbabwe, Zambia, Angola, and Mozambique.
Geographic control variables include caloric suitability, TseTse fly suitability, malaria suitability, ruggedness, elevation, 
logged minimum distances to navigable rivers and the coast, as well as absolute longitude, latitude, and their squares. 
Historical controls are logged minimum distances to trade routes in 1900, cities in 1900, and the first colonial capital, as well as
ethnic group level proxies for precolonial reliance on agriculture, political centralization, and exposure to the slave trades. 
Standard errors clustered on country in parentheses.
Significance codes: $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}")
}

mean.dv <- sapply(out.vars,function(var){
  round(mean(data.g25[,var],na.rm=T),3)
})

add.lines <- list(latex.cntr.fe(rep("\\checkmark",length(m.list))), 
                  latex.controls.geo(rep("\\checkmark",length(m.list))),
                  latex.controls.hist(rep("\\checkmark",length(m.list))),
                  latex.mean.dv(mean.dv))
# define which coefficients are shown in the output table
keep.lines <- which(grepl("crops",rownames(m.list[[2]]$coefficients)))

# prepare and save output table


fileConn<-file(paste0(tab.path,"het_amin.tex"))
writeLines(stargazer(m.list,
                     title="Heterogeneity by Colonial Economy",
                     keep=keep.lines,
                     column.sep.width = "25pt",
                     model.numbers = F,
                     multicolumn=F,# se = se,
                     dep.var.caption = "Outcome",dep.var.labels = c("Road Dens.", "Urban Pop. Dens.", "Lights p.c.", "HH Wealth"),
                     covariate.labels=c("Cash Crops (Y/N)", 
                                        "Cash Crops $\\times$ Concession Colony",
                                        "Cash Crops $\\times$ Labor Reserve Colony"),
                     font.size = "scriptsize",
                     notes.align = "c",label="het_amin",align =F,
                     add.lines = add.lines,digits = 3, intercept.top = T,intercept.bottom = F,
                     omit.stat = c("rsq","res.dev","ser"),
                     notes = latex.notes.long(1), notes.label = "", notes.append = F), 
           fileConn)
close(fileConn)


######## heterogeneity by settler share
m.list <- lapply(out.vars,function(y){
  out25 <- mdlr(y=y, x=c("hance_crops_dummy*settler_share_high","hance_crops_dummy*settler_share_medium",
                         "hance_minerals_dummy*settler_share_high","hance_minerals_dummy*settler_share_medium"), cntr=controls.std, fe="iso3c_col", cl="iso3c_col", dat = data.g25)
  return(out25)
})
summary(m.list[[4]])


latex.notes.long <- function(width = ""){
  paste0("\\parbox[t]{",width,"\\textwidth}{\\textbf{Notes:} OLS regressions with 0.25 degree lat/lon grid cells as units of analysis.
  The dependent variables in Columns 1-3 are defined as the natural logarithm of 1 plus (i) paved or improved road length in km per 1000 sqkm land area in 1998, 
  (ii) the urban population per sqkm land area in 2015, (iii) total night lights per 100'000 capita in 2015. 
The dependent variable in Column 4 is the asset-based household wealth score as reported in the DHS surveys averaged across all households and survey rounds per cell.
Cells without any geocoded DHS surveys are dropped. 
Data on settler shares compiled from country-specific colonial censuses from years between 1930 and 1960.
Geographic control variables include caloric suitability, TseTse fly suitability, malaria suitability, ruggedness, elevation, 
logged minimum distances to navigable rivers and the coast, as well as absolute longitude, latitude, and their squares. 
Historical controls are logged minimum distances to trade routes in 1900, cities in 1900, and the first colonial capital, as well as
ethnic group level proxies for precolonial reliance on agriculture, political centralization, and exposure to the slave trades. 
Standard errors clustered on country in parentheses.
Significance codes: $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}")
}

mean.dv <- sapply(out.vars,function(var){
  round(mean(data.g25[!is.na(data.g25$settler_share_high),var],na.rm=T),3)
})

add.lines <- list(latex.cntr.fe(rep("\\checkmark",length(m.list))), 
                  latex.controls.geo(rep("\\checkmark",length(m.list))),
                  latex.controls.hist(rep("\\checkmark",length(m.list))),
                  latex.mean.dv(mean.dv))
# define which coefficients are shown in the output table
keep.lines <- which(grepl("crops",rownames(m.list[[2]]$coefficients)))

# prepare and save output table


fileConn<-file(paste0(tab.path,"het_settlers.tex"))
writeLines(stargazer(m.list,
                     title="Heterogeneity by District-Level Settler Share",
                     keep=keep.lines,
                     column.sep.width = "25pt",
                     model.numbers = F,
                     multicolumn=F,# se = se,
                     dep.var.caption = "Outcome",dep.var.labels = c("Road Dens.", "Urban Pop. Dens.", "Lights p.c.", "HH Wealth"),
                     covariate.labels=c("Cash Crops (Y/N)", 
                                        "Cash Crops $\\times$ High Settler Share",
                                        "Cash Crops $\\times$ Intermediate Settler Share"),
                     font.size = "scriptsize",
                     notes.align = "c",label="het_amin",align =F,
                     add.lines = add.lines,digits = 3, intercept.top = T,intercept.bottom = F,
                     omit.stat = c("rsq","res.dev","ser"),
                     notes = latex.notes.long(1), notes.label = "", notes.append = F), 
           fileConn)
close(fileConn)



######## heterogeneity by colonial duration
m.list <- lapply(out.vars,function(y){
  out25 <- mdlr(y=y, x=c("hance_crops_dummy*time_since_col_high","hance_crops_dummy*time_since_col_medium",
                         "hance_minerals_dummy*time_since_col_high","hance_minerals_dummy*time_since_col_medium"), cntr=controls.std, fe="iso3c_col", cl="iso3c_col", dat = data.g25)
  return(out25)
})
summary(m.list[[4]])

latex.notes.long <- function(width = ""){
  paste0("\\parbox[t]{",width,"\\textwidth}{\\textbf{Notes:} OLS regressions with 0.25 degree lat/lon grid cells as units of analysis.
  The dependent variables in Columns 1-3 are defined as the natural logarithm of 1 plus (i) paved or improved road length in km per 1000 sqkm land area in 1998, 
  (ii) the urban population per sqkm land area in 2015, (iii) total night lights per 100'000 capita in 2015. 
The dependent variable in Column 4 is the asset-based household wealth score as reported in the DHS surveys averaged across all households and survey rounds per cell.
Cells without any geocoded DHS surveys are dropped. Data on effective date since coloniozation coded for a subet of Murdock groups by Henderson and Whatley (2013).
Geographic control variables include caloric suitability, TseTse fly suitability, malaria suitability, ruggedness, elevation, 
logged minimum distances to navigable rivers and the coast, as well as absolute longitude, latitude, and their squares. 
Historical controls are logged minimum distances to trade routes in 1900, cities in 1900, and the first colonial capital, as well as
ethnic group level proxies for precolonial reliance on agriculture, political centralization, and exposure to the slave trades. 
Standard errors clustered on country in parentheses.
Significance codes: $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}")
}

mean.dv <- sapply(out.vars,function(var){
  round(mean(data.g25[!is.na(data.g25$time_since_col),var],na.rm=T),3)
})

add.lines <- list(latex.cntr.fe(rep("\\checkmark",length(m.list))), 
                  latex.controls.geo(rep("\\checkmark",length(m.list))),
                  latex.controls.hist(rep("\\checkmark",length(m.list))),
                  latex.mean.dv(mean.dv))
# define which coefficients are shown in the output table
keep.lines <- which(grepl("crops",rownames(m.list[[2]]$coefficients)))

# prepare and save output table


fileConn<-file(paste0(tab.path,"het_colonial_duration.tex"))
writeLines(stargazer(m.list,
                     title="Heterogeneity by Time Since Effective Colonization",
                     keep=keep.lines,
                     column.sep.width = "25pt",
                     model.numbers = F,
                     multicolumn=F,# se = se,
                     dep.var.caption = "Outcome",dep.var.labels = c("Road Dens.", "Urban Pop. Dens.", "Lights p.c.", "HH Wealth"),
                     covariate.labels=c("Cash Crops (Y/N)", 
                                        "Cash Crops $\\times$ Long Colonial Duration",
                                        "Cash Crops $\\times$ Intermediate Colonial Duration"),
                     font.size = "scriptsize",
                     notes.align = "c",label="het_amin",align =F,
                     add.lines = add.lines,digits = 3, intercept.top = T,intercept.bottom = F,
                     omit.stat = c("rsq","res.dev","ser"),
                     notes = latex.notes.long(1), notes.label = "", notes.append = F), 
           fileConn)
close(fileConn)
