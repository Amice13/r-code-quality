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



####  mediation function with felm & unlim nb of mediators
geo.mediation <- function(yvar=yvar,mediators , Xconf =conf.pre,  Zconf=conf.int,  data){
  
  m.stage1 <- felm(as.formula(RegFor( y = yvar , x = c(cropvar, pubvar ,mediators, Xconf, Zconf) ,
                                      FE = "country_survey_round" , IV="0", clust = "0")),
                   data=data)
  betas <- coef(m.stage1)[mediators]
  
  betamat <- diag(betas, ncol = length(betas), nrow = length(betas))
  
  correctterm <- data.matrix(data[,mediators]) %*% data.matrix(betamat)
  data[["ytild"]] <- data[[yvar]]  + (correctterm %*% matrix(-1*rep(1,length(betas))))
  m.stage2 <- felm(as.formula(RegFor( y = "ytild" , x = c(cropvar , pubvar, conf.pre) ,
                                      FE = "country_survey_round" , IV="0", clust = cl_var)),
                   data=data)
  out <- coef(m.stage2)[c(cropvar , pubvar)]
  return(out)
}

####  mediation function with felm & unlim nb of mediators
ethn.mediation <- function(yvar=yvar,mediators , Xconf =conf.pre,  Zconf=conf.int,  data){
  
  m.stage1 <- felm(as.formula(RegFor( y = yvar , x = c(cropvar, pubvar ,mediators, Xconf, Zconf) ,
                                      FE = "loc.id_pubs_poly" , IV="0", clust = "0")),
                   data=data)
  betas <- coef(m.stage1)[mediators]
  
  betamat <- diag(betas, ncol = length(betas), nrow = length(betas))
  
  correctterm <- data.matrix(data[,mediators]) %*% data.matrix(betamat)
  data[["ytild"]] <- data[[yvar]]  + (correctterm %*% matrix(-1*rep(1,length(betas))))
  m.stage2 <- felm(as.formula(RegFor( y = "ytild" , x = c(cropvar , pubvar, conf.pre) ,
                                      FE = "loc.id_pubs_poly" , IV="0", clust = cl_var)),
                   data=data)
  out <- coef(m.stage2)[c(cropvar , pubvar)]
  return(out)
}


### define outcomes and control variables
yvars<- paste0("exogamy_l",c(1:11,15:16)) ## 
yvars_std <- paste0(yvars,"_std")
controls.geo <- c("ruggedness_nunn_puga" ,  "elevation"  ,  "malaria_suit_max",
                  "TSI_CRU_mean_1901_1920" , "temperature_fao" , "precipitation_fao",  "coast_log"  ,
                  "LONGNUM", "agric_suit", "LATNUM")
controls.hist <- c("explorers_log", "cities_log", "capital_log","dist.prot_log" , "dist.print_log","popc_mean_1720_1890_log")
controls.ind <- c("age","age.sq","age.m","age.m.sq")
base.x <-  c(controls.ind, controls.geo,controls.hist)


####################################################
#Mediator: Early Educated Elite
####################################################
X <- c(controls.ind, controls.geo, "explorers_log", "dist.prot_log" , "dist.print_log","popc_mean_1720_1890_log")
Z <- c("cities_log", "capital_log","urban_dummy","hh_wealth","educ1","educ2","educ3","educ1.m","educ2.m","educ3.m")
cropvar <- "hance_crops5_sum_15km_std"
pubvar <- "pubspc.23_poly_std"
mediators <- c("educ.sec.early.poly")
cl_var <- "loc.id_pubs_poly"

# define pretreatment confounders
conf.pre <- X

# define intermediate confounders
conf.int <- Z

dat <- dhs[,c(cropvar,pubvar, yvars_std, X, Z, mediators , cl_var, "country_survey_round")]
dat <- na.omit(dat)

# estimate baseline OLS without intermediate confounders to get total effect
m.list.base <- lapply(yvars_std, function(yvar){
  felm(as.formula(RegFor( y = yvar , x = c(cropvar , pubvar , conf.pre) ,
                          FE = "country_survey_round" , IV="0", clust = "loc.id_pubs_poly")),
       data=dat)
})


m.list.acde <- lapply(yvars_std, function(yvar){
  geo.mediation(yvar=yvar,mediators=mediators  , conf.pre,
                Zconf=conf.int, dat)
})


### cluster bootstrap
# define cluster variable
clusters <- unique(dat[,cl_var])
# set number of bootsraps
boots <- 150
print("Bootstrapping for ACDE... ")
t0 <- Sys.time()
set.seed(4321)

# make cluster
cl <- parallel::makeCluster(ncores)
parallel::clusterExport(cl, c("boots","clusters","dat",
                              "cl_var","yvars_std","cropvar","pubvar",
                              "conf.pre","conf.int","mediators",
                              "geo.mediation","ethn.mediation","RegFor"))
clusterEvalQ(cl, {
  library(lfe)
})


registerDoParallel(cl)  

# bootstrap
boot.ls <- lapply(yvars_std,function(yvar){
  print(paste("Bootstrapping coefs for", yvar, "..." ))
  
  mediation.coefs <- foreach (i=seq(boots))  %dorng% {
    
    units <- sample(clusters, size = length(clusters), replace=TRUE)
    # create bootstap sample with sapply
    df.bs <- sapply(units, function(x) which(dat[,cl_var]==x))
    dat.boot <- dat[unlist(df.bs),]
    
    m.base <- felm(as.formula(RegFor( y = yvar , x = c(cropvar , pubvar , conf.pre) ,
                                      FE = "country_survey_round" , IV="0", clust = "loc.id_pubs_poly")),
                   data=dat.boot)
    ate <- coef(m.base)[c(cropvar , pubvar)]
    acde <- geo.mediation(yvar=yvar ,mediators=mediators  , conf.pre,
                          Zconf=conf.int, dat.boot)
    acde <- cbind(ate, acde)
    return(acde) 
    
  }
  
  t0.1 <- Sys.time()
  print("has been going on for...")
  print(t0.1-t0)
  return(mediation.coefs)
})

stopImplicitCluster()

parallel::stopCluster(cl)
rm(cl)


### get upper and lower bounds of bootstrapped confidence intervals
q2 <- function(x){quantile(x, 0.025, na.rm = TRUE)}
q9 <- function(x){quantile(x, 0.975, na.rm = TRUE)}

boot.ci <- as.data.frame(do.call(rbind,lapply(boot.ls,function(x){
  
  med.coefs <- as.data.frame(do.call(rbind,x)) %>%
    rownames_to_column("treatment")
  med.coefs$treatment <- ifelse(grepl("crop", med.coefs$treatment), "Cash Crops", "Publications")
  
  med.coefs.ub <- med.coefs %>%
    group_by(treatment) %>%
    summarize_all(q9)
  names(med.coefs.ub)[c(2,3)] <- c("ub_boot_ate","ub_boot_acde")
  
  med.coefs.lb <- med.coefs %>%
    group_by(treatment) %>%
    summarize_all(q2)
  names(med.coefs.lb)[c(2,3)] <- c("lb_boot_ate","lb_boot_acde")
  
  out <- left_join(med.coefs.lb,med.coefs.ub)
  return(out)
})))




#### prepare regression results for visual output
# total effect
out.ls <- lapply(m.list.base, function(x){
  get_plot_data(x,coef_no=c(1,2))
})
out.df.base <- do.call(rbind,out.ls)
out.df.base$dv <- c(rep(paste0("Exogamy L",c(1:10)),each=2),rep(paste0("Exogamy L","11-14"),each=2),rep(paste0("Exogamy L",c(15:16)),each=2))
ticks <- seq(19.5,1,-1.5)
ord. <- c(ticks+0.4,ticks-0.2)
out.df.base$order <- ord.[order(ord.,decreasing=T)]
out.df.base$lb_boot <- boot.ci$lb_boot_ate
out.df.base$ub_boot <- boot.ci$ub_boot_ate

# acde
dv <- c(rep(paste0("Exogamy L",c(1:10)),each=2),rep(paste0("Exogamy L","11-14"),each=2),rep(paste0("Exogamy L",c(15:16)),each=2))
ord. <- c(ticks+0.2,ticks-0.4)
ord. <- ord.[order(ord.,decreasing=T)]
out.df.acde <- data.frame(beta=unlist(m.list.acde),se=NA,lb=NA,ub=NA,dv=dv,order=ord.,lb_boot=boot.ci$lb_boot_acde,ub_boot=boot.ci$ub_boot_acde)
out.df.acde$dv <- c(rep(paste0("Exogamy L",c(1:10)),each=2),rep(paste0("Exogamy L","11-14"),each=2),rep(paste0("Exogamy L",c(15:16)),each=2))


out.df.both <- rbind(out.df.base,out.df.acde)
out.df.both <- out.df.both[order(out.df.both$order,decreasing = T),]
out.df.both$what <- rep(c("Cash Crops (Total Effect)","Cash Crops (ACDE)","Publications (Total Effect)","Publications (ACDE)"),13)
out.df.both$what <- factor(out.df.both$what,levels=c("Cash Crops (Total Effect)","Cash Crops (ACDE)","Publications (Total Effect)","Publications (ACDE)"))

col1 <- wes_palette("Darjeeling1",5,"discrete")[2]
col2 <- wes_palette("Darjeeling1",5,"discrete")[3]
col3 <- wes_palette("Darjeeling1",5,"discrete")[5]
col4 <- wes_palette("Darjeeling1",15,"continuous")[2]



# do the plot from both models
p1 <- ggplot(out.df.both)
p1 <- p1 + geom_point(size=2.25,aes(x=beta,y=order,color=what,shape=what,fill=what)) +
  geom_errorbarh(aes(x=beta,y=order, xmin=lb_boot, xmax=ub_boot,color=what), size=1, height=0.0) +
  geom_vline(xintercept=0,linetype="dotted", size=0.6) +
  scale_y_continuous(breaks=ticks,minor_breaks = NULL,
                     labels=c(paste0("Exogamy L",c(1:10)),"Exogamy L11-14",paste0("Exogamy L",c(15:16)))) +
  #scale_x_continuous(minor_breaks = seq(0 , 0.55, 0.05), breaks = seq(0, 0.55, 0.1)) +
  labs(x = "Coefficients and 95% Confidence Intervals", y = "Exogamy at Ethnologue Levels",
       title="Causal Mediation Analysis",
       subtitle="Geographic Specifications. Mediator: Early Education (Poly.)"
       #,caption="Cash crop value per sqkm in 1960 USD (Poly.)\nCash Crops (SAR)\nCash Crop Suitability (Poly)\nCash Crops (Instrumented with Suitability)"
  ) +
  theme_minimal(base_size=14) +
  theme(axis.text.y = element_text(size=10),legend.position = "bottom") +
  scale_color_manual(values=c("Cash Crops (Total Effect)" = col1, "Cash Crops (ACDE)" = col3, "Publications (Total Effect)" =col2, "Publications (ACDE)" =col4),name="Treatment/Model") +
  scale_fill_manual(values=c("Cash Crops (Total Effect)" = col1, "Cash Crops (ACDE)" = col3, "Publications (Total Effect)" =col2, "Publications (ACDE)" =col4),name="Treatment/Model") +
  scale_shape_manual(values=c("Cash Crops (Total Effect)" = 24, "Cash Crops (ACDE)" = 22, "Publications (Total Effect)" = 25, "Publications (ACDE)" =23),name="Treatment/Model")
p1 <- p1 + guides(color=guide_legend(ncol=2,nrow=2,byrow=TRUE),
                  fill=guide_legend(ncol=2,nrow=2,byrow=TRUE),
                  shape=guide_legend(ncol=2,nrow=2,byrow=TRUE))
p1



####################################################
###### now ethnic leaver mediation analysis
####################################################
source("dhs/dhs_standardize_fun.R")

dhs.leavers.23.m<- dhs[which(dhs$stayer_wlms.m==0 & 
                               !is.na(dhs$hance_crops5_sqkm_ethn_std.m) & !is.na(dhs$pubspc.23_ethn_std.m) & 
                               dhs$LONGNUM!=0 & dhs$iso3c%in%sample_countries_hance),]
dhs.leavers.23.m <- standardize_dhs(dhs.leavers.23.m)



#Mediator: Early Education

X <- c(controls.ind,"popc_mean_1720_1890_mean_ethn.m_log","popc_mean_1720_1890_mean_ethn_log","hance_crops5_sqkm_ethn_std","pubspc.23_ethn_std")
Z <- c("educ1","educ2","educ3","educ1.m","educ2.m","educ3.m","hh_wealth")
cropvar <- "hance_crops5_sqkm_ethn_std.m"
pubvar <- "pubspc.23_ethn_std.m"
mediators <- c("educ.sec.early.ethn")
cl_var <- "loc.id_pubs_poly"

# define pretreatment confounders
conf.pre <- X

# define intermediate confounders
conf.int <- Z

dat <- dhs.leavers.23.m[,c(cropvar,pubvar, yvars_std, X, Z, mediators , cl_var, "country_survey_round")]
dat <- na.omit(dat)


m.list.base <- lapply(yvars_std, function(yvar){
  felm(as.formula(RegFor( y = yvar , x = c(cropvar , pubvar , conf.pre) ,
                          FE = "loc.id_pubs_poly" , IV="0", clust = "loc.id_pubs_poly")),
       data=dat)
})


m.list.acde <- lapply(yvars_std, function(yvar){
  ethn.mediation(yvar=yvar,mediators=mediators  , conf.pre,
                 Zconf=conf.int, dat)
})


### cluster bootstrap
# define cluster variable
clusters <- unique(dat[,cl_var])
# set number of bootsraps
boots <- 150
print("Bootstrapping for ACDE... ")
t0 <- Sys.time()
set.seed(1234)

# make cluster
cl <- parallel::makeCluster(ncores)
parallel::clusterExport(cl, c("boots","clusters","dat",
                              "cl_var","yvars_std","cropvar","pubvar",
                              "conf.pre","conf.int","mediators",
                              "geo.mediation","ethn.mediation","RegFor"))
clusterEvalQ(cl, {
  library(lfe)
})


registerDoParallel(cl)  

# bootstrap
boot.ls <- lapply(yvars_std,function(yvar){
  print(paste("Bootstrapping coefs for", yvar, "..." ))
  
  mediation.coefs <- foreach (i=seq(boots))  %dorng% {
    
    units <- sample(clusters, size = length(clusters), replace=TRUE)
    # create bootstap sample with sapply
    df.bs <- sapply(units, function(x) which(dat[,cl_var]==x))
    dat.boot <- dat[unlist(df.bs),]
    
    m.base <- felm(as.formula(RegFor( y = yvar , x = c(cropvar , pubvar , conf.pre) ,
                                      FE = "loc.id_pubs_poly" , IV="0", clust = "loc.id_pubs_poly")),
                   data=dat.boot)
    ate <- coef(m.base)[c(cropvar , pubvar)]
    acde <- ethn.mediation(yvar=yvar ,mediators=mediators  , conf.pre,
                           Zconf=conf.int, dat.boot)
    acde <- cbind(ate, acde)
    return(acde) 
    
  }
  
  t0.1 <- Sys.time()
  print("has been going on for...")
  print(t0.1-t0)
  return(mediation.coefs)
})

stopImplicitCluster()

parallel::stopCluster(cl)
rm(cl)


### get upper and lower bounds of bootstrapped confidence intervals
q2 <- function(x){quantile(x, 0.025, na.rm = TRUE)}
q9 <- function(x){quantile(x, 0.975, na.rm = TRUE)}

boot.ci <- as.data.frame(do.call(rbind,lapply(boot.ls,function(x){
  
  med.coefs <- as.data.frame(do.call(rbind,x)) %>%
    rownames_to_column("treatment")
  med.coefs$treatment <- ifelse(grepl("crop", med.coefs$treatment), "Cash Crops", "Publications")
  
  med.coefs.ub <- med.coefs %>%
    group_by(treatment) %>%
    summarize_all(q9)
  names(med.coefs.ub)[c(2,3)] <- c("ub_boot_ate","ub_boot_acde")
  
  med.coefs.lb <- med.coefs %>%
    group_by(treatment) %>%
    summarize_all(q2)
  names(med.coefs.lb)[c(2,3)] <- c("lb_boot_ate","lb_boot_acde")
  
  out <- left_join(med.coefs.lb,med.coefs.ub)
  return(out)
})))





#### prepare regression results for visual output
# total effect
out.ls <- lapply(m.list.base, function(x){
  get_plot_data(x,coef_no=c(1,2))
})
out.df.base <- do.call(rbind,out.ls)
out.df.base$dv <- c(rep(paste0("Exogamy L",c(1:10)),each=2),rep(paste0("Exogamy L","11-14"),each=2),rep(paste0("Exogamy L",c(15:16)),each=2))
ticks <- seq(19.5,1,-1.5)
ord. <- c(ticks+0.4,ticks-0.2)
out.df.base$order <- ord.[order(ord.,decreasing=T)]
out.df.base$lb_boot <- boot.ci$lb_boot_ate
out.df.base$ub_boot <- boot.ci$ub_boot_ate

# acde
dv <- c(rep(paste0("Exogamy L",c(1:10)),each=2),rep(paste0("Exogamy L","11-14"),each=2),rep(paste0("Exogamy L",c(15:16)),each=2))
ord. <- c(ticks+0.2,ticks-0.4)
ord. <- ord.[order(ord.,decreasing=T)]
out.df.acde <- data.frame(beta=unlist(m.list.acde),se=NA,lb=NA,ub=NA,dv=dv,order=ord.,lb_boot=boot.ci$lb_boot_acde,ub_boot=boot.ci$ub_boot_acde)
out.df.acde$dv <- c(rep(paste0("Exogamy L",c(1:10)),each=2),rep(paste0("Exogamy L","11-14"),each=2),rep(paste0("Exogamy L",c(15:16)),each=2))


out.df.both <- rbind(out.df.base,out.df.acde)
out.df.both <- out.df.both[order(out.df.both$order,decreasing = T),]
out.df.both$what <- rep(c("Cash Crops (Total Effect)","Cash Crops (ACDE)","Publications (Total Effect)","Publications (ACDE)"),13)
out.df.both$what <- factor(out.df.both$what,levels=c("Cash Crops (Total Effect)","Cash Crops (ACDE)","Publications (Total Effect)","Publications (ACDE)"))

col1 <- wes_palette("Darjeeling1",5,"discrete")[2]
col2 <- wes_palette("Darjeeling1",5,"discrete")[3]
col3 <- wes_palette("Darjeeling1",5,"discrete")[5]
col4 <- wes_palette("Darjeeling1",15,"continuous")[2]



# do the plot from both models
p2 <- ggplot(out.df.both)
p2 <- p2 + geom_point(size=2.25,aes(x=beta,y=order,color=what,shape=what,fill=what)) +
  geom_errorbarh(aes(x=beta,y=order, xmin=lb_boot, xmax=ub_boot,color=what), size=1, height=0.0) +
  geom_vline(xintercept=0,linetype="dotted", size=0.6) +
  scale_y_continuous(breaks=ticks,minor_breaks = NULL,
                     labels=c(paste0("Exogamy L",c(1:10)),"Exogamy L11-14",paste0("Exogamy L",c(15:16)))) +
  #scale_x_continuous(minor_breaks = seq(0 , 0.55, 0.05), breaks = seq(0, 0.55, 0.1)) +
  labs(x = "Coefficients and 95% Confidence Intervals", y = "Exogamy at Ethnologue Levels",
       title="Causal Mediation Analysis",
       subtitle="Male Leavers. Mediator: Early Education (Group)"
       #,caption="Cash crop value per sqkm in 1960 USD (Poly.)\nCash Crops (SAR)\nCash Crop Suitability (Poly)\nCash Crops (Instrumented with Suitability)"
  ) +
  theme_minimal(base_size=14) +
  theme(axis.text.y = element_text(size=10),legend.position = "bottom") +
  scale_color_manual(values=c("Cash Crops (Total Effect)" = col1, "Cash Crops (ACDE)" = col3, "Publications (Total Effect)" =col2, "Publications (ACDE)" =col4),name="Treatment/Model") +
  scale_fill_manual(values=c("Cash Crops (Total Effect)" = col1, "Cash Crops (ACDE)" = col3, "Publications (Total Effect)" =col2, "Publications (ACDE)" =col4),name="Treatment/Model") +
  scale_shape_manual(values=c("Cash Crops (Total Effect)" = 24, "Cash Crops (ACDE)" = 22, "Publications (Total Effect)" = 25, "Publications (ACDE)" =23),name="Treatment/Model")
p2 <- p2 + guides(color=guide_legend(ncol=2,nrow=2,byrow=TRUE),
                  fill=guide_legend(ncol=2,nrow=2,byrow=TRUE),
                  shape=guide_legend(ncol=2,nrow=2,byrow=TRUE))
p2

# combine with patchwork
p.out <- p1 | p2
#save
p.out + ggsave(paste0(fig.path,"dhs_figA26_boot.pdf"),width=16,height=9)