library(spatstat)
library(sp)
library(rgdal)
library(rgeos)
library(sf)
library(raster)
here()


# load parallelization
library(foreach)
library(doParallel)
library(parallel)
ncores <- detectCores()-1


# load hance points
hance_crops <- readRDS(here("data","hance_crops.rds"))

# prepare point processes, study area window and covariate list
coords.crops <-unique(hance_crops[,c("Longitude","Latitude")])
coords.g25 <- unique(data.g25[,c("Longitude","Latitude")])
g25.owin <- owin(mask=coords.g25)

ppp.crops <- ppp(x=coords.crops$Longitude,y=coords.crops$Latitude,window=g25.owin)




###
# do poisson model with covariates
# look at inhomogenous K or pcf and see if more clustering than predicted by covariates alone

# prepare covariates
cov.ls <- as.im(X=data.g25[,unique(c("Longitude","Latitude","suit_std",controls.std,controls.std2))])
names(cov.ls)

# load grid id raster
r25.id <- readRDS("data/r25.rds")

### estimate poisson point process model
ppm.crops <- ppm.ppp(ppp.crops, trend = as.formula(paste("~ suit_std +",paste(controls.std[c(1:17)],collapse="+"))),covariates = cov.ls)
summary(ppm.crops)

# simulate 1000 draws
set.seed(4321)
ppm.crops.sim <-simulate(ppm.crops,nsim=1000)


# identify cells with counterfactual points per simulation
cf.cells.ppm <- lapply(c(1:1000),function(i){
  out <- unique(extract(r25.id,cbind(ppm.crops.sim[[i]]$x,ppm.crops.sim[[i]]$y)))
})


### balance checks: estimate 1000 counterfactual coefficients
vars <-bal.vars
t0 <- Sys.time()
cl <- parallel::makeForkCluster(ncores)

registerDoParallel(cl)

cf.coefs.crops <- foreach (y=vars)  %dopar% {

  coefs.cf <- lapply(cf.cells.ppm,function(x){
    data.g25$crops.cf_dummy <- ifelse(data.g25$id%in%x,1,0)
    out <- felm(as.formula(paste(y,"~ crops.cf_dummy", "| 0 | 0 | 0")),data=data.g25[data.g25$popc_2010AD!=0,])$coef[2]
    return(out)
  })
  return(coefs.cf)

}
stopImplicitCluster()

parallel::stopCluster(cl)
rm(cl)

t1 <- Sys.time()
t1-t0 # 4 min
  


# order from small to large
cf.coefs.crops <- lapply(cf.coefs.crops,function(x){
  out <- unlist(x)
  out <- out[order(out)]
  return(out)
})

bal.labs <- c("Cash Crop Suitability","Caloric Suitability", "TseTse Suitability", "Malaria Suitability","Ruggedness","Elevation",
              "Dist. Navigable River", "Dist. Coast", "Dist. Trade Route 1900", "Dist. City 1900",
              "Dist. Colonial Capital", "Slavery (Med.)", "Slavery (High)", "Precolonial Agriculture (Med.)",
              "Precolonial Agriculture (High)","Precolonial Chiefdom", "Precolonial State", "Precolonial Statehood Missing")

cf.coefs.crops <- lapply(c(1:length(cf.coefs.crops)),function(i){
  out <- data.frame(cf.coef=cf.coefs.crops[[i]],what=bal.labs[i])
  return(out)
})

# bind together for plotting
cf.df.crops <- do.call(rbind,cf.coefs.crops)

## estimate treatment coefficients
coefs.treat.crops <- lapply(vars,function(y){
  out <- felm(as.formula(paste(y,"~ hance_crops_dummy", "| 0 | 0 | 0")),data=data.g25[data.g25$popc_2010AD!=0,])$coef[2]
  return(out)
})

##### plot
mu.balance <- data.frame(lb=sapply(cf.coefs.crops,function(x){x$cf.coef[25]}),
                         ub=sapply(cf.coefs.crops,function(x){x$cf.coef[975]}),
                         coef.treat=unlist(coefs.treat.crops),
                         what=bal.labs)

cf.df.crops$what <- factor(cf.df.crops$what,levels=bal.labs)
mu.balance$what <- factor(mu.balance$what,levels=bal.labs)

p <- ggplot(cf.df.crops, aes(x=cf.coef)) + 
  geom_histogram(aes(y=..density..), colour="black",size=0.2, fill="white",bins=100)+
  geom_density(alpha=.2, fill="grey30") +
  labs(x = "Distribution of 1000 Counterfactual Coefficients", y = "Density",
       title = "Covariate Balance: Actual vs. Counterfactual Cash Crop Distribution",
       subtitle="1000 simulated cash crop counterfactuals based on point process estimates",
       caption = "2.5 and 97.5 percentiles of the counterfactual coefficient distribution in blue; actual treatment coefficient in red.") + 
  facet_wrap(~ what,scales="free",ncol=3) 

p <- p + geom_vline(data=mu.balance, aes(xintercept=lb),linetype="dashed",color="blue") +
  geom_vline(data=mu.balance, aes(xintercept=ub),linetype="dashed",color="blue") +
  geom_vline(data=mu.balance, aes(xintercept=coef.treat),color="red")  + theme_minimal()
p
p + ggsave(paste0("output/tables_wd/","RI_balance_crops_ppm.pdf"),width=8,height=10)




####### now with outcome variables

out.vars <- c("road_dens_2015_log","urb_dens_2015_log","nl_2015_pc_log","hh_wealth")
out.labs <- c("Road Density 1998 (log)","Urb. Pop. Dens. 2015 (log)","Night Lights 2015 pc (log)","DHS Household Wealth")
t0 <- Sys.time()
cl <- parallel::makeForkCluster(ncores)

registerDoParallel(cl)

cf.coefs.out.crops <- foreach (y=out.vars)  %dopar% {
  
  coefs.cf <- lapply(cf.cells.ppm,function(x){
    data.g25$crops.cf_dummy <- ifelse(data.g25$id%in%x,1,0)
    out <- felm(as.formula(paste(y,"~ crops.cf_dummy", "| 0 | 0 | 0")),data=data.g25[data.g25$popc_2010AD!=0,])$coef[2]
    return(out)
  })
  return(coefs.cf)
  
}
stopImplicitCluster()

parallel::stopCluster(cl)
rm(cl)

t1 <- Sys.time()
t1-t0 # 4 min


# order from small to large
cf.coefs.out.crops <- lapply(cf.coefs.out.crops,function(x){
  out <- unlist(x)
  out <- out[order(out)]
  return(out)
})



cf.coefs.out.crops <- lapply(c(1:length(cf.coefs.out.crops)),function(i){
  out <- data.frame(cf.coef=cf.coefs.out.crops[[i]],what=out.labs[i])
  return(out)
})

cf.crops.mean <- sapply(cf.coefs.out.crops,function(x){
  mean(x$cf.coef)
})


# bind together for plotting
cf.df.out.crops <- do.call(rbind,cf.coefs.out.crops)


## estimate treatment coefficients
coefs.treat.out.crops <- lapply(out.vars,function(y){
  out <- felm(as.formula(paste(y,"~ hance_crops_dummy", "| 0 | 0 | 0")),data=data.g25[data.g25$popc_2010AD!=0,])$coef[2]
  return(out)
})



p.val=sapply(c(1:4),function(i){
  sum(abs(cf.coefs.out.crops[[i]]$cf.coef)>=coefs.treat.out.crops[[i]])/1000
})



##### plot
mu.treat <- data.frame(lb=sapply(cf.coefs.out.crops,function(x){x$cf.coef[25]}),
                       ub=sapply(cf.coefs.out.crops,function(x){x$cf.coef[975]}),
                       mu=cf.crops.mean,
                       p.val=sapply(c(1:4),function(i){
                         round((sum(abs(cf.coefs.out.crops[[i]]$cf.coef)>=coefs.treat.out.crops[[i]]))/(1000),3)
                       }),
                       coef.treat=unlist(coefs.treat.out.crops),
                       what=out.labs)
mu.treat$ri_est <- mu.treat$coef.treat-mu.treat$mu 

cf.df.out.crops$what <- factor(cf.df.out.crops$what,levels=out.labs)
mu.treat$what <- factor(mu.treat$what,levels=out.labs)


p <- ggplot(cf.df.out.crops, aes(x=cf.coef)) + 
  geom_histogram(aes(y=..density..), colour="black",size=0.2, fill="white",bins=100)+
  geom_density(alpha=.2, fill="grey30") +
  #scale_x_continuous(limits=c(0,0.43))+
  labs(x = "Distribution of 1000 Counterfactual Coefficients", y = "Density",
       title = "Treatment Effects: Actual vs. Counterfactual Cash Crop Distribution",
       subtitle="1000 simulated cash crop counterfactuals based on point process estimates",
       caption = "2.5 and 97.5 percentiles as well as mean of the counterfactual coefficient distribution in blue; actual treatment coefficient in red.") + 
  facet_wrap(~ what,scales="free",ncol=4) 


p <- p + geom_vline(data=mu.treat, aes(xintercept=lb),linetype="dashed",color="blue") +
  geom_vline(data=mu.treat, aes(xintercept=ub),linetype="dashed",color="blue") +
  geom_vline(data=mu.treat, aes(xintercept=mu),linetype="solid",color="blue",size=.8) +
  geom_vline(data=mu.treat, aes(xintercept=coef.treat),color="red",size=0.8) + theme_minimal()
p
p + ggsave(paste0("output/tables_wd/","RI_outcomes_crops_ppm.pdf"),width=12,height=3.5)


out.labs2 <- c("Road Density 1998 (log)","Urban Population Density 2015 (log)","Night Lights per capita 2015 (log)","DHS Household Wealth")
p.ls <- lapply(c(1:4),function(i){
  p <- ggplot(cf.df.out.crops[cf.df.out.crops$what==out.labs[i],], aes(x=cf.coef)) + 
    geom_histogram(aes(y=..density..), colour="black",size=0.2, fill="white",bins=100)+
    geom_density(alpha=.2, fill="grey30") +
    labs(x = "Coefficients", y = "Density",
         title = out.labs2[i],
         subtitle=paste("Estimate:",round(mu.treat$ri_est[i],3), " p-value:", round(mu.treat$p.val[i],3))) 
  p <- p + geom_vline(data=mu.treat[i,], aes(xintercept=lb),linetype="dashed",color="blue") +
    geom_vline(data=mu.treat[i,], aes(xintercept=ub),linetype="dashed",color="blue") +
    geom_vline(data=mu.treat[i,], aes(xintercept=mu),linetype="solid",color="blue",size=.8) +
    geom_vline(data=mu.treat[i,], aes(xintercept=coef.treat),color="red",size=0.8) + theme_minimal(base_size = 10)
  return(p)
})


library(patchwork)
p.out <- p.ls[[1]] | p.ls[[2]] | p.ls[[3]] | p.ls[[4]]

p.out <- p.out + plot_annotation(title = "Treatment Effects: Actual vs. Counterfactual Cash Crop Distribution",
                        subtitle="1000 simulated cash crop counterfactuals based on point process estimates",
                        caption = "2.5 and 97.5 percentiles as well as mean of the counterfactual coefficient distribution in blue; actual treatment coefficient in red.")

p.out + ggsave(paste0("output/tables_wd/","RI_outcomes_crops_ppm_new.pdf"),width=12,height=3.5)




####### check clustering
# average nearest-neighbor distance for original points
ann.ppp <- mean(nndist(ppp.crops,k=1))


ann.sim <- sapply(ppm.crops.sim,function(x){
  mean(nndist(x,k=1))
})
ann.df <- data.frame(ann.sim=ann.sim[order(ann.sim)])

p <- ggplot(ann.df, aes(x=ann.sim)) + 
  geom_histogram(aes(y=..density..), colour="black",size=0.2, fill="white",bins=100)+
  geom_density(alpha=.2, fill="grey30") +
  #scale_x_continuous(limits=c(0,0.43))+
  labs(x = "Distribution of 1000 Counterfactual ANNDs", y = "Density",
       title = "Clustering: Actual vs. Counterfactual Cash Crop Distribution",
       subtitle="1000 simulated cash crop counterfactuals based on point process estimates",
       caption = "2.5 and 97.5 percentiles as well as mean of the counterfactual ANN distribution in blue; actual ANN in red.") + 
  geom_vline(xintercept = ann.df$ann.sim[25],linetype="dashed",color="blue") +
  geom_vline(xintercept = ann.df$ann.sim[975],linetype="dashed",color="blue") +
  geom_vline(xintercept = ann.ppp,linetype="solid",color="red") + theme_minimal()
 
p
p + ggsave(paste0("output/tables_wd/","RI_ann_ppm.pdf"),width=6,height=6)



cf.cells.crops.unique <- lapply(cf.cells.ppm,function(x){
  out <- unique(x)
  return(out)
})
cf.df <- unlist(cf.cells.crops.unique)
cf.df <- data.frame(table(cf.df))
names(cf.df) <- c("id","cf.freq")
cf.df$id <- as.numeric(as.character(cf.df$id))
cf.df$cf.freq.cuts <-cut(cf.df$cf.freq,c(0,30,75,150,300,1000))
table(cf.df$cf.freq.cuts)

cf.df <- left_join(data.g25[,c("id","Longitude","Latitude")],cf.df)
saveRDS(cf.df,"data/cf_df_ppm.rds")












