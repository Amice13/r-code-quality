#### STEP 1 : LOAD LIBRARIES
library(haven)
library(tidyverse)
library(MASS)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(grid)
library(mvtnorm)
library(nlme)
library(plm)
library(kableExtra)
library(parallel)
library(robustbase)

####
############### THE FOLLOWING CODE HELPS US PARALLEL COMPUTE LATER ###############
############### CREDIT TO : Nathan VanHoudnos 
mclapply.hack <- function(..., mc.cores=NULL) {
  ## Create a cluster
  if( is.null(mc.cores) ) {
    size.of.list <- length(list(...)[[1]])
    mc.cores <- min(size.of.list, detectCores())
  }
  ## N.B. setting outfile to blank redirects output to
  ##      the master console, as is the default with
  ##      mclapply() on Linux / Mac
  cl <- makeCluster( mc.cores, outfile="" )
  
  ## Find out the names of the loaded packages
  loaded.package.names <- c(
    ## Base packages
    sessionInfo()$basePkgs,
    ## Additional packages
    names( sessionInfo()$otherPkgs ))
  
  tryCatch( {
    
    ## Copy over all of the objects within scope to
    ## all clusters.
    this.env <- environment()
    while( identical( this.env, globalenv() ) == FALSE ) {
      clusterExport(cl,
                    ls(all.names=TRUE, env=this.env),
                    envir=this.env)
      this.env <- parent.env(environment())
    }
    clusterExport(cl,
                  ls(all.names=TRUE, env=globalenv()),
                  envir=globalenv())
    
    ## Load the libraries on all the clusters
    ## N.B. length(cl) returns the number of clusters
    parLapply( cl, 1:length(cl), function(xx){
      lapply(loaded.package.names, function(yy) {
        require(yy , character.only=TRUE)})
    })
    
    ## Run the lapply in parallel
    return( parLapply( cl, ...) )
  }, finally = {
    ## Stop the cluster
    stopCluster(cl)
  })
}
##
##
############### ACTUAL WORK STARTS BELOW ###############
##
##
#### STEP 2 : DEFINE LOG LIKELIHOOD FUNCTION
corlik <- function(x) {
  res <- 0
  if(x >= -1 & x <= 1) res = 1 
  return(log(res))
}
varlik <- function(x) {
  res <- 0
  if(x > 0) res = 1
  return(log(res))
}
##
##
llk <- function(ymat,xmat1.1,xmat1.2,theta_vec){
  bvec <- theta_vec[1:length(varsused)]
  avec <- theta_vec[(1+length(varsused)):(length(p)-1)]
  
  L1 <- 0
  L3 <- 0
  L4 <- 0
  for(j in 1:nrow(ymat)){
    jo <<- j
    yvec <- as.numeric(ymat[j,])
    xvec1.1 <- as.numeric(xmat1.1[j,])
    xvec1.2 <- as.numeric(xmat1.2[j,])
    mu1 <- xvec1.1%*%bvec
    mu2 <- xvec1.2%*%bvec
    
    xvec.var1 <- xvec1.1[vvindex]
    xvec.var2 <- xvec1.2[vvindex]
    xalph1 <- xvec.var1%*%avec
    xalph2 <- xvec.var2%*%avec
    ss1 <- exp(xalph1)
    ss2 <- exp(xalph2)
    # we construct the covariance matrix out of the correlation coefficient
    # and the variance components
    covAB <- theta_vec['rho']*(sqrt(ss1)*sqrt(ss2))
    sigma <- array(c(ss1, covAB, covAB, ss2), dim=c(2,2))
    #
    # the log-likelihood of data
    L1 <- L1 + dmvnorm(x = yvec, mean = c(mu1,mu2), sigma = sigma, log=TRUE)
    # and the priors on the variance components
    L3 <- L3 + varlik(ss1)#!
    L4 <- L4 + varlik(ss2)#!
  }
  #
  # the prior = likelihood of rho (correlation coefficient)
  L2 <- corlik(theta_vec['rho'])
  # the joint likelihood
  return(-sum(L1 + L2 + L3 + L4))
}
##
##
#### STEP 4 : PREPARE DATA
varsused <- c('intercept','treat_hstat','treat_kids','treat_nat_me','treat_drk','cand')
varsused.var <- c('intercept'
                  ,'treat_hstat','treat_kids','treat_nat_me','treat_drk')
vvindex <- which(varsused %in% varsused.var)
##
##
df <- read_dta ("data/raw/imm.bjpols.dta")
##
##
# SET INITIAL VALUES FOR OPTIM.
p <- c(rep(0.0001,length(varsused)),rep(0.001,length(varsused.var)),0.01)
names(p) <- c(varsused,varsused.var,"rho")

mle_res_saved <- function(cname){
  tryCatch({
    df_filtered <- df %>% filter(countryt == cname & treat_gender ==0)
    df1 <- dplyr::select(df_filtered,'support',varsused[-1],'uid')
    df1 <- na.omit(df1)
    
    c1df <- subset(df1, df1$cand == 1)
    c2df <- subset(df1, df1$cand == 2)
    
    df.f <- merge(c1df,c2df,by='uid')
    colnames(df.f) <- c('uid','supportc1',varsused[-1],'supportc2',varsused[-1])
    df.fu1 <- df.f[,c(1:((ncol(df.f)+1)/2))]
    df.fu2 <- df.f[,c(1,(ncol(df.fu1)+1):ncol(df.f))]
    colnames(df.fu1) <- c('uid','support',varsused[-1])
    colnames(df.fu2) <- c('uid','support',varsused[-1])
    df.fu <- rbind(df.fu1,df.fu2)
    ymat <- data.matrix(df.f[,c('supportc1','supportc2')])
    xmatc1 <- data.matrix(cbind('intercept'=1, df.fu1[varsused[-1]]))
    xmatc2 <- data.matrix(cbind('intercept'=1, df.fu2[varsused[-1]]))
    
    ML <- optim(p, llk, ymat=ymat, xmat1.1=xmatc1, xmat1.2=xmatc2, 
                method='BFGS',hessian = TRUE)
    
    Fisher_inv <- solve(ML$hessian)
    mle_coef <- ML$par
    
    resultlist <- list(mle_coef,Fisher_inv)
    return(resultlist)
  },error=function(e){print('err')})
}
country_list <- unique(df$countryt)
#### STEP 5 : PERFORM MAXIMUM LIKELIHOOD ESTIMATION. (WARNING : TAKES VERY VERY LONG, IF NO TIME, JUST LOAD THE PRE-SAVED FILE.)
#bycon <- mclapply.hack(country_list, mle_res_saved,mc.cores = detectCores()) #(IF MAC USER, YOU CAN USE REGULAR MCLAPPLY. WINDOWS USERS NEED THE HACK.)
load('data/interim/variance_data/bycon_auth.rda')
bycon <- bycon[-(11:12)] #Spain doesn't work.
country_list <- country_list[-(11:12)]
##
##
# Now we do the same but don't model the variance with any covariates. We use this
# as just a sanity check. We see that our model has same results as Authors' model.
varsused1 <- c('intercept','treat_hstat','treat_kids','treat_nat_me','treat_drk','cand')
varsused1.var <- c('intercept')
vvindex <- c(which(varsused1 %in% varsused1.var)) # Will help later.

df <- read_dta ("data/raw/imm.bjpols.dta")
df_filtered <- df %>% filter(countryt == 'AU' & treat_gender ==0)
df1 <- dplyr::select(df_filtered,'support',varsused1[-1],'uid')
df1 <- na.omit(df1) # Missing data observations removed. Scope for Multiple Imputation?

c1df <- subset(df1, df1$cand == 1)
c2df <- subset(df1, df1$cand == 2)

df.f <- merge(c1df,c2df,by='uid')
colnames(df.f) <- c('uid','supportc1',varsused1[-1],'supportc2',varsused1[-1])
df.fu1 <- df.f[,c(1:((ncol(df.f)+1)/2))]
df.fu2 <- df.f[,c(1,(ncol(df.fu1)+1):ncol(df.f))]
colnames(df.fu1) <- c('uid','support',varsused1[-1])
colnames(df.fu2) <- c('uid','support',varsused1[-1])
df.fu <- rbind(df.fu1,df.fu2)

ymat <- data.matrix(df.f[,c('supportc1','supportc2')])
xmatc1 <- data.matrix(cbind('intercept'=1, df.fu1[varsused1[-1]]))
xmatc2 <- data.matrix(cbind('intercept'=1, df.fu2[varsused1[-1]]))
# Simple OLS (pooled) model.
olsmod <- lm(support ~ 
               treat_hstat +
               treat_kids +
               treat_nat_me + 
               treat_drk + 
               cand, 
             data = df_filtered)
# Panel model with individual random effects.
plmmod <- plm(support ~ 
                treat_hstat +
                treat_kids +
                treat_nat_me + 
                treat_drk + 
                cand, 
              data = df_filtered,
              model="random", 
              index=c("uid","cand"))
# Initial values for optim.....same procedure as before.
p1 <- c(rep(0.0001,length(varsused1)),rep(-7,length(varsused1.var)),0.01)
names(p1) <- c(varsused1,varsused1.var,"rho")
##
##
ML <- optim(p1, llk, ymat=ymat, xmat1.1=xmatc1, xmat1.2=xmatc2, 
            method='BFGS',hessian = TRUE)
##
##
Fisher_inv <- solve(ML$hessian)
mle_se <- sqrt(diag(Fisher_inv))
ols_se <- sqrt(diag(vcov(olsmod)))
plm_se <- sqrt(diag(vcov(plmmod)))
##
##
ols_coef <- olsmod$coefficients
plm_coef <- plmmod$coefficients
mle_coef <- ML$par
##
##
tab_coef <- data.frame(ols_coef,plm_coef,mle_coef[1:length(ols_coef)])
colnames(tab_coef) <- c('OLS Model','Panel Model(Authors)','Our Model')
rownames(tab_coef) <- c('Intercept','Job Status','Family Status','Middle Eastern','Skin Darkened','Cand')
tab_se <- data.frame(ols_se,plm_se,mle_se[1:length(ols_coef)])
colnames(tab_se) <- c('OLS Model','Panel Model(Authors)','Our Model')
rownames(tab_se) <- c('Intercept','Job Status','Family Status','Middle Eastern','Skin Darkened','Cand')
rownames_to_column(tab_coef, var = "Coefficients")
rownames_to_column(tab_se, var = "Standard Errors")
save(tab_coef,tab_se, file = 'data/interim/variance_data/varmod_au_tables2.rda')
##
##
#### STEP 6 : SIMULATIONS AND GRAPHS
##
##
## GENERATE TABLE OF RESULTS FOR MAIN MODEL (WITH ALL TREATMENTS)
resdf <- data.frame(matrix(0,nrow = 5,ncol = 10))
resdfse <- data.frame(matrix(0,nrow = 5,ncol = 10))
for(i in 1:length(bycon)){
  fg <- bycon[[i]]
  se_hats <- sqrt(diag(fg[[2]]))
  theta_hats <- fg[[1]]
  resdf[,i] <- theta_hats[7:11]
  resdfse[,i] <- se_hats[7:11]
}
country_list <- c("AU" ,"US", "CA", "UK", "NO", "CH", "KR", "JP", "FR", "DK")
colnames(resdf) <- country_list
colnames(resdfse) <- country_list
rownames(resdf) <- c('Intercept','Job Status','Family Status','Middle Eastern',
                     'Skin Darkened')
rownames(resdfse) <- rownames(resdf)
save(resdf,resdfse,file = 'data/interim/variance_data/restabs.rda')
##
##
## PLOT 1 : THE ESTIMATED PARAMETERS FOR THE VARIANCE FUNCTION.
##
##
plotlist2 <- list()
for(i in 1:length(bycon)){
  fg <- bycon[[i]]
  se_hats <- sqrt(diag(fg[[2]]))
  theta_hats <- fg[[1]]
  
  conresults <- cbind(theta_hats[(1+length(varsused)):(length(p)-1)]
                      ,se_hats[(1+length(varsused)):(length(p)-1)])
  colnames(conresults) <- c("b","se")
  
  conresults <- as.data.frame(conresults)
  conresults$var <- c('Intercept','Job Status','Family Status',
                      'Middle Eastern','Skin Darkened')
  conresults$var <- factor(conresults$var,
                           levels = rev(c('Intercept','Job Status','Family Status',
                                          'Middle Eastern','Skin Darkened')))
  pd <- position_dodge(0.07)
  a <- ggplot(conresults)+
    geom_pointrange(aes(var,y=b,ymin=b-(2*se),ymax=b+(2*se)),position = pd,col='#00BFC4',size=0.25)+
    geom_hline(yintercept = 0,linetype='dashed',col='#F8766D',size=0.25)+
    ylim(-3,3)+
    coord_flip()+
    theme(plot.title = element_text(hjust = 0.5),legend.position = "none")+
    labs(title = country_list[i],y = 'Coefficient',x = 'Variable')
  plotlist2[[i]] <- a
}
save(plotlist2,file = 'data/interim/variance_data/plotlist22_auth.rda')
##
##
## PLACEBO CHECK (COMPARE WITH EMPIRICAL VARIANCE)
##
##
varcompare <- colMeans(densdfgg)
varcompare <- data.frame(varcompare)
varcompare$empir <- 0
for(i in 1:nrow(varcompare)){
  df_f <- subset(df,df$countryt==rownames(varcompare)[i])
  df_f <- dplyr::select(df_f,'support')
  df_f <- na.omit(df_f)
  varcompare$empir[i] <- var(df_f$support)*((nrow(df_f)-1)/nrow(df_f))
}
colnames(varcompare) <- c('Our Model','Empirical')
save(varcompare, file = 'data/interim/variance_data/varcompare.rda')
##
##
## PLOT 2 : AGGREGATED VARIANCE BY COUNTRY.
simulator <- function(cname,m,f){
  dfcon <- df %>% filter(countryt == cname & treat_gender == 0)
  dfcon1 <- dplyr::select(dfcon,'support',varsused[-1],'uid')
  dfcon1 <- na.omit(dfcon1)
  attach(dfcon1)
  y <- support
  x1 <- data.matrix(cbind('intercept'=1, dfcon1[varsused[-1]]))
  x2 <- data.matrix(cbind('intercept'=1, dfcon1[varsused.var[-1]]))
  
  theta_sims <- mvrnorm(sims,mu = m,Sigma = f)
  
  qvec <- numeric(nrow(theta_sims))
  for(i in 1:nrow(theta_sims)){
    bv <- theta_sims[i,1:ncol(x1)]
    av <- theta_sims[i,(ncol(x1)+1):(ncol(x1)+ncol(x2))]
    means_samp <- x1%*%bv
    vars_samp <- x2%*%av
    qoi <- mean(exp(vars_samp))
    qvec[i] <- qoi
  }
  return(qvec)
  detach(dfcon1)
}
##
##
sims <- 100000
densdfgg <- data.frame(matrix(NA, nrow = sims, ncol = length(bycon)))
for(i in 1:length(bycon)){
  mvecmle <- bycon[[i]][[1]]
  fishinvmle <- bycon[[i]][[2]]
  densdfgg[,i] <- simulator(country_list[i],mvecmle,fishinvmle) 
}
##
##
colnames(densdfgg) <- country_list
data<- melt(densdfgg)
# save data
write_rds(data, 'data/interim/variance_data/sim_var_bycon.rds', compression = "gz")

labels <- data %>% 
  group_by(variable) %>%  
  summarise(xPos = mean(value),
            yPos = max((density(value))$y)+3)

plot1 <- ggplot(data,aes(x=value, fill=variable)) + geom_density(alpha=0.25)+
  scale_fill_manual(values=c("#00008B", "brown", "#FF0000",'black','#cc7722','#0b6623'
                             ,'#B22234','#C60630','light green','maroon'))+
  geom_text(data=labels, aes(x=xPos, y=yPos, colour=variable, label=variable,fontface = 'bold'),show.legend=F) +
  scale_color_manual(values=c("#00008B", "brown", "#FF0000",'black','#cc7722','#0b6623'
                              ,'#B22234','#C60630','light green','maroon'))+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "none")+
  labs(title = 'Distribution of the Aggregate Variance by country',x = expression(sigma_i^2),y = 'Density')
plot1
save(plot1,file='data/interim/variance_data/plot12_auth.rda')
plot11 <- ggplot(data,aes(x=value, fill=variable)) + geom_boxplot()+
  scale_fill_manual(values=c("#00008B", "brown", "#FF0000",'black','#cc7722','#0b6623'
                             ,'#B22234','#C60630','light green','maroon'))+
  scale_color_manual(values=c("#00008B", "brown", "#FF0000",'black','#cc7722','#0b6623'
                              ,'#B22234','#C60630','light green','maroon'))+
  labs(title = 'Polarization',x = expression(sigma^2~'(Polarization)'))
plot11
save(plot11,file='data/interim/variance_data/plot112_auth.rda')
##
##
## PLOT 3 : SIMULATED FIRST DIFFERENCES FOR JOB STATUS = 1 v. 0.
##
##
plotlist3 <- list()
for(i in 1:length(bycon)){
  df_filtered <- df %>% filter(countryt == country_list[i] & treat_gender ==0)
  df1 <- dplyr::select(df_filtered,'support',varsused.var[-1])
  df1 <- na.omit(df1)
  
  attach(df1)
  
  paramvec <- unlist(bycon[[i]][1])[(1+length(varsused)):(length(p)-1)] 
  sigmasqmat1 <- matrix(unlist(bycon[[i]][2]),ncol = length(p))
  sigmasqmat <- sigmasqmat1[(length(varsused)+1):(length(p)-1),(length(varsused)+1):(length(p)-1)]
  
  sims_mat <- rmvnorm(100000,paramvec,sigmasqmat)
  
  df1 <- data.matrix(df1)
  
  means_covars <- colMeans(df1)[-1]  # Setting all other covariates to their medians.
  means_covars <- c(1,means_covars)
  
  ## IF YOU WANT TO USE THE MEAN/MEDIAN VALUE APPROACH
  fdcalc <- function(hstat){
    means_covars_x <- means_covars
    means_covars_x[2] <- hstat  # Skill of Immigrant
    
    simvec_x <- sims_mat%*%means_covars_x
    simvec_x <- exp(simvec_x)
    
    xmean <<- mean(simvec_x)
    x_lb <<- quantile(simvec_x, probs = seq(0, 1, by= 0.00001))[2501]
    x_ub <<- quantile(simvec_x, probs = seq(0, 1, by= 0.00001))[97501]
  }
  
  ## IF YOU WANT TO USE THE OBSERVED VALUE APPROACH (PREFERRED BY US)
  fdcalc_hanmer <- function(hstat){
    covars_x <- df1
    covars_x[,2] <- hstat  # Skill of Immigrant
    
    sim_mean <- numeric(nrow(sims_mat))
    for(j in 1:nrow(sims_mat)){
      simvec_x <- covars_x%*%sims_mat[j,]
      simvec_x <- exp(simvec_x)
      sim_mean[j] <- mean(simvec_x)
    }
    xmean <<- mean(sim_mean)
    x_lb <<- quantile(sim_mean, probs = seq(0, 1, by= 0.00001))[2501]
    x_ub <<- quantile(sim_mean, probs = seq(0, 1, by= 0.00001))[97501]
  }
  
  hstat <- c(0,1)
  v <- as.data.frame(hstat)
  v$mean <- 0
  v$lb <- 0
  v$ub <- 0
  
  for(j in 1:nrow(v)){
    fdcalc_hanmer(v$hstat[j])
    v$mean[j] <- xmean
    v$lb[j] <- x_lb
    v$ub[j] <- x_ub
  }
  
  v$hstat <- c('Low','High')
  v$hstat <- factor(v$hstat,levels = c('Low','High'))
  
  detach(df1)

  a <- ggplot(v)+
    geom_pointrange(aes(x=hstat, y=mean, ymin=lb, ymax=ub),stroke=1,size=0.25)+
    geom_line(aes(y=mean,x=hstat,group=1),linetype = 'dashed')+
    labs(title = country_list[i])+
    xlab('Immigrant Job Status')+
    ylab(expression(sigma_i^2))
  plotlist3[[i]] <- a
}
save(plotlist3,file = 'data/interim/variance_data/plotlist3_auth.rda')
##
##
## PLOT 5 : SIMULATIONS OF OUR KEY QOI : CONSENSUS IN IMMIGRATION ATTITUDE. (DEFINED IN PAPER)
##
##
plotlist4 <- list()
for(i in 1:length(bycon)){
  df_filtered <- df %>% filter(countryt == country_list[i] & treat_gender ==0)
  df1 <- dplyr::select(df_filtered,'support',varsused[-1])
  df1 <- na.omit(df1)
  
  attach(df1)
  
  paramvec <- unlist(bycon[[i]][1])[1:(length(p)-1)] 
  sigmasqmat1 <- matrix(unlist(bycon[[i]][2]),ncol = length(p))
  sigmasqmat <- sigmasqmat1[1:(length(p)-1),1:(length(p)-1)]
  
  sims_mat <- rmvnorm(100000,paramvec,sigmasqmat)
  
  df1 <- data.matrix(df1)
  
  means_covars <- colMedians(df1)[-1]  # Setting all other covariates to their medians.
  means_covars <- c(means_covars,1,means_covars[varsused.var][-1])
  means_covars <- c(1,means_covars)
  
  ## IF YOU WANT TO USE THE MEAN/MEDIAN VALUE APPROACH
  fdcalc <- function(hstat){
    means_covars_x <- means_covars
    means_covars_x[which(names(means_covars_x)=='treat_hstat')] <- hstat  
    # Skill of Immigrant
    
    simvec_x.mu <- sims_mat[,1:length(varsused)]%*%means_covars_x[1:length(varsused)]
    simvec_x.var <- exp(
      sims_mat[,(length(varsused)+1):(length(p)-1)]%*%means_covars_x[(length(varsused)+1):(length(p)-1)]
    )
    
    mumean <- mean(simvec_x.mu)
    varmean <- mean(simvec_x.var)
    
    yvec <<- rnorm(100000,mean=mumean,sd=sqrt(varmean))
  }
  
  ## IF YOU WANT TO USE THE OBSERVED VALUE APPROACH (PREFERRED BY US)
  fdcalc_hanmer2 <- function(hstat){
    covars_x <- df1
    covars_x <- cbind(covars_x,1,covars_x[,which(colnames(df1)%in%varsused.var)])
    covars_x[,which(colnames(covars_x)=='treat_hstat')] <- hstat  
    # Skill of Immigrant
    
    sim_mean <- numeric(nrow(sims_mat))
    sim_vars <- numeric(nrow(sims_mat))
    for(j in 1:nrow(sims_mat)){
      simvec_x.mu <- covars_x[,1:length(varsused)]%*%sims_mat[j,1:length(varsused)]
      simvec_x.var <- exp(
        covars_x[,(length(varsused)+1):(length(p)-1)]%*%sims_mat[j,(length(varsused)+1):(length(p)-1)]
      )
      sim_mean[j] <- mean(simvec_x.mu)
      sim_vars[j] <- mean(simvec_x.var)
    }
    
    mumean <<- mean(sim_mean)
    mu_lb <<- quantile(sim_mean, probs = seq(0, 1, by= 0.00001))[2501]
    mu_ub <<- quantile(sim_mean, probs = seq(0, 1, by= 0.00001))[97501]
    varmean <<- mean(sim_vars)
    var_lb <<- quantile(sim_vars, probs = seq(0, 1, by= 0.00001))[2501]
    var_ub <<- quantile(sim_vars, probs = seq(0, 1, by= 0.00001))[97501]
  }
  
  v <- data.frame('dist' = rep(seq(0,0.5,0.01),2))
  v$jsval <- c(rep(0,nrow(v)/2),rep(1,nrow(v)/2))
  fdcalc_hanmer2(0)
  muval_0 <- mumean
  sdval_0 <- sqrt(varmean)
  muvallb_0 <- mu_lb
  sdvallb_0 <- sqrt(var_lb)
  muvalub_0 <- mu_ub
  sdvalub_0 <- sqrt(var_ub)
  fdcalc_hanmer2(1)
  muval_1 <- mumean
  sdval_1 <- sqrt(varmean)
  muvallb_1 <- mu_lb
  sdvallb_1 <- sqrt(var_lb)
  muvalub_1 <- mu_ub
  sdvalub_1 <- sqrt(var_ub)
  v$mu <- c(rep(muval_0,nrow(v)/2),rep(muval_1,nrow(v)/2))
  v$sd <- c(rep(sdval_0,nrow(v)/2),rep(sdval_1,nrow(v)/2))
  v$mulb <- c(rep(muvallb_0,nrow(v)/2),rep(muvallb_1,nrow(v)/2))
  v$sdlb <- c(rep(sdvallb_0,nrow(v)/2),rep(sdvallb_1,nrow(v)/2))
  v$muub <- c(rep(muvalub_0,nrow(v)/2),rep(muvalub_1,nrow(v)/2))
  v$sdub <- c(rep(sdvalub_0,nrow(v)/2),rep(sdvalub_1,nrow(v)/2))
  v$prob <- apply(v,1,FUN = function(x){
    pnorm(x[3]-x[1],x[3],x[4]) + pnorm(x[3]+x[1],x[3],x[4],lower.tail = FALSE)
  })
  v$problb <- apply(v,1,FUN = function(x){
    pnorm(x[5]-x[1],x[5],x[6]) + pnorm(x[5]+x[1],x[5],x[6],lower.tail = FALSE)
  })
  v$probub <- apply(v,1,FUN = function(x){
    pnorm(x[7]-x[1],x[7],x[8]) + pnorm(x[7]+x[1],x[7],x[8],lower.tail = FALSE)
  })
  v$jsval <- factor(v$jsval)
  levels(v$jsval) <- c('Low Job Status','High Job Status')
  detach(df1)

  a <- ggplot(v)+
    geom_line(aes(x=dist,y=prob,group=jsval,color=jsval),show.legend = FALSE)+
    geom_ribbon(aes(x=dist,ymin=problb,ymax=probub,fill=jsval,alpha=0.2),
                show.legend =  FALSE)+
    labs(title = country_list[i])+
    scale_color_manual(values=c("#FF0000","#00008B"))+
    xlab('Distance')+
    ylab('P(|Support|Job Status - E(Support|Job Status)| > Distance)')+
    theme(plot.title = element_text(hjust = 0.5))
  
  plotlist4[[i]] <- a
}
save(plotlist4,file = 'data/interim/variance_data/plotlist4_auth.rda')
