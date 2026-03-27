library(sf)
library(MASS)
#############FUNCTIONS#######################
cond_dist = function(mu,sigma,known){ #Function to calculate Schur complement
  allin = which(!is.na(known))
  mu_1 = mu[-allin];mu_2 = mu[allin]
  sigma_11 = sigma[-allin,-allin]; sigma_12 = sigma[-allin,allin]
  sigma_21 = sigma[allin,-allin]; sigma_22 = sigma[allin,allin]
  some_matrix = sigma_12%*%solve(sigma_22)
  return(list(newmu=mu_1+some_matrix%*%(known[allin]-mu_2),newsigma=sigma_11-some_matrix%*%sigma_21))
}

updated_prediction = function(mu=priors,sigma=in_cov,known=results){ #Calculate conditional distributions of each candidates share
  known = known/rowSums(known)
  counties = which(is.na(known[,1]))
  update = known
  for(i in 1:ncol(mu)){
    x = cond_dist(mu[,i],sigma, known=known[,i])
    update[counties,i] = pmax(x$newmu,0.01)#Conditional mean can theoretically be negative, so adjust
  }
  update = update/rowSums(update)
  return(list(newmu=update,newsigma=x$newsigma))
}

sims = function(data,mu,sigma,nsim,unknown,cands=c('braun','messer','rokita')){
  mu = as.matrix(mu); k = length(cands)
  turnout = indy$est_share
  copula = pnorm(mvrnorm(nsim*k,mu=numeric(nrow(sigma)),Sigma=sigma)/sqrt(diag(sigma)))#Set Gaussian copula
  shp = 1/(4*diag(sigma))-1; params = shp*mu[unknown,] #Calculate shape parameters for Gamma component of Dirichlet
  final = matrix(0,nrow=nsim,ncol=k) #Initialize matrix to store simulation values
  for(i in 1:nsim){
    props = qgamma(copula[(i-1)*k+1:k,],t(params),scale=1) #Simulate strengths
    props = t(props)/colSums(props) #Convert strengths to proportions
    comp = mu; comp[unknown,] = props #Add in simulation with counties already known
    final[i,] = turnout%*%comp
  }
  return(final)
}

plot_multiparty = function(shapes=in_shapes,results=new_preds$newmu,pal=c('red','green','orange'),border='black'){#Map of county winners
  cols = numeric(length(results))
  for(i in 1:nrow(shapes)){
    if('other'%in%colnames(results)){winner = which.max(results[i,-ncol(results)])}else{winner = which.max(results[i,])}
    cols[i] = colorRampPalette(c('white',pal[winner]))(100)[cut(results[i,winner]/sum(results[i,]),seq(0,1,0.01))]
  }
  plot(shapes$geometry,col=cols,border=border)
  if('other'%in%colnames(results)){b = length(cands);legend('bottomright',legend=cands[-b],col=pal,fill=pal)}
  else{legend('bottomright',legend=cands,col=pal,fill=pal)}
}

  #CONDITIONAL MULTIVARIATE NORMALITY 
jj = as.data.frame(MASS::mvrnorm(1e5,mu=numeric(4),Sigma=25*(diag(4)*0.4+0.6)))
names(jj) = c('Arizona','Georgia','Nevada','Pennsylvania')
cond_jj = jj[jj$Arizona<0 & jj$Nevada<0,c(2,4)]
colMeans(cond_jj); cov(cond_jj)
contour(kde2d(cond_jj$Georgia,cond_jj$Pennsylvania),xlab='Georgia',ylab='Pennsylvania',xlim=c(-15,10),ylim=c(-15,10),
        col=hcl.colors(10,'Spectral'),main='PA/GA Margins given Democratic wins in AZ/NV'); grid()
###########538 PLOTS##################
natesilver_2016 = c(22.3,7.4,2.2,20.7,-22.9,-4,-12.7,-12.5,-70.5,-0.7,4,-23.7,19.6,-12.9,11.6,2.9,12.5,18.2,16.1,-7.4,-13.9,-0.4,-25.7,-23.4,-4.2,-5.8,13.1,10.1,
               15,17.8,15.5,1.8,36.1,-1.2,-3.6,-11.5,-5.8,-19,-0.7,23.1,1.9,26,-9.2,-3.7,-14.4,7,15.5,12,8.6,10.5,-27.4,-5.6,-13.3,26.5,-5.3,35)
natesilver_2020 = c(20,8.4,-2.6,22.7,-30,-11.7,-24.1,-25.7,-85.9,-2.5,-0.9,-36.6,22,-17.9,10.8,1.5,13.2,18,18.6,-11.8,-21,-3.2,-31.4,-34.9,-8,-9.1,14.3,9.5,
                    6.3,18,12.2,-3.2,47.1,-6.1,-10.6,-20.3,-13,-29.1,-1.7,21.7,0.6,25.2,-19.6,-4.7,-27.2,7.5,17,15.7,1.5,13.8,-38.1,-12.5,-23.8,30.7,-8.3,34.8)

results_2016 = c(27.7,14.7,3.5,26.9,-30.1,-4.9,-13.6,-11.4,-86.8,1.2,5.1,-32.2,31.8,-17.1,19.2,9.4,20.6,29.8,19.6,-3,-14.8,10.3,-26.4,-27.2,0.2,-1.5,
                 17.8,18.6,20.4,25.1,20.7,2.2,54.2,-2.4,-0.4,-14.1,-8.2,-22.5,3.7,35.7,8.1,37.1,-11,0.7,-15.5,14.3,29.8,26,9,18.1,-26.4,-5.3,-15.7,42.1,0.8,46.3)
results_2020 = c(25.5,10.1,-0.3,27.6,-29.2,-13.5,-20.1,-19.0,-86.8,3.4,-0.2,-29.5,30.8,-17.0,16.1,8.2,14.7,25.9,18.6,-9.1,-23.1,7.4,-33.2,-33.5,-2.8,-7.1,
                 16.6,15.4,16.4,19.1,14.9,-6.5,53.0,-2.4,-7.4,-15.9,-10.8,-23.1,1.4,33.4,8.0,33.1,-16.1,-1.2,-20.8,11.7,26.2,23.2,5.6,20.5,-35.4,-10.1,-19.2,38.9,-0.6,43.4)

plot(results_2016,results_2016-natesilver_2016,xlab='2016 Trump Margin',ylab='Difference between Actual and Predicted Trump Margin',
  main='FiveThirtyEight 2016 Margins Compared with Actual Margins',pch=19); grid(); abline(0,0)
plot(results_2020,results_2020-natesilver_2020,xlab='2020 Trump Margin',ylab='Difference between Actual and Predicted Trump Margin',
     main='FiveThirtyEight 2020 Margins Compared with Actual Margins',pch=19); grid(); abline(0,0)
##########KENTUCKY PROBLEM TEST#######################
ddhq_2020 = c(21.9274,7.9004,-1.3926,28.4000,-33.0878,-12.6256,-22.1156,-21.7168,-83.8774,-2.1202,-0.7022,-35.0134,33.4726,-17.8004,10.9386,0.8308,13.8894,19.6886,21.3392,-12.5830,
              -21.6552,-1.6900,-28.5992,-34.5770,-7.6278,-9.7812,16.3080,9.0946,8.2106,25.3188,16.8430,-3.0444,39.6276,-5.4614,-10.6130,-20.3206,-12.1528,-28.2500,-0.9770,31.9950,
              1.3894,26.5186,-14.9410,-4.6988,-18.4302,7.7384,25.2370,18.9528,1.9576,17.0386,-30.2764,-11.7738,-22.7916,24.0374,-7.5674,37.2708)
state_ev = c(9,3,11,6,55,9,7,3,3,29,16,4,4,20,11,6,6,8,8,2,1,1,10,11,16,10,6,10,3,2,1,1,1,6,4,14,5,29,15,3,18,7,7,20,4,9,3,11,38,6,3,13,12,5,10,3)

early_states = c(18,51,15,54,52,46,1,27,48,42)
cormat1 = diag(56)*20+30
cormat2 = diag(sqrt(16+2*abs(ddhq_2020)))%*%(diag(56)*0.4+0.6)%*%diag(sqrt(16+2*abs(ddhq_2020)))
for(i in 1:10){
  k = early_states[1:i]
  known = rep(NA,56); known[k] = results_2020[k]
  #Model 1 vs. Model 2 updated prediction
  update1 = cond_dist(ddhq_2020,cormat1,known); update2 = cond_dist(ddhq_2020,cormat2,known)
  jj1 = mvrnorm(1e5,update1$newmu,update1$newsigma); jj2 = mvrnorm(1e5,update2$newmu,update2$newsigma) 
  #Model 1 vs. Model 2 mean electoral votes
  ev1 = ifelse(jj1>0,1,0)%*%state_ev[-k] + sum(ifelse(results_2020[k]>0,1,0)*state_ev[k])
  ev2 = ifelse(jj2>0,1,0)%*%state_ev[-k] + sum(ifelse(results_2020[k]>0,1,0)*state_ev[k])
  print(c(mean(ev1),mean(ev2)))
}



#########MVRNORM EXAMPLE#########################
x = mvrnorm(1e4,mu=c(5,-3),Sigma=diag(2)*10+6)
plot(x,cex=0.5,pch=18,col=ifelse(x[,1]>0,'orange','green'),main='',
     xlab='Race 1',ylab='Race 2',xlim=c(-15,20),ylim=c(-15,10)); abline(0,0); abline(0,1e5)
#Simulate more draws
samps = x[which(x[,1]<0),]; old_mean = colMeans(x); old_cov = cov(x)
while(nrow(samps)<10000){#Keep adding draws until 10,000 samples with the desired condition
  new = floor(10000*(length(x[,1])/nrow(samps)))
  new_draws = mvrnorm(new,mu=old_mean,Sigma=old_cov)
  samps = rbind(samps,new_draws[which(new_draws[,1]<0),])
}
plot(samps,cex=0.5,pch=18,col=ifelse(samps[,1]>0,'orange','green'),main='',
     xlab='Race 1',ylab='Race 2',xlim=c(-15,20),ylim=c(-15,10)); abline(0,0); abline(0,1e5)
mean(samps[,2]); var(samps[,2])


#########INDIANA PRIMARY EXAMPLE##############
indy = st_read('in_primary.shp')
distance = unclass(st_distance(st_centroid(indy$geometry)))
x = max(distance); in_cov = exp(-distance/x)*.01
cands = c('braun','messer','rokita')

#Set initial multivariate gamma distribution for each candidate, then plot
priors = matrix(0,nrow=92,ncol=3); colnames(priors) = cands
for(i in 1:length(cands)){
  if(cands[i]=='other'){priors[,i] = 0.5}
  else{priors[,i] = 0.8-(0.8-0.2)*(distance[which(indy$home==cands[i]),]/max(distance))}
}
priors = priors/rowSums(priors)

plot_multiparty(indy,priors)
#SIMS
#Sample 20 random counties as completed
set.seed(14000605) #Set seed for replicability of results
results = st_drop_geometry(indy[,5:7])
results[sample(1:92,72),]=NA #20 counties reporting
new_preds = updated_prediction(mu=priors,sigma=in_cov,known=results)
plot_multiparty(shapes=indy,results=new_preds$newmu,pal=c('red','green','orange'))
#10000 simulations of primary
jk = sims(indy,new_preds$newmu,new_preds$newsigma,nsim=10000,unknown=which(is.na(results[,1])),cands=cands)
table(apply(jk,1,which.max)) #Number of times each candidate wins
for(i in 1:ncol(jk)){print(quantile(jk[,i],c(0.025,0.5,0.975),na.rm=T))}#Confidence intervals
plot_multiparty(shapes=indy,results=st_drop_geometry(indy[,5:7]),pal=c('red','green','orange'))
