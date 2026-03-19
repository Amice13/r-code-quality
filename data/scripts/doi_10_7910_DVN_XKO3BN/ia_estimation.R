#################################
library("rstan")
#Please consult https://mc-stan.org/users/interfaces/rstan.html for instructions on downloading and installing RStan.

#set working dir to where file is
#setwd("")

load("ST20_LSQ_replication_IA_pre-estimation.RData")

inits3<-
  list(inits[[1]],inits[[2]],
       list(nu=20+rnorm(1,0,0.2),sigma2=sigmainit+rnorm(length(sigmainit),0,0.1),
            q=qinit+rnorm(length(qinit),0,0.2),theta2=thetainit2+rnorm(length(thetainit2),0,0.1),
            tau=tauinit2+rnorm(length(tauinit2),0,0.1),pos=m4+rnorm(1,0,0.1),p=p4+rnorm(1,0,0.2),ma=-1+rnorm(1,0,0.2),mi=-1+rnorm(1,0,0.2),
            beta=betainit+rnorm(length(betainit),0,0.2),delta=1+rnorm(1,0,0.2)))

stan.data3<-stan.data
stan.data3[[which(names(stan.data3)=="sig")]]<-NULL #unneccessary for analysis

## Model
#   N.B. compared to the model stored in the stan.fit3 object, the below model has been cleaned by removing superfluous
#   commented out lines of code, and updating comments
stan.code3<-"
  data {
    int<lower=1> J; // number of legislators cosponsor (for now keep same in M and N)
    int<lower=1> L; // number of legislators voting (for now keep same in M and N)
    int<lower=1> K; // number of bills consponsor (for now keep same in M and N)
    int<lower=1> B; // number of bills voting (for now keep same in M and N)
    int<lower=1> N; // number of observations cosponsors
    int<lower=1> M; // number of observations vote
    int<lower=1,upper=J> j[N]; // cosponsor legislator for observation n
    int<lower=1,upper=K> k[N]; // cosponsor bill for observation n
    int<lower=1,upper=L> l[M]; // voting legislator for observation n
    int<lower=1,upper=B> b[M]; // voting bill for observation n
    int<lower=0,upper=1> y[N]; // cosponsor of observation n
    int<lower=0,upper=1> z[M]; // vote of observation m
    int<lower=0,upper=1> party[N]; // party of legislator for observation n
    int<lower=0,upper=1> majcom[N]; // vote of observation n
    int<lower=0,upper=1> mincom[N]; // vote of observation n
  }
parameters {
  vector[B] tau;             
  vector[B] beta;  
  vector[J] theta2;
  vector[K] pos;
  real<lower=0,upper=15> sigma2[K];
  vector[K] q; //fixed effect of bill
  real p; //effect of party (Republicans)
  real ma; //effect of majority committee leadership
  real mi; //effect of minority committee leadership
  real<lower=0,upper=50> nu; //parameter for inverse gamma distribution
}
transformed parameters{
  vector[K] sigma;
  vector[L] theta;
  real mtheta3;
  real sdtheta3;
  mtheta3=mean(theta2);
  sdtheta3=sd(theta2);
  theta=(theta2-mtheta3)./sdtheta3;  
  sigma[1]=1;
  for(s in 2:K) sigma[s]=sigma2[s];
}
model{
  vector[N] mu;
  vector[M] mu2;
  for(s in 1:N) 
  mu[s]=(-(pos[k[s]]-theta[j[s]])^2-p*party[s]-ma*majcom[s]-mi*mincom[s]-q[k[s]])/sigma[k[s]];
  for(s in 1:M) 
  mu2[s]=beta[b[s]]*(theta[l[s]]- tau[b[s]]);
  nu~normal(20,5);
  sigma2~inv_gamma(nu,nu);
  p~normal(0,1);
  ma~normal(0,2);
  mi~normal(0,2);
  pos~normal(0,1);
  q~normal(0,5);
  tau ~ normal(0, 1);
  beta ~ normal(0, 5);
  theta2 ~ normal(0, 1);
  theta2[517] ~ normal(1,1); //Lalk (IA/R/H)
  theta2[658] ~ normal(1,1); //Reynolds American Inc. (RAI Services Company)...
  theta2[246] ~ normal(-1,1); //Hunter (IA/D/H)
  theta2[296] ~ normal(-1,1); //IA. Citizens for Community Improvement
    y ~  bernoulli_logit(mu );
    z ~  bernoulli_logit( mu2 );
}
"

aaa<-data.frame(name=rownames(df15),ideal=as.numeric(id$xbar[,1]),stringsAsFactors = F)
aaa<-aaa[order(aaa$ideal),]
which(sig!=0)
rownames(df15)[which(sig!=0)]
which(rownames(df15)=="Hunter (IA/D/H)")
which(rownames(df15)=="Lalk (IA/R/H)")

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
Sys.setenv(LOCAL_CPPFLAGS = '-march=native')

stan.fit3 <- stan(model_code=stan.code3, data=stan.data3, iter=2000, warmup=1000,
                  chains=3, thin=1,init = inits3, control = list(adapt_delta=0.99),sample_file = "IA_992.csv",open_progress=F)
#N.B. The model was estimated with R version 4.0.1
save.image("ST20_LSQ_replication_IA_estimation.RData")
