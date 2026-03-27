#Maximum Pseudo-likelihood Estimation for General Count ERGMs
#
#Currently, the ergm package does not directly support MPLE for count ERGMs,
#instead relying on contrastive divergence.  Unfortunately, CD is both slow and
#unreliable in many cases, while full MLE is far too expensive at present for
#large graphs with high-value counts.  In that regime, the MPLE may be worth
#considering.  Here, I have implemented one version of this approach.
#
#To get started, look to the function ergmCntMPLE, below.  I have also implemented
#coef, print, and summary methods for it, because I'm nice that way.
#
#Some general notes (more for my own reference):
#
#For a general count ERGM, the conditional edge value probability is
#
# Pr(Y_i=y|Y^c_i) = h(y,Y^c_i) exp(q t(y,Y^c_i)) / sum_{k=0}^Inf h(k,Y^c_i) exp(q t(k,Y^c_i))
# = 1/[sum_{k=0}^Inf h(k,Y^c_i)/h(y,Y^c_i) exp(q(t(k,Y^c_i)-t(y,Y^c_i)))]
#
# The PL is product of the above.
#
#It is useful to be aware of some standard reference measure ratios:
#
#  Uniform/Geometric: h(k,Y^c_i)/h(y,Y^c_i) = 1
#  Binomial: h(k,Y^c_i)/h(y,Y^c_i) = k!(m-k!)/(y!(m-y)!)
#  Poisson: h(k,Y^c_i)/h(y,Y^c_i) = y!/k!


#Load required packages
library(parallel)
library(ergm)
library(sna)
library(statnet.common)
library(sampling)
library(Rcpp)

#Fit a count ERGM to a valued network via MPLE, using precomputed edge variable
#properties.
#
#Arguments:
#  formula - an ergm formula
#  response - a character string containing the name of the edge variable to use
#    for edge values
#  reference - the reference measure to use; note that some reference measures 
#    require not-yet-implemented parameter space constraints to be trustworthy,
#    and may not give reliable results
#  max.count - the maximum count value attainable (Inf if unbounded)
#  max.count.safety - safety margin for partial sums, as multiple of the maximum
#    observed count value (for max.count.edgewise=FALSE) or of four times the square
#    root of the per-edge observed value (for max.count.edgewise=TRUE); higher values
#    may give more accurate results, but will increase memory requirements 
#    approximately linearly
#  max.count.edgewise - logical; should maximum and minimum counts be assessed on a
#    per-edge basis?
#  must.count - an integer that sets the upper bound of the interval that will always
#    be calculated for each edge variable when max.count.edgewise=T. By default, 
#    must.count=5, integrating {0,1,2,3,4,5} for every edge variable's support.
#    This will also be the full set of values for edge variables with 0 observed value.
#    For high-variance edge distribution, increasing this number is highly recommended; 
#    using mean or median of the edge distribution is usually conservatively safe
#  count.samples - maximum number of unique y values per edge to use for the local
#    normalizing factor; where lower than the number of elements in the normalizing
#    sum (determined by the safety margins), a not-at-all-clever rectangular
#    approximation is used (with evenly spaced sample values standing in for batches
#    of counts).  Lower values can save considerably on memory when edge values are
#    high, but can introduce numerical instability: if convergence begins to fail,
#    higher values may be needed.
#  regularization - type of regularization to be used (if desired); "none" provides
#    the raw MPLE, "L1" gives an L1-penalized MPLE (akin to LASSO), "L2"
#    provides an L2-penalized MPLE (akin to MAP with a Gaussian prior), 
#    "pseudoHuber" penalizes by the pseudo Huber function (which looks like L1
#    beyond a small radius from the origin, and otherwise like L2), and "L1pH"
#    uses L1 regularization by iterative pseudo-Huber (reducing the critical
#    radius each time).  Note that "L1pH" is more likely to work well than "L1,"
#    but optimization is slower.
#  regularization.param - penalty parameter for the regularizer (should be positive);
#    for pseudo-Huber, the second element sets the critical radius for the L2 to
#    L2 transition
#  sample.size - sample size for edge variable subsampling; if greater than the
#    number of edge variables, no sampling is performed (the default)
#  sample.method - sampling method to employ for estimating the pseudo-likelihood
#    (if sampling is used).  "weighted" sampling attempts to stratify by observed
#    values, oversampling edges with rarer values; "random" samples edge variables
#    uniformly
#  weight.type - type of weighting scheme to be used with the "weighted" sampling
#    method.  "TNT" stratifies by zero versus non-zero values, much like the 
#    "tie/no-tie" proposal method used in ergm; "flatval" attempts to produce
#    as uniform a sample as possible with respect to the set of tie values (thus
#    aggressively oversampling rarely observed counts)
#  coef.init - optionally, an initial coefficient vector to use
#  cores - number of cores to use when calculating changescores and such; more
#    cores may increase the memory cost, but will obviously speed things up
#  optim.method - "trust" if trust region optimization is employed, otherwise the
#    method for optim to use (defaults to "BFGS")
#  WtSumAsSampSiz - whether the summation of the inverse weight (=degrees of freedom)
#    is equal to the sample size of edge variables. It will be equal to the total edge
#    count if set as False. Experiments show that setting it to be the sample size
#    yields better calibration of uncertainty
#  seed - optionally, an integer that specifies the seeds for sampling edge vairables
#  prep - optionally, inputting pre-computed change score from 'ergmCntPrep' function.
#    This allows users to separate change score computation and parameter optimization
#    processes. By default, the function will perform the two processes consecutively
#    without saving the interim (giant) change score file.
#  ... - optionally, additional arguments to optim() or trust()
# 
#Value: an object of class ergmCntMPLE, which has print, summary, and coef methods.
#
#Notes:
#
#  This function first pre-computes properties of the edge variables (changescores and
#reference measure ratios), and then performs optimization using this pre-computed
#information.  This is relatively fast, but can consume a lot of RAM: roughly,
#you can expect the memory demands to scale as the number of edge variables times
#the number of statistics times the highest observed value multipled by max.count.safety
#when max.count.edgewise=FALSE, or otherwise something like the number of edge variables
#times the number of statistics times the 4 times the square root of the largest
#value times max.count safety.  (The latter may be much lower in practice, however,
#if many edges have low counts.)  Where there is no maximum count, precomputation also
#requires that we decide in which values to consider when computing conditional 
#likelihoods (since the changescore must be taken for every possible value of every edge
#variable).  When max.count.edgewise=FALSE, we use the largest observed edge value times 
#max.count.safety to determine this, using all states 0,...,[max count]*max.count.safety
#as the de facto edgewise support.  For very Poissonian networks, it is possible to make
#max.count.safety quite small (something like 1/sqrt(max val)), but by default it is set
#to a fairly high value.  Since memory costs scale roughly linearly in this parameter, 
#however, this is not always ideal.  Setting max.count.edgewise=TRUE provides a more
#nuanced approach, where the support is determined for each edge variable.  The set of
#values examines in this case is 0, 1, floor(y-max.count.safety*4*sqrt(y)), ...,
#ceily(y+max.count.safety*4*sqrt(y)), with y being the observed edge value.  (Where the
#first sum would produce negative numbers, is truncated.)   These bounds are based on the
#obesrvation that if the conditional likelihood of y is at all Poissonian, the probability
#of the observation falls away very rapidly as the expectation moves away from it.  We
#ensure that 0 and 1 are included since there are terms that greatly inflate the probability
#of these states (and one wants to be sure that they contribute to the normalization).  
#Note that this is a much, much more efficient approach than the above, but it is also
#potentially less accurate if the conditional expectation is not strongly localized.
#
#When the edge value ranges are extremely large, even the above may not be good enough
#to make computation feasible.  For that reason, we also allow for subsampling of edge
#values within the target range.  This is controlled by count.samples, which determines
#the maximum number of edge values to be used for the pseudo-likelihood calculation.
#Currently, when count.samples is smaller than the range, we evenly sample edge values
#along the range, then employing a very crude rectangular approximation to the sum (i.e.,
#we use each calculated value as a stand-in for a series of values, and weight it
#accordingly).  This is not very clever, and may eventually be improved upon; it also
#necessitates storing another vector for every edge variable in the worst case, which
#increases storage costs by around 1/3.  However, the gains are that (1) we can put a 
#hard upper limit on memory consumption, and (2) by restricting count.samples, we can
#obtain very large performance enhancements on some graphs.  The main liability is that
#the approximation can introduce numerical instability in some cases (e.g., when the
#expected count distribution under the model becomes extremely peaked, and the peaks
#are so narrow that they begin to get "lost" between the sample points).  If using 
#this option and optimization fails, you may need to increase count.samples (though
#it is also possible that the max.count.safety argument is too small).
#
#Another way to save memory (and time) is to approximate the pseudo-likelihood using a
#subsample of edge variables.  By default, a sampling design is employed that draws
#edge variables without replacement with probability approximately inverse of the 
#frequency of some function of their respective values.  (Only dichotomized values are
#used for TNT, while flatval uses the full distribution of exact counts.) This has the
#effect of oversampling rare values, and undersampling very common values (e.g., 0, in 
#zero-inflated networks).  Note that the without-replacement sampling method used here
#can work poorly when the sample size is not very small compared to the number of edge
#variables, and a warning will be generated when it is used with samples that may be 
#too large in relative terms.  The alternative is simple random sampling (without 
#replacement) of edge variables, which is also available.
#
#  It should be borne in mind that the usual caveats of the MPLE apply to the
#estimates produced by this function (especially standard errors and nominal
#p-values, which tend to be badly overconfident in binary ERGMs).  Our simulation studies
#suggest that the MPLE can work very well in count-valued networks, especially when the
#edge variance is large, but one should still be careful until we have a larger number
#of studies under our belts.  Caveat emptor.
#  
ergmCntMPLE<-function(formula, response, reference=c("uniform", "binomial", "geometric", "poisson"), max.count=Inf, max.count.safety=4, max.count.edgewise=TRUE, count.samples=Inf, regularization=c("none", "L2", "L1", "pseudoHuber", "L1pH"), regularization.param=c(1,1e-4), sample.size=Inf, sample.method=c("weighted", "random"), weight.type=c("TNT", "flatval"), coef.init=NULL, cores=1, optim.method="trust", 
                      seed=NULL,
                      must.count=5,
                      prep=NULL,
                      WtSumAsSampSiz=T, 
                      ...){
    nw0 <- ergm.getnetwork(formula);response2 <- response #here we use response2 because ergm_preprocess_response consumes the response string, but we need it later for ergmCntPrep (submitted as Issue#463 on github/ergm)
    ergm_preprocess_response(nw=nw0, response=response2)
      #generate new formula that use preprocessed networks
    mod<-ergm_model(formula=as.formula(paste0("nw0~",as.character(formula)[3])))
    np<-nparam(mod)
    parnam<- param_names(mod)
    rm(nw0);gc()
    if(is.null(prep)){
      prep<-ergmCntPrep(formula=formula,response=response,reference=reference, max.count=max.count, max.count.safety=max.count.safety, max.count.edgewise=max.count.edgewise, count.samples=count.samples, sample.size=sample.size, sample.method=sample.method, cores=cores, weight.type=weight.type, seed=seed, must.count = must.count,WtSumAsSampSiz=WtSumAsSampSiz)
    }
    rfun<-switch(match.arg(regularization),
      none=0,
      L1=1,
      L2=2,
      pseudoHuber=3,
      L1pH=3
    )
    if(is.null(coef.init))
      coef.init<-rnorm(np,sd=1e-3)
    if(optim.method=="trust"){
      require(trust)
      usetrust<-TRUE
      ps<-rep(1,np)
    }else
      usetrust<-FALSE
    if(match.arg(regularization)=="L1pH")
      regularization.param[2]<-0.1
    if(usetrust)
      fit<-trust(objfun=ergmCntNLPLDeriv, parinit=coef.init, rinit=1, rmax=100, parscale=ps, obj=prep, rtype=rfun, rparam=regularization.param, ...)
    else
      fit<-optim(coef.init, fn=ergmCntNLPL, obj=prep, rtype=rfun, rparam=regularization.param, method=optim.method, ...)
    while((match.arg(regularization)=="L1pH")&&(regularization.param[2]>1e-7)){
      regularization.param[2]<-regularization.param[2]/2
      cat("\tReducing pseudo-Huber radius to",regularization.param[2],"\n")
      if(usetrust)
        fit<-trust(objfun=ergmCntNLPLDeriv, parinit=fit$argument, rinit=1, rmax=100, parscale=ps, obj=prep, rtype=rfun, rparam=regularization.param, ...)
      else
        fit<-optim(fit$par, fn=ergmCntNLPL, obj=prep, rtype=rfun, rparam=regularization.param, method=optim.method, ...)
    }
    if(usetrust)
      fit$par<-fit$argument
    fit$coef<-fit$par
    names(fit$coef)<-parnam
    fit$pseudo.deviance<-2*fit$value
    if(usetrust)
      fit$pll.hessian<- -fit$hessian
    else
      fit$pll.hessian<- -optimHess(fit$par,fn=ergmCntNLPL,obj=prep, rtype=rfun, rparam=regularization.param)
    fit$cov<-try(solve(-fit$pll.hessian))
    if(inherits(fit$cov,"try-error")){
      require(MASS)
      fit$cov<-ginv(-fit$pll.hessian)
    }
    fit$formula<-formula
    fit$reference<-reference
    fit$response<-response
    fit$regularization<-match.arg(regularization)
    fit$regularization.param<-regularization.param
    fit$sample.method<-match.arg(sample.method)
    if(match.arg(sample.method)=="weighted")
      fit$weight.type<-match.arg(weight.type)
    class(fit)<-"ergmCntMPLE"
    fit
}


print.ergmCntMPLE<-function(x,digits = max(3, getOption("digits") - 3),...){
  cat("\nMPLE Coefficients:\n")
  print.default(format(x$coef, digits = digits), print.gap = 2, quote = FALSE)
  invisible(x)
}


summary.ergmCntMPLE<-function(object, ...){
  object$se<-diag(object$cov)^0.5
  object$z.score<-object$coef/object$se
  object$p.value<-2*(1-pnorm(abs(object$z.score)))
  class(object)<-"summary.ergmCntMPLE"
  object
}


print.summary.ergmCntMPLE<-function(x,digits = max(3, getOption("digits") - 3),...){
  cat("\n==========================\n")
  cat("Summary of model fit\n")
  cat("==========================\n\n")
  cat("Formula:   ")
  print(x$formula)
  cat("\n")
  cat("Response variable:",x$response,"\nReference measure:",x$reference,"\n")
  cat("\nRegularization:",x$regularization)
  if(x$regularization!="none")
    cat(" (regularization parameter ",paste(x$regularization.param,collapse=", "),")\n",sep="")
  else
    cat("\n")
  cat("\nMaximum Pseudolikelihood Results:\n")
  tab<-cbind(x$coef,x$se,x$z.score,x$p.value)
  colnames(tab)<-c("Estimate", "Std.Err", "Z value", "Pr(>z)")
  printCoefmat(tab, digits = digits, signif.stars = TRUE, P.values = TRUE, has.Pvalue = TRUE, na.print = "NA", cs.ind = 1:2, tst.ind = 3, ...)
  cat("\nPseudo-deviance",x$pseudo.deviance,"on",length(x$coef),"model degrees of freedom\n\n")
  invisible(x)
}


coef.ergmCntMPLE<-function(object, ....){
  object$coef
}


#Negative log pseudo-likelihood for ERGM count models, using precomputed 
#quantities.
cppFunction('double ergmCntNLPL(NumericVector coef, List obj, int rtype, NumericVector rparam){
  double lpl=0.0,ils,pd,reg=0.0;
  int i,j,k;

  //Make sure that we were passed a legitimate input, lest we crash
  if(!obj.inherits("ERGMCntPrep"))
    stop("Must be called with an ERGMCntPrep object.");

  //Go ahead and coerce the importance weights
  NumericVector iw = as<NumericVector>(obj["iw"]);

  //Gonna add it up
  for(i=0;i<iw.size();i++){
    //Set up the variables we will need
    ils=0.0;
    NumericVector rmr = as<NumericVector>(as<List>(obj["rmr"])[i]);
    NumericMatrix cs = as<NumericMatrix>(as<List>(obj["cs"])[i]);
    NumericVector ycwt = as<NumericVector>(as<List>(obj["ycwt"])[i]);
    //Find the inverse log sum
    for(j=0;j<rmr.size();j++){
      pd=0.0;
      for(k=0;k<coef.size();k++)
        pd+=cs(j,k)*coef[k];
      if(j==0)
        ils=rmr[j]+pd+log(ycwt[j]);
      else
        ils=R::logspace_add(ils,rmr[j]+pd+log(ycwt[j]));
    }
    //Add to the total (multiplying by inverse inclusion weight)
    lpl-=iw[i]*ils;
  }
  
  //Check process
  //Rcout << "Coef:";
  //for(i=0;i<coef.size();i++)
  //  Rcout << " " << coef[i];
  //Rcout << " LPL: " << lpl << "\\n";

  //Compute the regularization penalty, if any
  if(rtype==1){                              //L1 regularization
    for(i=0;i<coef.length();i++)
      reg+=fabs(coef[i]);
    reg*=rparam[0];
  }else if(rtype==2){                        //L2 regularization
    for(i=0;i<coef.length();i++)
      reg+=coef[i]*coef[i];
    reg*=rparam[0];
  }else if(rtype==3){                        //pseudo-Huber regularization
    for(i=0;i<coef.length();i++)
      reg+=rparam[1]*(sqrt(1.0+coef[i]*coef[i]/(rparam[1]*rparam[1]))-1.0);
    reg*=rparam[0];
  }

  //Return the negative log pseudo-likelihood (plus any regularization penalty)
  return -lpl+reg;
}')


#Negative log pseudo-likelihood and derivatives for ERGM count models, using precomputed 
#quantities.  Note that this output is formatted for the trust package.
cppFunction('List ergmCntNLPLDeriv(NumericVector coef, List obj, int rtype, NumericVector rparam){
  double lpl=0.0,ils,pd,reg=0.0;
  int i,j,k,l,p=coef.size();
  NumericVector gr(p),igr(p);                //Gradient of the lpl (penalized)
  NumericMatrix hess(p,p),ihess(p,p);        //Hessian of the lpl (penalized)

  //Make sure that we were passed a legitimate input, lest we crash
  if(!obj.inherits("ERGMCntPrep"))
    stop("Must be called with an ERGMCntPrep object.");

  //Go ahead and coerce the importance weights
  NumericVector iw = as<NumericVector>(obj["iw"]);

  //Gonna add it up
  for(i=0;i<iw.size();i++){
    //Set up the variables we will need
    ils=0.0;
    NumericVector rmr = as<NumericVector>(as<List>(obj["rmr"])[i]);
    NumericMatrix cs = as<NumericMatrix>(as<List>(obj["cs"])[i]);
    NumericVector ycwt = as<NumericVector>(as<List>(obj["ycwt"])[i]);
    for(j=0;j<p;j++){
      igr[j]=0.0;
      for(k=0;k<p;k++){
        ihess(j,k)=0.0;
      }
    }
    //Find the inverse log sum
    for(j=0;j<rmr.size();j++){
      pd=0.0;
      for(k=0;k<p;k++)
        pd+=cs(j,k)*coef[k];
      if(j==0)
        ils=rmr[j]+pd+log(ycwt[j]);
      else
        ils=R::logspace_add(ils,rmr[j]+pd+log(ycwt[j]));
    }
    //Have to make a second pass to get derivative elements (this is slower than one
    //pass, but we need to precompute ils to avoid overflow issues)
    for(j=0;j<rmr.size();j++){
      pd=0.0;
      for(k=0;k<p;k++)
        pd+=cs(j,k)*coef[k];
      for(k=0;k<p;k++){
        igr[k]-=cs(j,k)*exp(rmr[j]+pd-ils)*ycwt[j];   //Local gradient contribution
        for(l=0;l<p;l++){                             //Local Hessian contribution
          ihess(k,l)+=cs(j,k)*cs(j,l)*exp(rmr[j]+pd-ils)*ycwt[j];
        }
      }
    }
    //Add to the total (multiplying by inverse inclusion weight)
    lpl-=iw[i]*ils;                                                     //LPL
    for(j=0;j<p;j++){
      gr[j]-=iw[i]*igr[j];                                              //Gradient NLPL
      for(k=0;k<p;k++){
        hess(j,k)-=iw[i]*(igr[j]*igr[k]-ihess(j,k));                    //Hessian NLPL
      }
    }
  }

  //Compute the regularization penalty, if any
  if(rtype==1){                              //L1 regularization
    for(i=0;i<p;i++){
      reg+=fabs(coef[i]);
      gr[i]+=(coef[i] > 0.0 ? 1.0 : (coef[i] < 0.0 ? -1.0 : 0.0))*rparam[0];
    }
    reg*=rparam[0];
  }else if(rtype==2){                        //L2 regularization
    for(i=0;i<p;i++){
      reg+=coef[i]*coef[i];
      gr[i]+=2.0*coef[i]*rparam[0];
      hess(i,i)+=2.0*rparam[0];
    }
    reg*=rparam[0];
  }else if(rtype==3){                        //pseudo-Huber regularization
    for(i=0;i<p;i++){
      reg+=rparam[1]*(sqrt(1.0+coef[i]*coef[i]/(rparam[1]*rparam[1]))-1.0);
      gr[i]+=rparam[0]*coef[i]/(rparam[1]*sqrt(1.0+coef[i]*coef[i]/(rparam[1]*rparam[1])));
      hess(i,i)+=rparam[0]*rparam[1]/((rparam[1]*rparam[1]+coef[i]*coef[i])*sqrt(1.0+coef[i]*coef[i]/(rparam[i]*rparam[i])));
    }
    reg*=rparam[0];
  }

  //Return the penalized negative log pseudo-likelihood, gradient, and Hessian
  List out = List::create(Named("value") = -lpl+reg, Named("gradient") = gr, Named("hessian") = hess);
  return out;
}')


#Precompute quantities needed for calculating the count ERGM pseudo-likelihood;
#this is very consumptive of memory, but saves a lot of time.
ergmCntPrep<-function(formula,nw,response,reference=c("uniform", "binomial", "geometric", "poisson"), max.count=Inf, max.count.safety=4, max.count.edgewise=TRUE, count.samples=Inf, sample.size=Inf, sample.method=c("weighted", "random"), weight.type=c("TNT", "flatval"), cores=1, seed=NULL, must.count=1,WtSumAsSampSiz=T){
  #Get the network, and set things up
  nw<-ergm.getnetwork(formula)
  isdir<-is.directed(nw)  
  n<-network.size(nw)
  nev<-network.dyadcount(nw)              #Number of edge variables
  ss<-min(sample.size,nev)                #Sample size
  lrmr<-switch(match.arg(reference),      #Function to calculate log ref mes rat
    uniform = function(k,y) {rep(0,length(k))},
    binomial = function(k,y) {lchoose(max.count,k)-lchoose(max.count,y)},
    geometric = function(k,y) {rep(0,length(k))},
    poisson = function(k,y) {lfactorial(y)-lfactorial(k)}
  )
  #Set up edge variable sampling - use inverse freq weighting to smooth a bit
  ally<-as.sociomatrix(nw,response)       #All y values
  alli<-row(ally)                         #Senders
  allj<-col(ally)                         #Receivers
  ally<-gvectorize(ally,mode=ifelse(isdir,"digraph","graph"),censor.as.na=FALSE)
  alli<-gvectorize(alli,mode=ifelse(isdir,"digraph","graph"),censor.as.na=FALSE)
  allj<-gvectorize(allj,mode=ifelse(isdir,"digraph","graph"),censor.as.na=FALSE)
  if(ss==nev){                            #If ss==nev, no sampling
    samp<-1:nev                              #Everyone's in the sample
    iw<-rep(1,nev)                           #Inclusion weight is 1
  }else{                                  #Else, sample EVs
    if(match.arg(sample.method)=="random"){
      set.seed(seed)                         #Set seed for sampling
      samp<-sample(1:nev,ss)                 #Choose at random
      iw <- rep(1,ss)                        #Inclusion weight, before regularization
    }else{
      if(match.arg(weight.type)=="TNT"){          #"Tie/No-Tie" style weighting
        taby<-table(ally>0)  #c(FALSE, TRUE)
        wght<-(1/(2*taby))[1+(ally>0)]
        wght<-inclusionprobabilities(wght,ss)
        set.seed(seed)                             #Set seed for sampling
        samp<-which(UPpoisson(wght)>0)             #Draw the sample
        iw<- 1/(wght[samp])                        #Inclusion weight, before regularization
      }else if(match.arg(weight.type)=="flatval"){ #"Flat" value distribution weighting
        if(ss/nev>0.15)
          warning("Target sample size is ",round(ss/nev*100),"% of the total EV count.  Weighted sampling may be unreliable here - you may want to consider random sampling.")
        taby<-table(ally)
        wght<-(1/length(taby)/taby)[match(ally,names(taby))] #Ideal weights
        wght<-inclusionprobabilities(wght,ss)
        set.seed(seed)                           #Set seed for sampling
        samp<-which(UPpoisson(wght)>0)           #Draw the sample
        iw<- 1/(wght[samp])                        #Inclusion weight, before regularization
      }else{
        stop("Unknown weighting method ",weight.type," in ergmCntPrep.  Cannot go on like this.\n")
      }
    }
  }
  ss<-length(samp)                        #Should not have changed, but can...
  #Inclusion weight: regularize it to sample size or total dyads.
  if(WtSumAsSampSiz){
    iw <- iw*ss/sum(iw)
  }else{
    iw <- iw*nev/sum(iw)
  }
  #There may be some ignoreable numerical difference
  if(WtSumAsSampSiz & sum(iw)!=ss) warning("WtSumAsSampSiz=T, but sum of inclusion weight unequal to sample size, diff=",sum(iw)-ss)
  if(WtSumAsSampSiz==F & sum(iw)!=nev) warning("WtSumAsSampSiz=F, but sum of inclusion weight unequal to N of edge variables, diff=",sum(iw)-ss)
  
  y<-ally[samp]                           #Observed sampled y values
  snd<-alli[samp]                         #Sampled senders
  rec<-allj[samp]                         #Sampled receivers
  #Walk through edge variables and calculate exciting things
  if(max.count.edgewise){        #Use per-edge values
    yub<-pmin(pmax(must.count+1,ceiling(y+max.count.safety*4*sqrt(y))),max.count)  #Upper bounds
    ylb<-pmax(must.count+1,floor(y-max.count.safety*4*sqrt(y)))    #Lower bounds (0:must.count always included)
   yrng<-vector(mode="list",length=ss)        #y values for each edge
   ycwt<-vector(mode="list",length=ss)        #Weights for approximation
   for(i in 1:ss){
     if(count.samples>=yub[i]-ylb[i]){
       yrng[[i]]<-c(0:must.count,ylb[i]:yub[i])
       ycwt[[i]]<-rep(1,length(yrng[[i]]))
     }else{
       yrng[[i]]<-c(0:must.count,round(seq(from=ylb[i],to=yub[i],length=count.samples)))
       ycwt[[i]]<-c(rep(1, must.count+1),1,diff(round(seq(from=ylb[i],to=yub[i],length=count.samples))))
     }
   } 
  }else{                         #Use uniform edge value ranges
    maxy<-max(ally)                         #Maximum y value
    yrng<-vector(mode="list",length=ss)     #y values for each edge
    ycwt<-vector(mode="list",length=ss)     #Weights for approximation
    if(max.count<Inf){                      #Range of y values for normalization
      if(count.samples>=max.count+1){
        for(i in 1:ss){
          yrng[[i]]<-0:max.count
          ycwt[[i]]<-rep(1,1+max.count)
        }
      }else{
        for(i in 1:ss){
          yrng[[i]]<-c(0,round(seq(from=1,to=max.count,length=count.samples)))
          ycwt[[i]]<-c(1,1,diff(yrng[[i]]))
        }
      }
    }else{
      if(count.samples>=max.count.safety*maxy+1){
        for(i in 1:ss){
          yrng[[i]]<-0:(max.count.safety*maxy)
          ycwt[[i]]<-rep(1,1+max.count.safety*maxy)
        }
      }else{
        for(i in 1:ss){
          yrng[[i]]<-round(seq(from=0,to=ceiling(max.count.safety*maxy),length=count.samples))
          ycwt[[i]]<-c(diff(yrng[[i]]),1)
        }
      }
    }
  }
  rmr<-mclapply(1:ss,function(i){         #Ref meas ratio (one entry per EV)
    lrmr(yrng[[i]],y[i])
  },mc.cores=cores)
  cs<-mclapply(1:ss,function(i){          #Changescore list (one entry per EV)
    ergm.godfather(formula=formula, changes=lapply(yrng[[i]],function(z){matrix(c(snd[i],rec[i],z),nrow=1)}), response=response, changes.only=TRUE)
  },mc.cores=cores)
  #Return a list with all the goodies
  out<-list(snd=snd,rec=rec,y=y,rmr=rmr,cs=cs,ycwt=ycwt,iw=iw)
  class(out)<-"ERGMCntPrep"
  out
}

#"Real time" version - calculates probabilities on the fly.  This saves RAM,
#but is really, really slow.
ergmCntNLPLRT<-function(coef,formula,response,reference=c("uniform", "binomial", "geometric", "poisson"), max.count=Inf, log.sum.tol=1e-5){
  nw<-ergm.getnetwork(formula)
  isdir<-is.directed(nw)  
  n<-network.size(nw)
  lrmr<-switch(match.arg(reference),
    uniform = function(k,y) {rep(0,length(k))},
    binomial = function(k,y) {lchoose(max.count,k)-lchoose(max.count,y)},
    geometric = function(k,y) {rep(0,length(k))},
    poisson = function(k,y) {lfactorial(y)-lfactorial(k)}
  )
  lpsum<-function(vals,i,j,obs,form){
    #Get the log reference measure ratios
    lr<-lrmr(vals,obs)
    #Get the potential differences
    pd<-ergm.godfather(formula=form, changes=lapply(vals,function(z){matrix(c(i,j,z),nrow=1)}), response=response, changes.only=TRUE)
    #cat("lpsum",i,j,obs,"\n")
    #print(pd)
    pd<-as.vector(pd%*%coef)
    #print(pd)
    #Add everything up and return
    #cat("\tlocsum:",log_sum_exp(lr+pd),"binom:",-dbinom(y,max.count,1/(1+exp(-coef)),TRUE),"\n")
    log_sum_exp(lr+pd)
  }
  lpl<-0
  for(i in 1:n){  #Walk through the edge variables
    if(isdir)
      verts<-(1:n)[-i]
    else
      verts<-(i:n)[-1]
    for(j in verts){
      #Get the observed value
      if(!is.adjacent(nw,i,j))
        y<-0
      else
        y<-get.edge.attribute(get.edges(nw,i,alter=j),response)
      #Estimate the log sum
      if(max.count<Inf){  #If finite, then compute directly
        rng<-0:max.count
        lsum<-lpsum(rng,i,j,y,formula)
      }else{              #If not finite, estimate from partial sums
        osum<-lpsum(0:y,i,j,y,formula)
        ub<-max(2*y,2)
        lsum<-log_sum_exp(c(osum,lpsum((y+1):ub,i,j,y,formula)))
        #cat("",lsum)
        iter<-1
        while((abs(lsum-osum)>log.sum.tol)&&(iter<20)){
          osum<-lsum
          lsum<-log_sum_exp(c(osum,lpsum((ub+1):(2*ub),i,j,y,formula)))
          #cat("",lsum)
          ub<-2*ub
          iter<-iter+1
        }
        #cat("\n")
      }
      #Add to the lpl (pl=pl*1/sum)
      lpl<-lpl-lsum
    }
  }
  #Return the negative log pseudo-likelihood
  cat("par:",coef,"lpl:",lpl,"\n")
  -lpl
}

ergmCntMPLERT<-function(formula, response, reference=c("uniform", "binomial", "geometric", "poisson"), max.count=Inf, log.sum.tol=1e-5){
    np<-nparam(ergm_model(formula=formula,response=response))
    fit<-optim(rnorm(np,sd=1e-3),fn=ergmCntNLPLRT, formula=formula, response=response, reference=reference, max.count=max.count, log.sum.tol=log.sum.tol, method="BFGS")
    fit
}
