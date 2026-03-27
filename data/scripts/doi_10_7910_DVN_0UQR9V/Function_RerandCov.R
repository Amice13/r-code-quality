library("Runuran")
##########################################################################
################ functions for design ####################################
##########################################################################

### generate CRE
assign_CRE <- function(n, m, nperm){
  #Z = comb(n, m, nperm)
  Z = matrix(0, nrow = nperm, ncol = n)
  for(i in 1:nperm){
    Z[i, sample(c(1:n), m)] = 1
  }
  return(Z)
}
### generate ReM
assign_ReM <- function(n, m, X, pa, nperm){
  Z_rem = matrix(nrow = nperm, ncol = n)
  a = qchisq(pa, df = ncol(X))
  V_X_inv = solve(cov(X))
  k = 0
  while(k < nperm){
    Z = assign_CRE(n, m, 10^4)
    tau_hat_X = Z%*%X/m - (1-Z)%*%X/(n-m)
    M = diag( (m*(n-m))/n * tau_hat_X%*%as.matrix(V_X_inv)%*%t(tau_hat_X) )
    accept_num = sum(M<=a)
    if(accept_num>0){
      if(k+accept_num<=nperm){
        Z_rem[(k+1):(k+accept_num), ] = Z[M<=a, ]
      }else{
        Z_rem[(k+1):nperm, ] = Z[M<=a,][1:(nperm-k),]
      }
    }
    k = k+accept_num
    print(k)
  }
  return(Z_rem)
}

##########################################################################
################ functions for analysis ##################################
##########################################################################

### difference-in-means estimator
diff_est <- function(Z, Y){
  tau_hat = mean(Y[Z==1]) - mean(Y[Z==0])
  return(tau_hat)
}

### regression adjustment estimator
# W here should be a matrix
reg_adj_est <- function(Z, Y, W){
  lm1 = lm(Y[Z==1] ~ W[Z==1, ])
  lm0 = lm(Y[Z==0] ~ W[Z==0, ])
  tau_adj = mean(lm1$residuals) - mean(lm0$residuals) + sum( (lm1$coefficients[-1] - lm0$coefficients[-1])*colMeans(W) ) + lm1$coefficients[1] - lm0$coefficients[1]
  return(as.numeric(tau_adj))
}

### variance estimator for difference-in-means
var_diff_est <- function(Z, Y, W){
  n = length(Z)
  m = sum(Z)
  var_est = var(Y[Z==1])*n/m + var(Y[Z==0])*n/(n-m) - (cov(Y[Z==1],W[Z==1,]) - cov(Y[Z==0],W[Z==0,])) %*% solve(var(W)) %*% t(cov(Y[Z==1],W[Z==1,]) - cov(Y[Z==0],W[Z==0,]))
  return(var_est)
}

### variance estimator for regression-adjusted
var_adj_est <- function(Z, Y, W){
  n = length(Z)
  m = sum(Z)
  lm1 = lm(Y[Z==1] ~ W[Z==1, ])
  lm0 = lm(Y[Z==0] ~ W[Z==0, ])
  var_est = var(lm1$residuals)*n/m + var(lm0$residuals)*n/(n-m) - (cov(lm1$residual,W[Z==1,]) - cov(lm0$residual,W[Z==0,])) %*% solve(var(W)) %*% t(cov(lm1$residual,W[Z==1,]) - cov(lm0$residual,W[Z==0,]))
  return(var_est)
}

##########################################################################
###### functions for sample std. dist. under ReM #########################
##########################################################################

### sample from standardized distribution under rerandomization
sample_std_rerand <- function(n, R2, K, pa){
  epsilon0 = rnorm(n)
  a = qchisq(pa, df=K)
  chi_aK = sqrt(urchisq(n, df=K, lb=0, ub=a))
  S = 2*rbinom(n, 1, prob=0.5) - 1
  if(K>=2){
    beta_K = rbeta(n, shape1=1/2, shape2=(K-1)/2)
  }else{
    beta_K = 1
  }
  draw = sqrt(1-R2) * epsilon0 + sqrt(R2)*chi_aK*S*sqrt(beta_K)
  return(draw)
}

##########################################################################
####### functions for calculating the R2 measure #########################
##########################################################################

### calculate R2 measure
cal_R2 <- function(Y1, Y0, X, r1, r0){
  X = as.matrix(X)
  Y1proj = lm(Y1~X)$fitted.values
  Y0proj = lm(Y0~X)$fitted.values
  R2 = (var(Y1proj)/r1 + var(Y0proj)/r0 - var(Y1proj - Y0proj))/(var(Y1)/r1 + var(Y0)/r0 - var(Y1 - Y0))
  return(R2)
}