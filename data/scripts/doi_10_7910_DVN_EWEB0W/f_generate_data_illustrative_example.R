## Functions for data generation for the MOTIVATION examples

# scenarios 1--4 are in the paper
# scenarios 5--8 are in the appendix


# Z = Protected attributes, some of them correlated (high or low) with X
z_function <- function(n,X_cont) {
  if(scenario==1 | scenario==6) {
    Z = as.matrix(rbinom(n, 1, 1/(1+exp(-6*X_cont[,2])))) # Z_1 = f(X_2) HIGH correlation
    Z = cbind(Z,rnorm(n,0,1),rnorm(n,0,1),rbinom(n, 1, 0.3))
  } else if (scenario==2 | scenario==3 | scenario==4) {
    Z = as.matrix(rbinom(n, 1, 1/(1+exp(-1*X_cont[,2])))) # Z_1 = f(X_2) LOW correlation
    Z = cbind(Z,rnorm(n,0,1),rnorm(n,0,1),rbinom(n, 1, 0.3))
  } else if (scenario==5 | scenario==8) {
    Z = as.matrix(rbinom(n, 1, 1/(1+exp(-1*X_cont[,2]))))
    Z = cbind(Z,rnorm(n,0,1),rnorm(n,0,1),rbinom(n, 1, 0.3))
    Z = cbind(Z,X_cont[,4]+rnorm(n,0,2)) # Z_5 = f(X_4) LOW correlation (continuous)
  } else if (scenario==7) {
    Z = as.matrix(rbinom(n, 1, 1/(1+exp(-1*X_cont[,2]))))
    Z = cbind(Z,rnorm(n,0,1),rnorm(n,0,1),rbinom(n, 1, 0.3))
    Z = cbind(Z,X_cont[,4]+rnorm(n,0,1)) # Z_5 = f(X_4) HIGH correlation (continuous)
  } 
  return(Z)
}


# TAU, which depends on X and on Z, then is standardized to be (0,1) distributed, just for easy comparison across scenarios
tau_function <- function(x,z) {
  aux_baseline <- (pmax(x[,1], 0) - 0.5*x[,3])
  if (scenario==1 | scenario==2) {
    aux_bias = z[,1]
  } else if (scenario==3 | scenario==6) {
    aux_bias = z[,1] + x[,2]
  } else if (scenario==4) {
    aux_bias = z[,1] - 5*x[,2]
  } else if (scenario==5 | scenario==7) {
    aux_bias = z[,5] + x[,4]
  } else if (scenario==8) {
    aux_bias = z[,5] - x[,4]
  }
  aux_bias = aux_bias/sd(aux_bias)
  aux = aux_baseline + aux_bias
  temp = (aux - mean(aux))/sd(aux)
  return(temp)
}


# Full DATA, including {X,Z,W,Y}
generate_data = function(n1, n2,
                           p_continuous, p_discrete,
                           sigma_y,seed){
  set.seed(seed)
  n = n1 + n2
  
  X_cont = matrix(rnorm(n*p_continuous), n, p_continuous)
  X_disc = matrix(rbinom(n*p_discrete, 1, 0.3),n,p_discrete)
  X = cbind(X_cont,X_disc)
  p = p_continuous + p_discrete
  
  # Protected vars
  Z = z_function(n,X_cont)
  
  # Random assignment
  W = rbinom(n, 1, 0.5)
  
  # Tau = Treatment effect
  tau = tau_function(X,Z)
  
  # Y = Outcome (continuous)
  set.seed(seed)
  noise_y = sigma_y*runif(n)
  Y =  tau*W + noise_y
  
  train_data = data.table(Y=Y[c(1:n1)],
                          Z=Z[c(1:n1),],
                          W=W[c(1:n1)],
                          tau = tau[c(1:n1)],
                          X=X[c(1:n1),])
  
  test_data = data.table(Y=Y[c((n1+1):(n1+n2))],
                         Z=Z[c((n1+1):(n1+n2)), ],
                         W=W[c((n1+1):(n1+n2))],
                         tau = tau[c((n1+1):(n1+n2))],
                         X=X[c((n1+1):(n1+n2)),])
  
  return(list(train_data=train_data,
              test_data=test_data))
}
  

