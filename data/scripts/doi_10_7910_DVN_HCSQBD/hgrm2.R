# convert original response data to integers
fac2int <- function(x) as.integer(factor(x, exclude = c(NA, NaN)))

# logical or infix function
`%||%` <- function(a, b) if (!is.null(a)) a else b

# calculate gamma gradient for case i
si_gamma <- function(i) {
  sum(pik[i, ] * Lik[i, ] * (theta_ls - fitted_mean[i]))/
    fitted_var[i]/Li[i] * x[i, 2:p]
}

# calculate lambda gradient for case i
si_lambda <- function(i) {
  # sum(0.5 * pik[i, ] * Lik[i, ] * ((theta_ls - fitted_mean[i])^2/fitted_var[i] - 1))/Li[i] * z[i, 2:q]
  sum(0.5 * pik[i, ] * Lik[i, ] * ((theta_ls - fitted_mean[i])^2/fitted_var[i] - 1))/Li[i] * z[i, 1:q]
}

# check if a vector has at least two valid responses
invalid_grm <- function(x) max(x, na.rm = TRUE) < 2

# log likelihood function (return N * J matrix) y: N*J data frame alpha:
# length J list beta: length J numeric vector theta: length N numeric
# vector
loglik_grm <- function(alpha, beta, theta) {
  util <- outer(theta, beta)
  alpha_l <- simplify2array(unname(Map(function(x, y) x[y], alpha, y)))
  alpha_h <- simplify2array(unname(Map(function(x, y) x[y + 1L], alpha,
                                       y)))
  log(plogis(util + alpha_l) - plogis(util + alpha_h))
}

# posterior of theta (unnormalized) (returns N-vector) y: N*J data frame
# x: N*p model matrix z: N*q model matrix alpha: length J list beta:
# length J numeric vector gamma: p-vector lambda: q-vector theta_k:
# numeric scalar qw_k numeric scalar
theta_post_grm <- function(theta_k, qw_k) {
  wt_k <- dnorm(theta_k - fitted_mean, sd = sqrt(fitted_var)) * qw_k  # prior density * quadrature weight
  loglik <- rowSums(loglik_grm(alpha, beta, rep(theta_k, N)), na.rm = TRUE)
  logPop <- log(wt_k)
  exp(loglik + logPop)
}

# pseudo tabulated data for item J (returns K*H_j matrix) y_j: N-vector
# H_j: number of response categories for item j w: K*N matrix
dummy_fun_grm <- function(y_j, H_j) {
  dummy_mat <- outer(y_j, 1:H_j, "==")  # N*H_j matrix
  dummy_mat[is.na(dummy_mat)] <- 0
  w %*% dummy_mat
}

# pseudo tabulated data to pseudo data frame tab: K*H_j matrix theta_ls:
# K-vector
tab2df_grm <- function(tab, theta_ls) {
  H_j <- ncol(tab)
  theta <- rep(theta_ls, H_j)
  y <- rep(1:H_j, each = K)
  data.frame(y = factor(y), x = theta, wt = as.vector(tab))
}

# score function of alpha and beta (return a H_j*N matrix) Lik: N*K
# matrix pik: N*K matrix alpha: J-list beta: J-vector theta_ls: K-vector
sj_ab_grm <- function(j) {
  temp2 <- array(0, c(N, K, H[j] + 1))
  h <- .subset2(y, j)
  drv_h <- vapply(theta_ls, function(theta_k) exp(alpha[[j]][h] + beta[j] *
                                                    theta_k)/(1 + exp(alpha[[j]][h] + beta[j] * theta_k))^2, numeric(N))
  drv_h_plus_one <- -vapply(theta_ls, function(theta_k) exp(alpha[[j]][h +
                                                                         1L] + beta[j] * theta_k)/(1 + exp(alpha[[j]][h + 1L] + beta[j] *
                                                                                                             theta_k))^2, numeric(N))
  drv_h[h == 1, ] <- 0
  drv_h_plus_one[h == H[j], ] <- 0
  for (i in seq_len(N)) {
    if (is.na(h[i])) next
    temp2[i, , h[i]] <- drv_h[i, ]
    temp2[i, , h[i] + 1L] <- drv_h_plus_one[i, ]
  }
  comp_a <- pik * Lik/vapply(Lijk, `[`, 1:N, j, FUN.VALUE = numeric(N))  # N*K matrix
  s_alpha <- vapply(1:N, function(i) comp_a[i, ] %*% temp2[i, , 2:H[j]],
                    numeric(H[j] - 1L))  # (H[j]-1)*N matrix
  temp2_beta <- drv_h + drv_h_plus_one
  s_beta <- rowSums(comp_a * matrix(theta_ls, N, K, byrow = TRUE) * temp2_beta)  # N-vector
  s <- sweep(rbind(s_alpha, s_beta), 2, rowSums(Lik * pik), FUN = "/")
}


hgrm2 <- function(y, x = matrix(1, nrow(y), 1), z = matrix(1, nrow(y), 1),
                 beta_set = 1, sign_set = TRUE, control = list()){
  
  # match call
  cl <- match.call()
  
  # check y
  if ((!is.data.frame(y) & !is.matrix(y)) || ncol(y) == 1L)
    stop("'y' must be either a data.frame or a matrix with at least two columns.")
  
  # check missing columns
  y <- as.data.frame(y)
  N <- nrow(y)
  J <- ncol(y)
  for (j in seq(1, J)) y[[j]] <- fac2int(y[[j]])
  tmp <- match(TRUE, vapply(y, invalid_grm, logical(1L)))
  if (!is.na(tmp))
    stop(paste(names(y)[tmp], "does not have at least two valid responses"))
  # if(max(y, na.rm = TRUE)==2)
  #   stop("All items are dichotomous. Use 'hltm' ")
  H <- vapply(y, max, numeric(1), na.rm = TRUE)
  
  # check x and z (x and z should contain an intercept column)
  if (is.null(nrow(x)))
    x <- as.matrix(x)
  if (is.null(nrow(x)))
    z <- as.matrix(z)
  if (nrow(x) != N || nrow(z) != N)
    stop("both 'x' and 'z' must have the same number of rows as 'y'")
  p <- ncol(x)
  q <- ncol(z)
  x <- `colnames<-`(model.matrix(~ 0 + x), colnames(x) %||% paste("x", 1:p, sep = ""))
  z <- `colnames<-`(model.matrix(~ 0 + z), colnames(z) %||% paste("z", 1:q, sep = ""))
  
  # check beta_set and sign_set
  stopifnot(beta_set %in% 1:J, is.logical(sign_set))
  
  # control parameters
  con <- list(max_iter = 150, max_iter2 = 15, eps = 1e-04, eps2 = 0.001,
              K = 21, C = 5)  # control parameters
  con[names(control)] <- control
  
  # set environments for utility functions
  environment(loglik_grm) <- environment(theta_post_grm) <- environment(dummy_fun_grm) <- environment(tab2df_grm) <- environment()
  
  # GL points
  K <- con[["K"]]
  theta_ls <- con[["C"]] * GLpoints[[K]][["x"]]
  qw_ls <- con[["C"]] * GLpoints[[K]][["w"]]
  
  # initialization
  lm_opr <- tcrossprod(solve(crossprod(x)), x)
  theta_eap <- {
    tmp <- rowMeans(y, na.rm = TRUE)
    (tmp - mean(tmp, na.rm = TRUE))/sd(tmp, na.rm = TRUE)
  }
  theta_eap[is.na(theta_eap)] <- 0
  alpha <- lapply(H, function(x) c(Inf, seq(1, -1, length.out = x - 1),
                                   -Inf))
  beta <- vapply(y, function(y) cov(y, theta_eap, use = "complete.obs")/var(theta_eap),
                 numeric(1L))
  gamma <- lm_opr %*% theta_eap
  lambda <- rep(0, q)
  fitted_mean <- as.vector(x %*% gamma)
  fitted_var <- rep(1, N)
  
  # EM algorithm
  for (iter in seq(1, con[["max_iter"]])) {
    
    # store previous parameters
    alpha_prev <- alpha
    beta_prev <- beta
    gamma_prev <- gamma
    lambda_prev <- lambda
    
    # construct w_ik
    posterior <- Map(theta_post_grm, theta_ls, qw_ls)
    w <- {
      tmp <- matrix(unlist(posterior), N, K)
      t(sweep(tmp, 1, rowSums(tmp), FUN = "/"))
    }
    
    # maximization
    pseudo_tab <- Map(dummy_fun_grm, y, H)
    pseudo_y <- lapply(pseudo_tab, tab2df_grm, theta_ls = theta_ls)
    pseudo_lrm <- lapply(pseudo_y, function(df) lrm.fit(df[["x"]],
                                                        df[["y"]], weights = df[["wt"]])[["coefficients"]])
    beta <- vapply(pseudo_lrm, function(x) x[length(x)], numeric(1L))
    alpha <- lapply(pseudo_lrm, function(x) c(Inf, x[-length(x)], -Inf))
    theta_eap <- t(theta_ls %*% w)
    theta_vap <- t(theta_ls^2 %*% w) - theta_eap^2
    
    # variance regression
    gamma <- lm_opr %*% theta_eap
    r2 <- (theta_eap - x %*% gamma)^2 + theta_vap
    if (ncol(z)==1) lambda <- log(mean(r2)) else{
      s2 <- glm.fit(x = z, y = r2, intercept = FALSE, family = Gamma(link = "log"))[["fitted.values"]]
      loglik <- -0.5 * (log(s2) + r2/s2)
      LL0 <- sum(loglik)
      dLL <- 1
      for (m in seq(1, con[["max_iter2"]])) {
        gamma <- lm.wfit(x, theta_eap, w = 1/s2)[["coefficients"]]
        r2 <- (theta_eap - x %*% gamma)^2 + theta_vap
        var_reg <- glm.fit(x = z, y = r2, intercept = FALSE, family = Gamma(link = "log"))
        s2 <- var_reg[["fitted.values"]]
        loglik <- -0.5 * (log(s2) + r2/s2)
        LL_temp <- sum(loglik)
        dLL <- LL_temp - LL0
        if (dLL < con[["eps2"]])
          break
        LL0 <- LL_temp
      }
      lambda <- var_reg[["coefficients"]]
    }
    
    # location constraint
    tmp <- mean(x %*% gamma)
    alpha <- Map(function(x, y) x + tmp * y, alpha, beta)
    gamma[1L] <- gamma[1L] - tmp
    
    # scale constraint
    tmp <- mean(z %*% lambda)
    gamma <- gamma/exp(tmp/2)
    beta <- beta * exp(tmp/2)
    lambda[1L] <- lambda[1L] - tmp
    
    # direction contraint
    if (sign_set == (beta[beta_set] < 0)) {
      gamma <- -gamma
      beta <- -beta
    }
    fitted_mean <- as.vector(x %*% gamma)
    fitted_var <- exp(as.vector(z %*% lambda))
    cat(".")
    
    # check convergence
    if (sqrt(sum((beta - beta_prev)^2)) < con[["eps"]]) {
      cat("\n converged at iteration", iter, "\n")
      gamma <- setNames(gamma, paste("x", colnames(x), sep = "_"))
      lambda <- setNames(lambda, paste("z", colnames(z), sep = "_"))
      break
    } else if (iter == con[["max_iter"]]) {
      stop("algorithm did not converge; try increasing max_iter.")
      break
    } else next
  }
  
  
  # scale constraint (make the product of beta equal one)
  tmp <- exp(mean(log(abs(beta))))
  gamma <- gamma * tmp
  beta <- beta / tmp
  lambda[1L] <- lambda[1L] + 2 * log(tmp)

  
  # inference
  pik <- matrix(unlist(Map(partial(dnorm, x = theta_ls), mean = fitted_mean,
                           sd = sqrt(fitted_var))), N, K, byrow = TRUE) * matrix(qw_ls, N, K, byrow = TRUE)
  Lijk <- lapply(theta_ls, function(theta_k) exp(loglik_grm(alpha = alpha,
                                                            beta = beta, rep(theta_k, N))))  # K-list
  Lik <- vapply(Lijk, compose(exp, partial(rowSums, na.rm = TRUE),
                              log), numeric(N))
  Li <- rowSums(Lik * pik)
  
  # log likelihood
  log_Lik <- sum(log(Li))
  
  
  # outer product of gradients
  environment(sj_ab_grm) <- environment(si_gamma) <- environment(si_lambda) <- environment()
  s_ab <- unname(Reduce(rbind, lapply(1:J, sj_ab_grm)))
  s_lambda <- s_gamma <- NULL
  if (p > 1)
    s_gamma <- vapply(1:N, si_gamma, numeric(p - 1))
  # if (q > 1)
  #   s_lambda <- vapply(1:N, si_lambda, numeric(q - 1))
  # s_all <- rbind(s_ab, s_gamma, s_lambda)
  # s_all[is.na(s_all)] <- 0
  # covmat <- solve(tcrossprod(s_all))
  # se_all <- sqrt(diag(covmat))
  
  sH <- sum(H)
  s_lambda <- vapply(1:N, si_lambda, numeric(q))
  s_all <- rbind(s_ab[-sum(H), ], s_gamma, s_lambda)
  s_all[is.na(s_all)] <- 0
  covmat <- solve(tcrossprod(s_all))
  se_all <- sqrt(diag(covmat))
  
  # reorganize se_all
  sH <- sum(H)
  lambda_indices <- gamma_indices <- NULL
  if (p > 1)
    gamma_indices <- (sH + 1):(sH + p - 1)
  # if (q > 1)
  #   lambda_indices <- (sH + p):(sH + p + q - 2)
  # se_all <- c(se_all[1:sH], NA, se_all[gamma_indices], NA, se_all[lambda_indices])
  lambda_indices <- (sH + p):(sH + p + q - 1)
  se_all <- c(se_all[1:(sH-1)], NA, NA, se_all[gamma_indices], se_all[lambda_indices])
  
  # name se_all and covmat
  names_ab <- unlist(lapply(names(alpha), function(x) {
    tmp <- alpha[[x]]
    paste(x, c(names(tmp)[-c(1, length(tmp))], "Dscrmn"))
  }))
  names(se_all) <- c(names_ab, names(gamma), names(lambda))
  # rownames(covmat) <- colnames(covmat) <- c(names_ab, names(gamma)[-1L],
  #                                           names(lambda)[-1L])
  rownames(covmat) <- colnames(covmat) <- c(names_ab[-sH], names(gamma)[-1L],
                                            names(lambda))

  
  # item coefficients
  coef_item <- Map(function(a, b) c(a[-c(1L, length(a))], Dscrmn = b), alpha, beta)
  
  # all coefficients
  coef_all <- c(unlist(coef_item), gamma, lambda)
  coefs <- data.frame(Estimate = coef_all, Std_Error = se_all, z_value = coef_all/se_all,
                      p_value = 2 * (1 - pnorm(abs(coef_all/se_all))))
  rownames(coefs) <- names(se_all)
  
  # ability parameter estimates
  theta <- data.frame(est = theta_eap, se = sqrt(theta_vap))
  
  # output
  out <- list(coefficients = coefs, scores = theta, vcov = covmat, log_Lik = log_Lik,
              H = H, p = p, q = q, item_names = names(y), call = cl)
  class(out) <- c("hgrm", "hIRT")
  out
}
