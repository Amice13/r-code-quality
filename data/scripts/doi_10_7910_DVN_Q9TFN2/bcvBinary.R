# BOOTSTRAP AFTER CROSS-VALIDATION (BCV)

# Dependencies
library("glmnet")
library("Matrix")
library("stats")

## ESTIMATION TOOLS

#' Cross-Validation with Out-of-Fold Residual Extraction
#'
#' Performs cross-validation for binary response models and extracts
#' out-of-fold residual estimates for further use, such as bootstrapping.
#'
#' @param x A matrix of predictors.
#' @param y A response vector. Must be binary (e.g., 0 and 1).
#' @param link A string specifying the link function to use. Default is
#'             \code{"logit"}. Other options are supported if compatible with
#'             \code{binomial()} family.
#' @param nfolds Number of folds for #' cross-validation. Default is 10.
#' @param foldid Optional vector of integers indicating fold assignments. If
#'               \code{NULL}, folds are assigned randomly by \code{glmnet}.
#' @param standardize Logical; should predictors be standardized? Default is
#'                    \code{TRUE}.
#' @param intercept Logical; should an intercept be included in the model?
#'                  Default is \code{TRUE}.
#' @param parallel Logical; should parallel computation be used for
#'                 cross-validation? Default is \code{FALSE}.
#'
#' @details This function uses \code{cv.glmnet} for cross-validation and allows
#'          the extraction of out-of-fold residuals. These residuals are derived
#'          based on the inverse link (CDF) and its derivative (PDF) from the
#'          specified link function. If \code{link = "logit"}, the function
#'          optimizes for logistic regression with \code{glmnet}.
#'
#' @return An object of class \code{"cv.glmnet"} from \code{glmnet}, augmented
#'         with: \item{res_oof}{Matrix of out-of-fold residuals, useful for
#'         downstream analysis.}
#'
#' @note \itemize{ \item The \code{cv.glmnet} function randomly assigns folds
#'       unless \code{foldid} is specified. \item The residual extraction
#'       process is designed for binary response models. }
#'
#' @examples
#' \dontrun{
#' # Example data
#' set.seed(123)
#' x <- matrix(rnorm(100 * 10), nrow = 100, ncol = 10)
#' y <- rbinom(100, 1, 0.5)
#'
#' # Perform cross-validation
#' cv_result <- cv_binary(x, y, link = "logit", nfolds = 5)
#'
#' # Access out-of-fold residuals
#' head(cv_result$res_oof)
#' }
#'
#' @import glmnet
#' @export
cv_binary <- function(x, y, link = "logit", nfolds = 10, foldid = NULL,
                      standardize = TRUE, intercept = TRUE, parallel = FALSE) {
  fam <- binomial(link = link) # family
  cd_func <- fam$linkinv # inverse link (CDF)
  pd_func <- fam$mu.eta # inv. link deriv. (PDF)
  # if logit, switch to hard-coded (faster) glmnet routine
  switch(link,
    "logit" = {
      famnet <- "binomial"
    },
    {
      famnet <- fam
    }
  )
  cv <- cv.glmnet(x, y, family = famnet, nfolds = nfolds, foldid = foldid,
                  parallel = parallel, keep = TRUE,
                  standardize = standardize, intercept = intercept)
  # Extract out-of-fold residuals (for bootstrapping)
  ind_min <- cv$index[1] # position lambda_cv
  lin_oof <- cv$fit.preval[, ind_min] # out-of-fold linear forms
  cdf_oof <- cd_func(lin_oof)
  pdf_oof <- pd_func(lin_oof)
  res_oof <- matrix(pdf_oof * (cdf_oof - y) / (cdf_oof * (1 - cdf_oof)))
  cv$res_oof <- res_oof # out-of-fold residuals
  return(cv)
}

#' Bootstrap Penalty Calculation
#'
#' Computes a penalty parameter \code{lambda} based on bootstrapped residuals
#' for use in high-dimensional estimation.
#'
#' @param x A matrix of predictors.
#' @param res A vector of residuals from a (fitted) model.
#' @param standardize Logical; should the predictors be standardized? Default
#'                    is \code{TRUE}.
#' @param nboot Integer; the number of bootstrap samples.
#' @param c0 A tuning parameter for scaling the penalty (the score markup).
#' @param alpha A numeric value (between 0 and 1) specifying the quantile used
#'              to determine the penalty (the probability tolerance).
#'
#' @details This function applies a bootstrap procedure to calculate a penalty
#'          parameter \code{lambda}. The penalty is scaled by \code{c0} and
#'          depends on the maximum absolute bootstrap score across predictor
#'          variables. If \code{standardize = TRUE}, the predictors are
#'          standardized using their sample standard deviations before
#'          computation.
#'
#' @return A numeric value representing the penalty parameter \code{lambda}.
#'
#' @examples
#' \dontrun{
#' # Example data
#' set.seed(123)
#' x <- matrix(rnorm(100 * 10), nrow = 100, ncol = 10)
#' res <- rnorm(100) # residuals from a model
#'
#' # Calculate bootstrap penalty
#' lambda <- boot_pen(x, res, standardize = TRUE, nboot = 1000, c0 = 1.1, alpha
#' = 0.05)
#' lambda
#' }
#'
#' @import stats
#' @export
boot_pen <- function(x, res, standardize, nboot, c0, alpha) {
  n <- nrow(x)
  p <- ncol(x)
  if (standardize == TRUE) {
    stdx <- matrix(apply(x, 2, sd), 1, p)
    x_til <- x / kronecker(matrix(1, n, 1), stdx)
  } else {
    x_til <- x
  }
  score_e <- matrix(NA, p, nboot)
  e <- matrix(rnorm(n * nboot), n, nboot)
  score_e <- (1 / n) * t(kronecker(matrix(1, 1, p), res) * x_til) %*% e
  q1minalpha <- quantile(apply(abs(score_e), 2, max), 1 - alpha)[[1]]
  lambda <- c0 * q1minalpha
  return(lambda)
}

#' Binary Response Model with Bootstrapping After Cross-Validation (BCV)
#'
#' Fits a binary response model using cross-validation (CV) and refines it by
#' applying a bootstrap penalty procedure to determine the penalty parameter.
#' Includes results from both CV and BCV.
#'
#' @param x A matrix of predictors.
#' @param y A response vector. Must be binary (e.g., 0 and 1) for logistic
#'          regression.
#' @param link A string specifying the link function to use. Default is
#'             \code{"logit"}. Other options are supported if compatible with
#'             \code{binomial()} family.
#' @param nfolds Number of folds for cross-validation. Default is 10.
#' @param foldid Optional vector of integers indicating fold assignments. If
#'               \code{NULL}, folds are assigned randomly by \code{glmnet}.
#' @param standardize Logical; should predictors be standardized? Default is
#'                    \code{TRUE}.
#' @param intercept Logical; should an intercept be included in the model?
#'                  Default is \code{TRUE}.
#' @param parallel Logical; should parallel computation be used for
#'                 cross-validation? Default is \code{FALSE}.
#' @param nboot Integer; the number of bootstrap samples. Default is 1000.
#' @param c0 A tuning parameter for scaling the penalty. Default is 1.1.
#' @param alpha A numeric value (between 0 and 1) specifying the quantile used
#'              in the bootstrap penalty calculation. Default is
#'              \code{0.1 / log(max(dim(x)))}.
#'
#' @details This function first performs cross-validation using \code{cv_binary}
#'          to obtain out-of-fold residuals. It then calculates a penalty
#'          parameter using a bootstrap procedure via \code{boot_pen}. Finally,
#'          it fits a model with the calculated bootstrap penalty and returns
#'          the estimated coefficients and penalty parameter.
#'
#' @return A list containing:
#' \item{lambda}{The penalty parameter determined by the bootstrap procedure.}
#' \item{intr}{The estimated intercept of the model.}
#' \item{that}{A matrix of estimated coefficients for the predictors.}
#' \item{cv}{The cross-validation results from \code{cv_binary}, including
#'            out-of-fold residuals.}
#'
#' @note This procedure is specifically designed for binary response models. The
#'       bootstrap penalty refines the model fit obtained from cross-validation.
#'
#' @examples
#' \dontrun{
#' # Example data
#' set.seed(123)
#' x <- matrix(rnorm(100 * 10), nrow = 100, ncol = 10)
#' y <- rbinom(100, 1, 0.5)
#'
#' # Fit binary response model using BCV
#' result <- bcv_binary(x, y, link = "logit", nfolds = 5, nboot = 500)
#'
#' # Access results
#' result$lambda  # Bootstrap penalty parameter
#' result$intr    # Intercept estimate
#' result$that    # Coefficient estimates
#' }
#'
#' @import glmnet
#' @export
bcv_binary <- function(x, y, link = "logit", nfolds = 10, foldid = NULL,
                       standardize = TRUE, intercept = TRUE, parallel = FALSE,
                       nboot = 1000, c0 = 1.1, alpha = .1 / log(max(dim(x)))) {
  cv <- cv_binary(x, y, link = link, nfolds = nfolds, foldid = foldid,
                  parallel = parallel,
                  standardize = standardize, intercept = intercept)
  lambda_bcv <- boot_pen(x, res = cv$res_oof, standardize = standardize,
                         nboot = nboot, c0 = c0, alpha = alpha)
  coef_bcv <- coef(cv$glmnet.fit, s = lambda_bcv, exact = FALSE)
  return(list(lambda = lambda_bcv, intr = coef_bcv[1],
              that = matrix(coef_bcv[-1], ncol(x), 1), cv = cv))
}

#' Refit Binary Response Model for Selected Variables
#'
#' Refits a binary response model using variables selected by a selector
#' vector \code{theta}. If no variables are selected, the original parameters
#' are retained.
#'
#' @param x A matrix of predictors.
#' @param y A response vector. Must be binary (e.g., 0 and 1) for logistic
#'          regression.
#' @param link A string specifying the link function to use. Common options
#'             include \code{"logit"} and \code{"probit"}.
#' @param intr Numeric; the initial intercept value.
#' @param theta Numeric; coefficient (slope) estimates to be refitted.
#' @param intercept Logical; should an intercept be included in the model?
#'                  Default is \code{TRUE}.
#'
#' @details This function refits a binary response model (logistic or similar)
#'          using only the variables selected by \code{theta}. If no variables
#'          are selected, the original intercept and coefficients are retained.
#'          For selected variables, the model is refit using \code{glm}. If
#'          \code{intercept = FALSE}, the model is refit without an intercept.
#'
#' @return A list containing:
#' \item{intr}{The refitted intercept (or original if no refitting occurred).}
#' \item{that}{A matrix of refitted coefficients for the predictors, with
#'              unselected variables set to zero.}
#' \item{converged}{Logical; indicates whether the refitting procedure
#'                  converged. If no refitting occurred, this is \code{TRUE}.}
#'
#' @examples
#' \dontrun{
#' # Example data
#' set.seed(123)
#' x <- matrix(rnorm(100 * 10), nrow = 100, ncol = 10)
#' y <- rbinom(100, 1, 0.5)
#' theta <- rbinom(10, 1, 0.3) # Random variable selection
#'
#' # Refit binary model
#' result <- refit_binary(x, y, link = "logit", intr = 0, theta = theta)
#'
#' # Access results
#' result$intr  # Refitted intercept
#' result$that  # Refitted coefficients
#' result$converged  # Convergence status
#' }
#'
#' @import stats
#' @export
refit_binary <- function(x, y, link, intr, theta, intercept = TRUE) {
  sel <- as.vector(theta != 0) # selection
  # if something was selected, refit; o/w keep as is.
  if (sum(sel) > 0) {
    intr_post <- NA # initialize
    that_post <- matrix(NA, ncol(x), 1) # initialize
    fam <- binomial(link = link)
    # formula (below) depends on whether intercept is included or not ("0 +")
    if (intercept == TRUE) {
      refit <- glm(formula = as.matrix(y) ~ as.matrix(x[, sel]), family = fam)
      converged <- refit$converged
      # overwrite upon convergence; o/w keep as NA
      if (converged == TRUE) {
        coefs <- unname(coef(refit))
        intr_post <- coefs[1] # refitted intercept
        that_post[sel] <- coefs[-1] # refitted slopes
        that_post[!sel] <- 0 # zeros remain zeros
      }
    } else {
      refit <- glm(formula = as.matrix(y) ~ 0 + as.matrix(x[, sel]),
                   family = fam)
      converged <- refit$converged
      # overwrite upon convergence; o/w keep as NA
      if (converged == TRUE) {
        intr_post <- 0 # "no intercept" stored as zero
        that_post[sel] <- unname(coef(refit)) # refitted slopes
        that_post[!sel] <- 0 # zeros remain zeros
      }
    }
  } else {
    intr_post <- intr # keep as is
    that_post <- theta # keep as is (i.e. zeros)
    converged <- TRUE # b/c nothing to do
  }
  return(list(intr = intr_post, that = that_post, converged = converged))
}

#' Fit Binary Response Model Using Refitting After BCV Selection
#'
#' Fits a binary response model using a bootstrap cross-validation (BCV)
#' procedure followed by a refitting step to refine the model coefficients.
#'
#' @param x A matrix of predictors.
#' @param y A response vector. Must be binary (e.g., 0 and 1) for logistic
#'          regression.
#' @param link A string specifying the link function to use. Default is
#'             \code{"logit"}. Other options are supported if compatible with
#'             \code{binomial()} family.
#' @param nfolds Number of folds for cross-validation. Default is 10.
#' @param foldid Optional vector of integers indicating fold assignments. If
#'               \code{NULL}, folds are assigned randomly by \code{glmnet}.
#' @param standardize Logical; should predictors be standardized? Default is
#'                    \code{TRUE}.
#' @param intercept Logical; should an intercept be included in the model?
#'                  Default is \code{TRUE}.
#' @param parallel Logical; should parallel computation be used for
#'                 cross-validation? Default is \code{FALSE}.
#' @param nboot Integer; the number of bootstrap samples. Default is 1000.
#' @param c0 A tuning parameter for scaling the penalty. Default is 1.1.
#' @param alpha A numeric value (between 0 and 1) specifying the quantile used
#'              in the bootstrap penalty calculation. Default is
#'              \code{0.1 / log(max(dim(x)))}.
#'
#' @details This function first applies the BCV procedure (via
#'          \code{bcv_binary}) to perform variable selection and estimate
#'          coefficients. It then refits the model with the selected variables
#'          using \code{refit_binary}.
#'
#' @return A list containing:
#' \item{intr}{The refitted intercept.}
#' \item{that}{A matrix of refitted coefficients for the predictors.}
#' \item{converged}{Logical; indicates whether the refitting procedure
#'                  converged.}
#'
#' @examples
#' \dontrun{
#' # Example data
#' set.seed(123)
#' x <- matrix(rnorm(100 * 10), nrow = 100, ncol = 10)
#' y <- rbinom(100, 1, 0.5)
#'
#' # Fit binary response model using post-BCV refitting
#' result <- post_bcv_binary(x, y, link = "logit", nfolds = 5, nboot = 500)
#'
#' # Access results
#' result$intr  # Refitted intercept
#' result$that  # Refitted coefficients
#' result$converged  # Convergence status
#' }
#'
#' @import glmnet
#' @import stats
#' @export
post_bcv_binary <- function(x, y, link = "logit", nfolds = 10, foldid = NULL,
                            standardize = TRUE, intercept = TRUE,
                            parallel = FALSE, nboot = 1000,
                            c0 = 1.1, alpha = .1 / log(max(dim(x)))) {
  # bootstrap after cross-validation...
  bcv <- bcv_binary(x, y, link = link, nfolds = nfolds, foldid = foldid,
                    standardize = standardize, intercept = intercept,
                    parallel = parallel, nboot = nboot, c0 = c0, alpha = alpha)
  # ... refit afterwards
  post_bcv <- refit_binary(x, y, link = link, intr = bcv$intr,
                           theta = bcv$that, intercept = intercept)
  return(post_bcv)
}

#' Fit Linear Model Using Bootstrapping After Cross-Validation (BCV)
#'
#' Fits a linear regression model using a bootstrap cross-validation (BCV)
#' procedure to perform variable selection and coefficient estimation.
#'
#' @param x A matrix of predictors.
#' @param y A response vector. Must be numeric for linear regression.
#' @param weights A vector of weights for the observations. Default is
#'                \code{rep(1, nrow(x))}, i.e. unit weights.
#' @param nfolds Number of folds for cross-validation. Default is 10.
#' @param foldid Optional vector of integers indicating fold assignments. If
#'               \code{NULL}, folds are assigned randomly by \code{glmnet}.
#' @param standardize Logical; should predictors be standardized? Default is
#'                    \code{TRUE}.
#' @param intercept Logical; should an intercept be included in the model?
#'                  Default is \code{TRUE}.
#' @param parallel Logical; should parallel computation be used for
#'                 cross-validation? Default is \code{FALSE}.
#' @param nboot Integer; the number of bootstrap samples. Default is 1000.
#' @param c0 A tuning parameter for scaling the penalty. Default is 1.1.
#' @param alpha A numeric value (between 0 and 1) specifying the quantile used
#'              in the bootstrap penalty calculation. Default is
#'              \code{0.1 / log(max(dim(x)))}.
#'
#' @details This function uses cross-validation to estimate the optimal
#'          penalization parameter (lambda). It then applies a bootstrap
#'          penalty calculation via \code{boot_pen} to refine the lambda
#'          selection and estimate coefficients. The procedure accounts for
#'          observation weights and standardization options.
#'
#' @return A list containing:
#' \item{lambda}{The BCV-selected penalization parameter.}
#' \item{intr}{The estimated intercept.}
#' \item{that}{A matrix of estimated coefficients for the predictors.}
#' \item{cv}{Cross-validation results returned by \code{cv.glmnet}.}
#'
#' @examples
#' \dontrun{
#' # Example data
#' set.seed(123)
#' x <- matrix(rnorm(100 * 10), nrow = 100, ncol = 10)
#' y <- rnorm(100)
#'
#' # Fit linear model using BCV
#' result <- bcv_linear(x, y, nfolds = 5, nboot = 500)
#'
#' # Access results
#' result$lambda  # BCV-selected lambda
#' result$intr    # Estimated intercept
#' result$that    # Estimated coefficients
#' }
#'
#' @import glmnet
#' @import stats
#' @export
bcv_linear <- function(x, y, weights = rep(1, nrow(x)),
                       nfolds = 10, foldid = NULL,
                       standardize = TRUE, intercept = TRUE, parallel = FALSE,
                       nboot = 1000, c0 = 1.1, alpha = .1 / log(max(dim(x)))) {
  cv <- cv.glmnet(x, y, family = "gaussian", weights = weights,
                  nfolds = nfolds, foldid = foldid, keep = TRUE,
                  parallel = parallel,
                  standardize = standardize, intercept = intercept)
  # Extract out-of-fold residuals (for bootstrapping)
  ind_min <- cv$index[1] # position lambda_cv
  n <- nrow(x)
  lin_oof <- matrix(cv$fit.preval[, ind_min], n, 1) # out-of-fold linear forms
  res_oof <- matrix(weights) * (lin_oof - y) # out-of-fold residuals
  lambda_bcv <- (n / sum(weights)) *
    boot_pen(x, res = res_oof, standardize = standardize,
             nboot = nboot, c0 = c0, alpha = alpha)
  # ^-- here rescaling weights to sum to n to match glmnet internal scaling
  coef_bcv <- coef(cv$glmnet.fit, s = lambda_bcv, exact = FALSE) # interpolate
  return(list(lambda = lambda_bcv, intr = coef_bcv[1],
              that = matrix(coef_bcv[-1], ncol(x), 1), cv = cv))
  # ^-- cv results obtained as part of bcv
}

# Fit Linear Model Using Refit for Generic Variable Selector
#'
#' Refit a linear regression model for a given coefficient vector (theta)
#' that indicates which predictors are included in the model.
#'
#' @param x A matrix of predictors.
#' @param y A response vector. Must be numeric for linear regression.
#' @param weights A vector of weights for the observations. Default is
#'                \code{rep(1, nrow(x))}, i.e. unit weights.
#' @param intr A numeric value specifying the initial intercept. If no intercept
#'             is used, this can be set to 0.
#' @param theta A numeric vector; the coefficient estimates for the predictors.
#' @param intercept Logical; should an intercept be included in the model?
#'                  Default is \code{TRUE}.
#'
#' @details This function refits a linear regression model for variables
#'          selected by \code{theta}. If no variables are selected (all elements
#'          of \code{theta} are zero), the function returns the initial
#'          intercept and theta without refitting. Otherwise, the model is
#'          refitted using either a formula with or without an intercept, based
#'          on the value of \code{intercept}.
#'
#' @return A list containing:
#' \item{intr}{The refitted intercept or the initial intercept if no refit was
#'              performed.}
#' \item{that}{A numeric vector of refitted coefficients. Zeros are retained
#'              for non-selected variables.}
#' \item{converged}{Logical; indicates whether the refitting process converged.}
#'
#' @examples
#' \dontrun{
#' # Example data
#' set.seed(123)
#' x <- matrix(rnorm(100 * 10), nrow = 100, ncol = 10)
#' y <- rnorm(100)
#' theta <- c(1, 0, 1, 0, 0, 0, 0, 0, 0, 0)
#'
#' # Refit linear model
#' result <- refit_linear(x, y, theta = theta, intercept = TRUE)
#'
#' # Access results
#' result$intr    # Refitted intercept
#' result$that    # Refitted coefficients
#' result$converged # Convergence status
#' }
#'
#' @import stats
#' @export
refit_linear <- function(x, y, weights = rep(1, nrow(x)),
                         intr, theta, intercept = TRUE) {
  sel <- as.vector(theta != 0) # selection
  # if something was selected, refit; o/w keep as is.
  if (sum(sel) > 0) {
    intr_post <- NA # initialize
    that_post <- matrix(NA, ncol(x), 1) # initialize
    # formula (below) depends on whether intercept is included or not ("0 +")
    if (intercept == TRUE) {
      refit <- glm(formula = as.matrix(y) ~ as.matrix(x[, sel]),
                   weights = weights)
      converged <- refit$converged
      # overwrite upon convergence; o/w keep as NA
      if (converged == TRUE) {
        coefs <- unname(coef(refit))
        intr_post <- coefs[1] # refitted intercept
        that_post[sel] <- coefs[-1] # refitted slopes
        that_post[!sel] <- 0 # zeros remain zeros
      }
    } else {
      refit <- glm(formula = as.matrix(y) ~ 0 + as.matrix(x[, sel]),
                   weights = weights)
      converged <- refit$converged
      if (converged == TRUE) {
        intr_post <- 0 # "no intercept" stored as zero
        that_post[sel] <- unname(coef(refit)) # refitted slopes
        that_post[!sel] <- 0 # zeros remain zeros
      }
    }
  } else {
    intr_post <- intr # keep as is
    that_post <- theta # keep as is (i.e. zeros)
    converged <- TRUE # b/c nothing to do
  }
  return(list(intr = intr_post, that = that_post, converged = converged))
}

#' Fit Linear Model Using Refitting After BCV Selection (Post-BCV)
#'
#' This function performs a linear regression using refitting after
#' bootstrap-after-cross-validation (BCV) selection. The procedure first
#' applies BCV to select a model and then refits the model using the selected
#' variables.
#'
#' @param x A numeric matrix or data frame of explanatory variables.
#' @param y A numeric vector of responses.
#' @param weights A numeric vector of weights for the observations. Default is
#'   a vector of 1s, i.e. unit weights.
#' @param nfolds Integer, the number of folds for cross-validation. Default is
#'   10.
#' @param foldid A vector of fold assignments for cross-validation. If NULL,
#'   the function will generate the folds automatically.
#' @param standardize Logical, whether to standardize the predictors. Default is
#'   TRUE.
#' @param intercept Logical, whether to include an intercept in the model.
#'   Default is TRUE.
#' @param parallel Logical, whether to parallelize the computation. Default is
#'   FALSE.
#' @param nboot Integer, the number of bootstrap iterations for BCV. Default is
#'   1000.
#' @param c0 Numeric, a tuning parameter used in the BCV method. Default is 1.1.
#' @param alpha Numeric, a regularization parameter used in the BCV method.
#'   Default is \code{0.1 / log(max(dim(x)))}.
#'
#' @return A list containing the refitted linear model, including estimated
#'   coefficients and other model diagnostics.
#'
#' @seealso \code{\link{bcv_linear}}, \code{\link{refit_linear}}
#'
#' @examples
#' # Example usage of the post_bcv_linear function
#' data(iris)
#' x <- as.matrix(iris[, -5])
#' y <- iris$Sepal.Length
#' result <- post_bcv_linear(x, y)
#'
#' @export
post_bcv_linear <- function(x, y, weights = rep(1, nrow(x)),
                            nfolds = 10, foldid = NULL,
                            standardize = TRUE, intercept = TRUE,
                            parallel = FALSE, nboot = 1000,
                            c0 = 1.1, alpha = .1 / log(max(dim(x)))) {
  # bootstrap after cross-validation...
  bcv <- bcv_linear(x, y, weights = weights, nfolds = nfolds, foldid = foldid,
                    standardize = standardize, intercept = intercept,
                    parallel = parallel, nboot = nboot,
                    c0 = c0, alpha = alpha)
  # ... refit afterwards
  post_bcv <- refit_linear(x, y, weights = weights, intr = bcv$intr,
                           theta = bcv$that, intercept = intercept)
  return(post_bcv)
}

## INFERENCE TOOLS

#' Debias Coefficient on Non-Constant Regressor in Binary Response Model
#'
#' This function debiases the coefficient on the first non-constant regressor in
#' a binary response model, using bootstrap resampling and/or refitting
#' after cross-validation. The debiasing is based on initial estimates for the
#' intercept and coefficient values.
#'
#' @param x A numeric matrix or data frame of explanatory variables.
#' @param y A numeric vector of binary responses (0 or 1).
#' @param link A string specifying the link function to use. Default is
#'             \code{"logit"}. Other options are supported if compatible with
#'             \code{binomial()} family.
#' @param intr The initial estimate of the intercept parameter.
#' @param theta A numeric vector of initial coefficient estimates for the
#'   regressors.
#' @param nfolds Integer, the number of folds for cross-validation. Default is
#'   10.
#' @param foldid A vector of fold assignments for cross-validation. If NULL,
#'   the function will generate the folds automatically.
#' @param standardize Logical, whether to standardize the predictors. Default is
#'   TRUE.
#' @param intercept Logical, whether to include an intercept in the model.
#'   Default is TRUE.
#' @param parallel Logical, whether to parallelize the computation. Default is
#'   FALSE.
#' @param boot Logical, whether to apply bootstrap resampling. Default is TRUE.
#' @param post Logical, whether to perform a post-variable selection refitting.
#'   Default is TRUE.
#'
#' @return A list containing:
#'   \item{bhat}{The debiased coefficient estimate.}
#'   \item{vhat}{The estimated variance of the debiased coefficient.}
#'   \item{converged}{A logical indicating whether the algorithm converged.}
#'   \item{fit2}{The fitted model object from BCV or CV.}
#'   \item{postfit2}{The post-variable selection refitted model, if applicable.}
#'
#' @seealso \code{\link{bcv_linear}}, \code{\link{refit_linear}}
#'
#' @examples
#' # Example usage of debias_binary function
#' data(iris)
#' x <- as.matrix(iris[, -5])
#' y <- ifelse(iris$Sepal.Length > 5, 1, 0)
#' result <- debias_binary(x, y, intr = 0, theta = rep(0, ncol(x)))
#'
#' @export
debias_binary <- function(x, y, link = "logit", intr, theta,
                          nfolds = 10, foldid = NULL,
                          standardize = TRUE, intercept = TRUE,
                          parallel = FALSE, boot = TRUE, post = TRUE) {
  # Step 1: Initial (biased) estimation
  # Captured by intr and that (intercept and slope estimates)
  # Step 2: Estimate orthogonalization coefficients
  fam <- binomial(link = link) # family
  cd_func <- fam$linkinv # inverse link (CDF)
  pd_func <- fam$mu.eta # inv. link deriv. (PDF)
  lin_init <- as.numeric(intr + x %*% theta)
  cdf_init <- cd_func(lin_init)
  cdf1mincdf_init <- cdf_init * (1 - cdf_init)
  pdf_init <- pd_func(lin_init)
  m1_init <- pdf_init * (cdf_init - y) / cdf1mincdf_init
  em11_init <- as.vector((pdf_init^2) / cdf1mincdf_init)
  # ^-- here we use the binomial structure to simplify E[m11|X]
  # and thus the E[m11_i|X_i] estimates
  d <- x[, 1] # treatment
  w <- x[, -1] # controls
  bcv2 <- bcv_linear(x = w, y = d, weights = em11_init, nfolds = nfolds,
                     foldid = foldid, standardize = standardize,
                     intercept = intercept, parallel = parallel)
  # Continue with BCV (or CV)
  if (boot == TRUE) {
    intr2 <- bcv2$intr
    mhat <- bcv2$that
    converged <- TRUE # b/o no refitting
    fit2 <- bcv2
  } else {
    coefcv <- coef(bcv2$cv, s = "lambda.min")
    intr2 <- coefcv[1]
    mhat <- matrix(coefcv[-1], ncol = 1)
    converged <- TRUE # b/o no refitting
    fit2 <- bcv2$cv
  }
  # Refit post variable selection
  if (post == TRUE) {
    postaux <- refit_linear(x = w, y = d, weights = em11_init,
                            intr = intr2, theta = mhat, intercept = intercept)
    intr2 <- postaux$intr
    mhat <- postaux$that
    converged <- postaux$converged # refitting converged?
    fit2 <- postaux
  }
  # Step 3: Debiasing
  if (converged == TRUE) {
    resaux <- d - intr2 - w %*% mhat # auxilliary residuals
    step <- mean(m1_init * resaux) / mean(em11_init * resaux * d)
    bhat <- theta[1] - step # one-step debiasing
    # Variance estimation
    lin_hat <- as.numeric(intr + x %*% cbind(c(bhat, theta[-1])))
    cdf_hat <- cd_func(lin_hat)
    pdf_hat <- pd_func(lin_hat)
    cdf1mincdf_hat <- (cdf_hat * (1 - cdf_hat))
    em11_hat <- (pdf_hat^2) / cdf1mincdf_hat
    # ^-- again we use the binomial structure to simplify E[m11|X] (estimates)
    vhat <- 1 / mean(em11_hat * resaux * d)
    # ^-- here we use an information equality (likelihood structure used)
  } else {
    bhat <- NA # refitting failed, so nothing to report
    vhat <- NA
  }
  return(list(bhat = bhat, vhat = vhat, converged = converged,
              fit2 = fit2, postfit2 = ifelse(post == TRUE, postaux, NA)))
}

#' Debias Coefficient on Non-Constant Regressor in Binary Response Model Using
#' BCV
#'
#' This function applies bootstrap-after-cross-validation (BCV) to debias the
#' coefficient on the first non-constant regressor in a binary response model.
#' The procedure first performs BCV to estimate initial parameters and then
#' applies debiasing to improve the coefficient estimates.
#'
#' @param x A numeric matrix or data frame of explanatory variables.
#' @param y A numeric vector of binary responses (0 or 1).
#' @param link A string specifying the link function to use. Default is
#'             \code{"logit"}. Other options are supported if compatible with
#'             \code{binomial()} family.
#' @param nfolds Integer, the number of folds for cross-validation. Default is
#'               10.
#' @param foldid1 A vector of fold assignments for the first cross-validation
#'                (used in BCV). If NULL, the function will generate the folds
#'                automatically.
#' @param foldid2 A vector of fold assignments for the second cross-validation
#'                (used in debiasing). If NULL, the function will generate the
#'                folds automatically.
#' @param standardize Logical, whether to standardize the predictors. Default is
#'                    TRUE.
#' @param intercept Logical, whether to include an intercept in the model.
#'                  Default is TRUE.
#' @param parallel Logical, whether to parallelize the computation. Default is
#'                 FALSE.
#' @param nboot Integer, the number of bootstrap iterations for BCV. Default is
#'              1000.
#' @param c0 Numeric, a tuning parameter used in the BCV method. Default is 1.1.
#' @param alpha Numeric, a regularization parameter used in the BCV method.
#'              Default is 0.1 divided by the logarithm of the maximum number
#'              of features.
#'
#' @return A list containing: \item{bhat}{The debiased coefficient estimate.}
#'   \item{vhat}{The estimated variance of the debiased coefficient.}
#'   \item{converged}{A logical indicating whether the algorithm converged.}
#'   \item{bcv1}{The fitted model object from the first BCV step.}
#'
#' @seealso \code{\link{bcv_binary}}, \code{\link{debias_binary}}
#'
#' @examples
#' # Example usage of debias_bcv function
#' data(iris) x <- as.matrix(iris[, -5]) y <- ifelse(iris$Sepal.Length > 5, 1,
#' 0) result <- debias_bcv(x, y, foldid1 = sample(1:10, nrow(x), replace =
#' TRUE))
#'
#' @export
debias_bcv <- function(x, y, link = "logit", nfolds = 10,
                       foldid1 = NULL, foldid2 = NULL,
                       standardize = TRUE, intercept = TRUE, parallel = FALSE,
                       nboot = 1000, c0 = 1.1, alpha = .1 / log(max(dim(x)))) {
  # bootstrap after cross-validation...
  bcv1 <- bcv_binary(x, y, link = link, nfolds = nfolds, foldid = foldid1,
                     standardize = standardize, intercept = intercept,
                     parallel = parallel, nboot = nboot, c0 = c0, alpha = alpha)
  # ... debias afterwards
  if (anyNA(bcv1$that) == FALSE) {
    bcv3 <- debias_binary(x, y, link = link,
                          intr = bcv1$intr, theta = bcv1$that,
                          nfolds = nfolds, foldid = foldid2,
                          parallel = parallel,
                          standardize = standardize, intercept = intercept,
                          boot = TRUE, post = FALSE)
  }
  return(list(bhat = bcv3$bhat, vhat = bcv3$vhat,
              converged = bcv3$converged, bcv1 = bcv1))
}

#' Debias Coefficient on Non-Constant Regressor in Binary Response Model Using
#' Post-BCV Estimates
#'
#' This function applies post-bootstrap cross-validation (post-BCV) to debias
#' the coefficient on the first nonconstant regressor in a binary response
#' model. The procedure first performs post-BCV to estimate initial parameters
#' and then applies debiasing to improve the coefficient estimates.
#'
#' @param x A numeric matrix or data frame of explanatory variables.
#' @param y A numeric vector of binary responses (0 or 1).
#' @param link A string specifying the link function to use. Default is
#'             \code{"logit"}. Other options are supported if compatible with
#'             \code{binomial()} family.
#' @param nfolds Integer, the number of folds for cross-validation. Default is
#'               10.
#' @param foldid1 A vector of fold assignments for the first cross-validation
#'                (used in post-BCV). If NULL, the function will generate the
#'                folds automatically.
#' @param foldid2 A vector of fold assignments for the second cross-validation
#'                (used in debiasing). If NULL, the function will generate the 
#'                folds automatically.
#' @param standardize Logical, whether to standardize the predictors. Default is
#'                    TRUE.
#' @param intercept Logical, whether to include an intercept in the model.
#'                  Default is TRUE.
#' @param parallel Logical, whether to parallelize the computation. Default is
#'                 FALSE.
#' @param nboot Integer, the number of bootstrap iterations for post-BCV.
#'              Default is 1000.
#' @param c0 Numeric, a tuning parameter used in the post-BCV method. Default is
#'           1.1.
#' @param alpha Numeric, a regularization parameter used in the post-BCV method.
#'              Default is 0.1 divided by the logarithm of the maximum number of
#'              features.
#'
#' @return A list containing:
#'   \item{bhat}{The debiased coefficient estimate.}
#'   \item{vhat}{The estimated variance of the debiased coefficient.}
#'   \item{converged}{A logical indicating whether the algorithm converged.}
#'   \item{postbcv2}{The fitted model object from the second post-BCV step.}
#'   \item{postbcv1}{The fitted model object from the first post-BCV step.}
#'
#' @seealso \code{\link{post_bcv_binary}}, \code{\link{debias_binary}}
#'
#' @examples
#' # Example usage of debias_post_bcv function
#' data(iris)
#' x <- as.matrix(iris[, -5])
#' y <- ifelse(iris$Sepal.Length > 5, 1, 0)
#' result <- debias_post_bcv(x, y, foldid1 = sample(1:10, nrow(x), replace = TRUE))
#'
#' @export
debias_post_bcv <- function(x, y, link = "logit", nfolds = 10,
                            foldid1 = NULL, foldid2 = NULL,
                            standardize = TRUE, intercept = TRUE,
                            parallel = FALSE, nboot = 1000,
                            c0 = 1.1, alpha = .1 / log(max(dim(x)))) {
  # bootstrap after cross-validation...
  postbcv1 <- post_bcv_binary(x, y, link = link,
                              nfolds = nfolds, foldid = foldid1,
                              standardize = standardize, intercept = intercept,
                              parallel = parallel, nboot = nboot,
                              c0 = c0, alpha = alpha)
  # ... debias afterwards
  if (anyNA(postbcv1$that) == FALSE) {
    postbcv3 <- debias_binary(x, y, link = link,
                              intr = postbcv1$intr, theta = postbcv1$that,
                              nfolds = nfolds, foldid = foldid2,
                              standardize = standardize, intercept = intercept,
                              parallel = parallel, boot = TRUE, post = TRUE)
  }
  return(list(bhat = postbcv3$bhat, vhat = postbcv3$vhat,
              converged = postbcv3$converged,
              postbcv2 = postbcv3$fit2, postbcv1 = postbcv1))
}