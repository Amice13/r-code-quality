## DESCRIPTION
##   Post-Selection and Post-Regularization Inference in Linear
##   Models with Many Controls and Instruments
##
## DEPENDENCIES
##   $Wellness/data/proc/lasso/lasso_data.dta


# -----------------------------------------------------------------------------
# SETUP

# Model variables, passed from the command line
(args <- commandArgs(trailingOnly = TRUE))
if (length(args)) {
  ID <- c(args[1])  # 
  in_lasso <- c(args[2])  # 
  Y <- c(args[3])  # 
  d <- c(args[4])  # 
  x <- strsplit(args[5], " ")[[1]]  #
  aweight <- c(args[6])  #  
  dta_in <- c(args[7])  #  
  do_out_y <- c(args[8])  #  
  do_out_d <- c(args[9])
} else {
  # For interactive testing. Note that projdir (path to project directory) must be set below.
  ID <- "AnalysisID"
  effect <- "IV"
  in_lasso <- paste0("in_lasso_", effect) 
  Y <- "spend_0816_0717"
  d <- "hra_c_nomiss"
  x <- c("spend_0715_0716", "nonzero_spend_0715_0716", "spendRx_0715_0716", "spendOff_0715_0716", "spendHosp_0715_0716", 
         "everscreen", "active", "active_try", "cursmk", "othersmk", "formsmk", "drink", "drinkhvy", "chronic", 
         "health1", "health2", "problems", "energy", "ehealth", "overweight", "badhealth", "sedentary", "druguse", 
         "physician", "hospital", "sickdays", "hrsworked50", "jobsatisf1", "jobsatisf2", "mgmtsafety", "male", 
         "age50", "age37_49", "white", "salaryM1", "salaryQ1", "salaryQ2", "salaryQ3", "faculty", "AP")
  aweight <- "covg_0816_0717"
  projdir <- file.path("PATH_TO_PROJECT_DIR")
  setwd(projdir)
  dta_in <- file.path(projdir, paste0("data/proc/lasso/lasso_data_", Y, ".dta")) 
  do_out_y <- file.path(projdir, paste0("data/proc/lasso/lasso_do_", effect, "_y_", Y, ".do")) 
  do_out_d <- file.path(projdir, paste0("data/proc/lasso/lasso_do_", effect, "_d_", Y, ".do")) 
}
keepvars <- c(ID, in_lasso, Y, d, x, aweight)

# Install new packages to first element of .libPaths(). If this path is not writeable 
#  (e.g. after a fresh installation of R), install to default user library path
lib <- .libPaths()[1]
if (!dir.create(lib, showWarnings = FALSE)[1]) {
  lib <- Sys.getenv("R_LIBS_USER")
  dir.create(lib, showWarnings = FALSE)
}

# Select repository for downloading new R packages
repos <- "http://cran.us.r-project.org"

# Specify packages to load, installing as needed
packages <- c("tidyverse", "magrittr", "readstata13", "scales", "tools", "glmnet", 
              "foreign", "haven", "sandwich", "lmtest", "doParallel")
installed <- packages %in% installed.packages()[, "Package"]
invisible(lapply(packages[!installed], install.packages, lib = lib, repos = repos, dependencies=TRUE))
suppressMessages(invisible(lapply(packages, library, character.only=TRUE)))

# Seed for replicability of LASSO k-fold cross validation
seed <- 1


#CITES -----------------------------------------------------------------------------
## Chernuzhukov, Hansen, Spinderl (AERPP 2015)
# Post-Regularization Inference with Many Controls
# URL: http://pubs.aeaweb.org/doi/pdfplus/10.1257/aer.p20151022

# (p. 488)
# "...one could follow the double-selection strategy more closely by running Lasso regression 
# of d_i on x_i and z_i, Lasso regression of d_i on x_i, Lasso regression of y_i on x_i, and 
# then forming a 2SLS  estimator using instruments selected in the first step and controlling 
# for the union of controls selected in the three Lasso steps."

## Urminsky, Hansen, Chernozhukov, 2016
# Using Double-Lasso Regression for Principled Variable Selection


#Setup -----------------------------------------------------------------------------

print(paste("dta_in:", dta_in))

# Load the data
wellness_data <- read.dta13(dta_in) %>% 
  as_tibble() %>% 
  #filter_(paste(in_lasso,"==", 1)) %>%
  filter(!!sym(in_lasso) == 1) %>% 
  select(one_of(keepvars)) %>% 
  na.omit()

# (0)  Setup

# Function to print the variable names and coefficients post Lasso
save_lasso_vars <- function(coef, dofile = NULL, pre = "v_dbl_", print = TRUE) {
  # Lasso-selected variables, including intercept
  dimnames <- coef@Dimnames[[1]][coef@i + 1]
  dimnames_noconst <- dimnames %>% 
    .[!(. %in% c("(Intercept)"))]
  
  if (print) {
    data.frame(name = dimnames, coefficient = coef@x) %>%
      print()
    dimnames_noconst %>% 
      length() %>%
      paste("Number of selected controls (excluding intercept):", .) %>%
      print()
  }
  
  # If dofile is specified, create a do file for generating selected variables in Stata
  if (!is.null(dofile)) {
    if (length(dimnames_noconst) > 0) {
      dimnames_noconst %>% 
        gsub(pattern = ":", replacement = "*", x = ., fixed = TRUE) %>% 
        paste0("gen ", pre, 1:length(.), " = ", .) %>% 
        #as_tibble() %>% 
        enframe(name = NULL) %>% 
        write_csv(dofile, col_names = FALSE)
    } else {
      tibble() %>% write_csv(dofile, col_names = FALSE)
    }
    
  }
  
  # Return the indices of non-zero coefficients, where 0 is the index for the intercept term
  return(coef@i)
}

# Model
x_glmnet <- 2 %>% # =1 for direct effects, =2 for interactions
  combn(x, .) %>% # list of combinations of controls
  apply(2, paste, collapse="*") %>% 
  paste(collapse=" + ") %>% 
  paste0(" ~ ", .) %>% 
  as.formula() %>% 
  sparse.model.matrix(data = wellness_data) %>% 
  .[,-1] %T>%
  ncol() # Show number of potential predictors in x_2way, but return model matrix (%T>% pipe)

Y_glmnet <- as.matrix(dplyr::select(wellness_data, !!sym(Y)))
d_glmnet <- as.matrix(dplyr::select(wellness_data, !!sym(d)))
weights <-  as.matrix(dplyr::select(wellness_data, !!sym(aweight)))

num_cores <- min(detectCores(), 10)
cl<-makeCluster(num_cores)
registerDoParallel(cl)

set.seed(seed)

# STEP 1: select variables that predict outcomes
x_Y <- cv.glmnet(x = x_glmnet, y = Y_glmnet, weights = weights, alpha = 1, nfolds = 10, parallel = TRUE) %>% 
  coef(s = "lambda.min") %>%  # Lasso coefs using CV-MSE-minimizing penalty
  save_lasso_vars(dofile = do_out_y, pre = "v_dbl_y_")

# STEP 2: select variables that predict treatment
x_d <- cv.glmnet(x = x_glmnet, y = d_glmnet, weights = weights, alpha = 1, nfolds = 10, parallel = TRUE) %>% 
  coef(s = "lambda.min") %>%  # Lasso coefs using CV-MSE-minimizing penalty
  save_lasso_vars(dofile = do_out_d, pre = "v_dbl_d_")

stopCluster(cl)

# STEP 3: linear regression with both sets of variables; compare to results estimated in Stata
x_use_ <- x_glmnet[, union(x_Y, x_d)] %>% 
  as.matrix()
glm(Y_glmnet ~ 1 + d_glmnet + x_use_, weights = weights , data = wellness_data) %>% 
  coeftest(vcov = vcovHC(., "HC1"))

