###########################################################################
# FUNCTION
###########################################################################
diallelIV <- function(data, yvar, progeny, male, female, rep) {
  #############################################################
  # ANALYSIS OF DIALLEL - METHOD 4 (neither parents nor reciprocal F1's)
  # based on Griffing (1956)
  #############################################################
  ## data: *data frame* containing the data of the diallel experiment
  ## yvar: (*character string*) name of outcome variable in 'data' 
  ## progeny: (*character string*) name of variable designating crosses in 'data' 
  ## male: (*character string*) name of variable designating male parent in 'data'
  ## female: (*character string*) name of variable designating female parent in 'data'
  ## rep: (*character string*) name of variable designating replicate in 'data'
  
  #-------------------------------------------------------------
  # DATA
  # Making general input 'dataframe'
  #-------------------------------------------------------------
  dataframe <- data.frame(yvar = data[, yvar],
                          progeny = data[, progeny],
                          male = data[, male],
                          female = data[, female],
                          rep = data[, rep]
  )
  
  # p <- max(dataframe$male, dataframe$female) # Number of parents
  p <- max(dataframe$male, dataframe$female) # Number of parents
  r <- length(unique(dataframe$rep)) # Number of replicates

  dataframe$male = factor(dataframe$male, levels=as.character(1:p))
  dataframe$female = factor(dataframe$female, levels=as.character(1:p))
  dataframe$progeny = factor(dataframe$progeny)
  dataframe$rep = factor(dataframe$rep)
  
  #-------------------------------------------------------------
  # ANALYSIS OF PROGENY
  # ANOVA for variation over crosses (no decomposition by GCA and SCA)
  #-------------------------------------------------------------
  # ANOVA
  md1 <- lm(yvar ~ progeny + rep, contrasts=list(progeny="contr.sum"), data = dataframe)
  anvout <- anova(md1)
  attr(anvout, "heading")[2] <- paste("Response:", yvar)
  
  m <- anvout$Df[3] #degrees of freedom for error variance
  
  #-------------------------------------------------------------
  # DIALLEL ANALYSIS
  # ANOVA for variation by GCA and SCA
  #-------------------------------------------------------------
  # INPUT
  ## p x p Matrix of mean performance over replicates for each cross
  dataframe$yhat <- predict(lm(yvar ~ rep + progeny, data = dataframe))
  
  X <- xtabs(yhat ~ female + male,
             data=aggregate(yhat ~ female + male, dataframe, mean)
  )
  
  tX <- t(X)
  
  ## Sums
  X_i. <- rowSums(X + tX)   # X_i. = Sum_(i =/= j){x_ij} where x_ij = x_ji
  X_.. <- sum(X)            # X_.. = Sum_(i < j){x_ij}
  
  ## Sums of squares
  SS_g <- sum(X_i.^2)/(p-2) - 4*(X_..^2)/(p*(p-2))                  # SS for GCA
  SS_s <- sum(X^2) - sum(X_i.^2)/(p-2) + 2*(X_..)^2/((p-1)*(p-2))   # SS for SCA
  SS_e <- anvout$`Sum Sq`[3]/r                                      # SS for Error
  
  SS <- c(SS_g, SS_s, SS_e)
  
  ## Degrees of freedom for tests on F-values
  NDF <- c(p-1, p*(p - 3)/2, m) # Numerator degrees of freedom for MS_GCA, MS_SCA and MS_Error respectively
  DDF <- c(p*(p - 3)/2, m, NA)  # Denominator degrees of freedom for MS_GCA, MS_SCA and MS_Error respectively
  
  ## Mean squares
  M_g <- SS_g/(p-1)            # MS_GCA
  M_s <- SS_s/(p*(p-3)/2)      # MS_SCA
  M_e <- anvout$`Mean Sq`[3]/r # MS_Error
  
  MS <- c(M_g, M_s, M_e)
  
  #--------------
  # ESTIMATES
  #--------------
  # Grand mean
  mu <- (2/(p*(p-1)))*X_..
  
  # GCA's
  g <- (p*X_i. - 2*X_..)/(p*(p-2))
  
  # SCA's
  s <- X+t(X) - (matrix(rep(X_i.,p), nrow=p) + matrix(rep(X_i.,p), nrow=p, byrow=TRUE))/(p-2) + X_../((p-1)*(p-2)/2)
  diag(s) <- NA
  
  #--------------
  # ANOVA
  #--------------
  # ANOVA table with degrees of freedom, SS and MS
  anovaIV <- data.frame(Df = NDF,
                        `Sum Sq` = SS,
                        `Mean Sq` = MS,
                        check.names = FALSE)
  rownames(anovaIV) <- c("GCA", "SCA", "Error")
  class(anovaIV) <- c("anova", "data.frame")
  
  #--------------
  # OUTPUT
  #--------------
  return(
    list(
      anova=list(progeny=anvout,
                 diallel=anovaIV
      ),
      est=list(
        mu=mu,
        GCA=g,
        SCA=s
      )
    )
  )
}
