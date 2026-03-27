#' @param long Should the results be returned in long format?
RMA.est <- function(d, v, long=TRUE) {
  
  #analyzes MA data set using standard RE model estimators
  #produces estimate of true effect, CI around estimate,
  #and estimate of tau (could get CIs if wanted)
	
	# adjust stepadj (make it smaller by x0.5) and increase maxiter from 100 to 500 to prevent convergence problems
	#reMA <- rma(d, v, method="REML")
	
	reMA <- tryCatch(
		rma(d, v, method="REML", control = list(stepadj = .5, maxiter=500)),
		error = function(e) {
			NULL
		}
	)
	 
	# if it fails: return empty
	if (is.null(reMA)) {
		warning("RMA did not converge.")
		# initialize empty results object
		res <- data.frame(
			method = "reMA",
			term = "b0",
			estimate = NA,
			std.error = NA,
			statistic = NA,
			p.value = NA,	# one-tailed p-value of p-uniform's test of null-hypothesis of no effect
			conf.low = NA,
			conf.high = NA
		)
		return(returnRes(res, long))
	}
	  
  # assign NULL to tfMA if an error is raised
	tfMA <- NULL
  try({
	  tfMA <- trimfill(reMA, side="left", maxiter=500)
  }, silent=TRUE)  
  
  res <- data.frame(method="reMA", tidyRMA(reMA))
  res <- plyr::rbind.fill(res, data.frame(
	  method="reMA",
	  term=c("tau2", "I2", "Q"),
	  estimate=c(reMA$tau2, reMA$I2, reMA$QE),
	  std.error=c(reMA$se.tau2, NA, NA)
	))		
	  
    
  if (!is.null(tfMA)) {	  
	  res <- rbind(res, data.frame(method="TF", tidyRMA(tfMA)))
	  res <- plyr::rbind.fill(res, data.frame(
		  method="TF",
		  term="tau2",
		  estimate=tfMA$tau2,
		  std.error=tfMA$se.tau2
		))
	  res <- plyr::rbind.fill(res, data.frame(
		  method="TF",
		  term="kFilled",
		  estimate=tfMA$k0
		))
  }
    
  returnRes(res, long)
}








tidyRMA <- function(RMA) {
  res <- data.frame(
    term = if(length(RMA$b)==1) {"b0"} else {c("b0", "b1")},
    estimate = as.vector(RMA$b),
    std.error = RMA$se,
    statistic = RMA$zval,
    p.value = RMA$pval,
    conf.low = RMA$ci.lb,
    conf.high = RMA$ci.ub
  )
  rownames(res) <- NULL
  return(res)
}

tidyLM <- function(...) {
  res <- tidy(..., conf.int=TRUE)
  res$term[res$term == "(Intercept)"] <- "b0"
  res$term[res$term == "sqrt(v)"] <- "b1"
  res$term[res$term == "v"] <- "b1"
  return(res)
}


# returns a result data frame either in wide or long format
returnRes <- function(res, long=TRUE, reduce=TRUE) {
  if (is.null(res)) return(NULL)
  
  # convert all factor columns to characters
  res %>% mutate_if(is.factor, as.character) -> res	
  
  if (long==FALSE) {
    # return wide format
    return(res)
  } else {
    # transform to long format
    longRes <- melt(res, id.vars=c("method", "term"))
    if (reduce==TRUE & nrow(res) > 1) {longRes <- longRes %>% filter(!is.na(value)) %>% arrange(method, term, variable)}
    return(longRes)
  }
}


# simple wrapper: formats a number in f.2 format
f2 <- function(x, digits=2, prepoint=0, skipZero=FALSE) {
  
  if (skipZero == TRUE) {zero <- "."} else {zero <- "0."}
  
  if (length(dim(x)) == 2) {
    apply(x, 2, function(x2) {gsub("0.", zero, sprintf(paste("%",prepoint,".",digits,"f",sep=""), x2) , fixed=TRUE)})
  } else {
    gsub("0.", zero, sprintf(paste("%",prepoint,".",digits,"f",sep=""), x) , fixed=TRUE)
  }
}
