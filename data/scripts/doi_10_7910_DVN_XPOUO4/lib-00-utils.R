library(data.table)
library(knitr)
library(parallel)

## how remember works:
## pass it a variable you want to keep
## it will store it in a list 
## then at the bottom of your R document
## add the following:
##


remember <- function (v, k, silent=FALSE) {
  if (missing(k)) {
    k <- deparse(substitute(v))
  }
  
  ## save to the global r variable/list
  r[[k]] <<- v
  
  if (!silent) {
    print(r[[k]])
    flush.console()
  }
  
  invisible(r[[k]])
  ## return(r[[k]])
}

## function help with building reduced summaries
reduced.summary <- function (m) {
  magic.pattern <- "(wikia.com|jedipedia.de)"
  
  if (class(m)[1] == "coeftest") {
    x <- do.call(cbind, lapply(1:4, function (i) {m[,i]}))
    rownames(x) <- names(m[,1])
    colnames(x) <- c("Estimate","Std. Error","z value","Pr(>|z|)")
  } else {
    x <- coef(summary(m))
  }
  
  if (!class(x) == "list") {  
    x <- list(x)
  }
  
  out <- lapply(x, function (y) { y[!grepl(magic.pattern, rownames(y)),] })
  
  ## it's possible that this will only include one line/parameter (blocked)
  if (class(out[[1]]) == "numeric") {
    out <- lapply(out, function (y) {
      y <- as.data.frame(rbind(y))
      rownames(y) <- "blocked"
      return(y)
    })
  }
  
  Lower.95 <- round(out[[1]][,1] - (1.96*out[[1]][,2]),2)
  Upper.95 <- round(out[[1]][,1] + (1.96*out[[1]][,2]),2)
  cbind(out[[1]], Lower.95, Upper.95)
}

## make sure that appendix and nosave are always defined
if (!exists("appendix")) { appendix <- FALSE }
if (!exists("nosave")) { nosave <- FALSE }
