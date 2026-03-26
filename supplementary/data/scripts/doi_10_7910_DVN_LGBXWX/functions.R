
interleave <- function(v1, v2)
{
  if (length(v1) == 1) {
    c(v1, v2)
  } else {
    c(interleave(head(v1, -1), head(v2, -1)), tail(v1, 1), tail(v2, 1))
  }
}


save_figure <- function(g, filename, ...) {
  tiff(filename, ..., units = "in", res = 400)
  print(g)
  dev.off()
  # embed_fonts(filename, outfile = filename)
  return(NULL)
}

cut_p <- function(x) {
  x <- as.numeric(cut(x, breaks = c(0, .001, .01, .05), include.lowest = TRUE))
  x[is.na(x)] <- 4
  labels <- c("^*", "^*", "^*", " ")
  labels[x]
}

make_table_objects <- function(model, fmt = NULL)
{
  if (is.null(fmt)) {
    fmt <- "%.02f"
  }
  suff <- deparse(substitute(model))
  b <- fixef(model)
  se <- diag(vcov(model)) ^ .5
  z <- abs(b / se)
  p <- 2 * (1 - pnorm(z))
  sd <- c(unlist(VarCorr(model)) ^ .5, sigma(model))
  B <- paste0(sprintf(fmt, b), cut_p(p))
  S <- paste0("(", sprintf(fmt, se), ")")
  SD <- sprintf("%.02f", sd)
  assign(paste0("b_", suff), b, envir = globalenv())
  assign(paste0("se_", suff), se, envir = globalenv())
  assign(paste0("p_", suff), p, envir = globalenv())
  assign(paste0("sd_", suff), sd, envir = globalenv())
  assign(paste0("B_", suff), B, envir = globalenv())
  assign(paste0("S_", suff), S, envir = globalenv())
  assign(paste0("SD_", suff), SD, envir = globalenv())
  invisible()
}

# cluster-robust standard errors
mclx <- function(fm, dfcw, cluster1, cluster2 = NULL, vec = TRUE) {
  if (is.null(cluster2)) {
    M1 <- length(unique(cluster1))
    N <- length(cluster1)
    K <- fm$rank
    dfc1 <- (M1/(M1-1))*((N-1)/(N-K))
    u1 <- apply(sandwich::estfun(fm), 2, function(x) tapply(x, cluster1, sum))
    vc1 <- dfc1*sandwich::sandwich(fm, meat=crossprod(u1)/N )
    vcovMCL <- (vc1) * dfcw
    out <- lmtest::coeftest(fm, vcovMCL)
  } else {
    cluster12 <- paste(cluster1, cluster2, sep = "")
    M1 <- length(unique(cluster1))
    M2 <- length(unique(cluster2))
    M12 <- length(unique(cluster12))
    N <- length(cluster1)
    K <- fm$rank
    dfc1 <- (M1/(M1-1))*((N-1)/(N-K))
    dfc2 <- (M2/(M2-1))*((N-1)/(N-K))
    dfc12 <- (M12/(M12-1))*((N-1)/(N-K))
    u1 <- apply(sandwich::estfun(fm), 2, function(x) tapply(x, cluster1, sum))
    u2 <- apply(sandwich::estfun(fm), 2, function(x) tapply(x, cluster2, sum))
    u12 <- apply(sandwich::estfun(fm), 2, function(x) tapply(x, cluster12, sum))
    vc1 <- dfc1*sandwich::sandwich(fm, meat=crossprod(u1)/N )
    vc2 <- dfc2*sandwich::sandwich(fm, meat=crossprod(u2)/N )
    vc12 <- dfc12*sandwich::sandwich(fm, meat=crossprod(u12)/N)
    vcovMCL <- (vc1 + vc2 - vc12) * dfcw
    # if (se) {
    #   diag(vcovMCL) ^ .5
    # } else {
    out <- lmtest::coeftest(fm, vcovMCL)
    # }
  }
  vars <- c("post", "treatment:post")
  if (vec) {
    round(as.vector(t(out[vars, 1:2])), 3)
  } else {
    round(t(out[vars, 1:2]), 3)
  }
}

summarize <- function(x) data.table(y = mean(x), rbind(quantile(x, c(.025, .975))))

calc_balance <- function(x, tr, varname)
{
  imp <- median(x, na.rm = TRUE)
  data.table(
    varname,
    sprintf("%.02f", mean(ifelse(is.na(x[tr == 1]), median(x[tr == 1], na.rm = TRUE), x[tr == 1]))),
    sprintf("%.02f", mean(ifelse(is.na(x[tr == 0]), median(x[tr == 0], na.rm = TRUE), x[tr == 0]))),
    sprintf("%.02f", t.test(x ~ tr)$p.value),
    sprintf("%.02f", sd(x, na.rm = TRUE)),
    sum(is.na(x))
  )
}
