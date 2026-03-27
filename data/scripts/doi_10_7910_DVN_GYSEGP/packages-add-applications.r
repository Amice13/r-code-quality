# install packages from CRAN
p_needed <- c("readr",
              "haven",
              "reshape2",
              "ggplot2",
              "margins",
              "stringr",
              "purrr",
              "janitor",
              "magrittr",
              "gdata",
              "scales",
              "ggthemes",
              "labelled",
              "xts",
              "networkD3",
              "mokken",
              "stargazer",
              "broom",
              "xtable",
              "grid",
              "RColorBrewer",
              "readxl",
              "writexl",
              "AER",
              "lubridate",
              "legislatoR",
              "dplyr",
              "pageviews",
              "colorspace",
              "coefplot2",
              "coda",
              "rvest",
              "igraph",
              "texreg")
packages <- rownames(installed.packages())
p_to_install <- p_needed[!(p_needed %in% packages)]
if (length(p_to_install) > 0) {
  install.packages(p_to_install, repos = "http://cran.us.r-project.org")
}
lapply(p_needed, require, character.only = TRUE)


#install.packages("reshape")
#install.packages("coefplot2",repos="http://www.math.mcmaster.ca/bolker/R",type="source")

# code from colorspace
colorspace_mod <- function (n, h = c(300, 75), c. = c(35, 95), l = c(15, 90), power = c(0.8, 
                                                                                        1.2), fixup = TRUE, gamma = NULL, alpha = 1, ...) 
{
  if (!is.null(gamma)) 
    warning("'gamma' is deprecated and has no effect")
  if (n < 1L) 
    return(character(0L))
  h <- rep(h, length.out = 2L)
  c <- rep(c., length.out = 2L)
  l <- rep(l, length.out = 2L)
  power <- rep(power, length.out = 2L)
  rval <- seq(1, 0, length = n)
  rval <- hex(polarLUV(L = l[2L] - diff(l) * rval^power[2L], 
                       C = c[2L] - diff(c) * rval^power[1L], H = h[2L] - diff(h) * 
                         rval), fixup = fixup, ...)
  if (!missing(alpha)) {
    alpha <- pmax(pmin(alpha, 1), 0)
    alpha <- format(as.hexmode(round(alpha * 255 + 1e-04)), 
                    width = 2L, upper.case = TRUE)
    rval <- paste(rval, alpha, sep = "")
  }
  return(rval)
}


# find closest matches to identify label positions
find_matches <- function(x, y) {
  if(length(x) > length(y)) stop("x must not be longer than y.")
  dist_mat <- sapply(x, function(xx) abs(y - xx))
  rank_mat <- sapply(x, function(xx) rank(abs(y - xx)))
  closest_matches <- numeric()
  for(i in 1:ncol(dist_mat)){
    min_rank <- order(rank_mat[,i])[1]
    closest_matches[i] <- y[min_rank]
    rank_mat <- rank_mat[-min_rank,]
    dist_mat <- dist_mat[-min_rank,]
  }
  return(closest_matches)
}

# compute age
get_age <- function(from, to) {
  from_lt = as.POSIXlt(from)
  to_lt = as.POSIXlt(to)
  
  age = to_lt$year - from_lt$year
  
  ifelse(to_lt$mon < from_lt$mon |
           (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
         age - 1, age)
}

