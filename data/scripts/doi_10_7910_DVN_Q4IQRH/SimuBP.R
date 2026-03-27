SimuBP = function(bran, mupr, n0, tp)
# Generate sample path from a 2-type BP
# Input:
#       bran: a list specifying branching rule.
#             $span = "fixed"/"exp"/"unif"/"gam"
#             $para, a vector or matrix depending on $span
#              e.g., c(l1, l2) for $span = "fixed"
#                    c(a1, a2) for $span = "exp"
#                    cbind(c(a1, b1), c(a2, b2)) for $span = "unif"
#                    cbind(c(a1, b1), c(a2, b2)) for $span = "gam"
#             note parameters with subscript 1 and 2 are for wild-type and mutant cells, respectively
#             $offd, a vector c(p0, p1, ...) specifying offspring distribution
#       mupr: a vector c(mu1, mu2) specifying forward/backward mutation probabilities
#       n0:   a vector c(y0, x0), where y0 and x0 are the # of wild-type and mutant 
#             cells at t=0, respectively
#       tp:   a scalar, time of plating
# Output:
#        zt:  a scalar, total # of viable cells at tp
#        xt:  a scalar, # of mutant cells at tp
{
  yt = 0
  xt = 0
  p.vec = bran$offd
  if (sum(p.vec) != 1)
  {
    stop("The offspring distribution is not correct!")
  }
  # number of possible offspring, off.vec
  off.vec = (0 : (length(p.vec) - 1))
  y = n0[1]
  x = n0[2]
  z = y + x
  mu1 = mupr[1]
  mu2 = mupr[2]
  if (bran$span == "fixed")
  {
    l1 = bran$para[1]
    l2 = bran$para[2]
    arrtime1.vec = rep(l1, y)
    arrtime2.vec = rep(l2, x)
  }
  if (bran$span == "exp")
  {
    a1 = bran$para[1]
    a2 = bran$para[2]
    arrtime1.vec = rexp(y, a1)
    arrtime2.vec = rexp(x, a2)
  }
  if (bran$span == "unif")
  {
    a1 = bran$para[1, 1]
    b1 = bran$para[2, 1]
    a2 = bran$para[1, 2]
    b2 = bran$para[2, 2]
    arrtime1.vec = runif(y, a1, b1)
    arrtime2.vec = runif(x, a2, b2)
  }
  if (bran$span == "gam")
  {
    a1 = bran$para[1, 1]
    b1 = bran$para[2, 1]
    a2 = bran$para[1, 2]
    b2 = bran$para[2, 2]
    arrtime1.vec = rgamma(y, a1, b1)
    arrtime2.vec = rgamma(x, a2, b2)
  }
  ismut1.vec = rbinom(y, 1, mu1)
  ismut2.vec = 1 - rbinom(x, 1, mu2)
  # arrival time (life span for 1st generation) of birth events, once exceeds t, stop counting
  arrtime.wt.vec = c(arrtime1.vec[ismut1.vec == 0], arrtime2.vec[ismut2.vec == 0])
  arrtime.mt.vec = c(arrtime1.vec[ismut1.vec == 1], arrtime2.vec[ismut2.vec == 1])
  if (!identical(arrtime.wt.vec, numeric(0)))
  {
    # number of wild-types continue to split by tp, ncont.wt
    ncont.wt = sum(arrtime.wt.vec < tp)
    # number of wild-types by tp, yt
    yt = yt + sum(arrtime.wt.vec >= tp)
  } else
  {
    # number of wild-types continue to split by tp, ncont.wt
    ncont.wt = 0
    # number of wild-types by tp, yt
    yt = 0
  }
  if (!identical(arrtime.mt.vec, numeric(0)))
  {
    # number of mutants continue to split by tp, ncont.mt
    ncont.mt = sum(arrtime.mt.vec < tp)
    # number of mutants by tp, xt
    xt = xt + sum(arrtime.mt.vec >= tp)
  } else
  {
    # cont.mt.vec = T
    # number of mutants continue to split by tp, ncont.mt
    ncont.mt = 0
    # number of mutants by tp, xt
    xt = 0
  }
  zt = yt + xt
  ncont = ncont.wt + ncont.mt
  while (ncont > 0)
  {
    if (!identical(arrtime.wt.vec, numeric(0)))
    {
      arrtime.wt.prev.vec = arrtime.wt.vec[arrtime.wt.vec < tp]
      prob.wt.mat = rmultinom(ncont.wt, 1, p.vec)
      noffspring.wt.vec = colSums(prob.wt.mat * off.vec)
    } else
    {
      arrtime.wt.prev.vec = NULL
      noffspring.wt.vec = 0
    }
    if (!identical(arrtime.mt.vec, numeric(0)))
    {
      arrtime.mt.prev.vec = arrtime.mt.vec[arrtime.mt.vec < tp]
      prob.mt.mat = rmultinom(ncont.mt, 1, p.vec)
      noffspring.mt.vec = colSums(prob.mt.mat * off.vec)
    } else
    {
      arrtime.mt.prev.vec = NULL
      noffspring.mt.vec = 0
    }
    if (bran$span == "fixed")
    {
      l1 = bran$para[1]
      l2 = bran$para[2]
      spltime.wt.vec = rep(l1, sum(noffspring.wt.vec))
      spltime.mt.vec = rep(l2, sum(noffspring.mt.vec))
    }
    if (bran$span == "exp")
    {
      a1 = bran$para[1]
      a2 = bran$para[2]
      spltime.wt.vec = rexp(sum(noffspring.wt.vec), a1)
      spltime.mt.vec = rexp(sum(noffspring.mt.vec), a2)
    }
    if (bran$span == "unif")
    {
      a1 = bran$para[1, 1]
      b1 = bran$para[2, 1]
      a2 = bran$para[1, 2]
      b2 = bran$para[2, 2]
      spltime.wt.vec = runif(sum(noffspring.wt.vec), a1, b1)
      spltime.mt.vec = runif(sum(noffspring.mt.vec), a2, b2)
    }
    if (bran$span == "gam")
    {
      a1 = bran$para[1, 1]
      b1 = bran$para[2, 1]
      a2 = bran$para[1, 2]
      b2 = bran$para[2, 2]
      spltime.wt.vec = rgamma(sum(noffspring.wt.vec), a1, b1)
      spltime.mt.vec = rgamma(sum(noffspring.mt.vec), a2, b2)
    }
    arrtime1.vec = rep(arrtime.wt.prev.vec, noffspring.wt.vec) + spltime.wt.vec
    ismut1.vec = rbinom(sum(noffspring.wt.vec), 1, mu1)
    arrtime2.vec = rep(arrtime.mt.prev.vec, noffspring.mt.vec) + spltime.mt.vec
    ismut2.vec = 1 - rbinom(sum(noffspring.mt.vec), 1, mu2)
    arrtime.wt.vec = c(arrtime1.vec[ismut1.vec == 0], arrtime2.vec[ismut2.vec == 0])
    arrtime.mt.vec = c(arrtime1.vec[ismut1.vec == 1], arrtime2.vec[ismut2.vec == 1])
    
    if (!identical(arrtime.wt.vec, numeric(0)))
    {
      # number of wild-types continue to split by tp, ncont.wt
      ncont.wt = sum(arrtime.wt.vec < tp)
      # number of wild-types by tp, yt
      yt = yt + sum(arrtime.wt.vec >= tp)
    } else
    {
      # number of wild-types continue to split by tp, ncont.wt
      ncont.wt = 0
      # number of wild-types by tp, yt
      # yt = 0
    }
    if (!identical(arrtime.mt.vec, numeric(0)))
    {
      # number of mutants continue to split by tp, ncont.mt
      ncont.mt = sum(arrtime.mt.vec < tp)
      # number of mutants by tp, xt
      xt = xt + sum(arrtime.mt.vec >= tp)
    } else
    {
      # cont.mt.vec = T
      # number of mutants continue to split by tp, ncont.mt
      ncont.mt = 0
      # number of mutants by tp, xt
      # xt = 0
    }
    zt = yt + xt
    ncont = ncont.wt + ncont.mt
  }
  
  return(c(zt, xt))
}
