Alg2 = function(z0, a.vec, mu, tp)
# Generate sample path from a 2-type binary-fission MBP
# Input:
#       z0:    scalar, initial number of cells
#       a.vec: vector, life time rate parameter of wild-type and mutant cell 
#       mu:    scalar, mutation probability
#       tp:    scalar, time of plating
# Output:
#        zt:   scalar, total # of viable cells at tp
#        xt:   scalar, # of mutant cells at tp
{
  a1 = a.vec[1]
  a2 = a.vec[2]
  zt.vec = rep(NA, z0)
  xt.vec = rep(NA, z0)
  for (i in 1 : z0)
  {
    ndiv = round(10 * exp(a1 * tp))
    ix = (1 : ndiv)
    intarrt.vec = rexp(ndiv, ix * a1)
    while (sum(intarrt.vec) < tp)
    {
      ndiv.ext = round(10 * exp(a1 * tp))
      ix = ndiv + (1 : ndiv.ext)
      intarrt.ext.vec = rexp(ndiv.ext, ix * a1)
      intarrt.vec = c(intarrt.vec, intarrt.ext.vec)
      ndiv = ndiv + ndiv.ext
    }
    arrt.vec = cumsum(intarrt.vec)
    ndiv = sum(arrt.vec <= tp)
    zt.vec[i] = ndiv + 1
    mutflag.vec = rbinom(ndiv, 1, mu)
    nmut = sum(mutflag.vec)
    mutt.vec = arrt.vec[which(mutflag.vec == 1)]
    remt.vec = tp - mutt.vec
    xt.vec[i] = sum(rgeom(2 * nmut, rep(exp(-a2 * remt.vec), 2))) + 2 * nmut# + nmut
  }
  return(c(sum(zt.vec), sum(xt.vec)))
}
