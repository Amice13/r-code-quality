Alg3 = function(z0, a.vec, mu, tp)
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
  zt = sum(rgeom(z0, exp(-a1 * tp)))
  m = round(zt * mu) # number of mutations by tp
  if (m > 0)
  {
    tm.vec = log(runif(m) * (exp(a1 * tp) - 1) + 1) / a1
    xt = sum(rgeom(2 * m, exp(-a2 * (tp - tm.vec)))) + 2 * m
  } else
  {
    xt = 0
  }
  return(c(zt, xt))
}
