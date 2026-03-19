pLD = function(b1, N0, Nt, mu, k)
# Calculate PDF for LD distribution
# Input:
#       b1:      a scalar, cell growth rate
#       N0:      a scalar, initial population size
#       Nt:      a scalar, population size at time of plating
#       mu:      a scalar, cell mutation rate
#       k:       a scalar, number of mutant cells
# Output:
#        p.vec:  a vector containing p0, p1, ..., pk
{
  m = mu / b1 * (Nt - N0)
  phi = 1 - N0 / Nt
  p.vec = rep(0, k + 1)
  p.vec[1] = exp(-m)
  for (i in 1 : k)
  {
    for (j in 1 : i)
    {
      p.vec[i + 1] = p.vec[i + 1] + phi ^ (j - 1) * (1 - j * phi / (j + 1)) * p.vec[i + 1 - j]
    }
    p.vec[i + 1] = p.vec[i + 1] * m / i
  }
  return(p.vec)
}