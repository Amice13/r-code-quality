###########################################################################
# FUNCTION
###########################################################################
heritability <- function(diallelObj, t, r, p, F.inbr) {
  
#######################################
# estimating variances 
#######################################
# EMS[error] = entry-mean error, v.Error

MS.GCA <- diallelObj$anova$diallel[1,3]
MS.SCA <- diallelObj$anova$diallel[2,3]
MS.Error <- diallelObj$anova$diallel[3,3]


# Test for significance
df.GCA <- diallelObj$anova$diallel[1,1]
df.SCA <- diallelObj$anova$diallel[2,1]
df.Error <- diallelObj$anova$diallel[3,1]

F.stat.SCA <- MS.SCA / MS.Error
SCA.pval <- 1 - pf(F.stat.SCA, df1 = df.SCA, df2 = df.Error)

F.stat.GCA <- MS.GCA / MS.SCA
GCA.pval <- 1 - pf(F.stat.GCA, df1 = df.GCA, df2 = df.SCA)


v.Error <- MS.Error

# EMS[SCA] = v.Error + v.SCA
# v.SCA = EMS[SCA] - v.Error
v.SCA <- MS.SCA - v.Error
# EMS[GCA] = v.Error + v.SCA + (p-2) * v.GCA
# v.GCA = (EMS[GCA] - v.Error - v.SCA) / (p-2)
p <- 8
v.GCA <- (MS.GCA - v.Error - v.SCA) / (p - 2)

#######################################
# estimating Va and Vd 
#######################################

# Va = (4 / (1 + F)) * v.GCA
# Vd = (4 / (1 + F)^2) * v.SCA

F.inbr <- 1 

Va = (4 / (1 + F.inbr)) * v.GCA
Vd = (4 / (1 + F.inbr)^2) * v.SCA


#######################################
# heritability
#######################################

# Broad-sense

# H2 = V.G / (V.G + V.G*E / t + V.Error / rt )
V.G <- Va + Vd
V.GE <- 0
t <- 1
r <- 2

H2 <- V.G / (V.G + (V.GE / t) + (v.Error/ (r*t)) )

# Narrow-sense

# h2 = V.A / (V.G + V.G*E / t + V.Error / rt )
h2 <- Va / (V.G + (V.GE / t) + (v.Error/ (r*t)) )

# Note v.Error reported in diallel4 is already calcualted on entry
# means basis


# Baker's ratio

baker.rat <- (2*MS.GCA) / ((2*MS.GCA + MS.SCA))
  
  
return(
  data.frame(
    MS.GCA = MS.GCA,
    v.GCA = v.GCA,
    Va=Va,
    MS.SCA = MS.SCA,
    v.SCA = v.SCA,
    Vd=Vd,
    Ve=v.Error,
    H2=H2,
    h2=h2,
    br=baker.rat,
    GCA.pval = GCA.pval,
    SCA.pval= SCA.pval
  )
)

}


