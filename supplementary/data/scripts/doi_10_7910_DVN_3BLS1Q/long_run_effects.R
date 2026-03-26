library(msm)
long_run <- function(model, var_id, sample, imputed){
  if(class(model)[1]=="list") {
    vmat <- vcov(pool_mi(qhat=MIextract(model, fun = coef),
                         u=MIextract(model, fun = vcovHC)))
    nn <- grep(var_id, colnames(vmat))
    mm <- MIcombine(model)$coef[nn]
    vv <- vmat[nn, nn]
    point <- (mm[2]+mm[3])/(1-mm[1])
    se <- deltamethod(~(x3+x2)/(1-x1), mm, vv)
    cilow90 <- point + qnorm(.05)*se
    cihigh90 <- point + qnorm(.95)*se
    cilow95 <- point + qnorm(.025)*se
    cihigh95 <- point + qnorm(.975)*se
    return(data.frame(point, se, cilow90, cihigh90, cilow95, cihigh95, sample, imputed))
  }
  if(class(model)[1]=="pgmm") {
    vmat <- vcovHC(model)
    nn <- grep(var_id, colnames(vmat))
    mm <- model$coef[nn]
    vv <- vmat[nn, nn]
    point <- (mm[2]+mm[3])/(1-mm[1])
    se <- deltamethod(~(x3+x2)/(1-x1), mm, vv)
    cilow90 <- point + qnorm(.05)*se
    cihigh90 <- point + qnorm(.95)*se
    cilow95 <- point + qnorm(.025)*se
    cihigh95 <- point + qnorm(.975)*se
    return(data.frame(point, se, cilow90, cihigh90, cilow95, cihigh95, sample, imputed))
  }
}
