

spdm_table <- function(object, noModels = 5, digs= 4, caption = NULL, label = NULL,
                       file = NULL){
  library(xtable)
  library(spduration)
# get the index for risk and duration equation  
  
  dur_idx  <- 1:object$n.terms$duration
  risk_idx <- (object$n.terms$duration + 1):(object$n.terms$duration + object$n.terms$risk)

tbl_dur <- cbind(
            object$coefficients$duration[dur_idx],
            object$se[dur_idx],
            object$zstat[dur_idx],
            object$pval[dur_idx]
            )

tbl_risk <- cbind(
            object$coefficients$risk,
            object$se[risk_idx],
            object$zstat[risk_idx],
            object$pval[risk_idx]
            )

tbl_alpha <- cbind(
            object$coefficients$distr,
            object$se[length(object$se)],
            object$zstat[length(object$se)],
            object$pval[length(object$se)]
            )

colnames(tbl_dur) <- colnames(tbl_risk) <- colnames(tbl_alpha) <- c("Estimate", "Std. Error", "t value", "Pr(>|z|)")

tableResults = matrix('', nrow = 5 + nrow(tbl_dur), ncol = noModels)
colnames(tableResults) <- c("Variables", "DurCoeff", "DurSE",  "RiskCoef", "RiskSE")

# get variable names
vars <- unique(c(rownames(tbl_dur), rownames(tbl_risk), rownames(tbl_alpha)))

tableResults[,"Variables"]<- c(vars, "LogLikelihood", "AIC", "BIC", "N")

##create a list to store estimates
tbl <- list(tbl_dur, tbl_risk, tbl_alpha)

for (i in seq_along(tbl)){

temp <- tbl[[i]][match(tableResults[1:length(vars)], rownames(tbl[[i]])),]

estims = tbl[[i]][1:nrow(tbl[[i]]),'Estimate']
estims = round(as.numeric(as.character(estims)),digs)
pvals = abs(tbl[[i]][1:nrow(tbl[[i]]),'Pr(>|z|)'])
pvals = round(as.numeric(as.character(pvals)),digs)
estims = ifelse(pvals<=0.10 & !is.na(pvals) & pvals>0.05,
                paste('$', estims,'^{\\ast}$',sep=''), estims)
estims = ifelse(pvals<0.10 & !is.na(pvals) & pvals<=0.05,
                paste('$', estims,'^{\\ast\\ast}$',sep=''), estims)
estims = ifelse(is.na(estims),'',estims)
serrors = tbl[[i]][1: nrow(tbl[[i]]),'Std. Error']
serrors = round(as.numeric(as.character(serrors)),digs)
serrors = paste('(',serrors,')',sep='')
serrors = ifelse(serrors=='(NA)','',serrors)

if(i < 3){
      tableResults[1:nrow(tbl[[1]]), 2*i] = estims
      tableResults[1:nrow(tbl[[1]]), 2*i +1] = serrors
} 
else{
      tableResults[nrow(tbl[[1]]) + 1, 2] = estims
      tableResults[nrow(tbl[[1]]) + 1, 3] = serrors
}
}

tableResults[nrow(tbl[[1]]) + 2, 2] <- round(object$logL,digs)
tableResults[nrow(tbl[[1]]) + 3, 2] <- round(AIC(object),digs)
tableResults[nrow(tbl[[1]]) + 4, 2] <- round(BIC(object),digs)
tableResults[nrow(tbl[[1]]) + 5, 2] <- length(object$Y[,"fail"])


print(xtable(tableResults, caption = caption,
                    label = label), 
             sanitize.text.function = identity,
             file = file,
             caption.placement = "top", comment = FALSE,digits = 4,
             include.rownames = FALSE)
}
