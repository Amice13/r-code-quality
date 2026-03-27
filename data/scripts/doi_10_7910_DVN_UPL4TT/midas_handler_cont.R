.midas <- function(dgp, imp_meth="ppd", m, ncat=3){
    library(readr)
    imps = m
    type <- dgp$type
    
    filepath <- "kropko/"
    suff <- "data_tmp"
    
    outpath <- paste(filepath, suff, "/", sep="")
    imp_meth <- dgp$imp_meth
    data <- dgp$data
    v <- which(names(data)=="y_1")

    datafile <- paste(c(outpath, 'data.csv'), collapse= "")
    write.csv(data, datafile, row.names = FALSE)
    markerfile <-paste(c(outpath, 'marker.csv'), collapse= "")
    file.create(markerfile)

    print(type)
    while (file.exists(markerfile)) {
        Sys.sleep(1)
    }
    print("Commencing")
    time1 <- proc.time()
    imp <- list()
    imp$imputations <- list()
    for(n in seq(m)){
        imppath <- paste(outpath, "imp", n, '.csv', sep="")
        midas_imp <- read.csv(imppath)
        if(n == 1){
            imp$imputations$imp1 <- midas_imp
        }
        else if(n == 2){
            imp$imputations$imp2 <- midas_imp
        }
        else if(n == 3){
            imp$imputations$imp3 <- midas_imp
        }
        else if(n == 4){
            imp$imputations$imp4 <- midas_imp
        }
        else if(n == 5){
            imp$imputations$imp5 <- midas_imp
        }
        else if(n == 6){
            imp$imputations$imp6 <- midas_imp
        }
        else if(n == 7){
            imp$imputations$imp7 <- midas_imp
        }
        else if(n == 8){
            imp$imputations$imp8 <- midas_imp
        }
        else if(n == 9){
            imp$imputations$imp9 <- midas_imp
        }
        else if(n == 10){
            imp$imputations$imp10 <- midas_imp
        }
        else if(n == 11){
            imp$imputations$imp11 <- midas_imp
        }
        else if(n == 12){
            imp$imputations$imp12 <- midas_imp
        }
        else if(n == 13){
            imp$imputations$imp13 <- midas_imp
        }
        else if(n == 14){
            imp$imputations$imp14 <- midas_imp
        }
        else if(n == 15){
            imp$imputations$imp15 <- midas_imp
        }
        else if(n == 16){
            imp$imputations$imp16 <- midas_imp
        }
        else if(n == 17){
            imp$imputations$imp17 <- midas_imp
        }
        else if(n == 18){
            imp$imputations$imp18 <- midas_imp
        }
        else if(n == 19){
            imp$imputations$imp19 <- midas_imp
        }
        else if(n == 20){
            imp$imputations$imp20 <- midas_imp
        }
        else{
            stop("Something has gone wrong in the imputation assignment loop")
        }
    }
    time2 <- proc.time()
    time <- as.numeric((time2-time1)[3])			
    class(imp$imputations) <- "list"
    
    


    if (type=="continuous") {
        mvmatch <- mean(sapply(imp$imputations, FUN = function(d){sqrt(mean(((d$y_1 - dgp$true)^2)[dgp$miss]))}))
        mvmatch.bias <- mean(sapply(imp$imputations, FUN = function(d){abs(mean(d$y_1[dgp$miss]) - mean(dgp$true[dgp$miss]))}))
    } else {
        mvmatch <- mean(sapply(imp$imputations, FUN = function(d){mean((d$y_1==dgp$true)[dgp$miss])}))
        mvmatch.bias <- NA	
    }
    
    RHS <- pool(dgp$f1, data=imp$imputations, m=imps, FUN=bayesglm)
    coef1 <- sqrt(mean((RHS@coefficients - coef(dgp$rhs))^2))
    coef1.bias <- abs(mean(RHS@coefficients) - mean(coef(dgp$rhs)))
    coef1.mns <- mahalanobis(x=coef(dgp$rhs), center=RHS@coefficients, cov=vcov(dgp$rhs))
    eta <- model.matrix(dgp$rhs)%*%RHS@coefficients 
    fit1.av <- sqrt(mean((eta[dgp$obs] - dgp$rhs.fitted[dgp$obs])^2))
    fit1.av.bias <- abs(mean(eta[dgp$obs]) - mean(dgp$rhs.fitted[dgp$obs]))
    fit1.all <- sqrt(mean((eta - dgp$rhs.fitted)^2))
    fit1.all.bias <- abs(mean(eta) - mean(dgp$rhs.fitted))
    
    if(type=="continuous"){
        Pr.rmse <- NA; Pr.bias <- NA
        LHS <- pool(dgp$f2, data=imp$imputations, m=imps, FUN=bayesglm)
        coef2 <- sqrt(mean((LHS@coefficients - coef(dgp$lhs))^2))
        coef2.bias <- abs(mean(LHS@coefficients) - mean(coef(dgp$lhs)))
        coef2.mns <- mahalanobis(x=coef(dgp$lhs), center=LHS@coefficients, cov=vcov(dgp$lhs))
        eta <- model.matrix(dgp$lhs)%*%LHS@coefficients 
        fit2.av <- sqrt(mean((eta[dgp$obs] - dgp$lhs.fitted[dgp$obs])^2))
        fit2.av.bias <- abs(mean(eta[dgp$obs]) - mean(dgp$lhs.fitted[dgp$obs]))	
        fit2.all <- sqrt(mean((eta - dgp$lhs.fitted)^2))
        fit2.all.bias <- abs(mean(eta) - mean(dgp$lhs.fitted))		
    }
    res <- list()
    res$time <- time
    res$mvmatch <- mvmatch; res$mvmatch.bias <- mvmatch.bias
    res$pr.rmse <- Pr.rmse; res$pr.bias <- Pr.bias
    res$coef1 <- coef1; res$coef1.bias <- coef1.bias
    res$coef2 <- coef2; res$coef2.bias <- coef2.bias
    res$coef1.mns <- coef1.mns; res$coef2.mns <- coef2.mns
    res$fit1.all <- fit1.all; res$fit1.all.bias <- fit1.all.bias
    res$fit2.all <- fit2.all; res$fit2.all.bias <- fit2.all.bias
    res$fit1.av <- fit1.av; res$fit1.av.bias <- fit1.av.bias
    res$fit2.av <- fit2.av; res$fit2.av.bias <- fit2.av.bias
    res$data <- imp$imputations
    res$lhs <- LHS
    res$rhs <- RHS
    return(res)
}
