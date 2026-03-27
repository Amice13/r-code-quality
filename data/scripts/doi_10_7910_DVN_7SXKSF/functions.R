coefpaste <- function(val,par,digits=c(2,2), stars_val=NULL, stars_par=NULL){
    if(length(digits)==1)
        digits <- rep(digits,2)
    # internal function
    func <- function(coef,var){
        # handle 'val'/'coef'
        if(digits[1]>0)
            coefout <- sprintf(coef,fmt=paste("%#.",digits[1],"f",sep=""))
        else if(digits[1]==0)
            coefout <- round(coef,0)
        else if(digits[1]<0){
            cchar <- nchar(as.numeric(strsplit(as.character(coef),"[.]")[[1]][1])) + digits[1]
            coefout <- signif(coef,cchar)
        }
        # handle 'par'/'var'
        if(digits[2]>0)
            varout <- sprintf(var,fmt=paste("%#.",digits[2],"f",sep=""))
        else if(digits[2]==0)
            varout <- round(var,0)
        else if(digits[2]<0){
            vchar <- nchar(as.numeric(strsplit(as.character(var),"[.]")[[1]][1])) + digits[2]
            varout <- signif(var,vchar)
        }
        if(!is.null(stars_val))
            output <- paste(coefout, stars_val(coef,var), " (",varout,")", sep="")
        else if(!is.null(stars_par))
            output <- paste(coefout, " (",varout,")", stars_par(coef,var), sep="")
        else
            output <- paste(coefout, " (",varout,")",sep="")
        return(output)
    }
    # return
    if(length(val)==1)
        return(func(val,par))        
    else if(length(val)>1)
        return(mapply(func,val,par))
}

expResults <- function(dv, tr, labels=.groupNames, digits=2, useNA="na.rm"){
    # function to produce a simple treatment group results table
    if(!is.null(useNA)){
        if(useNA=="na.rm"){
            results <- cbind(round(by(dv,tr,mean,na.rm=TRUE),digits),
                            round(by(dv,tr,sd,na.rm=TRUE),digits),
                            by(dv,tr,length)) #treatment group means, SDs, and n
            for(i in 1:length(results[,3])){
                results[i,3] <- results[i,3]-sum(by(dv,tr,is.na)[[i]])
            }
        }
        else if(useNA==""){} # call imputation function
    }
    else {
        results <- cbind(round(by(dv,tr,mean),2),
                        round(by(dv,tr,sd),2),
                        by(dv,tr,length)) #treatment group means, SDs, and n
    }
    # detect indicator vars for calculating SD of proportion
    if(dim(table(dv))==2){
        if(sum(names(table(dv)) %in% c("0","1"))==2) { # check if indicator
            sdprop <- function(x){
                sd <- sqrt(as.numeric(prop.table(table(x))[1]*prop.table(table(x))[1]))
            }
            results[,2] <- round(by(dv,tr,sdprop),digits)
        }
    }
    # combine and add standard errors
    results <- cbind(results,round(results[,2]/sqrt(results[,3]),2))
    colnames(results) <- c("Mean","SD","N","SE")
    # add treatment group labels, if applicable
    if(is.null(labels)){
        if(exists(".groupNames")==TRUE){
            if(length(.groupNames)==dim(results)[1])
                rownames(results) <- .groupNames
        }
    }
    else {
        if(length(labels)==dim(results)[1])
            rownames(results) <- labels
    }
    class(results) <- c("expresults", class(results))
    return(results) # return results matrix
}

