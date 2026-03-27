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

mergeNA <- function(...) {
    vars <- list(...)
    if(length(vars)==1)
        return(vars[[1]])
    lengths <- sapply(vars,FUN=length)
    # check variable lengths
    if(!identical(rep(lengths[1],length(vars)), lengths))
        stop("Vectors specified have different lengths")
    # test for NAs in each vector
    a <- do.call(cbind,vars)
    amat <- is.na(a)
    # check for mutual missingness
    mutual <- rowSums(!amat) > 1 #mutual <- apply(!amat, 1, sum) > 1
    if(any(mutual) & sum(mutual)>=10)
        stop("Missingness is not mutually exclusive at 10 or more indices")
    else if(any(mutual))
        stop("Missingness is not mutually exclusive at indices ",paste(which(mutual),collapse=","))
    # positions of non-NAs in each vector
    notNA <- apply(!amat, 2, which)
    if(!is.list(notNA))
        notNA <- lapply(seq_len(ncol(notNA)), function(i) notNA[,i])
    # pairs of NA positions and merge-able values
    p <- mapply(function(val,pos) val[pos], vars, notNA, SIMPLIFY=FALSE)
    # replace NAs, if applicable
    vars[[1]][unlist(notNA)] <- unlist(p)
    return(vars[[1]])
}
