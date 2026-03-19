allequal <- function(x) length(unique(x)) == 1

formatM <- function(x,format.=0,for.latex=FALSE){
    if(format.=="*"){
        if(for.latex){
            if(x<.001) return("^{***}")
            if(x<.01) return("^{**}")
            if(x<.05) return("^*")
            #if(x<.1) return(".")
            else return("")
            }
        else{
            if(x<.001) return("***")
            if(x<.01) return("**")
            if(x<.05) return("*")
            #if(x<.1) return(".")
            else return("")
            }
        }
    else 
        return(formatC(x,digits=as.numeric(format.),format="f"))
    }

apply.template<-function(x,template,...){
	template <- as.matrix(template)
    pattern <- paste("\\(\\$",1:length(x),":[0-9*]\\)",sep="")
    if(length(names(x))) npattern <- paste("\\(\\$",names(x),":[0-9*]\\)",sep="")
    else npattern <- c()
    lx <- length(x)
    lp <- length(pattern)
    lnp <- length(npattern)
    lt <- length(template)
    
    res <- template
    for(i in 1:lx){
        regexp.res <- regexpr(pattern[i],template)
        if(any(regexp.res > 0)){
            index <- which(regexp.res >0)
            for(j in index){
                strt <- regexp.res[j]
                stp <- attr(regexp.res,"match.length")[j] + strt -1
                tt <- substr(template[j],strt,stp)
                format. <- gsub(")","",unlist(strsplit(tt,":"))[2])
                formatted <- formatM(x[i],format.,...)
                res[j] <- gsub(pattern[i],formatted,res[j])
            }
        }
        if(length(npattern))
            n.regexp.res <- regexpr(npattern[i],template)
        else n.regexp.res <- -1
        if(any(n.regexp.res > 0)){
            index <- which(n.regexp.res >0)
            for(j in index){
                strt <- n.regexp.res[j]
                stp <- attr(n.regexp.res,"match.length")[j] + strt -1
                tt <- substr(template[j],strt,stp)
                format. <- gsub(")","",unlist(strsplit(tt,":"))[2])
                formatted <- formatM(x[i],format.,...)
                res[j] <- gsub(npattern[i],formatted,res[j])
            }
        }
        }
    res 
    }

    
    
    
    
    
    
    
    
    
    
    