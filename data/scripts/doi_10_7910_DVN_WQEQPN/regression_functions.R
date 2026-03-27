## Functions
RegFor <- function(y,x, FE, IV = "0" , clust = "0"){
  ## The default is no IV and no cluster
  print(x)
  print(length(x))
  controls <- ifelse(length(x) == 1, x , paste(x, collapse = " + "))
  print(controls)
  part1 <- paste(y, " ~ ", controls)
  part2 <- ifelse(length(FE) == 1 , FE, paste(FE, collapse = " + "))
  part3 <- IV
  part4 <- ifelse(length(clust) == 1 , clust, parte(clust, sep = "+"))
  Formula <- paste(part1 , part2, part3, part4 ,  sep = " | ")
  return(Formula)
}

AddLines <- function(regN , title, content ) {
  ltoadd <- c(title, content)
  #if (regN +1 != length(ltoadd)) stop("Incorrect length for line given # of regs")
  #return(ltoadd)
}

LatMc <- function(entry,n) {
  return(paste0("\\multicolumn{" , n, "}{c}{", entry, "}" ))
}


##### function to extract coefficients, errors, and confidence intervals from model output
get_plot_data <- function(x,coef_no){
  beta <- coef(x)[coef_no]
  se <- sqrt(diag(x$clustervcv))[coef_no]
  lb <- beta - 1.96*se
  ub <- beta + 1.96*se
  ret <- data.frame(beta,se,lb,ub)
  return(ret)
}

##### function to estimate linear models with option to include spatial lags instrumented with spatial lags of baseline covariates
mdlr_dummy_plus <- function(y,x,cntr,fe,iv = NULL,cl,dat,wgt=NULL, 
                            conley_cutoff=NULL, lat=NULL, lon=NULL,kernel="bartlett",dist_fn="Haversine",
                            listw=NULL,lag_dummies=TRUE){
  require(dummies)
  require(spdep)
  require(lfe)
  
  ## provide function with
  # outcome variable y as string
  # main predictor x as string
  # exogeneous control variables cntr as string vector
  # fe variable(s) fe as string or "0" if no fe
  # instrument for main predictor iv as string but keep empty or NULL if no iv
  # cluster variable(s) for standard errors cl as string or "0" if no clustering
  # dataset dat
  # km cutoff for conley ses
  # lat and lon variables for conley ses
  # kernel and dist_fn for conley ses
  # connectivity matrix in listw format if spatial 2SLS with or without endogeneous x is required
  # set lag dummies to TRUE if you want to include spatial lags of fixed effects dummies as instruments, and to FALSE if not.
  control.str <- paste(cntr, collapse = " + ") # define control string
  if(is.null(listw)){ # first everything non-spatial with errors clustered by cl
    if(is.null(iv)){ # simple OLS without IV
      form.str <- paste(y, "~" , control.str, "+", x)
      add.felm <- paste("|", paste(fe, collapse = " + "), "| 0 |", paste(cl, collapse = " + "))
      form <- as.formula(paste(form.str, add.felm))
    } else { # 2SLS-IV
      form.str <- paste(y, "~" , control.str)
      add.felm <- paste("|", paste(fe, collapse = " + "), "|(", x, "~", iv, ")|", paste(cl, collapse = " + "))
      form <- as.formula(paste(form.str, add.felm))
    }
    if(is.null(conley_cutoff)) {
      if(is.null(wgt)){
        m <- felm(form, data=dat, exactDOF = T, keepCX=T) # estimate and return model
      } else{
        m <- felm(form, data=dat, exactDOF = T, keepCX=T,weights=dat[,wgt])  
      }
      return(m)
    } else { # same but now with conley SE
      source("Code/Conley/Conley_Spatial.R")
      if(is.null(iv)){ # OLS, no IV, Conley errors
        form.str <- paste(y, "~" ,control.str, "+", x)
        add.felm <- paste("|", paste(fe, collapse = " + "), "| 0 |", "0")
        form.c <- as.formula(paste(form.str, add.felm))
        if(is.null(wgt)){
          m.c <- felm(form.c, data=dat, keepCX = T, exactDOF = T)
        } else{
          m.c <- felm(form.c, data=dat, keepCX = T, exactDOF = T,weights=dat[,wgt])  
        }
        m.c$clustervcv <- ConleySpatial(m.c, dist_cutoff = conley_cutoff, dat=dat,
                                        lat=lat, lon=lon, kernel=kernel, dist_fn = dist_fn)
        return(m.c)
      } else { # 2SLS-IV with Conley errors
        form.str <- paste(y, "~" , control.str)
        add.felm <- paste("|", paste(fe, collapse = " + "), "|(", x, "~", iv, ")|", "0")
        form.c <- as.formula(paste(form.str, add.felm))
        m.c <- felm(form.c, data=dat, keepCX = T, exactDOF = T)
        m.c$clustervcv <- ConleySpatial(m.c, dist_cutoff = conley_cutoff, dat=dat,
                                        lat=lat, lon=lon, kernel=kernel, dist_fn = dist_fn)
        form.str.1 <- paste(x, "~" , control.str, " + ", iv) # re-estimate first stage to get Conley-based F stats
        add.felm.1 <- paste("|", paste(fe, collapse = " + "), "| 0 |", lat, " + ", lon)
        form.c.1 <- as.formula(paste(form.str.1, add.felm.1))
        m.c.1 <- felm(form.c.1, data=dat, keepCX = T,exactDOF = T)
        m.c.1$clustervcv <- ConleySpatial(m.c.1, dist_cutoff = conley_cutoff, dat=dat,
                                          lat=lat, lon=lon, kernel=kernel, dist_fn = dist_fn)
        m.c.1$f.value <- lfe::waldtest(m.c.1,paste(iv),type="cluster")["F"]
        out <- list(m.c,m.c.1)
        names(out) <- c("stage2","stage1")
        return(out)
      }  
    }
  } else { # now everything as spatial 2SLS with or without endogeneous x and also implement conley 
    # generate lag and lag_sq for outcome, all controls, and x
    if(fe!="0" & isTRUE(lag_dummies)) {
      dummies <- data.frame(dummy(fe,data=dat,sep="."))[,-1]
      dat <- cbind(dat,dummies)
      du <- names(dummies)
      splags <- lapply(names(dat[,c(y,cntr,du,x)]),
                       function(n){
                         lag <- lag.listw(listw,dat[,n],zero.policy = T)
                         lag.sq <- lag.listw(listw,lag,zero.policy = T)
                         temp <- data.frame(lag, lag.sq)
                         names(temp) <- paste0(c("lag_","lag.sq_"),n)
                         return(temp)
                       })
      splags <- data.frame(do.call(cbind,splags))
      dat <- cbind(dat,splags)
    } else {
      dummies <- data.frame(dummy(fe,data=dat,sep="."))[,-1]
      dat <- cbind(dat,dummies)
      du <- names(dummies)
      splags <- lapply(names(dat[,c(y,cntr,x)]),
                       function(n){
                         lag <- lag.listw(listw,dat[,n],zero.policy = T)
                         lag.sq <- lag.listw(listw,lag,zero.policy = T)
                         temp <- data.frame(lag, lag.sq)
                         names(temp) <- paste0(c("lag_","lag.sq_"),n)
                         return(temp)
                       })
      splags <- data.frame(do.call(cbind,splags))
      dat <- cbind(dat,splags)
    }
    # make model formula w/o endogeneous x and standard error cl
    if(is.null(iv)){ # simple OLS without IV
      if(fe=="0"){
        form.str <- paste(y, "~" , control.str, " + ", x)
      } else {
        form.str <- paste(y, "~" , control.str," + ", paste(du, collapse = " + "), " + ", x)
      }
      add.felm <- paste("|", "0", "|(", names(splags)[1], "~", 
                        paste(names(splags)[3:ncol(splags)], collapse = " + "), ")|", 
                        paste(cl, collapse = " + "))
      form <- as.formula(paste(form.str,add.felm))
    } else { # 2SLS-IV
      if(fe=="0"){
        form.str <- paste(y, "~" , control.str)
      } else {
        form.str <- paste(y, "~" , control.str, " + ", paste(du, collapse = " + "))
      }
      add.felm <- paste("|", "0", "|(", names(splags)[1], "|", x, "~", 
                        paste(names(splags)[3:(ncol(splags)-2)], collapse = " + "), "+", iv, ")|", 
                        paste(cl, collapse = " + "))
      form <- as.formula(paste(form.str,add.felm))
    }
    if(is.null(conley_cutoff)) { 
      m <- felm(form, data=dat, exactDOF = T, keepCX=T) # estimate and return model
      return(m)
    } else { # same but now with conley SE
      source("~/Code/Conley/Conley_Spatial.R")
      if(is.null(iv)){ # Spatial 2SLS without endogeneous x but with Conley errors
        if(fe=="0"){
          form.str <- paste(y, "~" , control.str, " + ", x)
          form.str.1 <- paste(names(splags)[1], "~" , control.str, " + ", x, " + ", 
                              paste(names(splags)[3:ncol(splags)], collapse = " + "))
        } else {
          form.str <- paste(y, "~" , control.str," + ", paste(du, collapse = " + "), " + ", x)
          form.str.1 <- paste(names(splags)[1], "~" , control.str, " + ", paste(du, collapse = " + "), " + ",
                              x, " + ", paste(names(splags)[3:ncol(splags)], collapse = " + "))
        }
        add.felm <-  paste("|", "0", "|(", names(splags)[1], "~", 
                           paste(names(splags)[3:ncol(splags)], collapse = " + "), ")|", 
                           "0")
        add.felm.1 <-  paste("| 0 | 0 |", 
                             "0")
        form.c <- as.formula(paste(form.str, add.felm))
        m.c <- felm(form.c, data=dat, keepCX = T, exactDOF = T)
        m.c$clustervcv <- ConleySpatial(m.c, dist_cutoff = conley_cutoff, dat=dat,
                                        lat=lat, lon=lon, kernel=kernel, dist_fn = dist_fn)
        
        form.c.1 <- as.formula(paste(form.str.1, add.felm.1)) # re-estimate first stage to get Conley-based F stats
        m.c.1 <- felm(form.c.1, data=dat, keepCX = T, exactDOF = T)
        m.c.1$clustervcv <- ConleySpatial(m.c.1, dist_cutoff = conley_cutoff,dat=dat,
                                          lat=lat, lon=lon, kernel=kernel, dist_fn = dist_fn)
        m.c.1$f.value <- lfe::waldtest(m.c.1,which(grepl("lag", rownames(m.c.1$coefficients))),type="cluster")["F"]
        out <- list(m.c,m.c.1)
        names(out) <- c("stage2","stage1.spatial")
        return(out)
        
      } else { # Spatial 2SLS with endogeneous x and Conley errors
        if(fe=="0"){
          form.str <- paste(y, "~" , control.str)
          form.str.1 <- paste(names(splags)[1], "~" , control.str, " + ", 
                              paste(names(splags)[3:ncol(splags)-2], collapse = " + "), " + ", iv)
          form.str.2 <- paste(x, "~" , control.str, " + ", 
                              paste(names(splags)[3:ncol(splags)-2], collapse = " + "), " + ", iv)
        } else {
          form.str <- paste(y, "~" , control.str, " + ", paste(du, collapse = " + "))
          form.str.1 <- paste(names(splags)[1], "~" , control.str, " + ", paste(du, collapse = " + "), " + ",
                              paste(names(splags)[3:(ncol(splags)-2)], collapse = " + "), " + ", iv)
          form.str.2 <- paste(x, "~" , control.str, " + ",  paste(du, collapse = " + "), " + ",
                              paste(names(splags)[3:(ncol(splags)-2)], collapse = " + "), " + ", iv)
        }
        add.felm <-paste("|", "0", "|(", names(splags)[1], "|", x, "~", 
                         paste(names(splags)[3:(ncol(splags)-2)], collapse = " + "), "+", iv, ")|", 
                         "0")
        add.felm.1 <-  paste("| 0 | 0 |", 
                             "0")
        form.c <- as.formula(paste(form.str, add.felm))
        m.c <- felm(form.c, data=dat, keepCX = T, exactDOF = T)
        m.c$clustervcv <- ConleySpatial(m.c, dist_cutoff = conley_cutoff, dat= dat,
                                        lat=lat, lon=lon, kernel=kernel, dist_fn = dist_fn)
        # re-estimate first stages to get Conley-based F stats
        # spatial lag
        form.c.1 <- as.formula(paste(form.str.1, add.felm.1))
        m.c.1 <- felm(form.c.1, data=dat, keepCX = T, exactDOF = T)
        m.c.1$clustervcv <- ConleySpatial(m.c.1, dist_cutoff = conley_cutoff, dat=dat,
                                          lat=lat, lon=lon, kernel=kernel, dist_fn = dist_fn)
        m.c.1$f.value <- lfe::waldtest(m.c.1,which(grepl("lag", rownames(m.c.1$coefficients))),type="cluster")["F"]
        # endogeneous x
        form.c.2 <- as.formula(paste(form.str.2, add.felm.1))
        m.c.2 <- felm(form.c.2, data=dat, keepCX = T, exactDOF = T)
        m.c.2$clustervcv <- ConleySpatial(m.c.2, dist_cutoff = conley_cutoff, dat=dat,
                                          lat=lat, lon=lon, kernel=kernel, dist_fn = dist_fn)
        m.c.2$f.value <- lfe::waldtest(m.c.2,paste(iv),type="cluster")["F"]
        out <- list(m.c,m.c.1,m.c.2)
        names(out) <- c("stage2","stage1.spatial","stage1.x")
        return(out)
      }  
    }
  }
}
