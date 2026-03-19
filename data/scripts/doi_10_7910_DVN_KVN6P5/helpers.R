
#save list of controls so that all models use the same
controls <- c("population_k", "landuse", "landineq", "lriverm_100km","railroads_100km", "coast_dist_100km", "trade_dist_100km","slaves_100k",
                              "area_1MsqK", "dem1km", "demvar1km", "malaria", "ctoil", "ctdiamonds", "LAT", "LON")


renameCovars <- function(star) {
    
    # change spacing
    star <- str_replace(star, "extracolsep\\{5pt\\}", "extracolsep\\{0pt\\}")
    
    # fix covariate names
    star <- str_replace(star, "^ population\\\\_k", "Population (100k)")
    star <- str_replace(star, "^ typeislam", "Islamic Kingdom")
    star <- str_replace(star, "^ typestateless", "Stateless")
    star <- str_replace(star, "^ typepagan", "Non-Islamic Kingdom")
    star <- str_replace(star, "^ type2pagan", "Non-Islamic Kingdom")
    star <- str_replace(star, "^ type19thislam", "Islamic Kingdom")
    star <- str_replace(star, "^ type19thstateless", "Stateless")
    star <- str_replace(star, "^ type2statelessIslamic", "Stateless (Islamic Influence)")
    star <- str_replace(star, "^ type2statelessNonIslamic", "Stateless (No Islamic Influence)")
    star <- str_replace(star, "^ landuse", "Land Suitability")
    star <- str_replace(star, "^ landineq", "Land Inequality")
    star <- str_replace(star, "^ lriverm\\\\_100km", "River Length (100km)")
    star <- str_replace(star, "^ railroads\\\\_100km", "Railroads (100km)")
    star <- str_replace(star, "^ slaves\\\\_100k", "Slave Exports (100k)")
    star <- str_replace(star, "^ area\\\\_1MsqK", "Area (1000km$^{2}$)")
    star <- str_replace(star, "^ dem1km", "Elevation (km)")
    star <- str_replace(star, "^ demvar1km", "Rough terrain (km)")
    star <- str_replace(star, "^ malaria", "Malaria Suitability")
    star <- str_replace(star, "^ ctoil", "Oil Fields")
    star <- str_replace(star, "^ ctdiamonds", "Diamond Mines")
    star <- str_replace(star, "^ LAT", "Latitude")
    star <- str_replace(star, "^ LON", "Longitude")
    star <- str_replace(star, "^ coast\\\\_dist\\\\_100km", "Coast Distance (100km)")
    star <- str_replace(star, "^ trade\\\\_dist\\\\_100km", "Trade Distance (100km)")
    star <- str_replace(star, "^ dhs\\\\_n\\\\_tmp", "DHS Surveys")
    return(star)
    
}


stampTime <- function(star) c(paste0("% exported on ", Sys.time()), star)

if(color.printing){
    fill1 <- hue_pal()(3)[2]  #green
    fill2 <- hue_pal()(3)[3]  #blue
    fill3 <-  hue_pal()(3)[1]  #red
    fill4 <- "grey80"
    border <- "white"
} else {
    fill1 <- "grey20"
    fill2 <- "grey60"
    fill3 <- "grey80"
    fill4 <- "grey95"
    border <- "white"
}


aggregate_sp <- function(i, spatialdata, raster, fun){
    
    holder <- NA
    
    if(!grepl("UNINHABITED", spatialdata@data[i,"TRIBE_NAME"])){ #uninhabitted
        data.tmp <- spatialdata[i,]
        rast.tmp <- crop(raster, extent(data.tmp))
        rast.tmp <- raster::extract(rast.tmp, data.tmp, fun=fun, na.rm=TRUE) # Extract raster values to list object
        holder <- as.vector(rast.tmp)
    } else {
        holder <- 0
    }
    
    return(holder)
    
}


spatialAggregate_par <- function(spatialdata, raster, name, fun=mean){
    
    #require(svMisc)
    require(raster)
    require(parallel)
    require(pbapply) #parallel function with progress bar
    
    #setup for parallel
    #https://gis.stackexchange.com/questions/188954/using-sp-method-to-subset-spatialpointsdataframe-with-spatialpolygon-returns-e
    #https://stackoverflow.com/questions/12023403/using-parlapply-and-clusterexport-inside-a-function
    #http://michael.hahsler.net/SMU/LearnROnYourOwn/code/doSnow.R
    cl <- makeCluster(detectCores())
    clusterEvalQ(cl, library(rgdal))
    clusterEvalQ(cl, library(raster))
    clusterEvalQ(cl, library(DescTools))  #gini function
    clusterExport(cl, varlist=c("aggregate_sp","spatialdata", "raster", "Gini"),
                  envir=environment())
    
    #input data validation
    #stopifnot(grepl("datum=WGS84", crs(spatialdata)@projargs)) 
    stopifnot(projection(spatialdata)==projection(raster))
    
    cat("Starting parallel spatial aggregation process ... \n")
    holder <- pblapply(X=1:nrow(spatialdata@data), 
                       FUN = function(x) aggregate_sp(i=x,spatialdata, raster, fun=fun), 
                       cl=cl)
    holder <- unlist(holder)
    stopCluster(cl)
    
    #output data validation
    stopifnot(class(holder)=="numeric")
    stopifnot(length(holder)==nrow(spatialdata@data))
    
    #append to the input data
    spatialdata@data <- cbind(spatialdata@data, holder, stringsAsFactors=FALSE)
    colnames(spatialdata@data)[ncol(spatialdata@data)] <- as.character(name)
    
    #export
    return(spatialdata)
    
}

spatialAggregate <- function(spatialdata, raster, name, fun=mean){
    
    require(svMisc)
    require(raster)
    
    #stopifnot(grepl("datum=WGS84", crs(spatialdata)@projargs)) 
    stopifnot(projection(spatialdata)==projection(raster))
    
    holder <- NA
    
    for(i in 1:nrow(spatialdata@data)){
        svMisc::progress(i, max.value=nrow(spatialdata@data))
        
        if(!grepl("UNINHABITED", spatialdata@data[i,"TRIBE_NAME"])){ #uninhabitted
            data.tmp <- spatialdata[i,]
            rast.tmp <- crop(raster, extent(data.tmp))
            rast.tmp <- raster::extract(rast.tmp, data.tmp, fun=fun, na.rm=TRUE) # Extract raster values to list object
            holder[i] <- as.vector(rast.tmp)
        } else {
            holder[i] <- 0
        }
    }
    
    stopifnot(class(holder)=="numeric")
    stopifnot(length(holder)==nrow(spatialdata@data))
    
    spatialdata@data <- cbind(spatialdata@data, holder, stringsAsFactors=FALSE)
    colnames(spatialdata@data)[ncol(spatialdata@data)] <- as.character(name)
    
    return(spatialdata)
    
}

simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
}


#function to get the length of spatiallines within spatial polygons
#borrowed from here: http://r-sig-geo.2731867.n2.nabble.com/R-sig-geo-Sum-lines-when-they-intersect-polygons-td6252850.html
lineLengthIntersects <- function(gridpolys,linegeom){ 
    ngrids = length(gridpolys) 
    ll = rep(0,ngrids) 
    for(i in 1:ngrids){ 
        svMisc::progress(i, max.value=ngrids)
        if(i==1131 | i == 1185){
            ll[i] <- 0
        } else {
            geomint = gIntersection(linegeom,gridpolys[i,]) 
            
            if(!is.null(geomint)){ 
                ll[i] = SpatialLinesLengths(geomint,TRUE) 
            } 
        }
    } 
    return(ll) 
} 


# contains function to collect balance test from MatchBalance object

baltest.collect <-  function(matchbal.out,var.names,after=TRUE)
{
    storemat <- matrix(NA,length(var.names),10)
    colnames(storemat) <- c("mean.Tr","mean.Co","sdiff","sdiff.pooled","var.ratio","T pval","KS pval","sqqmeandiff","sqqmeddiff","sqqmaxdiff")
    rownames(storemat) <- var.names
    
    if(after==FALSE){
        stopifnot(length(matchbal.out$BeforeMatching)==length(var.names))
        for(i in 1:length(matchbal.out$BeforeMatching))
        {
            storemat[i,1] <- matchbal.out$BeforeMatching[[i]]$mean.Tr
            storemat[i,2] <- matchbal.out$BeforeMatching[[i]]$mean.Co
            storemat[i,3] <- matchbal.out$BeforeMatching[[i]]$sdiff
            storemat[i,4] <- matchbal.out$BeforeMatching[[i]]$sdiff.pooled
            storemat[i,5] <- matchbal.out$BeforeMatching[[i]]$var.ratio
            storemat[i,6] <- matchbal.out$BeforeMatching[[i]]$p.value
            TT <- try(matchbal.out$BeforeMatching[[i]]$ks$ks.boot.pvalue,silent=T)
            storemat[i,7] <- ifelse(is.null(TT),NA,TT)
            storemat[i,8] <- matchbal.out$BeforeMatching[[i]]$qqsummary$meandiff
            storemat[i,9] <- matchbal.out$BeforeMatching[[i]]$qqsummary$mediandiff
            storemat[i,10] <- matchbal.out$BeforeMatching[[i]]$qqsummary$maxdiff
        }
        
    } else {
        stopifnot(length(matchbal.out$AfterMatching)==length(var.names))
        for(i in 1:length(matchbal.out$AfterMatching))
        {
            storemat[i,1] <- matchbal.out$AfterMatching[[i]]$mean.Tr
            storemat[i,2] <- matchbal.out$AfterMatching[[i]]$mean.Co
            storemat[i,3] <- matchbal.out$AfterMatching[[i]]$sdiff
            storemat[i,4] <- matchbal.out$AfterMatching[[i]]$sdiff.pooled
            storemat[i,5] <- matchbal.out$AfterMatching[[i]]$var.ratio
            storemat[i,6] <- matchbal.out$AfterMatching[[i]]$p.value
            TT <- try(matchbal.out$AfterMatching[[i]]$ks$ks.boot.pvalue,silent=T)
            storemat[i,7] <- ifelse(is.null(TT),NA,TT)
            storemat[i,8] <- matchbal.out$AfterMatching[[i]]$qqsummary$meandiff
            storemat[i,9] <- matchbal.out$AfterMatching[[i]]$qqsummary$mediandiff
            storemat[i,10] <- matchbal.out$AfterMatching[[i]]$qqsummary$maxdiff
        }
        
    }
    
    #cat("Minimum P value from T-Tests is",min(storemat[,6],na.rm=T),"\n")
    #cat("Minimum P value from KS-Tests is",min(storemat[,7],na.rm=T),"\n")
    #cat("Max qq.max.diff",max(storemat[,10]),"\n")
    return(storemat)
    
}



vcovClusterX <- function(
    model,
    cluster,
    dummies = FALSE
)
{
    require(sandwich)
    require(lmtest)
    if(nrow(model.matrix(model))!=length(cluster)){
        stop("check your data: cluster variable has different N than model")
    }
    M <- length(unique(cluster))
    N <- length(cluster)           
    K <- model$rank
    A <- 0
    if(dummies == TRUE){
        A <- M
    }
    if(M<40){
        warning("Fewer than 40 clusters, variances may be unreliable (could try block bootstrap instead).")
    }
    dfc <- (M/(M - 1)) * ((N - 1)/(N - K + A))
    
    uj  <- apply(sandwich::estfun(model), 2, function(x) tapply(x, cluster, sum))
    rcse.cov <- dfc * sandwich(model, meat = crossprod(uj)/N)
    return(rcse.cov)
}


# code to create a common legend
# #https://github.com/tidyverse/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs

grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right"), which=1) {
    
    #which legend to use
    which <- which
    
    plots <- list(...)
    position <- match.arg(position)
    g <- ggplotGrob(plots[[which]] + theme(legend.position = position))$grobs
    legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
    lheight <- sum(legend$height)
    lwidth <- sum(legend$width)
    gl <- lapply(plots, function(x) x + theme(legend.position="none"))
    gl <- c(gl, ncol = ncol, nrow = nrow)
    
    combined <- switch(position,
                       "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                              legend,
                                              ncol = 1,
                                              heights = unit.c(unit(1, "npc") - lheight, lheight)),
                       "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                             legend,
                                             ncol = 2,
                                             widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
    
    grid.newpage()
    grid.draw(combined)
    
    # return gtable invisibly
    invisible(combined)
    
}

myRobustSE <- function(mod, datatable=data, cluster=data$country_overlap){ # spatial=bind
    
    #does the model include country fe
    dummies <- ifelse(any(grepl("country_overlap", names(coefficients(mod)), fixed=TRUE)), TRUE, FALSE)
    
    #robust standard errors
    rows <- rownames(datatable) %in% rownames(model.matrix(mod))
    robustse <- sqrt(diag(vcovClusterX(mod, cluster[rows], dummies=dummies)))
    
    #morans i
    #dist.inv <- 1/(as.matrix(dist(coordinates(bind[rows,])))); diag(dist.inv) <- 0
    #moran <- paste0(round(Moran.I(mod$residuals, dist.inv)$observed,3), p.value(Moran.I(mod$residuals, dist.inv)$p.value))
    
    #return object
    dummies <- ifelse(dummies, "\\checkmark", "")
    controls <- ifelse(length(robustse) > 3, "\\checkmark", "")
    return <- list("mod"= mod, "robustse" = robustse, 
                   "dummies" = dummies, #"moran" = moran, 
                   "controls" = controls)
    return(return)
}

myExtract <- function(group){
    
    mods <- lapply(group, function(x) x[["mod"]])
    origses <- lapply(group, function(x) x[["origse"]])
    robustses <- lapply(group, function(x) x[["robustse"]])
    dummies <- c("Country FE", unlist(lapply(group, function(x) x[["dummies"]])))
    controls <- c("Controls", unlist(lapply(group, function(x) x[["controls"]])))
    addlines <- list(dummies, controls)
    
    #return objects
    return <- list("mods" = mods, "origses" = origses, "robustses" = robustses, "addlines" = addlines)
    return(return)
    
}
    

myCoefplot <- function(matrix, var, letter, labels.column="reg"){
    
    results <- matrix
    
    #create some objects to use in the plot
    breaks <- unique(results$x)
    limits <- c(min(unique(results$x))-.25, max(unique(results$x))+.25)
    labels <- unique(results[,labels.column])
    
    plot <- ggplot(results[results$var==var,], aes(x=x, y=coef)) +
        geom_hline(yintercept=0) +
        geom_errorbar(aes(ymin=coef-1.64*se, ymax=coef+1.64*se, color=sig), width=0, size=2) +
        geom_errorbar(aes(ymin=coef-1.96*se, ymax=coef+1.96*se, color=sig), width=0, size=1) +
        geom_point(aes(color=sig), shape=21, fill=border, size=3, stroke=1) + 
        scale_color_manual(values=c("TRUE"=fill1, "FALSE"=fill2), labels=c("p > .1", "p < .1"), name="Statistical Significance")+
        labs(y="Effect of Islamic Rule", x= NULL) + 
        ggtitle(paste0(letter, ". ", var))  + theme(legend.position="bottom") + 
        scale_x_continuous(limits=limits, breaks=breaks, labels=labels) + theme_bw()
    
    
    return(plot)
}

