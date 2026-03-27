
#############################################################
#### Top-Level Functions for use in TSMO Paper Analysis #####
#############################################################

# Multiplot Functions

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


# Function to Run Primary Variants of Models #

# This function executes all of the regression models in the following analysis, allowing for variation in the two forms of the IV, 
# two forms of the DV, and changing the added control variables. First two arguments should be strings, 
# third argument should be a list of control variables in string form.

lagged_dv <- c("polyarchy","liberal","partip","delib","egal",
               "elec","frefair","fr.assoc","fre.exp","suffr")
ioscore <- c("ioscore.polyarchy","ioscore.liberal","ioscore.partip","ioscore.delib","ioscore.egal",
             "ioscore.elec","ioscore.frefair","ioscore.fr.assoc","ioscore.fre.exp","ioscore.suffr")
dem.perf <- c("dem.perf.poly","dem.perf.liberal","dem.perf.partip","dem.perf.delib","dem.perf.egal",
              "dem.perf.poly","dem.perf.poly","dem.perf.poly","dem.perf.poly","dem.perf.poly")
neighbor <- c("neighbors400.poly","neighbors400.liberal","neighbors400.partip","neighbors400.delib","neighbors400.egal",
              "neighbors400.elec","neighbors400.frefair","neighbors400.fr.assoc","neighbors400.fre.exp","neighbors400.suffr")
inter <- c(paste0("tsmo.wtd.",lagged_dv,"_inter"))



tsmomodel <- function(dvform,ivform,controlvars,FEs=TRUE) {
  if(dvform == "diff"){
    dvs <- c("polyarchy_diff","liberal_diff","partip_diff","delib_diff","egal_diff",
             "elec_diff","frefair_diff","fr.assoc_diff","fre.exp_diff","suffr_diff")
  } else {
    dvs <- c("polyarchy_lead","liberal_lead","partip_lead","delib_lead","egal_lead",
             "elec_lead","frefair_lead","fr.assoc_lead","fre.exp_lead","suffr_lead")
  }
  if(ivform == "score"){
    ivs <- c("tsmoscore.polyarchy","tsmoscore.liberal","tsmoscore.partip","tsmoscore.delib","tsmoscore.egal",
             "tsmoscore.elec","tsmoscore.frefair","tsmoscore.fr.assoc","tsmoscore.fre.exp","tsmoscore.suffr")
  } else {
    ivs <- c("tsmo.wtd.polyarchy","tsmo.wtd.liberal","tsmo.wtd.partip","tsmo.wtd.delib","tsmo.wtd.egal",
             "tsmo.wtd.elec","tsmo.wtd.frefair","tsmo.wtd.fr.assoc","tsmo.wtd.fre.exp","tsmo.wtd.suffr")
  }
  varyingvars <- c(intersect(c("lagged_dv","ioscore","dem.perf","neighbor","inter"),controlvars))
  varying <- do.call("paste",c(lapply(varyingvars,function(x) get(x)),sep = "+"))
  df <- data.frame(dvs,ivs,varying)
  controls <- controlvars[!controlvars %in% varyingvars]
  if(FEs == TRUE) {
    forms <- apply(df,1, function(x) paste(x[1],paste(x[2],x[3],paste(controls,collapse = "+"),"factor(ccode)",sep = "+"),sep = "~"))
  } else {
    forms <- apply(df,1, function(x) paste(x[1],paste(x[2],x[3],paste(controls,collapse = "+"),sep = "+"),sep = "~"))
  }
  mods <- lapply(forms,lm)
  
  mods
}


# Function to Calculate Robust Clustered Standard Errors

cluster.ses <- function(mod,clust) {
  var.covar <- vcovCL(mod, cluster = clust)
  coeftest(mod, vcov. = var.covar)
}
