#Aim: subplots for each region

  # 1. get data for plots
  load(paste0("results/terrordata/terrordata",mm,".Rdata"))
  load(paste0("results/studyarea/studyarea",mm,".Rdata"))
  load(paste0("results/voronoi/voronpoly",mm,".Rdata"))
  voronpoly$id<-rownames(voronpoly@data)
  voron<-read.csv(paste0("results/voronoi/voronid",mm,".csv"))
  #load prevalence data
  load(paste0(getwd(),'/results/evaluation/prevalence.Rdata'))

  # 2. link results with polygons
  eval_df<- read.csv(paste0("results/evaluation/eval-df-xgb_",mm,".csv"))
  #in case there are similar values
  eval_df$prediction <- eval_df$prediction + rnorm(nrow(eval_df), 0, 0.000001)
  voronid<-voron
  voronid$id<-as.character(as.factor(voronid$id))
  ncells<-nrow(voronid)#if missing covariates some polygons are not considered
  nweeks<-53*5
  nobs<-ncells*nweeks
  stopifnot(nobs==nrow(eval_df),local=TRUE)
  voronid<-do.call("rbind", replicate(nweeks, voron, simplify = FALSE))
  #week number
  weekvec<-c(rep(1:nweeks, times=1, each=ncells))
  #put voronoi id on the dataframe
  eval_df<-cbind(eval_df,voronid)
  eval_df$id<-as.character(as.factor(eval_df$id))
  eval_df$week<-weekvec
  head(eval_df);tail(eval_df)
  #compute thresholds
  thresholds <- seq(min(eval_df$prediction,na.rm=TRUE), max(eval_df$prediction,na.rm=TRUE), length.out = 10000)
  false_negative <- rep(NA, length(thresholds))
  truth <- eval_df$terror == 1
  fn <- function(aa, bb){
    sum(!aa & bb) / sum(bb)
  }
  calc_fn <- function(t, eval_df, truth){
    pred      <- eval_df$prediction > t
    fn(pred, truth)
  }
  #Run in parallel if Mac, Linux, Unix, BSD Machine
  if(Sys.info()["sysname"] != 'Windows'){#to run on linux or Mac
    false_negative <- mclapply(thresholds, calc_fn, eval_df = eval_df, truth = truth, mc.cores = nbcores)
  } else {#to run on Windows
    false_negative <- lapply(thresholds, calc_fn, eval_df = eval_df, truth = truth)
  }
  false_negative <- simplify2array(false_negative)

  ########################IMPORTANT OPTION THRESHOLD FALSE NEGATIVES###################
  fnthres <- mypreval[mypreval$region==regionnames[[mm]],]
  fnthres <- fnthres$thres
  my_thresh <- max(thresholds[false_negative < fnthres])
  ######################END IMPORTANT OPTION THRESHOLD FALSE NEGATIVES#################
  
  
  # 3. Assign value and color to correct and wrong predicitons
  
  #1: obs. and predicted attack : correct prediction: green
  #2: non-observed and non-predicted attack : correct : white
  #3: observed and not predicted attack : wrong prediction: red
  #4: not observed but predicted attack : wrong prediction: orange
  colpal4<- c("#009E73","#FFFFFF","#D4261D","#E69F00")#green, white,  red, orange
  
  eval_df$pred<-as.factor(ifelse(eval_df$prediction>my_thresh,"yes","no"))#predicted outcome
  
  eval_df$perf<-ifelse(eval_df$terror>0 & eval_df$pred=="yes", 1,#true positive
                       ifelse(eval_df$terror<1 & eval_df$pred=="no",2,#true negative
                              ifelse(eval_df$terror>0 & eval_df$pred=="no",3, #false negative
                                     ifelse(eval_df$terror<1 & eval_df$pred=="yes",4,5))))#false positive and missing cases
  eval_df$perf <-as.factor(eval_df$perf)
  eval_df$col<-ifelse(eval_df$perf=="1", colpal4[1],
                      ifelse(eval_df$perf=="2", colpal4[2],
                             ifelse(eval_df$perf=="3", colpal4[3], 
                                    ifelse(eval_df$perf=="4", colpal4[4],"grey"))))
  eval_df$col<-as.factor(eval_df$col)

  # 4. Preparation for plotting Voronoi diagram
  #remove polygons without predictions
  keeppol <- voron$id
  map<-list()
  for (i in min(eval_df$week):max(eval_df$week)){
    map[[i]]<-voronpoly
    map[[i]]<-map[[i]][keeppol,]
    map[[i]]@data<-droplevels(map[[i]]@data)
    perf<-eval_df[eval_df$week==i,]
    map[[i]]@data = perf
  }

  # plot theme
  library(ggplot2)
  ASAtheme<-rasterVis::rasterTheme(region = "transparent",base_family = "serif",axis.title.x=element_text(size = 8),
                        legend.text = element_text(family = "serif",size=8),
                        axis.text = element_text(size = 9))
  myxlim = c(myregion@bbox["x", "min"],
           myregion@bbox["x", "max"])
  myylim = c(myregion@bbox["y", "min"],
           myregion@bbox["y", "max"])
  
  
  df <- data.frame(perf = c("1","2","3","4"), col = colpal4 )
  lookupTable <- unique(df)
  colRegions <- as.vector(lookupTable$col[match(levels(eval_df$perf), lookupTable$perf)])
  lattice::trellis.par.set(axis.line=list(col="black"))
  labelat = c(1,2,3,4)
  labeltext = rev(c("False positive","False negative", "True negative","True positive"))
  
  # 5. make and save plots
  #plot random week (e.g. week 180, for testing and illustrative predictive map in the paper)
  myweek<-180
  plot.new()
  p1 <- sp::spplot(map[[min(eval_df$week)+myweek-1]],"perf",xlim=myxlim,ylim=myylim,
                   col.regions = colRegions,col="black",lwd=0.5,
                   cex=0.75,main="",
                   scales = list(draw = FALSE),colorkey = FALSE)
  p1 <- p1 + latticeExtra::layer(sp::sp.polygons(myregion, fill="grey75"),
                                 data=list(myregion=myregion)) + p1 +
    latticeExtra::layer(sp::sp.polygons(myregion, fill=NA,col="black",lwd=1.5),
                        data=list(myregion=myregion))
  #save plot as pdf
  grDevices::cairo_pdf(file=paste0("results/figs/maps/predict",mm,"_",myweek,".pdf"), family="serif")
  print(p1)
  while (!is.null(dev.list()))  dev.off()

  # 6. save videos of the predicted maps
   makeplot <- function(){
    datalist <- map
    lapply(datalist, function(data){
      i <- unique(data$week)
      if (mm==3) {
      cexlab=0.3} else {cexlab=0.75}
    p1<-sp::spplot(data,"perf",col.regions = colRegions,col="black",lwd=0.5,
                 main=paste0("Predictions in week ", i),cex=0.75,xlim=myxlim,ylim=myylim,
                 scales = list(draw = FALSE), 
                 colorkey=list(width=1.5, space="bottom", tick.number=4, # not honoured, can be left out
                               labels=list(at=labelat,labels=labeltext,cex=cexlab )))
    p1 <- p1 + latticeExtra::layer(sp::sp.polygons(myregion, fill="grey75"),
                                     data=list(myregion=myregion)) + p1 +
        latticeExtra::layer(sp::sp.polygons(myregion, fill=NA,col="black",lwd=1.5),
                            data=list(myregion=myregion))
      
    print(p1)
    })
   }
  video_file <- file.path(paste0(getwd(), '/results/figs/maps/SuppVideo/Movie S',mm,'.mp4'))
  av::av_capture_graphics(makeplot(), video_file, 1280, 720, res = 144, framerate = 10,verbose=TRUE)
#END