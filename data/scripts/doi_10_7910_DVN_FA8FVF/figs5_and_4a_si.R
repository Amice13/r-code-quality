##############################################################
# Replication file for Kaufman, King, and Komisarchik (2018)
# This file uses the "compactness" software package
##############################################################

# if you'd like, use this line in RStudio to set your working directory
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

print("Running figs5_and_4a_si.R...")

##############################################################
############## Packages 
##############################################################
library(grid)
library(compactness) ## This should load all the remaining libraries below, but just in case:

library(sp)
library(sf)
library(rgdal)
library(raster)
library(rgeos)
library(png)
library(pracma)
library(jpeg)
library(image.CornerDetectionHarris)
library(magick)
library(geosphere)
library(shotGroups)
library(RcppRoll)
library(gbm)
library(randomForest)
library(e1071)
library(cleangeo)
library(ggplot2)
library(dplyr)

set.seed(02138)


##############################################################
############## Setup 
##############################################################

#Load training labels
  data(training_labels)
# Read shapefiles
  sl = "../data/both.shp"
  namecol = "NAME"
  shp = read_shapefiles(sl, namecol)

# Generate compactness features
  features = generate_features(shp) # Warning: this takes a while
  features = features[,-c(1:12, 14)]
  save(features, file="../results/training_features.RData")

# Merge training labels
  #load("../results/training_features.RData")
  bf2 = merge(features, train_labels, by.x="NAME", by.y="district")
  bf2 = bf2[,-29]

##############################################################
############## Cross-validation 
##############################################################

  a = c()
  resids = list()
  idx = bf2$set
  bf2 = bf2[,-c(1,30)]
  ols = list()
  boost = list()
  rfm = list()
  sv = list()
  
  for(i in 1:6){
    train = bf2[idx!= i,]
    test = bf2[idx==i,]
  
    ols[[i]] <- lm(compactness ~ polsby + boyce + hull + corners + sym_x + sym_y + orig_area  + varcoord_ratio + jagged +
                polsby*hull + polsby*sym_x + polsby*sym_y + sym_x*sym_y + varline + polsby*corners + hull*corners +
                polsby*sym_x*sym_y + cornervar_ratio + cornervar_ratio*varcoord_ratio,
              data = train)
    opreds <- predict(ols[[i]], newdata = test)
  
    boost[[i]] <- gbm(compactness ~ .,
                 data =train[,-1], interaction.depth = 3, n.trees = 2000, distribution = "gaussian")
    bpreds <- predict(boost[[i]], newdata = test, n.trees = 1000)
  
    rfm[[i]] <- randomForest(x = train[,-c(1,28)], y = train$compactness, ntree = 2000)
    rfpreds <- predict(rfm[[i]], test)
  
    sv[[i]] <- svm(compactness ~ . , data =train[,-1])
    svpreds <- predict(sv[[i]], test)
  
    ensemble = rowMeans(cbind(opreds, bpreds, rfpreds, svpreds))
    ensemble[ensemble > 100] = 100
    ensemble[ensemble < 0] = 0
    a[i] = cor(ensemble, test$compactness)
    resids[[i]] = data.frame(district = test$district, truth = test$compactness, preds = ensemble, set = i)
  
    label1 = paste0("rho == ", round(a[i],digits=2))
  
    df = data.frame(xv = rank(ensemble), yv=test$compactness)
    test = 100
    if(i==2){
      test = 98
    }
  
  plotx = ggplot(df, aes(x=xv, y=yv)) + ylab(paste0("Test Set: ", i)) + geom_point(size=.5) +
    xlab(paste("Train Sets:", paste(setdiff(1:6, i), collapse=","))) +
    geom_abline(intercept=0, slope=1) + coord_fixed() +
    scale_x_discrete(limit = c(1,25,50,75, 100), labels=c(1,25,50,75,100)) +
    scale_y_discrete(limit = c(1,25,50,75, test), labels=c(1,25,50,75,100)) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.line.x = element_line(color="black", size = .5),
          axis.line.y = element_line(color="black", size = .5),
          axis.ticks.y=element_blank(),
          axis.text.x = element_text(size=22),
          axis.title.x = element_text(size=22),
          axis.text.y = element_text(size=22),
          axis.title.y = element_text(size=22),
          legend.position = "none") +
    ggplot2::annotate("text", label=label1, x = 75, y=25, col="red", parse=T, size=10)
  assign(paste0("plot", i), plotx)

}

# Allows ggplotting in multiple panels
  multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    plots <- c(list(...), plotlist)
    numPlots = length(plots)
    if (is.null(layout)) {
      layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                       ncol = cols, nrow = ceiling(numPlots/cols))
    }
    if (numPlots==1) {
      print(plots[[1]])
    } else {
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
      for (i in 1:numPlots) {
        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                        layout.pos.col = matchidx$col))
      }
    }
  }

## Cross-Validation: Figure 5
  png(filename = "../results/fig5.png", width=1200, height=700, units="px")
  multiplot(plot1, plot4, plot2, plot5, plot3, plot6, cols=3)
  dev.off()  

  models = c(ols, boost, rfm, sv)
  
  save(models, file = "../results/final_models.RData")
  
##############################################################
############## Residuals 
##############################################################

## Figure 4a: Supplemental Information
## Figure 4b is in a separate script, "fig4b_si.R"
  resids = do.call(rbind, resids)
  resids$deviation = resids$preds - resids$truth
  resids$abs_dev = abs(resids$deviation)
  quantile(resids$deviation, c(0.025, 0.975)) # +/- 20
  hist(resids$deviation, xlab="Prediction Residual", main="95% bounds: [-20.7, 22.1]", 20)
  plot(resids$preds, resids$abs_dev, xlab="Prediction", ylab="Absolute Residual")
  m = lm(abs_dev ~ preds + I(preds^2), data=resids)
  lines(1:100, predict(m, newdata = data.frame(preds=1:100)), lwd=2, col="red")
  plot(resids$preds, resids$deviation, xlab="Prediction", ylab="Residual")
  m = lm(deviation ~ preds + I(preds^3), data=resids)
  lines(1:100, predict(m, newdata = data.frame(preds=1:100)), lwd=2, col="red")

## Find the 95% confidence band
  resids$predbin = cut(resids$preds, breaks=seq(0, 100, by=2.5))
  r2 = resids %>% group_by(predbin) %>%
    dplyr::summarize(top = quantile(abs_dev, 0.95), nc = n())
  
  dat = data.frame(pred = seq(5,97.5, length.out=length(r2$top)), top = r2$top, n = r2$nc)
  dat$meanint = dat$top / sqrt(dat$n)

  png(filename = "../results/fig4a_si.png", width=600, height=400, units="px")
  plot(resids$preds, resids$abs_dev, xlab='Predicted Compactness', ylab="Absolute Deviation",
       cex.lab=1.5, ylim = c(0,40))
      m = lm(top ~ pred + I(pred^2), data=dat)
      lines(1:100, predict(m, newdata = data.frame(pred=1:100)), lwd=2, col="red")
      summary(m)
      
    m = lm(meanint ~ pred + I(pred^2), data=dat)
    lines(1:100, predict(m, newdata = data.frame(pred=1:100)), lwd=2, col="black")
   dev.off()