#ALE plots for the main specification and robustness specification (that includes fatal events only)

library(ggplot2)
library(iml)
library(xgboost)

# converts the data.frame to a matrix.
predict_fun=function(model, newdata){
  newData_x = xgboost::xgb.DMatrix(data.matrix(newdata), missing = NA)
  results <- predict(model, newData_x)
  return(results)
}

#add loop for each model (XGB all and XGB fatality only)
modelnames <- c("XGB","XGBfatal")
xgbpaths <- list(paste0("../Main/results/xgb/"),paste0(getwd(),"/results/xgb/"))

aleall <-list()

for (j in 1:length(modelnames)){

  aledf <-list()
  
if(j==2) { #since Australia is not computed for fatality data
maxmm <- 12}  else {maxmm <- 13}

#add loop for each region  
for (mm in 1:maxmm){

myxgb <- xgboost::xgb.load(paste0(xgbpaths[j],'xgbsave_',mm))
myxgb_caret<- readRDS(paste0(xgbpaths[j],'xgb-until-2016_',mm,'.Rds'))

#fix the names
newdata <- myxgb_caret$trainingData[, 1:(ncol(myxgb_caret$trainingData)-1)]
names(newdata) <- gsub(' ', '_', names(newdata))

#make sure factor variables are as factor
#build predictor object
predictor <- iml::Predictor$new(myxgb, data = newdata, 
       y = myxgb_caret$trainingData$.outcome,predict.fun = predict_fun )

#remove country variables
allvars <- names(newdata)
torem <- "country"
allvars <-allvars[!grepl(paste0(torem, collapse = "|"), allvars)]
# remove variables that do not change (e.g. only zero in feature value)
alldata <- newdata[allvars]
alldata <- alldata[complete.cases(alldata),]
alldata <- Filter(function(x) sd(x) != 0, alldata)
allvars <- names(alldata)

#compute ALE effect
myeffect <- iml::FeatureEffects$new(predictor,
 feature = allvars, center.at = 0,  grid.size = 30,method = 'ale')

dfl <- myeffect$results
dfall <- list()
for (i in 1:length(dfl))
{
mydf <-dfl[[i]]
mydf$region  <-mm
names(mydf) <-c('type','y','x','feature','region')
mydf <- mydf[c('x','y','feature', 'region')]
dfall[[i]] <- mydf
}
dfalldata <- do.call(rbind, dfall)
aledf[[mm]] <- dfalldata
cat(mm)
}

aledfall <- do.call(rbind, aledf)
aledfall$model <- modelnames[[j]]
aleall[[j]] <- aledfall
}

myale <- do.call(rbind,aleall)
save(myale,file='results/xgb/myale.Rdata')

#PLOT

#add region names (A-M)
regionnames <- data.frame(regnames=as.character(c("A", "B","C","K","L","J", "I","D","H","E","G","F","M")),
                         region=as.integer(1:13))
#choice of colors for plots
twocolors <-  c(palettetown::pokepal('venusaur')[c(3)],"#A1D99B")
regcol <- c('XGB' = twocolors[1], 'XGBfatal' = twocolors[2])

myale$regnames <- regionnames$regnames[match(myale$region, regionnames$region)]
                                   
#rename structural variable for publication
myale$feature[myale$feature == 'x'] <- 'longitude (cell)'
myale$feature[myale$feature == 'y'] <- 'latitude (cell)'
myale$feature[myale$feature == 'altitude'] <- 'altitude (cell)'
myale$feature[myale$feature == 'border'] <- 'distance to nearest national border (cell)'
myale$feature[myale$feature == 'capital'] <- 'distance to nearest capital city (cell)'
myale$feature[myale$feature == 'conf'] <- 'number of conflict events (cell)'
myale$feature[myale$feature == 'popdens'] <- 'population density (cell)'
myale$feature[myale$feature == 'lum'] <- 'satellite night lights (cell)'
myale$feature[myale$feature == 'access'] <- 'travel time to the nearest large city (cell)'
myale$feature[myale$feature == 'gem'] <- 'gems or diamonds deposits (cell)'
myale$feature[myale$feature == 'imr'] <- 'infant mortality rate (cell)'
myale$feature[myale$feature == 'mountain'] <- 'mountain coverage (cell)'
myale$feature[myale$feature == 'oil'] <- 'petroleum deposits (cell)'
myale$feature[myale$feature == 'drug'] <- 'large-scale drug cultivation (cell)'
myale$feature[myale$feature == 'excethn'] <- 'excluded ethnic groups (cell)'
myale$feature[myale$feature == 'gcp'] <- 'gross cell product (cell)'
myale$feature[myale$feature == 'grip'] <- 'road network density (cell)'
myale$feature[myale$feature == 'gold'] <- 'gold deposits (cell)'
myale$feature[myale$feature == 'oil'] <- 'petroleum deposits (cell)'
myale$feature[myale$feature == 'LDI'] <- 'liberal democracy index (country)'
myale$feature[myale$feature == 'pcgdp'] <- 'per capita GDP (country)'
#rename procedural variable for publication
myale$feature[myale$feature == 'terrorl1'] <- 'terrorism in week lag 1 (cell)'
myale$feature[myale$feature == 'terrorl2'] <- 'terrorism in week lag 2 (cell)'
myale$feature[myale$feature == 'terrorl3'] <- 'terrorism in week lag 3 (cell)'
myale$feature[myale$feature == 'terrorl4'] <- 'terrorism in week lag 4 (cell)'
myale$feature[myale$feature == 'terrorl_month'] <- 'terrorism in month lag 1 (cell)'
myale$feature[myale$feature == 'total_terror_l1'] <- 'terrorism in week lag 1 (country)'
myale$feature[myale$feature == 'total_terror_l2'] <- 'terrorism in week lag 2 (country)'
myale$feature[myale$feature == 'total_terror_l3'] <- 'terrorism in week lag 3 (country)'
myale$feature[myale$feature == 'total_terror_l4'] <- 'terrorism in week lag 4 (country)'
myale$feature[myale$feature == 'total_terror_l52'] <- 'terrorism in year lag 1 (country)'
myale$feature[myale$feature == 'total_terror_month'] <- 'terrorism in month lag 1 (country)'
myale$feature[myale$feature == 'total_terror_change'] <- 'change of terrorism in the past (country)'
myale$feature[myale$feature == 'time_since_terror'] <- 'time since previous terrorist event (cell)'
myale$feature[myale$feature == 'terror_100km_l1'] <- 'terrorism in week lag 1 (within 100km radius)'

#change characters as factor for plots
myale$regnames <- as.factor(myale$regnames)
myale$feature <- as.factor(myale$feature)

#themes
sizeline <- 1 # size of line 
mytheme <-    
  ggplot2::theme(plot.margin = unit(c(0.1, 0.05, 0, 0.05), "cm"),
                 #margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
                 axis.line = element_line(color = 'black',size=0.1),
                 axis.text = element_text( size = 14),
                 axis.title=element_text(size=18),
                 legend.title = element_text(size=18),
                 legend.position = c("bottom"),
legend.background = element_rect(fill = "white", colour = NA),
strip.text = element_text(size = 24,face="bold"),
line = element_line(size=sizeline),
aspect.ratio=0.8)

#plot in a loop all features
fnames <- unique(myale$feature)

for (l in 1:length(fnames)){
fname <-fnames[l]
aledf <- myale[myale$feature==fname,]
  aledf <- rbind(aledf,aledf)
#plots
xname <-as.character(droplevels(fname))
ale1 <- ggplot(aledf, aes(x = x, y = y))
aleplt <- ale1 +
  geom_smooth(aes(color = model),span = 0.8,size = sizeline) + 
  labs(x = xname,y="ALE")+ scale_x_continuous(n.breaks = 4)+scale_y_continuous(n.breaks = 4)+
   guides(color=guide_legend(nrow=1,byrow=TRUE,title = "Models", title.position = "left")) + 
  ggplot2::scale_colour_manual(values = regcol) + 
  facet_wrap(~ regnames, ncol = 4, nrow = 4, scales='free',drop=TRUE)+
  ggthemes::theme_tufte(base_size = 14)+ mytheme 
print(aleplt)
ggsave(paste0("results/figs/ALE/ale",fname,".pdf"),aleplt,dpi = 450,width = 13.2,height = 11)
}

