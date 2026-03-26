# CFA_graphic.R

extra.width <- 1
if(intercepts | residuals){extra.width <- 1.25}
if(!is.na(filepng))
{png(filepng, width=600*length(main)*extra.width, height=600)}
o.par <- par(no.readonly=TRUE)
m <- matrix(data=1:(2*length(main)),
            nrow=2,ncol=length(main),byrow=TRUE)
layout(m,heights=0.15,1)
par(mai=c(0,0,0,0))
for (i in 1:length(main))
{
  plot(NA,xlim=c(0,10),ylim=c(0,1),
       main="",xlab="",ylab="",axes=FALSE)
  text(5,1,main[i],cex=1.5,pos=1)
}
grf <- semPlot::semPaths(model.estim,
                         bifactor=bifactor,
                         ask = FALSE,
                         style="lisrel",
                         what=what,
                         whatLabels=what,
                         layout=layout,
                         rotation=2,
                         edge.color="black",
                         intercepts=intercepts,
                         residuals=residuals,
                         edge.label.cex=1.2,
                         curvePivot=TRUE,
                         intAtSide=FALSE,
                         fade=FALSE,
                         sizeLat=8, 
                         nCharNodes=5, 
                         sizeInt=3,
                         sizeMan=5,
                         label.cex=1.5,
                         thresholds=FALSE,
                         title=FALSE)
par(o.par)
if(!is.na(filepng))
{dev.off()}
