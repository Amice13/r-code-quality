load("classd-voting-data-nocontrols.Rdata")
load("churchat-voting-data-nocontrols.Rdata")

classd.voting.data.nocontrols <- subset(transform(
   classd.voting.data.nocontrols,
   Percentage = 100*succ/freq
   ),year>=1995)


classd.voting.meansByNationsEB <- with(classd.voting.data.nocontrols,
   aggregate(Percentage,
      by=list(
         eb=eb,
         nation=nation),
      mean
   ))

classd.voting.meansByNationsEB <- transform(
   classd.voting.meansByNationsEB,
   eb=as.numeric(levels(eb)[as.numeric(eb)])
   )

names(classd.voting.meansByNationsEB) <- gsub("x","mPercentage",
   names(classd.voting.meansByNationsEB))

classd.voting.data.nocontrols <- merge(
   classd.voting.data.nocontrols,
   classd.voting.meansByNationsEB
   )
   
classd.voting.data.nocontrols <- transform(
   classd.voting.data.nocontrols,
   diffPercentage = Percentage - mPercentage
   )   
   
classd.voting.meansByNations <- with(classd.voting.data.nocontrols,
   aggregate(Percentage,
      by=list(
         classd=classd,
         nation=nation),
      mean
   ))

   
classd.voting.stdevByNations <- with(classd.voting.data.nocontrols,
   aggregate(Percentage,
      by=list(
         classd=classd,
         nation=nation),
      sd
   ))

classd.voting.DstdevByNations <- with(classd.voting.data.nocontrols,
   aggregate(diffPercentage,
      by=list(
         classd=classd,
         nation=nation),
      sd
   ))

   
classd.voting.meansByNations <- transform(
   classd.voting.meansByNations,
      classd=factor(classd,levels=rev(unique(classd)),labels=rev(c(
         "Manual workers",
         "Intermediate",
         "Salariat",
         "Self-employed",
            "Farmers"
         ))),
      stdev=classd.voting.stdevByNations$x,
      Dstdev=classd.voting.DstdevByNations$x
   )   

churchat.voting.data.nocontrols <- subset(transform(
   churchat.voting.data.nocontrols,
   Percentage = 100*succ/freq
   ),year>=1990)


churchat.voting.meansByNationsEB <- with(churchat.voting.data.nocontrols,
   aggregate(Percentage,
      by=list(
         eb=eb,
         nation=nation),
      mean
   ))

churchat.voting.meansByNationsEB <- transform(
   churchat.voting.meansByNationsEB,
   eb=as.numeric(levels(eb)[as.numeric(eb)])
   )

names(churchat.voting.meansByNationsEB) <- gsub("x","mPercentage",
   names(churchat.voting.meansByNationsEB))

churchat.voting.data.nocontrols <- merge(
   churchat.voting.data.nocontrols,
   churchat.voting.meansByNationsEB
   )
   
churchat.voting.data.nocontrols <- transform(
   churchat.voting.data.nocontrols,
   diffPercentage = Percentage - mPercentage
   )   
   
churchat.voting.meansByNations <- with(churchat.voting.data.nocontrols,
   aggregate(Percentage,
      by=list(
         churchat=churchat,
         nation=nation),
      mean
   ))

   
churchat.voting.stdevByNations <- with(churchat.voting.data.nocontrols,
   aggregate(Percentage,
      by=list(
         churchat=churchat,
         nation=nation),
      sd
   ))

churchat.voting.DstdevByNations <- with(churchat.voting.data.nocontrols,
   aggregate(diffPercentage,
      by=list(
         churchat=churchat,
         nation=nation),
      sd
   ))

   
churchat.voting.meansByNations <- transform(
   churchat.voting.meansByNations,
      churchat=factor(churchat,levels=rev(unique(churchat)),labels=rev(c(
         "At least weekly",
         "Less than weekly",
         "Never"
         ))),
      stdev=churchat.voting.stdevByNations$x,
      Dstdev=churchat.voting.DstdevByNations$x
   )   
   
library(lattice)
trellis.par.set(theme=col.whitebg())
source("col-bw.R")

(classd.barchart <- barchart(classd~x|nation,data=classd.voting.meansByNations,
   as.table=TRUE,
   layout=c(7,1),
   xlab="Percentage support for labor parties",
   xlim=c(0,80),
   aspect=1,
   par.strip.text=list(cex=.8),
   panel=function(x,y,subscripts,...){
      panel.grid(v=-1,h=0)
      panel.barchart(x,y,...)
      stdev <- classd.voting.meansByNations$stdev
      x0 <- x - stdev
      x1 <- x + stdev
      y <- as.numeric(y)
      panel.text(x/2,y,round(x,0),cex=.8)
      }
   ))   
   
(churchat.barchart <- barchart(churchat~x|nation,data=churchat.voting.meansByNations,
   as.table=TRUE,
   layout=c(7,1),
   xlab="Percentage support for Christian parties",
   xlim=c(0,80),
   aspect=1,
   par.strip.text=list(cex=.8),
   panel=function(x,y,subscripts,...){
      panel.grid(v=-1,h=0)
      panel.barchart(x,y,...)
      stdev <- churchat.voting.meansByNations$stdev
      x0 <- x - stdev
      x1 <- x + stdev
      y <- as.numeric(y)
      panel.text(x/2,y,round(x,0),cex=.8)
      }
   ))   

   
trellis.device(postscript,file="barchart-classd-90s.eps",
    paper="special",
    horizontal=FALSE,
    width=12,
    height=3
    )
    print(classd.barchart)
dev.off()         

trellis.device(pdf,file="barchart-classd-90s.pdf",
    paper="special",
    width=12,
    height=3,
   color=FALSE
    )
    print(classd.barchart)
dev.off()         



trellis.device(postscript,file="barchart-churchat-90s.eps",
    paper="special",
    horizontal=FALSE,
    width=12,
    height=3,
    )
    print(churchat.barchart)
dev.off()         

trellis.device(pdf,file="barchart-churchat-90s.pdf",
    paper="special",
    width=12,
    height=3,
   color=FALSE
    )
    print(churchat.barchart)
dev.off()         

