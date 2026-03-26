## =====================
## Importing Estimators
## =====================
simDatFiles <- list.files("Carter/simData", pattern=".*\\.RData", full.names=TRUE)
simDatFiles <- mixedsort(simDatFiles)

for (f in simDatFiles) {
  load(f)	# the simulation data frame always is called "res"
  Est.Results <- NULL
  res$repID <- res$replication
  cnt <- 1
  print(paste0(Sys.time(), "  ",f, " SimID=", cnt , "/", length(unique(res$repID))))
  for(i in unique(res$repID)){
    cnt<-cnt+1
    DT <- res[res$repID==i, ]
    SimulationDescription <- DT[1, c(3:8)]
    AKdata <- as.data.frame(cbind.data.frame(id=c(1:nrow(DT)), effect=DT$d, se=DT$se, constant=1))
    res0 <- rbind(
      RMA.est(d=DT$d, v=(DT$se)^2, long=TRUE),
      PETPEESE.est(DT$d, (DT$se)^2, PP.test = "one-sided", long=TRUE, runRMA=FALSE),
      pc_skew(t=(DT$d/DT$se), df=DT$N - 2, long=TRUE),
      pcurveEst(t=(DT$d/DT$se), df=DT$N - 2, progress=FALSE, long=TRUE, CI=FALSE),
      puniformEst(t.value=(DT$d/DT$se), n1=DT$n1, n2=DT$n2, skipBarelySignificant=TRUE),
      threePSM.est(d=DT$d, v=(DT$se)^2, min.pvalues=0, long=TRUE),
      fourPSM.est(d=DT$d, v=(DT$se)^2, min.pvalues=0, long=TRUE, fallback=FALSE),
      WAAP.est(d=DT$d, v=(DT$se)^2, long=TRUE),
      AK1.est(AKdata),
      AK2.est(AKdata),
      EK.est(d=DT$d, v=(DT$se)^2)    
    )
    p<-(1 - pt(q = abs(DT$d/DT$se), df = DT$N*2-2))*2
    delta.included.M <- mean(DT$EstEffect[p < .05 & p >= 0])
    if (is.nan(delta.included.M)) delta.included.M <- NA
    
    res0 <- rbind(res0,
                  data.frame(method="pcurve", term="delta.included", variable="mean", value = delta.included.M)	
    )
    res0 <- cbind.data.frame( SimulationDescription,res0)
    
    Est.Results <- rbind(Est.Results, res0)
  }
  save(Est.Results, file=paste0("Carter/analysisResults/analysis_", basename(f)), compress="gzip")
}



