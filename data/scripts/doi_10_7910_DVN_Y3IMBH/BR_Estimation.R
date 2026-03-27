simDatFiles <- list.files("BR/simData", pattern=".*\\.RData", full.names=TRUE)
simDatFiles <- mixedsort(simDatFiles)


for (f in simDatFiles) {
  load(f)	# the simulation data frame always is called "res"
  Est.Results <- NULL
  print(paste0(Sys.time(), "  ",f, " - ",length(simDatFiles)))
  for(i in unique(res$RepID)){
    #print(paste0(Sys.time(), "  ",f, " SimID=", i , "/", length(unique(res$RepID))))
    DT <- res[res$RepID==i, ]
    SimulationDescription <- DT[1, c(1:7)]
    AKdata <- as.data.frame(cbind.data.frame(id=DT$StdID, effect=DT$EstEffect, se=DT$SE, constant=1))
    res0 <- rbind(
      RMA.est(d=DT$EstEffect, v=(DT$SE)^2, long=TRUE),
      PETPEESE.est(DT$EstEffect, (DT$SE)^2, PP.test = "one-sided", long=TRUE, runRMA=FALSE),
      pc_skew(t=(DT$EstEffect/DT$SE), df=DT$obs*2 - 2, long=TRUE),
      pcurveEst(t=(DT$EstEffect/DT$SE), df=DT$obs*2 - 2, progress=FALSE, long=TRUE, CI=FALSE),
      puniformEst(t.value=(DT$EstEffect/DT$SE), n1=DT$obs, n2=DT$obs, skipBarelySignificant=TRUE),
      threePSM.est(d=DT$EstEffect, v=(DT$SE)^2, min.pvalues=0, long=TRUE),
      fourPSM.est(d=DT$EstEffect, v=(DT$SE)^2, min.pvalues=0, long=TRUE, fallback=FALSE),
      WAAP.est(d=DT$EstEffect, v=(DT$SE)^2, long=TRUE),
      AK1.est(AKdata),
      AK2.est(AKdata),
      EK.est(d=DT$EstEffect, v=(DT$SE)^2)    
    )
    p<-(1 - pt(q = abs(DT$EstEffect/DT$SE), df = DT$obs*2-2))*2
    delta.included.M <- mean(DT$EstEffect[p < .05 & p >= 0])
    if (is.nan(delta.included.M)) delta.included.M <- NA
    
    res0 <- rbind(res0,
                  data.frame(method="pcurve", term="delta.included", variable="mean", value = delta.included.M)	
    )
    res0 <- cbind.data.frame( SimulationDescription,res0)
    
    Est.Results <- rbind(Est.Results, res0)
  }
  save(Est.Results, file=paste0("BR/analysisResults/analysis_", basename(f)), compress="gzip")
}
