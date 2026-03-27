# LeafPhenologyGEDI.R
# Calculate differences in AGBD predictions from leaf-on vs. leaf-off data from L4A models

##### Read data #####

  # Leaf on and leaf off waveform data
    theData_On <- read.csv("LeafOnSimulatedMetricsAndBiomass.csv")
    theData_Off <- read.csv("LeafOffSimulatedMetricsAndBiomass.csv")
    
  # Candidate AGBD models
    modelSummary <- read.csv("CandidateModels.csv")
    
  # make a data frame of RH values to use; these were previously named "rhReal_#" in the simulator output
    metrics <- c("RH_10","RH_20","RH_30","RH_40","RH_50","RH_60","RH_70","RH_80","RH_90","RH_98")
    # Note: these RH values, and their interaction terms, already have a constant value of 100 added
    # to them to be consistent with the GEDI L4A models
    constantVal <- 100
    
##### Predict AGBD from model info #####
  
  for(i in 1:nrow(modelSummary)){
    
    int <- modelSummary$Intercept[i]
    coef1 <- modelSummary$a[i]
    coef2 <- modelSummary$b[i]
    coef3 <- modelSummary$c[i]
    coef4 <- modelSummary$d[i]
    if(modelSummary$TransfVar[i] %in% c("original")){
      var1 <- modelSummary$Var1[i]
      var2 <- modelSummary$Var2[i]
      var3 <- modelSummary$Var3[i]
      var4 <- modelSummary$Var4[i]
    }
    if(modelSummary$TransfVar[i] %in% c("log","sqrt")){
      var1 <- strsplit(modelSummary$Var1[i],'[()]')[[1]][2]
      var2 <- strsplit(modelSummary$Var2[i],'[()]')[[1]][2]
      var3 <- strsplit(modelSummary$Var3[i],'[()]')[[1]][2]
      var4 <- strsplit(modelSummary$Var4[i],'[()]')[[1]][2]
    }
    
    # if 1 predictor variables
      if(!is.na(var1) & is.na(var2)){
      # if untransformed variables  
        if(modelSummary$TransfVar[i]=="original"){
          newValsOn <- int + coef1*(theData_On[,var1])
          newValsOff <- int + coef1*(theData_Off[,var1])
        }
      # if log transformed variables
        if(modelSummary$TransfVar[i]=="log"){
          newValsOn <- int + coef1*log(theData_On[,var1])
          newValsOff <- int + coef1*log(theData_Off[,var1])
        }
      # if square root transformed variables
        if(modelSummary$TransfVar[i]=="sqrt"){
          newValsOn <- int + coef1*sqrt(theData_On[,var1])
          newValsOff <- int + coef1*sqrt(theData_Off[,var1])
        }  
      }
    # if 2 predictor variables
      if(!is.na(var2) & is.na(var3)){
      # if untransformed variables
        if(modelSummary$TransfVar[i]=="original"){
          newValsOn <- int + coef1*(theData_On[,var1]) + coef2*(theData_On[,var2])
          newValsOff <- int + coef1*(theData_Off[,var1]) + coef2*(theData_Off[,var2])
        }
      # if log transformed variables
        if(modelSummary$TransfVar[i]=="log"){
          newValsOn <- int + coef1*log(theData_On[,var1]) + coef2*log(theData_On[,var2])
          newValsOff <- int + coef1*log(theData_Off[,var1]) + coef2*log(theData_Off[,var2])
        }
      # if square root transformed variables
        if(modelSummary$TransfVar[i]=="sqrt"){
          newValsOn <- int + coef1*sqrt(theData_On[,var1]) + coef2*sqrt(theData_On[,var2])
          newValsOff <- int + coef1*sqrt(theData_Off[,var1]) + coef2*sqrt(theData_Off[,var2])
        } 
      }
    # if 3 predictor variables
      if(!is.na(var3) & is.na(var4)){
      # if untransformed variables
        if(modelSummary$TransfVar[i]=="original"){
          newValsOn <- int + coef1*(theData_On[,var1]) + coef2*(theData_On[,var2]) + coef3*(theData_On[,var3])
          newValsOff <- int + coef1*(theData_Off[,var1]) + coef2*(theData_Off[,var2]) + coef3*(theData_Off[,var3])
        }
      # if log transformed variables
        if(modelSummary$TransfVar[i]=="log"){
          newValsOn <- int + coef1*log(theData_On[,var1]) + coef2*log(theData_On[,var2]) + coef3*log(theData_On[,var3])
          newValsOff <- int + coef1*log(theData_Off[,var1]) + coef2*log(theData_Off[,var2]) + coef3*log(theData_Off[,var3])
        }
      # if square root transformed variables
        if(modelSummary$TransfVar[i]=="sqrt"){
          newValsOn <- int + coef1*sqrt(theData_On[,var1]) + coef2*sqrt(theData_On[,var2]) + coef3*sqrt(theData_On[,var3])
          newValsOff <- int + coef1*sqrt(theData_Off[,var1]) + coef2*sqrt(theData_Off[,var2]) + coef3*sqrt(theData_Off[,var3])
        } 
      }
    # if 4 predictor variables
      if(!is.na(var4)){
      # if untransformed variables
        if(modelSummary$TransfVar[i]=="original"){
          newValsOn <- int + coef1*(theData_On[,var1]) + coef2*(theData_On[,var2]) + coef3*(theData_On[,var3]) + coef4*(theData_On[,var4])
          newValsOff <- int + coef1*(theData_Off[,var1]) + coef2*(theData_Off[,var2]) + coef3*(theData_Off[,var3]) + coef4*(theData_Off[,var4])
        }
      # if log transformed variables
        if(modelSummary$TransfVar[i]=="log"){
          newValsOn <- int + coef1*log(theData_On[,var1]) + coef2*log(theData_On[,var2]) + coef3*log(theData_On[,var3]) + coef4*log(theData_On[,var4])
          newValsOff <- int + coef1*log(theData_Off[,var1]) + coef2*log(theData_Off[,var2]) + coef3*log(theData_Off[,var3]) + coef4*log(theData_Off[,var4])
        }
      # if square root transformed variables
        if(modelSummary$TransfVar[i]=="sqrt"){
          newValsOn <- int + coef1*sqrt(theData_On[,var1]) + coef2*sqrt(theData_On[,var2]) + coef3*sqrt(theData_On[,var3]) + coef4*sqrt(theData_On[,var4])
          newValsOff <- int + coef1*sqrt(theData_Off[,var1]) + coef2*sqrt(theData_Off[,var2]) + coef3*sqrt(theData_Off[,var3]) + coef4*sqrt(theData_Off[,var4])
        } 
      }   
    
      # If AGBD is log-transformed
        if(modelSummary$TransfAGB[i]=="log"){
          newValsOn <- modelSummary$BackTransf[i]*exp(newValsOn)
          newValsOff <- modelSummary$BackTransf[i]*exp(newValsOff)
        }
      
      # If AGBD is square root-transformed
        if(modelSummary$TransfAGB[i]=="sqrt"){
          newValsOn <- modelSummary$BackTransf[i]*(newValsOn^2)
          newValsOff <- modelSummary$BackTransf[i]*(newValsOff^2)
        }
      
      theData_On$newVals <- newValsOn
      theData_Off$newVals <- newValsOff

      names(theData_On)[names(theData_On)=="newVals"] <- paste0("pred_",modelSummary$ModelID[i])
      names(theData_Off)[names(theData_Off)=="newVals"] <- paste0("pred_",modelSummary$ModelID[i])
  }
  
##### Calculate overall RMSE and bias for each model #####
  
  # Get vector of observed biomass
  obsAGBD <- theData_On$AGBD_Mgha # this is identical for leaf on and leaf off data
  meanAGBD <- mean(obsAGBD)
  
for(i in 1:nrow(modelSummary)){
  
  # Get name of column for predicted data
  col_i <- paste0("pred_",modelSummary$ModelID[i])
  predAGBD_On <- theData_On[,col_i]
  predAGBD_Off <- theData_Off[,col_i]
  
  # Calculate RMSE (as % of total average biomass for Zofin)
  modelSummary$RMSE_On[i] <- (sqrt(mean((predAGBD_On-obsAGBD)^2))/meanAGBD)*100
  modelSummary$RMSE_Off[i] <- (sqrt(mean((predAGBD_Off-obsAGBD)^2))/meanAGBD)*100
  
  # Calculate site-wide
  modelSummary$Bias_On[i] <- 100*(sum(predAGBD_On)-sum(obsAGBD))/sum(obsAGBD)
  modelSummary$Bias_Off[i] <- 100*(sum(predAGBD_Off)-sum(obsAGBD))/sum(obsAGBD)
  
  # Calculate bias as Jim defined in methods
  modelSummary$Bias_Phenology[i] <- 100*(sum(predAGBD_Off)-sum(predAGBD_On))/sum(predAGBD_On)
  
}
  
  # Calculate change in RMSE
  modelSummary$RMSE_delta <- modelSummary$RMSE_Off - modelSummary$RMSE_On
  
##### Table 1. Basic results stats #####
  # Correlation between RH metrics and plot AGBD
  # NOTE: these reults were ultimately not included in the revised manuscript
  metricAGBD = data.frame(RH=metrics,
                          rOn=NA,
                          pOn=NA,
                          rOff=NA,
                          pOff=NA)
  for(i in 1:nrow(metricAGBD)){
    metricAGBD$rOn[i] <- cor.test(x=log(theData_On[,as.character(metricAGBD$RH[i])]),
                                  y=log(theData_On[,"AGBD_Mgha"]))$estimate
    metricAGBD$pOn[i] <- cor.test(x=log(theData_On[,as.character(metricAGBD$RH[i])]),
                                  y=log(theData_On[,"AGBD_Mgha"]))$p.value
    metricAGBD$rOff[i] <- cor.test(x=log(theData_Off[,as.character(metricAGBD$RH[i])]),
                                   y=log(theData_Off[,"AGBD_Mgha"]))$estimate
    metricAGBD$pOff[i] <- cor.test(x=log(theData_Off[,as.character(metricAGBD$RH[i])]),
                                   y=log(theData_Off[,"AGBD_Mgha"]))$p.value
  }
  write.csv(metricAGBD,"Table1_CorTerms.csv",row.names=F)
  
  # Mean and SD change in cover
    round(mean(theData_On$ALScover),2)
    round(sd(theData_On$ALScover),2)
    round(mean(theData_Off$ALScover),2)
    round(sd(theData_Off$ALScover),2)
    round(mean(theData_On$ALScover - theData_Off$ALScover),2)
    round(sd(theData_On$ALScover - theData_Off$ALScover),2)

  # Mean change in ALS cover
    round(mean(theData_On$ALScover - theData_Off$ALScover),2)
    round(sd(theData_On$ALScover - theData_Off$ALScover),2)
  
##### Tables 2 and S1. Top model summary #####  
  modelSummary <- modelSummary[order(modelSummary$Restriction,-modelSummary$Bias_Phenology),]
  # Create a text string with the model form
  modelSummary$fittedFormula <- NA
  
  # Create text versions of coefficients in scientific notation
  modelSummary$intText <- formatC(modelSummary$Intercept, format = "e", digits = 2)
  modelSummary$aText <- formatC(modelSummary$a, format = "e", digits = 2)
  modelSummary$bText <- formatC(modelSummary$b, format = "e", digits = 2)
  modelSummary$cText <- formatC(modelSummary$c, format = "e", digits = 2)
  modelSummary$dText <- formatC(modelSummary$d, format = "e", digits = 2)
  
  for(i in 1:nrow(modelSummary)){
    termAGBD <- ""
    term1 <- ""
    term2 <- ""
    term3 <- ""
    term4 <- ""
    
    if(modelSummary$Transf2[i]=="log"){termAGBD <- "log(AGBD)"}
    if(modelSummary$Transf2[i]=="sqrt"){termAGBD <- "sqrt(AGBD)"}
    
    term1 <- paste(modelSummary$intText[i] ,"+", modelSummary$aText[i],"?", modelSummary$Var1[i])
    if(!is.na(modelSummary$Var2[i])){
      term2 <- paste("+",modelSummary$bText[i],"?", modelSummary$Var2[i])
    }
    if(!is.na(modelSummary$Var3[i])){
      term3 <- paste("+",modelSummary$cText[i],"?", modelSummary$Var3[i])
    }
    if(!is.na(modelSummary$Var4[i])){
      term4 <- paste("+",modelSummary$dText[i],"?", modelSummary$Var4[i])
    }
    
    modelSummary$fittedFormula[i] <- paste(termAGBD,"=", term1,term2,term3,term4)
    
  }
  
  # Find top 5 models in each feature set:
  modelsSet1 <- head(modelSummary[modelSummary$Restriction=="1_None",],5)
  modelsSet2 <- head(modelSummary[modelSummary$Restriction=="2_noLow",],5)
  modelsSet3 <- head(modelSummary[modelSummary$Restriction=="3_with98",],5)
  modelsSet4 <- head(modelSummary[modelSummary$Restriction=="4_noLow_with98",],5)
  
  # Save main text table of just top model sets
  write.csv(rbind(modelsSet1[,c("fittedFormula","Bias_Phenology","Intercept","a","b","c","d")],
                  modelsSet2[,c("fittedFormula","Bias_Phenology","Intercept","a","b","c","d")],
                  modelsSet3[,c("fittedFormula","Bias_Phenology","Intercept","a","b","c","d")],
                  modelsSet4[,c("fittedFormula","Bias_Phenology","Intercept","a","b","c","d")]),
            row.names = F,
                  "Table2_BestModels.csv")
  
  write.csv(modelSummary[,c("fittedFormula","Bias_Phenology","Intercept","a","b","c","d")],
            row.names = F,
            "TableS1_AllModels.csv")
  
##### Table 3. Post-hoc Tukey table ####
  
  scenarioANOVA <- aov(Bias_Phenology~Restriction,
                       data=modelSummary)
  summary(scenarioANOVA)
  summary(lm(Bias_Phenology~Restriction,
             data=modelSummary))
  TukeyHSD(scenarioANOVA, conf.level = 0.95)
  
##### DEFINE FIGURE INFO ####
  par(las=1)

  colOn = "#1f78b4"
  colOff = "#fb9a99"
  
##### FIGURE 2: change in cover #####
  
    coverDens_on <- density(theData_On$ALScover*100)
    coverDens_off <- density(theData_Off$ALScover*100)
    coverDens_delta <- density(theData_On$ALScover*100 - theData_Off$ALScover*100)
    
tiff("Figure2.tiff", height=3, width=7, units = "in",res=500)
  
  par(mfrow=c(1,2), mar=c(3,2,1,1),oma=c(1,1,0,0), las=1)
  
  # A: Cover
  plot(x = coverDens_on$x, y = coverDens_on$y,
       col = colOn,
       lwd=3,
       type="l",
       xlim=c(0,100),
       ylim=range(c(coverDens_on$y,coverDens_off$y,coverDens_delta$y)),
       xlab=NA,ylab=NA)
  mtext("Density",las=0,side=2,outer=T)
  mtext("Canopy cover (%)",side=1,outer=F, line=2)

  text("a.",x=0,y=10)
  lines(x = coverDens_off$x, y = coverDens_off$y,
        col = colOff,
        lwd=3)
  legend(c("Leaf-on","Leaf-off"),
         lwd=3,
         col = c(colOn,colOff),
         lty=c(1,1),
         bty="n",
         x=0,y=9)
  
  # B: cover increase
  plot(x = coverDens_delta$x, y = coverDens_delta$y,
       ylim=range(c(coverDens_on$y,coverDens_off$y,coverDens_delta$y)),
       lwd=3,
       type="l",
       xlab=NA,ylab=NA)
    mtext("Leaf-on canopy cover increase (%)",side=1,outer=F, line=2)

      abline(v=0,col=adjustcolor("grey",0.5),lwd=2, lty=2)
      text("b.",x=-0.03,y=7)
      
dev.off()  

##### FIGURE 3: leaf-on vs leaf-on RH metric height #####
  
  axisLim <- range(c(range(theData_On[,metrics]),range(theData_Off[,metrics])))-constantVal
  RHnames <- c("RH10","RH20","RH30","RH40","RH50","RH60","RH70","RH80","RH90","RH98")
  
  
tiff(file="Figure3.tiff",width=7,height=3, units="in",res=500)

  par(mfrow=c(2,5), mar=c(1,1,0,0),oma=c(4,4,1,1), las=1)
  for(i in 1:length(RHnames)){
    plot(x=theData_On[,metrics[i]]-constantVal, y=theData_Off[,metrics[i]]-constantVal,
         xlim=axisLim,ylim=axisLim,
         pch=20,
         bty="l",xaxt="n",yaxt="n",
         col=adjustcolor("black",alpha.f = 0.2),
         cex = 1.5)
    text(RHnames[i],x=10,y=45, cex=1.5)
    abline(a=0,b=1, col = "black", lwd=1.5, lty=2)
    
    if(i %in% c(6:10)){
      axis(side=1, at=seq(0,40,10), cex.axis=1.5)
    }
    
    if(!(i %in% c(6:10))){
      axis(side=1, at=seq(0,40,10), labels=F)
    }
    
    if(i %in% c(1,6)){
      axis(side=2, at=seq(0,40,10), cex.axis=1.5)
    }
    
    if(!(i %in% c(1,6))){
      axis(side=2, at=seq(0,40,10), labels=F)
    }
    
    mtext("Leaf-on height (m)", side=1, outer=T, cex=1.2,line=2)
    mtext("Leaf-off height (m)", side=2, outer=T, cex=1.2,line=2, las=0) 
    
  }
dev.off()
  
##### FIGURE 4: grouped barplots of prediction difference values #####

# Create factor of restiction names so that they are in correct order for plotting
modelSummary$Restriction <- factor(modelSummary$Restriction)

tiff("Figure4.tiff", width=6, height = 4, units="in",res=500)
  par(mfrow=c(1,1), mar=c(4,4,1,1), oma=c(0,0,0,0), las=1)
  boxplot(Bias_Phenology~Restriction, data=modelSummary,
          xaxt="n",
          xlab=NA,
          ylab= NA,
          outcol = adjustcolor("black",0.99),
          pch=19)
  mtext("Model scenario", side=1, line=2)
  mtext("Systematic prediction difference (%)", side=2, line=3, las=0)
  
  text(x=1, y=-20.5,"1. All metrics", xpd=T, cex=0.75)
  text(x=1, y=-21.5,"permitted", xpd=T, cex=0.75)
  
  text(x=2, y=-20.5,"2. No RH < 50", xpd=T, cex=0.75)

  text(x=3, y=-20.5,"3. Includes RH98", xpd=T, cex=0.75)
  
  text(x=4, y=-20.5,"4. Includes RH98", xpd=T, cex=0.75)
  text(x=4, y=-21.5,"+ no RH < 50", xpd=T, cex=0.75)
  
  abline(h=0,col = "black", lwd=1.5, lty=2)
dev.off()

##### FIGURE 5: RMSE vs. systematic prediction difference #####
tiff("Figure5.tiff", width=4, height = 4,units="in",res=500)
  par(mfrow=c(1,1), las=1)
  plot(RMSE_delta~Bias_Phenology, data = modelSummary[!duplicated(modelSummary$Bias_Phenology),],
       pch=19,
       col = adjustcolor("black",0.5),
       xlab=NA,
       ylab = NA)
  mtext("Systematic prediction difference (%)", side = 1, line=2)
  mtext(expression(paste(Delta," RMSE (%)")), side=2, line=2, las=0)
  
 dev.off()
 
  cor.test(x = modelSummary[!duplicated(modelSummary$Bias_Phenology),"RMSE_delta"],
           y = modelSummary[!duplicated(modelSummary$Bias_Phenology),"Bias_Phenology"])

##### FIGURE S1: AGBD values #####
tiff("FigureS1.tiff", width=10, height = 8,units="in",res=500)  
  par(las=1)
  plot(x=0,y=0, type="n",
       xlab = "Aboveground biomass density (AGBD, Mg/ha)",
       ylab = "Distribution density",
       xlim=range(theData_On[,802:881],theData_Off[,802:881]),
       ylim=c(0,0.008))
  
  text("All candidate models", x=400, y=0.0081,
       adj=0)
  legend(c("Leaf-on","Leaf-off"),
         lwd=3,
         col = c(colOn,colOff),
         lty=c(1,1),
         bty="n",
         x=400,y=0.008)
  
  text("Model with lowest systematic prediction difference", x=400, y=0.00651,
       adj=0)
  legend(c("Leaf-on","Leaf-off"),
         lwd=3,
         col = "blue",
         lty=c(1,2),
         bty="n",
         x=400,y=0.0065)
  
  text("Model used in GEDI04_A releases 1,2", x=400, y=0.0051,
       adj=0)
  legend(c("Leaf-on","Leaf-off"),
         lwd=3,
         col = "red",
         lty=c(1,2),
         bty="n",
         x=400,y=0.005)
  
  legend(c("Plot-estimated mean AGBD"),
         lwd=3,
         col = "black",
         lty=c(1),
         bty="n",
         x=400,y=0.0038)
  

  
  #get ID of the model that had the lowest systematic difference
  lowestSysDiff <- modelSummary$ModelID[1]
  
  #get ID of the model used to predict AGBD in release 1 and release 2 of the GEDI04_A data product 
  releaseModel <- modelsSet4$ModelID[3]
  
  for(i in 1:80){
    
    dens_On <- density(theData_On[,paste0("pred_",i)])
    dens_Off <- density(theData_Off[,paste0("pred_",i)])
    mean_On <- mean(theData_On[,paste0("pred_",i)])
    mean_Off <- mean(theData_Off[,paste0("pred_",i)])
    
    lines(dens_On, col = adjustcolor(colOn,0.2),lwd=2)
    lines(dens_Off, col = adjustcolor(colOff,0.2),lwd=2)
    abline(v=mean_On, col = adjustcolor(colOn,0.1),lwd=1)
    abline(v=mean_Off, col = adjustcolor(colOff,0.1),lwd=1)
  }
  
  # plot model with lowest systematic difference in blue
    dens_On <- density(theData_On[,paste0("pred_",lowestSysDiff)])
    dens_Off <- density(theData_Off[,paste0("pred_",lowestSysDiff)])
    mean_On <- mean(theData_On[,paste0("pred_",lowestSysDiff)])
    mean_Off <- mean(theData_Off[,paste0("pred_",lowestSysDiff)])
    
    lines(dens_On, col = adjustcolor("blue",0.8),lwd=3)
    lines(dens_Off, col = adjustcolor("blue",0.8),lwd=3, lty=2)
    abline(v=mean_On, col = adjustcolor("blue",0.8),lwd=3)
    abline(v=mean_Off, col = adjustcolor("blue",0.8),lwd=3, lty=2)
    
  # plot model used to predict AGBD in release 1 and release 2 of the GEDI04_A data product 
    dens_On <- density(theData_On[,paste0("pred_",releaseModel)])
    dens_Off <- density(theData_Off[,paste0("pred_",releaseModel)])
    mean_On <- mean(theData_On[,paste0("pred_",releaseModel)])
    mean_Off <- mean(theData_Off[,paste0("pred_",releaseModel)])
    
    lines(dens_On, col = adjustcolor("red",0.8),lwd=3)
    lines(dens_Off, col = adjustcolor("red",0.8),lwd=3, lty=2)
    abline(v=mean_On, col = adjustcolor("red",0.8),lwd=3)
    abline(v=mean_Off, col = adjustcolor("red",0.8),lwd=3, lty=2)
  
  abline(v = meanAGBD, lwd=3)
  
 dev.off()

  