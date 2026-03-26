#################################
# Functions for use in analysis and plotting
# Dalston Ward (2018)
# "Dynamic Effects of Electoral Laws"
# Journal of Elections, Public Opinion, and Parties
# DOI: 10.1080/17457289.2018.1537279
#################################

########################################  Datset creation functions ######################################## 

#####This function creates the five or ten year average periods data
avg.maker <- function(country,five.yearU,five.yearL,dataframe,country.var,year.var){
  
  
  sub <- dataframe[dataframe[,country.var]==country & (dataframe[,year.var] < five.yearU & dataframe[,year.var] >= five.yearL),]
  
  small <- as.data.frame(lapply(1:ncol(sub), function(x){
    
    if(colnames(sub)[x] == year.var){ return(five.yearL) }
    if(colnames(sub)[x]=="id"){ return(paste(country, five.yearU-1, five.yearL, sep="_") )}
    if(class(sub[,x])%in%c("numeric","integer") & colnames(sub)[x] != year.var ) { 
      
      if(all(is.na(sub[,x]))){return(NA)} else {return(mean(sub[,x], na.rm=T) )}
      
    }
    if(colnames(sub)[x]==country.var) { return(country) }
  } #close apply function
  ) #close sapply
  ) #close as.data.frame
  colnames(small) <- colnames(sub)
  small$nelections <- nrow(sub)
  return(small) 
  #close apply
} #close function

#####This function takes a data frame and uses avg.maker and a five years object to return a data.frame

avgData.maker <- function(data, five_years){
  avgData <- NULL
  for(i in unique(data$ctr_n)){  
    
    what <- mapply(avg.maker,five_years[1:(length(five_years)-1)],five_years[2:length(five_years)],MoreArgs=list(country=i,dataframe=data,country.var="ctr_n",year.var="yr"),SIMPLIFY=FALSE)
    out <- NULL
    for(i in 1: length(what)){
      out <- rbind(out,what[[i]])
    }
    avgData <- rbind(avgData,out)
  }
  
  #Quick fix for a variable that got turned into a factor somehow 
  avgData$ctr_n <- as.character(avgData$ctr_n)
  return(avgData)
}

######## Panel_Balance_test #######
#Figure out what countries don't make for a balanced panel given treatment country
Panel_Balance_test <- function(data, country){
  bad <- vector()
  for(i in unique(data$ctr_n)){
    if(length(data$yr[ data$ctr_n==i])!= sum( data$ctr_n == country) ){
      print(i)
      print(length(data$yr[data$ctr_n==i]))
      bad <- c(bad,i)
    }
  }
  return(bad)
}

######################################## Synth inference functions ########################################

##### This function gives the average difference between the synthetic unit and the treated unit in the post-treatment period
SynthMeanEffect <- function(dataprep, synth, begin_time, end_time){
  time <- row.names(dataprep$Y1plot) %in% as.character(begin_time : end_time)
  gaps <- dataprep$Y1plot - (dataprep$Y0plot %*% synth$solution.w)
  out <- mean(gaps[time])
  return(out)
  #  return(mean(dataprep$Y1plot[time] - (dataprep$Y0plot[time] %*% synth$solution.w[time])))
}

#### This function helps for making inferences
SynthRMSPE <- function(dataprep, synth, begin_time, end_time){
  time <- row.names(dataprep$Y1plot) %in% as.character(begin_time : end_time)
  gaps <- dataprep$Y1plot - (dataprep$Y0plot %*% synth$solution.w)
  gaps.sq <- gaps^2
  mean.gaps <- mean(gaps.sq[time])
  return(sqrt(mean.gaps))
}

#### this function calculates the ratio of error for pre/post period.  NOTE: the dataprep object MUST have time.plot arguement filled. 
SynthErrorRatio <- function(dataprep, synth, treatment_time){
  pre_period <- dataprep$tag$time.plot[ ! dataprep$tag$time.plot >= treatment_time ]
  pre_error <- SynthRMSPE(dataprep, synth, pre_period[1], pre_period[ length(pre_period) ])
  # print(pre_error)
  post_period <- dataprep$tag$time.plot[ dataprep$tag$time.plot >= treatment_time ]
  post_error <- SynthRMSPE(dataprep, synth, post_period[1], post_period[ length(post_period) ])
  # print(post_error)
  out <- post_error/pre_error
  if(!is.null(dataprep$excluded)){
    names(out) <- as.character(dataprep$excluded)
  } else {
    names(out) <-  as.character(dataprep$names.and.numbers$unit.names[ dataprep$names.and.numbers$unit.numbers == dataprep$tag$treatment.identifier])
  }
  class(out) <- "SynthErrorRatio"
  return(out)
}

#prints the error ratio in a nice way.  
print.SynthErrorRatio <- function(obj){
  cat(" ************ Treatment RMSPE Ratio ********** \n")
  cat( names(obj), ": ", obj, "\n", sep = "")
  return()
}


SynthSummary <- function(dataprep, synth, treatment_time){
  
  RMSPE <- SynthErrorRatio(dataprep, synth, treatment_time)
  pre_period <- dataprep$tag$time.plot[ ! dataprep$tag$time.plot >= treatment_time ]
  pre_error <- SynthRMSPE(dataprep, synth, pre_period[1], pre_period[ length(pre_period) ])
  
  post_period <- dataprep$tag$time.plot[ dataprep$tag$time.plot >= treatment_time ]
  post_error <- SynthRMSPE(dataprep, synth, post_period[1], post_period[ length(post_period) ])
  
  ATE <- SynthMeanEffect(dataprep, synth, treatment_time, max(dataprep$tag$time.plot))
  tabs <- synth.tab(synth.res = synth, dataprep.res = dataprep, round.digit = 2)
  
  cat(" ******** Covariate Values ********* \n")
  print(tabs$tab.pred)
  cat( "\n******** Covariate Weights ******** \n")
  print(tabs$tab.v)
  cat( "\n******** Country Weights ******** \n")
  print(tabs$tab.w)
  cat("\n")
  print(RMSPE)
  cat( "\n******** Pre-Treatment RMSPE ******** \n")
  print(pre_error)
  cat( "\n******** Post-Treatment RMSPE ******* \n")
  print(post_error)
  cat( "\n******* ATE *********\n")
  print(ATE)
}
########################### Multi Synth Functions ########################################


#This function creates the dataprep objects necessary for placebo tests 
SynthPlacebosPrep <- function(data, predictors, dependent, unit.variable, time.variable, control.units, time.units.prior, time.units.ssr, unit.names, time.units.plot, ...){
  
  lapply(control.units, function(case){
    dataprep(foo = data,
             predictors = predictors,
             dependent = dependent,
             unit.variable = unit.variable,
             time.variable = time.variable,
             treatment.identifier = case,
             controls.identifier = control.units[ !control.units == case],
             time.predictors.prior = time.units.prior,
             time.optimize.ssr = time.units.ssr,
             unit.names.variable = unit.names,
             time.plot = time.units.plot, ...)
  })
}

#creates a data prep object for all the possible "leave one out" synthetic runs. 
SynthLeaveOneOutPrep <- function(data, predictors, dependent, unit.variable, time.variable, treatment.identifier, control.units, time.units.prior, time.units.ssr, unit.names, time.units.plot, ...){
  
  lapply(control.units, function(case){
    loo_prep <- dataprep(foo = data,
                         predictors = predictors,
                         dependent = dependent,
                         unit.variable = unit.variable,
                         time.variable = time.variable,
                         treatment.identifier = treatment.identifier,
                         controls.identifier = control.units[ ! control.units == case ],
                         time.predictors.prior = time.units.prior,
                         time.optimize.ssr = time.units.ssr,
                         unit.names.variable = unit.names,
                         time.plot = time.units.plot, ...)
    loo_prep$excluded <- unique(data[,unit.names][data[,unit.variable] == case])
    return(loo_prep)
  })
}

#creates a data prep object for all the possible "leave one out" covariate synthetic runs. 
SynthLeaveOneOutCovPrep <- function(data, predictors, dependent, unit.variable, time.variable, treatment.identifier, control.units, time.units.prior, time.units.ssr, unit.names, time.units.plot, ...){
  
  lapply(predictors, function(covariate){
    loo_prep <- dataprep(foo = data,
                         predictors = predictors[predictors != covariate],
                         dependent = dependent,
                         unit.variable = unit.variable,
                         time.variable = time.variable,
                         treatment.identifier = treatment.identifier,
                         controls.identifier = control.units,
                         time.predictors.prior = time.units.prior,
                         time.optimize.ssr = time.units.ssr,
                         unit.names.variable = unit.names,
                         time.plot = time.units.plot, ...)
    loo_prep$excluded <- covariate
    return(loo_prep)
  })
}

MultiSynth <- function(MultiSynthPrep_obj, parallel = FALSE){
  llply(.data = MultiSynthPrep_obj, .fun = synth, .parallel = parallel)
}

MultiSynthErrorRatios <- function(MultiSynthPrep_obj, MultiSynth_obj, treatment_time){
  
  #Calculate the error ratios here
  out <- mapply(SynthErrorRatio, MultiSynthPrep_obj, MultiSynth_obj, MoreArgs = list(treatment_time = treatment_time), SIMPLIFY = TRUE)
  #so that I can print them in a pretty way
  class(out) = "MultiSynthErrorRatio"
  return(out)
}

print.MultiSynthErrorRatio <- function(obj){
  # to_print <- sort(obj)
  to_print <- obj
  cat(" ************ Placebo RMSPE ********** \n")
  sapply(to_print, function(value){
    cat( names(to_print)[to_print == value], ": ", value, "\n", sep = "")
  })
  return()
}

MultiSynthPreErrorRatios <- function(MultiSynthPrep_obj, MultiSynth_obj, begin_time, end_time){
  
  #Calculate the pre treatment error ratios here
  out <- mapply(SynthRMSPE, MultiSynthPrep_obj, MultiSynth_obj, MoreArgs = list(begin_time = begin_time, end_time = end_time), SIMPLIFY = TRUE)
  names(out) <- sapply(MultiSynthPrep_obj, function(dataprep){ return(as.character(dataprep$names.and.numbers$unit.names[ dataprep$names.and.numbers$unit.numbers == dataprep$tag$treatment.identifier]))})
  
  #so that I can print them in a pretty way
  class(out) = "MultiSynthErrorRatio"
  return(out)
}

MultiSynthMeanEffects <- function(MultiSynthPrep_obj, MultiSynth_obj, begin_time, end_time){
  
  #Calculate the pre treatment error ratios here
  out <- mapply(SynthMeanEffect, MultiSynthPrep_obj, MultiSynth_obj, MoreArgs = list(begin_time = begin_time, end_time = end_time), SIMPLIFY = TRUE)
  names(out) <- sapply(MultiSynthPrep_obj, function(dataprep){ return(as.character(dataprep$names.and.numbers$unit.names[ dataprep$names.and.numbers$unit.numbers == dataprep$tag$treatment.identifier]))})
  
  #so that I can print them in a pretty way
  class(out) = "MultiSynthErrorRatio"
  return(out)
}

MultiSynthCovBalance <- function(MultiSynthPrep_obj, MultiSynth_obj, begin_time, end_time){
  
  #Calculate the pre treatment error ratios here
  out <- sapply(MultiSynth_obj, function(synth){ return(synth$loss.w)})
  names(out) <- sapply(MultiSynthPrep_obj, function(dataprep){ return(as.character(dataprep$names.and.numbers$unit.names[ dataprep$names.and.numbers$unit.numbers == dataprep$tag$treatment.identifier]))})
  
  #so that I can print them in a pretty way
  class(out) = "MultiSynthErrorRatio"
  return(out)
}

################################ Functions that help in plotting Synth Output #################################

ExtraVar_lines <- function(ExtraVar1, ExtraVar2 = NA, originalVar, treatmentYear, data, case, caseVar, yearVar, yearInt = 5, ...){
  
  print(c(data[data$ctr_n %in% case & data$yr == (treatmentYear - yearInt), originalVar], data[data$ctr_n %in% case & data$yr >= (treatmentYear), ExtraVar1]))
  print(data[data$ctr_n %in% case & data$yr >= (treatmentYear - yearInt), "yr"])
  
  lines( x = data[data$ctr_n %in% case & data$yr >= (treatmentYear - yearInt), "yr"], y = c(data[data$ctr_n %in% case & data$yr == (treatmentYear - yearInt), originalVar], data[data$ctr_n %in% case & data$yr >= (treatmentYear), ExtraVar1]),  ... )
  
  if(!is.na(ExtraVar2)){
    lines( x = data[data$ctr_n %in% case & data$yr >= (treatmentYear - yearInt), "yr"], y = c(data[data$ctr_n %in% case & data$yr == (treatmentYear - yearInt), originalVar], data[data$ctr_n %in% case & data$yr >= (treatmentYear), ExtraVar2]), ... )
  }
}

####### This function will provide the information necessary to make a gap plot with placebos 
MultiSynthGapPlotPrep <- function(MultiSynthPrep_obj, MultiSynth_obj, main_case){ #the inputs are lists.  
  
  mapply(function(prep, synth, main_case){
    
    if(prep$tag$treatment.identifier != main_case){
      skip <- FALSE
    } else { skip <- TRUE }
    
    gap <- prep$Y1plot - (prep$Y0plot %*% synth$solution.w)
    x.info <- prep$tag$time.plot
    
    return(list(y.info = gap, x.info = x.info, skip = skip))
    
  }, MultiSynthPrep_obj, MultiSynth_obj, MoreArgs = list(main_case = main_case), SIMPLIFY=FALSE)
} # close function

MultiSynthPathPlotPrep <- function(MultiSynthPrep_obj, MultiSynth_obj){
  
  mapply(function(prep, synth){
    
    path <- prep$Y0plot %*% synth$solution.w
    x.info <- prep$tag$time.plot
    return(list(y.info = path, x.info = x.info, skip = FALSE))
    
  }, MultiSynthPrep_obj, MultiSynth_obj, SIMPLIFY=FALSE)
}

#### This function does the plotting
MultiSynthPlotLines <- function(MultiSynthPlotPrep_obj, col = "gray48", lwd = 3/4, cex = 1, t = "l"){
  lapply(MultiSynthPlotPrep_obj,function(controlCase){
    if(!controlCase$skip){
      lines(controlCase$x.info, controlCase$y.info, t = t, col = col, lwd = lwd, cex = cex)
    }
  }) # close lapply
} # close function 

######## The function to use, combines the two above functions in a clean wrapper. 
MultiSynthPlotter <- function(MultiSynthPrep_obj, MultiSynth_obj, gap = TRUE, main_case, ...){
  if(gap){
    temp <- MultiSynthGapPlotPrep(MultiSynthPrep_obj, MultiSynth_obj, main_case )
  } else {
    temp <- MultiSynthPathPlotPrep(MultiSynthPrep_obj, MultiSynth_obj)
  }
  MultiSynthPlotLines(temp, ...)
}

path.plot3 <- function (synth.res = NA, dataprep.res = NA, tr.intake = NA, Ylab = c("Y Axis"), Xlab = c("Time"), Ylim = NA, Legend = c("Treated", "Synthetic"), Legend.position = c("topright"), Main = NA, Z.plot = FALSE, Xlim = NULL, Bty = "o") 
{
  if (Z.plot == FALSE) {
    if (sum(is.na(dataprep.res$Y1plot)) > 0) {
      stop("\n\n#####################################################", 
           "\nYou have missing Y data for the treated!\n\n")
    }
    if (sum(is.na(dataprep.res$Y0plot)) > 0) {
      stop("\n\n#####################################################", 
           "\nYou have missing Y data for the controls!\n\n")
    }
    y0plot1 <- dataprep.res$Y0plot %*% synth.res$solution.w
    if (sum(is.na(Ylim)) > 0) {
      Y.max <- max(c(y0plot1, dataprep.res$Y1plot))
      Y.min <- min(c(y0plot1, dataprep.res$Y1plot))
      Ylim <- c((Y.min - 0.3 * Y.min), (0.3 * Y.max + Y.max))
    }
    plot(dataprep.res$tag$time.plot, dataprep.res$Y1plot, 
         t = "o", pch = 20, col = "black", lwd = 2, main = Main, ylab = Ylab, 
         xlab = Xlab, xaxs = "i", yaxs = "i", ylim = Ylim, xlim = Xlim, xaxt  = "n", yaxt = "n", bty = Bty, cex.lab = 1.5, xpd = NA)
    lines(dataprep.res$tag$time.plot, y0plot1, col = "black", 
          lty = "dashed", lwd = 2, cex = 4/5)
  }
  else {
    z0plot <- dataprep.res$Z0 %*% synth.res$solution.w
    if (sum(is.na(Ylim)) > 0) {
      Y.max <- max(c(z0plot, dataprep.res$Z1))
      Y.min <- min(c(z0plot, dataprep.res$Z1))
      Ylim <- c((Y.min - 0.3 * Y.min), (0.3 * Y.max + Y.max))
    }
    plot(dataprep.res$tag$time.optimize.ssr, z0plot, t = "o", 
         col = "black", lwd = 2, main = Main, ylab = Ylab, 
         xlab = Xlab, xaxs = "i", yaxs = "i", ylim = Ylim, xlim = Xlim)
    lines(dataprep.res$tag$time.optimize.ssr, dataprep.res$Z1, 
          col = "black", lty = "dashed", lwd = 2, cex = 4/5)
  }
  abline(v = tr.intake, lty = 3, col = "black", lwd = 2)
  if (sum(is.na(Legend)) == 0) {
    legend(Legend.position, legend = Legend, lty = c(1,2,1), col = c("black", "black", "grey"), lwd = c(2, 2, 2), cex = 6/7, bty = "n")
  }
}

gaps.plot3 <- function (synth.res = NA, dataprep.res = NA, Ylab = c("Title"), 
                        Xlab = c("Time"), Main = c("Gaps: Treated - Synthetic"), 
                        tr.intake = NA, Ylim = NA, Z.plot = FALSE, Xlim = NULL) 
{
  if (Z.plot == FALSE) {
    if (sum(is.na(dataprep.res$Y1plot)) > 0) {
      stop("\n\n#####################################################", 
           "\nYou have missing Y data for the treated!\n\n")
    }
    if (sum(is.na(dataprep.res$Y0plot)) > 0) {
      stop("\n\n#####################################################", 
           "\nYou have missing Y data for the controls!\n\n")
    }
    gap <- dataprep.res$Y1plot - (dataprep.res$Y0plot %*% 
                                    synth.res$solution.w)
    if (sum(is.na(Ylim)) > 0) {
      Ylim <- c(-(0.3 * max(abs(gap)) + max(abs(gap))), 
                (0.3 * max(abs(gap)) + max(abs(gap))))
    }
    plot(dataprep.res$tag$time.plot, gap, t = "l", col = "black", 
         lwd = 3, main = Main, ylab = Ylab, xlab = Xlab, ylim = Ylim, 
         xaxs = "i", yaxs = "i", yaxt = "n", xaxt = "n", xlim = Xlim, cex.lab = 1.5, xpd = NA)
  }
  else {
    gap <- dataprep.res$Z1 - (dataprep.res$Z0 %*% synth.res$solution.w)
    if (sum(is.na(Ylim)) > 0) {
      Ylim <- c(-(0.3 * max(abs(gap)) + max(abs(gap))), 
                (0.3 * max(abs(gap)) + max(abs(gap))))
    }
    plot(dataprep.res$tag$time.optimize.ssr, gap, t = "l", 
         col = "black", lwd = 2, main = Main, ylab = Ylab, 
         xlab = Xlab, ylim = Ylim, xaxs = "i", yaxs = "i")
  }
  abline(h = 0, col = "black", lty = "dashed", lwd = 2)
  abline(v = tr.intake, col = "black", lty = "dotted", lwd = 2)
}
