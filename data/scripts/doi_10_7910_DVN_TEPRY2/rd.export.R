## Function to export rd.robust table:
require(xtable)
rd.export.one <- function(rd.fit){
  out.table <- round(cbind(rd.fit$coef[1],rd.fit$se[3], rd.fit$z[3], rd.fit$pv[3], rd.fit$ci[3]),3)
  dv <- paste(rd.fit$call)[2]
  print(xtable(cbind(dv,out.table)))
}
# for multiple models:
rd.export <- function(rd.fits){
  if(length(rd.fits)==1){
    rd.fit <- rd.fits
    out.table <- round(cbind(rd.fit$coef[1],rd.fit$se[3], rd.fit$z[3], rd.fit$pv[3], rd.fit$ci[3]),3)
    dv <- paste("DV = ",paste(rd.fit$call)[2],sep="")
    output <- (xtable(cbind(dv,out.table)))
  }
  if(length(rd.fits)>1){
    output <- NULL
    for(i in 1:length(rd.fits)){
      rd.fit <- unlist(rd.fits[i])
      out.table <- round(cbind(rd.fit$coef1,rd.fit$se3,rd.fit$z3,rd.fit$pv3,rd.fit$ci3,rd.fit$ci6),3)
      dv <- paste(rd.fit$call)[2]
      out.table <- cbind(dv,out.table)
      colnames(out.table) <- c("DV","Coef","Std.Err.","z","p-value","CI Lower","CI Upper")
      thisoutput <- (xtable(out.table))
      output <- (rbind(output,thisoutput))
    }
  }
  print((output))
}
##

# for multiple models, paper-formatted version of export:
rd.export2 <- function(rd.fits){
  if(length(rd.fits)==1){
    rd.fit <- rd.fits
    out.table <- round(cbind((100*rd.fit$h),rd.fit$coef[1], rd.fit$ci[3], rd.fit$pv[3]),3)
    dv <- paste("DV = ",paste(rd.fit$call)[2],sep="")
    output <- (xtable(cbind(dv,out.table)))
  }
  if(length(rd.fits)>1){
    output <- NULL
    for(i in 1:length(rd.fits)){
      rd.fit <- unlist(rd.fits[i])
      out.table <- round(cbind((100*rd.fit$h),rd.fit$coef1,rd.fit$ci3,rd.fit$ci6,rd.fit$pv3),2)
      out.table <- cbind(paste(rd.fit$call)[2],out.table)
      out.table <- cbind(out.table[,1],out.table[,2],out.table[,3],paste("(",out.table[,4],", ",out.table[,5],")",sep=""),out.table[,6])
      colnames(out.table) <- c("DV","BW","Coef","CI","p-value")
      thisoutput <- (xtable(out.table))
      output <- (rbind(output,thisoutput))
    }
  }
  print((output))
}

# for multiple models, paper-formatted version of export:
rd.export3 <- function(rd.fits){
    if(length(rd.fits)==1){
        rd.fit <- unlist(rd.fits[[1]])
        out.table <- round(cbind(rd.fit$coef1, rd.fit$ci3,rd.fit$ci6, rd.fit$pv3,as.numeric(rd.fit$tabl1.str1),(100*rd.fit$h)),3)
        dv <- paste("DV = ",paste(rd.fit$call)[2],sep="")
        output <- (xtable(cbind(dv,out.table)))
    }
    if(length(rd.fits)>1){
        output <- NULL
        for(i in 1:length(rd.fits)){
            rd.fit <- unlist(rd.fits[i])
            out.table <- round(cbind(rd.fit$coef1,rd.fit$ci3,rd.fit$ci6,rd.fit$pv3,as.numeric(rd.fit$tabl1.str1),(100*rd.fit$h)),2)
            out.table <- cbind(paste(rd.fit$call)[2],out.table)
            out.table <- cbind(out.table[,1],out.table[,2],paste("(",out.table[,3],",",out.table[,4],")",sep=""),out.table[,5],out.table[,6],out.table[,7])
            colnames(out.table) <- c("DV","Coef","CI","p-value","Obs","BW")
            thisoutput <- (xtable(out.table))
            output <- (rbind(output,thisoutput))
            # print((out.table))

        }
    }
    print((output))
}

# for multiple models, paper-formatted version of export:
rd.export4 <- function(rd.fits,digits_est=2,digits_bw=2){
  if(length(rd.fits)==1){
    rd.fit <- unlist(rd.fits[[1]])
    out.table <- round(cbind(rd.fit$coef1, rd.fit$ci3,rd.fit$ci6, rd.fit$pv3,as.numeric(rd.fit$N_h1+rd.fit$N_h2),round((100*rd.fit$bws1),digits_bw)),digits_est)
    dv <- paste("DV = ",paste(rd.fit$call)[2],sep="")
    output <- cbind(dv,out.table)
    return(output)
    print(xtable(output))
  }
  if(length(rd.fits)>1){
    output <- NULL
    for(i in 1:length(rd.fits)){
      rd.fit <- unlist(rd.fits[i])
      out.table.initial <- round(cbind(rd.fit$coef1,rd.fit$ci3,rd.fit$ci6,rd.fit$pv3,round((100*rd.fit$bws1),digits_bw),as.numeric(rd.fit$N_h1+rd.fit$N_h2)),digits_est)
      out.table.initial2 <- round(cbind(rd.fit$coef1,rd.fit$ci3,rd.fit$ci6,rd.fit$pv3,round((100*rd.fit$bws1),digits_bw),as.numeric(rd.fit$N_h1+rd.fit$N_h2)),digits_est)
      out.table <- cbind(paste(rd.fit$call)[2],out.table.initial)
      out.table <- cbind(out.table[,1],out.table[,2],out.table[,5],out.table[,6],out.table[,7])
      out.table2 <- cbind("", paste("(",out.table.initial2[,2],", ",out.table.initial2[,3],")",sep=""),"","","")
      colnames(out.table) <- c("DV","Coef","p-value","BW","Obs")
      colnames(out.table2) <- c("DV","Coef","p-value","BW","Obs")
      thisoutput <- (out.table)
      thisoutput2 <- (out.table2)
      output <- rbind(output,thisoutput,thisoutput2)
      
    }
  }
  return(output)
  print(xtable(output))
}


rd.export.numeric <- function(rd.fits){
  if(length(rd.fits)==1){
    rd.fit <- unlist(rd.fits[[1]])
    out.table.initial <- tibble(coef=rd.fit$coef1,cilo=rd.fit$ci3,cihi=rd.fit$ci6,pval=rd.fit$pv3,bw=(100*rd.fit$bws1),n=as.numeric(rd.fit$N_h1+rd.fit$N_h2))
    out.table <- bind_cols(outcome=paste(rd.fit$call)[2],out.table.initial)
    print(xtable(out.table),include.rownames = F)
    return(out.table)
  }
  if(length(rd.fits)>1){
    output <- NULL
    for(i in 1:length(rd.fits)){
      rd.fit <- unlist(rd.fits[i])
      out.table.initial <- tibble(coef=rd.fit$coef1,cilo=rd.fit$ci3,cihi=rd.fit$ci6,pval=rd.fit$pv3,bw=(100*rd.fit$bws1),n=as.numeric(rd.fit$N_h1+rd.fit$N_h2))
      out.table <- bind_cols(outcome=paste(rd.fit$call)[2],out.table.initial)
      output <- bind_rows(output,out.table)
    }
    print(xtable(output),include.rownames = F)
    return(output)
  }
}

rd.export.numeric.90 <- function(rd.fits){
  if(length(rd.fits)==1){
    rd.fit <- unlist(rd.fits[[1]])
    out.table.initial <- tibble(coef=rd.fit$coef1,cilo=rd.fit$ci3,cihi=rd.fit$ci6,cilo_90=rd.fit$coef2+qnorm(0.05)*rd.fit$se3,cihi_90=rd.fit$coef2+qnorm(0.95)*rd.fit$se3,pval=rd.fit$pv3,bw=(100*rd.fit$bws1),n=as.numeric(rd.fit$N_h1+rd.fit$N_h2))
    out.table <- bind_cols(outcome=paste(rd.fit$call)[2],out.table.initial)
    print(xtable(out.table),include.rownames = F)
    return(out.table)
  }
  if(length(rd.fits)>1){
    output <- NULL
    for(i in 1:length(rd.fits)){
      rd.fit <- unlist(rd.fits[i])
      out.table.initial <- tibble(coef=rd.fit$coef1,cilo=rd.fit$ci3,cihi=rd.fit$ci6,cilo_90=rd.fit$coef2+qnorm(0.05)*rd.fit$se3,cihi_90=rd.fit$coef2+qnorm(0.95)*rd.fit$se3,pval=rd.fit$pv3,bw=(100*rd.fit$bws1),n=as.numeric(rd.fit$N_h1+rd.fit$N_h2))
      out.table <- bind_cols(outcome=paste(rd.fit$call)[2],out.table.initial)
      output <- bind_rows(output,out.table)
    }
    print(xtable(output),include.rownames = F)
    return(output)
  }
}


# for multiple models, paper-formatted version of export:
rd.export.shares<- function(rd.fits){
    if(length(rd.fits)==1){
        rd.fit <- unlist(rd.fits[[1]])
        out.table <- round(cbind(rd.fit$coef1*100, rd.fit$ci3*100,rd.fit$ci6*100, rd.fit$pv3,as.numeric(rd.fit$tabl1.str1),(100*rd.fit$h)),3)
        dv <- paste("DV = ",paste(rd.fit$call)[2],sep="")
        output <- (xtable(cbind(dv,out.table)))
    }
    if(length(rd.fits)>1){
        output <- NULL
        for(i in 1:length(rd.fits)){
            rd.fit <- unlist(rd.fits[i])
            out.table.initial <- round(cbind(rd.fit$coef1*100,rd.fit$ci3*100,rd.fit$ci6*100,rd.fit$pv3,(100*rd.fit$h),as.numeric(rd.fit$tabl1.str1)),2)
            out.table.initial2 <- round(cbind(rd.fit$coef1*100,rd.fit$ci3*100,rd.fit$ci6*100,rd.fit$pv3,(100*rd.fit$h),as.numeric(rd.fit$tabl1.str1)),2)
            out.table <- cbind(paste(rd.fit$call)[2],out.table.initial)
            out.table <- cbind(out.table[,1],out.table[,2],out.table[,5],out.table[,6],out.table[,7])
             out.table2 <- cbind("", paste("(",out.table.initial2[,2],", ",out.table.initial2[,3],")",sep=""),"","","")
            colnames(out.table) <- c("DV","Coef","p-value","BW","Obs")
               colnames(out.table2) <- c("DV","Coef","p-value","BW","Obs")
         thisoutput <- (xtable(out.table))
             thisoutput2 <- (xtable(out.table2))
           output <- (rbind(output,thisoutput,thisoutput2))
            # print((out.table))

        }
    }
    print((output))
}

# for multiple models, randomization inference
rd.export.ri <- function(rd.fits){
  if(length(rd.fits)==1){
    rd.fit <- unlist(rd.fits[[1]])
    rd.fit <- data.frame(t(rd.fit))
    out.table <- round(cbind(rd.fit$obs.stat, rd.fit$ci1,rd.fit$ci2, rd.fit$asy.pvalue,as.numeric(rd.fit$sumstats2+rd.fit$sumstats7),(100*(rd.fit$window2-rd.fit$window1)/2)),2)
    output <- out.table

    output <- rbind(cbind(out.table[,1],out.table[,4],out.table[,5],out.table[,6]),
                    cbind(paste("(",out.table[,2],", ",out.table[,3],")",sep=""),"","","")
    )
    colnames(output) <- c("Diff. in means","Asymptotic p-value","Obs.","BW")
    
    return(output)
    print(xtable(output))
  }
  if(length(rd.fits)>1){
    output <- NULL
    for(i in 1:length(rd.fits)){
      rd.fit <- unlist(rd.fits[i])
      rd.fit <- data.frame(t(rd.fit))
      out.table <- round(cbind(rd.fit$obs.stat, rd.fit$ci1,rd.fit$ci2, rd.fit$asy.pvalue,as.numeric(rd.fit$sumstats2+rd.fit$sumstats7),(100*(rd.fit$window2-rd.fit$window1)/2)),2)
      thisoutput <- out.table
      
      thisoutput <- rbind(cbind(out.table[,1],out.table[,4],out.table[,5],out.table[,6]),
                      cbind(paste("(",out.table[,2],", ",out.table[,3],")",sep=""),"","","")
      )
      output <- rbind(output,thisoutput)
      colnames(output) <- c("Diff. in means","Asymptotic p-value","Obs.","BW")
      
    }
  }
  return(output)
  print(xtable(output))
}
