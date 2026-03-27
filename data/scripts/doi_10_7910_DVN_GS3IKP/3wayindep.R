#Based on code for 3-way independence available at 
#https://onlinecourses.science.psu.edu/stat504/print/book/export/html/102

f.twowayindep=function(temp,vars,dims,verbose,siglevel) {
  twoway=margin.table(temp,dims)
  result<-f.chisq.test2(twoway,correct = FALSE)
  if (verbose) {
    print(result)
    print(result$expected)
  }
  
  if (is.nan(result$p.value))
      cat("Could not calculate marginal independence of variables",vars[dims],"\n")
  else {
    if (result$p.value < siglevel)
      cat("**Therefore variables",vars[dims],"are NOT marginally independent (p=",format(result$p.value),")**\n")
    else
      cat("Therefore variables",vars[dims],"may be marginally independent.\n")
  }
  twoway=list(Frequency=twoway,RowPercentage=prop.table(twoway,2))
  return(result)
}

f.jointindep=function(temp,vars,dims1,dims2,verbose,siglevel) {
  ### Test for Joint Independence
  joint=ftable(temp, row.vars=dims1)
  result=f.chisq.test2(joint)
  if (verbose) print(result)
  
  if (is.nan(result$p.value)) {
      cat("Could not calculate chi-squared test for joint independence of variables",vars[dims1]," from",vars[dims2],".\n")
      return(1)
  }
  else {
    if (result$p.value<siglevel) 
      cat("**Therefore variables",vars[dims1],"are NOT jointly independent of",vars[dims2],"(p=",format(result$p.value),").**\n")
    else 
      cat("Therefore variables",vars[dims1],"may be jointly independent of",vars[dims2],".\n")
    return(result$p.value)
  }
}

f.chisq.test2=function(temp,correct=FALSE) {
  result <- tryCatch({
    chisq.test(temp,correct=correct)
  }, warning=function(w) {
    chisq.test(temp,simulate.p.value=T,B=10000)
  })
  return(result)
}
f.conditindep=function(temp,vars,dims1,dims2,verbose,siglevel) {
  #### Test for conditional independence
  ### To get partial tables for each level of dimension dims1
  X2=0
  if (dims1==1) {
    for (x in dim(temp)[dims1]) {
      result=f.chisq.test2(temp[x,,],correct=FALSE)
      if (verbose) print(result)
      X2=X2+result$statistic
    }
  } else if (dims1==2) {
    for (x in dim(temp)[dims1]) {
      result=f.chisq.test2(temp[,x,],correct=FALSE)
      if (verbose) print(result)
      X2=X2+result$statistic
    }
  } else if (dims1==3) {
    for (x in dim(temp)[dims1]) {
      result=f.chisq.test2(temp[,,x],correct=FALSE)
      if (verbose) print(result)
      X2=X2+result$statistic
    }
  }
  if (is.nan(X2)) {
    cat("Could not calculate conditional independence of variables",vars[dims2],"given ",vars[dims1],"\n")
    return(0)
  }
  else {
    if (1-pchisq(X2,df=dim(temp)[dims1]<siglevel)) {
      cat("**Therefore variables",vars[dims2],"are NOT conditionally independent given ",vars[dims1],"(p=",format(result$p.value),")**\n")
      return(1-pchisq(X2,df=dim(temp)[dims1]<siglevel))
    }
    else {
      cat("Therefore variables",vars[dims2],"may be conditionally independent given ",vars[dims1],"\n")
      return(0)
    }
  }
}

f.threewayindep=function(temp,temp.df,vars,verbose=F,siglevel=0.1) {
  # One-way table var 3
  Frequency=as.vector(margin.table(temp,3))
  CumFrequency=cumsum(Frequency)
  cbind(levels(temp.df[,vars[3]]),Frequency=Frequency,Percentage=Frequency/sum(Frequency),CumFrequency=CumFrequency,CumPercentage=CumFrequency/sum(Frequency))

  # One-way table var 2
  Frequency=as.vector(margin.table(temp,2))
  CumFrequency=cumsum(Frequency)
  cbind(levels(temp.df[,vars[2]]),Frequency=Frequency,Percentage=Frequency/sum(Frequency),CumFrequency=CumFrequency,CumPercentage=CumFrequency/sum(Frequency))

  # One-way Table var 1
  Frequency=as.vector(margin.table(temp,1))
  CumFrequency=cumsum(Frequency)
  cbind(levels(temp.df[,vars[1]]),Frequency=Frequency,Percentage=Frequency/sum(Frequency),CumFrequency=CumFrequency,CumPercentage=CumFrequency/sum(Frequency))
  if (verbose) summary(temp)

  f.twowayindep(temp,vars,c(3,2),verbose,siglevel)
  f.twowayindep(temp,vars,c(3,1),verbose,siglevel)
  f.twowayindep(temp,vars,c(1,2),verbose,siglevel)

  J1=f.jointindep(temp,vars,c(3,2),1,verbose,siglevel)
  J2=f.jointindep(temp,vars,c(3,1),2,verbose,siglevel)
  J3=f.jointindep(temp,vars,c(1,2),3,verbose,siglevel)
  if (!is.nan(J1+J2+J3))
    if (J1<siglevel & J2<siglevel & J3<siglevel)
      cat("Therefore variables ",vars,"are jointly dependent.\n")
  
  
  f.conditindep(temp,vars,1,c(2,3),verbose,siglevel)
  f.conditindep(temp,vars,2,c(1,3),verbose,siglevel)
  f.conditindep(temp,vars,3,c(1,2),verbose,siglevel)
  
    ### Mantel-Haenszel chi-squared test without continuity correction
    
    ft<-mantelhaen.test(temp)
    mantelhaen.test(temp,correct=FALSE) 
    print(ft)
    temp.condind<-loglin(temp, list(c(1,3), c(2,3)), fit=TRUE, param=TRUE) ### fit the cond.indep. model
    temp.condind
    1-pchisq(temp.condind$lrt, temp.condind$df)    
}
f.word.wrap <- function(string,length) {
  return(lapply(strwrap(string,length,simplify=F),paste,collapse="\n"))
}
