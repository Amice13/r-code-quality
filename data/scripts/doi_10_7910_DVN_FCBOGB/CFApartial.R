# CFApartial.R

group.equal <- ""
intercepts <- FALSE
residuals <- FALSE
layout <- "tree3"
if(invariance == "configural")  
{
  group.equal <- ""
}
if(invariance == "metric")  
{
  group.equal <- "loadings"
}
if(invariance == "scalar")  
{
  group.equal <- c("loadings","intercepts")
  intercepts <- TRUE
  layout <- "tree"
  bifactor <- ""
}
if(invariance == "strict")  
{
  group.equal <- c("loadings","intercepts","residuals")
  intercepts <- TRUE
  residuals <- TRUE
  layout <- "tree"
  bifactor <- ""
}

if(!is.na(filename))
{sink(paste0(filename,".txt"))}

cat("\n*** Confirmatory Factor Analysis: ",title," ***")
if(is.null(group)) # previous analysis
{cat("\n      (preliminary analysis)\n")} else
{cat("\n      (group.equal = ",paste0(group.equal,collapse=", "),")\n",sep="")}

# show current model
cat(model)

o.data <- data
groups <- ""
if(is.null(group)) # previous analysis
{
  groups <- c("All",levels(data$Group))
}
for (g.aux in groups)
{
  data <- o.data
  if(nchar(g.aux)>0 & g.aux!="All")
  {
    data <- subset(data,Group==g.aux)
  }
  if(is.null(group)) 
  {
    cat("\n\n-----------\n",g.aux,"\n-----------\n",sep="")
  }
  model.estim <- lavaan::cfa(model,
                             estimator="MLR",
                             optim.method="BFGS",
                             missing="fiml",
                             std.lv=TRUE,
                             data=data,
                             group=group,
                             group.equal=group.equal,
                             group.partial=group.partial)
  fitmeasure <- lavaan::fitMeasures(model.estim,output="text")
  # print(fitmeasure) # uncomment to see the full output
  lavsummary <- lavaan::summary(model.estim, standardized=TRUE)
  # print(lavsummary) # uncomment to see the full output
  
  cat("\nEXACT TEST (robust):\n")
  X2 <- as.numeric(fitmeasure["chisq.scaled"])
  df <- as.numeric(fitmeasure["df.scaled"])
  # metric, scalar, strict
  
  # heuristic
  X2.by.df <- X2/df
  # p value of X^2
  X2.p <- pchisq(q=X2,df=df,lower.tail=FALSE)
  cat("\n* X^2(",df,"): ",X2,sep="")
  cat("\n* p: ",X2.p,sep="")
  
  cat("\n\nTEST WITH TOLERANCE (robust):\n")
  RMSEA <- fitmeasure["rmsea.robust"]
  RMSEA.lwr <- fitmeasure["rmsea.ci.lower.robust"]
  RMSEA.upr <- fitmeasure["rmsea.ci.upper.robust"]
  RMSEA.pclose <- fitmeasure["rmsea.pvalue.robust"]
  H0.pclose <- ifelse(RMSEA.pclose<0.05,"rejected","not rejected")
  acceptance <- "plausible"
  if(RMSEA.upr < 0.05) {acceptance <- "highly plausible"}
  if(RMSEA.lwr > 0.05) {acceptance <- "not plausible"}
  cat("\n* RMSEA: ",round(RMSEA,3),
      " [",round(RMSEA.lwr,3),",",round(RMSEA.upr,3),"]",sep="")
  cat("\n* pclose: ",RMSEA.pclose,sep="")
  
  # reliability analysis
  cat("\n")
  cat("\nReliability analysis (McDonald's omega):\n")
  factors <- c("G","P","S","H")
  dt.reliability <- semTools::compRelSEM(model.estim)
  if(is.null(group))
  {
    names <- c("group",names(dt.reliability))
    dt.reliability <- data.frame(matrix(data=c(g.aux,as.vector(dt.reliability)),
                                        nrow=1,ncol=length(names)))
    names(dt.reliability) <- names
  }
  for(o.aux in factors)
  {
    if(sum(names(dt.reliability)==o.aux)==0) 
    {
      dt.reliability$new <- NA
      names(dt.reliability) <- c(names(dt.reliability)[1:(ncol(dt.reliability)-1)],o.aux)
    }
  }
  dt.reliability <- dt.reliability[,c("group",factors)]
  print(dt.reliability)
  omega.G1 <- dt.reliability$G[1] 
  omega.P1 <- dt.reliability$P[1] 	
  omega.S1 <- dt.reliability$S[1] 	
  omega.H1 <- dt.reliability$H[1] 	
  omega.G2 <- dt.reliability$G[2] 
  omega.P2 <- dt.reliability$P[2] 	
  omega.S2 <- dt.reliability$S[2] 	
  omega.H2 <- dt.reliability$H[2] 	
  omega.valid <- TRUE
  if(
    sum(dt.reliability[,2:ncol(dt.reliability)]<0,na.rm=TRUE)>0 |
    sum(dt.reliability[,2:ncol(dt.reliability)]>1,na.rm=TRUE)>0 |
    sum(is.na(dt.reliability[,2:ncol(dt.reliability)])>0)>0 |
    sum(!is.finite(as.numeric(unlist(dt.reliability[,2:ncol(dt.reliability)]))))>0
  )
  {
    omega.valid <- FALSE
  }
  
  # model selection
  CFI <- fitmeasure["cfi.robust"]	
  TLI <- fitmeasure["tli.robust"]
  PNFI <- fitmeasure["pnfi.scaled"]
  AGFI <- fitmeasure["agfi"]
  SABIC <- fitmeasure["bic2"] 
  cat("\n")
  cat("\nModel selection:\n")
  cat("\n* CFI = ",CFI,sep="")
  cat("\n* TLI = ",TLI,sep="")
  cat("\n* PNFI = ",PNFI,sep="")
  cat("\n* SABIC = ",SABIC,sep="")

  # graphic representation
  if(!is.na(filename))
  {
    sufix <- ""
    if(what!="std") # non-standard estimates
    {
      sufix <- "_est"
    }
    filepng <- paste0(filename,sufix,"_",g.aux,".png")
  } else
  {
    filepng <- NA
  }
  
  # graphic representation
  if(is.null(group)) # previous analysis
  {
    main <- paste0(title,": ",g.aux)
  } else
  {
    main <- paste0(title,":\nage ",lavsummary$data$group.label)
  }
  source("CFA_graphic.R")
}
cat("\n")

if(!is.na(filename))
{
  sink()
  cat("\nReport written on ",filename,".txt\n",
      "and image(s) stored in ",filename,"*.png\n",
      sep="")
}

# results
dt.result <- data.frame(
  X2, df, X2.p, X2.by.df, 
  RMSEA, RMSEA.lwr, RMSEA.upr, RMSEA.pclose, H0.pclose, 
  acceptance, invariance, 
  CFI, PNFI, AGFI, SABIC,  
  omega.G1, omega.P1, omega.S1, omega.H1, 
  omega.G2, omega.P2, omega.S2, omega.H2, omega.valid)

