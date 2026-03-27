##########
#### Assessing Ballot Structure and Split Ticket Voting
#### Barnes, Tchintian, Alles
#### Journal of Politics, forthcoming.
##########

#### updated: 06/25/2016

#### Creating
#### Directories

dir.create(match.dir, showWarnings=F, recursive=T)
dir.create(result.dir, showWarnings=F, recursive=T)


#### Regression Models
#### by Year & Party

for(j in 1:length(match.method)){

  #### Method Directories
  
  method.dir <- paste(match.dir, match.folder[j], sep="/")
  method.plot.dir <- paste(method.dir, "Plots", sep="/")
  method.sum.dir <- paste(method.dir, "Summary", sep="/")
  method.results.dir <- paste(result.dir, paste(match.folder[j], "Models"), sep="/")

  dir.create(method.sum.dir, showWarnings=F, recursive=T)
  dir.create(method.plot.dir, showWarnings=F, recursive=T)
  dir.create(method.results.dir, showWarnings=F)
  
  #### Matched Data

  mod.data <- subset(dat, gis.method == unique(dat$gis.method)[m] & year == unique(dat$year)[k])  

  party.dummies <- data.frame(matrix(nrow=nrow(mod.data), ncol=length(unique(mod.data$party.number))))
  loop.parties <- c()

  for(i in 1:length(unique(mod.data$party.number))){
    
    party.dummies[,i] <- with(mod.data, ifelse(party.number==unique(mod.data$party.number)[i], 1, 0))
    loop.parties[i] <- unique(mod.data$party.number)[i]
    colnames(party.dummies)[i] <- paste("gov.ticket.", i, sep="")
    
  } ; rm(i)
  
  mod.data <- na.omit(data.frame(mod.data, party.dummies)) ; rm(party.dummies)
  
  if(match.method[j] == "psm"){
    
    match <- matchit(electronic.device ~ poverty.rate + non.educated.rate + opposition.vote.share, 
                     data = mod.data, method = "nearest", distance = "probit", replace=T)
    
    match.dat <- match.data(match, "all")
    match.weight <- match.data(match, "all")$weights
    
    party.matched.data <- data.frame(subset(rbind(party.matched.data, 
                                                  data.frame(matching.method=match.method[j],
                                                             match.dat[!colnames(match.dat)%in%paste("gov.ticket.",seq(2,length(unique(mod.data$gov.ticket)),1),sep="")],
                                                             subclass=NA)),
                                            is.na(precinct) == F),
                                     row.names = NULL)
    
  } else {
    
    match <- matchit(electronic.device ~ poverty.rate + non.educated.rate + opposition.vote.share, 
                   cutpoints=with(mod.data, list(poverty.rate = hist(poverty.rate, breaks=5, plot=F)$breaks,
                                                 non.educated.rate = hist(non.educated.rate, breaks=5, plot=F)$breaks,
                                                 opposition.vote.share = hist(opposition.vote.share, breaks=5, plot=F)$breaks)), 
                   data = mod.data, method = "cem", k2k = T)
    
    match.dat <- match.data(match, "all")
    match.weight <- match.data(match, "all")$weights

    party.matched.data <- data.frame(subset(rbind(party.matched.data,
                                                  data.frame(matching.method=match.method[j],
                                                             match.dat[!colnames(match.dat) %in%
                                                                         paste("gov.ticket.",seq(2,length(unique(mod.data$gov.ticket)),1),sep="")])),
                                            is.na(precinct) == F), 
                                     row.names = NULL)
    
  }
  
  
  #### Matching Reports
  #### Figures C1 & C2 (Appendix)
  
  rep.file <- paste("matchingReport", gis.acro, match.method[j], "allParties", unique(mod.data$year), sep=".")
  
  jpeg(paste(method.plot.dir, paste(rep.file, "jpg", sep="."), sep="/"))
  plot(match)
  dev.off()

  
  #### Matching Reports
  #### Tables C1 & C2 (Appendix)
  
  cat(capture.output(summary(match)),
      file = paste(method.sum.dir, paste(rep.file, "txt", sep="."), sep="/"), 
      sep="\n",append=F)
  

  #### Standardized Difference in Means
  #### Tables C1 & C2 (Appendix)
  
  sd <- c(NA, with(subset(precincts, gis.method == unique(dat$gis.method)[m] & electronic.device == 1),
             c(sd(poverty.rate), sd(non.educated.rate), sd(opposition.vote.share))))

  sum.all <- data.frame(summary(match)$sum.all[4], sd)
  sum.all <- round(data.frame(sum.all, sum.all[,1]/sum.all[,2]), 4)
  sum.all <- data.frame(row.names(summary(match)$sum.all), sum.all, row.names=NULL)
  colnames(sum.all) <- c("covariates","mean.diff", "allTreated.sd", "stdDiff")
  
  sum.matched <- data.frame(summary(match)$sum.matched[4], sd)
  sum.matched <- round(data.frame(sum.matched, sum.matched[,1]/sum.matched[,2]), 4)
  sum.matched <- data.frame(row.names(summary(match)$sum.matched), sum.matched, row.names=NULL)
  colnames(sum.matched) <- c("covariates","mean.diff", "allTreated.sd", "stdDiff")
  
  rep.file <- paste("stdDiff", gis.acro, match.method[j], "allParties", unique(mod.data$year), sep=".")
  
  cat("Call:",
      capture.output(summary(match)$call),
      "\nStandardized difference in means for all data:",
      capture.output(sum.all),
      "\nStandardized difference in means for matched data:",
      capture.output(sum.matched),
      file = paste(method.sum.dir, paste(rep.file, "txt", sep="."), sep="/"), 
      sep="\n",append=F)
  
  
  rm(rep.file, sum.all, sum.matched, sd)
  
  
  #### Model Estimation
  #### Table 2 (Article) and Tables E1 & E2 (Appendix)
  
  mod <- lm(formula = as.formula(as.character(subset(read.csv(paste(main.dir,"mod.equations.csv",sep="/")),
                                                 year==unique(mod.data$year))$equation)), 
          weight = match.weight, data = match.dat)
  mod.sim <- sim(mod, n.sims=n.sims)
  
  
  #### Model Results (Store)

  p.coef <- orderBy(~ party.number, data = subset(parties,
                   year==unique(mod.data$year), select=c(party.number, fig.label)))
  
  p.coef <- as.character(p.coef[p.coef$party.number %in% loop.parties[-1], ]$fig.label)
      
  stargazer(mod, coeftest(mod, cluster.vcov(mod, as.factor(match.dat$precinct))),
            se = list(sqrt(diag(vcovHC(mod, type = "HC"))),
                      coeftest(mod, cluster.vcov(mod, as.factor(match.dat$precinct)))[,2]),
            title=paste("Level of Ballot Splitting, by Type of Voting Device (OLS). ", 
                        match.folder[j],", ", na.omit(unique(dat$gis.method))[m], ". ", 
                        "Provincial Election in Salta Province, ", 
                        unique(mod.data$year),".", sep=""), 
            type = "text",
            dep.var.labels = c(rep("% of Ballot Splitting",2)),
            column.labels = c("Robust SE","Clustered SE"),
            covariate.labels = c("Electronic Device", p.coef, 
                                 paste("Electronic x", p.coef),"(Intercept)"), 
            p = list(2 * pt(abs(coef(mod)/sqrt(diag(vcovHC(mod, type = "HC")))), 
                          df=nrow(match.dat)-2, lower.tail = FALSE),
                   coeftest(mod, cluster.vcov(mod, as.factor(match.dat$precinct)))[,4]),
            no.space = T,
            digits = 4, star.cutoffs = c(.05,.01,.001),
            model.numbers = F, model.names = F,
            omit.stat = c("f", "ser"),
            out = paste(method.results.dir, 
                      paste("mod", gis.acro, match.method[j], "allParties", 
                            unique(mod.data$year), "txt", sep="."), 
                      sep="/"))

  
  #### Coefficients (and CI)
  
  p.coef <- orderBy(~ party.number, data = subset(parties,
                                  year==unique(mod.data$year), select=c(party.number, party.label)))
  
  p.coef <- as.character(p.coef[p.coef$party.number %in% loop.parties[-1], ]$party.label)
  p.coef <- data.frame(coef = c("(Intercept)", "Electronic Device", p.coef, paste("Electronic x", p.coef)))
  
  store <- data.frame(matrix(ncol=6))
  colnames(store) <- c("MLE.coef", "sim.median", "ci.lo.95", "ci.lo.90", "ci.up.90", "ci.up.95")
  
  for(c in 1:nrow(p.coef)){
    
    loop.store <- data.frame(coef(mod)[c], 
                             t(quantile(coef(mod.sim)[,c], c(.5, .025,.050,.950,.975), names=F)))
    colnames(loop.store) <- c("MLE.coef", "sim.median", "ci.lo.95", "ci.lo.90", "ci.up.90", "ci.up.95")
    store <- na.omit(rbind(store, loop.store))
    rm(loop.store)
    
  } ; rm(c)
  
  p.coef <- data.frame(gis.method, match.method[j], unique(mod.data$year), p.coef)
  colnames(p.coef) <- c("gis.method", "match.method", "year", "coef")
  p.coef <- data.frame(p.coef, store, row.names=NULL) ; rm(store)
  
  pooled.coef <- na.omit(rbind(pooled.coef, p.coef)) ; rm(p.coef)
  
  
  ### Prediction
  ### Store
  
  p.coef <- subset(parties, year==unique(mod.data$year))
  p.coef <- p.coef[p.coef$party.number %in% loop.parties, c("party.number", "fig.label")]
  
  p.coef <- data.frame(gis.method, match.method[j], unique(mod.data$year), p.coef, row.names=NULL)
  
  colnames(p.coef)<-c("gis.method", "match.method", "year", "party.number", "party.label")
  
  b <- as.matrix(mod.sim@coef)
  
  X.alpha <- matrix(ifelse(Diagonal(n=nrow(p.coef), x=1)[,2:nrow(p.coef)]==1, yes=0,no=0), nrow=nrow(p.coef))
  X.beta  <- matrix(ifelse(Diagonal(n=nrow(p.coef), x=1)[,2:nrow(p.coef)]==1, yes=1,no=0), nrow=nrow(p.coef))
  X <- cbind(0, 1, X.alpha, X.beta)
  
  Xb <- t(as.matrix(X)%*% t(b)) ; rm(X.alpha,X.beta,X,b)
  
  pred <- data.frame(apply(Xb, 2, mean), t(apply(Xb, 2, quantile, probs=c(.025,.05,.95,.975))))
  colnames(pred) <- c("mean", "ci.lo.95", "ci.lo.90", "ci.up.90", "ci.up.95")
  
  pred <- data.frame(p.coef, pred)

  pooled.pred <- data.frame(orderBy(~ gis.method + match.method + year + party.number,
                                    data = subset(rbind(pooled.pred, pred), is.na(party.number)==F)),
                            row.names=NULL)
  
  rm(Xb, pred, p.coef)
  
  
  #### Cleaning Memory
  
  rm(match, match.dat, match.weight, mod, mod.sim, loop.parties)
  rm(method.dir, method.plot.dir, method.sum.dir, method.results.dir)
  
  
}


rm(mod.data, j)
