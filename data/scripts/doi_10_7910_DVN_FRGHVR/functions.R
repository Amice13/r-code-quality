## Functions

## Calculates Confidence Intervals ---- 


CIs <- function(d){
  colnames(d)[1:2] <- c("pe","se")
  d$upper <-d$pe + 1.96*d$se
  d$lower <-d$pe - 1.96*d$se
  return(d)
}


## Calculates Cluster Robust SE ----

vcovCluster <- function(model,cluster)
{
  require(sandwich)
  require(lmtest)
  if(nrow(model.matrix(model))!=length(cluster)){
    stop("check your data: cluster variable has different N than model")
  }
  M <- length(unique(cluster))
  N <- length(cluster)           
  K <- model$rank   
  if(M<50){
    warning("Fewer than 50 clusters, variances may be unreliable (could try block bootstrap instead).")
  }
  dfc <- (M/(M - 1)) * ((N - 1)/(N - K))
  uj  <- apply(estfun(model), 2, function(x) tapply(x, cluster, sum));
  rcse.cov <- dfc * sandwich(model, meat = crossprod(uj)/N)
  return(rcse.cov)
}


##  Prepares data for Conjoint Plot ----

create.plot.data <- function(form, 
                             names = c("Male","Female","Mexico","France","Germany","India","Iraq","Nigeria","Philippines","Russia","No formal education","4th Grade","8th Grade","High School","2 year College","College","Graduate","Used interpreter","Tried but unable","Broken English","Fluent English","Atheist","Catholic","Hindu","Jewish","Muslim","Protestant","More than once a week","Once a week","Once or twice a month","A few times a year","Seldom","Never"), 
                             data, 
                             choice=T,
                             basecategory = c("Male","Mexico","No formal education","Used interpreter","Atheist","More than once a week"))
{
  
  levels(data$education)  <- c("No formal education","4th Grade","8th Grade","High School","2 year College","College","Graduate")
  levels(data$language)   <- c("Used interpreter","Tried but unable","Broken English","Fluent English")                        
  levels(data$attends)    <- c("More than once a week","Once a week","Once or twice a month","A few times a year","Seldom","Never")
  
  rnames    <- data.frame(name = names)
  coefnames <- as.character(rnames$name[(rnames$name %in% basecategory)==F])
  # if(length(coefnames)!=27){
  #   stop('check spelling base categories to match:
  #                                     "Male","Female",
  #                                     "Mexico","France","Germany","India","Iraq","Nigeria","Philippines","Russia",
  #                                     "No formal education","4th Grade","8th Grade","High School","2 year College","College","Graduate",
  #                                     "Used interpreter","Tried but unable","Broken English","Fluent English",
  #                                     "Atheist","Catholic","Hindu","Jewish","Muslim","Protestant",
  #                                     "More than once a week","Once a week","Once or twice a month","A few times a year","Seldom","Never"')
  # }

  # Releveling
  data$gender    <- relevel(data$gender, basecategory[1])
  data$country   <- relevel(data$country, basecategory[2])
  data$education <- relevel(data$education, basecategory[3])
  data$language  <- relevel(data$language, basecategory[4])
  data$religion  <- relevel(data$religion, basecategory[5])
  data$attends   <- relevel(data$attends, basecategory[6])
  
  if(choice==T){
    
    #Getting results
    reg  <- lm(form, data) 
    coef <- reg$coef[2:length(reg$coef)]  #Coefs from model
    clse <- coeftest(reg, vcov = vcovCluster(reg, cluster = data[row.names(model.matrix(reg)),]$caseid))[2:length(reg$coef),2] #Clustered by respondent
    
    
    #Cleaning up the coefficient data frame.
    d <- data.frame(y1 = coef, r1 = clse, name = coefnames, row.names = coefnames)
  
    
    #Making base category 0
    d <- merge(rnames, d,  by="name", all.x=T, sort=F)
    d <- merge(rnames, d,  by="name", all.x=T, sort=F)
    d[is.na(d)] <- 0
    d <- d[,-1]
    row.names(d) <- rnames$name
    
  }else{ 
    ### Calculates SE for dependent variable 'RATING'
    vars      <- c("gender","country", "education","language" ,"religion","attends")
    coefs     <- vector("list",length(vars))
    ci        <- vector("list", length(vars))
    
    for (i in 1:length(vars)){      
      reg1 <- lm(data[,"rating"] ~ relevel(data[,vars[i]], ref = basecategory[i]), data = data)     
      coefs[[i]] <- reg1$coef[-1]
      ci[[i]] <- coeftest(reg1, vcov = vcovCluster(reg1, data[row.names(model.matrix(reg1)),]$caseid))[-1,2]
    }
    
    coeff        <- unlist(sapply(1:length(var), function(i) coefs[[i]]))
    names(coeff) <- coefnames
    clse         <- unlist(sapply(1:length(var), function(i) ci[[i]]))
    names(clse)  <- coefnames
    d            <- data.frame(y1 = coeff, r1 = clse, name = coefnames, row.names = coefnames)
    
    #Making base category 0
    d <- merge(rnames,d,  by = "name", all.x = T, sort = F)
    d <- merge(rnames,d,  by = "name", all.x = T, sort = F) #I dont know why but merging it twice gives me the right order. (!!)
    d[is.na(d)] <- 0
    d <- d[,-1]
    row.names(d) <- rnames$name
  }
  
  return(d)
}
         
                    
##  Makes Conjoint Plots ----
                             
cjoint_plot <- function(d, filename, a = -0.5, b = 0.5){  
  

  rnames <- data.frame(name=c("Male","Female","Mexico","France","Germany","India","Iraq","Nigeria","Philippines","Russia",
                              "No formal education","4th Grade","8th Grade","High School","2 year College","College","Graduate",
                              "Used interpreter","Tried but unable","Broken English","Fluent English",
                              "Atheist","Catholic","Hindu","Jewish","Muslim","Protestant",
                              "More than once a week","Once a week","Once or twice a month","A few times a year","Seldom","Never"))
  
  # group vars
  Gender            <- paste(rnames$name[1:2], sep="")
  Origin            <- paste(rnames$name[3:10], sep="")
  Education         <- paste(rnames$name[11:17], sep="")
  LanguageSkills    <- paste(rnames$name[18:21], sep="")
  Religion   	      <- paste(rnames$name[22:27], sep="")	  
  Frequency 	      <- paste(rnames$name[28:33], sep="")
  
  CIs <- function(d){
    colnames(d)[1:2] <- c("pe","se")
    d$upper <-d$pe + 1.96*d$se
    d$lower <-d$pe - 1.96*d$se
    return(d)
  }
  d <- CIs(d)
  d$var <- rownames(d)
  
  FillGroup <- function(d){
    
    d$gruppe <- NA
    d$gruppe[d$var %in% Gender]         <- "Gender"
    d$gruppe[d$var %in% Origin]      	  <- "Origin"
    d$gruppe[d$var %in% Education]      <- "Education"
    d$gruppe[d$var %in% LanguageSkills] <- "Language Skills"
    d$gruppe[d$var %in% Religion]       <- "Religion"
    d$gruppe[d$var %in% Frequency]      <- "Frequency"
    
    # reorder
    d <- rbind(d[d$var %in% Gender,],
               d[d$var %in% Origin,],
               d[d$var %in% Education,],
               d[d$var %in% LanguageSkills,],
               d[d$var %in% Religion,],
               d[d$var %in% Frequency,])
    return(d)
  }
  
  d <- FillGroup(d)
  d$order <- 1:nrow(d)
  
  GetLabels <- function(d){
    offset <- c("   ")
    
    d$var[d$var %in% Gender] <- paste(offset,c("female","male"))
    
    d$var[d$var %in% Origin] <- paste(offset, c("Mexico","France","Germany","India","Iraq","Nigeria","Philippines","Russia"))
    
    d$var[d$var %in% Education] <- paste(offset, c("No formal education","4th Grade","8th Grade","High School","2 year College","College","Graduate"))
    
    d$var[d$var %in% LanguageSkills] <- paste(offset,c("Used interpreter","Tried but unable","Broken English","Fluent English"))
    
    d$var[d$var %in% Religion] <- paste(offset,c("Atheist","Catholic","Hindu","Jewish","Muslim","Protestant"))
    
    d$var[d$var %in% Frequency] <- paste(offset,c("More than Once a week","Once a week","Once or twice a month","A few times a year","Seldom","Never"))
    
    return(d)
  }
  
  d <- GetLabels(d)
  
  # bring in sublabels           
  d <- d[order(d$order),]
  dd <- data.frame(var= c("Gender:",
                          " ",
                          "Origin:",
                          "  ",
                          "Education:",
                          "   ",
                          "Language:",
                          "    ",
                          "Religion:",
                          "     ",
                          "Frequency:"), 
                   order=c(.5,2.1,2.5,10.1,10.5,17.1,17.5,21.1,21.5,27.1,27.5), 
                   pe=1, se=1, upper=1, lower=1, gruppe=NA)
  d <- rbind(d,dd)
  d <- d[order(d$order),]
  d$var <- factor(d$var,levels=unique(d$var)[length(d$var):1])
  
  yylab  <- c("Change in Pr(Immigrant Preferred for Admission to U.S.)")
  
  p = ggplot(d,aes(y=pe,x=var))
  p = p + coord_flip(ylim = c(a, b))  
  p = p + geom_hline(yintercept = 0,size=.5,colour="black",linetype="dotted") 
  p = p + geom_pointrange(aes(ymin=lower,ymax=upper,width=.4),position="dodge",size=.6)
  p = p + scale_y_continuous(name=yylab,breaks=round(seq(a,b,.1),1))
  p = p + scale_colour_discrete("Attribute:") + scale_x_discrete(name="") 
  print(p)
  
  theme_bw1 <- function(base_size = 13, base_family = "") {
    theme_grey(base_size = base_size, base_family = base_family) %+replace%
      theme(
        axis.text.x     = element_text(size = base_size, colour = "black",  hjust = .5 , vjust=1),
        axis.text.y     = element_text(size = base_size , colour = "black", hjust = 0 , vjust=.5 ), # changes position of X axis text
        axis.ticks      = element_line(colour = "grey50"),
        axis.title.y    = element_text(size = base_size,angle=90,vjust=.01,hjust=.1),
        legend.position = "none"
      )
  }
  
  dev.off()
  pdf(paste(filename,".pdf",sep=""),width=10,height=12.5) # saves as pdf
  p = p  + theme_bw1()
  print(p)
  dev.off()	
}



##  cluster bootstrap ----

clusbootreg <- function(formula, data, cluster, reps = 1000)
{
  require(lmtest); require(sandwich); require(multiwayvcov)
  reg1       <- lm(formula, data)
  clusters   <- as.character(data[,cluster]) # De-factorize because factors mess up things
  clusters   <- names(table(clusters)) # Select only unique cluster names
  estims     <- matrix(NA, nrow=reps, ncol=length(coef(reg1))) # Empty matrix to store estimates
  vcovlist   <- list()
  clustervar <- data[,cluster]
  vars_name <- c()
  
  for(i in 1:reps)
  {
    index <- sample(1:length(clusters), length(clusters), replace=TRUE)
    aa    <- clusters[index]
    bb    <- table(aa)
    bootdat <- NULL
    for(j in 1:max(bb)){
      cc <- data[clustervar %in% names(bb[bb %in% j]),] #allows repetition of sampled unique clusters
      for(k in 1:j){
        bootdat <- rbind(bootdat, cc)
      }
    }
    
    # Ad hoc fix for a few bugs ->
    vars_name[i] <- ifelse(length(unique(bootdat[,all.vars(formula)[2]])) == 1, ('novar'), ('var'))
    if(length(unique(bootdat$Rel)) == 2 & length(unique(bootdat$tx.time)) == 2){
      estims[i,] <- coef(lm(formula, bootdat))
      vcovlist[[i]] <- vcov(lm(formula, bootdat))    
    }
    else{
      estims[i,] <- NA
      vcovlist[[i]] <- NA   
      
    }
  }
  
  rownames(estims) <- c(1:reps)
  badrnames <- (rownames(estims[is.na(estims[,3]),]))
  estims1   <- estims[(rownames(estims) %in% badrnames) == F,]
  
  if(length(badrnames)!=0){
    for(i in c(1:length(badrnames))){
      vcovlist[[ as.numeric(badrnames)[i]-(i-1) ]] <- NULL  
    }
  }
  # Ad hoc fix for a few bugs^
  
  val <- cbind(coef(reg1), sapply(1:dim(estims1)[2], function(i) sd(estims1[,i],na.rm=T))) #avoids NA problems with small clusters  
  dd <- c()
  
  coef_no <- length(lm(formula, data)$coefficients)
  
  for(i in 1:length( vcovlist )){if(dim(vcovlist[[i]])[1] != coef_no){dd <- c(dd,i)}} #eliminating those where dim < 5
  if(is.null(dd)==F) {vcovlist <- vcovlist[-dd]}
  
  boot.vcov <- boot.vcov <- apply(simplify2array(vcovlist), c(1,2), function(i) sum(i,na.rm = T))/length(vcovlist) # before (erased because it does not deal with NA): Reduce("+", vcovlist)/length(vcovlist)
  
  

  
  
  colnames(val) <- c("Estimate","Std. Error")
  
  return(list(estim = val, boot.vcov = boot.vcov))
}





