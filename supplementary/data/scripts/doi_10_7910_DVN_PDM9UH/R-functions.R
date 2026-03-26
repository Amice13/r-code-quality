# ----------------------------------------------------------------------------------------------------------
makeTransparent<-function(someColor, alph){
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                                              blue=curcoldata[3],alpha=alph, maxColorValue=255)})
}

# ----------------------------------------------------------------------------------------------------------
interaction_plot_continuous <- function(model, effect = "", moderator = "", varcov="default", minimum="min", maximum="max",colr = "grey",
                                        incr="default", num_points = 10, conf=.95, mean=FALSE, median=FALSE, alph=80, rugplot=T, 
                                        histogram=F, title="Marginal effects plot", xlabel="Value of moderator", 
                                        ylabel="Estimated marginal coefficient",pointsplot = F,plot = F,show_est = F) {
  # Extract Variance Covariance matrix
  if (varcov == "default"){
    covMat = vcov(model)
  }else{
    covMat = varcov
  }
  
  # Extract the data frame of the model
  mod_frame = model.frame(model)
  
  # Get coefficients of variables
  if(effect == "") {
    int.string <- rownames(summary(model)$coefficients)[grepl(":",rownames(summary(model)$coefficients))]
    effect <- substr(int.string,1,regexpr(":",int.string)[1]-1)
  }
  if(moderator == "") {
    int.string <- rownames(summary(model)$coefficients)[grepl(":",rownames(summary(model)$coefficients))]
    moderator <- substr(int.string,regexpr(":",int.string)[1]+1,nchar(int.string))
  }
  interaction <- paste(effect,":",moderator,sep="")
  beta_1 = summary(model)$coefficients[effect,1]
  beta_3 = summary(model)$coefficients[interaction,1]
  
  # Set range of the moderator variable
  # Minimum
  if (minimum == "min"){
    min_val = min(mod_frame[[moderator]])
  }else{
    min_val = minimum
  }
  # Maximum
  if (maximum == "max"){
    max_val = max(mod_frame[[moderator]])
  }else{
    max_val = maximum
  }
  
  # Check if minimum smaller than maximum
  if (min_val > max_val){
    stop("Error: Minimum moderator value greater than maximum value.")
  }
  
  # Determine intervals between values of the moderator
  if (incr == "default"){
    increment = (max_val - min_val)/(num_points - 1)
  }else{
    increment = incr
  }
  
  # Create list of moderator values at which marginal effect is evaluated
  x_2 <- seq(from=min_val, to=max_val, by=increment)
  
  # Compute marginal effects
  delta_1 = beta_1 + beta_3*x_2
  
  # Compute variances
  var_1 = covMat[effect,effect] + (x_2^2)*covMat[interaction, interaction] + 2*x_2*covMat[effect, interaction]
  
  # Standard errors
  se_1 = sqrt(var_1)
  
  # Upper and lower confidence bounds
  z_score = qnorm(1 - ((1 - conf)/2))
  upper_bound = delta_1 + z_score*se_1
  lower_bound = delta_1 - z_score*se_1
  
  # Determine the bounds of the graphing area
  max_y = max(upper_bound)
  min_y = min(lower_bound)
  
  # Make the histogram color
  hist_col = makeTransparent("grey",alph = alph)
  
  # Initialize plotting window
  if(plot) {
    plot(x=c(), y=c(), ylim=c(min_y, max_y), xlim=c(min_val, max_val), xlab=xlabel, ylab=ylabel, main=title)
    
    # Plot estimated effects
    if(!pointsplot) {
      lines(y=delta_1, x=x_2,col = makeTransparent(colr,alph = alph))
      lines(y=upper_bound, x=x_2, lty=2,col = makeTransparent(colr,alph = alph))
      lines(y=lower_bound, x=x_2, lty=2,col = makeTransparent(colr,alph = alph))
    }else{
      points(y = delta_1,x = x_2,col = makeTransparent(colr,alph = alph))
      segments(x_2,upper_bound,x_2,lower_bound,col = makeTransparent(colr,alph = alph))
      segments(x_2 - increment/8,upper_bound,x_2 + increment/8,upper_bound,col = makeTransparent(colr,alph = alph))
      segments(x_2 - increment/8,lower_bound,x_2 + increment/8,lower_bound,col = makeTransparent(colr,alph = alph))
    }
    # Add a dashed horizontal line for zero
    abline(h=0, lty=3)
    
    # Add a vertical line at the mean
    if (mean){
      abline(v = mean(mod_frame[[moderator]]), lty=2, col="red")
    }
    
    # Add a vertical line at the median
    if (median){
      abline(v = median(mod_frame[[moderator]]), lty=3, col="blue")
    }
    
    # Add Rug plot
    if (rugplot){
      rug(mod_frame[[moderator]])
    }
    if (show_est) {
      stars <- ifelse(abs(summary(model)$coefficients[interaction,3]) >2.6,"***",
                      ifelse(abs(summary(model)$coefficients[interaction,3]) > 1.96,"**",
                             ifelse(abs(summary(model)$coefficients[interaction,3]) > 1.65,"*","")))
      text(par('usr')[ 2 ], par('usr')[ 4 ],adj=c(1.05,1.2),
           labels = paste("Interaction: ",round(summary(model)$coefficients[interaction,1],3),stars," (",
                          round(summary(model)$coefficients[interaction,2],3),")",sep=""))
      
    }
    #Add Histogram (Histogram only plots when minimum and maximum are the min/max of the moderator)
    if (histogram & minimum=="min" & maximum=="max"){
      par(new=T)
      hist(mod_frame[[moderator]], axes=F, xlab="", ylab="",main="", border=hist_col, col=hist_col)
    } 
  }
  return(list(delta_1 = delta_1,x_2 = x_2,ub = upper_bound,lb = lower_bound,inc = increment))
}




# ----------------------------------------------------------------------------------------------------------
mi.extract <- function(model,im.dat,mfx = F,
                       effect = "",moderator = "",num_points = 10,
                       boot=F,subst="ALL") {
  if(subst == "ALL") {
    subst <- lapply(1:length(im.dat), function(x) 1:nrow(im.dat[[x]]))
  }
  
  if(boot) {
    mods <- sapply(1:length(im.dat), function(d) lmer(as.formula(model),data = im.dat[[d]][sample(1:nrow(im.dat[[d]]),replace = T),]))
  } else {
    mods <- sapply(1:length(im.dat), function(d) lmer(as.formula(model),data = im.dat[[d]]))
  }
  
  mfx.res <- NULL
  if(mfx) {
    delta <- apply(sapply(mods,function(m) interaction_plot_continuous(m,effect = effect,moderator = moderator,plot = F,num_points = num_points)$delta_1),1,mean)
    X_2 <- apply(sapply(mods,function(m) interaction_plot_continuous(m,effect = effect,moderator = moderator,plot = F,num_points = num_points)$x_2),1,mean)
    lb <- apply(sapply(mods,function(m) interaction_plot_continuous(m,effect = effect,moderator = moderator,plot = F,num_points = num_points)$lb),1,mean)
    ub <- apply(sapply(mods,function(m) interaction_plot_continuous(m,effect = effect,moderator = moderator,plot = F,num_points = num_points)$ub),1,mean)
    mfx.res <- cbind(delta,X_2,lb,ub)
  }
  
  coefs <- sapply(mods, function(m) summary(m)$coef[,1])
  ses <- sapply(mods,function(m) summary(m)$coef[,2])
  
  Q <- apply(coefs,1,mean)
  U <- apply(ses^2,1,mean)
  B <- apply((coefs - Q)^2,1,sum)/(ncol(coefs)-1)
  var <- U + (1 + 1/ncol(coefs))*B
  nu <- (ncol(coefs) - 1)*(1 + U/((1 + 1/ncol(coefs))*B))^2
  
  coef.table <- matrix(NA, nrow = nrow(coefs), ncol = 4)
  dimnames(coef.table) <- list(rownames(coefs),
                               c("Value", "Std. Error", "t-stat", "p-value"))
  coef.table[,1] <- Q
  coef.table[,2] <- sqrt(var)
  coef.table[,3] <- Q/sqrt(var)
  coef.table[,4] <- pt(abs(Q/sqrt(var)), df=nu, lower.tail=F)*2
  return(list(coef.table,mfx.res))
}