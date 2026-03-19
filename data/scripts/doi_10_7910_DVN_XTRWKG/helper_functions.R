### Function to generate balance table, accepting variables and variable labels as arguments
balance.table <- function(data, variables, labels = variables) {
  
  data <- data %>% dplyr::distinct(resp_id, .keep_all = TRUE)
  
  table <- sapply(variables, function(x) {
    balance.n <- length(na.omit(data[[x]]))
    balance.mean <- mean(as.numeric(data[[x]])==1, na.rm = TRUE)
    balance.sd <- sd(as.numeric(data[[x]])==1, na.rm = TRUE)
    balance.t <- (balance.mean - .5)/balance.sd
    balance.p <- pt(-abs(balance.t), balance.n)*2
    
    # one row of output corresponds to one variable
    row <- cbind(balance.n, balance.mean, balance.sd, balance.p)
    
    return(row)
  })
  
  rownames(table) <- c("N", "Mean", "SD", "p-value")
  colnames(table) <- labels
  
  return(t(table))
}

### Function to plot an AMCE output with user-provided labels, names, titles etc., to use internally only
amce.plot <- function(amce, title, outcome, variables = NULL, labels = variables, plot.data, 
                      outcome_lab = NULL, ...) {
  # if variables is null, use name given  by amce objects
  if(is.null(variables)){
    variables <- names(amce$attributes)
    labels <- variables
  }
  
  # gather all the estimates into one table, order by the order as given in 'variables'
  estimate.plot <- t(bind_rows(amce$estimates))[variables,]
  colnames(estimate.plot) <- c("estimate", "se")
  rownames(estimate.plot) <- labels
  # convert to data frame
  estimate.plot <- data.frame(estimate.plot)
  estimate.plot$var.labels <- factor(labels, levels = rev(labels))
  
  N.temp <- nrow(plot.data)
  
  # calculate 95% CIs
  estimate.plot$upper <- estimate.plot$estimate + qt(0.975, N.temp-1)*estimate.plot$se
  estimate.plot$lower <- estimate.plot$estimate - qt(0.975, N.temp-1)*estimate.plot$se
  
  # Generate plot
  if(is.null(outcome_lab)) {
    outcome_lab <- outcome
  }
  
  if(all(na.omit(plot.data[[outcome]]) %in% 0:1)) {
    y.label <- paste0("AMCE: Change in Pr(", outcome_lab,")")
  } else {
    y.label <- paste0("AMCE: Change in E[", outcome_lab,"]")
  }
  
  plot <- ggplot(estimate.plot, aes(y = estimate, x = var.labels)) +
    geom_point(position=position_dodge(.5), size=3) +
    coord_flip() +
    geom_hline(yintercept = 0, linetype = "dotdash") +
    geom_errorbar(data=estimate.plot, aes(ymin=lower, ymax=upper), width=0.2, position=position_dodge(.5), size=1) +
    theme_bw() +
    theme(text = element_text(size=20), plot.title=element_text(size=20, hjust=0.5), legend.position = "none") +
    labs(title= title) +
    xlab("") +
    #ylim(min(-0.03, 1.1*min(estimate.plot$lower)), max(0.03, 1.1*max(estimate.plot$upper))) +
    ylim(-.03, .4) +
    ylab(y.label)
  
  return(plot)
}
### Function to automatic run cjoint on a subgroup
amce.sub <- function(data, id, title, outcome, variables, labels=variables, 
                     subset.condition = NULL, path = NULL, file = NULL, filetype="eps",...){
  temp.data <- data[!is.na(data[[outcome]]), ]
  
  if (!is.null(subset.condition)){
    temp.data <- eval(parse(text=paste0("subset(temp.data,",subset.condition,")")))
  }
  
  formula <- as.formula(paste(outcome, "~", paste(variables, collapse="+")))
  
  # run AMCE on complete cases
  temp.data <- temp.data[complete.cases(temp.data[,c(outcome, variables, id)]), ]
  amce <- cjoint::amce(formula, data = temp.data[,c(outcome, variables, id)], cluster=TRUE, respondent.id=id)

  ## if title is null do not output graphs and tables
  if(is.null(path)){
    path <- getwd()
  }
  if(!is.null(title)){
    # setEPS()
    # postscript(file = paste0(path, title,".eps"))
    # plot(amce, main=title, text.size=17, ...)
    # dev.off()
    
    ## if file name is null remove all spaces in file names
    if(is.null(file)){
      file <- gsub("[ |\\-|(]+", "_",  
                   gsub(")+", "", 
                        tolower(title)), 
                   perl = TRUE)
    } 
    
    plot <- amce.plot(amce, title, outcome, variables, labels, temp.data,...)
    ggsave(filename=paste0(path, file, ".", filetype), plot=plot, width=11, height=6)
    
    table <- balance.table(data, variables=variables, labels=labels)
    write.csv(table, file=paste0(path, file, ".balance.csv"))
  }
  
  return(amce)
}

### Function to export regression tables to determine t-statistics
amce.compare <- function(data, id, title, outcome, 
                         variables, labels=variables, 
                         by,
                         path, subset.condition = NULL){
  require(multiwayvcov)
  require(stargazer)
  require(haven)
  require(ggplot2)
  
  temp.data <- data[!is.na(data[[outcome]]) & !is.na(data[[by]]), ]
  
  if (!is.null(subset.condition)){
    temp.data <- eval(parse(text=paste0("data[",subset.condition,",]")))
  }
  
  # if data is from Stata 13-14 (and read in by Haven),
  # convert categorical variables (labelled class) into true factors (factor class) in R
  if(class(temp.data[[by]])=="labelled") {
    temp.data[[by]] <- haven::as_factor(temp.data[[by]])
  }
  
  formula <- as.formula(paste(outcome, "~(", paste(variables, collapse="+"), ")*", by))
  
  lm.res <- lm(formula, data=temp.data)
  
  vcov <- multiwayvcov::cluster.vcov(lm.res, temp.data[,id])
  se <- sqrt(diag(vcov))
  
  ## if title is null do not output table
  if(is.null(path)){
    path <- getwd()
  }
  if(!is.null(title)){
    stargazer::stargazer(lm.res, se=list(se), type="text", out=paste0(path, title,".txt"))
  }
  
  ret <- list(coefficients = lm.res$coefficients, se = se, N = nobs(lm.res), subset.condition = subset.condition)
  
  return (ret)
}

### Function to generate comparison plots
compare.ggplot <- function(data, id, title, outcome, 
                           variables, labels = variables, 
                           by, by.var.label = NULL, by.labels = NULL,
                           path = NULL, filetype = "pdf", 
                           outcome_lab = NULL, ...){
  require(haven)
  
  if(!is.null(labels)) {
    if(length(labels) != length(variables)) {
      labels <- variables
      warning("Lengths of labels and variables differ -- using variable names as labels")
    } else {
      names(labels) <- variables # allowing labels to be referred to by variable names
    }
  }
  labels <- factor(labels, levels = rev(labels))
  
  # if data is from Stata 13-14 (and read in by Haven),
  # convert categorical variables (labelled class) into true factors (factor class) in R
  if(class(data[[by]])=="labelled") {
    data[[by]] <- haven::as_factor(data[[by]])
  }
  
  ## Get lower level estimates
  
  # values for all the levels of by
  by.values <- sort(na.omit(unique(data[[by]])))
  
  # labels for all the levels of by
  if(!is.null(by.labels)){
    if(!identical(sort(names(by.labels)), 
                  sort(levels(data[[by]])))) {
      warning("Names of 'by.labels' do not match levels of 'by'.")
    } else {
      
      by.labels <- by.labels[levels(data[[by]]) %in% by.values]
      levels(data[[by]])[levels(data[[by]]) %in% by.values] <- by.labels
      # reset by.values
      by.values <- sort(na.omit(unique(data[[by]])))
      
    }
  } else if(!is.null(levels(data[[by]]))){
    by.labels <- levels(data[[by]])[levels(data[[by]]) %in% by.values]
    warning("Using original levels of 'by' instead!")
  } else {
    by.labels <- by.values
    warning("Using original values of 'by' instead!")
  }
  
  
  # If by has only one value, runs amce.sub()
  if(length(by.values)==1){
    
    title.1.value <- paste(title, by.labels, sep=".")
    amce.sub(data, id, title.1.value, outcome, variables, labels)
    
    return("By has 1 value - runs amce.sub() instead")
  }
  
  # Run AMCE over all possible values of by variable
  amce.list <- lapply(by.values, function(value){
    ## sometime due to data error some values of by variables may not have enough data
    ## use try to skip it
    tryCatch(list(amce_obj=amce.sub(data, id, title=NULL, outcome, variables, labels,
                                    subset.condition=paste0(by, "==\"", as.character(value), '\"')),
                  by.value = value,
                  by.label = by.labels[by.values==value]),
             error= function(e) return(NULL))
  })
  
  
  # Collect all estimates
  estimates <- lapply(amce.list, function(amce) {
    tryCatch(data.frame(t(do.call(cbind, amce$amce_obj$estimates)),
                        realvar = names(amce$amce_obj$attributes),
                        var.labels = labels[names(amce$amce_obj$attributes)],
                        group = amce$by.value,
                        group.labels = amce$by.label,
                        stringsAsFactors=FALSE),
             error = function(e) return(NULL))
  })
  
  estimates.plot <- do.call(rbind,estimates)
  estimates.plot$lower <- estimates.plot$AMCE-1.96*estimates.plot$Std..Error
  estimates.plot$upper <- estimates.plot$AMCE+1.96*estimates.plot$Std..Error
  estimates.plot$group <- factor(estimates.plot$group)
  
  ### If the by variable has only 2 levels, add stars to highlight
  ###    statistically significant differences
  ### For variables with more than 2 levels, stars are complicated
  ###    since it is not clear which comparision they apply to.
  
  compare <- amce.compare(data, id, title, outcome, variables, labels, by, path)
  
  if(length(by.values) == 2) {
    ## Get Interaction Coefficients
    coef.reg <- compare$coefficients
    se.reg <- compare$se
    
    # labels for all possible values of all variables
    variable.labels <- unique(as.vector(sapply(variables, function(x) unique(na.omit(data[[x]])))))
    
    # use regex to attract interaction terms i.e. terms with : and levels of by
    interaction.names.regexp <- paste0("(", paste(variable.labels, collapse="|"), ")", ":.+", "(", paste(by.labels, collapse="|"), ")")
    
    coef.interactions <- coef.reg[grep(interaction.names.regexp, names(coef.reg))]
    se.interactions <- se.reg[grep(interaction.names.regexp, names(se.reg))]
    t.interactions <- coef.interactions/se.interactions
    p.interactions <- (1-pt(abs(t.interactions), compare$N-1))*2
    
    ## Generate stars
    star.interactions <- vector(mode="character", length=length(p.interactions))
    star.interactions[p.interactions<=0.1] <- "*"
    star.interactions[p.interactions<=0.05] <- "**"
    star.interactions[p.interactions<=0.01] <- "***"
    star.interactions[p.interactions>0.1] <- ""
    
    ## Add star to name of lower level variable
    
    names(star.interactions) <- gsub(interaction.names.regexp, "", names(p.interactions))
    
    ## Add stars
    levels(estimates.plot$var.labels) <- rev(paste(labels, star.interactions))
    
  }
  
  ## Titles and labels for graphs
  # If variable has a variable label (e.g. imported from Stata), use it in the label
  # As of 19/10/14, variable is dropped following any dplyr operation so this would not work
  if(is.null(by.var.label)) {
    if(!is.null(attr(data[[by]], "label"))) {
      by.var.label <- attr(data[[by]], "label")
    } else {
      by.var.label <- by
    }
  }
  
  
  # # Title is A vs. B if the by variable has only two values, and variable name otherwise
  # # comment out and keep only what in else{} if we don't need the "A vs. B" in the title
  # if(length(by.values) == 2){
  #   title.lab <- paste0("Heterogeneous treatment effect by ", by.var.label, ": ", paste(by.labels, collapse=" vs. "))
  # } else {
  #   title.lab <- paste0("Heterogeneous treatment effect by ", by.var.label)
  # }
  
  # Use variable label in graph title
  title.lab <- paste0("Heterogeneous treatment effect by ", by.var.label)
  
  
  # Generate plot
  if(is.null(outcome_lab)) {
    outcome_lab <- outcome
  }
  
  if(all(na.omit(data[[outcome]]) %in% 0:1)) {
    y.label <- paste0("AMCE: Change in Pr(", outcome_lab,")")
  } else {
    y.label <- paste0("AMCE: Change in E[", outcome_lab,"]")
  }
  
  
  plot <- ggplot(estimates.plot, aes(y = AMCE, x = var.labels, group = group, shape = group)) +
    geom_point(position = position_dodge(.9), size = 3) +
    coord_flip(ylim = c(-0.03, 0.4)) +
    geom_hline(yintercept = 0, linetype = "dotdash") +
    geom_errorbar(data = estimates.plot, 
                  aes(ymin = lower, ymax = upper), 
                  width = 0.4, 
                  position = position_dodge(.9), 
                  size = 1) +
    scale_shape_discrete(labels = by.labels) +
    theme_bw() +
    theme(text = element_text(size=20), 
          plot.title=element_text(size=20, hjust=0.5),
          legend.position = "bottom") +
    labs(title = title.lab,
         shape = "") +
    xlab("") + 
    ylab(y.label)
  
  if(is.null(path)){
    path <- getwd()
  }
  ggsave(filename=paste0(path, title, ".", filetype), plot=plot, width=11, height=6.2)
  
  return("Comparison figures successfully generated")
}

### Functions to extract coefficients and vcov from an AMCE object similar to coef() and vcov()
coef.amce <- function(object) {
  # it is possible to extract both AMCE and SEs from object$estimates
  # but coef.amce outputs only coefs to maintain compatibility with other coef functions
  coefs <- do.call(cbind, object$estimates)["AMCE",]
  names(coefs) <- names(object$estimates)
  
  return(coefs)
}
coefficients.amce <- coef.amce

vcov.amce <- function(object) {
  vcov <- object$vcov.prof
  
  return(vcov)
}

### Function to estimate indirect effect using parallel design
med.para <- function(formula=NULL, outcome, treatment, mediator, covs=NULL,  exp.id, data) {
  
  ## first experiment: randomize treatment only, no manipulation of mediator
  # estimate average treatment effect on the outcome
  # as average difference between treatment and control
  data.1 <- na.omit(data[data[[exp.id]] == 1, paste(c(outcome, treatment, covs))])
  formula.1 <- as.formula(paste(outcome, paste(c(treatment, covs), collapse = "+"), sep = "~"))
  fit.1 <- lm(formula.1, data = data.1)
  effect.1 <- coef(fit.1)[grep(treatment, names(coef(fit.1)))]
  
  ## second experiment: randomize both treatment and mediator
  # estimate average direct effect on the outcome
  # as average of differences between treatment and control
  # for each value of mediator,
  # over observed distribution of mediator
  data.2 <- na.omit(data[data[[exp.id]] == 2, paste(c(outcome, treatment, covs, mediator))])
  formula.2 <- as.formula(paste(outcome, paste(c(treatment, covs), collapse = "+"), sep = "~"))
  
  # if more than 1 mediator, create temporary placeholder
  if(length(mediator) > 1){
    data.2$mediator.join <- as.factor(apply(data.2[,colnames(data.2) %in% mediator], 1, paste, collapse=""))
    med <- "mediator.join"
    mediator.unique <- unique(data.2[[med]][!is.na(data.2[[med]])])
  } else{
    med <- mediator
    mediator.unique <- unique(data.2[[med]][!is.na(data.2[[med]])])
  }
  
  # calculate individual effect for each value of mediator, then average over empirical dist. of mediator
  effect.2.m <- sapply(mediator.unique, function(m){
    fit.2 <- lm(formula.2, data = data.2[which(data.2[[med]] == m),])
    weight.2 <- nobs(fit.2)
    effect.2 <- coef(fit.2)[grep(treatment, names(coef(fit.2)))]
    return(c(effect = effect.2,
             weight = weight.2))
  })
  effect.2 <- weighted.mean(effect.2.m[1,], effect.2.m[2,])

  return(list(treatment = treatment,
              mediator = mediator,
              total = unname(effect.1),
              indirect = unname(effect.1-effect.2),
              direct = unname(effect.2)))
}

### Function to bootstrap result from med.para
med.para.ri <- function(formula=NULL, 
                        outcome, treatment, mediator, covs=NULL,  
                        exp.id, data, block=NULL, clus=NULL,
                        niter=10000) {
  require(ri)
  
  # remove all missing data (since RI does not remove missing)
  data <- data[complete.cases(data[, c(outcome, treatment, covs, exp.id, block, clus)]), ]
  
  # obtain point estimates
  result.point <- med.para(outcome=outcome,
                           treatment=treatment,
                           mediator=mediator,
                           covs=covs,
                           exp.id=exp.id,
                           data=data)
  
  # purge the outcome variable of covariate-based noise
  if(!is.null(covs)){
    purge <- lm(as.formula(paste(outcome, paste(covs, collapse = "+"), sep = "~")), data = data)
    y.tilde <- resid(purge)  
  } else {
    y.tilde <- data[[outcome]]
  }
  data[["outcome.ri"]] <- y.tilde
  
  if(is.factor(data[[treatment]])) {
    Z <- as.integer(data[[treatment]]) - 1  
  } else {
    Z <- data[[treatment]]
  }
  
  
  # regress the purged outcome on permutations of treatment variable
  if(!is.null(block)){
    block <- as.integer(as.factor(data[[block]]))
  }
  if(!is.null(clus)){
    clus <- as.integer(as.factor(data[[clus]]))
  }
  perm <- genperms(Z, 
                   blockvar = block, 
                   clustvar = clus, 
                   maxiter = niter)
  
  total <- c()
  indirect <- c()
  direct <- c()
  
  for(i in 1:ncol(perm)) {
    data[["treatment.ri"]] <- perm[,i]
    
    result <- med.para(outcome="outcome.ri",
                       treatment="treatment.ri",
                       mediator=mediator,
                       covs=covs,
                       exp.id=exp.id,
                       data=data)
    
    if(!is.na(result$total)){
      total[i] <- result$total
      indirect[i] <- result$indirect
      direct[i] <- result$direct
    } else {
      total[i] <- NA
      indirect[i] <- NA
      direct[i] <- NA
    }
  }
  
  total.greater <- mean(total > result.point$total, na.rm=T)
  total.smaller <- mean(total < result.point$total, na.rm=T)
  indirect.greater <- mean(indirect > result.point$indirect, na.rm=T)
  indirect.smaller <- mean(indirect < result.point$indirect, na.rm=T)
  direct.greater <- mean(direct > result.point$direct, na.rm=T)
  direct.smaller <- mean(direct < result.point$direct, na.rm=T)
  
  return(list(treatment = result.point$treatment,
              mediator = result.point$mediator,
              total = result.point$total,
              total.greater = total.greater,
              total.smaller = total.smaller,
              indirect = result.point$indirect,
              indirect.greater = indirect.greater,
              indirect.smaller = indirect.smaller,
              direct = result.point$direct,
              direct.greater = direct.greater,
              direct.smaller = direct.smaller))
  
}
