##################################################
## Number of Legislators and Welfare            ##
## Created: 2021-08-30                          ##
## Author: Umberto Mignozzetti                  ##
## Obs: Auxiliary Functions                     ##
##################################################

## Auxiliary Functions
ls_mod <- function(x) {
  nl <- 'N Left'; nr <- 'N Right'
  enl <-'Eff N Left'; enr <- 'Eff N Right'
  bwlp <- 'BW Loc Poly'; bwbi <- 'BW Bias'
  for (i in 1:length(x)) {
    nl <- c(nl, x[[i]]$N_l)
    nr <- c(nr, x[[i]]$N_r)
    enl <- c(enl, x[[i]]$N_h_l)
    enr <- c(enr, x[[i]]$N_h_r)
    bwlp <- c(bwlp, x[[i]]$bw_lp)
    bwbi <- c(bwbi, x[[i]]$bw_bias)
  }
  return(list(nl,nr,enl,enr,bwlp,bwbi))
}

ls_semod <- function(x) {
  ls <- list()
  for (i in 1:length(x)) ls[[i]] <- x[[i]]$robust.se
  return(ls)
}

## Estimation Functions

# RD Robust - Trick to print output Stargazer
gen_rdrob <- function(dv, iv, ctrls = NULL, clust = NULL, kink = 0, polord = 1, subset = NULL) {
  if (!is.null(subset)) {
    dv <- dv[subset]
    iv <- iv[subset]
    if(!is.null(clust)) clust <- clust[subset]
    if(!is.null(ctrls)) {
      if (is.null(dim(ctrls))) ctrls <- ctrls[subset]
      else ctrls <- ctrls[subset,]
    }
    
  }
  mod <- rdrobust(dv, iv, covs = ctrls, deriv = kink, cluster = clust,
                  p = polord, q = polord+1)
  modlm <- lm(dv~iv)
  modlm$coefficients[2] = mod$coef[1]
  modlm$robust.se = diag(vcovHC(modlm))
  modlm$robust.se[2] = mod$se[3]
  modlm$N_l = mod$N[1]
  modlm$N_r = mod$N[2]
  modlm$N_h_l = mod$N_h[1]
  modlm$N_h_r = mod$N_h[2]
  modlm$bw_lp = round(mod$bws[1], 3)
  modlm$bw_bias = round(mod$bws[2], 3)
  modlm$subset_reg = subset
  return(modlm)
}

# Kink Plots
kink_plots <- function(vars, labs, cs, iv, clust = NULL, kink = 1,
                       polord = 1, xl = 'Variables', title,
                       regular = T) {
  mod <- data.frame(label = NA, coef = NA, SE = NA)
  for (i in 1:length(vars)) {
    if(cs[i]) ctr = dat[,controls_cs]
    else ctr = dat[,controls]
    if (regular) dv = (dat[,vars[i]] - mean(dat[iv<0,vars[i]], na.rm = T))/sd(dat[,vars[i]], na.rm = T)
    else dv = dat[,vars[i]]
    aux <- gen_rdrob(dv = dv, iv = iv, ctrls = ctr, clust = clust, kink = kink, polord = polord)
    mod <- rbind(mod, data.frame(label = labs[i], coef = aux$coefficients[2], SE = aux$robust.se[2]))
  }
  mod <- na.omit(mod)
  mod$label <- factor(mod$label, levels = labs)
  row.names(mod) <- NULL
  plt <- mod %>%
    mutate(conf.low = coef-1.645*SE,
           conf.high = coef+1.645*SE) %>%
    group_by(label) %>%
    ggplot(aes(x=label, y=coef)) + geom_point() +
    geom_errorbar(aes(ymin=conf.low, ymax=conf.high), size = .7,
                  width = 0.5) + xlab(xl) +
    ylab("Estimate") +
    ggtitle(title) +
    geom_hline(yintercept=0, linetype="dotted") +
    theme_bw() + coord_flip() +
    theme(plot.title = element_text(hjust = 0.5))
  return(list(plt, mod))
}

# Estimate Regression Dropping cases to check robustness
rd_dropping <- function(vars, todrop, iv, cs, clust = NULL,
                        kink = 0, polord = 1, labs,
                        xl = 'Estimates Dropping States (with 90% CI)',
                        title, regular = T) {
  aux_rd_dropping <- function(todrop, dv, iv, ctrls = NULL, clust = NULL, kink = 0, polord = 1, label, regular) {
    if (regular) dv = (dv-mean(dv[iv<0], na.rm = T))/sd(dv, na.rm = T)
    aux <- gen_rdrob(dv = dv, iv = iv, ctrls = ctrls, clust = clust, kink = kink, polord = polord)
    mod <- data.frame(label = label, dropped = 'Full Model', coef = aux$coefficients[2], SE = aux$robust.se[2])
    unique_todrop <- unique(todrop)
    for (i in unique_todrop) {
      aux <- gen_rdrob(dv = dv, iv = iv, ctrls = ctrls, clust = clust, kink = kink, polord = polord, subset = todrop != i)
      mod <- rbind(mod, data.frame(label = label, dropped = i, coef = aux$coefficients[2], SE = aux$robust.se[2]))
    }
    row.names(mod) <- NULL
    return(mod)
  }
  mod <- data.frame(label = NA, dropped = NA, coef = NA, SE = NA)
  for (i in 1:length(vars)) {
    if(cs[i]) ctr = dat[,controls_cs]
    else ctr = dat[,controls]
    aux <- aux_rd_dropping(todrop = todrop, dv = dat[,vars[i]],
                           iv = iv,ctrls = ctr,clust = clust,
                           label = labs[i], regular = regular)
    mod <- rbind(mod,aux)
  }
  mod <- na.omit(mod)
  drp <- c('Full Model', setdiff(unique(mod$dropped), 'Full Model'))
  mod$dropped <- factor(mod$dropped, levels = drp)
  mod$label <- factor(mod$label, levels = labs)
  plt <- mod %>%
    mutate(conf.low = coef-1.645*SE,
           conf.high = coef+1.645*SE) %>%
    group_by(dropped) %>%
    ggplot(aes(x=dropped, y=coef)) + geom_point() +
    geom_errorbar(aes(ymin=conf.low, ymax=conf.high), size = .7,
                  width = 0.5)+ xlab(xl) +
    ylab("Estimates (with 90% CI)") +
    ggtitle(title) +
    geom_hline(yintercept=0, linetype="dotted") +
    theme_bw() + facet_wrap(~label, ncol = 1)+
    theme(plot.title = element_text(hjust = 0.5))
  return(list(plt, mod))
}

# Estimate Sensitivity to Controls
rd_senscontrol <- function(vars, labs, cs, iv, clust = NULL,
                           kink = 0, polord = 1, dfr, regular = T) {
  mod <- data.frame(label = NA, control = NA, coef = NA, SE = NA)
  for (j in 1:length(vars)) {
    if (regular) dv = (dfr[,vars[j]] - mean(dfr[iv<0,vars[j]], na.rm = T))/sd(dfr[,vars[j]], na.rm = T)
    else dv = dfr[,vars[j]]
    if(cs[j]) ctrling <- as.list(sets::set_power(controls_cs))
    else ctrling <- as.list(sets::set_power(controls))
    for (i in ctrling) {
      ctr = as.character(i)
      if (length(ctr)==0) {
        ctr='NC'
        aux <- gen_rdrob(dv = dv, iv = iv, ctrls = NULL, clust = clust, kink = kink, polord = polord)
      } else aux <- gen_rdrob(dv = dv, iv = iv, ctrls = dfr[,ctr], clust = clust, kink = kink, polord = polord)
      mod <- rbind(mod, data.frame(label = labs[j], control = paste(ctr, collapse = ' + '), coef = aux$coefficients[2], SE = aux$robust.se[2]))
    }
  }
  mod <- na.omit(mod)
  mod$control <- factor(mod$control, levels = unique(mod$control[order(nchar(mod$control))]))
  mod$label <- factor(mod$label, levels = labs)
  row.names(mod) <- NULL
  return(mod)
}

# Estimate Sensitivity to Functional Form
rd_sensfform <- function(vds, iv, clust = NULL, kink = 0, polords = c(1:4), 
                         label, dfr, cs, title, regular = T) {
  mod <- data.frame(label = NA, polyorder = NA, coef = NA, SE = NA)
  for (j in 1:length(vds)) {
    if(cs[j]) ctr = dfr[,controls_cs]
    else ctr = dfr[,controls]
    for (i in polords) {
      if(regular) 
        dv = (dfr[,vds[j]] - mean(dfr[dfr[,iv]<0,vds[j]], na.rm = T))/sd(dfr[,vds[j]], na.rm = T)
      else dv = dfr[,vds[j]]
      aux <- gen_rdrob(dv = dv, iv = dfr[,iv],
                       ctrls = ctr,
                       clust = clust, kink = kink, polord = i)
      mod <- rbind(mod, data.frame(label = label[j],
                                   polyorder = as.character(i),
                                   coef = aux$coefficients[2],
                                   SE = aux$robust.se[2]))
    }
    mod <- na.omit(mod)
  }
  row.names(mod) <- NULL
  res <- mod
  mod$label <- factor(mod$label, levels = label)
  mod$conf.low = mod$coef-1.645*mod$SE
  mod$conf.high = mod$coef+1.645*mod$SE
  mod <- group_by(mod, polyorder)
  mod <- ggplot(data = mod, aes(x=polyorder, y=coef)) + geom_point() +
    geom_errorbar(aes(ymin=conf.low, ymax=conf.high), size = .7,
                  width = 0.5)+ xlab("Polynomial Order") +
    ylab("Estimates (with 90% CI)") +
    ggtitle(title) +
    geom_hline(yintercept=0, linetype="dotted") +
    theme_bw() + facet_wrap(~label, ncol = 4)+
    theme(plot.title = element_text(hjust = 0.5))
  return(list(mod, res))
}

# Fill with NA Void Regressions
na_fill <- function(mod) {
  mod$coefficients[2] = NA
  mod$robust.se[2] = NA; mod$N_l = NA
  mod$N_r = NA; mod$N_h_l = NA
  mod$N_h_r = NA; mod$bw_lp = NA
  mod$bw_bias = NA
  return(mod)
}

# Function to compute BWs
bw_robust <- function(vars, vi, dat,
                      bw = seq(.5, 2, .05), nams,
                      clust = NULL, title, regular = F,
                      xl = 'Bandwidth', yl = 'Estimates (with 90% CI)',
                      cs) {
  modmain_hs <- numeric()
  for (i in 1:length(vars)) {
    if(cs[i]) ctr = dat[,controls_cs]
    else ctr = dat[,controls]
    modmain_hs <- c(modmain_hs, gen_rdrob(dat[,vars[i]], dat[,vi], ctr)$bw_lp)
  }
  
  dfs <- list()
  for (i in 1:length(vars))
    dfs[[i]] <- data.frame(bw = bw, est = NA, cilo = NA,
                           cihi = NA, Variable = nams[i])
  for (i in bw) {
    for (j in 1:length(vars)) {
      if(cs[j]) ctr = dat[,controls_cs]
      else ctr = dat[,controls]
      if(regular) vd = (dat[,vars[j]] - mean(dat[dat[,vi]<0,vars[j]], na.rm = T))/sd(dat[,vars[j]], na.rm = T)
      else vd = dat[,vars[j]]
      reg <- rdrobust(y = vd, x = dat[,vi], covs = ctr,
                      h = i*modmain_hs[j], level = 90, cluster = clust)
      dfs[[j]]$est[dfs[[j]]$bw == i] <- reg$coef[1]
      dfs[[j]]$cilo[dfs[[j]]$bw == i] <- reg$ci[1,1]
      dfs[[j]]$cihi[dfs[[j]]$bw == i] <- reg$ci[1,2]
    }
  }
  res <- dfs[[1]]
  for (i in 2:length(vars)) res <- rbind(res, dfs[[i]])
  plt <- ggplot(res, aes(x = bw, y = est)) +
    geom_line(size = 2) +
    geom_hline(yintercept=0, size=.5, linetype=3)+
    geom_vline(xintercept=1, size=.5, linetype=2, color=2)+
    theme_bw() + labs(title = title) + xlab(xl) + ylab(yl) +
    geom_line(aes(x = bw, y = cilo), size = .7, linetype = 2) +
    geom_line(aes(x = bw, y = cihi), size = .7, linetype = 2) +
    facet_wrap(~Variable, ncol = 4)+
    theme(plot.title = element_text(hjust = 0.5))
  return(list(plt,res))
}

# Estimate Regression Adding Cuts one by one
rd_addcuts <- function(vars, addcuts = seq(from = 47.619*2.5, to = 47.619*12.5, 47.619),
                       iv, cs, clust = NULL, kink = 0, polord = 1, labs,
                       xl = 'Estimates Adding Cutoffs',
                       title, regular = T) {
  aux_rd_dropping <- function(todrop, dv, iv, ctrls = NULL, clust = NULL, kink = 0, polord = 1, label, regular, pop) {
    if (regular) dv = (dv-mean(dv[iv<0], na.rm = T))/sd(dv, na.rm = T)
    aux <- gen_rdrob(dv = dv, iv = iv, ctrls = ctrls, clust = clust, kink = kink, polord = polord)
    mod <- data.frame(label = label, addcuts = 'Full Model', coef = aux$coefficients[2], SE = aux$robust.se[2])
    rm(aux)
    for (i in todrop) {
      aux <- gen_rdrob(dv = dv, iv = iv, ctrls = ctrls, clust = clust, kink = kink, polord = polord, subset = dat$pop2003<i)
      mod <- rbind(mod, data.frame(label = label, addcuts = as.character(i), coef = aux$coefficients[2], SE = aux$robust.se[2]))
    }
    row.names(mod) <- NULL
    return(mod)
  }
  mod <- data.frame(label = NA, addcuts = NA, coef = NA, SE = NA)
  for (i in 1:length(vars)) {
    if(cs[i]) ctr = dat[,controls_cs]
    else ctr = dat[,controls]
    aux <- aux_rd_dropping(todrop = addcuts, dv = dat[,vars[i]],
                           iv = iv,ctrls = ctr,clust = clust,
                           label = labs[i], regular = regular,
                           pop=pop)
    mod <- rbind(mod,aux)
  }
  mod <- na.omit(mod)
  drp <- c('Full Model', setdiff(unique(mod$addcuts), 'Full Model'))
  mod$addcuts <- factor(mod$addcuts, levels = drp)
  mod$label <- factor(mod$label, levels = labs)
  plt = mod
  plt$conf.low = plt$coef-1.645*plt$SE
  plt$conf.high = plt$coef+1.645*plt$SE
  plt <- group_by(plt, addcuts)
  plt <- ggplot(data = plt, aes(x=addcuts, y=coef)) + geom_point() +
    geom_errorbar(aes(ymin=conf.low, ymax=conf.high), size = .7,
                  width = 0.5)+ xlab(xl) +
    ylab("Estimates (with 90% CI)") +
    ggtitle(title) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    geom_hline(yintercept=0, linetype="dotted") +
    theme_bw() + facet_wrap(~label, ncol = 1)+
    theme(plot.title = element_text(hjust = 0.5))
  return(list(plt, mod))
}

# Function to run simulations
funcsims <- function(n=100, k=1000, jumps = seq(1,10,1), shape1 = 1,
                     shape2 = 1, addnoise = F, controlled = T) {
  coef <- numeric()
  for (j in 1:n) {
    y <- rep(NA, k)
    x <- rbeta(n = k, shape1 = shape1, shape2 = shape2)
    
    y[x<1] = jumps[10]
    y[x<0.9] = jumps[9]; y[x<0.8] = jumps[8]; y[x<0.7] = jumps[7]
    y[x<0.6] = jumps[6]; y[x<0.5] = jumps[5]; y[x<0.4] = jumps[4]
    y[x<0.3] = jumps[3]; y[x<0.2] = jumps[2]; y[x<0.1] = jumps[1]
    
    if(addnoise) y = y + rnorm(k, 0, 0.1)
    
    cuts <- seq(0.1, 0.9, 0.1)
    run <- NA
    for(i in 1:length(x)) {
      val <- x[i]-cuts
      run[i] <- val[which.min(abs(val))]
    }
    
    if (controlled) mod <- gen_rdrob(y, run, ctrls = x)
    else mod <- gen_rdrob(y, run, ctrls = NULL)
    coef[j] <- mod$coefficients[2]
  }
  return(coef)
}

# Mignozzetti (2021) functions
genData <- function(dat) {
  res <- sample_frac(dat, replace = T)
  return(res)
}

estRDDmr <- function(dv, iv, cuts, varcut, valcut,
                     ctrls = NULL, clust = NULL, 
                     subset = NULL, fixed = FALSE, 
                     ff = 'late~vc', nboot = 50, 
                     nam, data) {
  dfassembler <- 
    data[subset, c(dv,iv,clust,ctrls,varcut)] %>%
    na.omit()
  res <- data.frame(late = NA, vc = NA, nobs = NA)
  lb <- cuts*0.5 # Lower bounds
  ub <- cuts*1.5 # Upper bounds
  for(i in 1:length(cuts)) {
    aux <- dfassembler[,valcut]>=lb[i]&dfassembler[,valcut]<=ub[i]
    aux <- dfassembler[aux,] # Subset dataset within bounds
    row.names(aux) <- NULL
    for (j in 1:nboot) {
      aux2 <- genData(aux)
      resaux <- NULL
      resaux <- gen_rdrob(aux2[,dv], aux2[,iv], aux2[,ctrls], clust = aux2[,clust])$coefficients[2]
      if(!is.null(resaux)) res <- bind_rows(res, data_frame(late = resaux, vc = valcut[i], nobs = nrow(aux)))
    }
  }
  return(res %>% na.omit())
}

## End of File