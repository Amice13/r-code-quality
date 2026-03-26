#####################################################################
## Functions used in replication file for                          ##
## It takes a village: Peer Effects and Externalities in           ##
##   Technology Adoption.                                          ##
## Authors: Romain Ferrali, Guy Grossman, Melina R Platas,         ##
##   Jonathan Rodden                                               ##
## June 1st, 2019                                                  ##
## Maintainer: Romain Ferrali (rferrali@nyu.edu)                   ##
#####################################################################


## estimation

myVC <- function(model, R = nRepVc, dev = devel, vclus = NULL) {
  if ("glm" %in% class(model) | dev == T) {
    if(is.null(vclus)) {
      cluster.vcov(model, ~ village)
    } else {
      cluster.vcov(model, vclus)
    }
  } else {
    if(is.null(vclus)) {
      cluster.boot(model, model$model$village, boot_type = "wild", R = R, parallel = cl)
    } else {
      cluster.boot(model, vclus, boot_type = "wild", R = R, parallel = cl)
    }
    
  }
}

iv <- function(..., g, rowNormalize = F, interaction = F) {
  first <- lm(...)
  nodesTmp <- bind_rows(g$nodes)
  nodesTmp$yhat <- predict(first)
  nodesTmp <- split(nodesTmp, nodesTmp$village, drop = T)
  g$nodes <- nodesTmp
  yhat <- netTreat("yhat", "union", g, rowNormalize = rowNormalize)
  y <- netTreat("adopt", "union", g, rowNormalize = rowNormalize)
  mf <- model.frame(first)
  f <- formula(first)
  if(!rowNormalize) {
    mf$nAdopt <- yhat
    f <- update(f, . ~ . + nAdopt)
    if(interaction) f <- update(f, . ~ . + nAdopt:high)
  } else {
    mf$pctAdopt <- yhat
    f <- update(f, . ~ . + pctAdopt)
    if(interaction) f <- update(f, . ~ . + pctAdopt:high)
  }
  second <- lm(f, data = mf)
  mf <- model.frame(second)
  resid <- model.response(mf) - predict(second, newdata = mf)
  vc <- mean(resid^2) * solve(crossprod(model.matrix(second), model.matrix(f, mf)))
  
  out <- list(first = first, second = second, vc = vc)
  class(out) <- "iv"
  out
}

coef.iv <- function(object, ...) coef(object$second, ...)
vcov.iv <- function(object, ...) object$vc
ss <- function(object) sqrt(diag(object$vc))
ts <- function(object) coef(object)/ss(object)
ps <- function(object) 2*pt(-abs(ts(object)), df=ifelse(class(object) == "iv", object$second$df, object$df))
linearHypothesis.iv <- function(m, ...) car::linearHypothesis(m$second, ...)


AMEselection <- function(modHear, modAdopt, modReduced) {
  coef1 <- t(mvrnorm(nRepVc, coef(mods[[modHear]]), modsVC[[modHear]]))
  coef2 <- t(mvrnorm(nRepVc, coef(mods[[modAdopt]]), modsVC[[modAdopt]]))
  coefR <- t(mvrnorm(nRepVc, coef(mods[[modReduced]]), modsVC[[modReduced]]))
  
  sto <- list()
  for (high in c(0,1)) {
    mf1 <- model.matrix(mods[[modHear]])
    mf1 <- mf1[model.frame(mods[[modHear]])$high == high,]
    mf2 <- model.matrix(mods[[modReduced]])
    mf2 <- mf2[model.frame(mods[[modReduced]])$high == high,]
    mf11 <- mf1
    mf11[,"nHeard"] <- mf11[,"nHeard"] + 1
    if(high) mf11[,"nHeard:high"] <- mf11[,"nHeard:high"] + 1
    mf21 <- mf2
    mf21[,"nAdopt"] <- mf21[,"nAdopt"] + 1
    if(high) mf21[,"nAdopt:high"] <- mf21[,"nAdopt:high"] + 1
    
    pe <- c(mean(plogis(mf11 %*% coef(mods[[modHear]])) - plogis(mf1 %*% coef(mods[[modHear]]))), 
            mean(plogis(mf21 %*% coef(mods[[modAdopt]])) - plogis(mf2 %*% coef(mods[[modAdopt]]))), 
            mean(plogis(mf21 %*% coef(mods[[modReduced]])) - plogis(mf2 %*% coef(mods[[modReduced]]))), 
            mean(plogis(mf11 %*% coef(mods[[modHear]])) *  plogis(mf21 %*% coef(mods[[modAdopt]])) 
                 - plogis(mf1 %*% coef(mods[[modHear]])) * plogis(mf2 %*% coef(mods[[modAdopt]]))))
    ci <- list(colMeans(plogis(mf11 %*% coef1) - plogis(mf1 %*% coef1)), 
               colMeans(plogis(mf21 %*% coef2) - plogis(mf2 %*% coef2)), 
               colMeans(plogis(mf21 %*% coefR) - plogis(mf2 %*% coefR)), 
               colMeans(plogis(mf11 %*% coef1) * plogis(mf21 %*% coef2) - 
                          plogis(mf1 %*% coef1) * plogis(mf2 %*% coef2)))
    ci <- ci %>% map(~ quantile(.x, c(.025, .975, .05, .95))) %>% do.call(rbind, .)
    sto[[high+1]] <- tibble(AME = pe, LB = ci[,1], UB = ci[,2], LB90 = ci[,3], UB90 = ci[,4], 
                            Effect = as_factor(c("Hear", "Adopt (conditional on hearing)", 
                                                 "Adopt (total effect)", "Adopt (reduced form)"))) %>% 
      mutate(Effect = fct_rev(Effect), 
             Effect = fct_relevel(Effect, "Adopt (reduced form)"), 
             Uptake = high)
  }
  bind_rows(sto)
}

shareAttr <- function(v) {
  k <- length(v)
  m <- map(unique(v), function(i) {
    u <- as.integer(v == i)
    mm <- Matrix(u, 1, k)
    t(mm) %*% mm
  })
  m <- Reduce("+", m)
  diag(m) <- 0
  drop0(m)
}

lowerTri2Vec <- function(m) {
  k <- nrow(m)
  nTerms <- k * (k-1) / 2
  k <- k-1
  mSum <- as.data.frame(summary(tril(m)))
  i <- mSum$i-1
  j <- mSum$j-1
  i <- j * (2*k-(j-1)) / 2 + i - (j + 1)
  i <- i + 1
  j <- rep(1, length(j))
  sparseMatrix(i, j, x = mSum$x, dims = c(nTerms, 1))
}


## display

mystar <- function(..., font.size = "footnotesize", no.space = T, float = F, keep.stat = c("n", "rsq"), omit = "(village)", 
                   dep.var.labels.include = F, vc = T) {
  cl0 <- as.list(match.call())
  cl0$font.size <- font.size
  cl0$no.space <- no.space
  cl0$float <- float
  cl0$keep.stat <- keep.stat
  cl0$omit <- omit
  cl0$dep.var.labels.include <- dep.var.labels.include
  cl0[[1]] <- quote(stargazer)
  cl0$vc <- NULL
  if(vc) {
    cl <- as.list(match.call(expand.dots = FALSE))
    cl <- cl$`...`
    if(!is.null(names(cl))) cl <- cl[names(cl) == ""]
    cl <- lapply(cl, eval)
    # vvc <- lapply(cl, function(m) vcovHC(m, "HC3"))
    vvc <- lapply(cl, function(m) myVC(m))
    se <- lapply(vvc, function(m) {
      sqrt(diag(m))
    })
    tstat <- mapply(function(m, vc) {
      coef(m) / sqrt(diag(vc))
    }, m = cl, vc = vvc, SIMPLIFY = F)
    pstat <- mapply(function(m, vc) {
      2*pt(-abs(coef(m) / sqrt(diag(vc))), df=m$df.res)
    }, m = cl, vc = vvc, SIMPLIFY = F)
    cl0$se <- se
    cl0$t <- tstat
    cl0$p <- pstat
  }
  eval(as.call(cl0))
}

starSD <- function(m) {
  co <- map(mods[m], ~ names(coef(.)))
  vc <- modsVC[m]
  map2(co, vc, function(co, vc) {
    sd <- sqrt(diag(vc))
    names(sd) <- co
    sd
  })
}

pvalToStars <- function(pval) {
  if(is.na(pval)) {
    return("")
  } else {
    pval <- cut(
      pval, 
      c(0,.001,.05,.1,1), 
      labels = c("$^{***}$", "$^{**}$", "$^{*}$", "")
    )
    as.character(pval)
  }
}

FtestIV <- function(mod, interaction = F) {
  if(!interaction) {
    mod0 <- lm(update(formula(mod), . ~ . - distMeeting), model.frame(mod))
  } else {
    mod0 <- lm(update(formula(mod), . ~ . - distMeeting - distMeeting:high), model.frame(mod))
  }
  
  test <- anova(mod0, mod, test = "F")
  pval <- pvalToStars(test$`Pr(>F)`[2])
  paste0(round(test$F[2], 3), pval)
}



Ftest <- function(m, vc, LHS) {
  test <- car::linearHypothesis(m, LHS, test = "F", vcov. = vc)
  pval <- pvalToStars(test[2,4])
  paste0(round(test[2,3], 3), pval)
}

beta12 <- function(ms, betas = NULL) {
  out <- map_chr(ms, function(m, betas) {
    mod <- mods[[m]]
    vc <- modsVC[[m]]
    # browser()
    hyp <- matrix(0, nrow = 1, ncol = length(coef(mod)))
    colnames(hyp) <- names(coef(mod))
    if(is.null(betas)) betas <- c("nAdopt", "nAdopt:high")
    hyp[,betas] <- 1
    pval <- car::linearHypothesis(mod, hyp, test = "F", vcov. = vc)[2,4]
    pval <- pvalToStars(pval)
    co <- round(sum(coef(mod)[betas]), 3)
    paste0(co, pval)
  }, betas = betas)
  c("$\\beta_1 + \\beta_2$", out)
}


## datagen

mGraph <- function(nodes, ties, ty = NULL, directed = F) {
  if(!is.null(ty)) {
    ties <- ties %>% filter(type == ty)
    if(ty != "geo") {
      ties <- ties %>% select(i,j)
    } else {
      ties <- ties %>% select(i, j, dist)
      g <- graph_from_data_frame(ties, directed = F, vertices = nodes)
      E(g)$w <- 1 / log(E(g)$dist + 1)
      return(g)
    }
  } else {
    ties <- ties %>% 
      filter(type %in% c("family", "friend", "lender", "solver")) %>% 
      select(i, j) %>% 
      distinct()
  }
  
  g <- graph_from_data_frame(ties, directed = TRUE, vertices = nodes)
  if(!directed) {
    g <- as.undirected(g, mode = "collapse")
  }
  g
}

netCov <- function(gs, FUN, ...) {
  unlist(map(.x = gs, .f = FUN, ...))
}

treat <- function(g, v, rowNormalize = F, weight = NULL, inTies = F) {
  m <- as_adj(g, attr = weight)
  if(inTies) m <- t(m)
  o <- m %*% as.numeric(v)
  if(rowNormalize) {
    o <- as.numeric(o) / rowSums(as.matrix(m))
    o[is.na(o)]  <- 0
  }
  as.numeric(o)
}


netTreat <- function(variable, graph, g, rowNormalize = F, weight = NULL, inTies = F) {
  map2(.x = g %>% pull(graph), .y = g$nodes, ~ treat(.x, pull(.y, variable), rowNormalize = rowNormalize, weight = weight, inTies = inTies)) %>% unlist()
}

treatSimple <- function(x, y) {
  x <- as_adj(x, sparse = F)
  yMat <- as_adj(y, sparse = F, attr = "weight")
  zeroes <- x == 0
  yMat[zeroes] <- 0
  yMat[yMat > 1] <- 0
  as.integer(yMat %*% V(y)$adopt)
}

treatCpl <- function(y) {
  yMat <- as_adj(y, sparse = F, attr = "weight")
  yMat[yMat == 1] <- 0
  as.integer(yMat %*% V(y)$adopt)
}


netTreatSimple <- function(graph, g) {
  g1 <- g %>% pull(graph)
  g2 <- g %>% pull(union)
  map2(g1, g2, ~ treatSimple(.x, .y)) %>% unlist()
}

netTreatCpl <- function(graph, g) {
  g <- g %>% pull(graph)
  map(g, ~ treatCpl(.)) %>% unlist()
}




makeGraphs <- function(nodes, ties) {
  gNodes <- nodes %>% 
    select(i, villageId, adopt, heard, leader, satisfaction, nSent) %>% 
    mutate(isSat = as.integer(!is.na(satisfaction)), 
           satisfaction = ifelse(is.na(satisfaction),0,satisfaction), 
           adopt3 = as.integer(nSent >= 3), 
           adopt5 = as.integer(nSent >= 5)
    ) %>% 
    split(.$villageId)
  gTies <- ties %>% 
    filter(i %in% nodes$i & j %in% nodes$i) %>% 
    left_join(nodes %>% select(i, vj = villageId), by = c("j" = "i")) %>% 
    filter(vj == villageId) %>% 
    split(.$villageId) 
  
  g <- tibble(villageId = sort(unique(ties$villageId)),
              nodes = gNodes, 
              friend = map2(gNodes, gTies, ~ mGraph(.x, .y, "friend")), 
              family = map2(gNodes, gTies, ~ mGraph(.x, .y, "family")), 
              lender = map2(gNodes, gTies, ~ mGraph(.x, .y, "lender")), 
              solver = map2(gNodes, gTies, ~ mGraph(.x, .y, "solver")), 
              speak = map2(gNodes, gTies, ~ mGraph(.x, .y, "speak", directed = T)), 
              union = map2(gNodes, gTies, ~ mGraph(.x, .y)), 
              unionD = map2(gNodes, gTies, ~ mGraph(.x, .y, directed = T)), 
              geo = map2(gNodes, gTies, ~ mGraph(.x, .y, "geo")))
  
  wgt <- pmap(
    g %>% select(friend, family, lender, solver), 
    function(friend, family, lender, solver) {
      gg <- as_adj(friend, sparse = F) + 
        as_adj(family, sparse = F) + 
        as_adj(lender, sparse = F) + 
        as_adj(solver, sparse = F)
      gg <- graph_from_adjacency_matrix(gg, weighted = T, mode = "undirected")
      E(gg)$weight
    }
  )
  
  g$union <- map2(g$union, wgt, 
                  function(g, w) {
                    E(g)$weight <- w
                    g
                  })
  
  
  g
}
