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


## display

mystar <- function(..., font.size = "footnotesize", no.space = T, float = F, keep.stat = c("n", "rsq"), omit = c("(village)", "(Constant)"), 
                   dep.var.labels.include = F, vc = T, dep.var.caption = "Dependent variable: registered") {
  cl0 <- as.list(match.call())
  cl0$font.size <- font.size
  cl0$no.space <- no.space
  cl0$float <- float
  cl0$keep.stat <- keep.stat
  cl0$omit <- omit
  cl0$dep.var.labels.include <- dep.var.labels.include
  cl0$dep.var.caption <- dep.var.caption
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


## datagen

mGraph <- function(nodes, ties, ty = NULL, directed = F, rm = F) {
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
  
  if(rm) {
    nodes <- nodes %>% mutate(hh = substr(as.character(i), 1, nchar(as.character(i))-2))
    rm <- inner_join(
      nodes %>% select(i, hh), 
      nodes %>% select(j = i, hh), 
      by = "hh"
    )
    ties <- anti_join(ties, rm, by = c("i", "j"))
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

treat <- function(g, v, rowNormalize = F, weight = NULL, inTies = F, filter = NULL) {
  m <- as_adj(g, attr = weight)
  if(is.null(filter)) filter <- rep(1, nrow(m))
  if(inTies) m <- t(m)
  o <- m %*% diag(filter) %*% as.numeric(v)
  if(rowNormalize) {
    o <- as.numeric(o) / rowSums(as.matrix(m) %*% diag(filter))
    o[is.na(o)]  <- 0
  }
  as.numeric(o)
}

degreeF <- function(g, filter) {
  m <- as_adj(g, attr = NULL)
  o <- m %*% diag(filter)
  rowSums(as.matrix(o)) %>% as.numeric()
}

degreeFilter <- function(graph, filter) {
  map2(.x = g %>% pull(graph), .y = filter, ~ degreeF(.x, .y)) %>% unlist()
}

netTreat <- function(variable, graph, g, rowNormalize = F, weight = NULL, inTies = F, filter = NULL) {
  if(is.null(filter)) {
    map2(.x = g %>% pull(graph), .y = g$nodes, ~ treat(.x, pull(.y, variable), rowNormalize = rowNormalize, weight = weight, inTies = inTies)) %>% unlist()
  } else {
    map2(.x = g %>% pull(graph), .y = g$nodes, ~ treat(.x, pull(.y, variable), rowNormalize = rowNormalize, weight = weight, inTies = inTies, filter = pull(.y, filter))) %>% unlist()
  }
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
    select(i, villageId, registered, LC) %>% 
    split(.$villageId)
  gTies <- ties %>% 
    filter(i %in% nodes$i & j %in% nodes$i) %>% 
    left_join(nodes %>% select(i, vj = villageId), by = c("j" = "i")) %>% 
    filter(vj == villageId) %>% 
    split(.$villageId) 
  g <- tibble(villageId = sort(unique(ties$villageId)),
              nodes = gNodes, 
              friend = map2(gNodes, gTies, ~ mGraph(.x, .y, "friend", rm = T)), 
              family = map2(gNodes, gTies, ~ mGraph(.x, .y, "family", rm = T)), 
              lender = map2(gNodes, gTies, ~ mGraph(.x, .y, "lender", rm = T)), 
              solver = map2(gNodes, gTies, ~ mGraph(.x, .y, "solver", rm = T)), 
              friendU = map2(gNodes, gTies, ~ mGraph(.x, .y, "friend", rm = T, directed = T)), 
              familyU = map2(gNodes, gTies, ~ mGraph(.x, .y, "family", rm = T, directed = T)), 
              lenderU = map2(gNodes, gTies, ~ mGraph(.x, .y, "lender", rm = T, directed = T)), 
              solverU = map2(gNodes, gTies, ~ mGraph(.x, .y, "solver", rm = T, directed = T)), 
              union = map2(gNodes, gTies, ~ mGraph(.x, .y)), 
              unionRM = map2(gNodes, gTies, ~ mGraph(.x, .y, rm = T)), 
              geo = map2(gNodes, gTies, ~ mGraph(.x, .y, "geo")))
  g
}


corstarsl <- function(x){ 
  x <- as.matrix(x) 
  R <- Hmisc::rcorr(x)$r 
  p <- Hmisc::rcorr(x)$P 
  
  ## define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", " ")))
  
  ## trunctuate the matrix that holds the correlations to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1] 
  
  ## build a new matrix that includes the correlations with their apropriate stars 
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x)) 
  diag(Rnew) <- paste(diag(R), " ", sep="") 
  rownames(Rnew) <- colnames(x) 
  colnames(Rnew) <- paste(colnames(x), "", sep="") 
  
  ## remove upper triangle
  Rnew <- as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew) 
  
  ## remove last column and return the matrix (which is now a data frame)
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  return(Rnew) 
}
