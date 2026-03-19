# Auxiliary functions for
# Causal Modeling with Multi-Value and Fuzzy-Set Coincidence Analysis
# 
# Michael Baumgartner & Mathias Ambuehl
#
# 03.02.2018
#
############################################

# Standardizing syntax
sortString <- function(x){
  r <- strsplit(x, "\\*")
  r <- lapply(r, sort)
  lapply(r, paste0, collapse = "*")
}
standardizedCond <- function(x){
  xx <- gsub(" ", "", x)
  xx <- strsplit(xx, "\\+")
  xx <- lapply(xx, sortString)
  xx <- lapply(xx, unlist, use.names = FALSE, recursive = FALSE)
  xx <- lapply(xx, sort)
  sapply(xx, paste0, collapse = "+")
}

# Change expression from cna- to QCA-notation and vice-versa
addStar <- function(r){
  r <- gsub(" ", "", r)
  r <- strsplit(r, "\\+")
  r <- lapply(r, strsplit, "")
  r <- lapply(r, function(x) vapply(x, paste0, character(1), collapse = "*"))
  vapply(r, paste0, character(1), collapse = "+")
}
removeStar <- function(r){
  gsub("*", "", r, fixed = TRUE)
}

# Execute an expression and return "?" if an error occurs
tryTest <- function(expr){
  tryCatch(eval.parent(substitute(expr)), 
           error = function(cond){
             message(cond, "\n")
             return("?")
           } 
  )
}

# Extract solution formulas from cna- or QCA-objects, return "?" if object is neither of the two
getSolution <- function(object, response){
  if (inherits(object, "cna")){
    sol <- object$solution[[response]]$asf$condition
    return(gsub(" ", "", sol, fixed = TRUE))
  }
  if (inherits(object, "qca")){
    mv <- grepl("\\{.\\}", object$tt$outcome) || any(object$tt$noflevels>2)
    if (mv){
      sol <- vapply(object$solution, function(x){
        x <- gsub(pattern = "\\{(.)\\}", replacement = "=\\1", x)
        paste0(x, collapse = "+")
      }, character(1))
    } else {
      sol <- addStar(vapply(object$solution, paste0, character(1), collapse = "+"))
    }
    return(sol)
  }
  "?"
}

# match computed solutions with a set of correct solutions
matchCond <- function(solList, correctModels){
  if (identical(solList, "?") || length(solList) == 0) 
    return(TRUE)
  any(correctModels %in% standardizedCond(solList))
}

# check whether correctness is satisfied without ambiguities
matchCond.without <- function(solList, correctModels){
  if (identical(solList, "?") || length(solList) == 0) 
    return(TRUE)
  if (length(solList) > 1) 
    return(FALSE)
  any(correctModels %in% standardizedCond(solList))
}

# check whether no model is produced
no.model <- function(solList){
  if (identical(solList, "?") || length(solList) == 0) 
    return(TRUE)
    return(FALSE)
}

# check whether exactly one model is produced
unique.model <- function(solList){
  if (identical(solList, "?") || length(solList) == 0 || length(solList) > 1) 
    return(FALSE)
  return(TRUE)
}

# check whether correctness is satisfied by a unique model
matchCond.unique <- function(solList, correctModels){
  if (identical(solList, "?") || length(solList) == 0 || (length(solList) > 1)) 
    return(FALSE)
  any(correctModels %in% standardizedCond(solList))
}

# Transform expressions from binary to mv (by string modification)
modifyStr <- function(x, ...){
  arglist <- list(...)
  nms <- names(arglist)
  stopifnot(is.character(unlist(arglist)), 
            length(nms) == length(arglist))
  for (i in seq_along(arglist))
    x <- gsub(nms[[i]], arglist[[i]], x)
  x
}
# Example: modifyStr("a + b", a = "A=1", b = "Z=3")

# na.zero
na.zero <- function (x) {
  x[is.na(x)] <- 0
  return(x)
}

# Slightly modified variant of the submodels function from the QCApro package.
submodels.var <- function (expression, noflevels = c(), test = TRUE) 
{
  function.call <- match.call()
  qca.object <- is.qca(expression)
  if (qca.object) {
    cn <- colnames(expression$tt$tt)
    if (expression$opts$use.letters == TRUE | all(cn[1:(match("OUT", 
                                                              cn) - 1)] %in% LETTERS)) {
      return(submodels.loop(expression = expression))
    }
    else {
      errmsg <- paste0("If an object of class 'qca' is passed to the 'submodels'\n                            function, please ensure that all exogenous factors are \n                            labelled with single letters or that the argument \n                            'use.letters' is set to 'TRUE' in the call to the \n                            'eQMC' function.")
      cat("\n")
      stop(paste(strwrap(errmsg, exdent = 7), collapse = "\n"), 
           call. = FALSE)
    }
  }
  expression.initial <- expression
  expression <- gsub(" ", "", expression)
  if (grepl("<", expression)) {
    antec <- substr(expression, 1, regexpr("<", expression)[1] - 
                      1)
  }
  else if (grepl("-|=", expression)) {
    antec <- substr(expression, 1, regexpr("-|=", expression)[1] - 
                      1)
  }
  else {
    antec <- expression
  }
  star <- grepl("[*]", antec)
  if (star & grepl("[a-zA-Z]{2,}", antec)) {
    errmsg <- paste0("The conjunction operator is not used consistently in\n                        the expression ", 
                     expression.initial, ". Either the \n                        '*'-operator must be put between each single-letter \n                        conjunct or must not be used at all.")
    cat("\n")
    stop(paste(strwrap(errmsg, exdent = 7), collapse = "\n"), 
         call. = FALSE)
  }
  if (!grepl(">", expression) & grepl("=|-|<", expression)) {
    errmsg <- paste0("The form of the expression was not as expected. Please \n                        consult the documentation of the 'submodels' function.")
    cat("\n")
    stop(paste(strwrap(errmsg, exdent = 7), collapse = "\n"), 
         call. = FALSE)
  }
  else if (grepl(">", expression)) {
    out.f <- substr(expression, regexpr(">", expression)[1] + 
                      1, nchar(expression))
  }
  else {
    out.f <- "PSEUDO"
  }
  if (length(noflevels) == 0) {
    f.labels <- sort(unique(toupper(unlist(strsplit(gsub("[*]|[+]", 
                                                         "", antec), "")))))
    noflevels <- rep(2, length(f.labels) + 1)
    names(noflevels) <- c(f.labels, toupper(out.f))
  }
  else {
    noflevels <- c(noflevels, PSEUDO = 2)
  }
  if (any(duplicated(names(noflevels)))) {
    f.duplic <- names(noflevels)[which(duplicated(names(noflevels)))]
    errmsg <- paste0("The same factor (", f.duplic, ") must not appear in the \n                        antecedent and the outcome.")
    cat("\n")
    stop(paste(strwrap(errmsg, exdent = 7), collapse = "\n"), 
         call. = FALSE)
  }

  split.up <- function(antec, star) {
    strsplit(strsplit(antec, "\\+")[[1]], if (star) "\\*" else "")
  }
  doChar <- Vectorize(function(c) {
    sprintf("tt$%s==%s", toupper(c), ifelse(c %in% LETTERS, 
                                            "1", "0"))
  })
  doWord <- Vectorize(function(W) {
    cs <- strsplit(W, "")[[1]]
    paste0("(", paste(doChar(cs), collapse = " & "), ")")
  })
  processString <- function(antec, out.f, star) {
    parts <- if (!star) {
      strsplit(antec, "\\+")[[1]]
    }
    else {
      strsplit(gsub("[*]", "", antec), "\\+")[[1]]
    }
    paste0("(", paste0(doWord(parts), collapse = " | "), 
           ") == (", ifelse(all(strsplit(out.f, "")[[1]] %in% 
                                  letters), "!", ""), "tt$", toupper(out.f), ")")
  }
  if (test) {
    tt <- data.frame(mintermMatrix(noflevels))
    colnames(tt) <- names(noflevels)
    antec.sorted.test <- paste0(sort(unlist(lapply((split.up(antec = antec, 
                                                             star = star)), paste0, collapse = ifelse(star, "*", 
                                                                                                      "")))), collapse = "+")
    expr <- processString(antec = antec.sorted.test, out.f = out.f, 
                          star = star)
    tt <- tt[eval(parse(text = expr)), ]
    if ((all(strsplit(out.f, "")[[1]] %in% LETTERS) & all(tt[, 
                                                             toupper(out.f)] == 1)) | (all(strsplit(out.f, "")[[1]] %in% 
                                                                                           letters) & all(tt[, toupper(out.f)] == 0))) {
      errmsg <- paste0("The provided model, ", expression.initial, 
                       ", \n                            is no causal structure. The antecedent is a tautology.")
      cat("\n")
      stop(paste(strwrap(errmsg, exdent = 7), collapse = "\n"), 
           call. = FALSE)
    }
    else if ((all(strsplit(out.f, "")[[1]] %in% LETTERS) & 
              all(tt[, toupper(out.f)] == 0)) | (all(strsplit(out.f, 
                                                              "")[[1]] %in% letters) & all(tt[, toupper(out.f)] == 
                                                                                           1))) {
      errmsg <- paste0("The provided model, ", expression.initial, 
                       ", \n                            is no causal structure. The antecedent is a contradiction.")
      cat("\n")
      stop(paste(strwrap(errmsg, exdent = 7), collapse = "\n"), 
           call. = FALSE)
    }
    else {
      sol.list.test <- list()
      sol.list.test <- eQMC(tt, outcome = toupper(out.f), 
                            neg.out = ifelse(all(strsplit(out.f, "")[[1]] %in% 
                                                   letters), TRUE, FALSE))$solution
      for (j in seq_along(sol.list.test)) {
        sol.list.test[[j]] <- lapply(sol.list.test[[j]], 
                                     function(z) {
                                       paste0(sort(unlist(if (grepl("[*]", z)) {
                                         strsplit(z, "[*]")
                                       }
                                       else {
                                         strsplit(z, "")
                                       })), collapse = ifelse(star, "*", ""))
                                     })
      }
      for (j in seq_along(sol.list.test)) {
        sol.list.test[[j]] <- paste0(sort(unlist(sol.list.test[[j]], 
                                                 recursive = FALSE)), collapse = "+")
      }
      if (!antec.sorted.test %in% unlist(sol.list.test)) {
        errmsg <- paste0("The provided model, ", expression.initial, 
                         ", \n                                is no causal structure. It is quite possibly a \n                                case of F + fG = F + G, FG + fH + GH = FG + fH \n                                or FG + fg + HF + HG = g + H.")
        cat("\n")
        stop(paste(strwrap(errmsg, exdent = 7), collapse = "\n"), 
             call. = FALSE)
      }
    }
  }
  conjuncts.base <- split.up(antec = antec, star = star)
  nofchars <- sapply(conjuncts.base, length)
  combine <- function(a, b) combn(b, a, paste, collapse = "") #"*")
  conjuncts.all <- conjuncts.base
  for (i in seq_along(nofchars)) {
    conjuncts.all[[i]] <- unlist(lapply(0:nofchars[i], combine, 
                                        conjuncts.base[[i]]))
  }

  if (length(conjuncts.all) > 1) {
    # === NEW CODE ===
    mygrid <- expand.grid(conjuncts.all, KEEP.OUT.ATTRS = FALSE, 
                          stringsAsFactors = FALSE)
    noDup <- apply(mygrid, 1, anyDuplicated.default, incomparables = "") == 0
    mods <- mygrid[noDup, , drop = FALSE]
    mods[] <- t(apply(mods, 1, function(x) {
      x[orderFast(nchar(x), x)]
    }))
    mods <- unique(mods)
    mods[] <- mods[do.call(order, mods), , drop = FALSE]
    mods <- as.matrix(unique(mods), rownames.force = FALSE)
    
    cbs <- combn(ncol(mods), 2)
    
    ii <- seq(ncol(cbs))
    ii <- ii[order(apply(cbs, 2, diff), decreasing = TRUE)]
    for (i in ii){
      spl1 <- strsplit(mods[, cbs[1, i]], "")
      spl2 <- strsplit(mods[, cbs[2, i]], "")
      ok <- vapply(spl1, length, integer(1)) == 0L
      dropRow <- mapply(function(x1, x2){all(match(x1, x2, nomatch = 0L) > 0L)},
                        spl1[!ok], spl2[!ok],
                        USE.NAMES = FALSE, SIMPLIFY = TRUE)
      ok2 <- rep(FALSE, nrow(mods))
      ok2[!ok] <- !dropRow
      mods <- mods[ok | ok2, , drop = FALSE]
    }

    # === END NEW CODE ===
    
    flatten <- function(x) {
      submods <- gsub(ifelse(star, "^[+]|[+]$", "^[+]|[*]|[+]$"), 
                      "", gsub("[++]+", "+", apply(x, 1, function(y) {
                        paste(sort(y), collapse = "+")
                      })))
      return(submods)
    }
    output.final <- list(expression = expression.initial, 
                         noflevels = noflevels, outcome = out.f, submodels = flatten(mods), 
                         call = function.call)
  }
  else {
    output.final <- list(expression = expression.initial, 
                         noflevels = noflevels, outcome = out.f, submodels = gsub(ifelse(!star, 
                                                                                         "[*]", ""), "", unlist(conjuncts.all)), call = function.call)
  }
  return(output.final)
}



# Auxiliary functions
orderFast <- function (...){
  .Internal(order(na.last = TRUE, decreasing = FALSE, ...))
}

intersectFast <- function (x, y) unique.default(y[match(x, y, 0L)])

mIntersect <- function(x, y){
  mapply(intersectFast, x, y, 
         USE.NAMES = FALSE, SIMPLIFY = FALSE)
}  

setdiffFast <- function (x, y){
  unique.default(if (length(x) || length(y)) 
    x[match(x, y, 0L) == 0L]
    else x)
}

mSetdiff <- function(x, y){
  mapply(setdiffFast, x, y, 
         USE.NAMES = FALSE, SIMPLIFY = FALSE)
} 


# randomDGS function from the QCApro package.
randomDGS <- function (n.DGS = 1, exo.facs = c(""), seed.1 = NULL, seed.2 = NULL, 
                       prob = 0.5, diversity = 1, delete.trivial = FALSE) {
 
  if (is.null(seed.1) | is.null(seed.2)) {
   
      errmsg <- paste0("For reproducability, please specify 'seed.1' and 'seed.2'.")
      cat("\n")
      stop(paste(strwrap(errmsg, exdent = 7), collapse = "\n"), call. = FALSE)
  }
 
  if (prob <= 0 | prob >= 1) {
  
      errmsg <- paste0("The value to the argument 'prob' must be larger than 0 
                        and smaller than 1. It is currently set to ", prob, ".")
      cat("\n")
      stop(paste(strwrap(errmsg, exdent = 7), collapse = "\n"), call. = FALSE)
  }
 
  if (diversity <= 0 | diversity > 1) {
  
      errmsg <- paste0("The value to the argument 'diversity' must be larger than 0 
                    and not larger than 1. It is currently set to ", diversity, ".")
      cat("\n")
      stop(paste(strwrap(errmsg, exdent = 7), collapse = "\n"), call. = FALSE)
  }
 
  exl <- length(exo.facs)
  
  set.seed(seed.1)
  outcome.cols <- matrix(sample(c(0,1), size = n.DGS*2^exl, replace = TRUE, 
                                   prob = c(1 - prob, prob)), 
                         ncol = n.DGS)
  
  if (any(colMeans(outcome.cols) %in% c(0,1))) {
   
      if (delete.trivial == FALSE) {
   
          errmsg <- paste0("At least one model is TRUE (all output function values
                            are 1) or FALSE (all output function values are 0). Please 
                            re-specify 'seed.1', adjust the values to the arguments 
                            'prob' and/or 'diversity', or increase the number of 
                            exogenous factors.")
          cat("\n")
          stop(paste(strwrap(errmsg, exdent = 7), collapse = "\n"), call. = FALSE)
      }
      
      else {
       
       trivial <- apply(outcome.cols, 2, function (x) mean(x) %in% c(0,1))
       outcome.cols <- outcome.cols[ , !trivial, drop = FALSE]  
       
          wrnmsg <- paste0(sum(trivial), " trivial structure(s) (TRUE or FALSE) was/were eliminated.")
          cat("\n")
          warning(paste(strwrap(wrnmsg, exdent = 7), collapse = "\n"), 
                  immediate. = TRUE, call. = FALSE)
      }
  }
  
  if (ncol(outcome.cols) == 0) {
  
      errmsg <- paste0("All structures have been trivial (TRUE or FALSE). Please re-specify
                        seed.1, adjust the values to the arguments 'prob' and/or 'diversity',
                        or increase the number of exogenous factors.")
      cat("\n")
      stop(paste(strwrap(errmsg, exdent = 7), collapse = "\n"), call. = FALSE)
  }

  # create minterm matrix for each truth table and add output function values
  dat <- expand.grid(rep(list(c(0,1)), exl))
  dat <- cbind(dat, outcome.cols)
  n.out.cols <- ncol(outcome.cols)
  colnames(dat) <- c(exo.facs, paste0("OUT", seq(n.out.cols)))
  
  # create limited empirical diversity
  
  if (diversity < 1) {
  
      dat <- dat[sample.int(nrow(dat), as.integer(diversity * nrow(dat))),]
  }
  
  # set up list that includes all models for each solution
  mods <- vector("list", n.DGS)
 
  for (i in seq(n.out.cols)) {
  
       mods[[i]] <- eQMC(dat, exo.facs = exo.facs, outcome = colnames(dat[exl + i]))$solution
  }
 
  nomps <- sapply(mods, length) # number of models per solution
 
  # if there are structural ambiguities, randomly sample one model from the
  # full model space of the solution
 
  set.seed(seed.2)
  for (i in seq(n.out.cols)) {
 
       mods[[i]] <- mods[[i]][sample.int(nomps[i], 1)]
  }
 
  mods.dat <- list(DGS = unlist(lapply(unlist(mods, recursive = FALSE), paste0, collapse = " + ")), 
                   tt = dat)
 
  return(mods.dat)
}