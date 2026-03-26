family.tsls <- function(object, ...) gaussian()

logLik.tsls <- function(object, ...) {
    class(object) <- c("lm")
    logLik(object)
}

pvalue.default <- function(object, parm, null = 0, side = 0, log.p = FALSE, vcov.command = NULL, ...) {
    cf <- coef(object)
    pnames <- names(cf)
    if (missing(parm)) 
        parm <- seq(along = pnames)
    else if (is.character(parm)) 
        parm <- match(parm, pnames, nomatch = 0)
    pv <- rep(NA, length = length(parm))
    names(pv) <- pnames[parm]
                                        #  ses <- sqrt(diag(vcov(object)))[parm]
    if (is.null(vcov.command)) {
        ses <- sqrt(diag(vcov(object)))[parm]
    }
    else {
        ses <- sqrt(diag(vcov.command(object)))[parm]
    }

    pv[] <- pnorm(cf[parm], mean = null, sd = ses, lower.tail = side < 1, log.p = FALSE)
    ifelse(side == rep(0,length(parm)), 1-2*abs(pv-.5), pv)
}

pvalue.glm <- function(object, parm, null = 0, side = 0, log.p = FALSE, vcov.command = NULL, ...)
    pvalue.default(object, parm, null = null, side = side, log.p = log.p, ...)

pvalue.Arima <- function(object, parm, null = 0, side = 0, log.p = FALSE, vcov.command = NULL, ...)
    pvalue.default(object, parm, null = null, side = side, log.p = log.p, ...)

pvalue.lm <- function(object, parm, null = 0, side = 0, log.p = FALSE, vcov.command = NULL, ...) {
    cf <- coef(object)
    pnames <- names(cf)
    if (missing(parm)) 
        parm <- seq(along = pnames)
    else if (is.character(parm)) 
        parm <- match(parm, pnames, nomatch = 0)
    pv <- rep(NA, length = length(parm))
    names(pv) <- pnames[parm]
                                        #  ses <- sqrt(diag(vcov(object)))[parm]
    if (is.null(vcov.command)) {
        ses <- sqrt(diag(vcov(object)))[parm]
    }
    else {
        ses <- sqrt(diag(vcov.command(object)))[parm]
    }

    pv[] <- pt((cf[parm]-null)/ses, df = length(case.names(object)) - length(cf), lower.tail = side < 1, log.p = FALSE)
    ifelse(side == rep(0,length(parm)), 1-2*abs(pv-.5), pv)
}

pvalue.tsls <- function(object, parm, null = 0, side = 0, log.p = FALSE, vcov.command = NULL, ...) {
    cf <- coef(object)
    pnames <- names(cf)
    if (missing(parm)) 
        parm <- seq(along = pnames)
    else if (is.character(parm)) 
        parm <- match(parm, pnames, nomatch = 0)
    pv <- rep(NA, length = length(parm))
    names(pv) <- pnames[parm]
                                        #  ses <- sqrt(diag(vcov(object)))[parm]
    if (is.null(vcov.command)) {
        ses <- sqrt(diag(vcov(object)))[parm]
    }
    else {
        ses <- sqrt(diag(vcov.command(object)))[parm]
    }

    pv[] <- pt((cf[parm]-null)/ses, df = object$n - object$p, lower.tail = side < 1, log.p = FALSE)
    ifelse(side == rep(0,length(parm)), 1-2*abs(pv-.5), pv)
}

pvalue <- function (object, parm, null = 0, side = 0, log.p = FALSE, vcov.command = NULL, ...) 
    UseMethod("pvalue")

variable.names.tsls <- function(object, ...)
    names(coef(object))

variable.names.Arima <- function(object, ...)
    names(coef(object))

variable.names.replacelist <- function(object, replacelist = NULL) {
    vn <- variable.names(object)
    if (!is.null(replacelist)) {
        for (item in replacelist) {
            if (length(item)==2) {
                vn <- ifelse(vn==item[1], item[2], vn)
            }
        }
    }
    return(vn)
}

family.Arima <- function(x, ...) list(family="")

get.modelinfo <- function(models, vcov.command = NULL, replacelist = NULL) {

    vns <- NULL
    vnlist <- NULL
    lls <- NULL
    ns <- NULL
    r2s <- NULL
    ar2s <- NULL
    pfstats <- NULL

    for(object in models)
    {
        vns <- append(vns, variable.names.replacelist(object, replacelist))
    }
    vns <- unique(vns)
    cfs <- matrix(NA, nrow=length(vns), ncol=length(models), dimnames=list(vns, names(models)))
    ses <- matrix(NA, nrow=length(vns), ncol=length(models), dimnames=list(vns, names(models)))
    pvs <- matrix(NA, nrow=length(vns), ncol=length(models), dimnames=list(vns, names(models)))

    for(i in 1:length(models))
    {
        object <- models[[i]]

        vn <- variable.names.replacelist(object, replacelist)
        cf <- coef(object)
        if (is.null(vcov.command)) {
            se <- sqrt(diag(vcov(object)))
        }
        else {
            if (length(vcov.command) == length(models)) {
                se <- sqrt(diag(vcov.command[[i]](object)))
            }
            else {
                se <- sqrt(diag(vcov(object)))
            }
        }
        

        pv <- pvalue(object, vcov.command = vcov.command[[i]])
        ll <- try(logLik(object)[1], silent=TRUE)
        if (typeof(ll) == "character") {
            ll <- NA
        }	
        if ("tsls" %in% class(object))
            n <- object$n
        else if ("Arima" %in% class(object))
            n <- length(resid(object))
        else
            n <- length(case.names(object))

        if (family(object)$family == "gaussian") {
            if ("tsls" %in% class(object)) {
                p <- object$p
                n <- object$n
                rdf <- n - p
                r <- object$residuals
                f <- object$response - r
                w <- object$weights
                hasintercept <- attr(terms(object$formula), "intercept")
            }
            else {
                p <- object$rank
                n <- NROW(object$qr$qr)
                rdf <- n - p
                r <- object$residuals
                f <- object$fitted
                w <- object$weights
                hasintercept <- attr(terms(object), "intercept")
            }
            if (is.null(w)) {
                mss <- if (hasintercept) 
                           sum((f - mean(f))^2)
                       else sum(f^2)
                rss <- sum(r^2)
            }
            else {
                mss <- if (hasintercept) {
                           m <- sum(w * f/sum(w))
                           sum(w * (f - m)^2)
                       }
                       else sum(w * f^2)
                rss <- sum(w * r^2)
                r <- sqrt(w) * r
            }
            df.int <- if (hasintercept) 
                          1
                      else 0
            resvar <- rss/(length(r) - p)
            r2 <- mss/(rss+mss)
            ar2 <- 1 - (rss/(rss + mss)) * (n - df.int) / rdf
            
            fstat <- c(value = (mss/(p - df.int))/resvar, 
                       numdf = p - df.int, dendf = rdf)
        }
        else {
            r2 <- NA
            ar2 <- NA
            fstat <- NA
        }

        cfs[vn,i] <- cf
        ses[vn,i] <- se
        pvs[vn,i] <- pv
        lls <- append(lls, ll)
        ns <- append(ns, n)
        r2s <- append(r2s, r2)
        ar2s <- append(ar2s, ar2)
        pfstats <- append(pfstats, pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE))
        mns <- names(models)
    }

    return(list(vns = vns,
                cfs = cfs,
                ses = ses,
                pvs = pvs,
                lls = lls,
                ns = ns,
                r2s = r2s,
                ar2s = ar2s,
                pfstats = pfstats,
                mns = mns))
}



format.numbers <- function(x, digits, scientific = 5, logged = FALSE) {
    if (logged) {
        log.x <- floor(x/log(10))
        x <- exp(x)
    }
    else {
        log.x <- floor(log(abs(x),10))
    }
    value <- ifelse(abs(log.x) >= scientific,
                    as.character(signif(x * 10^(-log.x), digits)),
                    as.character(signif(x, digits = pmax(digits,log.x+1))))
    expon <- ifelse(abs(log.x) >= scientific,
                    as.character(log.x),
                    NA)
    return(list(value = value, expon = expon))
}

format.numbers.value <- function(x, digits, scientific = 5, logged = FALSE) {
    format.numbers(x = x, digits = digits, scientific = scientific, logged = logged)$value
}

format.numbers.expon <- function(x, digits, scientific = 5, logged = FALSE) {
    format.numbers(x = x, digits = digits, scientific = scientific, logged = logged)$expon
}


make.outtable <- function(models, replacelist=NULL,
                          p.levels = c(.05, .01, .001),
                          p.levels.labels = NULL,
                          scientific = 5,
                          digits = 3, show.se = TRUE, show.n = TRUE,
                          show.r2 = NULL, show.ar2 = NULL, show.ll = NULL,
                          vcov.command = NULL) {

    if (any(c("lm","glm", "tsls","Arima") %in% class(models))) {
        models <- list(models)
    }
    
    for (object in models) {
        if ("glm" %in% class(object)) {
            if (is.null(show.ll))
                show.ll <- TRUE
        }
        if ("Arima" %in% class(object)) {
            if (is.null(show.ll))
                show.ll <- TRUE
        }
        else if (family(object)$family == "gaussian") {
            if (is.null(show.r2))
                show.r2 <- TRUE
            if (is.null(show.ar2))
                show.ar2 <- TRUE
        }
    }
    if (is.null(show.r2))
        show.r2 <- FALSE
    if (is.null(show.ar2))
        show.ar2 <- FALSE
    if (is.null(show.ll))
        show.ll <- FALSE
    

    modelinfo <- get.modelinfo(models, vcov.command = vcov.command, replacelist = replacelist)
    
    hlines <- output <- notes <- tablelines <- NULL

    vns <- modelinfo$vns
    cfs <- modelinfo$cfs
    ses <- modelinfo$ses
    pvs <- modelinfo$pvs
    lls <- modelinfo$lls
    ns <- modelinfo$ns
    r2s <- modelinfo$r2s
    ar2s <- modelinfo$ar2s
    pfstats <- modelinfo$pfstats
    mns <- modelinfo$mns
    
                                        #  if (!is.null(replacelist)) {
                                        #    for (item in replacelist) {
                                        #      if (length(item)==2) {
                                        #        vns <- ifelse(vns==item[1], item[2], vns)
                                        #      }
                                        #    }
                                        #  }

    cfst <- apply(cfs,2,format.numbers.value,digits=digits,scientific=scientific)
    sest <- apply(ses,2,format.numbers.value,digits=digits,scientific=scientific)
    cfst.exp <- apply(cfs,2,format.numbers.expon,digits=digits,scientific=scientific)
    sest.exp <- apply(ses,2,format.numbers.expon,digits=digits,scientific=scientific)
    
    sls <- matrix(NA, nrow = (show.se+1)*(dim(cfs)[1]), ncol = dim(cfs)[2]+1)
    exs <- matrix(NA, nrow = (show.se+1)*(dim(cfs)[1]), ncol = dim(cfs)[2]+1)
    prs <- matrix(FALSE, nrow = (show.se+1)*(dim(cfs)[1]), ncol = dim(cfs)[2]+1)

    if (!is.null(mns)) {
        tablematrix <- c("", mns)
        hlines <- c(0,1,1)
    }
    else {
        tablematrix <- NULL
        hlines <- 0
    }
    
    p.levels.rank <- rank(-p.levels, ties.method="first")
    p.levels <- p.levels[p.levels.rank]
    if (length(p.levels.labels) == length(p.levels))
        p.levels.labels <- p.levels.labels[p.levels.rank]
    else
        p.levels.labels <- sapply(p.levels.rank, function(x) paste(rep("*",x),collapse=""))

    for (i in 1:(dim(cfs)[1])) {
        tline <- vns[i]
        for (j in 1:(dim(cfs)[2])) {
            sl <- sum(pvs[i,j]<p.levels)
            if (is.na(cfst[i,j]))
                tline <- c(tline, "--")
            else {
                if (sl>0) {
                    sls[(show.se+1)*i-show.se,j+1] <- p.levels.labels[sl]
                }
                tline <- c(tline,cfst[i,j])
                exs[(show.se+1)*i-show.se,j+1] <- cfst.exp[i,j]
            }
        }
        tablematrix <- rbind(tablematrix, tline)

        if (show.se) {
            tline <- ""
            for (j in 1:(dim(cfs)[2])) {
                if (is.na(cfst[i,j]) | is.na(sest[i,j]))
                    tline <- c(tline, "")
                else {
                    tline <- c(tline,sest[i,j])
                    exs[(show.se+1)*i,j+1] <- sest.exp[i,j]
                    prs[(show.se+1)*i,j+1] <- TRUE
                }
            }
        }
        tablematrix <- rbind(tablematrix, tline)
    }

    hlines <- c(hlines,dim(tablematrix)[1])
    
    if (show.n) {
        tablematrix <- rbind(tablematrix, c("N", ns))
        sls <- rbind(sls, NA)
        exs <- rbind(exs, NA)
        prs <- rbind(prs, FALSE)
    }

    if (show.r2) {
        tablematrix <- rbind(tablematrix,
                             c("R", ifelse(is.na(r2s), "--", format.numbers.value(r2s, digits = digits, scientific = scientific))))
        sls <- rbind(sls, c(2, rep(NA, dim(sls)[2]-1)))
        exs <- rbind(exs, c(NA, format.numbers.expon(r2s, digits = digits, scientific = scientific)))
        prs <- rbind(prs, FALSE)
    }

    if (show.ar2) {
        tablematrix <- rbind(tablematrix,
                             c("Adjusted R", ifelse(is.na(ar2s), "--", format.numbers.value(ar2s, digits = digits, scientific = scientific))))
        sls <- rbind(sls, c(2, rep(NA, dim(sls)[2]-1)))
        exs <- rbind(exs, c(NA, format.numbers.expon(ar2s, digits = digits, scientific = scientific)))
        prs <- rbind(prs, FALSE)
    }

    if (show.ll) {
        tablematrix <- rbind(tablematrix,
                             c("Log Likelihood", ifelse(is.na(lls), "--", format.numbers.value(lls, digits = digits, scientific = scientific))))
        sls <- rbind(sls, NA)
        exs <- rbind(exs, c(NA, format.numbers.expon(lls, digits = digits, scientific = scientific)))
        prs <- rbind(prs, FALSE)
    }

    if (!is.null(mns)) {
        sls <- rbind(NA, sls)
        exs <- rbind(NA, exs)
        prs <- rbind(FALSE, prs)
    }

    hlines <- c(hlines, dim(tablematrix)[1])

    if (show.se)
        notes <- c(notes, "Standard errors in parentheses.")
    if (length(p.levels) > 0)
        notes <- c(notes, paste(paste(paste("<sup>",p.levels.labels,"</sup>",sep=""), "<i>p</i> < ", p.levels, sep = "", collapse = ", "), ".", sep=""))

    return(list(tablematrix = tablematrix, sls = sls,
                exs = exs, prs = prs, hlines = hlines,
                has.mns = !is.null(mns), notes = notes))
    
}

outtable.rtf <- function(models, file, replacelist=list(c("(Intercept)","Intercept")), ...) {
    intro <- "{\\rtf1\\ansi\\deff11\\deflang1024
{\\fonttbl{\\f3\\fnil\\fcharset0 Zapf Chancery;}
{\\f4\\fnil\\fcharset204 Zapf Chancery;}
{\\f5\\fnil\\fcharset204 Times New Roman;}
{\\f6\\fnil\\fcharset204 Arial;}
{\\f7\\fnil\\fcharset204 Arial;}
{\\f8\\fnil\\fcharset204 Courier New;}
{\\f9\\fnil\\fcharset2 Symbol;}
{\\f10\\fnil\\fcharset0 MT Extra;}
{\\f11\\fnil\\fcharset0 Times New Roman;}
{\\f12\\fnil\\fcharset0 Book Antiqua;}
{\\f13\\fnil\\fcharset0 Bookman Old Style;}
{\\f14\\fnil\\fcharset0 Century Gothic;}
{\\f15\\fnil\\fcharset0 New Century Schoolbook;}
{\\f16\\fnil\\fcharset0 Arial;}
{\\f17\\fnil\\fcharset0 Arial;}
{\\f18\\fnil\\fcharset0 Courier New;}"

    intro2 <- "}
{\\colortbl;
\\red0\\green0\\blue0;
\\red0\\green0\\blue255;
\\red0\\green255\\blue255;
\\red0\\green255\\blue0;
\\red255\\green0\\blue255;
\\red255\\green0\\blue0;
\\red255\\green255\\blue0;
\\red255\\green255\\blue255;
\\red0\\green0\\blue128;
\\red0\\green128\\blue128;
\\red0\\green128\\blue0;
\\red128\\green0\\blue128;
\\red128\\green0\\blue0;
\\red128\\green128\\blue0;
\\red128\\green128\\blue128;
\\red192\\green192\\blue192;
}
{\\stylesheet
{\\s0\\fs20\\snext0 Normal;}
{\\s2\\ql\\sb240\\sa60\\keepn\\f19\\b\\fs40 \\sbasedon0\\snext0 heading 1;}
{\\s6\\ql\\sb240\\sa60\\keepn\\f19\\b\\fs24 \\sbasedon0\\snext0 heading 5;}
{\\s1\\qc\\sb240\\sa60\\keepn\\f19\\b\\fs40 \\sbasedon0\\snext0 part;}
{\\s3\\ql\\sb240\\sa60\\keepn\\f19\\b\\fs32 \\sbasedon0\\snext0 heading 2;}
{\\s7\\ql\\sb240\\sa60\\keepn\\f19\\b\\fs24 \\sbasedon0\\snext0 heading 6;}
{\\s4\\ql\\sb240\\sa60\\keepn\\f19\\b\\fs32 \\sbasedon0\\snext0 heading 3;}
{\\s5\\ql\\sb240\\sa60\\keepn\\f19\\b\\fs24 \\sbasedon0\\snext0 heading 4;}
}"

    concl <- "}}}}"

                                        #"

    outinfo <- make.outtable(models, replacelist=replacelist, ...)
    tablematrix <- outinfo$tablematrix
    sls <- outinfo$sls
    has.mns <- outinfo$has.mns
    exs <- outinfo$exs
    prs <- outinfo$prs
    notes <- outinfo$notes
    hlines <- outinfo$hlines

    brdrtype <- c("\\brdrs", "\\brdrdb", "\\brdrtriple", "\\brdrth")

    paperw <- 12280

    cellwidths <- 125*c(max(sapply(tablematrix[,1], nchar, keepNA=FALSE)),
                        max(sapply(tablematrix[,1], nchar, keepNA=FALSE))+
                        max(nchar(tablematrix[,-1], keepNA=FALSE) + nchar(sls[,-1], keepNA=FALSE) + 2*prs[,-1])*(1:(dim(tablematrix)[2]-1)))

    if (max(cellwidths) >= paperw)
        paperw <- max(cellwidths) + 100

    sidew <- floor((paperw-max(cellwidths))/2)

    innerintro <- paste("\\paperw", paperw,
                        "\\paperh15900\\margl",sidew,
                        "\\margr",sidew,
                        "\\margt2540\\margb1760",
                        "\\pgnstart0\\widowctrl\\qj\\ftnbj\n{{{{",
                        sep = "")
    
    olines <- NULL
    
    for (i in 1:(dim(tablematrix)[1])) {
        tline <- "\\trowd"
        cellbrdr <- ""
        if ((i ==1) & (0 %in% hlines))
            cellbrdr <- paste("\\clbrdrt", brdrtype[sum(hlines==0)],sep="")
        if (i %in% hlines) {
            cellbrdr <- paste(c(cellbrdr,"\\clbrdrb", brdrtype[sum(hlines==i)]),collapse="")
        }
        tline <- c(tline, paste(cellbrdr,"\\cellx",
                                cellwidths,
                                sep=""))
        olines <- append(olines, paste(tline, collapse=""))

        for (j in 1:(dim(tablematrix)[2])) {
            if (is.na(tablematrix[i,j]))
                tline <- c("\\pard\\intbl","\\qc {{", "}}\\cell")
            else
                tline <- c("\\pard\\intbl",
                           ifelse(j == 1, "\\ql {{\\i ", "\\qc {{"),
                           ifelse(prs[i,j], "(", ""),
                           ifelse(tablematrix[i,j] == "--", "\\emdash", tablematrix[i,j]),
                           ifelse(is.na(exs[i,j]), "",
                                  paste("x10{\\super ",exs[i,j]," }",sep="")),
                           ifelse(prs[i,j], ")", ""),
                           ifelse(is.na(sls[i,j]), "", paste("{\\super ",sls[i,j],"}",sep="")),
                           "}}\\cell")
            olines <- append(olines, paste(tline, collapse=""))
        }
        olines <- append(olines, "\\row")
    }

    tabconcl <- "}"

    if (length(notes)>0) {
        notes <- gsub("<i>","{\\\\i ",notes)
        notes <- gsub("</i>","}",notes)
        notes <- gsub("<sup>","{\\\\super ",notes)
        notes <- gsub("</sup>","}",notes)
        nlines <- paste("\\ql\\fi0 {", notes, "}\\par", sep="")
    }
    else
        nlines <- NULL
    output <- paste(c(intro, intro2, innerintro, olines, tabconcl, nlines, concl), collapse = "\n")
    cat(output, file = file)
}


outtable.tex <- function(models, file, replacelist=list(c("(Intercept)","Intercept")), caption=NULL, label=NULL, tabularonly = F, notesintable = T, ...) {

    if (tabularonly) {
        intro <- concl <- ""
    }
    else {
        intro <- "\\begin{table}\n"
        if (!is.null(caption))
            intro <- paste(intro,"\\caption{",caption,"}\n",sep="")
        
        if (!is.null(label))
            intro <- paste(intro, "\\label{", label, "}\n",sep="")
        
        concl <- "\\end{table}"
    }

                                        #"

    outinfo <- make.outtable(models, replacelist=replacelist, ...)
    tablematrix <- outinfo$tablematrix
    sls <- outinfo$sls
    has.mns <- outinfo$has.mns
    exs <- outinfo$exs
    prs <- outinfo$prs
    notes <- outinfo$notes
    hlines <- outinfo$hlines

    repcenter <- paste(rep("c",dim(tablematrix)[2]-1),sep="",collapse="")

    intro2 <- paste("\\begin{center}\n\\begin{tabular}{l",repcenter,"}\n", sep="")

    olines <- NULL

    for (i in 1:(dim(tablematrix)[1])) {
        if ((i ==1) & (0 %in% hlines))
            olines <- append(olines, "\\hline\n")

        tline <- NULL
        
        for (j in 1:(dim(tablematrix)[2])) {
            if (is.na(tablematrix[i,j]))
                tline <- append(tline, "")
            else
                tline <- append(tline, paste(ifelse(j == 1, "\\emph{", ""),
                                             ifelse(prs[i,j], "(", ""),
                                             ifelse(is.na(exs[i,j]), "", "$"),
                                             ifelse(tablematrix[i,j] == "--", "---", tablematrix[i,j]),
                                             ifelse(is.na(exs[i,j]), "", paste("\\times 10^{", exs[i,j], "}$", sep="")),
                                             ifelse(is.na(sls[i,j]), "", paste("$^{\\textrm{\\footnotesize ",sls[i,j],"}}$",sep="")),
                                             ifelse(prs[i,j], ")", ""),
                                             ifelse(j == 1, "}", ""),
                                             sep = ""))

        }
        olines <- append(olines, paste(paste(tline, collapse=" & "), "\\\\\n", sep=""))

        if (i %in% hlines)
            olines <- append(olines, "\\hline\n")

    }

    tabconcl <- "\\end{tabular}\n\\end{center}\n"

    if (length(notes)>0) {
        notes <- gsub("<i>","\\\\emph{",notes)
        notes <- gsub("</i>","}",notes)
        notes <- gsub("<sup>","$^{\\\\textrm{\\\\footnotesize ",notes)
        notes <- gsub("</sup>","}}$",notes)
        notes <- gsub(" < "," $<$ ",notes)
        if (notesintable)
            nlines <- paste("\\multicolumn{",dim(tablematrix)[2],"}{l}{", notes, "}\\\\", sep="")
        else
            nlines <- paste(notes, "\n", sep="")
    }
    else
        nlines <- NULL
    if (notesintable)
        output <- paste(c(intro, intro2, olines, nlines, tabconcl, concl), collapse = "\n")
    else
        output <- paste(c(intro, intro2, olines, tabconcl, nlines, concl), collapse = "\n")

    cat(output, file = file)
}
