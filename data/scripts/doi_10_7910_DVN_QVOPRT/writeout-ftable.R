writeout.ftable <-
function (x, file = "", quote = TRUE, digits = getOption("digits"))
{
    if (!inherits(x, "ftable"))
        stop("'x' must be an \"ftable\" object")
    ox <- x
    charQuote <- function(s) if (quote)
        paste("\"", s, "\"", sep = "")
    else s
    makeLabels <- function(lst) {
        lens <- sapply(lst, length)
        cplensU <- c(1, cumprod(lens))
        cplensD <- rev(c(1, cumprod(rev(lens))))
        y <- NULL
        for (i in rev(seq(along = lst))) {
            ind <- 1 + seq(from = 0, to = lens[i] - 1) * cplensD[i +
                1]
            tmp <- character(length = cplensD[i])
            tmp[ind] <- charQuote(lst[[i]])
            y <- cbind(rep(tmp, times = cplensU[i]), y)
        }
        y
    }
    makeNames <- function(x) {
        nmx <- names(x)
        if (is.null(nmx))
            nmx <- rep("", length.out = length(x))
        nmx
    }
    xrv <- attr(x, "row.vars")
    xcv <- attr(x, "col.vars")
    LABS <- cbind(rbind(matrix("", nr = length(xcv), nc = length(xrv)),
        charQuote(makeNames(xrv)), makeLabels(xrv)), c(charQuote(makeNames(xcv)),
        rep("", times = nrow(x) + 1)))
    DATA <- rbind(if (length(xcv))
        t(makeLabels(xcv)), rep("", times = ncol(x)), format(unclass(x),
        digits = digits))
    x <- cbind(apply(LABS, 2, format, justify = "left"), apply(DATA,
        2, format, justify = "left"))
    cat(t(x), file = file, sep = c(rep("\t", ncol(x) - 1), "\n"))
    invisible(ox)
}
