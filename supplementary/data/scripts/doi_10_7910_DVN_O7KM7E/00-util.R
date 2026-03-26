#' Ensure the working directory contains desired subdirectories
#' 
#' For each desired subdirectory, the function checks if the subdirectory
#' already exists, and if it does not, creates it.
#' 
#' @param needed_dirs A character vector giving the desired subdirectories
#' 
#' @return Invisibly returns a character vector of the directories the function
#'     created because they were not present
prepare_directories <- function(needed_dirs = c("output", "plots")) {
    created_dirs <- character()
    for ( directory in needed_dirs ) {
        if ( dir.exists(directory) ) {
            next()
        } else {
            dir.create(directory)
            created_dirs <- c(created_dirs, directory)
        }
    }
    return(invisible(created_dirs))
}


#' Fix case citations to avoid filename problems
#' 
#' Takes LEXIS citations to U.S. Supreme Court precedents and removes periods
#' and changes spaces (" ") to dashes ("-").
#' 
#' @param citations A character vector of case citations
#' 
#' @return A character vector of citations with periods removed and spaces (" ")
#'     changed to dashes ("-")
fix_citation <- function(citations) {
    ## Alter the case citation so result files don't have spaces in the names,
    citations <- gsub(pattern = " ",  replacement = "-", x = citations)
    ## as well as eliminating the periods in U.S.
    citations <- gsub(pattern = "\\.", replacement = "", x = citations)
    return(citations)
}


#' Convenience function giving the sum of the absolute value of a vector
#' 
#' @param x A numeric vector
#' @param ... Additional arguments to pass to sum()
#' 
#' @return A numeric vector of length one equal to sum(abs(x), ...)
sum_abs <- function(x, ...) {
    return(sum(abs(x), ...))
}


#' Determine whether one variable Granger causes another from a table of coefficient signs
#' 
#' @param data A data frame giving the coefficient signs
#' @param var1 A character vector of length one giving the variable that may
#'     Granger cause the other variable
#' @param var2 A character vector of length one giving the variable that may be
#'     Granger caused by the other variable
#' 
#' @return A logical vector of length one indicating whether var1 Granger
#'     causes var2
granger_cause <- function(data, var1, var2) {
    other_vars <- setdiff(c("SC", "AC", "DC"), var1)
    v1_reliable <- abs(data[data[["var"]] == var1, var2])
    others_reliable <- any(abs(data[data[["var"]] %in% other_vars, var2]))
    return( v1_reliable & !others_reliable)
}

#' Determine if a set of lag coefficients exhibits a top-down or bottom-up pattern
#' 
#' @param data A data frame giving the coefficient signs
#' @param pattern A character vector of length one, should be either "top-down"
#'     or "bottom-up"
#' 
#' @return A logical vector of length one indicating whether the lag
#'     coefficients exhibit the pattern requested
has_pattern <- function(data, pattern) {
    if ( pattern == "top-down" ) {
        return(
            data$AC[data$var == "SC"]
            & data$DC[data$var == "AC"]
            & !data$SC[data$var == "AC"]
            & !data$SC[data$var == "DC"]
            & !data$AC[data$var == "DC"]
        )
    } else if ( pattern == "bottom-up" ) {
        return(
            data$SC[data$var == "AC"]
            & data$AC[data$var == "DC"]
            & !data$DC[data$var == "SC"]
            & !data$DC[data$var == "AC"]
            & !data$AC[data$var == "SC"]
        )
    } else {
        stop("pattern should be one of 'top-down' or 'bottom-up'")
    }
}



## Convenience plotting functions
#' Create a plot with no axis ticks or labels
#' 
#' @param ... Arguments passed to plot()
trimplot <- function(...) {
    plot(..., xaxt = "n", yaxt = "n", xlab = "", ylab = "")
}
#' Draw vertical whiskers on a plot
#' 
#' @param x The x coordinate of the vertical line
#' @param y1 The starting point for the whisker on the y axis
#' @param y2 The ending point for the whisker on the y axis
#' @param col The color of the whisker
whisker <- function(x, y1, y2, col) {
    arrows(x, y1, x, y2, length = 0.05, angle = 90, code = 3, col = col)
}
#' Title and label an axis, very close to the plot
#' 
#' @param side Which axis?
#' @param axis_label Label for the axis
#' @param tick_labels Should axis ticks be labeled? Default is TRUE.
#'     Alternatively a vector of labels for axis ticks
#' @param ticks_at Where should axis ticks be drawn? If NULL, the default,
#'     no axis ticks are drawn
close_axis <- function(side, axis_label, tick_labels = TRUE, ticks_at = NULL) {
    axis(side, tick = FALSE, line = -0.75, labels = tick_labels, at = ticks_at)
    mtext(axis_label, side, line = 1.5)
}
