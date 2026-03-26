#' Statistical Helper Functions for Summary Tables
#' 
#' Consistent statistical functions with NA handling for use across all analyses

# Basic descriptive statistics with NA removal
Mean <- function(x) {mean(x, na.rm = TRUE)}
SD <- function(x) {sd(x, na.rm = TRUE)}
Min <- function(x) {min(x, na.rm = TRUE)}
Max <- function(x) {max(x, na.rm = TRUE)}

# Missing data functions - multiple names for compatibility
Missing <- function(x) {sprintf("%.0f", sum(is.na(x)))}
`NA` <- function(x) {sprintf("%.0f", sum(is.na(x)))}  # Alternative name
NA_formatted <- function(x) {format(sum(is.na(x)), nsmall = 0)}  # Alternative format
