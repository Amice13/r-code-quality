

# LABELS -------------------------------------
uncorrected_lab <- "Off-the-shelf"
corrected_lab <- "Corrected"


# COLORS -------------------------------------
corrected_col <- "grey70"
invalid_col <- "grey95"
x_col <- "#FF3A20"

bias_col <- "tomato"
axis_col <- "grey30"


ss_col <- "#2F4172"


insig_color <- "grey75"
m_color <- "#2F4172"
s_color <- "#FF3A20"

# VOTE CHOICE COLORS
dem_col <- "#3333FF"
rep_col <- "#FF3333"


# FUNCTIONS -----------------------------------------

# FUNCTION FOR ROUNDING AXIS LABELS
fmt_dcimals <- function(decimals = 0){
  # return a function responsible for formatting the
  # axis labels with a given number of decimals
  function(x) as.character(round(x, decimals))
}

# FUNCTION TO REMOVE LEADING ZERO
no_zero <- function(x, decimals) {
  y <- as.character(round(x, decimals))
  y[x > 0 & x < 1] <- sprintf('.%s',x[x > 0 & x < 1]*10)
  y[x == 0] <- '0'
  y[x > -1 & x < 0] <- sprintf('-.%s',x[x > -1 & x < 0]*-10)
  y
}


