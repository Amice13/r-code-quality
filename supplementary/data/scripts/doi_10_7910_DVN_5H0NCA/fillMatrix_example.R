library(tidyverse)
library(fillMatrix)
library(jpeg)

library(abind)
library(ggimg)
library(cowplot)
library(magrittr)

theme_set(theme_nothing())

gruvbox <- c(dark0_hard = "#1d2021", dark0 = "#282828", dark0_soft = "#32302f",
dark1 = "#3c3836", dark2 = "#504945", dark3 = "#665c54", dark4 = "#7c6f64",
dark4_256 = "#7c6f64", gray_245 = "#928374", gray_244 = "#928374",
light0_hard = "#f9f5d7", light0 = "#fbf1c7", light0_soft = "#f2e5bc",
light1 = "#ebdbb2", light2 = "#d5c4a1", light3 = "#bdae93", light4 = "#a89984",
light4_256 = "#a89984", bright_red = "#fb4934", bright_green = "#b8bb26",
bright_yellow = "#fabd2f", bright_blue = "#83a598", bright_purple = "#d3869b",
bright_aqua = "#8ec07c", bright_orange = "#fe8019", neutral_red = "#cc241d",
neutral_green = "#98971a", neutral_yellow = "#d79921", neutral_blue = "#458588",
neutral_purple = "#b16286", neutral_aqua = "#689d6a", neutral_orange = "#d65d0e",
faded_red = "#9d0006", faded_green = "#79740e", faded_yellow = "#b57614",
faded_blue = "#076678", faded_purple = "#8f3f71", faded_aqua = "#427b58",
faded_orange = "#af3a03")

save_test <- function(img) { storage.mode(img) <- "double"; writeJPEG(img, "test.jpg") }

get_start_points <- function(max_pixel)
{
  nc <- ncol(max_pixel)
  nr <- nrow(max_pixel)
  start_row <- c()
  start_col <- c()

  # Top row of points
  index <- which(max_pixel[1,] == 1)
  if (length(index))
  {
    index <- index[which(!duplicated(cumsum(c(0, diff(index)) > 1)))]
    index[(index[-1] - 1L) != index[-length(index)]]
    start_row <- c(start_row, rep(1L, length(index)))
    start_col <- c(start_col, index)    
  }

  # Bottom row of points
  index <- which(max_pixel[nr,] == 1)
  if (length(index))
  {
    index <- index[which(!duplicated(cumsum(c(0, diff(index)) > 1)))]
    index[(index[-1] - 1L) != index[-length(index)]]
    start_row <- c(start_row, rep(nr, length(index)))
    start_col <- c(start_col, index)
  }

  # First col of points
  index <- which(max_pixel[,1] == 1)
  if (length(index))
  {
    index <- index[which(!duplicated(cumsum(c(0, diff(index)) > 1)))]
    index[(index[-1] - 1L) != index[-length(index)]]
    start_row <- c(start_row, index)
    start_col <- c(start_col, rep(1L, length(index)))
  }

  # Last col of points
  index <- which(max_pixel[,nc] == 1)
  if (length(index))
  {
    index <- index[which(!duplicated(cumsum(c(0, diff(index)) > 1)))]
    index[(index[-1] - 1L) != index[-length(index)]]
    start_row <- c(start_row, index)
    start_col <- c(start_col, rep(nc, length(index)))
  }

  return(tibble(start_row = start_row, start_col = start_col))
}

detect_panels <- function(img)
{
  # DETECT THE WHITE BITS
  max_pixel <- apply(img, c(1, 2), max)
  max_pixel <- (max_pixel > 0.85)
  storage.mode(max_pixel) <- "integer"

  spts <- get_start_points(max_pixel)

  all_white <- fillMatrix(max_pixel, spts$start_row, spts$start_col)

  currently_known <- all_white
  output <- list()
  while (TRUE)
  {
    first_unknown <- which(currently_known == 0)[1]
    if (is.na(first_unknown)) { break }
    fn_row <- row(currently_known)[first_unknown]
    fn_col <- col(currently_known)[first_unknown]

    img_to_fill <- 1 - currently_known
    new_area <- fillMatrix(img_to_fill, fn_row, fn_col)
    output <- c(output, list(new_area))
    currently_known <- currently_known + new_area
  }

  sizes <- map_int(output, ~ sum(..1))
  output <- output[sizes > length(img) * 0.005]
  sizes <- sizes[sizes > length(img) * 0.005]

  df <- tibble(size = sizes, xmin = 0, xmax = 0, ymin = 0, ymax = 0)
  for (j in seq_len(nrow(df)))
  {
    index <- which(output[[j]] == 1)
    res <- range(col(all_white)[index])
    df$xmin[j] <- res[1]
    df$xmax[j] <- res[2]
    res <- range(row(all_white)[index])
    df$ymin[j] <- res[1]
    df$ymax[j] <- res[2]
  }

  return(df)
}

add_panel_numbers <- function(panels)
{
  delta_x <- min(panels$xmax - panels$xmin) * 0.75
  delta_y <- min(panels$ymax - panels$ymin) * 0.75
  panels <- arrange(panels, ymin)
  panels$ytemp <- cumsum(c(0, diff(panels$ymin)) > delta_y)
  panels <- arrange(panels, xmin)
  panels$xtemp <- cumsum(c(0, diff(panels$xmin)) > delta_x)
  panels <- arrange(panels, ytemp, xtemp)
  panels$panel_id <- seq_len(nrow(panels))

  panels <- mutate(panels, image_path = basename(path), nx = ncol(img), ny = nrow(img))
  panels <- select(panels, image_path, everything())
  panels <- select(panels, -xtemp, -ytemp)
  panels
}