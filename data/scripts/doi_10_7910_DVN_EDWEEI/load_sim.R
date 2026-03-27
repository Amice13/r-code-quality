load_sim <- function(path, var_name, sea = FALSE) {
  require(data.table)
  df <- data.table::fread(path, header = FALSE)
  names(df) <- gsub("V", paste0(var_name, "_"), names(df))
  if (isFALSE(sea)) {
    df <- df[, c(seq(25, 400, 25))]
  }
  else {
    df <- df[, c(seq(25, 200, 25))]
  }
  return(df)
}