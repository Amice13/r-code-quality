# functions.R
# J.D. Hogan
# Systems Biology Team
# MRL Exploratory Science Center
# Script containing the local functions used in analyze.R

# FROM CYTOFKIT
# Noise reduced arsinh transformation
#
# Inverse hyperbolic sine transformation (arsinh) with a cofactor of 5, reduce
# noise from negative values
#
# Adopted from Plos Comp reviewer
cytofAsinh <- function(value, cofactor=150) {
  value <- value - 1
  loID <- which(value < 0)
  if (length(loID) > 0)
    value[loID] <- rnorm(length(loID), mean = 0, sd = 0.01)
  value <- value / cofactor
  value <- asinh(value)
  return(value)
}

# altered function for creating daFrame
prepData2 <- function(x, panel, md, features=NULL, cofactor=5,
                      panel_cols = list(channel = "fcs_colname",
                                        antigen = "antigen",
                                        class = "marker_class"),
                      md_cols = list(file = "file_name",
                                     id = "sample_id",
                                     factors = c("condition", "patient_id"))) {
  # check validity of input arguments
  if (!is(panel, "data.frame")) {
    panel <- data.frame(panel, check.names = FALSE, stringsAsFactors = FALSE)
  }
  
  if (!is(md, "data.frame")) {
    md <- data.frame(md, check.names = FALSE, stringsAsFactors = FALSE)
  }
  
  stopifnot(is.list(panel_cols), is.list(md_cols),
            c("channel", "antigen") %in% names(panel_cols),
            c("file", "id", "factors") %in% names(md_cols))
  
  if (!is.null(cofactor))
    stopifnot(is.numeric(cofactor), length(cofactor) == 1, cofactor > 0)
  
  if (is(x, "flowSet")) {
    fs <- x
  } else if (is.character(x)) {
    stopifnot(dir.exists(x))
    fcs <- list.files(x, ".fcs$", full.names = TRUE, ignore.case = TRUE)
    if (length(fcs) < 2) {
      stop("The specified directory contains",
           " none or only a single FCS file.")
    }
    
    stopifnot(all(vapply(fcs, isFCSfile, logical(1))))
    fs <- read.flowSet(fcs, transformation = F, truncate_max_range = F)
  } else {
    stop("Invalid argument 'x'; should be either a flowSet",
         " or a character string specifying the path to",
         " a directory containing a set of FCS files.")
  }
  
  # check channels listed in panel
  stopifnot(panel[[panel_cols$channel]] %in% colnames(fs))
  
  if (is.null(features)) {
    # default to channels listed in panel
    features <- as.character(panel[[panel_cols$channel]])
  } else {
    # check validity of 'features'
    chs <- colnames(fs)
    check1 <- is.logical(features) && length(features) == length(chs)
    check2 <- is.integer(features) && all(features %in% seq_along(chs))
    check3 <- all(features %in% chs)
    if (!any(check1, check2, check3))
      stop("Invalid argument 'features'. Should be either",
           " a logial vector,\n  a numeric vector of indices, or",
           " a character vector of column names.")
  }
  
  # use identifiers if filenames are not specified
  ids <- c(keyword(fs, "FILENAME"))
  if (is.null(unlist(ids))) {
    ids <- c(fsApply(fs, identifier))
  }
  
  # check that filenames match b/w flowSet & metadata &
  # reorder flowSet according to metadata table
  stopifnot(all(ids %in% md[[md_cols$file]]))
  fs <- fs[match(ids, md[[md_cols$file]])]
  
  # assure correctness of formats
  k <- c(md_cols$id, md_cols$factors)
  md <- data.frame(md)[, k] %>%
    mutate_all(factor) %>%
    dplyr::rename("sample_id" = md_cols$id)
  o <- order(md[[md_cols$factors[1]]])
  md$sample_id <- factor(md$sample_id, levels = md$sample_id[o])
  
  # replace problematic characters
  antigens <- panel[[panel_cols$antigen]]
  antigens <- gsub("-", "_", antigens)
  antigens <- gsub(":", ".", antigens)
  
  # column subsetting
  fs <- fs[, features]
  chs0 <- colnames(fs)
  
  # replace channel w/ antigen names
  m1 <- match(panel[[panel_cols$channel]], chs0, nomatch = 0)
  m2 <- match(chs0, panel[[panel_cols$channel]], nomatch = 0)
  flowCore::colnames(fs)[m1] <- antigens[m2]
  chs <- colnames(fs)
  
  # get exprs.
  es <- matrix(fsApply(fs, exprs), byrow = T,
               nrow = length(chs), dimnames = list(chs, NULL))
  # get nb. of cells per sample
  md$n_cells <- as.numeric(fsApply(fs, nrow))
  
  # get & check marker classes if provided
  valid_mcs <- c("type", "state", "none")
  if (is.null(panel_cols$class)) {
    mcs <- factor("none", levels = valid_mcs)
  } else {
    mcs <- factor(panel[[panel_cols$class]], levels = valid_mcs)
    mcs <- mcs[match(chs0, panel[[panel_cols$channel]])]
    if (any(is.na(mcs)))
      stop("Invalid marker classes detected.",
           " Valid classes are 'type', 'state', and 'none'.")
  }
  
  # construct row & column data
  rd <- DataFrame(
    row.names = chs, channel_name = chs0,
    marker_name = chs, marker_class = mcs)
  
  k <- setdiff(names(md), "n_cells")
  cd <- DataFrame(lapply(md[k], function(u) {
    v <- as.character(rep(u, md$n_cells))
    factor(v, levels = levels(u))
  }), row.names = NULL)
  
  # construct SCE
  SingleCellExperiment(
    assays = list(exprs = es), rowData = rd, colData = cd,
    metadata = list(experiment_info = md, cofactor = cofactor))
}

# re-create runDR function from prior version of CATALYST
runDR <- function(x, dr=c("UMAP", "TSNE", "PCA", "MDS", "DiffusionMap"),
                  cells=NULL, features=NULL, assay="exprs", ...) {
  # load scater library to be able to run these functions
  suppressMessages(library(scater))
  
  dr <- match.arg(dr)
  stopifnot(
    is.character(assay),
    length(assay) == 1,
    assay %in% assayNames(x))
  if (!is.null(features)) {
    stopifnot(is.character(features))
    if (length(features) == 1) {
      features <- match.arg(features, c("type", "state", "none"))
      features <- rownames(x)[rowData(x)$marker_class == features]
      if (length(features) == 0)
        stop("No features matched the specified marker class.")
    } else {
      stopifnot(features %in% rownames(x))
    }
    # use all features
    features <- rownames(x)
  }
  
  if (!is.null(cells)) {
    stopifnot(
      is.numeric(cells), length(cells) == 1,
      as.integer(cells) == cells, cells > 0)
    # split cell indices by sample
    cs <- split(seq_len(ncol(x)), paste0(x$sample_id, "_"))
    # sample at most 'n' cells per sample
    cs <- unlist(lapply(cs, function(u)
      sample(u, min(cells, length(u)))))
  } else {
    # use all cells
    cs <- TRUE
  }
  
  # run dimension reduction
  fun <- get(paste0("run", dr))
  y <- fun(x[, cs], subset_row = features, exprs_values = assay, ...)
  
  # return SCE when no cell subsetting has been done
  if (is.null(cells)) return(y)
  
  # else, write coordinates into original SCE
  xy <- reducedDim(y, dr)
  m <- matrix(NA, nrow = ncol(x), ncol = ncol(xy))
  m[cs, ] <- xy
  reducedDim(x, dr) <- m
  x
}
