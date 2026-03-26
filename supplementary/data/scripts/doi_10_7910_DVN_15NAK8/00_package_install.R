#----------------------------------------------------------------------------------------------------#
# Filename: 00_package_install.R
# File purpose: Import extra libraries
#----------------------------------------------------------------------------------------------------#
'%!in%' <- function(x,y)!('%in%'(x,y))

# store installed packages
install_packages <- installed.packages()

# check if needed packages are installed
pkgs <- c("devtools", "jtools", "ggplot2", "lavaan", "OpenMx",
          "Matrix", "modelsummary", "survey", "survival", "tidySEM",
          "haven", "RcppEigen", "StanHeaders", "rpf", "minqa", "kableExtra",
          "pkgbuild", "miniUI", "httpuv", "rlang", "fs"
          )
pkg_yes <- pkgs %in% install_packages[,1]

install_packages[which(install_packages[, 1] %in% pkgs), 3]

# install devtools if not installed
if("devtools" %!in% install_packages[, 1]){
  install.packages("devtools")
}

# install jtools if not installed
if("jtools" %!in% install_packages[, 1]){
  devtools::install_version("jtools", version = "2.2.2", repos = "http://cran.us.r-project.org")
}

# install ggplot2 if not installed
if("ggplot2" %!in% install_packages[, 1]){
  devtools::install_version("ggplot2", version = "3.5.1", repos = "http://cran.us.r-project.org")
}

# install lavaan if not installed
if("lavaan" %!in% install_packages[, 1]){
  devtools::install_version("lavaan", version = "0.6-16", repos = "http://cran.us.r-project.org")
}

# install OpenMX if not installed
if("OpenMx" %!in% install_packages[, 1]){
  devtools::install_version("OpenMx", version = "2.21.11", repos = "http://cran.us.r-project.org")
}

# install Matrix if not installed
if("Matrix" %!in% install_packages[, 1]){
  devtools::install_version("Matrix", version = "1.6-1.1", repos = "http://cran.us.r-project.org")
}

# install modelsummary if not installed
if("modelsummary" %!in% install_packages[, 1]){
  devtools::install_version("modelsummary", version = "1.4.3", repos = "http://cran.us.r-project.org")
}

# install survey if not installed
if("survey" %!in% install_packages[, 1]){
  devtools::install_version("survey", version = "4.2-1", repos = "http://cran.us.r-project.org")
}

# install survival if not installed
if("survival" %!in% install_packages[, 1]){
  devtools::install_version("survival", version = "3.5-7", repos = "http://cran.us.r-project.org")
}

# install tidySEM if not installed
if("tidySEM" %!in% install_packages[, 1]){
  devtools::install_version("tidySEM", version = "0.2.6", repos = "http://cran.us.r-project.org")
}


# install haven if not installed
if("haven" %!in% install_packages[, 1]){
  devtools::install_version("haven", version = "2.5.3", repos = "http://cran.us.r-project.org")
}

# install RcppEigen if not installed
if("RcppEigen" %!in% install_packages[, 1]){
  devtools::install_version("RcppEigen", version = "0.3.4.0.1", repos = "http://cran.us.r-project.org")
}
# install StanHeaders if not installed
if("StanHeaders" %!in% install_packages[, 1]){
  devtools::install_version("StanHeaders", version = "2.26.28", repos = "http://cran.us.r-project.org")
}
# install rpf if not installed
if("rpf" %!in% install_packages[, 1]){
  devtools::install_version("rpf", version = "1.0.14", repos = "http://cran.us.r-project.org")
}
# install minqa if not installed
if("minqa" %!in% install_packages[, 1]){
  devtools::install_version("minqa", version = "1.2.7", repos = "http://cran.us.r-project.org")
}
# install kableExtra if not installed
if("kableExtra" %!in% install_packages[, 1]){
  devtools::install_version("kableExtra", version = "1.3.4", repos = "http://cran.us.r-project.org")
}
# install pkgbuild if not installed
if("pkgbuild" %!in% install_packages[, 1]){
  devtools::install_version("pkgbuild", version = "1.4.2", repos = "http://cran.us.r-project.org")
}
# install miniUI if not installed
if("miniUI" %!in% install_packages[, 1]){
  devtools::install_version("miniUI", version = "0.1.1.1", repos = "http://cran.us.r-project.org")
}
# install httpuv if not installed
if("httpuv" %!in% install_packages[, 1]){
  devtools::install_version("httpuv", version = "1.6.12", repos = "http://cran.us.r-project.org")
}
# install rlang if not installed
if("rlang" %!in% install_packages[, 1]){
  devtools::install_version("rlang", version = "1.1.6", repos = "http://cran.us.r-project.org")
}
# install fs if not installed
if("fs" %!in% install_packages[, 1]){
  devtools::install_version("fs", version = "1.6.6", repos = "http://cran.us.r-project.org")
}

# Install versions of libraries if not installed
if("jtools" %in% install_packages[, 1]){
  lav_version <- unique(install_packages[which(install_packages[, 1] %in% "jtools"), 3])
  if("2.2.2" %in% lav_version)
  devtools::install_version("jtools", version = "2.2.2", repos = "http://cran.us.r-project.org")
}
if("ggplot2" %in% install_packages[, 1]){
  lav_version <- unique(install_packages[which(install_packages[, 1] %in% "ggplot2"), 3])
  if("3.5.1" %in% lav_version)
    devtools::install_version("ggplot2", version = "3.5.1", repos = "http://cran.us.r-project.org")
}
if("lavaan" %in% install_packages[, 1]){
  lav_version <- unique(install_packages[which(install_packages[, 1] %in% "lavaan"), 3])
  if("0.6-16" %in% lav_version)
    devtools::install_version("lavaan", version = "0.6-16", repos = "http://cran.us.r-project.org")
}
if("OpenMx" %in% install_packages[, 1]){
  lav_version <- unique(install_packages[which(install_packages[, 1] %in% "OpenMx"), 3])
  if("2.21.11" %in% lav_version)
    devtools::install_version("OpenMx", version = "2.21.11", repos = "http://cran.us.r-project.org")
}
if("Matrix" %in% install_packages[, 1]){
  lav_version <- unique(install_packages[which(install_packages[, 1] %in% "Matrix"), 3])
  if("1.6-1.1" %in% lav_version)
    devtools::install_version("Matrix", version = "1.6-1.1", repos = "http://cran.us.r-project.org")
}
if("modelsummary" %in% install_packages[, 1]){
  lav_version <- unique(install_packages[which(install_packages[, 1] %in% "modelsummary"), 3])
  if("1.4.3" %in% lav_version)
    devtools::install_version("modelsummary", version = "1.4.3", repos = "http://cran.us.r-project.org")
}
if("survey" %in% install_packages[, 1]){
  lav_version <- unique(install_packages[which(install_packages[, 1] %in% "survey"), 3])
  if("4.2-1" %in% lav_version)
    devtools::install_version("survey", version = "4.2-1", repos = "http://cran.us.r-project.org")
}
if("survival" %in% install_packages[, 1]){
  lav_version <- unique(install_packages[which(install_packages[, 1] %in% "survival"), 3])
  if("3.5-7" %in% lav_version)
    devtools::install_version("survival", version = "3.5-7", repos = "http://cran.us.r-project.org")
}
if("tidySEM" %in% install_packages[, 1]){
  lav_version <- unique(install_packages[which(install_packages[, 1] %in% "tidySEM"), 3])
  if("0.2.6" %in% lav_version)
    devtools::install_version("tidySEM", version = "0.2.6", repos = "http://cran.us.r-project.org")
}

if("haven" %in% install_packages[, 1]){
  lav_version <- unique(install_packages[which(install_packages[, 1] %in% "haven"), 3])
  if("2.5.3" %in% lav_version)
    devtools::install_version("haven", version = "2.5.3", repos = "http://cran.us.r-project.org")
}
if("RcppEigen" %in% install_packages[, 1]){
  lav_version <- unique(install_packages[which(install_packages[, 1] %in% "RcppEigen"), 3])
  if("0.3.4.0.1" %in% lav_version)
    devtools::install_version("RcppEigen", version = "0.3.4.0.1", repos = "http://cran.us.r-project.org")
}
if("StanHeaders" %in% install_packages[, 1]){
  lav_version <- unique(install_packages[which(install_packages[, 1] %in% "StanHeaders"), 3])
  if("2.26.28" %in% lav_version)
    devtools::install_version("StanHeaders", version = "2.26.28", repos = "http://cran.us.r-project.org")
}
if("rpf" %in% install_packages[, 1]){
  lav_version <- unique(install_packages[which(install_packages[, 1] %in% "rpf"), 3])
  if("1.0.14" %in% lav_version)
    devtools::install_version("rpf", version = "1.0.14", repos = "http://cran.us.r-project.org")
}

if("minqa" %in% install_packages[, 1]){
  lav_version <- unique(install_packages[which(install_packages[, 1] %in% "minqa"), 3])
  if("1.2.7" %in% lav_version)
    devtools::install_version("minqa", version = "1.2.7", repos = "http://cran.us.r-project.org")
}
if("kableExtra" %in% install_packages[, 1]){
  lav_version <- unique(install_packages[which(install_packages[, 1] %in% "kableExtra"), 3])
  if("1.3.4" %in% lav_version)
    devtools::install_version("kableExtra", version = "1.3.4", repos = "http://cran.us.r-project.org")
}

if("pkgbuild" %in% install_packages[, 1]){
  lav_version <- unique(install_packages[which(install_packages[, 1] %in% "pkgbuild"), 3])
  if("1.4.2" %in% lav_version)
    devtools::install_version("pkgbuild", version = "1.4.2", repos = "http://cran.us.r-project.org")
}
if("miniUI" %in% install_packages[, 1]){
  lav_version <- unique(install_packages[which(install_packages[, 1] %in% "miniUI"), 3])
  if("0.1.1.1" %in% lav_version)
    devtools::install_version("miniUI", version = "0.1.1.1", repos = "http://cran.us.r-project.org")
}
if("httpuv" %in% install_packages[, 1]){
  lav_version <- unique(install_packages[which(install_packages[, 1] %in% "httpuv"), 3])
  if("1.6.12" %in% lav_version)
    devtools::install_version("httpuv", version = "1.6.12", repos = "http://cran.us.r-project.org")
}
if("rlang" %in% install_packages[, 1]){
  lav_version <- unique(install_packages[which(install_packages[, 1] %in% "rlang"), 3])
  if("1.1.6" %in% lav_version)
    devtools::install_version("rlang", version = "1.1.6", repos = "http://cran.us.r-project.org")
}
if("fs" %in% install_packages[, 1]){
  lav_version <- unique(install_packages[which(install_packages[, 1] %in% "fs"), 3])
  if("1.6.6" %in% lav_version)
    devtools::install_version("fs", version = "1.6.6", repos = "http://cran.us.r-project.org")
}
