# install all the necessary packages for old version of Zelig
lapply(c('AER', 'Amelia', 'Formula', 'geepack', 'jsonlite',
         'sandwich', 'MatchIt', 'maxLik', 'survey',
         'MCMCpack', 'VGAM'), install.packages)
# install archived version
install.packages('https://cran.r-project.org/src/contrib/Archive/Zelig/Zelig_5.1.6.1.tar.gz', repos = NULL, type = 'source')