getFRED <- function(seriesname,dir='',to.q=FALSE) {
    url <- paste0('https://fred.stlouisfed.org/data/', seriesname, '.txt')
    fname <- paste0(dir,seriesname,'.txt')
    if (!file.exists(fname)) {
        ## Download the data from FRED
        download.file(url, destfile = fname, method = "wget")
    }
    FREDraw <- readLines(fname)

    # Where does the data start
    datastart = which(gsub(' ', '',FREDraw)=='DATEVALUE') - 2

    data <- read.table(fname, skip = datastart, header = TRUE)
    data$DATE <- as.Date(data$DATE) #as.numeric(format(as.Date(data$DATE), format='%Y%m%d'))

    if (to.q) {
        data$y <- as.numeric(format(data$DATE,'%Y'))
        m <- as.numeric(format(data$DATE,'%m'))
        data$q <- 1+floor(((m-1-12 %% 12)/3))
        data$DATE <- NULL
    }

    return(data)
}

