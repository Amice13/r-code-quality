####Donald Trump and the Lie - R Analysis File####

###SET WD###
setwd('/Users/rtruex/Dropbox/Election Legitimacy Tracking Survey/Data')
rm(list=ls(all=TRUE))

###LOAD PACKAGES AND FUNCTIONS###

library("ggplot2")
library("dplyr")
library("survey")
library("ebal")
library("janitor")
library("Hmisc")
library("list")
library("GGally")
library("broom.helpers")
library("stringr")
library("cjoint")
library("psych")


require(grid)
vp.layout <- function(x, y) viewport(layout.pos.row=x, layout.pos.col=y)
arrange_ggplot2 <- function(..., nrow=NULL, ncol=NULL, as.table=FALSE) {
  dots <- list(...)
  n <- length(dots)
  if(is.null(nrow) & is.null(ncol)) { nrow = floor(n/2) ; ncol = ceiling(n/nrow)}
  if(is.null(nrow)) { nrow = ceiling(n/ncol)}
  if(is.null(ncol)) { ncol = ceiling(n/nrow)}
  
  grid.newpage()
  pushViewport(viewport(layout=grid.layout(nrow,ncol) ) )
  ii.p <- 1
  for(ii.row in seq(1, nrow)){
    ii.table.row <- ii.row  
    if(as.table) {ii.table.row <- nrow - ii.table.row + 1}
    for(ii.col in seq(1, ncol)){
      ii.table <- ii.p
      if(ii.p > n) break
      print(dots[[ii.table]], vp=vp.layout(ii.table.row, ii.col))
      ii.p <- ii.p + 1
    }
  }
}

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

###LOAD DATA###

load('data.RData')

ftable(data$date)

data$date.num<-NA
data$date.num[data$date=="10-27"]<-1
data$date.num[data$date=="10-28"]<-2
data$date.num[data$date=="10-29"]<-3
data$date.num[data$date=="10-30"]<-4
data$date.num[data$date=="10-31"]<-5
data$date.num[data$date=="11-01"]<-6
data$date.num[data$date=="11-02"]<-7
data$date.num[data$date=="11-03"]<-8
data$date.num[data$date=="11-04"]<-9
data$date.num[data$date=="11-05"]<-10
data$date.num[data$date=="11-06"]<-11
data$date.num[data$date=="11-07"]<-12
data$date.num[data$date=="11-08"]<-13
data$date.num[data$date=="11-09"]<-14
data$date.num[data$date=="11-10"]<-15
data$date.num[data$date=="11-11"]<-16
data$date.num[data$date=="11-12"]<-17
data$date.num[data$date=="11-13"]<-18
data$date.num[data$date=="11-14"]<-19
data$date.num[data$date=="11-15"]<-20
data$date.num[data$date=="11-16"]<-21
data$date.num[data$date=="11-17"]<-22
data$date.num[data$date=="11-18"]<-23
data$date.num[data$date=="11-19"]<-24
data$date.num[data$date=="11-20"]<-25
data$date.num[data$date=="11-21"]<-26
data$date.num[data$date=="11-24"]<-29
data$date.num[data$date=="11-25"]<-30
data$date.num[data$date=="11-26"]<-31
data$date.num[data$date=="11-27"]<-32
data$date.num[data$date=="11-28"]<-33
data$date.num[data$date=="11-29"]<-34
data$date.num[data$date=="11-30"]<-35
data$date.num[data$date=="12-01"]<-36
data$date.num[data$date=="12-02"]<-37
data$date.num[data$date=="12-03"]<-38
data$date.num[data$date=="12-04"]<-39
data$date.num[data$date=="12-05"]<-40
data$date.num[data$date=="12-06"]<-41
data$date.num[data$date=="12-07"]<-42
data$date.num[data$date=="12-08"]<-43
data$date.num[data$date=="12-09"]<-44
data$date.num[data$date=="12-10"]<-45
data$date.num[data$date=="12-11"]<-46
data$date.num[data$date=="12-12"]<-47
data$date.num[data$date=="12-13"]<-48
data$date.num[data$date=="12-14"]<-49
data$date.num[data$date=="12-15"]<-50
data$date.num[data$date=="12-16"]<-51
data$date.num[data$date=="12-17"]<-52
data$date.num[data$date=="12-18"]<-53
data$date.num[data$date=="12-19"]<-54
data$date.num[data$date=="12-20"]<-55
data$date.num[data$date=="12-21"]<-56
data$date.num[data$date=="12-22"]<-57
data$date.num[data$date=="12-23"]<-58
data$date.num[data$date=="12-24"]<-59
data$date.num[data$date=="12-25"]<-60
data$date.num[data$date=="12-26"]<-61
data$date.num[data$date=="12-27"]<-62
data$date.num[data$date=="12-28"]<-63
data$date.num[data$date=="12-29"]<-64
data$date.num[data$date=="12-30"]<-65
data$date.num[data$date=="12-31"]<-66
data$date.num[data$date=="01-01"]<-67
data$date.num[data$date=="01-02"]<-68
data$date.num[data$date=="01-03"]<-69
data$date.num[data$date=="01-04"]<-70
data$date.num[data$date=="01-05"]<-71
data$date.num[data$date=="01-06"]<-72
data$date.num[data$date=="01-07"]<-73
data$date.num[data$date=="01-08"]<-74
data$date.num[data$date=="01-09"]<-75
data$date.num[data$date=="01-10"]<-76
data$date.num[data$date=="01-11"]<-77
data$date.num[data$date=="01-12"]<-78
data$date.num[data$date=="01-13"]<-79
data$date.num[data$date=="01-14"]<-80
data$date.num[data$date=="01-15"]<-81
data$date.num[data$date=="01-16"]<-82
data$date.num[data$date=="01-17"]<-83
data$date.num[data$date=="01-18"]<-84
data$date.num[data$date=="01-19"]<-85
data$date.num[data$date=="01-20"]<-86
data$date.num[data$date=="01-21"]<-87
data$date.num[data$date=="01-22"]<-88
data$date.num[data$date=="01-23"]<-89
data$date.num[data$date=="01-24"]<-90
data$date.num[data$date=="01-25"]<-91
data$date.num[data$date=="01-26"]<-92
data$date.num[data$date=="01-27"]<-93
data$date.num[data$date=="01-28"]<-94
data$date.num[data$date=="01-29"]<-95

ftable(data$date.num)

ftable(data$date)

data$postelection<-0
data$postelection[data$date=="10-27"]<-0
data$postelection[data$date=="10-28"]<-0
data$postelection[data$date=="10-29"]<-0
data$postelection[data$date=="10-30"]<-0
data$postelection[data$date=="10-31"]<-0
data$postelection[data$date=="11-01"]<-0
data$postelection[data$date=="11-02"]<-0
data$postelection[data$date=="11-03"]<-0
data$postelection[data$date=="11-04"]<-1
data$postelection[data$date=="11-05"]<-1
data$postelection[data$date=="11-06"]<-1
data$postelection[data$date=="11-07"]<-1
data$postelection[data$date=="11-08"]<-1
data$postelection[data$date=="11-09"]<-1
data$postelection[data$date=="11-10"]<-1
data$postelection[data$date=="11-11"]<-1
data$postelection[data$date=="11-12"]<-1
data$postelection[data$date=="11-13"]<-1
data$postelection[data$date=="11-14"]<-1
data$postelection[data$date=="11-15"]<-1
data$postelection[data$date=="11-16"]<-1
data$postelection[data$date=="11-17"]<-1
data$postelection[data$date=="11-18"]<-1
data$postelection[data$date=="11-19"]<-1
data$postelection[data$date=="11-20"]<-1
data$postelection[data$date=="11-21"]<-1
data$postelection[data$date=="11-24"]<-1
data$postelection[data$date=="11-25"]<-1
data$postelection[data$date=="11-26"]<-1
data$postelection[data$date=="11-27"]<-1
data$postelection[data$date=="11-28"]<-1
data$postelection[data$date=="11-29"]<-1
data$postelection[data$date=="11-30"]<-1
data$postelection[data$date=="12-01"]<-1
data$postelection[data$date=="12-02"]<-1
data$postelection[data$date=="12-03"]<-1
data$postelection[data$date=="12-04"]<-1
data$postelection[data$date=="12-05"]<-1
data$postelection[data$date=="12-06"]<-1
data$postelection[data$date=="12-07"]<-1
data$postelection[data$date=="12-08"]<-1
data$postelection[data$date=="12-09"]<-1
data$postelection[data$date=="12-10"]<-1
data$postelection[data$date=="12-11"]<-1
data$postelection[data$date=="12-12"]<-1
data$postelection[data$date=="12-13"]<-1
data$postelection[data$date=="12-14"]<-1
data$postelection[data$date=="12-15"]<-1
data$postelection[data$date=="12-16"]<-1
data$postelection[data$date=="12-17"]<-1
data$postelection[data$date=="12-18"]<-1
data$postelection[data$date=="12-19"]<-1
data$postelection[data$date=="12-20"]<-1
data$postelection[data$date=="12-21"]<-1
data$postelection[data$date=="12-22"]<-1
data$postelection[data$date=="12-23"]<-1
data$postelection[data$date=="12-24"]<-1
data$postelection[data$date=="12-25"]<-1
data$postelection[data$date=="12-26"]<-1
data$postelection[data$date=="12-27"]<-1
data$postelection[data$date=="12-28"]<-1
data$postelection[data$date=="12-29"]<-1
data$postelection[data$date=="12-30"]<-1
data$postelection[data$date=="12-31"]<-1
data$postelection[data$date=="01-01"]<-1
data$postelection[data$date=="01-02"]<-1
data$postelection[data$date=="01-03"]<-1
data$postelection[data$date=="01-04"]<-1
data$postelection[data$date=="01-05"]<-1
data$postelection[data$date=="01-06"]<-1
data$postelection[data$date=="01-07"]<-1
data$postelection[data$date=="01-08"]<-1
data$postelection[data$date=="01-09"]<-1
data$postelection[data$date=="01-10"]<-1
data$postelection[data$date=="01-11"]<-1
data$postelection[data$date=="01-12"]<-1
data$postelection[data$date=="01-13"]<-1
data$postelection[data$date=="01-14"]<-1
data$postelection[data$date=="01-15"]<-1
data$postelection[data$date=="01-16"]<-1
data$postelection[data$date=="01-17"]<-1
data$postelection[data$date=="01-18"]<-1
data$postelection[data$date=="01-19"]<-1
data$postelection[data$date=="01-20"]<-1
data$postelection[data$date=="01-21"]<-1
data$postelection[data$date=="01-22"]<-1
data$postelection[data$date=="01-23"]<-1
data$postelection[data$date=="01-24"]<-1
data$postelection[data$date=="01-25"]<-1
data$postelection[data$date=="01-26"]<-1
data$postelection[data$date=="01-27"]<-1
data$postelection[data$date=="01-28"]<-1
data$postelection[data$date=="01-29"]<-1

ftable(data$postelection)

data$postcall<-0
data$postcall[data$date=="10-27"]<-0
data$postcall[data$date=="10-28"]<-0
data$postcall[data$date=="10-29"]<-0
data$postcall[data$date=="10-30"]<-0
data$postcall[data$date=="10-31"]<-0
data$postcall[data$date=="11-01"]<-0
data$postcall[data$date=="11-02"]<-0
data$postcall[data$date=="11-03"]<-0
data$postcall[data$date=="11-04"]<-0
data$postcall[data$date=="11-05"]<-0
data$postcall[data$date=="11-06"]<-0
data$postcall[data$date=="11-07"]<-0
data$postcall[data$date=="11-08"]<-1
data$postcall[data$date=="11-09"]<-1
data$postcall[data$date=="11-10"]<-1
data$postcall[data$date=="11-11"]<-1
data$postcall[data$date=="11-12"]<-1
data$postcall[data$date=="11-13"]<-1
data$postcall[data$date=="11-14"]<-1
data$postcall[data$date=="11-15"]<-1
data$postcall[data$date=="11-16"]<-1
data$postcall[data$date=="11-17"]<-1
data$postcall[data$date=="11-18"]<-1
data$postcall[data$date=="11-19"]<-1
data$postcall[data$date=="11-20"]<-1
data$postcall[data$date=="11-21"]<-1
data$postcall[data$date=="11-24"]<-1
data$postcall[data$date=="11-25"]<-1
data$postcall[data$date=="11-26"]<-1
data$postcall[data$date=="11-27"]<-1
data$postcall[data$date=="11-28"]<-1
data$postcall[data$date=="11-29"]<-1
data$postcall[data$date=="11-30"]<-1
data$postcall[data$date=="12-01"]<-1
data$postcall[data$date=="12-02"]<-1
data$postcall[data$date=="12-03"]<-1
data$postcall[data$date=="12-04"]<-1
data$postcall[data$date=="12-05"]<-1
data$postcall[data$date=="12-06"]<-1
data$postcall[data$date=="12-07"]<-1
data$postcall[data$date=="12-08"]<-1
data$postcall[data$date=="12-09"]<-1
data$postcall[data$date=="12-10"]<-1
data$postcall[data$date=="12-11"]<-1
data$postcall[data$date=="12-12"]<-1
data$postcall[data$date=="12-13"]<-1
data$postcall[data$date=="12-14"]<-1
data$postcall[data$date=="12-15"]<-1
data$postcall[data$date=="12-16"]<-1
data$postcall[data$date=="12-17"]<-1
data$postcall[data$date=="12-18"]<-1
data$postcall[data$date=="12-19"]<-1
data$postcall[data$date=="12-20"]<-1
data$postcall[data$date=="12-21"]<-1
data$postcall[data$date=="12-22"]<-1
data$postcall[data$date=="12-23"]<-1
data$postcall[data$date=="12-24"]<-1
data$postcall[data$date=="12-25"]<-1
data$postcall[data$date=="12-26"]<-1
data$postcall[data$date=="12-27"]<-1
data$postcall[data$date=="12-28"]<-1
data$postcall[data$date=="12-29"]<-1
data$postcall[data$date=="12-30"]<-1
data$postcall[data$date=="12-31"]<-1
data$postcall[data$date=="01-01"]<-1
data$postcall[data$date=="01-02"]<-1
data$postcall[data$date=="01-03"]<-1
data$postcall[data$date=="01-04"]<-1
data$postcall[data$date=="01-05"]<-1
data$postcall[data$date=="01-06"]<-1
data$postcall[data$date=="01-07"]<-1
data$postcall[data$date=="01-08"]<-1
data$postcall[data$date=="01-09"]<-1
data$postcall[data$date=="01-10"]<-1
data$postcall[data$date=="01-11"]<-1
data$postcall[data$date=="01-12"]<-1
data$postcall[data$date=="01-13"]<-1
data$postcall[data$date=="01-14"]<-1
data$postcall[data$date=="01-15"]<-1
data$postcall[data$date=="01-16"]<-1
data$postcall[data$date=="01-17"]<-1
data$postcall[data$date=="01-18"]<-1
data$postcall[data$date=="01-19"]<-1
data$postcall[data$date=="01-20"]<-1
data$postcall[data$date=="01-21"]<-1
data$postcall[data$date=="01-22"]<-1
data$postcall[data$date=="01-23"]<-1
data$postcall[data$date=="01-24"]<-1
data$postcall[data$date=="01-25"]<-1
data$postcall[data$date=="01-26"]<-1
data$postcall[data$date=="01-27"]<-1
data$postcall[data$date=="01-28"]<-1
data$postcall[data$date=="01-29"]<-1
ftable(data$postcall)

data$postmichigan<-0
data$postmichigan[data$date=="10-27"]<-0
data$postmichigan[data$date=="10-28"]<-0
data$postmichigan[data$date=="10-29"]<-0
data$postmichigan[data$date=="10-30"]<-0
data$postmichigan[data$date=="10-31"]<-0
data$postmichigan[data$date=="11-01"]<-0
data$postmichigan[data$date=="11-02"]<-0
data$postmichigan[data$date=="11-03"]<-0
data$postmichigan[data$date=="11-04"]<-0
data$postmichigan[data$date=="11-05"]<-0
data$postmichigan[data$date=="11-06"]<-0
data$postmichigan[data$date=="11-07"]<-0
data$postmichigan[data$date=="11-08"]<-0
data$postmichigan[data$date=="11-09"]<-0
data$postmichigan[data$date=="11-10"]<-0
data$postmichigan[data$date=="11-11"]<-0
data$postmichigan[data$date=="11-12"]<-0
data$postmichigan[data$date=="11-13"]<-0
data$postmichigan[data$date=="11-14"]<-0
data$postmichigan[data$date=="11-15"]<-0
data$postmichigan[data$date=="11-16"]<-0
data$postmichigan[data$date=="11-17"]<-0
data$postmichigan[data$date=="11-18"]<-0
data$postmichigan[data$date=="11-19"]<-0
data$postmichigan[data$date=="11-20"]<-0
data$postmichigan[data$date=="11-21"]<-0
data$postmichigan[data$date=="11-24"]<-0
data$postmichigan[data$date=="11-25"]<-1
data$postmichigan[data$date=="11-26"]<-1
data$postmichigan[data$date=="11-27"]<-1
data$postmichigan[data$date=="11-28"]<-1
data$postmichigan[data$date=="11-29"]<-1
data$postmichigan[data$date=="11-30"]<-1
data$postmichigan[data$date=="12-01"]<-1
data$postmichigan[data$date=="12-02"]<-1
data$postmichigan[data$date=="12-03"]<-1
data$postmichigan[data$date=="12-04"]<-1
data$postmichigan[data$date=="12-05"]<-1
data$postmichigan[data$date=="12-06"]<-1
data$postmichigan[data$date=="12-07"]<-1
data$postmichigan[data$date=="12-08"]<-1
data$postmichigan[data$date=="12-09"]<-1
data$postmichigan[data$date=="12-10"]<-1
data$postmichigan[data$date=="12-11"]<-1
data$postmichigan[data$date=="12-12"]<-1
data$postmichigan[data$date=="12-13"]<-1
data$postmichigan[data$date=="12-14"]<-1
data$postmichigan[data$date=="12-15"]<-1
data$postmichigan[data$date=="12-16"]<-1
data$postmichigan[data$date=="12-17"]<-1
data$postmichigan[data$date=="12-18"]<-1
data$postmichigan[data$date=="12-19"]<-1
data$postmichigan[data$date=="12-20"]<-1
data$postmichigan[data$date=="12-21"]<-1
data$postmichigan[data$date=="12-22"]<-1
data$postmichigan[data$date=="12-23"]<-1
data$postmichigan[data$date=="12-24"]<-1
data$postmichigan[data$date=="12-25"]<-1
data$postmichigan[data$date=="12-26"]<-1
data$postmichigan[data$date=="12-27"]<-1
data$postmichigan[data$date=="12-28"]<-1
data$postmichigan[data$date=="12-29"]<-1
data$postmichigan[data$date=="12-30"]<-1
data$postmichigan[data$date=="12-31"]<-1
data$postmichigan[data$date=="01-01"]<-1
data$postmichigan[data$date=="01-02"]<-1
data$postmichigan[data$date=="01-03"]<-1
data$postmichigan[data$date=="01-04"]<-1
data$postmichigan[data$date=="01-05"]<-1
data$postmichigan[data$date=="01-06"]<-1
data$postmichigan[data$date=="01-07"]<-1
data$postmichigan[data$date=="01-08"]<-1
data$postmichigan[data$date=="01-09"]<-1
data$postmichigan[data$date=="01-10"]<-1
data$postmichigan[data$date=="01-11"]<-1
data$postmichigan[data$date=="01-12"]<-1
data$postmichigan[data$date=="01-13"]<-1
data$postmichigan[data$date=="01-14"]<-1
data$postmichigan[data$date=="01-15"]<-1
data$postmichigan[data$date=="01-16"]<-1
data$postmichigan[data$date=="01-17"]<-1
data$postmichigan[data$date=="01-18"]<-1
data$postmichigan[data$date=="01-19"]<-1
data$postmichigan[data$date=="01-20"]<-1
data$postmichigan[data$date=="01-21"]<-1
data$postmichigan[data$date=="01-22"]<-1
data$postmichigan[data$date=="01-23"]<-1
data$postmichigan[data$date=="01-24"]<-1
data$postmichigan[data$date=="01-25"]<-1
data$postmichigan[data$date=="01-26"]<-1
data$postmichigan[data$date=="01-27"]<-1
data$postmichigan[data$date=="01-28"]<-1
data$postmichigan[data$date=="01-29"]<-1
ftable(data$postmichigan)

data$postbarr<-0
data$postbarr[data$date=="10-27"]<-0
data$postbarr[data$date=="10-28"]<-0
data$postbarr[data$date=="10-29"]<-0
data$postbarr[data$date=="10-30"]<-0
data$postbarr[data$date=="10-31"]<-0
data$postbarr[data$date=="11-01"]<-0
data$postbarr[data$date=="11-02"]<-0
data$postbarr[data$date=="11-03"]<-0
data$postbarr[data$date=="11-04"]<-0
data$postbarr[data$date=="11-05"]<-0
data$postbarr[data$date=="11-06"]<-0
data$postbarr[data$date=="11-07"]<-0
data$postbarr[data$date=="11-08"]<-0
data$postbarr[data$date=="11-09"]<-0
data$postbarr[data$date=="11-10"]<-0
data$postbarr[data$date=="11-11"]<-0
data$postbarr[data$date=="11-12"]<-0
data$postbarr[data$date=="11-13"]<-0
data$postbarr[data$date=="11-14"]<-0
data$postbarr[data$date=="11-15"]<-0
data$postbarr[data$date=="11-16"]<-0
data$postbarr[data$date=="11-17"]<-0
data$postbarr[data$date=="11-18"]<-0
data$postbarr[data$date=="11-19"]<-0
data$postbarr[data$date=="11-20"]<-0
data$postbarr[data$date=="11-21"]<-0
data$postbarr[data$date=="11-24"]<-0
data$postbarr[data$date=="11-25"]<-0
data$postbarr[data$date=="11-26"]<-0
data$postbarr[data$date=="11-27"]<-0
data$postbarr[data$date=="11-28"]<-0
data$postbarr[data$date=="11-29"]<-0
data$postbarr[data$date=="11-30"]<-0
data$postbarr[data$date=="12-01"]<-0
data$postbarr[data$date=="12-02"]<-0
data$postbarr[data$date=="12-03"]<-1
data$postbarr[data$date=="12-04"]<-1
data$postbarr[data$date=="12-05"]<-1
data$postbarr[data$date=="12-06"]<-1
data$postbarr[data$date=="12-07"]<-1
data$postbarr[data$date=="12-08"]<-1
data$postbarr[data$date=="12-09"]<-1
data$postbarr[data$date=="12-10"]<-1
data$postbarr[data$date=="12-11"]<-1
data$postbarr[data$date=="12-12"]<-1
data$postbarr[data$date=="12-13"]<-1
data$postbarr[data$date=="12-14"]<-1
data$postbarr[data$date=="12-15"]<-1
data$postbarr[data$date=="12-16"]<-1
data$postbarr[data$date=="12-17"]<-1
data$postbarr[data$date=="12-18"]<-1
data$postbarr[data$date=="12-19"]<-1
data$postbarr[data$date=="12-20"]<-1
data$postbarr[data$date=="12-21"]<-1
data$postbarr[data$date=="12-22"]<-1
data$postbarr[data$date=="12-23"]<-1
data$postbarr[data$date=="12-24"]<-1
data$postbarr[data$date=="12-25"]<-1
data$postbarr[data$date=="12-26"]<-1
data$postbarr[data$date=="12-27"]<-1
data$postbarr[data$date=="12-28"]<-1
data$postbarr[data$date=="12-29"]<-1
data$postbarr[data$date=="12-30"]<-1
data$postbarr[data$date=="12-31"]<-1
data$postbarr[data$date=="01-01"]<-1
data$postbarr[data$date=="01-02"]<-1
data$postbarr[data$date=="01-03"]<-1
data$postbarr[data$date=="01-04"]<-1
data$postbarr[data$date=="01-05"]<-1
data$postbarr[data$date=="01-06"]<-1
data$postbarr[data$date=="01-07"]<-1
data$postbarr[data$date=="01-08"]<-1
data$postbarr[data$date=="01-09"]<-1
data$postbarr[data$date=="01-10"]<-1
data$postbarr[data$date=="01-11"]<-1
data$postbarr[data$date=="01-12"]<-1
data$postbarr[data$date=="01-13"]<-1
data$postbarr[data$date=="01-14"]<-1
data$postbarr[data$date=="01-15"]<-1
data$postbarr[data$date=="01-16"]<-1
data$postbarr[data$date=="01-17"]<-1
data$postbarr[data$date=="01-18"]<-1
data$postbarr[data$date=="01-19"]<-1
data$postbarr[data$date=="01-20"]<-1
data$postbarr[data$date=="01-21"]<-1
data$postbarr[data$date=="01-22"]<-1
data$postbarr[data$date=="01-23"]<-1
data$postbarr[data$date=="01-24"]<-1
data$postbarr[data$date=="01-25"]<-1
data$postbarr[data$date=="01-26"]<-1
data$postbarr[data$date=="01-27"]<-1
data$postbarr[data$date=="01-28"]<-1
data$postbarr[data$date=="01-29"]<-1
ftable(data$postbarr)

data$postelectoralcollege<-0
data$postelectoralcollege[data$date=="10-27"]<-0
data$postelectoralcollege[data$date=="10-28"]<-0
data$postelectoralcollege[data$date=="10-29"]<-0
data$postelectoralcollege[data$date=="10-30"]<-0
data$postelectoralcollege[data$date=="10-31"]<-0
data$postelectoralcollege[data$date=="11-01"]<-0
data$postelectoralcollege[data$date=="11-02"]<-0
data$postelectoralcollege[data$date=="11-03"]<-0
data$postelectoralcollege[data$date=="11-04"]<-0
data$postelectoralcollege[data$date=="11-05"]<-0
data$postelectoralcollege[data$date=="11-06"]<-0
data$postelectoralcollege[data$date=="11-07"]<-0
data$postelectoralcollege[data$date=="11-08"]<-0
data$postelectoralcollege[data$date=="11-09"]<-0
data$postelectoralcollege[data$date=="11-10"]<-0
data$postelectoralcollege[data$date=="11-11"]<-0
data$postelectoralcollege[data$date=="11-12"]<-0
data$postelectoralcollege[data$date=="11-13"]<-0
data$postelectoralcollege[data$date=="11-14"]<-0
data$postelectoralcollege[data$date=="11-15"]<-0
data$postelectoralcollege[data$date=="11-16"]<-0
data$postelectoralcollege[data$date=="11-17"]<-0
data$postelectoralcollege[data$date=="11-18"]<-0
data$postelectoralcollege[data$date=="11-19"]<-0
data$postelectoralcollege[data$date=="11-20"]<-0
data$postelectoralcollege[data$date=="11-21"]<-0
data$postelectoralcollege[data$date=="11-24"]<-0
data$postelectoralcollege[data$date=="11-25"]<-0
data$postelectoralcollege[data$date=="11-26"]<-0
data$postelectoralcollege[data$date=="11-27"]<-0
data$postelectoralcollege[data$date=="11-28"]<-0
data$postelectoralcollege[data$date=="11-29"]<-0
data$postelectoralcollege[data$date=="11-30"]<-0
data$postelectoralcollege[data$date=="12-01"]<-0
data$postelectoralcollege[data$date=="12-02"]<-0
data$postelectoralcollege[data$date=="12-03"]<-0
data$postelectoralcollege[data$date=="12-04"]<-0
data$postelectoralcollege[data$date=="12-05"]<-0
data$postelectoralcollege[data$date=="12-06"]<-0
data$postelectoralcollege[data$date=="12-07"]<-0
data$postelectoralcollege[data$date=="12-08"]<-0
data$postelectoralcollege[data$date=="12-09"]<-0
data$postelectoralcollege[data$date=="12-10"]<-0
data$postelectoralcollege[data$date=="12-11"]<-0
data$postelectoralcollege[data$date=="12-12"]<-0
data$postelectoralcollege[data$date=="12-13"]<-0
data$postelectoralcollege[data$date=="12-14"]<-1
data$postelectoralcollege[data$date=="12-15"]<-1
data$postelectoralcollege[data$date=="12-16"]<-1
data$postelectoralcollege[data$date=="12-17"]<-1
data$postelectoralcollege[data$date=="12-18"]<-1
data$postelectoralcollege[data$date=="12-19"]<-1
data$postelectoralcollege[data$date=="12-20"]<-1
data$postelectoralcollege[data$date=="12-21"]<-1
data$postelectoralcollege[data$date=="12-22"]<-1
data$postelectoralcollege[data$date=="12-23"]<-1
data$postelectoralcollege[data$date=="12-24"]<-1
data$postelectoralcollege[data$date=="12-25"]<-1
data$postelectoralcollege[data$date=="12-26"]<-1
data$postelectoralcollege[data$date=="12-27"]<-1
data$postelectoralcollege[data$date=="12-28"]<-1
data$postelectoralcollege[data$date=="12-29"]<-1
data$postelectoralcollege[data$date=="12-30"]<-1
data$postelectoralcollege[data$date=="12-31"]<-1
data$postelectoralcollege[data$date=="01-01"]<-1
data$postelectoralcollege[data$date=="01-02"]<-1
data$postelectoralcollege[data$date=="01-03"]<-1
data$postelectoralcollege[data$date=="01-04"]<-1
data$postelectoralcollege[data$date=="01-05"]<-1
data$postelectoralcollege[data$date=="01-06"]<-1
data$postelectoralcollege[data$date=="01-07"]<-1
data$postelectoralcollege[data$date=="01-08"]<-1
data$postelectoralcollege[data$date=="01-09"]<-1
data$postelectoralcollege[data$date=="01-10"]<-1
data$postelectoralcollege[data$date=="01-11"]<-1
data$postelectoralcollege[data$date=="01-12"]<-1
data$postelectoralcollege[data$date=="01-13"]<-1
data$postelectoralcollege[data$date=="01-14"]<-1
data$postelectoralcollege[data$date=="01-15"]<-1
data$postelectoralcollege[data$date=="01-16"]<-1
data$postelectoralcollege[data$date=="01-17"]<-1
data$postelectoralcollege[data$date=="01-18"]<-1
data$postelectoralcollege[data$date=="01-19"]<-1
data$postelectoralcollege[data$date=="01-20"]<-1
data$postelectoralcollege[data$date=="01-21"]<-1
data$postelectoralcollege[data$date=="01-22"]<-1
data$postelectoralcollege[data$date=="01-23"]<-1
data$postelectoralcollege[data$date=="01-24"]<-1
data$postelectoralcollege[data$date=="01-25"]<-1
data$postelectoralcollege[data$date=="01-26"]<-1
data$postelectoralcollege[data$date=="01-27"]<-1
data$postelectoralcollege[data$date=="01-28"]<-1
data$postelectoralcollege[data$date=="01-29"]<-1
ftable(data$postelectoralcollege)

data$postinsurrection<-0
data$postinsurrection[data$date=="10-27"]<-0
data$postinsurrection[data$date=="10-28"]<-0
data$postinsurrection[data$date=="10-29"]<-0
data$postinsurrection[data$date=="10-30"]<-0
data$postinsurrection[data$date=="10-31"]<-0
data$postinsurrection[data$date=="11-01"]<-0
data$postinsurrection[data$date=="11-02"]<-0
data$postinsurrection[data$date=="11-03"]<-0
data$postinsurrection[data$date=="11-04"]<-0
data$postinsurrection[data$date=="11-05"]<-0
data$postinsurrection[data$date=="11-06"]<-0
data$postinsurrection[data$date=="11-07"]<-0
data$postinsurrection[data$date=="11-08"]<-0
data$postinsurrection[data$date=="11-09"]<-0
data$postinsurrection[data$date=="11-10"]<-0
data$postinsurrection[data$date=="11-11"]<-0
data$postinsurrection[data$date=="11-12"]<-0
data$postinsurrection[data$date=="11-13"]<-0
data$postinsurrection[data$date=="11-14"]<-0
data$postinsurrection[data$date=="11-15"]<-0
data$postinsurrection[data$date=="11-16"]<-0
data$postinsurrection[data$date=="11-17"]<-0
data$postinsurrection[data$date=="11-18"]<-0
data$postinsurrection[data$date=="11-19"]<-0
data$postinsurrection[data$date=="11-20"]<-0
data$postinsurrection[data$date=="11-21"]<-0
data$postinsurrection[data$date=="11-24"]<-0
data$postinsurrection[data$date=="11-25"]<-0
data$postinsurrection[data$date=="11-26"]<-0
data$postinsurrection[data$date=="11-27"]<-0
data$postinsurrection[data$date=="11-28"]<-0
data$postinsurrection[data$date=="11-29"]<-0
data$postinsurrection[data$date=="11-30"]<-0
data$postinsurrection[data$date=="12-01"]<-0
data$postinsurrection[data$date=="12-02"]<-0
data$postinsurrection[data$date=="12-03"]<-0
data$postinsurrection[data$date=="12-04"]<-0
data$postinsurrection[data$date=="12-05"]<-0
data$postinsurrection[data$date=="12-06"]<-0
data$postinsurrection[data$date=="12-07"]<-0
data$postinsurrection[data$date=="12-08"]<-0
data$postinsurrection[data$date=="12-09"]<-0
data$postinsurrection[data$date=="12-10"]<-0
data$postinsurrection[data$date=="12-11"]<-0
data$postinsurrection[data$date=="12-12"]<-0
data$postinsurrection[data$date=="12-13"]<-0
data$postinsurrection[data$date=="12-14"]<-0
data$postinsurrection[data$date=="12-15"]<-0
data$postinsurrection[data$date=="12-16"]<-0
data$postinsurrection[data$date=="12-17"]<-0
data$postinsurrection[data$date=="12-18"]<-0
data$postinsurrection[data$date=="12-19"]<-0
data$postinsurrection[data$date=="12-20"]<-0
data$postinsurrection[data$date=="12-21"]<-0
data$postinsurrection[data$date=="12-22"]<-0
data$postinsurrection[data$date=="12-23"]<-0
data$postinsurrection[data$date=="12-24"]<-0
data$postinsurrection[data$date=="12-25"]<-0
data$postinsurrection[data$date=="12-26"]<-0
data$postinsurrection[data$date=="12-27"]<-0
data$postinsurrection[data$date=="12-28"]<-0
data$postinsurrection[data$date=="12-29"]<-0
data$postinsurrection[data$date=="12-30"]<-0
data$postinsurrection[data$date=="12-31"]<-0
data$postinsurrection[data$date=="01-01"]<-0
data$postinsurrection[data$date=="01-02"]<-0
data$postinsurrection[data$date=="01-03"]<-0
data$postinsurrection[data$date=="01-04"]<-0
data$postinsurrection[data$date=="01-05"]<-0
data$postinsurrection[data$date=="01-06"]<-0
data$postinsurrection[data$date=="01-07"]<-1
data$postinsurrection[data$date=="01-08"]<-1
data$postinsurrection[data$date=="01-09"]<-1
data$postinsurrection[data$date=="01-10"]<-1
data$postinsurrection[data$date=="01-11"]<-1
data$postinsurrection[data$date=="01-12"]<-1
data$postinsurrection[data$date=="01-13"]<-1
data$postinsurrection[data$date=="01-14"]<-1
data$postinsurrection[data$date=="01-15"]<-1
data$postinsurrection[data$date=="01-16"]<-1
data$postinsurrection[data$date=="01-17"]<-1
data$postinsurrection[data$date=="01-18"]<-1
data$postinsurrection[data$date=="01-19"]<-1
data$postinsurrection[data$date=="01-20"]<-1
data$postinsurrection[data$date=="01-21"]<-1
data$postinsurrection[data$date=="01-22"]<-1
data$postinsurrection[data$date=="01-23"]<-1
data$postinsurrection[data$date=="01-24"]<-1
data$postinsurrection[data$date=="01-25"]<-1
data$postinsurrection[data$date=="01-26"]<-1
data$postinsurrection[data$date=="01-27"]<-1
data$postinsurrection[data$date=="01-28"]<-1
data$postinsurrection[data$date=="01-29"]<-1

data$postinauguration<-0
data$postinauguration[data$date=="10-27"]<-0
data$postinauguration[data$date=="10-28"]<-0
data$postinauguration[data$date=="10-29"]<-0
data$postinauguration[data$date=="10-30"]<-0
data$postinauguration[data$date=="10-31"]<-0
data$postinauguration[data$date=="11-01"]<-0
data$postinauguration[data$date=="11-02"]<-0
data$postinauguration[data$date=="11-03"]<-0
data$postinauguration[data$date=="11-04"]<-0
data$postinauguration[data$date=="11-05"]<-0
data$postinauguration[data$date=="11-06"]<-0
data$postinauguration[data$date=="11-07"]<-0
data$postinauguration[data$date=="11-08"]<-0
data$postinauguration[data$date=="11-09"]<-0
data$postinauguration[data$date=="11-10"]<-0
data$postinauguration[data$date=="11-11"]<-0
data$postinauguration[data$date=="11-12"]<-0
data$postinauguration[data$date=="11-13"]<-0
data$postinauguration[data$date=="11-14"]<-0
data$postinauguration[data$date=="11-15"]<-0
data$postinauguration[data$date=="11-16"]<-0
data$postinauguration[data$date=="11-17"]<-0
data$postinauguration[data$date=="11-18"]<-0
data$postinauguration[data$date=="11-19"]<-0
data$postinauguration[data$date=="11-20"]<-0
data$postinauguration[data$date=="11-21"]<-0
data$postinauguration[data$date=="11-24"]<-0
data$postinauguration[data$date=="11-25"]<-0
data$postinauguration[data$date=="11-26"]<-0
data$postinauguration[data$date=="11-27"]<-0
data$postinauguration[data$date=="11-28"]<-0
data$postinauguration[data$date=="11-29"]<-0
data$postinauguration[data$date=="11-30"]<-0
data$postinauguration[data$date=="12-01"]<-0
data$postinauguration[data$date=="12-02"]<-0
data$postinauguration[data$date=="12-03"]<-0
data$postinauguration[data$date=="12-04"]<-0
data$postinauguration[data$date=="12-05"]<-0
data$postinauguration[data$date=="12-06"]<-0
data$postinauguration[data$date=="12-07"]<-0
data$postinauguration[data$date=="12-08"]<-0
data$postinauguration[data$date=="12-09"]<-0
data$postinauguration[data$date=="12-10"]<-0
data$postinauguration[data$date=="12-11"]<-0
data$postinauguration[data$date=="12-12"]<-0
data$postinauguration[data$date=="12-13"]<-0
data$postinauguration[data$date=="12-14"]<-0
data$postinauguration[data$date=="12-15"]<-0
data$postinauguration[data$date=="12-16"]<-0
data$postinauguration[data$date=="12-17"]<-0
data$postinauguration[data$date=="12-18"]<-0
data$postinauguration[data$date=="12-19"]<-0
data$postinauguration[data$date=="12-20"]<-0
data$postinauguration[data$date=="12-21"]<-0
data$postinauguration[data$date=="12-22"]<-0
data$postinauguration[data$date=="12-23"]<-0
data$postinauguration[data$date=="12-24"]<-0
data$postinauguration[data$date=="12-25"]<-0
data$postinauguration[data$date=="12-26"]<-0
data$postinauguration[data$date=="12-27"]<-0
data$postinauguration[data$date=="12-28"]<-0
data$postinauguration[data$date=="12-29"]<-0
data$postinauguration[data$date=="12-30"]<-0
data$postinauguration[data$date=="12-31"]<-0
data$postinauguration[data$date=="01-01"]<-0
data$postinauguration[data$date=="01-02"]<-0
data$postinauguration[data$date=="01-03"]<-0
data$postinauguration[data$date=="01-04"]<-0
data$postinauguration[data$date=="01-05"]<-0
data$postinauguration[data$date=="01-06"]<-0
data$postinauguration[data$date=="01-07"]<-0
data$postinauguration[data$date=="01-08"]<-0
data$postinauguration[data$date=="01-09"]<-0
data$postinauguration[data$date=="01-10"]<-0
data$postinauguration[data$date=="01-11"]<-0
data$postinauguration[data$date=="01-12"]<-0
data$postinauguration[data$date=="01-13"]<-0
data$postinauguration[data$date=="01-14"]<-0
data$postinauguration[data$date=="01-15"]<-0
data$postinauguration[data$date=="01-16"]<-0
data$postinauguration[data$date=="01-17"]<-0
data$postinauguration[data$date=="01-18"]<-0
data$postinauguration[data$date=="01-19"]<-0
data$postinauguration[data$date=="01-20"]<-0
data$postinauguration[data$date=="01-21"]<-1
data$postinauguration[data$date=="01-22"]<-1
data$postinauguration[data$date=="01-23"]<-1
data$postinauguration[data$date=="01-24"]<-1
data$postinauguration[data$date=="01-25"]<-1
data$postinauguration[data$date=="01-26"]<-1
data$postinauguration[data$date=="01-27"]<-1
data$postinauguration[data$date=="01-28"]<-1
data$postinauguration[data$date=="01-29"]<-1

###DEMOGRAPHIC VARIABLES###

#female
data$female<-NA
data$female[data$D1=="Male"]<-0
data$female[data$D1=="Female"]<-1
summary(data$female)

data$gender<-NA
data$gender[data$D1=="Male"]<-"Male"
data$gender[data$D1=="Female"]<-"Female"

#age
data$age<-NA
data$age<-2020-as.numeric(paste(data$D3))
data$age[data$D3==18]<-18
data$age[data$D3==19]<-19
data$age[data$D3==20]<-20
data$age[data$D3==21]<-21
data$age[data$D3==22]<-22
data$age[data$D3==23]<-23
data$age[data$D3==24]<-24
data$age[data$D3==25]<-25
data$age[data$D3==26]<-26
data$age[data$D3==27]<-27
data$age[data$D3==28]<-28
data$age[data$D3==29]<-29
data$age[data$D3==30]<-30
data$age[data$D3==31]<-31
data$age[data$D3==32]<-32
data$age[data$D3==33]<-33
data$age[data$D3==34]<-34
data$age[data$D3==35]<-35
data$age[data$D3==36]<-36
data$age[data$D3==37]<-37
data$age[data$D3==38]<-38
data$age[data$D3==39]<-39
data$age[data$D3==40]<-40
data$age[data$D3==41]<-41
data$age[data$D3==42]<-42
data$age[data$D3==43]<-43
data$age[data$D3==44]<-44
data$age[data$D3==45]<-45
data$age[data$D3==46]<-46
data$age[data$D3==47]<-47
data$age[data$D3==48]<-48
data$age[data$D3==49]<-49
data$age[data$D3==50]<-50
data$age[data$D3==51]<-51
data$age[data$D3==52]<-52
data$age[data$D3==53]<-53
data$age[data$D3==54]<-54
data$age[data$D3==55]<-55
data$age[data$D3==56]<-56
data$age[data$D3==57]<-57
data$age[data$D3==58]<-58
data$age[data$D3==59]<-59
data$age[data$D3==60]<-60
data$age[data$D3==61]<-61
data$age[data$D3==62]<-62
data$age[data$D3==63]<-63
data$age[data$D3==64]<-64
data$age[data$D3==65]<-65
data$age[data$D3==66]<-66
data$age[data$D3==67]<-67
data$age[data$D3==68]<-68
data$age[data$D3==69]<-69
data$age[data$D3==70]<-70
data$age[data$D3==71]<-71
data$age[data$D3==72]<-72
data$age[data$D3==73]<-73
data$age[data$D3==74]<-74
data$age[data$D3==75]<-75
data$age[data$D3==76]<-76
data$age[data$D3==77]<-77
data$age[data$D3==78]<-78
data$age[data$D3==79]<-79
data$age[data$D3==80]<-80
data$age[data$D3==81]<-81
data$age[data$D3==82]<-82
data$age[data$D3==83]<-83
data$age[data$D3==84]<-84
data$age[data$D3==85]<-85
data$age[data$D3==86]<-86
data$age[data$D3==87]<-87
data$age[data$D3==88]<-88
data$age[data$D3==89]<-89
data$age[data$D3==90]<-90
data$age[data$D3==91]<-91
data$age[data$D3==92]<-92
data$age[data$D3==93]<-93
data$age[data$D3==94]<-94
data$age[data$D3==95]<-95
data$age[data$D3==96]<-96
data$age[data$D3==97]<-97
data$age[data$D3==98]<-98
data$age[data$D3==99]<-99

data$age.1829<-NA
data$age.1829[data$age<30]<-1
data$age.1829[data$age>29]<-0

data$age.3049<-NA
data$age.3049[data$age<30]<-0
data$age.3049[data$age>29]<-1
data$age.3049[data$age>49]<-0

data$age.5064<-NA
data$age.5064[data$age<50]<-0
data$age.5064[data$age>49]<-1
data$age.5064[data$age>64]<-0

data$age.65plus<-NA
data$age.65plus[data$age<65]<-0
data$age.65plus[data$age>64]<-1

summary(data$age.1829)
summary(data$age.3049)

data$age.group=NA
data$age.group[data$age.1829==1]="18-29"
data$age.group[data$age.3049==1]="30-49"
data$age.group[data$age.5064==1]="50-64"
data$age.group[data$age.65plus==1]="65+"

#state
data$state.alabama<-0
data$state.alaska<-0
data$state.arizona<-0
data$state.arkansas<-0
data$state.california<-0
data$state.colorado<-0
data$state.connecticut<-0
data$state.delaware<-0
data$state.dc<-0
data$state.florida<-0
data$state.georgia<-0
data$state.hawaii<-0
data$state.idaho<-0
data$state.illinois<-0
data$state.indiana<-0
data$state.iowa<-0
data$state.kansas<-0
data$state.kentucky<-0
data$state.louisiana<-0
data$state.maine<-0
data$state.maryland<-0
data$state.massachusetts<-0
data$state.michigan<-0
data$state.minnesota<-0
data$state.mississippi<-0
data$state.missouri<-0
data$state.montana<-0
data$state.nebraska<-0
data$state.nevada<-0
data$state.newhampshire<-0
data$state.newjersey<-0
data$state.newmexico<-0
data$state.newyork<-0
data$state.northcarolina<-0
data$state.northdakota<-0
data$state.ohio<-0
data$state.oklahoma<-0
data$state.oregon<-0
data$state.pennsylvania<-0
data$state.rhodeisland<-0
data$state.southcarolina<-0
data$state.southdakota<-0
data$state.tennessee<-0
data$state.texas<-0
data$state.utah<-0
data$state.vermont<-0
data$state.virginia<-0
data$state.washington<-0
data$state.westvirginia<-0
data$state.wisconsin<-0
data$state.wyoming<-0

data$state.alabama[data$D2=="Alabama"]<-1
data$state.alaska[data$D2=="Alaska"]<-1
data$state.arizona[data$D2=="Arizona"]<-1
data$state.arkansas[data$D2=="Arkansas"]<-1
data$state.california[data$D2=="California"]<-1
data$state.colorado[data$D2=="Colorado"]<-1
data$state.connecticut[data$D2=="Connecticut"]<-1
data$state.delaware[data$D2=="Delaware"]<-1
data$state.florida[data$D2=="Florida"]<-1
data$state.georgia[data$D2=="Georgia"]<-1
data$state.hawaii[data$D2=="Hawaii"]<-1
data$state.idaho[data$D2=="Idaho"]<-1
data$state.illinois[data$D2=="Illinois"]<-1
data$state.indiana[data$D2=="Indiana"]<-1
data$state.iowa[data$D2=="Iowa"]<-1
data$state.kansas[data$D2=="Kansas"]<-1
data$state.kentucky[data$D2=="Kentucky"]<-1
data$state.louisiana[data$D2=="Louisiana"]<-1
data$state.maine[data$D2=="Maine"]<-1
data$state.maryland[data$D2=="Maryland"]<-1
data$state.massachusetts[data$D2=="Massachusetts"]<-1
data$state.michigan[data$D2=="Michigan"]<-1
data$state.minnesota[data$D2=="Minnesota"]<-1
data$state.mississippi[data$D2=="Mississippi"]<-1
data$state.missouri[data$D2=="Missouri"]<-1
data$state.montana[data$D2=="Montana"]<-1
data$state.nebraska[data$D2=="Nebraska"]<-1
data$state.nevada[data$D2=="Nevada"]<-1
data$state.newhampshire[data$D2=="New Hampshire"]<-1
data$state.newjersey[data$D2=="New Jersey"]<-1
data$state.newmexico[data$D2=="New Mexico"]<-1
data$state.newyork[data$D2=="New York"]<-1
data$state.northcarolina[data$D2=="North Carolina"]<-1
data$state.northdakota[data$D2=="North Dakota"]<-1
data$state.ohio[data$D2=="Ohio"]<-1
data$state.oklahoma[data$D2=="Oklahoma"]<-1
data$state.oregon[data$D2=="Oregon"]<-1
data$state.pennsylvania[data$D2=="Pennsylvania"]<-1
data$state.rhodeisland[data$D2=="Rhode Island"]<-1
data$state.southcarolina[data$D2=="South Carolina"]<-1
data$state.southdakota[data$D2=="South Dakota"]<-1
data$state.tennessee[data$D2=="Tennessee"]<-1
data$state.texas[data$D2=="Texas"]<-1
data$state.utah[data$D2=="Utah"]<-1
data$state.vermont[data$D2=="Vermont"]<-1
data$state.virginia[data$D2=="Virginia"]<-1
data$state.washington[data$D2=="Washington"]<-1
data$state.westvirginia[data$D2=="West Virginia"]<-1
data$state.wisconsin[data$D2=="Wisconsin"]<-1
data$state.wyoming[data$D2=="Wyoming"]<-1

ftable(data$state.alabama)
ftable(data$state.alaska)
ftable(data$state.arizona)
ftable(data$state.arkansas)
ftable(data$state.california)
ftable(data$state.colorado)
ftable(data$state.connecticut)
ftable(data$state.delaware)
ftable(data$state.florida)
ftable(data$state.georgia)
ftable(data$state.hawaii)
ftable(data$state.idaho)
ftable(data$state.illinois)
ftable(data$state.indiana)
ftable(data$state.iowa)
ftable(data$state.kansas)
ftable(data$state.kentucky)
ftable(data$state.louisiana)
ftable(data$state.maine)
ftable(data$state.maryland)
ftable(data$state.massachusetts)
ftable(data$state.michigan)
ftable(data$state.minnesota)
ftable(data$state.mississippi)
ftable(data$state.missouri)
ftable(data$state.montana)
ftable(data$state.nebraska)
ftable(data$state.nevada)
ftable(data$state.newhampshire)
ftable(data$state.newjersey)
ftable(data$state.newmexico)
ftable(data$state.newyork)
ftable(data$state.northcarolina)
ftable(data$state.northdakota)
ftable(data$state.ohio)
ftable(data$state.oklahoma)
ftable(data$state.oregon)
ftable(data$state.pennsylvania)
ftable(data$state.rhodeisland)
ftable(data$state.southcarolina)
ftable(data$state.southdakota)
ftable(data$state.tennessee)
ftable(data$state.texas)
ftable(data$state.utah)
ftable(data$state.vermont)
ftable(data$state.virginia)
ftable(data$state.washington)
ftable(data$state.westvirginia)
ftable(data$state.wisconsin)
ftable(data$state.wyoming)

#region
data$region.northeast<-0
data$region.midwest<-0
data$region.south<-0
data$region.west<-0

data$region.south[data$D2=="Alabama"]<-1
data$region.west[data$D2=="Alaska"]<-1
data$region.west[data$D2=="Arizona"]<-1
data$region.south[data$D2=="Arkansas"]<-1
data$region.west[data$D2=="California"]<-1
data$region.west[data$D2=="Colorado"]<-1
data$region.northeast[data$D2=="Connecticut"]<-1
data$region.south[data$D2=="Delaware"]<-1
data$region.south[data$D2=="Florida"]<-1
data$region.south[data$D2=="Georgia"]<-1
data$region.west[data$D2=="Hawaii"]<-1
data$region.midwest[data$D2=="Idaho"]<-1
data$region.midwest[data$D2=="Illinois"]<-1
data$region.midwest[data$D2=="Indiana"]<-1
data$region.midwest[data$D2=="Iowa"]<-1
data$region.midwest[data$D2=="Kansas"]<-1
data$region.south[data$D2=="Kentucky"]<-1
data$region.south[data$D2=="Louisiana"]<-1
data$region.northeast[data$D2=="Maine"]<-1
data$region.south[data$D2=="Maryland"]<-1
data$region.northeast[data$D2=="Massachusetts"]<-1
data$region.midwest[data$D2=="Michigan"]<-1
data$region.midwest[data$D2=="Minnesota"]<-1
data$region.south[data$D2=="Mississippi"]<-1
data$region.south[data$D2=="Missouri"]<-1
data$region.west[data$D2=="Montana"]<-1
data$region.midwest[data$D2=="Nebraska"]<-1
data$region.west[data$D2=="Nevada"]<-1
data$region.northeast[data$D2=="New Hampshire"]<-1
data$region.northeast[data$D2=="New Jersey"]<-1
data$region.west[data$D2=="New Mexico"]<-1
data$region.northeast[data$D2=="New York"]<-1
data$region.south[data$D2=="North Carolina"]<-1
data$region.midwest[data$D2=="North Dakota"]<-1
data$region.midwest[data$D2=="Ohio"]<-1
data$region.south[data$D2=="Oklahoma"]<-1
data$region.west[data$D2=="Oregon"]<-1
data$region.northeast[data$D2=="Pennsylvania"]<-1
data$region.northeast[data$D2=="Rhode Island"]<-1
data$region.south[data$D2=="South Carolina"]<-1
data$region.west[data$D2=="South Dakota"]<-1
data$region.south[data$D2=="Tennessee"]<-1
data$region.south[data$D2=="Texas"]<-1
data$region.west[data$D2=="Utah"]<-1
data$region.northeast[data$D2=="Vermont"]<-1
data$region.south[data$D2=="Virginia"]<-1
data$region.west[data$D2=="Washington"]<-1
data$region.south[data$D2=="West Virginia"]<-1
data$region.midwest[data$D2=="Wisconsin"]<-1
data$region.west[data$D2=="Wyoming"]<-1

#lowed
data$lowed<-NA
data$lowed[data$D4=="Did not graduate from high school"]<-1
data$lowed[data$D4=="High school graduate"]<-1
data$lowed[data$D4=="2-year college degree"]<-1
data$lowed[data$D4=="Some college, but no degree (yet)"]<-1
data$lowed[data$D4=="4-year college degree"]<-0
data$lowed[data$D4=="Postgraduate degree (MA, MBA, MD, JD, PhD, etc.)"]<-0
summary(data$lowed)

ftable(data$D4)

data$ed.hs<-NA
data$ed.hs[data$D4=="Did not graduate from high school"]<-1
data$ed.hs[data$D4=="High school graduate"]<-1
data$ed.hs[data$D4=="2-year college degree"]<-0
data$ed.hs[data$D4=="Some college, but no degree (yet)"]<-0
data$ed.hs[data$D4=="4-year college degree"]<-0
data$ed.hs[data$D4=="Postgraduate degree (MA, MBA, MD, JD, PhD, etc.)"]<-0

data$ed.college<-NA
data$ed.college[data$D4=="Did not graduate from high school"]<-0
data$ed.college[data$D4=="High school graduate"]<-0
data$ed.college[data$D4=="2-year college degree"]<-1
data$ed.college[data$D4=="Some college but no degree (yet)"]<-1
data$ed.college[data$D4=="4-year college degree"]<-1
data$ed.college[data$D4=="Postgraduate degree (MA, MBA, MD, JD, PhD, etc.)"]<-0

data$ed.graduate<-NA
data$ed.graduate[data$D4=="Did not graduate from high school"]<-0
data$ed.graduate[data$D4=="High school graduate"]<-0
data$ed.graduate[data$D4=="2-year college degree"]<-0
data$ed.graduate[data$D4=="Some college but no degree (yet)"]<-0
data$ed.graduate[data$D4=="4-year college degree"]<-0
data$ed.graduate[data$D4=="Postgraduate degree (MA, MBA, MD, JD, PhD, etc.)"]<-1

data$ed.group<-NA
data$ed.group[data$D4=="Did not graduate from high school"]<-"HS"
data$ed.group[data$D4=="High school graduate"]<-"HS"
data$ed.group[data$D4=="2-year college degree"]<-"College"
data$ed.group[data$D4=="Some college but no degree (yet)"]<-"College"
data$ed.group[data$D4=="4-year college degree"]<-"College"
data$ed.group[data$D4=="Postgraduate degree (MA, MBA, MD, JD, PhD, etc.)"]<-"Postgrad"

#race
data$race.white<-0
data$race.white[data$D5=="White"]<-1
summary(data$race.white)

data$race.black<-0
data$race.black[data$D5=="Black or African-American"]<-1
summary(data$race.black)

data$race.hispanic<-0
data$race.hispanic[data$D5=="Hispanic or Latino"]<-1
summary(data$race.hispanic)

data$race.other<-0
data$race.other[data$D5=="Other"]<-1
data$race.other[data$D5=="Native American"]<-1
data$race.other[data$D5=="Middle Eastern"]<-1
data$race.asian[data$D5=="Asian or Asian-American"]<-1
summary(data$race.other)

data$race.group<-data$D5
data$race.group[data$D5=="Other"]<-"Other"
data$race.group[data$D5=="Native American"]<-"Other"
data$race.group[data$D5=="Middle Eastern"]<-"Other"
data$race.group[data$D5=="Asian or Asian-American"]<-"Other"

#social.status
data$social.status<-NA
data$social.status[data$Q93=="1 - Bottom of ladder"]<-1
data$social.status[data$Q93=="2"]<-2
data$social.status[data$Q93=="3"]<-3
data$social.status[data$Q93=="4"]<-4
data$social.status[data$Q93=="5"]<-5
data$social.status[data$Q93=="6"]<-6
data$social.status[data$Q93=="7"]<-7
data$social.status[data$Q93=="8"]<-8
data$social.status[data$Q93=="9"]<-9
data$social.status[data$Q93=="10 - Top of ladder"]<-10

data$social.status.group<-NA
data$social.status.group[data$Q93=="1 - Bottom of ladder"]<-"Low"
data$social.status.group[data$Q93=="2"]<-"Low"
data$social.status.group[data$Q93=="3"]<-"Low"
data$social.status.group[data$Q93=="4"]<-"Medium"
data$social.status.group[data$Q93=="5"]<-"Medium"
data$social.status.group[data$Q93=="6"]<-"Medium"
data$social.status.group[data$Q93=="7"]<-"Medium"
data$social.status.group[data$Q93=="8"]<-"High"
data$social.status.group[data$Q93=="9"]<-"High"
data$social.status.group[data$Q93=="10 - Top of ladder"]<-"High"

hist(data$social.status)

###POLITICAL ATTITUDES AND VOTING BEHAVIOR###

#party
data$party.dem<-0
data$party.dem[data$P2a=="Democrat"]<-1
summary(data$party.dem)

data$party.rep<-0
data$party.rep[data$P2a=="Republican"]<-1
summary(data$party.rep)

data$party.indother<-0
data$party.indother[data$P2a=="Independent"]<-1
data$party.indother[data$P2a=="Other"]<-1
summary(data$party.indother)

data$party<-data$P2a
data$party[data$P2a=="Independent"]<-"Independent/Other"
data$party[data$P2a=="Other"]<-"Independent/Other"
data$party[data$P2a=="No answer"]<-NA

#vote.gen2016
data$vote.gen2016<-0
data$vote.gen2016[data$P3a=="Yes, I definitely voted"]<-1
summary(data$vote.gen2016)

#vote.trump.gen2016
data$vote.trump.gen2016<-0
data$vote.trump.gen2016[data$P3b=="Donald Trump"]<-1
summary(data$vote.trump.gen2016)

#vote.clinton.gen2016
data$vote.clinton.gen2016<-0
data$vote.clinton.gen2016[data$P3b=="Hillary Clinton"]<-1
summary(data$vote.clinton.gen2016)

#vote.gen2020
data$vote.gen2020<-0
data$vote.gen2020[data$P4a=="Yes I will definitely vote/I already voted"]<-1
summary(data$vote.gen2020)

#vote.trump.gen2020
data$vote.trump.gen2020<-0
data$vote.trump.gen2020[data$P4b1=="Donald Trump"]<-1
summary(data$vote.trump.gen2020)

#vote.biden.gen2020
data$vote.biden.gen2020<-0
data$vote.biden.gen2020[data$P4b1=="Joe Biden"]<-1
summary(data$vote.biden.gen2020)

#vote.gen2020.mail
data$vote.gen2020.mail<-NA
data$vote.gen2020.mail[data$P4c=="In person"]<-0
data$vote.gen2020.mail[data$P4c=="I am not sure"]<-0
data$vote.gen2020.mail[data$P4c=="Mail in ballot"]<-1
summary(data$vote.gen2020.mail)

#trump.refuse
data$trump.refuse<-NA
data$trump.refuse[data$Z1=="No"]<-0
data$trump.refuse[data$Z1=="I am not sure"]<-0
data$trump.refuse[data$Z1=="No answer"]<-0
data$trump.refuse[data$Z1=="Yes"]<-1
ftable(data$Z1)
summary(data$trump.refuse)

#trump.impeach
data$trump.impeach<-NA
data$trump.impeach[data$Z3=="Yes"]<-1
data$trump.impeach[data$Z3=="No"]<-0
data$trump.impeach[data$Z3=="I am not sure"]<-0
data$trump.impeach[data$Z3=="No answer"]<-0
ftable(data$Z3)
summary(data$trump.impeach)

#trump.military
data$trump.military<-NA
data$trump.military[data$Z2=="Yes"]<-1
data$trump.military[data$Z2=="No"]<-0
data$trump.military[data$Z2=="I am not sure"]<-0
data$trump.military[data$Z2=="No answer"]<-0
ftable(data$Z2)
summary(data$trump.military)

#trump.id
data$trump.id<-NA
data$trump.id[data$P4b1=="Donald Trump" & data$P5a=="Strongly agree"]<-5
data$trump.id[data$P4b1=="Donald Trump" & data$P5a=="Agree"]<-4
data$trump.id[data$P4b1=="Donald Trump" & data$P5a=="Neither agree or disagree"]<-3
data$trump.id[data$P4b1=="Donald Trump" & data$P5a=="Disagree"]<-2
data$trump.id[data$P4b1=="Donald Trump" & data$P5a=="Strongly disagree"]<-1
ftable(data$trump.id)

data$trump.id2<-NA
data$trump.id2[data$P4b1=="Donald Trump" & data$P5f=="Strongly agree"]<-5
data$trump.id2[data$P4b1=="Donald Trump" & data$P5f=="Agree"]<-4
data$trump.id2[data$P4b1=="Donald Trump" & data$P5f=="Neither agree or disagree"]<-3
data$trump.id2[data$P4b1=="Donald Trump" & data$P5f=="Disagree"]<-2
data$trump.id2[data$P4b1=="Donald Trump" & data$P5f=="Strongly disagree"]<-1
ftable(data$trump.id2)

data$biden.id<-NA
data$biden.id[data$P4b1=="Joe Biden" & data$P5a=="Strongly agree"]<-5
data$biden.id[data$P4b1=="Joe Biden" & data$P5a=="Agree"]<-4
data$biden.id[data$P4b1=="Joe Biden" & data$P5a=="Neither agree or disagree"]<-3
data$biden.id[data$P4b1=="Joe Biden" & data$P5a=="Disagree"]<-2
data$biden.id[data$P4b1=="Joe Biden" & data$P5a=="Strongly disagree"]<-1
ftable(data$biden.id)

data$biden.id2<-NA
data$biden.id2[data$P4b1=="Joe Biden" & data$P5f=="Strongly agree"]<-5
data$biden.id2[data$P4b1=="Joe Biden" & data$P5f=="Agree"]<-4
data$biden.id2[data$P4b1=="Joe Biden" & data$P5f=="Neither agree or disagree"]<-3
data$biden.id2[data$P4b1=="Joe Biden" & data$P5f=="Disagree"]<-2
data$biden.id2[data$P4b1=="Joe Biden" & data$P5f=="Strongly disagree"]<-1
ftable(data$biden.id2)

data$trump.id.mean = rowMeans(data[,c("trump.id", "trump.id2")], na.rm=TRUE)
data$biden.id.mean = rowMeans(data[,c("biden.id", "biden.id2")], na.rm=TRUE)

mean(data$trump.id.mean, na.rm=TRUE)
sd(data$trump.id.mean, na.rm=TRUE)

data$dem.therm<-as.numeric(paste(data$ThermD_1))
data$rep.therm<-as.numeric(paste(data$ThermR_1))
data$polarization<-abs(data$dem.therm-data$rep.therm)
hist(data$polarization)

###Make Demographic Dashboard###

require(dplyr)

means<-  data.frame(group_by(data, date) %>% 
    summarise(female = mean(female, na.rm = TRUE)))
  
vars<-c("lowed","race.white","race.black","race.hispanic","race.other","party.dem","party.rep","party.indother","vote.gen2016","vote.clinton.gen2016","vote.trump.gen2016","vote.gen2020","vote.biden.gen2020","vote.trump.gen2020","ed.hs","ed.college","ed.graduate","age.1829","age.3049","age.5064","age.65plus")  

for (j in vars) {
    try(eval(parse(text=paste("mean.",j,"<-  data.frame(group_by(data, date) %>% 
    summarise(",j," = mean(",j,", na.rm = TRUE),
    ))",sep=""))))
  try(eval(parse(text=paste("means<-merge(means, mean.",j,", by='date')",sep=""))))
}    

means

means$date.num<-NA
means$date.num[means$date=="10-27"]<-1
means$date.num[means$date=="10-28"]<-2
means$date.num[means$date=="10-29"]<-3
means$date.num[means$date=="10-30"]<-4
means$date.num[means$date=="10-31"]<-5
means$date.num[means$date=="11-01"]<-6
means$date.num[means$date=="11-02"]<-7
means$date.num[means$date=="11-03"]<-8
means$date.num[means$date=="11-04"]<-9
means$date.num[means$date=="11-05"]<-10
means$date.num[means$date=="11-06"]<-11
means$date.num[means$date=="11-07"]<-12
means$date.num[means$date=="11-08"]<-13
means$date.num[means$date=="11-09"]<-14
means$date.num[means$date=="11-10"]<-15
means$date.num[means$date=="11-11"]<-16
means$date.num[means$date=="11-12"]<-17
means$date.num[means$date=="11-13"]<-18
means$date.num[means$date=="11-14"]<-19
means$date.num[means$date=="11-15"]<-20
means$date.num[means$date=="11-16"]<-21
means$date.num[means$date=="11-17"]<-22
means$date.num[means$date=="11-18"]<-23
means$date.num[means$date=="11-19"]<-24
means$date.num[means$date=="11-20"]<-25
means$date.num[means$date=="11-21"]<-26
means$date.num[means$date=="11-24"]<-29
means$date.num[means$date=="11-25"]<-30
means$date.num[means$date=="11-26"]<-31
means$date.num[means$date=="11-27"]<-32
means$date.num[means$date=="11-28"]<-33
means$date.num[means$date=="11-29"]<-34
means$date.num[means$date=="11-30"]<-35
means$date.num[means$date=="12-01"]<-36
means$date.num[means$date=="12-02"]<-37
means$date.num[means$date=="12-03"]<-38
means$date.num[means$date=="12-04"]<-39
means$date.num[means$date=="12-05"]<-40
means$date.num[means$date=="12-06"]<-41
means$date.num[means$date=="12-07"]<-42
means$date.num[means$date=="12-08"]<-43
means$date.num[means$date=="12-09"]<-44
means$date.num[means$date=="12-10"]<-45
means$date.num[means$date=="12-11"]<-46
means$date.num[means$date=="12-12"]<-47
means$date.num[means$date=="12-13"]<-48
means$date.num[means$date=="12-14"]<-49
means$date.num[means$date=="12-15"]<-50
means$date.num[means$date=="12-16"]<-51
means$date.num[means$date=="12-17"]<-52
means$date.num[means$date=="12-18"]<-53
means$date.num[means$date=="12-19"]<-54
means$date.num[means$date=="12-20"]<-55
means$date.num[means$date=="12-21"]<-56
means$date.num[means$date=="12-22"]<-57
means$date.num[means$date=="12-23"]<-58
means$date.num[means$date=="12-24"]<-59
means$date.num[means$date=="12-25"]<-60
means$date.num[means$date=="12-26"]<-61
means$date.num[means$date=="12-27"]<-62
means$date.num[means$date=="12-28"]<-63
means$date.num[means$date=="12-29"]<-64
means$date.num[means$date=="12-30"]<-65
means$date.num[means$date=="12-31"]<-66
means$date.num[means$date=="01-01"]<-67
means$date.num[means$date=="01-02"]<-68
means$date.num[means$date=="01-03"]<-69
means$date.num[means$date=="01-04"]<-70
means$date.num[means$date=="01-05"]<-71
means$date.num[means$date=="01-06"]<-72
means$date.num[means$date=="01-07"]<-73
means$date.num[means$date=="01-08"]<-74
means$date.num[means$date=="01-09"]<-75
means$date.num[means$date=="01-10"]<-76
means$date.num[means$date=="01-11"]<-77
means$date.num[means$date=="01-12"]<-78
means$date.num[means$date=="01-13"]<-79
means$date.num[means$date=="01-14"]<-80
means$date.num[means$date=="01-15"]<-81
means$date.num[means$date=="01-16"]<-82
means$date.num[means$date=="01-17"]<-83
means$date.num[means$date=="01-18"]<-84
means$date.num[means$date=="01-19"]<-85
means$date.num[means$date=="01-20"]<-86
means$date.num[means$date=="01-21"]<-87
means$date.num[means$date=="01-22"]<-88
means$date.num[means$date=="01-23"]<-89
means$date.num[means$date=="01-24"]<-90
means$date.num[means$date=="01-25"]<-91
means$date.num[means$date=="01-26"]<-92
means$date.num[means$date=="01-27"]<-93
means$date.num[means$date=="01-28"]<-94
means$date.num[means$date=="01-29"]<-95


means$date[means$date=="10-28"]<-"."
means$date[means$date=="10-30"]<-"."
means$date[means$date=="11-01"]<-"."
means$date[means$date=="11-03"]<-"."
means$date[means$date=="11-05"]<-"."
means$date[means$date=="11-07"]<-"."
means$date[means$date=="11-09"]<-"."
means$date[means$date=="11-11"]<-"."
means$date[means$date=="11-13"]<-"."
means$date[means$date=="11-15"]<-"."
means$date[means$date=="11-17"]<-"."
means$date[means$date=="11-19"]<-"."

ftable(means$date.num)

write.csv(means,"sample.summary.csv")

pdf('fig-sample-overview1.pdf', width=8, height=10)
p1<-ggplot(means, aes(x=date.num, y=race.white)) + geom_point(alpha=.5) + geom_hline(yintercept=.75, lty=2, color="grey40") + theme_bw() + xlab("Date")+ ylab("Sample Proportion - White") + ylim(c(0,1)) + guides(color=guide_legend(title=NULL)) + ggtitle("Sample - White") + scale_color_manual(values=c("dodgerblue3")) + theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=7))  + scale_x_continuous(breaks=means$date.num, labels=means$date) + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + theme(plot.title = element_text(size = 11), axis.title.y = element_text(size = 11))
p2<-ggplot(means, aes(x=date.num, y=race.black)) + geom_point(alpha=.5) + geom_hline(yintercept=.09, lty=2, color="grey40") + theme_bw() + xlab("Date")+ ylab("Sample Proportion - Black") + ylim(c(0,1)) + guides(color=guide_legend(title=NULL)) + ggtitle("Sample - Black") + scale_color_manual(values=c("dodgerblue3")) + theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=7))  + scale_x_continuous(breaks=means$date.num, labels=means$date) + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + theme(plot.title = element_text(size = 11), axis.title.y = element_text(size = 11))
p3<-ggplot(means, aes(x=date.num, y=race.hispanic)) + geom_point(alpha=.5) + geom_hline(yintercept=.08, lty=2, color="grey40") + theme_bw() + xlab("Date")+ ylab("Sample Proportion - Hispanic") + ylim(c(0,1)) + guides(color=guide_legend(title=NULL)) + ggtitle("Sample - Hispanic") + scale_color_manual(values=c("dodgerblue3")) + theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=7)) + scale_x_continuous(breaks=means$date.num, labels=means$date) + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())  + theme(plot.title = element_text(size = 11), axis.title.y = element_text(size = 11))
p4<-ggplot(means, aes(x=date.num, y=race.other)) + geom_point(alpha=.5) + geom_hline(yintercept=.06, lty=2, color="grey40") + theme_bw() + xlab("Date")+ ylab("Sample Proportion - Other Race") + ylim(c(0,1)) + guides(color=guide_legend(title=NULL)) + ggtitle("Sample - Other Race") + scale_color_manual(values=c("dodgerblue3")) + theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=7))+ scale_x_continuous(breaks=means$date.num, labels=means$date) + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + theme(plot.title = element_text(size = 11), axis.title.y = element_text(size = 11)) 
p5<-ggplot(means, aes(x=date.num, y=age.1829)) + geom_point(alpha=.5) + geom_hline(yintercept=.11, lty=2, color="grey40") + theme_bw() + xlab("Date")+ ylab("Sample Proportion - Age 18-29") + ylim(c(0,1)) + guides(color=guide_legend(title=NULL)) + ggtitle("Sample - Age 18-29") + scale_color_manual(values=c("dodgerblue3")) + theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=7))  + scale_x_continuous(breaks=means$date.num, labels=means$date) + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())+ theme(plot.title = element_text(size = 11), axis.title.y = element_text(size = 11))
p6<-ggplot(means, aes(x=date.num, y=age.3049)) + geom_point(alpha=.5) + geom_hline(yintercept=.30, lty=2, color="grey40") + theme_bw() + xlab("Date")+ ylab("Sample Proportion - Age 30-49") + ylim(c(0,1)) + guides(color=guide_legend(title=NULL)) + ggtitle("Sample - Age 30-49") + scale_color_manual(values=c("dodgerblue3")) + theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=7)) + scale_x_continuous(breaks=means$date.num, labels=means$date) + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + theme(plot.title = element_text(size = 11), axis.title.y = element_text(size = 11))
p7<-ggplot(means, aes(x=date.num, y=age.5064)) + geom_point(alpha=.5) + geom_hline(yintercept=.29, lty=2, color="grey40") + theme_bw() + xlab("Date")+ ylab("Sample Proportion - Age 50-64") + ylim(c(0,1)) + guides(color=guide_legend(title=NULL)) + ggtitle("Sample - Age 50-64") + scale_color_manual(values=c("dodgerblue3")) + theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=7))  + scale_x_continuous(breaks=means$date.num, labels=means$date) + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())+ theme(plot.title = element_text(size = 11), axis.title.y = element_text(size = 11))
p8<-ggplot(means, aes(x=date.num, y=age.65plus)) + geom_point(alpha=.5) + geom_hline(yintercept=.31, lty=2, color="grey40") + theme_bw() + xlab("Date")+ ylab("Sample Proportion - Age 65+") + ylim(c(0,1)) + guides(color=guide_legend(title=NULL)) + ggtitle("Sample - Age 65+") + scale_color_manual(values=c("dodgerblue3")) + theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=7)) + scale_x_continuous(breaks=means$date.num, labels=means$date) + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + theme(plot.title = element_text(size = 11), axis.title.y = element_text(size = 11))
arrange_ggplot2(p1,p2,p3,p4,p5,p6,p7,p8, nrow=4, ncol=2)
dev.off()

pdf('fig-sample-overview2.pdf', width=8, height=10)
p9<-ggplot(means, aes(x=date.num, y=party.dem)) + geom_point(alpha=.5) + geom_hline(yintercept=.35, lty=2, color="grey40") + theme_bw() + xlab("Date")+ ylab("Sample Proportion - Democrat") + ylim(c(0,1)) + guides(color=guide_legend(title=NULL)) + ggtitle("Sample - Democrat") + scale_color_manual(values=c("dodgerblue3")) + theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=7)) + scale_x_continuous(breaks=means$date.num, labels=means$date) + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + theme(plot.title = element_text(size = 11), axis.title.y = element_text(size = 11))
p10<-ggplot(means, aes(x=date.num, y=party.indother)) + geom_point(alpha=.5) + geom_hline(yintercept=.33, lty=2, color="grey40") + theme_bw() + xlab("Date")+ ylab("Sample Proportion - Independent/Other") + ylim(c(0,1)) + guides(color=guide_legend(title=NULL)) + ggtitle("Sample - Ind/Other") + scale_color_manual(values=c("dodgerblue3")) + theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=7)) + scale_x_continuous(breaks=means$date.num, labels=means$date) + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + theme(plot.title = element_text(size = 11), axis.title.y = element_text(size = 11))
p11<-ggplot(means, aes(x=date.num, y=party.rep)) + geom_point(alpha=.5) + geom_hline(yintercept=.31, lty=2, color="grey40") + theme_bw() + xlab("Date")+ ylab("Sample Proportion - Republican") + ylim(c(0,1)) + guides(color=guide_legend(title=NULL)) + ggtitle("Sample - Republican") + scale_color_manual(values=c("dodgerblue3")) + theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=7))  + scale_x_continuous(breaks=means$date.num, labels=means$date) + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + theme(plot.title = element_text(size = 11), axis.title.y = element_text(size = 11))
p12<-ggplot(means, aes(x=date.num, y=ed.hs)) + geom_point(alpha=.5) + geom_hline(yintercept=.25, lty=2, color="grey40") + theme_bw() + xlab("Date")+ ylab("Sample Proportion - HS") + ylim(c(0,1)) + guides(color=guide_legend(title=NULL)) + ggtitle("Sample - High School Degree or less") + scale_color_manual(values=c("dodgerblue3")) + theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=7))  + scale_x_continuous(breaks=means$date.num, labels=means$date) + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + theme(plot.title = element_text(size = 11), axis.title.y = element_text(size = 11))
p13<-ggplot(means, aes(x=date.num, y=ed.college)) + geom_point(alpha=.5) + geom_hline(yintercept=.56, lty=2, color="grey40") + theme_bw() + xlab("Date")+ ylab("Sample Proportion - College") + ylim(c(0,1)) + guides(color=guide_legend(title=NULL)) + ggtitle("Sample - College Degree/Some College") + scale_color_manual(values=c("dodgerblue3")) + theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=7))  + scale_x_continuous(breaks=means$date.num, labels=means$date) + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + theme(plot.title = element_text(size = 11), axis.title.y = element_text(size = 11))
p14<-ggplot(means, aes(x=date.num, y=ed.graduate)) + geom_point(alpha=.5) + geom_hline(yintercept=.19, lty=2, color="grey40") + theme_bw() + xlab("Date")+ ylab("Sample Proportion - Postgrad") + ylim(c(0,1)) + guides(color=guide_legend(title=NULL)) + ggtitle("Sample - Postgraduate Degree") + scale_color_manual(values=c("dodgerblue3")) + theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=7)) + scale_x_continuous(breaks=means$date.num, labels=means$date) + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + theme(plot.title = element_text(size = 11), axis.title.y = element_text(size = 11))
p15<-ggplot(means, aes(x=date.num, y=female)) + geom_point(alpha=.5) + geom_hline(yintercept=.51, lty=2, color="grey40") + theme_bw() + xlab("Date")+ ylab("Sample Proportion - Female") + ylim(c(0,1)) + guides(color=guide_legend(title=NULL)) + ggtitle("Sample - Female") + scale_color_manual(values=c("dodgerblue3")) + theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=7))  + scale_x_continuous(breaks=means$date.num, labels=means$date) + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())+ theme(plot.title = element_text(size = 11), axis.title.y = element_text(size = 11))
arrange_ggplot2(p9,p10,p11,p12,p13,p14,p15, nrow=4, ncol=2)
dev.off()


###WHAT WOULD IT TAKE###

data$believe.repleader<-NA
data$believe.repleader[data$E4JB_1=="Yes"]<-1
data$believe.repleader[data$E4JB_1=="No"]<-0
data$believe.repleader[data$E4JB_1=="No answer"]<-0 
ftable(data$E4JB_1)
ftable(data$believe.repleader)

data$believe.supcourt<-NA
data$believe.supcourt[data$E4JB_2=="Yes"]<-1
data$believe.supcourt[data$E4JB_2=="No"]<-0
data$believe.supcourt[data$E4JB_2=="No answer"]<-0 
ftable(data$E4JB_2)
ftable(data$believe.supcourt)

data$believe.supcourt<-NA
data$believe.supcourt[data$E4JB_2=="Yes"]<-1
data$believe.supcourt[data$E4JB_2=="No"]<-0
data$believe.supcourt[data$E4JB_2=="No answer"]<-0 
ftable(data$E4JB_2)
ftable(data$believe.supcourt)

data$believe.electoral<-NA
data$believe.electoral[data$E4JB_3=="Yes"]<-1
data$believe.electoral[data$E4JB_3=="No"]<-0
data$believe.electoral[data$E4JB_3=="No answer"]<-0 
ftable(data$E4JB_3)
ftable(data$believe.electoral)

data$believe.congress<-NA
data$believe.congress[data$E4JB_4=="Yes"]<-1
data$believe.congress[data$E4JB_4=="No"]<-0
data$believe.congress[data$E4JB_4=="No answer"]<-0 
ftable(data$E4JB_4)
ftable(data$believe.congress)

data$believe.trumpconcedes<-NA
data$believe.trumpconcedes[data$E4JB_5=="Yes"]<-1
data$believe.trumpconcedes[data$E4JB_5=="No"]<-0
data$believe.trumpconcedes[data$E4JB_5=="No answer"]<-0 
ftable(data$E4JB_5)
ftable(data$believe.trumpconcedes)

data$believe.bidensworn<-NA
data$believe.bidensworn[data$E4JB_6=="Yes"]<-1
data$believe.bidensworn[data$E4JB_6=="No"]<-0
data$believe.bidensworn[data$E4JB_6=="No answer"]<-0 
ftable(data$E4JB_6)
ftable(data$believe.bidensworn)

data$believe.nothing<-NA
data$believe.nothing[data$believe.bidensworn==1]<-0
data$believe.nothing[data$believe.trumpconcedes==1]<-0
data$believe.nothing[data$believe.congress==1]<-0
data$believe.nothing[data$believe.electoral==1]<-0
data$believe.nothing[data$believe.supcourt==1]<-0
data$believe.nothing[data$believe.repleader==1]<-0

data$believe.nothing[data$believe.bidensworn==0 & data$believe.trumpconcedes==0  & data$believe.congress==0 & data$believe.electoral==0 & data$believe.supcourt==0 & data$believe.repleader==0]<-1
ftable(data$believe.nothing)

###SELF ESTEEM###

data$selfesteem.R1<-NA
data$selfesteem.R1[data$R1=="Strongly agree"]<-5
data$selfesteem.R1[data$R1=="Agree"]<-4
data$selfesteem.R1[data$R1=="Neither agree or disagree"]<-3
data$selfesteem.R1[data$R1=="Disagree"]<-2
data$selfesteem.R1[data$R1=="Strongly disagree"]<-1

data$selfesteem.R5<-NA
data$selfesteem.R5[data$R5=="Strongly agree"]<-5
data$selfesteem.R5[data$R5=="Agree"]<-4
data$selfesteem.R5[data$R5=="Neither agree or disagree"]<-3
data$selfesteem.R5[data$R5=="Disagree"]<-2
data$selfesteem.R5[data$R5=="Strongly disagree"]<-1
data$selfesteem.R5n<-  6-data$selfesteem.R5

data$selfesteem.R7<-NA
data$selfesteem.R7[data$R7=="Strongly agree"]<-5
data$selfesteem.R7[data$R7=="Agree"]<-4
data$selfesteem.R7[data$R7=="Neither agree or disagree"]<-3
data$selfesteem.R7[data$R7=="Disagree"]<-2
data$selfesteem.R7[data$R7=="Strongly disagree"]<-1

data$selfesteem.R9<-NA
data$selfesteem.R9[data$R9=="Strongly agree"]<-5
data$selfesteem.R9[data$R9=="Agree"]<-4
data$selfesteem.R9[data$R9=="Neither agree or disagree"]<-3
data$selfesteem.R9[data$R9=="Disagree"]<-2
data$selfesteem.R9[data$R9=="Strongly disagree"]<-1
data$selfesteem.R9n<-6-data$selfesteem.R9

data$selfesteem = rowMeans(data[,c("selfesteem.R1", "selfesteem.R5n", "selfesteem.R7", "selfesteem.R9n")], na.rm=TRUE)
summary(data$selfesteem)

data$selfesteem.group<-NA
data$selfesteem.group[data$selfesteem<5.01]<-"High"
data$selfesteem.group[data$selfesteem<4.49]<-"Medium"
data$selfesteem.group[data$selfesteem<3.00001]<-"Low"
ftable(data$selfesteem.group)

###POLITICAL VIOLENCE###

data$violence.V1<-NA
data$violence.V1[data$V1=="Strongly agree"]<-5
data$violence.V1[data$V1=="Agree"]<-4
data$violence.V1[data$V1=="Neither agree or disagree"]<-3
data$violence.V1[data$V1=="Disagree"]<-2
data$violence.V1[data$V1=="Strongly disagree"]<-1
ftable(data$violence.V1)

data$violence.V2<-NA
data$violence.V2[data$V2=="Strongly agree"]<-5
data$violence.V2[data$V2=="Agree"]<-4
data$violence.V2[data$V2=="Neither agree or disagree"]<-3
data$violence.V2[data$V2=="Disagree"]<-2
data$violence.V2[data$V2=="Strongly disagree"]<-1
ftable(data$violence.V2)

data$violence.V3<-NA
data$violence.V3[data$V3=="Strongly agree"]<-5
data$violence.V3[data$V3=="Agree"]<-4
data$violence.V3[data$V3=="Neither agree or disagree"]<-3
data$violence.V3[data$V3=="Disagree"]<-2
data$violence.V3[data$V3=="Strongly disagree"]<-1
ftable(data$violence.V3)

data$violence.V4<-NA
data$violence.V4[data$V4=="Strongly agree"]<-5
data$violence.V4[data$V4=="Agree"]<-4
data$violence.V4[data$V4=="Neither agree or disagree"]<-3
data$violence.V4[data$V4=="Disagree"]<-2
data$violence.V4[data$V4=="Strongly disagree"]<-1
ftable(data$violence.V4)

data$violence.V5<-NA
data$violence.V5[data$V5=="Strongly agree"]<-5
data$violence.V5[data$V5=="Agree"]<-4
data$violence.V5[data$V5=="Neither agree or disagree"]<-3
data$violence.V5[data$V5=="Disagree"]<-2
data$violence.V5[data$V5=="Strongly disagree"]<-1
ftable(data$violence.V5)

data$violence = rowMeans(data[,c("violence.V1", "violence.V2", "violence.V3", "violence.V4", "violence.V5")], na.rm=TRUE)
summary(data$violence)

violence<-as.data.frame(cbind(data$violence.V1, data$violence.V2, data$violence.V3, data$violence.V4, data$violence.V5))
colnames(violence)<-c("violence.V1", "violence.V2", "violence.V3", "violence.V4", "violence.V5")

###WINNER/LEGITIMACY###

data$legitimate<-NA
data$legitimate[data$E3=="Yes"]<-1
data$legitimate[data$E3=="No"]<-0
data$legitimate[data$E3=="No answer"]<-0

data$biden.win<-NA
data$biden.win[data$E2=="Joe Biden"]<-1
data$biden.win[data$E2=="Donald Trump"]<-0
data$biden.win[data$E2=="No answer"]<-0
ftable(data$biden.win)

ftable(data$legitimate)
ftable(data$biden.win, data$legitimate)

phi(ftable(data$biden.win, data$legitimate), digits = 3)

data$accept.ind<-rowMeans(data[,c("legitimate", "biden.win")], na.rm=TRUE)

###Implement Weighting###

population<-data.frame(matrix(data=NA, nrow=100,ncol=0))
population$ResponseId<-"weightingrow"
population$female<-.51
population$party.rep<-.31
population$party.dem<-.35
population$race.white<-.75   
population$race.black<-.09
population$race.hispanic<-.08
population$age.1829<-.11
population$age.3049<-.30
#population$age.5064<-.29
population$ed.hs<-.25
#population$ed.college<-.56

population$region.northeast<-0.172
population$region.midwest<-0.21
#population$region.west<-0.238
population$region.south<-0.38

population$treat<-1

date.list<-unique(data$date.num)

summary(data$female)
summary(data$party.rep)
summary(data$party.dem)
summary(data$race.white)
summary(data$race.black)
summary(data$race.hispanic)
summary(data$age.1829)
summary(data$age.3049)
summary(data$ed.hs)
summary(data$region.northeast)
summary(data$region.midwest)
summary(data$region.west)
summary(data$region.south)
    
for (j in date.list) {
  try(eval(parse(text=paste("data.",j,"<-subset(data, data$date.num=='",j,"')",sep=""))))
  try(eval(parse(text=paste("data.bal.weighting.",j,"<-data.frame(cbind(data.",j,"$ResponseId, data.",j,"$female, data.",j,"$party.rep, data.",j,"$party.dem, data.",j,"$race.white, data.",j,"$race.black, data.",j,"$race.hispanic, data.",j,"$age.1829, data.",j,"$age.3049, data.",j,"$ed.hs,data.",j,"$region.northeast,data.",j,"$region.south,data.",j,"$region.midwest))",sep=""))))
  try(eval(parse(text=paste("colnames(data.bal.weighting.",j,")<-c('ResponseId','female','party.rep', 'party.dem', 'race.white', 'race.black', 'race.hispanic', 'age.1829', 'age.3049', 'ed.hs', 'region.northeast','region.south','region.midwest')",sep=""))))
  try(eval(parse(text=paste("data.bal.weighting.",j,"$treat<-0",sep=""))))
  try(eval(parse(text=paste("data.bal.weighting.",j,"$female<-as.numeric(paste(data.bal.weighting.",j,"$female))",sep="")))) 
  try(eval(parse(text=paste("data.bal.weighting.",j,"$party.rep<-as.numeric(paste(data.bal.weighting.",j,"$party.rep))",sep="")))) 
  try(eval(parse(text=paste("data.bal.weighting.",j,"$party.dem<-as.numeric(paste(data.bal.weighting.",j,"$party.dem))",sep=""))))
  try(eval(parse(text=paste("data.bal.weighting.",j,"$race.white<-as.numeric(paste(data.bal.weighting.",j,"$race.white))",sep=""))))
  try(eval(parse(text=paste("data.bal.weighting.",j,"$race.black<-as.numeric(paste(data.bal.weighting.",j,"$race.black))",sep=""))))
  try(eval(parse(text=paste("data.bal.weighting.",j,"$race.hispanic<-as.numeric(paste(data.bal.weighting.",j,"$race.hispanic))",sep=""))))
  try(eval(parse(text=paste("data.bal.weighting.",j,"$age.1829<-as.numeric(paste(data.bal.weighting.",j,"$age.1829))",sep=""))))
  try(eval(parse(text=paste("data.bal.weighting.",j,"$age.3049<-as.numeric(paste(data.bal.weighting.",j,"$age.3049))",sep=""))))
  try(eval(parse(text=paste("data.bal.weighting.",j,"$ed.hs<-as.numeric(paste(data.bal.weighting.",j,"$ed.hs))",sep=""))))
  try(eval(parse(text=paste("data.bal.weighting.",j,"$region.northeast<-as.numeric(paste(data.bal.weighting.",j,"$region.northeast))",sep=""))))
  try(eval(parse(text=paste("data.bal.weighting.",j,"$region.south<-as.numeric(paste(data.bal.weighting.",j,"$region.south))",sep=""))))
  try(eval(parse(text=paste("data.bal.weighting.",j,"$region.midwest<-as.numeric(paste(data.bal.weighting.",j,"$region.midwest))",sep=""))))
  try(eval(parse(text=paste("data.bal<-rbind(data.bal.weighting.",j,", population)",sep=""))))
  try(eval(parse(text=paste("data.bal<-na.omit(data.bal)",sep=""))))
  try(eval(parse(text=paste("treat<-data.bal$treat",sep=""))))
  try(eval(parse(text=paste("X<-cbind(data.bal$female,data.bal$party.rep,data.bal$race.white, data.bal$race.black, data.bal$race.hispanic, data.bal$age.1829, data.bal$age.3049, data.bal$ed.hs, data.bal$region.northeast,data.bal$region.south,data.bal$region.midwest)",sep=""))))
  try(eval(parse(text=paste("eb.out<-ebalance(Treatment=treat, X=X, base.weight = NULL, norm.constant = NULL, coefs = NULL, max.iterations = 2000, constraint.tolerance = 1, print.level = 0)",sep=""))))
  try(eval(parse(text=paste("apply(X[treat==1,],2,mean)",sep=""))))
  try(eval(parse(text=paste("apply(X[treat==0,],2,weighted.mean,w=eb.out$w)",sep=""))))
  try(eval(parse(text=paste("apply(X[treat==0,],2,mean)",sep=""))))
  try(eval(parse(text=paste("weights<-subset(data.bal, data.bal$treat==0)",sep=""))))
  try(eval(parse(text=paste("weights$w<-eb.out$w",sep=""))))
  try(eval(parse(text=paste("weights<-cbind(weights$ResponseId, weights$w)",sep=""))))
  try(eval(parse(text=paste("colnames(weights)<-c('ResponseId','w')",sep=""))))
  try(eval(parse(text=paste("data.",j,"<-merge(data.",j,",weights,by='ResponseId',all=TRUE)",sep=""))))
  try(eval(parse(text=paste("data.",j,"$w<-as.numeric(paste(data.",j,"$w))",sep=""))))
} 

data<-rbind(data.1,data.2,data.3,data.4,data.5,data.6,data.7,data.8,data.9,data.10,data.11,data.12,data.13,data.14,data.15,data.16,data.17,data.18,data.19,data.20,data.21,data.22,data.23,data.24,data.25,data.29,data.36,data.39,data.43,data.46,data.50,data.53,data.57,data.74,data.78,data.85,data.88,data.92,data.95)

hist(data$w)

####ANALYSIS####

data.rep<-subset(data,data$party.rep==1)
data.dem<-subset(data,data$party.dem==1)
data.indother<-subset(data,data$party.indother==1)

###CORE FIGURES###
pd <- position_dodge(0.075)

estimates.dem<-matrix(data = NA, nrow=95, ncol=3)
estimates.dem<-data.frame(estimates.dem)
colnames(estimates.dem)<-c("date.num","date", "party")
estimates.dem$party<-"Democrat"
estimates.dem$date.num<-seq(1:95)
estimates.dem$date<-c("10-27","10-28","10-29","10-30","10-31","11-01","11-02","11-03","11-04","11-05","11-06","11-07","11-08","11-09","11-10","11-11","11-12","11-13","11-14","11-15","11-16","11-17","11-18","11-19","11-20","11-21","11-22","11-23","11-24","11-25","11-26","11-27","11-28","11-29","11-30","12-01","12-02","12-03","12-04","12-05","12-06","12-07","12-08","12-09","12-10","12-11","12-12","12-13","12-14","12-15","12-16","12-17","12-18","12-19","12-20","12-21","12-22","12-23","12-24","12-25","12-26","12-27","12-28","12-29","12-30","12-31","01-01","01-02","01-03","01-04","01-05","01-06","01-07","01-08","01-09","01-10","01-11","01-12","01-13","01-14","01-15","01-16","01-17","01-18","01-19","01-20","01-21","01-22","01-23","01-24","01-25","01-26","01-27","01-28","01-29")

estimates.rep<-estimates.dem
estimates.rep$party<-"Republican"

estimates.rep.sub<-estimates.dem
estimates.rep.sub$party<-"Republican"

estimates.rep.biden.win<-estimates.dem
estimates.rep.biden.win$party<-"Republicans, Election Legitimate"

estimates.rep.notbiden.win<-estimates.dem
estimates.rep.notbiden.win$party<-"Republicans, Election Not Legitimate"

estimates.ind<-estimates.dem
estimates.ind$party<-"Independent/Other"

estimates.pooled<-estimates.dem
estimates.pooled$party<-"Pooled"

outcome.list<-c("violence","violence.V1","violence.V2","violence.V3","violence.V4","violence.V5","trump.id.mean","biden.id.mean","polarization","rep.therm","dem.therm","selfesteem","biden.win","legitimate", "accept.ind")
outcome.list.1<-c("violence","violence.V1","violence.V2","violence.V3","violence.V4","violence.V5","trump.id.mean","biden.id.mean","polarization","rep.therm","dem.therm","selfesteem")
outcome.list.2<-c("biden.win","legitimate","accept.ind")

data.list<-c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","29","36","39","43","46","50","53","57","74","78","81","85")

for (i in data.list){
  try(eval(parse(text=paste("data.rep.",i,"<-subset(data.rep, data.rep$date.num==",i,")",sep=""))))
}


for (i in outcome.list){
  try(eval(parse(text=paste("estimates.",i,".dem<-estimates.dem",sep=""))))
  try(eval(parse(text=paste("estimates.",i,".rep<-estimates.rep",sep=""))))
  try(eval(parse(text=paste("estimates.",i,".ind<-estimates.ind",sep=""))))
  try(eval(parse(text=paste("estimates.",i,".pooled<-estimates.pooled",sep=""))))
  try(eval(parse(text=paste("estimates.",i,".rep.sub<-estimates.rep.sub",sep=""))))
  try(eval(parse(text=paste("estimates.",i,".rep.biden.win<-estimates.rep.biden.win",sep=""))))
  try(eval(parse(text=paste("estimates.",i,".rep.notbiden.win<-estimates.rep.notbiden.win",sep=""))))
  }

for (i in outcome.list.1){
for (j in 1:95) {
  try(eval(parse(text=paste("m",j,"<-lm(",i," ~ -1 + party, data = data.",j,", weights=data.",j,"$w)",sep=""))))
  try(eval(parse(text=paste("estimates.",i,".dem$mean[",j,"]<-summary(m",j,")$coefficients[1,1]",sep=""))))
  try(eval(parse(text=paste("estimates.",i,".dem$se[",j,"]<-summary(m",j,")$coefficients[1,2]",sep=""))))
  try(eval(parse(text=paste("estimates.",i,".rep$mean[",j,"]<-summary(m",j,")$coefficients[3,1]",sep=""))))
  try(eval(parse(text=paste("estimates.",i,".rep$se[",j,"]<-summary(m",j,")$coefficients[3,2]",sep=""))))
  try(eval(parse(text=paste("estimates.",i,".ind$mean[",j,"]<-summary(m",j,")$coefficients[2,1]",sep=""))))
  try(eval(parse(text=paste("estimates.",i,".ind$se[",j,"]<-summary(m",j,")$coefficients[2,2]",sep=""))))
  try(eval(parse(text=paste("estimates.",i,"<-rbind(estimates.",i,".dem,estimates.",i,".rep,estimates.",i,".ind)",sep=""))))
  try(eval(parse(text=paste("estimates.",i,"$l95ci<-estimates.",i,"$mean-1.96*estimates.",i,"$se",sep=""))))
  try(eval(parse(text=paste("estimates.",i,"$u95ci<-estimates.",i,"$mean+1.96*estimates.",i,"$se",sep=""))))
  try(eval(parse(text=paste("estimates.",i,"$outcome<-'",i,"'",sep=""))))
  try(eval(parse(text=paste("rm(m",j,")",sep=""))))
  }
}

for (i in outcome.list.1){
  for (j in 1:95) {
    try(eval(parse(text=paste("m",j,"<-lm(",i," ~ -1 + party, data = data.",j,", weights=data.",j,"$w)",sep=""))))
    try(eval(parse(text=paste("estimates.",i,".dem$mean[",j,"]<-summary(m",j,")$coefficients[1,1]",sep=""))))
    try(eval(parse(text=paste("estimates.",i,".dem$se[",j,"]<-summary(m",j,")$coefficients[1,2]",sep=""))))
    try(eval(parse(text=paste("estimates.",i,".rep$mean[",j,"]<-summary(m",j,")$coefficients[3,1]",sep=""))))
    try(eval(parse(text=paste("estimates.",i,".rep$se[",j,"]<-summary(m",j,")$coefficients[3,2]",sep=""))))
    try(eval(parse(text=paste("estimates.",i,".ind$mean[",j,"]<-summary(m",j,")$coefficients[2,1]",sep=""))))
    try(eval(parse(text=paste("estimates.",i,".ind$se[",j,"]<-summary(m",j,")$coefficients[2,2]",sep=""))))
    try(eval(parse(text=paste("estimates.",i,"<-rbind(estimates.",i,".dem,estimates.",i,".rep,estimates.",i,".ind)",sep=""))))
    try(eval(parse(text=paste("estimates.",i,"$l95ci<-estimates.",i,"$mean-1.96*estimates.",i,"$se",sep=""))))
    try(eval(parse(text=paste("estimates.",i,"$u95ci<-estimates.",i,"$mean+1.96*estimates.",i,"$se",sep=""))))
    try(eval(parse(text=paste("estimates.",i,"$outcome<-'",i,"'",sep=""))))
    try(eval(parse(text=paste("rm(m",j,")",sep=""))))
  }
}


for (i in outcome.list.2){
  for (j in 9:95) {
    try(eval(parse(text=paste("m",j,"<-lm(",i," ~ -1 + party, data = data.",j,", weights=data.",j,"$w)",sep=""))))
    try(eval(parse(text=paste("estimates.",i,".dem$mean[",j,"]<-summary(m",j,")$coefficients[1,1]",sep=""))))
    try(eval(parse(text=paste("estimates.",i,".dem$se[",j,"]<-summary(m",j,")$coefficients[1,2]",sep=""))))
    try(eval(parse(text=paste("estimates.",i,".rep$mean[",j,"]<-summary(m",j,")$coefficients[3,1]",sep=""))))
    try(eval(parse(text=paste("estimates.",i,".rep$se[",j,"]<-summary(m",j,")$coefficients[3,2]",sep=""))))
    try(eval(parse(text=paste("estimates.",i,".ind$mean[",j,"]<-summary(m",j,")$coefficients[2,1]",sep=""))))
    try(eval(parse(text=paste("estimates.",i,".ind$se[",j,"]<-summary(m",j,")$coefficients[2,2]",sep=""))))
    try(eval(parse(text=paste("estimates.",i,"<-rbind(estimates.",i,".dem,estimates.",i,".rep,estimates.",i,".ind)",sep=""))))
    try(eval(parse(text=paste("estimates.",i,"$l95ci<-estimates.",i,"$mean-1.96*estimates.",i,"$se",sep=""))))
    try(eval(parse(text=paste("estimates.",i,"$u95ci<-estimates.",i,"$mean+1.96*estimates.",i,"$se",sep=""))))
    try(eval(parse(text=paste("estimates.",i,"$outcome<-'",i,"'",sep=""))))
    try(eval(parse(text=paste("rm(m",j,")",sep=""))))
  }
}

for (i in outcome.list.2){
  for (j in 9:95) {
    try(eval(parse(text=paste("m",j,"<-lm(",i," ~ -1 + party, data = data.",j,", weights=data.",j,"$w)",sep=""))))
    try(eval(parse(text=paste("estimates.",i,".dem$mean[",j,"]<-summary(m",j,")$coefficients[1,1]",sep=""))))
    try(eval(parse(text=paste("estimates.",i,".dem$se[",j,"]<-summary(m",j,")$coefficients[1,2]",sep=""))))
    try(eval(parse(text=paste("estimates.",i,".rep$mean[",j,"]<-summary(m",j,")$coefficients[3,1]",sep=""))))
    try(eval(parse(text=paste("estimates.",i,".rep$se[",j,"]<-summary(m",j,")$coefficients[3,2]",sep=""))))
    try(eval(parse(text=paste("estimates.",i,".ind$mean[",j,"]<-summary(m",j,")$coefficients[2,1]",sep=""))))
    try(eval(parse(text=paste("estimates.",i,".ind$se[",j,"]<-summary(m",j,")$coefficients[2,2]",sep=""))))
    try(eval(parse(text=paste("estimates.",i,"<-rbind(estimates.",i,".dem,estimates.",i,".rep,estimates.",i,".ind)",sep=""))))
    try(eval(parse(text=paste("estimates.",i,"$l95ci<-estimates.",i,"$mean-1.96*estimates.",i,"$se",sep=""))))
    try(eval(parse(text=paste("estimates.",i,"$u95ci<-estimates.",i,"$mean+1.96*estimates.",i,"$se",sep=""))))
    try(eval(parse(text=paste("estimates.",i,"$outcome<-'",i,"'",sep=""))))
    try(eval(parse(text=paste("rm(m",j,")",sep=""))))
  }
} 


  for (j in 1:95) {
    try(eval(parse(text=paste("m",j,"<-lm(trump.id.mean ~ biden.win, data = data.rep.",j,", weights=data.rep.",j,"$w)",sep=""))))
    try(eval(parse(text=paste("estimates.trump.id.mean.rep.notbiden.win$mean[",j,"]<-summary(m",j,")$coefficients[1,1]",sep=""))))
    try(eval(parse(text=paste("estimates.trump.id.mean.rep.notbiden.win$se[",j,"]<-summary(m",j,")$coefficients[1,2]",sep=""))))
    try(eval(parse(text=paste("estimates.trump.id.mean.rep.biden.win$mean[",j,"]<-summary(m",j,")$coefficients[1,1]+summary(m",j,")$coefficients[2,1]",sep=""))))
    try(eval(parse(text=paste("estimates.trump.id.mean.rep.biden.win$se[",j,"]<-summary(m",j,")$coefficients[1,2]+summary(m",j,")$coefficients[2,2]",sep=""))))
    try(eval(parse(text=paste("estimates.trump.id.mean.rep.sub<-rbind(estimates.trump.id.mean.rep.biden.win,estimates.trump.id.mean.rep.notbiden.win)",sep=""))))
    try(eval(parse(text=paste("estimates.trump.id.mean.rep.sub$l95ci<-estimates.trump.id.mean.rep.sub$mean-1.96*estimates.trump.id.mean.rep.sub$se",sep=""))))
    try(eval(parse(text=paste("estimates.trump.id.mean.rep.sub$u95ci<-estimates.trump.id.mean.rep.sub$mean+1.96*estimates.trump.id.mean.rep.sub$se",sep=""))))
    try(eval(parse(text=paste("estimates.trump.id.mean.rep.sub$outcome<-'trump.id.mean'",sep=""))))
    try(eval(parse(text=paste("rm(m",j,")",sep=""))))
  }

for (j in 1:95) {
  try(eval(parse(text=paste("m",j,"<-lm(trump.id.mean ~ biden.win, data = data.rep.",j,", weights=data.rep.",j,"$w)",sep=""))))
  try(eval(parse(text=paste("estimates.trump.id.mean.rep.notbiden.win$mean[",j,"]<-summary(m",j,")$coefficients[1,1]",sep=""))))
  try(eval(parse(text=paste("estimates.trump.id.mean.rep.notbiden.win$se[",j,"]<-summary(m",j,")$coefficients[1,2]",sep=""))))
  try(eval(parse(text=paste("estimates.trump.id.mean.rep.biden.win$mean[",j,"]<-summary(m",j,")$coefficients[1,1]+summary(m",j,")$coefficients[2,1]",sep=""))))
  try(eval(parse(text=paste("estimates.trump.id.mean.rep.biden.win$se[",j,"]<-summary(m",j,")$coefficients[1,2]+summary(m",j,")$coefficients[2,2]",sep=""))))
  try(eval(parse(text=paste("estimates.trump.id.mean.rep.sub<-rbind(estimates.trump.id.mean.rep.biden.win,estimates.trump.id.mean.rep.notbiden.win)",sep=""))))
  try(eval(parse(text=paste("estimates.trump.id.mean.rep.sub$l95ci<-estimates.trump.id.mean.rep.sub$mean-1.96*estimates.trump.id.mean.rep.sub$se",sep=""))))
  try(eval(parse(text=paste("estimates.trump.id.mean.rep.sub$u95ci<-estimates.trump.id.mean.rep.sub$mean+1.96*estimates.trump.id.mean.rep.sub$se",sep=""))))
  try(eval(parse(text=paste("estimates.trump.id.mean.rep.sub$outcome<-'trump.id.mean'",sep=""))))
  try(eval(parse(text=paste("rm(m",j,")",sep=""))))
}


for (j in 1:95) {
  try(eval(parse(text=paste("m",j,"<-lm(selfesteem ~ biden.win, data = data.rep.",j,", weights=data.rep.",j,"$w)",sep=""))))
  try(eval(parse(text=paste("estimates.selfesteem.rep.notbiden.win$mean[",j,"]<-summary(m",j,")$coefficients[1,1]",sep=""))))
  try(eval(parse(text=paste("estimates.selfesteem.rep.notbiden.win$se[",j,"]<-summary(m",j,")$coefficients[1,2]",sep=""))))
  try(eval(parse(text=paste("estimates.selfesteem.rep.biden.win$mean[",j,"]<-summary(m",j,")$coefficients[1,1]+summary(m",j,")$coefficients[2,1]",sep=""))))
  try(eval(parse(text=paste("estimates.selfesteem.rep.biden.win$se[",j,"]<-summary(m",j,")$coefficients[1,2]+summary(m",j,")$coefficients[2,2]",sep=""))))
  try(eval(parse(text=paste("estimates.selfesteem.rep.sub<-rbind(estimates.selfesteem.rep.biden.win,estimates.selfesteem.rep.notbiden.win)",sep=""))))
  try(eval(parse(text=paste("estimates.selfesteem.rep.sub$l95ci<-estimates.selfesteem.rep.sub$mean-1.96*estimates.selfesteem.rep.sub$se",sep=""))))
  try(eval(parse(text=paste("estimates.selfesteem.rep.sub$u95ci<-estimates.selfesteem.rep.sub$mean+1.96*estimates.selfesteem.rep.sub$se",sep=""))))
  try(eval(parse(text=paste("estimates.selfesteem.rep.sub$outcome<-'selfesteem'",sep=""))))
  try(eval(parse(text=paste("rm(m",j,")",sep=""))))
}

for (j in 1:95) {
  try(eval(parse(text=paste("m",j,"<-lm(selfesteem ~ biden.win, data = data.rep.",j,", weights=data.rep.",j,"$w)",sep=""))))
  try(eval(parse(text=paste("estimates.selfesteem.rep.notbiden.win$mean[",j,"]<-summary(m",j,")$coefficients[1,1]",sep=""))))
  try(eval(parse(text=paste("estimates.selfesteem.rep.notbiden.win$se[",j,"]<-summary(m",j,")$coefficients[1,2]",sep=""))))
  try(eval(parse(text=paste("estimates.selfesteem.rep.biden.win$mean[",j,"]<-summary(m",j,")$coefficients[1,1]+summary(m",j,")$coefficients[2,1]",sep=""))))
  try(eval(parse(text=paste("estimates.selfesteem.rep.biden.win$se[",j,"]<-summary(m",j,")$coefficients[1,2]+summary(m",j,")$coefficients[2,2]",sep=""))))
  try(eval(parse(text=paste("estimates.selfesteem.rep.sub<-rbind(estimates.selfesteem.rep.biden.win,estimates.selfesteem.rep.notbiden.win)",sep=""))))
  try(eval(parse(text=paste("estimates.selfesteem.rep.sub$l95ci<-estimates.selfesteem.rep.sub$mean-1.96*estimates.selfesteem.rep.sub$se",sep=""))))
  try(eval(parse(text=paste("estimates.selfesteem.rep.sub$u95ci<-estimates.selfesteem.rep.sub$mean+1.96*estimates.selfesteem.rep.sub$se",sep=""))))
  try(eval(parse(text=paste("estimates.selfesteem.rep.sub$outcome<-'selfesteem'",sep=""))))
  try(eval(parse(text=paste("rm(m",j,")",sep=""))))
}


###CORE FIGURES FOR PAPER###

#Biden Win

estimates.biden.win<-subset(estimates.biden.win, is.na(estimates.biden.win$mean)==FALSE)
estimates.biden.win$u95ci[estimates.biden.win$u95ci>1]<-1

estimates.biden.win$date[estimates.biden.win$date=="10-26"]<-"."
estimates.biden.win$date[estimates.biden.win$date=="10-27"]<-"."
estimates.biden.win$date[estimates.biden.win$date=="10-29"]<-"."
estimates.biden.win$date[estimates.biden.win$date=="10-30"]<-"."
estimates.biden.win$date[estimates.biden.win$date=="11-01"]<-"."
estimates.biden.win$date[estimates.biden.win$date=="11-02"]<-"."
estimates.biden.win$date[estimates.biden.win$date=="11-04"]<-"."
estimates.biden.win$date[estimates.biden.win$date=="11-05"]<-"."
estimates.biden.win$date[estimates.biden.win$date=="11-07"]<-"."
estimates.biden.win$date[estimates.biden.win$date=="11-08"]<-"."
estimates.biden.win$date[estimates.biden.win$date=="11-10"]<-"."
estimates.biden.win$date[estimates.biden.win$date=="11-11"]<-"."
estimates.biden.win$date[estimates.biden.win$date=="11-13"]<-"."
estimates.biden.win$date[estimates.biden.win$date=="11-14"]<-"."
estimates.biden.win$date[estimates.biden.win$date=="11-16"]<-"."
estimates.biden.win$date[estimates.biden.win$date=="11-17"]<-"."
estimates.biden.win$date[estimates.biden.win$date=="11-19"]<-"."
estimates.biden.win$date[estimates.biden.win$date=="11-20"]<-"."

pdf('fig-bidenwin.pdf', width=7.75, height=3.5)
ggplot(estimates.biden.win, aes(x=date.num, y=mean, colour=party)) + geom_errorbar(aes(ymin=l95ci, ymax=u95ci), width=0, position=pd,  alpha=.5) +geom_line(position=pd, alpha=.5) +geom_point(position=pd, alpha=.5) + geom_line(aes(group = party), position=pd,alpha=.5) + theme_bw() + xlab("Date")+ ylab("Proportion Identifying Biden as Winner") + ylim(c(0,1)) + guides(color=guide_legend(title=NULL)) + ggtitle("Outcome: Believes Biden Won") + scale_color_manual(values=c("dodgerblue3","palevioletred4","red2")) + theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=5.5)) + facet_grid(. ~ party) + scale_x_continuous(breaks=estimates.biden.win$date.num, labels=estimates.biden.win$date) + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + theme(plot.title = element_text(size = 11))  + theme(axis.title.x=element_blank()) + annotate("text", x = 8, y = 0, label = "D", color="darkgreen", size=3, fontface =2) + annotate("text", x = 12, y = 0, label = "M", color="darkgreen", size=3, fontface =2) + annotate("text", x = 49, y = 0, label = "E", color="darkgreen", size=3, fontface =2) + annotate("text", x = 37, y = 0, label = "B", color="darkgreen", size=3, fontface =2) + annotate("text", x = 25, y = 0, label = "L", color="darkgreen", size=3, fontface =2)  + annotate("text", x = 73, y = 0, label = "C", color="darkgreen", size=3, fontface =2)  + annotate("text", x = 86, y = 0, label = "I", color="darkgreen", size=3, fontface =2)
dev.off()

m1<-lm(biden.win ~ -1+date, data = data, weights=data$w)
summary(m1)

m2<-lm(biden.win ~ -1+date, data = data)
summary(m2)

estimates.biden.win.pooled<-estimates.pooled

results<-data.frame((summary(m1)$coefficients[,1:2]))
colnames(results)<-c("mean","se")
results$date<-row.names(results)
results$date<-str_remove(results$date, "date")
results<-data.frame(cbind(results$date,results$mean,results$se))
colnames(results)<-c("date","mean","se")

estimates.biden.win.pooled<-merge(estimates.biden.win.pooled,results,by="date")

estimates.biden.win.pooled$mean<-as.numeric(estimates.biden.win.pooled$mean)
estimates.biden.win.pooled$se<-as.numeric(estimates.biden.win.pooled$se)

estimates.biden.win.pooled$l95ci<-estimates.biden.win.pooled$mean-1.96*estimates.biden.win.pooled$se
estimates.biden.win.pooled$u95ci<-estimates.biden.win.pooled$mean+1.96*estimates.biden.win.pooled$se
estimates.biden.win.pooled$u95ci[estimates.biden.win.pooled$u95ci>1]<-1

estimates.biden.win.pooled$date[estimates.biden.win.pooled$date=="10-26"]<-"."
estimates.biden.win.pooled$date[estimates.biden.win.pooled$date=="10-27"]<-"."
estimates.biden.win.pooled$date[estimates.biden.win.pooled$date=="10-29"]<-"."
estimates.biden.win.pooled$date[estimates.biden.win.pooled$date=="10-30"]<-"."
estimates.biden.win.pooled$date[estimates.biden.win.pooled$date=="11-01"]<-"."
estimates.biden.win.pooled$date[estimates.biden.win.pooled$date=="11-02"]<-"."
estimates.biden.win.pooled$date[estimates.biden.win.pooled$date=="11-04"]<-"."
estimates.biden.win.pooled$date[estimates.biden.win.pooled$date=="11-05"]<-"."
estimates.biden.win.pooled$date[estimates.biden.win.pooled$date=="11-07"]<-"."
estimates.biden.win.pooled$date[estimates.biden.win.pooled$date=="11-08"]<-"."
estimates.biden.win.pooled$date[estimates.biden.win.pooled$date=="11-10"]<-"."
estimates.biden.win.pooled$date[estimates.biden.win.pooled$date=="11-11"]<-"."
estimates.biden.win.pooled$date[estimates.biden.win.pooled$date=="11-13"]<-"."
estimates.biden.win.pooled$date[estimates.biden.win.pooled$date=="11-14"]<-"."
estimates.biden.win.pooled$date[estimates.biden.win.pooled$date=="11-16"]<-"."
estimates.biden.win.pooled$date[estimates.biden.win.pooled$date=="11-17"]<-"."
estimates.biden.win.pooled$date[estimates.biden.win.pooled$date=="11-19"]<-"."
estimates.biden.win.pooled$date[estimates.biden.win.pooled$date=="11-20"]<-"."

pdf('fig-bidenwin-pooled.pdf', width=7.25, height=3.5)
ggplot(estimates.biden.win.pooled, aes(x=date.num, y=mean))  + geom_errorbar(aes(ymin=l95ci, ymax=u95ci), width=0, position=pd,  alpha=.5) +geom_point(position=pd, alpha=.5) + geom_line(position=pd,alpha=.5) + theme_bw() + ylab("Proportion Identifying Biden as Winner") + ylim(c(0,1))  + ggtitle("Outcome: Believes Biden Won")  + theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=5.5))  + scale_x_continuous(breaks=estimates.biden.win.pooled$date.num, labels=estimates.biden.win.pooled$date) + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + theme(plot.title = element_text(size = 11)) + theme(axis.title.x=element_blank()) + annotate("text", x = 8, y = 0, label = "D", color="darkgreen", size=3, fontface =2) + annotate("text", x = 12, y = 0, label = "M", color="darkgreen", size=3, fontface =2) + annotate("text", x = 49, y = 0, label = "E", color="darkgreen", size=3, fontface =2) + annotate("text", x = 37, y = 0, label = "B", color="darkgreen", size=3, fontface =2) + annotate("text", x = 25, y = 0, label = "L", color="darkgreen", size=3, fontface =2)  + annotate("text", x = 73, y = 0, label = "C", color="darkgreen", size=3, fontface =2)  + annotate("text", x = 86, y = 0, label = "I", color="darkgreen", size=3, fontface =2)
dev.off()

estimates.biden.win.pooled.raw<-estimates.pooled

results<-data.frame((summary(m2)$coefficients[,1:2]))
colnames(results)<-c("mean","se")
results$date<-row.names(results)
results$date<-str_remove(results$date, "date")
results<-data.frame(cbind(results$date,results$mean,results$se))
colnames(results)<-c("date","mean","se")

estimates.biden.win.pooled.raw<-merge(estimates.biden.win.pooled.raw,results,by="date")

estimates.biden.win.pooled.raw$mean<-as.numeric(estimates.biden.win.pooled.raw$mean)
estimates.biden.win.pooled.raw$se<-as.numeric(estimates.biden.win.pooled.raw$se)

estimates.biden.win.pooled.raw$l95ci<-estimates.biden.win.pooled.raw$mean-1.96*estimates.biden.win.pooled.raw$se
estimates.biden.win.pooled.raw$u95ci<-estimates.biden.win.pooled.raw$mean+1.96*estimates.biden.win.pooled.raw$se
estimates.biden.win.pooled.raw$u95ci[estimates.biden.win.pooled.raw$u95ci>1]<-1

estimates.biden.win.pooled.raw$date[estimates.biden.win.pooled.raw$date=="10-26"]<-"."
estimates.biden.win.pooled.raw$date[estimates.biden.win.pooled.raw$date=="10-27"]<-"."
estimates.biden.win.pooled.raw$date[estimates.biden.win.pooled.raw$date=="10-29"]<-"."
estimates.biden.win.pooled.raw$date[estimates.biden.win.pooled.raw$date=="10-30"]<-"."
estimates.biden.win.pooled.raw$date[estimates.biden.win.pooled.raw$date=="11-01"]<-"."
estimates.biden.win.pooled.raw$date[estimates.biden.win.pooled.raw$date=="11-02"]<-"."
estimates.biden.win.pooled.raw$date[estimates.biden.win.pooled.raw$date=="11-04"]<-"."
estimates.biden.win.pooled.raw$date[estimates.biden.win.pooled.raw$date=="11-05"]<-"."
estimates.biden.win.pooled.raw$date[estimates.biden.win.pooled.raw$date=="11-07"]<-"."
estimates.biden.win.pooled.raw$date[estimates.biden.win.pooled.raw$date=="11-08"]<-"."
estimates.biden.win.pooled.raw$date[estimates.biden.win.pooled.raw$date=="11-10"]<-"."
estimates.biden.win.pooled.raw$date[estimates.biden.win.pooled.raw$date=="11-11"]<-"."
estimates.biden.win.pooled.raw$date[estimates.biden.win.pooled.raw$date=="11-13"]<-"."
estimates.biden.win.pooled.raw$date[estimates.biden.win.pooled.raw$date=="11-14"]<-"."
estimates.biden.win.pooled.raw$date[estimates.biden.win.pooled.raw$date=="11-16"]<-"."
estimates.biden.win.pooled.raw$date[estimates.biden.win.pooled.raw$date=="11-17"]<-"."
estimates.biden.win.pooled.raw$date[estimates.biden.win.pooled.raw$date=="11-19"]<-"."
estimates.biden.win.pooled.raw$date[estimates.biden.win.pooled.raw$date=="11-20"]<-"."

pdf('fig-bidenwin-pooled-raw.pdf', width=7.25, height=3.5)
ggplot(estimates.biden.win.pooled.raw, aes(x=date.num, y=mean))  + geom_errorbar(aes(ymin=l95ci, ymax=u95ci), width=0, position=pd,  alpha=.5) +geom_point(position=pd, alpha=.5) + geom_line(position=pd,alpha=.5) + theme_bw() + ylab("Proportion Identifying Biden as Winner") + ylim(c(0,1))  + ggtitle("Outcome: Believes Biden Won")  + theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=7))  + scale_x_continuous(breaks=estimates.biden.win.pooled.raw$date.num, labels=estimates.biden.win.pooled.raw$date) + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + theme(plot.title = element_text(size = 11)) + theme(axis.title.x=element_blank()) + annotate("text", x = 8, y = 0, label = "D", color="darkgreen", size=3, fontface =2) + annotate("text", x = 12, y = 0, label = "M", color="darkgreen", size=3, fontface =2) + annotate("text", x = 49, y = 0, label = "E", color="darkgreen", size=3, fontface =2) + annotate("text", x = 37, y = 0, label = "B", color="darkgreen", size=3, fontface =2) + annotate("text", x = 25, y = 0, label = "L", color="darkgreen", size=3, fontface =2)  + annotate("text", x = 73, y = 0, label = "C", color="darkgreen", size=3, fontface =2)  + annotate("text", x = 86, y = 0, label = "I", color="darkgreen", size=3, fontface =2)
dev.off()

#Legitimate

estimates.legitimate<-subset(estimates.legitimate, is.na(estimates.legitimate$mean)==FALSE)
estimates.legitimate$u95ci[estimates.legitimate$u95ci>1]<-1

estimates.legitimate$date[estimates.legitimate$date=="10-26"]<-"."
estimates.legitimate$date[estimates.legitimate$date=="10-27"]<-"."
estimates.legitimate$date[estimates.legitimate$date=="10-29"]<-"."
estimates.legitimate$date[estimates.legitimate$date=="10-30"]<-"."
estimates.legitimate$date[estimates.legitimate$date=="11-01"]<-"."
estimates.legitimate$date[estimates.legitimate$date=="11-02"]<-"."
estimates.legitimate$date[estimates.legitimate$date=="11-04"]<-"."
estimates.legitimate$date[estimates.legitimate$date=="11-05"]<-"."
estimates.legitimate$date[estimates.legitimate$date=="11-07"]<-"."
estimates.legitimate$date[estimates.legitimate$date=="11-08"]<-"."
estimates.legitimate$date[estimates.legitimate$date=="11-10"]<-"."
estimates.legitimate$date[estimates.legitimate$date=="11-11"]<-"."
estimates.legitimate$date[estimates.legitimate$date=="11-13"]<-"."
estimates.legitimate$date[estimates.legitimate$date=="11-14"]<-"."
estimates.legitimate$date[estimates.legitimate$date=="11-16"]<-"."
estimates.legitimate$date[estimates.legitimate$date=="11-17"]<-"."
estimates.legitimate$date[estimates.legitimate$date=="11-19"]<-"."
estimates.legitimate$date[estimates.legitimate$date=="11-20"]<-"."

pdf('fig-legitimate.pdf', width=7.75, height=3.5)
ggplot(estimates.legitimate, aes(x=date.num, y=mean, colour=party)) + geom_errorbar(aes(ymin=l95ci, ymax=u95ci), width=0, position=pd,  alpha=.5) +geom_line(position=pd, alpha=.5) +geom_point(position=pd, alpha=.5) + geom_line(aes(group = party), position=pd,alpha=.5) + theme_bw() + xlab("Date")+ ylab("Proportion Accepting Result as Legitimate") + ylim(c(0,1)) + guides(color=guide_legend(title=NULL)) + ggtitle("Outcome: Election Result Legitimate") + scale_color_manual(values=c("dodgerblue3","palevioletred4","red2")) + theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=5.5)) + facet_grid(. ~ party) + scale_x_continuous(breaks=estimates.legitimate$date.num, labels=estimates.legitimate$date) + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + theme(plot.title = element_text(size = 11))  + theme(axis.title.x=element_blank()) + annotate("text", x = 8, y = 0, label = "D", color="darkgreen", size=3, fontface =2) + annotate("text", x = 12, y = 0, label = "M", color="darkgreen", size=3, fontface =2) + annotate("text", x = 49, y = 0, label = "E", color="darkgreen", size=3, fontface =2) + annotate("text", x = 37, y = 0, label = "B", color="darkgreen", size=3, fontface =2) + annotate("text", x = 25, y = 0, label = "L", color="darkgreen", size=3, fontface =2)  + annotate("text", x = 73, y = 0, label = "C", color="darkgreen", size=3, fontface =2)  + annotate("text", x = 86, y = 0, label = "I", color="darkgreen", size=3, fontface =2)
dev.off()

m1<-lm(legitimate ~ -1+date, data = data, weights=data$w)
summary(m1)

m2<-lm(legitimate ~ -1+date, data = data)
summary(m2)

estimates.legitimate.pooled<-estimates.pooled

results<-data.frame((summary(m1)$coefficients[,1:2]))
colnames(results)<-c("mean","se")
results$date<-row.names(results)
results$date<-str_remove(results$date, "date")
results<-data.frame(cbind(results$date,results$mean,results$se))
colnames(results)<-c("date","mean","se")

estimates.legitimate.pooled<-merge(estimates.legitimate.pooled,results,by="date")

estimates.legitimate.pooled$mean<-as.numeric(estimates.legitimate.pooled$mean)
estimates.legitimate.pooled$se<-as.numeric(estimates.legitimate.pooled$se)

estimates.legitimate.pooled$l95ci<-estimates.legitimate.pooled$mean-1.96*estimates.legitimate.pooled$se
estimates.legitimate.pooled$u95ci<-estimates.legitimate.pooled$mean+1.96*estimates.legitimate.pooled$se
estimates.legitimate.pooled$u95ci[estimates.legitimate.pooled$u95ci>1]<-1

estimates.legitimate.pooled$date[estimates.legitimate.pooled$date=="10-26"]<-"."
estimates.legitimate.pooled$date[estimates.legitimate.pooled$date=="10-27"]<-"."
estimates.legitimate.pooled$date[estimates.legitimate.pooled$date=="10-29"]<-"."
estimates.legitimate.pooled$date[estimates.legitimate.pooled$date=="10-30"]<-"."
estimates.legitimate.pooled$date[estimates.legitimate.pooled$date=="11-01"]<-"."
estimates.legitimate.pooled$date[estimates.legitimate.pooled$date=="11-02"]<-"."
estimates.legitimate.pooled$date[estimates.legitimate.pooled$date=="11-04"]<-"."
estimates.legitimate.pooled$date[estimates.legitimate.pooled$date=="11-05"]<-"."
estimates.legitimate.pooled$date[estimates.legitimate.pooled$date=="11-07"]<-"."
estimates.legitimate.pooled$date[estimates.legitimate.pooled$date=="11-08"]<-"."
estimates.legitimate.pooled$date[estimates.legitimate.pooled$date=="11-10"]<-"."
estimates.legitimate.pooled$date[estimates.legitimate.pooled$date=="11-11"]<-"."
estimates.legitimate.pooled$date[estimates.legitimate.pooled$date=="11-13"]<-"."
estimates.legitimate.pooled$date[estimates.legitimate.pooled$date=="11-14"]<-"."
estimates.legitimate.pooled$date[estimates.legitimate.pooled$date=="11-16"]<-"."
estimates.legitimate.pooled$date[estimates.legitimate.pooled$date=="11-17"]<-"."
estimates.legitimate.pooled$date[estimates.legitimate.pooled$date=="11-19"]<-"."
estimates.legitimate.pooled$date[estimates.legitimate.pooled$date=="11-20"]<-"."

pdf('fig-legitimate-pooled.pdf', width=7.25, height=3.5)
ggplot(estimates.legitimate.pooled, aes(x=date.num, y=mean))  + geom_errorbar(aes(ymin=l95ci, ymax=u95ci), width=0, position=pd,  alpha=.5) +geom_point(position=pd, alpha=.5) + geom_line(position=pd,alpha=.5) + theme_bw() + ylab("Proportion Accepting Result as Legitimate") + ylim(c(0,1))  + ggtitle("Outcome: Election Result Legitimate")  + theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=7))  + scale_x_continuous(breaks=estimates.legitimate.pooled$date.num, labels=estimates.legitimate.pooled$date) + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + theme(plot.title = element_text(size = 11)) + theme(axis.title.x=element_blank()) + annotate("text", x = 8, y = 0, label = "D", color="darkgreen", size=3, fontface =2) + annotate("text", x = 12, y = 0, label = "M", color="darkgreen", size=3, fontface =2) + annotate("text", x = 49, y = 0, label = "E", color="darkgreen", size=3, fontface =2) + annotate("text", x = 37, y = 0, label = "B", color="darkgreen", size=3, fontface =2) + annotate("text", x = 25, y = 0, label = "L", color="darkgreen", size=3, fontface =2)  + annotate("text", x = 73, y = 0, label = "C", color="darkgreen", size=3, fontface =2)  + annotate("text", x = 86, y = 0, label = "I", color="darkgreen", size=3, fontface =2)
dev.off()

estimates.legitimate.pooled.raw<-estimates.pooled

results<-data.frame((summary(m2)$coefficients[,1:2]))
colnames(results)<-c("mean","se")
results$date<-row.names(results)
results$date<-str_remove(results$date, "date")
results<-data.frame(cbind(results$date,results$mean,results$se))
colnames(results)<-c("date","mean","se")

estimates.legitimate.pooled.raw<-merge(estimates.legitimate.pooled,results,by="date")

estimates.legitimate.pooled.raw$mean<-as.numeric(estimates.legitimate.pooled.raw$mean)
estimates.legitimate.pooled.raw$se<-as.numeric(estimates.legitimate.pooled.raw$se)

estimates.legitimate.pooled.raw$l95ci<-estimates.legitimate.pooled.raw$mean-1.96*estimates.legitimate.pooled.raw$se
estimates.legitimate.pooled.raw$u95ci<-estimates.legitimate.pooled.raw$mean+1.96*estimates.legitimate.pooled.raw$se
estimates.legitimate.pooled.raw$u95ci[estimates.legitimate.pooled.raw$u95ci>1]<-1

estimates.legitimate.pooled.raw$date[estimates.legitimate.pooled.raw$date=="10-26"]<-"."
estimates.legitimate.pooled.raw$date[estimates.legitimate.pooled.raw$date=="10-27"]<-"."
estimates.legitimate.pooled.raw$date[estimates.legitimate.pooled.raw$date=="10-29"]<-"."
estimates.legitimate.pooled.raw$date[estimates.legitimate.pooled.raw$date=="10-30"]<-"."
estimates.legitimate.pooled.raw$date[estimates.legitimate.pooled.raw$date=="11-01"]<-"."
estimates.legitimate.pooled.raw$date[estimates.legitimate.pooled.raw$date=="11-02"]<-"."
estimates.legitimate.pooled.raw$date[estimates.legitimate.pooled.raw$date=="11-04"]<-"."
estimates.legitimate.pooled.raw$date[estimates.legitimate.pooled.raw$date=="11-05"]<-"."
estimates.legitimate.pooled.raw$date[estimates.legitimate.pooled.raw$date=="11-07"]<-"."
estimates.legitimate.pooled.raw$date[estimates.legitimate.pooled.raw$date=="11-08"]<-"."
estimates.legitimate.pooled.raw$date[estimates.legitimate.pooled.raw$date=="11-10"]<-"."
estimates.legitimate.pooled.raw$date[estimates.legitimate.pooled.raw$date=="11-11"]<-"."
estimates.legitimate.pooled.raw$date[estimates.legitimate.pooled.raw$date=="11-13"]<-"."
estimates.legitimate.pooled.raw$date[estimates.legitimate.pooled.raw$date=="11-14"]<-"."
estimates.legitimate.pooled.raw$date[estimates.legitimate.pooled.raw$date=="11-16"]<-"."
estimates.legitimate.pooled.raw$date[estimates.legitimate.pooled.raw$date=="11-17"]<-"."
estimates.legitimate.pooled.raw$date[estimates.legitimate.pooled.raw$date=="11-19"]<-"."
estimates.legitimate.pooled.raw$date[estimates.legitimate.pooled.raw$date=="11-20"]<-"."

pdf('fig-legitimate-pooled-raw.pdf', width=7.25, height=3.5)
ggplot(estimates.legitimate.pooled, aes(x=date.num, y=mean))  + geom_errorbar(aes(ymin=l95ci, ymax=u95ci), width=0, position=pd,  alpha=.5) +geom_point(position=pd, alpha=.5) + geom_line(position=pd,alpha=.5) + theme_bw() + ylab("Proportion Accepting Result as Legitimate") + ylim(c(0,1))  + ggtitle("Outcome: Election Result Legitimate")  + theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=7))  + scale_x_continuous(breaks=estimates.legitimate.pooled$date.num, labels=estimates.legitimate.pooled$date) + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + theme(plot.title = element_text(size = 11)) + theme(axis.title.x=element_blank()) + annotate("text", x = 8, y = 0, label = "D", color="darkgreen", size=3, fontface =2) + annotate("text", x = 12, y = 0, label = "M", color="darkgreen", size=3, fontface =2) + annotate("text", x = 49, y = 0, label = "E", color="darkgreen", size=3, fontface =2) + annotate("text", x = 37, y = 0, label = "B", color="darkgreen", size=3, fontface =2) + annotate("text", x = 25, y = 0, label = "L", color="darkgreen", size=3, fontface =2)  + annotate("text", x = 73, y = 0, label = "C", color="darkgreen", size=3, fontface =2)  + annotate("text", x = 86, y = 0, label = "I", color="darkgreen", size=3, fontface =2)
dev.off()

data.postcall<-subset(data,data$date.num>12)
data.postcall.rep<-subset(data.postcall, data.postcall$party.rep==1)
data.postcall.indother<-subset(data.postcall, data.postcall$party.indother==1)
data.postcall.dem<-subset(data.postcall, data.postcall$party.dem==1)

data.postcall.rep$age.group <- factor(data.postcall.rep$age.group, levels = c("65+","18-29","30-49","50-64"))
data.postcall.indother$age.group <- factor(data.postcall.indother$age.group, levels = c("65+","18-29","30-49","50-64"))

data.postcall.rep$ed.group <- factor(data.postcall.rep$ed.group, levels = c("Postgrad","HS","College"))
data.postcall.indother$ed.group <- factor(data.postcall.indother$ed.group, levels = c("Postgrad","HS","College"))

data.postcall.rep$selfesteem.group <- factor(data.postcall.rep$selfesteem.group, levels = c("Low","Medium","High"))
data.postcall.indother$selfesteem.group <- factor(data.postcall.indother$selfesteem.group, levels = c("Low","Medium","High"))

m1<-lm(legitimate ~ gender + age.group + ed.group + social.status.group, data = data.postcall.rep, weights=data.postcall.rep$w)
summary(m1)
m2<-lm(legitimate ~ gender + age.group + ed.group + social.status.group, data = data.postcall.indother, weights=data.postcall.indother$w)
summary(m2)

positions <- c("18-29", "30-49", "50-64","65+","HS","College","Postgrad","Male","Female","Low","Medium","High")
pdf('fig-linearprob.pdf', width=6.5, height=5)
ggcoef_compare(list("Republican" = m1, "Independent/Other" = m2), type="faceted",variable_labels = c(gender = "Gender", age.group="Age Group",ed.group="Education",social.status.group="Social Status"), point_size=2,point_stroke=2,point_fill="grey50",stripped_rows=FALSE,colour=c("red1","purple"), significance_labels = NULL) + xlab("Coefficient Estimate") + theme_bw() + guides(color=guide_legend(title=NULL)) + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()) + theme(plot.title = element_text(size = 11)) + ggtitle("Outcome: Election Result Legitimate") + theme(legend.title = element_blank())   + scale_fill_manual(values = c('grey50')) + scale_color_manual(values = c('grey10','grey10','grey10','grey10')) +  scale_shape_manual(values = c(16,16)) + theme(legend.position = "none") +  xlim(c(-.3,.3))
dev.off()

m1<-lm(legitimate ~ gender + relevel(age.group, ref = '65+') + relevel(ed.group, ref = 'HS') + social.status.group, data = data.postcall.rep)
summary(m1)

m2<-lm(legitimate ~ gender + relevel(age.group, ref = '65+')  + relevel(ed.group, ref = 'HS') + social.status.group, data = data.postcall.indother)
summary(m2)

positions <- c("18-29", "30-49", "50-64","65+","HS","College","Postgrad","Male","Female","Low","Medium","High")
pdf('fig-linearprob-raw.pdf', width=6.5, height=5)
ggcoef_compare(list("Republican" = m1, "Independent/Other" = m2), type="faceted",variable_labels = c(gender = "Gender", age.group="Age Group",ed.group="Education",social.status.group="Social Status"), point_size=2,point_stroke=2,point_fill="grey50",stripped_rows=FALSE,colour=c("red1","purple"), significance_labels = NULL) + xlab("Coefficient Estimate") + theme_bw() + guides(color=guide_legend(title=NULL)) + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()) + theme(plot.title = element_text(size = 11)) + ggtitle("Outcome: Election Result Legitimate") + theme(legend.title = element_blank())   + scale_fill_manual(values = c('grey50')) + scale_color_manual(values = c('grey10','grey10','grey10','grey10')) +  scale_shape_manual(values = c(16,16)) + theme(legend.position = "none") +  xlim(c(-.3,.3))
dev.off()


#Acceptance Index

estimates.accept.ind<-subset(estimates.accept.ind, is.na(estimates.accept.ind$mean)==FALSE)
estimates.accept.ind$u95ci[estimates.accept.ind$u95ci>1]<-1

estimates.accept.ind$date[estimates.accept.ind$date=="10-26"]<-"."
estimates.accept.ind$date[estimates.accept.ind$date=="10-27"]<-"."
estimates.accept.ind$date[estimates.accept.ind$date=="10-29"]<-"."
estimates.accept.ind$date[estimates.accept.ind$date=="10-30"]<-"."
estimates.accept.ind$date[estimates.accept.ind$date=="11-01"]<-"."
estimates.accept.ind$date[estimates.accept.ind$date=="11-02"]<-"."
estimates.accept.ind$date[estimates.accept.ind$date=="11-04"]<-"."
estimates.accept.ind$date[estimates.accept.ind$date=="11-05"]<-"."
estimates.accept.ind$date[estimates.accept.ind$date=="11-07"]<-"."
estimates.accept.ind$date[estimates.accept.ind$date=="11-08"]<-"."
estimates.accept.ind$date[estimates.accept.ind$date=="11-10"]<-"."
estimates.accept.ind$date[estimates.accept.ind$date=="11-11"]<-"."
estimates.accept.ind$date[estimates.accept.ind$date=="11-13"]<-"."
estimates.accept.ind$date[estimates.accept.ind$date=="11-14"]<-"."
estimates.accept.ind$date[estimates.accept.ind$date=="11-16"]<-"."
estimates.accept.ind$date[estimates.accept.ind$date=="11-17"]<-"."
estimates.accept.ind$date[estimates.accept.ind$date=="11-19"]<-"."
estimates.accept.ind$date[estimates.accept.ind$date=="11-20"]<-"."

pdf('fig-acceptind.pdf', width=7.75, height=3.5)
ggplot(estimates.accept.ind, aes(x=date.num, y=mean, colour=party)) + geom_errorbar(aes(ymin=l95ci, ymax=u95ci), width=0, position=pd,  alpha=.5) +geom_line(position=pd, alpha=.5) +geom_point(position=pd, alpha=.5) + geom_line(aes(group = party), position=pd,alpha=.5) + theme_bw() + xlab("Date")+ ylab("Election Acceptance Index") + ylim(c(0,1)) + guides(color=guide_legend(title=NULL)) + ggtitle("Outcome: Election Acceptance Index") + scale_color_manual(values=c("dodgerblue3","palevioletred4","red2")) + theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=5.5)) + facet_grid(. ~ party) + scale_x_continuous(breaks=estimates.accept.ind$date.num, labels=estimates.accept.ind$date) + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + theme(plot.title = element_text(size = 11))  + theme(axis.title.x=element_blank()) + annotate("text", x = 8, y = 0, label = "D", color="darkgreen", size=3, fontface =2) + annotate("text", x = 12, y = 0, label = "M", color="darkgreen", size=3, fontface =2) + annotate("text", x = 49, y = 0, label = "E", color="darkgreen", size=3, fontface =2) + annotate("text", x = 37, y = 0, label = "B", color="darkgreen", size=3, fontface =2) + annotate("text", x = 25, y = 0, label = "L", color="darkgreen", size=3, fontface =2)  + annotate("text", x = 73, y = 0, label = "C", color="darkgreen", size=3, fontface =2)  + annotate("text", x = 86, y = 0, label = "I", color="darkgreen", size=3, fontface =2)
dev.off()

m1<-lm(accept.ind ~ -1+date, data = data, weights=data$w)
summary(m1)

m2<-lm(accept.ind ~ -1+date, data = data)
summary(m2)

estimates.accept.ind.pooled<-estimates.pooled

results<-data.frame((summary(m1)$coefficients[,1:2]))
colnames(results)<-c("mean","se")
results$date<-row.names(results)
results$date<-str_remove(results$date, "date")
results<-data.frame(cbind(results$date,results$mean,results$se))
colnames(results)<-c("date","mean","se")

estimates.accept.ind.pooled<-merge(estimates.accept.ind.pooled,results,by="date")

estimates.accept.ind.pooled$mean<-as.numeric(estimates.accept.ind.pooled$mean)
estimates.accept.ind.pooled$se<-as.numeric(estimates.accept.ind.pooled$se)

estimates.accept.ind.pooled$l95ci<-estimates.accept.ind.pooled$mean-1.96*estimates.accept.ind.pooled$se
estimates.accept.ind.pooled$u95ci<-estimates.accept.ind.pooled$mean+1.96*estimates.accept.ind.pooled$se
estimates.accept.ind.pooled$u95ci[estimates.accept.ind.pooled$u95ci>1]<-1

estimates.accept.ind.pooled$date[estimates.accept.ind.pooled$date=="10-26"]<-"."
estimates.accept.ind.pooled$date[estimates.accept.ind.pooled$date=="10-27"]<-"."
estimates.accept.ind.pooled$date[estimates.accept.ind.pooled$date=="10-29"]<-"."
estimates.accept.ind.pooled$date[estimates.accept.ind.pooled$date=="10-30"]<-"."
estimates.accept.ind.pooled$date[estimates.accept.ind.pooled$date=="11-01"]<-"."
estimates.accept.ind.pooled$date[estimates.accept.ind.pooled$date=="11-02"]<-"."
estimates.accept.ind.pooled$date[estimates.accept.ind.pooled$date=="11-04"]<-"."
estimates.accept.ind.pooled$date[estimates.accept.ind.pooled$date=="11-05"]<-"."
estimates.accept.ind.pooled$date[estimates.accept.ind.pooled$date=="11-07"]<-"."
estimates.accept.ind.pooled$date[estimates.accept.ind.pooled$date=="11-08"]<-"."
estimates.accept.ind.pooled$date[estimates.accept.ind.pooled$date=="11-10"]<-"."
estimates.accept.ind.pooled$date[estimates.accept.ind.pooled$date=="11-11"]<-"."
estimates.accept.ind.pooled$date[estimates.accept.ind.pooled$date=="11-13"]<-"."
estimates.accept.ind.pooled$date[estimates.accept.ind.pooled$date=="11-14"]<-"."
estimates.accept.ind.pooled$date[estimates.accept.ind.pooled$date=="11-16"]<-"."
estimates.accept.ind.pooled$date[estimates.accept.ind.pooled$date=="11-17"]<-"."
estimates.accept.ind.pooled$date[estimates.accept.ind.pooled$date=="11-19"]<-"."
estimates.accept.ind.pooled$date[estimates.accept.ind.pooled$date=="11-20"]<-"."

pdf('fig-acceptind-pooled.pdf', width=7.25, height=3.5)
ggplot(estimates.accept.ind.pooled, aes(x=date.num, y=mean))  + geom_errorbar(aes(ymin=l95ci, ymax=u95ci), width=0, position=pd,  alpha=.5) +geom_point(position=pd, alpha=.5) + geom_line(position=pd,alpha=.5) + theme_bw() + ylab("Election Acceptance Index") + ylim(c(0,1))  + ggtitle("Outcome: Election Acceptance Index")  + theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=5.5))  + scale_x_continuous(breaks=estimates.accept.ind.pooled$date.num, labels=estimates.accept.ind.pooled$date) + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + theme(plot.title = element_text(size = 11)) + theme(axis.title.x=element_blank()) + annotate("text", x = 8, y = 0, label = "D", color="darkgreen", size=3, fontface =2) + annotate("text", x = 12, y = 0, label = "M", color="darkgreen", size=3, fontface =2) + annotate("text", x = 49, y = 0, label = "E", color="darkgreen", size=3, fontface =2) + annotate("text", x = 37, y = 0, label = "B", color="darkgreen", size=3, fontface =2) + annotate("text", x = 25, y = 0, label = "L", color="darkgreen", size=3, fontface =2)  + annotate("text", x = 73, y = 0, label = "C", color="darkgreen", size=3, fontface =2)  + annotate("text", x = 86, y = 0, label = "I", color="darkgreen", size=3, fontface =2)
dev.off()

estimates.accept.ind.pooled.raw<-estimates.pooled

results<-data.frame((summary(m2)$coefficients[,1:2]))
colnames(results)<-c("mean","se")
results$date<-row.names(results)
results$date<-str_remove(results$date, "date")
results<-data.frame(cbind(results$date,results$mean,results$se))
colnames(results)<-c("date","mean","se")

estimates.accept.ind.pooled.raw<-merge(estimates.accept.ind.pooled.raw,results,by="date")

estimates.accept.ind.pooled.raw$mean<-as.numeric(estimates.accept.ind.pooled.raw$mean)
estimates.accept.ind.pooled.raw$se<-as.numeric(estimates.accept.ind.pooled.raw$se)

estimates.accept.ind.pooled.raw$l95ci<-estimates.accept.ind.pooled.raw$mean-1.96*estimates.accept.ind.pooled.raw$se
estimates.accept.ind.pooled.raw$u95ci<-estimates.accept.ind.pooled.raw$mean+1.96*estimates.accept.ind.pooled.raw$se
estimates.accept.ind.pooled.raw$u95ci[estimates.accept.ind.pooled.raw$u95ci>1]<-1

estimates.accept.ind.pooled.raw$date[estimates.accept.ind.pooled.raw$date=="10-26"]<-"."
estimates.accept.ind.pooled.raw$date[estimates.accept.ind.pooled.raw$date=="10-27"]<-"."
estimates.accept.ind.pooled.raw$date[estimates.accept.ind.pooled.raw$date=="10-29"]<-"."
estimates.accept.ind.pooled.raw$date[estimates.accept.ind.pooled.raw$date=="10-30"]<-"."
estimates.accept.ind.pooled.raw$date[estimates.accept.ind.pooled.raw$date=="11-01"]<-"."
estimates.accept.ind.pooled.raw$date[estimates.accept.ind.pooled.raw$date=="11-02"]<-"."
estimates.accept.ind.pooled.raw$date[estimates.accept.ind.pooled.raw$date=="11-04"]<-"."
estimates.accept.ind.pooled.raw$date[estimates.accept.ind.pooled.raw$date=="11-05"]<-"."
estimates.accept.ind.pooled.raw$date[estimates.accept.ind.pooled.raw$date=="11-07"]<-"."
estimates.accept.ind.pooled.raw$date[estimates.accept.ind.pooled.raw$date=="11-08"]<-"."
estimates.accept.ind.pooled.raw$date[estimates.accept.ind.pooled.raw$date=="11-10"]<-"."
estimates.accept.ind.pooled.raw$date[estimates.accept.ind.pooled.raw$date=="11-11"]<-"."
estimates.accept.ind.pooled.raw$date[estimates.accept.ind.pooled.raw$date=="11-13"]<-"."
estimates.accept.ind.pooled.raw$date[estimates.accept.ind.pooled.raw$date=="11-14"]<-"."
estimates.accept.ind.pooled.raw$date[estimates.accept.ind.pooled.raw$date=="11-16"]<-"."
estimates.accept.ind.pooled.raw$date[estimates.accept.ind.pooled.raw$date=="11-17"]<-"."
estimates.accept.ind.pooled.raw$date[estimates.accept.ind.pooled.raw$date=="11-19"]<-"."
estimates.accept.ind.pooled.raw$date[estimates.accept.ind.pooled.raw$date=="11-20"]<-"."

pdf('fig-accept.ind-pooled-raw.pdf', width=7.25, height=3.5)
ggplot(estimates.accept.ind.pooled.raw, aes(x=date.num, y=mean))  + geom_errorbar(aes(ymin=l95ci, ymax=u95ci), width=0, position=pd,  alpha=.5) +geom_point(position=pd, alpha=.5) + geom_line(position=pd,alpha=.5) + theme_bw() + ylab("Election Acceptance Index") + ylim(c(0,1))  + ggtitle("Outcome: Election Acceptance Index")  + theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=7))  + scale_x_continuous(breaks=estimates.accept.ind.pooled.raw$date.num, labels=estimates.accept.ind.pooled.raw$date) + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + theme(plot.title = element_text(size = 11)) + theme(axis.title.x=element_blank()) + annotate("text", x = 8, y = 0, label = "D", color="darkgreen", size=3, fontface =2) + annotate("text", x = 12, y = 0, label = "M", color="darkgreen", size=3, fontface =2) + annotate("text", x = 49, y = 0, label = "E", color="darkgreen", size=3, fontface =2) + annotate("text", x = 37, y = 0, label = "B", color="darkgreen", size=3, fontface =2) + annotate("text", x = 25, y = 0, label = "L", color="darkgreen", size=3, fontface =2)  + annotate("text", x = 73, y = 0, label = "C", color="darkgreen", size=3, fontface =2)  + annotate("text", x = 86, y = 0, label = "I", color="darkgreen", size=3, fontface =2)
dev.off()

#Violence

estimates.violence<-subset(estimates.violence, is.na(estimates.violence$mean)==FALSE)

estimates.violence$date[estimates.violence$date=="10-26"]<-"."
estimates.violence$date[estimates.violence$date=="10-27"]<-"."
estimates.violence$date[estimates.violence$date=="10-29"]<-"."
estimates.violence$date[estimates.violence$date=="10-30"]<-"."
estimates.violence$date[estimates.violence$date=="11-01"]<-"."
estimates.violence$date[estimates.violence$date=="11-02"]<-"."
estimates.violence$date[estimates.violence$date=="11-03"]<-"."
estimates.violence$date[estimates.violence$date=="11-04"]<-"."
estimates.violence$date[estimates.violence$date=="11-05"]<-"."
estimates.violence$date[estimates.violence$date=="11-07"]<-"."
estimates.violence$date[estimates.violence$date=="11-08"]<-"."
estimates.violence$date[estimates.violence$date=="11-09"]<-"."
estimates.violence$date[estimates.violence$date=="11-10"]<-"."
estimates.violence$date[estimates.violence$date=="11-11"]<-"."
estimates.violence$date[estimates.violence$date=="11-13"]<-"."
estimates.violence$date[estimates.violence$date=="11-14"]<-"."
estimates.violence$date[estimates.violence$date=="11-15"]<-"."
estimates.violence$date[estimates.violence$date=="11-16"]<-"."
estimates.violence$date[estimates.violence$date=="11-17"]<-"."
estimates.violence$date[estimates.violence$date=="11-19"]<-"."
estimates.violence$date[estimates.violence$date=="11-20"]<-"."

estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=26)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=27)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=28)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=30)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=31)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=32)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=33)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=34)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=36)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=37)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=38)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=40)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=41)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=42)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=44)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=45)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=47)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=48)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=49)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=51)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=52)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=54)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=55)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=56)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=58)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=59)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=60)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=61)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=62)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=63)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=64)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=65)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=66)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=67)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=68)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=69)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=70)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=71)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=72)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=73)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=75)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=76)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=77)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=79)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=80)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=82)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=83)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=84)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=86)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=87)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=89)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=90)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=91)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=93)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=94)

pdf('fig-violence.pdf', width=7.75, height=3.5)
ggplot(estimates.violence, aes(x=date.num, y=mean, colour=party)) + geom_errorbar(aes(ymin=l95ci, ymax=u95ci), width=0, position=pd,  alpha=.5) +geom_line(position=pd, alpha=.5) +geom_point(position=pd, alpha=.5) + geom_line(aes(group = party), position=pd,alpha=.5) + theme_bw() + xlab("Date")+ ylab("Radicalism Intention Scale") + ylim(c(1.5,3.5)) + guides(color=guide_legend(title=NULL)) + ggtitle("Outcome: Radicalism Intention Scale") + scale_color_manual(values=c("dodgerblue3","palevioletred4","red2")) + theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=5.5)) + facet_grid(. ~ party) + scale_x_continuous(breaks=estimates.violence$date.num, labels=estimates.violence$date) + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + theme(plot.title = element_text(size = 11))  + theme(axis.title.x=element_blank()) + annotate("text", x = 8, y = 1.5, label = "D", color="darkgreen", size=3, fontface =2) + annotate("text", x = 12, y = 1.5, label = "M", color="darkgreen", size=3, fontface =2) + annotate("text", x = 49, y = 1.5, label = "E", color="darkgreen", size=3, fontface =2) + annotate("text", x = 37, y = 1.5, label = "B", color="darkgreen", size=3, fontface =2) + annotate("text", x = 25, y = 1.5, label = "L", color="darkgreen", size=3, fontface =2)  + annotate("text", x = 73, y = 1.5, label = "C", color="darkgreen", size=3, fontface =2) + annotate("text", x = 86, y = 1.5, label = "I", color="darkgreen", size=3, fontface =2)
dev.off()

estimates.violence.V1<-subset(estimates.violence.V1, is.na(estimates.violence.V1$mean)==FALSE)

estimates.violence.V1$date[estimates.violence.V1$date=="10-26"]<-"."
estimates.violence.V1$date[estimates.violence.V1$date=="10-27"]<-"."
estimates.violence.V1$date[estimates.violence.V1$date=="10-29"]<-"."
estimates.violence.V1$date[estimates.violence.V1$date=="10-30"]<-"."
estimates.violence.V1$date[estimates.violence.V1$date=="11-01"]<-"."
estimates.violence.V1$date[estimates.violence.V1$date=="11-02"]<-"."
estimates.violence.V1$date[estimates.violence.V1$date=="11-03"]<-"."
estimates.violence.V1$date[estimates.violence.V1$date=="11-04"]<-"."
estimates.violence.V1$date[estimates.violence.V1$date=="11-05"]<-"."
estimates.violence.V1$date[estimates.violence.V1$date=="11-07"]<-"."
estimates.violence.V1$date[estimates.violence.V1$date=="11-08"]<-"."
estimates.violence.V1$date[estimates.violence.V1$date=="11-09"]<-"."
estimates.violence.V1$date[estimates.violence.V1$date=="11-10"]<-"."
estimates.violence.V1$date[estimates.violence.V1$date=="11-11"]<-"."
estimates.violence.V1$date[estimates.violence.V1$date=="11-13"]<-"."
estimates.violence.V1$date[estimates.violence.V1$date=="11-14"]<-"."
estimates.violence.V1$date[estimates.violence.V1$date=="11-15"]<-"."
estimates.violence.V1$date[estimates.violence.V1$date=="11-16"]<-"."
estimates.violence.V1$date[estimates.violence.V1$date=="11-17"]<-"."
estimates.violence.V1$date[estimates.violence.V1$date=="11-19"]<-"."
estimates.violence.V1$date[estimates.violence.V1$date=="11-20"]<-"."

estimates.violence.V1<-subset(estimates.violence.V1, estimates.violence.V1$date.num!=26)
estimates.violence.V1<-subset(estimates.violence.V1, estimates.violence.V1$date.num!=27)
estimates.violence.V1<-subset(estimates.violence.V1, estimates.violence.V1$date.num!=28)
estimates.violence.V1<-subset(estimates.violence.V1, estimates.violence.V1$date.num!=30)
estimates.violence.V1<-subset(estimates.violence.V1, estimates.violence.V1$date.num!=31)
estimates.violence.V1<-subset(estimates.violence.V1, estimates.violence.V1$date.num!=32)
estimates.violence.V1<-subset(estimates.violence.V1, estimates.violence.V1$date.num!=33)
estimates.violence.V1<-subset(estimates.violence.V1, estimates.violence.V1$date.num!=34)
estimates.violence.V1<-subset(estimates.violence.V1, estimates.violence.V1$date.num!=36)
estimates.violence.V1<-subset(estimates.violence.V1, estimates.violence.V1$date.num!=37)
estimates.violence.V1<-subset(estimates.violence.V1, estimates.violence.V1$date.num!=38)
estimates.violence.V1<-subset(estimates.violence.V1, estimates.violence.V1$date.num!=40)
estimates.violence.V1<-subset(estimates.violence.V1, estimates.violence.V1$date.num!=41)
estimates.violence.V1<-subset(estimates.violence.V1, estimates.violence.V1$date.num!=42)
estimates.violence.V1<-subset(estimates.violence.V1, estimates.violence.V1$date.num!=44)
estimates.violence.V1<-subset(estimates.violence.V1, estimates.violence.V1$date.num!=45)
estimates.violence.V1<-subset(estimates.violence.V1, estimates.violence.V1$date.num!=47)
estimates.violence.V1<-subset(estimates.violence.V1, estimates.violence.V1$date.num!=48)
estimates.violence.V1<-subset(estimates.violence.V1, estimates.violence.V1$date.num!=49)
estimates.violence.V1<-subset(estimates.violence.V1, estimates.violence.V1$date.num!=51)
estimates.violence.V1<-subset(estimates.violence.V1, estimates.violence.V1$date.num!=52)
estimates.violence.V1<-subset(estimates.violence.V1, estimates.violence.V1$date.num!=54)
estimates.violence.V1<-subset(estimates.violence.V1, estimates.violence.V1$date.num!=55)
estimates.violence.V1<-subset(estimates.violence.V1, estimates.violence.V1$date.num!=56)
estimates.violence.V1<-subset(estimates.violence.V1, estimates.violence.V1$date.num!=58)
estimates.violence.V1<-subset(estimates.violence.V1, estimates.violence.V1$date.num!=59)
estimates.violence.V1<-subset(estimates.violence.V1, estimates.violence.V1$date.num!=60)
estimates.violence.V1<-subset(estimates.violence.V1, estimates.violence.V1$date.num!=61)
estimates.violence.V1<-subset(estimates.violence.V1, estimates.violence.V1$date.num!=62)
estimates.violence.V1<-subset(estimates.violence.V1, estimates.violence.V1$date.num!=63)
estimates.violence.V1<-subset(estimates.violence.V1, estimates.violence.V1$date.num!=64)
estimates.violence.V1<-subset(estimates.violence.V1, estimates.violence.V1$date.num!=65)
estimates.violence.V1<-subset(estimates.violence.V1, estimates.violence.V1$date.num!=66)
estimates.violence.V1<-subset(estimates.violence.V1, estimates.violence.V1$date.num!=67)
estimates.violence.V1<-subset(estimates.violence.V1, estimates.violence.V1$date.num!=68)
estimates.violence.V1<-subset(estimates.violence.V1, estimates.violence.V1$date.num!=69)
estimates.violence.V1<-subset(estimates.violence.V1, estimates.violence.V1$date.num!=70)
estimates.violence.V1<-subset(estimates.violence.V1, estimates.violence.V1$date.num!=71)
estimates.violence.V1<-subset(estimates.violence.V1, estimates.violence.V1$date.num!=72)
estimates.violence.V1<-subset(estimates.violence.V1, estimates.violence.V1$date.num!=73)
estimates.violence.V1<-subset(estimates.violence.V1, estimates.violence.V1$date.num!=75)
estimates.violence.V1<-subset(estimates.violence.V1, estimates.violence.V1$date.num!=76)
estimates.violence.V1<-subset(estimates.violence.V1, estimates.violence.V1$date.num!=77)
estimates.violence.V1<-subset(estimates.violence.V1, estimates.violence.V1$date.num!=79)
estimates.violence.V1<-subset(estimates.violence.V1, estimates.violence.V1$date.num!=80)
estimates.violence.V1<-subset(estimates.violence.V1, estimates.violence.V1$date.num!=82)
estimates.violence.V1<-subset(estimates.violence.V1, estimates.violence.V1$date.num!=83)
estimates.violence.V1<-subset(estimates.violence.V1, estimates.violence.V1$date.num!=84)
estimates.violence.V1<-subset(estimates.violence.V1, estimates.violence.V1$date.num!=86)
estimates.violence.V1<-subset(estimates.violence.V1, estimates.violence.V1$date.num!=87)
estimates.violence.V1<-subset(estimates.violence.V1, estimates.violence.V1$date.num!=89)
estimates.violence.V1<-subset(estimates.violence.V1, estimates.violence.V1$date.num!=90)
estimates.violence.V1<-subset(estimates.violence.V1, estimates.violence.V1$date.num!=91)
estimates.violence.V1<-subset(estimates.violence.V1, estimates.violence.V1$date.num!=93)
estimates.violence.V1<-subset(estimates.violence.V1, estimates.violence.V1$date.num!=94)

pdf('fig-violenceV1.pdf', width=7.75, height=3.5)
ggplot(estimates.violence.V1, aes(x=date.num, y=mean, colour=party)) + geom_errorbar(aes(ymin=l95ci, ymax=u95ci), width=0, position=pd,  alpha=.5) +geom_line(position=pd, alpha=.5) +geom_point(position=pd, alpha=.5) + geom_line(aes(group = party), position=pd,alpha=.5) + theme_bw() + xlab("Date")+ ylab("Agreement Level") + ylim(c(1.5,3.5)) + guides(color=guide_legend(title=NULL)) + ggtitle("Outcome: Willingness to Participate in Violent Protest") + scale_color_manual(values=c("dodgerblue3","palevioletred4","red2")) + theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=7)) + facet_grid(. ~ party) + scale_x_continuous(breaks=estimates.violence.V1$date.num, labels=estimates.violence.V1$date) + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + theme(plot.title = element_text(size = 11))  + theme(axis.title.x=element_blank()) + annotate("text", x = 8, y = 1.5, label = "D", color="darkgreen", size=3, fontface =2) + annotate("text", x = 12, y = 1.5, label = "M", color="darkgreen", size=3, fontface =2) + annotate("text", x = 49, y = 1.5, label = "E", color="darkgreen", size=3, fontface =2) + annotate("text", x = 37, y = 1.5, label = "B", color="darkgreen", size=3, fontface =2) + annotate("text", x = 25, y = 1.5, label = "L", color="darkgreen", size=3, fontface =2)  + annotate("text", x = 73, y = 1.5, label = "C", color="darkgreen", size=3, fontface =2) + annotate("text", x = 86, y = 1.5, label = "I", color="darkgreen", size=3, fontface =2)
dev.off()

estimates.violence.V2<-subset(estimates.violence.V2, is.na(estimates.violence.V2$mean)==FALSE)

estimates.violence.V2$date[estimates.violence.V2$date=="10-26"]<-"."
estimates.violence.V2$date[estimates.violence.V2$date=="10-27"]<-"."
estimates.violence.V2$date[estimates.violence.V2$date=="10-29"]<-"."
estimates.violence.V2$date[estimates.violence.V2$date=="10-30"]<-"."
estimates.violence.V2$date[estimates.violence.V2$date=="11-01"]<-"."
estimates.violence.V2$date[estimates.violence.V2$date=="11-02"]<-"."
estimates.violence.V2$date[estimates.violence.V2$date=="11-03"]<-"."
estimates.violence.V2$date[estimates.violence.V2$date=="11-04"]<-"."
estimates.violence.V2$date[estimates.violence.V2$date=="11-05"]<-"."
estimates.violence.V2$date[estimates.violence.V2$date=="11-07"]<-"."
estimates.violence.V2$date[estimates.violence.V2$date=="11-08"]<-"."
estimates.violence.V2$date[estimates.violence.V2$date=="11-09"]<-"."
estimates.violence.V2$date[estimates.violence.V2$date=="11-10"]<-"."
estimates.violence.V2$date[estimates.violence.V2$date=="11-11"]<-"."
estimates.violence.V2$date[estimates.violence.V2$date=="11-13"]<-"."
estimates.violence.V2$date[estimates.violence.V2$date=="11-14"]<-"."
estimates.violence.V2$date[estimates.violence.V2$date=="11-15"]<-"."
estimates.violence.V2$date[estimates.violence.V2$date=="11-16"]<-"."
estimates.violence.V2$date[estimates.violence.V2$date=="11-17"]<-"."
estimates.violence.V2$date[estimates.violence.V2$date=="11-19"]<-"."
estimates.violence.V2$date[estimates.violence.V2$date=="11-20"]<-"."

estimates.violence.V2<-subset(estimates.violence.V2, estimates.violence.V2$date.num!=26)
estimates.violence.V2<-subset(estimates.violence.V2, estimates.violence.V2$date.num!=27)
estimates.violence.V2<-subset(estimates.violence.V2, estimates.violence.V2$date.num!=28)
estimates.violence.V2<-subset(estimates.violence.V2, estimates.violence.V2$date.num!=30)
estimates.violence.V2<-subset(estimates.violence.V2, estimates.violence.V2$date.num!=31)
estimates.violence.V2<-subset(estimates.violence.V2, estimates.violence.V2$date.num!=32)
estimates.violence.V2<-subset(estimates.violence.V2, estimates.violence.V2$date.num!=33)
estimates.violence.V2<-subset(estimates.violence.V2, estimates.violence.V2$date.num!=34)
estimates.violence.V2<-subset(estimates.violence.V2, estimates.violence.V2$date.num!=36)
estimates.violence.V2<-subset(estimates.violence.V2, estimates.violence.V2$date.num!=37)
estimates.violence.V2<-subset(estimates.violence.V2, estimates.violence.V2$date.num!=38)
estimates.violence.V2<-subset(estimates.violence.V2, estimates.violence.V2$date.num!=40)
estimates.violence.V2<-subset(estimates.violence.V2, estimates.violence.V2$date.num!=41)
estimates.violence.V2<-subset(estimates.violence.V2, estimates.violence.V2$date.num!=42)
estimates.violence.V2<-subset(estimates.violence.V2, estimates.violence.V2$date.num!=44)
estimates.violence.V2<-subset(estimates.violence.V2, estimates.violence.V2$date.num!=45)
estimates.violence.V2<-subset(estimates.violence.V2, estimates.violence.V2$date.num!=47)
estimates.violence.V2<-subset(estimates.violence.V2, estimates.violence.V2$date.num!=48)
estimates.violence.V2<-subset(estimates.violence.V2, estimates.violence.V2$date.num!=49)
estimates.violence.V2<-subset(estimates.violence.V2, estimates.violence.V2$date.num!=51)
estimates.violence.V2<-subset(estimates.violence.V2, estimates.violence.V2$date.num!=52)
estimates.violence.V2<-subset(estimates.violence.V2, estimates.violence.V2$date.num!=54)
estimates.violence.V2<-subset(estimates.violence.V2, estimates.violence.V2$date.num!=55)
estimates.violence.V2<-subset(estimates.violence.V2, estimates.violence.V2$date.num!=56)
estimates.violence.V2<-subset(estimates.violence.V2, estimates.violence.V2$date.num!=58)
estimates.violence.V2<-subset(estimates.violence.V2, estimates.violence.V2$date.num!=59)
estimates.violence.V2<-subset(estimates.violence.V2, estimates.violence.V2$date.num!=60)
estimates.violence.V2<-subset(estimates.violence.V2, estimates.violence.V2$date.num!=61)
estimates.violence.V2<-subset(estimates.violence.V2, estimates.violence.V2$date.num!=62)
estimates.violence.V2<-subset(estimates.violence.V2, estimates.violence.V2$date.num!=63)
estimates.violence.V2<-subset(estimates.violence.V2, estimates.violence.V2$date.num!=64)
estimates.violence.V2<-subset(estimates.violence.V2, estimates.violence.V2$date.num!=65)
estimates.violence.V2<-subset(estimates.violence.V2, estimates.violence.V2$date.num!=66)
estimates.violence.V2<-subset(estimates.violence.V2, estimates.violence.V2$date.num!=67)
estimates.violence.V2<-subset(estimates.violence.V2, estimates.violence.V2$date.num!=68)
estimates.violence.V2<-subset(estimates.violence.V2, estimates.violence.V2$date.num!=69)
estimates.violence.V2<-subset(estimates.violence.V2, estimates.violence.V2$date.num!=70)
estimates.violence.V2<-subset(estimates.violence.V2, estimates.violence.V2$date.num!=71)
estimates.violence.V2<-subset(estimates.violence.V2, estimates.violence.V2$date.num!=72)
estimates.violence.V2<-subset(estimates.violence.V2, estimates.violence.V2$date.num!=73)
estimates.violence.V2<-subset(estimates.violence.V2, estimates.violence.V2$date.num!=75)
estimates.violence.V2<-subset(estimates.violence.V2, estimates.violence.V2$date.num!=76)
estimates.violence.V2<-subset(estimates.violence.V2, estimates.violence.V2$date.num!=77)
estimates.violence.V2<-subset(estimates.violence.V2, estimates.violence.V2$date.num!=79)
estimates.violence.V2<-subset(estimates.violence.V2, estimates.violence.V2$date.num!=80)
estimates.violence.V2<-subset(estimates.violence.V2, estimates.violence.V2$date.num!=82)
estimates.violence.V2<-subset(estimates.violence.V2, estimates.violence.V2$date.num!=83)
estimates.violence.V2<-subset(estimates.violence.V2, estimates.violence.V2$date.num!=84)
estimates.violence.V2<-subset(estimates.violence.V2, estimates.violence.V2$date.num!=86)
estimates.violence.V2<-subset(estimates.violence.V2, estimates.violence.V2$date.num!=87)
estimates.violence.V2<-subset(estimates.violence.V2, estimates.violence.V2$date.num!=89)
estimates.violence.V2<-subset(estimates.violence.V2, estimates.violence.V2$date.num!=90)
estimates.violence.V2<-subset(estimates.violence.V2, estimates.violence.V2$date.num!=91)
estimates.violence.V2<-subset(estimates.violence.V2, estimates.violence.V2$date.num!=93)
estimates.violence.V2<-subset(estimates.violence.V2, estimates.violence.V2$date.num!=94)

pdf('fig-violenceV2.pdf', width=7.75, height=3.5)
ggplot(estimates.violence.V2, aes(x=date.num, y=mean, colour=party)) + geom_errorbar(aes(ymin=l95ci, ymax=u95ci), width=0, position=pd,  alpha=.5) +geom_line(position=pd, alpha=.5) +geom_point(position=pd, alpha=.5) + geom_line(aes(group = party), position=pd,alpha=.5) + theme_bw() + xlab("Date")+ ylab("Agreement Level") + ylim(c(1.5,3.5)) + guides(color=guide_legend(title=NULL)) + ggtitle("Outcome:  Willingness to Attack Police Forces") + scale_color_manual(values=c("dodgerblue3","palevioletred4","red2")) + theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=7)) + facet_grid(. ~ party) + scale_x_continuous(breaks=estimates.violence.V2$date.num, labels=estimates.violence.V2$date) + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + theme(plot.title = element_text(size = 11))  + theme(axis.title.x=element_blank()) + annotate("text", x = 8, y = 1.5, label = "D", color="darkgreen", size=3, fontface =2) + annotate("text", x = 12, y = 1.5, label = "M", color="darkgreen", size=3, fontface =2) + annotate("text", x = 49, y = 1.5, label = "E", color="darkgreen", size=3, fontface =2) + annotate("text", x = 37, y = 1.5, label = "B", color="darkgreen", size=3, fontface =2) + annotate("text", x = 25, y = 1.5, label = "L", color="darkgreen", size=3, fontface =2)  + annotate("text", x = 73, y = 1.5, label = "C", color="darkgreen", size=3, fontface =2) + annotate("text", x = 86, y = 1.5, label = "I", color="darkgreen", size=3, fontface =2)
dev.off()

estimates.violence.V3<-subset(estimates.violence.V3, is.na(estimates.violence.V3$mean)==FALSE)

estimates.violence.V3$date[estimates.violence.V3$date=="10-26"]<-"."
estimates.violence.V3$date[estimates.violence.V3$date=="10-27"]<-"."
estimates.violence.V3$date[estimates.violence.V3$date=="10-29"]<-"."
estimates.violence.V3$date[estimates.violence.V3$date=="10-30"]<-"."
estimates.violence.V3$date[estimates.violence.V3$date=="11-01"]<-"."
estimates.violence.V3$date[estimates.violence.V3$date=="11-02"]<-"."
estimates.violence.V3$date[estimates.violence.V3$date=="11-03"]<-"."
estimates.violence.V3$date[estimates.violence.V3$date=="11-04"]<-"."
estimates.violence.V3$date[estimates.violence.V3$date=="11-05"]<-"."
estimates.violence.V3$date[estimates.violence.V3$date=="11-07"]<-"."
estimates.violence.V3$date[estimates.violence.V3$date=="11-08"]<-"."
estimates.violence.V3$date[estimates.violence.V3$date=="11-09"]<-"."
estimates.violence.V3$date[estimates.violence.V3$date=="11-10"]<-"."
estimates.violence.V3$date[estimates.violence.V3$date=="11-11"]<-"."
estimates.violence.V3$date[estimates.violence.V3$date=="11-13"]<-"."
estimates.violence.V3$date[estimates.violence.V3$date=="11-14"]<-"."
estimates.violence.V3$date[estimates.violence.V3$date=="11-15"]<-"."
estimates.violence.V3$date[estimates.violence.V3$date=="11-16"]<-"."
estimates.violence.V3$date[estimates.violence.V3$date=="11-17"]<-"."
estimates.violence.V3$date[estimates.violence.V3$date=="11-19"]<-"."
estimates.violence.V3$date[estimates.violence.V3$date=="11-20"]<-"."

estimates.violence.V3<-subset(estimates.violence.V3, estimates.violence.V3$date.num!=26)
estimates.violence.V3<-subset(estimates.violence.V3, estimates.violence.V3$date.num!=27)
estimates.violence.V3<-subset(estimates.violence.V3, estimates.violence.V3$date.num!=28)
estimates.violence.V3<-subset(estimates.violence.V3, estimates.violence.V3$date.num!=30)
estimates.violence.V3<-subset(estimates.violence.V3, estimates.violence.V3$date.num!=31)
estimates.violence.V3<-subset(estimates.violence.V3, estimates.violence.V3$date.num!=32)
estimates.violence.V3<-subset(estimates.violence.V3, estimates.violence.V3$date.num!=33)
estimates.violence.V3<-subset(estimates.violence.V3, estimates.violence.V3$date.num!=34)
estimates.violence.V3<-subset(estimates.violence.V3, estimates.violence.V3$date.num!=36)
estimates.violence.V3<-subset(estimates.violence.V3, estimates.violence.V3$date.num!=37)
estimates.violence.V3<-subset(estimates.violence.V3, estimates.violence.V3$date.num!=38)
estimates.violence.V3<-subset(estimates.violence.V3, estimates.violence.V3$date.num!=40)
estimates.violence.V3<-subset(estimates.violence.V3, estimates.violence.V3$date.num!=41)
estimates.violence.V3<-subset(estimates.violence.V3, estimates.violence.V3$date.num!=42)
estimates.violence.V3<-subset(estimates.violence.V3, estimates.violence.V3$date.num!=44)
estimates.violence.V3<-subset(estimates.violence.V3, estimates.violence.V3$date.num!=45)
estimates.violence.V3<-subset(estimates.violence.V3, estimates.violence.V3$date.num!=47)
estimates.violence.V3<-subset(estimates.violence.V3, estimates.violence.V3$date.num!=48)
estimates.violence.V3<-subset(estimates.violence.V3, estimates.violence.V3$date.num!=49)
estimates.violence.V3<-subset(estimates.violence.V3, estimates.violence.V3$date.num!=51)
estimates.violence.V3<-subset(estimates.violence.V3, estimates.violence.V3$date.num!=52)
estimates.violence.V3<-subset(estimates.violence.V3, estimates.violence.V3$date.num!=54)
estimates.violence.V3<-subset(estimates.violence.V3, estimates.violence.V3$date.num!=55)
estimates.violence.V3<-subset(estimates.violence.V3, estimates.violence.V3$date.num!=56)
estimates.violence.V3<-subset(estimates.violence.V3, estimates.violence.V3$date.num!=58)
estimates.violence.V3<-subset(estimates.violence.V3, estimates.violence.V3$date.num!=59)
estimates.violence.V3<-subset(estimates.violence.V3, estimates.violence.V3$date.num!=60)
estimates.violence.V3<-subset(estimates.violence.V3, estimates.violence.V3$date.num!=61)
estimates.violence.V3<-subset(estimates.violence.V3, estimates.violence.V3$date.num!=62)
estimates.violence.V3<-subset(estimates.violence.V3, estimates.violence.V3$date.num!=63)
estimates.violence.V3<-subset(estimates.violence.V3, estimates.violence.V3$date.num!=64)
estimates.violence.V3<-subset(estimates.violence.V3, estimates.violence.V3$date.num!=65)
estimates.violence.V3<-subset(estimates.violence.V3, estimates.violence.V3$date.num!=66)
estimates.violence.V3<-subset(estimates.violence.V3, estimates.violence.V3$date.num!=67)
estimates.violence.V3<-subset(estimates.violence.V3, estimates.violence.V3$date.num!=68)
estimates.violence.V3<-subset(estimates.violence.V3, estimates.violence.V3$date.num!=69)
estimates.violence.V3<-subset(estimates.violence.V3, estimates.violence.V3$date.num!=70)
estimates.violence.V3<-subset(estimates.violence.V3, estimates.violence.V3$date.num!=71)
estimates.violence.V3<-subset(estimates.violence.V3, estimates.violence.V3$date.num!=72)
estimates.violence.V3<-subset(estimates.violence.V3, estimates.violence.V3$date.num!=73)
estimates.violence.V3<-subset(estimates.violence.V3, estimates.violence.V3$date.num!=75)
estimates.violence.V3<-subset(estimates.violence.V3, estimates.violence.V3$date.num!=76)
estimates.violence.V3<-subset(estimates.violence.V3, estimates.violence.V3$date.num!=77)
estimates.violence.V3<-subset(estimates.violence.V3, estimates.violence.V3$date.num!=79)
estimates.violence.V3<-subset(estimates.violence.V3, estimates.violence.V3$date.num!=80)
estimates.violence.V3<-subset(estimates.violence.V3, estimates.violence.V3$date.num!=82)
estimates.violence.V3<-subset(estimates.violence.V3, estimates.violence.V3$date.num!=83)
estimates.violence.V3<-subset(estimates.violence.V3, estimates.violence.V3$date.num!=84)
estimates.violence.V3<-subset(estimates.violence.V3, estimates.violence.V3$date.num!=86)
estimates.violence.V3<-subset(estimates.violence.V3, estimates.violence.V3$date.num!=87)
estimates.violence.V3<-subset(estimates.violence.V3, estimates.violence.V3$date.num!=89)
estimates.violence.V3<-subset(estimates.violence.V3, estimates.violence.V3$date.num!=90)
estimates.violence.V3<-subset(estimates.violence.V3, estimates.violence.V3$date.num!=91)
estimates.violence.V3<-subset(estimates.violence.V3, estimates.violence.V3$date.num!=93)
estimates.violence.V3<-subset(estimates.violence.V3, estimates.violence.V3$date.num!=94)

pdf('fig-violenceV3.pdf', width=7.75, height=3.5)
ggplot(estimates.violence.V3, aes(x=date.num, y=mean, colour=party)) + geom_errorbar(aes(ymin=l95ci, ymax=u95ci), width=0, position=pd,  alpha=.5) +geom_line(position=pd, alpha=.5) +geom_point(position=pd, alpha=.5) + geom_line(aes(group = party), position=pd,alpha=.5) + theme_bw() + xlab("Date")+ ylab("Agreement Level") + ylim(c(1.5,3.5)) + guides(color=guide_legend(title=NULL)) + ggtitle("Outcome: Willingness to Encourage Others to Join Violent Illegal Protests") + scale_color_manual(values=c("dodgerblue3","palevioletred4","red2")) + theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=7)) + facet_grid(. ~ party) + scale_x_continuous(breaks=estimates.violence.V3$date.num, labels=estimates.violence.V3$date) + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + theme(plot.title = element_text(size = 11))  + theme(axis.title.x=element_blank()) + annotate("text", x = 8, y = 1.5, label = "D", color="darkgreen", size=3, fontface =2) + annotate("text", x = 12, y = 1.5, label = "M", color="darkgreen", size=3, fontface =2) + annotate("text", x = 49, y = 1.5, label = "E", color="darkgreen", size=3, fontface =2) + annotate("text", x = 37, y = 1.5, label = "B", color="darkgreen", size=3, fontface =2) + annotate("text", x = 25, y = 1.5, label = "L", color="darkgreen", size=3, fontface =2)  + annotate("text", x = 73, y = 1.5, label = "C", color="darkgreen", size=3, fontface =2) + annotate("text", x = 86, y = 1.5, label = "I", color="darkgreen", size=3, fontface =2)
dev.off()


estimates.violence.V4<-subset(estimates.violence.V4, is.na(estimates.violence.V4$mean)==FALSE)

estimates.violence.V4$date[estimates.violence.V4$date=="10-26"]<-"."
estimates.violence.V4$date[estimates.violence.V4$date=="10-27"]<-"."
estimates.violence.V4$date[estimates.violence.V4$date=="10-29"]<-"."
estimates.violence.V4$date[estimates.violence.V4$date=="10-30"]<-"."
estimates.violence.V4$date[estimates.violence.V4$date=="11-01"]<-"."
estimates.violence.V4$date[estimates.violence.V4$date=="11-02"]<-"."
estimates.violence.V4$date[estimates.violence.V4$date=="11-03"]<-"."
estimates.violence.V4$date[estimates.violence.V4$date=="11-04"]<-"."
estimates.violence.V4$date[estimates.violence.V4$date=="11-05"]<-"."
estimates.violence.V4$date[estimates.violence.V4$date=="11-07"]<-"."
estimates.violence.V4$date[estimates.violence.V4$date=="11-08"]<-"."
estimates.violence.V4$date[estimates.violence.V4$date=="11-09"]<-"."
estimates.violence.V4$date[estimates.violence.V4$date=="11-10"]<-"."
estimates.violence.V4$date[estimates.violence.V4$date=="11-11"]<-"."
estimates.violence.V4$date[estimates.violence.V4$date=="11-13"]<-"."
estimates.violence.V4$date[estimates.violence.V4$date=="11-14"]<-"."
estimates.violence.V4$date[estimates.violence.V4$date=="11-15"]<-"."
estimates.violence.V4$date[estimates.violence.V4$date=="11-16"]<-"."
estimates.violence.V4$date[estimates.violence.V4$date=="11-17"]<-"."
estimates.violence.V4$date[estimates.violence.V4$date=="11-19"]<-"."
estimates.violence.V4$date[estimates.violence.V4$date=="11-20"]<-"."

estimates.violence.V4<-subset(estimates.violence.V4, estimates.violence.V4$date.num!=26)
estimates.violence.V4<-subset(estimates.violence.V4, estimates.violence.V4$date.num!=27)
estimates.violence.V4<-subset(estimates.violence.V4, estimates.violence.V4$date.num!=28)
estimates.violence.V4<-subset(estimates.violence.V4, estimates.violence.V4$date.num!=30)
estimates.violence.V4<-subset(estimates.violence.V4, estimates.violence.V4$date.num!=31)
estimates.violence.V4<-subset(estimates.violence.V4, estimates.violence.V4$date.num!=32)
estimates.violence.V4<-subset(estimates.violence.V4, estimates.violence.V4$date.num!=33)
estimates.violence.V4<-subset(estimates.violence.V4, estimates.violence.V4$date.num!=34)
estimates.violence.V4<-subset(estimates.violence.V4, estimates.violence.V4$date.num!=36)
estimates.violence.V4<-subset(estimates.violence.V4, estimates.violence.V4$date.num!=37)
estimates.violence.V4<-subset(estimates.violence.V4, estimates.violence.V4$date.num!=38)
estimates.violence.V4<-subset(estimates.violence.V4, estimates.violence.V4$date.num!=40)
estimates.violence.V4<-subset(estimates.violence.V4, estimates.violence.V4$date.num!=41)
estimates.violence.V4<-subset(estimates.violence.V4, estimates.violence.V4$date.num!=42)
estimates.violence.V4<-subset(estimates.violence.V4, estimates.violence.V4$date.num!=44)
estimates.violence.V4<-subset(estimates.violence.V4, estimates.violence.V4$date.num!=45)
estimates.violence.V4<-subset(estimates.violence.V4, estimates.violence.V4$date.num!=47)
estimates.violence.V4<-subset(estimates.violence.V4, estimates.violence.V4$date.num!=48)
estimates.violence.V4<-subset(estimates.violence.V4, estimates.violence.V4$date.num!=49)
estimates.violence.V4<-subset(estimates.violence.V4, estimates.violence.V4$date.num!=51)
estimates.violence.V4<-subset(estimates.violence.V4, estimates.violence.V4$date.num!=52)
estimates.violence.V4<-subset(estimates.violence.V4, estimates.violence.V4$date.num!=54)
estimates.violence.V4<-subset(estimates.violence.V4, estimates.violence.V4$date.num!=55)
estimates.violence.V4<-subset(estimates.violence.V4, estimates.violence.V4$date.num!=56)
estimates.violence.V4<-subset(estimates.violence.V4, estimates.violence.V4$date.num!=58)
estimates.violence.V4<-subset(estimates.violence.V4, estimates.violence.V4$date.num!=59)
estimates.violence.V4<-subset(estimates.violence.V4, estimates.violence.V4$date.num!=60)
estimates.violence.V4<-subset(estimates.violence.V4, estimates.violence.V4$date.num!=61)
estimates.violence.V4<-subset(estimates.violence.V4, estimates.violence.V4$date.num!=62)
estimates.violence.V4<-subset(estimates.violence.V4, estimates.violence.V4$date.num!=63)
estimates.violence.V4<-subset(estimates.violence.V4, estimates.violence.V4$date.num!=64)
estimates.violence.V4<-subset(estimates.violence.V4, estimates.violence.V4$date.num!=65)
estimates.violence.V4<-subset(estimates.violence.V4, estimates.violence.V4$date.num!=66)
estimates.violence.V4<-subset(estimates.violence.V4, estimates.violence.V4$date.num!=67)
estimates.violence.V4<-subset(estimates.violence.V4, estimates.violence.V4$date.num!=68)
estimates.violence.V4<-subset(estimates.violence.V4, estimates.violence.V4$date.num!=69)
estimates.violence.V4<-subset(estimates.violence.V4, estimates.violence.V4$date.num!=70)
estimates.violence.V4<-subset(estimates.violence.V4, estimates.violence.V4$date.num!=71)
estimates.violence.V4<-subset(estimates.violence.V4, estimates.violence.V4$date.num!=72)
estimates.violence.V4<-subset(estimates.violence.V4, estimates.violence.V4$date.num!=73)
estimates.violence.V4<-subset(estimates.violence.V4, estimates.violence.V4$date.num!=75)
estimates.violence.V4<-subset(estimates.violence.V4, estimates.violence.V4$date.num!=76)
estimates.violence.V4<-subset(estimates.violence.V4, estimates.violence.V4$date.num!=77)
estimates.violence.V4<-subset(estimates.violence.V4, estimates.violence.V4$date.num!=79)
estimates.violence.V4<-subset(estimates.violence.V4, estimates.violence.V4$date.num!=80)
estimates.violence.V4<-subset(estimates.violence.V4, estimates.violence.V4$date.num!=82)
estimates.violence.V4<-subset(estimates.violence.V4, estimates.violence.V4$date.num!=83)
estimates.violence.V4<-subset(estimates.violence.V4, estimates.violence.V4$date.num!=84)
estimates.violence.V4<-subset(estimates.violence.V4, estimates.violence.V4$date.num!=86)
estimates.violence.V4<-subset(estimates.violence.V4, estimates.violence.V4$date.num!=87)
estimates.violence.V4<-subset(estimates.violence.V4, estimates.violence.V4$date.num!=89)
estimates.violence.V4<-subset(estimates.violence.V4, estimates.violence.V4$date.num!=90)
estimates.violence.V4<-subset(estimates.violence.V4, estimates.violence.V4$date.num!=91)
estimates.violence.V4<-subset(estimates.violence.V4, estimates.violence.V4$date.num!=93)
estimates.violence.V4<-subset(estimates.violence.V4, estimates.violence.V4$date.num!=94)

pdf('fig-violenceV4.pdf', width=7.75, height=3.5)
ggplot(estimates.violence.V4, aes(x=date.num, y=mean, colour=party)) + geom_errorbar(aes(ymin=l95ci, ymax=u95ci), width=0, position=pd,  alpha=.5) +geom_line(position=pd, alpha=.5) +geom_point(position=pd, alpha=.5) + geom_line(aes(group = party), position=pd,alpha=.5) + theme_bw() + xlab("Date")+ ylab("Agreement Level") + ylim(c(1.5,3.5)) + guides(color=guide_legend(title=NULL)) + ggtitle("Outcome: Willingness to Go to War to Protect Rights of Group") + scale_color_manual(values=c("dodgerblue3","palevioletred4","red2")) + theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=7)) + facet_grid(. ~ party) + scale_x_continuous(breaks=estimates.violence.V4$date.num, labels=estimates.violence.V4$date) + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + theme(plot.title = element_text(size = 11))  + theme(axis.title.x=element_blank()) + annotate("text", x = 8, y = 1.5, label = "D", color="darkgreen", size=3, fontface =2) + annotate("text", x = 12, y = 1.5, label = "M", color="darkgreen", size=3, fontface =2) + annotate("text", x = 49, y = 1.5, label = "E", color="darkgreen", size=3, fontface =2) + annotate("text", x = 37, y = 1.5, label = "B", color="darkgreen", size=3, fontface =2) + annotate("text", x = 25, y = 1.5, label = "L", color="darkgreen", size=3, fontface =2)  + annotate("text", x = 73, y = 1.5, label = "C", color="darkgreen", size=3, fontface =2) + annotate("text", x = 86, y = 1.5, label = "I", color="darkgreen", size=3, fontface =2)
dev.off()


estimates.violence.V5<-subset(estimates.violence.V5, is.na(estimates.violence.V5$mean)==FALSE)

estimates.violence.V5$date[estimates.violence.V5$date=="10-26"]<-"."
estimates.violence.V5$date[estimates.violence.V5$date=="10-27"]<-"."
estimates.violence.V5$date[estimates.violence.V5$date=="10-29"]<-"."
estimates.violence.V5$date[estimates.violence.V5$date=="10-30"]<-"."
estimates.violence.V5$date[estimates.violence.V5$date=="11-01"]<-"."
estimates.violence.V5$date[estimates.violence.V5$date=="11-02"]<-"."
estimates.violence.V5$date[estimates.violence.V5$date=="11-03"]<-"."
estimates.violence.V5$date[estimates.violence.V5$date=="11-04"]<-"."
estimates.violence.V5$date[estimates.violence.V5$date=="11-05"]<-"."
estimates.violence.V5$date[estimates.violence.V5$date=="11-07"]<-"."
estimates.violence.V5$date[estimates.violence.V5$date=="11-08"]<-"."
estimates.violence.V5$date[estimates.violence.V5$date=="11-09"]<-"."
estimates.violence.V5$date[estimates.violence.V5$date=="11-10"]<-"."
estimates.violence.V5$date[estimates.violence.V5$date=="11-11"]<-"."
estimates.violence.V5$date[estimates.violence.V5$date=="11-13"]<-"."
estimates.violence.V5$date[estimates.violence.V5$date=="11-14"]<-"."
estimates.violence.V5$date[estimates.violence.V5$date=="11-15"]<-"."
estimates.violence.V5$date[estimates.violence.V5$date=="11-16"]<-"."
estimates.violence.V5$date[estimates.violence.V5$date=="11-17"]<-"."
estimates.violence.V5$date[estimates.violence.V5$date=="11-19"]<-"."
estimates.violence.V5$date[estimates.violence.V5$date=="11-20"]<-"."

estimates.violence.V5<-subset(estimates.violence.V5, estimates.violence.V5$date.num!=26)
estimates.violence.V5<-subset(estimates.violence.V5, estimates.violence.V5$date.num!=27)
estimates.violence.V5<-subset(estimates.violence.V5, estimates.violence.V5$date.num!=28)
estimates.violence.V5<-subset(estimates.violence.V5, estimates.violence.V5$date.num!=30)
estimates.violence.V5<-subset(estimates.violence.V5, estimates.violence.V5$date.num!=31)
estimates.violence.V5<-subset(estimates.violence.V5, estimates.violence.V5$date.num!=32)
estimates.violence.V5<-subset(estimates.violence.V5, estimates.violence.V5$date.num!=33)
estimates.violence.V5<-subset(estimates.violence.V5, estimates.violence.V5$date.num!=34)
estimates.violence.V5<-subset(estimates.violence.V5, estimates.violence.V5$date.num!=36)
estimates.violence.V5<-subset(estimates.violence.V5, estimates.violence.V5$date.num!=37)
estimates.violence.V5<-subset(estimates.violence.V5, estimates.violence.V5$date.num!=38)
estimates.violence.V5<-subset(estimates.violence.V5, estimates.violence.V5$date.num!=40)
estimates.violence.V5<-subset(estimates.violence.V5, estimates.violence.V5$date.num!=41)
estimates.violence.V5<-subset(estimates.violence.V5, estimates.violence.V5$date.num!=42)
estimates.violence.V5<-subset(estimates.violence.V5, estimates.violence.V5$date.num!=44)
estimates.violence.V5<-subset(estimates.violence.V5, estimates.violence.V5$date.num!=45)
estimates.violence.V5<-subset(estimates.violence.V5, estimates.violence.V5$date.num!=47)
estimates.violence.V5<-subset(estimates.violence.V5, estimates.violence.V5$date.num!=48)
estimates.violence.V5<-subset(estimates.violence.V5, estimates.violence.V5$date.num!=49)
estimates.violence.V5<-subset(estimates.violence.V5, estimates.violence.V5$date.num!=51)
estimates.violence.V5<-subset(estimates.violence.V5, estimates.violence.V5$date.num!=52)
estimates.violence.V5<-subset(estimates.violence.V5, estimates.violence.V5$date.num!=54)
estimates.violence.V5<-subset(estimates.violence.V5, estimates.violence.V5$date.num!=55)
estimates.violence.V5<-subset(estimates.violence.V5, estimates.violence.V5$date.num!=56)
estimates.violence.V5<-subset(estimates.violence.V5, estimates.violence.V5$date.num!=58)
estimates.violence.V5<-subset(estimates.violence.V5, estimates.violence.V5$date.num!=59)
estimates.violence.V5<-subset(estimates.violence.V5, estimates.violence.V5$date.num!=60)
estimates.violence.V5<-subset(estimates.violence.V5, estimates.violence.V5$date.num!=61)
estimates.violence.V5<-subset(estimates.violence.V5, estimates.violence.V5$date.num!=62)
estimates.violence.V5<-subset(estimates.violence.V5, estimates.violence.V5$date.num!=63)
estimates.violence.V5<-subset(estimates.violence.V5, estimates.violence.V5$date.num!=64)
estimates.violence.V5<-subset(estimates.violence.V5, estimates.violence.V5$date.num!=65)
estimates.violence.V5<-subset(estimates.violence.V5, estimates.violence.V5$date.num!=66)
estimates.violence.V5<-subset(estimates.violence.V5, estimates.violence.V5$date.num!=67)
estimates.violence.V5<-subset(estimates.violence.V5, estimates.violence.V5$date.num!=68)
estimates.violence.V5<-subset(estimates.violence.V5, estimates.violence.V5$date.num!=69)
estimates.violence.V5<-subset(estimates.violence.V5, estimates.violence.V5$date.num!=70)
estimates.violence.V5<-subset(estimates.violence.V5, estimates.violence.V5$date.num!=71)
estimates.violence.V5<-subset(estimates.violence.V5, estimates.violence.V5$date.num!=72)
estimates.violence.V5<-subset(estimates.violence.V5, estimates.violence.V5$date.num!=73)
estimates.violence.V5<-subset(estimates.violence.V5, estimates.violence.V5$date.num!=75)
estimates.violence.V5<-subset(estimates.violence.V5, estimates.violence.V5$date.num!=76)
estimates.violence.V5<-subset(estimates.violence.V5, estimates.violence.V5$date.num!=77)
estimates.violence.V5<-subset(estimates.violence.V5, estimates.violence.V5$date.num!=79)
estimates.violence.V5<-subset(estimates.violence.V5, estimates.violence.V5$date.num!=80)
estimates.violence.V5<-subset(estimates.violence.V5, estimates.violence.V5$date.num!=82)
estimates.violence.V5<-subset(estimates.violence.V5, estimates.violence.V5$date.num!=83)
estimates.violence.V5<-subset(estimates.violence.V5, estimates.violence.V5$date.num!=84)
estimates.violence.V5<-subset(estimates.violence.V5, estimates.violence.V5$date.num!=86)
estimates.violence.V5<-subset(estimates.violence.V5, estimates.violence.V5$date.num!=87)
estimates.violence.V5<-subset(estimates.violence.V5, estimates.violence.V5$date.num!=89)
estimates.violence.V5<-subset(estimates.violence.V5, estimates.violence.V5$date.num!=90)
estimates.violence.V5<-subset(estimates.violence.V5, estimates.violence.V5$date.num!=91)
estimates.violence.V5<-subset(estimates.violence.V5, estimates.violence.V5$date.num!=93)
estimates.violence.V5<-subset(estimates.violence.V5, estimates.violence.V5$date.num!=94)

pdf('fig-violenceV5.pdf', width=7.75, height=3.5)
ggplot(estimates.violence.V5, aes(x=date.num, y=mean, colour=party)) + geom_errorbar(aes(ymin=l95ci, ymax=u95ci), width=0, position=pd,  alpha=.5) +geom_line(position=pd, alpha=.5) +geom_point(position=pd, alpha=.5) + geom_line(aes(group = party), position=pd,alpha=.5) + theme_bw() + xlab("Date")+ ylab("Agreement Level") + ylim(c(1.5,3.5)) + guides(color=guide_legend(title=NULL)) + ggtitle("Outcome: Willingness to Retaliate Against Members of Other Group") + scale_color_manual(values=c("dodgerblue3","palevioletred4","red2")) + theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=7)) + facet_grid(. ~ party) + scale_x_continuous(breaks=estimates.violence.V5$date.num, labels=estimates.violence.V5$date) + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + theme(plot.title = element_text(size = 11))  + theme(axis.title.x=element_blank()) + annotate("text", x = 8, y = 1.5, label = "D", color="darkgreen", size=3, fontface =2) + annotate("text", x = 12, y = 1.5, label = "M", color="darkgreen", size=3, fontface =2) + annotate("text", x = 49, y = 1.5, label = "E", color="darkgreen", size=3, fontface =2) + annotate("text", x = 37, y = 1.5, label = "B", color="darkgreen", size=3, fontface =2) + annotate("text", x = 25, y = 1.5, label = "L", color="darkgreen", size=3, fontface =2)  + annotate("text", x = 73, y = 1.5, label = "C", color="darkgreen", size=3, fontface =2) + annotate("text", x = 86, y = 1.5, label = "I", color="darkgreen", size=3, fontface =2)
dev.off()


m1<-lm(violence ~ -1+date, data = data, weights=data$w)
summary(m1)

m2<-lm(violence ~ -1+date, data = data)
summary(m2)

estimates.violence.pooled<-estimates.pooled

results<-data.frame((summary(m1)$coefficients[,1:2]))
colnames(results)<-c("mean","se")
results$date<-row.names(results)
results$date<-str_remove(results$date, "date")
results<-data.frame(cbind(results$date,results$mean,results$se))
colnames(results)<-c("date","mean","se")

estimates.violence.pooled<-merge(estimates.violence.pooled,results,by="date")

estimates.violence.pooled$mean<-as.numeric(estimates.violence.pooled$mean)
estimates.violence.pooled$se<-as.numeric(estimates.violence.pooled$se)

estimates.violence.pooled$l95ci<-estimates.violence.pooled$mean-1.96*estimates.violence.pooled$se
estimates.violence.pooled$u95ci<-estimates.violence.pooled$mean+1.96*estimates.violence.pooled$se

estimates.violence$date[estimates.violence$date=="11-05"]<-"."
estimates.violence$date[estimates.violence$date=="11-07"]<-"."
estimates.violence$date[estimates.violence$date=="11-09"]<-"."
estimates.violence$date[estimates.violence$date=="11-11"]<-"."
estimates.violence$date[estimates.violence$date=="11-13"]<-"."
estimates.violence$date[estimates.violence$date=="11-15"]<-"."
estimates.violence$date[estimates.violence$date=="11-17"]<-"."
estimates.violence$date[estimates.violence$date=="11-19"]<-"."

pdf('fig-violence-pooled.pdf', width=7.25, height=3.5)
ggplot(estimates.violence.pooled, aes(x=date.num, y=mean))  + geom_errorbar(aes(ymin=l95ci, ymax=u95ci), width=0, position=pd,  alpha=.5) +geom_point(position=pd, alpha=.5) + geom_line(position=pd,alpha=.5) + theme_bw() + ylab("Radicalism Intention Scale") + ylim(c(1.5,3.5))  + ggtitle("Outcome: Radicalism Intention Scale")  + theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=7))  + scale_x_continuous(breaks=estimates.violence.pooled$date.num, labels=estimates.violence.pooled$date) + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + theme(plot.title = element_text(size = 11)) + theme(axis.title.x=element_blank()) + annotate("text", x = 8, y = 1.5, label = "D", color="darkgreen", size=3, fontface =2) + annotate("text", x = 12, y = 1.5, label = "M", color="darkgreen", size=3, fontface =2) + annotate("text", x = 49, y = 1.5, label = "E", color="darkgreen", size=3, fontface =2) + annotate("text", x = 37, y = 1.5, label = "B", color="darkgreen", size=3, fontface =2) + annotate("text", x = 25, y = 1.5, label = "L", color="darkgreen", size=3, fontface =2)  + annotate("text", x = 73, y = 1.5, label = "I", color="darkgreen", size=3, fontface =2)
dev.off()

estimates.violence.pooled.raw<-estimates.pooled

results<-data.frame((summary(m2)$coefficients[,1:2]))
colnames(results)<-c("mean","se")
results$date<-row.names(results)
results$date<-str_remove(results$date, "date")
results<-data.frame(cbind(results$date,results$mean,results$se))
colnames(results)<-c("date","mean","se")

estimates.violence.pooled.raw<-merge(estimates.violence.pooled.raw,results,by="date")

estimates.violence.pooled.raw$mean<-as.numeric(estimates.violence.pooled.raw$mean)
estimates.violence.pooled.raw$se<-as.numeric(estimates.violence.pooled.raw$se)

estimates.violence.pooled.raw$l95ci<-estimates.violence.pooled.raw$mean-1.96*estimates.violence.pooled.raw$se
estimates.violence.pooled.raw$u95ci<-estimates.violence.pooled.raw$mean+1.96*estimates.violence.pooled.raw$se

estimates.violence.pooled.raw$date[estimates.violence.pooled.raw$date=="11-05"]<-"."
estimates.violence.pooled.raw$date[estimates.violence.pooled.raw$date=="11-07"]<-"."
estimates.violence.pooled.raw$date[estimates.violence.pooled.raw$date=="11-09"]<-"."
estimates.violence.pooled.raw$date[estimates.violence.pooled.raw$date=="11-11"]<-"."
estimates.violence.pooled.raw$date[estimates.violence.pooled.raw$date=="11-13"]<-"."
estimates.violence.pooled.raw$date[estimates.violence.pooled.raw$date=="11-15"]<-"."
estimates.violence.pooled.raw$date[estimates.violence.pooled.raw$date=="11-17"]<-"."
estimates.violence.pooled.raw$date[estimates.violence.pooled.raw$date=="11-19"]<-"."

pdf('fig-violence-pooled-raw.pdf', width=7.25, height=3.5)
ggplot(estimates.violence.pooled.raw, aes(x=date.num, y=mean))  + geom_errorbar(aes(ymin=l95ci, ymax=u95ci), width=0, position=pd,  alpha=.5) +geom_point(position=pd, alpha=.5) + geom_line(position=pd,alpha=.5) + theme_bw() + ylab("Radicalism Intention Scale") + ylim(c(1.5,3.5))  + ggtitle("Outcome: Radicalism Intention Scale")  + theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=7))  + scale_x_continuous(breaks=estimates.violence.pooled.raw$date.num, labels=estimates.violence.pooled.raw$date) + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + theme(plot.title = element_text(size = 11)) + theme(axis.title.x=element_blank()) + annotate("text", x = 8, y = 1.5, label = "D", color="darkgreen", size=3, fontface =2) + annotate("text", x = 12, y = 1.5, label = "M", color="darkgreen", size=3, fontface =2) + annotate("text", x = 49, y = 1.5, label = "E", color="darkgreen", size=3, fontface =2) + annotate("text", x = 37, y = 1.5, label = "B", color="darkgreen", size=3, fontface =2) + annotate("text", x = 25, y = 1.5, label = "L", color="darkgreen", size=3, fontface =2)  + annotate("text", x = 73, y = 1.5, label = "I", color="darkgreen", size=3, fontface =2)
dev.off()

data.postcall.rep$age.group <- factor(data.postcall.rep$age.group, levels = c("65+","18-29","30-49","50-64"))
data.postcall.indother$age.group <- factor(data.postcall.indother$age.group, levels = c("65+","18-29","30-49","50-64"))
data.postcall.dem$age.group <- factor(data.postcall.dem$age.group, levels = c("65+","18-29","30-49","50-64"))

data.postcall.rep$ed.group <- factor(data.postcall.rep$ed.group, levels = c("Postgrad","HS","College"))
data.postcall.indother$ed.group <- factor(data.postcall.indother$ed.group, levels = c("Postgrad","HS","College"))
data.postcall.dem$ed.group <- factor(data.postcall.dem$ed.group, levels = c("Postgrad","HS","College"))


m3<-lm(violence ~ gender + age.group + ed.group + social.status.group, data = data.postcall.dem, weights=data.postcall.dem$w)
summary(m3)
m4<-lm(violence ~ gender + age.group + ed.group + social.status.group, data = data.postcall.rep, weights=data.postcall.rep$w)
summary(m4)
m5<-lm(violence ~ gender + age.group + ed.group + social.status.group, data = data.postcall.indother, weights=data.postcall.indother$w)
summary(m5)

pdf('fig-linearprob-violence.pdf', width=8, height=5)
ggcoef_compare(list("Democrat" = m3, "Independent/Other" = m5, "Republican" = m4), type="faceted",variable_labels = c(gender = "Gender", age.group="Age Group",ed.group="Education",social.status.group="Social Status"), point_size=2,point_stroke=2,point_fill="grey50",stripped_rows=FALSE,colour=c("red1","purple"), significance_labels = NULL) + xlab("Coefficient Estimate") + theme_bw() + guides(color=guide_legend(title=NULL)) + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()) + theme(plot.title = element_text(size = 11)) + ggtitle("Outcome: Radicalism Intention Scale") + theme(legend.title = element_blank())   + scale_fill_manual(values = c('grey50')) + scale_color_manual(values = c('grey10','grey10','grey10','grey10')) +  scale_shape_manual(values = c(16,16)) + theme(legend.position = "none")
dev.off()


m3<-lm(violence ~ gender + age.group + ed.group + social.status.group, data = data.postcall.dem)
summary(m3)
m4<-lm(violence ~ gender + age.group + ed.group + social.status.group, data = data.postcall.rep)
summary(m4)
m5<-lm(violence ~ gender + age.group + ed.group + social.status.group, data = data.postcall.indother)
summary(m5)

pdf('fig-linearprob-violence-raw.pdf', width=8, height=5)
ggcoef_compare(list("Democrat" = m3, "Independent/Other" = m5, "Republican" = m4), type="faceted",variable_labels = c(gender = "Gender", age.group="Age Group",ed.group="Education",social.status.group="Social Status"), point_size=2,point_stroke=2,point_fill="grey50",stripped_rows=FALSE,colour=c("red1","purple"), significance_labels = NULL) + xlab("Coefficient Estimate") + theme_bw() + guides(color=guide_legend(title=NULL)) + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()) + theme(plot.title = element_text(size = 11)) + ggtitle("Outcome: Radicalism Intention Scale") + theme(legend.title = element_blank())   + scale_fill_manual(values = c('grey50')) + scale_color_manual(values = c('grey10','grey10','grey10','grey10')) +  scale_shape_manual(values = c(16,16)) + theme(legend.position = "none")
dev.off()

m1<-lm(violence ~ postelection + postcall + postmichigan + postbarr  + postelectoralcollege + postinsurrection +  postinauguration, data = data, weights=data$w)
summary(m1)
m2<-lm(violence ~ postelection + postcall + postmichigan + postbarr  + postelectoralcollege + postinsurrection +  postinauguration, data = data.rep, weights=data.rep$w)
summary(m2)
m3<-lm(violence ~ postelection + postcall + postmichigan + postbarr  + postelectoralcollege + postinsurrection +  postinauguration, data = data.dem, weights=data.dem$w)
summary(m3)
m4<-lm(violence ~ postelection + postcall + postmichigan + postbarr  + postelectoralcollege + postinsurrection +  postinauguration, data = data.indother, weights=data.indother$w)
summary(m4)

m1tot<-lm(violence ~ postelection , data = data, weights=data$w)
summary(m1tot)
m2tot<-lm(violence ~ postelection , data = data.rep, weights=data.rep$w)
summary(m2tot)
m3tot<-lm(violence ~ postelection , data = data.dem, weights=data.dem$w)
summary(m3tot)
m4tot<-lm(violence ~ postelection , data = data.indother, weights=data.indother$w)
summary(m4tot)

#Identity

data$trump.id.mean = rowMeans(data[,c("trump.id", "trump.id2")], na.rm=TRUE)
data$biden.id.mean = rowMeans(data[,c("biden.id", "biden.id2")], na.rm=TRUE)

estimates.trump.id.mean.rep<-subset(estimates.trump.id.mean.rep, estimates.trump.id.mean.rep$date.num!=26)
estimates.trump.id.mean.rep<-subset(estimates.trump.id.mean.rep, estimates.trump.id.mean.rep$date.num!=27)
estimates.trump.id.mean.rep<-subset(estimates.trump.id.mean.rep, estimates.trump.id.mean.rep$date.num!=28)
estimates.trump.id.mean.rep<-subset(estimates.trump.id.mean.rep, estimates.trump.id.mean.rep$date.num!=30)
estimates.trump.id.mean.rep<-subset(estimates.trump.id.mean.rep, estimates.trump.id.mean.rep$date.num!=31)
estimates.trump.id.mean.rep<-subset(estimates.trump.id.mean.rep, estimates.trump.id.mean.rep$date.num!=32)
estimates.trump.id.mean.rep<-subset(estimates.trump.id.mean.rep, estimates.trump.id.mean.rep$date.num!=33)
estimates.trump.id.mean.rep<-subset(estimates.trump.id.mean.rep, estimates.trump.id.mean.rep$date.num!=34)
estimates.trump.id.mean.rep<-subset(estimates.trump.id.mean.rep, estimates.trump.id.mean.rep$date.num!=36)
estimates.trump.id.mean.rep<-subset(estimates.trump.id.mean.rep, estimates.trump.id.mean.rep$date.num!=37)
estimates.trump.id.mean.rep<-subset(estimates.trump.id.mean.rep, estimates.trump.id.mean.rep$date.num!=38)
estimates.trump.id.mean.rep<-subset(estimates.trump.id.mean.rep, estimates.trump.id.mean.rep$date.num!=40)
estimates.trump.id.mean.rep<-subset(estimates.trump.id.mean.rep, estimates.trump.id.mean.rep$date.num!=41)
estimates.trump.id.mean.rep<-subset(estimates.trump.id.mean.rep, estimates.trump.id.mean.rep$date.num!=42)
estimates.trump.id.mean.rep<-subset(estimates.trump.id.mean.rep, estimates.trump.id.mean.rep$date.num!=44)
estimates.trump.id.mean.rep<-subset(estimates.trump.id.mean.rep, estimates.trump.id.mean.rep$date.num!=45)
estimates.trump.id.mean.rep<-subset(estimates.trump.id.mean.rep, estimates.trump.id.mean.rep$date.num!=47)
estimates.trump.id.mean.rep<-subset(estimates.trump.id.mean.rep, estimates.trump.id.mean.rep$date.num!=48)
estimates.trump.id.mean.rep<-subset(estimates.trump.id.mean.rep, estimates.trump.id.mean.rep$date.num!=49)
estimates.trump.id.mean.rep<-subset(estimates.trump.id.mean.rep, estimates.trump.id.mean.rep$date.num!=51)
estimates.trump.id.mean.rep<-subset(estimates.trump.id.mean.rep, estimates.trump.id.mean.rep$date.num!=52)
estimates.trump.id.mean.rep<-subset(estimates.trump.id.mean.rep, estimates.trump.id.mean.rep$date.num!=54)
estimates.trump.id.mean.rep<-subset(estimates.trump.id.mean.rep, estimates.trump.id.mean.rep$date.num!=55)
estimates.trump.id.mean.rep<-subset(estimates.trump.id.mean.rep, estimates.trump.id.mean.rep$date.num!=56)
estimates.trump.id.mean.rep<-subset(estimates.trump.id.mean.rep, estimates.trump.id.mean.rep$date.num!=58)
estimates.trump.id.mean.rep<-subset(estimates.trump.id.mean.rep, estimates.trump.id.mean.rep$date.num!=59)
estimates.trump.id.mean.rep<-subset(estimates.trump.id.mean.rep, estimates.trump.id.mean.rep$date.num!=60)
estimates.trump.id.mean.rep<-subset(estimates.trump.id.mean.rep, estimates.trump.id.mean.rep$date.num!=61)
estimates.trump.id.mean.rep<-subset(estimates.trump.id.mean.rep, estimates.trump.id.mean.rep$date.num!=62)
estimates.trump.id.mean.rep<-subset(estimates.trump.id.mean.rep, estimates.trump.id.mean.rep$date.num!=63)
estimates.trump.id.mean.rep<-subset(estimates.trump.id.mean.rep, estimates.trump.id.mean.rep$date.num!=64)
estimates.trump.id.mean.rep<-subset(estimates.trump.id.mean.rep, estimates.trump.id.mean.rep$date.num!=65)
estimates.trump.id.mean.rep<-subset(estimates.trump.id.mean.rep, estimates.trump.id.mean.rep$date.num!=66)
estimates.trump.id.mean.rep<-subset(estimates.trump.id.mean.rep, estimates.trump.id.mean.rep$date.num!=67)
estimates.trump.id.mean.rep<-subset(estimates.trump.id.mean.rep, estimates.trump.id.mean.rep$date.num!=68)
estimates.trump.id.mean.rep<-subset(estimates.trump.id.mean.rep, estimates.trump.id.mean.rep$date.num!=69)
estimates.trump.id.mean.rep<-subset(estimates.trump.id.mean.rep, estimates.trump.id.mean.rep$date.num!=70)
estimates.trump.id.mean.rep<-subset(estimates.trump.id.mean.rep, estimates.trump.id.mean.rep$date.num!=71)
estimates.trump.id.mean.rep<-subset(estimates.trump.id.mean.rep, estimates.trump.id.mean.rep$date.num!=72)
estimates.trump.id.mean.rep<-subset(estimates.trump.id.mean.rep, estimates.trump.id.mean.rep$date.num!=73)
estimates.trump.id.mean.rep<-subset(estimates.trump.id.mean.rep, estimates.trump.id.mean.rep$date.num!=75)
estimates.trump.id.mean.rep<-subset(estimates.trump.id.mean.rep, estimates.trump.id.mean.rep$date.num!=76)
estimates.trump.id.mean.rep<-subset(estimates.trump.id.mean.rep, estimates.trump.id.mean.rep$date.num!=77)
estimates.trump.id.mean.rep<-subset(estimates.trump.id.mean.rep, estimates.trump.id.mean.rep$date.num!=79)
estimates.trump.id.mean.rep<-subset(estimates.trump.id.mean.rep, estimates.trump.id.mean.rep$date.num!=80)
estimates.trump.id.mean.rep<-subset(estimates.trump.id.mean.rep, estimates.trump.id.mean.rep$date.num!=82)
estimates.trump.id.mean.rep<-subset(estimates.trump.id.mean.rep, estimates.trump.id.mean.rep$date.num!=83)
estimates.trump.id.mean.rep<-subset(estimates.trump.id.mean.rep, estimates.trump.id.mean.rep$date.num!=84)
estimates.trump.id.mean.rep<-subset(estimates.trump.id.mean.rep, estimates.trump.id.mean.rep$date.num!=86)
estimates.trump.id.mean.rep<-subset(estimates.trump.id.mean.rep, estimates.trump.id.mean.rep$date.num!=87)
estimates.trump.id.mean.rep<-subset(estimates.trump.id.mean.rep, estimates.trump.id.mean.rep$date.num!=89)
estimates.trump.id.mean.rep<-subset(estimates.trump.id.mean.rep, estimates.trump.id.mean.rep$date.num!=90)
estimates.trump.id.mean.rep<-subset(estimates.trump.id.mean.rep, estimates.trump.id.mean.rep$date.num!=91)
estimates.trump.id.mean.rep<-subset(estimates.trump.id.mean.rep, estimates.trump.id.mean.rep$date.num!=93)
estimates.trump.id.mean.rep<-subset(estimates.trump.id.mean.rep, estimates.trump.id.mean.rep$date.num!=94)

estimates.trump.id.mean.rep$l95ci<-estimates.trump.id.mean.rep$mean-1.96*estimates.trump.id.mean.rep$se
estimates.trump.id.mean.rep$u95ci<-estimates.trump.id.mean.rep$mean+1.96*estimates.trump.id.mean.rep$se
estimates.trump.id.mean.rep$weights<-"Weighted"

estimates.trump.id.mean.rep$date[estimates.trump.id.mean.rep$date=="11-05"]<-"."
estimates.trump.id.mean.rep$date[estimates.trump.id.mean.rep$date=="11-07"]<-"."
estimates.trump.id.mean.rep$date[estimates.trump.id.mean.rep$date=="11-09"]<-"."
estimates.trump.id.mean.rep$date[estimates.trump.id.mean.rep$date=="11-11"]<-"."
estimates.trump.id.mean.rep$date[estimates.trump.id.mean.rep$date=="11-13"]<-"."
estimates.trump.id.mean.rep$date[estimates.trump.id.mean.rep$date=="11-15"]<-"."
estimates.trump.id.mean.rep$date[estimates.trump.id.mean.rep$date=="11-17"]<-"."
estimates.trump.id.mean.rep$date[estimates.trump.id.mean.rep$date=="11-19"]<-"."

pdf('fig-trumpidmean-rep.pdf', width=7.25, height=3.5)
ggplot(estimates.trump.id.mean.rep, aes(x=date.num, y=mean))  + geom_errorbar(aes(ymin=l95ci, ymax=u95ci), width=0, position=pd,  alpha=.5, color="red1") +geom_point(position=pd, alpha=.5,color="red1") + geom_line(position=pd,alpha=.5,color="red1") + theme_bw() + ylab("Trump Identification (mean)") + ylim(c(3,4.4))  + ggtitle("Outcome: Trump Identification, Sample: Republicans")  + theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=7))  + scale_x_continuous(breaks=estimates.trump.id.mean.rep$date.num, labels=estimates.trump.id.mean.rep$date) + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + theme(plot.title = element_text(size = 11)) + theme(axis.title.x=element_blank())  + annotate("text", x = 8, y = 3, label = "D", color="darkgreen", size=3, fontface =2) + annotate("text", x = 12, y = 3, label = "M", color="darkgreen", size=3, fontface =2) + annotate("text", x = 49, y = 3, label = "E", color="darkgreen", size=3, fontface =2) + annotate("text", x = 37, y = 3, label = "B", color="darkgreen", size=3, fontface =2) + annotate("text", x = 25, y = 3, label = "L", color="darkgreen", size=3, fontface =2)  + annotate("text", x = 73, y = 3, label = "C", color="darkgreen", size=3, fontface =2) + annotate("text", x = 86, y = 3, label = "I", color="darkgreen", size=3, fontface =2)
dev.off()

#Biden ID

estimates.biden.id.mean.dem<-subset(estimates.biden.id.mean.dem, estimates.biden.id.mean.dem$date.num!=26)
estimates.biden.id.mean.dem<-subset(estimates.biden.id.mean.dem, estimates.biden.id.mean.dem$date.num!=27)
estimates.biden.id.mean.dem<-subset(estimates.biden.id.mean.dem, estimates.biden.id.mean.dem$date.num!=28)
estimates.biden.id.mean.dem<-subset(estimates.biden.id.mean.dem, estimates.biden.id.mean.dem$date.num!=30)
estimates.biden.id.mean.dem<-subset(estimates.biden.id.mean.dem, estimates.biden.id.mean.dem$date.num!=31)
estimates.biden.id.mean.dem<-subset(estimates.biden.id.mean.dem, estimates.biden.id.mean.dem$date.num!=32)
estimates.biden.id.mean.dem<-subset(estimates.biden.id.mean.dem, estimates.biden.id.mean.dem$date.num!=33)
estimates.biden.id.mean.dem<-subset(estimates.biden.id.mean.dem, estimates.biden.id.mean.dem$date.num!=34)
estimates.biden.id.mean.dem<-subset(estimates.biden.id.mean.dem, estimates.biden.id.mean.dem$date.num!=36)
estimates.biden.id.mean.dem<-subset(estimates.biden.id.mean.dem, estimates.biden.id.mean.dem$date.num!=37)
estimates.biden.id.mean.dem<-subset(estimates.biden.id.mean.dem, estimates.biden.id.mean.dem$date.num!=38)
estimates.biden.id.mean.dem<-subset(estimates.biden.id.mean.dem, estimates.biden.id.mean.dem$date.num!=40)
estimates.biden.id.mean.dem<-subset(estimates.biden.id.mean.dem, estimates.biden.id.mean.dem$date.num!=41)
estimates.biden.id.mean.dem<-subset(estimates.biden.id.mean.dem, estimates.biden.id.mean.dem$date.num!=42)
estimates.biden.id.mean.dem<-subset(estimates.biden.id.mean.dem, estimates.biden.id.mean.dem$date.num!=44)
estimates.biden.id.mean.dem<-subset(estimates.biden.id.mean.dem, estimates.biden.id.mean.dem$date.num!=45)
estimates.biden.id.mean.dem<-subset(estimates.biden.id.mean.dem, estimates.biden.id.mean.dem$date.num!=47)
estimates.biden.id.mean.dem<-subset(estimates.biden.id.mean.dem, estimates.biden.id.mean.dem$date.num!=48)
estimates.biden.id.mean.dem<-subset(estimates.biden.id.mean.dem, estimates.biden.id.mean.dem$date.num!=49)
estimates.biden.id.mean.dem<-subset(estimates.biden.id.mean.dem, estimates.biden.id.mean.dem$date.num!=51)
estimates.biden.id.mean.dem<-subset(estimates.biden.id.mean.dem, estimates.biden.id.mean.dem$date.num!=52)
estimates.biden.id.mean.dem<-subset(estimates.biden.id.mean.dem, estimates.biden.id.mean.dem$date.num!=54)
estimates.biden.id.mean.dem<-subset(estimates.biden.id.mean.dem, estimates.biden.id.mean.dem$date.num!=55)
estimates.biden.id.mean.dem<-subset(estimates.biden.id.mean.dem, estimates.biden.id.mean.dem$date.num!=56)
estimates.biden.id.mean.dem<-subset(estimates.biden.id.mean.dem, estimates.biden.id.mean.dem$date.num!=58)
estimates.biden.id.mean.dem<-subset(estimates.biden.id.mean.dem, estimates.biden.id.mean.dem$date.num!=59)
estimates.biden.id.mean.dem<-subset(estimates.biden.id.mean.dem, estimates.biden.id.mean.dem$date.num!=60)
estimates.biden.id.mean.dem<-subset(estimates.biden.id.mean.dem, estimates.biden.id.mean.dem$date.num!=61)
estimates.biden.id.mean.dem<-subset(estimates.biden.id.mean.dem, estimates.biden.id.mean.dem$date.num!=62)
estimates.biden.id.mean.dem<-subset(estimates.biden.id.mean.dem, estimates.biden.id.mean.dem$date.num!=63)
estimates.biden.id.mean.dem<-subset(estimates.biden.id.mean.dem, estimates.biden.id.mean.dem$date.num!=64)
estimates.biden.id.mean.dem<-subset(estimates.biden.id.mean.dem, estimates.biden.id.mean.dem$date.num!=65)
estimates.biden.id.mean.dem<-subset(estimates.biden.id.mean.dem, estimates.biden.id.mean.dem$date.num!=66)
estimates.biden.id.mean.dem<-subset(estimates.biden.id.mean.dem, estimates.biden.id.mean.dem$date.num!=67)
estimates.biden.id.mean.dem<-subset(estimates.biden.id.mean.dem, estimates.biden.id.mean.dem$date.num!=68)
estimates.biden.id.mean.dem<-subset(estimates.biden.id.mean.dem, estimates.biden.id.mean.dem$date.num!=69)
estimates.biden.id.mean.dem<-subset(estimates.biden.id.mean.dem, estimates.biden.id.mean.dem$date.num!=70)
estimates.biden.id.mean.dem<-subset(estimates.biden.id.mean.dem, estimates.biden.id.mean.dem$date.num!=71)
estimates.biden.id.mean.dem<-subset(estimates.biden.id.mean.dem, estimates.biden.id.mean.dem$date.num!=72)
estimates.biden.id.mean.dem<-subset(estimates.biden.id.mean.dem, estimates.biden.id.mean.dem$date.num!=73)
estimates.biden.id.mean.dem<-subset(estimates.biden.id.mean.dem, estimates.biden.id.mean.dem$date.num!=75)
estimates.biden.id.mean.dem<-subset(estimates.biden.id.mean.dem, estimates.biden.id.mean.dem$date.num!=76)
estimates.biden.id.mean.dem<-subset(estimates.biden.id.mean.dem, estimates.biden.id.mean.dem$date.num!=77)
estimates.biden.id.mean.dem<-subset(estimates.biden.id.mean.dem, estimates.biden.id.mean.dem$date.num!=79)
estimates.biden.id.mean.dem<-subset(estimates.biden.id.mean.dem, estimates.biden.id.mean.dem$date.num!=80)
estimates.biden.id.mean.dem<-subset(estimates.biden.id.mean.dem, estimates.biden.id.mean.dem$date.num!=82)
estimates.biden.id.mean.dem<-subset(estimates.biden.id.mean.dem, estimates.biden.id.mean.dem$date.num!=83)
estimates.biden.id.mean.dem<-subset(estimates.biden.id.mean.dem, estimates.biden.id.mean.dem$date.num!=84)
estimates.biden.id.mean.dem<-subset(estimates.biden.id.mean.dem, estimates.biden.id.mean.dem$date.num!=86)
estimates.biden.id.mean.dem<-subset(estimates.biden.id.mean.dem, estimates.biden.id.mean.dem$date.num!=87)
estimates.biden.id.mean.dem<-subset(estimates.biden.id.mean.dem, estimates.biden.id.mean.dem$date.num!=89)
estimates.biden.id.mean.dem<-subset(estimates.biden.id.mean.dem, estimates.biden.id.mean.dem$date.num!=90)
estimates.biden.id.mean.dem<-subset(estimates.biden.id.mean.dem, estimates.biden.id.mean.dem$date.num!=91)
estimates.biden.id.mean.dem<-subset(estimates.biden.id.mean.dem, estimates.biden.id.mean.dem$date.num!=93)
estimates.biden.id.mean.dem<-subset(estimates.biden.id.mean.dem, estimates.biden.id.mean.dem$date.num!=94)

estimates.biden.id.mean.dem$l95ci<-estimates.biden.id.mean.dem$mean-1.96*estimates.biden.id.mean.dem$se
estimates.biden.id.mean.dem$u95ci<-estimates.biden.id.mean.dem$mean+1.96*estimates.biden.id.mean.dem$se
estimates.biden.id.mean.dem$weights<-"Weighted"

estimates.biden.id.mean.dem$date[estimates.biden.id.mean.dem$date=="11-05"]<-"."
estimates.biden.id.mean.dem$date[estimates.biden.id.mean.demdate=="11-07"]<-"."
estimates.biden.id.mean.dem$date[estimates.biden.id.mean.dem$date=="11-09"]<-"."
estimates.biden.id.mean.dem$date[estimates.biden.id.mean.dem$date=="11-11"]<-"."
estimates.biden.id.mean.dem$date[estimates.biden.id.mean.dem$date=="11-13"]<-"."
estimates.biden.id.mean.dem$date[estimates.biden.id.mean.dem$date=="11-15"]<-"."
estimates.biden.id.mean.dem$date[estimates.biden.id.mean.dem$date=="11-17"]<-"."
estimates.biden.id.mean.dem$date[estimates.biden.id.mean.dem$date=="11-19"]<-"."

pdf('fig-bidenidmean-dem.pdf', width=7.25, height=3.5)
ggplot(estimates.biden.id.mean.dem, aes(x=date.num, y=mean))  + geom_errorbar(aes(ymin=l95ci, ymax=u95ci), width=0, position=pd,  alpha=.5, color="dodgerblue3") +geom_point(position=pd, alpha=.5,color="dodgerblue3") + geom_line(position=pd,alpha=.5,color="dodgerblue3") + theme_bw() + ylab("Biden Identification (mean)") + ylim(c(3,4.4))  + ggtitle("Outcome: Biden Identification, Sample: Democrats")  + theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=7))  + scale_x_continuous(breaks=estimates.biden.id.mean.dem$date.num, labels=estimates.biden.id.mean.dem$date) + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + theme(plot.title = element_text(size = 11)) + theme(axis.title.x=element_blank())  + annotate("text", x = 8, y = 3, label = "D", color="darkgreen", size=3, fontface =2) + annotate("text", x = 12, y = 3, label = "M", color="darkgreen", size=3, fontface =2) + annotate("text", x = 49, y = 3, label = "E", color="darkgreen", size=3, fontface =2) + annotate("text", x = 37, y = 3, label = "B", color="darkgreen", size=3, fontface =2) + annotate("text", x = 25, y = 3, label = "L", color="darkgreen", size=3, fontface =2)  + annotate("text", x = 73, y = 3, label = "C", color="darkgreen", size=3, fontface =2) + annotate("text", x = 86, y = 3, label = "I", color="darkgreen", size=3, fontface =2)
dev.off()

m1<-lm(biden.id.mean ~ postelection + postcall + postmichigan + postbarr  + postelectoralcollege + postinsurrection +  postinauguration, data = data.dem, weights=data.dem$w)
summary(m1)
m2<-lm(trump.id.mean ~ postelection + postcall + postmichigan + postbarr  + postelectoralcollege + postinsurrection +  postinauguration, data = data.rep, weights=data.rep$w)
summary(m2)

m1tot<-lm(biden.id.mean ~ postelection, data = data.dem, weights=data.dem$w)
summary(m1tot)
m2tot<-lm(trump.id.mean ~ postelection, data = data.rep, weights=data.rep$w)
summary(m2tot)

m2tot<-lm(trump.id.mean ~ postcall + biden.win + biden.win*postcall, data = data.rep, weights=data.rep$w)
summary(m2tot)

m1tot<-lm(selfesteem ~ postelection, data = data.rep, weights=data.rep$w)
summary(m1tot)

summary(data.rep$trump.id.mean, na.rm=TRUE)

sd(data.rep$trump.id.mean, na.rm=TRUE)


estimates.trump.id.mean.rep.sub<-subset(estimates.trump.id.mean.rep.sub, is.na(estimates.trump.id.mean.rep.sub$mean)==FALSE)

estimates.trump.id.mean.rep.sub<-subset(estimates.trump.id.mean.rep.sub, estimates.trump.id.mean.rep.sub$date.num!=26)
estimates.trump.id.mean.rep.sub<-subset(estimates.trump.id.mean.rep.sub, estimates.trump.id.mean.rep.sub$date.num!=27)
estimates.trump.id.mean.rep.sub<-subset(estimates.trump.id.mean.rep.sub, estimates.trump.id.mean.rep.sub$date.num!=28)
estimates.trump.id.mean.rep.sub<-subset(estimates.trump.id.mean.rep.sub, estimates.trump.id.mean.rep.sub$date.num!=30)
estimates.trump.id.mean.rep.sub<-subset(estimates.trump.id.mean.rep.sub, estimates.trump.id.mean.rep.sub$date.num!=31)
estimates.trump.id.mean.rep.sub<-subset(estimates.trump.id.mean.rep.sub, estimates.trump.id.mean.rep.sub$date.num!=32)
estimates.trump.id.mean.rep.sub<-subset(estimates.trump.id.mean.rep.sub, estimates.trump.id.mean.rep.sub$date.num!=33)
estimates.trump.id.mean.rep.sub<-subset(estimates.trump.id.mean.rep.sub, estimates.trump.id.mean.rep.sub$date.num!=34)
estimates.trump.id.mean.rep.sub<-subset(estimates.trump.id.mean.rep.sub, estimates.trump.id.mean.rep.sub$date.num!=36)
estimates.trump.id.mean.rep.sub<-subset(estimates.trump.id.mean.rep.sub, estimates.trump.id.mean.rep.sub$date.num!=37)
estimates.trump.id.mean.rep.sub<-subset(estimates.trump.id.mean.rep.sub, estimates.trump.id.mean.rep.sub$date.num!=38)
estimates.trump.id.mean.rep.sub<-subset(estimates.trump.id.mean.rep.sub, estimates.trump.id.mean.rep.sub$date.num!=40)
estimates.trump.id.mean.rep.sub<-subset(estimates.trump.id.mean.rep.sub, estimates.trump.id.mean.rep.sub$date.num!=41)
estimates.trump.id.mean.rep.sub<-subset(estimates.trump.id.mean.rep.sub, estimates.trump.id.mean.rep.sub$date.num!=42)
estimates.trump.id.mean.rep.sub<-subset(estimates.trump.id.mean.rep.sub, estimates.trump.id.mean.rep.sub$date.num!=44)
estimates.trump.id.mean.rep.sub<-subset(estimates.trump.id.mean.rep.sub, estimates.trump.id.mean.rep.sub$date.num!=45)
estimates.trump.id.mean.rep.sub<-subset(estimates.trump.id.mean.rep.sub, estimates.trump.id.mean.rep.sub$date.num!=47)
estimates.trump.id.mean.rep.sub<-subset(estimates.trump.id.mean.rep.sub, estimates.trump.id.mean.rep.sub$date.num!=48)
estimates.trump.id.mean.rep.sub<-subset(estimates.trump.id.mean.rep.sub, estimates.trump.id.mean.rep.sub$date.num!=49)
estimates.trump.id.mean.rep.sub<-subset(estimates.trump.id.mean.rep.sub, estimates.trump.id.mean.rep.sub$date.num!=51)
estimates.trump.id.mean.rep.sub<-subset(estimates.trump.id.mean.rep.sub, estimates.trump.id.mean.rep.sub$date.num!=52)
estimates.trump.id.mean.rep.sub<-subset(estimates.trump.id.mean.rep.sub, estimates.trump.id.mean.rep.sub$date.num!=54)
estimates.trump.id.mean.rep.sub<-subset(estimates.trump.id.mean.rep.sub, estimates.trump.id.mean.rep.sub$date.num!=55)
estimates.trump.id.mean.rep.sub<-subset(estimates.trump.id.mean.rep.sub, estimates.trump.id.mean.rep.sub$date.num!=56)
estimates.trump.id.mean.rep.sub<-subset(estimates.trump.id.mean.rep.sub, estimates.trump.id.mean.rep.sub$date.num!=58)
estimates.trump.id.mean.rep.sub<-subset(estimates.trump.id.mean.rep.sub, estimates.trump.id.mean.rep.sub$date.num!=59)
estimates.trump.id.mean.rep.sub<-subset(estimates.trump.id.mean.rep.sub, estimates.trump.id.mean.rep.sub$date.num!=60)
estimates.trump.id.mean.rep.sub<-subset(estimates.trump.id.mean.rep.sub, estimates.trump.id.mean.rep.sub$date.num!=61)
estimates.trump.id.mean.rep.sub<-subset(estimates.trump.id.mean.rep.sub, estimates.trump.id.mean.rep.sub$date.num!=62)
estimates.trump.id.mean.rep.sub<-subset(estimates.trump.id.mean.rep.sub, estimates.trump.id.mean.rep.sub$date.num!=63)
estimates.trump.id.mean.rep.sub<-subset(estimates.trump.id.mean.rep.sub, estimates.trump.id.mean.rep.sub$date.num!=64)
estimates.trump.id.mean.rep.sub<-subset(estimates.trump.id.mean.rep.sub, estimates.trump.id.mean.rep.sub$date.num!=65)
estimates.trump.id.mean.rep.sub<-subset(estimates.trump.id.mean.rep.sub, estimates.trump.id.mean.rep.sub$date.num!=66)
estimates.trump.id.mean.rep.sub<-subset(estimates.trump.id.mean.rep.sub, estimates.trump.id.mean.rep.sub$date.num!=67)
estimates.trump.id.mean.rep.sub<-subset(estimates.trump.id.mean.rep.sub, estimates.trump.id.mean.rep.sub$date.num!=68)
estimates.trump.id.mean.rep.sub<-subset(estimates.trump.id.mean.rep.sub, estimates.trump.id.mean.rep.sub$date.num!=69)
estimates.trump.id.mean.rep.sub<-subset(estimates.trump.id.mean.rep.sub, estimates.trump.id.mean.rep.sub$date.num!=70)
estimates.trump.id.mean.rep.sub<-subset(estimates.trump.id.mean.rep.sub, estimates.trump.id.mean.rep.sub$date.num!=71)
estimates.trump.id.mean.rep.sub<-subset(estimates.trump.id.mean.rep.sub, estimates.trump.id.mean.rep.sub$date.num!=72)
estimates.trump.id.mean.rep.sub<-subset(estimates.trump.id.mean.rep.sub, estimates.trump.id.mean.rep.sub$date.num!=73)
estimates.trump.id.mean.rep.sub<-subset(estimates.trump.id.mean.rep.sub, estimates.trump.id.mean.rep.sub$date.num!=75)
estimates.trump.id.mean.rep.sub<-subset(estimates.trump.id.mean.rep.sub, estimates.trump.id.mean.rep.sub$date.num!=76)
estimates.trump.id.mean.rep.sub<-subset(estimates.trump.id.mean.rep.sub, estimates.trump.id.mean.rep.sub$date.num!=77)
estimates.trump.id.mean.rep.sub<-subset(estimates.trump.id.mean.rep.sub, estimates.trump.id.mean.rep.sub$date.num!=79)
estimates.trump.id.mean.rep.sub<-subset(estimates.trump.id.mean.rep.sub, estimates.trump.id.mean.rep.sub$date.num!=80)
estimates.trump.id.mean.rep.sub<-subset(estimates.trump.id.mean.rep.sub, estimates.trump.id.mean.rep.sub$date.num!=82)
estimates.trump.id.mean.rep.sub<-subset(estimates.trump.id.mean.rep.sub, estimates.trump.id.mean.rep.sub$date.num!=83)
estimates.trump.id.mean.rep.sub<-subset(estimates.trump.id.mean.rep.sub, estimates.trump.id.mean.rep.sub$date.num!=84)
estimates.trump.id.mean.rep.sub<-subset(estimates.trump.id.mean.rep.sub, estimates.trump.id.mean.rep.sub$date.num!=86)
estimates.trump.id.mean.rep.sub<-subset(estimates.trump.id.mean.rep.sub, estimates.trump.id.mean.rep.sub$date.num!=87)
estimates.trump.id.mean.rep.sub<-subset(estimates.trump.id.mean.rep.sub, estimates.trump.id.mean.rep.sub$date.num!=89)
estimates.trump.id.mean.rep.sub<-subset(estimates.trump.id.mean.rep.sub, estimates.trump.id.mean.rep.sub$date.num!=90)
estimates.trump.id.mean.rep.sub<-subset(estimates.trump.id.mean.rep.sub, estimates.trump.id.mean.rep.sub$date.num!=91)
estimates.trump.id.mean.rep.sub<-subset(estimates.trump.id.mean.rep.sub, estimates.trump.id.mean.rep.sub$date.num!=93)
estimates.trump.id.mean.rep.sub<-subset(estimates.trump.id.mean.rep.sub, estimates.trump.id.mean.rep.sub$date.num!=94)

estimates.trump.id.mean.rep.sub$date[estimates.trump.id.mean.rep.sub$date=="10-26"]<-"."
estimates.trump.id.mean.rep.sub$date[estimates.trump.id.mean.rep.sub$date=="10-27"]<-"."
estimates.trump.id.mean.rep.sub$date[estimates.trump.id.mean.rep.sub$date=="10-29"]<-"."
estimates.trump.id.mean.rep.sub$date[estimates.trump.id.mean.rep.sub$date=="10-30"]<-"."
estimates.trump.id.mean.rep.sub$date[estimates.trump.id.mean.rep.sub$date=="11-01"]<-"."
estimates.trump.id.mean.rep.sub$date[estimates.trump.id.mean.rep.sub$date=="11-02"]<-"."
estimates.trump.id.mean.rep.sub$date[estimates.trump.id.mean.rep.sub$date=="11-04"]<-"."
estimates.trump.id.mean.rep.sub$date[estimates.trump.id.mean.rep.sub$date=="11-05"]<-"."
estimates.trump.id.mean.rep.sub$date[estimates.trump.id.mean.rep.sub$date=="11-07"]<-"."
estimates.trump.id.mean.rep.sub$date[estimates.trump.id.mean.rep.sub$date=="11-08"]<-"."
estimates.trump.id.mean.rep.sub$date[estimates.trump.id.mean.rep.sub$date=="11-10"]<-"."
estimates.trump.id.mean.rep.sub$date[estimates.trump.id.mean.rep.sub$date=="11-11"]<-"."
estimates.trump.id.mean.rep.sub$date[estimates.trump.id.mean.rep.sub$date=="11-13"]<-"."
estimates.trump.id.mean.rep.sub$date[estimates.trump.id.mean.rep.sub$date=="11-14"]<-"."
estimates.trump.id.mean.rep.sub$date[estimates.trump.id.mean.rep.sub$date=="11-16"]<-"."
estimates.trump.id.mean.rep.sub$date[estimates.trump.id.mean.rep.sub$date=="11-17"]<-"."
estimates.trump.id.mean.rep.sub$date[estimates.trump.id.mean.rep.sub$date=="11-19"]<-"."
estimates.trump.id.mean.rep.sub$date[estimates.trump.id.mean.rep.sub$date=="11-20"]<-"."

pdf('fig-trumpidsub.pdf', width=8.25, height=4)
ggplot(estimates.trump.id.mean.rep.sub, aes(x=date.num, y=mean, colour=party)) + geom_errorbar(aes(ymin=l95ci, ymax=u95ci), width=0, position=pd,  alpha=.5) +geom_line(position=pd, alpha=.5) +geom_point(position=pd, alpha=.5) + geom_line(aes(group = party), position=pd,alpha=.5) + theme_bw() + xlab("Date") + ylab("Trump Identification (mean)") + ylim(c(2.5,4.75)) + guides(color=guide_legend(title=NULL)) + ggtitle("Outcome: Trump Identification, Sample Republicans") + scale_color_manual(values=c("red4","red1")) + theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + facet_grid(. ~ party) + theme(plot.subtitle=element_text(size=10, face="italic", color="black")) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=7)) + facet_grid(. ~ party) + scale_x_continuous(breaks=estimates.trump.id.mean.rep.sub$date.num, labels=estimates.trump.id.mean.rep.sub$date) + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + theme(plot.title = element_text(size = 11))  + theme(axis.title.x=element_blank()) + theme(plot.title = element_text(size = 11))  + theme(axis.title.x=element_blank()) + annotate("text", x = 8, y = 2.8, label = "D", color="darkgreen", size=3, fontface =2) + annotate("text", x = 12, y =2.8, label = "M", color="darkgreen", size=3, fontface =2) + annotate("text", x = 49, y = 2.8, label = "E", color="darkgreen", size=3, fontface =2) + annotate("text", x = 37, y = 2.8, label = "B", color="darkgreen", size=3, fontface =2) + annotate("text", x = 25, y = 2.8, label = "L", color="darkgreen", size=3, fontface =2)  + annotate("text", x = 73, y = 2.8, label = "C", color="darkgreen", size=3.25, fontface =2)  + annotate("text", x = 86, y = 2.8, label = "I", color="darkgreen", size=3, fontface =2)
dev.off()


#Self esteem

estimates.selfesteem<-subset(estimates.selfesteem, is.na(estimates.selfesteem$mean)==FALSE)

estimates.selfesteem<-subset(estimates.selfesteem, estimates.selfesteem$date.num!=26)
estimates.selfesteem<-subset(estimates.selfesteem, estimates.selfesteem$date.num!=27)
estimates.selfesteem<-subset(estimates.selfesteem, estimates.selfesteem$date.num!=28)
estimates.selfesteem<-subset(estimates.selfesteem, estimates.selfesteem$date.num!=30)
estimates.selfesteem<-subset(estimates.selfesteem, estimates.selfesteem$date.num!=31)
estimates.selfesteem<-subset(estimates.selfesteem, estimates.selfesteem$date.num!=32)
estimates.selfesteem<-subset(estimates.selfesteem, estimates.selfesteem$date.num!=33)
estimates.selfesteem<-subset(estimates.selfesteem, estimates.selfesteem$date.num!=34)
estimates.selfesteem<-subset(estimates.selfesteem, estimates.selfesteem$date.num!=36)
estimates.selfesteem<-subset(estimates.selfesteem, estimates.selfesteem$date.num!=37)
estimates.selfesteem<-subset(estimates.selfesteem, estimates.selfesteem$date.num!=38)
estimates.selfesteem<-subset(estimates.selfesteem, estimates.selfesteem$date.num!=40)
estimates.selfesteem<-subset(estimates.selfesteem, estimates.selfesteem$date.num!=41)
estimates.selfesteem<-subset(estimates.selfesteem, estimates.selfesteem$date.num!=42)
estimates.selfesteem<-subset(estimates.selfesteem, estimates.selfesteem$date.num!=44)
estimates.selfesteem<-subset(estimates.selfesteem, estimates.selfesteem$date.num!=45)
estimates.selfesteem<-subset(estimates.selfesteem, estimates.selfesteem$date.num!=47)
estimates.selfesteem<-subset(estimates.selfesteem, estimates.selfesteem$date.num!=48)
estimates.selfesteem<-subset(estimates.selfesteem, estimates.selfesteem$date.num!=49)
estimates.selfesteem<-subset(estimates.selfesteem, estimates.selfesteem$date.num!=51)
estimates.selfesteem<-subset(estimates.selfesteem, estimates.selfesteem$date.num!=52)
estimates.selfesteem<-subset(estimates.selfesteem, estimates.selfesteem$date.num!=54)
estimates.selfesteem<-subset(estimates.selfesteem, estimates.selfesteem$date.num!=55)
estimates.selfesteem<-subset(estimates.selfesteem, estimates.selfesteem$date.num!=56)
estimates.selfesteem<-subset(estimates.selfesteem, estimates.selfesteem$date.num!=58)
estimates.selfesteem<-subset(estimates.selfesteem, estimates.selfesteem$date.num!=59)
estimates.selfesteem<-subset(estimates.selfesteem, estimates.selfesteem$date.num!=60)
estimates.selfesteem<-subset(estimates.selfesteem, estimates.selfesteem$date.num!=61)
estimates.selfesteem<-subset(estimates.selfesteem, estimates.selfesteem$date.num!=62)
estimates.selfesteem<-subset(estimates.selfesteem, estimates.selfesteem$date.num!=63)
estimates.selfesteem<-subset(estimates.selfesteem, estimates.selfesteem$date.num!=64)
estimates.selfesteem<-subset(estimates.selfesteem, estimates.selfesteem$date.num!=65)
estimates.selfesteem<-subset(estimates.selfesteem, estimates.selfesteem$date.num!=66)
estimates.selfesteem<-subset(estimates.selfesteem, estimates.selfesteem$date.num!=67)
estimates.selfesteem<-subset(estimates.selfesteem, estimates.selfesteem$date.num!=68)
estimates.selfesteem<-subset(estimates.selfesteem, estimates.selfesteem$date.num!=69)
estimates.selfesteem<-subset(estimates.selfesteem, estimates.selfesteem$date.num!=70)
estimates.selfesteem<-subset(estimates.selfesteem, estimates.selfesteem$date.num!=71)
estimates.selfesteem<-subset(estimates.selfesteem, estimates.selfesteem$date.num!=72)
estimates.selfesteem<-subset(estimates.selfesteem, estimates.selfesteem$date.num!=73)
estimates.selfesteem<-subset(estimates.selfesteem, estimates.selfesteem$date.num!=75)
estimates.selfesteem<-subset(estimates.selfesteem, estimates.selfesteem$date.num!=76)
estimates.selfesteem<-subset(estimates.selfesteem, estimates.selfesteem$date.num!=77)
estimates.selfesteem<-subset(estimates.selfesteem, estimates.selfesteem$date.num!=79)
estimates.selfesteem<-subset(estimates.selfesteem, estimates.selfesteem$date.num!=80)
estimates.selfesteem<-subset(estimates.selfesteem, estimates.selfesteem$date.num!=82)
estimates.selfesteem<-subset(estimates.selfesteem, estimates.selfesteem$date.num!=83)
estimates.selfesteem<-subset(estimates.selfesteem, estimates.selfesteem$date.num!=84)
estimates.selfesteem<-subset(estimates.selfesteem, estimates.selfesteem$date.num!=86)
estimates.selfesteem<-subset(estimates.selfesteem, estimates.selfesteem$date.num!=87)
estimates.selfesteem<-subset(estimates.selfesteem, estimates.selfesteem$date.num!=89)
estimates.selfesteem<-subset(estimates.selfesteem, estimates.selfesteem$date.num!=90)
estimates.selfesteem<-subset(estimates.selfesteem, estimates.selfesteem$date.num!=91)
estimates.selfesteem<-subset(estimates.selfesteem, estimates.selfesteem$date.num!=93)
estimates.selfesteem<-subset(estimates.selfesteem, estimates.selfesteem$date.num!=94)

estimates.selfesteem$date[estimates.selfesteem$date=="10-26"]<-"."
estimates.selfesteem$date[estimates.selfesteem$date=="10-27"]<-"."
estimates.selfesteem$date[estimates.selfesteem$date=="10-29"]<-"."
estimates.selfesteem$date[estimates.selfesteem$date=="10-30"]<-"."
estimates.selfesteem$date[estimates.selfesteem$date=="11-01"]<-"."
estimates.selfesteem$date[estimates.selfesteem$date=="11-02"]<-"."
estimates.selfesteem$date[estimates.selfesteem$date=="11-04"]<-"."
estimates.selfesteem$date[estimates.selfesteem$date=="11-05"]<-"."
estimates.selfesteem$date[estimates.selfesteem$date=="11-07"]<-"."
estimates.selfesteem$date[estimates.selfesteem$date=="11-08"]<-"."
estimates.selfesteem$date[estimates.selfesteem$date=="11-10"]<-"."
estimates.selfesteem$date[estimates.selfesteem$date=="11-11"]<-"."
estimates.selfesteem$date[estimates.selfesteem$date=="11-13"]<-"."
estimates.selfesteem$date[estimates.selfesteem$date=="11-14"]<-"."
estimates.selfesteem$date[estimates.selfesteem$date=="11-16"]<-"."
estimates.selfesteem$date[estimates.selfesteem$date=="11-17"]<-"."
estimates.selfesteem$date[estimates.selfesteem$date=="11-19"]<-"."
estimates.selfesteem$date[estimates.selfesteem$date=="11-20"]<-"."

pdf('fig-selfesteem.pdf', width=8.25, height=4)
ggplot(estimates.selfesteem, aes(x=date.num, y=mean, colour=party)) + geom_errorbar(aes(ymin=l95ci, ymax=u95ci), width=0, position=pd,  alpha=.5) +geom_line(position=pd, alpha=.5) +geom_point(position=pd, alpha=.5) + geom_line(aes(group = party), position=pd,alpha=.5) + theme_bw() + xlab("Date") + ylab("Self Esteem (mean)") + ylim(c(3.25,4.3)) + guides(color=guide_legend(title=NULL)) + ggtitle("Outcome: Self Esteem") + scale_color_manual(values=c("dodgerblue3","palevioletred4","red1")) + theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + facet_grid(. ~ party) + theme(plot.subtitle=element_text(size=10, face="italic", color="black")) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=7)) + facet_grid(. ~ party) + scale_x_continuous(breaks=estimates.selfesteem$date.num, labels=estimates.selfesteem$date) + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + theme(plot.title = element_text(size = 11))  + theme(axis.title.x=element_blank()) + theme(plot.title = element_text(size = 11))  + theme(axis.title.x=element_blank()) + annotate("text", x = 8, y = 3.25, label = "D", color="darkgreen", size=3, fontface =2) + annotate("text", x = 12, y = 3.25, label = "M", color="darkgreen", size=3, fontface =2) + annotate("text", x = 49, y = 3.25, label = "E", color="darkgreen", size=3, fontface =2) + annotate("text", x = 37, y = 3.25, label = "B", color="darkgreen", size=3, fontface =2) + annotate("text", x = 25, y = 3.25, label = "L", color="darkgreen", size=3, fontface =2)  + annotate("text", x = 73, y = 3.25, label = "C", color="darkgreen", size=3.25, fontface =2)  + annotate("text", x = 86, y = 3.25, label = "I", color="darkgreen", size=3, fontface =2)
dev.off()

estimates.selfesteem.rep<-subset(estimates.selfesteem.rep, estimates.selfesteem.rep$date.num!=26)
estimates.selfesteem.rep<-subset(estimates.selfesteem.rep, estimates.selfesteem.rep$date.num!=27)
estimates.selfesteem.rep<-subset(estimates.selfesteem.rep, estimates.selfesteem.rep$date.num!=28)
estimates.selfesteem.rep<-subset(estimates.selfesteem.rep, estimates.selfesteem.rep$date.num!=30)
estimates.selfesteem.rep<-subset(estimates.selfesteem.rep, estimates.selfesteem.rep$date.num!=31)
estimates.selfesteem.rep<-subset(estimates.selfesteem.rep, estimates.selfesteem.rep$date.num!=32)
estimates.selfesteem.rep<-subset(estimates.selfesteem.rep, estimates.selfesteem.rep$date.num!=33)
estimates.selfesteem.rep<-subset(estimates.selfesteem.rep, estimates.selfesteem.rep$date.num!=34)
estimates.selfesteem.rep<-subset(estimates.selfesteem.rep, estimates.selfesteem.rep$date.num!=36)
estimates.selfesteem.rep<-subset(estimates.selfesteem.rep, estimates.selfesteem.rep$date.num!=37)
estimates.selfesteem.rep<-subset(estimates.selfesteem.rep, estimates.selfesteem.rep$date.num!=38)
estimates.selfesteem.rep<-subset(estimates.selfesteem.rep, estimates.selfesteem.rep$date.num!=40)
estimates.selfesteem.rep<-subset(estimates.selfesteem.rep, estimates.selfesteem.rep$date.num!=41)
estimates.selfesteem.rep<-subset(estimates.selfesteem.rep, estimates.selfesteem.rep$date.num!=42)
estimates.selfesteem.rep<-subset(estimates.selfesteem.rep, estimates.selfesteem.rep$date.num!=44)
estimates.selfesteem.rep<-subset(estimates.selfesteem.rep, estimates.selfesteem.rep$date.num!=45)
estimates.selfesteem.rep<-subset(estimates.selfesteem.rep, estimates.selfesteem.rep$date.num!=47)
estimates.selfesteem.rep<-subset(estimates.selfesteem.rep, estimates.selfesteem.rep$date.num!=48)
estimates.selfesteem.rep<-subset(estimates.selfesteem.rep, estimates.selfesteem.rep$date.num!=49)
estimates.selfesteem.rep<-subset(estimates.selfesteem.rep, estimates.selfesteem.rep$date.num!=51)
estimates.selfesteem.rep<-subset(estimates.selfesteem.rep, estimates.selfesteem.rep$date.num!=52)
estimates.selfesteem.rep<-subset(estimates.selfesteem.rep, estimates.selfesteem.rep$date.num!=54)
estimates.selfesteem.rep<-subset(estimates.selfesteem.rep, estimates.selfesteem.rep$date.num!=55)
estimates.selfesteem.rep<-subset(estimates.selfesteem.rep, estimates.selfesteem.rep$date.num!=56)
estimates.selfesteem.rep<-subset(estimates.selfesteem.rep, estimates.selfesteem.rep$date.num!=58)
estimates.selfesteem.rep<-subset(estimates.selfesteem.rep, estimates.selfesteem.rep$date.num!=59)
estimates.selfesteem.rep<-subset(estimates.selfesteem.rep, estimates.selfesteem.rep$date.num!=60)
estimates.selfesteem.rep<-subset(estimates.selfesteem.rep, estimates.selfesteem.rep$date.num!=61)
estimates.selfesteem.rep<-subset(estimates.selfesteem.rep, estimates.selfesteem.rep$date.num!=62)
estimates.selfesteem.rep<-subset(estimates.selfesteem.rep, estimates.selfesteem.rep$date.num!=63)
estimates.selfesteem.rep<-subset(estimates.selfesteem.rep, estimates.selfesteem.rep$date.num!=64)
estimates.selfesteem.rep<-subset(estimates.selfesteem.rep, estimates.selfesteem.rep$date.num!=65)
estimates.selfesteem.rep<-subset(estimates.selfesteem.rep, estimates.selfesteem.rep$date.num!=66)
estimates.selfesteem.rep<-subset(estimates.selfesteem.rep, estimates.selfesteem.rep$date.num!=67)
estimates.selfesteem.rep<-subset(estimates.selfesteem.rep, estimates.selfesteem.rep$date.num!=68)
estimates.selfesteem.rep<-subset(estimates.selfesteem.rep, estimates.selfesteem.rep$date.num!=69)
estimates.selfesteem.rep<-subset(estimates.selfesteem.rep, estimates.selfesteem.rep$date.num!=70)
estimates.selfesteem.rep<-subset(estimates.selfesteem.rep, estimates.selfesteem.rep$date.num!=71)
estimates.selfesteem.rep<-subset(estimates.selfesteem.rep, estimates.selfesteem.rep$date.num!=72)
estimates.selfesteem.rep<-subset(estimates.selfesteem.rep, estimates.selfesteem.rep$date.num!=73)
estimates.selfesteem.rep<-subset(estimates.selfesteem.rep, estimates.selfesteem.rep$date.num!=75)
estimates.selfesteem.rep<-subset(estimates.selfesteem.rep, estimates.selfesteem.rep$date.num!=76)
estimates.selfesteem.rep<-subset(estimates.selfesteem.rep, estimates.selfesteem.rep$date.num!=77)
estimates.selfesteem.rep<-subset(estimates.selfesteem.rep, estimates.selfesteem.rep$date.num!=79)
estimates.selfesteem.rep<-subset(estimates.selfesteem.rep, estimates.selfesteem.rep$date.num!=80)
estimates.selfesteem.rep<-subset(estimates.selfesteem.rep, estimates.selfesteem.rep$date.num!=82)
estimates.selfesteem.rep<-subset(estimates.selfesteem.rep, estimates.selfesteem.rep$date.num!=83)
estimates.selfesteem.rep<-subset(estimates.selfesteem.rep, estimates.selfesteem.rep$date.num!=84)
estimates.selfesteem.rep<-subset(estimates.selfesteem.rep, estimates.selfesteem.rep$date.num!=86)
estimates.selfesteem.rep<-subset(estimates.selfesteem.rep, estimates.selfesteem.rep$date.num!=87)
estimates.selfesteem.rep<-subset(estimates.selfesteem.rep, estimates.selfesteem.rep$date.num!=89)
estimates.selfesteem.rep<-subset(estimates.selfesteem.rep, estimates.selfesteem.rep$date.num!=90)
estimates.selfesteem.rep<-subset(estimates.selfesteem.rep, estimates.selfesteem.rep$date.num!=91)
estimates.selfesteem.rep<-subset(estimates.selfesteem.rep, estimates.selfesteem.rep$date.num!=93)
estimates.selfesteem.rep<-subset(estimates.selfesteem.rep, estimates.selfesteem.rep$date.num!=94)

estimates.selfesteem.rep$l95ci<-estimates.selfesteem.rep$mean-1.96*estimates.selfesteem.rep$se
estimates.selfesteem.rep$u95ci<-estimates.selfesteem.rep$mean+1.96*estimates.selfesteem.rep$se
estimates.selfesteem.rep$weights<-"Weighted"

estimates.selfesteem.rep$date[estimates.selfesteem.rep$date=="11-05"]<-"."
estimates.selfesteem.rep$date[estimates.selfesteem.rep$date=="11-07"]<-"."
estimates.selfesteem.rep$date[estimates.selfesteem.rep$date=="11-09"]<-"."
estimates.selfesteem.rep$date[estimates.selfesteem.rep$date=="11-11"]<-"."
estimates.selfesteem.rep$date[estimates.selfesteem.rep$date=="11-13"]<-"."
estimates.selfesteem.rep$date[estimates.selfesteem.rep$date=="11-15"]<-"."
estimates.selfesteem.rep$date[estimates.selfesteem.rep$date=="11-17"]<-"."
estimates.selfesteem.rep$date[estimates.selfesteem.rep$date=="11-19"]<-"."

pdf('fig-selfesteem.rep.pdf', width=7.25, height=3.5)
ggplot(estimates.selfesteem.rep, aes(x=date.num, y=mean, colour="red1")) + geom_errorbar(aes(ymin=l95ci, ymax=u95ci), width=0, position=pd,  alpha=.5, colour="red1") +geom_line(position=pd, alpha=.5, colour="red1") +geom_point(position=pd, alpha=.5, colour="red1") + geom_line(aes(group = party), position=pd,alpha=.5, colour="red1") + theme_bw() + xlab("Date") + ylab("Self Esteem (mean)") + ylim(c(3.25,4.3)) + guides(color=guide_legend(title=NULL)) + ggtitle("Outcome: Self Esteem, Sample: Republicans")+ theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  + theme(plot.subtitle=element_text(size=10, face="italic", color="black")) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=7))  + scale_x_continuous(breaks=estimates.selfesteem.rep$date.num, labels=estimates.selfesteem.rep$date) + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + theme(plot.title = element_text(size = 11))  + theme(axis.title.x=element_blank()) + theme(plot.title = element_text(size = 11))  + theme(axis.title.x=element_blank()) + annotate("text", x = 8, y = 3.25, label = "D", color="darkgreen", size=3, fontface =2) + annotate("text", x = 12, y = 3.25, label = "M", color="darkgreen", size=3, fontface =2) + annotate("text", x = 49, y = 3.25, label = "E", color="darkgreen", size=3, fontface =2) + annotate("text", x = 37, y = 3.25, label = "B", color="darkgreen", size=3, fontface =2) + annotate("text", x = 25, y = 3.25, label = "L", color="darkgreen", size=3, fontface =2)  + annotate("text", x = 73, y = 3.25, label = "C", color="darkgreen", size=3.25, fontface =2)  + annotate("text", x = 86, y = 3.25, label = "I", color="darkgreen", size=3, fontface =2)
dev.off()



####UNWEIGHTED DISAGGREGATED ANALYSIS####

estimates.dem<-matrix(data = NA, nrow=95, ncol=3)
estimates.dem<-data.frame(estimates.dem)
colnames(estimates.dem)<-c("date.num","date", "party")
estimates.dem$party<-"Democrat"
estimates.dem$date.num<-seq(1:95)
estimates.dem$date<-c("10-27","10-28","10-29","10-30","10-31","11-01","11-02","11-03","11-04","11-05","11-06","11-07","11-08","11-09","11-10","11-11","11-12","11-13","11-14","11-15","11-16","11-17","11-18","11-19","11-20","11-21","11-22","11-23","11-24","11-25","11-26","11-27","11-28","11-29","11-30","12-01","12-02","12-03","12-04","12-05","12-06","12-07","12-08","12-09","12-10","12-11","12-12","12-13","12-14","12-15","12-16","12-17","12-18","12-19","12-20","12-21","12-22","12-23","12-24","12-25","12-26","12-27","12-28","12-29","12-30","12-31","01-01","01-02","01-03","01-04","01-05","01-06","01-07","01-08","01-09","01-10","01-11","01-12","01-13","01-14","01-15","01-16","01-17","01-18","01-19","01-20","01-21","01-22","01-23","01-24","01-25","01-26","01-27","01-28","01-29")

estimates.rep<-estimates.dem
estimates.rep$party<-"Republican"

estimates.ind<-estimates.dem
estimates.ind$party<-"Independent/Other"

estimates.pooled<-estimates.dem
estimates.pooled$party<-"Pooled"

outcome.list<-c("violence","trump.id.mean","biden.id.mean","polarization","rep.therm","dem.therm","selfesteem","biden.win","legitimate")
outcome.list.1<-c("violence","trump.id.mean","biden.id.mean","polarization","rep.therm","dem.therm","selfesteem")
outcome.list.2<-c("biden.win","legitimate")

data.list<-c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","29","36","39","43","46","50","53","57","74","78","81","85")

for (i in outcome.list){
  try(eval(parse(text=paste("estimates.",i,".dem<-estimates.dem",sep=""))))
  try(eval(parse(text=paste("estimates.",i,".rep<-estimates.rep",sep=""))))
  try(eval(parse(text=paste("estimates.",i,".ind<-estimates.ind",sep=""))))
  try(eval(parse(text=paste("estimates.",i,".pooled<-estimates.pooled",sep=""))))
}

for (i in outcome.list.1){
  for (j in 1:95) {
    try(eval(parse(text=paste("m",j,"<-lm(",i," ~ -1 + party, data = data.",j,")",sep=""))))
    try(eval(parse(text=paste("estimates.",i,".dem$mean[",j,"]<-summary(m",j,")$coefficients[1,1]",sep=""))))
    try(eval(parse(text=paste("estimates.",i,".dem$se[",j,"]<-summary(m",j,")$coefficients[1,2]",sep=""))))
    try(eval(parse(text=paste("estimates.",i,".rep$mean[",j,"]<-summary(m",j,")$coefficients[3,1]",sep=""))))
    try(eval(parse(text=paste("estimates.",i,".rep$se[",j,"]<-summary(m",j,")$coefficients[3,2]",sep=""))))
    try(eval(parse(text=paste("estimates.",i,".ind$mean[",j,"]<-summary(m",j,")$coefficients[2,1]",sep=""))))
    try(eval(parse(text=paste("estimates.",i,".ind$se[",j,"]<-summary(m",j,")$coefficients[2,2]",sep=""))))
    try(eval(parse(text=paste("estimates.",i,"<-rbind(estimates.",i,".dem,estimates.",i,".rep,estimates.",i,".ind)",sep=""))))
    try(eval(parse(text=paste("estimates.",i,"$l95ci<-estimates.",i,"$mean-1.96*estimates.",i,"$se",sep=""))))
    try(eval(parse(text=paste("estimates.",i,"$u95ci<-estimates.",i,"$mean+1.96*estimates.",i,"$se",sep=""))))
    try(eval(parse(text=paste("estimates.",i,"$outcome<-'",i,"'",sep=""))))
    try(eval(parse(text=paste("rm(m",j,")",sep=""))))
  }
}

for (i in outcome.list.1){
  for (j in 1:95) {
    try(eval(parse(text=paste("m",j,"<-lm(",i," ~ -1 + party, data = data.",j,")",sep=""))))
    try(eval(parse(text=paste("estimates.",i,".dem$mean[",j,"]<-summary(m",j,")$coefficients[1,1]",sep=""))))
    try(eval(parse(text=paste("estimates.",i,".dem$se[",j,"]<-summary(m",j,")$coefficients[1,2]",sep=""))))
    try(eval(parse(text=paste("estimates.",i,".rep$mean[",j,"]<-summary(m",j,")$coefficients[3,1]",sep=""))))
    try(eval(parse(text=paste("estimates.",i,".rep$se[",j,"]<-summary(m",j,")$coefficients[3,2]",sep=""))))
    try(eval(parse(text=paste("estimates.",i,".ind$mean[",j,"]<-summary(m",j,")$coefficients[2,1]",sep=""))))
    try(eval(parse(text=paste("estimates.",i,".ind$se[",j,"]<-summary(m",j,")$coefficients[2,2]",sep=""))))
    try(eval(parse(text=paste("estimates.",i,"<-rbind(estimates.",i,".dem,estimates.",i,".rep,estimates.",i,".ind)",sep=""))))
    try(eval(parse(text=paste("estimates.",i,"$l95ci<-estimates.",i,"$mean-1.96*estimates.",i,"$se",sep=""))))
    try(eval(parse(text=paste("estimates.",i,"$u95ci<-estimates.",i,"$mean+1.96*estimates.",i,"$se",sep=""))))
    try(eval(parse(text=paste("estimates.",i,"$outcome<-'",i,"'",sep=""))))
    try(eval(parse(text=paste("rm(m",j,")",sep=""))))
  }
}


for (i in outcome.list.2){
  for (j in 9:95) {
    try(eval(parse(text=paste("m",j,"<-lm(",i," ~ -1 + party, data = data.",j,")",sep=""))))
    try(eval(parse(text=paste("estimates.",i,".dem$mean[",j,"]<-summary(m",j,")$coefficients[1,1]",sep=""))))
    try(eval(parse(text=paste("estimates.",i,".dem$se[",j,"]<-summary(m",j,")$coefficients[1,2]",sep=""))))
    try(eval(parse(text=paste("estimates.",i,".rep$mean[",j,"]<-summary(m",j,")$coefficients[3,1]",sep=""))))
    try(eval(parse(text=paste("estimates.",i,".rep$se[",j,"]<-summary(m",j,")$coefficients[3,2]",sep=""))))
    try(eval(parse(text=paste("estimates.",i,".ind$mean[",j,"]<-summary(m",j,")$coefficients[2,1]",sep=""))))
    try(eval(parse(text=paste("estimates.",i,".ind$se[",j,"]<-summary(m",j,")$coefficients[2,2]",sep=""))))
    try(eval(parse(text=paste("estimates.",i,"<-rbind(estimates.",i,".dem,estimates.",i,".rep,estimates.",i,".ind)",sep=""))))
    try(eval(parse(text=paste("estimates.",i,"$l95ci<-estimates.",i,"$mean-1.96*estimates.",i,"$se",sep=""))))
    try(eval(parse(text=paste("estimates.",i,"$u95ci<-estimates.",i,"$mean+1.96*estimates.",i,"$se",sep=""))))
    try(eval(parse(text=paste("estimates.",i,"$outcome<-'",i,"'",sep=""))))
    try(eval(parse(text=paste("rm(m",j,")",sep=""))))
  }
}

for (i in outcome.list.2){
  for (j in 9:95) {
    try(eval(parse(text=paste("m",j,"<-lm(",i," ~ -1 + party, data = data.",j,")",sep=""))))
    try(eval(parse(text=paste("estimates.",i,".dem$mean[",j,"]<-summary(m",j,")$coefficients[1,1]",sep=""))))
    try(eval(parse(text=paste("estimates.",i,".dem$se[",j,"]<-summary(m",j,")$coefficients[1,2]",sep=""))))
    try(eval(parse(text=paste("estimates.",i,".rep$mean[",j,"]<-summary(m",j,")$coefficients[3,1]",sep=""))))
    try(eval(parse(text=paste("estimates.",i,".rep$se[",j,"]<-summary(m",j,")$coefficients[3,2]",sep=""))))
    try(eval(parse(text=paste("estimates.",i,".ind$mean[",j,"]<-summary(m",j,")$coefficients[2,1]",sep=""))))
    try(eval(parse(text=paste("estimates.",i,".ind$se[",j,"]<-summary(m",j,")$coefficients[2,2]",sep=""))))
    try(eval(parse(text=paste("estimates.",i,"<-rbind(estimates.",i,".dem,estimates.",i,".rep,estimates.",i,".ind)",sep=""))))
    try(eval(parse(text=paste("estimates.",i,"$l95ci<-estimates.",i,"$mean-1.96*estimates.",i,"$se",sep=""))))
    try(eval(parse(text=paste("estimates.",i,"$u95ci<-estimates.",i,"$mean+1.96*estimates.",i,"$se",sep=""))))
    try(eval(parse(text=paste("estimates.",i,"$outcome<-'",i,"'",sep=""))))
    try(eval(parse(text=paste("rm(m",j,")",sep=""))))
  }
} 

#Biden Win

estimates.biden.win<-subset(estimates.biden.win, is.na(estimates.biden.win$mean)==FALSE)
estimates.biden.win$u95ci[estimates.biden.win$u95ci>1]<-1

estimates.biden.win$date[estimates.biden.win$date=="10-26"]<-"."
estimates.biden.win$date[estimates.biden.win$date=="10-27"]<-"."
estimates.biden.win$date[estimates.biden.win$date=="10-29"]<-"."
estimates.biden.win$date[estimates.biden.win$date=="10-30"]<-"."
estimates.biden.win$date[estimates.biden.win$date=="11-01"]<-"."
estimates.biden.win$date[estimates.biden.win$date=="11-02"]<-"."
estimates.biden.win$date[estimates.biden.win$date=="11-04"]<-"."
estimates.biden.win$date[estimates.biden.win$date=="11-05"]<-"."
estimates.biden.win$date[estimates.biden.win$date=="11-07"]<-"."
estimates.biden.win$date[estimates.biden.win$date=="11-08"]<-"."
estimates.biden.win$date[estimates.biden.win$date=="11-10"]<-"."
estimates.biden.win$date[estimates.biden.win$date=="11-11"]<-"."
estimates.biden.win$date[estimates.biden.win$date=="11-13"]<-"."
estimates.biden.win$date[estimates.biden.win$date=="11-14"]<-"."
estimates.biden.win$date[estimates.biden.win$date=="11-16"]<-"."
estimates.biden.win$date[estimates.biden.win$date=="11-17"]<-"."
estimates.biden.win$date[estimates.biden.win$date=="11-19"]<-"."
estimates.biden.win$date[estimates.biden.win$date=="11-20"]<-"."

pdf('fig-bidenwin-raw.pdf', width=7.75, height=3.5)
ggplot(estimates.biden.win, aes(x=date.num, y=mean, colour=party)) + geom_errorbar(aes(ymin=l95ci, ymax=u95ci), width=0, position=pd,  alpha=.5) +geom_line(position=pd, alpha=.5) +geom_point(position=pd, alpha=.5) + geom_line(aes(group = party), position=pd,alpha=.5) + theme_bw() + xlab("Date")+ ylab("Proportion Identifying Biden as Winner") + ylim(c(0,1)) + guides(color=guide_legend(title=NULL)) + ggtitle("Outcome: Believes Biden Won") + scale_color_manual(values=c("dodgerblue3","palevioletred4","red2")) + theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=5.5)) + facet_grid(. ~ party) + scale_x_continuous(breaks=estimates.biden.win$date.num, labels=estimates.biden.win$date) + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + theme(plot.title = element_text(size = 11))  + theme(axis.title.x=element_blank()) + annotate("text", x = 8, y = 0, label = "D", color="darkgreen", size=3, fontface =2) + annotate("text", x = 12, y = 0, label = "M", color="darkgreen", size=3, fontface =2) + annotate("text", x = 49, y = 0, label = "E", color="darkgreen", size=3, fontface =2) + annotate("text", x = 37, y = 0, label = "B", color="darkgreen", size=3, fontface =2) + annotate("text", x = 25, y = 0, label = "L", color="darkgreen", size=3, fontface =2)  + annotate("text", x = 73, y = 0, label = "C", color="darkgreen", size=3, fontface =2)  + annotate("text", x = 86, y = 0, label = "I", color="darkgreen", size=3, fontface =2)
dev.off()

#Legitimate

estimates.legitimate<-subset(estimates.legitimate, is.na(estimates.legitimate$mean)==FALSE)
estimates.legitimate$u95ci[estimates.legitimate$u95ci>1]<-1

estimates.legitimate$date[estimates.legitimate$date=="10-26"]<-"."
estimates.legitimate$date[estimates.legitimate$date=="10-27"]<-"."
estimates.legitimate$date[estimates.legitimate$date=="10-29"]<-"."
estimates.legitimate$date[estimates.legitimate$date=="10-30"]<-"."
estimates.legitimate$date[estimates.legitimate$date=="11-01"]<-"."
estimates.legitimate$date[estimates.legitimate$date=="11-02"]<-"."
estimates.legitimate$date[estimates.legitimate$date=="11-04"]<-"."
estimates.legitimate$date[estimates.legitimate$date=="11-05"]<-"."
estimates.legitimate$date[estimates.legitimate$date=="11-07"]<-"."
estimates.legitimate$date[estimates.legitimate$date=="11-08"]<-"."
estimates.legitimate$date[estimates.legitimate$date=="11-10"]<-"."
estimates.legitimate$date[estimates.legitimate$date=="11-11"]<-"."
estimates.legitimate$date[estimates.legitimate$date=="11-13"]<-"."
estimates.legitimate$date[estimates.legitimate$date=="11-14"]<-"."
estimates.legitimate$date[estimates.legitimate$date=="11-16"]<-"."
estimates.legitimate$date[estimates.legitimate$date=="11-17"]<-"."
estimates.legitimate$date[estimates.legitimate$date=="11-19"]<-"."
estimates.legitimate$date[estimates.legitimate$date=="11-20"]<-"."

pdf('fig-legitimate-raw.pdf', width=7.75, height=3.5)
ggplot(estimates.legitimate, aes(x=date.num, y=mean, colour=party)) + geom_errorbar(aes(ymin=l95ci, ymax=u95ci), width=0, position=pd,  alpha=.5) +geom_line(position=pd, alpha=.5) +geom_point(position=pd, alpha=.5) + geom_line(aes(group = party), position=pd,alpha=.5) + theme_bw() + xlab("Date")+ ylab("Proportion Accepting Result as Legitimate") + ylim(c(0,1)) + guides(color=guide_legend(title=NULL)) + ggtitle("Outcome: Election Result Legitimate") + scale_color_manual(values=c("dodgerblue3","palevioletred4","red2")) + theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=5.5)) + facet_grid(. ~ party) + scale_x_continuous(breaks=estimates.legitimate$date.num, labels=estimates.legitimate$date) + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + theme(plot.title = element_text(size = 11))  + theme(axis.title.x=element_blank()) + annotate("text", x = 8, y = 0, label = "D", color="darkgreen", size=3, fontface =2) + annotate("text", x = 12, y = 0, label = "M", color="darkgreen", size=3, fontface =2) + annotate("text", x = 49, y = 0, label = "E", color="darkgreen", size=3, fontface =2) + annotate("text", x = 37, y = 0, label = "B", color="darkgreen", size=3, fontface =2) + annotate("text", x = 25, y = 0, label = "L", color="darkgreen", size=3, fontface =2)  + annotate("text", x = 73, y = 0, label = "C", color="darkgreen", size=3, fontface =2)  + annotate("text", x = 86, y = 0, label = "I", color="darkgreen", size=3, fontface =2)
dev.off()

#Violence

estimates.violence<-subset(estimates.violence, is.na(estimates.violence$mean)==FALSE)

estimates.violence$date[estimates.violence$date=="10-26"]<-"."
estimates.violence$date[estimates.violence$date=="10-27"]<-"."
estimates.violence$date[estimates.violence$date=="10-29"]<-"."
estimates.violence$date[estimates.violence$date=="10-30"]<-"."
estimates.violence$date[estimates.violence$date=="11-01"]<-"."
estimates.violence$date[estimates.violence$date=="11-02"]<-"."
estimates.violence$date[estimates.violence$date=="11-03"]<-"."
estimates.violence$date[estimates.violence$date=="11-04"]<-"."
estimates.violence$date[estimates.violence$date=="11-05"]<-"."
estimates.violence$date[estimates.violence$date=="11-07"]<-"."
estimates.violence$date[estimates.violence$date=="11-08"]<-"."
estimates.violence$date[estimates.violence$date=="11-09"]<-"."
estimates.violence$date[estimates.violence$date=="11-10"]<-"."
estimates.violence$date[estimates.violence$date=="11-11"]<-"."
estimates.violence$date[estimates.violence$date=="11-13"]<-"."
estimates.violence$date[estimates.violence$date=="11-14"]<-"."
estimates.violence$date[estimates.violence$date=="11-15"]<-"."
estimates.violence$date[estimates.violence$date=="11-16"]<-"."
estimates.violence$date[estimates.violence$date=="11-17"]<-"."
estimates.violence$date[estimates.violence$date=="11-19"]<-"."
estimates.violence$date[estimates.violence$date=="11-20"]<-"."

estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=26)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=27)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=28)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=30)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=31)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=32)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=33)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=34)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=36)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=37)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=38)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=40)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=41)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=42)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=44)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=45)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=47)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=48)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=49)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=51)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=52)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=54)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=55)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=56)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=58)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=59)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=60)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=61)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=62)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=63)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=64)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=65)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=66)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=67)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=68)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=69)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=70)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=71)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=72)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=73)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=75)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=76)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=77)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=79)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=80)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=82)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=83)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=84)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=86)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=87)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=89)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=90)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=91)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=93)
estimates.violence<-subset(estimates.violence, estimates.violence$date.num!=94)

pdf('fig-violence-raw.pdf', width=7.75, height=3.5)
ggplot(estimates.violence, aes(x=date.num, y=mean, colour=party)) + geom_errorbar(aes(ymin=l95ci, ymax=u95ci), width=0, position=pd,  alpha=.5) +geom_line(position=pd, alpha=.5) +geom_point(position=pd, alpha=.5) + geom_line(aes(group = party), position=pd,alpha=.5) + theme_bw() + xlab("Date")+ ylab("Radicalism Intention Scale") + ylim(c(1.5,3.5)) + guides(color=guide_legend(title=NULL)) + ggtitle("Outcome: Radicalism Intention Scale") + scale_color_manual(values=c("dodgerblue3","palevioletred4","red2")) + theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=5.5)) + facet_grid(. ~ party) + scale_x_continuous(breaks=estimates.violence$date.num, labels=estimates.violence$date) + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + theme(plot.title = element_text(size = 11))  + theme(axis.title.x=element_blank()) + annotate("text", x = 8, y = 1.5, label = "D", color="darkgreen", size=3, fontface =2) + annotate("text", x = 12, y = 1.5, label = "M", color="darkgreen", size=3, fontface =2) + annotate("text", x = 49, y = 1.5, label = "E", color="darkgreen", size=3, fontface =2) + annotate("text", x = 37, y = 1.5, label = "B", color="darkgreen", size=3, fontface =2) + annotate("text", x = 25, y = 1.5, label = "L", color="darkgreen", size=3, fontface =2)  + annotate("text", x = 73, y = 1.5, label = "C", color="darkgreen", size=3, fontface =2) + annotate("text", x = 86, y = 1.5, label = "I", color="darkgreen", size=3, fontface =2)
dev.off()

####CONJOINT ANALYSIS####

###Install/Load Packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE, repos = "http://cran.us.r-project.org")
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("psych","ggplot2","stargazer","jtools","multcomp","janitor","interactions",
              "tidyverse","scales", "utils", "nnet", "cregg")
ipak(packages)

##Load and Clean Data
data.conjoint<-read.csv("conjoint.csv",stringsAsFactors=FALSE)
data.conjoint<-subset(data.conjoint,data.conjoint$gc==1) #select good completes
data.conjoint<-subset(data.conjoint,data.conjoint$Day!="SL")
data.conjoint<-subset(data.conjoint,data.conjoint$Day!="")
data.conjoint<-subset(data.conjoint,as.numeric(data.conjoint$Day)>34) #select days when conjoint was on survey
data.conjoint<-subset(data.conjoint, is.na(vers_CBCONJOINT)==FALSE) #remove cases that did not do the conjoint

data.conjoint$id<-seq(1:length(data.conjoint$rid)) #Create sequential respondent id. 

weights.merge<-data.frame(cbind(data$ResponseId,data$rid.x, data$w))
colnames(weights.merge)<-c("ResponseId","rid", "w")            

data.conjoint<-merge(data.conjoint,weights.merge, by="rid",all.x=TRUE,all.y=FALSE)
data.conjoint$w<-as.numeric(paste(data.conjoint$w))

##Rename Conjoint Columns
#Round 1, Candidate 1
names(data.conjoint)[names(data.conjoint) == "b516a009.7de9.4311.8a96.576015a0ba42.1.1_CBCONJOINT"] <- "Profession.r1c1"
names(data.conjoint)[names(data.conjoint) == "X347bb331.98fe.4589.bc68.fd10f71b2fb5.1.1_CBCONJOINT"] <- "Military.r1c1"
names(data.conjoint)[names(data.conjoint) == "b1737a5c.ed32.4bc3.9300.1af5b7e421e9.1.1_CBCONJOINT"] <- "Religion.r1c1"
names(data.conjoint)[names(data.conjoint) == "d4263278.85b4.45bf.9593.ce040103cf07.1.1_CBCONJOINT"] <- "Race.r1c1"
names(data.conjoint)[names(data.conjoint) == "c0a15296.2872.441c.b2c1.6b6980bf7810.1.1_CBCONJOINT"] <- "Gender.r1c1"
names(data.conjoint)[names(data.conjoint) == "X16aaee94.33d1.45f1.abf9.af2280a8bf1c.1.1_CBCONJOINT"] <- "Age.r1c1"
names(data.conjoint)[names(data.conjoint) == "X25620c8d.0ff6.4191.bd1e.a4916c1ec423.1.1_CBCONJOINT"] <- "Opinion.r1c1"

#Round 1, Candidate 2
names(data.conjoint)[names(data.conjoint) == "b516a009.7de9.4311.8a96.576015a0ba42.1.2_CBCONJOINT"] <- "Profession.r1c2"
names(data.conjoint)[names(data.conjoint) == "X347bb331.98fe.4589.bc68.fd10f71b2fb5.1.2_CBCONJOINT"] <- "Military.r1c2"
names(data.conjoint)[names(data.conjoint) == "b1737a5c.ed32.4bc3.9300.1af5b7e421e9.1.2_CBCONJOINT"] <- "Religion.r1c2"
names(data.conjoint)[names(data.conjoint) == "d4263278.85b4.45bf.9593.ce040103cf07.1.2_CBCONJOINT"] <- "Race.r1c2"
names(data.conjoint)[names(data.conjoint) == "c0a15296.2872.441c.b2c1.6b6980bf7810.1.2_CBCONJOINT"] <- "Gender.r1c2"
names(data.conjoint)[names(data.conjoint) == "X16aaee94.33d1.45f1.abf9.af2280a8bf1c.1.2_CBCONJOINT"] <- "Age.r1c2"
names(data.conjoint)[names(data.conjoint) == "X25620c8d.0ff6.4191.bd1e.a4916c1ec423.1.2_CBCONJOINT"] <- "Opinion.r1c2"

#Round 2, Candidate 1
names(data.conjoint)[names(data.conjoint) == "b516a009.7de9.4311.8a96.576015a0ba42.2.1_CBCONJOINT"] <- "Profession.r2c1"
names(data.conjoint)[names(data.conjoint) == "X347bb331.98fe.4589.bc68.fd10f71b2fb5.2.1_CBCONJOINT"] <- "Military.r2c1"
names(data.conjoint)[names(data.conjoint) == "b1737a5c.ed32.4bc3.9300.1af5b7e421e9.2.1_CBCONJOINT"] <- "Religion.r2c1"
names(data.conjoint)[names(data.conjoint) == "d4263278.85b4.45bf.9593.ce040103cf07.2.1_CBCONJOINT"] <- "Race.r2c1"
names(data.conjoint)[names(data.conjoint) == "c0a15296.2872.441c.b2c1.6b6980bf7810.2.1_CBCONJOINT"] <- "Gender.r2c1"
names(data.conjoint)[names(data.conjoint) == "X16aaee94.33d1.45f1.abf9.af2280a8bf1c.2.1_CBCONJOINT"] <- "Age.r2c1"
names(data.conjoint)[names(data.conjoint) == "X25620c8d.0ff6.4191.bd1e.a4916c1ec423.2.1_CBCONJOINT"] <- "Opinion.r2c1"

#Round 2, Candidate 2
names(data.conjoint)[names(data.conjoint) == "b516a009.7de9.4311.8a96.576015a0ba42.2.2_CBCONJOINT"] <- "Profession.r2c2"
names(data.conjoint)[names(data.conjoint) == "X347bb331.98fe.4589.bc68.fd10f71b2fb5.2.2_CBCONJOINT"] <- "Military.r2c2"
names(data.conjoint)[names(data.conjoint) == "b1737a5c.ed32.4bc3.9300.1af5b7e421e9.2.2_CBCONJOINT"] <- "Religion.r2c2"
names(data.conjoint)[names(data.conjoint) == "d4263278.85b4.45bf.9593.ce040103cf07.2.2_CBCONJOINT"] <- "Race.r2c2"
names(data.conjoint)[names(data.conjoint) == "c0a15296.2872.441c.b2c1.6b6980bf7810.2.2_CBCONJOINT"] <- "Gender.r2c2"
names(data.conjoint)[names(data.conjoint) == "X16aaee94.33d1.45f1.abf9.af2280a8bf1c.2.2_CBCONJOINT"] <- "Age.r2c2"
names(data.conjoint)[names(data.conjoint) == "X25620c8d.0ff6.4191.bd1e.a4916c1ec423.2.2_CBCONJOINT"] <- "Opinion.r2c2"

#Round 3, Candidate 1
names(data.conjoint)[names(data.conjoint) == "b516a009.7de9.4311.8a96.576015a0ba42.3.1_CBCONJOINT"] <- "Profession.r3c1"
names(data.conjoint)[names(data.conjoint) == "X347bb331.98fe.4589.bc68.fd10f71b2fb5.3.1_CBCONJOINT"] <- "Military.r3c1"
names(data.conjoint)[names(data.conjoint) == "b1737a5c.ed32.4bc3.9300.1af5b7e421e9.3.1_CBCONJOINT"] <- "Religion.r3c1"
names(data.conjoint)[names(data.conjoint) == "d4263278.85b4.45bf.9593.ce040103cf07.3.1_CBCONJOINT"] <- "Race.r3c1"
names(data.conjoint)[names(data.conjoint) == "c0a15296.2872.441c.b2c1.6b6980bf7810.3.1_CBCONJOINT"] <- "Gender.r3c1"
names(data.conjoint)[names(data.conjoint) == "X16aaee94.33d1.45f1.abf9.af2280a8bf1c.3.1_CBCONJOINT"] <- "Age.r3c1"
names(data.conjoint)[names(data.conjoint) == "X25620c8d.0ff6.4191.bd1e.a4916c1ec423.3.1_CBCONJOINT"] <- "Opinion.r3c1"

#Round 3, Candidate 2
names(data.conjoint)[names(data.conjoint) == "b516a009.7de9.4311.8a96.576015a0ba42.3.2_CBCONJOINT"] <- "Profession.r3c2"
names(data.conjoint)[names(data.conjoint) == "X347bb331.98fe.4589.bc68.fd10f71b2fb5.3.2_CBCONJOINT"] <- "Military.r3c2"
names(data.conjoint)[names(data.conjoint) == "b1737a5c.ed32.4bc3.9300.1af5b7e421e9.3.2_CBCONJOINT"] <- "Religion.r3c2"
names(data.conjoint)[names(data.conjoint) == "d4263278.85b4.45bf.9593.ce040103cf07.3.2_CBCONJOINT"] <- "Race.r3c2"
names(data.conjoint)[names(data.conjoint) == "c0a15296.2872.441c.b2c1.6b6980bf7810.3.2_CBCONJOINT"] <- "Gender.r3c2"
names(data.conjoint)[names(data.conjoint) == "X16aaee94.33d1.45f1.abf9.af2280a8bf1c.3.2_CBCONJOINT"] <- "Age.r3c2"
names(data.conjoint)[names(data.conjoint) == "X25620c8d.0ff6.4191.bd1e.a4916c1ec423.3.2_CBCONJOINT"] <- "Opinion.r3c2"

#Candidate Choice

data.conjoint$choice.r1c1<-ifelse(data.conjoint$Q189==1,1,0)
data.conjoint$choice.r1c2<-ifelse(data.conjoint$Q189==2,1,0)
data.conjoint$choice.r2c1<-ifelse(data.conjoint$Q190==1,1,0)
data.conjoint$choice.r2c2<-ifelse(data.conjoint$Q190==2,1,0)
data.conjoint$choice.r3c1<-ifelse(data.conjoint$Q191==1,1,0)
data.conjoint$choice.r3c2<-ifelse(data.conjoint$Q191==2,1,0)

##Reshape Data wide -> long
data.conjoint.long<-reshape(data.conjoint,
                   varying  = list(c("choice.r1c1", "choice.r1c2", "choice.r2c1", "choice.r2c2", "choice.r3c1", "choice.r3c2"), 
                                   c("Profession.r1c1", "Profession.r1c2", "Profession.r2c1", "Profession.r2c2", "Profession.r3c1", "Profession.r3c2"),
                                   c("Military.r1c1", "Military.r1c2", "Military.r2c1", "Military.r2c2", "Military.r3c1", "Military.r3c2"),
                                   c("Religion.r1c1", "Religion.r1c2", "Religion.r2c1", "Religion.r2c2", "Religion.r3c1", "Religion.r3c2"),
                                   c("Race.r1c1", "Race.r1c2", "Race.r2c1", "Race.r2c2", "Race.r3c1", "Race.r3c2"),
                                   c("Gender.r1c1", "Gender.r1c2", "Gender.r2c1", "Gender.r2c2", "Gender.r3c1", "Gender.r3c2"),
                                   c("Age.r1c1", "Age.r1c2", "Age.r2c1", "Age.r2c2", "Age.r3c1", "Age.r3c2"),
                                   c("Opinion.r1c1", "Opinion.r1c2", "Opinion.r2c1", "Opinion.r2c2", "Opinion.r3c1", "Opinion.r3c2")),
                   v.names = c("Choice","G.Profession", "F.Military", "E.Religion","D.Race", "C.Gender", "B.Age", "A.Opinion"),
                   timevar = "Round.Candidate",
                   times = c("Round1Cand1","Round1Cand2","Round2Cand1","Round2Cand2","Round3Cand1","Round3Cand2"),
                   direction = "long")

#Convert Attributes to Factors
data.conjoint.long$G.Profession<-factor(data.conjoint.long$G.Profession, labels = c("Business Owner", "Doctor", "Farmer","Teacher", "Lawyer"))
data.conjoint.long$F.Military<-factor(data.conjoint.long$F.Military, labels = c("None", "Military Experience"))
data.conjoint.long$E.Religion<-factor(data.conjoint.long$E.Religion, labels = c("Catholic", "Jewish", "Protestant"))
data.conjoint.long$D.Race<-factor(data.conjoint.long$D.Race, labels = c("Asian", "Black", "Latino", "White"))
data.conjoint.long$C.Gender<-factor(data.conjoint.long$C.Gender, labels = c("Female", "Male"))
data.conjoint.long$B.Age<-factor(data.conjoint.long$B.Age, labels = c("75", "36", "45", "52", "60", "68"))
data.conjoint.long$A.Opinion<-factor(data.conjoint.long$A.Opinion, labels = c("Trump Lost", "Trump Won"))

#Legitimacy
data.conjoint.long$new<-data.conjoint.long$E3
data.conjoint.long$new<-na_if(data.conjoint.long$E3,3) #Exclude DKs
data.conjoint.long$ElectionBelief<-factor(data.conjoint.long$new, labels = c("Outcome Legitimate", "Outcome Illegitimate")) 

##Analysis

data.conjoint.long$legitimate<-NA
data.conjoint.long$legitimate[data.conjoint.long$E3==1]<-1
data.conjoint.long$legitimate[data.conjoint.long$E3==2]<-0
data.conjoint.long$legitimate[data.conjoint.long$E3==3]<-0

data.conjoint.long$party.dem<-0
data.conjoint.long$party.dem[data.conjoint.long$P2a=="Democrat"]<-1
summary(data.conjoint.long$party.dem)

data.conjoint.long$party.rep<-0
data.conjoint.long$party.rep[data.conjoint.long$P2a=="1"]<-1
summary(data.conjoint.long$party.rep)

data.conjoint.long$party.indother<-0
data.conjoint.long$party.indother[data.conjoint.long$P2a=="3"]<-1
data.conjoint.long$party.indother[data.conjoint.long$P2a=="4"]<-1
summary(data.conjoint.long$party.indother)

data.conjoint.long.rep<-subset(data.conjoint.long, data.conjoint.long$party.rep==1)
data.conjoint.long.indother<-subset(data.conjoint.long, data.conjoint.long$party.indother==1)

data.conjoint.long.rep.legitimate<-subset(data.conjoint.long, data.conjoint.long$party.rep==1&data.conjoint.long$legitimate==1)
data.conjoint.long.rep.notlegitimate<-subset(data.conjoint.long, data.conjoint.long$party.rep==1&data.conjoint.long$legitimate==0)

m1<-lm(Choice ~ G.Profession+F.Military+E.Religion+D.Race+C.Gender+B.Age+A.Opinion,  data = data.conjoint.long.rep)
summary(m1)
m2<-lm(Choice ~  G.Profession+F.Military+E.Religion+D.Race+C.Gender+B.Age+A.Opinion,  data = data.conjoint.long.indother)
summary(m2)

pdf('fig-conjoint-raw.pdf', width=7.25, height=7)
ggcoef_compare(list("Sample: Republicans" = m1, "Sample: Independent/Other" = m2), type="faceted", point_size=2,point_stroke=2,point_fill="grey50",stripped_rows=FALSE,colour=c("red1","purple"), significance_labels = NULL, variable_labels = c(A.Opinion = "Opinion",G.Profession="Profession", E.Religion="Religion",D.Race = "Race", B.Age="Age",C.Gender="Gender",F.Military="Military")) + xlab("Change in the Probability of Winning") + ylab("Candidate Attribute") + xlim(c(-.28,.28)) + theme_bw() + guides(color=guide_legend(title=NULL)) + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()) + theme(plot.title = element_text(size = 11))  + theme(legend.title = element_blank())   + scale_fill_manual(values = c('grey50')) + scale_color_manual(values = c('grey50','grey50','grey50','grey50','grey50','grey50','grey50')) +  scale_shape_manual(values = c(16,16)) + theme(legend.position = "none") 
dev.off()

m3<-lm(Choice ~ G.Profession+F.Military+E.Religion+D.Race+C.Gender+B.Age+A.Opinion,  data = data.conjoint.long.rep, weights=data.conjoint.long.rep$w)
summary(m3)
m4<-lm(Choice ~  G.Profession+F.Military+E.Religion+D.Race+C.Gender+B.Age+A.Opinion,  data = data.conjoint.long.indother, weights=data.conjoint.long.indother$w)
summary(m4)

pdf('fig-conjoint.pdf', width=7.25, height=7)
ggcoef_compare(list("Sample: Republicans" = m3, "Sample: Independent/Other" = m4), type="faceted", point_size=2,point_stroke=2,point_fill="grey50",stripped_rows=FALSE,colour=c("red1","purple"), significance_labels = NULL, variable_labels = c(A.Opinion = "Opinion",G.Profession="Profession", E.Religion="Religion",D.Race = "Race", B.Age="Age",C.Gender="Gender",F.Military="Military")) + xlab("Change in the Probability of Winning") + ylab("Candidate Attribute") + xlim(c(-.28,.28)) + theme_bw() + guides(color=guide_legend(title=NULL)) + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()) + theme(plot.title = element_text(size = 11))  + theme(legend.title = element_blank())   + scale_fill_manual(values = c('grey50')) + scale_color_manual(values = c('grey50','grey50','grey50','grey50','grey50','grey50','grey50')) +  scale_shape_manual(values = c(16,16)) + theme(legend.position = "none") 
dev.off()

m5<-lm(Choice ~ G.Profession+F.Military+E.Religion+D.Race+C.Gender+B.Age+A.Opinion,  data = data.conjoint.long.rep.legitimate, weights=data.conjoint.long.rep.legitimate$w)
summary(m5)
m6<-lm(Choice ~  G.Profession+F.Military+E.Religion+D.Race+C.Gender+B.Age+A.Opinion,  data = data.conjoint.long.rep.notlegitimate, weights=data.conjoint.long.rep.notlegitimate$w)
summary(m6)

pdf('fig-conjoint-comp.pdf', width=10.25, height=7)
ggcoef_compare(list("Sample: All Republicans" = m3, "Sample: Republicans, Election Legitimate" = m5, "Sample: Republicans, Election Not Legitimate" = m6), type="faceted", point_size=2,point_stroke=2,point_fill="grey50",stripped_rows=FALSE,colour=c("red1","red3","red2"), significance_labels = NULL, variable_labels = c(A.Opinion = "Opinion",G.Profession="Profession", E.Religion="Religion",D.Race = "Race", B.Age="Age",C.Gender="Gender",F.Military="Military")) + xlab("Change in the Probability of Winning") + ylab("Candidate Attribute") + xlim(c(-.35,.35)) + theme_bw() + guides(color=guide_legend(title=NULL)) + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()) + theme(plot.title = element_text(size = 11))  + theme(legend.title = element_blank())   + scale_fill_manual(values = c('grey50')) + scale_color_manual(values = c('grey50','grey50','grey50','grey50','grey50','grey50','grey50')) +  scale_shape_manual(values = c(16,16)) + theme(legend.position = "none") 
dev.off()
