# Matti Liski and Iivo Vehviläinen
# 
# "Gone with the wind?
# An empirical analysis of the renewable energy rent transfer"
# Journal of the Association of Environmental and Resource Economists
#
# Last update: 2020-04-02 / Iivo Vehvilainen, iivo.vehvilainen@aalto.fi
#
# Replication instructions:
# - Copy the script and data file "LV_wind.RData" to a folder
# - Enter the full path of the folder to DATA.FOLDER below
# - Note that running the script will empty current R session
# - Ensure that all required packages are installed (see System setup below)
# - Run script (in Mac R Studio, load script and press "Shift"+"Apple Key"+"S")

# Configure -------------------------------------------------------------------

# clear old stuff (running this script will clear the current session in R)
rm(list=ls(all=TRUE))

# Data folder, modify here to point to the appropriate folder with the data files

DATA.FOLDER = "/Users/iivo/Dropbox (Aalto)/Share/Identification/model"

# Controls for the output
#
# VERBOSE setting
#   0 - No output
#   1 - Key Figures and tables in R console
#   2 - All Figures and tables in R console

VERBOSE = 2

# Control the start and end of the analysis (data is from 2001 to 2014)

FIT.START.YEAR = 2001
FIT.END.YEAR = 2017

# System setup ----------------------------------------------------------------

# if packages are needed uncomment and run the following line
#install.packages(c("AER","ivpack", "systemfit", "data.table", "ggplot2", "RColorBrewer", "scales", "stargazer"))

# load required packages

# statistical tools
library(AER)
library(ivpack)
library(systemfit)

# data handling, graphics and formatting
library(data.table)
library(ggplot2)
library(RColorBrewer)
library(scales)
library(stargazer)

# set the working directory
setwd(DATA.FOLDER)

# control if figures are saved to disk
SAVE.FIGURES = T

# set where to dump unneccesary output (different on Windows/Mac platform)
if (Sys.info()['sysname']=="Windows") {
  DEV.NULL = "NUL"
} else {
  DEV.NULL = "/dev/null"
}

# Load and preprocess data ----------------------------------------------------

# dt.fit contains required data for estimation:
#   yr                = year
#   mo                = month
#   date              = first day of the month in date format
#   Jan,...,Dec       = month-of-year dummies 
#   month             = repeated month as factor
#   Q1,...,Q4         = quarterly dummies
#   quarter           = repeated quarter as factor
#   year2001,...      = year dummies
#   year              = repeated year as factor
#   hicp              = HICP inflation rate for EU (year 2010 = 100)
#   spot2010          = system spot price in 2010 euro, EUR/MWh
#   coal2010          = coal price in 2010 euro, EUR/MWh
#   euets2010         = price of EU Emission Trading Scheme rights in 2010 euro, EUR/tCO2
#   demand            = actual demand in TWh
#   thermal           = total (conventional) thermal power generation + trade in total in TWh
#   nuclear           = nuclear power generation in TWh
#   wind              = wind power generation in TWh
#   hydro             = hydro power generation in TWh
#   demand.xx         = actual demand by area in TWh
#   inflow            = monthly inflow in absolute terms, TWh
#   past.inflows      = sum of the previous 12 month inflows, TWh
#   temp              = monthly mean temperature of the Nordic capitals
#   spot.vola         = monthly mean of the hourly volatility of spot price
#
# dt.eea and dt.supply.yr contain descriptive data for Tables/Figures in the Appendix.

load("LV_wind.RData")

# adjust start and end year used for fitting
start.year = FIT.START.YEAR
end.year = FIT.END.YEAR

# some housekeeping and dummies for fixed effects
dt.fit = dt.fit[yr>=start.year&yr<=end.year,]
n.years = end.year-start.year+1 

years = start.year:end.year 
year.names = paste("year", years, sep="")
month.names = paste("month", month.abb, sep="")

# Auxiliary functions ---------------------------------------------------------

# Multiple plot function (from Cookbook for R)
# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/

multiplot = function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# function to format plots

figure.format <- function(dt.pic, xlabel, ylabel, title, bguide=FALSE) {
  # plot data and fitted values 
  if (colnames(dt.pic)[1]=="date" & dim(dt.pic)[2] == 3) {
    dt.pic[, date:=as.Date(date)]
    p.figure = ggplot(melt(dt.pic, id="date"))+geom_line(aes(date, value, color=variable, linetype=variable), show.legend=bguide)
    p.figure = p.figure + scale_linetype_manual(name = "Value", values=c("solid", "11")) + scale_color_manual(values=c("black", "black"), guide=FALSE) 
    if(dim(dt.pic)[1]>12) {
      p.figure = p.figure + scale_x_date(breaks = date_breaks("year"), labels= date_format("%Y")) + scale_y_continuous(labels = comma)
    } else {
      p.figure = p.figure + scale_x_date(breaks = date_breaks("months"), labels= date_format("%b")) + scale_y_continuous(labels = comma)
      p.figure = p.figure + scale_x_date(breaks = date_breaks("months"), labels= month.abb) + scale_y_continuous(labels = comma)
    }
  }
  
  if (colnames(dt.pic)[1]=="mo") {
    p.figure = ggplot(dt.pic)+geom_line(aes(x=mo, y=mean),linetype="solid")
    p.figure = p.figure + geom_ribbon(aes(x=mo, ymin=lwr, ymax=upr), alpha=.2, colour=NA, show.legend = F)
    p.figure = p.figure + scale_x_continuous(breaks=c(1:12),labels=c(month.abb[dt.pic$mo])) + scale_y_continuous(labels = comma)
  }
  
  if (colnames(dt.pic)[1]=="mo" & dim(dt.pic)[2] == 3) {
    dt.pic[, lwr:=mean-sd]
    dt.pic[, upr:=mean+sd]
    dt.pic[, sd:=NULL]
  }
  
  if (colnames(dt.pic)[1]=="mo" & dim(dt.pic)[2] >= 5) {
    p.figure = p.figure + geom_line(aes(x=mo, y=act), linetype="11", show.legend=bguide)
  }
  
  if (colnames(dt.pic)[1]=="mo" & dim(dt.pic)[2] == 6) {
    p.figure = p.figure + geom_line(aes(x=mo, y=invariant), linetype="dashed", show.legend=bguide)
  }
  
  p.figure = p.figure + theme_bw(base_family="serif", base_size = 12) 
  p.figure = p.figure + xlab(xlabel) + ylab(ylabel) + ggtitle(title)
  return(p.figure)
}

# function to suppress Robust Standard Errors messages from ivpack

fun.robust = function(obj) {
  capture.output({y=robust.se(obj)}, file=DEV.NULL)
  return(y)
}

# CALCULATIONS -----------------------------------------------------------------------------

# report that we start the fit
if (VERBOSE > 0) {
  cat("#-----------------------------------------------------------------------------\n")
  cat(" Time period: ", start.year, "-", end.year, "\n")
  cat("#-----------------------------------------------------------------------------\n")
}


# 2 EMPIRICAL STRATEGY --------------------------------------------------------

# 2.2 Markets and data --------------------------------------------------------

# Residualize variables with respect to the fixed effects ----
#  - Following Lovell, 1963 

o.fe.temp = lm(temp ~ month + year, data=dt.fit)
o.fe.inflow = lm(inflow ~ month + year, data=dt.fit)
o.fe.past.inflows = lm(past.inflows ~ month + year, data=dt.fit)

o.fe.spot = lm(spot2010 ~ month + year, data=dt.fit)
o.fe.coal = lm(coal2010 ~ month + year, data=dt.fit)
o.fe.euets = lm(euets2010 ~ month + year, data=dt.fit)

o.fe.demand = lm(demand ~ month + year, data=dt.fit)
o.fe.hydro = lm(hydro ~ month + year, data=dt.fit)
o.fe.nuclear = lm(nuclear ~ month + year, data=dt.fit)
o.fe.thermal = lm(thermal ~ month + year, data=dt.fit)

o.fe.spot.vola = lm(spot.vola ~ month + year, data=dt.fit)

# Save the within fixed effects variation
dt.fit$temp.a = o.fe.temp$residuals
dt.fit$inflow.a = o.fe.inflow$residuals
dt.fit$past.inflows.a = o.fe.past.inflows$residuals

dt.fit$spot2010.a = o.fe.spot$residuals
dt.fit$coal2010.a = o.fe.coal$residuals
dt.fit$euets2010.a = o.fe.euets$residuals

dt.fit$demand.a = o.fe.demand$residuals
dt.fit$hydro.a = o.fe.hydro$residuals
dt.fit$nuclear.a = o.fe.nuclear$residuals
dt.fit$thermal.a = o.fe.thermal$residuals
dt.fit$spot.vola.a = o.fe.spot.vola$residuals

# Wind is increasing throughout the time period hence log-transformed fit to correct for the increasing variations
o.fe.wind = lm(log(wind) ~ month + year, data=dt.fit)

# Correct for log-bias in the estimation
# (Neyman and Scott, Correction for Bias Introduced by a Transformation of Variables, Annals of Mathematical Statistics, 1960)

dt.adj.wind = data.table(dt.fit[, .(yr,mo)], fit.raw=exp(o.fe.wind$fitted.values), exp.residual=exp(o.fe.wind$residuals))
dt.adj.wind[, adj:=mean(exp.residual), by=yr]
dt.adj.wind[, fit:=fit.raw*adj]

# Residual for wind calculated as a difference from the fitted values 
dt.fit$wind.a = dt.fit$wind - dt.adj.wind$fit

# Demand regressions

# Regress demand with varying set of covariants and fixed effects, results reported in the Appendix
o.demand.l = list()
o.demand.l[[1]] = lm(demand ~ temp.a, data=dt.fit)
o.demand.l[[2]] = lm(demand ~ temp.a + month, data=dt.fit)
o.demand.l[[3]] = lm(demand ~ temp.a + year, data=dt.fit)
o.demand.l[[4]] = lm(demand ~ temp.a + month + year, data=dt.fit)
o.demand.l[[5]] = lm(demand ~ temp.a + month + year + spot2010.a, data=dt.fit)

# .. Table 1 Annual supply by technology in the Nordic region ----

if (VERBOSE > 0) {
  # Get annual average supply by technology and area
  dt.supply.area = dt.supply.yr[, lapply(.SD, function(x) round(mean(x/1000),1)), .SDcols=c("hydro", "thermal", "wind", "nuclear"), by=c("area")][, c("hydro", "thermal", "wind", "nuclear"), with=F]
  
  # Add totals
  dt.supply.area = rbind(dt.supply.area, dt.supply.area[, lapply(.SD, sum)])
  
  table.text = "Annual supply by technology in the Nordic region"
  
  setcolorder(dt.supply.area, c("hydro", "nuclear", "thermal", "wind"))
  colnames(dt.supply.area) = c("HYDRO", "NUCLEAR", "THERMAL", "WIND")
  rownames(dt.supply.area) = c("DEN", "FIN", "NOR", "SWE", "Total")
  
  stargazer(dt.supply.area, type="text", summary=FALSE, rownames=TRUE, digits=1, font.size="scriptsize", title=table.text)
  #stargazer(dt.supply.area, type="latex", summary=FALSE, rownames=TRUE, digits=1, font.size="scriptsize", title=table.text)
}

# .. Table 2 Summary statistics ----

if (VERBOSE > 0) {
  # Calculate within fixed effects standard deviations  
  dt.sd = data.table(id="SD", dt.fit[, lapply(.SD, sd), .SDcols=c("demand.a", "hydro.a", "nuclear.a", "thermal.a", "wind.a", "temp.a", "inflow.a", "past.inflows.a", "spot2010.a", "coal2010.a", "euets2010.a")])
  
  # Collect to a table and format
  setnames(dt.sd, c("demand.a", "hydro.a", "nuclear.a", "thermal.a", "wind.a", "temp.a", "inflow.a", "past.inflows.a", "spot2010.a", "coal2010.a", "euets2010.a"), c("demand", "hydro", "nuclear", "thermal", "wind", "temp", "inflow", "past.inflows", "spot2010", "coal2010", "euets2010"))
  dt.descriptives = rbind(data.table(id="Mean", dt.fit[, lapply(.SD, mean), .SDcols=c("demand", "hydro", "nuclear", "thermal", "wind", "temp", "inflow", "past.inflows", "spot2010", "coal2010", "euets2010")]),
                          data.table(id="Median", dt.fit[, lapply(.SD, median), .SDcols=c("demand", "hydro", "nuclear", "thermal", "wind", "temp", "inflow", "past.inflows", "spot2010", "coal2010", "euets2010")]),
                          dt.sd,
                          data.table(id="Min", dt.fit[, lapply(.SD, min), .SDcols=c("demand", "hydro", "nuclear", "thermal", "wind", "temp", "inflow", "past.inflows", "spot2010", "coal2010", "euets2010")]),
                          data.table(id="Max", dt.fit[, lapply(.SD, max), .SDcols=c("demand", "hydro", "nuclear", "thermal", "wind", "temp", "inflow", "past.inflows", "spot2010", "coal2010", "euets2010")]))
  dt.descriptives = dcast(melt(dt.descriptives, id.vars = "id"), variable ~ id)
  setcolorder(dt.descriptives, c("variable", "Mean", "Median", "SD", "Min", "Max"))
  dt.descriptives$variable = c("Demand", "Hydro", "Nuclear", "Thermal", "Wind", "Temperature", "Inflow", "Past inflows", "Spot price", "Coal price", "Carbon price")
  
  table.text = "Summary statistics"
  
  # Output results
  stargazer(dt.descriptives, type="text", summary = F, digits = 2, rownames = F, title=table.text)
  #stargazer(dt.descriptives, type="latex", summary = F, digits = 2, rownames = F, title=table.text)
}

# .. Figure 1 Wind generation over time ----

if (VERBOSE > 0) {
  # Collect data
  dt.pic.wind = data.table(date=as.Date(dt.fit$date), model=dt.adj.wind$fit, act=dt.fit$wind)
  p.wind = figure.format(dt.pic.wind, " ", "TWh/month", "")
  
  # Save if specified
  if (SAVE.FIGURES==TRUE) {
    pdf(file="p_wind.pdf", width=3.94, height=2.62, pointsize=9)
    print(p.wind+theme_bw(base_family="serif", base_size = 8))
    dev.off()
  }
  
  # Output
  print(p.wind)
}

# 2.3 The adjusting margin: outputs -------------------------------------------

# Regression of output technologies on wind and controls
o.thermal.q = lm(thermal ~ wind.a + temp.a + inflow.a + past.inflows + month + year, data=dt.fit)
o.hydro.q = lm(hydro ~ wind.a + temp.a + inflow.a + past.inflows + month + year, data=dt.fit)
o.nuclear.q = lm(nuclear ~ wind.a + temp.a + inflow.a + past.inflows + month + year, data=dt.fit)

# .. Table 3 Regression of output of other technologies on wind ----

if (VERBOSE > 0) {
  # Collect specifcation to report
  o.volumes = list(o.thermal.q, o.hydro.q, o.nuclear.q)
  
  # Calculate robust standard errors
  robust.se = lapply(o.volumes, function(o) fun.robust(o)[,2])
  
  # Collect F-statistics
  f.stats.volumes = c("F Statistic", sapply(o.volumes, function(o) round(summary(o)$f[1],1)))
  
  # Format table
  covariate.names = names(o.thermal.q$coefficients)
  omit.covariants = c("Constant", covariate.names[sapply(c(month.names, year.names), function(n) match(n, covariate.names))])
  omit.covariants = omit.covariants[!is.na(omit.covariants)]
  covariate.labels = c("Wind", "Temperature", "Inflow", "Past inflows")
  
  add.lines = list(c("Month FE", "Yes", "Yes", "Yes"), c("Year FE", "Yes", "Yes", "Yes"), f.stats.volumes)
  
  table.text = "Regression of output of other technologies on wind"
  
  # Output
  stargazer(o.volumes, type="text", single.row = T, omit.stat = c("F", "ser"), report=c("vsc"), se=robust.se, add.lines = add.lines, omit=omit.covariants, covariate.labels = covariate.labels, font.size="scriptsize", title=table.text)
  #stargazer(o.volumes, type="latex", single.row = T, omit.stat = c("F", "ser"), report=c("vsc"), se=robust.se, add.lines = add.lines, omit=omit.covariants, covariate.labels = covariate.labels, font.size="scriptsize", title=table.text)
}


# Thermal quantity regression

# Specifications for 1st stage regression of thermal quantities
fm.thermal.l = list()

fm.thermal.l[[1]] = thermal ~ month + year
fm.thermal.l[[2]] = thermal ~ wind.a + temp.a + month + year
fm.thermal.l[[3]] = thermal ~ wind.a + inflow.a + past.inflows + month + year
fm.thermal.l[[4]] = thermal ~ wind.a + temp.a + inflow.a + past.inflows + month + year
fm.thermal.l[[5]] = thermal ~ wind.a + temp.a + inflow.a + past.inflows + coal2010.a + euets2010.a + month + year

# Carry out the regression
o.thermal.l = lapply(fm.thermal.l, function(fm) lm(fm, data=dt.fit))

# Select baseline model for thermal and regress hydro against the same covariates
o.thermal = o.thermal.l[[4]] 
o.hydro = lm(hydro ~ wind.a + temp.a + inflow.a + past.inflows + month + year, data=dt.fit)

# .. Figure 2 Historical and fitted adjusting margin supply ----

if (VERBOSE > 0) {
  # Collect data from the model above
  dt.pic.thermal = data.table(date=as.Date(dt.fit$date), model=o.thermal$fitted, act=dt.fit$thermal)
  
  # Format plots
  p.thermal.policy = figure.format(dt.pic.thermal, " ", "TWh/month", "")
  
  # Save if specified
  if (SAVE.FIGURES==TRUE) {
    pdf(file="p_policies.pdf", width=3.94, height=2.62, pointsize=9)
    print(p.thermal.policy+theme_bw(base_family="serif", base_size = 8))
    dev.off()
  }
  
  # Output
  print(p.thermal.policy)
}

# 2.4 The adjusting margin: prices --------------------------------------------

# Define instruments and formulas on the basis of the specifications 

spec.second.stage.l = list()
spec.second.stage.l[[1]] = "| wind.a + temp.a + coal2010.a + euets2010.a + year + month"
spec.second.stage.l[[2]] = "| wind.a + inflow.a + past.inflows + coal2010.a + euets2010.a + year + month"
spec.second.stage.l[[3]] = "| wind.a + temp.a + inflow.a + past.inflows + coal2010.a + euets2010.a + year + month"

fm.iv.l = lapply(spec.second.stage.l, function(s) as.formula(paste("log(spot2010) ~ thermal.a + coal2010.a + euets2010.a + year + month", s)))
o.iv.l = list()

# For comparison, add OLS estimate with the same covariants
o.ols = lm(log(spot2010) ~ thermal.a + coal2010.a + euets2010.a + month + year, data=dt.fit)
o.iv.l[[1]] = o.ols

# Run instrumental variables regression with ivreg from AER package
o.iv.l[[2]] = ivreg(fm.iv.l[[1]], data=dt.fit)
o.iv.l[[3]] = ivreg(fm.iv.l[[2]], data=dt.fit)
o.iv.l[[4]] = ivreg(fm.iv.l[[3]], data=dt.fit)

# Select basemodel
o.iv = o.iv.l[[4]]

# Correct for log-bias in the estimation 
# (Neyman and Scott, Correction for Bias Introduced by a Transformation of Variables, Annals of Mathematical Statistics, 1960)
dt.adj.log = data.table(dt.fit[, .(yr,mo)], fit.raw=exp(o.iv$fitted.values), residual=o.iv$residuals)
dt.adj.log[, adj:=exp(sd(residual)^2/2), by=mo]
dt.adj.log[, fit:=fit.raw*adj]

adj.factor.log = dt.adj.log$adj[1:12]

# .. Table 4 Regressions of ln(p(t)) with OLS and IV ----

if (VERBOSE > 0) {
  # Calculate robust standard errors
  robust.se = lapply(o.iv.l, function(o) fun.robust(o)[,2])
  
  # Collect F-statistics
  thermal.fs = sapply(o.thermal.l, function(o) round(summary(o)$f[1],1))
  add.lines = list(c("F Statistic (1st stage)", c(round(summary(o.ols)$fstatistic[1],1), thermal.fs[2:5])), c("Seasonal FE", "Yes", "Yes", "Yes", "Yes", "Yes"), c("Annual FE", "Yes", "Yes", "Yes", "Yes", "Yes"))
  
  table.text = "Regressions of ln(p(t)) with OLS and IV"
  covariate.labels = c("Thermal", "Coal price", "Carbon price")
  
  # Output
  stargazer(o.iv.l, type="text", single.row=T, omit.stat=c("F", "ser"), report=c("vsc"), add.lines=add.lines, se=robust.se, omit=c("Constant", month.names, year.names), covariate.labels = covariate.labels, title=table.text)
  #stargazer(o.iv.l, type="latex", single.row=T, omit.stat=c("F", "ser"), report=c("vsc"), add.lines=add.lines, se=robust.se, omit=c("Constant", month.names, year.names), covariate.labels = covariate.labels, title=table.text)
}

# .. Figure 3 Historical and fitted prices from the IV regression ----

if (VERBOSE > 0) {
  # Collect data
  dt.pic.spot = data.table(date=as.Date(dt.fit$date), model=exp(o.iv$fitted.values), act = dt.fit$spot2010)
  dt.pic.spot$model = dt.pic.spot$model*adj.factor.log
  p.spot = figure.format(dt.pic.spot, "", "Price, EUR2010/MWh", "")
  
  # Save if specified
  if (SAVE.FIGURES==TRUE) {
    pdf(file="p_price.pdf", width=3.94, height=2.62, pointsize=9)
    print(p.spot+theme_bw(base_family="serif", base_size = 8))
    dev.off()
  } 
  
  # Output
  print(p.spot)
}

# 2.5 Counterfactual analysis -------------------------------------------------

# Trends in wind and thermal quantities

# .. Figure 4 Trends in wind and thermal quantities and prices ----

if (VERBOSE > 0) { 
  # Collect data
  dt.yr.trend = na.omit(data.table(yr=years,Thermal=12*o.thermal$coefficients[year.names], Wind=12*exp(o.fe.wind$coefficients[1]+o.fe.wind$coefficients[year.names])))
  
  # Add zero for first year dummy
  dt.pic = melt(dt.yr.trend, id="yr")
  dt.pic = rbind(dt.pic, data.table(variable=dt.pic[, unique(variable)], yr=start.year, value=0))
  
  # Format plot
  p.trend = ggplot(dt.pic, aes(yr, value, linetype=variable))+geom_point(aes(shape=variable), size=1)+stat_smooth(method="loess", color="black", size=0.5)
  p.trend = p.trend + theme_bw(base_family="serif", base_size = 12) + theme(legend.title=element_blank())  
  p.trend = p.trend + scale_x_continuous(breaks=seq(start.year, end.year, by=2)) + ylim(c(-60,60)) 
  p.trend = p.trend + xlab("") + ylab("TWh/a") + ggtitle("")
  
  # Remove input price variation from output prices
  dt.res = dt.fit[, .(yr, spot2010, coal2010, euets2010)]
  dt.res[, residualized:=exp(log(spot2010)-(coal2010-mean(coal2010))*o.iv$coefficients["coal2010.a"]-(euets2010-mean(euets2010))*o.iv$coefficients["euets2010.a"])]
  
  # Take annual means
  dt.pic = dt.res[, .(actual = mean(spot2010), residualized = mean(residualized)), by=yr]
  
  # Format figure
  p.price.res = ggplot(dt.pic, aes(yr, residualized))+geom_point(shape=20, size=1)+stat_smooth(method="loess", color="black", size=0.5)
  p.price.res = p.price.res + theme_bw(base_family="serif", base_size = 12) + theme(legend.title=element_blank()) 
  p.price.res = p.price.res + scale_x_continuous(breaks=seq(start.year, end.year, by=2)) + ylim(c(0,70)) 
  p.price.res = p.price.res + xlab("") + ylab("EUR/MWh") + ggtitle("")
  
  # Output
  if (SAVE.FIGURES==TRUE) {
    pdf(file="p_trends.pdf", width=5.12, height=2.62, pointsize=9)
    multiplot(p.trend +theme_bw(base_family="serif", base_size = 8) + theme(legend.title=element_blank(), legend.position=c(0.7, 0.87), legend.direction="horizontal"), p.price.res +theme_bw(base_family="serif", base_size = 8), cols=2)
    dev.off()
  }
  
  multiplot(p.trend + theme(legend.title=element_blank(), legend.position=c(0.6, 0.87), legend.direction="horizontal"), p.price.res, cols=2)
}


# We perform the fit with two packages as they provide slightly different output capabilities
# Systemfit prediction intervals are used later

dummies = c("Jan + Feb + Mar + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec + year2002 + year2003 + year2004 + year2005 + year2006 + year2007 + year2008 + year2009 + year2010 + year2011 + year2012 + year2013 + year2014 + year2015 + year2016 + year2017")

fm.iv2 = as.formula(paste("log(spot2010) ~ thermal + coal2010 + euets2010 +", dummies))
inst.iv2 = as.formula(paste("~  log(wind) + temp + inflow + past.inflows + coal2010 + euets2010 +", dummies))
o.iv2 = systemfit(fm.iv2, inst = inst.iv2, method="2SLS", data=dt.fit)

# Verify reported numbers in the paper

# average wind in the past decade (close to 20 TWh): 23 TWh
mean(dt.fit[yr>=2008&yr<=2017, sum(wind), by=yr]$V1)


# Invariant price distribution

# Create data set for invariant calculations
dt.invariant = dt.fit[, c("yr", "mo", month.abb, year.names, "month", "year"), with=F]

# Marginal costs set to mean value of the period
dt.invariant$coal2010 = mean(dt.fit$coal2010)
dt.invariant$euets2010 = mean(dt.fit$euets2010)

# Theraml volumes mean values of 1st stage fit for thermal
dt.invariant$thermal = rep(data.table(mo=1:12, thermal=o.thermal$fitted.values)[, mean(thermal), by=mo]$V1, n.years)

# Add mean values for other generation types
dt.invariant$wind = rep(dt.adj.wind[, mean(fit), by=mo]$V1, n.years)
dt.invariant$hydro = rep(dt.fit[, mean(hydro), by=mo]$V1, n.years)
dt.invariant$nuclear = rep(dt.fit[, mean(nuclear), by=mo]$V1, n.years)

# Invariant monthly PRICE

# calculate price estimate and confidence and prediction intervals
dt.price.conf = data.table(mo=1:12, exp(predict(o.iv2, interval = "confidence", level = 0.95, newdata=dt.invariant)))[, lapply(.SD, mean), by=mo]
dt.price.pred = data.table(mo=1:12, exp(predict(o.iv2, interval = "prediction", level = 0.95, newdata=dt.invariant)))[, lapply(.SD, mean), by=mo]

# Adjust column names
col.names.c = c("mean", "lwr", "upr")
col.names.p = c("p.mean", "p.lwr", "p.upr")
setnames(dt.price.conf, c("eq1.pred", "eq1.lwr", "eq1.upr"), col.names.c)
setnames(dt.price.pred, c("eq1.pred", "eq1.lwr", "eq1.upr"), col.names.p)

# Correct for log-bias in the estimation
# (Neyman and Scott, Correction for Bias Introduced by a Transformation of Variables, Annals of Mathematical Statistics, 1960)
dt.price.conf$adj = adj.factor.log
dt.price.pred$adj = adj.factor.log
dt.price.conf[, (col.names.c) := lapply(.SD, function(x) x*dt.price.conf[["adj"]] ), .SDcols=col.names.c]
dt.price.pred[, (col.names.p) := lapply(.SD, function(x) x*dt.price.pred[["adj"]] ), .SDcols=col.names.p]
dt.price.conf[, adj:=NULL]
dt.price.pred[, adj:=NULL]
dt.price.conf = dt.price.conf[, lapply(.SD, mean), .SDcols=c("mean","lwr","upr"), by=mo]
dt.price.pred = dt.price.pred[, lapply(.SD, mean), .SDcols=c("p.mean","p.lwr","p.upr"), by=mo]

# .. Figure 5 Expected monthly prices ----

if (VERBOSE > 0) {
  # Collect price data
  dt.pic = cbind(dt.price.conf, dt.price.pred[, (col.names.p), with=F])
  
  # Format figures
  p.price.conf = ggplot(dt.pic) + geom_line(aes(x=as.numeric(mo), y=mean))
  p.price.conf = p.price.conf + geom_ribbon(aes(x=as.numeric(mo), ymin=lwr, ymax=upr), alpha=.2, colour=NA, show.legend = F)
  p.price.conf = p.price.conf + geom_ribbon(aes(x=as.numeric(mo), ymin=p.lwr, ymax=p.upr), alpha=.2, colour=NA, show.legend = F)
  p.price.conf = p.price.conf + scale_x_continuous(breaks=c(1:12),labels=c(month.abb[dt.pic$mo])) + scale_y_continuous(labels = comma)
  p.price.conf = p.price.conf + theme_bw(base_family="sans", base_size = 12) 
  p.price.conf = p.price.conf + xlab("") + ylab("EUR/MWh") + ggtitle( "")
  
  # Save if specified
  if (SAVE.FIGURES==TRUE) {
    pdf(file="p_invariant_price.pdf", width=3.94, height=2.62, pointsize=9)
    print(p.price.conf+ylim(20,61)+theme_bw(base_family="serif", base_size = 9))
    dev.off()
  } 
  
  # Output
  print(p.price.conf)
}


# 3 Results: the equilibrium impact of wind power entry -----------------------

# 3.1 Prices ------------------------------------------------------------------

# Invariant monthly THERMAL

# Effect of additional wind to invariant prices 

# Profile for monthly wind generation in the Nordic area
wind.profile.mo = dt.adj.wind[, mean(fit), by=mo]$V1/sum(dt.adj.wind[, mean(fit), by=mo]$V1)

# Wind is variated from 0..50 TWh
wind.analyze = c(0, 10, 20, 30, 40, 50)
wind.change = -sum(dt.invariant[, mean(wind), by=mo]$V1) + wind.analyze

# Number of counterfactual scenarios
n.scenarios = length(wind.analyze)

# Create monthly scenarios for wind change and new wind
m.wind.change = as.matrix(sapply(wind.change, function(x) {round(wind.profile.mo*x,3)}))
colnames(m.wind.change) = wind.analyze

m.wind.analyze = as.matrix(sapply(wind.analyze, function(x) {round(wind.profile.mo*x,3)}))
colnames(m.wind.analyze) = wind.analyze

# Calculate monthly new thermal generation volumes after wind change
m.thermal.analyze = dt.invariant[, mean(thermal), by=mo]$V1 - m.wind.change
colnames(m.thermal.analyze) = wind.analyze

# Calculate the change in log values (used for log-log estimations)
log.change.thermal = log(m.thermal.analyze)-log(dt.invariant[, mean(thermal), by=mo]$V1)

# Function to calculate transformation corrected prices for a given model and functional form (used in robustness analysis)

fun.wind.price = function(o.model, specification, bound.type="prediction") {
  # Calculate new prices on the basis of varying condensing and otherwise invariant data 
  dt.invariant.prices = data.table()
  for (j in 1:dim(m.thermal.analyze)[2]) {
    dt.predict$thermal = rep(m.thermal.analyze[,j], n.years)
    dt.predict$log.thermal.hat.a = rep(log.change.thermal[,j], n.years)
    dt.predict$thermal.a = rep(-m.wind.change[,j], n.years)
    dt.predict$thermal.hat.a = rep(-m.wind.change[,j], n.years)
    
    switch(specification,
           log = {
             dt.price.j = data.table(mo=1:12, value=round(exp(predict(o.model, interval = bound.type, level = 0.95, newdata=dt.predict)),2))[, lapply(.SD, mean), by=mo]
             
             # Format table
             col.names = c("mean", "low", "high")
             setnames(dt.price.j, c("mo", col.names))
             setcolorder(dt.price.j, c("low", "mean", "high"))
             
             # Fix log bias in estimation
             dt.price.j$adj = adj.factor.log
             dt.price.j[, (col.names) := lapply(.SD, function(x) x*dt.price.j[["adj"]] ), .SDcols=col.names]
             dt.price.j[, adj:=NULL]
           },
           linear = {
             dt.price.j = data.table(mo=1:12, round(predict(o.model, newdata=dt.predict),2))[, lapply(.SD, mean), by=mo]
             setnames(dt.price.j, "V2", "linear")
           },
           sqrt = {
             dt.price.j = data.table(mo=1:12, round((predict(o.model, newdata=dt.predict))^2,2))[, lapply(.SD, mean), by=mo]
             setnames(dt.price.j, "V2", "sqrt")
             
             # Adjust for sqrt transformation bias
             dt.price.j$adj = adj.factor.sqrt
             dt.price.j[, sqrt := sqrt + dt.price.j[["adj"]]]
             dt.price.j[, adj:=NULL]
           },
           cuberoot = {
             dt.price.j = data.table(mo=1:12, round((predict(o.model, newdata=dt.predict))^3,2))[, lapply(.SD, mean), by=mo]
             setnames(dt.price.j, "V2", "cuberoot")
             
             # Adjust for sqrt transformation bias
             dt.price.j$adj = adj.factor.cuberoot
             dt.price.j[, cuberoot := cuberoot + dt.price.j[["adj"]]]
             dt.price.j[, adj:=NULL]
           })
    
    dt.invariant.prices = rbind(dt.invariant.prices, data.table(wind = wind.analyze[j], dt.price.j))
  }
  
  return(dt.invariant.prices)
}

# Create a copy for prediction
dt.predict = copy(dt.invariant)

# Calculate predicted prices with log-specification
dt.price.forward = fun.wind.price(o.iv2, "log")

# Verify reported numbers in the paper

# drop in prices with 10 TWh wind (ca. 12 %)
1-dt.price.forward[wind==10, mean(mean)]/dt.price.forward[wind==0, mean(mean)]

# drop in prices with 20 TWh or 5 % market share wind (ca. 23 %)
1-dt.price.forward[wind==20, mean(mean)]/dt.price.forward[wind==0, mean(mean)]

# drop in prices with 40 TWh or 10 % market share wind (ca. 40 %)
1-dt.price.forward[wind==40, mean(mean)]/dt.price.forward[wind==0, mean(mean)]

# wind share (5 % and 10 %)
20/mean(dt.fit[, sum(demand),by=yr]$V1)
40/mean(dt.fit[, sum(demand),by=yr]$V1)


# 3.2 Consumer surplus --------------------------------------------------------

# Total annual invariant electricity market expenditures

# Set demand to mean of the period
dt.invariant$demand = rep(round(dt.fit[, mean(demand), by=mo]$V1, 3), n.years)

# Create a data table with prices and demand
dt.expenditure = data.table(melt(data.table(dt.price.forward[, c("wind", "low", "mean", "high"), with=FALSE], demand = dt.invariant[, mean(demand), by=mo]$V1), id=c("wind", "demand"), value.name="price"))

# Calculate expenditures (in MEUR)
dt.expenditure[, expenditure := round(demand*price)]

# Collect results as a table and format as in paper
dt.expenditure.table = dcast(dt.expenditure[, sum(expenditure), by=.(wind, variable)], wind~variable, value.var="V1")

# .. Table 5 Annual prices and expenditures ----

if (VERBOSE > 0) {
  # Table formatting
  table.text = "Annual prices and expenditures"
  
  dt.price.table.exp = dt.price.forward[, lapply(.SD,mean), .SDcols=c("low","mean","high"), by=wind]
  dt.price.table.exp[, share:=sprintf("%.2f %%", wind/400*100)]
  setcolorder(dt.price.table.exp, c("wind", "share", "low", "mean", "high"))
  dt.price.expenditure = cbind(dt.price.table.exp, dt.expenditure.table[, c("low", "mean", "high")])
  
  # Output
  stargazer(dt.price.expenditure, type="text", summary=FALSE, rownames=FALSE, digits=2, title=table.text)
  #stargazer(dt.price.expenditure, type="latex", summary=FALSE, rownames=FALSE, digits=2, title=table.text)
}

# Verify reported numbers in the paper

# Drop in expenditures with 20 TWh wind (23 %)
1-dt.expenditure[wind==20&variable=="mean", mean(expenditure)]/dt.expenditure[wind==0&variable=="mean", mean(expenditure)]

# Drop in expenditures with 40 TWh wind (41 %)
1-dt.expenditure[wind==40&variable=="mean", mean(expenditure)]/dt.expenditure[wind==0&variable=="mean", mean(expenditure)]

# Consumer willingness to pay for MWh of wind generation

# Calculate shares of average demand by area
dt.area.demand = dt.fit[, c("demand.dk", "demand.fi", "demand.no", "demand.se"), with=FALSE]
n.area.shares = colSums(dt.area.demand)/sum(dt.area.demand)

# Allocate expenditure according to the average demand in each area
dt.expend.mean = t(round(as.matrix(dt.expenditure.table$mean) %*% n.area.shares))
dt.expend.mean = rbind(dt.expend.mean, colSums(dt.expend.mean))

# Change in consumer expenditure 
dt.expend.diff.mean = t(diff(t(dt.expend.mean)))

# Relative change in consumer expenditure (expenditure divided by the addition of wind, start from 0)
dt.expend.relative.mean = (sapply(seq(1:5), function(j) round(-dt.expend.diff.mean[,j]/10,0)))
dt.expend.relative = as.data.table(dt.expend.relative.mean)

colnames(dt.expend.relative) = c("10", "20", "30", "40", "50")
rownames(dt.expend.relative) = c("Denmark", "Finland", "Norway", "Sweden", "Total")

# .. Table 6 Willingness to pay for wind ----

if (VERBOSE > 0) {
  # Table formatting
  table.text = "Willingness to pay for wind"
  
  # Output
  stargazer(dt.expend.relative, type="text", summary=FALSE, rownames=F, digits=0, title=table.text)
  #stargazer(dt.expend.relative, type="latex", summary=FALSE, rownames=F, digits=0, title=table.text)
}

# 3.3 Producer surplus by technology ------------------------------------------

# THERMAL and WIND volumes change by the scenario
thermal.invariant = melt(m.thermal.analyze)$value
wind.invariant = melt(m.wind.analyze)$value

# HYDRO and NUCLEAR are assumed to be the same over increasing wind
hydro.invariant = rep(dt.fit[, mean(hydro), by=mo]$V1, n.scenarios)
nuclear.invariant = rep(dt.fit[, mean(nuclear), by=mo]$V1, n.scenarios)

# Collect volumes
dt.volumes = data.table(mo=1:12, bound=rep(c("low", "mean", "high"), each=n.scenarios*12), scenario=rep(rep(wind.analyze, each=12),3), thermal.raw=rep(thermal.invariant,3), hydro.raw=rep(hydro.invariant,3), nuclear=rep(nuclear.invariant,3), wind=rep(wind.invariant,3))
setkey(dt.volumes, scenario, mo)

# Get prediction interval for thermal volumes 
# (low and high prices driven by changes in the division of labor between thermal and hydro)
dt.predict$wind.a = 0
dt.predict$temp.a = 0
dt.predict$inflow.a = 0
dt.predict$past.inflows = dt.fit[, mean(past.inflows)]

dt.thermal = data.table(dt.fit[, .(mo)], predict(o.thermal, interval="prediction", newdata=dt.predict))[, lapply(.SD, mean), by=mo]
dt.thermal[, low := lwr-fit]
dt.thermal[, mean := 0]
dt.thermal[, high := upr-fit]

dt.thermal = melt(dt.thermal[, .(mo, low, mean, high)], id="mo", value.name="delta", variable.name="bound")

# Combine data
dt.volumes = dt.volumes[dt.thermal, on=c("mo","bound")]

# Update volumes based on high/low price bound
dt.volumes[, thermal:=thermal.raw+delta]
dt.volumes[, hydro:=hydro.raw-delta]

# Get prices by scenario and bound
dt.prices = melt(dt.price.forward[, .(mo, scenario=wind, low, mean, high)], id=c("mo", "scenario"), value.name="price", variable.name="bound")

# Combine data
dt.volumes = dt.volumes[dt.prices, on=c("scenario","mo","bound")]

# Column names
type.names = c("thermal", "hydro", "nuclear", "wind")

# calculate revenue
dt.revenue = dt.volumes[, lapply(.SD, function(x) x*price), .SDcols=type.names, by=.(scenario, mo, bound)]
dt.revenue[scenario=="0"&bound=="mean", sum(hydro)]

# subtract marginal cost from thermal and wind
dt.thermal.cost = data.table(yr=rep(dt.fit[, unique(yr)], each=n.scenarios*3*12), dt.volumes[, .(mo, bound, scenario, thermal)])

# add fixed effects
dt.thermal.cost[, beta.0m := o.iv2$coefficients[paste("eq1_",month.abb[mo], sep="")]]
dt.thermal.cost[, beta.0y := o.iv2$coefficients[paste("eq1_year",yr, sep="")]]
dt.thermal.cost[is.na(beta.0m), beta.0m := 0]
dt.thermal.cost[is.na(beta.0y), beta.0y := 0]

# Add covariates
dt.thermal.cost[, beta.0:=beta.0m+beta.0y+o.iv2$coefficients[1]+o.iv2$coefficients["eq1_coal2010"]*dt.invariant$coal2010+o.iv2$coefficients["eq1_euets2010"]*dt.invariant$euets2010]
dt.thermal.cost[, beta.1:=o.iv2$coefficients["eq1_thermal"]]

# Calculate marginal costs = prices (to verify)
dt.thermal.cost[, price:=exp(beta.0+beta.1*thermal)]

# Calculate cost (integrated from above)
dt.thermal.cost[, thermal.cost:=1/beta.1*(exp(beta.0+beta.1*thermal)-exp(beta.0))]

dt.costs = dt.thermal.cost[, .(thermal=mean(thermal.cost)), by=.(scenario, mo, bound)]
dt.costs$hydro = 0
dt.costs$nuclear = 0
dt.costs$wind = dt.volumes$wind*10

# Calculate surplus
dt.surplus = melt(data.table(dt.revenue[, .(scenario, mo, bound)], dt.revenue[, c(type.names), with=F]-dt.costs[, c(type.names), with=F]), id=c("scenario", "mo", "bound"), value.name = "surplus")
dt.surplus[, variable:=as.character(variable)]

# Calculate change in surplus
dt.base = dt.surplus[scenario==0, .(base=surplus), by=.(bound, variable, mo)]
dt.surplus = dt.surplus[dt.base, on=c("bound", "variable", "mo")]
dt.surplus[, surplus.change:=surplus-base]
setkey(dt.surplus, variable, scenario, bound, mo)

# Collect results as a table and format as in paper
dt.surplus.table.mean = t(dcast(dt.surplus[bound=="mean", round(sum(surplus.change)), by=.(scenario, variable)], scenario~variable, value.var="V1"))[2:5, ]
dt.surplus.table.mean = rbind(dt.surplus.table.mean, colSums(dt.surplus.table.mean))

dt.surplus.table = as.data.table(dt.surplus.table.mean)
colnames(dt.surplus.table) = paste(wind.analyze, "TWh")

# .. Table 7 Surplus change by technology ----

if (VERBOSE > 0) {
  # Table formatting
  table.text = "Surplus change by technology"
  
  # Output
  stargazer(dt.surplus.table, type="text", summary=FALSE, rownames=FALSE, digits=0, font.size="tiny", title=table.text)
  #stargazer(dt.surplus.table, type="latex", summary=FALSE, rownames=FALSE, digits=0, font.size="tiny", title=table.text)
}

# Verify reported numbers in the paper

# hydro revenue with zero wind (8.78 billion euro)
round(dt.revenue[scenario=="0"&bound=="mean", sum(hydro)]/1000,2)

# percentage of hydro output of the mean annual demand (55 % vs. ca. 50 percent)
dt.fit[, mean(hydro)*12]/dt.fit[, mean(demand)*12]


# 3.4 Policy context ----------------------------------------------------------

# Verify reported numbers in the paper 

# annual mean price at 50 TWh of wind (22 EUR/MWh)
dt.price.forward[wind==50, mean(mean)]

# Emission calculus

# Get prediction interval for thermal quantities
dt.predict.thermal = dt.fit[, .(month, year)]
dt.predict.thermal$wind.a = 0
dt.predict.thermal$temp.a = 0
dt.predict.thermal$inflow.a = 0
dt.predict.thermal$past.inflows = dt.fit[, mean(past.inflows)]

# Collect data
dt.thermal.e = data.table(mo=1:12, predict(o.thermal, interval="prediction", newdata = dt.predict.thermal))
setnames(dt.thermal.e, c("fit"), c("mean"))

# Take mean over months
dt.thermal.pred = dt.thermal.e[, lapply(.SD, mean), .SDcols=c("mean", "lwr", "upr"), by=mo]

# Subtract from thermal output the wind scenarios
dt.qth = as.data.table(melt(lapply(dt.thermal.pred[, 2:4], function(x) x - m.wind.change)))
setnames(dt.qth, c("mo", "wind", "thermal", "variable"))
dt.qth[variable=="lwr", variable:="low"]
dt.qth[variable=="upr", variable:="high"]

# Collect data
dt.emissions = data.table(data.table(dt.expenditure[, .(variable=as.character(variable), wind, price)])[order(variable, wind)], thermal=dt.qth[order(variable, wind)]$thermal)
dt.emissions[, mo:=rep(1:12, n.scenarios*3)]

# Collect coefficients from the IV regression
dt.emissions$beta.1 = o.iv2$coefficients["eq1_thermal"]
dt.emissions$beta.2 = o.iv2$coefficients["eq1_euets2010"]

# Calculate emissions (in MtCO2)
dt.emissions[, emissions:=beta.2*(1/beta.1*price)]

# Collect results as a table and format as in paper
dt.emissions.table = dcast(dt.emissions[, round(sum(emissions),1), by=.(wind, variable)], wind~variable, value.var="V1")
dt.thermal.table = dcast(dt.emissions[, round(sum(thermal)), by=.(wind, variable)], wind~variable, value.var="V1")
setcolorder(dt.thermal.table, c("wind", "low", "mean", "high"))
setcolorder(dt.emissions.table, c("wind", "low", "mean", "high"))

# .. Table 8 Annual emissions ----

if (VERBOSE > 0) {
  # Table formatting
  table.text = "Annual emissions"
  
  dt.thermal.emission = cbind(dt.thermal.table, dt.emissions.table[, c("low", "mean", "high")])
  
  # Output
  stargazer(dt.thermal.emission, type="text", summary=FALSE, rownames=FALSE, digits=1, title=table.text)
  #stargazer(dt.thermal.emission, type="latex", summary=FALSE, rownames=FALSE, digits=1, title=table.text)
}


# .. Table 9 Reduction in social cost ----

if (VERBOSE > 0) {
  # Collect emissions
  emissions = unname(unlist(dt.thermal.emission[, 6]))
  emission.reductions = emissions-emissions[1]
  
  # Lower value for CO2 reductions (Nordhaus, PNAS 2017)
  # dollar value for 2015 are 7, 32, 77 USD/tCO2, USD/EUR rate used is 1.3257
  cost.carbon = c(5.3, 24.1, 58.1)
  
  # Damages from local pollutants from coal generation (Muller, Mendelsohn and Nordhaus, 2011)
  # calculated from GED/VA values of 0.78, 2.20, and 3.63 to GED/kWh values of 0.010, 0.028, and 0.046 USD/kWh
  # converted from year 2000 USD to year 2010 USD and to EUR with rate 1.3257
  
  cost.health = c(10.5, 29.7, 49.0)
  
  # Above values are for the U.S.
  # Mean value for Europe from Scasny, Massetti, Melichar and Carrara (2015)
  
  cost.health[2] = 17.60
  
  # Collect all data for social costs
  dt.social.cost = as.data.table(cbind(outer(emissions, cost.carbon), outer(emissions, cost.health), outer(emissions, cost.carbon+cost.health)))
  setnames(dt.social.cost, paste(rep(c("Carbon", "Health", "Total"), each=3), rep(c("Low", "Mean", "High"),3), sep="."))
  
  # Format table
  dt.social.cost$wind = wind.analyze
  dt.social.cost$emissions = as.character(round(emissions,1))
  setcolorder(dt.social.cost, c("wind", "emissions"))
  
  table.text = "Reduction in social cost"
  
  stargazer(dt.social.cost, summary=F, type="text", digits=0, digit.separator=",", rownames = F, title=table.text)
  #stargazer(dt.social.cost, summary=F, type="latex", digits=0, digit.separator=",", rownames = F, title=table.text)
}

# Verify reported numbers in the paper 

# reduction of carbon emissions (17 MtCO2)
dt.thermal.emission[wind==0, 6]-dt.thermal.emission[wind==50, 6]

# or 47 %
(dt.thermal.emission[wind==0, 6]-dt.thermal.emission[wind==50, 6])/dt.thermal.emission[wind==0, 6]

# reduction of carbon costs (409.7 = around 400 MEUR)
dt.social.cost[wind==0, Carbon.Mean]-dt.social.cost[wind==50, Carbon.Mean]

# reduction of healt costs (299.2 = around 300 MEUR)
dt.social.cost[wind==0, Health.Mean]-dt.social.cost[wind==50, Health.Mean]

# add to the willingness-to-pay (14.2 EUR/MWh)
(dt.social.cost[wind==0, Total.Mean]-dt.social.cost[wind==50, Total.Mean])/50

# range in willingness-to-pay (5.4-36.4 EUR/MWh)
(dt.social.cost[wind==0, Total.Low]-dt.social.cost[wind==50, Total.Low])/50
(dt.social.cost[wind==0, Total.High]-dt.social.cost[wind==50, Total.High])/50

# .. Table 10 Impact of wind to EU ETS passthrough ----

if (VERBOSE > 0) {
  # Set scenarios for EU ETS prices
  eu.ets = c(0, 5, 10, 20, 50)
  
  # Calculate new prices with varying EU ETS price
  dt.predict = copy(dt.invariant)
  dt.price.mc = data.table()
  for (i in 1:length(eu.ets)) {
    dt.predict$euets2010 = eu.ets[i]
    
    dt.price.mc = rbind(dt.price.mc, fun.wind.price(o.iv2, "log")[, .(wind, euets=eu.ets[i], spot=mean)])
  }
  dt.predict = copy(dt.invariant)
  
  # Arrange and report results
  dt.mc.table = data.table(dcast(dt.price.mc[, .(price=mean(spot)), by=.(euets,wind)], wind~euets, value.var="price"))
  
  setnames(dt.mc.table, as.character(eu.ets), paste("euets", eu.ets, sep=""))
  dt.mc.table[, change.abs:=(euets50-euets0)]
  
  # Table formatting
  table.text = "Impact of wind to EU ETS passthrough"
  
  stargazer(dt.mc.table, type="text", digits=2, summary=FALSE, rownames=FALSE, title=table.text)
  #stargazer(dt.mc.table, type="latex", digits=2, summary=FALSE, rownames=FALSE, title=table.text)
}


# 4 Robustness ----------------------------------------------------------------

# Market structure

# Function to calculate marginal revenues

fun.mr = function(share, scen) {
  # Reservation price curve for the supply is p(q) = exp(b0-b1*q)
  
  # Get coefficients for the supply curve from the IV estimation above, incl. variation over months
  b1 = o.iv$coefficients["thermal.a"]
  dt.mr = data.table(mo=1:12, b0=o.iv$coefficients[1]+o.iv$coefficients[month.names])
  dt.mr[is.na(b0), b0:=o.iv$coefficients[1]]
  
  # Qw is the change of wind in the scenario, = change in thermal by our assumption, per month
  dt.mr$qw = m.wind.change[, scen]
  
  # Qh is the invariant supply from hydro per month
  dt.mr$qh = dt.invariant[, mean(hydro), by=mo]$V1
  
  # Qth is the thermal volume (invariant + wind change) at which the elasticity is calculated
  dt.mr$qth = m.thermal.analyze[, scen]
  
  # Calculate elasticities
  dt.mr[, delta.q := 0.01]
  dt.mr[, p:=exp(b0-b1*qw)]
  dt.mr[, p1:=exp(b0+b1*(qw-delta.q/2))]
  dt.mr[, p2:=exp(b0+b1*(qw+delta.q/2))]
  dt.mr[, delta.p:=p1-p2]
  dt.mr[, ela:=(delta.p/p)/(delta.q/qth)]
  
  # Calculate marginal revenu for a hydro player with a given share
  dt.mr[, mr:=round(p+ela*share*qh,2)]
  
  return(dt.mr$mr)
}

# Calculate marginal revenues for a hydro plant with 15 % market share in each counterfactual
dt.marginal = data.table(sapply(1:n.scenarios, function(j) fun.mr(0.15, j)))

# .. Table 11 Impact of wind on marginal revenues in seasons ----

if (VERBOSE > 0) {
  # Format Table
  table.text = "Impact of wind on marginal revenues in seasons"
  
  rownames(dt.marginal) = c(month.abb)
  setnames(dt.marginal, as.character(c(wind.analyze)))
  
  # Output
  stargazer(dt.marginal, type="text", rownames=T, summary=F, digits=2, title=table.text)
  #stargazer(dt.marginal, type="latex", rownames=T, summary=F, digits=2, title=table.text)
}



# APPENDIX A: Data ------------------------------------------------------------

# Demand

# ... Table A.1 Demand regression ----

if (VERBOSE > 1) {
  # Add instrumented spot estimate
  o.demand.l[[6]] = ivreg(demand ~ spot2010.a + temp.a + month + year | coal2010.a + euets2010.a + wind.a + inflow.a + past.inflows + temp.a + month + year, data=dt.fit)
  
  # Estimate first stage separately for the F Statistics report below
  o.demand.fs = lm(spot2010.a ~ coal2010.a + euets2010.a + wind.a + inflow.a + past.inflows + temp.a + month + year, data=dt.fit)
  
  # Demand has been fitted above in o.demand.l, pick F-test values
  add.lines = list(c("F Statistic", sapply(o.demand.l[1:5], function(o) round(summary(o)$f[1],1)), round(summary(o.demand.fs)$f[1],1)))
  robust.se = lapply(o.demand.l, function(o) fun.robust(o)[,2])
  
  # Format table
  covariate.names = names(o.demand.l[[4]]$coefficients)
  order = c(covariate.names[1:2], "spot2010.a", month.names[c(1:3,5:12)])
  covariate.labels = c("Temperature", "Spot price", month.abb[c(1:3,5:12)], paste("Year ", (start.year+1):end.year, sep=""))
  table.text = "Demand regression"
  
  # Output
  stargazer(o.demand.l,type="text", single.row=T, omit.stat=c("F", "ser"), report=c("vsc"), covariate.labels = covariate.labels, add.lines=add.lines, se=robust.se, order=order, font.size="scriptsize", title=table.text)
  #stargazer(o.demand.l,type="latex", single.row=T, omit.stat=c("F", "ser"), report=c("vsc"), covariate.labels = covariate.labels, add.lines=add.lines, se=robust.se, order=order, font.size="scriptsize", title=table.text)
}

# Verify reported numbers in the paper:
# impact of demand elasticity to prices

# Collect data
dt.predict$thermal.a = dt.fit$thermal.a
dt.predict$wind.a = dt.fit$wind.a
dt.predict$spot2010.a = dt.fit$spot2010.a
dt.predict$temp.a = dt.fit$temp.a
dt.predict$coal2010.a = dt.fit$coal2010.a
dt.predict$euets2010.a = dt.fit$euets2010.a
dt.predict$inflow.a = dt.fit$inflow.a
dt.predict$past.inflows = dt.fit$past.inflows
dt.predict$month = dt.fit$month
dt.predict$year = dt.fit$year

# Create invariant data set
dt.invariant$past.inflows = rep(dt.fit[, mean(past.inflows), by=yr]$V1, each=12)
dt.invariant$thermal.a = 0
dt.invariant$coal2010.a = 0
dt.invariant$euets2010.a = 0
dt.invariant$wind.a = 0
dt.invariant$temp.a = 0
dt.invariant$inflow.a = 0

# Loop through counterfactuals
dt.result = data.table()
for (j in 1:dim(m.thermal.analyze)[2]) {
  # Update thermal quantity
  dt.predict$thermal.a = rep(-m.wind.change[,j], n.years)

  # Inelastic demand estimate
  dt.predd = data.table(mo=1:12, value=round(predict(o.demand.l[[4]], newdata=dt.predict),2))
  dt.predict$demand.hat = dt.predd$value
  
  # Estimate prices with inelastic demand
  dt.price.1 = data.table(mo=1:12, value=round(exp(predict(o.iv, interval = bound.type, level = 0.95, newdata=dt.predict)),2)) #[, lapply(.SD, mean), by=mo]
  
  # Take baseline price
  dt.price.base = data.table(mo=1:12, value=round(exp(predict(o.iv, interval = bound.type, level = 0.95, newdata=dt.invariant)),2)) #[, lapply(.SD, mean), by=mo]
  
  # Adjust the price shock
  dt.predict$spot2010.a = dt.price.1$value-dt.price.base$value
  
  # Elastic demand estimate
  dt.predd = data.table(mo=1:12, value=round(predict(o.demand.l[[6]], interval = bound.type, level = 0.95, newdata=dt.predict),2))
  dt.predict$demand.price = dt.predd$value
  
  # Update thermal shock
  dt.predict[, thermal.a:=thermal.a+demand.price-demand.hat]
  
  # Estimate prices with demand elasticity
  dt.price.2 = data.table(mo=1:12, value=round(exp(predict(o.iv, interval = bound.type, level = 0.95, newdata=dt.predict)),2))[, lapply(.SD, mean), by=mo]
  
  # Save data
  dt.result = rbind(dt.result, data.table(dt.price.1, elastic=dt.price.2$value, scenario=wind.analyze[j]))
}

# Collect data 
dt.demand.elasticity = dt.result[, lapply(.SD, mean), .SDcols=c("value","elastic"), by=scenario]

# Reduction of the high price (around 1 EUR/MWh)
dt.demand.elasticity[scenario==0, value-elastic]

# Increase of the low price (around 0.8 EUR/MWh)
dt.demand.elasticity[scenario==50, value-elastic]


# Seasonal profiles

# .. Figure A.1 Seasonal profiles ----

if (VERBOSE > 1) {
  # Collect data
  dt.pic.hydro = data.table(mo=1:12, mean=dt.fit[, mean(hydro), by=mo]$V1, sd=dt.fit[, sd(hydro), by=mo]$V1)
  dt.pic.inflow = data.table(mo=1:12, mean=dt.fit[, mean(inflow), by=mo]$V1, sd=dt.fit[, sd(inflow), by=mo]$V1)
  dt.pic.thermal = data.table(mo=1:12, mean=dt.fit[, mean(thermal), by=mo]$V1, sd=dt.fit[, sd(thermal), by=mo]$V1)
  dt.pic.wind = data.table(mo=1:12, mean=dt.fit[, mean(wind), by=mo]$V1, sd=dt.fit[, sd(wind), by=mo]$V1)
  dt.pic.nuclear = data.table(mo=1:12, mean=dt.fit[, mean(nuclear), by=mo]$V1, sd=dt.fit[, sd(nuclear), by=mo]$V1)
  dt.pic.demand = data.table(mo=1:12, mean=dt.fit[, mean(demand), by=mo]$V1, sd=dt.fit[, sd(demand), by=mo]$V1)
  
  # Create figures
  p.supply.hydro = figure.format(dt.pic.hydro, "Month", "TWh/month", "HYDRO")
  p.supply.inflow = figure.format(dt.pic.inflow, "Month", "TWh/month", "INFLOW")
  p.supply.thermal = figure.format(dt.pic.thermal, "Month", "TWh/month", "THERMAL")
  p.supply.wind = figure.format(dt.pic.wind, "Month", "TWh/month", "WIND")
  p.supply.nuclear = figure.format(dt.pic.nuclear, "Month", "TWh/month", "NUCLEAR")
  p.supply.total = figure.format(dt.pic.demand, "Month", "TWh/month", "DEMAND")
  
  # Save if specified
  if (SAVE.FIGURES==TRUE) {
    pdf(file="p_seasonal.pdf", width=3.94, height=5.91, pointsize=7)
    multiplot(p.supply.hydro+theme_bw(base_family="serif", base_size=7), p.supply.thermal+theme_bw(base_family="serif", base_size=7), p.supply.wind+theme_bw(base_family="serif", base_size=7), p.supply.inflow+theme_bw(base_family="serif", base_size=7), p.supply.nuclear+theme_bw(base_family="serif", base_size=7), p.supply.total+theme_bw(base_family="serif", base_size=7), cols=2)
    dev.off()
  }
  
  # Output
  multiplot(p.supply.hydro, p.supply.thermal, p.supply.wind, p.supply.inflow, p.supply.nuclear, p.supply.total, cols=2)
}


# Variation in covariates

# .. Figure A.2 Within fixed effects variation in covariates ----

if (VERBOSE > 1) {
  # Collect data and create figures
  p.chk.1 = ggplot(dt.fit, aes(mo,inflow.a))+geom_point(size=1)+geom_smooth(method="glm", se=F)+theme_bw()+scale_x_continuous(breaks=seq(1,12), labels=month.abb)+xlab("")+ylab("Inflow, TWh")#+ylim(-25,25)
  p.chk.2 = ggplot(dt.fit, aes(mo,temp.a))+geom_point(size=1)+geom_smooth(method="glm", se=F)+theme_bw()+scale_x_continuous(breaks=seq(1,12), labels=month.abb)+xlab("")+ylab("Temperature")+ylim(-5,5)
  p.chk.3 = ggplot(dt.fit, aes(mo,coal2010.a))+geom_point(size=1)+geom_smooth(method="glm", se=F)+theme_bw()+scale_x_continuous(breaks=seq(1,12), labels=month.abb)+xlab("")+ylab("Coal, EUR/t")#+ylim(-25,25)
  p.chk.4 = ggplot(dt.fit, aes(mo,past.inflows.a))+geom_point(size=1)+geom_smooth(method="glm", se=F)+theme_bw()+scale_x_continuous(breaks=seq(1,12), labels=month.abb)+xlab("")+ylab("Past inflow, TWh")+ylim(-50,50)
  p.chk.5 = ggplot(dt.fit, aes(mo,wind.a))+geom_point(size=1)+geom_smooth(method="glm", se=F)+theme_bw()+scale_x_continuous(breaks=seq(1,12), labels=month.abb)+xlab("")+ylab("Wind, TWh")+ylim(-2,2)
  p.chk.6 = ggplot(dt.fit, aes(mo,euets2010.a))+geom_point(size=1)+geom_smooth(method="glm", se=F)+theme_bw()+scale_x_continuous(breaks=seq(1,12), labels=month.abb)+xlab("")+ylab("EU ETS, EUR/tCO2")+ylim(-15,15)
  
  # Save if specified
  if (SAVE.FIGURES==TRUE) {
    pdf(file="p_residuals.pdf", width=4.76, height=5.91, pointsize=8)
    multiplot(p.chk.1+theme_bw(base_family="serif", base_size = 8),p.chk.2+theme_bw(base_family="serif", base_size = 8), p.chk.3+theme_bw(base_family="serif", base_size = 8),p.chk.4+theme_bw(base_family="serif", base_size = 8),p.chk.5+theme_bw(base_family="serif", base_size = 8),p.chk.6+theme_bw(base_family="serif", base_size = 8),cols=2)
    dev.off()
  }
  
  # Output  
  multiplot(p.chk.1, p.chk.2, p.chk.3, p.chk.4, p.chk.5, p.chk.6, cols=2)  
}

# Annual trends in other quantities

# .. Figure A.3 Annual trend in other quantities ----

if (VERBOSE > 1) { 
  # Collect data
  dt.yr.trend = na.omit(data.table(yr=years,Thermal=12*o.thermal$coefficients[year.names], Wind=12*exp(o.fe.wind$coefficients[1]+o.fe.wind$coefficients[year.names])))
  dt.pic = melt(dt.yr.trend, id="yr")
  dt.pic = rbind(dt.pic, data.table(variable=dt.pic[, unique(variable)], yr=start.year, value=0))
  
  color.pal = brewer.pal(5, "Dark2")
  
  # Format plot
  p.trend.2 = ggplot(dt.pic, aes(yr, value, linetype=variable, color=variable))+geom_point(aes(shape=variable), size=0.7)+stat_smooth(method="loess", size=0.5)
  p.trend.2 = p.trend.2+ theme_bw(base_family="serif", base_size = 12) + theme(legend.title=element_blank())  
  p.trend.2 = p.trend.2 + scale_x_continuous(breaks=seq(start.year, end.year, by=2)) + ylim(c(-60,60)) + scale_color_manual(values=color.pal[1:2]) + scale_shape_manual(values=c(16,17))
  p.trend.2 = p.trend.2 + xlab("") + ylab("TWh/a") + ggtitle("")
  
  # Collect data
  dt.yr.trend = na.omit(data.table(yr=years,Demand=12*o.demand.l[[4]]$coefficients[year.names],Hydro=12*o.hydro.q$coefficients[year.names],Nuclear=12*o.nuclear.q$coefficients[year.names]))
  dt.pic = melt(dt.yr.trend, id="yr")
  dt.pic = rbind(dt.pic, data.table(variable=dt.pic[, unique(variable)], yr=start.year, value=0))
  
  # Format plot
  p.trend.other = ggplot(dt.pic, aes(yr, value, linetype=variable, color=variable))+geom_point(aes(shape=variable), size=0.7)+stat_smooth(method="loess", size=0.5)
  p.trend.other = p.trend.other + theme_bw(base_family="serif", base_size = 12) + theme(legend.title=element_blank()) 
  p.trend.other = p.trend.other + scale_x_continuous(breaks=seq(start.year, end.year, by=2)) + ylim(c(-60,60)) + scale_color_manual(values=color.pal[3:5]) + scale_shape_manual(values=c(15,18,1))
  p.trend.other = p.trend.other + xlab("") + ylab("TWh/a") + ggtitle("")
  
  # Output
  if (SAVE.FIGURES==TRUE) {
    pdf(file="p_trend_other.pdf", width=5.12, height=2.62, pointsize=9)
    multiplot(p.trend.2 +theme_bw(base_family="serif", base_size = 8) + theme(legend.title=element_blank(), legend.position=c(0.7, 0.87), legend.direction="horizontal"), p.trend.other +theme_bw(base_family="serif", base_size = 8) + theme(legend.title=element_blank(), legend.position=c(0.5, 0.87), legend.direction="horizontal") + ylab(""), cols=2)
    dev.off()
  }
  
  print(p.trend.other)
}


# Spot Market Volatility

# Define specifications (first without fixed monthly effects)

specvola.l = list()
specvola.l[[1]] = c("wind.a + month + year")
specvola.l[[2]] = c("wind.a + temp.a + month + year")
specvola.l[[3]] = c("wind.a + temp.a + inflow.a + past.inflows.a + month + year")
specvola.l[[4]] = c("wind.a + temp.a + inflow.a + past.inflows.a + coal2010.a + euets2010.a + month + year")

n.volas = length(specvola.l)

# Create formulas for fitting

fm.vola.l = lapply(1:n.volas, function(i) as.formula(paste("spot.vola.a ~ ", specvola.l[[i]])))

# Fit models with standard methods

o.vola.l = lapply(1:n.volas, function(i) lm(fm.vola.l[[i]], data=dt.fit))

# .. Figure A.4 Spot volatility ----

if (VERBOSE > 1) {  
  # Format volatility figures
  p.vola.spot = ggplot(dt.fit,aes(as.Date(date), spot.vola))+geom_point(size=0.2)+geom_smooth(color="black", method="loess")
  p.vola.spot = p.vola.spot + scale_x_date(breaks = date_breaks("2 years"), labels= date_format("%Y")) + scale_y_continuous(labels = comma, limits=c(0,25))
  p.vola.spot = p.vola.spot + theme_bw(base_family="sans", base_size = 12) 
  p.vola.spot = p.vola.spot + xlab(" ") + ylab("Monthly spot volatility, %")
  
  p.vola.wind = ggplot(dt.fit, aes(wind, spot.vola))+geom_point(size=0.2)+geom_smooth(color="black", method="loess")
  p.vola.wind = p.vola.wind + scale_y_continuous(labels = comma, limits=c(0,25)) 
  p.vola.wind = p.vola.wind + theme_bw(base_family="sans", base_size = 12) 
  p.vola.wind = p.vola.wind + xlab("Wind, TWh") + ylab("Monthly spot volatility, %")
  
  # Save if specified
  if (SAVE.FIGURES==TRUE) {
    pdf(file="p_volatility.pdf", width=6, height=3, pointsize=8)
    multiplot(p.vola.spot+theme_bw(base_family="serif", base_size = 7), p.vola.wind+theme_bw(base_family="serif", base_size = 7), cols=2)
    dev.off()
  }
  
  # Output
  multiplot(p.vola.spot, p.vola.wind, cols=2)
}

# .. Table A.2 Regression of monthly volatility ----

if (VERBOSE > 1) {  
  month.fe = c("Month FE", "No", "No", "No", "Yes", "Yes")
  year.fe = c("Year FE", "No", "No", "No", "No", "Yes")
  
  # Collect F-statistics
  add.lines.vola = list(month.fe, year.fe, c("F Statistic", sapply(o.vola.l, function(o) round(summary(o)$f[1],1))))
  
  # Report robust standard error values
  robust.se.vola = lapply(o.vola.l, function(o) fun.robust(o)[,2])
  
  # Table formatting
  table.text = "Regression of monthly volatility"
  order = c("Wind", "Temperature", "Inflow", "Past inflows", "Coal price", "Carbon price")
  covariate.labels = c("Wind", "Temperature", "Inflow", "Past inflows", "Coal price", "Carbon price")
  
  stargazer(o.vola.l, single.row=TRUE, align=F, type="text", font.size="small", report=c("vsc"),  omit.stat=c("f", "ser"), digits=2, omit=c("Constant", month.names, year.names), add.lines=add.lines.vola, se=robust.se.vola, order=order, dep.var.caption="", dep.var.labels.include=FALSE, covariate.labels = covariate.labels, title=table.text)
  #stargazer(o.vola.l, single.row=TRUE, align=F, type="latex", font.size="small", report=c("vsc"),  omit.stat=c("f", "ser"), digits=2, omit=c("Constant", month.names, year.names), add.lines=add.lines.vola, se=robust.se.vola, order=order, dep.var.caption="", dep.var.labels.include=FALSE, covariate.labels = covariate.labels, title=table.text)
}

# APPENDIX C: Supplementary results -------------------------------------------

# Quantity regressions

# .. Table C.1 Thermal output regression ----

if (VERBOSE > 1) {
  # Calculate robust standard errors
  robust.se.wind.thermal = lapply(o.thermal.l, function(o) fun.robust(o)[,2])
  
  # Collect F-statistics (calculated earlier)
  add.lines.wind.thermal = list(c("F Statistic", sapply(o.thermal.l, function(o) round(summary(o)$f[1],1))))
  
  # Table formatting
  table.text = "Thermal output regression"
  covariate.labels = c("Wind", "Temperature", "Inflow", "Past inflows", "Coal price", "Carbon price")
  
  # Output
  stargazer(o.thermal.l, type="text", single.row=T, omit.stat=c("F", "ser"), report=c("vsc"), add.lines=add.lines.wind.thermal, se=robust.se.wind.thermal, dep.var.caption="", dep.var.labels.include=FALSE, omit=c("Constant", month.names, year.names), covariate.labels = covariate.labels, title=table.text)
  #stargazer(o.thermal.l, type="latex", single.row=T, omit.stat=c("F", "ser"), report=c("vsc"), add.lines=add.lines.wind.thermal, se=robust.se.wind.thermal, dep.var.caption="", dep.var.labels.include=FALSE, omit=c("Constant", month.names, year.names), covariate.labels = covariate.labels, title=table.text)
}


# Range of counterfactuals

# .. Figure C.1 Actual and counterfactual thermal productions ----

if (VERBOSE > 1) {
  # Collect data
  dt.pic = data.table(melt(m.thermal.analyze), spot=dt.price.forward$mean)
  dt.pic[, id:=paste("Wind", Var2)]
  dt.pic[, Var1:=NULL]
  dt.pic[, Var2:=NULL]
  setnames(dt.pic, "value", "thermal")
  
  # Format figure
  dt.pic = rbind(dt.pic, data.table(thermal=dt.fit$thermal, spot=dt.fit$spot2010, id="Actual"))
  p.interpolate = ggplot(dt.pic, aes(thermal, spot))+geom_point(aes(shape=id,size=id),fill=2)
  p.interpolate = p.interpolate+scale_shape_manual(values=c(21,17,15,3,7,8,10))+scale_size_manual(values=c(1,rep(2,6)))
  p.interpolate = p.interpolate+labs(x="Thermal output, TWh/month", y="Price, EUR/MWh")
  p.interpolate = p.interpolate + theme_bw(base_family="sans", base_size = 12) 
  
  # Save if specified
  if (SAVE.FIGURES==TRUE) {
    pdf(file="p_interpolate.pdf", width=6, height=3, pointsize=8)
    print(p.interpolate+theme_bw(base_family="serif", base_size=9))
    dev.off()
  }
  
  # Output
  print(p.interpolate)
}


# Comparison to historical emissions

# .. Figure C.2 Actual and modeled emissions ----

if (VERBOSE > 1) {
  # Collect data from the IV regression
  dt.hist.emissions = dt.fit[, .(yr, mo, thermal, price=spot2010, coal2010, euets2010)]
  dt.hist.emissions$beta.0const = o.iv2$coefficients[1]
  dt.hist.emissions$beta.0mo = rep(o.iv2$coefficients[paste("eq1_",month.abb, sep="")], n.years)
  dt.hist.emissions[is.na(beta.0mo), beta.0mo := 0]
  dt.hist.emissions$beta.0yr = rep(o.iv2$coefficients[paste("eq1_",year.names, sep="")], each=12)
  dt.hist.emissions[is.na(beta.0yr), beta.0yr := 0]
  dt.hist.emissions[, beta.0coal := o.iv2$coefficients["eq1_coal2010"]*coal2010]
  dt.hist.emissions[, beta.0euets := o.iv2$coefficients["eq1_euets2010"]*euets2010]
  dt.hist.emissions[, beta.0:=beta.0const+beta.0mo+beta.0yr+beta.0coal+beta.0euets]
  dt.hist.emissions[, c("beta.0const", "beta.0mo", "beta.0yr", "beta.0coal", "beta.0euets"):=NULL]
  dt.hist.emissions$beta.1 = o.iv2$coefficients["eq1_thermal"]
  dt.hist.emissions$beta.2 = o.iv2$coefficients["eq1_euets2010"]
  
  # Sample values for beta_1 and beta_2
  beta.1s = rnorm(1000000,mean=o.iv2$coefficients["eq1_thermal"], sd=fun.robust(o.iv2)[,2]["eq1_thermal"])
  beta.2s = rnorm(1000000,mean=o.iv2$coefficients["eq1_euets2010"], sd=fun.robust(o.iv2)[,2]["eq1_euets2010"])
  
  # Calculate the 90 % confidence interval for the ratio of beta_2/beta_1 
  ratio.bounds = quantile(beta.2s/beta.1s, probs=c(0.05, 0.95))
  
  # Calculate emissions
  dt.hist.emissions[, emissions.mean:=beta.2*(1/beta.1*price)]
  dt.hist.emissions[, emissions.low:=ratio.bounds[1]*price]
  dt.hist.emissions[, emissions.high:=ratio.bounds[2]*price]
  
  # Format figure
  dt.hist = dt.hist.emissions[, lapply(.SD, sum), .SDcols=c("emissions.mean", "emissions.low", "emissions.high"), by=yr][dt.eea[, .(act=sum(emissions)), by=yr], on="yr"][yr>=2005,]
  dt.pic = melt(dt.hist[, .(yr, emissions.mean, act)], id="yr")
  p.hist.co2 = ggplot(dt.pic) + geom_line( aes(yr, value, linetype=variable), show.legend=F) + geom_ribbon(data=dt.hist, aes(x=yr, ymin=emissions.low, ymax=emissions.high), alpha=0.2)
  p.hist.co2 = p.hist.co2 + scale_linetype_manual(name = "Value", values=c("solid", "11", "11", "11")) + scale_x_continuous(breaks=dt.hist$yr)
  p.hist.co2 = p.hist.co2 + theme_bw(base_family="serif", base_size = 12)
  p.hist.co2 = p.hist.co2 + xlab("") + ylab("Emissions, MtCO2")
  
  # Save if specified
  if (SAVE.FIGURES==TRUE) {
    pdf(file="p_histco2.pdf", width=3.94*1.5, height=2.62*1.5, pointsize=9)
    print(p.hist.co2+theme_bw(base_family="serif", base_size=9))
    dev.off()
  }
  
  # Output
  print(p.hist.co2)
}

# Willingess to pay for wind

# .. Table C.2 Willingness to pay for wind ----

if (VERBOSE > 1) {
  # Allocate expenditure according to the average demand in each area
  dt.expend.low = t(round(as.matrix(dt.expenditure.table$low) %*% n.area.shares))
  dt.expend.high = t(round(as.matrix(dt.expenditure.table$high) %*% n.area.shares))
  dt.expend.low = rbind(dt.expend.low, colSums(dt.expend.low))
  dt.expend.high = rbind(dt.expend.high, colSums(dt.expend.high))
  
  # Change in consumer expenditure 
  dt.expend.diff.low = t(diff(t(dt.expend.low)))
  dt.expend.diff.high = t(diff(t(dt.expend.high)))
  
  # Relative change in consumer expenditure (expenditure divided by the addition of wind, start from 0)
  dt.expend.relative.low = (sapply(seq(1:5), function(j) round(-dt.expend.diff.low[,j]/10,0)))
  dt.expend.relative.high = (sapply(seq(1:5), function(j) round(-dt.expend.diff.high[,j]/10,0)))
  
  # Merge tables with confidence intervals
  n = prod(dim(dt.expend.relative.low))
  dt.expend.conf = as.data.table(matrix(lapply(1:n, function(i) paste("[", dt.expend.relative.low[i], ", ", dt.expend.relative.high[i], "]", sep="")), nrow=dim(dt.expend.relative.mean)[1]))
  
  dt.expend.relative = rbindlist(lapply(1:5, function(i) rbind(as.data.table(dt.expend.relative.mean)[i, 1:(n.scenarios-1)], dt.expend.conf[i,])))
  
  colnames(dt.expend.relative) = c("10", "20", "30", "40", "50")
  
  dt.expend.relative$id = c("Denmark", "", "Finland", "", "Norway", "", "Sweden", "", "Total", "")
  setcolorder(dt.expend.relative, "id")
  
  # Table formatting
  table.text = "Willingness to pay for wind"
  
  # Output
  stargazer(dt.expend.relative, type="text", summary=FALSE, rownames=F, digits=0, title=table.text)
  #stargazer(dt.expend.relative, type="latex", summary=FALSE, rownames=F, digits=0, title=table.text)
}

# Change in producer surplus

# .. Table C.3 Surplus change by technology ----

if (VERBOSE > 1) {
  # Collect results as a table and format as in paper
  dt.surplus.table.low = t(dcast(dt.surplus[bound=="low", round(sum(surplus.change)), by=.(scenario, variable)], scenario~variable, value.var="V1"))[2:5, ]
  dt.surplus.table.high = t(dcast(dt.surplus[bound=="high", round(sum(surplus.change)), by=.(scenario, variable)], scenario~variable, value.var="V1"))[2:5, ]
  
  dt.surplus.table.low = rbind(dt.surplus.table.low, colSums(dt.surplus.table.low))
  dt.surplus.table.high = rbind(dt.surplus.table.high, colSums(dt.surplus.table.high))
  
  n = prod(dim(dt.surplus.table.mean))
  
  dt.surplus.table.low = format(dt.surplus.table.low, big.mark=",", trim=TRUE)
  dt.surplus.table.mean = format(dt.surplus.table.mean, big.mark=",", trim=TRUE)
  dt.surplus.table.high = format(dt.surplus.table.high, big.mark=",", trim=TRUE)
  
  dt.surplus.conf = as.data.table(matrix(lapply(1:n, function(i) paste("[", dt.surplus.table.low[i], ", ", dt.surplus.table.high[i], "]", sep="")), nrow=dim(dt.surplus.table.high)[1]))
  dt.surplus.conf[, V1:=NULL]
  
  dt.surplus.table = rbindlist(lapply(1:5, function(i) rbind(as.data.table(dt.surplus.table.mean)[i, 2:n.scenarios], dt.surplus.conf[i,])))
  # add total revenues for generators
  dt.surplus.table$id = c("HYDRO", "", "NUCLEAR", "", "THERMAL","", "WIND", "", "Total", "")
  setcolorder(dt.surplus.table, "id")
  
  # Table formatting
  table.text = "Surplus change by technology"
  
  # Output
  stargazer(dt.surplus.table, type="text", summary=FALSE, rownames=FALSE, digits=0, font.size="scriptsize", title=table.text)
  #stargazer(dt.surplus.table, type="latex", summary=FALSE, rownames=FALSE, digits=0, font.size="scriptsize", title=table.text)
}


# APPENDIX D: Additional robustness analysis ----------------------------------

# Seasonal pattern of the estimated outputs

# .. Table D.1 Seasonal patterns ----

if (VERBOSE > 1) {
  # Thermal and hydro regressions done above, this just for reporting
  add.lines = list(c("F Statistic", sapply(list(o.thermal, o.hydro), function(o) round(summary(o)$f[1],1))))
  
  robust.seh = lapply(list(o.thermal, o.hydro), function(o) fun.robust(o)[,2])
  
  # Format table
  covariate.names = names(o.thermal$coefficients)
  order = c(covariate.names[1:5], month.names[c(1:3,5:12)])
  covariate.labels = c("Wind", "Temperature", "Inflow", "Past inflows", month.abb[c(1:3,5:12)], paste("Year ", (start.year+1):end.year, sep=""))
  table.text = "Seasonal patterns"
  
  # Output
  stargazer(list(o.thermal, o.hydro),type="text", single.row=T, omit.stat=c("ser", "F"), report=("vcs"), covariate.labels = covariate.labels, add.lines=add.lines, se=robust.se, order=order, font.size="scriptsize", title=table.text)
  #stargazer(list(o.thermal, o.hydro),type="latex", single.row=T, omit.stat=c("ser", "F"), report=("vcs"), covariate.labels = covariate.labels, add.lines=add.lines, se=robust.se, order=order, font.size="scriptsize", title=table.text)
}

# .. Figure D.1 Residuals of regression “thermal” for months in 2001-2017 ----

if (VERBOSE > 1) {
  # Collect data  
  dt.pic = data.table(dt.fit[, .(yr, mo, thermal)], thermal.r = o.thermal$residuals)
  dt.pic[, month:=factor(month.abb[mo], levels=month.abb)]
  
  # Format figure
  p.thermal.res = ggplot(dt.pic, aes(yr, thermal.r))+geom_point(size=0.5)+stat_smooth(method="glm")+ facet_wrap( ~ month,  ncol=2)
  p.thermal.res = p.thermal.res + theme_bw(base_family="serif", base_size = 12)
  p.thermal.res = p.thermal.res + xlab("") + ylab("Residual, TWh/month")
  
  # Save if specified
  if (SAVE.FIGURES==TRUE) {
    pdf(file="p_thermal_residuals.pdf", width=5, height=8, pointsize=8)
    p.thermal.res + theme_bw(base_family="serif", base_size = 8)
    dev.off()
  }
  
  print(p.thermal.res)
}


# Exclusion restriction of the IV

# .. Figure D.2 Split sample supply curve ----

if (VERBOSE > 1) {
  # Calculate residualized spot prices
  dt.res = dt.fit[, .(yr, spot2010, coal2010, euets2010)]
  dt.res[, residualized:=exp(log(spot2010)-(coal2010-mean(coal2010))*o.iv$coefficients["coal2010.a"]-(euets2010-mean(euets2010))*o.iv$coefficients["euets2010.a"])]
  
  # Collect data
  dt.pic = rbind(data.table(thermal=dt.fit[yr<=2011,]$thermal, spot=dt.res[yr<=2011,]$residualized, id="2001-2011"), data.table(thermal=dt.fit[yr>2011,]$thermal, spot=dt.res[yr>2011,]$residualized, id="2012-2017"))
  
  # Format figure
  p.split.slope = ggplot(dt.pic, aes(thermal, spot, color=id))+geom_point(aes(shape=id,size=id)) + stat_smooth(size=0.5)
  p.split.slope = p.split.slope + scale_shape_manual(values=c(16,17))+scale_size_manual(values=c(0.7,0.7)) + scale_color_brewer(palette="Dark2")
  p.split.slope = p.split.slope + labs(x="Thermal output, TWh/month", y="Price, EUR/MWh")
  p.split.slope = p.split.slope + theme_bw(base_family="sans", base_size = 12) 
  
  # Output
  if (SAVE.FIGURES==TRUE) {
    pdf(file="p_split_slope.pdf", width=3.94, height=2.62, pointsize=9)
    print(p.split.slope +theme_bw(base_family="serif", base_size = 8) + theme(legend.title=element_blank(), legend.position=c(0.7, 0.87), legend.direction="horizontal")) 
    dev.off()
  }
  
  print(p.split.slope) + theme(legend.title=element_blank(), legend.position=c(0.7, 0.87), legend.direction="horizontal")
}

# .. Figure D.3 Supply curve slope over time ----

if (VERBOSE > 1) {
  # Regress OLS model with interactions 
  o.slopes = lm(log(spot2010) ~ thermal.a + coal2010.a + euets2010.a + thermal.a*year + month, data=dt.fit)
  
  # Collect data
  dt.slopes = data.table(yr=(start.year+1):end.year, slope=o.slopes$coefficients["thermal.a"]+o.slopes$coefficients[grep("thermal.a:", names(o.slopes$coefficients))], confint(o.slopes)[grep("thermal.a:", rownames(confint(o.slopes))), ]+o.slopes$coefficients["thermal.a"])
  setnames(dt.slopes, c("yr", "mean", "low", "high"))
  
  dt.slopes$base = o.iv$coefficients["thermal.a"]
  
  # Format figure
  p.slopes = ggplot(dt.slopes)+geom_pointrange(aes(x=yr, y=mean, ymin=low, ymax=high), size=0.2) + geom_line(aes(yr, base), color="red", linetype="dashed", size=0.5)
  p.slopes = p.slopes + theme_bw(base_family="serif", base_size = 12) + theme(legend.title=element_blank())  
  p.slopes = p.slopes + scale_x_continuous(breaks=seq(start.year, end.year, by=2)) #+ ylim(c(0,0.4)) #+ scale_linetype_manual(values=c("dashed", "solid", "dotted","dotdash")) + scale_shape_manual(values=c(1,16,24,8))
  p.slopes = p.slopes + xlab("") + ylab(expression(beta[1])) + ggtitle("")
  
  # Output
  if (SAVE.FIGURES==TRUE) {
    pdf(file="p_slopes.pdf", width=3.94, height=2.62, pointsize=9)
    print(p.slopes +theme_bw(base_family="serif", base_size = 8)) 
    dev.off()
  }
  
  print(p.slopes)
}

# Functional form specification

# .. Table D.2 IV regression with various functional forms ----

if (VERBOSE > 1) {
  # Create invariant data set
  dt.invariant$temp = rep(data.table(mo=1:12, V1=o.fe.temp$fitted.values)[, mean(V1), by=mo]$V1, n.years)
  dt.invariant$inflow = rep(data.table(mo=1:12, V1=o.fe.inflow$fitted.values)[, mean(V1), by=mo]$V1, n.years)
  dt.invariant$past.inflows = rep(dt.fit[, mean(past.inflows), by=yr]$V1, each=12)
  dt.invariant$thermal.a = 0
  dt.invariant$thermal.hat.a = 0
  dt.invariant$log.thermal.hat.a = 0
  dt.invariant$coal2010.a = 0
  dt.invariant$euets2010.a = 0
  dt.invariant$wind.a = 0
  dt.invariant$temp.a = 0
  dt.invariant$inflow.a = 0
  
  # Log model for the price
  fm.log = as.formula(paste("log(spot2010) ~ thermal.a + coal2010.a + euets2010.a +", dummies, " | wind.a + temp.a + inflow.a + past.inflows + coal2010.a + euets2010.a +", dummies))
  o.iv.log = ivreg(fm.log, data=dt.fit, x=TRUE)
  
  # Correct for log-bias in the estimation
  dt.adj.log = data.table(dt.fit[, .(yr,mo)], fit.raw=exp(o.iv.log$fitted.values), residual=o.iv.log$residuals)
  dt.adj.log[, adj:=exp(sd(residual)^2/2), by=mo]
  dt.adj.log[, fit:=fit.raw*adj]
  
  adj.factor.log = dt.adj.log[, mean(adj), by=mo]$V1
  
  # Linear model for the price
  fm.linear = as.formula(paste("spot2010 ~ thermal.a + coal2010.a + euets2010.a +", dummies, " | wind.a + temp.a + inflow.a + past.inflows + coal2010.a + euets2010.a +", dummies))
  o.iv.linear = ivreg(fm.linear, data=dt.fit, x=TRUE)
  
  # Square root model for the price
  fm.iv.sqrt = as.formula(paste("sqrt(spot2010) ~ thermal.a + coal2010.a + euets2010.a +", dummies, " | wind.a + temp.a + inflow.a + past.inflows + coal2010.a + euets2010.a +", dummies))
  o.iv.sqrt = ivreg(fm.iv.sqrt, data=dt.fit, x=TRUE)
  
  # Adjust for sqrt transformation bias (mean, we ignore the variance bias that is of order of magnitude smaller)
  dt.adj.sqrt = data.table(dt.fit[, .(yr,mo)], fit.raw=o.iv.sqrt$fitted.values^2, residual=o.iv.sqrt$residuals)
  dt.adj.sqrt[, adj:=sd(residual)^2, by=mo]
  dt.adj.sqrt[, fit:=fit.raw+adj]
  
  adj.factor.sqrt = dt.adj.sqrt[, mean(adj), by=mo]$V1
  
  # Cuberoot model for the price
  dt.fit[, cuberoot.spot := spot2010^(1/3)]
  
  fm.iv.cuberoot = as.formula(paste("cuberoot.spot ~ thermal.a + coal2010.a + euets2010.a +", dummies, " | wind.a + temp.a + inflow.a + past.inflows + coal2010.a + euets2010.a +", dummies))
  o.iv.cuberoot = ivreg(fm.iv.cuberoot, data=dt.fit, x=TRUE)
  
  # Adjust for sqrt transformation bias (mean, we ignore the variance bias that is of order of magnitude smaller)
  dt.adj.cube = data.table(dt.fit[, .(yr,mo)], fit.raw=o.iv.cuberoot$fitted.values^3, residual=o.iv.cuberoot$residuals)
  dt.adj.cube[, adj:=sd(residual)^2, by=mo]
  dt.adj.cube[, fit:=fit.raw+adj]
  
  adj.factor.cuberoot = dt.adj.cube[, mean(adj), by=mo]$V1
  
  # Log-log model with 2SLS
  # first stage: log of thermal quantities
  o.thermal.fs = lm(log(thermal) ~ wind.a + temp.a + inflow.a + past.inflows + coal2010.a + euets2010.a + month + year, data=dt.fit)
  dt.fit$log.thermal.hat = o.thermal.fs$fitted.values
  
  # residualize
  o.fe.thermal.hat = lm(log.thermal.hat ~ month + year, data=dt.fit)
  dt.fit$log.thermal.hat.a = o.fe.thermal.hat$residuals
  
  # second stage
  o.loglog = lm(log(spot2010) ~ log.thermal.hat.a + coal2010.a + euets2010.a + month + year, data=dt.fit)
  
  # correct bias
  dt.adj.loglog = data.table(dt.fit[, .(yr,mo)], fit.raw=exp(o.loglog$fitted.values), residual=o.loglog$residuals)
  dt.adj.loglog[, adj:=exp(sd(residual)^2/2), by=mo]
  dt.adj.loglog[, fit:=fit.raw*adj]
  
  adj.factor.loglog = dt.adj.loglog[, mean(adj), by=mo]$V1
  
  # Collect (transformation fixed) prices
  price.log = data.table(mo=1:12, p=predict(o.iv.log, newdata=dt.invariant))[, mean(exp(p)), by=mo]$V1*adj.factor.log
  price.linear = data.table(mo=1:12, p=predict(o.iv.linear, newdata=dt.invariant))[, mean(p), by=mo]$V1
  price.sqrt = data.table(mo=1:12, p=predict(o.iv.sqrt, newdata=dt.invariant)^2+adj.factor.sqrt)[, mean(p), by=mo]$V1
  price.cuberoot = data.table(mo=1:12, p=predict(o.iv.cuberoot, newdata=dt.invariant)^3+adj.factor.cuberoot)[, mean(p), by=mo]$V1
  price.loglog = data.table(mo=1:12, p=predict(o.loglog, newdata=dt.invariant))[, mean(exp(p)), by=mo]$V1*adj.factor.loglog
  
  table.text = "IV regression with various functional forms"
  
  covariate.names = names(o.iv.log$coefficients)
  omit.covariants = c("Constant", covariate.names[sapply(c(month.abb, year.names), function(n) match(n, covariate.names))])
  omit.covariants = omit.covariants[!is.na(omit.covariants)]
  covariate.labels = c("Thermal", "Log(Thermal)", "Coal price", "Carbon price")
  
  o.robustas = list(o.iv.log, o.iv.linear, o.iv.sqrt, o.iv.cuberoot, o.loglog)
  robust.se = lapply(o.robustas, function(o) fun.robust(o)[,2])
  
  add.lines = list(c("Month FE", rep("Yes", length(o.robustas))), c("Year FE", rep("Yes", length(o.robustas))))
  
  # Output
  stargazer(o.robustas, type="text", single.row=TRUE, font.size="small", report=c("vcs"), omit.stat = c("ser", "F"), digits=3, se=robust.se, omit=omit.covariants, covariate.labels = covariate.labels, add.lines=add.lines, title=table.text, dep.var.caption="", dep.var.labels.include=FALSE)
  #stargazer(o.robustas, type="latex", single.row=TRUE, font.size="small", report=c("vcs"), omit.stat = c("ser", "F"), digits=3, se=robust.se, omit=omit.covariants, covariate.labels = covariate.labels, add.lines=add.lines, title=table.text, dep.var.caption="", dep.var.labels.include=FALSE)
}

# .. Figure D.4 Invariant price with varying functional form ----

if (VERBOSE > 1) {
  # Collect data
  dt.predict = copy(dt.invariant)
  dt.pic = data.table(mo=1:12, log=price.log, linear=price.linear, sqrt=price.sqrt, cuberoot=price.cuberoot, log.log=price.loglog)
  
  # Format figure
  p.form.robustness = ggplot(melt(dt.pic, id="mo"), aes(mo, value, linetype=variable))+geom_line()
  p.form.robustness = p.form.robustness + scale_x_continuous(breaks=1:12, labels=month.abb) + scale_y_continuous(labels = comma, limits = c(27,42))
  p.form.robustness = p.form.robustness + theme_bw(base_family="sans", base_size = 12) + theme(legend.title=element_blank())
  p.form.robustness = p.form.robustness + labs(x="Month", y="Price, EUR/MWh", linetype="Functional form") + ggtitle("")
  
  # Save if specified
  if (SAVE.FIGURES==TRUE) {
    pdf(file="p_form_robustness.pdf", width=3.94, height=2.62, pointsize=9)
    print(p.form.robustness+theme_bw(base_family="serif", base_size = 9)+ theme(legend.title=element_blank()))
    dev.off()
  }
  
  # Output  
  print(p.form.robustness)
}

# .. Table D.3 Counterfactual prices with various functional forms ----

if (VERBOSE > 1) {
  # Collect data
  dt.prices.robustness = data.table(fun.wind.price(o.iv2, "log", "confidence"), linear=fun.wind.price(o.iv.linear, "linear")$linear, sqrt=fun.wind.price(o.iv.sqrt, "sqrt")$sqrt, cuberoot=fun.wind.price(o.iv.cuberoot, "cuberoot")$cuberoot, loglog=fun.wind.price(o.loglog, "log")$mean)
  dt.form.robustness = dt.prices.robustness[, lapply(.SD, mean), .SDcols = c("low", "mean", "high", "linear", "sqrt", "cuberoot", "loglog"), by=wind]
  
  # Format table
  table.text = "Counterfactual prices with various functional forms"
  
  # Output
  stargazer(dt.form.robustness, type="text", summary=FALSE, rownames=FALSE, single.row=TRUE, align=TRUE, font.size="small", digits=2, title=table.text)
  #stargazer(dt.form.robustness, type="latex", summary=FALSE, rownames=FALSE, single.row=TRUE, align=TRUE, font.size="small", digits=2, title=table.text)
}

# Seasonal stability

# .. Table D.4 Thermal output regression with interactions ----

if (VERBOSE > 1) {
  # Specifications with interactions
  fm.interactions.l = list()
  
  fm.interactions.l[[1]] = thermal ~ wind.a + temp.a + inflow.a + past.inflows + month + year
  fm.interactions.l[[2]] = thermal ~ wind.a + temp.a + inflow.a + past.inflows + month + year + month*inflow.a
  fm.interactions.l[[3]] = thermal ~ wind.a + temp.a + inflow.a + past.inflows + month + year + month*past.inflows
  fm.interactions.l[[4]] = thermal ~ wind.a + temp.a + inflow.a + past.inflows + month + year + month*inflow.a + month*past.inflows
  
  o.thermali.l = lapply(fm.interactions.l, function(fm) lm(fm, data=dt.fit))
  
  # collect F-statistics
  f.stats.iac = list(c("F Statistic", sapply(o.thermali.l, function(o) round(summary(o)$f[1],1))))
  
  # Report robust standard error values
  robust.se.iac = lapply(o.thermali.l, function(o) fun.robust(o)[,2])
  
  # Format table
  covariants.iac = c(names(o.thermali.l[[4]]$coefficients)[2:5], month.names, paste("inflow.a", month.names, sep=":"), paste("past.inflows", month.names, sep=":"))
  month.reg = month.abb[c(1:3,5:12)]
  covariate.labels = c("Wind", "Temperature", "Inflow", "Past inflows", month.reg, paste("Inflow:", month.reg, sep=""), paste("Past inflows:", month.reg, sep=""))
  table.text = "Thermal output regression with interactions"
  
  # output table
  stargazer(o.thermali.l[1:4], single.row=TRUE, align=FALSE, type="text", font.size="tiny", report=c("vsc"), digits=3, add.lines=f.stats.iac, se=robust.se.iac, order=paste0("^", covariants.iac, "$"), omit=year.names, omit.stat = c("f", "ser"), covariate.labels = covariate.labels, dep.var.caption="", dep.var.labels.include=FALSE, title=table.text)
  #stargazer(o.thermali.l[1:4], single.row=TRUE, align=FALSE, type="latex", font.size="tiny", report=c("vsc"), digits=3, add.lines=f.stats.iac, se=robust.se.iac, order=paste0("^", covariants.iac, "$"), omit=year.names, omit.stat = c("f", "ser"), covariate.labels = covariate.labels, dep.var.caption="", dep.var.labels.include=FALSE, title=table.text)
}

# Redefining the adjusting margin

# Define a joint variable for thermal and nuclear
dt.fit[, tn := thermal+nuclear]

# Residualize against fixed effects
o.fe.tn = lm(tn ~ month+year, data=dt.fit)
dt.fit$tn.a = o.fe.tn$residuals

# .. Table D.5 Regression of output of other technologies on wind ----

if (VERBOSE > 1) {
  # Output regression for thermal + nuclear
  o.tn.q = lm(tn ~ wind.a + temp.a + inflow.a + past.inflows + month + year, data=dt.fit)
  
  # Collect specifcation to report
  o.volumes = list(o.thermal.q, o.hydro.q, o.nuclear.q, o.tn.q)
  
  # Calculate robust standard errors
  robust.se = lapply(o.volumes, function(o) fun.robust(o)[,2])
  
  # Collect F-statistics
  f.stats.volumes = c("F Statistic", sapply(o.volumes, function(o) round(summary(o)$f[1],1)))
  
  # Format table
  covariate.names = names(o.thermal.q$coefficients)
  omit.covariants = c("Constant", covariate.names[sapply(c(month.names, year.names), function(n) match(n, covariate.names))])
  omit.covariants = omit.covariants[!is.na(omit.covariants)]
  covariate.labels = c("Wind", "Temperature", "Inflow", "Past inflows")
  add.lines = list(c("Month FE", "Yes", "Yes", "Yes", "Yes"), c("Year FE", "Yes", "Yes", "Yes", "Yes"), f.stats.volumes)
  
  table.text = "Regression of output of other technologies on wind"
  
  # Output
  stargazer(o.volumes, type="text", single.row = T, omit.stat = c("F", "ser"), report=c("vsc"), se=robust.se, add.lines = add.lines, omit=omit.covariants, covariate.labels = covariate.labels, font.size="scriptsize", title=table.text)
  #stargazer(o.volumes, type="latex", single.row = T, omit.stat = c("F", "ser"), report=c("vsc"), se=robust.se, add.lines = add.lines, omit=omit.covariants, covariate.labels = covariate.labels, font.size="scriptsize", title=table.text)
}

# .. Table D.6 Regressions of ln(p(t)) with OLS and IV (thermal + nuclear as margin) ----

if (VERBOSE > 1) {
  # Run first stage (for the F-statistics)
  o.tn.l = list()
  o.tn.l[[1]] = lm(tn ~ wind.a + temp.a + coal2010.a + euets2010.a + year + month, data=dt.fit)
  o.tn.l[[2]] = lm(tn ~ wind.a + inflow.a + past.inflows + coal2010.a + euets2010.a + year + month, data=dt.fit)
  o.tn.l[[3]] = lm(tn ~ wind.a + temp.a + inflow.a + past.inflows + coal2010.a + euets2010.a + year + month, data=dt.fit)
  
  # Run the IV regression
  fm.tn.l = lapply(spec.second.stage.l, function(s) as.formula(paste("log(spot2010) ~ tn.a + coal2010.a + euets2010.a + year + month", s)))
  
  # For comparison, add OLS estimate with the same covariants
  o.tn.ols = lm(log(spot2010) ~ tn.a + coal2010.a + euets2010.a + month + year, data=dt.fit)
  
  o.ivtn.l = list()
  o.ivtn.l[[1]] = o.tn.ols
  
  # Run instrumental variables regression with ivreg from AER package
  o.ivtn.l[[2]] = ivreg(fm.tn.l[[1]], data=dt.fit)
  o.ivtn.l[[3]] = ivreg(fm.tn.l[[2]], data=dt.fit)
  o.ivtn.l[[4]] = ivreg(fm.tn.l[[3]], data=dt.fit)
  
  # Calculate robust standard errors
  robust.se = lapply(o.ivtn.l, function(o) fun.robust(o)[,2])
  
  # Collect F-statistics
  add.lines = list(c("F Statistic (1st stage)", c(round(summary(o.tn.ols)$fstatistic[1],1), sapply(o.tn.l, function(o) round(summary(o)$f[1],1)))), c("Seasonal FE", "Yes", "Yes", "Yes", "Yes", "Yes"), c("Annual FE", "Yes", "Yes", "Yes", "Yes", "Yes"))
  
  # Format table
  table.text = "Regressions of ln(p(t)) with OLS and IV"
  covariate.labels = c("Thermal and nuclear", "Coal price", "Carbon price")
  
  # Output
  stargazer(o.ivtn.l, type="text", single.row = T, omit.stat=c("F", "ser"), report=c("vsc"), add.lines=add.lines, se=robust.se, omit=c("Constant", month.names, year.names), covariate.labels = covariate.labels, title=table.text)
  #stargazer(o.ivtn.l, type="latex", single.row = T, omit.stat=c("F", "ser"), report=c("vsc"), add.lines=add.lines, se=robust.se, omit=c("Constant", month.names, year.names), covariate.labels = covariate.labels, title=table.text)
}

# Annual regressions

# .. Table D.7 Annual regression of output of other technologies on wind ----

if (VERBOSE > 1) {
  # Collect year level data
  dt.fita = dt.fit[, lapply(.SD, mean), .SDcols=c(colnames(dt.fit)[40:71]), by=yr]
  
  # Carry out regressions
  fm.volumesa.l = lapply(c("thermal","hydro","nuclear"), function(type) as.formula(paste(type,"~ wind + temp + inflow + past.inflows")))
  o.volumesa = lapply(fm.volumesa.l, function(o) lm(o,data=dt.fita))
  
  # Calculate robust standard errors
  robust.se = lapply(o.volumesa, function(o) fun.robust(o)[,2])
  
  # Collect F-statistics
  add.lines = list(c("F Statistic", sapply(o.volumesa, function(o) round(summary(o)$f[1],1))))
  
  # Format table
  covariate.names = names(o.volumesa[[1]]$coefficients)
  covariate.labels = c("Wind", "Temperature", "Inflow", "Past inflows")
  
  table.text = "Annual regression of output of other technologies on wind"
  
  # Output
  stargazer(o.volumesa, type="text", single.row = T, omit.stat = c("F", "ser"), report=c("vsc"), add.lines = add.lines, se = robust.se, covariate.labels = covariate.labels, font.size="scriptsize", title=table.text)
  #stargazer(o.volumesa, type="latex", single.row = T, omit.stat = c("F", "ser"), report=c("vsc"), add.lines = add.lines, se = robust.se, covariate.labels = covariate.labels, font.size="scriptsize", title=table.text)
}


# .. Table D.8 Regressions of ln(p(t)) with OLS and IV (annual data) ----

if (VERBOSE > 1) {
  # Run first stage (for the F-statistics)
  o.thermala.l = list()
  o.thermala.l[[1]] = lm(thermal ~ wind + temp + coal2010 + euets2010, data=dt.fita)
  o.thermala.l[[2]] = lm(thermal ~ wind + inflow + past.inflows + coal2010 + euets2010, data=dt.fita)
  o.thermala.l[[3]] = lm(thermal ~ wind + temp + inflow + past.inflows + coal2010 + euets2010, data=dt.fita)
  
  # For comparison, add OLS estimate with the same covariants
  o.olsa = lm(log(spot2010) ~ thermal + coal2010 + euets2010, data=dt.fita)
  
  o.iva = list()
  o.iva[[1]] = o.olsa
  
  # Run instrumental variables regression with ivreg from AER package
  o.iva[[2]] = ivreg(log(spot2010) ~ thermal + coal2010 + euets2010 | wind + temp + coal2010 + euets2010, data=dt.fita)
  o.iva[[3]] = ivreg(log(spot2010) ~ thermal + coal2010 + euets2010 | wind + inflow + past.inflows + coal2010 + euets2010, data=dt.fita)
  o.iva[[4]] = ivreg(log(spot2010) ~ thermal + coal2010 + euets2010 | wind + temp + inflow + past.inflows + coal2010 + euets2010, data=dt.fita)
  
  # Calculate robust standard errors
  robust.se = lapply(o.iva, function(o) fun.robust(o)[,2])
  
  # Collect F-statistics
  add.lines = list(c("F Statistic (1st stage)", c(round(summary(o.olsa)$fstatistic[1],1), sapply(o.thermala.l, function(o) round(summary(o)$f[1],1)))))
  
  table.text = "Regressions of ln(p(t)) with OLS and IV"
  covariate.labels = c("Thermal", "Coal price", "Carbon price")
  
  stargazer(o.iva, type="text", single.row = T, omit.stat=c("F", "ser"), report=c("vsc"), add.lines=add.lines, se=robust.se, omit=c("Constant", month.names, year.names), covariate.labels = covariate.labels, title=table.text)
  #stargazer(o.iva, type="latex", single.row = T, omit.stat=c("F", "ser"), report=c("vsc"), add.lines=add.lines, se=robust.se, omit=c("Constant", month.names, year.names), covariate.labels = covariate.labels, title=table.text)
}






