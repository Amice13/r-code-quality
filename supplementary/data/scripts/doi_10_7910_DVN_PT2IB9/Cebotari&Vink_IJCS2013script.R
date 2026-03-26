# load QCA package into current session
library(QCA)
update.packages(ask = 'graphics', checkBuilt = TRUE)

# import data Cebotari & Vink 2013
setwd("/Applications/QCAR/Datafiles")
CVraw <- read.table("CebotariVink2013raw.txt", header = TRUE, row.names = "case")
head(CVraw)

# define data frame
CV <- data.frame(matrix(rep(numeric(29), 6), nrow = 29,
	dimnames = list(row.names(CVraw),
	toupper(names(CVraw)))))
CV

# Simple Bar Plot 
counts <- table(CVraw$groupcon)
barplot(counts, main = "Group Concentration",
	xlab = "Number of Groups",
	ylab = "Frequency")

# Simple Horizontal Bar Plot with Added Labels 
barplot(counts, main = "Group Concentration", horiz = TRUE,
        names.arg = c("0", "1", "2", "3"))

# Grouped Bar Plot
counts2 <- table(CVraw$groupcon, CVraw$ethfract)
barplot(counts2, main = "Group Concentration and Ethnic Fractionalization",
        xlab = "Number of Groups", col = c("darkblue", "red"),
        legend = rownames(counts), beside = TRUE)

# Median and Mean
median(CVraw$groupcon)
mean(CVraw$groupcon)

# calibration
CV$PROTEST <- calibrate(CVraw$protest, type = "fuzzy", logistic = TRUE,
	thresholds = c(0.5, 1.5, 3))
CV$GROUPCON <- calibrate(CVraw$groupcon, type = "fuzzy", logistic = TRUE,
	thresholds = c(0, 1.25, 3))
CV$DEMSCORE <- calibrate(CVraw$demscore, type = "fuzzy", logistic = TRUE,
	thresholds = c(2, 7, 9.5))
CV$ETHFRACT <- calibrate(CVraw$ethfract, type = "fuzzy", logistic = TRUE,
	thresholds = c(0, 0.495, 0.8))
CV$POLDIS <- calibrate(CVraw$poldis, type = "fuzzy", logistic = TRUE,
	thresholds = c(0, 0.75, 3))
CV$PRIDE <- calibrate(CVraw$pride, type = "fuzzy", logistic = TRUE,
	thresholds = c(0.5, 1.5, 2.5))

round(CV, 3)

# necessity analysis 1
## you can adjust the inclusion and coverage cut-offs
CVnec1 <- superSubset(CV, outcome = "PROTEST", incl.cut = 0.90, cov.cut = 0.52)
CVnec1

# plot necessity1 GROUPCON
plot(CV$GROUPCON, CV$PROTEST, pch = 19,
	xlab = "GROUPCON", ylab = "PROTEST",
	abline(0, 1))
cases <- c(2, 15, 20)
text(CV$GROUPCON[cases], CV$PROTEST[cases],
	labels = rownames(CV)[cases],
	pos = c(4, rep(2, 4)))

# necessity analysis 0
CVnec0 <- superSubset(CV, outcome = "PROTEST", neg.out = TRUE, 
	incl.cut = 0.90, cov.cut = 0.52)
CVnec0

# plot necessity0 PRIDE
plot(CV$PRIDE, (1 - CV$PROTEST),
	xlab = "PRIDE", ylab = "protest",
	abline(0, 1))
cases <- c(5, 27)
text(CV$PRIDE[cases], (1 - CV$PROTEST)[cases],
	labels = rownames(CV)[cases],
	pos = c(4, rep(2, 4)))

# create truth table 1
CVtt1 <- truthTable(CV, outcome = "PROTEST", incl.cut1 = 0.8,
	show.cases = TRUE, sort.by = c("incl", "n"))
CVtt1

# Boolean minimiation 1 (complex)
CVcomplex1 <- eqmcc(CVtt1, details = TRUE, show.cases = TRUE)
CVcomplex1

# Boolean minimiation 1 (parsimonious, without row dominance)
## row.dom is now set to FALSE by default
CVtt1pars <- truthTable(CV, outcome = "PROTEST", 
	incl.cut1 = 0.8, complete = TRUE,
	show.cases = TRUE, sort.by = c("incl", "n"))
CVtt1pars

CVpars1nrd <- eqmcc(CVtt1pars, include = "?", details = TRUE, show.cases = TRUE)
CVpars1nrd

# PI chart to check for row dominance
CVpars1nrd$PIchart

# Boolean minimization 1 (parsimonious, with row dominance)
CVpars1rd <- eqmcc(CVtt1pars, include = "?", row.dom = TRUE,
  details = TRUE, show.cases = TRUE)
CVpars1rd

# Check Simplifying Assumptions
CVpars1rd$SA$M1

# plot sufficiency 1 (S1)
PIsc1 <- CVpars1rd$pims
PIsc1

par(mfrow = c(2, 2))
for(i in 1:4){
	plot(PIsc1[ , i], CV$PROTEST, pch = 19, ylab = "PROTEST",
	xlab = colnames(PIsc1)[i], xlim = c(0, 1), ylim = c(0, 1),
	main = paste("PI", print(i)))
	abline(0, 1)
	}

# create truth table 0
CVtt0 <- truthTable(CV, outcome = "PROTEST", neg.out = TRUE,
                    incl.cut1 = 0.8, show.cases = TRUE, sort.by = c("incl", "n"))
CVtt0

# Boolean minimization 0 (parsimonious, without row dominance)
CVtt0pars <- truthTable(CV, outcome = "PROTEST", neg.out = TRUE, 
	incl.cut1 = 0.8495, complete = TRUE,
	show.cases = TRUE, sort.by = c("incl", "n"))
CVtt0pars
CVpars0nrd <- eqmcc(CVtt0pars, include = "?", details = TRUE, show.cases = TRUE)
CVpars0nrd

# PI chart to check for row dominance
CVpars0nrd$PIchart

# Boolean minimization 0 (parsimonious, with row dominance)
CVtt0pars <- truthTable(CV, outcome = "PROTEST", neg.out = TRUE, 
	incl.cut1 = 0.8495, complete = TRUE,
	show.cases = TRUE, sort.by = c("incl", "n"))
CVtt0pars
CVpars0rd <- eqmcc(CVtt0pars, include = "?", row.dom = TRUE,
  details = TRUE, show.cases = TRUE)
CVpars0rd
# row dominance is necessary to reproduce same outcome as in Cebotari & Vink 2013

# Check Simplifying Assumptions
CVpars0rd$SA$M1

# plot sufficiency 0 (S1), with outlier case label
PIsc0 <- CVpars0rd$pims
PIsc0
par(mfrow = c(2, 2))
for(i in 1:4){
	plot(PIsc0[ , i], (1 - CV$PROTEST), pch = 19, ylab = "protest",
	xlab = colnames(PIsc0)[i], xlim = c(0, 1), ylim = c(0, 1),
	main = paste("PI", print(i)))
	abline(0, 1)
	text(PIsc0[ , i][c(2)], (1 - CV$PROTEST)[c(2)],
	labels = rownames(CV)[c(2)],
	pos = c(1))
	}

