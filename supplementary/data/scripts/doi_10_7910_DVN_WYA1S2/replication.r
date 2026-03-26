## analysis for Fortunato-Turner AJPS article ##
## we note that the original analysis was conducted with R version 3.4.0 ##
## these are the packages required ##
	rm(list = ls())
	library(mnormt)
	library(fBasics)
	library(corrplot)
	library(plm)
	library(pcse)
	library(lme4)
	library(matrixStats)
	library(doBy)
	
## now, SET YOUR DIRECTORY and read in data ##
	data <- read.table('replication.txt', sep = '|', header = TRUE)

## repair some broken strings ##
	data$state <- as.character(data$state)
	data$state[data$state == 'NewHampshire'] <- 'New Hampshire'
	data$state[data$state == 'NewJersey'] <- 'New Jersey'
	data$state[data$state == 'NewMexico'] <- 'New Mexico'
	data$state[data$state == 'NewYork'] <- 'New York'
	data$state[data$state == 'NorthCarolina'] <- 'North Carolina'
	data$state[data$state == 'NorthDakota'] <- 'North Dakota'
	data$state[data$state == 'RhodeIsland'] <- 'Rhode Island'
	data$state[data$state == 'SouthCarolina'] <- 'South Carolina'
	data$state[data$state == 'SouthDakota'] <- 'South Dakota'
	data$state[data$state == 'WestVirginia'] <- 'West Virginia'

## add term limits information ##
	data$term <- 0
	data$term[data$state == 'Arizona' & data$year > 1999] <- 1
	data$term[data$state == 'Arkansas' & data$year > 1997] <- 1
	data$term[data$state == 'California' & data$year > 1995] <- 1
	data$term[data$state == 'Colorado' & data$year > 1997] <- 1
	data$term[data$state == 'Florida' & data$year > 1999] <- 1
	data$term[data$state == 'Idaho' & (data$year > 1993  & data$year < 2003)] <- 1
	data$term[data$state == 'Louisiana' & data$year > 2006] <- 1
	data$term[data$state == 'Massachusetts' & (data$year > 1993 & data$year < 1998)] <- 1
	data$term[data$state == 'Maine' & data$year > 1995] <- 1
	data$term[data$state == 'Michigan' & data$year > 1997] <- 1
	data$term[data$state == 'Missouri' & data$year > 2001] <- 1
	data$term[data$state == 'Montana' & data$year > 1999] <- 1
	data$term[data$state == 'Nebraska' & data$year > 2005] <- 1
	data$term[data$state == 'Nevada' & data$year > 1997] <- 1
	data$term[data$state == 'Ohio' & data$year > 1999] <- 1
	data$term[data$state == 'Oklahoma' & data$year > 2003] <- 1
	data$term[data$state == 'Oregon' & (data$year > 1991 & data$year < 2003)] <- 1
	data$term[data$state == 'South Dakota' & data$year > 1999] <- 1
	data$term[data$state == 'Utah' & (data$year > 1993 & data$year < 2004)] <- 1
	data$term[data$state == 'Washington' & (data$year > 1991 & data$year < 1998)] <- 1
	data$term[data$state == 'Wyoming' & (data$year > 1991 & data$year < 2005)] <- 1

## standardize economic variables to ease estimation ##
	data$pcRevenue <- scale(data$stateRevenue)
	data$pcExpend <- scale(data$stateExpend)
	data$pcDebt <- scale(data$stateDebt)
	data$income <- scale(data$income)
	data$unemployment <- scale(data$unemployment)

## create rate of taxation variable ##
	data$taxRate <- data$stateTaxes/data$PerCapitaIncome

## create indicator for divided government and then the lagged dependent variable ##
	data$disp <- data$disposition == 'S'
	data$lagDv <- c()

	for(i in 1:length(data$state)){
		if(length(data$year[data$year == (data$year[i] - 1)]) > 0){
			data$lagDv[i] <- data$mean[data$state == data$state[i] & data$year == (data$year[i] - 1)]
		}
	}

## this creates the descriptive statistics in FIGURE 1 of the Main Text ##
## and the bivariate analyses in FIGURE 2 of the Main Text ##
## draw annual median risk estimates ##
	data$median <- c()
	for(i in 1:length(data$state)){
		data$median[i] <- median(as.numeric(data[i, c(31:1030)]))
	}

## extract states that issue G.O. bonds ##
## and create mean estimates ##
	states <- unique(data$state[data$noIssue == 0])
	m <- c()
	v <- c()

	for(s in 1:length(states)){
		m[s] <- mean(data$mean[data$state == states[s]])
		v[s] <- var(data$mean[data$state == states[s]])
	}

## this creates the descriptive graphic FIGURE 1 ##
	states <- unique(data$state[data$noIssue == 0])
	states <- states[order(m)]
	
	pdf('timeBrokenGray.pdf', width = 15, height = 10)
		par(mfrow = c(5, 8), oma = c(5, 5, 5, 0), mar = c(2, 2, 2, 2))
		for(s in states){
			plot(1, 1, type = 'n',
				ylim = c(-2, 4),
				xlim = c(1995, 2010),
				xlab = '',
				ylab = '',
				main = s,
				axes = FALSE)
			axis(1)
			axis(2, at = c(-2, 0, 2, 4))
			points(data$year[data$state == s], data$V500[data$state == s], col = rgb(0, 0, 0, 0.5), pch = 1, cex = 1.7, lwd = 1.25)
			polygon(c(min(data$year)-0.5, min(data$year)-0.5, max(data$year)+0.5, max(data$year)+0.5),
			c(min(data$V500[data$state == s])-0.25, max(data$V500[data$state == s]+0.25), 
			max(data$V500[data$state == s])+0.25, min(data$V500[data$state == s])-0.25),
			col = rgb(0, 0, 0, 0.1), border = FALSE)
			lines(c(min(data$year)-0.25, max(data$year)+0.25),
			c(mean(data$V500[data$state == s]), mean(data$V500[data$state == s])), col = rgb(0, 0, 0, 0.7), lwd = 2)
		}
		mtext('Credit Risk Over Time in the American States', outer = TRUE, font = 2, cex = 1.7, line = 2)
		mtext('Credit Risk', outer = TRUE, side = 2, font = 2, line = 3, cex = 1.25)
		mtext('Year', outer = TRUE, side = 1, font = 2, line = 3, cex = 1.25)
	dev.off()

## this creates the bivariate analysis FIGURE 2 ##
	pdf('rawPlotsGray.pdf', width = 10, height = 10)
		par(mfrow = c(4, 4), oma = c(7, 7, 9, 0), mar = c(3, 3, 3, 3))
		for(i in 1995:2010){
			plot(1, 1, type = 'n',
				ylim = c(-2, 4),
				xlim = c(0, 0.65),
				xlab = '',
				ylab = '',
				main = paste(i),
				axes = FALSE)
			axis(1, at = c(0, 0.2, 0.4, 0.6))
			axis(2, at = c(-2, 0, 2, 4))
			mod <- lm(data$mean[data$year == i] ~ data$squire[data$year == i])
			points(data$squire[data$noIssue == 1 & data$year == i], data$mean[data$noIssue == 1 & data$year == i], pch = 1, col = rgb(0, 0, 0, 0.5), cex = 1.5)
			points(data$squire[data$noIssue == 0 & data$year == i], data$mean[data$noIssue == 0 & data$year == i], pch = 16, col = rgb(0, 0, 0, 0.25), cex = 1.5)
			abline(mod, lwd = 2, col = rgb(0, 0, 0, 0.4))
			coef <- round(coef(mod)[2], 3)
			text(0.3, 3.75, substitute(paste(beta, " = ", coef), list(coef = coef)))
		}
		mtext('Credit Risk and Legislative Capacity\nin the American States 1995-2010', outer = TRUE, font = 2, cex = 1.7, line = 2)
		mtext('Credit Risk', outer = TRUE, side = 2, font = 2, line = 3, cex = 1.25)
		mtext('Squire Index', outer = TRUE, side = 1, font = 2, line = 3, cex = 1.25)
	dev.off()

## now the main models ##
## first create the time-series object ##
## and constrain the data to the 40 G.O. bond issuing states ##
	use <- pdata.frame(data, index = c('state', 'year'))
	issue <- pdata.frame(data[data$noIssue == 0, ], index = c('state', 'year'),
		drop.const.series = TRUE)

## this loop performs both the 2-stage and single stage models  ##
## shown in Main Text Table 1 ##

## create the objects to hold our estimates ##
	coef1 <- c()
	coef2 <- c()
	coef <- c()
	rSq1 <- c()
	rSq2 <- c()
	rSq <- c()
	first <- c()
	second <- c()
	
## collapse relevant data to state means for 2-stage model ##
	new <- summaryBy(squire + acir + revenueLimit + spendingLimit + debtRestriction ~ state, FUN = mean, data = data[data$noIssue == 0, ])
	summary(new)
	issue$state <- as.character(issue$state)

## open loop and begin estimation ##
## note that this may take a few hours depending on your machine ##
## note also that the posterior smapling may cause small deviations ##
## in your results from those published in the final manuscript ##
## but the differences should be quite small ##
	for(i in 1:1000){
		dv <- c()
		state <- c()

	## this sub-loop regresses all states individually, cycling through the 1,000 risk estimates ##
		for(j in 1:length(unique(issue$state))){
			issue1 <- issue[issue$state == unique(issue$state)[j], ]

			mod1 <- lm(issue1[, 9+i] ~ term + (disposition == 'S') + scale(turnoverWeighted90) + scale(unemployment) + scale(income) + scale(taxRate) +
				scale(pcExpend) + scale(pcRevenue) + scale(pcDebt), data = issue1)	

		## this takes and logs posterior draws and then extracts t-VAls ##
			pars <- rmnorm(n = 100, na.omit(coef(mod1)), na.omit(vcov(mod1)))
			coefs <- matrix(rep(NA, 1000), ncol = 10, nrow = 100)
			tVals <- coef(summary(mod1))[, 3]
			t <- matrix(rep(NA, 10), ncol = 10, nrow = 1)
			count <- 1
			for(c in 1:length(coef(mod1))){
				if(is.na(coef(mod1)[c]) == FALSE){
					coefs[, c] <- pars[, count]
					t[, c] <- tVals[count]
					count <- count + 1
				}
			}
			coef1 <- rbind(coef1, coefs)
			dv[j] <- mean(pars[, 1])
			state[j] <- unique(issue$state)[j]
			rSq1 <- rbind(rSq1, summary(mod1)$r.squared)
			
			hold <- data.frame(t, unique(issue$state)[j])
			first <- rbind(first, hold)
		}

## and now the second stage of that model ##
		temp1 <- data.frame(dv, state)
		temp2 <- merge(new, temp1, by = 'state')
		mod2 <- lm(dv ~ squire.mean + (acir.mean < 6) + revenueLimit.mean + spendingLimit.mean + debtRestriction.mean, data = temp2)
		rSq2[i] <- summary(mod2)$r.squared
		hold <- coef(summary(mod2))[, 3]
		pars <- rmnorm(n = 100, coef(mod2), vcov(mod2))
		coef2 <- rbind(coef2, pars)
		second <- rbind(hold, second)

## and now estimate the pooled model ##
		plmModel <- pggls(issue[, 9+i] ~ squire + term + (disposition == 'S') + turnoverWeighted90 + (acir < 6) + unemployment + income + taxRate + pcExpend +
			pcRevenue + pcDebt + revenueLimit + spendingLimit + debtRestriction, data = issue, model = 'pooling')
		rSq[i] <- summary(plmModel)$rsqr
		pars <- rmnorm(n = 100, coef(plmModel), vcov(plmModel))
		coef <- rbind(coef, pars)
	
## give an update ##
		cat('iteration', i, 'of 1,000 complete...\n')

## close the loop ##
	}

## this piece of the script produces the values for Main Text TABLE 1 ##
## summarize the fit and estimates and extract p-vals for the first stage of the two-stage model ##	
	summary(rSq1)
	summary(coef1)
	p <- c()
	for(i in 1:dim(coef1)[2]){
		if(mean(coef1[, i], na.rm = TRUE) > 0){
			p[i] <- length(na.omit(coef1[, i][coef1[, i] < 0])) / length(na.omit(coef1[, i]))
		}else{
			p[i] <- length(na.omit(coef1[, i][coef1[, i] > 0])) / length(na.omit(coef1[, i]))
		}
	}
	names <- c('Intercept', 'Term Limits', 'Divided Government', 'Historical Turnover', 'Unemplyment Rate', 'PC Income', 'Average Tax Burden', 'PC Spending', 'PC Revenue', 'PC Debt')
## this next line will generate the output for "2SLS" "Time varying" estimates in Table 1 ##
	cbind(names, round(colMeans(coef1, na.rm = TRUE), 3), round(colSds(coef1, na.rm = TRUE), 3), round(p, 3))

## summarize the fit and estimates and extract p-vals for the second stage of the two-stage model ##	
	summary(rSq2)
	summary(coef2)
	p <- c()
	for(i in 1:dim(coef2)[2]){
		if(mean(coef2[, i], na.rm = TRUE) > 0){
			p[i] <- length(na.omit(coef2[, i][coef2[, i] < 0])) / length(na.omit(coef2[, i]))
		}else{
			p[i] <- length(na.omit(coef2[, i][coef2[, i] > 0])) / length(na.omit(coef2[, i]))
		}
	}
	names <- c('Intercept', 'Squire Index', 'ACIR Lax', 'Revenue Limit', 'Spending Limit', 'Debt Restriction')
## this next line will generate the output for "2SLS" "Time stable" estimates in Table 1 ##
	cbind(names, round(colMeans(coef2), 3), round(colSds(coef2), 3), round(p, 3))

## summarize the fit and estimates and extract p-vals for the pooled model ##
	summary(rSq)
	summary(coef)
	p <- c()
	for(i in 1:dim(coef)[2]){
		if(mean(coef[, i], na.rm = TRUE) > 0){
			p[i] <- length(na.omit(coef[, i][coef[, i] < 0])) / length(na.omit(coef[, i]))
		}else{
			p[i] <- length(na.omit(coef[, i][coef[, i] > 0])) / length(na.omit(coef[, i]))
		}
	}
	names <- c('Intercept', 'Squire Index', 'Term Limits', 'Divided Government', 'Historical Turnover', 'ACIR Lax', 'Unemplyment Rate', 'PC Income', 'Average Tax Burden', 'PC Spending', 'PC Revenue', 'PC Debt', 'Revenue Limit', 'Spending Limit', 'Debt Restriction')
## this next line will generate the output for "Pooled FGLS" in Table 1 ##
	cbind(names, round(colMeans(coef), 3), round(colSds(coef), 3), round(p, 3))

## now plot individual state estimates ##
## this is FIGURE 3 in the Supporting Information ##
## single graph plots of t-vals densities ##
	colnames(first) <- c('Intercept', 'Term Limits', 'Divided Government', 'Historical Turnover', 'Unemplyment Rate',
		'PC Income', 'Average Tax Burden', 'PC Spending', 'PC Revenue', 'PC Debt', 'state')

## note that the loop generating this figure will produce errors ##
## when attempting to plot tVals for parameters that were omitted ##
## from some models (for example, term limits are omitted from ##
## each state where term limits were never implemented) there should ##
## be 29 such error clusters in total ##
	pdf('tValsFirst.pdf', width = 18, height = 12)
		par(mfrow = c(5, 8), oma = c(5, 5, 5, 0), mar = c(2, 2, 2, 2))

		for(s in unique(first$state)){
			tData <- first[first$state == s, c(2:10)]
			tData[tData > 30 | tData < -30] <- NA
			plot(1, 1, type = 'n',
				xlim = c(-20, 10), 
	    		ylim = c(.9, (length(colnames(first)))), 
	    		bty = "n",
	    		yaxt = "n",
	    		ylab = "n", 
	    		main = s,
	    		axes = FALSE)
			axis(1, at = c(-10, -5, 0, 5, 10))
			for (i in 1:length(colnames(tData))) {
			  abline(h = i, col = "gray")
			  text((-20), (i + .3), colnames(tData)[i], pos = 4, cex = .75)
			  if(length(na.omit(tData[,i])) > 0){
				  d <- try(density(na.omit(tData[,i])))
				  d$y <- try(d$y/max(d$y)*.85 + i)
				  try(lines(d, lwd = 1.5, col = "blue"))
			  }
			}

		rect( -1.65, 1, 1.65, 10, col=rgb(.1,.1,.1,alpha=.1), border="transparent" )
		}
		mtext("Bootstrapped First Stage T-Values by State", side=3, line = 1, cex=2, font = 2, outer = TRUE)
	dev.off()

## interactive model (sampling will cause small deviations from our results) ##
## this loop estimates the models that produce ##
## the interactive effects shown in Main Text FIGURE 3 ##
## the table of these results is Supporting Information TABLE 5 ##
	coef <- c()
	r2 <- c()
	for(i in 1:1000){
		plmModel <- pggls(issue[, 9+i] ~ squire * term + (disposition == 'S') + (acir < 6) + turnoverWeighted90 + unemployment + income + taxRate + pcExpend + pcRevenue + pcDebt + revenueLimit + spendingLimit + debtRestriction, data = issue, model = 'pooling')
		r2[i] <- summary(plmModel)$rsqr
		pars <- rmnorm(n = 100, coef(plmModel), vcov(plmModel))
		coef <- rbind(coef, pars)
	}

	names <- c('Intercept', 'Squire Index', 'Term Limits', 'Divided', 'ACIR Lax', 'Historical Turnover', 'Unemplyment Rate', 'PC Income', 'Tax Rate', 'PC Spending', 'PC Revenue', 'PC Debt', 'Revenue Limit', 'Spending Limit', 'Debt Restriction', 'Interaction')

	cbind(names, round(colMeans(coef), 3), round(colSds(coef), 3))

## make effect plot ##
	pdf('interactionEffect.pdf', width = 7, height = 7)
		plot(1, 1, type = 'n',
			xlab = 'Squire Index',
			ylab = 'Change to Credit Risk',
			xlim = c(0, 0.65),
			ylim = c(-0.5, 1),
			main = 'Marginal Effect of Term Limits on Credit Risk\nConditioned by Legislative Capacity',
			axes = FALSE)
		axis(1)
		axis(2)
		range <- seq(0.025, 0.65, 0.001)
		lines(c(c(0, 0.65)), c(0, 0), col = rgb(0, 0, 0, 0.3), lty = 3, lwd = 2)

		hi <- c()
		lo <- c()
		md <- c()

		for(i in 1:length(range)){

			base <- coef[, 1] + 
				coef[, 2] * range[i] + 
				coef[, 4] * mean(issue$disposition == 'S') + 
				coef[, 5] * mean(issue$turnoverWeighted90) + 
				coef[, 6] * mean(issue$acir < 6) + 
				coef[, 7] * mean(issue$unemployment) + 
				coef[, 8] * mean(issue$income) + 
				coef[, 9] * mean(issue$taxRate) + 
				coef[, 10] * mean(issue$pcExpend) + 
				coef[, 11] * mean(issue$pcRevenue) + 
				coef[, 12] * mean(issue$pcDebt) + 
				coef[, 13] * mean(issue$revenueLimit) + 
				coef[, 14] * mean(issue$spendingLimit) + 
				coef[, 15] * mean(issue$debtRestriction)

			change <- coef[, 1] + 
				coef[, 2] * range[i] + 
				coef[, 3] + 
				coef[, 4] * mean(issue$disposition == 'S') + 
				coef[, 5] * mean(issue$turnoverWeighted90) + 
				coef[, 6] * mean(issue$acir < 6) + 
				coef[, 7] * mean(issue$unemployment) + 
				coef[, 8] * mean(issue$income) + 
				coef[, 9] * mean(issue$taxRate) + 
				coef[, 10] * mean(issue$pcExpend) + 
				coef[, 11] * mean(issue$pcRevenue) + 
				coef[, 12] * mean(issue$pcDebt) + 
				coef[, 13] * mean(issue$revenueLimit) + 
				coef[, 14] * mean(issue$spendingLimit) + 
				coef[, 15] * mean(issue$debtRestriction) + 
				coef[, 16] * range[i]
				
			hi[i] <- quantile(change - base, 0.975)
			lo[i] <- quantile(change - base, 0.025)
			md[i] <- quantile(change - base, 0.5)

		}

		lines(range, md, col = rgb(0, 0, 0, 0.5), lwd = 2)
		polygon(c(range, rev(range)), c(hi, rev(lo)), col = rgb(0, 0, 0, 0.3), border = FALSE)
	dev.off()

## ** ## ** The remainder of this script reproduces tables and figures in the Supporting Information ** ## ** ##
## the following bit creates Supporting Information FIGURE 1 ##
## which compares the factor analytic risk estimates to numerical means ##
pdf('risk.pdf', height = 10, width = 10)
	par(mfrow = c(2, 2))

		hist(data$average,
			col = rgb(0, 0, 0, 0.25),
			xlab = 'Numerical Mean Risk',
			main = 'Histogram of Bond Rating Numerical Means',
			border = rgb(0, 0, 0, 0.25),
			axes = FALSE)
		axis(1, at = c(1:10))
		axis(2)

		hist(data$mean,
			col = rgb(0, 0, 0, 0.25),
			xlim = c(-2, 3),
			xlab = 'Estimated Bond Risk',
			main = 'Histogram of\nOrdinal Factor Analysis Risk Estimate',
			border = rgb(0, 0, 0, 0.25),
			axes = FALSE)
		axis(1, at = c(-2:3))
		axis(2)

		plot(data$mean, data$sd, type = 'n',
			xlim = c(-2.5, 3.5),
			ylim = c(0, 0.12),
			xlab = 'Mean Estimated Risk',
			ylab = 'Standard Deviation of Estimated Risk',
			main = 'Certainty of Risk Estimates',
			axes = FALSE)
		axis(1)
		axis(2)
		points(data$mean, data$sd, col = rgb(0, 0, 0, 0.1), pch = 2)

		plot(data$average, data$mean, type = 'n',
			xlim = c(0, 10),
			ylim = c(-2, 3.5),
			xlab = 'Numerical Mean Risk',
			ylab = 'Factor Analysis Risk Estimate',
			main = 'Correlation Between\nRisk Estimates',
			axes = FALSE)
		axis(1)
		axis(2)
		points(data$average+rnorm(data$average, 0, 0.1),
			data$mean+rnorm(data$average, 0, 0.1), pch = 2, col = rgb(0, 0, 0, 0.1))
		abline(lm(data$mean ~ data$average), lwd = 2, col = rgb(0, 0, 0, 0.25))
		lines(lowess(data$average[is.na(data$average) == FALSE], data$mean[is.na(data$average) == FALSE]), col = rgb(1, 0, 0, 0.25), lwd = 2)
dev.off()

## now, model risk using the numerical means of the risk estimates ##
## this model is given in TABLE 1 of the Supporting Information ##
	numerical <- pggls(average ~ squire + (disposition == 'S') + turnoverWeighted90 + (acir < 6) + term + unemployment + income + taxRate + pcExpend + pcRevenue + pcDebt + revenueLimit + spendingLimit + debtRestriction, data = issue, model = 'pooling')
	summary(numerical)

## the model with panel-corrected standard errors ##
## this is given in TABLE 2 of the Supporting Information ##
	linear <- lm(mean ~ squire + ((disposition == 'S') + turnoverWeighted90) + (acir < 6) + term + unemployment + income + taxRate + pcExpend + pcRevenue + pcDebt + revenueLimit + spendingLimit + debtRestriction, data = issue)
	summary(linear)
	
	newError <- pcse(linear, issue$state, issue$year)
	summary(newError)

## this model includes the lagged estimates of risk ##
## it is given in TABLE 3 in the Supporting Information ##	
	plmLag <- pggls(mean ~ lag(mean) + squire + ((disposition == 'S') + turnoverWeighted90) + (acir < 6) + term + unemployment + income + taxRate + pcExpend + pcRevenue + pcDebt + revenueLimit + spendingLimit + debtRestriction, data = issue, model = 'pooling')
	summary(plmLag)

## this model includes interactions with term limits, turnover, and divided government ##
## it is given in TABLE 4 of the Supporting Information ##
	plmModel <- pggls(mean ~ squire * (term  + turnoverWeighted90 + (disposition == 'S')) + (acir < 6) + unemployment + income + taxRate + pcExpend + pcRevenue + pcDebt + revenueLimit + spendingLimit + debtRestriction, data = issue, model = 'pooling')
	summary(plmModel)

## this code reproduces the correlation matrix ##
## this is FIGURE 2 in the Supporting Information ##
	frame <- data.frame(data$squire, data$term, data$turnoverWeighted90, (data$disposition == 'S'), (data$acir < 6), data$unemployment, data$income,
		data$taxRate,  data$pcExpend, data$pcRevenue,  data$pcDebt, data$revenueLimit, data$spendingLimit, data$debtRestriction)

	colnames(frame) <- c('Squire', 'Term Limits', 'Historical Turnover', 'Divided Government', 'ACIR Lax', 'Unemployment', 'Income', 'Tax Rate', 'Expenditures',
		'Revenue', 'Debt', 'Revenue Limit', 'Spending Limit', 'Debt Restriction')
	cors <- cor(frame)
pdf('correlationMatrix.pdf', width = 12, height = 12)
	corrplot(cors, method = 'color', addCoef.col = 'black', cl.pos = 'n', tl.col='black')
dev.off()

## bootstrapped full model (sampling will cause small deviations from our results) ##
## because the above fgls models require a balanced sample we estimate standard OLS models ##
## but use just 1/4 of the data -- the key results remain ##
## these are summarized in TABLE 6 of the Supporting Information ##
## it is likely that there will small differences between the cardinal values ##
## recovered here and those reported in teh SI file as a function of the resampling ##
	t <- c()
	coef <- c()
	r2 <- c()
	set.seed(0624)
	for(i in 1:1000){
		new <- round(runif(160, .5, 640.4999))
		bsLm <- lm(mean ~ squire + ((disposition == 'S') + turnoverWeighted90) + (acir < 6) + term + unemployment + income + taxRate + pcExpend +
			pcRevenue + pcDebt + revenueLimit + spendingLimit + debtRestriction, data = issue[new, ])
		t[i] <- summary(bsLm)$coefficients[2, 3]
		coef <- rbind(coef, rmnorm(100, na.omit(coef(bsLm)), vcov(bsLm)))
		r2[i] <- summary(bsLm)$r.squared
	}
	summary(r2)
	summary(t)
	quantile(t, 0.025)
	cbind(round(colMeans(coef), 3), round(colSds(coef), 3))
	
## the following loops work through bootstraps of the 2-stage model ##
## first, predict a distribution of risk estimates for each state ##
	data$predSd <- c()
	data$predMean <- c()
	data$draw <- c()
	for(s in states){
		model <- lm(mean ~ disp + scale(unemployment) + term + scale(turnoverWeighted90) + scale(income) + scale(taxRate) + scale(pcExpend) + scale(pcRevenue) + scale(pcDebt), data = data[data$state == s, ])
		coef <- rmnorm(1000, na.omit(coef(model)), vcov(model))
		data$predMean[data$state == s] <- mean(coef[, 1])
		data$predSd[data$state == s] <- sd(coef[, 1])
	}

## now collapse the relevant data to state observations ##	
## and estimate models ##	
## these results are given in TABLE 7 in the Supporting Information ##
## note that these are estimated dvs, so your results will vary from those in the document ##
	new <- summaryBy(predMean + predSd + squire + acir + revenueLimit + spendingLimit + debtRestriction ~ state, FUN = mean, data = data[data$noIssue == 0, ])
	
	model1 <- lm(predMean.mean ~ squire.mean, data = new)
	summary(model1)

	model2 <- lm(predMean.mean ~ squire.mean + (acir.mean < 6) + revenueLimit.mean + spendingLimit.mean + debtRestriction.mean, data = new)
	summary(model2)

## now iterate the estimation via bootstrap (sampling will cause small deviations from our results) ##
## we have to rig it a bit to make certain we get variation on all our variables ##
## so we will reject any models in which there is no variation over one or more predictors ##	
## the summary figure is FIGURE 4 in the Supporting Information ##
## it is likely that there will small differences between the cardinal values ##
## recovered here and those reported in teh SI file as a function of the resampling ##
	coef1 <- c()
	coef2 <- c()
	set.seed(1)
	i <- 1
	while(i <= 1000){
		obs <- round(runif(40, 0.5, 40.49999))
		
		model <- lm(predMean.mean ~ squire.mean + (acir.mean < 6) + revenueLimit.mean + spendingLimit.mean + debtRestriction.mean, data = new[obs, ])
		if(length(coef(model)[is.na(coef(model)) == TRUE]) == 0){
			pars2 <- rmnorm(100, na.omit(coef(model)), vcov(model))
			coef2 <- rbind(coef2, pars2)

			model <- lm(predMean.mean ~ squire.mean, data = new[obs, ])
			pars1 <- rmnorm(100, coef(model), vcov(model))
			coef1 <- rbind(coef1, pars1)
			
			i <- i + 1
		}
	}

	pdf('bootStrappedCoef.pdf')
		hist(coef1[, 2],
			xlim = c(-2, 11),
			xlab = 'Parameter',
			ylab = 'Density',
			main = 'Bootstrapped Parameter Estimates',
			col = rgb(1, 0, 0, 0.4),
			border = rgb(1, 0, 0, 0.1))
		arrows(0, 15000, 0, 0, col = rgb(0, 0, 1, 0.4), length = 0.125, lwd = 2)
		text(0, 16000, paste(round((length(coef1[, 2][coef1[, 2] > 0])/length(coef1[, 2])) * 100), 'th percentile', sep = ''), col = rgb(0, 0, 1, 0.7))
	dev.off()