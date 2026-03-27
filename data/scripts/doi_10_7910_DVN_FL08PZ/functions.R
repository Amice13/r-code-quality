########functions

###add terms to a formula object
addTerms <- function(eq, terms) {
	e <- as.character(eq)
	return(as.formula(paste("y ~ ", paste(terms, collapse="+"), "+", 
			e[3])))
}

####fit a sibling FE model given an equation
fitSibModel <- function(eq, returnRestricted = FALSE) {
	##identify set of respondents with a sibling
		eq0 <- addTerms(eq, c("clus", "grad"))
	##get the set of respondents that have complete data
		r <- lm(eq0, data = data)
		d <- r$model
	##find the set of respondents with a sibling
		tab <- table(d$clus)
		wh <- which(is.element(d$clus, as.numeric(names(
			tab[which(tab==2)]))))

	##subtract of mean of each sibling pair
		d <- d[wh,]
		ag <- aggregate(data.matrix(d) ~ d$clus, FUN = mean)
		rownames(ag) <- ag[,1]
		ag <- ag[,-1]
		d2 <- d - ag[as.character(d$clus),]

		sib <- lm(eq, data = d2)
		if (!returnRestricted) return(sib)
		if (returnRestricted) return(d)
}

getSummaryTerm <- function(mod, cname) {
	co <- coef(mod)[cname]
	star <- ifelse(summary(mod)$coefficients[cname,4] < 0.05,
		"*", "")
	entry <- paste( round(co, 3), star, sep='')
	return( entry )
}

extract.summary <- function(reg.res) {
	sum.tab <- array('', c(3, length(reg.res)*2))
	rownames(sum.tab) <- c("Cognition", "HUI", "Walking Speed")
	cn <- rep('', ncol(sum.tab))
	i <- 1
	for (j in 1:length(reg.res)) {
		cn[i] <- paste(names(reg.res)[j], "pooled")
		i <- i+1
		cn[i] <- paste(names(reg.res)[j], "sib")
		i <- i+1
	}
	colnames(sum.tab) <- cn

	for (j in 1:ncol(sum.tab)) {
		rname <- strsplit(cn[j], " ")[[1]]
		ex <- paste("COG", rname[2])
		wh <- grep(ex, names(reg.res[[rname[1]]]))
		su <- summary(reg.res[[rname[1]]][[wh]])$coefficients
		sum.tab[1,j] <- paste(round(su[2,1],3), 
			ifelse(su[2,4] < 0.05, "*", ""), sep="")

		ex <- paste("HUI", rname[2])
		wh <- grep(ex, names(reg.res[[rname[1]]]))
		su <- summary(reg.res[[rname[1]]][[wh]])$coefficients
		sum.tab[2,j] <- paste(round(su[2,1],3), 
			ifelse(su[2,4] < 0.05, "*", ""), sep="")

		ex <- paste("WALK", rname[2])
		wh <- grep(ex, names(reg.res[[rname[1]]]))
		if (length(wh)==0) next
		su <- summary(reg.res[[rname[1]]][[wh]])$coefficients
		sum.tab[3,j] <- paste(round(su[2,1],3), 
			ifelse(su[2,4] < 0.05, "*", ""), sep="")		
	}
	return(sum.tab)
	
}

###figures

plot.health <- function(eqn, health = c("cog04", "hui04", "educ04"),
	xlab = '', main = '', hlab = c("Cognition", "HUI", "Education"),
	pos = c(90, 190, 290), err = TRUE, bs = 1000, add = FALSE,
	adj = 0, y.pos = 1:3, year = '') {
	y.pos <- 1:length(health)
if (!add) {
	plot(0,0, pch='', xlim = c(-.5,.5), ylim = c(.85,length(health)+.75), 
		xlab = xlab,
		ylab = '', main = main, axes=FALSE)
	abline(v = 0, lty=3, col='grey')
	axis(1)
}
	z <- c(-2, 2)
	i <- 1
	for (h in 1:length(health)) {
		e <- as.character(eqn)[3]
		data[[paste("z", health[h], sep="")]] <- scale(data[[health[h]]])
		eq <- as.formula(paste("y ~", paste("z", health[h], sep=""), "+", e))
		m <- glm(eq, data = data, family = binomial(link = 'probit'))
		if (health[h] == "walk_speed2") z <- rev(z)
		if (err) {
			fun <- function(j) {
				d <- sample(1:nrow(m$model), size = nrow(m$model),
					replace = TRUE)
				d <- m$model[d,]
				m0 <- glm(eq, data = d, family = binomial(link =
					'probit'))
				p <- pnorm( qnorm(mean(m0$model$y)) + 
					z*coef(m0)[2])
				return(p[2]-p[1])
			}
			boot <- unlist(lapply(1:1000, fun))
		}
		
		pred <- pnorm( qnorm(mean(m$model$y, na.rm=TRUE)) + z*coef(m)[2])
		points(pred[2]-pred[1], y.pos[h]+adj, pch=20)
		q <- quantile(boot, c(.025, .975))
		segments(y0 = y.pos[h]+adj, y1 = y.pos[h]+adj, x0 = q[1], x1 = q[2])
		if (!add) text(pred[2]-pred[1], y.pos[h]+adj, hlab[i], pos = 3)
		text(q[2], y.pos[h]+adj, year, pos = 4, cex = .67)
		i <- i+1
		if (health[h] == "walk_speed2") z <- rev(z)
	}
}

reg.out2 <- function(eqn, dvs, restrict.sample = FALSE) {
	####regression table
	####one column for each specification (pooled probit and 
	#	mean differenced sibling), set of IVs

	e <- as.character(eqn)

	reg.res <- list()
	for (D in dvs) {
		reg.res[[D]] <- list()

		data$y <- data[[D]]

	ivs <- c("cog04", "hui04")
	if(length(grep("12", D))==1) ivs <- c("cog10", "hui10",
		"walk_speed2")

		eq <- as.formula(paste("y ~ ", paste(ivs, collapse="+"), "+", e[3]))
		###grads only
		reg.res[[D]][["grads only"]] <- glm(eq, data = data[
			grep("G", data$id),], family = binomial(link = "probit"))

		###pooled grads and sibs
		reg.res[[D]][["all pooled"]] <- glm(eq, data = data, 
			family = binomial(link = 'probit'))

		##identify set of respondents with a sibling
		eq0 <- as.formula(paste("y ~ ", paste(ivs, collapse="+"), 
			"+", e[3], "+ clus"))
		r <- lm(eq0, data = data)
		d <- r$model
		tab <- table(d$clus)
		wh <- which(is.element(d$clus, as.numeric(names(
			tab[which(tab==2)]))))

		##subtract of mean of each sibling pair
		d <- r$model[wh,]
		ag <- aggregate(data.matrix(d) ~ d$clus, FUN = mean)
		rownames(ag) <- ag[,1]
		ag <- ag[,-1]
		d2 <- d - ag[as.character(d$clus),]

		reg.res[[D]][["all sib"]] <- lm(eq, data = d2)
		if (restrict.sample) reg.res[[D]][["all pool-sib"]] <-
			glm(eq, data = d, family = binomial(link = "probit"))
	}

	return(reg.res)
}


extract.summary2 <- function(reg.res) {
	sum.tab <- array('', c(3, length(reg.res)*3))
	rownames(sum.tab) <- c("Cognition", "HUI", "Walking Speed")
	cn <- rep('', ncol(sum.tab))
	i <- 1
	for (j in 1:length(reg.res)) {
		cn[i] <- paste(names(reg.res)[j], "grad")
		i <- i+1
		cn[i] <- paste(names(reg.res)[j], "pooled")
		i <- i+1
		cn[i] <- paste(names(reg.res)[j], "sib")
		i <- i+1
	}
	colnames(sum.tab) <- cn

	for (j in 1:ncol(sum.tab)) {
		rname <- strsplit(cn[j], " ")[[1]]
		ex <- paste("cog[0-9]+", rname[2])
		wh <- grep(ex, names(reg.res[[rname[1]]]))
		su <- summary(reg.res[[rname[1]]][[wh]])$coefficients
		sum.tab[1,j] <- paste(round(su[2,1],3), 
			ifelse(su[2,4] < 0.05, "*", ""), sep="")

		ex <- paste("hui[0-9]+", rname[2])
		wh <- grep(ex, names(reg.res[[rname[1]]]))
		su <- summary(reg.res[[rname[1]]][[wh]])$coefficients
		sum.tab[2,j] <- paste(round(su[2,1],3), 
			ifelse(su[2,4] < 0.05, "*", ""), sep="")

		ex <- paste("walk_speed2", rname[2])
		wh <- grep(ex, names(reg.res[[rname[1]]]))
		if (length(wh)==0) next
		su <- summary(reg.res[[rname[1]]][[wh]])$coefficients
		sum.tab[3,j] <- paste(round(su[2,1],3), 
			ifelse(su[2,4] < 0.05, "*", ""), sep="")		
	}

	return(sum.tab)
}


