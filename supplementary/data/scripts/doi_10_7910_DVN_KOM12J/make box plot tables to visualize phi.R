attr(post.f1.full.binary.scores, "dimnames")
pharma.factors <- read.delim(file=file.choose(), colClasses="factor")

print.pharma.factors <- function(data, phi.start, phi.end, title.text, pharma.factors){
	phi.out <- data[,phi.start:phi.end]
	phi.mean <- colMeans(phi.out)

	high.low <- apply(phi.out, 2, quantile, probs = c(0.025, 0.975),  na.rm = TRUE)
	high.low <- t(high.low)
	countries <- as.character(pharma.factors$Country)

	phi.data <- cbind(countries, phi.mean, high.low)
	colnames(phi.data) <- c("country", "mean.value", "lower", "upper")
	phi.data <- as.data.frame(phi.data)

	phi.data$mean.value  <- as.numeric(as.character(phi.data$mean.value))
	phi.data$lower  <- as.numeric(as.character(phi.data$lower))
	phi.data$upper  <- as.numeric(as.character(phi.data$upper))

	phi.data <- phi.data[order(phi.data$mean.value),]
	phi.data$country <- reorder(phi.data$country, phi.data$mean.value)

	p <- ggplot(phi.data, aes(x=country, y=mean.value))
	p <- p + geom_point()
	p <- p + geom_errorbar(aes(ymin=lower, ymax=upper)) 
	p <- p + coord_flip()
	p <- p+ labs(list(title = title.text, x = "Country", y = "Index Value"))
	print(p)
	p <- p + ylim(-3.25,3.25)
	}




print.pharma.factors.90 <- function(data, phi.start, phi.end, title.text, factors.data){
	phi.out <- data[,phi.start:phi.end]
	phi.mean <- colMeans(phi.out)

	high.low <- apply(phi.out, 2, quantile, probs = c(0.05, 0.95),  na.rm = TRUE)
	high.low <- t(high.low)
	countries <- as.character(factors.data$Country)

	phi.data <- cbind(countries, phi.mean, high.low)
	colnames(phi.data) <- c("country", "mean.value", "lower", "upper")
	phi.data <- as.data.frame(phi.data)

	phi.data$mean.value  <- as.numeric(as.character(phi.data$mean.value))
	phi.data$lower  <- as.numeric(as.character(phi.data$lower))
	phi.data$upper  <- as.numeric(as.character(phi.data$upper))

	phi.data <- phi.data[order(phi.data$mean.value),]
	phi.data$country <- reorder(phi.data$country, phi.data$mean.value)

	p <- ggplot(phi.data, aes(x=country, y=mean.value))
	p <- p + geom_point()
	p <- p + geom_errorbar(aes(ymin=lower, ymax=upper)) 
	p <- p + coord_flip()
	p <- p+ labs(list(title = title.text, x = "Country", y = "Index Value\n(90% credible intervales)"))
	p <- p + ylim(-3.5,3.5)
	print(p)
	}




flip.print.pharma.factors.90 <- function(data, phi.start, phi.end, title.text, factors.data){
	phi.out <- data[,phi.start:phi.end]
	phi.mean <- colMeans(phi.out)

	high.low <- apply(phi.out, 2, quantile, probs = c(0.05, 0.95),  na.rm = TRUE)
	high.low <- t(high.low)
	countries <- as.character(factors.data$Country)

	phi.data <- cbind(countries, phi.mean, high.low)
	colnames(phi.data) <- c("country", "mean.value", "lower", "upper")
	phi.data <- as.data.frame(phi.data)

	phi.data$mean.value  <- as.numeric(as.character(phi.data$mean.value))
	phi.data$lower  <- as.numeric(as.character(phi.data$lower))
	phi.data$upper  <- as.numeric(as.character(phi.data$upper))


	phi.data$mean.value  <- phi.data$mean.value*-1
	phi.data$lower  <- phi.data$lower*-1
	phi.data$upper  <- phi.data$upper*-1


	phi.data <- phi.data[order(phi.data$mean.value),]
	phi.data$country <- reorder(phi.data$country, phi.data$mean.value)

	p <- ggplot(phi.data, aes(x=country, y=mean.value))
	p <- p + geom_point()
	p <- p + geom_errorbar(aes(ymin=lower, ymax=upper)) 
	p <- p + coord_flip()
	p <- p+ labs(list(title = title.text, x = "Country", y = "Index Value\n(90% credible intervales)"))
	p <- p + ylim(-3.5,3.5)
	print(p)
	}



rank.countries <- function(countries, data, phi.start, phi.end, model){
	phi.out <- data[,phi.start:phi.end]
	phi.mean <- colMeans(phi.out)
	rank.countries <- as.matrix(rank(phi.mean))
	colnames(rank.countries) <- model
	countries <- cbind(countries, rank.countries)
	return(countries)
	}



ranked.countries <- as.character(pharma.factors$Country)

central.medical.store.data <- subset(pharma.factors, X7.02.01 == 1) 

library(ggplot2)
library(MCMCpack)


central.medical.store.data <- subset(pharma.factors, X7.02.01 == 1) 



load(file=file.choose())
ls()
data <- basic.intitutions
colnames(data)


title.text <- "Regulatory Infrastructure and Good Practices"
model <- ""
phi.start <- 29
phi.end <- length(pharma.factors$Country) + phi.start -1


ranked.countries <- rank.countries(ranked.countries, data, phi.start, phi.end, model)
print.pharma.factors.90(data, phi.start, phi.end, title.text, pharma.factors)
flip.print.pharma.factors.90(data, phi.start, phi.end, title.text, pharma.factors)
heidel.diag(data)
summary(data)

print.pharma.factors(data, phi.start, phi.end, title.text, pharma.factors)


cms.ranked.countries <- as.character(central.medical.store.data$Country)
phi.end <- length(central.medical.store.data$Country) + phi.start -1
ranked.countries <- rank.countries(cms.ranked.countries, data, phi.start, phi.end, model)
print.pharma.factors.90(data, phi.start, phi.end, title.text, central.medical.store.data)
print.pharma.factors(data, phi.start, phi.end, title.text, central.medical.store.data)





table.out <- summary(data)$statistics
rownames(table.out)
table.out <- table.out[1:28,]
write.table(table.out, file=file.choose(), sep="\t")



save(summary(reg.and.monitor.private.regular.inspections)$statistics, file=file.choose())
save(summary(reg.and.monitor.private.annual.inspections)$statistics, file=file.choose())
save(summary(post.f1.IPR.scores)$statistics, file=file.choose())
