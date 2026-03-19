contr.results <- list(cog = list(), hui = list(),
	walk = list())
contr.summary <- array('', c(3, 6))
rownames(contr.summary) <- c("Cognition","HUI","Walking Speed")
colnames(contr.summary) <- c("2008 pooled", "2008 sibling",
	"2010 pooled", "2010 sibling", "2012 pooled", "2012 sibling")
######Cognition Tables behind the estimates in the summary table (1st row)
##2008
#pooled
data$y <- data$give08
health.terms <- "cog04"
eq <- addTerms(eqn, health.terms)
summary(contr.results[['cog']][['2008']] <- 
	glm(eq,
	data = data, family = binomial(link = 'probit')))
contr.summary['Cognition','2008 pooled'] <- getSummaryTerm(
	contr.results[['cog']][['2008']], health.terms)
#sibling
contr.results[['cog']][['2008 sib']] <- fitSibModel(eq)
contr.summary['Cognition','2008 sibling'] <- getSummaryTerm(
	contr.results[['cog']][['2008 sib']], health.terms)

##2010
#pooled
data$y <- data$give10
health.terms <- "cog04"
eq <- addTerms(eqn, health.terms)
summary(contr.results[['cog']][['2010']] <- 
	glm(eq,
	data = data, family = binomial(link = 'probit')))
contr.summary['Cognition','2010 pooled'] <- getSummaryTerm(
	contr.results[['cog']][['2010']], health.terms)
#sibling
contr.results[['cog']][['2010 sib']] <- fitSibModel(eq)
contr.summary['Cognition','2010 sibling'] <- getSummaryTerm(
	contr.results[['cog']][['2010 sib']], health.terms)

##2012
#pooled
data$y <- data$give12
health.terms <- "cog10"
eq <- addTerms(eqn, health.terms)
summary(contr.results[['cog']][['2012']] <- 
	glm(eq,
	data = data, family = binomial(link = 'probit')))
contr.summary['Cognition','2012 pooled'] <- getSummaryTerm(
	contr.results[['cog']][['2012']], health.terms)
#sibling
contr.results[['cog']][['2012 sib']] <- fitSibModel(eq)
contr.summary['Cognition','2012 sibling'] <- getSummaryTerm(
	contr.results[['cog']][['2012 sib']], health.terms)

######HUI Tables behind the estimates in the summary table (2nd row)
##2008
data$y <- data$give08
health.terms <- "hui04"
eq <- addTerms(eqn, health.terms)
summary(contr.results[['hui']][['2008']] <- 
	glm(eq,
	data = data, family = binomial(link = 'probit')))
contr.summary['HUI','2008 pooled'] <- getSummaryTerm(
	contr.results[['hui']][['2008']], health.terms)
#sibling
contr.results[['hui']][['2008 sib']] <- fitSibModel(eq)
contr.summary['HUI','2008 sibling'] <- getSummaryTerm(
	contr.results[['hui']][['2008 sib']], health.terms)

##2010
#pooled
data$y <- data$give10
health.terms <- "hui04"
eq <- addTerms(eqn, health.terms)
summary(contr.results[['hui']][['2010']] <- 
	glm(eq,
	data = data, family = binomial(link = 'probit')))
contr.summary['HUI','2010 pooled'] <- getSummaryTerm(
	contr.results[['hui']][['2010']], health.terms)
#sibling
contr.results[['hui']][['2010 sib']] <- fitSibModel(eq)
contr.summary['HUI','2010 sibling'] <- getSummaryTerm(
	contr.results[['hui']][['2010 sib']], health.terms)

##2012
#pooled
data$y <- data$give12
health.terms <- "hui10"
eq <- addTerms(eqn, health.terms)
summary(contr.results[['hui']][['2012']] <- 
	glm(eq,
	data = data, family = binomial(link = 'probit')))
contr.summary['HUI','2012 pooled'] <- getSummaryTerm(
	contr.results[['hui']][['2012']], health.terms)
#sibling
contr.results[['hui']][['2012 sib']] <- fitSibModel(eq)
contr.summary['HUI','2012 sibling'] <- getSummaryTerm(
	contr.results[['hui']][['2012 sib']], health.terms)


######Walking Speed Tables behind the estimates in the summary table (3nd row)
##2012
#pooled
data$y <- data$give12
health.terms <- c("walking_speed", "walk_speed_mis")
eq <- addTerms(eqn, health.terms)
summary(contr.results[['walk']][['2012']] <- 
	glm(eq,
	data = data, family = binomial(link = 'probit')))
contr.summary['Walking Speed','2012 pooled'] <- getSummaryTerm(
	contr.results[['walk']][['2012']], health.terms[1])
#sibling
contr.results[['walk']][['2012 sib']] <- fitSibModel(eq)
contr.summary['Walking Speed','2012 sibling'] <- getSummaryTerm(
	contr.results[['walk']][['2012 sib']], health.terms[1])


####################################voting summary table
contr.summary.table <- stargazer(contr.summary, type='text')

########Tables for appendix
omit.ex <- "^(fl|mn|ca|il|az|tx|colo|other_state|Constant)"
appendix.results <- list()

#######regression results from summary table
##cog
ccog.summary <- stargazer(contr.results[[1]], type = 'text',
	omit = omit.ex, column.labels = names(contr.results[[1]]))
##hui
chui.summary <- stargazer(contr.results[[2]], type = 'text',
	omit = omit.ex, column.labels = names(contr.results[[2]]))
##walking
cwalk.summary <- stargazer(contr.results[[3]], type = 'text',
	omit = omit.ex, column.labels = names(contr.results[[3]]))

##########Voting appendix tables
##2008
data$y <- data$give08
health.terms <- c("cog04", "hui04")
eq <- addTerms(eqn, health.terms)
###grads only
appendix.results[['Grads Only 2008']] <- 
	glm(eq, data = subset(data, grad == 1), 
	family = binomial(link = 'probit'))
###pooled
appendix.results[['Pooled 2008']] <- 
	glm(eq, data = data, 
	family = binomial(link = 'probit'))
###sibling model
appendix.results[['Sib 2008']] <- fitSibModel(eq)

##2010
data$y <- data$give10
health.terms <- c("cog04", "hui04")
eq <- addTerms(eqn, health.terms)
###grads only
appendix.results[['Grads Only 2010']] <- 
	glm(eq, data = subset(data, grad == 1), 
	family = binomial(link = 'probit'))
###pooled
appendix.results[['Pooled 2010']] <- 
	glm(eq, data = data, 
	family = binomial(link = 'probit'))
###sibling model
appendix.results[['Sib 2010']] <- fitSibModel(eq)

##2012
data$y <- data$give12
health.terms <- c("cog10", "hui10", "walking_speed", 
	"walk_speed_mis")
eq <- addTerms(eqn, health.terms)
###grads only
appendix.results[['Grads Only 2012']] <- 
	glm(eq, data = subset(data, grad == 1), 
	family = binomial(link = 'probit'))
###pooled
appendix.results[['Pooled 2012']] <- 
	glm(eq, data = data, 
	family = binomial(link = 'probit'))
###sibling model
appendix.results[['Sib 2012']] <- fitSibModel(eq)

contr.appendix.full <- stargazer(appendix.results, type = 'text',
	omit = omit.ex, column.labels = names(appendix.results))

#############restricted sample estimation.....
##2008
data$y <- data$give08
health.terms <- c("cog04", "hui04")
eq <- addTerms(eqn, health.terms)
###sibling model
rdat <- fitSibModel(eq, returnRestricted = TRUE)
appendix.results[['Grads Only 2008']] <- 
	glm(eq, data = subset(rdat, grad == 1), 
	family = binomial(link = 'probit'))
###pooled
appendix.results[['Pooled 2008']] <- 
	glm(eq, data = rdat, 
	family = binomial(link = 'probit'))
###sibling model
appendix.results[['Sib 2008']] <- fitSibModel(eq)

##2010
data$y <- data$give10
health.terms <- c("cog04", "hui04")
eq <- addTerms(eqn, health.terms)
###sibling model
rdat <- fitSibModel(eq, returnRestricted = TRUE)
appendix.results[['Grads Only 2010']] <- 
	glm(eq, data = subset(rdat, grad == 1), 
	family = binomial(link = 'probit'))
###pooled
appendix.results[['Pooled 2010']] <- 
	glm(eq, data = rdat, 
	family = binomial(link = 'probit'))
###sibling model
appendix.results[['Sib 2010']] <- fitSibModel(eq)

##2012
data$y <- data$give12
health.terms <- c("cog10", "hui10","walking_speed", "walk_speed_mis")
eq <- addTerms(eqn, health.terms)
###sibling model
rdat <- fitSibModel(eq, returnRestricted = TRUE)
appendix.results[['Grads Only 2012']] <- 
	glm(eq, data = subset(rdat, grad == 1), 
	family = binomial(link = 'probit'))
###pooled
appendix.results[['Pooled 2012']] <- 
	glm(eq, data = rdat, 
	family = binomial(link = 'probit'))
###sibling model
appendix.results[['Sib 2012']] <- fitSibModel(eq)

contr.appendix.restricted <- stargazer(appendix.results, type = 'text',
	omit = omit.ex, column.labels = names(appendix.results))

###############Rare Events Logit
library(Zelig)

re.summary <- array('', c(3, 6))
rownames(re.summary) <- c("Cognition","HUI","Walking Speed")
colnames(re.summary) <- c("2008 standard", "2008 RE",
	"2010 standard", "2010 RE", "2012 standard", "2012 RE")

######Cognition Tables behind the estimates in the summary table (1st row)
##2008
#pooled
re.summary['Cognition','2008 standard'] <- contr.summary['Cognition','2008 pooled']
data$y <- data$give08
health.terms <- "cog04"
eq <- addTerms(eqn, health.terms)
mod <- zelig(eq, data = data, model = "relogit")
re.summary['Cognition','2008 RE'] <- getSummaryTerm(mod, health.terms)

##2010
#pooled
re.summary['Cognition','2010 standard'] <- contr.summary['Cognition','2010 pooled']
data$y <- data$give10
health.terms <- "cog04"
eq <- addTerms(eqn, health.terms)
mod <- zelig(eq, data = data, model = "relogit")
re.summary['Cognition','2010 RE'] <- getSummaryTerm(mod, health.terms)

##2012
#pooled
re.summary['Cognition','2012 standard'] <- contr.summary['Cognition','2012 pooled']
data$y <- data$give12
health.terms <- "cog04"
eq <- addTerms(eqn, health.terms)
mod <- zelig(eq, data = data, model = "relogit")
re.summary['Cognition','2012 RE'] <- getSummaryTerm(mod, health.terms)

######HUI Tables behind the estimates in the summary table (2nd row)
##2008
re.summary['HUI','2008 standard'] <- contr.summary['HUI','2008 pooled']
data$y <- data$give08
health.terms <- "hui04"
eq <- addTerms(eqn, health.terms)
mod <- zelig(eq, data = data, model = "relogit")
re.summary['HUI','2008 RE'] <- getSummaryTerm(mod, health.terms)

##2010
#pooled
re.summary['HUI','2010 standard'] <- contr.summary['HUI','2010 pooled']
data$y <- data$give10
health.terms <- "hui04"
eq <- addTerms(eqn, health.terms)
mod <- zelig(eq, data = data, model = "relogit")
re.summary['HUI','2010 RE'] <- getSummaryTerm(mod, health.terms)

##2012
#pooled
re.summary['HUI','2012 standard'] <- contr.summary['HUI','2012 pooled']
data$y <- data$give12
health.terms <- "hui10"
eq <- addTerms(eqn, health.terms)
mod <- zelig(eq, data = data, model = "relogit")
re.summary['HUI','2012 RE'] <- getSummaryTerm(mod, health.terms)

######Walking Speed Tables behind the estimates in the summary table (3nd row)
##2012
#pooled
re.summary['Walking Speed','2012 standard'] <- contr.summary['Walking Speed','2012 pooled']
data$y <- data$give12
health.terms <- c("walking_speed", "walk_speed_mis")
eq <- addTerms(eqn, health.terms)
mod <- zelig(eq, data = data, model = "relogit")
re.summary['Walking Speed','2012 RE'] <- getSummaryTerm(mod, health.terms[1])


####################################rare events summary table
re.summary.table <- stargazer(re.summary, type='text')


baseline <- list()
######Cognition Tables behind the estimates in the summary table (1st row)
##2008
#pooled
data$y <- data$give08
summary(baseline[['cog']][['2008']] <- 
	glm(eqn,
	data = data, family = binomial(link = 'probit')))
#sibling
baseline[['cog']][['2008 sib']] <- fitSibModel(eqn)

##2010
#pooled
data$y <- data$give10
summary(baseline[['cog']][['2010']] <- 
	glm(eqn,
	data = data, family = binomial(link = 'probit')))
#sibling
baseline[['cog']][['2010 sib']] <- fitSibModel(eqn)

##2012
#pooled
data$y <- data$give12
summary(baseline[['cog']][['2012']] <- 
	glm(eqn,
	data = data, family = binomial(link = 'probit')))
#sibling
baseline[['cog']][['2012 sib']] <- fitSibModel(eqn)

baseline <- stargazer(baseline, type = 'text',
	omit = omit.ex, column.labels = names(appendix.results))


#################other models
library(MASS)
library(pscl)

eq <- addTerms(eqn, c("cog04","hui04"))
yr <- "08"
data$zAge <- as.numeric(data$zAge)
data$zAge2 <- as.numeric(data$zAge2)

data$y <- data[[paste("give", yr, sep="")]]
res <- list()
res[[1]] <- glm(eq, data=data, family = binomial(link = 'probit'))

data$y <- data[[paste("n.give", yr, sep="")]]
res[[1]] <- glm.nb(eq, data=data)

data$y <- ifelse(data[[paste("n.give", yr, sep="")]] > 30, 31, data$n.give08)
res[[2]] <- glm.nb(eq, data=data)

cat <- c(0, rep(1, 4), rep(2, 4), rep(3, 8), rep(4, 100))
data$y <- factor(cat[data$y+1])
mod <- lm(eq, data = data)
sub <- mod$model
res[[3]] <- polr(eq, data = sub, method = 'probit')

####amount
data$y <- ifelse(is.na(data$amount) | data$amount <= 0, NA,
	ifelse(data$amount > 10000, 10000, data$amount))
library(AER)

data$y <- ifelse(is.na(data$amount) | data$amount < 0, 0,
	ifelse(data$amount > 10000, 10000, data$amount))
res[[4]] <- lm(eq, data = data[which(data$y > 0),])
res[[5]] <- tobit(eq, left = 0, right = 10000, data = data)

data$y <- ifelse(data[[paste("n.give", yr, sep="")]] > 30, 31, data$n.give08)
cat <- c(0, rep(1, 4), rep(2, 4), rep(3, 8), rep(4, 100))
data$y <- factor(cat[data$y+1])
mod <- lm(eq, data = data)
sub <- mod$model
res[[3]] <- (polr(eq, data = sub, method = 'probit',
	Hess = TRUE))

other.models <- stargazer(res, type = 'text',
	omit = omit.ex, column.labels = names(appendix.results))



