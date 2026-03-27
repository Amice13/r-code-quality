voting.results <- list(cog = list(), hui = list(),
	walk = list())
voting.summary <- array('', c(3, 6))
rownames(voting.summary) <- c("Cognition","HUI","Walking Speed")
colnames(voting.summary) <- c("2008 pooled", "2008 sibling",
	"2010 pooled", "2010 sibling", "2012 pooled", "2012 sibling")
######Cognition Tables behind the estimates in the summary table (1st row)
##2008
#pooled
data$y <- data$cat08
health.terms <- "cog04"
eq <- addTerms(eqn, health.terms)
summary(voting.results[['cog']][['2008']] <- 
	glm(eq,
	data = data, family = binomial(link = 'probit')))
voting.summary['Cognition','2008 pooled'] <- getSummaryTerm(
	voting.results[['cog']][['2008']], health.terms)
#sibling
voting.results[['cog']][['2008 sib']] <- fitSibModel(eq)
voting.summary['Cognition','2008 sibling'] <- getSummaryTerm(
	voting.results[['cog']][['2008 sib']], health.terms)

##2010
#pooled
data$y <- data$cat10
health.terms <- "cog04"
eq <- addTerms(eqn, health.terms)
summary(voting.results[['cog']][['2010']] <- 
	glm(eq,
	data = data, family = binomial(link = 'probit')))
voting.summary['Cognition','2010 pooled'] <- getSummaryTerm(
	voting.results[['cog']][['2010']], health.terms)
#sibling
voting.results[['cog']][['2010 sib']] <- fitSibModel(eq)
voting.summary['Cognition','2010 sibling'] <- getSummaryTerm(
	voting.results[['cog']][['2010 sib']], health.terms)

##2012
#pooled
data$y <- data$cat12
health.terms <- "cog10"
eq <- addTerms(eqn, health.terms)
summary(voting.results[['cog']][['2012']] <- 
	glm(eq,
	data = data, family = binomial(link = 'probit')))
voting.summary['Cognition','2012 pooled'] <- getSummaryTerm(
	voting.results[['cog']][['2012']], health.terms)
#sibling
voting.results[['cog']][['2012 sib']] <- fitSibModel(eq)
voting.summary['Cognition','2012 sibling'] <- getSummaryTerm(
	voting.results[['cog']][['2012 sib']], health.terms)

######HUI Tables behind the estimates in the summary table (2nd row)
##2008
data$y <- data$cat08
health.terms <- "hui04"
eq <- addTerms(eqn, health.terms)
summary(voting.results[['hui']][['2008']] <- 
	glm(eq,
	data = data, family = binomial(link = 'probit')))
voting.summary['HUI','2008 pooled'] <- getSummaryTerm(
	voting.results[['hui']][['2008']], health.terms)
#sibling
voting.results[['hui']][['2008 sib']] <- fitSibModel(eq)
voting.summary['HUI','2008 sibling'] <- getSummaryTerm(
	voting.results[['hui']][['2008 sib']], health.terms)

##2010
#pooled
data$y <- data$cat10
health.terms <- "hui04"
eq <- addTerms(eqn, health.terms)
summary(voting.results[['hui']][['2010']] <- 
	glm(eq,
	data = data, family = binomial(link = 'probit')))
voting.summary['HUI','2010 pooled'] <- getSummaryTerm(
	voting.results[['hui']][['2010']], health.terms)
#sibling
voting.results[['hui']][['2010 sib']] <- fitSibModel(eq)
voting.summary['HUI','2010 sibling'] <- getSummaryTerm(
	voting.results[['hui']][['2010 sib']], health.terms)

##2012
#pooled
data$y <- data$cat12
health.terms <- "hui10"
eq <- addTerms(eqn, health.terms)
summary(voting.results[['hui']][['2012']] <- 
	glm(eq,
	data = data, family = binomial(link = 'probit')))
voting.summary['HUI','2012 pooled'] <- getSummaryTerm(
	voting.results[['hui']][['2012']], health.terms)
#sibling
voting.results[['hui']][['2012 sib']] <- fitSibModel(eq)
voting.summary['HUI','2012 sibling'] <- getSummaryTerm(
	voting.results[['hui']][['2012 sib']], health.terms)


######Walking Speed Tables behind the estimates in the summary table (3nd row)
##2012
#pooled
data$y <- data$cat12
health.terms <- c("walking_speed", "walk_speed_mis")
eq <- addTerms(eqn, health.terms)
summary(voting.results[['walk']][['2012']] <- 
	glm(eq,
	data = data, family = binomial(link = 'probit')))
voting.summary['Walking Speed','2012 pooled'] <- getSummaryTerm(
	voting.results[['walk']][['2012']], health.terms[1])
#sibling
voting.results[['walk']][['2012 sib']] <- fitSibModel(eq)
voting.summary['Walking Speed','2012 sibling'] <- getSummaryTerm(
	voting.results[['walk']][['2012 sib']], health.terms[1])


####################################voting summary table
voting.summary.table <- stargazer(voting.summary, type='text')

########Tables for appendix
omit.ex <- "^(fl|mn|ca|il|az|tx|colo|other_state)"
appendix.results <- list()

#######regression results from summary table
##cog
cog.summary <- stargazer(voting.results[[1]], type = 'text',
	omit = omit.ex, column.labels = names(voting.results[[1]]))
##hui
hui.summary <- stargazer(voting.results[[2]], type = 'text',
	omit = omit.ex, column.labels = names(voting.results[[2]]))
##walking
walk.summary <- stargazer(voting.results[[3]], type = 'text',
	omit = omit.ex, column.labels = names(voting.results[[3]]))

##########Voting appendix tables
##2008
data$y <- data$cat08
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
data$y <- data$cat10
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
data$y <- data$cat12
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

voting.appendix.full <- stargazer(appendix.results, type = 'text',
	omit = omit.ex, column.labels = names(appendix.results))

#############restricted sample estimation.....
##2008
data$y <- data$cat08
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
data$y <- data$cat10
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
data$y <- data$cat12
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

voting.appendix.restricted <- stargazer(appendix.results, type = 'text',
	omit = omit.ex, column.labels = names(appendix.results))


baseline <- list()
######Cognition Tables behind the estimates in the summary table (1st row)
##2008
#pooled
data$y <- data$cat08
summary(baseline[['cog']][['2008']] <- 
	glm(eqn,
	data = data, family = binomial(link = 'probit')))
#sibling
baseline[['cog']][['2008 sib']] <- fitSibModel(eqn)

##2010
#pooled
data$y <- data$cat10
summary(baseline[['cog']][['2010']] <- 
	glm(eqn,
	data = data, family = binomial(link = 'probit')))
#sibling
baseline[['cog']][['2010 sib']] <- fitSibModel(eqn)

##2012
#pooled
data$y <- data$cat12
summary(baseline[['cog']][['2012']] <- 
	glm(eqn,
	data = data, family = binomial(link = 'probit')))
#sibling
baseline[['cog']][['2012 sib']] <- fitSibModel(eqn)

baseline <- stargazer(baseline, type = 'text',
	omit = omit.ex, column.labels = names(appendix.results))


