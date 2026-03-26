grp.results <- list(cog = list(), hui = list(),
	walk = list())
grp.summary <- array('', c(3, 4))
rownames(grp.summary) <- c("Cognition","HUI","Walking Speed")
colnames(grp.summary) <- c("2004 pooled", "2004 sibling",
	"2011 pooled", "2011 sibling")
######Cognition Tables behind the estimates in the summary table (1st row)
##2011
#pooled
data$y <- data$vote08
health.terms <- "cog10"
eq <- addTerms(eqn, health.terms)
summary(grp.results[['cog']][['2011']] <- 
	glm(eq,
	data = data, family = binomial(link = 'probit')))
grp.summary['Cognition','2011 pooled'] <- getSummaryTerm(
	grp.results[['cog']][['2011']], health.terms)
#sibling
grp.results[['cog']][['2011 sib']] <- fitSibModel(eq)
grp.summary['Cognition','2011 sibling'] <- getSummaryTerm(
	grp.results[['cog']][['2011 sib']], health.terms)

##2011
#pooled
data$y <- data$vote08
health.terms <- "hui10"
eq <- addTerms(eqn, health.terms)
summary(grp.results[['hui']][['2011']] <- 
	glm(eq,
	data = data, family = binomial(link = 'probit')))
grp.summary['HUI','2011 pooled'] <- getSummaryTerm(
	grp.results[['hui']][['2011']], health.terms)
#sibling
grp.results[['hui']][['2011 sib']] <- fitSibModel(eq)
grp.summary['HUI','2011 sibling'] <- getSummaryTerm(
	grp.results[['hui']][['2011 sib']], health.terms)


######Walking Speed Tables behind the estimates in the summary table (3nd row)
##2012
#pooled
health.terms <- c("walking_speed", "walk_speed_mis")
eq <- addTerms(eqn, health.terms)
summary(grp.results[['walk']][['2011']] <- 
	glm(eq,
	data = data, family = binomial(link = 'probit')))
grp.summary['Walking Speed','2011 pooled'] <- getSummaryTerm(
	grp.results[['walk']][['2011']], health.terms[1])
#sibling
grp.results[['walk']][['2011 sib']] <- fitSibModel(eq)
grp.summary['Walking Speed','2011 sibling'] <- getSummaryTerm(
	grp.results[['walk']][['2011 sib']], health.terms[1])


####################################voting summary table
polgrp.summary.table <- stargazer(grp.summary, type='text')

########Tables for appendix
omit.ex <- "^(fl|mn|ca|il|az|tx|colo|other_state|Constant)"
appendix.results <- list()

##########Voting appendix tables
health.terms <- c("cog10", "hui10", "walking_speed", "walk_speed_mis")
eq <- addTerms(eqn, health.terms)
###grads only
appendix.results[['Grads Only 2011']] <- 
	glm(eq, data = subset(data, grad == 1), 
	family = binomial(link = 'probit'))
###pooled
appendix.results[['Pooled 2011']] <- 
	glm(eq, data = data, 
	family = binomial(link = 'probit'))
###sibling model
appendix.results[['Sib 2011']] <- fitSibModel(eq)

polgrp.appendix.full <- stargazer(appendix.results, type = 'text',
	omit = omit.ex, column.labels = names(appendix.results))

#############restricted sample estimation.....
health.terms <- c("cog10", "hui10","walking_speed", "walk_speed_mis")
eq <- addTerms(eqn, health.terms)
###sibling model
rdat <- fitSibModel(eq, returnRestricted = TRUE)
appendix.results[['Grads Only 2011']] <- 
	glm(eq, data = subset(rdat, grad == 1), 
	family = binomial(link = 'probit'))
###pooled
appendix.results[['Pooled 2011']] <- 
	glm(eq, data = rdat, 
	family = binomial(link = 'probit'))
###sibling model
appendix.results[['Sib 2011']] <- fitSibModel(eq)

polgrp.appendix.restricted <- stargazer(appendix.results, type = 'text',
	omit = omit.ex, column.labels = names(appendix.results))

