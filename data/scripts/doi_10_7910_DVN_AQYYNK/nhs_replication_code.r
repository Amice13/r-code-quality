# load libraries

library(ggplot2)
library(stargazer)
library(ordinal)

# load up the data

setwd("Z:\\projects\\covid19\\nhs\\article\\replication\\share")

nhs <- read.csv("nhs_replication_data.csv", header=TRUE)

# create figure to visualise mean levels of trust

# unweighted version

trust_data <- data.frame(

	Categories = c("The government in Westminster", "The Prime Minister", "Parliament", "Your local Member of Parliament (MP)", "The NHS", "The police", "The courts", "News from traditional media", "News shared on social media"),
	Mean = c(mean(nhs$trustGov, na.rm=TRUE), mean(nhs$trustPM, na.rm=TRUE), mean(nhs$trustParl, na.rm=TRUE), mean(nhs$trustMP, na.rm=TRUE), mean(nhs$trustNHS, na.rm=TRUE), mean(nhs$trustPolice, na.rm=TRUE), mean(nhs$trustCourts, na.rm=TRUE), mean(nhs$trustTradNews, na.rm=TRUE), mean(nhs$trustSocNews, na.rm=TRUE)),
	SD = c(sd(nhs$trustGov, na.rm=TRUE), sd(nhs$trustPM, na.rm=TRUE), sd(nhs$trustParl, na.rm=TRUE), sd(nhs$trustMP, na.rm=TRUE), sd(nhs$trustNHS, na.rm=TRUE), sd(nhs$trustPolice, na.rm=TRUE), sd(nhs$trustCourts, na.rm=TRUE), sd(nhs$trustTradNews, na.rm=TRUE), sd(nhs$trustSocNews, na.rm=TRUE))
) 


# open file handle

png(filename = "trust_levels_unweighted.png", width = 1200, height = 1600)

ggplot(trust_data, aes(x = reorder(Categories, -Mean), y = Mean)) +
  geom_bar(stat = "identity", fill = "lightgrey") +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2) +
  geom_text(aes(label = paste0("Mean: ", round(Mean, 2), "\nSD: ", round(SD, 2))), vjust = 0.4, size = 8) + 
  coord_flip() +
  xlab('') +
  ylab('Trust Level') +
  ylim(0, 7) +
  ggtitle('Trust Levels in Different Institutions') +
  theme_minimal() +
  theme(
    text = element_text(size = 28), 
    axis.title.x = element_text(size = 32), 
    axis.title.y = element_text(size = 32), 
    axis.text = element_text(size = 28), 
    plot.title = element_text(size = 36, hjust = 0.5) 
  )

  
dev.off()



# weighted version

# needs a weighted SD function

weighted.sd <- function(x, w) {
  
  # remove NA values
  
  valid_data <- !is.na(x)
  x <- x[valid_data]
  w <- w[valid_data]
  
  mean_x <- sum(x * w) / sum(w)
  sqrt(sum(w * (x - mean_x)^2) / sum(w))
}

trust_data <- data.frame(

	Categories = c("The government in Westminster", "The Prime Minister", "Parliament", "Your local Member of Parliament (MP)", "The NHS", "The police", "The courts", "News from traditional media", "News shared on social media"),
	Mean = c(weighted.mean(nhs$trustGov, nhs$W8, na.rm=TRUE), weighted.mean(nhs$trustPM, nhs$W8, na.rm=TRUE), weighted.mean(nhs$trustParl, nhs$W8, na.rm=TRUE), weighted.mean(nhs$trustMP, nhs$W8, na.rm=TRUE), weighted.mean(nhs$trustNHS, nhs$W8, na.rm=TRUE), weighted.mean(nhs$trustPolice, nhs$W8, na.rm=TRUE), weighted.mean(nhs$trustCourts, nhs$W8, na.rm=TRUE), weighted.mean(nhs$trustTradNews, nhs$W8, na.rm=TRUE), weighted.mean(nhs$trustSocNews, nhs$W8, na.rm=TRUE)),
	SD = c(weighted.sd(nhs$trustGov, nhs$W8), weighted.sd(nhs$trustPM, nhs$W8), weighted.sd(nhs$trustParl, nhs$W8), weighted.sd(nhs$trustMP, nhs$W8), weighted.sd(nhs$trustNHS, nhs$W8), weighted.sd(nhs$trustPolice, nhs$W8), weighted.sd(nhs$trustCourts, nhs$W8), weighted.sd(nhs$trustTradNews, nhs$W8), weighted.sd(nhs$trustSocNews, nhs$W8))
) 

png(filename = "trust_levels_weighted.png", width = 1200, height = 1600)

ggplot(trust_data, aes(x = reorder(Categories, -Mean), y = Mean)) +
  geom_bar(stat = "identity", fill = "lightgrey") +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2) +
  geom_text(aes(label = paste0("Mean: ", round(Mean, 2), "\nSD: ", round(SD, 2))), vjust = 0.4, size = 8) + 
  coord_flip() +
  xlab('') +
  ylab('Trust Level') +
  ylim(0, 7) +
  ggtitle('Trust Levels in Different Institutions') +
  theme_minimal() +
  theme(
    text = element_text(size = 28), 
    axis.title.x = element_text(size = 32), 
    axis.title.y = element_text(size = 32), 
    axis.text = element_text(size = 28), 
    plot.title = element_text(size = 36, hjust = 0.5) 
  )

dev.off()

# create tables 

# respondent level descriptives

varNames <- c(	
	"Trust in the NHS", rep("", 6), 
	"Region", rep("", 8), 
	"Gender", "", 
	"Ethnic minority", "", 
	"Income", rep("", 13), 
	"Higher education", "", 
	"Voted Conservative 2019", "", 
	rep("", 2)
	) 
	
categories <- c(
	"1 (Not at all)", "2", "3", "4", "5", "6", "7 (Completely)", 
	"North East", "North West", "Yorkshire and the Humber", "East Midlands", "West Midlands", "East of England", "London", "South East", "South West", 
	"Female", "Male", 
	"Yes", "No", 
	"Below £5000", "£5000 - £9999", "£10,000 - £14,999", "£15,000 - £19,999", "£20,000 - £24,999", "£25,000 - £29,999", "£30,000 - £34,999", "£35,000 - £39,999", "£40,000 - £44,999", "£45,000 - £49,999", "£50,000 - £59,999", "£60,000 - £69,999", "£70,000 - £99,999", "£100,000 +",
	"Yes", "No", 
	"Yes", "No", 			
	rep("", 2)
	)
	
vars <- c(	
	(round(length(nhs$id[nhs$trustNHS == 1 & !is.na(nhs$trustNHS)]) / length(nhs$id[!is.na(nhs$trustNHS)])*100 , 2)), 
	(round(length(nhs$id[nhs$trustNHS == 2 & !is.na(nhs$trustNHS)]) / length(nhs$id[!is.na(nhs$trustNHS)])*100 , 2)), 
	(round(length(nhs$id[nhs$trustNHS == 3 & !is.na(nhs$trustNHS)]) / length(nhs$id[!is.na(nhs$trustNHS)])*100 , 2)), 
	(round(length(nhs$id[nhs$trustNHS == 4 & !is.na(nhs$trustNHS)]) / length(nhs$id[!is.na(nhs$trustNHS)])*100 , 2)), 
	(round(length(nhs$id[nhs$trustNHS == 5 & !is.na(nhs$trustNHS)]) / length(nhs$id[!is.na(nhs$trustNHS)])*100 , 2)), 
	(round(length(nhs$id[nhs$trustNHS == 6 & !is.na(nhs$trustNHS)]) / length(nhs$id[!is.na(nhs$trustNHS)])*100 , 2)), 
	(round(length(nhs$id[nhs$trustNHS == 7 & !is.na(nhs$trustNHS)]) / length(nhs$id[!is.na(nhs$trustNHS)])*100 , 2)), 
	(round(length(nhs$id[nhs$region == 1 & !is.na(nhs$region)]) / length(nhs$id[!is.na(nhs$region)])*100 , 2)),
	(round(length(nhs$id[nhs$region == 2 & !is.na(nhs$region)]) / length(nhs$id[!is.na(nhs$region)])*100 , 2)),
	(round(length(nhs$id[nhs$region == 3 & !is.na(nhs$region)]) / length(nhs$id[!is.na(nhs$region)])*100 , 2)),
	(round(length(nhs$id[nhs$region == 4 & !is.na(nhs$region)]) / length(nhs$id[!is.na(nhs$region)])*100 , 2)),
	(round(length(nhs$id[nhs$region == 5 & !is.na(nhs$region)]) / length(nhs$id[!is.na(nhs$region)])*100 , 2)),
	(round(length(nhs$id[nhs$region == 6 & !is.na(nhs$region)]) / length(nhs$id[!is.na(nhs$region)])*100 , 2)),
	(round(length(nhs$id[nhs$region == 7 & !is.na(nhs$region)]) / length(nhs$id[!is.na(nhs$region)])*100 , 2)),
	(round(length(nhs$id[nhs$region == 8 & !is.na(nhs$region)]) / length(nhs$id[!is.na(nhs$region)])*100 , 2)),
	(round(length(nhs$id[nhs$region == 9 & !is.na(nhs$region)]) / length(nhs$id[!is.na(nhs$region)])*100 , 2)), 
	(round(length(nhs$id[nhs$women == 1 & !is.na(nhs$women)]) / length(nhs$id[!is.na(nhs$women)])*100 , 2)), 
	(round(length(nhs$id[nhs$women == 0 & !is.na(nhs$women)]) / length(nhs$id[!is.na(nhs$women)])*100 , 2)), 
	(round(length(nhs$id[nhs$ethnicity == 1 & !is.na(nhs$ethnicity)]) / length(nhs$id[!is.na(nhs$ethnicity)])*100 , 2)), 
	(round(length(nhs$id[nhs$ethnicity == 0 & !is.na(nhs$ethnicity)]) / length(nhs$id[!is.na(nhs$ethnicity)])*100 , 2)), 
	(round(length(nhs$id[nhs$income == 1 & !is.na(nhs$income)]) / length(nhs$id[!is.na(nhs$income)])*100 , 2)),
	(round(length(nhs$id[nhs$income == 2 & !is.na(nhs$income)]) / length(nhs$id[!is.na(nhs$income)])*100 , 2)),
	(round(length(nhs$id[nhs$income == 3 & !is.na(nhs$income)]) / length(nhs$id[!is.na(nhs$income)])*100 , 2)),
	(round(length(nhs$id[nhs$income == 4 & !is.na(nhs$income)]) / length(nhs$id[!is.na(nhs$income)])*100 , 2)),
	(round(length(nhs$id[nhs$income == 5 & !is.na(nhs$income)]) / length(nhs$id[!is.na(nhs$income)])*100 , 2)),
	(round(length(nhs$id[nhs$income == 6 & !is.na(nhs$income)]) / length(nhs$id[!is.na(nhs$income)])*100 , 2)),
	(round(length(nhs$id[nhs$income == 7 & !is.na(nhs$income)]) / length(nhs$id[!is.na(nhs$income)])*100 , 2)),
	(round(length(nhs$id[nhs$income == 8 & !is.na(nhs$income)]) / length(nhs$id[!is.na(nhs$income)])*100 , 2)),
	(round(length(nhs$id[nhs$income == 9 & !is.na(nhs$income)]) / length(nhs$id[!is.na(nhs$income)])*100 , 2)),
	(round(length(nhs$id[nhs$income == 10 & !is.na(nhs$income)]) / length(nhs$id[!is.na(nhs$income)])*100 , 2)),
	(round(length(nhs$id[nhs$income == 11 & !is.na(nhs$income)]) / length(nhs$id[!is.na(nhs$income)])*100 , 2)),
	(round(length(nhs$id[nhs$income == 12 & !is.na(nhs$income)]) / length(nhs$id[!is.na(nhs$income)])*100 , 2)),
	(round(length(nhs$id[nhs$income == 13 & !is.na(nhs$income)]) / length(nhs$id[!is.na(nhs$income)])*100 , 2)),
	(round(length(nhs$id[nhs$income == 14 & !is.na(nhs$income)]) / length(nhs$id[!is.na(nhs$income)])*100 , 2)),
	(round(length(nhs$id[nhs$higherEd == 1 & !is.na(nhs$higherEd)]) / length(nhs$id[!is.na(nhs$higherEd)])*100 , 2)), 
	(round(length(nhs$id[nhs$higherEd == 0 & !is.na(nhs$higherEd)]) / length(nhs$id[!is.na(nhs$higherEd)])*100 , 2)), 
	(round(length(nhs$id[nhs$conVote19 == 1 & !is.na(nhs$conVote19)]) / length(nhs$id[!is.na(nhs$conVote19)])*100 , 2)), 
	(round(length(nhs$id[nhs$conVote19 == 0 & !is.na(nhs$conVote19)]) / length(nhs$id[!is.na(nhs$conVote19)])*100 , 2)),

	rep("", 2)			
		)
	
respondentDescriptives <- data.frame(varNames[1:20], categories[1:20], vars[1:20], varNames[21:40], categories[21:40], vars[21:40])

colnames(respondentDescriptives) <- c("Factor", "Category", "%", "Factor", "Category", "%")

# display the table

respondentDescriptives

# regional level descriptives

categories <- c("Factor", "Mean", "SD", "Min", "Max") 

cancerDelay <- c("Cancer 2 week wait time breached (%)", round(mean(nhs$cancerDelay, na.rm=TRUE)*100, 2), round(sd(nhs$cancerDelay, na.rm=TRUE)*100, 2), round(min(nhs$cancerDelay, na.rm=TRUE)*100, 2), round(max(nhs$cancerDelay, na.rm=TRUE)*100, 2))
emergencyDelay <- c("Emergency more than 4 hour wait (%)", round(mean(nhs$emergencyDelay, na.rm=TRUE)*100, 2), round(sd(nhs$emergencyDelay, na.rm=TRUE)*100, 2), round(min(nhs$emergencyDelay, na.rm=TRUE)*100, 2), round(max(nhs$emergencyDelay, na.rm=TRUE)*100, 2))
unemp <- c("Unemployment rate (%)", round(mean(nhs$unemploymentRate, na.rm=TRUE), 2), round(sd(nhs$unemploymentRate, na.rm=TRUE), 2), round(min(nhs$unemploymentRate, na.rm=TRUE), 2), round(max(nhs$unemploymentRate, na.rm=TRUE), 2))
conShare <- c("Conservative share in constituency (%)", round(mean(nhs$conShare, na.rm=TRUE)*100, 2), round(sd(nhs$conShare, na.rm=TRUE)*100, 2), round(min(nhs$conShare, na.rm=TRUE)*100, 2), round(max(nhs$conShare, na.rm=TRUE)*100, 2))


regionalDescriptives <- data.frame(categories, cancerDelay, emergencyDelay, unemp, conShare)

# display the table

regionalDescriptives

# spin the table around

t((regionalDescriptives))

# run time variant models 

# base model including income

weightedOLS <- lm(trustNHS ~ cancerDelay + emergencyDelay + women + income + age + ethnicity + higherEd + conVote19 + unemploymentRate + conShare + factor(region), data = nhs, weights=W8)
	
# remove income 

weightedOLSnoIncome <- lm(trustNHS ~ cancerDelay + emergencyDelay + women + age + ethnicity + higherEd + conVote19 + unemploymentRate + conShare + factor(region), data = nhs, weights=W8)


stargazer(weightedOLS, weightedOLSnoIncome, covariate.labels = c("Cancer referral delay new", "Hospital A&E delay", "Woman", "Income", "Age", "Member of ethnic minority", "Higher education", "Voted Conservative in 2019", "Constituency unemployment rate", "Conservative vote share 2019", "North West", "Yorkshire and the Humber", "East Midlands", "West Midlands", "East of England", "London", "South East", "South West", "(Intercept)"), type="text")

# weighted ordered for the appendix

# base model including income
	
weightedOrdered <- clm(as.factor(trustNHS) ~  cancerDelay + emergencyDelay + women + income + age + ethnicity + higherEd + conVote19 + unemploymentRate + conShare + factor(region), data = nhs, weights=W8)

# remove income 

weightedOrderedNoIncome <- clm(as.factor(trustNHS) ~  cancerDelay + emergencyDelay + women + age + ethnicity + higherEd + conVote19 + unemploymentRate + conShare + factor(region), data = nhs, weights=W8)

stargazer(weightedOrdered, weightedOrderedNoIncome, covariate.labels = c("Cancer referral delay new", "Hospital A&E delay", "Woman", "Income", "Age", "Member of ethnic minority", "Higher education", "Voted Conservative in 2019", "Constituency unemployment rate", "Conservative vote share 2019", "North West", "Yorkshire and the Humber", "East Midlands", "West Midlands", "East of England", "London", "South East", "South West", "(Intercept)"), type="text")


