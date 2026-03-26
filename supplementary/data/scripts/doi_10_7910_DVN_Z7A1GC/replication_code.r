library(texreg) # for model table output
library(knitr)  # for descriptive table output

# set working directory 

setwd("Z:\\projects\\covid19\\share\\working_papers\\trust_in_doctors_and_scientists")

# load up the data

uk <- read.csv("replication_data.csv", header=TRUE) 

# calculate the means for each period

# create vectors for waves and 
# means trust of doctors and scientists

firstWave <- 6
lastWave <- 24

waves <- c(firstWave:lastWave)

docMeans <- c(firstWave:lastWave)
sciMeans <- c(firstWave:lastWave)

trust <- data.frame(waves, docMeans, sciMeans, generalMeans, PMMeans, GovMeans)

for (i in firstWave:lastWave) { 

	trust$docMeans[trust$wave==i] <- mean(uk$trustDocs[uk$wave==i], na.rm=TRUE) 
	trust$sciMeans[trust$wave==i] <- mean(uk$trustScien[uk$wave==i], na.rm=TRUE) 
	trust$generalMeans[trust$wave==i] <- mean(uk$trustGeneral[uk$wave==i], na.rm=TRUE)
	trust$PMMeans[trust$wave==i] <- mean(uk$trustPM[uk$wave==i], na.rm=TRUE)
	trust$GovMeans[trust$wave==i] <- mean(uk$trustGov[uk$wave==i], na.rm=TRUE)

}

dates <- c("Dec 2022", "Jan 2023", "Feb 2023", "Mar 2023", "Apr 2023", "May 2023", "Jun 2023", "Jul 2023", "Aug 2023", "Sep 2023", "Oct 2023", "Nov 2023", "Dec 2023", "Jan 2023", "Feb 2023", "Mar 2023", "Apr 2023", "May 2023", "Jun 2023")

plot(trust$waves, trust$docMeans, ylim = c(1, 7), pch=16, col="red", xaxt="n", xlab="", ylab="Mean values")
axis(1, at=waves, labels=dates, las=2)
lines(trust$waves, trust$docMeans, col="red")
points(trust$waves, trust$sciMeans, pch=16, col="blue")
lines(trust$waves, trust$sciMeans, col="blue")
points(trust$waves, trust$generalMeans, pch=16, col="black")
lines(trust$waves, trust$generalMeans, col="black")
points(trust$waves, trust$PMMeans, pch=16, col="green")
lines(trust$waves, trust$PMMeans, col="green")
points(trust$waves, trust$GovMeans, pch=16, col="purple")
lines(trust$waves, trust$GovMeans, col="purple")

legend("topright", legend = c("Trust in doctors", "Trust in scientists", "General trust", "Trust in PM", "Trust in government"),
       col = c("red", "blue", "black", "green", "purple"), pch = 16, lty = 1)
	   
# descriptives 

# calculate the values 

age_uk_mean <- round(mean(uk$age), 2)
age_uk_min <- min(uk$age)
age_uk_max <- max(uk$age)
age_uk_sd <- round(sd(uk$age), 2)

sex_uk_men <- round(length(uk$id[uk$women == 0]) / length(uk$id) * 100, 2)
sex_uk_women <- round(length(uk$id[uk$women == 1]) / length(uk$id) * 100, 2)

ethnic_count <- length(uk$id[uk$ethnic==1 & !is.na(uk$ethnic)])
ethnic_pc <- round(length(uk$id[uk$ethnic==1 & !is.na(uk$ethnic)]) / length(uk$id[!is.na(uk$ethnic)]) * 100, 2) 

edu_uk_less_univ <- round(length(uk$id[uk$university == 0 & !is.na(uk$university)]) / length(uk$id[!is.na(uk$university)]) * 100, 2)
edu_uk_univ <- round(length(uk$id[uk$university == 1 & !is.na(uk$university)]) / length(uk$id[!is.na(uk$university)]) * 100, 2)

left_uk_left <- round(length(uk$leftRight[uk$leftRight < 5 & !is.na(uk$leftRight)]) / length(uk$leftRight[!is.na(uk$leftRight)]) * 100, 2)
left_uk_middle <- round(length(uk$leftRight[uk$leftRight == 5 & !is.na(uk$leftRight)]) / length(uk$leftRight[!is.na(uk$leftRight)]) * 100, 2)
left_uk_right <- round(length(uk$leftRight[uk$leftRight > 5 & !is.na(uk$leftRight)]) / length(uk$leftRight[!is.na(uk$leftRight)]) * 100, 2)

trust_uk_general_mean <- round(mean(uk$trustGeneral, na.rm = TRUE), 2)
trust_uk_general_min <- min(uk$trustGeneral, na.rm = TRUE)
trust_uk_general_max <- max(uk$trustGeneral, na.rm = TRUE)
trust_uk_general_sd <- round(sd(uk$trustGeneral, na.rm = TRUE), 2)

trust_uk_gov_mean <- round(mean(uk$trustGov, na.rm = TRUE), 2)
trust_uk_gov_min <- min(uk$trustGov, na.rm = TRUE)
trust_uk_gov_max <- max(uk$trustGov, na.rm = TRUE)
trust_uk_gov_sd <- round(sd(uk$trustGov, na.rm = TRUE), 2)

trust_uk_pm_mean <- round(mean(uk$trustPM, na.rm = TRUE), 2)
trust_uk_pm_min <- min(uk$trustPM, na.rm = TRUE)
trust_uk_pm_max <- max(uk$trustPM, na.rm = TRUE)
trust_uk_pm_sd <- round(sd(uk$trustPM, na.rm = TRUE), 2)

n_uk <- length(uk$id)

# stitch into table

desc_table <- data.frame(
  Variable = c("Age (Mean)", "Age (Min)", "Age (Max)", "Age (SD)",
               "Men (%)", "Women (%)",
			   "Ethnic minority (count)", "Ethnic minority (%)", 
               "Less than University (%)", "University (%)",
               "Left (%)", "Center (%)", "Right (%)",
               "Trust General (Mean)", "Trust General (Min)", "Trust General (Max)", "Trust General (SD)",
               "Trust Gov (Mean)", "Trust Gov (Min)", "Trust Gov (Max)", "Trust Gov (SD)",
               "Trust PM (Mean)", "Trust PM (Min)", "Trust PM (Max)", "Trust PM (SD)",
               "Sample Size (N)"),
  Value = c(age_uk_mean, age_uk_min, age_uk_max, age_uk_sd,
            sex_uk_men, sex_uk_women,
			ethnic_count, ethnic_pc, 
            edu_uk_less_univ, edu_uk_univ,
            left_uk_left, left_uk_middle, left_uk_right,
            trust_uk_general_mean, trust_uk_general_min, trust_uk_general_max, trust_uk_general_sd,
            trust_uk_gov_mean, trust_uk_gov_min, trust_uk_gov_max, trust_uk_gov_sd,
            trust_uk_pm_mean, trust_uk_pm_min, trust_uk_pm_max, trust_uk_pm_sd,
            n_uk)
)

# display the table

kable(desc_table, format = "html", caption = "Descriptive Statistics for UK Sample")

html_file <- "desc_table.html"
kable(desc_table, format = "html", caption = "Descriptive Statistics for UK Sample") %>%
  cat(file = html_file)

browseURL(html_file)

# descriptive religion

# notes on religion 

# religion: 
# 1	No, I do not regard myself as belonging to any particular religion.
# 2	Yes - Church of England/Anglican/Episcopal
# 3	Yes - Roman Catholic
# 4	Yes - Presbyterian/Church of Scotland
# 5	Yes - Methodist
# 6	Yes - Baptist
# 7	Yes - United Reformed Church
# 8	Yes - Free Presbyterian
# 9	Yes - Brethren
# 10	Yes - Judaism
# 11	Yes - Hinduism
# 12	Yes - Islam
# 13	Yes - Sikhism
# 14	Yes - Buddhism
# 15	Yes - Other
# 16	Prefer not to say
# 17	Yes – Orthodox Christian
# 18	Yes - Pentecostal (e.g. Assemblies of God, Elim Pentecostal Church, New Testament Church of God, Redeemed Christian Chur
# 19	Yes - Evangelical – independent/non-denominational (e.g. FIEC, Pioneer, Vineyard, Newfrontiers)
# 98a	Skipped
# 99a	Not Asked

# religion2: 
# 1	no rel
# 2	CofE
# 3 RC
# 4 Other Christ
# 5 Method
# 6 Bapt
# 7 Juda
# 8 Hindu
# 9 Islam
# 10 Other
# 11 Orthodox
# 12 Pentecost
# 13 Evange
# 14 Prefer not to say 
# 15 Skipped question

# religion3
# as religion2, but with 1 set to NA

# religion2 therefore uses "no religion" as the baseline
# religion3 uses "CofE" as the baseline

noRelCount <- length(uk$id[uk$religion2==1])
noRelPC <- round(length(uk$id[uk$religion2==1]) / length(uk$id) * 100, 2)
CofECount <- length(uk$id[uk$religion2==2])
CofEPC <- round(length(uk$id[uk$religion2==2]) / length(uk$id) * 100, 2)
RCCount <- length(uk$id[uk$religion2==3])
RCPC <- round(length(uk$id[uk$religion2==3]) / length(uk$id) * 100, 2)
OCCount <- length(uk$id[uk$religion2==4])
OCPC <- round(length(uk$id[uk$religion2==4]) / length(uk$id) * 100, 2)
methodCount <- length(uk$id[uk$religion2==5])
methodPC <- round(length(uk$id[uk$religion2==5]) / length(uk$id) * 100, 2)
baptCount <- length(uk$id[uk$religion2==6])
baptPC <- round(length(uk$id[uk$religion2==6]) / length(uk$id) * 100, 2)
judaCount <- length(uk$id[uk$religion2==7])
judaPC <- round(length(uk$id[uk$religion2==7]) / length(uk$id) * 100, 2)
hinduCount <- length(uk$id[uk$religion2==8])
hinduPC <- round(length(uk$id[uk$religion2==8]) / length(uk$id) * 100, 2)
islamCount <- length(uk$id[uk$religion2==9])
islamPC <- round(length(uk$id[uk$religion2==9]) / length(uk$id) * 100, 2)
otherCount <- length(uk$id[uk$religion2==10])
otherPC <- round(length(uk$id[uk$religion2==10]) / length(uk$id) * 100, 2)
orthCount <- length(uk$id[uk$religion2==11])
orthPC <- round(length(uk$id[uk$religion2==11]) / length(uk$id) * 100, 2)
pentCount <- length(uk$id[uk$religion2==12])
pentPC <- round(length(uk$id[uk$religion2==12]) / length(uk$id) * 100, 2)
evanCount <- length(uk$id[uk$religion2==13])
evanPC <- round(length(uk$id[uk$religion2==13]) / length(uk$id) * 100, 2)
preferCount <- length(uk$id[uk$religion2==14])
preferPC <- round(length(uk$id[uk$religion2==14]) / length(uk$id) * 100, 2)
skipCount <- length(uk$id[uk$religion2==15])
skipPC <- round(length(uk$id[uk$religion2==15]) / length(uk$id) * 100, 2)

counts <- c(noRelCount, CofECount, RCCount, OCCount, methodCount, baptCount, judaCount, hinduCount, islamCount, otherCount, orthCount, pentCount, evanCount, preferCount, skipCount)
percents <- c(noRelPC, CofEPC, RCPC, OCPC, methodPC, baptPC, judaPC, hinduPC, islamPC, otherPC, orthPC, pentPC, evanPC, preferPC, skipPC)
names <- c("No religion", "Church of England", "Roman Catholic", "Other Christian", "Methodist", "Baptist", "Judaism", "Hindu", "Islam", "Other", "Orthodox", "Pentecostal", "Evangelical", "Prefer not to say", "Skipped question")

rf <- data.frame(names, counts, percents)

rf

# models 

m1 <- lm(trustDocs ~ age + women + ethnic + university + leftRight + trustGeneral + trustGov + trustPM, data = uk)
m2 <- lm(trustScien ~ age + women + ethnic + university + leftRight + trustGeneral + trustGov + trustPM, data = uk)
m3 <- lm(trustDocs ~ age + women + ethnic + university + leftRight + trustGeneral + trustGov + trustPM + factor(religion2), data = uk)
m4 <- lm(trustScien ~ age + women + ethnic + university + leftRight + trustGeneral + trustGov + trustPM + factor(religion2), data = uk)
m5 <- lm(trustDocs ~ age + women + ethnic + university + leftRight + trustGeneral + trustGov + trustPM + factor(religion3), data = uk)
m6 <- lm(trustScien ~ age + women + ethnic + university + leftRight + trustGeneral + trustGov + trustPM + factor(religion3), data = uk)


names(m1$coefficients) <- c("Intercept", "Age", "Women", "Ethnic", "University", "Left-right", "General trust", "Trust government", "Trust PM")
names(m2$coefficients) <- c("Intercept", "Age", "Women", "Ethnic", "University", "Left-right", "General trust", "Trust government", "Trust PM")
names(m3$coefficients) <- c("Intercept", "Age", "Women", "Ethnic", "University", "Left-right", "General trust", "Trust government", "Trust PM", "Church of England", "Roman Catholic", "Other Christian", "Methodist", "Baptist", "Judaism", "Hindu", "Islam", "Other", "Orthodox", "Pentecostal", "Evangelical", "Prefer not to say", "Skipped question")
names(m4$coefficients) <- c("Intercept", "Age", "Women", "Ethnic", "University", "Left-right", "General trust", "Trust government", "Trust PM", "Church of England", "Roman Catholic", "Other Christian", "Methodist", "Baptist", "Judaism", "Hindu", "Islam", "Other", "Orthodox", "Pentecostal", "Evangelical", "Prefer not to say", "Skipped question")
names(m5$coefficients) <- c("Intercept", "Age", "Women", "Ethnic", "University", "Left-right", "General trust", "Trust government", "Trust PM", "Roman Catholic", "Other Christian", "Methodist", "Baptist", "Judaism", "Hindu", "Islam", "Other", "Orthodox", "Pentecostal", "Evangelical", "Prefer not to say", "Skipped question")
names(m6$coefficients) <- c("Intercept", "Age", "Women", "Ethnic", "University", "Left-right", "General trust", "Trust government", "Trust PM", "Roman Catholic", "Other Christian", "Methodist", "Baptist", "Judaism", "Hindu", "Islam", "Other", "Orthodox", "Pentecostal", "Evangelical", "Prefer not to say", "Skipped question")


screenreg(list(m1, m2, m3, m4, m5, m6), custom.model.names = c("(1) Trust doctors", "(2) Trust scientists", "(3) Trust doctors", "(4) Trust scientists", "(5) Trust doctors", "(6) Trust scientists"), stars = c(0.1, 0.05, 0.01))
htmlreg(list(m1, m2, m3, m4, m5, m6), custom.model.names = c("(1) Trust doctors", "(2) Trust scientists", "(3) Trust doctors", "(4) Trust scientists", "(5) Trust doctors", "(6) Trust scientists"), stars = c(0.1, 0.05, 0.01), file="output.html")
browseURL("output.html") 



