# REPLICATION FOR ANALYSIS IN FARAG (2020)
# MASS-ELITE DIFFERENCES IN NEW DEMOCRACIES: TUNISIA AS A CASE STUDY (2010-2016)

# 29 APRIL 2020
# R Studio version 1.1.463 and the plyr package 1.8.6


# Setting-up analysis ####

# Clear working space
rm(list = ls())

# Set working directory #
setwd("...")

# Load packages #
library(plyr)


# MASS ISSUE POSITIONS ####

# Mass positions 2011 ####

# Load data
MIP2011 <- read.csv("Tunisia_ABII.csv", row.names = 1, sep =",", dec=".", header = TRUE)


# Question 512: Suppose there was a scale from 1-10 measuring the extent to which democracy is suitable for your country, with 1 meaning that democracy is absolutely inappropriate for your country and 10 meaning that democracy is completely appropriate for your country. To what extent do you think democracy is appropriate for your country?

# Dichotomize responses
MIP2011$q512_recoded <- revalue(MIP2011$q512, c("1. absolutely unsatisfied"="anti-democracy", "2"="anti-democracy", "3"="anti-democracy", "4"="anti-democracy", "5"= "anti-democracy", "6"="pro-democracy", "7"="pro-democracy", "8"="pro-democracy", "9"="pro-democracy", "10. very satisfied"="pro-democracy"))

# Calculate frequency

round(prop.table(table(MIP2011$q512_recoded))*100, 2)

# Calculate relative difference 
(48.91-39.05) / ((48.91+39.05) / (2))

# Calculate difference in percentage 
48.91-39.05


# Question 5161: To what extent do you agree or disagree with the following statements? 1. Under a democratic system, the country's economic performance is weak.

# Dichotomize responses
MIP2011$q5161_recoded <- revalue(MIP2011$q5161, c("1. i strongly agree"="anti-democracy", "2. i agree"="anti-democracy", "3. i disagree"="pro-democracy", "4. i strongly disagree"="pro-democracy"))

# Calculate frequency #

round(prop.table(table(MIP2011$q5161_recoded))*100, 2)

# Calculate relative difference 
(57.69-17.06) / ((57.69+17.06) / (2))

# Calculate difference in percentage 
57.69-17.06


# Question 5163: To what extent do you agree or disagree with the following statements? Democratic systems are not effective at maintaining order and stability. 

# Dichotomize responses
MIP2011$q5163_recoded <- revalue(MIP2011$q5163, c("1. i strongly agree"="anti-democracy", "2. i agree"="anti-democracy", "3. i disagree"="pro-democracy", "4. i strongly disagree"="pro-democracy"))

# Calculate frequency #

round(prop.table(table(MIP2011$q5163_recoded))*100, 2)

# Calculate relative difference 
(58.44-16.64) / ((58.44+16.64) / (2))

# Calculate difference in percentage 
58.44-16.64


# Question 523: To what extent do you think the lack of respect for human rights for security purposes in your country is justified? 

# Dichotomize responses
MIP2011$q523_recoded <- revalue(MIP2011$q523, c("1. justified to a great extent"="anti-democracy", "2. justified to a medium extent"="anti-democracy", "3. justified to a limited extent"="anti-democracy", "4. not justified at all"="pro-democracy"))

# Calculate frequency #

round(prop.table(table(MIP2011$q523_recoded))*100, 2)

# Calculate relative difference 
(53.26-29.68) / ((53.26+29.68) / (2))

# Calculate difference in percentage 
53.26-29.68


# Question 910t: Do you have feelings of victory or personal loss with regard to the Tunisian revolution? 

# Dichotomize responses
MIP2011$t910_recoded <- revalue(MIP2011$t910, c("1. more of personal loss"="anti-democracy", "3. more of personal gain"="pro-democracy"))

# Calculate frequency #

round(prop.table(table(MIP2011$t910_recoded))*100, 2)

# Calculate relative difference 
(41.14-8.28) / ((41.14+8.28) / (2))

# Calculate difference in percentage 
41.14-8.28


# Question 6052:  To what extent do you agree or disagree with each of the following principles in the formulation of your country's laws and regulations? The government and parliament should enact laws in accordance with Islamic law.

# Dichotomize responses
MIP2011$q6052_recoded <- revalue(MIP2011$q6052, c("1. i strongly agree"="pro-Islamism", "2. i agree"="pro-Islamism", "3. i disagree"="anti-Islamism", "4. i strongly disagree"="anti-Islamism"))

# Calculate frequency #

round(prop.table(table(MIP2011$q6052_recoded))*100, 2)

# Calculate relative difference 
(56.69-30.43) / ((56.69+30.43) / (2))

# Calculate difference in percentage 
56.69-30.43


# Question 6053:  To what extent do you agree or disagree with each of the following principles in the formulation of your country's laws and regulations? The government and Shura Council should enact laws in accordance with citizens' wishes with regard to certain subjects and in accordance with Islamic law with regard to other subjects.  

# Dichotomize responses
MIP2011$q6053_recoded <- revalue(MIP2011$q6053, c("1. i strongly agree"="pro-Islamism", "2. i agree"="pro-Islamism", "3. i disagree"="anti-Islamism", "4. i strongly disagree"="anti-Islamism"))

# Calculate frequency #

round(prop.table(table(MIP2011$q6053_recoded))*100, 2)

# Calculate relative difference 
(76.25-12.54) / ((76.25+12.54) / (2))

# Calculate difference in percentage 
76.25-12.54


# Question 6054:  To what extent do you agree or disagree with each of the following principles in the formulation of your country's laws and regulations?  The government and parliament should enact penal laws in accordance with Islamic law.

# Dichotomize responses
MIP2011$q6054_recoded <- revalue(MIP2011$q6054, c("1. i strongly agree"="pro-Islamism", "2. i agree"="pro-Islamism", "3. i disagree"="anti-Islamism", "4. i strongly disagree"="anti-Islamism"))

# Calculate frequency #

round(prop.table(table(MIP2011$q6054_recoded))*100, 2)

# Calculate relative difference 
(29.52-55.10) / ((29.52+55.10) / (2))

# Calculate difference in percentage 
29.52-55.10


# Question 6055:  To what extent do you agree or disagree with each of the following principles in the formulation of your country's laws and regulations? The government and parliament should enact personal status laws (marriage, divorce) in accordance with Islamic law.  

# Dichotomize responses
MIP2011$q6055_recoded <- revalue(MIP2011$q6055, c("1. i strongly agree"="pro-Islamism", "2. i agree"="pro-Islamism", "3. i disagree"="anti-Islamism", "4. i strongly disagree"="anti-Islamism"))

# Calculate frequency #

round(prop.table(table(MIP2011$q6055_recoded))*100, 2)

# Calculate relative difference 
(55.60-28.68) / ((55.60+28.68) / (2))

# Calculate difference in percentage 
55.60-28.68


# Question 6077:  The opinions of Islamic jurists and religious scholars differ with regard to their interpretations of certain issues in Islam. I want to ask to what extent you agree or disagree with some of these issues?  In order to meet the demands of the modern economy, banks should be allowed to charge interest.   

# Dichotomize responses
MIP2011$q6077_recoded <- revalue(MIP2011$q6077, c("1. i strongly agree"="anti-Islamism", "2. i agree"="anti-Islamism", "3. i disagree"="pro-Islamism", "4. i strongly disagree"="pro-Islamism"))

# Calculate frequency #

round(prop.table(table(MIP2011$q6077_recoded))*100, 2)

# Calculate relative difference 
(34.03-36.87) / ((34.03+36.87) / (2))

# Calculate difference in percentage 
34.03-36.87


# Mass positions 2013 ####

# Load data
MIP2013 <- read.csv("Tunisia_ABIII.csv", row.names = 1, sep =",", dec=".", header = TRUE)


# Question 512: Suppose there was a scale from 0-10 measuring the extent to which democracy is suitable for your country, with 0 meaning that democracy is absolutely inappropriate for your country and 10 meaning that democracy is completely appropriate for your country. To what extent do you think democracy is appropriate for your country?

# Dichotomize responses
MIP2013$q512_recoded <- revalue(MIP2013$q512, c("Absolutely inappropriate"="anti-democracy", "1"="anti-democracy", "2"="anti-democracy", "3"="anti-democracy", "4"= "anti-democracy", "6"="pro-democracy", "7"="pro-democracy", "8"="pro-democracy", "9"="pro-democracy", "Completely appropriate"="pro-democracy"))

# Calculate frequency

round(prop.table(table(MIP2013$q512_recoded))*100, 2)

# Calculate relative difference 
(36.95-35.28) / ((36.95+35.28) / (2))

# Calculate difference in percentage 
36.95-35.28


# Question 5161: To what extent do you agree or disagree with the following statements? 1. Under a democratic system, the country's economic performance is weak.

# Dichotomize responses
MIP2013$q5161_recoded <- revalue(MIP2013$q5161, c("I strongly agree"="anti-democracy", "I somewhat agree"="anti-democracy", "I somewhat disagree"="pro-democracy", "I strongly disagree"="pro-democracy"))

# Calculate frequency #

round(prop.table(table(MIP2013$q5161_recoded))*100, 2)

# Calculate relative difference 
(46.71-35.70) / ((46.71+35.70) / (2))

# Calculate difference in percentage 
46.71-35.70


# Question 5163: To what extent do you agree or disagree with the following statements? Democratic systems are not effective at maintaining order and stability. 

# Dichotomize responses
MIP2013$q5163_recoded <- revalue(MIP2013$q5163, c("I strongly agree"="anti-democracy", "I somewhat agree"="anti-democracy", "I somewhat disagree"="pro-democracy", "I strongly disagree"="pro-democracy"))

# Calculate frequency #

round(prop.table(table(MIP2013$q5163_recoded))*100, 2)

# Calculate relative difference 
(41.95-41.12) / ((41.95+41.12) / (2))

# Calculate difference in percentage 
41.95-41.12


# Question 523: To what extent do you think the lack of respect for human rights is justified in order to maintain security in your country?  

# Dichotomize responses
MIP2013$q523_recoded <- revalue(MIP2013$q523, c("Justified to a great extent"="anti-democracy", "Justified to a medium extent"="anti-democracy", "Justified to a limited extent"="anti-democracy", "Not justified at all"="pro-democracy"))

# Calculate frequency #

round(prop.table(table(MIP2013$q523_recoded))*100, 2)

# Calculate relative difference 
(60.13-34.95) / ((60.13+34.95) / (2))

# Calculate difference in percentage 
60.13-34.95


# Question 810a: Do you have feelings of victory or personal loss with regard to the Tunisian revolution? 

# Dichotomize responses
MIP2013$q810a_recoded <- revalue(MIP2013$q810a, c("More of personal loss"="anti-democracy", "More of personal victory"="pro-democracy"))

# Calculate frequency #

round(prop.table(table(MIP2013$q810a_recoded))*100, 2)

# Calculate relative difference 
(33.86-25.69) / ((33.86+25.69) / (2))

# Calculate difference in percentage 
33.86-25.69


# Question 6052:  To what extent do you agree or disagree with each of the following principles in the formulation of your country's laws and regulations? The government and parliament should enact laws in accordance with Islamic law.

# Dichotomize responses
MIP2013$q6052_recoded <- revalue(MIP2013$q6052, c("I strongly agree"="pro-Islamism", "I somewhat agree"="pro-Islamism", "I somewhat disagree"="anti-Islamism", "I strongly disagree"="anti-Islamism"))

# Calculate frequency #

round(prop.table(table(MIP2013$q6052_recoded))*100, 2)

# Calculate relative difference 
(58.30-35.61) / ((58.30+35.61) / (2))

# Calculate difference in percentage 
58.30-35.61


# Question 6053:  To what extent do you agree or disagree with each of the following principles in the formulation of your country's laws and regulations? The government and parliament should enact laws in accordance with citizens' wishes with regard to certain subjects and in accordance with Islamic law with regard to other subjects.  

# Dichotomize responses
MIP2013$q6053_recoded <- revalue(MIP2013$q6053, c("I strongly agree"="pro-Islamism", "I somewhat agree"="pro-Islamism", "I somewhat disagree"="anti-Islamism", "I strongly disagree"="anti-Islamism"))

# Calculate frequency #

round(prop.table(table(MIP2013$q6053_recoded))*100, 2)

# Calculate relative difference 
(70.64-21.85) / ((70.64+21.85) / (2))

# Calculate difference in percentage 
70.64-21.85


# Question 6054:  To what extent do you agree or disagree with each of the following principles in the formulation of your country's laws and regulations?  The government and parliament should enact penal laws in accordance with Islamic law.

# Dichotomize responses
MIP2013$q6054_recoded <- revalue(MIP2013$q6054, c("I strongly agree"="pro-Islamism", "I somewhat agree"="pro-Islamism", "I somewhat disagree"="anti-Islamism", "I strongly disagree"="anti-Islamism"))

# Calculate frequency #

round(prop.table(table(MIP2013$q6054_recoded))*100, 2)

# Calculate relative difference 
(32.86-58.80) / ((32.86+58.80) / (2))

# Calculate difference in percentage 
32.86-58.80


# Question 6055:  To what extent do you agree or disagree with each of the following principles in the formulation of your country's laws and regulations?  The government and parliament should enact personal status laws (marriage, divorce) in accordance with Islamic law.  

# Dichotomize responses
MIP2013$q6055_recoded <- revalue(MIP2013$q6055, c("I strongly agree"="pro-Islamism", "I somewhat agree"="pro-Islamism", "I somewhat disagree"="anti-Islamism", "I strongly disagree"="anti-Islamism"))

# Calculate frequency #

round(prop.table(table(MIP2013$q6055_recoded))*100, 2)

# Calculate relative difference 
(59.38-32.78) / ((59.38+32.78) / (2))

# Calculate difference in percentage 
59.38-32.78


# Question 605a: Which of the following sentences is the closest to your point of view?  Choose sentence 1 or sentence 2. The 1 st  sentence: I prefer a religious political party over a non-religious political party. The 2 nd  sentence: I prefer a non-religious political party over a religious political party 

# Dichotomize responses
MIP2013$q605a_recoded <- revalue(MIP2013$q605a, c("I Strongly agree with the 1st sentence"="pro-Islamism", "I agree with the 1st sentence"="pro-Islamism", "I agree with the 2nd sentence"="anti-Islamism", "I strongly agree with the 2nd sentence"="anti-Islamism"))

# Calculate frequency #

round(prop.table(table(MIP2013$q605a_recoded))*100, 2)

# Calculate relative difference 
(57.96-25.02) / ((57.96+25.02) / (2))

# Calculate difference in percentage 
57.96-25.02


# Mass positions 2016 ####

# Load data
MIP2016 <- read.csv("Tunisia_ABIV.csv", row.names = 1, sep =",", dec=".", header = TRUE)


# Question 512: Suppose there was a scale from 0-10 measuring the extent to which democracy is suitable for your country, with 0 meaning that democracy is absolutely inappropriate for your country and 10 meaning that democracy is completely appropriate for your country. To what extent do you think democracy is appropriate for your country?

# Dichotomize responses
MIP2016$q512_recoded <- revalue(MIP2016$q512, c("Completely inappropriate"="anti-democracy", "1"="anti-democracy", "2"="anti-democracy", "3"="anti-democracy", "4"= "anti-democracy", "6"="pro-democracy", "7"="pro-democracy", "8"="pro-democracy", "9"="pro-democracy", "Completely appropriate"="pro-democracy"))

# Calculate frequency

round(prop.table(table(MIP2016$q512_recoded))*100, 2)

# Calculate relative difference 
(36.83-38.50) / ((36.83+38.50) / (2))

# Calculate difference in percentage 
36.83-38.50


# Question 5161: To what extent do you agree or disagree with the following statements? Under a democratic system, the country's economic performance is weak.

# Dichotomize responses
MIP2016$q5161_recoded <- revalue(MIP2016$q5161, c("I strongly agree"="anti-democracy", "I agree"="anti-democracy", "I disagree"="pro-democracy", "I strongly disagree"="pro-democracy"))

# Calculate frequency #

round(prop.table(table(MIP2016$q5161_recoded))*100, 2)

# Calculate relative difference 
(47.42-45.42) / ((47.42+45.42) / (2))

# Calculate difference in percentage 
47.42-45.42


# Question 5163: To what extent do you agree or disagree with the following statements? Democratic systems are not effective at maintaining order and stability. 

# Dichotomize responses
MIP2016$q5163_recoded <- revalue(MIP2016$q5163, c("I strongly agree"="anti-democracy", "I agree"="anti-democracy", "I disagree"="pro-democracy", "I strongly disagree"="pro-democracy"))

# Calculate frequency #

round(prop.table(table(MIP2016$q5163_recoded))*100, 2)

# Calculate relative difference 
(42.83-51.42) / ((42.83+51.42) / (2))

# Calculate difference in percentage 
42.83-51.42


# Question 523: To what degree would you agree that the violation of human rights committed by the government in your country is justifiable in the name of promoting security and stability? 

# Dichotomize responses
MIP2016$q523_recoded <- revalue(MIP2016$q523, c("Completely justified"="anti-democracy", "Somewhat justified"="anti-democracy", "Not very justified"="anti-democracy", "Not justifiable at all"="pro-democracy"))

# Calculate frequency #

round(prop.table(table(MIP2016$q523_recoded))*100, 2)

# Calculate relative difference 
(48.17-48.58) / ((48.17+48.58) / (2))

# Calculate difference in percentage 
48.17-48.58


# Question 605:  Which of the following statements is the closest to your point of view? 

# Dichotomize responses
MIP2016$q605_recoded <- revalue(MIP2016$q605, c("Laws of our country should be based entirely on the sharia"="anti-Islamism", "Laws of our country should be based mostly on the sharia"="anti-Islamism", "Laws of our country should be based equally on sharia and the will of the people"="pro-Islamism", "Laws of our country should be based mostly on the will of the people"="anti-Islamism", "Laws of our country should be based entirely on the will of the people"="anti-Islamism"))

# Calculate frequency #

round(prop.table(table(MIP2016$q605_recoded))*100, 2)

# Calculate relative difference 
(62.00-34.08) / ((62.00+34.08) / (2))

# Calculate difference in percentage 
62.00-34.08


# Question 6077:  Today as in the past, Muslim scholars and jurists sometimes disagree about the proper interpretation of Islam in response to present-day issues. For each of the statements listed below, please indicate whether you agree strongly, agree, disagree, or disagree strongly with the interpretation of Islam that is presented. In order to meet the demands of the modern economy, banks should be allowed to charge interest.  

# Dichotomize responses
MIP2016$q6077_recoded <- revalue(MIP2016$q6077, c("I strongly agree"="anti-Islamism", "I agree"="anti-Islamism", "I disagree"="pro-Islamism", "I strongly disagree"="pro-Islamism"))

# Calculate frequency #

round(prop.table(table(MIP2016$q6077_recoded))*100, 2)

# Calculate relative difference 
(40.25-48.42) / ((40.25+48.42) / (2))

# Calculate difference in percentage 
40.25-48.42


# MASS ISSUE SALIENCE ####

# Mass salience 2011 ####

MIS2011 <- read.csv("Tunisia_ABII.csv", row.names = 1, sep =",", dec=".", header = TRUE)

# Questions 2061 and 2062: What are the two most important challenges your country is facing today?

# Most important challenge 
round(prop.table(table(MIS2011$q2061))*100, 2)

# Second most important challenge 
round(prop.table(table(MIS2011$q2062))*100, 2)


# Mass salience 2013 ####

MIS2013 <- read.csv("Tunisia_ABIII.csv", row.names = 1, sep =",", dec=".", header = TRUE)

# Questions 2061 and 2062: What are the two most important challenges your country is facing today?

# Most important challenge 
round(prop.table(table(MIS2013$q2061))*100, 2)

# Second most important challenge 
round(prop.table(table(MIS2013$q2062))*100, 2)


# MASS salience 2016 ####

MIS2016 <- read.csv("Tunisia_ABIV.csv", row.names = 1, sep =",", dec=".", header = TRUE)

# Questions 2061 and 2062: What are the two most important challenges your country is facing today?

# Most important challenge 
round(prop.table(table(MIS2016$q2061))*100, 2)

# Second most important challenge 
round(prop.table(table(MIS2016$q2062))*100, 2)


# ELITE ISSUE POSITIONS ####

# Ennahda positions ####

# Load coding of Ennahda political party's 2014 manifesto

EnnahdaIP <- read.csv("Ennahda_Party_Positions.csv", row.names = 1, sep =",", dec=".", header = TRUE)


# Calculate democratic-authoritarian dimension for Ennahda

(EnnahdaIP$X201.1+EnnahdaIP$X201.2+EnnahdaIP$X202.1+EnnahdaIP$X203+EnnahdaIP$X304)-(EnnahdaIP$X202.2+EnnahdaIP$X204+EnnahdaIP$X305.4)

# Calculate secular-Islamist dimension for Ennahda

(EnnahdaIP$X601+EnnahdaIP$X603)-(EnnahdaIP$X602+EnnahdaIP$X604)


# Nidaa Tounes positions ####

# Load coding of Nidaa Tounes political party's 2014 manifesto

NidaaTounesIP <- read.csv("Nidaa_Tounes_Party_Positions.csv", row.names = 1, sep =",", dec=".", header = TRUE)

# Calculate democratic-authoritarian dimension for Nidaa Tounes

(NidaaTounesIP$X201.1+NidaaTounesIP$X201.2+NidaaTounesIP$X202.1+NidaaTounesIP$X203+NidaaTounesIP$X304)-(NidaaTounesIP$X202.2+NidaaTounesIP$X204+NidaaTounesIP$X305.4)

# Calculate secular-Islamist dimension for Nidaa Tounes

(NidaaTounesIP$X601+NidaaTounesIP$X603)-(NidaaTounesIP$X602+NidaaTounesIP$X604)


# ELITE ISSUE SALIENCE ####

# Ennahda issue salience ####

EnnahdaIP[ , c("PD1", "PD2", "PD3", "PD4", "PD5", "PD6", "PD7")] 


# Nidaa issue salience ####

NidaaTounesIP[ , c("PD1", "PD2", "PD3", "PD4", "PD5", "PD6", "PD7")] 

  