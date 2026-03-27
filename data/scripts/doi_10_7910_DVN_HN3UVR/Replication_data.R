install.packages("XML")
install.packages("tm")
install.packages("textstem")
install.packages("quanteda")
install.packages("quanteda.textstats") 
install.packages("quanteda.textplots")
install.packages("tidytext")
install.packages("rlang")
install.packages("ggplot2")
install.packages("viridis")
install.packages("wordcloud")
install.packages("tokenizers")
install.packages("hunspell")
install.packages("dplyr")
install.packages("stm")
library(XML)
library(tm)
library(textstem)
library(quanteda)
library(quanteda.textstats) #for descriptive statistics
library(quanteda.textplots)
library(tidytext)
library(rlang)
library(ggplot2)
library(viridis)
library(wordcloud)
library(hunspell)
library(tokenizers)
library(dplyr)
library(stm)

##Scraping couldn't be done in a dynamic way (via Selenium) because the debates had very different URLs
##So each debate webpage was manually opened and scraped
##SCRAPING Ukraine

setwd("data/")

# Reading the xml 
ukraine_data= read_xml('https://www.europarl.europa.eu/doceo/document/CRE-9-2022-06-07-ITM-010_EN.xml')
# display food_data
ukraine_data

ukraine_xml <- xmlParse(ukraine_data)
class(ukraine_xml)
root <- xmlRoot(ukraine_xml)
xmlName(root)

xmlSize(root) # 118, so the number of children nodes is defined not by unified elements (just INTERVENTION, but also many other things)

# here I compare two neigbour nodes
root[[61]]

root[[62]] # so, they are not the same


#### In this case, we have to use XPath approach

#first, we extract the elements of INTERVENTION node.

#but let's check 
root[['INTERVENTION']] 

### here i am just looking at the structure
# root[[62]]['PARA']
# root[[62]][c('ORATEUR', 'PARA')]

?xpathSApply

# speech
speech <- xpathSApply(root, '//INTERVENTION', fun = xmlValue) 

# language
lang <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LG") 

#name of member of parliament
name_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LIB")

# party
party_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "PP")

df_1 <- data.frame(name = name_orateur, language = lang, text = speech, party = party_orateur)

write.table(df_1, "data/debates_ukraine\\UKR_2022_06_07.txt", sep = '^')

#-----------------------------------------------------------
#repeat the same for the other URLs to create a corpus of TXTs
ukraine_data= read_xml('https://www.europarl.europa.eu/doceo/document/CRE-9-2022-05-19-ITM-003_EN.xml') 

ukraine_xml <- xmlParse(ukraine_data) 
root <- xmlRoot(ukraine_xml) 
root[['INTERVENTION']]

# speech
speech <- xpathSApply(root, '//INTERVENTION', fun = xmlValue) 

# language
lang <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LG") 

#name of member of parliament
name_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LIB")

# party
party_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "PP")

df_2 <- data.frame(name = name_orateur, language = lang, text = speech, party = party_orateur) 

write.table(df_2, "data/debates_ukraine\\UKR_2022_05_19.txt", sep = '^')

#---------------------------------
ukraine_data= read_xml('https://www.europarl.europa.eu/doceo/document/CRE-9-2022-05-19-ITM-005_EN.xml')

ukraine_xml <- xmlParse(ukraine_data)
root <- xmlRoot(ukraine_xml)
root[['INTERVENTION']]

# speech
speech <- xpathSApply(root, '//INTERVENTION', fun = xmlValue) 

# language
lang <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LG") 

#name of member of parliament
name_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LIB")

# party
party_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "PP")

df_3 <- data.frame(name = name_orateur, language = lang, text = speech, party = party_orateur) 

write.table(df_3, "data/debates_ukraine\\UKR_2022_05_19(1).txt", sep = '^')


#---------------------------------
ukraine_data= read_xml('https://www.europarl.europa.eu/doceo/document/CRE-9-2022-05-19-ITM-008-07_EN.xml')

ukraine_xml <- xmlParse(ukraine_data)
root <- xmlRoot(ukraine_xml)
root[['INTERVENTION']]

# speech
speech <- xpathSApply(root, '//INTERVENTION', fun = xmlValue) 

# language
lang <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LG") 

#name of member of parliament
name_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LIB")

# party
party_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "PP")

df_4 <- data.frame(name = name_orateur, language = lang, text = speech, party = party_orateur) 

write.table(df_4, "data/debates_ukraine\\UKR_2022_05_19(2).txt", sep = '^')

#---------------------------------

ukraine_data= read_xml('https://www.europarl.europa.eu/doceo/document/CRE-9-2022-05-19-ITM-008-08_EN.xml')

ukraine_xml <- xmlParse(ukraine_data)
root <- xmlRoot(ukraine_xml)
root[['INTERVENTION']]

# speech
speech <- xpathSApply(root, '//INTERVENTION', fun = xmlValue) 

# language
lang <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LG") 

#name of member of parliament
name_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LIB")

# party
party_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "PP")

df_5 <- data.frame(name = name_orateur, language = lang, text = speech, party = party_orateur) 

write.table(df_5, "data/debates_ukraine\\UKR_2022_05_19(3).txt", sep = '^')


#---------------------------------

ukraine_data= read_xml('https://www.europarl.europa.eu/doceo/document/CRE-9-2022-05-05-ITM-004_EN.xml')

ukraine_xml <- xmlParse(ukraine_data)
root <- xmlRoot(ukraine_xml)
root[['INTERVENTION']]

# speech
speech <- xpathSApply(root, '//INTERVENTION', fun = xmlValue) 

# language
lang <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LG") 

#name of member of parliament
name_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LIB")

# party
party_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "PP")

df_6 <- data.frame(name = name_orateur, language = lang, text = speech, party = party_orateur) 

write.table(df_6, "data/debates_ukraine\\UKR_2022_2022_05_05.txt", sep = '^')


#---------------------------------

ukraine_data= read_xml('https://www.europarl.europa.eu/doceo/document/CRE-9-2022-05-05-ITM-007-12_EN.xml')

ukraine_xml <- xmlParse(ukraine_data)
root <- xmlRoot(ukraine_xml)
root[['INTERVENTION']]

# speech
speech <- xpathSApply(root, '//INTERVENTION', fun = xmlValue) 

# language
lang <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LG") 

#name of member of parliament
name_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LIB")

# party
party_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "PP")

df_7 <- data.frame(name = name_orateur, language = lang, text = speech, party = party_orateur) 

write.table(df_7, "data/debates_ukraine\\UKR_2022_2022_05_05(1).txt", sep = '^')


#---------------------------------
ukraine_data= read_xml('https://www.europarl.europa.eu/doceo/document/CRE-9-2022-05-05-ITM-011-05_EN.xml')

ukraine_xml <- xmlParse(ukraine_data)
root <- xmlRoot(ukraine_xml)
root[['INTERVENTION']]

# speech
speech <- xpathSApply(root, '//INTERVENTION', fun = xmlValue) 

# language
lang <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LG") 

#name of member of parliament
name_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LIB")

# party
party_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "PP")

df_8 <- data.frame(name = name_orateur, language = lang, text = speech, party = party_orateur) 

write.table(df_8, "data/debates_ukraine\\UKR_2022_2022_05_05(2).txt", sep = '^')


#---------------------------------
ukraine_data= read_xml('https://www.europarl.europa.eu/doceo/document/CRE-9-2022-05-04-ITM-004_EN.xml')

ukraine_xml <- xmlParse(ukraine_data)
root <- xmlRoot(ukraine_xml)
root[['INTERVENTION']]

# speech
speech <- xpathSApply(root, '//INTERVENTION', fun = xmlValue) 

# language
lang <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LG") 

#name of member of parliament
name_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LIB")

# party
party_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "PP")

df_9 <- data.frame(name = name_orateur, language = lang, text = speech, party = party_orateur) 

write.table(df_9, "data/debates_ukraine\\UKR_2022_2022_05_04.txt", sep = '^')

#---------------------------------

ukraine_data= read_xml('https://www.europarl.europa.eu/doceo/document/CRE-9-2022-05-03-ITM-014_EN.xml')

ukraine_xml <- xmlParse(ukraine_data)
root <- xmlRoot(ukraine_xml)
root[['INTERVENTION']]

# speech
speech <- xpathSApply(root, '//INTERVENTION', fun = xmlValue) 

# language
lang <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LG") 

#name of member of parliament
name_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LIB")

# party
party_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "PP")

df_10 <- data.frame(name = name_orateur, language = lang, text = speech, party = party_orateur) 

write.table(df_10, "data/debates_ukraine\\UKR_2022_2022_05_03.txt", sep = '^')

#---------------------------------
ukraine_data= read_xml('https://www.europarl.europa.eu/doceo/document/CRE-9-2022-05-03-ITM-015_EN.xml')

ukraine_xml <- xmlParse(ukraine_data)
root <- xmlRoot(ukraine_xml)
root[['INTERVENTION']]

# speech
speech <- xpathSApply(root, '//INTERVENTION', fun = xmlValue) 

# language
lang <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LG") 

#name of member of parliament
name_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LIB")

# party
party_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "PP")

df_11 <- data.frame(name = name_orateur, language = lang, text = speech, party = party_orateur) 

write.table(df_11, "data/debates_ukraine\\UKR_2022_2022_05_03(2).txt", sep = '^')

#---------------------------------
ukraine_data= read_xml('https://www.europarl.europa.eu/doceo/document/CRE-9-2022-04-07-ITM-010-04_EN.xml')

ukraine_xml <- xmlParse(ukraine_data)
root <- xmlRoot(ukraine_xml)
root[['INTERVENTION']]

# speech
speech <- xpathSApply(root, '//INTERVENTION', fun = xmlValue) 

# language
lang <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LG") 

#name of member of parliament
name_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LIB")

# party
party_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "PP")

df_12 <- data.frame(name = name_orateur, language = lang, text = speech, party = party_orateur) 

write.table(df_12, "data/debates_ukraine\\UKR_2022_2022_04_07.txt", sep = '^')

#---------------------------------
ukraine_data= read_xml('https://www.europarl.europa.eu/doceo/document/CRE-9-2022-04-07-ITM-010-05_EN.xml')

ukraine_xml <- xmlParse(ukraine_data)
root <- xmlRoot(ukraine_xml)
root[['INTERVENTION']]

# speech
speech <- xpathSApply(root, '//INTERVENTION', fun = xmlValue) 

# language
lang <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LG") 

#name of member of parliament
name_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LIB")

# party
party_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "PP")

df_13 <- data.frame(name = name_orateur, language = lang, text = speech, party = party_orateur) 

write.table(df_13, "data/debates_ukraine\\UKR_2022_2022_04_07(1).txt", sep = '^')

#---------------------------------

ukraine_data= read_xml('https://www.europarl.europa.eu/doceo/document/CRE-9-2022-04-06-ITM-003_EN.xml')

ukraine_xml <- xmlParse(ukraine_data)
root <- xmlRoot(ukraine_xml)
root[['INTERVENTION']]

# speech
speech <- xpathSApply(root, '//INTERVENTION', fun = xmlValue) 

# language
lang <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LG") 

#name of member of parliament
name_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LIB")

# party
party_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "PP")

df_14 <- data.frame(name = name_orateur, language = lang, text = speech, party = party_orateur) 

write.table(df_14, "data/debates_ukraine\\UKR_2022_2022_04_06.txt", sep = '^')

#---------------------------------

ukraine_data= read_xml('https://www.europarl.europa.eu/doceo/document/CRE-9-2022-04-05-ITM-003_EN.xml')

ukraine_xml <- xmlParse(ukraine_data)
root <- xmlRoot(ukraine_xml)
root[['INTERVENTION']]

# speech
speech <- xpathSApply(root, '//INTERVENTION', fun = xmlValue) 

# language
lang <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LG") 

#name of member of parliament
name_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LIB")

# party
party_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "PP")

df_15 <- data.frame(name = name_orateur, language = lang, text = speech, party = party_orateur) 

write.table(df_15, "data/debates_ukraine\\UKR_2022_2022_04_05.txt", sep = '^')

#---------------------------------

ukraine_data= read_xml('https://www.europarl.europa.eu/doceo/document/CRE-9-2022-03-24-ITM-010-04_EN.xml')

ukraine_xml <- xmlParse(ukraine_data)
root <- xmlRoot(ukraine_xml)
root[['INTERVENTION']]

# speech
speech <- xpathSApply(root, '//INTERVENTION', fun = xmlValue) 

# language
lang <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LG") 

#name of member of parliament
name_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LIB")

# party
party_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "PP")

df_16 <- data.frame(name = name_orateur, language = lang, text = speech, party = party_orateur) 

write.table(df_16, "data/debates_ukraine\\UKR_2022_2022_03_24.txt", sep = '^')

#---------------------------------
ukraine_data= read_xml('https://www.europarl.europa.eu/doceo/document/CRE-9-2022-03-23-ITM-019_EN.xml')

ukraine_xml <- xmlParse(ukraine_data)
root <- xmlRoot(ukraine_xml)
root[['INTERVENTION']]

# speech
speech <- xpathSApply(root, '//INTERVENTION', fun = xmlValue) 

# language
lang <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LG") 

#name of member of parliament
name_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LIB")

# party
party_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "PP")

df_17 <- data.frame(name = name_orateur, language = lang, text = speech, party = party_orateur) 

write.table(df_17, "data/debates_ukraine\\UKR_2022_2022_03_23.txt", sep = '^')

#---------------------------------
ukraine_data= read_xml('https://www.europarl.europa.eu/doceo/document/CRE-9-2022-03-09-ITM-006_EN.xml')

ukraine_xml <- xmlParse(ukraine_data)
root <- xmlRoot(ukraine_xml)
root[['INTERVENTION']]

# speech
speech <- xpathSApply(root, '//INTERVENTION', fun = xmlValue) 

# language
lang <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LG") 

#name of member of parliament
name_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LIB")

# party
party_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "PP")

df_18 <- data.frame(name = name_orateur, language = lang, text = speech, party = party_orateur) 

write.table(df_18, "data/debates_ukraine\\UKR_2022_2022_03_09.txt", sep = '^')

#---------------------------------
ukraine_data= read_xml('https://www.europarl.europa.eu/doceo/document/CRE-9-2022-03-09-ITM-008_EN.xml')

ukraine_xml <- xmlParse(ukraine_data)
root <- xmlRoot(ukraine_xml)
root[['INTERVENTION']]

# speech
speech <- xpathSApply(root, '//INTERVENTION', fun = xmlValue) 

# language
lang <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LG") 

#name of member of parliament
name_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LIB")

# party
party_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "PP")

df_19 <- data.frame(name = name_orateur, language = lang, text = speech, party = party_orateur) 

write.table(df_19, "data/debates_ukraine\\UKR_2022_2022_03_09(2).txt", sep = '^')

#---------------------------------
ukraine_data= read_xml('https://www.europarl.europa.eu/doceo/document/CRE-9-2022-03-08-ITM-009_EN.xml')

ukraine_xml <- xmlParse(ukraine_data)
root <- xmlRoot(ukraine_xml)
root[['INTERVENTION']]

# speech
speech <- xpathSApply(root, '//INTERVENTION', fun = xmlValue) 

# language
lang <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LG") 

#name of member of parliament
name_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LIB")

# party
party_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "PP")

df_20 <- data.frame(name = name_orateur, language = lang, text = speech, party = party_orateur) 

write.table(df_20, "data/debates_ukraine\\UKR_2022_2022_03_08.txt", sep = '^')

#---------------------------------
ukraine_data= read_xml('https://www.europarl.europa.eu/doceo/document/CRE-9-2022-03-01-ITM-009_EN.xml')

ukraine_xml <- xmlParse(ukraine_data)
root <- xmlRoot(ukraine_xml)
root[['INTERVENTION']]

# speech
speech <- xpathSApply(root, '//INTERVENTION', fun = xmlValue) 

# language
lang <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LG") 

#name of member of parliament
name_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LIB")

# party
party_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "PP")

df_21 <- data.frame(name = name_orateur, language = lang, text = speech, party = party_orateur) 

write.table(df_21, "data/debates_ukraine\\UKR_2022_2022_03_01.txt", sep = '^')

#---------------------------------
ukraine_data= read_xml('https://www.europarl.europa.eu/doceo/document/CRE-9-2022-03-01-ITM-011_EN.xml')

ukraine_xml <- xmlParse(ukraine_data)
root <- xmlRoot(ukraine_xml)
root[['INTERVENTION']]

# speech
speech <- xpathSApply(root, '//INTERVENTION', fun = xmlValue) 

# language
lang <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LG") 

#name of member of parliament
name_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LIB")

# party
party_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "PP")

df_22 <- data.frame(name = name_orateur, language = lang, text = speech, party = party_orateur) 

write.table(df_22, "data/debates_ukraine\\UKR_2022_2022_03_01(1).txt", sep = '^')

#-----------------------------------

ukraine_data= read_xml('https://www.europarl.europa.eu/doceo/document/CRE-9-2022-06-08-ITM-016-04_EN.xml') 

ukraine_xml <- xmlParse(ukraine_data) 
root <- xmlRoot(ukraine_xml) 
root[['INTERVENTION']]

# speech
speech <- xpathSApply(root, '//INTERVENTION', fun = xmlValue) 

# language
lang <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LG") 

#name of member of parliament
name_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LIB")

# party
party_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "PP")

df_23 <- data.frame(name = name_orateur, language = lang, text = speech, party = party_orateur) 

write.table(df_23, "data/debates_ukraine\\UKR_2022_06_08.txt", sep = '^')

#----------------------------------------

ukraine_data= read_xml('https://www.europarl.europa.eu/doceo/document/CRE-9-2022-06-22-ITM-024_EN.xml') 

ukraine_xml <- xmlParse(ukraine_data) 
root <- xmlRoot(ukraine_xml) 
root[['INTERVENTION']]

# speech
speech <- xpathSApply(root, '//INTERVENTION', fun = xmlValue) 

# language
lang <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LG") 

#name of member of parliament
name_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LIB")

# party
party_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "PP")

df_24 <- data.frame(name = name_orateur, language = lang, text = speech, party = party_orateur) 

write.table(df_24, "data/debates_ukraine\\UKR_2022_06_22.txt", sep = '^')

#----------------------------------------

ukraine_data= read_xml('https://www.europarl.europa.eu/doceo/document/CRE-9-2022-06-22-ITM-015_EN.xml') 

ukraine_xml <- xmlParse(ukraine_data) 
root <- xmlRoot(ukraine_xml) 
root[['INTERVENTION']]

# speech
speech <- xpathSApply(root, '//INTERVENTION', fun = xmlValue) 

# language
lang <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LG") 

#name of member of parliament
name_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LIB")

# party
party_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "PP")

df_25 <- data.frame(name = name_orateur, language = lang, text = speech, party = party_orateur) 

write.table(df_25, "data/debates_ukraine\\UKR_2022_06_22.1.txt", sep = '^')

#----------------------------------------

ukraine_data = read_xml('https://www.europarl.europa.eu/doceo/document/CRE-9-2022-06-23-ITM-009-03_EN.xml') 

ukraine_xml <- xmlParse(ukraine_data) 
root <- xmlRoot(ukraine_xml) 
root[['INTERVENTION']]

# speech
speech <- xpathSApply(root, '//INTERVENTION', fun = xmlValue) 

# language
lang <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LG") 

#name of member of parliament
name_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LIB")

# party
party_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "PP")

df_26 <- data.frame(name = name_orateur, language = lang, text = speech, party = party_orateur) 

write.table(df_26, "data/debates_ukraine\\UKR_2022_06_23.txt", sep = '^')

#----------------------------------------

ukraine_data = read_xml('https://www.europarl.europa.eu/doceo/document/CRE-9-2022-06-23-ITM-009-01_EN.xml') 

ukraine_xml <- xmlParse(ukraine_data) 
root <- xmlRoot(ukraine_xml) 
root[['INTERVENTION']]

# speech
speech <- xpathSApply(root, '//INTERVENTION', fun = xmlValue) 

# language
lang <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LG") 

#name of member of parliament
name_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LIB")

# party
party_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "PP")

df_27 <- data.frame(name = name_orateur, language = lang, text = speech, party = party_orateur) 

write.table(df_27, "data/debates_ukraine\\UKR_2022_06_23.1.txt", sep = '^')

#----------------------------------------

ukraine_data = read_xml('https://www.europarl.europa.eu/doceo/document/CRE-9-2022-06-23-ITM-008-01_EN.xml') 

ukraine_xml <- xmlParse(ukraine_data) 
root <- xmlRoot(ukraine_xml) 
root[['INTERVENTION']]

# speech
speech <- xpathSApply(root, '//INTERVENTION', fun = xmlValue) 

# language
lang <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LG") 

#name of member of parliament
name_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LIB")

# party
party_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "PP")

df_28 <- data.frame(name = name_orateur, language = lang, text = speech, party = party_orateur) 

write.table(df_28, "data/debates_ukraine\\UKR_2022_06_23.2.txt", sep = '^')

#----------------------------------------

ukraine_data = read_xml('https://www.europarl.europa.eu/doceo/document/CRE-9-2022-07-06-ITM-011-04_EN.xml') 

ukraine_xml <- xmlParse(ukraine_data) 
root <- xmlRoot(ukraine_xml) 
root[['INTERVENTION']]

# speech
speech <- xpathSApply(root, '//INTERVENTION', fun = xmlValue) 

# language
lang <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LG") 

#name of member of parliament
name_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LIB")

# party
party_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "PP")

df_29 <- data.frame(name = name_orateur, language = lang, text = speech, party = party_orateur) 

write.table(df_29, "data/debates_ukraine\\UKR_2022_07_06.txt", sep = '^')

#----------------------------------------

ukraine_data = read_xml('https://www.europarl.europa.eu/doceo/document/CRE-9-2022-09-13-ITM-013_EN.xml') 

ukraine_xml <- xmlParse(ukraine_data) 
root <- xmlRoot(ukraine_xml) 
root[['INTERVENTION']]

# speech
speech <- xpathSApply(root, '//INTERVENTION', fun = xmlValue) 

# language
lang <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LG") 

#name of member of parliament
name_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LIB")

# party
party_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "PP")

df_30 <- data.frame(name = name_orateur, language = lang, text = speech, party = party_orateur) 

write.table(df_30, "data/debates_ukraine\\UKR_2022_09_13.txt", sep = '^')

#----------------------------------------

ukraine_data = read_xml('https://www.europarl.europa.eu/doceo/document/CRE-9-2022-10-05-ITM-002_EN.xml') 

ukraine_xml <- xmlParse(ukraine_data) 
root <- xmlRoot(ukraine_xml) 
root[['INTERVENTION']]

# speech
speech <- xpathSApply(root, '//INTERVENTION', fun = xmlValue) 

# language
lang <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LG") 

#name of member of parliament
name_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LIB")

# party
party_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "PP")

df_31 <- data.frame(name = name_orateur, language = lang, text = speech, party = party_orateur) 

write.table(df_31, "data/debates_ukraine\\UKR_2022_10_05.txt", sep = '^')

#----------------------------------------

ukraine_data = read_xml('https://www.europarl.europa.eu/doceo/document/CRE-9-2022-10-06-ITM-005-06_EN.xml') 

ukraine_xml <- xmlParse(ukraine_data) 
root <- xmlRoot(ukraine_xml) 
root[['INTERVENTION']]

# speech
speech <- xpathSApply(root, '//INTERVENTION', fun = xmlValue) 

# language
lang <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LG") 

#name of member of parliament
name_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LIB")

# party
party_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "PP")

df_32 <- data.frame(name = name_orateur, language = lang, text = speech, party = party_orateur) 

write.table(df_32, "data/debates_ukraine\\UKR_2022_10_06.txt", sep = '^')

#----------------------------------------

ukraine_data = read_xml('https://www.europarl.europa.eu/doceo/document/CRE-9-2022-10-18-ITM-016_EN.xml') 

ukraine_xml <- xmlParse(ukraine_data) 
root <- xmlRoot(ukraine_xml) 
root[['INTERVENTION']]

# speech
speech <- xpathSApply(root, '//INTERVENTION', fun = xmlValue) 

# language
lang <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LG") 

#name of member of parliament
name_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LIB")

# party
party_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "PP")

df_33 <- data.frame(name = name_orateur, language = lang, text = speech, party = party_orateur) 

write.table(df_33, "data/debates_ukraine\\UKR_2022_10_18.txt", sep = '^')

#----------------------------------------

ukraine_data = read_xml('https://www.europarl.europa.eu/doceo/document/CRE-9-2022-10-18-ITM-002_EN.xml') 

ukraine_xml <- xmlParse(ukraine_data) 
root <- xmlRoot(ukraine_xml) 
root[['INTERVENTION']]

# speech
speech <- xpathSApply(root, '//INTERVENTION', fun = xmlValue) 

# language
lang <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LG") 

#name of member of parliament
name_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LIB")

# party
party_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "PP")

df_34 <- data.frame(name = name_orateur, language = lang, text = speech, party = party_orateur) 

write.table(df_34, "data/debates_ukraine\\UKR_2022_10_18.1.txt", sep = '^')

#----------------------------------------

ukraine_data = read_xml('https://www.europarl.europa.eu/doceo/document/CRE-9-2022-10-19-ITM-017_EN.xml') 

ukraine_xml <- xmlParse(ukraine_data) 
root <- xmlRoot(ukraine_xml) 
root[['INTERVENTION']]

# speech
speech <- xpathSApply(root, '//INTERVENTION', fun = xmlValue) 

# language
lang <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LG") 

#name of member of parliament
name_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LIB")

# party
party_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "PP")

df_35 <- data.frame(name = name_orateur, language = lang, text = speech, party = party_orateur) 

write.table(df_35, "data/debates_ukraine\\UKR_2022_10_19.txt", sep = '^')

#----------------------------------------

ukraine_data = read_xml('https://www.europarl.europa.eu/doceo/document/CRE-9-2022-10-20-ITM-012-04_EN.xml') 

ukraine_xml <- xmlParse(ukraine_data) 
root <- xmlRoot(ukraine_xml) 
root[['INTERVENTION']]

# speech
speech <- xpathSApply(root, '//INTERVENTION', fun = xmlValue) 

# language
lang <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LG") 

#name of member of parliament
name_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LIB")

# party
party_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "PP")

df_36 <- data.frame(name = name_orateur, language = lang, text = speech, party = party_orateur) 

write.table(df_36, "data/debates_ukraine\\UKR_2022_10_20.txt", sep = '^')

#----------------------------------------

ukraine_data = read_xml('https://www.europarl.europa.eu/doceo/document/CRE-9-2022-10-20-ITM-003_EN.xml') 

ukraine_xml <- xmlParse(ukraine_data) 
root <- xmlRoot(ukraine_xml) 
root[['INTERVENTION']]

# speech
speech <- xpathSApply(root, '//INTERVENTION', fun = xmlValue) 

# language
lang <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LG") 

#name of member of parliament
name_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LIB")

# party
party_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "PP")

df_37 <- data.frame(name = name_orateur, language = lang, text = speech, party = party_orateur) 

write.table(df_37, "data/debates_ukraine\\UKR_2022_10_20.1.txt", sep = '^')

#----------------------------------------

ukraine_data = read_xml('https://www.europarl.europa.eu/doceo/document/CRE-9-2022-11-22-ITM-018_EN.xml') 

ukraine_xml <- xmlParse(ukraine_data) 
root <- xmlRoot(ukraine_xml) 
root[['INTERVENTION']]

# speech
speech <- xpathSApply(root, '//INTERVENTION', fun = xmlValue) 

# language
lang <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LG") 

#name of member of parliament
name_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LIB")

# party
party_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "PP")

df_38 <- data.frame(name = name_orateur, language = lang, text = speech, party = party_orateur) 

write.table(df_38, "data/debates_ukraine\\UKR_2022_11_22.txt", sep = '^')

#----------------------------------------

ukraine_data = read_xml('https://www.europarl.europa.eu/doceo/document/CRE-9-2022-11-23-ITM-009_EN.xml') 

ukraine_xml <- xmlParse(ukraine_data) 
root <- xmlRoot(ukraine_xml) 
root[['INTERVENTION']]

# speech
speech <- xpathSApply(root, '//INTERVENTION', fun = xmlValue) 

# language
lang <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LG") 

#name of member of parliament
name_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LIB")

# party
party_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "PP")

df_39 <- data.frame(name = name_orateur, language = lang, text = speech, party = party_orateur) 

write.table(df_39, "data/debates_ukraine\\UKR_2022_11_23.txt", sep = '^')

#----------------------------------------

ukraine_data = read_xml('https://www.europarl.europa.eu/doceo/document/CRE-9-2022-11-24-ITM-010-01_EN.xml') 

ukraine_xml <- xmlParse(ukraine_data) 
root <- xmlRoot(ukraine_xml) 
root[['INTERVENTION']]

# speech
speech <- xpathSApply(root, '//INTERVENTION', fun = xmlValue) 

# language
lang <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LG") 

#name of member of parliament
name_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LIB")

# party
party_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "PP")

df_40 <- data.frame(name = name_orateur, language = lang, text = speech, party = party_orateur) 

write.table(df_40, "data/debates_ukraine\\UKR_2022_11_24.txt", sep = '^')

#----------------------------------------

ukraine_data = read_xml('https://www.europarl.europa.eu/doceo/document/CRE-9-2022-11-24-ITM-005-03_EN.xml') 

ukraine_xml <- xmlParse(ukraine_data) 
root <- xmlRoot(ukraine_xml) 
root[['INTERVENTION']]

# speech
speech <- xpathSApply(root, '//INTERVENTION', fun = xmlValue) 

# language
lang <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LG") 

#name of member of parliament
name_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LIB")

# party
party_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "PP")

df_41 <- data.frame(name = name_orateur, language = lang, text = speech, party = party_orateur) 

write.table(df_41, "data/debates_ukraine\\UKR_2022_11_24.1.txt", sep = '^')

#----------------------------------------

ukraine_data = read_xml('https://www.europarl.europa.eu/doceo/document/CRE-9-2022-12-15-ITM-004_EN.xml') 

ukraine_xml <- xmlParse(ukraine_data) 
root <- xmlRoot(ukraine_xml) 
root[['INTERVENTION']]

# speech
speech <- xpathSApply(root, '//INTERVENTION', fun = xmlValue) 

# language
lang <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LG") 

#name of member of parliament
name_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "LIB")

# party
party_orateur <- xpathSApply(root, "//INTERVENTION/ORATEUR", xmlGetAttr, "PP")

df_42 <- data.frame(name = name_orateur, language = lang, text = speech, party = party_orateur) 

write.table(df_42, "data/debates_ukraine\\UKR_2022_12_15.txt", sep = '^')

#---------------------------------

#merge all data frames

df_total_UKR <- rbind.data.frame(df_1,df_2,df_3,df_4,df_5,df_6,df_7,df_8,df_9,df_10,df_11,df_12,df_13,df_14,df_15,df_16,df_17,df_18,df_19,df_20,df_21,df_22,df_23,df_24,df_25,df_26,df_27,df_28,df_29,df_30,df_31,df_32,df_33,df_34,df_35,df_36,df_37,df_38,df_39,df_40,df_41,df_42)

#write as a txt file the df_total_UKR: data frame with all interventions

df_total_UKR$id <- row.names(df_total_UKR)


write.table(df_total_UKR, "data/debates_ukraine\\df_total_UKR.txt", sep = '^')

#------------------------------------------------------------------

#42 debates about Ukraine in total

#------------------------------------------------------------------

#subset the data frame in n dataframes corresponding to n languages

df_DE <- subset.data.frame(df_total_UKR, language == "DE")
write.table(df_DE, "data/debates_ukraine/divided_per_language\\UKR_DE.txt", sep = '^')

df_EN <- subset.data.frame(df_total_UKR, language == "EN")
write.table(df_EN, "data/debates_ukraine/divided_per_language\\UKR_EN.txt", sep = '^')

df_FR <- subset.data.frame(df_total_UKR, language == "FR")
write.table(df_FR, "data/debates_ukraine/divided_per_language\\UKR_FR.txt", sep = '^')

df_IT <- subset.data.frame(df_total_UKR, language == "IT")
write.table(df_IT, "data/debates_ukraine/divided_per_language\\UKR_IT.txt", sep = '^')

df_NL <- subset.data.frame(df_total_UKR, language == "NL")
write.table(df_NL, "data/debates_ukraine/divided_per_language\\UKR_NL.txt", sep = '^')

df_DA <- subset.data.frame(df_total_UKR, language == "DA")
write.table(df_DA, "data/debates_ukraine/divided_per_language\\UKR_DA.txt", sep = '^')

df_EL <- subset.data.frame(df_total_UKR, language == "EL")
write.table(df_EL, "data/debates_ukraine/divided_per_language\\UKR_EL.txt", sep = '^')

df_ES <- subset.data.frame(df_total_UKR, language == "ES")
write.table(df_ES, "data/debates_ukraine/divided_per_language\\UKR_ES.txt", sep = '^')

df_FI <- subset.data.frame(df_total_UKR, language == "FI")
write.table(df_FI, "data/debates_ukraine/divided_per_language\\UKR_FI.txt", sep = '^')

df_PT <- subset.data.frame(df_total_UKR, language == "PT")
write.table(df_PT, "data/debates_ukraine/divided_per_language\\UKR_PT.txt", sep = '^')

df_SU <- subset.data.frame(df_total_UKR, language == "SU")
write.table(df_SU, "data/debates_ukraine/divided_per_language\\UKR_SU.txt", sep = '^')

df_SV <- subset.data.frame(df_total_UKR, language == "SV")
write.table(df_SV, "data/debates_ukraine/divided_per_language\\UKR_SV.txt", sep = '^')

df_CS <- subset.data.frame(df_total_UKR, language == "CS")
write.table(df_CS, "data/debates_ukraine/divided_per_language\\UKR_CS.txt", sep = '^')

df_ET <- subset.data.frame(df_total_UKR, language == "ET")
write.table(df_ET, "data/debates_ukraine/divided_per_language\\UKR_ET.txt", sep = '^')

df_LV <- subset.data.frame(df_total_UKR, language == "LV")
write.table(df_LV, "data/debates_ukraine/divided_per_language\\UKR_LV.txt", sep = '^')

df_LT <- subset.data.frame(df_total_UKR, language == "LT")
write.table(df_LT, "data/debates_ukraine/divided_per_language\\UKR_LT.txt", sep = '^')

df_HU <- subset.data.frame(df_total_UKR, language == "HU")
write.table(df_HU, "data/debates_ukraine/divided_per_language\\UKR_HU.txt", sep = '^')

df_MT <- subset.data.frame(df_total_UKR, language == "MT")
write.table(df_MT, "data/debates_ukraine/divided_per_language\\UKR_MT.txt", sep = '^')

df_PL <- subset.data.frame(df_total_UKR, language == "PL")
write.table(df_PL, "data/debates_ukraine/divided_per_language\\UKR_PL.txt", sep = '^')

df_SK <- subset.data.frame(df_total_UKR, language == "SK")
write.table(df_SK, "data/debates_ukraine/divided_per_language\\UKR_SK.txt", sep = '^')

df_SL <- subset.data.frame(df_total_UKR, language == "SL")
write.table(df_SL, "data/debates_ukraine/divided_per_language\\UKR_SL.txt", sep = '^')

df_BG <- subset.data.frame(df_total_UKR, language == "BG")
write.table(df_BG, "data/debates_ukraine/divided_per_language\\UKR_BG.txt", sep = '^')

df_RO <- subset.data.frame(df_total_UKR, language == "RO")
write.table(df_RO, "data/debates_ukraine/divided_per_language\\UKR_RO.txt", sep = '^')

df_GA <- subset.data.frame(df_total_UKR, language == "GA")
write.table(df_GA, "data/debates_ukraine/divided_per_language\\UKR_GA.txt", sep = '^')

df_HR <- subset.data.frame(df_total_UKR, language == "HR")
write.table(df_HR, "data/debates_ukraine/divided_per_language\\UKR_HR.txt", sep = '^')


#translate all files by Google Translate and merge them again in a single data frame in English

#read each file as a R dataframe
df_BG_transl <- read.csv("data/debates_ukraine/divided_per_language/UKR_BG.txt",
                         sep="^",
                         header=T)

df_CS_transl <- read.csv("data/debates_ukraine/divided_per_language/UKR_CS.txt",
                         sep="^",
                         header=T)

df_DA_transl <- read.csv("data/debates_ukraine/divided_per_language/UKR_DA.txt",
                         sep="^",
                         header=T)

df_DE_transl <- read.csv("data/debates_ukraine/divided_per_language/UKR_DE.txt",
                         sep="^",
                         header=T)

df_EL_transl <- read.csv("data/debates_ukraine/divided_per_language/UKR_EL.txt",
                         sep="^",
                         header=T)

df_EN <- read.csv("data/debates_ukraine/divided_per_language/UKR_EN.txt",
                  sep = "^",
                  header=T)

df_ES_transl <- read.csv("data/debates_ukraine/divided_per_language/UKR_ES.txt",
                         sep="^",
                         header=T)

df_ET_transl <- read.csv("data/debates_ukraine/divided_per_language/UKR_ET.txt",
                         sep="^",
                         header=T)

df_FI_transl <- read.csv("data/debates_ukraine/divided_per_language/UKR_FI.txt",
                         sep="^",
                         header=T)

df_FR_transl <- read.csv("data/debates_ukraine/divided_per_language/UKR_FR.txt",
                         sep="^",
                         header=T)

df_GA_transl <- read.csv("data/debates_ukraine/divided_per_language/UKR_GA.txt",
                         sep="^",
                         header=T)

df_HR_transl <- read.csv("data/debates_ukraine/divided_per_language/UKR_HR.txt",
                         sep="^",
                         header=T)

df_HU_transl <- read.csv("data/debates_ukraine/divided_per_language/UKR_HU.txt",
                         sep="^",
                         header=T)

df_IT_transl <- read.csv("data/debates_ukraine/divided_per_language/UKR_IT.txt",
                         sep="^",
                         header=T)

df_LT_transl <- read.csv("data/debates_ukraine/divided_per_language/UKR_LT.txt",
                         sep="^",
                         header=T)

df_LV_transl <- read.csv("data/debates_ukraine/divided_per_language/UKR_LV.txt",
                         sep="^",
                         header=T)

df_NL_transl <- read.csv("data/debates_ukraine/divided_per_language/UKR_NL.txt",
                         sep="^",
                         header=T)

df_PL_transl <- read.csv("data/debates_ukraine/divided_per_language/UKR_PL.txt",
                         sep="^",
                         header=T)

df_PT_transl <- read.csv("data/debates_ukraine/divided_per_language/UKR_PT.txt",
                         sep="^",
                         header=T)

df_RO_transl <- read.csv("data/debates_ukraine/divided_per_language/UKR_RO.txt",
                         sep="^",
                         header=T)

df_SK_transl <- read.csv("data/debates_ukraine/divided_per_language/UKR_SK.txt",
                         sep="^",
                         header=T)

df_SL_transl <- read.csv("data/debates_ukraine/divided_per_language/UKR_SL.txt",
                         sep="^",
                         header=T)

df_SV_transl <- read.csv("data/debates_ukraine/divided_per_language/UKR_SV.txt",
                         sep="^",
                         header=T)

#merge all dataframes into one
df_total_transl_UKR <- rbind.data.frame(df_BG_transl,df_CS_transl,df_DA_transl,df_DE_transl,df_EL_transl,df_EN,df_ES_transl,df_ET_transl,df_FI_transl,df_FR_transl,df_GA_transl,df_HR_transl,df_HU_transl,df_IT_transl,df_LT_transl,df_LV_transl,df_NL_transl,df_PL_transl,df_PT_transl,df_RO_transl,df_SK_transl,df_SL_transl,df_SV_transl)

#write the single dataframe with all translations into one file
write.table(df_total_transl_UKR, "data/debates_ukraine\\df_total_transl_UKR.txt", sep = '^')


#I manually modified the txt file to fix some separators ^ that were printed in the wrong places

df_total_transl_UKR <- read.csv("data/debates_ukraine\\df_total_transl_UKR.txt", sep = '^')

parties <- unique(df_total_transl_UKR$party) 
parties

#I manually changed party names in txt to merge different names referring to the same parties

## KEEP ONLY DOCUMENTS CONTAINING "REFUGEE(S)", "ASYLUM", "(IM)MIGRANT(S)", "(IM)MIGRATION"
df_UKR_refugees <- filter(df_total_transl_UKR, grepl("refugee|refugees|asylum|migrant|migration|migrants|immigration|immigrant|immigrants",
                                                     text, ignore.case = T))

write.table(df_UKR_refugees, "data/debates_ukraine\\df_UKR_refugees.txt", sep = '^')

# Function to count words in a string
count_words <- function(text) {
  words <- unlist(strsplit(as.character(text), "\\s+"))
  return(length(words))
}

# Apply the count_words function to each cell in the 'text' column of the df_UKR_refugees
word_counts_UKR <- sapply(df_UKR_refugees$text, count_words)

# Calculate the average number of words
average_word_count_UKR <- mean(word_counts_UKR)

# Print the result
print(average_word_count_UKR)

#the interventions on Ukrainian refugees have 301 words on average

df_UKR_refugees <- read.csv("data/debates_ukraine/df_UKR_refugees.txt",sep="^",header=T)

#-------------------------------------------------
##SCRAPING Syria

setwd("data//debates_syria")

#scrape all html debates in the period 2014-2022 and save them in txt
#1
url <- "https://www.europarl.europa.eu/doceo/document/CRE-8-2014-07-15-ITM-012_EN.html"
source <- readLines(url, encoding = "UTF-8")
parsed_doc <- htmlParse(source, encoding = "UTF-8")
CRE_2014_07_15 <- xpathSApply(parsed_doc, path = '//*[contains(concat( " ", @class, " " ), concat( " ", "doc_box_header", " " ))]', xmlValue)
write_lines(CRE_2014_07_15, "data/debates_syria/CRE_2014_07_15.txt")

#2
url <- "https://www.europarl.europa.eu/doceo/document/CRE-8-2014-09-17-ITM-016_EN.html"
source <- readLines(url, encoding = "UTF-8")
parsed_doc <- htmlParse(source, encoding = "UTF-8")
CRE_2014_09_17 <- xpathSApply(parsed_doc, path = '//*[contains(concat( " ", @class, " " ), concat( " ", "doc_box_header", " " ))]', xmlValue)
write_lines(CRE_2014_09_17, "data/debates_syria/CRE_2014_09_17.txt")

#3
url <- "https://www.europarl.europa.eu/doceo/document/CRE-8-2014-09-18-ITM-012-03_EN.html"
source <- readLines(url, encoding = "UTF-8")
parsed_doc <- htmlParse(source, encoding = "UTF-8")
CRE_2014_09_18 <- xpathSApply(parsed_doc, path = '//*[contains(concat( " ", @class, " " ), concat( " ", "doc_box_header", " " ))]', xmlValue)
write_lines(CRE_2014_09_18, "data/debates_syria/CRE_2014_09_18.txt")

#4
url <- "https://www.europarl.europa.eu/doceo/document/CRE-8-2015-02-11-ITM-015_EN.html"
source <- readLines(url, encoding = "UTF-8")
parsed_doc <- htmlParse(source, encoding = "UTF-8")
CRE_2015_02_11 <- xpathSApply(parsed_doc, path = '//*[contains(concat( " ", @class, " " ), concat( " ", "doc_box_header", " " ))]', xmlValue)
write_lines(CRE_2015_02_11, "data/debates_syria/CRE_2015_02_11.txt")

#5
url <- "https://www.europarl.europa.eu/doceo/document/CRE-8-2015-06-11-ITM-002-01_EN.html"
source <- readLines(url, encoding = "UTF-8")
parsed_doc <- htmlParse(source, encoding = "UTF-8")
CRE_2015_06_11 <- xpathSApply(parsed_doc, path = '//*[contains(concat( " ", @class, " " ), concat( " ", "doc_box_header", " " ))]', xmlValue)
write_lines(CRE_2015_06_11, "data/debates_syria/CRE_2015_06_11.txt")

#6
url <- "https://www.europarl.europa.eu/doceo/document/CRE-8-2015-10-07-ITM-007_EN.html"
source <- readLines(url, encoding = "UTF-8")
parsed_doc <- htmlParse(source, encoding = "UTF-8")
CRE_2015_10_07 <- xpathSApply(parsed_doc, path = '//*[contains(concat( " ", @class, " " ), concat( " ", "doc_box_header", " " ))]', xmlValue)
write_lines(CRE_2015_10_07, "data/debates_syria/CRE_2015_10_07.txt")

#7
url <- "https://www.europarl.europa.eu/doceo/document/CRE-8-2015-11-11-ITM-019_EN.html"
source <- readLines(url, encoding = "UTF-8")
parsed_doc <- htmlParse(source, encoding = "UTF-8")
CRE_2015_11_11 <- xpathSApply(parsed_doc, path = '//*[contains(concat( " ", @class, " " ), concat( " ", "doc_box_header", " " ))]', xmlValue)
write_lines(CRE_2015_11_11, "data/debates_syria/CRE_2015_11_11.txt")

#8
url <- "https://www.europarl.europa.eu/doceo/document/CRE-8-2016-01-19-ITM-013_EN.html"
source <- readLines(url, encoding = "UTF-8")
parsed_doc <- htmlParse(source, encoding = "UTF-8")
CRE_2016_01_19 <- xpathSApply(parsed_doc, path = '//*[contains(concat( " ", @class, " " ), concat( " ", "doc_box_header", " " ))]', xmlValue)
write_lines(CRE_2016_01_19, "data/debates_syria/CRE_2016_01_19.txt")

#9
url <- "https://www.europarl.europa.eu/doceo/document/CRE-8-2016-03-08-ITM-010_EN.html"
source <- readLines(url, encoding = "UTF-8")
parsed_doc <- htmlParse(source, encoding = "UTF-8")
CRE_2016_03_08 <- xpathSApply(parsed_doc, path = '//*[contains(concat( " ", @class, " " ), concat( " ", "doc_box_header", " " ))]', xmlValue)
write_lines(CRE_2016_03_08, "data/debates_syria/CRE_2016_03_08.txt")

#10
url <- "https://www.europarl.europa.eu/doceo/document/CRE-8-2015-04-30-ITM-016-02_EN.html"
source <- readLines(url, encoding = "UTF-8")
parsed_doc <- htmlParse(source, encoding = "UTF-8")
CRE_2015_04_30 <- xpathSApply(parsed_doc, path = '//*[contains(concat( " ", @class, " " ), concat( " ", "doc_box_header", " " ))]', xmlValue)
write_lines(CRE_2015_04_30, "data/debates_syria/CRE_2015_04_30.txt")

#11
url <- "https://www.europarl.europa.eu/doceo/document/CRE-8-2015-02-12-ITM-006-02_EN.html"
source <- readLines(url, encoding = "UTF-8")
parsed_doc <- htmlParse(source, encoding = "UTF-8")
CRE_2015_02_12 <- xpathSApply(parsed_doc, path = '//*[contains(concat( " ", @class, " " ), concat( " ", "doc_box_header", " " ))]', xmlValue)
write_lines(CRE_2015_02_12, "data/debates_syria/CRE_2015_02_12.txt")

#12
url <- "https://www.europarl.europa.eu/doceo/document/CRE-8-2016-10-05-ITM-014_EN.html"
source <- readLines(url, encoding = "UTF-8")
parsed_doc <- htmlParse(source, encoding = "UTF-8")
CRE_2016_10_05 <- xpathSApply(parsed_doc, path = '//*[contains(concat( " ", @class, " " ), concat( " ", "doc_box_header", " " ))]', xmlValue)
write_lines(CRE_2016_10_05, "data/debates_syria/CRE_2016_10_05.txt")

#13
url <- "https://www.europarl.europa.eu/doceo/document/CRE-8-2016-10-05-ITM-012_EN.html"
source <- readLines(url, encoding = "UTF-8")
parsed_doc <- htmlParse(source, encoding = "UTF-8")
CRE_2016_10_05_1 <- xpathSApply(parsed_doc, path = '//*[contains(concat( " ", @class, " " ), concat( " ", "doc_box_header", " " ))]', xmlValue)
write_lines(CRE_2016_10_05_1, "data/debates_syria/CRE_2016_10_05_1.txt")

#14
url <- "https://www.europarl.europa.eu/doceo/document/CRE-8-2016-10-06-ITM-006-02_EN.html"
source <- readLines(url, encoding = "UTF-8")
parsed_doc <- htmlParse(source, encoding = "UTF-8")
CRE_2016_10_06 <- xpathSApply(parsed_doc, path = '//*[contains(concat( " ", @class, " " ), concat( " ", "doc_box_header", " " ))]', xmlValue)
write_lines(CRE_2016_10_06, "data/debates_syria/CRE_2016_10_06.txt")

#15
url <- "https://www.europarl.europa.eu/doceo/document/CRE-8-2016-11-22-ITM-009_EN.html"
source <- readLines(url, encoding = "UTF-8")
parsed_doc <- htmlParse(source, encoding = "UTF-8")
CRE_2016_11_22 <- xpathSApply(parsed_doc, path = '//*[contains(concat( " ", @class, " " ), concat( " ", "doc_box_header", " " ))]', xmlValue)
write_lines(CRE_2016_11_22, "data/debates_syria/CRE_2016_11_22.txt")

#16
url <- "https://www.europarl.europa.eu/doceo/document/CRE-8-2016-11-24-ITM-009-03_EN.html"
source <- readLines(url, encoding = "UTF-8")
parsed_doc <- htmlParse(source, encoding = "UTF-8")
CRE_2016_11_24 <- xpathSApply(parsed_doc, path = '//*[contains(concat( " ", @class, " " ), concat( " ", "doc_box_header", " " ))]', xmlValue)
write_lines(CRE_2016_11_24, "data/debates_syria/CRE_2016_11_24.txt")

#17
url <- "https://www.europarl.europa.eu/doceo/document/CRE-8-2017-05-16-ITM-010_EN.html"
source <- readLines(url, encoding = "UTF-8")
parsed_doc <- htmlParse(source, encoding = "UTF-8")
CRE_2017_05_16 <- xpathSApply(parsed_doc, path = '//*[contains(concat( " ", @class, " " ), concat( " ", "doc_box_header", " " ))]', xmlValue)
write_lines(CRE_2017_05_16, "data/debates_syria/CRE_2017_05_16.txt")

#18
url <- "https://www.europarl.europa.eu/doceo/document/CRE-8-2017-05-18-ITM-012-07_EN.html"
source <- readLines(url, encoding = "UTF-8")
parsed_doc <- htmlParse(source, encoding = "UTF-8")
CRE_2017_05_18 <- xpathSApply(parsed_doc, path = '//*[contains(concat( " ", @class, " " ), concat( " ", "doc_box_header", " " ))]', xmlValue)
write_lines(CRE_2017_05_18, "data/debates_syria/CRE_2017_05_18.txt")

#19
url <- "https://www.europarl.europa.eu/doceo/document/CRE-8-2018-02-06-ITM-015_EN.html"
source <- readLines(url, encoding = "UTF-8")
parsed_doc <- htmlParse(source, encoding = "UTF-8")
CRE_2018_02_06 <- xpathSApply(parsed_doc, path = '//*[contains(concat( " ", @class, " " ), concat( " ", "doc_box_header", " " ))]', xmlValue)
write_lines(CRE_2018_02_06, "data/debates_syria/CRE_2018_02_06.txt")

#20
url <- "https://www.europarl.europa.eu/doceo/document/CRE-8-2018-02-28-ITM-017_EN.html"
source <- readLines(url, encoding = "UTF-8")
parsed_doc <- htmlParse(source, encoding = "UTF-8")
CRE_2018_02_28 <- xpathSApply(parsed_doc, path = '//*[contains(concat( " ", @class, " " ), concat( " ", "doc_box_header", " " ))]', xmlValue)
write_lines(CRE_2018_02_28, "data/debates_syria/CRE_2018_02_28.txt")

#21
url <- "https://www.europarl.europa.eu/doceo/document/CRE-8-2018-03-13-ITM-017_EN.html"
source <- readLines(url, encoding = "UTF-8")
parsed_doc <- htmlParse(source, encoding = "UTF-8")
CRE_2018_03_13 <- xpathSApply(parsed_doc, path = '//*[contains(concat( " ", @class, " " ), concat( " ", "doc_box_header", " " ))]', xmlValue)
write_lines(CRE_2018_03_13, "data/debates_syria/CRE_2018_03_13.txt")

#22
url <- "https://www.europarl.europa.eu/doceo/document/CRE-8-2018-03-15-ITM-012-06_EN.html"
source <- readLines(url, encoding = "UTF-8")
parsed_doc <- htmlParse(source, encoding = "UTF-8")
CRE_2018_03_15 <- xpathSApply(parsed_doc, path = '//*[contains(concat( " ", @class, " " ), concat( " ", "doc_box_header", " " ))]', xmlValue)
write_lines(CRE_2018_03_15, "data/debates_syria/CRE_2018_03_15.txt")

#23
url <- "https://www.europarl.europa.eu/doceo/document/CRE-8-2018-04-17-ITM-013_EN.html"
source <- readLines(url, encoding = "UTF-8")
parsed_doc <- htmlParse(source, encoding = "UTF-8")
CRE_2018_04_17 <- xpathSApply(parsed_doc, path = '//*[contains(concat( " ", @class, " " ), concat( " ", "doc_box_header", " " ))]', xmlValue)
write_lines(CRE_2018_04_17, "data/debates_syria/CRE_2018_04_17.txt")

#24
url <- "https://www.europarl.europa.eu/doceo/document/CRE-8-2019-02-12-ITM-015_EN.html"
source <- readLines(url, encoding = "UTF-8")
parsed_doc <- htmlParse(source, encoding = "UTF-8")
CRE_2019_02_12 <- xpathSApply(parsed_doc, path = '//*[contains(concat( " ", @class, " " ), concat( " ", "doc_box_header", " " ))]', xmlValue)
write_lines(CRE_2019_02_12, "data/debates_syria/CRE_2019_02_12.txt")

#25
url <- "https://www.europarl.europa.eu/doceo/document/CRE-9-2019-10-09-ITM-018_EN.html"
source <- readLines(url, encoding = "UTF-8")
parsed_doc <- htmlParse(source, encoding = "UTF-8")
CRE_2019_10_09 <- xpathSApply(parsed_doc, path = '//*[contains(concat( " ", @class, " " ), concat( " ", "doc_box_header", " " ))]', xmlValue)
write_lines(CRE_2019_10_09, "data/debates_syria/CRE_2019_10_09.txt")

#26
url <- "https://www.europarl.europa.eu/doceo/document/CRE-9-2019-10-23-ITM-007_EN.html"
source <- readLines(url, encoding = "UTF-8")
parsed_doc <- htmlParse(source, encoding = "UTF-8")
CRE_2019_10_23 <- xpathSApply(parsed_doc, path = '//*[contains(concat( " ", @class, " " ), concat( " ", "doc_box_header", " " ))]', xmlValue)
write_lines(CRE_2019_10_23, "data/debates_syria/CRE_2019_10_23.txt")

#27
url <- "https://www.europarl.europa.eu/doceo/document/CRE-9-2019-10-24-ITM-010-03_EN.html"
source <- readLines(url, encoding = "UTF-8")
parsed_doc <- htmlParse(source, encoding = "UTF-8")
CRE_2019_10_24 <- xpathSApply(parsed_doc, path = '//*[contains(concat( " ", @class, " " ), concat( " ", "doc_box_header", " " ))]', xmlValue)
write_lines(CRE_2019_10_24, "data/debates_syria/CRE_2019_10_24.txt")

#28
url <- "https://www.europarl.europa.eu/doceo/document/CRE-9-2020-02-11-ITM-011_EN.html"
source <- readLines(url, encoding = "UTF-8")
parsed_doc <- htmlParse(source, encoding = "UTF-8")
CRE_2020_02_11 <- xpathSApply(parsed_doc, path = '//*[contains(concat( " ", @class, " " ), concat( " ", "doc_box_header", " " ))]', xmlValue)
write_lines(CRE_2020_02_11, "data/debates_syria/CRE_2020_02_11.txt")

#29
url <- "https://www.europarl.europa.eu/doceo/document/CRE-9-2022-12-13-ITM-018_EN.html"
source <- readLines(url, encoding = "UTF-8")
parsed_doc <- htmlParse(source, encoding = "UTF-8")
CRE_2022_12_13 <- xpathSApply(parsed_doc, path = '//*[contains(concat( " ", @class, " " ), concat( " ", "doc_box_header", " " ))]', xmlValue)
write_lines(CRE_2022_12_13, "data/debates_syria/CRE_2022_12_13.txt")

#---------------------------------------------

#translate all documents in English via Google Translate
#add separator ^ manually in the txt files
#manually merge all documents into a single txt file named "df_total_transl_SYR"

#import all txt files into a single object
df_total_SYR <- as.data.frame(read.csv("data/debates_syria/translated/df_total_transl_SYR.txt",
                                       sep = "^",
                                       header = F))

#add column names
df_total_SYR <- fix(df_total_SYR)

#delete empty rows in the "party" column
df_total_SYR <- df_total_SYR[!(df_total_SYR$party == "" | is.na(df_total_SYR$party)), ]

# Add an "id" column with unique ids for each row
df_SYR_refugees$id <- row.names(df_SYR_refugees)

write.table(df_total_SYR, sep = "^", "data/debates_syria/df_total_transl_SYR.txt")

#Change party names in txt to merge different names referring to the same parties and import it in R
df_total_SYR <- as.data.frame(read.csv("data/debates_syria/df_total_transl_SYR.txt",
                                       sep = "^",
                                       header = T))

#adjust party names
parties2 <- unique(df_total_SYR$party) 
parties2

## KEEP ONLY DOCUMENTS CONTAINING "REFUGEE(S)", "ASYLUM", "(IM)MIGRANT(S)", "(IM)MIGRATION"
df_SYR_refugees <- filter(df_total_SYR, grepl("refugee|refugees|asylum|migrant|migrants|migration|immigration|immigrant|immigrants", 
                                              text, ignore.case = T))

write.table(df_SYR_refugees, "data/debates_syria/df_SYR_refugees.txt", sep = '^')

# Apply the count_words function to each cell in the 'text' column of the df_SYR_refugees
word_counts_SYR <- sapply(df_SYR_refugees$text, count_words)

# Calculate the average number of words
average_word_count_SYR <- mean(word_counts_SYR)

# Print the result
print(average_word_count_SYR)

#the statements on Syrian refugees have 236 words on average

df_SYR_refugees <- read.csv("data/debates_syria/df_SYR_refugees.txt",sep="^",header=T)

#-------------------------------------------------------

#both data frames are reduced to data frames including only a two columns: "text" and "id"
#the old two data frames with all metadata are kept under the names "df_UKR_refugees" and "df_SYR_refugees" and included to the appendix material, 
#as they might be useful for further research investigating differences among political groups' or individual speakers' discourses

df_UKR <- subset(df_UKR_refugees, select = -c(party, name, language))
df_SYR <- subset(df_SYR_refugees, select = -c(party, name))

# Write the merged text to a text file
write.table(df_UKR, sep = "^", "data/debates_ukraine\\df_UKR.txt")
write.table(df_SYR, sep = "^", "data/debates_syria\\df_SYR.txt")

#-------------------------------------------------------

##TOKENIZATION AND PREPROCESSING - Ukraine

df_UKR <- read.csv("data/debates_ukraine\\df_UKR.txt",sep="^",header=T)

# Convert to lowercase
df_UKR$text <- tolower(df_UKR$text)

#remove the possessive "'s" now, before removing punctuation, to avoid the blend of the possessive to the referring word
df_UKR$text <- gsub("'s", " ", df_UKR$text)

# Remove punctuation, numbers
df_UKR$text <- tm::removePunctuation(df_UKR$text)
df_UKR$text <- tm::removeNumbers(df_UKR$text)

# Remove words with less than 3 characters from the text
pattern <- "\\b\\w{1,2}\\b"  # Matches words with 1 or 2 characters
df_UKR$text <- gsub(pattern, " ", df_UKR$text)

# lemmatization
df_UKR$text <- textstem::lemmatize_strings(df_UKR$text, dictionary = lexicon::hash_lemmas)

#remove stopwords
df_UKR$text <- tm::removeWords(df_UKR$text, stopwords("en"))

#REMOVE CUSTOM WORDS
  #remove long hyphens "—" and single inverted comma "’" as they were not removed with the rest of the punctuation
df_UKR$text <- gsub("–", " ", df_UKR$text)
df_UKR$text <- gsub("’", " ", df_UKR$text)

  #remove modal verbs
df_UKR$text <- gsub(" can ", " ", df_UKR$text)
df_UKR$text <- gsub(" may ", " ", df_UKR$text)
df_UKR$text <- gsub(" must ", " ", df_UKR$text)
df_UKR$text <- gsub(" shall ", " ", df_UKR$text)
df_UKR$text <- gsub(" have to ", " ", df_UKR$text)
df_UKR$text <- gsub(" need ", " ", df_UKR$text)
df_UKR$text <- gsub(" will ", " ", df_UKR$text)

  #COME GIA FATTO NELL'ARTICOLO: 
  #remove recurring words referring to speaker’s qualifications and roles, and forms of address and courtesy
df_UKR$text <- gsub(" mr ", " ", df_UKR$text)
df_UKR$text <- gsub(" mrs ", " ", df_UKR$text)
df_UKR$text <- gsub(" mister ", " ", df_UKR$text)
df_UKR$text <- gsub(" madam ", " ", df_UKR$text)
df_UKR$text <- gsub(" madame ", " ", df_UKR$text)
df_UKR$text <- gsub(" dear ", " ", df_UKR$text)
df_UKR$text <- gsub(" honorable ", " ", df_UKR$text)
df_UKR$text <- gsub(" chairman ", " ", df_UKR$text)
df_UKR$text <- gsub(" president ", " ", df_UKR$text)
df_UKR$text <- gsub("vice president", " ", df_UKR$text)
df_UKR$text <- gsub(" commissioner ", " ", df_UKR$text)
df_UKR$text <- gsub("ladies and gentlemen", " ", df_UKR$text)
df_UKR$text <- gsub("on behalf of", " ", df_UKR$text)
df_UKR$text <- gsub(" rapporteur ", " ", df_UKR$text)
df_UKR$text <- gsub("member of the commission", " ", df_UKR$text)
df_UKR$text <- gsub("high representative of the union for foreign affairs and security policy", " ", df_UKR$text)

  #remove political groups' names
df_UKR$text <- gsub("ppe group", " ", df_UKR$text)
df_UKR$text <- gsub(" ppe ", " ", df_UKR$text)
df_UKR$text <- gsub("ni group", " ", df_UKR$text)
df_UKR$text <- gsub(" ni ", " ", df_UKR$text)
df_UKR$text <- gsub("the left group", " ", df_UKR$text)
df_UKR$text <- gsub("the left", " ", df_UKR$text)
df_UKR$text <- gsub("s&d group", " ", df_UKR$text)
df_UKR$text <- gsub("s&d", " ", df_UKR$text)
df_UKR$text <- gsub("verts/ale group", " ", df_UKR$text)
df_UKR$text <- gsub("verts/ale", " ", df_UKR$text)
df_UKR$text <- gsub("ecr group", " ", df_UKR$text)
df_UKR$text <- gsub(" ecr ", " ", df_UKR$text)
df_UKR$text <- gsub("renew group", " ", df_UKR$text)
df_UKR$text <- gsub(" renew  ", " ", df_UKR$text)

#a doublecheck of the dataset shows the presence of "refugees" attached to other words. 
#I procede to change change it to "refugee", by adding a space before and after the word
df_UKR$text <- gsub("refugees", " refugee ", df_UKR$text)

write.table(df_UKR, sep = "^", "data/debates_ukraine\\df_UKR.txt", row.names = FALSE)

#----------------------------------------------

##TOKENIZATION AND PREPROCESSING - SYRIA

df_SYR <- read.csv("data/debates_syria\\df_SYR.txt",sep="^",header=T)

# Convert to lowercase
df_SYR$text <- tolower(df_SYR$text)

#remove the possessive "'s" now, before removing punctuation, to avoid the blend of the possessive to the referring word
df_SYR$text <- gsub("'s", " ", df_SYR$text)

# Remove punctuation, numbers
df_SYR$text <- tm::removePunctuation(df_SYR$text)
df_SYR$text <- tm::removeNumbers(df_SYR$text)

# Remove words with less than 3 characters from the text
pattern <- "\\b\\w{1,2}\\b"  # Matches words with 1 or 2 characters
df_SYR$text <- gsub(pattern, " ", df_SYR$text)

# lemmatization
df_SYR$text <- textstem::lemmatize_strings(df_SYR$text, dictionary = lexicon::hash_lemmas)

# remove stopwords
df_SYR$text <- tm::removeWords(df_SYR$text, stopwords("en"))

#REMOVE CUSTOM WORDS
#remove long hyphens "—" and single inverted comma "’" as they were not removed with the rest of the punctuation
df_SYR$text <- gsub("–", " ", df_SYR$text)
df_SYR$text <- gsub("’", " ", df_SYR$text)

#remove modal verbs
df_SYR$text <- gsub(" can ", " ", df_SYR$text)
df_SYR$text <- gsub(" may ", " ", df_SYR$text)
df_SYR$text <- gsub(" must ", " ", df_SYR$text)
df_SYR$text <- gsub(" shall ", " ", df_SYR$text)
df_SYR$text <- gsub(" have to ", " ", df_SYR$text)
df_SYR$text <- gsub(" need ", " ", df_SYR$text)
df_SYR$text <- gsub(" will ", " ", df_SYR$text)

#COME GIA FATTO NELL'ARTICOLO: 
#remove recurring words referring to speaker’s qualifications and roles, and forms of address and courtesy
df_SYR$text <- gsub(" mr ", " ", df_SYR$text)
df_SYR$text <- gsub(" mrs ", " ", df_SYR$text)
df_SYR$text <- gsub(" mister ", " ", df_SYR$text)
df_SYR$text <- gsub("madam ", " ", df_SYR$text)
df_SYR$text <- gsub(" madame ", " ", df_SYR$text)
df_SYR$text <- gsub(" dear ", " ", df_SYR$text)
df_SYR$text <- gsub(" honorable ", " ", df_SYR$text)
df_SYR$text <- gsub(" chairman ", " ", df_SYR$text)
df_SYR$text <- gsub(" president ", " ", df_SYR$text)
df_SYR$text <- gsub("vice president", " ", df_SYR$text)
df_SYR$text <- gsub(" commissioner ", " ", df_SYR$text)
df_SYR$text <- gsub("ladies and gentlemen", " ", df_SYR$text)
df_SYR$text <- gsub("on behalf of", " ", df_SYR$text)
df_SYR$text <- gsub(" rapporteur ", " ", df_SYR$text)
df_SYR$text <- gsub("member of the commission", " ", df_SYR$text)
df_SYR$text <- gsub("high representative of the union for foreign affairs and security policy", " ", df_SYR$text)

#remove political groups' names
df_SYR$text <- gsub("ppe group", " ", df_SYR$text)
df_SYR$text <- gsub(" ppe ", " ", df_SYR$text)
df_SYR$text <- gsub("ni group", " ", df_SYR$text)
df_SYR$text <- gsub(" ni ", " ", df_SYR$text)
df_SYR$text <- gsub("the left group", " ", df_SYR$text)
df_SYR$text <- gsub("the left", " ", df_SYR$text)
df_SYR$text <- gsub("s&d group", " ", df_SYR$text)
df_SYR$text <- gsub("s&d", " ", df_SYR$text)
df_SYR$text <- gsub("verts/ale group", " ", df_SYR$text)
df_SYR$text <- gsub("verts/ale", " ", df_SYR$text)
df_SYR$text <- gsub("ecr group", " ", df_SYR$text)
df_SYR$text <- gsub(" ecr ", " ", df_SYR$text)
df_SYR$text <- gsub("renew group", " ", df_SYR$text)
df_SYR$text <- gsub(" renew  ", " ", df_SYR$text)
df_SYR$text <- gsub(" alde  ", " ", df_SYR$text)
df_SYR$text <- gsub("efdd", " ", df_SYR$text)
df_SYR$text <- gsub("enf group", " ", df_SYR$text)
df_SYR$text <- gsub(" enf  ", " ", df_SYR$text)

#a doublecheck of the dataset shows a consistent presence of "refugees" and "jihadists"
#I procede to manually lemmatize them
df_SYR$text <- gsub("refugees", " refugee ", df_SYR$text)
df_SYR$text <- gsub("jihadists", " jihadist ", df_SYR$text)

write.table(df_SYR, sep = "^", "data/debates_syria\\df_SYR.txt",  row.names = FALSE)

#----------------------------------------------

##DESCRIPTIVE STATISTICS
##Ukraine

#find the 50 most frequent words
# Split the processed text into individual words
words_UKR <- unlist(strsplit(df_UKR$text, "\\s+"))

# Count occurrences of each word
word_counts_UKR <- table(words_UKR)

# Convert the result to a data frame for better handling
word_counts_df_UKR <- as.data.frame(word_counts_UKR)

# Order the data frame by frequency in descending order
word_counts_df_UKR <- word_counts_df_UKR[order(-word_counts_df_UKR$Freq), ]

# Print the 50 most frequent words
cat("50 Most Frequent Words in processed_df_UKR$text:\n")
print(head(word_counts_df_UKR, 50))

# Select the top 50 most frequent words
top_50_words_UKR <- head(word_counts_df_UKR, 50)

# Create a ggplot bar chart
ggplot(top_50_words_UKR, aes(x = reorder(words_UKR, -Freq), y = Freq)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "50 Most Frequent Words - Ukraine", x = "Word", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#create  wordcloud
wordcloud::wordcloud(words = top_50_words_UKR$words_UKR, freq = top_50_words_UKR$Freq, max.words = 50, scale = c(2, 0.4), random.order = FALSE, colors = "black")
title(main = "Top 50 Words - Ukraine",  cex.main = 1, adj = 0.5, line = 3)

#------------------------------------

##BIGRAM ANALYSIS

# Tokenize data without requiring any particular package
ngram_token <-  function(x) unlist(lapply(ngrams(words(x), 2), paste, collapse=" "), use.names=FALSE)

corpus_UKR_ref <- VCorpus(VectorSource(df_UKR$text))
funs <- list(stripWhitespace,
             removePunctuation,
             function(x) removeWords(x, stopwords("english")),
             content_transformer(tolower))
corpus_UKR_ref <- tm_map(corpus_UKR_ref, FUN = tm_reduce, tmFuns = funs)

# Pass into TDM control argument
tdm <- TermDocumentMatrix(corpus_UKR_ref, control = list(tokenize = ngram_token))
freq <- rowSums(as.matrix(tdm))
tdm_freq <- data.frame(term = names(freq), occurrences = freq)
tdm_freq

UKRsubset <- tdm_freq[grep("refugee|asylum|migration|immigration|migrant|migrants|immigrant|immigrants", tdm_freq$term), ]

#plot the 20 most frequent bigrams
# Sort the data frame by occurrences in descending order
UKRsubset <- UKRsubset[order(-UKRsubset$occurrences), ]

# Plot the first 20 occurrences
ggplot(head(UKRsubset, 20), aes(x = reorder(term, occurrences), y = occurrences)) +
  geom_bar(stat = "identity", fill = "gray") +
  labs(x = "Bigram", y = "Occurrences") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Migration-related bigrams - Ukraine")


#------------------------------------------

##DESCRIPTIVE STATISTICS
##SYRIA

#find the 50 most frequent words
# Split the processed text into individual words
words_SYR <- unlist(strsplit(df_SYR$text, "\\s+"))

# Count occurrences of each word
word_counts_SYR <- table(words_SYR)

# Convert the result to a data frame for better handling
word_counts_df_SYR <- as.data.frame(word_counts_SYR)

# Order the data frame by frequency in descending order
word_counts_df_SYR <- word_counts_df_SYR[order(-word_counts_df_SYR$Freq), ]

# Print the 50 most frequent words
cat("50 Most Frequent Words in processed_df_SYR$text:\n")
print(head(word_counts_df_SYR, 50))

# Select the top 50 most frequent words
top_50_words_SYR <- head(word_counts_df_SYR, 50)

# Create a ggplot bar chart
ggplot(top_50_words_SYR, aes(x = reorder(words_SYR, -Freq), y = Freq)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "50 Most Frequent Words - Syria", x = "Word", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#create  wordcloud
wordcloud::wordcloud(words = top_50_words_SYR$words_SYR, freq = top_50_words_SYR$Freq, max.words = 50, scale = c(2, 0.4), random.order = FALSE, colors = "black")
title(main = "Top 50 Words - Syria",  cex.main = 1, adj = 0.5, line = 3)

#------------------------------------

##BIGRAM ANALYSIS Syria

# Tokenize data without requiring any particular package
corpus_SYR_ref <- VCorpus(VectorSource(df_SYR$text))
funs2 <- list(stripWhitespace,
             removePunctuation,
             function(x) removeWords(x, stopwords("english")),
             content_transformer(tolower))
corpus_SYR_ref <- tm_map(corpus_SYR_ref, FUN = tm_reduce, tmFuns = funs2)

# Pass into TDM control argument
tdm2 <- TermDocumentMatrix(corpus_SYR_ref, control = list(tokenize = ngram_token))
freq2 <- rowSums(as.matrix(tdm2))
tdm_freq2 <- data.frame(term = names(freq2), occurrences = freq2)
tdm_freq2

SYRsubset <- tdm_freq2[grep("refugee|asylum|migration|immigration|migrant|migrants|immigrant|immigrants", tdm_freq2$term), ]

#plot the 20 most frequent bigrams
# Sort the data frame by occurrences in descending order
SYRsubset <- SYRsubset[order(-SYRsubset$occurrences), ]

# Plot the first 20 occurrences
ggplot(head(SYRsubset, 20), aes(x = reorder(term, occurrences), y = occurrences)) +
  geom_bar(stat = "identity", fill = "gray") +
  labs(x = "Bigram", y = "Occurrences") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Migration-related bigrams - Syria")

#------------------------------------------

#TF-IDF: words differentiating parliamentary discourses on Syria and Ukraine

# Merge text from the 'text' column into a single object
text_SYR <- paste(df_SYR$text, collapse = " ")
text_UKR <- paste(df_UKR$text, collapse = " ")

# Create data frames
df_SYR_tfidf <- data.frame(text = text_SYR)
df_UKR_tfidf <- data.frame(text = text_UKR)

# Add a column indicating the source document
df_SYR_tfidf$document <- "Syria"
df_UKR_tfidf$document <- "Ukraine"

# Tokenize the text into words
words_UKR <- unlist(tokenize_words(df_UKR_tfidf$text))
words_SYR <- unlist(tokenize_words(df_SYR_tfidf$text))

# Check if each word is in the English dictionary
english_words_UKR <- words_UKR[sapply(words_UKR, function(x) hunspell_check(x, dict=dictionary("en_US")))]
english_words_SYR <- words_SYR[sapply(words_SYR, function(x) hunspell_check(x, dict=dictionary("en_US")))]

# Update the dataframe with English words only
df_UKR_tfidf$text <- paste(english_words_UKR, collapse = " ")
df_SYR_tfidf$text <- paste(english_words_SYR, collapse = " ")

# Merge the data frames
merged_df <- rbind(df_SYR_tfidf, df_UKR_tfidf)

#create corpus object out of the merged_df's "text" column
corpus_tfidf <- quanteda::corpus(merged_df, text_field = "text")

# Tokenize the corpus column of merged_df
tokens_tfidf <- quanteda::tokens(corpus_tfidf) 

#create new dfm with tokens
dfm_tfidf <- quanteda::dfm(tokens_tfidf)

#Group texts by case (Ukraine vs Syria) and build a new dfm
dfm_tfidf_grouped <- quanteda::dfm_group(dfm_tfidf, groups = document)

#Weight a dfm by term frequency-inverse document frequency (tf-idf)
weight_tfidf <- quanteda::dfm_tfidf(dfm_tfidf_grouped) 
head(weight_tfidf,20)

#run tf-idf analysis showing the 20 most exclusive words
stat_tfidf <- quanteda.textstats::textstat_frequency(weight_tfidf, n = 20, groups=document, ties_method = "random", force=TRUE)

#plot the results
stat_tfidf%>%
  group_by(group) %>%
  ungroup() %>%
  mutate(feature = as.factor(feature),
         group=group,
         name = tidytext::reorder_within(feature, frequency, group)) %>%
  ggplot(aes(name, frequency)) +
  geom_point() +
  facet_wrap(~ group, scales = "free_y") +
  coord_flip() +
  tidytext::scale_x_reordered() +
  labs(y = "Term Frequency-Inverse Document Frequency", x = "Words",
       title = "",
       subtitle = "") +
  ggtitle("Most exclusive words (Syria and Ukraine)")+
  theme(text = element_text(size=12)) +
  theme(axis.text=element_text(size=9)) +
  theme(strip.text = element_text(size=12))

#the terms "northeast" and "polish" are manually removed from corpus_tfidf, as they were not found by the hunspell_check function 
corpus_tfidf <- gsub("polish", " ", corpus_tfidf)
corpus_tfidf <- gsub("northeast", " ", corpus_tfidf)
#rerun tf-idf analysis from the "# Tokenize the corpus column of merged_df" line until no named entities to remove appear

#----------------------------------------------------

#STM

df_UKR_refugees <- read.csv("data/debates_ukraine/df_UKR_refugees.txt",sep="^",header=T)
df_UKR_refugees <- subset(df_UKR_refugees, select = -c(language, name, party))

df_SYR_refugees <- read.csv("data/debates_syria/df_SYR_refugees.txt",sep="^",header=T)
df_SYR_refugees <- subset(df_SYR_refugees, select = -c(name, party))

df_UKR <- read.csv("data/debates_ukraine\\df_UKR.txt",sep="^",header=T)
df_SYR <- read.csv("data/debates_syria\\df_SYR.txt",sep="^",header=T)

# Add a column "case" with values "UKR"
df_UKR_refugees$case <- "UKR"
df_SYR_refugees$case <- "SYR"
df_UKR$case <- "UKR"
df_SYR$case <- "SYR"

# Merge the two data frames
merged_df_refugees_STM <- rbind(df_UKR_refugees, df_SYR_refugees)
merged_df_STM <- rbind(df_UKR, df_SYR)

#read and preprocess data (without stemming, as the text has already been lemmatised)
processed <- textProcessor(merged_df_STM$text, metadata = merged_df_STM, stem = FALSE) 
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <- out$meta

#Semantic coherence - Exclusivity
#a min number of 10 and a max of 50 topics is set
# search for optimal k number of topics
#Semantic coherence - Exclusivity: a k that holds high exclusivity and coherence
#a min number of 10 and a max of 50 topics is set
storage_10_50 <- searchK(out$documents, out$vocab, K = c(10:50), init.type = "Spectral",
                         prevalence =~ case, data = out$meta)
storage_10_50$results
store_k <- storage_10_50$results
str(store_k)
store_k$K <- unlist(store_k$K)
store_k$K
store_k$exclus <- unlist(store_k$exclus)
store_k$semcoh <- unlist(store_k$semcoh)
store_k$heldout <- unlist(store_k$heldout)
store_k$residual <- unlist(store_k$residual)
store_k$bound <- unlist(store_k$bound)
store_k$lbound <- unlist(store_k$lbound)
store_k$em.its <- unlist(store_k$em.its)  
str(store_k)

write.csv(x = store_k, file = "data\\semantic_coherence_stm.csv")

topics_metrics <-store_k[,1:3]

#A trade-off between semantic coherence and exclusivity of topic models
lo <- loess(topics_metrics$exclus~topics_metrics$semcoh)
jpeg("data\\k_number.jpeg", width = 16, height = 8, units = 'in', res = 500)
plot(x = topics_metrics$semcoh, y = topics_metrics$exclus,type = "p", ylab = "Exclusivity", 
     xlab = "Semantic Coherence")
#main = "Trade-off between Semantic Coherence and Exclusivity")
xl <- seq(min(topics_metrics$semcoh),max(topics_metrics$semcoh), (max(topics_metrics$semcoh) - min(topics_metrics$semcoh))/1000)
lines(xl, predict(lo,xl), col='red', lwd=2)
text(topics_metrics$exclus~topics_metrics$semcoh, labels=topics_metrics$K, data=topics_metrics, cex=0.8, font=2.9)
dev.off()

#21k, 22k, 28k, 29k and 36k seem to be good choices. I run STM analysis for all of them to see when convergence is reached 
#Convergence assures researchers that the results of the STM analysis are reliable and can be interpreted with confidence.

#run STM for k=21
modelPrevFit_21 <- stm(documents = out$documents, vocab = out$vocab,
                       K = 21, prevalence = ~ case, 
                       max.em.its = 75, data = out$meta,
                       init.type = "Spectral", seed = 323253221)

# Plot the STM showing highest probability and FREX (frequent and exclusive) words
plot(modelPrevFit_21, xlim = c(0, 0.2), type = "summary", n=10, family = "sans", 
     text.cex = 1, labeltype = "prob", main = "Top 10 high probabililty words - 21k")

plot(modelPrevFit_21, xlim = c(0, 0.2), type = "summary", n=10, family = "sans", 
     text.cex = 1, labeltype = "frex", main = "Top 10 frequent and exclusive words - 21k")


#run STM for k=22
modelPrevFit_22 <- stm(documents = out$documents, vocab = out$vocab,
                       K = 22, prevalence = ~ case, 
                       max.em.its = 75, data = out$meta,
                       init.type = "Spectral", seed = 323253221)

# Plot the STM showing highest probability and FREX (frequent and exclusive) words
plot(modelPrevFit_22, xlim = c(0, 0.2), type = "summary", n=10, family = "sans", 
     text.cex = 0.9, labeltype = "prob", main = "Top 10 high probabililty words - 22k")

plot(modelPrevFit_22, xlim = c(0, 0.2), type = "summary", n=10, family = "sans", 
     text.cex = 0.9, labeltype = "frex", main = "Top 10 frequent and exclusive words - 22k")


#run STM for k=28
modelPrevFit_28 <- stm(documents = out$documents, vocab = out$vocab,
                       K = 28, prevalence = ~ case, 
                       max.em.its = 75, data = out$meta,
                       init.type = "Spectral", seed = 323253221)

# Plot the STM showing highest probability and FREX (frequent and exclusive) words
plot(modelPrevFit_28, xlim = c(0, 0.2), type = "summary", n=10, family = "sans", 
     text.cex = 0.9, labeltype = "prob", main = "Top 10 high probabililty words - 28k")

plot(modelPrevFit_28, xlim = c(0, 0.2), type = "summary", n=10, family = "sans", 
     text.cex = 0.9, labeltype = "frex", main = "Top 10 frequent and exclusive words - 28k")


#run STM for k=29
modelPrevFit_29 <- stm(documents = out$documents, vocab = out$vocab,
                       K = 29, prevalence = ~ case, 
                       max.em.its = 75, data = out$meta,
                       init.type = "Spectral", seed = 323253221)

# Plot the STM showing highest probability and FREX (frequent and exclusive) words
plot(modelPrevFit_29, xlim = c(0, 0.2), type = "summary", n=10, family = "sans", 
     text.cex = 1, labeltype = "prob", main = "Top 10 high probabililty words - 29k")

plot(modelPrevFit_29, xlim = c(0, 0.2), type = "summary", n=10, family = "sans", 
     text.cex = 1, labeltype = "frex", main = "Top 10 frequent and exclusive words - 29k")


#run STM for k=36
modelPrevFit_36 <- stm(documents = out$documents, vocab = out$vocab,
                       K = 36, prevalence = ~ case, 
                       max.em.its = 75, data = out$meta,
                       init.type = "Spectral", seed = 323253221)

# Plot the STM showing highest probability and FREX (frequent and exclusive) words
plot(modelPrevFit_36, xlim = c(0, 0.2), type = "summary", n=10, family = "sans", 
     text.cex = 0.9, labeltype = "prob", main = "Top 10 high probabililty words - 36k")

plot(modelPrevFit_36, xlim = c(0, 0.2), type = "summary", n=10, family = "sans", 
     text.cex = 0.9, labeltype = "frex", main = "Top 10 frequent and exclusive words - 36k")


#convergence was reached only for K=28. 
#therefore the 28k is the optimal K

#TOPIC VALIDATION

#We get 4 groups of words from each topic modeling:
#1. Highest prob: words within each topic with the highest probability
#2. Frex: words that are both frequent and exclusive
#Exclusivity refers to the top words associated with a topic that are unlikely to appear among the top words of another topic
#3. and 4. Lift and Score are measures provided in other packages

#A first validation consists of an interpretation of the topics by combining Highest prob and Frex words
plot(modelPrevFit_28, xlim = c(0, 0.2), type = "summary", n=10, family = "sans", 
     text.cex = 0.9, labeltype = "prob", main = "Top 10 high probabililty words - 28k")

plot(modelPrevFit_28, xlim = c(0, 0.2), type = "summary", n=10, family = "sans", 
     text.cex = 0.9, labeltype = "frex", main = "Top 10 frequent and exclusive words - 28k")


#A second validation consists in using the function findThoughts(). 
#This function allows to view the actual documents associated with specific topics to try and determine if the topic coding makes sense. 
#We can then browse the top n documents associated to each topic

#topic 1
thoughts1 <- findThoughts(modelPrevFit_28,
                          texts = merged_df_refugees_STM$text,
                          n = 3,
                          topics = 1)
thoughts1

#topic 2
thoughts2 <- findThoughts(modelPrevFit_28,
                          texts = merged_df_refugees_STM$text,
                          n = 3,
                          topics = 2)
thoughts2

#topic 3
thoughts3 <- findThoughts(modelPrevFit_28,
                          texts = merged_df_refugees_STM$text,
                          n = 15,
                          topics = 3)
thoughts3

#topic 4
thoughts4 <- findThoughts(modelPrevFit_28,
                          texts = merged_df_refugees_STM$text,
                          n = 3,
                          topics = 4)
thoughts4

#topic 5
thoughts5 <- findThoughts(modelPrevFit_28,
                          texts = merged_df_refugees_STM$text,
                          n = 3,
                          topics = 5)
thoughts5

#topic 6
thoughts6 <- findThoughts(modelPrevFit_28,
                          texts = merged_df_refugees_STM$text,
                          n = 3,
                          topics = 6)
thoughts6

#topic 7
thoughts7 <- findThoughts(modelPrevFit_28,
                          texts = merged_df_refugees_STM$text,
                          n = 3,
                          topics = 7)
thoughts7

#topic 8
thoughts8 <- findThoughts(modelPrevFit_28,
                          texts = merged_df_refugees_STM$text,
                          n = 3,
                          topics = 8)
thoughts8

#topic 9
thoughts9 <- findThoughts(modelPrevFit_28,
                          texts = merged_df_refugees_STM$text,
                          n = 3,
                          topics = 9)
thoughts9

#topic 10
thoughts10 <- findThoughts(modelPrevFit_28,
                           texts = merged_df_refugees_STM$text,
                           n = 3,
                           topics = 10)
thoughts10

#topic 11
thoughts11 <- findThoughts(modelPrevFit_28,
                           texts = merged_df_refugees_STM$text,
                           n = 3,
                           topics = 11)
thoughts11

#topic 12
thoughts12 <- findThoughts(modelPrevFit_28,
                           texts = merged_df_refugees_STM$text,
                           n = 3,
                           topics = 12)
thoughts12

#topic 13
thoughts13 <- findThoughts(modelPrevFit_28,
                           texts = merged_df_refugees_STM$text,
                           n = 3,
                           topics = 13)
thoughts13

#topic 14
thoughts14 <- findThoughts(modelPrevFit_28,
                           texts = merged_df_refugees_STM$text,
                           n = 3,
                           topics = 14)
thoughts14

#topic 15
thoughts15 <- findThoughts(modelPrevFit_28,
                           texts = merged_df_refugees_STM$text,
                           n = 3,
                           topics = 15)
thoughts15

#topic 16
thoughts16 <- findThoughts(modelPrevFit_28,
                           texts = merged_df_refugees_STM$text,
                           n = 3,
                           topics = 16)
thoughts16

#topic 17
thoughts17 <- findThoughts(modelPrevFit_28,
                           texts = merged_df_refugees_STM$text,
                           n = 3,
                           topics = 17)
thoughts17

#topic 18
thoughts18 <- findThoughts(modelPrevFit_28,
                           texts = merged_df_refugees_STM$text,
                           n = 3,
                           topics = 18)
thoughts18

#topic 19
thoughts19 <- findThoughts(modelPrevFit_28,
                           texts = merged_df_refugees_STM$text,
                           n = 3,
                           topics = 19)
thoughts19

#topic 20
thoughts20 <- findThoughts(modelPrevFit_28,
                           texts = merged_df_refugees_STM$text,
                           n = 3,
                           topics = 20)
thoughts20

#topic 21
thoughts21 <- findThoughts(modelPrevFit_28,
                           texts = merged_df_refugees_STM$text,
                           n = 3,
                           topics = 21)
thoughts21

#topic 22
thoughts22 <- findThoughts(modelPrevFit_28,
                           texts = merged_df_refugees_STM$text,
                           n = 3,
                           topics = 22)
thoughts22

#topic 23
thoughts23 <- findThoughts(modelPrevFit_28,
                           texts = merged_df_refugees_STM$text,
                           n = 3,
                           topics = 23)
thoughts23

#topic 24
thoughts24 <- findThoughts(modelPrevFit_28,
                           texts = merged_df_refugees_STM$text,
                           n = 3,
                           topics = 24)
thoughts24

#topic 25
thoughts25 <- findThoughts(modelPrevFit_28,
                           texts = merged_df_refugees_STM$text,
                           n = 3,
                           topics = 25)
thoughts25

#topic 26
thoughts26 <- findThoughts(modelPrevFit_28,
                           texts = merged_df_refugees_STM$text,
                           n = 3,
                           topics = 26)
thoughts26

#topic 27
thoughts27 <- findThoughts(modelPrevFit_28,
                           texts = merged_df_refugees_STM$text,
                           n = 3,
                           topics = 27)
thoughts27

#topic 28
thoughts28 <- findThoughts(modelPrevFit_28,
                           texts = merged_df_refugees_STM$text,
                           n = 3,
                           topics = 28)
thoughts28

# topics are labelled by triangulating high-probability and frequent/exclusive words with findThoughts function
#TOPIC LABELS:
# Topic 1: CONDEMNATION OF WAR (TERRITORIAL INTEGRITY)
# Topic 2: EFFECTS OF WAR ON PRICES AND TRANSPORT
# Topic 3: TERRORISM AND JIHAD
# Topic 4: THE EU'S INVOLVEMENT IN SYRIA
# Topic 5: THE ROLE OF BELARUS
# Topic 6: EASTERN EUROPEAN SOLIDARITY TOWARDS UKRAINIAN REFUGEES
# Topic 7: PERSECUTION OF KURDS BY TURKEY
# Topic 8: SUFFERING OF CIVILIANS AND INTERESTS OF DIFFERENT FACTIONS
# Topic 9: CEASEFIRE AND PEACEFUL SOLUTION
# Topic 10: RECEPTION OF REFUGEES (TEMPORARY PROTECTION DIRECTIVE AND MOLDOVA)
# Topic 11: DEFEND UKRAINE AND EUROPE FROM RUSSIA
# Topic 12: THE ROLE OF EASTERN EUROPEAN COUNTRIES
# Topic 13: CRIMES AGAINST WOMEN AND CHILDREN
# Topic 14: THE ROLE OF POLAND
# Topic 15: SUPPORT UKRAINIAN CULTURE
# Topic 16: MANAGEMENT OF MIGRATION FLOW
# Topic 17: THE IMPACT OF WAR ON FOOD SECURITY
# Topic 18: TRUST FUNDS FOR SYRIA AND AFRICA
# Topic 19: PERSECUTIONS AGAINST CHRISTIAN COMMUNITIES
# Topic 20: THE EU'S ERRORS AND POWERLESSNESS
# Topic 21: FIGHT AGAINST TERRORISM AND INTERNATIONAL INVOLVEMENT IN SYRIA 
# Topic 22: MIGRANTS' BLACKMAIL BY TURKEY AGAINST THE EU 
# Topic 23: HUMANITARIAN CATASTROPHE IN SYRIA
# Topic 24: HUMAN-RIGHT VIOLATION BY ISLAMIC FACTIONS
# Topic 25: WEAPONISATION AND DESTRUCTION IN SYRIA
# Topic 26: TURKISH OPERATION IN NORTH-EAST SYRIA
# Topic 27: RUSSIAN AGGRESSION TO EUROPEAN DEMOCRACY
# Topic 28: THE DRAMATIC NATURE OF WAR (SUPPORT TO UKRAINE VS HORRORS OF THE SYRIAN WAR)


# Case (Syria and Ukraine) proportion in the interventions shaping each topic

# Extract the theta matrix (document-topic probabilities)
theta_matrix <- as.data.frame(modelPrevFit_28$theta)

# Add case information to the dataframe 
theta_matrix$case <- merged_df_refugees_STM$case

# Calculate the number of documents for each topic and case combination
topic_case_counts <- table(apply(theta_matrix[, -ncol(theta_matrix)], 1, which.max), theta_matrix$case)

# Print the counts
print(topic_case_counts)

# Convert the table to a data frame
topic_case_df <- as.data.frame(topic_case_counts)

# Rename the columns for clarity
colnames(topic_case_df) <- c("Topic", "Case", "Count")

# Calculate proportions within each topic
topic_case_df$Proportion <- topic_case_df$Count / rowSums(table(topic_case_df$Topic, topic_case_df$Case))

case_colors <- c("gray", "black")

ggplot(topic_case_df, aes(x = Proportion, y = factor(Topic), fill = factor(Case))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Proportion", y = "Topic", fill = "Case") +
  ggtitle("Case proportion per topic")+
  scale_fill_manual(values = case_colors) 


#end
#-------------------------------------------------------------------------