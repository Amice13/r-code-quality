# Libraries
install.packages("plyr")
library(plyr)

########################
###### Read in the data
########################

lab <- read.csv("Copartisans_labour.csv", stringsAsFactors=F)
cons <- read.csv("Copartisans_conservative.csv", stringsAsFactors=F)

########################
###### Organize Labour data
########################

# Organize emotion variables
lab$excited <- as.numeric(mapvalues(lab$excited, c("Not at all","Slightly","Moderately","Much","Very much"), 1:5))
lab$angry <- as.numeric(mapvalues(lab$angry, c("Not at all","Slightly","Moderately","Much","Very much"), 1:5))
lab$hopeful <- as.numeric(mapvalues(lab$hopeful, c("Not at all","Slightly","Moderately","Much","Very much"), 1:5))
lab$proud <- as.numeric(mapvalues(lab$proud, c("Not at all","Slightly","Moderately","Much","Very much"), 1:5))
lab$disgusted <- as.numeric(mapvalues(lab$disgusted, c("Not at all","Slightly","Moderately","Much","Very much"), 1:5))

# Organize treatment variable
lab$treat_m <- ifelse(lab$treat_m=="Moral",1,0)

# Create measure of average positive emotion
lab$positive <- apply(lab[,c("proud","hopeful","excited")],1,mean,na.rm=T)

# Organize gender variable
colnames(lab)[colnames(lab)=="sex"] <- "male"
lab$male <- ifelse(lab$male=="Male",1,0)

# Organize education variable
lab$educ <- as.numeric(mapvalues(lab$educ, c("College/A levels", "Graduate degree (MA/MSc/MPhil/other)", "Secondary school/GCSE", "Undergraduate degree (BA/BSc/other)", "No formal qualifications", "Doctorate degree (PhD/MD/other)"), c(3,5,2,4,1,5)))

# Organize ethnicity variable
colnames(lab)[colnames(lab)=="ethnicity"] <- "white"
lab$white <- ifelse(lab$white=="Caucasian",1,0)

########################
###### Organize Conservative data
########################

# Organize emotion variables
cons$excited <- as.numeric(mapvalues(cons$excited, c("Not at all","Slightly","Moderately","Much","Very much"), 1:5))
cons$angry <- as.numeric(mapvalues(cons$angry, c("Not at all","Slightly","Moderately","Much","Very much"), 1:5))
cons$hopeful <- as.numeric(mapvalues(cons$hopeful, c("Not at all","Slightly","Moderately","Much","Very much"), 1:5))
cons$proud <- as.numeric(mapvalues(cons$proud, c("Not at all","Slightly","Moderately","Much","Very much"), 1:5))
cons$disgusted <- as.numeric(mapvalues(cons$disgusted, c("Not at all","Slightly","Moderately","Much","Very much"), 1:5))

# Organize treatment variable
cons$treat_m <- ifelse(cons$treat_m=="Moral",1,0)

# Create measure of average positive emotion
cons$positive <- apply(cons[,c("proud","hopeful","excited")],1,mean,na.rm=T)

# Organize gender variable
colnames(cons)[colnames(cons)=="sex"] <- "male"
cons$male <- ifelse(cons$male=="Male",1,0)

# Organize education variable
cons$educ <- as.numeric(mapvalues(cons$educ, c("College/A levels", "Graduate degree (MA/MSc/MPhil/other)", "Secondary school/GCSE", "Undergraduate degree (BA/BSc/other)", "No formal qualifications", "Doctorate degree (PhD/MD/other)"), c(3,5,2,4,1,5)))

# Organize ethnicity variable
colnames(cons)[colnames(cons)=="ethnicity"] <- "white"
cons$white <- ifelse(cons$white=="White",1,0)

# Organize income variable
cons$income <- as.numeric(mapvalues(cons$income, c("£40,000 - £49,999",   "Less than £10,000 " , "£30,000 - £39,999" ,  "£50,000 - £59,999",   "£20,000 - £29,999"   ,"£16,000 - £19,999"  , "£10,000 - £15,999"  , "£80,000 - £89,999"  , "£60,000 - £69,999"  ,"£70,000 - £79,999" ,  "£100,000 - £149,999" ,"£90,000 - £99,999" ,"More than £150,000" ), c(6,1,5,7,4,3,2,10,8,9,12,11,13)))

########################
###### Save the data
########################

save(lab, cons, file="Study2.RData")
