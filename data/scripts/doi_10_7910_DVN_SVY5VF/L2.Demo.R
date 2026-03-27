## Author: Kabir Khanna
## Updated: December 20, 2015
## Note: Adds Pr(Precinct, Age, Gender | Race) based on L2 demographic data to replication dataset

## Load Replication Dataset
load("FL.Anon.RData")

eth <- c("whi", "bla", "his", "asi", "oth")

## Subset Voters with Non-Missing Self-Reported Race
keep <- c("VoterID", "District", "Precinct", "vote08", "Voters_Age", "Voters_Gender", 
          paste("p", eth, sep = "_"), paste("r_pid", eth, sep = "_"), paste("SR", toupper(eth), sep = "."))
df <- fl.anon[!is.na(fl.anon$SR.WHI) & !is.na(fl.anon$SR.BLA) & !is.na(fl.anon$SR.HIS) & !is.na(fl.anon$SR.ASI) & !is.na(fl.anon$SR.OTH), keep]

precincts <- unique(df[!is.na(df$Precinct), "Precinct"])

## Census age categories: lower and upper bounds
age.cat <- seq(5, 23)
age.lower <- c(18, 20, 21, 22, 25, 30, 35, 40, 45, 50, 55, 60, 62, 65, 67, 70, 75, 80, 85)
age.upper <- c(19, 20, 21, 24, 29, 34, 39, 44, 49, 54, 59, 61, 64, 66, 69, 74, 79, 84, 99)

for (k in 1:length(eth)) {
  print(eth[k])
  assign(paste(eth[k], "tot", sep = "."), sum(df[paste("SR", toupper(eth[k]), sep = ".")], na.rm = T))
}

for (j in 1:length(age.cat)) {
  print(age.cat[j])
  df[!is.na(df$Voters_Age) & df$Voters_Age >= age.lower[j] & df$Voters_Age <= age.upper[j], "age.cat"] <- age.cat[j]
}

for (p in 1:length(precincts)) {
  df.prc <- df[!is.na(df$Precinct) & df$Precinct == precincts[p], ]
  
  age.cat.temp <- unique(df.prc$age.cat)[!is.na(unique(df.prc$age.cat))]
  age.cat.temp <- age.cat.temp[order(age.cat.temp)]
  
  for (k in 1:length(eth)) {
    print(eth[k])
    for (j in 1:length(age.cat.temp)) {
      assign(paste(eth[k], "fem", age.cat[j], "tot", sep = "."), 
             sum(df.prc[df.prc$Voters_Gender == "F" & !is.na(df.prc$age.cat) & df.prc$age.cat == age.cat.temp[j], 
                        paste("SR", toupper(eth[k]), sep = ".")], na.rm = T))
      assign(paste(eth[k], "mal", age.cat[j], "tot", sep = "."), 
             sum(df.prc[df.prc$Voters_Gender == "M" & !is.na(df.prc$age.cat) & df.prc$age.cat == age.cat.temp[j], 
                        paste("SR", toupper(eth[k]), sep = ".")], na.rm = T))
      
      df.prc[df.prc$Voters_Gender == "F" & !is.na(df.prc$age.cat) & df.prc$age.cat == age.cat.temp[j], 
             paste("l2_prc_age_sex", eth[k], sep = "_")] <- 
        get(paste(eth[k], "fem", age.cat[j], "tot", sep = ".")) / get(paste(eth[k], "tot", sep = "."))
      
      df.prc[df.prc$Voters_Gender == "M" & !is.na(df.prc$age.cat) & df.prc$age.cat == age.cat.temp[j], 
             paste("l2_prc_age_sex", eth[k], sep = "_")] <- 
        get(paste(eth[k], "mal", age.cat[j], "tot", sep = ".")) / get(paste(eth[k], "tot", sep = "."))
    }
  df[!is.na(df$Precinct) & df$Precinct == precincts[p], paste("l2_prc_age_sex", eth[k], sep = "_")] <- 
    df.prc[, paste("l2_prc_age_sex", eth[k], sep = "_")]
  }
  print(paste("Completed", p, "of", length(precincts), "precincts"))
}

df.l2 <- df
save(df.l2, file = "df.l2.RData")
