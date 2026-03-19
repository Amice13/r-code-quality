#### 1. Read in Muraoka and Rosas's data ####
load("data/raw/muraoka_rosas_replication/CSES_All_Merged.RData")


#### 2. Cleaning ####
## limit to income quintiles
df <- cses.all.merged[cses.all.merged$quintile==1,]
rm(cses.all.merged)

## remove NAs
df <- na.omit(df[,c("individual_id", "country", "wave", "year", "income",
                    "age", "gender",
                    "education", "perceive_position", "swiid_gini",
                    "gdpcapita", "enpp", "party_id", "position")])

## order unique survey-party id
df$sp <- paste0(df$country, df$wave, df$year, df$party_id)
for(i in 1:length(unique(df$sp))){
  df$sp[df$sp==unique(df$sp)[i]] <- i
}
df$sp <- as.numeric(df$sp)

## order unique survey id (country-year id)
df$s <- paste0(df$country, df$wave, df$year)
for(i in 1:length(unique(df$s))){
  df$s[df$s==unique(df$s)[i]] <- i
}
df$s <- as.numeric(df$s)

## order unique party id
df$p <- NA
for(i in 1:length(unique(df$party_id))){
  df$p[df$party_id==unique(df$party_id)[i]] <- i
}
df$p <- as.numeric(df$p)

## outcome variables
df$perception.error <- df$perceive_position - df$position
df$perception.error.abs <- abs(df$perception.error)
#plot(density(df$perception.error, na.rm=TRUE))
#lines(density(df$perception.error.abs, na.rm=TRUE), col="red")

## centering by survey
for(i in unique(df$s)){
  df$perception.error[df$s==i] <- df$perception.error[df$s==i] - mean(df$perception.error[df$s==i])
}

for(i in unique(df$s)){
  df$perception.error.abs[df$s==i] <- df$perception.error.abs[df$s==i] - mean(df$perception.error.abs[df$s==i])
}

## dummies for income categories
df$q1 <- as.numeric(df$income==1)
df$q2 <- as.numeric(df$income==2)
df$q4 <- as.numeric(df$income==4)
df$q5 <- as.numeric(df$income==5)
income.cat <- as.matrix(df[,c("q1", "q2", "q4", "q5")])
which(rowSums(income.cat) > 1)

## individual-level controls
X <- cbind (df$education, df$age, df$gender)

## centering by survey
for(i in unique(df$s)){
  X[df$s==i,] <- sapply (1:ncol(X), function(p){X[df$s==i,p] - mean(X[df$s==i,p])})
}
X <- as.matrix(X)

## survey-party-level variables
sp.var <- unique(df[,c("sp", "s", "p", "position")])
which(duplicated(sp.var$sp)==TRUE)

## centering
sp.var$position <- sp.var$position - mean(sp.var$position)

## survey-level variables
s.var <- unique(df[,c("s", "swiid_gini", "enpp", "gdpcapita")])
which(duplicated(s.var$s)==TRUE)

## log gdp per capita
s.var$loggdpcap <- log(s.var$gdpcapita)

## log swiid
s.var$logswiid <- log(s.var$swiid_gini)

## centering
s.var$logswiid <- s.var$logswiid - mean(s.var$logswiid)
s.var$enpp <- s.var$enpp - mean(s.var$enpp)
s.var$loggdpcap <- s.var$loggdpcap - mean(s.var$loggdpcap)

## party indicators
p <- unique(df[,c("p")])
which(duplicated(p)==TRUE)

#### 3. Create Subsets ####

## Merging income.cat
df2 <- cbind(df, income.cat)

## Merging position from sp.var
colnames(sp.var)

for(i in unique(sp.var$sp)){
  df2$position[df2$sp==i] <- sp.var$position[sp.var$sp==i]
}

which(is.na(df2$position))

## Merging logswiid and loggdpcap from s.var
colnames(s.var)

df2$logswiid <- NA
df2$loggdpcap <- NA

for(i in unique(s.var$s)){
  df2$logswiid[df2$s==i] <- s.var$logswiid[s.var$s==i]
  df2$loggdpcap[df2$s==i] <- s.var$loggdpcap[s.var$s==i]
  df2$enpp[df2$s==i] <- s.var$enpp[s.var$s==i]
}

which(is.na(df2$logswiid))

## Creating Left Center and Right subsets

left.df <- df2[df2$position<= -1.65,]
center.df <- df2[df2$position > -1.65 & df2$position <= 1.34,]
right.df <- df2[df2$position > 1.34,]

## check number of observations
nrow(df2)==nrow(left.df)+nrow(center.df)+nrow(right.df)

## order unique survey-party id
#### For Left
left.df$sub.sp <- NA
for(i in 1:length(unique(left.df$sp))){
  left.df$sub.sp[left.df$sp==unique(left.df$sp)[i]] <- i
}
left.df$sub.sp <- as.numeric(left.df$sub.sp)

#### For Center
center.df$sub.sp <- NA
for(i in 1:length(unique(center.df$sp))){
  center.df$sub.sp[center.df$sp==unique(center.df$sp)[i]] <- i
}
center.df$sub.sp <- as.numeric(center.df$sub.sp)

#### For Right
right.df$sub.sp <- NA
for(i in 1:length(unique(right.df$sp))){
  right.df$sub.sp[right.df$sp==unique(right.df$sp)[i]] <- i
}
right.df$sub.sp <- as.numeric(right.df$sub.sp)

## order unique survey id (country-year id)
#### For Left
left.df$sub.s <- NA
for(i in 1:length(unique(left.df$s))){
  left.df$sub.s[left.df$s==unique(left.df$s)[i]] <- i
}
left.df$sub.s <- as.numeric(left.df$sub.s)

#### For Center
center.df$sub.s <- NA
for(i in 1:length(unique(center.df$s))){
  center.df$sub.s[center.df$s==unique(center.df$s)[i]] <- i
}
center.df$sub.s <- as.numeric(center.df$sub.s)

#### For Right
right.df$sub.s <- NA
for(i in 1:length(unique(right.df$s))){
  right.df$sub.s[right.df$s==unique(right.df$s)[i]] <- i
}
right.df$sub.s <- as.numeric(right.df$sub.s)

## order unique party id
#### For Left
left.df$sub.p <- NA
for(i in 1:length(unique(left.df$party_id))){
  left.df$sub.p[left.df$party_id==unique(left.df$party_id)[i]] <- i
}
left.df$sub.p <- as.numeric(left.df$sub.p)

#### For Center
center.df$sub.p <- NA
for(i in 1:length(unique(center.df$party_id))){
  center.df$sub.p[center.df$party_id==unique(center.df$party_id)[i]] <- i
}
center.df$sub.p <- as.numeric(center.df$sub.p)

#### For Right
right.df$sub.p <- NA
for(i in 1:length(unique(right.df$party_id))){
  right.df$sub.p[right.df$party_id==unique(right.df$party_id)[i]] <- i
}
right.df$sub.p <- as.numeric(right.df$sub.p)

#### 4. Save data to analysis folder ####
save(right.df, center.df, left.df, file="data/analysis/muraoka_rosas_replication/mr_data.rda")

