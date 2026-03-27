# Hayley Arader
# March 2016
# Senior Thesis: "Minority Rules: Evidence of Racial Salience in the 2008 Democratic Presidential Primaries" 


# PACKAGES #################################

#install.packages("effects")
#install.packages("lme4")
#install.packages("colorspace")
#install.packages("ggplot2")
#install.packages("dplyr")

library(ggplot2)
library(lme4)
library(colorspace)
library(effects)
library(dplyr)

# READ IN DATA SET AND VARIABLES #################################

#Load dataset
load("/Users/hayleyarader/Desktop/cces_2008_common.rdata")

#Read in variables, these are unchanged

original <- x

#Part III-B: PROFILE SURVEY
zip <- original$V202
regstatus <- original$V203
zipreg <- original$V204
birthyr <- original$V207
gender <- original$V208
employment <- original$V209
race <- original$V211
education <- original$V213
marital <- original$V214
bornagain <- original$V215
imptrelig <- original$V216
churchattendance <- original$V217
prayerfreq <- original$V218
religion <- original$V219
protestant <- original$V220
churchgroup <- original$V221
baptist <- original$V222
methodist <- original$V223
nondenom <- original$V224
lutheran <- original$V225
presbyterian <- original$V226
pentecostal <- original$V227
episcopal <- original$V228
christian <- original$V229
congregational <- original$V230
holiness <- original$V231
reformed <- original$V232
adventist <- original$V233
catholic <- original$V234
mormon <- original$V235
orthodox <- original$V236
jewish <- original$V237
muslim <- original$V238
buddhist <- original$V239
hindu <- original$V240
child <- original$V242
numchild <- original$V241
polidealogy <- original$V243
newsinterest <- original$V244
levelinterest <- original$V245
income <- original$V246
agegroup <- original$V247
zip2 <- original$V253
regstatus2 <- original$V256
zipreg2 <- original$V258
statecode <- original$V251

df <-
  data.frame(
    statecode, zip, regstatus, zipreg, birthyr, gender, employment, race, education, marital, bornagain, imptrelig, churchattendance, prayerfreq, religion, protestant, churchgroup, baptist, methodist, nondenom, lutheran, presbyterian, pentecostal, episcopal, christian, congregational, holiness, reformed, adventist, catholic, mormon, orthodox, jewish, muslim, buddhist, hindu, child, numchild, polidealogy, newsinterest, levelinterest, income, agegroup, zip2, regstatus2, zipreg2
  )

#State codes
df$state[df$statecode == 1 | df$statecode == "01"] <- "Alabama"
df$state[df$statecode == 2 | df$statecode == "02"] <- "Alaska"
df$state[df$statecode == 4 | df$statecode == "04"] <- "Arizona"
df$state[df$statecode == 5 | df$statecode == "05"] <- "Arkansas"
df$state[df$statecode == 6 | df$statecode == "06"] <- "California"
df$state[df$statecode == 8 | df$statecode == "08"] <- "Colorado"
df$state[df$statecode == 9 | df$statecode == "09"] <- "Connecticut"
df$state[df$statecode == 10] <- "Delaware"
df$state[df$statecode == 11] <- "DC"
df$state[df$statecode == 12] <- "Florida"
df$state[df$statecode == 13] <- "Georgia"
df$state[df$statecode == 15] <- "Hawaii"
df$state[df$statecode == 16] <- "Idaho"
df$state[df$statecode == 17] <- "Illinois"
df$state[df$statecode == 18] <- "Indiana"
df$state[df$statecode == 19] <- "Iowa"
df$state[df$statecode == 20] <- "Kansas"
df$state[df$statecode == 21] <- "Kentucky"
df$state[df$statecode == 22] <- "Louisiana"
df$state[df$statecode == 23] <- "Maine"
df$state[df$statecode == 24] <- "Maryland"
df$state[df$statecode == 25] <- "Massachusetts"
df$state[df$statecode == 26] <- "Michigan"
df$state[df$statecode == 27] <- "Minnesota"
df$state[df$statecode == 28] <- "Mississippi"
df$state[df$statecode == 29] <- "Missouri"
df$state[df$statecode == 30] <- "Montana"
df$state[df$statecode == 31] <- "Nebraska"
df$state[df$statecode == 32] <- "Nevada"
df$state[df$statecode == 33] <- "New Hampshire"
df$state[df$statecode == 34] <- "New Jersey"
df$state[df$statecode == 35] <- "New Mexico"
df$state[df$statecode == 36] <- "New York"
df$state[df$statecode == 37] <- "North Carolina"
df$state[df$statecode == 38] <- "North Dakota"
df$state[df$statecode == 39] <- "Ohio"
df$state[df$statecode == 40] <- "Oklahoma"
df$state[df$statecode == 41] <- "Oregon"
df$state[df$statecode == 42] <- "Pennsylvania"
df$state[df$statecode == 44] <- "Rhode Island"
df$state[df$statecode == 45] <- "South Carolina"
df$state[df$statecode == 46] <- "South Dakota"
df$state[df$statecode == 47] <- "Tennessee"
df$state[df$statecode == 48] <- "Texas"
df$state[df$statecode == 49] <- "Utah"
df$state[df$statecode == 50] <- "Vermont"
df$state[df$statecode == 51] <- "Virginia"
df$state[df$statecode == 53] <- "Washington"
df$state[df$statecode == 54] <- "West Virginia"
df$state[df$statecode == 55] <- "Wisconsin"
df$state[df$statecode == 56] <- "Wyoming"

# Part III-C: PRE ELECTION SURVEY
df$mostimpt <- original$CC301
df$partyid <- original$CC307
df$partystrength <- original$CC307a

#CC308-CC309 are questions about political knowledge

df$abortion <- original$CC310
df$affirm <- original$CC313

# Support/oppose issue questions, CC316a-i, are coded below

# Ideological scales, liberal = 0 and conservative = 100

df$scale <- original$CC317a
df$dempartyscale <- original$CC317b
df$reppartyscale <- original$CC317c
df$bushpartyscale <- original$CC317d
df$obamascale <- original$CC317h
df$mccainpartyscale <- original$CC317g


# What is the race or ethnicity of your member of the US house of reps? Issue: what is they're wrong? Recoded below.
df$houserace <- original$CC319

# Did you vote in the Presidential primary or attend a caucus between January and June of this year?
df$primaryvoteyesprimary <- original$CC324_1
df$primaryvoteyescaucus <- original$CC324_2
df$primaryvoteno <- original$CC324_3
df$primaryvotechoice <- original$CC325a
df$caucusvotechoice <- original$CC325b

df$intendtovote <- original$CC326
df$votechoice <- original$CC326b

#Military household based off CC328_1, coded below

#Union household based of CC329, coded below

df$citizen <- original$CC332

#Homeowner based off CC333, coded below

#Part III-D: POST ELECTION SURVEY

df$partyreg <- original$CC402
df$novvote <- original$CC403
df$abstentionreason <- original$CC404
df$line <- original$CC406fff
#For whom did respondent vote in general
df$preschoice <- original$CC410

#CC415 asks about political involvement, coded below

# Variables I constructed

#Binary Variable: Voted for Obama?
df$Obama <- rep(0, 32800)
df$Obama[df$primaryvotechoice == "Barack Obama"] <- 1

# Binary Variable, voted for Clinton?
df$Clinton <- rep(0, 32800)
df$Clinton[df$primaryvotechoice == "Hillary Clinton"] <- 1

# Numeric scale for education level
df$edscale <- rep(NA, 32800)
df$edscale[education == "No HS"] <- 1
df$edscale[education == "High school graduate"] <- 2
df$edscale[education == "Some college"] <- 3
df$edscale[education == "2-year"] <- 4
df$edscale[education == "4-year"] <- 5
df$edscale[education == "Post-grad"] <- 6

# Binary variable, 1 = nonwhite respondent, 0 = white
df$nonwhite <- rep(1, 32800)
df$nonwhite[race == "White"] <- 0

# Bingary variable, 1 = female, 0 = male
df$female <- rep(1, 32800)
df$female[gender == "Male"] <- 0

# Bingary variable, 1 = black, 0 = other
df$black <- rep(0, 32800)
df$black[race == "Black"] <- 1

# Bingary variable, 1 = hispanic, 0 = other
df$hispanic <- rep(0, 32800)
df$hispanic[race == "Hispanic"] <- 1

# Bingary variable, 1 = asian, 0 = other
df$asian <- rep(0, 32800)
df$asian[race == "Asian"] <- 1

# Bingary variable, 1 = Native American, Mixed, Other, or Middle Eastern, 0 = other
df$othernonwhite <- rep(0, 32800)
df$othernonwhite[race == "Native American" |
                   race == "Mixed" | race == "Other" | race == "Middle Eastern"] <- 1

# Binary Variable, 0 = white or black or hispanic, 1 = other
df$othernonwhite2 <- rep(NA, 32800)
df$othernonwhite2[df$hispanic == 1 | df$black == 1 | df$white == 1] = 0
df$othernonwhite2[df$asian == 1 | df$othernonwhite == 1] = 1

# Bingary variable, 1 = white, 0 = other
df$white <- rep(0, 32800)
df$white[race == "White"] <- 1

# Numeric party scale
df$partyscale <- rep(NA, 32800)
df$partyscale[df$partystrength == "Strong Democrat"] <- 1
df$partyscale[df$partystrength == "Not very strong Democrat"] <- 2
df$partyscale[df$partystrength == "Lean Democrat"] <- 3
df$partyscale[df$partystrength == "Independent"] <- 4
df$partyscale[df$partystrength == "Lean Republican"] <- 5
df$partyscale[df$partystrength == "Not very strong Republican"] <- 6
df$partyscale[df$partystrength == "Strong Republican"] <- 7

# Income categories
df$incomecat <- rep(NA, 32800)
df$incomecat[income == "less than $10,000"] <- 1
df$incomecat[income == "$10,000 - $14,999"] <- 2
df$incomecat[income == "$15,000 - $19,999"] <- 3
df$incomecat[income == "$20,000 - $24,999"] <- 4
df$incomecat[income == "$25,000 - $29,999"] <- 5
df$incomecat[income == "$30,000 - $39,999"] <- 6
df$incomecat[income == "$40,000 - $49,999"] <- 7
df$incomecat[income == "$50,000 - $59,999"] <- 8
df$incomecat[income == "$60,000 - $69,999"] <- 9
df$incomecat[income == "$70,000 - $79,999"] <- 10
df$incomecat[income == "$80,000 - $99,999"] <- 11
df$incomecat[income == "$100,000 - $119,999"] <- 12
df$incomecat[income == "$120,000 - $149,999"] <- 13
df$incomecat[income == "$150,000 or more"] <- 14

df$age <- 2008 - (as.numeric(levels(birthyr))[birthyr])

df$married <- rep(0, 32800)
df$married[marital == "Married"] <- 1

df$parent <- rep(0, 32800)
df$parent[child == "Yes"] <- 1

df$news <- rep(NA, 32800)
df$news[newsinterest == "Hardly at all"] <- 1
df$news[newsinterest == "Only now and then"] <- 2
df$news[newsinterest == "Some of the time"] <- 3
df$news[newsinterest == "Most of the time"] <- 4

df$abortscale <- rep(NA, 32800)
df$abortscale[df$abortion == "By law, never permit abortion"] <- 4
df$abortscale[df$abortion == "Abortion only if rape, incest, life in danger"] <-
  3
df$abortscale[df$abortion == "Abortion only if need established"] <- 2
df$abortscale[df$abortion == "By law, always allow abortion"] <- 1

df$affirmscale <- rep(NA, 32800)
df$affirmscale[df$affirm == "Strongly support"] <- 1
df$affirmscale[df$affirm == "Somewhat support"] <- 2
df$affirmscale[df$affirm == "Somewhat oppose"] <- 3
df$affirmscale[df$affirm == "Strongly oppose"] <- 4

# Withdraw troops from Iraq within 180 days?
df$withdraw <- rep(NA, 32800)
df$withdraw[original$CC316a == "Support"] <- 1
df$withdraw[original$CC316a == "Oppose"] <- 0

# Increase Minimum Wage from $5.15 to $7.25?
df$minwage <- rep(NA, 32800)
df$minwage[original$CC316b == "Support"] <- 1
df$minwage[original$CC316b == "Oppose"] <- 0

# Allow federal funding of embryonic stem cell research?
df$stemcell <- rep(NA, 32800)
df$stemcell[original$CC316c == "Support"] <- 1
df$stemcell[original$CC316c == "Oppose"] <- 0

# Allow U.S. spy agencies to eavesdrop on overseas terrorist suspects without first getting a court order?
df$eavesdrop <- rep(NA, 32800)
df$eavesdrop[original$CC316d == "Support"] <- 1
df$eavesdrop[original$CC316d == "Oppose"] <- 0

#Fund a $20 billion program to provide health insurance for children in families earning less than $43,000?
df$childhealth <- rep(NA, 32800)
df$childhealth[original$CC316e == "Support"] <- 1
df$childhealth[original$CC316e == "Oppose"] <- 0

#Constitutional Amendment banning Gay Marriage?
df$bangaymar <- rep(NA, 32800)
df$bangaymar[original$CC316f == "Support"] <- 1
df$bangaymar[original$CC316f == "Oppose"] <- 0

#Federal assistance for homeowners facing foreclosure and large lending institutions at risk of failing?
df$fedhousing <- rep(NA, 32800)
df$fedhousing[original$CC316g == "Support"] <- 1
df$fedhousing[original$CC316g == "Oppose"] <- 0

#Extend the North American Free trade Agreement (NAFTA) to include Peru and Columbia?
df$nafta <- rep(NA, 32800)
df$nafta[original$CC316h == "Support"] <- 1
df$nafta[original$CC316h == "Oppose"] <- 0

#U.S. Government's $700 Billion Bank Bailout Plan?
df$bailout <- rep(NA, 32800)
df$bailout[original$CC316i == "Support"] <- 1
df$bailout[original$CC316i == "Oppose"] <- 0

#Do you consider this candidate to be...
# HONEST
df$bohonest <- rep(NA, 32800)
df$bohonest[original$CC317H_BO == "Yes"] <- 1
df$bohonest[original$CC317H_BO == "No"] <- 0

df$jmhonest <- rep(NA, 32800)
df$jmhonest[original$CC317H_BO == "Yes"] <- 1
df$jmhonest[original$CC317H_BO == "No"] <- 0

# KNOWLEDGEABLE
df$boknowledge <- rep(NA, 32800)
df$boknowledge[original$CC317H_BO == "Yes"] <- 1
df$boknowledge[original$CC317H_BO == "No"] <- 0

df$jmknowledge <- rep(NA, 32800)
df$jmknowledge[original$CC317I_BO == "Yes"] <- 1
df$jmknowledge[original$CC317I_BO == "No"] <- 0

# EXPERIENCED
df$boexperience <- rep(NA, 32800)
df$boexperience[original$CC317E_BO == "Yes"] <- 1
df$boexperience[original$CC317E_BO == "No"] <- 0

df$jmexperience <- rep(NA, 32800)
df$jmexperience[original$CC317E_BO == "Yes"] <- 1
df$jmexperience[original$CC317E_BO == "No"] <- 0

# Race of house rep. Nonwhite? Come back to this***
df$nonwhitehouse <- rep(NA, 32800)
df$nonwhitehouse[df$houserace == "Black" |
                   df$houserace == "Hispanic" | df$houserace == "Other"] <- 1
df$nonwhitehouse[df$houserace == "White"] <- 0

#Binary military variable, 1 = Either I or members of my immediate family are or have served in the U.S. military, 0 = neither myself nor members of my immediate family have served in military
df$military <- rep(NA, 32800)
df$military[original$CC328_5 == "Yes"] <- 0
df$military[original$CC328_5 == "No"] <- 1

#Binary union varaible. 0 = I am not a union member and no one in my family is, 1 = either I am or someone in my family is a union member (or both)
df$union <- rep(NA, 32800)
df$union[original$CC329 == "I and no one in household union member"] <-
  0
df$union[original$CC329 == "I am a union member, no one in house in union"] <-
  1
df$union[original$CC329 == "I'm not in union, house member in union"] <-
  1
df$union[original$CC329 == "I and house member in union"] <- 1

#Citizen variables
#Immigrant at all?
df$immigrant <- rep(NA, 32800)
df$immigrant[df$citizen == "Immigrant Citizen" |
               df$citizen == "Immigrant non-citizen"] <- 1
df$immigrant[df$citizen == "First generation" |
               df$citizen == "Second generation" |
               df$citizen == "Third generation"] <- 0

#Respondent, parents, and grandparents all born in US
df$thirdgen <- rep(0, 32800)
df$thirdgen[is.na(df$citizen) == TRUE] <- NA
df$thirdgen[df$citizen == "Third generation"] <- 1

#immigration scale
df$immigrantscale <- rep(NA, 32800)
df$immigrantscale[df$citizen == "Immigrant Citizen"] <- 4
df$immigrantscale[df$citizen == "Immigrant non-citizen"] <- 5
df$immigrantscale[df$citizen == "First generation"] <- 3
df$immigrantscale[df$citizen == "Second generation"] <- 2
df$immigrantscale[df$citizen == "Third generation"] <- 1

#homeowner binary variable
df$homeowner <- rep(0, 32800)
df$homeowner[is.na(original$CC333) == TRUE] <- NA
df$homeowner[original$CC333 == "I own my apartment or house"] <- 1

#Political involvement, 1 = any at all
df$polinvolvement <- rep(0, 32800)
df$polinvolvement[original$CC415_1 == "Yes" |
                    original$CC415_2 == "Yes" |
                    original$CC415_3 == "Yes" |
                    original$CC415_4 == "Yes" |
                    original$CC415_5 == "Yes" | original$CC415_6 == "Yes"] <- 1

# SUBSET DATA-FRAMES #################################
# "df" has all observations. Now subsetting, where df.prim includes only respondents who voted in the primary. df.prim2 includes respondents who voted for either Clinton or Obama in the primary. df.primdem includes respondents who voted in the primary and who are registered as Democrat

df.prim <- subset(df, primaryvotechoice != "NA")
df.prim2 <-
  subset(df.prim, primaryvotechoice == "Hillary Clinton" |
           primaryvotechoice == "Barack Obama")
df.primdem <- subset(df.prim, partyid == "Democrat")

# SUMMARY STATISTICS #################################
#sumstats<-data.frame(df$female, )
#summary(df$Obama)
#summary(df.prim$Obama)
#summary(df.prim2$Obama)

#summary(df)
#summary(df.prim)
#summary(df.prim2)

#table(final$black*final$affirmscale, final$Obama)
#table(final$female*final$abortscale, final$Obama)

# Create matrices for summary statistics of three data-frames

mat <- matrix(ncol = 18, nrow = 7)
mat[1,1] <- "Var"
mat[2,1] <- "Min"
mat[3,1] <- "Max"
mat[4,1] <- "Median"
mat[5,1] <- "Mean"
mat[6,1] <- "NAs"
mat[7,1] <- "Obs"

df.mat <- mat
df.prim.mat <- mat
df.prim2.mat <- mat

sum.df.mat <-
  data.frame(
    df$Obama, df$female, df$black, df$hispanic, df$asian, df$othernonwhite, df$age, df$incomecat, df$married, df$partyscale, df$edscale, df$immigrant, df$military, df$withdraw, df$bailout, df$affirmscale, df$abortscale
  )
sum.df.prim.mat <-
  data.frame(
    df.prim$Obama, df.prim$female, df.prim$black, df.prim$hispanic, df.prim$asian, df.prim$othernonwhite, df.prim$age, df.prim$incomecat, df.prim$married, df.prim$partyscale, df.prim$edscale, df.prim$immigrant, df.prim$military, df.prim$withdraw, df.prim$bailout, df.prim$affirmscale, df.prim$abortscale
  )
sum.df.prim2.mat <-
  data.frame(
    df.prim2$Obama, df.prim2$female, df.prim2$black, df.prim2$hispanic, df.prim2$asian, df.prim2$othernonwhite, df.prim2$age, df.prim2$incomecat, df.prim2$married, df.prim2$partyscale, df.prim2$edscale, df.prim2$immigrant, df.prim2$military, df.prim2$withdraw, df.prim2$bailout, df.prim2$affirmscale, df.prim2$abortscale
  )

for (i in 1:17) {
  x <- sum.df.mat[,i]
  df.mat[1,i + 1] <- colnames(sum.df.mat)[i]
  df.mat[2,i + 1] <- min(na.omit(x))
  df.mat[3,i + 1] <- max(na.omit(x))
  df.mat[4,i + 1] <- median(na.omit(x))
  df.mat[5,i + 1] <- mean(na.omit(x))
  df.mat[6,i + 1] <- sum(is.na(x))
  df.mat[7,i + 1] <- length(x)
}

for (i in 1:17) {
  x <- sum.df.prim.mat[,i]
  df.prim.mat[1,i + 1] <- colnames(sum.df.prim.mat)[i]
  df.prim.mat[2,i + 1] <- min(na.omit(x))
  df.prim.mat[3,i + 1] <- max(na.omit(x))
  df.prim.mat[4,i + 1] <- median(na.omit(x))
  df.prim.mat[5,i + 1] <- mean(na.omit(x))
  df.prim.mat[6,i + 1] <- sum(is.na(x))
  df.prim.mat[7,i + 1] <- length(x)
}

for (i in 1:17) {
  x <- sum.df.prim2.mat[,i]
  df.prim2.mat[1,i + 1] <- colnames(sum.df.prim2.mat)[i]
  df.prim2.mat[2,i + 1] <- min(na.omit(x))
  df.prim2.mat[3,i + 1] <- max(na.omit(x))
  df.prim2.mat[4,i + 1] <- median(na.omit(x))
  df.prim2.mat[5,i + 1] <- mean(na.omit(x))
  df.prim2.mat[6,i + 1] <- sum(is.na(x))
  df.prim2.mat[7,i + 1] <- length(x)
}

(88+59+20+7)/(577+350+41+18+174)
sum(final$black==1 & final$Obama==0)/sum(final$black==1)
sum(final[is.na(final$affirmscale)==FALSE,]$black==1 & final[is.na(final$affirmscale)==FALSE,]$Obama==0 & (final[is.na(final$affirmscale)==FALSE,]$affirmscale)==1)/sum(final[is.na(final$affirmscale)==FALSE,]$black==1 & final[is.na(final$affirmscale)==FALSE,]$Obama==0)
sum(final[is.na(final$affirmscale)==FALSE,]$black==1 & final[is.na(final$affirmscale)==FALSE,]$Obama==0 & (final[is.na(final$affirmscale)==FALSE,]$affirmscale)==2)/sum(final[is.na(final$affirmscale)==FALSE,]$black==1 & final[is.na(final$affirmscale)==FALSE,]$Obama==0)
sum(final[is.na(final$affirmscale)==FALSE,]$black==1 & final[is.na(final$affirmscale)==FALSE,]$Obama==0 & (final[is.na(final$affirmscale)==FALSE,]$affirmscale)==3)/sum(final[is.na(final$affirmscale)==FALSE,]$black==1 & final[is.na(final$affirmscale)==FALSE,]$Obama==0)
sum(final[is.na(final$affirmscale)==FALSE,]$black==1 & final[is.na(final$affirmscale)==FALSE,]$Obama==0 & (final[is.na(final$affirmscale)==FALSE,]$affirmscale)==4)/sum(final[is.na(final$affirmscale)==FALSE,]$black==1 & final[is.na(final$affirmscale)==FALSE,]$Obama==0)


#write.csv(df.mat, file="SummaryStatistics_df.csv")
#write.csv(df.prim.mat, file="SummaryStatistics_dfprim.csv")
#write.csv(df.prim2.mat, file="SummaryStatistics_dfprim2.csv")

# REGRESSIONS #################################

# From here on out, I'm just looking at respondents who voted for either HRC or Obama

final <- df.prim2
final$race2 <- rep(NA, length(final[,1]))
final$race2[final$race == "White"] <- "White"
final$race2[final$race == "Black"] <- "Black"
final$race2[final$race == "Hispanic"] <- "Hispanic"

#correlation -- used na.omit, is that okay?
corr <- cor(na.omit(sum.df.prim2.mat))
write.csv(corr, file="correlation.csv")

simplemodel=glm(Obama ~ black + female, data=final)

model1 <-
  glm(
    Obama ~ female + black + hispanic + asian + othernonwhite + age + incomecat + married + partyscale + edscale + immigrant + military + withdraw + bailout + affirmscale + abortscale, data =
      final
  )

model2 <-
  glm(
    Obama ~ female + black + hispanic + asian + othernonwhite + age + incomecat + affirmscale + abortscale + female *
      black + female * hispanic + female * asian + female * othernonwhite + black *
      affirmscale + female * abortscale, data = final
  )

model3 <-
  glm(
    Obama ~ female + black + hispanic + age + incomecat + affirmscale + abortscale + female *
      black + black * affirmscale, data = final
  )

#state fixed effects
#model4<-glmer(Obama ~ (1|female) + (1|black) + (1|hispanic) + (1|asian) + (1|othernonwhite) + (1|age) + (1|incomecat) + (1|affirmscale) + (1|abortscale) + (state), data = final, family=binomial)

#model5<-glmer(Obama ~ (female) + (black) + (hispanic) + (asian) + (othernonwhite) + (age) + (incomecat) + (affirmscale) + (abortscale) + (female*black) + (female*hispanic) + (female*asian) + (female*othernonwhite) + (black*affirmscale) + (female*abortscale) + (1|state), data = final, family=binomial)

# ADD IN STATE EFFECTS #################################
f = file.choose()
census <- read.csv(f, header = TRUE)

n <- length(final[,1])
m <- length(census[,1])
final$Prim.Date.Level <- rep(NA, n)
final$Prim.Date.Category <- rep(NA, n)
final$Percent.Black <- rep(NA, n)
final$Percent.Black.Level <- rep(NA, n)
final$Percent.Black.Category <- rep(NA,n)
final$After.Race.Speech <- rep(NA,n)
final$After.Super.Tues <- rep(NA,n)
final$Prim.Date <- rep(NA,n)

for (i in 1:m) {
  a <- census$Prim.Date.Category.Num[i]
  b <- census$Percent.Black[i]
  c <- census$Prim.Date.Level[i]
  d <- census$Percent.Black.Level[i]
  e <- census$Percent.Black.Category[i]
  f <- census$After.Race.Speech[i]
  g <- census$After.Super.Tues[i]
  h <- (census$Prim.Date[i])
  
  for (j in 1:n) {
    if (final$state[j] == census$State[i]) {
      final$Prim.Date.Category[j] <- a
      final$Percent.Black[j] <- b
      final$Prim.Date.Level[j] <- c
      final$Percent.Black.Level[j] <- d
      final$Percent.Black.Category[j] <- e
      final$After.Race.Speech[j] <- f
      final$After.Super.Tues[j] <- g
      final$Prim.Date[j] <- h
    }
  }
}

# Regressions with State effects #################################

model4 <-
  glm(
    Obama ~ female + black + hispanic + age + incomecat + affirmscale + abortscale + Percent.Black + Prim.Date.Level + Percent.Black *
      black + Prim.Date.Level * black + female * black + Prim.Date.Category + Prim.Date.Category *
      black, data = final
  )
model5 <-
  glm(
    Obama ~ female + black + hispanic + age + incomecat + affirmscale + abortscale + Percent.Black + Percent.Black *
      black + female * black + Prim.Date.Category + Prim.Date.Category * black, data = final
  )
finalmodel <-
  glm(
    Obama ~ female + black + hispanic + age + incomecat + affirmscale + abortscale + Percent.Black + Percent.Black *
      black + black * female, data = final
  )

# Testing Model #################################
meanlist <- rep(NA, 100)
for (i in 1:100) {
  samp <- sample_frac(final, 1)
  n = length(samp[,1])
  df80 <- samp[1:(.8 * n),]
  df20 <- samp[((.8 * n) + 1):n,]
  finalmodel.test <-
    glm(
      Obama ~ female + black + hispanic + edscale + age + affirmscale + abortscale + Percent.Black + Percent.Black *
        black + black * female, data = df80
    )
  #finalmodel.test<-glm(Obama ~ female + black + hispanic + age + affirmscale + abortscale + Percent.Black + Percent.Black*black + black*female + edscale + married, data = final)
  newdata.test <- df20
  #newdata.test$pred<-predict(finalmodel.test, newdata = newdata.test, type = "response")
  newdata.test$pred <-
    predict(finalmodel.test, newdata = newdata.test, type = "response")
  
  newdata.test$pred2 <- rep(NA, .2 * n)
  newdata.test$pred2[newdata.test$pred > .5000] = 1
  newdata.test$pred2[newdata.test$pred < .5000] = 0
  
  newdata.test$dif <- rep(NA, .2 * n)
  newdata.test$dif = newdata.test$pred2 - newdata.test$Obama
  #dif=newdata.test$pred2-newdata.test$Obama
  
  test.df <-
    data.frame(newdata.test$pred, newdata.test$pred2, newdata.test$Obama, newdata.test$dif)
  
  meanlist[i] = sum(na.omit(test.df$newdata.test.dif == 0)) / length(na.omit(test.df$newdata.test.dif))
}
mean(meanlist)

# Statistics by section#################################

#Chapter 5 Section 2, summary statistics
#summary(final$Obama)
#summary(final$female)
#summary(final$black)
#summary(final$hispanic)
#summary(final$asian)
#summary(final$othernonwhite)
#summary(final$age)
#summary(final$incomecat)
#summary(final$affirmscale)
#summary(final$abortscale)


mean(final$Obama) #perent voted BHO
1 - mean(final$Obama) #percent voted HR
mean(final$Obama[final$female == 1]) #percent women voted for BHO
1 - mean(final$Obama[final$female == 1]) #percent women voted for HRC
mean(final$Obama[final$female == 0]) #percent men voted for BHO
1 - mean(final$Obama[final$female == 0]) #percent men voted for HRC
mean(final$Obama[final$black == 1]) #percent blacks voted for BHO
mean(final$Obama[final$white == 1]) #percent whites voted for BHO


sum(final$Obama == 0 &
      final$female == 1 &
      final$white == 1) / sum(final$Obama == 0) #proportion of HRC voters that were white women
sum(final$Obama == 0 &
      final$female == 0 &
      final$white == 1) / sum(final$Obama == 0) #proportion of HRC voters that were white men
sum(final$Obama == 0 &
      final$female == 1 &
      final$black == 1) / sum(final$Obama == 0) #proportion of HRC voters that were black women
sum(final$Obama == 0 &
      final$female == 0 &
      final$black == 1) / sum(final$Obama == 0) #proportion of HRC voters that were black men

sum(final$Obama == 1 &
      final$female == 1 &
      final$white == 1) / sum(final$Obama == 1) #proportion of BHO voters that were white women
sum(final$Obama == 1 &
      final$female == 0 &
      final$white == 1) / sum(final$Obama == 1) #proportion of BHO voters that were white men
sum(final$Obama == 1 &
      final$female == 1 &
      final$black == 1) / sum(final$Obama == 1) #proportion of BHO voters that were black women
sum(final$Obama == 1 &
      final$female == 0 &
      final$black == 1) / sum(final$Obama == 1) #proportion of BHO voters that were black men

#male-female proportions by race
men.mat = matrix(nrow = 3, ncol = 5)
men.mat[1,1] = "MEN"
men.mat[2,1] = "BHO"
men.mat[3,1] = "HRC"
men.mat[1,2] = "White"
men.mat[1,3] = "Black"
men.mat[1,4] = "Hispanic"
men.mat[1,5] = "Asian"
men.mat[2,2] = sum(final$Obama == 1 &
                     final$race == "White" &
                     final$female == 0) / sum(final$race == "White" & final$female == 0)
men.mat[2,3] = sum(final$Obama == 1 &
                     final$race == "Black" &
                     final$female == 0) / sum(final$race == "Black" & final$female == 0)
men.mat[2,4] = sum(final$Obama == 1 &
                     final$race == "Hispanic" &
                     final$female == 0) / sum(final$race == "Hispanic" &
                                                final$female == 0)
men.mat[2,5] = sum(final$Obama == 1 &
                     final$race == "Asian" &
                     final$female == 0) / sum(final$race == "Asian" & final$female == 0)
men.mat[3,2] = sum(final$Obama == 0 &
                     final$race == "White" &
                     final$female == 0) / sum(final$race == "White" & final$female == 0)
men.mat[3,3] = sum(final$Obama == 0 &
                     final$race == "Black" &
                     final$female == 0) / sum(final$race == "Black" & final$female == 0)
men.mat[3,4] = sum(final$Obama == 0 &
                     final$race == "Hispanic" &
                     final$female == 0) / sum(final$race == "Hispanic" &
                                                final$female == 0)
men.mat[3,5] = sum(final$Obama == 0 &
                     final$race == "Asian" &
                     final$female == 0) / sum(final$race == "Asian" & final$female == 0)
men.mat

women.mat = matrix(nrow = 3, ncol = 5)
women.mat[1,1] = "WOMEN"
women.mat[2,1] = "BHO"
women.mat[3,1] = "HRC"
women.mat[1,2] = "White"
women.mat[1,3] = "Black"
women.mat[1,4] = "Hispanic"
women.mat[1,5] = "Asian"
women.mat[2,2] = sum(final$Obama == 1 &
                       final$race == "White" &
                       final$female == 1) / sum(final$race == "White" & final$female == 1)
women.mat[2,3] = sum(final$Obama == 1 &
                       final$race == "Black" &
                       final$female == 1) / sum(final$race == "Black" & final$female == 1)
women.mat[2,4] = sum(final$Obama == 1 &
                       final$race == "Hispanic" &
                       final$female == 1) / sum(final$race == "Hispanic" &
                                                  final$female == 1)
women.mat[2,5] = sum(final$Obama == 1 &
                       final$race == "Asian" &
                       final$female == 1) / sum(final$race == "Asian" & final$female == 1)
women.mat[3,2] = sum(final$Obama == 0 &
                       final$race == "White" &
                       final$female == 1) / sum(final$race == "White" & final$female == 1)
women.mat[3,3] = sum(final$Obama == 0 &
                       final$race == "Black" &
                       final$female == 1) / sum(final$race == "Black" & final$female == 1)
women.mat[3,4] = sum(final$Obama == 0 &
                       final$race == "Hispanic" &
                       final$female == 1) / sum(final$race == "Hispanic" &
                                                  final$female == 1)
women.mat[3,5] = sum(final$Obama == 0 &
                       final$race == "Asian" &
                       final$female == 1) / sum(final$race == "Asian" & final$female == 1)
women.mat

# PREDICTED PROBABILITIES #################################
finalmodel <-
  glm(
    Obama ~ female + black + hispanic + age + incomecat + affirmscale + abortscale + Percent.Black + Percent.Black *
      black + black * female, data = final
  )

testdf = final
final$testpred <-
  (predict(finalmodel, newdata = testdf, type = "response"))
coefficients <- coef(finalmodel)
logodds <- exp(coef(finalmodel))
probabilities <- exp(coef(finalmodel)) / (1 + exp(coef(finalmodel)))

exp((coefficients[1])+coefficients[2]+coefficients[3]+coefficients[4])+coefficients[5]+coefficients[6]+coefficients[7]+coefficients[8]+coefficients[9])


mean(na.omit(final$testpred))
sd(na.omit(final$testpred))

# Controlling for all but race and gender
df.race.gender = with(
  final, data.frame(
    female = female, black = black, hispanic = hispanic, age = mean(age), incomecat =
      mean(na.omit(incomecat)), affirmscale = (mean(na.omit(affirmscale))), abortscale =
      mean(na.omit(abortscale)), Percent.Black = mean(na.omit(Percent.Black))
  )
)
df.race.gender$pred = predict(finalmodel, newdata = df.race.gender, type =
                                "response")
m <- length(df.race.gender[,1])
df.race.gender$race <- rep("White", m)
df.race.gender$race[df.race.gender$black == 1] <- "Black"
df.race.gender$race[df.race.gender$hispanic == 1] <- "Hispanic"


# Controlling for all but race, gender, and age
df.age = with(
  final, data.frame(
    female = female, black = black, hispanic = hispanic, age = (age), incomecat =
      mean(na.omit(incomecat)), affirmscale = (mean(na.omit(affirmscale))), abortscale =
      mean(na.omit(abortscale)), Percent.Black = mean(na.omit(Percent.Black))
  )
)
df.age$pred = predict(finalmodel, newdata = df.age, type = "response")
m <- length(df.age[,1])
df.age$race <- rep("White", m)
df.age$race[df.age$black == 1] <- "Black"
df.age$race[df.age$hispanic == 1] <- "Hispanic"

# Controlling for all but race, gender, and income
df.income = with(
  final, data.frame(
    female = female, black = black, hispanic = hispanic, age = mean(age), incomecat =
      ((incomecat)), affirmscale = (mean(na.omit(affirmscale))), abortscale =
      mean(na.omit(abortscale)), Percent.Black = mean(na.omit(Percent.Black))
  )
)
df.income$pred = predict(finalmodel, newdata = df.income, type = "response")
m <- length(df.income[,1])
df.income$race <- rep("White", m)
df.income$race[df.income$black == 1] <- "Black"
df.income$race[df.income$hispanic == 1] <- "Hispanic"

#State effects
df.state = with(
  final, data.frame(female=female, black=black, hispanic=hispanic, age=mean(age), incomecat = mean(na.omit(incomecat)), affirmscale = (mean(na.omit(affirmscale))), abortscale =
                      mean(na.omit(abortscale)), Percent.Black = (Percent.Black))
)
df.state$pred = predict(finalmodel, newdata = df.state, type = "response")
#write.csv(df.state, file="StateProbabilities.csv")
df.state.bf=subset(df.state, df.state$female == 1 & df.state$black == 1)
df.state.bm=subset(df.state, df.state$female == 0 & df.state$black == 1)
df.state.wm=subset(df.state, df.state$female == 0 & df.state$black == 0  & df.state$hispanic == 0 )
df.state.wf=subset(df.state, df.state$female == 1 & df.state$black == 0  & df.state$hispanic == 0 )

df.state.gg=df.state
df.state.gg$race<-rep("White", length(df.state[,1]))
df.state.gg$race[df.state$black==1]<-"Black"
df.state.gg$race[df.state$hispanic==1]<-"Hispanic"
df.state.gg$gender<-rep("Male", length(df.state[,1]))
df.state.gg$gender[df.state$female==1]<-"Female"

ggplot(data=df.state.gg, aes(x=Percent.Black, y=pred)) + geom_point() + facet_grid(gender~race)

  

# Figures -----------------------------------------------------------------
#Figure 1a: Proportions of Voter Turnout by Race, 750 x 500
{#variables for figure
{
  voted<-rep(0, 32800)
  voted[df$primaryvoteyesprimary=="Yes" | df$primaryvoteyescaucus=="Yes"]<-1
  df$ggvoted<-rep(0, 32800)
  df$ggvoted[voted==1]<-"Voted"
  df$ggvoted[voted==0]<-"Did Not Vote"
  df$ggrace<-rep("Other\n(n=925)",32800)
  df$ggrace[df$race=="White"]="White\n(n=26,971)"
  df$ggrace[df$race=="Black"]="Black\n(n=2000)"
  df$ggrace[df$race=="Hispanic"]="Hispanic\n(n=20000)"
  df$ggrace[df$race=="Asian"]="Asian\n(n=446)"
  df$ggrace[df$race=="Mixed"]="Mixed\n(n=458)"
}
#plot
p.figure1a<-ggplot(df, aes(ggvoted, fill=ggvoted, colour="black")) + scale_colour_manual(guide="none", values="black") +
    geom_bar(aes(y = ..count.. / sapply(PANEL, FUN=function(x) sum(count[PANEL == x])))) +
    facet_wrap(~ggrace, nrow=1, ncol=6) +
    scale_fill_manual(name="Did the Respondent\nVote in the\nPrimaries?", values=c("#003399", "#D5e8F9")) +
    ylab("Proportions of Voter Turnout") + scale_y_continuous(labels = scales::percent) +
    xlab("Vote Behavior by Race") + 
    ggtitle(expression(atop(bold("Figure 1a:"), atop("Proportions of Voter Turnout Within Racial Groups", ""))))
}

#Figure 1b: Proportions of Voter Turnout by Gender
{
p.figure1b<-{ggplot(df, aes(ggvoted, fill=ggvoted, colour="black")) + scale_colour_manual(guide="none", values="black") +
    geom_bar(aes(y = ..count.. / sapply(PANEL, FUN=function(x) sum(count[PANEL == x])))) +
    facet_wrap(~gender, nrow=1, ncol=6) +
    scale_fill_manual(name="Did the Respondent\nVote in the\nPrimaries?", values=c("#003399", "#D5e8F9")) +
    ylab("Proportions of Voter Turnout") + scale_y_continuous(labels = scales::percent) +
    xlab("Vote Behavior by Gender") + 
    ggtitle(expression(atop(bold("Figure 1b:"), atop("Proportions of Voter Turnout Within Gender Groups", ""))))
}
}

#Figure 2a: Proportions of Voter Turnout by Race
{
p.figure2a=ggplot(data=df, aes(x=factor(1), fill=as.factor(race))) + 
  geom_bar(aes(y = ..count.. / sapply(PANEL, FUN=function(x) sum(count[PANEL == x])))) + 
  facet_grid(.~ggvoted)+ coord_polar(theta="y") +
  labs(fill="Race") + 
  ggtitle(expression(atop(bold("Figure 2a:"), atop("Voter Turnout Broken Down by Race", "")))) + 
xlab("") + ylab("Voter Behavior")+ scale_y_continuous(labels = scales::percent)
}

#Figure 2b: Proportions of Voter Turnout by Gender
{p.figure2b<-ggplot(data=df, aes(x=factor(1), fill=as.factor(gender))) + 
  geom_bar(aes(y = ..count.. / sapply(PANEL, FUN=function(x) sum(count[PANEL == x])))) + 
  facet_grid(.~ggvoted)+ coord_polar(theta="y") +
  labs(fill="Gender") + scale_fill_manual(values=c("blue", "pink")) +
  ggtitle(expression(atop(bold("Figure 2b:"), atop("Voter Turnout Broken Down by Gender", "")))) + 
  xlab("") + ylab("Voter Behavior")+ scale_y_continuous(labels = scales::percent)
}

#Figure 3a: Distribution of Party Strength, with Normal Approximations by Vote Category
{
#lines and functions required:
{
df$primparty <- rep(NA, 32800)
df$primparty[df$primaryvotechoice == "Hillary Clinton" |
               df$primaryvotechoice == "Barack Obama" |
               df$primaryvotechoice == "John Edwards" |
               df$primaryvotechoice == "Bill Richardson"] = "Democrat"
df$primparty[df$primaryvotechoice == "John McCain" |
               df$primaryvotechoice == "Mitt Romney" |
               df$primaryvotechoice == "Mike Huckabee" |
               df$primaryvotechoice == "Ron Paul" |
               df$primaryvotechoice == "Rudolph Giuliani"] = "Republican"

ggframe = subset(df, is.na(df$partyscale) == FALSE)
ggframe.dem = subset(df, is.na(df$partyscale) == FALSE & df$primparty == "Democrat")
ggframe.rep = subset(df, is.na(df$partyscale) == FALSE & df$primparty == "Republican")
ggframe.elec = subset(
  df, is.na(df$partyscale) == FALSE &
    is.na(df$preschoice) == FALSE &
    df$preschoice != "I did not vote in this race" &
    df$preschoice != "I did not vote in the election"
)

x = seq(from = 1, to = 7, by = .1)
yfit = function(x) {dnorm(x, mean = mean(na.omit(ggframe$partyscale)), sd = sd(na.omit(ggframe$partyscale)))}
yfit.dem = function(x) {dnorm(x, mean = mean(na.omit(ggframe.dem$partyscale)), sd = sd(na.omit(ggframe.dem$partyscale)))}
yfit.rep = function(x) {dnorm(x, mean = mean(na.omit(ggframe.rep$partyscale)), sd = sd(na.omit(ggframe.rep$partyscale)))}
yfit.elec = function(x) {dnorm(x, mean = mean(na.omit(ggframe.elec$partyscale)), sd = sd(na.omit(ggframe.elec$partyscale)))}
yfit.prim = function(x) {dnorm(x, mean = mean(na.omit(df.prim$partyscale)), sd = sd(na.omit(df.prim$partyscale)))}
yfit.hrc = function(x) {dnorm(x, mean = mean(na.omit(df.prim$partyscale[df.prim$primaryvotechoice == "Hillary Clinton"])), sd = sd(na.omit(df.prim$partyscale[df.prim$primaryvotechoice =="Hillary Clinton"])))}
yfit.bho = function(x) {dnorm(x, mean = mean(na.omit(df.prim$partyscale[df.prim$primaryvotechoice =="Barack Obama"])), sd = sd(na.omit(df.prim$partyscale[df.prim$primaryvotechoice =="Barack Obama"])))}

}
#figure
{
p.figure3a=ggplot(data = ggframe, aes(x = partyscale)) +
  geom_histogram(aes(y = ..density.., alpha = .2, fill = ..x..), binwidth =1, origin = -.5) +
  scale_x_continuous(breaks = 1:7) +
  stat_function(fun = yfit, lwd = 2, aes(colour = "All Respondents")) +
  stat_function(fun = yfit.dem, lwd = 2, aes(colour = "Voted Democrat in Primary")) +
  stat_function(fun = yfit.rep, lwd = 2, aes(colour = "Voted Republican in Primary")) +
  ggtitle(expression(atop(bold("Figure 3a:"), atop("Distribution of Party Strength, with Normal Approximations by Vote Category","")))) +
  scale_alpha(guide = "none") +
  scale_colour_manual("Category", values = c("black", "blue", "red")) +
  ylab("Density") +
  scale_fill_gradient("Count", low = "blue", high = "red", limits = c(1,7), name = "name") +
  xlab(expression(atop("Party Strength", atop("1=Strong Democrat, 4=Independent, 7=Strong Republican", ""))))
}
}

#Figure 3b: Distribution of Party Strength of Democrats, with Normal Approximations by Vote Category
{p.figure3b=
  ggplot(data = ggframe[ggframe$primparty == "Democrat",], aes(x = partyscale)) +
  geom_histogram(aes(y = ..density.., alpha = .2, fill = ..x..), binwidth = 1, origin =
                   -.5) +
  scale_x_continuous(breaks = 1:7) +
  stat_function(fun = yfit, lwd = 2, aes(colour = "All Respondents")) +
  stat_function(fun = yfit.dem, lwd = 2, aes(colour = "Voted Democrat in Primary")) +
  stat_function(fun = yfit.hrc, lwd = 2, aes(colour = "Voted for Clinton in Primary")) +
  stat_function(fun = yfit.bho, lwd = 2, aes(colour = "Voted for Obama in Primary")) +
  scale_colour_manual("Category", values = c("black", "blue", "pink", "purple")) +
  ggtitle(expression(atop(
    bold("Figure 3b:"), atop("Distribution of Party Strength out of Democrats, with Normal Approximations by Vote Category", "")
  ))) +
  scale_alpha(guide = "none") +
  ylab("Density") +
  scale_fill_gradient(
    "Count", low = "blue", high = "red", limits = c(1,7), name = "name"
  ) +
  xlab(expression(atop(
    "Party Strength", atop("1=Strong Democrat, 4=Independent, 7=Strong Republican", "")
  )))
}

#Figure 4a: Voter Choice Broken Down by Race
{p.figure4a=ggplot(data=final, aes(x=factor(1), fill=as.factor(race))) + 
  geom_bar(aes(y = ..count.. / sapply(PANEL, FUN=function(x) sum(count[PANEL == x])))) + 
  facet_grid(.~primaryvotechoice)+ coord_polar(theta="y") +
  labs(fill="Race") + 
  ggtitle(expression(atop(bold("Figure 4a:"), atop("Voter Choice Broken Down by Race", "")))) + 
  xlab("") + ylab("Voter Choice")+ scale_y_continuous(labels = scales::percent)
}

#Figure 4b: Voter Choice Broken Down by Gender
{p.figure4b=ggplot(data=final, aes(x=factor(1), fill=as.factor(gender))) + 
  geom_bar(aes(y = ..count.. / sapply(PANEL, FUN=function(x) sum(count[PANEL == x])))) + 
  facet_grid(.~primaryvotechoice)+ coord_polar(theta="y") +
  labs(fill="Gender") + 
  scale_fill_manual(values=c("blue", "pink"))+
  ggtitle(expression(atop(bold("Figure 4b:"), atop("Voter Choice Broken Down by Gender", "")))) + 
  xlab("") + ylab("Voter Choice")+ scale_y_continuous(labels = scales::percent)
}

#Figure 5: Proportions of Voter Turnout Within Racial Groups, Separated by Gender
{p.figure5=ggplot(df.prim2, aes(x=gender, fill=primaryvotechoice)) + 
  geom_bar(position="fill") + facet_grid(. ~ race) + 
  ggtitle(expression(atop(bold("Figure 5:"), atop("Proportions of Vote Choice within Racial Groups, Separated by Gender", "")))) +
  xlab("Gender") + labs(fill="Vote Choice") + ylab("Proportions")
}

#Figure 6 is regression output

#Figure 7: Predicted Probabilities of Voting for Obama Over Clinton, 854 x 357
{
p.figure7 <-
  ggplot(data = final, aes(x = testpred)) + geom_histogram(aes(fill = "")) +
  scale_fill_manual(values = "#4ECDC4", name = "Total Respondents", guide=FALSE) + 
  geom_vline(xintercept = mean(na.omit(final$testpred))) + 
  xlab("Predicted Probability of Voting for Obama") + 
  ggtitle(expression(atop(bold("Figure 7:"), atop("Histogram of Predicted Probabilities of Voting for Obama", "")))) +
  geom_text(aes(x=.6, label="\nMean Probability", y=100))
}

#Figure 8: Predicted Probabilities of Voting for Obama Over Clinton, Separated by Gender, 854 x 357
{
p.figure8 <-
  ggplot(data = final, aes(x = testpred)) + geom_histogram(aes(fill = factor(gender)), position =
                                                             "dodge") + scale_fill_manual(name =
                                                                                            "Gender", values = c("#7f7fff", "#ff99d6")) + geom_vline(xintercept = mean(na.omit(final$testpred))) + xlab("Predicted Probability of Voting for Obama") + 
  ggtitle(expression(atop(bold("Figure 8:"), atop("Histogram of Predicted Probabilities of Voting for Obama, by Gender", "")))) +
  geom_text(aes(x=.6, label="\nMean Probability", y=50))
}

#Figure 9: Predicted Probabilities of Voting for Obama Over Clinton, Separated by Race, 854 x 357
{
p.figure9 <-
  ggplot(data = final[is.na(final$race2) == FALSE,], aes(x = testpred)) + geom_histogram(aes(fill = factor((na.omit(
    race2
  )))), position =
    "dodge") + geom_vline(xintercept = mean(na.omit(final$testpred))) + xlab("Predicted Probability of Voting for Obama") + 
  ggtitle(expression(atop(bold("Figure 9:"), atop("Histogram of Predicted Probabilities of Voting for Obama, by Race", "")))) +
  scale_fill_manual(name =  "Race", values = c("#4ECDC4", "#556270", "#FF6B6B")) +
geom_text(aes(x=.6, label="\nMean Probability", y=50))
}

#Figure 10: Predicted Probabilities of Voting for Obama Over Clinton, Broken Down by Gender and Race, 854 x 507
{
  p.figure10 = ggplot(df.race.gender, aes(x = factor(female), fill = factor(female))) +
    geom_bar(aes(y = pred), stat = "identity", position = "dodge") +
    scale_fill_manual(
      name = "Gender", breaks = c(0,1),labels = c("Male", "Female"), values =
        c("blue", "pink")
    ) +
    facet_grid(. ~ race) + xlab("Gender") + ylab("Predicted Probability of Voting for Obama") +
    theme(axis.text.x = element_blank()) + geom_hline(yintercept = .5) + scale_y_continuous(limits =
                                                                                              c(0,1)) +
  ggtitle(expression(atop(bold("Figure 10:"), atop("Predicted Probabilities of Voting for Obama by Race and Gender", atop(italic("Holding All Other Variables at Mean Value", ""))))))

}

#Figure 11: Predicted Probabilities of Voting for Obama Over Clinton, Broken Down by Gender, Race, and Age, 854 x 507
{
p.figure11 = ggplot(df.age, aes(x = age)) +
  geom_point(aes(y = pred, colour = factor(female))) +
  scale_colour_manual(
    name = "Gender", breaks = c(0,1),labels = c("Male", "Female"), values =
      c("blue", "pink")
  ) +
  facet_grid(. ~ race) + xlab("Age") + ylab("Predicted Probability of Voting for Obama") +
  geom_hline(yintercept = .5) + scale_y_continuous(limits =
                                                     c(0,1)) +
  ggtitle(expression(atop(bold("Figure 11:"),
    atop(
      "Predicted Probabilities of Voting for Obama Over Clinton, Broken Down by Gender, Race, and Age", atop(italic(
        "Holding All Other Variables at Mean Value", ""
      ))
    )
  )))
}

#Figure 12: Predicted Probabilities of Voting for Obama Over Clinton, Broken Down by Gender, Race, and Income, 854 x 507
{
p.figure12 = ggplot(df.income, aes(x = factor(incomecat))) +
  geom_point(aes(y = pred, colour = factor(female))) +
  scale_colour_manual(
    name = "Gender", breaks = c(0,1),labels = c("Male", "Female"), values =
      c("blue", "pink")
  ) +
  facet_grid(. ~ race) + xlab("Income Category,(from 1=Less Than $10,000 to 14=$150,000 or more)") + ylab("Predicted Probability of Voting for Obama") +
  geom_hline(yintercept = .5) + scale_y_continuous(limits =
                                                     c(0,1)) +
  ggtitle(expression(atop(bold("Figure 12:"),
    atop(
      "Predicted Probabilities of Voting for Obama Over Clinton, Broken Down by Gender, Race, and Income", atop(italic(
        "Holding All Other Variables at Mean Value", ""
      ))
    ))
  ))
}


#Figure 13: Predicted Probabilities broken down by Percent Black
{final2<-final
final2$point.white.m<-rep(NA, length(final[,1]))
final2$point.black.m<-rep(NA, length(final[,1]))
final2$point.white.f<-rep(NA, length(final[,1]))
final2$point.black.f<-rep(NA, length(final[,1]))
for(i in 1:length(final[,1])){
  final2$point.white.f[i]<-mean(na.omit(final$testpred[final$white==1 & final$female==1 & final$Percent.Black==final$Percent.Black[i]]))
  final2$point.black.f[i]<-mean(na.omit(final$testpred[final$black==1 & final$female==1 & final$Percent.Black==final$Percent.Black[i]]))
  final2$point.white.m[i]<-mean(na.omit(final$testpred[final$white==1 & final$female==0 & final$Percent.Black==final$Percent.Black[i]]))
  final2$point.black.m[i]<-mean(na.omit(final$testpred[final$black==1 & final$female==0 & final$Percent.Black==final$Percent.Black[i]]))
}
p.figure14<-
 {ggplot(data = final2, aes(((Percent.Black)))) +
   geom_point(aes(y = testpred), colour = "#7f95ff") +
   geom_line(aes(y = point.white.f), colour="white", size=2.5) +  
   geom_line(aes(y = point.black.f), colour="black", size=2.5) + 
   geom_line(aes(y = point.white.m), colour="white", size=2.5) +  
   geom_line(aes(y = point.black.m), colour="black", size=2.5) + 
   geom_point(aes(y = point.white.f), shape=21, colour="white", fill="pink", size=3) + 
   geom_point(aes(y = point.black.f), shape=21, colour="black", fill="pink", size=3) +
   geom_point(aes(y = point.white.m), shape=21, colour="white", fill="blue", size=3) +
   geom_point(aes(y = point.black.m), shape=21, colour="black", fill="blue", size=3) + 
   ylab("Probability of Voting for Obama") + xlab("% of Blacks Living in State") +
     xlim(0,.4) +
     ggtitle(expression(atop(bold("Figure 14:"), atop("Predicted Probabilities of Voting for Obama, by Percentage of Blacks Living in State", "")))) 
}
}

#Figure 14: Predicted Probabilities broken down by Percent Black
{
df.state.gg=df.state
df.state.gg$race<-rep("White", length(df.state[,1]))
df.state.gg$race[df.state$black==1]<-"Black"
df.state.gg$race[df.state$hispanic==1]<-"Hispanic"
df.state.gg$gender<-rep("Male", length(df.state[,1]))
df.state.gg$gender[df.state$female==1]<-"Female"

ggplot(data = df.state.gg, aes(x = Percent.Black, y = pred)) + 
  facet_grid(gender ~ race) + 
  scale_y_continuous(labels = scales::percent, limits=c(.3, .8)) +
  scale_x_continuous(labels = scales::percent, limits=c(0,.5)) +
  ylab("Predicted Probability of Voting for Obama") + 
  xlab("Percentage of State Population Identifying as Black") + 
  geom_line() +
  geom_point(aes(colour=factor(female))) + 
  scale_colour_manual(values=c("blue", "pink"), name="Gender", labels="Male", "Female") 
}

#Figure 15: Predicted Probabilities broken down by State date primary
{
point<-rep(NA, length(final[,1]))
for(i in 1:19){
  point[final$Prim.Date.Level==i]<-mean(na.omit(final$testpred[final$Prim.Date.Level==i]))
}
ggframe<-data.frame(final, point)

p.figure14<-
  ggplot(data = ggframe, aes(factor(Prim.Date.Level))) +
  geom_point(aes(y = testpred), colour = "#7f95ff") +
  geom_point(
    aes(y = point, colour = "Mean Predicted Probablity\nof Voting for Obama\nby Primary Date Order"), size = 3) +
  scale_colour_manual(values = c("#FF8A63"), name = "") + 
  geom_line(aes(x = Prim.Date.Level, y = point)) + 
  xlab("Order or Primary Date") + 
  ylab("Probability of Voting for Obama") +
  geom_vline(xintercept=7, colour="black") + 
  geom_vline(xintercept=14.5, colour="black") +
  geom_text(aes(x=7, label="\nSuper Tuesday", y=1.1)) +
  geom_text(aes(x=14.5, label="\nObama's Speech\non Race", y=1.1)) +
  ggtitle(expression(atop(bold("Figure 14:"), atop("Predicted Probabilities of Voting for Obama, by Order of Primary Date", ""))))

}





