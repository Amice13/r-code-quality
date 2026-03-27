# Install Packages (optional) ---------------------------------------------
#install.packages(c("haven",
#"tidyverse",
#"dplyr",
#"EdSurvey",
#"stringr", 
#"SnowballC",
#"psych",
#"stargazer",
#"purrr",
#"ggplot2",
#"reshape2",
# "lme4",
#"performance",
#"BIFIEsurvey",
#"nortest",
#"dplyr",
#"tidyr",
#"intsvy"))


# load packages and data --------------------------------------------------
library(haven)
library(tidyverse)
library(dplyr)
library(EdSurvey)



# Set your root-directory and the path where the OECD-data will be --------
root <- "~/PISA/"
setwd(root)
path <- paste0(root,"your_path/")


# Download PISA_DATA ------------------------------------------------------
variable_names <- c("cntstuid",
                    "cntschid",
                    "cnt",
                    "cntryid",
                    "escs",
                    
                    "bfmj2",
                    "disclisci",
                    "immig",
                    "langn",
                    "sc061q02ta",
                    "sc061q07ta",
                    
                    "schaut",
                    
                    "lead",
                    "w_fstuwt",
                    "w_schgrnrabwt",
                    
                    "sc035q01na", "sc035q02ta", "sc035q03ta",
                    "sc035q04ta", "sc035q05ta", "sc035q06ta",
                    
                    "st038q01na","st038q02na","st038q03na",
                    "st038q04na","st038q05na","st038q06na",
                    "st038q07na","st038q08na",
                    "st034q01ta","st034q02ta","st034q03ta",
                    "st034q04ta","st034q05ta","st034q06ta",
                    
                    "st034q01ta","st034q02ta","st034q03ta",
                    "st034q04ta","st034q05ta","st034q06ta",
                    
                    "natcen")

for(i in seq(1,80)){
  v <- paste0("w_fsturwt",as.character(i))
  variable_names <- append(variable_names,v)
}

for(i in seq(1,10)){
  v <- paste0("pv",as.character(i),"scie")
  variable_names <- append(variable_names,v)
  v <- paste0("pv",as.character(i),"read")
  variable_names <- append(variable_names,v)
  v <- paste0("pv",as.character(i),"math")
  variable_names <- append(variable_names,v)
}


countries <- c("AUS","AUT","DEU","CAN","CHE","SWE","FIN","FRA","GBR",
               "HKG", "ITA","JPN","KOR","SGP","POL","RUS",
               "SVK","SVN","USA")
y <- 2015
for (country in countries) {
  #setwd(pate0(path,"2015/"))
  
  PISAtest <- readPISA(path = paste0(path,y,"/"), countries =country,
                       cognitive = "response")
  PISA <- as_tibble(EdSurvey::getData(data = PISAtest, 
                                      varnames = variable_names,
                                      addAttributes = T, omittedLevels =F))
  
  PISA <- PISA %>% mutate(year = y) 
  
  if (country == "GBR"){
    PISA <- PISA %>% filter(natcen == "UNITED KINGDOM - EXCL. SCOTLAND")
  }
  
  setwd(paste0(root,"prepared_data/2015/"))
  write.csv(PISA, paste0(country,"_",y,".csv"))
}

# Read PISA Data for 2009 -------------------------------------------------
# Dependent Variables
# pv1scie --> Plausible Value 1 for Science score
# pv1read --> Plausible Value 1 for Reading score
# pv1math --> Plausible Value 1 for Math Score

# We need: ESCS = escs
# ISEI (Father) = BFMJ 
# Disciplinary Climate = disclima
# Migration background = immig
# language at home = test language = langn
# Teacher absenteeism = SC17Q06 
# Student absenteeism = SC17Q02


# School: Accountability = composite of sc16 items
## sc16q01 = Assessments - Child's progress
## sc16q02 = Assessments - Student promotion
## sc16q03 = Assessments - Instruction 
## sc16q04 = Assessments - National performance
## sc16q05 = Assessments - School's progress
## sc16q06 = Assessments - Teachers 
## sc16q07 = Assessments - Curriculum 
## sc16q08 = Assessments - Other Schools 


# School autonomy  = composite of sc24 items:
## sc24qa1 = Responsibility teacher hire - principal
## sc24qa2 = Responsibility teacher hire - teachers
## sc24qa3 = Responsibility teacher hire - school governing board
## sc24qa4 = Responsibility teacher hire - regional or local education authority
## sc24qa5 = Responsibility teacher hire - national education authority



## sc24qb1 = Responsibility firing teachers - principal
## sc24qb2 = Responsibility firing teachers - teachers
## sc24qb3 = Responsibility firing teachers - school governing board



## sc24qc1 = Responsibility starting salaries - principal
## sc24qc2 = Responsibility starting salaries - teachers
## sc24qc3 = Responsibility starting salaries - school governing board



## sc24qd1 = Responsibility salary increases - principal
## sc24qd2 = Responsibility salary increases - teachers
## sc24qd3 = Responsibility salary increases - school governing board


## sc24qe1 = Responsibility formulate budget - principal
## sc24qe2 = Responsibility formulate budget - teachers
## sc24qe3 = Responsibility formulate budget - school governing board


## sc24qf1 = Responsibility budget allocation - principal
## sc24qf2 = Responsibility budget allocation - teachers
## sc24qf3 = Responsibility budget allocation - school governing board


## sc24qg1 = Responsibility student discipline - principal
## sc24qg2 = Responsibility student discipline - teachers
## sc24qg3 = Responsibility student discipline - school governing board


## sc24qh1 = Responsibility student assessment - principal
## sc24qh2 = Responsibility student assessment - teachers
## sc24qh3 = Responsibility student assessment - school governing board


## sc24qi1 = Responsibility student admission - principal
## sc24qi2 = Responsibility student admission - teachers
## sc24qi3 = Responsibility student admission - school governing board


## sc24qj1 = Responsibility textbook use - principal
## sc24qj2 = Responsibility textbook use - teachers
## sc24qj3 = Responsibility textbook use - school governing board


## sc24qk1 = Responsibility course content - principal
## sc24qk2 = Responsibility course content - teachers
## sc24qk3 = Responsibility course content - school governing board


## sc24ql1 = Responsibility courses offered - principal
## sc24ql2 = Responsibility courses offered - teachers
## sc24ql3 = Responsibility courses offered - school governing board 


# Educational Leadership = SC26Q items
## SC26Q01 = Professional development
## SC26Q02 = Educational goals - Teachers 
## SC26Q03 = Observe in classrooms 
## SC26Q04 = Student performance 
## SC26Q05 = Give suggestions 
## SC26Q06 = Monitor student's work 
## SC26Q07 = Teacher's problems 
## SC26Q08 = Teachers Updating skills 
## SC26Q09 = Educational Goals - Classroom
## SC26Q10 = Exam results into account 
## SC26Q11 = Curriculum Responsibility 
## SC26Q12 = Classroom problems 
## SC26Q13 = Disruptive behaviour 
## SC26Q14 = Take over lessons 

## SUBNATIO = Country Region if applicable

# Additioanlly:
# Being Bullied = st038-items
# Sense of Belonging = st034-items
# Weights to be applied = w_fstuwt


variable_names <- c("stidstd",
                    "schoolid",
                    "cnt",
                    "country",
                    "escs",
                    "bfmj",
                    "disclima",
                    "immig",
                    "langn",
                    "sc17q02",
                    "sc17q06",
                    
                    "sc16q01","sc16q02","sc16q03",
                    "sc16q04","sc16q05","sc16q06",
                    "sc16q07","sc16q08",
                    
                    #School autonomy  = composite of sc24 items:
                    "sc24qa1","sc24qa2","sc24qa3","sc24qa4","sc24qa5",
                    "sc24qb1","sc24qb2","sc24qb3","sc24qb4","sc24qb5",
                    "sc24qc1","sc24qc2","sc24qc3","sc24qc4","sc24qc5",
                    "sc24qd1","sc24qd2","sc24qd3","sc24qd4","sc24qd5",
                    "sc24qe1","sc24qe2","sc24qe3","sc24qe4","sc24qe5",
                    "sc24qf1","sc24qf2","sc24qf3","sc24qf4","sc24qf5",
                    "sc24qg1","sc24qg2","sc24qg3","sc24qg4","sc24qg5",
                    "sc24qh1","sc24qh2","sc24qh3","sc24qh4","sc24qh5",
                    "sc24qi1","sc24qi2","sc24qi3","sc24qi4","sc24qi5",
                    "sc24qj1","sc24qj2","sc24qj3","sc24qj4","sc24qj5",
                    "sc24qk1","sc24qk2","sc24qk3","sc24qk4","sc24qk5",
                    "sc24ql1","sc24ql2","sc24ql3","sc24ql4","sc24ql5",
                    
                    ## leadership
                    "sc26q01","sc26q02","sc26q03",
                    "sc26q04","sc26q05","sc26q06",
                    "sc26q07","sc26q08","sc26q09",
                    "sc26q10","sc26q11","sc26q12",
                    "sc26q13","sc26q14",
                    "w_fstuwt",
                    
                    "subnatio")


for(i in seq(1,5)){
  v <- paste0("pv",as.character(i),"scie")
  variable_names <- append(variable_names,v)
  v <- paste0("pv",as.character(i),"read")
  variable_names <- append(variable_names,v)
  v <- paste0("pv",as.character(i),"math")
  variable_names <- append(variable_names,v)
}

for(i in seq(1,80)){
  v <- paste0("w_fstr",as.character(i))
  variable_names <- append(variable_names,v)
}

countries <- c("AUS","AUT","DEU","CAN","CHE","SWE","FIN","FRA","GBR",
               "HKG", "ITA","JPN","KOR","SGP","NOR","POL","RUS",
               "SVK","SVN","USA")
y = 2009
for (country in countries) {
  
  PISAtest <- readPISA(path = paste0(path,y,"/"), countries = country,
                       cognitive = "response")
  
  PISA <- as_tibble(getData(data = PISAtest, 
                            varnames = variable_names,
                            addAttributes = T, omittedLevels =F))
  
  PISA <- PISA %>% mutate(year = y)
  
  if(country == "GBR"){
    PISA <- PISA %>% filter(subnatio == "United Kingdom (England, Wales & Northern Ireland")
    
  }
  
  setwd(paste0(root,"prepared_data/",y,"/"))
  write.csv(PISA, paste0(country,"_",as.character(y),".csv"))
}





# Read PISA Data for 2000 -------------------------------------------------
# Dependent Variables
# pv1scie --> Plausible Value 1 for Science score
# pv1read --> Plausible Value 1 for Reading score
# pv1math --> Plausible Value 1 for Math Score

# We need: ESCS = In separate dataframe --> merge with left_join()

# ISEI (Father) = bfmj 
# Disciplinary Climate = disclima
# Migration background = cpmstrict from:
## st16q01 = country of birth, self
## st16q02 = country of birth, mother
## st16q03 = country of birth, father
# language at home = test language = st17q01

# Student absenteeism = SC19Q02
# Teacher absenteeism = SC19Q08

# School: Accountability = composite of sc18 items
## SC18Q01 = Parents information
## SC18Q02 = Promotion decisions
## SC18Q03 = Instructional Grouping
## SC18Q04 = National Comparison
## SC18Q05 = Progress Monitoring
## SC18Q06 = Teachers Effectiveness

# School autonomy  = composite of sc22 items, 
# ratio: teachers + principal versus not a school responsibility
## sc22q01 = hiring teachers
## sc22q02 = firing teachers
## sc22q03 = establishing teachers' starting salaries
## sc22q04 = determining teachers' salary increases
## sc22q05 = formulating the school budget
## sc22q06 = deciding on budget allocations within the school

## schauton


# Educational Leadership = composite of sc22 items
# when answered == Principal
## sc22q01 = hiring teachers
## sc22q02 = firing teachers
## sc22q03 = establishing teachers' starting salaries
## sc22q04 = determining teachers' salary increases
## sc22q05 = formulating the school budget
## sc22q06 = deciding on budget allocations within the school
## sc22q07 = establishing student disciplinary policies
## sc22q08 = h)	establishing student assessment policies
## sc22q09 = approving students for admittance to school
## sc22q10 = choosing which textbooks are used
## sc22q11 = determining course content
## sc22q12 = deciding which courses are offered

## SUBNATIO = Country Region if applicable


variable_names <- c("stidstd",
                    "schoolid",
                    "country",
                    "isei",
                    
                    "bfmj",
                    "disclima",
                    "sc19q02",
                    "sc19q08",
                    
                    
                    "st16q01", "st16q02", "st16q03",
                    "st17q01",
                    "sc18q01","sc18q02","sc18q03",
                    "sc18q04","sc18q05","sc18q06",
                    "sc22q01","sc22q02","sc22q03",
                    "sc22q03","sc22q04","sc22q08",
                    "sc22q07","sc22q08","sc22q09",
                    "sc22q10","sc22q11","sc22q12",
                    "schauton",
                    "w_fstuwt_read",
                    "w_fstuwt_scie",
                    "w_fstuwt_math",
                    "subnatio")

for(i in seq(1,80)){
  v <- paste0("w_fstr_math",as.character(i))
  variable_names <- append(variable_names,v)
  
  v <- paste0("w_fstr_read",as.character(i))
  variable_names <- append(variable_names,v)
  
  v <- paste0("w_fstr_scie",as.character(i))
  
  variable_names <- append(variable_names,v)
  
}

for(i in seq(1,5)){
  v <- paste0("pv",as.character(i),"scie")
  variable_names <- append(variable_names,v)
  v <- paste0("pv",as.character(i),"read")
  variable_names <- append(variable_names,v)
  v <- paste0("pv",as.character(i),"math")
  variable_names <- append(variable_names,v)
}

## kein Science?! WTF?!
countries <- c("AUS","AUT","DEU","CAN","GBR", "JPN","FIN","SWE","KOR","USA")
y = 2000
for (country in countries) {
  
  #downloadPISA(root = paste0(path,y,"/"), years= 2000)
  
  PISAtest <- readPISA(path = paste0(path,y,"/"), countries = country,
                       cognitive = "response")
  
  PISA <- as_tibble(getData(data = PISAtest, 
                            varnames = variable_names,
                            addAttributes = T, omittedLevels =F))
  
  PISA <- PISA %>% mutate(year = y)
  
  if(country == "GBR"){
    PISA <- PISA %>% filter(subnatio != 1)
    
  }
  
  
  if (y == 2000) {
    #print("true")
    ESCS_PISA2000 <- read_sav(paste0(path,y,"/PISA2000_ESCS/ESCS_PISA2000.sav"))
    
    # rename columns
    names(ESCS_PISA2000) <- ESCS_PISA2000 %>% colnames() %>% tolower()
    
    # prepare columns for merging-process
    ESCS_PISA2000$stidstd <- as.double(ESCS_PISA2000$stidstd)
    ESCS_PISA2000$schoolid <- as.double(ESCS_PISA2000$schoolid)
    ESCS_PISA2000$subnatio <- as.double(ESCS_PISA2000$subnatio)
    ESCS_PISA2000 <- as_tibble(ESCS_PISA2000)
    # merge columns and purge unnecessary columns
    PISA <- left_join(PISA, ESCS_PISA2000)
    
    remove(ESCS_PISA2000)
    
    ## subnatio + f_weights
    #PISA <- PISA %>% select(c(-starts_with("w_fstr"),-subnatio))
    #PISA <- PISA %>% relocate(escs, .after = country)
    
  }
  
  setwd(paste0(root,"prepared_data/",y,"/"))
  write.csv(PISA, paste0(country,"_",y,".csv"))
  
}


