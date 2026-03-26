###############################################################################
## RECODING
###############################################################################

library("foreign")
library("car")
data2 <- read.dta("Fall2010data.dta")
data <- data2

# Create 'condition' variable
## 1=Control
## 2=Pro Descriptive (Americans)
## 3=Pro Descriptive+Injunctive (Americans)
## 4=Con Descriptive (Americans)
## 5=Con Descriptive+Injunctive (Americans)
data$condition <- NA
control <- recode(data$control,"1=1;else=0")
## "Americans" treatment wording
pda <- recode(data$pda,"1=2;else=0")
pdia <- recode(data$pdia,"1=3;else=0")
cda <- recode(data$cda,"1=4;else=0")
cdia <- recode(data$cdia,"1=5;else=0")
temp.cond1 <- control + pda + pdia + cda + cdia
temp.cond1 <- recode(temp.cond1,"0=NA")
## "Students" treatment wording
pds <- recode(data$pds,"1=2;else=0")
pdis <- recode(data$pdis,"1=3;else=0")
cds <- recode(data$cds,"1=4;else=0")
cdis <- recode(data$cdis,"1=5;else=0")
temp.cond2 <- pds + pdis + cds + cdis
temp.cond2 <- recode(temp.cond2,"0=NA")
data$condition <- temp.cond1
for(i in 1:length(data$condition)){
    if(is.na(data$condition[i]))
        data$condition[i] <- temp.cond2[i]
}

# 7-point partisanship measure: -1=Rep, 0=Indep, +1=Dem
data$partyid = recode(data$partisanship,"2=1;1=-1;3=0;4=0;else=0",as.factor.result=FALSE)
    temp1 = recode(data$partisanship2,"1=1;2=.67;else=0",as.factor.result=FALSE)
    temp2 = recode(data$partisanship3,"1=-1;2=-.67;else=0",as.factor.result=FALSE)
    temp3 = recode(data$partisanship4,"1=-.33;2=.33;else=0",as.factor.result=FALSE)
for(i in 1:length(data$partyid)){
    if(!temp1[i]==0) data$partyid[i] = temp1[i]
    else if(!temp2[i]==0) data$partyid[i] = temp2[i]
    else if(!temp3[i]==0) data$partyid[i] = temp3[i]
}

# rescale ideology
data$ideology <- round((data$ideology-4)/3,2)


# recode GW human induced question so higher scores equal human-induced
# data$human_induced <- recode(data$human_induced,"1=7;2=6;3=5;4=4;5=3;6=2;7=1")

# rescale DVs
data$global_warming <- round((data$global_warming-4)/3,2)
data$human_induced <- round((data$human_induced-4)/3,2)
data$support_cap <- round((data$support_cap-4)/3,2)
data$smallercar <- round((data$smallercar-4)/3,2)
data$ce_ppi <- round((data$ce_ppi-4)/3,2)
data$ce_er <- round((data$ce_er-4)/3,2)

## CLEAN DATA
## There are 310 missing observations (in other conditions); delete them
data = subset(data,!is.na(data$condition))
data <- subset(data,select=c(condition,global_warming,human_induced,support_cap,smallercar,ce_ppi,ce_er,partyid,ideology))


## treatment group missing data imputation
for(i in 1:dim(data)[1]){
    temp.data = subset(data,data$condition==data$condition[i])
    if(is.na(data$global_warming[i]))
        data$global_warming[i] = sample(temp.data$global_warming,1)
    if(is.na(data$human_induced[i]))
        data$human_induced[i] = sample(temp.data$human_induced,1)
    if(is.na(data$support_cap[i]))
        data$support_cap[i] = sample(temp.data$support_cap,1)
    if(is.na(data$smallercar[i]))
        data$smallercar[i] = sample(temp.data$smallercar,1)
    if(is.na(data$ce_ppi[i]))
        data$ce_ppi[i] = sample(temp.data$ce_ppi,1)
    if(is.na(data$ce_er[i]))
        data$ce_er[i] = sample(temp.data$ce_er,1)
}

# Create working dataset
write.csv(data,"gsu2010-data.csv")
