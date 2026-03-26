### Licensed to Drive, but not to Work: The Labor Market Effects of Driver Licenses for Unauthorized Immigrants
### Journal of Economics, Race, and Policy
### Hans Lueders
### Replication code
### 22 March 2021




# 1. Preparation ----------------------------------------------------------

## Set your working directory here
path <- ""
setwd(path)

## Load packages
library(foreign)
library(ggplot2)
library(stargazer)
library(lfe)
library(xtable)

## Load data
d <- read.dta(paste(path, "LicensedToDrive_replicationdata.dta", sep=""))

## Identify the post-treatment period
d$post <- 0
d$post[d$year>=2015] <- 1

## code year for event study plots
d$yearEVENT <- d$year - 2014
d$yearEVENT <- factor(d$yearEVENT, levels=seq(-4,5), 
                      labels = c("2010", "2011", "2012", "2013", "2014", "2015", 
                                 "2016", "2017", "2018", "2019"))
d$yearEVENT <- relevel(d$yearEVENT, ref = "2014")

## code treatment and control groups
# naturalization definition
d$treatgroup <- NA
d$treatgroup[d$mexican == 1 & d$citizen == "naturalized citizen"] <- 0
d$treatgroup[d$mexican == 1 & d$citizen == "not a citizen"] <- 1

# skills & residence definition
d$treatgroupUNAUTH <- NA
d$treatgroupUNAUTH[d$mexican == 1 & d$citizen == "naturalized citizen" & d$yearsUS > 5 & 
                     (d$educ_cat == "Less than HS" | d$educ_cat == "HS or GED")] <- 0
d$treatgroupUNAUTH[d$mexican == 1 & d$citizen == "not a citizen" & d$yearsUS > 5 & 
                     (d$educ_cat == "Less than HS" | d$educ_cat == "HS or GED")] <- 1
d$treatgroupUNAUTH[d$treatgroupUNAUTH == 1 & d$citizenMAX == 1] <- 0
d$treatgroupUNAUTH[d$treatgroupUNAUTH == 1 & d$year_entry <= 1980] <- 0
d$treatgroupUNAUTH[d$treatgroupUNAUTH == 1 & d$sspDICH  == 1] <- 0
d$treatgroupUNAUTH[d$treatgroupUNAUTH == 1 & (d$industry  == "Public Administration" | d$industry == "Active Duty Military")] <- 0

# alternative control group: non-Hispanic White natural-born U.S. citizens
d$treatgroupWHITE <- NA
d$treatgroupWHITE[d$racwht == 1 & d$mexican == 0 & d$latino == 0 & d$citizen == "citizen by birth"] <- 0
d$treatgroupWHITE[d$mexican == 1 & d$citizen == "not a citizen"] <- 1

# Placebo test: European immigrants
d$treatgroupEUROPEAN <- NA
d$treatgroupEUROPEAN[d$european == 1 & d$citizen == "naturalized citizen"] <- 0
d$treatgroupEUROPEAN[d$european == 1 & d$citizen == "not a citizen"] <- 1

## Code hourly wages
d$wages.hourly <- d$wages2019 / d$hoursworkedyear
d$wages.hourly[d$wages.hourly >= quantile(d$wages.hourly, probs=0.95, na.rm=T)] <- quantile(d$wages.hourly, probs=0.95, na.rm=T)

## Create two subsets of the data
# power analysis: 2007-2014
d.power <- subset(d, year <= 2014)

# main data
d <- subset(d, year >= 2010)




# 2. Main text: Results ---------------------------------------------------

#--------------------------------------------------------------------------------------------------------#
### Table 1: Effect of driver licenses for unauthorized immigrants on access to private transportation ###
#--------------------------------------------------------------------------------------------------------#

## Regressions
m1 <- felm(car_dich ~ treatgroup*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
           data=d)
m2 <- felm(car_dich ~ treatgroupUNAUTH*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, data=d)
m3 <- felm(travel_car ~ treatgroup*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
           data=d)
m4 <- felm(travel_car ~ treatgroupUNAUTH*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, data=d)

## Output
stargazer(m1, m2, m3, m4,
          omit.stat = c("rsq", "ser", "f"),
          omit=c("county"),
          df = F,
          notes.align="c",
          no.space=T,
          font.size="tiny",
          star.cutoffs = c(0.05,0.01,0.001),
          model.names=F,
          column.labels = c("nat. def.", "skills/res. def.", 
                            "nat. def.", "skills/res. def."),
          dep.var.labels.include = T)


#----------------------------------------------------------------------#
### Figure 1: Event study plots for access to private transportation ###
#----------------------------------------------------------------------#

## 1a: car ownership
# Regressions
m1 <- felm(car_dich ~ treatgroup*yearEVENT + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*as.numeric(yearEVENT) | county | 0 | hhID, data=d)
m2 <- felm(car_dich ~ treatgroupUNAUTH*yearEVENT + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*as.numeric(yearEVENT) | county | 0 | hhID, data=d)

# get coefficients
coef <- data.frame(matrix(nrow=20, ncol=4))
colnames(coef) <- c("model", "year", "b", "se")
coef$model <- rep(c(1,2), each=10)
coef$model <- factor(coef$model, levels=c(1,2), labels = c("naturalization def.", "skills & residence def."))
coef$year <- 2010:2019
for(i in 1:10){
  if(coef$year[i] == 2014){
    coef[i,3] <- 0
    coef[i+10,3] <- 0
  }
  if(coef$year[i] != 2014){  
    coef[i,3:4] <- summary(m1)$coef[paste("treatgroup:yearEVENT",coef$year[i], sep=""),1:2]
    coef[i+10,3:4] <- summary(m2)$coef[paste("treatgroupUNAUTH:yearEVENT",coef$year[i+10], sep=""),1:2]
  }
}

# plot
p <- ggplot() + 
  annotate("rect", xmin=2014.5,xmax=2019.5, ymin=-Inf, ymax=Inf, alpha=.5, fill="seashell3") +
  geom_hline(yintercept = 0, lty=2) +
  geom_point(data=coef, aes(x=year, y=b, col=model),
             position=position_dodge(width=0.5), size=2) + 
  geom_errorbar(data=coef, aes(x=year, ymin=b-1.96*se, ymax=b+1.96*se, col=model), 
                width=0, position=position_dodge(width=0.5), size=1) + 
  scale_x_continuous(breaks=2010:2019) +
  scale_color_manual(values = c("gray30", "gray65"), name="") +    
  xlab("Year") + 
  ylab("Estimate") + 
  theme_classic() + 
  theme(axis.title = element_text(size=18, face="bold"),
        axis.text = element_text(size=16),
        legend.text = element_text(size=18),
        legend.position = "bottom")
ggsave("Coefficientplot_carownershipBOTH.png", p, width=7, height=5)


## 1b: travel by car
# Regressions
m1 <- felm(travel_car ~ treatgroup*yearEVENT + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*as.numeric(yearEVENT) | county | 0 | hhID, data=d)
m2 <- felm(travel_car ~ treatgroupUNAUTH*yearEVENT + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*as.numeric(yearEVENT) | county | 0 | hhID, data=d)

# get coefficients
coef <- data.frame(matrix(nrow=20, ncol=4))
colnames(coef) <- c("model", "year", "b", "se")
coef$model <- rep(c(1,2), each=10)
coef$model <- factor(coef$model, levels=c(1,2), labels = c("naturalization def.", "skills & residence def."))
coef$year <- 2010:2019
for(i in 1:10){
  if(coef$year[i] == 2014){
    coef[i,3] <- 0
    coef[i+10,3] <- 0
  }
  if(coef$year[i] != 2014){  
    coef[i,3:4] <- summary(m1)$coef[paste("treatgroup:yearEVENT",coef$year[i], sep=""),1:2]
    coef[i+10,3:4] <- summary(m2)$coef[paste("treatgroupUNAUTH:yearEVENT",coef$year[i+10], sep=""),1:2]
  }
}

# plot
p <- ggplot() + 
  annotate("rect", xmin=2014.5,xmax=2019.5, ymin=-Inf, ymax=Inf, alpha=.5, fill="seashell3") +
  geom_hline(yintercept = 0, lty=2) +
  geom_point(data=coef, aes(x=year, y=b, col=model),
             position=position_dodge(width=0.5), size=2) + 
  geom_errorbar(data=coef, aes(x=year, ymin=b-1.96*se, ymax=b+1.96*se, col=model), 
                width=0, position=position_dodge(width=0.5), size=1) + 
  scale_x_continuous(breaks=2010:2019) +
  scale_color_manual(values = c("gray30", "gray65"), name="") +    
  xlab("Year") + 
  ylab("Estimate") + 
  theme_classic() + 
  theme(axis.title = element_text(size=18, face="bold"),
        axis.text = element_text(size=16),
        legend.text = element_text(size=18),
        legend.position = "bottom")
ggsave("Coefficientplot_travelcarBOTH.png", p, width=7, height=5)


#---------------------------------------------------------------------------------------------------#
### Table 2: Effect of driver licenses for unauthorized immigrants on their labor market outcomes ###
#---------------------------------------------------------------------------------------------------#

## Regressions
m1 <- felm(worked ~ treatgroup*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
           data=d)
m2 <- felm(worked ~ treatgroupUNAUTH*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
           data=d)
m3 <- felm(hoursworkedyear ~ treatgroup*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
           data=d[d$worked==1,])
m4 <- felm(hoursworkedyear ~ treatgroupUNAUTH*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
           data=d[d$worked==1,])
m5 <- felm(earnings2019_log ~ treatgroup*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
           data=d[d$worked==1,])
m6 <- felm(earnings2019_log ~ treatgroupUNAUTH*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
           data=d[d$worked==1,])

## Output
stargazer(m1, m2, m3, m4, m5, m6,
          omit.stat = c("rsq", "ser", "f"),
          omit=c("county"),
          df = F,
          notes.align="c",
          no.space=T,
          font.size="tiny",
          star.cutoffs = c(0.05,0.01,0.001),
          model.names=F,
          column.labels = c("nat. def.", "skills/res. def.", 
                            "nat. def.", "skills/res. def.",
                            "nat. def.", "skills/res. def."),
          dep.var.labels.include = T)


#-----------------------------------------------------------#
### Figure 2: Event study plots for labor market outcomes ###
#-----------------------------------------------------------#

## 2a: employed
# Regressions
m1 <- felm(worked ~ treatgroup*yearEVENT + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*as.numeric(yearEVENT) | county | 0 | hhID, data=d)
m2 <- felm(worked ~ treatgroupUNAUTH*yearEVENT + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*as.numeric(yearEVENT) | county | 0 | hhID, data=d)

# get coefficients
coef <- data.frame(matrix(nrow=20, ncol=4))
colnames(coef) <- c("model", "year", "b", "se")
coef$model <- rep(c(1,2), each=10)
coef$model <- factor(coef$model, levels=c(1,2), labels = c("naturalization def.", "skills & residence def."))
coef$year <- 2010:2019
for(i in 1:10){
  if(coef$year[i] == 2014){
    coef[i,3] <- 0
    coef[i+10,3] <- 0
  }
  if(coef$year[i] != 2014){  
    coef[i,3:4] <- summary(m1)$coef[paste("treatgroup:yearEVENT",coef$year[i], sep=""),1:2]
    coef[i+10,3:4] <- summary(m2)$coef[paste("treatgroupUNAUTH:yearEVENT",coef$year[i+10], sep=""),1:2]
  }
}


# plot
p <- ggplot() + 
  annotate("rect", xmin=2014.5,xmax=2019.5, ymin=-Inf, ymax=Inf, alpha=.5, fill="seashell3") +
  geom_hline(yintercept = 0, lty=2) +
  geom_point(data=coef, aes(x=year, y=b, col=model),
             position=position_dodge(width=0.5), size=2) + 
  geom_errorbar(data=coef, aes(x=year, ymin=b-1.96*se, ymax=b+1.96*se, col=model), 
                width=0, position=position_dodge(width=0.5), size=1) + 
  scale_x_continuous(breaks=2010:2019) +
  scale_color_manual(values = c("gray30", "gray65"), name="") +    
  xlab("Year") + 
  ylab("Estimate") + 
  theme_classic() + 
  theme(axis.title = element_text(size=18, face="bold"),
        axis.text = element_text(size=16),
        legend.text = element_text(size=18),
        legend.position = "bottom")
ggsave("Coefficientplot_workedBOTH.png", p, width=7, height=5)


## 2b: hours worked per year
# Regressions
m1 <- felm(hoursworkedyear ~ treatgroup*yearEVENT + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*as.numeric(yearEVENT) | county | 0 | hhID, 
           data=d[d$worked==1,])
m2 <- felm(hoursworkedyear ~ treatgroupUNAUTH*yearEVENT + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*as.numeric(yearEVENT) | county | 0 | hhID, 
           data=d[d$worked==1,])

# get coefficients
coef <- data.frame(matrix(nrow=20, ncol=4))
colnames(coef) <- c("model", "year", "b", "se")
coef$model <- rep(c(1,2), each=10)
coef$model <- factor(coef$model, levels=c(1,2), labels = c("naturalization def.", "skills & residence def."))
coef$year <- 2010:2019
for(i in 1:10){
  if(coef$year[i] == 2014){
    coef[i,3] <- 0
    coef[i+10,3] <- 0
  }
  if(coef$year[i] != 2014){  
    coef[i,3:4] <- summary(m1)$coef[paste("treatgroup:yearEVENT",coef$year[i], sep=""),1:2]
    coef[i+10,3:4] <- summary(m2)$coef[paste("treatgroupUNAUTH:yearEVENT",coef$year[i+10], sep=""),1:2]
  }
}

# plot
p <- ggplot() + 
  annotate("rect", xmin=2014.5,xmax=2019.5, ymin=-Inf, ymax=Inf, alpha=.5, fill="seashell3") +
  geom_hline(yintercept = 0, lty=2) +
  geom_point(data=coef, aes(x=year, y=b, col=model),
             position=position_dodge(width=0.5), size=2) + 
  geom_errorbar(data=coef, aes(x=year, ymin=b-1.96*se, ymax=b+1.96*se, col=model), 
                width=0, position=position_dodge(width=0.5), size=1) + 
  scale_x_continuous(breaks=2010:2019) +
  scale_color_manual(values = c("gray30", "gray65"), name="") +    
  xlab("Year") + 
  ylab("Estimate") + 
  theme_classic() + 
  theme(axis.title = element_text(size=18, face="bold"),
        axis.text = element_text(size=16),
        legend.text = element_text(size=18),
        legend.position = "bottom")
ggsave("Coefficientplot_hoursworkedyearBOTH.png", p, width=7, height=5)


## 2c: earnings
# Regressions
m1 <- felm(earnings2019_log ~ treatgroup*yearEVENT + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*as.numeric(yearEVENT) | county | 0 | hhID, 
           data=d[d$worked==1,])
m2 <- felm(earnings2019_log ~ treatgroupUNAUTH*yearEVENT + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*as.numeric(yearEVENT) | county | 0 | hhID, 
           data=d[d$worked==1,])

# get coefficients
coef <- data.frame(matrix(nrow=20, ncol=4))
colnames(coef) <- c("model", "year", "b", "se")
coef$model <- rep(c(1,2), each=10)
coef$model <- factor(coef$model, levels=c(1,2), labels = c("naturalization def.", "skills & residence def."))
coef$year <- 2010:2019
for(i in 1:10){
  if(coef$year[i] == 2014){
    coef[i,3] <- 0
    coef[i+10,3] <- 0
  }
  if(coef$year[i] != 2014){  
    coef[i,3:4] <- summary(m1)$coef[paste("treatgroup:yearEVENT",coef$year[i], sep=""),1:2]
    coef[i+10,3:4] <- summary(m2)$coef[paste("treatgroupUNAUTH:yearEVENT",coef$year[i+10], sep=""),1:2]
  }
}

# plot
p <- ggplot() + 
  annotate("rect", xmin=2014.5,xmax=2019.5, ymin=-Inf, ymax=Inf, alpha=.5, fill="seashell3") +
  geom_hline(yintercept = 0, lty=2) +
  geom_point(data=coef, aes(x=year, y=b, col=model),
             position=position_dodge(width=0.5), size=2) + 
  geom_errorbar(data=coef, aes(x=year, ymin=b-1.96*se, ymax=b+1.96*se, col=model), 
                width=0, position=position_dodge(width=0.5), size=1) + 
  scale_x_continuous(breaks=2010:2019) +
  scale_color_manual(values = c("gray30", "gray65"), name="") +    
  xlab("Year") + 
  ylab("Estimate") + 
  theme_classic() + 
  theme(axis.title = element_text(size=18, face="bold"),
        axis.text = element_text(size=16),
        legend.text = element_text(size=18),
        legend.position = "bottom")
ggsave("Coefficientplot_earningsBOTH.png", p, width=7, height=5)




# 3. Main text: Probing the null result on employment ---------------------


#-----------------------------------------------------#
### Table 3: Effects on employment status by gender ###
#-----------------------------------------------------#

## Regressions
m1 <- felm(worked ~ treatgroup*as.factor(post)*female + age + age_squared + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, data=d)
m2 <- felm(worked ~ treatgroupUNAUTH*as.factor(post)*female + age + age_squared + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, data=d)

# Output
stargazer(m1, m2,
          omit.stat = c("rsq", "ser", "f"),
          omit=c("county"),
          df = F,
          notes.align="c",
          no.space=T,
          font.size="tiny",
          star.cutoffs = c(0.05,0.01,0.001),
          model.names=F,
          dep.var.labels.include = T)


#--------------------------------------------------------------------------#
### Figure 3: No effect on employment status across different age groups ###
#--------------------------------------------------------------------------#

## code age groups
d$agegroup <- NA
d$agegroup[d$age >= 16 & d$age <= 28] <- 1
d$agegroup[d$age >= 29 & d$age <= 41] <- 2
d$agegroup[d$age >= 42 & d$age <= 53] <- 3
d$agegroup[d$age >= 54 & d$age <= 65] <- 4

## write a quick function that analyzes the data by age group and returns a plot
agefun <- function(dv) {
  # create and prepare output object
  out <- matrix(nrow=8, ncol=4)
  out[,1] <- rep(1:4,2)
  out[,2] <- rep(1:2, each=4)
  out <- data.frame(out)
  colnames(out) <- c("agegroup", "model", "b", "se")
  out$agegroup <- factor(out$agegroup, levels=1:4, labels = c("16-28", "29-41", "42-53", "54-65"))
  out$model <- factor(out$model, levels=1:2, labels = c("naturalization def.", "skills & residence def."))
  
  # naturalization definition
  out[1,3:4] <- summary(felm(d[d$agegroup==1,dv] ~ treatgroup*as.factor(post) + female + yearsUS + yearsUS_squared + 
                               marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
                             data=d[d$agegroup==1,]))$coefficients["treatgroup:as.factor(post)1",1:2]
  out[2,3:4] <- summary(felm(d[d$agegroup==2,dv] ~ treatgroup*as.factor(post) + female + yearsUS + yearsUS_squared + 
                               marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
                             data=d[d$agegroup==2,]))$coefficients["treatgroup:as.factor(post)1",1:2]  
  out[3,3:4] <- summary(felm(d[d$agegroup==3,dv] ~ treatgroup*as.factor(post) + female + yearsUS + yearsUS_squared + 
                               marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
                             data=d[d$agegroup==3,]))$coefficients["treatgroup:as.factor(post)1",1:2]  
  out[4,3:4] <- summary(felm(d[d$agegroup==4,dv] ~ treatgroup*as.factor(post) + female + yearsUS + yearsUS_squared + 
                               marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
                             data=d[d$agegroup==4,]))$coefficients["treatgroup:as.factor(post)1",1:2]    
  
  # skills & residence definition
  out[5,3:4] <- summary(felm(d[d$agegroup==1,dv] ~ treatgroupUNAUTH*as.factor(post) + female + yearsUS + yearsUS_squared + 
                               marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
                             data=d[d$agegroup==1,]))$coefficients["treatgroupUNAUTH:as.factor(post)1",1:2]
  out[6,3:4] <- summary(felm(d[d$agegroup==2,dv] ~ treatgroupUNAUTH*as.factor(post) + female + yearsUS + yearsUS_squared + 
                               marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
                             data=d[d$agegroup==2,]))$coefficients["treatgroupUNAUTH:as.factor(post)1",1:2]  
  out[7,3:4] <- summary(felm(d[d$agegroup==3,dv] ~ treatgroupUNAUTH*as.factor(post) + female + yearsUS + yearsUS_squared + 
                               marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
                             data=d[d$agegroup==3,]))$coefficients["treatgroupUNAUTH:as.factor(post)1",1:2]  
  out[8,3:4] <- summary(felm(d[d$agegroup==4,dv] ~ treatgroupUNAUTH*as.factor(post) + female + yearsUS + yearsUS_squared + 
                               marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
                             data=d[d$agegroup==4,]))$coefficients["treatgroupUNAUTH:as.factor(post)1",1:2] 
  
  # plot
  p <- ggplot() + 
    geom_hline(yintercept = 0, lty=2) + 
    geom_point(data=out, aes(x=agegroup, y=b, col=model),
               position = position_dodge(width=0.5), size=2) + 
    geom_errorbar(data=out, aes(x=agegroup, ymin=b - 1.96*se, ymax=b+1.96*se, col=model),
                  position = position_dodge(width=0.5), width=0, size=1) + 
    scale_color_manual(values = c("gray30", "gray80"), name="") +
    xlab("Age group") + 
    ylab("Estimate") + 
    theme_classic() + 
    theme(axis.title = element_text(size=18, face="bold"),
          axis.text = element_text(size=16),
          legend.text = element_text(size=18),
          legend.position = "bottom")
  
  # return
  return(p)
}

## use function for employment status
age.worked <- agefun(dv = "worked")
ggsave("agegroup_worked.png", age.worked, width=7, height=5)


#----------------------------------------------------------------------------#
### Figure 4: No effect on employment status by labor market accessibility ###
#----------------------------------------------------------------------------#

## write functions that analyze the data by PUMA sub-samples
# naturalization definition
het.main <- function(dv){
  ## define treatment variable
  d$dv <- d[,dv]
  
  ## define output container
  out <- data.frame(matrix(nrow = 8, ncol=4))
  colnames(out) <- c("var", "high", "b", "se")
  out$var <- rep(1:4,each=2)
  out$var <- factor(out$var, levels=1:4, labels = c("Urbanization", "Commute time",
                                                    "Public transit use", "Employment rate"))
  out$high <- rep(c(0,1),4)
  out$high <- factor(out$high, levels=c(0,1), labels = c("low", "high"))
  
  
  ## urbanization
  # low
  out[1,3:4] <- summary(felm(dv ~ treatgroup*as.factor(post) + 
                               age + age_squared + female + yearsUS + yearsUS_squared + 
                               marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
                             data=d[d$puma_popdens_log <= quantile(d$puma_popdens_log, probs=0.25,
                                                                   na.rm=T),]))$coefficients["treatgroup:as.factor(post)1",1:2]
  
  # high
  out[2,3:4] <- summary(felm(dv ~ treatgroup*as.factor(post) + 
                               age + age_squared + female + yearsUS + yearsUS_squared + 
                               marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
                             data=d[d$puma_popdens_log >= quantile(d$puma_popdens_log, probs=0.75,
                                                                   na.rm=T),]))$coefficients["treatgroup:as.factor(post)1",1:2]
  
  ## commute times
  # short
  out[3,3:4] <- summary(felm(dv ~ treatgroup*as.factor(post) + 
                               age + age_squared + female + yearsUS + yearsUS_squared + 
                               marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
                             data=d[d$traveltimePUMA2014 <= quantile(d$traveltimePUMA2014, probs=0.25,
                                                                     na.rm=T),]))$coefficients["treatgroup:as.factor(post)1",1:2]
  # long
  out[4,3:4] <- summary(felm(dv ~ treatgroup*as.factor(post) + 
                               age + age_squared + female + yearsUS + yearsUS_squared + 
                               marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
                             data=d[d$traveltimePUMA2014 >= quantile(d$traveltimePUMA2014, probs=0.75,
                                                                     na.rm=T),]))$coefficients["treatgroup:as.factor(post)1",1:2]
  
  ## public transit use
  # low
  out[5,3:4] <- summary(felm(dv ~ treatgroup*as.factor(post) + 
                               age + age_squared + female + yearsUS + yearsUS_squared + 
                               marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
                             data=d[d$travelpublicPUMA2014 <= quantile(d$travelpublicPUMA2014, probs=0.25,
                                                                       na.rm=T),]))$coefficients["treatgroup:as.factor(post)1",1:2]
  # high
  out[6,3:4] <- summary(felm(dv ~ treatgroup*as.factor(post) + 
                               age + age_squared + female + yearsUS + yearsUS_squared + 
                               marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
                             data=d[d$travelpublicPUMA2014 >= quantile(d$travelpublicPUMA2014, probs=0.75,
                                                                       na.rm=T),]))$coefficients["treatgroup:as.factor(post)1",1:2]
  
  ## baseline employment rate
  # low
  out[7,3:4] <- summary(felm(dv ~ treatgroup*as.factor(post) + 
                               age + age_squared + female + yearsUS + yearsUS_squared + 
                               marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
                             data=d[d$workedPUMA2014 <= quantile(d$workedPUMA2014, probs=0.25,
                                                                 na.rm=T),]))$coefficients["treatgroup:as.factor(post)1",1:2]
  # high
  out[8,3:4] <- summary(felm(dv ~ treatgroup*as.factor(post) + 
                               age + age_squared + female + yearsUS + yearsUS_squared + 
                               marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
                             data=d[d$workedPUMA2014 >= quantile(d$workedPUMA2014, probs=0.75,
                                                                 na.rm=T),]))$coefficients["treatgroup:as.factor(post)1",1:2]
  
  
  ## plot
  p <- ggplot() + 
    geom_hline(yintercept = 0, lty=2) + 
    geom_point(data=out, aes(x=var, y=b, col=high),
               position = position_dodge(width=0.5), size=2) + 
    geom_errorbar(data=out, aes(x=var, ymin=b - 1.96*se, ymax=b+1.96*se, col=high),
                  position = position_dodge(width=0.5), width=0, size=1) + 
    scale_color_manual(values = c("gray30", "gray80"), name="") +
    xlab("Subset variable") + 
    ylab("Estimate") + 
    theme_classic() + 
    theme(axis.title = element_text(size=18, face="bold"),
          axis.text.x = element_text(size=16, angle=30, hjust=1),
          axis.text.y = element_text(size=16),
          legend.text = element_text(size=18),
          legend.position = "bottom")
  
  ## output
  return(p)
}


# skills & residence definition
het.alt <- function(dv){
  ## define treatment variable
  d$dv <- d[,dv]
  
  ## define output container
  out <- data.frame(matrix(nrow = 8, ncol=4))
  colnames(out) <- c("var", "high", "b", "se")
  out$var <- rep(1:4,each=2)
  out$var <- factor(out$var, levels=1:4, labels = c("Urbanization", "Commute time",
                                                    "Public transit use", "Employment rate"))
  out$high <- rep(c(0,1),4)
  out$high <- factor(out$high, levels=c(0,1), labels = c("low", "high"))
  
  
  ## urbanization
  # low
  out[1,3:4] <- summary(felm(dv ~ treatgroupUNAUTH*as.factor(post) + 
                               age + age_squared + female + yearsUS + yearsUS_squared + 
                               marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
                             data=d[d$puma_popdens_log <= quantile(d$puma_popdens_log, probs=0.25,
                                                                   na.rm=T),]))$coefficients["treatgroupUNAUTH:as.factor(post)1",1:2]
  
  # high
  out[2,3:4] <- summary(felm(dv ~ treatgroupUNAUTH*as.factor(post) + 
                               age + age_squared + female + yearsUS + yearsUS_squared + 
                               marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
                             data=d[d$puma_popdens_log >= quantile(d$puma_popdens_log, probs=0.75,
                                                                   na.rm=T),]))$coefficients["treatgroupUNAUTH:as.factor(post)1",1:2]
  
  ## commute times
  # short
  out[3,3:4] <- summary(felm(dv ~ treatgroupUNAUTH*as.factor(post) + 
                               age + age_squared + female + yearsUS + yearsUS_squared + 
                               marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
                             data=d[d$traveltimePUMA2014 <= quantile(d$traveltimePUMA2014, probs=0.25,
                                                                     na.rm=T),]))$coefficients["treatgroupUNAUTH:as.factor(post)1",1:2]
  # long
  out[4,3:4] <- summary(felm(dv ~ treatgroupUNAUTH*as.factor(post) + 
                               age + age_squared + female + yearsUS + yearsUS_squared + 
                               marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
                             data=d[d$traveltimePUMA2014 >= quantile(d$traveltimePUMA2014, probs=0.75,
                                                                     na.rm=T),]))$coefficients["treatgroupUNAUTH:as.factor(post)1",1:2]
  
  ## public transit use
  # low
  out[5,3:4] <- summary(felm(dv ~ treatgroupUNAUTH*as.factor(post) + 
                               age + age_squared + female + yearsUS + yearsUS_squared + 
                               marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
                             data=d[d$travelpublicPUMA2014 <= quantile(d$travelpublicPUMA2014, probs=0.25,
                                                                       na.rm=T),]))$coefficients["treatgroupUNAUTH:as.factor(post)1",1:2]
  # high
  out[6,3:4] <- summary(felm(dv ~ treatgroupUNAUTH*as.factor(post) + 
                               age + age_squared + female + yearsUS + yearsUS_squared + 
                               marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
                             data=d[d$travelpublicPUMA2014 >= quantile(d$travelpublicPUMA2014, probs=0.75,
                                                                       na.rm=T),]))$coefficients["treatgroupUNAUTH:as.factor(post)1",1:2]
  
  ## baseline employment rate
  # low
  out[7,3:4] <- summary(felm(dv ~ treatgroupUNAUTH*as.factor(post) + 
                               age + age_squared + female + yearsUS + yearsUS_squared + 
                               marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
                             data=d[d$workedPUMA2014 <= quantile(d$workedPUMA2014, probs=0.25,
                                                                 na.rm=T),]))$coefficients["treatgroupUNAUTH:as.factor(post)1",1:2]
  # high
  out[8,3:4] <- summary(felm(dv ~ treatgroupUNAUTH*as.factor(post) + 
                               age + age_squared + female + yearsUS + yearsUS_squared + 
                               marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
                             data=d[d$workedPUMA2014 >= quantile(d$workedPUMA2014, probs=0.75,
                                                                 na.rm=T),]))$coefficients["treatgroupUNAUTH:as.factor(post)1",1:2]
  
  
  ## plot
  p <- ggplot() + 
    geom_hline(yintercept = 0, lty=2) + 
    geom_point(data=out, aes(x=var, y=b, col=high),
               position = position_dodge(width=0.5), size=2) + 
    geom_errorbar(data=out, aes(x=var, ymin=b - 1.96*se, ymax=b+1.96*se, col=high),
                  position = position_dodge(width=0.5), width=0, size=1) + 
    scale_color_manual(values = c("gray30", "gray80"), name="") +
    xlab("Subset variable") + 
    ylab("Estimate") + 
    theme_classic() + 
    theme(axis.title = element_text(size=18, face="bold"),
          axis.text.x = element_text(size=16, angle=30, hjust=1),
          axis.text.y = element_text(size=16),
          legend.text = element_text(size=18),
          legend.position = "bottom")
  
  ## output
  return(p)
}

## use function
# naturalization definition
p1 <- het.main(dv = "worked")
ggsave("HetMain_worked.png", p1, width=7, height=5)

# skills & residence definition
p2 <- het.alt(dv = "worked")
ggsave("HetAlt_worked.png", p2, width=7, height=5)



#------------------------------#
### Figure 5: Power Analysis ###
#------------------------------#

## Preparation
# Code new pre/post period
d.power$post <- 0
d.power$post[d.power$year>=2011] <- 1

# define range of treatment effects
range <- seq(0,4,0.2)

### a: naturalization definition
# select data
d2 <- subset(d.power, year <= 2014 & age >= 16 & age <= 65 & !is.na(worked) & !is.na(treatgroup))

# compute true placebo treatment effect
m <- felm(worked ~ treatgroup*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
            marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, data=d2)
te.placebo <- summary(m)$coef["treatgroup:as.factor(post)1",1]

# define key statistics
mean2011 <- mean(d2$worked[d2$year==2011 & d2$treatgroup == 1], na.rm=T)
mean2012 <- mean(d2$worked[d2$year==2012 & d2$treatgroup == 1], na.rm=T)
mean2013 <- mean(d2$worked[d2$year==2013 & d2$treatgroup == 1], na.rm=T)
mean2014 <- mean(d2$worked[d2$year==2014 & d2$treatgroup == 1], na.rm=T)
n2011 <- length(d2$worked[d2$year==2011 & d2$treatgroup == 1])
n2012 <- length(d2$worked[d2$year==2012 & d2$treatgroup == 1])
n2013 <- length(d2$worked[d2$year==2013 & d2$treatgroup == 1])
n2014 <- length(d2$worked[d2$year==2014 & d2$treatgroup == 1])

# create output container to store estimated treatment effects
power.out <- matrix(nrow=length(range)*1000,ncol=3)
power.out[,1] <- rep(range, each=1000)

## conduct simulation
for(i in 1:length(range)){
  # define treatment effect
  te <- range[i]
  
  # re-define workedBOOT
  d2$workedBOOT <- d2$worked
  
  # move some likely unauthorized immigrants from unemployed to employed
  d2$workedBOOT[d2$treatgroup == 1 & d2$workedBOOT == 0 & d2$year==2011] <- 
    rbinom(length(d2$workedBOOT[d2$treatgroup == 1 & d2$workedBOOT == 0 & d2$year==2011]), 
           size=1, prob = te * ((n2011/100) / (n2011-n2011*mean2011)))
  
  d2$workedBOOT[d2$treatgroup == 1 & d2$workedBOOT == 0 & d2$year==2012] <- 
    rbinom(length(d2$workedBOOT[d2$treatgroup == 1 & d2$workedBOOT == 0 & d2$year==2012]), 
           size=1, prob = te * ((n2012/100) / (n2012-n2012*mean2012)))
  
  d2$workedBOOT[d2$treatgroup == 1 & d2$workedBOOT == 0 & d2$year==2013] <- 
    rbinom(length(d2$workedBOOT[d2$treatgroup == 1 & d2$workedBOOT == 0 & d2$year==2013]), 
           size=1, prob = te * ((n2013/100) / (n2013-n2013*mean2013)))
  
  d2$workedBOOT[d2$treatgroup == 1 & d2$workedBOOT == 0 & d2$year==2014] <- 
    rbinom(length(d2$workedBOOT[d2$treatgroup == 1 & d2$workedBOOT == 0 & d2$year==2014]), 
           size=1, prob = te * ((n2014/100) / (n2014-n2014*mean2014)))
  
  # sampling
  for(j in 1:1000){
    # draw sample
    d2.sample <- d2[sample(1:dim(d2)[1], dim(d2)[1], replace=T),]
    
    # new regression
    m <- felm(workedBOOT ~ treatgroup*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
                marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
              data=d2.sample)
    
    # store
    power.out[(i-1)*1000 + j,2:3] <- summary(m)$coef["treatgroup:as.factor(post)1",1:2]
    
    # where am I?
    print(paste("te = ", te, "; iteration = ", j, sep=""))
  }
  # save data
  save(power.out, file="PowerWorkedWorking.RData")  
}

# save output
power.out <- data.frame(power.out)
colnames(power.out) <- c("te", "b", "se")
power.out$significant <- 0
power.out$significant[power.out$b / power.out$se > 1.96] <- 1
save(power.out, file="PowerWorkedFinal.RData")


### b: skills & residence definition
# restrict data
d2 <- subset(d.power, year <= 2014 & age >= 16 & age <= 65 & !is.na(worked) & !is.na(treatgroupUNAUTH))

# compute true placebo treatment effect
m <- felm(worked ~ treatgroupUNAUTH*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
            marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, data=d2)
te.placebo <- summary(m)$coef["treatgroupUNAUTH:as.factor(post)1",1]

# define key statistics
mean2011 <- mean(d2$worked[d2$year==2011 & d2$treatgroupUNAUTH == 1], na.rm=T)
mean2012 <- mean(d2$worked[d2$year==2012 & d2$treatgroupUNAUTH == 1], na.rm=T)
mean2013 <- mean(d2$worked[d2$year==2013 & d2$treatgroupUNAUTH == 1], na.rm=T)
mean2014 <- mean(d2$worked[d2$year==2014 & d2$treatgroupUNAUTH == 1], na.rm=T)
n2011 <- length(d2$worked[d2$year==2011 & d2$treatgroupUNAUTH == 1])
n2012 <- length(d2$worked[d2$year==2012 & d2$treatgroupUNAUTH == 1])
n2013 <- length(d2$worked[d2$year==2013 & d2$treatgroupUNAUTH == 1])
n2014 <- length(d2$worked[d2$year==2014 & d2$treatgroupUNAUTH == 1])

# output
power.out <- matrix(nrow=length(range)*1000,ncol=3)
power.out[,1] <- rep(range, each=1000)

## conduct simulation
for(i in 1:length(range)){
  # define treatment effect
  te <- range[i]
  
  # re-define workedBOOT
  d2$workedBOOT <- d2$worked
  
  # move some likely unauthorized immigrants from unemployed to employed
  d2$workedBOOT[d2$treatgroupUNAUTH == 1 & d2$workedBOOT == 0 & d2$year==2011] <- 
    rbinom(length(d2$workedBOOT[d2$treatgroupUNAUTH == 1 & d2$workedBOOT == 0 & d2$year==2011]), 
           size=1, prob = te * ((n2011/100) / (n2011-n2011*mean2011)))
  
  d2$workedBOOT[d2$treatgroupUNAUTH == 1 & d2$workedBOOT == 0 & d2$year==2012] <- 
    rbinom(length(d2$workedBOOT[d2$treatgroupUNAUTH == 1 & d2$workedBOOT == 0 & d2$year==2012]), 
           size=1, prob = te * ((n2012/100) / (n2012-n2012*mean2012)))
  
  d2$workedBOOT[d2$treatgroupUNAUTH == 1 & d2$workedBOOT == 0 & d2$year==2013] <- 
    rbinom(length(d2$workedBOOT[d2$treatgroupUNAUTH == 1 & d2$workedBOOT == 0 & d2$year==2013]), 
           size=1, prob = te * ((n2013/100) / (n2013-n2013*mean2013)))
  
  d2$workedBOOT[d2$treatgroupUNAUTH == 1 & d2$workedBOOT == 0 & d2$year==2014] <- 
    rbinom(length(d2$workedBOOT[d2$treatgroupUNAUTH == 1 & d2$workedBOOT == 0 & d2$year==2014]), 
           size=1, prob = te * ((n2014/100) / (n2014-n2014*mean2014)))
  
  # sampling
  for(j in 1:1000){
    # draw sample
    d2.sample <- d2[sample(1:dim(d2)[1], dim(d2)[1], replace=T),]
    
    # new regression
    m <- felm(workedBOOT ~ treatgroupUNAUTH*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
                marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
              data=d2.sample)
    
    # store
    power.out[(i-1)*1000 + j,2:3] <- summary(m)$coef["treatgroupUNAUTH:as.factor(post)1",1:2]
    
    # where am I?
    print(paste("te = ", te, "; iteration = ", j, sep=""))
  }
  # save data
  save(power.out, file="PowerWorkedWorkingAlt.RData")
}

# save output
power.out <- data.frame(power.out)
colnames(power.out) <- c("te", "b", "se")
power.out$significant <- 0
power.out$significant[power.out$b / power.out$se > 1.96] <- 1
save(power.out, file="PowerWorkedFinalAlt.RData")

## Aggregate data
# naturalization definition
load("PowerWorkedFinal.RData")
power.agg.main <- aggregate(power.out$significant, by=list(te = power.out$te), FUN=mean)
colnames(power.agg.main) <- c("te", "power")

# skills & residence definition
load("PowerWorkedFinalAlt.RData")
power.agg.alt <- aggregate(power.out$significant, by=list(te = power.out$te), FUN=mean)
colnames(power.agg.alt) <- c("te", "power")

# combine both
power.agg <- rbind(power.agg.main, power.agg.alt)
power.agg$model <- rep(c(1,2),each=21)
power.agg$model <- factor(power.agg$model, levels = c(1,2), 
                          labels = c("naturalization def.", "skills & residence def."))

## plot
p <- ggplot() + 
  geom_hline(yintercept = 80, lty=2) +
  geom_point(data=power.agg, aes(x=te, y=power*100, col=model)) + 
  geom_line(data=power.agg, aes(x=te, y=power*100, col=model)) +
  scale_x_continuous(breaks=seq(0,4,0.5)) +
  scale_y_continuous(breaks=seq(0,100,20)) +
  scale_color_manual(values = c("gray30", "gray80"), name="") +
  xlab("Pseudo-Treatment Effect\n(percentage points)") +
  ylab("Power\n(% significant coefficient)") + 
  theme_classic() + 
  theme(axis.title = element_text(size=18, face="bold"),
        axis.text = element_text(size=16),
        legend.text = element_text(size=18),
        legend.position = "bottom")
p
ggsave("PowerAnalysisBoth.png", p, width=7, height=5)






# 4. Main text: Robustness ------------------------------------------------

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------#
### Table 4: The effect of driver licenses for unauthorized immigrants is robust to an alternative control group: non-Hispanic white natural-born U.S. citizens ###
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------#

## Regressions
m1 <- felm(car_dich ~ treatgroupWHITE*as.factor(post) + age + age_squared + female + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, data=d)
m2 <- felm(travel_car ~ treatgroupWHITE*as.factor(post) + age + age_squared + female + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, data=d)
m3 <- felm(worked ~ treatgroupWHITE*as.factor(post) + age + age_squared + female + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, data=d)
m4 <- felm(hoursworkedyear ~ treatgroupWHITE*as.factor(post) + age + age_squared + female + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
           data=d[d$worked==1,])
m5 <- felm(earnings2019_log ~ treatgroupWHITE*as.factor(post) + age + age_squared + female + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
           data=d[d$worked==1,])


## Output
stargazer(m1, m2, m3, m4, m5,
          omit.stat = c("rsq", "ser", "f"),
          omit=c("county"),
          df = F,
          notes.align="c",
          no.space=T,
          font.size="tiny",
          star.cutoffs = c(0.05,0.01,0.001),
          model.names=F,
          dep.var.labels.include = T)


#---------------------------------------------------------------------------------------------#
### Table 5: Placebo Test: No effect of the driver license reform among European immigrants ###
#---------------------------------------------------------------------------------------------#

## Regressions
m1 <- felm(car_dich ~ treatgroupEUROPEAN*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, data=d)
m2 <- felm(travel_car ~ treatgroupEUROPEAN*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, data=d)
m3 <- felm(worked ~ treatgroupEUROPEAN*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, data=d)
m4 <- felm(hoursworkedyear ~ treatgroupEUROPEAN*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
           data=d[d$worked==1,])
m5 <- felm(earnings2019_log ~ treatgroupEUROPEAN*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
           data=d[d$worked==1,])

## Output
stargazer(m1, m2, m3, m4, m5,
          omit.stat = c("rsq", "ser", "f"),
          omit=c("county"),
          df = F,
          notes.align="c",
          no.space=T,
          font.size="tiny",
          star.cutoffs = c(0.05,0.01,0.001),
          model.names=F,
          dep.var.labels.include = T)





# 5. Appendix: Data description and summary statistics --------------------

#----------------------------------#
### Table A4: Summary Statistics ###
#----------------------------------#

## define variables for which we want summary statistics
# no further restrictions
vars <- c("treatgroup", "car_dich", "travel_car", "worked",
          "age", "female", "yearsUS", "marstat_married", "nminor16",
          "educ_lessHS", "educ_HS_GED", "educ_somecollege", "educ_BA", "educ_graduate",
          "puma_popdens", "traveltimePUMA2014", "travelpublicPUMA2014", "workedPUMA2014",
          "travel_occupancy", "travel_occupancy_single")

# those conditional on worked==1
vars2 <- c("hoursworkedyear", "earnings2019",
           "hoursworked", "weeksworked2",
           "wages2019", "wages.hourly", "industry_agriculture", "industry_construction",
           "industry_manufacturing", "industry_transportation")

## define output matrices
out <- matrix(nrow = length(vars), ncol=3)
out2 <- matrix(nrow = length(vars2), ncol=3)

## loops to get summary statistics
for(i in 1:length(vars)){
  dv <- vars[i]
  out[i,1] <- mean(d[!is.na(d$treatgroup),dv], na.rm=T)
  out[i,2] <- median(d[!is.na(d$treatgroup),dv], na.rm=T)
  out[i,3] <- sd(d[!is.na(d$treatgroup),dv], na.rm=T)
}

for(i in 1:length(vars2)){
  dv <- vars2[i]
  out2[i,1] <- mean(d[!is.na(d$treatgroup) & d$worked == 1,dv], na.rm=T)
  out2[i,2] <- median(d[!is.na(d$treatgroup) & d$worked == 1,dv], na.rm=T)
  out2[i,3] <- sd(d[!is.na(d$treatgroup) & d$worked == 1,dv], na.rm=T)
}


## turn into data frame
out <- data.frame(rbind(out, out2))
colnames(out) <- c("mean", "median", "sd")
out$var <- c(vars, vars2)
out <- out[,c(4,1,2,3)]


## add further variables
# treatgroup: UNAUTH
out <- rbind(out, c("treatgroupUNAUTH", mean(d$treatgroupUNAUTH[!is.na(d$treatgroupUNAUTH)]),
                    median(d$treatgroupUNAUTH[!is.na(d$treatgroupUNAUTH)]),
                    sd(d$treatgroupUNAUTH[!is.na(d$treatgroupUNAUTH)])))

# treatgroup: WHITE
out <- rbind(out, c("treatgroupWHITE", mean(d$treatgroupWHITE[!is.na(d$treatgroupWHITE)]),
                    median(d$treatgroupWHITE[!is.na(d$treatgroupWHITE)]),
                    sd(d$treatgroupWHITE[!is.na(d$treatgroupWHITE)])))

# treatgroup: EUROPEAN
out <- rbind(out, c("treatgroupEUROPEAN", mean(d$treatgroupEUROPEAN[!is.na(d$treatgroupEUROPEAN)]),
                    median(d$treatgroupEUROPEAN[!is.na(d$treatgroupEUROPEAN)]),
                    sd(d$treatgroupEUROPEAN[!is.na(d$treatgroupEUROPEAN)])))


## turn statistics back to numeric
out$mean <- as.numeric(out$mean)
out$median <- as.numeric(out$median)
out$sd <- as.numeric(out$sd)

## variable names
out$var <- c("Likely unauthorized (naturalization def.)", "Car ownership", "Car commute", "Employed",
             "Age", "Female", "Years in the US", "Married", "Number of children",
             "Less than high school degree", "High school degree or GED",
             "Some college", "College degree", "Graduate degree",
             "Urbanization (PUMA)", "Average travel time (PUMA, 2014)", 
             "Average public transit use (PUMA, 2014)", "Average employment (PUMA, 2014)",
             "Carpooling", "Drive car alone", "Hours worked per year", "Earnings",
             "Hours worked per week", "Weeks worked per year", "Wages", "Hourly wages", 
             "Industry: agriculture", "Industry: construction", "Industry: manufacturing", 
             "Industry: transportation", "Likely unauthorized (skills &  residence def.)",
             "Likely unauthorized (White control)", "Likely unauthorized (European placebo)")

## output
print(xtable(out, digits=3, align="llccc", caption="Summary statistics"), include.rownames=FALSE, caption.placement = "top")



#---------------------------------------------------------------#
### Figure A1: Distribution of industries by treatment status ###
#---------------------------------------------------------------#

## A1a: Naturalization definition
toplot <- data.frame(prop.table(table(d$industry, d$treatgroup), 1))
colnames(toplot) <- c("industry", "treatgroup", "share")

toplot2 <- toplot[c(order(toplot$share[toplot$treatgroup == 1], decreasing=T),
                    order(toplot$share[toplot$treatgroup == 1], decreasing=T)+18),]
toplot2$order <- c(rep(1:18,2))
toplot2$treatgroupFACTOR <- factor(toplot2$treatgroup, levels=c(0,1), 
                                   labels = c("naturalized", "likely unauthorized"))


p <- ggplot(toplot2, aes(x = reorder(industry, -order), y = share*100, fill=treatgroupFACTOR)) + 
  geom_bar(stat="identity", position = position_dodge(), width=-0.75) +
  coord_flip() +
  scale_y_continuous(breaks=seq(0,100,10), limits = c(0,100)) +
  scale_fill_manual(values=c("maroon3", "cornflowerblue"), name="") +
  xlab("") + 
  ylab("Share (%)") +
  theme_classic() +
  theme(axis.title = element_text(size=18, face="bold"),
        axis.text = element_text(size=16),
        legend.text = element_text(size=18),
        legend.position = "bottom")
ggsave("industry_treatgroup.png", p, width=7, height=5)


## A1b: Skills & residence definition
toplot <- data.frame(prop.table(table(d$industry, d$treatgroupUNAUTH), 1))
colnames(toplot) <- c("industry", "treatgroup", "share")

toplot2 <- toplot[c(order(toplot$share[toplot$treatgroup == 1], decreasing=T),
                    order(toplot$share[toplot$treatgroup == 1], decreasing=T)+18),]
toplot2$order <- c(rep(1:18,2))
toplot2$treatgroupFACTOR <- factor(toplot2$treatgroup, levels=c(0,1), 
                                   labels = c("naturalized", "likely unauthorized"))


p <- ggplot(toplot2, aes(x = reorder(industry, -order), y = share*100, fill=treatgroupFACTOR)) + 
  geom_bar(stat="identity", position = position_dodge(), width=-0.75) +
  coord_flip() +
  scale_y_continuous(breaks=seq(0,100,10), limits = c(0,100)) +
  scale_fill_manual(values=c("maroon3", "cornflowerblue"), name="") +
  xlab("") + 
  ylab("Share (%)") +
  theme_classic() +
  theme(axis.title = element_text(size=18, face="bold"),
        axis.text = element_text(size=16),
        legend.text = element_text(size=18),
        legend.position = "bottom")
ggsave("industry_treatgroupUNAUTH.png", p, width=7, height=5)


## A1c: White control
toplot <- data.frame(prop.table(table(d$industry, d$treatgroupWHITE), 1))
colnames(toplot) <- c("industry", "treatgroup", "share")

toplot2 <- toplot[c(order(toplot$share[toplot$treatgroup == 1], decreasing=T),
                    order(toplot$share[toplot$treatgroup == 1], decreasing=T)+18),]
toplot2$order <- c(rep(1:18,2))
toplot2$treatgroupFACTOR <- factor(toplot2$treatgroup, levels=c(0,1), 
                                   labels = c("citizen", "likely unauthorized"))


p <- ggplot(toplot2, aes(x = reorder(industry, -order), y = share*100, fill=treatgroupFACTOR)) + 
  geom_bar(stat="identity", position = position_dodge(), width=-0.75) +
  coord_flip() +
  scale_y_continuous(breaks=seq(0,100,10), limits = c(0,100)) +
  scale_fill_manual(values=c("maroon3", "cornflowerblue"), name="") +
  xlab("") + 
  ylab("Share (%)") +
  theme_classic() +
  theme(axis.title = element_text(size=18, face="bold"),
        axis.text = element_text(size=16),
        legend.text = element_text(size=18),
        legend.position = "bottom")
ggsave("industry_treatgroupWHITE.png", p, width=7, height=5)



# 6. Appendix: Additional outcomes ----------------------------------------

#--------------------------------------------------------------------------------------------------------#
### Table A5: Effect of driver licenses for unauthorized immigrants on car ownership (household-level) ###
#--------------------------------------------------------------------------------------------------------#

## Regressions
m1 <- felm(car_dich ~ treatgroup*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
           data=d[d$head==1,])
m2 <- felm(car_dich ~ treatgroupUNAUTH*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
           data=d[d$head==1,])


## Output
stargazer(m1, m2, 
          omit.stat = c("rsq", "ser", "f"),
          omit=c("county"),
          df = F,
          notes.align="c",
          no.space=T,
          font.size="tiny",
          star.cutoffs = c(0.05,0.01,0.001),
          model.names=F,
          column.labels = c("nat. def.", "skills/res. def."),
          dep.var.labels.include = T)


#-----------------------------------------------------------------------------------------------------------------------------#
### Table A6: Effect of driver licenses for unauthorized immigrants on additional measures of private transportation access ###
#-----------------------------------------------------------------------------------------------------------------------------#

## Regressions
m1 <- felm(travel_occupancy ~ treatgroup*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
           data=d)
m2 <- felm(travel_occupancy ~ treatgroupUNAUTH*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
           data=d)
m3 <- felm(travel_occupancy_single ~ treatgroup*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
           data=d)
m4 <- felm(travel_occupancy_single ~ treatgroupUNAUTH*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
           data=d)

## Output
stargazer(m1, m2, m3, m4,
          omit.stat = c("rsq", "ser", "f"),
          omit=c("county"),
          df = F,
          notes.align="c",
          no.space=T,
          font.size="tiny",
          star.cutoffs = c(0.05,0.01,0.001),
          model.names=F,
          column.labels = c("nat. def.", "skills/res. def.", 
                            "nat. def.", "skills/res. def."),
          dep.var.labels.include = T)


#---------------------------------------------------------------------------------------------------------#
### Table A7: Effect of driver licenses for unauthorized immigrants on additional labor market outcomes ###
#---------------------------------------------------------------------------------------------------------#

## Regressions
m1 <- felm(hoursworked ~ treatgroup*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
           data=d[d$worked==1,])
m2 <- felm(hoursworked ~ treatgroupUNAUTH*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
           data=d[d$worked==1,])
m3 <- felm(weeksworked2 ~ treatgroup*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
           data=d[d$worked==1,])
m4 <- felm(weeksworked2 ~ treatgroupUNAUTH*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
           data=d[d$worked==1,])


## Output
stargazer(m1, m2, m3, m4, 
          omit.stat = c("rsq", "ser", "f"),
          omit=c("county"),
          df = F,
          notes.align="c",
          no.space=T,
          font.size="tiny",
          star.cutoffs = c(0.05,0.01,0.001),
          model.names=F,
          column.labels = c("nat. def.", "skills/res. def.", 
                            "nat. def.", "skills/res. def."),
          dep.var.labels.include = T)


#-----------------------------------------------------#
### Table A8: No effect of driver licenses on wages ###
#-----------------------------------------------------#

## Regressions
m1 <- felm(log(wages.hourly + 1) ~ treatgroup*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
           data=d[d$worked==1,])
m2 <- felm(log(wages.hourly + 1) ~ treatgroupUNAUTH*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
           data=d[d$worked==1,])

## Output
stargazer(m1, m2, 
          omit.stat = c("rsq", "ser", "f"),
          omit=c("county"),
          df = F,
          notes.align="c",
          no.space=T,
          font.size="tiny",
          star.cutoffs = c(0.05,0.01,0.001),
          model.names=F,
          column.labels = c("nat. def.", "skills/res. def."),
          dep.var.labels.include = T)


#-------------------------------------------------------------------------------#
### Table A9: Few changes in the occupation of likely unauthorized immigrants ###
#-------------------------------------------------------------------------------#

## Regressions
m1 <- felm(industry_agriculture ~ treatgroup*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
           data=d[d$worked==1,])
m2 <- felm(industry_agriculture ~ treatgroupUNAUTH*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
           data=d[d$worked==1,])
m3 <- felm(industry_construction ~ treatgroup*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
           data=d[d$worked==1,])
m4 <- felm(industry_construction ~ treatgroupUNAUTH*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
           data=d[d$worked==1,])
m5 <- felm(industry_manufacturing ~ treatgroup*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
           data=d[d$worked==1,])
m6 <- felm(industry_manufacturing ~ treatgroupUNAUTH*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
           data=d[d$worked==1,])
m7 <- felm(industry_transportation ~ treatgroup*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
           data=d[d$worked==1,])
m8 <- felm(industry_transportation ~ treatgroupUNAUTH*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
           data=d[d$worked==1,])

## Output
stargazer(m1, m2, m3, m4, m5, m6, m7, m8,
          omit.stat = c("rsq", "ser", "f"),
          omit=c("county"),
          df = F,
          notes.align="c",
          no.space=T,
          font.size="tiny",
          star.cutoffs = c(0.05,0.01,0.001),
          model.names=F,
          column.labels = c("nat. def.", "skills/res. def.", 
                            "nat. def.", "skills/res. def.",
                            "nat. def.", "skills/res. def.",
                            "nat. def.", "skills/res. def."),
          dep.var.labels.include = T)





# 7. Appendix: Additional Analyses ----------------------------------------

#--------------------------------------------------------------------------------------------------------------#
### Table A10: Effects of driver licenses for unauthorized immigrants by gender 1: naturalization definition ###
#--------------------------------------------------------------------------------------------------------------#

## Regressions
m1 <- felm(car_dich ~ treatgroup*as.factor(post)*female + age + age_squared + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, data=d)
m2 <- felm(travel_car ~ treatgroup*as.factor(post)*female + age + age_squared + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, data=d)
m3 <- felm(hoursworkedyear ~ treatgroup*as.factor(post)*female + age + age_squared + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
           data=d[d$worked==1,])
m4 <- felm(earnings2019_log ~ treatgroup*as.factor(post)*female + age + age_squared + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
           data=d[d$worked==1,])

## Output
stargazer(m1, m2, m3, m4,
          omit.stat = c("rsq", "ser", "f"),
          omit=c("county"),
          df = F,
          notes.align="c",
          no.space=T,
          font.size="tiny",
          star.cutoffs = c(0.05,0.01,0.001),
          model.names=F,
          dep.var.labels.include = T)


#------------------------------------------------------------------------------------------------------------------#
### Table A11: Effects of driver licenses for unauthorized immigrants by gender 2: skills & residence definition ###
#------------------------------------------------------------------------------------------------------------------#

## Regressions
m1 <- felm(car_dich ~ treatgroupUNAUTH*as.factor(post)*female + age + age_squared + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, data=d)
m2 <- felm(travel_car ~ treatgroupUNAUTH*as.factor(post)*female + age + age_squared + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, data=d)
m3 <- felm(hoursworkedyear ~ treatgroupUNAUTH*as.factor(post)*female + age + age_squared + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
           data=d[d$worked==1,])
m4 <- felm(earnings2019_log ~ treatgroupUNAUTH*as.factor(post)*female + age + age_squared + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
           data=d[d$worked==1,])


## Output
stargazer(m1, m2, m3, m4,
          omit.stat = c("rsq", "ser", "f"),
          omit=c("county"),
          df = F,
          notes.align="c",
          no.space=T,
          font.size="tiny",
          star.cutoffs = c(0.05,0.01,0.001),
          model.names=F,
          dep.var.labels.include = T)


#-----------------------------------------------#
### Figure A2: Treatment effects by age group ###
#-----------------------------------------------#

## Use the function that was defined above
# car ownership
age.carown <- agefun(dv = "car_dich")
ggsave("agegroup_carown.png", age.carown, width=7, height=5)

# travel by car
age.travelcar <- agefun(dv = "travel_car")
ggsave("agegroup_travelcar.png", age.travelcar, width=7, height=5)

# hoursworked
d$hoursworkedyear2 <- d$hoursworkedyear
d$hoursworkedyear2[d$worked == 0] <- NA
age.hoursworkedyear <- agefun(dv = "hoursworkedyear2")
ggsave("agegroup_hoursworkedyear.png", age.hoursworkedyear, width=7, height=5)

# earnings
d$earnings2019_log2 <- d$earnings2019_log
d$earnings2019_log2[d$worked == 0] <- NA
age.earnings <- agefun(dv = "earnings2019_log2")
ggsave("agegroup_earnings.png", age.earnings, width=7, height=5)



#-----------------------------------------------------------------------#
### Figure A3: Results by labor market accessibility 1: Car ownership ###
#-----------------------------------------------------------------------#

## Use the functions defined above:
# naturalization definition
p <- het.main(dv = "car_dich")
ggsave("HetMain_carown.png", p, width=7, height=5)

# skills & residence definition
p <- het.alt(dv = "car_dich")
ggsave("HetAlt_carown.png", p, width=7, height=5)



#---------------------------------------------------------------------#
### Figure A4: Results by labor market accessibility 2: Car commute ###
#---------------------------------------------------------------------#

## Use the functions defined above:
# naturalization definition
p <- het.main(dv = "travel_car")
ggsave("HetMain_travelcar.png", p, width=7, height=5)

# skills & residence definition
p <- het.alt(dv = "travel_car")
ggsave("HetAlt_travelcar.png", p, width=7, height=5)



#-------------------------------------------------------------------------------#
### Figure A5: Results by labor market accessibility 3: Hours worked per year ###
#-------------------------------------------------------------------------------#

## Use the functions defined above:
# naturalization definition
p <- het.main(dv = "hoursworkedyear2")
ggsave("HetMain_hoursworkedyear.png", p, width=7, height=5)

# skills & residence definition
p <- het.alt(dv = "hoursworkedyear2")
ggsave("HetAlt_hoursworkedyear.png", p, width=7, height=5)



#------------------------------------------------------------------#
### Figure A6: Results by labor market accessibility 4: Earnings ###
#------------------------------------------------------------------#

## Use the functions defined above:
# naturalization definition
p <- het.main(dv = "earnings2019_log2")
ggsave("HetMain_earnings.png", p, width=7, height=5)

# skills & residence definition
p <- het.alt(dv = "earnings2019_log2")
ggsave("HetAlt_earnings.png", p, width=7, height=5)



#---------------------------------------------------------------------------------------------------#
### Table A12: Results for weeks worked and earnings when non-working respondents are not dropped ###
#---------------------------------------------------------------------------------------------------#

## Regressions
m1 <- felm(hoursworkedyear ~ treatgroup*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
           data=d)
m2 <- felm(hoursworkedyear ~ treatgroupUNAUTH*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
           data=d)
m3 <- felm(hoursworkedyear ~ treatgroup*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
           data=d[d$worked==0,])
m4 <- felm(hoursworkedyear ~ treatgroupUNAUTH*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
           data=d[d$worked==0,])
m5 <- felm(earnings2019_log ~ treatgroup*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
           data=d)
m6 <- felm(earnings2019_log ~ treatgroupUNAUTH*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
           data=d)
m7 <- felm(earnings2019_log ~ treatgroup*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
           data=d[d$worked==0,])
m8 <- felm(earnings2019_log ~ treatgroupUNAUTH*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | hhID, 
           data=d[d$worked==0,])

## Output
stargazer(m1, m2, m3, m4, m5, m6, m7, m8, 
          omit.stat = c("rsq", "ser", "f"),
          omit=c("county"),
          df = F,
          notes.align="c",
          no.space=T,
          font.size="tiny",
          star.cutoffs = c(0.05,0.01,0.001),
          model.names=F,
          column.labels = c("nat. def.", "skills/res. def.",
                            "nat. def.", "skills/res. def.",
                            "nat. def.", "skills/res. def.",
                            "nat. def.", "skills/res. def."),
          dep.var.labels.include = T)





# 8. Appendix: Robustness Checks ------------------------------------------

#--------------------------------------------------------------------------------------------#
### Table A13: Robustness to the omission of time trends: Access to private transportation ###
#--------------------------------------------------------------------------------------------#

## Regressions
m1 <- felm(car_dich ~ treatgroup*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) | county + year | 0 | hhID, 
           data=d)
m2 <- felm(car_dich ~ treatgroupUNAUTH*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) | county + year | 0 | hhID, data=d)
m3 <- felm(travel_car ~ treatgroup*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) | county + year | 0 | hhID, 
           data=d)
m4 <- felm(travel_car ~ treatgroupUNAUTH*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) | county + year | 0 | hhID, data=d)

## Output
stargazer(m1, m2, m3, m4,
          omit.stat = c("rsq", "ser", "f"),
          omit=c("county"),
          df = F,
          notes.align="c",
          no.space=T,
          font.size="tiny",
          star.cutoffs = c(0.05,0.01,0.001),
          model.names=F,
          column.labels = c("nat. def.", "skills/res. def.", 
                            "nat. def.", "skills/res. def."),
          dep.var.labels.include = T)


#--------------------------------------------------------------------------------#
### Table A14 Robustness to the omission of time trends: Labor market outcomes ###
#--------------------------------------------------------------------------------#

## Regressions
m1 <- felm(worked ~ treatgroup*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) | county + year | 0 | hhID, 
           data=d)
m2 <- felm(worked ~ treatgroupUNAUTH*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) | county + year | 0 | hhID, data=d)
m3 <- felm(hoursworkedyear ~ treatgroup*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) | county + year | 0 | hhID, 
           data=d[d$worked==1,])
m4 <- felm(hoursworkedyear ~ treatgroupUNAUTH*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) | county + year | 0 | hhID, 
           data=d[d$worked==1,])
m5 <- felm(earnings2019_log ~ treatgroup*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) | county + year | 0 | hhID, 
           data=d[d$worked==1,])
m6 <- felm(earnings2019_log ~ treatgroupUNAUTH*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) | county + year | 0 | hhID, data=d[d$worked==1,])


## Output
stargazer(m1, m2, m3, m4, m5, m6,
          omit.stat = c("rsq", "ser", "f"),
          omit=c("county"),
          df = F,
          notes.align="c",
          no.space=T,
          font.size="tiny",
          star.cutoffs = c(0.05,0.01,0.001),
          model.names=F,
          column.labels = c("nat. def.", "skills/res. def.", 
                            "nat. def.", "skills/res. def.",
                            "nat. def.", "skills/res. def."),
          dep.var.labels.include = T)



#-------------------------------------------------------------------------------------#
### Table A15: Robustness to clustering by county: Access to private transportation ###
#-------------------------------------------------------------------------------------#

## Regressions
m1 <- felm(car_dich ~ treatgroup*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | county, 
           data=d)
m2 <- felm(car_dich ~ treatgroupUNAUTH*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | county, data=d)
m3 <- felm(travel_car ~ treatgroup*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | county, 
           data=d)
m4 <- felm(travel_car ~ treatgroupUNAUTH*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | county, data=d)


## Output
stargazer(m1, m2, m3, m4,
          omit.stat = c("rsq", "ser", "f"),
          omit=c("county"),
          df = F,
          notes.align="c",
          no.space=T,
          font.size="tiny",
          star.cutoffs = c(0.05,0.01,0.001),
          model.names=F,
          column.labels = c("nat. def.", "skills/res. def.", 
                            "nat. def.", "skills/res. def."),
          dep.var.labels.include = T)




#-----------------------------------------------------------------------------------#
### Table A16: Robustness to clustering by PUMA: Access to private transportation ###
#-----------------------------------------------------------------------------------#

## Regressions
m1 <- felm(car_dich ~ treatgroup*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | puma, 
           data=d)
m2 <- felm(car_dich ~ treatgroupUNAUTH*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | puma, data=d)
m3 <- felm(travel_car ~ treatgroup*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | puma, 
           data=d)
m4 <- felm(travel_car ~ treatgroupUNAUTH*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | puma, data=d)


## Output
stargazer(m1, m2, m3, m4,
          omit.stat = c("rsq", "ser", "f"),
          omit=c("county"),
          df = F,
          notes.align="c",
          no.space=T,
          font.size="tiny",
          star.cutoffs = c(0.05,0.01,0.001),
          model.names=F,
          column.labels = c("nat. def.", "skills/res. def.", 
                            "nat. def.", "skills/res. def."),
          dep.var.labels.include = T)




#--------------------------------------------------------------------------#
### Table A17: Robustness to clustering by county: Labor market outcomes ###
#--------------------------------------------------------------------------#

## Regressions
m1 <- felm(worked ~ treatgroup*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | county, 
           data=d)
m2 <- felm(worked ~ treatgroupUNAUTH*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | county, data=d)
m3 <- felm(hoursworkedyear ~ treatgroup*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | county, 
           data=d[d$worked==1,])
m4 <- felm(hoursworkedyear ~ treatgroupUNAUTH*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | county, 
           data=d[d$worked==1,])
m5 <- felm(earnings2019_log ~ treatgroup*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | county, 
           data=d[d$worked==1,])
m6 <- felm(earnings2019_log ~ treatgroupUNAUTH*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | county, 
           data=d[d$worked==1,])

## Output
stargazer(m1, m2, m3, m4, m5, m6,
          omit.stat = c("rsq", "ser", "f"),
          omit=c("county"),
          df = F,
          notes.align="c",
          no.space=T,
          font.size="tiny",
          star.cutoffs = c(0.05,0.01,0.001),
          model.names=F,
          column.labels = c("nat. def.", "skills/res. def.", 
                            "nat. def.", "skills/res. def.",
                            "nat. def.", "skills/res. def."),
          dep.var.labels.include = T)




#------------------------------------------------------------------------#
### Table A18: Robustness to clustering by PUMA: Labor market outcomes ###
#------------------------------------------------------------------------#

## Regressions
m1 <- felm(worked ~ treatgroup*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | puma, 
           data=d)
m2 <- felm(worked ~ treatgroupUNAUTH*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | puma, 
           data=d)
m3 <- felm(hoursworkedyear ~ treatgroup*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | puma, 
           data=d[d$worked==1,])
m4 <- felm(hoursworkedyear ~ treatgroupUNAUTH*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | puma, 
           data=d[d$worked==1,])
m5 <- felm(earnings2019_log ~ treatgroup*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | puma, 
           data=d[d$worked==1,])
m6 <- felm(earnings2019_log ~ treatgroupUNAUTH*as.factor(post) + age + age_squared + female + yearsUS + yearsUS_squared + 
             marstat_married + nminor16 + as.factor(educ_cat) + as.factor(county)*year | county + year | 0 | puma, 
           data=d[d$worked==1,])

## Output
stargazer(m1, m2, m3, m4, m5, m6,
          omit.stat = c("rsq", "ser", "f"),
          omit=c("county"),
          df = F,
          notes.align="c",
          no.space=T,
          font.size="tiny",
          star.cutoffs = c(0.05,0.01,0.001),
          model.names=F,
          column.labels = c("nat. def.", "skills/res. def.", 
                            "nat. def.", "skills/res. def.",
                            "nat. def.", "skills/res. def."),
          dep.var.labels.include = T)





