rm(list = ls())


## data "GaltonFamilies" from the R package "HistData"
## family: family ID, a factor with levels 001-204
## father: height of father
## mother: height of mother
## midparentHeight: mid-parent height, calculated as (father + 1.08*mother)/2
## children: number of children in this family
## childNum: number of this child within family. Children are listed in decreasing order of height for boys followed by girls
## gender: child gender, a factor with levels female male
## childHeight: height of child

GaltonFamilies = read.table("GaltonFamilies.txt", header = TRUE)
plot(childHeight ~ midparentHeight, data = GaltonFamilies)


## data "prostate" from the R package "ElemStatLearn"
## meaning of the variables
# lcavol =  log cancer volume
# lweight = log prostate weight
# age in years
# lbph = log of the amount of benign prostatic hyperplasia
# svi = seminal vesicle invasion
# lcp = log of capsular penetration
# gleason = a numeric vector
# pgg45 = percent of Gleason score 4 or 5
# lpsa = response
prostate = read.table("prostate.txt", header = TRUE)


## fit a linear model by the ordinary least squares
fit_lpsa = lm(lpsa ~ lcavol + lweight + age + lbph + svi  + lcp + gleason + pgg45, 
              data = prostate)

print(summary(fit_lpsa), digits = 3)



## data "mroz" from R package "wooldridge"
## variables
## inlf: =1 if in labor force, 1975  
## hours: hours worked, 1975
## kidslt6: # kids < 6 years
## kidsge6: # kids 6-18
## age: woman's age in yrs
## educ: years of schooling
## wage: est. wage from earn, hrs
## repwage: rep. wage at interview in 1976
## hushrs: hours worked by husband, 1975
## husage: husband's age
## huseduc: husband's years of schooling
## huswage: husband's hourly wage, 1975
## faminc: family income, 1975
## mtr: fed. marg. tax rte facing woman
## motheduc: mother's years of schooling
## fatheduc: father's years of schooling
## unem: unem. rate in county of resid
## city: =1 if live in Standard Metropolitan Statistical Area (SMSA)
## exper: actual labor mkt exper
## nwifeinc: (faminc - wage*hours)/1000
## lwage: log(wage)
## expersq: exper^2


mroz = read.table("mroz.txt", header = TRUE)

mroz_fit = glm(inlf ~ kidslt6 + kidsge6 + age + educ 
               + hushrs + husage + huseduc 
               + huswage + unem + city, 
               family = binomial(link = logit), data = mroz)

print(summary(mroz_fit), digits = 3)